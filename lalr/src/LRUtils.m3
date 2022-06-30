
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Portions of this source file were 
   mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Lookahead.mi, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

MODULE LRUtils 

(* Collected utilities for CFG analysis. *) 

; IMPORT Fmt 
; IMPORT IntIntTbl 
; IMPORT Word 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen  
; FROM Failures IMPORT Backout   
; IMPORT IntSets 
; IMPORT LALRTypes 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT MessageCodes 

; TYPE AFT = MessageCodes . T 

; CONST AkLR1List = LALRTypes . AutomatonKindTyp . AkLR1List 
; CONST AkLR1Set = LALRTypes . AutomatonKindTyp . AkLR1Set 
; CONST AkLR0List = LALRTypes . AutomatonKindTyp . AkLR0List  

(* VISIBLE: *) 
; PROCEDURE ComputeShortestDerivable ( Gram : LRTable . GrammarTyp ) 
  (* For each nonterminal, compute the length of the shortest 
     derivable terminal string.  Set the ShortestDerivable fields in
     each production and each nonterminal. 
  *) 

  = VAR LRHSTok : LbeStd . TokTyp 
  ; VAR LSomethingChanged : BOOLEAN 
  ; VAR LProdValue : LbeStd . LimitedTokCtTyp 
  ; VAR LRHSTokValue : LbeStd . LimitedTokCtTyp 
  ; VAR LRightSs : CARDINAL 

  ; BEGIN (* ComputeShortestDerivable *) 
      REPEAT (* Until converge. *) 
        LSomethingChanged := FALSE 
      ; FOR RProdSs := 0 TO Gram . NextProdSs - 1 
        DO (* For each production, recompute shortest derivable of its RHS, 
              using current values for the NTs occurring in its RHS.
           *) 
          LProdValue := 0 
        ; WITH WProd = Gram . ProdArrayRef ^ [ RProdSs ] 
          DO IF WProd . Right = NIL 
             THEN 
               LProdValue := 0 
             ELSE 
               LRightSs := 0 
             ; LOOP (* Thru RHS symbols. *) 
                 IF LRightSs >= NUMBER ( WProd . Right ^ ) 
                 THEN 
                   EXIT 
                 ELSE 
                   LRHSTok := WProd . Right ^ [ LRightSs ] 
                 ; IF Gram . FirstAstNonterminal <= LRHSTok 
                      AND LRHSTok <= Gram . LastNonterminal 
                   THEN (* RHS token is a NT. *) 
                     LRHSTokValue 
                       := Gram . NontermInfoArrayRef ^ 
                            [ LRHSTok - Gram . FirstAstNonterminal ] 
                          . ShortestDerivable 
                   ; IF LRHSTokValue = LbeStd . ParseCheckInfinity 
                     THEN 
                       LProdValue := LbeStd . ParseCheckInfinity 
                     ; EXIT 
                     END (* IF *) 
                   ELSIF WProd . Left = LbeStd . Tok__Augment 
                   THEN 
                     LRHSTokValue := 2 
                   ELSE (* RHS terminal. *) 
                     LRHSTokValue := 1 
                   END (* IF *) 
                 ; LbeStd . IncLimitedTokCt ( LProdValue , LRHSTokValue ) 
                   (* ^Saturates at ParseCheckInfinity, but this is unlikely 
                      to happen. 
                   *) 
                 ; INC ( LRightSs ) 
                 END (* IF *) 
               END (* LOOP *) 
             END (* IF *) 
          ; WProd . ShortestDerivable := LProdValue 

          (* Propagate newly computed value for production into its LHS NT. *)
          ; IF WProd . Left >= Gram . FirstAstNonterminal 
            THEN 
              WITH 
                WNontermShortestDerivable 
                = Gram . NontermInfoArrayRef ^ 
                    [ WProd . Left - Gram . FirstAstNonterminal ] 
                  . ShortestDerivable 
              DO IF WNontermShortestDerivable > LProdValue 
                 THEN 
                   WNontermShortestDerivable := LProdValue 
                 ; LSomethingChanged := TRUE 
                 END (* IF *) 
              END (* WITH *) 
            END (* IF *) 
          END (* WITH *) 
        END (* FOR *) 
      UNTIL NOT LSomethingChanged 
    END ComputeShortestDerivable 

(* VISIBLE: *) 
; PROCEDURE NullableNonterms ( Gram : LRTable . GrammarTyp ) 
  : IntSets . T (* Nonterminals that can derive empty. *) 
  (* Die Nichtterminale welche nullable sind *) 

  = VAR CnToDoSet : IntSets . T   (* noch zu ueberpruefende Nichterminale *) 
        (* ^Unbiased. *) 
  ; VAR CnSomethingChanged : BOOLEAN (* hatte der letzte Schritt Erfolg *) 
  ; VAR CnResult : IntSets . T (* Nonterminals. *) 

  ; PROCEDURE IsNowNullable ( nt : IntSets . ValidElemT ) 

    = VAR LI : LALRTypes . ListCountTyp 
    ; VAR LProdSs : LRTable . ProdNoTyp 
    ; VAR LSuccess : BOOLEAN 
    ; VAR LRight : LbeStd . TokTyp 

    ; BEGIN (* IsNowNullable *) 
        WITH 
          WNontermInfo 
          = Gram . NontermInfoArrayRef ^ [ nt - Gram . FirstAstNonterminal ] 
        DO (* Betrachte alle Produktionen mit linker Seite nt *) 
           LSuccess := FALSE 
        ; LProdSs := 0 
        ; WHILE LProdSs < WNontermInfo . Used AND NOT LSuccess 
          DO (* Auswahl der einzelnen Produktion *) 
             WITH 
               WProduction 
               = Gram . ProdArrayRef ^ [ WNontermInfo . ProdListRef 
                 ^ [ LProdSs ] ] 
             DO (* Pruefe ob rechte Seite in Nullables* liegt *) 
                LSuccess := TRUE 
             ; LI := 0 
             ; WHILE LI < WProduction . Len AND LSuccess 
               DO LRight := WProduction . Right ^ [ LI ] 
               ; LSuccess 
                   := LRight >= Gram . FirstAstNonterminal 
                      AND IntSets . IsElement ( LRight , CnResult )
               ; INC ( LI ) 
               END (* WHILE *) 
             END (* WITH *) 
          ; INC ( LProdSs ) 
          END (* WHILE *) 
        END (* WITH *) 
      ; IF LSuccess 
        THEN 
          CnResult := IntSets . Include ( CnResult , nt ) 
        ; CnToDoSet := IntSets . Exclude ( CnToDoSet , nt )
        ; CnSomethingChanged := TRUE 
        END (* IF *) 
      END IsNowNullable 

  ; BEGIN (* NullableNonterms *)
      CnToDoSet 
        := IntSets . Range 
             ( Lo := Gram . FirstAstNonterminal 
             , Hi := Gram . LastNonterminal 
             ) 
    ; CnResult := IntSets . Empty ( )
    ; REPEAT 
        CnSomethingChanged := FALSE 

      (* Pruefe ob jetzt ein weiteres *) 
      (* Nichtterminal terminalisierbar ist *) 
      ; IntSets . ForAllDo ( CnToDoSet , IsNowNullable ) 
      UNTIL NOT CnSomethingChanged (* solange bis sich nichts aendert *) 
    ; RETURN CnResult 
    END NullableNonterms 

(* VISIBLE: *) 
; PROCEDURE ComputeNullabilityAndFirst ( Gram : LRTable . GrammarTyp ) 
  (* For each AstNonterminal and Nonterminal, compute its nullability
     (in Gram . NullableNts) and its FIRST set (in field First of its
     NonterminalInfo).
  *) 

  = VAR LRightSs : CARDINAL 
  ; VAR LProdSs : LRTable . ProdNoTyp 
  ; VAR LRhsTok : LbeStd . TokTyp 
  ; VAR LSomethingChanged : BOOLEAN 

  ; BEGIN 
    (* Just in case we reexecute ComputeNullabilityAndFirst: *) 
      Gram . NullableNTs := IntSets . Empty ( ) 
    ; FOR RNT := Gram . FirstAstNonterminal TO Gram . LastNonterminal 
      DO WITH WNTInfo 
                = Gram . NontermInfoArrayRef 
                  ^ [ RNT - Gram . FirstAstNonterminal ]  
        DO
          WNTInfo . First := IntSets . Empty ( )     
        END (* WITH *) 
      END (* FOR *) 
    ; REPEAT (* Iterate until no changes. *)  
        LSomethingChanged := FALSE 
      ; FOR RNT := Gram . FirstAstNonterminal TO Gram . LastNonterminal 
        DO WITH WNTInfo 
                = Gram . NontermInfoArrayRef 
                  ^ [ RNT - Gram . FirstAstNonterminal ]  
          DO (* For each NT, described by WNTInfo: *) 
  IF RNT = 82
  THEN Assertions . DoNothing ( ) 
  END 
; 
            FOR RProdListSs := 0 TO WNTInfo . Used - 1 
            DO 
              LProdSs := WNTInfo . ProdListRef ^ [ RProdListSs ] 
            ; WITH WProd = Gram . ProdArrayRef ^ [ LProdSs ] 
              DO (* Each production WProd of nonterminal RNT: *) 
                LRightSs := 0 
; IF LProdSs = 57 
  THEN Assertions . DoNothing ( ) 
  END 
              ; LOOP (* Thru RHS symbols of WProd. *)  
                  IF LRightSs >= WProd . Len 
                  THEN (* Entire RHS is nullable. *) 
                    IF NOT IntSets . IsElement ( RNT , Gram . NullableNTs ) 
                    THEN 
                      Gram . NullableNTs 
                        := IntSets . Include ( Gram . NullableNTs , RNT ) 
                    ; LSomethingChanged := TRUE 
                    END (* IF *) 
                  ; EXIT (* RHS symbols of WProd. *)
                  ELSE (* There is another RHS symbol. *)  
                    LRhsTok := WProd . Right ^ [ LRightSs ] 
                  ; IF LRhsTok < Gram . FirstAstNonterminal
                       (* ^Includes ModToks. *)  
(* CHECK: Do we need a lower bound on this? Probably we won't have a standard
          token here, with possible exception of the standard varterm toks. *) 
                    THEN (* LRhsTok is a terminal. *) 
                      IF NOT IntSets . IsElement ( LRhsTok , WNTInfo . First ) 
                      THEN 
                        WNTInfo . First 
                          := IntSets . Include ( WNTInfo . First , LRhsTok ) 
                        (* ^Spontaneous. *) 
                      ; LSomethingChanged := TRUE 
                      END (* IF *)
                    ; EXIT (* RHS symbols.  We can't see past this terminal. *)
                    ELSE (* LRhsTok is AstNonterminal or Nonterminal. *)
                      IF LRhsTok <= Gram . LastAstNonterminal 
                         AND NOT IntSets . IsElement 
                                   ( LRhsTok , WNTInfo . First ) 
                      THEN (* LRhsTok is AstNonterminal *) 
                        WNTInfo . First 
                          := IntSets . Include ( WNTInfo . First , LRhsTok ) 
                        (* ^Spontaneous. *) 
                      ; LSomethingChanged := TRUE 
                      END (* IF *)
                    ; WITH WRhsNTInfo 
                           = Gram . NontermInfoArrayRef ^ 
                               [ LRhsTok - Gram . FirstAstNonterminal ]  
                      DO IF NOT IntSets . IsSubset 
                                  ( WRhsNTInfo . First , WNTInfo . First ) 
                        THEN 
                          WNTInfo . First 
                            := IntSets . Union 
                                 ( WNTInfo . First , WRhsNTInfo . First ) 
                          (* ^Propagated. *) 
                        ; LSomethingChanged := TRUE 
                        END (* IF *) 
                      ; IF IntSets . IsElement ( LRhsTok , Gram . NullableNTs ) 
                        THEN (* Look for another RHS symbol. *) 
                          INC ( LRightSs )  
                        ELSE (* We can't see past this RHS NT *) 
                          EXIT (* RHS symbols of WProd. *)  
                        END (* IF *) 
                      END (* WITH WRhsNTInfo *) 
                    END (* IF *) 
                  END (* IF *) 
                END (* LOOP Thru RHS symbols. *)  
              END (* WITH *)  
            END (* FOR each production *) 
          END (* WITH *) 
        END (* FOR each NT. *) 
      UNTIL NOT LSomethingChanged 
    END ComputeNullabilityAndFirst 

(* VISIBLE: *) 
; PROCEDURE ComputeLast ( Gram : LRTable . GrammarTyp ) 
  (* For each AstNonterminal and Nonterminal, compute its LAST 
     set (in field Last of its NonterminalInfo).
  *) 

  = BEGIN 
(* TODO: Provide this. *) 
    END ComputeLast 

; PROCEDURE AddHashTerm  
    ( OldHash , NewTerm : LALRTypes . HashCodeTyp ) 
  : LALRTypes . HashCodeTyp 
  (* Assuming LALRTypes . HashCodeTyp has 32-bit range, this intends to compute
     the same 32-bit hash codes on either a 32-bit or 64-bit machine.
  *)

  = VAR LResult : LALRTypes . HashCodeTyp 

  ; BEGIN 
      IF Word . GT 
           ( OldHash 
           , Word . Minus ( LALRTypes . HashCodeMaxUns , NewTerm )  
           ) 
      THEN 
        LResult := Word . Minus ( OldHash , LALRTypes . HashCodeMaxUns )
      ELSE 
        LResult := OldHash  
      END (* IF *)  
    ; LResult := Word . Plus ( LResult , NewTerm )  
    ; RETURN LResult 
    END AddHashTerm 

(* VISIBLE: *) 
; PROCEDURE ComputeLastItemLR0Hash ( Gram : LRTable . GrammarTyp ) 
  (* Compute item hash for the last item of the last state, using LR(0) 
     treatment of the item.  Includes only Prod and DotPos. 
  *) 

  (* Designed to be same on 32-bit and 64-bit computers. *) 

  = CONST ProdNoShiftCt = 5 (* Room for 32 dot positions without collision. *)

  ; VAR LItemSs : LALRTypes . ItemSsTyp 
  ; VAR LLR0Term : LALRTypes . HashCodeTyp 

  ; BEGIN 
      WITH WState = Gram . StateArrayRef ^ [ Gram . NextStateSs - 1 ] 
      DO LItemSs := WState . FirstItemSs + WState . ItemCt - 1 
      ; WITH WItem = Gram . ItemArrayRef ^ [ LItemSs ] 
        DO 
          LLR0Term 
            := Word . And 
                 ( Word . LeftShift ( WItem . ProdSs , ProdNoShiftCt )
                 , 16_FFFFFFFF 
                 ) 
        ; LLR0Term := AddHashTerm ( LLR0Term , WItem . DotPosition ) 
        ; WItem . LR0ItemHashCode := LLR0Term 
        END (* WITH *) 
      END (* WITH *) 
    END ComputeLastItemLR0Hash 

(* VISIBLE: *) 
; PROCEDURE ItemsCompare 
    ( Gram : LRTable . GrammarTyp 
    ; Left , Right : LALRTypes . ItemSsTyp 
    ; AutomatonKind : LALRTypes . AutomatonKindTyp 
    ) 
  : [ - 1 .. 1 ] 
  (* Compare according to an arbitrary but repeatable total ordering. 
     But for readability of dumped item sets, put nonclosure items first. 
     PRE: Left and Right are valid item subscripts. 
  *) 
  (* For LR(0), we consider only the production and dot position. 
     For LR(1), list or set, also consider the LR1 lookahead set.
  *) 

  = VAR LLeftLHS , LRightLHS : LbeStd . TokTyp 

  ; BEGIN 
      IF Left = Right THEN RETURN 0 END (* IF *) 
    ; WITH WLeftItem = Gram . ItemArrayRef ^ [ Left ] 
           , WRightItem = Gram . ItemArrayRef ^ [ Right ] 
      DO 
      (* Make augmented start production go first. *) 
        LLeftLHS := Gram . ProdArrayRef ^ [ WLeftItem . ProdSs ] . Left 
      ; LRightLHS := Gram . ProdArrayRef ^ [ WRightItem . ProdSs ] . Left 
      ; IF LLeftLHS = LbeStd . Tok__Augment 
        THEN IF LRightLHS # LbeStd . Tok__Augment
          THEN RETURN - 1 
          ELSE RETURN 0
          END (* IF *) 
        ELSE IF LRightLHS = LbeStd . Tok__Augment
          THEN RETURN 1 
       (* ELSE fall through. *) 
          END (* IF *) 
        END (* IF *) 

      (* Make nonclosure items go before closure items. *) 
      ; IF WLeftItem . DotPosition > 0 AND WRightItem . DotPosition = 0 
        THEN RETURN - 1 
        ELSIF WLeftItem . DotPosition = 0 AND WRightItem . DotPosition > 0 
        THEN RETURN 1 
        END (* IF *) 

      (* Next use ascending production number. *) 
      ; IF WLeftItem . ProdSs < WRightItem . ProdSs 
        THEN RETURN - 1 
        ELSIF WLeftItem . ProdSs > WRightItem . ProdSs 
        THEN RETURN 1 
        END (* IF *) 

      (* Next use ascending dot position. *) 
      ; IF WLeftItem . DotPosition < WRightItem . DotPosition 
        THEN RETURN - 1 
        ELSIF WLeftItem . DotPosition > WRightItem . DotPosition 
        THEN RETURN 1
        END (* IF *) 

      (* If LR(1), finally, use LR(1) lookahead set ordering. *)  
      ; CASE AutomatonKind 
        OF AkLR1List 
        , AkLR1Set
        => RETURN IntSets . Compare ( WLeftItem . LR1Set , WRightItem . LR1Set )
        | AkLR0List
        => RETURN 0 
        END (* CASE *) 
      END (* WITH *) 
    END ItemsCompare 

(* VISIBLE: *) 
; PROCEDURE ComputeNewestStateHashContributions ( Gram : LRTable . GrammarTyp ) 
  (* Of the newest Item, to the hash codes of the newest state. *) 

  = VAR LLR1Term : LALRTypes . HashCodeTyp 

  ; BEGIN (* ComputeNewestStateHashContributions *) 
      WITH WState = Gram . StateArrayRef ^ [ Gram . NextStateSs - 1 ] 
      DO WITH WItem = Gram . ItemArrayRef ^ [ Gram . NextItemSs - 1 ] 
         DO IF WItem . DotPosition > 0 
           THEN (* Only nonclosure items contribute to state hash code. *)  

           (* Production and dot position (which are included in item hash code)
              contribute to all state hash codes. *) 
             WState . LR1ListHashCode 
               := AddHashTerm 
                    ( WState . LR1ListHashCode , WItem . LR0ItemHashCode ) 
           ; IF WState . LR1SetRepStateSs = Gram . NextStateSs - 1 
             THEN (* It's an LR(1) set state too. *) 
               WState . LR1SetHashCode 
                 := AddHashTerm 
                      ( WState . LR1SetHashCode , WItem . LR0ItemHashCode ) 
             END (* IF*) 
           ; IF WState . LR0ListRepStateSs = Gram . NextStateSs - 1 
             THEN (* It's an LR(0) list state too. *) 
               WState . LR0ListHashCode 
                 := AddHashTerm 
                      ( WState . LR0ListHashCode , WItem . LR0ItemHashCode ) 
             END (* IF *) 

           (* Lookahead set contributes to LR(1) list and set hash code. *) 
           ; LLR1Term := IntSets . Hash ( WItem . LR1Set ) 
           ; WState . LR1ListHashCode 
               := AddHashTerm ( WState . LR1ListHashCode , LLR1Term ) 
           ; WState . LR1SetHashCode 
               := AddHashTerm ( WState . LR1SetHashCode , LLR1Term ) 
(* TODO: Maybe do something so that list order affects hash code, for lists
         with identical set contents? *) 
           END (* IF *) 
         END (* WITH *) 
      END (* WITH *) 
    END ComputeNewestStateHashContributions 

; PROCEDURE CheckSetOrderList 
    ( Gram : LRTable . GrammarTyp ; READONLY State : LALRTypes . StateTyp ) 
  RAISES { Backout } 

  = VAR LPrevItemSs , LItemSs : LALRTypes . ItemSsTyp 
  ; VAR LCompare : [ - 1 .. 1 ]
  ; VAR LReordered : BOOLEAN  

  ; BEGIN 
      IF State . SetOrderList = NIL 
      THEN
        CantHappen ( AFT . A_CheckSetOrderList_Nil_list ) 
      ELSE  
        LReordered := FALSE 
      ; LPrevItemSs 
          := State . SetOrderList ^ [ FIRST ( State . SetOrderList ^ ) ]
      ; FOR RListSs := FIRST ( State . SetOrderList ^ ) + 1 
            TO LAST ( State . SetOrderList ^ )
        DO
          LItemSs := State . SetOrderList ^ [ RListSs ] 
        ; LCompare := ItemsCompare ( Gram , LPrevItemSs , LItemSs , AkLR1Set ) 
        ; Assert 
            ( LCompare = - 1 
            , AFT . A_CheckSetOrderList_Not_ordered
            ) 
        ; IF LPrevItemSs > LItemSs 
          THEN LReordered := TRUE 
          END (* IF *) 
        ; LPrevItemSs := LItemSs 
        END (* FOR *) 
      ; IF LReordered 
        THEN 
          INC ( Gram . LR1ReorderedStateCt ) 
        END (* IF *) 
      END (* IF *) 
    END CheckSetOrderList

; PROCEDURE LazyComputeSetOrderList 
    ( Gram : LRTable . GrammarTyp ; VAR State : LALRTypes . StateTyp ) 
  RAISES { Backout } 

  = VAR LListRef : LALRTypes . ItemSsArrayRefTyp 
  ; VAR LListCt , LListSs , LListNextSs , LListEmptySs , LParentListSs 
        : CARDINAL 
  ; VAR LLeftListSs , LRightListSs , LGreaterListSs : CARDINAL 
  ; VAR LLeftItemSs , LRightItemSs , LParentItemSs : LALRTypes . ItemSsTyp 
  ; VAR LReinsertItemSs , LGreaterItemSs : LALRTypes . ItemSsTyp  
  ; VAR LCompare : [ - 1 .. 1 ] 

  ; BEGIN 
      IF State . SetOrderList = NIL 
         AND State . NonclosureItemCt > 0 (* Paranoia. *) 
      THEN (* We need to build SetOrderList. *) 
        LListCt := State . NonclosureItemCt 
      ; LListRef := NEW ( LALRTypes . ItemSsArrayRefTyp , LListCt ) 
      ; State . SetOrderList := LListRef 

      (* Heapsort.  The standard heap uses 1-origin subscripts.  Since
         there will be lots of these, we don't really want to waste the
         zero-th element of the open array.  So we use it that way. 
         The two children of p are at 2*p+1 and 2*p+2.  
         The parent of c is at (c-1)DIV 2.   
      *) 
      (* Phase one: in LListRef ^, build a heap of subscripts to nonclosure
         items . *) 
      ; LListNextSs := 0 (* Spot in heap we are about to fill. *) 
      ; FOR RItemSs := State . FirstItemSs 
            TO State . FirstItemSs + State . ItemCt - 1 
        DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
          DO IF WItem . DotPosition > 0 (* Nonclosure item. *) 
           THEN 
             LListEmptySs := LListNextSs
           ; LOOP (* Promote towards root. *) 
               IF LListEmptySs = 0 
               THEN (* We Reached the root.  Put item here. *) 
                 LListRef ^ [ 0 ] := RItemSs 
               ; EXIT 
               ELSE (* Look at the parent. *) 
                 LParentListSs := ( LListEmptySs - 1 ) DIV 2 
               ; LParentItemSs := LListRef ^ [ LParentListSs ] 
               ; LCompare 
                   := ItemsCompare ( Gram , LParentItemSs , RItemSs , AkLR1Set )
               ; IF LCompare = 1 
                 THEN (* Parent is greater. Put new item here. *) 
                   LListRef ^ [ LListEmptySs ] := RItemSs 
                 ; EXIT 
                 ELSE (* Move data from parent down to here. *) 
                   LListRef ^ [ LListEmptySs ] := LParentItemSs 
                 ; LListEmptySs := LParentListSs 
                 END (* IF *)   
               END (* IF *) 
             END (* LOOP *) 
           ; INC ( LListNextSs ) 
           END (* IF *) 
          END (* WITH *) 
        END (* FOR *) 
      ; Assert 
          ( LListNextSs = LListCt 
          , AFT . A_LazyComputeSetOrderList_Wrong_NonclosureItemCt 
          ) 

      (* Phase two. *) 
      ; FOR RListSs := LListCt - 1 TO 1 BY - 1 (* Spot for greatest item. *)
        DO 
          LReinsertItemSs := LListRef ^ [ RListSs ] (* Make space. *) 
        ; LListRef ^ [ RListSs ] := LListRef ^ [ 0 ]
        ; LListSs := 0 
        ; LOOP 
            LLeftListSs := ( LListSs * 2 ) + 1 
          ; LRightListSs := LLeftListSs + 1 
          ; IF LLeftListSs >= RListSs 
            THEN (* No children. We are done. *) 
              LListRef ^ [ LListSs ] := LReinsertItemSs 
            ; EXIT
            ELSE (* We at least have a left child. *)  
              LLeftItemSs := LListRef ^ [ LLeftListSs ] 
            ; IF LRightListSs >= RListSs 
              THEN (* No right child. Treat left as greater child. *)  
                LGreaterListSs := LLeftListSs 
              ; LGreaterItemSs := LLeftItemSs 
              ELSE (* Both a left and right child. *) 
                LRightItemSs := LListRef ^ [ LRightListSs ] 
              ; LCompare 
                  := ItemsCompare 
                       ( Gram , LLeftItemSs , LRightItemSs , AkLR1Set )
              ; IF LCompare = 1 
                THEN (* Left is greater child. *) 
                  LGreaterListSs := LLeftListSs 
                ; LGreaterItemSs := LLeftItemSs 
                ELSE 
                  LGreaterListSs := LRightListSs 
                ; LGreaterItemSs := LRightItemSs 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          ; LCompare 
              := ItemsCompare 
                   ( Gram , LReinsertItemSs , LGreaterItemSs , AkLR1Set ) 
          ; IF LCompare = 1 
            THEN (* Parent is greatest of 3.  We are done. *) 
              LListRef ^ [ LListSs ] := LReinsertItemSs 
            ; EXIT 
            ELSE (* Push this item down to its greater child. *) 
              LListRef ^ [ LListSs ] := LGreaterItemSs 
            ; LListSs := LGreaterListSs 
            END (* IF *) 
          END (* LOOP *)  
        END (* FOR *) 
      ; CheckSetOrderList ( Gram , State ) 
      END (* IF *) 
    END LazyComputeSetOrderList 

(* VISIBLE: *) 
; PROCEDURE AreEqualLR1SetStates 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs1 , StateSs2 : LALRTypes . StateSsTyp 
    ) 
  : BOOLEAN 
  RAISES { Backout } 

  = VAR LItemSs1 , LItemSs2 : LALRTypes . ItemSsTyp 
  ; VAR LCompare : [ - 1 .. 1 ] 

  ; BEGIN 
      WITH WState1 = Gram . StateArrayRef ^ [ StateSs1 ] 
           , WState2 = Gram . StateArrayRef ^ [ StateSs2 ] 
      DO 
        IF WState1 . ItemCt # WState2 . ItemCt 
        THEN RETURN FALSE 
        ELSIF WState1 . NonclosureItemCt # WState2 . NonclosureItemCt 
        THEN RETURN FALSE 
        ELSE 
          LazyComputeSetOrderList ( Gram , WState1 ) 
        ; LazyComputeSetOrderList ( Gram , WState2 ) 
        ; Assert 
            ( NUMBER ( WState1 . SetOrderList ^ ) 
              = NUMBER ( WState2 . SetOrderList ^ ) 
            , AFT . A_AreEqualLR1SetStates_Unequal_list_lengths 
            ) 
        ; FOR RListSs := FIRST ( WState1 . SetOrderList ^ ) 
              TO LAST ( WState1 . SetOrderList ^ ) 
          DO 
            LItemSs1 := WState1 . SetOrderList ^ [ RListSs ] 
          ; LItemSs2 := WState2 . SetOrderList ^ [ RListSs ] 
          ; LCompare := ItemsCompare ( Gram , LItemSs1 , LItemSs2 , AkLR1Set ) 
          ; IF LCompare # 0 
            THEN RETURN FALSE 
            END (* IF *) 
          END (* FOR *) 
        ; RETURN TRUE  
        END (* IF *) 
      END (* WITH *) 
    END AreEqualLR1SetStates 

(* VISIBLE: *) 
; PROCEDURE AreEqualStates 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs1 , StateSs2 : LALRTypes . StateSsTyp 
    ; AutomatonKind : LALRTypes . AutomatonKindTyp 
    ) 
  : BOOLEAN 
  RAISES { Backout } 

  = VAR LItemSs1 , LToItemSs1 , LItemSs2 , LToItemSs2 : LALRTypes . ItemSsTyp 
  ; VAR LCompare : [ - 1 .. 1 ] 

  ; BEGIN (* AreEqualStates *)
      IF AutomatonKind = AkLR1Set 
      THEN 
        RETURN AreEqualLR1SetStates ( Gram , StateSs1 , StateSs2 ) 
      ELSE 
      (* We only look at the core items, since sets with the same core will 
         either have the same closure or will have sets of closure items that 
         differ only in the insertion of Tok__Empty in one or more places. 
         Such insertions make no difference to either the GOTO or the parsing
         actions, so we consider the states to be equivalent. 
      *) 
        WITH WState1 = Gram . StateArrayRef ^ [ StateSs1 ] 
             , WState2 = Gram . StateArrayRef ^ [ StateSs2 ] 
        DO
          IF WState1 . ItemCt # WState1 . ItemCt 
          THEN RETURN FALSE 
          ELSIF WState1 . NonclosureItemCt # WState2 . NonclosureItemCt 
          THEN RETURN FALSE 
          ELSE 
            LItemSs1 := WState1 . FirstItemSs 
          ; LItemSs2 := WState2 . FirstItemSs 
          ; LToItemSs1 := LItemSs1 + WState1 . ItemCt 
          ; LToItemSs2 := LItemSs2 + WState2 . ItemCt 
          ; LOOP 
              IF LItemSs1 = LToItemSs1 
              THEN RETURN TRUE 
              ELSE (* Another pair of items. *) 
                WITH WItem1 = Gram . ItemArrayRef ^ [ LItemSs1 ] 
                     , WItem2 = Gram . ItemArrayRef ^ [ LItemSs2 ] 
                DO IF WItem1 . DotPosition = 0 
                  THEN (* Item1 is a closure item. *)  
                    IF WItem2 . DotPosition = 0
                    THEN (* Both closure items.  Fall through to skip them. *) 
                    ELSE (* Closure/nonclosure *) 
                      RETURN FALSE 
                    END (* IF *) 
                  ELSE (* Item1 is a nonclosure item. *)
                    IF WItem2 . DotPosition = 0
                    THEN (* Nonclosure/closure. *) 
                      RETURN FALSE 
                    ELSE (* Two nonclosure items. *) 
                      LCompare 
                        := ItemsCompare 
                             ( Gram , LItemSs1 , LItemSs2 , AutomatonKind )
                    ; IF LCompare # 0 
                      THEN RETURN FALSE
                   (* ELSE Loop for more items. *) 
                      END (* IF *) 
                    END (* IF *) 
                  END (* IF *) 
                ; INC ( LItemSs1 ) 
                ; INC ( LItemSs2 ) 
                END (* WITH *) 
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *) 
        END (* WITH *) 
      END (* IF *) 
    END AreEqualStates 

(* VISIBLE: *) 
; PROCEDURE CheckLRwithLALR ( Gram : LRTable . GrammarTyp ) 
  (* Compare LR(1) lookahead token sets with LALR(1) lookaheads for merged
     states. 
  *) 

  = VAR LLRTokSet : IntSets . T 

  ; BEGIN 
      FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO WITH WState = Gram . StateArrayRef ^ [ RStateSs ]
         DO IF WState . LR0ListRepStateSs = RStateSs (* An LR(0) state. *) 
           THEN 
             FOR RItemInStateSs := 0 TO WState . ItemCt - 1 
             DO WITH WItem 
                        = Gram . ItemArrayRef 
                            ^ [ RItemInStateSs + WState . FirstItemSs ]  
               DO IF WItem . Rep = LALRTypes . ReprKindTyp . RedRep 
                 THEN (* ^nur fuer diese Items werden Sets errechnete. *)  
                   LLRTokSet := WItem . LR1Set 
                 ; IF NOT IntSets . IsEmpty ( WState . LR0ListStateSet )   
                   THEN 
                     PROCEDURE Visit ( OtherStateSs : IntSets . ValidElemT )
                     = BEGIN 
                         WITH WOtherState 
                              = Gram . StateArrayRef ^ [ OtherStateSs ]
                         , WOtherItem 
                           = Gram . ItemArrayRef 
                             ^ [ WOtherState . FirstItemSs + RItemInStateSs ] 
                         DO 
                           LLRTokSet 
                            := IntSets . Union 
                                 ( LLRTokSet , WOtherItem . LR1Set )
                         END (* WITH *) 
                       END Visit
                   ; BEGIN 
                       IntSets . ForAllDo ( WState . LR0ListStateSet , Visit ) 
                     END  (* Block *) 
                   END (* IF *) 
                 ; IF NOT IntSets . Equal ( LLRTokSet , WItem . Set ) 
                   THEN 
                     Assertions . MessageText 
                       ( "LALR-LR lookahead mismatch, state " 
                         & Fmt . Int ( RStateSs ) 
                         & ", item "
                         & Fmt . Int ( RItemInStateSs + WState . FirstItemSs ) 
                       ) 
                   END (* IF *) 
                 END (* IF *) 
               END (* WITH *)  
             END (* FOR *) 
           END (* IF *) 
         END (* WITH *) 
      END (* FOR *)  
    END CheckLRwithLALR 

(* Tables for several things that need to be atomized in sets. *) 
; CONST InitItemTblSize = 200 (* For one state. *)  
; CONST InitStateTblSize = 2000  

(* Common Tbl subtype, for all the tables we need here. *) 
; TYPE LALRTblTyp 
  = IntIntTbl . Default OBJECT 
      Gram : LRTable . GrammarTyp 
    END (* LALRTblTyp *) 

(* Table of LR(0) Items. *) 
; REVEAL LALRTypes . LR0ItemTblTyp 
  = LALRTblTyp BRANDED "LALRTypes.LR0ItemTblTyp" OBJECT 
    OVERRIDES 
      keyEqual := ItemEqualOverride  
    ; keyHash := ItemHashOverride  
    END (* ItemTblTyp *) 

; PROCEDURE ItemEqualOverride 
    ( Self : LALRTypes . LR0ItemTblTyp 
    ; READONLY LeftItemSs , RightItemSs : INTEGER 
    ) 
  : BOOLEAN 
  (* Wrapper to match interfaces. *) 

  = BEGIN 
      RETURN 
        ItemsCompare ( Self . Gram , LeftItemSs , RightItemSs , AkLR0List ) 
        = 0  
    END ItemEqualOverride  

; PROCEDURE ItemHashOverride 
    ( Self : LALRTypes . LR0ItemTblTyp ; READONLY ItemSs : INTEGER 
    ) 
  : Word . T 
  (* Wrapper to match interfaces. *) 

  = BEGIN 
      WITH WItem = Self . Gram . ItemArrayRef ^ [ ItemSs ] 
      DO 
        RETURN WItem . LR0ItemHashCode  
      END (* WITH *) 
    END ItemHashOverride

; PROCEDURE InitItemTbl ( Gram : LRTable . GrammarTyp ) 

  = BEGIN 
      IF Gram . LR0ItemTbl = NIL 
      THEN Gram . LR0ItemTbl := NEW ( LALRTypes . LR0ItemTblTyp , Gram := Gram )
      END (* IF *) 
    ; EVAL Gram . LR0ItemTbl . init ( InitItemTblSize ) 
    END InitItemTbl 

(* VISIBLE: *) 
; PROCEDURE UniqueLR0Item  
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs :  LALRTypes . ItemSsTyp 
    ; VAR IsNew : BOOLEAN 
    ) 
  : LALRTypes . ItemSsTyp 
  (* If there is already an equal item in Gram.LR0ItemTbl, set 
     IsNew FALSE and return its item subscript. 
     Otherwise, set IsNew TRUE and return ItemSs.
  *) 

(* TODO: Replace UniqueLR0ITem with just a production number set. *) 

  = VAR LResult : INTEGER := 0  

  ; BEGIN 
      IF Gram . LR0ItemTbl . get ( ItemSs , (*VAR*) LResult ) 
      THEN 
        IsNew := FALSE  
      ; RETURN LResult 
      ELSE 
        EVAL Gram . LR0ItemTbl . put ( ItemSs , ItemSs )  
      ; IsNew := TRUE 
      ; RETURN ItemSs 
      END (* IF *) 
    END UniqueLR0Item  

; PROCEDURE InitStateTbls ( Gram : LRTable . GrammarTyp ) 

  = BEGIN 
      IF Gram . LR1ListStateTbl = NIL 
      THEN Gram . LR1ListStateTbl 
             := NEW ( LALRTypes . LR1ListTblTyp , Gram := Gram ) 
      END (* IF *) 
    ; EVAL Gram . LR1ListStateTbl . init ( InitStateTblSize ) 

    ; IF Gram . LR1SetStateTbl = NIL 
      THEN Gram . LR1SetStateTbl 
             := NEW ( LALRTypes . LR1SetTblTyp , Gram := Gram )
      END (* IF *) 
    ; EVAL Gram . LR1SetStateTbl . init ( InitStateTblSize ) 

    ; IF Gram . LR0ListStateTbl = NIL 
      THEN Gram . LR0ListStateTbl 
             := NEW ( LALRTypes . LR0ListTblTyp , Gram := Gram )
      END (* IF *) 
    ; EVAL Gram . LR0ListStateTbl . init ( InitStateTblSize ) 
    END InitStateTbls 

(* Atomizing LR1List states: *) 

; REVEAL LALRTypes . LR1ListTblTyp 
  = LALRTblTyp BRANDED "LALRTypes.LR1ListTblTyp" OBJECT 
    OVERRIDES 
      keyEqual := LR1ListEqualOverride  
    ; keyHash := LR1ListHashOverride  
    END (* LR1ListTblTyp *) 

; PROCEDURE LR1ListEqualOverride 
    ( Self : LALRTypes . LR1ListTblTyp 
    ; READONLY LeftStateSs , RightStateSs : INTEGER 
    ) 
  : BOOLEAN 
  (* Wrapper to match interfaces. *) 

  = BEGIN 
      RETURN 
        AreEqualStates ( Self . Gram , LeftStateSs , RightStateSs , AkLR1List ) 
(* FIXME: How can we get Backout from AreEqualStates to propagate
          through here?  We would have to change Table so that Equal
          RAISES ANY . *)  
          
    END LR1ListEqualOverride  

; PROCEDURE LR1ListHashOverride 
    ( Self : LALRTypes . LR1ListTblTyp ; READONLY StateSs : INTEGER ) 
  : Word . T 
  (* Wrapper to match interfaces. *) 

  = BEGIN 
      WITH WState = Self . Gram . StateArrayRef ^ [ StateSs ] 
      DO 
        RETURN WState . LR1ListHashCode  
      END (* WITH *) 
    END LR1ListHashOverride

(* Atomizing LR1Set states: *) 

; REVEAL LALRTypes . LR1SetTblTyp 
  = LALRTblTyp BRANDED "LALRTypes.LR1SetTblTyp" OBJECT 
    OVERRIDES 
      keyEqual := LR1SetEqualOverride  
    ; keyHash := LR1SetHashOverride  
    END (* LR1SetTblTyp *) 

; PROCEDURE LR1SetEqualOverride 
    ( Self : LALRTypes . LR1SetTblTyp 
    ; READONLY LeftStateSs , RightStateSs : INTEGER 
    ) 
  : BOOLEAN 
  (* Wrapper to match interfaces. *) 

  = BEGIN 
      RETURN 
        AreEqualStates ( Self . Gram , LeftStateSs , RightStateSs , AkLR1Set ) 
    END LR1SetEqualOverride  

; PROCEDURE LR1SetHashOverride 
    ( Self : LALRTypes . LR1SetTblTyp ; READONLY StateSs : INTEGER ) 
  : Word . T 
  (* Wrapper to match interfaces. *) 

  = BEGIN 
      WITH WState = Self . Gram . StateArrayRef ^ [ StateSs ] 
      DO 
        RETURN WState . LR1SetHashCode  
      END (* WITH *) 
    END LR1SetHashOverride

(* Atomizing LR0List states: *) 

; REVEAL LALRTypes . LR0ListTblTyp 
  = LALRTblTyp BRANDED "LALRTypes.LR0ListTblTyp" OBJECT 
    OVERRIDES 
      keyEqual := LR0ListEqualOverride  
    ; keyHash := LR0ListHashOverride  
    END (* LR0ListTblTyp *) 

; PROCEDURE LR0ListEqualOverride 
    ( Self : LALRTypes . LR0ListTblTyp 
    ; READONLY LeftStateSs , RightStateSs : INTEGER 
    ) 
  : BOOLEAN 
  (* Wrapper to match interfaces. *) 

  = BEGIN 
      RETURN 
        AreEqualStates ( Self . Gram , LeftStateSs , RightStateSs , AkLR0List )
    END LR0ListEqualOverride  

; PROCEDURE LR0ListHashOverride 
    ( Self : LALRTypes . LR0ListTblTyp ; READONLY StateSs : INTEGER ) 
  : Word . T 
  (* Wrapper to match interfaces. *) 

  = BEGIN 
      WITH WState = Self . Gram . StateArrayRef ^ [ StateSs ] 
      DO 
        RETURN WState . LR0ListHashCode  
      END (* WITH *) 
    END LR0ListHashOverride

(* VISIBLE: *) 
; PROCEDURE UniqueState   
    ( Gram : LRTable . GrammarTyp 
    ; StateSs :  LALRTypes . StateSsTyp 
    ; VAR IsNew : BOOLEAN 
    ) 
  : LALRTypes . StateSsTyp 
  (* If there is already an equal state in Gram . LR0ListStateTbl, 
     Set IsNew FALSE and return its state subscript. 
     Otherwise, set IsNew TRUE and return StateSs.
  *) 

  = VAR LTbl : IntIntTbl . T 
  ; VAR LResult : INTEGER 

  ; BEGIN 
      LTbl := Gram . LR0ListStateTbl 
    ; IF LTbl . get ( StateSs , (*VAR*) LResult ) 
      THEN 
        IsNew := FALSE  
      ; RETURN LResult 
      ELSE 
        EVAL LTbl . put ( StateSs , StateSs )  
      ; IsNew := TRUE 
      ; RETURN StateSs 
      END (* IF *) 
    END UniqueState  

(* VISIBLE: *) 
; PROCEDURE ShortestDerivable 
    ( Gram : LRTable . GrammarTyp ; Tok : LbeStd . TokTyp ) 
  : LbeStd . LimitedTokCtTyp 

  = BEGIN (* ShortestDerivable *) 
      IF Gram # NIL 
      THEN 
(* TODO: Just for completeness, cases for the builtin tokens. *) 
        IF Tok < Gram . FirstTerminal 
        THEN 
          RETURN LbeStd . ParseCheckInfinity 
        ELSIF Tok < Gram . FirstAstNonterminal 
        THEN (* This includes ModToks. *) 
          RETURN 1 
        ELSIF Tok <= Gram . LastNonterminal 
        THEN 
          RETURN 
            Gram . NontermInfoArrayRef [ Tok - Gram . FirstAstNonterminal ] 
              . ShortestDerivable 
        ELSE 
          RETURN LbeStd . ParseCheckInfinity 
        END (* IF *) 
      ELSE RETURN 0 
      END (* IF *) 
    END ShortestDerivable 

(* VISIBLE: *) 
; PROCEDURE StripGrammar ( Gram : LRTable . GrammarTyp ) 
  (* Remove stuff only needed during LR generation. *) 

  = BEGIN
      IF Gram # NIL
      THEN
        Gram . ProdArrayRef := NIL 
      ; Gram . ItemArrayRef := NIL 
      ; Gram . StateArrayRef := NIL 
      ; Gram . NontermInfoArrayRef := NIL 
      ; Gram . NullableNTs := NIL 
      ; Gram . LR1ListStateTbl := NIL 
      ; Gram . LR1SetStateTbl := NIL 
      ; Gram . LR0ListStateTbl := NIL 
      ; Gram . LR0ItemTbl := NIL
      END (* IF *) 
    END StripGrammar 

; BEGIN 
  END LRUtils 
. 

