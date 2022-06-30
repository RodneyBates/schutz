
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Automaton.mi, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

MODULE Automaton 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert 
; FROM Failures IMPORT Backout  
; IMPORT IntSets 
; IMPORT LALRTypes 
; FROM LRTable IMPORT NontermKindTyp
; IMPORT LRUtils 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT MessageCodes 
; IMPORT PortTypes 

; TYPE AFT = MessageCodes . T 

; CONST InitItemCount = 200        (* Anfangsplatzgroesse fuer Items *) 
; CONST InitStateMultiplier = 2.5  

(* Required protocol: call sequence must fit the following 
   regular expression: 
     InitAutomaton 
     AddOperator* 
     (AddProduction|AddExternalProduction)* 
     AfterProductions 
     Build 
*) 

(* All visible procedures in this interface return harmlessly if Gram = NIL. *)

(* We use the technique described in:
     Johannes Roehrich: "Methods for the Automatic Construction of Error
     Correcting Parsers", Acta Informatica 13, 115-139 (1980) 
   for constructing a deterministically continuable parser.
*)

(* VISIBLE: *) 
; PROCEDURE InitAutomaton 
    ( Gram : LRTable . GrammarTyp 
    ; ProdCt : PortTypes . Int32Typ 
    ; FirstTerminal : LbeStd . TokTyp 
    ; LastTerminal : LbeStd . TokTyp 
    ; FirstAstNonterminal : LbeStd . TokTyp 
    ; LastAstNonterminal : LbeStd . TokTyp 
    ; FirstNonterminal : LbeStd . TokTyp 
    ; LastNonterminal : LbeStd . TokTyp 
    ; StartSymbol : LbeStd . TokTyp 
    ) 
  (* Initialisiert den Automat, d.h. die zur Automatenkonstruktion 
     noetigen Daten werden vom Module Rules uebernommen *) 

  = BEGIN (* InitAutomaton *) 
      IF Gram # NIL 
      THEN 
        Gram . ProdCt := ProdCt + 1 (* Allow for augmented start production. *)
      ; Gram . ProdArrayRef 
          := NEW ( LALRTypes . ProdVarArrayTyp , Gram . ProdCt ) 
      ; Gram . ItemArrayRef 
          := NEW ( LALRTypes . ItemVarArrayTyp , InitItemCount ) 
      ; Gram . StateArrayRef := NIL 
      ; Gram . FirstTerminal := FirstTerminal 
      ; Gram . LastTerminal := LastTerminal 
      ; Gram . FirstAstNonterminal := FirstAstNonterminal 
      ; Gram . LastAstNonterminal := LastAstNonterminal 
      ; Gram . FirstNonterminal := FirstNonterminal 
      ; Gram . LastNonterminal := LastNonterminal 
      ; Gram . StartSymbol := StartSymbol 
      ; Gram . NontermInfoArrayRef 
          := NEW 
               ( LALRTypes . NontermInfoArrayRefTyp 
               , Gram . LastNonterminal - Gram . FirstAstNonterminal + 1 
               ) 
      ; FOR RI := 0 TO NUMBER ( Gram . NontermInfoArrayRef ^ ) - 1 
        DO Gram . NontermInfoArrayRef ^ [ RI ] 
             := LALRTypes . NontermInfoTyp 
                  { Used := 0 
                  , ProdListRef := NIL 
                  , ShortestDerivable := LbeStd . ParseCheckInfinity 
                  , First := IntSets . Empty ( ) 
                  , Last := IntSets . Empty ( ) 
                  , Kind := NontermKindTyp . NtkNull 
                  } 
        END (* FOR *) 
      ; Gram . OperArrayRef 
          := NEW 
               ( LALRTypes . OperArrayRefTyp 
               , LastAstNonterminal - FirstTerminal + 1 
               ) 
      ; FOR RI := 0 TO NUMBER ( Gram . OperArrayRef ^ ) - 1 
        DO Gram . OperArrayRef ^ [ RI ] 
             := LALRTypes . OperTyp 
                  { Precedence := LRTable . PrecedenceNull   
                  , Ass := LRTable . AssocTyp . none 
                  } 
        END (* FOR *) 
      ; Gram . ProdCt := 0 
      ; Gram . ItemCt := 0 
      ; Gram . LR1ListStateCt := 0 
      ; Gram . LR1ReorderedStateCt := 0 
      ; Gram . LR1SetStateCt := 0 
      ; Gram . LR0ListStateCt := 0 
      ; Gram . LR1SetStateMergeCt := 0 
      ; Gram . LR0ListStateMergeCt := 0 
      ; Gram . NextProdSs := 0 
      ; Gram . NextItemSs := 0 
      ; Gram . NextStateSs := LALRTypes . FirstRealStateSs 
      ; Gram . IsBnf := TRUE 
      END (* IF *) 
    END InitAutomaton 

(* VISIBLE: *) 
; PROCEDURE ActualProdCt 
    ( Gram : LRTable . GrammarTyp ) : LRTable . ProdNoTyp 
  (* Call this any time after InitAutomaton. *) 

  = BEGIN (* ActualProdCt *) 
      IF Gram # NIL 
      THEN RETURN Gram . NextProdSs 
      ELSE RETURN 0 
      END (* IF *) 
    END ActualProdCt 

(* VISIBLE: *) 
; PROCEDURE FirstPrec 
    ( Gram : LRTable . GrammarTyp ) : LRTable . PrecedenceTyp 

  = BEGIN (* FirstPrec *) 
      IF Gram # NIL 
      THEN RETURN LRTable . PrecedenceLowestReal 
      ELSE RETURN LRTable . PrecedenceNull 
      END (* IF *) 
    END FirstPrec 

(* VISIBLE: *) 
; PROCEDURE NextPrec 
    ( Gram : LRTable . GrammarTyp ; Arg : LRTable . PrecedenceTyp ) 
    : LRTable . PrecedenceTyp 

  = BEGIN (* NextPrec *) 
      IF Gram # NIL 
      THEN RETURN Arg + 1 
(* TODO               ^Overflow?  Not very likely. *) 
      ELSE RETURN LRTable . PrecedenceNull  
      END (* IF *) 
    END NextPrec 

(* VISIBLE: *) 
; PROCEDURE AddOperator 
    ( Gram : LRTable . GrammarTyp 
    ; Operator : LbeStd . TokTyp (* ^Must be a terminal. *) 
    ; Precedence : LRTable . PrecedenceTyp 
    ; Associativity : LRTable . AssocTyp 
    ) 
  (* Call this any time after InitAutomaton. *) 

  = BEGIN (* AddOperator *) 
      IF Gram # NIL 
      THEN 
        Gram . OperArrayRef ^ [ Operator - Gram . FirstTerminal ] 
          := LALRTypes . OperTyp 
               { Precedence := Precedence , Ass := Associativity } 
      END (* IF *) 
    END AddOperator 

(* VISIBLE: *) 
; PROCEDURE NoteNontermKind 
    ( Gram : LRTable . GrammarTyp 
    ; Nonterm : LbeStd . TokTyp 
    ; Kind : NontermKindTyp 
    ; DefaultOnly : BOOLEAN := FALSE 
      (* ^Set it only if it is now NtkNull *) 
    ) 
  (* Call this any time after InitAutomaton. *) 

  = BEGIN (* NoteNontermKind *) 
      IF Gram # NIL 
      THEN 
        WITH 
          WNtInfo 
          = Gram . NontermInfoArrayRef 
              ^ [ Nonterm - Gram . FirstAstNonterminal ]
        DO
          IF NOT DefaultOnly OR WNtInfo . Kind = NontermKindTyp . NtkNull 
          THEN WNtInfo . Kind := Kind 
          END (* IF *) 
        END (* WITH *) 
      END (* IF *) 
    END NoteNontermKind 

(* VISIBLE: *) 
; PROCEDURE AddProduction 
    ( Gram : LRTable . GrammarTyp 
    ; Left : LbeStd . TokTyp (* A Nonterminal *) 
    ; Right : LRTable . TokArrayRefTyp 
      (* ^May be shared. May be NIL to denote the empty string. *) 
    ; Precedence : LRTable . PrecedenceTyp := LRTable . PrecedenceNull  
    ; Associativity := LRTable . AssocTyp . none 
    ; IsList : BOOLEAN := FALSE 
    ; OptionIdSet := LRTable . OptionIdSetEmpty
    ; BuildTok : LbeStd . TokTyp := LbeStd . Tok__Null 
    ) 
  RAISES { Backout } 
  (* Call this any time after InitAutomaton. *) 

  = VAR LProdCount , LNewProdCount : CARDINAL 
  ; VAR LNewProdArrayRef : LALRTypes . ProdVarArrayTyp 
  ; VAR LRHSLength : CARDINAL 

  ; BEGIN (* AddProduction *) 
      IF Gram # NIL 
      THEN 
        Assert
          ( Gram . FirstTerminal <= Left 
          , AFT . A_Automaton_AddProduction__Low_LHS_Token
          ) 
      ; IF Right = NIL 
        THEN LRHSLength := 0 
        ELSE LRHSLength := NUMBER ( Right ^ ) 
        END (* IF *) 
      ; FOR RI := 0 TO LRHSLength - 1 
        DO 
          Assert
            ( Gram . FirstTerminal <= Right ^ [ RI ]  
            , AFT . A_Automaton_AddProduction__Low_RHS_Token
            ) 
        END (* FOR *)  
      ; LProdCount := NUMBER ( Gram . ProdArrayRef ^ ) 
      ; IF LProdCount <= Gram . NextProdSs 
        THEN 
          LNewProdCount := 2 * LProdCount 
        ; LNewProdArrayRef 
            := NEW ( LALRTypes . ProdVarArrayTyp , LNewProdCount ) 
        ; SUBARRAY ( LNewProdArrayRef ^ , 0 , LProdCount ) 
            := Gram . ProdArrayRef ^ 
        ; Gram . ProdArrayRef := LNewProdArrayRef 
        END (* IF *) 
      ; WITH WProd = Gram . ProdArrayRef ^ [ Gram . NextProdSs ] 
        DO WProd . ProdNo := Gram . NextProdSs 
           (* This is redundant, but would allow for a renumbering of 
              productions, which apparently was once done in lalr. *) 
        ; WProd . Ass := Associativity 
        ; WProd . Precedence := Precedence 
        ; WProd . ReduceStates  
            := LALRTypes . StateSsListTyp { Used := 0 , Array := NIL } 
        ; WProd . ShortestDerivable := LbeStd . ParseCheckInfinity 
        ; WProd . Left := Left 
        ; WProd . BuildTok := BuildTok 
        ; WProd . IsList := IsList 
        ; IF Left >= Gram . FirstAstNonterminal 
          THEN 
            INC 
              ( Gram . NontermInfoArrayRef ^ 
                  [ Left - Gram . FirstAstNonterminal ] 
                . Used 
              ) 
          END (* IF *) 
        ; WProd . Right := Right 
        ; WProd . Len := LRHSLength 
        ; WProd . OptionIdSet := OptionIdSet 
        END (* WITH WProd *) 
      ; INC ( Gram . NextProdSs ) 
      END (* IF *) 
    END AddProduction 

(* VISIBLE: *) 
; PROCEDURE AddExternalProduction 
    ( Gram : LRTable . GrammarTyp 
    ; Left : LbeStd . TokTyp (* A Nonterminal *) 
    ; Right : LRTable . TokArrayRefTyp 
      (* ^May be shared. May be NIL to denote the empty string. *) 
    ; PrecedenceTok : LbeStd . TokTyp := LbeStd . Tok__Null 
      (* Otherwise, a terminal *) 
    ; IsList : BOOLEAN := FALSE 
    ; OptionIdSet := LRTable . OptionIdSetEmpty
    ; BuildTok : LbeStd . TokTyp := LbeStd . Tok__Null 
    ) 
  RAISES { Backout } 
  (* Call this any time after InitAutomaton and all calls on AddOperator. 
     Takes predecence and associativity from PrecedenceTok, if non-null, 
     otherwise by a default rule (from rightmost terminal in RHS). 
  *) 

  = VAR LTok : LbeStd . TokTyp 
  ; LPrecedence : LRTable . PrecedenceTyp 
  ; LAssociativity : LRTable . AssocTyp 

  ; BEGIN (* AddProduction *) 
      IF Gram # NIL 
      THEN 
        IF PrecedenceTok # LbeStd . Tok__Null 
        THEN (* explizite Prioritaet geht vor *) 
          WITH 
            WOper 
            = Gram . OperArrayRef ^ [ PrecedenceTok - Gram . FirstTerminal ] 
          DO 
            LPrecedence := WOper . Precedence 
          ; LAssociativity := WOper . Ass 
          END (* WITH *) 
        ELSE 
          LAssociativity := LRTable . AssocTyp . none 
          (* ^Initialisierung auf keine Associativitaet *) 
        ; LPrecedence := LRTable . PrecedenceNull (* und keine Prioritaet *) 
        ; IF Right # NIL 
          THEN 
            FOR LI := 0 TO NUMBER ( Right ^ ) - 1 
            DO 
(* TODO: Do this RtoL and stop when a precedence/assoc has been found. *) 
              LTok := Right ^ [ LI ] 
            ; Assert 
                ( Gram . FirstTerminal <= LTok 
                  AND LTok <= Gram . LastNonterminal 
                , AFT . A_AutomatonAddProduction_RhsTokOutOfRange 
                ) 
            ; IF Gram . FirstTerminal <= LTok 
                 AND LTok <= Gram . LastAstNonterminal 
              THEN 
                WITH 
                  WOper = Gram . OperArrayRef ^ [ LTok - Gram . FirstTerminal ]
                DO IF WOper . Ass # LRTable . AssocTyp . none 
                   THEN (* der letzte Operator innerhalb der Regel gilt *) 
                     LPrecedence := WOper . Precedence 
                   ; LAssociativity := WOper . Ass 
                   END (* IF *) 
                END (* WITH *) 
              END (* IF *) 
            END (* FOR *) 
          END (* IF *) 
        ; AddProduction 
            ( Gram 
            , Left 
            , Right 
            , LPrecedence 
            , LAssociativity 
            , IsList 
            , OptionIdSet 
            , BuildTok 
            ) 
        END (* IF *) 
      END (* IF *) 
    END AddExternalProduction 

; PROCEDURE SortIntoNTProdList 
    ( Gram : LRTable . GrammarTyp ; ProdSs : LRTable . ProdNoTyp ) 
  (* Die angegebene Produktion wird gem. ihrer linken Seite in die 
     zugh. ProdListRef ^ sortiert eingetragen *) 
  (* Not the global production list, the per-NT list for production's LHS. *)
  (* Sort is ascending on ShortestDerivable, which must have been computed.  
     This satisfies Roehrich's criterion. *)

  = VAR LI : LALRTypes . ListCountTyp 

  ; BEGIN (* SortIntoNTProdList *) 
      WITH WProd = Gram . ProdArrayRef ^ [ ProdSs ] 
      DO IF WProd . Left >= Gram . FirstAstNonterminal 
         THEN 
           WITH 
             WNontermInfo 
             = Gram . NontermInfoArrayRef ^ 
                 [ WProd . Left - Gram . FirstAstNonterminal ] 
           DO LI := WNontermInfo . Used 
           ; LOOP 
               IF LI <= 0 
               THEN 
                 EXIT 
               ELSE 
                 IF Gram . ProdArrayRef ^ 
                      [ WNontermInfo . ProdListRef ^ [ LI - 1 ] ] 
                    . ShortestDerivable 
                    > WProd . ShortestDerivable 
                 THEN 
                   WNontermInfo . ProdListRef ^ [ LI ] 
                     := WNontermInfo . ProdListRef ^ [ LI - 1 ] 
                 ; DEC ( LI ) 
                 ELSE 
                   EXIT 
                 END (* IF *) 
               END (* IF *) 
             END (* LOOP *) 
           (* neue Produktion eintragen *) 
           ; WNontermInfo . ProdListRef ^ [ LI ] := ProdSs 
           ; INC ( WNontermInfo . Used ) 
           END (* WITH *) 
         END (* IF *) 
      END (* WITH *) 
    END SortIntoNTProdList 

; PROCEDURE CompareShortestDerivableAgainstNullable 
    ( Gram : LRTable . GrammarTyp ) 
  RAISES { Backout } 

  = VAR LN1 , LN2 : BOOLEAN 

  ; BEGIN 
      FOR RNT := Gram . FirstAstNonterminal TO Gram . LastNonterminal 
      DO WITH WNTInfo 
              = Gram . NontermInfoArrayRef 
                ^ [ RNT - Gram . FirstAstNonterminal ]  
        DO
          LN1 := WNTInfo . ShortestDerivable = 0 
        ; LN2 := IntSets . IsElement ( RNT , Gram . NullableNTs ) 
        ; Assert 
            ( LN1 = LN2 
            , AFT . A_CompareShortestDerivableAgainstNullable_Mismatch
            ) 
        END (* WITH *) 
      END (* FOR *) 
    END CompareShortestDerivableAgainstNullable 

; PROCEDURE MakeAugmentProduction  
    ( Gram : LRTable . GrammarTyp ) 
  RAISES { Backout } 

  = VAR LRight : LRTable . TokArrayRefTyp 

  ; BEGIN (* MakeAugmentProduction *) 
    (* Construct the augmenting production. *) 
      LRight := NEW ( LRTable . TokArrayRefTyp , 2 ) 
    ; LRight ^ [ 0 ] := Gram . StartSymbol (* Unaugmented start symbol. *)
    ; LRight ^ [ 1 ] := LbeStd . Tok__EndOfImage 
    ; AddProduction 
        ( Gram 
        , Left := LbeStd . Tok__Augment 
        , Right := LRight 
        , BuildTok := LbeStd . Tok__Augment 
        ) 
      (* ^The augmenting production. *) 
    ; Gram . AugmentProdSs := Gram . NextProdSs - 1 
    END MakeAugmentProduction 

; PROCEDURE AfterProductions ( Gram : LRTable . GrammarTyp ) 
  RAISES { Backout } 
  (* Some processing done after all productions have been added, but before
     Build.  This once needed to be separated from Build.
  *) 

  = VAR LNullableNTs : IntSets . T 

  ; BEGIN (* AfterProductions *) 
      IF Gram # NIL 
      THEN 
        FOR RI := 0 TO Gram . LastNonterminal - Gram . FirstAstNonterminal 
        DO WITH WNontermInfo = Gram . NontermInfoArrayRef ^ [ RI ] 
           DO WNontermInfo . ProdListRef 
                := NEW ( LALRTypes . ProdNoArrayRefTyp , WNontermInfo . Used ) 
           ; WNontermInfo . Used := 0 
           END (* WITH *) 
        END (* FOR *) 
      ; MakeAugmentProduction ( Gram ) 
      ; LRUtils . ComputeShortestDerivable ( Gram ) 
      ; FOR RProdSs := 0 TO Gram . NextProdSs - 1 
        DO 
  IF RProdSs = 24 OR RProdSs = 25 
  THEN Assertions . DoNothing ( ) 
  END 
; 
          SortIntoNTProdList ( Gram , RProdSs ) 
        END (* FOR *) 
      ; LRUtils . ComputeNullabilityAndFirst ( Gram ) 
        (* ^We will need these for either LALR(1) or full LR(1) construction. *)
; LNullableNTs := LRUtils . NullableNonterms ( Gram ) 
; Assert 
    ( IntSets . Equal ( LNullableNTs , Gram . NullableNTs ) 
    , AFT . A_AfterProductions_Nullables_disagree
    ) 
; CompareShortestDerivableAgainstNullable ( Gram ) 
(* TODO: ^Remove these after sufficient testing. *) 
      ; Assert 
          ( NUMBER 
              ( Gram . NontermInfoArrayRef ^ 
                  [ Gram . StartSymbol - Gram . FirstAstNonterminal ] 
                . ProdListRef ^ 
              ) 
            >= 1 
          , AFT . A_AfterProductions_NoStartProduction 
          ) 
      END (* IF *) 
    END AfterProductions 

; PROCEDURE CreateState ( Gram : LRTable . GrammarTyp ) 
  (* Beschaffe den naechsten State und initialisiere ihn mit 
     dem naechsten (aktuellen+1) Item *) 

  = VAR LStateCount : PortTypes . Int32Typ 
  ; VAR LNewStateCount : PortTypes . Int32Typ 
  ; VAR LNewStateArrayRef : LALRTypes . StateVarArrayTyp 

  ; BEGIN (* CreateState *) 
    (* Ensure state array is allocated and big enough. *) 
      IF Gram . StateArrayRef = NIL 
      THEN 
        Gram . StateArrayRef 
          := NEW 
               ( LALRTypes . StateVarArrayTyp 
               , TRUNC ( FLOAT ( Gram . NextProdSs ) * InitStateMultiplier ) 
               ) 
      ELSE 
        LStateCount := NUMBER ( Gram . StateArrayRef ^ ) 
      ; IF LStateCount <= Gram . NextStateSs 
        THEN 
          LNewStateCount := 2 * LStateCount 
        ; LNewStateArrayRef 
            := NEW ( LALRTypes . StateVarArrayTyp , LNewStateCount ) 
        ; SUBARRAY ( LNewStateArrayRef ^ , 0 , LStateCount ) 
            := Gram . StateArrayRef ^ 
        ; Gram . StateArrayRef := LNewStateArrayRef 
        END (* IF *) 
      END (* IF *) 

    ; WITH WState = Gram . StateArrayRef ^ [ Gram . NextStateSs ] 
      DO WState . ItemCt := 0 
      ; WState . NonclosureItemCt := 0 
      ; WState . FirstItemSs := Gram . NextItemSs 
      ; WState . ReduceItemSs := LALRTypes . ItemSsNull 
      ; WState . Action := LRTable . StateNoNull 
      ; WState . Kind := LALRTypes . StateKindTyp . None 
      ; WState . LR1ListHashCode := 0 
      ; WState . LR1SetHashCode := 0 
      ; WState . LR0ListHashCode := 0 
      ; WState . IsTargetOfAstNTTransition := FALSE 
      ; WState . LR1SetRepStateSs := Gram . NextStateSs (* Self-representative*)
      ; WState . LR1SetStateSet := IntSets . Empty ( ) 
      ; WState . LR0ListRepStateSs := Gram . NextStateSs (* Self-rep. *)
      ; WState . LR0ListStateSet := IntSets . Empty ( ) 
      END (* WITH *) 
    ; INC ( Gram . NextStateSs ) 
    END CreateState 

; PROCEDURE CreateItem 
    ( Gram : LRTable . GrammarTyp 
    ; ProdSs : LRTable . ProdNoTyp 
    ; ReadSym : LbeStd . TokTyp 
    ; DotPos : LbeStd . TokNoTyp 
    ; PrevItemSs : LALRTypes . ItemSsTyp 
    ; LookaheadSet : IntSets . T 
    ; IsClosure : BOOLEAN 
    ) 
  (* Beschaffe das naechste Item *)
  (* Put the new item at the end of the item array, with the next available
     ItemSs. *)  

  = VAR LItemCount : PortTypes . Int32Typ 
  ; VAR LNewItemCount : PortTypes . Int32Typ 
  ; VAR LNewItemArrayRef : LALRTypes . ItemVarArrayTyp 

  ; BEGIN (* CreateItem *) 

    (* Ensure item array is allocated and adequate. *) 
      IF Gram . ItemArrayRef = NIL 
      THEN 
        Gram . ItemArrayRef 
          := NEW ( LALRTypes . ItemVarArrayTyp , InitItemCount ) 
      ELSE 
        LItemCount := NUMBER ( Gram . ItemArrayRef ^ ) 
      ; IF LItemCount <= Gram . NextItemSs 
        THEN 
          LNewItemCount := 2 * LItemCount 
        ; LNewItemArrayRef 
            := NEW ( LALRTypes . ItemVarArrayTyp , LNewItemCount ) 
        ; SUBARRAY ( LNewItemArrayRef ^ , 0 , LItemCount ) 
            := Gram . ItemArrayRef ^ 
        ; Gram . ItemArrayRef := LNewItemArrayRef 
        END (* IF *) 
      END (* IF *) 

    (* Initialize the new item *) 
    ; WITH WItem = Gram . ItemArrayRef ^ [ Gram . NextItemSs ] 
      DO WItem . ProdSs := ProdSs 
      ; WItem . DotPosition := DotPos 
      ; WItem . ReadSet := IntSets . Empty ( ) 
      ; WItem . Set := IntSets . Empty ( ) 
      ; WItem . LR1Set := IntSets . Empty ( )  
      ; WItem . Relation . Used := 0 
      ; WItem . Relation . Array := NIL 
      ; WItem . ReadTok := ReadSym 
      ; WItem . Rep := LALRTypes . ReprKindTyp . NoRep 
      ; WItem . RepItemSs := LALRTypes . ItemSsNull 
      ; WItem . GoToStateSs := LALRTypes . StateSsNull 
      ; WItem . PrevItemSs := PrevItemSs 
      ; WItem . LR1Set := LookaheadSet 
      ; WItem . Number := 0 
      END (* WITH *) 
    ; WITH WState = Gram . StateArrayRef ^ [ Gram . NextStateSs - 1 ] 
      DO 
        INC ( WState . ItemCt ) 
      ; INC ( WState . NonclosureItemCt , ORD ( NOT IsClosure ) ) 
      END (* WITH *) 
    ; INC ( Gram . NextItemSs ) 
    END CreateItem 

; PROCEDURE DiscardMostRecentItem 
    ( Gram : LRTable . GrammarTyp ; IsClosure : BOOLEAN 
) 

  = BEGIN 
      DEC ( Gram . NextItemSs ) 
    ; WITH WState = Gram . StateArrayRef ^ [ Gram . NextStateSs - 1 ] 
      DO 
        DEC ( WState . ItemCt ) 
      ; DEC ( WState . NonclosureItemCt , ORD ( NOT IsClosure ) ) 
      END (* WITH *) 
    END DiscardMostRecentItem 

; PROCEDURE CloseItem 
    ( Gram : LRTable . GrammarTyp ; ToCloseItemSs : LALRTypes . ItemSsTyp ) 
  RAISES { Backout } 
  (* Add closure items for the most recently added item, depth-first. *) 
(* TODO: This procedure is confusing.  It uses the latest item (which changes
         during recursion) and the latest state (which does not). *) 

  = VAR LUniqueItemSs : LALRTypes . ItemSsTyp 
  ; VAR LProdSs : LRTable . ProdNoTyp 
  ; VAR LReadTok : LbeStd . TokNoTyp 
  ; VAR LIsNew : BOOLEAN := FALSE 

  ; BEGIN (* CloseItem *) 
      WITH WClosingItem = Gram . ItemArrayRef ^ [ ToCloseItemSs ] 
      DO (* Add direct closure items for ToCloseItemSs *) 
         IF Gram . FirstAstNonterminal <= WClosingItem . ReadTok 
            AND WClosingItem . ReadTok <= Gram . LastNonterminal 
         THEN (* Symbol nach den Punkt ein Nichtterminal ist. *) 
           WITH 
             WNontermInfo 
             = Gram . NontermInfoArrayRef ^ 
                 [ WClosingItem . ReadTok - Gram . FirstAstNonterminal ] 
           DO
             FOR RI := 0 TO WNontermInfo . Used - 1 
             (* All productions with this terminal as LHS. *) 
(* CHECK: Can this array be made just the right size, so we can use NUMBER
       instead of Used?  And then remove Used? 
*) 
             DO (* Fuege ein Item hinzu, falls dies noch nicht vorhanden *) 
               LProdSs := WNontermInfo . ProdListRef ^ [ RI ] 
             ; WITH WProd = Gram . ProdArrayRef ^ [ LProdSs ] 
               DO (* LReadTok fuer Huelle item bestimmen. *) 
                 IF WProd . Len > 0 
                 THEN LReadTok := WProd . Right ^ [ 0 ] 
                 ELSE LReadTok := LbeStd . Tok__Null 
                 END (* IF *) 
               ; CreateItem (* A closure item. *) 
                   ( Gram 
                   , LProdSs 
                   , LReadTok 
                   , DotPos := 0 
                   , PrevItemSs := ToCloseItemSs 
                   , LookaheadSet := IntSets . Empty ( ) 
                   , IsClosure := TRUE 
                   ) 
               ; LRUtils . ComputeLastItemLR0Hash ( Gram ) 
               ; LUniqueItemSs 
                   := LRUtils . UniqueLR0Item 
                        ( Gram , Gram . NextItemSs - 1 , (*VAR*) LIsNew ) 
                      (* ^Only the LR(0) part is used for uniqueness. *) 
               ; IF LIsNew 
                 THEN 
                   INC ( Gram . ItemCt ) 
                 ; CloseItem ( Gram , LUniqueItemSs )
                 ELSE 
                   DiscardMostRecentItem ( Gram , IsClosure := TRUE ) 
                 END (* IF *)
               END (* WITH *) 
             END (* FOR *) 
           END (* WITH WNontermInfo *) 
         END (* IF *) 
      END (* WITH *) 
    END CloseItem 

; PROCEDURE MakeFirstState 
    ( Gram : LRTable . GrammarTyp ) : LALRTypes . StateSsTyp 
  RAISES { Backout } 

  = VAR LIsNew : BOOLEAN 
  ; VAR LResult : LALRTypes . StateSsTyp 

  ; BEGIN (* MakeFirstState *)
      WITH WAugProd = Gram . ProdArrayRef ^ [ Gram . AugmentProdSs ] 
      DO (* Bilde einen neun Zustand *) 
        LRUtils . InitItemTbl ( Gram )  
      ; CreateState ( Gram ) 

      (* Beschaffe neues Item und trage die Produktion mit dem neuen 
         Startsymbol ein *) 
      ; CreateItem 
          ( Gram 
          , Gram . AugmentProdSs  
          , ReadSym := WAugProd . Right ^ [ 0 ] (* Unaugmented start symbol. *)
          , DotPos := 0 
          , PrevItemSs := LALRTypes . ItemSsNull 
          , LookaheadSet := IntSets . Empty ( ) 
          , IsClosure := FALSE (* Despite DotPos := 0. *)  
          ) 
        (* ^The augmenting item.  A nonclosure item. *) 
      ; LRUtils . ComputeLastItemLR0Hash ( Gram ) 
      ; LRUtils . ComputeNewestStateHashContributions ( Gram ) 
      ; INC ( Gram . ItemCt ) 

      (* Bilde Huelle *) 
      ; CloseItem ( Gram , Gram . NextItemSs - 1 ) 
 
      ; LResult 
          := LRUtils . UniqueState 
               ( Gram , Gram . NextStateSs - 1 , (*VAR, Dead*) LIsNew )
      ; Assert 
          ( LResult = Gram . NextStateSs - 1 
          , AFT . A_MakeFirstState_Duplicate_initial_state 
          ) 
      ; INC ( Gram . LR0ListStateCt ) 
      ; RETURN LResult 
      END (* WITH *) 
    END MakeFirstState 

; PROCEDURE GoTo 
    ( Gram : LRTable . GrammarTyp 
    ; FromStateSs : LALRTypes . StateSsTyp 
    ; Symbol : LbeStd . TokTyp 
    ; VAR IsNew : BOOLEAN 
(* TODO ^This output parameter is dead everywhere.  Remove it. *) 
    ) 
  : LALRTypes . StateSsTyp 
  RAISES { Backout } 
  (* Either locate state GOTO(FromStateSs,Symbol), if it already exists, or
     if not, construct it.  Return its StateSs either way. 
  *) 

  = VAR LNewStateSs : LALRTypes . StateSsTyp 
  ; VAR LProdSs : LRTable . ProdNoTyp 
  ; VAR LNewDotPos : LbeStd . TokNoTyp 
  ; VAR LUniqueLR0ListStateSs : LALRTypes . StateSsTyp 
  ; VAR LNewReadTok : LbeStd . TokTyp 
  ; VAR LIsNewLR0List : BOOLEAN 

  ; BEGIN (* GoTo *) 
    (* Beschaffe neuen State *) 
      LRUtils . InitItemTbl ( Gram )  
    ; CreateState ( Gram ) 
    ; LNewStateSs := Gram . NextStateSs - 1 
    ; WITH WFromState = Gram . StateArrayRef ^ [ FromStateSs ] 
      DO 
        FOR RFromItemSs := WFromState . FirstItemSs 
            TO WFromState . FirstItemSs + WFromState . ItemCt - 1 
        DO (* Fuer alle Items *) 
          WITH WFromItem = Gram . ItemArrayRef ^ [ RFromItemSs ] 
          DO IF WFromItem . ReadTok = Symbol 
             THEN (* Mit Symbol nach den Punkt *) 
               LProdSs := WFromItem . ProdSs 
             ; LNewDotPos := WFromItem . DotPosition + 1 
             ; WITH WProd = Gram . ProdArrayRef ^ [ LProdSs ] 
               DO (* erweitere den Zustand *) 
                  IF LNewDotPos < WProd . Len 
                  THEN LNewReadTok := WProd . Right ^ [ LNewDotPos ] 
                  ELSE LNewReadTok := LbeStd . Tok__Null 
                  END (* IF *) 
               ; CreateItem 
                   ( Gram 
                   , LProdSs 
                   , LNewReadTok 
                   , LNewDotPos 
                   , PrevItemSs := RFromItemSs 
                   , LookaheadSet := WFromItem . LR1Set 
                   , IsClosure := FALSE (* This is a Goto item. *) 
                   ) 
               ; LRUtils . ComputeLastItemLR0Hash ( Gram ) 
               ; LRUtils . ComputeNewestStateHashContributions ( Gram ) 
               ; INC ( Gram . ItemCt ) 
               ; CloseItem ( Gram , Gram . NextItemSs - 1 ) 
               END (* WITH *) 
             END (* IF *) 
          END (* WITH *) 
        END (* FOR *) 
      ; LUniqueLR0ListStateSs 
          := LRUtils . UniqueState 
               ( Gram , LNewStateSs , (*VAR*) LIsNewLR0List )
      (* Trage den berechneten State in NextStateSs ein *) 
      ; FOR RFromItemSs := WFromState . FirstItemSs 
            TO WFromState . FirstItemSs + WFromState . ItemCt - 1 
        DO (* Fuer alle Items *) 
           WITH WFromItem = Gram . ItemArrayRef ^ [ RFromItemSs ] 
           DO (* nach dem Punkt ist zu bearbeiten Mit Symbol danach. *)
              IF WFromItem . ReadTok = Symbol 
              THEN (* Trage den Folgezustand ein *) 
                WFromItem . GoToStateSs := LUniqueLR0ListStateSs 
              END (* IF *) 
           END (* WITH *) 
        END (* FOR *) 
      ; IF NOT LIsNewLR0List 
        THEN (* Discard the (duplicate) LR(1) list state we just created. *)   
          DEC ( Gram . NextStateSs ) 
        END (* IF *) 
      END (* WITH WFromState *) 
    ; RETURN LUniqueLR0ListStateSs 
    END GoTo 

(* VISIBLE: *) 
; PROCEDURE Build ( Gram : LRTable . GrammarTyp ) 
  RAISES { Backout } 
  (* Build the LR(0) automaton, after precedence info and productions 
     have been inserted. *) 

  = VAR LTok : LbeStd . TokTyp 
  ; VAR LFromStateSs : LALRTypes . StateSsTyp 
  ; VAR LIsNew : BOOLEAN 

  ; BEGIN (* Build *) 
      IF Gram # NIL 
      THEN 
        LRUtils . InitStateTbls ( Gram ) 
      ; LFromStateSs := MakeFirstState ( Gram ) 
      ; LOOP 
          WITH WFromState = Gram . StateArrayRef ^ [ LFromStateSs ] 
          DO FOR RFromItemSs := WFromState . FirstItemSs 
                 TO WFromState . FirstItemSs + WFromState . ItemCt - 1 
             DO (* Symbol nach dem Punkt ist zu bearbeiten *) 
                LTok := Gram . ItemArrayRef ^ [ RFromItemSs ] . ReadTok 
             ; IF Gram . FirstTerminal <= LTok 
                  AND LTok <= Gram . LastNonterminal 
               THEN (* gibt es ein naechstes Symbol, trage es ein. *) 
                 EVAL 
                   GoTo ( Gram , LFromStateSs , LTok , (* VAR, Dead *) LIsNew )
               END (* IF *) 
             END (* FOR *) 
          END (* WITH *) 
        ; INC ( LFromStateSs ) 
        ; IF LFromStateSs >= Gram . NextStateSs THEN EXIT END (* IF*) 
        END (* LOOP *) 
      END (* IF *) 
    END Build 

; BEGIN (* Automaton *) 
  END Automaton 
. 

