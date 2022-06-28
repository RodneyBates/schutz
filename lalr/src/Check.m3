
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Check.m1, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* check and repair inconsistences *) 

MODULE Check 

; IMPORT Fmt 
; IMPORT Thread
; IMPORT Wr 

; IMPORT Assertions 
; FROM Assertions IMPORT CantHappen , AssertionFailure  
; IMPORT Debug 
; FROM Debug 
  IMPORT tConflict , DebugHead , DebugState , DebugEnd 
  , InformIgnored , InformKept , InformConflict 
  , InformSublist , InformLowPri , InformRightAss , InformLeftAss 
; IMPORT Infos 
; IMPORT IntSets 
; IMPORT LALRTypes 
; FROM LALRTypes IMPORT ReprKindTyp 
; IMPORT LbeStd 
; IMPORT LRTable 
; FROM LRTable IMPORT AssocTyp , NontermKindTyp , PrecedenceNull  
; IMPORT MessageCodes 
; IMPORT Messages 

; TYPE AFT = MessageCodes . T 

; VAR DoRepair : BOOLEAN := TRUE (* For control with debugger. *) 

; CONST ParseInfoManFileNameSuffix = "ParseInfoMan" 
; CONST ParseInfoGenFileNameSuffix = "ParseInfoGen" 

; PROCEDURE RepairItemSet 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : LALRTypes . StateSsTyp 
    ; ConflictingLookaheadSet : IntSets . T 
    ; VAR (* IN OUT *) IsOK : BOOLEAN 
    ) 
  RAISES { AssertionFailure } 

  = VAR RisShiftRedSet : IntSets . T 
  ; VAR RisRedRedSet : IntSets . T 
  ; VAR RisShiftRedRedSet : IntSets . T 
  ; VAR RisRepShiftRedSet : IntSets . T 
  ; VAR RisRepRedRedSet : IntSets . T 
  ; VAR RepReadRedRedSet : IntSets . T 
  ; VAR RisAutoRepShiftRedSet : IntSets . T 
  ; VAR RisAutoRepRedRedSet : IntSets . T 
  ; VAR RisAutoRepShiftRedRedSet : IntSets . T 
  ; VAR RisSublistSet : IntSets . T 

  ; PROCEDURE RisIsSublistSepProd  
      ( READONLY Prod : LALRTypes . ProdTyp ; LookaheadTok : LbeStd . TokTyp ) 
    : BOOLEAN 
    (* Prod is a production for a sublist token, of the form we expect 
       to be ambiguous, and its first separator is Lookahead. *) 

    = VAR LResult : BOOLEAN  
    ; VAR LRhsLast : CARDINAL 

    ; BEGIN 
        LResult := FALSE 
      ; IF Gram . NontermInfoArrayRef 
             ^ [ Prod . Left - Gram . FirstAstNonterminal ] . Kind 
           = NontermKindTyp . NtkSublist 
        THEN (* LHS is a sublist token. *)   
          IF Prod . Right # NIL 
          THEN 
            LRhsLast := LAST ( Prod . Right ^ ) 
          ; IF LRhsLast >= 2 
               AND Prod . Right ^ [ 1 ] = LookaheadTok 
                  (* ^Only the first separator will contribute to the 
                     shift-reduce conflict we expect from this type of 
                     production.  It will be the lookahead for an ambiguous 
                     reduce. *) 
            THEN 
              LResult := TRUE 
            END (* IF *) 
          END (* IF *) 
        END (* IF *)  
      ; RETURN LResult 
      END RisIsSublistSepProd 

  ; PROCEDURE RisIsSublistNoSepProd  
      ( READONLY Prod : LALRTypes . ProdTyp ) 
    : BOOLEAN 
    (* Prod is a production for a sublist token, of the form we expect 
       to be ambiguous, and its first separator is Lookahead. *) 

    = VAR LResult : BOOLEAN  

    ; BEGIN 
        LResult := FALSE 
      ; IF Gram . NontermInfoArrayRef 
             ^ [ Prod . Left - Gram . FirstAstNonterminal ] . Kind 
           = NontermKindTyp . NtkSublist 
        THEN (* LHS is a sublist token. *)   
          IF Prod . Right # NIL 
             AND LAST ( Prod . Right ^ ) = 1 
          THEN 
            LResult := TRUE 
          END (* IF *) 
        END (* IF *)  
      ; RETURN LResult 
      END RisIsSublistNoSepProd 

  ; PROCEDURE RisToken ( LookaheadTok : IntSets . ValidElemT ) 
    RAISES { AssertionFailure } 

    = VAR LRedItemCt , LShiftItemCt : CARDINAL 
    ; VAR LRedItemRetainedCt , LShiftItemRetainedCt : CARDINAL  
    ; VAR LUsePrecAndAssoc : BOOLEAN 
    ; VAR LAssocOfMaxPrec : AssocTyp    
    ; VAR LAssocOfMaxPrecRed : AssocTyp    
    ; VAR LAssocOfMaxPrecShift : AssocTyp 
    ; VAR LMaxPrec : LRTable . PrecedenceTyp 
    ; VAR LMaxRedPrec : LRTable . PrecedenceTyp 
    ; VAR LMaxShiftPrec : LRTable . PrecedenceTyp 
    ; VAR LMinRedProdNo : LRTable . ProdNoTyp 
    ; VAR LReduceSublistSepSet : IntSets . T 
          (* ^Sublist tokens with a reduce item on LookaheadTok and first 
             separator = LookaheadTok in this state. *) 
    ; VAR LReduceSublistNoSepSet : IntSets . T 
          (* Sublist tokens with a reduce item with no separator. *) 
    ; VAR LConflictFree : BOOLEAN 

    ; BEGIN (* RisToken *) 
        LUsePrecAndAssoc := TRUE 
      ; LRedItemCt := 0 
      ; LShiftItemCt := 0 
      ; LRedItemRetainedCt := 0 
      ; LShiftItemRetainedCt := 0 
      ; LMaxRedPrec := PrecedenceNull  
      ; LAssocOfMaxPrecRed := AssocTyp . none 
      ; LMaxShiftPrec := PrecedenceNull 
      ; LAssocOfMaxPrecShift := AssocTyp . none 
      ; LReduceSublistSepSet := IntSets . Empty ( ) 
      ; LReduceSublistNoSepSet := IntSets . Empty ( ) 
      ; LMinRedProdNo := LRTable . ProdNoInfinity 

      ; WITH WState = Gram . StateArrayRef ^ [ StateSs ] 
        DO 

        (* Make a pass over the state, looking for sublist reduce items. *) 
          FOR RItemSs := WState . FirstItemSs 
              TO WState . FirstItemSs + WState . ItemCt - 1 
          DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
             DO IF WItem . Rep = ReprKindTyp . RedRep 
                   AND IntSets . IsElement ( LookaheadTok , WItem . Set ) 
                THEN (* Reduce item on Lookahead. *) 
                  WITH WProd = Gram . ProdArrayRef ^ [ WItem . ProdSs ] 
                  DO IF RisIsSublistSepProd ( WProd , LookaheadTok ) 
                    THEN 
                      LReduceSublistSepSet 
                        := IntSets . Include 
                             ( LReduceSublistSepSet , WProd . Left ) 
                    END (* IF *) 
                  ; IF RisIsSublistNoSepProd ( WProd ) 
                    THEN 
                      LReduceSublistNoSepSet 
                        := IntSets . Include 
                             ( LReduceSublistNoSepSet , WProd . Left ) 
                    END (* IF *) 
                  END (* WITH WProd *) 
                END (* IF *) 
             END (* WITH WItem *) 
          END (* FOR Items in WState *) 

        (* If there were any sublist reduce items, make another pass over the 
           state, repairing expected ambiguities due to sublist productions, 
           by discarding shifts. *) 
        ; IF IntSets . Card ( LReduceSublistSepSet ) > 0 
             OR IntSets . Card ( LReduceSublistNoSepSet ) > 0 
          THEN 
            FOR RItemSs := WState . FirstItemSs 
                 TO WState . FirstItemSs + WState . ItemCt - 1 
            DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
               DO IF WItem . Rep IN LALRTypes . ReprKindSetSentential 
                  THEN (* Shift item. *) 
                    WITH WProd = Gram . ProdArrayRef ^ [ WItem . ProdSs ] 
                    DO IF RisIsSublistSepProd ( WProd , LookaheadTok ) 
                          AND WItem . ReadTok = LookaheadTok 
                          AND IntSets . IsElement 
                                ( WProd . Left , LReduceSublistSepSet ) 
                      THEN (* Shift item for sublist with separators. *) 
                        IF Verbose 
                        THEN 
                          InformSublist ( Gram , RItemSs , LookaheadTok ) 
                        END (* IF *) 
                      ; WItem . Rep 
                          := LALRTypes . IgnoredCounterpart ( WItem . Rep ) 
                      ; RisSublistSet 
                          := IntSets . Include ( RisSublistSet , LookaheadTok )
                      END (* IF *) 
                    ; IF ( WItem . DotPosition = 0 
                           AND IntSets . IsElement 
                                 ( WItem . ReadTok , ConflictingLookaheadSet ) 
(* FIXME: Here, we really need  WItem . ReadTok to be in 
          FIRST ( some member of LReduceSublistNoSepSet ) 
          (or an argument why this must follow.) *) 
                         ) 
                         OR ( WItem . DotPosition = 1 
                              AND RisIsSublistNoSepProd ( WProd ) 
                            ) 
                      THEN (* Shift item for sublist with no separators. *) 
                        IF Verbose 
                        THEN 
                          InformSublist ( Gram , RItemSs , LookaheadTok ) 
                        END (* IF *) 
                      ; WItem . Rep 
                          := LALRTypes . IgnoredCounterpart ( WItem . Rep ) 
                      ; RisSublistSet 
                          := IntSets . Include ( RisSublistSet , LookaheadTok )
                      END (* IF *)  
                    END (* WITH *) 
                  END (* IF *) 
               END (* WITH WItem *) 
            END (* FOR Items in WState *) 
          END (* IF *) 

        (* Make another pass over the state, gathering info about items. *) 
        ; FOR RItemSs := WState . FirstItemSs 
                 TO WState . FirstItemSs + WState . ItemCt - 1 
            DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
               DO IF WItem . Rep = ReprKindTyp . RedRep 
                     AND IntSets . IsElement ( LookaheadTok , WItem . Set ) 
                  THEN (* Reduce item on Lookahead. *) 
                    INC ( LRedItemCt )
                  ; WITH WProd = Gram . ProdArrayRef ^ [ WItem . ProdSs ] 
                    DO IF WProd . Precedence = PrecedenceNull  
                       THEN 
                         LUsePrecAndAssoc := FALSE 
                       ELSIF WProd . Precedence > LMaxRedPrec 
                       THEN 
                         LMaxRedPrec := WProd . Precedence 
                       ; LAssocOfMaxPrecRed := WProd . Ass 
                       END (* IF *) 
                    ; IF WProd . ProdNo < LMinRedProdNo 
                      THEN 
                        LMinRedProdNo := WProd . ProdNo 
                      END (* IF *) 
                    END (* WITH WProd *) 
                  ELSIF WItem . Rep IN LALRTypes . ReprKindSetSentential 
                        AND WItem . ReadTok = LookaheadTok 
                  THEN (* Shift item on Lookahead. *) 
                    INC ( LShiftItemCt )
                  ; IF LShiftItemCt = 1 (* First shift item on Lookahead. *)  
                    THEN 
                      WITH WOper 
                             = Gram . OperArrayRef 
                               ^ [ LookaheadTok - Gram . FirstTerminal ] 
                      DO 
                        IF WOper . Precedence = PrecedenceNull 
                        THEN 
                          LUsePrecAndAssoc := FALSE 
                        ELSE 
                          LMaxShiftPrec := WOper . Precedence 
                        ; LAssocOfMaxPrecShift := WOper . Ass  
                        END (* IF *) 
                      END (* WITH WOper *) 
                    END (* IF *) 
                  END (* IF *) 
               END (* WITH WItem *) 
            END (* FOR Items in WState *) 

        (* Use the gathered information to repair the state. *) 
        ; IF LRedItemCt = 0 
             OR ( LRedItemCt = 1 AND LShiftItemCt = 0 ) 
          THEN (* No conflicts remain. *) 
          ELSIF LUsePrecAndAssoc 
          THEN (* All relevant items have a precedence and associativity. 
                  Use these to repair. *)  
            IF LMaxRedPrec > LMaxShiftPrec 
            THEN 
              LMaxPrec := LMaxRedPrec 
            ; LAssocOfMaxPrec := LAssocOfMaxPrecRed 
            ELSE 
              LMaxPrec := LMaxShiftPrec 
            ; LAssocOfMaxPrec := LAssocOfMaxPrecShift 
            END (* IF *) 
          ; FOR RItemSs := WState . FirstItemSs 
                TO WState . FirstItemSs + WState . ItemCt - 1 
            DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
               DO IF WItem . Rep = ReprKindTyp . RedRep 
                     AND IntSets . IsElement ( LookaheadTok , WItem . Set )
                  THEN (* Reduce item on Lookahead. *)
                    WITH WProd = Gram . ProdArrayRef ^ [ WItem . ProdSs ] 
                    DO IF WProd . Precedence < LMaxPrec 
                       THEN (* Reduce item has lower precedence. Discard it. *)
                         IF Verbose 
                         THEN 
                           InformLowPri ( Gram , RItemSs , LookaheadTok ) 
                         END (* IF *) 
                       ; WItem . Set 
                           := IntSets . Exclude 
                                ( WItem . Set , LookaheadTok )
                       ; IF IntSets . IsEmpty ( WItem . Set ) 
                         THEN 
                           WItem . Rep 
                             := LALRTypes . IgnoredCounterpart ( WItem . Rep ) 
                         END (* IF *) 
                       ELSIF WProd . Precedence = LMaxPrec 
                             AND LAssocOfMaxPrec 
                                 IN LRTable . AssocSetRightOrNonAssoc 
                             AND LMaxShiftPrec = LMaxPrec 
                       THEN (* Reduce item has max precedence, but discard it 
                               for lack of left associativity of Lookahead. *) 
                         IF Verbose 
                         THEN 
                           InformRightAss ( Gram , RItemSs , LookaheadTok ) 
                         END (* IF *) 
                       ; WItem . Set 
                           := IntSets . Exclude 
                                ( WItem . Set , LookaheadTok )
                       ; IF IntSets . IsEmpty ( WItem . Set ) 
                         THEN 
                           WItem . Rep 
                             := LALRTypes . IgnoredCounterpart ( WItem . Rep )
                         END (* IF *) 
                       ELSE 
                         IF Verbose 
                         THEN (* Keep this reduce item. *) 
                           InformKept ( Gram , RItemSs , LookaheadTok ) 
                         END (* IF *) 
                       ; INC ( LRedItemRetainedCt ) 
                       END (* IF *) 
                    END (* WITH WProd *) 
                  ELSIF WItem . Rep IN LALRTypes . ReprKindSetSentential 
                        AND WItem . ReadTok = LookaheadTok 
                  THEN (* A shift item on Lookahead. *) 
                    IF LMaxShiftPrec < LMaxPrec 
                    THEN (* Shift item has lower precedence.  Discard it. *) 
                      IF Verbose 
                      THEN 
                        InformLowPri ( Gram , RItemSs , LookaheadTok ) 
                      END (* IF *) 
                    ; WItem . Rep 
                        := LALRTypes . IgnoredCounterpart ( WItem . Rep ) 
                    ELSIF LMaxShiftPrec = LMaxPrec 
                          AND LAssocOfMaxPrec 
                              IN LRTable . AssocSetLeftOrNonAssoc 
                          AND LMaxRedPrec = LMaxPrec 
                    THEN (* Shift item has max precedence, but discard it for
                            lack of right associativity of Lookahead. *) 
                      IF Verbose 
                      THEN 
                        InformLeftAss ( Gram , RItemSs , LookaheadTok ) 
                      END (* IF *) 
                    ; WItem . Rep 
                        := LALRTypes . IgnoredCounterpart ( WItem . Rep )
                    ELSE (* Keep this shift item. *) 
                      IF Verbose 
                      THEN 
                        InformKept ( Gram , RItemSs , LookaheadTok ) 
                      END (* IF *) 
                    ; INC ( LShiftItemRetainedCt ) 
                    END (* IF *) 
                  END (* IF *) 
               END (* WITH WItem *) 
            END (* FOR Items in WState *) 

          ELSE (* NOT LUsePrecAndAssoc Use "automatic" repair rules. *) 
            IF LShiftItemCt > 0 
            THEN (* shift wird reduce vorgezogen *) 
              FOR RItemSs := WState . FirstItemSs 
                  TO WState . FirstItemSs + WState . ItemCt - 1 
              DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
                 DO IF WItem . Rep = ReprKindTyp . RedRep 
                       AND IntSets . IsElement 
                             ( LookaheadTok , WItem . Set ) 
                    THEN (* Reduce item on Lookahead.  Discard it. *)
                      IF Verbose 
                      THEN 
                        InformIgnored ( Gram , RItemSs , LookaheadTok ) 
                      END (* IF *) 
                    ; WItem . Set 
                        := IntSets . Exclude ( WItem . Set , LookaheadTok ) 
                    ; IF IntSets . IsEmpty ( WItem . Set ) 
                      THEN 
                        WItem . Rep 
                          := LALRTypes . IgnoredCounterpart ( WItem . Rep )
                      END (* IF *) 
                    ELSIF WItem . Rep IN LALRTypes . ReprKindSetSentential 
                          AND WItem . ReadTok = LookaheadTok 
                    THEN (* Shift item on Lookahead.  Keep it. *)
                      IF Verbose 
                      THEN 
                        InformKept ( Gram , RItemSs , LookaheadTok ) 
                      END (* IF *) 
                    ; INC ( LShiftItemRetainedCt ) 
                    END (* IF *) 
                 END (* WITH WItem *) 
              END (* FOR Items in WState *) 
            ELSE (* erstes reduce auswaehlen *) 
              FOR RItemSs := WState . FirstItemSs 
                  TO WState . FirstItemSs + WState . ItemCt - 1 
              DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
                 DO IF WItem . Rep = ReprKindTyp . RedRep 
                       AND IntSets . IsElement 
                             ( LookaheadTok , WItem . Set ) 
                    THEN (* Reduce item on Lookahead. *)
                      WITH WProd = Gram . ProdArrayRef ^ [ WItem . ProdSs ] 
                      DO IF WProd . ProdNo = LMinRedProdNo 
                         THEN (* Earliest-defined reduce item, keep it. *) 
                           IF Verbose 
                           THEN  
                             InformKept ( Gram , RItemSs , LookaheadTok ) 
                           END (* IF *) 
                         ; INC ( LRedItemRetainedCt ) 
                         ELSE (* Later reduce item.  Discard it. *) 
                           IF Verbose 
                           THEN 
                             InformIgnored ( Gram , RItemSs , LookaheadTok ) 
                           END (* IF *) 
                         ; WItem . Set 
                             := IntSets . Exclude 
                                  ( WItem . Set , LookaheadTok ) 
                         ; IF IntSets . IsEmpty ( WItem . Set ) 
                           THEN 
                             WItem . Rep 
                               := LALRTypes . IgnoredCounterpart 
                                    ( WItem . Rep ) 
                           END (* IF *) 
                         END (* IF *) 
                      END (* WITH WProd *) 
                    END (* IF *) 
                 END (* WITH WItem *) 
              END (* FOR Items in WState *) 
            END (* IF *) 
          END (* IF *) 

        (* See if conflicts persist after repairs. *) 
        ; LConflictFree := FALSE 
        ; IF LRedItemRetainedCt > 1 
          THEN 
            IF LShiftItemRetainedCt > 0 
            THEN (* Plural reduce items and at least one shift item. *) 
              IF Verbose 
              THEN 
                InformConflict ( tConflict . ShRedRed ) 
              END (* IF *) 
            ; RisShiftRedRedSet 
                := IntSets . Include ( RisShiftRedRedSet , LookaheadTok ) 
            ELSE (* Plural reduce items only. *) 
              IF Verbose 
              THEN 
                InformConflict ( tConflict . RedRed ) 
              END (* IF *) 
            ; RisRedRedSet 
                := IntSets . Include ( RisRedRedSet , LookaheadTok ) 
            END (* IF *) 
          ELSIF LRedItemRetainedCt = 1 
          THEN 
            IF LShiftItemRetainedCt > 0 
            THEN (* One reduce item and at least one shift item. *) 
              IF Verbose 
              THEN 
                InformConflict ( tConflict . ShRed ) 
              END (* IF *) 
            ; RisShiftRedSet 
                := IntSets . Include ( RisShiftRedSet , LookaheadTok ) 
            ELSE (* One reduce item and no shifts.  No conflict. *) 
              LConflictFree := TRUE 
            END (* IF *) 
          ELSE (* No reduce items.  No conflict. *) 
            LConflictFree := TRUE 
          END (* IF *) 
        ; IF VerboseDebug THEN Debug . NewLine ( ) END (* IF *) 
        ; IF LConflictFree 
          THEN 
            IF LRedItemCt > 1 
            THEN 
              IF LShiftItemCt > 0 
              THEN 
                IF LUsePrecAndAssoc 
                THEN 
                  RepReadRedRedSet 
                    := IntSets . Include ( RepReadRedRedSet , LookaheadTok ) 
                ELSE 
                  RisAutoRepShiftRedRedSet 
                    := IntSets . Include 
                         ( RisAutoRepShiftRedRedSet , LookaheadTok ) 
                END (* IF *) 
              ELSE (* LShiftItemCt = 0 *) 
                IF LUsePrecAndAssoc 
                THEN 
                  RisRepRedRedSet 
                    := IntSets . Include ( RisRepRedRedSet , LookaheadTok )
                ELSE 
                  RisAutoRepRedRedSet 
                    := IntSets . Include ( RisAutoRepRedRedSet , LookaheadTok )
                END (* IF *) 
              END (* IF *) 
            ELSIF LRedItemCt = 1 
            THEN 
              IF LShiftItemCt > 0 
              THEN 
                IF LUsePrecAndAssoc 
                THEN 
                  RisRepShiftRedSet 
                    := IntSets . Include ( RisRepShiftRedSet , LookaheadTok )
                ELSE 
                  RisAutoRepShiftRedSet 
                    := IntSets . Include 
                         ( RisAutoRepShiftRedSet , LookaheadTok ) 
                END (* IF *) 
              ELSE (* LShiftItemCt = 0 *) 
                (* This can happen if the sublist repair was done. *) 
              END (* IF *) 
            ELSE (* LRedItemCt = 0 *) 
              CantHappen ( AFT . A_CheckRepairItemSet_NoConflict2 ) 
            END (* IF *) 
          ELSE 
            IsOK := FALSE 
          END (* IF *) 
        END (* WITH WState *) 
      END RisToken 

  ; BEGIN (* RepairItemSet *) 
      IF DoRepair 
      THEN 
        RisShiftRedSet := IntSets . Empty ( )
      ; RisRedRedSet := IntSets . Empty ( )
      ; RisShiftRedRedSet := IntSets . Empty ( )
      ; RisRepShiftRedSet := IntSets . Empty ( )
      ; RisRepRedRedSet := IntSets . Empty ( )
      ; RepReadRedRedSet := IntSets . Empty ( )
      ; RisAutoRepShiftRedSet := IntSets . Empty ( )
      ; RisAutoRepRedRedSet := IntSets . Empty ( )
      ; RisAutoRepShiftRedRedSet := IntSets . Empty ( )
      ; RisSublistSet := IntSets . Empty ( )
      ; IF VerboseDebug 
        THEN 
          DebugHead ( Gram , StateSs , ConflictingLookaheadSet ) 
(* FIXME: This is looping up deep inside DebugState, at a recursion depth of
          5, recursing much deeper during each loop iteration. 
*) 
        ; DebugState ( Gram , StateSs , ConflictingLookaheadSet ) 
        END (* IF *) 
      ; <* FATAL ANY *> BEGIN 
          IntSets . ForAllDo ( ConflictingLookaheadSet , RisToken )  
        END (* Block *) 
      ; Messages . TextOnly 
          ( MessageCodes . Image ( AFT . I_StateIsNotLR ) 
            & ": " 
            & Fmt . Int ( StateSs ) 
          , MessageCodes . KindTyp . MkInformation 
          ) 
      ; IF InformNormalSublist 
           AND NOT IntSets . IsEmpty ( RisSublistSet ) 
        THEN 
          Messages . SemError0 ( AFT . I_NormalSublistConflict ) 
        ; Infos . WriteTokSet ( Gram , RisSublistSet ) 
        END (* IF *) 
      ; IF NOT IntSets . IsEmpty ( RisShiftRedSet ) 
        THEN 
          Messages . SemError0 ( AFT . E_ShiftReduceConflict ) 
        ; Infos . WriteTokSet ( Gram , RisShiftRedSet ) 
        END (* IF *) 
      ; IF NOT IntSets . IsEmpty ( RisRedRedSet ) 
        THEN 
          Messages . SemError0 ( AFT . E_ReduceReduceConflict ) 
        ; Infos . WriteTokSet ( Gram , RisRedRedSet ) 
        END (* IF *) 
      ; IF NOT IntSets . IsEmpty ( RisShiftRedRedSet ) 
        THEN 
          Messages . SemError0 ( AFT . E_ShiftReduceReduceConflict ) 
        ; Infos . WriteTokSet ( Gram , RisShiftRedRedSet ) 
        END (* IF *) 
      ; IF NOT IntSets . IsEmpty ( RisRepShiftRedSet ) 
        THEN 
          Messages . SemError0 ( AFT . I_RepairedShiftReduceConflict ) 
        ; Infos . WriteTokSet ( Gram , RisRepShiftRedSet ) 
        END (* IF *) 
      ; IF NOT IntSets . IsEmpty ( RisRepRedRedSet ) 
        THEN 
          Messages . SemError0 ( AFT . I_RepairedReduceReduceConflict ) 
        ; Infos . WriteTokSet ( Gram , RisRepRedRedSet ) 
        END (* IF *) 
      ; IF NOT IntSets . IsEmpty ( RepReadRedRedSet ) 
        THEN 
          Messages . SemError0 ( AFT . I_RepairedShiftReduceReduceConflict ) 
        ; Infos . WriteTokSet ( Gram , RepReadRedRedSet ) 
        END (* IF *) 
      ; IF NOT IntSets . IsEmpty ( RisAutoRepShiftRedSet ) 
        THEN 
          Messages . SemError0 ( AFT . W_DefaultRepairedShiftReduceConflict ) 
        ; Infos . WriteTokSet ( Gram , RisAutoRepShiftRedSet ) 
        END (* IF *) 
      ; IF NOT IntSets . IsEmpty ( RisAutoRepRedRedSet ) 
        THEN 
          Messages . SemError0 ( AFT . W_DefaultRepairedReduceReduceConflict ) 
        ; Infos . WriteTokSet ( Gram , RisAutoRepRedRedSet ) 
        END (* IF *) 
      ; IF NOT IntSets . IsEmpty ( RisAutoRepShiftRedRedSet ) 
        THEN 
          Messages . SemError0
            ( AFT . W_DefaultRepairedShiftReduceReduceConflict ) 
        ; Infos . WriteTokSet ( Gram , RisAutoRepShiftRedRedSet ) 
        END (* IF *) 
      ; IF VerboseDebug THEN DebugEnd ( ) END (* IF *) 
      END (* IF *) 
    END RepairItemSet 

(* VISIBLE: *) 
; PROCEDURE CheckForConflicts 
    ( Gram : LRTable . GrammarTyp ; VAR IsOK : BOOLEAN ) 
  RAISES { AssertionFailure } 
  (* Pruefe ob die Zustaende Konflikte beinhalten, 
     so weit moeglich werden Konflikte mit Hilfe von 
     Prioritaeten und Assoziativitaeten geloest, 
     falls keine Korektur moeglich ist wird das Programm mit 
     einer Fehlermeldung beendet, sonst steht ein konfliktfreier 
     Automat zur Auswertung zur Verfuegung *) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LSymbolSet : IntSets . T 
  ; VAR LConflictingLookaheadSet : IntSets . T 
  ; VAR LTempSet : IntSets . T 
  ; VAR LListSet : IntSets . T 
  ; VAR LAttemptedRepairSetCt : CARDINAL 

  ; BEGIN (* CheckForConflicts *) 
      LAttemptedRepairSetCt := 0 
    ; IsOK := TRUE 

    (* fuer Debug wird in Number ein Verweis auf den zugeh. State eingetragen *)
    (* The Number field is used earlier in Lookahead, for an entirely 
       different purpose. *) 
    (* Also copy Item . Set to Item . OrigSet, for info output later. *)
    ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO WITH WState = Gram . StateArrayRef ^ [ RStateSs ] 
         DO FOR RItemSs := WState . FirstItemSs 
                TO WState . FirstItemSs + WState . ItemCt - 1 
            DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
               DO WItem . Number := RStateSs 
               ; WItem . OrigSet := WItem . Set 
               END (* WITH WItem *) 
            END (* FOR Items in WState *) 
         END (* WITH WState *) 
      END (* FOR All states *) 
    ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO WITH WState = Gram . StateArrayRef ^ [ RStateSs ] 
         DO LConflictingLookaheadSet := IntSets . Empty ( ) 
         ; LSymbolSet := IntSets . Empty ( ) 
         ; LListSet := IntSets . Empty ( ) 
         (* First find tokens calling for reduce on a list production. *) 
         ; FOR RItem := WState . FirstItemSs 
               TO WState . FirstItemSs + WState . ItemCt - 1 
           DO WITH WItem = Gram . ItemArrayRef ^ [ RItem ] 
              DO IF WItem . Rep = ReprKindTyp . RedRep 
                    AND Gram . ProdArrayRef ^ [ WItem . ProdSs ] . IsList 
                 THEN 
                   LListSet := IntSets . Union ( LListSet , WItem . Set ) 
                 ELSE 
                 END (* IF *) 
              END (* WITH WItem *) 
           END (* FOR Items in WState *) 

         (* Now look for conflicts. *) 
         ; FOR RItem := WState . FirstItemSs 
               TO WState . FirstItemSs + WState . ItemCt - 1 
           DO WITH WItem = Gram . ItemArrayRef ^ [ RItem ] 
              DO CASE WItem . Rep 
                 OF ReprKindTyp . TermRep 
                 , ReprKindTyp . AstNontermRep 
                 => IF 
FALSE AND              IntSets . IsElement ( WItem . ReadTok , LListSet ) 
                    THEN (* Discard this shift action. *) 
                      WItem . Rep 
                        := LALRTypes . IgnoredCounterpart ( WItem . Rep ) 
                    ELSIF IntSets . IsElement ( WItem . ReadTok , LSymbolSet ) 
                    THEN 
                      LConflictingLookaheadSet 
                        := IntSets . Include 
                             ( LConflictingLookaheadSet , WItem . ReadTok )
                    ELSE 
                      LSymbolSet 
                        := IntSets . Include ( LSymbolSet , WItem . ReadTok )
                    END (* IF *) 
                 | ReprKindTyp . RedRep 
                 => LTempSet 
                     := IntSets . Intersection ( WItem . Set , LSymbolSet ) 
                 ; LConflictingLookaheadSet 
                     := IntSets . Union ( LConflictingLookaheadSet , LTempSet )
                 ; LSymbolSet 
                     := IntSets . Union ( LSymbolSet , WItem . Set ) 
                 ELSE 
                 END (* CASE *) 
              END (* WITH WItem *) 
           END (* FOR Items in WState *) 
         ; IF NOT IntSets . IsEmpty ( LConflictingLookaheadSet ) 
           THEN 
             RepairItemSet 
               ( Gram 
               , RStateSs 
               , LConflictingLookaheadSet 
               , (* IN OUT *) IsOK 
               ) 
           ; INC ( LAttemptedRepairSetCt ) 
           END (* IF *) 
         END (* WITH WState *) 
      END (* FOR All states *) 
    ; Messages . TextOnly 
        ( MessageCodes . Image ( AFT . I_Non_LR_State_Count ) 
          & " = " 
          & Fmt . Int ( LAttemptedRepairSetCt ) 
        , MessageCodes . KindTyp . MkInformation 
        ) 
    END CheckForConflicts 

; BEGIN (* Check *) 
    Verbose := TRUE 
  ; VerboseDebug := TRUE  
  ; InformNormalSublist := FALSE 
  END Check 
. 

