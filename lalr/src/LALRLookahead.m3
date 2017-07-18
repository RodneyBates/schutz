
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Lookahead.mi, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Compute LALR(1) lookahead sets *) 
(* Algorithm from "Efficient Computation of LALR(1) Look-Ahead Sets",
   Framk DeRemer and Thomas Pennello, ACM Transactions on Programming
   Languages and systems, Vol. 4, No. 4, October 1982, Pages 615-639. 
*) 

MODULE LALRLookahead 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , AssertionFailure , CantHappen 
; IMPORT Check 
; IMPORT IntProdVarArray 
; IMPORT IntItemVarArray 
; IMPORT IntSets 
; IMPORT IntStateVarArray 
; IMPORT LALRTypes 
; FROM LALRTypes IMPORT ReprKindTyp 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT LRUtils 
; IMPORT MessageCodes 

; TYPE AFT = MessageCodes . T 

; CONST PFetch = IntProdVarArray . Fetch 
; CONST IFetch = IntItemVarArray . Fetch 
; CONST SFetch = IntStateVarArray . Fetch 

; VAR reportedError : BOOLEAN := FALSE 

; PROCEDURE MarkRep ( Gram : LRTable . GrammarTyp ) 
  (* Markiert Items die eine Uebergang oder eine Reduktion repraesentieren *) 

  = VAR LReduceProdSet : IntSets . T (* Set of production numbers. *) 
  ; VAR LRepArrayRef : LALRTypes . ItemSsArrayRefTyp 
        (* ^Subscripted by terminals and nonterminals, biased by
           Gram . FirstTerminal.  Maps to item subscripts. 
        *) 
  ; VAR LShiftTok : LbeStd . TokTyp 

  ; BEGIN (* MarkRep *) 
      LRepArrayRef 
        := NEW 
             ( LALRTypes . ItemSsArrayRefTyp 
             , Gram . LastNonterminal - Gram . FirstTerminal + 1 
             ) 
    ; FOR RSymbol := 0 TO NUMBER ( LRepArrayRef ^ ) - 1 
      DO LRepArrayRef ^ [ RSymbol ] := LALRTypes . ItemSsNull 
      END (* FOR *) 
    ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO 
        LReduceProdSet := IntSets . Empty ( ) 
      ; WITH WState = Gram . StateArrayRef ^ [ RStateSs ]
        DO FOR RItemSs := WState . FirstItemSs 
               TO WState . FirstItemSs + WState . ItemCt - 1 
           DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ]
              DO IF WItem . DotPosition 
                    >= Gram . ProdArrayRef ^ [ WItem . ProdSs ] . Len 
                 THEN (* Dot at right end. *) 

                   (* Within one state, each reducable production has exactly
                      one item that represents it. *) 
                   IF IntSets . IsElement ( WItem . ProdSs , LReduceProdSet ) 
                   THEN (* We already saw a reduce item on this production in
                           this state. 
                        *) 
                     WItem . Rep := ReprKindTyp . NoRep 
(* CHECK: Is there any reason to set RepItemSs to the other copy of the item? *)
                   ELSE 
                     WItem . RepItemSs := RItemSs (* Self-representative. *) 
                   ; WItem . Rep := ReprKindTyp . RedRep 
                   ; LReduceProdSet 
                       := IntSets . Include ( LReduceProdSet , WItem . ProdSs )
                   END (* IF *) 
                 ELSE (* Shift/GOTO item. *) 

                 (* Within one state, each shiftable symbol has exactly one 
                    item that represents it. *)  
                   LShiftTok := WItem . ReadTok (* Token after dot. *) 
                 ; WITH WOtherRepItemSs 
                        = LRepArrayRef ^ [ LShiftTok - Gram . FirstTerminal ] 
                   DO 
                     IF WOtherRepItemSs # LALRTypes . ItemSsNull  
                     THEN (* We already saw a shift item on this token in
                             this state. 
                          *) 
                       WItem . Rep := ReprKindTyp . NoRep 
                     ; WItem . RepItemSs := WOtherRepItemSs  
                     ELSE 
                       WOtherRepItemSs := RItemSs 
                     ; WItem . RepItemSs := RItemSs (* Self-representative. *) 
                     ; IF LShiftTok < Gram . FirstAstNonterminal 
                       THEN (* A pure terminal. *) 
                         WItem . Rep := ReprKindTyp . TermRep 
                       ELSIF LShiftTok < Gram . FirstNonterminal 
                       THEN (* An AST nonterminal. *) 
                         WItem . Rep := ReprKindTyp . AstNontermRep 
                       ELSE (* Other nonterminal. *) 
                         WItem . Rep := ReprKindTyp . NontermRep 
                       END (* IF *) 
                     END (* IF *) 
                   END (* WITH *) 
                 END (* IF *) 

              (* nur fuer diese Items werden Sets benoetigt *) 
              ; IF WItem . Rep IN LALRTypes . ReprKindSetRedOrNonterm 
                THEN 
                  WItem . Set := IntSets . Empty ( ) 
                  (* ^ A lookahead symbol set. *) 
                END (* IF *) 
              END (* WITH *) 
           END (* FOR *) 

        (* Reinitialize just the used elements of LRepArrayRef ^ *) 
        ;  FOR RItemSs := WState . FirstItemSs 
               TO WState . FirstItemSs + WState . ItemCt - 1 
           DO WITH 
                WItem 
                  = Gram . ItemArrayRef ^ [ RItemSs ]
              DO IF WItem . Rep IN LALRTypes . ReprKindSetShift
                 THEN (* We used this element. *) 
                   LRepArrayRef ^ [ WItem . ReadTok - Gram . FirstTerminal ] 
                     := LALRTypes . ItemSsNull  
                 END (* IF *) 
              END (* WITH *) 
           END (* FOR *) 
        END (* WITH *) 
      END (* FOR *) 
    END MarkRep 

; PROCEDURE LR0ListRepStateSs 
    ( Gram : LRTable . GrammarTyp ; StateSs : LALRTypes . StateSsTyp )
  : LALRTypes . StateSsTyp 
  (* The LR(0) state that represents StateSs. *) 

  = BEGIN 
      WITH WState = Gram . StateArrayRef ^ [ StateSs ] 
      DO 
        RETURN WState . LR0ListRepStateSs 
      END (* WITH *) 
    END LR0ListRepStateSs 

; PROCEDURE ClearRelation ( Gram : LRTable . GrammarTyp ) 

  = BEGIN (* ClearRelation *) 
      FOR Item := 0 TO Gram . NextItemSs - 1 
      DO WITH WItem = Gram . ItemArrayRef ^ [ Item ] 
        DO 
          WItem . Relation . Used := 0 
        ; WItem . Relation . Array := NIL 
        END (* WITH *) 
      END (* FOR *) 
    END ClearRelation 

; CONST InitRelationListCount = 10 
        (* Anfangsplatzgroesse fuer Relationlisten *) 

; PROCEDURE PutInRelation 
    ( Gram : LRTable . GrammarTyp 
    ; Left : LALRTypes . ItemSsTyp 
    ; Right : LALRTypes . ItemSsTyp 
    ) 

  = VAR LRelationCount : LALRTypes . ListCountTyp 
  ; VAR LNewRelationCount : LALRTypes . ListCountTyp 
  ; VAR LNewArrayRef : LALRTypes . ItemSsArrayRefTyp 

(* TODO: Use VarArray for Relation. *) 
  ; BEGIN (* PutInRelation *) 
    (* zu bearbeitende Relation auswaehlen *) 
      WITH WRelation = Gram . ItemArrayRef ^ [ Left ] . Relation 
      DO (* pruefen ob Eintrag bereits vorhanden *) 
         FOR RItemSs := 0 TO WRelation . Used - 1 
         DO IF WRelation . Array ^ [ RItemSs ] = Right 
            THEN 
              RETURN 
            END (* IF *) 
         END (* FOR *) 

      (* eventuell Speicher beschaffen *) 
      ; IF WRelation . Array = NIL 
        THEN (* Allocate inital array. *) 
          WRelation . Array 
            := NEW ( LALRTypes . ItemSsArrayRefTyp , InitRelationListCount ) 
        ; FOR RItemSs := 0 TO NUMBER ( WRelation . Array ^ ) - 1 
          DO (* Paranoia *) 
             WRelation . Array ^ [ RItemSs ] := LALRTypes . ItemSsNull 
          END (* FOR *) 
        ELSE 
          LRelationCount := NUMBER ( WRelation . Array ^ ) 
        ; IF LRelationCount <= WRelation . Used 
          THEN (* Expand array. *) 
            LNewRelationCount := 2 * LRelationCount 
          ; LNewArrayRef 
              := NEW ( LALRTypes . ItemSsArrayRefTyp , LNewRelationCount ) 
          ; SUBARRAY ( LNewArrayRef ^ , 0 , LRelationCount ) 
              := WRelation . Array ^ 
          ; FOR RItemSs := LRelationCount TO NUMBER ( LNewArrayRef ^ ) - 1 
            DO (* Paranoia *) 
               LNewArrayRef ^ [ RItemSs ] := LALRTypes . ItemSsNull 
            END (* FOR *) 
          ; WRelation . Array := LNewArrayRef 
          END (* IF *) 
        END (* IF *) 

      (* actuell Eintrag. *) 
      ; WRelation . Array ^ [ WRelation . Used ] := Right 
      ; INC ( WRelation . Used ) 
      END (* WITH *) 
    END PutInRelation 

; PROCEDURE ComputeDR ( Gram : LRTable . GrammarTyp ) 
  (* Berechnung der "direct read symbols" (DR) *) 
  (* DeRemer & Pennello, p 632: p is an LR(0) state, A a nonterminal.
                                A      t        
     DR (p,A) := { t in T | p  ---> r ---> }    
  *) 

  = BEGIN (* ComputeDR *) 
      (* Betrachte alle Zustande. *) 
      FOR RpStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO WITH WpState = Gram . StateArrayRef ^ [ RpStateSs ]
        DO IF WpState . LR0ListRepStateSs = RpStateSs (* An LR(0) state. *)   
          THEN (* Betrachte alle Nichterminaluebergaenge von WpState *) 
            FOR RpItemSs := WpState . FirstItemSs 
                TO WpState . FirstItemSs + WpState . ItemCt - 1 
            DO (* fuer alle Item - ein Item entspricht (p,A) *) 
              WITH WpItem = Gram . ItemArrayRef ^ [ RpItemSs ]
              DO (* pruefe ob pA einen Nichtterminaluebergang  repraesentiert *) 
                IF WpItem . Rep IN LALRTypes . ReprKindSetAllNonterm 
                THEN (* Bestimme r *) 

                  (* Berechne DR (p,A) als Menge aller in r lesbaren Terminale *)
                  WITH 
                    WrState 
                      = Gram . StateArrayRef 
                        ^ [ LR0ListRepStateSs ( Gram , WpItem . GoToStateSs ) ]
                  DO (* Zustand r *) 
                    FOR RItemSs := WrState . FirstItemSs 
                        TO WrState . FirstItemSs + WrState . ItemCt - 1 
                    DO WITH WrItem = Gram . ItemArrayRef ^ [ RItemSs ]
                       DO (* ein Item von r *) 
                          IF WrItem . Rep IN LALRTypes . ReprKindSetSentential 
                          THEN 
                            WITH 
                              WSet = Gram . ItemArrayRef ^ [ RpItemSs ] . Set
                            DO 
                              WSet 
                                := IntSets . Include 
                                     ( WSet , WrItem . ReadTok )
                            END (* WITH *) 
                          END (* IF *) 
                       END (* WITH *) (* ein Item von r *) 
                    END (* FOR *) (* Zustand r *) 
                  END (* WITH *) 
                END (* IF *) 
              END (* WITH *) 
            END (* FOR *) 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    END ComputeDR 

(* TODO: Combine ComputeDR and ComputeReadsRelation. *) 

; PROCEDURE ComputeReadsRelation 
    ( Gram : LRTable . GrammarTyp 
    ; Nullables : IntSets . T (* Nonterminals *) 
    ) 
  (*                              A      C                       
     (p,A) reads (r,C)  falls  p ---> r ---> and  C =>* epsilon 
  *) 

  = BEGIN (* ComputeReadsRelation *) 
      (* Betrachte alle Zustande. *) 
      FOR RpStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO WITH WpState = Gram . StateArrayRef ^ [ RpStateSs ]
        DO IF WpState . LR0ListRepStateSs = RpStateSs (* An LR(0) state. *)   
          THEN (* Betrachte alle Nichterminaluebergaenge von WpState *) 
            FOR RpItemSs := WpState . FirstItemSs 
                TO WpState . FirstItemSs + WpState . ItemCt - 1 
            DO (* fuer alle Item - ein Item entspricht (p,A) *) 
              WITH WpItem = Gram . ItemArrayRef ^ [ RpItemSs ]
              DO IF WpItem . Rep IN LALRTypes . ReprKindSetAllNonterm 
                THEN (* WpItem einen Nichterminaluebergang repraesentiert *)
                (* Berechne zugh. rC's *) 
                  WITH 
                    WrState 
                      = Gram . StateArrayRef 
                        ^ [ LR0ListRepStateSs ( Gram , WpItem . GoToStateSs ) ]
                  DO FOR RItemSs := WrState . FirstItemSs 
                         TO WrState . FirstItemSs + WrState . ItemCt - 1 
                    DO WITH WrItem = Gram . ItemArrayRef ^ [ RItemSs ]
                       DO IF WrItem . Rep IN LALRTypes . ReprKindSetAllNonterm 
                         THEN (* Pruefe ob C => Epsilon *) 
                           IF IntSets . IsElement 
                                ( WrItem . ReadTok , Nullables )
                           THEN (* gueltiges rC gefunden *) 
                               (* Fuege rC zur Relation hinzu *) 
                             PutInRelation ( Gram , RpItemSs , RItemSs ) 
                           END (* IF *) 
                         END (* IF *) 
                       END (* WITH *) 
                    END (* FOR *) 
                  END (* WITH *) 
                END (* IF *) 
              END (* WITH *) 
            END (* FOR *) 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    END ComputeReadsRelation

; VAR GSaveIntermediateInfo : BOOLEAN := TRUE 
(* TODO: ^Make this a command line option. *)  

; PROCEDURE AssignReadSetsAndRelations ( Gram : LRTable . GrammarTyp ) 
  (* For all items, 1) Copy Set to ReadSet.
                    2) Copy Relation to ReadsRelation.
     All, for future reference only.  Relation will be reinitialized in the
     future, and Set will be altered in the future.  
     We really only need to do this for LR(0) states, but it's just so much
     easier to leave this coded for all items period. 
  *) 

  = BEGIN (* AssignReadSets *) 
      IF GSaveIntermediateInfo 
      THEN 
        FOR Item := 0 TO Gram . NextItemSs - 1 
        DO WITH WItem = Gram . ItemArrayRef ^ [ Item ]
           DO IF WItem . Rep IN LALRTypes . ReprKindSetAllNonterm 
              THEN 
                WItem . ReadSet := WItem . Set  
              ; WItem . ReadsRelation := WItem . Relation 
              END (* IF *) 
           END (* WITH *) 
        END (* FOR *) 
      END (* IF *) 
    END AssignReadSetsAndRelations 

; PROCEDURE TreatReadConflict ( <* UNUSED *> Empty : BOOLEAN ) 

  = BEGIN (* TreatReadConflict *) 
      IF NOT reportedError 
      THEN 
      (* Do not report this fact 
        ErrorMessage (eNotLRk,eInformation,0,0); 
      *) 
(* CHECK: Presumably because precedence and associativity is tried later 
          to resolve conflicts? *) 
        reportedError := TRUE 
      END (* IF *) 
    END TreatReadConflict 

; PROCEDURE TreatFollowConflict ( Empty : BOOLEAN ) 

  = BEGIN (* TreatFollowConflict *) 
      IF NOT Empty 
      THEN 
        IF NOT reportedError 
        THEN 
        (* do not report this fact 
        ErrorMessage (eNotLRk,eInformation,0,0); 
        *) 
(* CHECK: Presumably because precedence and associativity is tried later 
          to resolve conflicts? *) 
          reportedError := TRUE 
        END (* IF *) 
      END (* IF *) 
    END TreatFollowConflict 

; CONST 
    InitItemStackCount : LALRTypes . ListCountTyp = 100 (* Why be stingy? *)

; VAR Stack := LALRTypes . ItemSsListTyp { Used := 0 , Array := NIL } 
(* TODO: ^Get this out of a global variable. *) 

; PROCEDURE ClearItemStack ( ) 

  = BEGIN (* ClearItemStack *) 
      Stack . Used := 0 
    END ClearItemStack 

; PROCEDURE PushItem ( ItemSs : LALRTypes . ItemSsTyp ) 

  = VAR LStackCount : LALRTypes . ListCountTyp 
  ; VAR LNewStackCount : LALRTypes . ListCountTyp 
  ; VAR LNewArrayRef : LALRTypes . ItemSsArrayRefTyp 

  ; BEGIN (* PushItem *) 
      IF Stack . Array = NIL 
      THEN (* Allocate initial array. *) 
        Stack . Array 
          := NEW ( LALRTypes . ItemSsArrayRefTyp , InitItemStackCount ) 
      ELSE 
        LStackCount := NUMBER ( Stack . Array ^ ) 
      ; IF LStackCount <= Stack . Used 
        THEN (* Extend array. *) 
          Assert 
            ( LAST ( LALRTypes . ListCountTyp ) DIV 2 >= LStackCount 
            , AFT . A_Lookahead_PushItem_Stack_overflow
            ) 
        ; LNewStackCount := 2 * LStackCount 
        ; LNewArrayRef 
            := NEW ( LALRTypes . ItemSsArrayRefTyp , LNewStackCount ) 
        ; SUBARRAY ( LNewArrayRef ^ , 0 , LStackCount ) := Stack . Array ^ 
        ; Stack . Array := LNewArrayRef 
        END (* IF *) 
      END (* IF *) 
    ; Stack . Array ^ [ Stack . Used ] := ItemSs 
    ; INC ( Stack . Used ) 
    END PushItem 

; PROCEDURE PopItem ( ) : LALRTypes . ItemSsTyp 
  RAISES { AssertionFailure } 

  = BEGIN (* PopItem *) 
      Assert ( Stack . Used >= 1 , AFT . A_LookaheadPopItem_EmptyStack ) 
    ; DEC ( Stack . Used ) 
    ; RETURN Stack . Array ^ [ Stack . Used ] 
    END PopItem 

; PROCEDURE TopItem ( ) : LALRTypes . ItemSsTyp 
  RAISES { AssertionFailure } 

  = BEGIN (* TopItem *) 
      Assert ( Stack . Used >= 1 , AFT . A_LookaheadTopItem_EmptyStack ) 
    ; RETURN Stack . Array ^ [ Stack . Used - 1 ] 
    END TopItem 

; PROCEDURE ItemStackDepth ( ) : INTEGER 

  = BEGIN (* ItemStackDepth *) 
      RETURN Stack . Used 
    END ItemStackDepth 

; PROCEDURE ClearNumbers ( Gram : LRTable . GrammarTyp ) 
  (* We really only need to do this for LR(0) states, but it's just so much
     easier to leave this coded for all items period. 
  *) 

  = BEGIN (* ClearNumbers *) 
      FOR RItemSs := 0 TO Gram . NextItemSs - 1 
      DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ]
         DO IF WItem . Rep IN LALRTypes . ReprKindSetAllNonterm 
            THEN 
              WItem . Number := 0 
            ELSE 
              WItem . Number := LALRTypes . ItemMarkInfinity  
            END (* IF *) 
         END (* WITH *) 
      END (* FOR *) 
    END ClearNumbers 

; PROCEDURE Traverse 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : LALRTypes . ItemSsTyp 
    ; TreatConflict : ConflictProc 
    ) 
  RAISES { AssertionFailure } 

  = VAR LItemStackDepth : LALRTypes . ItemMarkTyp 
  ; VAR LTransItemSs : LALRTypes . ItemSsTyp 
  ; VAR LTop : LALRTypes . ItemSsTyp 
  ; VAR LEmptyCycle , LCyclic : BOOLEAN 

  ; BEGIN (* Traverse *) 
      WITH WItem = Gram . ItemArrayRef ^ [ ItemSs ]
      DO 
      (* Push x on S *) 
         PushItem ( ItemSs ) 

      (* con d: Depth of S *) 
      ; LItemStackDepth := ItemStackDepth ( ) 

      (* N x <- d *) 
      ; WItem . Number := LItemStackDepth 
      (* F x <- F' x is already done. *) 

      (* for all y in X such that x R y DO *) 
      ; WITH WRelation = WItem . Relation 
        DO FOR ArrayIndex := 0 TO WRelation . Used - 1 
           DO LTransItemSs := WRelation . Array ^ [ ArrayIndex ] 

          (* If Ny = 0 then call Traverse y *) 
           ; IF Gram . ItemArrayRef ^ [ LTransItemSs ] . Number = 0 
             THEN 
               Traverse ( Gram , LTransItemSs , TreatConflict ) 
             END (* IF *) 

           (* Assign N x := Min (N x, N y) *) 
           ; IF Gram . ItemArrayRef ^ [ LTransItemSs ] . Number 
                < WItem . Number 
             THEN 
               WItem . Number 
                 := Gram . ItemArrayRef ^ [ LTransItemSs ] . Number 
             END (* IF *) 
          (* F x := F x union F y *) 
           ; WItem . Set 
               := IntSets . Union 
                    ( WItem . Set 
                    , Gram . ItemArrayRef ^ [ LTransItemSs ] . Set 
                    ) 
           END (* FOR *) 
        END (* WITH *) 

      (* IF N x = d *) 
      ; IF WItem . Number = LItemStackDepth 
        THEN 
          LCyclic := FALSE 
        ; LEmptyCycle := TRUE 

        (* Then repeat *) 
        ; REPEAT 
            (* Assign N (Top OF S) <- Infinity) ; F(Top OF S) <- F x *) 
            LTop := TopItem ( ) 
          ; Gram . ItemArrayRef ^ [ LTop ] . Number 
              := LALRTypes . ItemMarkInfinity 
          ; IF LTop # ItemSs 
            THEN 
              Gram . ItemArrayRef ^ [ LTop ] . Set := WItem . Set  
            ; LCyclic := TRUE 
            ; LEmptyCycle 
                := LEmptyCycle 
                   AND IntSets . IsEmpty 
                         ( Gram . ItemArrayRef ^ [ LTop ] . ReadSet ) 
                       (* For the first group of calls on Traverse, 
                          initiated by the first call on Digraph, 
                          all ReadSets are Empty.  LEmptyCycle is 
                          not used. 
                       *) 
            END (* IF *) 
          (* until (Pop OF S) = x *) 
          UNTIL PopItem ( ) = ItemSs 
(* TODO: Recode this without the side-effect function PopItem. *) 
        ; IF LCyclic 
          THEN 
            LEmptyCycle 
              := LEmptyCycle AND IntSets . IsEmpty ( WItem . ReadSet )
          ; TreatConflict ( LEmptyCycle ) 
          END (* IF *) 
        END (* IF *) 
      END (* WITH *) 
    END Traverse 

; PROCEDURE Digraph 
    ( Gram : LRTable . GrammarTyp ; TreatConflict : ConflictProc ) 
  RAISES { AssertionFailure } 

  = BEGIN (* Digraph *) 
    (* Let S be an initially empty stack OF elements of X *) 
      ClearItemStack ( ) 

    (* "Let N be an ARRAY OF zeros indexed by elements of X" sic. 
       Actually, an array of integers, initialized to zeros, and ... *) 
    ; ClearNumbers ( Gram ) 

    (* For x in X such that N x = 0,, but only for LR(0) states. *) 
    ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO WITH WState = Gram . StateArrayRef ^ [ RStateSs ]
        DO IF WState . LR0ListRepStateSs = RStateSs (* An LR(0) state. *)   
          THEN 
            FOR RItemSs := WState . FirstItemSs 
                TO WState . FirstItemSs + WState . ItemCt - 1 
            DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ]
              DO IF WItem . Number = 0 
                 THEN Traverse ( Gram , RItemSs , TreatConflict ) 
                 END (* IF *) 
              END (* WITH *) 
            END (* FOR *) 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    END Digraph 

; PROCEDURE IAndLRecurse 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : LALRTypes . StateSsTyp (* State moving to. *) 
    ; ProdSs : LRTable . ProdNoTyp     (* Prod whose RHS we traverse. *) 
    ; DotPosition : LbeStd . TokNoTyp  (* And dot position therein. *) 
    ; RightItemSs : LALRTypes . ItemSsTyp    (* Item moving from. *) 
    ; Nullables : IntSets . T (* Nonterminals *) 
    ; VAR DerivesEmpty : BOOLEAN 
    ) 

  = VAR LItemSs : LALRTypes . ItemSsTyp 
  ; VAR LRepItemSs : LALRTypes . ItemSsTyp 

  ; BEGIN (* IAndLRecurse *) 
      WITH WState = Gram . StateArrayRef ^ [ StateSs ]
      DO (* finde zugehoeriges Item *) 
        LItemSs := WState . FirstItemSs 
      ; LOOP 
          WITH WItem = Gram . ItemArrayRef ^ [ LItemSs ]
          DO IF WItem . ProdSs = ProdSs AND WItem . DotPosition = DotPosition 
             THEN EXIT 
             ELSIF LItemSs >= WState . FirstItemSs + WState . ItemCt - 1
             THEN 
               CantHappen ( AFT . A_IAndLRecurse_Item_not_found ) 
             ELSE 
               INC ( LItemSs ) 
             END (* IF *) 
          END (* WITH *) 
        END (* LOOP *) 
      ; WITH WItem = Gram . ItemArrayRef ^ [ LItemSs ]
        DO (* eindeutigen Repraesentanten beschaffen *) 
           (* jedoch mit speziellen (WITH-Statement) weiterarbeiten *) 
           LRepItemSs := WItem . RepItemSs 

        (* zugehoerige Production beschaffen *) 
        ; IF WItem . DotPosition 
             < Gram . ProdArrayRef ^ [ WItem . ProdSs ] . Len 
          THEN (* Ende noch nicht ereicht *) 
            (* Follow another dot shift. *) 
            IAndLRecurse 
              ( Gram 
              , LR0ListRepStateSs ( Gram , WItem . GoToStateSs ) 
              , WItem . ProdSs 
              , WItem . DotPosition + 1 
              , RightItemSs 
              , Nullables 
              , (*VAR*) DerivesEmpty 
              ) 
          ; IF DerivesEmpty 
            THEN 
              IF Gram . ItemArrayRef ^ [ LRepItemSs ] . Rep 
                 IN LALRTypes . ReprKindSetAllNonterm 
              THEN (* Include zu sich selbst ausfiltern, *) 
                   (* da nicht konstruktiv bei Followberechnung *) 
                IF LRepItemSs # RightItemSs 
                THEN  (* In Include eintragen *) 
                  PutInRelation ( Gram , LRepItemSs , RightItemSs ) 
                END (* IF *) 
              END (* IF *) 
            ; DerivesEmpty 
                := WItem . ReadTok >= Gram . FirstAstNonterminal 
(* CHECK:          ^Is this condition redundant? An optimization? *) 
                   AND IntSets . IsElement ( WItem . ReadTok , Nullables ) 
            END (* IF *) 
          ELSE (* Ende der Produktion wurde erreicht *) 
            DerivesEmpty := TRUE 
          (* In Lookback eintragen. *) 
          ; PutInRelation ( Gram , LRepItemSs , RightItemSs ) 
          END (* IF *) 
        END (* WITH *) 
      END (* WITH *) 
    END IAndLRecurse 

; PROCEDURE ComputeIncludesAndLookback 
    ( Gram : LRTable . GrammarTyp 
    ; Nullables : IntSets . T (* Nonterminals *) 
    ) 

  = VAR LRepItemSs : LALRTypes . ItemSsTyp 
  ; VAR LDerivesEmpty : BOOLEAN 

  ; BEGIN (* ComputeIncludesAndLookback *) 
    (* loesche bisherige Relation fuer neue Includesrelation *) 
      ClearRelation ( Gram ) 

    (* Betrachte alle States *) 
    ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO WITH WState = Gram . StateArrayRef ^ [ RStateSs ]
        DO IF WState . LR0ListRepStateSs = RStateSs (* An LR(0) state. *)   
          THEN (* Betrachte alle Nichterminaluebergaenge *) 
            FOR RRightItemSs := WState . FirstItemSs 
                TO WState . FirstItemSs + WState . ItemCt - 1 
            DO WITH WRightItem = Gram . ItemArrayRef ^ [ RRightItemSs ]
(* CHECK: Originally, a copy of Gram . ItemArrayRef ^ [ RRightItemSs ]
          was copied into a local here.  The only fields ever referenced 
          are Rep and ReadTok, which are not changing at this point.  So I 
          made it a WITH-binding.  Is this reasoning right? *) 
               DO IF WRightItem . Rep IN LALRTypes . ReprKindSetAllNonterm 
                  THEN (* Finde Situationen deren Produktion mit dem *) 
                     (* zur RRightItemSs gehoerigen Nichterminal beginnen *) 
                    FOR RProdItemSs := WState . FirstItemSs 
                        TO WState . FirstItemSs + WState . ItemCt - 1 
                    DO WITH WProdItem = Gram . ItemArrayRef ^ [ RProdItemSs ]
                      DO IF TRUE OR WProdItem . Rep = ReprKindTyp . RedRep 
(* FIXME: ^This is wrong.  We will need a set of productions that have been
          processed, to avoid duplication. *) 
                        THEN 
                          WITH WProd 
                               = Gram . ProdArrayRef ^ [ WProdItem . ProdSs ]
                          DO IF WProdItem . DotPosition = 0 
                                AND WProd . Left = WRightItem . ReadTok 
                             THEN (* Closure item on first item's ReadTok *)
                               IF 0 < WProd . Len 
                               THEN 
                                 IAndLRecurse 
                                   ( Gram 
                                   , LR0ListRepStateSs 
                                       ( Gram , WProdItem . GoToStateSs ) 
                                   , WProdItem . ProdSs 
                                   , WProdItem . DotPosition + 1 
                                   , RRightItemSs 
                                   , Nullables 
                                   , (*VAR*) LDerivesEmpty 
                                   ) 
                               ; IF LDerivesEmpty 
                                 THEN (* Repraesentant beschaffen *) 
                                   LRepItemSs := WProdItem . RepItemSs 
                                 (* pruefen ob Nichtterminaluebergang *) 
                                 ; IF Gram . ItemArrayRef ^ [ LRepItemSs ]. Rep 
                                      IN LALRTypes . ReprKindSetAllNonterm 
                                   THEN 
                                     IF LRepItemSs # RRightItemSs 
                                     THEN (* Include zu sich selbst ausfiltern,
                                             da nicht konstruktiv bei 
                                             Followberechnung *) 
                                       (* In Include eintragen *) 
                                       PutInRelation 
                                         ( Gram , LRepItemSs , RRightItemSs ) 
                                     END (* IF *) 
                                   END (* IF *) 
                                 END (* IF *) 
                               ELSE (* Leer Prod. In Lookback eintragen. *) 
                                 PutInRelation 
                                   ( Gram , RProdItemSs , RRightItemSs ) 
                               END (* IF *) 
                             END (* IF Closure item on first item's ReadTok *) 
                          END (* WITH *) 
                        END (* IF *) 
                       END (* WITH *) 
                    END (* FOR *) 
                  END (* IF *) 
               END (* WITH *) 
            END (* FOR *) 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    END ComputeIncludesAndLookback 

; PROCEDURE ComputeLA ( Gram : LRTable . GrammarTyp ) 

  = VAR LLookbackItemSs : LALRTypes . ItemSsTyp 

  ; BEGIN (* ComputeLA *) 
      FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO WITH WState = Gram . StateArrayRef ^ [ RStateSs ]
        DO IF WState . LR0ListRepStateSs = RStateSs (* An LR(0) state. *)   
          THEN 
            FOR RItemSs := WState . FirstItemSs 
                TO WState . FirstItemSs + WState . ItemCt - 1 
            DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ]
              DO 
                IF WItem . Rep = ReprKindTyp . RedRep 
                  (* WItem eine Reduktion darstellt *) 
                THEN (* Berechne Look Ahead Set *) 
                     (* fuer alle Item in Lookback *) 
                  WITH WRelation = WItem . Relation 
                  DO FOR Index := 0 TO WRelation . Used - 1 
                    DO LLookbackItemSs := WRelation . Array ^ [ Index ] 
; IF RItemSs = 7229
     AND IntSets . IsElement 
           ( 94, Gram . ItemArrayRef ^ [ LLookbackItemSs ] . Set )
  THEN 
    Assertions . DoNothing ( ) 
  END 
                    (* fuege Follow(lookback) hinzu *) 
                    ; WItem . Set 
                        := IntSets . Union 
                             ( WItem . Set 
                             , Gram . ItemArrayRef ^ [ LLookbackItemSs ] . Set 
                             ) 
                    END (* FOR *) 
                  END (* WITH *) 
                END (* IF *) 
              END (* WITH *)  
            END (* FOR *) 
          END (* IF *) 
        END (* WITH *) 
      END (* FOR *) 
    END ComputeLA 

(* VISIBLE: *) 
; PROCEDURE ComputeLALR ( Gram : LRTable . GrammarTyp ) 
  RAISES { AssertionFailure } 
  (* Berechnung der LALR(1) LookAheadSets *) 

  = VAR LNullables : IntSets . T (* Nonterminals *) := NIL 
(* TODO: Save LNullables in Gram. *) 

  (* Algorithm step labels from page 633 of referenced paper: *) 

  ; BEGIN (* ComputeLALR *) 
      LNullables := LRUtils . NullableNonterms ( Gram ) (* A *) 
    ; MarkRep ( Gram ) 
    ; ComputeDR ( Gram ) (* In Item.Set. *)             (* B *) 
    ; ComputeReadsRelation ( Gram , LNullables )        (* C *) 
    (* Here, Item.Relation is Reads. *) 
    ; Digraph ( Gram , TreatReadConflict )              (* D *) 
    (* Here, Item.Set is Read. *) 
    ; AssignReadSetsAndRelations ( Gram ) 
    ; ComputeIncludesAndLookback ( Gram , LNullables )  (* E *) 
    (* Here, Item.Relation is Includes and Lookback. *) 
    ; Digraph ( Gram , TreatFollowConflict )            (* F *) 
    (* Here, Item.Set is Followl *) 
    ; ComputeLA ( Gram )                                (* G *) 
    END ComputeLALR 

; BEGIN (* LALRLookahead *) 
  END LALRLookahead 
. 

