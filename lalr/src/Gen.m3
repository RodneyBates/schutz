
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates 2001 .. 2007, from Cocktail, lalr, Gen.mi, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Generate parsing tables *) 

MODULE Gen 

; IMPORT Fmt 
; IMPORT Stdio
; IMPORT Thread 
; IMPORT Time 
; IMPORT Wr 

; FROM Failures IMPORT Backout 
; IMPORT Compress 
; IMPORT Default 
; IMPORT IntSets 
; IMPORT LALRTypes 
; FROM LALRTypes IMPORT StateKindTyp , ReprKindTyp 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT PortTypes 

; IMPORT Assertions 

; FROM Assertions IMPORT Assert 

; IMPORT MessageCodes 

; TYPE AFT = MessageCodes . T 

; PROCEDURE MarkReachableByAstNT ( Gram : LRTable . GrammarTyp ) 

  = BEGIN (* MarkReachableByAstNT *) 

    (* Betrachte alle Zustaende *) 
      FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO WITH WState = Gram . StateArrayRef ^ [ RStateSs ] 
         DO 
           FOR RItemSs := WState . FirstItemSs 
               TO WState . FirstItemSs + WState . ItemCt - 1 
           DO (* Thru items *) 
             WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
             DO (* Nur Ast Nichterminaluebergaenge *) 
               IF Gram . FirstAstNonterminal <= WItem . ReadTok 
                  AND WItem . ReadTok <= Gram . LastAstNonterminal 
               THEN 
                 Gram . StateArrayRef ^ [ WItem . GoToStateSs ] 
                 . IsTargetOfAstNTTransition 
                 := TRUE  
               END (* IF *) 
             END (* WITH *) 
           END (* LOOP *) 
         END (* WITH *) 
      END (* FOR *) 
    END MarkReachableByAstNT 

; PROCEDURE ClassifyStates ( Gram : LRTable . GrammarTyp ) 
  RAISES { Backout } 
  (* Zustaende klassifizieren und mit einer neuen Nummer versehen *) 

  = VAR LItemSs : LALRTypes . ItemSsTyp 
  ; VAR LRepCount : PortTypes . Int32Typ 
  ; VAR LReadStateCt : LRTable . StateNoTyp := 1 
        (* StateNo zero is used internally in Default as a Null. *) 
  ; VAR LAcceptProdNo : LRTable . ProdNoTyp := LRTable . ProdNoNull  

  ; BEGIN (* ClassifyStates *) 
      Gram . FirstReadAction := LReadStateCt 
      (* Make a pass over states, classifying them, numbering the 
         Lookup states, and locating the accept state. *) 
    ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO (* Betrachte alle Zustaende *) 
         WITH WState = Gram . StateArrayRef ^ [ RStateSs ] 
         DO IF WState . ItemCt = 1 
            THEN 
              LRepCount := 1 
            ; LItemSs := WState . FirstItemSs 
            ELSE 
              LRepCount := 0 
            ; FOR RItemSs := WState . FirstItemSs 
                  TO WState . FirstItemSs + WState . ItemCt - 1 
              DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
                 DO CASE WItem . Rep 
                   OF ReprKindTyp . TermRep 
                   , ReprKindTyp . NontermRep 
                   , ReprKindTyp . AstNontermRep 
                   , ReprKindTyp . RedRep 
                   => INC ( LRepCount ) 
                   ; LItemSs := RItemSs 
                   ELSE 
                   END (* CASE *) 
                 END (* WITH *) 
              END (* FOR *) 
            END (* IF *) 
         ; IF LRepCount = 1 
           THEN (* Zustand enthaelt nur ein Item *) 
             WITH WItem = Gram . ItemArrayRef ^ [ LItemSs ] 
             DO IF WItem . Rep = ReprKindTyp . RedRep 
                THEN (* es handelt sich um eine Reduktion *) 
                     (* beschaffe zugh. Produktion *) 
                  WITH WProd = Gram . ProdArrayRef ^ [ WItem . ProdSs ] 
                  DO IF WState . IsTargetOfAstNTTransition 
                        OR NOT LALRTypes . UseReadReduceStates 
                     THEN (* Reduce states need to be numbered in the same 
                             range with read states. *) 
                       WState . Kind := StateKindTyp . Lookup 
                     ; WState . Action := LReadStateCt 
                     ; WState . ReduceItemSs := LALRTypes . ItemSsNull 
                     ; INC ( LReadStateCt ) 
                     ; IF WProd . Left = LbeStd . Tok__Augment 
                       THEN 
                         Assert 
                           ( LItemSs = WState . FirstItemSs 
                           , AFT . A_GenClassifyStates_StopItemSstFirstItem 
                           ) 
                       ; WState . Kind := StateKindTyp . Accept 
                       ; Gram . AcceptAction := WState . Action 
                       ; Gram . AcceptProdNo := WProd . ProdNo 
                       END (* IF *) 
                     ELSE                      
                       WState . ReduceItemSs := LItemSs 
                     ; IF WProd . Left = LbeStd . Tok__Augment 
                       THEN 
                         Assert 
                           ( LItemSs = WState . FirstItemSs 
                           , AFT . A_GenClassifyStates_StopItemSstFirstItem 
                           ) 
                       ; WState . Kind := StateKindTyp . Accept 
                       ; LAcceptProdNo 
                           := Gram . ProdArrayRef ^ 
                                [ Gram . ItemArrayRef ^ 
                                    [ WState . ReduceItemSs ] 
                                  . ProdSs 
                                ] 
                              . ProdNo 
                       ELSE 
                         WState . Kind := StateKindTyp . Reduce 
                       END (* IF *) 
                     ; WState . Action := LAST ( LRTable . StateNoTyp ) 
                     END (* IF *) 
                  END (* WITH *) 
                ELSE (* es handelt sich um einen Read-Zustand *) 
                  WState . Kind := StateKindTyp . Lookup 
                ; WState . Action := LReadStateCt 
                ; WState . ReduceItemSs := LALRTypes . ItemSsNull 
                ; INC ( LReadStateCt ) 
                END (* IF *) 
             END (* WITH *) 
           ELSE (* Zustand mit mehreren Items, die Repraesanten sind, 
                   muss ein Read-Zustand sein *) 
             WState . Kind := StateKindTyp . Lookup 
           ; WState . Action := LReadStateCt 
           ; WState . ReduceItemSs := LALRTypes . ItemSsNull 
           ; INC ( LReadStateCt ) 
           END (* IF *) 
         END (* WITH *) 
      END (* FOR *) 

    (* Nummern fuer ReduceStates *) 
    ; Gram . FirstReadRedAction := LReadStateCt 
    ; Gram . FirstReduceAction := LReadStateCt 

    (* If there are shift-reduce states, make a second pass over the 
       states, renumbering them by classes. *) 
    (* Trage neue Nummern fuer ReadStates, ReadTermStates, und 
       ReadNontermStates ein *) 
    ; IF LALRTypes . UseReadReduceStates 
      THEN 
        INC ( Gram . FirstReduceAction , Gram . ProdCt ) 
      ; Gram . AcceptAction := LAcceptProdNo + Gram . FirstReadRedAction 
      ; Gram . AcceptProdNo := LAcceptProdNo 
(* 
      ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
        DO WITH WState = Gram . StateArrayRef ^ [ RStateSs ] 
           DO CASE WState . Kind 
              OF StateKindTyp . Reduce , StateKindTyp . Accept 
              => WState . Action 
                   := Gram . FirstReadRedAction 
                      + Gram . ProdArrayRef ^ 
                          [ Gram . ItemArrayRef ^ [ WState . ReduceItemSs ] 
                            . ProdSs 
                          ] 
                        . ProdNo 
              ELSE 
              END (* CASE *) 
           END (* WITH *) 
        END (* FOR *) 
*) 
      END (* IF *) 
    ; Gram . ActionCt := Gram . FirstReduceAction + Gram . ProdCt 
    END ClassifyStates 

; PROCEDURE MakeReduceInfo ( Gram : LRTable . GrammarTyp ) 

  = BEGIN (* MakeReduceInfo *) 
      Gram . ReduceInfoRef 
        := NEW ( LRTable . ReduceInfoArrayRefTyp , Gram . NextProdSs ) 
    ; FOR RProdSs := 0 TO Gram . NextProdSs - 1 
      DO WITH WProd = Gram . ProdArrayRef ^ [ RProdSs ] 
         DO Gram . ReduceInfoRef ^ [ WProd . ProdNo ] 
              := LRTable . ReduceInfoTyp 
                   { LhsTok := WProd . Left 
                   , BuildTok := WProd . BuildTok 
                   , RhsLen := WProd . Len 
                   , OptionIdSet := WProd . OptionIdSet 
                   } 
         END (* WITH *) 
      END (* FOR *) 
    END MakeReduceInfo 

; PROCEDURE MakeContinuation ( Gram : LRTable . GrammarTyp ) 

  = VAR LItemSs : LALRTypes . ItemSsTyp 

  ; BEGIN (* MakeContinuation *) 
      Gram . ContinuationRef 
        := NEW ( LRTable . ContinuationRefTyp , Gram . FirstReadRedAction ) 
    ; Gram . ContinuationRef ^ [ 0 ] := 0 

    (* Betrachte alle Zustaende *) 
    ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO WITH WState = Gram . StateArrayRef ^ [ RStateSs ] 
         DO CASE WState . Kind <* NOWARN *> 
            OF StateKindTyp . Lookup 
            => (* Lookup Zustaende *) 
              LItemSs := WState . FirstItemSs 
            ; LOOP (* Thru items *) 
                IF LItemSs >= WState . FirstItemSs + WState . ItemCt 
                THEN (* Something is wrong with the grammar. *)  
                  Gram . ContinuationRef ^ [ WState . Action ] 
                    := LbeStd . Tok__EndOfImage 
                ; EXIT
                ELSE
                  WITH WItem = Gram . ItemArrayRef ^ [ LItemSs ] 
                  DO (* Nur Terminaluebergaenge und Reduktionen *) 
                     IF Gram . ItemArrayRef ^ [ WItem . RepItemSs ] . Rep 
                        IN LALRTypes . ReprKindSetRedOrSentential 
                     THEN 
                       WITH WBestItem 
                            = Gram . ItemArrayRef ^ [ WItem . RepItemSs ] 
                       DO IF WBestItem . Rep = ReprKindTyp . RedRep 
                          THEN (* Waehle ein beliebiges Element aus *) 
                            Gram . ContinuationRef ^ [ WState . Action ] 
                              := IntSets . ArbitraryMember 
                                   ( WBestItem . Set ) 
                          ELSE 
                            Gram . ContinuationRef ^ [ WState . Action ] 
                              := WBestItem . ReadTok 
                          END (* IF *) 
                       END (* WITH *) 
                     ; EXIT 
                     ELSE 
                       INC ( LItemSs ) 
                     END (* IF *) 
                   END (* WITH *) 
                END (* IF *) 
              END (* LOOP *) 
            | StateKindTyp . Reduce 
            => WITH WItem = Gram . ItemArrayRef ^ [ WState . ReduceItemSs ] 
               DO (* Waehle ein beliebiges Element aus *) 
                  Gram . ContinuationRef ^ [ WState . Action ] 
                    := IntSets . ArbitraryMember ( WItem . Set ) 
               END (* WITH *) 
            | StateKindTyp . Accept 
            => Gram . ContinuationRef ^ [ WState . Action ] 
                 := LbeStd . Tok__EndOfImage 
            END (* CASE *) 
         END (* WITH *) 
      END (* FOR *) 
    END MakeContinuation 

(* VISIBLE: *) 
; PROCEDURE InitTableLine 
    ( Gram : LRTable . GrammarTyp ; VAR Line : LRTable . StateNoArrayRefTyp ) 

  = BEGIN (* InitTableLine *) 
      IF Line = NIL 
      THEN 
        Line 
          := NEW 
               ( LRTable . StateNoArrayRefTyp 
               , Gram . LastNonterminal - Gram . FirstTerminal + 1 
               ) 
      END (* IF *) 
    ; FOR RSymbol := 0 TO NUMBER ( Line ^ ) - 1 
      DO Line ^ [ RSymbol ] := LRTable . StateNoNull 
      END (* FOR *) 
    END InitTableLine 

(* VISIBLE: *) 
; PROCEDURE MakeTableLine 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : LALRTypes . StateSsTyp 
    ; VAR Result : LRTable . StateNoArrayRefTyp 
    ) 
    : LRTable . StateNoTyp 
  (* Construct a literal (uncompressed) row of the parsing table, from
     the items in the state.  
     Formal StateSs is the state subscript.  The function result is the 
     state code. *) 

  = VAR LReduceAction : LRTable . StateNoTyp 

  ; BEGIN (* MakeTableLine *) 
      WITH WState = Gram . StateArrayRef ^ [ StateSs ] 
      DO IF WState . Kind = StateKindTyp . Lookup 
            OR ( NOT LALRTypes . UseReadReduceStates 
                 AND WState . Kind = StateKindTyp . Reduce 
               ) 
         THEN (* nur ReadStates *) 
           (* alle Items *) 
           FOR RItemSs := WState . FirstItemSs 
               TO WState . FirstItemSs + WState . ItemCt - 1 
           DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
              DO CASE WItem . Rep 
                 OF ReprKindTyp . TermRep 
                 , ReprKindTyp . NontermRep 
                 => WITH WTargetState 
                         = Gram . StateArrayRef ^ [ WItem . GoToStateSs ]
                   DO IF WTargetState . ReduceItemSs = LALRTypes . ItemSsNull
                     THEN 
                       Result ^ [ WItem . ReadTok - Gram . FirstTerminal ] 
                         := WTargetState . Action 
                     ELSE 
                       Result ^ [ WItem . ReadTok - Gram . FirstTerminal ] 
                         := Gram . ProdArrayRef ^ 
                              [ Gram . ItemArrayRef ^ 
                                  [ WTargetState . ReduceItemSs ] 
                                . ProdSs 
                              ]   
                            . ProdNo 
                            + Gram . FirstReadRedAction 
                     END (* IF *) 
                   END (* WITH *) 
                 | ReprKindTyp . AstNontermRep 
                 => Result ^ [ WItem . ReadTok - Gram . FirstTerminal ] 
                      := Gram . StateArrayRef ^ [ WItem . GoToStateSs ] 
                         . Action 
                 | ReprKindTyp . RedRep 
                 => LReduceAction 
                      := Gram . FirstReduceAction 
                         + Gram . ProdArrayRef ^ [ WItem . ProdSs ] . ProdNo 
                 ; PROCEDURE Visit ( Tok : IntSets . ValidElemT ) 
                   = BEGIN 
                       Result ^ [ Tok - Gram . FirstTerminal ] 
                         := LReduceAction 
                     END Visit 
                 ; <* FATAL ANY *> BEGIN (* Block *) 
                     IntSets . ForAllDo ( WItem . Set , Visit ) 
                   END (* Block *) 
                 ELSE 
                 END (* CASE *) 
              END (* WITH *) 
           END (* FOR *) 
         END (* IF *) 
      ; RETURN ( WState . Action ) 
      END (* WITH *) 
    END MakeTableLine 

; PROCEDURE MakeTable ( Gram : LRTable . GrammarTyp ) 

  = VAR LStateNo : LRTable . StateNoTyp 
  ; VAR LTStateNo , LNStateNo : LRTable . StateNoTyp 
  ; VAR LDefaultState : LRTable . StateNoTyp 
  ; VAR LDefaultT : Default . T 
  ; VAR LTableLine : LRTable . StateNoArrayRefTyp 

  ; BEGIN (* MakeTable *) 
    (* Erstellen der Listen fuer Defaultberechnung *) 
      LDefaultT := Default . New ( Gram ) 
    ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO InitTableLine ( Gram , LTableLine ) 
      ; LStateNo 
          := MakeTableLine ( Gram , RStateSs , (* VAR *) LTableLine ) 
      ; IF LStateNo < Gram . FirstReadRedAction 
        THEN 
          Default . PutInDefaultList ( LDefaultT , LStateNo , LTableLine ) 
        END (* IF *) 
      END (* FOR *) 

    (* Berechnung der Defaults *) 
    ; Default . ComputeDefaults ( LDefaultT ) 

    (* Comprimieren der Tabelle *) 
    (* - - - alternative a NOT USED 
    ; Compress . InitCompressTable ( Gram ) 
    ; LStateNo 
        := Default . GetNextStateNo ( LDefaultT , LRTable . StateNoNull ) 
    ; WHILE LStateNo # LRTable . StateNoNull 
      DO Default . ConstructDefaultedRow 
           ( LDefaultT 
           , LStateNo 
           , (* OUT, but preallocated *) LTableLine 
           , (* VAR *) LDefaultState 
           ) 
      ; Compress . CompressTableLine 
          ( Gram , LStateNo , LDefaultState , LTableLine ) 
      ; LStateNo := Default . GetNextStateNo ( LDefaultT , LStateNo ) 
      END (* WHILE *) 
    ; Compress . InitCompressNTable ( Gram ) 
    ; LStateNo 
        := Default . GetNextStateNo ( LDefaultT , LRTable . StateNoNull ) 
    ; WHILE LStateNo # LRTable . StateNoNull 
      DO Default . ConstructDefaultedRow 
           ( LDefaultT 
           , LStateNo 
           , (* OUT, but preallocated *) LTableLine 
           , (* VAR *) LDefaultState 
           ) 
      ; Compress . CompressNTableLine ( Gram , LStateNo , LTableLine ) 
      ; LStateNo := Default . GetNextStateNo ( LDefaultT , LStateNo ) 
      END (* WHILE *) 
    - - - End of alternative a. *) 
(* CHECK:  It looks like alternative b was intended to work in conjuction
           with Default.SortTState and Default.SortNStates, but these are
           uncalled.  It also looks like alternative a (commented out, above
           was intended to work without the sort calls.
*) 

    (* - - - alternative b *) 
    ; Compress . InitCompressTable ( Gram ) 
    ; Compress . InitCompressNTable ( Gram ) 
    ; FOR RStateNo := 0 TO Gram . FirstReadRedAction - 1 
      DO 
        LTStateNo := Default . GetTSortState ( LDefaultT , RStateNo ) 
      ; Default . ConstructDefaultedRow 
          ( LDefaultT 
          , LTStateNo 
          , (* OUT, but preallocated *) LTableLine 
          , (* VAR *) LDefaultState 
          ) 
      ; Compress . CompressTableLine 
          ( Gram , LTStateNo , LDefaultState , LTableLine ) 
      ; LNStateNo := Default . GetNSortState ( LDefaultT , RStateNo ) 
      ; IF LNStateNo # LTStateNo 
        THEN 
          Default . ConstructDefaultedRow 
            ( LDefaultT 
            , LNStateNo 
            , (* OUT, but preallocated *) LTableLine , (* VAR *) LDefaultState 
            ) 
        END (* IF *) 
      ; Compress . CompressNTableLine ( Gram , LNStateNo , LTableLine ) 
      END (* FOR *) 
    (* - - - End of alternative b. *) 
    ; Gram . StartStateNo 
        := Gram . StateArrayRef ^ [ LALRTypes . FirstRealStateSs ] . Action 
    ; Gram . DeletionCostRef 
        := NEW 
             ( LRTable . CostArrayRefTyp 
             , Gram . LastAstNonterminal - Gram . FirstTerminal + 1 
             ) 
    ; FOR RI := 0 TO NUMBER ( Gram . DeletionCostRef ^ ) - 1 
      DO Gram . DeletionCostRef ^ [ RI ] := 1 
      END (* FOR *) 
    ; Gram . InsertionCostRef 
        := NEW 
             ( LRTable . CostArrayRefTyp 
             , Gram . LastAstNonterminal - Gram . FirstTerminal + 1 
             ) 
    ; FOR RI := 0 TO NUMBER ( Gram . InsertionCostRef ^ ) - 1 
      DO Gram . InsertionCostRef ^ [ RI ] := 1 
      END (* FOR *) 
    ; Gram . AscendingInsertionCostRef 
        := NEW 
             ( LRTable . TokArrayRefTyp 
             , Gram . LastAstNonterminal - LbeStd . Tok__FirstLangDep + 1 
             ) 
    ; FOR RI := 0 TO NUMBER ( Gram . AscendingInsertionCostRef ^ ) - 1 
      DO Gram . AscendingInsertionCostRef ^ [ RI ] 
           := RI + LbeStd . Tok__FirstLangDep 
      END (* FOR *) 
    END MakeTable 

(* +++ *) 

; VAR TraceWr : Wr . T := Stdio . stderr 

(* VISIBLE: *) 
; PROCEDURE GenTables ( Gram : LRTable . GrammarTyp ) 
  RAISES { Backout } 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 

    BEGIN (* GenTables *) 
      MarkReachableByAstNT ( Gram ) 
    ; ClassifyStates ( Gram ) 
    ; MakeReduceInfo ( Gram ) 
    ; MakeContinuation ( Gram ) 
    ; IF Trace 
      THEN 
        Wr . PutText ( TraceWr , " Starting table construction, Time :" ) 
      ; Wr . PutText 
          ( TraceWr , Fmt . Pad ( Fmt . LongReal ( Time . Now ( ) ) , 5 ) ) 
      ; Wr . PutText ( TraceWr , Wr . EOL ) 
      END (* IF *) 
    ; MakeTable ( Gram ) 
    ; IF Trace 
      THEN 
        Wr . PutText ( TraceWr , " Finished table construction, Time :" ) 
      ; Wr . PutText 
          ( TraceWr , Fmt . Pad ( Fmt . LongReal ( Time . Now ( ) ) , 5 ) ) 
      ; Wr . PutText ( TraceWr , Wr . EOL ) 
      END (* IF *) 
    END GenTables 

; BEGIN (* Gen *) 
  END Gen 
. 

