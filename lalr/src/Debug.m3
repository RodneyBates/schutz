
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Debug.mi, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Display information for debugging a grammar. *) 
(* TODO: This seems to make little sense, either when
         looking at its output or its code.  Rework it.
*) 

MODULE Debug 

; IMPORT Fmt 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr 


; IMPORT Assertions 
; FROM Assertions IMPORT Assert , AssertionFailure 
; IMPORT Infos 
; IMPORT IntSets 
; IMPORT LALRTypes 
; FROM LALRTypes IMPORT ReprKindTyp , StateSsTyp , ItemSsTyp 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT LRUtils 
; IMPORT MessageCodes 
; IMPORT PortTypes 

; TYPE AFT = MessageCodes . T 

; CONST HorizArrowChar = '_' 
; CONST VertArrowChar = '|' 

(* Part A: derivation from start to site of the problem. 
   Part B: Getting the problem lookahead? 
   Part C: Getting the LHS of the reduce 
   Part D: What it conflicts with. 
*) 
(* CHECK: ^Is this description right? *) 

; CONST InitTab = 0 
; CONST MaxTabA = 40 
; CONST MaxTabB = 30 
; CONST MaxTabC = 50 
; CONST MaxTabD = 40 
; CONST InitChainLength = 50 

; TYPE tProdPathElmt 
    = RECORD 
        ProdSs : LRTable . ProdNoTyp 
      ; DotPosition : LbeStd . TokNoTyp 
      END (* RECORD *) 

; TYPE ProdPathArrayRefTyp = REF ARRAY OF tProdPathElmt 

; TYPE tProdPath 
    = RECORD 
        Used : LALRTypes . ListCountTyp 
      ; Array : ProdPathArrayRefTyp 
(* TODO: ^A candidate for VarArray. *) 
      END (* RECORD *) 

; TYPE tItemChainElmt 
    = RECORD 
        ItemSs : ItemSsTyp 
      ; Last : LALRTypes . ListCountTyp 
        (* ^Identifies the item this is a shift of a closure of. *) 
      END (* RECORD *) 

; TYPE ItemChainArrayRefTyp = REF ARRAY OF tItemChainElmt 

; CONST ChainNull = - 1 (* FIRST ( LALRTypes . ListCountTyp ) *) 
(* CHECK: ^Why not use this. *) 

; TYPE tItemChain 
    = RECORD 
        ReachedItemSet : IntSets .  T (* of LALRTypes . ItemSsTyp *) 
      ; level : INTEGER (* LALRTypes . ListCountTyp Union { - 1 } *) 
      ; Used : LALRTypes . ListCountTyp 
      ; chain : ItemChainArrayRefTyp 
(* TODO: ^A candidate for VarArray. *) 
      END (* RECORD *) 

; VAR ProdPathA : tProdPath 
; VAR ItemPathB : LALRTypes . ItemSsListTyp 
; VAR ItemPathC : LALRTypes . ItemSsListTyp 
; VAR ChainD : tItemChain 
; VAR ProdPathD : tProdPath 

; PROCEDURE WriteVoc 
    ( Gram : LRTable . GrammarTyp 
    ; Tok : LbeStd . TokTyp 
    ; VAR length : INTEGER 
      (* ^Set to the length of the string written. *)  
(* CHECK: ^ There seems to be some inconsistency in the type passed to this. *)
    ) 

  = VAR LImage : TEXT 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* WriteVoc *) 
      LImage := Gram . tokImage ( Tok ) 
    ; Wr . PutText ( DebugWr , LImage ) 
    ; length := Text . Length ( LImage ) 
    END WriteVoc 

; PROCEDURE VocLength 
    ( Gram : LRTable . GrammarTyp ; Tok : LbeStd . TokTyp ) 
    : LbeStd . CharNoTyp 

  = BEGIN (* VocLength *) 
      RETURN Text . Length ( Gram . tokImage ( Tok ) ) 
    END VocLength 

(* VISIBLE: *) 
; PROCEDURE NewLine ( ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* NewLine *) 
      Wr . PutText ( DebugWr , Wr . EOL ) 
    END NewLine 

; PROCEDURE Flush ( ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* NewLine *) 
      Wr . Flush ( DebugWr ) 
    END Flush  

; PROCEDURE WriteTab ( d : LbeStd . CharNoTyp ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* WriteTab *) 
      FOR i := 1 TO d DO Wr . PutChar ( DebugWr , ' ' ) END (* FOR *) 
    END WriteTab 

; PROCEDURE WriteProd 
    ( Gram : LRTable . GrammarTyp 
    ; ProdSs : LRTable . ProdNoTyp 
    ; DotPos : LbeStd . TokNoTyp 
    ; VAR (* IN OUT *) IndentPos : INTEGER 
      (* ^Incremented by length of text preceeding the dot. *) 
    ) 

  = VAR LLength : INTEGER 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* WriteProd *) 
      WITH WProd = Gram . ProdArrayRef ^ [ ProdSs ] 
      DO IF WProd . Len = 0 
         THEN 
           Wr . PutText ( DebugWr , "<empty>" ) 
         ELSE 
           FOR RI := 0 TO WProd . Len - 1 
           DO WriteVoc ( Gram , WProd . Right [ RI ] , (* VAR *) LLength ) 
           ; Wr . PutChar ( DebugWr , ' ' ) 
           ; IF RI < DotPos THEN INC ( IndentPos , LLength + 1 ) END (* IF *) 
           END (* FOR *) 
         END (* IF *) 
      END (* WITH *) 
    END WriteProd 

; CONST ItemNoPad = 7 

; PROCEDURE WriteItem 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LLength : INTEGER 

  ; BEGIN (* WriteItem *) 
      WITH 
        WItem = Gram . ItemArrayRef ^ [ ItemSs ] 
      , WProd = Gram . ProdArrayRef ^ [ WItem . ProdSs ] 
      DO 
        Wr . PutText ( DebugWr , "  Item No " ) 
      ; Wr . PutText 
          ( DebugWr , Fmt . Pad ( Fmt . Int ( ItemSs ) , ItemNoPad ) ) 
      ; Wr . PutChar ( DebugWr , ' ' ) 
      ; WriteVoc ( Gram , WProd . Left , (* VAR *) LLength ) 
      ; Wr . PutText ( DebugWr , " ::= " ) 
      ; IF WProd . Len = 0 
        THEN 
          Wr . PutText ( DebugWr , Infos . LRDotString ) 
        ELSE 
          IF WItem . DotPosition = 0 
          THEN 
            Wr . PutText ( DebugWr , Infos . LRDotString ) 
          END (* IF *) 
        ; FOR i := 0 TO WProd . Len - 1 
          DO WriteVoc ( Gram , WProd . Right [ i ] , (* VAR *) LLength ) 
          ; IF WItem . DotPosition = i + 1 
            THEN 
              Wr . PutText ( DebugWr , Infos . LRDotString ) 
            ELSE 
              Wr . PutChar ( DebugWr , ' ' ) 
            END (* IF *) 
          END (* FOR *) 
        END (* IF *) 
      ; IF WItem . DotPosition = WProd . Len 
        THEN 
          Wr . PutText ( DebugWr , " {" ) 
        ; WriteVoc ( Gram , LookaheadTok , (* VAR *) LLength ) 
        ; Wr . PutChar ( DebugWr , '}' ) 
        END (* IF *) 
      ; NewLine ( ) 
      ; Flush ( ) 
      END (* WITH *) 
    END WriteItem 

(* VISIBLE: *) 
; PROCEDURE InformIgnored 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* InformIgnored *) 
      Wr . PutText ( DebugWr , "ignored                 " ) 
    ; WriteItem ( Gram , ItemSs , LookaheadTok ) 
    END InformIgnored 

(* VISIBLE: *) 
; PROCEDURE InformLowPri 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* InformLowPri *) 
      Wr . PutText ( DebugWr , "ignored (precedence)    " ) 
    ; WriteItem ( Gram , ItemSs , LookaheadTok ) 
    END InformLowPri 

(* VISIBLE: *) 
; PROCEDURE InformSublist 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* InformLowPri *) 
      Wr . PutText ( DebugWr , "ignored (sublist shift)    " ) 
    ; WriteItem ( Gram , ItemSs , LookaheadTok ) 
    END InformSublist 

(* VISIBLE: *) 
; PROCEDURE InformRightAss 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* InformRightAss *) 
      Wr . PutText ( DebugWr , "ignored (associativity) " ) 
    ; WriteItem ( Gram , ItemSs , LookaheadTok ) 
    END InformRightAss 

(* VISIBLE: *) 
; PROCEDURE InformLeftAss 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* InformLeftAss *) 
      Wr . PutText ( DebugWr , "ignored (associativity) " ) 
    ; WriteItem ( Gram , ItemSs , LookaheadTok ) 
    END InformLeftAss 

(* VISIBLE: *) 
; PROCEDURE InformKept 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* InformKept *) 
      Wr . PutText ( DebugWr , "retained                " ) 
    ; WriteItem ( Gram , ItemSs , LookaheadTok ) 
    END InformKept 

(* VISIBLE: *) 
; PROCEDURE InformConflict ( kind : tConflict ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* InformConflict *) 
      CASE kind 
      OF tConflict . ShRed 
      => Wr . PutText ( DebugWr , "there is a shift-reduce conflict" ) 
      | tConflict . RedRed 
      => Wr . PutText ( DebugWr , "there is a reduce-reduce conflict" ) 
      | tConflict . ShRedRed 
      => Wr . PutText ( DebugWr , "there is a shift-reduce/reduce conflict" ) 
      ELSE 
      END (* CASE *) 
    ; NewLine ( ) 
    ; Flush ( ) 
    END InformConflict 

(* VISIBLE: *) 
; PROCEDURE DebugHead 
    ( Gram : LRTable . GrammarTyp 
    ; State : LRTable . StateNoTyp 
    ; ConflictSet : IntSets . T 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* DebugHead *) 
      IF NoTrace THEN RETURN END (* IF *) 
    ; Wr . PutText 
       ( DebugWr , "--------------------------------------------------------" )
    ; Wr . PutText 
       ( DebugWr , "--------------------------------------------------------" )
    ; NewLine ( ) 
    ; Wr . PutText ( DebugWr , "Parsing conflict in state " ) 
    ; Wr . PutText ( DebugWr , Fmt . Int ( State ) ) 
    ; Infos . WriteTokSet 
        ( Gram 
        , ConflictSet
        , DebugWr
        , " For tokens "
        ) 
    ; NewLine ( ) 
    ; Flush ( ) 
    END DebugHead 

(* VISIBLE: *) 
; PROCEDURE DebugEnd ( ) 

  = BEGIN (* DebugEnd *) 
      IF NoTrace THEN RETURN END (* IF *) 
    ; NewLine ( ) 
    ; Flush ( ) 
    END DebugEnd 

; PROCEDURE Possible 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : ItemSsTyp 
    ; Tok : LbeStd . TokTyp 
    ) 
    : BOOLEAN 
  (* Builds ItemPathB *) 

  = TYPE TriLogicTyp = { yes , no , maybe } 

  ; VAR PossReachedItemSet : IntSets .  T (* of LALRTypes . ItemSsTyp *) 

  ; PROCEDURE GetRep ( ItemSs : ItemSsTyp ) : ReprKindTyp 
    (* Bestimme die zu ItemSs gehoerige Repraesentantenart unabhaengig 
       vom Eintrag, es muss TermRep, NontermRep oder RedRep 
       zurueckgeliefert werden, NoRep ist nicht zulaessig *) 

    = VAR LTok : LbeStd . TokTyp 

    ; BEGIN (* GetRep *) 
        WITH 
          WItem = Gram . ItemArrayRef ^ [ ItemSs ] 
        , WProd = Gram . ProdArrayRef ^ [ WItem . ProdSs ] 
(* CHECK: Why can't we just return WItem . Rep here? *) 
        DO IF WItem . DotPosition = WProd . Len 
           THEN 
             RETURN ReprKindTyp . RedRep 
           ELSE 
             LTok := WProd . Right [ WItem . DotPosition ] 
           ; IF LTok < Gram . FirstAstNonterminal 
             THEN 
               RETURN ReprKindTyp . TermRep 
             ELSIF LTok < Gram . FirstNonterminal 
             THEN 
               RETURN ReprKindTyp . AstNontermRep 
             ELSE 
               RETURN ReprKindTyp . NontermRep 
             END (* IF *) 
           END (* IF *) 
        END (* WITH *) 
      END GetRep 

  ; PROCEDURE Poss 
      ( StateSs : StateSsTyp 
      ; ProdSs : LRTable . ProdNoTyp 
      ; DotPosition : LbeStd . TokNoTyp 
      ; Depth : INTEGER 
      ) 
      : TriLogicTyp 

    = VAR LResult : TriLogicTyp 
    ; VAR LNonterm : LbeStd . TokTyp 
    ; VAR LItemSs : ItemSsTyp 

    ; BEGIN (* Poss *) 
        (* finde zugh. item *) 
        WITH WState = Gram . StateArrayRef ^ [ StateSs ] 
        DO LItemSs := WState . FirstItemSs 
        ; LOOP 
            WITH WItem = Gram . ItemArrayRef ^ [ LItemSs ] 
            DO IF WItem . ProdSs = ProdSs 
                  AND WItem . DotPosition = DotPosition 
               THEN 
                 EXIT 
               END (* IF *) 
            ; INC ( LItemSs ) 
            END (* WITH *) 
          END (* LOOP *) 
        END (* WITH *) 
      ; IF IntSets . IsElement ( LItemSs , PossReachedItemSet ) 
        THEN 
          RETURN TriLogicTyp . no 
        END (* IF *) 
      ; PossReachedItemSet 
          := IntSets . Include ( PossReachedItemSet , LItemSs ) 
      ; WITH WItem = Gram . ItemArrayRef ^ [ LItemSs ] 
        DO CASE GetRep ( LItemSs ) <* NOWARN *> 
           OF ReprKindTyp . TermRep , ReprKindTyp . AstNontermRep 
           => IF Tok = WItem . ReadTok 
              THEN 
                ItemPathB . Used := Depth 
              ; ItemPathB . Array 
                  := NEW ( LALRTypes . ItemSsArrayRefTyp , Depth ) 
              ; ItemPathB . Array ^ [ Depth - 1 ] := LItemSs 
              ; PossReachedItemSet 
                  := IntSets . Exclude ( PossReachedItemSet , LItemSs ) 
              ; RETURN TriLogicTyp . yes 
              ELSE 
                PossReachedItemSet 
                  := IntSets . Exclude ( PossReachedItemSet , LItemSs ) 
              ; RETURN TriLogicTyp . no 
              END (* IF *) 
           | ReprKindTyp . RedRep 
           => PossReachedItemSet 
                := IntSets . Exclude ( PossReachedItemSet , LItemSs ) 
           ; RETURN TriLogicTyp . maybe 
           | ReprKindTyp . NontermRep 
           => LResult := TriLogicTyp . no 
           ; LNonterm := WItem . ReadTok 
           ; WITH WState = Gram . StateArrayRef ^ [ StateSs ] 
             DO FOR FItem := WState . FirstItemSs 
                    TO WState . FirstItemSs + WState . ItemCt - 1 
                DO WITH 
                     WItem2 = Gram . ItemArrayRef ^ [ FItem ] 
                   , WProd2 = Gram . ProdArrayRef ^ [ WItem2 . ProdSs ] 
                   DO IF WProd2 . Left = LNonterm 
                      THEN 
                        CASE 
                          Poss 
                            ( StateSs 
                            , WItem2 . ProdSs 
                            , WItem2 . DotPosition 
                            , Depth + 1 
                            ) 
                        OF TriLogicTyp . yes 
                        => ItemPathB . Array ^ [ Depth - 1 ] := LItemSs 
                        ; PossReachedItemSet 
                            := IntSets . Exclude 
                                 ( PossReachedItemSet , LItemSs ) 
                        ; RETURN TriLogicTyp . yes 
                        | TriLogicTyp . no 
                        => (* Go around loop. *) 
                        | TriLogicTyp . maybe 
                        => CASE 
                             Poss 
                               ( WItem . GoToStateSs 
                               , ProdSs 
                               , DotPosition + 1 
                               , Depth 
                               ) 
                           OF TriLogicTyp . yes 
                           => PossReachedItemSet 
                                := IntSets . Exclude 
                                     ( PossReachedItemSet , LItemSs ) 
                           ; RETURN TriLogicTyp . yes 
                           | TriLogicTyp . no 
                           => 
                           | TriLogicTyp . maybe 
                           => LResult := TriLogicTyp . maybe 
                           END (* CASE *) 
                        END (* CASE *) 
                      END (* IF *) 
                   END (* WITH *) 
                END (* FOR *) 
             END (* WITH *) 
           ; PossReachedItemSet 
               := IntSets . Exclude ( PossReachedItemSet , LItemSs ) 
           ; RETURN LResult 
           END (* CASE *) 
        END (* WITH *) 
      END Poss 

  ; BEGIN (* Possible *) 
      VAR LResult : BOOLEAN 

    ; BEGIN (* Block *) 
        WITH WItem = Gram . ItemArrayRef ^ [ ItemSs ] 
        DO PossReachedItemSet := IntSets . Empty ( ) 
        ; LResult 
            := Poss 
                 ( WItem . Number 
                 , WItem . ProdSs 
                 , WItem . DotPosition 
                 , Depth := 1 
                 ) 
               = TriLogicTyp . yes 
        ; RETURN LResult 
        END (* WITH *) 
      END (* Block *) 
    END Possible 

; PROCEDURE SearchProdPathA 
    ( Gram : LRTable . GrammarTyp 
    ; From : LbeStd . TokTyp 
    ; To : LbeStd . TokTyp 
    ; maxdepth : INTEGER 
    ; depth : INTEGER 
    ; VAR found : BOOLEAN 
    ; VAR (* IN OUT *) ReachableNontermSet : IntSets .  T 
    ) 

  = VAR LProdSs : LRTable . ProdNoTyp 

  ; BEGIN (* SearchProdPathA *) 
      IF From = To 
      THEN 
        ProdPathA . Used := depth 
      ; ProdPathA . Array := NEW ( ProdPathArrayRefTyp , depth ) 
      ; found := TRUE 
      ELSIF depth < maxdepth 
      THEN (* Betrachte alle zu From gehoerige Produktionen *) 
        WITH 
          WNontermInfo 
          = Gram . NontermInfoArrayRef ^ [ From - Gram . FirstAstNonterminal ] 
        DO FOR RI := 0 TO WNontermInfo . Used - 1 
           DO (* Betrachte eine einzelne Produktion *) 
              LProdSs := WNontermInfo . ProdListRef ^ [ RI ] 
           ; WITH WProduction = Gram . ProdArrayRef ^ [ LProdSs ] 
             DO FOR RDotPosition := 0 TO WProduction . Len - 1 
                DO IF WProduction . Right [ RDotPosition ] 
                      >= Gram . FirstAstNonterminal 
                      AND WProduction . Right [ RDotPosition ] 
                          <= Gram . LastNonterminal 
                   THEN 
                     (* Nichtterminale auf der rechten Seite weiterverfolgen *)
                     IF NOT IntSets . IsElement 
                              ( WProduction . Right [ RDotPosition ] 
                              , ReachableNontermSet 
                              ) 
                     THEN 
                       ReachableNontermSet 
                         := IntSets . Include 
                              ( ReachableNontermSet 
                              , WProduction . Right [ RDotPosition ] 
                              ) 
                     ; SearchProdPathA 
                         ( Gram 
                         , WProduction . Right [ RDotPosition ] 
                         , To 
                         , maxdepth 
                         , depth + 1 
                         , (* VAR *) found 
                         , (* IN OUT *) ReachableNontermSet 
                         ) 
                     ; ReachableNontermSet 
                         := IntSets . Exclude 
                              ( ReachableNontermSet 
                              , WProduction . Right [ RDotPosition ] 
                              ) 
                     END (* IF *) 
                   ; IF found 
                     THEN (* Pfad festhalten *) 
                       ProdPathA . Array ^ [ depth ] . ProdSs := LProdSs 

                    (* Position vor dem Nichtterminal angeben *) 
                     ; ProdPathA . Array ^ [ depth ] . DotPosition 
                         := RDotPosition 
                     ; RETURN 
                     END (* IF *) 
                   END (* IF *) 
                END (* FOR *) 
             END (* WITH *) 
           END (* FOR *) 
        END (* WITH *) 
      END (* IF *) 
    END SearchProdPathA 

; EXCEPTION Looped ( CARDINAL ) 

; PROCEDURE FindProdPathA 
    ( Gram : LRTable . GrammarTyp ; N : LbeStd . TokTyp ) 
  RAISES { Looped } 

  = VAR LMaxDepth : INTEGER 
  ; VAR LFound : BOOLEAN 
  ; VAR LReachedNontermSet : IntSets .  T     (* reached Nonterminals *) 

  ; BEGIN (* FindProdPathA *) 
      LMaxDepth := 0 
    ; LFound := FALSE 
    ; LReachedNontermSet := IntSets . Empty ( ) 
    ; REPEAT 
        INC ( LMaxDepth )
      ; IF LMaxDepth > Gram . LastNonterminal 
        THEN RAISE Looped ( LMaxDepth ) 
        END (* IF *) 
      ; SearchProdPathA 
          ( Gram 
          , Gram . StartSymbol 
          , N 
          , LMaxDepth 
          , 0 
          , (* VAR *) LFound 
          , (* IN OUT *) LReachedNontermSet 
          ) 
      UNTIL LFound 
(* Loops deeper here on Semi6.ldl1.  N is 29. 
   From WritePartA, from VisitConflictTok(28),
   from DebutRedItem(..StateSs=11, ItemSs=34,
        ConflictSet={28,33},
        ExplainedItemSet=16_000000000199b938, or 33,35}
   from DebugState(..ConflictingLookaheadTokSet = {28,33}
   from RepairItemSet
        
*) 
    END FindProdPathA 

; PROCEDURE WritePartA 
    ( Gram : LRTable . GrammarTyp 
    ; VAR Indent (* IN OUT *) : INTEGER 
    ; N : LbeStd . TokTyp 
    ) 
  (* Drucke den Trace vom Startsymbol zum Nichtterminal N *) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* WritePartA *)
      TRY  
        FindProdPathA ( Gram , N ) 
      EXCEPT Looped ( Count ) 
      => Wr . PutText ( DebugWr , "WritePartA forcibly terminated after " ) 
      ; Wr . PutText ( DebugWr , Fmt . Int ( Count )  ) 
      ; Wr . PutText ( DebugWr , " iterations." ) 
      ; NewLine ( ) 
      ; RETURN 
      END (* EXCEPT *) 
    ; FOR RI := 0 TO ProdPathA . Used - 1 
      DO WriteTab ( Indent ) 
      ; WriteProd 
          ( Gram 
          , ProdPathA . Array ^ [ RI ] . ProdSs 
          , ProdPathA . Array ^ [ RI ] . DotPosition 
          , (* IN OUT *) Indent 
          ) 
      ; NewLine ( ) 
      ; IF Indent > MaxTabA 
           OR ( RI = ProdPathA . Used - 1 AND Indent > InitTab ) 
        THEN 
          WriteTab ( InitTab ) 
        ; Wr . PutChar ( DebugWr , ' ' )
        ; FOR j := InitTab + 2 TO Indent 
          DO Wr . PutChar ( DebugWr , HorizArrowChar ) 
          END (* FOR *) 
        ; Wr . PutChar ( DebugWr , VertArrowChar ) 
        ; NewLine ( ) 
        ; Indent := InitTab 
        ; WriteTab ( Indent ) 
        ; Wr . PutChar ( DebugWr , VertArrowChar ) 
        ; NewLine ( ) 
        END (* IF *) 
      ; Flush ( ) 
      END (* FOR *) 
    ; ProdPathA . Array := NIL 
    END WritePartA 

; PROCEDURE WritePartB 
    ( Gram : LRTable . GrammarTyp 
    ; VAR (* IN OUT *) Indent : INTEGER 
    ; I : ItemSsTyp 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LProdSs : LRTable . ProdNoTyp 
  ; VAR LDotPos : LbeStd . TokNoTyp 
  ; VAR LDotPos1 : LbeStd . TokNoTyp 
  ; VAR LLength : INTEGER 
  ; VAR LLength1 : INTEGER 

  ; BEGIN (* WritePartB *) 
      LProdSs := Gram . ItemArrayRef ^ [ I ] . ProdSs 
    ; LDotPos := Gram . ItemArrayRef ^ [ I ] . DotPosition - 1 
    ; LDotPos1 
        := Gram . ItemArrayRef ^ [ ItemPathB . Array ^ [ 0 ] ] . DotPosition 
    ; LLength1 := 0 
    ; WriteTab ( Indent ) 

    ; WITH WProduction = Gram . ProdArrayRef ^ [ LProdSs ] 
      DO FOR RI := 0 TO WProduction . Len - 1 
         DO WriteVoc ( Gram , WProduction . Right [ RI ] , (* VAR *) LLength ) 
         ; Wr . PutChar ( DebugWr , ' ' ) 
         ; IF RI < LDotPos 
           THEN 
             INC ( Indent , LLength + 1 ) 
           ELSIF RI < LDotPos1 
           THEN 
             INC ( LLength1 , LLength + 1 ) 
           END (* IF *) 
         END (* FOR *) 
      END (* WITH *) 
    ; DEC ( LLength1 )  (* Laenge von VertArrowChar *) 
    ; NewLine ( ) 
    ; Flush ( ) 
    ; FOR RI := 1 TO ItemPathB . Used - 1 
      DO IF Indent + LLength1 + 1 > MaxTabB AND LLength1 > 1  
         THEN 
(* REVIEW: What are we writing here? *) 
           WriteTab ( Indent ) 
         ; Wr . PutChar ( DebugWr , VertArrowChar ) 
         ; Wr . PutText ( DebugWr , "  " ) 
         ; FOR j := 3 TO LLength1 
           DO Wr . PutChar ( DebugWr , HorizArrowChar )  
           END (* FOR *) 
         ; Wr . PutChar ( DebugWr , VertArrowChar ) 
         ; NewLine ( ) 
         ; WriteTab ( Indent ) 
         ; Wr . PutChar ( DebugWr , VertArrowChar ) 
         ; Wr . PutChar ( DebugWr , ' ' ) 
         ; Wr . PutChar ( DebugWr , VertArrowChar ) 
         ; NewLine ( ) 
         ; LLength1 := 1 
         END (* IF *) 
      ; LProdSs 
          := Gram . ItemArrayRef ^ [ ItemPathB . Array ^ [ RI ] ] . ProdSs 
      ; LDotPos 
          := Gram . ItemArrayRef ^ [ ItemPathB . Array ^ [ RI ] ] 
             . DotPosition 
      ; WriteTab ( Indent ) 
      ; Wr . PutChar ( DebugWr , VertArrowChar ) 
      ; WriteTab ( LLength1 ) 
      ; WriteProd ( Gram , LProdSs , LDotPos , (* IN OUT *) LLength1 ) 
      ; NewLine ( ) 
      ; Flush ( ) 
      END (* FOR *) 
    ; WriteTab ( Indent ) 
    ; Wr . PutChar ( DebugWr , VertArrowChar ) 
    ; NewLine ( ) 
    ; Flush ( ) 
    END WritePartB 

; PROCEDURE SearchItemPathC 
    ( Gram : LRTable . GrammarTyp 
    ; VAR (* IN OUT *) ConflictSet : IntSets .  T 
    ; MaxDepth : INTEGER 
    ; depth : INTEGER 
    ; ItemSs : ItemSsTyp 
    ; VAR found : BOOLEAN 
    ) 

  = VAR LConflictSet : IntSets .  T 
  ; VAR LI : LALRTypes . ListCountTyp 

  ; BEGIN (* SearchItemPathC *) 
      WITH WItem = Gram . ItemArrayRef ^ [ ItemSs ] 
      DO 
        INC ( depth ) 
      ; LConflictSet := IntSets . Intersection ( WItem . ReadSet , ConflictSet )
      ; found := NOT IntSets . IsEmpty ( LConflictSet ) 
      ; IF found THEN ConflictSet := LConflictSet END (* IF *) 
      ; IF found 
        THEN 
          ItemPathC . Used := depth 
        ; ItemPathC . Array := NEW ( LALRTypes . ItemSsArrayRefTyp , depth ) 
        ; ItemPathC . Array ^ [ depth - 1 ] := ItemSs 
        ELSIF depth < MaxDepth 
        THEN 
          WITH WRelation = Gram . ItemArrayRef ^ [ ItemSs ] . Relation 
          DO LI := 0 
          ; WHILE LI < WRelation . Used AND NOT found 
            DO SearchItemPathC 
                 ( Gram 
                 , (* IN OUT *) ConflictSet 
                 , MaxDepth 
                 , depth 
                 , WRelation . Array ^ [ LI ] 
                 , (* VAR *) found 
                 ) 
            ; INC ( LI ) 
            END (* WHILE *) 
          ; IF found 
            THEN 
              ItemPathC . Array ^ [ depth - 1 ] := ItemSs 
            END (* IF *) 
          END (* WITH *) 
        END (* IF *) 
      END (* WITH *) 
    END SearchItemPathC 

; PROCEDURE FindItemPathC 
    ( Gram : LRTable . GrammarTyp 
    ; VAR (* IN OUT *) ConflictSet : IntSets .  T 
    ; ItemSs : ItemSsTyp 
    ) 

  = VAR LMaxDepth : INTEGER 
  ; VAR LFound : BOOLEAN 
  ; VAR LI : LALRTypes . ListCountTyp 

  ; BEGIN (* FindItemPathC *) 
      LMaxDepth := 0 
    ; LFound := FALSE 
    ; REPEAT 
        INC ( LMaxDepth ) 
      ; WITH WRelation = Gram . ItemArrayRef ^ [ ItemSs ] . Relation 
        DO LI := 0 
        ; WHILE LI < WRelation . Used AND NOT LFound 
          DO SearchItemPathC 
               ( Gram 
               , (* IN OUT *) ConflictSet 
               , LMaxDepth (* depth := *) 
               , 0 
               , WRelation . Array ^ [ LI ] 
               , (* VAR *) LFound 
               ) 
          ; INC ( LI ) 
          END (* WHILE *) 
        END (* WITH *) 
      UNTIL LFound 
    END FindItemPathC 

; PROCEDURE UnRepItemPathC ( Gram : LRTable . GrammarTyp ) 
    (* Waehle moeglichst kurz zu beendende Items aus *) 

  = VAR LStateSs : StateSsTyp 
  ; VAR LPathItemSs : ItemSsTyp 
  ; VAR LPathDerivable , LDerivable : PortTypes . Int32Typ 
(* FIXME: What type is this?  A Character number, not limited. *) 

  ; BEGIN (* UnRepItemPathC *) 
      FOR RI := 0 TO ItemPathC . Used - 2 
      DO LPathItemSs := ItemPathC . Array ^ [ RI ] 
      ; WITH WPathItem = Gram . ItemArrayRef ^ [ LPathItemSs ] 
        DO WITH WProd = Gram . ProdArrayRef ^ [ WPathItem . ProdSs ] 
           DO LPathDerivable := 0 
           ; FOR j := WPathItem . DotPosition TO WProd . Len - 1 
             DO INC 
                  ( LPathDerivable 
                  , LRUtils . ShortestDerivable ( Gram , WProd . Right [ j ] ) 
                  ) 
             END (* FOR *) 
           END (* WITH *) 
        ; LStateSs := WPathItem . Number 
        ; WITH WState = Gram . StateArrayRef ^ [ LStateSs ] 
          DO FOR RItemSs := WState . FirstItemSs 
                 TO WState . FirstItemSs + WState . ItemCt - 1 
             DO IF Gram . ItemArrayRef ^ [ RItemSs ] . RepItemSs 
                   = WPathItem . RepItemSs 
                THEN 
                  WITH 
                    WProd 
                    = Gram . ProdArrayRef ^ 
                        [ Gram . ItemArrayRef ^ [ RItemSs ] . ProdSs ] 
                  DO LDerivable := 0 
                  ; FOR j := Gram . ItemArrayRef ^ [ RItemSs ] . DotPosition 
                          TO WProd . Len - 1 
                    DO INC 
                         ( LDerivable 
                         , LRUtils . ShortestDerivable 
                             ( Gram , WProd . Right [ j ] ) 
                         ) 
                    END (* FOR *) 
                  ; IF LDerivable < LPathDerivable 
                    THEN 
                      LPathItemSs := RItemSs 
                    ; LPathDerivable := LDerivable 
                    END (* IF *) 
                  END (* WITH *) 
                END (* IF *) 
             END (* FOR *) 
          END (* WITH *) 
        ; ItemPathC . Array ^ [ RI ] := LPathItemSs 
        END (* WITH *) 
      END (* FOR *) 
    END UnRepItemPathC 

; PROCEDURE WritePartC 
    ( Gram : LRTable . GrammarTyp 
    ; VAR (* IN OUT *) Indent : INTEGER 
    ; I : ItemSsTyp 
    ; Tok : LbeStd . TokTyp 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LProdSs : LRTable . ProdNoTyp 
  ; VAR LDotPos : INTEGER 
  ; VAR LLength1 : INTEGER 

  ; BEGIN (* WritePartC *) 
      FOR RI := ItemPathC . Used - 2 TO 0 BY - 1 
      DO IF Indent > MaxTabC 
         THEN 
           WriteTab ( InitTab ) 
         ; Wr . PutChar ( DebugWr , ' ' )
         ; FOR j := InitTab + 2 TO Indent 
           DO Wr . PutChar ( DebugWr , HorizArrowChar )  
           END (* FOR *) 
         ; Wr . PutChar ( DebugWr , VertArrowChar ) 
         ; NewLine ( ) 
         ; Indent := InitTab 
         ; WriteTab ( Indent ) 
         ; Wr . PutChar ( DebugWr , VertArrowChar ) 
         ; NewLine ( ) 
        ; Flush ( ) 
         END (* IF *) 
      ; LProdSs 
          := Gram . ItemArrayRef ^ [ ItemPathC . Array ^ [ RI ] ] . ProdSs 
      ; LDotPos 
          := Gram . ItemArrayRef ^ [ ItemPathC . Array ^ [ RI ] ] 
             . DotPosition 
      ; WriteTab ( Indent ) 
      ; WriteProd ( Gram , LProdSs , LDotPos , (* IN OUT *) Indent ) 
      ; NewLine ( ) 
      ; Flush ( ) 
      END (* FOR *) 

    (* Fortsetzung fuer Reduce *) 
    ; WITH 
        WProd 
        = Gram . ProdArrayRef ^ [ Gram . ItemArrayRef ^ [ I ] . ProdSs ] 
      DO LLength1 := Indent 
      ; LProdSs := Gram . ItemArrayRef ^ [ I ] . ProdSs 
      ; LDotPos := Gram . ItemArrayRef ^ [ I ] . DotPosition 
      ; WriteTab ( LLength1 ) 
      ; WriteProd ( Gram , LProdSs , LDotPos , (* IN OUT *) LLength1 ) 
      ; NewLine ( ) 
      ; Flush ( ) 

    (* erlaeutere Reduce *) 
      ; LDotPos := VocLength ( Gram , WProd . Left ) 
      ; IF Indent >= ( 4 + 7 + LDotPos ) 
        THEN 
          DEC ( Indent , 4 + 7 + LDotPos ) 
                  (* Laenge Text "reduce " + Laenge linke Seite *) 
        ELSE  (* = Laenge ' -> ' *) 
          WriteTab ( Indent ) 
        ; Wr . PutChar ( DebugWr , VertArrowChar ) 
        ; FOR j := Indent + 1 TO 4 + 7 + LDotPos 
          DO Wr . PutChar ( DebugWr , HorizArrowChar ) 
          END (* FOR *) 
        ; NewLine ( ) 
        ; WriteTab ( 4 + 7 + LDotPos + 1 ) 
        ; Wr . PutChar ( DebugWr , VertArrowChar ) 
        ; NewLine ( ) 
        ; Indent := 0 
        END (* IF *) 
      ; WriteTab ( Indent ) 
      ; Wr . PutText ( DebugWr , "reduce " ) 
      ; WriteVoc ( Gram , WProd . Left , (* VAR *) LDotPos ) 
      ; Wr . PutText ( DebugWr , " ::= " ) 
      ; IF WProd . Len = 0 
        THEN 
          Wr . PutText ( DebugWr , "<empty>" ) 
        ELSE 
          FOR RI := 0 TO WProd . Len - 1 
          DO WriteVoc ( Gram , WProd . Right [ RI ] , (* VAR *) LDotPos ) 
          ; IF RI < WProd . Len - 1 
            THEN 
              Wr . PutChar ( DebugWr , ' ' ) 
            END (* IF *) 
          END (* FOR *) 
        END (* IF *) 
      ; Wr . PutText ( DebugWr , Infos . LRDotString ) 
      ; Wr . PutText ( DebugWr , " {" ) 
      ; WriteVoc ( Gram , Tok , (* VAR *) LDotPos ) 
      ; Wr . PutText ( DebugWr , "} ?" ) 
      ; NewLine ( ) 
      ; Flush ( ) 
      END (* WITH *) 
    END WritePartC 

; PROCEDURE MakeChainD ( Gram : LRTable . GrammarTyp ) 

  = VAR LStateSs : StateSsTyp 
  ; VAR LReadTok : LbeStd . TokTyp 

  ; PROCEDURE PutInChain 
      ( ItemSs : ItemSsTyp ; Last : LALRTypes . ListCountTyp ) 

    = VAR LStateSs : StateSsTyp 
    ; VAR LI : ItemSsTyp 
    ; VAR LCount : LALRTypes . ListCountTyp 
    ; VAR LNewCount : LALRTypes . ListCountTyp 
    ; VAR LNewChainRef : ItemChainArrayRefTyp 

    ; BEGIN (* PutInChain *) 
      (* Zum ItemSs gehoerige Produktion *) 
        WITH 
          WProd 
          = Gram . ProdArrayRef 
            ^ [ Gram . ItemArrayRef ^ [ ItemSs ] . ProdSs ] 

        DO (* Betrachte alle zur Produktion gehoerigen Items *) 
           LCount := NUMBER ( ChainD . chain ^ ) 
        ; WHILE Gram . ItemArrayRef ^ [ ItemSs ] . DotPosition < WProd . Len 
                AND NOT IntSets . IsElement 
                          ( ItemSs , ChainD . ReachedItemSet ) 
          DO (* ItemSs in Kette eintragen *) 
             IF ChainD . Used >= LCount 
             THEN 
               LNewCount := 2 * LCount 
             ; LNewChainRef := NEW ( ItemChainArrayRefTyp , LNewCount ) 
             ; SUBARRAY ( LNewChainRef ^ , 0 , LCount ) := ChainD . chain ^ 
             ; ChainD . chain := LNewChainRef 
             ; LCount := LNewCount 
             END (* IF *) 
          ; ChainD . chain ^ [ ChainD . Used ] . Last := Last 
          ; ChainD . chain ^ [ ChainD . Used ] . ItemSs := ItemSs 
          ; INC ( ChainD . Used ) 
          ; ChainD . ReachedItemSet 
              := IntSets . Include ( ChainD . ReachedItemSet , ItemSs ) 

          (* Punkt nach rechts schieben *) 
          ; LStateSs (* Folgezustand *) 
              := Gram . ItemArrayRef ^ [ ItemSs ] . GoToStateSs 
          ; LI 
              := Gram . StateArrayRef ^ [ LStateSs ] . FirstItemSs 
                 (* erstes Item *) 

          (* suche ItemSs mit selber Produktion und um 1 groesserer Position *)
          ; WHILE Gram . ItemArrayRef ^ [ LI ] . ProdSs 
                  # Gram . ItemArrayRef ^ [ ItemSs ] . ProdSs 
                  OR Gram . ItemArrayRef ^ [ LI ] . DotPosition 
                     # Gram . ItemArrayRef ^ [ ItemSs ] . DotPosition + 1 
            DO INC ( LI ) 
            END (* WHILE *) 
          ; ItemSs := LI 
          END (* WHILE *) 
        END (* WITH *) 
      END PutInChain 

  ; BEGIN (* MakeChainD *) 
      VAR LastCount : PortTypes . Int32Typ 
    ; VAR LItemSs : ItemSsTyp 

    ; BEGIN (* Block  Chain initialisieren *) 
        IF ChainD . chain = NIL 
        THEN 
          ChainD . Used := 0 
        ; ChainD . level := ChainNull 
        ; ChainD . chain := NEW ( ItemChainArrayRefTyp , InitChainLength ) 
        ; ChainD . ReachedItemSet := IntSets . Empty ( ) 
        ; PutInChain ( 0 , ChainNull ) 

        ; LOOP 
            LastCount := ChainD . Used 
          ; IF ChainD . level = LastCount - 1 THEN EXIT END (* IF *) 
          ; WHILE ChainD . level < LastCount - 1 
            DO INC ( ChainD . level ) 
            ; LItemSs := ChainD . chain ^ [ ChainD . level ] . ItemSs 

          (* Falls Nichtterminal nach dem Punkt steht, wird 
             weiterverfolgt *) 
            ; LReadTok := Gram . ItemArrayRef ^ [ LItemSs ] . ReadTok 
            ; IF LReadTok >= Gram . FirstAstNonterminal 
                 AND LReadTok <= Gram . LastNonterminal 
              THEN (* moegliche Fortsetzungen betrachten *) 
                LStateSs := Gram . ItemArrayRef ^ [ LItemSs ] . Number 
              ; FOR RI := Gram . StateArrayRef ^ [ LStateSs ] . FirstItemSs 
                    TO Gram . StateArrayRef ^ [ LStateSs ] . FirstItemSs 
                       + Gram . StateArrayRef ^ [ LStateSs ] . ItemCt 
                       - 1 
                DO WITH WItem = Gram . ItemArrayRef ^ [ RI ] 
                   DO IF Gram . ProdArrayRef ^ [ WItem . ProdSs ] . Left 
                         = LReadTok 
                         AND Gram . ItemArrayRef ^ [ RI ] . DotPosition = 0 
                      THEN 
                        PutInChain ( RI , ChainD . level ) 
                      END (* IF *) 
                   END (* WITH *) 
                END (* FOR *) 
              END (* IF *) 
            END (* WHILE *) 
          END (* LOOP *) 
        END (* IF *) 
      END (* Block  IF *) (* Item suchen *) 
    END MakeChainD 

; PROCEDURE FindProdPathD 
    ( Gram : LRTable . GrammarTyp 
    ; NT : LbeStd . TokTyp 
    ; EndState : StateSsTyp 
    ) 

  = VAR LLast : LALRTypes . ListCountTyp 
  ; VAR LItemSs : ItemSsTyp 
  ; VAR LDepth : LALRTypes . ListCountTyp 

  ; BEGIN (* FindProdPathD *) 
    (* evtl. (d.h. beim ersten mal) Kette aufbauen *) 
      MakeChainD ( Gram ) 
    ; LLast := 0 
    ; LOOP  
        IF LLast >= ChainD . Used 
        THEN (* This loop once failed to find its target. *) 
          ProdPathD . Used := 0 
        ; RETURN 
        END (* IF *) 
      ; LItemSs := ChainD . chain ^ [ LLast ] . ItemSs 
      ; IF Gram . ItemArrayRef ^ [ LItemSs ] . Number = EndState 
        THEN 
          IF NT 
             = Gram . ProdArrayRef ^ 
                 [ Gram . ItemArrayRef ^ [ LItemSs ] . ProdSs ] 
               . Left 
          THEN 
            EXIT 
          END (* IF *) 
        END (* IF *) 
      ; INC ( LLast ) 
      END (* LOOP *) 

    (* Tiefe bestimmen *) 
    ; LDepth := 0 
    ; ChainD . level := LLast 
    ; WHILE ChainD . level # ChainNull 
      DO INC ( LDepth ) 
      ; ChainD . level := ChainD . chain ^ [ ChainD . level ] . Last 
      END (* WHILE *) 

    (* Chain in Path uebertragen *) 
    ; ProdPathD . Used := LDepth 
    ; ProdPathD . Array := NEW ( ProdPathArrayRefTyp , LDepth ) 
    ; ChainD . level := LLast 
    ; WHILE LDepth > 0 
      DO LItemSs := ChainD . chain ^ [ ChainD . level ] . ItemSs 
      ; ProdPathD . Array ^ [ LDepth - 1 ] . ProdSs 
          := Gram . ItemArrayRef ^ [ LItemSs ] . ProdSs 
      ; ProdPathD . Array ^ [ LDepth - 1 ] . DotPosition 
          := Gram . ItemArrayRef ^ [ LItemSs ] . DotPosition 
      ; DEC ( LDepth ) 
      ; ChainD . level := ChainD . chain ^ [ ChainD . level ] . Last 
      END (* WHILE *) 
    END FindProdPathD 

; PROCEDURE WritePartD 
    ( Gram : LRTable . GrammarTyp 
    ; dist : INTEGER 
    ; StateSs : StateSsTyp 
    ; Tok : LbeStd . TokTyp 
    ; RedItemSs : ItemSsTyp 
    ; VAR (* IN OUT *) ExplainedItemSet : IntSets .  T 
    ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LLength : INTEGER 
  ; VAR LPos : INTEGER 

  ; BEGIN (* WritePartD *) 
      Wr . PutText ( DebugWr , "RedItemSs: " )
    ; Wr . PutText ( DebugWr , Fmt . Int ( RedItemSs ) )
    ; NewLine ( ) 

    ; WITH 
        WRedProd 
        = Gram . ProdArrayRef 
          ^ [ Gram . ItemArrayRef ^ [ RedItemSs ] . ProdSs ] 
      , WState = Gram . StateArrayRef ^ [ StateSs ] 
      DO FOR RItemSs := WState . FirstItemSs 
             TO WState . FirstItemSs + WState . ItemCt - 1 
         DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
            DO IF WItem . ReadTok = Tok 
                  AND NOT IntSets . IsElement ( RItemSs , ExplainedItemSet ) 
               THEN 
                 ExplainedItemSet 
                   := IntSets . Include ( ExplainedItemSet , RItemSs ) 
; Wr . PutText ( DebugWr , "RItemSs: " )
; Wr . PutText ( DebugWr , Fmt . Int ( RItemSs ) )
; NewLine ( ) 
               ; LPos := InitTab 
; Wr . PutText ( DebugWr , "ProdSs: " )
; Wr . PutText ( DebugWr , Fmt . Int ( WItem . ProdSs ) )
; NewLine ( ) 
               ; WITH WProd = Gram . ProdArrayRef ^ [ WItem . ProdSs ] 
                 DO IF ( WItem . DotPosition 
                         # Gram . ItemArrayRef ^ [ RedItemSs ] . DotPosition 
                       ) 
                       OR ( WProd . Left # WRedProd . Left ) 
                    THEN 
                      (* Drucke Trace fuer Read - Ableitung von Startzustand *)
                      NewLine ( ) 
                    ; FindProdPathD ( Gram , WProd . Left , StateSs ) 
                    ; FOR RI := 0 TO ProdPathD . Used - 2 
                      DO WriteTab ( LPos ) 
                      ; WriteProd 
                          ( Gram 
                          , ProdPathD . Array ^ [ RI ] . ProdSs 
                          , ProdPathD . Array ^ [ RI ] . DotPosition 
                          , (* IN OUT *) LPos 
                          ) 
                      ; NewLine ( ) 
                      ; Flush ( ) 
                      ; IF LPos > MaxTabD 
                        THEN 
                          WriteTab ( InitTab ) 
                        ; Wr . PutChar ( DebugWr , ' ' ) 
                        ; FOR j := InitTab + 2 TO LPos 
                          DO Wr . PutChar ( DebugWr , HorizArrowChar ) 
                          END (* FOR *) 
                        ; Wr . PutChar ( DebugWr , VertArrowChar ) 
                        ; NewLine ( ) 
                        ; LPos := InitTab 
                        ; WriteTab ( LPos ) 
                        ; Wr . PutChar ( DebugWr , VertArrowChar ) 
                        ; NewLine ( ) 
                        ; Flush ( ) 
                        END (* IF *) 
                      END (* FOR *) 
                    ; ProdPathD . Array := NIL 
                    ; WriteTab ( LPos ) 
                    ; WriteProd 
                        ( Gram , WItem . ProdSs , 0 , (* IN OUT *) LPos ) 
                    ; NewLine ( ) 
                    ; Flush ( ) 
                    ; LLength := VocLength ( Gram , WProd . Left ) 
                    ; IF LPos >= ( 4 + 7 + LLength ) 
                      THEN 
                        DEC ( LPos , 4 + 7 + LLength )    
                              (* Laenge Text "shift  " + Laenge linke Seite *) 
                      ELSE (* + Laenge ' -> ' *) 
                        WriteTab ( LPos ) 
                      ; Wr . PutChar ( DebugWr , VertArrowChar ) 
                      ; FOR j := LPos + 1 TO 4 + 7 + LLength 
                        DO Wr . PutChar ( DebugWr , HorizArrowChar ) 
                        END (* FOR *) 
                      ; NewLine ( ) 
                      ; WriteTab ( 4 + 7 + LLength + 1 ) 
                      ; Wr . PutChar ( DebugWr , VertArrowChar ) 
                      ; NewLine ( ) 
                      ; Flush ( ) 
                      ; LPos := 0 
                      END (* IF *) 
                    ELSE (* Trace der Reduktion passt zum Read *) 
                           (* selbe Distanz wie bei Reduktion verwenden *) 
                      LPos := dist 
                    END (* IF *) 

                   (* erlaeutere Read *) 
                 ; WriteTab ( LPos ) 
                 ; Wr . PutText ( DebugWr , "shift  " ) 
                 ; WriteVoc ( Gram , WProd . Left , (* VAR *) LLength ) 
                 ; Wr . PutText ( DebugWr , " ::= " ) 
                 ; IF WItem . DotPosition = 0 
                   THEN 
                     Wr . PutText ( DebugWr , Infos . LRDotString ) 
                   END (* IF *) 
                 ; FOR RI := 0 TO WProd . Len - 1 
                   DO WriteVoc 
                        ( Gram , WProd . Right [ RI ] , (* VAR *) LLength ) 
                   ; IF RI = WItem . DotPosition - 1 
                     THEN 
                       Wr . PutText ( DebugWr , Infos . LRDotString ) 
                     ELSIF RI < WProd . Len - 1 
                     THEN 
                       Wr . PutChar ( DebugWr , ' ' ) 
                     END (* IF *) 
                   END (* FOR *) 
                 ; Wr . PutText ( DebugWr , " ?" ) 
                 ; NewLine ( ) 
                 ; Flush ( ) 
                 END (* WITH *) 
               END (* IF *) 
            END (* WITH *) 
         END (* FOR *) 
      END (* WITH *) 
    END WritePartD 

; PROCEDURE DebugRedItem 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : StateSsTyp            (* Zustand in dem der Konflikt auftritt *)
    ; ConflictSet : IntSets .   T 
    ; ItemSs : ItemSsTyp              (* am Konflikt beteiligte Reduktion *) 
    ; VAR (* IN OUT *) ExplainedItemSet : IntSets .  T 
    ) 

  = PROCEDURE VisitConflictTok ( Tok : IntSets . ValidElemT ) 

    = VAR LItemSs : ItemSsTyp 
    ; VAR LPos : INTEGER 

    ; BEGIN 
        Wr . PutText ( DebugWr , "Token: " )
      ; Wr . PutText ( DebugWr , Fmt . Int ( Tok ) )
      ; NewLine ( ) 
      ; WITH 
          WItem 
          = Gram . ItemArrayRef ^ 
              [ ItemPathC . Array ^ [ ItemPathC . Used - 1 ] ] 
        DO WITH WState = Gram . StateArrayRef ^ [ WItem . GoToStateSs ] 
           DO LItemSs := WState . FirstItemSs 
           ; LOOP 
               IF LItemSs >= WState . FirstItemSs + WState . ItemCt 
               THEN 
                 EXIT 
               END (* IF *) 

             (* Pruefe ob Terminal Tok moeglich *) 
             ; IF Possible ( Gram , LItemSs , Tok ) 
               THEN 
                 Wr . PutText ( DebugWr , "Item Ss: " )
               ; Wr . PutText ( DebugWr , Fmt . Int ( LItemSs ) )
               ; NewLine ( ) 
               ; LPos := InitTab  (* akt. Randabstand *) 
               ; IF NOT Fast 
                 THEN (* wie kommt man von Startsymbol zum Problem *) 
                   Wr . PutText ( DebugWr , "Part A -------------------------" )
                 ; NewLine ( ) 
                 ; WritePartA 
                     ( Gram 
                     , (* IN OUT *) LPos 
                     , Gram . ProdArrayRef ^ 
                         [ Gram . ItemArrayRef ^ [ LItemSs ] . ProdSs ] 
                       . Left 
                     ) 
                 END (* IF *) 

               (* wie kommt man zu Vorschauzeichen *) 
               ; Wr . PutText ( DebugWr , "Part B -------------------------" )
               ; NewLine ( ) 
               ; WritePartB ( Gram , (* IN OUT *) LPos , LItemSs ) 

               (* wie kommt man zur linken Seite der Red *) 
               ; Wr . PutText ( DebugWr , "Part C -------------------------" )
               ; NewLine ( ) 
               ; WritePartC ( Gram , (* IN OUT *) LPos , ItemSs , Tok ) 

               (* womit kollidiert die Reduktion *) 
               ; Wr . PutText ( DebugWr , "Part D -------------------------" )
               ; NewLine ( ) 
               ; WritePartD 
                   ( Gram 
                   , LPos 
                   , StateSs 
                   , Tok 
                   , ItemSs 
                   , (* IN OUT *) ExplainedItemSet 
                   )

               ; NewLine ( ) 
               ; NewLine ( ) 
               ; ItemPathB . Array := NIL 
               ; EXIT 
               END (* IF *) 
             ; INC ( LItemSs ) 
             END (* LOOP *) 
           END (* WITH *) 
        END (* WITH *) 
      END VisitConflictTok 

  ; BEGIN (* DebugRedItem *) 
      VAR LConflictSet : IntSets .  T 
    ; BEGIN (* Block *) 
        LConflictSet := ConflictSet  
      ; FindItemPathC ( Gram , (* IN OUT *) LConflictSet , ItemSs ) 
          (* fuer Part C *) 
      ; UnRepItemPathC ( Gram )
      ; IntSets . ForAllDo ( LConflictSet , VisitConflictTok ) 
      ; ItemPathC . Array := NIL 
      END (* Block *) 
    END DebugRedItem 

(* VISIBLE: *) 
; PROCEDURE DebugState 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : LRTable . StateNoTyp (* inconsistent State *) 
    ; ConflictingLookaheadTokSet : IntSets .  T (* Conflict Set *) 
    ) 
  (* Erzeuge Zusatzinformation zum Zustand 'State' mit Konfliktmenge 'Set' *) 
  (* wird fuer jeden inkonsitenten Zustand ausgefuehrt *) 

  = VAR LExplainedItemSet : IntSets .  T (* Explained Items *) 

  ; BEGIN (* DebugState *) 
      IF NoTrace THEN RETURN END (* IF *) 
    ; NewLine ( ) 

    (* finde alle Reduktionen die an einem Konflikt beteiligt sind *) 
    ; WITH WState = Gram . StateArrayRef ^ [ StateSs ] 
      DO LExplainedItemSet := IntSets . Empty ( )
      ; FOR RItemSs := WState . FirstItemSs 
            TO WState . FirstItemSs + WState . ItemCt - 1 
        DO WITH WItem = Gram . ItemArrayRef ^ [ RItemSs ] 
           DO IF WItem . Rep = ReprKindTyp . RedRep 
              THEN 
                IF NOT IntSets . Disjoint 
                         ( ConflictingLookaheadTokSet , WItem . Set ) 
                THEN (* Bearbeite konfliktbeladene Reduktion *) 
                  DebugRedItem 
                    ( Gram 
                    , StateSs 
                    , ConflictingLookaheadTokSet 
                    , RItemSs 
                    , (* IN OUT *) LExplainedItemSet 
                    )
                END (* IF *) 
              END (* IF *) 
           END (* WITH *) 
        END (* FOR *) 
      END (* WITH *) 
    END DebugState 

; PROCEDURE Check ( ) 

  = <* FATAL AssertionFailure *> 
    BEGIN
      Assert ( ChainNull # 0 , AFT . A_Debug_ChainNullIsZero ) 
    END Check 

; BEGIN (* Debug *) 
    NoTrace := TRUE (* There is still at least a second loopup in this. *) 
  ; Fast := FALSE 
  ; ChainD . chain := NIL 
  ; Check ( ) 
  END Debug 
. 

