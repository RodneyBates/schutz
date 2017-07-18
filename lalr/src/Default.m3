
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001 .. 2007, from Cocktail, lalr, Default.mi, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Compute default states *) 
(* Here, "default" refers to a table compression technique.  A state S1 can
   appeal to a different state S2, for those transitions that are the same
   in S2 as in S1.  In this case, S1 will denote S2 as its default.  Then 
   the comb vector for S1 can have an empty element for any transitions that
   are to be referred to the comb vector of S2.  This knocks teeth off the
   comb vector of S1, making it easier to find a place for it. 
*) 

MODULE Default 

; IMPORT IntSets 
; IMPORT LALRTypes 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT PortTypes 

; TYPE TransitionTyp 
    = RECORD (* Uebergang*) 
        Tok : LbeStd . TokTyp 
      ; StateNo : LRTable . StateNoTyp (* Folgezustand *) 
      ; Link : TransitionRefTyp        (* weitere Uebergaenge *) 
      END (* RECORD *) 

; TYPE TransitionRefTyp = REF TransitionTyp (* Uebergange *) 

; TYPE TableRowTyp 
    = RECORD 
        Transitions : TransitionRefTyp  (* Uebergaenge sortiert nach Tok *) 
(* TODO:  ^Replace this linked list by a VLA.  This will require precounting
          of terminal transitions.  This could probably be done in 
          MakeTableLink, passed back to Gen.MakeTable, thne into 
          PutInDefaultList.
*) 
      ; Terms : IntSets . T             (* Terminale fuer die es einen 
                                           Uebergang gibt.  Includes Ast
                                           nonterminals. *) 
                                        (* Biased - Gram . FirstTerminal *) 
      ; Succ : LRTable . StateNoTyp   (* Nachfolger in sortierter Liste *) 
      ; DefaultState : LRTable . StateNoTyp (* Defaultzustand *) 
      ; Saving : PortTypes . Card32Typ  (* Einsparung durch Defaultzustand *) 
      ; NontermCount : PortTypes . Card32Typ (* NTs with out-transitions. *) 
      ; ToTest : BOOLEAN 
      ; InTest : BOOLEAN 
      END (* RECORD *) 

; TYPE TableRefTyp = REF ARRAY (* LRTable . StateNoTyp *) OF TableRowTyp 

; CONST Brand = "Default.T" 

; REVEAL T 
    = Public 
        BRANDED 
          Brand 
          OBJECT 
            Gram : LRTable . GrammarTyp 
          ; TableRef : TableRefTyp 
          (* TableRef ^ [ LRTable . StateNoNull ] bildet den Anker *) 
          ; TSorting : LRTable . StateNoArrayRefTyp := NIL 
          ; NSorting : LRTable . StateNoArrayRefTyp := NIL 
          OVERRIDES 
            Init := Init 
          END (* OBJECT *) 

(* VISIBLE: *) 
; PROCEDURE New ( Gram : LRTable . GrammarTyp ) : T 

  = BEGIN (* New *) 
      RETURN NEW ( T ) . Init ( Gram ) 
    END New 

; PROCEDURE Init ( Self : T ; Gram : LRTable . GrammarTyp ) : T 
  (* Erzeugen / Initialisieren der Datenstruktur zur Berechnug der 
     Defaultzustaende *) 

  = BEGIN (* Init *) 
      Self . Gram := Gram 
    ; Self . TableRef 
        := NEW ( TableRefTyp , Self . Gram . FirstReadRedAction ) 
    ; Self . TSorting := NIL 
    ; Self . NSorting := NIL 

    ; FOR RStateNo := 0 TO NUMBER ( Self . TableRef ^ ) - 1 
      DO WITH WLine = Self . TableRef ^ [ RStateNo ] 
         DO WLine . Transitions := NIL 
         ; WLine . Terms := IntSets . Empty ( ) 
         ; WLine . Succ := LRTable . StateNoNull 
         ; WLine . DefaultState := LRTable . StateNoNull 
         ; WLine . Saving := 0 
         ; WLine . NontermCount := 0 
         ; WLine . ToTest := TRUE 
         ; WLine . InTest := FALSE 
         END (* WITH *) 
      END (* FOR *) 
    ; RETURN Self 
    END Init 

(* VISIBLE: *) 
; PROCEDURE GetNextStateNo 
    ( Self : T ; StateNo : LRTable . StateNoTyp ) : LRTable . StateNoTyp 
  (* Fortschalten des Zustandes nach festgelegter Strategie *) 
  (* Strategie : Zuerst Zustaende mit vielen Eintraegen *) 

  = BEGIN (* GetNextStateNo *) 
      RETURN Self . TableRef ^ [ StateNo ] . Succ 
    END GetNextStateNo 

(* VISIBLE: *) 
; PROCEDURE PutInDefaultList 
    ( Self : T 
    ; ReadStateCode : LRTable . StateNoTyp 
    ; READONLY TableLine : LRTable . StateNoArrayRefTyp 
    ) 
  (* Eintragen der in TableLine enthaltenen Zeile in die Datenstruktur *) 

  = VAR LTrans : TransitionRefTyp 
  ; VAR LTransSucc : TransitionRefTyp 
  ; VAR LStateNo : LRTable . StateNoTyp 
  ; VAR LSuccTerms : LbeStd . TokNoTyp 
  ; VAR LTermTransitionCt : LbeStd . TokNoTyp 

  ; BEGIN (* PutInDefaultList *) 
      WITH WReadStateRow = Self . TableRef ^ [ ReadStateCode ] 
      DO WReadStateRow . Transitions := NIL 
      ; LTermTransitionCt := 0 
      ; FOR RTok := Self . Gram . LastNonterminal 
            TO Self . Gram . FirstTerminal BY - 1 
        DO 
          WITH WTransState = TableLine [ RTok - Self . Gram . FirstTerminal ] 
          DO 
            IF WTransState # LRTable . StateNoNull 
            THEN 
              LTrans := NEW ( TransitionRefTyp ) 
            ; LTrans ^ . Tok := RTok 
            ; LTrans ^ . StateNo := WTransState 
            ; LTrans ^ . Link := WReadStateRow . Transitions 
            ; WReadStateRow . Transitions := LTrans 
            ; IF Self . Gram . FirstTerminal <= RTok 
                 AND RTok <= Self . Gram . LastAstNonterminal 
                 (* ^Tokens parser can return. *) 
              THEN 
                WReadStateRow . Terms 
                  := IntSets . Include ( WReadStateRow . Terms , RTok ) 
              ; INC ( LTermTransitionCt ) 
              ELSE 
                INC ( WReadStateRow . NontermCount ) 
              END (* IF *) 
            END (* IF *) 
          END (* WITH *) 
        END (* FOR *) 

      (* Einsortieren *) 
      ; LStateNo := LRTable . StateNoNull 

      (* Suche Element vor (State) bzw. nach (Succ) dem einzufuegenden Element *) 
      ; LOOP 
          WReadStateRow . Succ := Self . TableRef ^ [ LStateNo ] . Succ 
        ; IF WReadStateRow . Succ = LRTable . StateNoNull 
          THEN 
            EXIT 
          ELSE 
            WITH WSuccRow = Self . TableRef ^ [ WReadStateRow . Succ ] 
            DO LSuccTerms := IntSets . Card ( WSuccRow . Terms ) 
            ; IF LTermTransitionCt > LSuccTerms 
              THEN (* Rows having more terminal transitions sort earlier. *) 
                EXIT 
              ELSIF LTermTransitionCt = LSuccTerms 
              THEN 
                LTrans := WReadStateRow . Transitions 
              ; LTransSucc := WSuccRow . Transitions 
              ; WHILE LTrans # NIL 
                      AND LTransSucc # NIL 
                      AND LTrans ^ . Tok = LTransSucc ^ . Tok 
                DO LTrans := LTrans ^ . Link 
                ; LTransSucc := LTransSucc ^ . Link 
                END (* WHILE *) 
              ; IF LTrans # NIL 
                   AND LTransSucc # NIL 
                   AND LTrans ^ . Tok > LTransSucc ^ . Tok 
                THEN (* Row with higher-numbered Token first. *) 
                  EXIT 
                END (* IF *) 
              END (* IF *) 
            END (* WITH *) 
          END (* IF *) 
        ; LStateNo := Self . TableRef ^ [ LStateNo ] . Succ 
        END (* LOOP *) 

      (* Einketten *) 
      (* Als Nachfolger beim Vorgaenger eintragen *) 
      ; Self . TableRef ^ [ LStateNo ] . Succ := ReadStateCode 
      (* Nachfolger ist bereits eingetragen *) 
      END (* WITH *) 
    END PutInDefaultList 

; PROCEDURE ComputeSaving 
    ( Self : T 
    ; StateNo : LRTable . StateNoTyp 
    ; DefState : LRTable . StateNoTyp 
    ) 
    : PortTypes . Card32Typ 
  (* Zero result could mean use of DefState as a default for StateNo is 
     incorrect, not merely unhelpful *) 

  = VAR LDefSaving : PortTypes . Card32Typ 
  ; VAR LTrans , DefTrans : TransitionRefTyp 

  ; BEGIN (* ComputeSaving *) 
      LDefSaving := 0 

    (* ist DefState als Defaultzustand brauchbar ? *) 
    ; IF IntSets . IsSubset 
           ( Self . TableRef ^ [ DefState ] . Terms 
           , Self . TableRef ^ [ StateNo ] . Terms 
           ) 
      THEN (* Berechne Einsparung *) 
        LTrans := Self . TableRef ^ [ StateNo ] . Transitions 
      ; DefTrans := Self . TableRef ^ [ DefState ] . Transitions 
      ; WHILE DefTrans # NIL 
              AND DefTrans ^ . Tok <= Self . Gram . LastAstNonterminal 
        DO (* Zeichen, fuer die DefState keinen Eintrag hat, 
              in StateNo ueberlesen *) 
           WHILE LTrans ^ . Tok # DefTrans ^ . Tok 
           DO LTrans := LTrans ^ . Link 
           END (* WHILE *) 
        ; IF LTrans ^ . StateNo = DefTrans ^ . StateNo 
          THEN 
            INC ( LDefSaving ) 
          END (* IF *) 
        ; DefTrans := DefTrans ^ . Link 
        ; LTrans := LTrans ^ . Link 
        END (* WHILE *) 
      END (* IF *) 
    ; RETURN LDefSaving 
    END ComputeSaving 

; PROCEDURE ComputeBestDefaults ( Self : T ) 
  (* Berechnug des besten Defaultzustandes ohne Ruecksicht auf Zyklen *) 

  = VAR LLastStateNo : LRTable . StateNoTyp 
  ; VAR LStateNo : LRTable . StateNoTyp 
  ; VAR LDefStateNo : LRTable . StateNoTyp 
  ; VAR LDefSaving : PortTypes . Card16Typ 
  ; VAR LComparingBackwards : BOOLEAN 

  ; BEGIN (* ComputeBestDefaults *) 
      LStateNo := Self . TableRef ^ [ LRTable . StateNoNull ] . Succ 
    ; LLastStateNo := LStateNo 
    ; WHILE LStateNo # LRTable . StateNoNull 
      DO WITH WStateRow = Self . TableRef ^ [ LStateNo ] 
         DO IF NOT IntSets . Equal 
                     ( Self . TableRef ^ [ LLastStateNo ] . Terms 
                     , WStateRow . Terms 
                     ) 
            THEN 
              LLastStateNo := LStateNo 
            END (* IF *) 

         (* Suche Defaultzustand *) 
         ; LDefStateNo := LLastStateNo 
         ; LComparingBackwards := TRUE 
         ; LOOP 
            (* kein Default zu sich selbst *) 
             IF LDefStateNo = LStateNo 
             THEN 
               LDefStateNo := Self . TableRef ^ [ LDefStateNo ] . Succ 
             ; LComparingBackwards := FALSE 
             END (* IF *) 

           (* Listenende erreicht *) 
           ; IF LDefStateNo = LRTable . StateNoNull THEN EXIT END (* IF *) 

           (* keine Aussicht auf besseren Defaultzustand *) 
           ; IF IntSets . Card ( Self . TableRef ^ [ LDefStateNo ] . Terms ) 
                < WStateRow . Saving 
             THEN 
               EXIT 
             END (* IF *) 
           ; LDefSaving := ComputeSaving ( Self , LStateNo , LDefStateNo ) 
           ; IF LDefSaving > 0 AND LDefSaving >= WStateRow . Saving 
             THEN 
               WStateRow . DefaultState := LDefStateNo 
             ; WStateRow . Saving := LDefSaving 
             ; WStateRow . ToTest := LComparingBackwards 
             END (* IF *) 
           ; LDefStateNo := Self . TableRef ^ [ LDefStateNo ] . Succ 
           END (* LOOP *) 
         END (* WITH *) 
      ; LStateNo := Self . TableRef ^ [ LStateNo ] . Succ 
      END (* WHILE *) 
    END ComputeBestDefaults 

; PROCEDURE EliminateCycles ( Self : T ) 

  = VAR LStateNo : LRTable . StateNoTyp 
  ; VAR LWorstStateNo : LRTable . StateNoTyp 
  ; VAR LWorstSaving : PortTypes . Card16Typ 
  ; VAR LCurrStateNo : LRTable . StateNoTyp 
  ; VAR LFirstStateNo : LRTable . StateNoTyp 
  ; VAR LDefStateNo : LRTable . StateNoTyp 
  ; VAR LDefSaving : PortTypes . Card16Typ 

  ; BEGIN (* EliminateCycles *) 
    (* Pruefe alle Zustaende *) 
      LStateNo := Self . TableRef ^ [ LRTable . StateNoNull ] . Succ 
    ; WHILE LStateNo # LRTable . StateNoNull 
      DO IF Self . TableRef ^ [ LStateNo ] . ToTest 
         THEN (* Zustand noch nicht geprueft *) 
           WITH WStateRow = Self . TableRef ^ [ LStateNo ] 
           DO WStateRow . ToTest := FALSE 

            (* Pruefe ob Zyklus vorhanden *) 
           ; WStateRow . InTest := TRUE 
           ; LCurrStateNo := WStateRow . DefaultState 
           ; WHILE LCurrStateNo # LRTable . StateNoNull 
                   AND NOT Self . TableRef ^ [ LCurrStateNo ] . InTest 
             DO Self . TableRef ^ [ LCurrStateNo ] . InTest := TRUE 
             ; LCurrStateNo 
                 := Self . TableRef ^ [ LCurrStateNo ] . DefaultState 
             END (* WHILE *) 
           ; LFirstStateNo := LCurrStateNo 

            (* Flags zuruecksetzen *) 
           ; WStateRow . InTest := FALSE 
           ; LCurrStateNo := WStateRow . DefaultState 
           END (* WITH *) 

         ; WHILE LCurrStateNo # LRTable . StateNoNull 
                 AND Self . TableRef ^ [ LCurrStateNo ] . InTest 
           DO Self . TableRef ^ [ LCurrStateNo ] . InTest := FALSE 
           ; LCurrStateNo 
               := Self . TableRef ^ [ LCurrStateNo ] . DefaultState 
           END (* WHILE *) 

         (* Falls Zyklus erkannt diesen aufbrechen *) 
         ; IF LFirstStateNo = LRTable . StateNoNull 
           THEN (* kein Zyklus - Zustand fortschalten *) 
             LStateNo := Self . TableRef ^ [ LStateNo ] . Succ 
           ELSE 
             LWorstStateNo := LFirstStateNo 
           ; LWorstSaving := Self . TableRef ^ [ LWorstStateNo ] . Saving 
           ; LCurrStateNo 
               := Self . TableRef ^ [ LFirstStateNo ] . DefaultState 
           ; WHILE LCurrStateNo # LFirstStateNo 
             DO IF Self . TableRef ^ [ LCurrStateNo ] . Saving < LWorstSaving 
                THEN 
                  LWorstStateNo := LCurrStateNo 
                ; LWorstSaving 
                    := Self . TableRef ^ [ LWorstStateNo ] . Saving 
                END (* IF *) 
             ; LCurrStateNo 
                 := Self . TableRef ^ [ LCurrStateNo ] . DefaultState 
             END (* WHILE *) 

           (* Anfang fuer neu Defaultsuche festhalten *) 
           ; LDefStateNo 
               := Self . TableRef ^ [ LWorstStateNo ] . DefaultState 
           ; LDefStateNo := Self . TableRef ^ [ LDefStateNo ] . Succ 
           ; WITH WWorstRow = Self . TableRef ^ [ LWorstStateNo ] 
             DO WWorstRow . DefaultState := LRTable . StateNoNull 
             ; WWorstRow . Saving := 0 

             (* Neuen Defaultzustand berechnen *) 
             ; LOOP (* kein Default zu sich selbst *) 
                 IF LDefStateNo = LWorstStateNo 
                 THEN 
                   LDefStateNo := Self . TableRef ^ [ LDefStateNo ] . Succ 
                 END (* IF *) 

               (* Listenende erreicht *) 
               ; IF LDefStateNo = LRTable . StateNoNull 
                 THEN 
                   EXIT 
                 END (* IF *) 

               (* keine Aussicht auf besseren Defaultzustand *) 
               ; IF IntSets . Card 
                      ( Self . TableRef ^ [ LDefStateNo ] . Terms ) 
                    < WWorstRow . Saving 
                 THEN 
                   EXIT 
                 END (* IF *) 
               ; LDefSaving 
                   := ComputeSaving ( Self , LWorstStateNo , LDefStateNo ) 
               ; IF LDefSaving > 0 AND LDefSaving >= WWorstRow . Saving 
                 THEN 
                   WWorstRow . DefaultState := LDefStateNo 
                 ; WWorstRow . Saving := LDefSaving 
                 END (* IF *) 
               ; LDefStateNo := Self . TableRef ^ [ LDefStateNo ] . Succ 
               END (* LOOP *) 
             ; IF WWorstRow . DefaultState # LRTable . StateNoNull 
               THEN (* im naechsten Durchlauf noch einmal auf Zyklus pruefen *) 
                 Self . TableRef ^ [ LStateNo ] . ToTest := TRUE 
               END (* IF *) 
             END (* WITH  IF *) 
           END (* IF  WITH *) 
         ELSE 
           LStateNo := Self . TableRef ^ [ LStateNo ] . Succ 
         END (* IF *) 
      END (* WHILE *) 
    END EliminateCycles 

(* VISIBLE: *) 
; PROCEDURE ComputeDefaults ( Self : T ) 
  (* Berechnung der variablen Defaultzustaende und entfernen, der dadurch 
     in der Tabelle (Datenstruktur) ueberfluessigen Eintraege *) 

  = BEGIN (* ComputeDefaults *) 
      IF NOT NoDefault 
      THEN 
        ComputeBestDefaults ( Self ) 
      ; EliminateCycles ( Self ) 
      END (* IF *) 
    END ComputeDefaults 

(* VISIBLE: *) 
; PROCEDURE ConstructDefaultedRow 
    ( Self : T 
    ; ReadStateCode : LRTable . StateNoTyp 
    ; VAR TableLine : LRTable . StateNoArrayRefTyp 
      (* ^Preallocated by caller, but filled in entirely by this procedure. *) 
    ; VAR DefaultStateNo : LRTable . StateNoTyp 
    ) 
  (* Auslesen der durch 'ReadState' bezeichneten Zeile aus der Datenstruktur *)

  = VAR LTrans : TransitionRefTyp 

  ; BEGIN (* ConstructDefaultedRow *) 
      FOR RI := 0 TO NUMBER ( TableLine ^ ) - 1 
      DO TableLine ^ [ RI ] := LRTable . StateNoNull 
      END (* FOR *) 
    ; WITH WReadStateRow = Self . TableRef ^ [ ReadStateCode ] 
      DO DefaultStateNo := WReadStateRow . DefaultState 
      ; LTrans := WReadStateRow . Transitions 
      ; WHILE LTrans # NIL 
        DO TableLine [ LTrans ^ . Tok - Self . Gram . FirstTerminal ] 
             := LTrans ^ . StateNo 
        ; LTrans := LTrans ^ . Link 
        END (* WHILE *) 
      END (* WITH *) 

    (* Werte Default aus *) 
    ; WITH WDefault = Self . TableRef ^ [ DefaultStateNo ] 
      DO LTrans := WDefault . Transitions 
      ; WHILE LTrans # NIL 
              AND LTrans ^ . Tok <= Self . Gram . LastAstNonterminal 
        DO WITH WStateNo 
                =  TableLine [ LTrans ^ . Tok - Self . Gram . FirstTerminal ] 
           DO IF WStateNo = LTrans ^ . StateNo 
             THEN WStateNo := LRTable . StateNoNull 
             END (* IF *) 
           END (* WITH *) 
        ; LTrans := LTrans ^ . Link 
        END (* WHILE *) 
      END (* WITH *) 
    END ConstructDefaultedRow 

(* CHECK: This is uncalled. Eliminate it or call it? *) 
; <* UNUSED *> PROCEDURE SortTStates ( Self : T ) 

  = VAR LIsSorted : BOOLEAN 
  ; VAR LStateNo : LRTable . StateNoTyp 
  ; VAR LKey , LLastKey : PortTypes . Card32Typ 

  ; BEGIN (* SortTStates *) 
      Self . TSorting 
        := NEW 
             ( LRTable . StateNoArrayRefTyp 
             , Self . Gram . FirstReadRedAction 
             ) 
    ; FOR RStateNo := 0 TO NUMBER ( Self . TSorting ^ ) - 1 
      DO Self . TSorting ^ [ RStateNo ] := RStateNo 
      END (* FOR *) 
    ; LOOP 
        LIsSorted := TRUE 
      ; LStateNo := Self . TSorting ^ [ LRTable . StateNoNull ] 
      ; LLastKey 
          := IntSets . Card ( Self . TableRef ^ [ LStateNo ] . Terms ) 
             - Self . TableRef ^ [ LStateNo ] . Saving 
      ; FOR I := 1 TO Self . Gram . FirstReadRedAction - 1 
        DO LStateNo := Self . TSorting ^ [ I ] 
        ; LKey 
            := IntSets . Card ( Self . TableRef ^ [ LStateNo ] . Terms ) 
               - Self . TableRef ^ [ LStateNo ] . Saving 
(* TODO: WITH the above, 4 places. *) 
        ; IF LLastKey < LKey 
          THEN 
            LIsSorted := FALSE 
          ; Self . TSorting ^ [ I ] := Self . TSorting ^ [ I - 1 ] 
          ; Self . TSorting ^ [ I - 1 ] := LStateNo 
          ELSE 
            LLastKey := LKey 
          END (* IF *) 
        END (* FOR *) 
      ; IF LIsSorted THEN EXIT END (* IF *) 
      ; LIsSorted := TRUE 
      ; LStateNo 
          := Self . TSorting ^ [ Self . Gram . FirstReadRedAction - 1 ] 
      ; LLastKey 
          := IntSets . Card ( Self . TableRef ^ [ LStateNo ] . Terms ) 
             - Self . TableRef ^ [ LStateNo ] . Saving 
      ; FOR I := Self . Gram . FirstReadRedAction - 2 TO 0 BY - 1 
        DO LStateNo := Self . TSorting ^ [ I ] 
        ; LKey 
            := IntSets . Card ( Self . TableRef ^ [ LStateNo ] . Terms ) 
               - Self . TableRef ^ [ LStateNo ] . Saving 
        ; IF LLastKey > LKey 
          THEN 
            LIsSorted := FALSE 
          ; Self . TSorting ^ [ I ] := Self . TSorting ^ [ I + 1 ] 
          ; Self . TSorting ^ [ I + 1 ] := LStateNo 
          ELSE 
            LLastKey := LKey 
          END (* IF *) 
        END (* FOR *) 
      ; IF LIsSorted THEN EXIT END (* IF *) 
      END (* LOOP *) 
    END SortTStates 

(* CHECK: This is uncalled. Eliminate it or call it? *) 
; <* UNUSED *> PROCEDURE SortNStates ( Self : T ) 

  = VAR LIsSorted : BOOLEAN 
  ; VAR LStateNo : LRTable . StateNoTyp 
  ; VAR LKey , LLastKey : PortTypes . Card32Typ 

  ; BEGIN (* SortNStates *) 
      Self . NSorting 
        := NEW 
             ( LRTable . StateNoArrayRefTyp 
             , Self . Gram . FirstReadRedAction 
             ) 
    ; FOR RStateNo := 0 TO NUMBER ( Self . NSorting ^ ) - 1 
      DO Self . NSorting ^ [ RStateNo ] := RStateNo 
      END (* FOR *) 
    ; LOOP 
        LIsSorted := TRUE 
      ; LStateNo := Self . NSorting ^ [ LRTable . StateNoNull ] 
      ; LLastKey := Self . TableRef ^ [ LStateNo ] . NontermCount 
      ; FOR I := 1 TO Self . Gram . FirstReadRedAction - 1 
        DO LStateNo := Self . NSorting ^ [ I ] 
        ; LKey := Self . TableRef ^ [ LStateNo ] . NontermCount 
        ; IF LLastKey < LKey 
          THEN 
            LIsSorted := FALSE 
          ; Self . NSorting ^ [ I ] := Self . NSorting ^ [ I - 1 ] 
          ; Self . NSorting ^ [ I - 1 ] := LStateNo 
          ELSE 
            LLastKey := LKey 
          END (* IF *) 
        END (* FOR *) 
      ; IF LIsSorted THEN EXIT END (* IF *) 
      ; LIsSorted := TRUE 
      ; LStateNo 
          := Self . NSorting ^ [ Self . Gram . FirstReadRedAction - 1 ] 
      ; LLastKey := Self . TableRef ^ [ LStateNo ] . NontermCount 
      ; FOR I := Self . Gram . FirstReadRedAction - 2 TO 0 BY - 1 
        DO LStateNo := Self . NSorting ^ [ I ] 
        ; LKey := Self . TableRef ^ [ LStateNo ] . NontermCount 
        ; IF LLastKey > LKey 
          THEN 
            LIsSorted := FALSE 
          ; Self . NSorting ^ [ I ] := Self . NSorting ^ [ I + 1 ] 
          ; Self . NSorting ^ [ I + 1 ] := LStateNo 
          ELSE 
            LLastKey := LKey 
          END (* IF *) 
        END (* FOR *) 
      ; IF LIsSorted THEN EXIT END (* IF *) 
      END (* LOOP *) 
    END SortNStates 

(* VISIBLE: *) 
; PROCEDURE GetTSortState 
    ( Self : T ; StateNo : LRTable . StateNoTyp ) : LRTable . StateNoTyp 
  (* Fortschalten des Zustandes nach festgelegter Strategie *) 
  (* Strategie : Zuerst Zustaende mit vielen Eintraegen *) 

  = BEGIN (* GetTSortState *) 
      IF Self . TSorting = NIL 
      THEN 
        RETURN StateNo 
      ELSE 
        RETURN Self . TSorting ^ [ StateNo ] 
      END (* IF *) 
    END GetTSortState 

(* VISIBLE: *) 
; PROCEDURE GetNSortState 
    ( Self : T ; StateNo : LRTable . StateNoTyp ) : LRTable . StateNoTyp 
  (* Fortschalten des Zustandes nach festgelegter Strategie *) 
  (* Strategie : Zuerst Zustaende mit vielen Eintraegen *) 

  = BEGIN (* GetNSortState *) 
      IF Self . NSorting = NIL 
      THEN 
        RETURN StateNo 
      ELSE 
        RETURN Self . NSorting ^ [ StateNo ] 
      END (* IF *) 
    END GetNSortState 

; BEGIN (* Default *) 
  END Default 
. 

