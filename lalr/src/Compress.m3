
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001 .. 2007, from Cocktail, lalr, Compress.mi, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Compress parse table *) 

MODULE Compress 

; IMPORT LALRTypes 
; IMPORT LRTable 
; IMPORT LbeStd 

; CONST InitTableMax = 1500 
; CONST InitNTableMax = 500 

(* VISIBLE: *) 
; PROCEDURE InitCompressTable ( Gram : LRTable . GrammarTyp ) 

  = BEGIN (* InitCompressTable *) 
      Gram . BaseRef 
        := NEW ( LRTable . BaseRefTyp , Gram . FirstReadRedAction ) 
    ; Gram . DefaultRef 
        := NEW ( LRTable . StateNoArrayRefTyp , Gram . FirstReadRedAction ) 
    ; FOR RStateNo := 0 TO NUMBER ( Gram . BaseRef ^ ) - 1 
      DO Gram . BaseRef ^ [ RStateNo ] := 0 
      ; Gram . DefaultRef ^ [ RStateNo ] := LRTable . StateNoNull 
      END (* FOR *) 
    ; Gram . ActionTableRef 
        := NEW 
             ( LRTable . ActionTableRefTyp 
             , Gram . LastAstNonterminal 
               - Gram . FirstTerminal 
               + 1 
               + InitTableMax 
             ) 
    ; FOR RBaseNo := 0 TO NUMBER ( Gram . ActionTableRef ^ ) - 1 
      DO Gram . ActionTableRef ^ [ RBaseNo ] := LRTable . ActionTableElemNull 
      END (* FOR *) 
    ; Gram . Filling := 0 
    END InitCompressTable 

(* VISIBLE: *) 
; PROCEDURE CompressTableLine 
    ( Gram : LRTable . GrammarTyp 
    ; State : LRTable . StateNoTyp 
    ; DefaultState : LRTable . StateNoTyp 
    ; VAR TableLine : LRTable . StateNoArrayRefTyp 
    ) 
(* Terminale komprimieren *) 

  = VAR LBaseNo : LRTable . ParseActionTyp 
  ; VAR LSymbol : LbeStd . TokTyp 
  ; VAR LOldActionTableCt : CARDINAL  
  ; VAR LNextTokArrayRef : LRTable . TokArrayRefTyp 
  ; VAR LStartSymbol , LStopSymbol , LPrevSymbol : LbeStd . TokTyp 
  ; VAR LNewActionTableRef : LRTable . ActionTableRefTyp 
  ; VAR LAction : LRTable . ParseActionTyp 
  ; VAR LTermTransitionFound : BOOLEAN 
  ; VAR LBaseNoWorks : BOOLEAN 

  ; BEGIN (* CompressTableLine *) 
      Gram . DefaultRef ^ [ State ] := DefaultState 

    (* Turn the terminal and Ast nonterminal portion of row 
       Table [State, ...] into a subscript-linked list *) 
    ; LNextTokArrayRef 
        := NEW 
             ( LRTable . TokArrayRefTyp 
             , Gram . LastAstNonterminal - Gram . FirstTerminal + 1 
             ) 
    ; LSymbol := Gram . FirstTerminal 
    ; LTermTransitionFound := FALSE 
    ; LPrevSymbol := LbeStd . Tok__Null 
    ; LOOP (* To find the first token with an action. *) 
        IF LSymbol > Gram . LastAstNonterminal 
        THEN 
          EXIT 
        ELSE 
          IF TableLine ^ [ LSymbol - Gram . FirstTerminal ] 
             # LRTable . StateNoNull 
          THEN 
            LStartSymbol := LSymbol 
          ; LPrevSymbol := LSymbol 
          ; LTermTransitionFound := TRUE 
          ; EXIT 
          ELSE 
            INC ( LSymbol ) 
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    ; INC ( LSymbol ) 
    ; LOOP (* Thru the rest of the tokens. *) 
        IF LSymbol > Gram . LastAstNonterminal 
        THEN 
          EXIT 
        ELSE 
          IF TableLine ^ [ LSymbol - Gram . FirstTerminal ] 
             # LRTable . StateNoNull 
          THEN 
            LNextTokArrayRef ^ [ LPrevSymbol - Gram . FirstTerminal ] 
              := LSymbol 
          ; LPrevSymbol := LSymbol 
          END (* IF *) 
        ; INC ( LSymbol ) 
        END (* IF *) 
      END (* LOOP *) 
    ; LStopSymbol := LPrevSymbol 

    (* search for a usable base LBaseNo *) 
    ; LBaseNo := 0 
    ; IF LTermTransitionFound 
      THEN 
        LOOP (* Thru base candidates *) 
          LBaseNoWorks := TRUE 
        ; LSymbol := LStartSymbol 
        ; LOOP (* Thru tokens with transitions, for this base. *) 
            IF Gram . ActionTableRef ^ 
                 [ LBaseNo + LSymbol - Gram . FirstTerminal ] 
               . Check 
               # LRTable . StateNoNull 
            THEN 
              LBaseNoWorks := FALSE 
            ; EXIT 
            ELSIF LSymbol = LStopSymbol 
            THEN 
              EXIT 
            ELSE 
              LSymbol 
                := LNextTokArrayRef ^ [ LSymbol - Gram . FirstTerminal ] 
            END (* IF *) 
          END (* LOOP *) 
        ; IF LBaseNoWorks 
          THEN 
            EXIT 
          ELSE 
            INC ( LBaseNo ) 
          ; LOldActionTableCt := NUMBER ( Gram . ActionTableRef ^ ) 
          ; IF LBaseNo + Gram . LastAstNonterminal - Gram . FirstTerminal 
               >= LOldActionTableCt 
            THEN (* Expand action table. *) 
              LNewActionTableRef 
                := NEW 
                     ( LRTable . ActionTableRefTyp 
                     , LBaseNo 
                       + Gram . LastAstNonterminal 
                       - Gram . FirstTerminal 
                       + 1 
                       + InitTableMax 
                     ) 
            ; SUBARRAY ( LNewActionTableRef ^ , 0 , LOldActionTableCt ) 
                := Gram . ActionTableRef ^ 
            ; FOR RI := LOldActionTableCt 
                    TO NUMBER ( LNewActionTableRef ^ ) - 1 
              DO LNewActionTableRef ^ [ RI ] := LRTable . ActionTableElemNull 
              END (* FOR *) 
            ; Gram . ActionTableRef := LNewActionTableRef 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      END (* IF *) 
    ; Gram . BaseRef ^ [ State ] := LBaseNo 
    ; FOR LSymbol := Gram . FirstTerminal TO Gram . LastAstNonterminal 
      DO LAction := TableLine ^ [ LSymbol - Gram . FirstTerminal ] 
      ; IF LAction # LRTable . StateNoNull 
        THEN 
          Gram . ActionTableRef ^ [ LBaseNo + LSymbol - Gram . FirstTerminal ] 
            := LRTable . ActionTableElemTyp 
                 { Check := State , Next := LAction } 
        ; INC ( Gram . Filling ) 
        END (* IF *) 
      END (* FOR *) 
    ; LNextTokArrayRef := NIL 
    END CompressTableLine 

(* VISIBLE: *) 
; PROCEDURE InitCompressNTable ( Gram : LRTable . GrammarTyp ) 

  = BEGIN (* InitCompressNTable *) 
      Gram . NBaseRef 
        := NEW ( LRTable . BaseRefTyp , Gram . FirstReadRedAction ) 
    ; FOR RStateNo := 0 TO NUMBER ( Gram . NBaseRef ^ ) - 1 
      DO Gram . NBaseRef ^ [ RStateNo ] := 0 
      END (* FOR *) 
    ; Gram . NNextRef 
        := NEW 
             ( LRTable . ParseActionArrayRefTyp 
             , Gram . LastNonterminal 
               - Gram . FirstNonterminal 
               + 1 
               + InitNTableMax 
             ) 
    ; FOR RBaseNo := 0 TO NUMBER ( Gram . NNextRef ^ ) - 1 
      DO Gram . NNextRef ^ [ RBaseNo ] := LRTable . StateNoNull 
      END (* FOR *) 
    ; Gram . NFilling := 0 
    END InitCompressNTable 

(* VISIBLE: *) 
; PROCEDURE CompressNTableLine 
    ( Gram : LRTable . GrammarTyp 
    ; State : LRTable . StateNoTyp 
    ; VAR TableLine : LRTable . StateNoArrayRefTyp 
    ) 
  (* Nichtterminale komprimieren *) 

  = VAR LBaseNo : LRTable . ParseActionTyp 
  ; VAR LSymbol : LbeStd . TokTyp 
  ; VAR LOldNTableCt : LRTable . ParseActionTyp 
  ; VAR LNextTokArrayRef : LRTable . TokArrayRefTyp 
  ; VAR LStartSymbol , LStopSymbol , LPrevSymbol : LbeStd . TokTyp 
  ; VAR LNewNNextRef : LRTable . ParseActionArrayRefTyp 
  ; VAR LAction : LRTable . ParseActionTyp 
  ; VAR LNontermTransitionFound : BOOLEAN 
  ; VAR LBaseNoWorks : BOOLEAN 

  ; BEGIN (* CompressNTableLine *) 
    (* Turn the nonterminal portion of row Table [State, ...] 
       into a subscript-linked list *) 
      LNextTokArrayRef 
        := NEW 
             ( LRTable . TokArrayRefTyp 
             , Gram . LastNonterminal - Gram . FirstNonterminal + 1 
             ) 
    ; LSymbol := Gram . FirstNonterminal 
    ; LNontermTransitionFound := FALSE 
    ; LPrevSymbol := LbeStd . Tok__Null 
    ; LOOP (* To find the first token with an action. *) 
        IF LSymbol > Gram . LastNonterminal 
        THEN 
          EXIT 
        ELSIF TableLine ^ [ LSymbol - Gram . FirstTerminal ] 
              # LRTable . StateNoNull 
        THEN 
          LStartSymbol := LSymbol 
        ; LPrevSymbol := LSymbol 
        ; LNontermTransitionFound := TRUE 
        ; EXIT 
        ELSE 
          INC ( LSymbol ) 
        END (* IF *) 
      END (* LOOP *) 
    ; INC ( LSymbol ) 
    ; LOOP (* Thru the rest of the tokens. *) 
        IF LSymbol > Gram . LastNonterminal 
        THEN 
          EXIT 
        ELSE 
          IF TableLine ^ [ LSymbol - Gram . FirstTerminal ] 
             # LRTable . StateNoNull 
          THEN 
            LNextTokArrayRef ^ [ LPrevSymbol - Gram . FirstNonterminal ] 
              := LSymbol 
          ; LPrevSymbol := LSymbol 
          END (* IF *) 
        ; INC ( LSymbol ) 
        END (* IF *) 
      END (* LOOP *) 
    ; LStopSymbol := LPrevSymbol 

    (* search for a usable base LBaseNo *) 
    ; LBaseNo := 0 
    ; IF LNontermTransitionFound 
      THEN 
        LOOP (* Thru base candidates *) 
          LBaseNoWorks := TRUE 
        ; LSymbol := LStartSymbol 
        ; LOOP (* Thru tokens with transitions, for this base. *) 
            LAction 
              := Gram . NNextRef ^ 
                   [ LBaseNo + LSymbol - Gram . FirstNonterminal ] 
          ; IF LAction # LRTable . StateNoNull 
               AND LAction # TableLine ^ [ LSymbol - Gram . FirstTerminal ] 
            THEN 
              LBaseNoWorks := FALSE 
            ; EXIT 
            ELSIF LSymbol = LStopSymbol 
            THEN 
              EXIT 
            ELSE 
              LSymbol 
                := LNextTokArrayRef ^ [ LSymbol - Gram . FirstNonterminal ] 
            END (* IF *) 
          END (* LOOP *) 
        ; IF LBaseNoWorks 
          THEN 
            EXIT 
          ELSE 
            INC ( LBaseNo ) 
          ; LOldNTableCt := NUMBER ( Gram . NNextRef ^ ) 
          ; IF LBaseNo + Gram . LastNonterminal - Gram . FirstNonterminal 
               >= LOldNTableCt 
            THEN (* Expand action table. *) 
              LNewNNextRef 
                := NEW 
                     ( LRTable . ParseActionArrayRefTyp 
                     , LBaseNo 
                       + Gram . LastNonterminal 
                       - Gram . FirstNonterminal 
                       + 1 
                       + InitNTableMax 
                     ) 
            ; SUBARRAY ( LNewNNextRef ^ , 0 , LOldNTableCt ) 
                := Gram . NNextRef ^ 
            ; FOR RI := LOldNTableCt TO NUMBER ( LNewNNextRef ^ ) - 1 
              DO LNewNNextRef ^ [ RI ] := LRTable . StateNoNull 
              END (* FOR *) 
            ; Gram . NNextRef := LNewNNextRef 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      ELSE
(* CHECK:  As of 2007-6-20, LALRTypes.Action treats zero base no as an error.
           Is this right?  It came up in a state where there are no nonterm
           shifts.  
*) 
      END (* IF *) 
    ; Gram . NBaseRef ^ [ State ] := LBaseNo 
    ; FOR LSymbol := Gram . FirstNonterminal TO Gram . LastNonterminal 
      DO LAction := TableLine ^ [ LSymbol - Gram . FirstTerminal ] 
      ; IF LAction # LRTable . StateNoNull 
        THEN 
          Gram . NNextRef ^ [ LBaseNo + LSymbol - Gram . FirstNonterminal ] 
            := LAction 
        ; INC ( Gram . NFilling ) 
        END (* IF *) 
      END (* FOR *) 
    ; LNextTokArrayRef := NIL 
    END CompressNTableLine 

; BEGIN (* Compress *) 
  END Compress 
. 

