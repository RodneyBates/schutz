
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LexTable 

(* A fast two-way mapping between integers and strings.  For string-to-int,
   uses a lexical scanner whose transition table is prebuilt by 
   BuildLexMachine.  For the reverse, uses a directly subscripted
   array.
*)    

; IMPORT Text 

; FROM Assertions IMPORT Assert , AssertionFailure 
; IMPORT LexTableRep 
; IMPORT MessageCodes 
; IMPORT PortTypes 

; TYPE AFT = MessageCodes . T 

(* VISIBLE: *) 
; PROCEDURE ToText ( Table : T ; Value : ValueTyp ) : TEXT 
  (* NIL if Value not in Table. *) 

  = VAR LSs : PortTypes . Int32Typ 

  ; BEGIN 
      IF Table = NIL OR Table ^ . NamesRef = NIL  
      THEN RETURN NIL 
      ELSE 
        LSs := Value - Table ^ . MinValue 
      ; IF LSs < 0 OR Value > Table ^ . MaxValue 
        THEN RETURN NIL 
        ELSE
          RETURN Table ^ . NamesRef ^ [ LSs ] 
        END (* IF *) 
      END (* IF *) 
    END ToText 

(* VISIBLE: *) 
; PROCEDURE ValueFromChars ( Table : T ; READONLY Name : ARRAY OF CHAR ) 
  : ValueTyp 
  (* ValueNull if Name is not in Table. *) 

  = VAR LNameSs : INTEGER 
  ; VAR LNameLast : INTEGER 
  ; VAR LStateNo : LexTableRep . StateNoTyp 
  ; VAR LChar : CHAR 
  ; VAR LTransition : LexTableRep . TransitionTyp 

  ; BEGIN 
      IF Table = NIL 
         OR Table ^ . StatesRef = NIL 
         OR NUMBER ( Table ^ . StatesRef ^ ) = 0  
      THEN RETURN ValueNull 
      ELSE 
        LNameSs := 0 
      ; LNameLast := LAST ( Name ) 
      ; LStateNo := 0 
      ; LOOP 
          IF LNameSs > LNameLast 
          THEN LChar := LexTableRep . NullChar 
          ELSE 
            LChar := Name [ LNameSs ] 
          END (* IF *) 
        ; WITH WState = Table ^ . StatesRef ^ [ LStateNo ] 
          DO 
            IF LChar < WState . Min OR LChar > WState . Max 
            THEN RETURN ValueNull 
            ELSE 
              LTransition 
                := Table ^ . SpaceRef [ ORD ( LChar ) + WState . SpaceBias ] 
            ; IF LTransition < 0 
              THEN RETURN LTransition - LexTableRep . FirstRealValue 
              ELSIF LTransition = LexTableRep . NoTransition 
              THEN RETURN ValueNull 
              ELSE 
                LStateNo := LTransition 
              ; INC ( LNameSs ) 
              END (* IF *) 
            END (* IF *) 
          END (* WITH *) 
        END (* LOOP *) 
      END (* IF *) 
    END ValueFromChars  

; CONST MaxStringLength = 512 

(* VISIBLE: *) 
; PROCEDURE ValueFromText ( Table : T ; Name : TEXT ) : ValueTyp 
  RAISES { AssertionFailure } 
  (* ValueNull if Name is not in Table. *) 

  = VAR LChars : ARRAY [ 0 .. MaxStringLength - 1 ] OF CHAR 
  ; VAR LLength : INTEGER 

  ; BEGIN
      Assert 
        ( Text . Length ( Name ) <= MaxStringLength 
        , AFT . A_ValueFromText_StringTooLong 
        ) 
    ; Text . SetChars ( LChars , Name ) 
    ; LLength := MIN ( Text . Length ( Name ) , MaxStringLength ) 
    ; RETURN ValueFromChars ( Table , SUBARRAY ( LChars , 0 , LLength ) ) 
    END ValueFromText 

; BEGIN 
  END LexTable 
. 
