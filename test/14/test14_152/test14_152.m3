(* Rodney M. Bates.  CS 697V, Sp2006, Example Format module. *)

MODULE test14_152

; IMPORT Text

; CONST MaxDigits = 19 
  (* Big enough for 2**63 - 1, in case CARDINAL is 64 bits. *) 

; PROCEDURE IntToText ( IntVal : CARDINAL ) : TEXT
  (* Convert IntVal to a character string of exactly the needed length. *) 

  = VAR LResult := ARRAY [ 1 .. MaxDigits ] OF CHAR { ' ' , .. } 
  ; CONST Last = LAST ( LResult ) 
  ; VAR LRemainder : INTEGER := IntVal 
  ; VAR LSs : INTEGER := Last (* Next unfilled element, moving leftward. *) 
  ; VAR LDigit : INTEGER

  ; BEGIN
      IF IntVal = 0
      THEN RETURN "0" 
      ELSE
        LOOP
          IF LRemainder = 0
          THEN EXIT
          ELSE
            LDigit := LRemainder MOD 10
          ; LRemainder := LRemainder DIV 10
          ; LResult [ LSs ] := VAL ( LDigit + ORD ( '0' ) , CHAR )
          ; DEC ( LSs )
          END (* IF*)
        END (* LOOP *)
      ; RETURN 
          Text . FromChars 
            ( SUBARRAY ( LResult , LSs , Last - LSs ) )
      END (* IF *)
    END IntToText

; BEGIN
  END test14_152
.

