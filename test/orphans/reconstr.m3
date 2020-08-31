(* Rodney M. Bates.  CS 697V, Sp2006, Example Format module. *)
(* Christopher Janzen
   ctjanzen
   CS697v
   Spring 2006
   Assignment 3
   Format.m3
*)

MODULE Format

; IMPORT Text

; CONST MaxDigits = 22
  (* Big enough for 2**63 - 1, in case CARDINAL is 64 bits. *)

; PROCEDURE IntToText ( IntVal : CARDINAL ) : TEXT
    (* Convert IntVal to a character string of exactly the needed length. *)

  = VAR LResult := ARRAY [ 1 .. MaxDigits ] OF CHAR { ' ' , .. }
  ; CONST Last = LAST ( LResult )
  ; VAR LRemainder : INTEGER := IntVal
  ; VAR LSs : INTEGER
      := Last (* Next unfilled element, moving leftward. *)
  ; VAR LDigit : INTEGER

  ; BEGIN
      IF IntVal = 0
      THEN
        RETURN "0"
      ELSE
        LOOP
          IF LRemainder = 0
          THEN
            EXIT
          ELSE
            LDigit := LRemainder MOD 10
          ; LRemainder := LRemainder DIV 10
          ; LResult [ LSs ] := VAL ( LDigit + ORD ( '0' ) , CHAR )
          ; DEC ( LSs )
          END (* IF*)
        END (* LOOP *)
      ; RETURN Text . FromChars ( SUBARRAY ( LResult , LSs , Last - LSs ) )
      END (* IF *)
    END IntToText

; PROCEDURE IntToDollars ( IntVal : INTEGER ) : TEXT
    (* Convert IntVal to a character string in $-#.## format. *)


  = VAR LResult := ARRAY [ 1 .. MaxDigits ] OF CHAR { ' ' , .. }
  ; CONST Last = LAST ( LResult )
  ; VAR LRemainder : INTEGER := IntVal
  ; VAR LSs
      : INTEGER
      := Last (* Next unfilled element, moving leftward. *)
  ; VAR LDigit : INTEGER
  ; VAR IsNeg : BOOLEAN := FALSE


  ; BEGIN
      IF IntVal < 0
      THEN
        IsNeg := TRUE
      ; END (* IF *)
      IF IntVal = 0
      THEN
        RETURN "$0.00"
      ELSE
        LOOP
          IF LSs = Last - 2
          THEN




          IF LRemainder = 0
          THEN
            EXIT
          ELSE
            LDigit := LRemainder MOD 10
          ; LRemainder := LRemainder DIV 10
          ; LResult [ LSs ] := VAL ( LDigit + ORD ( '0' ) , CHAR )
          ; DEC ( LSs )
          END (* IF*)
        END (* LOOP *)
      ; RETURN Text . FromChars ( SUBARRAY ( LResult , LSs , Last - LSs ) )
      END (* IF *)
    END IntToText
; BEGIN
  END Format
.

