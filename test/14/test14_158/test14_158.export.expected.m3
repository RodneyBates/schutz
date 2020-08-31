(* Rodney M. Bates.  CS 697V, Sp2006, Example Format module. *)

MODULE test14_158

; IMPORT Text

; CONST MaxDigits = 22
  (* Big enough for 2**63 - 1, in case INTEGER is 64 bits. *)

; PROCEDURE IntToText ( Cents : INTEGER ) : TEXT
  (* Convert Cents to a character string with floating dollar sign, optional
     minus sign, at least three digits total, and a decimal point with two
     digits to its right.
  *)

  = VAR LResult := ARRAY [ 1 .. MaxDigits ] OF CHAR { ' ' , .. }
  ; CONST Last = LAST ( LResult )
  ; VAR LRemainder : CARDINAL
  ; VAR LSs : INTEGER := Last (* Next unfilled element, moving leftward. *)
    AR LDigit : INTEGER

  ; BEGIN
      IF Cents = FIRST ( INTEGER )
      THEN
        RETURN "$-21474836.48"
      ELSE
        LRemainder := ABS ( Cents )
      ; LOOP
          IF LRemainder = 0 AND LSs <= Last - 4
          THEN
            EXIT
          ELSE
            IF LSs = Last - 2
            THEN
              LResult [ LSs ] := '.'
            ; DEC ( LSs )
            END (* IF *)
          ; LDigit := LRemainder MOD 10
          ; LRemainder := LRemainder DIV 10
          ; LResult [ LSs ] := VAL ( LDigit + ORD ( '0' ) , CHAR )
          ; DEC ( LSs )
          END (* IF*)
        END (* LOOP *)
      ; IF Cents < 0
        THEN
          LResult [ LSs ] := '-'
        ; DEC ( LSs )
        END (* IF *)
      ; LResult [ LSs ] := '$'
      ; DEC ( LSs )
      ; RETURN Text . FromChars ( SUBARRAY ( LResult , LSs , Last - LSs ) )
      END (* IF *)
    END IntToText

; BEGIN
  END test14_158
.

