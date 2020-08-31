(* Rodney M. Bates.  CS 697V, Sp2006, Example Format module. *)

MODULE Format

; IMPORT Text

; PROCEDURE ToWordsOnes ( N : [ 0 .. 9 ] ) : TEXT

  = BEGIN
      CASE N
      OF 0 => RETURN ""
      | 1 => RETURN "One"
      | 2 => RETURN "Two"
      | 3 => RETURN "Three"
      | 4 => RETURN "Four"
      | 5 => RETURN "Five"
      | 6 => RETURN "Six"
      | 7 => RETURN "Seven"
      | 8 => RETURN "Eight"
      | 9 => RETURN "Nine"
      END (* CASE *)
    END ToWordsOnes

; PROCEDURE ToWordsTeens ( N : [ 0 .. 9 ] ) : TEXT

  = BEGIN
      CASE N
      OF 0 => RETURN "Ten"
      | 1 => RETURN "Eleven"
      | 2 => RETURN "Twelve"
      | 3 => RETURN "Thirteen"
      | 4 => RETURN "Fourteen"
      | 5 => RETURN "Fifteen"
      | 6 => RETURN "Sixteen"
      | 7 => RETURN "Seventeen"
      | 8 => RETURN "Eighteen"
      | 9 => RETURN "Nineteen"
      END (* CASE *)
    END ToWordsTeens

; PROCEDURE ToWordsTens ( N : [ 2 .. 9 ] ) : TEXT

  = BEGIN
      CASE N
      OF 2 => RETURN "Twenty"
      | 3 => RETURN "Thirty"
      | 4 => RETURN "Forty"
      | 5 => RETURN "Fifty"
      | 6 => RETURN "Sixty"
      | 7 => RETURN "Seventy"
      | 8 => RETURN "Eighty"
      | 9 => RETURN "Ninety"
      END (* CASE *)
    END ToWordsTens

; PROCEDURE ToWords1To999 ( N : [ 1 .. 999 ] ) : TEXT

  = VAR LHundreds : [ 0 .. 9 ]
  ; VAR LRemainder : INTEGER
  ; VAR LTens : [ 0 .. 9 ]
  ; VAR LOnes : [ 0 .. 9 ]
  ; VAR LResult : TEXT := ""

  ; BEGIN
      LHundreds := N DIV 100
    ; LRemainder := N MOD 100
    ; LTens := LRemainder DIV 10
    ; LOnes := LRemainder MOD 10
    ; IF LHundreds > 0
      THEN
        LResult := ToWordsOnes ( LHundreds ) & " Hundred"
      ; IF LTens > 0 OR LOnes > 0
        THEN
          LResult := LResult & " "
        END (* IF *)
      END (* IF *)
    ; CASE LTens
      OF 0 => RETURN LResult & ToWordsOnes ( LOnes )
      | 1 => RETURN LResult & ToWordsTeens ( LOnes )
      ELSE LResult := LResult & ToWordsTens ( LTens )
      ; IF LOnes > 0
        THEN
          LResult := LResult & " " & ToWordsOnes ( LOnes )
        END (* IF *)
      ; RETURN LResult
      END (* IF *)
    END ToWords1To999

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
  ; VAR LSs : INTEGER
      := Last (* Next unfilled element, moving leftward. *)
  ; VAR LDigit : INTEGER

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

; PROCEDURE ToWords ( Value : CARDINAL ) : TEXT

  = VAR LRemainder : INTEGER
  ; VAR LOnes : INTEGER
  ; VAR LThousands : INTEGER
  ; VAR LMillions : INTEGER
  ; VAR LBillions : INTEGER
  ; VAR LResult : TEXT := ""

  ; BEGIN
      IF Value = 0
      THEN
        RETURN "Zero"
      ELSE
        LOnes := Value MOD 1000
      ; LRemainder := Value DIV 1000
      ; LThousands := LRemainder MOD 1000
      ; LRemainder := LRemainder DIV 1000
      ; LMillions := LRemainder MOD 1000
      ; LBillions := LRemainder DIV 1000
      ; IF LBillions > 0
        THEN
          LResult := LResult & ToWords1To999 ( LBillions ) & " Billion"
        END (* IF *)
      ; IF LMillions > 0
        THEN
          IF LBillions > 0
          THEN
            LResult := LResult & " "
          ; LResult := LResult & ToWords1To999 ( LMillions ) & " Million"
          END (* IF *)
        END (* IF *)
      ; IF LThousands > 0
        THEN
          IF LBillions > 0 OR LMillions > 0
          THEN
            LResult := LResult & " "
          ; LResult := LResult & ToWords1To999 ( LThousands ) & " Thousand"
          END (* IF *)
        END (* IF *)
      ; IF LOnes > 0
        THEN
          IF LBillions > 0 OR LMillions > 0 OR LThousands > 0
          THEN
            LResult := LResult & " "
          ; LResult := LResult & ToWords1To999 ( LOnes )
          END (* IF *)
        END (* IF *)
      END
    ; RETURN LResult
    END ToWords

; PROCEDURE ToCheckDollars ( Cents : CARDINAL ) : TEXT

  = VAR LCents : INTEGER
  ; LDollars : INTEGER
  ; LResult : TEXT

  ; BEGIN
      LCents := Cents MOD 100
    ; LDollars := Cents DIV 100
    ; LResult := ToWords ( LDollars )
    ; IF LCents > 0
      THEN
        LResult := LResult & " and " & IntToText ( LCents ) & "/100"
      END (* IF *)
    ; IF LDollars = 1 AND LCents = 0
      THEN
        LResult := LResult & " Dollar"
      ELSE
        LResult := LResult & " Dollars"
      END (* IF *)
    ; RETURN LResult
    END ToCheckDollars

; PROCEDURE ToPounds ( Value : INTEGER ) : TEXT

  = VAR LPence : INTEGER
  ; VAR LShillings : INTEGER
  ; VAR LPounds : INTEGER
  ; VAR LRemainder : INTEGER
  ; VAR LResult : TEXT := ""

  ; BEGIN
      IF Value = 0
      THEN
        RETURN "Zero Pounds"
      ELSE
        LPence := Value MOD 12
      ; LRemainder := Value DIV 12
      ; LShillings := LRemainder MOD 20
      ; LPounds := LRemainder DIV 20
      ; IF LPounds > 0
        THEN
          LResult := LResult & ToWords ( LPounds )
        ; IF LPounds = 1
          THEN
            LResult := LResult & " Pound"
          ELSE
            LResult := LResult & " Pounds"
          END (* IF *)
        END (* IF *)
      ; IF LShillings > 0
        THEN
          IF LPounds > 0
          THEN
            IF LPence > 0
            THEN
              LResult := LResult & ", "
            ELSE
              LResult := LResult & " and "
            END (* IF *)
          END (* IF *)
        ; LResult := LResult & ToWords ( LShillings )
        ; IF LShillings = 1
          THEN
            LResult := LResult & " Shilling"
          ELSE
            LResult := LResult & " Shillings"
          END (* IF *)
        END (* IF *)
      ; IF LShillings > 0
        THEN
          IF LPounds > 0
          THEN
            IF LPence > 0
            THEN
              LResult := LResult & ", "
            ELSE
              LResult := LResult & " and "
            END (* IF *)
          END
        (*

          THEN
            LResult := LResult & " Shilling"
          ELSE
            LResult := LResult & " Shillings"
          END (* IF *)
        END (* IF *)

                                                                                                                                                                                                                                                 END (* IF *)
    END ToPounds

; BEGIN
  END Format
.


