(* Rodney M. Bates.  CS 810, Fall 2004.  Assigment 7, 
   Module for  conversion of integers to words and words to integers. 
*)



(* Bimal Deepak Manukonda
   Student ID: 996-19-5442
   Assignment 7  *)



MODULE WordNumbers
; IMPORT Text
; VAR i : INTEGER
;


PROCEDURE ToWordsOnes ( N : [ 0 .. 9 ] ) : TEXT

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

(* VISIBLE *)
; PROCEDURE ToWords ( N : [ 0 .. 999 ] ) : TEXT

  = BEGIN
      IF N = 0
      THEN
        RETURN "Zero"
      ELSE
        RETURN ToWords1To999 ( N )
      END (* IF *)
    END ToWords
;


(***********************************To Int Function **********************)


(* Function    To    Convert    Words    To    Integers *)

(************************ Procedure HashCode *****************)
(* Increments string past one word and returns the word's hash code.
   Consider the string ended by a blank , tab , or null char.
*)
PROCEDURE HashCode ( String : TEXT ; stindex : INTEGER ) : INTEGER
  = VAR LResult : INTEGER := 0
  ; Lch : CHAR
  ; lindex : INTEGER := 0
  ; Ltemp : INTEGER
  ; LTerm : INTEGER
  ; LSpace : INTEGER
  ; strlen : INTEGER := 100
  ; CONST Multiplier : INTEGER = 23
  ; BEGIN
      IF ( stindex = 1 )
      THEN
        strlen := Text . Length ( String )
      ;
      END
    ;

    WHILE ( TRUE )
      DO
        IF ( stindex = 0 )
        THEN
          Lch := Text . GetChar ( String , lindex )
        ; lindex := lindex + 1
        ;
        ELSE
          Lch := Text . GetChar ( String , i )
        ; i := i + 1
        ;

        END
      ;

      Ltemp := ORD ( Lch )
      ; IF ( lindex
             = Text . Length
               ( String ) OR Lch = ' ' OR Lch = '\t' OR i = strlen
             )
        THEN
          EXIT
        ELSE
          IF ( 'A' <= Lch AND Lch <= 'Z' )
          THEN
            Ltemp := ORD ( Lch ) - ORD ( 'A' ) + ORD ( 'a' )
          ;
          END
        ; LTerm := Ltemp * Multiplier
        ; LSpace := LAST ( INTEGER ) - LResult
        ; IF ( LSpace < LTerm )
          THEN
            LResult := LTerm - LSpace
          ELSE
            LResult := LResult + LTerm
          ;
       (* String:=String+1;*)
          END
        ;

        END
      ;

      END
    ; RETURN ( LResult )
    END HashCode
;


CONST TableSize : INTEGER = 51
;
(*Must be prime.Should be 1.5 times the number of words to be inserted*)
(*******************************Record Declaration **************)

TYPE TableElemTyp = RECORD TeString : TEXT ; TeValue : INTEGER ; END
;

TYPE INDEX = [ 0. .. 50 ]
; TABLE = ARRAY INDEX OF TableElemTyp
; VAR Table : TABLE
;

(*TYPE
     TEREF = REF TableElemTyp;
*)

(**********************Procedure HashInsert ******************)
(* Inserts String into Hash Table
   Considers the String ended by a blank ,tab, or null char.
*)

PROCEDURE HashInsert ( String : TEXT ; Value : INTEGER )
  = VAR LWordStart : TEXT
  ; LHash : INTEGER
  ; LInitialProbe : INTEGER
  ; LProbe : INTEGER
  ; LIncr : INTEGER
  ; teref : TableElemTyp
  ; BEGIN

      LWordStart := String
    ; LHash := HashCode ( String , 0 )
    ; LInitialProbe := LHash MOD TableSize
    ; LProbe := LInitialProbe
    ; LIncr := ( LHash MOD ( TableSize - 1 ) ) + 1
    ;
           (* Must Not Be A Multiple Of TableSize *)

    WHILE ( TRUE )
      DO
        teref := Table [ LProbe ]
      ;

      IF (   teref . TeString = NIL )
        THEN
          (* We Found An UnUsed Element . Fill It *)
          teref . TeString := LWordStart
        ; teref . TeValue := Value
        ;

        EXIT
        ELSE
          LProbe := LProbe + LIncr
        ;                                (* The Table Is Full.
                                           declaring TableSize big 
                                           enough to begin with *)
        END
      ;
      END
    ;
    END HashInsert
;

(***************************** Procedure CaseInsensitiveEqual ********)
(* Considers either string ended by a blank,tab,or null char .*)


PROCEDURE CaseInsensitiveEqual ( S1 : TEXT ; S2 : TEXT ) : BOOLEAN
  = VAR j : INTEGER := i
  ; l : INTEGER := 0
  ; Lch1 : CHAR
  ; Lch2 : CHAR
  ; lt1 : INTEGER
  ; lt2 : INTEGER
  ; BEGIN
      WHILE ( TRUE )
      DO

        Lch1 := Text . GetChar ( S1 , j )
      ; Lch2 := Text . GetChar ( S2 , l )
      ; IF ( j = Text . Length ( S1 ) OR Lch1 = ' ' OR Lch1 = '\t' )
        THEN
          IF ( l = Text . Length ( S2 ) OR Lch2 = ' ' OR Lch2 = '\t' )
          THEN
            RETURN ( TRUE )
          ELSE
            RETURN ( FALSE )
          END
        ;
        ELSIF ( l = Text . Length ( S2 ) OR Lch2 = ' ' OR Lch2 = '\t' )
        THEN
          RETURN ( FALSE )
        ELSE
          IF ( 'A' <= Lch1 AND Lch1 <= 'Z' )
          THEN
            lt1 := ORD ( Lch1 ) - ORD ( 'A' ) + ORD ( 'a' )
          ;
          END
        ; IF ( 'A' <= Lch2 AND Lch2 <= 'Z' )
          THEN
            lt2 := ORD ( Lch2 ) - ORD ( 'A' ) + ORD ( 'a' )
          ;
          END
        ; IF ( lt1 # lt2 )
          THEN
            RETURN ( FALSE )
          ELSE
            l := l + 1
          ; j := j + 1
          ;


          END
        ;
        END
      ;
      END
    ; RETURN ( FALSE )
    END CaseInsensitiveEqual
;



(****************** Procedure HashLookUp ********************)
(*Increments String past one word and returns the word's numeric value.
  Consider the string ended by a blank,tab, or null char.
  Result could be EmptyString Or InvalidWord.
*)

PROCEDURE HashLookUp ( String : TEXT ) : INTEGER
  = VAR LWordStart : INTEGER
  ; LHash : INTEGER
  ; LInitialProbe : INTEGER
  ; LProbe : INTEGER
  ; LIncr : INTEGER
  ; refrec : TableElemTyp
  ; BEGIN
      LWordStart := i
    ; (*Text.GetChar(String,i);*)
    LHash := HashCode ( String , 1 )
    ; IF ( LWordStart = i )
      THEN
        RETURN ( EmptyString )
      ELSE
        LInitialProbe := LHash MOD TableSize
      ; LProbe := LInitialProbe
      ; LIncr := LHash MOD ( TableSize - 1 ) + 1
      ;
           (* Must be a multiple of Tablesize *)
      WHILE ( TRUE )
        DO
          refrec := Table [ LProbe ]
        ; IF ( refrec . TeString = NIL )
          THEN
            (* We found an unused element.This is not a known 
                               string. *)
            RETURN ( InvalidWord )
          ELSE
            IF ( CaseInsensitiveEqual ( String , refrec . TeString ) )
            THEN
              RETURN ( refrec . TeValue )
            ELSE
              LProbe := LProbe + LIncr
            ;
            (*IF ( LProbe = LInitialProbe) THEN
                                (* This Cannot Happen *)
                                END;*)
            END
          ;
          END
        ;
        END
      ;
      END
    ; RETURN ( InvalidWord )
    END HashLookUp
;


(****************************** Procedure MakeTableEmpty ********)



PROCEDURE MakeTableEmpty ( )
  = VAR

  BEGIN
      FOR k := FIRST ( Table ) TO LAST ( Table )
      DO Table [ k ] . TeString := NIL
      ; Table [ k ] . TeValue := 0
      ;
      (*LHashElem := LHashElem + 1;*)
      END
    ;
    END MakeTableEmpty
;


(*IsInitialized : BOOLEAN = FALSE;*)



(********************   Procedure EnsureInitialized **********)
PROCEDURE EnsureInitialized ( )
  = VAR IsInitialized : BOOLEAN := FALSE
  ; BEGIN

      IF ( NOT IsInitialized )
      THEN
        MakeTableEmpty ( )
      ;

        HashInsert ( "zero" , 0 )
      ; HashInsert ( "one" , 1 )
      ; HashInsert ( "two" , 2 )
      ; HashInsert ( "three" , 3 )
      ; HashInsert ( "four" , 4 )
      ; HashInsert ( "five" , 5 )
      ; HashInsert ( "six" , 6 )
      ; HashInsert ( "seven" , 7 )
      ; HashInsert ( "eight" , 8 )
      ; HashInsert ( "nine" , 9 )
      ; HashInsert ( "ten" , 10 )
      ; HashInsert ( "eleven" , 11 )
      ; HashInsert ( "twelve" , 12 )
      ; HashInsert ( "thirteen" , 13 )
      ; HashInsert ( "fourteen" , 14 )
      ; HashInsert ( "fifteen" , 15 )
      ; HashInsert ( "sixteen" , 16 )
      ; HashInsert ( "seventeen" , 17 )
      ; HashInsert ( "eighteen" , 18 )
      ; HashInsert ( "nineteen" , 19 )
      ; HashInsert ( "twenty" , 20 )
      ; HashInsert ( "thirty" , 30 )
      ; HashInsert ( "fourty" , 40 )
      ; HashInsert ( "fifty" , 50 )
      ; HashInsert ( "sixty" , 60 )
      ; HashInsert ( "seventy" , 70 )
      ; HashInsert ( "eighty" , 80 )
      ; HashInsert ( "ninety" , 90 )
      ; HashInsert ( "hundred" , 100 )
      ; IsInitialized := TRUE
      ;
      END
    ;
    END EnsureInitialized
;

(*********************** Trim Procedure ***************************)
(*Increment String to skip leading blanks and tabs*)

PROCEDURE Trim ( String : TEXT )
  = VAR Lch : CHAR
  ; BEGIN
      WHILE ( TRUE )
      DO
        Lch := Text . GetChar ( String , i )
      ; IF ( Lch = ' ' OR Lch = '\t' )
        THEN
          i := i + 1
        ;
        ELSE
          EXIT
        END
      ;
      END
    ;

    END Trim
;

(*************************NextWordValue***************************)


(*Increments String past leading white space, plus the next word and 
  returns the word's numeric value.
  Considers the string ended by by a blank,tab or null char.
  Could return Emptystring or InvalidWord.
*)

PROCEDURE NextWordValue ( String : TEXT ) : INTEGER
  = VAR LWordStart : TEXT
  ; VAR LTo : INTEGER
  ; VAR LResult : INTEGER
  ; BEGIN
      Trim ( String )
    ; RETURN HashLookUp ( String )
    END NextWordValue
;




(**************************Visible- TOInt  ****************************)
PROCEDURE ToInt ( String : TEXT ) : INTEGER
  =
(* Returns InvalidWord  for Untranslatable strings.
   Returns Empty String for a completely empty string.
   Otherwise Converts string in the  range 0..999 to an integer.
   This fails in some cases.
*)
  VAR LResult : INTEGER
  ; VAR LTerm : INTEGER
  ; BEGIN

      EnsureInitialized ( )
    ; i := 0
    ; LResult := EmptyString
    ; WHILE ( TRUE )
      DO
        LTerm := NextWordValue ( String )
      ; IF ( LTerm = InvalidWord )
        THEN
          LResult := InvalidWord
        ; EXIT
        ELSIF ( LTerm = EmptyString )
        THEN
          EXIT
        ELSIF ( LTerm = 0 )
        THEN
          IF ( LResult = EmptyString )
          THEN
            LTerm := NextWordValue ( String )
          ; IF ( LTerm = EmptyString )
            THEN
              RETURN ( 0 )
            ;
            ELSE
              RETURN ( InvalidWord )
            ;
            END
          ;
          ELSE
            RETURN ( InvalidWord )
          ;
          END
        ;
        ELSE
          IF LResult = EmptyString
          THEN
            LResult := 0
          ;
          END
        ; IF ( LTerm = 100 )
          THEN
            LResult := LResult * 100
          ;
          ELSE
            LResult := LResult + LTerm
          ;
          END
        ;
        END
      ;
      END
    ;

    IF (   0 <= LResult AND LResult <= 999 )
      THEN
        RETURN ( LResult )
      ;
      ELSE
        RETURN InvalidWord
      ;
      END
    ;
    END ToInt
;


BEGIN

  END WordNumbers
.















