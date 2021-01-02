
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Misc 

(* Some miscellaneous utilities, not easily otherwise categorized. *) 

; IMPORT Char 
; IMPORT Date 
; IMPORT Fingerprint 
; IMPORT Fmt 
; IMPORT FS 
; IMPORT OSError 
; IMPORT Pathname 
; IMPORT Process 
; IMPORT Rd
; IMPORT TextRd 
; IMPORT TextSeq 
; IMPORT TextWr 
; IMPORT Text 
; IMPORT Thread
; IMPORT Wr 

; IMPORT LbeStd 
; IMPORT PortTypes 
; IMPORT UnsafeUtils 

; CONST FixedLength = 85 
; CONST FixedBlanks = ARRAY [ 0 .. FixedLength - 1 ] OF CHAR { ' ' , .. } 

; VAR FixedText : TEXT 

(* EXPORTED: *) 
; PROCEDURE LoadYourself ( )
  (* Call this early, to get the containing dynamic library package loaded. *)

  = VAR I : INTEGER
  ; BEGIN
      I := 1765087
    END LoadYourself

; CONST DecVals
    = ARRAY OF INTEGER
        { 0
        , 9                      (* 1 *)
        , 99                     (* 2 *)
        , 999                    (* 3 *)
        , 9999                   (* 4 *)
        , 99999                  (* 5 *)
        , 999999                 (* 6 *)
        , 9999999                (* 7 *)
        , 99999999               (* 8 *)
        , 999999999              (* 9 *)
        , 2147483647             (* 10 *) (* = 16_7FFFFFFF *)
(* Use these for the 64-bit case:  
        , 9999999999             (* 10 *)
        , 99999999999            (* 11 *)
        , 999999999999           (* 12 *)
        , 9999999999999          (* 13 *)
        , 99999999999999         (* 14 *)
        , 999999999999999        (* 15 *)
        , 9999999999999999       (* 16 *)
        , 99999999999999999      (* 17 *)
        , 999999999999999999     (* 18 *)
        , 9223372036854775807    (* 19 *) (* = 16_7FFFFFFFFFFFFFFF *)
*)
        } 

(* EXPORTED: *)
; PROCEDURE CeilLog10 ( Val : INTEGER ) : INTEGER
(* Number of characters required to represent Val in decimal. *)
(* Currently works only up to 2**31-1, regardless of INTEGER size. *)

  = CONST ForFIRST = 11 (* For 64-bit case, 20 *)
  ; VAR Abs : INTEGER
  ; VAR LLo , LHi , LProbe , LResult : INTEGER
  ; VAR LIsNeg : BOOLEAN := FALSE  

  ; BEGIN
      IF Val = FIRST ( INTEGER )
      THEN RETURN ForFIRST
      END (* IF *) 
    ; IF Val < 0
      THEN
        Abs := - Val
      ; LIsNeg := TRUE
      ELSE Abs := Val 
      END
    (* INVARIANT : DecVals [ LLo ] < Abs <= DecVals [ LHi] *) 
    ; LLo := 0
    ; LHi := LAST ( DecVals ) 
    ; LOOP
        IF LLo + 1 >= LHi
        THEN
          LResult := LHi
        ; EXIT 
        ELSE 
          LProbe := ( LLo + LHi ) DIV 2
        ; IF DecVals [ LProbe ] < Abs
          THEN LLo := LProbe 
          ELSE LHi := LProbe 
          END (*( IF *) 
        END (* IF *) 
      END (* LOOP *)
    ; INC ( LResult , ORD ( LIsNeg ) ) 
    ; RETURN LResult 
    END CeilLog10
    
(* EXPORTED: *) 
; PROCEDURE Blanks ( Length : PortTypes . Int32Typ ) : TEXT 
  (* A TEXT of length Length, all blanks. *) 

  = VAR LRemaining : PortTypes . Int32Typ 
  ; VAR LResult : TEXT 

(* TODO: Make this keep an array or text around, expanding when needed,
         and then available for the next time.
*) 
(* TODO: I think this is duplicated somewhere. *) 
  ; BEGIN (* Blanks *) 
      IF Length <= FixedLength 
      THEN (* Fast path: *) 
        RETURN Text . FromChars ( SUBARRAY ( FixedBlanks , 0 , Length ) ) 
      ELSE 
        LRemaining := Length - FixedLength 
      ; LResult := FixedText 
      ; WHILE LRemaining >= FixedLength 
        DO LResult := LResult & FixedText 
        ; DEC ( LRemaining , FixedLength ) 
        END (* WHILE *) 
      ; RETURN 
          LResult 
          & Text . FromChars ( SUBARRAY ( FixedBlanks , 0 , LRemaining ) ) 
      END (* IF *) 
    END Blanks 

(* EXPORTED: *) 
; PROCEDURE IntegerImage ( Value : INTEGER ) : TEXT 

  = BEGIN 
      RETURN Fmt . Int ( Value ) 
    END IntegerImage 

(* EXPORTED: *) 
; PROCEDURE BooleanImageShort ( Value : BOOLEAN ) : TEXT 
  (* "F" or "T" *) 

  = BEGIN (* BooleanImageShort *) 
      IF Value THEN RETURN "T" ELSE RETURN "F" END (* IF *) 
    END BooleanImageShort 

(* EXPORTED: *) 
; PROCEDURE DateImage ( READONLY Value : Date . T ) : TEXT 

  = BEGIN 
      RETURN 
        Fmt . Int ( Value . year ) 
        & "/" & Fmt . Pad ( Fmt . Int ( ORD ( Value . month ) + 1 ) , 2 , '0' ) 
        & "/" & Fmt . Pad ( Fmt . Int ( Value . day ) , 2 , '0' )
        & " " & Fmt . Pad ( Fmt . Int ( Value . hour ) , 2 , '0' )
        & ":" & Fmt . Pad ( Fmt . Int ( Value . minute ) , 2 , '0' )
        & ":" & Fmt . Pad ( Fmt . Int ( Value . second ) , 2 , '0' )
        & " " & Value . zone
    END DateImage 

(* EXPORTED: *) 
; PROCEDURE FingerprintImage ( READONLY Value : Fingerprint . T ) : TEXT 

  = BEGIN 
      RETURN 
        Fmt . Pad ( Fmt . Unsigned ( Value . byte [ 0 ] , 16 ) , 2 , '0' )  
        & Fmt . Pad ( Fmt . Unsigned ( Value . byte [ 1 ] , 16 ) , 2 , '0' )  
        & Fmt . Pad ( Fmt . Unsigned ( Value . byte [ 2 ] , 16 ) , 2 , '0' )  
        & Fmt . Pad ( Fmt . Unsigned ( Value . byte [ 3 ] , 16 ) , 2 , '0' )  
        & Fmt . Pad ( Fmt . Unsigned ( Value . byte [ 4 ] , 16 ) , 2 , '0' )  
        & Fmt . Pad ( Fmt . Unsigned ( Value . byte [ 5 ] , 16 ) , 2 , '0' )  
        & Fmt . Pad ( Fmt . Unsigned ( Value . byte [ 6 ] , 16 ) , 2 , '0' )  
        & Fmt . Pad ( Fmt . Unsigned ( Value . byte [ 7 ] , 16 ) , 2 , '0' )  
    END FingerprintImage 

; VAR WrT : TextWr . T 
; VAR RdT : TextRd . T 

; CONST AddressImageLen = BYTESIZE ( ADDRESS ) * 2 

(* EXPORTED: *) 
; PROCEDURE RefanyImage ( Ref : REFANY ) : TEXT 

  = BEGIN
      RETURN 
        "16_" 
        & Fmt . Pad 
            ( Fmt . Unsigned ( UnsafeUtils . IntOfRefany ( Ref ) , base := 16 ) 
            , length := AddressImageLen 
            , padChar := '0' 
            ) 
    END RefanyImage 

(* EXPORTED: *) 
; PROCEDURE EscapeText ( String : TEXT ) : TEXT 
(* Add Modula-3 TEXT literal escapes. Do not add enclosing double quotes. *) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    <* FATAL Rd . Failure *> 
    <* FATAL Rd . EndOfFile *> 
    VAR C : CHAR 

  ; BEGIN (* EscapeText *) 
      EVAL WrT . init ( ) 
    ; EVAL RdT . init ( String ) 
    ; WHILE NOT Rd . EOF ( RdT ) 
      DO 
        C := Rd . GetChar ( RdT ) 
      ; CASE C 
        OF '\"' , '\'' , '\\' 
        => Wr . PutChar ( WrT , '\\' ) 
        ; Wr . PutChar ( WrT , C ) 
        | '\n' 
        => Wr . PutChar ( WrT , '\\' ) 
        ; Wr . PutChar ( WrT , 'n' ) 
        | '\t' 
        => Wr . PutChar ( WrT , '\\' ) 
        ; Wr . PutChar ( WrT , 't' ) 
        | '\r' 
        => Wr . PutChar ( WrT , '\\' ) 
        ; Wr . PutChar ( WrT , 'r' ) 
        | '\f' 
        => Wr . PutChar ( WrT , '\\' ) 
        ; Wr . PutChar ( WrT , 'f' ) 
        | ' ' .. '!' , '#' .. '&' , '(' .. '[' , ']' .. '~'
        , LbeStd . LeftPlaceholderDelimChar   
        , LbeStd . RightPlaceholderDelimChar   
(* FIXME: Make this adapt to placeholder delimiter strings of length > 1. *) 
        => Wr . PutChar ( WrT , C ) 
        ELSE 
          Wr . PutChar ( WrT , '\\' ) 
(* TODO: Use hex instead of octal here. *) 
        ; Wr . PutText 
            ( WrT 
            , Fmt . Pad 
                ( Fmt . Int ( ORD ( C ) , base := 8 ) 
                , length := 3 
                , padChar := '0' 
                , align := Fmt . Align . Right 
                ) 
            ) 
        END (* CASE *) 
      END (* WHILE *) 
    ; RETURN TextWr . ToText ( WrT ) 
    END EscapeText 

(* EXPORTED: *) 
; PROCEDURE QuoteText ( String : TEXT ) : TEXT 
  (* Add escape sequences and string quotes. *) 

  = BEGIN 
      RETURN "\"" & EscapeText ( String ) & "\""  
    END QuoteText 

(* EXPORTED: *) 
; PROCEDURE QuoteChar ( Ch : CHAR ) : TEXT 
  (* Add escape sequences and character quotes. *) 

  = BEGIN 
      RETURN "'" & EscapeText ( Text . FromChar ( Ch ) ) & "'"  
    END QuoteChar  

(* EXPORTED: *) 
; PROCEDURE ReadAndUnquoteText ( RdT : Rd . T ) : TEXT 
  (* Read a Modula-3 string from RdT.  Remove leading and trailing blanks 
     and bounding string quotes.  Also convert escape sequences inside to 
     single characters.  Consume the string from RdT and return its 
     unquoted version. 
  *) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    <* FATAL Rd . Failure *> 
    <* FATAL Rd . EndOfFile *> 
    VAR C : CHAR 
  ; VAR C2 : CHAR 
  ; VAR C3 : CHAR 
  ; VAR LValue : INTEGER 
  ; VAR LDigitCount : INTEGER 

  ; BEGIN (* ReadAndUnquoteText *) 
      LOOP (* Scan through leading blanks . *) 
        IF Rd . EOF ( RdT )  
        THEN RETURN "" (* No string at all. *)    
        ELSE 
          C := Rd . GetChar ( RdT ) 
        ; CASE C 
          OF ' ' => (* Go around loop *) 
          | '\"' 
          => (* Opening string quote. *) 
            EXIT 
          ELSE (* Something other than a string. *) 
            Rd . UnGetChar ( RdT ) 
          ; RETURN "" 
          END (* CASE  *) 
        END (* IF *) 
      END (* LOOP *) 
    ; EVAL WrT . init ( ) 
    ; LOOP 
        IF Rd . EOF ( RdT ) 
        THEN (* No closing quote.  Quietly terminate the string anyway. *) 
          EXIT 
        ELSE 
          C := Rd . GetChar ( RdT ) 
        ; CASE C 
          OF '\"' 
          => (* Unescaped quote, this is the end of the string. *)
            EXIT  

          | '\\' 
          =>  
            IF Rd . EOF ( RdT ) 
            THEN (* What is the right thing to do now? Just leave the 
                    backslash hanging at the end? 
                 *) 
              Wr . PutChar ( WrT , C ) 
            ; EXIT 
            ELSE 
              C2 := Rd . GetChar ( RdT ) 
            ; CASE C2 
              OF '0' .. '7' 
              => LDigitCount := 1 
              ; LValue := ORD ( C2 ) - ORD ( '0' ) 
              ; LOOP
                  IF LDigitCount >= 3 OR Rd . EOF ( RdT ) 
                  THEN EXIT 
                  ELSE 
                    C3 := Rd . GetChar ( RdT ) 
                  ; CASE C3 
                    OF '0' .. '7' 
                    => LValue := LValue * 8 + ORD ( C3 ) - ORD ( '0' ) 
                    ; INC ( LDigitCount ) 
                    ELSE EXIT 
                    END (* CASE *)  
                  END (* IF *)  
                END (* LOOP *) 
              ; LValue := MAX ( LValue , ORD ( LAST ( CHAR ) ) )  
              ; Wr . PutChar ( WrT , VAL ( LValue , CHAR ) ) 

              | 'n' => Wr . PutChar ( WrT , '\n' )   
              | 't' => Wr . PutChar ( WrT , '\t' ) 
              | 'r' => Wr . PutChar ( WrT , '\r' ) 
              | 'f' => Wr . PutChar ( WrT , '\f' ) 

              ELSE (* Anything else escapes to itself. *)  
                Wr . PutChar ( WrT , C2 ) 
              END (* CASE *)  
            END (* IF *) 

          ELSE 
            Wr . PutChar ( WrT , C ) 
          END (* CASE  *) 
        END (* IF *) 
      END (* LOOP *) 
    ; RETURN TextWr . ToText ( WrT ) 
    END ReadAndUnquoteText  

(* EXPORTED: *) 
; PROCEDURE UnescapeText ( String : TEXT ) : TEXT 
  (* Remove Modula-3 TEXT literal escapes. *)  

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    <* FATAL Rd . Failure *> 
    <* FATAL Rd . EndOfFile *> 
    VAR C : CHAR 
  ; VAR C2 : CHAR 
  ; VAR C3 : CHAR 
  ; VAR LValue : INTEGER 
  ; VAR LDigitCount : INTEGER 

  ; BEGIN (* UnescapeText *) 
      EVAL WrT . init ( ) 
    ; EVAL RdT . init ( String ) 
    ; WHILE NOT Rd . EOF ( RdT ) 
      DO 
        C := Rd . GetChar ( RdT ) 
(* FIXME: Handle hex escapes. *) 
      ; IF C = '\\' 
        THEN 
          IF Rd . EOF ( RdT ) 
          THEN (* What is the right thing to do now? *) 
            Wr . PutChar ( WrT , C ) 
          ELSE 
            C2 := Rd . GetChar ( RdT ) 
          ; CASE C2 
            OF '0' .. '7' 
            => LDigitCount := 1 
            ; LValue := ORD ( C2 ) - ORD ( '0' ) 
            ; LOOP
                IF LDigitCount >= 3 OR Rd . EOF ( RdT ) 
                THEN EXIT 
                ELSE 
                  C3 := Rd . GetChar ( RdT ) 
                ; CASE C3 
                  OF '0' .. '7' 
                  => LValue := LValue * 8 + ORD ( C3 ) - ORD ( '0' ) 
                  ; INC ( LDigitCount ) 
                  ELSE EXIT 
                  END (* CASE *)  
                END (* IF *)  
              END (* LOOP *) 
            ; LValue := MAX ( LValue , ORD ( LAST ( CHAR ) ) )  
            ; Wr . PutChar ( WrT , VAL ( LValue , CHAR ) ) 
            | 'n' => Wr . PutChar ( WrT , '\n' )   
            | 't' => Wr . PutChar ( WrT , '\t' ) 
            | 'r' => Wr . PutChar ( WrT , '\r' ) 
            | 'f' => Wr . PutChar ( WrT , '\f' ) 
            ELSE 
              Wr . PutChar ( WrT , C2 ) 
            END (* CASE *)  
          END (* IF *) 
        ELSE 
          Wr . PutChar ( WrT , C ) 
        END (* IF *) 
      END (* WHILE *) 
    ; RETURN TextWr . ToText ( WrT ) 
    END UnescapeText 

(* EXPORTED: *) 
; PROCEDURE UnquoteText ( String : TEXT ) : TEXT 
  (* Remove any leading and trailing blanks and bounding string quotes.
     If there are bounding quotes at each end, convert escape sequences 
     inside to single characters. 
  *) 

  = VAR LLength : INTEGER 
  ; VAR LStart : INTEGER  
  ; VAR LChar : CHAR
  ; VAR LHasLeadingQuote , LHasTrailingQuote : BOOLEAN  

  ; BEGIN 
      LLength := Text . Length ( String ) 
    ; LStart := 0 
    ; LOOP 
        IF LLength <= 0 
        THEN EXIT 
        ELSE 
          LChar := Text . GetChar ( String , LLength - 1 ) 
        ; IF LChar = ' ' 
          THEN DEC ( LLength ) 
          ELSE
            LHasTrailingQuote := LChar = '"'
          ; DEC ( LLength , ORD ( LHasTrailingQuote ) )   
          ; EXIT  
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    ; LOOP 
        IF LStart >= LLength  
        THEN EXIT 
        ELSE 
          LChar := Text . GetChar ( String , LStart ) 
        ; IF LChar = ' ' 
          THEN 
            INC ( LStart ) 
          ; DEC ( LLength ) 
          ELSE
            LHasLeadingQuote := LChar = '"'
          ; DEC ( LLength , ORD ( LHasLeadingQuote ) )   
          ; INC ( LStart , ORD ( LHasLeadingQuote ) )  
          ; EXIT 
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    ; IF LHasLeadingQuote AND LHasTrailingQuote 
      THEN RETURN UnescapeText ( Text . Sub ( String , LStart , LLength ) ) 
      ELSE RETURN Text . Sub ( String , LStart , LLength ) 
      END (* IF *) 
    END UnquoteText 

; PROCEDURE UnquoteChar ( String : TEXT ) : CHAR  
  (* Remove leading and trailing blanks and bounding character quotes.
     Convert any escape sequence inside to a single character.
     Convert to a CHAR.  
     Return LbeStd . CharNull, if invalid. 
  *) 

  = VAR LLength : INTEGER 
  ; VAR LStart : INTEGER  
  ; VAR LCharString : TEXT 
  ; VAR LChar : CHAR 

  ; BEGIN 
      LLength := Text . Length ( String ) 
    ; LStart := 0 
    ; LOOP 
        IF LLength <= 0 
        THEN EXIT 
        ELSE 
          LChar := Text . GetChar ( String , LLength - 1 ) 
        ; IF LChar = ' ' 
          THEN DEC ( LLength ) 
          ELSE
            IF LChar = '\'' 
            THEN 
              DEC ( LLength ) 
            END (* IF *)  
          ; EXIT  
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    ; LOOP 
        IF LStart >= LLength  
        THEN EXIT 
        ELSE 
          LChar := Text . GetChar ( String , LStart ) 
        ; IF LChar = ' ' 
          THEN 
            INC ( LStart ) 
          ; DEC ( LLength ) 
          ELSE
            IF LChar = '\'' 
            THEN 
              INC ( LStart ) 
            ; DEC ( LLength ) 
            END (* IF *) 
          ; EXIT 
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    ; LCharString 
        := UnescapeText ( Text . Sub ( String , LStart , LLength ) ) 
    ; IF Text . Length ( LCharString ) # 1 
      THEN 
        RETURN LbeStd . CharNull 
      ELSE 
        RETURN Text . GetChar ( LCharString , 0 ) 
      END (* IF *) 
    END UnquoteChar  

(* EXPORTED: *) 
; PROCEDURE CharIsEqual 
    ( Ch1 , Ch2 : CHAR ; CaseSensitive : BOOLEAN := TRUE ) 
  : BOOLEAN 

  = BEGIN 
      IF NOT CaseSensitive 
      THEN 
        IF 'A' <= Ch1 AND Ch1 <= 'Z' 
        THEN Ch1 := VAL ( ORD ( Ch1 ) - ORD ( 'A' ) + ORD ( 'a' ) , CHAR ) 
        END (* IF *) 
      ; IF 'A' <= Ch2 AND Ch2 <= 'Z' 
        THEN Ch2 := VAL ( ORD ( Ch2 ) - ORD ( 'A' ) + ORD ( 'a' ) , CHAR ) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN Ch1 = Ch2 
    END CharIsEqual 

(* EXPORTED: *) 
; PROCEDURE CharCompare  
    ( Ch1 , Ch2 : CHAR ; CaseSensitive : BOOLEAN := TRUE ) 
  : [ - 1 .. 1 ] 

  = BEGIN 
      IF NOT CaseSensitive 
      THEN 
        IF 'A' <= Ch1 AND Ch1 <= 'Z' 
        THEN Ch1 := VAL ( ORD ( Ch1 ) - ORD ( 'A' ) + ORD ( 'a' ) , CHAR ) 
        END (* IF *) 
      ; IF 'A' <= Ch2 AND Ch2 <= 'Z' 
        THEN Ch2 := VAL ( ORD ( Ch2 ) - ORD ( 'A' ) + ORD ( 'a' ) , CHAR ) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN Char . Compare ( Ch1 , Ch2 )  
    END CharCompare  

; VAR GFilePathSeparator : TEXT  

; PROCEDURE InitGFilePathSeparator ( ) 

  = VAR LSeq : TextSeq . T 
  ; VAR LCompose : TEXT 
  ; VAR LSub1 : TEXT 

  ; <* FATAL Pathname . Invalid *> 
    BEGIN (* InitGFilePathSeparator *)
      LSeq := NEW ( TextSeq . T ) 
              . fromArray ( ARRAY OF TEXT { NIL , "A" , "B" } )
      (* Pathname . Compose demands valid arc names.  We only want the
         separator, so fool it with some real arc names, then take them off. *)
    ; LCompose := Pathname . Compose ( LSeq )  
    ; LSub1 := Text . Sub ( LCompose , 1 ) 
    ; GFilePathSeparator
        := Text . Sub ( LSub1 , 0 , Text . Length ( LSub1 ) - 1 ) 
    END InitGFilePathSeparator  

(* EXPORTED: *) 
; PROCEDURE FilePathSeparator ( ) : TEXT 
  (* OS-independent. *) 

  = BEGIN 
      RETURN GFilePathSeparator 
    END FilePathSeparator 

; CONST Digits = SET OF CHAR { '0' .. '9' } 

(* EXPORTED: *) 
; PROCEDURE WoVersionSuffix ( Name : TEXT ) : TEXT 
  (* Remove any suffix that is either dot tilde digit* tilde, or just tilde. *)

= VAR LLength : INTEGER 
; VAR LSs : INTEGER 

; BEGIN 
    IF Name = NIL 
    THEN RETURN Name 
    ELSE 
      LLength := Text . Length ( Name ) 
    ; LSs := LLength - 1 
    ; IF LSs < 0 OR Text . GetChar ( Name , LSs ) # '~' 
      THEN (* Does not end with tilde. *) 
        RETURN Name 
      ELSE (* We have a tilde at the end. *) 
        DEC ( LSs )
      ; IF LSs >= 0 AND Text . GetChar ( Name , LSs ) IN Digits 
        THEN (* We have digit tilde at the end. *)   
          DEC ( LSs ) (* One digit consumed. *) 
        ; LOOP  
            IF  LSs < 0 
            THEN (* No dot tilde before the digits. *) 
              RETURN Name 
            ELSE 
              CASE Text . GetChar ( Name , LSs ) 
              OF '0' .. '9' 
              => DEC ( LSs )
              | '~' 
              => DEC ( LSs ) 
              ; IF LSs >= 0 AND Text . GetChar ( Name , LSs  ) = '.' 
                THEN (* We have dot tilde digit* tilde. Strip it. *) 
                  RETURN Text . Sub ( Name , 0 , LSs ) 
                ELSE (* No dot before tilde digit* tilde. *) 
                  RETURN Name 
                END (* IF *) 
              ELSE (* No tilde before digit* tilde. *) 
                RETURN Name 
              END (* CASE *) 
            END (* IF *) 
          END (* LOOP  *) 
        ELSE (* We have trailing single tilde only. Strip it. *) 
          RETURN Text . Sub ( Name , 0 , LSs + 1 )  
        END (* IF *) 
      END (* IF *) 
    END (* IF *) 
  END WoVersionSuffix 

(* EXPORTED: *) 
; PROCEDURE AbsFileName ( Name : TEXT ) : TEXT 
  (* Name need not exist. OS-independent. *) 

  = VAR LAbsCurrent : TEXT 

  ; BEGIN (* AbsFileName *) 
      IF Pathname . Absolute ( Name ) 
      THEN RETURN Name 
      ELSE 
        TRY 
          LAbsCurrent := FS . GetAbsolutePathname ( Pathname . Current ) 
        ; RETURN LAbsCurrent & GFilePathSeparator & Name 
        EXCEPT OSError . E 
        => RETURN ""
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END AbsFileName 

(* EXPORTED: *) 
; PROCEDURE RelFileName 
    ( Name : TEXT  
    ; RelativeTo : TEXT := NIL 
      (* ^NIL, "", and Pathname.Current all mean git directory. *) 
    ) 
  : TEXT 
  (* If anything goes wrong, returns Name unchanged. *)

  = VAR LAbsPath : Pathname . T 
  ; VAR LAbsArcs : Pathname . Arcs 
  ; VAR LRelTo : Pathname . T 
  ; VAR LRelToArcs : Pathname . Arcs 
  ; VAR LResultArcs : Pathname . Arcs 
  ; VAR LAbsSize : CARDINAL 
  ; VAR LRelToSize : CARDINAL 
  ; VAR LI : CARDINAL 
  ; VAR LResult : TEXT  
  
  ; BEGIN 
      IF Name = NIL 
         OR NOT Pathname . Valid ( Name ) 
         OR ( RelativeTo = NIL AND NOT Pathname . Absolute ( Name ) )  
      THEN RETURN Name 
      ELSE 
        TRY 
          LAbsPath := AbsFileName ( Name ) 
        ; LAbsArcs := Pathname . Decompose ( LAbsPath ) 
        ; IF RelativeTo = NIL 
             OR Text . Equal ( RelativeTo , "" ) 
             OR Text . Equal ( RelativeTo , Pathname . Current ) 
          THEN LRelTo := Process . GetWorkingDirectory ( ) 
          ELSE LRelTo := AbsFileName ( RelativeTo ) 
          END (* IF *)  
        ; LRelToArcs := Pathname . Decompose ( LRelTo ) 
        ; LAbsSize := LAbsArcs . size ( ) 
        ; LRelToSize := LRelToArcs . size ( ) 
        ; LI := 0 
        ; WHILE LI < LAbsSize 
                AND LI < LRelToSize 
                AND Text . Equal 
                      ( LAbsArcs . get ( LI ) , LRelToArcs . get ( LI ) ) 
          DO INC ( LI ) 
          END (* IF *) 
        ; LResultArcs := NEW ( TextSeq . T ) . init ( sizeHint := 10 ) 
        ; LResultArcs . addhi ( NIL ) (* Make it relative. *) 
        ; FOR RJ := LI TO LRelToSize - 1  
          DO LResultArcs . addhi ( Pathname . Parent ) 
          END (* FOR *) 
        ; WHILE LI < LAbsSize 
          DO 
            LResultArcs . addhi ( LAbsArcs . get ( LI ) ) 
          ; INC ( LI ) 
          END (* WHILE *) 
        ; LResult := Pathname . Compose ( LResultArcs ) 
        EXCEPT ELSE RETURN Name 
        END (* TRY EXCEPT *) 
      ; RETURN LResult 
      END (* IF *) 
    END RelFileName 

(* EXPORTED: *) 
; PROCEDURE JoinPath ( Path : TEXT ; File : TEXT ) : TEXT 
  (* OS-independent. *) 

  = BEGIN (* JoinPath *) 
      RETURN Pathname . Join ( Path , File , ext := NIL )  
    END JoinPath

(* EXPORTED: *) 
; PROCEDURE PickleName ( Name : TEXT ) : TEXT 
  (* Convert text file suffix or checkpoint suffix to pickle suffix. 
     First remove version suffix. 
     If this is already a pickle suffix, return it. 
     Works on anything from a full path name down to 
     just a suffix without even a dot. *) 

  = VAR LLength : CARDINAL 
  ; VAR LName : TEXT 

  ; BEGIN (* PickleName *) 
      IF Name = NIL 
      THEN 
        RETURN "" 
      ELSE 
        LName := WoVersionSuffix ( Name ) 
      ; LLength := Text . Length ( LName ) 
      ; IF LLength >= 1 AND Text . GetChar ( LName , LLength - 1 ) = '_' 
        THEN 
          IF LLength >= 2 AND Text . GetChar ( LName , LLength - 2 ) = '_' 
          THEN (* It's a checkpoint name. *) 
            RETURN Text . Sub ( LName , 0 , LLength - 1 ) 
          ELSE (* It's already a pickle name. *) 
            RETURN LName 
          END (* IF *) 
        ELSE (* It's a text file name. *) 
          RETURN LName & "_" 
        END (* IF *) 
      END (* IF *) 
    END PickleName 

(* EXPORTED: *) 
; PROCEDURE CheckpointName ( Name : TEXT ) : TEXT 
  (* Convert text file suffix or pickle to checkpoint suffix. 
     First remove version suffix. 
     If this is already a checkpoint suffix, return it. 
     Works on anything from a full path name down to 
     just a suffix without even a dot. *) 

  = VAR LLength : CARDINAL 
  ; VAR LName : TEXT 

  ; BEGIN (* PickleName *) 
      IF Name = NIL 
      THEN 
        RETURN "" 
      ELSE 
        LName := WoVersionSuffix ( Name ) 
      ; LLength := Text . Length ( LName ) 
      ; IF LLength >= 1 AND Text . GetChar ( LName , LLength - 1 ) = '_' 
        THEN 
          IF LLength >= 2 AND Text . GetChar ( LName , LLength - 2 ) = '_' 
          THEN (* It's already a checkpoint name. *) 
            RETURN LName 
          ELSE (* It's a pickle name. *) 
            RETURN LName & "_" 
          END (* IF *) 
        ELSE (* It's a text name. *) 
          RETURN LName & "__" 
        END (* IF *) 
      END (* IF *) 
    END CheckpointName 

(* EXPORTED: *) 
; PROCEDURE TextName ( Name : TEXT ) : TEXT 
  (* Convert a pickle suffix or checkpoint suffix to a text file suffix. 
     First remove version suffix. 
     If this is already a text file suffix, return it. 
     Works on anything from a full path name down to 
     just a suffix without even a dot. *) 

  = VAR LLength : CARDINAL 
  ; VAR LName : TEXT 

  ; BEGIN (* TextName *) 
      IF Name = NIL 
      THEN 
        RETURN "" 
      ELSE 
        LName := WoVersionSuffix ( Name ) 
      ; LLength := Text . Length ( LName ) 
      ; IF LLength >= 1 AND Text . GetChar ( LName , LLength - 1 ) = '_' 
        THEN 
          IF LLength >= 2 AND Text . GetChar ( LName , LLength - 2 ) = '_' 
          THEN (* It's a checkpoint name. *) 
            RETURN Text . Sub ( LName , 0 , LLength - 2 ) 
          ELSE (* It's a pickle name. *) 
            RETURN Text . Sub ( LName , 0 , LLength - 1 ) 
          END (* IF *) 
        ELSE 
          RETURN LName 
        END (* IF *) 
      END (* IF *) 
    END TextName 

(* EXPORTED: *) 
; PROCEDURE IsPickleName ( Name : TEXT ) : BOOLEAN 
  (* Ignores a version suffix.  Works on anything from a full path name 
     down to just a suffix without even a dot. 
  *) 

  = VAR LLength : CARDINAL 
  ; VAR LName : TEXT 

  ; BEGIN (* IsPickleName *) 
      IF Name = NIL 
      THEN 
        RETURN FALSE 
      ELSE 
        LName := WoVersionSuffix ( Name ) 
      ; LLength := Text . Length ( LName ) 
      ; RETURN LLength >= 1  
               AND Text . GetChar ( LName , LLength - 1 ) = '_' 
               AND ( LLength = 1 
                     OR Text . GetChar ( LName , LLength - 2 ) # '_' 
                   ) 
      END (* IF *) 
    END IsPickleName 

(* EXPORTED: *) 
; PROCEDURE IsPickleOrCheckpointName ( Name : TEXT ) : BOOLEAN 
  (* Ignores a version suffix.  Works on anything from a full path name 
     down to just a suffix without even a dot. 
  *) 

  = VAR LLength : CARDINAL 
  ; VAR LName : TEXT 

  ; BEGIN (* IsPickleName *) 
      IF Name = NIL 
      THEN 
        RETURN FALSE 
      ELSE 
        LName := WoVersionSuffix ( Name ) 
      ; LLength := Text . Length ( LName ) 
      ; RETURN LLength >= 1 AND Text . GetChar ( LName , LLength - 1 ) = '_' 
      END (* IF *) 
    END IsPickleOrCheckpointName 

; BEGIN (* Misc *) 
    FixedText := Text . FromChars ( FixedBlanks ) 
  ; WrT := TextWr . New ( ) 
  ; RdT := TextRd . New ( "" )
  ; InitGFilePathSeparator ( ) 
  END Misc 
. 
