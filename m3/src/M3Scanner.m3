
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE M3Scanner 

; IMPORT TextIntTbl 

; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 
; IMPORT SchutzCoroutine 
; IMPORT LangUtil  
; IMPORT LbeStd 
; IMPORT M3InitTokStrings 
; IMPORT M3Tok 
; IMPORT MessageCodes 
; IMPORT PortTypes 
; IMPORT ScannerIf 
; IMPORT Strings 

; TYPE AFT = MessageCodes . T 

; CONST GoodChars 
    = SET OF CHAR 
        { LbeStd . CharEndOfImage , LbeStd . CharNewLine , ' ' , '.' , ':' 
        , ';' , '*' , '/' , '<' , '>' , '=' , '#' , '|' , '^' , ',' , '&' 
        , '[' , ']' , '{' , '}' , '+' , '|' , '-' , '_' , '!' , '@' , '(' 
        , ')' , 'a' .. 'z' , 'A' .. 'Z' , '0' .. '9' , '"'
        , LbeStd . LeftPlaceholderDelimChar 
        , LbeStd . RightPlaceholderDelimChar   
        } 

; VAR RwTable : TextIntTbl . Default 
; VAR PhTable : TextIntTbl . Default 

; PROCEDURE InitTables ( ) 

  = BEGIN 
      RwTable := NEW ( TextIntTbl . Default ) . init ( sizeHint := 61 )
    ; M3InitTokStrings . InitRw ( RwTable )  
    ; PhTable := NEW ( TextIntTbl . Default ) . init ( sizeHint := 235 ) 
    ; M3InitTokStrings . InitPh ( PhTable )  
    END InitTables

(* VISIBLE *) 
; PROCEDURE Scan ( Cr : SchutzCoroutine . T ) 


  = VAR Sif : ScannerIf . ScanIfTyp 
  ; VAR InString : Strings . StringTyp 
  ; VAR State : LbeStd . ScanStateTyp 
  ; VAR SaveState : LbeStd . ScanStateTyp 
  ; VAR InLength : Strings . StringSsTyp 
  ; VAR Pos : PortTypes . Int32Typ 
  ; VAR TokString : Strings . StringTyp 
  ; VAR Ch : CHAR 
  ; VAR BackCh : CHAR (* Meaningful only when Pos = - 1 *) 
    ; AreAllBlanks : BOOLEAN 

  (* It is possible to put back at most one character, and it must 
     be the one that was gotten there. *) 
  ; PROCEDURE PutBackChar ( Ch : CHAR ) 
    RAISES { AssertionFailure } 

    = BEGIN (* PutBackChar *) 
        TRY 
          DEC ( Pos ) 
        ; IF Pos < 0 
          THEN 
            Assert ( Pos = - 1 , AFT . A_M3Scanner_Scan_PutBackChar_LowPos ) 
          ; BackCh := Ch 
          ELSE 
            Assert 
              ( Strings . IthChar ( InString , Pos ) = Ch 
              , AFT . A_M3Scanner_Scan_PutBackChar_WrongChar 
              ) 
          END (* IF *) 
        EXCEPT Strings . SsOutOfBounds 
        => RAISE AssertionFailure ( "Strings.SsOutOfBounds" ) 
        END (* TRY EXCEPT *) 
      END PutBackChar 

  ; PROCEDURE EnsureOneChar ( ) 
    RAISES { AssertionFailure } 

    = BEGIN (* EnsureOneChar *) 
        TRY 
          IF Pos < 0 
          THEN 
            Assert ( Pos = - 1 , AFT . A_M3Scanner_Scan_EnsureOneChar_LowPos ) 
          ; Ch := BackCh 
          ELSE 
            WHILE Pos >= InLength 
            DO ScannerIf . ConsumeChars 
                 ( Sif 
                 , (* VAR *) State 
                 , InLength 
                 , (* VAR *) InString 
                 , (* VAR *) AreAllBlanks 
                 ) 
            ; InLength := Strings . Length ( InString ) 
            ; Pos := 0 
            END (* WHILE *) 
          ; Ch := Strings . IthChar ( InString , Pos ) 
          END (* IF *) 
        EXCEPT Strings . SsOutOfBounds 
        => RAISE AssertionFailure ( "Strings.SsOutOfBounds" ) 
        END (* TRY EXCEPT *) 
      END EnsureOneChar 

  (* NOTE: The invariant on when to call EnsureOneChar is inconsistent. 
           I don't want to call it prematurely when a lookahead 
           is not required. So it is generally called right before 
           a character is to be examined.  But something earlier 
           could have already examined the same character, meaning it 
           will already have been done for this character. 
           Fortunately, it is harmless to call it multiple times 
           without incrementing Pos. *) 

  ; PROCEDURE BegOfTok ( ) 
    RAISES { AssertionFailure } 

    = BEGIN (* BegOfTok *) 
        IF Pos > 0 
        THEN 
          ScannerIf . ConsumeChars 
            ( Sif 
            , (* VAR *) State 
            , Pos 
            , (* VAR *) InString 
            , (* VAR *) AreAllBlanks 
            ) 
        ; InLength := Strings . Length ( InString ) 
        ; Pos := 0 
        END (* IF *) 
      ; ScannerIf . NoteBeg ( Sif , State ) 
      END BegOfTok 

  ; PROCEDURE DeliverTok 
      ( Tok : LbeStd . TokTyp ; MakeIdle : BOOLEAN := TRUE ) 
    RAISES { AssertionFailure } 

    = BEGIN (* DeliverTok *) 
        ScannerIf . AccumSlice ( Sif , TokString ) 
      ; IF MakeIdle THEN State := LbeStd . SsIdle END (* IF *) 
      ; ScannerIf . DeliverTok 
          ( Sif , State , Pos , Tok , InString , AreAllBlanks ) 
      ; InLength := Strings . Length ( InString ) 
      ; Pos := 0 
      ; Strings . MakeEmpty ( TokString ) 
      END DeliverTok 

  ; PROCEDURE AppendAndDeliverTok ( Tok : LbeStd . TokTyp ) 
    RAISES { AssertionFailure } 

    = BEGIN (* AppendAndDeliverTok *) 
        Strings . AppendCharInPlace ( TokString , Ch ) 
      ; INC ( Pos ) 
      ; DeliverTok ( Tok ) 
      END AppendAndDeliverTok 

  ; PROCEDURE NextChar ( ) 
    RAISES { AssertionFailure } 

    = BEGIN (* NextChar *) 
        Strings . AppendCharInPlace ( TokString , Ch ) 
      ; INC ( Pos ) 
      ; State := LbeStd . SsInTok 
      ; EnsureOneChar ( ) 
      END NextChar 

  ; PROCEDURE LexErrorChars ( Code : LbeStd . ErrCodeTyp ) 
    RAISES { AssertionFailure } 

    = BEGIN (* LexErrorChars *) 
        State := LbeStd . SsInTok 
      ; EnsureOneChar ( ) 
      ; WHILE NOT Ch IN GoodChars DO NextChar ( ) END (* WHILE *) 
      ; ScannerIf . LexErr ( Sif , Code ) 
      ; DeliverTok ( LbeStd . Tok__LexErrChars ) 
      END LexErrorChars 

  ; PROCEDURE Ident ( ) 
    RAISES { AssertionFailure } 

    = VAR LIntTok : INTEGER 

    ; BEGIN (* Ident *) 
        BegOfTok ( ) 
      ; WHILE Ch 
              IN SET OF CHAR { 'a' .. 'z' , 'A' .. 'Z' , '_' , '0' .. '9' } 
        DO NextChar ( ) 
        END (* WHILE *) 
      ; IF RwTable . get ( Strings . ToText ( TokString ) , LIntTok ) 
        THEN 
          DeliverTok ( LIntTok ) 
        ELSE 
          DeliverTok ( M3Tok . Id ) 
        END (* IF *) 
      END Ident 

  ; PROCEDURE Placeholder ( ) 
    RAISES { AssertionFailure } 

    = VAR LIntTok : INTEGER 

    ; BEGIN (* Ident *) 
        BegOfTok ( ) 
      ; NextChar ( ) 
      ; WHILE Ch 
              IN SET OF CHAR { 'a' .. 'z' , 'A' .. 'Z' , '_' , '0' .. '9' } 
        DO NextChar ( ) 
        END (* WHILE *)
      ; IF Ch = LbeStd . RightPlaceholderDelimChar 
        THEN 
          NextChar ( ) 
        ELSE 
          ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedPlaceholder ) 
        ; Strings . AppendCharInPlace 
            ( TokString , LbeStd . RightPlaceholderDelimChar )  
        END (* IF *) 
      ; IF PhTable . get ( Strings . ToText ( TokString ) , LIntTok ) 
        THEN 
          DeliverTok ( LIntTok ) 
        ELSE 
          ScannerIf . LexErr ( Sif , LbeStd . LeUnknownPlaceholder ) 
        ; DeliverTok ( LbeStd . Tok__LexErrChars ) 
        END (* IF *) 
      END Placeholder  

  ; PROCEDURE Number ( ) 
    RAISES { AssertionFailure } 

    = BEGIN (* Number *) 
        BegOfTok ( ) 
      ; WHILE Ch IN SET OF CHAR { '0' .. '9' } 
        DO NextChar ( ) 
        END (* WHILE *)
      ; CASE Ch
        OF '_' 
        => NextChar ( ) 
        ; IF Ch IN SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' } 
          THEN 
            WHILE Ch IN SET OF CHAR { '0' .. '9' , 'A' .. 'F' , 'a' .. 'f' } 
            DO NextChar ( ) 
            END (* WHILE *) 
          ELSE 
            ScannerIf . LexErr ( Sif , LbeStd . LeNoHexDigit ) 
          END (* IF *) 
        ; DeliverTok ( M3Tok . Number ) 
        ; RETURN 
        | '.' 
        => INC ( Pos ) 
        ; EnsureOneChar ( ) 
        ; IF Ch = '.' 
          THEN PutBackChar ( '.' ) 
          ELSE
            Strings . AppendCharInPlace ( TokString , '.' ) 
          ; IF Ch IN SET OF CHAR { '0' .. '9' } 
            THEN 
              WHILE Ch IN SET OF CHAR { '0' .. '9' } 
              DO NextChar ( ) 
              END (* WHILE *) 
            ELSE 
              ScannerIf . LexErr ( Sif , LbeStd . LeNoFractionalDigit ) 
            END (* IF *) 
          END (* IF *) 
        ELSE 
        END  
      ; CASE Ch
        OF 'E' , 'e' , 'D' , 'd' , 'X' , 'x'  
        => NextChar ( ) 
        ; CASE Ch 
          OF '+' , '-' 
          => NextChar ( ) 
          ELSE 
          END 
        ; IF  Ch IN SET OF CHAR { '0' .. '9' } 
          THEN 
            WHILE Ch IN SET OF CHAR { '0' .. '9' } 
            DO NextChar ( ) 
            END (* WHILE *)
          ELSE 
            ScannerIf . LexErr ( Sif , LbeStd . LeNoExponentDigit ) 
          END (* IF *) 
        ELSE 
        END  
      ; DeliverTok ( M3Tok . Number ) 
      END Number 

  ; PROCEDURE String ( ) 
    RAISES { AssertionFailure } 

    = VAR LCount : PortTypes . Int32Typ 

    ; BEGIN (* String *) 
        BegOfTok ( ) 
      ; NextChar ( ) (* Consume the opening quote. *) 
      ; LOOP 
          CASE Ch 
          OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
          => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedString ) 
          ; Strings . AppendCharInPlace ( TokString , '"' ) 
          ; DeliverTok ( M3Tok . TextLit ) 
          ; EXIT 

          | '"' 
          => AppendAndDeliverTok ( M3Tok . TextLit ) 
          ; EXIT 

          | '\\' 
          => NextChar ( ) 
          ; CASE Ch 
            OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
            => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedString ) 
            ; Strings . AppendCharInPlace ( TokString , '"' ) 
            ; DeliverTok ( M3Tok . TextLit ) 
            ; EXIT 

            | '0' .. '7' 
            => LCount := 3 
            ; WHILE LCount > 0 AND Ch IN SET OF CHAR { '0' .. '7' } 
              DO NextChar ( ) 
              ; DEC ( LCount ) 
              END (* WHILE *) 
            ; IF LCount > 0 
              THEN 
                ScannerIf . LexErr ( Sif , LbeStd . LeBadOctalDigit ) 
              END (* IF *) 

            | '\t' , '\r' , '\f' , '\\' , '\'' , '\"' 
            => NextChar ( ) 

            ELSE 
              NextChar ( ) (* For now, just allow anything to be escaped. *) 
            END (* CASE *) 

          ELSE 
            NextChar ( ) 
          END (* CASE *) 
        END (* LOOP *) 
      END String 

  ; PROCEDURE CharLit ( ) 
    RAISES { AssertionFailure } 

    = VAR LCount : PortTypes . Int32Typ 

    ; BEGIN (* CharLit *) 
        BegOfTok ( ) 
      ; NextChar ( ) (* Consume the opening quote. *) 
      ; CASE Ch 
        OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
        => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedChar ) 
        ; Strings . AppendCharInPlace ( TokString , '\'' ) 
        ; DeliverTok ( M3Tok . CharLit ) 
        ; RETURN 

        | '\\' 
        => NextChar ( ) 
        ; CASE Ch 
          OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
          => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedChar ) 
          ; Strings . AppendCharInPlace ( TokString , '\'' ) 
          ; DeliverTok ( M3Tok . CharLit )
          ; RETURN  

          | '0' .. '7' 
          => LCount := 3 
          ; WHILE LCount > 0 AND Ch IN SET OF CHAR { '0' .. '7' } 
            DO NextChar ( ) 
            ; DEC ( LCount ) 
            END (* WHILE *) 
          ; IF LCount > 0 
            THEN 
              ScannerIf . LexErr ( Sif , LbeStd . LeBadOctalDigit ) 
            END (* IF *) 

          | '\t' , '\r' , '\f' , '\\' , '\'' , '\"' 
          => NextChar ( ) 

          ELSE 
            NextChar ( ) (* For now, just allow anything to be escaped. *) 
          END (* CASE *) 

        ELSE 
          NextChar ( ) 
        END (* CASE *) 
      ; IF Ch = '\'' 
        THEN  
          NextChar ( ) 
        ELSE 
          Strings . AppendCharInPlace ( TokString , '\'' ) 
        END (* IF *) 
      ; DeliverTok ( M3Tok . CharLit ) 
      END CharLit 

  ; PROCEDURE CommentSuffix ( ) 
    RAISES { AssertionFailure } 

    = BEGIN 
        SaveState := State 
      ; State := LbeStd . SsInTok 
      ; LOOP (* Thru chars in comment *) 
          (* INVARIANT: EnsureOneChar ( ) has been done since the last 
                        INC ( Pos ) *) 
          CASE Ch 
          OF LbeStd . CharEndOfImage 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Char, but do not append to token. *) 
          ; DeliverTok ( LbeStd . Tok__CmntAtEndOfLine , TRUE )
          ; EXIT 
          | LbeStd . CharNewLine 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Char, but do not append to token. *) 
          ; DeliverTok ( LbeStd . Tok__CmntAtEndOfLine , FALSE )
          ; EXIT 
          | '(' 
          => Strings . AppendCharInPlace ( TokString , '(' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF Ch = '*' 
            THEN 
              Strings . AppendCharInPlace ( TokString , '*' ) 
            ; INC ( SaveState , 2 ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            END (* IF *) 

          | '*' 
          => Strings . AppendCharInPlace ( TokString , '*' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF Ch = ')' 
            THEN 
              Strings . AppendCharInPlace ( TokString , ')' ) 
            ; INC ( Pos ) 
            ; IF SaveState = LbeStd . SsInCmnt 
              THEN 
                State := SaveState 
              ; DeliverTok ( LbeStd . Tok__Cmnt ) 
              ; EXIT 
              ELSE 
                DEC ( SaveState , 2 ) 
              ; EnsureOneChar ( ) 
              END (* IF *) 
            END (* IF *) 
          ELSE 
            Strings . AppendCharInPlace ( TokString , Ch ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          END (* CASE *) 
        END (* LOOP *) 
      END CommentSuffix 

  ; PROCEDURE PragmaSuffix ( ) 
    RAISES { AssertionFailure } 

    = BEGIN 
        SaveState := State 
      ; State := LbeStd . SsInTok 
      ; LOOP (* Thru chars in comment *) 
          (* INVARIANT: EnsureOneChar ( ) has been done since the last 
                        INC ( Pos ) *) 
          CASE Ch 
          OF LbeStd . CharEndOfImage 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Nl, but do not append to token. *) 
          ; DeliverTok ( LbeStd . Tok__CmntAtEndOfLine , TRUE )
          ; EXIT 
          | LbeStd . CharNewLine 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Nl, but do not append to token. *) 
          ; DeliverTok ( LbeStd . Tok__CmntAtEndOfLine , FALSE )
          ; EXIT 
          | '<' 
          => Strings . AppendCharInPlace ( TokString , '<' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF Ch = '*' 
            THEN 
              Strings . AppendCharInPlace ( TokString , '*' ) 
            ; INC ( SaveState , 2 ) 
            ; INC ( Pos ) 
            ; EnsureOneChar ( ) 
            END (* IF *) 

          | '*' 
          => Strings . AppendCharInPlace ( TokString , '*' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF Ch = '>' 
            THEN 
              Strings . AppendCharInPlace ( TokString , '>' ) 
            ; INC ( Pos ) 
            ; IF SaveState = LbeStd . SsInCmnt + 1 
              THEN 
                State := SaveState 
              ; DeliverTok ( LbeStd . Tok__Cmnt ) 
              ; EXIT 
              ELSE 
                DEC ( SaveState , 2 ) 
              ; EnsureOneChar ( ) 
              END (* IF *) 
            END (* IF *) 
          ELSE 
            Strings . AppendCharInPlace ( TokString , Ch ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          END (* CASE *) 
        END (* LOOP *) 
      END PragmaSuffix 

  ; BEGIN (* Scan *) 
      TRY 
        Sif := NARROW ( Cr , ScannerIf . ScanIfTyp ) 
      ; Strings . MakeEmpty ( TokString ) 
      ; ScannerIf . GetInitialChars 
          ( Sif 
          , (* VAR *) State 
          , (* VAR *) InString 
          , (* VAR *) AreAllBlanks 
          ) 
      ; InLength := Strings . Length ( InString ) 
      ; Pos := 0 
      ; LOOP (* Through tokens, not necessarily successive ones. *) 
          (* (Non-)INVARIANT: EnsureOneChar has not necessarily been done. *) 
          EnsureOneChar ( ) 
        ; IF Ch = LbeStd . CharNewLine 
          THEN 
            INC ( Pos )   
          ELSIF State >= LbeStd . SsInCmnt 
          THEN 
            BegOfTok ( ) 
          ; IF ( ( State - LbeStd . SsInCmnt ) MOD 2 ) = 0 
            THEN 
              CommentSuffix ( ) 
            ELSE 
              PragmaSuffix ( ) 
            END 
          ELSIF State = LbeStd . SsInTok 
          THEN 
            CantHappen ( AFT . A_M3Scanner_Scan_StateInToken ) 
          ELSE (* State = LbeStd . SsIdle *) 
            CASE Ch 
            OF ' ' , LbeStd . CharNewLine , LbeStd . CharTab 
            => INC ( Pos ) 
            | LbeStd . CharEndOfImage 
            => BegOfTok ( ) 
            ; DeliverTok ( LbeStd . Tok__EndOfImage ) 
            | 'a' .. 'z' , 'A' .. 'Z'   
            => Ident ( )

            | LbeStd . LeftPlaceholderDelimChar 
            => Placeholder ( )  

            | '0' .. '9' 
            => Number ( ) 

            | '"' 
            => String ( ) 

            | '\'' 
            => CharLit ( )  

            | '(' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '*' 
              THEN 
                Strings . AppendCharInPlace ( TokString , Ch ) 
              ; INC ( Pos ) 
              ; EnsureOneChar ( ) 
              ; State := LbeStd . SsInCmnt 
              ; CommentSuffix ( ) 
              ELSE 
                DeliverTok ( M3Tok . OpenParen_Tok ) 
              END (* IF *) 

            | '<' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; CASE Ch 
              OF  '=' 
              => AppendAndDeliverTok ( M3Tok . LessEqual_Tok ) 
              |  ':' 
              => AppendAndDeliverTok ( M3Tok . Subtype_Tok ) 
              |  '*' 
              => Strings . AppendCharInPlace ( TokString , Ch ) 
              ; INC ( Pos ) 
              ; EnsureOneChar ( ) 
              ; State := LbeStd . SsInCmnt + 1  
              ; PragmaSuffix ( ) 
              ELSE 
                DeliverTok ( M3Tok . Less_Tok ) 
              END (* CASE *) 

            | ':' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '=' 
              THEN 
                AppendAndDeliverTok ( M3Tok . Becomes_Tok ) 
              ELSE 
                DeliverTok ( M3Tok . Colon_Tok ) 
              END (* IF *) 

            | '.' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '.' 
              THEN 
                AppendAndDeliverTok ( M3Tok . Ellipsis_Tok ) 
              ELSE 
                DeliverTok ( M3Tok . Dot_Tok ) 
              END (* IF *) 

            | '=' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '>' 
              THEN 
                AppendAndDeliverTok ( M3Tok . Arrow_Tok ) 
              ELSE 
                DeliverTok ( M3Tok . Equal_Tok ) 
              END (* IF *) 

            | '>' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '=' 
              THEN 
                AppendAndDeliverTok ( M3Tok . GreaterEqual_Tok ) 
              ELSE 
                DeliverTok ( M3Tok . Greater_Tok ) 
              END (* IF *) 

            | '+' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Plus_Tok ) 
            | '-' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Minus_Tok ) 
            | '^' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Deref_Tok ) 
            | '#' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Unequal_Tok ) 
            | ';' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Semicolon_Tok ) 
            | '[' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . OpenBracket_Tok ) 
            | ']' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . CloseBracket_Tok ) 
            | '{' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . OpenBrace_Tok ) 
            | '}' 
            => BegOfTok ( ) 
             ; AppendAndDeliverTok ( M3Tok . CloseBrace_Tok ) 
            | ')' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . CloseParen_Tok ) 
            | ',' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Comma_Tok ) 
            | '&' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Ampersand_Tok ) 
            | '|' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Stroke_Tok ) 
            | '*' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Star_Tok ) 
            | '/' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( M3Tok . Slash_Tok ) 

            ELSE 
              BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; LexErrorChars ( LbeStd . LeBadChars ) 
            END (* CASE *) 
          END (* IF *) 
       END (* LOOP *) 
       EXCEPT AssertionFailure 
      => <* FATAL AssertionFailure *> 
        BEGIN 
          BegOfTok ( ) 
        ; DeliverTok ( LbeStd . Tok__Unknown ) 
        END (* Block *) 
      END (* TRY EXCEPT *) 
    END Scan 

; BEGIN (* M3Scanner *) 
    InitTables ( ) 
  ; LangUtil . RegisterScanner ( LbeStd . LangM3 , Scan ) 
  END M3Scanner 
. 
