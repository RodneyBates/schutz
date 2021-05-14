
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Ldl1Scanner 

; IMPORT TextIntTbl 

; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 
; IMPORT SchutzCoroutine 
; IMPORT LangUtil  
; IMPORT LbeStd 
; IMPORT Ldl1Tok 
; IMPORT MessageCodes 
; IMPORT PortTypes 
; IMPORT ScannerIf 
; IMPORT Strings 
; TYPE AFT = MessageCodes . T 

; CONST GoodChars 
    = SET OF CHAR 
        { LbeStd . CharEndOfImage , LbeStd . CharNewLine , ' ' , '.' , ':' 
        , ';' , '^' , '$' , ',' , '=' , '*' , '>' 
        , '[' , ']' , '{' , '}' , '+' , '|' , '-' , '_' , '!' , '@' , '(' 
        , ')' , 'a' .. 'z' , 'A' .. 'Z' , '0' .. '9' , '"' 
        } 

; VAR RwTable : TextIntTbl . Default 

; PROCEDURE InitRwTable ( ) 

  = BEGIN (* InitRwTable *) 
      RwTable := NEW ( TextIntTbl . Default ) . init ( sizeHint := 15 ) 
    ; EVAL RwTable . put ( "END" , Ldl1Tok . RwEND_Tok ) 
    ; EVAL RwTable . put ( "FILL" , Ldl1Tok . RwFILL_Tok ) 
    ; EVAL RwTable . put ( "HORIZ" , Ldl1Tok . RwHORIZ_Tok ) 
    ; EVAL RwTable . put ( "NONE" , Ldl1Tok . RwNONE_Tok ) 
    ; EVAL RwTable . put ( "NONEMPTY" , Ldl1Tok . RwNONEMPTY_Tok ) 
    ; EVAL RwTable . put ( "LEFT" , Ldl1Tok . RwLEFT_Tok ) 
    ; EVAL RwTable . put ( "LDL" , Ldl1Tok . RwLDL_Tok ) 
    ; EVAL RwTable . put ( "MEMBER" , Ldl1Tok . RwMEMBER_Tok ) 
    ; EVAL RwTable . put ( "PLURAL" , Ldl1Tok . RwPLURAL_Tok ) 
    ; EVAL RwTable . put ( "NONPLURAL" , Ldl1Tok . RwNONPLURAL_Tok ) 
    ; EVAL RwTable . put ( "PREC" , Ldl1Tok . RwPREC_Tok ) 
    ; EVAL RwTable . put ( "PRESENT" , Ldl1Tok . RwPRESENT_Tok ) 
    ; EVAL RwTable . put ( "RIGHT" , Ldl1Tok . RwRIGHT_Tok ) 
    ; EVAL RwTable . put ( "START" , Ldl1Tok . RwSTART_Tok ) 
    ; EVAL RwTable . put ( "VARTERM" , Ldl1Tok . RwVARTERM_Tok ) 
    ; EVAL RwTable . put ( "VERT" , Ldl1Tok . RwVERT_Tok ) 
    ; EVAL RwTable . put ( "BUILD" , Ldl1Tok . RwBUILD_Tok ) 
    ; EVAL RwTable . put ( "CASE" , Ldl1Tok . RwCASE_Tok ) 
    ; EVAL RwTable . put ( "OF" , Ldl1Tok . RwOF_Tok ) 
    ; EVAL RwTable . put ( "ELSE" , Ldl1Tok . RwELSE_Tok ) 
    ; EVAL RwTable . put ( "ABSENT" , Ldl1Tok . RwABSENT_Tok ) 
    ; EVAL RwTable . put ( "EMPTY" , Ldl1Tok . RwEMPTY_Tok ) 
    ; EVAL RwTable . put ( "VERSION" , Ldl1Tok . RwVERSION_Tok ) 
    ; EVAL RwTable . put ( "NIL" , Ldl1Tok . RwNIL_Tok ) 
    ; EVAL RwTable . put ( "INDENT" , Ldl1Tok . RwINDENT_Tok ) 
    ; EVAL RwTable . put ( "INLINE" , Ldl1Tok . RwINLINE_Tok ) 
    ; EVAL RwTable . put ( "SUFFIXES" , Ldl1Tok . RwSUFFIXES_Tok ) 
    ; EVAL RwTable . put ( "NOPARSE" , Ldl1Tok . RwNOPARSE_Tok ) 
    END InitRwTable 

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
  ; <* UNUSED *> PROCEDURE PutBackChar ( Ch : CHAR ) 
    RAISES { AssertionFailure } 

    = BEGIN (* PutBackChar *) 
        TRY 
          DEC ( Pos ) 
        ; IF Pos < 0 
          THEN 
            Assert ( Pos = - 1 , AFT . A_Ldl1Scanner_Scan_PutBackChar_LowPos ) 
          ; BackCh := Ch 
          ELSE 
            Assert 
              ( Strings . IthChar ( InString , Pos ) = Ch 
              , AFT . A_Ldl1Scanner_Scan_PutBackChar_WrongChar 
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
            Assert ( Pos = - 1 , AFT . A_Ldl1Scanner_Scan_EnsureOneChar_LowPos ) 
          ; Ch := BackCh 
          ELSE 
            WHILE Pos >= InLength 
            DO ScannerIf . ConsumeChars 
                 ( Sif , State , InLength , InString , AreAllBlanks ) 
            ; InLength := Strings . Length ( InString ) 
            ; Pos := 0 
            END (* WHILE *) 
          ; Ch := Strings . IthChar ( InString , Pos ) 
          END (* IF *) 
        EXCEPT Strings . SsOutOfBounds 
        => RAISE AssertionFailure ( "Strings.SsOutOfBounds" ) 
        END (* TRY EXCEPT *) 
      END EnsureOneChar 

  (* NOTE: The invariant on EnsureOneChar is inconsistent. 
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
            ( Sif , State , Pos , InString , AreAllBlanks ) 
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
          DeliverTok ( Ldl1Tok . Ident ) 
        END (* IF *) 
      END Ident 

  ; PROCEDURE Integer ( ) 
    RAISES { AssertionFailure } 

    = BEGIN (* Integer *) 
        BegOfTok ( ) 
      ; WHILE Ch IN SET OF CHAR { '0' .. '9' } 
        DO NextChar ( ) 
        END (* WHILE *) 
      ; DeliverTok ( Ldl1Tok . Integer ) 
      END Integer 

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
          ; DeliverTok ( Ldl1Tok . String ) 
          ; EXIT 

          | '"' 
          => AppendAndDeliverTok ( Ldl1Tok . String ) 
          ; EXIT 

          | '\\' 
          => NextChar ( ) 
          ; CASE Ch 
            OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
            => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedString ) 
            ; Strings . AppendCharInPlace ( TokString , '\\' ) 
            ; Strings . AppendCharInPlace ( TokString , '"' ) 
            ; DeliverTok ( Ldl1Tok . String ) 
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

  ; PROCEDURE CommentSuffix ( ) 
    RAISES { AssertionFailure } 

    = BEGIN 
        SaveState := State 
      ; State := LbeStd . SsInTok 
      ; LOOP (* Thru chars in comment *) 
          (* INVARIANT: EnsureOneChar ( ) has been done since the last 
                        INC ( Pos ) *) 
          CASE Ch 
          OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
          => State := SaveState 
          ; INC ( Pos )  (* Consume the Nl, but do not append to token. *) 
          ; DeliverTok ( LbeStd . Tok__CmntAtEndOfLine , FALSE )
          ; EXIT 
          | '(' 
          => Strings . AppendCharInPlace ( TokString , '(' ) 
          ; INC ( Pos ) 
          ; EnsureOneChar ( ) 
          ; IF Ch = '*' 
            THEN 
              Strings . AppendCharInPlace ( TokString , '*' ) 
            ; INC ( SaveState ) 
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
                DEC ( SaveState ) 
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

  ; BEGIN (* Scan *) 
      TRY 
        Sif := Cr (* Implicit NARROW can't fail. *) 
      ; Strings . MakeEmpty ( TokString ) 
      ; ScannerIf . GetInitialChars ( Sif , State , InString , AreAllBlanks ) 
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
          ; CommentSuffix ( ) 
          ELSIF State = LbeStd . SsInTok 
          THEN 
            CantHappen ( AFT . A_Ldl1Scanner_Scan_StateInToken ) 
          ELSE (* State = LbeStd . SsIdle *) 
            CASE Ch 
            OF ' ' , LbeStd . CharNewLine , LbeStd . CharTab 
            => INC ( Pos ) 
            | LbeStd . CharEndOfImage 
            => BegOfTok ( ) 
            ; DeliverTok ( LbeStd . Tok__EndOfImage ) 
            | 'a' .. 'z' , 'A' .. 'Z'   
            => Ident ( ) 

            | '0' .. '9' 
            => Integer ( ) 

            | '"' 
            => String ( ) 

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
                DeliverTok ( Ldl1Tok . OpenParen_Tok ) 
              END (* IF *) 

            | '}' 
            => BegOfTok ( ) 
             ; AppendAndDeliverTok ( Ldl1Tok . CloseBrace_Tok ) 

            | '|' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '|' 
              THEN 
                Strings . AppendCharInPlace ( TokString , Ch ) 
              ; INC ( Pos ) 
              ; EnsureOneChar ( ) 
              ; IF Ch = '|' 
                THEN 
                  AppendAndDeliverTok ( Ldl1Tok . TripleStroke_Tok ) 
                ELSE 
                  DeliverTok ( Ldl1Tok . DoubleStroke_Tok ) 
                END (* IF *) 
              ELSE 
                DeliverTok ( Ldl1Tok . Stroke_Tok ) 
              END (* IF *) 

            | '-' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '>' 
              THEN 
                Strings . AppendCharInPlace ( TokString , Ch ) 
              ; INC ( Pos ) 
              ; EnsureOneChar ( ) 
              ; CASE Ch 
                OF '*' 
                => AppendAndDeliverTok ( Ldl1Tok . ArrowStar_Tok ) 
                ELSE 
                  DeliverTok ( Ldl1Tok . Arrow_Tok ) 
                END (* CASE *) 
              ELSE 
                DeliverTok ( Ldl1Tok . Minus_Tok ) 
              END (* IF *) 

            | ':' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = ':' 
              THEN 
                Strings . AppendCharInPlace ( TokString , Ch ) 
              ; INC ( Pos ) 
              ; EnsureOneChar ( ) 
              ; CASE Ch 
                OF '=' 
                => Strings . AppendCharInPlace ( TokString , Ch ) 
                ; INC ( Pos ) 
                ; EnsureOneChar ( ) 
                ; CASE Ch
                  OF '+' 
                  => Strings . AppendCharInPlace ( TokString , Ch ) 
                  ; INC ( Pos ) 
                  ; EnsureOneChar ( ) 
                  ; CASE Ch
                    OF '+' 
                    => AppendAndDeliverTok 
                         ( Ldl1Tok . ColonColonEqualsPlusPlus_Tok ) 
                    ELSE 
                      DeliverTok ( Ldl1Tok . ColonColonEqualsPlus_Tok ) 
                    END (* CASE *) 
                  | '*' 
                  => AppendAndDeliverTok ( Ldl1Tok . ColonColonEqualsStar_Tok ) 
                  ELSE 
                    DeliverTok ( Ldl1Tok . ColonColonEquals_Tok ) 
                  END (* CASE *) 
                | '*'
                => AppendAndDeliverTok ( Ldl1Tok . ColonColonStar_Tok ) 
                | '+'
                => AppendAndDeliverTok ( Ldl1Tok . ColonColonPlus_Tok ) 
                ELSE 
                  DeliverTok ( Ldl1Tok . ColonColon_Tok ) 
                END (* CASE *) 
              ELSE 
                DeliverTok ( Ldl1Tok . Colon_Tok ) 
              END (* IF *) 

            | '+' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . Plus_Tok ) 
            | '^' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . Circumflex_Tok ) 
            | '$' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . Dollar_Tok ) 
            | '.' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . Dot_Tok ) 
            | ';' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . Semicolon_Tok ) 
            | '[' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . OpenBracket_Tok ) 
            | ']' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . CloseBracket_Tok ) 
            | '{' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . OpenBrace_Tok ) 
            | '=' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . Equals_Tok ) 
            | '_' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . Underscore_Tok ) 
            | '!' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . Bang_Tok ) 
            | '@' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . At_Tok ) 
            | ')' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . CloseParen_Tok ) 
            | ',' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl1Tok . Comma_Tok ) 

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

; BEGIN (* Ldl1Scanner *) 
    InitRwTable ( ) 
  ; LangUtil . RegisterScanner ( LbeStd . LangLdl1 , Scan ) 
  END Ldl1Scanner 
. 
