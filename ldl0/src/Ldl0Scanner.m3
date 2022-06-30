
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Ldl0Scanner 

; IMPORT TextIntTbl 

; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout  
; IMPORT SchutzCoroutine 
; IMPORT LangUtil  
; IMPORT LbeStd 
; IMPORT Ldl0Tok 
; IMPORT MessageCodes 
; IMPORT PortTypes 
; IMPORT ScannerIf 
; IMPORT Strings 

; TYPE AFT = MessageCodes . T 

; CONST GoodChars 
    = SET OF CHAR 
        { LbeStd . CharEndOfImage , LbeStd . CharNewLine , ' ' , '.' , ':' 
        , ';' 
        , '[' , ']' , '{' , '}' , '+' , '|' , '-' , '_' , '!' , '@' , '(' 
        , ')' , 'a' .. 'z' , 'A' .. 'Z' , '0' .. '9' , '"' 
        } 

; VAR RwTable : TextIntTbl . Default 

; PROCEDURE InitRwTable ( ) 

  = BEGIN (* InitRwTable *) 
      RwTable := NEW ( TextIntTbl . Default ) . init ( sizeHint := 15 ) 
    ; EVAL RwTable . put ( "END" , Ldl0Tok . RwEND_Tok ) 
    ; EVAL RwTable . put ( "FILL" , Ldl0Tok . RwFILL_Tok ) 
    ; EVAL RwTable . put ( "HORIZ" , Ldl0Tok . RwHORIZ_Tok ) 
    ; EVAL RwTable . put ( "NONE" , Ldl0Tok . RwNONE_Tok ) 
    ; EVAL RwTable . put ( "NONEMPTY" , Ldl0Tok . RwNONEMPTY_Tok ) 
    ; EVAL RwTable . put ( "LEFT" , Ldl0Tok . RwLEFT_Tok ) 
    ; EVAL RwTable . put ( "LDL" , Ldl0Tok . RwLDL_Tok ) 
    ; EVAL RwTable . put ( "MEMBER" , Ldl0Tok . RwMEMBER_Tok ) 
    ; EVAL RwTable . put ( "PLURAL" , Ldl0Tok . RwPLURAL_Tok ) 
    ; EVAL RwTable . put ( "PREC" , Ldl0Tok . RwPREC_Tok ) 
    ; EVAL RwTable . put ( "PRESENT" , Ldl0Tok . RwPRESENT_Tok ) 
    ; EVAL RwTable . put ( "RIGHT" , Ldl0Tok . RwRIGHT_Tok ) 
    ; EVAL RwTable . put ( "START" , Ldl0Tok . RwSTART_Tok ) 
    ; EVAL RwTable . put ( "VARTERM" , Ldl0Tok . RwVARTERM_Tok ) 
    ; EVAL RwTable . put ( "VERT" , Ldl0Tok . RwVERT_Tok ) 
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
  ; PROCEDURE PutBackChar ( Ch : CHAR ) 
    RAISES { Backout } 

    = BEGIN (* PutBackChar *) 
        TRY 
          DEC ( Pos ) 
        ; IF Pos < 0 
          THEN 
            Assert ( Pos = - 1 , AFT . A_Ldl0Scanner_Scan_PutBackChar_LowPos ) 
          ; BackCh := Ch 
          ELSE 
            Assert 
              ( Strings . IthChar ( InString , Pos ) = Ch 
              , AFT . A_Ldl0Scanner_Scan_PutBackChar_WrongChar 
              ) 
          END (* IF *) 
        EXCEPT Strings . SsOutOfBounds 
        => RAISE Backout ( "Strings.SsOutOfBounds" ) 
        END (* TRY EXCEPT *) 
      END PutBackChar 

  ; PROCEDURE EnsureOneChar ( ) 
    RAISES { Backout } 

    = BEGIN (* EnsureOneChar *) 
        TRY 
          IF Pos < 0 
          THEN 
            Assert 
              ( Pos = - 1 , AFT . A_Ldl0Scanner_Scan_EnsureOneChar_LowPos ) 
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
        => RAISE Backout ( "Strings.SsOutOfBounds" ) 
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
    RAISES { Backout } 

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
    RAISES { Backout } 

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
    RAISES { Backout } 

    = BEGIN (* AppendAndDeliverTok *) 
        Strings . AppendCharInPlace ( TokString , Ch ) 
      ; INC ( Pos ) 
      ; DeliverTok ( Tok ) 
      END AppendAndDeliverTok 

  ; PROCEDURE NextChar ( ) 
    RAISES { Backout } 

    = BEGIN (* NextChar *) 
        Strings . AppendCharInPlace ( TokString , Ch ) 
      ; INC ( Pos ) 
      ; State := LbeStd . SsInTok 
      ; EnsureOneChar ( ) 
      END NextChar 

  ; PROCEDURE LexErrorChars ( Code : LbeStd . ErrCodeTyp ) 
    RAISES { Backout } 

    = BEGIN (* LexErrorChars *) 
        State := LbeStd . SsInTok 
      ; EnsureOneChar ( ) 
      ; WHILE NOT Ch IN GoodChars DO NextChar ( ) END (* WHILE *) 
      ; ScannerIf . LexErr ( Sif , Code ) 
      ; DeliverTok ( LbeStd . Tok__LexErrChars ) 
      END LexErrorChars 

  ; PROCEDURE Ident ( ) 
    RAISES { Backout } 

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
          DeliverTok ( Ldl0Tok . Ident ) 
        END (* IF *) 
      END Ident 

  ; PROCEDURE Integer ( ) 
    RAISES { Backout } 

    = BEGIN (* Integer *) 
        BegOfTok ( ) 
      ; WHILE Ch IN SET OF CHAR { '0' .. '9' } 
        DO NextChar ( ) 
        END (* WHILE *) 
      ; DeliverTok ( Ldl0Tok . Integer ) 
      END Integer 

  ; PROCEDURE String ( ) 
    RAISES { Backout } 

    = VAR LCount : PortTypes . Int32Typ 

    ; BEGIN (* String *) 
        BegOfTok ( ) 
      ; NextChar ( ) (* Consume the opening quote. *) 
      ; LOOP 
          CASE Ch 
          OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
          => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedString ) 
          ; Strings . AppendCharInPlace ( TokString , '"' ) 
          ; DeliverTok ( Ldl0Tok . String ) 
          ; EXIT 

          | '"' 
          => AppendAndDeliverTok ( Ldl0Tok . String ) 
          ; EXIT 

          | '\\' 
          => NextChar ( ) 
          ; CASE Ch 
            OF LbeStd . CharEndOfImage , LbeStd . CharNewLine 
            => ScannerIf . LexErr ( Sif , LbeStd . LeUnclosedString ) 
            ; Strings . AppendCharInPlace ( TokString , '\\' ) 
            ; Strings . AppendCharInPlace ( TokString , '"' ) 
            ; DeliverTok ( Ldl0Tok . String ) 
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
    RAISES { Backout } 

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
            CantHappen ( AFT . A_Ldl0Scanner_Scan_StateInToken ) 
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
                DeliverTok ( Ldl0Tok . OpenParen_Tok ) 
              END (* IF *) 

            | '}' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '+' 
              THEN 
                AppendAndDeliverTok ( Ldl0Tok . CloseBracePlus_Tok ) 
              ELSE 
                DeliverTok ( Ldl0Tok . CloseBrace_Tok ) 
              END (* IF *) 

            | '|' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '|' 
              THEN 
                AppendAndDeliverTok ( Ldl0Tok . DoubleStroke_Tok ) 
              ELSE 
                DeliverTok ( Ldl0Tok . Stroke_Tok ) 
              END (* IF *) 

            | '-' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = '>' 
              THEN 
                AppendAndDeliverTok ( Ldl0Tok . Arrow_Tok ) 
              ELSE 
                LexErrorChars ( LbeStd . LeBadChars ) 
              END (* IF *) 

            | ':' 
            => BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; State := LbeStd . SsInTok 
            ; EnsureOneChar ( ) 
            ; IF Ch = ':' 
              THEN 
                INC ( Pos ) 
              ; EnsureOneChar ( ) 
              ; IF Ch = '=' 
                THEN 
                  Strings . AppendCharInPlace ( TokString , ':' ) 
                ; AppendAndDeliverTok ( Ldl0Tok . ColonColonEquals_Tok ) 
                ELSE 
                  DeliverTok ( Ldl0Tok . Colon_Tok ) 
                ; PutBackChar ( ':' ) 
                END (* IF *) 
              ELSE 
                IF Ch = '=' 
                THEN 
                  AppendAndDeliverTok ( Ldl0Tok . ColonEquals_Tok ) 
                ELSE 
                  DeliverTok ( Ldl0Tok . Colon_Tok ) 
                END (* IF *) 
              END (* IF *) 

            | '.' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl0Tok . Dot_Tok ) 
            | ';' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl0Tok . Semicolon_Tok ) 
            | '[' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl0Tok . OpenBracket_Tok ) 
            | ']' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl0Tok . CloseBracket_Tok ) 
            | '{' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl0Tok . OpenBrace_Tok ) 
            | '=' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl0Tok . Equals_Tok ) 
            | '_' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl0Tok . Underscore_Tok ) 
            | '!' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl0Tok . Bang_Tok ) 
            | '@' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl0Tok . At_Tok ) 
            | ')' 
            => BegOfTok ( ) 
            ; AppendAndDeliverTok ( Ldl0Tok . CloseParen_Tok ) 

            ELSE 
              BegOfTok ( ) 
            ; Strings . AppendCharInPlace ( TokString , Ch ) 
            ; INC ( Pos ) 
            ; LexErrorChars ( LbeStd . LeBadChars ) 
            END (* CASE *) 
          END (* IF *) 
        END (* LOOP *) 
      EXCEPT Backout 
      => <* FATAL Backout *> 
        BEGIN 
          BegOfTok ( ) 
        ; DeliverTok ( LbeStd . Tok__Unknown ) 
        END (* Block *) 
      END (* TRY EXCEPT *) 
    END Scan 

; BEGIN (* Ldl0Scanner *) 
    InitRwTable ( ) 
  ; LangUtil . RegisterScanner ( LbeStd . LangLdl0 , Scan ) 
  END Ldl0Scanner 
. 
