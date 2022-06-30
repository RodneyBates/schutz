
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* "File Strings.m3". Rodney M. Bates.  Sep. 1997 *) 

MODULE Strings EXPORTS Strings 

(* A string package, implemented as mutable in place, thus tailored to certain 
   usage patterns. 
*) 

; IMPORT Text 

; IMPORT MessageCodes 
; IMPORT Assertions 
; FROM Failures IMPORT Backout 

; FROM Assertions IMPORT Assert 

; TYPE AFT = MessageCodes . T 

(* TOTO: After changing this to compile in cm3, it has gotten pretty
         ridiculous.  Rework it somehow. *) 
(* Representation rules: 
 
   A Strings.T is a mutable string.  It is designed to efficiently 
   support the expected usage pattern where lots of substrings of 
   Text.Ts will be passed around, maybe substringed again, and 
   many of them eventually will be concatenated, left to right into 
   a new string.  This might also be converted to at Text.T, after 
   it is finished.  Procedures which might need to allocate a 
   space which could be later expanded by concatenation accept an 
   optional hint parameter for the length the string could grow to. 
 
   INTERFACE/MODULE Strings provides a more or less abstract 
   general string manipulation interface, while supporting this 
   typical usage pattern.  It does this primarily by lazy substringing 
   (just saving the original string and the range of subscripts to it), 
   and doing the real substring extraction when concatenating. 
 
   It keeps things as a Text.T until an operation well suited to changing 
   a string in place is done.  Then it switches to using a SpaceTyp, 
   which is the same Modula-3 type, but these are never shared and are 
   therefore mutable. 
 
   A Strings.T may contain either a Text.T, a SpaceTyp, or neither, 
   or both. 
 
   Invariants: 
 
   - The length of the represented string = ToSs - FromSs 
 
   - Text = NIL AND Space = NIL IMPLIES ToSs = FromSs = 0 
     ( i.e. empty string ) 
 
   - IF Text # NIL, 
     THEN Text.Sub ( Text , FromSs , ToSs - FromSs ) 
          is the represented string, as a Text.T 
 
   - IF Space # NIL 
     THEN FromSs = 0 
          AND SUBARRAY ( Space ^ , FromSs , ToSs ) 
              is the represented string. 
 
   - There is not a trailing '\000' in Space ^ 
 
   - EventualLengthHint is the largest value ever received for this string. 
 
*) 

(* VISIBLE: *) 
; PROCEDURE FromText 
    ( Txt : TEXT ; EventualLengthHint : StringSsTyp := 0 ) : T 

  = VAR Result : T 

  ; BEGIN (* FromText *) 
      IF Txt = NIL THEN Txt := "" END 
    ; Result . Text := Txt 
    ; Result . FromSs := 0 
    ; Result . ToSs := Text . Length ( Txt ) 
    ; Result . Space := NIL 
    ; Result . EventualLengthHint := EventualLengthHint 
    ; RETURN Result 
    END FromText 

; PROCEDURE ForceToHaveExactText 
    ( VAR String : T ) RAISES { Backout } 

  (* Force to have a TEXT, whose entire contents are the 
     represented string. *) 

  = VAR LLength := String . ToSs - String . FromSs 

  ; BEGIN (* ForceToHaveExactText *) 
      IF LLength = 0 
      THEN 
        IF String . Text = NIL THEN String . Text := "" END (* IF *) 
      ELSE 
        IF String . Text = NIL 
        THEN 
          Assert 
            ( String . FromSs = 0 , AFT . A_ForceToHaveExactText_BadFromSs ) 
        ; String . Text 
            := Text . FromChars ( SUBARRAY ( String . Space ^ , 0 , LLength ) ) 
        ELSIF String . FromSs = 0 
        THEN 
          IF String . ToSs < Text . Length ( String . Text ) 
          THEN 
            String . Text := Text . Sub ( String . Text , 0 , String . ToSs ) 
            (* But Space is OK as is. *) 
       (* ELSE Text is already exactly the string represented. *) 
          END (* IF *) 
        ELSE 
          Assert 
            ( String . Space = NIL , AFT . A_ForceToHaveExactText_NonNILSpace ) 
        ; String . Text 
            := Text . Sub ( String . Text , String . FromSs , LLength ) 
        ; String . FromSs := 0 
        ; String . ToSs := LLength 
        END (* IF *) 
      END (* IF *) 
    END ForceToHaveExactText 

(* VISIBLE: *) 
; PROCEDURE ToText 
    ( String : T ; From : StringSsTyp := 0 ) : TEXT 
    RAISES { Backout } 

  = BEGIN (* ToText *) 
      IF From = 0 
      THEN 
        ForceToHaveExactText ( String ) 
      ; RETURN String . Text 
      ELSE 
        IF String . Text # NIL 
        THEN 
          RETURN Text . Sub ( String . Text , From ) 
        ELSE 
          Assert ( String . FromSs = 0 , AFT . A_Substring_BadFromSs ) 
        ; RETURN 
            Text . FromChars 
              ( SUBARRAY ( String . Space ^ , From , String . ToSs - From ) ) 
        END (* IF *) 
      END (* IF *) 
    END ToText 

(* VISIBLE: *) 
; PROCEDURE ToTextNonNIL 
    ( String : T ; From : StringSsTyp := 0 ) : TEXT 
    RAISES { Backout } 

  = VAR LResult : TEXT 

  ; BEGIN (* ToTextNonNIL *) 
      LResult := ToText ( String , From ) 
    ; IF LResult = NIL 
      THEN RETURN "" 
      ELSE RETURN LResult
      END (*( IF *) 
    END ToTextNonNIL

(* VISIBLE: *) 
; PROCEDURE FromChar 
    ( Ch : CHAR ; EventualLengthHint : StringSsTyp := 0 ) : T 

  = VAR Result : T 

  ; BEGIN (* FromChar *) 
      Result . EventualLengthHint := EventualLengthHint 
    ; IF EventualLengthHint > 1 
      THEN 
        Result . Space := NEW ( SpaceTyp , EventualLengthHint ) 
      ; Result . Space ^ [ 0 ] := Ch 
      ; Result . Text := NIL 
      ELSE 
        Result . Text := Text . FromChar ( Ch ) 
      ; Result . Space := NIL 
      END (* IF *) 
    ; Result . FromSs := 0 
    ; Result . ToSs := 1 
    ; RETURN Result 
    END FromChar 

(* VISIBLE: *) 
; PROCEDURE FromChars 
    ( READONLY Chars : ARRAY OF CHAR 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    : T 

  = VAR LLength := NUMBER ( Chars ) 
  ; VAR Result : T 

  ; BEGIN (* FromChars *) 
      Result . EventualLengthHint := EventualLengthHint 
    ; IF EventualLengthHint > LLength 
      THEN 
        Result . Space := NEW ( SpaceTyp , EventualLengthHint ) 
      ; SUBARRAY ( Result . Space ^ , 0 , LLength ) := Chars 
      ; Result . Text := NIL 
      ELSE 
        Result . Text := Text . FromChars ( Chars ) 
      ; Result . Space := NIL 
      END (* IF *) 
    ; Result . FromSs := 0 
    ; Result . ToSs := LLength 
    ; RETURN Result 
    END FromChars 

(* VISIBLE: *) 
; PROCEDURE FetchChars 
    ( VAR Chars : ARRAY OF CHAR ; String : T ) 
    RAISES { Backout } 
  (* Truncate or leave suffix of Chars unchanged if lengths are # *) 

  = VAR LLength := MIN ( String . FromSs - String . ToSs , NUMBER ( Chars ) ) 

  ; BEGIN (* FetchChars *) 
      IF String . Space # NIL 
      THEN 
        Assert ( String . FromSs = 0 , AFT . A_FetchChars_BadFromSs ) 
      ; SUBARRAY ( Chars , 0 , LLength ) 
          := SUBARRAY ( String . Space ^ , 0 , LLength ) 
      ELSIF String . Text # NIL 
      THEN 
        FOR RI := 0 TO LLength 
        DO Chars [ RI ] 
           := Text . GetChar ( String . Text , String . FromSs + RI ) 
        END (* FOR *) 
   (* ELSE do nothing, as this String is empty. *) 
      END (* IF *) 
    END FetchChars 

(* VISIBLE: *) 
; PROCEDURE IthChar 
    ( String : T ; I : StringSsTyp ) : CHAR RAISES { SsOutOfBounds } 

  = VAR LSs := I + String . FromSs 

  ; BEGIN (* IthChar *) 
      IF I < 0 OR LSs >= String . ToSs 
      THEN 
        RAISE SsOutOfBounds 
      ELSE 
        IF String . Space # NIL 
        THEN 
          RETURN String . Space ^ [ LSs ] 
        ELSE 
          RETURN Text . GetChar ( String . Text , LSs ) 
        END (* IF *) 
      END (* IF *) 
    END IthChar 

; PROCEDURE MergeEventualLengthHint 
    ( VAR String : T ; EventualLengthHint : StringSsTyp := 0 ) 

  = BEGIN (* MergeEventualLengthHint *) 
      String . EventualLengthHint 
        := MAX ( String . EventualLengthHint , EventualLengthHint ) 
    END MergeEventualLengthHint 

; PROCEDURE ForceToHaveSpaceOnly 
    ( VAR String : T ) RAISES { Backout } 

  (* AND, Space to have MAX ( EventualLengthHint , Length ) CHARs *) 

  = VAR LLength := String . ToSs - String . FromSs 
  ; VAR LNewSpace : SpaceTyp 

  ; BEGIN (* ForceToHaveSpaceOnly *) 
      IF String . Space = NIL 
      THEN 
        MergeEventualLengthHint ( String , LLength ) 
      ; String . Space := NEW ( SpaceTyp , String . EventualLengthHint ) 
      ; IF String . Text # NIL 
        THEN 
          FOR RI := 0 TO LLength - 1  
          DO String . Space ^ [ RI ] 
               := Text . GetChar ( String . Text , String . FromSs + RI ) 
          END (* FOR *) 
        ; String . FromSs := 0 
        ; String . ToSs := LLength 
        ; String . Text := NIL 
        END (* IF *) 
      ELSE 
        Assert ( String . FromSs = 0 , AFT . A_ForceToHaveSpaceOnly_BadFromSs ) 
      ; IF NUMBER ( String . Space ^ ) < String . EventualLengthHint 
        THEN (* Space # NIL AND it is too short *) 
          LNewSpace := NEW ( SpaceTyp , String . EventualLengthHint ) 
        ; SUBARRAY ( LNewSpace ^ , 0 , LLength ) 
            := SUBARRAY ( String . Space ^ , 0 , LLength ) 
        ; String . Space := LNewSpace 
        ; String . ToSs := LLength 
        END (* IF *) 
      END (* IF *) 
    ; String . Text := NIL 
    END ForceToHaveSpaceOnly 

(* VISIBLE: *) 
; PROCEDURE StoreIthChar 
    ( VAR String : T 
    ; I : StringSsTyp 
    ; Ch : CHAR 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    RAISES { SsOutOfBounds , Backout } 

  = BEGIN (* StoreIthChar *) 
      IF I < 0 OR I + String . FromSs >= String . ToSs 
      THEN 
        RAISE SsOutOfBounds 
      ELSE 
        MergeEventualLengthHint ( String , EventualLengthHint ) 
      ; ForceToHaveSpaceOnly ( String ) 
      ; String . Space ^ [ I ] := Ch 
      END (* IF *) 
    END StoreIthChar 

(* VISIBLE: *) 
; PROCEDURE AppendInPlace 
    ( VAR (*IN OUT *) Left : T 
    ; Right : T 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    RAISES { Backout } 

  = VAR LLeftLength , LRightLength , LResultLength : StringSsTyp 

  ; BEGIN (* AppendInPlace *) 
      LRightLength := Right . ToSs - Right . FromSs 
    ; MergeEventualLengthHint ( Left , EventualLengthHint ) 
    ; IF LRightLength > 0 
      THEN 
        LLeftLength := Left . ToSs - Left . FromSs 
      ; LResultLength := LLeftLength + LRightLength 
      ; MergeEventualLengthHint ( Left , LResultLength ) 
      ; ForceToHaveSpaceOnly ( Left ) 
      ; IF Right . Space # NIL 
        THEN 
          SUBARRAY ( Left . Space ^ , Left . ToSs , LRightLength ) 
            := SUBARRAY ( Right . Space ^ , Right . FromSs , LRightLength ) 
        ELSE 
          FOR RI := 0 TO LRightLength - 1 
          DO Left . Space ^ [ Left . ToSs + RI ] 
               := Text . GetChar ( Right . Text , Right . FromSs + RI )  
          END (* FOR *) 
        END (* IF *) 
      ; Left . ToSs := Left . FromSs + LResultLength 
      END (* IF *) 
    END AppendInPlace 

(* VISIBLE: *) 
; PROCEDURE AppendTextInPlace 
    ( VAR (*IN OUT *) Left : T 
    ; Right : TEXT (* Can be NIL *) 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    RAISES { Backout } 

  = VAR LLeftLength , LRightLength , LResultLength : StringSsTyp 

  ; BEGIN (* AppendTextInPlace *) 
      IF Right # NIL 
      THEN 
        LRightLength := Text . Length ( Right ) 
      ; MergeEventualLengthHint ( Left , EventualLengthHint ) 
      ; IF LRightLength > 0 
        THEN 
          LLeftLength := Left . ToSs - Left . FromSs 
        ; LResultLength := LLeftLength + LRightLength 
        ; MergeEventualLengthHint ( Left , LResultLength ) 
        ; ForceToHaveSpaceOnly ( Left ) 
        ; FOR RI := 0 TO Text . Length ( Right ) - 1 
          DO 
            Left . Space ^ [ Left . ToSs + RI ] 
              := Text . GetChar ( Right , RI ) 
          END (* FOR *) 
        ; Left . ToSs := Left . FromSs + LResultLength 
        END (* IF *) 
      END (* IF *) 
    END AppendTextInPlace 

(* VISIBLE: *) 
; PROCEDURE AppendCharInPlace 
    ( VAR (*IN OUT *) Left : T 
    ; Ch : CHAR 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    RAISES { Backout } 

  = BEGIN (* AppendCharInPlace *) 
      MergeEventualLengthHint ( Left , EventualLengthHint ) 
    ; MergeEventualLengthHint ( Left , Left . ToSs - Left . FromSs + 1 ) 
    ; ForceToHaveSpaceOnly ( Left ) 
    ; Left . Space ^ [ Left . ToSs ] := Ch 
    ; INC ( Left . ToSs ) 
    END AppendCharInPlace 

(* VISIBLE: *) 
; PROCEDURE MakeEmpty ( VAR String : T ) 

  = BEGIN (* MakeEmpty *) 
      String . FromSs := 0 
    ; String . ToSs := 0 
    ; String . Text := NIL 
    END MakeEmpty 

(* VISIBLE: *) 
; PROCEDURE Empty ( EventualLengthHint : StringSsTyp := 0 ) : T 

  = VAR Result : T 

  ; BEGIN (* Empty *) 
      Result . Text := NIL 
    ; Result . FromSs := 0 
    ; Result . ToSs := 0 
    ; Result . Space := NIL 
    ; Result . EventualLengthHint := EventualLengthHint 
    ; RETURN Result 
    END Empty 

(* VISIBLE: *) 
; PROCEDURE TruncateInPlace 
    ( VAR (* IN OUT *)  String : T ; ToLength : StringSsTyp ) 
    RAISES { Backout } 
  (* NOOP if ToLength > Length ( String *) 

  = BEGIN (* TruncateInPlace *) 

      ForceToHaveSpaceOnly ( String ) 
    ; Assert ( String . FromSs = 0 , AFT . A_Truncate_BadFromSs ) 
    ; String . ToSs := MAX ( 0 , MIN ( String . ToSs , ToLength ) ) 

(* 
 
      IF ToLength <= 0 
      THEN 
        String . Text := NIL 
      ; String . FromSs := 0 
      ; String . ToSs := 0 
      (* Keep Space around and rememver EventualLengthHint *) 
      ELSIF ToLength < String . ToSs - String . FromSs 
      THEN (* Must properly shorten, but not to empty *) 
        ForceToHaveSpaceOnly ( String ) 
      ; Assert 
          ( String . FromSs = 0 
          , AFT . A_TruncateInPlace_BadFromSs 
          ) 
      ; String . ToSs := ToLength 
      END (* IF *) 
*) 
    END TruncateInPlace 

(* VISIBLE: *) 
; PROCEDURE LeftTruncateInPlace 
    ( VAR (* IN OUT *) String : T ; ToLength : StringSsTyp ) 
    RAISES { Backout } 
  (* Remove characters from the left, shortening the string 
     to ToLength. *) 
  (* NOOP if ToLength > Length ( String *) 

  = BEGIN (* LeftTruncateInPlace *) 
      IF ToLength <= 0 
      THEN 
        String . FromSs := 0 
      ; String . ToSs := 0 
      ; String . Text := NIL 
      (* Keep Space around *) 
      ELSIF ToLength < String . ToSs - String . FromSs 
      THEN (* Must properly shorten, but not to empty *) 
        ForceToHaveSpaceOnly ( String ) 
      ; Assert ( String . FromSs = 0 , AFT . A_LeftTruncateInPlace_BadFromSs ) 
      ; SUBARRAY ( String . Space ^ , 0 , ToLength ) 
          := SUBARRAY 
               ( String . Space ^ , String . ToSs - ToLength , ToLength ) 
      ; String . ToSs := ToLength 
      END (* IF *) 
    END LeftTruncateInPlace 

(* VISIBLE: *) 
; PROCEDURE InsertBlanksInPlace 
    ( VAR (* IN OUT *) String : T 
    ; PrefixLength : StringSsTyp 
    ; BlankCount : INTEGER 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    RAISES { SsOutOfBounds , Backout } 
  (* After the prefix, insert BlankCount blanks, shifting 
     the suffix right and extending the string by BlankCount *) 

  = VAR LLength : StringSsTyp 
  ; VAR LSuffixLength : StringSsTyp 

  ; BEGIN (* InsertBlanksInPlace *) 
      MergeEventualLengthHint ( String , EventualLengthHint ) 
    ; LLength := String . ToSs - String . FromSs 
    ; IF PrefixLength > LLength 
      THEN 
        RAISE SsOutOfBounds 
      ELSE 
        IF BlankCount > 0 
        THEN (* non-empty insertion  *) 
          MergeEventualLengthHint ( String , LLength + BlankCount ) 
        ; ForceToHaveSpaceOnly ( String ) 
        ; Assert 
            ( String . FromSs = 0 
            , AFT . A_InsertBlanksInPlace_RightBadFromSs 
            ) 
        ; LSuffixLength := LLength - PrefixLength 
        ; SUBARRAY 
            ( String . Space ^ , PrefixLength + BlankCount , LSuffixLength ) 
            := SUBARRAY ( String . Space ^ , PrefixLength , LSuffixLength ) 
        ; FOR RI := PrefixLength TO PrefixLength + BlankCount - 1  
          DO String . Space ^ [ RI ] := ' ' 
          END (* FOR *) 
        ; INC ( String . ToSs , BlankCount ) 
        END (* IF *) 
      END (* IF *) 
    END InsertBlanksInPlace 

(* VISIBLE: *) 

; PROCEDURE DeleteCharsInPlace 
    ( VAR (* IN OUT *) String : T 
    ; PrefixLength : StringSsTyp 
    ; DeleteCount : INTEGER 
    ) 
    RAISES { SsOutOfBounds , Backout } 
  (* After the prefix, delete DeleteCount characters, 
     shifting the suffix left and shortening the string 
     by DeleteCount. Attempting to delete beyond the 
     end of the string does nothing. *) 

  = VAR LLength : StringSsTyp 
  ; VAR LDeleteCount : StringSsTyp 
  ; VAR LSuffixLength : StringSsTyp 

  ; BEGIN (* DeleteCharsInPlace *) 
      LLength := String . ToSs - String . FromSs 
    ; IF PrefixLength > LLength 
      THEN 
        RAISE SsOutOfBounds 
      ELSE 
        LDeleteCount := MIN ( DeleteCount , LLength - PrefixLength ) 
      ; IF LDeleteCount > 0 
        THEN 
          ForceToHaveSpaceOnly ( String ) 
        ; Assert 
            ( String . FromSs = 0 , AFT . A_DeleteCharsInPlace_LeftBadFromSs ) 
        ; LSuffixLength := LLength - PrefixLength - LDeleteCount 
        ; SUBARRAY ( String . Space ^ , PrefixLength , LSuffixLength ) 
            := SUBARRAY 
                 ( String . Space ^ 
                 , PrefixLength + DeleteCount 
                 , LSuffixLength 
                 ) 
        ; DEC ( String . ToSs , LDeleteCount ) 
        END (* IF *) 
      END (* IF *) 
    END DeleteCharsInPlace 

(* VISIBLE: *) 

; PROCEDURE Length ( String : T ) : StringSsTyp 

  = BEGIN (* Length *) 
      RETURN String . ToSs - String . FromSs 
    END Length 

(* VISIBLE: *) 
; PROCEDURE Substring 
    ( String : T 
    ; From : StringSsTyp := 0 
    ; For : StringSsTyp := LAST ( StringSsTyp ) 
    ) : T 
    RAISES { Backout } 

  = VAR LLength : StringSsTyp 
  ; VAR LResult : T 

  ; BEGIN (* Substring *) 
      LLength := Length ( String ) 
    ; From := MIN ( From , LLength ) 
    ; For := MIN ( For , LLength - From ) 
    ; IF For <= 0 OR From >= LLength 
      THEN 
        RETURN Empty ( )  
      ELSE 
        IF String . Text # NIL 
        THEN 
          LResult . Text := String . Text 
        ; LResult . FromSs := String . FromSs + From 
        ; LResult . ToSs := String . FromSs + From + For 
        ELSE (* Can never share Space *) 
          Assert ( String . FromSs = 0 , AFT . A_Substring_BadFromSs ) 
        ; LResult . Text 
            := Text . FromChars ( SUBARRAY ( String . Space ^ , From , For ) ) 
        ; LResult . FromSs := 0 
        ; LResult . ToSs := For 
        END (* IF *) 
      ; LResult . Space := NIL 
      ; LResult . EventualLengthHint := 0 
      ; RETURN LResult 
      END (* IF *) 
    END Substring 

(* VISIBLE: *) 
; PROCEDURE AreEqual ( Left , Right : T ) : BOOLEAN 

  = VAR LLeftLength := Length ( Left ) 
  ; VAR LRightLength := Length ( Right ) 

  ; BEGIN (* AreEqual *) 
      IF LLeftLength # LRightLength 
      THEN 
        RETURN FALSE 
      ELSIF LLeftLength = 0 
      THEN 
        RETURN TRUE 
      ELSE (* Lengths are equal and > 0 *)  
        IF Left . Space = NIL 
        THEN (* Left is Text.T. *) 
          IF Right . Space = NIL 
          THEN (* Both are Text.Ts. *) 
            RETURN Text . Equal ( Left . Text , Right . Text ) 
          ELSE (* Left is Text.T, right is space. *) 
            RETURN AreEqual ( Right , Left ) 
          END (* IF *) 
        ELSE (* Left is space. *) 
          IF Right . Space = NIL 
          THEN (* Left is space, Right is Text.T. *) 
            FOR RI := 0 TO LRightLength - 1 
            DO 
              IF Left . Space ^ [ RI ]  
                 # Text . GetChar ( Right . Text , Right . FromSs + RI ) 
              THEN 
                RETURN FALSE 
              END (* IF *) 
            END (* FOR *) 
          ; RETURN TRUE 
          ELSE (* Both are spaces. *)  
            RETURN 
              SUBARRAY ( Left . Space ^ , Left . FromSs , LLeftLength ) 
              = SUBARRAY ( Right . Space ^ , Right . FromSs , LLeftLength ) 
          END (* IF*) 
        END (* IF*) 
      END (* IF *) 
    END AreEqual 

(* VISIBLE: *) 
; PROCEDURE AreEqualButForTrailingBlanks ( Left , Right : T ) : BOOLEAN 

  = VAR LLeftLength := Length ( Left ) 
  ; VAR LRightLength := Length ( Right ) 

  ; BEGIN (* AreEqualButForTrailingBlanks *) 
      IF LLeftLength = 0 AND  LRightLength = 0 
      THEN 
        RETURN TRUE 
      ELSE (* Lengths are > 0 *)  
        IF Left . Space = NIL 
        THEN (* Left is Text.T. *) 
          IF Right . Space = NIL 
          THEN (* Both are Text.Ts. *) 
            WHILE LLeftLength > 0 
                  AND Text . GetChar 
                        ( Left . Text , Left . FromSs + LLeftLength - 1 ) 
                      = ' ' 
            DO DEC ( LLeftLength ) 
            END 
          ; WHILE LRightLength > 0 
                  AND Text . GetChar 
                        ( Right . Text , Right . FromSs + LRightLength - 1 ) 
                      = ' ' 
            DO DEC ( LRightLength ) 
            END 
          ; IF LLeftLength # LRightLength
            THEN RETURN FALSE 
            ELSIF LLeftLength = 0 
            THEN RETURN TRUE 
            ELSE 
              FOR RI := 0 TO LRightLength - 1 
              DO 
                IF Text . GetChar ( Left . Text , Left . FromSs + RI ) 
                   # Text . GetChar ( Right . Text , Right . FromSs + RI ) 
                THEN 
                  RETURN FALSE 
                END (* IF *) 
              END (* FOR *) 
            ; RETURN TRUE 
            END (* IF *) 
          ELSE (* Left is Text.T, right is space. *) 
            RETURN AreEqualButForTrailingBlanks ( Right , Left ) 
          END (* IF on Right. *) 
        ELSE (* Left is space. *) 
          IF Right . Space = NIL 
          THEN (* Left is space, Right is Text.T. *) 
            WHILE LLeftLength > 0 
                  AND Left . Space ^ [ Left . FromSs + LLeftLength - 1 ] = ' ' 
            DO DEC ( LLeftLength ) 
            END 
          ; WHILE LRightLength > 0 
                  AND Text . GetChar 
                        ( Right . Text , Right . FromSs + LRightLength - 1 ) 
                       = ' ' 
            DO DEC ( LRightLength ) 
            END 
          ; IF LLeftLength # LRightLength
            THEN RETURN FALSE 
            ELSIF LLeftLength = 0 
            THEN RETURN TRUE 
            ELSE 
              FOR RI := 0 TO LRightLength - 1 
              DO 
                IF Left . Space ^ [ RI ]  
                   # Text . GetChar ( Right . Text , Right . FromSs + RI ) 
                THEN 
                  RETURN FALSE 
                END (* IF *) 
              END (* FOR *) 
            ; RETURN TRUE 
            END (* IF *) 
          ELSE (* Both are spaces. *)  
            WHILE LLeftLength > 0 
                  AND Left . Space 
                      ^ [ Left . FromSs + LLeftLength - 1 ] = ' ' 
            DO DEC ( LLeftLength ) 
            END 
          ; WHILE LRightLength > 0 
                  AND Right . Space 
                      ^ [ Right . FromSs + LRightLength - 1 ] = ' ' 
            DO DEC ( LRightLength ) 
            END 
          ; IF LLeftLength # LRightLength
            THEN RETURN FALSE 
            ELSIF LLeftLength = 0 
            THEN RETURN TRUE 
            ELSE 
               RETURN 
                  SUBARRAY ( Left . Space ^ , Left . FromSs , LLeftLength ) 
                  = SUBARRAY 
                      ( Right . Space ^ , Right . FromSs , LLeftLength ) 
            END (* IF *) 
          END (* IF*) 
        END (* IF *) 
      END (* IF *) 
    END AreEqualButForTrailingBlanks  

(* VISIBLE: *) 
; PROCEDURE Copy ( String : T ) : T 

  = VAR LResult : T 

  ; BEGIN (* Copy *) 
      IF String . Space = NIL 
      THEN 
        RETURN String 
      ELSE 
        LResult := String 
      ; LResult . Space 
          := NEW 
               ( SpaceTyp 
               , MAX ( Length ( String ) , String . EventualLengthHint ) 
               ) 
      ; SUBARRAY ( LResult . Space ^ , 0 , String . ToSs ) 
          := SUBARRAY ( String . Space ^ , 0 , String . ToSs ) 
      ; RETURN LResult 
      END (* IF *) 
    END Copy 

(* VISIBLE: *) 
; PROCEDURE VerbatimCopy ( Old : T ; VAR New : T ) 

  = BEGIN 
      New := Old 
    (* Field Text can point to shared object, since it is immutable. 
       Field Space must be copied, since it is not. 
    *) 
    ; IF New . Space # NIL 
      THEN 
        New . Space := NEW ( SpaceTyp , NUMBER ( Old . Space ^ ) ) 
      ; New . Space ^ := Old . Space ^ 
      END (* IF *) 
    END VerbatimCopy 

(* VISIBLE: *) 
; PROCEDURE PosOf1stNonblank 
    ( READONLY String : T ) : StringSsTyp 
    RAISES { Backout } 

  = VAR LSs : StringSsTyp := String . FromSs 

  ; BEGIN (* PosOf1stNonblank *) 
      IF String . Text # NIL 
      THEN 
        WHILE LSs < String . ToSs 
              AND Text . GetChar ( String . Text , LSs ) = ' ' 
        DO INC ( LSs ) 
        END (* WHILE *) 
      ; RETURN LSs - String . FromSs 
      ELSIF String . Space # NIL 
      THEN 
        Assert ( String . FromSs = 0 , AFT . A_PosOf1stNonblank_BadFromSs ) 
      ; WHILE LSs < String . ToSs AND String . Space ^ [ LSs ] = ' ' 
        DO INC ( LSs ) 
        END (* WHILE *) 
      ; RETURN LSs 
      ELSE 
        Assert 
          ( String . FromSs = 0 AND String . ToSs = 0 
          , AFT . A_PosOf1stNonblank_BadEmptyString 
          ) 
      ; RETURN 0 
      END (* IF *) 
    END PosOf1stNonblank 

(* VISIBLE: *) 
; PROCEDURE PosOfLastNonblank 
    ( READONLY String : T ) : StringSsSignedTyp 
    RAISES { Backout } 

  = VAR LSs : StringSsSignedTyp := String . ToSs 

  ; BEGIN (* PosOfLastNonblank *) 
      IF String . Text # NIL 
      THEN 
        LOOP
          DEC ( LSs ) 
        ; IF LSs < String . FromSs 
             OR Text . GetChar ( String . Text , LSs ) # ' ' 
          THEN RETURN LSs - String . FromSs  
          END 
        END 
      ELSIF String . Space # NIL 
      THEN 
        Assert ( String . FromSs = 0 , AFT . A_PosOfLastNonblank_BadFromSs ) 
      ; LOOP 
          DEC ( LSs ) 
        ; IF LSs < 0 OR String . Space ^ [ LSs ] # ' ' 
          THEN RETURN LSs 
          END
        END 
      ELSE 
        Assert 
          ( String . FromSs = 0 AND String . ToSs = 0 
          , AFT . A_PosOfLastNonblank_BadEmptyString 
          ) 
      ; RETURN - 1  
      END (* IF *) 
    END PosOfLastNonblank 

(* VISIBLE: *) 
; PROCEDURE InvokeWithArrayOfChar 
    ( String : T ; Proc : ProcArrayOfChar ) 
    RAISES { Backout } 

  = TYPE EmptyArrayOfCharTyp = ARRAY [ 0 .. - 1 ] OF CHAR 

  ; VAR LLength := Length ( String ) 

  ; BEGIN (* InvokeWithArrayOfChar *) 
      IF LLength = 0 
      THEN 
        Proc ( EmptyArrayOfCharTyp { } ) 
      ELSE 
        ForceToHaveSpaceOnly ( String ) 
      ; Assert 
          ( String . FromSs = 0 , AFT . A_InvokeWithArrayOfChar_BadFromss ) 
      ; Proc ( SUBARRAY ( String . Space ^ , 0 , LLength ) ) 
      END (* IF *) 
    END InvokeWithArrayOfChar 

(* VISIBLE: *) 
; PROCEDURE AtLeastBlanks ( MinLength : StringSsTyp ) : T 
  RAISES { Backout } 

  (* Return a string that is all blanks and of at least MinLength *) 
  (* Depends on clients' not mutating the result. *) 

  = BEGIN 
      MergeEventualLengthHint ( Blanks , MinLength ) 
    ; ForceToHaveSpaceOnly ( Blanks ) 
    ; WHILE Blanks . ToSs < MinLength 
      DO Blanks . Space ^ [ Blanks . ToSs ] := ' '  
      ; INC ( Blanks . ToSs ) 
      END 
    ; RETURN Blanks 
    END AtLeastBlanks 

; VAR Blanks : T 

; BEGIN (* Strings *) 
    Blanks 
      := FromText 
           ( "                                                               " 
           , EventualLengthHint := 255 
           ) 
  END Strings 
. 




