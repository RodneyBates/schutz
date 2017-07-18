
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE SharedStrings 

(* Strings of CHAR, taken from several representations, turned into atoms. *) 

(* Except for swapping the strings "Pickle" and "Pickle2 AS Pickle", make no
   changes to the following line, as it is recognized/edited by scripts.
   (see scripts/topickle.sh and scripts/topickle2.sh)
*) 
; IMPORT Pickle2 AS Pickle (* The Pickle2 mess. *)
; IMPORT Rd 
; IMPORT SharedStringRefTbl 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Word 
; IMPORT Wr 

; IMPORT LbeStd 
; IMPORT Strings 
; IMPORT Assertions 
; IMPORT Misc 

; IMPORT MessageCodes 

; FROM Assertions IMPORT Assert , AssertionFailure  

; TYPE AFT = MessageCodes . T 

; REVEAL T 
    = BRANDED 
        Brand 
        OBJECT 
          StringNo : StringNoTyp 
        ; Tok : LbeStd . TokTyp 
        ; Value : TEXT 
        END (* OBJECT *) 

(* VISIBLE: *) 
; PROCEDURE Equal ( k1 , k2 : T ) : BOOLEAN 

  = BEGIN 
      IF k1 = NIL THEN k1 := Null END 
    ; IF k2 = NIL THEN k2 := Null END 
    ; RETURN 
        k1 . Tok = k2 . Tok 
        AND Text . Equal ( k1 . Value , k2 . Value )  
    END Equal 

(* VISIBLE: *) 
; PROCEDURE Hash ( k : T ) : Word . T 

  = BEGIN 
      IF k = NIL THEN k := Null END 
    ; RETURN Text . Hash ( k . Value ) + 7 * k . Tok 
    END Hash 

; VAR NextAvailStringNo : StringNoTyp := FirstRealStringNo 

(* TODO: Decide what to do about NIL shared strings 
         for the remaining cases. *) 

(* VISIBLE: *) 
; PROCEDURE UniqueStringCt ( ) : StringNoTyp 

  = BEGIN (* UniqueStringCt *) 
      RETURN NextAvailStringNo - FirstRealStringNo 
    END UniqueStringCt 

(* FIXME: This will have to be redone to support renumbering 
          and weak references.  Will probably need its own 
          hash table. This will work, until we need to free 
          up memory and/or StringNoTyp space. *) 

; CONST InitHashSize = 1301 

; VAR HashTable : SharedStringRefTbl . T 
(* TODO: This is silly.  The table maps pointers to themselves. *) 

(* VISIBLE: *) 
; PROCEDURE FromText 
    ( String : TEXT 
    ; Tok : LbeStd . TokTyp := LbeStd . Tok__Null 
    ) 
  : T 
  RAISES { AssertionFailure } 

  = VAR LRef : REFANY 
  ; VAR LNew : T 
  ; VAR LResult : T 

  ; BEGIN (* FromText *) 
      IF String = NIL 
      THEN 
        String := ""
      END (* IF *) 
    ; LNew 
        := NEW 
             ( T 
             , StringNo := 0 
             , Tok := Tok 
             , Value := String 
             ) 
    ; IF HashTable . get ( LNew , (* VAR *) LRef ) 
      THEN 
        LResult := LRef 
      ; Assert 
          ( Equal ( LNew , LResult ) 
          , AFT . A_SharedStrings_FromText_TokMismatch  
          ) 
      ELSE 
        LResult := LNew 
      ; LResult . StringNo := NextAvailStringNo 
      ; EVAL HashTable . put ( LResult , LResult ) 
      ; INC ( NextAvailStringNo ) 
      END (* IF *) 
    ; RETURN LResult 
    END FromText 

(* VISIBLE: *) 
; PROCEDURE FromString 
    ( String : Strings . T 
    ; Tok : LbeStd . TokTyp := LbeStd . Tok__Null 
    ) 
  : T 
  RAISES { AssertionFailure } 

  = BEGIN (* FromString *) 
      RETURN FromText ( Strings . ToText ( String ) , Tok ) 
    END FromString 

(* VISIBLE: *) 
; PROCEDURE FromArrayOfChar 
    ( READONLY String : ARRAY OF CHAR 
    ; Tok : LbeStd . TokTyp := LbeStd . Tok__Null 
    ) 
  : T 
  RAISES { AssertionFailure } 

  = BEGIN (* FromArrayOfChar *) 
      RETURN FromText ( Text . FromChars ( String ) , Tok ) 
    END FromArrayOfChar 

(* VISIBLE: *) 
; PROCEDURE SetTok ( String : T ; Tok : LbeStd . TokTyp ) 

  = BEGIN (* SetTok *) 
      IF String # NIL 
      THEN String . Tok := Tok 
      END (* IF *) 
    END SetTok 

(* VISIBLE: *) 
; PROCEDURE Length ( String : T ) : LengthTyp 

  = BEGIN (* Length *) 
      IF String = NIL 
      THEN RETURN 0 
      ELSE RETURN Text . Length ( String . Value ) 
      END (* IF *) 
    END Length 

(* VISIBLE: *) 
; PROCEDURE StringNo ( String : T ) : StringNoTyp 

  = BEGIN (* StringNo *) 
      IF String # NIL 
      THEN RETURN String . StringNo 
      ELSE RETURN StringNoNull 
      END (* IF *) 
    END StringNo 

(* VISIBLE: *) 
; PROCEDURE Tok ( String : T ) : LbeStd . TokTyp 

  = BEGIN (* Tok *) 
      IF String # NIL 
      THEN RETURN String . Tok 
      ELSE RETURN LbeStd . Tok__Null  
      END (* IF *) 
    END Tok 

(* VISIBLE: *) 
; PROCEDURE ToString ( String : T ) : Strings . T 

  = BEGIN (* ToString *) 
      IF String = NIL 
      THEN 
        RETURN Strings . FromText ( "" ) 
      ELSE 
        RETURN Strings . FromText ( String . Value ) 
      END (* IF *) 
    END ToString 

(* VISIBLE: *) 
; PROCEDURE ToText ( String : T ) : TEXT 
  (* Will not return NIL *) 

  = BEGIN (* ToText *) 
      IF String = NIL 
      THEN 
        RETURN "" 
      ELSE 
        RETURN String . Value 
      END (* IF *) 
    END ToText 

(* VISIBLE: *) 
; PROCEDURE Image ( String : T ; Indent := LbeStd . StdIndent ) : TEXT 
  (* Will not return NIL *) 

  = BEGIN (* Image *) 
      IF String = NIL 
      THEN 
        RETURN Misc . Blanks ( Indent ) & "NIL" 
      ELSE 
        RETURN 
          Misc . Blanks ( Indent ) 
          & "SharedString.T{" 
          & LbeStd . NumIdTokImage ( String . Tok ) 
          & "," 
          & StringNoImage ( String . StringNo ) 
          & ",\"" 
          & Misc . EscapeText ( String . Value ) 
          & "\"}" 
      END (* IF *) 
    END Image 

(* VISIBLE: *) 
; PROCEDURE IthChar 
    ( String : T ; I : LengthTyp ) : CHAR RAISES { SsOutOfBounds } 

  = BEGIN (* IthChar *) 
      IF String = NIL THEN String := Null END 
    ; IF I < 0 OR I >= Text . Length ( String . Value ) 
      THEN 
        RAISE SsOutOfBounds 
      ELSE 
        RETURN Text . GetChar ( String . Value , I ) 
      END (* IF *) 
    END IthChar 

; PROCEDURE SpecialWrite 
    ( <* UNUSED *> Self : Pickle . SpecialPublic 
    ; ref : REFANY 
    ; writer : Pickle . Writer 
    ) 
  RAISES { Pickle . Error } 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* SpecialWrite *) 
      IF TYPECODE ( ref ) # TYPECODE ( T ) 
      THEN RAISE Pickle . Error ( "SharedStrings.SpecialWrite, bad TYPECODE" ) 
      ELSE 
        WITH WString = NARROW ( ref , T ) 
        DO 
          writer . writeInt ( WString . Tok ) 
(* TODO: pack these values. *) 
        ; writer . write ( WString . Value ) 
        END (* WITH *) 
      END (* IF *) 
    END SpecialWrite 

; PROCEDURE SpecialRead 
    ( <* UNUSED *> Self : Pickle . SpecialPublic 
    ; reader : Pickle . Reader 
    ; <* UNUSED *> id : Pickle . RefID 
    ) 
  : REFANY 
  RAISES { Pickle . Error } 

  = <* FATAL Rd . Failure *> 
    <* FATAL Rd . EndOfFile *> 
    <* FATAL Thread . Alerted *> 
    VAR LInt : INTEGER 
  ; VAR LTok : LbeStd . TokTyp 
  ; VAR LText : TEXT 

  ; BEGIN (* SpecialRead *) 
      LInt := reader . readInt ( ) 
    ; LTok := LInt 
    ; LText := reader . read ( ) 
    ; TRY 
        RETURN FromText ( LText , LTok ) 
      EXCEPT
        AssertionFailure 
      => RAISE Pickle . Error ( "SharedStrings.SpecialRead, can't convert." ) 
      END 
    END SpecialRead 

; BEGIN (* SharedStrings *) 
    HashTable 
      := NEW ( SharedStringRefTbl . Default ) 
         . init ( sizeHint := InitHashSize ) 
  ; Pickle . RegisterSpecial 
      ( NEW 
          ( Pickle . Special 
          , sc := TYPECODE ( T ) 
          , write := SpecialWrite 
          , read := SpecialRead 
          ) 
      ) 
  ; <* FATAL ANY *> BEGIN 
(* TODO: There seems to be no nice way to take care of exceptions raised
         in module initialization block bodies.
*) 
      Null := FromText ( "" )   
    END (* Block *) 
  END SharedStrings 
. 
