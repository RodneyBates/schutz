  
(* -----------------------------------------------------------------------1- *)
(* File Layout.m3  Modula-3 source code.                                     *)
(* Copyright 2020, Rodney M. Bates.                                          *)
(* rbates@acm.org                                                            *)
(* Licensed under the MIT license.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Layout 

(* A layer on top of Wr that counts lines and characters. *) 

; IMPORT Text 
; IMPORT Thread
; IMPORT Wr 

; REVEAL T 
  = BRANDED Brand OBJECT 
      UsedStream : Wr . T := NIL 
    ; TabCtFunc : TabCtFuncTyp := DefaultTabCtFunc 
    ; LineNo : CARDINAL := 0  
    ; CharNo : CARDINAL := 0 
    ; EolState : [ 0 .. 1 ] := 0   
      (* ^The stream ends with this many characters of an incomplete 
         EOL sequence. *)
    ; Simple : BOOLEAN := TRUE 
    END 

; CONST Tab = '\t'
; CONST WideTab = W'\t'

(* VISIBLE: *) 
; PROCEDURE Init 
    ( Stream : T 
    ; UseStream : Wr . T 
    ; Simple : BOOLEAN := TRUE 
    ; TabCtFunc : TabCtFuncTyp := DefaultTabCtFunc 
    ) 
  : T 
  (* Initialize and return Stream, setting its line and character numbers 
     to zero, and ready to write to UseStream.
     If Simple, future Put* operations on Stream will not check the written
     characters for tabs or ends-of-lines, which, if present, would leave line 
     and character numbers inaccurate.  Only calls to PutEol will be recognized
     as ends-of-lines and only calls to PutTab as tabs.  This can be 
     considerably faster.  If NOT Simple, future Put* operations will look for 
     and interpret tabs and ends-of-lines.  This will be slower.  When a tab is
     accounted for, by either method of discovery, callback TabCtFunc will be 
     used to ascertain how many spaces the tab expands to. 
     Tabs are *not* expanded into blanks in the output, only counted as 
     multiple characters for maintaining character number.     
     You can do operations on UseStream directly, but those that write to it
     will probably undermine line and character counting. 
  *) 

  = BEGIN 
      Stream . UsedStream := UseStream 
    ; Stream . Simple := Simple 
    ; Stream . TabCtFunc := TabCtFunc 
    ; Stream . LineNo := 0 
    ; Stream . CharNo := 0 
    ; Stream . EolState := 0 
    ; RETURN Stream 
    END Init  

(* VISIBLE: *) 
; PROCEDURE DefaultTabCtFunc ( CharNo : CARDINAL ) : CARDINAL 
  (* Tab stops wherever (zero-origin)CharNo MOD 8 = 0. *) 

  = BEGIN 
      RETURN 8 - ( CharNo MOD 8 ) 
    END DefaultTabCtFunc 

(* VISIBLE: *) 
; PROCEDURE UsedStream ( Stream : T ) : Wr . T 
  (* The value of UseStream that was passed to Stream when it was initialized. *) 

  = BEGIN 
      RETURN Stream . UsedStream 
    END UsedStream 

; PROCEDURE AccountForChar ( Stream : T ; Ch : WIDECHAR ) 
  (* Account for the effects of having written Ch to Stream. *) 

  = BEGIN 
      IF Stream . EolState = 1 AND Ch = EolWChar1 
      THEN (* Last of a two-char eol sequence. *) 
        INC ( Stream . LineNo ) 
      ; Stream . CharNo := 0 
      ; Stream . EolState := 0 
      ELSIF Ch = EolWChar0 
      THEN (* First char of an eol sequence.  Possibly EolState = 1. *) 
        IF EolLen < 2 
        THEN (* Single-char eol. *) 
          INC ( Stream . LineNo ) 
        ; Stream . CharNo := 0 
        (* Stream . EolState = 0 can't fail.  *) 
        ELSE (* First of two-char eol sequence. *) 
          INC ( Stream . CharNo ) 
        ; Stream . EolState := 1 
        END (* IF *) 
      ELSIF Ch = WideTab  
      THEN (* Tab char.  Possibly EolState = 1. *) 
        INC ( Stream . CharNo , Stream . TabCtFunc ( Stream . CharNo ) ) 
      ; Stream . EolState := 0 
      ELSE (* Not a notable character.  (EolWChar1 is notable only if 
              EolState = 1.) *) 
        INC ( Stream . CharNo ) 
      ; Stream . EolState := 0 
      END (* IF *) 
    END AccountForChar 

(* TODO: Consider whether we want a version of PutText that truncates to CHAR *)

(* VISIBLE: *) 
; PROCEDURE PutText ( Stream : T ; Tx : TEXT ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write characters of Tx as CHARs to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in Tx
     and adjacent characters. 
  *) 

  = BEGIN 
      IF Tx # NIL 
      THEN 
        Wr . PutText ( Stream . UsedStream , Tx ) 
      ; IF Stream . Simple 
        THEN 
          INC ( Stream . CharNo , Text . Length ( Tx ) ) 
        ELSE 
          FOR RI := 0 TO Text . Length ( Tx ) - 1 
          DO 
            AccountForChar ( Stream , Text . GetWideChar ( Tx , RI ) ) 
          (* NOTE: Do not truncate.          ^ *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END PutText  

(* VISIBLE: *) 
; PROCEDURE PutWideText ( Stream : T ; Tx : TEXT ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write chars of Tx as WIDECHARs to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in Tx
     and adjacent characters. 
  *) 

  = BEGIN 
      IF Tx # NIL 
      THEN 
        Wr . PutWideText ( Stream . UsedStream , Tx ) 
      (* ^Don't truncate any wide chars in Tx. *) 
      ; IF Stream . Simple 
        THEN 
          INC ( Stream . CharNo , Text . Length ( Tx ) ) 
        ELSE 
          FOR RI := 0 TO Text . Length ( Tx ) - 1 
          DO 
            AccountForChar ( Stream , Text . GetWideChar ( Tx , RI ) ) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END PutWideText  

(* VISIBLE: *) 
; PROCEDURE PutChar ( Stream : T ; Ch : CHAR ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write Ch to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in Ch
     and adjacent characters. 
  *) 

  = BEGIN 
      Wr . PutChar ( Stream . UsedStream , Ch ) 
    ; IF Stream . Simple 
      THEN 
        INC ( Stream . CharNo ) 
      ELSE 
        AccountForChar ( Stream , VAL ( ORD ( Ch ) , WIDECHAR ) ) 
      END (* IF *) 
    END PutChar 

(* VISIBLE: *) 
; PROCEDURE PutWideChar ( Stream : T ; WCh : WIDECHAR ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write WCh to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in WCh
     and adjacent characters. 
  *) 

  = BEGIN 
      Wr . PutWideChar ( Stream . UsedStream , WCh ) 
    ; IF Stream . Simple 
      THEN 
        INC ( Stream . CharNo ) 
      ELSE 
        AccountForChar (Stream , WCh ) 
      END (* IF *) 
    END PutWideChar 

(* VISIBLE: *) 
; PROCEDURE PutString ( Stream : T ; READONLY String : ARRAY OF CHAR ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write String as CHARs to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in String
     and adjacent characters. 
  *) 

  = BEGIN 
      Wr . PutString ( Stream . UsedStream , String ) 
    ; IF Stream . Simple 
      THEN 
        INC ( Stream . CharNo , NUMBER ( String ) ) 
      ELSE 
        FOR RI := FIRST ( String ) TO LAST ( String ) 
        DO 
          AccountForChar ( Stream , VAL ( ORD ( String [ RI ] ) , WIDECHAR ) ) 
        END (* IF *) 
      END (* IF *) 
    END PutString 

(* VISIBLE: *) 
; PROCEDURE PutWideString ( Stream : T ; READONLY WString : ARRAY OF WIDECHAR ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write WString as WIDECHARs to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in WString
     and adjacent characters. 
  *) 

  = BEGIN 
      Wr . PutWideString ( Stream . UsedStream , WString ) 
    ; IF Stream . Simple 
      THEN 
        INC ( Stream . CharNo , NUMBER ( WString ) ) 
      ELSE 
        FOR RI := FIRST ( WString ) TO LAST ( WString ) 
        DO 
          AccountForChar ( Stream , WString [ RI ] ) 
        END (* IF *) 
      END (* IF *) 
    END PutWideString 

(* VISIBLE: *) 
; PROCEDURE PutEol ( Stream : T ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write an end-of-line sequence, as CHARs, to UsedStream(Stream).  
     If Stream was initialized with Simple, calling this is the only way that 
     will be recognized as affecting CurrentLine and CharPos. 
  *) 

  = BEGIN 
      Wr . PutText ( Stream . UsedStream, Wr . EOL ) 
    ; INC ( Stream . LineNo ) 
    ; Stream . CharNo := 0 
    ; Stream . EolState := 0 
    END PutEol 

(* VISIBLE: *) 
; PROCEDURE PutWideEol ( Stream : T ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write an end-of-line sequence, as WIDECHARs, to UsedStream(Stream).  
     If Stream was initialized with Simple, calling this is the only way that 
     will be recognized as affecting CurrentLine and CharPos. 
  *) 

  = BEGIN 
      Wr . PutWideText ( Stream . UsedStream, Wr . EOL ) 
    ; INC ( Stream . LineNo ) 
    ; Stream . CharNo := 0 
    ; Stream . EolState := 0 
    END PutWideEol 

(* VISIBLE: *) 
; PROCEDURE PutTab ( Stream : T ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a horizontal tab, as a CHAR, to UsedStream(Stream).  
     If Stream was initialized with Simple, calling this is the only way that 
     will be recognized as affecting CharPos. 
     Use callback TabCtFunc, as was supplied to Init, to ascertain how far to 
     tab to. Do not expand the tab into blanks in the output, only count its 
     possibly multiple spaces for maintaining character number.  
  *) 

  = BEGIN 
      Wr . PutChar ( Stream . UsedStream , Tab ) 
    ; INC ( Stream . CharNo , Stream . TabCtFunc ( Stream . CharNo ) ) 
    ; Stream . EolState := 0 
    END PutTab 

(* VISIBLE: *) 
; PROCEDURE PutWideTab ( Stream : T ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a horizontal tab, as a WIDECHAR, to UsedStream(Stream).  
     If Stream was initialized with Simple, calling this is the only way that 
     will be recognized as affecting CharPos. 
     Use callback TabCtFunc, as was supplied to Init, to ascertain how far to 
     tab to. Do not expand the tab into blanks in the output, only count its 
     possibly multiple spaces for maintaining character number.  
  *) 

  = BEGIN 
      Wr . PutWideChar ( Stream . UsedStream , WideTab ) 
    ; INC ( Stream . CharNo , Stream . TabCtFunc ( Stream . CharNo ) ) 
    ; Stream . EolState := 0 
    END PutWideTab 

; PROCEDURE AccountForPadChars 
    ( Stream : T ; Ct : CARDINAL ; PadChar : WIDECHAR ) 

  = BEGIN 
      IF Stream . EolState = 1 AND PadChar = EolWChar1  
      THEN (* Last of a two-char end-of-line sequence. *) 
        INC ( Stream . LineNo ) 
      ; Stream . EolState := 0 
      ; Stream . CharNo := Ct - 1 
        (* The rest will not be part of EOL sequence. *) 
      ELSIF PadChar = EolWChar0 
      THEN (* First char of an EOL sequence.  Possibly EolState = 1. *)
        IF EolLen = 0 
        THEN (* Single-char EOL.  => EolState = 0. *) 
          INC ( Stream . LineNo , Ct ) (* Count all as EOLs. *) 
        ; Stream . CharNo := 0 
        ELSE (* First of a two-char EOL sequence. *) 
          INC ( Stream . CharNo , Ct ) 
          (* Only the last could be part of EOL sequence. *)  
        ; Stream . EolState := 1
        END (* IF *) 
      ELSIF PadChar = WideTab 
      THEN (* Tab char.  Possibly EolState = 1 *) 
        FOR RI := 1 TO Ct 
        DO INC ( Stream . CharNo , Stream . TabCtFunc ( Stream . CharNo ) ) 
        END (* FOR *) 
      ; Stream . EolState := 0
      ELSE (* Not a notable char.  (EolWChar1 is notable only if EolState=1.) *)
        INC ( Stream . CharNo , Ct ) 
      ; Stream . EolState := 0 
      END (* IF *) 
    END AccountForPadChars 

(* VISIBLE: *) 
; PROCEDURE PadRel ( Stream : T ; Ct : CARDINAL ; PadChar : CHAR := ' ' ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write Ct copies of PadChar. *) 

  = BEGIN 
      FOR RI := 1 TO Ct 
      DO Wr . PutChar ( Stream . UsedStream , PadChar ) 
      END (* FOR *) 
    ; IF Stream . Simple 
      THEN 
        INC ( Stream . CharNo , Ct ) 
      ELSE AccountForPadChars 
             ( Stream , Ct , VAL ( ORD ( PadChar ) , WIDECHAR ) ) 
      END (* IF *) 
    END PadRel 

(* VISIBLE: *) 
; PROCEDURE PadRelWide 
     ( Stream : T ; Ct : CARDINAL ; PadChar : WIDECHAR := W' ' ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write Ct copies of PadChar as wide characters. *) 

  = BEGIN 
      FOR RI := 1 TO Ct 
      DO Wr . PutWideChar ( Stream . UsedStream , PadChar ) 
      END (* FOR *) 
    ; IF Stream . Simple 
      THEN 
        INC ( Stream . CharNo , Ct ) 
      ELSE AccountForPadChars ( Stream , Ct , PadChar ) 
      END (* IF *) 
    END PadRelWide 

(* VISIBLE: *) 
; PROCEDURE PadAbs ( Stream : T ; Pos : CARDINAL ; PadChar : CHAR := ' ' ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write MAX(0,Pos-CharNo(Stream)) copies of PadChar. *) 

  = VAR LCt : INTEGER 

  ; BEGIN 
      LCt := Pos - Stream . CharNo  
    ; IF LCt > 0 
      THEN PadRel ( Stream , LCt , PadChar ) 
      END (* IF *) 
    END PadAbs 

(* VISIBLE: *) 
; PROCEDURE PadAbsWide 
    ( Stream : T ; Pos : CARDINAL ; PadChar : WIDECHAR := W' ' ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write MAX(0,Pos-CharNo(Stream)) copies of PadChar as wide characters. *) 

  = VAR LCt : INTEGER 

  ; BEGIN 
      LCt := Pos - Stream . CharNo  
    ; IF LCt > 0 
      THEN PadRelWide ( Stream , LCt , PadChar ) 
      END (* IF *) 
    END PadAbsWide 

(* VISIBLE: *) 
; PROCEDURE LineNo ( Stream : T ) : CARDINAL 
  (* The zero-origin line number currently being written into. *) 

  = BEGIN 
      RETURN Stream . LineNo 
    END LineNo  

(* VISIBLE: *) 
; PROCEDURE CharNo ( Stream : T ) : CARDINAL 
  (* The zero-origin character number about to be written-to. *) 

  = BEGIN 
      RETURN Stream . LineNo 
    END CharNo 

(* Constants, after module initialization. *) 
; VAR EolLen : INTEGER := 0 
; VAR EolWChar0 : WIDECHAR := VAL ( 0 , WIDECHAR ) 
; VAR EolWChar1 : WIDECHAR := VAL ( 0 , WIDECHAR ) 

; BEGIN (* Layout *) 
    EolLen := Text . Length ( Wr . EOL ) 
  ; EolWChar0 := VAL ( ORD ( Text . GetChar ( Wr . EOL , 0 ) ) , WIDECHAR )
  ; IF EolLen > 1 
    THEN 
      EolWChar1 := VAL ( ORD ( Text . GetChar ( Wr . EOL , 1 ) ) , WIDECHAR )
    END (* IF *) 
  END Layout
.
