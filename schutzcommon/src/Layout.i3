  
(* -----------------------------------------------------------------------1- *)
(* File Layout.i3  Modula-3 source code.                                     *)
(* Copyright 2020, Rodney M. Bates.                                          *)
(* rbates@acm.org                                                            *)
(* Licensed under the MIT license.                          .                *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Layout 

(* A layer on top of Wr that counts lines and characters. *) 

; IMPORT Thread
; IMPORT Wr 

; TYPE T <: ROOT   

; CONST Brand = "Layout-0.1"

(* NOTE on counting characters:  This module counts either a CHAR or a
   WIDECHAR as one character, for purposes of maintaining CharNo.  If
   you are writing exclusively one or the other, or perhaps doing some
   kind of left-to-right self-describing variable-length encoding that
   readers of your stream can decode from the byte stream alone, this is
   probably what you want.  Otherwise, using this module or even counting
   characters at all may not make much sense.
*)  

(* NOTE on encoding of ends-of-lines:  This module uses the host-dependent
   Wr.EOL as the end-of-line sequence, both for inserting and recognizing.  
   When this is two characters, it treats an incomplete sequence as an
   ordinary character.
*)    

; TYPE TabCtFuncTyp = PROCEDURE ( CharNo : CARDINAL ) : CARDINAL 
  (* A callback to determine how many blanks a tab at position 
     CharNo counts as.
  *) 

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

; PROCEDURE DefaultTabCtFunc ( CharNo : CARDINAL ) : CARDINAL 
  (* Tab stops wherever (zero-origin)CharNo MOD 8 = 0. *) 

; PROCEDURE UsedStream ( Stream : T ) : Wr . T 
  (* The value of UseStream that was passed to Stream when it was initialized. *) 

; PROCEDURE PutText ( Stream : T ; Tx : TEXT ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write characters of Tx as CHARs to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in Tx
     and adjacent characters. 
  *) 

; PROCEDURE PutWideText ( Stream : T ; Tx : TEXT ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write chars of Tx as WIDECHARs to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in Tx
     and adjacent characters. 
  *) 

; PROCEDURE PutChar ( Stream : T ; Ch : CHAR ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write Ch to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in Ch
     and adjacent characters. 
  *) 

; PROCEDURE PutWideChar ( Stream : T ; WCh : WIDECHAR ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write WCh to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in WCh
     and adjacent characters. 
  *) 

; PROCEDURE PutString ( Stream : T ; READONLY String : ARRAY OF CHAR ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write String as CHARs to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in String
     and adjacent characters. 
  *) 

; PROCEDURE PutWideString ( Stream : T ; READONLY WString : ARRAY OF WIDECHAR ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write WString as WIDECHARs to UsedStream(Stream), which must be open. 
     If Stream was initialized with Simple=FALSE, check 
     for and take into account, tabs and end-of-line sequences in WString
     and adjacent characters. 
  *) 

; PROCEDURE PutEol ( Stream : T ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write an end-of-line sequence, as CHARs, to UsedStream(Stream).  
     If Stream was initialized with Simple, calling this is the only way that 
     will be recognized as affecting CurrentLine and CharPos. 
  *) 

; PROCEDURE PutWideEol ( Stream : T ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write an end-of-line sequence, as WIDECHARs, to UsedStream(Stream).  
     If Stream was initialized with Simple, calling this is the only way that 
     will be recognized as affecting CurrentLine and CharPos. 
  *) 

; PROCEDURE PutTab ( Stream : T ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a horizontal tab, as a CHAR, to UsedStream(Stream).  
     If Stream was initialized with Simple, calling this is the only way that 
     will be recognized as affecting CharPos. 
     Use callback TabCtFunc, as was supplied to Init, to ascertain how far to 
     tab to. Do not expand the tab into blanks in the output, only count its 
     possibly multiple spaces for maintaining character number.  
  *) 

; PROCEDURE PutWideTab ( Stream : T ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a horizontal tab, as a WIDECHAR, to UsedStream(Stream).  
     If Stream was initialized with Simple, calling this is the only way that 
     will be recognized as affecting CharPos. 
     Use callback TabCtFunc, as was supplied to Init, to ascertain how far to 
     tab to. Do not expand the tab into blanks in the output, only count its 
     possibly multiple spaces for maintaining character number.  
  *) 

; PROCEDURE PadRel ( Stream : T ; Ct : CARDINAL ; PadChar : CHAR := ' ' ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write Ct copies of PadChar. *) 

; PROCEDURE PadRelWide 
    ( Stream : T ; Ct : CARDINAL ; PadChar : WIDECHAR := W' ' ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write Ct copies of PadChar as wide characters. *) 

; PROCEDURE PadAbs ( Stream : T ; Pos : CARDINAL ; PadChar : CHAR := ' ' ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write MAX(0,Pos-CharNo(Stream)) copies of PadChar. *) 

; PROCEDURE PadAbsWide 
    ( Stream : T ; Pos : CARDINAL ; PadChar : WIDECHAR := W' ' ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write MAX(0,Pos-CharNo(Stream)) copies of PadChary. *) 

; PROCEDURE LineNo ( Stream : T ) : CARDINAL 
  (* The zero-origin line number currently being written into. *) 

; PROCEDURE CharNo ( Stream : T ) : CARDINAL 
  (* The zero-origin character number about to be written-to. *) 

; END Layout
.

