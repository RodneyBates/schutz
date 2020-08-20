
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Ui 

; IMPORT Thread 
; IMPORT VBT 

; IMPORT Assertions 
; IMPORT EditWindow 
; IMPORT LbeStd 
; IMPORT PaintHs 
; IMPORT ScannerIf
; IMPORT Worker 

<* PRAGMA LL *>

; PROCEDURE ReplayFileOpen ( FileName : TEXT ) 

; PROCEDURE ReplayFileCloseImage ( RecordedName : TEXT ) 

; PROCEDURE ReplayFileCloseWindow ( RecordedName : TEXT ; WindowNo : INTEGER ) 

; PROCEDURE ReplayFileSave ( RecordedName : TEXT ) 

; PROCEDURE ReplayFileSaveAs ( FileName : TEXT ) 

; PROCEDURE ReplayFileQuit ( ) 

; PROCEDURE ReplayFileExport ( FileName : TEXT ) 

; PROCEDURE ReplayEditCut ( ) 

; PROCEDURE ReplayEditCopy ( ) 

; PROCEDURE Paste ( Window : EditWindow . WindowTyp ; Time : VBT . TimeStamp ) 
  <* LL . sup <= VBT . mu *> 

; PROCEDURE ReplayEditPaste ( Window : EditWindow . WindowTyp ; Text : TEXT ) 

; PROCEDURE ReplaySemParse ( ) 

; PROCEDURE ReplaySemAccept ( ) 

; PROCEDURE ReplaySemAnalyze ( ) 

; PROCEDURE ReplayVertScroll 
    ( Max : INTEGER ; Thumb : INTEGER ; Value : INTEGER ) 

; PROCEDURE ReplayHorizScroll ( Value : INTEGER ) 

; PROCEDURE PromptAndCloseAllImages 
    ( Closure : Worker . ClosureTyp ; QuitAfter : BOOLEAN ) 
  RAISES { Assertions . AssertionFailure , Thread . Alerted } 
  (* PRE: Closure . Form only is set. Image is chosen inside. *)  
  (* On Worker thread. *) 

; PROCEDURE ParseKbd 
    ( Lang : LbeStd . LangTyp 
    ; ScanIf : ScannerIf . ScanIfTyp 
    ; PosRelTo : LbeStd . LimitedCharNoTyp 
    ; VAR NewTreeRef : LbeStd . EstRootTyp 
    ) 
  (* Parse from the keyboard. *) 

; PROCEDURE Install 
    ( EditFileName : TEXT 
    ; PlaybackFileName : TEXT 
    ; DoRunPlayback : BOOLEAN 
    ; RespectStops : BOOLEAN 
    ; RecordFileName : TEXT 
    ; DelayTime : INTEGER 
    ) 
  : BOOLEAN (* => Success. *) 
  RAISES { Assertions . AssertionFailure } 

(* Convenience procedures for setting standard fields of closures. *) 

; PROCEDURE SetImageTrans ( Closure : Worker . ClosureTyp ) 
  : Worker . ClosureTyp 
  (* PRE: ImageTrans is set, set ImagePers from it. *) 

; PROCEDURE SetImagePers ( Closure : Worker . ClosureTyp ) 
  : Worker . ClosureTyp 
  (* PRE: ImageTrans is set, set ImagePers from it. *) 

; PROCEDURE SetImageTransAndPers ( Closure : Worker . ClosureTyp ) 
  : Worker . ClosureTyp 
  (* PRE: Window is set, set ImageTrans and ImagePers from it. *) 

(* ************************************************************** *) 

; PROCEDURE DefaultWindow ( ) : PaintHs . WindowRefTyp 
  (* Temporary, while there is only one window. *) 

; END Ui 
. 
