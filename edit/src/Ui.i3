
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Ui 

; IMPORT Font 
; IMPORT PaintOp 
; IMPORT Thread 
; IMPORT VBT 

; IMPORT EditWindow 
; FROM Failures IMPORT Backout 
; IMPORT LbeStd 
; IMPORT PaintHs 
; IMPORT ScannerIf
; IMPORT Worker 

<* PRAGMA LL *>

(* User interface appearance things of single-instance. *)

; TYPE PaintOpsTyp  
    = ARRAY PaintHs . TextAttrComponentTyp 
      OF PaintOp . T 

; TYPE PaintOps2DTyp  
    = ARRAY PaintHs . TextAttrComponentTyp , PaintHs . TextAttrComponentTyp
      OF PaintOp . T 

; TYPE FontsTyp = ARRAY PaintHs . TextAttrComponentTyp OF Font . T 

; TYPE DerivedInfoRefTyp = REF DerivedInfoTyp 

; TYPE DerivedInfoTyp 
    = RECORD
        DiPaintOpBg : PaintOp . T 
      ; DiPaintOpBorder : PaintOp . T 
      ; DiPaintOpFg : PaintOp . T 
      ; DiPaintOpBgFg : PaintOp . T 
      ; DiPaintOps2D : PaintOps2DTyp 
      ; DiPaintOpsBg : PaintOpsTyp 
      ; DiPaintOpsDec : PaintOpsTyp 
      ; DiPaintOpsChar : PaintOpsTyp 
      END (* DerivedInfoTyp *)

; VAR GDerivedInfoRef : DerivedInfoRefTyp

; PROCEDURE ComputeDerivedInfo ( Info : DerivedInfoRefTyp )

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
  RAISES { Backout , Thread . Alerted } 
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
  RAISES { Backout } 

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
