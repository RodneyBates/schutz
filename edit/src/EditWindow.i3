
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE EditWindow 

; FROM Failures IMPORT Backout 
; IMPORT FormsVBT  
; IMPORT Point 
; IMPORT VBT 

; IMPORT Strings 
; IMPORT PaintHs 

<* PRAGMA LL *> 

(* The following are chosen so CharPointTyp is structurally equal to Point.T. 
   But we don't just refer to Point.T , in case some implementation of 
   this interface wants to use a GUI system other than Trestle. *) 

; TYPE CharCoordTyp = INTEGER 
; TYPE CharPointTyp = RECORD h , v : CharCoordTyp END (* RECORD *)
  (* ^ This is structurally = Point.T, but has s different name
       to help avoid mixups between points measured in pixels
       (use Point.T), and Characters (use CharPointTyp).
  *)
; TYPE PixelCoordTyp = INTEGER 

; TYPE T = WindowTyp 

; TYPE WindowTyp <: WindowPublic 

; TYPE WindowPublic = PaintHs . WindowRefTyp 

; CONST MinVertGap = 3 (* Vertical pixels between rows of text. *) 

; PROCEDURE Init 
    ( Window : T 
    ; Form : FormsVBT . T 
    ; Margin : Point . T := Point . Origin 
      (* Which means some other default. *) 
    ; VertGap : PixelCoordTyp := MinVertGap 
    ) 
  : T 
  RAISES { Backout }
  <* LL.sup < Window *> 

; TYPE CursorStateTyp 
    = { CsBlinkingOff   (* Used only in stored state *) 
      , CsBlinkingOn    (* Used only in stored state *) 
      , CsInvisible , CsGrayed , CsSolid 
      , CsBlinking      (* Used only internally. *) 
      } 

; TYPE CursorSetStateTyp 
    = [ CursorStateTyp . CsInvisible .. CursorStateTyp . CsBlinking ] 

; PROCEDURE Form ( Window : T ) : FormsVBT . T
  (* The FormsVBT.T that this window is inside of. *)    

; PROCEDURE SouthEastToCorner ( Window : WindowTyp ) : CharPointTyp 
  (* Follows the usual _to_ invariant, i.e. the first char _not_ included. *) 

; PROCEDURE SetCursorPosition 
    ( Window : WindowTyp 
    ; CharPosInWindow : CharCoordTyp 
    ; LineNoInWindow : CharCoordTyp 
    ) 
  <* LL . sup < Window . mu *> 

; PROCEDURE PaintCursorCoordinates ( Window : WindowTyp ) 

; PROCEDURE UpdateVertScroller ( Window : WindowTyp ) 
  <* LL.sup < Window *> 

; PROCEDURE UpdateHorizScroller ( Window : WindowTyp ) 
  <* LL.sup < Window *> 

; PROCEDURE MakeCursorVisible ( Window : WindowTyp ) 
  <* LL . sup < BlinkerLock *> 

; PROCEDURE PaintWindowBlank ( Window : WindowTyp ) 
  <* LL . sup < Window . mu *> 

; PROCEDURE PaintLine 
    ( Window : WindowTyp 
    ; FromPosInWindow : CharCoordTyp 
    ; LineNoInWindow : CharCoordTyp 
    ; PaintText : TEXT (* May be NIL *) 
    ; FromSsInString : Strings . StringSsTyp 
    ; ToSsInString : Strings . StringSsTyp 
    ; Attr : PaintHs . TextAttrTyp  
    ) 
  <* LL . sup < Window . mu *> 

; PROCEDURE ClearLine 
    ( Window : WindowTyp 
    ; FromPosInWindow : CharCoordTyp 
    ; LineNoInWindow : CharCoordTyp 
    ) 
  <* LL . sup < Window . mu *> 

; PROCEDURE PaintLeftArrow
    ( Window : WindowTyp 
    ; LineNoInWindow : CharCoordTyp 
    ; IsVisible : BOOLEAN 
    ) 
  <* LL . sup < Window . mu *> 

; PROCEDURE PaintRightArrow 
    ( Window : WindowTyp 
    ; LineNoInWindow : CharCoordTyp 
    ; IsVisible : BOOLEAN 
    ) 
  <* LL . sup < Window . mu *> 

; PROCEDURE PaintBackground 
    ( Window : WindowTyp 
    ; LineNoInWindow : CharCoordTyp 
    ; FromPosInWindow : CharCoordTyp 
    ; Len : CharCoordTyp 
    ; BgColor : PaintHs . TextAttrComponentTyp 
    ) 
  <* LL . sup < Window . mu *> 

; PROCEDURE PaintText 
    ( Window : WindowTyp ; VbtName : TEXT ; Value : TEXT ) 

; PROCEDURE PaintWindowState 
    ( Window : WindowTyp ; VbtName : TEXT ; State : INTEGER ) 

; PROCEDURE PaintWindowSavedState ( Window : WindowTyp ; State : BOOLEAN ) 

; PROCEDURE PaintInsertMode ( Window : WindowTyp ) 

; PROCEDURE Beep ( ) 

; PROCEDURE TakeKBFocus ( Window : WindowTyp ; TimeStamp : VBT . TimeStamp ) 
  <* LL . sup < Window . mu *> 

; PROCEDURE ScreenSelection ( ) : VBT . Selection 

; PROCEDURE ReplayMouseClickPixel 
    ( Window : WindowTyp ; H : INTEGER ; V : INTEGER ) 

; PROCEDURE ReplayMouseClickChar  
    ( Window : WindowTyp ; H : INTEGER ; V : INTEGER ) 

; PROCEDURE ReplayClearSelection ( ) 

; PROCEDURE ReplaySweepSelection 
    ( Window : WindowTyp ; H : INTEGER ; V : INTEGER ) 

; PROCEDURE ReplayEndSweepSelection ( Window : WindowTyp ) 
  <* LL . sup < Window . mu *> 

; PROCEDURE ReplayBeginKey ( Window : WindowTyp ) 

; PROCEDURE ReplayEndKey ( Window : WindowTyp ) 

; PROCEDURE ReplayCharDelFwd ( Window : WindowTyp ) 

; PROCEDURE ReplayCharDelBwd ( Window : WindowTyp ) 

; PROCEDURE ReplayDeleteRestOfLine ( Window : WindowTyp ) 

; PROCEDURE ReplayCharTranspose ( Window : WindowTyp ) 

; PROCEDURE ReplayCursorLeft ( Window : WindowTyp ) 

; PROCEDURE ReplayCursorRight ( Window : WindowTyp ) 

; PROCEDURE ReplayCursorUp ( Window : WindowTyp ) 

; PROCEDURE ReplayCursorDown ( Window : WindowTyp ) 

; PROCEDURE ReplayPriorKey ( Window : WindowTyp ) 

; PROCEDURE ReplayNextKey ( Window : WindowTyp ) 

; PROCEDURE ReplayHomeKey ( Window : WindowTyp ) 

; PROCEDURE ReplaySetInsertMode ( Window : WindowTyp ; Value : BOOLEAN ) 

; PROCEDURE ToggleInsertMode ( Window : WindowTyp ) 

; PROCEDURE ReplayCharType ( Window : WindowTyp ; Ch : CHAR ) 

; PROCEDURE BeginPaintGroup ( Window : WindowTyp ) 
  <* LL . sup < Window . mu *> 

; PROCEDURE EndPaintGroup ( Window : WindowTyp ) 
  <* LL . sup < Window . mu *> 

; END EditWindow 
. 

