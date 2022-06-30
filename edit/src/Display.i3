
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Display 

; IMPORT Thread 

; IMPORT LbeStd 
; IMPORT EditWindow 
; IMPORT Marks 
; IMPORT PaintHs 
; IMPORT Errors 

; FROM Failures IMPORT Backout 

<* PRAGMA LL *> 

(* Global variables for state of on-screen selection: *) 

; VAR GPreselImageTrans : PaintHs . ImageTransientTyp := NIL 
; VAR GPreselMark : PaintHs . LineMarkMeatTyp := NIL 

; VAR GTextSel : TEXT := NIL 
  (* The selection could be stored here.  If so, use it.
     othewise, Manifest the text from the variables below. 
  *) 
; VAR GSelImageTrans : PaintHs . ImageTransientTyp := NIL 
; VAR GStartSelMark : PaintHs . LineMarkMeatTyp := NIL 
; VAR GEndSelMark : PaintHs . LineMarkMeatTyp := NIL 
  (*  ^These are image and marks for the start and end of the global
      selection.  All NIL if there is none. 
  *) 

; PROCEDURE LinesRefIsBegOfImage 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ) 
    : BOOLEAN 

; PROCEDURE LinesRefIsEndOfImage 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ) 
    : BOOLEAN 

; PROCEDURE LineCtOfBolTokMark 
    ( EstRoot : LbeStd . EstRootTyp ; TokMark : Marks . TokMarkTyp ) 
  : LbeStd . LineNoTyp 
  RAISES { Backout } 
  (* For a TokMark identifying a regular LinesRef (not Blank Lines), will be 
     zero.  Also zero for a blank line, start at end. 
     otherwise the count of blank lines. *) 

; PROCEDURE ActualNoOfLines 
    ( LineCt : LbeStd . LineNoTyp ) : LbeStd . LineNoTyp 
  (* LineCt values have the invariant that they 
     are zero for a text-containing LinesRef or EndOfImage.  This allows 
     Lines to be easily detected. But sometimes you just 
     want the actual LineCt, regardless of whether this is 
     a blank line or not.  This function gives this. *) 

; PROCEDURE ActualLineCtOfLinesRef 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ) 
    : LbeStd . LineNoTyp 

; PROCEDURE LineNoInWindow 
    ( (* UNCHANGED *) WindowRef : PaintHs . WindowRefTyp 
    ; DesiredLinesRef : PaintHs . LinesRefMeatTyp 
    ) 
  : LbeStd . LineNoSignedTyp 
  RAISES { Backout } 
  (* DesiredLinesRef points to a line rec known to be visible in 
     the window described by WindowRef. Return the window-relative 
     line number of the first line represented by DesiredLinesRef. 
     The first line visible in the window is number 0.  The 
     result could be < 0, since DesiredLinesRef could point to a 
     blank line with > 1 line and some of these are above the 
     top of the window. *) 

; PROCEDURE SecureSucc 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Ensure that the successor of LinesRef is really the 
     following line, i.e. NOT LinesRef.GapAfter. *) 

; PROCEDURE SecurePred 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Ensure that the predecessor of LinesRef is really the 
     previous line, i.e. NOT predecessor.GapAfter. *) 

; PROCEDURE ClearWindow ( WindowRef : PaintHs . WindowRefTyp ) 
  <* LL.sup < Window *> 

; PROCEDURE PaintTempEditedLine 
    ( (* UNCHANGED *) WindowRef : PaintHs . WindowRefTyp 
    ; LineNoInWindow : LbeStd . LineNoSignedTyp 
    ; TempEditRef : PaintHs . TempEditRefTyp 
    ; READONLY BolTokMark : Marks . TokMarkTyp 
    ; LineNo : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout } 

; PROCEDURE PaintUneditedNonblankLine 
    ( (* UNCHANGED *) WindowRef : PaintHs . WindowRefTyp 
    ; LineNoInWindow : LbeStd . LineNoSignedTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ) 
  RAISES { Backout } 

; PROCEDURE PaintLinesRangeOneWindow 
    ( Window : PaintHs . WindowRefTyp 
    ; FromLinesRef : PaintHs . LinesRefMeatTyp  
    ; FromLineNo : LbeStd . LineNoTyp
    ; ThruLinesRef : PaintHs . LinesRefMeatTyp  
    ; ThruLineNo : LbeStd . LineNoTyp
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE PaintLinesRangeAllWindows 
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; FromLinesRef : PaintHs . LinesRefMeatTyp  
    ; FromLineNo : LbeStd . LineNoTyp
    ; ThruLinesRef : PaintHs . LinesRefMeatTyp  
    ; ThruLineNo : LbeStd . LineNoTyp
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE PaintWindowFromLines 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; VAR BlankLinesAtEnd : LbeStd . LineNoTyp 
    ; VAR ToLinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR ToLineNo : LbeStd . LineNoSignedTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE HorizScrollWindowRefToContainCursor 
    ( (* IN OUT *) WindowRef : PaintHs . WindowRefTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
      (* Set true if anything requiring a repaint happens. *) 
    ) 
  (* Absolute cursor position doesn't change.  Window moves 
     the minimum necessary to contain it. *) 

; PROCEDURE HorizMoveCursorWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; CursorMovement : EditWindow . CharCoordTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
      (* Set true if anything requiring a repaint happens. *) 
    ) 
  RAISES { Backout } 
  (* Will scroll WindowRef if necessary to keep cursor inside. *) 

; PROCEDURE HorizMoveCursorAndRepaintWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; CursorMovement : EditWindow . CharCoordTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Will scroll WindowRef if necessary to keep cursor inside. *) 

; PROCEDURE NonblankLengthOfCurrentLine ( WindowRef : PaintHs . WindowRefTyp ) 
  : LbeStd . CharNoTyp 

; PROCEDURE HorizMoveCursorToEndAndRepaint 
    ( WindowRef : PaintHs . WindowRefTyp ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE HorizScrollWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; WindowMovement : LbeStd . LimitedCharNoSignedTyp 
      (* ^Of window frame, relative to contents. *) 
    ; DoDragCursor : BOOLEAN 
      (* ^Means keep cursor in same window-relative position. 
          Even if NOT DoDragCursor, it will be dragged to 
          prevent its getting outside the window. *) 
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE CreateEmptyLinesRefList 
    ( (* IN OUT *) ImageRef : PaintHs . ImageTransientTyp ) 

; PROCEDURE InitImageFirstWindow ( ImageTrans : PaintHs . ImageTransientTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* Creates a mark list if there is none.  
     Creates a cursor at BOI if there is no cursor. 
     Leaves line list untouched. 
  *) 

; PROCEDURE PaintWindowFromCursor 
    ( (* IN OUT *) WindowRef : PaintHs . WindowRefTyp 
    ; VAR BlankLinesAtEnd : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* The cursor must point to a valid LinesRef. *) 

; PROCEDURE ReconstructLinesAndPaint 
    ( (* IN OUT *) ImageRef : PaintHs . ImageTransientTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Any temp edits have been flushed.  
     Depends only on the list of MarkRecs.  Completely rebuilds 
     LinesRefs for the image and all the pointers thereto from 
     the MarkRecs and WindowRecs. Then repaints all windows of 
     this image. 
  *) 

; PROCEDURE MakeLinesRefsNotVisible 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; VAR (* IN OUT *) FromLinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR (* IN OUT *) ThruLinesRef : PaintHs . LinesRefMeatTyp 
      (* ^After return, FromLinesRef, ThruLinesRef, and/or 
         any of the LinesRefs between might have been unlinked. 
         None of these pointers should be used or stored 
         by the caller. *) 
    ) 
  RAISES { Backout } 
  (* FromLinesRef .. ThruLinesRef, belonging to ImageRec, have 
     become invisible altogether.  Unlink any LinesRefs which 
     should no longer be retained. *) 

; PROCEDURE MakeLinesRefsNotVisibleInWindow 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; VAR (* IN OUT *) FromLinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR (* IN OUT *) ThruLinesRef : PaintHs . LinesRefMeatTyp 
      (* ^FromLinesRef, ThruLinesRef, and/or any of the lines 
         between might have been unlinked.  None of these 
         pointers should be used or stored by the caller. *) 
    ; WindowNo : PaintHs . WindowNoTyp 
    ) 
  RAISES { Backout } 
  (* FromLinesRef .. ThruLinesRef are no longer visible in WindowNo. 
     Node this in the LinesRefs' LrVisibleIn fields. 
     If some of the LinesRefs become invisible altogether, 
     call MakeLinesRefsNotVisible for them. *) 

; PROCEDURE MoveCursorWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; WantedMovement : EditWindow . CharPointTyp 
    ; VAR (* OUT *) ActualMovement : EditWindow . CharPointTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE MoveCursorAndRepaintWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; WantedMovement : EditWindow . CharPointTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE MoveCursorAbsoluteInsideWindow 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; AbsPosition : EditWindow . CharPointTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* AbsPosition must lie within the window. *) 

; PROCEDURE AbsoluteCursorPositionInWindow 
    ( WindowRef : PaintHs . WindowRefTyp ) 
  : EditWindow . CharPointTyp 
  RAISES { Backout , Thread . Alerted } 
  (* AbsPosition must lie within the window. *) 

; PROCEDURE VertScrollAndRepaint 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; WantedMovement : LbeStd . LineNoSignedTyp 
      (* Positive means window frame moves down, relative to image. *) 
    ; DoDragCursor : BOOLEAN 
      (* ^Means keep cursor in same window-relative position. 
          Even if NOT DoDragCursor, it will be dragged to 
          prevent its getting outside the window. *) 
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE ReshapeWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; OldSize : EditWindow . CharPointTyp 
    ; NewSize : EditWindow . CharPointTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

; VAR GMostRecentBeepCode : Errors . ErrorTyp 

; PROCEDURE Beep ( Code : Errors . ErrorTyp ) 

; PROCEDURE NoteImageSavedState 
    ( ImageRef : PaintHs . ImageTransientTyp ; State : BOOLEAN ) 

; PROCEDURE NoteImageParsedState 
    ( ImageRef : PaintHs . ImageTransientTyp ; State : BOOLEAN ) 

; PROCEDURE NoteImageAnalyzedState 
    ( ImageRef : PaintHs . ImageTransientTyp ; State : BOOLEAN ) 

; PROCEDURE Reparse ( (* IN OUT *) ImageRef : PaintHs . ImageTransientTyp ) 
  RAISES { Backout , Thread . Alerted } 

; END Display 
. 
