
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Selection 

; IMPORT Thread 

; IMPORT PaintHs 
; IMPORT EditWindow 
; FROM Assertions IMPORT AssertionFailure 

; TYPE SelectionTyp 
  = OBJECT 
      PreselImage : PaintHs . ImageTransientTyp := NIL 
    ; PreselMark : PaintHs . LineMarkMeatTyp := NIL 
      (* The Presel* fields are for preselection.  We want not to clear
         the current selection on a new downclick, because it might be
         just a cursor-setting click, not the start of a drag.  So we
         store info about it here and leave the old selection unmolested
         unless/until there is a drag out of the character cell. 
      *) 

    ; SelText : TEXT := NIL 
      (* The selection could be stored here.  NIL if not. If so, use it.
         otherwise, Manifest the text from the variables below. 
      *) 
    ; SelImage : PaintHs . ImageTransientTyp := NIL 
    ; SelStartMark : PaintHs . LineMarkMeatTyp := NIL 
    ; SelEndMark : PaintHs . LineMarkMeatTyp := NIL 
      (*  ^These are image and marks for the start and end of the global
          selection.  All NIL if there is none. 
      *) 
    END (* SelectionTyp *) 

(* The one and only current selection, a truly global variable: *) 

; VAR Current : SelectionTyp := NIL 

; VAR KeepLeftoverMarks : BOOLEAN := TRUE  
  (* ^This is here for testing.  Set it TRUE, and every mouse click will leave
     a presel mark in the mark list forever, which provides a way to test 
     handling of multiple marks in flush temp edit and parse.
  *) 

; PROCEDURE ClearSelection  ( ) 
  RAISES { AssertionFailure , Thread . Alerted } 
  (* Does not clear preselection or SelText. *) 

(* VISIBLE: *) 
; PROCEDURE Preselect ( Window : PaintHs . WindowRefTyp ) 
  RAISES { AssertionFailure , Thread . Alerted } <* NOWARN *>
  (* Notes the current cursor location as preselection.  Leaves any 
     actual selection alone, in case there is no drag done. 
  *) 

; PROCEDURE DragSelection 
    ( Window : PaintHs . WindowRefTyp 
    ; AbsPosition : EditWindow . CharPointTyp 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

; PROCEDURE ManifestSelectionAsText ( ) : TEXT 
  RAISES { AssertionFailure , Thread . Alerted } <* NOWARN *> 

; END Selection 
. 