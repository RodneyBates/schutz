
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LineMarks 

(* This module contains procedures for constructing a line mark 
   to the previous or the next new line, given an existing line 
   mark.  GetPrevLine can start with a mark to 
   any point within a line.  GetNextLine must begin 
   at a new line, because it also constructs the text image of 
   the line as it traverses it, and this requires knowing the 
   positions on the line. 
 
   GetPrevLine cannot always stop at the new line immediately to 
   the left, because for some line breaks and implicit new lines, 
   it can't tell whether the new line is taken.  In this case, it
   assumes not and keeps traversing leftward.  It takes parameter 
   ExistingMark, that the caller presumably believes lies to the 
   left of the start mark.  If GetPrevLine hits this, it will stop 
   there, even if it otherwise wouldn't. 
*) 

; IMPORT Thread 

; IMPORT LbeStd 
; IMPORT Marks 
; IMPORT Strings 
; IMPORT PaintHs 

; FROM Assertions IMPORT AssertionFailure 

; PROCEDURE GetNextLine 
    ( Lang : LbeStd . LangTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; READONLY StartMark : Marks . TokMarkTyp 
    ; READONLY ExistingMark : Marks . TokMarkTyp 
    ; VAR DidHitExistingMark : BOOLEAN 
    ; VAR NewMark : Marks . TokMarkTyp 
    ; VAR AtEndOfImage : BOOLEAN 
      (* ^TRUE if StartMark was at EOI, though not necessarily the 
         rightmost Nl of an adjacent group. *) 
    ; VAR LineText : Strings . StringTyp 
    ; VAR BlankLineCt : LbeStd . LineNoTyp 
    ; VAR TextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
    ; VAR LineErrArrayRef : PaintHs . LineErrArrayRefTyp 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

; PROCEDURE GetPrevLine 
    ( Lang : LbeStd . LangTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; READONLY StartMark : Marks . TokMarkTyp 
    ; READONLY ExistingMark : Marks . TokMarkTyp 
    ; VAR NewMark : Marks . TokMarkTyp 
    ; VAR AtBegOfImage : BOOLEAN 
      (* ^TRUE if StartMark was at BOI, though not necessarily the 
         leftmost Nl of an adjacent group. *) 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

; PROCEDURE GetLMBegOfImage 
    ( Lang : LbeStd . LangTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; VAR NewMark : Marks . TokMarkTyp 
    ) 
  RAISES { AssertionFailure } 
  (* This will always be the BOI FS item, which is not necessarily the
     normal rightmost of all marks that lead to the same Nl.
  *) 

; PROCEDURE GetRMBegOfImage 
    ( Lang : LbeStd . LangTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; VAR NewMark : Marks . TokMarkTyp 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 
  (* The normal rightmost of all marks that lead to the BOI Nl. *) 

; PROCEDURE GetEndOfImage 
    ( Lang : LbeStd . LangTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; VAR NewMark : Marks . TokMarkTyp 
    ) 
  RAISES { AssertionFailure } 

; END LineMarks 
. 
