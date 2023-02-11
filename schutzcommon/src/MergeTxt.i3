
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2023, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE MergeTxt 

(* This module merges a localized set of text edit changes into an Est. *) 

; IMPORT Thread 

; IMPORT EstHs 
; IMPORT LbeStd 
; IMPORT Marks 
; IMPORT Strings 

; FROM Failures IMPORT Backout 

(* There could be up to four new TokMarks, denoting new beginnings of
   lines, built by MergeTextEdit. If the old line is actually a blank
   line mod, it could be split into two blank lines with a text
   insertion line between.  Furthermore, the insertion could have an
   imbedded new line, making it two text insertions.  If the line is
   not a blank line, it can become two if there is a new line in the
   insertion. *)

; CONST TokMarkCt = 4 
; TYPE TokMarkSsTyp = [ 0 .. TokMarkCt - 1 ] 
; TYPE EstRefArrayTyp = ARRAY TokMarkSsTyp OF LbeStd . EstRootTyp   

; PROCEDURE MergeTextEdit 
    ( Lang : LbeStd . LangTyp 
    ; EstRootRef : EstHs . EstRefTyp 
    ; READONLY StartTokMark : Marks . TokMarkTyp 
    ; READONLY EndTokMark : Marks . TokMarkTyp 
    ; BlankLineNo : LbeStd . LineNoTyp 
      (* Line number within a blank line mod or EndOfImage. *) 
    ; DelFromPos : LbeStd . LimitedCharNoTyp 
    ; DelToPos : LbeStd . LimitedCharNoTyp 
      (* ^Chars in [ DelFromPos .. DelToPos - 1 ] are deleted and replaced
          by those in InsText [ DelFromPos .. DelFromPos + InsLen - 1 ] *) 
    ; READONLY InsText : Strings . StringTyp  
      (* ^This is an image of the entire line, after the edits to be merged. *)
    ; InsLen : LbeStd . LimitedCharNoTyp 
    ; InsNlPos : LbeStd . LimitedCharNoTyp 
      (* ^Position in the edited line before which a new line goes. 
          LbeStd . LimitedCharNoInfinity if no new line at all. 
          Otherwise, must lie in the interval 
          [ DelFromPos , DelFromPos + InsLen ] *) 
    ; NlIndentPos : LbeStd . LimitedCharNoTyp 
      (* ^If InsNlPos # LbeStd . LimitedCharNoInfinity, this the amount 
          of indentation of the new text line, after the inserted Nl. 
          Ignored otherwise. *) 
    ; DelNlShift : LbeStd . LimitedCharNoTyp 
      (* ^When LbeStd . LimitedCharNoInfinity, no new line is deleted. 
          Otherwise, the new line at the end of the line denoted by 
          BolTokMark/BlankLineNo is deleted, and DelNlShift is the 
          amount to add to character positions on the formerly following 
          line to get it appended to the first line. 
          InsNlPos and DelNlShift cannot both be unequal to 
          LbeStd . LimitedCharNoInfinity. 
      *) 
    ; VAR NewEstRootRef : EstHs . EstRefTyp  
    ; VAR NodeNoChange : LbeStd . EstNodeNoTyp 
    ; VAR MaxTouchedNodeNo : LbeStd . EstNodeNoTyp 
      (* ^Est nodes with old node numbers > MaxTouchedNodeNo will have node
          numbers in the new tree that differ by NodeNoChange, which can be
          negative or positive.  Any references to them need to be adjusted
          to match.  MergeTextEdit doesn't know where they are, so caller
          must do it.  OTOH, MergeTextEdit does adjust node numbers in
          marks it produces.
      *) 
    ; VAR NewBolTokMark : Marks . TokMarkTyp 
    ; VAR NewLinesCt : LbeStd . LineNoTyp 
          (* ^A ModBlankLine counts only as one here, regardless of
             ModBlankLineCt. *) 
    ; VAR LeadingBlankLinesIncluded : LbeStd . LineNoTyp  
      (* ^This many lines from leading blank mods were incorporated into the 
         resulting set of tree children. *)  
    ; VAR TrailingBlankLinesIncluded : LbeStd . LineNoTyp  
      (* ^Similarly, for trailing mods. *) 
    ) 
  RAISES { Backout , Thread . Alerted } 

  (* MergeTextEdit puts a single location group of text modifications
     into an Est.  The string at ModTextStringRef will have no leading
     or trailing blanks.  ModTextStringRef will be NIL instead of
     pointing to an empty string. *)

; END MergeTxt 
. 
