
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE TempMark 
(* Package TempMark. 
   TempMark contains procedures for converting normal marks 
   to temporary marks and back, and for unmarking temporary 
   marks after parsing. 

   Temporary marks use a necessarily complicated scheme to keep
   them attached to any token or Mod during parse traversal and reparsing.
   Some of the tokens are implicit in the Est and some of the marks
   will be attached to text edits that are scanned, parsed, and put
   into the new Est in the normal way. 
*) 

; IMPORT Thread 

; IMPORT LbeStd 
; IMPORT ParseHs 
; IMPORT PaintHs 

; FROM Failures IMPORT Backout 

; PROCEDURE BuildTempMarkList 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; VAR ParseInfo : ParseHs . ParseInfoTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Convert the marks in an Est into a temp mark list 
     and mark bits in the Est nodes.  
     TempMarks could be located on any of the following:
     - ModBlankLine
     - ModCmnt
     - ModText
     - InsTok
     - AstString 
     - EOI
     TempMarks will not be attached to empty ModTexts (ModTexts with field
     ModTextStringRef = NIL ), except in the special case that there is 
     nothing but empty ModTexts on the line, in which case all the 
     TempMarks will be on the rightmost empty ModText.  
  *) 

; PROCEDURE RebuildMarkList 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; ImageRef : PaintHs . ImageTransientTyp 
    ; OldEstRef : LbeStd . EstRootTyp 
    ; NewEstRef : LbeStd . EstRootTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Rebuild marks from temporary marks after reparse. *) 

; PROCEDURE UnmarkEst 
    ( EstRef : LbeStd . EstRootTyp 
    ; RootAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Remove any leftover temp mark mark bits in the Est. 
     This is needed for the OLD Est, after reparsing. 
  *) 

; PROCEDURE DisconnectMarksFromLinesRefs 
    ( ImageTrans : PaintHs . ImageTransientTyp ) 
  (* For every mark, remove its reference to a LinesRef. *) 

; PROCEDURE SavePermanentMarks 
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; VAR SavedMarkListRef : ParseHs . TempMarkArrayRefTyp 
    ) 
  RAISES { Backout } 

; PROCEDURE RestorePermanentMarks 
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; SavedMarkListRef : ParseHs . TempMarkArrayRefTyp 
    ) 

; END TempMark 
. 
