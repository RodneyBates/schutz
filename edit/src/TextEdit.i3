
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This module carries out various text editing operations. *) 

INTERFACE TextEdit 

; IMPORT Thread 

; IMPORT PaintHs 

; FROM Assertions IMPORT AssertionFailure 

; PROCEDURE FlushEdit ( ImageRef : PaintHs . ImageTransientTyp ) 
  RAISES { AssertionFailure , Thread . Alerted } 

; PROCEDURE DeleteChar 
    ( WindowRef : PaintHs . WindowRefTyp ; DeletingBwd : BOOLEAN ) 
  RAISES { AssertionFailure , Thread . Alerted } 

  (* To delete a Nl, call DeleteChar with the cursor beyond 
     the last char of the line, and DeletingBwd false. *) 

; PROCEDURE InsertOrOverlayChar 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; NewChar : CHAR 
    ; IsInsert : BOOLEAN (* Otherwise Overlay *) 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

; PROCEDURE InsertOrOverlayString  
    ( WindowRef : PaintHs . WindowRefTyp 
    ; String : TEXT 
    ; IsInsert : BOOLEAN (* Otherwise Overlay *) 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

; PROCEDURE TransposeChars ( WindowRef : PaintHs . WindowRefTyp ) 
    RAISES { AssertionFailure , Thread . Alerted }  

; PROCEDURE DeleteRestOfLine ( WindowRef : PaintHs . WindowRefTyp ) 
    RAISES { AssertionFailure , Thread . Alerted } 

; PROCEDURE DeleteBetweenMarks 
    ( ImageTrans : PaintHs . ImageTransientTyp   
    ; FromMark : PaintHs . LineMarkMeatTyp 
    ; ThruMark : PaintHs . LineMarkMeatTyp 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

; PROCEDURE AcceptRepairUnderCursor ( WindowRef : PaintHs . WindowRefTyp ) 
  RAISES { AssertionFailure , Thread . Alerted } 

; PROCEDURE ToggleInsertMode ( Window : PaintHs . WindowRefTyp ) 
  : BOOLEAN (* Now Is insert. *) 

; PROCEDURE SetInsertMode 
    ( Window : PaintHs . WindowRefTyp ; Value : BOOLEAN ) 

; PROCEDURE BruteForceVerifyAllLinesRefs 
    ( ImageRef : PaintHs . ImageTransientTyp ; RepairIsOK : BOOLEAN ) 
  RAISES { AssertionFailure , Thread . Alerted } 
  (* Absent header is OK.
     Empty list is OK.
  *) 

; END TextEdit 
. 
