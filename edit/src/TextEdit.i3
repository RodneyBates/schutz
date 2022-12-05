
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* TextEdit carries out text editing the Est and text painting. *) 

INTERFACE TextEdit 

; IMPORT Thread 

; IMPORT PaintHs 

; FROM Failures IMPORT Backout 

; PROCEDURE FlushEdit ( ImageRef : PaintHs . ImageTransientTyp ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE DeleteChar 
    ( WindowRef : PaintHs . WindowRefTyp ; DeletingBwd : BOOLEAN ) 
  RAISES { Backout , Thread . Alerted } 

  (* To delete a Nl, call DeleteChar with the cursor beyond 
     the last char of the line, and DeletingBwd false. *) 

; PROCEDURE InsertOrOverlayChar 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; NewChar : CHAR 
    ; IsInsert : BOOLEAN (* Otherwise Overlay *) 
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE InsertOrOverlayString  
    ( WindowRef : PaintHs . WindowRefTyp 
    ; String : TEXT 
    ; IsInsert : BOOLEAN (* Otherwise Overlay *) 
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE TransposeChars ( WindowRef : PaintHs . WindowRefTyp ) 
    RAISES { Backout , Thread . Alerted }  

; PROCEDURE DeleteRestOfLine ( WindowRef : PaintHs . WindowRefTyp ) 
    RAISES { Backout , Thread . Alerted } 

; PROCEDURE DeleteBetweenMarks 
    ( ImageTrans : PaintHs . ImageTransientTyp   
    ; FromMark : PaintHs . LineMarkMeatTyp 
    ; ThruMark : PaintHs . LineMarkMeatTyp
      (* Delete *thru* the line beginning at ThruMark (including its new line),
         but only *to* its LmCharPos. *)  
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE AcceptRepairUnderCursor ( WindowRef : PaintHs . WindowRefTyp ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE ToggleInsertMode ( Window : PaintHs . WindowRefTyp ) 
  : BOOLEAN (* Now Is insert. *) 

; PROCEDURE SetInsertMode 
    ( Window : PaintHs . WindowRefTyp ; Value : BOOLEAN ) 

; PROCEDURE BruteForceVerifyAllLinesRefs 
    ( ImageRef : PaintHs . ImageTransientTyp ; RepairIsOK : BOOLEAN ) 
  RAISES { Backout , Thread . Alerted } 
  (* Absent header is OK.
     Empty list is OK.
  *) 

; END TextEdit 
. 
