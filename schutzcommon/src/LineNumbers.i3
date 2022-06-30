
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LineNumbers 

(* Some routines for manipulating line numbers of displayed code. *) 

; IMPORT Wr 
; IMPORT Thread 

; IMPORT EstHs  
; FROM Failures IMPORT Backout 
; IMPORT LbeStd 
; IMPORT PaintHs  

; PROCEDURE LineCtExport ( ImageRef : PaintHs . ImageTransientTyp ) 
  : LbeStd . LineNoTyp 
  RAISES { Backout , Thread . Alerted } 
  (* Count of lines as would be exported to a text file, i.e., with
     proposed but not accepted syntactic corrections removed. *)  

; PROCEDURE LineCtDisplay 
    ( ImageRef : PaintHs . ImageTransientTyp ) 
  : LbeStd . LineNoTyp 
  RAISES { Backout , Thread . Alerted } 
  (* Count of lines as would appear in a window.  This can differ
     from LineCtExport in case of repairs shown in a window but not in a file. 
  *)  
  (* Compute line count by repeated invocation of LineMarks . GetNextLine *) 

; PROCEDURE WriteDisplay 
    ( ImageRef : PaintHs . ImageTransientTyp ; WrT : Wr . T ) 
  RAISES { Backout , Thread . Alerted } 
  (* Write to WrT, by repeated invocation of LineMarks . GetNextLine *) 

; PROCEDURE EstimateOfLinesInSubtree 
    ( READONLY WidthInfo : EstHs . WidthInfoTyp 
    ; LineBreakCt : LbeStd . LineNoSignedTyp 
    ) 
  : LbeStd . LineNoSignedTyp  
  RAISES { Backout } 
  (* Actually, of the number of new lines at the top level. *) 

; PROCEDURE EstimateLineNo
    ( LineMark : PaintHs . LineMarkMeatTyp 
    ; ImageRef : PaintHs . ImageTransientTyp 
    ; VAR LineNo : LbeStd . LineNoTyp 
    ; VAR IsExact : BOOLEAN 
    ) 
  RAISES { Backout } 

; END LineNumbers 
. 
