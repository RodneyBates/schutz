
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Parser 

(* Parsing.  Includes batch parsing from a file and in incremental
   (re)parsing of an Est with possibly text edits embedded within.
   Handles sytax error recovery/repair and (re)building of the Est.
*)  

; IMPORT Thread 

; IMPORT LbeStd 
; IMPORT ParseHs 

; FROM Assertions IMPORT AssertionFailure 

; PROCEDURE Parse 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; ParseTravStateRef : ParseHs . ParseTravStateRefTyp
    ; VAR NewTreeRef : LbeStd . EstRootTyp 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

; END Parser 
. 
