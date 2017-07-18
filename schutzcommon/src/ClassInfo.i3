
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* A little data about CFG tokens that denote classes (i.e., sets) of 
   other tokens.  Used in generating concrete syntax. *) 

INTERFACE ClassInfo 

; IMPORT LbeStd 

; CONST Brand = "ClassInfo"

; TYPE ClassInfoTyp = RECORD 
    ClassTok : LbeStd . TokTyp := LbeStd . Tok__Null 
  ; UseCount : CARDINAL := 0  
  END 

; TYPE ClassInfoRefTyp = REF ClassInfoTyp 

; TYPE T = ClassInfoRefTyp 

; END ClassInfo 
. 
