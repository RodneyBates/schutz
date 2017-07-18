
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE TokString

(* Open arrays of tokens.  Used in CFG productions and to instantiate Table. *) 

; IMPORT Word 

; IMPORT LRTable 

; CONST Brand = "TokString"

; TYPE T = LRTable . TokArrayRefTyp 
(*TODO:    ^ Move the declaration of this in here, and adjust all references 
             it to lead here. *) 
; CONST TokStringSsNull = FIRST ( INTEGER ) 

; PROCEDURE Equal ( Left , Right : T ) : BOOLEAN 

; PROCEDURE Hash ( Value : T ) : Word . T 

; END TokString
. 

