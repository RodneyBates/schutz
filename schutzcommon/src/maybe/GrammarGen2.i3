
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Generate concrete syntax rules from format syntax (and also abstract 
   syntax).
*) 

INTERFACE GrammarGen2 

; IMPORT Assertions 
; IMPORT LdlSemantics 

; PROCEDURE Generate ( LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { Assertions . AssertionFailure } 

; END GrammarGen2 
. 

