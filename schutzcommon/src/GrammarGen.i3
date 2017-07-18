
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE GrammarGen 

(* Generate concrete syntax rules from format syntax (and also abstract 
   syntax).
*) 

; IMPORT Assertions 
; IMPORT LdlSemantics 

; PROCEDURE Generate 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; Factored : BOOLEAN := TRUE 
    ) 
  RAISES { Assertions . AssertionFailure } 

; END GrammarGen
. 

