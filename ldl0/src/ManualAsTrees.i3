
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE ManualAsTrees 
(* Package ManualAsTrees. 
   Hardcoded Est builder for the initial version of Ldl0.
*)  

; IMPORT Assertions 
; IMPORT LbeStd 

; PROCEDURE LanguageDefinition 
    ( Lang : LbeStd . LangTyp := LbeStd . LangNull ) : LbeStd . EstRootTyp 
  RAISES { Assertions . AssertionFailure } 

; END ManualAsTrees 
. 

