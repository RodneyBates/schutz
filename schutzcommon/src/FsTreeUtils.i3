
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE FsTreeUtils 

(* Common code that works on FsTrees.  Common to both Ldl0 and Ldl1. *)  

; FROM Failures IMPORT Backout 
; IMPORT LangUtil 
; IMPORT LdlSemantics 

; PROCEDURE FinishFsRule 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; FsRuleNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  RAISES { Backout } 

; PROCEDURE CloseFormatsEmpty ( LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { Backout } 
  (* Compute the closure of the FormatsEmpty property. *) 

; END FsTreeUtils 
.

