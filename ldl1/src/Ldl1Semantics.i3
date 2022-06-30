
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Ldl1Semantics 

; FROM Failures IMPORT Backout 
; IMPORT LbeStd 
; IMPORT LdlSemantics 

; PROCEDURE CheckContainment 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; SuperNodeNo : LbeStd . EstNodeNoTyp 
    ; SubNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR (* IN OUT *) HasError : BOOLEAN 
    ) 

; PROCEDURE FirstOcc 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; SemRef : LdlSemantics . SemTyp 
    ) 
  : LdlSemantics . SemDeclTyp 
    RAISES { Backout } 
  (* Follow a SemAddlDef to its first occurence.  Otherwise, identity. *) 

; PROCEDURE Analyze 
    ( Est : LbeStd . EstRootTyp 
    ; LdlLang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  : LdlSemantics . LangInfoRefTyp 
  RAISES { Backout } 
  (* Does not do LALR Generation. *) 

; VAR Bootstrapping : BOOLEAN := FALSE 

; END Ldl1Semantics 

. 
