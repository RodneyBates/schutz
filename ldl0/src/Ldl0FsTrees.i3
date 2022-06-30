
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Ldl0FsTrees 
(* Package Ldl0FsTrees. 
   Build FsTrees for a language definition written in Ldl0.
*) 

; FROM Assertions IMPORT AssertionFailure 
; IMPORT AstView 
; FROM Failures IMPORT Backout 
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT LdlSemantics 

; PROCEDURE BuildFixedTerm 
    ( <* UNUSED *> VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; Tok : LbeStd . TokTyp 
    ) 
  RAISES { AssertionFailure } 

; PROCEDURE BuildVarTerm 
    ( <* UNUSED *> VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; LdlNode : AstView . AstRefTyp  
    ; VarTermTok : LbeStd . TokTyp 
    ; VarTermModTok : LbeStd . TokTyp 
    ) 
  RAISES { AssertionFailure } 

; PROCEDURE Build 
    ( VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; FsIdentSemRef : LdlSemantics . SemFsRefTyp 
    ; AsIdentSemRef : LdlSemantics . SemDeclAsNodeTyp 
    ; IsStart : BOOLEAN 
    ) 
  : LangUtil . FsNodeRefTyp 
  RAISES { AssertionFailure } 

; END Ldl0FsTrees 
. 
