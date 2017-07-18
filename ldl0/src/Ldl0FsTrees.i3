
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Ldl0FsTrees 
(* Package Ldl0FsTrees. 
   Build FsTrees for a language definition written in Ldl0.
*) 

; IMPORT Assertions 
; IMPORT AstView 
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT LdlSemantics 

; PROCEDURE BuildFixedTerm 
    ( <* UNUSED *> VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; Tok : LbeStd . TokTyp 
    ) 
  RAISES { Assertions . AssertionFailure } 

; PROCEDURE BuildVarTerm 
    ( <* UNUSED *> VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; LdlNode : AstView . AstRefTyp  
    ; VarTermTok : LbeStd . TokTyp 
    ; VarTermModTok : LbeStd . TokTyp 
    ) 
  RAISES { Assertions . AssertionFailure } 

; PROCEDURE Build 
    ( VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; FsIdentSemRef : LdlSemantics . SemFsRefTyp 
    ; AsIdentSemRef : LdlSemantics . SemDeclAsNodeTyp 
    ; IsStart : BOOLEAN 
    ) 
  : LangUtil . FsNodeRefTyp 
  RAISES { Assertions . AssertionFailure } 

; END Ldl0FsTrees 
. 
