
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Ldl1FsTrees 
(* Package Ldl1FsTrees. 
   Build FsTrees for a language definition written in Ldl1.
*) 

; IMPORT AstView 
; FROM Failures IMPORT Backout 
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT LdlSemantics 

; PROCEDURE BuildFixedTerm 
    ( <* UNUSED *> VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; Tok : LbeStd . TokTyp 
    ) 
  RAISES { Backout } 

; PROCEDURE BuildVarTerm 
    ( <* UNUSED *> VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; LdlNode : AstView . AstRefTyp  
    ; VarTermTok : LbeStd . TokTyp 
    ; VarTermModTok : LbeStd . TokTyp 
    ) 
  RAISES { Backout } 

; PROCEDURE Build 
    ( VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; FsIdentSemRef : LdlSemantics . SemFsRefTyp 
    ; AsIdentSemRef : LdlSemantics . SemDeclAsNodeTyp 
    ; IsStart : BOOLEAN 
    ) 
  : LangUtil . FsNodeRefTyp 
  RAISES { Backout } 
  (* Build an FS tree for a single AS node. *) 

; END Ldl1FsTrees 
. 
