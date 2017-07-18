
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE TokRelation 

(* A relation on INTEGER X INTEGER.  Used for tokens. *) 

; IMPORT IntSets 
; IMPORT LbeStd 

; TYPE ElementTyp = IntSets . ValidElemT  
; TYPE T <: REFANY 

; PROCEDURE New ( LeftMin : ElementTyp := 0 ; LeftMax : ElementTyp ) : T 

; PROCEDURE AddRule 
    ( VAR Relation : T ; Left : ElementTyp ; Right : ElementTyp ) 

; PROCEDURE SetRightRelatives 
    ( Relation : T ; Left : ElementTyp ; Relatives : IntSets . T ) 

; PROCEDURE Close ( VAR Relation : T ) 

; PROCEDURE IsElement ( Relation : T ; Left , Right : ElementTyp ) : BOOLEAN 

; PROCEDURE RightRelatives ( Relation : T ; Left : ElementTyp ) : IntSets . T 

; PROCEDURE GroundRightRelatives ( Relation : T ; Left : ElementTyp ) 
  : IntSets . T 

; TYPE ProcOfTokTyp = PROCEDURE ( Tok : LbeStd . TokTyp ) RAISES ANY 

; PROCEDURE ForAllGroundMembers 
    ( Relation : T 
    ; ClassTok : LbeStd . TokTyp 
    ; VisitMember : ProcOfTokTyp 
    ) 
  RAISES ANY 

; END TokRelation 
. 
