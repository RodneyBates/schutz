
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LangMap 

(* Alter the map from (numeric) language codes of type LbeStd.LangTyp to 
   references to a record full of language-specific information.
*) 

; IMPORT LbeStd 
; IMPORT LdlSemantics 

; PROCEDURE AddOrChange 
    ( Lang : LbeStd . LangTyp ; LangInfoRef : LdlSemantics . LangInfoRefTyp ) 

; PROCEDURE Remove ( Lang : LbeStd . LangTyp ) 

; PROCEDURE LangInfo 
    ( Lang : LbeStd . LangTyp ) : LdlSemantics . LangInfoRefTyp 

; END LangMap 
. 
