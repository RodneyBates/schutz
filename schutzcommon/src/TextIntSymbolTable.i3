
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE TextIntSymbolTable

(* A dictionary, mapping from TEXT to Est node numbers. *) 

; IMPORT LbeStd 

; TYPE T <: REFANY 

; PROCEDURE New ( ) : T 

; PROCEDURE Find 
    ( Map : T 
    ; String : TEXT 
    ; VAR WasFound : BOOLEAN  
    ; VAR NodeNo : LbeStd . EstNodeNoTyp 
    ) 

; PROCEDURE FindOrAdd  
    ( Map : T 
    ; String : TEXT 
    ; NewNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR WasFound : BOOLEAN  
    ; VAR FoundNodeNo : LbeStd . EstNodeNoTyp 
    ) 
  (* Set WasFound to whether String was already in the map. 
     If String is not in the map, add it, mapping to NewNodeNo.
     Set FoundNodeNo to what String now maps to.  
  *) 

; END TextIntSymbolTable
.

