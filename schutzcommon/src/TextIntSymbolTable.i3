
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE TextIntSymbolTable

(* A find-or-add style dictionary, mapping from TEXT to INTEGERs. *) 

; IMPORT LbeStd 

; TYPE T <: REFANY 

; PROCEDURE New ( ) : T 

; PROCEDURE Find 
    ( Map : T 
    ; String : TEXT 
    ; VAR WasFound : BOOLEAN  
    ; VAR ResultVal : INTEGER 
    ) 

; PROCEDURE FindOrAdd  
    ( Map : T 
    ; String : TEXT 
    ; NewVal : INTEGER 
    ; VAR WasFound : BOOLEAN  
    ; VAR FoundVal : INTEGER 
    )
  (* Set WasFound to whether String was already in the map. 
     If String is not in the map, add it, mapping to NewVal.
     Set FoundVal to what String now maps to.  
  *) 

; END TextIntSymbolTable
.

