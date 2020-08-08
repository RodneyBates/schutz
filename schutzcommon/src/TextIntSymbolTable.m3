
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE TextIntSymbolTable

(* A find-or-add style dictionary, mapping from TEXT to INTEGERs. *) 

; IMPORT LbeStd 
; IMPORT TextIntTbl 

; REVEAL T = TextIntTbl . Default BRANDED "TextIntTable.T" OBJECT END 

; CONST InitHashSize = 201 

; PROCEDURE New ( ) : T 

  = BEGIN
      RETURN 
        NEW ( T ) . init ( sizeHint := InitHashSize )
    END New 

; PROCEDURE Find 
    ( Map : T 
    ; String : TEXT 
    ; VAR WasFound : BOOLEAN  
    ; VAR ResultVal : INTEGER 
    ) 

  = BEGIN
      WasFound := Map . get ( String , ResultVal ) 
    END Find 

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

  = BEGIN
      WasFound := Map . get ( String , FoundVal ) 
    ; IF NOT WasFound 
      THEN 
        EVAL Map . put ( String , NewVal ) 
      END (* IF *) 
    END FindOrAdd 

; BEGIN 
  END TextIntSymbolTable 
. 
