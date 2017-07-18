
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE TextIntSymbolTable

(* A dictionary, mapping from TEXT to Est node numbers. *) 

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
    ; VAR NodeNo : LbeStd . EstNodeNoTyp 
    ) 

  = BEGIN
      WasFound := Map . get ( String , NodeNo ) 
    END Find 

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

  = BEGIN
      WasFound := Map . get ( String , FoundNodeNo ) 
    ; IF NOT WasFound 
      THEN 
        EVAL Map . put ( String , NewNodeNo ) 
      END (* IF *) 
    END FindOrAdd 

; BEGIN 
  END TextIntSymbolTable 
. 
