
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Gen.md, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Generate parsing tables *) 

INTERFACE Gen 

; FROM Failures IMPORT Backout 
; IMPORT LALRTypes  
; IMPORT LRTable 

; VAR Trace : BOOLEAN := FALSE 

; PROCEDURE InitTableLine 
    ( Gram : LRTable . GrammarTyp ; VAR Line : LRTable . StateNoArrayRefTyp ) 

; PROCEDURE MakeTableLine 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : LALRTypes . StateSsTyp 
    ; VAR Result : LRTable . StateNoArrayRefTyp 
    ) 
    : LRTable . StateNoTyp 
  (* Construct a literal (uncompressed) row of the parsing table, from
     the items in the state.  
     Formal StateSs is the state subscript.  The function result is the 
     state code. *) 

; PROCEDURE GenTables ( Gram : LRTable . GrammarTyp ) 
  RAISES { Backout } 

; END Gen 
. 

