
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Compress.md, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* compress parse table *) 

INTERFACE Compress 

; IMPORT LRTable 

; PROCEDURE InitCompressTable ( Gram : LRTable . GrammarTyp ) 

; PROCEDURE CompressTableLine 
    ( Gram : LRTable . GrammarTyp 
    ; State : LRTable . StateNoTyp 
    ; DefaultState : LRTable . StateNoTyp 
    ; VAR TableLine : LRTable . StateNoArrayRefTyp 
    ) 

; PROCEDURE InitCompressNTable ( Gram : LRTable . GrammarTyp ) 

; PROCEDURE CompressNTableLine 
    ( Gram : LRTable . GrammarTyp 
    ; State : LRTable . StateNoTyp 
    ; VAR TableLine : LRTable . StateNoArrayRefTyp 
    ) 

; END Compress 
. 

