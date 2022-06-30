
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Lookahead.md, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Compute LALR(1) lookahead sets *) 

INTERFACE LALRLookahead 

; FROM Failures IMPORT Backout 
; IMPORT LRTable 

; TYPE ConflictProc = PROCEDURE ( p0 : BOOLEAN ) 

; PROCEDURE ComputeLALR ( Gram : LRTable . GrammarTyp ) 
  RAISES { Backout } 
  (* Berechnung der LALR(1) LookAheadSets *) 

; END LALRLookahead 
. 

