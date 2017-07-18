
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE RefSet 

(* Sets of reference values.  Implemented as linear search, with obvious
   performance implications.  But it's immune to damamge caused by the
   GC's moving heap objects.
*) 

; TYPE T <: REFANY  

; PROCEDURE Empty ( ) : T 
  (* Return a new, empty, set of references *) 

; PROCEDURE Add ( Value : REFANY ; VAR Set : T ) 
  (* Add Value to Set. *) 

; PROCEDURE IsElement ( Value : REFANY ; Set : T ) : BOOLEAN  

; END RefSet 
. 
