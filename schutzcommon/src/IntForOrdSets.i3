
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Used to instantiate OrdSets, to give sets of integers. *) 

INTERFACE IntForOrdSets 

; TYPE T = INTEGER 

; CONST NullElem = FIRST ( T ) 

; TYPE ValidElemT = [ NullElem + 1 .. LAST ( T ) ]  

; CONST Brand = "Integer"

; END IntForOrdSets 
. 
