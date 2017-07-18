
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Reduce.md, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Check if the grammar is reduced *) 

INTERFACE Reduce 

; IMPORT LRTable 

; PROCEDURE IsReduced ( Gram : LRTable . GrammarTyp ) : BOOLEAN 
  (* prueft ob die im Modul Automaton bekannte Grammatik 
     reduziert ist. Falls nein wird das Programm mittels einer 
     Fehlermeldung abgebrochen 
  *)
  (* Only errors cause FALSE to be returned, not information or warnings. *)  

; END Reduce 
. 


