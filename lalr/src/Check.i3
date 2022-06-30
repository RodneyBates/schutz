
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Check.md, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

INTERFACE Check 

; FROM Failures IMPORT Backout 
; IMPORT LRTable 

; VAR Verbose : BOOLEAN := TRUE 
; VAR VerboseDebug : BOOLEAN := FALSE  
; VAR InformNormalSublist : BOOLEAN := FALSE 

; PROCEDURE CheckForConflicts 
    ( Gram : LRTable . GrammarTyp ; VAR IsOK : BOOLEAN ) 
  RAISES { Backout } 
  (* Pruefe ob die Zustaende Konflikte beinhalten, 
     so weit moeglich werden Konflikte mit Hilfe von 
     Prioritaeten und Assoziativitaeten geloest, 
     falls keine Korektur moeglich ist wird ok auf FALSE gesetzt 
     einer Fehlermeldung beendet, sonst steht ein konfliktfreier 
     Automat zur Auswertung zur Verfuegung *) 

; END Check 
. 

