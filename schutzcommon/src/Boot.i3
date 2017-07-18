
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Boot 

(* Print some data generated during Ldl bootstrapping. *) 

; IMPORT Wr 

; IMPORT EstUtil 

; PROCEDURE PrintSizes ( WrT : Wr . T ) 

; PROCEDURE PrintStats 
    ( WrT : Wr . T ; READONLY Stats : EstUtil . StatisticsTyp ) 

; END Boot 
. 

