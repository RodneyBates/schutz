
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Ldl0MakeEst 

; FROM Failures IMPORT Backout 
; IMPORT LbeStd 

; PROCEDURE Root 
    ( Lang : LbeStd . LangTyp := LbeStd . LangNull ) : LbeStd . EstRootTyp  
  RAISES { Backout }

; END Ldl0MakeEst 
. 
