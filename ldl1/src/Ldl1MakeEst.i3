
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Ldl1MakeEst 

; IMPORT LbeStd 
; FROM Assertions IMPORT AssertionFailure 

; PROCEDURE Root 
    ( Lang : LbeStd . LangTyp := LbeStd . LangNull ) : LbeStd . EstRootTyp  
  RAISES { AssertionFailure }

; END Ldl1MakeEst 
. 
