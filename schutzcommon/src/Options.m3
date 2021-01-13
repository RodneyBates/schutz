
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)



MODULE Options 

(* Global options. *) 

(* VISIBLE: *) 
; PROCEDURE SetDerivedDebugOptions ( ) 

  = BEGIN 
      AssertionChecking := DebugLevel >= 1 
    ; ExpensiveChecking := DebugLevel >= 2
    ; LogMessages := DebugLevel >= 3  
    ; TreeBrowsing := DebugLevel >= 4 
    ; TraceParse := DebugLevel >= 5 
    ; AllowProceedAfterAssert := DebugLevel >= FIRST ( DebugLevelTyp )  
    ; AllowTerminateAfterAssert := DebugLevel >= FIRST ( DebugLevelTyp )  
    END SetDerivedDebugOptions 

; BEGIN 
  END Options 
. 

