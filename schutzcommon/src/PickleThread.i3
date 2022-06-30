
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE PickleThread 

(* Wrapper around Pickle.Write, using a separate thread with a big
   enough stack.
*) 

; IMPORT Thread 
; IMPORT Wr 

; IMPORT Assertions 

; FROM Failures IMPORT Backout 

; EXCEPTION Error ( TEXT ) 

; PROCEDURE Write 
    ( WrT : Wr . T 
    ; Ref : REFANY 
    ) 
  RAISES { Error , Backout , Thread . Alerted } 
  (* Do Pickle . Write with the same parameters, in a distinct thread
     whose stack will be set large for pickling.  Serialize requests.
     Forward Alerts to the pickling thread, "backward" the listed exceptions
     from the pickling thread to the caller.
  *) 

; END PickleThread
.


