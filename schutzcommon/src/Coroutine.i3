
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Coroutine 

(* Implement simple paired coroutines.  (Use oversynchronized threads to 
   do it. 
*) 

; IMPORT Assertions 

; CONST Brand = "Coroutine.T.Brand" 

; TYPE T <: ROOT 
  (* A Coroutine . T controls the relationship 
     between two threads, synchronized as coroutines. *) 

; TYPE ProcOfT = PROCEDURE ( Cr : T ) 

; PROCEDURE Init ( Cr : T ; ChildProc : ProcOfT ) : T 
  (* Create A pair with the calling thread as one coroutine 
     (called the "creator", and a new thread executing ChildProc 
     as the other (called the "child"). 
     The creator will continue to execute and the child will not. *) 

; PROCEDURE Resume 
    ( Cr : T ) 
  (* Switch execution to the other coroutine of T. 
     A Noop if the caller is the creator of Cr and 
     ChildProc has returned. *) 
    RAISES { Assertions . AssertionFailure } 

; PROCEDURE HasReturned ( Cr : T ) : BOOLEAN 
  (* TRUE if ChildProc of the child coroutine has returned. *) 

; END Coroutine 
. 

