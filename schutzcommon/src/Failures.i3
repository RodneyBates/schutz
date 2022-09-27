
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Failures

(* Support for handling runtime errors and uncaught exceptions (which are
   turned into runtime errors). *)

(* Lots of code in exporting module Failures is invoked, directly or indirectly,
   via its registering of a backstop callback with the runtime system. *)

; IMPORT RT0
; IMPORT Thread 

; <*IMPLICIT*>
  EXCEPTION Backout ( TEXT )  
  (* Backout and recover from an unhandled exception or runtime failure.
     If this goes uncaught, a crash will result.  It's implicit, so no
     danger of its being blocked.
     
     Don't raise this, only catch it.  It will be raised inside
     module Failures. 
  *) 

; <*IMPLICIT*>
  EXCEPTION Ignore
  (* Ignore an unhandled exception or runtime failure, and proceed.
  
     Don't raise this. It will be raised inside module Failures.
     Catch it at and only at a place where an exception or runtime
     error can be ignored. 
  *) 

; <*IMPLICIT*>
  EXCEPTION Terminate ( TEXT )
  (* Raise to terminate gracefully, even when there are windows up. *)
  
; TYPE FailureActionTyp = { FaCrash , FaBackout , FaIgnore }
; TYPE FailureActionSetTyp = SET OF FailureActionTyp 

; TYPE QueryProcTyp 
  = PROCEDURE
      ( READONLY Act : RT0 . RaiseActivation
      ; StoppedReason : TEXT 
      ; AllowedActions : FailureActionSetTyp
      )
    : FailureActionTyp

(* Return codes: *)
; CONST RcNormal = 0 
; CONST RcProblem = 1 (* Couldn't find files, etc. *)
; CONST RcCancelled = 2
; CONST RcFailure = 3 (* Assertion failure or runtime error. *)
(* These are misuse of the failure handling mechanism: *) 
; CONST RcBadTerminate = 4 
; CONST RcBadBackout = 5 
; CONST RcBadIgnore = 6
; CONST RcBadQuery = 7 

; PROCEDURE ExitWFailure ( ) 

; TYPE BackstopProcTyp
  = PROCEDURE
      ( VAR a : RT0 . RaiseActivation ; raises: BOOLEAN )
    RAISES ANY
  (* This must match RTException.Backstop, which can't be used
     by name here, because RTException is an UNSAFE interface. *)
  (* The RTS may make callback calls on procedures of this type when
     exception 'a' has been raised, but, (if raises,) 'a' is blocked
     by lack of a covering RAISES clause, or, (if NOT raises,) 'a'
     is not handled. *) 

; PROCEDURE RegisterQueryProc ( QueryProc : QueryProcTyp ; FDoGui := FALSE )
  (* Register a query procedure for Thread.Self. *) 
  RAISES { Thread . Alerted } 

; TYPE ThreadInfoRefTyp <: REFANY 

; PROCEDURE ExcName
    ( READONLY Act : RT0 . RaiseActivation ; Secondary := FALSE ) : TEXT
  (* Name of exception raised by Act. 
     Secondary means the uncaught or blocked RT error, after an 
     original exception, if such exists.
  *) 

; PROCEDURE ActivationLocation ( READONLY Act : RT0 . RaiseActivation ) : TEXT
    (* Code location where the raise denoted by Apt. *) 

; END Failures
.

