
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Failures

(* Support for handling runtime errors and uncaught exceptions (which are
   turned into runtime errors). *)

(* Lots of code in the exporting module is invoked, directly or indirectly,
   via its registering of a backstop callback with the runtime system. *)

; IMPORT RT0

; TYPE FailureActionTyp = { FaCrash , FaBackout , FaIgnore }
; TYPE FailureActionSetTyp = SET OF FailureActionTyp 

; TYPE QueryProcTyp
  = PROCEDURE
      ( READONLY Act : RT0 . RaiseActivation
      ; String1 , String2 : TEXT 
      ; AllowedActions : FailureActionSetTyp
      )
    : FailureActionTyp

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


; PROCEDURE RegisterQueryProc ( QueryProc : QueryProcTyp )
  (* Register a query procedure for Thread.Self. *) 

; TYPE ThreadInfoRefTyp <: REFANY 

; PROCEDURE ExcName ( READONLY Act : RT0 . RaiseActivation ) : TEXT

; PROCEDURE ActivationImage ( READONLY Act : RT0 . RaiseActivation ) : TEXT 

; <*IMPLICIT*>
  EXCEPTION Backout 
  (* Backout and recover from an unhandled exception or runtime failure. *) 

; <*IMPLICIT*>
  EXCEPTION Ignore
  (* Ignore an unhandled exception or runtime failure. *) 

; END Failures
.

