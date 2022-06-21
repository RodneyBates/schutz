
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
; IMPORT RTException 

; TYPE FailureActionTyp = { FaCrash , FaBackout , FaIgnore } 

; TYPE QueryProcTyp = PROCEDURE ( RT0 . ActivationPtr ) : FailureActionTyp

; PROCEDURE RegisterFailureActions 
    ( BackstopProc : RTException . Backstop ; QueryProc : QueryProcTyp )
  (* Register these procedures for Thread.Self. *) 

; TYPE BkstpInfoRefTyp <: REFANY 

; <*IMPLICIT*>
  EXCEPTION Backout 
  (* Backout and recover from an unhandled exception or runtime failure. *) 

; <*IMPLICIT*>
  EXCEPTION Ignore
  (* Ignore an unhandled exception or runtime failure. *) 

; END Failures
.

