
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Schedule a method override to be called back as soon as possible after 
   a specified time. *)

INTERFACE Delay 

; TYPE Public = OBJECT 
    METHODS 
      apply ( ) RAISES ANY 
      (* An override of apply is what will be called back at the requested time. 
         Any exceptions it raises will be caught and ignored. 
      *) 
    END (* Public *) 

; TYPE T <: Public 

; PROCEDURE ScheduleRelative ( RelTime : LONGREAL ; Event : T ) 
  (* Arrange for Event . apply to be called back (on a thread provided herein),
     as soon as possible after RelTime seconds.  
     If Event is currently scheduled but has not begun, 
     cancel its old scheduled time, before rescheduling.  
     If Event is currently executing, allow it to continue, while scheduling
     another callback to it. 
     Callbacks on all scheduled events will be serialized,, including a
     rescheduled currently executing one.  
  *) 

; PROCEDURE ScheduleAbsolute ( AbsTime : LONGREAL ; Event : T ) 
  (* Like ScheduleRelative, but arrange to call back at absolute time
     AbsTime.
  *) 

; PROCEDURE Cancel ( Event : T ; VAR WasScheduled : BOOLEAN ) 
  (* If Event is currently scheduled but has not begun, cancel it and set
     WasCancelled := TRUE.  Otherwise, just set WasScheduled = FALSE. 
  *) 

; PROCEDURE Alert ( Event : T ; VAR WasExecuting : BOOLEAN ) 
  (* If Event is executing, Alert it and set WasExecuting := TRUE.  
     Otherwise, just set WasExecuting := FALSE. 
  *) 

; PROCEDURE CancelOrAlert 
    ( Event : T ; VAR WasScheduled : BOOLEAN ; VAR WasExecuting : BOOLEAN ) 
  (* If Event is currently scheduled but has not begun, cancel it and set
     WasCancelled := TRUE.  Otherwise, set WasScheduled := FALSE.
     If Event is executing, Alert it and set WasExecuting := TRUE.
     Otherwise, set WasScheduled := FALSE.
     (These cases are mutually exclusive.)   
  *) 

; PROCEDURE IsScheduled ( Event : T ) : BOOLEAN 
  (* Event is scheduled but has not begun execution.  This result could be 
     outdated by the time you get it. 
  *) 

; PROCEDURE IsExecuting ( Event : T ) : BOOLEAN 
  (* Event is currently executing.  This result could be outdated by the time 
     you get it. 
  *) 

; END Delay 
. 
