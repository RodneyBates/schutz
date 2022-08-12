
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This module handles coordination between the worker thread and
   other parts of the editor.  
*) 

INTERFACE Worker 

; IMPORT FormsVBT 
; IMPORT Thread 
; IMPORT VBT 

; IMPORT Assertions  
; IMPORT EditWindow
; IMPORT Failures 
; IMPORT MessageCodes
; IMPORT PaintHs 

<* PRAGMA LL *>

(* ====================== Scheduling of worker thread. ===================== *)

(* Scheduling of worker thread jobs will contain at most, one
   immediate job and one queued job.  This is ensured by Request*
   procedures' either refusing a request, or keeping the requesting
   thread waiting in the request procedure.
   
   If there is an immediate job, it is already scheduled, meaning
   either it is currently being worked on, or will be next, when the
   worker thread receives CPU cycles.
   
   The one queued job, if any, could suffer starvation behind arriving
   immediate jobs.  A queued job is scheduled when no job is in front
   of it, whence it is further treated the same as an immediate job.
*) 

; TYPE GranularityTyp  
    = { Global (* Must exclude all other work. *) 
      , Image  (* Must exclude work on the same image. *) 
      , Window (* Must exclude work on the same window. *) 
      } 

; TYPE ClosureTyp  
    = OBJECT 
        Form : FormsVBT . T 
      ; Window : EditWindow . T (* PaintHs . WindowRefTyp *)  
        (* For VBT-level callbacks, the Form is not known. *) 
      ; Time : VBT . TimeStamp := FIRST ( INTEGER ) 
      ; ImageTrans : PaintHs . ImageTransientTyp := NIL 
        (* ^Could be different from the image open in Form. *) 
      ; ImagePers : PaintHs . ImagePersistentTyp := NIL 
        (* ^Could be different from the image open in Form. *) 
      ; IsInteractive : BOOLEAN := TRUE 
        (* Will be set by RequestWork*.  apply overrides can test this. *) 
      ; Granularity : GranularityTyp (* For now, treat everything as Global. *)
      METHODS
        apply ( ) RAISES { Failures . Backout , Thread . Alerted } 
      END 

; TYPE WorkResultTyp 
   = { WrtBusyImmed  (* Worker thread is working on an immediate task. *) 
     , WrtBusyQueued (* Worker thread is working on a queued task. *) 
     , WrtDone       (* Worker thread has completed the requested task. *) 
     , WrtStopped    (* Worker thread stopped as requested. *) 
     , WrtFailed     (* Worker thread had an assert failure, RT error, etc. *) 
     , WrtRefused    (* Worker thread refused the task because it was already
                        busy with another task. *) 
     } 

; TYPE WorkResultNotRefusedTyp 
   = [ WorkResultTyp . WrtBusyImmed .. WorkResultTyp . WrtFailed ]

; PROCEDURE RequestWork (* Immediate. *) 
    ( Closure : ClosureTyp 
    ; Interactive : BOOLEAN := FALSE  
      (* ^Causes assertion failures or runtime errors raised in the 
          worker thread to query the user about what to do. *)
      (* Default is appropriate for playback work. *) 
    ; Granularity : GranularityTyp := GranularityTyp . Global 
    ; WaitToStart : BOOLEAN := FALSE 
      (* Instead of refusing, wait to start the work, before returning . *) 
    ; WaitToFinish : BOOLEAN := TRUE  
      (* ^Don't return until work is refused or done. *) 
      (* Default is appropriate for playback work. *) 
    ) 
  : WorkResultTyp 
  RAISES { Thread . Alerted (* Only if NOT Interactive. *) } 
  <* LL.sup <= VBT.mu *> 
  (* GUI threads call this to ask for work to be done. *) 

; PROCEDURE RequestWorkInteractive (* Immediate. *) 
    ( Closure : ClosureTyp 
    ; Granularity : GranularityTyp := GranularityTyp . Global 
    ) 
  : WorkResultTyp 
  <* LL.sup <= VBT.mu *> 
  (* Same as RequestWork 
       ( .. 
       , Interactive := TRUE , WaitToFinish := FALSE, WaitToStart:= FALSE 
       ) 
     but also statically known not to raise Thread . Alerted. 
  *) 

; PROCEDURE IsIdle ( ) : BOOLEAN 

; PROCEDURE AwaitIdle ( ) 
  : WorkResultTyp 
    (* ^Of doubtful use, since who knows what work just finished. *) 
  RAISES { Thread . Alerted } 
  <* LL.sup <= VBT.mu *> 
  (* GUI threads can call this to wait for the worker thread to be idle. *) 

; PROCEDURE CancelImmedWork 
    ( WaitToFinish : BOOLEAN := FALSE 
      (* ^Don't return until worker thread has stopped. *) 
    ) 
  : WorkResultNotRefusedTyp  
  RAISES { Thread . Alerted } 
  <* LL.sup <= VBT.mu *> 
  (* GUI threads call this to stop any active work. *)   

; PROCEDURE RequestQueuedWork 
    ( Closure : ClosureTyp 
    ; Granularity : GranularityTyp := GranularityTyp . Global 
    ) 
  : ClosureTyp (* A previously queued closure that was cancelled, or NIL. *)
  RAISES { Thread . Alerted } 
  <* LL.sup <= VBT.mu *>
  (* If there is a previously queued and not yet scheduled job, cancel it
     and return its closure.
  *)

; PROCEDURE CancelQueuedWork 
    ( WaitToFinish : BOOLEAN := FALSE 
      (* ^Don't return until worker thread has stopped. *) 
    ) 
  : ClosureTyp (* The now-cancelled, queued closure, or NIL. *) 
  RAISES { Thread . Alerted } 
  <* LL.sup <= VBT.mu *> 

; PROCEDURE ExistingQueuedWork ( ) 
  : ClosureTyp (* The queued closure, or NIL. *) 
  <* LL.sup <= VBT.mu *> 

(*
; PROCEDURE Failure 
    ( String1 : TEXT 
    ; String2 : TEXT 
    ; CrashCode : MessageCodes . T 
    ; DoWriteCheckpoint : BOOLEAN 
      (* ^Requests Failure to write a checkpoint. *) 
    ) 
  : BOOLEAN (* Raise Backout. *) 
  <* LL.sup < VBT.mu *>
  (* Worker thread calls this from inside an assertion failure.
     Failure does not return until it is known what to do, which could
     involve querying the user, if appropriate. 
  *)
*)

; PROCEDURE AnswerGuiAssertDialog  
    ( FailureAction : Failures . FailureActionTyp ) 
  <* LL.sup <= VBT.mu *> 
  (* Gui threads call this to report user response to an assertion dialog. *) 

; PROCEDURE IAmWorkerThread ( ) : BOOLEAN 

; PROCEDURE DoGuiActions ( ) : BOOLEAN 

; PROCEDURE Init ( ) 
  (* Things that must be done later than module initialization. *) 

; END Worker 
. 

