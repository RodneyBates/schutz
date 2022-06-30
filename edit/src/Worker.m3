
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This module handles coordination between the worker thread and
   other parts of the editor.  
*) 

(* It looks like the highest lock level that can happen when requesting
   work is VBT.mu (or VBT.mu.w for some VBT v, which just means VBT.mu
   is locked, but with a different interpretation of how it protects
   things).
*) 

MODULE Worker 

; IMPORT FormsVBT 
; IMPORT Process
; IMPORT RT0 
; IMPORT Thread 
(* ; IMPORT VBT (* Used in LL pragmas. *) *) 

; IMPORT AssertDevel 
; IMPORT Assertions 
; FROM Failures IMPORT Backout 
; IMPORT Display 
; IMPORT Errors
; IMPORT Failures 
; IMPORT MessageCodes 
; IMPORT Options
; IMPORT UiDevel 

<* PRAGMA LL *> 
<* PRAGMA NORETURN *>

(* Just for sugar: *) 
; CONST WrtBusyImmed = WorkResultTyp . WrtBusyImmed
; CONST WrtBusyQueued = WorkResultTyp . WrtBusyQueued
; CONST WrtDone = WorkResultTyp . WrtDone
; CONST WrtStopped = WorkResultTyp . WrtStopped 
; CONST WrtFailed = WorkResultTyp . WrtFailed
; CONST WrtRefused = WorkResultTyp . WrtRefused

(* Changing appearance for busy/idle states. *) 

; <* UNUSED *> PROCEDURE MakeLookColor ( ColorName : TEXT ) 
(* Leave this in, for now. *) 

  = <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
   (* FormsVBT . MakeActive ( Options . MainForm , "Fv_Background" ) 
    ; *) 
      FormsVBT . PutTextProperty 
        ( Options . MainForm , "Fv_StopButton_Pixmap" , "Color" , ColorName ) 
    END MakeLookColor    

; PROCEDURE MakeLookIdle ( ) 

  = <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
   (* FormsVBT . MakeActive ( Options . MainForm , "Fv_Background" ) 
    ; *) 
      FormsVBT . PutTextProperty 
        ( Options . MainForm , "Fv_StopButton_Pixmap" , "Color" , "Black" ) 
    ; FormsVBT . MakePassive ( Options . MainForm , "Fv_StopButton" ) 
    END MakeLookIdle  

; PROCEDURE MakeLookBusy ( <* UNUSED *> State : WorkResultTyp ) 

  = <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
   (* FormsVBT . MakeDormant ( Options . MainForm , "Fv_Background" ) 
      This makes all descendents, even the stop button dormant, even when
      it is made active explicitly, below
    ; *) 
      FormsVBT . PutTextProperty 
        ( Options . MainForm , "Fv_StopButton_Pixmap" , "Color" , "Red" ) 
    ; FormsVBT . MakeActive ( Options . MainForm , "Fv_StopButton" ) 
    END MakeLookBusy 

(* TODO: Expand MakeLookBusy and MakeLookIdle to change cursor shape.
         Also, do all windows, when we have > 1
*) 

(* Synchronization between worker thread and GUI thread(s): *) 

; VAR Mu : MUTEX := NIL  
  (* In the locking order, Mu > v, FORALL v : VBT . T  *) 
(* Mu protects these variables: *) 
; VAR WaitingForWork : Thread . Condition := NIL   
; VAR WaitingForBusy : Thread . Condition := NIL   
; VAR WaitingForIdle : Thread . Condition := NIL  
; VAR WaitingForAssertDialog : Thread . Condition := NIL  
; VAR StoredState : WorkResultNotRefusedTyp := WrtDone 
; VAR QueryingAssert : BOOLEAN := FALSE
; VAR StoredClosure : ClosureTyp := NIL 
      (* ^When # NIL,  immediate work has been accepted, though worker
         may or may not have gotten it.  If also StoredState = WrtBusy,  
         then worker is working on it. 
      *) 
; VAR StoredFailureAction : Failures . FailureActionTyp 
; VAR QueuedClosure : ClosureTyp := NIL
      (* ^When QueuedClosure # NIL, it is either queued or being worked on.
         If also StoredClosure # NIL, then immediate work was accepted or in
         progress at the time QueuedClosure was requested and is continuing.
      *)   
      (* There will have to be one of these for every window open, when we go 
         to multiple windows.  Nevertheless,  all will be protected by the 
         single, global Mu, because operations with Image or Global 
         granularity will need to examine all the queued closures. 
      *) 
(* End of Mu-protected variables. *)

(*EXPORTED*) 
; PROCEDURE RequestWork 
    ( Closure : ClosureTyp 
    ; Interactive : BOOLEAN := FALSE  
      (* ^Causes assertion failures to query the user about what to do. *)
      (* Default is appropriate for playback work. *) 
    ; Granularity : GranularityTyp := GranularityTyp . Global 
    ; WaitToStart : BOOLEAN := FALSE 
      (* Instead of accepting a refusal, wait to start the work. *) 
    ; WaitToFinish : BOOLEAN := TRUE  
      (* ^Don't return until work is refused or done. *) 
      (* Default is appropriate for playback work. *) 
    ) 
  : WorkResultTyp 
  RAISES { Thread . Alerted (* Only if NOT Interactive. *) } 
  <* LL.sup <= VBT.mu *> 
  (* GUI threads call this to ask the worker to do something. *) 

  = VAR LResult : WorkResultTyp  

  ; BEGIN
      IF Thread . Self ( ) = WorkerThread 
      THEN (* Without vetting Trestle, I have no idea whether this can happen,
              but it seems like a safe thing to do. 
           *) 
        TRY 
          Closure . apply ( ) 
        EXCEPT 
        | Backout 
        => IF Assertions . DoTerminate 
           THEN 
             Assertions . TerminatingNormally := TRUE 
           ; Process . Exit ( 1 ) <* NORETURN *>
           ELSE RETURN WrtFailed  
           END (* IF *) 
        END (* TRY EXCEPT *) 
      ; RETURN WrtDone 
      ELSE 
        LOCK Mu 
        DO 
          IF WaitToStart  
          THEN 
            WHILE StoredClosure # NIL OR QueuedClosure # NIL  
            DO Thread . AlertWait ( Mu , WaitingForIdle ) 
            END (* WHILE *) 
          ELSIF StoredClosure # NIL 
          THEN 
           (* NOTE: Will not refuse immediate work when busy with queued. *)
            RETURN WrtRefused 
          END (* IF *) 
        ; StoredClosure := Closure 
        ; StoredClosure . IsInteractive := Interactive 
        ; Closure . Granularity := Granularity 
        ; Thread . Signal ( WaitingForWork ) 
        ; IF WaitToFinish 
          THEN
            WHILE StoredClosure # NIL 
            DO Thread . AlertWait ( Mu , WaitingForIdle ) 
            END (* WHILE *) 
          ; LResult := StoredState 
          ELSE 
            LResult := WrtBusyImmed 
            (* Even though worker may not have found it yet. *)    
          END (* IF *) 
        END (* LOCK *) 
      ; IF LResult = WrtRefused 
        THEN Display . Beep ( Errors . ErrorTyp . EWorkRefused ) 
        END (* IF *) 
      ; RETURN LResult  
      END (* IF *) 
    END RequestWork 

(*EXPORTED*) 
; PROCEDURE RequestWorkInteractive 
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

  = VAR LResult : WorkResultTyp  

  ; BEGIN
      IF Thread . Self ( ) = WorkerThread 
      THEN (* Without vetting Trestle, I have no idea whether this can happen,
              but it seems like a safe thing to do. 
           *)         
        TRY 
          Closure . apply ( ) 
        EXCEPT 
        | Thread . Alerted 
        => Display . Beep ( Errors . ErrorTyp . EControlC ) 
(* TODO: Perhaps invert the sense of when to beep, i.e., beep
         when the cancel was "refused" because it was too late.
*) 
        ; RETURN WrtStopped  
        | Backout 
        => IF Assertions . DoTerminate 
           THEN 
             Assertions . TerminatingNormally := TRUE 
           ; Process . Exit ( 1 ) <* NORETURN *>
           ELSE RETURN WrtFailed  
           END (* IF *) 
        END (* TRY EXCEPT *) 
      ; RETURN WrtDone 
      ELSE 
        LOCK Mu 
        DO
          IF StoredClosure # NIL   
          THEN 
          (* NOTE: Will not refuse immediate work when there is queued. *)
            LResult := WrtRefused 
          ELSE 
            StoredClosure := Closure 
          ; StoredClosure . IsInteractive := TRUE 
          ; Closure . Granularity := Granularity 
          ; Thread . Signal ( WaitingForWork ) 
          (* Even though worker hasn't found the work yet, we can forcast 
             what will happen. 
          *)    
          ; IF QueuedClosure # NIL 
            THEN 
              LResult := WrtBusyQueued 
            ELSE 
              LResult := WrtBusyImmed 
            END (* IF *) 
          END (* CASE *) 
        END (* LOCK *) 
      ; IF LResult = WrtRefused 
        THEN Display . Beep ( Errors . ErrorTyp . EWorkRefused ) 
        END (* IF *) 
      ; RETURN LResult  
      END (* IF *) 
    END RequestWorkInteractive  

(*EXPORTED*) 
; PROCEDURE CancelImmedWork 
    ( WaitToFinish : BOOLEAN := FALSE 
      (* ^Don't return until worker thread has stopped. *) 
    ) 
  : WorkResultNotRefusedTyp 
  RAISES { Thread . Alerted } 
  <* LL.sup <= VBT.mu *> 
  (* GUI threads call this to stop any active work. *)   

  = BEGIN 
      LOCK Mu 
      DO 
        StoredClosure := NIL 
      ; IF StoredState = WrtBusyImmed 
        THEN 
          Thread . Alert ( WorkerThread )  
        END (* IF *) 
      ; IF WaitToFinish 
        THEN
          WHILE StoredState = WrtBusyImmed 
          DO Thread . AlertWait ( Mu , WaitingForIdle ) 
          END (* WHILE *) 
        END (* IF*) 
      ; RETURN StoredState 
      END (* LOCK *)  
    END CancelImmedWork 

(*EXPORTED*) 
; PROCEDURE RequestQueuedWork 
    ( Closure : ClosureTyp 
    ; Granularity : GranularityTyp := GranularityTyp . Global 
    ) 
  : ClosureTyp (* The previously queued closure that was cancelled, or NIL. *) 
  <* LL.sup <= VBT.mu *> 

  = VAR LResult : ClosureTyp 

  ; BEGIN
      Closure . Granularity := Granularity 
    ; LOCK Mu 
      DO 
        LResult := QueuedClosure (* The one cancelled, if any. *)  
      ; QueuedClosure := Closure 
      ; IF StoredState = WrtBusyQueued 
        THEN (* Busy doing queued work, cancel it. *) 
          Thread . Alert ( WorkerThread ) 
        END (* IF *) 
      ; Thread . Signal ( WaitingForWork ) 
        (* Worker thread could be busy doing immediate work, in which case,
           let it complete.  It will do this queued work next. 
        *) 
      END (* LOCK *) 
    ; RETURN LResult  
    END RequestQueuedWork

(*EXPORTED*) 
; PROCEDURE CancelQueuedWork 
    ( WaitToFinish : BOOLEAN := FALSE 
      (* ^Don't return until worker thread has stopped. *) 
    ) 
  : ClosureTyp (* The now-cancelled, queued closure, or NIL. *) 
  RAISES { Thread . Alerted } 
  <* LL.sup <= VBT.mu *> 

  = VAR LResult : ClosureTyp 

  ; BEGIN
      LOCK Mu 
      DO 
        LResult := QueuedClosure (* The one cancelled, if any. *) 
      ; QueuedClosure := NIL 
      ; IF StoredState = WrtBusyQueued 
        THEN (* Busy doing queued work, cancel it. *) 
          Thread . Alert ( WorkerThread ) 
        END (* IF *) 
      ; IF WaitToFinish 
        THEN
          WHILE StoredState = WrtBusyQueued  
          DO Thread . AlertWait ( Mu , WaitingForIdle ) 
          END (* WHILE *) 
        END (* IF*) 
      END (* LOCK *) 
    ; RETURN LResult  
    END CancelQueuedWork

(*EXPORTED*) 
; PROCEDURE ExistingQueuedWork ( ) 
  : ClosureTyp (* The queued closure, or NIL. *) 
  <* LL.sup <= VBT.mu *> 

  = VAR LResult : ClosureTyp 

  ; BEGIN
      LOCK Mu 
      DO 
        LResult := QueuedClosure 
      END (* LOCK *) 
    ; RETURN LResult  
    END ExistingQueuedWork

(*EXPORTED*) 
; PROCEDURE IsIdle ( ) : BOOLEAN 

  = VAR LResult : BOOLEAN 

  ; BEGIN 
      LOCK Mu 
      DO
        LResult := StoredClosure = NIL AND QueuedClosure = NIL 
      END (* LOCK *) 
    ; RETURN LResult  
    END IsIdle 

(*EXPORTED*) 
; PROCEDURE AwaitIdle ( ) 
  : WorkResultTyp 
    (* ^Of doubtful use, since who knows what work just finished. *) 
  RAISES { Thread . Alerted } 
  <* LL.sup <= VBT.mu *> 
  (* GUI threads can call this to wait for the worker thread to be idle. *) 

  = VAR LResult : WorkResultTyp  

  ; BEGIN
      LOCK Mu 
      DO
        WHILE StoredClosure # NIL OR QueuedClosure # NIL 
        DO Thread . AlertWait ( Mu , WaitingForIdle ) 
        END (* WHILE *) 
      ; LResult := StoredState 
      END (* LOCK *) 
    ; RETURN LResult  
    END AwaitIdle  

(* Synchronization procedures called by the worker thread. *) 

; PROCEDURE GetWork ( VAR Closure : ClosureTyp ) 
  <* LL.sup < VBT.mu *>
  (* Worker thread calls this to get a task. 
     GetWork Doesn't return until there is one. 
  *) 

  = BEGIN 
      LOCK Mu 
      DO
        WHILE StoredClosure = NIL AND QueuedClosure = NIL 
        DO Thread . Wait ( Mu , WaitingForWork ) 
        END (* WHILE *) 
      ; IF QueuedClosure # NIL 
        THEN 
          Closure := QueuedClosure 
        ; StoredState := WrtBusyQueued 
        ELSE 
          Closure := StoredClosure 
        ; StoredState := WrtBusyImmed 
        END (* IF *) 
      ; Thread . Signal ( WaitingForBusy ) 
      END (* LOCK *)  
    ; MakeLookBusy ( StoredState ) 
    (* ^It would be nicer to do this inside RequestWork, where it would
       be indivisible with the change in State, in case the worker thread
       didn't get around to it before another user event.  But that would
       create messy problems with lock levels, since GUI threads can 
       sometimes request work with VBT.mu locked, sometimes not.  I am
       presuming MakeLookBusy will eventually have to lock VBT.mu, 
       especially if it changes the cursor.  But then again, maybe not.
    *)  
    END GetWork 

; PROCEDURE BecomeIdle 
    ( NewState : [ WrtDone .. WrtFailed ] ) 
  <* LL.sup < VBT.mu *>
  (* Worker thread calls this to report that it has become idle. 
     This can be because it finished, failed, or stopped on request. 
  *) 

  = BEGIN 
      MakeLookIdle ( ) 
    ; LOCK Mu 
      DO
        IF Thread . TestAlert ( ) 
        THEN NewState := WrtStopped 
        END (* IF *) 
      ; IF NewState = WrtStopped  
        THEN (* Whoever stopped it will have set StoredClosure or 
                QueuedClosure to NIL or maybe to a newer task that
                must not be overlaid. 
             *) 
        ELSE (* We need to set the closure that was executing to NIL *) 
          CASE StoredState <* NOWARN *> 
          OF WrtBusyImmed 
          => StoredClosure := NIL 
          | WrtBusyQueued 
          => QueuedClosure := NIL 
          END (* CASE *) 
        END (* IF *) 
      ; StoredState := NewState  
      ; Thread . Broadcast ( WaitingForIdle ) 
      END (* LOCK *)  
    END BecomeIdle  

(*EXPORTED*)
(* Registered as a callback. *)
; PROCEDURE FailureQuery
    ( READONLY Act : RT0 . RaiseActivation
    ; ExceptionText , ActivationText : TEXT 
    ; AllowedActions : Failures . FailureActionSetTyp
    )
  : Failures . FailureActionTyp 

  <* LL.sup < VBT.mu *>
  (* This is a callback, invoked from within Failures, when a runtime error
     occurs on the worker thread.  The runtime error could be secondary to
     a blocked or unhandled exception, especially Assertions.Backout,
     which gets some special treatment.  It notifies the user, either on the
     command line in a dialog, and queries the user's desire what to do. 
  *)    

  = VAR LInteractive : BOOLEAN
  ; VAR LMessage : TEXT 
  ; VAR LResult : Failures . FailureActionTyp 

  ; BEGIN 
      LMessage
        := AssertDevel . CheckpointForFailure 
             ( MessageCodes . T . NullCode , DisplayGui := FALSE ) 
    ; LOCK Mu 
      DO IF StoredClosure # NIL
        THEN LInteractive := StoredClosure . IsInteractive 
        ELSIF QueuedClosure # NIL
        THEN LInteractive := QueuedClosure . IsInteractive 
        ELSE LInteractive := FALSE
        END (* IF *) 
      END (* LOCK *)
    ; IF LInteractive 
      THEN
(* TODO: put message into the dialog. *)
        UiDevel . ShowGuiAssertDialog ( ExceptionText , ActivationText ) 
        (* It would be more obviously right to show and remove this dialog  
           while holding Mu, but I can't figure out a way that satisfies a 
           consistent lock order.  In any case, unlocking Mu is OK, because
           we will only get here when StoredState = WrtBusyImmed or
           WrtBusyQueued, which means the only thing any other thread
           can do is wait on WaitingForIdle. 
        *) 
      ; LOCK Mu 
        DO
          QueryingAssert := TRUE 
        ; WHILE QueryingAssert 
          DO Thread . Wait ( Mu , WaitingForAssertDialog ) 
          END (* WHILE *)
        ; LResult := StoredFailureAction 
        END (* LOCK *)  
      ; UiDevel . RemoveGuiAssertDialog ( ) 
      ELSE
(* TODO: Query command line, depending on option. *) 
        Assertions . DoTerminate := TRUE 
      ; LResult := Failures . FailureActionTyp . FaCrash 
      END (* IF *) 
    ; RETURN LResult 
    END FailureQuery
    
(*Old version: remove someday. 
(*EXPORTED*) 
; PROCEDURE xFailure 
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

  = VAR LInteractive : BOOLEAN 
  ; VAR LResult : BOOLEAN 

  ; BEGIN 
      IF DoWriteCheckpoint 
      THEN 
        AssertDevel . CheckpointForFailure ( CrashCode ) 
      END (* IF *) 
    ; LOCK Mu 
      DO IF StoredClosure # NIL
        THEN LInteractive := StoredClosure . IsInteractive 
        ELSIF QueuedClosure # NIL
        THEN LInteractive := QueuedClosure . IsInteractive 
        ELSE LInteractive := FALSE
        END (* IF *) 
      END (* LOCK *)
    ; IF LInteractive 
      THEN  
        UiDevel . ShowGuiAssertDialog ( String1 , String2 ) 
        (* It would be more obviously right to show and remove this dialog  
           while holding Mu, but I can't figure out a way that satisfies a 
           consistent lock order.  In any case, unlocking Mu is OK, because
           we will only get here when StoredState = WrtBusyImmed or
           WrtBusyQueued, which means the only thing any other thread
           can do is wait on WaitingForIdle. 
        *) 
      ; LOCK Mu 
        DO
          QueryingAssert := TRUE 
        ; WHILE QueryingAssert 
          DO Thread . Wait ( Mu , WaitingForAssertDialog ) 
          END (* WHILE *) 
        ; CASE StoredFailureAction 
          OF Failures . FailureActionTyp . FaCrash
          => Assertions . DoTerminate := TRUE 
          ; LResult := TRUE (* Raise Backout. *) 
          | Failures . FailureActionTyp . FaBackout 
          => LResult := TRUE (* Raise Backout. *) 
          | Failures . FailureActionTyp . FaIgnore   
          => LResult := FALSE (* Do not raise Backout. *) 
          END (* CASE *) 
        END (* LOCK *)  
      ; UiDevel . RemoveGuiAssertDialog ( ) 
      ELSE 
        Assertions . DoTerminate := TRUE 
      ; LResult := TRUE (* Always raise Backout in batch. *) 
      END (* IF *) 
    ; RETURN LResult 
    END Failure
*)

(*EXPORTED*) 
; PROCEDURE ReportAssertDialog  
    ( FailureAction : Failures . FailureActionTyp ) 
  <* LL.sup <= VBT.mu *> 
  (* Gui threads call this to report to us, the user's response to
     a failure dialog. *) 

  = BEGIN 
      LOCK Mu 
      DO
        StoredFailureAction := FailureAction 
      ; QueryingAssert := FALSE  
      ; Thread . Signal ( WaitingForAssertDialog )  
      END (* LOCK *)  
    END ReportAssertDialog 

(* The worker thread itself: *) 

; PROCEDURE WorkerThreadApply 
    ( <* UNUSED *> Self : WorkerThreadClosureTyp ) : REFANY 

  = VAR LClosure : ClosureTyp 

  ; BEGIN
      Failures . RegisterQueryProc ( FailureQuery ) 
    ; LOOP 
        GetWork ( LClosure  ) 
      ; TRY 
          LClosure . apply ( ) 
        ; BecomeIdle ( WrtDone ) 
        EXCEPT 
        | Thread . Alerted 
        => Display . Beep ( Errors . ErrorTyp . EControlC ) 
(* TODO: Perhaps invert the sense of when to beep, i.e., beep
         when the cancel was "refused" because it was too late?
*) 
        ; BecomeIdle ( WrtStopped ) 
        | Backout 
        => IF Assertions . DoTerminate OR NOT StoredClosure . IsInteractive 
           THEN 
             Assertions . TerminatingNormally := TRUE 
           ; Process . Exit ( 1 ) <* NORETURN *>
           ELSE BecomeIdle ( WrtFailed ) 
           END (* IF *) 
        END (* TRY EXCEPT *) 
      END (* LOOP *) 
    END WorkerThreadApply 

; TYPE WorkerThreadClosureTyp 
    = Thread . SizedClosure OBJECT 
      OVERRIDES 
        apply := WorkerThreadApply 
      END 

; VAR WorkerThreadClosure : WorkerThreadClosureTyp 
; VAR WorkerThread : Thread . T  

(*EXPORTED*) 
; PROCEDURE IAmWorkerThread ( ) : BOOLEAN 

  = BEGIN 
      RETURN WorkerThread # NIL AND Thread . Self ( ) = WorkerThread 
    END IAmWorkerThread 

(*EXPORTED*) 
; PROCEDURE DoGuiActions ( ) : BOOLEAN  

  = BEGIN 
      LOCK Mu 
      DO 
        RETURN IAmWorkerThread ( ) AND StoredClosure . IsInteractive 
      END (* LOCK *) 
    END DoGuiActions 

(*EXPORTED*) 
; PROCEDURE Init ( ) 
  (* Things that must be done later than module initialization. *) 

  = BEGIN 
(* TOTO:  Something more rational than making all thread stacks the
          same size, big enough for pickling.
*) 
    END Init 

; VAR WantedThreadStackSize := 64000 (* Word.T's *) (* = 256 K Bytes. *)  

; BEGIN (* Worker *) 
    Mu := NEW ( MUTEX ) 
  ; WaitingForWork := NEW ( Thread . Condition ) 
  ; WaitingForBusy := NEW ( Thread . Condition ) 
  ; WaitingForIdle := NEW ( Thread . Condition ) 
  ; WaitingForAssertDialog := NEW ( Thread . Condition ) 
  ; StoredState := WorkResultTyp . WrtDone 
  ; QueryingAssert := FALSE
  ; WorkerThreadClosure := NEW ( WorkerThreadClosureTyp ) 
  ; WorkerThreadClosure . stackSize := WantedThreadStackSize 
  ; WorkerThread := Thread . Fork ( WorkerThreadClosure ) 
  END Worker 
. 

