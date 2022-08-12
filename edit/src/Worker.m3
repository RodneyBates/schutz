
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
; IMPORT Rd 
; IMPORT RT0 
; IMPORT RTIO
; IMPORT Stdio 
; IMPORT Text 
; IMPORT TextWr 
; IMPORT Thread
; IMPORT Wr 
(* ; IMPORT VBT (* Used in LL pragmas. *) *) 

; IMPORT AssertDevel 
; IMPORT Assertions 
; FROM Failures IMPORT Backout 
; IMPORT Display 
; IMPORT Errors
; IMPORT Failures
; IMPORT LbeStd 
; IMPORT MessageCodes 
; IMPORT Options
; IMPORT PaintHs 
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

; VAR WorkerMu : MUTEX := NIL  
  (* In the locking order, WorkerMu > v, FORALL v : VBT . T  *) 
(* WorkerMu protects these variables: *) 
; VAR WaitingForWork : Thread . Condition := NIL   
; VAR WaitingForBusy : Thread . Condition := NIL   
; VAR WaitingForIdle : Thread . Condition := NIL  
; VAR WaitingForGuiAssertDialog : Thread . Condition := NIL  
; VAR StoredState : WorkResultNotRefusedTyp := WrtDone 
; VAR QueryingAssert : BOOLEAN := FALSE
; VAR ActiveClosure : ClosureTyp := NIL 
      (* ^When # NIL,  immediate work has been accepted, though worker
         may or may not have gotten it.  If also StoredState = WrtBusy,  
         then worker is working on it. 
      *) 
; VAR StoredFailureAction : Failures . FailureActionTyp 
; VAR QueuedClosure : ClosureTyp := NIL
      (* ^When QueuedClosure # NIL, it is either queued or being worked on.
         If also ActiveClosure # NIL, then immediate work was accepted or in
         progress at the time QueuedClosure was requested and is continuing.
      *)   
      (* There will have to be one of these for every window open, when we go 
         to multiple windows.  Nevertheless,  all will be protected by the 
         single, global WorkerMu, because operations with Image or Global 
         granularity will need to examine all the queued closures. 
      *) 
(* End of WorkerMu-protected variables. *)

(*EXPORTED*) 
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
  (* GUI threads call this to ask the worker to do something. *) 

  = VAR LResult : WorkResultTyp  

  ; BEGIN
      IF Thread . Self ( ) = WorkerThread 
      THEN (* Without vetting Trestle, I have no idea whether this can happen,
              but it seems like a safe thing to do. 
           *) 
        TRY 
          Closure . apply ( ) 
        EXCEPT Backout => 
        END (* TRY EXCEPT *) 
      ; RETURN WrtDone 
      ELSE 
        LOCK WorkerMu 
        DO 
          IF WaitToStart  
          THEN 
            WHILE ActiveClosure # NIL OR QueuedClosure # NIL  
            DO Thread . AlertWait ( WorkerMu , WaitingForIdle ) 
            END (* WHILE *) 
          ELSIF ActiveClosure # NIL 
          THEN 
           (* NOTE: Will not refuse immediate work when busy with queued work. *)
            RETURN WrtRefused 
          END (* IF *) 
        ; ActiveClosure := Closure 
        ; ActiveClosure . IsInteractive := Interactive 
        ; Closure . Granularity := Granularity 
        ; Thread . Signal ( WaitingForWork ) 
        ; IF WaitToFinish 
          THEN
            WHILE ActiveClosure # NIL 
            DO Thread . AlertWait ( WorkerMu , WaitingForIdle ) 
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
; PROCEDURE RequestWorkInteractive (* Immediate. *)  
    ( Closure : ClosureTyp 
    ; Granularity : GranularityTyp := GranularityTyp . Global 
    ) 
  : WorkResultTyp 

  <* LL.sup <= VBT.mu *> 
  (* Same function as RequestWork 
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
        | Backout =>
        END (* TRY EXCEPT *) 
      ; RETURN WrtDone 
      ELSE 
        LOCK WorkerMu 
        DO
          IF ActiveClosure # NIL   
          THEN 
          (* NOTE: Will not refuse immediate work when there is queued. *)
            LResult := WrtRefused 
          ELSE 
            ActiveClosure := Closure 
          ; ActiveClosure . IsInteractive := TRUE 
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
      LOCK WorkerMu 
      DO 
        ActiveClosure := NIL 
      ; IF StoredState = WrtBusyImmed 
        THEN 
          Thread . Alert ( WorkerThread )  
        END (* IF *) 
      ; IF WaitToFinish 
        THEN
          WHILE StoredState = WrtBusyImmed 
          DO Thread . AlertWait ( WorkerMu , WaitingForIdle ) 
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
  (* If there is a previously queued and not yet scheduled job, cancel it
     and return its closure.
  *)

  = VAR LResult : ClosureTyp 

  ; BEGIN
      Closure . Granularity := Granularity 
    ; LOCK WorkerMu 
      DO 
        LResult := QueuedClosure (* The one to be cancelled, if any. *)  
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
      LOCK WorkerMu 
      DO 
        LResult := QueuedClosure (* The one to be cancelled, if any. *) 
      ; QueuedClosure := NIL 
      ; IF StoredState = WrtBusyQueued 
        THEN (* Busy doing queued work, cancel it. *) 
          Thread . Alert ( WorkerThread ) 
        END (* IF *) 
      ; IF WaitToFinish 
        THEN
          WHILE StoredState = WrtBusyQueued  
          DO Thread . AlertWait ( WorkerMu , WaitingForIdle ) 
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
      LOCK WorkerMu 
      DO 
        LResult := QueuedClosure 
      END (* LOCK *) 
    ; RETURN LResult  
    END ExistingQueuedWork

(*EXPORTED*) 
; PROCEDURE IsIdle ( ) : BOOLEAN 

  = VAR LResult : BOOLEAN 

  ; BEGIN 
      LOCK WorkerMu 
      DO
        LResult := ActiveClosure = NIL AND QueuedClosure = NIL 
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
      LOCK WorkerMu 
      DO
        WHILE ActiveClosure # NIL OR QueuedClosure # NIL 
        DO Thread . AlertWait ( WorkerMu , WaitingForIdle ) 
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
      LOCK WorkerMu 
      DO
        WHILE ActiveClosure = NIL AND QueuedClosure = NIL 
        DO Thread . Wait ( WorkerMu , WaitingForWork ) 
        END (* WHILE *) 
      ; IF ActiveClosure # NIL 
        THEN 
          Closure := ActiveClosure 
        ; StoredState := WrtBusyImmed 
        ELSE 
          Closure := QueuedClosure 
        ; StoredState := WrtBusyQueued 
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
    ; LOCK WorkerMu 
      DO
        IF Thread . TestAlert ( ) 
        THEN NewState := WrtStopped 
        END (* IF *) 
      ; IF NewState = WrtStopped  
        THEN (* Whoever stopped it will have set ActiveClosure or 
                QueuedClosure to NIL or maybe to a newer task that
                must not be overlaid. 
             *) 
        ELSE (* We need to set the closure that was executing to NIL *) 
          CASE StoredState <* NOWARN *> 
          OF WrtBusyImmed 
          => ActiveClosure := NIL 
          | WrtBusyQueued 
          => QueuedClosure := NIL 
          END (* CASE *) 
        END (* IF *) 
      ; StoredState := NewState  
      ; Thread . Broadcast ( WaitingForIdle ) 
      END (* LOCK *)  
    END BecomeIdle  

; PROCEDURE CheckpointForFailure ( CrashCode : MessageCodes . T )
  : TEXT (* Message about checkpoint, to be emitted a bit later. *) 

  = VAR LWindow : PaintHs . WindowRefTyp 
  ; VAR LImageRef : PaintHs . ImageTransientTyp 
  ; VAR LReason : TEXT
  ; VAR LMessage : TEXT
  ; VAR LMsgWrT : TextWr . T 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* CheckpointForFailure *)  

      LMsgWrT := TextWr . New ( ) 
    (* ; Wr . PutText ( LMsgWrT , "" ) *) 
    ; LWindow := Options . MainWindow  
   (* LWindow := FormsVBT . GetGeneric ( Options . MainForm , "Fv_LbeWindow" ) 
      Can attempt to reacquire a mutex already held. *) 
    ; IF LWindow = NIL 
      THEN
        Wr . PutText
          ( LMsgWrT , "No image identified, no checkpoint written." ) 
      ELSE 
        LImageRef := LWindow . WrImageRef 
      ; IF LImageRef = NIL 
        THEN 
          LImageRef := Options . OpeningImageRef 
        ; IF LImageRef # NIL 
          THEN 
            Wr . PutText 
              ( LMsgWrT 
              , "Using Options.OpeningImageRef for checkpoint." & Wr . EOL 
              ) 
          END (* IF *) 
        END (* IF *) 
      ; IF LImageRef # NIL 
        THEN 
          LImageRef . ItPers . IpCrashCode := ORD ( CrashCode ) 
        ; LReason := MessageCodes . Image ( CrashCode )  
        ; Wr . PutText 
              ( LMsgWrT 
              , AssertDevel . WriteCheckpointQuiet
                  ( LImageRef , DoCreateVersion := TRUE )
              ) 
        END (* IF *) 
      END (* IF *)
    ; LMessage := TextWr . ToText ( LMsgWrT ) 
    ; RETURN LMessage 
    END CheckpointForFailure
    
; CONST AnsCrash = "t" 
; CONST AnsBackout = "b" 
; CONST AnsIgnore = "i" 

; PROCEDURE FailureMessageCommandLine 
    ( READONLY Act : RT0 . RaiseActivation
    ; StoppedReason : TEXT 
    ; CheckpointMsg : TEXT 
    )
  (* Write a command line notification of a failure. *) 

  = VAR LResponse : TEXT 
  ; VAR LResult : Failures . FailureActionTyp 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN
      RTIO . PutText ( Wr . EOL )
    ; RTIO . PutText 
        ( "###############################################################" ) 
    ; RTIO . PutText ( Wr . EOL ) 
    ; RTIO . PutText ( "OH NO!" )
    ; RTIO . PutText ( Wr . EOL ) 
    ; RTIO . PutText ( "The usually marvelous " )
    ; RTIO . PutText ( LbeStd . AppName )
    ; RTIO . PutText ( " editor has suffered a humiliating" ) 
    ; RTIO . PutText ( Wr . EOL )
    ; RTIO . PutText ( StoppedReason ) 
    ; RTIO . PutText ( " " )
    ; RTIO . PutText ( Failures . ExcName ( Act ) ) 
    ; RTIO . PutText ( Wr . EOL ) 
    ; RTIO . PutText ( "at source code " ) 
    ; RTIO . PutText ( Failures . ActivationLocation ( Act ) ) 
    ; RTIO . PutText ( Wr . EOL ) 
    ; RTIO . PutText ( CheckpointMsg ) 
    ; RTIO . PutText ( Wr . EOL ) 
    ; RTIO . Flush ( )
    END FailureMessageCommandLine 

; PROCEDURE FailureDialogCommandLine 
    ( AllowedActions : Failures . FailureActionSetTyp )
  : Failures . FailureActionTyp
  (* Conduct a command line dialog about a blocked or uncaught exception. *) 

  = VAR LResponse : TEXT 
  ; VAR LResult : Failures . FailureActionTyp 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN
      AllowedActions
        := AllowedActions
           + Failures . FailureActionSetTyp
               { Failures . FailureActionTyp . FaCrash }
      (* ^ Always allow crashing. *) 
    ; LOOP 
        IF Failures . FailureActionTyp . FaCrash IN AllowedActions 
        THEN 
          RTIO . PutText ( "-- Type \"" )
        ; RTIO . PutText ( AnsCrash )
        ; RTIO . PutText ( "\" + RETURN to terminate " )
        ; RTIO . PutText ( LbeStd . AppName & "." ) 
        ; RTIO . PutText ( Wr . EOL )
        END (* IF *)

      ; IF Failures . FailureActionTyp . FaBackout IN AllowedActions 
        THEN 
          RTIO . PutText ( "-- Type \"" )
        ; RTIO . PutText ( AnsBackout )
        ; RTIO . PutText 
           ( "\" + RETURN to backout and attempt to reset to the state prior" ) 
        ; RTIO . PutText ( " to the last command." ) 
        ; RTIO . PutText ( Wr . EOL )
        ; RTIO . PutText ( "     This has a decent chance of working." ) 
        ; RTIO . PutText ( Wr . EOL )
        END (* IF *)

      ; IF Failures . FailureActionTyp . FaIgnore IN AllowedActions 
        THEN 
          RTIO . PutText ( "-- Type \"" )
        ; RTIO . PutText ( AnsIgnore )
        ; RTIO . PutText 
           ( "\" + RETURN to ignore the failure and force execution to continue." ) 
        ; RTIO . PutText ( Wr . EOL )
        ; RTIO . PutText 
            ( "     WARNING: no telling what will happen if you try this." ) 
        ; RTIO . PutText ( Wr . EOL )
        END (* IF *) 

      ; RTIO . PutText ( LbeStd . AppName & "> " ) 
      ; RTIO . Flush ( )
      ; TRY 
          LResponse := Rd . GetLine ( Stdio . stdin )   
        EXCEPT ELSE 
          LResponse := "" 
        END (* TRY EXCEPT *)

      ; IF Failures . FailureActionTyp . FaCrash IN AllowedActions 
           AND Text . Equal ( LResponse , AnsCrash ) 
        THEN 
          LResult := Failures . FailureActionTyp . FaCrash 
        ; EXIT 
        ELSIF Failures . FailureActionTyp . FaBackout IN AllowedActions 
              AND Text . Equal ( LResponse , AnsBackout )
        THEN 
          LResult := Failures . FailureActionTyp . FaBackout 
        ; EXIT 
        ELSIF Failures . FailureActionTyp . FaIgnore IN AllowedActions 
              AND Text . Equal ( LResponse , AnsIgnore ) 
        THEN 
          LResult := Failures . FailureActionTyp . FaIgnore 
        ; EXIT 
        ELSE 
          RTIO . PutText ( "Unrecognized answer: \"" ) 
        ; RTIO . PutText ( LResponse ) 
        ; RTIO . PutText ( "\"" ) 
        ; RTIO . PutText ( Wr . EOL )
        END (* IF *)  
      END (* LOOP *)
    ; RETURN LResult 
    END FailureDialogCommandLine 

(*EXPORTED*)
(* Registered as a callback. *)
; PROCEDURE FailureQuery
    ( READONLY Act : RT0 . RaiseActivation
    ; StoppedReason : TEXT 
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

  = VAR LCheckpointMsg : TEXT 
  ; VAR LResult : Failures . FailureActionTyp 
  ; VAR LInteractive : BOOLEAN
  ; VAR LDoGuiActions : BOOLEAN
  
  ; BEGIN 
      LOCK WorkerMu 
      DO IF ActiveClosure # NIL
        THEN LInteractive := ActiveClosure . IsInteractive 
        ELSE LInteractive := FALSE
        END (* IF *)
      ; LDoGuiActions := LInteractive AND IAmWorkerThread ( ) 
      END (* LOCK *)
    ; LCheckpointMsg := CheckpointForFailure ( MessageCodes . T . NullCode ) 
    ; FailureMessageCommandLine
        ( Act , StoppedReason , LCheckpointMsg ) 

    ; IF LDoGuiActions 
      THEN
        IF Failures . FailureActionTyp . FaBackout IN AllowedActions
        THEN FormsVBT . MakeActive ( Options . MainForm , "Fv_Assert_Backout" )
        ELSE FormsVBT . MakeVanish ( Options . MainForm , "Fv_Assert_Backout" )
        END (* IF *) 
      ; IF Failures . FailureActionTyp . FaCrash IN AllowedActions
        THEN FormsVBT . MakeActive ( Options . MainForm , "Fv_Assert_Terminate" )
        ELSE FormsVBT . MakeVanish ( Options . MainForm , "Fv_Assert_Terminate" )
        END (* IF *) 
      ; IF Failures . FailureActionTyp . FaIgnore IN AllowedActions
        THEN FormsVBT . MakeActive ( Options . MainForm , "Fv_Assert_Ignore" )
        ELSE FormsVBT . MakeVanish ( Options . MainForm , "Fv_Assert_Ignore" )
        END (* IF *) 
      ; UiDevel . ShowGuiAssertDialog
          ( StoppedReason & ": " & Failures . ExcName ( Act )
          , Failures . ActivationLocation ( Act )
          , LCheckpointMsg 
          ) 
        (* It would be more obviously right to show and remove this dialog  
           while holding WorkerMu, but I can't figure out a way that satisfies 
           a consistent lock order.  In any case, unlocking WorkerMu is OK,
           because we will only get here when StoredState = WrtBusyImmed or
           WrtBusyQueued, which means the only thing any other thread
           can do is wait on WaitingForIdle. 
        *) 
      ; LOCK WorkerMu 
        DO QueryingAssert := TRUE 
        ; WHILE QueryingAssert 
          DO Thread . Wait ( WorkerMu , WaitingForGuiAssertDialog ) 
          END (* WHILE *)
        ; LResult := StoredFailureAction 
        END (* LOCK *)
      ; UiDevel . RemoveGuiAssertDialog ( ) 
      ELSE (* Command line for query. *) 
        IF LInteractive
        THEN LResult := FailureDialogCommandLine ( AllowedActions )
        ELSE LResult := Failures . FailureActionTyp . FaCrash
        END (* IF *) 
      END (* IF *)
    ; CASE LResult
      OF Failures . FailureActionTyp . FaCrash
      => RTIO . PutText 
           ( "Terminating " & LbeStd . AppName & "." 
             & Wr . EOL  
             & "###############################################################"
             & Wr . EOL  
           ) 
      | Failures . FailureActionTyp . FaBackout  
      => RTIO . PutText
           ( "Backing up to before the last command." 
             & Wr . EOL  
             & "###############################################################"
             & Wr . EOL  
           ) 
      | Failures . FailureActionTyp . FaIgnore 
      => RTIO . PutText
           ( "Forging Quixotically ahead, in spite of the failure." 
             & Wr . EOL  
             & "###############################################################"
             & Wr . EOL  
           ) 
      END (* CASE *)  
    ; RTIO . Flush ( )
    ; RETURN LResult 
    END FailureQuery
    
(*EXPORTED*) 
; PROCEDURE AnswerGuiAssertDialog 
    ( FailureAction : Failures . FailureActionTyp ) 
  <* LL.sup <= VBT.mu *> 
  (* Gui threads call this to report to us, the user's response to
     a failure dialog. *) 

  = BEGIN 
      LOCK WorkerMu 
      DO
        StoredFailureAction := FailureAction
      ; QueryingAssert := FALSE  
      ; Thread . Signal ( WaitingForGuiAssertDialog )  
      END (* LOCK *)  
    END AnswerGuiAssertDialog 

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
        | Backout => 
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
      LOCK WorkerMu 
      DO RETURN IAmWorkerThread ( ) AND ActiveClosure . IsInteractive 
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
    WorkerMu := NEW ( MUTEX ) 
  ; WaitingForWork := NEW ( Thread . Condition ) 
  ; WaitingForBusy := NEW ( Thread . Condition ) 
  ; WaitingForIdle := NEW ( Thread . Condition ) 
  ; WaitingForGuiAssertDialog := NEW ( Thread . Condition ) 
  ; StoredState := WorkResultTyp . WrtDone 
  ; QueryingAssert := FALSE
  ; WorkerThreadClosure := NEW ( WorkerThreadClosureTyp ) 
  ; WorkerThreadClosure . stackSize := WantedThreadStackSize 
  ; WorkerThread := Thread . Fork ( WorkerThreadClosure ) 
  END Worker 
. 

