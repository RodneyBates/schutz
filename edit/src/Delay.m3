
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Schedule a method override to be called back as soon as possible after 
   a specified time. *)

MODULE Delay 

; IMPORT Thread 
; IMPORT Time

; REVEAL T = Public BRANDED OBJECT 
      LLink : T := NIL (* LLink and RLink are non-NIL IFF scheduled. *)   
    ; RLink : T := NIL 
    ; AbsTime : LONGREAL := 0.0D0 
    ; ID := "Delay.T" (* Help for debugging. *) 
    END  

; TYPE ThreadClosureTyp 
    = Thread . Closure 
      OBJECT 
        ID := "Delay.ThreadClosureTyp" (* Help for debugging. *) 
      OVERRIDES apply := WorkerProc 
      END 

; VAR ThreadClosure : ThreadClosureTyp 
; VAR Worker : Thread . T 
; VAR WorkAvailable : Thread . Condition 
; VAR Lock : MUTEX (* Single global lock. *) 
(* Lock protects all fields declared in type T, of all nodes of subtypes of T,
   plus the globals below: *) 
; VAR Agenda : T 
      (* List header of a doubly-linked list of events, most imminent on the
         left end of the list.  AbsTime field is unused in this header. *) 
; VAR ExecutingEvent : T := NIL 
; VAR LastExecutedEvent : T := NIL  (* For help debugging. *) 
(* End globals protected by Lock. *)  

; PROCEDURE LockedCancel ( Event : T ) 
  (* PRE: Lock is locked, Event is non-NIL and in Agenda. *) 

  = VAR LIsFirst : BOOLEAN 

  ; BEGIN 
      LIsFirst := Agenda . RLink = Event 
    ; Event . LLink . RLink := Event . RLink 
    ; Event . RLink . LLink := Event . LLink 
    ; IF LIsFirst 
         AND ExecutingEvent = NIL (* Worker is not executing a callback. *) 
      THEN (* Worker thread could be in AlertPause for Event's time. *) 
        Thread . Alert ( Worker ) 
      END (* IF *) 
    END LockedCancel 

(* VISIBLE: *) 
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

  = BEGIN 
      ScheduleAbsolute ( Time . Now ( ) + RelTime , Event ) 
    (* Yeah, I know this could get messed up if somebody changes the clock. *) 
    END ScheduleRelative 

(* VISIBLE: *) 
; PROCEDURE ScheduleAbsolute ( AbsTime : LONGREAL ; Event : T ) 
  (* Like ScheduleRelative, but arrange to call back at absolute time
     AbsTime.
  *) 

  = VAR LPred : T 
  ; VAR LSucc : T 
  ; VAR LWasEmpty : BOOLEAN 

  ; BEGIN 
      IF Event # NIL 
      THEN 
        LOCK Lock 
        DO 
          LWasEmpty := Agenda . LLink = Agenda 
        ; IF Event . LLink # NIL 
          THEN (* Its' already in Agenda.  Remove it before rescheduling. *) 
            LockedCancel ( Event ) 
          END (* IF *) 
        ; Event . AbsTime := AbsTime 
        ; LPred := Agenda . LLink (* Farthest in future. *)  
        ; WHILE LPred # Agenda AND LPred . AbsTime > AbsTime 
          DO 
            LPred := LPred . LLink 
          END (* WHILE *) 
        ; LSucc := LPred . RLink  
        ; Event . LLink := LPred 
        ; Event . RLink := LSucc 
        ; LPred . RLink := Event 
        ; LSucc . LLink := Event 
        ; IF LWasEmpty 
          THEN (* Worker thread could be waiting for Agenda to become 
                  nonempty. *)
            Thread . Signal ( WorkAvailable ) 
          ELSIF LPred = Agenda (* Event has become the new most-imminent. *)
                AND ExecutingEvent = NIL (* Worker not executing a callback. *)
          THEN (* Worker could be in AlertPause waiting for a no-longer 
                  most-imminent event. *)
            Thread . Alert ( Worker ) 
          END (* IF *) 
        END (* LOCK *) 
      END (* IF *) 
    END ScheduleAbsolute

(* VISIBLE: *) 
; PROCEDURE Cancel ( Event : T ; VAR WasScheduled : BOOLEAN ) 
  (* If Event is currently scheduled but has not begun, cancel it and set
     WasCancelled := TRUE.  Otherwise, just set WasScheduled := FALSE. 
  *) 

  = BEGIN 
      IF Event # NIL 
      THEN 
        LOCK Lock 
        DO 
          WasScheduled := Event . LLink # NIL 
        ; IF WasScheduled 
          THEN 
            LockedCancel ( Event ) 
          ; Event . LLink := NIL 
          ; Event . RLink := NIL 
          ; Event . AbsTime := 0.0D0  
          END (* IF *) 
        END (* LOCK *)
      END (* IF *) 
    END Cancel 

(* VISIBLE: *) 
; PROCEDURE Alert ( Event : T ; VAR WasExecuting : BOOLEAN ) 
  (* If Event is executing, Alert it and set WasExecuting := TRUE.  
     Otherwise, just set WasExecuting := FALSE. 
  *) 

  = BEGIN 
      IF Event # NIL 
      THEN 
        LOCK Lock 
        DO 
          WasExecuting := Event = ExecutingEvent  
        ; IF WasExecuting 
          THEN (* Worker is inside the callback of Event. *) 
            Thread . Alert ( Worker ) 
          END (* IF *) 
        END (* LOCK *) 
      END (* IF *) 
    END Alert 

(* VISIBLE: *) 
; PROCEDURE CancelOrAlert 
    ( Event : T ; VAR WasScheduled : BOOLEAN ; VAR WasExecuting : BOOLEAN ) 
  (* If Event is currently scheduled but has not begun, cancel it and set
     WasCancelled := TRUE.  Otherwise, set WasScheduled := FALSE.
     If Event is executing, Alert it and set WasExecuting := TRUE.
     Otherwise, set WasScheduled := FALSE.
     (These cases are mutually exclusive.)   
  *) 

  = BEGIN 
      IF Event # NIL 
      THEN 
        LOCK Lock 
        DO 
          WasScheduled := Event . LLink # NIL 
        ; WasExecuting := Event = ExecutingEvent  
        ; IF WasScheduled 
          THEN 
            LockedCancel ( Event ) 
          ; Event . LLink := NIL 
          ; Event . RLink := NIL 
          ; Event . AbsTime := 0.0D0  
          ELSIF WasExecuting 
          THEN (* Worker is inside the callback of Event. *) 
            Thread . Alert ( Worker ) 
          END (* IF *) 
        END (* LOCK *) 
      END (* IF *) 
    END CancelOrAlert 

(* VISIBLE: *) 
; PROCEDURE IsScheduled ( Event : T ) : BOOLEAN 
  (* Event is scheduled but has not begun execution.  This result could be 
     outdated by the time you get it. 
  *) 

  = BEGIN 
      IF Event = NIL 
      THEN RETURN FALSE 
      ELSE 
        LOCK Lock 
        DO 
          RETURN Event . LLink # NIL 
        END (* LOCK *) 
      END (* IF *) 
    END IsScheduled 

(* VISIBLE: *) 
; PROCEDURE IsExecuting ( Event : T ) : BOOLEAN 
  (* Event is currently executing.  This result could be outdated by the time 
     you get it. 
  *) 

  = BEGIN 
      IF Event = NIL 
      THEN RETURN FALSE 
      ELSE 
        LOCK Lock 
        DO 
          RETURN Event = ExecutingEvent  
        END (* LOCK *) 
      END (* IF *) 
    END IsExecuting 

; PROCEDURE WorkerProc 
  ( <* UNUSED *> ThreadClosure : ThreadClosureTyp ) : REFANY  

  = VAR LTime : LONGREAL 
  ; VAR LEvent : T 

  ; BEGIN 
      LOOP (* Nonterminating. *) 
        LOCK Lock 
        DO 
          ExecutingEvent := NIL (* A little self-healing. *) 
        (* Wait for Agenda to be nonempty. *)  
        ; WHILE Agenda . LLink = Agenda
          DO (* Agenda is empty.  Wait for it to become nonempty.  Nothing 
                else relevant can happen. *)  
            Thread . Wait ( Lock , WorkAvailable ) 
          END (* WHILE *) 
        (* Agenda is nonempty.  Save the most-imminent time. *) 
        ; LTime := Agenda . RLink . AbsTime 
          (* ^Scheduled time of earliest item on agenda. *) 
        END (* LOCK *) 
      ; TRY 
        (* Pause until the most-imminent time. *) 
          Thread . AlertPause ( LTime )
          (* ^Will be alerted if there is any alteration to the most-imminent
             event on Agenda. *) 
        ; LOCK Lock 
          DO (* Above-noted scheduled time has arrived.  But Agenda could have
                changed before we got Lock again. *) 
            IF Thread . TestAlert ( ) 
            THEN (* A late alert that happened while waiting to reacquire Lock.
                    Just go around the outer loop. *)  
            ELSIF Agenda . LLink = Agenda 
            THEN (* All events were removed while waiting to reacquire Lock.  
                    Just go around the outer loop. *)  
            ELSE 
              LEvent := Agenda . RLink 
            (* LEvent could be one that crowded in front of the one we were
               waiting for, but if so, its time has also arrived, and we want 
               to run its callback first. *)  
            ; IF LEvent . AbsTime <= Time . Now ( ) 
              THEN 
              (* It's too late to stop LEvent now. *) 
                LEvent . LLink . RLink := LEvent . RLink 
              ; LEvent . RLink . LLink := LEvent . LLink 
              ; LEvent . LLink := NIL 
              ; LEvent . RLink := NIL 
              ; ExecutingEvent := LEvent 
              (* Starting now, any Alert will go to the callback. *) 
              ; Thread . Release ( Lock ) 
              ; TRY 
                  LEvent . apply ( ) 
                EXCEPT ELSE (* Particularly Thread.Alerted. *)  
                END (* EXCEPT *) 
              ; EVAL Thread . TestAlert ( ) (* Clear any pending alert. *) 
              ; Thread . Acquire ( Lock ) 
              ; LastExecutedEvent := ExecutingEvent 
              ; ExecutingEvent := NIL 
              (* Starting now, any new Alert will be handled by WorkerProc. *) 
              ELSE (* The event we were waiting for has disappeared or been
                      postponed, and there is no other whose time has come. *) 
              END (* IF *)
            END (* IF *) 
          END (* LOCK *)  
        EXCEPT 
        Thread . Alerted 
        => (* Agenda could have somehow changed.  Just go around the outer 
              loop. *) 
        END (* TRY EXCEPT *) 
      END (* LOOP *) 
    END WorkerProc 

; BEGIN (* Delay *) 
    Lock := NEW ( MUTEX )  
    (* Nothing that uses this module could be cyclically required to be
       initialized first. *) 
  ; Agenda := NEW ( T ) 
  ; Agenda . LLink := Agenda 
  ; Agenda . RLink := Agenda 
  ; ExecutingEvent := NIL 
  ; LastExecutedEvent := NIL 
  ; WorkAvailable := NEW ( Thread . Condition ) 
  ; ThreadClosure := NEW ( ThreadClosureTyp ) 
  ; Worker := Thread . Fork ( ThreadClosure ) 
  END Delay 
. 
