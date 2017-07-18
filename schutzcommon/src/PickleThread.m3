
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE PickleThread 

; IMPORT Thread 

(* Wrapper around Pickle.Write, using a separate thread with a big
   enough stack.
*) 

(* Except for swapping the strings "Pickle" and "Pickle2 AS Pickle", make no
   changes to the following line, as it is recognized/edited by scripts.
   (see scripts/topickle.sh and scripts/topickle2.sh)
*) 
; IMPORT Pickle2 AS Pickle (* The Pickle2 mess. *)
; IMPORT Wr 

; IMPORT LbeStd 
; IMPORT Assertions 

; FROM Assertions IMPORT AssertionFailure 

; TYPE StateTyp 
    = { Idle             (* Waiting for a client to request work. *)  
      , Busy             (* Worker thread is busy handling a request. *) 
      , Done             (* Worker thread has finished normally. 
                            Waiting to notify client . *) 
      , Error            (* Worker thread has finished with exception Error. 
                            Waiting for client to find out. *)  
      , AssertionFailure (* Worker thread has finished with AssertionFailure. 
                            Waiting for client to find out. *) 
      , Alerted          (* Worker thread has finished with Alerted. 
                            Waiting for client to find out. *) 
      }  

; VAR Mu : MUTEX  
  (* In the locking order, Mu > VBT . mu *) 

(* Mu protects these variables: *) 
; VAR WaitingForWork : Thread . Condition  
      (* Worker thread waits for work. *) 
; VAR WaitingForIdle : Thread . Condition 
      (* Clients wait for the worker thread to be available. *)   
; VAR WaitingForDone : Thread . Condition 
      (* The client being served waits for worker thread to finisn. *)  

; VAR StoredState : StateTyp := StateTyp . Idle 
; VAR StoredWrT : Wr . T 
; VAR StoredRef : REFANY 
; VAR StoredExceptionArg : TEXT 
(* End of Mu-protected variables. *)  

(* VISIBLE: *) 
; PROCEDURE Write 
    ( WrT : Wr . T 
    ; Ref : REFANY 
    ) 
  RAISES { Error , AssertionFailure , Thread . Alerted } 
  (* Do Pickle . Write with the same parameters, in a distinct thread
     whose stack will be set large for pickling.  Serialize requests.
     Forward Alerts to the pickling thread, "backward" the listed exceptions
     from the pickling thread to the caller.
  *) 

  = BEGIN 
      LOCK Mu 
      DO 
        WHILE StoredState # StateTyp . Idle 
        DO Thread . AlertWait ( Mu , WaitingForIdle ) 
           (* Let Thread . Alerted propagate out. *)  
        END (* IF *) 
      ; StoredWrT := WrT 
      ; StoredRef := Ref 
      ; StoredState := StateTyp . Busy 
      ; Thread . Signal ( WaitingForWork ) 
      ; LOOP 
          CASE StoredState <* NOWARN *> 
          OF StateTyp . Busy 
           => TRY 
                Thread . AlertWait ( Mu , WaitingForDone ) 
              EXCEPT 
              Thread . Alerted (* Client was alerted, forward to worker. *) 
              => Thread . Alert ( WorkerThread ) 
              END (* IF *) 
            (* And loop to wait some more. *) 
          | StateTyp . Done  
           => StoredState := StateTyp . Idle  
            ; Thread . Signal ( WaitingForIdle ) 
            ; EXIT (* And return normally. *) 
          | StateTyp . Error 
           => StoredState := StateTyp . Idle  
            ; Thread . Signal ( WaitingForIdle ) 
            ; RAISE Error ( StoredExceptionArg )   
          | StateTyp . AssertionFailure 
           => StoredState := StateTyp . Idle  
            ; Thread . Signal ( WaitingForIdle ) 
            ; RAISE AssertionFailure ( StoredExceptionArg )   
          | StateTyp . Alerted 
           => StoredState := StateTyp . Idle  
            ; Thread . Signal ( WaitingForIdle ) 
            ; RAISE Thread . Alerted 
          END (* CASE *) 
        END (* LOOP *) 
      END (* LOCK *) 
    END Write 

; PROCEDURE GetWork ( VAR WrT : Wr . T ; VAR Ref : REFANY ) 

  = BEGIN 
      LOCK Mu
      DO
        WHILE StoredState # StateTyp . Busy 
        DO TRY 
             Thread . AlertWait ( Mu , WaitingForWork ) 
           EXCEPT 
           Thread . Alerted 
           => StoredState := StateTyp . Alerted 
           ; Thread . Signal ( WaitingForDone ) 
           ; RETURN 
           END (* TRY EXCEPT *) 
        END (* WHILE *) 
      ; WrT := StoredWrT 
      ; Ref := StoredRef
      END (* LOCK *) 
    END GetWork 

; PROCEDURE FinishNormally ( ) 

  = BEGIN 
      LOCK Mu
      DO
        StoredState := StateTyp . Done 
      ; Thread . Signal ( WaitingForDone ) 
      END (* LOCK *) 
    END FinishNormally 

; PROCEDURE FinishError ( Arg : TEXT ) 

  = BEGIN 
      LOCK Mu
      DO
        StoredState := StateTyp . Error 
      ; StoredExceptionArg := Arg  
      ; Thread . Signal ( WaitingForDone ) 
      END (* LOCK *) 
    END FinishError 

; PROCEDURE FinishAssertionFailure ( Arg : TEXT ) 

  = BEGIN 
      LOCK Mu
      DO
        StoredState := StateTyp . AssertionFailure  
      ; StoredExceptionArg := Arg  
      ; Thread . Signal ( WaitingForDone ) 
      END (* LOCK *) 
    END FinishAssertionFailure  

; PROCEDURE FinishAlert ( ) 

  = BEGIN 
      LOCK Mu
      DO
        StoredState := StateTyp . Alerted  
      ; Thread . Signal ( WaitingForDone ) 
      END (* LOCK *) 
    END FinishAlert  

; PROCEDURE WorkerApply ( <* UNUSED *> Self : WorkerClosureTyp ) : REFANY 

  = VAR LWrT : Wr . T  
  ; VAR LRef : REFANY 

  ; BEGIN 
      LOOP 
        GetWork ( (* VAR *) LWrT , (* VAR *) LRef ) 
      ; TRY
          Pickle . Write ( LWrT , LbeStd . ImagePickleIdInfoRef ) 
        ; Pickle . Write ( LWrT , LRef ) 
        ; FinishNormally ( ) 
        EXCEPT 
        | Error ( EArg ) => FinishError ( EArg )  
        | AssertionFailure ( EArg ) => FinishAssertionFailure ( EArg )  
        | Thread . Alerted => FinishAlert ( )   
        ELSE FinishError ( NIL ) 
        END (* TRY EXCEPT *) 
      END (* LOOP *) 
    END WorkerApply 

; TYPE WorkerClosureTyp 
    = Thread . SizedClosure OBJECT 
      OVERRIDES 
        apply := WorkerApply 
      END 

; VAR WorkerClosure : WorkerClosureTyp 
; VAR WorkerThread : Thread . T  

; VAR WantedThreadStackSize := 1024000 (* Word.T's *) (* = 4 Meg Bytes. *)  

; BEGIN (* PickleThread *) 
    Mu := NEW ( MUTEX ) 
  ; WaitingForWork := NEW ( Thread . Condition ) 
  ; WaitingForIdle := NEW ( Thread . Condition ) 
  ; WaitingForDone := NEW ( Thread . Condition ) 
  ; StoredState := StateTyp . Idle 

  ; WorkerClosure := NEW ( WorkerClosureTyp ) 
  ; WorkerClosure . stackSize := WantedThreadStackSize 
  ; WorkerThread := Thread . Fork ( WorkerClosure ) 
  END PickleThread
.

