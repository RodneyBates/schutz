
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Coroutine 

(* Implement simple paired coroutines.  (Use oversynchronized threads to 
   do it. 
*) 

; IMPORT Thread 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert 
; IMPORT MessageCodes 

; TYPE AFT = MessageCodes . T 

; TYPE ClosureTyp 
    = Thread . Closure 
        OBJECT 
          Cr : T 
        ; ChildProc : ProcOfT 
        OVERRIDES 
          apply := ApplyBody 
        END (* OBJECT  ClosureTyp *) 

; TYPE RoutineKindTyp = { Creator , Child } 

; REVEAL T 
    = MUTEX 
        BRANDED 
          Brand 
          OBJECT 
  (* A Coroutine . T controls the relationship 
     between two threads, synchronized as coroutines. *) 
            Closure : ClosureTyp 
(* CHECK: ^Do we really need this field? *) 
          ; Active : RoutineKindTyp 
          ; ChildHasReturned : BOOLEAN 
          ; Threads 
            : ARRAY [ RoutineKindTyp . Creator .. RoutineKindTyp . Child ] 
              OF Thread . T 
          ; Conditions 
            : ARRAY [ RoutineKindTyp . Creator .. RoutineKindTyp . Child ] 
              OF Thread . Condition 
          END (* OBJECT  T *) 

; PROCEDURE ApplyBody ( Self : ClosureTyp ) : REFANY 

  = <* FATAL Assertions . AssertionFailure *> BEGIN (* ApplyBody *) 
      LOCK Self . Cr 
      DO (* If necessary, wait for initial resume by creator *) 
         IF Self . Cr . Active = RoutineKindTyp . Creator 
         THEN 
           Thread . Wait 
             ( Self . Cr , Self . Cr . Conditions [ RoutineKindTyp . Child ] ) 
         ; Assert 
             ( Self . Cr . Active = RoutineKindTyp . Child 
             , AFT . A_CoroutineApplyBody_ChildNotActiveInitially 
             ) 
         END (* IF *) 
      ; Assert 
          ( Self . Cr . Threads [ RoutineKindTyp . Child ] 
            = Thread . Self ( ) 
          , AFT . A_CoroutineApplyBody_ChildInitiallyNotSelfThread 
          ) 
      END (* LOCK  *) 
    ; Self . ChildProc ( Self . Cr ) 
    ; LOCK Self . Cr 
      DO Assert 
           ( Self . Cr . Active = RoutineKindTyp . Child 
           , AFT . A_CoroutineApplyBody_ChildReturnedWhenNotActive 
           ) 
      ; Assert 
          ( Self . Cr . Threads [ RoutineKindTyp . Child ] 
            = Thread . Self ( ) 
          , AFT . A_CoroutineApplyBody_ChildReturnedButNotSelfThread 
          ) 
      ; Self . Cr . ChildHasReturned := TRUE 
      ; LockedResume ( Self . Cr ) 
      END (* LOCK *) 
    ; LOOP Resume ( Self . Cr ) END (* LOOP *) 
    END ApplyBody 

(* VISIBLE: *) 
; PROCEDURE Init ( Cr : T ; ChildProc : ProcOfT ) : T 
  (* Create A pair with the calling thread as one coroutine 
     (called the "creator", and a new thread executing ChildProc 
     as the other (called the "child"). 
     The creator will continue to execute and the child will not. *) 

  = VAR LClosure : ClosureTyp 

  ; BEGIN (* Init *) 
      LOCK Cr 
      DO LClosure := NEW ( ClosureTyp , Cr := Cr , ChildProc := ChildProc ) 
      ; Cr . Active := RoutineKindTyp . Creator 
      ; Cr . ChildHasReturned := FALSE 
      ; Cr . Conditions [ RoutineKindTyp . Creator ] 
          := NEW ( Thread . Condition ) 
      ; Cr . Conditions [ RoutineKindTyp . Child ] 
          := NEW ( Thread . Condition ) 
      ; Cr . Threads [ RoutineKindTyp . Creator ] := Thread . Self ( ) 
      ; Cr . Threads [ RoutineKindTyp . Child ] := Thread . Fork ( LClosure ) 
      ; Cr . Closure := LClosure 
      END (* LOCK *) 
    ; RETURN Cr 
    END Init 

; PROCEDURE LockedResume ( Cr : T ) RAISES { Assertions . AssertionFailure } 
  (* Caller must hold lock on Cr *) 

  = VAR LSelf : RoutineKindTyp 

  ; BEGIN (* LockedResume *) 
      Assert 
        ( Cr . Threads [ Cr . Active ] = Thread . Self ( ) 
        , AFT . A_CoroutineLockedResume_WrongThread 
        ) 
    ; LSelf := Cr . Active 
    ; Cr . Active := VAL ( 1 - ORD ( LSelf ) , RoutineKindTyp ) 
    ; Thread . Signal ( Cr . Conditions [ Cr . Active ] ) 
    ; Thread . Wait ( Cr , Cr . Conditions [ LSelf ] ) 
    END LockedResume 

(* VISIBLE: *) 
; PROCEDURE Resume ( Cr : T ) RAISES { Assertions . AssertionFailure } 
  (* Switch execution to the other coroutine of T. 
     A Noop if the caller is the creator of Cr and 
     ChildProc has returned. *) 

  = BEGIN (* Resume *) 
      LOCK Cr DO LockedResume ( Cr ) END (* LOCK *) 
    END Resume 

(* VISIBLE: *) 
; PROCEDURE HasReturned ( Cr : T ) : BOOLEAN 
  (* TRUE if ChildProc of the child coroutine has returned. *) 

  = BEGIN (* HasReturned *) 
      LOCK Cr DO RETURN Cr . ChildHasReturned END (* LOCK *) 
    END HasReturned 

; BEGIN (* Coroutine *) 
  END Coroutine 
. 

