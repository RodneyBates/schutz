 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE Failures 

(* UNSAFE stuff needed for snagging runtime errors from the RTS and
   querying the user about what to do with them. *)

(* Library: *)
; IMPORT Compiler 
; IMPORT Fmt 
; IMPORT M3toC
; IMPORT Process 
; IMPORT RT0
; IMPORT RTException
; IMPORT RTIO 
; IMPORT RTProcedureSRC  
; IMPORT RuntimeError
; IMPORT TextWr 
; IMPORT Thread
; IMPORT Wr 

(* Application: *)
; IMPORT Assertions
; IMPORT LbeStd 
; IMPORT Misc 

<* PRAGMA LL *> 

; VAR GOldBackstop : RTException . Backstop

; TYPE FailureSyncTyp
    = MUTEX OBJECT (* Protects its fields: *) 
        Depth : INTEGER := 0
      ; BackstopIdle : Thread . Condition := NIL 
      END (* FailureSyncTyp *)
  (* ^To make sure no changes in backstop registration while executing
     inside a backstop.
  *) 

; VAR GFailureSync : FailureSyncTyp := NIL  

; TYPE ThreadInfoTyp = RECORD
      Link : ThreadInfoRefTyp := NIL
    ; Thread : Thread . T 
    ; QueryProc : QueryProcTyp := NIL
    ; QueryingActPtr : RT0 . ActivationPtr := NIL
    ; DoGui := FALSE 
    END
    
; REVEAL ThreadInfoRefTyp = BRANDED REF ThreadInfoTyp

; VAR GThreadInfoList : ThreadInfoRefTyp := NIL 

(* EXPORTED *) 
; PROCEDURE RegisterQueryProc ( QueryProc : QueryProcTyp ; FDoGui := FALSE )
  RAISES { Thread . Alerted } 
  (* Register a query procedure for Thread.Self. *) 

  = VAR LInfoRef , LTrailingInfoRef , LNewInfoRef : ThreadInfoRefTyp 
  ; VAR LThreadSelf : Thread . T
  
  ; BEGIN
      Thread . Acquire ( GFailureSync )
    ; WHILE GFailureSync . Depth > 0
      DO Thread . AlertWait ( GFailureSync , GFailureSync . BackstopIdle ) 
      END (* WHILE *) 
    (* No thread is inside backstop handling, neither while it holds
       GFailureSync, nor during querying, when it does not. *) 
    ; TRY (* FINALLY *)
        LThreadSelf := Thread . Self ( )
      ; IF LThreadSelf = NIL THEN RETURN END (* IF *) 
      ; LNewInfoRef 
          := NEW ( ThreadInfoRefTyp 
                 , Link := GThreadInfoList
                 , Thread := LThreadSelf
                 , QueryProc := QueryProc
                 , QueryingActPtr := NIL 
                 , DoGui := FDoGui 
                 )
      ; IF GThreadInfoList = NIL
           OR GThreadInfoList ^ . Thread = LThreadSelf 
        THEN (* Make the new node the only node. *)
          LNewInfoRef . Link := GThreadInfoList
        ; GThreadInfoList := LNewInfoRef
        ELSE 
          LTrailingInfoRef := NIL 
        ; LInfoRef := GThreadInfoList 
        ; LOOP
            IF LInfoRef = NIL
            THEN
              LNewInfoRef ^ . Link := NIL
            ; LTrailingInfoRef ^ . Link  := LNewInfoRef 
            ; EXIT 
            ELSIF LInfoRef ^ . Thread = LThreadSelf
            THEN (* Replace old node with the new one. *)
              LNewInfoRef ^ . Link := LTrailingInfoRef ^ . Link 
            ; LTrailingInfoRef ^ . Link := LNewInfoRef
            ; EXIT
            ELSE
              LTrailingInfoRef := LInfoRef 
            ; LInfoRef := LInfoRef ^ . Link 
            END (* IF *)
          END (* LOOP *)
        END (* IF *)
      FINALLY 
        Thread . Release ( GFailureSync )
      END (* FINALLY *) 
    END RegisterQueryProc 

 ; PROCEDURE QueryDefault
     ( <* UNUSED *> READONLY Act : RT0 . RaiseActivation
     ; <* UNUSED *> StoppedReason : TEXT 
     ; <* UNUSED *> AllowedActions : FailureActionSetTyp
     )
   : FailureActionTyp

   = BEGIN
(* IMPLEMENT ME. *)
       RETURN FailureActionTyp . FaCrash 
(*
      NewExc = NIL 
              THEN RTException . InvokeBackstop ( Act , raises )
              ELSE (* An interesting exception. *)
                (* Query the user about it. *) 
                IF Act . module # NIL
                THEN LFileString := Act . module . file
                END (* IF *)
              ; LLocation
                  := M3toC . StoT ( LFileString )
                     & ":" & Fmt . Int ( Act . line )
              ; LMessage
                  := "Runtime error " & RuntimeError . Tag ( LRTArg ) 
              ; LDoTerminate
                  := TThread . QueryProc  
                       ( LLocation  
                       , LMessage 
                       , AFT . A_RuntimeError
                       , DoWriteCheckpoint := TRUE
                       )
*)
     END QueryDefault 

; PROCEDURE ThreadInfoRef ( ) : ThreadInfoRefTyp
  (* The thread info for the executing thread. *)
  <* LL >= GFailureSync *>

  = VAR LThreadInfoRef : ThreadInfoRefTyp 
  ; VAR LThreadSelf : Thread . T
  
  ; BEGIN
      LThreadSelf := Thread . Self ( )
    ; IF LThreadSelf = NIL (* Can happen? *) 
      THEN RETURN
             NEW ( ThreadInfoRefTyp 
                 , Link := NIL
                 , Thread := LThreadSelf
                 , QueryProc := QueryDefault
                 ) 
      ELSE
        LThreadInfoRef := GThreadInfoList 
      ; LOOP
          IF LThreadInfoRef = NIL
          THEN
            RETURN
              NEW ( ThreadInfoRefTyp
                  , Link := NIL
                  , Thread := LThreadSelf
                  , QueryProc := QueryDefault
                  ) 
          END (* IF *)
        ; IF LThreadInfoRef . Thread = LThreadSelf
          THEN RETURN LThreadInfoRef
          END (* IF *)
        ; LThreadInfoRef := LThreadInfoRef . Link 
        END (* LOOP *) 
      END (* IF *)  
    END ThreadInfoRef

; PROCEDURE RTExcArg ( Arg : RT0 . ExceptionArg ) : RuntimeError . T
  = VAR LIntArg : INTEGER

  ; BEGIN
      LIntArg := LOOPHOLE ( Arg , INTEGER ) 
    ; IF LIntArg < ORD ( FIRST ( RuntimeError . T ) ) 
         OR LIntArg > ORD ( LAST ( RuntimeError . T ) )
      THEN LIntArg := ORD ( RuntimeError . T . Unknown )
      END (* IF *)
    ; RETURN VAL ( LIntArg , RuntimeError . T ) 
    END RTExcArg 

; CONST SecondaryRtes
    = SET OF RuntimeError . T
               { RuntimeError . T . UnhandledException
               , RuntimeError . T . BlockedException
               }

; PROCEDURE StoppedReason ( WasBlocked : BOOLEAN ) : TEXT

  = BEGIN
      IF WasBlocked THEN RETURN "Blocked"
      ELSE RETURN "Uncaught"
      END (* IF *) 
    END StoppedReason 

(* EXPORTED *) 
; PROCEDURE ExcName
    ( READONLY Act : RT0 . RaiseActivation ; Secondary := FALSE )
    : TEXT
  (* Name of exception raised by Act. 
     Secondary means the uncaught or blocked RT error, after an
     original exception, if such exists.
  *) 

  = VAR LExc : RT0 . ExceptionPtr
  ; VAR LArg : RT0 . ExceptionArg 
  
  ; BEGIN
      IF Act . un_except = NIL (* There is no secondary failure. *)
         OR Secondary (* There is, and it is asked-for. *) 
      THEN 
        LExc := Act . exception
      ; LArg := Act . arg 
      ELSE (* The primary exception was pushed down. *) 
        LExc := Act . un_except
      ; LArg := Act . un_arg 
      END (* IF *)
    ; RETURN ExcNameInternal ( Act , LExc , LArg ) 
    END ExcName 

; PROCEDURE ExcNameInternal
    ( READONLY Act : RT0 . RaiseActivation
    ; Exc : RT0 . ExceptionPtr
    ; Arg : ADDRESS 
    )
  : TEXT

  = VAR LResult : TEXT
  ; VAR LTextWrT : TextWr . T 
  ; VAR LRTArg : RuntimeError . T 

  ; <* FATAL Wr . Failure *>
    <* FATAL Thread . Alerted *>
    BEGIN
      IF Exc = NIL OR Exc . name = NIL 
      THEN RETURN "<unknown exception>"
      ELSE
        LTextWrT := TextWr . New ( ) 
      ; Wr . PutText
          ( LTextWrT , M3toC . StoT ( LOOPHOLE ( Exc . name , ADDRESS ) ) ) 
      ; IF Exc = GRuntimeErrorRef
        THEN
          LRTArg := RTExcArg ( Arg ) 
        ; Wr . PutText ( LTextWrT , "("  ) 
        ; Wr . PutText ( LTextWrT , RuntimeError . Tag ( LRTArg ) ) 
        ; IF LRTArg = RuntimeError . T . AssertFailed AND Act . info0 # NIL
          THEN
            Wr . PutText ( LTextWrT , "(\"" ) 
          ; Wr . PutText ( LTextWrT , LOOPHOLE ( Act . info0 , TEXT ) ) 
          ; Wr . PutText ( LTextWrT , "\")" ) 
          END (* IF *)
        ; Wr . PutText ( LTextWrT , ")" ) 
        ELSIF Exc = GAssertionFailureRef
        THEN
          Wr . PutText ( LTextWrT , "(\"" ) 
        ; Wr . PutText ( LTextWrT , LOOPHOLE ( Arg , TEXT ) ) 
        ; Wr . PutText ( LTextWrT , "\")" ) 
        END (* IF *)
      ; LResult := TextWr . ToText ( LTextWrT )
      ; IF LResult = NIL THEN LResult := "" END (* IF *)
        (* ^Do we really need this? *) 
      ; RETURN LResult 
      END (* IF *) 
    END ExcNameInternal

(* EXPORTED *) 
; PROCEDURE ActivationLocation ( READONLY Act : RT0 . RaiseActivation ) : TEXT
    (* Code location where the raise denoted by Apt occurred. *) 

  = VAR LResult : TEXT
  ; LWrT : TextWr . T
  ; VAR LOffset : INTEGER 
  ; VAR LProcAddr : ADDRESS  
  ; VAR LFileNameS , LProcNameS : ADDRESS 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *>
    BEGIN
      IF Act . module # NIL
      THEN LFileNameS := Act . module . file
      ELSE LFileNameS := NIL 
      END (* IF *)
    ; LWrT := TextWr . New ( ) 
    ; IF LFileNameS = NIL AND Act . pc # NIL
      THEN (* Use the pc to get the raise location. *)
        Wr . PutText ( LWrT , "pc = 16_" )
      ; Misc . PutHex ( LWrT , LOOPHOLE ( Act . pc , INTEGER ) ) 
      ; RTProcedureSRC . FromPC
          ( Act . pc
          , (*VAR*) LProcAddr , (*VAR*) LFileNameS , (*VAR*) LProcNameS
          )
      ; LOffset
          := LOOPHOLE ( Act . pc , INTEGER ) - LOOPHOLE  ( LProcAddr , INTEGER )
      ; IF 0 <= LOffset AND LOffset < 65536 (* Why this limit? *) 
        THEN
          IF LProcNameS # NIL THEN
            Wr . PutText ( LWrT , " = " )
          ; Wr . PutText ( LWrT , M3toC . StoT ( LProcNameS ) )
          ; IF LOffset # 0
            THEN
              Wr . PutText ( LWrT , " + ")
            ; Misc . PutHex ( LWrT , LOffset)
            END (* IF *)
          END (* IF *)
        ; IF LFileNameS # NIL THEN
            Wr . PutText ( LWrT , " in file " )
          ; Wr . PutText ( LWrT , M3toC . StoT ( LFileNameS ) )
          END (* IF *)
        END (* IF *)
      ELSIF LFileNameS # NIL
      THEN
        Wr . PutText ( LWrT , "file \"") 
      ; Wr . PutText ( LWrT , M3toC . StoT ( LFileNameS ) )
      ; Wr . PutText ( LWrT , "\"")
      ; IF Act . line # 0
        THEN
          Wr . PutText ( LWrT , ", line ")
        ; Wr . PutText ( LWrT , Fmt . Int ( Act . line ) )
        END (* IF *)
      ELSE Wr . PutText ( LWrT , "<unknown location>" )
      END (* IF *) 
    ; LResult := TextWr . ToText ( LWrT ) 
    ; RETURN LResult 
    END ActivationLocation 

; PROCEDURE DoTerminate
    ( <* UNUSED *> READONLY Act : RT0 . RaiseActivation )

  = BEGIN
      RTIO . PutText ( Wr . EOL )
    ; RTIO . PutText ( "##### " )
    ; RTIO . PutText ( " Terminating " )
    ; RTIO . PutText ( LbeStd . AppName )
    ; RTIO . PutText ( ". #####" )
    ; RTIO . PutText ( Wr . EOL )
    ; RTIO . Flush ( )
    ; RAISE Terminate ( "" ) 
    END DoTerminate

; PROCEDURE TerminateBluntly ( ExitCode : INTEGER ) 

  = BEGIN
      RTIO . PutText ( "##### Bluntly terminating " )
    ; RTIO . PutText ( LbeStd . AppName )
    ; RTIO . PutText ( " with exit code " )
    ; RTIO . PutText ( Fmt . Int ( ExitCode ) ) 
    ; RTIO . PutText ( ". #####" )
    ; RTIO . PutText ( Wr . EOL )
    ; RTIO . Flush ( ) 
    ; Process . Exit ( ExitCode ) <* NORETURN *>
    END TerminateBluntly

; CONST SecondaryMap 
   = ARRAY BOOLEAN (* wasBlocked *)
     OF RuntimeError . T
          { RuntimeError . T . UnhandledException
          , RuntimeError . T . BlockedException
          }

; PROCEDURE ActIsFresh ( READONLY Act : RT0 . RaiseActivation ) : BOOLEAN
  (* We got this activation directly through RTHooks.Raise. *) 

  = BEGIN
      RETURN Act . info0 = NIL
             AND Act . info1 = NIL 
             AND Act . un_except = NIL 
             AND Act . un_arg = NIL 
    END ActIsFresh 

; PROCEDURE BackstopPrimary
    ( VAR Act : RT0 . RaiseActivation ; wasBlocked : BOOLEAN )
    RAISES ANY
(* Turn the fault into a secondary RT error, using the same
     activation, and reraise it. *)

  (* This is mostly just duplication of the default backstop in the
     runtime system, but we can't access that, while keeping our Backstop
     in use.
  *) 

  = BEGIN
      Act . un_except := Act . exception
    ; Act . un_arg    := Act . arg
    ; Act . exception := GRuntimeErrorRef 
    ; Act . arg
        := LOOPHOLE ( ORD ( SecondaryMap [ wasBlocked ] ) , RT0 . ExceptionArg ) 
    ; RTException . Raise ( Act ) 
    END BackstopPrimary 

; PROCEDURE Backstop ( VAR Act : RT0 . RaiseActivation ; wasBlocked : BOOLEAN )
    RAISES ANY
  (* Replace the RTS's default backstop.  This is called-back when an
     exception, including the runtime error exception RTException.E, 
     has been raised but has been found to be either blocked by lack
     of a RAISES clause or unhandled. *) 

  = VAR LActPtr : RT0 . ActivationPtr
  ; VAR LExc : RT0 . ExceptionPtr
  ; VAR LThreadInfoRef : ThreadInfoRefTyp
  ; VAR LRTArg : RuntimeError . T 
  ; VAR LAction : FailureActionTyp
  ; VAR LAllowedActions : FailureActionSetTyp
  ; VAR LActIsFresh : BOOLEAN 
  ; VAR LPrimaryWasBlocked : BOOLEAN 
  
  ; BEGIN
      LOCK GFailureSync 
      DO
        INC ( GFailureSync . Depth ) 
      ; LThreadInfoRef := ThreadInfoRef ( )
      END (* LOCK *) 

    ; LActPtr := LOOPHOLE ( ADR ( Act ) , RT0 . ActivationPtr )
    ; IF Act . exception = GTerminateRef 
      THEN (* This shouldn't happen. *) 
        RTIO . PutText ( Wr . EOL )
      ; RTIO . PutText ( "##### " )
      ; RTIO . PutText ( StoppedReason ( wasBlocked ) )  
      ; RTIO . PutText ( " exception Failures.Terminate. #####" )
      ; RTIO . PutText ( Wr . EOL )
      ; TerminateBluntly ( 2 ) <* NORETURN *> 
      END (* IF *)

    ; LActIsFresh := ActIsFresh ( Act )

    ; IF LActIsFresh AND Act . exception = GBackoutRef
      THEN (* Backout raised outside Failures and not handled. *) 
        RTIO . PutText ( Wr . EOL )
      ; RTIO . PutText ( "##### " )
      ; RTIO . PutText ( StoppedReason ( wasBlocked ) )  
      ; RTIO . PutText ( " exception Failures.Backout. #####" )
      ; RTIO . PutText ( Wr . EOL )
      ; TerminateBluntly ( 3 ) <* NORETURN *>
      ELSIF LActIsFresh AND Act . exception = GIgnoreRef
      THEN (* Ignore raised outside Failures and not handled. *)
        RTIO . PutText ( Wr . EOL )
      ; RTIO . PutText ( "##### " )
      ; RTIO . PutText ( StoppedReason ( wasBlocked ) )  
      ; RTIO . PutText ( " exception Failures.Ignore. #####" )
      ; RTIO . PutText ( Wr . EOL )
      ; TerminateBluntly ( 4 ) <* NORETURN *>
      ELSIF LActIsFresh AND LThreadInfoRef ^ . QueryingActPtr # NIL  
        (* Act was raised from somewhere dynamically inside a query about
           a previous failure.  We can't do anything more with this.
        *) 
      THEN
        RTIO . PutText ( Wr . EOL )
      ; RTIO . PutText ( "##### " )
      ; RTIO . PutText ( StoppedReason ( wasBlocked ) )  
      ; RTIO . PutText ( " exception while querying a previous one. #####" )
      ; RTIO . PutText ( Wr . EOL )

      ; RTIO . PutText ( "Original exception: " )
      ; RTIO . PutText ( ExcName ( LThreadInfoRef ^ . QueryingActPtr ^ ) ) 
      ; RTIO . PutText ( Wr . EOL )

      ; RTIO . PutText ( "Raised at: " )
      ; RTIO . PutText
          ( ActivationLocation ( LThreadInfoRef ^ . QueryingActPtr ^ ) ) 
      ; RTIO . PutText ( Wr . EOL )

      ; RTIO . PutText ( "New exception: " )
      ; RTIO . PutText ( ExcName ( Act ) ) 
      ; RTIO . PutText ( Wr . EOL )

      ; RTIO . PutText ( "Raised at: " )
      ; RTIO . PutText ( ActivationLocation ( Act ) ) 
      ; RTIO . PutText ( Wr . EOL )
      ; RTIO . Flush ( ) 

      ; TerminateBluntly ( 4 ) <* NORETURN *>
      ELSE 
        LExc := Act . exception 
      ; IF LExc # GRuntimeErrorRef (* Programmer-declared exception. *) 
        THEN BackstopPrimary ( Act , wasBlocked )
        ELSE (* RuntimeError.E => runtime error. *)
          LRTArg := RTExcArg ( Act . arg )
        ; IF NOT LRTArg IN SecondaryRtes  
          THEN (* Primary RT error. *)
            BackstopPrimary ( Act , wasBlocked )
          ELSE (* Secondary RT error (Unhandled or blocked), which
                  is now also Unhandled or blocked. 
                  Client code has passedup two chances to catch: the original
                  exception and a secondary exception (RT error: unhandled or
                  blocked), which also is now unhandled or blocked.  Now give
                  the human user some options. *)
            IF Act . un_except = GAssertionFailureRef 
            THEN (* Only for AssertionFailure, is it clear that just raising
                    and catching an exception (Ignore) will resume execution
                    at the right place to ignore. *)  
              LAllowedActions := Assertions . AllowedActions 
            ELSE 
              LAllowedActions
                := FailureActionSetTyp
                     { FailureActionTyp . FaCrash
                     , FailureActionTyp . FaBackout
                     }
            END (* IF *)

          (* Query the user about what to do. *)
          ; LPrimaryWasBlocked := LRTArg = RuntimeError . T . BlockedException 
          ; LThreadInfoRef ^ . QueryingActPtr := LActPtr   
          ; LAction 
              := LThreadInfoRef . QueryProc
                   ( Act
                   , StoppedReason ( LPrimaryWasBlocked )
                   , LAllowedActions
                   )  

          (* Query's answer received. *) 
          ; LThreadInfoRef ^ . QueryingActPtr := NIL  
          ; CASE LAction 
            OF FailureActionTyp . FaBackout 
              => (* Change to Failures.Backout, which client code
                    can catch to recover from the original exception. *)
                Act . exception := GBackoutRef
              ; Act . arg := LOOPHOLE ( ActivationLocation ( Act ) , ADDRESS )
(* REVIEW: Somewhere, the TEXT argument of AssertionFailure is caught and
           copied to the argument of Backout, which is different from what
           is done here.  But in general, the primary exception to Backout
           can be anything.  What to do?
*)
              ; RTException . Raise ( Act )

            | FailureActionTyp . FaIgnore 
              => (* Ignoring an exception will likely work only when the
                    original exception was Assertions.AssertionFailure,
                    and thus something in Assertions is in the call chain,
                    where it can catch Failures.Ignore and return without
                    action action. *)
                Act . exception := GIgnoreRef 
              ; RTException . Raise ( Act )

            | FailureActionTyp . FaCrash 
            =>  RTIO . PutText ( Wr . EOL )
              ; RTIO . PutText ( "##### " )
              ; RTIO . PutText ( StoppedReason ( LPrimaryWasBlocked ) ) 
              ; RTIO . PutText ( " exception " )
              ; RTIO . PutText ( ExcName ( Act ) )
              ; RTIO . PutText ( "##### " )
              ; RTIO . PutText ( Wr . EOL )

              ; RTIO . PutText ( "  allowed to fail by user." )
              ; RTIO . PutText ( Wr . EOL )

              ; RTIO . PutText ( "Raised at: " )
              ; RTIO . PutText ( ActivationLocation ( Act ) ) 
              ; RTIO . PutText ( Wr . EOL )
              ; RTIO . Flush ( ) 

              ; DoTerminate ( Act )
            END (* CASE *)
          END (* IF *) 
        END (* IF *)
      END (* IF *)
    ; LOCK GFailureSync 
      DO
        DEC ( GFailureSync . Depth )
      ; IF GFailureSync . Depth <= 0
        THEN Thread . Signal ( GFailureSync . BackstopIdle )
        END (* IF *) 
      END (* LOCK *) 
    END Backstop 

(* Initializing RT0.ExceptionPtr values for certain declared exceptions: *)

; VAR GBackoutRef : RT0 . ExceptionPtr := NIL

; PROCEDURE InitBackoutRef ( ) 
  (* Initialize GBackoutRef, the RT0.ExceptionPtr for exception Backout. *)

  = BEGIN
      IF GBackoutRef = NIL
      THEN 
        TRY
          RAISE Backout ( "" )    
        EXCEPT
        | Backout 
        => GBackoutRef
             := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ActivationPtr )
                         (* ^ NOTE: Misnamed Compiler.ThisException should
                            be named ThisActivation!! *) 
              ^ . exception
        ELSE RETURN  
        END (* EXCEPT *)
      END (* IF *)
    END InitBackoutRef

; VAR GIgnoreRef : RT0 . ExceptionPtr := NIL  

; PROCEDURE InitIgnoreRef ( ) 
  (* Initialize GIgnoreRef, the RT0.ExceptionPtr for exception Ignore. *)

  = BEGIN
      IF GIgnoreRef = NIL
      THEN 
        TRY
          RAISE Ignore   
        EXCEPT
        | Ignore 
        => GIgnoreRef
           := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ActivationPtr ) 
                         (* ^ NOTE: Misnamed Compiler.ThisException should
                            be named ThisActivation!! *) 
              ^ . exception 
        ELSE RETURN 
        END (* EXCEPT *)
      END (* IF *)
    END InitIgnoreRef

; VAR GTerminateRef : RT0 . ExceptionPtr := NIL  

; PROCEDURE InitTerminateRef ( ) 
  (* Initialize GTerminateRef, the RT0.ExceptionPtr for exception Terminate. *)

  = BEGIN
      IF GTerminateRef = NIL
      THEN 
        TRY
          RAISE Terminate ( "" ) 
        EXCEPT
        | Terminate 
        => GTerminateRef
           := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ActivationPtr ) 
                         (* ^ NOTE: Misnamed Compiler.ThisException should
                            be named ThisActivation!! *) 
              ^ . exception 
        ELSE RETURN 
        END (* EXCEPT *)
      END (* IF *)
    END InitTerminateRef

; VAR GAssertionFailureRef : RT0 . ExceptionPtr := NIL  

; PROCEDURE InitAssertionFailureRef ( ) 
  (* Initialize GAssertionFailureRef, the RT0.ExceptionPtr for exception
     Assertions.AssertionFailure.
  *)

  = BEGIN
      IF GAssertionFailureRef = NIL
      THEN 
        TRY
          RAISE Assertions . AssertionFailure ( "" )   
        EXCEPT
        | Assertions . AssertionFailure 
        => GAssertionFailureRef
           := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ActivationPtr )
                         (* ^ NOTE: Misnamed Compiler.ThisException should
                            be named ThisActivation!! *) 
              ^ . exception 
        ELSE RETURN 
        END (* EXCEPT *)
      END (* IF *)
    END InitAssertionFailureRef

; VAR GRuntimeErrorRef : RT0 . ExceptionPtr := NIL  

; BEGIN (* Failures *)
    (* It is essential to initialize these exception pointers before
       execution gets into Backstop, because they raise exceptions
       that would torpedo Backstop.
    *) 
    InitBackoutRef ( ) 
  ; InitIgnoreRef ( ) 
  ; InitTerminateRef ( ) 
  ; InitAssertionFailureRef ( )
  ; GRuntimeErrorRef := RuntimeError . Self ( )
  ; GOldBackstop := RTException . SetBackstop ( Backstop )
  ; GFailureSync := NEW ( FailureSyncTyp )
  ; GFailureSync . Depth := 0 
  ; GFailureSync . BackstopIdle := NEW ( Thread . Condition ) 
  END Failures 
. 
