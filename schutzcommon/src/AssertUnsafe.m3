
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2021, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE AssertUnsafe 

(* UNSAFE stuff needed for snagging runtime errors from the RTS and
   querying the user about what to do with them. *)

; IMPORT Compiler 
; IMPORT Fmt 
; IMPORT M3toC 
; IMPORT RT0
; IMPORT RTException
; IMPORT RuntimeError
; IMPORT Thread

; IMPORT Assertions
; IMPORT MessageCodes

; TYPE AFT = MessageCodes . T 
; TYPE RterrT = RuntimeError . T 

; VAR GRteDotE : RT0 . ExceptionPtr (* RuntimeError . E  *)
; VAR GBackoutExc
        : RT0 . ExceptionPtr (* Assertions . Backout *)
; VAR GAssertionFailureExc
        : RT0 . ExceptionPtr (* Assertions . AssertionFailure *)
; VAR GOldBackstop : RTException . Backstop 

; CONST BackstoppedRtes
    = SET OF RterrT { RterrT . UnhandledException , RterrT . BlockedException } 

; PROCEDURE Backstop ( VAR Act : RT0 . RaiseActivation ; raises : BOOLEAN )
    RAISES ANY 

  = VAR LExc , LNewExc : RT0 . ExceptionPtr
  ; VAR LIntArg : INTEGER 
  ; VAR LFileString : ADDRESS := NIL 
  ; VAR LLocation , LMessage : TEXT := ""
  ; VAR LDoTerminate : BOOLEAN 
  
  ; BEGIN
      TRY
        EVAL RTException . SetBackstop ( GOldBackstop )
        (* ^Temporarily revert to the RT system's default backstop. *)

      ; TYPECASE Thread . Self ( )
        OF NULL => RTException . InvokeBackstop ( Act , raises )
                   (* ^Let the default backstop handle it. *)
        | Assertions . AssertThreadT ( TThread )
        => LExc := Act . exception 
        ; IF LExc # GRteDotE (* RuntimeError . E *)
          THEN RTException . InvokeBackstop ( Act , raises )
          ELSE
            LIntArg := LOOPHOLE ( Act . arg , INTEGER ) 
          ; IF LIntArg < ORD ( FIRST ( RterrT ) ) 
               OR LIntArg > ORD ( LAST ( RterrT ) )
            THEN LIntArg := ORD ( RterrT . Unknown )
            END (* IF *)
          ; IF NOT VAL ( LIntArg , RuntimeError . T ) IN BackstoppedRtes  
            THEN RTException . InvokeBackstop ( Act , raises )
            ELSE
              CASE TThread . AssertAction 
              OF Assertions . ActionTyp . Backout => LNewExc := GBackoutExc  
              | Assertions . ActionTyp . AssertionFailure
                => LNewExc := GAssertionFailureExc
              ELSE LNewExc := NIL
              END (* CASE *)
            ; IF LNewExc = NIL 
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
                  := "Runtime error "
                     & RuntimeError . Tag ( VAL ( LIntArg , RterrT ) ) 
              ; LDoTerminate
                  := TThread . QueryProc  
                       ( LLocation  
                       , LMessage 
                       , AFT . A_RuntimeError
                       , DoWriteCheckpoint := TRUE
                       )
              ; IF LDoTerminate 
                THEN RTException . InvokeBackstop ( Act , raises )
                ELSE
                  (* Change this exception to Backout or AssertionFailure
                     and raise that. *)
                  Act . un_except := Act . exception
                ; Act . un_arg := Act . arg
                ; Act . exception := LNewExc
                ; Act . arg := LOOPHOLE ( ORD ( RterrT . Unknown ) , ADDRESS ) 
                ; RTException . Raise ( Act )
                  (* ^This will rescan the frames for RAISES and a handler for
                     the changed exception.  Hopefully, it will get handled. *)
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *)
        ELSE (* Not our subtype of Thread.T. *) 
          RTException . InvokeBackstop ( Act , raises )
        END (* TYPECASE *) 
      FINALLY (* Rehook us as backstop. *) 
        GOldBackstop :=  RTException . SetBackstop ( Backstop )
        (* ^Restore this procedure as backstop, for next time. *)
      END (* FINALLY *) 
    END Backstop 

; PROCEDURE GetBackoutExc ( ) : RT0 . ExceptionPtr
  (* The RT0.ExceptionPtr for Assertions . Backout. *)

  = VAR LAct : RT0 . ActivationPtr

  ; BEGIN
      TRY
        RAISE Assertions . Backout ( "" )  
      EXCEPT
      | Assertions . Backout 
      => LAct := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ActivationPtr ) 
      ; RETURN LAct . exception
      ELSE RETURN NIL 
      END (* EXCEPT *) 
    END GetBackoutExc

; PROCEDURE GetAssertionFailureExc ( ) : RT0 . ExceptionPtr
  (* The RT0.ExceptionPtr for Assertions . AssertionFailure. *)

  = VAR LAct : RT0 . ActivationPtr

  ; BEGIN
      TRY
        RAISE Assertions . AssertionFailure ( "" )  
      EXCEPT
      | Assertions . AssertionFailure 
      => LAct := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ActivationPtr ) 
      ; RETURN  LAct . exception
      ELSE RETURN  NIL 
      END (* EXCEPT *) 
    END GetAssertionFailureExc

; BEGIN (* AssertUnsafe *)
    GRteDotE := RuntimeError . Self ( ) (* RuntimeError . E *) 
  ; GBackoutExc := GetBackoutExc ( )
  ; GAssertionFailureExc := GetAssertionFailureExc ( )
  ; GOldBackstop := RTException . SetBackstop ( Backstop )
  END AssertUnsafe 
. 
