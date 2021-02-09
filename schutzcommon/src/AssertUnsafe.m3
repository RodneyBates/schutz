
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE AssertUnsafe 

(* UNSAFE stuff needed in handling assertions and runtime errors. *)

; IMPORT Compiler 
; IMPORT Fmt 
; IMPORT M3toC 
; IMPORT RT0
; IMPORT RTException
; IMPORT RuntimeError 

; IMPORT Assertions
; IMPORT MessageCodes

; TYPE AFT = MessageCodes . T 
; TYPE RterrT = RuntimeError . T 

; CONST BackstoppedRtes
    = SET OF RterrT { RterrT . UnhandledException , RterrT . BlockedException } 

; VAR GRteDotE : RT0 . ExceptionPtr (* RuntimeError . E  *)
; VAR GBackoutExc : RT0 . ExceptionPtr (* Assertions . Backout *)
; VAR GOldBackstop : RTException . Backstop 

; PROCEDURE Backstop ( VAR Act : RT0 . RaiseActivation ; raises : BOOLEAN )
    RAISES ANY 

  = VAR LExc : RT0 . ExceptionPtr
  ; VAR LIntArg : INTEGER 
  ; VAR LFileString : ADDRESS := NIL 
  ; VAR LLocation , LMessage : TEXT := ""
  ; VAR LDoTerminate : BOOLEAN 
  
  ; BEGIN
      TRY
        EVAL RTException . SetBackstop ( GOldBackstop )
        (* ^Set back to the default backstop. *)
      ; IF GBackoutExc = NIL OR NOT Worker . IAmWorkerThread ( ) 
        THEN RTException . InvokeBackstop ( Act , raises )
             (* ^Let the default backstop handle it. *)
        ELSE 
          LExc := Act . exception 
        ; IF LExc # GRteDotE (* RuntimeError . E *)
          THEN RTException . InvokeBackstop ( Act , raises )
               (* ^Let the default backstop handle it. *)
          ELSE
            LIntArg := LOOPHOLE ( Act . arg , INTEGER ) 
          ; IF LIntArg < ORD ( FIRST ( RterrT ) ) 
               OR LIntArg > ORD ( LAST ( RterrT ) )
            THEN LIntArg := ORD ( RterrT . Unknown )
            END (* IF *)
          ; IF VAL ( LIntArg , RuntimeError . T ) IN BackstoppedRtes 
            THEN RTException . InvokeBackstop ( Act , raises )
                 (* ^Let the default backstop handle it. *)
            ELSE (* An interesting exception. *)
              (* Query the user about it. *) 
              IF Act . module # NIL
              THEN LFileString := Act . module . file
              END (* IF *)
            ; LLocation
                := M3toC . StoT ( LFileString ) & ":" & Fmt . Int ( Act . line )
            ; LMessage
                := "Runtime error "
                   & RuntimeError . Tag ( VAL ( LIntArg , RterrT ) ) 
            ; LDoTerminate
              := Assertions . Callback
                       ( LLocation  
                       , LMessage 
                       , AFT . A_RuntimeError
                       , DoWriteCheckpoint := TRUE
                       )
            ; IF LDoTerminate 
              THEN RTException . InvokeBackstop ( Act , raises )
                   (* ^Let the default backstop handle this one too. *)
              ELSE
                (* Change this exception to Assertions.Backout and
                   raise that. *)
                Act . un_except := Act . exception
              ; Act . un_arg := Act . arg
              ; Act . exception := GBackoutExc
              ; Act . arg := LOOPHOLE ( ORD ( RterrT . Unknown ) , ADDRESS ) 
              ; RTException . Raise ( Act )
                (* ^This will rescan the frames for RAISES and a handler for the
                   changed exception.  Hopefully, it will get handled. *)
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      FINALLY
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
      ; GBackoutExc := LAct . exception
      ELSE GBackoutExc := NIL 
      END (* EXCEPT *) 
    END GetBackoutExc

; BEGIN (* AssertUnsafe *)
    GRteDotE := RuntimeError . Self ( ) (* RuntimeError . E *) 
  ; GBackoutExc := GetBackoutExc ( )
  ; GOldBackstop := RTException . SetBackstop ( Backstop )
  END AssertUnsafe 
. 
