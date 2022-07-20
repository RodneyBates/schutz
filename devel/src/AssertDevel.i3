
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE AssertDevel

; IMPORT RT0 

; IMPORT Failures 
; IMPORT MessageCodes 
; IMPORT PaintHs 

; VAR DoStop : BOOLEAN := TRUE 
  (* Causes the *Dialog routines to stop and conduct a dialog about what 
     to do about the failure. 
  *) 

; PROCEDURE WriteCheckpoint 
    ( ImageRef : PaintHs . ImageTransientTyp (* Noop if NIL. *) 
    ; Message : TEXT 
    ; DoCreateVersion : BOOLEAN 
    ) 
  (* Writes messages to stderr and shows them in a dialog box, if
     interactive. *) 

; PROCEDURE WriteCheckpointQuiet 
    ( ImageRef : PaintHs . ImageTransientTyp (* Noop if NIL. *) 
    ; DoCreateVersion : BOOLEAN 
    ) 
  : TEXT (* For display in a dialog box. *) (* May be NIL. *)
  (* Constructs and returns a message, for caller to decide what to do with.
     The message is EOL-terminated and may have internal EOLs too.
  *)

(* TODO: Is this really the right place for this procedure? *) 

; PROCEDURE xxxCheckpointForFailure ( CrashCode : MessageCodes . T )
  : TEXT (* Message about checkpoint. *) 

; PROCEDURE yyyFailureMessageCommandLine 
    ( READONLY Act : RT0 . RaiseActivation
    ; StoppedReason : TEXT 
    ; CheckpointMsg : TEXT 
    )
  (* Write a command line notification of a failure. *) 

; PROCEDURE zzzFailureDialogCommandLine 
    ( AllowedActions : Failures . FailureActionSetTyp )
  : Failures . FailureActionTyp
  (* Conduct a command line dialog about a blocked or uncaught exception. *) 

; END AssertDevel 
.
