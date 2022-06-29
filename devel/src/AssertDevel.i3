
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE AssertDevel 

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

; PROCEDURE WriteCheckpointNoGui  
    ( ImageRef : PaintHs . ImageTransientTyp (* Noop if NIL. *) 
    ; Message : TEXT 
    ; DoCreateVersion : BOOLEAN 
    ) 
  : TEXT (* For display in a dialog box. *) (* May be NIL. *)
  (* Returns messages, in case caller already has things locked
     which would try to reacquire the same mutex on same thread.
  *)

; PROCEDURE CheckpointForFailure
    ( CrashCode : MessageCodes . T ; DisplayGui := TRUE )
  : TEXT (* Message to user. *) 

; PROCEDURE AssertDialogCommandLine 
    ( String1 : TEXT 
    ; String2 : TEXT 
    ; Code : MessageCodes . T 
    ; DoWriteCheckpoint : BOOLEAN 
    ) 
  : BOOLEAN 
  (* Conduct a command line dialog about an assertion failure. *) 

; PROCEDURE RuntimeFailureDialog ( ) 
  (* Conduct a command line dialog about a runtime error. *) 

; END AssertDevel 
.
