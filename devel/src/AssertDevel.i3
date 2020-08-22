
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
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

; PROCEDURE CheckpointForFailure ( CrashCode : MessageCodes . T ) 

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
