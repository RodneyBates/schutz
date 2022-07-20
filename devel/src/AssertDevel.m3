 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE AssertDevel 

; IMPORT Process 
; IMPORT Rd
; IMPORT RT0   
; IMPORT RTIO  
; IMPORT Stdio 
; IMPORT Text 
; IMPORT TextWr  
; IMPORT Thread 
; IMPORT Wr 

; IMPORT Assertions
; IMPORT Failures 
; IMPORT Files 
; IMPORT Images 
; IMPORT LbeStd 
; IMPORT MessageCodes 
; IMPORT Misc 
; IMPORT Options  
; IMPORT PaintHs 
; IMPORT UiDevel  
; IMPORT UiRecPlay   
; IMPORT Worker 

(*EXPORTED*) 
; PROCEDURE WriteCheckpoint 
    ( ImageRef : PaintHs . ImageTransientTyp (* Noop if NIL. *) 
    ; Message : TEXT 
    ; DoCreateVersion : BOOLEAN 
    ) 
  (* Writes messages to stderr and, if GUI, shows them in a dialog box. *)
  = VAR LResultMessage : TEXT

  ; BEGIN
      LResultMessage 
        := WriteCheckpointQuiet ( ImageRef , DoCreateVersion )
    ; RTIO . PutText ( LResultMessage )
    ; RTIO . Flush ( ) 
    ; IF Worker . DoGuiActions ( ) 
         AND LResultMessage # NIL
         AND NOT Text . Equal ( LResultMessage , "" )
      THEN UiDevel . ShowCheckpointNotice ( LResultMessage ) 
      END (*IF *)
(* TODO: Something with LResultMessage. *) 
    END WriteCheckpoint 

(*EXPORTED*) 
; PROCEDURE WriteCheckpointQuiet 
    ( ImageRef : PaintHs . ImageTransientTyp (* Noop if NIL. *) 
    ; DoCreateVersion : BOOLEAN 
    ) 
  : TEXT (* For display in a dialog box. *) (* May be NIL. *)
  (* Constructs and returns a message, for caller to decide what to do with.
     The message is EOL-terminated and may have internal EOLs too.
  *)

(* TODO: Is this really the right place for this procedure? *) 

  = VAR LCheckpointName : TEXT 
  ; VAR LMessage : TEXT
  ; VAR LMsgWrT : TextWr .T 

  ; CONST DefaultFileName = "IncompletelyOpened" 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN
      LMsgWrT := TextWr . New ( ) 
    ; IF ImageRef # NIL 
      THEN 
        LCheckpointName 
          := Misc . CheckpointName ( ImageRef . ItPers . IpAbsPklFileName )  
      ; IF LCheckpointName = NIL OR Text . Equal ( LCheckpointName , "" ) 
        THEN
          LCheckpointName 
            := Misc . AbsFileName ( Misc . CheckpointName ( DefaultFileName ) )
        END (* IF *)
      ; ImageRef . ItPers . IpCrashCommand := UiRecPlay . CurrentCommand ( ) 
(* TODO: ^Whoa, this is definitely not the right place to do this! *) 
      ; TRY 
          Files . WriteImagePickle 
            ( Images . PersistentImageToSave ( ImageRef , ForSave := FALSE )  
            , LCheckpointName  
            , DoCreateVersion := DoCreateVersion  
            )  
        (* Checkpoint write was successful. *) 
        ; Wr . PutText 
            ( LMsgWrT 
            , "A checkpoint has been written to file: " & Wr . EOL & "\""
            ) 
        ; Wr . PutText ( LMsgWrT , LCheckpointName ) 
        ; Wr . PutText ( LMsgWrT , "\"" & Wr . EOL ) 
        ; IF DoCreateVersion 
          THEN Wr . PutText ( LMsgWrT , "It is a new version." & Wr . EOL )
          ELSE Wr . PutText ( LMsgWrT , "It is NOT a new version." & Wr . EOL ) 
          END (* IF *) 

        EXCEPT (* Checkpoint write failed. *) 
          Files . Error ( EMessage ) 
        => Wr . PutText
             ( LMsgWrT , "Unable to write a checkpoint file because " ) 
        ; Wr . PutText ( LMsgWrT , EMessage ) 
        ; Wr . PutText ( LMsgWrT , "." & Wr . EOL )
        ELSE 
          Wr . PutText
            ( LMsgWrT , "Unable to write a checkpoint file. " & Wr . EOL ) 
        END (* TRY EXCEPT *) 
      ELSE 
        Wr . PutText ( LMsgWrT , "No Image to checkpoint." & Wr . EOL ) 
      END (* IF *)
    ; LMessage := TextWr . ToText ( LMsgWrT ) 
    ; RETURN LMessage 
    END WriteCheckpointQuiet  

(*EXPORTED*) 
; PROCEDURE xxxCheckpointForFailure ( CrashCode : MessageCodes . T )
  : TEXT (* Message about checkpoint. *) 

  = VAR LWindow : PaintHs . WindowRefTyp 
  ; VAR LImageRef : PaintHs . ImageTransientTyp 
  ; VAR LReason : TEXT 
  ; VAR LMessage : TEXT 
  ; VAR LMsgWrT : TextWr . T 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* CheckpointForFailure *)  

      LMsgWrT := TextWr . New ( ) 
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
        ; LMessage
            := WriteCheckpointQuiet ( LImageRef , DoCreateVersion := TRUE )
        END (* IF *) 
      END (* IF *)
    ; RETURN LMessage 
    END xxxCheckpointForFailure
    
; CONST AnsCrash = "c" 
; CONST AnsBackout = "b" 
; CONST AnsIgnore = "i" 

(*EXPORTED*) 
; PROCEDURE yyyFailureMessageCommandLine 
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
    ; RTIO . PutText ( "at: " ) 
    ; RTIO . PutText ( Failures . ActivationLocation ( Act ) ) 
    ; RTIO . PutText ( Wr . EOL ) 
    ; RTIO . PutText ( CheckpointMsg ) 
    ; RTIO . PutText ( Wr . EOL ) 
    END yyyFailureMessageCommandLine 

(*EXPORTED*) 
; PROCEDURE zzzFailureDialogCommandLine 
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
        ; RTIO . PutText ( "\" to terminate. " )
        ; RTIO . PutText ( Wr . EOL )
        END (* IF *)

      ; IF Failures . FailureActionTyp . FaBackout IN AllowedActions 
        THEN 
          RTIO . PutText ( "-- Type \"" )
        ; RTIO . PutText ( AnsIgnore )
        ; RTIO . PutText 
           ( "\" to attempt to reset to the state prior to the last command." ) 
        ; RTIO . PutText ( Wr . EOL )
        ; RTIO . PutText ( "This has a decent chance of working." ) 
        ; RTIO . PutText ( Wr . EOL )
        END (* IF *)

      ; IF Failures . FailureActionTyp . FaIgnore IN AllowedActions 
        THEN 
          RTIO . PutText ( "-- Type \"" )
        ; RTIO . PutText ( AnsIgnore )
        ; RTIO . PutText 
           ( "\" to ignore the failure and force execution to continue." ) 
        ; RTIO . PutText ( Wr . EOL )
        ; RTIO . PutText 
            ( "   WARNING: no telling what will happen if you try this." ) 
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
    END zzzFailureDialogCommandLine 

; BEGIN 
  END AssertDevel 
. 
