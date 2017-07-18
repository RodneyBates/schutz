
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE AssertDevel 

; IMPORT Process 
; IMPORT Rd  
; IMPORT Stdio 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr 

; IMPORT Assertions
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

(* VISIBLE: *) 
; PROCEDURE WriteCheckpoint 
    ( ImageRef : PaintHs . ImageTransientTyp (* Noop if NIL. *) 
    ; <* UNUSED *> Message : TEXT 
    ; DoCreateVersion : BOOLEAN 
    ) 
(* TODO: Is this really the right place for this procedure? *) 

  = VAR LCheckpointName : TEXT 
  ; VAR LMessage : TEXT 
  ; VAR LGui : BOOLEAN 

  ; CONST DefaultFileName = "IncompletelyOpened" 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN 
      IF ImageRef # NIL 
      THEN 
        LCheckpointName 
          := Misc . CheckpointName ( ImageRef . ItPers . IpAbsPklFileName )  
      ; IF LCheckpointName = NIL OR Text . Equal ( LCheckpointName , "" ) 
        THEN
          LCheckpointName 
            := Misc . AbsFileName ( Misc . CheckpointName ( DefaultFileName ) )
        END (* IF *) 
      ; ImageRef . ItPers . IpCrashCommand := UiRecPlay . CurrentCommand ( ) 
      ; LGui := Worker . DoGuiActions ( ) 
      ; TRY 
          Files .WriteImagePickle 
            ( Images . PersistentImageToSave ( ImageRef , ForSave := FALSE )  
            , LCheckpointName  
            , DoCreateVersion := DoCreateVersion  
            )  
        (* Checkpoint write was successful. *) 
        ; Wr . PutText 
            ( Stdio . stderr 
            , "A checkpoint has been written to file:"
               & Wr . EOL  
               & "\"" 
               & LCheckpointName 
               & "\"" 
               & Wr . EOL  
            ) 
        ; IF DoCreateVersion 
          THEN
            Wr . PutText 
              ( Stdio . stderr , "It is a new version." & Wr . EOL )
          ELSE 
            Wr . PutText 
              ( Stdio . stderr , "It is NOT a new version." & Wr . EOL ) 
          END (* IF *) 
        ; IF LGui 
          THEN
            LMessage 
              := "A checkpoint has been written to file:" 
                 & Wr . EOL 
                 & "\"" & LCheckpointName & "\""  
          ; IF DoCreateVersion 
            THEN
              LMessage := LMessage & Wr . EOL & "It is a new version." 
            ELSE 
              LMessage := LMessage & Wr . EOL & "It is NOT a new version." 
            END (* IF *) 
          ; UiDevel . ShowCheckpointNotice ( LMessage ) 
          END (* IF *) 

        EXCEPT (* Checkpoint write failed. *) 
          Files . Error ( EMessage ) 
        => Wr . PutText 
             ( Stdio . stderr 
             , "And furthermore, it can't write a checkpoint file because: " 
               & EMessage 
               & "." 
               & Wr . EOL  
             ) 
        ; Wr . Flush ( Stdio . stderr ) 
        ; IF LGui
          THEN
            LMessage 
              := "Unable to write a checkpoint to file:" 
                 & Wr . EOL 
                 & "\"" & LCheckpointName & "\""  
                 & Wr . EOL 
                 & EMessage 
          ; UiDevel . ShowCheckpointNotice ( LMessage ) 
          END (* IF *) 
        ELSE 
          Wr . PutText 
            ( Stdio . stderr 
            , "And furthermore, it can't write a checkpoint file." 
              & Wr . EOL  
            ) 
        ; Wr . Flush ( Stdio . stderr ) 
        ; IF LGui
          THEN
            LMessage 
              := "Unable to write a checkpoint to file:" 
                 & Wr . EOL 
                 & "\"" & LCheckpointName & "\"" 
          ; UiDevel . ShowCheckpointNotice ( LMessage ) 
          END (* IF *) 
        END (* TRY EXCEPT *) 
      ELSE 
        Wr . PutText ( Stdio . stderr , "No Image to checkpoint." & Wr . EOL ) 
      END (* IF *) 
    ; Wr . Flush ( Stdio . stderr )
    END WriteCheckpoint 

(* VISIBLE: *) 
; PROCEDURE CheckpointForFailure ( CrashCode : MessageCodes . T ) 

  = VAR LWindow : PaintHs . WindowRefTyp 
  ; VAR LImageRef : PaintHs . ImageTransientTyp 
  ; VAR LMessage : TEXT 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* CheckpointForFailure *)  
      LWindow := Options . MainWindow  
   (* LWindow := FormsVBT . GetGeneric ( Options . MainForm , "Fv_LbeWindow" ) 
      Can attempt to reacquire a mutex already held. *) 
    ; IF LWindow # NIL 
      THEN 
        LImageRef := LWindow . WrImageRef 
      ; IF LImageRef = NIL 
        THEN 
          LImageRef := Options . OpeningImageRef 
        ; IF LImageRef # NIL 
          THEN 
            Wr . PutText 
              ( Stdio . stderr 
              , "Using Options.OpeningImageRef for checkpoint." & Wr . EOL 
              ) 
          END (* IF *) 
        END (* IF *) 
      ; IF LImageRef # NIL 
        THEN 
          LImageRef . ItPers . IpCrashCode := ORD ( CrashCode )  
        ; LMessage := MessageCodes . Image ( CrashCode ) 
        ; WriteCheckpoint ( LImageRef , LMessage , DoCreateVersion := TRUE ) 
        END (* IF *) 
      END (* IF *) 
    END CheckpointForFailure 

(* VISIBLE: *) 
; PROCEDURE AssertDialogCommandLine 
    ( <*UNUSED*> String1 : TEXT 
    ; <*UNUSED*> String2 : TEXT 
    ; Code : MessageCodes . T 
    ; <*UNUSED*> DoWriteCheckpoint : BOOLEAN 
    ) 
  : BOOLEAN 
  (* Conduct a command line dialog about an assertion failure. *) 

  = VAR LResponse : TEXT 
  ; VAR LResult : BOOLEAN 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN 
      Wr . PutText 
        ( Stdio . stderr 
        , "###############################################################"
          & Wr . EOL  
          & "OH NO!" 
          & Wr . EOL  
          & "The usually marvelous " 
          & LbeStd . AppName 
          & " editor has suffered a humiliating"
          & Wr . EOL  
          & "ASSERTION FAILURE!" 
          & Wr . EOL  
        ) 
    ; CheckpointForFailure ( Code ) 
    ; IF DoStop 
      THEN 
        Wr . PutText 
          ( Stdio . stderr , "-- Type \"t\" to terminate. " & Wr . EOL ) 
      ; Wr . PutText 
          ( Stdio . stderr 
          , "-- Type \"c\" to force execution to continue."  
            & Wr . EOL 
            & "   WARNING: there is no telling what might happen if you try this." 
            & Wr . EOL 
          ) 
      ; Wr . PutText 
          ( Stdio . stderr  
          , "-- Otherwise, " 
             & LbeStd . AppName 
             & " will attempt to reset itself to the state" 
             & Wr . EOL  
             & "   prior to the last command, which is not guaranteed to work." 
             & Wr . EOL  
          )  
      ; Wr . PutText ( Stdio . stderr , LbeStd . AppName & "> " ) 
      ; Wr . Flush ( Stdio . stderr )
      ; TRY 
          LResponse := Rd . GetLine ( Stdio . stdin )  
        EXCEPT ELSE 
          LResponse := "" 
        END (* TRY EXCEPT *)
      ELSE 
        LResponse := "t" 
      END (* IF *) 
    ; IF Text . Equal ( LResponse , "c" ) 
      THEN 
        Wr . PutText 
          ( Stdio . stderr 
          , "Forging Quixotically ahead, in spite of the failure." 
            & Wr . EOL  
            & "###############################################################"
            & Wr . EOL  
          ) 
      ; Wr . Flush ( Stdio . stderr )
      ; LResult := FALSE 
      ELSIF Text . Equal ( LResponse , "t" ) 
      THEN 
        Wr . PutText 
          ( Stdio . stderr 
          , "Terminating " & LbeStd . AppName & "." 
            & Wr . EOL  
            & "###############################################################"
            & Wr . EOL  
          ) 
      ; Wr . Flush ( Stdio . stderr )
      ; Assertions . TerminatingNormally := TRUE 
      ; Process . Exit ( 1 ) 
      ; LResult := TRUE (* Dead *)  
      ELSE 
        Wr . PutText 
          ( Stdio . stderr 
          , "Attempting to back up to before the last command." 
            & Wr . EOL  
            & "###############################################################"
            & Wr . EOL  
          ) 
      ; Wr . Flush ( Stdio . stderr )
      ; LResult := TRUE 
      END (* IF *)  
    ; RETURN LResult 
    END AssertDialogCommandLine 

; PROCEDURE InnerRuntimeFailureDialog ( ) 

  = VAR LResponse : TEXT 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN 
      Wr . PutText 
        ( Stdio . stderr 
        , "###############################################################"
          & Wr . EOL  
          & "OH NO!" 
          & Wr . EOL  
          & "The usually marvelous " 
          & LbeStd . AppName 
          & " editor has suffered a humiliating"
          & Wr . EOL  
          & "RUNTIME ERROR!" 
          & Wr . EOL  
        ) 
    ; CheckpointForFailure ( MessageCodes . T . RuntimeError ) 
    ; IF DoStop 
      THEN 
        Wr . PutText 
          ( Stdio . stderr , "-- <Type Return> to terminate. " & Wr . EOL ) 
      ; Wr . PutText ( Stdio . stderr , LbeStd . AppName & "> " ) 
      ; Wr . Flush ( Stdio . stderr )
      ; TRY 
          LResponse := Rd . GetLine ( Stdio . stdin )  
        EXCEPT ELSE 
          LResponse := "" 
        END (* TRY EXCEPT *)
      ELSE 
      END (* IF *) 
    END InnerRuntimeFailureDialog 

(* VISIBLE: *) 
; PROCEDURE RuntimeFailureDialog ( ) 
  (* Conduct a command line dialog about a runtime error. *) 

  = BEGIN 
      IF NOT Assertions . TerminatingNormally
      THEN 
        InnerRuntimeFailureDialog ( ) 
      END (* IF *) 
    END RuntimeFailureDialog 

; BEGIN 
  END AssertDevel 
. 
