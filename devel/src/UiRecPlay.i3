
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE UiRecPlay 

; IMPORT FormsVBT 
; IMPORT Thread 

; IMPORT PaintHs 

; TYPE CommandTyp 
    = { Null 
      , STOP 
      , FileOpen
      , FileQuit
      , FileSave
      , FileSaveAs
      , FileExport
      , FileCloseImage
      , FileCloseWindow
      , EditCut 
      , EditCopy 
      , EditPaste  
      , MouseClickPixel  
      , MouseClickChar 
      , CharDelFwd
      , CharDelBwd
      , DeleteRestOfLine 
      , CharType
      , CharTranspose 
      , CursorUp
      , CursorDown
      , CursorLeft
      , CursorRight
      , BeginKey 
      , EndKey 
      , PriorKey 
      , NextKey 
      , HomeKey
      , InsertModeOn 
      , InsertModeOff 
      , ClearSelection 
      , SweepSelection 
      , EndSweepSelection 
      , SemParse 
      , SemAccept  
      , SemAnalyze 
      , VertScroll 
      , HorizScroll 
      , TakeFocus
      , Repaint 
      , ReconstructLines 
      , VerifyLinesRefs 
      , ForceAssert 
      , MergeText
      , BrowseEst 
      , SetDebugLevel 
      , WriteCheckpoint 
      , WriteStats
      , WriteEstPickle
      , GenEstModule
      , WriteParseInfo
      , WriteFsTrees
      , WriteSemPickle
      , GenTokInterface
      , GenChildInterface
      , SearchStringFwd 
      , SearchStringBwd 
      , ReplaceString 
      } 

; PROCEDURE BeginCommand ( Command : CommandTyp ) : TEXT 

; PROCEDURE BeginCommandPlusString 
    ( Command : CommandTyp ; Param : TEXT ) : TEXT 

; PROCEDURE BeginCommandPlusChar 
    ( Command : CommandTyp ; Param : CHAR ) : TEXT 

; PROCEDURE BeginCommandPlusStringInt 
    ( Command : CommandTyp ; Param1 : TEXT ; Param2 : INTEGER ) 
  : TEXT 

; PROCEDURE BeginCommandPlusStringBool2 
    ( Command : CommandTyp 
    ; Param1 : TEXT 
    ; Param2 : BOOLEAN 
    ; Param3 : BOOLEAN 
    ) 
  : TEXT 

; PROCEDURE BeginCommandPlusString2BoolInt 
    ( Command : CommandTyp 
    ; Param1 : TEXT 
    ; Param2 : TEXT 
    ; Param3 : BOOLEAN 
    ; Param4 : INTEGER  
    ) 
  : TEXT 

; PROCEDURE BeginCommandPlusInt 
   ( Command : CommandTyp ; Param : INTEGER ) : TEXT 

; PROCEDURE BeginCommandPlusInt2 
    ( Command : CommandTyp ; Param1 : INTEGER ; Param2 : INTEGER ) : TEXT 

; PROCEDURE BeginCommandPlusInt3 
    ( Command : CommandTyp 
    ; Param1 : INTEGER 
    ; Param2 : INTEGER 
    ; Param3 : INTEGER 
    ) 
  : TEXT 

; PROCEDURE CurrentCommand ( ) : TEXT 

; PROCEDURE RecordString ( CommandString : TEXT  ) 

; PROCEDURE Record ( Command : CommandTyp ) 

; PROCEDURE RecordPlusString ( Command : CommandTyp ; Param : TEXT ) 

; PROCEDURE RecordPlusChar ( Command : CommandTyp ; Param : CHAR ) 

; PROCEDURE RecordPlusStringInt 
    ( Command : CommandTyp ; Param1 : TEXT ; Param2 : INTEGER ) 

; PROCEDURE RecordPlusStringBool2 
    ( Command : CommandTyp 
    ; Param1 : TEXT 
    ; Param2 : BOOLEAN 
    ; Param3 : BOOLEAN 
    ) 

; PROCEDURE RecordPlusString2BoolInt 
    ( Command : CommandTyp 
    ; Param1 : TEXT 
    ; Param2 : TEXT 
    ; Param3 : BOOLEAN 
    ; Param4 : INTEGER  
    ) 

; PROCEDURE RecordPlusInt ( Command : CommandTyp ; Param : INTEGER ) 

; PROCEDURE RecordPlusInt2 
    ( Command : CommandTyp ; Param1 : INTEGER ; Param2 : INTEGER ) 

; PROCEDURE RecordPlusInt3 
    ( Command : CommandTyp 
    ; Param1 : INTEGER 
    ; Param2 : INTEGER 
    ; Param3 : INTEGER 
    ) 

; PROCEDURE RecordOpen 
     ( RecPlayForm : FormsVBT . T ; FileName : TEXT ; Enabled : BOOLEAN ) 
  : BOOLEAN (* Successful. *) 

; PROCEDURE RecordEnable ( )   
  RAISES { Thread . Alerted } 

; PROCEDURE RecordClose ( ) 
  RAISES { Thread . Alerted }  

; PROCEDURE PlaybackOpen ( RecPlayForm : FormsVBT . T ; FileName : TEXT ) 
  : BOOLEAN (* Successful. *) 
  RAISES { Thread . Alerted } 

; VAR RespectStops : BOOLEAN 

; PROCEDURE ExecuteOneAction 
    ( CommandLine : TEXT 
    ; Window : PaintHs . WindowRefTyp 
    ; RespectStops : BOOLEAN 
    ; VAR DoSelfStop : BOOLEAN 
    ) 
  RAISES { Thread . Alerted } 

; PROCEDURE AttachHandlers ( WindowForm : FormsVBT . T ) 

; END UiRecPlay 
. 

