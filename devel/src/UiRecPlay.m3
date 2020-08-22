
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE UiRecPlay 

; IMPORT FileRd 
; IMPORT FileWr 
; IMPORT FloatMode 
; IMPORT Fmt 
; IMPORT FormsVBT 
; IMPORT Lex 
; IMPORT OSError 
; IMPORT Rd 
; IMPORT Rsrc 
; IMPORT Stdio 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Trestle 
; IMPORT TrestleComm 
; IMPORT TypescriptVBT 
; IMPORT TextRd 
; IMPORT VBT 
; IMPORT Wr 

; IMPORT Assertions 
; FROM Assertions IMPORT AssertionFailure 
; IMPORT BuildLexMachine  
; IMPORT Display 
; IMPORT EditWindow 
; IMPORT Errors 
; IMPORT LbeStd 
; IMPORT LexTable 
; IMPORT Messages  
; IMPORT Misc 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT Search 
; IMPORT SyncQueue  
; IMPORT Ui 
; IMPORT UiDevel 
; IMPORT UiSearch 
; IMPORT VersionedFiles 

; CONST DL = Messages . StdErrLine 

; VAR GCurrentCommand : TEXT 

; PROCEDURE InnerBeginCommand ( CommandString : TEXT  ) 

  = BEGIN 
      IF CommandString = NIL 
      THEN CommandString := "<NIL>"
      END (* IF *) 
    ; GCurrentCommand := CommandString 
    END InnerBeginCommand  

(* VISIBLE: *) 
; PROCEDURE BeginCommand ( Command : CommandTyp ) : TEXT 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText ( CommandTable , ORD ( Command ) ) 
    ; InnerBeginCommand ( LCommandString ) 
    ; RETURN LCommandString 
    END BeginCommand 

(* VISIBLE: *) 
; PROCEDURE BeginCommandPlusString 
    ( Command : CommandTyp ; Param : TEXT ) : TEXT 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " \"" 
           & Misc . EscapeText ( Param ) 
           & "\""
    ; InnerBeginCommand ( LCommandString ) 
    ; RETURN LCommandString 
    END BeginCommandPlusString  

(* VISIBLE: *) 
; PROCEDURE BeginCommandPlusChar 
    ( Command : CommandTyp ; Param : CHAR ) : TEXT 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " \'" 
           & Misc . EscapeText ( Text . FromChar ( Param ) ) 
           & "\'"
    ; InnerBeginCommand ( LCommandString ) 
    ; RETURN LCommandString 
    END BeginCommandPlusChar 

(* VISIBLE: *) 
; PROCEDURE BeginCommandPlusStringInt 
    ( Command : CommandTyp ; Param1 : TEXT ; Param2 : INTEGER ) : TEXT 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " \"" 
           & Misc . EscapeText ( Param1 ) 
           & "\" "
           & Fmt . Int ( Param2 )  
    ; InnerBeginCommand ( LCommandString ) 
    ; RETURN LCommandString 
    END BeginCommandPlusStringInt  

(* VISIBLE: *) 
; PROCEDURE BeginCommandPlusStringBool2 
    ( Command : CommandTyp 
    ; Param1 : TEXT 
    ; Param2 : BOOLEAN 
    ; Param3 : BOOLEAN 
    ) 
  : TEXT 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " \"" 
           & Misc . EscapeText ( Param1 ) 
           & "\" "
           & Fmt . Bool ( Param2 )  
           & " " 
           & Fmt . Bool ( Param3 )  
    ; InnerBeginCommand ( LCommandString ) 
    ; RETURN LCommandString 
    END BeginCommandPlusStringBool2 

(* VISIBLE: *) 
; PROCEDURE BeginCommandPlusString2BoolInt 
    ( Command : CommandTyp 
    ; Param1 : TEXT 
    ; Param2 : TEXT 
    ; Param3 : BOOLEAN 
    ; Param4 : INTEGER  
    ) 
  : TEXT 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " \"" 
           & Misc . EscapeText ( Param1 ) 
           & "\" \"" 
           & Misc . EscapeText ( Param2 ) 
           & "\" "
           & Fmt . Bool ( Param3 )  
           & " " 
           & Fmt . Int ( Param4 )  
    ; InnerBeginCommand ( LCommandString ) 
    ; RETURN LCommandString 
    END BeginCommandPlusString2BoolInt 

(* VISIBLE: *) 
; PROCEDURE BeginCommandPlusInt 
    ( Command : CommandTyp ; Param : INTEGER ) : TEXT 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " " 
           & Fmt . Int ( Param )  
    ; InnerBeginCommand ( LCommandString ) 
    ; RETURN LCommandString 
    END BeginCommandPlusInt  

(* VISIBLE: *) 
; PROCEDURE BeginCommandPlusInt2 
    ( Command : CommandTyp ; Param1 : INTEGER ; Param2 : INTEGER ) : TEXT 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " " 
           & Fmt . Int ( Param1 )  
           & " " 
           & Fmt . Int ( Param2 )  
    ; InnerBeginCommand ( LCommandString ) 
    ; RETURN LCommandString 
    END BeginCommandPlusInt2  

(* VISIBLE: *) 
; PROCEDURE BeginCommandPlusInt3 
    ( Command : CommandTyp 
    ; Param1 : INTEGER 
    ; Param2 : INTEGER 
    ; Param3 : INTEGER 
    ) 
  : TEXT 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " " 
           & Fmt . Int ( Param1 )  
           & " " 
           & Fmt . Int ( Param2 )  
           & " " 
           & Fmt . Int ( Param3 )  
    ; InnerBeginCommand ( LCommandString ) 
    ; RETURN LCommandString 
    END BeginCommandPlusInt3  

(* VISIBLE: *) 
; PROCEDURE CurrentCommand ( ) : TEXT 

  = BEGIN 
      RETURN GCurrentCommand 
    END CurrentCommand 

(* VISIBLE: *) 
; PROCEDURE RecordString ( CommandString : TEXT  ) 

  = VAR LWindow : PaintHs . WindowRefTyp 
  ; VAR LImageRef : PaintHs . ImageTransientTyp 

  ; <* FATAL FormsVBT . Error *> 
    BEGIN 
      IF CommandString = NIL 
      THEN CommandString := "<UnknownCommand>"
      END (* IF *) 
    ; IF RecordClosure # NIL
         AND RecordClosure . IsEnabled  
         AND RecordClosure . SyncQueueT # NIL 
      THEN 
        TRY 
          SyncQueue . Put 
            ( RecordClosure . SyncQueueT , CommandString & Wr . EOL ) 
        EXCEPT Thread . Alerted => (* Ignore *) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    ; LWindow := FormsVBT . GetGeneric ( Options . MainForm , "Fv_LbeWindow" ) 
    ; IF LWindow # NIL 
      THEN 
        LImageRef := LWindow . WrImageRef 
      ; IF LImageRef # NIL 
        THEN 
          LImageRef . ItPers . IpLastCommand := CommandString 
        ; IF LImageRef . ItHistoryWrT # NIL 
             AND NOT Wr . Closed ( LImageRef . ItHistoryWrT ) 
          THEN 
            TRY 
              Wr . PutText 
                ( LImageRef . ItHistoryWrT , CommandString & Wr . EOL ) 
            ; Wr . Flush ( LImageRef . ItHistoryWrT ) 
            EXCEPT Wr . Failure => (* Ignore *) 
            | Thread . Alerted => (* Ignore *) 
            END (* TRY EXCEPT *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END RecordString 

(* VISIBLE: *) 
; PROCEDURE Record ( Command : CommandTyp ) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText ( CommandTable , ORD ( Command ) ) 
    ; RecordString ( LCommandString ) 
    END Record 

(* VISIBLE: *) 
; PROCEDURE RecordPlusString ( Command : CommandTyp ; Param : TEXT ) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " \"" 
           & Misc . EscapeText ( Param ) 
           & "\""
    ; RecordString ( LCommandString ) 
    END RecordPlusString  

(* VISIBLE: *) 
; PROCEDURE RecordPlusChar ( Command : CommandTyp ; Param : CHAR ) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " \'" 
           & Misc . EscapeText ( Text . FromChar ( Param ) ) 
           & "\'"
    ; RecordString ( LCommandString ) 
    END RecordPlusChar 

(* VISIBLE: *) 
; PROCEDURE RecordPlusStringInt 
    ( Command : CommandTyp ; Param1 : TEXT ; Param2 : INTEGER ) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " \"" 
           & Misc . EscapeText ( Param1 ) 
           & "\" "
           & Fmt . Int ( Param2 )  
    ; RecordString ( LCommandString ) 
    END RecordPlusStringInt  

(* VISIBLE: *) 
; PROCEDURE RecordPlusStringBool2 
    ( Command : CommandTyp 
    ; Param1 : TEXT 
    ; Param2 : BOOLEAN 
    ; Param3 : BOOLEAN 
    ) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " \"" 
           & Misc . EscapeText ( Param1 ) 
           & "\" "
           & Fmt . Bool ( Param2 )  
           & " " 
           & Fmt . Bool ( Param3 )  
    ; RecordString ( LCommandString ) 
    END RecordPlusStringBool2 

(* VISIBLE: *) 
; PROCEDURE RecordPlusString2BoolInt 
    ( Command : CommandTyp 
    ; Param1 : TEXT 
    ; Param2 : TEXT 
    ; Param3 : BOOLEAN 
    ; Param4 : INTEGER  
    ) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " \"" 
           & Misc . EscapeText ( Param1 ) 
           & "\" \"" 
           & Misc . EscapeText ( Param2 ) 
           & "\" "
           & Fmt . Bool ( Param3 )  
           & " " 
           & Fmt . Int ( Param4 )  
    ; RecordString ( LCommandString ) 
    END RecordPlusString2BoolInt 

(* VISIBLE: *) 
; PROCEDURE RecordPlusInt ( Command : CommandTyp ; Param : INTEGER ) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " " 
           & Fmt . Int ( Param )  
    ; RecordString ( LCommandString ) 
    END RecordPlusInt  

(* VISIBLE: *) 
; PROCEDURE RecordPlusInt2 
    ( Command : CommandTyp ; Param1 : INTEGER ; Param2 : INTEGER ) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " " 
           & Fmt . Int ( Param1 )  
           & " " 
           & Fmt . Int ( Param2 )  
    ; RecordString ( LCommandString ) 
    END RecordPlusInt2  

(* VISIBLE: *) 
; PROCEDURE RecordPlusInt3 
    ( Command : CommandTyp 
    ; Param1 : INTEGER 
    ; Param2 : INTEGER 
    ; Param3 : INTEGER 
    ) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString  
        := LexTable . ToText 
             ( CommandTable , ORD ( Command ) ) 
           & " " 
           & Fmt . Int ( Param1 )  
           & " " 
           & Fmt . Int ( Param2 )  
           & " " 
           & Fmt . Int ( Param3 )  
    ; RecordString ( LCommandString ) 
    END RecordPlusInt3  

; PROCEDURE SetActive ( READONLY VBTs : ARRAY OF TEXT ) 
   
  = <* FATAL FormsVBT . Error *> 
    BEGIN 
      FOR RI := FIRST ( VBTs ) TO LAST ( VBTs ) 
      DO 
        FormsVBT . MakeActive ( Options . RecPlayForm , VBTs [ RI ] ) 
      END (* FOR *) 
    END SetActive 

; PROCEDURE SetDormant ( READONLY VBTs : ARRAY OF TEXT ) 
   
  = <* FATAL FormsVBT . Error *> 
    BEGIN 
      FOR RI := FIRST ( VBTs ) TO LAST ( VBTs ) 
      DO 
        FormsVBT . MakeDormant ( Options . RecPlayForm , VBTs [ RI ] ) 
      END (* FOR *) 
    END SetDormant 

(* Used for all open dialogs: *) 

; TYPE OpenOKClosureTyp 
    = FormsVBT . Closure 
        OBJECT 
          FileName : TEXT := NIL 
        END (* OBJECT *) 

; PROCEDURE OpenDialog  
    ( Form : FormsVBT . T 
    ; <*UNUSED*> DialogName : TEXT 
    ; PreloadFileName : TEXT
    ; Handler : OpenOKClosureTyp  
    ) 

  = <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
   (* FormsVBT . PutText
        ( Form 
        , "Rc_OpenDialog" 
        , DialogName  
        )
    ; *) 
(* FIX: ^ *) 
      FormsVBT . PutText 
        ( Form 
        , "Rc_OpenDialog_FileName" 
        , PreloadFileName 
        )
    ; FormsVBT . Attach 
        ( Form , "Rc_OpenDialog_OK" , Handler ) 
    ; FormsVBT . PopUp ( Form , "Rc_OpenDialog" ) 
    END OpenDialog  

; EXCEPTION OpenFailure 

(* Recording: *) 

; CONST DefaultRecordFileName = "test.play"

; CONST RecordOpenActive 
    = ARRAY OF TEXT 
         { "Rc_Record_Close" 
         , "Rc_Record_Enable" 
         , "Rc_Record_Reopen" 
         } 

; CONST RecordOpenDormant 
    = ARRAY OF TEXT 
         { "Rc_Record_Open" 
         , "Rc_Record_Append" 
         , "Rc_Record_Disable" 
         } 

; CONST RecordClosedActive 
    = ARRAY OF TEXT 
         { "Rc_Record_Open" 
         , "Rc_Record_Append" 
         } 

; CONST RecordClosedDormant  
    = ARRAY OF TEXT 
         { "Rc_Record_Close" 
         , "Rc_Record_Enable" 
         , "Rc_Record_Disable" 
         , "Rc_Record_Reopen" 
         } 

; CONST RecordEnabledActive 
    = ARRAY OF TEXT 
         { "Rc_Record_Disable" 
         , "Rc_Record_InsertStop"
         } 

; CONST RecordEnabledDormant 
    = ARRAY OF TEXT 
         { "Rc_Record_Enable" } 

; CONST RecordDisabledActive = RecordEnabledDormant 

; CONST RecordDisabledDormant = RecordEnabledActive  

; VAR RecordClosedFileString : TEXT := NIL 

; TYPE RecordStateTyp = { Disabled , Enabled , Draining } 

; TYPE RecordSyncTyp = MUTEX OBJECT 
    (* Including stuff that the Mutex protects. *) 
      State : RecordStateTyp  
    ; WaitingEnable : Thread . Condition := NIL 
    ; WaitingDisable : Thread . Condition := NIL 
    END 

; TYPE RecordClosureTyp = Thread . Closure OBJECT
      Sync : RecordSyncTyp := NIL 
    ; ThreadT : Thread . T := NIL 
    ; SyncQueueT : SyncQueue . T := NIL
    ; IsEnabled : BOOLEAN := FALSE 
      (* ^This is an unsynchronized cache of Sync . State = Enabled. *)  
    ; IsOpen : BOOLEAN := FALSE 
    OVERRIDES 
      apply := RecordThread 
    END 

; VAR RecordClosure : RecordClosureTyp 

; PROCEDURE RecordOpenSuccess 
    ( Form : FormsVBT . T ; FileName : TEXT ; WrT : Wr . T ; Enabled : BOOLEAN ) 

  = VAR LTypescriptVBT : TypescriptVBT . T 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN
Enabled := FALSE ;  
(* FIX: ^ *) 
      RecordClosure . IsEnabled := Enabled  
    ; RecordClosure . IsOpen := TRUE   
    ; LOCK RecordClosure . Sync 
      DO 
        IF Enabled 
        THEN 
          RecordClosure . Sync . State := RecordStateTyp . Enabled 
        ELSE 
          RecordClosure . Sync . State := RecordStateTyp . Disabled 
        END (* IF *) 
      END (* LOCK *) 
    ; Options . RecordFileName := FileName   
    ; Options . RecordWrT := WrT 
    ; LTypescriptVBT := FormsVBT . GetVBT ( Form , "Rc_Record_Log" ) 
    ; TypescriptVBT . ClearHistory ( LTypescriptVBT ) 
    ; SetDormant ( RecordOpenDormant ) 
    ; SetActive ( RecordOpenActive ) 
    ; FormsVBT . PutText ( Form , "Rc_Record_FileName" , FileName )
    END RecordOpenSuccess 

(* VISIBLE: *) 
; PROCEDURE RecordOpen 
     ( RecPlayForm : FormsVBT . T ; FileName : TEXT ; Enabled : BOOLEAN ) 
  : BOOLEAN (* Successful. *) 

  = VAR LWrT : Wr . T 
  ; VAR LSuccess : BOOLEAN 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
      TRY 
        IF FileName = NIL 
        THEN FileName := "" 
        END (* IF *) 
      ; IF Text . Equal ( FileName , "" )  
        THEN 
          LSuccess := FALSE 
        ; RAISE OpenFailure 
        ELSE 
          LWrT := VersionedFiles . OpenWrite ( FileName ) 
        ; LSuccess := TRUE 
        END 
      EXCEPT 
      VersionedFiles . Error ( EMessage ) 
      => FormsVBT . PutText 
           ( RecPlayForm 
           , "Rc_ErrorPopup_Message" 
           , "Couldn't open file \"" & FileName & "\": " & EMessage 
           )
      ; FormsVBT . PopUp ( RecPlayForm , "Rc_ErrorPopup" ) 
      ; LSuccess := FALSE
      ELSE 
        FormsVBT . PutText 
           ( RecPlayForm 
           , "Rc_ErrorPopup_Message" 
           , "Couldn't open file \"" & FileName & "\""
           )
      ; FormsVBT . PopUp ( RecPlayForm , "Rc_ErrorPopup" ) 
      ; LSuccess := FALSE  
      END (* TRY EXCEPT *) 
    ; IF LSuccess 
      THEN 
        RecordOpenSuccess ( RecPlayForm , FileName , LWrT , Enabled ) 
   (* ELSE leave the dialog up and let the user either change
           something and try again, or explicitly cancel. *)  
      END (* IF *) 
    ; RETURN LSuccess 
    END RecordOpen 

; PROCEDURE RecordOpenOK
    ( <* UNUSED *> Closure : OpenOKClosureTyp   
    ; Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
      LFileName := FormsVBT . GetText ( Form , "Rc_OpenDialog_FileName" )
    ; IF RecordOpen ( Form , LFileName , Enabled := FALSE ) 
      THEN 
        FormsVBT . PopDown ( Form , "Rc_OpenDialog" )
      ; FormsVBT . Attach ( Form , "Rc_OpenDialog_OK" , NIL ) 
      (* Otherwise, leave the dialog up and let the user either change
         something and try again, or explicitly cancel. *)  
      END (* IF *) 
    END RecordOpenOK

; VAR RecordOpenOKHandler : OpenOKClosureTyp  

; PROCEDURE RecordOpenDialog 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      OpenDialog 
        ( Form 
        , "Open Record File to (Re)write."
        , Options . RecordFileName   
        , RecordOpenOKHandler
        ) 
    END RecordOpenDialog 

; PROCEDURE RecordAppendOK
    ( <* UNUSED *> Closure : OpenOKClosureTyp   
    ; Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 
  ; LWrT : Wr . T 
  ; LSuccess : BOOLEAN 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
      LFileName 
        := FormsVBT . GetText ( Form , "Rc_OpenDialog_FileName" )
    ; TRY 
        IF LFileName = NIL 
        THEN LFileName := "" 
        END (* IF *) 
      ; IF Text . Equal ( LFileName , "" )  
        THEN 
          LSuccess := FALSE 
        ; RAISE OpenFailure 
        ELSE 
          LWrT := FileWr . OpenAppend ( LFileName ) 
        ; LSuccess := TRUE 
        END 
      EXCEPT 
      ELSE 
        FormsVBT . PutText 
           ( Form 
           , "Rc_ErrorPopup_Message" 
           , "Couldn't open file \"" & LFileName & "\""
           )
      ; FormsVBT . PopUp ( Form , "Rc_ErrorPopup" ) 
      ; LSuccess := FALSE  
      END (* TRY EXCEPT *) 
    ; IF LSuccess 
      THEN 
        RecordOpenSuccess ( Form , LFileName , LWrT , Enabled := FALSE ) 
      ; FormsVBT . PopDown ( Form , "Rc_OpenDialog" )
      ; FormsVBT . Attach ( Form , "Rc_OpenDialog_OK" , NIL ) 
   (* ELSE leave the dialog up and let the user either change
           something and try again, or explicitly cancel. *)  
      END (* IF *) 
    END RecordAppendOK

; VAR RecordAppendOKHandler : OpenOKClosureTyp  

; PROCEDURE RecordAppendDialog 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      OpenDialog 
        ( Form 
        , "Open Record File to Append to."
        , Options . RecordFileName  
        , RecordAppendOKHandler
        ) 
    END RecordAppendDialog 

; PROCEDURE RecordDisable ( Closure : RecordClosureTyp )
  (* NOOP if already disabled. *) 

  = BEGIN 
      RecordClosure . IsEnabled := FALSE 
    ; TRY 
        SyncQueue . Close ( Closure . SyncQueueT , WaitForEmpty := TRUE ) 
      ; LOCK Closure . Sync 
        DO 
          IF Closure . Sync . State # RecordStateTyp . Disabled 
          THEN 
            Closure . Sync . State := RecordStateTyp . Draining 
          ; Thread . AlertWait 
              ( Closure . Sync , Closure . Sync . WaitingDisable ) 
          END (* IF *) 
        END (* LOCK *) 
      EXCEPT Thread . Alerted => (* Ignore. *) 
      END (* TRY EXCEPT *) 
    ; SetDormant ( RecordDisabledDormant ) 
    ; SetActive ( RecordDisabledActive ) 
    END RecordDisable  

; PROCEDURE RecordReopenProc  
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LTypescriptVBT : TypescriptVBT . T 

  ; <* FATAL FormsVBT . Error *> 
    BEGIN 
      IF RecordClosure . IsOpen 
      THEN 
        RecordDisable ( RecordClosure )  
      ; TRY 
          Wr . Close ( Options . RecordWrT )  
        EXCEPT Thread . Alerted => (* Ignore. *) 
        | Wr . Failure => (* What can we do?. *) 
        END (* TRY EXCEPT *) 
      ; TRY 
          Options . RecordWrT := FileWr . Open ( Options . RecordFileName ) 
        (* ^Hope this doesn't fail. *) 
        ; LTypescriptVBT := FormsVBT . GetVBT ( Form , "Rc_Record_Log" ) 
        ; TypescriptVBT . ClearHistory ( LTypescriptVBT ) 
        ; SetDormant ( RecordOpenDormant ) 
        ; SetActive ( RecordOpenActive ) 
        EXCEPT OSError . E 
        => (* Give up. *) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END RecordReopenProc  

(* VISIBLE: *) 
; PROCEDURE RecordClose ( )
  RAISES { Thread . Alerted }  

  = BEGIN 
      IF RecordClosure # NIL AND RecordClosure . IsOpen 
      THEN 
        RecordDisable ( RecordClosure )  
      ; TRY 
          Wr . Close ( Options . RecordWrT ) 
        EXCEPT Wr . Failure => (* Ignore this. *) 
        END (* TRY EXCEPT *) 
      ; RecordClosure . IsOpen := FALSE 
      END (* IF *) 
    END RecordClose  

; PROCEDURE RecordCloseProc   
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN
      TRY 
        RecordClose ( ) 
      EXCEPT Thread . Alerted => (* Ignore. *) 
      END (* TRY EXCEPT *) 
    ; FormsVBT . PutText 
        ( Form 
        , "Rc_Record_FileName" 
        , RecordClosedFileString 
        ) 
    ; SetDormant ( RecordClosedDormant ) 
    ; SetActive ( RecordClosedActive ) 
    ; IF NOT PlaybackClosure . IsOpen 
      THEN Options . PlaybackFileName := Options . RecordFileName 
      END (* IF *) 
    END RecordCloseProc 

; VAR GRecordLogWrT : Wr . T 
(* TODO: ^Put this inside RecordClosure. *) 

; PROCEDURE RecordThread ( Self : RecordClosureTyp ) : REFANY 

  = VAR LStateBeforeCheck : RecordStateTyp 
  ; VAR LSelfDisable : BOOLEAN := FALSE 
  ; VAR LLine : TEXT 

  ; BEGIN
      LSelfDisable := FALSE 
    ; LOOP (* Forever, unless Alerted. *) 
        LOCK RecordClosure . Sync 
        DO 
          IF LSelfDisable 
          THEN 
            Self . IsEnabled := FALSE 
          ; Self . Sync . State := RecordStateTyp . Disabled 
          END (* IF *) 
        (* Wait while disabled. *)
        ; LStateBeforeCheck := Self . Sync . State  
        ; WHILE Self . Sync . State = RecordStateTyp . Disabled 
          DO (* Disabling of this thread is final. *) 
            Thread . Signal ( Self . Sync . WaitingDisable ) 
          ; Thread . Wait ( Self . Sync , Self . Sync . WaitingEnable ) 
          END (* WHILE *) 
        END (* LOCK *) 
      ; IF LStateBeforeCheck = RecordStateTyp . Disabled 
        THEN 
          SetDormant ( RecordEnabledDormant ) 
        ; SetActive ( RecordEnabledActive ) 
        END (* IF *) 
      ; LSelfDisable := FALSE 
      ; TRY 
          LLine := SyncQueue . Get ( Self . SyncQueueT ) 
        ; TRY 
            Wr . PutText ( Options . RecordWrT , LLine ) 
          ; Wr . Flush ( Options . RecordWrT ) 
          EXCEPT
            Wr . Failure 
          => LSelfDisable := TRUE 
          | Thread . Alerted 
          => EXIT 
          END (* TRY EXCEPT *) 
        ; Wr . PutText ( GRecordLogWrT , LLine ) 
        ; Wr . Flush ( GRecordLogWrT ) 
        EXCEPT 
          SyncQueue . NotInitialized   
        => LSelfDisable := TRUE  
        | Wr . Failure => (* Ignore *) 
(* TODO: Pop a dialog? *) 
        | Thread . Alerted 
        => EXIT 
        END (* TRY EXCEPT *) 
      END (* LOOP Forever *) 
    ; RETURN NIL 
    END RecordThread 

(* VISIBLE: *) 
; PROCEDURE RecordEnable ( )   

  = BEGIN 
      IF Options . RecordWrT # NIL 
         AND NOT Wr . Closed ( Options . RecordWrT ) 
      THEN 
        LOCK RecordClosure . Sync 
        DO 
          IF RecordClosure . Sync . State # RecordStateTyp . Enabled
          THEN 
            RecordClosure . Sync . State := RecordStateTyp . Enabled 
          ; Thread . Signal ( RecordClosure . Sync . WaitingEnable )  
          END (* IF *) 
        END (* LOCK *) 
      ; TRY 
          EVAL SyncQueue . Init   
                 ( RecordClosure . SyncQueueT , MaxContents := 1 ) 
        ; RecordClosure . IsEnabled := TRUE  
        EXCEPT Thread . Alerted 
        => RecordDisable ( RecordClosure ) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END RecordEnable 

; PROCEDURE RecordEnableProc   
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      RecordEnable ( ) 
    END RecordEnableProc 

; PROCEDURE RecordDisableProc   
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN
      RecordDisable ( RecordClosure )  
    END RecordDisableProc 

; PROCEDURE RecordInsertStopProc   
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN
      Record ( CommandTyp . STOP ) 
    END RecordInsertStopProc 

(* Playback: *) 

; CONST DefaultPlaybackFileName = "test.play"

; CONST PlaybackOpenActive 
    = ARRAY OF TEXT 
         { "Rc_Playback_Close" 
         , "Rc_Playback_Reopen" 
         , "Rc_Playback_Run" 
         , "Rc_Playback_Skip" 
         , "Rc_Playback_Step" 
         } 

; CONST PlaybackOpenDormant  
    = ARRAY OF TEXT 
         { "Rc_Playback_Open" 
         , "Rc_Playback_Stop" 
         } 

; CONST PlaybackClosedActive 
    = ARRAY OF TEXT 
         { "Rc_Playback_Open" } 

; CONST PlaybackClosedDormant = PlaybackOpenActive  

; CONST PlaybackRunningActive 
    = ARRAY OF TEXT 
         { "Rc_Playback_Stop" } 

; CONST PlaybackRunningDormant  
    = ARRAY OF TEXT 
         { "Rc_Playback_Run" 
         , "Rc_Playback_Skip" 
         , "Rc_Playback_Step" 
         } 

; CONST PlaybackStoppedActive = PlaybackRunningDormant  

; CONST PlaybackStoppedDormant = PlaybackRunningActive   

; VAR PlaybackClosedFileString : TEXT := NIL 

; TYPE PlaybackStateTyp 
    = { Stopped , Stepping , Skipping , Running } 

; TYPE PlaybackSyncTyp = MUTEX OBJECT 
    (* Including stuff that the Mutex protects. *) 
      Delay : LONGREAL := 0.0D0 
    ; State : PlaybackStateTyp := PlaybackStateTyp . Stopped   
    ; WaitingRun : Thread . Condition := NIL
    ; WaitingStop : Thread . Condition := NIL 
    ; RespectStops : BOOLEAN := TRUE 
    END 

; TYPE PlaybackClosureTyp = Thread . Closure OBJECT
      Sync : PlaybackSyncTyp := NIL
    ; ThreadT : Thread . T := NIL 
    ; Window : EditWindow . T := NIL 
    ; TypescriptVBT : TypescriptVBT . T := NIL 
    ; LogWrT : Wr . T 
    ; NextLine : TEXT := NIL 
    ; AtEOF : BOOLEAN := FALSE 
    ; IsOpen : BOOLEAN := FALSE   
    OVERRIDES 
      apply := PlaybackThread 
    END 

; CONST CommentStarters = SET OF CHAR { '%' , '#' , ';' }  

; VAR PlaybackClosure : PlaybackClosureTyp 

; PROCEDURE ReadPlaybackLine ( Closure : PlaybackClosureTyp ) 
  RAISES { Thread . Alerted } 

  = VAR LLength : INTEGER 
  ; VAR LI : INTEGER 

  ; BEGIN 
    (* NOTE: This skips (but echos) blank lines and comment-only lines.
             The result is that stepping will not step through these, but
             only real commands.  Code for executing commands is redundantly 
             defended against blank and comment-only lines, should you
             ever wish to remove the skipping here. *) 
      LOOP (* Through blank lines and all-comment lines, stopping with the
              first line with a command, or EOF. *) 
        TRY 
          Closure . NextLine := Rd . GetLine ( Options . PlaybackRdT )
(* CHECK:  Can Options . PlaybackRdT go in the closure? *) 
	EXCEPT 
	  Rd . EndOfFile 
	, Rd . Failure 
	=> Closure . AtEOF := TRUE 
	; TRY 
            Wr . PutText ( Closure . LogWrT , "<END OF FILE>" ) 
          ; Wr . PutText ( Closure . LogWrT , Wr . EOL ) 
          ; Wr . Flush ( Closure . LogWrT ) 
          EXCEPT Wr . Failure => (* Ignore log failure *) 
          END (* TRY EXCEPT *) 
        ; EXIT 
	END (* TRY EXCEPT *) 
      ; IF Closure . NextLine # NIL 
        THEN 
          TRY 
            Wr . PutText ( Closure . LogWrT , Closure . NextLine ) 
          ; Wr . PutText ( Closure . LogWrT , Wr . EOL ) 
          ; Wr . Flush ( Closure . LogWrT ) 
          EXCEPT Wr . Failure => (* Ignore log failure *) 
          END (* TRY EXCEPT *) 
        ; LLength := Text . Length ( Closure . NextLine ) 
        ; LI := 0 
        ; WHILE LI < LLength 
                AND Text . GetChar ( Closure . NextLine , LI ) 
                    IN Lex . Blanks 
          DO INC ( LI ) 
          END (* WHILE *) 
        ; IF LI < LLength 
             AND NOT Text . GetChar ( Closure . NextLine , LI ) 
                     IN CommentStarters  
          THEN EXIT 
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    END ReadPlaybackLine 

(* VISIBLE: *) 
; PROCEDURE PlaybackOpen ( RecPlayForm : FormsVBT . T ; FileName : TEXT ) 
  : BOOLEAN (* Successful. *) 

  = VAR LRdT : Rd . T 
  ; VAR LSuccess : BOOLEAN 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
      TRY 
        IF FileName = NIL 
        THEN FileName := "" 
        END (* IF *) 
      ; IF Text . Equal ( FileName , "" )  
        THEN 
          LSuccess := FALSE 
        ; RAISE OpenFailure 
        ELSE 
          LRdT := FileRd . Open ( FileName ) 
        ; LSuccess := TRUE 
        END 
      EXCEPT 
      ELSE 
        FormsVBT . PutText 
          ( RecPlayForm 
          , "Rc_ErrorPopup_Message" 
          , "Couldn't open file \"" & FileName & "\""
          )
      ; FormsVBT . PopUp ( RecPlayForm , "Rc_ErrorPopup" ) 
      ; LSuccess := FALSE  
      END (* TRY EXCEPT *) 
    ; IF LSuccess 
      THEN 
        LOCK PlaybackClosure . Sync 
        DO 
          PlaybackClosure . Sync . State := PlaybackStateTyp . Stopped 
        END (* IF*) 
      ; Options . PlaybackFileName := FileName   
      ; Options . PlaybackRdT := LRdT 
      ; FormsVBT . PutText ( RecPlayForm , "Rc_Playback_FileName" , FileName )
      ; PlaybackClosure . TypescriptVBT 
          := FormsVBT . GetVBT ( RecPlayForm , "Rc_Playback_Log" ) 
      ; TypescriptVBT . ClearHistory ( PlaybackClosure . TypescriptVBT ) 
      ; PlaybackClosure . LogWrT 
          := TypescriptVBT . GetWr ( PlaybackClosure . TypescriptVBT ) 
      ; PlaybackClosure . AtEOF := FALSE 
      ; PlaybackClosure . IsOpen := TRUE  
      ; TRY 
          ReadPlaybackLine ( PlaybackClosure ) 
        EXCEPT Thread . Alerted => (* Ignore. *) 
        END (* TRY EXCEPT *) 
      ; SetDormant ( PlaybackOpenDormant ) 
      ; SetActive ( PlaybackOpenActive ) 
      ; FormsVBT . PutText ( RecPlayForm , "Rc_Playback_Message" , "Stopped" ) 
      END (* IF *) 
    ; RETURN LSuccess 
    END PlaybackOpen

; PROCEDURE PlaybackOpenOK
    ( <* UNUSED *> OpenOKClosure : OpenOKClosureTyp   
    ; Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
      LFileName := FormsVBT . GetText ( Form , "Rc_OpenDialog_FileName" )
    ; IF PlaybackOpen ( Form , LFileName ) 
      THEN 
        FormsVBT . PopDown ( Form , "Rc_OpenDialog" )
      ; FormsVBT . Attach ( Form , "Rc_OpenDialog_OK" , NIL ) 
      (* Otherwise, leave the dialog up and let the user either change
         something and try again, or explicitly cancel. *)  
      END (* IF *) 
    END PlaybackOpenOK

; VAR PlaybackOpenOKHandler : OpenOKClosureTyp  

; PROCEDURE PlaybackOpenDialog 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 
(* TODO: Connect all callbacks.  What? *) 

  = BEGIN 
      OpenDialog 
        ( Form 
        , "Open Playback File."
        , Options . PlaybackFileName  
        , PlaybackOpenOKHandler
        ) 
    END PlaybackOpenDialog 

(* VISIBLE: *) 
; PROCEDURE ExecuteOneAction 
    ( CommandLine : TEXT 
    ; Window : PaintHs . WindowRefTyp 
    ; RespectStops : BOOLEAN 
    ; VAR DoSelfStop : BOOLEAN 
    ) 
  RAISES { Thread . Alerted } 

  = VAR EoaTextRd : Rd . T 

  ; PROCEDURE EoaCharParam ( ) : CHAR  
    RAISES { Thread . Alerted } 

    = VAR LQuoted : TEXT 
    ; VAR LUnquoted : CHAR 

    ; BEGIN
        TRY 
          Lex . Skip ( EoaTextRd )  
        ; LQuoted := Rd . GetLine ( EoaTextRd ) 
        ; IF Text . GetChar ( LQuoted , 0 ) 
             IN CommentStarters 
          THEN RETURN VAL ( 0 , CHAR ) 
          ELSE 
            LUnquoted := Misc . UnquoteChar ( LQuoted ) 
          ; RETURN LUnquoted  
          END (* IF *) 
        EXCEPT 
        | Rd . Failure 
        , Rd . EndOfFile 
        => RETURN FIRST ( CHAR ) 
        END (* TRY EXCEPT *) 
      END EoaCharParam 

  ; PROCEDURE EoaTextParam ( ) : TEXT 

    = BEGIN
        RETURN Misc . ReadAndUnquoteText ( EoaTextRd ) 
      END EoaTextParam 

  ; PROCEDURE EoaIntParam ( ) : INTEGER  
    RAISES { Thread . Alerted } 

    = VAR LResult : INTEGER 

    ; BEGIN
        TRY 
          Lex . Skip ( EoaTextRd )  
        ; LResult := Lex . Int ( EoaTextRd ) 
        ; RETURN LResult   
        EXCEPT 
        | Lex . Error 
        , Rd . Failure 
        , FloatMode . Trap 
        => RETURN FIRST ( INTEGER ) 
        END (* TRY EXCEPT *) 
      END EoaIntParam 

  ; PROCEDURE EoaBoolParam ( ) : BOOLEAN   
    RAISES { Thread . Alerted } 

    = VAR LResult : BOOLEAN 

    ; BEGIN
        TRY 
          Lex . Skip ( EoaTextRd )  
        ; LResult := Lex . Bool ( EoaTextRd ) 
        ; RETURN LResult   
        EXCEPT 
        | Lex . Error 
        , Rd . Failure 
        => RETURN FALSE 
        END (* TRY EXCEPT *) 
      END EoaBoolParam 

  ;  PROCEDURE BoaShowStopped ( Message : TEXT ) 

     = <* FATAL FormsVBT . Error *> 
       <* FATAL FormsVBT . Unimplemented *> 
       BEGIN 
         DoSelfStop := TRUE  
       ; SetDormant ( PlaybackStoppedDormant ) 
       ; SetActive ( PlaybackStoppedActive ) 
       ; FormsVBT . PutText 
           ( Options . RecPlayForm 
           , "Rc_Playback_Message" 
           , Message 
           ) 
       END BoaShowStopped

  ; BEGIN (* ExecuteOneAction *) 
      VAR LCommandString : TEXT 
    ; VAR LCommandInt : LexTable . ValueTyp  
    ; VAR LCommand : CommandTyp 
    ; VAR LCh : CHAR 

    ; <* FATAL Rd . Failure *> 
      BEGIN (* Block ExecuteOneAction *) 
        IF CommandLine # NIL 
        THEN 
          EoaTextRd := NEW ( TextRd . T ) . init ( CommandLine ) 
        ; Lex . Skip ( EoaTextRd )  
        ; IF NOT Rd . EOF ( EoaTextRd ) 
          THEN 
            LCommandString := Lex . Scan ( EoaTextRd )  
          ; IF NOT Text . GetChar ( LCommandString , 0 ) 
               IN SET OF CHAR { '%' , '#' , ';' } 
            THEN 
              TRY 
                LCommandInt  
                  := LexTable . ValueFromText ( CommandTable , LCommandString ) 
              EXCEPT AssertionFailure 
              => LCommandInt := LexTable . ValueNull 
(* TODO: Pop a dialog or something here? *) 
              END (* TRY EXCEPT *) 
            ; IF LCommandInt = LexTable . ValueNull 
              THEN 
                BoaShowStopped ( "Stopped at invalid command" ) 
              ELSE 
                LCommand := VAL ( LCommandInt , CommandTyp ) 
(* TODO: Protect this from blowing up if bad LCommandInt *) 
              ; CASE LCommand 
                OF CommandTyp . STOP   
                => Record ( CommandTyp . STOP )  
                ; IF RespectStops 
                   THEN 
                     BoaShowStopped ( "Stopped at STOP command" ) 
                   END (* IF *) 

                | CommandTyp . FileOpen 
                => Ui . ReplayFileOpen ( EoaTextParam ( ) ) 

                | CommandTyp . FileSave 
                => Ui . ReplayFileSave ( EoaTextParam ( ) ) 

                | CommandTyp . FileSaveAs 
                => Ui . ReplayFileSaveAs ( EoaTextParam ( ) ) 

                | CommandTyp . FileQuit 
                => Ui . ReplayFileQuit ( ) 

                | CommandTyp . FileExport 
                => Ui . ReplayFileExport ( EoaTextParam ( ) ) 

                | CommandTyp . FileCloseImage 
                => Ui . ReplayFileCloseImage ( EoaTextParam ( ) ) 

                | CommandTyp . FileCloseWindow  
                => Ui . ReplayFileCloseWindow 
                     ( EoaTextParam ( ) , EoaIntParam ( ) ) 

                | CommandTyp . EditCut => Ui . ReplayEditCut ( ) 

                | CommandTyp . EditCopy => Ui . ReplayEditCopy ( ) 

                | CommandTyp . EditPaste 
                => Ui . ReplayEditPaste ( Window , EoaTextParam ( ) ) 

                | CommandTyp . SearchStringFwd  
                => UiSearch . ReplayStringSearchFwd 
                     ( EoaTextParam ( ) 
                     , EoaBoolParam ( ) 
                     , EoaBoolParam ( )  
                     ) 

                | CommandTyp . SearchStringBwd  
                => UiSearch . ReplayStringSearchBwd 
                     ( EoaTextParam ( ) 
                     , EoaBoolParam ( ) 
                     , EoaBoolParam ( )  
                     ) 

                | CommandTyp . ReplaceString
                => UiSearch . ReplayReplace 
                     ( EoaTextParam ( ) 
                     , EoaTextParam ( ) 
                     , EoaBoolParam ( ) 
                     , VAL ( EoaIntParam ( ) , Search . ReplaceKindTyp )   
                     ) 

                | CommandTyp . SemParse => Ui . ReplaySemParse ( ) 

                | CommandTyp . SemAccept => Ui . ReplaySemAccept ( ) 

                | CommandTyp . SemAnalyze => Ui . ReplaySemAnalyze ( ) 

                | CommandTyp . VertScroll  
                  => Ui . ReplayVertScroll 
                       ( EoaIntParam ( ) , EoaIntParam ( ) , EoaIntParam ( ) ) 
                | CommandTyp . HorizScroll  
                  => Ui . ReplayHorizScroll ( EoaIntParam ( ) ) 
                | CommandTyp . MouseClickPixel  
                  => EditWindow . ReplayMouseClickPixel  
                       ( Window 
                       , EoaIntParam ( ) 
                       , EoaIntParam ( ) 
                       ) 

                | CommandTyp . MouseClickChar  
                  => EditWindow . ReplayMouseClickChar  
                       ( Window 
                       , EoaIntParam ( ) 
                       , EoaIntParam ( ) 
                       ) 

                | CommandTyp . CharDelBwd 
                => EditWindow . ReplayCharDelBwd ( Window ) 

                | CommandTyp . CharDelFwd 
                => EditWindow . ReplayCharDelFwd ( Window ) 

                | CommandTyp . CursorLeft 
                => EditWindow . ReplayCursorLeft ( Window ) 

                | CommandTyp . CursorRight 
                => EditWindow . ReplayCursorRight ( Window ) 

                | CommandTyp . CursorUp 
                => EditWindow . ReplayCursorUp ( Window ) 

                | CommandTyp . CursorDown 
                => EditWindow . ReplayCursorDown ( Window ) 

                | CommandTyp . BeginKey
                => EditWindow . ReplayBeginKey ( Window )  

                | CommandTyp . EndKey 
                => EditWindow . ReplayEndKey ( Window ) 

                | CommandTyp . PriorKey 
                => EditWindow . ReplayPriorKey ( Window ) 

                | CommandTyp . NextKey 
                => EditWindow . ReplayNextKey ( Window ) 

                | CommandTyp . HomeKey 
                => EditWindow . ReplayHomeKey ( Window ) 

                | CommandTyp . InsertModeOn  
                => EditWindow . ReplaySetInsertMode ( Window , TRUE ) 
           
                | CommandTyp . InsertModeOff  
                => EditWindow . ReplaySetInsertMode ( Window , FALSE ) 
           
                | CommandTyp . DeleteRestOfLine 
                => EditWindow . ReplayDeleteRestOfLine ( Window ) 

                | CommandTyp . CharType  
                => LCh := EoaCharParam ( )  
                ; CASE LCh  
                  OF LbeStd . CharFirstPrintable 
                     .. LbeStd . CharLastPrintable 
                    , LbeStd . CharNewLine 
                    , LbeStd . CharReturn 
                    , LbeStd . CharTab  
                  => EditWindow . ReplayCharType ( Window , LCh ) 
                  ELSE 
                    Display . Beep ( Errors . ErrorTyp . EBadChar ) 
                  END (* CASE *) 

                | CommandTyp . CharTranspose  
                => EditWindow . ReplayCharTranspose ( Window ) 

                | CommandTyp . ClearSelection  
                => EditWindow . ReplayClearSelection ( ) 

                | CommandTyp . SweepSelection 
                => EditWindow . ReplaySweepSelection 
                     ( Window 
                     , EoaIntParam ( ) 
                     , EoaIntParam ( ) 
                     ) 

                | CommandTyp . EndSweepSelection   
                => EditWindow . ReplayEndSweepSelection ( Window ) 

                | CommandTyp . WriteCheckpoint
                => UiDevel . ReplayWriteCheckpoint ( ) 

                | CommandTyp . SetDebugLevel 
                => UiDevel . ReplaySetDebugLevel ( EoaIntParam ( ) ) 

                | CommandTyp . TakeFocus 
                => UiDevel . ReplayTakeFocus ( ) 

                | CommandTyp . Repaint
                => UiDevel . ReplayRepaint ( ) 

                | CommandTyp . ReconstructLines
                => UiDevel . ReplayReconstructLines ( ) 

                | CommandTyp . VerifyLinesRefs
                => UiDevel . ReplayVerifyLinesRefs ( ) 

                | CommandTyp . ForceAssert
                => UiDevel . ReplayForceAssert ( ) 

                | CommandTyp . MergeText 
                => UiDevel . ReplayMergeText ( ) 

                | CommandTyp . BrowseEst 
                => UiDevel . ReplayBrowseEst ( ) 

                | CommandTyp . WriteStats 
                => UiDevel . ReplayWriteStats ( EoaTextParam ( ) ) 

                | CommandTyp . WriteEstPickle 
                => UiDevel . ReplayWriteEstPickle ( EoaTextParam ( ) )  

                | CommandTyp . GenEstModule 
                => UiDevel . ReplayGenEstModule ( EoaTextParam ( ) ) 

                | CommandTyp . WriteParseInfo 
                => UiDevel . ReplayWriteParseInfo ( EoaTextParam ( ) ) 

                | CommandTyp . WriteFsTrees 
                => UiDevel . ReplayWriteFsTrees ( EoaTextParam ( ) ) 

                | CommandTyp . WriteSemPickle 
                => UiDevel . ReplayWriteSemPickle ( EoaTextParam ( ) ) 

                | CommandTyp . GenTokInterface 
                => UiDevel . ReplayGenTokInterface ( EoaTextParam ( ) ) 

                | CommandTyp . GenChildInterface
                => UiDevel . ReplayGenChildInterface ( EoaTextParam ( ) ) 

                ELSE 
                  BoaShowStopped ( "Stopped at unimplemented command" ) 
                END (* CASE *) 
              END (* IF valid command code. *) 
            END (* IF NOT comment. *) 
          END (* IF NOT EOF. *) 
        END (* IF CommandLine # NIL *) 
      END (* Block ExecuteOneAction. *)  
    END ExecuteOneAction 

; PROCEDURE PlaybackOneAction 
    ( PlaybackClosure : PlaybackClosureTyp 
    ; RespectStops : BOOLEAN 
    ; DoSkip : BOOLEAN 
    ; VAR DoSelfStop : BOOLEAN 
    ) 

  = <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN (* PlaybackOneAction *) 
      DoSelfStop := FALSE 
    ; IF PlaybackClosure . AtEOF 
      THEN 
        DoSelfStop := TRUE 
      ; SetDormant ( PlaybackStoppedDormant ) 
      ; SetActive ( PlaybackStoppedActive ) 
      ; FormsVBT . PutText 
          ( Options . RecPlayForm 
          , "Rc_Playback_Message" 
          , "Stopped at end of file" 
          ) 
      ELSE 
        TRY 
          IF NOT DoSkip 
          THEN
            ExecuteOneAction 
              ( PlaybackClosure . NextLine 
              , PlaybackClosure . Window 
              , RespectStops  
              , (* VAR *) DoSelfStop 
              ) 
          END (* IF NOT DoSkip. *)  
        ; ReadPlaybackLine ( PlaybackClosure ) 
        EXCEPT Thread . Alerted 
        => DoSelfStop := TRUE 
      ; SetDormant ( PlaybackStoppedDormant ) 
      ; SetActive ( PlaybackStoppedActive ) 
      ; FormsVBT . PutText 
          ( Options . RecPlayForm 
          , "Rc_Playback_Message" 
          , "Playback cancelled" 
          ) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END PlaybackOneAction 

; PROCEDURE Delay 
    ( Self : PlaybackClosureTyp 
    ; Time : LONGREAL 
    ; VAR State : PlaybackStateTyp 
    ) 

  = VAR LOldDelay : LONGREAL 
  ; VAR LNewDelay : LONGREAL 
  ; VAR LWholeSeconds : INTEGER 
  ; VAR LMessage : TEXT 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
      IF Time < 0.05D0  
      THEN
        FormsVBT . PutText 
          ( Options . RecPlayForm 
          , "Rc_Playback_Message" 
          , "Running" 
          ) 
      ELSE 
        LOldDelay := Time 
      ; LWholeSeconds := FLOOR ( Time )   
      ; LMessage 
         := "Delaying " 
             & Fmt . LongReal 
                ( Time , style := Fmt . Style . Fix , prec := 1 )   
             & " seconds, "
      ; FormsVBT . PutText 
          ( Options . RecPlayForm 
          , "Rc_Playback_Message" 
          , LMessage 
            & Fmt . Int ( LWholeSeconds ) 
            & " remaining"
          ) 
      ; Thread . Pause 
          ( Time - FLOAT ( LWholeSeconds , LONGREAL ) )  
      ; LOOP 
          IF LWholeSeconds <= 0 
          THEN EXIT 
          ELSE  
            LOCK Self . Sync
            DO 
              State := Self . Sync . State 
            ; LNewDelay := Self . Sync . Delay 
            END (* LOCK *) 
          ; IF State # PlaybackStateTyp . Running 
            THEN (* User stopped during the delay. *) 
              EXIT 
            ELSE 
              INC ( LWholeSeconds , FLOOR ( LNewDelay - LOldDelay ) ) 
            ; LOldDelay := LNewDelay 
            ; IF LWholeSeconds <= 0 
              THEN (* The delay is done. *) 
                EXIT 
              ELSE 
                FormsVBT . PutText 
                  ( Options . RecPlayForm 
                  , "Rc_Playback_Message" 
                  , LMessage 
                    & Fmt . Int ( LWholeSeconds ) 
                    & " remaining"
                  ) 
              ; Thread . Pause ( 1.0D0 ) 
              ; DEC ( LWholeSeconds ) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      END (* IF *) 
    END Delay 

; PROCEDURE PlaybackThread ( Self : PlaybackClosureTyp ) : REFANY 

  = VAR LDelay : LONGREAL 
  ; VAR LSelfStop : BOOLEAN := FALSE 
  ; VAR LRespectStops : BOOLEAN := TRUE 
  ; VAR LState : PlaybackStateTyp 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
      LSelfStop := FALSE 
    ; LOOP 
        LOCK Self . Sync 
        DO 
          IF LSelfStop 
          THEN 
            Self . Sync . State := PlaybackStateTyp . Stopped  
          END (* IF *) 
        ; WHILE Self . Sync . State = PlaybackStateTyp . Stopped  
          DO 
            Thread . Signal ( Self . Sync . WaitingStop )  
          ; Thread . Wait ( Self . Sync , Self . Sync . WaitingRun ) 
          END (* WHILE *) 
        ; LState := Self . Sync . State 
        ; LDelay := Self . Sync . Delay 
        ; LRespectStops := Self . Sync . RespectStops 
        END (* LOCK *) 
      ; CASE LState <* NOWARN *>
        OF PlaybackStateTyp . Running 
        => FormsVBT . PutText 
             ( Options . RecPlayForm 
             , "Rc_Playback_Message" 
             , "Executing" 
             ) 
        ; PlaybackOneAction 
            ( Self 
            , LRespectStops 
            , DoSkip := FALSE 
            , (* VAR *) DoSelfStop := LSelfStop 
            ) 
        ; IF NOT LSelfStop 
          THEN 
            Delay ( Self , LDelay , (* VAR *) LState ) 
          END (* IF *) 
        | PlaybackStateTyp . Skipping 
        => PlaybackOneAction 
             ( Self 
             , LRespectStops 
             , DoSkip := TRUE 
             , (* VAR *) DoSelfStop := LSelfStop 
             )   
        ; FormsVBT . PutText 
            ( Options . RecPlayForm 
            , "Rc_Playback_Message" 
            , "Stopped after skip" 
            ) 
        ; LSelfStop := TRUE 
        | PlaybackStateTyp . Stepping 
        => PlaybackOneAction 
             ( Self 
             , LRespectStops 
             , DoSkip := FALSE 
             , (* VAR *) DoSelfStop := LSelfStop 
             )   
        ; FormsVBT . PutText 
            ( Options . RecPlayForm 
            , "Rc_Playback_Message" 
            , "Stopped after step" 
            ) 
        ; LSelfStop := TRUE 
        END (* CASE *) 
      END (* LOOP *) 
    END PlaybackThread 

; PROCEDURE Stop ( VAR WasRunning : BOOLEAN ) 

  = BEGIN 
      LOCK PlaybackClosure . Sync 
      DO 
        WasRunning 
          := PlaybackClosure . Sync . State # PlaybackStateTyp . Stopped 
      ; IF WasRunning   
        THEN 
          PlaybackClosure . Sync . State := PlaybackStateTyp . Stopped 
        ; Thread . Wait 
            ( PlaybackClosure . Sync , PlaybackClosure . Sync . WaitingStop ) 
        END (* IF *) 
      ; PlaybackClosure . Sync . RespectStops := TRUE 
      END (* LOCK *) 
    END Stop 

; PROCEDURE PlaybackReopenProc  
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LWasRunning : BOOLEAN 

  ; BEGIN 
      Stop ( (* VAR *) LWasRunning (* Dead *) ) 
    ; TRY 
        Rd . Seek ( Options . PlaybackRdT , 0 )  
      ; TypescriptVBT . ClearHistory ( PlaybackClosure . TypescriptVBT ) 
      ; PlaybackClosure . AtEOF := FALSE 
      ; ReadPlaybackLine ( PlaybackClosure ) 
      ; SetDormant ( PlaybackOpenDormant ) 
      ; SetActive ( PlaybackOpenActive ) 
      EXCEPT Rd . Failure 
      , Thread . Alerted 
      => (* Reopen failed. *) 
(* TODO: Pop a dialog. *) 
      END (* TRY EXCEPT *) 
    END PlaybackReopenProc  

; PROCEDURE PlaybackCloseProc  
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LWasRunning : BOOLEAN 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
      Stop ( (* VAR *) LWasRunning (* Dead *) ) 
    ; TRY 
        Rd . Close ( Options . PlaybackRdT ) 
      EXCEPT Rd . Failure 
      , Thread . Alerted 
      => (* Just ignore these.  We are closing, after all. *) 
      END (* TRY EXCEPT *) 
    ; FormsVBT . PutText 
        ( Form 
        , "Rc_Playback_FileName" 
        , PlaybackClosedFileString 
        )
    ; PlaybackClosure . IsOpen := FALSE   
    ; SetDormant ( PlaybackClosedDormant ) 
    ; SetActive ( PlaybackClosedActive ) 
    END PlaybackCloseProc  

; PROCEDURE PlaybackRunProc  
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LDidRun : BOOLEAN 

  ; BEGIN 
      LOCK PlaybackClosure . Sync 
      DO 
        LDidRun := PlaybackClosure . Sync . State = PlaybackStateTyp . Stopped 
      ; IF LDidRun 
        THEN 
          PlaybackClosure . Sync . State := PlaybackStateTyp . Running 
        ; SetDormant ( PlaybackRunningDormant ) 
        ; SetActive ( PlaybackRunningActive ) 
        ; Thread . Signal ( PlaybackClosure . Sync . WaitingRun ) 
        END (* IF *) 
      END (* LOCK *) 
    END PlaybackRunProc  

; PROCEDURE PlaybackStopProc  
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LWasRunning : BOOLEAN 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
      Stop ( (* VAR *) LWasRunning ) 
    ; IF LWasRunning  
      THEN 
        SetDormant ( PlaybackStoppedDormant ) 
      ; SetActive ( PlaybackStoppedActive ) 
      ; FormsVBT . PutText 
          ( Form , "Rc_Playback_Message" , "Stopped by user" ) 
      END (* IF *) 
    END PlaybackStopProc  

; PROCEDURE PlaybackSkipProc  
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      LOCK PlaybackClosure . Sync 
      DO 
        IF PlaybackClosure . Sync . State = PlaybackStateTyp . Stopped 
        THEN 
          PlaybackClosure . Sync . State := PlaybackStateTyp . Skipping  
        ; Thread . Signal ( PlaybackClosure . Sync . WaitingRun ) 
        ; Thread . Wait 
            ( PlaybackClosure . Sync , PlaybackClosure . Sync . WaitingStop ) 
        END (* IF *) 
      ; PlaybackClosure . Sync . RespectStops := TRUE 
      END (* LOCK *) 
    END PlaybackSkipProc  

; PROCEDURE PlaybackStepProc  
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      LOCK PlaybackClosure . Sync 
      DO 
        IF PlaybackClosure . Sync . State = PlaybackStateTyp . Stopped 
        THEN 
          PlaybackClosure . Sync . State := PlaybackStateTyp . Stepping  
        ; Thread . Signal ( PlaybackClosure . Sync . WaitingRun ) 
        ; Thread . Wait 
            ( PlaybackClosure . Sync , PlaybackClosure . Sync . WaitingStop ) 
        END (* IF *) 
      ; PlaybackClosure . Sync . RespectStops := TRUE 
      END (* LOCK *) 
    END PlaybackStepProc

; <* UNUSED *> CONST Divisor = 1.0D1   

; PROCEDURE ConvertDelay ( Value : INTEGER ) : LONGREAL 

  = VAR LDivisor := 10.0
(* COMPILER BUG!  pm3 sometimes turns 1.0D1 into a NaN, unpredictably,
                  and consistently turns 0.1D0 into a NaN
   SHEESH!
*) 
  ; VAR LValue1 : REAL 
  ; VAR LValue2 : REAL 
  ; VAR LValue3 : LONGREAL 

  ; BEGIN 
      LValue1 := FLOAT ( Value , REAL ) 
    ; LValue2 := LValue1 / LDivisor  
    ; LValue3 := FLOAT ( Value , LONGREAL )  
(* Giving up dividing by 10 for now. *) 
    ; RETURN LValue3   
    END ConvertDelay 

; PROCEDURE GetPlaybackDelay ( ) 

  = VAR LValue : INTEGER 
  ; VAR LDelay : LONGREAL 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    BEGIN 
      LValue 
        := FormsVBT . GetInteger 
             ( Options . RecPlayForm , "Rc_Playback_Delay" ) 
    ; LDelay := ConvertDelay ( LValue ) 
    ; LOCK PlaybackClosure . Sync 
      DO 
        PlaybackClosure . Sync . Delay := LDelay 
      END (* LOCK *) 
    END GetPlaybackDelay   

; PROCEDURE PlaybackDelayProc  
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      GetPlaybackDelay ( ) 
    END PlaybackDelayProc  

; VAR NullHandler : OpenOKClosureTyp

; VAR CommandTable : LexTable . T 

; TYPE CommandPairTyp 
    = RECORD 
        String : TEXT 
      ; Value : CommandTyp 
      ; ReverseMap : BOOLEAN := TRUE 
      END
; TYPE P = CommandPairTyp 

; CONST Commands = ARRAY OF CommandPairTyp 
    { P { "Devel_GenChild" , CommandTyp . GenChildInterface }  
    , P { "STOP" , CommandTyp . STOP } 
    , P { "File_Open_Text" , CommandTyp . FileOpen } 
    , P { "File_Quit" , CommandTyp . FileQuit } 
    , P { "File_Save_Text" , CommandTyp . FileSave } 
    , P { "File_SaveAs_Text" , CommandTyp . FileSaveAs } 
    , P { "File_Export_Text" , CommandTyp . FileExport } 
    , P { "File_CloseImage_Text" , CommandTyp . FileCloseImage } 
    , P { "File_CloseWindow_TextInt" , CommandTyp . FileCloseWindow } 
    , P { "Edit_Cut" , CommandTyp . EditCut } 
    , P { "Edit_Copy" , CommandTyp . EditCopy } 
    , P { "Edit_Paste" , CommandTyp . EditPaste } 
    , P { "MouseClick" , CommandTyp . MouseClickPixel } 
    , P { "MouseClickChar" , CommandTyp . MouseClickChar } 
    , P { "CharDelFwd" , CommandTyp . CharDelFwd } 
    , P { "CharDelBwd" , CommandTyp . CharDelBwd } 
    , P { "DeleteRestOfLine" , CommandTyp . DeleteRestOfLine } 
    , P { "CharType_Char" , CommandTyp . CharType } 
    , P { "CharTranspose" , CommandTyp . CharTranspose } 
    , P { "ClearSelection" , CommandTyp . ClearSelection } 
    , P { "SweepSelection" , CommandTyp . SweepSelection } 
    , P { "EndSweepSelection" , CommandTyp . EndSweepSelection } 
    , P { "CursorUp" , CommandTyp . CursorUp } 
    , P { "CursorDown" , CommandTyp . CursorDown } 
    , P { "CursorLeft" , CommandTyp . CursorLeft } 
    , P { "CursorRight" , CommandTyp . CursorRight } 
    , P { "Begin" , CommandTyp . BeginKey , ReverseMap := FALSE } 
    , P { "BeginKey" , CommandTyp . BeginKey } 
    , P { "End" , CommandTyp . EndKey , ReverseMap := FALSE } 
    , P { "EndKey" , CommandTyp . EndKey } 
    , P { "Prior" , CommandTyp . PriorKey , ReverseMap := FALSE } 
    , P { "PriorKey" , CommandTyp . PriorKey } 
    , P { "Next" , CommandTyp . NextKey , ReverseMap := FALSE } 
    , P { "NextKey" , CommandTyp . NextKey } 
    , P { "Home" , CommandTyp . HomeKey } 
    , P { "InsertModeOn" , CommandTyp . InsertModeOn } 
    , P { "InsertModeOff" , CommandTyp . InsertModeOff } 
    , P { "SemParse" , CommandTyp . SemParse } 
    , P { "SemAccept" , CommandTyp . SemAccept } 
    , P { "SemAnalyze" , CommandTyp . SemAnalyze } 
    , P { "SearchStringFwd" , CommandTyp . SearchStringFwd } 
    , P { "SearchStringBwd" , CommandTyp . SearchStringBwd } 
    , P { "ReplaceString" , CommandTyp . ReplaceString } 
    , P { "VertScroll" , CommandTyp . VertScroll } 
    , P { "HorizScroll" , CommandTyp . HorizScroll } 

    , P { "Devel_WriteCheckpoint" , CommandTyp . WriteCheckpoint }
    , P { "Devel_TakeFocus" , CommandTyp . TakeFocus }
    , P { "Devel_Repaint" , CommandTyp . Repaint }
    , P { "Devel_ForceAssert" , CommandTyp . ForceAssert }
    , P { "Devel_MergeText" , CommandTyp . MergeText }
    , P { "Devel_ReconstructLines" , CommandTyp . ReconstructLines }
    , P { "Devel_VerifyLinesRefs" , CommandTyp . VerifyLinesRefs }
    , P { "Devel_BrowseEst" , CommandTyp . BrowseEst }
    , P { "Devel_SetDebugLevel_Int" , CommandTyp . SetDebugLevel }
    , P { "Devel_WriteStats_Text" , CommandTyp . WriteStats }
    , P { "Devel_WriteEstPickle_Text" , CommandTyp . WriteEstPickle }
    , P { "Devel_GenEstModule_Text" , CommandTyp . GenEstModule }
    , P { "Devel_WriteParseInfo_Text" , CommandTyp . WriteParseInfo }
    , P { "Devel_WriteFsTrees_Text" , CommandTyp . WriteFsTrees }
    , P { "Devel_WriteSemPickle_Text" , CommandTyp . WriteSemPickle }
    , P { "Devel_GenTokInterface_Text" , CommandTyp . GenTokInterface }
    , P { "Devel_GenChildInterface_Text" , CommandTyp . GenChildInterface }
} 

(* TODO: Build the table and pickle it at Lbe build time, rather than
         on every startup.
*) 
; PROCEDURE EnsureInitTable ( ) 

  = BEGIN 
      IF CommandTable = NIL 
      THEN
        BuildLexMachine . MakeEmpty ( ) 
      ; FOR RI := FIRST ( Commands ) TO LAST ( Commands ) 
        DO WITH WPair = Commands [ RI ] 
          DO 
            BuildLexMachine . AddPair 
              ( WPair . String , ORD ( WPair . Value ) )  
          END (* WITH *) 
        END (* FOR *) 
      ; CommandTable := BuildLexMachine . Build ( ) 
      END (* IF *) 
    END EnsureInitTable 

; PROCEDURE EnsureInit ( ) 

  = BEGIN 
      IF RecordClosure = NIL 
      THEN 
        RecordClosure := NEW ( RecordClosureTyp ) 
      ; RecordClosure . SyncQueueT := NEW ( SyncQueue . T ) 
      ; RecordClosure . Sync := NEW ( RecordSyncTyp ) 
      ; RecordClosure . Sync . State := RecordStateTyp . Disabled   
      ; RecordClosure . Sync . WaitingEnable := NEW ( Thread . Condition ) 
      ; RecordClosure . Sync . WaitingDisable := NEW ( Thread . Condition ) 
      ; RecordClosure . ThreadT := Thread . Fork ( RecordClosure ) 
      ; RecordClosure . IsOpen := FALSE   
      END (* IF *) 
    ; IF PlaybackClosure = NIL 
      THEN 
        PlaybackClosure := NEW ( PlaybackClosureTyp ) 
      ; PlaybackClosure . Sync := NEW ( PlaybackSyncTyp ) 
      ; PlaybackClosure . Sync . State := PlaybackStateTyp . Stopped   
      ; PlaybackClosure . Sync . WaitingRun := NEW ( Thread . Condition ) 
      ; PlaybackClosure . Sync . WaitingStop := NEW ( Thread . Condition ) 
      ; PlaybackClosure . ThreadT := Thread . Fork ( PlaybackClosure ) 
      ; PlaybackClosure . Sync . RespectStops := RespectStops  
      ; PlaybackClosure . IsOpen := FALSE   
      END (* IF *) 
    ; EnsureInitTable ( )  
    END EnsureInit 

; VAR GIsInstalled : BOOLEAN := FALSE 

; PROCEDURE RecPlayProc  
    ( MainForm : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *> 
    BEGIN 
      TRY 
        IF GIsInstalled 
        THEN
          Trestle . MoveNear ( Options . RecPlayForm , MainForm ) 
        ELSE
          PlaybackClosure . Window 
            := FormsVBT . GetGeneric ( MainForm , "Fv_LbeWindow" ) 
        ; Trestle . Install ( Options . RecPlayForm ) 
        ; GIsInstalled := TRUE 
     (* ; Trestle . AwaitDelete ( Options . RecPlayForm ) 
        ^ Can't do this running a thread that handles a VBT event.
          It tries to reacquire the same MUTEX, VBT.mu. 
          No need for this thread to wait anyway. *) 
        END (* IF *) 
      EXCEPT
      | TrestleComm . Failure 
      => TRY 
          DL ( LbeStd . AppName 
               & ": Could not open display " 
               & Options . Display 
               & " for record/play window." 
             )
        EXCEPT Thread . Alerted=> (* Ignore *) 
        END (* TRY EXCEPT *) 
      END (* TRY EXCEPT *) 
    ; GetPlaybackDelay ( ) (* Initial value. *)  
    END RecPlayProc 

; PROCEDURE InitDialogHandlers ( ) 

  = BEGIN 
      RecordOpenOKHandler 
        := NEW ( OpenOKClosureTyp , apply := RecordOpenOK )  
    ; RecordAppendOKHandler 
        := NEW ( OpenOKClosureTyp , apply := RecordAppendOK )  
    ; PlaybackOpenOKHandler 
        := NEW ( OpenOKClosureTyp , apply := PlaybackOpenOK )  
    ; NullHandler := NEW ( OpenOKClosureTyp )  
    END InitDialogHandlers 

; PROCEDURE AttachButtonHandlers ( RecPlayForm : FormsVBT . T ) 

  = <* FATAL FormsVBT . Error *> 
    BEGIN (* AttachButtonHandlers *) 
      FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Record_Open" , RecordOpenDialog ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Record_Append" , RecordAppendDialog ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Record_Reopen" , RecordReopenProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Record_Close" , RecordCloseProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Record_Enable" , RecordEnableProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Record_Disable" , RecordDisableProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Record_InsertStop" , RecordInsertStopProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Playback_Open" , PlaybackOpenDialog ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Playback_Reopen" , PlaybackReopenProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Playback_Close" , PlaybackCloseProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Playback_Run" , PlaybackRunProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Playback_Stop" , PlaybackStopProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Playback_Skip" , PlaybackSkipProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Playback_Step" , PlaybackStepProc ) 
    ; FormsVBT . AttachProc 
        ( RecPlayForm , "Rc_Playback_Delay" , PlaybackDelayProc ) 
    END AttachButtonHandlers 

(* VISIBLE: *) 
; PROCEDURE AttachHandlers ( WindowForm : FormsVBT . T ) 

  = <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    <* FATAL Rd . Failure *> 
    BEGIN 
(* TODO: Try to postpone this initialization until the record/play
         window is actually opened? 
*) 
      Options . RecordFileName := DefaultRecordFileName 
    ; Options . PlaybackFileName := DefaultPlaybackFileName 
    ; Options . RecPlayForm := NEW ( FormsVBT . T ) 
    ; TRY 
        EVAL Options . RecPlayForm . initFromRsrc
               ( "RecPlay.fv" , Options . ResourcePath )
      EXCEPT 
        Rsrc . NotFound 
        => TRY 
            DL ( LbeStd . AppName & ": Unable to locate resource RecPlay.fv" )
          EXCEPT Thread . Alerted=> (* Ignore *) 
          END (* TRY EXCEPT *) 
        ; RETURN 
      | FormsVBT . Error ( msg )
        => DL ( LbeStd . AppName & ": Unable to init from resource RecPlay.fv ("
                & msg & ")" )  
        ; RETURN 
      | Thread . Alerted 
        => RETURN 
      END 
    ; RecordClosedFileString 
        := FormsVBT . GetText 
             ( Options . RecPlayForm , "Rc_Record_FileName" ) 
    ; PlaybackClosedFileString 
        := FormsVBT . GetText 
             ( Options . RecPlayForm , "Rc_Playback_FileName" ) 
    ; AttachButtonHandlers ( Options . RecPlayForm ) 
    ; InitDialogHandlers ( ) 
    ; FormsVBT . AttachProc 
        ( WindowForm , "Fv_Devel_RecPlayWindow" , RecPlayProc ) 
    ; EnsureInit ( ) 
    ; GRecordLogWrT 
        := TypescriptVBT . GetWr 
             ( FormsVBT . GetVBT ( Options . RecPlayForm , "Rc_Record_Log" ) ) 
    ; RespectStops := TRUE 
    END AttachHandlers 

; BEGIN 
  END UiRecPlay 
. 
