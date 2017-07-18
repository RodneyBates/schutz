
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE UiDevel 

; IMPORT RuntimeError 

; IMPORT AnyEvent 
; IMPORT FormsVBT 
; IMPORT Pathname 
(* Except for swapping the strings "Pickle" and "Pickle2 AS Pickle", make no
   changes to the following line, as it is recognized/edited by scripts.
   (see scripts/topickle.sh and scripts/topickle2.sh)
*) 
; IMPORT Pickle2 AS Pickle (* The Pickle2 mess. *)
; IMPORT Rd 
; IMPORT Rsrc 
; IMPORT Stdio 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Trestle 
; IMPORT TrestleComm 
; IMPORT TypescriptVBT 
; IMPORT VBT 
; IMPORT Wr 

; IMPORT Assertions 
; FROM Assertions IMPORT AssertionFailure 
; IMPORT Boot 
; IMPORT Display 
; IMPORT EditWindow 
; IMPORT EstUtil 
; IMPORT Files 
; IMPORT GenConstEst 
; IMPORT Images 
; IMPORT Infos 
; IMPORT LbeStd  
; IMPORT LdlSemantics 
; IMPORT MessageCodes 
; IMPORT Misc 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT SharedStrings 
; IMPORT TextEdit  
; IMPORT TreeBrowse 
; IMPORT Ui 
; IMPORT UiRecPlay 
; IMPORT VersionedFiles 
; IMPORT Worker 


(* Default names for output files: *) 
; CONST StatisticsName = "Statistics"
; CONST MakeEstName = "MakeEst" (* Will have ".m3" appended. *)  
; CONST EstPickleName = "Est.pkl"
; CONST ParseInfoName = "ParseInfo.dump"
; CONST FsTreesName = "FsTrees.dump"
; CONST SemPickleName = "Sem.pkl"
; CONST TokInterfaceName = "Tok" (* Will have ".i3" appended. *)
; CONST ChildInterfaceName = "Child" (* Will have ".i3" appended. *)

; CONST NullTime = FIRST ( VBT . TimeStamp ) 

(* Common code for the items in the development menu that write files and:
   1) need a file name dialog for the name of the file to write, and
   2) when done successfully, may precipitate starting the next of the group.
*) 

; TYPE WorkerClosureWriteTyp 
   = Worker . ClosureTyp 
        OBJECT 
          FileName : TEXT := NIL 
        ; WrT : Wr . T := NIL 
        ; DoAll : BOOLEAN := FALSE  
        END 
  (* Holds info for the file-writing operations.  When a menu item is 
     clicked, one of these is allocated, Form, Window, ImageTrans, ImagePers,
     and DoAll are set by WriteDialog and the file name prompt box is popped.
     A FormsVBT.Closure that points to this is also allocated and attached 
     to the OK button of the prompt box.  
     If/when the user clicks OK, Trestle calls back its apply method, which 
     is always procedure WriteOKCallback.  This stores the 
     file name and opens WrT, then requests the WorkProc of the closure.  
  *)  

; TYPE WriteOKClosureTyp 
    = FormsVBT . Closure 
        OBJECT 
          WorkerClosure : WorkerClosureWriteTyp 
          (* This is tangled, but seems necessary to avoid a Worker 
             dependency on VBT.  We need a FormsVBT.Closure to attach
             to OK buttons, and a worker closure to use the worker
             thread.  So, the former points to the latter.  
          *) 
        END (* OBJECT *) 

; PROCEDURE WriteFileNameDialog  
    ( DialogName : TEXT 
    ; FileSuffix : TEXT
    ; Closure : WorkerClosureWriteTyp 
    ; MustBeAnalyzed : BOOLEAN 
    ) 
  (* PRE: Closure . Window, Time, and DoAll are set.  
          Closure . apply is overridden with a WorkProc. 
  *) 
  (* This is called when user wants one of the generated files to be
     written. It pops either an error dialog, or a file name dialog. 
  *) 

  = VAR LForm : FormsVBT . T 
  ; VAR LImageTrans : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LPreloadFileName : TEXT 
  ; VAR LVBTClosure : FormsVBT . Closure 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; IF Closure . Window # NIL 
      THEN 
        LImageTrans := Closure . Window . WrImageRef 
      ; IF LImageTrans # NIL 
        THEN
          LImagePers := LImageTrans . ItPers  
        ; IF LImagePers . IpLang  < LbeStd . LangFirstLdl  
             OR LImagePers . IpLang > LbeStd . LangLastLdl  
          THEN  
            FormsVBT . PutText 
              ( LForm 
              , "Fv_ErrorPopup_Message" 
              , "The open image is not a Language Definition Language"
              )
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          ELSIF MustBeAnalyzed AND NOT LImagePers . IpIsAnalyzed 
          THEN 
            FormsVBT . PutText 
              ( LForm 
              , "Fv_ErrorPopup_Message" 
              , "The open image must be analyzed."
              )
            ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          ELSE 
            FormsVBT . PutText
              ( LForm 
              , "Fv_BootWriteDialog_Title" 
              , DialogName 
              )
          ; LPreloadFileName 
              := Misc . JoinPath 
                   ( Options . DevelWritePath 
                   , LdlSemantics . LdlModuleName 
                       ( Closure . Window . WrImageRef ) 
                     & FileSuffix 
                   ) 
          ; FormsVBT . PutText 
              ( LForm 
              , "Fv_BootWriteDialog_FileName" 
              , LPreloadFileName 
              )
          ; TYPECASE FormsVBT . GetTheEvent ( LForm ) 
            OF AnyEvent . Misc ( TMisc ) 
            => IF TMisc . misc . type = FormsVBT . MakeEventMiscCodeType 
               THEN (* This was precipitated by a previous write dialog
                       finishing its WorkProc and finding DoAll TRUE. *) 
                 Closure . DoAll := TRUE 
               END (* IF *) 
            ELSE 
            END (* TYPECASE *) 
          ; LVBTClosure 
              := NEW ( WriteOKClosureTyp 
                     , WorkerClosure := Closure 
                     , apply := WriteOKCallback 
                     ) 
          ; FormsVBT . Attach 
              ( LForm , "Fv_BootWriteDialog_OK" , LVBTClosure ) 
          ; FormsVBT . PopUp ( LForm , "Fv_BootWriteDialog" ) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END WriteFileNameDialog  

(* Common procedures for after OK of a write dialog has been clicked. *) 

; PROCEDURE WriteOKCallback 
    ( CallbackClosure : WriteOKClosureTyp   
    ; Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; Time : VBT . TimeStamp 
    ) 
  (* PRE: CallbackClosure . WorkerClosure . Window, Time, and DoAll are set.  
          CallbackClosure . WorkerClosure . apply is overridden with a
           WorkProc. 
  *) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN
      CallbackClosure . WorkerClosure . Time := Time 
    ; CallbackClosure . WorkerClosure . FileName 
        := FormsVBT . GetText ( Form , "Fv_BootWriteDialog_FileName" ) 
 (* ; FormsVBT . PopDown ( Form , "Fv_BootWriteDialog" )
    ; FormsVBT . Attach 
        ( Options . MainForm , "Fv_BootWriteDialog_OK" , NIL ) 
 *) 
    (* Although very unlikely, there is technicallly a messy timing problem, 
       because FormsVBT.MakeEvent does not queue the event, but executes it
       entirely inside its dymanic scope.  After "GenarateAll", it could
       still be keeping the worker thread busy after popping the file name
       dialog that the user has responded to here, resulting in the work
       we now request being refused.  
    *) 
    ; TRY 
        EVAL Worker . RequestWork 
               ( CallbackClosure . WorkerClosure 
               , Interactive := TRUE 
               , WaitToStart := TRUE 
               , WaitToFinish := FALSE 
               )
      EXCEPT Thread . Alerted => 
      (* Suppress warning.  Can't happen, since Interactive := TRUE *) 
      END (* TRY EXCEPT *)  
    END WriteOKCallback  

; PROCEDURE OpenWrite ( Closure : WorkerClosureWriteTyp ) 
  : BOOLEAN (* Success *) 
  RAISES { Thread . Alerted } 
  (* PRE: Closure . Window and FileName are set. *) 
  (* POST: If opened successfully, Closure . WrT is set and open. *) 
  (* Runs on worker thread. *) 

  = VAR LForm : FormsVBT . T 
  ; VAR LSuccess : BOOLEAN 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; IF Closure . FileName = NIL OR Text . Equal ( Closure . FileName , "" )  
      THEN 
         IF Closure . IsInteractive 
         THEN 
           FormsVBT . PutText 
             ( LForm 
             , "Fv_ErrorPopup_Message" 
             , "Empty file name specified." 
             )
        ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
        END (* IF *) 
      ; Closure . WrT := NIL 
      ; LSuccess := FALSE 
      ELSE 
        TRY 
          Closure . WrT := VersionedFiles . OpenWrite ( Closure . FileName ) 
        ; LSuccess := TRUE 
        EXCEPT 
        VersionedFiles . Error ( EMessage ) 
        => IF Closure . IsInteractive 
           THEN 
             FormsVBT . PutText 
               ( LForm 
               , "Fv_ErrorPopup_Message" 
               , "Couldn't open file \"" & Closure . FileName & "\": "
                 & EMessage 
               )
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          END (* IF *) 
        ; Closure . WrT := NIL 
        ; LSuccess := FALSE
        END (* TRY EXCEPT *) 
      END (* IF *) 
    ; RETURN LSuccess 
    END OpenWrite  

; PROCEDURE FinishWrite 
    ( Closure : WorkerClosureWriteTyp 
    ; Success : BOOLEAN 
    ; CommandString : TEXT 
    ; SuccessorName : TEXT 
    ) 
  RAISES { Thread . Alerted }   
  (* PRE: Closure . Window, FileName, DoAll, and WrT are set, WrT is open. *) 
  (* Runs on worker thread. *) 

  = VAR LForm : FormsVBT . T 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; TRY 
        Wr . Close ( Closure . WrT ) 
      EXCEPT Wr . Failure 
      => Success := FALSE 
      END (* TRY EXCEPT *) 
    ; IF Success 
      THEN 
        FormsVBT . PopDown ( LForm , "Fv_BootWriteDialog" )
      ; FormsVBT . Attach 
          ( LForm , "Fv_BootWriteDialog_OK" , NIL ) 
      ; Options . DevelWritePath 
          := Pathname . Prefix 
               ( Misc . AbsFileName ( Closure . FileName ) )  
      ; UiRecPlay . RecordString ( CommandString ) 
      ; IF Closure . DoAll 
        THEN 
          FormsVBT . MakeEvent 
            ( LForm 
            , SuccessorName 
            , Closure . Time (* Which will surely be outdated by now. *)  
            ) 
        END (* IF *) 
      ELSE 
        IF Closure . IsInteractive 
        THEN 
          FormsVBT . PutText 
            ( LForm 
            , "Fv_ErrorPopup_Message" 
            , "Failed to write file \"" & Closure . FileName & "\""
            )
        ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
        END (* IF *) 
      END (* IF *) 
    END FinishWrite 

(* Write statistics. *) 

; PROCEDURE WriteStatsWorkProc ( Closure : WorkerClosureWriteTyp ) 
  RAISES { AssertionFailure , Thread . Alerted }   
  (* PRE: Closure . Window, DoAll, and FileName are set. *) 
  (* Runs on worker thread. *) 

  = VAR LStats : EstUtil . StatisticsTyp 
  ; VAR LCommandString : TEXT  

  ; BEGIN 
      EVAL Ui . SetImageTransAndPers ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommandPlusString 
             ( UiRecPlay . CommandTyp . WriteStats , Closure . FileName ) 
    ; IF OpenWrite ( Closure ) 
      THEN 
        Boot . PrintSizes ( Closure . WrT ) 
      ; IF Closure . ImagePers # NIL 
           AND Closure . ImagePers . IpEstRoot # NIL 
        THEN 
          EstUtil . Statistics 
            ( Closure . ImagePers . IpEstRoot , LStats ) 
        ; Boot . PrintStats ( Closure . WrT , LStats ) 
        END (* IF *) 
      ; FinishWrite 
          ( Closure 
          , Success := TRUE 
          , CommandString := LCommandString
          , SuccessorName := "Fv_Devel_WriteEstPickle"  
          ) 
      END (* IF *) 
    END WriteStatsWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayWriteStats ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureWriteTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , FileName := FileName 
                     , DoAll := FALSE 
                     , apply := WriteStatsWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayWriteStats 

; PROCEDURE WriteStatsCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      WriteFileNameDialog
        ( "Write Ldl Statistics"
        , StatisticsName 
        , Closure
            := NEW ( WorkerClosureWriteTyp
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , DoAll := FALSE (* May change later. *) 
                   , apply := WriteStatsWorkProc 
                   )  
        , MustBeAnalyzed := FALSE 
        ) 
    END WriteStatsCallback 

(* Write Est pickle. *) 

; PROCEDURE WriteEstPickleWorkProc ( Closure : WorkerClosureWriteTyp )  
  RAISES { Thread . Alerted }   
  (* PRE: Closure . Window, DoAll, and FileName are set. *) 
  (* Runs on worker thread. *) 

  = VAR LCommandString : TEXT  
  ; VAR LSuccess : BOOLEAN 

  ; BEGIN 
      EVAL Ui . SetImageTransAndPers ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommandPlusString 
             ( UiRecPlay . CommandTyp . WriteEstPickle , Closure . FileName ) 
    ; IF OpenWrite ( Closure ) 
      THEN 
        IF Closure . ImageTrans # NIL 
        THEN 
          TRY
            Pickle . Write 
              ( Closure . WrT , LbeStd . EstPickleIdInfoRef ) 
          ; Pickle . Write 
              ( Closure . WrT , Closure . ImagePers . IpEstRoot )
          EXCEPT 
          ELSE 
            LSuccess := FALSE  
          END 
        ; LSuccess := TRUE  
        ELSE 
          LSuccess := FALSE 
        END 
      ; FinishWrite 
          ( Closure 
          , LSuccess 
          , CommandString := LCommandString 
          , SuccessorName := "Fv_Devel_GenEstModule"  
          ) 
      END (* IF *) 
    END WriteEstPickleWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayWriteEstPickle ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureWriteTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , FileName := FileName 
                     , DoAll := FALSE 
                     , apply := WriteEstPickleWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayWriteEstPickle 

; PROCEDURE WriteEstPickleCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      WriteFileNameDialog
        ( "Write Est Pickle " 
        , EstPickleName 
        , Closure
            := NEW ( WorkerClosureWriteTyp
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , DoAll := FALSE (* May change later. *) 
                   , apply := WriteEstPickleWorkProc 
                   )  
        , MustBeAnalyzed := FALSE 
        ) 
    END WriteEstPickleCallback 

(* Generate Est module. *) 

; PROCEDURE GenEstModuleWorkProc ( Closure : WorkerClosureWriteTyp )  
  RAISES { AssertionFailure , Thread . Alerted }   
  (* PRE: Closure . Window, DoAll, and FileName are set. *) 
  (* Runs on worker thread. *)   

  = VAR LLangInfoRef : LdlSemantics . LangInfoRefTyp 
  ; VAR LCommandString : TEXT 
  ; VAR LSuccess : BOOLEAN 

  ; BEGIN 
      EVAL Ui . SetImageTransAndPers ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommandPlusString 
             ( UiRecPlay . CommandTyp . GenEstModule , Closure . FileName ) 
    ; IF OpenWrite ( Closure ) 
      THEN 
        LSuccess := FALSE  
      ; IF Closure . ImageTrans # NIL 
           AND Closure . ImagePers . IpEstRoot # NIL 
        THEN 
          LLangInfoRef := Closure . ImagePers . IpSemRoot 
          (* ^Implicit NARROW can't fail. *)
        ; IF LLangInfoRef # NIL 
          THEN 
            GenConstEst . WriteStream 
              ( Lang := Closure . ImagePers . IpLang 
              , EstRoot := Closure . ImagePers . IpEstRoot 
              , LangInfoRef := LLangInfoRef 
              , UseLdlTok := TRUE 
              , ModuleName := Pathname . LastBase ( Closure . FileName ) 
              , WrT := Closure . WrT 
              ) 
          ; LSuccess := TRUE 
          END (* IF *) 
        END (* IF *)  
      ; FinishWrite 
          ( Closure 
          , LSuccess 
          , CommandString := LCommandString 
          , SuccessorName := "Fv_Devel_WriteParseInfo" 
          ) 
      END (* IF *) 
    END GenEstModuleWorkProc  

(* VISIBLE: *) 
; PROCEDURE ReplayGenEstModule ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureWriteTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , FileName := FileName 
                     , DoAll := FALSE 
                     , apply := GenEstModuleWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayGenEstModule 

; PROCEDURE GenEstModuleCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      WriteFileNameDialog
        ( "Generate Est-builder module"
        , MakeEstName & ".m3" 
        , Closure
            := NEW ( WorkerClosureWriteTyp
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , DoAll := FALSE (* May change later. *) 
                   , apply := GenEstModuleWorkProc 
                   )  
        , MustBeAnalyzed := FALSE 
        ) 
    END GenEstModuleCallback 

(* Write parse info. *) 

; PROCEDURE WriteParseInfoWorkProc ( Closure : WorkerClosureWriteTyp )  
  RAISES { Thread . Alerted }   
  (* PRE: Closure . Window, DoAll, and FileName are set. *) 
  (* Runs on worker thread. *)  

  = VAR LLangInfoRef : LdlSemantics . LangInfoRefTyp   
  ; VAR LCommandString : TEXT 
  ; VAR LSuccess : BOOLEAN 

  ; BEGIN 
      EVAL Ui . SetImageTransAndPers ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommandPlusString 
             ( UiRecPlay . CommandTyp . WriteParseInfo , Closure . FileName ) 
    ; IF OpenWrite ( Closure ) 
      THEN 
        IF Closure . ImageTrans # NIL 
        THEN 
          LLangInfoRef := Closure . ImagePers . IpSemRoot 
          (* ^Implicit NARROW can't fail. *)
        ; IF LLangInfoRef = NIL OR LLangInfoRef ^ . Gram = NIL 
          THEN 
            LSuccess := FALSE 
          ELSE 
            Infos . WriteProductions ( LLangInfoRef ^ . Gram , Closure . WrT ) 
          ; Infos . WriteStates ( LLangInfoRef ^ . Gram , Closure . WrT ) 
          ; LSuccess := TRUE  
          END 
        ELSE 
          LSuccess := FALSE 
        END 
      ; FinishWrite 
          ( Closure 
          , LSuccess 
          , CommandString := LCommandString 
          , SuccessorName := "Fv_Devel_WriteFsTrees" 
          ) 
      END (* IF *) 
    END WriteParseInfoWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayWriteParseInfo ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureWriteTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , FileName := FileName 
                     , DoAll := FALSE 
                     , apply := WriteParseInfoWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayWriteParseInfo 

; PROCEDURE WriteParseInfoCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      WriteFileNameDialog
        ( "Write Ldl parse info"
        , ParseInfoName 
        , Closure
            := NEW ( WorkerClosureWriteTyp
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , DoAll := FALSE (* May change later. *) 
                   , apply := WriteParseInfoWorkProc 
                   )  
        , MustBeAnalyzed := TRUE  
        ) 
    END WriteParseInfoCallback 

(* Write FS trees. *) 

; PROCEDURE WriteFsTreesWorkProc ( Closure : WorkerClosureWriteTyp )  
  RAISES { Thread . Alerted }   
  (* PRE: Closure . Window, DoAll, and FileName are set. *) 
  (* Runs on worker thread. *)  

  = VAR LCommandString : TEXT 
  ; VAR LSuccess : BOOLEAN 

  ; BEGIN 
      EVAL Ui . SetImageTransAndPers ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommandPlusString 
             ( UiRecPlay . CommandTyp . WriteFsTrees , Closure . FileName ) 
    ; IF OpenWrite ( Closure ) 
      THEN 
        IF Closure . ImageTrans # NIL 
        THEN 
          TRY
            LdlSemantics . DumpAllFsTreesStream  
              ( Closure . WrT 
              , Closure . ImagePers . IpLang 
              , IncludeDefaultValuedFields := FALSE 
              ) 
          EXCEPT ELSE 
            LSuccess := FALSE  
          END 
        ; LSuccess := TRUE  
        ELSE 
          LSuccess := FALSE 
        END 
      ; FinishWrite 
          ( Closure 
          , LSuccess 
          , CommandString := LCommandString 
          , SuccessorName := "Fv_Devel_WriteSemPickle" 
          ) 
      END (* IF *) 
    END WriteFsTreesWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayWriteFsTrees ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureWriteTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , FileName := FileName 
                     , DoAll := FALSE 
                     , apply := WriteFsTreesWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayWriteFsTrees 

; PROCEDURE WriteFsTreesCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      WriteFileNameDialog
        ( "Write format syntax trees"
        , FsTreesName  
        , Closure
            := NEW ( WorkerClosureWriteTyp
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , DoAll := FALSE (* May change later. *) 
                   , apply := WriteFsTreesWorkProc 
                   )  
        , MustBeAnalyzed := TRUE  
        ) 
    END WriteFsTreesCallback 

(* Write semantic pickle. *) 

; PROCEDURE WriteSemPickleWorkProc ( Closure : WorkerClosureWriteTyp )  
  RAISES { Thread . Alerted }   
  (* PRE: Closure . Window, DoAll, and FileName are set. *) 
  (* Runs on worker thread. *)  

  = VAR LCommandString : TEXT 
  ; VAR LSuccess : BOOLEAN 

  ; BEGIN 
      EVAL Ui . SetImageTransAndPers ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommandPlusString 
             ( UiRecPlay . CommandTyp . WriteSemPickle , Closure . FileName ) 
    ; IF OpenWrite ( Closure ) 
      THEN 
        IF Closure . ImageTrans # NIL 
        THEN 
          TRY
            Pickle . Write ( Closure . WrT , LbeStd . LangPickleIdInfoRef ) 
          ; Pickle . Write 
              ( Closure . WrT , Closure . ImagePers . IpSemRoot ) 
          EXCEPT ELSE 
            LSuccess := FALSE  
          END 
        ; LSuccess := TRUE  
        ELSE 
          LSuccess := FALSE 
        END 
      ; FinishWrite 
          ( Closure 
          , LSuccess 
          , CommandString := LCommandString 
          , SuccessorName := "Fv_Devel_GenTokInterface"   
          ) 
      END (* IF *) 
    END WriteSemPickleWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayWriteSemPickle ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureWriteTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , FileName := FileName 
                     , DoAll := FALSE 
                     , apply := WriteSemPickleWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayWriteSemPickle 

; PROCEDURE WriteSemPickleCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      WriteFileNameDialog
        ( "Write Ldl semantic pickle"
        , SemPickleName 
        , Closure
            := NEW ( WorkerClosureWriteTyp
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , DoAll := FALSE (* May change later. *) 
                   , apply := WriteSemPickleWorkProc 
                   )  
        , MustBeAnalyzed := TRUE
        ) 
    END WriteSemPickleCallback 

(* Generate token interface. *) 

; PROCEDURE GenTokInterfaceWorkProc ( Closure : WorkerClosureWriteTyp )  
  RAISES { Thread . Alerted }   
  (* PRE: Closure . Window, DoAll, and FileName are set. *) 
  (* Runs on worker thread. *)  

  = VAR LForm : FormsVBT . T 
  ; VAR LLangInfoRef : LdlSemantics . LangInfoRefTyp 
  ; VAR LCommandString : TEXT 
  ; VAR LSuccess : BOOLEAN 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    <* FATAL Wr . Failure *> 
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; EVAL Ui . SetImageTransAndPers ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommandPlusString 
             ( UiRecPlay . CommandTyp . GenTokInterface , Closure . FileName ) 
    ; IF OpenWrite ( Closure ) 
      THEN 
        IF Closure . ImageTrans # NIL 
        THEN 
          LLangInfoRef := Closure . ImagePers . IpSemRoot 
          (* ^Implicit NARROW can't fail. *)
        ; IF LLangInfoRef = NIL 
          THEN 
            LSuccess := FALSE 
          ELSE 
            TRY 
              LdlSemantics . WriteLdlTokInterfaceToStream 
                ( Closure . WrT 
                , LLangInfoRef 
                , Pathname . LastBase ( Closure . FileName )
                )  
            ; LSuccess := TRUE  
            EXCEPT Wr . Failure 
            => IF Closure . IsInteractive 
               THEN
                 FormsVBT . PutText 
                   ( LForm 
                   , "Fv_ErrorPopup_Message" 
                   , "Write failure writing \"" & Closure . FileName & "\"" 
                   ) 
               ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
               END (* IF *) 
            ; Wr . Close ( Closure . WrT ) 
            ; LSuccess := FALSE 
            END (* TRY EXCEPT *) 
          END 
        ELSE 
          LSuccess := FALSE 
        END 
      ; FinishWrite 
          ( Closure 
          , LSuccess 
          , CommandString := LCommandString 
          , SuccessorName := "Fv_Devel_GenChildInterface" 
          ) 
      END (* IF *) 
    END GenTokInterfaceWorkProc  

(* VISIBLE: *) 
; PROCEDURE ReplayGenTokInterface ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureWriteTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , FileName := FileName 
                     , DoAll := FALSE 
                     , apply := GenTokInterfaceWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayGenTokInterface 

; PROCEDURE GenTokInterfaceCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      WriteFileNameDialog
        ( "Generate LdlTok interface "
        , TokInterfaceName & ".i3"
        , Closure
            := NEW ( WorkerClosureWriteTyp
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , DoAll := FALSE (* May change later. *) 
                   , apply := GenTokInterfaceWorkProc 
                   )  
        , MustBeAnalyzed := TRUE 
        ) 
    END GenTokInterfaceCallback 

(* Generate child interface. *) 

; PROCEDURE GenChildInterfaceWorkProc ( Closure : WorkerClosureWriteTyp )  
  RAISES { AssertionFailure , Thread . Alerted }   
  (* PRE: Closure . Window, DoAll, and FileName are set. *) 
  (* Runs on worker thread. *)  

  = VAR LForm : FormsVBT . T 
  ; VAR LLangInfoRef : LdlSemantics . LangInfoRefTyp 
  ; VAR VAR LCommandString : TEXT 
  ; VAR LSuccess : BOOLEAN 

  ; <* FATAL FormsVBT . Error *> 
    <* FATAL FormsVBT . Unimplemented *> 
    <* FATAL Wr . Failure *> 
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; EVAL Ui . SetImageTransAndPers ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommandPlusString 
             ( UiRecPlay . CommandTyp . GenChildInterface 
             , Closure . FileName 
             ) 
    ; IF OpenWrite ( Closure ) 
      THEN 
        IF Closure . ImageTrans # NIL 
        THEN 
          LLangInfoRef := Closure . ImagePers . IpSemRoot 
          (* ^Implicit NARROW can't fail. *)
        ; IF LLangInfoRef = NIL 
          THEN 
            LSuccess := FALSE 
          ELSE 
            TRY 
              LdlSemantics . WriteLdlChildInterfaceToStream 
                ( Closure . WrT 
                , LLangInfoRef 
                , Pathname . LastBase ( Closure . FileName ) 
                )  
            ; LSuccess := TRUE  
            EXCEPT Wr . Failure 
            => IF Closure . IsInteractive 
               THEN
                 FormsVBT . PutText 
                   ( LForm 
                   , "Fv_ErrorPopup_Message" 
                   , "Write failure writing \"" & Closure . FileName & "\"" 
                   ) 
               ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
               END (* IF *) 
            ; Wr . Close ( Closure . WrT ) 
            ; LSuccess := FALSE 
            END (* TRY EXCEPT *) 
          END 
        ELSE 
          LSuccess := FALSE 
        END 
      ; Closure . DoAll := FALSE 
      ; FinishWrite 
          ( Closure 
          , LSuccess 
          , CommandString := LCommandString 
          , SuccessorName := NIL 
          ) 
      END (* IF *) 
    END GenChildInterfaceWorkProc  

(* VISIBLE: *) 
; PROCEDURE ReplayGenChildInterface ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureWriteTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , FileName := FileName 
                     , DoAll := FALSE 
                     , apply := GenChildInterfaceWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayGenChildInterface 

; PROCEDURE GenChildInterfaceCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      WriteFileNameDialog
        ( "Generate LdlChild interface"
        , ChildInterfaceName  & ".i3"
        , Closure
            := NEW ( WorkerClosureWriteTyp
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , DoAll := FALSE (* May change later. *) 
                   , apply := GenChildInterfaceWorkProc 
                   )  
        , MustBeAnalyzed := TRUE 
        ) 
    END GenChildInterfaceCallback 

(* Write/generate all the files in this group. *) 

; PROCEDURE GenAllCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      WriteFileNameDialog
        ( "Write Ldl Statistics"
        , StatisticsName 
        , Closure
            := NEW ( WorkerClosureWriteTyp
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , DoAll := TRUE 
                   , apply := WriteStatsWorkProc 
                   )  
        , MustBeAnalyzed := FALSE (* Applies only to WriteStats. *)  
        ) 
    END GenAllCallback

(* Simple actions with no dialog and succession. *) 

(* Write checkpoint. *) 

; PROCEDURE WriteCheckpointWorkProc ( Closure : Worker . ClosureTyp )  
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = VAR LForm : FormsVBT . T 
  ; VAR LCheckpointName : TEXT 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; EVAL Ui . SetImageTransAndPers ( Closure ) 
    ; IF Closure . ImageTrans # NIL 
      THEN 
        Closure . ImagePers . IpCrashCode 
          := ORD ( MessageCodes . T . NullCode )   
      ; Closure . ImagePers . IpCrashCommand := NIL 
      ; LCheckpointName 
          := Misc . CheckpointName ( Closure . ImagePers . IpAbsPklFileName )  
      ; TRY 
          Files . WriteImagePickle 
            ( Images . PersistentImageToSave 
                ( Closure . ImageTrans , ForSave := FALSE ) 
            , LCheckpointName 
            , DoCreateVersion := TRUE 
            )  
        ; Wr . PutText 
            ( Stdio . stderr 
            , "A checkpoint has been written to file:"
               & Wr . EOL  
               & "\"" 
               & LCheckpointName 
               & "\"" 
               & Wr . EOL  
            ) 
        ; Wr . Flush ( Stdio . stderr )
        ; IF Closure . IsInteractive  
          THEN 
             FormsVBT . PutText 
               ( LForm 
               , "Fv_ErrorPopup_Message" 
               , "Checkpoint written to file \"" & LCheckpointName & "\""     
               ) 
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          END (* IF *) 
        EXCEPT
          Files . Error ( EMessage ) 
        => IF Closure . IsInteractive 
           THEN 
             FormsVBT . PutText 
               ( LForm 
               , "Fv_ErrorPopup_Message" 
               , "Checkpoint failed for file \"" & LCheckpointName 
                 & "\", because: " & EMessage 
               ) 
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          (* Leave the Save dialog up and let the user either change
             something and try again, or explicitly cancel. *)  
          END (* IF *) 
        ELSE 
          IF Closure . IsInteractive 
          THEN 
             FormsVBT . PutText 
               ( LForm 
               , "Fv_ErrorPopup_Message" 
               , "Checkpoint failed for file \"" & LCheckpointName & "\""     
               ) 
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          (* Leave the Save dialog up and let the user either change
             something and try again, or explicitly cancel. *)  
          END (* IF *) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END WriteCheckpointWorkProc   

(* VISIBLE: *) 
; PROCEDURE ReplayWriteCheckpoint ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := WriteCheckpointWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayWriteCheckpoint   

; PROCEDURE WriteCheckpointCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      EVAL Worker . RequestWorkInteractive  
             ( NEW ( Worker . ClosureTyp 
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )    
                   , Time := Time 
                   , apply := WriteCheckpointWorkProc 
                   ) 
             ) 
    END WriteCheckpointCallback  

(* Take focus.  No need to do this on worker thread. *) 

(* VISIBLE: *) 
; PROCEDURE ReplayTakeFocus ( ) 

  = VAR LWindow : EditWindow . T 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LWindow 
        := FormsVBT . GetGeneric ( Options . MainForm , "Fv_LbeWindow" )  
(*  ; UiRecPlay . Record ( UiRecPlay . CommandTyp . TakeFocus ) 
      This was ridiculous. 
*) 
    ; EditWindow . TakeKBFocus ( LWindow , NullTime ) 
    END ReplayTakeFocus  

; PROCEDURE TakeFocusCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LWindow : EditWindow . T 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )  
(*  ; UiRecPlay . Record ( UiRecPlay . CommandTyp . TakeFocus ) 
      This was ridiculous. 
*) 
    ; EditWindow . TakeKBFocus ( LWindow , Time ) 
    END TakeFocusCallback 

(* Repaint. *) 

; PROCEDURE RepaintWorkProc ( Closure : Worker . ClosureTyp )  
  RAISES { AssertionFailure , Thread . Alerted }   
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   
 
  = VAR LCommandString : TEXT 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . Repaint ) 
    ; Display . PaintWindowFromLines  
        ( Closure . Window 
        , (* VAR *) LTrailingBlankLines (* Dead. *) 
        , (* VAR *) LLinesRef (* Dead. *) 
        , (* VAR *) LLineNo  (* Dead. *) 
        )
    ; UiRecPlay . RecordString ( LCommandString ) 
    END RepaintWorkProc  

(* VISIBLE: *) 
; PROCEDURE ReplayRepaint ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := RepaintWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayRepaint  

; PROCEDURE RepaintCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      EVAL Worker . RequestWorkInteractive  
             ( NEW ( Worker . ClosureTyp 
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )    
                   , Time := Time 
                   , apply := RepaintWorkProc 
                   ) 
             ) 
    END RepaintCallback

(* Reconstruct lines. *)   

; PROCEDURE ReconstructLinesWorkProc ( Closure : Worker . ClosureTyp )  
  RAISES { AssertionFailure , Thread . Alerted }   
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = VAR LCommandString : TEXT 

  ; BEGIN 
      EVAL Ui . SetImageTrans ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommand 
             ( UiRecPlay . CommandTyp . ReconstructLines ) 
    ; Display . ReconstructLinesAndPaint ( Closure . ImageTrans )
    ; UiRecPlay . RecordString ( LCommandString ) 
    END ReconstructLinesWorkProc  

(* VISIBLE: *) 
; PROCEDURE ReplayReconstructLines ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := ReconstructLinesWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayReconstructLines  

; PROCEDURE ReconstructLinesCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      EVAL Worker . RequestWorkInteractive  
             ( NEW ( Worker . ClosureTyp 
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )    
                   , Time := Time 
                   , apply := ReconstructLinesWorkProc 
                   ) 
             ) 
    END ReconstructLinesCallback

(* Verify LinesRefs. *) 

; PROCEDURE VerifyLinesRefsWorkProc ( Closure : Worker . ClosureTyp )  
  RAISES { AssertionFailure , Thread . Alerted }   
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   
 
  = VAR LCommandString : TEXT 

  ; BEGIN 
      EVAL Ui . SetImageTrans ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommand 
             ( UiRecPlay . CommandTyp . VerifyLinesRefs ) 
    ; TextEdit . BruteForceVerifyAllLinesRefs 
        ( Closure . ImageTrans , RepairIsOK := FALSE )
    ; UiRecPlay . RecordString ( LCommandString ) 
    END VerifyLinesRefsWorkProc  

(* VISIBLE: *) 
; PROCEDURE ReplayVerifyLinesRefs ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := VerifyLinesRefsWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayVerifyLinesRefs  

; PROCEDURE VerifyLinesRefsCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      EVAL Worker . RequestWorkInteractive  
             ( NEW ( Worker . ClosureTyp 
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )    
                   , Time := Time 
                   , apply := VerifyLinesRefsWorkProc 
                   ) 
             ) 
    END VerifyLinesRefsCallback

(* Force assertion failure. *) 

; PROCEDURE ForceAssertWorkProc ( <* UNUSED *> Self : Worker . ClosureTyp ) 

  = VAR LSavedCallback : Assertions . CallbackTyp 

  ; BEGIN 
      UiRecPlay . Record ( UiRecPlay . CommandTyp . ForceAssert ) 
(* CHECK: Do we really care about recording this? *) 
    ; TRY
        Thread . AlertPause ( 5.0D0 ) 
      EXCEPT Thread . Alerted 
      => RETURN 
      END 
    ; TRY 
        LSavedCallback := Assertions . Callback 
      ; Assertions . Callback := Worker . Failure 
      ; TRY 
          Assertions . CantHappenText ( "User forced assertion" )  
        EXCEPT 
          RuntimeError.E => Assertions . DoNothing ( ) 
        | AssertionFailure  
          => Assertions . DoNothing ( ) 
        END (* TRY EXCEPT *) 
      FINALLY
        Assertions . Callback := LSavedCallback 
      END (* TRY FINALLY *) 
    END ForceAssertWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayForceAssert ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := ForceAssertWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => (* Discard. *) 
      END (* TRY EXCEPT *) 
    END ReplayForceAssert  

; PROCEDURE ForceAssertCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      EVAL Worker . RequestWorkInteractive  
             ( NEW ( Worker . ClosureTyp 
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , apply := ForceAssertWorkProc  
                   ) 
             ) 
    END ForceAssertCallback  

(* Replay last operation. No worker thread needed yet. *) 

; PROCEDURE ReplayLastCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LWindow : EditWindow . T 
  ; VAR LImageRef : PaintHs . ImageTransientTyp 
  ; VAR LDoSelfStop : BOOLEAN 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LWindow 
        := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )  
    ; IF LWindow # NIL 
      THEN 
        LImageRef := LWindow . WrImageRef 
      ; IF LImageRef # NIL AND LImageRef . ItPers . IpCrashCommand # NIL  
        THEN 
          Assertions . MessageText 
            ( "Replaying failing command: " 
              & LImageRef . ItPers . IpCrashCommand 
            ) 
        ; TRY 
            UiRecPlay . ExecuteOneAction 
              ( LImageRef . ItPers . IpCrashCommand 
              , LWindow 
              , RespectStops := TRUE (* Can a stop happen? *) 
              , (* VAR *) DoSelfStop := LDoSelfStop (* Dead *) 
              ) 
          EXCEPT Thread . Alerted => (* Ignore. *) 
          END (* TRY EXCEPT *) 
        END (* IF *) 
      END (* IF *) 
    END ReplayLastCallback 

(* MergeText. *) 

; PROCEDURE MergeTextWorkProc ( Closure : Worker . ClosureTyp )  
  RAISES { AssertionFailure , Thread . Alerted }   
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)  
 
  = VAR LCommandString : TEXT 

  ; BEGIN 
      EVAL Ui . SetImageTrans ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . MergeText ) 
    ; TextEdit . FlushEdit ( Closure . ImageTrans ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END MergeTextWorkProc  

(* VISIBLE: *) 
; PROCEDURE ReplayMergeText ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := MergeTextWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => (* Discard. *) 
      END (* TRY EXCEPT *) 
    END ReplayMergeText 

; PROCEDURE MergeTextCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      EVAL Worker . RequestWorkInteractive  
             ( NEW ( Worker . ClosureTyp 
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , apply := MergeTextWorkProc  
                   ) 
             ) 
    END MergeTextCallback  

(* Browse Est. This has its own thread. *) 

; PROCEDURE BrowseThread ( Self : BrowserClosure ) : REFANY 

  = VAR LSession : TreeBrowse . T 
  ; VAR LTypescript : TypescriptVBT . T 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LTypescript := FormsVBT . GetVBT ( Self . Form , "Br_Typescript" ) 
    ; UiRecPlay . Record ( UiRecPlay . CommandTyp . BrowseEst ) 
(* CHECK: Do we really want to record this? *) 
    ; LSession 
        := NEW ( TreeBrowse . T ) . initSessionRdWr  
              ( Self . Window . WrImageRef . ItPers . IpEstRoot   
              , Self . Window . WrImageRef . ItPers . IpLang 
              , TypescriptVBT . GetRd ( LTypescript ) 
              , TypescriptVBT . GetWr ( LTypescript ) 
              ) 
    ; TRY 
        TreeBrowse . Interp ( LSession ) 
      EXCEPT AssertionFailure => (* Disregard. *) 
      END (* TRY EXCEPT *) 
    ; Trestle . Delete ( Self . Form ) 
    ; RETURN NIL 
    END BrowseThread  

; TYPE BrowserClosure 
    = Thread . Closure 
        OBJECT 
          Window : EditWindow . T 
        ; Form : FormsVBT . T 
        OVERRIDES 
          apply := BrowseThread 
        END (* OBJECT *)

(* VISIBLE: *)
; PROCEDURE ReplayBrowseEst ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      FormsVBT . MakeEvent 
        ( Options . MainForm 
        , "Fv_Devel_BrowseEst"                 
        , NullTime 
        )
    END ReplayBrowseEst 

; PROCEDURE BrowseEstCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LForm : FormsVBT . T 
  ; VAR LWindow : EditWindow . T 
  ; VAR LThread : Thread . T 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )  
    ; IF LWindow . WrImageRef # NIL 
      THEN
	TRY 
	  LForm 
	    := NEW ( FormsVBT . T ) 
	       . initFromRsrc ( "Browse.fv" , Options . ResourcePath )
	EXCEPT 
	  Rsrc . NotFound , Rd . Failure 
	  => FormsVBT . PutText 
	       ( Options . MainForm , "Fv_ErrorPopup_Message" 
	       , "Unable to locate resource Browse.fv" 
	       ) 
	  ; FormsVBT . PopUp ( Options . MainForm , "Fv_ErrorPopup" ) 
	  ; RETURN 
	| Thread . Alerted 
	  => RETURN 
	END 
      ; TRY
	  Trestle . Install ( LForm ) 
	EXCEPT
	| TrestleComm . Failure 
	  => FormsVBT . PutText 
	       ( Options . MainForm , "Fv_ErrorPopup_Message" 
	       , "Could not open Browser window on display " 
                 & Options . Display 
	       ) 
	  ; FormsVBT . PopUp ( Options . MainForm , "Fv_ErrorPopup" ) 
	  ; RETURN 
	END 
      ; LThread 
	  := Thread . Fork 
	       ( NEW ( BrowserClosure , Window := LWindow , Form := LForm ) ) 
      END 
    END BrowseEstCallback 

(* Set debug level. No worker thread needed. *) 

(* VISIBLE: *) 
; PROCEDURE ShowDebugOptions ( ) 

  = <* FATAL FormsVBT . Error *> 
    BEGIN 
      IF Options . AllowProceedAfterAssert 
      THEN
        FormsVBT . MakeActive ( Options . MainForm , "Fv_Assert_Proceed" ) 
      ELSE 
        FormsVBT . MakeDormant ( Options . MainForm , "Fv_Assert_Proceed" ) 
      END (* IF *) 
    ; IF Options . AllowTerminateAfterAssert 
      THEN
        FormsVBT . MakeActive ( Options . MainForm , "Fv_Assert_Terminate" ) 
      ELSE 
        FormsVBT . MakeDormant ( Options . MainForm , "Fv_Assert_Terminate" ) 
      END (* IF *) 
    END ShowDebugOptions 

(* VISIBLE: *)
; PROCEDURE ReplaySetDebugLevel ( Level : INTEGER ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      FormsVBT . PutInteger 
        ( Options . MainForm , "Fv_Devel_DebugLevelValue" , Level )  
    ; FormsVBT . MakeEvent 
        ( Options . MainForm 
        , "Fv_Devel_DebugLevelApply"
        , NullTime 
        )
    END ReplaySetDebugLevel  

; PROCEDURE SetDebugLevelCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 
  (* Called back when "apply" is clicked.  Called indirectly from 
     SetDebugLevelOKCallback. 
  *) 

  = VAR LValue : INTEGER 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LValue := FormsVBT . GetInteger ( Form , "Fv_Devel_DebugLevelValue" ) 
    ; Options . DebugLevel 
        := MAX ( FIRST ( Options . DebugLevelTyp ) 
               , MIN ( LAST ( Options . DebugLevelTyp ) 
                     , LValue 
                     ) 
               ) 
    ; Options . SetDerivedDebugOptions ( ) 
    ; ShowDebugOptions ( ) 
    ; UiRecPlay . RecordPlusInt 
        ( UiRecPlay . CommandTyp . SetDebugLevel , LValue ) 
    END SetDebugLevelCallback 

; PROCEDURE SetDebugLevelOKCallback 
    ( Form : FormsVBT . T 
    ; Name : TEXT 
    ; EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      SetDebugLevelCallback ( Form , Name , EventData , Time ) 
    ; FormsVBT . PopDown ( Form , "Fv_Devel_DebugLevelDialog" )
    END SetDebugLevelOKCallback 

(* Replays for write operations with dialogs and successors. *) 

(* Failure dialogs: *) 

(* VISIBLE: *) 
; PROCEDURE ShowGuiAssertDialog ( Location , Message : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN
      FormsVBT . PutText 
        ( Options . MainForm , "Fv_Assert_Location" , Location  ) 
    ; FormsVBT . PutText 
        ( Options . MainForm , "Fv_Assert_Message" , Message )
    ; FormsVBT . PopUp ( Options . MainForm , "Fv_AssertDialog" ) 
    ; FormsVBT . MakeDormant ( Options . MainForm , "Fv_Background" ) 
    END ShowGuiAssertDialog  

; PROCEDURE AssertBackoutProc 
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      Worker . ReportAssertDialog 
        ( Worker . FailureActionTyp . FaBackout ) 
    END AssertBackoutProc 

; PROCEDURE AssertProceedProc 
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      Worker . ReportAssertDialog 
        ( Worker . FailureActionTyp . FaProceed ) 
    END AssertProceedProc 

; PROCEDURE AssertTerminateProc 
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      Worker . ReportAssertDialog 
        ( Worker . FailureActionTyp . FaTerminate ) 
    END AssertTerminateProc 

(* VISIBLE: *) 
; PROCEDURE RemoveGuiAssertDialog ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN
      FormsVBT . MakeActive ( Options . MainForm , "Fv_Background" ) 
    ; FormsVBT . PopDown ( Options . MainForm , "Fv_AssertDialog" ) 
    END RemoveGuiAssertDialog  

(* VISIBLE: *) 
; PROCEDURE ShowCheckpointNotice ( Message: TEXT (* May be multiline. *) ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      FormsVBT . PutText 
        ( Options . MainForm , "Fv_CheckpointPopup_Message" , Message ) 
    ; FormsVBT . PopUp ( Options . MainForm , "Fv_CheckpointPopup" ) 
    (* Let the user close the popup. *) 
    END ShowCheckpointNotice

; PROCEDURE AttachButtonHandlers ( Form : FormsVBT . T ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN (* AttachButtonHandlers *) 
      FormsVBT . AttachProc 
        ( Form , "Fv_Assert_Backout" , AssertBackoutProc )
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Assert_Proceed" , AssertProceedProc )
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Assert_Terminate" , AssertTerminateProc )
    END AttachButtonHandlers 

; PROCEDURE AttachMenuHandlers ( Form : FormsVBT . T ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN (* AttachMenuHandlers *) 
      FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_WriteCheckpoint" 
        , WriteCheckpointCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Devel_TakeFocus" , TakeFocusCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Devel_Repaint" , RepaintCallback ) 
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_ReconstructLines" 
        , ReconstructLinesCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_VerifyLinesRefs" 
        , VerifyLinesRefsCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Devel_ForceAssert" , ForceAssertCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Devel_ReplayLast" , ReplayLastCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Devel_MergeText" , MergeTextCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Devel_BrowseEst" , BrowseEstCallback )
    ; FormsVBT . PutInteger (* Initial debug level. *) 
        ( Form 
        , "Fv_Devel_DebugLevelValue" 
        , Options . DebugLevel 
        )  
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_DebugLevelApply" 
        , SetDebugLevelCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_DebugLevelOK" 
        , SetDebugLevelOKCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Devel_WriteStats" , WriteStatsCallback ) 
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_WriteEstPickle" 
        , WriteEstPickleCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_GenEstModule" 
        , GenEstModuleCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_WriteParseInfo" 
        , WriteParseInfoCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_WriteFsTrees" 
        , WriteFsTreesCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_WriteSemPickle" 
        , WriteSemPickleCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_GenTokInterface" 
        , GenTokInterfaceCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form 
        , "Fv_Devel_GenChildInterface" 
        , GenChildInterfaceCallback 
        ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Devel_GenAll" , GenAllCallback ) 
    END AttachMenuHandlers 

(* VISIBLE: *) 
; PROCEDURE AttachHandlers ( Form : FormsVBT . T ) 

  = BEGIN 
      AttachMenuHandlers ( Form ) 
    ; AttachButtonHandlers ( Form ) 
    END AttachHandlers 

; BEGIN 
    Options . DevelWritePath 
      := "/home/rodney/proj/lbe/working/boot"
         (* Pathname . Current *)  
  END UiDevel 
. 
