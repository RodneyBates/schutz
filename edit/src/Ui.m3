
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Ui 

(* Library: *) 
; IMPORT Font 
; IMPORT FormsVBT 
; IMPORT Pathname 
; IMPORT Rd  
; IMPORT Rsrc 
; IMPORT Text 
; IMPORT TextRefTbl 
; IMPORT Thread 
; IMPORT Trestle 
; IMPORT TrestleComm 
; IMPORT VBT 
; IMPORT Wr 

(* Application: *) 
; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout   
; IMPORT Display 
; IMPORT EditWindow 
; IMPORT EstUtil  
; IMPORT Files 
; IMPORT Images 
; IMPORT LbeStd  
; IMPORT LdlSemantics 
; IMPORT MessageCodes 
; IMPORT Messages 
; IMPORT Misc 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT ParseHs 
; IMPORT PortTypes 
; IMPORT Selection 
; IMPORT ScannerIf 
; IMPORT Strings 
; IMPORT TextEdit 
; IMPORT UiDevel 
; IMPORT UiRecPlay 
; IMPORT UiSearch 
; IMPORT VersionedFiles 
; IMPORT Worker 
; IMPORT WriteTrv 

<* PRAGMA LL *> 

; TYPE AFT = MessageCodes . T 

; CONST DL = Messages . StdErrLine 

; PROCEDURE InitForm ( Form : FormsVBT . T ) 
  RAISES { Backout } 

  = VAR LWindow : EditWindow . T  
  ; VAR LFont : Font . T 
  ; VAR LFvFontName 
           := FormsVBT . GetTextProperty 
               ( Form , "Fv_Window" , "Font" ) 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LFont := Font . FromName 
                 ( ARRAY OF TEXT 
                    { (* "-*-courier-medium-r-*-*-*-140-*-*-*-*-*-*" *)
                      LFvFontName 
                    }
                 , useXft := FALSE
                 ) 
    ; LWindow := EditWindow . Init 
        ( NEW ( EditWindow . T ) 
        , Form := Form 
        , Font := LFont 
        ) 
    ; FormsVBT . PutGeneric ( Form , "Fv_LbeWindow" , LWindow ) 
    ; FormsVBT . PutInteger ( Form , "Fv_Modified" , 0 ) 
    ; FormsVBT . PutInteger ( Form , "Fv_Unparsed" , 0 ) 
    ; FormsVBT . PutInteger ( Form , "Fv_Unanalyzed" , 0 ) 
    ; AttachFileMenuHandlers ( Form ) 
    ; AttachEditMenuHandlers ( Form ) 
    ; AttachSemMenuHandlers ( Form ) 
    ; AttachOtherHandlers ( Form ) 
    ; UiDevel . AttachHandlers ( Form ) 
    ; UiRecPlay . AttachHandlers ( Form ) 
    END InitForm 

(* Some utility closures: *) 

; TYPE WorkerClosureTextTyp
    = Worker . ClosureTyp OBJECT TextParam : TEXT END 

; TYPE WorkerClosureTextIntTyp
    = WorkerClosureTextTyp OBJECT IntParam : PortTypes . Int32Typ END 

(* Convenience procedures for setting standard fields of closures. *) 

(* VISIBLE: *) 
; PROCEDURE SetImageTrans ( Closure : Worker . ClosureTyp ) 
  : Worker . ClosureTyp 
  (* PRE: ImageTrans is set, set ImagePers from it. *) 

  = BEGIN 
      IF Closure . Window # NIL 
      THEN 
        Closure . ImageTrans := Closure . Window . WrImageRef  
      END (* IF *) 
    ; RETURN Closure  
    END SetImageTrans 

(* VISIBLE: *) 
; PROCEDURE SetImagePers ( Closure : Worker . ClosureTyp ) 
  : Worker . ClosureTyp 
  (* PRE: ImageTrans is set, set ImagePers from it. *) 

  = BEGIN 
      IF Closure . ImageTrans # NIL 
      THEN 
        Closure . ImagePers := Closure . ImageTrans . ItPers 
      END (* IF *) 
    ; RETURN Closure  
    END SetImagePers 

(* VISIBLE: *) 
; PROCEDURE SetImageTransAndPers ( Closure : Worker . ClosureTyp ) 
  : Worker . ClosureTyp 
  (* PRE: Window is set, set ImageTrans and ImagePers from it. *) 

  = BEGIN 
      IF Closure . Window # NIL 
      THEN 
        Closure . ImageTrans := Closure . Window . WrImageRef 
      ; IF Closure . ImageTrans # NIL 
        THEN 
          Closure . ImagePers := Closure . ImageTrans . ItPers 
        END (* IF *) 
      END (* IF *)
    ; RETURN Closure  
    END SetImageTransAndPers 

(******************************** Open ***********************************) 

; PROCEDURE OpenEmptyFile 
    ( ImageName : TEXT ; AbsTextFileName : TEXT ; AbsPickleFileName : TEXT ) 
  : PaintHs . ImageTransientTyp 
  RAISES { Backout , Files . Error , Thread . Alerted } 
  (* Runs on worker thread. *) 

  = VAR LImageRef : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 

  ; BEGIN 
      LImageRef := Files . OpenEmptyFile ( ImageName ) 
    ; IF LImageRef # NIL 
      THEN 
        LImagePers := LImageRef . ItPers
      ; LImagePers . IpImageName := ImageName 
      ; LImagePers . IpAbsTextFileName := AbsTextFileName 
      ; LImagePers . IpAbsPklFileName := AbsPickleFileName 
      ; Files . WriteImagePickle 
          ( Images . PersistentImageToSave ( LImageRef , ForSave := TRUE )
          , AbsPickleFileName 
          , DoCreateVersion := TRUE 
          )
      ; LImageRef . ItIsSaved := TRUE 
      ; EVAL Images . ImageTable . put 
               ( LImagePers . IpImageName , LImageRef ) 
      END (* IF *) 
    ; RETURN LImageRef 
    END OpenEmptyFile 

; PROCEDURE OpenWorkProc ( Self : WorkerClosureTextTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Self . Window is set. Self . TextParam is file name. *) 
  (* Runs on worker thread. *) 

  = VAR LForm : FormsVBT . T 
  ; VAR LSimpleName : TEXT 
  ; VAR LImageName : TEXT 
  ; VAR LAbsFileName : TEXT 
  ; VAR LAbsTextFileName : TEXT 
  ; VAR LAbsPickleFileName : TEXT 
  ; VAR LRef : REFANY 
  ; VAR LCommandString : TEXT 
  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>

    BEGIN (* OpenWork *) 
      TRY (* EXCEPT *) 
        LForm := EditWindow . Form ( Self . Window ) 
      ; LSimpleName := Pathname . Last ( Self . TextParam ) 
      ; LImageName := Misc . TextName ( LSimpleName ) 
      ; LAbsFileName := Misc . AbsFileName ( Self . TextParam ) 
      ; IF Images . ImageTable . get ( LImageName , LRef ) 
        THEN (* It's already loaded. *) 
          Self . ImageTrans := LRef 
        ; Self . ImagePers := Self . ImageTrans . ItPers 
        ; Assert 
            ( Text . Equal ( Self . ImagePers . IpImageName , LImageName ) 
            , AFT . A_Files_InnerOpen_ImageNameMismatch 
            ) 
        ; IF LAbsFileName = NIL 
             OR NOT Text . Equal 
                      ( Self . ImagePers . IpAbsPklFileName 
                      , Misc . PickleName ( LAbsFileName ) 
                      ) 
(* CHECK: ^Is this really the right criterion, when it is a text file? *)  
          THEN (* But the loaded version has a different file path. *) 
            RAISE Files . Error 
              ( "Already open as " & Self . ImagePers . IpAbsPklFileName ) 
          ELSE 
            FormsVBT . MakeDormant ( LForm , "Fv_File_Open" ) 
          ; IF Self . ImageTrans . ItPers . IpLang = LbeStd . LangLdl0 
               OR Self . ImageTrans . ItPers . IpLang = LbeStd . LangLdl1 
            THEN 
              Options . DevelWritePath 
                := Pathname . Prefix 
                     ( Misc . AbsFileName ( Self . TextParam ) )  
            END (* IF *) 
          ; RETURN 
          END (* IF *) 
        ELSE (* Must read or create. *) 
          IF Misc . IsPickleOrCheckpointName ( Self . TextParam )  
          THEN 
            LAbsTextFileName := Misc . TextName ( LAbsFileName ) 
          ; LAbsPickleFileName := Misc . PickleName ( LAbsFileName )  
          ; IF Files . RegularFileExists ( LAbsFileName ) 
            THEN 
              LCommandString 
                := UiRecPlay . BeginCommandPlusString 
                     ( UiRecPlay . CommandTyp . FileOpen 
                     , Misc . RelFileName 
                         ( Self . TextParam 
                         , Pathname . Prefix ( Options . RecordFileName ) 
                         )   
                     ) 
            ; Self . ImageTrans 
                := Files . ReadNamedImageFile ( LAbsFileName ) 
            ; Self . ImagePers := Self . ImageTrans . ItPers 
            ; Self . ImagePers . IpImageName := LImageName 
(* TODO: Check/handle the case where previously existing name or path
         has changed. 
*) 
            ; Self . ImagePers . IpAbsTextFileName := LAbsTextFileName  
            ; Self . ImagePers . IpAbsPklFileName := LAbsPickleFileName   
         (* ; Self . ImagePers . IpLineHeaderRef := NIL *) 
            ; Self . ImageTrans . ItWindowList := NIL 
            ; Self . ImageTrans . ItVisibleIn := PaintHs . WindowNoSetEmpty 
            ; Self . ImageTrans . ItScannerIf := NIL 
            ; Self . ImageTrans . ItIsSaved := TRUE  
            ; EVAL Images . ImageTable . put 
                     ( Self . ImagePers . IpImageName , Self . ImageTrans ) 
            ; UiRecPlay . RecordString ( LCommandString ) 
            ELSIF Files . RegularFileExists ( LAbsTextFileName ) 
            THEN  
              IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
            ; FormsVBT . PutText 
                ( LForm 
                , "Fv_AltOpenDialog_FirstFileName" 
                , LAbsPickleFileName  
                ) 
            ; FormsVBT . PutText 
                ( LForm 
                , "Fv_AltOpenDialog_SecondFileName" 
                , LAbsTextFileName 
                ) 
            ; FormsVBT . PopUp ( LForm , "Fv_AltOpenDialog" ) 
            ELSE 
              LCommandString 
                := UiRecPlay . BeginCommandPlusString 
                     ( UiRecPlay . CommandTyp . FileOpen 
                     , Misc . RelFileName 
                         ( Self . TextParam 
                         , Pathname . Prefix ( Options . RecordFileName ) 
                         )
                     ) 
            ; Self . ImageTrans 
                := OpenEmptyFile 
                     ( LImageName  , LAbsTextFileName , LAbsPickleFileName )
            ; UiRecPlay . RecordString ( LCommandString ) 
            END (* IF *) 
          ELSE (* It's a text file name. *) 
            LAbsTextFileName := LAbsFileName  
          ; LAbsPickleFileName := Misc . PickleName ( LAbsFileName )  
          ; IF Files . RegularFileExists ( LAbsTextFileName ) 
            THEN 
              LCommandString 
                := UiRecPlay . BeginCommandPlusString 
                     ( UiRecPlay . CommandTyp . FileOpen 
                     , Misc . RelFileName 
                         ( Self . TextParam 
                         , Pathname . Prefix ( Options . RecordFileName ) 
                         )
                     ) 
            ; Self . ImageTrans 
               := Files . OpenNamedTextFile ( LAbsTextFileName ) 
            ; Self . ImagePers := Self . ImageTrans . ItPers 
            ; Self . ImagePers . IpImageName := LImageName 
            ; Files . WriteImagePickle 
                ( Images . PersistentImageToSave 
                    ( Self . ImageTrans , ForSave := TRUE ) 
                , LAbsPickleFileName 
                , DoCreateVersion := TRUE 
                )  
            ; Self . ImageTrans . ItIsSaved := TRUE 
            ; EVAL Images . ImageTable . put 
                     ( Self . ImagePers . IpImageName , Self . ImageTrans ) 
            ; UiRecPlay . RecordString ( LCommandString ) 
            ELSIF Files . RegularFileExists ( LAbsPickleFileName ) 
              THEN  
                IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
              ; FormsVBT . PutText 
                  ( LForm 
                  , "Fv_AltOpenDialog_FirstFileName" 
                  , LAbsTextFileName 
                  ) 
              ; FormsVBT . PutText 
                  ( LForm 
                  , "Fv_AltOpenDialog_SecondFileName" 
                  , LAbsPickleFileName 
                  ) 
              ; FormsVBT . PopUp ( LForm , "Fv_AltOpenDialog" ) 
            ELSE 
              LCommandString  
                := UiRecPlay . BeginCommandPlusString 
                     ( UiRecPlay . CommandTyp . FileOpen 
                     , Misc . RelFileName 
                         ( Self . TextParam 
                         , Pathname . Prefix ( Options . RecordFileName ) 
                         )
                     ) 
            ; Self . ImageTrans 
                := OpenEmptyFile 
                     ( LImageName  , LAbsTextFileName , LAbsPickleFileName )
            ; UiRecPlay . RecordString ( LCommandString ) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      ; IF Self . ImageTrans # NIL 
        THEN
          Images . ConnectImageToWindow ( Self . ImageTrans , Self . Window ) 
        ; Display . InitImageFirstWindow ( Self . ImageTrans ) 
        ; FormsVBT . MakeDormant ( LForm , "Fv_File_Open" ) 
        ; IF Self . ImageTrans . ItPers . IpLang = LbeStd . LangLdl0 
             OR Self . ImageTrans . ItPers . IpLang = LbeStd . LangLdl1 
          THEN 
            Options . DevelWritePath 
              := Pathname . Prefix 
                   ( Misc . AbsFileName ( Self . TextParam ) )  
          END (* IF *) 
        END (* IF *) 
      EXCEPT
      Files . Error ( EMessage ) 
      => IF Worker . DoGuiActions ( ) 
        THEN 
          IF Thread . TestAlert ( )    
          THEN 
            FormsVBT . MakeActive ( LForm , "Fv_File_Open" ) 
          ; Options . OpeningImageRef := NIL 
          ; RAISE Thread . Alerted 
          ELSE 
            FormsVBT . PutText ( LForm , "Fv_ErrorPopup_Message" , EMessage ) 
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          (* Leave the Open dialog up and let the user either change
             something and try again, or explicitly cancel. *) 
          END (* IF *) 
        ELSE  
          Assertions . MessageText ( EMessage ) 
        ; Options . OpeningImageRef := NIL 
        ; RAISE Backout ( EMessage ) 
        END (* IF *) 

      | Backout ( EMessage )  
      => IF Worker . DoGuiActions ( ) 
        THEN 
          IF NOT Thread . TestAlert ( )    
          THEN 
            FormsVBT . MakeActive ( LForm , "Fv_File_Open" ) 
          ; Options . OpeningImageRef := NIL 
          ; RAISE Thread . Alerted 
          ELSE 
            FormsVBT . PutText ( LForm , "Fv_ErrorPopup_Message" , EMessage ) 
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          (* Leave the Open dialog up and let the user either change
             something and try again, or explicitly cancel. *)  
          END (* IF *) 
        ELSE 
(* CHECK: Presumably, the EMessage was already displayed in this case. *) 
          Options . OpeningImageRef := NIL 
        ; RAISE Backout ( EMessage ) 
        END (* IF *) 

      | Thread . Alerted 
      => FormsVBT . MakeActive ( LForm , "Fv_File_Open" ) 
      ; Options . OpeningImageRef := NIL 
      ; RAISE Thread . Alerted 

      | Images . NoMoreWindows 
      => 
(* TOTO: Pop a dialog here. *) 
      END (* TRY EXCEPT *) 
    ; Options . OpeningImageRef := NIL 
    END OpenWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayFileOpen ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      IF FileName # NIL AND NOT Text . Equal ( FileName , "" ) 
      THEN
        TRY 
          EVAL Worker . RequestWork 
                 ( NEW ( WorkerClosureTextTyp 
                       , Window 
                           := FormsVBT . GetGeneric 
                                ( Options . MainForm , "Fv_LbeWindow" )   
                       , TextParam := FileName 
                       , apply := OpenWorkProc 
                       ) 
                 ) 
        ; TRY 
            FormsVBT . PutText 
              ( Options . MainForm , "Fv_OpenDialog_FileName" , FileName )
          EXCEPT ELSE 
          END (* TRY EXCEPT *) 
        EXCEPT Thread . Alerted => 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END ReplayFileOpen 

; PROCEDURE OpenCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 
  ; VAR LWindow : EditWindow . T 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LFileName := FormsVBT . GetText ( Form , "Fv_OpenDialog_FileName" )
    ; IF LFileName # NIL AND NOT Text . Equal ( LFileName , "" ) 
      THEN 
        LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" ) 
      ; EditWindow . TakeKBFocus ( LWindow , Time ) 
      ; EVAL Worker . RequestWorkInteractive  
               ( NEW ( WorkerClosureTextTyp 
                     , Window := LWindow 
                     , Time := Time 
                     , TextParam := LFileName 
                     , apply := OpenWorkProc 
                     ) 
               ) 
      ; FormsVBT . PopDown ( Form , "Fv_OpenDialog" )
      END (* IF *) 
    END OpenCallback 

; PROCEDURE AltOpenYesCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LFileName 
        := FormsVBT . GetText ( Form , "Fv_AltOpenDialog_SecondFileName" )
    ; IF LFileName # NIL AND NOT Text . Equal ( LFileName , "" ) 
      THEN 
        EVAL Worker . RequestWorkInteractive  
               ( NEW ( WorkerClosureTextTyp 
                     , Window 
                         := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                     , Time := Time 
                     , TextParam := LFileName 
                     , apply := OpenWorkProc 
                     ) 
               ) 
      ; FormsVBT . PopDown ( Form , "Fv_AltOpenDialog" )
      ; FormsVBT . PopDown ( Form , "Fv_OpenDialog" )
      END (* IF *) 
    END AltOpenYesCallback 

; PROCEDURE AltOpenNoWorkProc ( Self : WorkerClosureTextTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Self . Window is set. Self . TextParam is file name. *) 
  (* Runs on worker thread. *) 

  = VAR LSimpleName : TEXT 
  ; VAR LImageName : TEXT 
  ; VAR LAbsFileName : TEXT 
  ; VAR LAbsTextFileName : TEXT 
  ; VAR LAbsPickleFileName : TEXT 
  ; VAR LCommandString : TEXT 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LSimpleName := Pathname . Last ( Self . TextParam ) 
    ; LImageName := Misc . TextName ( LSimpleName ) 
    ; LAbsFileName := Misc . AbsFileName ( Self . TextParam ) 
    ; LAbsTextFileName := Misc . TextName ( LAbsFileName ) 
    ; LAbsPickleFileName := Misc . PickleName ( LAbsFileName )  
    ; TRY 
        Self . ImageTrans 
          := OpenEmptyFile 
               ( LImageName  , LAbsTextFileName , LAbsPickleFileName )  
      EXCEPT Files . Error => Self . ImageTrans := NIL 
      END (* TRY EXCEPT *) 
    ; IF Self . ImageTrans # NIL 
      THEN
        LCommandString 
          := UiRecPlay . BeginCommandPlusString 
               ( UiRecPlay . CommandTyp . FileOpen 
               , Misc . RelFileName 
                   ( Self . TextParam 
                   , Pathname . Prefix ( Options . RecordFileName ) 
                   )
               ) 
      ; TRY
          Images . ConnectImageToWindow ( Self . ImageTrans , Self . Window ) 
        ; Display . InitImageFirstWindow ( Self . ImageTrans ) 
        ; FormsVBT . MakeDormant 
            ( EditWindow . Form ( Self . Window ) , "Fv_File_Open" ) 
        ; IF Self . ImageTrans . ItPers . IpLang = LbeStd . LangLdl0 
             OR Self . ImageTrans . ItPers . IpLang = LbeStd . LangLdl1 
          THEN 
            Options . DevelWritePath 
              := Pathname . Prefix ( Misc . AbsFileName ( Self . TextParam ) )
          END (* IF *) 
        ; UiRecPlay . RecordString ( LCommandString ) 
        EXCEPT Images . NoMoreWindows 
        => 
(* TOTO: Pop a dialog. *) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END AltOpenNoWorkProc 

; PROCEDURE AltOpenNoCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LFileName 
        := FormsVBT . GetText 
             ( Form , "Fv_AltOpenDialog_FirstFileName" ) 
    ; IF LFileName # NIL AND NOT Text . Equal ( LFileName , "" ) 
      THEN 
        EVAL Worker . RequestWorkInteractive  
               ( NEW ( WorkerClosureTextTyp 
                     , Window 
                         := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                     , Time := Time 
                     , TextParam := LFileName 
                     , apply := AltOpenNoWorkProc 
                     ) 
               ) 
      ; FormsVBT . PopDown ( Form , "Fv_AltOpenDialog" )
      ; FormsVBT . PopDown ( Form , "Fv_OpenDialog" )
      END (* IF *) 
    END AltOpenNoCallback 

; PROCEDURE AltOpenCancelCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      FormsVBT . PopDown ( Form , "Fv_AltOpenDialog" )
    ; FormsVBT . PopDown ( Form , "Fv_OpenDialog" )
    END AltOpenCancelCallback 

(******************************** Save ***********************************) 

; PROCEDURE InnerSave ( Closure : Worker . ClosureTyp ) 
  : BOOLEAN (* Success *) 
  (* PRE: Closure . Window, ImageTrans, and ImagePers set. *) 
  (* Runs on worker thread. *) 

  = VAR LForm : FormsVBT . T 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; IF Closure . ImageTrans # NIL 
      THEN 
        TRY 
          Files . WriteImagePickle 
            ( Images . PersistentImageToSave 
                ( Closure . ImageTrans , ForSave := TRUE )
            , Closure . ImagePers . IpAbsPklFileName 
            , DoCreateVersion := TRUE 
            )  
        ; Closure . ImageTrans . ItIsSaved := TRUE  
        ; Display . NoteImageSavedState 
            ( Closure . ImageTrans , Closure . ImageTrans . ItIsSaved ) 
        EXCEPT
          Files . Error ( EMessage ) 
        => IF Closure . IsInteractive 
           THEN 
             FormsVBT . PutText 
               ( LForm , "Fv_ErrorPopup_Message" , EMessage ) 
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          (* Leave the Save dialog up and let the user either change
             something and try again, or explicitly cancel. *)  
          END (* IF *) 
        ; RETURN FALSE 
        ELSE 
          IF Closure . IsInteractive 
          THEN 
             FormsVBT . PutText 
               ( LForm 
               , "Fv_ErrorPopup_Message" 
               , "Save failed for " 
                 & Closure . ImagePers . IpAbsPklFileName 
               ) 
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          (* Leave the Save dialog up and let the user either change
             something and try again, or explicitly cancel. *)  
          END (* IF *) 
        ; RETURN FALSE 
        END (* TRY EXCEPT *) 
      ; RETURN TRUE 
      ELSE RETURN FALSE 
      END (* IF *) 
    END InnerSave  

; PROCEDURE FileSaveWorkProc ( Closure : Worker . ClosureTyp ) 
  (* PRE: Closure . Window and ImageTrans set by caller. *) 
  (* Runs on worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      EVAL SetImagePers ( Closure ) 
    ; LCommandString 
        := UiRecPlay . BeginCommandPlusString 
              ( UiRecPlay . CommandTyp . FileSave
              , Closure . ImagePers . IpImageName  
              ) 
    ; IF InnerSave ( Closure ) 
      THEN 
        UiRecPlay . RecordString ( LCommandString ) 
      END (* IF *)  
    END FileSaveWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayFileSave ( RecordedName : TEXT ) 

  = VAR LImageName : TEXT 
  ; VAR LRef : REFANY 
  ; VAR LImageTrans : PaintHs . ImageTransientTyp 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LImageName := Pathname . Last ( Misc . TextName ( RecordedName ) )  
    ; IF Images . ImageTable . get ( LImageName , LRef ) 
      THEN 
        LImageTrans := LRef 
      ; TRY 
          EVAL Worker . RequestWork 
                 ( NEW ( Worker . ClosureTyp 
                       , Window 
                           := FormsVBT . GetGeneric 
                                ( Options . MainForm , "Fv_LbeWindow" )   
                       , ImageTrans := LImageTrans 
                       , apply := FileSaveWorkProc 
                       ) 
                 ) 
        EXCEPT Thread . Alerted => 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END ReplayFileSave 

; PROCEDURE SaveCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LWindow : EditWindow . T 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )  
    ; IF LWindow # NIL AND LWindow . WrImageRef # NIL  
      THEN 
        EVAL Worker . RequestWorkInteractive  
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                     , Window := LWindow 
                     , ImageTrans := LWindow . WrImageRef 
                     , Time := Time 
                     , apply := FileSaveWorkProc 
                     ) 
               ) 
      END (* IF *) 
    END SaveCallback  

; PROCEDURE SaveAndExportWorkProc ( Closure : WorkerClosureTextTyp ) 
  (* PRE: Closure . Window, ImageTrans, and ImagePers are set. *) 
  (* PRE: TextParam is file name. *) 
  (* Runs on worker thread. *) 

  = BEGIN 
      FileSaveWorkProc ( Closure ) 
    ; TRY 
        EVAL InnerExportOK ( Closure ) 
      EXCEPT Thread . Alerted => (* Ignore. *) 
      | Backout => (* Already handled. *) 
      END (* TRY EXCEPT *) 
    END SaveAndExportWorkProc 

; PROCEDURE SaveAndExportCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LWindow : EditWindow . T 
  ; VAR LImageTrans : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )  
    ; IF LWindow # NIL 
      THEN 
        LImageTrans := LWindow . WrImageRef 
      ; IF LImageTrans # NIL 
        THEN 
          LImagePers := LImageTrans . ItPers 
        ; EVAL Worker . RequestWorkInteractive  
                 ( NEW ( WorkerClosureTextTyp 
                       , Window := LWindow 
                       , ImageTrans := LWindow . WrImageRef 
                       , TextParam := LImagePers . IpAbsTextFileName 
                       , Time := Time 
                       , apply := SaveAndExportWorkProc 
                       ) 
                 ) 
        END (* IF*) 
      END (* IF *) 
    END SaveAndExportCallback  

(***************************** Save dialog *******************************) 

; TYPE SaveDialogInfoTyp 
    = RECORD 
        QuitAfter : BOOLEAN := FALSE 
      ; UnsavedImagesExist : BOOLEAN := FALSE 
      ; DoCloseAllWindows : BOOLEAN := FALSE 
      ; DoCloseAllImages : BOOLEAN := FALSE 
      ; IsPoppedUp : BOOLEAN := FALSE 
      END (* RECORD *) 

; VAR SaveDialogInfo : SaveDialogInfoTyp 
  (* This is only accessed by code running on the worker thread, and is thus
     serialized. 
  *) 

; PROCEDURE SaveDialogDisconnect ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout } 
  (* Disconnect an image that is to be closed, but first had a save prompt
     done for it. 
  *) 
  (* PRE: Closure . Window, ImageTrans, ImagePers are set. *) 
  (* Runs on worker thread. *) 

  = VAR LForm : FormsVBT . T 
  ; VAR LCommandString : TEXT 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; IF SaveDialogInfo . DoCloseAllWindows 
      THEN (* Only prompt fonce, even if it is in multiple windows. *) 
        LCommandString 
          := UiRecPlay . BeginCommandPlusString  
               ( UiRecPlay . CommandTyp . FileCloseImage 
               , Closure . ImagePers . IpImageName
               ) 
      ; Images . DisconnectImageFromAllWindows ( Closure . ImageTrans )
      ; Images . DiscardImage ( Closure . ImageTrans )  
      ; FormsVBT . MakeActive ( LForm , "Fv_File_Open" ) 
      ; UiRecPlay . RecordString ( LCommandString ) 
      ELSE 
         LCommandString 
           := UiRecPlay . BeginCommandPlusStringInt 
                ( UiRecPlay . CommandTyp . FileCloseWindow 
                , Closure . ImagePers . IpImageName
                , Closure . Window . WrWindowNo 
                ) 
      ; Images . DisconnectImageFromWindow 
          ( Closure . ImageTrans , Closure . Window ) 
      ; IF Closure . ImageTrans . ItWindowList = NIL 
        THEN 
          Images . DiscardImage ( Closure . ImageTrans )  
        ; FormsVBT . MakeActive ( LForm , "Fv_File_Open" ) 
        END (* IF *) 
      ; UiRecPlay . RecordString ( LCommandString ) 
      END 
(* TODO: Decide if we want to close the windows too. *) 
    END SaveDialogDisconnect 

(* VISIBLE: *) 
; PROCEDURE PromptAndCloseAllImages 
    ( Closure : Worker . ClosureTyp ; QuitAfter : BOOLEAN ) 
  RAISES { Backout , Thread . Alerted }
  (* PRE: Closure . Window only is set. Image is chosen inside. *)  
  (* Runs on worker thread. *) 

  = VAR LImageRef : PaintHs . ImageTransientTyp 

  ; BEGIN (* PromptAndCloseAllImages *) 
      LImageRef := ExtractAnImage ( ) 
    ; IF LImageRef = NIL 
      THEN (* No images remain open. *) 
        IF QuitAfter 
        THEN 
          UiRecPlay . Record ( UiRecPlay . CommandTyp . FileQuit ) 
        ; UiRecPlay . RecordClose ( ) 
        ; ReallyQuit ( ) 
        END (* IF *) 
      ELSE 
        Closure . ImageTrans := LImageRef 
      ; EVAL SetImagePers ( Closure ) 
      ; PromptForSave 
          ( Closure  
          , QuitAfter := QuitAfter
          , DoCloseAllImages := TRUE 
          , DoCloseAllWindows := TRUE 
          )  
      END (* IF *) 
    END PromptAndCloseAllImages  

; PROCEDURE PromptForSave
    ( Closure : Worker . ClosureTyp 
    ; QuitAfter : BOOLEAN 
    ; DoCloseAllImages : BOOLEAN  
    ; DoCloseAllWindows : BOOLEAN  
    ) 
  RAISES { Backout } 
  (* PRE: Closure . Window, ImageTrans, ImagePers are set. *) 
  (* Runs on worker thread. *) 

  = VAR LForm : FormsVBT . T 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN (* PromptForSave *) 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; IF Closure . ImageTrans # NIL 
      THEN 
        SaveDialogInfo . QuitAfter := QuitAfter 
      ; SaveDialogInfo . UnsavedImagesExist := FALSE 
      ; SaveDialogInfo . DoCloseAllImages := DoCloseAllImages  
      ; SaveDialogInfo . DoCloseAllWindows := DoCloseAllWindows 
      ; IF Closure . ImageTrans . ItIsSaved 
        THEN (* Simulate an OK response *) 
          SaveDialogDisconnect ( Closure ) 
        ; IF SaveDialogInfo . DoCloseAllImages 
          THEN (* Do another image. *) 
            TRY 
              PromptAndCloseAllImages 
                ( Closure , SaveDialogInfo . QuitAfter ) 
            EXCEPT Thread . Alerted => (* Ignore. *) 
            END (* TRY EXCEPT *) 
          END (* IF *) 
        ELSE 
          FormsVBT . PutText 
            ( LForm 
            , "Fv_SaveDialog_Image" 
            , "Save modified image " 
              & Closure . ImagePers . IpImageName & "?" 
            ) 
        ; FormsVBT . PopUp ( LForm , "Fv_SaveDialog" ) 
        ; SaveDialogInfo . IsPoppedUp := TRUE 
     (* ; FormsVBT . MakeDormant ( LForm , "Fv_Background" ) 
          This makes the stop sign dormant too. 
     *) 
        END (* IF *) 
      END (* IF *) 
    END PromptForSave 

; PROCEDURE ExtractAnImage ( ) : PaintHs . ImageTransientTyp 
  (* Get an arbitrary Image from those that are open. 
     NIL if none exists. *) 

  = VAR LTblIterator : TextRefTbl . Iterator 
  ; VAR LName : TEXT 
  ; VAR LRef : REFANY 
  ; VAR LResult : PaintHs . ImageTransientTyp  
  ; VAR LFound : BOOLEAN 

  ; BEGIN (* ExtractAnImage *) 
      LTblIterator := Images . ImageTable . iterate ( ) 
    ; LOOP 
        LFound := LTblIterator . next ( LName , LRef ) 
      ; IF NOT LFound  
        THEN 
          LResult := NIL 
        ; LTblIterator := NIL (* Paranoia *) 
        ; EXIT 
        ELSIF LRef # NIL 
        THEN 
          LResult := LRef  
        ; LTblIterator := NIL (* Paranoia *) 
        ; EXIT 
        END (* IF *) 
      END (* LOOP *) 
    ; RETURN LResult 
    END ExtractAnImage 

; PROCEDURE ReallyQuit ( ) 
  (* Can run on an event callback thread, playback thread, 
     or the worker thread. 
  *) 

  = BEGIN 
      Assertions . TerminatingNormally := TRUE 
 (* ; UiRecPlay . RecordClose ( ) Deadlocks. *) 
    ; Trestle . Delete ( Options . MainForm ) 
    END ReallyQuit

; PROCEDURE SaveDialogWorkProc 
    ( Closure : WorkerClosureTextTyp (* TextParam is button name. *) ) 
  RAISES { Backout } 
  (* PRE: Closure . Window is set.  Closure . Textparm is button name. *) 
  (* Runs on worker thread. *) 

  = VAR LForm : FormsVBT . T 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; EVAL SetImageTransAndPers ( Closure ) 
    ; IF SaveDialogInfo . IsPoppedUp 
      THEN (* Bad playback sequences could cause it not to be. *) 
        IF Text . Equal ( Closure . TextParam , "Fv_SaveDialog_Cancel" ) 
        THEN 
          SaveDialogInfo . DoCloseAllWindows := FALSE 
        ; SaveDialogInfo . DoCloseAllImages := FALSE 
        ; SaveDialogInfo . QuitAfter := FALSE 
        ELSIF Text . Equal ( Closure . TextParam , "Fv_SaveDialog_Save" ) 
        THEN
          IF InnerSave ( Closure )  
          THEN 
            SaveDialogDisconnect ( Closure ) 
          END (* IF *) 
        ELSIF Text . Equal ( Closure . TextParam , "Fv_SaveDialog_Forget" ) 
        THEN 
          SaveDialogDisconnect ( Closure ) 
        ; SaveDialogInfo . UnsavedImagesExist := TRUE 
        ELSE 
          CantHappen ( AFT . A_Files_SaveDialogApply_BadComponentName ) 
        END (* IF *) 
      ; FormsVBT . MakeActive ( LForm , "Fv_Background" ) 
      ; FormsVBT . PopDown ( LForm , "Fv_SaveDialog" ) 
      ; SaveDialogInfo . IsPoppedUp := FALSE  
      ; IF SaveDialogInfo . DoCloseAllImages 
        THEN 
          TRY 
            PromptAndCloseAllImages 
              ( Closure , SaveDialogInfo . QuitAfter ) 
          EXCEPT Thread . Alerted => (* Ignore. *) 
          END (* TRY EXCEPT *) 
        END (* IF *) 
      END (* IF *) 
    END SaveDialogWorkProc 

; PROCEDURE SaveDialogCallback  
    ( Form : FormsVBT . T 
    ; Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 
  (* FormsVBT event callback, called for any response to the save dialog. *)

  = <* FATAL FormsVBT . Error *>
    BEGIN (* SaveDialogCallback *) 
      EVAL Worker . RequestWorkInteractive  
             ( NEW ( WorkerClosureTextTyp 
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )    
                   , Time := Time 
                   , TextParam := Name (* Of the button. *) 
                   , apply := SaveDialogWorkProc 
                   ) 
             ) 
    END SaveDialogCallback 

(******************************** SaveAs *********************************) 

; PROCEDURE SaveAsCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 
  (* Just pop the SaveAs dialog. *) 

  = VAR LWindow : EditWindow . T  

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LWindow 
        := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" ) 
    ; IF LWindow . WrImageRef # NIL 
      THEN 
        FormsVBT . PutText 
          ( Form 
          , "Fv_SaveAsDialog_FileName" 
          , LWindow . WrImageRef . ItPers . IpAbsPklFileName 
          )
      ; FormsVBT . PopUp ( Form , "Fv_SaveAsDialog" ) 
      END (* IF *) 
    END SaveAsCallback  

; PROCEDURE InnerSaveAsOK ( Closure : WorkerClosureTextTyp ) 
  : BOOLEAN (* Success *) 
  (* Handle the internal name change. *) 
  (* PRE: Closure . Window, ImageTrans, ImagePers are set,
          Closure . TextParam is new image name. 
  *) 
  (* Runs on worker thread. *) 

  = VAR LForm : FormsVBT . T 
  ; VAR LSimpleName : TEXT 
  ; VAR LImageName : TEXT 
  ; VAR LCommandString : TEXT := NIL 
  ; VAR LRef : REFANY  
  ; VAR LSuccess : BOOLEAN 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; IF Closure . TextParam # NIL 
         AND NOT Text . Equal ( Closure . TextParam , "" )  
         AND Closure . Window # NIL 
         AND Closure . ImageTrans # NIL 
      THEN 
        LCommandString 
          := UiRecPlay . BeginCommandPlusString 
               ( UiRecPlay . CommandTyp . FileSaveAs 
               , Misc . RelFileName 
                   ( Closure . TextParam 
                   , Pathname . Prefix ( Options . RecordFileName ) 
                   ) 
               ) 
      ; LSimpleName := Pathname . Last ( Closure . TextParam ) 
      ; LImageName := Misc . TextName ( LSimpleName ) 
      ; IF Text . Equal ( LImageName , Closure . ImagePers . IpImageName ) 
        THEN (* Image name is not changing. *) 
          Closure . ImagePers . IpAbsPklFileName := Closure . TextParam 
        ; FormsVBT . PutText 
            ( LForm , "Fv_PathName" , Closure . TextParam )
        ; LSuccess := TRUE 
        ELSE (* There is a change of image name. *) 
          IF Images . ImageTable . get ( LImageName , LRef ) 
          THEN (* An image by the new name already exists. *) 
            IF Closure . IsInteractive 
            THEN 
              FormsVBT . PutText 
                ( LForm 
                , "Fv_SaveAsDuplDialog_Name" 
                , LImageName 
                )
            ; FormsVBT . PopUp 
                ( LForm , "Fv_SaveAsDuplDialog" ) 
            END (* IF *) 
          ; LSuccess := FALSE 
          ELSE 
            EVAL Images . ImageTable . delete 
                   ( Closure . ImagePers . IpImageName , LRef ) 
          ; EVAL Images . ImageTable . put 
                   ( LImageName , Closure . ImageTrans ) 
          ; Closure . ImagePers . IpAbsPklFileName := Closure . TextParam 
          ; Closure . ImagePers . IpImageName := LImageName 
          ; FormsVBT . PutText 
              ( LForm , "Fv_PathName" , Closure . TextParam )
          ; FormsVBT . PutText 
              ( LForm , "Fv_ImageName" , LImageName )
          ; LSuccess := TRUE 
          END (* IF *) 
        END (* IF *) 
      ELSE LSuccess := FALSE 
      END (* IF *) 
    ; IF LSuccess 
         AND InnerSave ( Closure ) 
      THEN 
        UiRecPlay . RecordString ( LCommandString ) 
      ; RETURN TRUE 
      ELSE RETURN FALSE 
      END (* IF *) 
    END InnerSaveAsOK  

; PROCEDURE ReplaySaveAsWorkProc 
    ( Closure : WorkerClosureTextTyp ) 
  (* PRE: Closure . Window is set.  Closure . TextParam is file name. *) 
  (* Runs on worker thread. *) 

  = BEGIN 
      EVAL SetImageTransAndPers ( Closure ) 
    ; IF InnerSaveAsOK ( Closure ) 
      THEN (* Unless something changed, we should always get here. *) 
        TRY 
          FormsVBT . PutText 
            ( EditWindow . Form ( Closure . Window )  
            , "Fv_SaveAsDialog_FileName" 
            , Closure . TextParam  
            )
        EXCEPT ELSE (* Oh forget it. *)  
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END ReplaySaveAsWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayFileSaveAs ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureTextTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , TextParam := FileName 
                     , apply := ReplaySaveAsWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayFileSaveAs 

; PROCEDURE SaveAsOKWorkProc 
    ( Closure : WorkerClosureTextTyp ) 
  (* PRE: Closure . Window is set, Closure . TextParam is file name. *) 
  (* Runs on worker thread. *) 

  = VAR LForm : FormsVBT . T 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; EVAL SetImageTransAndPers ( Closure ) 
    ; IF Misc . IsPickleName ( Closure . TextParam ) 
      THEN 
        IF InnerSaveAsOK ( Closure ) 
        THEN 
          FormsVBT . PopDown ( LForm , "Fv_SaveAsDialog" ) 
        (* Otherwise, leave the dialog up and let the user either change
           something and try again, or explicitly cancel. *)  
        END (* IF *)  
      ELSE
        FormsVBT . PutText 
          ( LForm 
          , "Fv_SaveAsNameDialog_FirstFileName" 
          , Closure . TextParam  
          ) 
      ; FormsVBT . PutText 
          ( LForm 
          , "Fv_SaveAsNameDialog_SecondFileName" 
          , Misc . PickleName ( Closure . TextParam )  
          ) 
      ; FormsVBT . PopUp ( LForm , "Fv_SaveAsNameDialog" ) 
      END (* IF *) 
    END SaveAsOKWorkProc 

; PROCEDURE SaveAsOKCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LFileName := FormsVBT . GetText ( Form , "Fv_SaveAsDialog_FileName" )
    ; EVAL Worker . RequestWorkInteractive  
             ( NEW ( WorkerClosureTextTyp 
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )    
                   , Time := Time 
                   , TextParam := LFileName 
                   , apply := SaveAsOKWorkProc 
                   ) 
             ) 
    END SaveAsOKCallback  

; PROCEDURE SaveAsNameYesCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LFileName 
        := FormsVBT . GetText 
             ( Form , "Fv_SaveAsNameDialog_SecondFileName" )
    ; FormsVBT . PutText 
        ( Form 
        , "Fv_SaveAsDialog_FileName" 
        , LFileName 
        ) 
    ; FormsVBT . PopDown ( Form , "Fv_SaveAsNameDialog" )
    (* Make the user accept the name in the SaveAs dialog, still up. *) 
    END SaveAsNameYesCallback 

; PROCEDURE SaveAsNameCancelCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      FormsVBT . PopDown ( Form , "Fv_SaveAsNameDialog" )
    END SaveAsNameCancelCallback 

(*********************************Export**********************************) 

; VAR WTWrT : Wr . T 
(* TODO ^Move this inside WriteText, if/when we ever get around the CG bug
        in the PM3 gcc-based backend that affects it when there. *) 

; PROCEDURE WriteText 
    ( Closure : WorkerClosureTextTyp (* TextParam is file name. *) ) 
  : BOOLEAN (* True if successful *) 
  RAISES { Backout , Thread . Alerted } 
  (* Do the actual writing. *) 

  = PROCEDURE WriteProc  
      ( <* UNUSED *> ImageRef : PaintHs . ImageTransientTyp 
      ; String : Strings . StringTyp 
      )  
    RAISES { Backout , Thread . Alerted } 

    = BEGIN
        TRY 
          Wr . PutText ( WTWrT , Strings . ToTextNonNIL ( String ) ) 
        ; Wr . PutText ( WTWrT , Wr . EOL ) 
        EXCEPT Wr . Failure 
        => 
(* TODO: Pop a dialog. *) 
          RAISE Thread . Alerted 
        END (* TRY EXCEPT *) 
      END WriteProc 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN (* WriteText *) 

      VAR LForm : FormsVBT . T 
    ; VAR LSuccess : BOOLEAN 

    ; BEGIN (* Block for WriteText *) 
        LForm := EditWindow . Form ( Closure . Window ) 
      ; TRY 
          WTWrT := VersionedFiles . OpenWrite ( Closure . TextParam ) 
        ; TextEdit . FlushEdit ( Closure . ImageTrans ) 
        ; WriteTrv . WriteText 
            ( Closure . ImageTrans , WriteProc , DoGenerateText := TRUE ) 
        ; Wr . Close ( WTWrT  ) 
        ; LSuccess := TRUE  
        EXCEPT 
        VersionedFiles . Error ( EMessage ) 
        => IF Closure . IsInteractive 
           THEN 
             FormsVBT . PutText 
               ( LForm 
               , "Fv_ErrorPopup_Message" 
               , "Can't open file \"" & Closure . TextParam & "\": "
                 & EMessage 
               )
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          END (* IF *) 
        ; LSuccess := FALSE  
        | Thread . Alerted => RAISE Thread . Alerted 
        | Backout ( EArg ) => RAISE Backout ( EArg ) 
        ELSE 
          IF Closure . IsInteractive
           THEN 
             FormsVBT . PutText 
               ( LForm 
               , "Fv_ErrorPopup_Message" 
               , "Can't write file \"" & Closure . TextParam & "\""
               )
          ; FormsVBT . PopUp ( LForm , "Fv_ErrorPopup" ) 
          END (* IF *) 
        ; LSuccess := FALSE  
        END (* TRY EXCEPT *) 
      ; RETURN LSuccess 
      END (* Block *) 
    END WriteText 

; PROCEDURE ExportCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 
  (* Just pop the export file name dialog. *) 

  = VAR LWindow : EditWindow . T  

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" ) 
    ; IF LWindow . WrImageRef # NIL 
      THEN 
        FormsVBT . PutText 
          ( Form 
          , "Fv_ExportDialog_FileName" 
          , LWindow . WrImageRef . ItPers . IpAbsTextFileName 
          )
      ; FormsVBT . PopUp ( Form , "Fv_ExportDialog" ) 
      END (* IF *) 
    END ExportCallback  

; PROCEDURE InnerExportOK 
    ( Closure : WorkerClosureTextTyp ) 
  : BOOLEAN (* Success *) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window, ImageTrans, and ImagePers are set. *) 
  (* PRE: TextParam is file name. *) 
  (* Runs on worker thread. *) 

  = VAR LCommandString : TEXT := NIL 

  ; BEGIN 
(* TODO: Fix so file names with pickle, check point, and version suffixes
         are prompted and changed, as in SaveAs. 
*) 
      IF Closure . TextParam # NIL 
         AND NOT Text . Equal ( Closure . TextParam , "" )  
         AND Closure . Window # NIL 
         AND Closure . ImageTrans # NIL 
      THEN 
        LCommandString 
          := UiRecPlay . BeginCommandPlusString 
               ( UiRecPlay . CommandTyp . FileExport 
               , Misc . RelFileName 
                   ( Closure . TextParam 
                   , Pathname . Prefix ( Options . RecordFileName ) 
                   ) 
               ) 
      ; IF WriteText ( Closure )  
        THEN
          Closure . ImagePers . IpAbsTextFileName 
            := Misc . AbsFileName ( Closure . TextParam ) 
        ; UiRecPlay . RecordString ( LCommandString ) 
        ; RETURN TRUE 
        ELSE RETURN FALSE 
        END (* IF *) 
      ELSE RETURN FALSE 
      END 
    END InnerExportOK  

; PROCEDURE ReplayExportWorkProc 
    ( Closure : WorkerClosureTextTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. Closure . TextParam is file name. *) 
  (* Runs on worker thread. *) 

  = BEGIN 
      EVAL SetImageTransAndPers ( Closure ) 
    ; IF InnerExportOK ( Closure ) 
      THEN
        TRY 
          FormsVBT . PutText 
            ( EditWindow . Form ( Closure . Window ) 
            , "Fv_ExportDialog_FileName" 
            , Closure . TextParam  
            )
        EXCEPT ELSE (* Just forget it. *)  
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END ReplayExportWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayFileExport ( FileName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureTextTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , TextParam := FileName 
                     , apply := ReplayExportWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayFileExport 

; PROCEDURE ExportOKWorkProc ( Closure : WorkerClosureTextTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. Closure . TextParam is file name.*) 
  (* Runs on worker thread. *) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      EVAL SetImageTransAndPers ( Closure ) 
    ; IF InnerExportOK ( Closure ) 
      THEN 
        FormsVBT . PopDown 
          ( EditWindow . Form ( Closure . Window ) , "Fv_ExportDialog" ) 
      END (* IF *)  
    (* Otherwise, leave the dialog up and let the user either change
       something and try again, or explicitly cancel. *)  
    END ExportOKWorkProc 

; PROCEDURE ExportOKCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LFileName := FormsVBT . GetText ( Form , "Fv_ExportDialog_FileName" )
    ; EVAL Worker . RequestWorkInteractive  
             ( NEW ( WorkerClosureTextTyp 
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )    
                   , Time := Time 
                   , TextParam := LFileName 
                   , apply := ExportOKWorkProc 
                   ) 
             ) 
    END ExportOKCallback  

; <* UNUSED *> PROCEDURE ExportButtonWorkProc 
     ( Closure : WorkerClosureTextTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* Does unconditional export, using IpAbsTextFileName *) 
  (* PRE: Closure . Window is set. Closure . TextParam is file name.*) 
  (* Runs on worker thread. *) 

  = BEGIN 
      EVAL SetImageTransAndPers ( Closure ) 
    ; EVAL InnerExportOK ( Closure ) 
    END ExportButtonWorkProc 

(*********************************Close***********************************) 

; PROCEDURE ReplayCloseImageWorkProc ( Closure : WorkerClosureTextTyp ) 
  RAISES { Backout } 
  (* PRE: Closure . Window is set. Closure . TextParam is image name.*) 
  (* Runs on worker thread. *) 

  = VAR LImageName : TEXT 
  ; VAR LCommandString : TEXT 
  ; VAR LRef : REFANY 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LImageName 
        := Pathname . Last ( Misc . TextName ( Closure . TextParam ) )  
    ; IF Images . ImageTable . get ( LImageName , LRef ) 
      THEN 
        Closure . ImageTrans := LRef 
      ; EVAL SetImagePers ( Closure ) 
      ; LCommandString 
          := UiRecPlay . BeginCommandPlusString  
               ( UiRecPlay . CommandTyp . FileCloseImage 
               , Closure . ImagePers . IpImageName
               ) 
      ; Images . DisconnectImageFromAllWindows ( Closure . ImageTrans ) 
      ; Images . DiscardImage ( Closure . ImageTrans ) 
      ; FormsVBT . MakeActive ( Options . MainForm , "Fv_File_Open" ) 
      ; UiRecPlay . RecordString ( LCommandString ) 
      END (* IF *) 
    END ReplayCloseImageWorkProc  

(* VISIBLE: *) 
; PROCEDURE ReplayFileCloseImage ( RecordedName : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureTextTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , TextParam := RecordedName 
                     , apply := ReplayCloseImageWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayFileCloseImage 

; PROCEDURE CloseImageWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout } 
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *) 

  = BEGIN 
      EVAL SetImageTransAndPers ( Closure ) 
    ; PromptForSave 
        ( Closure 
        , QuitAfter := FALSE 
        , DoCloseAllImages := FALSE 
        , DoCloseAllWindows := TRUE 
        ) 
    END CloseImageWorkProc 

; PROCEDURE CloseImageCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ;  Time : VBT . TimeStamp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      EVAL Worker . RequestWorkInteractive  
             ( NEW ( Worker . ClosureTyp 
                   , Window 
                       := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )   
                   , Time := Time 
                   , apply := CloseImageWorkProc 
                   ) 
             ) 
    END CloseImageCallback   

; PROCEDURE ReplayCloseWindowWorkProc 
    ( Closure : WorkerClosureTextIntTyp ) 
  RAISES { Backout } 
  (* PRE: Closure . Window is set.  
          Closure . TextParam is image name from the command. 
          Closure . IntParam is window number from the command. 
  *) 
  (* Runs on worker thread. *) 

  = VAR LImageName : TEXT 
  ; VAR LCommandString : TEXT := NIL 
  ; VAR LRef : REFANY 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LImageName 
        := Pathname . Last ( Misc . TextName ( Closure . TextParam ) )
    ; IF Images . ImageTable . get ( LImageName , LRef ) 
      THEN 
        Closure . ImageTrans := LRef 
      ; IF Closure . ImageTrans # NIL 
        THEN 
          EVAL SetImagePers ( Closure ) 
        ; Closure . Window := Closure . ImageTrans . ItWindowList 
        ; LCommandString 
            := UiRecPlay . BeginCommandPlusStringInt 
                 ( UiRecPlay . CommandTyp . FileCloseWindow  
                 , Misc . RelFileName 
                     ( Closure . ImagePers . IpAbsPklFileName 
                     , Pathname . Prefix ( Options . RecordFileName ) 
                     ) 
(* FIX: ^This is not consistent with FileCloseImage, etc. *) 
                 , Closure . Window . WrWindowNo 
                 ) 
        ; LOOP 
            IF Closure . Window = NIL 
            THEN (* Image not open in Window. *) 
              EXIT 
            ELSIF Closure . Window . WrWindowNo = Closure . IntParam  
            THEN 
              Images . DisconnectImageFromWindow 
                ( Closure . ImageTrans , Closure . Window ) 
(* TODO: ^Decide and implement something about closing the window itself.  
         Also do so in CloseWindowProc. 
*) 
            ; IF Closure . ImageTrans . ItWindowList = NIL 
              THEN
                Images . DiscardImage ( Closure . ImageTrans ) 
              ; FormsVBT . MakeActive 
                  ( EditWindow . Form ( Closure . Window ) , "Fv_File_Open" ) 
              END (* IF *) 
            ; UiRecPlay . RecordString ( LCommandString ) 
            ; EXIT 
            ELSE 
              Closure . Window := Closure . Window . WrImageLink 
            END (* IF *) 
          END (* LOOP *)  
        END (* IF *) 
      END (* IF *) 
    END ReplayCloseWindowWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayFileCloseWindow 
    ( RecordedName : TEXT ; WindowNo : INTEGER ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureTextIntTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , TextParam := RecordedName 
                     , IntParam := WindowNo 
                     , apply := ReplayCloseWindowWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayFileCloseWindow 

; PROCEDURE CloseWindowWorkProc ( Closure : Worker . ClosureTyp ) 
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)  

  = VAR LCommandString : TEXT 

  ; BEGIN 
      TRY 
        EVAL SetImageTransAndPers ( Closure ) 
      ; LCommandString 
          := UiRecPlay . BeginCommandPlusStringInt 
               ( UiRecPlay . CommandTyp . FileCloseWindow  
               , Misc . RelFileName 
                   ( Closure . ImageTrans . ItPers . IpAbsPklFileName 
                   , Pathname . Prefix ( Options . RecordFileName ) 
                   ) 
               , Closure . Window . WrWindowNo 
               ) 
      ; IF Closure . Window # NIL 
        THEN IF Closure . ImageTrans # NIL 
          THEN 
            IF Closure . ImageTrans . ItWindowList # NIL 
               AND Closure . ImageTrans . ItWindowList . WrImageLink # NIL 
            THEN (* There are plural windows into this image. *) 
              Images . DisconnectImageFromWindow 
                ( Closure . ImageTrans , Closure . Window ) 
(* TODO: ^Decide and implement something about closing the window itself.  
         Also in ReplayFileCloseWindow. *) 
            ; UiRecPlay . RecordString ( LCommandString ) 
            ELSE (* This is the only window into this image. *) 
              PromptForSave 
                ( Closure 
                , QuitAfter := FALSE 
                , DoCloseAllImages := FALSE 
                , DoCloseAllWindows := TRUE 
                ) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      EXCEPT Backout => 
      END (* TRY EXCEPT *) 
    END CloseWindowWorkProc 

; <* UNUSED *> PROCEDURE CloseWindowCallback 
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
                   , apply := CloseWindowWorkProc 
                   ) 
             ) 
    END CloseWindowCallback   

; PROCEDURE CloseAllWorkProc ( Closure : Worker . ClosureTyp )  
  RAISES { Backout }  
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = BEGIN 
      TRY 
        PromptAndCloseAllImages ( Closure , QuitAfter := FALSE ) 
      EXCEPT Thread . Alerted => (* Ignore. *) 
      END (* TRY EXCEPT *) 
    END CloseAllWorkProc 

; <* UNUSED *> PROCEDURE CloseAllCallback 
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
                   , apply := CloseAllWorkProc 
                   ) 
             ) 
    END CloseAllCallback 

(********************************* Quit **********************************) 

(* VISIBLE: *) 
; PROCEDURE ReplayFileQuit ( ) 

  = BEGIN 
      UiRecPlay . Record ( UiRecPlay . CommandTyp . FileQuit ) 
    ; ReallyQuit ( ) 
    END ReplayFileQuit 

; PROCEDURE QuitWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = BEGIN 
      PromptAndCloseAllImages ( Closure , QuitAfter := TRUE ) 
    END QuitWorkProc 

; PROCEDURE QuitCallback  
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
                   , apply := QuitWorkProc 
                   ) 
             ) 
    END QuitCallback 

(******************************** File Menu ******************************) 

; PROCEDURE AttachFileMenuHandlers ( Form : FormsVBT . T ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN
      FormsVBT . AttachProc 
        ( Form , "Fv_OpenDialog_Open" , OpenCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_File_Close" , CloseImageCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_File_Save" , SaveCallback ) 
    ; FormsVBT . AttachProc  
        ( Form , "Fv_File_SaveAs" , SaveAsCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_SaveAsDialog_OK" , SaveAsOKCallback ) 
    ; FormsVBT . AttachProc  
        ( Form , "Fv_File_Export" , ExportCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_ExportDialog_OK" , ExportOKCallback ) 
    ; FormsVBT . AttachProc 
         ( Form , "Fv_File_Quit" , QuitCallback ) 
    ; SaveDialogInfo . QuitAfter := FALSE 
    ; SaveDialogInfo . UnsavedImagesExist := FALSE 
    ; SaveDialogInfo . DoCloseAllWindows := FALSE 
    ; SaveDialogInfo . DoCloseAllImages := FALSE 
    ; SaveDialogInfo . IsPoppedUp := FALSE 
    ; FormsVBT . AttachProc  
        ( Form , "Fv_SaveDialog_Save" , SaveDialogCallback ) 
    ; FormsVBT . AttachProc  
        ( Form , "Fv_SaveDialog_Cancel" , SaveDialogCallback ) 
    ; FormsVBT . AttachProc  
        ( Form , "Fv_SaveDialog_Forget" , SaveDialogCallback ) 
    END AttachFileMenuHandlers 

(******************************** Edit menu ******************************)

; PROCEDURE EditCutWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = VAR LSel : Selection . SelectionTyp 
  ; VAR LTextSel : TEXT 
  ; VAR LLeftMark : PaintHs . LineMarkMeatTyp  
  ; VAR LRightMark : PaintHs . LineMarkMeatTyp  
  ; VAR LImage : PaintHs . ImageTransientTyp 
  ; VAR LCommandString : TEXT 

  ; BEGIN 
      LSel := Selection . Current 
    ; IF LSel # NIL 
         AND Closure . Window . WrImageRef = LSel . SelImage 
      THEN (* Don't cut from a window whose image the selection isn't in. *) 
        LCommandString 
          := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . EditCut ) 
      ; IF LSel . SelText = NIL 
        THEN
          LSel . SelText 
            := Selection . ManifestSelectionAsText ( ) 
        END (* IF *) 
      ; LTextSel := LSel . SelText 
      (* ^Paranoid coding, in case ClearSelection clears SelText too. *) 
      ; LImage := LSel . SelImage 
      ; PaintHs . GetMarksInOrder 
          ( Mark1 := LSel . SelStartMark 
          , Mark2 := LSel . SelEndMark 
          , (* VAR *) Left := LLeftMark 
          , (* VAR *) Right := LRightMark 
          ) 
      ; TextEdit . DeleteBetweenMarks 
          ( LImage 
          , LLeftMark 
          , LRightMark 
          ) 
      ; Selection . ClearSelection ( ) 
        (* LSel will be obsolete here. *) 
      ; Selection . Current . SelText := LTextSel  
      ; UiRecPlay . RecordString ( LCommandString ) 
      END (* IF *) 
    END EditCutWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayEditCut ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := EditCutWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayEditCut 

; PROCEDURE EditCutCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 
  ; VAR LWindow : EditWindow . T 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LFileName := FormsVBT . GetText ( Form , "Fv_OpenDialog_FileName" )
    ; IF LFileName # NIL AND NOT Text . Equal ( LFileName , "" ) 
      THEN 
        LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" ) 
      ; EVAL Worker . RequestWorkInteractive  
               ( NEW ( Worker  . ClosureTyp 
                     , Window := LWindow 
                     , Time := Time 
                     , apply := EditCutWorkProc 
                     ) 
               ) 
      END (* IF *) 
    END EditCutCallback 

; PROCEDURE EditCopyWorkProc ( <* UNUSED *> Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = VAR LCommandString : TEXT 

  ; BEGIN 
      IF Selection . Current # NIL AND Selection . Current . SelText = NIL 
      THEN
        LCommandString 
          := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . EditCopy ) 
      ; Selection . Current . SelText 
          := Selection . ManifestSelectionAsText ( ) 
      ; UiRecPlay . RecordString ( LCommandString ) 
      END (* IF *) 
    (* Do we want to clear the swept text attributes? *) 
    END EditCopyWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayEditCopy ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := EditCopyWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayEditCopy 

; PROCEDURE EditCopyCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LFileName : TEXT 
  ; VAR LWindow : EditWindow . T 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LFileName := FormsVBT . GetText ( Form , "Fv_OpenDialog_FileName" )
    ; IF LFileName # NIL AND NOT Text . Equal ( LFileName , "" ) 
      THEN 
        LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" ) 
      ; EVAL Worker . RequestWorkInteractive  
               ( NEW ( Worker  . ClosureTyp 
                     , Window := LWindow 
                     , Time := Time 
                     , apply := EditCopyWorkProc 
                     ) 
               ) 
      END (* IF *) 
    END EditCopyCallback 

; PROCEDURE ReadSelections  
    ( Window : EditWindow . WindowTyp 
    ; Time : VBT . TimeStamp 
    )
  <* LL . sup <= VBT . mu *> 
(* Experimental, to help ascertain what Trestle, X, and other applications
    are doing with selections. *) 

  = VAR LSelectionValue : VBT . Value := NIL 
  ; VAR LSelectionRef : REFANY := NIL 
  ; VAR LSelectionExceptionCaught : BOOLEAN := FALSE 
  ; VAR LSelectionCode : VBT . ErrorCode 
  ; VAR LSelectionText : TEXT := NIL 
  ; VAR LTargetValue : VBT . Value := NIL 
  ; VAR LTargetRef : REFANY := NIL 
  ; VAR LTargetExceptionCaught : BOOLEAN := FALSE 
  ; VAR LTargetCode : VBT . ErrorCode 
  ; VAR LTargetText : TEXT := NIL 

  ; BEGIN 
      TRY 
        LSelectionValue := VBT . Read ( Window , VBT . Source , Time ) 
      ; IF LSelectionValue # NIL 
        THEN
          LSelectionRef := LSelectionValue . toRef ( ) 
        ; TYPECASE LSelectionRef 
          OF NULL => 
          | TEXT ( TString ) 
          => LSelectionText := TString 
          ELSE 
          END (* TYPECASE *) 
        END (* IF *) 
      EXCEPT 
        VBT . Error ( ECode ) 
      => LSelectionExceptionCaught := TRUE 
      ; LSelectionCode := ECode 
      END (* TRY EXCEPT *) 

    ; TRY 
        LTargetValue := VBT . Read ( Window , VBT . Target , Time ) 
      ; IF LTargetValue # NIL 
        THEN
          LTargetRef := LTargetValue . toRef ( ) 
        ; TYPECASE LTargetRef 
          OF NULL => 
          | TEXT ( TString ) 
          => LTargetText := TString 
          ELSE 
          END (* TYPECASE *) 
        END (* IF *) 
      EXCEPT 
        VBT . Error ( ECode ) 
      => LTargetExceptionCaught := TRUE 
      ; LTargetCode := ECode 
      END (* TRY EXCEPT *) 

    ; Assertions . DoNothing ( ) 

    END ReadSelections 

; PROCEDURE EditPasteWorkProc ( Closure : WorkerClosureTextTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set.  Closure . CharParam is char typed. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommandPlusString    
             ( UiRecPlay . CommandTyp . EditPaste , Closure . TextParam ) 
    ; TextEdit . InsertOrOverlayString   
        ( Closure . Window 
        , Closure . TextParam 
        , Closure . Window . WrInsertMode 
        ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END EditPasteWorkProc 

(* VISIBLE: *) 
; PROCEDURE Paste ( Window : EditWindow . WindowTyp ; Time : VBT . TimeStamp ) 
  <* LL . sup <= VBT . mu *> 

  = VAR LValue : VBT . Value := NIL 
  ; VAR LRef : REFANY := NIL 
  ; VAR LExceptionCaught1 : BOOLEAN := FALSE 
  ; VAR LExceptionCaught2 : BOOLEAN := FALSE 
  ; VAR LCode1 : VBT . ErrorCode 
  ; VAR LCode2 : VBT . ErrorCode 

  ; BEGIN 

ReadSelections (* q.v. *) ( Window , Time ) 
; 
      TRY 
        LValue 
          := VBT . Read ( Window , EditWindow . ScreenSelection ( ) , Time ) 
      EXCEPT 
        VBT . Error ( ECode ) 
      => LExceptionCaught1 := TRUE 
      ; LCode1 := ECode 
      END (* TRY EXCEPT *) 
    ; IF LExceptionCaught1 
      THEN (* Emacs may have some data here: *) 
        TRY 
          LValue 
            := VBT . Read ( Window , EditWindow . ScreenSelection ( ) , Time ) 
        EXCEPT 
          VBT . Error ( ECode ) 
        => LExceptionCaught2 := TRUE 
        ; LCode2 := ECode 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    ; IF NOT LExceptionCaught2 AND NOT LExceptionCaught2 AND LValue # NIL 
      THEN 
        TRY 
          LRef := LValue . toRef ( ) 
        ; TYPECASE LRef 
          OF NULL => 
          | TEXT ( TString ) 
          => EVAL Worker . RequestWorkInteractive 
               ( NEW ( WorkerClosureTextTyp 
                     , Window := Window 
                     , TextParam := TString  
                     , apply := EditPasteWorkProc 
                     ) 
               ) 
          ELSE 
          END (* TYPECASE *) 
        EXCEPT VBT . Error => (* Ignore. *) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END Paste

(* VISIBLE *) 
; PROCEDURE ReplayEditPaste ( Window : EditWindow . WindowTyp ; Text : TEXT ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureTextTyp 
                     , Window := Window 
                     , TextParam := Text 
                     , apply := EditPasteWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayEditPaste 

; PROCEDURE EditPasteCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = VAR LWindow : EditWindow . T 

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" ) 
    ; Paste ( LWindow , Time ) 
    END EditPasteCallback 

; PROCEDURE AttachEditMenuHandlers ( Form : FormsVBT . T ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      FormsVBT . AttachProc 
        ( Form , "Fv_Edit_Cut" , EditCutCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Edit_Copy" , EditCopyCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Edit_Paste" , EditPasteCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Edit_Search" , UiSearch . SearchWindowCallback ) 
    ; UiSearch . AttachSearchHandlers ( Form ) 
    END AttachEditMenuHandlers 

(*********************************Buttons, etc.***************************) 

; PROCEDURE HasErrors ( ) : BOOLEAN 
(* TODO:  This scheme needs to be made thread safe. *) 

  = BEGIN 
      RETURN 
        Messages . MessageCount ( MessageCodes . KindTyp . MkError ) > 0 
        OR Messages . MessageCount ( MessageCodes . KindTyp . MkFatal ) > 0 
        OR Messages . MessageCount ( MessageCodes . KindTyp . MkAssert ) > 0 
    END HasErrors 

(*********************************** Stop ********************************) 

; PROCEDURE StopButtonCallback 
    ( <* UNUSED *> Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      TRY 
        EVAL Worker . CancelImmedWork ( WaitToFinish := FALSE ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END StopButtonCallback 

(****************************** Accept repair ****************************) 

; PROCEDURE AcceptRepairWorkProc ( Closure : Worker . ClosureTyp )  
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . SemAccept ) 
    ; TextEdit . AcceptRepairUnderCursor ( Closure . Window ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END AcceptRepairWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplaySemAccept ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork  
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := AcceptRepairWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplaySemAccept  

; PROCEDURE AcceptRepairCallback 
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
                   , apply := AcceptRepairWorkProc 
                   ) 
               ) 
    END AcceptRepairCallback 

(*********************************** Parse *******************************) 

; PROCEDURE ParseWorkProc ( Self : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted }   
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = VAR LCommandString : TEXT 

  ; BEGIN 
      EVAL SetImageTransAndPers ( Self ) 
    ; IF Self . ImageTrans # NIL 
      THEN   
        LCommandString 
          := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . SemParse ) 
      ; Messages . ZeroMessageCounts ( ) 
      ; Messages . TextOnly 
          ( "Parsing -------------------------------------------------" 
          , Kind := MessageCodes . KindTyp . MkInformation 
          ) 
      ; Display . Reparse ( Self . ImageTrans ) 
      ; IF EstUtil . HasSyntErrors 
             ( Self . ImagePers . IpEstRoot ) 
        THEN 
          Messages . TextOnly 
            ( "Parse complete, with errors -----------------------------" 
            , Kind := MessageCodes . KindTyp . MkInformation 
            ) 
        ELSE 
          Messages . TextOnly 
            ( "Parse complete, no errors -------------------------------" 
            , Kind := MessageCodes . KindTyp . MkInformation 
            ) 
        END  
      ; UiRecPlay . RecordString ( LCommandString ) 
      END (* IF *)  
    END ParseWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplaySemParse ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork  
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := ParseWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplaySemParse 

; PROCEDURE ParseCallback 
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
                   , apply := ParseWorkProc 
                   ) 
               ) 
    END ParseCallback 

(********************************** Analyze ******************************) 

; PROCEDURE AnalyzeWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = VAR LCommandString : TEXT 

  ; BEGIN 
      EVAL SetImageTransAndPers ( Closure ) 
    ; IF Closure . ImageTrans # NIL 
      THEN   
        LCommandString 
          := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . SemAnalyze ) 
      ; IF NOT Closure . ImagePers . IpIsParsed 
           OR NOT Closure . ImagePers . IpIsAnalyzed 
        THEN 
          Messages . ZeroMessageCounts ( ) 
        END (* IF *) 
      ; IF NOT Closure . ImagePers . IpIsParsed 
        THEN 
          Messages . TextOnly 
            ( "Parsing -------------------------------------------------" 
            , Kind := MessageCodes . KindTyp . MkInformation 
            ) 
        ; Display . Reparse ( Closure . ImageTrans )
        ; IF EstUtil . HasSyntErrors ( Closure . ImagePers . IpEstRoot ) 
          THEN 
            Messages . TextOnly 
              ( "Parse failed --------------------------------------------" 
              , Kind := MessageCodes . KindTyp . MkInformation 
              ) 
          ; RETURN 
          END  
        END (* IF *)
      ; IF NOT Closure . ImagePers . IpIsAnalyzed 
        THEN 
          IF Closure . ImagePers . IpLang = LbeStd . LangLdl0 
             OR Closure . ImagePers . IpLang = LbeStd . LangLdl1 
          THEN 
            Messages . TextOnly 
              ( "Analyzing Ldl -------------------------------------------" 
              , Kind := MessageCodes . KindTyp . MkInformation 
              ) 
          ; Closure . ImagePers . IpSemRoot 
              := LdlSemantics . Analyze 
                   ( Closure . ImagePers . IpEstRoot 
                   , Closure . ImagePers . IpLang 
                   ) 
          ; IF HasErrors ( ) 
            THEN 
              Messages . TextOnly 
                ( "Analysis failed -----------------------------------------" 
                , Kind := MessageCodes . KindTyp . MkInformation 
                ) 
            ; RETURN 
            ELSE 
              Closure . ImagePers . IpSemRoot 
                := LdlSemantics . Analyze 
                     ( Closure . ImagePers . IpEstRoot ) 
            ; IF HasErrors ( ) 
              THEN 
                Messages . TextOnly 
                  ( "LALR generation failed --------------------------------" 
                  , Kind := MessageCodes . KindTyp . MkInformation 
                  ) 
              ; RETURN 
              END (* IF *)   
            END (* IF *)   
          END (* IF *) 
        ; Display . NoteImageSavedState ( Closure . ImageTrans , FALSE ) 
        END 
      ; Display . NoteImageAnalyzedState ( Closure . ImageTrans , TRUE ) 
      ; Messages . TextOnly 
          ( "Done analyzing ------------------------------------------" 
          , Kind := MessageCodes . KindTyp . MkInformation 
          ) 
      ; UiRecPlay . RecordString ( LCommandString ) 
      END (* IF *) 
    END AnalyzeWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplaySemAnalyze ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        EVAL Worker . RequestWork  
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := AnalyzeWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplaySemAnalyze 

; PROCEDURE AnalyzeCallback   
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
                   , apply := AnalyzeWorkProc 
                   ) 
               ) 
    END AnalyzeCallback   

(*************************** Toggle insert mode **************************) 

; PROCEDURE InsertButtonCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LWindow : EditWindow . T  

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" ) 
    ; EditWindow . ToggleInsertMode ( LWindow ) 
    END InsertButtonCallback 

(*************************** Semantics menu ******************************)

; PROCEDURE AttachSemMenuHandlers ( Form : FormsVBT . T ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      FormsVBT . AttachProc 
        ( Form , "Fv_Sem_Parse" , ParseCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Sem_Accept" , AcceptRepairCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Sem_Analyze" , AnalyzeCallback ) 
    END AttachSemMenuHandlers 

(**************************** Scrolling **********************************)

; PROCEDURE VertScrollWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = VAR LForm : FormsVBT . T 
  ; VAR LMax : INTEGER 
  ; VAR LThumb : INTEGER 
  ; VAR LValue : INTEGER 
  ; VAR LCommandString : TEXT 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      LForm := EditWindow . Form ( Closure . Window ) 
    ; EVAL SetImageTrans ( Closure ) 
    ; IF Closure . ImageTrans # NIL 
      THEN 
        LCommandString 
          := UiRecPlay . BeginCommandPlusInt3 
               ( UiRecPlay . CommandTyp . VertScroll , LMax , LThumb , LValue )
      ; LMax 
          := FormsVBT . GetIntegerProperty 
               ( LForm , "Fv_VertScroller" , "Max" )
      ; LThumb 
          := FormsVBT . GetIntegerProperty 
               ( LForm , "Fv_VertScroller" , "Thumb" )
      ; LValue 
          := FormsVBT . GetInteger ( LForm , "Fv_VertScroller" )
      ; TRY 
          IF LValue = 0 
          THEN (* Move to beginning, despite inaccuracies of estimation. *) 
            Display . VertScrollAndRepaint
              ( WindowRef := Closure . Window 
              , WantedMovement := FIRST ( LbeStd . LineNoSignedTyp ) 
              , DoDragCursor := FALSE 
              ) 
          ELSIF LValue = LMax - LThumb  
          THEN (* Move to end, in spite of inaccuracies of estimation. *) 
            Display . VertScrollAndRepaint
              ( WindowRef := Closure . Window 
              , WantedMovement := LAST ( LbeStd . LineNoSignedTyp ) 
              , DoDragCursor := FALSE 
              ) 
          ELSE (* Move to somewhere in between. *)  
            Display . VertScrollAndRepaint
              ( WindowRef := Closure . Window 
              , WantedMovement := LValue - Closure . Window . WrVertScroll 
              , DoDragCursor := FALSE 
              ) 
          END (* IF *)  
        ; UiRecPlay . RecordString ( LCommandString ) 
        EXCEPT Thread . Alerted 
        => (* Trestle will have already moved the scroller thumb and will 
              move it back, but not until the mouse moves into the edit VBT. 
           *) 
          EditWindow . UpdateVertScroller ( Closure . Window ) 
        ; RAISE Thread . Alerted 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END VertScrollWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayVertScroll 
    ( Max : INTEGER ; Thumb : INTEGER ; Value : INTEGER ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      TRY 
        FormsVBT . PutIntegerProperty  
          ( Options . MainForm , "Fv_VertScroller" , "Max" , Max )
      ; FormsVBT . PutIntegerProperty  
          ( Options . MainForm , "Fv_VertScroller" , "Thumb" , Thumb )
      ; FormsVBT . PutInteger  
          ( Options . MainForm , "Fv_VertScroller" , Value )
      ; EVAL Worker . RequestWork  
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := VertScrollWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayVertScroll  

; PROCEDURE VertScrollCallback 
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
                   , apply := VertScrollWorkProc 
                   ) 
               ) 
    END VertScrollCallback 

; PROCEDURE HorizScrollWorkProc ( Closure : Worker . ClosureTyp )  
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* Runs on worker thread. *)   

  = VAR LValue : INTEGER 
  ; VAR LCommandString : TEXT 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      EVAL SetImageTrans ( Closure ) 
    ; IF Closure . ImageTrans # NIL 
      THEN 
        LCommandString 
          := UiRecPlay . BeginCommandPlusInt 
               ( UiRecPlay . CommandTyp . HorizScroll , LValue ) 
      ; LValue 
          := FormsVBT . GetInteger 
               ( EditWindow . Form ( Closure . Window ) , "Fv_HorizScroller" )
      ; TRY 
          Display . HorizScrollWindowRef
            ( WindowRef := Closure . Window 
            , WindowMovement := LValue - Closure . Window . WrHorizScroll 
            , DoDragCursor := FALSE 
            ) 
        ; UiRecPlay . RecordString ( LCommandString ) 
        EXCEPT Thread . Alerted 
        => (* Trestle will have already moved the scroller thumb and will 
              move it back, but not  until the mouse moves into the edit VBT. 
           *) 
          EditWindow . UpdateHorizScroller ( Closure . Window ) 
        ; RAISE Thread . Alerted 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END HorizScrollWorkProc 

(* VISIBLE: *) 
; PROCEDURE ReplayHorizScroll ( Value : INTEGER ) 

  = <* FATAL FormsVBT . Unimplemented *>
    <* FATAL FormsVBT . Error *>
    BEGIN 
      TRY 
        FormsVBT . PutInteger  
          ( Options . MainForm , "Fv_HorizScroller" , Value )
      ; EVAL Worker . RequestWork  
               ( NEW ( Worker . ClosureTyp 
                     , Window 
                         := FormsVBT . GetGeneric 
                              ( Options . MainForm , "Fv_LbeWindow" )   
                     , apply := HorizScrollWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayHorizScroll  

; PROCEDURE HorizScrollCallback 
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
                   , apply := HorizScrollWorkProc 
                   ) 
               ) 
    END HorizScrollCallback 

(* VISIBLE: *) 
; PROCEDURE ParseKbd 
    ( <* UNUSED *> Lang : LbeStd . LangTyp 
    ; <* UNUSED *> ScanIf : ScannerIf . ScanIfTyp 
    ; <* UNUSED *> PosRelTo : LbeStd . LimitedCharNoTyp 
    ; <* UNUSED *> VAR NewTreeRef : LbeStd . EstRootTyp 
    ) 

  (* Parse from the keyboard. *) 

  = <* UNUSED *> VAR LParseInfo : ParseHs . ParseInfoTyp 

  ; BEGIN (* ParseKbd *) 
(* 
      LParseInfo . PiLang := Lang 
    ; LParseInfo . PiScanIf := ScanIf 
    ; LParseInfo . PiGram := LangUtil . Gram ( Lang ) 
    ; LParseInfo . PiInsertNilFixedChildren := FALSE 
    ; LParseInfo . PiOrigTempMarkListRef := NIL 
    ; Parser . Parse 
        ( LParseInfo 
        , ParseTrv . InitParseKbd ( LParseInfo , PosRelTo ) 
        , NewTreeRef 
        ) 
*)  END ParseKbd 

; PROCEDURE AttachOtherHandlers ( Form : FormsVBT . T ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      FormsVBT . AttachProc 
        ( Form , "Fv_Save" , SaveAndExportCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Parse" , ParseCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_Analyze" , AnalyzeCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_InsertModeButton" , InsertButtonCallback )
    ; FormsVBT . AttachProc 
        ( Form , "Fv_VertScroller" , VertScrollCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_HorizScroller" , HorizScrollCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_AltOpenDialog_Yes" , AltOpenYesCallback )
    ; FormsVBT . AttachProc 
        ( Form , "Fv_AltOpenDialog_No" , AltOpenNoCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_AltOpenDialog_Cancel" , AltOpenCancelCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_SaveAsNameDialog_Yes" , SaveAsNameYesCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_SaveAsNameDialog_Cancel" , SaveAsNameCancelCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Fv_StopButton" , StopButtonCallback ) 
    END AttachOtherHandlers 

(**********************************Setup************************************) 

; VAR GRecPlayWindowIsOpen : BOOLEAN := FALSE  

; PROCEDURE OpenRecPlayWindow ( ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      IF NOT GRecPlayWindowIsOpen 
      THEN 
        FormsVBT . MakeEvent 
          ( Options . MainForm 
          , "Fv_Devel_RecPlayWindow"                 
          , FormsVBT . GetTheEventTime ( Options . MainForm ) 
          )
      ; Thread . Pause ( 2.0D0 ) 
(* TODO: Investigate whether syncnronization is needed here to allow for
       the window to come up, and either do it properly, or eliminate the
       pause, if not. 
*) 
      ; GRecPlayWindowIsOpen := TRUE 
      END (* IF *) 
    END OpenRecPlayWindow 

; VAR IconWindow : VBT . T := NIL 
; VAR IconTitle := "Schutz"
; VAR OldInstall := FALSE 

; PROCEDURE TrestleInstall ( Form : FormsVBT . T ) 
  RAISES { TrestleComm . Failure } 

  = BEGIN 
      IF OldInstall 
      THEN 
        Trestle . Install 
          ( Form , iconWindow := IconWindow , iconTitle := IconTitle ) 
      ELSE 
        Trestle . Attach ( Form ) 
      ; Trestle . Decorate 
          ( Form , iconWindow := IconWindow , iconTitle := IconTitle )
      ; Trestle . MoveNear ( Form , NIL )  
      END (* IF *) 
    END TrestleInstall 

(* VISIBLE: *) 
; PROCEDURE Install 
    ( EditFileName : TEXT 
    ; PlaybackFileName : TEXT 
    ; DoRunPlayback : BOOLEAN 
    ; RespectStops : BOOLEAN 
    ; RecordFileName : TEXT 
    ; DelayTime : INTEGER 
    )
  : BOOLEAN (* => Success. *) 
  RAISES { Backout } 
  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      TRY 
        Options . MainForm 
          := NEW ( FormsVBT . T ) 
             . initFromRsrc ( "Schutz.fv" , Options . ResourcePath )
      EXCEPT 
        Rsrc . NotFound 
        => DL ( LbeStd . AppName & ": Unable to locate resource Schutz.fv" )  
        ; RETURN FALSE 
      | Rd . Failure  
        => DL ( LbeStd . AppName & ": Unable to read resource Schutz.fv" )  
        ; RETURN FALSE
      | FormsVBT . Error ( msg )
        => DL ( LbeStd . AppName & ": Unable to init from resource Schutz.fv ("
                & msg & ")" )  
        ; RETURN FALSE
      | Thread . Alerted 
        => RETURN FALSE 
      END 
    ; UiRecPlay . RespectStops := RespectStops 
    ; InitForm ( Options . MainForm ) 
    ; IF Options . MainForm = NIL THEN RETURN FALSE END 
    ; Options . MainWindow 
        := FormsVBT . GetGeneric ( Options . MainForm , "Fv_LbeWindow" ) 
    ; FormsVBT . PutInteger 
        ( Options . MainForm 
        , "Fv_Devel_DebugLevelValue" 
        , Options . DebugLevel 
        )
    ; TRY
        TrestleInstall ( Options . MainForm ) 
      ; IF EditFileName # NIL AND NOT Text . Equal ( EditFileName , "" ) 
        THEN
          FormsVBT . PutText 
            ( Options . MainForm , "Fv_OpenDialog_FileName" , EditFileName )
        ; FormsVBT . MakeEvent 
            ( Options . MainForm 
            , "Fv_OpenDialog_Open"                 
            , FormsVBT . GetTheEventTime ( Options . MainForm ) 
            )
        END (* IF *) 
      ; IF DelayTime >= 0 
        THEN 
          FormsVBT . PutInteger 
             ( Options . RecPlayForm , "Rc_Playback_Delay" , DelayTime ) 
        ; FormsVBT . MakeEvent 
            ( Options . RecPlayForm 
            , "Rc_Playback_Delay"                 
            , FormsVBT . GetTheEventTime ( Options . MainForm ) 
            )
        END (* IF *) 
      ; IF PlaybackFileName # NIL 
           AND NOT Text . Equal ( PlaybackFileName , "" ) 
        THEN 
          OpenRecPlayWindow ( ) 
        ; TRY 
            IF UiRecPlay . PlaybackOpen 
                 ( Options . RecPlayForm , PlaybackFileName ) 
            THEN
              IF DoRunPlayback
              THEN 
                FormsVBT . MakeEvent 
                  ( Options . RecPlayForm 
                  , "Rc_Playback_Run"                 
                  , FormsVBT . GetTheEventTime ( Options . MainForm ) 
                  )
              END (* IF *) 
            END (* IF *) 
          EXCEPT Thread . Alerted => (* Ignore. *) 
          END (* TRY EXCEPT *) 
        END (* IF *) 
      (* Do this after playback, in case they have the same name. *) 
      ; IF RecordFileName # NIL 
           AND NOT Text . Equal ( RecordFileName , "" ) 
        THEN
          OpenRecPlayWindow ( )
        ; IF UiRecPlay . RecordOpen 
               ( Options . RecPlayForm , RecordFileName , Enabled := TRUE ) 
          THEN 
            TRY 
              UiRecPlay . RecordEnable ( ) 
            EXCEPT Thread . Alerted => (* Ignore. *) 
            END (* TRY EXCEPT *) 
          END (* IF *) 
        END (* IF *) 
      ; Trestle . AwaitDelete ( Options . MainForm ) 
   (* ; UiRecPlay . RecordClose ( ) *)
      EXCEPT
      | TrestleComm . Failure =>
          DL ( LbeStd . AppName 
               & ": Could not open display " & Options . Display 
             )
        ; Assertions . TerminatingNormally := TRUE
        ; RETURN FALSE 
      END
    ; RETURN TRUE 
    END Install 

(* VISIBLE: *) 
; PROCEDURE DefaultWindow ( ) : PaintHs . WindowRefTyp 

  = VAR LForm : FormsVBT . T 
  ; VAR LWindow : PaintHs . WindowRefTyp

  ; <* FATAL FormsVBT . Error *>
    BEGIN 
      LForm := Options . MainForm 
    ; LWindow  := FormsVBT . GetGeneric ( LForm , "Fv_LbeWindow" ) 
    ; RETURN LWindow 
    END DefaultWindow 

; BEGIN 
  END Ui 
. 
