
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2023, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Images 

; IMPORT TextRefTbl 
; IMPORT TextWr 
; IMPORT Thread 
; IMPORT Wr 

; FROM Failures IMPORT Backout 
; IMPORT Display 
; IMPORT EstUtil 
; IMPORT Files 
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT MessageCodes 
; IMPORT Misc 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT ParseHs 
; IMPORT Parser 
; IMPORT ParseTrv 
; IMPORT ScannerIf 
; IMPORT Selection 
; IMPORT TempMark
; IMPORT TextEdit 
; IMPORT TreeBrowse 
; IMPORT UiRecPlay 

(* EXPORTED *) 
; PROCEDURE ConnectImageToWindow
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; WindowRef : PaintHs . WindowRefTyp 
    ) 
  RAISES { NoMoreWindows } 

  = VAR LWindowRef : PaintHs . WindowRefTyp 
  ; VAR LWindowPredRef : PaintHs . WindowRefTyp 
  ; VAR LWindowNo : PaintHs . WindowNoTyp
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 

  ; BEGIN 
      IF ImageRef # NIL AND WindowRef # NIL 
      THEN
        LImagePers := ImageRef . ItPers 
      ; LWindowRef := LImagePers . IpWindowList 
      ; WHILE LWindowRef # NIL 
        DO 
          IF LWindowRef = WindowRef 
          THEN  (* It's already done. *)  
            RETURN 
          END 
        ; LWindowRef := LWindowRef . WrImageLink 
        END  
      ; IF LImagePers . IpVisibleIn < PaintHs . WindowNoSetFull 
        THEN 
          EVAL PaintHs . InitWindowRef ( WindowRef ) 
        ; LWindowNo := PaintHs . WindowNoMin 
        ; WHILE LWindowNo IN LImagePers . IpVisibleIn 
          DO INC ( LWindowNo ) 
          END 
        ; LImagePers . IpVisibleIn 
            := LImagePers . IpVisibleIn
               + PaintHs . WindowNoSetTyp { LWindowNo }  
        ; WindowRef . WrWindowNo := LWindowNo 
        ; IF LImagePers . IpWindowList = NIL 
             OR LImagePers . IpWindowList . WrWindowNo > LWindowNo 
          THEN 
            WindowRef . WrImageLink := LImagePers . IpWindowList 
          ; LImagePers . IpWindowList := WindowRef 
          ELSE
            LWindowPredRef := LImagePers . IpWindowList  
          ; LOOP 
              LWindowRef := LWindowPredRef . WrImageLink 
            ; IF LWindowRef = NIL OR LWindowRef . WrWindowNo > LWindowNo 
              THEN 
                WindowRef . WrImageLink := LWindowRef 
              ; LWindowPredRef . WrImageLink := WindowRef 
              ; EXIT 
              ELSE 
                LWindowPredRef := LWindowRef 
              END 
            END 
          END 
        ; WindowRef . WrImageRef := ImageRef 
        ELSE 
          RAISE NoMoreWindows 
        END 
      END (* IF *) 
    END ConnectImageToWindow  

; PROCEDURE ExcerptMarkList 
    ( Image : PaintHs . ImageTransientTyp 
    ; NewImagePers : PaintHs . ImagePersistentTyp 
    ; RemoveLinesRefs : BOOLEAN 
    ) 
  RAISES { Backout } 
  (* Copy the mark list, but remove marks not to be saved in the
     pickle file.  At present, only the first cursor is saved. 
     Also NIL out mark fields that have no meaning in a pickle. *) 

  = VAR LMark : PaintHs . LineMarkTyp 
  ; VAR LNewMark : PaintHs . LineMarkMeatTyp 

  ; BEGIN 
      IF Image . ItPers . IpMarkHeader = NIL 
      THEN NewImagePers . IpMarkHeader := NIL 
      ELSE 
        NewImagePers . IpMarkHeader := PaintHs . NewLineMarkHeader ( ) 
      ; NewImagePers . IpMarkCt := 0 
      ; LMark := Image . ItPers . IpMarkHeader 
      ; LOOP 
          TYPECASE LMark . LmRightLink 
          OF PaintHs . LineMarkMeatTyp ( TRightMark ) 
             (* NIL can't happen. *) 
          => PaintHs . UpdateLineMarkMeat ( Image , TRightMark ) 
          ; IF TRightMark . LmMarkSs = PaintHs . MarkSsTyp . MarkSsCursor 
                OR Selection . KeepLeftoverMarks 
            THEN (* Found a mark to keep. *) 
              LNewMark
                := PaintHs . NewLineMarkMeat ( NewImagePers . IpMarkHeader ) 
            ; LNewMark . LmWindowRef := NIL  
            ; LNewMark . LmMarkSs := TRightMark . LmMarkSs  
            ; LNewMark . LmTokMark := TRightMark . LmTokMark  
            ; IF RemoveLinesRefs 
              THEN LNewMark . LmLinesRef := NIL  
              ELSE LNewMark . LmLinesRef := TRightMark . LmLinesRef 
              END (* IF *) 
            ; LNewMark . LmCharPos := TRightMark . LmCharPos  
            ; LNewMark . LmLineNo := TRightMark . LmLineNo   
            ; PaintHs . LinkLineMarkToRight 
                ( InsertToRightOfRef := NewImagePers . IpMarkHeader 
                , RefToInsert := LNewMark 
                ) 
            ; INC ( NewImagePers . IpMarkCt )
            END (* IF *) 
          ; LMark := TRightMark 
          ELSE (* On the right is the header.  End of Marks list. *) 
            EXIT 
          END (* TYPECASE *) 
        END (* LOOP *) 
      END (* IF *) 
    END ExcerptMarkList 

(* EXPORTED *) 
; PROCEDURE PersistentImageToSave  
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; ForSave : BOOLEAN 
    ) 
  : PaintHs . ImagePersistentTyp  
  RAISES { Backout , Thread . Alerted } 
  (* If ForSave, has SIDE EFFECT of TextEdit . FlushEdit, because this must 
     be done to prevent the cleaning up from losing information. 
  *) 

  = VAR LOldImagePers : PaintHs . ImagePersistentTyp
  ; VAR LNewImagePers : PaintHs . ImagePersistentTyp

  ; BEGIN 
      LOldImagePers := ImageTrans . ItPers 
    ; LNewImagePers:= NEW ( PaintHs . ImagePersistentTyp ) . initDefaults ( ) 
    ; LNewImagePers . IpHistoryText := NIL  
    ; TYPECASE ImageTrans . ItHistoryWrT 
      OF NULL => 
      | TextWr . T ( TTextWrT ) 
        => IF NOT Wr . Closed ( TTextWrT ) 
          THEN
            LNewImagePers . IpHistoryText := TextWr . ToText ( TTextWrT ) 
          ; <* FATAL Wr . Failure *> 
            BEGIN (* Block *) 
              Wr . PutText ( TTextWrT , LNewImagePers . IpHistoryText ) 
            END (* Block *) 
          END (* IF *) 
      ELSE 
      END (* TYPECASE *) 
    ; IF ForSave 
      THEN 
        TextEdit . FlushEdit ( ImageTrans ) 
      ; LNewImagePers . IpLineHeaderRef := NIL 
      ; LNewImagePers . IpTempEditRef := NIL  
      ; LNewImagePers . IpCrashCommand := NIL 
      ; LNewImagePers . IpCrashCode := ORD ( MessageCodes . T . NullCode )   
      ; LNewImagePers . IpIsMutatedSinceCommandStarted := FALSE 
      ELSE 
        LNewImagePers . IpLineHeaderRef := LOldImagePers . IpLineHeaderRef   
      ; LNewImagePers . IpTempEditRef := LOldImagePers . IpTempEditRef 
      ; LNewImagePers . IpCrashCommand := LOldImagePers . IpCrashCommand   
      ; LNewImagePers . IpCrashCode := LOldImagePers . IpCrashCode    
      ; LNewImagePers . IpIsMutatedSinceCommandStarted 
          := LOldImagePers . IpIsMutatedSinceCommandStarted  
      END (* IF *) 
    ; ExcerptMarkList 
        ( ImageTrans , LNewImagePers , RemoveLinesRefs := ForSave ) 
    ; LNewImagePers . IpIsAnalyzed := FALSE (* Since IpSemRoot not copied. *) 

    (* Remaining Ip fields are just copied: *) 
    ; LNewImagePers . IpEstRoot := LOldImagePers . IpEstRoot 
    ; LNewImagePers . IpTempEditState := LOldImagePers . IpTempEditState
    ; LNewImagePers . IpImageName := LOldImagePers . IpImageName
    ; LNewImagePers . IpAbsPklFileName := LOldImagePers . IpAbsPklFileName 
    ; LNewImagePers . IpAbsTextFileName := LOldImagePers . IpAbsTextFileName
    ; LNewImagePers . IpVerNo := LOldImagePers . IpVerNo
    ; LNewImagePers . IpVerUpdKind := LOldImagePers . IpVerUpdKind
    ; LNewImagePers . IpVerState := LOldImagePers . IpVerState
    ; LNewImagePers . IpLastCommand := LOldImagePers . IpLastCommand  
    ; LNewImagePers . IpHistoryText := LOldImagePers . IpHistoryText   
    ; LNewImagePers . IpLang := LOldImagePers . IpLang 
    ; LNewImagePers . IpIsParsed := LOldImagePers . IpIsParsed
    ; LNewImagePers . IpLineCtDisplay := LOldImagePers . IpLineCtDisplay 
    ; LNewImagePers . IpLineCtIsExact := LOldImagePers . IpLineCtIsExact  

    ; RETURN LNewImagePers 
    END PersistentImageToSave 

; PROCEDURE RemoveWindowNosFromImageRef 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; WindowNoSet : PaintHs . WindowNoSetTyp 
    ) 
  RAISES { Backout } 
  (* PRE: ImageRef # NIL *) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp  
  ; VAR LFromLinesRef : PaintHs . LinesRefMeatTyp  
  
  ; BEGIN
      IF ImageRef = NIL THEN RETURN END (* IF *)
    ; LImagePers := ImageRef . ItPers 
    ; IF LImagePers = NIL THEN RETURN END (* IF *)
    ; LImagePers . IpVisibleIn := LImagePers . IpVisibleIn - WindowNoSet 
    ; IF LImagePers . IpLineHeaderRef # NIL 
      THEN 
        TYPECASE LImagePers . IpLineHeaderRef . LrRightLink <* NOWARN *>
        OF PaintHs . LinesRefMeatTyp ( TFirstLinesRef ) 
        => LLinesRef := TFirstLinesRef 
        ; LOOP 
            LLinesRef . LrVisibleIn 
              := LLinesRef . LrVisibleIn - WindowNoSet 
          ; IF NOT LLinesRef . LrGapAfter 
               AND NOT PaintHs . LineIsVisible ( LLinesRef ) 
               AND NOT LLinesRef . LrHasMark 
            THEN 
              LFromLinesRef := LLinesRef 
            ; LOOP
                TYPECASE LLinesRef . LrRightLink 
                OF PaintHs . LinesRefMeatTyp ( TSuccLinesRef ) 
                => TSuccLinesRef . LrVisibleIn 
                     := TSuccLinesRef . LrVisibleIn - WindowNoSet 
                ; IF NOT TSuccLinesRef . LrGapAfter 
                     AND NOT PaintHs . LineIsVisible ( TSuccLinesRef ) 
                     AND NOT TSuccLinesRef . LrHasMark 
                  THEN 
                    LLinesRef := TSuccLinesRef 
                  ELSE
                    Display . MakeLinesRefsNotVisible 
                      ( ImageRef , LFromLinesRef , LLinesRef )  
                  ; LLinesRef := TSuccLinesRef 
                  ; EXIT 
                  END (* IF *)  
                ELSE (* This is the dummy at the end. *) 
                  IF LLinesRef # LFromLinesRef 
                  THEN (* But it's not the first one in the current range. 
                          Back up one and unlink. *) 
                    LLinesRef := LLinesRef . LrLeftLink 
                  ; Display . MakeLinesRefsNotVisible 
                      ( ImageRef , LFromLinesRef , LLinesRef )  
                  END (* IF *) 
                ; RETURN 
                END (* TYPECASE *) 
              END (* LOOP *) 
            END (* IF *) 
          ; TYPECASE LLinesRef . LrRightLink 
            OF PaintHs . LinesRefMeatTyp ( TRightLinesRef ) 
            => LLinesRef := TRightLinesRef 
            ELSE EXIT 
            END (* TYPECASE *) 
          END (* LOOP *) 
        END (* TYPECASE *) 
      END (* IF *) 
    END RemoveWindowNosFromImageRef 

; PROCEDURE InnerRemoveImageFromWindow
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; WindowRef : PaintHs . WindowRefTyp 
    ; VAR (* IN OUT *) RemovedWindowNoSet : PaintHs . WindowNoSetTyp 
    ) 
  (* PRE: ImageRef # NIL AND ImageRef . ItPers # NIL *) 

  = VAR LWindowRef : PaintHs . WindowRefTyp 
  ; VAR LWindowPredRef : PaintHs . WindowRefTyp 

  ; BEGIN 
      LWindowRef := ImageRef . ItPers . IpWindowList 
    ; IF LWindowRef = WindowRef 
      THEN (* The first window is the one.  Unlink it. *) 
        ImageRef . ItPers . IpWindowList := LWindowRef . WrImageLink 
      ELSE 
        LOOP 
          LWindowPredRef := LWindowRef  
        ; LWindowRef := LWindowRef . WrImageLink 
        ; IF LWindowRef = NIL 
          THEN (* WindowRef is not displaying ImageRef, no action. *) 
            RETURN 
          ELSIF LWindowRef = WindowRef 
          THEN (* This is the one, unlink it. *) 
            LWindowPredRef . WrImageLink := LWindowRef . WrImageLink 
          ; EXIT 
          END 
        END 
      END (* IF *) 
    ; Display . ClearWindow ( WindowRef ) 
    ; EVAL PaintHs . InitWindowRef ( WindowRef ) 
    ; RemovedWindowNoSet 
        := RemovedWindowNoSet 
           + PaintHs . WindowNoSetTyp { LWindowRef . WrWindowNo } 
    END InnerRemoveImageFromWindow 

(* EXPORTED *) 
; PROCEDURE DisconnectImageFromWindow
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; WindowRef : PaintHs . WindowRefTyp 
    ) 
  RAISES { Backout } 

  = VAR LWindowNoSet : PaintHs . WindowNoSetTyp 

  ; BEGIN 
      IF ImageRef # NIL 
      THEN 
        LWindowNoSet := PaintHs . WindowNoSetEmpty 
      ; InnerRemoveImageFromWindow ( ImageRef , WindowRef , LWindowNoSet ) 
      ; RemoveWindowNosFromImageRef ( ImageRef , LWindowNoSet ) 
      END 
    END DisconnectImageFromWindow 

(* EXPORTED *) 
; PROCEDURE DisconnectImageFromAllWindows 
    ( ImageRef : PaintHs . ImageTransientTyp ) 
  RAISES { Backout } 

  = VAR LWindowNoSet : PaintHs . WindowNoSetTyp 
  ; VAR LWindowRef : PaintHs . WindowRefTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 

  ; BEGIN 
      IF ImageRef # NIL
      THEN 
        LImagePers := ImageRef . ItPers
      ; IF LImagePers # NIL 
        THEN 
          LWindowNoSet := PaintHs . WindowNoSetEmpty
        ; LWindowRef := LImagePers . IpWindowList 
        ; WHILE LWindowRef # NIL 
          DO  
            InnerRemoveImageFromWindow ( ImageRef , LWindowRef , LWindowNoSet ) 
          ; LWindowRef := LWindowRef . WrImageLink 
          END (* WHILE *) 
        ; RemoveWindowNosFromImageRef ( ImageRef , LWindowNoSet ) 
        END (* IF *)
      END (* IF *) 
    END DisconnectImageFromAllWindows 

(* EXPORTED *) 
; PROCEDURE DiscardImage ( ImageRef : PaintHs . ImageTransientTyp )  
  (* ^No action if ImageRef is in any window. *) 

  = VAR LRef : REFANY 

  ; BEGIN 
      IF ImageRef # NIL
         AND ImageRef . ItPers #  NIL
         AND ImageRef . ItPers . IpWindowList = NIL 
      THEN 
        EVAL ImageTable . delete 
               ( ImageRef . ItPers . IpImageName , (* VAR *) LRef ) 
      END (* IF *) 
    END DiscardImage 

(* EXPORTED *) 
; PROCEDURE WriteCheckpoint  
    ( ImageRef : PaintHs . ImageTransientTyp ) 
  : BOOLEAN (* Success *) 
  (* There is a more complete procedure to do this at 
     AssertDevel.WriteCheckpoint.
     This one is just for use without a GUI interface. 
  *) 

  = BEGIN 
      IF ImageRef # NIL 
      THEN 
        ImageRef . ItPers . IpCrashCode := ORD ( MessageCodes . T . NullCode )   
      ; ImageRef . ItPers . IpCrashCommand := UiRecPlay . CurrentCommand ( ) 
      ; TRY 
          Files . WriteImagePickle 
            ( PersistentImageToSave ( ImageRef , ForSave := FALSE ) 
            , Misc . CheckpointName ( ImageRef . ItPers . IpAbsPklFileName )  
            , DoCreateVersion := TRUE 
            )  
        ; ImageRef . ItIsSaved := TRUE 
        EXCEPT
          Files . Error (* ( EMessage ) *)  
        => RETURN FALSE 
        ELSE 
          RETURN FALSE 
        END (* TRY EXCEPT *) 
      ; RETURN TRUE 
      ELSE RETURN FALSE 
      END (* IF *) 
    END WriteCheckpoint 

(* EXPORTED *) 
; PROCEDURE ParseTree 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; InsertNilFixedChildren := FALSE 
    ) 
  RAISES { Backout , Thread . Alerted } 
(* CHECK: Does this really belong here?  Maybe in Display? *) 

  (* Reparse a (possibly) modified tree. *) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LParseInfo : ParseHs . ParseInfoTyp 
  ; VAR LOldEstRef : LbeStd . EstRootTyp 
  ; VAR LNewEstRef : LbeStd . EstRootTyp 
  ; VAR LScannerIf : ScannerIf . ScanIfTyp 

  ; BEGIN (* ParseTree *) 
      LImagePers := ImageRef . ItPers 
    ; LParseInfo . PiLang := LImagePers . IpLang 
    ; LScannerIf 
        := LangUtil . ScannerIfForLang ( LImagePers . IpLang ) 
 (* ; ImageRef . ItScannerIf := LScannerIf These things choke pickles. *) 
    ; LParseInfo . PiScanIf := LScannerIf 
    ; LParseInfo . PiGram := LangUtil . Gram ( LImagePers . IpLang ) 
    ; LParseInfo . PiInsertNilFixedChildren 
        := InsertNilFixedChildren
    ; LOldEstRef := LImagePers . IpEstRoot 
    ; TempMark . BuildTempMarkList ( ImageRef , LParseInfo ) 
    ; IF Options . TreeBrowsing 
      THEN
        TreeBrowse . Browse 
          ( LOldEstRef , LImagePers . IpLang 
          , "After BuildMarkList, before parse" 
          ) 
      END (* IF *) 
    ; Parser . Parse 
        ( LParseInfo 
        , ParseTrv . InitParseEst ( LParseInfo , LOldEstRef ) 
        , LNewEstRef  
        ) 
    ; IF Options . TreeBrowsing 
      THEN
        TreeBrowse . Browse 
          ( LNewEstRef , LImagePers . IpLang 
          , "After Parse, before RebuildMarkList" 
          ) 
      END (* IF *) 
    ; TempMark . RebuildMarkList 
        ( ParseInfo := LParseInfo 
        , ImageRef := ImageRef 
        , OldEstRef := LOldEstRef 
        , NewEstRef := LNewEstRef 
        ) 
    ; IF Options . TreeBrowsing 
      THEN 
        TreeBrowse . Browse 
          ( LNewEstRef , LImagePers . IpLang 
          , "After Parse and after RebuildMarkList" 
          ) 
      END (* IF *)  
    ; EstUtil . UnmarkContainsTempMark ( LOldEstRef ) 
    ; INC ( LImagePers . IpLineCtDisplay , LParseInfo . PiLineCtIncr ) 
    ; LImagePers . IpLineCtIsExact := FALSE 
(* TODO: Note a version. *) 
    END ParseTree 

; BEGIN 
    ImageTable := NEW ( TextRefTbl . Default ) . init ( sizeHint := 100 ) 
  END Images 
. 
