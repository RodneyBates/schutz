
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE UiSearch 

; IMPORT FormsVBT 
; IMPORT Rd 
; IMPORT Rsrc 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Trestle 
; IMPORT TrestleComm 
; IMPORT VBT 

; IMPORT Assertions 
; FROM Assertions IMPORT AssertionFailure 
; IMPORT EditWindow 
; IMPORT Options 
; IMPORT Search 
; IMPORT UiRecPlay 
; IMPORT Worker 

; TYPE WorkerClosureSearchTyp
    = Worker . ClosureTyp OBJECT 
        SearchString : TEXT 
      ; CaseSensitive : BOOLEAN 
      ; Set : BOOLEAN 
      END 

; PROCEDURE StringSearchFwdWorkProc ( Self : WorkerClosureSearchTyp ) 
  RAISES { Thread . Alerted } 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
      := UiRecPlay . BeginCommandPlusStringBool2  
           ( UiRecPlay . CommandTyp . SearchStringFwd 
           , Self . SearchString 
           , Self . CaseSensitive 
           , Self . Set 
           ) 
    ; TRY (* EXCEPT *) 
        EVAL Search . StringSearchFwd 
          ( Self . Window 
          , Self . SearchString 
          , Self . CaseSensitive 
          , StartAtBOI := Self . Set 
          ) 
      EXCEPT
        AssertionFailure => (* Discard. *) <* NOWARN *> 
     (* Let Thread . Alerted through. *) 
      END (* TRY EXCEPT *) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END StringSearchFwdWorkProc

(* VISIBLE: *) 
; PROCEDURE ReplayStringSearchFwd 
    ( SearchString : TEXT ; CaseSensitive : BOOLEAN ; SetToBOI : BOOLEAN ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      IF SearchString # NIL AND NOT Text . Equal ( SearchString , "" ) 
      THEN
        TRY 
          EVAL Worker . RequestWork 
                 ( NEW ( WorkerClosureSearchTyp 
                       , Window 
                           := FormsVBT . GetGeneric 
                                ( Options . MainForm , "Fv_LbeWindow" )   
                       , SearchString := SearchString 
                       , CaseSensitive := CaseSensitive 
                       , Set := SetToBOI 
                       , apply := StringSearchFwdWorkProc 
                       ) 
                 ) 
        EXCEPT Thread . Alerted => 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END ReplayStringSearchFwd 

; PROCEDURE FirstOrNextCallback 
    ( Form : FormsVBT . T 
    ; Time : VBT . TimeStamp 
    ; SetToBOI : BOOLEAN 
    ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *> 
    VAR LSearchString : TEXT 
  ; VAR LIsCaseSensitive : BOOLEAN 
  ; VAR LIsRegexp : BOOLEAN 
  ; VAR LIsSyntactic : BOOLEAN 
  ; VAR LWindow : EditWindow . T 

  ; BEGIN 
      LSearchString := FormsVBT . GetText ( Form , "Sch_SearchString" ) 
    ; LIsCaseSensitive 
        := FormsVBT . GetBoolean ( Form , "Sch_IsCaseSensitive" ) 
    ; LIsRegexp := FormsVBT . GetBoolean ( Form , "Sch_IsRegexp" ) 
    ; LIsSyntactic := FormsVBT . GetBoolean ( Form , "Sch_IsSyntactic" ) 
    ; IF LIsRegexp 
      THEN 
        FormsVBT . PutText 
          ( Form 
          , "Fv_ErrorPopup_Message" 
          , "Regular expression searches are not implemented." 
          )
      ; FormsVBT . PopUp ( Form , "Fv_ErrorPopup" ) 
      ELSIF LIsSyntactic 
      THEN 
        FormsVBT . PutText 
          ( Form 
          , "Fv_ErrorPopup_Message" 
          , "Syntactic searches are not implemented." 
          )
      ; FormsVBT . PopUp ( Form , "Fv_ErrorPopup" ) 
      ELSE 
        LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" ) 
      ; EditWindow . TakeKBFocus ( LWindow , Time )  
      ; EVAL Worker . RequestWorkInteractive  
               ( NEW ( WorkerClosureSearchTyp 
                     , Window := LWindow 
                     , Time := Time 
                     , SearchString := LSearchString 
                     , CaseSensitive := LIsCaseSensitive 
                     , Set := SetToBOI 
                     , apply := StringSearchFwdWorkProc 
                     ) 
               ) 
      END (* IF *) 
    END FirstOrNextCallback 

; PROCEDURE FirstCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      FirstOrNextCallback ( Form , Time , SetToBOI := TRUE ) 
    END FirstCallback 

; PROCEDURE NextCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      FirstOrNextCallback ( Form , Time , SetToBOI := FALSE ) 
    END NextCallback 

; PROCEDURE StringSearchBwdWorkProc ( Self : WorkerClosureSearchTyp ) 
  RAISES { Thread . Alerted } 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommandPlusStringBool2  
           ( UiRecPlay . CommandTyp . SearchStringBwd 
           , Self . SearchString 
           , Self . CaseSensitive 
           , Self . Set 
           ) 
    ; TRY (* EXCEPT *) 
        EVAL Search . StringSearchBwd 
          ( Self . Window 
          , Self . SearchString 
          , Self . CaseSensitive 
          , StartAtEOI := Self . Set 
          ) 
      EXCEPT
        AssertionFailure => (* Discard. *) <* NOWARN *> 
     (* Let Thread . Alerted through. *) 
      END (* TRY EXCEPT *) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END StringSearchBwdWorkProc

(* VISIBLE: *) 
; PROCEDURE ReplayStringSearchBwd 
    ( SearchString : TEXT ; CaseSensitive : BOOLEAN ; SetToEOI : BOOLEAN ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      IF SearchString # NIL AND NOT Text . Equal ( SearchString , "" ) 
      THEN
        TRY 
          EVAL Worker . RequestWork 
                 ( NEW ( WorkerClosureSearchTyp 
                       , Window 
                           := FormsVBT . GetGeneric 
                                ( Options . MainForm , "Fv_LbeWindow" )   
                       , SearchString := SearchString 
                       , CaseSensitive := CaseSensitive 
                       , Set := SetToEOI 
                       , apply := StringSearchBwdWorkProc 
                       ) 
                 ) 
        EXCEPT Thread . Alerted => 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END ReplayStringSearchBwd 

; PROCEDURE LastOrPrevCallback 
    ( Form : FormsVBT . T 
    ; Time : VBT . TimeStamp 
    ; SetToEOI : BOOLEAN 
    ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *> 
    VAR LSearchString : TEXT 
  ; VAR LIsCaseSensitive : BOOLEAN 
  ; VAR LIsRegexp : BOOLEAN 
  ; VAR LIsSyntactic : BOOLEAN 
  ; VAR LWindow : EditWindow . T 

  ; BEGIN 
      LSearchString := FormsVBT . GetText ( Form , "Sch_SearchString" ) 
    ; LIsCaseSensitive 
        := FormsVBT . GetBoolean ( Form , "Sch_IsCaseSensitive" ) 
    ; LIsRegexp := FormsVBT . GetBoolean ( Form , "Sch_IsRegexp" ) 
    ; LIsSyntactic := FormsVBT . GetBoolean ( Form , "Sch_IsSyntactic" ) 
    ; IF LIsRegexp 
      THEN 
        FormsVBT . PutText 
          ( Form 
          , "Fv_ErrorPopup_Message" 
          , "Regular expression searches are not implemented." 
          )
      ; FormsVBT . PopUp ( Form , "Fv_ErrorPopup" ) 
      ELSIF LIsSyntactic 
      THEN 
        FormsVBT . PutText 
          ( Form 
          , "Fv_ErrorPopup_Message" 
          , "Syntactic searches are not implemented." 
          )
      ; FormsVBT . PopUp ( Form , "Fv_ErrorPopup" ) 
      ELSE 
        LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" ) 
      ; EditWindow . TakeKBFocus ( LWindow , Time )  
      ; EVAL Worker . RequestWorkInteractive  
               ( NEW ( WorkerClosureSearchTyp 
                     , Window := LWindow 
                     , Time := Time 
                     , SearchString := LSearchString 
                     , CaseSensitive := LIsCaseSensitive 
                     , Set := SetToEOI 
                     , apply := StringSearchBwdWorkProc 
                     ) 
               ) 
      END (* IF *) 
    END LastOrPrevCallback 

; PROCEDURE PrevCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      LastOrPrevCallback ( Form , Time , SetToEOI := FALSE ) 
    END PrevCallback 

; PROCEDURE LastCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      LastOrPrevCallback ( Form , Time , SetToEOI := TRUE ) 
    END LastCallback 

(* Replace variants: *) 


; TYPE WorkerClosureSearchReplaceTyp
    = Worker . ClosureTyp OBJECT 
        SearchString : TEXT 
      ; ReplaceString : TEXT 
      ; CaseSensitive : BOOLEAN 
      ; ReplaceKind : Search . ReplaceKindTyp 
      END 

; PROCEDURE ReplaceWorkProc ( Self : WorkerClosureSearchReplaceTyp ) 
  RAISES { Thread . Alerted } 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommandPlusString2BoolInt   
             ( UiRecPlay . CommandTyp . ReplaceString  
             , Self . SearchString 
             , Self . ReplaceString 
             , Self . CaseSensitive 
             , ORD ( Self . ReplaceKind ) 
             ) 
    ; TRY (* EXCEPT *) 
        Search . Replace 
          ( Self . Window 
          , Self . SearchString 
          , Self . ReplaceString 
          , Self . CaseSensitive 
          , Self . ReplaceKind  
          ) 
      EXCEPT
        AssertionFailure => (* Discard. *) <* NOWARN *> 
     (* Let Thread . Alerted through. *) 
      END (* TRY EXCEPT *) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END ReplaceWorkProc

(* VISIBLE: *) 
; PROCEDURE ReplayReplace  
    ( SearchString : TEXT 
    ; ReplaceString : TEXT 
    ; CaseSensitive : BOOLEAN 
    ; ReplaceKind : Search . ReplaceKindTyp 
    ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      IF ReplaceString # NIL AND NOT Text . Equal ( ReplaceString , "" ) 
         AND ( ReplaceKind = Search . ReplaceKindTyp . Once 
               OR ( SearchString # NIL 
                    AND NOT Text . Equal ( SearchString , "" ) 
                  ) 
             ) 
      THEN
        TRY 
          EVAL Worker . RequestWork 
                 ( NEW ( WorkerClosureSearchReplaceTyp 
                       , Window 
                           := FormsVBT . GetGeneric 
                                ( Options . MainForm , "Fv_LbeWindow" )   
                       , SearchString := SearchString 
                       , ReplaceString := ReplaceString 
                       , CaseSensitive := CaseSensitive 
                       , ReplaceKind := ReplaceKind 
                       , apply := ReplaceWorkProc 
                       ) 
                 ) 
        EXCEPT Thread . Alerted => 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END ReplayReplace 

; PROCEDURE InnerReplaceCallback 
    ( Form : FormsVBT . T 
    ; Time : VBT . TimeStamp 
    ; ReplaceKind : Search . ReplaceKindTyp 
    ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *> 
    VAR LSearchString : TEXT 
  ; VAR LReplaceString : TEXT 
  ; VAR LIsCaseSensitive : BOOLEAN 
  ; VAR LIsRegexp : BOOLEAN 
  ; VAR LIsSyntactic : BOOLEAN 
  ; VAR LWindow : EditWindow . T 

  ; BEGIN 
      IF Worker . IsIdle ( )  
      THEN 
        LSearchString := FormsVBT . GetText ( Form , "Sch_SearchString" ) 
      ; LReplaceString := FormsVBT . GetText ( Form , "Sch_ReplaceString" ) 
      ; LIsCaseSensitive 
          := FormsVBT . GetBoolean ( Form , "Sch_IsCaseSensitive" ) 
      ; LIsRegexp := FormsVBT . GetBoolean ( Form , "Sch_IsRegexp" ) 
      ; LIsSyntactic := FormsVBT . GetBoolean ( Form , "Sch_IsSyntactic" ) 
      ; IF LIsRegexp 
        THEN 
          FormsVBT . PutText 
            ( Form 
            , "Fv_ErrorPopup_Message" 
            , "Regular expression searches are not implemented." 
            )
        ; FormsVBT . PopUp ( Form , "Fv_ErrorPopup" ) 
        ELSIF LIsSyntactic 
        THEN 
          FormsVBT . PutText 
            ( Form 
            , "Fv_ErrorPopup_Message" 
            , "Syntactic searches are not implemented." 
            )
        ; FormsVBT . PopUp ( Form , "Fv_ErrorPopup" ) 
        ELSIF ReplaceKind = Search . ReplaceKindTyp . All 
        THEN 
          FormsVBT . PutText 
            ( Form 
            , "Fv_ErrorPopup_Message" 
            , "Replace All is not implemented." 
            )
        ; FormsVBT . PopUp ( Form , "Fv_ErrorPopup" ) 
        ELSE 
          LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" ) 
        ; EditWindow . TakeKBFocus ( LWindow , Time )  
        ; EVAL Worker . RequestWorkInteractive  
                 ( NEW ( WorkerClosureSearchReplaceTyp 
                       , Window := LWindow 
                       , Time := Time 
                       , SearchString := LSearchString 
                       , ReplaceString := LReplaceString 
                       , CaseSensitive := LIsCaseSensitive 
                       , ReplaceKind := ReplaceKind 
                       , apply := ReplaceWorkProc 
                       ) 
                 ) 
        END (* IF *) 
      ELSE (* Worker thread is busy. *) 
        EditWindow . Beep ( ) 
      END (* IF *) 
    END InnerReplaceCallback 

; PROCEDURE ReplaceOnceCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      InnerReplaceCallback ( Form , Time , Search . ReplaceKindTyp . Once ) 
    END ReplaceOnceCallback 

; PROCEDURE ReplaceNextCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      InnerReplaceCallback ( Form , Time , Search . ReplaceKindTyp . Next ) 
    END ReplaceNextCallback 

; PROCEDURE ReplaceRestCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      InnerReplaceCallback ( Form , Time , Search . ReplaceKindTyp . Rest ) 
    END ReplaceRestCallback 

; PROCEDURE ReplaceAllCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 

  = BEGIN 
      InnerReplaceCallback ( Form , Time , Search . ReplaceKindTyp . All ) 
    END ReplaceAllCallback 

(* Search and replace window/dialog. *) 

; PROCEDURE InnerAttachSearchHandlers ( Form : FormsVBT . T ) 

  = <* FATAL FormsVBT . Error *>
    BEGIN 
      FormsVBT . AttachProc ( Form , "Sch_First" , FirstCallback ) 
    ; FormsVBT . AttachProc ( Form , "Sch_Next" , NextCallback ) 
    ; FormsVBT . AttachProc ( Form , "Sch_Prev" , PrevCallback ) 
    ; FormsVBT . AttachProc ( Form , "Sch_Last" , LastCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Sch_ReplaceOnce" , ReplaceOnceCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Sch_ReplaceNext" , ReplaceNextCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Sch_ReplaceRest" , ReplaceRestCallback ) 
    ; FormsVBT . AttachProc 
        ( Form , "Sch_ReplaceAll" , ReplaceAllCallback ) 
    END InnerAttachSearchHandlers 

(* VISIBLE: *) 
; PROCEDURE AttachSearchHandlers ( Form : FormsVBT . T ) 

  = BEGIN 
      IF NOT SeparateSearchWindow 
      THEN InnerAttachSearchHandlers ( Form ) 
      END (* IF *) 
    END AttachSearchHandlers 

; TYPE SearchWindowTyp 
    = FormsVBT . T OBJECT 
        SearchForm : FormsVBT . T 
      ; WindowForm : FormsVBT . T 
      END 

(* VISIBLE: *) 
; PROCEDURE SearchWindowCallback 
    ( Form : FormsVBT . T 
    ; <* UNUSED *> Name : TEXT 
    ; <* UNUSED *> EventData : REFANY 
    ; <* UNUSED *> Time : VBT . TimeStamp 
    ) 

  = VAR LSearchForm : SearchWindowTyp 
  ; VAR LWindow : EditWindow . T 
  ; VAR LSplitValue : INTEGER 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      IF SeparateSearchWindow 
      THEN 
        LWindow := FormsVBT . GetGeneric ( Form , "Fv_LbeWindow" )  
      ; IF LWindow . WrSearchForm = NIL 
        THEN 
          TRY 
            LSearchForm 
              := NEW ( SearchWindowTyp ) 
                 . initFromRsrc ( "Search.fv" , Options . ResourcePath )
          EXCEPT 
            Rsrc . NotFound , Rd . Failure 
            => FormsVBT . PutText 
                 ( Options . MainForm , "Fv_ErrorPopup_Message" 
                 , "Unable to locate resource Search.fv" 
                 ) 
            ; FormsVBT . PopUp ( Options . MainForm , "Fv_ErrorPopup" ) 
            ; RETURN 
          | Thread . Alerted 
            => RETURN 
          END (* TRY EXCEPT *) 
        ; TRY
            Trestle . Install ( LSearchForm ) 
          EXCEPT
          | TrestleComm . Failure 
            => FormsVBT . PutText 
                 ( Options . MainForm , "Fv_ErrorPopup_Message" 
                 , "Could not open Search window on display " 
                   & Options . Display 
                 ) 
            ; FormsVBT . PopUp ( Options . MainForm , "Fv_ErrorPopup" ) 
            ; RETURN 
          END (* TRY EXCEPT *) 
        ; LWindow . WrSearchForm := LSearchForm 
        ; LSearchForm . WindowForm := Form 
        ; InnerAttachSearchHandlers ( LSearchForm ) 
        ELSE (* Window already has a search form. *) 
          TRY 
            Trestle . MoveNear ( Form , NIL ) (* Bring to top. *) 
(* FIX: ^This doesn't bring it to the top.  And maybe we want to toggle
        its appearance.  And if it was closed without our being notified,
        it will never be seen again.  And this is possibly all moot, because
        we may not use the separate window UI design anyway.
*)  
          EXCEPT
          | TrestleComm . Failure 
          => FormsVBT . PutText 
               ( Options . MainForm , "Fv_ErrorPopup_Message" 
               , "Could not open Assert on display " & Options . Display 
               ) 
          ; FormsVBT . PopUp ( Options . MainForm , "Fv_ErrorPopup" ) 
          END (* TRY EXCEPT *) 
        END (* IF *) 
      ELSE (* Search Ui is inside the edit window. *) 
        LSplitValue := FormsVBT . GetInteger ( Form , "Sch_TSplit" ) 
      ; FormsVBT . PutInteger ( Form , "Sch_TSplit" , 1 - LSplitValue ) 
      END (* IF *) 
    END SearchWindowCallback  

; BEGIN 
  END UiSearch 
. 

