
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* 2022-8-10: The functions in this module are replaced by code in
   module Worker.  The whole thing can be removed.
*)


MODULE GuiAssert 

; IMPORT VBT
; IMPORT FormsVBT
; IMPORT Trestle 
; IMPORT TrestleComm
; IMPORT Rsrc 
; IMPORT Thread 
; IMPORT Text 

; IMPORT Assertions 
; IMPORT Options 

; TYPE AssertClosureTyp 
    = FormsVBT . Closure 
        OBJECT 
          DoTerminate : BOOLEAN := FALSE 
        OVERRIDES 
          apply := AssertApply 
        END (* OBJECT *) 

; VAR AssertClosure : AssertClosureTyp 

; PROCEDURE AssertApply 
    ( Closure : AssertClosureTyp 
    ; Form : FormsVBT . T 
    ; Name : TEXT 
    ; Time : VBT . TimeStamp 
    ) 

  = BEGIN (* AssertApply *)
(* TODO: Add record/playback for these actions. *)  
      IF Text . Equal ( Name , "Fv_Assert_Continue" ) 
      THEN 
        Closure . DoTerminate := FALSE 
      ELSIF Text . Equal ( Name , "Fv_Assert_Checkpoint" ) 
      THEN 
(* FIX: Write the checkpoint here. *) 
        Closure . DoTerminate := FALSE 
      ELSIF Text . Equal ( Name , "Fv_Assert_Terminate" ) 
      THEN 
        Closure . DoTerminate := TRUE  
      ELSE 
        Closure . DoTerminate := TRUE  
      END (* IF *) 
    ; Trestle . Delete ( Form ) 
    END AssertApply 

; VAR AssertForm : FormsVBT . T 

; PROCEDURE AssertCallback ( Location , Message : TEXT ) : BOOLEAN 

  = BEGIN
      IF AssertForm = NIL 
      THEN RETURN Assertions . RaiseOnFailure 
      ELSE  
        FormsVBT . PutText 
          ( AssertForm , "Fv_Assert_Location" , Location  ) 
      ; FormsVBT . PutText 
          ( AssertForm , "Fv_Assert_Message" , Message )
      ; TRY
          Trestle . Install ( AssertForm ) 
        EXCEPT
        | TrestleComm . Failure 
          => FormsVBT . PutText 
               ( Options . MainForm , "Fv_ErrorPopup_Message" 
               , "Could not open Assert on display " & Options . Display 
               ) 
          ; FormsVBT . PopUp ( Options . MainForm , "Fv_ErrorPopup" ) 
          ; RETURN Assertions . RaiseOnFailure 
        END (* TRY EXCEPT *) 
      ; FormsVBT . PutText 
          ( Options . MainForm , "Fv_Assert_Location" , Location  ) 
      ; FormsVBT . PutText 
          ( Options . MainForm , "Fv_Assert_Message" , Message )
      ; FormsVBT . PopUp ( Options . MainForm , "Fv_Assert" ) 
      ; FormsVBT . MakeDormant ( Options . MainForm , "Fv_Background" ) 
(* FIX: This won't work.  Trestle locks the global VBT.mu prior to
        giving an event to Lbe, and holds the lock until the event
        processing is done.  If we get an assert failure, we will
        undoubtedly need VBT.mu for the display of the assert dialog.
        So, we need to have a separate thread executing the core Lbe
        operations and a main thread handling events.  With this
        organization, it should be possible to revert to having
        the assertion dialog be a ZChassis of the main form. 
*) 
      ; Trestle . AwaitDelete ( AssertForm ) 
      ; FormsVBT . MakeActive ( Options . MainForm , "Fv_Background" ) 
      ; FormsVBT . PopDown ( Options . MainForm , "Fv_Assert" ) 
      ; RETURN AssertClosure . DoTerminate  
      END 
    END AssertCallback 

(* VISIBLE: *) 
; PROCEDURE AttachHandlers ( ) 

  = BEGIN (* AttachHandlers *) 
      AssertClosure := NEW ( AssertClosureTyp ) 
    ; AssertClosure . DoTerminate := FALSE 
    ; TRY 
        AssertForm 
          := NEW ( FormsVBT . T ) 
             . initFromRsrc ( "Assert.fv" , Options . ResourcePath )
      EXCEPT 
        Rsrc . NotFound 
        => FormsVBT . PutText 
             ( Options . MainForm , "Fv_ErrorPopup_Message" 
             , "Unable to locate resource Assert.fv" 
             ) 
        ; FormsVBT . PopUp ( Options . MainForm , "Fv_ErrorPopup" ) 
        ; AssertForm := NIL 
        ; RETURN 
      | Thread . Alerted 
        => AssertForm := NIL 
        ; RETURN 
      END 
    ; FormsVBT . Attach 
        ( AssertForm , "Fv_Assert_Checkpoint" , AssertClosure ) 
    ; FormsVBT . Attach 
        ( AssertForm , "Fv_Assert_Continue" , AssertClosure ) 
    ; FormsVBT . Attach 
        ( AssertForm , "Fv_Assert_Terminate" , AssertClosure ) 
 (* ; Assertions . Callback := AssertCallback *) 
    END AttachHandlers 

; BEGIN 
  END GuiAssert 
. 

