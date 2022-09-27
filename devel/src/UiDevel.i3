
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* UiDevel provides functions in the Devel pulldown menu. *)

INTERFACE UiDevel 

; IMPORT FormsVBT 

; PROCEDURE AttachHandlers ( Form : FormsVBT . T ) 

; PROCEDURE ReplayWriteCheckpoint ( ) 

; PROCEDURE ReplayTakeFocus ( ) 

; PROCEDURE ReplayRepaint ( ) 

; PROCEDURE ReplayReconstructLines ( ) 

; PROCEDURE ReplayVerifyLinesRefs ( ) 

; PROCEDURE ReplayForceAssert ( ) 

; PROCEDURE ReplayMergeText ( ) 

; PROCEDURE ReplayBrowseEst ( ) 

; PROCEDURE ReplaySetDebugLevel ( Level : INTEGER ) 

; PROCEDURE ReplayWriteStats ( FileName : TEXT ) 

; PROCEDURE ReplayWriteEstPickle ( FileName : TEXT ) 

; PROCEDURE ReplayGenEstModule ( FileName : TEXT ) 

; PROCEDURE ReplayWriteParseInfo ( FileName : TEXT ) 

; PROCEDURE ReplayWriteFsTrees ( FileName : TEXT ) 

; PROCEDURE ReplayWriteSemPickle ( FileName : TEXT ) 

; PROCEDURE ReplayGenTokInterface ( FileName : TEXT ) 

; PROCEDURE ReplayGenChildInterface ( FileName : TEXT ) 

; PROCEDURE ShowGuiAssertDialog ( Failure , Location , Checkpoint : TEXT ) 
  RAISES { FormsVBT . Error } 

; PROCEDURE RemoveGuiAssertDialog ( ) 

; PROCEDURE ShowCheckpointNotice ( Message: TEXT (* May be multiline. *) ) 

(* Replaced.  Eventually remove. *) 
; PROCEDURE ShowDebugOptionsx ( ) 

; END UiDevel  
. 
