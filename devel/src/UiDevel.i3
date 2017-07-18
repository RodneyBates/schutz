
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

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

; PROCEDURE ShowGuiAssertDialog ( Location , Message : TEXT ) 

; PROCEDURE RemoveGuiAssertDialog ( ) 

; PROCEDURE ShowCheckpointNotice ( Message: TEXT (* May be multiline. *) ) 

; PROCEDURE ShowDebugOptions ( ) 

; END UiDevel  
. 
