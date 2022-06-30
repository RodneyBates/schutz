
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Images 

; IMPORT Thread 

; IMPORT TextRefTbl 

; IMPORT PaintHs 

; FROM Failures IMPORT Backout 

; EXCEPTION NoMoreWindows 

; PROCEDURE ConnectImageToWindow 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; WindowRef : PaintHs . WindowRefTyp 
    ) 
  RAISES { NoMoreWindows } 

; PROCEDURE PersistentImageToSave  
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; ForSave : BOOLEAN 
    ) 
  : PaintHs . ImagePersistentTyp  
  RAISES { Backout , Thread . Alerted } 
  (* If ForSave, has SIDE EFFECT of TextEdit . FlushEdit, because this must 
     be done to prevent the cleaning up from losing information. 
  *) 

; PROCEDURE DisconnectImageFromWindow
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; WindowRef : PaintHs . WindowRefTyp 
    ) 
  RAISES { Backout } 

; PROCEDURE DisconnectImageFromAllWindows 
    ( ImageRef : PaintHs . ImageTransientTyp )
  RAISES { Backout } 

; PROCEDURE DiscardImage ( ImageRef : PaintHs . ImageTransientTyp )  
  (* ^No action if ImageRef is in any window. *) 

; PROCEDURE ParseTree 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; InsertNilFixedChildren := FALSE 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Reparse a (possibly) modified tree. *) 

; VAR ImageTable : TextRefTbl . T 
(* TODO: ^Abstract this some way. *) 

; PROCEDURE WriteCheckpoint  
    ( ImageRef : PaintHs . ImageTransientTyp ) 
  : BOOLEAN (* Success *) 
  (* There is a more complete procedure to do this at 
     AssertDevel.WriteCheckpoint.
     This one is just for use without a GUI interface. 
  *) 

; END Images 
. 


