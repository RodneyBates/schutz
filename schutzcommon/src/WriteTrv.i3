
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE WriteTrv 

(* Procedures for traversing Ests and generating text or token streams. *) 

; IMPORT Thread 

; FROM Failures IMPORT Backout 
; IMPORT PaintHs 
; IMPORT SharedStrings 
; IMPORT Strings 

; TYPE DeliverLineProcTyp 
    = PROCEDURE 
        ( ImageRef : PaintHs . ImageTransientTyp 
        ; Line : Strings . StringTyp 
        ) 
      RAISES { Backout , Thread . Alerted } 

; TYPE DeliverTokProcTyp 
    = PROCEDURE 
        ( ImageRef : PaintHs . ImageTransientTyp 
        ; StringRef : SharedStrings . T 
        ) 
      RAISES { Backout , Thread . Alerted } 

; PROCEDURE WriteText 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; DeliverLineProc : DeliverLineProcTyp 
    ; DoGenerateText : BOOLEAN := TRUE 
    ; DoGenerateErrors : BOOLEAN := FALSE 
    ) 
    RAISES { Backout , Thread . Alerted } 

; PROCEDURE WriteToks 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; DeliverTokProc : DeliverTokProcTyp 
    ; CmntsWanted : BOOLEAN 
    ) 
    RAISES { Backout , Thread . Alerted } 

; END WriteTrv 
. 
