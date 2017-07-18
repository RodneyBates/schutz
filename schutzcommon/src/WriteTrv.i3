
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE WriteTrv 

(* Procedures for traversing Ests and generating text or token streams. *) 

; IMPORT Thread 

; IMPORT Strings 
; IMPORT SharedStrings 
; IMPORT PaintHs 
; IMPORT Assertions 

; TYPE DeliverLineProcTyp 
    = PROCEDURE 
        ( ImageRef : PaintHs . ImageTransientTyp 
        ; Line : Strings . StringTyp 
        ) 
      RAISES { Assertions . AssertionFailure , Thread . Alerted } 

; TYPE DeliverTokProcTyp 
    = PROCEDURE 
        ( ImageRef : PaintHs . ImageTransientTyp 
        ; StringRef : SharedStrings . T 
        ) 
      RAISES { Assertions . AssertionFailure , Thread . Alerted } 

; PROCEDURE WriteText 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; DeliverLineProc : DeliverLineProcTyp 
    ; DoGenerateText : BOOLEAN := TRUE 
    ; DoGenerateErrors : BOOLEAN := FALSE 
    ) 
    RAISES { Assertions . AssertionFailure , Thread . Alerted } 

; PROCEDURE WriteToks 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; DeliverTokProc : DeliverTokProcTyp 
    ; CmntsWanted : BOOLEAN 
    ) 
    RAISES { Assertions . AssertionFailure , Thread . Alerted } 

; END WriteTrv 
. 
