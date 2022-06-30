
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Search 

; IMPORT Thread 

; FROM Failures IMPORT Backout 
; IMPORT LbeStd 
; IMPORT PaintHs 

; PROCEDURE ClearMatch 
    ( Window : PaintHs . WindowRefTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
      (* Set to TRUE if anything happens that requires a repaint. *) 
    ) 

; PROCEDURE SetMatchAndRepaint 
    ( Window : PaintHs . WindowRefTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ; LineNo : LbeStd . LineNoTyp 
    ; FromPos : LbeStd . CharNoTyp 
    ; ToPos : LbeStd . CharNoTyp 
    ; NewCursorLineNoInWindow : LbeStd . LineNoSignedTyp 
      (* ^Not just where it is wanted.  Must be where it would be if there
          were no scrolling, which could be outside the window.
      *) 
    ; Forward : BOOLEAN 
    ; PredHintMark : PaintHs . LineMarkMeatTyp := NIL 
    ; SuccHintMark : PaintHs . LineMarkMeatTyp := NIL 
    ) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE StringSearchFwd  
    ( Window : PaintHs . WindowRefTyp 
    ; SearchString : TEXT 
    ; CaseSensitive : BOOLEAN 
    ; StartAtBOI : BOOLEAN := FALSE 
    ) 
  : BOOLEAN (* String was found (and cursor moved to it.) *) 
  RAISES { Backout , Thread . Alerted } 

; PROCEDURE StringSearchBwd  
    ( Window : PaintHs . WindowRefTyp 
    ; SearchString : TEXT 
    ; CaseSensitive : BOOLEAN 
    ; StartAtEOI : BOOLEAN := FALSE 
    ) 
  : BOOLEAN (* String was found (and cursor moved to it.) *) 
  RAISES { Backout , Thread . Alerted } 

; TYPE ReplaceKindTyp 
    = { Once (* Only replace matched string. *) 
      , Next (* Replace matched string and do a search for next. *) 
      , Rest (* Replace matched string and all subsequent occurrences in 
                this image. *)  
      , All  (* Replace all occurrences in all open images. *) 
      } 

; PROCEDURE Replace 
    ( Window : PaintHs . WindowRefTyp 
    ; SearchString : TEXT 
    ; ReplaceString : TEXT 
    ; CaseSensitive : BOOLEAN 
    ; ReplaceKind : ReplaceKindTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

; END Search 
. 


