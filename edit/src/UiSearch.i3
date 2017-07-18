
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE UiSearch 

; IMPORT FormsVBT 
; IMPORT VBT 

; IMPORT Search 

; VAR SeparateSearchWindow : BOOLEAN := FALSE 

; PROCEDURE ReplayStringSearchFwd 
    ( SearchString : TEXT ; CaseSensitive : BOOLEAN ; SetToBOI : BOOLEAN ) 

; PROCEDURE ReplayStringSearchBwd 
    ( SearchString : TEXT ; CaseSensitive : BOOLEAN ; SetToEOI : BOOLEAN ) 

; PROCEDURE ReplayReplace  
    ( SearchString : TEXT 
    ; ReplaceString : TEXT 
    ; CaseSensitive : BOOLEAN 
    ; ReplaceKind : Search . ReplaceKindTyp 
    ) 

; PROCEDURE SearchWindowCallback 
    ( Form : FormsVBT . T 
    ; Name : TEXT 
    ; EventData : REFANY 
    ; Time : VBT . TimeStamp 
    ) 
(* Call this when item search-and-replace of edit menu has been clicked. *) 

; PROCEDURE AttachSearchHandlers ( Form : FormsVBT . T ) 
(* Call this after creation of Form for an edit window. *) 

; END UiSearch 
. 


