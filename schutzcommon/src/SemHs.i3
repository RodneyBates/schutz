
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE SemHs 

(* Language-independent data structure for incremental semantic analysis 
   during editing.
   Not in use yet.   
*) 

; IMPORT LbeStd 
; IMPORT PortTypes 

; TYPE SortTyp = PortTypes . Card16Typ 

; CONST BottomSort = 1 
; CONST IntSort = 2 
; CONST BoolSort = 3 
; CONST RealSort = 4 
; CONST SpixSort = 5 
; CONST BioSort = 6 

; TYPE SortClassTyp 
    = { ListSortClass , NodeSortClass , ClassSortClass , IntSortClass 
      , BoolSortClass 
      , RealSortClass , StringSortClass , IdSortClass , TermSortClass 
      } 
; TYPE AttrRefTyp <: ADDRESS 

; PROCEDURE SortClass 
    ( Lang : LbeStd . LangTyp ; Sort : SortTyp ) : SortClassTyp 

; PROCEDURE IsSubsort ( Left , Right : SortTyp ) : BOOLEAN 

; PROCEDURE Sup ( Left , Right : SortTyp ) : SortTyp 

; PROCEDURE ListLen ( ListAttrRef : AttrRefTyp ) : PortTypes . Card16Typ 

; PROCEDURE NodeChildCt 
    ( Lang : LbeStd . LangTyp ; NodeSort : SortTyp ) : PortTypes . Card16Typ 

; PROCEDURE GetChild 
    ( Parent : AttrRefTyp 
    ; ChildNo : PortTypes . Card16Typ 
    ; VAR Result : AttrRefTyp 
    ) 

; PROCEDURE SortOfAttr ( AttrRef : AttrRefTyp ) : SortTyp 

; TYPE VarNoTyp = PortTypes . Card16Typ 

; PROCEDURE IsVar ( AttrRef : AttrRefTyp ) : BOOLEAN 

; PROCEDURE VarNo ( AttrRef : AttrRefTyp ) : VarNoTyp 

; PROCEDURE IsOperator ( AttrRef : AttrRefTyp ) : BOOLEAN 

; END SemHs 
. 
