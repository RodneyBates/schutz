
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LRTableAccess 

(* Utilities for accessing LR parsing tables. *) 

; IMPORT PortTypes 
; IMPORT LbeStd 

(* LR parsing states *) 

(* VISIBLE: *) 
; PROCEDURE InitialState ( Lang : LbeStd . LangTyp ) : LRStateTyp 

  = BEGIN
      RETURN 0 
    END InitialState

(* VISIBLE: *) 
; PROCEDURE GoTo 
    ( Lang : LbeStd . LangTyp 
    ; FromState : LRStateTyp 
    ; Tok : LbeStd . TokTyp 
    ) 
    : LRStateTyp 

  = BEGIN 
      RETURN 0 
    END GoTo 

(* VISIBLE: *) 
; PROCEDURE SymbolCt 
    ( Lang : LbeStd . LangTyp ) : PortTypes . Card16Typ 
(* Number of grammar symbols *) 

  = BEGIN
      RETURN 0 
    END SymbolCt 

(* Repair costs *) 

(* VISIBLE: *) 
; PROCEDURE DeletionCost 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) 
    : LbeStd . RepairCostTyp 

  = BEGIN
      RETURN LbeStd . RepairCostInfinity
    END DeletionCost 

(* VISIBLE: *) 
; PROCEDURE InsertionCost 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) 
    : LbeStd . RepairCostTyp 

  = BEGIN
      RETURN LbeStd . RepairCostInfinity
    END InsertionCost 

(* VISIBLE: *) 
; PROCEDURE ShortParseCheckCost 
    ( Lang : LbeStd . LangTyp 
    ; Expected : LbeStd . LimitedTokCtTyp 
    ; Actual : LbeStd . LimitedTokCtTyp 
    ; Accepted : BOOLEAN 
    ) 
    : LbeStd . RepairCostTyp 
  (* Return additional cost due to parse check being shorter than 
     desired.  Take into account the case where input has been accepted. *) 

  = BEGIN
      RETURN LbeStd . RepairCostInfinity
    END ShortParseCheckCost 

(* VISIBLE: *) 
; PROCEDURE GetAscendingInsertionCostTok 
    ( Lang : LbeStd . LangTyp 
    ; I : PortTypes . Card16Typ 
    ; VAR Tok : LbeStd . TokTyp 
    ; VAR Cost : LbeStd . RepairCostTyp 
    ) 
  (* The Ith grammar symbol and its cost, in ascending order of cost *) 

  = BEGIN
    END GetAscendingInsertionCostTok 

(* Action tables *) 

(* VISIBLE: *) 
; PROCEDURE GetAction 
    ( Lang : LbeStd . LangTyp 
    ; LrState : LRStateTyp 
    ; Tok : LbeStd . TokTyp 
    ; VAR Result : ActionTyp 
    ) 

  = BEGIN
    END GetAction 

(* VISIBLE: *) 
; PROCEDURE GetContinAction 
    ( Lang : LbeStd . LangTyp 
    ; LrState : LRStateTyp 
    ; VAR Result : ActionTyp 
    ) 

  = BEGIN
    END GetContinAction 

; BEGIN 
  END LRTableAccess 
. 
