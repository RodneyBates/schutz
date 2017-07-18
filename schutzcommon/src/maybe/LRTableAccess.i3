
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LRTableAccess 

(* Utilities for accessing LR parsing tables. *) 

; IMPORT PortTypes 
; IMPORT LbeStd 

(* LR parsing states *) 

; TYPE LRStateTyp = PortTypes . Card16Typ 

; PROCEDURE InitialState ( Lang : LbeStd . LangTyp ) : LRStateTyp 

; PROCEDURE GoTo 
    ( Lang : LbeStd . LangTyp 
    ; FromState : LRStateTyp 
    ; Tok : LbeStd . TokTyp 
    ) 
    : LRStateTyp 

(* Number of grammar symbols *) 

; PROCEDURE SymbolCt 
    ( Lang : LbeStd . LangTyp ) : PortTypes . Card16Typ 

(* Repair costs *) 

; PROCEDURE DeletionCost 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) 
    : LbeStd . RepairCostTyp 

; PROCEDURE InsertionCost 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) 
    : LbeStd . RepairCostTyp 

; PROCEDURE ShortParseCheckCost 
    ( Lang : LbeStd . LangTyp 
    ; Expected : LbeStd . LimitedTokCtTyp 
    ; Actual : LbeStd . LimitedTokCtTyp 
    ; Accepted : BOOLEAN 
    ) 
    : LbeStd . RepairCostTyp 
  (* Return additional cost due to parse check being shorter than 
     desired.  Take into account the case where input has been accepted. *) 

; PROCEDURE GetAscendingInsertionCostTok 
    ( Lang : LbeStd . LangTyp 
    ; I : PortTypes . Card16Typ 
    ; VAR Tok : LbeStd . TokTyp 
    ; VAR Cost : LbeStd . RepairCostTyp 
    ) 
  (* The Ith grammar symbol and its cost, in ascending order of cost *) 

(* Action tables *) 

; TYPE ActionKindTyp 
    = { ActKindAccept , ActKindShift , ActKindReduce , ActKindErr } 

(* A flattened variant record: *) 
; TYPE ActionTyp 
    = RECORD 
(* TODO: Figure out why on earth the compiler thinks this is a packed
         record and then objects that fields are crossing word boundaries. *) 
        ActKind : BITS 16 FOR ActionKindTyp 
      (* ActKind = ActKindShift: *) 
      ; ActShiftLRState : LRStateTyp 
      ; ActShiftTok : LbeStd . TokTyp 
      (* ActKind = ActKindReduce: *) 
      ; ActReduceRightSideLen : PortTypes . Card16Typ
        (* ^ PortTypes . Card8Typ is plenty *)
      ; ActReduceLeftSide : LbeStd . TokTyp 
      (* ActKind = ActKindErr: *) 
      ; ActErrCode : LbeStd . ErrCodeTyp 
      END (* RECORD  ActionTyp *) 

; PROCEDURE GetAction 
    ( Lang : LbeStd . LangTyp 
    ; LrState : LRStateTyp 
    ; Tok : LbeStd . TokTyp 
    ; VAR Result : ActionTyp 
    ) 

; PROCEDURE GetContinAction 
    ( Lang : LbeStd . LangTyp 
    ; LrState : LRStateTyp 
    ; VAR Result : ActionTyp 
    ) 

; END LRTableAccess 
. 
