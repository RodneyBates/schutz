
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LRUtils 

(* Collected utilities for CFG analysis. *) 

; FROM Failures IMPORT Backout
; IMPORT IntSets 
; IMPORT LbeStd 
; IMPORT LALRTypes 
; IMPORT LRTable 

; PROCEDURE ComputeShortestDerivable ( Gram : LRTable . GrammarTyp ) 
  (* For each nonterminal, compute the length of the shortest 
     derivable terminal string.  Set the ShortestDerivable fields in
     each production and each nonterminal. 
  *) 

; PROCEDURE NullableNonterms ( Gram : LRTable . GrammarTyp ) 
  : IntSets . T (* Nonterminals that can derive empty. *) 
  (* Die Nichtterminale welche nullable sind *) 

; PROCEDURE ComputeNullabilityAndFirst ( Gram : LRTable . GrammarTyp ) 
  (* For each AstNonterminal and Nonterminal, compute its nullability
     (in Gram . NullableNts) and its FIRST set (in field First of its
     NonterminalInfo).
  *) 

; PROCEDURE ComputeLast ( Gram : LRTable . GrammarTyp ) 
  (* For each AstNonterminal and Nonterminal, compute its LAST 
     set (in field Last of its NonterminalInfo).
  *) 

; PROCEDURE ComputeLastItemLR0Hash ( Gram : LRTable . GrammarTyp ) 
  (* Compute item hash for the last item of the last state, using LR(0) 
     treatment of the item.  Includes only Prod and DotPos. 
  *) 

; PROCEDURE ItemsCompare 
    ( Gram : LRTable . GrammarTyp 
    ; Left , Right : LALRTypes . ItemSsTyp 
    ; AutomatonKind : LALRTypes . AutomatonKindTyp 
    ) 
  : [ - 1 .. 1 ] 
  (* Compare according to an arbitrary but repeatable total ordering. 
     But for readability of dumped item sets, put nonclosure items first. 
     PRE: Left and Right are valid item subscripts. 
  *) 
  (* For LR(0), we consider only the production and dot position. 
     For LR(1), list or set, also consider the LR1 lookahead set.
  *) 

; PROCEDURE ComputeNewestStateHashContributions ( Gram : LRTable . GrammarTyp ) 
  (* Of the newest Item, to the hash codes of the newest state. *) 

; PROCEDURE AreEqualStates 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs1 , StateSs2 : LALRTypes . StateSsTyp 
    ; AutomatonKind : LALRTypes . AutomatonKindTyp 
    ) 
  : BOOLEAN 
  RAISES { Backout } 
  (* We only look at the core items, since sets with the same core will 
     either have the same closure or will have sets of closure items that 
     differ only in the insertion of Tok__Empty in one or more places. 
     Such insertions make no difference to either the GOTO or the parsing
     actions, so we consider the states to be equivalent. 
  *) 

; PROCEDURE CheckLRwithLALR ( Gram : LRTable . GrammarTyp ) 
  (* Compare LR(1) lookahead token sets with LALR(1) lookaheads for merged
     states. 
  *) 

; PROCEDURE InitItemTbl ( Gram : LRTable . GrammarTyp ) 

; PROCEDURE UniqueLR0Item  
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs :  LALRTypes . ItemSsTyp 
    ; VAR IsNew : BOOLEAN 
    ) 
  : LALRTypes . ItemSsTyp 
  (* If there is already an equal item in Gram.LR0ItemTbl, set 
     IsNew FALSE and return its item subscript. 
     Otherwise, set IsNew TRUE and return ItemSs.
  *) 

; PROCEDURE InitStateTbls ( Gram : LRTable . GrammarTyp ) 

; PROCEDURE UniqueState   
    ( Gram : LRTable . GrammarTyp 
    ; StateSs :  LALRTypes . StateSsTyp 
    ; VAR IsNew : BOOLEAN 
    ) 
  : LALRTypes . StateSsTyp 
  (* If there is already an equal state in Gram . LR0ListStateTbl, 
     Set IsNew FALSE and return its state subscript. 
     Otherwise, set IsNew TRUE and return StateSs.
  *) 

; PROCEDURE ShortestDerivable 
    ( Gram : LRTable . GrammarTyp ; Tok : LbeStd . TokTyp ) 
  : LbeStd . LimitedTokCtTyp 

; PROCEDURE StripGrammar ( Gram : LRTable . GrammarTyp ) 
  (* Remove stuff only needed during LR generation. *) 

; END LRUtils 
. 



