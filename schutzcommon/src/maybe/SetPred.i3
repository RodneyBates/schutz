
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE SetPred 

(* An algebra that captures a predicate applied with quantifiers to some set. *)

(* Let Dom be a set and Pred be a predicate on the members of Dom.
   From these, derive a function SP (Dom,Pred), usually just SP.
   Let Sets be the powerset of Dom.  SP:Sets->T, with the following
   property:
     Let S be a member of Sets.
     SP(S) = SpNever if NOT Pred(s), for all s IN S.
     SP(S) = SpAlways if Pred(s), for all s in S.
     SP(S) = SpSometimes if Pred(s) AND NOT Pred(t), for some s,t in S.
     SP(S) = SpUnknown if unknown which of the above holds.
     
   This module defines an AND-like and an OR-like operator on T, that 
   preserves the above definition, without needing to actually know
   Dom, Pred, or SP.
*) 

; TYPE T 
    = { SpUnknown   
      , SpNever     
      , SpSometimes 
      , SpAlways    
      } 

; TYPE SpSetTyp = SET OF T 

; CONST SpSetKnown = SpSetTyp { T . SpNever , T . SpSometimes , T . SpAlways } 

; CONST SpSetPossiblyTrue 
    = SpSetTyp { T . SpUnknown , T . SpSometimes , T . SpAlways } 

; CONST SpSetPossiblyFalse 
    = SpSetTyp { T . SpUnknown , T . SpNever , T . SpSometimes } 

; CONST SpSetKnownPossiblyTrue = SpSetTyp { T . SpSometimes , T . SpAlways } 

; CONST SpSetKnownPossiblyFalse = SpSetTyp { T . SpNever , T . SpSometimes } 

; CONST SpSetPossiblyFalseOrTrue = SpSetTyp { T . SpUnknown .. T . SpAlways } 

; PROCEDURE Image ( Value : T ) : TEXT  
  
; PROCEDURE And ( Left , Right : T ) : T 

; PROCEDURE Or ( Left , Right : T ) : T 

; END SetPred 
. 

