
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LexTableRep

; IMPORT PortTypes 
; IMPORT LexTable   

; TYPE TransitionTyp = PortTypes . Int16Typ 
; CONST NoTransition = LAST ( TransitionTyp ) 
; CONST LastRealTransition = NoTransition - 1 
; CONST FirstRealValue = FIRST ( TransitionTyp ) 
(* Negative "transitions" are mapped-to values, biased by FirstRealValue . 
   NoTransition means there is none.  
   Nonnegative transitions < NoTransition are unbiased states to go to. 
*) 

; CONST NullChar 
    = VAL ( ORD ( FIRST ( LexTable . ValidCharTyp ) ) - 1 , CHAR ) 

; TYPE StateNoTyp = [ 0 .. LastRealTransition ] 

; TYPE SpaceSsTyp = PortTypes . Card32Typ 
; TYPE SpaceTyp = ARRAY (* SpaceSsTyp *) OF TransitionTyp  
  (* A single array of transitions with concatenated transition subranges for
     the various states, in no particular order. *) 
; TYPE SpaceRefTyp = REF SpaceTyp 

; TYPE StateTyp 
    = RECORD 
        Min : CHAR := LAST ( CHAR ) 
      ; Max : CHAR := FIRST ( CHAR ) 
      ; SpaceBias : PortTypes . Int32Typ := FIRST ( SpaceSsTyp ) 
        (* ^Add this to ORD of a character to get a Space subscript. *)    
      END 

; TYPE StatesTyp = ARRAY (* StateNoTyp *) OF StateTyp 
; TYPE StatesRefTyp = REF StatesTyp 

; TYPE NamesTyp 
    = ARRAY (* LexTable . ValueTyp, relative to MinValue *) OF TEXT 
; TYPE NamesRefTyp = REF NamesTyp 

; TYPE TableTyp 
    = RECORD 
        SpaceRef : SpaceRefTyp 
      ; StatesRef : StatesRefTyp
      ; MinValue : LexTable . ValueTyp 
      ; MaxValue : LexTable . ValueTyp 
      ; NamesRef : NamesRefTyp 
      END 

; TYPE TableRefTyp = REF TableTyp 

; REVEAL LexTable . T = BRANDED "LexTable.T" REF TableTyp 

; END LexTableRep  
. 
