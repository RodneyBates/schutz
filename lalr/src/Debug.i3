
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Debug.md, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Compute debugging information *) 

INTERFACE Debug 

; IMPORT Wr 

; IMPORT IntSets 
; IMPORT LALRTypes
; IMPORT LbeStd 
; IMPORT LRTable

; TYPE tConflict = { ShRed , RedRed , ShRedRed } 

; VAR NoTrace , Fast : BOOLEAN 

; VAR DebugWr : Wr . T 

; PROCEDURE DebugHead 
    ( Gram : LRTable . GrammarTyp 
    ; State : LRTable . StateNoTyp 
    ; ConflictSet : IntSets . T 
    ) 

; PROCEDURE DebugState 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : LRTable . StateNoTyp 
    ; ConflictingLookaheadTokSet : IntSets . T 
    ) 
(* Erzeuge Zusatzinformation zum Zustand 'State' mit Konfliktmenge 'Set' *) 

; PROCEDURE DebugEnd ( ) 

; PROCEDURE InformIgnored 
    ( Gram : LRTable . GrammarTyp 
    ; Item : LALRTypes . ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

; PROCEDURE InformSublist 
    ( Gram : LRTable . GrammarTyp 
    ; Item : LALRTypes . ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

; PROCEDURE InformLowPri 
    ( Gram : LRTable . GrammarTyp 
    ; Item : LALRTypes . ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

; PROCEDURE InformLeftAss 
    ( Gram : LRTable . GrammarTyp 
    ; Item : LALRTypes . ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

; PROCEDURE InformRightAss 
    ( Gram : LRTable . GrammarTyp 
    ; Item : LALRTypes . ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

; PROCEDURE InformKept 
    ( Gram : LRTable . GrammarTyp 
    ; Item : LALRTypes . ItemSsTyp 
    ; LookaheadTok : LbeStd . TokTyp 
    ) 

; PROCEDURE InformConflict ( kind : tConflict ) 

; PROCEDURE NewLine ( ) 

; END Debug 
. 

