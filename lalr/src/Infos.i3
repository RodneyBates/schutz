
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Infos.md, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Print some informations about the generation *) 

INTERFACE Infos 

; IMPORT Wr 

; IMPORT Assertions 
; IMPORT IntSets 
; IMPORT LALRTypes 
; IMPORT LRTable 

; CONST LRDotCharText = "\260" (* ISO latin-1 Degree symbol. *) 
; CONST LRDotString = " " & LRDotCharText & " " 

; PROCEDURE ReprKindImage ( Rep : LALRTypes . ReprKindTyp ) : TEXT 

; PROCEDURE StateKindImage ( Kind : LALRTypes . StateKindTyp ) : TEXT 

; PROCEDURE AssocTypImage ( Kind : LRTable . AssocTyp ) : TEXT 

; PROCEDURE WritePrecAndAssocList   
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

; PROCEDURE WriteSsList 
    ( <* UNUSED *> Gram : LRTable . GrammarTyp 
    ; Array : LALRTypes . SsArrayRefTyp  
    ; Used : LALRTypes .  ListCountTyp
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 
  (* Uses whole lines. *) 

; PROCEDURE WriteProd 
    ( Gram : LRTable . GrammarTyp 
    ; ProdSs : LRTable . ProdNoTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

; PROCEDURE WriteProductions 
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

; PROCEDURE WriteTokSet 
    ( Gram : LRTable . GrammarTyp 
    ; Set : IntSets . T
 
    ; WrT : Wr . T := NIL 
      (* NIL means use Messages . MessageWr ( ) *) 
    ; FirstLinePrefix : TEXT := "" 
    ) 

; PROCEDURE WriteItem 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : LALRTypes . ItemSsTyp 
    ; WrT : Wr . T := NIL 
      (* NIL means use Messages . MessageWr ( ) *) 
    ) 

; PROCEDURE WriteItemSet 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : LALRTypes . StateSsTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

; PROCEDURE WriteState 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : LALRTypes . StateSsTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

; PROCEDURE WriteStates 
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

; PROCEDURE WriteInfo 
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 
  (* Ausgabe von statistischen Informationen auf file 'Wr' *) 

; PROCEDURE WriteLRTables 
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    )
  RAISES { Assertions . AssertionFailure } 

; END Infos 
. 

