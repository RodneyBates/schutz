
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE SuffixInfo 

(* Data structure concerning file name suffixes. *) 

; IMPORT LbeStd 

; TYPE T 
    = RECORD 
        FileName : TEXT 
      ; Tok : LbeStd . TokTyp := LbeStd . Tok__Null 
        (* Top AST node for this file suffix. *) 
      ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
      ; IsPickle : BOOLEAN := FALSE 
      END (* RECORD *) 
  (* ^This type is used in three different ways, all of which use different
     subsets of its fields:
     1) The SuffixInfoTbl: Tok, Lang
     2) The HardSuffixInfoTbl: FileName, Lang
     3) For passing around as parameters: Tok, Lang, IsPickle
  *) 

; CONST Null 
    = T { FileName := NIL 
        , Tok := LbeStd . Tok__Null 
        , Lang := LbeStd . LangNull 
        , IsPickle := FALSE 
        } 

; CONST Brand = "SuffixInfo.T" 

; END SuffixInfo 
. 


