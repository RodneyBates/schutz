
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE TokStringInfo 

(* Data mapped-to by a Table that is keyed by a string of tokens. 
   Used to instantiate Table.
*) 

; IMPORT LbeStd 

; CONST Brand = "TokStringInfo"

; TYPE TokStringInfoTyp = RECORD 
    LHSTok : LbeStd . TokTyp := LbeStd . Tok__Null 
  ; UseCount : CARDINAL := 0  
  END 

; TYPE TokStringInfoRefTyp = REF TokStringInfoTyp 

; TYPE T = TokStringInfoRefTyp 

; END TokStringInfo 
. 

