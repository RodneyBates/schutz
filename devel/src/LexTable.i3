
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LexTable  

(* A fast two-way mapping between integers and strings.  For string-to-int,
   uses a lexical scanner whose transition table is prebuilt by 
   BuildLexMachine.  For the reverse, uses a directly subscripted
   array.
*)    

; IMPORT Assertions 
; IMPORT PortTypes

; TYPE ValueTyp = PortTypes . Card16Typ 
; CONST ValueNull = LAST ( ValueTyp ) 
; TYPE T <: REFANY

; TYPE ValidCharTyp = [ ' ' .. '~' ] (* Printable chars. *)   

; PROCEDURE ToText ( Table : T ; Value : ValueTyp ) : TEXT 
  (* NIL if Value not in Table. *) 

; PROCEDURE ValueFromChars ( Table : T ; READONLY Name : ARRAY OF CHAR ) 
  : ValueTyp 
  (* ValueNull if Name is not in Table. *) 

; PROCEDURE ValueFromText ( Table : T ; Name : TEXT ) : ValueTyp 
  RAISES { Assertions . AssertionFailure } 
  (* ValueNull if Name is not in Table. *) 

; END LexTable 
. 
