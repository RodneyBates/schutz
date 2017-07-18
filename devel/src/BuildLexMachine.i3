
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE BuildLexMachine 

(* Build a table for LexTable to use.  Entire table must be built
   before it can be used.  Calls must follow the protocol given by\
   the regular expression:
     MakeEmpty AddPair* Build
*) 

; IMPORT LexTable 

(* VISIBLE *) 
; PROCEDURE MakeEmpty ( ) 

(* VISIBLE *) 
; PROCEDURE AddPair 
    ( AddString : TEXT 
    ; Value : LexTable . ValueTyp 
    ; ReverseMap : BOOLEAN := TRUE
      (* Also build the reverse mapping for this pair. *)   
    ) 

(* TODO: Make this detect and report
         1) duplicate strings 
     and 2) duplicate values with ReverseMap
*) 

(* VISIBLE *) 
; PROCEDURE Build ( ) : LexTable . T

; END BuildLexMachine 
. 
