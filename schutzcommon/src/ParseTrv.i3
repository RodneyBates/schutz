
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE ParseTrv 

(* Parse traverser.  This module traverses a tree for purposes of reparsing. 
   It feeds the Parser with a stream of tokens with attached Est fragments 
   as semantics data.  Some of the tokens are nonterminals, with whole Est 
   subtrees attached.  It invokes scanning incrementally, as needed.  It 
   also can read characters from a file for batch parsing, presenting the
   same interfaces to the Parser and scanner as for reparsing an Est.   
*) 

; IMPORT Assertions 
; IMPORT LbeStd 
; IMPORT ParseHs 
; IMPORT Rd 
; IMPORT Thread 

; FROM Assertions IMPORT AssertionFailure 

(* Interface to parser *) 

; PROCEDURE PrependToken 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; ParseTravStateRef : ParseHs . ParseTravStateRefTyp 
    ; TokInfo : ParseHs . TokInfoTyp 
    ) 
  : ParseHs . ParseTravStateRefTyp 
  (* Return a parse traverse state with TokInfo added at left 
     end of input stream. *) 

; TYPE SuccKindTyp = { SuccKindAdvance , SuccKindDescend } 

; PROCEDURE NextParseTravState 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; FromStateRef : ParseHs . ParseTravStateRefTyp 
    ; SuccKind : SuccKindTyp 
    ) 
  : ParseHs . ParseTravStateRefTyp 
  RAISES { Thread . Alerted , AssertionFailure } 
  (* Get the next parse traversal state, after one in hand. 
     When the current state is a whole subtree, (which implies 
     it contains no syntactic modifications,) 
     SuccKindDescend will force the parse traverser to start 
     down through the children, whereas SuccKindAdvance will 
     move beyond the subtree.  SuccKindAdvance must be used 
     when the current state is a terminal. 
  *) 

; PROCEDURE InitParseEst 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp ; EstRef : LbeStd . EstRootTyp ) 
  : ParseHs . ParseTravStateRefTyp 
  RAISES { AssertionFailure } 
  (* Get an initial parse traversal state for a tree. *) 

; PROCEDURE InitParseFile 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp ; File : Rd . T ) 
  : ParseHs . ParseTravStateRefTyp 
  (* Get an initial parse traversal state for a file. *) 

; PROCEDURE InitParseKbd 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; PosRelTo : LbeStd . LimitedCharNoTyp 
    ) 
  : ParseHs . ParseTravStateRefTyp 
  (* Get an initial parse traversal state to parse from keyboard. *) 

; END ParseTrv 
. 
