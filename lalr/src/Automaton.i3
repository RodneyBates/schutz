
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Automaton.md, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

INTERFACE Automaton 

; FROM Failures IMPORT Backout 
; IMPORT LbeStd 
; IMPORT LRTable 

(* Required protocol: call sequence must fit the following 
   regular expression: 
     InitAutomaton 
     AddOperator* 
     (AddProduction|AddExternalProduction)* 
     AfterProductions 
     Build 
*) 

(* All procedures in this interface return harmlessly if Gram = NIL. *) 

; PROCEDURE InitAutomaton 
    ( Gram : LRTable . GrammarTyp 
    ; ProdCt : LRTable . ProdNoTyp 
    ; FirstTerminal : LbeStd . TokTyp 
    ; LastTerminal : LbeStd . TokTyp 
    ; FirstAstNonterminal : LbeStd . TokTyp 
    ; LastAstNonterminal : LbeStd . TokTyp 
    ; FirstNonterminal : LbeStd . TokTyp 
    ; LastNonterminal : LbeStd . TokTyp 
    ; StartSymbol : LbeStd . TokTyp 
    ) 

  (* When productions are added, StartSymbol must not appear in any 
     RHS and must appear as the LHS of exactly one production, whose 
     RHS must not be empty. *) 

; PROCEDURE FirstPrec 
    ( Gram : LRTable . GrammarTyp ) : LRTable . PrecedenceTyp 

; PROCEDURE NextPrec 
    ( Gram : LRTable . GrammarTyp ; Arg : LRTable . PrecedenceTyp ) 
    : LRTable . PrecedenceTyp 

; PROCEDURE AddOperator 
    ( Gram : LRTable . GrammarTyp 
    ; Operator : LbeStd . TokTyp 
      (* ^Must be a terminal. *) 
    ; Precedence : LRTable . PrecedenceTyp 
    ; Associativity : LRTable . AssocTyp 
    ) 
  (* Call this any time after InitAutomaton. *) 

; PROCEDURE NoteNontermKind 
    ( Gram : LRTable . GrammarTyp 
    ; Nonterm : LbeStd . TokTyp 
    ; Kind : LRTable . NontermKindTyp 
    ; DefaultOnly : BOOLEAN := FALSE 
      (* ^Set it only if it is now NtkNull *) 
    ) 
  (* Call this any time after InitAutomaton. *) 

; PROCEDURE AddExternalProduction 
    ( Gram : LRTable . GrammarTyp 
    ; Left : LbeStd . TokTyp (* A Nonterminal *) 
    ; Right : LRTable . TokArrayRefTyp 
      (* ^May be shared. May be NIL to denote the empty string. *) 
    ; PrecedenceTok : LbeStd . TokTyp := LbeStd . Tok__Null 
      (* Otherwise, a terminal *) 
    ; IsList : BOOLEAN := FALSE 
    ; OptionIdSet := LRTable . OptionIdSetEmpty
    ; BuildTok : LbeStd . TokTyp := LbeStd . Tok__Null 
    ) 
  RAISES { Backout } 
  (* Call this any time after InitAutomaton and all calls on AddOperator. 
     Takes predecence and associativity from PrecedenceTok, if non-null, 
     otherwise by a default rule (from rightmost terminal in RHS). 
  *) 

; PROCEDURE AddProduction 
    ( Gram : LRTable . GrammarTyp 
    ; Left : LbeStd . TokTyp (* A Nonterminal *) 
    ; Right : LRTable . TokArrayRefTyp 
      (* ^May be shared. May be NIL to denote the empty string. *) 
    ; Precedence : LRTable . PrecedenceTyp := LRTable . PrecedenceNull  
    ; Associativity := LRTable . AssocTyp . none 
    ; IsList : BOOLEAN := FALSE 
    ; OptionIdSet := LRTable . OptionIdSetEmpty
    ; BuildTok : LbeStd . TokTyp := LbeStd . Tok__Null 
    ) 
  RAISES { Backout } 
  (* Call this any time after InitAutomaton. *) 

; PROCEDURE ActualProdCt 
    ( Gram : LRTable . GrammarTyp ) : LRTable . ProdNoTyp  
  (* Call this any time after InitAutomaton. *) 

; PROCEDURE AfterProductions ( Gram : LRTable . GrammarTyp ) 
  RAISES { Backout } 
  (* Some processing done after all productions have been added, but before
     Build.  This once needed to be separated from Build.
  *) 

; PROCEDURE Build ( Gram : LRTable . GrammarTyp ) 
  RAISES { Backout } 
  (* Build the LR(0) automaton, after precedence info and productions 
     have been inserted. *) 

; END Automaton 
. 
