
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LRTable 

(* Internal data used in generating and accessing LALR parsing tables. *) 

; IMPORT Fmt 

; IMPORT Assertions 
; IMPORT LbeStd 
; IMPORT PortTypes 

; TYPE StateNoTyp = LbeStd . LRStateTyp 
  (* Used for a renumbering of states, after much of the analysis 
     is done. Field Action has the new number.  Denotes a state only for 
     states we actually will push on the parse stack.  States that 
     have a single, LR(0) reduce have the number of the production to 
     reduce by, added to a base above the last numbered state in field 
     Action.  Values in this range mean shift, then reduce.  Another 
     range of production numbers lies above this, to denote simple 
     reduce actions. *) 

; CONST StateNoImage = LbeStd . LRStateImage 

; CONST StateNoNull = 0 
        (* Some arrays need this to be a valid subscript. *) 

; TYPE StateNoArrayRefTyp = REF ARRAY OF StateNoTyp 

; TYPE TokArrayRefTyp = REF ARRAY OF LbeStd . TokTyp 

; TYPE BaseRefTyp = REF ARRAY OF CARDINAL 

; TYPE ParseActionTyp = PortTypes . Int32Typ 
  (* State numbers and two ranges of production numbers in a single 
     value space. *) 

; TYPE ParseActionArrayRefTyp = REF ARRAY OF ParseActionTyp 

; TYPE ActionTableElemTyp 
    = RECORD Check : LbeStd . TokTyp ; Next : ParseActionTyp END (* RECORD *) 

; CONST ActionTableElemNull 
    = ActionTableElemTyp { Next := StateNoNull , Check := StateNoNull } 

; TYPE ActionTableRefTyp = REF ARRAY (* StateNoTyp *) OF ActionTableElemTyp 

; TYPE ContinuationRefTyp 
    = REF ARRAY (* LbeStd . TokTyp *) OF LbeStd . TokTyp 

; TYPE ProdNoTyp = PortTypes . Int32Typ 

; CONST ProdNoNull = FIRST ( ProdNoTyp ) 
; CONST ProdNoInfinity = LAST ( ProdNoTyp ) 

; PROCEDURE ProdNoImage ( ProdNo : ProdNoTyp ) : TEXT 

; TYPE NontermKindTyp 
    = { NtkNull 
      , NtkUnused 
      , NtkAbstractOnly
      , NtkAbstractAndConcrete 
      , NtkSublist 
      , NtkPartial 
      , NtkConcreteOnly 
      } 

; TYPE NontermKindSetTyp = SET OF NontermKindTyp 

; CONST NontermKindSetShouldTerminate 
   = NontermKindSetTyp 
       { NontermKindTyp . NtkPartial 
       , NontermKindTyp . NtkAbstractAndConcrete 
       , NontermKindTyp . NtkConcreteOnly  
       } 
  (* ^Things that should derive a terminal string. *) 

; CONST NontermKindSetShouldBeBuildable 
   = NontermKindSetTyp 
       { NontermKindTyp . NtkAbstractOnly 
       , NontermKindTyp . NtkAbstractAndConcrete 
       } 
  (* ^Things that should build an Ast. *) 

; CONST NontermKindSetShouldBeDerivable 
   = NontermKindSetTyp 
       { NontermKindTyp . NtkConcreteOnly 
       , NontermKindTyp . NtkSublist
       , NontermKindTyp . NtkPartial  
       } 
  (* ^Things that the start symbol should derive. *) 

; CONST MaxOptionId = BITSIZE ( INTEGER ) 
  (* ^Hopefully, the compiler's simplest, native-word-sized set. *)  
  (* FIXME: This makes pickles non-interchangeable between different
            word sizes.
  *) 

; TYPE OptionIdTyp = [ 0 .. MaxOptionId ] 
; CONST OptionIdNull = LAST ( OptionIdTyp )
; TYPE OptionIdRealTyp = [ 0 .. OptionIdNull - 1 ] 
; TYPE OptionIdSetTyp = SET OF OptionIdRealTyp 

; CONST OptionIdForListTrail = FIRST ( OptionIdTyp ) 
; CONST OptionIdSetForListTrail = OptionIdSetTyp { OptionIdForListTrail } 
  (* ^For a list kind that has optional trailing separators, these are
     used to communicate that they are actually present. *)  

; CONST (* PROCEDURE *) OptionIdImage = Fmt . Int 

; PROCEDURE OptionIdSetImage ( Set : OptionIdSetTyp ) : TEXT 

; CONST OptionIdSetEmpty = OptionIdSetTyp { } 

; TYPE ReduceInfoTyp 
    = RECORD 
        LhsTok : LbeStd . TokTyp 
      ; BuildTok : LbeStd . TokTyp 
      ; RhsLen : LbeStd . TokNoTyp 
      ; OptionIdSet : OptionIdSetTyp 
      END (* RECORD *) 

; TYPE ReduceInfoArrayRefTyp 
    = REF ARRAY (* Production no. *) OF ReduceInfoTyp 

; TYPE PrecedenceTyp = PortTypes . Int16Typ 

; CONST PrecedenceImage = Fmt . Int 

; CONST PrecedenceNull = FIRST ( PrecedenceTyp ) (* Negative. *) 
; CONST PrecedenceLowestReal = 0 
; CONST PrecedenceMax = LAST ( PrecedenceTyp )  

; TYPE AssocTyp = { right , left , none , nonassoc } 

; PROCEDURE AssocImage ( Val : AssocTyp ) : TEXT 

; TYPE AssocSetTyp = SET OF AssocTyp 
; CONST AssocSetLeftOrNonAssoc 
          = AssocSetTyp { AssocTyp . left , AssocTyp . nonassoc } 
; CONST AssocSetRightOrNonAssoc  
          = AssocSetTyp { AssocTyp . right , AssocTyp . nonassoc } 

; TYPE CostArrayRefTyp = REF ARRAY OF LbeStd . RepairCostTyp 

; TYPE Public 
    = OBJECT 
        IsBnf : BOOLEAN := TRUE 
      ; IsGenerated : BOOLEAN := FALSE 
      ; FactoredClasses : BOOLEAN := FALSE 
      ; FactoredSubstrings : BOOLEAN := FALSE 
      ; FactoredAlts : BOOLEAN := FALSE 
      ; ProdCt : ProdNoTyp := 0 (* Anzahl Produktionen *) 
      ; ItemCt := 0 
      ; LR1ListStateCt := 0 
      ; LR1ReorderedStateCt := 0 
      ; LR1SetStateCt := 0 
      ; LR0ListStateCt := 0 
      ; LR1SetStateMergeCt : CARDINAL := 0 
      ; LR0ListStateMergeCt : CARDINAL := 0 
      ; FirstTerminal : LbeStd . TokTyp 
      ; LastTerminal : LbeStd . TokTyp 
      ; FirstAstNonterminal : LbeStd . TokTyp 
      ; LastAstNonterminal : LbeStd . TokTyp 
        (* ^Nonterminals that are also Ast nodes. *) 
      ; FirstNonterminal : LbeStd . TokTyp 
      ; LastNonterminal : LbeStd . TokTyp 
        (* ^Nonterminals that are NOT Ast nodes. *) 
      (* Terminals, AstNonterminals, Nonterminals must be numbered in that 
         order and contiguous. 
       *) 
      ; StartSymbol : LbeStd . TokTyp (* Nonterminal *) 
        (* neues Startsymbol, das nur in einer Regel auftritt *) 
      ; StartStateNo : StateNoTyp 
      ; AcceptAction : ParseActionTyp 
      ; AcceptProdNo : ProdNoTyp := ProdNoNull 
      ; FirstReadAction : ParseActionTyp 
      ; FirstReadRedAction : ParseActionTyp 
      ; FirstReduceAction : ParseActionTyp 
      ; ActionCt : ParseActionTyp 
      ; ReduceInfoRef : ReduceInfoArrayRefTyp := NIL 
        (* Subscripted by ProdNoTyp *) 
      ; ActionTableRef : ActionTableRefTyp := NIL 
        (* Subscripted by sort of a TokTyp, but with an increment 
           added which was found in BaseRef ^ *) 
      ; ContinuationRef : ContinuationRefTyp := NIL 
        (* Subscripted by StateNoTyp *) 
      ; BaseRef : BaseRefTyp := NIL 
        (* Subscripted by StateNoTyp *) 
      ; DefaultRef : StateNoArrayRefTyp := NIL 
        (* Subscripted by StateNoTyp *) 
      ; NNextRef : ParseActionArrayRefTyp := NIL 
        (* Subscripted by sort of a TokTyp, but with an increment 
           added which was found in BaseRef ^ *) 
      ; NBaseRef : BaseRefTyp := NIL 
        (* Subscripted by StateNoTyp *) 
      ; DeletionCostRef : CostArrayRefTyp 
        (* Subscripted by terminals and ast nonterminals, biased by 
           FirstTerminal *) 
      ; InsertionCostRef : CostArrayRefTyp 
        (* Subscripted by terminals and ast nonterminals, biased by
           FirstTerminal *) 
      ; AscendingInsertionCostRef : TokArrayRefTyp 
        (* Insertable tokens, in order of ascending cost. *) 
      METHODS 
        tokImage ( Tok : LbeStd . TokTyp ) : TEXT := TokImageDefault 
        (* A callback, to be overridden by a client. *) 
      END (* OBJECT Public *) 

; PROCEDURE TokImageDefault ( Self : Public ; Tok : LbeStd . TokTyp ) : TEXT 

; TYPE GrammarTyp <: Public 

; PROCEDURE EmptyString ( ) : TokArrayRefTyp 
  (* Could be shared.  Treat as immutable. *) 

; PROCEDURE SingletonString ( Tok : LbeStd . TokTyp ) 
  : TokArrayRefTyp
  (* Could be shared.  Treat as immutable. *) 

; PROCEDURE IsEmptyString 
    ( String : TokArrayRefTyp ; Tok : LbeStd . TokTyp ) 
  : BOOLEAN 
  (* String is empty.  Did I really need to say that? *) 

; PROCEDURE IsSingletonString 
    ( String : TokArrayRefTyp ; Tok : LbeStd . TokTyp ) 
  : BOOLEAN 
  (* String contains exactly one token. *) 

; PROCEDURE IsSingletonStringOf 
    ( String : TokArrayRefTyp ; Tok : LbeStd . TokTyp ) 
  : BOOLEAN 
  (* String contains exactly one token, and it is Tok. *) 

; PROCEDURE Action 
    ( Gram : GrammarTyp 
    ; State : LbeStd . LRStateTyp 
    ; Tok : LbeStd . TokTyp 
    ) 
  : LbeStd . LRStateTyp 
  RAISES { Assertions . AssertionFailure } 

; PROCEDURE AcceptProdNo ( Gram : GrammarTyp ) : ProdNoTyp 

; PROCEDURE Continuation 
    ( Gram : GrammarTyp ; State : LbeStd . LRStateTyp ) : LbeStd . TokTyp 
  RAISES { Assertions . AssertionFailure } 

; PROCEDURE ShortParseCheckCost 
    ( Gram : GrammarTyp 
    ; Expected : LbeStd . LimitedTokCtTyp 
    ; Actual : LbeStd . LimitedTokCtTyp 
    ; Accepted : BOOLEAN 
    ) 
  : LbeStd . RepairCostTyp 
  (* Return additional cost due to parse check being shorter than 
     desired.  Take into account the case where input has been accepted. *) 

; END LRTable 
. 
