
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LALRTypes 

(* This interface has types and low level operations thereon, used only by
   the LALR processing code in subdirectory lalr.
*) 

; IMPORT IntIntTbl 

; IMPORT IntSets 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT PortTypes 

; VAR UseReadReduceStates : BOOLEAN := FALSE 

; TYPE HashCodeTyp = PortTypes . Int32Typ 
  (* ^32-bit, regardless of host native word size. *) 
; CONST HashCodeMaxUns = 16_FFFFFFFF (* Unsigned. *) 

; TYPE ListCountTyp = PortTypes . Card32Typ  

; TYPE SsTyp = PortTypes . Int32Typ 
; TYPE SsArrayRefTyp = REF ARRAY OF SsTyp 

; TYPE ProdNoArrayRefTyp = REF ARRAY OF LRTable . ProdNoTyp 

(* Derived information about nonterminals. *) 

; TYPE NontermInfoTyp 
    = RECORD 
        Used : ListCountTyp := 0 
      ; ProdListRef : ProdNoArrayRefTyp := NIL 
      ; First : IntSets . T := NIL 
      ; Last : IntSets . T := NIL 
        (* ^Sets of terminals and Ast nonterms that can be first/last in a 
           string derivable from this NT. *)  
      ; ShortestDerivable : LbeStd . LimitedTokCtTyp 
        (* ^For all productions with this LHS symbol. *) 
      ; Kind : LRTable . NontermKindTyp := LRTable . NontermKindTyp . NtkNull 
      END (* RECORD *) 

; TYPE NontermInfoArrayRefTyp 
    = REF 
        ARRAY (* LbeStd . TokTyp, Biased FirstNonterminal *) 
        OF NontermInfoTyp 

(* Operators. *) 

; TYPE OperTyp 
    = RECORD 
        Precedence : LRTable . PrecedenceTyp 
      ; Ass : LRTable. AssocTyp 
      END (* RECORD *) 

; TYPE OperArrayRefTyp 
    = REF ARRAY (* LbeStd . TokTyp, Biased FirstTerminal *) OF OperTyp 

(* Productions. *) 

; TYPE ProdTyp 
    = RECORD 
        ProdNo : LRTable . ProdNoTyp 
      ; ReduceStates : StateSsListTyp 
(* TODO: ^2012-2-17, This is not effectively used. *) 
      ; Right : LRTable . TokArrayRefTyp 
      ; ShortestDerivable : PortTypes . Int32Typ 
      ; Len : LbeStd . TokNoTyp 
      ; BuildTok : LbeStd . TokTyp 
      ; Left : LbeStd . TokTyp (* Nonterminal *) 
      ; Precedence : LRTable . PrecedenceTyp 
      ; Ass : LRTable. AssocTyp 
      ; IsList : BOOLEAN 
        (* Denotes a generated production of form L' ::= L' <sep> L', 
           created from an Ast list rule. *) 
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      END (* RECORD *) 

; TYPE ProdVarArrayTyp = (* REFANY *) REF ARRAY OF ProdTyp  
  (* Revealed in LALRRep.i3 to be IntProdVarArray.T.  
     We can't declare it here that way without introducing cyclic imports:
  *) 

(* Representative Items. 
   A distinguished item within an item set is the _representative_ 
   (Repraesentant) of several with similar properties. 
*) 

; TYPE ReprKindTyp 
    = { NoRep             (* kein Repraesentant *) 
      , TermRep           (* Repraesentant fuer einen Terminaluebergang *) 
      , AstNontermRep     (* Repraesentant fuer einen Nichtterminaluebergang 
                             die ein Ast buildet *) 
      , NontermRep        (* Repraesentant fuer einen Nichtterminaluebergang *)
                          (* In one item set, there is at most one of TermRep,
                             AstNontermRep, or NontermRep, for a token. *) 
      , RedRep            (* Repraesentant fuer eine Reduktion *) 
                          (* In one item set, there is at most one of RedRep
                             for a production. *) 
      , TermRepIgn        (* TermRep, ignored to repair a conflict. *) 
      , AstNontermRepIgn  (* AstNontermRep, ignored to repair a conflict. *) 
      , NontermRepIgn     (* NontermRep, ignored to repair a conflict. *) 
      , RedRepIgn         (* RedRep, ignored to repair a conflict. *) 
      } 

; PROCEDURE IgnoredCounterpart ( Val : ReprKindTyp ) : ReprKindTyp 
  (* Converts any of TermRep, NontermRep, AstNontermRep, or RedRep to its 
     ignored counterpart, otherwise identity.
  *) 

; TYPE ReprKindSetTyp = SET OF ReprKindTyp 

; CONST ReprKindSetSentential 
    = ReprKindSetTyp { ReprKindTyp . TermRep , ReprKindTyp . AstNontermRep } 

; CONST ReprKindSetRedOrSentential 
    = ReprKindSetTyp 
        { ReprKindTyp . TermRep 
        , ReprKindTyp . AstNontermRep 
        , ReprKindTyp . RedRep 
        } 

; CONST ReprKindSetShift
    = ReprKindSetTyp 
        { ReprKindTyp . TermRep 
        , ReprKindTyp . AstNontermRep 
        , ReprKindTyp . NontermRep 
        } 

; CONST ReprKindSetAllNonterm 
    = ReprKindSetTyp { ReprKindTyp . AstNontermRep , ReprKindTyp . NontermRep } 
; CONST ReprKindSetRedOrNonterm 
    = ReprKindSetTyp 
        { ReprKindTyp . RedRep 
        , ReprKindTyp . AstNontermRep 
        , ReprKindTyp . NontermRep 
        } 

(* Items. *) 

; TYPE ItemMarkTyp = PortTypes . Card32Typ 

; CONST ItemMarkInfinity = LAST ( ItemMarkTyp ) 
; CONST ItemMarkMax = ItemMarkInfinity - 1  

; TYPE ItemSsTyp = SsTyp 

; CONST ItemSsNull = FIRST ( ItemSsTyp ) 

; TYPE ItemTyp 
    = RECORD 
        GoToStateSs : StateSsTyp  (* Folgezustand *) 
      ; PrevItemSs : ItemSsTyp    (* For a core item, the item that led to it
                                     via a transition.  For a closure item,
                                     the item it closes.  *) 
      ; ProdSs : LRTable . ProdNoTyp        (* Anzahl der Produktion *) 
      ; DotPosition : LbeStd . TokNoTyp 
                                  (* Position innerhalb der Produktion 
                                     des Punktes *) 
      ; RepItemSs : ItemSsTyp     (* ItemSs des zug. Repraesentanten *) 
        (* My be self-referential, or, if a symbol follows the dot, gives
           the item that represents this one. 
        *) 
      ; Rep : ReprKindTyp         (* Art des Uebergans falls Repraesentant *) 
      ; Set : IntSets . T         (* DR / Read / Follow bzw LookAheadSet *) 
          (* Terminals and nonterminals. *) 
      ; OrigSet : IntSets . T     (* Copy of Set, before conflict repairs. *) 
      ; ReadSet : IntSets . T     (* Read fuer Fehlerbehandlung *) 
      ; LR1Set : IntSets . T      (* Full LR(1) lookahead set. *) 
      ; Relation : ItemSsListTyp  (* reads / includes bzw. lookback *) 
      ; ReadsRelation : ItemSsListTyp 
        (* ^Saved copy of reads, for evidence. *) 
      ; LR0ItemHashCode : HashCodeTyp := 0  
        (* ^For nonclosure item, treated as LR(0). *) 
      ; ReadTok : LbeStd . TokTyp (* Symbol hinter dem Punkt. 
                                     = LbeStd . Tok__Null , falls keiner. *) 
      ; Number : ItemMarkTyp      (* Marke fuer Digraph *) 
      END (* RECORD *) 

; TYPE ItemVarArrayTyp = (* REFANY *) REF ARRAY OF ItemTyp 
  (* Revealed in LALRRep.i3 to be IntItemVarArray.T.  
     We can't declare it here that way without introducing cyclic imports:
  *) 

(* Item lists. *) 

; TYPE ItemSsArrayRefTyp = SsArrayRefTyp 

; TYPE ItemSsListTyp 
    = RECORD 
        Used : ListCountTyp 
      ; Array : ItemSsArrayRefTyp 
      END (* RECORD *) 

(* States. *) 

; TYPE StateSsTyp = SsTyp 
  (* Used for subscripts to StateArrayRef ^, the original 
     state numbering when the LR(0) automaton is constructed. *) 
; CONST StateSsNull = 0 
; CONST FirstRealStateSs = 1 (* This will be the start state. *) 
  (* Zero is reserved as an error action in parse tables. *) 
(* CHECK: Would it be cleaner to exploit the fact that states get 
          a StateNo, distinct from their subscript and only reserve 
          zero in that numbering? *) 

; TYPE HashSsTyp = [ 0 .. 508 ] (* value count = 509 is prime. *) 
(* TODO: Fix to use a prime near some constant multiple of the number 
         of nonterminals, or some other estimate of state count. *) 

; TYPE StateKindTyp 
    = { None    (* nicht eingetragen *) 
      , Lookup  (* When entering this state, push it on 
                   the parse stack as usual and find the 
                   next action from the parsing table row 
                   for this state. 
                *) 
      , Reduce  (* Read, then Reduce. An LR(0) state. 
                   When about to enter this state, instead, shift 
                   and then reduce, using a production number 
                   from the only reduce item, which in this case, is 
                   identified by ReduceItemSs.  
                *) 
      , Accept  (* Shift EOF, then reduce by S' -> S <EOF> 
                   Actually, can just stop. 
                *) 
      } 

; TYPE StateKindSetTyp = SET OF StateKindTyp 

; TYPE StateTyp 
    = RECORD 
        SetOrderList : ItemSsArrayRefTyp 
        (* List of item subscripts, in item-canonical order (according to
           LRUtils.ItemsCompare) of just tne nonclosure items.  Used to 
           compare two states for equality as sets. *)  
      ; ItemCt : ItemSsTyp := 0  (* Anzahl Items in Menge. *) 
      ; NonclosureItemCt : ItemSsTyp := 0  
      ; FirstItemSs : ItemSsTyp  (* Subscript of first item in ItemArrayRef^. 
                                    The items in a set are contiguous there. *)
      ; ReduceItemSs : ItemSsTyp (* If Kind IN {Reduce, Accept}, this is the 
                                    Item number of the only reduce item. *) 
      ; Action : LRTable . StateNoTyp      
                                 (* After renumbering. If Kind = Lookup, this 
                                    will the the state number of the table row 
                                    for this state. 
                                    If Kind IN { Reduce, Accept }, this 
                                    will be a biased production number of 
                                    the production to reduce by, after 
                                    shifting the symbol that lead to this
                                    state. *) 
      ; LR1ListHashCode : HashCodeTyp := 0 
        (* ^For state as a list of LR(1) items. *) 
      ; LR1SetHashCode : HashCodeTyp := 0 
        (* ^For state as a set of LR(1) items. *)
      ; LR0ListHashCode : HashCodeTyp := 0 
        (* ^For state as a list of LR(0) items. *)
      ; LR1SetRepStateSs : StateSsTyp 
      ; LR0ListRepStateSs : StateSsTyp 
        (* ^The (LR(1) List) state that represents this LR(1)Set or LR(0)List 
           state.  Points to self, if is its own representative. *)  
      ; LR1SetStateSet : IntSets . T 
      ; LR0ListStateSet : IntSets . T  
        (* ^Other LR(1)Set or LR(0)List states this state represents 
           (Not including itself. *) 
      ; Kind : StateKindTyp := StateKindTyp . None 
      ; IsTargetOfAstNTTransition : BOOLEAN := FALSE 
      END (* RECORD StateTyp *) 

; TYPE StateVarArrayTyp = (* REFANY *) REF ARRAY OF StateTyp 
  (* Revealed in LALRRep.i3 to be IntStateVarArray.T.  
     We can't declare it here that way without introducing cyclic imports:
  *) 

(* Lists of states. *) 

; TYPE StateSsArrayRefTyp = SsArrayRefTyp 

; TYPE StateSsListTyp 
    = RECORD 
        Used : ListCountTyp 
      ; Array : StateSsArrayRefTyp 
      END (* RECORD *) 

(* Revelation of LRTable . GrammerTyp.  Here are fields that are only
   accessed by code within the lalr subdirectory. 
*) 

(* Automaton construction kinds. *) 

; TYPE AutomatonKindTyp  
    = { AkLR1List  (* States are lists of LR(1) items. *) 
      , AkLR1Set   (* States are sets of LR(1) items. *) 
      , AkLR0List  (* States are lists of LR(0) items. *) 
      } 

; TYPE LR0ItemTblTyp <: IntIntTbl . Default 
; TYPE LR1ListTblTyp <: IntIntTbl . Default 
; TYPE LR1SetTblTyp <: IntIntTbl . Default 
; TYPE LR0ListTblTyp <: IntIntTbl . Default 

; REVEAL LRTable . GrammarTyp 
    =  LRTable . Public 
       BRANDED "GrammarTyp" 
       OBJECT 
         ProdArrayRef : ProdVarArrayTyp := NIL (* alle Produktionen *) 
       ; NextProdSs : LRTable . ProdNoTyp := 0 
         (* ^Index naechste P. *) 
       ; ItemArrayRef : ItemVarArrayTyp := NIL (* alle Items *) 
       ; NextItemSs : ItemSsTyp := 0 (* Index akt. bzw. naechstes I *) 
       ; StateArrayRef : StateVarArrayTyp := NIL (* alle States *) 
         (* Element zero is reserved as a Null state, and, in 
            StateArrayRef ^, is unoccupied. *) 
       ; NextStateSs : StateSsTyp := 0 (* Index naechste Zustand. *) 
       ; NontermInfoArrayRef : NontermInfoArrayRefTyp := NIL 
         (* enthaelt Liste von Produktionen mit gleicher linken Seite *) 
       ; OperArrayRef : OperArrayRefTyp := NIL 
       ; AugmentProdSs : LRTable . ProdNoTyp := 0 
       ; Filling : PortTypes . Int32Typ 
       ; NFilling : PortTypes . Int32Typ 
       ; NullableNTs : IntSets . T
       ; LR1ListStateTbl : LR1ListTblTyp 
       ; LR1SetStateTbl : LR1SetTblTyp  
       ; LR0ListStateTbl : LR0ListTblTyp   
       (* Used to atomize structurally equal item sets/lists of stated kind. *) 
       ; LR0ItemTbl : LR0ItemTblTyp 
       (* Distinct LR(1) items that have the the same kernel will just be
          represented by one item, with a uned lookahead set, which is ignored
          by the LR(0) item.  Thus we only need to atomize LR(0) items. 
          This table is for the currently being-built state, and emptied 
          when a new state is begun. *) 
       END (* REVEAL LRTable . GrammarTyp *) 

; END LALRTypes 
. 
