
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001 .. 2007, from Cocktail, lalr, Reduce.mi, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Check if the grammar is reduced *) 

MODULE Reduce 

; IMPORT Assertions 
; IMPORT IntProdVarArray 
; IMPORT IntItemVarArray 
; IMPORT IntSets 
; IMPORT IntStateVarArray 
; IMPORT LALRTypes 
; IMPORT LbeStd 
; IMPORT LRTable 
; FROM LRTable IMPORT NontermKindTyp
; IMPORT MessageCodes 
; FROM Messages IMPORT SemErrorText 
; IMPORT PortTypes 

; TYPE AFT = MessageCodes . T 

; PROCEDURE ComputeDerivable ( Gram : LRTable . GrammarTyp ) : IntSets . T  
  (* Pruefe ob alle Vokabularzeichen erreichbar sind *) 

  = VAR LNonterm : LbeStd . TokTyp 
  ; VAR LToDoSet : IntSets . T (* noch zu bearbeiten *) 
  ; VAR LDoneSet : IntSets . T (* bereits bearbeitet *) 
  ; VAR LRhsTok : LbeStd . TokTyp 
  ; VAR LDerivableSet : IntSets . T (* Funktionsergebniss *)

  ; BEGIN (* ComputeDerivable *) 

    (* Startsymbol ist zu bearbeiten. *) 
      LToDoSet := IntSets . Singleton ( Gram . StartSymbol ) 
    (* Startsymbol ist auch erreichbar. *) 
    ; LDerivableSet := LToDoSet 

    (* Terminale sind nicht mehr zu bearbeiten *) 
    ; LDoneSet := IntSets . Range ( Gram . FirstTerminal , Gram . LastTerminal )
    ; REPEAT (* waehle ein Nichtterminal zur Bearbeitung aus *) 
        LNonterm := IntSets . ExtractArbitraryMember ( (* VAR *) LToDoSet ) 
      ; LDoneSet := IntSets . Include ( LDoneSet , LNonterm ) 
      ; WITH 
          WNontermInfo 
          = Gram . NontermInfoArrayRef ^ 
              [ LNonterm - Gram . FirstAstNonterminal ] 
        DO FOR RProdSs := 0 TO WNontermInfo . Used - 1 
           DO WITH 
                WProduction 
                = Gram . ProdArrayRef ^ [ WNontermInfo . ProdListRef 
                  ^ [ RProdSs ] ]  
              DO (* alle Vocabularzeichen auf der rechten Seite werden 
                    hiermit erreichbar 
                 *) 
                 FOR LI := 0 TO WProduction . Len - 1 
                 DO LRhsTok := WProduction . Right ^ [ LI ] 
                 ; LDerivableSet 
                     := IntSets . Include ( LDerivableSet , LRhsTok ) 
                 (* Noch nicht erledigte Vokabularzeichen die rechts  
                    auftreten sind zu bearbeiten.  
                 *) 
                 ; IF NOT IntSets . IsElement ( LRhsTok , LDoneSet ) 
                   THEN 
                     LToDoSet := IntSets . Include ( LToDoSet , LRhsTok ) 
                   END (* IF *) 
                 END (* FOR *) 
              END (* WITH *) 
           END (* FOR *) 
        END (* WITH *) 
      UNTIL IntSets . IsEmpty ( LToDoSet ) 
    ; RETURN LDerivableSet 
    END ComputeDerivable 

; PROCEDURE ComputeBuildable  
    ( Gram : LRTable . GrammarTyp ; DerivableSet : IntSets . T ) 
  : IntSets . T 
  (* Pruefe ob alle Ast Nichtterminale baubar sind. *)

  = VAR LBuildableSet : IntSets . T (* Funktionsergebniss *) 

  ; BEGIN 
    (* Compute set of buildable Ast nontermals, from production list. *) 
      LBuildableSet := IntSets . Empty ( ) 
    ; FOR RProdSs := 0 TO Gram . NextProdSs - 1 
      DO WITH WProd = Gram . ProdArrayRef ^ [ RProdSs ] 
         DO IF Gram . FirstAstNonterminal <= WProd . BuildTok  
               AND WProd . BuildTok <= Gram . LastAstNonterminal 
               AND IntSets . IsElement ( WProd . Left , DerivableSet ) 
           THEN 
             LBuildableSet 
               := IntSets . Include ( LBuildableSet , WProd . BuildTok ) 
           END (* IF *) 
        END (* WITH *) 
      END (* FOR *)
    ; RETURN LBuildableSet  
    END ComputeBuildable  

; PROCEDURE ReportDerivableOrBuildable 
    ( Gram : LRTable . GrammarTyp 
    ; DerivableSet : IntSets . T 
    ; BuildableSet : IntSets . T 
    ) 
  : BOOLEAN 
  (* Pruefe ob alle Vokabularzeichen erreichbar oder baubar sind *) 

  = VAR LResult : BOOLEAN (* Funktionsergebniss *) 

  ; BEGIN (* ReportDerivableOrBuildable *) 
      LResult := TRUE 

    (* Terminale *) 
    ; FOR RTok := Gram . FirstTerminal TO Gram . LastTerminal 
      DO IF RTok >= LbeStd . Tok__FirstLangDep 
            AND NOT IntSets . IsElement ( RTok , DerivableSet ) 
         THEN 
           SemErrorText 
             ( " " & Gram . tokImage ( RTok ) 
             , AFT . W_Terminal_is_not_derivable 
             ) 
         END (* IF *) 
      END (* FOR *) 

    (* Ast Nichterminale *) 
    ; FOR RTok := Gram . FirstAstNonterminal TO Gram . LastAstNonterminal 
      DO IF Gram . NontermInfoArrayRef ^ 
              [ RTok - Gram . FirstAstNonterminal ] . Kind 
            IN LRTable . NontermKindSetShouldBeBuildable 
(* REVIEW:  ^Isn't this identically true because of loop bounds? *) 
            AND NOT IntSets . IsElement ( RTok , BuildableSet ) 
        THEN 
          SemErrorText 
            ( " " & Gram . tokImage ( RTok ) 
            , AFT . W_abstract_nonterminal_is_not_buildable 
            ) 
        END (* IF *) 
      END (* FOR *) 

    (* Andere Nichterminale *) 
    ; FOR RTok := Gram . FirstNonterminal TO Gram . LastNonterminal 
      DO IF Gram . NontermInfoArrayRef ^ 
              [ RTok - Gram . FirstAstNonterminal ] . Kind 
            IN LRTable . NontermKindSetShouldBeDerivable 
            AND NOT IntSets . IsElement ( RTok , DerivableSet ) 
        THEN 
          SemErrorText 
            ( " " & Gram . tokImage ( RTok ) 
            , AFT . W_Concrete_nonterminal_is_not_derivable 
            ) 
        END (* IF *) 
      END (* FOR *) 
    ; RETURN LResult 
    END ReportDerivableOrBuildable 

; PROCEDURE TestTerminating 
    ( Gram : LRTable . GrammarTyp ; DerivableSet : IntSets . T ) : BOOLEAN 
  (* Pruefe ob alle Nichtterminale terminalisierbar sind *) 

  = VAR TTToDoSet : IntSets . T (* noch zu ueberpruefende Nichterminale *) 
  ; VAR TTDoneSet 
      : IntSets . T (* als terminalisierbar erkannte Vokabularzeichen *) 
  ; VAR TTChangesOccurred : BOOLEAN (* hatte der letzte Schritt erfolg *) 
  ; VAR TTTerminates : BOOLEAN 

  ; PROCEDURE IsYetTerm ( NonTerm : LbeStd . TokTyp ) 

    = VAR LI : PortTypes . Int32Typ 
    ; VAR LProdSs : LRTable . ProdNoTyp 
    ; VAR LRhsTok : LbeStd . TokTyp 
    ; VAR LSuccess : BOOLEAN 

    ; BEGIN (* IsYetTerm *) 
        WITH 
          WNontermInfo 
          = Gram . NontermInfoArrayRef ^ 
              [ NonTerm - Gram . FirstAstNonterminal ] 
        DO (* Betrachte alle Produktionen mit linker Seite Nonterm *) 
          LProdSs := 0 
        ; LSuccess := FALSE 
        ; WHILE LProdSs < WNontermInfo . Used AND NOT LSuccess 
          DO WITH 
               WProduction 
               = Gram . ProdArrayRef ^ [ WNontermInfo . ProdListRef 
                 ^ [ LProdSs ] ] 
             DO (* Pruefe ob rechte Seite in todo* liegt *) 
                LSuccess := TRUE 
             ; LI := 0 
             ; WHILE ( LI < WProduction . Len ) AND LSuccess 
               DO LRhsTok := WProduction . Right [ LI ] 
               ; LSuccess 
                   := IntSets . IsElement ( LRhsTok , TTDoneSet ) 
               ; INC ( LI ) 
               END (* WHILE *) 
             END (* WITH *) 
          ; INC ( LProdSs ) 
          END (* WHILE *) 
        (* Here, LSuccess means there exists a production whose RHS symbols
           all terminate.
        *) 
        END (* WITH *) 
      ; IF LSuccess 
        THEN 
          TTDoneSet := IntSets . Include ( TTDoneSet , NonTerm )
        ; TTToDoSet := IntSets . Exclude ( TTToDoSet , NonTerm )
        ; TTChangesOccurred := TRUE 
        END (* IF *) 
      END IsYetTerm 

  ; PROCEDURE Visit ( Nonterm : IntSets . ValidElemT ) 

    = BEGIN 
      (* Ein ereichbares Nichtterminal, das nicht terminalisiserbar 
         ist, fuehrt zum Abbruch *) 
        WITH 
          WNtInfo 
            = Gram . NontermInfoArrayRef 
                ^ [ Nonterm - Gram . FirstAstNonterminal ] 
        DO
          IF WNtInfo . Kind IN LRTable . NontermKindSetShouldTerminate  
          THEN 
            IF IntSets . IsElement ( Nonterm , DerivableSet ) 
            THEN 
              IF WNtInfo . Used = 0 
              THEN 
                SemErrorText 
                  ( " " & Gram . tokImage ( Nonterm ) 
                  , AFT . E_Concrete_nonterminal_has_no_production 
                  ) 
              ELSE 
                SemErrorText 
                  ( " " & Gram . tokImage ( Nonterm ) 
                  , AFT . E_Concrete_nonterminal_does_not_terminate 
                  ) 
              END (* IF *) 
            ; TTTerminates := FALSE 
            ELSE (* It's not derivable. *)  
              IF Gram . FirstNonterminal <= Nonterm 
                 AND Nonterm <= Gram . LastNonterminal  
              THEN (* It's a pure concrete nonterminal.  Even though it's 
                      unreachable, this may be useful information. *) 
                IF WNtInfo . Used = 0 
                THEN 
                  SemErrorText 
                    ( " " & Gram . tokImage ( Nonterm ) 
                    , AFT . I_Unreachable_concrete_nonterminal_has_no_production 
                    ) 
                ELSE 
                  SemErrorText 
                    ( " " & Gram . tokImage ( Nonterm ) 
                    , AFT . I_Unreachable_concrete_nonterminal_does_not_terminate 
                    ) 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* WITH *) 
      END Visit 

  ; BEGIN (* TestTerminating *) 
    (* TTToDoSet = Menge aller Nichtterminale *) 
      TTToDoSet := IntSets . Empty ( )  
    ; FOR RNonterm := Gram . FirstAstNonterminal TO Gram . LastNonterminal 
      DO IF Gram . NontermInfoArrayRef 
            ^ [ RNonterm - Gram . FirstAstNonterminal ] . Kind 
            # NontermKindTyp . NtkUnused 
         THEN 
           TTToDoSet 
             := IntSets . Include ( TTToDoSet , RNonterm ) 
         END (* IF *)  
      END (* FOR *) 
(* TODO: ^It would probably be more efficient to allocate and fill an array
         of nonterminals, then use IntSets . FromArray, given OrdSets' 
         functional style.
*) 

    (* TTDoneSet := Menge alle Terminale *) 
    ; TTDoneSet 
        := IntSets . Range ( Gram . FirstTerminal , Gram . LastTerminal ) 

    ; REPEAT 
        TTChangesOccurred := FALSE 

      (* Pruefe ob jetzt ein weiteres *) 
      (* Nichtterminal terminalisierbar ist *) 
      ; FOR RNonterm := Gram . FirstAstNonterminal 
            TO Gram . LastNonterminal 
        DO IF IntSets . IsElement ( RNonterm , TTToDoSet ) 
           THEN IsYetTerm ( RNonterm ) 
           END (* IF *) 
        END (* FOR *) 
      UNTIL NOT TTChangesOccurred   (* solange bis sich nichts aendert *) 
    ; TTTerminates := TRUE 
    ; <* FATAL ANY *> BEGIN
        IntSets . ForAllDo ( TTToDoSet , Visit ) 
      END 
    ; RETURN TTTerminates 
    END TestTerminating 

(* VISIBLE *) 
; PROCEDURE IsReduced ( Gram : LRTable . GrammarTyp ) : BOOLEAN 
  (* prueft ob die im Modul Automaton bekannte Grammatik 
     reduziert ist. Falls nein wird das Programm mittels einer 
     Fehlermeldung abgebrochen 
  *)
  (* Only errors cause FALSE to be returned, not information or warnings. *)  

  = VAR LOK : BOOLEAN 
  ; VAR LOKDerivable : BOOLEAN 
  ; VAR LOKTerminating : BOOLEAN 
  ; VAR LBuildableSet : IntSets . T (* baubare Symbole  *) 
  ; VAR LDerivableSet : IntSets . T (* erreichbare Symbole *) 

  ; BEGIN (* IsReduced *) 
      LDerivableSet := ComputeDerivable ( Gram ) 
    ; LBuildableSet := ComputeBuildable ( Gram , LDerivableSet ) 
    ; LOKDerivable 
        := ReportDerivableOrBuildable( Gram , LDerivableSet , LBuildableSet ) 
    ; LOKTerminating := TestTerminating ( Gram , LDerivableSet ) 
    ; LOK := LOKDerivable AND LOKTerminating 
      (* kein genereller Abbruch *) 
    ; RETURN LOK 
    END IsReduced 

; BEGIN (* Reduce *) 
  END Reduce 
. 

