
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Mechanically converted to Modula-3 and extensively modified by 
   Rodney M. Bates, 2001, 2002, from Cocktail, lalr, Infos.mi, 
   which was originally written in Modula-2 and part of LALR: 

   Author: Bertram Vielsack, University of Karlsruhe

   Supervisor: Josef Grosch, grosch@cocolab.de,

   at GMD Forschungsstelle at the University of Karlsruhe  Note: GMD 
   (National German Research Centre for Computer Science) does not exist
   in this form any more. GMD has been merged with "Fraunhofergesellschaft".   
*) 

(* Print some informations about the generation *) 

MODULE Infos 

; IMPORT Fmt 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr 

; FROM Failures IMPORT Backout 
; IMPORT Gen 
; IMPORT IntSets 
; IMPORT LALRTypes 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT Messages 

; CONST RightMargin = 80 

; CONST ProdNoImage = LRTable . ProdNoImage (* A PROCEDURE. *) 

; PROCEDURE WriteOnePrecAndAssoc  
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ; Tok : LbeStd . TokTyp 
    ; OperInfo : LALRTypes . OperTyp
    ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* WriteOnePrecAndAssoc *)  
      Wr . PutText ( WrT , Fmt . Pad ( LbeStd . NumIdTokImage ( Tok ) , 9 ) )
    ; Wr . PutText ( WrT , " " ) 
    ; Wr . PutText 
        ( WrT 
        , Fmt . Pad ( LRTable . PrecedenceImage ( OperInfo . Precedence ) , 5 )
        ) 
    ; Wr . PutText ( WrT , " " ) 
    ; Wr . PutText 
        ( WrT 
        , Fmt . Pad 
            ( LRTable . AssocImage ( OperInfo . Ass ) 
            , 8 
            , align := Fmt . Align . Left 
            )
        ) 
    ; Wr . PutText ( WrT , " " ) 
    ; Wr . PutText ( WrT , Gram . tokImage ( Tok ) ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    END WriteOnePrecAndAssoc 

(* VISIBLE: *) 
; PROCEDURE WritePrecAndAssocList  
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

  = VAR LWrT : Wr . T 

  ; <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* WritePrecAndAssocList *) 
      IF WrT = NIL 
      THEN 
        LWrT := Messages . MessageWr ( ) 
      ELSE 
        LWrT := WrT 
      END (* IF *) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "(* **** Precedence and associativity ***** *) " ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; IF Gram = NIL 
      THEN 
        Wr . PutText ( LWrT , "(* Grammar is NIL. *) " ) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ELSIF Gram . OperArrayRef = NIL 
      THEN 
        Wr . PutText ( LWrT , "(* OperArrayRef is NIL. *) " ) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ELSE 
        Wr . PutText ( LWrT , "(* NumTok   Prec Assoc    TokName *) " ) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ; FOR RSs := 0 TO NUMBER ( Gram . OperArrayRef ^ ) - 1 
        DO WriteOnePrecAndAssoc 
             ( Gram 
             , WrT 
             , RSs + Gram . FirstTerminal 
             , Gram . OperArrayRef ^ [ RSs ] 
             ) 
        END (* FOR *) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      END (* IF *) 
    END WritePrecAndAssocList 

(* VISIBLE: *) 
; PROCEDURE ReprKindImage ( Rep : LALRTypes . ReprKindTyp ) : TEXT 

  = BEGIN (* ReprKindImage *) 
      CASE Rep 
      OF LALRTypes . ReprKindTyp . NoRep 
      => RETURN "NoRep" 
      | LALRTypes . ReprKindTyp . TermRep 
      => RETURN "TermRep" 
      | LALRTypes . ReprKindTyp . AstNontermRep 
      => RETURN "AstNontermRep" 
      | LALRTypes . ReprKindTyp . NontermRep 
      => RETURN "NontermRep" 
      | LALRTypes . ReprKindTyp . RedRep 
      => RETURN "RedRep" 
      | LALRTypes . ReprKindTyp . TermRepIgn 
      => RETURN "TermRepIgn" 
      | LALRTypes . ReprKindTyp . AstNontermRepIgn 
      => RETURN "AstNontermRepIgn" 
      | LALRTypes . ReprKindTyp . NontermRepIgn 
      => RETURN "NontermRepIgn" 
      | LALRTypes . ReprKindTyp . RedRepIgn 
      => RETURN "RedRepIgn" 
      END (* CASE *) 
    END ReprKindImage 

(* VISIBLE: *) 
; PROCEDURE StateKindImage ( Kind : LALRTypes . StateKindTyp ) : TEXT 

  = BEGIN (* StateKindImage *) 
      CASE Kind 
      OF LALRTypes . StateKindTyp . None 
      => RETURN "None" 
      | LALRTypes . StateKindTyp . Lookup 
      => RETURN "Lookup" 
      | LALRTypes . StateKindTyp . Reduce 
      => RETURN "Reduce" 
      | LALRTypes . StateKindTyp . Accept 
      => RETURN "Accept" 
      END (* CASE *) 
    END StateKindImage 

(* VISIBLE: *) 
; PROCEDURE AssocTypImage ( Kind : LRTable . AssocTyp ) : TEXT 

  = BEGIN (* AssocTypImage *) 
      CASE Kind 
      OF LRTable . AssocTyp . left 
      => RETURN "Left" 
      | LRTable . AssocTyp . right 
      => RETURN "Right" 
      | LRTable . AssocTyp . none 
      => RETURN "None" 
      | LRTable . AssocTyp . nonassoc 
      => RETURN "Nonassoc" 
      END (* CASE *) 
    END AssocTypImage 

(* VISIBLE: *) 
; PROCEDURE WriteSsList 
    ( <* UNUSED *> Gram : LRTable . GrammarTyp 
    ; Array : LALRTypes . SsArrayRefTyp  
    ; Used : LALRTypes .  ListCountTyp
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 
  (* Uses whole lines. *) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LWrT : Wr . T 
  ; VAR LText : TEXT 
  ; VAR LImage : TEXT 
  ; VAR LHasMultipleLines : BOOLEAN := FALSE 

  ; BEGIN (* WriteSsList *) 
      IF WrT = NIL 
      THEN 
        LWrT := Messages . MessageWr ( ) 
      ELSE 
        LWrT := WrT 
      END (* IF *) 
    ; IF Used <= 0 
      THEN 
        Wr . PutText ( LWrT , " < > " ) 
      ELSE 
        LText := " < " & Fmt . Int ( Array ^ [ 0 ] ) 
      ; FOR FI := 1 TO Used - 1 
        DO LImage := Fmt . Int ( Array ^ [ FI ] ) 
        ; IF Text . Length ( LText ) + 3 + Text . Length ( LImage ) 
             > RightMargin 
          THEN 
            Wr . PutText ( LWrT , LText ) 
          ; Wr . PutText ( LWrT , Wr . EOL ) 
          ; LText := "  " 
          ; LHasMultipleLines := TRUE 
          END (* IF *) 
        ; LText := LText & " , " & LImage 
        END (* FOR *) 
      ; Wr . PutText ( LWrT , LText ) 
      ; IF LHasMultipleLines 
        THEN 
          Wr . PutText ( LWrT , Wr . EOL ) 
        END (* IF *) 
      ; Wr . PutText ( LWrT , " > " ) 
      END (* IF *) 
    END WriteSsList 

(* VISIBLE: *) 
; PROCEDURE WriteTokSet 
    ( Gram : LRTable . GrammarTyp 
    ; Set : IntSets . T 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ; FirstLinePrefix : TEXT := "" 
    ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR Wts_WrT : Wr . T 
  ; VAR Wts_Prefix : TEXT 
  ; VAR Wts_PrefixLength : LbeStd . CharNoTyp 
  ; VAR Wts_LineLength : LbeStd . CharNoTyp 
  ; VAR Wts_IsMultiLine : BOOLEAN := FALSE 
  ; VAR Wts_IsFirstTok : BOOLEAN := TRUE 

  ; PROCEDURE VisitTok ( Tok : IntSets . ValidElemT ) 

    = <* FATAL Thread . Alerted *> 
      <* FATAL Wr . Failure *> 
      VAR LTokImage : TEXT 
    ; VAR LTokLength : LbeStd . CharNoTyp 

    ; BEGIN (* VisitTok *) 
        LTokImage := Gram . tokImage ( Tok ) 
      ; LTokLength := Text . Length ( LTokImage ) 
      ; IF Wts_LineLength + LTokLength + 3 > RightMargin 
        THEN 
          Wr . PutText ( Wts_WrT , Wr . EOL ) 
        ; Wr . PutText ( Wts_WrT , Wts_Prefix ) 
        ; Wts_LineLength := Wts_PrefixLength 
        ; Wts_IsMultiLine := TRUE 
        END (* IF *) 
      ; IF NOT Wts_IsFirstTok 
        THEN 
          Wr . PutText ( Wts_WrT , " , " ) 
        END (* IF *) 
      ; Wr . PutText ( Wts_WrT , LTokImage ) 
      ; Wts_LineLength := Wts_LineLength + 3 + LTokLength 
      ; Wts_IsFirstTok := FALSE 
      END VisitTok 

  ; BEGIN (* WriteTokSet *) 
      IF WrT = NIL 
      THEN 
        Wts_WrT := Messages . MessageWr ( ) 
      ELSE 
        Wts_WrT := WrT 
      END (* IF *) 
    ; IF IntSets . IsEmpty ( Set ) 
      THEN 
        Wr . PutText ( Wts_WrT , FirstLinePrefix & " { } " & Wr . EOL ) 
      ELSE 
        Wr . PutText ( Wts_WrT , FirstLinePrefix & " { " ) 
      ; Wts_PrefixLength := Text . Length ( FirstLinePrefix ) 
      ; Wts_Prefix := Fmt . Pad ( "" , Wts_PrefixLength ) 
      ; Wts_LineLength := Wts_PrefixLength + 3 
      ; <* FATAL ANY *> (* Can't happen, VisitTok *) 
        BEGIN IntSets . ForAllDo ( Set , VisitTok ) 
        END (* Block *) 
      ; IF Wts_IsMultiLine 
        THEN 
          Wr . PutText ( Wts_WrT , Wr . EOL ) 
        ; Wr . PutText ( Wts_WrT , Wts_Prefix ) 
        END (* IF *) 
      ; Wr . PutText ( Wts_WrT , " } " & Wr . EOL ) 
      END (* IF *) 
    END WriteTokSet 

(* VISIBLE: *) 
; PROCEDURE WriteProd 
    ( Gram : LRTable . GrammarTyp 
    ; ProdSs : LRTable . ProdNoTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LWrT : Wr . T 

  ; BEGIN (* WriteProd *) 
      IF WrT = NIL 
      THEN 
        LWrT := Messages . MessageWr ( ) 
      ELSE 
        LWrT := WrT 
      END (* IF *) 
    ; WITH WProd = Gram . ProdArrayRef ^ [ ProdSs ] 
      DO Wr . PutText ( LWrT , "Production No " ) 
      ; Wr . PutText ( LWrT , ProdNoImage ( ProdSs ) ) 
      ; Wr . PutText ( LWrT , ", ShortestDerivable = " ) 
      ; Wr . PutText ( LWrT , Fmt . Int ( WProd . ShortestDerivable ) ) 
      ; Wr . PutText ( LWrT , ", Len = " ) 
      ; Wr . PutText ( LWrT , Fmt . Int ( WProd . Len ) ) 
      ; Wr . PutText ( LWrT , ", Precedence = " ) 
      ; Wr . PutText ( LWrT , Fmt . Int ( WProd . Precedence ) ) 
      ; Wr . PutText ( LWrT , ", Assoc = " ) 
      ; Wr . PutText ( LWrT , AssocTypImage ( WProd . Ass ) ) 
      ; Wr . PutText ( LWrT , ", Reduce = " ) 
      ; WriteSsList 
          ( Gram 
          , WProd . ReduceStates . Array 
          , WProd . ReduceStates . Used 
          , LWrT 
          ) 
      ; Wr . PutText ( LWrT , ", Options = " ) 
      ; Wr . PutText 
          ( LWrT , LRTable . OptionIdSetImage ( WProd . OptionIdSet ) ) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ; Wr . PutText ( LWrT , "  " ) 
      ; Wr . PutText ( LWrT , Gram . tokImage ( WProd . Left ) ) 
      ; IF WProd . BuildTok # 0 
        THEN
          Wr . PutText ( LWrT , " BUILD " ) 
        ; Wr . PutText ( LWrT , Gram . tokImage ( WProd . BuildTok ) ) 
        END 
      ; Wr . PutText ( LWrT , " ::= " ) 
      ; IF WProd . Len = 0 
        THEN 
          Wr . PutText ( LWrT , " <empty> " ) 
        ELSE 
          FOR i := 0 TO WProd . Len - 1 
          DO Wr . PutText ( LWrT , Gram . tokImage ( WProd . Right [ i ] ) ) 
          ; Wr . PutText ( LWrT , " " ) 
          END (* FOR *) 
        END (* IF *) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      END (* WITH *) 
    END WriteProd 

(* VISIBLE: *) 
; PROCEDURE WriteProductions 
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

  = VAR LWrT : Wr . T 

  ; <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* WriteProductions *) 
      IF WrT = NIL 
      THEN 
        LWrT := Messages . MessageWr ( ) 
      ELSE 
        LWrT := WrT 
      END (* IF *) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "(* ************ Productions: ************* *) " & Wr . EOL ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; IF Gram # NIL 
      THEN 
        FOR FProdSs := 0 TO Gram . NextProdSs - 1 
        DO WriteProd ( Gram , FProdSs , WrT ) 
        END (* FOR *) 
      END (* IF *) 
    END WriteProductions 

; VAR GWriteExtraLookaheadInfo : BOOLEAN := TRUE 

(* VISIBLE: *) 
; PROCEDURE WriteItem 
    ( Gram : LRTable . GrammarTyp 
    ; ItemSs : LALRTypes . ItemSsTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LWrT : Wr . T 
  ; VAR LLineText : TEXT 

  ; BEGIN (* WriteItem *) 
      IF WrT = NIL 
      THEN 
        LWrT := Messages . MessageWr ( ) 
      ELSE 
        LWrT := WrT 
      END (* IF *) 
    ; WITH 
        WItem = Gram . ItemArrayRef ^ [ ItemSs ] 
      , WProd = Gram . ProdArrayRef ^ [ WItem . ProdSs ] 
      DO 
        Wr . PutText ( LWrT , "  Item No " ) 
      ; Wr . PutText ( LWrT , ItemSsImage ( ItemSs ) ) 
      ; Wr . PutText ( LWrT , ", GoToStateSs = " ) 
      ; Wr . PutText ( LWrT , StateSsImage ( WItem . GoToStateSs ) ) 
      ; Wr . PutText ( LWrT , ", PrevItemSs = " ) 
      ; Wr . PutText ( LWrT , ItemSsImage ( WItem . PrevItemSs ) ) 
      ; Wr . PutText ( LWrT , ", ProdSs = " ) 
      ; Wr . PutText ( LWrT , ProdNoImage ( WItem . ProdSs ) ) 
      ; Wr . PutText ( LWrT , ", RepItemSs = " ) 
      ; Wr . PutText ( LWrT , ItemSsImage ( WItem . RepItemSs ) ) 
      ; Wr . PutText ( LWrT , ", Rep = " ) 
      ; Wr . PutText ( LWrT , ReprKindImage ( WItem . Rep ) ) 
      ; Wr . PutText ( LWrT , ", LR0ItemHashCode = " ) 
      ; Wr . PutText ( LWrT , Fmt . Int ( WItem . LR0ItemHashCode ) ) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 

      ; LLineText := "    " & Gram . tokImage ( WProd . Left ) & " ::= " 
      ; IF WProd . Len = 0 
        THEN 
          LLineText := LLineText & LRDotString
        ELSE 
          IF WItem . DotPosition = 0 
          THEN 
            LLineText := LLineText & LRDotString 
          END (* IF *) 
        ; FOR i := 0 TO WProd . Len - 1 
          DO 
            LLineText := LLineText & Gram . tokImage ( WProd . Right [ i ] ) 
          ; IF WItem . DotPosition = i + 1 
            THEN 
              LLineText := LLineText & LRDotString 
            ELSE 
              LLineText := LLineText & " " 
            END (* IF *) 
          END (* FOR *) 
        END (* IF *) 

      (* Write token sets: *) 
      ; IF IntSets . IsEmpty ( WItem . LR1Set )  
        THEN 
          LLineText := LLineText & "{} " 
        ELSE 
          IF Text . Length ( LLineText ) 
             + 15 * IntSets . Card ( WItem . LR1Set ) 
             > RightMargin 
          THEN 
            Wr . PutText ( LWrT , LLineText ) 
          ; Wr . PutText ( LWrT , Wr . EOL ) 
          ; LLineText := "    LRSet:  " 
          END (* IF *) 
        ; WriteTokSet ( Gram , WItem . LR1Set , LWrT , LLineText ) 
          (* ^Which writes EOL. *) 
        ; LLineText := ""  
        END (* IF *) 

      ; IF (* WItem . Rep IN LALRTypes . ReprKindSetRedOrNonterm 
           OR *) NOT IntSets . IsEmpty ( WItem . Set )  
        THEN (* Set is relevant. *) 
          IF Text . Length ( LLineText ) + 15 * IntSets . Card ( WItem . Set ) 
             > RightMargin 
          THEN 
            Wr . PutText ( LWrT , LLineText ) 
          ; Wr . PutText ( LWrT , Wr . EOL ) 
          ; LLineText := "    LALRSet:" 
          ELSE LLineText := LLineText & "LALRSet: " 
          END (* IF *) 
        ; WriteTokSet ( Gram , WItem . Set , LWrT , LLineText ) 
          (* ^Which writes EOL. *) 
        ; LLineText := ""  

        ; IF GWriteExtraLookaheadInfo 
             AND NOT IntSets . Equal ( WItem . Set , WItem . OrigSet ) 
          THEN
            WriteTokSet 
              ( Gram , WItem . OrigSet , LWrT 
              , "    Unrepaired:"
              ) 
            (* ^Which writes EOL. *) 
          END (* IF *) 

        ; IF GWriteExtraLookaheadInfo 
             AND NOT IntSets . Equal ( WItem . ReadSet , WItem . Set ) 
          THEN
            WriteTokSet 
              ( Gram , WItem . ReadSet , LWrT 
              , "    Read:   "
              ) 
            (* ^Which writes EOL. *) 
          END (* IF *) 
        END (* IF *) 

      ; IF GWriteExtraLookaheadInfo AND WItem . Relation . Used > 0 
        THEN 
          Wr . PutText ( LWrT , "    Relation = " ) 
        ; WriteSsList 
            ( Gram , WItem . Relation . Array , WItem . Relation . Used , LWrT )
        ; Wr . PutText ( LWrT , Wr . EOL ) 
        END (* IF *) 

      ; IF GWriteExtraLookaheadInfo AND WItem . ReadsRelation . Used > 0 
        THEN 
          Wr . PutText ( LWrT , "    ReadsRel = " ) 
        ; WriteSsList 
            ( Gram 
            , WItem . ReadsRelation . Array 
            , WItem . ReadsRelation . Used 
            , LWrT 
            ) 
        ; Wr . PutText ( LWrT , Wr . EOL ) 
        END (* IF *) 

      ; IF LLineText # NIL 
        THEN 
          Wr . PutText ( LWrT , LLineText ) 
        ; Wr . PutText ( LWrT , Wr . EOL ) 
        END (* IF *) 
      END (* WITH *) 
    END WriteItem 

(* VISIBLE: *) 
; PROCEDURE WriteItemSet 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : LALRTypes . StateSsTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LWrT : Wr . T 

  ; BEGIN (* WriteItemSet *) 
      IF WrT = NIL 
      THEN 
        LWrT := Messages . MessageWr ( ) 
      ELSE 
        LWrT := WrT 
      END (* IF *) 
    ; WITH WState = Gram . StateArrayRef ^ [ StateSs ] 
      DO 
        Wr . PutText ( LWrT , "{ " & Wr . EOL ) 
      ; FOR FItemSs := WState . FirstItemSs 
            TO WState . FirstItemSs + WState . ItemCt - 1 
        DO WriteItem ( Gram , FItemSs , WrT ) 
        END (* FOR *) 
      ; Wr . PutText ( LWrT , "} " & Wr . EOL ) 
      END (* WITH *) 
    END WriteItemSet 

; PROCEDURE ItemSsImage ( Ss : LALRTypes . ItemSsTyp ) : TEXT 

  = BEGIN 
      IF Ss = LALRTypes . ItemSsNull 
      THEN RETURN "Null" 
      ELSE RETURN Fmt . Int ( Ss ) 
      END (* IF *) 
    END ItemSsImage 

; PROCEDURE StateSsImage ( Ss : LALRTypes . StateSsTyp ) : TEXT 

  = BEGIN 
      IF Ss = LALRTypes . StateSsNull 
      THEN RETURN "Null" 
      ELSE RETURN Fmt . Int ( Ss ) 
      END (* IF *) 
    END StateSsImage 

; PROCEDURE IntSetsValidElemImage ( Elem : IntSets . ValidElemT ) : TEXT 
  (* Need to wrap Fmt . Int for correct signature. *) 

  = BEGIN 
      RETURN Fmt . Int ( Elem ) 
    END IntSetsValidElemImage

; PROCEDURE WriteRepSet ( WrT : Wr . T ; Set : IntSets . T ; Label : TEXT ) 

  = VAR LPrefix : TEXT 

  ; BEGIN 
      IF NOT IntSets . IsEmpty ( Set ) 
      THEN 
        Wr . PutText ( WrT , Label ) 
      ; Wr . PutText ( WrT , " = " )  
      ; LPrefix := Fmt . Pad ( "" , Text . Length ( Label ) + 3 )  
      ; Wr . PutText 
          ( WrT 
          , IntSets . Image ( Set , IntSetsValidElemImage , LPrefix ) 
          ) 
      ; Wr . PutText ( WrT , Wr . EOL  ) 
      END (* IF *) 
    END WriteRepSet 

(* VISIBLE: *) 
; PROCEDURE WriteState 
    ( Gram : LRTable . GrammarTyp 
    ; StateSs : LALRTypes . StateSsTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LWrT : Wr . T 

  ; BEGIN (* WriteState *) 
      IF WrT = NIL 
      THEN 
        LWrT := Messages . MessageWr ( ) 
      ELSE 
        LWrT := WrT 
      END (* IF *) 
    ; WITH WState = Gram . StateArrayRef ^ [ StateSs ] 
      DO 
        Wr . PutText 
          ( LWrT , "--------------------------------------------------------" )
      ; Wr . PutText 
          ( LWrT , "--------------------------------------------------------" )
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ; Wr . PutText ( LWrT , "State No " ) 
      ; Wr . PutText ( LWrT , StateSsImage ( StateSs ) ) 
      ; Wr . PutText ( LWrT , ", Kind = " ) 
      ; Wr . PutText ( LWrT , StateKindImage ( WState . Kind ) ) 
      ; Wr . PutText ( LWrT , ", FirstItemSs = " ) 
      ; Wr . PutText ( LWrT , ItemSsImage ( WState . FirstItemSs ) ) 
      ; Wr . PutText ( LWrT , ", ItemCt = " ) 
      ; Wr . PutText ( LWrT , ItemSsImage ( WState . ItemCt ) ) 
      ; Wr . PutText ( LWrT , ", NonclosureItemCt = " ) 
      ; Wr . PutText ( LWrT , ItemSsImage ( WState . NonclosureItemCt ) ) 
      ; Wr . PutText ( LWrT , ", ReduceItemSs = " ) 
      ; Wr . PutText ( LWrT , ItemSsImage ( WState . ReduceItemSs ) ) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ; Wr . PutText ( LWrT , "  , LR1SetRepStateSs = " ) 
      ; Wr . PutText ( LWrT , StateSsImage ( WState . LR1SetRepStateSs ) ) 
      ; Wr . PutText ( LWrT , ", LR0ListRepStateSs = " ) 
      ; Wr . PutText ( LWrT , StateSsImage ( WState . LR0ListRepStateSs ) ) 
   (* ; Wr . PutText ( LWrT , Wr . EOL ) *) 
      ; Wr . PutText ( LWrT , ", Action = " ) 
      ; Wr . PutText ( LWrT , Fmt . Int ( WState . Action ) ) 
      ; Wr . PutText ( LWrT , ", LR1ListHashCode = " ) 
      ; Wr . PutText ( LWrT , Fmt . Int ( WState . LR1ListHashCode ) ) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ; Wr . PutText ( LWrT , "  , LR1SetHashCode = " ) 
      ; Wr . PutText ( LWrT , Fmt . Int ( WState . LR1SetHashCode ) ) 
      ; Wr . PutText ( LWrT , ", LR0ListHashCode = " ) 
      ; Wr . PutText ( LWrT , Fmt . Int ( WState . LR0ListHashCode ) ) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      ; IF WState . SetOrderList # NIL 
        THEN 
          Wr . PutText ( LWrT , "SetOrderList =" ) 
        ; Wr . PutText ( LWrT , Wr . EOL ) 
        ; WriteSsList 
            ( Gram 
            , WState . SetOrderList  
            , NUMBER ( WState . SetOrderList ^ ) 
            , LWrT 
            ) 
        ; Wr . PutText ( LWrT , Wr . EOL ) 
        END (* IF *) 
      ; WriteRepSet 
          ( LWrT , WState . LR1SetStateSet , "  , LR1SetStateSet" )
      ; WriteRepSet 
          ( LWrT , WState . LR0ListStateSet , "  , LR0ListStateSet" )
      ; WriteItemSet ( Gram , StateSs , LWrT ) 
      END (* WITH *) 
    END WriteState 

(* VISIBLE: *) 
; PROCEDURE WriteStates 
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 
    
  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LWrT : Wr . T 

  ; BEGIN (* WriteStates *) 
      IF WrT = NIL 
      THEN 
        LWrT := Messages . MessageWr ( ) 
      ELSE 
        LWrT := WrT 
      END (* IF *) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText 
        ( LWrT , "(* ************ States: ************* *) " & Wr . EOL ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; IF Gram # NIL 
      THEN 
        FOR FStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
        DO WriteState ( Gram , FStateSs , WrT ) 
        END (* FOR *) 
      END (* IF *) 
    END WriteStates 

(* VISIBLE: *) 
; PROCEDURE WriteInfo 
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    ) 
  (* Ausgabe von statistischen Informationen auf file 'f' *) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LWrT : Wr . T 

  ; BEGIN (* WriteInfo *) 
      IF WrT = NIL 
      THEN 
        LWrT := Messages . MessageWr ( ) 
      ELSE 
        LWrT := WrT 
      END (* IF *) 
    ; Wr . PutText ( LWrT , "Statistics: " ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "-------------------" ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "Terminals            : " ) 
    ; Wr . PutText 
        ( LWrT , Fmt . Int ( Gram . LastTerminal - Gram . FirstTerminal + 1 ) ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "Abstract Nonterminals: " ) 
    ; Wr . PutText 
        ( LWrT 
        , Fmt . Int 
            ( Gram . LastAstNonterminal - Gram . FirstAstNonterminal + 1 ) 
        ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "Concrete Nonterminals: " ) 
    ; Wr . PutText 
        ( LWrT 
        , Fmt . Int ( Gram . LastNonterminal - Gram . FirstNonterminal + 1 ) 
        ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "Productions          : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( Gram . NextProdSs ) ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "Items                : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( Gram . ItemCt ) ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "LR(1) list states    : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( Gram . LR1ListStateCt ) ) 
    ; Wr . PutText ( LWrT , ", with " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( Gram . LR1ReorderedStateCt ) ) 
    ; Wr . PutText ( LWrT , " of them having reordered items" ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "LR(1) set states     : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( Gram . LR1SetStateCt ) ) 
    ; Wr . PutText ( LWrT , ", with " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( Gram . LR1SetStateMergeCt ) ) 
    ; Wr . PutText ( LWrT , " of them merged" ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "LR(0) list states    : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( Gram . LR0ListStateCt ) ) 
    ; Wr . PutText ( LWrT , ", with " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( Gram . LR0ListStateMergeCt ) ) 
    ; Wr . PutText ( LWrT , " of them merged" ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "ReadStates           : " ) 
    ; Wr . PutText 
        ( LWrT 
        , Fmt . Int ( Gram . FirstReadRedAction - Gram . FirstReadAction ) 
        ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "Base                 : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( ( Gram . FirstReadRedAction ) * 2 ) ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "NBase                : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( ( Gram . FirstReadRedAction ) * 2 ) ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; Wr . PutText ( LWrT , "Default              : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( ( Gram . FirstReadRedAction ) * 2 ) ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 

(* FIXME: 
    ; Wr . PutText ( LWrT , "Terminals            : " ) 
    ; LBaseSize := NUMBER ( Gram . BaseRef ^ ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( ( LBaseSize + 1 ) * 4 ) ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( ( ( Gram . Filling * 100 ) + ( LBaseSize DIV 2 ) ) DIV LBaseSize , 4 ) 
    ; Wr . PutChar ( LWrT , '%' ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
    ; LNSize := NUMBER ( Gram . NBaseRef ^ ) - FirstAstTerminal 
    ; Wr . PutText ( LWrT , "Nonterminals         : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( LNSize * 2 ) ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( ( ( Gram . NFilling * 100 ) + ( LNSize DIV 2 ) ) DIV LNSize , 4 ) 
    ; Wr . PutChar ( LWrT , '%' ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
 
    ; Wr . PutText ( LWrT , "Length               : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( ( Gram . StateCt - Gram . FirstReduceAction ) * 2 ) ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
 
    ; Wr . PutText ( LWrT , "Left                 : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( ( Gram . StateCt - Gram . FirstReduceAction  ) * 2 ) ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
 
    ; Wr . PutText ( LWrT , "Continuation         : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( ( Gram . FirstReadRedAction ) * 2 ) ) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
 
    ; Wr . PutText ( LWrT , "Cpu-Time [ms]        : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( 0 , 6 ) 
(* FIX: put this value in. *) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
 
    ; Wr . PutText ( LWrT , "Memory used          : " ) 
    ; Wr . PutText ( LWrT , Fmt . Int ( , 6 ) 
(* FIX: put this value in. *) 
    ; Wr . PutText ( LWrT , Wr . EOL ) 
*)  
    END WriteInfo 

; CONST MaxLineLen = 80 

; PROCEDURE PrintTableLine 
    ( Gram : LRTable . GrammarTyp 
    ; InfoWr : Wr . T 
    ; StateNo : LRTable . StateNoTyp 
    ; Line : LRTable . StateNoArrayRefTyp 
    ; DefaultStateNo : LRTable . StateNoTyp := LRTable . StateNoNull 
    ; DoPrintDefaults : BOOLEAN := FALSE 
    ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR PtlLineLength : CARDINAL := 0  

  ; PROCEDURE PtlWriteText ( T : TEXT ) 

    = BEGIN 
        Wr . PutText ( InfoWr , T ) 
      ; INC ( PtlLineLength , Text . Length ( T ) ) 
      END PtlWriteText 

  ; PROCEDURE PtlWriteEOL ( ) 

    = BEGIN 
        Wr . PutText ( InfoWr , Wr . EOL ) 
      ; PtlLineLength := 0 
      END PtlWriteEOL 

  ; BEGIN (* PrintTableLine *) 
      VAR LNextState : LRTable . StateNoTyp 
    ; VAR LText : TEXT 
    ; BEGIN (* Block PrintTableLine *) 
        PtlWriteText ( "State " ) 
      ; PtlWriteText ( StateSsImage ( StateNo ) ) 
      ; Wr . PutChar ( InfoWr , ':' ) 
      ; IF DoPrintDefaults 
        THEN
          IF DefaultStateNo = LRTable . StateNoNull 
          THEN 
            PtlWriteText ( "(No default)" ) 
          ELSE
            PtlWriteText ( "(Default=" ) 
          ; PtlWriteText ( StateSsImage ( DefaultStateNo ) ) 
          ; PtlWriteText ( ")" ) 
          END (* IF *) 
        END (* IF *) 
      ; FOR RTok := Gram . FirstTerminal TO Gram . LastNonterminal 
        DO 
          LNextState := Line ^ [ RTok - Gram . FirstTerminal ] 
        ; IF LNextState # LRTable . StateNoNull 
          THEN 
            LText 
              := " (" & Gram . tokImage ( RTok ) & "," 
                 & StateSsImage ( LNextState ) & ")" 
          ; IF PtlLineLength + Text . Length ( LText ) > MaxLineLen
            THEN 
              PtlWriteEOL ( ) 
            ; PtlWriteText ( "  " ) 
            END (* IF *) 
          ; PtlWriteText ( LText ) 
          END (* IF *) 
        END (* FOR *) 
      ; PtlWriteEOL ( ) 
      END (* Block *) 
    END PrintTableLine 

; PROCEDURE PrintTable ( Gram : LRTable . GrammarTyp ; InfoWr : Wr . T ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LStateNo : LRTable . StateNoTyp 
  ; VAR LTableLine : LRTable . StateNoArrayRefTyp 
  ; VAR LDefaultStateNo : LRTable . StateNoTyp 

  ; BEGIN (* PrintTable *) 
      Gen . InitTableLine ( Gram , LTableLine ) 
    ; Wr . PutText ( InfoWr , Wr . EOL ) 
    ; Wr . PutText 
        ( InfoWr , "(* ************** Simple LR Table *************** *) " ) 
    ; Wr . PutText ( InfoWr , Wr . EOL ) 
    ; Wr . PutText 
        ( InfoWr , "(* ********* No default transitions shown ********* *) " ) 
    ; Wr . PutText ( InfoWr , Wr . EOL ) 
    ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO 
        Gen . InitTableLine ( Gram , LTableLine ) 
      ; LStateNo 
          := Gen . MakeTableLine ( Gram , RStateSs , (* VAR *) LTableLine ) 
      ; IF LStateNo < Gram . FirstReadRedAction 
        THEN 
          LDefaultStateNo := Gram . DefaultRef ^ [ LStateNo ] 
        ; PrintTableLine 
            ( Gram , InfoWr , LStateNo , LTableLine , LDefaultStateNo , TRUE ) 
        END (* IF *) 
      END (* FOR *) 
    ; LTableLine := NIL 
    ; Wr . PutText ( InfoWr , Wr . EOL ) 
    END PrintTable 

; PROCEDURE ReconstructTableLine 
    ( Gram : LRTable . GrammarTyp 
    ; StateNo : LRTable . StateNoTyp 
    ; VAR Line : LRTable . StateNoArrayRefTyp 
    ) 
  RAISES { Backout } 

  = BEGIN (* ReconstructTableLine *) 
      FOR RTok := Gram . FirstTerminal TO Gram . LastNonterminal 
      DO Line ^ [ RTok - Gram . FirstTerminal ] 
           := LRTable . Action ( Gram , StateNo , RTok ) 
      END (* FOR *) 
    END ReconstructTableLine 

; PROCEDURE CheckTable ( Gram : LRTable . GrammarTyp ; InfoWr : Wr . T ) 
  RAISES { Backout } 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LStateNo : LRTable . StateNoTyp 
  ; VAR LTableLine : LRTable . StateNoArrayRefTyp 
  ; VAR LAction : LRTable . ParseActionTyp 
  ; VAR LReconstructedAction : LRTable . ParseActionTyp 
  ; VAR LReconstructedTableLine : LRTable . StateNoArrayRefTyp 
  ; VAR LDiscrepancyFound : BOOLEAN 

  ; BEGIN (* CheckTable *) 
      Wr . PutText ( InfoWr , Wr . EOL ) 
    ; Wr . PutText 
        ( InfoWr 
        , "(* **************** Complete Table **************** *) " 
        ) 
    ; Wr . PutText ( InfoWr , Wr . EOL ) 
    ; Wr . PutText 
        ( InfoWr 
        , "(* ********* Default transitions inserted ********* *) " 
        ) 
    ; Wr . PutText ( InfoWr , Wr . EOL ) 
    ; LDiscrepancyFound := FALSE 
    ; FOR RStateSs := LALRTypes . FirstRealStateSs TO Gram . NextStateSs - 1 
      DO 
        Gen . InitTableLine ( Gram , LTableLine ) 
      ; Gen . InitTableLine ( Gram , LReconstructedTableLine ) 
      ; LStateNo 
          := Gen . MakeTableLine ( Gram , RStateSs , (* VAR *) LTableLine ) 
      ; IF LStateNo < Gram . FirstReadRedAction 
        THEN 
          ReconstructTableLine ( Gram , LStateNo , LReconstructedTableLine ) 
        ; FOR RTok := Gram . FirstTerminal TO Gram . LastNonterminal 
          DO LReconstructedAction 
               := LReconstructedTableLine ^ [ RTok - Gram . FirstTerminal ] 
          ; LAction := LTableLine ^ [ RTok - Gram . FirstTerminal ] 
          ; IF LReconstructedAction # LAction 
               AND ( RTok <= Gram . LastAstNonterminal 
                     OR LAction # LRTable . StateNoNull 
                   ) 
            THEN 
(* TODO: This is a mess. *) 
              IF FALSE AND NOT LDiscrepancyFound 
              THEN 
                Wr . PutText ( InfoWr , Wr . EOL ) 
              ; Wr . PutText 
                  ( InfoWr 
                  , "(* **************** Complete Table **************** *) " 
                  ) 
              ; Wr . PutText ( InfoWr , Wr . EOL ) 
              ; Wr . PutText 
                  ( InfoWr 
                  , "(* ********* Default transitions inserted ********* *) " 
                  ) 
              ; Wr . PutText ( InfoWr , Wr . EOL ) 
              ; Wr . PutText ( InfoWr , Wr . EOL ) 
              ; LDiscrepancyFound := TRUE 
              END (* IF *) 
            ; Wr . PutText ( InfoWr , "Tables disagree, State " ) 
            ; Wr . PutText ( InfoWr , StateSsImage ( LStateNo ) ) 
            ; Wr . PutText ( InfoWr , ", Token " ) 
            ; Wr . PutText ( InfoWr , Gram . tokImage ( RTok ) ) 
            ; Wr . PutText ( InfoWr , ", Original action: " ) 
            ; Wr . PutText ( InfoWr , Fmt . Int ( LAction ) ) 
            ; Wr . PutText ( InfoWr , ", Reconstructed action: " ) 
            ; Wr . PutText ( InfoWr , Fmt . Int ( LReconstructedAction ) ) 
            ; Wr . PutText ( InfoWr , Wr . EOL ) 
            END (* IF *) 
          END (* FOR *) 
        ; PrintTableLine 
            ( Gram , InfoWr , LStateNo , LReconstructedTableLine ) 
        END (* IF *) 
      END (* FOR *) 
    ; LTableLine := NIL 
    ; Wr . PutText ( InfoWr , Wr . EOL ) 
    END CheckTable 

; PROCEDURE PrintReduceInfo ( Gram : LRTable . GrammarTyp ; InfoWr : Wr . T ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* PrintReduceInfo *) 
      IF Gram . ReduceInfoRef = NIL 
      THEN
        Wr . PutText ( InfoWr , Wr . EOL ) 
      ; Wr . PutText 
          ( InfoWr 
          , "(* *********** No ReduceInfo available ************ *) " 
          ) 
      ; Wr . PutText ( InfoWr , Wr . EOL ) 
      ; Wr . PutText ( InfoWr , Wr . EOL ) 
      ELSE 
        Wr . PutText ( InfoWr , Wr . EOL ) 
      ; Wr . PutText 
          ( InfoWr 
          , "(* ****************** ReduceInfo ****************** *) " 
          ) 
      ; Wr . PutText ( InfoWr , Wr . EOL ) 
      ; FOR RProdSs := 0 TO NUMBER ( Gram . ReduceInfoRef ^ ) - 1 
        DO
          WITH WReduceInfo = Gram . ReduceInfoRef ^ [ RProdSs ] 
          DO  
            Wr . PutText ( InfoWr , "ReduceInfo (" ) 
          ; Wr . PutText ( InfoWr , ProdNoImage ( RProdSs ) ) 
          ; Wr . PutText ( InfoWr , ") = " ) 

          ; Wr . PutText ( InfoWr , Gram . tokImage ( WReduceInfo . LhsTok  ) ) 
          ; IF WReduceInfo . BuildTok # LbeStd . Tok__Null 
            THEN
              Wr . PutText ( InfoWr , " BUILD " )             
            ; Wr . PutText 
                ( InfoWr , Gram . tokImage ( WReduceInfo . BuildTok ) ) 
            END (* IF *) 
          ; Wr . PutChar ( InfoWr , ',' ) 
          ; Wr . PutText ( InfoWr , Fmt . Int ( WReduceInfo . RhsLen ) ) 
          ; Wr . PutChar ( InfoWr , ',' ) 
          ; Wr . PutText 
              ( InfoWr 
              , LRTable . OptionIdSetImage ( WReduceInfo . OptionIdSet ) 
              )  
          ; Wr . PutText ( InfoWr , Wr . EOL ) 
          END (* WITH *) 
        END (* FOR *) 
      ; Wr . PutText ( InfoWr , Wr . EOL ) 
      END (* IF *) 
    END PrintReduceInfo 

(* VISIBLE: *) 
; PROCEDURE WriteLRTables 
    ( Gram : LRTable . GrammarTyp 
    ; WrT : Wr . T := NIL (* NIL means use Messages . MessageWr ( ) *) 
    )
  RAISES { Backout } 

  = VAR LWrT : Wr . T 

  ; BEGIN 
      IF WrT = NIL 
      THEN 
        LWrT := Messages . MessageWr ( ) 
      ELSE 
        LWrT := WrT 
      END (* IF *) 
    ; PrintTable ( Gram , LWrT ) 
    ; CheckTable ( Gram , LWrT ) 
    ; PrintReduceInfo ( Gram , LWrT ) 
    END WriteLRTables 

; BEGIN (* Infos *) 
  END Infos 
. 

