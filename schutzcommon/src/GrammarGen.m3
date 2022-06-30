
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE GrammarGen 

(* Generate concrete syntax rules from format syntax (and also abstract 
   syntax).
*) 

; IMPORT Fmt 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout  
; IMPORT Automaton
; IMPORT ClassInfo 
; IMPORT IntSets
; IMPORT LangUtil 
; FROM LangUtil IMPORT PredicateKindTyp , FsKindTyp , CardTyp , CardSetTyp 
; FROM LangUtil 
  IMPORT CardSetEmpty , CardSetAbsent , CardSetList , CardSetEmptyList 
         , CardSetNonemptyList , CardSetPluralList , CardSetNonpluralList 
; IMPORT LangUtilLo 
; IMPORT LbeStd 
; FROM LbeStd IMPORT TokClassTyp 
; IMPORT LdlSemantics 
; FROM LdlSemantics 
  IMPORT FsTreeMapBias , TokMapBias , LangInfoRefTyp
; IMPORT LALRTypes 
; IMPORT LRTable 
; FROM LRTable IMPORT NontermKindTyp 
; IMPORT MessageCodes 
; IMPORT Messages 
; FROM Messages IMPORT SemError 
; IMPORT PortTypes 
; IMPORT SetClassInfoTbl 
; IMPORT TokString 
; IMPORT TokStringInfo 
; IMPORT TokStringInfoTbl
; IMPORT UncertainBool  

; TYPE AFT = MessageCodes . T 

; TYPE TokSetTyp = IntSets . T 

; CONST ListCardEmpty = LdlSemantics . ListCardTyp . ListCardEmpty 
; CONST ListCardSingleton = LdlSemantics . ListCardTyp . ListCardSingleton 
; CONST ListCardPlural = LdlSemantics . ListCardTyp . ListCardPlural 

; CONST UbTrue = UncertainBool . T . True  

; TYPE FragTyp 
  = RECORD 
      OptionIdSet : LRTable . OptionIdSetTyp 
    ; TokStringRef : LRTable . TokArrayRefTyp
    END 

  (* A Frag is a portion of a production RHS. *) 
; TYPE FragSetTyp = ARRAY OF FragTyp 
; TYPE FragSetRefTyp = REF FragSetTyp
; TYPE FragSetSetTyp = ARRAY OF FragSetRefTyp   
; TYPE FragSetSetRefTyp = REF FragSetSetTyp   

; PROCEDURE SingletonFragsetSingletonString ( Tok : LbeStd . TokTyp ) 
  : FragSetRefTyp 
  (* With empty OptionId set. *) 

  = VAR LResult : FragSetRefTyp 

  ; BEGIN (* SingletonFragsetSingletonString *)
      LResult := NEW ( FragSetRefTyp , 1 )
    ; LResult ^ [ 0 ] . TokStringRef := LRTable . SingletonString ( Tok )  
    ; LResult ^ [ 0 ] . OptionIdSet := LRTable . OptionIdSetEmpty 
    ; RETURN LResult 
    END SingletonFragsetSingletonString

; PROCEDURE SingletonFragset ( TokString : LRTable . TokArrayRefTyp ) 
  : FragSetRefTyp 
  (* With empty OptionId set. *) 

  = VAR LResult : FragSetRefTyp 

  ; BEGIN (* SingletonFragset *)
      LResult := NEW ( FragSetRefTyp , 1 )
    ; LResult ^ [ 0 ] . TokStringRef := TokString 
    ; LResult ^ [ 0 ] . OptionIdSet := LRTable . OptionIdSetEmpty 
    ; RETURN LResult 
    END SingletonFragset

; PROCEDURE ListToksSubset 
    ( LangInfo : LangInfoRefTyp ; ParentSet : TokSetTyp ) 
  : TokSetTyp 
  (* The set of tokens from ParentSet that are abstract list tokens. *) 

  = VAR LResult : TokSetTyp 

  ; BEGIN 
      LResult 
        := IntSets . Project 
             ( ParentSet 
             , LangInfo . VarTermModToks 
             , LangInfo . AsStarToks - 1 
             )  
    ; RETURN LResult 
    END ListToksSubset

; PROCEDURE StarListToksSubset 
    ( LangInfo : LangInfoRefTyp ; ParentSet : TokSetTyp ) 
  : TokSetTyp 
  (* The set of tokens from ParentSet that are abstract star list tokens. *) 

  = VAR LResult : TokSetTyp 

  ; BEGIN 
      LResult 
        := IntSets . Project 
             ( ParentSet , LangInfo . AsPlusToks , LangInfo . AsStarToks - 1 )  
    ; RETURN LResult 
    END StarListToksSubset



(* TODO:
          (* Check for duplicate fragment.  Assume N is small enough so it
             is not worth hashing.  Just do a brute-force O(N**2) check. *) 
          ; FOR RI := 0 TO P1fsrFssNextProductSs - 1 
            DO IF TokArrayEqual ( P1fsrFssProductFragSet ^ [ RI ] , LNewFrag ) 
              THEN
                SemError 
                  ( FsSubtreeNodeRef . FsLdlNodeNo 
                  , AFT . W_Ambiguous_FS_rule
                  ) 
              END (* IF *) 
            END (* FOR *) 
*)





; TYPE GrammarSubTyp 
    = LRTable . GrammarTyp 
        OBJECT 
          LangInfo : LangInfoRefTyp 
        OVERRIDES 
          tokImage := TokImage 
        END (* OBJECT *) 

; PROCEDURE TokImage 
    ( <* UNUSED *> Self : GrammarSubTyp ; Tok : LbeStd . TokTyp ) : TEXT 
(* TODO: By using the global hack of LbeStd . LangLdlAnalyzed, we avoid
         needing Self to get the LangInfo.  That would allow dispensing
         entirely with this method and having everything call LangUtil.TokImage
         directly.  Better yet, just put Lang into GrammarTyp and dispense with 
         GrammarSubTyp altogether?  But it's hard to avoid circular imports 
         here.  Straighten this out somehow. 
*) 
  = BEGIN (* TokImage *) 
      RETURN LangUtil . TokImage ( Tok , LbeStd . LangLdlAnalyzed ) 
    END TokImage 

(* Sets of list cardinalities. *) 

; PROCEDURE ClassTokProdCt 
    ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  : CARDINAL 
    (* Number of CS productions needed by Tok if it is a class, or zero. *) 

  = BEGIN 
      TYPECASE LdlSemantics . SemDeclOfTok ( LangInfo , Tok ) 
      OF NULL (* A previously reported error. *)
      => RETURN 0 
      | LdlSemantics . SemFirstOccClassTyp ( TSemClassDecl ) 
      => RETURN LdlSemantics . CountOfNonclassTokSetProductions 
                  ( LangInfo , TSemClassDecl . TokSet ) 
      ELSE RETURN 0 
      END (* TYPECASE *) 
    END ClassTokProdCt 

; PROCEDURE PreloadNamedClasses 
    ( LangInfo : LangInfoRefTyp ; Gram : GrammarSubTyp ) 
  (* Create a new ClassTable in LangInfo and load it with classes named in the 
     language definition. 
  *) 

  = VAR LMemberTokSet : TokSetTyp 
  ; VAR LClassInfoRef : ClassInfo . ClassInfoRefTyp 
  ; VAR LPrevClassInfoRef : ClassInfo . ClassInfoRefTyp 

  ; BEGIN 
(* CHECK: Do we need two class tables?  For manual and generated grammars? *) 
      IF LangInfo # NIL AND Gram # NIL 
      THEN 
        LangInfo ^ . ClassTable 
          := NEW ( SetClassInfoTbl . Default ) 
             . init 
                 ( sizeHint 
                     := LangInfo ^ . AsCsClassToks - LangInfo ^ . AsPartialToks 
                 )
      ; FOR RTok 
            := LangInfo ^ . AsPartialToks TO LangInfo ^ . AsCsClassToks - 1 
        DO 
          LMemberTokSet := LdlSemantics . TokSetOfTok ( LangInfo , RTok ) 
        ; IF IntSets . IsEmpty ( LMemberTokSet ) 
          THEN
            Messages . SemErrorText 
              ( ": " & Gram . tokImage ( RTok ) 
              , AFT . I_Class_is_empty 
              ) 
          END (* IF *) 
        ; LClassInfoRef := NEW ( ClassInfo . ClassInfoRefTyp ) 
        ; LClassInfoRef ^ . ClassTok := RTok 
        ; LClassInfoRef ^ . UseCount := 0
        ; IF LangInfo . ClassTable . get 
               ( LMemberTokSet , (* VAR *) LPrevClassInfoRef ) 
          THEN
            CASE IntSets . Card ( LMemberTokSet ) 
            OF 0 => (* Ignore here, was informed about earlier. *)  
            | 1 
            => Messages . SemErrorText 
                ( ": " 
                  & Gram . tokImage ( LPrevClassInfoRef ^ . ClassTok ) 
                  & " and " 
                  & Gram . tokImage ( RTok ) 
                , AFT . I_Singleton_classes_rename_the_same_token
                ) 
            ELSE 
              Messages . SemErrorText 
                ( ": " 
                  & Gram . tokImage ( LPrevClassInfoRef ^ . ClassTok ) 
                  & " and " 
                  & Gram . tokImage ( RTok ) 
                , AFT . I_Classes_have_identical_contents
                )
            END (* CASE *)  
          (* Keep the old ClassInfoRef to use the 1st token. *) 
          ELSE 
            EVAL LangInfo . ClassTable . put ( LMemberTokSet , LClassInfoRef ) 
          END (* IF *) 
        END (* FOR *) 
      END (* IF *) 
    END PreloadNamedClasses 

; PROCEDURE AltFrag 
    ( FsAltNodeRef : LangUtil . FsNodeRefTyp 
      (* ^An FsNode with exactly one EstChild descendent. *) 
    ; PrincipalChildTok : LbeStd . TokTyp
      (* ^LbeStd.Tok__Null means omit principal child from the fragment 
         entirely.  Otherwise, insert PrincipalChildTok in place of any 
         AS child. *)
    )
  : LRTable . TokArrayRefTyp 
  RAISES { Backout } 
  (* A token string containing the tokens FsAltNodeRef will format to,
     with the Est child token replaced by PrincipalChildTok.  
     Always allocates a new token string.  Never mutates or reuses a
     previously existing one. *) 

  = PROCEDURE AfCount ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
    : CARDINAL
    (* Count the regular (non-Alt) tokens needed. *)   

    = VAR LResult : CARDINAL  

    ; BEGIN
        CASE FsNodeRef . FsKind <* NOWARN *> 
        OF FsKindTyp . FsKindInsTok 
        => RETURN 1 
        | FsKindTyp . FsKindEstChildOfFixed 
        , FsKindTyp . FsKindEstChildOfList  
        => RETURN ORD ( PrincipalChildTok # LbeStd . Tok__Null ) 
        | FsKindTyp . FsKindLineBreakOpt 
        , FsKindTyp . FsKindLineBreakReqd
        , FsKindTyp . FsKindBegOfImage (* Could this happen? *) 
        , FsKindTyp . FsKindEndOfImage (* Could this happen? *) 
        => RETURN 0  
        | FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeHoriz 
        , FsKindTyp . FsKindSubtreeFill
        , FsKindTyp . FsKindCondFmt 
        => LResult := 0 
        ; FOR RI := 0 TO NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
          DO INC ( LResult 
                 , AfCount ( FsNodeRef . FsChildren ^ [ RI ] ) 
                 ) 
          END (* FOR *) 
        ; RETURN LResult 
        END (* CASE *) 
      END AfCount 

  ; VAR AfRhsString : LRTable . TokArrayRefTyp 
  ; VAR AfNextSs : PortTypes . Int32Typ 

  ; PROCEDURE AfFill ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Backout } 

    = BEGIN
        CASE FsNodeRef . FsKind <* NOWARN *> 
        OF FsKindTyp . FsKindInsTok 
        => AfRhsString ^ [ AfNextSs ] := FsNodeRef . FsTok 
        ; INC ( AfNextSs ) 
        | FsKindTyp . FsKindEstChildOfFixed 
        , FsKindTyp . FsKindEstChildOfList  
        => IF PrincipalChildTok # LbeStd . Tok__Null  
          THEN 
            AfRhsString [ AfNextSs ] := PrincipalChildTok
          ; INC ( AfNextSs ) 
          END (* IF *) 
        | FsKindTyp . FsKindLineBreakOpt 
        , FsKindTyp . FsKindLineBreakReqd
        , FsKindTyp . FsKindBegOfImage (* Could this happen? *) 
        , FsKindTyp . FsKindEndOfImage (* Could this happen? *) 
        => (* Ignore these. *)  
        | FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeHoriz 
        , FsKindTyp . FsKindSubtreeFill
        , FsKindTyp . FsKindCondFmt 
        => FOR RI := 0 TO NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
          DO AfFill ( FsNodeRef . FsChildren ^ [ RI ] ) 
          END (* FOR *) 
        END (* CASE *) 
      END AfFill 

  ; BEGIN (* AltFrag *) 
      VAR LFragLen : CARDINAL 

    ; BEGIN (* Block AltFrag *) 
        LFragLen := AfCount ( FsAltNodeRef ) 
      ; INC ( LFragLen , ORD ( DoInsertPredMarkTok ) ) 
      ; AfRhsString := NEW ( LRTable . TokArrayRefTyp , LFragLen )  
        (* ^Even if LFragLen = 0. *) 
      ; AfNextSs := 0
      ; AfFill ( FsAltNodeRef ) 
      ; IF DoInsertPredMarkTok 
        THEN
          AfRhsString [ AfNextSs ] := PredMarkTok
        ; INC ( AfNextSs ) 
        END (* IF *) 
      ; Assert 
          ( AfNextSs = LFragLen 
          , AFT . A_AltFrag_Count_mismatch 
          )
      ; RETURN AfRhsString  
      END (* Block AltFrag *)  
    END AltFrag 

; CONST LineLen = 78 (* TODO: Move this somewhere common. *) 

; PROCEDURE CheckChildrenFormatEmpty 
    ( LangInfo : LangInfoRefTyp 
    ; ListTokSet : TokSetTyp 
    ; FixedTokSet : TokSetTyp 
    ; CardSet : CardSetTyp 
    ; FsRuleNodeRef : LangUtil . FsNodeRefTyp 
    ; ChildNo : CARDINAL 
    ; AltNo : CARDINAL 
    ; Message : AFT 
    ; MinCount : CARDINAL 
    ) 
  (* Emit message(s) saying there is ambiguity in the FS for a single 
     alternative of a single Est child, due to children that can format empty.  
     If at least MinCount of the children in the union of FixedSet and ListSet,
     with the list cardinalities in CardSet applied to those in ListSet, can
     format empty, emit a message.  The other parameters are for
     identifying information in the message.
  *)  

  = VAR CcfeChildTokSet : TokSetTyp 
  ; VAR CcfeChildCt : CARDINAL 
  ; VAR CcfeOutPos : CARDINAL 
  ; VAR CcfeTokEmitted : BOOLEAN 
  ; VAR CcfeWr : Wr . T 

  ; PROCEDURE CcfeVisitFixedTok ( ChildTok : IntSets . ValidElemT ) 

    = VAR LFixedFsRuleNode : LangUtil . FsNodeRefTyp 

    ; BEGIN 
        LFixedFsRuleNode 
          := LangInfo . FsTreeMapRef ^ [ ChildTok - FsTreeMapBias ] 
     ; IF LFixedFsRuleNode . FsFormatsEmpty = UbTrue 
          THEN 
            CcfeChildTokSet := IntSets . Include ( CcfeChildTokSet , ChildTok )
          END (* IF *) 
      END CcfeVisitFixedTok 

  ; PROCEDURE CcfeVisitListTok ( ChildTok : IntSets . ValidElemT ) 

    = VAR LListFsRuleNode : LangUtil . FsNodeRefTyp 
    ; VAR LFormatsEmpty : BOOLEAN 

    ; BEGIN 
        LListFsRuleNode 
          := LangInfo . FsTreeMapRef ^ [ ChildTok - FsTreeMapBias ] 
      ; IF NOT LListFsRuleNode . FsEstListHasBookends 
        THEN
          LFormatsEmpty := FALSE (* May change. *) 
        ; IF CardTyp . EmptyList IN CardSet 
             AND LangInfo . AsPlusToks <= LListFsRuleNode . FsTok 
             AND LListFsRuleNode . FsTok < LangInfo . AsStarToks  
          THEN LFormatsEmpty := TRUE 
          END (* IF *) 
        ; IF CardTyp . SingletonList IN CardSet 
             AND LListFsRuleNode . FsFormatsEmpty = UbTrue 
          THEN LFormatsEmpty := TRUE 
          END (* IF *) 
        ; IF CardTyp . PluralList IN CardSet 
             AND LListFsRuleNode . FsFormatsEmpty = UbTrue 
             AND NOT LListFsRuleNode . FsEstListHasSeps 
          THEN LFormatsEmpty := TRUE 
          END (* IF *) 
        ; IF LFormatsEmpty 
          THEN 
            CcfeChildTokSet := IntSets . Include ( CcfeChildTokSet , ChildTok )
          END (* IF *) 
        END (*( IF *)  
      END CcfeVisitListTok 

  ; PROCEDURE CcfeVisitChild ( ChildTok : IntSets . ValidElemT ) 

    = <* FATAL Thread . Alerted *> 
      <* FATAL Wr . Failure *> 
      VAR LTokImage : TEXT 
    ; VAR LTokImageLen : CARDINAL 

    ; BEGIN  
        IF CcfeTokEmitted 
        THEN 
          Wr . PutChar ( CcfeWr , ',' ) 
        ; INC ( CcfeOutPos ) 
        END (* IF *) 
      ; LTokImage := LangInfo . GenGram . tokImage ( ChildTok ) 
      ; LTokImageLen := Text . Length ( LTokImage ) 
      ; IF CcfeOutPos + LTokImageLen > LineLen 
        THEN 
          Wr . PutText ( CcfeWr , Wr . EOL ) 
        ; Wr . PutText ( CcfeWr , "  " ) 
        ELSE 
          Wr . PutChar ( CcfeWr , ' ' ) 
        END (* IF *) 
      ; Wr . PutText ( CcfeWr , LTokImage ) 
      ; INC ( CcfeOutPos , LTokImageLen ) 
      END CcfeVisitChild 

  ; <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* CheckChildrenFormatEmpty *) 
      VAR LSetCard : CARDINAL 
    ; VAR LTokNoun : TEXT 

    ; BEGIN (* Block. *) 
        CcfeChildTokSet := IntSets . Empty ( ) 
      ; CcfeChildCt := 0 
      ; <* FATAL ANY *> BEGIN 
          IntSets . ForAllDo ( ListTokSet , CcfeVisitListTok ) 
        ; IntSets . ForAllDo ( FixedTokSet , CcfeVisitFixedTok ) 
        END (* Block. *) 
      ; LSetCard := IntSets . Card ( CcfeChildTokSet ) 
      ; IF LSetCard + ORD ( CardTyp . Absent IN CardSet )  >= MinCount 
        THEN 
          IF LSetCard = 1 
          THEN LTokNoun := "token" 
          ELSE LTokNoun := "tokens" 
          END (* IF *) 
        ; Messages . SemErrorText 
            ( " Parent node " 
              & LangInfo . GenGram . tokImage ( FsRuleNodeRef . FsTok ) 
              & ", AS child " 
              & Fmt . Int ( ChildNo ) 
              & ", alternative "
              & Fmt . Int ( AltNo ) 
              & LTokNoun 
            , Message 
            )  
        ; CcfeWr := Messages . MessageWr ( ) 
        ; IF CardTyp . Absent IN CardSet 
          THEN 
            Wr . PutText ( CcfeWr , "   " ) 
          ; Wr . PutText ( CcfeWr , LbeStd . LeftPlaceholderDelimText ) 
          ; Wr . PutText ( CcfeWr , "Absent" )
          ; Wr . PutText ( CcfeWr , LbeStd . RightPlaceholderDelimText ) 
          ; CcfeOutPos := 11 
          ; CcfeTokEmitted := TRUE 
          ELSE 
            Wr . PutText ( CcfeWr , "  " ) 
          ; CcfeOutPos := 3 
          ; CcfeTokEmitted := FALSE 
          END (* IF *) 
        ; <* FATAL ANY *> BEGIN 
            IntSets . ForAllDo ( CcfeChildTokSet , CcfeVisitChild ) 
          END (* Block. *) 
        ; Wr . PutText ( CcfeWr , Wr . EOL ) 
        END (* IF *) 
      END (* Block. *) 
    END CheckChildrenFormatEmpty 

; VAR GPossibleTokstringAtomTokCt : CARDINAL  
(* FIXME: This is a mess.  We need the final count of nonterminals before
          initializing the grammar.  This includes atom tokens for factored 
          substrings.  But we can't atomize these without generating the
          substrings, which we aren't doing until pass 4, and the grammar
          has to be initialized before pass 4.  

          The right way would be for pass 1 to generate the substrings and
          atomize them.  That would involve a lot of rework, as pass 1 only
          counts things, right now.  

          So as a quick kludge, we count, during pass 1, all the alternatives, 
          in GPossibleTokstringAtomTokCt.  This is guaranteed to be enough,
          and maybe not just absurdly extravagant.  We use this to initialize
          the grammar, which surely means some unused heap array capacity.  

          GPossibleTokstringAtomTokCt is also an inappropriate global variable, 
          but hopefully the counting problem will be fixed before we need to 
          make this code thread-safe.
*)  

; PROCEDURE FragCountForCondConstruct 
    ( <* UNUSED *> LangInfo : LangInfoRefTyp  
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  : CARDINAL (* Number of alternatives. *) 
  (* PRE: FsAltFragCt fields have been computed and stored.  
     FsNodeRef is the first alternative of a conditional construct.  It
     can also be a (unconditional) principal child.  
     Return the total count of fragments for all alternatives. 
  *) 

  = VAR LResult : CARDINAL 
  ; VAR LFsAltNodeRef : LangUtil . FsNodeRefTyp 

  ; BEGIN 
      LResult := 0 
    ; LFsAltNodeRef := FsNodeRef 
(* FIXME: If there is a subtree on top of a predicate list, this
          will treat the subtree as a single, unconditional alternative,
          which is wrong.  But if the subtree is not on top of a predicate,
          that would be right. *) 
    ; WHILE LFsAltNodeRef # NIL 
      DO
(* REVIEW: Check this for unconditional present child.  ??? *) 
        INC ( LResult , LFsAltNodeRef . FsAltFragCt )  
      ; LFsAltNodeRef := LFsAltNodeRef . FsCondAltRef 
      END (* WHILE *) 
    ; RETURN LResult 
    END FragCountForCondConstruct  

; PROCEDURE WarnListAmbiguity 
    ( Gram : GrammarSubTyp 
    ; FsListRuleNodeRef : LangUtil . FsNodeRefTyp 
    ; ListPrefix : TEXT 
    ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LFsDecoratedElemNodeRef : LangUtil . FsNodeRefTyp 
  ; VAR LFsAltRef : LangUtil . FsNodeRefTyp 
  ; VAR LMessageTerm : TEXT 
  ; VAR LAltNumber : CARDINAL 
  ; VAR LWrT : Wr . T 

  ; BEGIN 
      LFsDecoratedElemNodeRef 
        := LdlSemantics . FsCondOrEstOfFsList ( FsListRuleNodeRef )
    ; IF LFsDecoratedElemNodeRef . FsKind = FsKindTyp . FsKindCondFmt 
      THEN LMessageTerm := "," 
      ELSE LMessageTerm := "."
      END (* IF *) 
    ; Messages . SemErrorText
        ( ": " 
          & ListPrefix 
          & " " 
          & Gram . tokImage ( FsListRuleNodeRef . FsTok ) 
          & " has empty list child with no insertion tokens"
          & LMessageTerm 
        , AFT . W_Ambiguity
        ) 
    ; IF LFsDecoratedElemNodeRef . FsKind = FsKindTyp . FsKindCondFmt 
      THEN 
        LFsAltRef := LFsDecoratedElemNodeRef
      ; LAltNumber := 1 
      ; LWrT := Messages . MessageWr ( ) 
      ; Wr . PutText ( LWrT , "  for alternative(s) " )  
      ; LOOP 
          IF LFsAltRef . FsFormatsEmpty = UbTrue 
          THEN
            IF LFsAltRef # LFsDecoratedElemNodeRef 
            THEN 
              Wr . PutText ( LWrT , ", " ) 
            END (* IF *) 
          ; Wr . PutText ( LWrT , Fmt . Int ( LAltNumber ) )
          END (* IF *) 
        ; IF LFsAltRef . FsKind # FsKindTyp . FsKindCondFmt 
             OR LFsAltRef . FsCondPredicate . PredicateKind 
                = PredicateKindTyp . PredicateKindTrue 
          THEN EXIT 
          ELSE 
            LFsAltRef := LFsAltRef . FsCondAltRef 
          ; INC ( LAltNumber ) 
          ; IF LFsAltRef = NIL THEN EXIT END (* IF *) 
               (* Probably can't happen. *) 
          END (* IF *) 
        END (* LOOP *) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      END (* IF *) 
    END WarnListAmbiguity 

; PROCEDURE Pass1FsRule 
    ( LangInfo : LangInfoRefTyp ; FsRuleNodeRef : LangUtil . FsNodeRefTyp ) 
  RAISES { Backout } 
  (* 1) Count concrete syntax rules needed for this FS rule, 
        in LangInfo ^ . GenProductions. 
     2) Set the FsAltFragCt fields of each predicate and unconditional principal
        child FsNode. 
     3) If token classes are to be factored out (GDoFactorClasses), 
        create a token for each unique principal child set, storing a copy
        of this token in FsCondPredicate . PredicateClass.  These tokens 
        are also accessible from LangInfo ^ . ClassTable, using the set 
        as search key. 
     4) If token substrings are to be factored out (GDoFactorSubstrings), 
        create a token for each unique substring that occurs as an alternative, 
        storing a copy of this token in FsAltTok of the FsNode for the 
        alternative.  These tokens are also accessible from  
        LangInfo ^ . SubstringTable, using the substring as search key. 
CLEAN UP^ Not so, at least not yet.  
     This is unnested from Generate, just to keep things from getting 
     too deep. 
  *) 

  = VAR P1fsrChildNo : CARDINAL 
  ; VAR P1fsrAltNo : CARDINAL 

  ; PROCEDURE P1fsrRemainingCases 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; IsExplicitElse : BOOLEAN 
      )
    : CARDINAL (* Count of fragments. *)  
    RAISES { Backout } 
    (* Handle every case that satisfies both FsAltCardSet and the FsAlt*TokSet
       fields.   These are all for the same format alternative, denoted by
       FsNodeRef. *) 

    = VAR LResult : CARDINAL 
    ; VAR LListCard : CARDINAL 
    ; VAR LStarListTokSet : TokSetTyp 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LSingletonListElemTokSet : TokSetTyp   
    ; VAR LFixedTokSetCard : CARDINAL 
    ; VAR LTok : LbeStd . TokTyp 
    ; VAR LPredClassTokSet : TokSetTyp 

    ; BEGIN (* P1fsrRemainingCases *) 
        LResult := 0 
      ; LSingletonListElemTokSet := IntSets . Empty ( ) (* Could change. *)
      ; LListCard := IntSets . Card ( FsNodeRef . FsAltListTokSet ) 
      ; IF LListCard > 0 
        THEN 
          LStarListCard := IntSets . Card ( FsNodeRef . FsAltStarListTokSet ) 
        ELSE 
          LStarListCard := 0 
        END (* IF *) 
      ; LFixedTokSetCard := IntSets . Card ( FsNodeRef . FsAltFixedTokSet ) 
      ; CheckChildrenFormatEmpty 
          ( LangInfo 
          , FsNodeRef . FsAltFixedTokSet 
          , FsNodeRef . FsAltListTokSet 
          , FsNodeRef . FsAltCardSet  
          , FsRuleNodeRef 
          , P1fsrChildNo 
          , P1fsrAltNo 
          , AFT . W_Ambiguous_multiple_children_format_empty 
          , MinCount := 2 
          ) 

      (* Absent. *) 
      ; IF LangUtil . FsEstChildRef ( FsNodeRef ) . FsEstChildOpt
           IN LangUtil . ChildOptSetOptByAS 
           AND CardTyp . Absent IN FsNodeRef . FsAltCardSet 
        THEN 
          IF FsNodeRef . FsCondDoParse 
          THEN INC ( LResult ) 
          END (* IF *) 
        END (* IF *) 
 
      (* Empty list. *) 
      ; IF LStarListCard > 0 
           AND CardTyp . EmptyList IN FsNodeRef . FsAltCardSet 
        THEN
          IF FsNodeRef . FsCondDoParse 
          THEN INC ( LResult ) 
          END (* IF *) 
        END (* IF *) 

      (* Singleton and plural lists. *) 
      ; IF LListCard > 0 AND FsNodeRef . FsCondDoParse 
        THEN 
          IF CardTyp . SingletonList IN FsNodeRef . FsAltCardSet 
          THEN
            INC ( LResult , LListCard ) 
          END (* IF *) 
        ; IF CardTyp . PluralList IN FsNodeRef . FsAltCardSet 
          THEN
            INC ( LResult , LListCard ) 
          END (* IF *) 
        END (* IF *) 

      (* Fixed children. *) 
      ; IF LFixedTokSetCard > 0 AND FsNodeRef . FsCondDoParse  
        THEN 
          IF LFixedTokSetCard > 1 
          THEN 
            LTok 
              := LdlSemantics . TokForAtomTokSet (* SIDE EFFECTS! *)
                   ( LangInfo 
                   , FsNodeRef 
                   , FsNodeRef . FsAltFixedTokSet 
                   , UseCountIncrement := ORD ( GDoFactorClasses ) 
                   )
(* TODO: Decide whether/what for we need LTok. *) 
          ELSE 
            LTok := IntSets . ArbitraryMember ( FsNodeRef . FsAltFixedTokSet )  
          END (* IF *) 
(* FIXME: ^This does nothing but create an atom for the Fixed set, which
           we don't use here. 
           This is all wrong for GDoFactorClasses.  We need the class set
           to include any list cardinality tokens discovered above. *) 
        (* Here, LTok is the CS token that will derive the fixed set. *)  
(* TODO: What is the right class to store here?:  *) 
        ; LPredClassTokSet 
            := IntSets . Union 
                 ( FsNodeRef . FsAltFixedTokSet , FsNodeRef . FsAltListTokSet )
        ; LPredClassTokSet 
            := IntSets . Union ( LPredClassTokSet , LSingletonListElemTokSet )  
        ; EVAL LdlSemantics . TokForAtomTokSet (* SIDE EFFECTS! *)
                 ( LangInfo 
                 , FsNodeRef 
                 , LPredClassTokSet 
                 , UseCountIncrement := 0 
                 )
(* FIXME^ What is this token for? *) 
        ; IF GDoFactorClasses 
          THEN INC ( LResult ) (* Either a single Est tok or a class tok. *) 
          ELSE INC ( LResult , LFixedTokSetCard ) (* All toks in the class. *)  
          END (* IF *) 
        END (* IF *) 
      ; INC ( GPossibleTokstringAtomTokCt , LResult ) 
      ; RETURN LResult 
      END P1fsrRemainingCases  

  ; PROCEDURE P1fsrPredicate ( FsPredNodeRef : LangUtil . FsNodeRefTyp ) 
    : CARDINAL (* Count of fragments. *)  
    RAISES { Backout } 
    (* PRE: FsPredNodeRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 

    = VAR LListCard : CARDINAL 
    ; VAR LResult : CARDINAL 
    ; VAR LStarListCard : CARDINAL 

    ; BEGIN (* P1fsrPredicate *) 
        LResult := 0 
(* TODO: Handle overflow of LResult. *) 
      ; IF FsPredNodeRef = NIL 
        THEN 
(* REVIEW:  Can this happen? *)
          CantHappen ( AFT . A_P1fsrPredicate_NIL_Alternative )  
        ELSE 
          CASE FsPredNodeRef . FsKind <* NOWARN *> 
          OF FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill
          , FsKindTyp . FsKindEstChildOfFixed
          , FsKindTyp . FsKindEstChildOfList 
(* TODO: When NOPARSE addition is complete, this case should be impossible. 
   Why? *) 
          => (* Default case, no predicate, no ELSE. *)  
            LResult 
               := P1fsrRemainingCases  
                    ( FsPredNodeRef 
                    , IsExplicitElse := FALSE 
                    ) 

          | FsKindTyp . FsKindCondFmt 
          => CASE FsPredNodeRef . FsCondPredicate . PredicateKind 
            OF PredicateKindTyp . PredicateKindNull 
            , PredicateKindTyp . PredicateKindFalse 
            => CantHappen 
                 ( AFT . A_GrammarGen_P1fsrPredicate_Leftover_null_or_false_predicate ) 

            (* Explicit ELSE. *) 
            | PredicateKindTyp . PredicateKindTrue
            => LResult 
                 := P1fsrRemainingCases  
                      ( FsPredNodeRef 
                      , IsExplicitElse := TRUE 
                      ) 

            (* ABSENT. *) 
            | PredicateKindTyp . PredicateKindAbsent 
            => IF FsPredNodeRef . FsCondDoParse 
              THEN LResult := 1 
              END (* IF *) 

            (* PRESENT. *) 
            | PredicateKindTyp . PredicateKindPresent 
            => LResult 
                 := P1fsrRemainingCases 
                      ( FsPredNodeRef 
                      , IsExplicitElse := FALSE 
                      )

            (* EMPTY. *) 
            | PredicateKindTyp . PredicateKindEmptyList 
            => LStarListCard 
                := IntSets . Card ( FsPredNodeRef . FsAltStarListTokSet ) 
            ; IF FsPredNodeRef . FsCondDoParse 
              THEN 
                CheckChildrenFormatEmpty 
                  ( LangInfo 
                  , IntSets . Empty ( ) 
                  , FsPredNodeRef . FsAltStarListTokSet 
                  , FsPredNodeRef . FsAltCardSet 
                  , FsRuleNodeRef 
                  , P1fsrChildNo 
                  , P1fsrAltNo 
                  , AFT . W_Ambiguous_EMPTY_predicate_on_multiple_star_lists 
                  , MinCount := 2 
                  ) 
              ; LResult := 1 
(* FIXME^ Not necessarily. *) 
              END (* IF *) 

            (* NONEMPTY. *) 
            | PredicateKindTyp . PredicateKindNonemptyList
            => LListCard := IntSets . Card ( FsPredNodeRef . FsAltListTokSet ) 
            ; CheckChildrenFormatEmpty 
                ( LangInfo 
                , IntSets . Empty ( ) 
                , FsPredNodeRef . FsAltListTokSet 
                , FsPredNodeRef . FsAltCardSet  
                , FsRuleNodeRef 
                , P1fsrChildNo 
                , P1fsrAltNo 
                , AFT . W_Ambiguous_NONEMPTY_predicate_on__multiple_nonempty_lists 
                , MinCount := 2 
                ) 
            ; IF FsPredNodeRef . FsCondDoParse 
              THEN 
                IF CardTyp . SingletonList IN FsPredNodeRef . FsAltCardSet 
                THEN INC ( LResult , LListCard ) 
                END (* IF *) 
              ; IF CardTyp . PluralList IN FsPredNodeRef . FsAltCardSet 
                THEN INC ( LResult , LListCard ) 
                END (* IF *) 
              END (* IF *) 

            (* NONPLURAL. *) 
            | PredicateKindTyp . PredicateKindNonpluralList 
            => LListCard := IntSets . Card ( FsPredNodeRef . FsAltListTokSet ) 
            ; LStarListCard 
                := IntSets . Card ( FsPredNodeRef . FsAltStarListTokSet ) 
            ; CheckChildrenFormatEmpty 
                ( LangInfo 
                , IntSets . Empty ( ) 
                , FsPredNodeRef . FsAltListTokSet 
                , FsPredNodeRef . FsAltCardSet  
                , FsRuleNodeRef 
                , P1fsrChildNo 
                , P1fsrAltNo 
                , AFT . E_Ambiguous_NONPLURAL_predicate_on_multiple_star_lists
                , MinCount := 2 
                ) 
            ; IF FsPredNodeRef . FsCondDoParse 
              THEN 
                IF LStarListCard > 0 
                   AND CardTyp . EmptyList IN FsPredNodeRef . FsAltCardSet 
                THEN INC ( LResult , LStarListCard ) 
                END (* IF *) 
              ; IF CardTyp . SingletonList IN FsPredNodeRef . FsAltCardSet 
                THEN INC ( LResult , LListCard ) 
                END (* IF *) 
              END (* IF *) 

            (* PLURAL. *) 
            | PredicateKindTyp . PredicateKindPluralList 
            => LListCard := IntSets . Card ( FsPredNodeRef . FsAltListTokSet ) 
            ; IF FsPredNodeRef . FsCondDoParse 
              THEN 
                CheckChildrenFormatEmpty 
                  ( LangInfo 
                  , IntSets . Empty ( ) 
                  , FsPredNodeRef . FsAltListTokSet 
                  , FsPredNodeRef . FsAltCardSet 
                  , FsRuleNodeRef 
                  , P1fsrChildNo 
                  , P1fsrAltNo 
                  , AFT . W_Ambiguous_PLURAL_predicate_on__multiple_nonempty_lists 
                  , MinCount := 2 
                  ) 
              ; INC ( LResult , LListCard ) 
              END (* IF *) 

            (* MEMBER. *) 
            | PredicateKindTyp . PredicateKindInClass 
            => LResult 
                := P1fsrRemainingCases 
                     ( FsPredNodeRef 
                     , IsExplicitElse := FALSE 
                     )
            END (* CASE PredicateKind *) 

          | FsKindTyp . FsKindNull 
          , FsKindTyp . FsKindBegOfImage
          , FsKindTyp . FsKindEndOfImage 
          , FsKindTyp . FsKindLineBreakReqd
          , FsKindTyp . FsKindLineBreakOpt
          => CantHappen ( AFT . A_P1FsrPredicate__Bad_FsNode ) 
          END (* CASE FsKind *)  
        END (* IF *) 
      ; FsPredNodeRef . FsAltFragCt := LResult 
      ; INC ( GPossibleTokstringAtomTokCt , LResult ) 
      ; RETURN LResult 
      END P1fsrPredicate 

  ; PROCEDURE P1fsrCondConstruct 
      ( VAR FsCondFmtNodeRef : LangUtil . FsNodeRefTyp ) 
    : CARDINAL (* Count of fragments. *)  
    RAISES { Backout } 
    (* PRE: FsCondFmtNodeRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 
    (* NOT to be called recursively for subsequent predicates. *) 

    = VAR LOrigFsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR LEstChildFsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR LPredFsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR LCount : CARDINAL 
    ; VAR LResult : CARDINAL 

    ; BEGIN (* P1fsrCondConstruct *) 
        LOrigFsNodeRef := FsCondFmtNodeRef 
      ; LResult := 0 
(* CHECK: Be sure that, and document why, we can never have a NIL as an else
        alternative (i.e., FsCondFmtNodeRef # NIL *) 
      ; INC ( P1fsrChildNo )  
      ; P1fsrAltNo := 0 
      ; LEstChildFsNodeRef := LangUtil . FsEstChildRef ( FsCondFmtNodeRef )  
(* CHECK ^Each alternative can have its won EstChildRef.  What to do? *) 

      (* Process the alternatives. *) 
      ; LPredFsNodeRef := FsCondFmtNodeRef 
      ; WHILE LPredFsNodeRef # NIL 
        DO
          INC ( P1fsrAltNo )  
        ; LCount := P1fsrPredicate ( LPredFsNodeRef )
        ; INC ( LResult , LCount ) 
        ; LPredFsNodeRef := LPredFsNodeRef . FsCondAltRef 
        END (* WHILE *) 

      ; IF FsCondFmtNodeRef = NIL  
        THEN
          SemError
            ( LOrigFsNodeRef . FsLdlNodeNo 
            , AFT . E_No_satisfiable_alternative
            ) 
        END (* IF *) 
      ; RETURN LResult   
      END P1fsrCondConstruct 

  ; PROCEDURE P1fsrUncondPrincipalChild 
      ( FsChildNodeRef : LangUtil . FsNodeRefTyp )
    : CARDINAL (* Fragment count. *) 
    RAISES { Backout } 
    (* PRE: FsChildNodeRef is a principal child that is NOT inside a conditional
       construct.  It could still have multiple alternatives if it's optional 
       or contains list(s).
    *) 

    = VAR LResult : CARDINAL 
    ; VAR LListCard : CARDINAL 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LFixedTokSetCard : CARDINAL
    ; VAR LClassTok : LbeStd . TokTyp 

    ; BEGIN 
        LResult := 0 
(* TODO: Something about LResult overflowing. *) 
      ; INC ( P1fsrChildNo ) 
      ; LListCard := IntSets . Card ( FsChildNodeRef . FsAltListTokSet ) 
      ; IF LListCard > 0 
        THEN (* We have a list child. *) 
          LStarListCard 
            := IntSets . Card ( FsChildNodeRef . FsAltStarListTokSet ) 
        ; IF FsChildNodeRef . FsEstChildOpt IN LangUtil . ChildOptSetOptByAS 
          THEN
            INC ( LResult ) (* For absent. *)  
          ; IF LStarListCard > 0 
            THEN 
              SemError
                ( FsChildNodeRef . FsLdlNodeNo 
                , AFT . E_Ambiguous_optional_star_list 
                ) 
            END (* IF *) 
          ELSIF LStarListCard > 0 
          THEN
            INC ( LResult ) (* For <list>__0 *) 
          ; IF LStarListCard > 1 
            THEN 
              SemError
                ( FsChildNodeRef . FsLdlNodeNo 
                , AFT . E_Ambiguous_multiple_star_lists 
                ) 
            END (* IF *) 
          END (* IF *) 
        ; INC ( LResult , LListCard * 2 ) (* For <list>__1 and  <list>__2 *) 
        ELSE (* No list children. *) 
        END (* IF *) 
      ; LFixedTokSetCard := IntSets . Card ( FsChildNodeRef . FsAltFixedTokSet )
      ; IF LFixedTokSetCard > 0
        THEN
          IF LFixedTokSetCard > 1
          THEN
            LClassTok 
              := LdlSemantics . TokForAtomTokSet (* SIDE EFFECTS! *) 
                   ( LangInfo
                   , FsChildNodeRef
                   , FsChildNodeRef . FsAltFixedTokSet
                   , UseCountIncrement := ORD ( GDoFactorClasses ) 
                   )
          ; FsChildNodeRef . FsCondPredicate . PredicateClass := LClassTok 
(* FIXME: ^This is not what we really want, although it won't break anything. *)
          END (* IF *)
        ; IF GDoFactorClasses 
          THEN INC ( LResult ) (* Either a single Est tok or a class tok. *)   
          ELSE INC ( LResult , LFixedTokSetCard ) (* All toks in the class. *)
          END (* IF *)
        END (* IF *)
      ; FsChildNodeRef . FsAltFragCt := LResult 
      ; INC ( GPossibleTokstringAtomTokCt ) 
      ; RETURN LResult 
      END P1fsrUncondPrincipalChild 

  ; PROCEDURE P1fsrUncondFsSubtree 
      ( FsSubtreeNodeRef : LangUtil . FsNodeRefTyp ) 
    : CARDINAL (* Fragment count. *) 
    RAISES { Backout } 
    (* PRE: FsSubtreeNodeRef has FsKindEstFixed* or FsKindSubtree*. *) 
    (* PRE: We are NOT inside a conditional construct. *) 

    = VAR LChildCt : CARDINAL  
    ; VAR LChildFragCt : CARDINAL  
    ; VAR LProductCt : CARDINAL  

    ; BEGIN (* P1fsrUncondFsSubtree  *) 
        LChildCt := NUMBER ( FsSubtreeNodeRef . FsChildren ^ ) 
      ; LProductCt := 1 

      (* Traverse the children. *)
      ; FOR RI := 0 TO LChildCt - 1 
        DO  
          LChildFragCt 
            := P1fsrUncondFsChild 
                 ( FsSubtreeNodeRef . FsChildren ^ [ RI ] ) 
        ; IF LChildFragCt > 0 
          THEN LProductCt := LProductCt * LChildFragCt 
          END (* IF *) 
(* REVIEW: Why not multiply by zero?  It just means can't be formatted. 
           This should be rare or maybe impossible, but propagating this
           property is the right thing to do.
*)   
        END (* FOR *) 
      ; RETURN LProductCt 
      END P1fsrUncondFsSubtree 

  ; PROCEDURE P1fsrUncondFsChild 
      ( VAR FsChildNodeRef : LangUtil . FsNodeRefTyp ) 
    : CARDINAL (* Fragment count. *) 
    RAISES { Backout } 
    (* Called only when outside a conditional construct. *) 
    (* Mostly a dispatching procedure. *) 

    = BEGIN (* P1fsrUncondFsChild *) 
        CASE FsChildNodeRef . FsKind <* NOWARN *> 
        OF FsKindTyp . FsKindSubtreeHoriz
        , FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeFill 
        => RETURN P1fsrUncondFsSubtree ( FsChildNodeRef ) 

        | FsKindTyp . FsKindNull 
        , FsKindTyp . FsKindBegOfImage
        , FsKindTyp . FsKindEndOfImage 
        , FsKindTyp . FsKindLineBreakReqd
        , FsKindTyp . FsKindLineBreakOpt
        => RETURN 1 (* One fragment, empty string.*) 

        | FsKindTyp . FsKindInsTok 
        => RETURN 1 (* One fragment, the insertion token. *) 

        | FsKindTyp . FsKindEstChildOfFixed 
        , FsKindTyp . FsKindEstChildOfList  
        => RETURN P1fsrUncondPrincipalChild ( FsChildNodeRef ) 

        | FsKindTyp . FsKindCondFmt 
        => RETURN P1fsrCondConstruct ( FsChildNodeRef ) 

        END (* CASE *) 
      END P1fsrUncondFsChild 

  ; PROCEDURE P1fsrFixedRule ( FsFixedRuleNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Backout } 
    (* PRE: FsFixedRuleNodeRef has FsKindEstFixed*. *) 

    = VAR LFragCt : CARDINAL  

    ; BEGIN (* P1fsrFixedRule *) 
        LFragCt := P1fsrUncondFsSubtree ( FsFixedRuleNodeRef ) 
      ; INC ( LangInfo ^ . GenProductions , LFragCt)  
      END P1fsrFixedRule 

  ; PROCEDURE P1fsrListRule 
      ( FsListRuleNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Backout } 
    (* PRE: FsFixedRuleNodeRef has FsKindEstList*. *) 

    = VAR LFragCt : CARDINAL   
    ; VAR LAltCt : CARDINAL   
    ; VAR LFsElemNodeRef : LangUtil . FsNodeRefTyp 

    ; BEGIN (* P1fsrListRule *) 
        LFragCt 
          := P1fsrUncondFsChild 
               ( (* IN OUT *) FsListRuleNodeRef . FsChildren ^ [ 0 ] 
                 (* ^Possibly changed to remove unsatisfiable alternatives. *)
               ) 
      ; INC ( LangInfo ^ . GenProductions , LFragCt)  

      ; LFsElemNodeRef 
          := LdlSemantics . FsCondOrEstOfFsList ( FsListRuleNodeRef ) 
      ; LAltCt := FragCountForCondConstruct ( LangInfo , LFsElemNodeRef ) 

      ; TYPECASE 
          LdlSemantics . SemDeclOfTok ( LangInfo , FsListRuleNodeRef . FsTok ) 
        OF NULL => (* A previously reported error. *)

        | LdlSemantics . SemDeclAsListNodeTyp ( TSemDeclAsList ) 
        => (* Semantically analyzed as a list rule. *)
          IF TSemDeclAsList . ListCardUsed [ ListCardEmpty ] 
          THEN (* Empty list occurs as an Est child of another AS node, which 
                  further implies it's a star list. *) 
            INC ( LangInfo ^ . GenProductions ) 
            (* ^EmptyTok BUILD ListTok ::= <empty>. *)
          ; IF FsRuleNodeRef . FsFormatsEmpty = UbTrue 
            THEN (* This means the element, with per-element insertion tokens 
                    formats empty. *) 
              WarnListAmbiguity 
                ( LangInfo . GenGram , FsRuleNodeRef , "Star list" ) 
            END (* IF *) 
          END (* IF *)  

        ; IF TSemDeclAsList . ListCardUsed [ ListCardSingleton ] 
          THEN 
            INC ( LangInfo ^ . GenProductions , LAltCt ) 
            (* ^ SingletonTok BUILD ListTok ::= <alt>, for all alts. *) 
          END (* IF *)  

        ; IF TSemDeclAsList . ListCardUsed [ ListCardPlural ] 
          THEN 
            INC ( LangInfo ^ . GenProductions , 4 + 3 * LAltCt ) 
            (* See P4fsrListRule for the productions to be generated. *) 
          ; IF FsRuleNodeRef . FsFormatsEmpty = UbTrue 
               (* ^This means the element, with per-element insertion tokens 
                   formats empty> *) 
               AND NOT FsRuleNodeRef . FsEstListHasSeps
            THEN  
              WarnListAmbiguity 
                ( LangInfo . GenGram , FsRuleNodeRef , "No-separator list" ) 
            END (* IF *) 
          END (* IF *)  

        ELSE (* Not analyzed as a list rule. *)
        END (* TYPECASE *) 
      END P1fsrListRule 

  ; BEGIN (* Pass1FsRule *)
      IF FsRuleNodeRef # NIL 
      THEN 
        P1fsrChildNo := 0 
      ; CASE FsRuleNodeRef . FsKind 
        OF FsKindTyp . FsKindEstFixedHoriz  
        , FsKindTyp . FsKindEstFixedVert  
        , FsKindTyp . FsKindEstFixedFill  
        => P1fsrFixedRule ( FsRuleNodeRef )

        | FsKindTyp . FsKindEstListHoriz  
        , FsKindTyp . FsKindEstListVert  
        , FsKindTyp . FsKindEstListFill   
        , FsKindTyp . FsKindEstListTrailHoriz  
        , FsKindTyp . FsKindEstListTrailVert  
        , FsKindTyp . FsKindEstListTrailFill   
        => P1fsrListRule ( FsRuleNodeRef )

        ELSE 
        END (* CASE *) 
      END (* IF *) 
    END Pass1FsRule  

; PROCEDURE BetweenPass2And3
    ( LangInfo : LangInfoRefTyp ; Gram : GrammarSubTyp ) 
  RAISES { Backout } 
  (* Between pass 2 and pass 3:
       1) Expand the token map to hold generated CS tokens.
       2) Initialize the LR automaton.
  *) 
    
  = BEGIN 
      LdlSemantics . ExpandTokMap 
        ( LangInfo . TokMapRef 
        , LangInfo . GcsChildToks - TokMapBias 
        ) 
(* 
    ; LdlSemantics . ExpandFsTreeMap 
        ( LangInfo . FsTreeMapRef 
        , LangInfo . GcsChildToks - FsTreeMapBias 
        ) 
*) 
    ; Automaton . InitAutomaton 
        ( Gram := Gram 
        , ProdCt := LangInfo . GenProductions + GPossibleTokstringAtomTokCt  
        , FirstTerminal := LbeStd . Tok__BegOfImage 
        , LastTerminal := LangInfo . VarTermToks - 1 
        , FirstAstNonterminal := LangInfo . VarTermModToks 
        , LastAstNonterminal := LangInfo . AsListCardToks - 1 
        , FirstNonterminal := LangInfo . AsListCardToks   
(* TODO: Rework all the LR stuff so it doesn't try to work on
       tokens in the range AsPartialToks .. AsCsClassToks - 1, 
       (which are ClassOnly tokens), so we can take advantage of the fact 
       that they aren't used in the concrete syntax, and shorten the
       table NNextRef on the left.
*) 
        , LastNonterminal 
            := LangInfo . GcsChildToks - 1 
               + GPossibleTokstringAtomTokCt 
        , StartSymbol := LangInfo . StartTok 
        ) 
    ; LdlSemantics . NoteStandardNontermKinds ( LangInfo , Gram ) 
    ; LdlSemantics . NoteNontermKindForRange 
        ( Gram 
        , FromTok := LangInfo . VarTermModToks 
        , ToTok := LangInfo . AsStarToks 
        , Kind := NontermKindTyp . NtkAbstractOnly 
        ) 
    ; LdlSemantics . NoteNontermKindForRange 
        ( Gram 
        , FromTok := LangInfo . AsSublistToks 
        , ToTok := LangInfo . AsListCardToks 
        , Kind := NontermKindTyp . NtkConcreteOnly   
        ) 
    ; LdlSemantics . NoteNontermKindForRange 
        ( Gram 
        , FromTok := LangInfo . AsListCardToks 
        , ToTok := LangInfo . AsPartialToks 
        , Kind := NontermKindTyp . NtkPartial 
        ) 
    ; LdlSemantics . NoteNontermKindForRange 
        ( Gram 
        , FromTok := LangInfo . AsClassOnlyToks 
        , ToTok := LangInfo . CsFixedToks 
        , Kind := NontermKindTyp . NtkUnused 
        ) 

    (* PredMarkTok ::= <empty> *) 
    ; IF DoInsertPredMarkTok
      THEN 
        Automaton . AddExternalProduction 
          ( Gram , Left := PredMarkTok , Right := NIL ) 
      END (* IF *) 

    ; GenProdsForDeclaredClass ( LangInfo , LangInfo . StartTok )
      (* Start symbol only.  Other classes, declared and internally
         generated are handled by GenProdsForUsedClasses. *) 

    END BetweenPass2And3

; PROCEDURE GenProdsForDeclaredClass 
    ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  RAISES { Backout } 
  (* If Tok is an ldl-declared class, generate productions for its members 
     and also note it is a concrete only token.
  *) 

  = BEGIN 
      TYPECASE LdlSemantics . SemDeclOfTok ( LangInfo , Tok ) 
      OF NULL => (* A previously reported error. *)
      | LdlSemantics . SemFirstOccClassTyp ( TSemClassDecl ) 
      => LdlSemantics . GenNonclassTokSetProductions 
          ( LangInfo 
          , LangInfo ^ . GenGram
          , Tok 
          , TSemClassDecl . TokSet 
          , ReplaceListCardToks := TRUE  
          ) 
      ; Automaton . NoteNontermKind 
          ( LangInfo ^ . GenGram , Tok , NontermKindTyp . NtkConcreteOnly ) 
      ELSE 
      END (* TYPECASE *) 
    END GenProdsForDeclaredClass 

; PROCEDURE SingletonOptionIdSet ( OptionId : LRTable . OptionIdTyp )
  : LRTable . OptionIdSetTyp 
  (* Or empty set, if OptionId = OptionIdNull. *) 

  = BEGIN 
      IF OptionId =LRTable . OptionIdNull  
      THEN 
        RETURN LRTable . OptionIdSetEmpty 
      ELSE 
        RETURN LRTable . OptionIdSetTyp { OptionId } 
      END (* IF *) 
    END SingletonOptionIdSet 

; VAR DoInsertPredMarkTok : BOOLEAN := FALSE  

; VAR PredMarkTok : LbeStd . TokTyp := LbeStd . Tok__Null

; PROCEDURE Pass4FsRule 
    ( LangInfo : LangInfoRefTyp 
    ; Gram : GrammarSubTyp 
    ; FsRuleNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  RAISES { Backout } 
  (* Generate Concrete syntax rules from one FS rule. *) 
  (* This is unnested from Generate, just to keep things from getting 
     too deep. 
  *) 

  = VAR P4fsrCondResultRef : FragSetRefTyp := NIL  
  ; VAR P4fsrCondResultSs : CARDINAL 
  ; VAR P4fsrCondMaxFragLen : CARDINAL 

  ; PROCEDURE P4fsrFillFrag 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
        (* ^An FsNode that has exactly one principal EstChild descendent. *) 
      ; FragString : LRTable . TokArrayRefTyp 
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      ) 
    RAISES { Backout } 
    (* Fills exactly one fragment in P4fsrCondResultRef. *) 

    = VAR LFragTok : LbeStd . TokTyp 

    ; BEGIN 
        WITH WFrag = P4fsrCondResultRef ^ [ P4fsrCondResultSs ] 
        DO 
          IF FragString = NIL OR NUMBER ( FragString ^ ) = 0 
          THEN WFrag . TokStringRef := FragString 
          ELSIF GDoFactorSubstrings 
          THEN 
            IF NUMBER ( FragString ^ ) = 1 
            THEN 
              WFrag . TokStringRef := FragString 
            ELSE 
              LFragTok 
                := LdlSemantics . TokForAtomTokString 
                     ( LangInfo 
                     , FsNodeRef 
                     , FragString 
                     , UseCountIncrement := 1 
                     ) 
            ; WFrag . TokStringRef 
                := LRTable . SingletonString ( LFragTok ) 
            END (* IF *) 
          ELSE 
            WFrag . TokStringRef := FragString 
          END (* IF *) 
        ; IF WFrag . TokStringRef # NIL 
          THEN 
            P4fsrCondMaxFragLen 
              := MAX ( P4fsrCondMaxFragLen 
                     , NUMBER ( WFrag . TokStringRef ^ ) 
                     )
          END (* IF *) 
        ; WFrag . OptionIdSet := OptionIdSet 
        END (* WITH *)         
      ; INC ( P4fsrCondResultSs ) 
      END P4fsrFillFrag 

  ; PROCEDURE P4fsrFillAltFrag 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
        (* ^An FsNode that has exactly one principal EstChild descendent. *) 
      ; PrincipalChildTok : LbeStd . TokTyp  
        (* ^LbeStd.Tok__Null means omit principal child from the fragment. *)
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      ) 
    RAISES { Backout } 
    (* Fills exactly one fragment in P4fsrCondResultRef. *) 

    = VAR LAltFrag : LRTable . TokArrayRefTyp

    ; BEGIN 
        LAltFrag := AltFrag ( FsNodeRef , PrincipalChildTok ) 
      ; P4fsrFillFrag ( FsNodeRef , LAltFrag , OptionIdSet ) 
      END P4fsrFillAltFrag 

  ; PROCEDURE P4fsrFillTokSetFrags 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
        (* ^An FsNode that has exactly one EstChild descendent. *) 
      ; TokSet : TokSetTyp  
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      ) 
    RAISES { Backout } 
    (* Fill fragments formatted by FsNodeRef, but with each member of TokSet
       replacing the one principal child. *) 
    (* Checks FsCondDoParse. *) 

    = PROCEDURE P4fsrFtsfVisit ( PrincipalChildTok : IntSets . ValidElemT ) 
      RAISES { Backout } 

      = BEGIN 
          P4fsrFillAltFrag ( FsNodeRef , PrincipalChildTok , OptionIdSet ) 
        END P4fsrFtsfVisit

    ; <* FATAL ANY *> 
      BEGIN (* P4fsrFillTokSetFrags *) 
        IF FsNodeRef . FsCondDoParse 
        THEN 
          IntSets . ForAllDo ( TokSet , P4fsrFtsfVisit ) 
        END (* IF *) 
      END P4fsrFillTokSetFrags 

  ; PROCEDURE P4fsrFillEmptyListFrag
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; ListTok : LbeStd . TokTyp  
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      ) 
    RAISES { Backout } 
    (* Fills one fragment, with an empty list. *) 
    (* Checks FsCondDoParse. *) 

    = VAR LEmptyListTok : LbeStd . TokTyp 

    ; BEGIN 
        IF FsNodeRef . FsCondDoParse 
        THEN 
          LEmptyListTok 
            := LdlSemantics . ListCardTok ( LangInfo , ListTok , ListCardEmpty )
        ; P4fsrFillAltFrag 
            ( FsNodeRef 
            , PrincipalChildTok := LEmptyListTok 
            , OptionIdSet := OptionIdSet 
            ) 
        END (* IF *) 
      END P4fsrFillEmptyListFrag
 
  ; PROCEDURE P4fsrFillSingletonFrags 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
        (* ^An FsNode that has exactly one EstChild descendent. *) 
      ; ListToksSubset : TokSetTyp 
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      ) 
    RAISES { Backout }
    (* PRE:  P4fsrCondResultRef ^ has enough space, starting at 
             P4fsrCondResultSs, for IntSets . Card ( ListToksSubset ) fragments.
       POST: P4fsrCondResultRef ^ has been filled with the fragments formatted 
             by FsNodeRef, with principal children being the Singleton list 
             tokens for each list node in ListToksSubset.  P4fsrCondResultSs 
             is updated accordingly.
    *) 
    (* Checks FsCondDoParse. *) 

    = PROCEDURE P4fsrFsfVisit ( ListTok : IntSets . ValidElemT ) 
      RAISES { Backout } 

      = VAR LSingletonTok : LbeStd . TokTyp 

      ; BEGIN 
          LSingletonTok 
            := LdlSemantics . ListCardTok 
                 ( LangInfo , ListTok , ListCardSingleton )
        ; P4fsrFillAltFrag 
            ( FsNodeRef 
            , PrincipalChildTok := LSingletonTok 
            , OptionIdSet := OptionIdSet 
            ) 
        END P4fsrFsfVisit

    ; BEGIN (* P4fsrFillSingletonFrags  *) 
        IF FsNodeRef . FsCondDoParse 
        THEN 
          <* FATAL ANY *> 
          BEGIN 
            IntSets . ForAllDo ( ListToksSubset , P4fsrFsfVisit ) 
          END (* Block. *) 
        END (* IF *) 
      END P4fsrFillSingletonFrags  

  ; PROCEDURE P4fsrFillPluralFrags 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
        (* ^An FsNode that has a unique EstChild descendent. *) 
      ; ListToksSubset : TokSetTyp 
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      ) 
    RAISES { Backout }
    (* PRE:  P4fsrCondResultRef ^ has enough space, starting at 
             P4fsrCondResultSs, for IntSets . Card ( ListToksSubset ) fragments.
       POST: P4fsrCondResultRef ^ has been filled with the fragments formatted 
             by FsNodeRef, with principal children being the Plural list 
             tokens of each list node in ListToksSubset.  P4fsrCondResultSs 
             is updated accordingly.
    *) 
    (* Checks FsCondDoParse. *) 

    = PROCEDURE P4fsrFpfVisit ( ListTok : IntSets . ValidElemT ) 
      RAISES { Backout } 

      = VAR LPluralTok : LbeStd . TokTyp 

      ; BEGIN 
          LPluralTok 
            := LdlSemantics . ListCardTok 
                 ( LangInfo , ListTok , ListCardPlural )
        ; P4fsrFillAltFrag 
            ( FsNodeRef 
            , PrincipalChildTok := LPluralTok 
            , OptionIdSet := OptionIdSet 
            ) 
        END P4fsrFpfVisit

    ; BEGIN (* P4fsrFillPluralFrags *)
        IF FsNodeRef . FsCondDoParse 
        THEN 
          <* FATAL ANY *> 
          BEGIN  
            IntSets . ForAllDo ( ListToksSubset , P4fsrFpfVisit ) 
          END (* Block. *) 
        END (* IF *) 
      END P4fsrFillPluralFrags  

  ; PROCEDURE P4fsrIncludeListCardAltToks  
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
        (* ^An FsNode that has exactly one EstChild descendant. *) 
      ; ListChildTokSet : TokSetTyp 
      ; ListCard : LdlSemantics . ListCardTyp 
      ; VAR (* IN OUT *) FragTokSet : TokSetTyp 
      ) 
    RAISES { Backout }
    (* Include into FragTokSet, a token for each string formatted by FsNodeRef,
       with the list cardinality token for cardinality ListCard of a member
       of ListChildTokSet substituted for its one Ast child.   
    *) 
    (* Checks FsCondDoParse. *) 

    = PROCEDURE P4fsrIlftVisit ( ListTok : IntSets . ValidElemT ) 
      RAISES { Backout } 

      = VAR LListCardTok : LbeStd . TokTyp 
            (* Tok for a list with cardinality ListCard and element ListTok. *)
      ; VAR LAltFrag : LRTable . TokArrayRefTyp 
            (* ^Insertion toks of the alternative, surrounding LListCardTok. *) 
      ; VAR LAltTok : LbeStd . TokTyp (* Atom token for LAltFrag. *)  

      ; BEGIN 
          LListCardTok 
            := LdlSemantics . ListCardTok ( LangInfo , ListTok , ListCard )
        ; LAltFrag := AltFrag ( FsNodeRef , PrincipalChildTok := LListCardTok ) 
        ; IF LRTable . IsSingletonStringOf ( LAltFrag , ListTok ) 
          THEN (* This is unlikely, but maybe someday, a list-cardinality-set
                  optimization could make it happen. *)  
            LAltTok := ListTok 
          ELSE 
            LAltTok 
              := LdlSemantics . TokForAtomTokString 
                   ( LangInfo 
                   , FsNodeRef 
                   , LAltFrag
                   , UseCountIncrement := 1 
                   ) 
          END (* IF *) 
        ; FragTokSet := IntSets . Include ( FragTokSet , LAltTok ) 
        END P4fsrIlftVisit

    ; BEGIN (* P4fsrIncludeListCardAltToks *) 
        IF FsNodeRef . FsCondDoParse 
(* CHECK:  ^Is this correctly set for nonpredicate subtree? *) 
        THEN 
          <* FATAL ANY *> 
          BEGIN 
            IntSets . ForAllDo ( ListChildTokSet , P4fsrIlftVisit ) 
          END (* Block. *) 
        END (* IF *) 
      END P4fsrIncludeListCardAltToks  

  ; PROCEDURE P4fsrIncludeFixedAltToks  
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
        (* ^An FsNode that has exactly one EstChild descendent. *) 
      ; FixedChildTokSet : TokSetTyp 
      ; VAR (* IN OUT *) FragTokSet : TokSetTyp 
      ) 
    RAISES { Backout }
    (* Include into FragTokSet, a token for each string formatted by FsNodeRef,
       with a member of FixedChildTokSet substituted for its one Ast child.
    *) 
    (* Checks FsCondDoParse. *) 

    = PROCEDURE P4fsrIfatVisit ( FixedTok : IntSets . ValidElemT ) 
      RAISES { Backout } 

      = VAR LAltFrag : LRTable . TokArrayRefTyp 
            (* ^Insertion toks of the alternative, surrounding FixedTok. *) 
      ; VAR LAltTok : LbeStd . TokTyp (* Atom token for LAltFrag. *)  

      ; BEGIN 
          LAltFrag := AltFrag ( FsNodeRef , PrincipalChildTok := FixedTok ) 
        ; LAltTok 
            := LdlSemantics . TokForAtomTokString 
                 ( LangInfo 
                 , FsNodeRef 
                 , LAltFrag
                 , UseCountIncrement := 1 
                 ) 
        ; FragTokSet := IntSets . Include ( FragTokSet , LAltTok ) 
        END P4fsrIfatVisit

    ; BEGIN (* P4fsrIncludeFixedAltToks *) 
        IF FsNodeRef . FsCondDoParse 
        THEN 
          IF LRTable . IsSingletonStringOf 
               ( AltFrag ( FsNodeRef , LbeStd . Tok__Unknown ) 
               , LbeStd . Tok__Unknown 
               )
          THEN (* Strings will each be singleton, member of FixedChildTokSet. *)
            FragTokSet := IntSets . Union ( FragTokSet , FixedChildTokSet ) 
          ELSE 
            <* FATAL ANY *> 
            BEGIN 
              IntSets . ForAllDo ( FixedChildTokSet , P4fsrIfatVisit ) 
            END (* Block. *) 
          END (* IF *) 
        END (* IF *) 
      END P4fsrIncludeFixedAltToks  

   ; PROCEDURE P4fsrRemainingCases 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR (* IN OUT *) AltTokSet : TokSetTyp 
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      )
    RAISES { Backout } 
    (* PRE: FsNodeRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 
    (* For every case that satisfies both FsAltCardSet and the FsAlt*TokSet
       fields, either fill P4fsrCondResultRef ^ with its alternative string, 
       or include into AltTokSet, a token that will derive its alternative 
       string. 
    *) 

    = VAR LListCard : CARDINAL 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LFixedTokSetCard : CARDINAL 
    ; VAR LClassTok  : LbeStd . TokTyp 

    ; BEGIN (* P4fsrRemainingCases *) 
        LListCard := IntSets . Card ( FsNodeRef . FsAltListTokSet ) 
      ; IF LListCard > 0 
        THEN 
          LStarListCard := IntSets . Card ( FsNodeRef . FsAltStarListTokSet ) 
        ELSE 
          LStarListCard := 0 
        END (* IF *) 
      ; LFixedTokSetCard := IntSets . Card ( FsNodeRef . FsAltFixedTokSet ) 

      (* Absent. *) 
      ; IF FsNodeRef . FsCondDoParse 
           AND CardTyp . Absent IN FsNodeRef . FsAltCardSet 
           AND LangUtil . FsEstChildRef ( FsNodeRef ) . FsEstChildOpt 
               IN LangUtil . ChildOptSetOptByAS 
        THEN 
          P4fsrFillAltFrag 
            ( FsNodeRef 
            , PrincipalChildTok := LbeStd . Tok__Null 
            , OptionIdSet := LRTable . OptionIdSetEmpty 
            ) 
          (* ^For absent, always flatten into the Est-building production. *) 

      (* Empty lists. *) 
        ELSIF LStarListCard > 0 
              AND CardTyp . EmptyList IN FsNodeRef . FsAltCardSet 
        THEN
          IF GDoFactorAlts 
          THEN 
            P4fsrIncludeListCardAltToks 
              ( FsNodeRef 
              , FsNodeRef . FsAltStarListTokSet 
              , ListCardEmpty 
              , (* IN OUT *) AltTokSet 
              ) 
          ELSE 
            P4fsrFillEmptyListFrag 
              ( FsNodeRef 
              , ListTok := IntSets . Minimum ( FsNodeRef . FsAltStarListTokSet )
              , OptionIdSet := OptionIdSet 
              ) 
(* CHECK: Don't we now do this for all members of 
          FsNodeRef . FsAltStarListTokSet, even if there are more than one?  *) 
          END (* IF *) 
        END (* IF *) 

      (* Singleton and plural lists. *) 
      ; IF LListCard > 0 
        THEN 
          IF CardTyp . SingletonList IN FsNodeRef . FsAltCardSet 
          THEN
            IF GDoFactorAlts 
            THEN 
              P4fsrIncludeListCardAltToks 
                ( FsNodeRef 
                , FsNodeRef . FsAltListTokSet 
                , ListCardSingleton 
                , (* IN OUT *) AltTokSet 
                ) 
            ELSE 
              P4fsrFillSingletonFrags 
                ( FsNodeRef 
                , FsNodeRef . FsAltListTokSet 
                , OptionIdSet := OptionIdSet 
                ) 
            END (* IF *) 
          END (* IF *) 
        ; IF CardTyp . PluralList IN FsNodeRef . FsAltCardSet 
          THEN
            IF GDoFactorAlts 
            THEN 
              P4fsrIncludeListCardAltToks 
                ( FsNodeRef 
                , FsNodeRef . FsAltListTokSet 
                , ListCardPlural 
                , (* IN OUT *) AltTokSet 
                ) 
            ELSE 
              P4fsrFillPluralFrags 
                ( FsNodeRef 
                , FsNodeRef . FsAltListTokSet 
                , OptionIdSet := OptionIdSet 
                ) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 

      (* Fixed nodes. *) 
      ; IF FsNodeRef . FsCondDoParse AND LFixedTokSetCard > 0  
        THEN 
          IF GDoFactorAlts  
          THEN 
            P4fsrIncludeFixedAltToks
              ( FsNodeRef 
              , FsNodeRef . FsAltFixedTokSet , (* IN OUT *) AltTokSet 
              ) 
          ELSIF GDoFactorClasses 
          THEN 
            IF FsNodeRef . FsCondDoParse 
            THEN 
              LClassTok 
                := LdlSemantics . TokForAtomTokSet (* SIDE EFFECTS! *) 
                     ( LangInfo 
                     , FsNodeRef 
                     , FsNodeRef . FsAltFixedTokSet 
                     , MustBePresent := TRUE 
                     )
  (* TODO: This token could be stored in FsNodeRef . FsCondPredicate . 
         PredicateClass.  Do so and then use it from there.  This would probably
         also allow the MustBePresent case to be eliminated and maybe side
         effects simplified. 
  *) 
            ; Assert 
                ( LClassTok # LbeStd . Tok__Null  
                , AFT . A_GrammarGen_P4fsrRemainingCases_Class_atom_token_not_in_table
                )  
            ; P4fsrFillAltFrag 
                ( FsNodeRef , LClassTok , OptionIdSet := OptionIdSet )
            END (* IF *) 
          ELSE 
            P4fsrFillTokSetFrags 
              ( FsNodeRef , FsNodeRef . FsAltFixedTokSet , OptionIdSet ) 
          END (* IF *) 
        END (* IF *) 
      END P4fsrRemainingCases  

  ; PROCEDURE P4fsrPredicate 
      ( FsPredNodeRef : LangUtil . FsNodeRefTyp 
        (* An FsNode with exactly one Est child descendent. *) 
      ; VAR (* IN OUT *) AltTokSet : TokSetTyp 
      ) 
    RAISES { Backout } 
    (* For each alternative that FsPredNodeRef applies to, either fill 
       P4fsrCondResultRef ^ with its alternative string, or include into 
       AltTokSet, a token that will derive its alternative string. 
    *) 
    (* PRE:  P4fsrCondResultRef ^ has enough space, starting at 
             P4fsrCondResultSs, to hold the fragments of the alternative. 
    *) 

    = VAR LOrigResultSs : CARDINAL 
    ; VAR LListCard : CARDINAL 
    ; VAR LListTok : LbeStd . TokTyp 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LOptionIdSet : LRTable . OptionIdSetTyp 

    ; BEGIN (* P4fsrPredicate *) 
        IF FsPredNodeRef = NIL 
        THEN 
(* REVIEW: Can this happen? *)
          CantHappen ( AFT . A_P4fsrPredicate_NIL_Alternative )  
        ELSE 
          LOrigResultSs := P4fsrCondResultSs 
        ; CASE FsPredNodeRef . FsKind <* NOWARN *> 
          OF FsKindTyp . FsKindCondFmt 
          => CASE <* NOWARN *> FsPredNodeRef . FsCondPredicate . PredicateKind  
            OF PredicateKindTyp . PredicateKindNull 
            , PredicateKindTyp . PredicateKindFalse 
            => CantHappen 
                 ( AFT . A_GrammarGen_P4fsrPredicate_Leftover_null_or_false_predicate ) 

            (* Explicit ELSE *) 
            | PredicateKindTyp . PredicateKindTrue
            => P4fsrRemainingCases  
                 ( FsPredNodeRef 
                 , (* IN OUT *) AltTokSet 
                 , SingletonOptionIdSet ( FsPredNodeRef . FsOptionIds [ TRUE ] )
(* REVIEW: Generation of FsOptionIds. *) 
                 ) 

            (* ABSENT *)     
            | PredicateKindTyp . PredicateKindAbsent 
            => IF FsPredNodeRef . FsCondDoParse 
              THEN 
              (* We fill the empty alternative, regardless of GDoFactorAlts. *)
                P4fsrFillAltFrag 
                  ( FsPredNodeRef 
                  , PrincipalChildTok := LbeStd . Tok__Null 
                  , OptionIdSet := LRTable . OptionIdSetEmpty 
                  )
              END (* IF *)  

            (* PRESENT *) 
            | PredicateKindTyp . PredicateKindPresent 
            => P4fsrRemainingCases 
                 ( FsPredNodeRef 
                 , (* IN OUT *) AltTokSet 
                 , SingletonOptionIdSet ( FsPredNodeRef . FsOptionIds [ TRUE ] )
                 )  

            (* EMPTY *) 
            | PredicateKindTyp . PredicateKindEmptyList 
            => LStarListCard 
                := IntSets . Card ( FsPredNodeRef . FsAltStarListTokSet ) 
            ; IF GDoFactorAlts 
              THEN  
                P4fsrIncludeListCardAltToks 
                  ( FsPredNodeRef 
                  , FsPredNodeRef . FsAltStarListTokSet 
                  , ListCardEmpty 
                  , (* IN OUT *) AltTokSet 
                  ) 
              ELSE 
                LListTok 
                  := IntSets . Minimum ( FsPredNodeRef . FsAltStarListTokSet ) 
                (* ^If LStarListCard > 1, it's ambiguous and was warned-about
                    in pass 1. Just arbitrarily choose one list, but it must
                    be the same one as chosen in pass 1. 
                *) 
(* CHECK: Don't we do them all? *) 
              ; P4fsrFillEmptyListFrag 
                  ( FsPredNodeRef 
                  , LListTok 
                  , SingletonOptionIdSet 
                      ( FsPredNodeRef . FsOptionIds [ TRUE ] )
                  ) 
              END (* IF *) 

            (* NONEMPTY *) 
            | PredicateKindTyp . PredicateKindNonemptyList
            => LListCard := IntSets . Card ( FsPredNodeRef . FsAltListTokSet ) 
            ; IF GDoFactorAlts 
              THEN  
                IF CardTyp . SingletonList IN FsPredNodeRef . FsAltCardSet 
                THEN  
                  P4fsrIncludeListCardAltToks 
                    ( FsPredNodeRef 
                    , FsPredNodeRef . FsAltListTokSet 
                    , ListCardSingleton  
                    , (* IN OUT *) AltTokSet 
                    ) 
                END (* IF *) 
              ; IF CardTyp . PluralList IN FsPredNodeRef . FsAltCardSet 
                THEN  
                  P4fsrIncludeListCardAltToks 
                    ( FsPredNodeRef 
                    , FsPredNodeRef . FsAltListTokSet 
                    , ListCardPlural
                    , (* IN OUT *) AltTokSet 
                    ) 
                END (* IF *) 
              ELSE (* Don't factor alternatives. *) 
                LOptionIdSet 
                  := SingletonOptionIdSet 
                       ( FsPredNodeRef . FsOptionIds [ TRUE ] )
              ; IF CardTyp . SingletonList IN FsPredNodeRef . FsAltCardSet 
                THEN  
                  P4fsrFillSingletonFrags 
                    ( FsPredNodeRef 
                    , FsPredNodeRef . FsAltListTokSet , LOptionIdSet 
                    ) 
                END (* IF *) 
              ; IF CardTyp . PluralList IN FsPredNodeRef . FsAltCardSet 
                THEN  
                  P4fsrFillPluralFrags 
                    ( FsPredNodeRef 
                    , FsPredNodeRef . FsAltListTokSet , LOptionIdSet 
                    ) 
                END (* IF *) 
              END (* IF *) 

            (* NONPLURAL *) 
            | PredicateKindTyp . PredicateKindNonpluralList 
            => LListCard := IntSets . Card ( FsPredNodeRef . FsAltListTokSet ) 
            ; LStarListCard 
                := IntSets . Card ( FsPredNodeRef . FsAltStarListTokSet ) 
            ; IF GDoFactorAlts 
              THEN 
                IF LStarListCard > 0  
                   AND CardTyp . EmptyList IN FsPredNodeRef . FsAltCardSet 
                THEN 
                  P4fsrIncludeListCardAltToks 
                    ( FsPredNodeRef 
                    , FsPredNodeRef . FsAltStarListTokSet 
                    , ListCardEmpty 
                    , (* IN OUT *) AltTokSet 
                    ) 
                END (* IF *) 
              ; IF CardTyp . SingletonList IN FsPredNodeRef . FsAltCardSet 
                THEN
                  P4fsrIncludeListCardAltToks 
                    ( FsPredNodeRef 
                    , FsPredNodeRef . FsAltListTokSet 
                    , ListCardSingleton  
                    , (* IN OUT *) AltTokSet 
                    ) 
                END (* IF *) 
              ELSE (* Don't factor alternatives. *) 
                LOptionIdSet 
                  := SingletonOptionIdSet 
                       ( FsPredNodeRef . FsOptionIds [ TRUE ] ) 
              ; IF LStarListCard > 0  
                   AND CardTyp . EmptyList IN FsPredNodeRef . FsAltCardSet 
                THEN 
                  LListTok 
                    := IntSets . Minimum ( FsPredNodeRef . FsAltStarListTokSet )
(* CHECK: Don't we now do all of these? *) 
                ; P4fsrFillEmptyListFrag 
                    ( FsPredNodeRef , LListTok , LOptionIdSet ) 
                END (* IF *) 
              ; IF CardTyp . SingletonList IN FsPredNodeRef . FsAltCardSet 
                THEN
                  P4fsrFillSingletonFrags 
                    ( FsPredNodeRef 
                    , FsPredNodeRef . FsAltListTokSet 
                    , LOptionIdSet 
                    ) 
                END (* IF *) 
              END (* IF *) 

            (* PLURAL *) 
            | PredicateKindTyp . PredicateKindPluralList 
            => LListCard := IntSets . Card ( FsPredNodeRef . FsAltListTokSet ) 
            ; IF GDoFactorAlts 
              THEN 
                P4fsrIncludeListCardAltToks 
                  ( FsPredNodeRef 
                  , FsPredNodeRef . FsAltListTokSet 
                  , ListCardPlural
                  , (* IN OUT *) AltTokSet 
                  ) 
              ELSE 
                P4fsrFillPluralFrags 
                  ( FsPredNodeRef 
                  , FsPredNodeRef . FsAltListTokSet 
                  , SingletonOptionIdSet 
                      ( FsPredNodeRef . FsOptionIds [ TRUE ] ) 
                  ) 
              END (* IF *) 

            (* MEMBER *) 
            | PredicateKindTyp . PredicateKindInClass 
            => P4fsrRemainingCases 
                ( FsPredNodeRef 
                , (* IN OUT *) AltTokSet 
                , SingletonOptionIdSet 
                    ( FsPredNodeRef . FsOptionIds [ TRUE ] ) 
                )  

            END (* CASE PredicateKind *) 

          (* Unconditional alternative. *) 
          | FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill
          , FsKindTyp . FsKindEstChildOfFixed
          , FsKindTyp . FsKindEstChildOfList 
          (* These are unconditional alternatives. *) 
          => P4fsrRemainingCases  
               ( FsPredNodeRef 
               , (* IN OUT *) AltTokSet 
               , SingletonOptionIdSet ( FsPredNodeRef . FsOptionIds [ TRUE ] )
               ) 

          | FsKindTyp . FsKindNull 
          , FsKindTyp . FsKindBegOfImage
          , FsKindTyp . FsKindEndOfImage 
          , FsKindTyp . FsKindLineBreakReqd
          , FsKindTyp . FsKindLineBreakOpt
          => CantHappen ( AFT . A_P4fsrPredicate__Bad_FsNode ) 

          END (* CASE FsKind *)  
        ; Assert 
            ( TRUE 
              OR P4fsrCondResultSs - LOrigResultSs 
                 = FsPredNodeRef . FsAltFragCt 
            , AFT . A_P4fsrPredicate_Result_count_mismatch 
            ) 
        END (* IF *) 
      END P4fsrPredicate 

  ; PROCEDURE P4fsrCondConstruct 
      ( FsCondFmtNodeRef : LangUtil . FsNodeRefTyp 
        (* The first predicate FsNode of an entire conditional construct. *) 
      ; VAR MaxFragLen : CARDINAL 
      ) 
    : FragSetRefTyp 
    RAISES { Backout } 
    (* Handle a complete conditional construct. *) 

(* TODO ^This probably also works for a principal child not inside any 
        conditional, but we use a separate procedure for that.  Resolve. *) 

    = VAR LEstChildFsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR LPredFsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR LFragTokSet : TokSetTyp 
    ; VAR LPresentTok : LbeStd . TokTyp 

    ; BEGIN (* P4fsrCondConstruct *) 
        LEstChildFsNodeRef := LangUtil . FsEstChildRef ( FsCondFmtNodeRef )  

      (* Count total fragments and allocate a result array for the
         conditional construct. 
      *) 
      ; P4fsrCondResultRef 
          := NEW ( FragSetRefTyp 
                 
, ( FragCountForCondConstruct ( LangInfo , FsCondFmtNodeRef ) + 2 ) * 2 
(* FIXME^ Get this count right. *) 
                 ) 
      ; P4fsrCondResultSs := 0 
      ; P4fsrCondMaxFragLen := 0
      ; LFragTokSet := IntSets . Empty ( )  

      (* Traverse the alternatives. *) 
      ; LPredFsNodeRef := FsCondFmtNodeRef 
      ; WHILE LPredFsNodeRef # NIL 
        DO
          P4fsrPredicate ( LPredFsNodeRef , (* IN OUT *) LFragTokSet )
        ; LPredFsNodeRef := LPredFsNodeRef . FsCondAltRef 
        END (* WHILE *) 

      (* Fill a single Atom token for the group of present alternatives. *) 
      ; IF NOT IntSets . IsEmpty ( LFragTokSet ) 
        THEN 
          LPresentTok 
            := LdlSemantics . TokForAtomTokSet 
                 ( LangInfo 
                 , FsCondFmtNodeRef 
                 , LFragTokSet  
                 , UseCountIncrement := 1 
                 )  
        ; P4fsrFillFrag 
            ( LPredFsNodeRef 
            , LRTable . SingletonString ( LPresentTok ) 
            , SingletonOptionIdSet ( FsCondFmtNodeRef . FsOptionIds [ FALSE ] )
            ) 
        END (* IF *) 

; IF P4fsrCondResultRef # NIL 
  THEN
    VAR LN : CARDINAL := NUMBER ( P4fsrCondResultRef ^ ) 
  ; VAR LNew : FragSetRefTyp 
  ; BEGIN 
      IF P4fsrCondResultSs < LN 
      THEN 
        LNew := NEW ( FragSetRefTyp , P4fsrCondResultSs ) 
      ; LNew ^ := SUBARRAY ( P4fsrCondResultRef ^ , 0 , P4fsrCondResultSs ) 
      ; P4fsrCondResultRef := LNew 
      END 
    END 
  END 
(* FIXME^: Get the count right and eliminate this cruft. *) 
      ; Assert
( TRUE OR P4fsrCondResultSs = NUMBER ( P4fsrCondResultRef ^ )  
          , AFT . A_P4fsrCondConstruct_underfull
          )  
      ; MaxFragLen := P4fsrCondMaxFragLen 
      ; RETURN P4fsrCondResultRef 
      END P4fsrCondConstruct 

  ; PROCEDURE P4fsrUncondPrincipalChild 
      ( FsChildNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR MaxFragLen : CARDINAL 
      )
    : FragSetRefTyp  
    RAISES { Backout } 
    (* Used only for a principal child that is not inside a conditional
       construct.  It could optional though (a different kind of conditional) 
       and still have multiple fragments.
    *) 

    = VAR LResultCt : CARDINAL 
    ; VAR LFixedTokSetCard : CARDINAL 
    ; VAR LListCard : CARDINAL 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LTok : LbeStd . TokTyp 
    ; VAR LOptionIdSet : LRTable . OptionIdSetTyp 

    ; BEGIN
        LResultCt := 0  
      ; INC ( LResultCt 
            , ORD ( FsChildNodeRef . FsEstChildOpt 
                    IN LangUtil . ChildOptSetOptByAS 
                  ) 
            ) 
      ; LListCard := IntSets . Card ( FsChildNodeRef . FsAltListTokSet ) 
      ; IF LListCard > 0 
        THEN 
          LStarListCard 
            := IntSets . Card ( FsChildNodeRef . FsAltStarListTokSet ) 
        ; INC ( LResultCt , ORD ( LStarListCard > 0 ) ) 
        ; INC ( LResultCt , LListCard * 2 ) (* Singleton and Plural cases. *) 
        ELSE 
          LStarListCard := 0 
        END (* IF *) 
      ; LFixedTokSetCard 
          := IntSets . Card ( FsChildNodeRef . FsAltFixedTokSet ) 
      ; IF GDoFactorClasses 
        THEN INC ( LResultCt , ORD ( LFixedTokSetCard > 0 ) ) 
        ELSE INC ( LResultCt , LFixedTokSetCard ) 
        END (* IF *) 

      ; P4fsrCondResultRef := NEW ( FragSetRefTyp , LResultCt  ) 
      ; P4fsrCondResultSs := 0 
      ; P4fsrCondMaxFragLen := 0
      ; LOptionIdSet 
          := SingletonOptionIdSet ( FsChildNodeRef . FsOptionIds [ FALSE ] ) 

      ; IF FsChildNodeRef . FsEstChildOpt IN LangUtil . ChildOptSetOptByAS 
        THEN 
          P4fsrFillAltFrag 
            ( FsChildNodeRef 
            , PrincipalChildTok := LbeStd . Tok__Null 
            , OptionIdSet := LRTable . OptionIdSetEmpty 
            )
        END (* IF *)  
      ; IF LStarListCard > 0 
        THEN
          P4fsrFillEmptyListFrag 
            ( FsChildNodeRef 
            , IntSets . Minimum ( FsChildNodeRef . FsAltStarListTokSet ) 
            , LOptionIdSet 
            ) 
        END (* IF *) 
      ; P4fsrFillSingletonFrags 
          ( FsChildNodeRef , FsChildNodeRef . FsAltListTokSet , LOptionIdSet ) 
      ; P4fsrFillPluralFrags 
          ( FsChildNodeRef , FsChildNodeRef . FsAltListTokSet , LOptionIdSet ) 
      ; IF LFixedTokSetCard > 0  
        THEN 
          IF GDoFactorClasses 
          THEN 
            LTok 
              := LdlSemantics . TokForAtomTokSet (* SIDE EFFECTS! *) 
                   ( LangInfo 
                   , FsChildNodeRef 
                   , FsChildNodeRef . FsAltFixedTokSet 
                   , MustBePresent := TRUE 
                   )
(* TODO: ^This could be stored in FsChildNodeRef . FsTok.
         Use it from there.
*) 
          ; Assert  
              ( LTok # LbeStd . Tok__Null  
              , AFT . A_GrammarGen_P4fsrUncondPrincipalChild_Class_atom_token_not_in_table 
              ) 
          ; P4fsrFillAltFrag ( FsChildNodeRef , LTok , LOptionIdSet )  
          ELSE 
            P4fsrFillTokSetFrags 
              ( FsChildNodeRef , FsChildNodeRef . FsAltFixedTokSet , LOptionIdSet ) 
          END (* IF *) 
        END (* IF *) 
      ; Assert
          ( P4fsrCondResultSs = NUMBER ( P4fsrCondResultRef ^ )  
          , AFT . A_P4fsrUncondPrincipalChild_underfull
          )  
      ; MaxFragLen := P4fsrCondMaxFragLen 
      ; RETURN P4fsrCondResultRef 
      END P4fsrUncondPrincipalChild 

  ; PROCEDURE P4fsrUncondFsSubtree 
      ( FsSubtreeNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR MaxFragLen : CARDINAL  
      ; VAR (* IN OUT *) NextOptionId : LRTable . OptionIdTyp 
      ) 
    : FragSetRefTyp 
    RAISES { Backout } 
    (* Called only when outside a conditional construct. *) 

    = VAR P4fsrUfsChildrensFragSets : FragSetSetRefTyp 
    ; VAR P4fsrUfsProductFrag : LRTable . TokArrayRefTyp 
    ; VAR P4fsrUfsProductFragSet : FragSetRefTyp 
    ; VAR P4fsrUfsNextFragSetSs : PortTypes . Int32Typ  
    ; VAR P4fsrUfsChildCt : PortTypes . Int32Typ 
    ; VAR P4fsrUfsNextProductSs : INTEGER  

    ; PROCEDURE P4fsrUfsRecurse 
        ( FragSetNo : INTEGER  
        ; StartPos : PortTypes . Int32Typ 
        ; OptionIdSet : LRTable . OptionIdSetTyp 
        ) 

      = VAR LFragLen : PortTypes . Int32Typ 
      ; VAR LNewFrag : LRTable . TokArrayRefTyp 

      ; BEGIN 
          IF FragSetNo >= P4fsrUfsChildCt  
          THEN (* End of concatenation *) 
            (* Always copy P4fsrUfsProductFrag, because it could be altered 
               later. 
            *) 
(* TODO: Eliminate duplicate fragments in this set.  This no doubt means 
         the AS/FS is format-ambiguous, so give error if so.
*) 
            LNewFrag := NEW ( LRTable . TokArrayRefTyp , StartPos ) 
          ; LNewFrag ^ := SUBARRAY (  P4fsrUfsProductFrag ^ , 0 , StartPos )
          ; WITH WFrag = P4fsrUfsProductFragSet ^ [ P4fsrUfsNextProductSs ] 
            DO 
              WFrag . TokStringRef := LNewFrag 
            ; WFrag . OptionIdSet := OptionIdSet  
            END (* WITH *) 
          ; INC ( P4fsrUfsNextProductSs )  
          ; MaxFragLen := MAX ( MaxFragLen , StartPos ) 
          ELSE (* Do more concatenation. *) 
            WITH WChildFragSet = P4fsrUfsChildrensFragSets [ FragSetNo ] 
            DO 
              FOR RI := 0 TO NUMBER ( WChildFragSet ^ ) - 1  
              DO WITH WFrag = WChildFragSet [ RI ]  
                DO 
                  LFragLen := NUMBER ( WFrag . TokStringRef ^ ) 
                ; SUBARRAY ( P4fsrUfsProductFrag ^ , StartPos , LFragLen ) 
                    := WFrag . TokStringRef ^ 
                ; P4fsrUfsRecurse 
                    ( FragSetNo + 1 
                    , StartPos + LFragLen 
                    , OptionIdSet + WFrag . OptionIdSet 
                    ) 
                END (* WITH *)  
              END (* FOR *) 
            END (* WITH *) 
          END (* IF *) 
        END P4fsrUfsRecurse 

    ; BEGIN (* P4fsrUncondFsSubtree  *) 
        VAR LProductCt : CARDINAL  
      ; VAR LProductMaxFragLen : CARDINAL  
            (* ^Sum of the max lengths of the sets. *) 
      ; VAR LChildFragSet : FragSetRefTyp 
      ; VAR LChildFragCt : CARDINAL  
      ; VAR LMaxFragLen : CARDINAL 
      ; VAR LResult : FragSetRefTyp 

      ; BEGIN (* Body block for P4fsrUncondFsSubtree. *) 
          MaxFragLen := 0 
        ; P4fsrUfsChildCt := NUMBER ( FsSubtreeNodeRef . FsChildren ^ ) 
        ; P4fsrUfsChildrensFragSets 
            := NEW 
                 ( FragSetSetRefTyp , P4fsrUfsChildCt (* Maybe extravagant. *) )
        ; P4fsrUfsNextFragSetSs := P4fsrUfsChildCt  
        ; LProductCt := 1 
        ; LProductMaxFragLen := 0  

        (* Traverse the children. *)
        ; FOR RI := P4fsrUfsChildCt - 1 TO 0 BY - 1 
          (* Do them in RtoL temporal order, and store the results spatially
             RtoL in P4fsrUfsChildrensFragSets, so it reads normally. 
          *) 
          DO  
            LChildFragSet 
              := P4fsrUncondFsChild 
                   ( FsSubtreeNodeRef . FsChildren ^ [ RI ] 
                   , (* VAR *) LMaxFragLen 
                   , (* IN OUT *) NextOptionId 
                   ) 
          (* Barring error cases, there should never be an empty fragment
             set.  If there were, it would mean the fs child cannot format
             and the concatenation set would be empty too.  So we use the
             representational convention that a NIL fragment set actually 
             denotes a singleton set whose one member is the empty string
             and has an empty OptionIdSet.  Furthermore, we can put such 
             a fragment set into the concatenated set by just omitting it 
             entirely from the operation.  On the other hand, a pointer to 
             an array with zero NUMBER really does mean an empty fragment set.  
          *) 
          ; IF LChildFragSet # NIL 
            THEN  
              LChildFragCt := NUMBER ( LChildFragSet ^ ) 
            ; IF LChildFragCt > 0 
              THEN  
                DEC ( P4fsrUfsNextFragSetSs ) 
              ; P4fsrUfsChildrensFragSets ^ [ P4fsrUfsNextFragSetSs ] 
                  := LChildFragSet 
              ; LProductCt := LProductCt * LChildFragCt 
              ; INC ( LProductMaxFragLen , LMaxFragLen ) 
              ELSE (* Empty set.  The whole result will be empty too. *) 
                RETURN LChildFragSet 
              END (* IF *) 
            END (* IF *) 
          END (* FOR *) 

        (* Regular-expression-like concatenation. *) 
        ; IF P4fsrUfsNextFragSetSs = P4fsrUfsChildCt  
          THEN (* If all children format to the empty string, we want to return
                  a singleton fragment set containing the empty string with 
                  empty OptionIdSet.  *) 
            RETURN NIL 
          ELSE 
            P4fsrUfsProductFragSet := NEW ( FragSetRefTyp , LProductCt )
          ; P4fsrUfsNextProductSs := 0  
          ; P4fsrUfsProductFrag 
              := NEW ( LRTable . TokArrayRefTyp , LProductMaxFragLen ) 
          ; P4fsrUfsRecurse 
              ( FragSetNo := P4fsrUfsNextFragSetSs   
              , StartPos := 0 
              , OptionIdSet := LRTable . OptionIdSetEmpty 
              ) 
          ; IF P4fsrUfsNextProductSs = LProductCt 
            THEN LResult := P4fsrUfsProductFragSet 
            ELSE 
              LResult := NEW ( FragSetRefTyp , P4fsrUfsNextProductSs )
            ; LResult ^ 
                := SUBARRAY 
                     ( P4fsrUfsProductFragSet ^ , 0 , P4fsrUfsNextProductSs ) 
            END (* IF *) 
          ; RETURN LResult 
          END (* IF *) 
        END (* Body block for P4fsrUncondFsSubtree *) 
      END P4fsrUncondFsSubtree 

  ; PROCEDURE P4fsrUncondFsChild 
      ( FsChildNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR MaxFragLen : CARDINAL  
      ; VAR (* IN OUT *) NextOptionId : LRTable . OptionIdTyp 
      ) 
    : FragSetRefTyp 
    RAISES { Backout } 
    (* Called only when outside a conditional construct. *) 
    (* Mostly a dispatching procedure. *) 

    = BEGIN (* P4fsrUncondFsChild *) 
        CASE FsChildNodeRef . FsKind <* NOWARN *> 
        OF FsKindTyp . FsKindSubtreeHoriz
        , FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeFill 
        => RETURN 
             P4fsrUncondFsSubtree 
               ( FsChildNodeRef 
               , (* VAR *) MaxFragLen 
               , (* IN OUT *) NextOptionId 
               ) 

        | FsKindTyp . FsKindNull 
        , FsKindTyp . FsKindBegOfImage
        , FsKindTyp . FsKindEndOfImage 
        , FsKindTyp . FsKindLineBreakReqd
        , FsKindTyp . FsKindLineBreakOpt
        => (* These derive <empty>. *) 
          MaxFragLen := 0 
        ; RETURN NIL 
          (* This really means a singleton fragment set with empty string
             and empty OptionIdSet . *)  

        | FsKindTyp . FsKindInsTok 
        => MaxFragLen := 1 (* Token *)  
        ; RETURN SingletonFragsetSingletonString ( FsChildNodeRef . FsTok ) 

        | FsKindTyp . FsKindEstChildOfFixed 
        , FsKindTyp . FsKindEstChildOfList  
        => RETURN 
             P4fsrUncondPrincipalChild 
               ( FsChildNodeRef , (* VAR *) MaxFragLen )

        | FsKindTyp . FsKindCondFmt 
        => RETURN 
             P4fsrCondConstruct ( FsChildNodeRef , (* VAR *) MaxFragLen )

        END (* CASE *) 
      END P4fsrUncondFsChild 

  ; PROCEDURE P4fsrFixedRule ( FsFixedRuleNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Backout } 

    = VAR LMaxFragLen : CARDINAL 
    ; VAR LFragSet : FragSetRefTyp  
    ; VAR LOptionId : LRTable . OptionIdTyp 

    ; BEGIN (* P4fsrFixedRule *) 
        LOptionId := 0 
      ; LFragSet 
          := P4fsrUncondFsSubtree 
               ( FsFixedRuleNodeRef 
               , (* VAR, dead *) LMaxFragLen 
               , (* IN OUT *) LOptionId 
               ) 
      ; IF LFragSet = NIL 
        THEN (* Singleton FragSet, empty string, with empty OptionIdSet. *) 
          Automaton . AddExternalProduction 
            ( Gram 
            , Left := FsFixedRuleNodeRef . FsTok
            , Right := LRTable . EmptyString ( ) 
            , BuildTok := FsFixedRuleNodeRef . FsTok 
            ) 
        ELSE 
          FOR RI := 0 TO NUMBER ( LFragSet ^ ) - 1 
          DO 
            WITH WFrag = LFragSet ^ [ RI ] 
            DO  
              Automaton . AddExternalProduction 
                ( Gram 
                , Left := FsFixedRuleNodeRef . FsTok
                , Right := WFrag . TokStringRef 
                , BuildTok := FsFixedRuleNodeRef . FsTok 
                , OptionIdSet := WFrag . OptionIdSet 
                ) 
            END (* WITH *) 
          END (* FOR *) 
        END (* IF *) 
      END P4fsrFixedRule 

  ; PROCEDURE P4FsrBuildListSeps 
      ( FsListRuleRef : LangUtil . FsNodeRefTyp 
      ; VAR Seps : LRTable . TokArrayRefTyp 
      ; VAR SepCt : INTEGER 
      ) 
    RAISES { Backout } 
    (* Make Seps point to a token string for the list separators, even if 
       empty, and set SepCt to the count of list separators. 
    *) 

    = VAR LSepCt : CARDINAL 
    ; VAR LNextSs : CARDINAL 

    ; BEGIN 
      (* Count the visible separator tokens. *) 
        LSepCt := 0 
      ; FOR RI := 1 TO NUMBER ( FsListRuleRef . FsChildren ^ ) - 1 
        DO
          INC ( LSepCt 
              , ORD ( FsListRuleRef . FsChildren ^ [ RI ] . FsKind  
                      = FsKindTyp . FsKindInsTok 
                    ) 
              ) 
        END (* FOR *)
      ; SepCt := LSepCt  

      (* Fill an array with them. *) 
      ; Seps := NEW ( LRTable . TokArrayRefTyp , LSepCt ) 
      ; LNextSs := 0  
      ; FOR RI := 1 TO NUMBER ( FsListRuleRef . FsChildren ^ ) - 1 
        DO 
          WITH WFsListChild = FsListRuleRef . FsChildren ^ [ RI ] 
          DO 
            IF WFsListChild. FsKind  
                = FsKindTyp . FsKindInsTok 
            THEN 
              Seps ^ [ LNextSs ] 
                := WFsListChild. FsTok 
            ; INC ( LNextSs ) 
            END (* IF *) 
          END (* WITH *) 
        END (* FOR *) 
      ; Assert 
          ( LNextSs = LSepCt 
          , AFT . A_GrammarGen_P4FsrLazyBuildListSeps_Count_mismatch
          ) 
      END P4FsrBuildListSeps  

  ; PROCEDURE P4fsrListRule 
      ( FsListRuleRef : LangUtil . FsNodeRefTyp ; Trailing : BOOLEAN ) 
    RAISES { Backout } 

    = VAR LElemFragSet : FragSetRefTyp
    ; VAR LFragsetCt : CARDINAL 
    ; VAR LMaxFragLen : CARDINAL 
    ; VAR LSeps : LRTable . TokArrayRefTyp := NIL 
    ; VAR LSepCt : INTEGER := FIRST ( INTEGER ) 
    ; VAR LFragLen , LFragLen2 : CARDINAL 
    ; VAR LRight : LRTable . TokArrayRefTyp 
    ; VAR LOptionId : LRTable . OptionIdTyp 

    ; BEGIN (* P4fsrListRule *) 
        TYPECASE 
          LdlSemantics . SemDeclOfTok ( LangInfo , FsListRuleRef . FsTok ) 
        OF NULL => (* A previously reported error. *)

        | LdlSemantics . SemDeclAsListNodeTyp ( TSemDeclAsList ) 

        (* Do the list Ast child. *) 

        => LOptionId := 0 
        ; LElemFragSet 
            := P4fsrUncondFsChild 
                 ( LdlSemantics . FsElemOfFsList ( FsListRuleRef ) 
                 , (* VAR, dead. *) LMaxFragLen 
                 , (* IN OUT *) LOptionId 
                 ) 
        ; IF LElemFragSet = NIL 
          THEN (* A list element can derive empty if there are separators
                  and still be unambiguous. 
               *)  
            LElemFragSet := SingletonFragset ( LRTable . EmptyString ( ) ) 
            (* Singleton fragset, empty string, with empty OptionIdSet. *) 
            (* ^Handle this special case by making it non-special. *) 
          END (* IF *) 
        ; LFragsetCt := NUMBER ( LElemFragSet ^ ) 
        ; P4FsrBuildListSeps 
            ( FsListRuleRef , (* VAR *) LSeps , (* VAR *) LSepCt )

        (* Generate the production for the empty list case. *) 

        ; IF FsListRuleRef . FsEmptyListTok # LbeStd . Tok__Null 
          THEN
          (* Add EmptyListTok BUILD ListTok ::= <empty> *) 
            Automaton . AddExternalProduction 
              ( Gram 
              , Left := FsListRuleRef . FsEmptyListTok 
              , Right := LRTable . EmptyString ( )  
              , BuildTok := FsListRuleRef . FsTok  
              ) 
          END (* IF *) 
        (* Ldl semantics: Empty list does not allow trailing separators. *) 

        (* Generate productions for the singleton list case. *) 

        ; IF LFragsetCt > 0 
          THEN 
            IF FsListRuleRef . FsSingletonListTok # LbeStd . Tok__Null 
            THEN 
              FOR RI := 0 TO LFragsetCt - 1 
              DO
                WITH WFrag = LElemFragSet ^ [ RI ] 
                DO  

                (* Add SingletonTok BUILD ListTok ::= <alt>, for all 
                   alternatives. *)
                  Automaton . AddExternalProduction 
                    ( Gram 
                    , Left := FsListRuleRef . FsSingletonListTok
                    , Right := WFrag . TokStringRef 
                    , BuildTok := FsListRuleRef . FsTok  
                    , OptionIdSet := LRTable . OptionIdSetEmpty 
                    ) 

                (* If optional trailing separators are possible, add
                   SingletonTok BUILD ListTok ::= <alt> <seps>,
                   for all alternatives. *)
                ; IF Trailing AND LSepCt > 0 
                  THEN 
                    LFragLen := NUMBER ( WFrag . TokStringRef ^ ) 
                  ; LRight 
                     := NEW ( LRTable . TokArrayRefTyp , LFragLen + LSepCt )
                  ; SUBARRAY ( LRight ^ , 0 , LFragLen ) 
                     := WFrag . TokStringRef ^ 
                  ; SUBARRAY ( LRight ^ , LFragLen , LSepCt ) := LSeps ^ 
                  ; Automaton . AddExternalProduction 
                      ( Gram 
                      , Left := FsListRuleRef . FsSingletonListTok
                      , Right := LRight 
                      , BuildTok := FsListRuleRef . FsTok  
                      , OptionIdSet := LRTable . OptionIdSetForListTrail 
                      ) 
                  END (* IF *) 
                END (* WITH *) 
              END (* FOR *) 
            END (* IF *) 

          (* Generate productions for the plural list case. *) 

          ; IF FsListRuleRef . FsPluralListTok # LbeStd . Tok__Null 
            THEN 

            (* Add PluralTok BUILD ListTok ::= SublistTok *) 
              Automaton . AddExternalProduction 
                ( Gram 
                , Left := FsListRuleRef . FsPluralListTok 
                , Right
                    := LRTable . SingletonString 
                         ( FsListRuleRef . FsSublistTok )
                , BuildTok := FsListRuleRef . FsTok  
                , OptionIdSet := LRTable . OptionIdSetEmpty 
                ) 

              (* If optional trailing separators are possible, add 
                 PluralTok BUILD ListTok ::= SublistTok <seps> *) 
            ; IF Trailing AND LSepCt > 0 
              THEN 
                LRight := NEW ( LRTable . TokArrayRefTyp , 1 + LSepCt )
              ; LRight ^ [ 0 ] := FsListRuleRef . FsSublistTok 
              ; SUBARRAY ( LRight ^ , 1 , LSepCt ) := LSeps ^ 
              ; Automaton . AddExternalProduction 
                  ( Gram 
                  , Left := FsListRuleRef . FsPluralListTok
                  , Right := LRight 
                  , BuildTok := FsListRuleRef . FsTok  
                  , OptionIdSet := LRTable . OptionIdSetForListTrail 
                  ) 
              END (* IF *) 

            (* Add SublistTok ::= SublistTok <seps> SublistTok. *) 
            ; LRight := NEW ( LRTable . TokArrayRefTyp , 2 + LSepCt )
            ; LRight ^ [ 0 ] := FsListRuleRef . FsSublistTok 
            ; SUBARRAY ( LRight ^ , 1 , LSepCt ) := LSeps ^ 
            ; LRight ^ [ 1 + LSepCt ] := FsListRuleRef . FsSublistTok 
            ; Automaton . AddExternalProduction 
                ( Gram 
                , Left := FsListRuleRef . FsSublistTok 
                , Right := LRight
                , BuildTok := LbeStd . Tok__Null
                , OptionIdSet := LRTable . OptionIdSetEmpty 
                ) 

            (* Loop through all alternative list element strings. *) 
            ; FOR RI := 0 TO LFragsetCt - 1 
              DO WITH WFrag = LElemFragSet ^ [ RI ] 
                DO 
                  LFragLen := NUMBER ( WFrag . TokStringRef ^ ) 

                (* Add SublistTok ::= <alt> <seps> SublistTok. (for all alts) *)
                ; LRight 
                    := NEW ( LRTable . TokArrayRefTyp , LFragLen + LSepCt + 1 )
                ; SUBARRAY ( LRight ^ , 0 , LFragLen ) := WFrag . TokStringRef ^
                ; SUBARRAY ( LRight ^ , LFragLen , LSepCt ) := LSeps ^ 
                ; LRight ^ [ LFragLen + LSepCt ] 
                    := FsListRuleRef . FsSublistTok 
                ; Automaton . AddExternalProduction 
                    ( Gram 
                    , Left := FsListRuleRef . FsSublistTok 
                    , Right := LRight
                    , BuildTok := LbeStd . Tok__Null
                    , OptionIdSet := LRTable . OptionIdSetEmpty 
                    ) 

                (* Add SublistTok ::= SublistTok <seps> <alt>. (for all alts) *)
                ; LRight 
                    := NEW ( LRTable . TokArrayRefTyp , 1 + LSepCt + LFragLen )
                ; LRight ^ [ 0 ] := FsListRuleRef . FsSublistTok 
                ; SUBARRAY ( LRight ^ , 1 , LSepCt ) := LSeps ^ 
                ; SUBARRAY ( LRight ^ , 1 + LSepCt , LFragLen ) 
                    := WFrag . TokStringRef ^
                ; Automaton . AddExternalProduction 
                    ( Gram 
                    , Left := FsListRuleRef . FsSublistTok 
                    , Right := LRight
                    , BuildTok := LbeStd . Tok__Null
                    , OptionIdSet := LRTable . OptionIdSetEmpty 
                    ) 

                (* Cartesian square of all alternative list element strings. *) 
                ; FOR RJ := 0 TO LFragsetCt - 1 
                  DO WITH WFrag2 = LElemFragSet ^ [ RJ ] 
                    DO 
                      LFragLen2 := NUMBER ( WFrag2 . TokStringRef ^ ) 

                    (* Add SublistTok ::= <alt> <seps> <alt2>. 
                       (for all alts X alts2) *) 
                    ; LRight 
                        := NEW ( LRTable . TokArrayRefTyp 
                               , LFragLen + LSepCt + LFragLen2 
                               )
                    ; SUBARRAY ( LRight ^ , 0 , LFragLen ) 
                        := WFrag . TokStringRef ^
                    ; SUBARRAY ( LRight ^ , LFragLen , LSepCt ) := LSeps ^ 
                    ; SUBARRAY ( LRight ^ , LFragLen + LSepCt , LFragLen2 ) 
                        := WFrag2 . TokStringRef ^
                    ; Automaton . AddExternalProduction 
                        ( Gram 
                        , Left := FsListRuleRef . FsSublistTok 
                        , Right := LRight
                        , BuildTok := LbeStd . Tok__Null
                        , OptionIdSet := LRTable . OptionIdSetEmpty 
                        ) 
                    END (* WITH *) 
                  END (* FOR *) 
                END (* WITH *) 
              END (* FOR *) 
            END (* IF *) 
          END (* IF *) 

        ELSE (* A previously reported error. *)
        END (* TYPECASE *) 
      END P4fsrListRule 

  ; BEGIN (* Pass4FsRule *) 
      IF FsRuleNodeRef # NIL 
      THEN 
        CASE FsRuleNodeRef . FsKind 
        OF FsKindTyp . FsKindEstFixedHoriz  
        , FsKindTyp . FsKindEstFixedVert  
        , FsKindTyp . FsKindEstFixedFill  
        => P4fsrFixedRule ( FsRuleNodeRef )

        | FsKindTyp . FsKindEstListHoriz  
        , FsKindTyp . FsKindEstListVert  
        , FsKindTyp . FsKindEstListFill   
        => P4fsrListRule ( FsRuleNodeRef , Trailing := FALSE )

        | FsKindTyp . FsKindEstListTrailHoriz  
        , FsKindTyp . FsKindEstListTrailVert  
        , FsKindTyp . FsKindEstListTrailFill   
        => P4fsrListRule ( FsRuleNodeRef , Trailing := TRUE )

        ELSE 
        END (* CASE *) 
      END (* IF *) 
    END Pass4FsRule  

; PROCEDURE GenProdsForUsedClasses 
    ( LangInfo : LangInfoRefTyp 
    ; Gram : GrammarSubTyp 
    ) 
  RAISES { Backout }
  (* Generate productions for all used classes in ClassTable. *)  

  = VAR LClassInfoRef : ClassInfo . ClassInfoRefTyp 

  ; VAR LIterator : SetClassInfoTbl . Iterator 
  ; VAR LSet : TokSetTyp 

  ; BEGIN (* GenProdsForUsedClasses *) 
      LIterator := LangInfo . ClassTable . iterate ( ) 
    ; WHILE LIterator . next ( (* VAR *) LSet , (* VAR *) LClassInfoRef ) 
      DO
        IF LClassInfoRef ^ . UseCount > 0 
        THEN 
          LdlSemantics . GenNonclassTokSetProductions 
            ( LangInfo 
            , Gram , LClassInfoRef ^ . ClassTok 
            , LSet 
            , ReplaceListCardToks := TRUE
            ) 
        ; Automaton . NoteNontermKind 
            ( Gram 
            , LClassInfoRef ^ . ClassTok 
            , NontermKindTyp . NtkConcreteOnly 
            )
        END (* IF *) 
      END (* WHILE *) 
    END GenProdsForUsedClasses 

; PROCEDURE GenProdsForUsedSubstrings 
    ( LangInfo : LangInfoRefTyp 
    ; Gram : GrammarSubTyp 
    ) 
  (* Generate productions for all used substrings in TokStringTable. *)  

  = VAR LTokStringInfoRef : TokStringInfo . TokStringInfoRefTyp 
  ; VAR LIterator : TokStringInfoTbl . Iterator 
  ; VAR LTokString : TokString . T  

  ; BEGIN (* GenProdsForUsedTokStrings *) 
      LIterator := LangInfo . TokStringTable . iterate ( ) 
    ; WHILE 
        LIterator . next ( (* VAR *) LTokString , (* VAR *) LTokStringInfoRef ) 
      DO
        IF LTokStringInfoRef ^ . UseCount > 0 
        THEN 
          Automaton . AddProduction 
            ( Gram 
            , Left := LTokStringInfoRef ^ . LHSTok 
            , Right := LTokString 
            , Precedence := LRTable . PrecedenceMax 
            ) 
        ; Automaton . NoteNontermKind 
            ( Gram 
            , LTokStringInfoRef ^ . LHSTok 
            , NontermKindTyp . NtkConcreteOnly 
            )
        END (* IF *) 
      END (* WHILE *) 
    END GenProdsForUsedSubstrings 

; VAR GDoFactorClasses : BOOLEAN := TRUE 
; VAR GDoFactorSubstrings : BOOLEAN := TRUE 
; VAR GDoFactorAlts : BOOLEAN := TRUE 
(* TODO: ^Make these fields in GrammarSubType, after bootstrapping problems
         have been overcome.
*)

(* VISIBLE: *) 
; PROCEDURE Generate 
    ( LangInfo : LangInfoRefTyp 
    ; Factored : BOOLEAN := TRUE 
    ) 
  RAISES { Backout } 

  = VAR LGram : GrammarSubTyp  
  ; VAR LMessageCt : PortTypes . Card32Typ 

  ; BEGIN (* Block *)
      IF LangInfo = NIL THEN RETURN END (* IF *) 
    ; GDoFactorClasses := Factored  
    ; GDoFactorSubstrings := Factored  
    ; GDoFactorAlts := Factored  
    ; GPossibleTokstringAtomTokCt := 0 

    ; IF 
TRUE OR 
        Messages . MessageCount ( MessageCodes . KindTyp . MkError ) = 0 
      THEN 
(*TODO: Fix this so it doesn't suppress CS generation if there are error
        messages, but only from processing the handwritten CS. 
*) 
        LGram := NEW ( GrammarSubTyp ) 
      ; LGram . LangInfo := LangInfo 
      ; LGram . IsGenerated := TRUE   
      ; LGram . FactoredClasses := GDoFactorClasses 
      ; LGram . FactoredSubstrings := GDoFactorSubstrings 
      ; LGram . FactoredAlts := GDoFactorAlts 
      ELSE LGram := NIL 
      END (* IF *) 
    ; LangInfo . GenGram := LGram 
    ; LMessageCt 
        := Messages . MessageCount ( MessageCodes . KindTyp . MkError ) 
    ; PreloadNamedClasses ( LangInfo , LGram ) 
    ; LangInfo ^ . TokStringTable 
        := NEW ( TokStringInfoTbl . Default ) 
           . init 
               ( sizeHint 
                   := LangInfo ^ . AsCsClassToks - LangInfo ^ . AsPartialToks 
               )
(* FIXME: ^This size hint has no connection to the price of eggs. *) 

    (* Pass 1 over the FS rules: *) 

    ; LangInfo . GcsChildToks := LangInfo . CsFixedToks
    ; IF DoInsertPredMarkTok 
      THEN
        PredMarkTok := LdlSemantics . NewInternalConcreteTok ( LangInfo , NIL ) 
(* FIXME: Get a real Fs node for this.          ^ *) 
      END (* IF *) 
    ; LangInfo . GenProductions 
        := ClassTokProdCt ( LangInfo , LangInfo . StartTok )      

    (* For all FS rules. *) 
    ; FOR RTok := LangUtilLo . TokClassFirstTok 
                    ( LangInfo , TokClassTyp . TokClassAsPlus )
          TO LangUtilLo . TokClassLastTok 
               ( LangInfo , TokClassTyp . TokClassAsFixed )
      DO Pass1FsRule 
           ( LangInfo 
           , LangInfo . FsTreeMapRef ^ [ RTok - FsTreeMapBias ]
           ) 
(* FIXME: Handle the Trailing list tokens. *) 
      END (* FOR *) 

    ; IF Messages . MessageCount ( MessageCodes . KindTyp . MkError ) 
         = LMessageCt  
      THEN 
        BetweenPass2And3 ( LangInfo , LGram ) 
      ; WITH WManOper = LangInfo . Gram . OperArrayRef ^ 
        DO SUBARRAY 
             ( LangInfo . GenGram . OperArrayRef ^ , 0 , NUMBER ( WManOper ) )
           := WManOper
        END (* WITH *) 
        (* We should be able to just use the same prefix of OperArray for the 
           generated grammar. *) 
(* TODO: Straighten out the way we set up OperArrayRef.  Automaton.InitAutomaton
         reinitializes it for the generated grammar, and then we just overlay
         part of it here.  Probably do InsertPrecAndAssoc again, but there are 
         two of those, one for Ldl0 and one for Ldl1, and they use Ldl[0|1]Tok
         and Ldl[0|1]Child. Messy. *) 

      (* Pass 4 over the FS rules: *) 

      (* For all FS rules. *)
      ; FOR RTok := LangUtilLo . TokClassFirstTok 
                      ( LangInfo , TokClassTyp . TokClassAsPlus )
            TO LangUtilLo . TokClassLastTok 
                 ( LangInfo , TokClassTyp . TokClassAsFixed )
        DO Pass4FsRule 
             ( LangInfo 
             , LGram 
             , LangInfo . FsTreeMapRef ^ [ RTok - FsTreeMapBias ]
             ) 
        END (* FOR *) 
(* FIXME: Handle the Trailing list tokens. *) 
      ; IF Messages . MessageCount ( MessageCodes . KindTyp . MkError ) 
           = LMessageCt  
        THEN 

        (* Finish up. *) 
          GenProdsForUsedClasses ( LangInfo , LGram ) 
        ; GenProdsForUsedSubstrings ( LangInfo , LGram ) 
        ; LdlSemantics . FinishLRGeneration 
            ( LangInfo , LGram , LangInfo . Productions ) 
        END (* IF *) 
      END (* IF *) 
    END Generate 

; BEGIN (* GrammarGen *)
  END GrammarGen
. 
