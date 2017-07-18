
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Generate concrete syntax rules from format syntax (and also abstract 
   syntax).
*) 

MODULE GrammarGen2 

; IMPORT Text 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 
; IMPORT Automaton
; IMPORT ClassInfo 
; IMPORT Infos 
; IMPORT IntSets
; IMPORT LangUtil 
; FROM LangUtil IMPORT PredicateKindTyp 
; FROM LangUtil IMPORT FsKindTyp 
; IMPORT LbeStd 
; IMPORT LdlSemantics 
; FROM LdlSemantics 
  IMPORT FsTreeMapBias , TokMapBias , StringMapBias , LangInfoRefTyp
; IMPORT LRTable 
; FROM LRTable IMPORT NontermKindTyp 
; IMPORT MessageCodes 
; IMPORT Messages 
; FROM Messages IMPORT SemError 
; IMPORT PortTypes 
; IMPORT SetClassInfoTbl 
; IMPORT SharedStrings 
; IMPORT TokRelation 

; TYPE AFT = MessageCodes . T 

; TYPE TokSetTyp = IntSets . T 

; CONST ListCardEmpty = LdlSemantics . ListCardTyp . ListCardEmpty 
; CONST ListCardSingleton = LdlSemantics . ListCardTyp . ListCardSingleton 
; CONST ListCardPlural = LdlSemantics . ListCardTyp . ListCardPlural 

; TYPE FragTyp 
  = RECORD 
      OptionIdSet : LRTable . OptionIdSetTyp 
    ; TokStringRef : LRTable . TokArrayRefTyp
    END 
; TYPE FragSetTyp = ARRAY OF FragTyp 
; TYPE FragSetRefTyp = REF FragSetTyp
; TYPE FragSetSetTyp = ARRAY OF FragSetRefTyp   
  ; FragSetSetRefTyp = REF FragSetSetTyp   

; PROCEDURE SingletonFragsetSingletonString ( Tok : LbeStd . TokTyp ) 
  : FragSetRefTyp 

  = VAR LResult : FragSetRefTyp 

  ; BEGIN (* SingletonFragsetSingletonString *)
      LResult := NEW ( FragSetRefTyp , 1 )
    ; LResult ^ [ 0 ] . TokStringRef := LRTable . SingletonString ( Tok )  
    ; LResult ^ [ 0 ] . OptionIdSet := LRTable . OptionIdSetEmpty 
    ; RETURN LResult 
    END SingletonFragsetSingletonString

; PROCEDURE SingletonFragset ( TokString : LRTable . TokArrayRefTyp ) 
  : FragSetRefTyp 

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



(*
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
         directly.  
         Better yet, just put Lang into GrammarTyp and dispense with 
         GrammarSubTyp altogether?  
         But it's hard to avoid circular imports here.  Straighten this out
         somehow. 
*) 
  = BEGIN (* TokImage *) 
      RETURN LangUtil . TokImage ( Tok , LbeStd . LangLdlAnalyzed ) 
    END TokImage 

(* Sets of list cardinalities. *) 

; TYPE CardTyp = LangUtil . CardTyp 
; TYPE CardSetTyp = LangUtil . CardSetTyp 
; CONST CardSetEmpty = LangUtil . CardSetEmpty 
; CONST CardSetAbsent = LangUtil . CardSetAbsent  
; CONST CardSetEmptyList = LangUtil . CardSetEmptyList  

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
  (* Create a new ClassTable and load it with classes named in the 
     language definition. 
  *) 

  = VAR LMemberSet : TokSetTyp 
  ; VAR LClassInfoRef : ClassInfo . ClassInfoRefTyp 
  ; VAR LPrevClassInfoRef : ClassInfo . ClassInfoRefTyp 

  ; BEGIN 
      LangInfo ^ . ClassTable 
        := NEW ( SetClassInfoTbl . Default ) 
           . init 
               ( sizeHint 
                   := LangInfo ^ . AsCsClassToks - LangInfo ^ . AsPartialToks 
               )
    ; FOR RTok := LangInfo ^ . AsPartialToks TO LangInfo ^ . AsCsClassToks - 1 
      DO 
        LMemberSet := LdlSemantics . AsTokSetOfTok ( LangInfo , RTok ) 
      ; LClassInfoRef := NEW ( ClassInfo . ClassInfoRefTyp ) 
      ; LClassInfoRef ^ . ClassTok := RTok 
      ; LClassInfoRef ^ . UseCount := 0
      ; IF LangInfo . ClassTable . get 
             ( LMemberSet , (* VAR *) LPrevClassInfoRef ) 
        THEN
          Messages . SemErrorText 
            ( ": " 
              & Gram . tokImage ( LPrevClassInfoRef ^ . ClassTok ) 
              & " and " 
              & Gram . tokImage ( RTok ) 
            , AFT . I_Classes_have_identical_contents
            ) 
        END (* IF *) 
      ; EVAL LangInfo . ClassTable . put ( LMemberSet , LClassInfoRef ) 
      END (* FOR *) 
    END PreloadNamedClasses 

; PROCEDURE NewClassTok 
    ( LangInfo : LangInfoRefTyp ; FsNodeRef : LangUtil . FsNodeRefTyp ) 
  : LbeStd . TokTyp 
  (* Allocate a token for an internally-generated class. *) 

  = VAR LResult : LbeStd . TokTyp 

  ; BEGIN 
      IF LangInfo ^ . GcsChildToks + 1 < LAST ( LbeStd . TokTyp )  
      THEN 
        LResult := LangInfo ^ . GcsChildToks 
      ; INC ( LangInfo ^ . GcsChildToks ) 
      ; RETURN LResult 
      ELSE
        SemError 
          ( FsNodeRef . FsLdlNodeNo , AFT . E_Token_space_overflow ) 
      ; RETURN LbeStd . Tok__Null 
      END (* IF *)
    END NewClassTok 

; PROCEDURE ClassTokForSet 
    ( LangInfo : LangInfoRefTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; TokSet : TokSetTyp 
    ; UseCountIncrement : CARDINAL := 0
    ; MustBePresent : BOOLEAN := FALSE  
    ) 
  : LbeStd . TokTyp 
  RAISES { AssertionFailure } 
  (* WARNING: This function has SIDE EFFECTS! 
              1) Adds a rule to LangInfo ^ . ClassTable, if not there
              2) Increments its UseCount
              3) Appropriately increments production count. 
  *) 
  (* PRE: FsNodeRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 
(* TODO: We should be able to split the case UseCountIncrement=0,MustBePresent
         (done in pass 4) from otherwise (done in pass 1). 
*) 

  = VAR LCard : CARDINAL 
  ; VAR LClassInfoRef : ClassInfo . ClassInfoRefTyp 
  ; VAR LClassTok : LbeStd . TokTyp 
  
  ; BEGIN 
      LCard := IntSets . Card ( TokSet ) 
    ; IF LCard = 0
      THEN 
        RETURN LbeStd . Tok__Empty 
      ELSIF LCard = 1 
      THEN 
        RETURN IntSets . ArbitraryMember ( TokSet ) 
      ELSIF LangInfo ^ . ClassTable . get ( TokSet , (* VAR *) LClassInfoRef ) 
      THEN (* It's already in the ClassTable. *) 
        IF LClassInfoRef ^ . UseCount = 0 AND UseCountIncrement > 0 
        THEN
          INC ( LangInfo ^ . GenProductions , LCard ) 
        END (* IF *)  
      ; INC ( LClassInfoRef ^ . UseCount , UseCountIncrement ) 
      ; RETURN LClassInfoRef ^ . ClassTok
      ELSIF MustBePresent 
      THEN 
        CantHappen ( AFT . A_GrammarGen_ClassTokForSet_Not_in_table )  
      ; RETURN LbeStd . Tok__Null (* Just to suppress compiler warning. *) 
      ELSE (* Add a new entry to the ClassTable. *) 
        LClassInfoRef := NEW ( ClassInfo . ClassInfoRefTyp ) 
      ; LClassTok := NewClassTok ( LangInfo , FsNodeRef )  
      ; LClassInfoRef ^ . ClassTok := LClassTok 
      ; IF UseCountIncrement > 0 
        THEN
          INC ( LangInfo ^ . GenProductions , LCard ) 
        END (* IF *)  
      ; LClassInfoRef ^ . UseCount := UseCountIncrement 
      ; EVAL LangInfo ^ . ClassTable . put ( TokSet , LClassInfoRef ) 
      ; RETURN LClassTok 
      END (* IF *) 
    END ClassTokForSet 

; PROCEDURE SingletonListOpt 
    ( LangInfo : LangInfoRefTyp 
    ; FsRuleNodeRef : LangUtil. FsNodeRefTyp 
    ; FsListChildRef : LangUtil. FsNodeRefTyp 
    ; ListTokSet : TokSetTyp 
    ) 
  : IntSets . T (* Set of list element tokens optimizable. *) 
  RAISES { AssertionFailure } 
  (* PRE: FsListChildRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 

  = VAR SloTokRel : TokRelation . T 
  ; VAR SloAllElemTokSet : TokSetTyp 
  ; VAR SloSingletonOptMin : LbeStd . TokTyp := LbeStd . Tok__Null 
  ; VAR SloSingletonOptMapRef : LRTable . TokArrayRefTyp 

  ; PROCEDURE SloFwdOuter ( OuterListTok : IntSets . ValidElemT ) 
    RAISES { AssertionFailure } 

    = VAR SloFoOuterElemTokSet : TokSetTyp 

    ; PROCEDURE SloFoFwdInner ( InnerListTok : IntSets . ValidElemT ) 
      = VAR LInnerListFsNode : LangUtil. FsNodeRefTyp 
      ; VAR LInnerElemFsNode : LangUtil. FsNodeRefTyp 
      ; VAR LInnerElemTokSet : TokSetTyp 

      ; BEGIN 
          IF InnerListTok # OuterListTok
          THEN 
            LInnerListFsNode 
              := LdlSemantics . FsRuleForTokUsingLangInfo 
                   ( LangInfo , InnerListTok ) 
          ; LInnerElemFsNode 
              := LangUtil . FsEstChildRef 
                   ( LInnerListFsNode . FsChildren 
                       ^ [ LangUtil . FsChildNoListEstChild ] 
                   ) 
          ; LInnerElemTokSet 
              := TokRelation . GroundRightRelatives 
                   ( LangInfo . ClassRelation , LInnerElemFsNode . FsTok ) 
          ; SloFoOuterElemTokSet 
              := IntSets . Difference 
                   ( SloFoOuterElemTokSet , LInnerElemTokSet ) 
          END (* IF *) 
        END SloFoFwdInner

    ; BEGIN (* SloFwdOuter *) 
        VAR LListFsNode : LangUtil. FsNodeRefTyp 
      ; VAR LElemFsNode : LangUtil. FsNodeRefTyp 
      ; BEGIN (* Block SloFwdOuter *) 
          LListFsNode 
            := LdlSemantics . FsRuleForTokUsingLangInfo 
                 ( LangInfo , OuterListTok ) 
        ; Assert 
            ( LListFsNode . FsKind IN LangUtil . FsKindSetEstList 
            , AFT . A_GrammarGen_SloFwdOuter_NotList
            ) 
        ; LElemFsNode 
            := LangUtil . FsEstChildRef 
                 ( LListFsNode . FsChildren 
                     ^ [ LangUtil . FsChildNoListEstChild ] 
                 ) 
        ; SloFoOuterElemTokSet 
            := TokRelation . GroundRightRelatives 
                 ( LangInfo . ClassRelation , LElemFsNode . FsTok ) 
        ; IF IntSets . IsElement 
               ( FsRuleNodeRef . FsTok , SloFoOuterElemTokSet ) 
          THEN SloFoOuterElemTokSet := IntSets . Empty ( ) 
          END (* IF *) 

        ; <* FATAL ANY *> BEGIN 
            IntSets . ForAllDo ( ListTokSet , SloFoFwdInner ) 
          END (* Block *)
        ; TokRelation . SetRightRelatives 
            ( SloTokRel , OuterListTok , SloFoOuterElemTokSet ) 
        ; SloAllElemTokSet 
            := IntSets . Union ( SloAllElemTokSet , SloFoOuterElemTokSet ) 
        END (* Block *) 
      END SloFwdOuter

  ; BEGIN (* SingletonListOpt *)  
      VAR LEstChildRef : LangUtil. FsNodeRefTyp 
    ; BEGIN (* Block for SingletonListOpt *) 
        IF NOT IntSets . IsEmpty ( ListTokSet ) 
        THEN
          SloTokRel 
            := TokRelation . New 
                 ( IntSets . Minimum ( ListTokSet ) 
                 , IntSets . Maximum ( ListTokSet ) 
                 )
        ; SloAllElemTokSet := IntSets . Empty ( ) 
        (* SloFwdOuter and SloFoFwdInner constitute a doubly-nested loop over 
           the cartesian square of ListTokSet. 
        *) 
        ; <* FATAL ANY *> BEGIN 
            IntSets . ForAllDo ( ListTokSet , SloFwdOuter ) 
          END (* Block *) 

        (* Here, we have: 
           1) SloTokRel maps each member of ListTokSet to the set of element 
              tokens that can be elements of that list and to no other list in 
              ListTokSet, i.e., the set of element tokens that can be 
              singleton-optimized to a singleton of that list.  
           2) SloAllElemTokSet is the union of the sets mapped-to by SloTokRel.

           We need SloSingletonOptMapRef, a commute of SloTokRel, with 
           SloAllElemTokSet as its left domain.  This will be a function.  
        *) 
        
        ; IF IntSets . Card ( SloAllElemTokSet ) > 0 
          THEN 
            SloSingletonOptMin := IntSets . Minimum ( SloAllElemTokSet ) 
          ; SloSingletonOptMapRef 
              := NEW ( LRTable . TokArrayRefTyp 
                     , IntSets . Maximum ( SloAllElemTokSet ) 
                       - SloSingletonOptMin + 1 
                     )  
          ; FOR RI := FIRST ( SloSingletonOptMapRef ^ ) 
                   TO LAST ( SloSingletonOptMapRef ^ )
            DO SloSingletonOptMapRef ^ [ RI] := LbeStd . Tok__Null 
            END (* FOR *) 
          (* A doubly-nested loop over the left (unique) and right (possibly 
             multiple) members of SloTokRel. 
          *) 
          ; FOR RListTok 
                := IntSets . Minimum ( ListTokSet ) 
                TO IntSets . Maximum ( ListTokSet ) 
            DO 
              PROCEDURE SloRevInner ( ElemTok : IntSets . ValidElemT ) 

              = BEGIN 
                  WITH WMapElem 
                       = SloSingletonOptMapRef ^ [ ElemTok - SloSingletonOptMin ] 
                  DO 
                    Assert 
                      ( WMapElem = LbeStd . Tok__Null  
                      , AFT . A_GrammarGen_SloRevInner_NotAFunction 
                      ) 
                  ; WMapElem := RListTok
                  END (* WITH *) 
                END SloRevInner

            ; <* FATAL ANY *> BEGIN 
                IntSets . ForAllDo 
                   ( TokRelation . RightRelatives ( SloTokRel , RListTok ) 
                   , SloRevInner 
                   )
              END (* Block *)
            END (* FOR *) 
          ; FsListChildRef . FsSingletonOptMin := SloSingletonOptMin 
          ; FsListChildRef . FsSingletonOptMapRef := SloSingletonOptMapRef 
          ; LEstChildRef := LangUtil . FsEstChildRef ( FsListChildRef ) 
          ; IF LEstChildRef # FsListChildRef 
            THEN 
              LEstChildRef . FsSingletonOptMin := SloSingletonOptMin 
            ; LEstChildRef . FsSingletonOptMapRef := SloSingletonOptMapRef 
(* TODO: Here, the singleton optimization map variables are copied up from
         the FsEstChild node to the CondFmt or FsSublist node that encloses
         it.  Make the FsTrees.dump not generate duplicate output for this,
         if indeed it is reference-equal to that on the FsEstChild.
*)  
            END (* IF *)  
          END (* IF *) 
        END (* IF *) 
      END (* Block *) 
    ; RETURN SloAllElemTokSet 
    END SingletonListOpt 

; PROCEDURE Pass1FsRule 
    ( LangInfo : LangInfoRefTyp ; FsRuleNodeRef : LangUtil . FsNodeRefTyp ) 
  RAISES { AssertionFailure } 
  (* 1) Count concrete syntax rules needed for this FS rule. 
     2) Set the FsFragCt fields of each predicate and unconditional principal
        child FsNode. 
     3) Note the need for list cardinality tokens for other list nodes, because
        of their use as principal children of this Fs Tree.  
     4) Create tokens for principal child sets. 
     This is unnested from Generate, just to keep things from getting 
     too deep. 
  *) 

  = VAR P1fsrCardSet : CardSetTyp := CardSetEmpty  
  ; VAR P1fsrTokSet : TokSetTyp := NIL  

  ; PROCEDURE P1fsrNoteNeededListCardTok 
     ( ListTok : IntSets . ValidElemT 
     ; Card : LdlSemantics . ListCardTyp 
     )
    (* Note the need for a list cardinality token for cardinality Card,
       of list token ListTok.
    *) 

    = VAR LListSemDecl : LdlSemantics . SemDeclTyp 

    ; BEGIN 
        LListSemDecl  
          := LdlSemantics . SemDeclOfTok ( LangInfo , ListTok ) 
      ; TYPECASE LListSemDecl 
        OF NULL => 

        | LdlSemantics . SemDeclAsListNodeTyp ( TSemDeclAsList )  
        => TSemDeclAsList . ListCardUsed [ Card ] := TRUE 

        ELSE 
        END (* TYPECASE *) 
      END P1fsrNoteNeededListCardTok 

  ; PROCEDURE P1fsrNoteNeededSingletonListCardTok 
      ( ListTok : IntSets . ValidElemT ) 
    (* Specialized version of P1fsrNoteNeededListCardTok for 
       singleton list cardinality.  Needed as a callback. 
    *) 

    = BEGIN 
        P1fsrNoteNeededListCardTok ( ListTok , ListCardSingleton ) 
      END P1fsrNoteNeededSingletonListCardTok 

  ; PROCEDURE P1fsrNoteNeededPluralListCardTok 
      ( ListTok : IntSets . ValidElemT ) 
    (* Specialized version of P1fsrNoteNeededListCardTok for 
       plural list cardinality.  Needed as a callback. 
    *) 

    = BEGIN 
        P1fsrNoteNeededListCardTok ( ListTok , ListCardPlural ) 
      END P1fsrNoteNeededPluralListCardTok 

  ; PROCEDURE P1fsrCases 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; TokSet : TokSetTyp 
      ; CardSet : CardSetTyp 
      ; IsExplicitElse : BOOLEAN 
      )
    : CARDINAL (* Count of fragments. *)  
    RAISES { AssertionFailure } 
    (* Handle every case that satisfies both TokSet and CardSet. *) 

    = VAR LResult : CARDINAL 
    ; VAR LListSet : TokSetTyp 
    ; VAR LListCard : CARDINAL 
    ; VAR LStarListSet : TokSetTyp 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LFixedTokSet : TokSetTyp 
    ; VAR LSingletonListElemTokSet : IntSets . T  
    ; VAR LFixedTokSetCard : CARDINAL 
    ; VAR LTok : LbeStd . TokTyp 
    ; VAR LPredClassTokSet : TokSetTyp 

    ; BEGIN 
        LResult := 0 
      ; LSingletonListElemTokSet := IntSets . Empty ( ) (* Could change. *)
      ; LListSet := ListToksSubset ( LangInfo , TokSet )
      ; LListCard := IntSets . Card ( LListSet ) 
      ; IF LListCard > 0 
        THEN 
          LStarListSet := StarListToksSubset ( LangInfo , LListSet ) 
        ; LStarListCard := IntSets . Card ( LStarListSet ) 
        ; LFixedTokSet := IntSets . Difference ( TokSet , LListSet ) 
        ELSE 
          LStarListSet := IntSets . Empty ( )  
        ; LStarListCard := 0 
        ; LFixedTokSet := TokSet 
        END (* IF *) 
      ; LFixedTokSetCard := IntSets . Card ( LFixedTokSet ) 

      (* Absent. *) 
      ; IF LangUtil . FsEstChildRef ( FsNodeRef ) . FsEstChildIsOptional
           AND CardTyp . Absent IN CardSet 
        THEN 
          IF LStarListCard > 0 
          THEN
            IF IsExplicitElse 
            THEN 
              SemError
                ( FsNodeRef . FsLdlNodeNo 
                , AFT . E_Ambiguous_ABSENT_and_possibly_EMPTY_list_In_ELSE 
                ) 
            ELSE 
              SemError
                ( FsNodeRef . FsLdlNodeNo 
                , AFT . E_Ambiguous_ABSENT_and_possibly_EMPTY_list
                ) 
            END (* IF *) 
          END (* IF *) 
        ; INC ( LResult , ORD ( FsNodeRef . FsCondDoParse ) ) 
        ELSIF LStarListCard > 0 AND CardTyp . EmptyList IN CardSet 
 
        (* Empty list. *) 
        THEN
          IF LStarListCard > 1 
          THEN
            IF IsExplicitElse 
            THEN 
              SemError
                ( FsNodeRef . FsLdlNodeNo 
                , AFT . E_Ambiguous_Multiple_empty_lists_in_ELSE
                ) 
            ELSE 
              SemError
                ( FsNodeRef . FsLdlNodeNo 
                , AFT . E_Ambiguous_Multiple_empty_lists
                )
            END (* IF *)  
          END (* IF *) 
        ; IF FsNodeRef . FsCondDoParse 
          THEN 
            LTok := IntSets . Minimum ( LStarListSet ) 
          ; P1fsrNoteNeededListCardTok ( LTok , ListCardEmpty )
          ; INC ( LResult ) 
          END (* IF *) 
        END (* IF *) 

      (* Singleton and plural lists. *) 
      ; IF LListCard > 0 AND FsNodeRef . FsCondDoParse 
        THEN 
          IF CardTyp . SingletonList IN CardSet 
          THEN
            <* FATAL ANY *> BEGIN 
              IntSets . ForAllDo 
                ( LListSet , P1fsrNoteNeededSingletonListCardTok )
            END (* Block *)
          ; INC ( LResult , LListCard ) 
          ; LSingletonListElemTokSet 
              := SingletonListOpt 
                   ( LangInfo , FsRuleNodeRef , FsNodeRef , LListSet ) 
          END (* IF *) 
        ; IF CardTyp . PluralList IN CardSet 
          THEN
            <* FATAL ANY *> BEGIN 
              IntSets . ForAllDo 
                ( LListSet , P1fsrNoteNeededPluralListCardTok )
            END (* Block *) 
          ; INC ( LResult , LListCard ) 
          END (* IF *) 
        END (* IF *) 

      (* Fixed children. *) 
      ; IF LFixedTokSetCard > 0 AND FsNodeRef . FsCondDoParse  
        THEN 
(* TODO: Maybe use Chris' original scheme and generate a (possibly anonymous)
         class here, and in predicates too.  If there is no empty alternative,
         this would be safe against introducing LALR conflicts anyway, but it
         might take an additional, simple prepass over the alternatives to 
         distinguish that case. 
*) 
          IF LFixedTokSetCard > 1 
          THEN 
            LTok 
              := ClassTokForSet (* SIDE EFFECTS! *)
                   ( LangInfo 
                   , FsNodeRef 
                   , LFixedTokSet 
                   , UseCountIncrement := 1 
                   )
          ELSE 
            LTok := IntSets . ArbitraryMember ( LFixedTokSet )  
          END (* IF *) 
        (* Here, LTok is the CS token that will derive the fixed set. *)  
        ; LPredClassTokSet := IntSets . Union ( LFixedTokSet , LListSet )  
        ; LPredClassTokSet 
            := IntSets . Union ( LPredClassTokSet , LSingletonListElemTokSet )  
        ; FsNodeRef . FsCondPredicate . PredicateClass 
            := ClassTokForSet (* SIDE EFFECTS! *)
                 ( LangInfo 
                 , FsNodeRef 
                 , LPredClassTokSet 
                 , UseCountIncrement := 1 
                 )
(*      ; INC ( LResult ) *) 
        ; INC ( LResult , LFixedTokSetCard )  
        END (* IF *) 
      ; RETURN LResult 
      END P1fsrCases  

  ; PROCEDURE P1fsrPredicate ( FsPredNodeRef : LangUtil . FsNodeRefTyp ) 
    : CARDINAL (* Count of fragments. *)  
    RAISES { AssertionFailure } 
    (* PRE: FsPredNodeRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 
    (* Think of this as returning a singleton set singleton token fragment. *) 

    = PROCEDURE P1fsrPredGetListSetInfo 
        ( VAR ListSet : TokSetTyp ; VAR ListCard : CARDINAL ) 

      = BEGIN
          ListSet := ListToksSubset ( LangInfo , P1fsrTokSet )
        ; ListCard := IntSets . Card ( ListSet ) 
        ; IF ListCard = 0 
          THEN
            SemError
              ( FsPredNodeRef . FsLdlNodeNo 
              , AFT . W_Unsatisfiable_list_cardinality_predicate_No_list_children 
              ) 
          ; FsPredNodeRef . FsCondPredicate . PredicateKind 
              := PredicateKindTyp . PredicateKindFalse 
          END (* IF *) 
        END P1fsrPredGetListSetInfo 

    ; BEGIN (* P1fsrPredicate *) 

        VAR LListSet : TokSetTyp
      ; VAR LListCard : CARDINAL 
      ; VAR LListTok : LbeStd . TokTyp 
      ; VAR LMemberSet : TokSetTyp 
      ; VAR LResult : CARDINAL 
      ; VAR LStarListSet : TokSetTyp
      ; VAR LStarListCard : CARDINAL 
      ; VAR LSingletonListElemTokSet : IntSets . T 

      ; BEGIN (* Block for P1fsrPredicate *) 
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
(* TODO: When NOPARSE addition is complete, this case should be impossible. *) 
            => LResult 
                 := P1fsrCases  
                      ( FsPredNodeRef 
                      , P1fsrTokSet 
                      , P1fsrCardSet 
                      , IsExplicitElse := FALSE 
                      ) 
            ; P1fsrCardSet := CardSetEmpty 
            ; P1fsrTokSet := IntSets . Empty ( )              

            | FsKindTyp . FsKindCondFmt 
            => CASE FsPredNodeRef . FsCondPredicate . PredicateKind 
              OF PredicateKindTyp . PredicateKindNull 
              => (* Maybe earlier-reported error? Will be eliminated. *) 

              | PredicateKindTyp . PredicateKindFalse 
              => (* This alternative will be eliminated. *) 

              | PredicateKindTyp . PredicateKindTrue
              => LResult 
                   := P1fsrCases  
                        ( FsPredNodeRef 
                        , P1fsrTokSet 
                        , P1fsrCardSet 
                        , IsExplicitElse := TRUE 
                        ) 
              ; P1fsrCardSet := CardSetEmpty 
              ; P1fsrTokSet := IntSets . Empty ( )              

              | PredicateKindTyp . PredicateKindAbsent 
              => IF NOT LangUtil . FsEstChildRef ( FsPredNodeRef ) 
                        . FsEstChildIsOptional
                THEN 
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unsatisfiable_ABSENT_on_required_child
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                ; P1fsrCardSet := P1fsrCardSet - CardSetAbsent  
                ELSIF CardTyp . Absent IN P1fsrCardSet 
                THEN
                  LResult := ORD ( FsPredNodeRef . FsCondDoParse )  
                ; P1fsrCardSet := P1fsrCardSet - CardSetAbsent  
                ELSE 
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unreachable_duplicate_ABSENT_predicate  
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                END (* IF *)  

              | PredicateKindTyp . PredicateKindPresent 
              => LResult 
                   := P1fsrCases 
                        ( FsPredNodeRef 
                        , P1fsrTokSet 
                        , P1fsrCardSet - CardSetAbsent  
                        , IsExplicitElse := FALSE 
                        )
              ; IF LResult = 0 
                THEN  
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unsatisfiable_PRESENT_No_members_remain 
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                END (* IF *)
              ; P1fsrTokSet := IntSets . Empty ( ) 
              ; P1fsrCardSet := CardSetAbsent  

              | PredicateKindTyp . PredicateKindEmptyList 
              => IF CardTyp . EmptyList IN P1fsrCardSet 
                THEN
                  LStarListSet := StarListToksSubset ( LangInfo , P1fsrTokSet )
                ; LStarListCard := IntSets . Card ( LStarListSet ) 
                ; IF LStarListCard = 0 
                  THEN SemError
                       ( FsPredNodeRef . FsLdlNodeNo 
                       , AFT . W_Unsatisfiable_EMPTY_predicate_No_star_lists   
                       ) 
                  ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                      := PredicateKindTyp . PredicateKindFalse 
                  ELSE (* LStarListCard > 0 *) 
                    IF LStarListCard > 1 
                    THEN  
                      SemError
                        ( FsPredNodeRef . FsLdlNodeNo 
                        , AFT . E_Ambiguous_EMPTY_predicate_on_multiple_star_lists 
                        )
                    END (* IF *) 
                  ; IF FsPredNodeRef . FsCondDoParse 
                    THEN 
                      LListTok := IntSets . Minimum ( LStarListSet ) 
                      (* ^If LStarListCard > 1, it's ambiguous. Just arbitrarily
                         choose one list. 
                      *) 
                    ; LResult := 1 
                    ; P1fsrNoteNeededListCardTok ( LListTok , ListCardEmpty ) 
                    END (* IF *) 
                  END (* IF *) 
(* MAYBE: Create a class token for P1fsrTokSet 
          and put it into the predicate's class tok.
*) 
                ; P1fsrCardSet := P1fsrCardSet - CardSetEmptyList 
                ; IF P1fsrCardSet = CardSetEmpty 
                  THEN (* Remove all the list tokens. *)  
                    P1fsrTokSet 
                      := IntSets . Difference ( P1fsrTokSet , LStarListSet )
                  END (* IF *) 
                ELSE 
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unreachable_duplicate_EMPTY_predicate  
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                END (* IF *) 

              | PredicateKindTyp . PredicateKindNonemptyList
              => IF CardTyp . SingletonList IN P1fsrCardSet 
                    OR CardTyp . PluralList IN P1fsrCardSet 
                THEN
                  P1fsrPredGetListSetInfo 
                    ( (* VAR *) LListSet , (* VAR *) LListCard )
                ; IF LListCard > 0 
                  THEN 
                    IF CardTyp . SingletonList IN P1fsrCardSet 
                       AND FsPredNodeRef . FsCondDoParse 
                    THEN                       
                      <* FATAL ANY *> BEGIN 
                        IntSets . ForAllDo 
                          ( LListSet , P1fsrNoteNeededSingletonListCardTok )
                      END (* Block *) 
                    ; INC ( LResult , LListCard ) 
                    END (* IF *) 
                  ; IF CardTyp . PluralList IN P1fsrCardSet 
                       AND FsPredNodeRef . FsCondDoParse 
                    THEN 
                      <* FATAL ANY *> 
                      BEGIN IntSets . ForAllDo 
                              ( LListSet , P1fsrNoteNeededPluralListCardTok )
                      END (* Block *) 
                    ; INC ( LResult , LListCard ) 
                    END (* IF *) 
                  END (* IF *) 
                ; LSingletonListElemTokSet 
                    := SingletonListOpt 
                         ( LangInfo , FsRuleNodeRef , FsPredNodeRef , LListSet )
(* MAYBE: Create a class token for 
          IntSets . Union ( P1fsrTokSet , LSingletonListElemTokSet )
          and put it into the predicate's class tok.
*) 
                ; P1fsrCardSet 
                    := P1fsrCardSet 
                       - CardSetTyp 
                           { CardTyp . SingletonList , CardTyp . PluralList } 
                ; IF P1fsrCardSet = CardSetEmpty 
                  THEN (* Remove all the list tokens. *)
                    P1fsrTokSet 
                      := IntSets . Difference ( P1fsrTokSet , LListSet )
                  END (* IF *) 
                ELSE 
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unreachable_NONEMPTY_predicate  
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                END (* IF *)  

              | PredicateKindTyp . PredicateKindNonpluralList 
              => IF CardTyp . EmptyList IN P1fsrCardSet 
                    OR CardTyp . SingletonList IN P1fsrCardSet 
                THEN
                  P1fsrPredGetListSetInfo 
                    ( (* VAR *) LListSet , (* VAR *) LListCard )
                ; IF LListCard = 0 
                  THEN
                    SemError
                      ( FsPredNodeRef . FsLdlNodeNo 
                      , AFT . W_Unsatifiable_NONPLURAL_No_list_members  
                      ) 
                  ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                      := PredicateKindTyp . PredicateKindFalse 
                  ELSE 
                    LStarListSet 
                      := StarListToksSubset ( LangInfo , P1fsrTokSet )
                  ; LStarListCard := IntSets . Card ( LStarListSet ) 
                  ; IF LStarListCard > 1 
                       AND CardTyp . EmptyList IN P1fsrCardSet 
                    THEN
                      SemError
                        ( FsPredNodeRef . FsLdlNodeNo 
                        , AFT . E_Ambiguous_NONPLURAL_predicate_on_multiple_star_lists 
                        ) 
                    END (* IF *) 
                  ; IF LStarListCard > 0 
                       AND CardTyp . EmptyList IN P1fsrCardSet 
                       AND FsPredNodeRef . FsCondDoParse 
                    THEN 
                      LListTok := IntSets . Minimum ( LStarListSet ) 
                    ; P1fsrNoteNeededListCardTok ( LListTok , ListCardEmpty ) 
                    ; INC ( LResult ) 
                    END (* IF *) 
                  ; IF CardTyp . SingletonList IN P1fsrCardSet 
                       AND FsPredNodeRef . FsCondDoParse
                    THEN 
                      <* FATAL ANY *> BEGIN 
                        IntSets . ForAllDo 
                          ( LListSet , P1fsrNoteNeededSingletonListCardTok )
                      END (* Block *) 
                    ; INC ( LResult , LListCard ) 
                    END (* IF *) 
                  END (* IF *) 
                ; LSingletonListElemTokSet 
                    := SingletonListOpt 
                         ( LangInfo , FsRuleNodeRef , FsPredNodeRef , LListSet )
(* MAYBE: Create a class token for 
          IntSets . Union ( P1fsrTokSet , LSingletonListElemTokSet )
          and put it into the predicate's class tok.
*) 
                ; P1fsrCardSet 
                    := P1fsrCardSet 
                       - CardSetTyp 
                           { CardTyp . EmptyList , CardTyp . SingletonList } 
                ; IF P1fsrCardSet = CardSetEmpty 
                  THEN (* Remove all the list tokens. *)
                    P1fsrTokSet 
                      := IntSets . Difference ( P1fsrTokSet , LListSet )
                  END (* IF *) 
                ELSE 
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unreachable_NONPLURAL_predicate  
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                END (* IF *) 

              | PredicateKindTyp . PredicateKindPluralList 
              => IF CardTyp . PluralList IN P1fsrCardSet 
                THEN
                  P1fsrPredGetListSetInfo 
                    ( (* VAR *) LListSet , (* VAR *) LListCard )
                ; IF LListCard > 0 
                     AND FsPredNodeRef . FsCondDoParse 
                  THEN 
                    <* FATAL ANY *> BEGIN 
                      IntSets . ForAllDo  
                        ( LListSet , P1fsrNoteNeededPluralListCardTok ) 
                    END (* Block *) 
                  ; INC ( LResult , LListCard ) 
                  END (* IF *) 
                ; LSingletonListElemTokSet 
                    := SingletonListOpt 
                         ( LangInfo , FsRuleNodeRef , FsPredNodeRef , LListSet )
(* MAYBE: Create a class token for 
          IntSets . Union ( P1fsrTokSet , LSingletonListElemTokSet )
          and put it into the predicate's class tok.
*) 
                ; P1fsrCardSet 
                    := P1fsrCardSet - CardSetTyp { CardTyp . PluralList } 
                ; IF P1fsrCardSet = CardSetEmpty 
                  THEN (* Remove all the list tokens. *)
                    P1fsrTokSet 
                      := IntSets . Difference ( P1fsrTokSet , LListSet )
                  END (* IF *) 
                ELSE 
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unreachable_PLURAL_predicate  
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                END (* IF *)  

              | PredicateKindTyp . PredicateKindInClass 
              => LMemberSet 
                   := LdlSemantics . AsTokSetOfTok 
                        ( LangInfo 
                        , FsPredNodeRef . FsCondPredicate . PredicateClass 
                        )
              ; LMemberSet 
                  := IntSets . Intersection ( LMemberSet , P1fsrTokSet ) 
(* FIX:  Someday, Ldl[0|1]FsTrees should compute this intersection, if empty,
         warn and delete the alternative, or store the intersection in
         FsPredNodeRef . FsCondPredicate . PredicateClass.
*) 


              ; LResult 
                  := P1fsrCases 
                       ( FsPredNodeRef 
                       , LMemberSet  
                       , P1fsrCardSet - CardSetAbsent  
                       , IsExplicitElse := FALSE 
                       )
              ; IF LResult = 0 
                THEN 
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unsatisfiable_MEMBER_No_members 
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                END (* IF *) 
              ; P1fsrTokSet 
                  := IntSets . Difference ( P1fsrTokSet , LMemberSet )
              END (* CASE PredicateKind *) 

            END (* CASE FsKind *)  
          END (* IF *) 
        ; FsPredNodeRef . FsFragCt := LResult 
        ; RETURN LResult 
        END (* Block *) 
      END P1fsrPredicate 

  ; PROCEDURE P1fsrCondConstruct 
      ( VAR (* IN OUT *) FsCondFmtNodeRef : LangUtil . FsNodeRefTyp 
        (* ^Possibly changed to eliminate unsatisfiable alternatives. *) 
      ) 
    : CARDINAL (* Count of fragments. *)  
    RAISES { AssertionFailure } 
    (* PRE: FsCondFmtNodeRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 
    (* NOT to be called recursively for subsequent predicates. *) 

    = PROCEDURE P1fsrCcEliminateFalsePreds 
        ( VAR (* IN OUT *) FsNodeRef : LangUtil . FsNodeRefTyp 
          (* ^Possibly changed to eliminate unsatisfiable alternatives. *) 
        ) 

      = BEGIN 
          WHILE FsNodeRef # NIL 
                AND FsNodeRef . FsKind = FsKindTyp . FsKindCondFmt 
                AND ( FsNodeRef . FsCondPredicate . PredicateKind 
                      = PredicateKindTyp . PredicateKindFalse 
                      OR FsNodeRef . FsCondPredicate . PredicateKind 
                         = PredicateKindTyp . PredicateKindNull 
                    ) 
          DO (* Skip these, to eliminate them. *)  
            FsNodeRef := FsNodeRef . FsCondAltRef 
          END (* WHILE *) 
        ; IF FsNodeRef # NIL 
          THEN 
            IF FsNodeRef . FsKind = FsKindTyp . FsKindCondFmt 
            THEN 
              P1fsrCcEliminateFalsePreds 
                ( (* IN OUT *) FsNodeRef . FsCondAltRef ) 
            END (* IF *) 
          END (* IF *) 
        END P1fsrCcEliminateFalsePreds

    ; BEGIN (* P1fsrCondConstruct *) 
        VAR LOrigFsNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR LEstChildFsNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR LPredFsNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR LCount : CARDINAL 
      ; VAR LResult : CARDINAL 

      ; BEGIN (* Block for P1fsrCondConstruct *) 
          LOrigFsNodeRef := FsCondFmtNodeRef 
        ; LResult := 0 
(* CHECK: Be sure, and document how, we can never have a NIL as an else
          alternative (i.e., FsCondFmtNodeRef # NIL *) 
        ; LEstChildFsNodeRef := LangUtil . FsEstChildRef ( FsCondFmtNodeRef )  

        (* Initialize the cardinality and token sets. *) 
        ; P1fsrCardSet := LangUtil . CardSetUniverse
          (* We put Absent in P1fsrCardSet even when the Est child is required,
             to detect and warn about ABSENT predicates on required children.
          *)  
(* TODO: We can detect this error without needing Absent.  See P1fsrPredicate,
         PredicateKindAbsent case. 
*) 
        ; P1fsrTokSet 
            := LdlSemantics . AsTokSetOfTok 
                 ( LangInfo , LEstChildFsNodeRef . FsTok ) 

        (* Initially process the alternatives. *) 
        ; LPredFsNodeRef := FsCondFmtNodeRef 
        ; WHILE LPredFsNodeRef # NIL 
          DO
            LCount := P1fsrPredicate ( LPredFsNodeRef )
          ; INC ( LResult , LCount ) 
          ; LPredFsNodeRef := LPredFsNodeRef . FsCondAltRef 
          END (* WHILE *) 

        (* Eliminate unsatisfiable predicates. *)
        ; P1fsrCcEliminateFalsePreds ( (* IN OUT *) FsCondFmtNodeRef ) 
        ; IF FsCondFmtNodeRef = NIL  
          THEN
            SemError
              ( LOrigFsNodeRef . FsLdlNodeNo 
              , AFT . E_No_satisfiable_alternative
              ) 
          END (* IF *) 
        ; RETURN LResult   
        END (* Block *) 
      END P1fsrCondConstruct 

  ; PROCEDURE P1fsrUncondPrincipalChild 
      ( FsChildNodeRef : LangUtil . FsNodeRefTyp )
    : CARDINAL (* Fragment count. *) 
    RAISES { AssertionFailure } 
    (* PRE: FsChildNodeRef is a principal child that is NOT inside a conditional
       construct.  It could still have multiple alternatives if it's optional 
       or contains list(s).
    *) 

    = VAR LResult : CARDINAL 
    ; VAR LTokSet : TokSetTyp 
    ; VAR LTokCard : CARDINAL 
    ; VAR LListCard : CARDINAL 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LListSet : TokSetTyp 
    ; VAR LStarListSet : TokSetTyp 
    ; VAR LFixedTokSet : TokSetTyp 
    ; VAR LFixedTokSetCard : CARDINAL
    ; VAR LClassTok : LbeStd . TokTyp 

    ; BEGIN 
        LResult := 0 
(* TODO: Something about LResult overflowing. *) 
      ; LTokSet 
          := LdlSemantics . AsTokSetOfTok 
               ( LangInfo , FsChildNodeRef . FsTok ) 
      ; LTokCard := IntSets . Card ( LTokSet ) 
      ; INC ( LResult , ORD ( FsChildNodeRef . FsEstChildIsOptional ) ) 
        (* ^For empty alternative. *) 
      ; LListSet := ListToksSubset ( LangInfo , LTokSet )
      ; LListCard := IntSets . Card ( LListSet ) 
      ; IF LListCard > 0 
        THEN 
          LStarListSet := StarListToksSubset ( LangInfo , LListSet )
        ; LStarListCard := IntSets . Card ( LStarListSet ) 
        ; IF FsChildNodeRef . FsEstChildIsOptional 
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
            P1fsrNoteNeededListCardTok 
              ( IntSets . Minimum ( LStarListSet ) , ListCardEmpty )
          ; INC ( LResult ) (* For <list>__0 *) 
          ; IF LStarListCard > 1 
            THEN 
              SemError
                ( FsChildNodeRef . FsLdlNodeNo 
                , AFT . E_Ambiguous_multiple_star_lists 
                ) 
            END (* IF *) 
          END (* IF *) 
        ; <* FATAL ANY *> BEGIN 
            IntSets . ForAllDo 
              ( LListSet , P1fsrNoteNeededSingletonListCardTok )
          ; IntSets . ForAllDo ( LListSet , P1fsrNoteNeededPluralListCardTok )
          END (* Block *) 
        ; INC ( LResult , LListCard * 2 ) (* For <list>__1 and  <list>__2 *) 
        ; LFixedTokSet := IntSets . Difference ( LTokSet , LListSet ) 
        ELSE 
          LFixedTokSet := LTokSet          
        END (* IF *) 
      ; EVAL SingletonListOpt 
               ( LangInfo , FsRuleNodeRef , FsChildNodeRef , LListSet ) 
      ; LFixedTokSetCard := IntSets . Card ( LFixedTokSet )
      ; IF LFixedTokSetCard > 0
        THEN
          IF LFixedTokSetCard > 1
          THEN
            LClassTok 
              := ClassTokForSet (* SIDE EFFECTS! *) 
                   ( LangInfo
                   , FsChildNodeRef
                   , LFixedTokSet
                   , UseCountIncrement := 1
                   )
          ; FsChildNodeRef . FsCondPredicate . PredicateClass := LClassTok 
(* FIXME: ^This is not what we really want, although it won't break anything. *)
          END (* IF *)
(*      ; INC ( LResult )
          (* ^Either a single Est tok or a class tok. *)   
*) 
        ; INC ( LResult , LFixedTokSetCard )
        END (* IF *)
      ; FsChildNodeRef . FsFragCt := LResult 
      ; RETURN LResult 
      END P1fsrUncondPrincipalChild 

  ; PROCEDURE P1fsrUncondFsSubtree 
      ( FsSubtreeNodeRef : LangUtil . FsNodeRefTyp ) 
    : CARDINAL (* Fragment count. *) 
    RAISES { AssertionFailure } 
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
                 ( (* IN OUT *) FsSubtreeNodeRef . FsChildren ^ [ RI ] 
                   (* ^Possibly changed to remove unsatisfiable alternatives. *)
                 ) 
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
      ( VAR (* IN OUT *) FsChildNodeRef : LangUtil . FsNodeRefTyp 
        (* ^Possibly changed to eliminate unsatisfiable alternatives. *) 
      ) 
    : CARDINAL (* Fragment count. *) 
    RAISES { AssertionFailure } 
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
        => RETURN 
             P1fsrCondConstruct ( (* IN OUT *) FsChildNodeRef ) 

        END (* CASE *) 
      END P1fsrUncondFsChild 

  ; PROCEDURE P1fsrFixedRule ( FsFixedRuleNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { AssertionFailure } 
    (* PRE: FsFixedRuleNodeRef has FsKindEstFixed*. *) 

    = VAR LFragCt : CARDINAL  

    ; BEGIN (* P1fsrFixedRule *) 
        LFragCt := P1fsrUncondFsSubtree ( FsFixedRuleNodeRef ) 
      ; INC ( LangInfo ^ . GenProductions , LFragCt)  
      END P1fsrFixedRule 

  ; PROCEDURE P1fsrListRule 
      ( FsListRuleNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { AssertionFailure } 
    (* PRE: FsFixedRuleNodeRef has FsKindEstList*. *) 

    = VAR LFragCt : CARDINAL   

    ; BEGIN (* P1fsrListRule *) 
        LFragCt 
          := P1fsrUncondFsChild 
               ( (* IN OUT *) FsListRuleNodeRef . FsChildren ^ [ 0 ] 
                 (* ^Possibly changed to remove unsatisfiable alternatives. *)
               ) 
      ; INC ( LangInfo ^ . GenProductions , LFragCt)  
      END P1fsrListRule 

  ; BEGIN (* Pass1FsRule *)
      IF FsRuleNodeRef # NIL 
      THEN 
        CASE FsRuleNodeRef . FsKind 
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

; PROCEDURE AltCountForCondConstruct 
    ( <* UNUSED *> LangInfo : LangInfoRefTyp  
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  : CARDINAL (* Number of alternatives. *) 
  (* FsNodeRef is the first alternative of a conditional construct.  It
     can also be a (unconditional) principal child.
  *) 

  = VAR LResult : CARDINAL 
  ; VAR LFsAltNodeRef : LangUtil . FsNodeRefTyp 

  ; BEGIN 
      LResult := 0 
    ; LFsAltNodeRef := FsNodeRef 
    ; WHILE LFsAltNodeRef # NIL 
      DO
(* REVIEW: Check this for unconditional present child.  ??? *) 
        INC ( LResult , LFsAltNodeRef . FsFragCt )  
      ; LFsAltNodeRef := LFsAltNodeRef . FsCondAltRef 
      END (* WHILE *) 
    ; RETURN LResult 
    END AltCountForCondConstruct  

(* As of 2011-03-21, we could move the work done in pass 2 into pass 1.
   But leave it here in case we want to generate optimized list productions
   for specialized combinations of empty, singleton, and plural.  We won't
   know what combinations exist until pass 2. 
*) 

; PROCEDURE Pass2FsRule 
    ( LangInfo : LangInfoRefTyp 
    ; Gram : GrammarSubTyp 
    ; FsRuleNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  (* Pass 2.  For all AS list tokens: 
     1) Count productions to derive empty, singleton, and plural lists,
        when these are actually used. 
     2) Copy list cardinality tokens from semantic nodes to Fs nodes.
     3) Put FsTreeMap rules in for the list cardinality tokens.  
  *) 

  = VAR LAltCt : CARDINAL 
  ; VAR LFsElemNodeRef : LangUtil . FsNodeRefTyp 
  ; VAR LElemTokSet : TokSetTyp 

  ; BEGIN (* Pass2FsRule *) 

      IF FsRuleNodeRef # NIL 
      THEN 
        CASE FsRuleNodeRef . FsKind 
        OF FsKindTyp . FsKindEstListHoriz  
        , FsKindTyp . FsKindEstListVert  
        , FsKindTyp . FsKindEstListFill   
        , FsKindTyp . FsKindEstListTrailHoriz  
        , FsKindTyp . FsKindEstListTrailVert  
        , FsKindTyp . FsKindEstListTrailFill   

        => TYPECASE 
            LdlSemantics . SemDeclOfTok ( LangInfo , FsRuleNodeRef . FsTok ) 
          OF NULL => (* A previously reported error. *)

          | LdlSemantics . SemDeclAsListNodeTyp ( TSemDeclAsList ) 
          => (* It's a list rule.  Count its productions. *)
            LAltCt 
              := AltCountForCondConstruct 
                   ( LangInfo , FsRuleNodeRef . FsChildren ^ [ 0 ] ) 
          ; IF TSemDeclAsList . ListCardUsed [ ListCardEmpty ] 
            THEN 
              INC ( LangInfo ^ . GenProductions ) 
              (* ^EmptyTok BUILD ListTok ::= <empty>. *)
            ; FsRuleNodeRef . FsEmptyListTok 
                := TSemDeclAsList . ListCardToks [ ListCardEmpty ]
            ; LangInfo . FsTreeMapRef 
              ^ [ FsRuleNodeRef . FsEmptyListTok - FsTreeMapBias ] 
                := FsRuleNodeRef  
(* TODO: A whole lot of stuff to rework how list cardinality tokens are 
         generated.  Do them much earlier in Ldl[01]Semantics, and put them
         in close-in ranges where it will be convenient to give them 
         entries in the token-subscripted maps.
*) 
            ; LFsElemNodeRef := LdlSemantics . FsElemOfFsList ( FsRuleNodeRef ) 
            ; IF LFsElemNodeRef . FsFormatsEmpty 
                 IN LangUtil . FormatsEmptySetPossiblyYes 
              THEN (* The list child can format empty. This implies both that
                      a list child Est can format empty and that there are no
                      insertion tokens surrounding it.
                   *)  
                LElemTokSet 
                  := LdlSemantics . AsTokSetOfTok 
                       ( LangInfo , LFsElemNodeRef . FsTok ) 
(* CHECK: Is this the right token set, even when there are multiple 
          alternatives, etc.?
*) 
              ; (* DECLARE *) 
                  PROCEDURE Visit ( IntSingleElemTok : IntSets . ValidElemT ) 

                  = VAR LMemberFsTree : LangUtil . FsNodeRefTyp 

                  ; BEGIN 
                      LMemberFsTree  
                        := LangInfo . FsTreeMapRef 
                           ^ [ IntSingleElemTok - LbeStd . Tok__FirstLangDep ] 
                    ; IF LMemberFsTree # NIL 
                         AND LMemberFsTree . FsFormatsEmpty 
                             IN LangUtil . FormatsEmptySetPossiblyYes 
                      THEN
                        Messages . SemErrorText 
                          ( ": " 
                            & Gram . tokImage ( IntSingleElemTok ) 
                            & " of star list: " 
                            & Gram . tokImage ( FsRuleNodeRef . FsTok ) 
                            & " " 
                          , AFT . W_Ambiguous_empty_element
                          ) 
(* CHECK: Can we move this error to pass 1? *) 
                      END (* IF *) 
                    END Visit 

                ; <* FATAL ANY *> BEGIN 
                  LdlSemantics . ForAllAsMembersDo 
                    ( LangInfo , LElemTokSet , Visit ) 
                END (* Block *)
              END (* IF *) 
            END (* IF *)  

          ; IF TSemDeclAsList . ListCardUsed [ ListCardSingleton ] 
            THEN 
              INC ( LangInfo ^ . GenProductions , LAltCt ) 
              (* ^ SingletonTok BUILD ListTok ::= <alt>, for all alts. *) 
            ; FsRuleNodeRef . FsSingletonListTok 
                := TSemDeclAsList . ListCardToks [ ListCardSingleton ] 
            ; LangInfo . FsTreeMapRef 
              ^ [ FsRuleNodeRef . FsSingletonListTok - FsTreeMapBias ] 
                := FsRuleNodeRef  
            END (* IF *)  

          ; IF TSemDeclAsList . ListCardUsed [ ListCardPlural ] 
            THEN 
              INC ( LangInfo ^ . GenProductions , 4 + 3 * LAltCt ) 
              (* See P4fsrListRule for the productions to be generated. *) 
            ; FsRuleNodeRef . FsPluralListTok 
                := TSemDeclAsList . ListCardToks [ ListCardPlural ] 
            ; LangInfo . FsTreeMapRef 
                ^ [ FsRuleNodeRef . FsPluralListTok - FsTreeMapBias ] 
                := FsRuleNodeRef  
            END (* IF *)  

          ELSE (* Not analyzed as a list rule. *)
          END (* TYPECASE *) 

        ELSE (* Not syntactically a list rule. *)  
        END (* CASE *) 
      END (* IF *) 
    END Pass2FsRule 

; PROCEDURE GenClassTokProds 
    ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  RAISES { AssertionFailure } 
  (* If Tok is a class, generate productions for its members and also note
     it is a concrete only token.
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
          ) 
      ; Automaton . NoteNontermKind 
          ( LangInfo ^ . GenGram , Tok , NontermKindTyp . NtkConcreteOnly ) 
      ELSE 
      END (* TYPECASE *) 
    END GenClassTokProds 

; PROCEDURE ListCardTok  
    ( LangInfo : LangInfoRefTyp 
    ; ListTok : LbeStd . TokTyp 
    ; Card : LdlSemantics . ListCardTyp 
    ) 
  : LbeStd . TokTyp  
  RAISES { AssertionFailure } 
  (* The list cardinality token for Card of list ListTok. *) 

  = VAR LListSemDecl : LdlSemantics . SemDeclTyp 

  ; BEGIN 
      LListSemDecl  := LdlSemantics . SemDeclOfTok ( LangInfo , ListTok ) 
    ; TYPECASE LListSemDecl 
      OF NULL => RETURN LbeStd . Tok__Null  

      | LdlSemantics . SemDeclAsListNodeTyp ( TSemDeclAsList )  
      => Assert 
           ( TSemDeclAsList . ListCardToks [ Card ] 
             >= LbeStd . Tok__FirstLangDep 
           , AFT . A_ListCardTok_No_cardinality_token 
           ) 
      ; RETURN TSemDeclAsList . ListCardToks [ Card ] 

      ELSE RETURN LbeStd . Tok__Null  
      END (* TYPECASE *) 
    END ListCardTok 

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

; PROCEDURE Pass4FsRule 
    ( LangInfo : LangInfoRefTyp 
    ; Gram : GrammarSubTyp 
    ; FsRuleNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  RAISES { AssertionFailure } 
  (* Generate Concrete syntax rules from one FS rule. *) 
  (* This is unnested from Generate, just to keep things from getting 
     too deep. 
  *) 

  = VAR P4fsrCardSet : CardSetTyp := CardSetEmpty  
  ; VAR P4fsrTokSet : TokSetTyp := NIL  
  ; VAR P4fsrCondResultRef : FragSetRefTyp := NIL  
  ; VAR P4fsrCondResultSs : CARDINAL 
  ; VAR P4fsrCondMaxFragLen : CARDINAL 

  ; PROCEDURE P4fsrAltFrag 
      ( FsAltNodeRef : LangUtil . FsNodeRefTyp 
      ; PrincipalChildTok : LbeStd . TokTyp
      )
    : LRTable . TokArrayRefTyp 
    RAISES { AssertionFailure } 

    = PROCEDURE P4fsrAfCount ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
      : CARDINAL
      (* Count the regular (non-Alt) tokens needed. *)   

      = VAR LResult : CARDINAL  

      ; BEGIN
          CASE FsNodeRef . FsKind <* NOWARN *> 
          OF FsKindTyp . FsKindInsTok 
          => RETURN 1 
          | FsKindTyp . FsKindEstChildOfFixed 
          , FsKindTyp . FsKindEstChildOfList  
          => RETURN ORD ( PrincipalChildTok # LbeStd . Tok__Empty ) 
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
                   , P4fsrAfCount ( FsNodeRef . FsChildren ^ [ RI ] ) 
                   ) 
            END (* FOR *) 
          ; RETURN LResult 
          END (* CASE *) 
        END P4fsrAfCount 

    ; VAR P4fsrAfRhsString : LRTable . TokArrayRefTyp 
    ; VAR P4fsrAfNextSs : PortTypes . Int32Typ 

    ; PROCEDURE P4fsrAfFill ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
      RAISES { AssertionFailure } 

      = BEGIN
          CASE FsNodeRef . FsKind <* NOWARN *> 
          OF FsKindTyp . FsKindInsTok 
          => P4fsrAfRhsString ^ [ P4fsrAfNextSs ] := FsNodeRef . FsTok 
          ; INC ( P4fsrAfNextSs ) 
          | FsKindTyp . FsKindEstChildOfFixed 
          , FsKindTyp . FsKindEstChildOfList  
          => IF PrincipalChildTok # LbeStd . Tok__Empty  
            THEN 
              P4fsrAfRhsString [ P4fsrAfNextSs ] := PrincipalChildTok
            ; INC ( P4fsrAfNextSs ) 
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
            DO P4fsrAfFill ( FsNodeRef . FsChildren ^ [ RI ] ) 
            END (* FOR *) 
          END (* CASE *) 
        END P4fsrAfFill 

    ; BEGIN (* P4fsrAltFrag *) 
        VAR LFragLen : CARDINAL 

      ; BEGIN (* Block P4fsrAltFrag *) 
          LFragLen := P4fsrAfCount ( FsAltNodeRef ) 
        ; P4fsrAfRhsString 
            := NEW ( LRTable . TokArrayRefTyp , LFragLen )  
        ; P4fsrAfNextSs := 0
        ; P4fsrAfFill ( FsAltNodeRef ) 
        ; Assert 
            ( P4fsrAfNextSs = LFragLen 
            , AFT . A_P4fsrAltFrag_Count_mismatch 
            )
        ; RETURN P4fsrAfRhsString  
        END (* Block P4fsrAltFrag *)  
      END P4fsrAltFrag 

  ; PROCEDURE P4fsrFillFrag 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
        (* ^An FsNode that has a unique principal EstChild descendent. *) 
      ; PrincipalChildTok : LbeStd . TokTyp  
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      ) 
    RAISES { AssertionFailure } 
    (* Fills exactly one fragment in P4fsrCondResultRef. *) 

    = BEGIN 
        IF FsNodeRef . FsCondDoParse 
        THEN 
          WITH WFragRef = P4fsrCondResultRef ^ [ P4fsrCondResultSs ] 
          DO 
            WFragRef . TokStringRef 
              := P4fsrAltFrag ( FsNodeRef , PrincipalChildTok ) 
          ; IF WFragRef . TokStringRef # NIL 
            THEN 
              P4fsrCondMaxFragLen 
                := MAX ( P4fsrCondMaxFragLen 
                       , NUMBER ( WFragRef . TokStringRef ^ ) 
                       )
            END (* IF *) 
          ; WFragRef . OptionIdSet := OptionIdSet 
          END (* WITH *)         
        ; INC ( P4fsrCondResultSs ) 
        END (* IF *) 
      END P4fsrFillFrag 

  ; PROCEDURE P4fsrFillTokSetFrags 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
        (* ^An FsNode that has a unique EstChild descendent. *) 
      ; TokSet : IntSets . T 
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      ) 
    RAISES { AssertionFailure } 

    = PROCEDURE P4fsrFtsfVisit ( PrincipalChildTok : IntSets . ValidElemT ) 
      RAISES { AssertionFailure } 

      = BEGIN 
          P4fsrFillFrag 
            ( FsNodeRef , PrincipalChildTok , OptionIdSet ) 
        END P4fsrFtsfVisit

    ; <* FATAL ANY *> 
      BEGIN (* P4fsrFillTokSetFrags *) 
        IntSets . ForAllDo ( TokSet , P4fsrFtsfVisit ) 
      END P4fsrFillTokSetFrags 

  ; PROCEDURE P4fsrFillEmptyListFrag
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; ListTok : LbeStd . TokTyp  
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      ) 
    RAISES { AssertionFailure } 
    (* Fills one fragment, with an empty list. *) 

    = VAR LEmptyListTok : LbeStd . TokTyp 

    ; BEGIN 
        LEmptyListTok := ListCardTok ( LangInfo , ListTok , ListCardEmpty )
      ; P4fsrFillFrag 
          ( FsNodeRef 
          , PrincipalChildTok := LEmptyListTok 
          , OptionIdSet := OptionIdSet 
          ) 
      END P4fsrFillEmptyListFrag
 
  ; PROCEDURE P4fsrFillSingletonFrags 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
        (* ^An FsNode that has a unique EstChild descendent. *) 
      ; ListToksSubset : TokSetTyp 
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      ) 
    RAISES { AssertionFailure }
    (* PRE:  P4fsrCondResultRef ^ has enough space, starting at 
             P4fsrCondResultSs, for IntSets . Card ( ListToksSubset ) fragments.
       POST: P4fsrCondResultRef ^ has been filled with the fragments of 
             FsNodeRef, with principal children being the Singleton list tokens 
             for each list node in ListToksSubset.  P4fsrCondResultSs is updated
             accordingly.
    *) 

    = PROCEDURE P4fsrFsfVisit ( ListTok : IntSets . ValidElemT ) 
      RAISES { AssertionFailure } 

      = VAR LSingletonTok : LbeStd . TokTyp 

      ; BEGIN 
          LSingletonTok 
            := ListCardTok ( LangInfo , ListTok , ListCardSingleton )
        ; P4fsrFillFrag 
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
    RAISES { AssertionFailure }
    (* PRE:  P4fsrCondResultRef ^ has enough space, starting at 
             P4fsrCondResultSs, for IntSets . Card ( ListToksSubset ) fragments.
       POST: P4fsrCondResultRef ^ has been filled with the fragments of 
             FsNodeRef, with principal children being the Plural list tokens 
             of each list node in ListToksSubset.  P4fsrCondResultSs is updated 
             accordingly.
    *) 

    = PROCEDURE P4fsrFpfVisit ( ListTok : IntSets . ValidElemT ) 
      RAISES { AssertionFailure } 

      = VAR LPluralTok : LbeStd . TokTyp 

      ; BEGIN 
          LPluralTok := ListCardTok ( LangInfo , ListTok , ListCardPlural )
        ; P4fsrFillFrag 
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

  ; PROCEDURE P4fsrFillCases 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; TokSet : TokSetTyp 
      ; CardSet : CardSetTyp
      ; OptionIdSet : LRTable . OptionIdSetTyp 
      )
    RAISES { AssertionFailure } 
    (* PRE: FsNodeRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 
    (* Fill for every case that satifies both TokSet and CardSet. *) 

    = VAR LListSet : TokSetTyp 
    ; VAR LListCard : CARDINAL 
    ; VAR LStarListSet : TokSetTyp 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LFixedTokSet : TokSetTyp 
    ; VAR LFixedTokSetCard : CARDINAL 

    ; BEGIN
        IF FsNodeRef . FsCondDoParse 
        THEN 
          LListSet := ListToksSubset ( LangInfo , TokSet )
        ; LListCard := IntSets . Card ( LListSet ) 
        ; IF LListCard > 0 
          THEN 
            LStarListSet := StarListToksSubset ( LangInfo , LListSet )
          ; LStarListCard := IntSets . Card ( LStarListSet ) 
          ; LFixedTokSet := IntSets . Difference ( TokSet , LListSet )
          ELSE 
            LStarListSet := IntSets . Empty ( ) 
          ; LStarListCard := 0 
          ; LFixedTokSet := TokSet          
          END (* IF *) 
        ; LFixedTokSetCard := IntSets . Card ( LFixedTokSet ) 

        (* Absent. *) 
        ; IF LangUtil . FsEstChildRef ( FsNodeRef ) . FsEstChildIsOptional 
             AND CardTyp . Absent IN CardSet 
          THEN 
            P4fsrFillFrag 
              ( FsNodeRef 
              , PrincipalChildTok := LbeStd . Tok__Empty 
              , OptionIdSet := LRTable . OptionIdSetEmpty 
              ) 

        (* Empty lists. *) 
          ELSIF LStarListCard > 0 AND CardTyp . EmptyList IN CardSet 
          THEN
            P4fsrFillEmptyListFrag 
              ( FsNodeRef 
              , ListTok := IntSets . Minimum ( LStarListSet ) 
              , OptionIdSet := OptionIdSet 
              ) 
          END (* IF *) 

        (* Singleton and plural lists. *) 
        ; IF LListCard > 0 
          THEN 
            IF CardTyp . SingletonList IN CardSet 
            THEN
              P4fsrFillSingletonFrags 
                ( FsNodeRef 
                , LListSet 
                , OptionIdSet := OptionIdSet 
                ) 
            END (* IF *) 
          ; IF CardTyp . PluralList IN CardSet 
            THEN
              P4fsrFillPluralFrags 
                ( FsNodeRef 
                , LListSet 
                , OptionIdSet := OptionIdSet 
                ) 
            END (* IF *) 
          END (* IF *) 

        (* Fixed nodes. *) 
        ; IF LFixedTokSetCard > 0  
          THEN 
(*          P4fsrFillFrag 
              ( FsNodeRef 
              , ClassTokForSet (* SIDE EFFECTS! *) 
                  ( LangInfo , FsNodeRef , LFixedTokSet , MustBePresent := TRUE )
(* TODO: This token could be stored in FsNodeRef . FsCondPredicate . 
         PredicateClass.  Do so and then use it from there.  This would probably
         also allow the MustBePresent case to be eliminated and maybe side
         effects simplified. 
*) 
              , OptionIdSet := OptionIdSet 
              ) 
*) 
            P4fsrFillTokSetFrags ( FsNodeRef , LFixedTokSet , OptionIdSet ) 
          END (* IF *) 
        END (* IF *) 
      END P4fsrFillCases  

  ; PROCEDURE P4fsrFillPredicate 
      ( FsPredNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { AssertionFailure } 
    (* PRE:  P4fsrCondResultRef ^ has enough space, starting at 
             P4fsrCondResultSs, to hold the fragments of the alternative. 
       POST: P4fsrCondResultRef ^ has been filled with the fragments of
             the alternative. P4fsrCondResultSs is incremented accordingly.
    *) 

    = VAR LOrigResultSs : CARDINAL 
    ; VAR LMemberSet : TokSetTyp 
    ; VAR LListSet : TokSetTyp 
    ; VAR LListCard : CARDINAL 
    ; VAR LListTok : LbeStd . TokTyp 
    ; VAR LStarListSet : TokSetTyp 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LOptionIdSet : LRTable . OptionIdSetTyp 

    ; BEGIN (* P4fsrFillPredicate *) 
        IF FsPredNodeRef = NIL 
        THEN 
(* REVIEW: Can this happen? *)
          CantHappen ( AFT . A_P4fsrFillPredicate_NIL_Alternative )  
        ELSE 
          LOrigResultSs := P4fsrCondResultSs 
        ; CASE FsPredNodeRef . FsKind <* NOWARN *> 
          OF FsKindTyp . FsKindCondFmt 
          => CASE <* NOWARN *> 
               FsPredNodeRef . FsCondPredicate . PredicateKind  
            OF PredicateKindTyp . PredicateKindNull 
            => (* Previously detected error. *) 

            | PredicateKindTyp . PredicateKindTrue
            => P4fsrFillCases  
                 ( FsPredNodeRef 
                 , P4fsrTokSet 
                 , P4fsrCardSet 
                 , SingletonOptionIdSet ( FsPredNodeRef . FsOptionIds [ TRUE ] )
                 ) 
            ; P4fsrCardSet := CardSetEmpty 
            ; P4fsrTokSet := IntSets . Empty ( )              

            | PredicateKindTyp . PredicateKindAbsent 
            => IF CardTyp . Absent IN P4fsrCardSet
              THEN (* FsTrees will have ensured that we don't also have
                      the possibility of an EmptyList. 
                      ( Is it FsTrees or Pass1 that does this now?  It may
                        change later to FsTrees. ) 
                   *) 
                P4fsrFillFrag 
                  ( FsPredNodeRef 
                  , PrincipalChildTok := LbeStd . Tok__Empty 
                  , OptionIdSet 
                      := SingletonOptionIdSet 
                           ( FsPredNodeRef . FsOptionIds [ TRUE ] ) 
                  )
              ; P4fsrCardSet := P4fsrCardSet - CardSetAbsent  
              END (* IF *)  

            | PredicateKindTyp . PredicateKindPresent 
            => P4fsrFillCases 
                 ( FsPredNodeRef 
                 , P4fsrTokSet 
                 , P4fsrCardSet - CardSetAbsent   
                 , SingletonOptionIdSet ( FsPredNodeRef . FsOptionIds [ TRUE ] )
                 )  
            ; P4fsrTokSet := IntSets . Empty ( ) 

            | PredicateKindTyp . PredicateKindEmptyList 
            => IF CardTyp . EmptyList IN P4fsrCardSet 
              THEN
                LStarListSet 
                  := StarListToksSubset ( LangInfo , P4fsrTokSet ) 
              ; LStarListCard := IntSets . Card ( LStarListSet ) 
              ; IF LStarListCard > 0  
                THEN 
                  LListTok := IntSets . Minimum ( LStarListSet ) 
                  (* ^If LStarListCard > 1, it's ambiguous and was warned-about
                      in pass 1. Just arbitrarily choose one list, but it must
                      be the same one as chosen in pass 1. 
                  *) 
                ; P4fsrFillEmptyListFrag 
                    ( FsPredNodeRef 
                    , LListTok 
                    , SingletonOptionIdSet 
                        ( FsPredNodeRef . FsOptionIds [ TRUE ] )
                    ) 
             (* ELSE some previously detected error. *) 
                END (* IF *) 

              ; P4fsrCardSet := P4fsrCardSet - CardSetEmptyList
              ; IF P4fsrCardSet = CardSetEmpty 
                THEN (* Remove all the list tokens. *)
                  P4fsrTokSet 
                    := IntSets . Difference ( P4fsrTokSet , LStarListSet )
                END (* IF *) 
              END (* IF *) 

            | PredicateKindTyp . PredicateKindNonemptyList
            => IF CardTyp . SingletonList IN P4fsrCardSet 
                  OR CardTyp . PluralList IN P4fsrCardSet 
              THEN
                LListSet := ListToksSubset ( LangInfo , P4fsrTokSet )
              ; LListCard := IntSets . Card ( LListSet ) 
              ; LOptionIdSet 
                  := SingletonOptionIdSet 
                       ( FsPredNodeRef . FsOptionIds [ TRUE ] )
              ; IF LListCard > 0 
                THEN 
                  IF CardTyp . SingletonList IN P4fsrCardSet 
                  THEN  
                    P4fsrFillSingletonFrags 
                      ( FsPredNodeRef , LListSet , LOptionIdSet ) 
                  END (* IF *) 
                ; IF CardTyp . PluralList IN P4fsrCardSet 
                  THEN  
                    P4fsrFillPluralFrags 
                      ( FsPredNodeRef , LListSet , LOptionIdSet ) 
                  END (* IF *) 
                END (* IF *) 
              ; P4fsrCardSet 
                  := P4fsrCardSet 
                     - CardSetTyp 
                         { CardTyp . SingletonList , CardTyp . PluralList } 
              ; IF P4fsrCardSet = CardSetEmpty 
                THEN (* Remove all the list tokens. *)
                  P4fsrTokSet := IntSets . Difference ( P4fsrTokSet , LListSet )
                END (* IF *) 
              END (* IF *)  

            | PredicateKindTyp . PredicateKindNonpluralList 
            => IF CardTyp . EmptyList IN P4fsrCardSet 
                  OR CardTyp . SingletonList IN P4fsrCardSet 
              THEN
                LListSet := ListToksSubset ( LangInfo , P4fsrTokSet )
              ; LListCard := IntSets . Card ( LListSet ) 
              ; LOptionIdSet 
                  := SingletonOptionIdSet 
                       ( FsPredNodeRef . FsOptionIds [ TRUE ] ) 
              ; IF LListCard > 0 
                THEN 
                  LStarListSet := StarListToksSubset ( LangInfo , LListSet )
                ; LStarListCard := IntSets . Card ( LStarListSet ) 
                ; IF CardTyp . EmptyList IN P4fsrCardSet 
                     AND LStarListCard > 0  
                  THEN 
                    LListTok := IntSets . Minimum ( LStarListSet ) 
                  ; P4fsrFillEmptyListFrag 
                      ( FsPredNodeRef , LListTok , LOptionIdSet ) 
                  END (* IF *) 
                ; IF CardTyp . SingletonList IN P4fsrCardSet 
                  THEN
                    P4fsrFillSingletonFrags 
                      ( FsPredNodeRef , LListSet , LOptionIdSet ) 
                  END (* IF *) 
                END (* IF *) 
              ; P4fsrCardSet 
                  := P4fsrCardSet 
                     - CardSetTyp 
                         { CardTyp . EmptyList , CardTyp . SingletonList } 
              ; IF P4fsrCardSet = CardSetEmpty 
                THEN (* Remove all the list tokens. *)
                  P4fsrTokSet := IntSets . Difference ( P4fsrTokSet , LListSet )
                END (* IF *) 
              END (* IF *) 

            | PredicateKindTyp . PredicateKindPluralList 
            => IF CardTyp . PluralList IN P4fsrCardSet 
              THEN
                LListSet := ListToksSubset ( LangInfo , P4fsrTokSet )
              ; LListCard := IntSets . Card ( LListSet ) 
              ; IF LListCard > 0 
                THEN 
                  P4fsrFillPluralFrags 
                    ( FsPredNodeRef 
                    , LListSet 
                    , SingletonOptionIdSet 
                        ( FsPredNodeRef . FsOptionIds [ TRUE ] ) 
                    ) 
                END (* IF *) 
              ; P4fsrCardSet 
                  := P4fsrCardSet - CardSetTyp { CardTyp . PluralList } 
              ; IF P4fsrCardSet = CardSetEmpty 
                THEN (* Remove all the list tokens. *)
                  P4fsrTokSet := IntSets . Difference ( P4fsrTokSet , LListSet )
                END (* IF *) 
              END (* IF *)  

            | PredicateKindTyp . PredicateKindInClass 
            => LMemberSet 
                 := LdlSemantics . AsTokSetOfTok 
                      ( LangInfo 
                      , FsPredNodeRef . FsCondPredicate . PredicateClass 
                      )
            ; LMemberSet := IntSets . Intersection ( LMemberSet , P4fsrTokSet )
(* FIX:  Someday, Ldl[0|1]FsTrees should compute this intersection, if empty,
       warn and delete the alternative, or store the intersection in
       FsPredNodeRef . FsCondPredicate . PredicateClass.
*) 
            ; P4fsrFillCases 
                 ( FsPredNodeRef 
                 , LMemberSet 
                 , P4fsrCardSet - CardSetAbsent   
                 , SingletonOptionIdSet 
                     ( FsPredNodeRef . FsOptionIds [ TRUE ] ) 
                 )  
            ; P4fsrTokSet := IntSets . Difference ( P4fsrTokSet , LMemberSet )

            END (* CASE PredicateKind *) 

          | FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill
          , FsKindTyp . FsKindEstChildOfFixed
          , FsKindTyp . FsKindEstChildOfList
          => P4fsrFillCases  
               ( FsPredNodeRef 
               , P4fsrTokSet 
               , P4fsrCardSet 
               , SingletonOptionIdSet ( FsPredNodeRef . FsOptionIds [ TRUE ] )
               ) 
          ; P4fsrCardSet := CardSetEmpty 
          ; P4fsrTokSet := IntSets . Empty ( )              

          | FsKindTyp . FsKindNull 
          , FsKindTyp . FsKindBegOfImage
          , FsKindTyp . FsKindEndOfImage 
          , FsKindTyp . FsKindLineBreakReqd
          , FsKindTyp . FsKindLineBreakOpt
          => (* Ignore these. *) 

          END (* CASE FsKind *)  
        ; Assert 
            ( P4fsrCondResultSs - LOrigResultSs = FsPredNodeRef . FsFragCt 
            , AFT . A_P4fsrFillPredicate_Result_count_mismatch 
            ) 
        END (* IF *) 
      END P4fsrFillPredicate 

  ; PROCEDURE P4fsrCondConstruct 
      ( FsCondFmtNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR MaxFragLen : CARDINAL 
      ) 
    : FragSetRefTyp 
    RAISES { AssertionFailure } 
    (* Also (probably) works for a principal child not inside any conditional. *) 
(* TODO^ But we have a separate procedure for that.  Resolve. *) 

    = VAR LEstChildFsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR LPredFsNodeRef : LangUtil . FsNodeRefTyp 

    ; BEGIN (* P4fsrCondConstruct *) 
        LEstChildFsNodeRef := LangUtil . FsEstChildRef ( FsCondFmtNodeRef )  

      (* Initialize the cardinality and token sets. *) 
      ; P4fsrCardSet := LangUtil . CardSetUniverse
      ; IF NOT LangUtil . FsEstChildRef 
                 ( FsCondFmtNodeRef ) . FsEstChildIsOptional
        THEN 
          P4fsrCardSet := P4fsrCardSet - CardSetAbsent  
        END (* IF *) 
      ; P4fsrTokSet 
          := LdlSemantics . AsTokSetOfTok 
               ( LangInfo , LEstChildFsNodeRef . FsTok ) 

      (* Count total fragments and allocate a result array for the
         conditional construct. 
      *) 
      ; P4fsrCondResultRef 
          := NEW ( FragSetRefTyp 
                 , AltCountForCondConstruct ( LangInfo , FsCondFmtNodeRef ) 
                 ) 
      ; P4fsrCondResultSs := 0 
      ; P4fsrCondMaxFragLen := 0

      (* Traverse the alternatives. *) 
      ; LPredFsNodeRef := FsCondFmtNodeRef 
      ; WHILE LPredFsNodeRef # NIL 
        DO
          P4fsrFillPredicate ( LPredFsNodeRef )
        ; LPredFsNodeRef := LPredFsNodeRef . FsCondAltRef 
        END (* WHILE *) 

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
    RAISES { AssertionFailure } 
    (* Used only for a principal child that is not inside a conditional
       construct.  It could optional though (a different kind of conditional) 
       and still have multiple fragments.
    *) 

    = VAR LResultCt : CARDINAL 
    ; VAR LTokSet : TokSetTyp 
    ; VAR LFixedTokSet : TokSetTyp 
    ; VAR LFixedTokSetCard : CARDINAL 
    ; VAR LListCard : CARDINAL 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LListSet : TokSetTyp 
    ; VAR LStarListSet : TokSetTyp 
    ; VAR LTok : LbeStd . TokTyp 
    ; VAR LOptionIdSet : LRTable . OptionIdSetTyp 

    ; BEGIN
        LResultCt := 0  
      ; LTokSet 
          := LdlSemantics . AsTokSetOfTok 
               ( LangInfo , FsChildNodeRef . FsTok ) 
      ; INC ( LResultCt , ORD ( FsChildNodeRef . FsEstChildIsOptional ) ) 
      ; LListSet := ListToksSubset ( LangInfo , LTokSet )
      ; LListCard := IntSets . Card ( LListSet ) 
      ; IF LListCard > 0 
        THEN 
          LStarListSet := StarListToksSubset ( LangInfo , LListSet )
        ; LStarListCard := IntSets . Card ( LStarListSet ) 
        ; INC ( LResultCt 
              , ORD ( LStarListCard > 0 
                      AND NOT FsChildNodeRef . FsEstChildIsOptional 
                    ) 
              ) 
        ; INC ( LResultCt , LListCard * 2 ) (* Singleton and Plural cases. *) 
        ; LFixedTokSet := IntSets . Difference ( LTokSet , LListSet )
        ELSE 
          LStarListSet := IntSets . Empty ( ) 
        ; LStarListCard := 0 
        ; LFixedTokSet := LTokSet 
        END (* IF *) 
      ; LFixedTokSetCard := IntSets . Card ( LFixedTokSet ) 
      ; INC ( LResultCt , ORD ( LFixedTokSetCard > 0 ) )                  

      ; P4fsrCondResultRef := NEW ( FragSetRefTyp , LResultCt  ) 
      ; P4fsrCondResultSs := 0 
      ; P4fsrCondMaxFragLen := 0
      ; LOptionIdSet 
          := SingletonOptionIdSet 
               ( FsChildNodeRef . FsOptionIds [ TRUE ] ) 

      ; IF FsChildNodeRef . FsEstChildIsOptional 
        THEN 
          P4fsrFillFrag 
            ( FsChildNodeRef 
            , PrincipalChildTok := LbeStd . Tok__Empty 
            , OptionIdSet := LRTable . OptionIdSetEmpty 
            ) 
        ELSIF LStarListCard > 0 
        THEN
          P4fsrFillEmptyListFrag 
            ( FsChildNodeRef 
            , IntSets . Minimum ( LStarListSet ) 
            , LOptionIdSet 
            ) 
        END (* IF *) 
      ; P4fsrFillSingletonFrags 
          ( FsChildNodeRef , LListSet , LOptionIdSet ) 
      ; P4fsrFillPluralFrags 
          ( FsChildNodeRef , LListSet , LOptionIdSet ) 
      ; IF LFixedTokSetCard > 0  
        THEN 
(*        LTok 
            := ClassTokForSet (* SIDE EFFECTS! *) 
                 ( LangInfo 
                 , FsChildNodeRef 
                 , LFixedTokSet 
                 , MustBePresent := TRUE 
                 )
(* TODO: ^This could be stored in FsChildNodRef . FsTok.
         Use it from there.
*) 
        ; P4fsrFillFrag ( FsChildNodeRef , LTok , LOptionIdSet )  
*) 
          P4fsrFillTokSetFrags ( FsChildNodeRef , LFixedTokSet , LOptionIdSet ) 
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
    RAISES { AssertionFailure } 
    (* Called only when outside a conditional construct. *) 

    = VAR P4fsrFssChildrensFragSets : FragSetSetRefTyp 
    ; VAR P4fsrFssProductFrag : LRTable . TokArrayRefTyp 
    ; VAR P4fsrFssProductFragSet : FragSetRefTyp 
    ; VAR P4fsrFssNextFragSetSs : PortTypes . Int32Typ  
    ; VAR P4fsrFssChildCt : PortTypes . Int32Typ 
    ; VAR P4fsrFssNextProductSs : INTEGER  

    ; PROCEDURE P4fsrFssRecurse 
        ( FragSetNo : INTEGER  
        ; StartPos : PortTypes . Int32Typ 
        ; OptionIdSet : LRTable . OptionIdSetTyp 
        ) 

      = VAR LFragLen : PortTypes . Int32Typ 
      ; VAR LNewFrag : LRTable . TokArrayRefTyp 

      ; BEGIN 
          IF FragSetNo >= P4fsrFssChildCt  
          THEN (* End of concatenation *) 
            (* Always copy P4fsrFssProductFrag, because it could be altered 
               later. 
            *) 
(* TODO: Eliminate duplicate fragments in this set.  This no doubt means 
         the AS/FS is format-ambiguous, so give error if so.
*) 
            LNewFrag := NEW ( LRTable . TokArrayRefTyp , StartPos ) 
          ; LNewFrag ^ := SUBARRAY (  P4fsrFssProductFrag ^ , 0 , StartPos )
          ; WITH WFrag = P4fsrFssProductFragSet ^ [ P4fsrFssNextProductSs ] 
            DO 
              WFrag . TokStringRef := LNewFrag 
            ; WFrag . OptionIdSet := OptionIdSet  
            END (* WITH *) 
          ; INC ( P4fsrFssNextProductSs )  
          ; MaxFragLen := MAX ( MaxFragLen , StartPos ) 
          ELSE (* Do more concatenation. *) 
            WITH WChildFragSet = P4fsrFssChildrensFragSets [ FragSetNo ] 
            DO 
              FOR RI := 0 TO NUMBER ( WChildFragSet ^ ) - 1  
              DO WITH WFrag = WChildFragSet [ RI ]  
                DO 
                  LFragLen := NUMBER ( WFrag . TokStringRef ^ ) 
                ; SUBARRAY ( P4fsrFssProductFrag ^ , StartPos , LFragLen ) 
                    := WFrag .TokStringRef ^ 
                ; P4fsrFssRecurse 
                    ( FragSetNo + 1 
                    , StartPos + LFragLen 
                    , OptionIdSet + WFrag . OptionIdSet 
                    ) 
                END (* WITH *)  
              END (* FOR *) 
            END (* WITH *) 
          END (* IF *) 
        END P4fsrFssRecurse 

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
        ; P4fsrFssChildCt := NUMBER ( FsSubtreeNodeRef . FsChildren ^ ) 
        ; P4fsrFssChildrensFragSets 
            := NEW 
                 ( FragSetSetRefTyp , P4fsrFssChildCt (* Maybe extravagant. *) )
        ; P4fsrFssNextFragSetSs := P4fsrFssChildCt  
        ; LProductCt := 1 
        ; LProductMaxFragLen := 0  

        (* Traverse the children. *)
        ; FOR RI := P4fsrFssChildCt - 1 TO 0 BY - 1 
          (* Do them in RtoL temporal order, and store the results spatially
             RtoL in P4fsrFssChildrensFragSets, so it reads normally. 
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
             denotes a singleton set whose one member is the empty string. 
             Furthermore, we can put such a fragment set into the
             concatenated set by just omitting it entirely from the
             operation.  On the other hand, a pointer to an array with zero 
             NUMBER really would mean an empty fragment set.  
          *) 
(* TODO: Only one copy of singleton fragset, empty string. *)
          ; IF LChildFragSet # NIL 
            THEN  
              LChildFragCt := NUMBER ( LChildFragSet ^ ) 
            ; IF LChildFragCt > 0 
              THEN  
                DEC ( P4fsrFssNextFragSetSs ) 
              ; P4fsrFssChildrensFragSets ^ [ P4fsrFssNextFragSetSs ] 
                  := LChildFragSet 
              ; LProductCt := LProductCt * LChildFragCt 
              ; INC ( LProductMaxFragLen , LMaxFragLen ) 
              ELSE (* Empty set.  The whole result will be empty too. *) 
                RETURN LChildFragSet 
              END (* IF *) 
            END (* IF *) 
          END (* FOR *) 

        (* Regular-expression-like concatenation. *) 
        ; IF P4fsrFssNextFragSetSs = P4fsrFssChildCt  
          THEN (* If all children format to the empty string, we want to return
                  a singleton fragment set containing the empty string.  We
                  use a NIL fragment set to represent this, since it is a
                  common case, whereas empty sets never (seldom?) get returned.
               *) 
(* TODO: Only one copy of singleton fragset, empty string. 
         Note, it has empty OptionIdSet here. 
*) 
            RETURN NIL 
          ELSE 
            P4fsrFssProductFragSet := NEW ( FragSetRefTyp , LProductCt )
          ; P4fsrFssNextProductSs := 0  
          ; P4fsrFssProductFrag 
              := NEW ( LRTable . TokArrayRefTyp , LProductMaxFragLen ) 
          ; P4fsrFssRecurse 
              ( FragSetNo := P4fsrFssNextFragSetSs   
              , StartPos := 0 
              , OptionIdSet := LRTable . OptionIdSetEmpty 
              ) 
          ; IF P4fsrFssNextProductSs = LProductCt 
            THEN LResult := P4fsrFssProductFragSet 
            ELSE 
              LResult := NEW ( FragSetRefTyp , P4fsrFssNextProductSs )
            ; LResult ^ 
                := SUBARRAY 
                     ( P4fsrFssProductFragSet ^ , 0 , P4fsrFssNextProductSs ) 
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
    RAISES { AssertionFailure } 
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
        => (* Ignore these. *) 
          MaxFragLen := 0 
        ; RETURN NIL 
          (* This really means a singleton fragment set with empty string. *)  
(* TODO: Only one copy of singleton fragset, empty string. 
         Note, it has empty OptionIdSet here. 
*) 

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
    RAISES { AssertionFailure } 

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
        THEN (* Singleton FragSet, empty string. *) 
(* TODO: Only one copy of singleton fragset, empty string. 
         Note, it has empty OptionIdSet here. 
*) 
          Automaton . AddProduction 
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
              Automaton . AddProduction 
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

  ; PROCEDURE P4fsrListRule ( FsListRuleRef : LangUtil . FsNodeRefTyp ) 
    RAISES { AssertionFailure } 

    = VAR LElemFragSet : FragSetRefTyp
    ; VAR LFragsetCt : CARDINAL 
    ; VAR LMaxFragLen : CARDINAL 
    ; VAR LSepCount : CARDINAL 
    ; VAR LRightNextSs : CARDINAL 
    ; VAR LRight1 : LRTable . TokArrayRefTyp 
    ; VAR LRight2 : LRTable . TokArrayRefTyp 
    ; VAR LFragLen : CARDINAL 
    ; VAR LOptionId : LRTable . OptionIdTyp 

    ; BEGIN (* P4fsrListRule *) 
        TYPECASE 
          LdlSemantics . SemDeclOfTok ( LangInfo , FsListRuleRef . FsTok ) 
        OF NULL => (* A previously reported error. *)

        | LdlSemantics . SemDeclAsListNodeTyp ( TSemDeclAsList ) 
        => (* Do the Est list child. *) 
          LOptionId := 0 
        ; LElemFragSet 
            := P4fsrUncondFsChild 
                 ( FsListRuleRef . FsChildren ^ [ 0 ] 
                 , (* VAR, dead. *) LMaxFragLen 
                 , (* IN OUT *) LOptionId 
                 ) 
        ; IF LElemFragSet = NIL 
          THEN (* A list element can derive empty if there are separators
                  and still be unambiguous. 
               *)  
            LElemFragSet := SingletonFragset ( NIL ) 
(* TODO: Only one copy of singleton fragset, empty string. 
         Note, it has empty OptionIdSet here. 
*) 
            (* ^Handle this special case by making it non-special. *) 
          END (* IF *) 
        ; IF FsListRuleRef . FsEmptyListTok # LbeStd . Tok__Null 
          THEN
          (* Add EmptyListTok BUILD ListTok ::= <empty> *) 
            Automaton . AddProduction 
              ( Gram 
              , Left := TSemDeclAsList . ListCardToks [ ListCardEmpty ] 
              , Right := LRTable . EmptyString ( )  
              , BuildTok := FsListRuleRef . FsTok  
              ) 
          END (* IF *) 
        ; LFragsetCt := NUMBER ( LElemFragSet ^ ) 
        ; IF LFragsetCt > 0 
          THEN 
            IF FsListRuleRef . FsSingletonListTok # LbeStd . Tok__Null 
            THEN 

            (* Add SingletonTok BUILD ListTok ::= Alt, for all alternatives. *)
              FOR RI := 0 TO LFragsetCt - 1 
              DO
                WITH WFrag = LElemFragSet ^ [ RI ] 
                DO  
                  Automaton . AddProduction 
                    ( Gram 
                    , Left 
                        := TSemDeclAsList . ListCardToks [ ListCardSingleton ]
                    , Right := WFrag . TokStringRef 
                    , BuildTok := FsListRuleRef . FsTok  
                    , OptionIdSet := WFrag . OptionIdSet 
                    ) 
                END (* WITH *) 
              END (* FOR *) 
            END (* IF *) 

          ; IF FsListRuleRef . FsPluralListTok # LbeStd . Tok__Null 
            THEN 

            (* Add PartialTok ::= SublistTok *) 
              Automaton . AddProduction 
                ( Gram 
                , Left := FsListRuleRef . FsPartialTok 
                , Right
                    := LRTable . SingletonString 
                         ( FsListRuleRef . FsSublistTok )
                , BuildTok := LbeStd . Tok__Null 
                ) 

            (* Add PluralTok BUILD ListTok ::= SublistTok *) 
            ; Automaton . AddProduction 
                ( Gram 
                , Left := TSemDeclAsList . ListCardToks [ ListCardPlural ] 
                , Right
                    := LRTable . SingletonString 
                         ( FsListRuleRef . FsSublistTok )
                , BuildTok := FsListRuleRef . FsTok  
                ) 

            (* Add PartialTok ::= Alt, for all alternatives. *) 
            ; FOR RI := 0 TO LFragsetCt - 1 
              DO
                WITH WFrag = LElemFragSet ^ [ RI ] 
                DO  
                  Automaton . AddProduction 
                    ( Gram 
                    , Left := FsListRuleRef . FsPartialTok  
                    , Right := WFrag . TokStringRef  
                    , BuildTok := LbeStd . Tok__Null 
                    , OptionIdSet := WFrag . OptionIdSet 
                    ) 
                END (* WITH *) 
              END (* FOR *) 

            (* Count the separators. *)           
            ; LSepCount := 0 
            ; FOR RI := 1 TO NUMBER ( FsListRuleRef . FsChildren ^ ) - 1 
              DO
                INC ( LSepCount 
                    , ORD ( FsListRuleRef . FsChildren ^ [ RI ] . FsKind  
                            = FsKindTyp . FsKindInsTok 
                          ) 
                    ) 
              END (* FOR *) 

            (* Add PluralTok BUILD ListTok 
                     ::= SublistTok <separators> PartialTok 
            *) 
            ; LRight1 := NEW ( LRTable . TokArrayRefTyp , 1 + LSepCount + 1 ) 
            ; LRight1 ^ [ 0 ] := FsListRuleRef . FsSublistTok 
            ; LRightNextSs := 1  
            ; FOR RI := 1 TO NUMBER ( FsListRuleRef . FsChildren ^ ) - 1 
              DO  
                IF FsListRuleRef . FsChildren ^ [ RI ] . FsKind  
                   = FsKindTyp . FsKindInsTok 
                THEN 
                  LRight1 ^ [ LRightNextSs ] 
                    := FsListRuleRef . FsChildren ^ [ RI ] . FsTok  
                ; INC ( LRightNextSs ) 
                END (* IF *) 
              END (* FOR *) 
            ; Assert 
                ( LRightNextSs = 1 + LSepCount 
                , AFT . A_Right_subscript_mismatch_1
                ) 
            ; LRight1 ^ [ LSepCount + 1 ] := FsListRuleRef . FsPartialTok 
            ; Automaton . AddProduction 
                ( Gram 
                , Left := TSemDeclAsList . ListCardToks [ ListCardPlural ] 
                , Right := LRight1 
                , BuildTok := FsListRuleRef . FsTok  
                ) 

            (* Add PartialTok ::= SublistTok <separators> PartialTok *) 
            ; Automaton . AddProduction 
                ( Gram 
                , Left := FsListRuleRef . FsPartialTok  
                , Right := LRight1
                , BuildTok := LbeStd . Tok__Null 
                ) 

            (* Add PluralTok BUILD ListTok ::= Alt <separators> PartialTok 
               and PartialTok              ::= Alt <separators> PartialTok, 
               each for all alternatives. 
            *) 
(* CONSIDER: Is it worth the trouble to atomize LRight2? If so, use the 
             mechanism on LRight1 also, just in case it helps. 
*) 
            ; FOR RI := 0 TO LFragsetCt - 1 
              DO 
                WITH WFrag = LElemFragSet ^ [ RI ] 
                DO 
                  LFragLen := NUMBER ( WFrag . TokStringRef ^ ) 
                ; LRight2 
                    := NEW 
                         ( LRTable . TokArrayRefTyp , LFragLen + LSepCount + 1 )
                ; SUBARRAY ( LRight2 ^ , 0 , LFragLen ) 
                    := WFrag . TokStringRef ^ 
                ; SUBARRAY ( LRight2 ^ , LFragLen , LSepCount ) 
                    := SUBARRAY ( LRight1 ^ , 1 , LSepCount ) 
                ; LRight2 ^ [ LFragLen + LSepCount ] 
                   := FsListRuleRef . FsPartialTok 
                ; Automaton . AddProduction 
                    ( Gram 
                    , Left := TSemDeclAsList . ListCardToks [ ListCardPlural ] 
                    , Right := LRight2 
                    , BuildTok := FsListRuleRef . FsTok  
                    , OptionIdSet := WFrag . OptionIdSet 
                    ) 
                ; Automaton . AddProduction 
                    ( Gram 
                    , Left := FsListRuleRef . FsPartialTok  
                    , Right := LRight2
                    , BuildTok := LbeStd . Tok__Null 
                    , OptionIdSet := WFrag . OptionIdSet 
                    ) 
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
        , FsKindTyp . FsKindEstListTrailHoriz  
        , FsKindTyp . FsKindEstListTrailVert  
        , FsKindTyp . FsKindEstListTrailFill   
        => P4fsrListRule ( FsRuleNodeRef )

        ELSE 
        END (* CASE *) 
      END (* IF *) 
    END Pass4FsRule  

; PROCEDURE GenClassProds 
    ( LangInfo : LangInfoRefTyp 
    ; Gram : GrammarSubTyp 
    ) 
  RAISES { AssertionFailure }
  (* Generate productions for all classes in ClassTable. *)  

(* TODO: This largely duplicates GenClassTokProds and
         LdlSemantics.GenNonclassTokSetProductions.  Unify.
*) 

  = VAR GcpClassInfoRef : ClassInfo . ClassInfoRefTyp 

  ; PROCEDURE GcpVisitTok ( RhsTok : IntSets . ValidElemT ) 
    RAISES { AssertionFailure } 
  
    = BEGIN 
        Automaton . AddProduction 
          ( Gram 
          , Left := GcpClassInfoRef ^ . ClassTok 
          , Right := LRTable . SingletonString ( RhsTok ) 
          ) 
      END GcpVisitTok 

  ; VAR LIterator : SetClassInfoTbl . Iterator 
  ; VAR LSet : TokSetTyp 

  ; BEGIN (* GenClassProds *) 
      LIterator := LangInfo . ClassTable . iterate ( ) 
    ; WHILE LIterator . next ( LSet , GcpClassInfoRef ) 
      DO
        IF GcpClassInfoRef ^ . UseCount > 0 
        THEN 
          <* FATAL ANY *> BEGIN 
            IntSets . ForAllDo ( LSet , GcpVisitTok ) 
          END (* Block *) 
        ; Automaton . NoteNontermKind 
            ( Gram 
            , GcpClassInfoRef ^ . ClassTok 
            , NontermKindTyp . NtkConcreteOnly 
            )
        END (* IF *) 
      END (* WHILE *) 
    END GenClassProds 

; PROCEDURE BetweenPass2And3
    ( LangInfo : LangInfoRefTyp ; Gram : GrammarSubTyp ) 
  (* Between pass 2 and pass 3:
       1) Expand the token map to hold generated CS tokens.
       2) Initialize the LALR automaton.
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
        , ProdCt := LangInfo . GenProductions 
        , FirstTerminal := LbeStd . Tok__BegOfImage 
        , LastTerminal := LangInfo . VarTermToks - 1 
        , FirstAstNonterminal := LangInfo . VarTermModToks 
        , LastAstNonterminal := LangInfo . AsListCardToks - 1 
        , FirstNonterminal := LangInfo . AsListCardToks   
(* TODO: Rework all the LALR stuff so it doesn't try to work on
       tokens in the range AsPartialToks .. AsCsClassToks - 1, 
       (which are ClassOnly tokens), so we can take advantage of the fact 
       that they aren't used in the concrete syntax, and shorten the
       table NNextRef on the left.
*) 
        , LastNonterminal := LangInfo . GcsChildToks - 1 
        , StartSymbol := LangInfo . StartTok 
        ) 
    ; LdlSemantics . NoteStandardNontermKinds ( LangInfo , Gram ) 
    ; LdlSemantics . NoteNontermKindForRange 
        ( Gram 
        , LangInfo . VarTermModToks 
        , LangInfo . AsStarToks 
        , NontermKindTyp . NtkAbstractOnly 
        ) 
    ; LdlSemantics . NoteNontermKindForRange 
        ( Gram 
        , LangInfo . AsSublistToks 
        , LangInfo . AsListCardToks 
        , NontermKindTyp . NtkConcreteOnly   
        ) 
    ; LdlSemantics . NoteNontermKindForRange 
        ( Gram 
        , LangInfo . AsListCardToks 
        , LangInfo . AsPartialToks 
        , NontermKindTyp . NtkPartial 
        ) 
    ; LdlSemantics . NoteNontermKindForRange 
        ( Gram 
        , LangInfo . AsClassOnlyToks 
        , LangInfo . CsFixedToks 
        , NontermKindTyp . NtkUnused 
        ) 
    END BetweenPass2And3

(* VISIBLE: *) 
; PROCEDURE Generate ( LangInfo : LangInfoRefTyp ) 
  RAISES { AssertionFailure } 

  = VAR LGram : GrammarSubTyp  
  ; VAR LMessageCt : PortTypes . Card32Typ  

  ; BEGIN (* Block *) 
      IF Messages . MessageCount ( MessageCodes . KindTyp . MkError ) = 0 
      THEN 
(*TODO: Fix this so it doesn't suppress CS generation if there are error
        messages, but only from processing the handwritten CS. 
*) 
        LGram := NEW ( GrammarSubTyp ) 
      ; LGram . LangInfo := LangInfo 
      ; LGram . IsGenerated := TRUE   
      ELSE LGram := NIL 
      END (* IF *) 
    ; LangInfo . GenGram := LGram 
    ; LMessageCt 
        := Messages . MessageCount ( MessageCodes . KindTyp . MkError ) 
    ; PreloadNamedClasses ( LangInfo , LGram ) 

    (* Pass 1 over the FS rules: *) 

    ; LangInfo . GcsChildToks := LangInfo . CsFixedToks
    ; LangInfo . GenProductions 
        := ClassTokProdCt ( LangInfo , LangInfo . StartTok )      

    (* For all FS rules. *) 
    ; FOR RTok := LangInfo . VarTermModToks TO LangInfo . AsFixedToks - 1 
      DO Pass1FsRule 
           ( LangInfo 
           , LangInfo . FsTreeMapRef ^ [ RTok - FsTreeMapBias ]
           ) 
      END (* FOR *) 

    ; IF Messages . MessageCount ( MessageCodes . KindTyp . MkError ) 
         = LMessageCt  
      THEN 

      (* Pass 2, over list rules only: *) 
        FOR RTok := LangInfo . VarTermModToks TO LangInfo . AsStarToks - 1 
        DO Pass2FsRule 
             ( LangInfo 
             , LGram
             , LangInfo . FsTreeMapRef ^ [ RTok - FsTreeMapBias ]
             ) 
        END (* FOR *) 

      ; BetweenPass2And3 ( LangInfo , LGram ) 

      (* Between pass 3 and pass 4: *) 

      ; GenClassTokProds ( LangInfo , LangInfo . StartTok )      

      (* Pass 4 over the FS rules: *) 

      (* For all FS rules. *) 
      ; FOR RTok := LangInfo . VarTermModToks TO LangInfo . AsFixedToks - 1 
        DO Pass4FsRule 
             ( LangInfo 
             , LGram 
             , LangInfo . FsTreeMapRef ^ [ RTok - FsTreeMapBias ]
             ) 
        END (* FOR *) 
      ; IF Messages . MessageCount ( MessageCodes . KindTyp . MkError ) 
           = LMessageCt  
        THEN 

        (* Finish up. *) 
          GenClassProds ( LangInfo , LGram ) 
        ; LdlSemantics . FinishLalrGeneration 
            ( LangInfo , LGram , LangInfo . Productions ) 
        END (* IF *) 
      END (* IF *) 
    END Generate 

; BEGIN (* GrammarGen2 *)
  END GrammarGen2
. 
