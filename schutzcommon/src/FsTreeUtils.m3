
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE FsTreeUtils 

(* Common code that works on FsTrees.  Common to both Ldl0 and Ldl1. *)  

; IMPORT Fmt 

; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout   
; IMPORT EstHs  
; IMPORT IntSets 
; IMPORT LdlSemantics 
; FROM LdlSemantics 
  IMPORT DeclKindTyp , FsTreeMapBias , LangInfoRefTyp
; IMPORT LbeStd 
; FROM LbeStd IMPORT TokClassTyp 
; IMPORT LangUtil 
; FROM LangUtil 
   IMPORT FsKindTyp , PredicateKindTyp 
   , CardSetTyp , CardSetEmpty , CardSetAbsent , CardSetList , CardSetEmptyList
   , CardSetNonemptyList , CardSetPluralList , CardSetNonpluralList 
; IMPORT LangUtilLo 
; IMPORT LRTable 
; IMPORT MessageCodes 
; FROM Messages IMPORT TextOnly , SemError 
; IMPORT TokRelation 
; IMPORT UncertainBool 

; CONST UbUnknown = UncertainBool . T . Unknown 
; CONST UbFalse = UncertainBool . T . False 
; CONST UbTrue = UncertainBool . T . True  

; TYPE TokSetTyp = IntSets . T 

; TYPE CardTyp = LangUtil . CardTyp 
; CONST ListCardEmpty = LdlSemantics . ListCardTyp . ListCardEmpty 
; CONST ListCardSingleton = LdlSemantics . ListCardTyp . ListCardSingleton 
; CONST ListCardPlural = LdlSemantics . ListCardTyp . ListCardPlural 

; CONST OptRequired = LangUtil . ChildOptTyp . OptRequired 
; CONST OptOptional = LangUtil . ChildOptTyp . OptOptional
; CONST OptAbsent = LangUtil . ChildOptTyp . OptAbsent 
; CONST OptPresent = LangUtil . ChildOptTyp . OptPresent 

; TYPE AFT = MessageCodes . T 

; EXCEPTION Done 

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

; PROCEDURE ComputeFsAbsentEstChildFormatsEmpty 
    ( <* UNUSED *> LangInfo : LangInfoRefTyp 
    ; FsAltNodeRef : LangUtil . FsNodeRefTyp 
    )
  RAISES { Backout } 
  (* FsAltNodeRef is an alternative node.  Compute its 
     FsAbsentEstChildFormatsEmpty, i.e., does it format empty when its Est
     child is absent.  Do this even if the Est child is Required.  
  *) 

  = PROCEDURE CfsaRecurse ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Done , Backout } 
    (* PRE: FsNodeRef is an alternative or any descendant thereof. *) 

    = BEGIN 
        CASE FsNodeRef . FsKind 
        OF FsKindTyp . FsKindNull 
        , FsKindTyp . FsKindLineBreakReqd
        , FsKindTyp . FsKindLineBreakOpt
        => (* Ignore these. *) 

        | FsKindTyp . FsKindEstChildOfFixed 
        , FsKindTyp . FsKindEstChildOfList 
        => (* Treat these as absent. *) 

        | FsKindTyp . FsKindInsTok 
        => FsNodeRef . FsAbsentEstChildFormatsEmpty := FALSE 
          ; RAISE Done 

        | FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeHoriz 
        , FsKindTyp . FsKindSubtreeFill 
        , FsKindTyp . FsKindCondFmt (* Happens only at top level. *)  
        => (* Traverse these too. *) 
            FOR RFsChildNo := 0 TO NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
            DO CfsaRecurse ( FsNodeRef . FsChildren ^ [ RFsChildNo ] ) 
            END (* FOR *) 

        ELSE CantHappen ( AFT . A_FsTreeUtils_CfsaRecurse_Bad_FsKind ) 
        END (* CASE *) 
      END CfsaRecurse

  ; BEGIN (* ComputeFsAbsentEstChildFormatsEmpty *) 
      FsAltNodeRef . FsAbsentEstChildFormatsEmpty := TRUE (* May change. *) 
    ; TRY CfsaRecurse ( FsAltNodeRef )  
      EXCEPT Done => 
      END (* EXCEPT *) 
    END ComputeFsAbsentEstChildFormatsEmpty 

; PROCEDURE ComputeSingletonListOptFields 
    ( LangInfo : LangInfoRefTyp 
    ; FsRuleNodeRef : LangUtil. FsNodeRefTyp 
    ; FsListChildRef : LangUtil. FsNodeRefTyp 
    ; ListTokSet : TokSetTyp 
    ) 
  RAISES { Backout } 
  (* PRE: FsListChildRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 

  (* Set FsSingletonOptMin and FsSingletonOptMapRef of FsListChildRef.  
     Also copy these fields down to the enclosed FsEstChildOf* node,
     if it is a different Fs node. 
  *) 

  = VAR CsloTokRel : TokRelation . T 
  ; VAR CsloAllElemTokSet : TokSetTyp 
  ; VAR CsloSingletonOptMin : LbeStd . TokTyp := LbeStd . Tok__Null 
  ; VAR CsloSingletonOptMapRef : LRTable . TokArrayRefTyp 

  ; PROCEDURE CsloFwdOuter ( OuterListTok : IntSets . ValidElemT ) 
    RAISES { Backout } 

    = VAR CsloFoOuterElemTokSet : TokSetTyp 

    ; PROCEDURE CsloFoFwdInner ( InnerListTok : IntSets . ValidElemT ) 
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
          ; CsloFoOuterElemTokSet 
              := IntSets . Difference 
                   ( CsloFoOuterElemTokSet , LInnerElemTokSet ) 
          END (* IF *) 
        END CsloFoFwdInner

    ; BEGIN (* CsloFwdOuter *) 
        VAR LListFsNode : LangUtil. FsNodeRefTyp 
      ; VAR LElemFsNode : LangUtil. FsNodeRefTyp 
      ; BEGIN (* Block CsloFwdOuter *) 
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
        ; CsloFoOuterElemTokSet 
            := TokRelation . GroundRightRelatives 
                 ( LangInfo . ClassRelation , LElemFsNode . FsTok ) 
        ; IF IntSets . IsElement 
               ( FsRuleNodeRef . FsTok , CsloFoOuterElemTokSet ) 
          THEN CsloFoOuterElemTokSet := IntSets . Empty ( ) 
          END (* IF *) 

        ; <* FATAL ANY *> BEGIN 
            IntSets . ForAllDo ( ListTokSet , CsloFoFwdInner ) 
          END (* Block *)
        ; TokRelation . SetRightRelatives 
            ( CsloTokRel , OuterListTok , CsloFoOuterElemTokSet ) 
        ; CsloAllElemTokSet 
            := IntSets . Union ( CsloAllElemTokSet , CsloFoOuterElemTokSet ) 
        END (* Block *) 
      END CsloFwdOuter

  ; BEGIN (* ComputeSingletonListOptFields *)  
      VAR LEstChildRef : LangUtil. FsNodeRefTyp 
    ; BEGIN (* Block for ComputeSingletonListOptFields *) 
        IF NOT IntSets . IsEmpty ( ListTokSet ) 
        THEN
          CsloTokRel 
            := TokRelation . New 
                 ( IntSets . Minimum ( ListTokSet ) 
                 , IntSets . Maximum ( ListTokSet ) 
                 )
        ; CsloAllElemTokSet := IntSets . Empty ( ) 
        (* CsloFwdOuter and CsloFoFwdInner constitute a doubly-nested loop over 
           the cartesian square of ListTokSet. 
        *) 
        ; <* FATAL ANY *> BEGIN 
            IntSets . ForAllDo ( ListTokSet , CsloFwdOuter ) 
          END (* Block *) 

        (* Here, we have: 
           1) CsloTokRel maps each member of ListTokSet to the set of element 
              tokens that can be elements of that list and to no other list in 
              ListTokSet, i.e., the set of element tokens that can be 
              singleton-optimized to a singleton of that list.  
           2) CsloAllElemTokSet is the union of the sets mapped-to by 
              CsloTokRel.

           We need CsloSingletonOptMapRef, a commute of CsloTokRel, with 
           CsloAllElemTokSet as its left domain.  This will be a function.  
        *) 
        
        ; IF IntSets . Card ( CsloAllElemTokSet ) > 0 
          THEN 
            CsloSingletonOptMin := IntSets . Minimum ( CsloAllElemTokSet ) 
          ; CsloSingletonOptMapRef 
              := NEW ( LRTable . TokArrayRefTyp 
                     , IntSets . Maximum ( CsloAllElemTokSet ) 
                       - CsloSingletonOptMin + 1 
                     )  
          ; FOR RI := FIRST ( CsloSingletonOptMapRef ^ ) 
                   TO LAST ( CsloSingletonOptMapRef ^ )
            DO CsloSingletonOptMapRef ^ [ RI] := LbeStd . Tok__Null 
            END (* FOR *) 
          (* A doubly-nested loop over the left (unique) and right (possibly 
             multiple) members of CsloTokRel. 
          *) 
          ; FOR RListTok 
                := IntSets . Minimum ( ListTokSet ) 
                TO IntSets . Maximum ( ListTokSet ) 
            DO 
              PROCEDURE CsloRevInner ( ElemTok : IntSets . ValidElemT ) 

              = BEGIN 
                  WITH WMapElem 
                       = CsloSingletonOptMapRef 
                         ^ [ ElemTok - CsloSingletonOptMin ] 
                  DO 
                    Assert 
                      ( WMapElem = LbeStd . Tok__Null  
                      , AFT . A_GrammarGen_SloRevInner_NotAFunction 
                      ) 
                  ; WMapElem := RListTok
                  END (* WITH *) 
                END CsloRevInner

            ; <* FATAL ANY *> BEGIN 
                IntSets . ForAllDo 
                   ( TokRelation . RightRelatives ( CsloTokRel , RListTok ) 
                   , CsloRevInner 
                   )
              END (* Block *)
            END (* FOR *) 
          ; FsListChildRef . FsSingletonOptMin := CsloSingletonOptMin 
          ; FsListChildRef . FsSingletonOptMapRef := CsloSingletonOptMapRef 
          ; LEstChildRef := LangUtil . FsEstChildRef ( FsListChildRef ) 
          ; IF LEstChildRef # FsListChildRef 
            THEN (* Copy these values down to the Est child Fs node. *)  
              LEstChildRef . FsSingletonOptMin := CsloSingletonOptMin 
            ; LEstChildRef . FsSingletonOptMapRef := CsloSingletonOptMapRef 
            END (* IF *)  
          END (* IF *) 
        END (* IF *) 
      END (* Block *) 
    END ComputeSingletonListOptFields 

(* VISIBLE: *) 
; PROCEDURE FinishFsRule 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; FsRuleNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  RAISES { Backout } 
  (* 1) Note the need for list cardinality tokens for other list nodes, 
        due to their use as principal children of this Fs Tree, in the 
        semantic node attached to the Abstract list declaration of the other 
        list node in the LDL tree.  
     2) Check for, warn about, and eliminate unreachable alternatives. 
     3) Compute and store values in FsAltListTokSet, FsAltStarListTokSet,
        FsAltFixedTokSet, and FsAltCardSet. 
     4) Apply predicates to the FsEstChildOpt field, making finer distinctions
        among the Optional group.
     5) Compute/store FsAbsentEstChildFormatsEmpty and FsEstListHasBookends. 
     6) Make FsTreeMap elements for list cardinality tokens point to their
         FsList rule.   
  *) 

  = VAR FfrCardSet : CardSetTyp := CardSetEmpty  
  ; VAR FfrTokSet : TokSetTyp := NIL  
  ; VAR FfrChildOpt : LangUtil . ChildOptTyp 

  ; PROCEDURE FfrNoteNeededListCardTok 
     ( ListTok : IntSets . ValidElemT 
     ; Card : LdlSemantics . ListCardTyp 
     )
    (* Note in the LDL tree, the need for a list cardinality token for 
       cardinality Card, of list token ListTok, in the semantic node on 
       the abstract list rule for ListTok. 
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
      END FfrNoteNeededListCardTok 

  ; PROCEDURE FfrNoteNeededEmptyListCardTok 
      ( ListTok : IntSets . ValidElemT ) 
    (* Specialized version of FfrNoteNeededListCardTok for 
       empty list cardinality.  Needed as a callback. *) 

    = BEGIN 
        FfrNoteNeededListCardTok ( ListTok , ListCardEmpty ) 
      END FfrNoteNeededEmptyListCardTok 

  ; PROCEDURE FfrNoteNeededSingletonListCardTok 
      ( ListTok : IntSets . ValidElemT ) 
    (* Specialized version of FfrNoteNeededListCardTok for 
       singleton list cardinality.  Needed as a callback. *) 

    = BEGIN 
        FfrNoteNeededListCardTok ( ListTok , ListCardSingleton ) 
      END FfrNoteNeededSingletonListCardTok 

  ; PROCEDURE FfrNoteNeededPluralListCardTok 
      ( ListTok : IntSets . ValidElemT ) 
    (* Specialized version of FfrNoteNeededListCardTok for 
       plural list cardinality.  Needed as a callback. *) 

    = BEGIN 
        FfrNoteNeededListCardTok ( ListTok , ListCardPlural ) 
      END FfrNoteNeededPluralListCardTok 

  ; PROCEDURE FfrRemainingCases 
      ( FsNodeRef : LangUtil . FsNodeRefTyp (* PRE: ^Non-NIL. *) 
      ; TokSet : TokSetTyp 
      ; CardSet : CardSetTyp 
      )
    : BOOLEAN (* A case remained. *) 
    RAISES { Backout } 
    (* Handle every case that satisfies both TokSet and CardSet. 
       These are all for the same format alternative, denoted by
       FsNodeRef. *) 

    = VAR LResult : BOOLEAN  
    ; VAR LListTokSet : TokSetTyp 
    ; VAR LListCard : CARDINAL 
    ; VAR LStarListTokSet : TokSetTyp 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LFixedTokSet : TokSetTyp 
    ; VAR LFixedTokSetCard : CARDINAL 

    ; BEGIN (* FfrRemainingCases *) 
        LResult := FALSE 
      ; LListTokSet := ListToksSubset ( LangInfo , TokSet )
      ; LListCard := IntSets . Card ( LListTokSet ) 
      ; IF LListCard > 0 
        THEN 
          LStarListTokSet := StarListToksSubset ( LangInfo , LListTokSet ) 
        ; LStarListCard := IntSets . Card ( LStarListTokSet ) 
        ; LFixedTokSet := IntSets . Difference ( TokSet , LListTokSet ) 
        ELSE 
          LStarListTokSet := IntSets . Empty ( )  
        ; LStarListCard := 0 
        ; LFixedTokSet := TokSet 
        END (* IF *) 
      ; LFixedTokSetCard := IntSets . Card ( LFixedTokSet ) 
(* MAYBE: Do we want to store LFixedTokSet or a token therefor, in
          PredClassTok? *) 
      ; FsNodeRef . FsAltListTokSet := LListTokSet 
      ; FsNodeRef . FsAltStarListTokSet := LStarListTokSet 
      ; FsNodeRef . FsAltFixedTokSet := LFixedTokSet 
      ; FsNodeRef . FsAltCardSet := CardSet 

      (* Absent. *) 
      ; IF LangUtil . FsEstChildRef ( FsNodeRef ) . FsEstChildOpt
           IN LangUtil . ChildOptSetOptByAS 
           AND CardTyp . Absent IN CardSet 
        THEN (* The absent case exists. *) 
          LResult := TRUE 
        END (* IF *) 
 
      (* Empty list. *) 
      ; IF LStarListCard > 0 AND CardTyp . EmptyList IN CardSet  
        THEN (* An empty list case exists. *) 
          IF FsNodeRef . FsCondDoParse 
          THEN 
            IF FsNodeRef . FsCondDoParse 
            THEN 
              <* FATAL ANY *> BEGIN 
                IntSets . ForAllDo 
                  ( LListTokSet , FfrNoteNeededEmptyListCardTok )
              END (* Block *) 
            END (* IF *) 
          END (* IF *) 
        ; LResult := TRUE 
        END (* IF *) 

      (* Singleton and plural lists. *) 
      ; IF LListCard > 0 
        THEN 
          IF CardTyp . SingletonList IN CardSet 
          THEN
            IF FsNodeRef . FsCondDoParse 
            THEN 
              <* FATAL ANY *> BEGIN 
                IntSets . ForAllDo 
                  ( LListTokSet , FfrNoteNeededSingletonListCardTok )
              END (* Block *)
            END (* IF *) 
          ; ComputeSingletonListOptFields 
              ( LangInfo , FsRuleNodeRef , FsNodeRef , LListTokSet )
          ; LResult := TRUE 
          END (* IF *) 
        ; IF CardTyp . PluralList IN CardSet 
          THEN
            IF FsNodeRef . FsCondDoParse 
            THEN 
              <* FATAL ANY *> BEGIN 
                IntSets . ForAllDo 
                  ( LListTokSet , FfrNoteNeededPluralListCardTok )
              END (* Block *) 
            END (* IF *) 
          ; LResult := TRUE 
          END (* IF *) 
        END (* IF *) 

      (* Fixed children. *) 
      ; IF LFixedTokSetCard > 0 
        THEN 
          LResult := TRUE 
        END (* IF *) 

      ; IF LResult 
        THEN 
          ComputeFsAbsentEstChildFormatsEmpty ( LangInfo , FsNodeRef ) 
        END (* IF *) 
      ; RETURN LResult 
      END FfrRemainingCases  

  ; PROCEDURE FfrPredicate ( FsPredNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Backout } 
    (* PRE: FsPredNodeRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 

    = PROCEDURE FfrPredGetListSetInfo 
        ( VAR ListSet : TokSetTyp ; VAR ListCard : CARDINAL ) 

      = BEGIN
          ListSet := ListToksSubset ( LangInfo , FfrTokSet )
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
        END FfrPredGetListSetInfo 

    ; BEGIN (* FfrPredicate *) 

        VAR LFsEstChildRef : LangUtil . FsNodeRefTyp 
      ; VAR LListCard : CARDINAL 
      ; VAR LMemberTokSet : TokSetTyp 
      ; VAR LListTokSet : TokSetTyp
      ; VAR LStarListTokSet : TokSetTyp
      ; VAR LStarListCard : CARDINAL 
      ; VAR LAltCardSet : CardSetTyp 

      ; BEGIN (* Block for FfrPredicate *) 
          IF FsPredNodeRef = NIL 
          THEN 
(* REVIEW:  Can this happen? *)
            CantHappen ( AFT . A_FfrPredicate_NIL_Alternative )  
          ELSE 
            ComputeFsAbsentEstChildFormatsEmpty ( LangInfo , FsPredNodeRef ) 
          ; LFsEstChildRef := LangUtil . FsEstChildRef ( FsPredNodeRef ) 
          ; Assert
              ( ( LFsEstChildRef . FsEstChildOpt = OptRequired ) 
                = ( FfrChildOpt = OptRequired ) 
              , AFT . A_FsTreeUtils_FfrPredicate_Inconsistent_requiredness
              ) 
          ; CASE FsPredNodeRef . FsKind <* NOWARN *> 
            OF FsKindTyp . FsKindSubtreeVert 
            , FsKindTyp . FsKindSubtreeHoriz 
            , FsKindTyp . FsKindSubtreeFill
            , FsKindTyp . FsKindEstChildOfFixed
            , FsKindTyp . FsKindEstChildOfList 
(* TODO: When NOPARSE addition is complete, this case should be impossible. *) 
(* Why? *) 
            => (* It's a default case, no predicate, no ELSE. *) 
              IF NOT FfrRemainingCases 
                       ( FsPredNodeRef , FfrTokSet , FfrCardSet ) 
              THEN 
                SemError
                  ( FsPredNodeRef . FsLdlNodeNo 
                  , AFT . W_Unsatisfiable_default_alternative 
                  ) 
              END (* IF *) 
            ; LFsEstChildRef . FsEstChildOpt := FfrChildOpt
            ; FfrCardSet := CardSetEmpty 
            ; FfrTokSet := IntSets . Empty ( )              

            | FsKindTyp . FsKindCondFmt 
            => CASE FsPredNodeRef . FsCondPredicate . PredicateKind 
              OF PredicateKindTyp . PredicateKindNull 
              => (* Maybe earlier-reported error? Will be eliminated. *) 

              | PredicateKindTyp . PredicateKindFalse 
              => (* This alternative will be eliminated. *) 

              (* Explicit ELSE. *) 
              | PredicateKindTyp . PredicateKindTrue
              => IF NOT FfrRemainingCases  
                          ( FsPredNodeRef , FfrTokSet , FfrCardSet ) 
                THEN 
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unsatisfiable_ELSE_alternative 
                    ) 
                ELSE 
                END (* IF *) 
              ; LFsEstChildRef . FsEstChildOpt := FfrChildOpt
              ; FfrCardSet := CardSetEmpty 
              ; FfrTokSet := IntSets . Empty ( )              

              (* ABSENT. *) 
              | PredicateKindTyp . PredicateKindAbsent 
              => LListTokSet := ListToksSubset ( LangInfo , FfrTokSet )
              ; FsPredNodeRef . FsAltListTokSet := LListTokSet 
              ; FsPredNodeRef . FsAltStarListTokSet 
                  := StarListToksSubset ( LangInfo , LListTokSet ) 
              ; FsPredNodeRef . FsAltFixedTokSet 
                  := IntSets . Difference ( FfrTokSet , LListTokSet )
              ; FsPredNodeRef . FsAltCardSet := CardSetAbsent 
              ; IF NOT LFsEstChildRef . FsEstChildOpt 
                       IN LangUtil . ChildOptSetOptByAS 
                THEN 
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unsatisfiable_ABSENT_on_required_child
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                ELSIF NOT CardTyp . Absent IN FfrCardSet 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unreachable_ABSENT_predicate  
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                ELSE 
                  LFsEstChildRef . FsEstChildOpt := OptAbsent 
                ; FfrChildOpt := OptPresent (* For future alts. *)
                END (* IF *)  
              (* Since having CardTyp.Absent completely specifies the case for
                 this predicate, we set the token sets to what they would be
                 if the child were NOT absent, just as possibly useful info. *) 
              ; FfrCardSet := FfrCardSet - CardSetAbsent  
              ; IF NOT CardTyp . Absent IN FfrCardSet 
                   AND IntSets . IsEmpty ( FfrTokSet ) 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . I_ABSENT_predicate_can_not_fail  
                    ) 
                END (* IF *) 

              (* PRESENT. *) 
              | PredicateKindTyp . PredicateKindPresent 
              => IF NOT FfrRemainingCases 
                          ( FsPredNodeRef 
                          , FfrTokSet 
                          , FfrCardSet - CardSetAbsent  
                          )
                THEN  
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unsatisfiable_PRESENT_No_members_remain 
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                END (* IF *)
              ; IF LFsEstChildRef . FsEstChildOpt # OptRequired 
                THEN 
                  LFsEstChildRef . FsEstChildOpt := OptPresent  
                ; FfrChildOpt := OptAbsent (* For future alts. *) 
                END (* IF *) 
              ; FfrTokSet := IntSets . Empty ( ) 
              ; FfrCardSet := FfrCardSet * CardSetAbsent  
              ; IF NOT CardTyp . Absent IN FfrCardSet 
                   AND IntSets . IsEmpty ( FfrTokSet ) 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . I_PRESENT_predicate_can_not_fail  
                    ) 
                END (* IF *) 

              (* EMPTY. *) 
              | PredicateKindTyp . PredicateKindEmptyList 
              => IF NOT CardTyp . EmptyList IN FfrCardSet 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unreachable_duplicate_EMPTY_predicate  
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                ELSE 
                  LStarListTokSet := StarListToksSubset ( LangInfo , FfrTokSet )
                ; LStarListCard := IntSets . Card ( LStarListTokSet ) 
                ; IF LStarListCard = 0 
                  THEN 
                    SemError
                      ( FsPredNodeRef . FsLdlNodeNo 
                      , AFT . W_Unsatisfiable_EMPTY_predicate_No_star_lists   
                      ) 
                  ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                      := PredicateKindTyp . PredicateKindFalse 
                  ELSE (* LStarListCard > 0 *) 
                    IF FsPredNodeRef . FsCondDoParse 
                    THEN 
                      <* FATAL ANY *> BEGIN 
                        IntSets . ForAllDo 
                          ( LListTokSet , FfrNoteNeededEmptyListCardTok )
                      END (* Block *) 
                    END (* IF *) 
                  ; FsPredNodeRef . FsAltStarListTokSet := LStarListTokSet 
                  ; FsPredNodeRef . FsAltListTokSet := LStarListTokSet  
                  ; FsPredNodeRef . FsAltFixedTokSet := IntSets . Empty ( ) 
                  ; FsPredNodeRef . FsAltCardSet := CardSetEmptyList  
                  END (* IF *) 
                END (* IF *) 
              ; LFsEstChildRef . FsEstChildOpt := FfrChildOpt
(* MAYBE: Create a class token for FfrTokSet 
      and put it into the predicate's class tok, or somewhere.
      Huh?
*) 
              ; FfrCardSet := FfrCardSet - CardSetEmptyList 
              ; IF FfrCardSet * CardSetList = CardSetEmpty 
                THEN (* Remove all the list tokens. *)  
                  FfrTokSet 
                    := IntSets . Difference ( FfrTokSet , LStarListTokSet )
                END (* IF *) 
              ; IF NOT CardTyp . Absent IN FfrCardSet 
                   AND IntSets . IsEmpty ( FfrTokSet ) 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . I_EMPTY_predicate_can_not_fail  
                    ) 
                END (* IF *) 

              (* NONEMPTY. *) 
              | PredicateKindTyp . PredicateKindNonemptyList
              => LAltCardSet := FfrCardSet * CardSetNonemptyList 
              ; IF LAltCardSet = CardSetEmpty 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unreachable_NONEMPTY_predicate  
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                ELSE 
                  FfrPredGetListSetInfo 
                    ( (* VAR *) LListTokSet , (* VAR *) LListCard )
                ; IF LListCard = 0 
                  THEN 
                    SemError
                      ( FsPredNodeRef . FsLdlNodeNo 
                      , AFT . W_Unsatisfiable_NONEMPTY_predicate_No_lists 
                      ) 
                  ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                      := PredicateKindTyp . PredicateKindFalse 
                  ELSE 
                    IF FsPredNodeRef . FsCondDoParse 
                    THEN 
                      IF CardTyp . SingletonList IN FfrCardSet 
                      THEN                       
                        <* FATAL ANY *> BEGIN 
                          IntSets . ForAllDo 
                            ( LListTokSet , FfrNoteNeededSingletonListCardTok )
                        END (* Block *) 
                      END (* IF *) 
                    ; IF CardTyp . PluralList IN FfrCardSet 
                      THEN 
                        <* FATAL ANY *> 
                        BEGIN IntSets . ForAllDo 
                                ( LListTokSet , FfrNoteNeededPluralListCardTok )
                        END (* Block *) 
                      END (* IF *) 
                    END (* IF *) 
                  END (* IF *) 
                ; ComputeSingletonListOptFields 
                    ( LangInfo , FsRuleNodeRef , FsPredNodeRef , LListTokSet )
(* MAYBE: Create a class token for 
      IntSets . Union ( FfrTokSet , LSingletonListElemTokSet )
      and put it into the predicate's class tok.
*) 
      ; FsPredNodeRef . FsAltStarListTokSet := IntSets . Empty ( ) 
                ; FsPredNodeRef . FsAltListTokSet := LListTokSet 
                ; FsPredNodeRef . FsAltFixedTokSet := IntSets . Empty ( ) 
                ; FsPredNodeRef . FsAltCardSet := LAltCardSet 
                ; LFsEstChildRef . FsEstChildOpt := FfrChildOpt
                ; FfrCardSet := FfrCardSet - CardSetNonemptyList  
                ; IF FfrCardSet * CardSetList = CardSetEmpty 
                  THEN (* Remove all the list tokens. *)
                    FfrTokSet 
                      := IntSets . Difference ( FfrTokSet , LListTokSet )
                  END (* IF *) 
                END (* IF *)  
              ; IF NOT CardTyp . Absent IN FfrCardSet 
                   AND IntSets . IsEmpty ( FfrTokSet ) 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . I_NONEMPTY_predicate_can_not_fail  
                    ) 
                END (* IF *) 

              (* NONPLURAL. *) 
              | PredicateKindTyp . PredicateKindNonpluralList 
              => LAltCardSet := FfrCardSet * CardSetNonpluralList 
              ; IF LAltCardSet = CardSetEmpty 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unreachable_NONPLURAL_predicate  
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                ELSE 
                  FfrPredGetListSetInfo 
                    ( (* VAR *) LListTokSet , (* VAR *) LListCard )
                ; IF LListCard = 0 
                  THEN
                    SemError
                      ( FsPredNodeRef . FsLdlNodeNo 
                      , AFT . W_Unsatisfiable_NONPLURAL_predicate_No_lists 
                      ) 
                  ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                      := PredicateKindTyp . PredicateKindFalse 
                  ELSE 
                    LStarListTokSet 
                      := StarListToksSubset ( LangInfo , LListTokSet )
                  ; LStarListCard := IntSets . Card ( LStarListTokSet ) 
                  ; IF FsPredNodeRef . FsCondDoParse  
                    THEN 
                      IF LStarListCard > 0 
                         AND CardTyp . EmptyList IN FfrCardSet 
                      THEN 
                        <* FATAL ANY *> BEGIN 
                          IntSets . ForAllDo 
                            ( LListTokSet , FfrNoteNeededEmptyListCardTok )
                        END (* Block *) 
                      END (* IF *) 
                    ; IF CardTyp . SingletonList IN FfrCardSet 
                      THEN 
                        <* FATAL ANY *> BEGIN 
                          IntSets . ForAllDo 
                            ( LListTokSet , FfrNoteNeededSingletonListCardTok )
                        END (* Block *) 
                      END (* IF *) 
                    END (* IF *) 
                  END (* IF *) 
                ; ComputeSingletonListOptFields 
                    ( LangInfo , FsRuleNodeRef , FsPredNodeRef , LListTokSet )
(* MAYBE: Create a class token for 
      IntSets . Union ( FfrTokSet , LSingletonListElemTokSet )
      and put it into the predicate's class tok.
*) 
                ; FsPredNodeRef . FsAltStarListTokSet := LStarListTokSet 
                ; FsPredNodeRef . FsAltListTokSet := LListTokSet 
                ; FsPredNodeRef . FsAltFixedTokSet := IntSets . Empty ( ) 
                ; FsPredNodeRef . FsAltCardSet := LAltCardSet 
                ; LFsEstChildRef . FsEstChildOpt := FfrChildOpt
                ; FfrCardSet := FfrCardSet - CardSetNonpluralList  
                ; IF FfrCardSet * CardSetList = CardSetEmpty 
                  THEN (* Remove all the list tokens. *)
                    FfrTokSet 
                      := IntSets . Difference ( FfrTokSet , LListTokSet )
                  END (* IF *) 
                END (* IF *) 
              ; IF NOT CardTyp . Absent IN FfrCardSet 
                   AND IntSets . IsEmpty ( FfrTokSet ) 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . I_NONPLURAL_predicate_can_not_fail  
                    ) 
                END (* IF *) 

              (* PLURAL. *) 
              | PredicateKindTyp . PredicateKindPluralList 
              => IF NOT CardTyp . PluralList IN FfrCardSet 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unreachable_PLURAL_predicate  
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                ELSE 
                  FfrPredGetListSetInfo 
                    ( (* VAR *) LListTokSet , (* VAR *) LListCard )
                ; IF LListCard = 0 
                  THEN 
                    SemError
                      ( FsPredNodeRef . FsLdlNodeNo 
                      , AFT . W_Unsatisfiable_PLURAL_predicate_No_lists 
                      ) 
                  ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                      := PredicateKindTyp . PredicateKindFalse 
                  ELSE 
                    IF FsPredNodeRef . FsCondDoParse 
                    THEN 
                      <* FATAL ANY *> BEGIN 
                        IntSets . ForAllDo  
                          ( LListTokSet , FfrNoteNeededPluralListCardTok ) 
                      END (* Block *) 
                    END (* IF *) 
                  END (* IF *) 
                ; ComputeSingletonListOptFields 
                    ( LangInfo , FsRuleNodeRef , FsPredNodeRef , LListTokSet )
(* MAYBE: Create a class token for 
      IntSets . Union ( FfrTokSet , LSingletonListElemTokSet )
      and put it into the predicate's class tok.
*) 
                ; FsPredNodeRef . FsAltStarListTokSet := IntSets . Empty ( ) 
                ; FsPredNodeRef . FsAltListTokSet := LListTokSet 
                ; FsPredNodeRef . FsAltFixedTokSet := IntSets . Empty ( ) 
                ; FsPredNodeRef . FsAltCardSet := FfrCardSet * CardSetPluralList
                ; LFsEstChildRef . FsEstChildOpt := FfrChildOpt
                ; FfrCardSet := FfrCardSet - CardSetPluralList 
                ; IF FfrCardSet * CardSetList = CardSetEmpty 
                  THEN (* Remove all the list tokens. *)
                    FfrTokSet 
                      := IntSets . Difference ( FfrTokSet , LListTokSet )
                  END (* IF *) 
                END (* IF *)  
              ; IF NOT CardTyp . Absent IN FfrCardSet 
                   AND IntSets . IsEmpty ( FfrTokSet ) 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . I_PLURAL_predicate_can_not_fail  
                    ) 
                END (* IF *) 

              (* MEMBER. *) 
              | PredicateKindTyp . PredicateKindInClass 
              => LMemberTokSet 
                   := LdlSemantics . AsTokSetOfTok 
                        ( LangInfo 
                        , FsPredNodeRef . FsCondPredicate . PredicateClass 
                        )
              ; LMemberTokSet 
                  := IntSets . Intersection ( LMemberTokSet , FfrTokSet ) 
              ; IF NOT FfrRemainingCases 
                         ( FsPredNodeRef 
                         , LMemberTokSet  
                         , FfrCardSet - CardSetAbsent  
                         )
                THEN 
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . W_Unsatisfiable_MEMBER_No_members 
                    ) 
                ; FsPredNodeRef . FsCondPredicate . PredicateKind 
                    := PredicateKindTyp . PredicateKindFalse 
                END (* IF *) 
              ; LFsEstChildRef . FsEstChildOpt := FfrChildOpt
              ; FfrTokSet := IntSets . Difference ( FfrTokSet , LMemberTokSet )
              ; IF NOT CardTyp . Absent IN FfrCardSet 
                   AND IntSets . IsEmpty ( FfrTokSet ) 
                THEN
                  SemError
                    ( FsPredNodeRef . FsLdlNodeNo 
                    , AFT . I_MEMBER_predicate_can_not_fail  
                    ) 
                END (* IF *) 
              END (* CASE PredicateKind *) 

            (* Unconditional alternative. *) 
            | FsKindTyp . FsKindNull 
            , FsKindTyp . FsKindBegOfImage
            , FsKindTyp . FsKindEndOfImage 
            , FsKindTyp . FsKindLineBreakReqd
            , FsKindTyp . FsKindLineBreakOpt
            => CantHappen ( AFT . A_P1FsrPredicate__Bad_FsNode ) 
            END (* CASE FsKind *)  
          END (* IF *) 
        END (* Block *) 
      END FfrPredicate 

  ; PROCEDURE FfrCondConstruct 
      ( VAR (* IN OUT *) FsCondFmtNodeRef : LangUtil . FsNodeRefTyp 
        (* ^Possibly changed to eliminate unsatisfiable alternatives. *) 
      ) 
    RAISES { Backout } 
    (* PRE: FsCondFmtNodeRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 
    (* NOT to be called recursively for subsequent predicates. *) 

    = PROCEDURE FfrCcEliminateFalsePreds 
        ( VAR (* IN OUT *) FsNodeRef : LangUtil . FsNodeRefTyp 
          (* ^Possibly changed to eliminate unsatisfiable alternatives. *) 
        ) 
      (* Remove predicates whith PredicateKindNull or PredicateKindFalse
         from the list of alternatives.
      *) 

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
              FfrCcEliminateFalsePreds 
                ( (* IN OUT *) FsNodeRef . FsCondAltRef ) 
            END (* IF *) 
          END (* IF *) 
        END FfrCcEliminateFalsePreds

    ; BEGIN (* FfrCondConstruct *) 
        VAR LOrigFsNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR LEstChildFsNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR LPredFsNodeRef : LangUtil . FsNodeRefTyp 

      ; BEGIN (* Block for FfrCondConstruct *) 
          LOrigFsNodeRef := FsCondFmtNodeRef 
(* CHECK: Be sure that, and document why, we can never have a NIL as an else
          alternative (i.e., FsCondFmtNodeRef # NIL *) 
        ; LEstChildFsNodeRef := LangUtil . FsEstChildRef ( FsCondFmtNodeRef )  

        (* Initialize the cardinality and token sets. *) 
        ; FfrCardSet := LangUtil . CardSetUniverse
        ; IF NOT LEstChildFsNodeRef . FsEstChildOpt 
                 IN LangUtil . ChildOptSetOptByAS
          THEN 
            FfrCardSet := FfrCardSet - CardSetAbsent  
          END (* IF *) 
        ; FfrTokSet 
            := LdlSemantics . AsTokSetOfTok 
                 ( LangInfo , LEstChildFsNodeRef . FsTok ) 
        ; FfrChildOpt := LEstChildFsNodeRef . FsEstChildOpt

        (* Initially process the alternatives. *) 
        ; LPredFsNodeRef := FsCondFmtNodeRef 
        ; WHILE LPredFsNodeRef # NIL 
          DO
            FfrPredicate ( LPredFsNodeRef )
          ; LPredFsNodeRef := LPredFsNodeRef . FsCondAltRef 
          END (* WHILE *) 

        (* Eliminate unsatisfiable predicates. *)
        ; FfrCcEliminateFalsePreds ( (* IN OUT *) FsCondFmtNodeRef ) 
        ; IF FsCondFmtNodeRef = NIL  
          THEN
            SemError
              ( LOrigFsNodeRef . FsLdlNodeNo 
              , AFT . E_No_satisfiable_alternative
              ) 
          END (* IF *) 
        END (* Block *) 
      END FfrCondConstruct 

(* CHECK: Do we want/need ComputeFsAbsentEstChildFormatsEmpty? 
           what node would it be on? *) 

  ; PROCEDURE FfrUncondPrincipalChild 
      ( FsChildNodeRef : LangUtil . FsNodeRefTyp )
    RAISES { Backout } 
    (* PRE: FsChildNodeRef is a principal child that is NOT inside a conditional
       construct.  It could still have multiple alternatives if it's optional 
       or contains list(s).
    *) 

    = VAR LTokSet : TokSetTyp 
    ; VAR LTokCard : CARDINAL 
    ; VAR LListCard : CARDINAL 
    ; VAR LStarListCard : CARDINAL 
    ; VAR LListTokSet : TokSetTyp 
    ; VAR LStarListTokSet : TokSetTyp 
    ; VAR LFixedTokSet : TokSetTyp 
    ; VAR LFixedTokSetCard : CARDINAL
    ; VAR LCardSet : CardSetTyp 

    ; BEGIN 
        FsChildNodeRef . FsAbsentEstChildFormatsEmpty := TRUE 
      ; LCardSet := CardSetEmpty 
      ; LTokSet 
          := LdlSemantics . AsTokSetOfTok 
               ( LangInfo , FsChildNodeRef . FsTok ) 
      ; LTokCard := IntSets . Card ( LTokSet ) 
      ; LListTokSet := ListToksSubset ( LangInfo , LTokSet )
      ; LListCard := IntSets . Card ( LListTokSet ) 
      ; IF LListCard > 0 
        THEN (* We have a list child. *) 
          LStarListTokSet := StarListToksSubset ( LangInfo , LListTokSet )
        ; LStarListCard := IntSets . Card ( LStarListTokSet ) 
        ; IF LStarListCard > 0 
          THEN 
            LCardSet := LCardSet + CardSetList 
          ; <* FATAL ANY *> BEGIN 
              IntSets . ForAllDo 
                ( LStarListTokSet , FfrNoteNeededEmptyListCardTok )
            END (* Block *) 
          ELSE 
            LCardSet := LCardSet + CardSetNonemptyList 
          END (* IF *) 
        ; <* FATAL ANY *> BEGIN 
            IntSets . ForAllDo 
              ( LListTokSet , FfrNoteNeededSingletonListCardTok )
          ; IntSets . ForAllDo ( LListTokSet , FfrNoteNeededPluralListCardTok )
          END (* Block *) 
        ; LFixedTokSet := IntSets . Difference ( LTokSet , LListTokSet ) 
        ELSE 
          LStarListTokSet := IntSets . Empty ( )  
        ; LFixedTokSet := LTokSet          
        END (* IF *) 
      ; ComputeSingletonListOptFields 
          ( LangInfo , FsRuleNodeRef , FsChildNodeRef , LListTokSet ) 
      ; LFixedTokSetCard := IntSets . Card ( LFixedTokSet )
      ; IF FsChildNodeRef . FsEstChildOpt IN LangUtil . ChildOptSetOptByAS 
        THEN
          LCardSet := LCardSet + CardSetAbsent  
        END (* IF *) 
      ; FsChildNodeRef . FsAltListTokSet := LListTokSet 
      ; FsChildNodeRef . FsAltStarListTokSet := LStarListTokSet 
      ; FsChildNodeRef . FsAltFixedTokSet := LFixedTokSet 
      ; FsChildNodeRef . FsAltCardSet := LCardSet 
      END FfrUncondPrincipalChild 

  ; PROCEDURE FfrUncondFsSubtree ( FsSubtreeNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Backout } 
    (* PRE: FsSubtreeNodeRef has FsKindEstFixed* or FsKindSubtree*. *) 
    (* PRE: We are NOT inside a conditional construct. *) 

    = VAR LChildCt : CARDINAL  

    ; BEGIN (* FfrUncondFsSubtree  *) 
        LChildCt := NUMBER ( FsSubtreeNodeRef . FsChildren ^ ) 

      (* Traverse the children. *)
      ; FOR RI := 0 TO LChildCt - 1 
        DO  
          FfrUncondFsChild 
            ( (* IN OUT *) FsSubtreeNodeRef . FsChildren ^ [ RI ] ) 
        END (* FOR *) 
      END FfrUncondFsSubtree 

  ; PROCEDURE FfrUncondFsChild 
      ( VAR (* IN OUT *) FsChildNodeRef : LangUtil . FsNodeRefTyp 
        (* ^Possibly changed to eliminate unsatisfiable alternatives. *) 
      ) 
    RAISES { Backout } 
    (* Called only when outside a conditional construct. *) 
    (* Mostly a dispatching procedure. *) 

    = BEGIN (* FfrUncondFsChild *) 
        CASE FsChildNodeRef . FsKind <* NOWARN *> 
        OF FsKindTyp . FsKindSubtreeHoriz
        , FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeFill 
        => FfrUncondFsSubtree ( FsChildNodeRef ) 

        | FsKindTyp . FsKindEstChildOfFixed 
        , FsKindTyp . FsKindEstChildOfList  
        => FfrUncondPrincipalChild ( FsChildNodeRef ) 

        | FsKindTyp . FsKindCondFmt 
        => FfrCondConstruct ( (* IN OUT *) FsChildNodeRef ) 

        | FsKindTyp . FsKindNull 
        , FsKindTyp . FsKindBegOfImage
        , FsKindTyp . FsKindEndOfImage 
        , FsKindTyp . FsKindLineBreakReqd
        , FsKindTyp . FsKindLineBreakOpt
        , FsKindTyp . FsKindInsTok 
        => (* Ignore these. *) 

        END (* CASE *) 
      END FfrUncondFsChild 

  ; PROCEDURE FfrListRule 
      ( FsListRuleNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Backout } 
    (* PRE: FsListRuleNodeRef has FsKindEstList*. *) 

    = PROCEDURE HasBookendsRecurse ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
      RAISES { Done } 

      = BEGIN
          IF FsNodeRef # NIL
          THEN 
            CASE FsNodeRef . FsKind 
            OF FsKindTyp . FsKindNull 
            , FsKindTyp . FsKindLineBreakReqd
            , FsKindTyp . FsKindLineBreakOpt
            => (* Ignore these. *) 

            | FsKindTyp . FsKindInsTok 
            => FsRuleNodeRef . FsEstListHasBookends := TRUE 
              ; RAISE Done 

            | FsKindTyp . FsKindSubtreeVert 
            , FsKindTyp . FsKindSubtreeHoriz 
            , FsKindTyp . FsKindSubtreeFill 
            => (* Traverse these. *) 
                FOR RFsChildNo := 0 TO NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
                DO HasBookendsRecurse 
                     ( FsNodeRef . FsChildren ^ [ RFsChildNo ] ) 
                END (* FOR *) 

            ELSE (* Don't descend into anything else. *) 
            END (* CASE *)
          END (* IF *) 
        END HasBookendsRecurse 

    ; VAR LFsChildNo : LangUtil . FsChildNoTyp 
    ; VAR LFsChildCt : LangUtil . FsChildNoTyp 

    ; BEGIN (* FfrListRule *) 
      (* Assign FsTreeMap elements of list cardinality tokens to point to their
         FsList rule. *)  
        IF FsListRuleNodeRef . FsEmptyListTok # LbeStd . Tok__Null
        THEN 
          LangInfo . FsTreeMapRef 
          ^ [ FsListRuleNodeRef . FsEmptyListTok - FsTreeMapBias ] 
            := FsListRuleNodeRef  
        END (* IF *) 
      ; LangInfo . FsTreeMapRef 
        ^ [ FsListRuleNodeRef . FsSingletonListTok - FsTreeMapBias ] 
          := FsListRuleNodeRef  

      ; LangInfo . FsTreeMapRef 
          ^ [ FsListRuleNodeRef . FsPluralListTok - FsTreeMapBias ] 
          := FsListRuleNodeRef  

      (* Compute FsEstListHasBookends. *) 
      ; FsListRuleNodeRef . FsEstListHasBookends := FALSE (* May change. *) 
      ; TRY 
          HasBookendsRecurse 
            ( FsListRuleNodeRef . FsChildren ^ [ EstHs . FmtNoListEstChild ] ) 
        EXCEPT Done => 
        END (* EXCEPT *) 

      (* Compute FsEstListHasSeps. *) 
      ; FsListRuleNodeRef . FsEstListHasSeps := FALSE (* May change. *)  
      ; LFsChildNo := 1 (* First separator, if it exists. *) 
      ; LFsChildCt := NUMBER ( FsListRuleNodeRef . FsChildren ^ )  
      ; LOOP 
          IF LFsChildNo >= LFsChildCt 
          THEN (* Found no separators. *) 
            EXIT 
          ELSE 
            CASE FsListRuleNodeRef . FsChildren ^ [ LFsChildNo ] . FsKind 
            OF FsKindTyp . FsKindNull 
            , FsKindTyp . FsKindLineBreakReqd
            , FsKindTyp . FsKindLineBreakOpt
            => (* Ignore these. *) 
            | FsKindTyp . FsKindInsTok 
            => FsListRuleNodeRef . FsEstListHasSeps := TRUE 
              ; EXIT 
            ELSE CantHappen 
                   ( AFT . A_FsTreeUtils_FfrListRule_Bad_FsKind_for_separator ) 
            END (* CASE *)  
          ; INC ( LFsChildNo ) 
          END (* IF *) 
        END (* LOOP *) 
 
      ; FfrUncondFsChild 
          ( (* IN OUT, but won't change in this case. *) 
            FsListRuleNodeRef . FsChildren ^ [ EstHs . FmtNoListEstChild ] 
          ) 
      END FfrListRule 

  ; BEGIN (* FinishFsRule *)
      IF FsRuleNodeRef # NIL 
      THEN 
        CASE FsRuleNodeRef . FsKind 
        OF FsKindTyp . FsKindEstFixedHoriz  
        , FsKindTyp . FsKindEstFixedVert  
        , FsKindTyp . FsKindEstFixedFill  
        => FfrUncondFsSubtree ( FsRuleNodeRef )

        | FsKindTyp . FsKindEstListHoriz  
        , FsKindTyp . FsKindEstListVert  
        , FsKindTyp . FsKindEstListFill   
        , FsKindTyp . FsKindEstListTrailHoriz  
        , FsKindTyp . FsKindEstListTrailVert  
        , FsKindTyp . FsKindEstListTrailFill   
        => FfrListRule ( FsRuleNodeRef )

        ELSE (* Can't happen. *) 
        END (* CASE *) 
      END (* IF *) 
    END FinishFsRule  

(* Transitive-closure-like computation of FormatsEmpty. *) 

; PROCEDURE CloseDeclaredClassFormatsEmpty  
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; ClassTok : LbeStd . TokTyp 
    ; VAR (* IN OUT *) ChangesOccurred : BOOLEAN 
    ) 

  = VAR LSemMemberDecl : LdlSemantics . SemDeclTyp 

  ; BEGIN 
      TYPECASE LdlSemantics . SemDeclOfTok ( LangInfo , ClassTok ) 
      OF NULL => (* ClassTok has no semantic node. *)  
      | LdlSemantics . SemFirstOccClassTyp ( TSemClassDecl ) 
      => IF TSemClassDecl . FormatsEmpty # UbUnknown 
        THEN (* It's already closed. *) 
        ELSE
          (* DECLARE *)   
            PROCEDURE CcVisitMember 
              ( MemberTok : LbeStd . TokTyp ) RAISES { Done } 

            = VAR LMemberFsTree : LangUtil . FsNodeRefTyp 
            ; VAR LResultFormatsEmpty : UncertainBool . T 

            ; BEGIN 
                LSemMemberDecl 
                  := LdlSemantics . SemDeclOfTok ( LangInfo , MemberTok ) 
              ; IF LSemMemberDecl = NIL 
                THEN (* MemberTok has a non-decl sem node.  No change. *) 
                ELSE 
                  CASE LSemMemberDecl . DeclKind 
                  OF DeclKindTyp . VarTermRule 
                  , DeclKindTyp . String 
                  => (* This class member is irrelevant. *)  

                  | DeclKindTyp . FixedRule  
                  , DeclKindTyp . StarRule 
                  , DeclKindTyp . PlusRule 
                  => LMemberFsTree  
                       := LangInfo . FsTreeMapRef 
                            ^ [ MemberTok - LbeStd . Tok__FirstLangDep ] 
                  ; IF LMemberFsTree = NIL 
                    THEN (* This class member can't format empty. *) 
                    ELSE 
                      LResultFormatsEmpty 
                        := UncertainBool . Or 
                             ( TSemClassDecl . FormatsEmpty 
                             , LMemberFsTree . FsFormatsEmpty 
                             )
                    ; IF LResultFormatsEmpty # TSemClassDecl . FormatsEmpty 
                      THEN (* This class member caused a change. *)  
                        TSemClassDecl . FormatsEmpty := LResultFormatsEmpty 
                      ; ChangesOccurred := TRUE 
                      ; IF TSemClassDecl . FormatsEmpty # UbUnknown 
                        THEN 
                          RAISE Done  
                        END (* IF *) 
                      END (* IF *) 
                    END (* IF *) 

                  ELSE (* This class member is irrelevant. *)  
                  END (* CASE *) 
                END (* IF *) 
              END CcVisitMember 
   
          ; <* FATAL ANY *> BEGIN (* Block *)  
              TRY 
                TokRelation . ForAllGroundMembers 
                  ( LangInfo . ClassRelation 
                  , TSemClassDecl . DeclTok 
                  , CcVisitMember 
                  ) 
              EXCEPT Done => 
              END (* TRY EXCEPT *) 
          END (* Block *) 
        END (* IF *) 
      ELSE (* ClassTok has some other type of semantic node. *) 
      END (* TYPECASE *) 
    END CloseDeclaredClassFormatsEmpty 

; PROCEDURE CloseFsChildListFormatsEmpty 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR (* IN OUT *) ChangesOccurred : BOOLEAN 
    ) 
  RAISES { Backout } 

  = VAR LFsChildRef : LangUtil . FsNodeRefTyp 
  ; VAR LResultFormatsEmpty : UncertainBool . T   
  ; VAR LChildFormatsEmpty : UncertainBool . T   
  ; VAR LChangesOccurred : BOOLEAN := FALSE 

  ; BEGIN 
      IF FsNodeRef . FsChildren # NIL (* Probably can't fail. *) 
      THEN 
        LResultFormatsEmpty := UbTrue (* As for empty string. *) 
      ; LChangesOccurred := FALSE 
      ; FOR RI := 0 TO NUMBER ( FsNodeRef . FsChildren ^ ) - 1
        DO 
          LFsChildRef := FsNodeRef . FsChildren ^ [ RI ] 
        ; CloseFsInteriorNodeFormatsEmpty 
            ( LangInfo 
            , LFsChildRef 
            , (* IN OUT *) LChangesOccurred 
            , (* VAR *) LChildFormatsEmpty 
              (* This is for the entire conditional construct, if any, whereas
                 if child is a CondFmt node, its FsFormatsEmpty field will be
                 only for the first alternative. *)  
            )
        ; LResultFormatsEmpty 
            := UncertainBool . And 
                 ( LResultFormatsEmpty , LChildFormatsEmpty ) 
        (* If LResultFormatsEmpty = UbFalse, we now have the final closed
           result for FsNodeRef . FsFsFormatsEmpty, but we still want to
           reevaluate any additional children to the left. *)  
        END (* FOR *) 
      ; IF LResultFormatsEmpty # FsNodeRef . FsFormatsEmpty 
        THEN
          FsNodeRef . FsFormatsEmpty := LResultFormatsEmpty
        ; ChangesOccurred := TRUE 
        END (* IF *) 
      END (* IF *) 
    END CloseFsChildListFormatsEmpty 

; PROCEDURE CloseFsCondConstructFormatsEmpty 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
      (* ^The first alternative. *) 
    ; VAR (* IN OUT *) ChangesOccurred : BOOLEAN 
    ; VAR CondFormatsEmpty : UncertainBool . T 
          (* ^For the entire conditional construct.  This is not stored in any
             FsNode, so must be returned separately. *) 
    ) 
  RAISES { Backout } 

  = VAR CccAltFormatsEmpty : UncertainBool . T 

  ; PROCEDURE CccEmptyStarListTokFormatsEmpty ( Tok : IntSets . ValidElemT ) 
    (* PRE Tok is for an abstract star list. *) 

    = VAR LListFsRuleNode : LangUtil . FsNodeRefTyp 
    ; VAR LListFormatsEmpty : UncertainBool . T 

    ; BEGIN
        LListFsRuleNode := LangInfo . FsTreeMapRef ^ [ Tok - FsTreeMapBias ] 
      ; LListFormatsEmpty 
          := UncertainBool . FromBool 
               ( NOT LListFsRuleNode . FsEstListHasBookends ) 
      ; CccAltFormatsEmpty 
          := UncertainBool . Or ( CccAltFormatsEmpty , LListFormatsEmpty ) 
      END CccEmptyStarListTokFormatsEmpty

  ; PROCEDURE CccSingletonListTokFormatsEmpty ( Tok : IntSets . ValidElemT ) 
    (* PRE Tok is for an abstract list. *) 

    = VAR LListFsRuleNode : LangUtil . FsNodeRefTyp 
    ; VAR LListFormatsEmpty : UncertainBool . T 

    ; BEGIN
        LListFsRuleNode := LangInfo . FsTreeMapRef ^ [ Tok - FsTreeMapBias ] 
      ; LListFormatsEmpty 
          := UncertainBool . And 
               ( UncertainBool . FromBool 
                   ( NOT LListFsRuleNode . FsEstListHasBookends )  
               , LListFsRuleNode . FsFormatsEmpty 
                 (* ^This does not take bookends into account. *) 
               ) 
      ; CccAltFormatsEmpty 
          := UncertainBool . Or ( CccAltFormatsEmpty , LListFormatsEmpty ) 
      END CccSingletonListTokFormatsEmpty

  ; PROCEDURE CccPluralListTokFormatsEmpty ( Tok : IntSets . ValidElemT ) 
    (* PRE Tok is for an abstract list. *) 

    = VAR LListFsRuleNode : LangUtil . FsNodeRefTyp 
    ; VAR LListFormatsEmpty : UncertainBool . T 

    ; BEGIN
        LListFsRuleNode := LangInfo . FsTreeMapRef ^ [ Tok - FsTreeMapBias ] 
      ; LListFormatsEmpty 
          := UncertainBool . And 
               ( UncertainBool . FromBool 
                   ( NOT LListFsRuleNode . FsEstListHasBookends 
                     AND NOT LListFsRuleNode . FsEstListHasSeps 
                   )  
               , LListFsRuleNode . FsFormatsEmpty 
                 (* ^This does not take bookends or separators into account. *) 
               ) 
      ; CccAltFormatsEmpty 
          := UncertainBool . Or ( CccAltFormatsEmpty , LListFormatsEmpty ) 
      END CccPluralListTokFormatsEmpty

  ; PROCEDURE CccFixedTokFormatsEmpty 
      ( Tok : IntSets . ValidElemT ) 
    (* PRE Tok is for an abstract list. *) 

    = VAR LFixedFsRuleNode : LangUtil . FsNodeRefTyp 

    ; BEGIN
        LFixedFsRuleNode := LangInfo . FsTreeMapRef ^ [ Tok - FsTreeMapBias ] 
      ; CccAltFormatsEmpty 
          := UncertainBool . Or 
               ( CccAltFormatsEmpty , LFixedFsRuleNode . FsFormatsEmpty ) 
      END CccFixedTokFormatsEmpty

  ; PROCEDURE CccRemainingCases ( FsNodeRef : LangUtil . FsNodeRefTyp ) 

    = BEGIN 
        CccAltFormatsEmpty := UbFalse (* Zero of Or, may change. *) 
      ; IF FsNodeRef . FsCondDoParse 
        THEN 

        (* Absent. *) 
          IF CardTyp . Absent IN FsNodeRef . FsAltCardSet
             AND LangUtil . FsEstChildRef ( FsNodeRef ) . FsEstChildOpt 
                 IN LangUtil . ChildOptSetOptByAS 
          THEN 
             CccAltFormatsEmpty 
               := UncertainBool . FromBool 
                    ( FsNodeRef . FsAbsentEstChildFormatsEmpty ) 
          END (* IF *) 

        (* Empty lists. *) 
        ; IF CardTyp . EmptyList IN FsNodeRef . FsAltCardSet
          THEN
            <* FATAL ANY *> BEGIN 
              IntSets . ForAllDo 
                ( FsNodeRef . FsAltStarListTokSet 
                , CccEmptyStarListTokFormatsEmpty 
                ) 
            END (* Block *) 
          END (* IF *) 

        (* Singleton lists. *) 
        ; IF CardTyp . SingletonList IN FsNodeRef . FsAltCardSet
          THEN
            <* FATAL ANY *> BEGIN 
              IntSets . ForAllDo 
                ( FsNodeRef . FsAltListTokSet 
                , CccSingletonListTokFormatsEmpty 
                ) 
            END (* Block *) 
          END (* IF *) 

        (* Plural lists. *) 
        ; IF CardTyp . PluralList IN FsNodeRef . FsAltCardSet
          THEN
            <* FATAL ANY *> BEGIN 
              IntSets . ForAllDo 
                ( FsNodeRef . FsAltListTokSet 
                , CccPluralListTokFormatsEmpty 
                ) 
            END (* Block *) 
          END (* IF *) 

        (* Fixed nodes. *) 
        ; <* FATAL ANY *> BEGIN 
            IntSets . ForAllDo 
              ( FsNodeRef . FsAltFixedTokSet 
              , CccFixedTokFormatsEmpty 
              ) 
          END (* Block *) 

        END (* IF *) 
      END CccRemainingCases 

  ; VAR LAltRef : LangUtil . FsNodeRefTyp 

  ; BEGIN (* CloseFsCondConstructFormatsEmpty *) 
      LAltRef := FsNodeRef 
    ; CondFormatsEmpty := UbFalse (* Zero of Or, may change *) 
    ; LOOP (* Thru' alternatives. *)  
        CloseFsChildListFormatsEmpty
          ( LangInfo , LAltRef , (* IN OUT *) ChangesOccurred )
      ; CASE LAltRef . FsCondPredicate . PredicateKind
        OF PredicateKindTyp . PredicateKindNull 
        , PredicateKindTyp . PredicateKindFalse 
        => (* These should have been eliminated earlier. *) 
          CantHappen 
            ( AFT . A_FsTreeUtils_CloseFsCondConstructFormatsEmpty_Leftover_null_or_false_predicate ) 
        ; CccAltFormatsEmpty := UbFalse (* Because this alt can't happen. *)  

        (* Explicit ELSE *) 
        | PredicateKindTyp . PredicateKindTrue
        => CccRemainingCases ( LAltRef )

        (* ABSENT *)  
        | PredicateKindTyp . PredicateKindAbsent 
        => IF LAltRef . FsCondDoParse 
           THEN 
             CccAltFormatsEmpty 
               := UncertainBool . FromBool 
                    ( LAltRef . FsAbsentEstChildFormatsEmpty ) 
           END (* IF *) 

        (* PRESENT *) 
        | PredicateKindTyp . PredicateKindPresent
        => CccRemainingCases ( LAltRef ) 
 
        (* EMPTY *) 
        | PredicateKindTyp . PredicateKindEmptyList 
        => IF LAltRef . FsCondDoParse 
           THEN
             CccAltFormatsEmpty := UbFalse (* Zero of Or, may change. *)
           ; <* FATAL ANY *> BEGIN 
               IntSets . ForAllDo 
                 ( LAltRef . FsAltStarListTokSet 
                 , CccEmptyStarListTokFormatsEmpty 
                 ) 
             END (* Block *) 
           END (* IF *) 

        (* NONEMPTY *) 
        | PredicateKindTyp . PredicateKindNonemptyList
        => IF LAltRef . FsCondDoParse 
           THEN
             CccAltFormatsEmpty := UbFalse (* Zero of Or, may change. *)
           ; <* FATAL ANY *> BEGIN 
               IntSets . ForAllDo 
                 ( LAltRef . FsAltListTokSet 
                 , CccSingletonListTokFormatsEmpty 
                 ) 
             ; IntSets . ForAllDo 
                 ( LAltRef . FsAltListTokSet 
                 , CccPluralListTokFormatsEmpty 
                 ) 
             END (* Block *) 
           END (* IF *) 

        (* NONPLURAL *) 
        | PredicateKindTyp . PredicateKindNonpluralList 
        => IF LAltRef . FsCondDoParse 
           THEN
             CccAltFormatsEmpty := UbFalse (* Zero of Or, may change. *)
           ; <* FATAL ANY *> BEGIN 
               IntSets . ForAllDo 
                 ( LAltRef . FsAltStarListTokSet 
                 , CccEmptyStarListTokFormatsEmpty 
                 ) 
             ; IntSets . ForAllDo 
                 ( LAltRef . FsAltListTokSet 
                 , CccSingletonListTokFormatsEmpty 
                 ) 
             END (* Block *) 
           END (* IF *) 

        (* PLURAL *) 
        | PredicateKindTyp . PredicateKindPluralList 
        => IF LAltRef . FsCondDoParse 
           THEN
             CccAltFormatsEmpty := UbFalse (* Zero of Or, may change. *) 
           ; <* FATAL ANY *> BEGIN 
               IntSets . ForAllDo 
                 ( LAltRef . FsAltListTokSet 
                 , CccPluralListTokFormatsEmpty 
                 ) 
             END (* Block *) 
           END (* IF *) 

        (* MEMBER *)  
        | PredicateKindTyp . PredicateKindInClass 
        => CccRemainingCases ( LAltRef ) 
        END (* CASE *) 
      ; LAltRef . FsFormatsEmpty := CccAltFormatsEmpty 
      ; CondFormatsEmpty 
          := UncertainBool . Or ( CondFormatsEmpty , CccAltFormatsEmpty )
      ; LAltRef := LAltRef . FsCondAltRef 
      ; IF LAltRef = NIL
        THEN EXIT 
        ELSIF LAltRef . FsKind # FsKindTyp . FsKindCondFmt
        THEN (* Last, unconditional alternative. *) 
          CccRemainingCases ( LAltRef ) 
        ; LAltRef . FsFormatsEmpty := CccAltFormatsEmpty 
        ; CondFormatsEmpty 
            := UncertainBool . Or ( CondFormatsEmpty , CccAltFormatsEmpty )
        ; EXIT 
        ELSE (* Go around another time. *) 
        END (* IF *)  
      END (* LOOP *)  
    END CloseFsCondConstructFormatsEmpty 

; PROCEDURE CloseFsInteriorNodeFormatsEmpty 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR (* IN OUT *) ChangesOccurred : BOOLEAN 
    ; VAR NodeFormatsEmpty : UncertainBool . T 
          (* ^FsNodeRef could be a conditional construct, and its computed
             FsFormatsEmpty will reflect only its one alternative.  
             The FormatsEmpty property of an entire conditional construct is 
             not stored in any FsNode, so it needs to be returned separately
             in this parameter. *) 
    ) 
  RAISES { Backout } 
  (* FsNodeRef is anything other than a top-level FsRule node. *) 

  = VAR LDeclNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LFsRuleNode : LangUtil . FsNodeRefTyp 

  ; BEGIN 
      IF FsNodeRef # NIL 
      THEN
        NodeFormatsEmpty := UbUnknown 
      ELSE 
        CASE FsNodeRef . FsKind 
        OF FsKindTyp . FsKindSubtreeHoriz
        , FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeFill 
        => CloseFsChildListFormatsEmpty 
             ( LangInfo , FsNodeRef , (* IN OUT *) ChangesOccurred ) 
        ; NodeFormatsEmpty := FsNodeRef . FsFormatsEmpty 

        | FsKindTyp . FsKindNull 
        , FsKindTyp . FsKindBegOfImage
        , FsKindTyp . FsKindEndOfImage 
        , FsKindTyp . FsKindLineBreakReqd
        , FsKindTyp . FsKindLineBreakOpt
        , FsKindTyp . FsKindInsTok 
        , FsKindTyp . FsKindAstString 
        => (* All these will always have a known, spontaneous, unchanged 
              FsFormatsEmpty. *) 
           NodeFormatsEmpty := FsNodeRef . FsFormatsEmpty 

        | FsKindTyp . FsKindEstChildOfFixed 
        , FsKindTyp . FsKindEstChildOfList  
        => LDeclNodeNo 
             := LdlSemantics . EstNodeNoOfTokDecl 
                  ( LangInfo , FsNodeRef . FsTok ) 
        ; TYPECASE LangInfo . SemMapRef ^ [ LDeclNodeNo ] . SemRef 
          OF NULL => NodeFormatsEmpty := FsNodeRef . FsFormatsEmpty 

          | LdlSemantics . SemFirstOccClassTyp ( TSemClassDecl ) 
          => (* Est child token is a class. *)  
            NodeFormatsEmpty := TSemClassDecl . FormatsEmpty 

          | LdlSemantics. SemDeclTyp ( TSemDecl ) 
          => (* Est child token is form a single AS rule. *) 
            CASE TSemDecl . DeclKind 
            OF DeclKindTyp . VarTermRule 
            , DeclKindTyp . String 
            => (* These always format nonempty. *)
              NodeFormatsEmpty := UbFalse  

            | DeclKindTyp . FixedRule 
            , DeclKindTyp . StarRule 
            , DeclKindTyp . PlusRule 
            => (* A single Fs rule. *) 
              LFsRuleNode 
                := LangInfo . FsTreeMapRef 
                     ^ [ FsNodeRef . FsTok - LbeStd . Tok__FirstLangDep ] 
            ; NodeFormatsEmpty := LFsRuleNode . FsFormatsEmpty 
            ELSE NodeFormatsEmpty := FsNodeRef . FsFormatsEmpty 
            END (* CASE *) 

          ELSE NodeFormatsEmpty := FsNodeRef . FsFormatsEmpty  
          END (* TYPECASE *) 
        ; IF FsNodeRef . FsFormatsEmpty # NodeFormatsEmpty 
          THEN 
            FsNodeRef . FsFormatsEmpty := NodeFormatsEmpty 
          ; ChangesOccurred := TRUE 
          END (* IF *) 

        | FsKindTyp . FsKindCondFmt 
        => CloseFsCondConstructFormatsEmpty 
             ( LangInfo 
             , FsNodeRef 
             , (* IN OUT *) ChangesOccurred 
             , (* VAR *) CondFormatsEmpty := NodeFormatsEmpty 
             )

        ELSE (* Ignore others. *) 
          NodeFormatsEmpty := FsNodeRef . FsFormatsEmpty 
        END (* CASE *) 
      END (* IF *) 
    END CloseFsInteriorNodeFormatsEmpty 

; PROCEDURE CloseFsRuleFormatsEmpty 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; FsRuleTok : LbeStd . TokTyp 
    ; VAR (* IN OUT *) ChangesOccurred : BOOLEAN 
    ) 
  RAISES { Backout } 

  = VAR LFsNodeRef : LangUtil . FsNodeRefTyp
  ; VAR LFsEstChild : LangUtil . FsNodeRefTyp 
  ; VAR LFsChildWOBookends : LangUtil . FsNodeRefTyp 
  ; VAR LListChildFormatsEmpty : UncertainBool . T   

  ; BEGIN 
      LFsNodeRef := LangInfo . FsTreeMapRef ^ [ FsRuleTok - FsTreeMapBias ]
    ; IF LFsNodeRef # NIL AND LFsNodeRef . FsFormatsEmpty = UbUnknown 
      THEN
        CASE LFsNodeRef . FsKind 
        OF FsKindTyp . FsKindEstFixedHoriz 
        , FsKindTyp . FsKindEstFixedVert 
        , FsKindTyp . FsKindEstFixedFill
        => CloseFsChildListFormatsEmpty 
             ( LangInfo , LFsNodeRef , (* IN OUT *) ChangesOccurred ) 

        | FsKindTyp . FsKindEstListHoriz 
        , FsKindTyp . FsKindEstListVert 
        , FsKindTyp . FsKindEstListFill 
        , FsKindTyp . FsKindEstListTrailHoriz  
        , FsKindTyp . FsKindEstListTrailVert  
        , FsKindTyp . FsKindEstListTrailFill   
        => LFsEstChild 
             := LFsNodeRef . FsChildren ^ [ EstHs . FmtNoListEstChild ]
        ; LFsChildWOBookends  
             := LdlSemantics . FsCondOrEstOfFsList ( LFsNodeRef ) (* Exists. *)
        ; CloseFsInteriorNodeFormatsEmpty 
            ( LangInfo 
            , LFsChildWOBookends 
            , (* IN OUT *) ChangesOccurred 
            , (* VAR *) NodeFormatsEmpty := LListChildFormatsEmpty 
            )
        ; LFsNodeRef . FsFormatsEmpty := LListChildFormatsEmpty   
          (* ^This is for the conditional construct, or Est child, if there 
             is no conditional construct, but does not consider list bookends 
             or separators.  The true formats-empty property for the entire 
             list depends on the list cardinality that is applied to it from 
             above, but can always be computed at a parent site from this 
             FsFormatsEmpty value, the applied list cardinality, and 
             spontaneous properties FsEstListHasBookends and FsEstListHasSeps. 
          *)  

        ELSE (* Ignore others. *) 
        END (* CASE *) 
      END (* IF *) 
    END CloseFsRuleFormatsEmpty 

(* VISIBLE: *) 
; PROCEDURE CloseFormatsEmpty ( LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { Backout } 
  (* Compute the closure of the FormatsEmpty property. *) 

  = VAR LChangesOccurred : BOOLEAN := TRUE
  ; VAR LIterationCt : CARDINAL := 0   
       (* Always force one initial iteration of each closure procedure. *) 

  ; BEGIN 
      WHILE LChangesOccurred  
      DO (* Iterate until nothing changes. *)
        LChangesOccurred := FALSE  
      ; FOR RTok := LangUtilLo . TokClassFirstTok 
                      ( LangInfo , TokClassTyp . TokClassAsPlus )
            TO LangUtilLo . TokClassLastTok 
                 ( LangInfo , TokClassTyp . TokClassAsStarTrailing )
        DO
          CloseFsRuleFormatsEmpty 
            ( LangInfo , RTok , (* IN OUT *) LChangesOccurred ) 
        END (* FOR *) 
      ; FOR RClassTok := LangInfo . AsPartialToks 
            TO LangInfo . AsClassOnlyToks - 1 
        DO
          CloseDeclaredClassFormatsEmpty 
            ( LangInfo , RClassTok , (* IN OUT *) LChangesOccurred ) 
        END (* FOR *) 
      ; INC ( LIterationCt ) 
      END (* WHILE *) 
    ; TextOnly 
        ( Fmt . Int ( LIterationCt ) 
          & " iterations closing FormatsEmpty." 
        , Kind := MessageCodes . KindTyp . MkInformation 
        ) 
    END CloseFormatsEmpty 

; BEGIN 
  END FsTreeUtils
.

