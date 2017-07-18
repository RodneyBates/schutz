
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Ldl1FsTrees 
(* Package Ldl1FsTrees. 
   Build FsTrees for a language definition written in Ldl1.
*) 

; IMPORT Fmt 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 
; IMPORT AstView 
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT IntSets 
; IMPORT LangUtil 
; FROM LangUtil 
  IMPORT ChildOptTyp , FsKindTyp , PredicateKindTyp 
; IMPORT LangUtilLo 
; IMPORT LangUtilRep 
; IMPORT LbeStd 
; FROM LbeStd IMPORT TokClassTyp 
; IMPORT Ldl1Child 
; IMPORT Ldl1Semantics 
; IMPORT Ldl1Tok 
; IMPORT LdlSemantics 
; IMPORT LRTable 
; IMPORT MessageCodes 
; FROM Messages IMPORT SemError , SemError2 , SemErrorText  
; IMPORT SharedStrings 
; IMPORT TextIntSymbolTable 
; IMPORT UncertainBool  

; TYPE AFT = MessageCodes . T 

; CONST ListCardEmpty = LdlSemantics . ListCardTyp . ListCardEmpty 
; CONST ListCardSingleton = LdlSemantics . ListCardTyp . ListCardSingleton 
; CONST ListCardPlural = LdlSemantics . ListCardTyp . ListCardPlural 

; CONST UbUnknown = UncertainBool . T . Unknown 
; CONST UbFalse = UncertainBool . T . False 
; CONST UbTrue = UncertainBool . T . True  

; VAR UseImpliedIndentation : BOOLEAN := FALSE 

(* VISIBLE: *) 
; PROCEDURE BuildFixedTerm 
    ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  RAISES { AssertionFailure } 

  = VAR LFsChild : LangUtil . FsNodeRefTyp 
  ; VAR LFsRoot : LangUtil . FsNodeRefTyp 
  ; VAR LFmtNoMapRef : LangUtilRep . FsFmtNoMapRefTyp
  ; VAR LFsChildrenRef : LangUtil . FsChildrenArrayRefTyp 

  ; BEGIN (* BuildFixedTerm *) 
      LFsChild 
        := NEW 
             ( LangUtil . FsNodeRefTyp 
             , FsFmtNo := EstHs . FmtNoModTok 
             , FsLeftFmtNo := EstHs . FmtNoModTok 
             , FsRightFmtNo := EstHs . FmtNoModTok  
             , FsIndentCode := LangUtil . IndentCodeInitial 
             , FsTok := Tok 
             , FsKind := FsKindTyp . FsKindInsTok 
             , FsInsTokRef 
                 := LangInfo . StringMapRef ^ 
                      [ Tok - LbeStd . Tok__FirstLangDep ] 
             , FsFormatsEmpty := UbFalse 
             , FsHasLineBreak := LangUtil . FormatsEmptyTyp . FeNo  
             ) 
    ; LFsChildrenRef := NEW ( LangUtil . FsChildrenArrayRefTyp , 1 ) 
    ; LFsChildrenRef ^ [ 0 ] := LFsChild 
    ; LFmtNoMapRef := NEW ( LangUtilRep . FsFmtNoMapRefTyp , 1 )
    ; LFmtNoMapRef ^ [ 0 ] := 0  
    ; LFsRoot 
        := NEW 
             ( LangUtil . FsNodeRefTyp 
             , FsFmtNo := EstHs . FmtNoModTok 
             , FsLeftFmtNo := EstHs . FmtNoModTok 
             , FsRightFmtNo := EstHs . FmtNoModTok  
             , FsTok := Tok 
             , FsIndentCode := LangUtil . IndentCodeInitial 
             , FsKind := FsKindTyp . FsKindEstFixedHoriz 
             , FsChildren := LFsChildrenRef 
             , FsFmtNoMapRef := LFmtNoMapRef 
             , FsFormatsEmpty := UbFalse  
             , FsHasLineBreak := LangUtil . FormatsEmptyTyp . FeNo  
             ) 
    ; TRY 
        LdlSemantics . PostNumberFsTree ( LFsRoot ) 
      EXCEPT 
      LdlSemantics . NodeNoOverflow 
        => CantHappen 
             ( AFT . A_Ldl1FsTrees_BuildFixedTerm_NodeNoOverflow ) 
      END 
    ; LangInfo . FsTreeMapRef ^ [ Tok - LbeStd . Tok__FirstLangDep ] := LFsRoot 
    END BuildFixedTerm 

(* VISIBLE: *) 
; PROCEDURE BuildVarTerm 
    ( VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; LdlNode : AstView . AstRefTyp  
    ; VarTermTok : LbeStd . TokTyp 
    ; VarTermModTok : LbeStd . TokTyp 
    ) 
  RAISES { AssertionFailure } 

  = VAR LFsChild : LangUtil . FsNodeRefTyp 
  ; VAR LFsRoot : LangUtil . FsNodeRefTyp 
  ; VAR LFmtNoMapRef : LangUtilRep . FsFmtNoMapRefTyp
  ; VAR LFsChildrenRef : LangUtil . FsChildrenArrayRefTyp 

  ; BEGIN (* BuildVarTerm *) 
      LFsRoot 
        := NEW 
             ( LangUtil . FsNodeRefTyp 
             , FsFmtNo := EstHs . FmtNoAstString  
             , FsLeftFmtNo := EstHs . FmtNoAstString  
             , FsRightFmtNo := EstHs . FmtNoAstString  
             , FsIndentCode := LangUtil . IndentCodeInitial 
             , FsTok := VarTermTok 
             , FsKind := FsKindTyp . FsKindAstString 
             , FsLdlNodeNo := LdlNode . NodeNo 
             , FsFormatsEmpty := UbFalse 
             , FsHasLineBreak := LangUtil . FormatsEmptyTyp . FeNo  
             ) 
    ; TRY 
        LdlSemantics . PostNumberFsTree ( LFsRoot ) 
      EXCEPT 
      LdlSemantics . NodeNoOverflow 
        => CantHappen 
             ( AFT . A_Ldl1FsTrees_BuildVarTerm_NodeNoOverflow1 ) 
      END 
    ; LangInfo . FsTreeMapRef ^ [ VarTermTok - LbeStd . Tok__FirstLangDep ] 
        := LFsRoot 
    ; LFsChild 
        := NEW 
             ( LangUtil . FsNodeRefTyp 
             , FsFmtNo := EstHs . FmtNoModTok 
             , FsLeftFmtNo := EstHs . FmtNoModTok 
             , FsRightFmtNo := EstHs . FmtNoModTok  
             , FsIndentCode := LangUtil . IndentCodeInitial 
             , FsTok := VarTermTok 
             , FsKind := FsKindTyp . FsKindEstChildOfFixed 
             , FsEstChildNo := 0 
             , FsEstChildIsOptional := FALSE 
             , FsEstChildOpt := ChildOptTyp . OptRequired 
             , FsDeletableItemsAreToRight := FALSE 
             , FsChildIndentCode := LangUtil . IndentCodeInitial 
             , FsLdlNodeNo := LdlNode . NodeNo 
             , FsFormatsEmpty := UbFalse 
             , FsHasLineBreak := LangUtil . FormatsEmptyTyp . FeNo  
             ) 
    ; LFsChildrenRef := NEW ( LangUtil . FsChildrenArrayRefTyp , 1 ) 
    ; LFsChildrenRef ^ [ 0 ] := LFsChild 
    ; LFmtNoMapRef := NEW ( LangUtilRep . FsFmtNoMapRefTyp , 1 )
    ; LFmtNoMapRef ^ [ 0 ] := 0  
    ; LFsRoot 
        := NEW 
             ( LangUtil . FsNodeRefTyp 
             , FsFmtNo := EstHs . FmtNoModTok 
             , FsLeftFmtNo := EstHs . FmtNoModTok 
             , FsRightFmtNo := EstHs . FmtNoModTok  
             , FsTok := VarTermModTok 
             , FsIndentCode := LangUtil . IndentCodeInitial 
             , FsKind := FsKindTyp . FsKindEstFixedHoriz 
             , FsChildren := LFsChildrenRef 
             , FsFmtNoMapRef := LFmtNoMapRef 
             , FsLdlNodeNo := LdlNode . NodeNo 
             , FsFormatsEmpty := UbFalse 
             , FsHasLineBreak := LangUtil . FormatsEmptyTyp . FeNo  
             ) 
    ; TRY 
        LdlSemantics . PostNumberFsTree ( LFsRoot ) 
      EXCEPT 
      LdlSemantics . NodeNoOverflow 
        => CantHappen 
             ( AFT . A_Ldl1FsTrees_BuildVarTerm_NodeNoOverflow2 ) 
      END 
    ; LangInfo . FsTreeMapRef ^ [ VarTermModTok - LbeStd . Tok__FirstLangDep ] 
        := LFsRoot 
    END BuildVarTerm 

(* VISIBLE: *) 
; PROCEDURE Build 
    ( VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; FsIdentSemRef : LdlSemantics . SemFsRefTyp 
    ; AsIdentSemRef : LdlSemantics . SemDeclAsNodeTyp 
    ; IsStart : BOOLEAN 
    ) 
  : LangUtil . FsNodeRefTyp 
  RAISES { AssertionFailure } 
  (* Build an FS tree for a single AS node. *) 

  = VAR BldFsIdentNode 
      := AstView . AstRef ( LangInfo . Root , FsIdentSemRef . NodeNo ) 
  ; VAR BldFsRuleNode 
      := AstView . AstRef ( LangInfo . Root , FsIdentSemRef . RefRuleNodeNo ) 
  ; VAR BldAsIdentNode 
      := AstView . AstRef ( LangInfo . Root , AsIdentSemRef . NodeNo ) 
  ; VAR BldAsRuleNode 
      := AstView . AstRef ( LangInfo . Root , AsIdentSemRef . DeclRuleNodeNo ) 
(* CHECK: Can we avoid calling AstView . AstRef? *) 
  ; VAR BldNextFmtNo : EstHs . FmtNoTyp 

  ; VAR BldNextAsChildExists : BOOLEAN 
  ; VAR BldNextAsChildName : AstView . AstRefTyp 
  ; VAR BldNextAsChildClass : AstView . AstRefTyp 
  ; VAR BldAsChildren : AstView . AstRefTyp 
  ; VAR BldNextAsChild : AstView . AstRefTyp 
  (* ^BldNextAsChildExists, BldNextAsChild, BldNextAsChildName, 
     BldNextAsChildClass, and BldAsChildren are used to step through abstract 
     children, in step with the format syntax nodes that correspond to them.  
     BldNextAsChildExists tells whether there is another abstract child.  If 
     so, BldNextAsChildClass is the tree node of the abstract child type, and 
     BldNextAsChildName is the node for its child name, if any.  For a list 
     As/Fs rule, BldAsChildren is Null, and BldNextAsChild is meaningless.  
     For a fixed As/Fs rule, BldAsChildren is the tree node for the list of 
     abstract children and BldNextAsChild is the tree node for the next child. 
  *) 

  ; VAR BldAltlistNode : AstView . AstRefTyp 
  ; VAR BldNextAltNode : AstView . AstRefTyp 
  ; VAR BldElseNode : AstView . AstRefTyp 
  (* ^BldAltlistNode, BldNextAltNode, and BldElseNode are used to work through
     the alternatives of a CASE or the one alternative of a conditional
     format. 
     BldAltListNode and BldElseNode are copied children of the FsChildCase 
     node. Both are null, for a single CF node. 
     BldNextAltNode is the next alternative, following the current one.  
     It is Null if none exists.  This is enough info for ConsumeAlternative 
     to sequence through the alternatives.
  *)  

  ; VAR BldFsCondAltRef : LangUtil . FsNodeRefTyp 
    (* ^Used to get the value for FsCondAltRef up from an Est child,
       where it is built by a recursive call on FsTreeForCondConstruct
       for the next alternative, back up to the enclosing instance of
       FsTreeForCondConstruct.  It only carries info upwards, during
       recursive returns, from an Est child to its surrounding
       CondFormat node, so it does not need to be stacked.  *)

  ; VAR BldFsKindForEstChild : FsKindTyp 
  ; VAR BldEstDescendantCt : LbeStd . EstChildNoTyp := LbeStd . EstChildNoNull 
  ; VAR BldFsNodeForEstDescendant : LangUtil . FsNodeRefTyp := NIL 
  ; VAR BldLdlFsEstChild : AstView . AstRefTyp 
  ; VAR BldCondIndentCode : LangUtil . IndentCodeTyp 
  ; VAR BldPrincipalChildTokSet : IntSets . T 
  ; VAR BldCondFmtDepth := 0 
  ; VAR BldPredicate := LangUtil . PredicateNull  
  ; VAR BldIsList : BOOLEAN 
  ; VAR BldIsInFirstLine : BOOLEAN 
  ; VAR BldCondStartsInFirstLine : BOOLEAN 
  ; VAR BldACondEndsInFirstLine : BOOLEAN 
  ; VAR BldACondEndsNotInFirstLine : BOOLEAN 
  ; VAR BldIsRightOfEstListChild : BOOLEAN 
  ; VAR BldLineBreakFollowsEstChild : BOOLEAN 
  ; VAR BldAbsentAltSeen : BOOLEAN := FALSE 
  ; VAR BldPresentAltSeen : BOOLEAN := FALSE 
  ; VAR BldPrevLineBreakRef : LangUtil . FsNodeRefTyp 
  ; VAR BldNextOptionId : LRTable . OptionIdTyp 
  ; VAR BldNextCondId : LRTable . OptionIdTyp 

  ; PROCEDURE IncOptionId 
      ( VAR NextOptionId : LRTable . OptionIdTyp 
      ; <* UNUSED *> KindForMsg : TEXT 
      ) 

    = BEGIN 
        IF NextOptionId > LAST ( LRTable . OptionIdRealTyp ) 
        THEN
          SemError 
            ( BldAsIdentNode . NodeNo 
            , AFT . E_Too_many_conditional_children 
            ) 
(* FIXME: ^Make this check earlier, on the AS, so it doesn't get repeated. *)
        ELSE 
          INC ( NextOptionId ) 
        END (* IF *) 
      END IncOptionId 

  ; PROCEDURE ConsumeAsChild ( ) RAISES { AssertionFailure } 

    = VAR LAsGrandChildren : ARRAY [ 0 .. 1 ] OF AstView . AstRefTyp 

    ; BEGIN (* ConsumeAsChild *) 
        IF BldAsChildren = AstView . AstRefNull 
        THEN 
          BldNextAsChildExists := FALSE 
        ELSIF BldNextAsChildExists 
        THEN 
          AstView . NextChild 
            ( BldAsChildren , BldNextAsChild , BldNextAsChildExists ) 
        ; IF BldNextAsChildExists 
          THEN 
            AstView . GetChildren 
              ( BldNextAsChild , LAsGrandChildren , LangInfo . LdlLang ) 
          ; BldNextAsChildName 
              := LAsGrandChildren [ Ldl1Child . AsReqdChild_ChildName ] 
          ; BldNextAsChildClass 
              := LAsGrandChildren [ Ldl1Child . AsReqdChild_ChildClass] 
          END (* IF *) 
        END (* IF *) 
      END ConsumeAsChild 

  ; PROCEDURE FmtNoOfNextAsChild ( ) : EstHs . FmtNoTyp 
    RAISES { AssertionFailure } 

    = VAR LLeafElem : EstHs . LeafElemTyp 

    ; BEGIN (* FmtNoOfNextAsChild *) 
        IF BldAsChildren = AstView . AstRefNull 
        THEN (* This is the Est child of a list. *) 
          RETURN EstHs . FmtNoListEstChild 
        ELSE 
(* FIX: This doesn't make any sense.  We are interested in FmtNos in the
        FsTrees being generated for the described language, not in the
        Est for the LDL description. *) 
          EstUtil . GetLeafElem 
            ( RootRef := LangInfo . Root . NodeRef 
            , NodeNo := BldNextAsChild . NodeNo 
            , ResultLeafElem := LLeafElem 
            ) 
        ; RETURN LLeafElem . LeFmtNo 
        END (* IF *) 
      END FmtNoOfNextAsChild 

  ; PROCEDURE InitiateFmtNos ( ) 

    = BEGIN 
        IF BldNextFmtNo = EstHs . FmtNoNull 
        THEN 
          BldNextFmtNo := EstHs . FmtNoListEstChild 
        END (* IF *) 
      END InitiateFmtNos 

  ; PROCEDURE AssignFmtNo ( ) : EstHs . FmtNoTyp 

    = VAR LResult : EstHs . FmtNoTyp 

    ; BEGIN (* AssignFmtNo *) 
        LResult := BldNextFmtNo 
      ; IF LResult # EstHs . FmtNoNull 
        THEN INC ( BldNextFmtNo ) 
        END (* IF *) 
      ; RETURN LResult 
      END AssignFmtNo 

  ; PROCEDURE SetLeftAndRightFmtNo ( FsRef : LangUtil . FsNodeRefTyp ) 

    = BEGIN (* SetLeftAndRightFmtNo *) 
        FsRef . FsLeftFmtNo := FsRef . FsFmtNo 
      ; FsRef . FsRightFmtNo := FsRef . FsFmtNo 
      END SetLeftAndRightFmtNo 

  ; PROCEDURE CheckFmtNoMatch 
      ( LdlFsChild : AstView . AstRefTyp 
      ; FsEstChild : LangUtil . FsNodeRefTyp 
      ) 
    RAISES { AssertionFailure } 

    = VAR LFmtNoOfNextAsChild := FmtNoOfNextAsChild ( ) 
    ; VAR LNextAsChildClass := BldNextAsChildClass 

    ; BEGIN (* CheckFmtNoMatch *) 
(* FIX: This check is completely wrong.  We need to search for all 
        instances of Ests of the kind that this As/Fs name identifies. *) 
        IF FALSE AND Ldl1Semantics . Bootstrapping 
        THEN 
          IF LFmtNoOfNextAsChild # FsEstChild . FsFmtNo 
          THEN 
            SemError2 
              ( LdlFsChild . NodeNo 
              , LNextAsChildClass . NodeNo 
              , AFT . E_AbstractFmtNoMismatchesComputed 
              ) 
          END (* IF *) 
        END (* IF *) 
      END CheckFmtNoMatch

  ; PROCEDURE Indenter 
      ( IndenterNode : AstView . AstRefTyp 
      ; NewLineIndentCode : LangUtil . IndentCodeTyp 
      ; IndentCode : LangUtil . IndentCodeTyp 
      ) 
    : LangUtil . IndentCodeTyp 
    RAISES { AssertionFailure } 

    = VAR LResult : INTEGER 
    ; VAR LIntNode : AstView . AstRefTyp 

    ; BEGIN 
        IF IndenterNode . NodeRef = NIL 
        THEN 
          LResult := NewLineIndentCode 
        ELSE
          LIntNode  
            := AstView . Child 
                 ( IndenterNode 
                 , Ldl1Child . BolIndenter_IndentCode 
                 , LangInfo . LdlLang 
                 ) 
        ; IF LIntNode . NodeRef = NIL 
          THEN LResult := 0 
          ELSE 
            LResult 
              := AstView . IntValue 
                   ( AstView . Child 
                       ( IndenterNode 
                       , Ldl1Child . BolIndenter_IndentCode 
                       , LangInfo . LdlLang 
                       ) 
                   ) 
          END (* IF *) 
        ; CASE EstUtil . EstTok ( IndenterNode . NodeRef ) <* NOWARN *>
          OF Ldl1Tok . BolIndenter
          , Ldl1Tok . ParentIndenter
          , Ldl1Tok . PosIndenter
          , Ldl1Tok . BolPlusIndenter
          , Ldl1Tok . ParentPlusIndenter
          , Ldl1Tok . PosPlusIndenter
          => (* Positive. *) 

          | Ldl1Tok . BolMinusIndenter
          , Ldl1Tok . ParentMinusIndenter
          , Ldl1Tok . PosMinusIndenter
          => (* Negative. *) 
            LResult := - LResult
          END (* CASE *) 
        ; CASE EstUtil . EstTok ( IndenterNode . NodeRef ) <* NOWARN *>
          OF Ldl1Tok . BolIndenter
          , Ldl1Tok . BolPlusIndenter
          , Ldl1Tok . BolMinusIndenter 
          => (* Absolute. *) 
(* TODO: Implement absolute indenters. *) 
            SemError
              ( IndenterNode . NodeNo 
              , AFT . W_Ldl1_FS_Absolute_indenters_not_implemented 
              ) 
          ; LResult := NewLineIndentCode 

          | Ldl1Tok . ParentIndenter
          , Ldl1Tok . ParentPlusIndenter
          , Ldl1Tok . ParentMinusIndenter
          => (* Relative to indentation passed by parent. *) 
           INC ( LResult , NewLineIndentCode ) 

          | Ldl1Tok . PosIndenter
          , Ldl1Tok . PosPlusIndenter
          , Ldl1Tok . PosMinusIndenter
          => (* Relative to current spot *) 
           INC ( LResult , IndentCode ) 
          END (* CASE *) 
        END (* IF *) 
      ; LResult := MIN ( LResult , LangUtil . IndentCodeLastNormal ) 
      ; LResult := MAX ( LResult , LangUtil . IndentCodeFirstNormal ) 
      ; RETURN LResult 
      END Indenter 

  ; PROCEDURE FsTreeForString 
      ( LdlNode : AstView . AstRefTyp
      ; SemDecl : LdlSemantics . SemFirstOccStringTyp 
      ; VAR (* IN OUT *) CurrentIndentCode : LangUtil . IndentCodeTyp 
      ) 
      : LangUtil . FsNodeRefTyp 

    = VAR LResult : LangUtil . FsNodeRefTyp 

    ; BEGIN (* FsTreeForString *) 
        LResult 
          := NEW 
               ( LangUtil . FsNodeRefTyp 
               , FsFmtNo := AssignFmtNo ( ) 
               , FsIndentCode := CurrentIndentCode 
               , FsTok := SharedStrings . Tok ( SemDecl . InsertionString ) 
               , FsKind := FsKindTyp . FsKindInsTok 
               , FsInsTokRef := SemDecl . InsertionString 
               , FsIsInsideList := BldIsList
               , FsIsInsideCondFmt := BldCondFmtDepth > 0 
               , FsIsInFirstLine := BldIsInFirstLine  
               , FsIsRightOfEstListChild := BldIsRightOfEstListChild
               , FsLdlNodeNo := LdlNode . NodeNo 
               , FsFormatsEmpty := UbFalse  
               ) 
      ; SetLeftAndRightFmtNo ( LResult ) 
      ; IF UseImpliedIndentation   
        THEN 
          LdlSemantics . IncIndentCode 
            ( CurrentIndentCode 
            , SharedStrings . Length ( SemDecl . InsertionString )
              + 1 (* For token separator. *)  
(* TODO:      ^ If token separators are ever made more sophisticated than 
         unconditionally one blank, then this will need to adapt.
*) 
            ) 
        END (* IF *) 
      ; RETURN LResult 
      END FsTreeForString 

  ; PROCEDURE FsTreeForEstChild 
      ( LdlFsChildNode : AstView . AstRefTyp 
      ; Tok : LbeStd . TokTyp 
      ; IndentCode : LangUtil . IndentCodeTyp 
        (* ^Regular indentation for current token. *) 
      ; ChildIndentCode : LangUtil . IndentCodeTyp 
        (* ^From INDENT in the Est child. *) 
      ; FsKindForLineBreak : FsKindTyp 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LAsChildNo : LbeStd . EstChildNoTyp 
    ; VAR LResult : LangUtil . FsNodeRefTyp  
    ; VAR LFmtNo : EstHs . FmtNoTyp 
    ; VAR LAltNode : AstView . AstRefTyp 
    ; VAR LEstChildOpt : ChildOptTyp 
    ; VAR LPredicate : LangUtil . PredicateTyp 
    ; VAR LEstChildIsOptional : BOOLEAN 

    ; BEGIN (* FsTreeForEstChild *) 
        IF BldNextAsChildExists 
        THEN 
          TYPECASE 
            LangInfo . SemMapRef ^ [ BldNextAsChildClass . NodeNo ] . SemRef 
          OF NULL 
          => LAsChildNo := LbeStd . EstChildNoNull 
          | LdlSemantics . SemChildClassRefTyp ( TAsChildClassRef ) 
          => LAsChildNo := TAsChildClassRef . ChildNo 
          ELSE 
            LAsChildNo := LbeStd . EstChildNoNull 
          END (* TYPECASE *) 
        ; LPredicate := BldPredicate 
        ; BldPredicate := LangUtil . PredicateNull   
        ; IF BldCondFmtDepth = 0 OR AstView . IsNull ( BldNextAltNode ) 
          THEN (* There is no subsequent CASE alternative, not even an ELSE. *) 
            InitiateFmtNos ( ) 
          ; LFmtNo := AssignFmtNo ( ) 
          ; BldFsCondAltRef := NIL 
          ELSE (* Recurse into alternative, to assign FmtNos in right order. *)
            LAltNode := BldNextAltNode 
          ; ConsumeAlternative ( ) 
          ; BldFsCondAltRef 
              := FsTreeForCondConstruct
                   ( LAltNode 
                   , BldCondIndentCode 
                   , FsKindForLineBreak 
                   , (* VAR *) EstChildFmtNo := LFmtNo 
                   )  
          END (* IF *) 
        ; IF BldEstDescendantCt > 0 AND NOT BldLineBreakFollowsEstChild 
          THEN
(* FIX: This doesn't take into account multiple alternatives. *) 
          (*SemError
              ( LdlFsChildNode . NodeNo 
              , AFT . W_Ldl1_FS_No_line_break_since_the_previous_est_child
              ) 
          *) 
            SemErrorText 
              ( LangInfo . Gram . tokImage ( AsIdentSemRef . DeclTok ) 
(* TODO: This method (Heh, Heh) of calling a tokImage seems pretty kludgy. *)
                & ", descendant no " 
                & Fmt . Int ( BldEstDescendantCt )  
              , AFT . W_Ldl1_FS_No_line_break_since_the_previous_est_child_
              ) 
          END (* IF *) 
        ; IF NOT BldIsList (* Child of list is never optional. *)
             AND EstUtil . EstTok ( BldNextAsChild . NodeRef ) 
                 = Ldl1Tok . AsOptChild 
                 (* ^It's an optional child of fixed. *) 
          THEN 
(* TODO: This application of predicates to compute LEstChildOpt is
         redundant to code in FinishFsTree.  Maybe remove it here and
         just set to either OptRequired or OptOptional? *) 
            IF LPredicate . PredicateKind 
               = PredicateKindTyp . PredicateKindAbsent
               OR BldPresentAltSeen 
            THEN (* The optional Est child must be absent. *) 
              LEstChildOpt := ChildOptTyp . OptAbsent 
            ELSIF LPredicate . PredicateKind 
                  = PredicateKindTyp . PredicateKindPresent
                  OR BldAbsentAltSeen 
            THEN (* The optional Est child must be present. *) 
              LEstChildOpt := ChildOptTyp . OptPresent 
            ELSE 
              LEstChildOpt := ChildOptTyp . OptOptional 
            END (* IF *) 
          ; LEstChildIsOptional := TRUE 
          ELSE 
            LEstChildOpt := ChildOptTyp . OptRequired 
          ; LEstChildIsOptional := FALSE 
          END (* IF *) 
        ; LResult 
            := NEW 
                 ( LangUtil . FsNodeRefTyp 
                 , FsFmtNo := LFmtNo  
                 , FsIndentCode := IndentCode 
                 , FsTok := Tok 
                 , FsKind := BldFsKindForEstChild 
                 , FsChildIndentCode := ChildIndentCode 
                 , FsEstChildOpt := LEstChildOpt  
                 , FsEstChildIsOptional := LEstChildIsOptional  
                 , FsEstChildNo := LAsChildNo 
                 , FsIsInsideList := BldIsList
                 , FsIsInsideCondFmt := BldCondFmtDepth > 0 
                 , FsIsInFirstLine := BldIsInFirstLine  
                 , FsLdlNodeNo := LdlFsChildNode . NodeNo 
                 , FsFormatsEmpty := UbUnknown 
                 ) 
        ; IF LEstChildIsOptional  
          THEN (* We need an option id for hand-written CS. *) 
            LResult . FsOptionIds [ FALSE ] := BldNextOptionId  
          ELSE 
            LResult . FsOptionIds [ FALSE ] := LRTable . OptionIdNull   
          END (* IF *) 

        ; IF BldCondFmtDepth > 0 (* Implies controlled by a predicate. *)  
             OR LEstChildIsOptional 
          THEN 
            LResult . FsOptionIds [ TRUE ] := BldNextCondId            
          ; IncOptionId ( (* VAR *) BldNextCondId , "generated" ) 
          ELSE 
            LResult . FsOptionIds [ TRUE ] := LRTable . OptionIdNull   
          END (* IF *) 

(* TODO: We could get a head start on transitive FormatsEmpty here, in some
         cases of the Est child class token's FormatsEmpty properties.
*) 
        ; IF BldIsList 
          THEN
            LResult . FsFirstListElemIndentCode 
              := LangUtil . IndentCodeInitial 
(* FIX: ^This is not right. It needs to allow for indenters in a CF node. *) 
          END (* IF *) 
        ; SetLeftAndRightFmtNo ( LResult ) 
        ; CheckFmtNoMatch ( LdlFsChildNode , LResult ) 
        ; IF BldFsNodeForEstDescendant = NIL 
          THEN 
            BldFsNodeForEstDescendant := LResult  
          ; BldLdlFsEstChild := LdlFsChildNode 
          ELSIF BldCondFmtDepth > 0 
          THEN
            SemError2
              ( LdlFsChildNode . NodeNo 
              , BldLdlFsEstChild . NodeNo 
              , AFT . E_ConditionalFormatHasMoreThanOneEstDescendant
              ) 
          END (* IF *) 
        ; INC ( BldEstDescendantCt ) 
        ; BldLineBreakFollowsEstChild := FALSE 
        ; IF BldCondFmtDepth = 0 
          THEN 
            BldPrincipalChildTokSet
              := LdlSemantics . AsTokSetOfAstRef 
                   ( LangInfo , BldNextAsChildClass ) 
          ; IF LEstChildIsOptional 
            THEN 
              IncOptionId ( (* VAR *) BldNextOptionId , "handwritten" ) 
            END (* IF *)
          ; ConsumeAsChild ( )
          ELSE 
            (* Postpone ConsumeAsChild until the end of the entire conditional 
               construct. 
            *) 
          END (* IF *) 
        ELSE 
          SemError2 
            ( LdlFsChildNode . NodeNo 
            , BldAsIdentNode . NodeNo 
            , AFT . E_AbstractNodeHasNoCorrespondingPrincipalChild
            ) 
        ; LResult := NIL 
        END (* IF *) 
      ; BldIsRightOfEstListChild 
          := BldIsRightOfEstListChild OR BldIsList 
      ; RETURN LResult 
      END FsTreeForEstChild 

  ; PROCEDURE FsTreeForIdent 
      ( IdentNode : AstView . AstRefTyp 
      ; VAR (* IN OUT *) CurrentIndentCode : LangUtil . IndentCodeTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LAsDeclNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LAsSemDecl : LdlSemantics . SemDeclTyp 
    ; VAR LWasFound : BOOLEAN 
    ; VAR LHasErrors : BOOLEAN 

    ; BEGIN 
        TYPECASE IdentNode . NodeRef 
        OF SharedStrings . T ( TSharedString ) 
        => TextIntSymbolTable . Find 
             ( LangInfo . SymbolTable 
             , SharedStrings . ToText ( TSharedString ) 
             , LWasFound 
             , LAsDeclNodeNo 
             ) 
        ; IF LWasFound 
          THEN 
            LAsSemDecl := LangInfo . SemMapRef ^ [ LAsDeclNodeNo ] . SemRef 
          ; IF LAsSemDecl = NIL 
            THEN RETURN NIL 
            ELSE 
              LangInfo . SemMapRef ^ [ IdentNode . NodeNo ] . SemRef 
                := NEW 
                     ( LdlSemantics . SemRefTyp 
                     , NodeNo := IdentNode . NodeNo 
                     , RefDeclId := LAsDeclNodeNo 
                     , RefTok := LAsSemDecl . DeclTok 
                     ) 
            ; TYPECASE LAsSemDecl 
              OF NULL 
              => CantHappen 
                   ( AFT . A_Ldl1FsTrees_FsTreeForIdent_NotDecl ) 
              | LdlSemantics . SemFirstOccClassTyp ( TSemClass ) 
              => IF TSemClass . SingletonTok # LbeStd . Tok__Null 
                 THEN 
                   TYPECASE LdlSemantics . SemDeclOfTok 
                             ( LangInfo , TSemClass . SingletonTok ) 
                   OF NULL => 
                   | LdlSemantics . SemFirstOccStringTyp ( TSemString ) 
                   => RETURN 
                        FsTreeForString 
                          ( IdentNode 
                          , TSemString 
                          , (* IN OUT *) CurrentIndentCode 
                            := CurrentIndentCode 
                          )   
                   ELSE 
                   END (* TYPECASE *) 
                 END (* IF*) 
              ELSE 
              END (* TYPECASE *) 
            ; IF BldNextAsChildExists 
              THEN
                Ldl1Semantics . CheckContainment 
                   ( LangInfo 
                   , IdentNode . NodeNo 
                   , BldNextAsChildClass . NodeNo 
                   , LHasErrors 
                   ) 
              ; IF LHasErrors 
                THEN RETURN NIL  
                ELSE                 
                  RETURN 
                    FsTreeForEstChild 
                      ( IdentNode 
                      , LAsSemDecl . DeclTok 
                      , IndentCode := CurrentIndentCode 
                      , ChildIndentCode := CurrentIndentCode 
                      , FsKindForLineBreak := FsKindForLineBreak
                      ) 
                END (* IF *) 
              ELSE 
                SemError 
                  ( IdentNode . NodeNo 
                  , AFT . E_NoAbstractChildExistsForThisFormatRuleChild 
                  ) 
              ; RETURN NIL 
              END (* IF *) 
            END (* IF *) 
          ELSE 
            SemError 
              ( IdentNode . NodeNo 
              , AFT . E_FormatRuleChildIsUndeclared 
              ) 
          ; RETURN NIL 
          END (* IF *) 
        ELSE 
          CantHappen 
            ( AFT . A_Ldl1FsTrees_FsTreeForIdent_IdentTokenNotString ) 
        ; RETURN NIL 
        END (* TYPECASE *) 
      END FsTreeForIdent 

  ; PROCEDURE FsTreeForFsChildPlain      
      ( FsChild : AstView . AstRefTyp 
      ; NewLineIndentCode : LangUtil . IndentCodeTyp 
      ; VAR (* IN OUT *) CurrentIndentCode : LangUtil . IndentCodeTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ; DoBuildFsTree : BOOLEAN 
(* TODO: ^This is now always TRUE. *) 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LAsSemDecl: LdlSemantics . SemDeclTyp 
    ; VAR LChildIndentCode : LangUtil . IndentCodeTyp 
    ; VAR LAsDeclNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LHasErrors : BOOLEAN 
    ; VAR LWasFound : BOOLEAN 
    ; VAR LFsChildren : ARRAY [ 0 .. Ldl1Child . FsChildPlain_ChildClass ] 
                        OF AstView . AstRefTyp 

    ; BEGIN (* FsTreeForFsChildPlain *) 
        LHasErrors := FALSE 
      ; AstView . GetChildren ( FsChild , LFsChildren , LangInfo . LdlLang ) 
      ; WITH 
          WFsChildClass = LFsChildren [ Ldl1Child . FsChildPlain_ChildClass ] 
        DO CASE EstUtil . EstTok ( WFsChildClass . NodeRef ) 
           OF LbeStd . Tok__Null 
           => RETURN NIL 

           | Ldl1Tok . Alternation 
           => SemError 
                ( WFsChildClass . NodeNo 
                , AFT . E_UnimplementedAnonymousClass 
                ) 
           ; RETURN NIL 

           | Ldl1Tok . DontCare 
           => IF BldNextAsChildExists 
              THEN 
                LAsSemDecl
                  := Ldl1Semantics . FirstOcc 
                       ( LangInfo 
                       , LangInfo . SemMapRef ^ 
                           [ BldNextAsChildClass . NodeNo ] . SemRef 
                       )
              ; LAsDeclNodeNo := BldAsIdentNode . NodeNo  
              ELSE
                SemError 
                  ( WFsChildClass . NodeNo 
                  , AFT . E_NoAbstractChildExistsForThisFormatRuleChild 
                  ) 
              ; LHasErrors := TRUE 
              END (* IF *) 

           | Ldl1Tok . Ident 
           => TextIntSymbolTable . Find 
                ( LangInfo . SymbolTable 
                , SharedStrings . ToText ( WFsChildClass . NodeRef ) 
                , LWasFound 
                , LAsDeclNodeNo 
                ) 
           ; IF LWasFound 
             THEN 
               LAsSemDecl := LangInfo . SemMapRef ^ [ LAsDeclNodeNo ] . SemRef 
             ; IF LAsSemDecl= NIL 
               THEN RETURN NIL 
               ELSE 
                 LangInfo . SemMapRef ^ [ WFsChildClass . NodeNo ] . SemRef 
                   := NEW ( LdlSemantics . SemRefTyp 
                          , NodeNo := WFsChildClass . NodeNo 
                          , RefDeclId := LAsDeclNodeNo 
                          , RefTok := LAsSemDecl . DeclTok
                          ) 
               ; TYPECASE LAsSemDecl
                 OF NULL 
                 => CantHappen 
                      ( AFT . A_Ldl1FsTrees_FsTreeForFsChildPlain_NotDecl ) 
                 | LdlSemantics . SemFirstOccClassTyp ( TSemClass ) 
                 => IF TSemClass . SingletonTok # LbeStd . Tok__Null 
                    THEN
                      WITH WSingletonDeclNodeNo  
                           = LangInfo . TokMapRef ^ 
                               [ TSemClass . SingletonTok 
                                 - LdlSemantics . TokMapBias 
                               ] 
                      DO TYPECASE 
                           LangInfo . SemMapRef ^ 
                              [ WSingletonDeclNodeNo ] . SemRef 
                        OF NULL => 
                        | LdlSemantics . SemFirstOccStringTyp ( TSemString ) 
                        => IF LFsChildren 
                                [ Ldl1Child . FsChildPlain_ChildName ] 
                              . NodeRef 
                              # NIL 
                          THEN 
                            SemError2
                              ( WFsChildClass . NodeNo 
                              , WSingletonDeclNodeNo 
                              , AFT . E_StringFsChildCannotHaveChildName 
                              ) 
                          END (* IF *) 
                        ; IF LFsChildren 
                               [ Ldl1Child . FsChildPlain_ChildIndenter ] 
                             . NodeRef 
                             # NIL 
                          THEN 
                            SemError2
                              ( WFsChildClass . NodeNo 
                              , WSingletonDeclNodeNo 
                              , AFT . E_StringFsChildCannotHaveIndenter 
                              ) 
                          END (* IF *) 
                        ; IF DoBuildFsTree 
                          THEN 
                            RETURN 
                              FsTreeForString 
                                ( FsChild 
                                , TSemString 
                                , (* IN OUT *) CurrentIndentCode 
                                  := CurrentIndentCode 
                                )
                          ELSE RETURN NIL 
                          END (* IF *) 
                        ELSE 
                        END (* TYPECASE *) 
                      END (* WITH *) 
                    END (* IF *) 
                 ELSE 
                 END (* TYPECASE *) 
               ; IF BldNextAsChildExists 
                 THEN
                   Ldl1Semantics . CheckContainment 
                     ( LangInfo 
                     , WFsChildClass . NodeNo 
                     , BldNextAsChildClass . NodeNo 
                     , LHasErrors 
                     ) 
(* FIXME: ^This is too demanding.  Inside an Fs predicate, WFsChildClass
          can be smaller than BlsNextAsChildClass.  But we don't know that
          until FsTreeUtils.FinishFsRule. *) 
                 END (* IF *) 
               END (* IF *)  
             ELSE 
               SemError 
                 ( WFsChildClass . NodeNo , AFT . E_ChildClassIsUndeclared ) 
             ; RETURN NIL 
             END (* IF *) 

           ELSE 
             CantHappen ( AFT . A_FsTreeForFsChildPlainBadToken ) 
           ; RETURN NIL 
           END (* CASE *) 
         ; WITH WFsChildName 
                = LFsChildren [ Ldl1Child . FsChildPlain_ChildName ] 
           DO IF WFsChildName . NodeRef # NIL AND BldNextAsChildExists 
              THEN 
                IF BldNextAsChildName . NodeRef # NIL 
                   AND BldNextAsChildName . NodeRef 
                       # WFsChildName . NodeRef 
                THEN 
                  SemError2 
                    ( WFsChildName . NodeNo 
                    , BldNextAsChildName . NodeNo 
                    , AFT . E_FormatSyntaxChildNameMismatchesAbstractChildName 
                    ) 
                ; RETURN NIL  
                END (* IF *) 
              END (* IF *) 
           END (* WITH *) 
         ; IF LHasErrors OR LAsSemDecl = NIL OR NOT DoBuildFsTree 
           THEN RETURN NIL 
           ELSE 
             LangInfo . SemMapRef ^ [ WFsChildClass . NodeNo ] . SemRef 
               := NEW 
                    ( LdlSemantics . SemRefTyp 
                    , NodeNo := WFsChildClass . NodeNo 
                    , RefDeclId := LAsDeclNodeNo 
                    , RefTok := LAsSemDecl. DeclTok 
                    ) 
           ; LChildIndentCode 
               := Indenter 
                    ( LFsChildren 
                        [ Ldl1Child . FsChildPlain_ChildIndenter ] 
                    , NewLineIndentCode := NewLineIndentCode
                    , IndentCode := CurrentIndentCode 
                    ) 
           ; RETURN FsTreeForEstChild
                      ( FsChild 
                      , LAsSemDecl. DeclTok 
                      , IndentCode := CurrentIndentCode 
                      , ChildIndentCode := LChildIndentCode 
                      , FsKindForLineBreak := FsKindForLineBreak
                      ) 
           END (* IF *) 
        END (* WITH *)
      END FsTreeForFsChildPlain 

  ; PROCEDURE FsTreeForCondConstruct 
      ( AltNode : AstView . AstRefTyp 
      ; IndentCode : LangUtil . IndentCodeTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ; VAR EstChildFmtNo : EstHs . FmtNoTyp 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 
    (* Either a CASE or a conditional format construct. *) 

    = VAR LFsChildrenNode : AstView . AstRefTyp 
    ; VAR LClassNode : AstView . AstRefTyp := AstView . AstRefNull  
         (* ^Meaningful only if PredicateKind = PredicateKindInClass *) 
    ; VAR LSaveBldFsNodeForEstDescendant : LangUtil . FsNodeRefTyp 
    ; VAR LChildren 
          : ARRAY [ 0 .. Ldl1Child . FsCondMember_Children ] 
            OF AstView . AstRefTyp 
    ; VAR LLdlFsChild : AstView . AstRefTyp 
    ; VAR LLdlFsChildExists : BOOLEAN 
    ; VAR LTempFsChildrenRef : LangUtil . FsChildrenArrayRefTyp 
    ; VAR LNextFsChildNo : LangUtil . FsChildNoTyp 
    ; VAR LIndentCode : LangUtil . IndentCodeTyp 
    ; VAR LResult : LangUtil . FsNodeRefTyp 
    ; VAR LLeadingChildCt : LbeStd . EstChildNoTyp := 0 
    ; VAR LFsNodeForEstChild : LangUtil . FsNodeRefTyp := NIL 
    ; VAR LLdlEstChild : AstView . AstRefTyp := AstView . AstRefNull 
    ; VAR LDeclNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LFsEstDescendant : LangUtil . FsNodeRefTyp 
    ; VAR LPredicate : LangUtil . PredicateTyp 
    ; VAR LDoParse : BOOLEAN := FALSE 
    ; VAR LMemberSet : IntSets . T 
    ; VAR LTok : LbeStd . TokTyp  
    ; VAR LWasFound : BOOLEAN 
    ; VAR LHasErrors := FALSE 
    ; VAR LEstChildOpt : ChildOptTyp 
    ; VAR LEstChildIsOptional : BOOLEAN 

    ; BEGIN (* FsTreeForCondConstruct *) 
        BldIsInFirstLine := BldCondStartsInFirstLine 
      ; EstChildFmtNo := EstHs . FmtNoNull 
      ; LClassNode := AstView . AstRefNull 
      ; AstView . GetChildren ( AltNode , LChildren , LangInfo . LdlLang ) 

      (* Compute LPredicate. *) 
      ; CASE EstUtil . EstTok ( AltNode . NodeRef ) <* NOWARN *>
        OF Ldl1Tok . FsCondAbsent 
        => LPredicate . PredicateKind 
             := PredicateKindTyp . PredicateKindAbsent 
        ; LFsChildrenNode := LChildren [ Ldl1Child . FsCondAbsent_Children ] 
        ; LDoParse 
            := AstView . IsNull 
                 ( LChildren [ Ldl1Child . FsCondAbsent_NoParse ] ) 

        | Ldl1Tok . FsCondPresent 
        => LPredicate . PredicateKind 
             := PredicateKindTyp . PredicateKindPresent 
        ; LFsChildrenNode := LChildren [ Ldl1Child . FsCondPresent_Children ] 
        ; LDoParse 
            := AstView . IsNull 
                 ( LChildren [ Ldl1Child . FsCondPresent_NoParse ] ) 

        | Ldl1Tok . FsCondEmpty 
        => LPredicate . PredicateKind 
             := PredicateKindTyp . PredicateKindEmptyList 
        ; LFsChildrenNode := LChildren [ Ldl1Child . FsCondEmpty_Children ] 
        ; LDoParse 
            := AstView . IsNull 
                 ( LChildren [ Ldl1Child . FsCondEmpty_NoParse ] ) 

        | Ldl1Tok . FsCondNonempty 
        => LPredicate . PredicateKind 
              := PredicateKindTyp . PredicateKindNonemptyList 
        ; LFsChildrenNode := LChildren [ Ldl1Child . FsCondNonempty_Children ] 
        ; LDoParse 
            := AstView . IsNull 
                 ( LChildren [ Ldl1Child . FsCondNonempty_NoParse ] ) 

        | Ldl1Tok . FsCondPlural 
        => LPredicate . PredicateKind 
             := PredicateKindTyp . PredicateKindPluralList 
        ; LFsChildrenNode := LChildren [ Ldl1Child . FsCondPlural_Children ] 
        ; LDoParse 
            := AstView . IsNull 
                 ( LChildren [ Ldl1Child . FsCondPlural_NoParse ] )

        | Ldl1Tok . FsCondNonplural 
        => LPredicate . PredicateKind 
             := PredicateKindTyp . PredicateKindNonpluralList 
        ; LFsChildrenNode 
            := LChildren [ Ldl1Child . FsCondNonplural_Children ] 
        ; LDoParse 
            := AstView . IsNull 
                 ( LChildren [ Ldl1Child . FsCondNonplural_NoParse ] ) 

        | Ldl1Tok . FsCondMember 
        => LPredicate . PredicateKind 
             := PredicateKindTyp . PredicateKindInClass 
        ; LFsChildrenNode := LChildren [ Ldl1Child . FsCondMember_Children ] 
        ; LDoParse 
            := AstView . IsNull 
                 ( LChildren [ Ldl1Child . FsCondMember_NoParse ]) 
        ; LClassNode := LChildren [ Ldl1Child . FsCondMember_Class ] 
 
        | Ldl1Tok . FsCondElse 
        => (* This is an ELSE construct. *) 
          LPredicate . PredicateKind 
            := PredicateKindTyp . PredicateKindTrue 
        ; LFsChildrenNode := LChildren [ Ldl1Child . FsCondElse_Children ] 
        ; LDoParse 
            := AstView . IsNull ( LChildren [ Ldl1Child . FsCondElse_NoParse ] )

(* TODO: The following case alternative should be removable after completion 
         of the NOPARSE addition to Ldl1.
*) 
        | Ldl1Tok . FsFixedChildList 
        => (* ELSE construct in an older Ldl1 syntax. before NOPARSE . *) 
          LPredicate . PredicateKind 
            := PredicateKindTyp . PredicateKindTrue 
        ; LFsChildrenNode := AltNode  
        ; LDoParse := TRUE 
          
        END (* CASE *)
 
      ; LSaveBldFsNodeForEstDescendant := BldFsNodeForEstDescendant 
        (* ^I hate doing stacking this way, but it saves lots of rework. *) 
      ; BldFsNodeForEstDescendant := NIL 
      ; LFsEstDescendant := NIL 
      ; BldLdlFsEstChild := AstView . AstRefNull 
      ; LPredicate . PredicateClass := LbeStd . Tok__Null 
      ; IF NOT BldNextAsChildExists 
        THEN 
          SemError2 
            ( AltNode . NodeNo 
            , BldAsIdentNode . NodeNo 
            , AFT . E_AbstractNodeHasNoChildForThisConditionalFormatSubtree  
            ) 
        ; BldFsNodeForEstDescendant := LSaveBldFsNodeForEstDescendant 
        ; LResult := NIL 
(* TODO: ^Move this test out to root calls on FsTreeForCondConstruct, so we
         don't emit the message once for each alternative.
*) 
        ELSE 

          (* Semantics checks on LPredicate. *) 
          CASE LPredicate . PredicateKind 
          OF PredicateKindTyp . PredicateKindTrue 
          => 

          | PredicateKindTyp . PredicateKindAbsent 
          => 

          | PredicateKindTyp . PredicateKindPresent 
          => IF LdlSemantics . AMemberIsInSomeRange 
                  ( LangInfo 
                  , BldNextAsChildClass 
                  , LangUtilLo . TokClassFirstTok 
                      ( LangInfo , TokClassTyp . TokClassAsPlus ) 
                  , LangUtilLo . TokClassLastTok 
                      ( LangInfo , TokClassTyp . TokClassAsStarTrailing ) 
                  , LangUtilLo . TokClassFirstTok 
                      ( LangInfo , TokClassTyp . TokClassVarTerm ) 
                  , LangUtilLo . TokClassLastTok 
                      ( LangInfo , TokClassTyp . TokClassVarTerm  ) 
                  ) 
             THEN 
               BldPrincipalChildTokSet := IntSets . Empty ( ) 
             END (* IF *) 

          | PredicateKindTyp . PredicateKindEmptyList 
          , PredicateKindTyp . PredicateKindNonemptyList 
          , PredicateKindTyp . PredicateKindPluralList 
          , PredicateKindTyp . PredicateKindNonpluralList 
          =>  

          | PredicateKindTyp . PredicateKindInClass 
          => TextIntSymbolTable . Find 
               ( LangInfo . SymbolTable 
               , SharedStrings . ToText ( LClassNode . NodeRef ) 
               , LWasFound 
               , LDeclNodeNo 
               ) 
          ; IF LWasFound 
            THEN 
              TYPECASE LangInfo . SemMapRef ^ [ LDeclNodeNo ] . SemRef 
              OF LdlSemantics . SemDeclAsNodeTyp ( TDecl ) 
              => LangInfo . SemMapRef ^ [ LClassNode . NodeNo ] . SemRef 
                   := NEW 
                        ( LdlSemantics . SemRefTyp 
                        , NodeNo := LClassNode . NodeNo 
                        , RefDeclId := TDecl . NodeNo 
                        , RefTok := TDecl . DeclTok 
                        ) 
              ; LPredicate . PredicateClass := TDecl . DeclTok
(* FIX: Take the intersection with the abstract child class and with
        previous alternatives.  Warn if empty. *)  
              | LdlSemantics . SemFirstOccClassTyp ( TDecl ) 
              => LangInfo . SemMapRef ^ [ LClassNode . NodeNo ] . SemRef 
                   := NEW 
                        ( LdlSemantics . SemRefTyp 
                        , NodeNo := LClassNode . NodeNo 
                        , RefDeclId := TDecl . NodeNo 
                        , RefTok := TDecl . DeclTok 
                        ) 
              ; LPredicate . PredicateClass := TDecl . DeclTok 
(* FIX: Take the intersection with the abstract child class and with
        previous alternatives.  Warn if empty. *)  
              | LdlSemantics . SemFirstOccStringTyp ( TDecl ) 
(* CHECK: How can it be a string?  A string is always an insertion token,
          but we want an abstract node or class thereof.
*) 
              => LangInfo . SemMapRef ^ [ LClassNode . NodeNo ] . SemRef 
                   := NEW 
                        ( LdlSemantics . SemRefTyp 
                        , NodeNo := LClassNode . NodeNo 
                        , RefDeclId := TDecl . NodeNo 
                        , RefTok := TDecl . DeclTok 
                        ) 
              ; LPredicate . PredicateClass := TDecl . DeclTok 
(* FIX: Take the intersection with the abstract child class and with
        previous alternatives.  Warn if empty. *)  
              ELSE 
                SemError2 
                  ( LClassNode . NodeNo 
                  , LDeclNodeNo 
                  , AFT . E_MemberMustBeOfAClassAbstractNodeOrString 
                  ) 
              ; LHasErrors := TRUE 
              END (* TYPECASE *) 
            ; IF NOT LHasErrors 
              THEN
                LMemberSet := IntSets . Intersection 
                  ( BldPrincipalChildTokSet
                  , LdlSemantics . AsTokSetOfTok 
                      ( LangInfo , LPredicate . PredicateClass) 
                  ) 
              ; BldPrincipalChildTokSet 
                  := IntSets . Difference 
                       ( BldPrincipalChildTokSet , LMemberSet ) 
              END (* IF *) 
            ELSE 
              SemError ( LClassNode . NodeNo , AFT . E_NotDeclared ) 
            ; LHasErrors := TRUE 
            END (* IF *) 
          ELSE 
          END (* CASE *) 

        (* Go through children. *) 
        ; BldPredicate := LPredicate (* For use by FsTreeForEstChild. *) 
        ; AstView . FirstChild 
            ( LFsChildrenNode , LLdlFsChild , LLdlFsChildExists ) 
        ; LTempFsChildrenRef 
            := NEW 
                 ( LangUtil . FsChildrenArrayRefTyp 
                 , AstView . FastExtravagantChildCt ( LFsChildrenNode ) 
                 ) 
        ; LNextFsChildNo := 0 
        ; LIndentCode := IndentCode 
        ; WHILE LLdlFsChildExists 
          DO WITH WFsChild = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
             DO WFsChild 
                  := FsTreeForFsChild 
                       ( LLdlFsChild 
                       , LangUtil . IndentCodeInitial 
                       , (* IN OUT *) CurrentIndentCode := LIndentCode 
                       , FsKindForLineBreak := FsKindForLineBreak
                       ) 
             ; IF WFsChild # NIL 
               THEN 
                 IF NOT WFsChild . FsKind IN LangUtil . FsKindSetFormatting 
                 THEN (* It's the Est child, or subtree containing it. *) 
                   IF LFsNodeForEstChild = NIL 
                   THEN (* It's the first Est-containing child. *)  
                     LLeadingChildCt := LNextFsChildNo 
                   ; LFsNodeForEstChild := WFsChild 
                   ; LLdlEstChild := LLdlFsChild 
                   ; IF BldFsNodeForEstDescendant = NIL 
                     THEN (* No Est child was inside it. *) 
                       SemError2 
                         ( LLdlFsChild . NodeNo 
                         , LLdlEstChild . NodeNo 
                         , AFT . E_NoEstChildIsInsidePredicate  
                         ) 
                     ; LHasErrors := TRUE 
                     END (* IF *) 
                   ; IF BldFsCondAltRef = NIL 
                        (* No further alternative given in Ldl. *) 
                        AND BldFsNodeForEstDescendant # NIL 
                            (* This one has an Est child. *)
                        AND LPredicate . PredicateKind 
                            # PredicateKindTyp . PredicateKindTrue
                            (* This one is conditional. *)  
                     THEN (* We are inside a conditional format construct.
                             The one alternative to this predicate case is
                             the referent of BldFsNodeForEstDescendant.  But we
                             need a copy of it, because its indentation and
                             first-line-occupancy may differ. 
                          *) 
                       BldFsCondAltRef 
                         := LangUtil . CopyOfFsNode 
                              ( BldFsNodeForEstDescendant ) 
                     ; BldFsCondAltRef . FsIndentCode := IndentCode 
                     ; BldFsCondAltRef . FsChildIndentCode := IndentCode 
                     ; BldFsCondAltRef . FsIsInFirstLine 
                         := BldCondStartsInFirstLine 
                     ; BldFsCondAltRef . FsCondAltRef := NIL 
                       (* ^Terminate the alternative list. *) 
(* TODO: Compute BldFsCondAltRef . FsEstChildOpt, which can be different. *) 
                     
(* TODO: Rework this when we are taking OptionIds out of FsNodes. *) 
(* For later: ; IF BldFsCondAltRef . FsEstChildOpt = ChildOptTyp . OptOptional*)
                     ; IF BldFsCondAltRef . FsEstChildOpt 
                          # ChildOptTyp . OptRequired  
                       THEN (* We need an option id for generated CS. *) 
                         BldFsNodeForEstDescendant . FsOptionIds [ TRUE ] 
                           := BldNextCondId 
                         (* Put the new OptionId in the old FsNode and let the
                            copied old OptionId stay in the new FsNode.  This
                            gets the order right, w/ or w/o reversal. 
                         *) 
                       ; IncOptionId ( (* VAR *) BldNextCondId , "generated" ) 
                       ELSE 
                         BldFsCondAltRef . FsOptionIds [ TRUE ] 
                           := LRTable . OptionIdNull 
                       END (* IF *) 

                     END (* IF *) 
                   ELSE (* We previously saw an Est-containing child. *)  
                     SemError2 
                       ( LLdlFsChild . NodeNo 
                       , LLdlEstChild . NodeNo 
                       , AFT . E_ExtraEstChildIsInsidePredicate 
                       ) 
                   ; LHasErrors := TRUE 
                   END (* IF *) 
                 END (* IF *) 
               END (* IF *) 
             ; INC ( LNextFsChildNo , ORD ( WFsChild # NIL ) )   
             END (* WITH *) 
          ; AstView . NextChild 
              ( LFsChildrenNode , LLdlFsChild , LLdlFsChildExists ) 
          END (* WHILE *) 

        ; IF LFsNodeForEstChild = NIL 
          THEN 
            SemError 
              ( LFsChildrenNode . NodeNo 
              , AFT . E_ConditionalFormatSubtreeHasNoTreeChild 
              ) 
          ; LHasErrors := TRUE 
          END (* IF *) 
        ; IF LHasErrors 
          THEN 
            BldFsNodeForEstDescendant := LSaveBldFsNodeForEstDescendant 
          ; LResult := NIL 
          ELSE 
            IF BldFsNodeForEstDescendant = NIL 
            THEN
              LTok := LbeStd . Tok__Null  
            ; LEstChildOpt := ChildOptTyp . OptRequired 
            ELSE 
              EstChildFmtNo := BldFsNodeForEstDescendant . FsFmtNo  
            ; LTok := BldFsNodeForEstDescendant . FsTok 
            ; LEstChildOpt := BldFsNodeForEstDescendant . FsEstChildOpt
            END (* IF *) 
          ; LEstChildIsOptional := LEstChildOpt # ChildOptTyp . OptRequired 
          ; LResult 
              := NEW 
                   ( LangUtil . FsNodeRefTyp 
                   , FsFmtNo := EstChildFmtNo 
                   , FsChildren 
                       := NEW 
                            ( LangUtil . FsChildrenArrayRefTyp 
                            , LNextFsChildNo 
                            ) 
                   , FsTok := LTok 
                   , FsKind := FsKindTyp . FsKindCondFmt 
                   , FsCondPredicate := LPredicate 
                   , FsCondDoParse := LDoParse 
                   , FsLeadingChildCt := LLeadingChildCt 
                   , FsLeftFmtNo := LTempFsChildrenRef ^ [ 0 ] . FsLeftFmtNo 
                   , FsRightFmtNo 
                       := LTempFsChildrenRef ^ [ LNextFsChildNo - 1 ] 
                          . FsRightFmtNo 
                   , FsEstDescendantRef := BldFsNodeForEstDescendant 
                   , FsEstChildIsOptional := LEstChildIsOptional
                   , FsEstChildOpt := LEstChildOpt
                   , FsCondAltRef := BldFsCondAltRef 
                   , FsIsInsideList := BldIsList
                   , FsIsInsideCondFmt := TRUE 
                   , FsIsInFirstLine := BldIsInFirstLine 
                   , FsLdlNodeNo := AltNode . NodeNo 
                   , FsFormatsEmpty := UbUnknown 
                   ) 
          ; IF BldFsNodeForEstDescendant # NIL 
            THEN
              LResult . FsOptionIds := BldFsNodeForEstDescendant . FsOptionIds  
            END (* IF *)  
          ; LResult . FsChildren ^ 
              := SUBARRAY ( LTempFsChildrenRef ^ , 0 , LNextFsChildNo ) 
          ; BldFsNodeForEstDescendant := LSaveBldFsNodeForEstDescendant 
          END (* IF *) 
        END (* IF *) 
      ; IF BldIsInFirstLine 
        THEN
          BldACondEndsInFirstLine := TRUE 
        ELSE
          BldACondEndsNotInFirstLine := TRUE 
        END (* IF *) 
      ; CASE LPredicate . PredicateKind 
        OF PredicateKindTyp . PredicateKindAbsent
        => BldAbsentAltSeen := TRUE 
        | PredicateKindTyp . PredicateKindPresent
        => BldPresentAltSeen := TRUE 
        ELSE 
        END (* CASE *) 

      ; RETURN LResult 
      END FsTreeForCondConstruct 

  ; PROCEDURE FsTreeForFsSubtree 
      ( FsChildrenNode : AstView . AstRefTyp 
      ; FsKind : FsKindTyp 
      ; IndentCode : LangUtil . IndentCodeTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ; IsStart : BOOLEAN 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LLdlFsChild : AstView . AstRefTyp 
    ; VAR LLdlFsChildExists : BOOLEAN 
    ; VAR LTempFsChildrenRef : LangUtil . FsChildrenArrayRefTyp 
    ; VAR LNextFsChildNo : LangUtil . FsChildNoTyp 
    ; VAR LIndentCode : LangUtil . IndentCodeTyp 
    ; VAR LFmtNo : EstHs . FmtNoTyp 
    ; VAR LLeftFmtNo : EstHs . FmtNoTyp 
    ; VAR LRightFmtNo : EstHs . FmtNoTyp 
    ; VAR LLeadingChildCt : LbeStd . EstChildNoTyp := 0 
    ; VAR LFsNodeForEstChild : LangUtil . FsNodeRefTyp := NIL 
    ; VAR LLdlEstChild : AstView . AstRefTyp := AstView . AstRefNull 
    ; VAR LResult : LangUtil . FsNodeRefTyp 
    ; VAR LSavePrevLineBreakRef : LangUtil . FsNodeRefTyp 
    ; VAR LHasErrors : BOOLEAN := FALSE 

    ; BEGIN (* FsTreeForFsSubtree  *) 
        LSavePrevLineBreakRef := BldPrevLineBreakRef 
      ; BldPrevLineBreakRef := NIL 
      ; AstView . FirstChild 
          ( FsChildrenNode , LLdlFsChild , LLdlFsChildExists ) 
      ; LTempFsChildrenRef 
          := NEW 
               ( LangUtil . FsChildrenArrayRefTyp 
               , AstView . FastExtravagantChildCt ( FsChildrenNode ) 
                 + 2 * ORD ( IsStart ) 
               ) 
      ; LNextFsChildNo := 0 
      ; LIndentCode := IndentCode

      (* Maybe a BOI node for a start Ast node. *)  
      ; IF IsStart AND NOT LbeStd . UseAugmentTrees 
        THEN 
          WITH WFsNodeRef = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
          DO 
            WFsNodeRef 
              := NEW 
                   ( LangUtil . FsNodeRefTyp 
                   , FsFmtNo := AssignFmtNo ( ) 
                   , FsIndentCode := IndentCode 
                   , FsTok := LbeStd . Tok__BegOfImage 
                   , FsKind := FsKindTyp . FsKindBegOfImage 
                   , FsIsInsideList := BldIsList 
                   , FsIsInsideCondFmt := FALSE 
                   , FsIsInFirstLine := BldIsInFirstLine  
                   , FsLdlNodeNo := FsChildrenNode . NodeNo 
                   , FsFormatsEmpty := UbTrue   
                   ) 
          ; SetLeftAndRightFmtNo ( WFsNodeRef ) 
          END (* WITH *) 
        ; INC ( LNextFsChildNo ) 
        END (* IF *) 

      (* Do the children. *) 
      ; WHILE LLdlFsChildExists 
        DO WITH WFsChild = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
           DO WFsChild 
                := FsTreeForFsChild 
                     ( LLdlFsChild 
                     , LangUtil . IndentCodeInitial 
                     , (* IN OUT *) CurrentIndentCode := LIndentCode 
                     , FsKindForLineBreak := FsKindForLineBreak
                     ) 
           ; IF BldCondFmtDepth > 0 
                AND WFsChild # NIL 
                AND NOT WFsChild . FsKind IN LangUtil . FsKindSetFormatting 
             THEN (* This subtree is the Est-containing child of a CondFmt. *) 
               IF LFsNodeForEstChild = NIL 
               THEN 
                 LLeadingChildCt := LNextFsChildNo 
               ; LFsNodeForEstChild := WFsChild 
               ; LLdlEstChild := LLdlFsChild 
               ELSE 
                 SemError2 
                   ( LLdlFsChild . NodeNo 
                   , LLdlEstChild . NodeNo 
                   , AFT . E_OnlyOneNonFormatterChildAllowedWhenInsideConditionalFormat
                   ) 
               ; LHasErrors := TRUE 
               END (* IF *) 
             END (* IF *) 
           ; INC ( LNextFsChildNo , ORD ( WFsChild # NIL ) ) 
           END (* WITH *) 
        ; AstView . NextChild 
            ( FsChildrenNode , LLdlFsChild , LLdlFsChildExists ) 
        END (* WHILE *) 
      ; IF LHasErrors 
        THEN LResult := NIL 
        ELSE 

          (* Maybe an EOI node for a start Ast node. *)  
          IF IsStart AND NOT LbeStd . UseAugmentTrees 
          THEN 
            WITH WFsNodeRef = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
            DO WFsNodeRef 
                := NEW 
                     ( LangUtil . FsNodeRefTyp 
                     , FsFmtNo := AssignFmtNo ( ) 
                     , FsIndentCode := IndentCode 
                     , FsTok := LbeStd . Tok__EndOfImage 
                     , FsKind := FsKindTyp . FsKindEndOfImage 
                     , FsIsInsideList := BldIsList
                     , FsIsInsideCondFmt := FALSE  
                     , FsIsInFirstLine := BldIsInFirstLine  
                     , FsIsRightOfEstListChild := BldIsRightOfEstListChild
                       (* ^This could only happen if we ever change to allow
                          a list Est node to be autonomous. *) 
                     , FsLdlNodeNo := FsChildrenNode . NodeNo 
                     , FsFormatsEmpty := UbTrue   
                     ) 
            ; SetLeftAndRightFmtNo ( WFsNodeRef ) 
            END (* WITH *) 
          ; INC ( LNextFsChildNo ) 
          END (* IF *) 

        ; IF LNextFsChildNo = 0 
          THEN 
            LLeftFmtNo := EstHs . FmtNoNull 
          ; LRightFmtNo := EstHs . FmtNoNull 
          ELSE 
            TYPECASE LTempFsChildrenRef ^ [ 0 ] 
            OF NULL 
            => LLeftFmtNo := EstHs . FmtNoNull 
            | LangUtil . FsNodeRefTyp ( TFsNodeRef ) 
            => LLeftFmtNo := TFsNodeRef . FsLeftFmtNo 
            END (* TYPECASE *) 
          ; TYPECASE LTempFsChildrenRef ^ [ LNextFsChildNo - 1 ] 
            OF NULL 
            => LRightFmtNo := EstHs . FmtNoNull 
            | LangUtil . FsNodeRefTyp ( TFsNodeRef ) 
            => LRightFmtNo := TFsNodeRef . FsRightFmtNo 
            END (* TYPECASE *) 
          END (* IF *) 
        ; IF BldFsNodeForEstDescendant = NIL 
          THEN LFmtNo := LLeftFmtNo  
          ELSE
            LFmtNo := BldFsNodeForEstDescendant . FsFmtNo 
          END 

        (* The result FsSubtree node. *) 
        ; LResult 
            := NEW 
                 ( LangUtil . FsNodeRefTyp 
                 , FsFmtNo := LFmtNo 
                 , FsIndentCode := IndentCode 
                 , FsChildren 
                     := NEW ( LangUtil . FsChildrenArrayRefTyp 
                            , LNextFsChildNo 
                            ) 
                 , FsKind := FsKind 
                 , FsLeftFmtNo := LLeftFmtNo 
                 , FsRightFmtNo := LRightFmtNo 
                 , FsLeadingChildCt := LLeadingChildCt 
                 , FsEstDescendantRef := BldFsNodeForEstDescendant 
                 , FsIsInsideList := BldIsList
                 , FsIsInsideCondFmt := BldCondFmtDepth > 0 
                 , FsLdlNodeNo := FsChildrenNode . NodeNo 
                 , FsFormatsEmpty := UbUnknown 
                 ) 
        ; IF BldCondFmtDepth > 0 AND BldFsNodeForEstDescendant # NIL 
          THEN
            LResult . FsOptionIds := BldFsNodeForEstDescendant . FsOptionIds  
          END (* IF *)  
        ; LResult . FsChildren ^ 
            := SUBARRAY ( LTempFsChildrenRef ^ , 0 , LNextFsChildNo ) 
        ; IF BldPrevLineBreakRef # NIL 
          THEN 
            BldPrevLineBreakRef . FsLineBreakToReachFmtNo := LRightFmtNo 
          END (* IF *) 
        END (* IF *) 
      ; BldPrevLineBreakRef := LSavePrevLineBreakRef 
      ; RETURN LResult 
      END FsTreeForFsSubtree 

  ; PROCEDURE ConsumeAlternative ( ) 
    RAISES { AssertionFailure } 

    = VAR LWasFound : BOOLEAN 

    ; BEGIN (* ConsumeAlternative *) 
        IF NOT AstView . IsNull ( BldNextAltNode ) 
        THEN 
          IF BldNextAltNode . NodeNo = BldElseNode . NodeNo 
          THEN (* This was the ELSE node. No more alternatives exist. *) 
            BldNextAltNode := AstView . AstRefNull 
          ELSE 
            AstView . NextChild ( BldAltlistNode , BldNextAltNode , LWasFound )
          ; IF NOT LWasFound 
            THEN
              BldNextAltNode := BldElseNode (* Which could be Null. *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END ConsumeAlternative 

  ; PROCEDURE FsTreeForFsChildCondFmt 
      ( LdlFsCondFmtNode : AstView . AstRefTyp 
      ; VAR (* IN OUT *) CurrentIndentCode : LangUtil . IndentCodeTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LResult : LangUtil . FsNodeRefTyp 
    ; VAR LEstChildFmtNo : EstHs . FmtNoTyp 

    ; BEGIN (* FsTreeForFsChildCondFmt *) 
        LResult := NIL  
      ; INC ( BldCondFmtDepth ) 
      ; IF BldCondFmtDepth > 1 
        THEN 
          SemError 
            ( LdlFsCondFmtNode . NodeNo 
            , AFT . E_NestedConditionalFormattingConstruct 
            ) 
        ELSE 
          BldAltlistNode := AstView . AstRefNull 
        ; BldNextAltNode := AstView . AstRefNull 
        ; BldElseNode := AstView . AstRefNull (* Defensive. *) 
        ; BldCondIndentCode := CurrentIndentCode 
        ; BldCondStartsInFirstLine := BldIsInFirstLine 
        ; BldACondEndsInFirstLine := FALSE 
        ; BldACondEndsNotInFirstLine := FALSE 
        ; IF BldNextAsChildExists 
          THEN 
            BldPrincipalChildTokSet
              := LdlSemantics . AsTokSetOfAstRef 
                   ( LangInfo , BldNextAsChildClass ) 
          ; BldAbsentAltSeen := FALSE 
          ; BldPresentAltSeen := FALSE 
          ; LResult 
              := FsTreeForCondConstruct 
                   ( AltNode := LdlFsCondFmtNode  
                   , IndentCode := CurrentIndentCode 
                   , FsKindForLineBreak := FsKindForLineBreak
                   , (* VAR *) EstChildFmtNo := LEstChildFmtNo 
                   ) 
                 (* ^There will be no recursive call on this. *) 
          ; IF LResult # NIL AND LResult . FsEstChildIsOptional 
            THEN 
              IncOptionId ( (* VAR *) BldNextOptionId , "handwritten" ) 
            END (* IF *)
          ; ConsumeAsChild ( )
          ELSE 
            SemError2 
              ( LdlFsCondFmtNode . NodeNo 
              , BldAsIdentNode . NodeNo 
              , AFT . E_AbstractNodeHasNoPrincipalChildForThisCondition
              ) 
          END (* IF *) 
        END (* IF *) 
      ; DEC ( BldCondFmtDepth ) 
      ; RETURN LResult 
      END FsTreeForFsChildCondFmt 

  ; PROCEDURE FsTreeForFsChildCase
      ( LdlFsCaseNode : AstView . AstRefTyp 
      ; VAR (* IN OUT *) CurrentIndentCode : LangUtil . IndentCodeTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LResult : LangUtil . FsNodeRefTyp 
    ; VAR LEstChildFmtNo : EstHs . FmtNoTyp 
    ; VAR L1stAltNode : AstView . AstRefTyp
    ; VAR LWasFound : BOOLEAN  
    ; VAR LChildren 
          : ARRAY [ 0 .. Ldl1Child . FsChildCase_Else ] 
            OF AstView . AstRefTyp 

    ; BEGIN (* FsTreeForFsChildCase *) 
        LResult := NIL  
      ; INC ( BldCondFmtDepth ) 
      ; IF BldCondFmtDepth > 1 
        THEN 
          SemError 
            ( LdlFsCaseNode . NodeNo 
            , AFT . E_NestedCaseConstruct 
            ) 
        ELSE 
          AstView . GetChildren 
            ( LdlFsCaseNode , LChildren , LangInfo . LdlLang ) 
        ; BldAltlistNode := LChildren [ Ldl1Child . FsChildCase_Alternatives ] 
        ; BldElseNode := LChildren [ Ldl1Child . FsChildCase_Else ] 
        (* Ldl1 syntax requires/ensures at least one regular alternative, but 
           this is coded more generally to handle empty alternative list and 
           absent ELSE. 
        *) 
        ; AstView . FirstChild ( BldAltlistNode , L1stAltNode , LWasFound ) 
        ; IF LWasFound 
          THEN 
            AstView . NextChild ( BldAltlistNode , BldNextAltNode , LWasFound )
          ; IF NOT LWasFound 
            THEN
              BldNextAltNode := BldElseNode (* Which may be null. *) 
            END (* IF *) 
          ELSE 
            L1stAltNode := BldElseNode
          ; BldNextAltNode := AstView . AstRefNull  
          END (* IF *) 
        ; IF AstView . IsNull ( L1stAltNode ) 
          THEN
            SemError 
              ( LdlFsCaseNode . NodeNo 
              , AFT . E_CaseWithNoAlternatives 
              ) 
          ELSE 
            BldCondIndentCode := CurrentIndentCode 
          ; BldCondStartsInFirstLine := BldIsInFirstLine 
          ; BldACondEndsInFirstLine := FALSE 
          ; BldACondEndsNotInFirstLine := FALSE 
          ; IF BldNextAsChildExists 
            THEN 
              BldPrincipalChildTokSet
                := LdlSemantics . AsTokSetOfAstRef 
                     ( LangInfo , BldNextAsChildClass ) 
            ; BldAbsentAltSeen := FALSE 
            ; BldPresentAltSeen := FALSE 
            ; LResult 
                := FsTreeForCondConstruct 
                     ( AltNode := L1stAltNode 
                     , IndentCode := CurrentIndentCode 
                     , FsKindForLineBreak := FsKindForLineBreak
                     , (* VAR *) EstChildFmtNo := LEstChildFmtNo 
                     ) 
            ; BldIsInFirstLine := NOT BldACondEndsNotInFirstLine 
            ; IF BldACondEndsInFirstLine AND BldACondEndsNotInFirstLine 
              THEN 
(* TODO: Postpone this warning until we see if a line break is the next
         thing, in which case, it doesn't matter. *) 
                SemError 
                  ( LdlFsCaseNode . NodeNo 
                  , AFT . W_Ldl1FsTrees_alternatives_are_inconsistent_about_having_line_breaks
                  ) 
              END (* IF *) 
            ; IF LResult . FsEstChildIsOptional 
              THEN 
                IncOptionId ( (* VAR *) BldNextOptionId , "handwritten" ) 
              END (* IF *)
            ; ConsumeAsChild ( )
            ELSE 
              SemError2 
                ( LdlFsCaseNode . NodeNo 
                , BldAsIdentNode . NodeNo 
                , AFT . E_AbstractNodeHasNoPrincipalChildForThisCaseConstruct
                ) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      ; DEC ( BldCondFmtDepth ) 
      ; RETURN LResult 
      END FsTreeForFsChildCase 

  ; PROCEDURE FsTreeForFsChild 
      ( LdlFsChildNode : AstView . AstRefTyp 
      ; NewLineIndentCode : LangUtil . IndentCodeTyp 
(* TODO: Eliminate this parameter, here and in a few other procs it
         gets passed to.  This value is always LangUtil . IndentCodeInitial
         (* But, is that right? *) 
*) 
      ; VAR (* IN OUT *) CurrentIndentCode : LangUtil . IndentCodeTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LResult : LangUtil . FsNodeRefTyp 

    ; BEGIN (* FsTreeForFsChild *) 
        CASE EstUtil . EstTok ( LdlFsChildNode . NodeRef ) 
        OF Ldl1Tok . FsHorizSubtree 
        , Ldl1Tok . FsDefaultSubtree 
        => RETURN 
             FsTreeForFsSubtree 
               ( AstView . Child 
                   ( LdlFsChildNode 
                   , Ldl1Child . FsHorizSubtree_Children 
                   (* This depends on FsHorizSubtree_Children and 
                      FsDefaultSubtree_Children being equal. *) 
                   , LangInfo . LdlLang 
                   ) 
               , FsKindTyp . FsKindSubtreeHoriz 
               , CurrentIndentCode 
               , FsKindTyp . FsKindLineBreakOpt 
               , IsStart := FALSE 
               ) 

        | Ldl1Tok . FsVertSubtree 
        => RETURN 
             FsTreeForFsSubtree 
               ( AstView . Child 
                   ( LdlFsChildNode 
                   , Ldl1Child . FsVertSubtree_Children 
                   , LangInfo . LdlLang 
                   ) 
               , FsKindTyp . FsKindSubtreeVert 
               , CurrentIndentCode 
               , FsKindTyp . FsKindLineBreakReqd
               , IsStart := FALSE 
               ) 

        | Ldl1Tok . FsFillSubtree 
        => RETURN 
             FsTreeForFsSubtree 
               ( AstView . Child 
                   ( LdlFsChildNode 
                   , Ldl1Child . FsFillSubtree_Children 
                   , LangInfo . LdlLang 
                   ) 
               , FsKindTyp . FsKindSubtreeFill 
               , CurrentIndentCode 
               , FsKindTyp . FsKindLineBreakOpt 
               , IsStart := FALSE 
               ) 

        | Ldl1Tok . BolIndenter
        , Ldl1Tok . ParentIndenter
        , Ldl1Tok . PosIndenter
        , Ldl1Tok . BolPlusIndenter
        , Ldl1Tok . ParentPlusIndenter
        , Ldl1Tok . PosPlusIndenter
        , Ldl1Tok . BolMinusIndenter
        , Ldl1Tok . ParentMinusIndenter
        , Ldl1Tok . PosMinusIndenter
        => CurrentIndentCode 
             := Indenter 
                  ( LdlFsChildNode 
                  , NewLineIndentCode := NewLineIndentCode 
                  , IndentCode := CurrentIndentCode 
                  ) 
        ; RETURN NIL 

        | Ldl1Tok . LineBreak 
        => CurrentIndentCode := NewLineIndentCode 
        ; LResult 
            := NEW 
                 ( LangUtil . FsNodeRefTyp 
                 , FsFmtNo := AssignFmtNo ( ) 
                 , FsIndentCode := CurrentIndentCode 
                 , FsKind := FsKindForLineBreak 
                 , FsIsInsideList := BldIsList
                 , FsIsInsideCondFmt := BldCondFmtDepth > 0 
                 , FsIsInFirstLine := BldIsInFirstLine  
                 , FsIsRightOfEstListChild := BldIsRightOfEstListChild
                 , FsLdlNodeNo := LdlFsChildNode . NodeNo 
                 , FsFormatsEmpty := UbTrue  
                 ) 
        ; SetLeftAndRightFmtNo ( LResult ) 
        ; IF BldPrevLineBreakRef # NIL 
          THEN 
            BldPrevLineBreakRef . FsLineBreakToReachFmtNo := LResult . FsFmtNo 
          END (* IF *) 
        ; BldPrevLineBreakRef := LResult  
        ; BldIsInFirstLine := FALSE 
        ; BldLineBreakFollowsEstChild := TRUE  
        ; RETURN LResult 

        | Ldl1Tok . Ident 
(* CHECK: Does AS of Ldl1 allow this to happen? *) 
        => RETURN FsTreeForIdent 
             ( LdlFsChildNode 
             , (* IN OUT *) CurrentIndentCode := CurrentIndentCode 
             , FsKindForLineBreak := FsKindForLineBreak 
             ) 

        | Ldl1Tok . String 
(* CHECK: Does AS of Ldl1 allow this to happen? *) 
        => TYPECASE 
             Ldl1Semantics . FirstOcc 
               ( LangInfo 
               , LangInfo . SemMapRef ^ [ LdlFsChildNode . NodeNo ] . SemRef 
               ) 
           OF LdlSemantics . SemFirstOccStringTyp ( TSemDecl ) 
           => (* Literal string. *) 
              RETURN 
                FsTreeForString 
                  ( LdlFsChildNode 
                  , TSemDecl 
                  , (* IN OUT *) CurrentIndentCode := CurrentIndentCode 
                  ) 
           ELSE 
             CantHappen 
               ( AFT . A_Ldl1FsTrees_FsTreeForFsChild_SemStringOnNotStringFirstOccurrence ) 
           ; RETURN NIL 
           END (* TYPECASE *) 

        | Ldl1Tok . FsChildPlain 
        => RETURN FsTreeForFsChildPlain 
                    ( LdlFsChildNode 
                    , NewLineIndentCode 
                    , (* IN OUT *) CurrentIndentCode := CurrentIndentCode
                    , FsKindForLineBreak := FsKindForLineBreak
                    , DoBuildFsTree := TRUE  
                    ) 

        | Ldl1Tok . FsCondAbsent 
        , Ldl1Tok . FsCondPresent 
        , Ldl1Tok . FsCondEmpty 
        , Ldl1Tok . FsCondNonempty 
        , Ldl1Tok . FsCondPlural 
        , Ldl1Tok . FsCondNonplural 
        , Ldl1Tok . FsCondMember 
        , Ldl1Tok . FsCondElse  
        => RETURN 
             FsTreeForFsChildCondFmt 
               ( LdlFsChildNode 
               , (* IN OUT *) CurrentIndentCode := CurrentIndentCode 
               , FsKindForLineBreak := FsKindForLineBreak
               ) 

        | Ldl1Tok . FsChildCase 
        => RETURN  
             FsTreeForFsChildCase 
               ( LdlFsChildNode 
               , (* IN OUT *) CurrentIndentCode := CurrentIndentCode 
               , FsKindForLineBreak := FsKindForLineBreak
               ) 

        ELSE 
          CantHappen ( AFT . A_Ldl1FsTrees_FsTreeForFsChild_BadSyntax ) 
        ; RETURN NIL 
        END (* CASE *) 
      END FsTreeForFsChild 

  ; PROCEDURE BuildFmtNoMaps 
      ( FsRef : LangUtil . FsNodeRefTyp 
      ; NextAltFsRef : LangUtil . FsNodeRefTyp 
      ; VAR (* IN OUT *) MaxFmtNo : EstHs . FmtNoTyp 
      ) 
    RAISES { AssertionFailure } 
    (* Also fills in null FmtNos that are to the left of the Ast child
       in an FsList tree and fills in FsRightFmtNo and FsLeftFmtNo *) 

    = VAR LMaxFmtNo : EstHs . FmtNoTyp 
    ; VAR LLastChildNo : INTEGER 

    ; BEGIN (* BuildFmtNoMaps *) 
        IF FsRef # NIL 
        THEN 

        (* Recursive pass. 
           1) Fill in FmtNos that were not done previously.
           2) Fill in FsLeftFmtNo and FsRightFmtNo fields that depend on 1)
           3) Compute max fmt no. for nodes that have children.
        *)  

          CASE FsRef . FsKind 
          OF FsKindTyp . FsKindBegOfImage 
          , FsKindTyp . FsKindEndOfImage 
          , FsKindTyp . FsKindInsTok 
          , FsKindTyp . FsKindLineBreakOpt 
          , FsKindTyp . FsKindLineBreakReqd 
          =>  IF FsRef . FsFmtNo = EstHs . FmtNoNull 
              THEN 
                FsRef . FsFmtNo := AssignFmtNo ( ) 
              ; SetLeftAndRightFmtNo ( FsRef ) 
              END (* IF *) 
            ; MaxFmtNo := MAX ( MaxFmtNo , FsRef . FsFmtNo ) 
            ; RETURN 
          | FsKindTyp . FsKindEstChildOfList 
          =>  Assert 
                ( FsRef . FsFmtNo = EstHs . FmtNoListEstChild 
                , AFT . A_BuildFmtNoMaps_ListChildNotFmtNoZero
                ) 
            ; BuildFmtNoMaps ( NextAltFsRef , NIL , MaxFmtNo ) 
            ; RETURN 
          | FsKindTyp . FsKindEstChildOfFixed 
          => Assert 
                ( FsRef . FsFmtNo # EstHs . FmtNoNull  
                , AFT . A_BuildFmtNoMaps_FixedChildNoFmtNo  
                ) 
            ; MaxFmtNo := MAX ( MaxFmtNo , FsRef . FsFmtNo ) 
            ; BuildFmtNoMaps ( NextAltFsRef , NIL , MaxFmtNo ) 
            ; RETURN 
          | FsKindTyp . FsKindEstFixedVert 
          , FsKindTyp . FsKindEstFixedHoriz 
          , FsKindTyp . FsKindEstFixedFill 
          , FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill 
          , FsKindTyp . FsKindEstListVert 
          , FsKindTyp . FsKindEstListHoriz 
          , FsKindTyp . FsKindEstListFill 
          , FsKindTyp . FsKindEstListTrailVert 
          , FsKindTyp . FsKindEstListTrailHoriz 
          , FsKindTyp . FsKindEstListTrailFill 
          =>  LMaxFmtNo := 0 
            ; LLastChildNo := NUMBER ( FsRef . FsChildren ^ ) - 1 
            ; FOR RChildNo := 0 TO LLastChildNo 
              DO WITH WChild = FsRef . FsChildren ^ [ RChildNo ] 
                 DO IF WChild # NIL 
                    THEN 
                      BuildFmtNoMaps ( WChild , NextAltFsRef , LMaxFmtNo ) 
                    ; IF RChildNo = 0 
                      THEN 
                        FsRef . FsLeftFmtNo := WChild . FsLeftFmtNo 
                      END (* IF *) 
                    ; IF RChildNo = LLastChildNo 
                      THEN 
                        FsRef . FsRightFmtNo := WChild . FsRightFmtNo 
                      END (* IF *) 
                    END (* IF *) 
                 END (* WITH *) 
              END (* FOR *) 
          | FsKindTyp . FsKindCondFmt 
(* CHECK: Do we really need to do this in 3 separate sections?  They all
          look almost identical, and the code for the cases above should
          work here too.
*) 
          =>  LMaxFmtNo := 0 
            ; NextAltFsRef := FsRef . FsCondAltRef 
            ; LLastChildNo := NUMBER ( FsRef . FsChildren ^ ) - 1 
            ; FOR RChildNo := 0 TO FsRef . FsLeadingChildCt - 1  
              DO WITH WChild = FsRef . FsChildren ^ [ RChildNo ] 
                 DO IF WChild # NIL 
                    THEN 
                      BuildFmtNoMaps ( WChild , NextAltFsRef , LMaxFmtNo ) 
                    ; IF RChildNo = 0 
                      THEN 
                        FsRef . FsLeftFmtNo := WChild . FsLeftFmtNo 
                      END (* IF *) 
                    END (* IF *) 
                 END (* WITH *) 
              END (* FOR *) 
            ; WITH WEstContainingChild 
                   = FsRef . FsChildren ^ [ FsRef . FsLeadingChildCt ]
              DO 
                BuildFmtNoMaps 
                  ( WEstContainingChild , NextAltFsRef , LMaxFmtNo ) 
              ; IF FsRef . FsLeadingChildCt = 0 
                THEN 
                  FsRef . FsLeftFmtNo := WEstContainingChild . FsLeftFmtNo 
                END (* IF *) 
              ; IF FsRef . FsLeadingChildCt = LLastChildNo  
                THEN 
                  FsRef . FsRightFmtNo := WEstContainingChild . FsRightFmtNo 
                END (* IF *) 
              END (* WITH *) 
            ; FOR RChildNo := FsRef . FsLeadingChildCt + 1 TO LLastChildNo 
              DO WITH WChild = FsRef . FsChildren ^ [ RChildNo ] 
                 DO IF WChild # NIL 
                    THEN 
                      BuildFmtNoMaps ( WChild , NextAltFsRef , LMaxFmtNo ) 
                    ; IF RChildNo = LLastChildNo 
                      THEN 
                        FsRef . FsRightFmtNo := WChild . FsRightFmtNo 
                      END (* IF *) 
                    END (* IF *) 
                 END (* WITH *) 
              END (* FOR *) 
          ELSE (* of CASE *)  
            CantHappen ( AFT . A_BuildFmtNoMaps_BadFsKind ) 
          END (* CASE *) 

        (* Get here only for Fs nodes that have children. 
           LMaxFmtNo is the max within the subtree rooted here.
           Create the FmtNo map. 
        *) 

        ; FsRef . FsFmtNoMapRef 
            := NEW ( LangUtilRep . FsFmtNoMapRefTyp , LMaxFmtNo + 1 ) 
        ; FOR RFmtNo := 0 TO NUMBER ( FsRef . FsFmtNoMapRef ^ ) - 1 
          DO FsRef . FsFmtNoMapRef ^ [ RFmtNo ] := LangUtil . FsChildNoNull 
          END (* FOR *) 
        ; FOR RChildNo := 0 TO LLastChildNo 
          DO WITH WChild = FsRef . FsChildren ^ [ RChildNo ] 
             DO IF WChild # NIL 
                THEN 
                  CASE WChild . FsKind <* NOWARN *>
                  OF FsKindTyp . FsKindEstChildOfList 
                  , FsKindTyp . FsKindEstChildOfFixed 
                  =>  IF NextAltFsRef # NIL 
                         AND NextAltFsRef . FsFmtNoMapRef # NIL 
                      THEN
                        FOR RAltFmtNo := 0 
                            TO NUMBER ( NextAltFsRef . FsFmtNoMapRef ^ ) - 1
                        DO
                          IF NextAltFsRef . FsFmtNoMapRef ^ [ RAltFmtNo ] 
                             # LangUtil . FsChildNoNull 
                          THEN 
                            FsRef . FsFmtNoMapRef ^ [ RAltFmtNo ] 
                              := LangUtil . FsChildNoAlt 
                          END (* IF *) 
                        END (* FOR *) 
                      END (* IF *)  
                    ; FsRef . FsFmtNoMapRef ^ [ WChild . FsFmtNo ] := RChildNo 
                  | FsKindTyp . FsKindBegOfImage 
                  , FsKindTyp . FsKindEndOfImage 
                  , FsKindTyp . FsKindInsTok 
                  , FsKindTyp . FsKindLineBreakOpt
                  , FsKindTyp . FsKindLineBreakReqd 
                  =>  FsRef . FsFmtNoMapRef ^ [ WChild . FsFmtNo ] := RChildNo 
                  | FsKindTyp . FsKindEstFixedVert 
                  , FsKindTyp . FsKindEstFixedHoriz 
                  , FsKindTyp . FsKindEstFixedFill 
                  , FsKindTyp . FsKindSubtreeVert 
                  , FsKindTyp . FsKindSubtreeHoriz 
                  , FsKindTyp . FsKindSubtreeFill 
                  , FsKindTyp . FsKindEstListVert 
                  , FsKindTyp . FsKindEstListHoriz 
                  , FsKindTyp . FsKindEstListFill 
                  , FsKindTyp . FsKindEstListTrailVert 
                  , FsKindTyp . FsKindEstListTrailHoriz 
                  , FsKindTyp . FsKindEstListTrailFill 
                  , FsKindTyp . FsKindCondFmt 
                  => FOR RChildFmtNo := 0 
                         TO NUMBER ( WChild . FsFmtNoMapRef ^ ) - 1 
                     DO IF FsRef . FsIsInsideCondFmt 
                           AND WChild . FsFmtNoMapRef ^ [ RChildFmtNo ] 
                               = LangUtil . FsChildNoAlt 
                        THEN 
                          FsRef . FsFmtNoMapRef ^ [ RChildFmtNo ] 
                            := LangUtil . FsChildNoAlt
                        ELSIF WChild . FsFmtNoMapRef ^ [ RChildFmtNo ] 
                              # LangUtil . FsChildNoNull 
                        THEN 
                          FsRef . FsFmtNoMapRef ^ [ RChildFmtNo ] 
                            := RChildNo 
                        END (* IF *) 
                     END (* FOR *) 
                  END (* CASE *) 
                END (* IF *) 
             END (* WITH *) 
          END (* FOR *) 
        ; MaxFmtNo := MAX ( MaxFmtNo , LMaxFmtNo ) 
        END (* IF *) 
      END BuildFmtNoMaps 

  ; PROCEDURE Placeholder 
      ( LdlNode : AstView . AstRefTyp ; Tok : LbeStd . TokTyp ) 
    : LangUtil . FsNodeRefTyp 

    = BEGIN (* Placeholder *) 
        RETURN 
          NEW  ( LangUtil . FsNodeRefTyp 
               , FsFmtNo := EstHs . FmtNoAstString  
               , FsLeftFmtNo := EstHs . FmtNoAstString  
               , FsRightFmtNo := EstHs . FmtNoAstString  
               , FsIndentCode := LangUtil . IndentCodeInitial 
               , FsTok := Tok 
               , FsKind := FsKindTyp . FsKindAstString 
               , FsLdlNodeNo := LdlNode . NodeNo 
               , FsFormatsEmpty := UbFalse 
               ) 
      END Placeholder  

  ; PROCEDURE FixedRule 
      ( <* UNUSED *> FmtKind : LangUtil . FmtKindTyp 
      ; FsKind : FsKindTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ; IsStart : BOOLEAN 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LResult : LangUtil . FsNodeRefTyp := NIL  
    ; VAR LMaxFmtNo : EstHs . FmtNoTyp 
    ; VAR LAsGrandChildren : ARRAY [ 0 .. 1 ] OF AstView . AstRefTyp 

    ; BEGIN (* FixedRule *) 
        BldFsKindForEstChild := FsKindTyp . FsKindEstChildOfFixed 
      ; BldIsList := FALSE  
      ; BldEstDescendantCt := 0 
      ; BldLineBreakFollowsEstChild := FALSE 
      ; BldFsNodeForEstDescendant := NIL 
      ; BldLdlFsEstChild := AstView . AstRefNull 
      ; BldNextFmtNo := EstHs . FmtNoInitial 
      ; BldFsCondAltRef := NIL (* Defensive. *) 
      ; BldIsInFirstLine := TRUE 
      ; BldIsRightOfEstListChild := FALSE 
      ; BldPrevLineBreakRef := NIL
      ; CASE EstUtil . EstTok ( BldAsRuleNode . NodeRef ) 
        OF Ldl1Tok . AsFixedRule 
        => BldAsChildren 
             := AstView . Child 
                  ( BldAsRuleNode , Ldl1Child . AsFixedRule_Children 
                  , LangInfo . LdlLang ) 
        ; AstView . FirstChild 
            ( BldAsChildren , BldNextAsChild , BldNextAsChildExists ) 
        ; IF BldNextAsChildExists 
          THEN 
            AstView . GetChildren 
              ( BldNextAsChild , LAsGrandChildren , LangInfo . LdlLang ) 
          ; BldNextAsChildName 
              := LAsGrandChildren [ Ldl1Child . AsReqdChild_ChildName ] 
          ; BldNextAsChildClass 
              := LAsGrandChildren [ Ldl1Child . AsReqdChild_ChildClass ] 
          END (* IF *) 
        ; LResult 
            := FsTreeForFsSubtree 
                 ( AstView . Child 
                     ( BldFsRuleNode , Ldl1Child . FsFixedHorizRule_Children 
                     , LangInfo . LdlLang ) 
                  (* ^ This depends on FsFixed[Default|Horiz|Vert|Fill]Rule 
                       all having the same form. This is asserted in 
                       Ldl1Semantics.*) 
                 , FsKind 
                 , LangUtil . IndentCodeInitial 
                 , FsKindForLineBreak 
                 , IsStart := IsStart  
                 ) 
        ; LResult . FsTok := AsIdentSemRef . DeclTok 
        ; LMaxFmtNo := 0 (* Not actually needed. *) 
        ; BuildFmtNoMaps ( LResult , NIL , LMaxFmtNo ) 
        ; IF BldNextAsChildExists 
          THEN 
            SemError2 
              ( BldFsIdentNode . NodeNo 
              , BldNextAsChild . NodeNo 
              , AFT . E_FormatSyntaxRuleHasNoChildForAsChild 
              ) 
          END (* IF *) 
        ELSE 
          SemError2 
            ( BldFsIdentNode . NodeNo 
            , BldAsIdentNode . NodeNo 
            , AFT . E_FixedFormatSyntaxRuleRequiresFixedAbstractRule 
            ) 
        END (* CASE *) 
      ; IF LResult # NIL 
        THEN 
          LResult . FsPlaceholder 
            := Placeholder ( BldFsRuleNode , AsIdentSemRef . DeclTok ) 
        END (* IF *) 
      ; IF BldIsInFirstLine AND BldEstDescendantCt > 0   
        THEN
        (* SemError
            ( BldFsIdentNode . NodeNo 
            , AFT . W_Ldl1_FS_Fixed_rule_has_no_line_break
            ) 
        *) 
          SemErrorText 
            ( LangInfo . Gram . tokImage ( AsIdentSemRef . DeclTok ) 
(* TODO: This method (Heh, Heh) of calling a tokImage seems pretty kludgy. *)
            , AFT . W_Ldl1_FS_Fixed_rule_has_no_line_break_
            ) 
(* TODO: Also warn if some alternatives lack a line break, and there is none
       following the conditional construct.  The latter condition takes
       yet another semi-global boolean to detect. *) 
        END (* IF *) 
      ; RETURN LResult 
      END FixedRule 

  ; PROCEDURE ListRule 
      ( <* UNUSED *> FmtKind : LangUtil . FmtKindTyp 
      ; FsKind : FsKindTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ; IsStart : BOOLEAN 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = CONST IsFsStarRule = TRUE 

    ; VAR LLdlFsChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 
    ; VAR LAsChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 
    ; VAR LTempFsChildrenRef : LangUtil . FsChildrenArrayRefTyp 
    ; VAR LNextFsChildNo : LangUtil . FsChildNoTyp 
    ; VAR LAsRuleTok : LbeStd . TokTyp 
    ; VAR LListChildTok : LbeStd . TokTyp 
    ; VAR LLdlFsChild : AstView . AstRefTyp 
    ; VAR LLdlFsChildExists : BOOLEAN 
    ; VAR LResult : LangUtil . FsNodeRefTyp 
    ; VAR LMaxFmtNo : EstHs . FmtNoTyp 
    ; VAR LIndentCode : LangUtil . IndentCodeTyp 
    ; VAR LLeftFmtNo : EstHs . FmtNoTyp 
    ; VAR LRightFmtNo : EstHs . FmtNoTyp 
    ; VAR LThruFmtNo : EstHs . FmtNoTyp 
    ; VAR LSemDeclAsListNodeRef : LdlSemantics . SemDeclAsListNodeTyp 
    ; VAR LFsEstDescendantRef : LangUtil . FsNodeRefTyp := NIL 

    ; BEGIN (* ListRule *) 
        BldFsKindForEstChild := FsKindTyp . FsKindEstChildOfList 
      ; BldIsList := TRUE 
      ; BldFsNodeForEstDescendant := NIL 
      ; BldEstDescendantCt := 0 
      ; BldLineBreakFollowsEstChild := FALSE 
      ; BldFsCondAltRef := NIL (* Defensive. *) 
      ; BldLdlFsEstChild := AstView . AstRefNull 
      ; BldIsInFirstLine := TRUE   
      ; BldIsRightOfEstListChild := FALSE 
      ; BldPrevLineBreakRef := NIL 
      ; BldNextFmtNo := EstHs . FmtNoNull 
      ; AstView . GetChildren 
          ( BldFsRuleNode , LLdlFsChildren , LangInfo . LdlLang ) 
      ; IF IsStart 
        THEN  
          SemError 
            ( BldFsIdentNode . NodeNo 
            , AFT . E_FormatListRuleCannotBeARootNode  
            ) 
        END 
      ; WITH 
          WListChild 
          = LLdlFsChildren [ Ldl1Child . FsListHorizRule_ListChild ] 
        , WFormatters 
             = LLdlFsChildren [ Ldl1Child . FsListHorizRule_Formatters ] 
        (* ^ These depend on FsList[Default|Horiz|Vert|Fill]Rule 
             all having the same form. This is asserted in Ldl1Semantics.*) 
        DO LTempFsChildrenRef 
             := NEW 
                  ( LangUtil . FsChildrenArrayRefTyp 
                  , AstView . FastExtravagantChildCt ( WFormatters ) 
                    + 1 (* For WListChild *) 
                  ) 
        ; LNextFsChildNo := 0 
        ; LIndentCode := LangUtil . IndentCodeInitial 
        ; LAsRuleTok := EstUtil . EstTok ( BldAsRuleNode . NodeRef ) 
        ; CASE LAsRuleTok 
          OF Ldl1Tok . AsPlusRule , Ldl1Tok . AsStarRule 
          => AstView . GetChildren 
               ( BldAsRuleNode , LAsChildren , LangInfo . LdlLang ) 
          ; BldNextAsChildName 
              := LAsChildren [ Ldl1Child . AsStarRule_ChildName ] 
          ; BldNextAsChildClass 
              := LAsChildren [ Ldl1Child . AsStarRule_ChildClass ] 
          ; BldNextAsChildExists := TRUE 
          ; BldAsChildren := AstView . AstRefNull 

          (* Do the Est list child. *) 
          ; LListChildTok := EstUtil . EstTok ( WListChild . NodeRef ) 
          ; CASE LListChildTok 
            OF Ldl1Tok . FsChildPlain 
            , Ldl1Tok . FsHorizSubtree 
            , Ldl1Tok . FsDefaultSubtree 
            , Ldl1Tok . FsVertSubtree 
            , Ldl1Tok . FsFillSubtree 
            , Ldl1Tok . FsCondAbsent 
            , Ldl1Tok . FsCondPresent 
            , Ldl1Tok . FsCondEmpty 
            , Ldl1Tok . FsCondNonempty 
            , Ldl1Tok . FsCondPlural 
            , Ldl1Tok . FsCondMember 
            , Ldl1Tok . FsCondElse  
            , Ldl1Tok . FsChildCase 
            => WITH WListFsTree = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
               DO 
                 WListFsTree 
                   := FsTreeForFsChild 
                        ( WListChild 
                        , LangUtil . IndentCodeInitial 
                        , (* IN OUT *) CurrentIndentCode := LIndentCode 
                        , FsKindForLineBreak := FsKindForLineBreak 
                        ) 
               ; IF WListFsTree = NIL 
                 THEN 
                   LFsEstDescendantRef := NIL 
                 ELSE 
                   IF LListChildTok = Ldl1Tok . FsChildPlain 
                   THEN
                     LFsEstDescendantRef := WListFsTree 
                   ELSE
                     LFsEstDescendantRef := WListFsTree . FsEstDescendantRef 
                   (* If it's a CASE construct, it doesn't matter to the
                      Parser which alternative's Est descendant. *)  
                   END (* IF *)  
                 END (* IF *) 
               END (* WITH *) 
            ; INC ( LNextFsChildNo ) 

            | Ldl1Tok . Ident 
            => WITH WListFsTree = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
               DO WListFsTree 
                    := FsTreeForIdent 
                         ( WListChild 
                         , (* IN OUT *) CurrentIndentCode := LIndentCode 
                         , FsKindForLineBreak := FsKindForLineBreak 
                         ) 
               ; IF WListFsTree # NIL 
                 THEN 
                   IF WListFsTree . FsKind # BldFsKindForEstChild  
                   THEN
                     SemError 
                       ( WListChild . NodeNo 
                       , AFT . E_ListChildNotAbstractList 
                       ) 
                   ; WListFsTree := NIL 
                   END (* IF *) 
                 END (* IF *) 
               ; LFsEstDescendantRef := WListFsTree 
               END (* WITH *) 
            ; INC ( LNextFsChildNo ) 
            ELSE 
              SemError 
                ( WListChild . NodeNo , AFT . E_InvalidListChildKind ) 
            END (* CASE *) 
          ; IF LAsRuleTok = Ldl1Tok . AsStarRule AND NOT IsFsStarRule 
            THEN 
              SemError2 
                ( BldFsIdentNode . NodeNo 
                , BldAsIdentNode . NodeNo 
                , AFT . E_AbstractStarRuleRequiresFormatStarRule 
                ) 
            END (* IF *) 
          ELSE 
            SemError2 
              ( BldFsIdentNode . NodeNo 
              , BldAsIdentNode . NodeNo 
              , AFT . E_ListFormatSyntaxRuleForNotListAbstractRule 
              ) 
          END (* CASE *) 

        (* Go through the separators. *) 
        ; AstView . FirstChild 
            ( WFormatters , LLdlFsChild , LLdlFsChildExists ) 
        ; WHILE LLdlFsChildExists 
          DO WITH WFsSepChild = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
             DO WFsSepChild 
                  := FsTreeForFsChild 
                       ( LLdlFsChild 
                       , LangUtil . IndentCodeInitial 
                       , (* IN OUT *) CurrentIndentCode := LIndentCode 
                       , FsKindForLineBreak := FsKindForLineBreak 
                       ) 
             ; INC ( LNextFsChildNo , ORD ( WFsSepChild # NIL ) )  
             END (* WITH *) 
          ; AstView . NextChild 
              ( WFormatters , LLdlFsChild , LLdlFsChildExists ) 
          END (* WHILE *) 
        ; IF LNextFsChildNo = 0 
          THEN 
            LLeftFmtNo := EstHs . FmtNoNull 
          ; LThruFmtNo := EstHs . FmtNoNull 
          ; LRightFmtNo := EstHs . FmtNoNull 
          ELSE 
            TYPECASE LTempFsChildrenRef ^ [ 0 ] 
            OF NULL 
            => LLeftFmtNo := EstHs . FmtNoNull 
            ; LThruFmtNo := EstHs . FmtNoNull 
            | LangUtil . FsNodeRefTyp ( TFsNodeRef ) 
            => LLeftFmtNo := TFsNodeRef . FsLeftFmtNo 
            ; LThruFmtNo := TFsNodeRef . FsRightFmtNo 
            END (* TYPECASE *) 
          ; TYPECASE LTempFsChildrenRef ^ [ LNextFsChildNo - 1 ] 
            OF NULL 
            => LRightFmtNo := EstHs . FmtNoNull 
            | LangUtil . FsNodeRefTyp ( TFsNodeRef ) 
            => LRightFmtNo := TFsNodeRef . FsRightFmtNo 
            END (* TYPECASE *) 
          END (* IF *) 
        ; LSemDeclAsListNodeRef := AsIdentSemRef (* NARROW OK *) 
        ; LResult 
            := NEW 
                 ( LangUtil . FsNodeRefTyp 
                 , FsChildren 
                     := NEW 
                          ( LangUtil . FsChildrenArrayRefTyp 
                          , LNextFsChildNo 
                          ) 
                 , FsPlaceholder 
                     := Placeholder ( BldFsRuleNode , AsIdentSemRef . DeclTok )
                 , FsIndentCode := LangUtil . IndentCodeInitial 
                 , FsKind := FsKind 
                 , FsFmtNo := EstHs . FmtNoListEstChild 
                 , FsLeftFmtNo := LLeftFmtNo 
                 , FsRightFmtNo := LRightFmtNo 
                 , FsListSliceThruFmtNo := LThruFmtNo 
                 , FsTok := AsIdentSemRef . DeclTok 
                 , FsSublistTok := LSemDeclAsListNodeRef . SublistTok 
                 , FsPartialTok := LSemDeclAsListNodeRef . PartialTok 
                 , FsEmptyListTok 
                     := LSemDeclAsListNodeRef 
                        . ListCardToks [ ListCardEmpty ] 
                 , FsSingletonListTok 
                     := LSemDeclAsListNodeRef 
                        . ListCardToks [ ListCardSingleton ] 
                 , FsPluralListTok 
                     := LSemDeclAsListNodeRef 
                        . ListCardToks [ ListCardPlural ] 
                 , FsEstDescendantRef := LFsEstDescendantRef 
                 , FsIsInsideList := TRUE 
                 , FsIsInsideCondFmt := FALSE 
                 , FsLdlNodeNo := BldFsRuleNode . NodeNo 
                 , FsFormatsEmpty := UbUnknown 
                 ) 
        ; LResult . FsChildren ^ 
            := SUBARRAY ( LTempFsChildrenRef ^ , 0 , LNextFsChildNo ) 
        ; LdlSemantics . PatchListFields ( LResult , LResult , LIndentCode ) 
        ; LMaxFmtNo := 0 (* Not actually needed. *) 
        ; BuildFmtNoMaps ( LResult , NIL , LMaxFmtNo ) 
        ; IF BldPrevLineBreakRef # NIL 
          THEN 
            BldPrevLineBreakRef . FsLineBreakToReachFmtNo := LRightFmtNo 
          END (* IF *) 
        END (* WITH *) 
      ; IF BldIsInFirstLine 
        THEN
        (*SemError
            ( BldFsIdentNode . NodeNo 
            , AFT . W_Ldl1_FS_List_rule_has_no_line_break
            ) 
        *) 
          SemErrorText 
            ( LangInfo . Gram . tokImage ( AsIdentSemRef . DeclTok ) 
(* TODO: This method (Heh, Heh) of calling a tokImage seems pretty kludgy. *)
            , AFT . W_Ldl1_FS_List_rule_has_no_line_break_
            ) 
(* TODO: Also warn if some alternatives lack a line break, and there is none
       following the conditional construct.  The latter condition takes
       yet another semi-global boolean to detect. *) 
        END (* IF *) 
      ; RETURN LResult 
      END ListRule 

  ; BEGIN (* Build *) 
      BldCondFmtDepth := 0 
    ; BldPrincipalChildTokSet := IntSets . Empty ( )  
    ; BldNextOptionId := FIRST ( LRTable . OptionIdRealTyp )
    ; BldNextCondId := FIRST ( LRTable . OptionIdRealTyp )

    ; VAR LResult : LangUtil . FsNodeRefTyp 
    ; BEGIN (* Block for Build *) 
   (* ; IsStart := FALSE (* Trying new system of putting the EOI/BOI on the
                            augmenting production, rather than the original
                            start production. *) 
   *) 
        CASE EstUtil . EstTok ( BldFsRuleNode . NodeRef ) <* NOWARN *>
        OF Ldl1Tok . FsFixedHorizRule 
        , Ldl1Tok . FsFixedDefaultRule 
        => LResult :=  
             FixedRule 
               ( LangUtil . FmtKindTyp . FmtKindHoriz 
               , FsKindTyp . FsKindEstFixedHoriz 
               , FsKindTyp . FsKindLineBreakOpt 
               , IsStart := IsStart 
               ) 
        | Ldl1Tok . FsFixedVertRule 
        => LResult :=  
             FixedRule 
               ( LangUtil . FmtKindTyp . FmtKindVert 
               , FsKindTyp . FsKindEstFixedVert 
               , FsKindTyp . FsKindLineBreakReqd 
               , IsStart := IsStart 
               ) 
        | Ldl1Tok . FsFixedFillRule 
        => LResult := 
             FixedRule 
               ( LangUtil . FmtKindTyp . FmtKindFill 
               , FsKindTyp . FsKindEstFixedFill 
               , FsKindTyp . FsKindLineBreakOpt 
               , IsStart := IsStart 
               ) 
        | Ldl1Tok . FsListHorizRule 
        , Ldl1Tok . FsListDefaultRule 
        => LResult :=  
             ListRule 
               ( LangUtil . FmtKindTyp . FmtKindHoriz 
               , FsKindTyp . FsKindEstListHoriz 
               , FsKindTyp . FsKindLineBreakOpt 
               , IsStart := IsStart 
               ) 
        | Ldl1Tok . FsListVertRule 
        => LResult :=  
             ListRule 
               ( LangUtil . FmtKindTyp . FmtKindVert 
               , FsKindTyp . FsKindEstListVert 
               , FsKindTyp . FsKindLineBreakReqd 
               , IsStart := IsStart 
               ) 
        | Ldl1Tok . FsListFillRule 
        => LResult :=  
             ListRule 
               ( LangUtil . FmtKindTyp . FmtKindFill 
               , FsKindTyp . FsKindEstListFill 
               , FsKindTyp . FsKindLineBreakOpt 
               , IsStart := IsStart 
               ) 
        | Ldl1Tok . FsListTrailHorizRule 
        , Ldl1Tok . FsListTrailDefaultRule 
        => LResult :=  
             ListRule 
               ( LangUtil . FmtKindTyp . FmtKindHoriz 
               , FsKindTyp . FsKindEstListTrailHoriz 
               , FsKindTyp . FsKindLineBreakOpt 
               , IsStart := IsStart 
               ) 
        | Ldl1Tok . FsListTrailVertRule 
        => LResult :=  
             ListRule 
               ( LangUtil . FmtKindTyp . FmtKindVert 
               , FsKindTyp . FsKindEstListTrailVert 
               , FsKindTyp . FsKindLineBreakReqd 
               , IsStart := IsStart 
               ) 
        | Ldl1Tok . FsListTrailFillRule 
        => LResult :=  
             ListRule 
               ( LangUtil . FmtKindTyp . FmtKindFill 
               , FsKindTyp . FsKindEstListTrailFill 
               , FsKindTyp . FsKindLineBreakOpt 
               , IsStart := IsStart 
               ) 
        | Ldl1Tok . FsInlineRule 
        => RETURN NIL 
        END (* CASE *) 
      ; TRY 
          LdlSemantics . PostNumberFsTree ( LResult ) 
        EXCEPT 
        LdlSemantics . NodeNoOverflow (* ( EMaxValue ) *) 
         => SemError 
              ( BldFsIdentNode . NodeNo 
              , AFT . E_FormatSyntaxRuleTooComplex 
              ) 
        END 
      ; RETURN LResult 
      END (* Block for Build. *) 
    END Build 

; PROCEDURE Check ( ) 
  (* Check some things that we depend on being statically true, but
     can't conveniently encode in the language type system.
  *) 

  = <* FATAL AssertionFailure *> 
    BEGIN 
      Assert 
        ( Ldl1Child . AsReqdChild_ChildName = Ldl1Child . AsOptChild_ChildName 
          AND Ldl1Child . AsReqdChild_ChildClass 
              = Ldl1Child . AsOptChild_ChildClass 
        , AFT . A_Ldl1FsTrees_DifferentChildNameOrClass 
        ) 
      (* ^FixedRule depends on this equality. *) 
    ; Assert 
        ( Ldl1Child . AsStarRule_ChildName = Ldl1Child . AsPlusRule_ChildName 
          AND Ldl1Child . AsStarRule_ChildClass 
              = Ldl1Child . AsPlusRule_ChildClass 
        , AFT . A_Ldl1FsTrees_DifferentChildNameOrClass 
        ) 
      (* ^ListRule depends on this equality. *) 
    ; Assert 
        ( Ldl1Child . FsHorizSubtree_Children 
          = Ldl1Child . FsDefaultSubtree_Children 
          AND Ldl1Child . FsHorizSubtree_Children 
              = Ldl1Child . FsVertSubtree_Children 
          AND Ldl1Child . FsHorizSubtree_Children 
              = Ldl1Child . FsFillSubtree_Children
          (* The last two equalities are not required here, but are required
             in GrammarGen. *)  
        , AFT . A_Ldl1FsTrees_DifferentChildrenOfSubtree  
        ) 
      (* ^ListRule depends on this equality. *) 
    ; Assert
        ( Ldl1Child . BolIndenter_IndentCode 
          = Ldl1Child . ParentIndenter_IndentCode
          AND Ldl1Child . BolIndenter_IndentCode 
              = Ldl1Child . PosIndenter_IndentCode
          AND Ldl1Child . BolIndenter_IndentCode 
              = Ldl1Child . BolPlusIndenter_IndentCode
          AND Ldl1Child . BolIndenter_IndentCode 
              = Ldl1Child . ParentPlusIndenter_IndentCode
          AND Ldl1Child . BolIndenter_IndentCode 
              = Ldl1Child . PosPlusIndenter_IndentCode
          AND Ldl1Child . BolIndenter_IndentCode 
              = Ldl1Child . BolMinusIndenter_IndentCode
          AND Ldl1Child . BolIndenter_IndentCode 
              = Ldl1Child . ParentMinusIndenter_IndentCode
          AND Ldl1Child . BolIndenter_IndentCode 
              = Ldl1Child . PosMinusIndenter_IndentCode
        , AFT . A_Ldl1FsTrees_DifferentIndentCode  
        ) 
      (* ^Indenter depends on this equality. *) 
    END Check 

; BEGIN (* Ldl1FsTrees *) 
    Check ( ) 
  END Ldl1FsTrees 
. 
