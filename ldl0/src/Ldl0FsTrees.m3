
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Ldl0FsTrees 
(* Package Ldl0FsTrees. 
   Build FsTrees for a language definition written in Ldl0.
*) 

; FROM Assertions IMPORT Assert , AssertionFailure , CantHappen 
; FROM Failures IMPORT Backout  
; IMPORT AstView 
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LangUtil 
; FROM LangUtil 
   IMPORT ChildOptTyp , FsKindTyp , FormatsEmptyTyp , PredicateKindTyp 
; IMPORT LangUtilLo 
; IMPORT LangUtilRep 
; IMPORT LbeStd 
; FROM LbeStd IMPORT TokClassTyp 
; IMPORT Ldl0Child 
; IMPORT Ldl0Semantics 
; IMPORT Ldl0Tok 
; IMPORT LdlSemantics 
; IMPORT LRTable 
; IMPORT MessageCodes 
; FROM Messages IMPORT SemError , SemError2 
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
             , FsHasLineBreak := FormatsEmptyTyp . FeNo  
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
             , FsHasLineBreak := FormatsEmptyTyp . FeNo  
             ) 
    ; TRY 
        LdlSemantics . PostNumberFsTree ( LFsRoot ) 
      EXCEPT 
      LdlSemantics . NodeNoOverflow 
        => CantHappen 
             ( AFT . A_Ldl1FsTrees_BuildFixedTerm_NodeNoOverflow ) 
      END 
    ; LangInfo . FsTreeMapRef ^ [ Tok - LbeStd . Tok__FirstLangDep ] 
        := LFsRoot  
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
             , FsHasLineBreak := FormatsEmptyTyp . FeNo  
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
             , FsHasLineBreak := FormatsEmptyTyp . FeNo  
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
             , FsHasLineBreak := FormatsEmptyTyp . FeNo  
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

  = VAR BldFsIdentNode 
      := AstView . AstRef ( LangInfo . Root , FsIdentSemRef . NodeNo ) 
  ; VAR BldFsRuleNode 
      := AstView . AstRef ( LangInfo . Root , FsIdentSemRef . RefRuleNodeNo ) 
  ; VAR BldAsIdentNode 
      := AstView . AstRef ( LangInfo . Root , AsIdentSemRef . NodeNo ) 
  ; VAR BldAsRuleNode 
      := AstView . AstRef ( LangInfo . Root , AsIdentSemRef . DeclRuleNodeNo ) 
(* CHECK: Can we avoid calling AstView . AstRef? *) 
  ; VAR NextFmtNo : EstHs . FmtNoTyp 

  (* NextAsChildExists, NextAsChild, NextAsChildName, NextAsChildClass, 
     and AsChildren are used to step through abstract children, in step 
     with the format syntax nodes that correspond to them.  NextAsChildExists 
     tells whether there is another abstract child.  If so, NextAsChildClass 
     is the tree node of the abstract child type, and NextAsChildName is the 
     node for its child name, if any.  For a list As/Fs rule, AsChildren is 
     Null, and NextAsChild is meaningless.  For a fixed As/Fs rule, AsChildren 
     is the tree node for the list of abstract children and NextAsChild is the 
     tree node for the next child. 
  *) 
  ; VAR NextAsChildExists : BOOLEAN 
  ; VAR NextAsChildName : AstView . AstRefTyp 
  ; VAR NextAsChildClass : AstView . AstRefTyp 
  ; VAR AsChildren : AstView . AstRefTyp 
  ; VAR NextAsChild : AstView . AstRefTyp 

  ; VAR BldFsKindForEstChild : FsKindTyp 
  ; VAR BldFsKindForLineBreak : FsKindTyp 
  ; VAR FsEstChildCt : LbeStd . EstChildNoTyp := LbeStd . EstChildNoNull 
  ; VAR FsEstChild : LangUtil . FsNodeRefTyp := NIL 
  ; VAR BldIsList : BOOLEAN 
  ; VAR BldIsInFirstLine : BOOLEAN 
  ; VAR BldIsRightOfEstListChild : BOOLEAN 
  ; VAR BldPrevLineBreakRef : LangUtil . FsNodeRefTyp 
  ; VAR BldNextOptionId : LRTable . OptionIdTyp 
  ; VAR BldNextCondId : LRTable . OptionIdTyp 

  ; VAR CondFmtDepth := 0 

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

  ; PROCEDURE ConsumeAsChild ( ) 
    RAISES { AssertionFailure } 

    = VAR LAsGrandChildren : ARRAY [ 0 .. 1 ] OF AstView . AstRefTyp 

    ; BEGIN (* ConsumeAsChild *) 
        IF AsChildren = AstView . AstRefNull 
        THEN 
          NextAsChildExists := FALSE 
        ELSIF NextAsChildExists 
        THEN 
          AstView . NextChild ( AsChildren , NextAsChild , NextAsChildExists ) 
        ; IF NextAsChildExists 
          THEN 
            AstView . GetChildren 
              ( NextAsChild , LAsGrandChildren , LangInfo . LdlLang ) 
          ; NextAsChildName 
              := LAsGrandChildren [ Ldl0Child . AsReqdChild_ChildName ] 
          ; NextAsChildClass 
              := LAsGrandChildren [ Ldl0Child . AsReqdChild_ChildClass ] 
          END (* IF *) 
        END (* IF *) 
      END ConsumeAsChild 

  ; PROCEDURE FmtNoOfNextAsChild ( ) : EstHs . FmtNoTyp 
    RAISES { AssertionFailure } 

    = VAR LLeafElem : EstHs . LeafElemTyp 

    ; BEGIN (* FmtNoOfNextAsChild *) 
        IF AsChildren = AstView . AstRefNull 
        THEN (* This is the Est child of a list. *) 
          RETURN EstHs . FmtNoListEstChild 
        ELSE 
(* FIX: This doesn't make any sense.  We are interested in FmtNos in the
        FsTrees being generated for the described language, not in the
        Est for the LDL description. *) 
          EstUtil . GetLeafElem 
            ( RootRef := LangInfo . Root . NodeRef 
            , NodeNo := NextAsChild . NodeNo 
            , ResultLeafElem := LLeafElem 
            ) 
        ; RETURN LLeafElem . LeFmtNo 
        END (* IF *) 
      END FmtNoOfNextAsChild 

  ; PROCEDURE AssignFmtNo ( ) : EstHs . FmtNoTyp 

    = VAR LResult : EstHs . FmtNoTyp 

    ; BEGIN (* AssignFmtNo *) 
        LResult := NextFmtNo 
      ; IF LResult # EstHs . FmtNoNull THEN INC ( NextFmtNo ) END (* IF *) 
      ; RETURN LResult 
      END AssignFmtNo 

  ; PROCEDURE SetLeftAndRightFmtNo ( FsRef : LangUtil . FsNodeRefTyp ) 

    = BEGIN (* SetLeftAndRightFmtNo *) 
        FsRef . FsLeftFmtNo := FsRef . FsFmtNo 
      ; FsRef . FsRightFmtNo := FsRef . FsFmtNo 
      END SetLeftAndRightFmtNo 

  ; PROCEDURE CheckFmtNoMatch 
      ( FsChild : AstView . AstRefTyp 
      ; FsEstChild : LangUtil . FsNodeRefTyp 
      ) 
    RAISES { AssertionFailure } 

    = VAR LFmtNoOfNextAsChild := FmtNoOfNextAsChild ( ) 
    ; VAR LNextAsChildClass := NextAsChildClass 

    ; BEGIN (* CheckFmtNoMatch *) 
(* FIX: This check is completely wrong.  We need to search for all 
        instances of Ests of the kind that this As/Fs name identifies. *) 
        IF FALSE AND Ldl0Semantics . Bootstrapping 
        THEN 
          IF LFmtNoOfNextAsChild # FsEstChild . FsFmtNo 
          THEN 
            SemError2 
              ( FsChild . NodeNo 
              , LNextAsChildClass . NodeNo 
              , AFT . E_AbstractFmtNoMismatchesComputed 
              ) 
          END (* IF *) 
        END (* IF *) 
      END CheckFmtNoMatch 

  ; PROCEDURE FsChildPlain 
      ( AsChildName : AstView . AstRefTyp 
      ; AsChildClass : AstView . AstRefTyp 
      ; FsChild : AstView . AstRefTyp 
      ; IndentCode : LangUtil . IndentCodeTyp 
      ) 
      : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LFsChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 
    ; VAR LAsFirstOcc : LdlSemantics . SemDeclTyp 
    ; VAR LChildIndentCode : LangUtil . IndentCodeTyp 
    ; VAR LAsDeclNodeNo : INTEGER 
    ; VAR LChildNo : LbeStd . EstChildNoTyp 
    ; VAR LHasErrors : BOOLEAN 
    ; VAR LWasFound : BOOLEAN 

    ; BEGIN (* FsChildPlain *) 
        LHasErrors := FALSE 
      ; AstView . GetChildren ( FsChild , LFsChildren , LangInfo . LdlLang ) 
      ; WITH WFsChildName = LFsChildren [ Ldl0Child . FsChildPlain_ChildName ] 
        DO IF WFsChildName . NodeRef # NIL 
           THEN 
             IF AsChildName . NodeRef # NIL 
                AND AsChildName . NodeRef # WFsChildName . NodeRef 
             THEN 
               SemError2 
                 ( WFsChildName . NodeNo 
                 , AsChildName . NodeNo 
                 , AFT . E_FormatSyntaxChildNameMismatchesAbstractChildName 
                 ) 
             ; LHasErrors := TRUE 
             END (* IF *) 
           END (* IF *) 
        END (* WITH *) 
      ; WITH 
          WFsChildClass = LFsChildren [ Ldl0Child . FsChildPlain_ChildClass ] 
        DO CASE EstUtil . EstTok ( WFsChildClass . NodeRef ) 
           OF LbeStd . Tok__Null 
           => RETURN NIL 
           | Ldl0Tok . DontCare 
           =>  LAsFirstOcc 
                 := LangInfo . SemMapRef ^ [ AsChildClass . NodeNo ] . SemRef 
           | Ldl0Tok . Ident 
           => TextIntSymbolTable . Find 
                ( LangInfo . SymbolTable 
                , SharedStrings . ToText ( WFsChildClass . NodeRef ) 
                , LWasFound 
                , LAsDeclNodeNo 
                ) 
           ; IF LWasFound 
             THEN 
               LAsFirstOcc 
                 := LangInfo . SemMapRef ^ [ LAsDeclNodeNo ] . SemRef 
             ; LangInfo . SemMapRef ^ [ WFsChildClass . NodeNo ] . SemRef 
                 := NEW 
                      ( LdlSemantics . SemRefTyp 
                      , NodeNo := WFsChildClass . NodeNo 
                      , RefDeclId := LAsDeclNodeNo 
                      , RefTok := LAsFirstOcc . DeclTok 
                      ) 
             ; Ldl0Semantics . CheckContainment 
                 ( LangInfo 
                 , WFsChildClass . NodeNo 
                 , AsChildClass . NodeNo 
                 , LHasErrors 
                 ) 
             ELSE 
               SemError 
                 ( WFsChildClass . NodeNo , AFT . E_ChildClassIsUndeclared ) 
             ; LHasErrors := TRUE 
             END (* IF *) 
           ELSE 
             CantHappen ( AFT . A_FsChildPlainBadToken ) 
           END (* CASE *) 
        END (* WITH *) 
      ; IF LHasErrors OR LAsFirstOcc = NIL 
        THEN 
          RETURN NIL 
        ELSE 
          LChildIndentCode := IndentCode 
        ; IF NextFmtNo = EstHs . FmtNoNull 
          THEN 
            NextFmtNo := EstHs . FmtNoListEstChild 
          END (* IF *) 
        ; TYPECASE LangInfo . SemMapRef ^ [ AsChildClass . NodeNo ] . SemRef 
          OF NULL 
          => LChildNo := LbeStd . EstChildNoNull 
          | LdlSemantics . SemChildClassRefTyp ( TChildRef ) 
          => LChildNo := TChildRef . ChildNo 
          ELSE 
            LChildNo := LbeStd . EstChildNoNull 
          END (* TYPECASE *) 
        ; FsEstChild 
            := NEW 
                 ( LangUtil . FsNodeRefTyp 
                 , FsFmtNo := AssignFmtNo ( ) 
                 , FsIndentCode := IndentCode 
                 , FsTok := LAsFirstOcc . DeclTok 
                 , FsKind := BldFsKindForEstChild 
                 , FsChildIndentCode := LChildIndentCode 
                 , FsEstChildOpt 
                     := LangUtil . ChildOpt 
                          ( NOT BldIsList (* Child of list is never optional. *)
                            AND EstUtil . EstTok ( NextAsChild . NodeRef ) 
                                = Ldl0Tok . AsOptChild 
                                (* ^It's an optional child of fixed. *) 
                          ) 
                 , FsEstChildNo := LChildNo 
                 , FsIsInsideList := BldIsList 
                 , FsIsInsideCondFmt := CondFmtDepth > 0 
                 , FsIsInFirstLine := BldIsInFirstLine  
                 , FsLdlNodeNo := FsChild . NodeNo 
                 , FsFormatsEmpty := UbUnknown 
                 ) 
        ; FsEstChild . FsEstChildIsOptional 
            := FsEstChild . FsEstChildOpt # ChildOptTyp . OptRequired 
(* FIXME: Check-for/set OptAbsent or OptPresent. *) 
        ; IF FsEstChild . FsEstChildIsOptional  
          THEN (* We need an option id for hand-written CS. *) 
            FsEstChild . FsOptionIds [ FALSE ] := BldNextOptionId 
          ELSE
            FsEstChild . FsOptionIds [ FALSE ] := LRTable . OptionIdNull   
          END (* IF *) 
        ; IF CondFmtDepth > 0 OR FsEstChild . FsEstChildIsOptional 
          THEN (* We need an option id for generated CS. *)
            FsEstChild . FsOptionIds [ TRUE ] := BldNextCondId  
          ; IncOptionId ( BldNextCondId , "generated" ) 
          ELSE 
            FsEstChild . FsOptionIds [ TRUE ] := LRTable . OptionIdNull   
          END (* IF *) 
        ; IF BldIsList 
          THEN
            FsEstChild . FsFirstListElemIndentCode 
              := LangUtil . IndentCodeInitial 
(* FIX: ^This is not right. It needs to allow for indenters in a CF node. *) 
          END (* IF *) 
        ; SetLeftAndRightFmtNo ( FsEstChild ) 
        ; INC ( FsEstChildCt ) 
        ; CheckFmtNoMatch ( FsChild , FsEstChild ) 
        ; BldIsRightOfEstListChild 
            := BldIsRightOfEstListChild OR BldIsList 
        ; RETURN FsEstChild 
        END (* IF *) 
      END FsChildPlain 

  ; PROCEDURE FsChildCondFmt 
      ( FsChildNode : AstView . AstRefTyp 
      ; PredicateKind : PredicateKindTyp 
      ; FsChildrenNode : AstView . AstRefTyp 
      ; ClassNode : AstView . AstRefTyp 
         (* ^Meaningful only if PredicateKind = PredicateKindInClass *) 
      ; FsKindForLineBreak : FsKindTyp 
      ; IndentCode : LangUtil . IndentCodeTyp 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LFsChild : AstView . AstRefTyp 
    ; VAR LFsChildExists : BOOLEAN 
    ; VAR LTempFsChildrenRef : LangUtil . FsChildrenArrayRefTyp 
    ; VAR LNextFsChildNo : LangUtil . FsChildNoTyp 
    ; VAR LIndentCode : LangUtil . IndentCodeTyp 
    ; VAR LFsCondAltCopyRef : LangUtil . FsNodeRefTyp 
    ; VAR LResult : LangUtil . FsNodeRefTyp 
    ; VAR LLeadingChildCt : LbeStd . EstChildNoTyp := 0 
    ; VAR LFsTreeAstChild : LangUtil . FsNodeRefTyp := NIL 
    ; VAR LAstChild : AstView . AstRefTyp := AstView . AstRefNull 
    ; VAR LWasFound : BOOLEAN 
    ; VAR LHasErrors := FALSE 
    ; VAR LDeclNodeNo : INTEGER 
    ; VAR LPredicate : LangUtil . PredicateTyp 
    ; VAR LAltIsAlwaysEmpty : BOOLEAN := FALSE  

    ; BEGIN (* FsChildCondFmt *) 
        INC ( CondFmtDepth ) 
      ; IF CondFmtDepth > 1 
        THEN 
          SemError 
            ( FsChildNode . NodeNo , AFT . E_NestedConditionalFormatConstruct ) 
        END (* IF *) 
      ; LPredicate . PredicateKind := PredicateKind 
      ; LPredicate . PredicateClass := LbeStd . Tok__Null 
      ; IF NOT NextAsChildExists 
        THEN 
          SemError2 
            ( FsChildNode . NodeNo 
            , BldAsIdentNode . NodeNo 
            , AFT . E_AsNodeHasNoChildForThisConditionalFormatSubtree  
            ) 
        ; DEC ( CondFmtDepth ) 
        ; RETURN NIL 
        ELSE 
          CASE PredicateKind 

          OF PredicateKindTyp . PredicateKindPresent 
          => LAltIsAlwaysEmpty := TRUE 

          | PredicateKindTyp . PredicateKindNonemptyList 
          => LAltIsAlwaysEmpty := TRUE 

          | PredicateKindTyp . PredicateKindPluralList 
          => LAltIsAlwaysEmpty := FALSE 

          | PredicateKindTyp . PredicateKindInClass 
          => LAltIsAlwaysEmpty := FALSE 
          ;  TextIntSymbolTable . Find 
               ( LangInfo . SymbolTable 
               , SharedStrings . ToText ( ClassNode . NodeRef ) 
               , LWasFound 
               , LDeclNodeNo 
               ) 
          ; IF LWasFound 
            THEN 
              TYPECASE LangInfo . SemMapRef ^ [ LDeclNodeNo ] . SemRef 
              OF LdlSemantics . SemDeclAsNodeTyp ( TDecl ) 
              => LangInfo . SemMapRef ^ [ ClassNode . NodeNo ] . SemRef 
                   := NEW 
                        ( LdlSemantics . SemRefTyp 
                        , NodeNo := ClassNode . NodeNo 
                        , RefDeclId := TDecl . NodeNo 
                        , RefTok := TDecl . DeclTok 
                        ) 
              ; LPredicate . PredicateClass := TDecl . DeclTok 
              | LdlSemantics . SemFirstOccClassTyp ( TDecl ) 
              => LangInfo . SemMapRef ^ [ ClassNode . NodeNo ] . SemRef 
                   := NEW 
                        ( LdlSemantics . SemRefTyp 
                        , NodeNo := ClassNode . NodeNo 
                        , RefDeclId := TDecl . NodeNo 
                        , RefTok := TDecl . DeclTok 
                        ) 
              ; LPredicate . PredicateClass := TDecl . DeclTok 
              | LdlSemantics . SemFirstOccStringTyp ( TDecl ) 
              => LangInfo . SemMapRef ^ [ ClassNode . NodeNo ] . SemRef 
                   := NEW 
                        ( LdlSemantics . SemRefTyp 
                        , NodeNo := ClassNode . NodeNo 
                        , RefDeclId := TDecl . NodeNo 
                        , RefTok := TDecl . DeclTok 
                        ) 
              ; LPredicate . PredicateClass := TDecl . DeclTok 
              ELSE 
                SemError2 
                  ( ClassNode . NodeNo 
                  , LDeclNodeNo 
                  , AFT . E_MemberMustBeOfAClassAbstractNodeOrString 
                  ) 
              ; LHasErrors := TRUE 
              END (* TYPECASE *) 
            ELSE 
              SemError ( ClassNode . NodeNo , AFT . E_NotDeclared ) 
            ; LHasErrors := TRUE 
            END (* IF *) 

          ELSE (* Other predicates can't happen in Ldl0. *) 
          END (* CASE *) 
        ; AstView . FirstChild ( FsChildrenNode , LFsChild , LFsChildExists ) 
        ; LTempFsChildrenRef 
            := NEW 
                 ( LangUtil . FsChildrenArrayRefTyp 
                 , AstView . FastExtravagantChildCt ( FsChildrenNode ) 
                 ) 
        ; LNextFsChildNo := 0 
        ; LIndentCode := IndentCode 
        ; WHILE LFsChildExists 
          DO WITH WFsTreeChild = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
             DO WFsTreeChild 
                  := FsChild 
                       ( LFsChild 
                       , FsKindForLineBreak 
                       , IndentCode 
                       , LIndentCode 
                       ) 
             ; IF WFsTreeChild # NIL 
               THEN 
                 IF NOT WFsTreeChild . FsKind 
                        IN LangUtil . FsKindSetFormatting 
                 THEN (* It's the Est child, or subtree containing it. *) 
                   IF LFsTreeAstChild = NIL 
                   THEN  (* It's the first Est-containing child. *)
                     LLeadingChildCt := LNextFsChildNo 
                   ; LFsTreeAstChild := WFsTreeChild 
                   ; LAstChild := LFsChild 
                   ELSE (* We previously did an Est-containing child. *)
                     SemError2 
                       ( LFsChild . NodeNo 
                       , LAstChild . NodeNo 
                       , AFT . E_OnlyOneNonFormatterChildOfConditionalFormatNodeIsAllowed 
                       ) 
                   ; LHasErrors := TRUE 
                   END (* IF *) 
                 END (* IF *) 
               END (* IF *) 
             END (* WITH *) 
          ; INC ( LNextFsChildNo ) 
          ; AstView . NextChild ( FsChildrenNode , LFsChild , LFsChildExists ) 
          END (* WHILE  *) 
        ; IF LFsTreeAstChild = NIL 
          THEN 
            SemError 
              ( FsChildrenNode . NodeNo 
              , AFT . E_ConditionalFormatSubtreeHasNoTreeChild 
              ) 
          ; LHasErrors := TRUE 
          END (* IF *) 
        ; IF LHasErrors 
          THEN 
            DEC ( CondFmtDepth ) 
          ; RETURN NIL 
          ELSE (* In some cases, the negation of the predicate can imply
                  the Est child (along with all the other children) must 
                  be absent from the alternative described by FsCondAltRef.
                  LAltIsAlwaysEmpty signals this.  However, for now, we 
                  make an FsNode for it anyway.  
               *) 
            LFsCondAltCopyRef := LangUtil . CopyOfFsNode ( FsEstChild ) 
          ; LFsCondAltCopyRef . FsIndentCode := IndentCode 
          ; LFsCondAltCopyRef . FsChildIndentCode := IndentCode 
          ; LFsCondAltCopyRef . FsCondAltRef := NIL 
          ; IF LFsCondAltCopyRef . FsEstChildIsOptional
            THEN 
              FsEstChild . FsOptionIds [ TRUE ] := BldNextCondId  
              (* Put the new OptionId in the old FsNode and let the 
                 copied old OptionId stay in the new FsNode.  This 
                 gets the order right, w/ or w/o reversal. 
              *) 
            ; IncOptionId ( BldNextCondId , "generated" ) 
            ELSE 
              LFsCondAltCopyRef . FsOptionIds [ TRUE ] 
                := LRTable . OptionIdNull 
            END (* IF *) 
          ; LResult 
              := NEW 
                   ( LangUtil . FsNodeRefTyp 
                   , FsFmtNo := FsEstChild . FsFmtNo 
                   , FsChildren 
                       := NEW 
                            ( LangUtil . FsChildrenArrayRefTyp 
                            , LNextFsChildNo 
                            ) 
                   , FsTok := FsEstChild . FsTok 
                   , FsKind := FsKindTyp . FsKindCondFmt 
                   , FsCondPredicate := LPredicate 
                   , FsLeadingChildCt := LLeadingChildCt 
                   , FsLeftFmtNo := LTempFsChildrenRef ^ [ 0 ] . FsLeftFmtNo 
                   , FsRightFmtNo 
                       := LTempFsChildrenRef ^ [ LNextFsChildNo - 1 ] 
                          . FsRightFmtNo
                   , FsEstDescendantRef := FsEstChild  
                   , FsCondAltRef := LFsCondAltCopyRef 
                   , FsEstChildOpt := FsEstChild . FsEstChildOpt
                   , FsEstChildIsOptional := FsEstChild . FsEstChildIsOptional
                   , FsIsInsideList := BldIsList 
                   , FsIsInsideCondFmt := CondFmtDepth > 0 
                   , FsIsInFirstLine := BldIsInFirstLine  
                   , FsLdlNodeNo := FsChildNode . NodeNo 
                   , FsFormatsEmpty := UbUnknown 
                   ) 
          ; IF FsEstChild # NIL 
            THEN
             LResult . FsOptionIds := FsEstChild . FsOptionIds  
            END (* IF *)  
          ; LResult . FsChildren ^ 
              := SUBARRAY ( LTempFsChildrenRef ^ , 0 , LNextFsChildNo ) 
          ; DEC ( CondFmtDepth ) 
          ; RETURN LResult 
          END (* IF *) 
        END (* IF *) 
      END FsChildCondFmt 

  ; PROCEDURE FsString 
      ( LdlNode : AstView . AstRefTyp 
      ; SemDecl : LdlSemantics . SemFirstOccStringTyp 
      ; VAR (* IN OUT *) CurrentIndentCode : LangUtil . IndentCodeTyp 
      ) 
    : LangUtil . FsNodeRefTyp 

    = VAR LResult : LangUtil . FsNodeRefTyp 

    ; BEGIN (* FsString *) 
        LResult 
          := NEW 
               ( LangUtil . FsNodeRefTyp 
               , FsFmtNo := AssignFmtNo ( ) 
               , FsIndentCode := CurrentIndentCode 
               , FsTok := SharedStrings . Tok ( SemDecl . InsertionString ) 
               , FsKind := FsKindTyp . FsKindInsTok 
               , FsInsTokRef := SemDecl . InsertionString 
               , FsIsInsideList := BldIsList 
               , FsIsInsideCondFmt := CondFmtDepth > 0 
               , FsIsInFirstLine := BldIsInFirstLine  
               , FsIsRightOfEstListChild := BldIsRightOfEstListChild
               , FsLdlNodeNo := LdlNode . NodeNo 
               , FsFormatsEmpty := UbFalse 
               ) 
      ; SetLeftAndRightFmtNo ( LResult ) 
      ; LdlSemantics . IncIndentCode 
          ( CurrentIndentCode 
          , SharedStrings . Length ( SemDecl . InsertionString )
            + 1 (* For separator. *)  
          ) 
      ; RETURN LResult 
      END FsString 

  ; PROCEDURE FsChild 
      ( FsChildNode : AstView . AstRefTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ; NewLineIndentCode : LangUtil . IndentCodeTyp 
      ; VAR (* IN OUT *) CurrentIndentCode : LangUtil . IndentCodeTyp 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 
    ; VAR LResult : LangUtil . FsNodeRefTyp 
    ; VAR LIndentCodeChild : AstView . AstRefTyp 
    ; VAR LClassDeclNodeNo : INTEGER 
    ; VAR LAsSemDecl : LdlSemantics . SemDeclTyp 
    ; VAR LWasFound : BOOLEAN 
    ; VAR LHasErrors : BOOLEAN 

    ; BEGIN (* FsChild *) 
        CASE EstUtil . EstTok ( FsChildNode . NodeRef ) 
        OF Ldl0Tok . FsDefaultSubtree 
        , Ldl0Tok . FsHorizSubtree 
        => RETURN 
             FsSubtree 
               ( AstView . Child 
                   ( FsChildNode , Ldl0Child . FsHorizSubtree_Children 
                   , LangInfo . LdlLang ) 
               , FsKindTyp . FsKindSubtreeHoriz 
               , FsKindTyp . FsKindLineBreakOpt 
               , CurrentIndentCode 
               ) 
        | Ldl0Tok . FsVertSubtree 
        => RETURN 
             FsSubtree 
               ( AstView . Child 
                   ( FsChildNode , Ldl0Child . FsVertSubtree_Children 
                   , LangInfo . LdlLang ) 
               , FsKindTyp . FsKindSubtreeVert 
               , FsKindTyp . FsKindLineBreakReqd 
               , CurrentIndentCode 
               ) 
        | Ldl0Tok . FsFillSubtree 
        => RETURN 
             FsSubtree 
               ( AstView . Child 
                   ( FsChildNode , Ldl0Child . FsFillSubtree_Children 
                   , LangInfo . LdlLang ) 
               , FsKindTyp . FsKindSubtreeFill 
               , FsKindTyp . FsKindLineBreakOpt 
               , CurrentIndentCode 
               ) 
        | Ldl0Tok . LineBreak 
        => CurrentIndentCode := NewLineIndentCode 
        ; LIndentCodeChild 
            := AstView . Child 
                 ( FsChildNode , Ldl0Child . LineBreak_IndentCode 
                 , LangInfo . LdlLang 
                 ) 
        ; IF LIndentCodeChild . NodeRef # NIL 
          THEN 
            LdlSemantics . IncIndentCode 
              ( CurrentIndentCode , AstView . IntValue ( LIndentCodeChild ) ) 
          END (* IF *) 
        ; LResult 
            := NEW 
                 ( LangUtil . FsNodeRefTyp 
                 , FsFmtNo := AssignFmtNo ( ) 
                 , FsIndentCode := CurrentIndentCode 
                 , FsKind := FsKindForLineBreak 
                 , FsIsInsideList := BldIsList 
                 , FsIsInsideCondFmt := CondFmtDepth > 0 
                 , FsIsInFirstLine := BldIsInFirstLine  
                 , FsIsRightOfEstListChild := BldIsRightOfEstListChild
                 , FsLdlNodeNo := FsChildNode . NodeNo 
                 , FsFormatsEmpty := UbTrue 
                 ) 
        ; SetLeftAndRightFmtNo ( LResult ) 
        ; IF BldPrevLineBreakRef # NIL 
          THEN 
            BldPrevLineBreakRef . FsLineBreakToReachFmtNo := LResult . FsFmtNo 
          END (* IF *) 
        ; BldPrevLineBreakRef := LResult  
        ; BldIsInFirstLine := FALSE 
        ; RETURN LResult 
        | Ldl0Tok . Ident 
        => TYPECASE FsChildNode . NodeRef 
           OF SharedStrings . T ( TSharedString ) 
           => TextIntSymbolTable . Find 
                ( LangInfo . SymbolTable 
                , SharedStrings . ToText ( TSharedString ) 
                , LWasFound 
                , LClassDeclNodeNo 
                ) 
           ; IF LWasFound 
             THEN 
               LAsSemDecl 
                 := LangInfo . SemMapRef ^ [ LClassDeclNodeNo ] . SemRef 
             ; LangInfo . SemMapRef ^ [ FsChildNode . NodeNo ] . SemRef 
                 := NEW 
                      ( LdlSemantics . SemRefTyp 
                      , NodeNo := FsChildNode . NodeNo 
                      , RefDeclId := LClassDeclNodeNo 
                      , RefTok := LAsSemDecl . DeclTok 
                      ) 
             ; TYPECASE LAsSemDecl 
               OF NULL 
               => CantHappen ( AFT . A_Ldl0FsTreesFsChildNotFirstOcc ) 
               ; RETURN NIL 
               | LdlSemantics . SemFirstOccStringTyp ( TSemDecl ) 
               => (* Named string. *) 
                  RETURN 
                    FsString ( FsChildNode , TSemDecl , CurrentIndentCode ) 
               | LdlSemantics . SemDeclTyp 
               => Ldl0Semantics . CheckContainment 
                    ( LangInfo 
                    , FsChildNode . NodeNo 
                    , LAsSemDecl . NodeNo 
                    , LHasErrors 
                    ) 
               ; IF NOT LHasErrors 
                 THEN 
                   IF NextFmtNo = EstHs . FmtNoNull 
                   THEN 
                     NextFmtNo := EstHs . FmtNoListEstChild 
                   END (* IF *) 
                 ; FsEstChild 
                     := NEW 
                          ( LangUtil . FsNodeRefTyp 
                          , FsFmtNo := AssignFmtNo ( ) 
                          , FsIndentCode := CurrentIndentCode 
                          , FsTok := LAsSemDecl . DeclTok 
                          , FsKind := BldFsKindForEstChild 
                          , FsChildIndentCode := CurrentIndentCode 
                          , FsIsInsideList := BldIsList 
                          , FsIsInsideCondFmt := CondFmtDepth > 0 
                          , FsIsInFirstLine := BldIsInFirstLine  
                          , FsLdlNodeNo := FsChildNode . NodeNo 
                          , FsFormatsEmpty := UbUnknown 
                          ) 
                  ; IF BldIsList 
                    THEN
                      LResult . FsFirstListElemIndentCode := CurrentIndentCode 
(* FIXME: ^This is not right. *) 
                    END (* IF *) 
                 ; SetLeftAndRightFmtNo ( FsEstChild ) 
                 ; INC ( FsEstChildCt ) 
                 ; CheckFmtNoMatch ( FsChildNode , FsEstChild ) 
                 ; BldIsRightOfEstListChild 
                     := BldIsRightOfEstListChild OR BldIsList 
                 ; RETURN FsEstChild 
                 ELSE RETURN NIL 
                 END (* IF *) 
               END (* TYPECASE *) 
             ELSE 
               SemError 
                 ( FsChildNode . NodeNo 
                 , AFT . E_FormatSyntaxChildIsUndeclared 
                 ) 
             ; RETURN NIL 
             END (* IF *) 
           ELSE 
             CantHappen ( AFT . A_Ldl0FsTreesFsChild_IdentTokenNotString ) 
           ; RETURN NIL 
           END (* TYPECASE *) 
        | Ldl0Tok . String 
        => TYPECASE 
             Ldl0Semantics . FirstOcc 
               ( LangInfo 
               , LangInfo . SemMapRef ^ [ FsChildNode . NodeNo ] . SemRef 
               ) 
           OF LdlSemantics . SemFirstOccStringTyp ( TSemDecl ) 
           => (* Literal string. *) 
              RETURN FsString ( FsChildNode , TSemDecl , CurrentIndentCode ) 
           ELSE 
             CantHappen 
               ( AFT . A_Ldl0FsTreesFsChild_SemStringOnNotStringFirstOccurrence ) 
           ; RETURN NIL 
           END (* TYPECASE *) 
        | Ldl0Tok . FsChildPlain 
        => IF NextAsChildExists 
           THEN 
             LResult 
               := FsChildPlain 
                    ( NextAsChildName 
                    , NextAsChildClass 
                    , FsChildNode 
                    , CurrentIndentCode 
                    ) 
           ; IF LResult # NIL AND LResult . FsEstChildIsOptional
             THEN (* For handwritten grammar, there is one OptionId for each
                     optional AS child.
                  *)   
               IncOptionId ( BldNextOptionId , "handwritten" ) 
             END (* IF *) 
           ; ConsumeAsChild ( ) 
           ; RETURN LResult 
           ELSE 
             SemError2 
               ( FsChildNode . NodeNo 
               , AsChildren . NodeNo 
               , AFT . E_AsNodeHasNoChildForThisFsChild 
               ) 
           ; RETURN NIL 
           END (* IF *) 
        | Ldl0Tok . FsCondPresent 
        => AstView . GetChildren 
             ( FsChildNode , LChildren , LangInfo . LdlLang ) 
        ; RETURN 
            FsChildCondFmt 
              ( FsChildNode 
              , PredicateKindTyp . PredicateKindPresent 
              , LChildren [ Ldl0Child . FsCondPresent_Children ] 
              , AstView . AstRefNull 
              , FsKindForLineBreak
              , CurrentIndentCode 
              ) 
        | Ldl0Tok . FsCondNonempty 
        => AstView . GetChildren 
             ( FsChildNode , LChildren , LangInfo . LdlLang ) 
        ; RETURN 
            FsChildCondFmt 
              ( FsChildNode 
              , PredicateKindTyp . PredicateKindNonemptyList 
              , LChildren [ Ldl0Child . FsCondNonempty_Children ] 
              , AstView . AstRefNull 
              , FsKindForLineBreak
              , CurrentIndentCode 
              ) 
        | Ldl0Tok . FsCondPlural 
        => AstView . GetChildren 
             ( FsChildNode , LChildren , LangInfo . LdlLang ) 
        ; RETURN 
            FsChildCondFmt 
              ( FsChildNode 
              , PredicateKindTyp . PredicateKindPluralList 
              , LChildren [ Ldl0Child . FsCondPlural_Children ] 
              , AstView . AstRefNull 
              , FsKindForLineBreak
              , CurrentIndentCode 
              ) 
        | Ldl0Tok . FsCondMember 
        => AstView . GetChildren 
             ( FsChildNode , LChildren , LangInfo . LdlLang ) 
        ; RETURN 
            FsChildCondFmt 
              ( FsChildNode 
              , PredicateKindTyp . PredicateKindInClass 
              , LChildren [ Ldl0Child . FsCondMember_Children ] 
              , LChildren [ Ldl0Child . FsCondMember_Class ] 
              , FsKindForLineBreak
              , CurrentIndentCode 
              ) 
        ELSE 
          CantHappen ( AFT . A_Ldl0FsTreesFsChildBadSyntax ) 
        ; RETURN NIL 
        END (* CASE *) 
      END FsChild 

  ; PROCEDURE FsSubtree 
      ( FsChildrenNode : AstView . AstRefTyp 
      ; FsKind : FsKindTyp 
      ; FsKindForLineBreak : FsKindTyp 
      ; IndentCode : LangUtil . IndentCodeTyp 
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LFsChild : AstView . AstRefTyp 
    ; VAR LFsChildExists : BOOLEAN 
    ; VAR LTempFsChildrenRef : LangUtil . FsChildrenArrayRefTyp 
    ; VAR LNextFsChildNo : LangUtil . FsChildNoTyp 
    ; VAR LIndentCode : LangUtil . IndentCodeTyp 
    ; VAR LLeftFmtNo : EstHs . FmtNoTyp 
    ; VAR LRightFmtNo : EstHs . FmtNoTyp 
    ; VAR LSavePrevLineBreakRef : LangUtil . FsNodeRefTyp 
    ; VAR LResult : LangUtil . FsNodeRefTyp 
    ; VAR LLeadingChildCt : LbeStd . EstChildNoTyp := 0 
    ; VAR LFsTreeAstChild : LangUtil . FsNodeRefTyp := NIL 
    ; VAR LLdlEstChild : AstView . AstRefTyp := AstView . AstRefNull 

    ; BEGIN (* FsSubtree  *) 
        LSavePrevLineBreakRef := BldPrevLineBreakRef 
      ; BldPrevLineBreakRef := NIL 
      ; AstView . FirstChild ( FsChildrenNode , LFsChild , LFsChildExists ) 
      ; LTempFsChildrenRef 
          := NEW 
               ( LangUtil . FsChildrenArrayRefTyp 
               , AstView . FastExtravagantChildCt ( FsChildrenNode ) 
                 + 2 * ORD ( IsStart ) 
               ) 
      ; LNextFsChildNo := 0 
      ; LIndentCode := IndentCode 
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
                   , FsIsInsideList := FALSE 
                   , FsIsInsideCondFmt := FALSE  
                   , FsIsInFirstLine := BldIsInFirstLine  
                   , FsLdlNodeNo := FsChildrenNode . NodeNo 
                   , FsFormatsEmpty := UbTrue 
                   ) 
          ; SetLeftAndRightFmtNo ( WFsNodeRef ) 
          END (* WITH *) 
        ; INC ( LNextFsChildNo ) 
        END (* IF *) 
      ; WHILE LFsChildExists 
        DO WITH WFsTreeChild = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
          DO WFsTreeChild  
               := FsChild 
                    ( LFsChild 
                    , FsKindForLineBreak 
                    , IndentCode 
                    , LIndentCode 
                    ) 

          ; IF CondFmtDepth > 0 
               AND WFsTreeChild # NIL 
               AND NOT WFsTreeChild . FsKind 
                       IN LangUtil . FsKindSetFormatting 
            THEN (* This subtree is the Est-containing child of a CondFmt. *) 
              IF LFsTreeAstChild = NIL 
              THEN 
                LLeadingChildCt := LNextFsChildNo 
              ; LFsTreeAstChild := WFsTreeChild 
              ; LLdlEstChild := LFsChild 
              ELSE 
                SemError2 
                  ( LFsChild . NodeNo 
                  , LLdlEstChild . NodeNo 
                  , AFT . E_OnlyOneNonFormatterChildAllowedWhenInsideConditionalFormat
                  ) 
              END (* IF *) 
            END (* IF *) 
          END (* WITH *) 
        ; INC ( LNextFsChildNo ) 
        ; AstView . NextChild ( FsChildrenNode , LFsChild , LFsChildExists ) 
        END (* WHILE *) 
      ; IF IsStart AND NOT LbeStd . UseAugmentTrees 
        THEN 
          WITH WFsNodeRef = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
          DO 
            WFsNodeRef 
              := NEW 
                   ( LangUtil . FsNodeRefTyp 
                   , FsFmtNo := AssignFmtNo ( ) 
                   , FsIndentCode := IndentCode 
                   , FsTok := LbeStd . Tok__EndOfImage 
                   , FsKind := FsKindTyp . FsKindEndOfImage 
                   , FsLeadingChildCt := LLeadingChildCt 
                   , FsIsInsideList := FALSE 
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
      ; LResult 
          := NEW 
               ( LangUtil . FsNodeRefTyp 
               , FsIndentCode := IndentCode 
               , FsChildren 
                   := NEW ( LangUtil . FsChildrenArrayRefTyp , LNextFsChildNo ) 
               , FsKind := FsKind 
               , FsLeftFmtNo := LLeftFmtNo 
               , FsRightFmtNo := LRightFmtNo 
               , FsEstDescendantRef := FsEstChild 
               , FsIsInsideList := BldIsList 
               , FsIsInsideCondFmt := CondFmtDepth > 0 
               , FsLdlNodeNo := FsChildrenNode . NodeNo 
               , FsFormatsEmpty := UbUnknown 
               ) 
      ; IF CondFmtDepth > 0 AND FsEstChild # NIL 
        THEN
          LResult . FsOptionIds := FsEstChild . FsOptionIds  
        END (* IF *)  
      ; LResult . FsChildren ^ 
          := SUBARRAY ( LTempFsChildrenRef ^ , 0 , LNextFsChildNo ) 
      ; IF BldPrevLineBreakRef # NIL 
        THEN 
          BldPrevLineBreakRef . FsLineBreakToReachFmtNo := LRightFmtNo 
        END (* IF *) 
      ; BldPrevLineBreakRef := LSavePrevLineBreakRef 
      ; RETURN LResult 
      END FsSubtree 

  ; PROCEDURE BuildFmtNoMaps 
      ( FsRef : LangUtil . FsNodeRefTyp 
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
          CASE FsRef . FsKind 
          OF FsKindTyp . FsKindEstChildOfList 
          => Assert 
               ( FsRef . FsFmtNo = EstHs . FmtNoListEstChild 
               , AFT . A_BuildFmtNoMaps_NotStartingOver 
               ) 
          | FsKindTyp . FsKindEstChildOfFixed 
          , FsKindTyp . FsKindBegOfImage 
          , FsKindTyp . FsKindEndOfImage 
          , FsKindTyp . FsKindInsTok 
          , FsKindTyp . FsKindLineBreakOpt 
          , FsKindTyp . FsKindLineBreakReqd 
          => IF FsRef . FsFmtNo = EstHs . FmtNoNull 
             THEN 
               FsRef . FsFmtNo := AssignFmtNo ( ) 
             ; SetLeftAndRightFmtNo ( FsRef ) 
             END (* IF *) 
          ; MaxFmtNo := MAX ( MaxFmtNo , FsRef . FsFmtNo ) 
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
          => LMaxFmtNo := 0 
          ; LLastChildNo := NUMBER ( FsRef . FsChildren ^ ) - 1 
          ; FOR RChildNo := 0 TO LLastChildNo 
            DO WITH WChild = FsRef . FsChildren ^ [ RChildNo ] 
               DO IF WChild # NIL 
                  THEN 
                    BuildFmtNoMaps ( WChild , LMaxFmtNo ) 
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
                    , FsKindTyp . FsKindBegOfImage 
                    , FsKindTyp . FsKindEndOfImage 
                    , FsKindTyp . FsKindInsTok 
                    , FsKindTyp . FsKindLineBreakOpt 
                    , FsKindTyp . FsKindLineBreakReqd 
                    => FsRef . FsFmtNoMapRef ^ [ WChild . FsFmtNo ] := RChildNo 
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
                       DO IF WChild . FsFmtNoMapRef ^ [ RChildFmtNo ] 
                             # LangUtil . FsChildNoNull 
                          THEN 
                            FsRef . FsFmtNoMapRef ^ [ RChildFmtNo ] := RChildNo 
                          END (* IF *) 
                       END (* FOR *) 
                    END (* CASE *) 
                  END (* IF *) 
               END (* WITH *) 
            END (* FOR *) 
          ; MaxFmtNo := MAX ( MaxFmtNo , LMaxFmtNo ) 
          ELSE 
            CantHappen ( AFT . A_BuildFmtNoMaps_BadFsKind ) 
          END (* CASE *) 
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
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = VAR LResult : LangUtil . FsNodeRefTyp 
    ; VAR LMaxFmtNo : EstHs . FmtNoTyp 
    ; VAR LAsGrandChildren : ARRAY [ 0 .. 1 ] OF AstView . AstRefTyp 

    ; BEGIN (* FixedRule *) 
        BldFsKindForLineBreak := FsKindTyp . FsKindLineBreakOpt 
      ; BldFsKindForEstChild := FsKindTyp . FsKindEstChildOfFixed 
      ; BldIsList := FALSE  
      ; BldIsInFirstLine := TRUE 
      ; BldIsRightOfEstListChild := FALSE 
      ; BldPrevLineBreakRef := NIL 
      ; CASE EstUtil . EstTok ( BldAsRuleNode . NodeRef ) 
        OF Ldl0Tok . AsFixedRule 
        => AsChildren 
             := AstView . Child 
                  ( BldAsRuleNode , Ldl0Child . AsFixedRule_Children 
                  , LangInfo . LdlLang ) 
        ; AstView . FirstChild ( AsChildren , NextAsChild , NextAsChildExists ) 
        ; IF NextAsChildExists 
          THEN 
            AstView . GetChildren 
              ( NextAsChild , LAsGrandChildren , LangInfo . LdlLang ) 
          ; NextAsChildName 
              := LAsGrandChildren [ Ldl0Child . AsReqdChild_ChildName ] 
          ; NextAsChildClass 
              := LAsGrandChildren [ Ldl0Child . AsReqdChild_ChildClass ] 
          END (* IF *) 
        ; FsEstChildCt := 0 
        ; FsEstChild := NIL 
        ; NextFmtNo := EstHs . FmtNoInitial 
        ; LResult 
            := FsSubtree 
                 ( AstView . Child 
                     ( BldFsRuleNode , Ldl0Child . FsFixedHorizRule_Children 
                     , LangInfo . LdlLang ) 
                  (* ^ This depends on FsFixed[Horiz|Vert|Fill]Rule 
                       having the same form. *) 
                 , FsKind 
                 , FsKindForLineBreak
                 , LangUtil . IndentCodeInitial 
                 ) 
        ; LResult . FsTok := AsIdentSemRef . DeclTok 
        ; LMaxFmtNo := 0 (* Not actually needed. *) 
        ; BuildFmtNoMaps ( LResult , LMaxFmtNo ) 
        ; IF NextAsChildExists 
          THEN 
            SemError2 
              ( BldFsIdentNode . NodeNo 
              , NextAsChild . NodeNo 
              , AFT . E_FormatSyntaxRuleNoChildForAsChild 
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
      ; IF BldIsInFirstLine AND FsEstChildCt >= 2  
        THEN
(* FIX: This check is not right. *) 
          SemError
            ( BldFsIdentNode . NodeNo 
            , AFT . W_Ldl0_FS_Fixed_rule_has_no_line_break
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
      ) 
    : LangUtil . FsNodeRefTyp 
    RAISES { AssertionFailure } 

    = CONST IsStar = TRUE 

    ; VAR LFsChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 
    ; VAR LAsChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 
    ; VAR LTempFsChildrenRef : LangUtil . FsChildrenArrayRefTyp 
    ; VAR LNextFsChildNo : LangUtil . FsChildNoTyp 
    ; VAR LAsRuleTok : LbeStd . TokTyp 
    ; VAR LFsChild : AstView . AstRefTyp 
    ; VAR LFsChildExists : BOOLEAN 
    ; VAR LResult : LangUtil . FsNodeRefTyp 
    ; VAR LMaxFmtNo : EstHs . FmtNoTyp 
    ; VAR LIndentCode : LangUtil . IndentCodeTyp 
    ; VAR LLeftFmtNo : EstHs . FmtNoTyp 
    ; VAR LRightFmtNo : EstHs . FmtNoTyp 
    ; VAR LThruFmtNo : EstHs . FmtNoTyp 
    ; VAR LSemDeclAsListNodeRef : LdlSemantics . SemDeclAsListNodeTyp 
    ; VAR LFsEstDescendantRef : LangUtil . FsNodeRefTyp := NIL 

    ; BEGIN (* ListRule *) 
        BldFsKindForLineBreak := FsKindTyp . FsKindLineBreakOpt 
      ; BldFsKindForEstChild := FsKindTyp . FsKindEstChildOfList 
      ; BldIsRightOfEstListChild := TRUE  
      ; FsEstChild := NIL 
      ; BldIsList := TRUE 
      ; BldIsInFirstLine := FALSE  
      ; BldPrevLineBreakRef := NIL 
      ; AstView . GetChildren ( BldFsRuleNode , LFsChildren , LangInfo . LdlLang ) 
      ; NextFmtNo := EstHs . FmtNoNull 
      ; WITH 
          WListChild = LFsChildren [ Ldl0Child . FsListHorizRule_ListChild ] 
        DO WITH 
             WFormatters 
             = LFsChildren [ Ldl0Child . FsListHorizRule_Formatters ] 
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
             OF Ldl0Tok . AsPlusRule , Ldl0Tok . AsStarRule 
             => AstView . GetChildren 
                  ( BldAsRuleNode , LAsChildren , LangInfo . LdlLang ) 
             ; NextAsChildName 
                 := LAsChildren [ Ldl0Child . AsStarRule_ChildName ] 
             ; NextAsChildClass 
                 := LAsChildren [ Ldl0Child . AsStarRule_ChildClass ] 
             ; NextAsChildExists := TRUE 
             ; AsChildren := AstView . AstRefNull 
             ; CASE EstUtil . EstTok ( WListChild . NodeRef ) 
               OF Ldl0Tok . FsChildPlain 
               => LTempFsChildrenRef ^ [ LNextFsChildNo ] 
                    := FsChildPlain 
                         ( NextAsChildName 
                         , NextAsChildClass 
                         , WListChild 
                         , LIndentCode 
                         ) 
               ; LFsEstDescendantRef 
                   := LTempFsChildrenRef ^ [ LNextFsChildNo ] 
               ; INC ( LNextFsChildNo ) 
               | Ldl0Tok . FsDefaultSubtree 
               , Ldl0Tok . FsHorizSubtree 
               , Ldl0Tok . FsVertSubtree 
               , Ldl0Tok . FsFillSubtree 
               , Ldl0Tok . FsCondPresent 
               , Ldl0Tok . FsCondNonempty 
               , Ldl0Tok . FsCondPlural 
               , Ldl0Tok . FsCondMember 
               => WITH WNextFsChild = LTempFsChildrenRef ^ [ LNextFsChildNo ] 
                 DO WNextFsChild 
                     := FsChild 
                          ( WListChild 
                          , FsKindForLineBreak
                          , LangUtil . IndentCodeInitial 
                          , LIndentCode 
                          ) 
                 ; IF WNextFsChild # NIL 
                   THEN 
                     LFsEstDescendantRef := WNextFsChild . FsEstDescendantRef  
                   ELSE 
                     LFsEstDescendantRef := NIL 
                   END (* IF *) 
                 END (* WITH *) 
               ; INC ( LNextFsChildNo ) 
               | Ldl0Tok . Ident 
               => SemError
                    ( WListChild . NodeNo 
                    , AFT . E_UnimplementedIdentifierOnlyAsChildOfList 
                    ) 
(* TODO: Allow a simple Ident here also, as is done for fixed children
         in FsChild *) 
               ELSE 
                 SemError 
                   ( WListChild . NodeNo , AFT . E_InvalidListChildKind ) 
               END (* CASE *) 
             (* NOTE: The following shouldn't happen, as there is no such 
                      thing as an FsList rule which is not star. *) 
             ; IF LAsRuleTok = Ldl0Tok . AsStarRule AND NOT IsStar 
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
           ; AstView . FirstChild ( WFormatters , LFsChild , LFsChildExists ) 
           ; WHILE LFsChildExists 
             DO LTempFsChildrenRef ^ [ LNextFsChildNo ] 
                  := FsChild 
                       ( LFsChild 
                       , FsKindForLineBreak
                       , LangUtil . IndentCodeInitial 
                       , LIndentCode 
                       ) 
             ; INC ( LNextFsChildNo ) 
             ; AstView . NextChild ( WFormatters , LFsChild , LFsChildExists ) 
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
           ; IF FsEstChild # NIL 
             THEN
               FsEstChild . FsFirstListElemIndentCode 
                 := LangUtil . IndentCodeInitial 
             END (* IF*) 
           ; LSemDeclAsListNodeRef := AsIdentSemRef (* NARROW OK *) 
           ; LResult 
               := NEW 
                    ( LangUtil . FsNodeRefTyp 
                    , FsChildren 
                        := NEW 
                             ( LangUtil . FsChildrenArrayRefTyp 
                             , LNextFsChildNo 
                             ) 
                    , FsIndentCode := LangUtil . IndentCodeInitial 
                    , FsKind := FsKind 
                    , FsFmtNo := EstHs . FmtNoListEstChild 
                    , FsLeftFmtNo := LLeftFmtNo 
                    , FsRightFmtNo := LRightFmtNo 
                    , FsListSliceThruFmtNo := LThruFmtNo 
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
                    , FsIsInsideList := FALSE 
                    , FsIsInsideCondFmt := FALSE 
                    , FsLdlNodeNo := BldFsRuleNode . NodeNo 
                    , FsFormatsEmpty := UbUnknown 
                    ) 
           ; LResult . FsChildren ^ 
               := SUBARRAY ( LTempFsChildrenRef ^ , 0 , LNextFsChildNo ) 
           ; LdlSemantics . PatchListFields ( LResult , LResult , LIndentCode )
           ; LResult . FsTok := AsIdentSemRef . DeclTok 
           ; LMaxFmtNo := 0 (* Not actually needed. *) 
           ; BuildFmtNoMaps ( LResult , LMaxFmtNo ) 
           ; LResult . FsPlaceholder 
               := Placeholder ( BldFsRuleNode , AsIdentSemRef . DeclTok ) 
	   ; IF BldPrevLineBreakRef # NIL 
 	     THEN 
	       BldPrevLineBreakRef . FsLineBreakToReachFmtNo := LRightFmtNo 
	     END (* IF *) 
           END (* WITH *) 
        END (* WITH *) 
      ; IF BldIsInFirstLine 
        THEN
          SemError
            ( BldFsIdentNode . NodeNo 
            , AFT . W_Ldl0_FS_List_rule_has_no_line_break
            ) 
(* TODO: Also warn if some alternatives lack a line break, and there is none
       following the conditional construct.  The latter condition takes
       yet another semi-global boolean to detect. *) 
        END (* IF *) 
      ; RETURN LResult 
      END ListRule 

  ; BEGIN (* Build *) 
      CondFmtDepth := 0 
    ; BldNextOptionId := FIRST ( LRTable . OptionIdRealTyp )
    ; BldNextCondId := FIRST ( LRTable . OptionIdRealTyp )

    ; VAR LResult : LangUtil . FsNodeRefTyp 
    ; BEGIN (* Block for Build *) 
   (*   IsStart := FALSE (* Trying new system of putting the EOI/BOI on the
                            augmenting production, rather than the original
                            start production. *) 
   *) 
        CASE EstUtil . EstTok ( BldFsRuleNode . NodeRef ) <* NOWARN *> 
        OF Ldl0Tok . FsFixedDefaultRule 
        , Ldl0Tok . FsFixedHorizRule 
        => LResult :=  
             FixedRule 
               ( LangUtil . FmtKindTyp . FmtKindHoriz 
               , FsKindTyp . FsKindEstFixedHoriz 
               , FsKindTyp . FsKindLineBreakOpt  
               ) 
        | Ldl0Tok . FsFixedVertRule 
        => LResult :=  
             FixedRule 
               ( LangUtil . FmtKindTyp . FmtKindVert 
               , FsKindTyp . FsKindEstFixedVert 
               , FsKindTyp . FsKindLineBreakReqd  
               ) 
        | Ldl0Tok . FsFixedFillRule 
        => LResult :=  
             FixedRule 
               ( LangUtil . FmtKindTyp . FmtKindFill 
               , FsKindTyp . FsKindEstFixedFill 
               , FsKindTyp . FsKindLineBreakOpt  
               ) 
        | Ldl0Tok . FsListDefaultRule 
        , Ldl0Tok . FsListHorizRule 
        => LResult :=  
             ListRule 
               ( LangUtil . FmtKindTyp . FmtKindHoriz 
               , FsKindTyp . FsKindEstListHoriz 
(* FIXME: Could be FsKindEstListTrail... *) 
               , FsKindTyp . FsKindLineBreakOpt  
               ) 
        | Ldl0Tok . FsListVertRule 
        => LResult :=  
             ListRule 
               ( LangUtil . FmtKindTyp . FmtKindVert 
               , FsKindTyp . FsKindEstListVert 
(* FIXME: Could be FsKindEstListTrail... *) 
               , FsKindTyp . FsKindLineBreakReqd
               ) 
        | Ldl0Tok . FsListFillRule 
        => LResult :=  
             ListRule 
               ( LangUtil . FmtKindTyp . FmtKindFill 
               , FsKindTyp . FsKindEstListFill 
(* FIXME: Could be FsKindEstListTrail... *) 
               , FsKindTyp . FsKindLineBreakOpt  
               ) 
        END (* CASE *) 
      ; TRY 
          LdlSemantics . PostNumberFsTree ( LResult ) 
        EXCEPT LdlSemantics . NodeNoOverflow (* ( EMaxValue ) *)  
        => SemError 
            ( BldFsIdentNode . NodeNo 
            , AFT . E_FormatSyntaxRuleTooComplex 
            ) 
        END 
      ; RETURN LResult 
      END (* Block for Build *) 
    END Build 

; PROCEDURE Checks ( ) 

  = <* FATAL AssertionFailure *> 
    BEGIN
      Assert 
        ( Ldl0Child . AsReqdChild_ChildName = Ldl0Child . AsOptChild_ChildName 
          AND Ldl0Child . AsReqdChild_ChildClass 
              = Ldl0Child . AsOptChild_ChildClass 
        , AFT . A_Ldl0FsTrees_DifferentChildNameOrClass 
        ) 
      (* ^FixedRule depends on this equality. *) 
    ; Assert 
        ( Ldl0Child . AsStarRule_ChildName = Ldl0Child . AsPlusRule_ChildName 
          AND Ldl0Child . AsStarRule_ChildClass 
              = Ldl0Child . AsPlusRule_ChildClass 
        , AFT . A_Ldl0FsTrees_DifferentChildNameOrClass 
        ) 
      (* ^ListRule depends on this equality. *) 
    END Checks 

; BEGIN (* Ldl0FsTrees *) 
    Checks ( ) 
  END Ldl0FsTrees 
. 
