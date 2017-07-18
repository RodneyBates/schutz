
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Ldl1Semantics 

; IMPORT Date 
; IMPORT Fingerprint 
; IMPORT Fmt 
; IMPORT Text 
; IMPORT Time 
; IMPORT Word 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 
; IMPORT AstView 
; IMPORT Automaton 
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT FsTreeUtils
; IMPORT IntSets 
; IMPORT LangMap 
; IMPORT LangUtil 
; IMPORT LangUtilLo 
; IMPORT LbeStd 
; FROM LbeStd IMPORT TokClassTyp 
; IMPORT Ldl1Child 
; IMPORT Ldl1FsTrees 
; IMPORT Ldl1Tok 
; IMPORT LdlSemantics 
; FROM LdlSemantics IMPORT DeclKindTyp 
; IMPORT LRTable 
; FROM LRTable IMPORT NontermKindTyp
; IMPORT MessageCodes 
; IMPORT Messages 
; FROM Messages IMPORT SemError , SemError2 , SemErrorText  
; IMPORT PortTypes 
; IMPORT SharedStrings 
; IMPORT TextIntSymbolTable 
; IMPORT TokRelation 
; IMPORT TravUtil 
; IMPORT UncertainBool 

; TYPE AFT = MessageCodes . T 

(* Unqualified renames of enumeration values. *) 

; CONST TokClassNull = LbeStd . TokClassTyp . TokClassNull
; CONST TokClassBuiltin = LbeStd . TokClassTyp . TokClassBuiltin
; CONST TokClassMisc = LbeStd . TokClassTyp . TokClassMisc
; CONST TokClassConstTerm = LbeStd . TokClassTyp . TokClassConstTerm
; CONST TokClassVarTerm = LbeStd . TokClassTyp . TokClassVarTerm
; CONST TokClassVarTermMod = LbeStd . TokClassTyp . TokClassVarTermMod
; CONST TokClassAsPlus = LbeStd . TokClassTyp . TokClassAsPlus
; CONST TokClassAsPlusTrailing = LbeStd . TokClassTyp . TokClassAsPlusTrailing
; CONST TokClassAsStarTrailing = LbeStd . TokClassTyp . TokClassAsStarTrailing
; CONST TokClassAsStar = LbeStd . TokClassTyp . TokClassAsStar
; CONST TokClassAsFixed = LbeStd . TokClassTyp . TokClassAsFixed
; CONST TokClassSublist = LbeStd . TokClassTyp . TokClassSublist
; CONST TokClassListCard = LbeStd . TokClassTyp . TokClassListCard
; CONST TokClassPartial = LbeStd . TokClassTyp . TokClassPartial
; CONST TokClassAsClass = LbeStd . TokClassTyp . TokClassAsClass
; CONST TokClassAsCsClass = LbeStd . TokClassTyp . TokClassAsCsClass
; CONST TokClassCsClass = LbeStd . TokClassTyp . TokClassCsClass
; CONST TokClassCsPlus = LbeStd . TokClassTyp . TokClassCsPlus
; CONST TokClassCsPlural = LbeStd . TokClassTyp . TokClassCsPlural
; CONST TokClassCsStar = LbeStd . TokClassTyp . TokClassCsStar
; CONST TokClassCsFixed = LbeStd . TokClassTyp . TokClassCsFixed
; CONST TokClassCsGen = LbeStd . TokClassTyp . TokClassCsGen
; CONST TokClassUnused = LbeStd . TokClassTyp . TokClassUnused

; CONST ListCardEmpty = LdlSemantics . ListCardTyp . ListCardEmpty 
; CONST ListCardSingleton = LdlSemantics . ListCardTyp . ListCardSingleton 
; CONST ListCardPlural = LdlSemantics . ListCardTyp . ListCardPlural 

(* VISIBLE: *) 
; PROCEDURE FirstOcc 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; SemRef : LdlSemantics . SemTyp 
    ) 
  : LdlSemantics . SemDeclTyp 
  RAISES { AssertionFailure } 

  = BEGIN (* FirstOcc *) 
      TYPECASE SemRef 
      OF LdlSemantics . SemFirstOccTyp ( TSemFirstOcc ) 
      => RETURN TSemFirstOcc 
      | LdlSemantics . SemAddlDefCsTyp ( TAddlDef ) 
      => WITH WDecl = LangInfo . SemMapRef ^ [ TAddlDef . RefDeclId ] 
         DO TYPECASE WDecl . SemRef 
            OF LdlSemantics . SemDeclTyp ( TSemDecl ) 
            => RETURN TSemDecl 
            ELSE 
              CantHappen 
                ( AFT . A_Ldl1SemanticsClassFirstOcc_Bad_first_CS_occurence ) 
            ; RETURN NIL 
            END (* TYPECASE *) 
         END (* WITH *) 
      | LdlSemantics . SemAddlDefClassTyp ( TAddlDef ) 
      => WITH WDecl = LangInfo . SemMapRef ^ [ TAddlDef . RefDeclId ] 
         DO TYPECASE WDecl . SemRef 
            OF LdlSemantics . SemFirstOccClassTyp ( TSemDecl ) 
            => RETURN TSemDecl 
            ELSE 
              CantHappen 
                ( AFT . A_Ldl1SemanticsClassFirstOcc_BadFirstClassOcc ) 
            ; RETURN NIL 
            END (* TYPECASE *) 
         END (* WITH *) 
      | LdlSemantics . SemAddlDefStringTyp ( TAddlDef ) 
      => WITH WDecl = LangInfo . SemMapRef ^ [ TAddlDef . RefDeclId ] 
         DO TYPECASE WDecl . SemRef 
            OF LdlSemantics . SemFirstOccStringTyp ( TSemDecl ) 
            => RETURN TSemDecl 
            ELSE 
              CantHappen ( AFT . A_Ldl1SemanticsClassFirstOcc_BadStringDef ) 
            ; RETURN NIL 
            END (* TYPECASE *) 
         END (* WITH *) 
      | LdlSemantics . SemDeclTyp ( TSemDecl ) 
      => RETURN TSemDecl 
      | LdlSemantics . SemRefTyp ( TSemRef ) 
      => RETURN 
           FirstOcc 
             ( LangInfo 
             , LangInfo . SemMapRef ^ [ TSemRef . RefDeclId ] . SemRef 
             ) 
      ELSE 
        CantHappen ( AFT . A_FirstOcc_BadSemRefTyp ) 
      ; RETURN NIL 
      END (* TYPECASE *) 
    END FirstOcc 

(* VISIBLE: *) 
; PROCEDURE CheckContainment 
    ( LangInfo : LdlSemantics . LangInfoRefTyp 
    ; SuperNodeNo : LbeStd . EstNodeNoTyp 
    ; SubNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR (* IN OUT *) HasError : BOOLEAN 
    ) 

  = VAR LSuperSemRef : LdlSemantics . SemRefTyp 
  ; VAR LSubSemRef : LdlSemantics . SemRefTyp 
  ; VAR LSuperSemDecl : LdlSemantics . SemDeclTyp 
  ; VAR LSubSemDecl : LdlSemantics . SemDeclTyp 

  ; BEGIN (* CheckContainment *) 
      LSuperSemRef := LangInfo . SemMapRef ^ [ SuperNodeNo ] . SemRef 
    ; TYPECASE LSuperSemRef 
      OF NULL 
      => SemError ( SuperNodeNo , AFT . E_Nil_Super_Reference ) 
(* CHECK: should this really be an error, an assertion failure, or just 
          protection against earlier errors? Same below *) 
      | LdlSemantics . SemRefTyp ( TSuperSemRef ) 
      => LSuperSemDecl 
           := LangInfo . SemMapRef ^ [ TSuperSemRef . RefDeclId ] . SemRef 
      ; LSubSemRef := LangInfo . SemMapRef ^ [ SubNodeNo ] . SemRef 
      ; TYPECASE LSubSemRef 
        OF NULL 
        => SemError ( SubNodeNo , AFT . E_BadSubReference ) 
        | LdlSemantics . SemRefTyp ( TSubSemRef ) 
        => LSubSemDecl 
             := LangInfo . SemMapRef ^ [ TSubSemRef . RefDeclId ] . SemRef 
        ; TYPECASE LSuperSemDecl 
          OF LdlSemantics . SemFirstOccClassTyp ( TSuperClass ) 
          => TYPECASE LSubSemDecl 
             OF LdlSemantics . SemFirstOccClassTyp ( TSubClass ) 
             => IF NOT IntSets . IsSubset 
                         ( TSubClass . TokSet , TSuperClass . TokSet ) 
                THEN 
                  SemError2 
                    ( SuperNodeNo 
                    , SubNodeNo 
                    , AFT . E_ClassDoesNotContainClass 
                    ) 
                ; HasError := TRUE 
                END (* IF *) 
             | LdlSemantics . SemDeclTyp 
             => IF NOT IntSets . IsElement 
                         ( LSubSemDecl . DeclTok , TSuperClass . TokSet ) 
                THEN 
                  SemError2 
                    ( SuperNodeNo 
                    , SubNodeNo 
                    , AFT . E_ClassDoesNotContainNode 
                    ) 
                ; HasError := TRUE 
                END (* IF *) 
             END (* TYPECASE *) 
          | LdlSemantics . SemDeclTyp 
          => TYPECASE LSubSemDecl 
             OF LdlSemantics . SemFirstOccClassTyp ( TSubClass ) 
             => IF TSubClass . SingletonTok = LbeStd . Tok__Null 
                   OR TSubClass . SingletonTok # LSuperSemDecl . DeclTok 
                THEN 
                  SemError2 
                    ( SuperNodeNo 
                    , SubNodeNo 
                    , AFT . E_NodeIsNotSupersetOfClass 
                    ) 
                ; HasError := TRUE 
                END (* IF *) 
             | LdlSemantics . SemDeclTyp 
             => IF LSubSemDecl . DeclTok # LSuperSemDecl . DeclTok 
                THEN 
                  SemError2 
                    ( SuperNodeNo , SubNodeNo , AFT . E_NodeIsNotEqual ) 
                ; HasError := TRUE 
                END (* IF *) 
             END (* TYPECASE *) 
          END (* TYPECASE *) 
        END (* TYPECASE *) 
      END (* TYPECASE *) 
    END CheckContainment 

; PROCEDURE CsDeclKindMatchesAsDeclKind 
    ( CsDeclKind : DeclKindTyp ; AsDeclKind : DeclKindTyp ) : BOOLEAN 
(* TODO: Use ranges for these formals, and put the ranges in LdlSemantics. *)

  = BEGIN (* CsDeclKindMatchesAsDeclKind *) 
      CASE AsDeclKind 
      OF DeclKindTyp . AsClassRule
      , DeclKindTyp . AsCsClassRule 
(* TODO: Allow a singleton class here. *) 
      => RETURN FALSE  
      | DeclKindTyp . FixedRule 
      => RETURN CsDeclKind = DeclKindTyp . CsFixedRule 
                OR CsDeclKind = DeclKindTyp . CsAltRule 
      | DeclKindTyp . StarRule 
      => RETURN CsDeclKind = DeclKindTyp . CsStarRule 
                OR CsDeclKind = DeclKindTyp . CsPlusRule
                OR CsDeclKind = DeclKindTyp . CsPluralRule
                OR CsDeclKind = DeclKindTyp . CsFixedRule  
                OR CsDeclKind = DeclKindTyp . CsAltRule 
(* TODO: ^Insist that a CsAltRule actually be a singleton alternation. *) 
      | DeclKindTyp . PlusRule 
      => RETURN CsDeclKind = DeclKindTyp . CsPlusRule 
                OR CsDeclKind = DeclKindTyp . CsPluralRule
                OR CsDeclKind = DeclKindTyp . CsFixedRule 
                OR CsDeclKind = DeclKindTyp . CsAltRule 
(* TODO: ^Insist that a CsAltRule actually be a singleton alternation. *) 
(* TODO: ^Code these sensibly with sets. *) 
      ELSE 
        RETURN FALSE 
      END (* CASE *) 
    END CsDeclKindMatchesAsDeclKind 

; PROCEDURE VersionNos 
      ( Lang : LbeStd . LangTyp ; EstRef : EstHs . EstRefTyp ) 
  : LdlSemantics . IntListRefTyp 
  RAISES { AssertionFailure } 

  = VAR VnCt : PortTypes . Int32Typ 
  ; VAR VnResult : LdlSemantics . IntListRefTyp := NIL 

  ; PROCEDURE CountChild ( <* UNUSED *> Node : AstView . AstRefTyp )  

    = BEGIN 
        INC ( VnCt ) 
      END CountChild  

  ; PROCEDURE DoChild ( Node : AstView . AstRefTyp )  

    = VAR LValue : PortTypes . Int32Typ 

    ; BEGIN 
        LValue := AstView . IntValue ( Node ) 
      ; IF LValue < 0 
        THEN 
          SemError 
            ( Node . NodeNo 
            , AFT . E_VersionComponentMustBeNonnegative 
            ) 
        ; LValue := 0 
        ELSIF LValue > 255 
        THEN 
          SemError 
            ( Node . NodeNo 
            , AFT . E_VersionComponentMustFitInAByte  
            ) 
        ; LValue := 255 
        END (* IF *) 
      ; VnResult ^ [ VnCt ] := LValue 
      ; INC ( VnCt ) 
      END DoChild 

  ; BEGIN (* VersionNos *) 
      VAR LLangDefNode : AstView . AstRefTyp 
    ; VAR LVersionNode : AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        LLangDefNode 
          := AstView . AstRefTyp 
               { NodeRef := EstRef , NodeNo := 0 , ChildNo :=0 }
      ; LVersionNode 
          := AstView . Child 
               ( LLangDefNode 
               , Ldl1Child . LanguageDefinition_Version 
               , Lang 
               )
      ; VnCt := 0 
      ; <* FATAL ANY *>  
        BEGIN 
          AstView . TraverseChildren ( LVersionNode , CountChild )
        END (* Block *) 
      ; IF VnCt > 0 
        THEN   
          VnResult := NEW ( LdlSemantics . IntListRefTyp , VnCt )  
        ; VnCt := 0 
        ; <* FATAL ANY *>  
          BEGIN 
            AstView . TraverseChildren ( LVersionNode , DoChild )  
          END (* Block *) 
        ; IF VnCt > 4 
          THEN 
            SemError 
              ( LVersionNode . NodeNo 
              , AFT . E_OnlyFourVersionComponentsAllowed 
              ) 
          END (* IF *) 
        END (* IF *) 
      ; RETURN VnResult 
      END (* Block *) 
    END VersionNos 

; PROCEDURE Pass1 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Pass 1: 
       - Find the start and precedence rules and check that there is 
         at most one occurrence of each. 
       - Declare all As identifiers. 
       - Link up all additional occurrences of AsClass identifiers 
       - Declare all VARTERM rules. 
       - Count the VARTERM, AsFixed, AsStar, AsPlus, and AsClass identifiers. 
       - Locate the language name and check that it matches the name 
         at the end. 
       - Traverse all As, Fs and Cs rules, declaring all strings, 
         assigning tokens to strings, and linking up subsequent 
         occurrences of the same string. 
       - Fill in the SemMap for every Ast node. 
  *) 

  = PROCEDURE VisitRule ( RuleNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure } 

    = VAR OptionalChildCt : CARDINAL 

    ; PROCEDURE VisitRhsNode ( Node : AstView . AstRefTyp ) 
      RAISES { AssertionFailure } 

      = VAR LWasFound : BOOLEAN 
      ; VAR LOldDeclNodeNo : LbeStd . EstNodeNoTyp 
      ; VAR LNewSem : LdlSemantics . SemAddlDefStringTyp 

      ; BEGIN (* VisitRhsNode *) 
          IF NOT Node . IsOptSingletonList
          THEN 
            LdlSemantics . MapAstRef ( LangInfo , Node ) 
          ; CASE EstUtil . EstTok ( Node . NodeRef ) 
            OF Ldl1Tok . String 
            => TextIntSymbolTable . FindOrAdd 
                 ( LangInfo . SymbolTable 
                 , SharedStrings . ToText ( Node . NodeRef ) 
                 , Node . NodeNo 
                 , LWasFound 
                 , LOldDeclNodeNo 
                 ) 
            ; IF LWasFound 
              THEN 
                TYPECASE LangInfo . SemMapRef ^ [ LOldDeclNodeNo ] . SemRef 
                OF LdlSemantics . SemFirstOccStringTyp ( TFirstOcc ) 
                => LNewSem 
                     := NEW 
                          ( LdlSemantics . SemAddlDefStringTyp 
                          , NodeNo := Node . NodeNo 
                          , RefDeclId := LOldDeclNodeNo 
                          , RefTok := TFirstOcc . DeclTok 
                          ) 
                ; Assert 
                    ( Node . NodeRef 
                      = LangInfo . SemMapRef ^ [ LOldDeclNodeNo ] . EstRef 
                    , AFT . A_VisitRhsNode_StringNotShared 
                    ) 
                ; LangInfo . SemMapRef ^ [ Node . NodeNo ] . SemRef 
                    := LNewSem 
                (* Link new LdlSemantics . SemAddlDefClassTyp onto the 
                   first-seen LdlSemantics . SemFirstOccClassTyp 
                *) 
                ; LNewSem . NextNodeNo := TFirstOcc . NextNodeNo 
                ; TFirstOcc . NextNodeNo := Node . NodeNo 
                ELSE 
                  CantHappen ( AFT . A_VisitRhsNode_NotString ) 
                END (* TYPECASE *) 
              ELSE 
                LangInfo . SemMapRef ^ [ Node . NodeNo ] . SemRef 
                  := NEW 
                       ( LdlSemantics . SemFirstOccStringTyp 
                       , NodeNo := Node . NodeNo 
                       , DeclKind := DeclKindTyp . String 
                       , DeclRuleNodeNo := RuleNode . NodeNo 
                       , DeclTok := LangInfo . StringToks 
                       , InsertionString 
                           := LdlSemantics . InsertionString 
                                ( Node . NodeRef , LangInfo . StringToks ) 
                       ) 
              ; INC ( LangInfo . StringToks ) 
              ; INC 
                  ( LangInfo . TokCounts ^ [ TokClassConstTerm ] )
              ; INC 
                  ( LangInfo . TokPart ^ [ TokClassConstTerm ] )
              END (* IF *) 
            | Ldl1Tok . AsOptChild 
            => INC ( OptionalChildCt ) 
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END VisitRhsNode 

    ; PROCEDURE VarTermRule ( NameNode : AstView . AstRefTyp ) 
      RAISES { AssertionFailure } 

      = VAR LWasFound : BOOLEAN 
      ; VAR LOldDeclNodeNo : LbeStd . EstNodeNoTyp 

      ; BEGIN (* VarTermRule *) 
          LdlSemantics . MapAstRef ( LangInfo , NameNode ) 
        ; TextIntSymbolTable . FindOrAdd 
            ( LangInfo . SymbolTable 
            , SharedStrings . ToText ( NameNode . NodeRef ) 
            , NameNode . NodeNo 
            , LWasFound 
            , LOldDeclNodeNo 
            ) 
        ; IF LWasFound 
          THEN 
            SemError2 
              ( NameNode . NodeNo , LOldDeclNodeNo , AFT . E_Redeclared ) 
          ELSE 
            LangInfo . SemMapRef ^ [ NameNode . NodeNo ] . SemRef 
              := NEW 
                   ( LdlSemantics . SemDeclTyp 
                   , NodeNo := NameNode . NodeNo 
                   , DeclKind := DeclKindTyp . VarTermRule 
                   , DeclRuleNodeNo := RuleNode . NodeNo 
                   ) 
          ; INC ( LangInfo . VarTermToks ) 
          ; INC ( LangInfo . TokCounts ^ [ TokClassVarTerm ] )
          END (* IF *) 
        END VarTermRule 

    ; PROCEDURE MultiAsNodeDecl 
        ( DeclKind : [ DeclKindTyp . FixedRule .. DeclKindTyp . PlusRule ] 
        ; VAR (* IN OUT *) DeclCt : LbeStd . TokTyp 
        ; TokClass : TokClassTyp 
        ; Parents : AstView . AstRefTyp 
        ) 
      RAISES { AssertionFailure } 

      = PROCEDURE VisitAsNodeDecl ( DeclNode : AstView . AstRefTyp ) 
        RAISES { AssertionFailure } 

        = VAR LWasFound : BOOLEAN 
        ; VAR LOldDeclNodeNo : LbeStd . EstNodeNoTyp 

        ; BEGIN (* VisitAsNodeDecl *) 
            LdlSemantics . MapAstRef ( LangInfo , DeclNode ) 
          ; TextIntSymbolTable . FindOrAdd 
              ( LangInfo . SymbolTable 
              , SharedStrings . ToText ( DeclNode . NodeRef ) 
              , DeclNode . NodeNo 
              , LWasFound 
              , LOldDeclNodeNo 
              ) 
          ; IF LWasFound 
            THEN 
              SemError2 
                ( DeclNode . NodeNo , LOldDeclNodeNo , AFT . E_Redeclared ) 
            ELSIF DeclKind = DeclKindTyp . FixedRule  
            THEN  
              LangInfo . SemMapRef ^ [ DeclNode . NodeNo ] . SemRef 
                := NEW 
                     ( LdlSemantics . SemDeclAsFixedNodeTyp 
                     , NodeNo := DeclNode . NodeNo 
                     , DeclKind := DeclKind 
                     , DeclRuleNodeNo := RuleNode . NodeNo 
                     , OptionalChildCt := OptionalChildCt 
                     ) 
            ; INC ( DeclCt )
            ; INC ( LangInfo . TokCounts ^ [ TokClass ] )  
            ELSE 
              LangInfo . SemMapRef ^ [ DeclNode . NodeNo ] . SemRef 
                := NEW 
                     ( LdlSemantics . SemDeclAsListNodeTyp 
                     , NodeNo := DeclNode . NodeNo 
                     , DeclKind := DeclKind 
                     , DeclRuleNodeNo := RuleNode . NodeNo 
                     ) 
            ; INC ( DeclCt ) 
            ; INC ( LangInfo . TokCounts ^ [ TokClass ] )  
            ; CASE DeclKind <* NOWARN *> 
              OF DeclKindTyp . StarRule 
              => INC ( LangInfo . AsListCardToks 
                     , 3 (* Empty, Singleton, and Plural card tokens. *) 
                     ) 
              ; INC ( LangInfo . TokCounts ^ [ TokClassListCard ] 
                    , 3 (* Empty, Singleton and Plural card tokens. *) 
                    )

              | DeclKindTyp . PlusRule 
              => INC ( LangInfo . AsListCardToks 
                     , 2 (* Singleton and Plural card tokens. *) 
                     ) 
              ; INC ( LangInfo . TokCounts ^ [ TokClassListCard ] 
                    , 2 (* Singleton and Plural card tokens. *) 
                    )
              END (* CASE *) 
            END (* IF *) 
          END VisitAsNodeDecl 

      ; <* FATAL ANY *>  
        BEGIN (* MultiAsNodeDecl *) 
          AstView . TraverseChildren ( Parents , VisitAsNodeDecl ) 
        END MultiAsNodeDecl 

    ; PROCEDURE AsClassDecl 
        ( DeclNode : AstView . AstRefTyp 
        ; VAR (* IN OUT *) DeclCt : LbeStd . TokTyp 
        ; TokClass : TokClassTyp 
        ) 

      = VAR LWasFound : BOOLEAN 
      ; VAR LOldDeclNodeNo : LbeStd . EstNodeNoTyp 
      ; VAR LNewSem : LdlSemantics . SemAddlDefClassTyp 

      ; BEGIN (* AsClassDecl *) 
          TextIntSymbolTable . FindOrAdd 
            ( LangInfo . SymbolTable 
            , SharedStrings . ToText ( DeclNode . NodeRef ) 
            , DeclNode . NodeNo 
            , LWasFound 
            , LOldDeclNodeNo 
            ) 
        ; IF LWasFound 
          THEN 
            TYPECASE LangInfo . SemMapRef ^ [ LOldDeclNodeNo ] . SemRef 
            OF LdlSemantics . SemFirstOccClassTyp ( TFirstOcc ) 
            => LNewSem 
                 := NEW 
                      ( LdlSemantics . SemAddlDefClassTyp 
                      , NodeNo := DeclNode . NodeNo 
                      , RefDeclId := LOldDeclNodeNo 
                      ) 
            ; LangInfo . SemMapRef ^ [ DeclNode . NodeNo ] . SemRef 
                := LNewSem 
            (* Link new LdlSemantics . SemAddlDefClassTyp onto the 
               first-seen LdlSemantics . SemFirstOccClassTyp 
            *) 
(* CHECK: Is there any reason not to do this easily, in reverse order, 
          as we do here? *) 
            ; LNewSem . NextNodeNo := TFirstOcc . NextNodeNo 
            ; TFirstOcc . NextNodeNo := DeclNode . NodeNo 
            ELSE 
              SemError2 
                ( DeclNode . NodeNo , LOldDeclNodeNo , AFT . E_Redeclared ) 
            END (* TYPECASE *) 
          ELSE 
            LangInfo . SemMapRef ^ [ DeclNode . NodeNo ] . SemRef 
              := NEW 
                   ( LdlSemantics . SemFirstOccClassTyp 
                   , NodeNo := DeclNode . NodeNo 
                   , DeclKind := DeclKindTyp . AsClassRule 
                   , DeclRuleNodeNo := RuleNode . NodeNo
                   , FormatsEmpty := UncertainBool . T . Unknown 
                   ) 
          ; INC ( DeclCt ) 
          ; INC ( LangInfo . TokCounts ^ [ TokClass ] )  
          END (* IF *) 
        END AsClassDecl 

    ; BEGIN (* VisitRule *) 
        VAR LChildren : ARRAY [ 0 .. Ldl1Child . CsStarRule_Separators ] 
            OF AstView . AstRefTyp 

      ; BEGIN (* Block *) 
          IF RuleNode . NodeRef # NIL 
          THEN 
            OptionalChildCt := 0  
          ; LdlSemantics . MapAstRef ( LangInfo , RuleNode ) 
          ; AstView . GetChildren 
              ( RuleNode , (* VAR *) LChildren , LangInfo . LdlLang ) 
          ; LdlSemantics . MapAstRefs ( LangInfo , LChildren ) 
          ; CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
            OF Ldl1Tok . StartRule 
            => IF LangInfo . StartRule . NodeRef = NIL 
               THEN 
                 LangInfo . StartRule := RuleNode 
               ELSE 
                 SemError ( RuleNode . NodeNo , AFT . E_MultipleStartRules ) 
               END (* IF *) 
            ; VisitRhsNode ( LChildren [ Ldl1Child . StartRule_StartName ] ) 
            | Ldl1Tok . PrecRule 
            => IF LangInfo . PrecRule . NodeRef = NIL 
               THEN 
                 LangInfo . PrecRule := RuleNode 
               ELSE 
                 SemError 
                   ( RuleNode . NodeNo , AFT . E_MultiplePrecedenceRules ) 
               END (* IF *) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . PrecRule_Levels ] , VisitRhsNode ) 
              END (* Block. *) 
            | Ldl1Tok . AsVarTermRule 
            => VarTermRule ( LChildren [ Ldl1Child . AsVarTermRule_Name ] ) 
            | Ldl1Tok . AsFixedRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . AsFixedRule_Children ] 
                  , VisitRhsNode 
                  ) 
              END (* Block *) 
            ; MultiAsNodeDecl 
                ( DeclKindTyp . FixedRule 
                , LangInfo . AsFixedToks 
                , TokClassAsFixed 
                , LChildren [ Ldl1Child . AsFixedRule_Parents ] 
                ) 
            | Ldl1Tok . AsStarRule 
            => MultiAsNodeDecl 
                 ( DeclKindTyp . StarRule 
                 , LangInfo . AsStarToks 
                 , TokClassAsStar 
                 , LChildren [ Ldl1Child . AsStarRule_Parents ] 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . AsStarRule_ChildName ] 
                  , VisitRhsNode 
                  ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . AsStarRule_ChildClass ] 
                  , VisitRhsNode 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . AsPlusRule 
            => MultiAsNodeDecl 
                 ( DeclKindTyp . PlusRule 
                 , LangInfo . AsPlusToks 
                 , TokClassAsPlus 
                 , LChildren [ Ldl1Child . AsPlusRule_Parents ] 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . AsPlusRule_ChildName ] 
                  , VisitRhsNode 
                  ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . AsPlusRule_ChildClass ] 
                  , VisitRhsNode 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . AsClassRule 
            => AsClassDecl 
                 ( LChildren [ Ldl1Child . AsClassRule_ClassName ] 
                 , LangInfo . AsClassOnlyToks 
                 , TokClassAsClass 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . AsClassRule_ClassMembers ] 
                  , VisitRhsNode 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . AsCsClassRule 
            => AsClassDecl 
                 ( LChildren [ Ldl1Child . AsCsClassRule_ClassName ] 
                 , LangInfo . AsCsClassToks 
                 , TokClassAsCsClass 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . AsCsClassRule_ClassMembers ] 
                  , VisitRhsNode 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . CsAltRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . CsAltRule_Alternatives ] 
                  , VisitRhsNode 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . CsFixedRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . CsFixedRule_Rhs ] , VisitRhsNode ) 
              END (* Block. *) 
            | Ldl1Tok . CsStarRule 
            , Ldl1Tok . CsPlusRule 
            , Ldl1Tok . CsPluralRule 
            , Ldl1Tok . CsStarTrailRule 
            , Ldl1Tok . CsPlusTrailRule 
            , Ldl1Tok . CsPluralTrailRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                   ( LChildren [ Ldl1Child . CsStarRule_ListChild ] 
                   , VisitRhsNode 
                   ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . CsStarRule_Separators ] 
                  , VisitRhsNode 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . FsFixedHorizRule 
            , Ldl1Tok . FsFixedDefaultRule 
            , Ldl1Tok . FsFixedVertRule 
            , Ldl1Tok . FsFixedFillRule 
            , Ldl1Tok . FsInlineRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . FsFixedHorizRule_Children ] 
                  , VisitRhsNode 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . FsListDefaultRule 
            , Ldl1Tok . FsListHorizRule 
            , Ldl1Tok . FsListVertRule 
            , Ldl1Tok . FsListFillRule 
            , Ldl1Tok . FsListTrailDefaultRule 
            , Ldl1Tok . FsListTrailHorizRule 
            , Ldl1Tok . FsListTrailVertRule 
            , Ldl1Tok . FsListTrailFillRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . FsListHorizRule_ListChild ] 
                  , VisitRhsNode 
                  ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . FsListHorizRule_Formatters ] 
                  , VisitRhsNode 
                  ) 
              END (* Block. *) 
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END (* Block *) 
      END VisitRule 

  ; BEGIN (* Pass1 *) 
      VAR LRootChildren 
          : ARRAY [ 0 .. Ldl1Child . LanguageDefinition_ClosingName ] 
            OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        LangInfo . SemMapRef ^ [ LangInfo . Root . NodeNo ] . EstRef 
          := LangInfo . Root . NodeRef 
      ; AstView . GetChildren 
          ( LangInfo . Root , (* VAR *) LRootChildren , LangInfo . LdlLang ) 
      ; LdlSemantics . MapAstRefs ( LangInfo , LRootChildren ) 
      ; LangInfo . LanguageName 
          := LRootChildren [ Ldl1Child . LanguageDefinition_LanguageName ] 
             . NodeRef 
      ; IF SharedStrings . StringNo ( LangInfo . LanguageName ) 
           # SharedStrings . StringNo 
               ( LRootChildren 
                   [ Ldl1Child . LanguageDefinition_ClosingName ] . NodeRef 
               ) 
        THEN 
          SemError 
            ( LRootChildren [ Ldl1Child . LanguageDefinition_ClosingName ] 
              . NodeNo 
            , AFT . E_LanguageClosingNameMismatch 
            ) 
        END (* IF *) 
      ; <* FATAL ANY *> 
        BEGIN 
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl1Child . LanguageDefinition_Rules ] 
            , VisitRule 
            ) 
        END (* Block *) 

      END (* Block *) 
    END Pass1 

; PROCEDURE BetweenPass1And2 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  (* Between passes 1 and 2: 
     - Rebias counts of varterm, abstract, and generated list tokens to prepare
       for assigning actual token values in pass 2.  Varterms get two 
       tokens each. 
     - Create an initialized token map big enough for the 
       string, varterm, abstract, and generated list tokens. 
     - Create an empty ClassRelation of the right size 
       for all abstract tokens. 
     - Create an empty StringMap. 
     - Check that a there was a start rule. 
  *) 

  = VAR LTokCt : LbeStd . TokTyp 

  ; BEGIN (* BetweenPass1And2 *) 
      LangInfo . AsSublistToks 
        := LangInfo . AsPlusToks + LangInfo . AsStarToks 
    ; LangInfo . AsPartialToks 
        := LangInfo . AsPlusToks + LangInfo . AsStarToks 
    (* ^These numbers are just token counts, for use in computing bases. *) 
    ; LTokCt (* Total for string, varterm, sublist, list cardinality,
                partial, and all abstract tokens. *) 
        := LangInfo . StringToks 
           + LangInfo . VarTermToks * 2 
           + LangInfo . AsPlusToks 
           + LangInfo . AsStarToks 
           + LangInfo . AsFixedToks 
           + LangInfo . AsSublistToks 
           + LangInfo . AsListCardToks 
           + LangInfo . AsPartialToks 
           + LangInfo . AsClassOnlyToks 
           + LangInfo . AsCsClassToks 
    ; LangInfo . AsCsClassToks (* Beginning of range for AsCsClassToks *) 
        := LangInfo . StringToks 
           + LangInfo . VarTermToks * 2 
           + LangInfo . AsPlusToks 
           + LangInfo . AsStarToks 
           + LangInfo . AsFixedToks 
           + LangInfo . AsSublistToks 
           + LangInfo . AsListCardToks 
           + LangInfo . AsPartialToks 
           + LangInfo . AsClassOnlyToks 
    ; LangInfo . AsClassOnlyToks (* Beginning of range for AsClassOnlyToks *) 
        := LangInfo . StringToks 
           + LangInfo . VarTermToks * 2 
           + LangInfo . AsPlusToks 
           + LangInfo . AsStarToks 
           + LangInfo . AsFixedToks 
           + LangInfo . AsSublistToks 
           + LangInfo . AsListCardToks 
           + LangInfo . AsPartialToks 
    ; LangInfo . AsPartialToks (* Beginning of range for AsPartialToks *) 
        := LangInfo . StringToks 
           + LangInfo . VarTermToks * 2 
           + LangInfo . AsPlusToks 
           + LangInfo . AsStarToks 
           + LangInfo . AsFixedToks 
           + LangInfo . AsSublistToks 
           + LangInfo . AsListCardToks 
    ; LangInfo . AsListCardToks (* Beginning of range for AsListCardToks *) 
        := LangInfo . StringToks 
           + LangInfo . VarTermToks * 2 
           + LangInfo . AsPlusToks 
           + LangInfo . AsStarToks 
           + LangInfo . AsFixedToks 
           + LangInfo . AsSublistToks 
    ; LangInfo . AsSublistToks (* Beginning of range for AsSublistToks *) 
        := LangInfo . StringToks 
           + LangInfo . VarTermToks * 2 
           + LangInfo . AsPlusToks 
           + LangInfo . AsStarToks 
           + LangInfo . AsFixedToks 
    ; LangInfo . AsFixedToks (* Beginning of range for AsFixedToks *) 
        := LangInfo . StringToks 
           + LangInfo . VarTermToks * 2 
           + LangInfo . AsPlusToks 
           + LangInfo . AsStarToks 
    ; LangInfo . AsStarToks (* Beginning of range for AsStarToks *) 
        := LangInfo . StringToks 
           + LangInfo . VarTermToks * 2 
           + LangInfo . AsPlusToks 
    ; LangInfo . AsPlusToks (* Beginning of range for AsPlusToks *) 
        := LangInfo . StringToks + LangInfo . VarTermToks * 2 
    ; LangInfo . VarTermModToks (* Beginning of range for VarTermModToks *) 
        := LangInfo . StringToks + LangInfo . VarTermToks 
    ; LangInfo . VarTermToks (* Beginning of range for VarTermToks *) 
        := LangInfo . StringToks 

    (* Now change these to biases to be added to a Plus or Star token, to
       get the Sublist/Partial token.
    *) 
    ; DEC ( LangInfo . AsSublistToks , LangInfo . AsPlusToks ) 
    ; DEC ( LangInfo . AsPartialToks , LangInfo . AsPlusToks ) 

    ; LdlSemantics . StackTokCounts1 ( LangInfo ) 

    ; LangInfo . TokMapRef 
        := LdlSemantics . NewTokMap ( LTokCt - LdlSemantics . TokMapBias ) 
    ; LangInfo . ClassRelation 
        := TokRelation . New 
             ( LeftMin := LangInfo . AsClassOnlyToks 
             , LeftMax := LTokCt - 1 
             ) 
    ; LangInfo . StringMapRef 
        := LdlSemantics . NewStringMap 
             ( LTokCt - LdlSemantics . StringMapBias ) 
    ; IF LangInfo . StartRule . NodeRef = NIL 
      THEN 
        SemError ( LangInfo . Root . NodeNo , AFT . E_NoStartRule ) 
      END (* IF *) 
    END BetweenPass1And2 

; PROCEDURE Pass2 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Pass 2: 
       - Assign token numbers to varterms and abstract idents. 
       - Generate Sublist and Partial tokens for abstract list rules.  
       - Generate list cardinality tokens. 
       - Fill in token map entries for varterms, abstract ident token 
         numbers and generated list tokens, (assigned in this pass) 
         and string token numbers (assigned earlier).
       - Fill in StringMap entries for the same things as token map entries. 
       - Copy tokens from first to all subsequent occurrences of 
         an abstract class Lhs. 
  *) 

  = VAR P2VarTermTokCt : CARDINAL 

  ; PROCEDURE VisitRule ( RuleNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure } 

    = PROCEDURE VisitMaybeString ( Node : AstView . AstRefTyp ) 
      RAISES { AssertionFailure } 

      = BEGIN (* VisitMaybeString *) 
          IF NOT Node . IsOptSingletonList
          THEN 
            CASE EstUtil . EstTok ( Node . NodeRef ) 
            OF Ldl1Tok . String 
            => TYPECASE LangInfo . SemMapRef ^ [ Node . NodeNo ] . SemRef 
               OF LdlSemantics . SemFirstOccStringTyp ( TSemRef ) 
               => LangInfo . TokMapRef 
                  ^ [ TSemRef . DeclTok - LdlSemantics . TokMapBias ] 
                    := Node . NodeNo 
               ; LangInfo . StringMapRef 
                 ^ [ TSemRef . DeclTok - LdlSemantics . StringMapBias ] 
                   := TSemRef . InsertionString 
               ELSE 
               END (* TYPECASE *) 
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END VisitMaybeString 

    ; PROCEDURE AsNodeDecl 
        ( DeclNode : AstView . AstRefTyp 
        ; VAR (* IN OUT *) DeclCt : LbeStd . TokTyp 
        ; TokClass : TokClassTyp 
        ) 
      RAISES { AssertionFailure } 
      (* Including VarTerm decls. *) 

      = VAR LVarTermModTok : LbeStd . TokTyp  

      ; BEGIN (* AsNodeDecl *) 
          TYPECASE <* NOWARN *> 
            LangInfo . SemMapRef ^ [ DeclNode . NodeNo ] . SemRef 
          OF NULL 
          => 

          (* AsList node declaration: *) 
          | LdlSemantics . SemDeclAsListNodeTyp ( TDecl ) 
          => 
          (* Assign abstract token. *)  
            TDecl . DeclTok := DeclCt 
          ; LangInfo . TokMapRef ^ [ DeclCt - LdlSemantics . TokMapBias ] 
              := DeclNode . NodeNo 
          ; LangInfo . StringMapRef ^ [ DeclCt - LdlSemantics . StringMapBias ]
              := LdlSemantics . PlaceholderString 
                   ( DeclNode . NodeRef , TDecl . DeclTok ) 
          ; INC ( DeclCt ) 
          ; INC ( LangInfo . TokPart ^ [ TokClass ] )  

          (* Assign sublist token. *) 
          ; TDecl . SublistTok := TDecl . DeclTok + LangInfo . AsSublistToks  
          ; LangInfo . TokMapRef ^ 
              [ TDecl . SublistTok - LdlSemantics . TokMapBias ] 
              := DeclNode . NodeNo 
          ; LangInfo . StringMapRef ^ 
              [ TDecl . SublistTok - LdlSemantics . StringMapBias ] 
              := LdlSemantics . SublistString 
                   ( DeclNode . NodeRef , TDecl . DeclTok ) 

          (* Maybe assign ListCardEmpty token. *) 
          ; IF TDecl . DeclKind = DeclKindTyp . StarRule 
            THEN
              WITH WTok = TDecl . ListCardToks [ ListCardEmpty ] 
              DO 
                WTok := LangInfo . AsListCardToks 
              ; LangInfo . TokMapRef ^ [ WTok - LdlSemantics . TokMapBias ] 
                  := DeclNode . NodeNo 
              ; LangInfo . StringMapRef ^ 
                  [ WTok - LdlSemantics . StringMapBias ] 
                  := LdlSemantics . EmptyListString 
                       ( DeclNode . NodeRef , TDecl . DeclTok ) 
              END (* WITH *) 
            ; INC ( LangInfo . AsListCardToks ) 
            ; INC ( LangInfo . TokPart ^ [ TokClassListCard ] )
            END (* IF *) 

          (* Maybe assign ListCardSingleton token. *) 
          ; IF TDecl . DeclKind IN LdlSemantics . DeclKindSetStarOrPlus  
(* CHECK ^Why not unconditional?  It is already known to be a list rule. *) 
            THEN
              WITH WTok = TDecl . ListCardToks [ ListCardSingleton ] 
              DO 
                WTok := LangInfo . AsListCardToks 
              ; LangInfo . TokMapRef ^ [ WTok - LdlSemantics . TokMapBias ] 
                  := DeclNode . NodeNo 
              ; LangInfo . StringMapRef ^ 
                  [ WTok - LdlSemantics . StringMapBias ] 
                  := LdlSemantics . SingletonListString 
                       ( DeclNode . NodeRef , TDecl . DeclTok ) 
              END (* WITH *) 
            ; INC ( LangInfo . AsListCardToks ) 
            ; INC ( LangInfo . TokPart ^ [ TokClassListCard ] )
            END (* IF *) 

          (* Assign ListCardPlural token. *) 
          ; WITH WTok = TDecl . ListCardToks [ ListCardPlural ] 
            DO 
              WTok := LangInfo . AsListCardToks 
            ; LangInfo . TokMapRef ^ [ WTok - LdlSemantics . TokMapBias ] 
                := DeclNode . NodeNo 
            ; LangInfo . StringMapRef ^ 
                [ WTok - LdlSemantics . StringMapBias ] 
                := LdlSemantics . PluralListString 
                     ( DeclNode . NodeRef , TDecl . DeclTok ) 
            END (* WITH *) 
          ; INC ( LangInfo . AsListCardToks ) 
          ; INC ( LangInfo . TokPart ^ [ TokClassListCard ] )

          (* Assign partial token. *) 
          ; TDecl . PartialTok := TDecl . DeclTok + LangInfo . AsPartialToks  
          ; LangInfo . TokMapRef ^ 
              [ TDecl . PartialTok - LdlSemantics . TokMapBias ] 
              := DeclNode . NodeNo 
          ; LangInfo . StringMapRef ^ 
              [ TDecl . PartialTok - LdlSemantics . StringMapBias ] 
              := LdlSemantics . PartialString 
                   ( DeclNode . NodeRef , TDecl . DeclTok ) 

          (* Other declaration: *) 
          | LdlSemantics . SemDeclTyp ( TDecl ) 
          => TDecl . DeclTok := DeclCt 
          ; LangInfo . TokMapRef ^ [ DeclCt - LdlSemantics . TokMapBias ] 
              := DeclNode . NodeNo 
          ; LangInfo . StringMapRef ^ [ DeclCt - LdlSemantics . StringMapBias ]
              := LdlSemantics . PlaceholderString 
                   ( DeclNode . NodeRef , TDecl . DeclTok )

          (* If a VarTerm, put its ModTokVarTerm into the StringMap. *)  
          ; IF TDecl . DeclKind = DeclKindTyp . VarTermRule 
            THEN
              LVarTermModTok := DeclCt + P2VarTermTokCt
            ; LangInfo . StringMapRef 
                ^ [ LVarTermModTok - LdlSemantics . StringMapBias ]
                := LdlSemantics . ModTokString 
                     ( DeclNode . NodeRef , LVarTermModTok )
            END (* IF *) 
          ; INC ( DeclCt ) 
          ; INC ( LangInfo . TokPart ^ [ TokClass ] )  
          END (* TYPECASE *) 
        END AsNodeDecl 

    ; PROCEDURE MultiAsNodeDecl 
        ( Parents : AstView . AstRefTyp 
        ; VAR (* IN OUT *) DeclCt : LbeStd . TokTyp 
        ; TokClass : TokClassTyp 
        ) 
      RAISES { AssertionFailure } 

      = PROCEDURE VisitAsDecl ( DeclNode : AstView . AstRefTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* VisitAsDecl *) 
            AsNodeDecl ( DeclNode , DeclCt , TokClass ) 
          END VisitAsDecl 

      ; <* FATAL ANY *> 
        BEGIN (* MultiAsNodeDecl *) 
          AstView . TraverseChildren ( Parents , VisitAsDecl ) 
        END MultiAsNodeDecl 

    ; PROCEDURE AsClassRule 
        ( ClassName : AstView . AstRefTyp 
        ; VAR (* IN OUT *) DeclCt : LbeStd . TokTyp 
        ; TokClass : TokClassTyp 
        ) 
      RAISES { AssertionFailure } 

      = BEGIN (* AsClassRule *) 
          TYPECASE LangInfo . SemMapRef ^ [ ClassName . NodeNo ] . SemRef 
          OF LdlSemantics . SemFirstOccClassTyp 
          => AsNodeDecl ( ClassName , DeclCt , TokClass ) 
          | LdlSemantics . SemAddlDefTyp ( TSemDef ) 
          => TSemDef . RefTok := FirstOcc ( LangInfo , TSemDef ) . DeclTok 
          ELSE 
            CantHappen 
              ( AFT . A_Ldl1SemanticsPass2VisitRule_BadClassAttribute ) 
          END (* TYPECASE *) 
        END AsClassRule 

    ; BEGIN (* VisitRule *) 
        VAR LChildren : ARRAY [ 0 .. Ldl1Child . CsStarRule_Separators ] 
            OF AstView . AstRefTyp 

      ; BEGIN (* Block *) 
          IF RuleNode . NodeRef # NIL 
          THEN 
            AstView . GetChildren 
              ( RuleNode , (* VAR *) LChildren , LangInfo . LdlLang ) 
          ; CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
            OF Ldl1Tok . AsFixedRule 
            => MultiAsNodeDecl 
                 ( LChildren [ Ldl1Child . AsFixedRule_Parents ] 
                 , LangInfo . AsFixedToks 
                 , TokClassAsFixed 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                ( LChildren [ Ldl1Child . AsFixedRule_Children ] 
                , VisitMaybeString 
                ) 
              END (* Block. *) 
            | Ldl1Tok . AsVarTermRule 
            => AsNodeDecl 
                 ( LChildren [ Ldl1Child . AsVarTermRule_Name ] 
                 , LangInfo . VarTermToks 
                 , TokClassVarTerm 
                 ) 
            ; INC ( LangInfo . VarTermModToks ) 
            ; INC ( LangInfo . TokPart ^ [ TokClassVarTermMod ] )
              (* Extra token value for a ModTok subtree. *) 
            | Ldl1Tok . AsStarRule 
            => MultiAsNodeDecl 
                 ( LChildren [ Ldl1Child . AsStarRule_Parents ] 
                 , LangInfo . AsStarToks 
                 , TokClassAsStar 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                ( LChildren [ Ldl1Child . AsStarRule_ChildClass ] 
                , VisitMaybeString 
                ) 
              END (* Block. *) 
            | Ldl1Tok . AsPlusRule 
            => MultiAsNodeDecl 
                 ( LChildren [ Ldl1Child . AsPlusRule_Parents ] 
                 , LangInfo . AsPlusToks 
                 , TokClassAsPlus 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . AsPlusRule_ChildClass ] 
                  , VisitMaybeString 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . AsClassRule 
            => AsClassRule 
                  ( LChildren [ Ldl1Child . AsClassRule_ClassName ] 
                  , LangInfo . AsClassOnlyToks 
                  , TokClassAsClass 
                  ) 
            | Ldl1Tok . AsCsClassRule 
            => AsClassRule 
                  ( LChildren [ Ldl1Child . AsClassRule_ClassName ] 
                  , LangInfo . AsCsClassToks 
                  , TokClassAsCsClass 
                  ) 
            | Ldl1Tok . CsAltRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                 ( LChildren [ Ldl1Child . CsAltRule_Alternatives ] 
                 , VisitMaybeString 
                 ) 
              END (* Block. *) 
            | Ldl1Tok . CsFixedRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . CsFixedRule_Rhs ] 
                  , VisitMaybeString 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . CsStarRule 
            , Ldl1Tok . CsPlusRule 
            , Ldl1Tok . CsPluralRule 
            , Ldl1Tok . CsStarTrailRule 
            , Ldl1Tok . CsPlusTrailRule 
            , Ldl1Tok . CsPluralTrailRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . CsStarRule_ListChild ] 
                  , VisitMaybeString 
                  ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . CsStarRule_Separators ] 
                  , VisitMaybeString 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . FsFixedHorizRule 
            , Ldl1Tok . FsFixedDefaultRule 
            , Ldl1Tok . FsFixedVertRule 
            , Ldl1Tok . FsFixedFillRule 
            , Ldl1Tok . FsInlineRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . FsFixedHorizRule_Children ] 
                  , VisitMaybeString 
                  ) 
              END (* Block. *) 
            | Ldl1Tok . FsListDefaultRule 
            , Ldl1Tok . FsListHorizRule 
            , Ldl1Tok . FsListVertRule 
            , Ldl1Tok . FsListFillRule 
            , Ldl1Tok . FsListTrailDefaultRule 
            , Ldl1Tok . FsListTrailHorizRule 
            , Ldl1Tok . FsListTrailVertRule 
            , Ldl1Tok . FsListTrailFillRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . FsListHorizRule_ListChild ] 
                  , VisitMaybeString 
                  ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl1Child . FsListHorizRule_Formatters ] 
                  , VisitMaybeString 
                  ) 
              END (* Block. *) 
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END (* Block *) 
      END VisitRule 

  ; BEGIN (* Pass2 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl1Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        P2VarTermTokCt := LangInfo . VarTermModToks - LangInfo . VarTermToks 
      ; AstView . GetChildren 
          ( LangInfo . Root , (* VAR *) LRootChildren , LangInfo . LdlLang ) 
      ; <* FATAL ANY *> 
        BEGIN 
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl1Child . LanguageDefinition_Rules ] 
            , VisitRule 
            ) 
        END (* Block *) 
      END (* Block *) 
    END Pass2 

; PROCEDURE BetweenPass2And3 
    ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 

  = BEGIN (* BetweenPass2And3 *) 
      WITH WTokPart = LangInfo . TokPart ^ 
      DO
      (* Change these biases back to ends of ranges. *) 
        INC ( LangInfo . AsSublistToks , LangInfo . AsStarToks ) 
      ; INC ( LangInfo . AsPartialToks , LangInfo . AsStarToks ) 
      ; INC ( WTokPart [ TokClassSublist ] , WTokPart [ TokClassAsStar ] ) 
      ; INC ( WTokPart [ TokClassPartial ] , WTokPart [ TokClassAsStar ] ) 
      END (* WITH *) 
    END BetweenPass2And3 

; PROCEDURE Pass3 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Pass 3: 
       - For member idents and strings of class rules, insert their 
         direct containment rules into ClassRelation. 
       - Attach semantic ref nodes to member idents of class rules. 
  *) 

  = PROCEDURE VisitRule ( RuleNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure } 

    = PROCEDURE AsClassRule 
        ( ClassName : AstView . AstRefTyp 
        ; ClassMembers : AstView . AstRefTyp 
        ) 
      RAISES { AssertionFailure } 

      = VAR ClassTok : LbeStd . TokTyp 

      ; PROCEDURE VisitMember ( MemberNode : AstView . AstRefTyp ) 
        RAISES { AssertionFailure } 

        = VAR LWasFound : BOOLEAN 
        ; VAR LDeclNodeNo : LbeStd . EstNodeNoTyp 
        ; VAR LTok : LbeStd . TokTyp 

        ; BEGIN (* VisitMember *) 
            WITH 
              WMemberSemRef 
              = LangInfo . SemMapRef ^ [ MemberNode . NodeNo ] . SemRef 
            DO CASE EstUtil . EstTok ( MemberNode . NodeRef ) 
               OF Ldl1Tok . String 
               => TYPECASE WMemberSemRef 
                  OF LdlSemantics . SemFirstOccStringTyp ( TSemRef ) 
                  => LangInfo . TokMapRef 
                     ^ [ TSemRef . DeclTok - LdlSemantics . TokMapBias ] 
                       := MemberNode . NodeNo 
                  ; LTok := TSemRef . DeclTok 
                  ; LangInfo . StringMapRef ^ 
                      [ TSemRef . DeclTok - LdlSemantics . StringMapBias ] 
                      := TSemRef . InsertionString 
                  | LdlSemantics . SemAddlDefStringTyp ( TSemRef ) 
                  => LTok := TSemRef . RefTok 
                  ELSE 
                    CantHappen ( AFT . A_Pass3VisitMember_BadStringSemRef ) 
                  END (* TYPECASE *) 
               ; TokRelation . AddRule 
                   ( LangInfo . ClassRelation , ClassTok , LTok ) 
               | Ldl1Tok . Ident 
               => TextIntSymbolTable . Find 
                    ( LangInfo . SymbolTable 
                    , SharedStrings . ToText ( MemberNode . NodeRef ) 
                    , LWasFound 
                    , LDeclNodeNo 
                    ) 
               ; IF LWasFound 
                 THEN 
                   TYPECASE LangInfo . SemMapRef ^ [ LDeclNodeNo ] . SemRef 
                   OF LdlSemantics . SemDeclTyp ( TSemDecl ) 
                   => CASE TSemDecl . DeclKind 
                      OF DeclKindTyp . VarTermRule 
                      , DeclKindTyp . String 
                      , DeclKindTyp . AsClassRule 
                      , DeclKindTyp . AsCsClassRule 
                      , DeclKindTyp . FixedRule 
                      , DeclKindTyp . StarRule 
                      , DeclKindTyp . PlusRule 
                      => (* A known varterm, string, or abstract node. *) 
                         WMemberSemRef 
                           := NEW 
                                ( LdlSemantics . SemRefTyp 
                                , NodeNo := MemberNode . NodeNo 
                                , RefDeclId := TSemDecl . NodeNo 
                                , RefTok := TSemDecl . DeclTok 
                                ) 
                      ; TokRelation . AddRule 
                          ( LangInfo . ClassRelation 
                          , ClassTok 
                          , TSemDecl . DeclTok 
                          ) 
                      ELSE 
                        SemError2 
                          ( MemberNode . NodeNo 
                          , LDeclNodeNo 
                          , AFT . E_AbstractClassBadMemberKind 
                          ) 
                      END (* CASE *) 
                   ELSE 
                     CantHappen 
                       ( AFT . A_Ldl1SemanticsPass3VisitRuleAsClassRuleVisitMember_NotDeclKindTyp 
                       ) 
                   END (* TYPECASE *) 
                 ELSE 
                   SemError 
                     ( MemberNode . NodeNo 
                     , AFT . E_AbstractClassUndeclaredMember 
                     ) 
                 END (* IF *) 
               ELSE 
               END (* CASE *) 
            END (* WITH *) 
          END VisitMember 

      ; BEGIN (* AsClassRule *) 
          TYPECASE LangInfo . SemMapRef ^ [ ClassName . NodeNo ] . SemRef 
          OF LdlSemantics . SemFirstOccClassTyp ( TFirstOcc ) 
          => ClassTok := TFirstOcc . DeclTok 
          ; <* FATAL ANY *> 
            BEGIN 
              AstView . TraverseChildren ( ClassMembers , VisitMember ) 
            END (* Block *) 

          | LdlSemantics . SemAddlDefTyp ( TSemDef ) 
          => ClassTok := FirstOcc ( LangInfo , TSemDef ) . DeclTok 
          ; <* FATAL ANY *> 
            BEGIN 
              AstView . TraverseChildren ( ClassMembers , VisitMember ) 
            END (* Block *) 

          ELSE 
            CantHappen 
              ( AFT . A_Ldl1SemanticsPass3VisitRule_BadClassAttribute ) 
          END (* TYPECASE *) 
        END AsClassRule 

    ; BEGIN (* VisitRule *) 
        VAR LChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 

      ; BEGIN (* Block *) 
          IF RuleNode . NodeRef # NIL 
          THEN 
            AstView . GetChildren 
              ( RuleNode , (* VAR *) LChildren , LangInfo . LdlLang ) 
          ; CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
            OF Ldl1Tok . AsClassRule 
            , Ldl1Tok . AsCsClassRule 
            => AsClassRule 
                 ( LChildren [ Ldl1Child . AsClassRule_ClassName ] 
                 , LChildren [ Ldl1Child . AsClassRule_ClassMembers ] 
                 ) 
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END (* Block *) 
      END VisitRule 

  ; BEGIN (* Pass3 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl1Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        AstView . GetChildren 
          ( LangInfo . Root , (* VAR *) LRootChildren , LangInfo . LdlLang ) 
      ; <* FATAL ANY *> 
        BEGIN 
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl1Child . LanguageDefinition_Rules ] 
            , VisitRule 
            ) 
        END (* Block *) 
      END (* Block *) 
    END Pass3 

; PROCEDURE BetweenPass3And4 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Between passes 3 and 4: 
     - Verify token counts assigned. 
     - Compute transitive closure of ClassRelation. 
  *) 

  = BEGIN (* BetweenPass3And4 *) 
      Assert 
        ( LangInfo . AsCsClassToks - LdlSemantics . TokMapBias 
          = NUMBER ( LangInfo . TokMapRef ^ ) 
        , AFT . A_BetweenPass3And4_TokenCountMismatch 
        ) 
      (* It would be better to check each range of tokens separately, 
         but that would require several extra fields. *) 
    ; TokRelation . Close ( LangInfo . ClassRelation ) 
    END BetweenPass3And4 

; VAR DoReportProductions : BOOLEAN := TRUE 

; <* UNUSED *> PROCEDURE StartReportProductions 
    ( Message : TEXT ; ProductionCt : LRTable . ProdNoTyp ) 

  = BEGIN 
      IF DoReportProductions 
      THEN
        Messages . TextOnly 
          ( Message 
          , Kind := MessageCodes . KindTyp . MkInformation 
          ) 
      ; Messages . TextOnly 
          ( "Initial production count: " 
            & Fmt . Pad ( Fmt . Int ( ProductionCt ) , 7 )   
          , Kind := MessageCodes . KindTyp . MkInformation 
          ) 
      END (* IF *) 
    END StartReportProductions 

; <* UNUSED *> PROCEDURE ReportProductions 
    ( RuleNode : AstView . AstRefTyp 
    ; ProductionCt : LRTable . ProdNoTyp 
    ) 

  = BEGIN 
      IF DoReportProductions 
      THEN
        Messages . TextOnly 
          ( "Rule node: " 
            & Fmt . Pad ( Fmt . Int ( RuleNode . NodeNo ) , 7 )   
            & ", production count: " 
            & Fmt . Pad ( Fmt . Int ( ProductionCt ) , 7 )   
          , Kind := MessageCodes . KindTyp . MkInformation 
          ) 
      END (* IF *) 
    END ReportProductions 

; PROCEDURE Pass4 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Pass 4: 
       - Use ClassRelation to compute token sets of As classes and 
         store them in the LdlSemantics . SemFirstOccClassTyp node.  If the 
         class is a singleton, note this. Do all this the first time we 
         encounter either the class rule, or a reference thereto.  This 
         obviates doing it in yet another pass. 
       - Set AltName field of LdlSemantics . SemFirstOccStringTyp of a string 
         to the first singleton class that contains it. 
       - Declare each Cs LHS ident that is not already an abstract name. 
         and count these by kinds, for token assignment later. 
       - Check that a Cs LHS ident that is already an abstract name 
         refers to single abstract node.  Connect such a Cs ident to 
         its matching ident in an As rule. 
       - Count the Cs rules that build each As node. 
       - Link each LHS ident of an Fs rule to its As declaration and 
         vice versa. 
       - Check that there is at most one Fs rule per As node. 
       - For a Child of an abstract node rule, check that it is 
         an abstract node, a string, or a set thereof.  Give it a 
         semantic ref node.  If it is a child of a fixed node, give it
         a child number.   
       - Check the start rule to see that it names an abstract 
         fixed node and set LangInfo . StartIdentNodeNo to its declaration. 
  *) 

  = PROCEDURE LazyComputeTokSet 
      ( FirstOccClass : LdlSemantics . SemFirstOccClassTyp 
        (* PRE: FirstOccClass # NIL *) 
      ) 

    = BEGIN (* LazyComputeTokSet *) 
        IF FirstOccClass # NIL 
        THEN 
          IF FirstOccClass . TokSet = NIL 
          THEN 
            FirstOccClass . TokSet 
              := TokRelation . RightRelatives 
                   ( LangInfo . ClassRelation , FirstOccClass . DeclTok ) 
          ; IF IntSets . IsElement 
                 ( FirstOccClass . DeclTok , FirstOccClass . TokSet ) 
            THEN 
              SemError ( FirstOccClass . NodeNo , AFT . E_CyclicClass ) 
            END (* IF *) 
          ; IF IntSets . Card ( FirstOccClass . TokSet ) = 1 
            THEN 
              FirstOccClass . SingletonTok 
                := IntSets . Minimum ( FirstOccClass . TokSet ) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END LazyComputeTokSet 

  ; PROCEDURE Suffixes 
      ( SuffixesNode : AstView . AstRefTyp ; StartTok : LbeStd . TokTyp ) 
    RAISES { AssertionFailure } 

    = VAR SfCt : PortTypes . Int32Typ 
    ; VAR SfResult : LdlSemantics . SuffixListRefTyp := NIL 

    ; PROCEDURE CountChild ( <* UNUSED *> Node : AstView . AstRefTyp )  

      = BEGIN 
          INC ( SfCt ) 
        END CountChild  

    ; PROCEDURE DoChild ( Node : AstView . AstRefTyp )  
      RAISES { AssertionFailure } 

      = BEGIN 
          SfResult ^ [ SfCt ] 
            := LdlSemantics . SuffixPairTyp
                 { Tok := StartTok 
                 , Suffix 
                     := SharedStrings . ToText 
                          ( LdlSemantics . InsertionString 
                              ( Node . NodeRef , StartTok ) 
(* TODO: Decide what token this really should be.  Is it redundant with
         the Tok of the suffix pair?  
*) 
                          )  
                 } 
        ; INC ( SfCt ) 
        END DoChild 

    ; BEGIN (* Suffixes *) 
        SfCt := 0 
      ; <* FATAL ANY *> 
        BEGIN 
          AstView . TraverseChildren ( SuffixesNode , CountChild )
        END (* Block *) 
      ; IF SfCt > 0 
        THEN   
          SfResult := NEW ( LdlSemantics . SuffixListRefTyp , SfCt )  
        ; SfCt := 0 
        ; <* FATAL ANY *> 
          BEGIN 
            AstView . TraverseChildren ( SuffixesNode , DoChild )  
          END (* Block *) 
        END (* IF *) 
      ; LangInfo . Suffixes := SfResult 
      END Suffixes 

  ; PROCEDURE StartRule 
      ( StartIdNode : AstView . AstRefTyp 
      ; SuffixesNode : AstView . AstRefTyp 
      ) 
    RAISES { AssertionFailure } 

    = VAR LSemDecl : LdlSemantics . SemDeclTyp 
    ; VAR LDeclNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LWasFound : BOOLEAN 

    ; BEGIN (* StartRule *) 
        TextIntSymbolTable . Find 
          ( LangInfo . SymbolTable 
          , SharedStrings . ToText ( StartIdNode . NodeRef ) 
          , LWasFound 
          , LDeclNodeNo 
          ) 
      ; IF LWasFound 
        THEN 
          WITH WDeclSemRef = LangInfo . SemMapRef ^ [ LDeclNodeNo ] . SemRef 
          DO TYPECASE WDeclSemRef 
             OF LdlSemantics . SemDeclAsNodeTyp ( TSemDeclAsNode ) 
             => (* A known abstract node. *)
                LSemDecl := TSemDeclAsNode 
 
             | LdlSemantics . SemFirstOccClassTyp ( TFirstOccClass ) 
             => (* A known class. *) 
               LazyComputeTokSet ( TFirstOccClass ) 
             ; LSemDecl := TFirstOccClass 
             ELSE 
               SemError 
                 ( StartIdNode . NodeNo 
                 , AFT . E_StartSymbolNotAnAbstractOrStringToken 
                 ) 
             ; LSemDecl := NIL 
             END (* TYPECASE *) 
          ; LangInfo . StartIdentNodeNo := LDeclNodeNo 
          ; IF LSemDecl # NIL 
            THEN 
              LangInfo . SemMapRef ^ [ StartIdNode . NodeNo ] . SemRef 
                := NEW 
                     ( LdlSemantics . SemRefTyp 
                     , NodeNo := StartIdNode . NodeNo 
                     , RefDeclId := LDeclNodeNo 
                     , RefTok := LSemDecl . DeclTok 
                     ) 
            ; LangInfo . StartTok := LSemDecl . DeclTok 
            END (* IF *) 
          ; Suffixes ( SuffixesNode , LangInfo . StartTok ) 
          END (* WITH *) 
        ELSE 
          SemError ( StartIdNode . NodeNo , AFT . E_UndeclaredStartSymbol ) 
        END (* IF *) 
      END StartRule 

  ; PROCEDURE VisitRule ( RuleNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure } 

    = VAR EstChildNo : LbeStd . EstChildNoTyp 

    ; PROCEDURE AsChildClass ( ChildClassNode : AstView . AstRefTyp ) 
      RAISES { AssertionFailure } 

      = PROCEDURE VisitSetElement ( Tok : IntSets . ValidElemT ) 

        = BEGIN (* VisitSetElement *) 
            IF Tok >= LangInfo . AsCsClassToks 
            THEN 
              SemError2 
                ( ChildClassNode . NodeNo 
                , LangInfo . TokMapRef ^ [ Tok - LdlSemantics . TokMapBias ] 
                , AFT . E_AbstractChildContainsAnElementNotAnAbstractOrStringToken 
                ) 
            END (* IF *) 
          END VisitSetElement 

      ; BEGIN (* AsChildClass *) 
          VAR LWasFound : BOOLEAN 
        ; VAR LDeclNodeNo : LbeStd . EstNodeNoTyp 

        ; BEGIN (* Block *) 
            CASE EstUtil . EstTok ( ChildClassNode . NodeRef ) <* NOWARN *>
            OF Ldl1Tok . Alternation 
            => SemError 
                 ( ChildClassNode . NodeNo 
                 , AFT . E_UnimplementedAnonymousClass 
                 ) 
            | Ldl1Tok . Ident , Ldl1Tok . Integer 
            => TextIntSymbolTable . FindOrAdd 
                 ( LangInfo . SymbolTable 
                 , SharedStrings . ToText ( ChildClassNode . NodeRef ) 
                 , ChildClassNode . NodeNo 
                 , LWasFound 
                 , LDeclNodeNo 
                 ) 
             ; IF LWasFound 
               THEN 
                 WITH 
                   WDeclSemRef 
                     = LangInfo . SemMapRef ^ [ LDeclNodeNo ] . SemRef 
                 DO TYPECASE WDeclSemRef 
                    OF NULL => 
                    | LdlSemantics . SemFirstOccClassTyp ( TFirstOccClass ) 
                    => (* A known class. *) 
                      LazyComputeTokSet ( TFirstOccClass ) 
                    ; IF TFirstOccClass . SingletonTok = LbeStd . Tok__Null 
                      THEN (* Not a singleton class. *) 
                        <* FATAL ANY *> 
                        BEGIN 
                          IntSets . ForAllDo 
                            ( TFirstOccClass . TokSet , VisitSetElement ) 
                        END (* Block *) 
                      ; LangInfo . SemMapRef ^ [ ChildClassNode . NodeNo ] 
                        . SemRef 
                          := NEW 
                               ( LdlSemantics . SemChildClassRefTyp 
                               , NodeNo := ChildClassNode . NodeNo 
                               , RefDeclId := LDeclNodeNo 
                               , RefTok := TFirstOccClass . DeclTok 
                               , ChildNo := EstChildNo 
                               ) 
                      ELSE (* A singleton class. *) 
                        LangInfo . SemMapRef ^ [ ChildClassNode . NodeNo ] 
                        . SemRef 
                          := NEW 
                               ( LdlSemantics . SemChildClassRefTyp 
                               , NodeNo := ChildClassNode . NodeNo 
                               , RefDeclId 
                                   := LangInfo . TokMapRef ^ 
                                        [ TFirstOccClass . SingletonTok 
                                          - LdlSemantics . TokMapBias 
                                        ] 
                               , RefTok := TFirstOccClass . SingletonTok 
                               , ChildNo := EstChildNo 
                               ) 
                      END (* IF *) 
                    | LdlSemantics . SemDeclTyp ( TDeclSemRef ) 
                    => CASE TDeclSemRef . DeclKind 
                       OF DeclKindTyp . VarTermRule 
                       , DeclKindTyp . FixedRule 
                       , DeclKindTyp . StarRule 
                       , DeclKindTyp . PlusRule 
                       => (* A known abstract node or varterm. *) 
                          LangInfo . SemMapRef ^ [ ChildClassNode . NodeNo ] 
                          . SemRef 
                            := NEW 
                                 ( LdlSemantics . SemChildClassRefTyp 
                                 , NodeNo := ChildClassNode . NodeNo 
                                 , RefDeclId := LDeclNodeNo 
                                 , RefTok := TDeclSemRef . DeclTok 
                                 , ChildNo := EstChildNo 
                                 ) 
                       ELSE 
                         SemError 
                           ( ChildClassNode . NodeNo 
                           , AFT . E_AbstractChildNotAnAbstractOrStringToken 
                           ) 
                       END (* CASE *) 
                    ELSE 
                      SemError 
                        ( ChildClassNode . NodeNo 
                        , AFT . E_AbstractChildNotAnAbstractOrStringToken 
                        ) 
                    END (* TYPECASE *) 
                 END (* WITH *) 
               ELSE 
                 SemError 
                   ( ChildClassNode . NodeNo , AFT . E_UndeclaredAbstractChild ) 
               END (* IF *) 
            END (* CASE *) 
          ; INC ( EstChildNo ) 
          END (* Block *) 
        END AsChildClass 

    ; PROCEDURE AsChild ( ChildNode : AstView . AstRefTyp ) 
      RAISES { AssertionFailure } 

      = VAR LChildren : ARRAY [ 0 .. 1 ] OF AstView . AstRefTyp 

      ; BEGIN (* AsChild *) 
          AstView . GetChildren 
            ( ChildNode , (* VAR *) LChildren , LangInfo . LdlLang ) 
        ; CASE EstUtil . EstTok ( ChildNode . NodeRef ) 
          OF Ldl1Tok . AsReqdChild , Ldl1Tok . AsOptChild 
          => AsChildClass ( LChildren [ Ldl1Child . AsReqdChild_ChildClass ] ) 
(* TODO: Check that all ChildName fields are distinct w/in one AsFixedRule. *) 
          ELSE 
            CantHappen ( AFT . A_Pass4VisitRuleAsChild_BadEstTok ) 
          END (* CASE *) 
        END AsChild 

    ; PROCEDURE AsClassRule ( ClassName : AstView . AstRefTyp ) 
      RAISES { AssertionFailure } 

      = VAR LSet : IntSets . T 

      ; BEGIN (* AsClassRule *) 
          TYPECASE LangInfo . SemMapRef ^ [ ClassName . NodeNo ] . SemRef 
          OF LdlSemantics . SemFirstOccClassTyp ( TFirstOcc ) 
          => LazyComputeTokSet ( TFirstOcc ) 
          ; LSet := IntSets . Project 
                      ( TFirstOcc . TokSet 
                      , LbeStd . Tok__BegOfImage 
                      , LangUtilLo . TokClassLastTok 
                          ( LangInfo , TokClassAsFixed )
                      ) 
          ; INC ( LangInfo . Productions , IntSets . Card ( LSet ) ) 
          ; IF LbeStd . Tok__FirstLangDep <= TFirstOcc . SingletonTok 
               AND TFirstOcc . SingletonTok < LangInfo . StringToks 
            THEN (* Which IMPLIES TFirstOcc . SingletonTok 
                                  # LbeStd . EstTok__Null *) 
              TYPECASE 
                LdlSemantics . SemDeclOfTok 
                  ( LangInfo , TFirstOcc . SingletonTok )
              OF LdlSemantics . SemFirstOccStringTyp ( TSemString ) 
              => IF TSemString . AltName = LbeStd . EstNodeNoNull 
                 THEN 
                   TSemString . AltName := ClassName . NodeNo 
                 END (* IF *) 
              ELSE 
                CantHappen ( AFT . A_Pass4AsClassRule_StringNotString ) 
              END (* TYPECASE *) 
            END (* IF *) 
          ELSE 
          END (* TYPECASE *) 
        END AsClassRule 

    ; PROCEDURE CsRule 
        ( DeclKind : [ DeclKindTyp . CsFixedRule .. DeclKindTyp . CsAltRule ]
        ; VAR (* IN OUT *) DeclCt : LbeStd . TokTyp 
        ; TokClass : TokClassTyp 
        ; IdNode : AstView . AstRefTyp 
        ; BuildNode : AstView . AstRefTyp 
        ; RhsNode : AstView . AstRefTyp := AstView . AstRefNull 
          (* Meaningful only if DeclKind = DeclKindTyp . CsFixedRule *) 
        ) 
      RAISES { AssertionFailure } 

      = PROCEDURE TraverseRhs 
          ( VAR OptionCt : LRTable . OptionIdTyp 
          ; VAR RhsCt : LbeStd . TokNoTyp 
          ) 
        RAISES { AssertionFailure } 

        = PROCEDURE VisitCsFixedDescendent ( Node : AstView . AstRefTyp ) 
          RAISES { AssertionFailure } 

          = BEGIN (* VisitCsFixedDescendent *) 
              IF NOT Node . IsOptSingletonList
              THEN 
                CASE EstUtil . EstTok ( Node . NodeRef ) 
                OF Ldl1Tok . Concatenation 
                => IF OptionCt - 1 >= LAST ( LRTable . OptionIdRealTyp ) 
                   THEN 
                     SemError2 
                       ( RuleNode . NodeNo 
                       , Node . NodeNo 
                       , AFT . E_TooManyOptionalSubconstructsInConcreteRule 
                       ) 
                   ELSE 
                     INC ( OptionCt ) 
                   END (* IF *) 
                | Ldl1Tok . Nil 
                => SemError2 
                     ( RuleNode . NodeNo 
                     , Node . NodeNo 
                     , AFT . E_UnimplementedNilInConcreteRule  
                     ) 
                ELSE 
                  INC ( RhsCt ) 
                END (* CASE *) 
              END (* IF *) 
            END VisitCsFixedDescendent 

        ; BEGIN (* TraverseRhs *) 
(* REVIEW: This can't be called twice for a rule, because it increments
           LangInfo . Productions, but if there is a BUILD, there may be
           two semantic nodes, each to have OptionCt and RhsCt filled in.
           Surely, there is a cleaner way.
*) 
            IF DeclKind = DeclKindTyp . CsFixedRule 
            THEN 
              OptionCt := 0 
            ; RhsCt := 0 
            ; <* FATAL ANY *> 
              BEGIN 
                AstView . TraverseTree ( RhsNode , VisitCsFixedDescendent ) 
              END (* Block *) 
            ; OptionCt := MAX ( 0 , OptionCt - 1 ) 
              (* This is crude, but sure easier to code.  The fixed 
                 rule usually has one occurrence of a Concatenation node, at 
                 the top level, which is not an optional substring. 
                 All others inside are.  So we count them all and decrease 
                 for the top one. *) 
            ; INC ( LangInfo . Productions , Word . Shift ( 1 , OptionCt ) ) 
            ELSE 
              OptionCt := 0 
            ; RhsCt := 1 
            END (* IF *) 
          END TraverseRhs 

      ; BEGIN (* CsRule *) 
          VAR LWasFound : BOOLEAN 
        ; VAR LOldDeclNodeNo : LbeStd . EstNodeNoTyp 
        ; VAR LNewSemBuild : LdlSemantics . SemAddlDefCsTyp 
        ; VAR LNewSemAddlDefCs : LdlSemantics . SemAddlDefCsTyp 
        ; VAR LNewSemFirstOccCs : LdlSemantics . SemFirstOccCsTyp 

        ; BEGIN (* Block for CsRule *) 
            LNewSemBuild := NIL 
          ; IF BuildNode . NodeRef # NIL 
            THEN 
              TextIntSymbolTable . FindOrAdd 
                ( LangInfo . SymbolTable 
                , SharedStrings . ToText ( BuildNode . NodeRef ) 
                , BuildNode . NodeNo 
                , LWasFound 
                , LOldDeclNodeNo 
                ) 
            ; IF LWasFound 
              THEN 
                WITH 
                  WOldSemRef 
                  = LangInfo . SemMapRef ^ [ LOldDeclNodeNo ] . SemRef 
                DO TYPECASE WOldSemRef 
                   OF NULL 
                   => SemError2 
                       ( BuildNode . NodeNo 
                       , LOldDeclNodeNo 
                       , AFT . E_CS_rule_BUILD_does_not_name_an_abstract_node 
                       ) 
                   | LdlSemantics . SemDeclAsNodeTyp ( TOldSemRef ) 
                   => (* Build a known abstract node. *) 
                      IF NOT CsDeclKindMatchesAsDeclKind 
                               ( DeclKind , TOldSemRef . DeclKind ) 
                      THEN 
                        SemError2 
                          ( BuildNode . NodeNo 
                          , LOldDeclNodeNo 
                          , AFT . E_CS_rule_wrong_build_kind_for_AS_node 
                          ) 
                      END (* IF *) 
                   ; INC ( TOldSemRef . CsRuleCt ) 
                   ; LNewSemBuild 
                       := NEW 
                            ( LdlSemantics . SemAddlDefCsTyp 
                            , NodeNo := BuildNode . NodeNo 
                            , RefDeclId := LOldDeclNodeNo 
                            , RefTok := TOldSemRef . DeclTok 
                            ) 
                   ; LangInfo . SemMapRef ^ [ BuildNode . NodeNo ] . SemRef 
                       := LNewSemBuild 
                   ; TraverseRhs 
                       ( LNewSemBuild . OptionCt , LNewSemBuild . RhsCt ) 

                   ELSE 
                     SemError2 
                       ( BuildNode . NodeNo 
                       , LOldDeclNodeNo 
                       , AFT . E_CS_rule_BUILD_does_not_name_an_abstract_node 
                       ) 
                   END (* TYPECASE *) 
                END (* WITH *) 
              ELSE 
                SemError
                  ( BuildNode . NodeNo
                  , AFT . E_Undeclared_build_node
                  ) 
              END (* IF *) 
            END (* IF *) 

          ; TextIntSymbolTable . FindOrAdd 
              ( LangInfo . SymbolTable 
              , SharedStrings . ToText ( IdNode . NodeRef ) 
              , IdNode . NodeNo 
              , LWasFound 
              , LOldDeclNodeNo 
              ) 
          ; IF LWasFound 
            THEN 
              WITH 
                WOldSemRef 
                = LangInfo . SemMapRef ^ [ LOldDeclNodeNo ] . SemRef 
              DO TYPECASE WOldSemRef 
                 OF LdlSemantics . SemDeclAsNodeTyp ( TOldSemRef ) 
                 => (* A rule for a known abstract node. *)
                   LNewSemAddlDefCs 
                     := NEW 
                          ( LdlSemantics . SemAddlDefCsTyp 
                          , NodeNo := IdNode . NodeNo 
                          , RefDeclId := LOldDeclNodeNo 
                          , RefTok := TOldSemRef . DeclTok 
                          ) 
                 ; LangInfo . SemMapRef ^ [ IdNode . NodeNo ] . SemRef 
                     := LNewSemAddlDefCs 
                 ; IF LNewSemBuild = NIL 
                   THEN  
                     IF NOT CsDeclKindMatchesAsDeclKind 
                              ( DeclKind , TOldSemRef . DeclKind ) 
                     THEN 
                       SemError2 
                         ( IdNode . NodeNo 
                         , LOldDeclNodeNo 
                         , AFT . E_CsRuleWrongKindForAsNode 
                         ) 
                     END (* IF *) 
                   ; INC ( TOldSemRef . CsRuleCt ) 
                   ; TraverseRhs 
                       ( LNewSemAddlDefCs . OptionCt 
                       , LNewSemAddlDefCs . RhsCt 
                       ) 
                   ELSE 
(* CHECK: is this legal? *) 
                     LNewSemAddlDefCs . OptionCt := LNewSemBuild . OptionCt   
                   ; LNewSemAddlDefCs . RhsCt := LNewSemBuild . RhsCt   
                   END (* IF *) 
                 | LdlSemantics . SemFirstOccClassTyp ( TFirstOccClass ) 
                 => LazyComputeTokSet ( TFirstOccClass ) 
                 ; LNewSemAddlDefCs 
                     := NEW 
                          ( LdlSemantics . SemAddlDefCsTyp 
                          , NodeNo := IdNode . NodeNo 
                          , RefDeclId := LOldDeclNodeNo 
                          , RefTok := TFirstOccClass . DeclTok 
                          ) 
                 ; LangInfo . SemMapRef ^ [ IdNode . NodeNo ] . SemRef 
                     := LNewSemAddlDefCs 
                 ; LNewSemAddlDefCs . NextNodeNo 
                     := TFirstOccClass . NextNodeNo 
                 ; TFirstOccClass . NextNodeNo := IdNode . NodeNo 
                 ; IF LNewSemBuild = NIL 
                   THEN 
                     TraverseRhs 
                       ( LNewSemAddlDefCs . OptionCt 
                       , LNewSemAddlDefCs . RhsCt 
                       ) 
                   ELSE 
                     LNewSemAddlDefCs . OptionCt := LNewSemBuild . OptionCt   
                   ; LNewSemAddlDefCs . RhsCt := LNewSemBuild . RhsCt   
                   END (* IF*) 
(* CHECK: Is this the way we really want the semantics? It means a 
          Cs rule naming an abstract class, even a singleton, is 
          not an ast building rule.  This seems to be needed to allow 
          e.g., directly nested concatenations in the As, while having 
          an unambiguous parsing syntax.  I suppose we could go back 
          to the code below and just use a Cs token not declared as a 
          class. *) 
(*               ; IF TFirstOccClass . SingletonTok = LbeStd . Tok__Null 
                   THEN 
                     SemError2 
                       ( IdNode . NodeNo 
                       , TFirstOccClass . NodeNo 
                       , AFT . E_ConcreteBuildingRuleForPluralClass 
                       ) 
                   ELSE (* Another kind of building rule for a known 
                           abstract node. *) 
                     LDeclIdNodeNo 
                       := LangInfo . TokMapRef ^ 
                            [ TFirstOccClass . SingletonTok 
                              - LdlSemantics . TokMapBias 
                            ] 
                   ; TYPECASE 
                       LangInfo . SemMapRef ^ [ LDeclIdNodeNo ] . SemRef 
                     OF NULL => 
                     | LdlSemantics . SemDeclAsNodeTyp ( TOldSemRef ) 
                     => (* The singleton class is for a known abstract node. *) 
                        IF NOT CsDeclKindMatchesAsDeclKind 
                            ( DeclKind , TOldSemRef . DeclKind ) 
                        THEN 
                           SemError2 
                             ( IdNode . NodeNo 
                             , LDeclIdNodeNo 
                             , AFT . E_CsRuleWrongKindForAsNode 
                             ) 
                        END 
                      ; LangInfo . SemMapRef ^ [ IdNode . NodeNo ] . SemRef 
                          := NEW 
                               ( LdlSemantics . SemRefTyp 
                               , NodeNo := IdNode . NodeNo 
                               , RefDeclId := LDeclIdNodeNo 
                               , RefTok := TFirstOccClass . SingletonTok 
                               ) 
                     ; INC ( TOldSemRef . CsRuleCt ) 
                     ELSE 
                       SemError2 
                         ( IdNode . NodeNo 
                         , LOldDeclNodeNo 
                         , AFT . E_ConcreteIdentAlreadyDeclaredButNotAbstract 
                         ) 
                     END (* TYPECASE *) 
                   END (* IF *) 
*) 
                 | LdlSemantics . SemFirstOccCsTyp ( TFirstOccCs ) 
                 => (* A subsequent occurrence of a concrete nonterminal 
                       that is not an abstract node. *) 
                    LNewSemAddlDefCs 
                      := NEW 
                           ( LdlSemantics . SemAddlDefCsTyp 
                           , NodeNo := IdNode . NodeNo 
                           , RefDeclId := LOldDeclNodeNo 
                           , RefTok := TFirstOccCs . DeclTok 
                           ) 
                 ; LangInfo . SemMapRef ^ [ IdNode . NodeNo ] . SemRef 
                     := LNewSemAddlDefCs 
                 ; LNewSemAddlDefCs . NextNodeNo := TFirstOccCs . NextNodeNo 
                 ; TFirstOccCs . NextNodeNo := IdNode . NodeNo 
                 ; IF LNewSemBuild = NIL 
                   THEN  
                     TraverseRhs 
                       ( LNewSemAddlDefCs . OptionCt , LNewSemAddlDefCs . RhsCt )
                   ELSE 
                     LNewSemAddlDefCs . OptionCt := LNewSemBuild . OptionCt   
                   ; LNewSemAddlDefCs . RhsCt := LNewSemBuild . RhsCt   
                   END (* IF *) 
                 ELSE 
                   SemError2 
                     ( IdNode . NodeNo 
                     , LOldDeclNodeNo 
                     , AFT . E_ConcreteIdentAlreadyDeclaredAndNotAbstract 
                     ) 
                 END (* TYPECASE *) 
              END (* WITH *) 
            ELSE (* This is a new Cs nonterminal *) 
              LNewSemFirstOccCs 
                := NEW 
                     ( LdlSemantics . SemFirstOccCsTyp 
                     , NodeNo := IdNode . NodeNo 
                     , DeclKind := DeclKind 
                     , DeclRuleNodeNo := RuleNode . NodeNo 
                     ) 
            ; LangInfo . SemMapRef ^ [ IdNode . NodeNo ] . SemRef 
                := LNewSemFirstOccCs 
            ; IF LNewSemBuild = NIL 
              THEN  
                TraverseRhs 
                  ( LNewSemFirstOccCs . OptionCt , LNewSemFirstOccCs . RhsCt ) 
              ELSE 
                LNewSemFirstOccCs . OptionCt := LNewSemBuild . OptionCt   
              ; LNewSemFirstOccCs . RhsCt := LNewSemBuild . RhsCt   
              END (* IF *) 
            ; INC ( DeclCt ) 
            ; INC ( LangInfo . TokCounts ^ [ TokClass ] )  
            END (* IF *) 
          END (* Block *) 
        END CsRule 

    ; PROCEDURE VisitCsAlternative ( <* UNUSED *> Node : AstView . AstRefTyp ) 

      = BEGIN (* VisitCsAlternative *) 
          INC ( LangInfo . Productions ) 
        END VisitCsAlternative 

    ; PROCEDURE VisitFsLhs ( RefNode : AstView . AstRefTyp ) 

      = VAR DeclNodeNo : LbeStd . EstNodeNoTyp 

      ; PROCEDURE LinkFsRule 
          ( SemDeclAsNode : LdlSemantics . SemDeclAsNodeTyp ) 

        = BEGIN (* LinkFsRule *) 
            IF SemDeclAsNode . FsIdentNodeNo = LbeStd . EstNodeNoNull 
            THEN 
              LangInfo . SemMapRef ^ [ RefNode . NodeNo ] . SemRef 
                := NEW 
                     ( LdlSemantics . SemFsRefTyp 
                     , NodeNo := RefNode . NodeNo 
                     , RefDeclId := SemDeclAsNode . NodeNo 
                     , RefTok := SemDeclAsNode . DeclTok 
                     , RefRuleNodeNo := RuleNode . NodeNo 
                     ) 
            ; SemDeclAsNode . FsIdentNodeNo := RefNode . NodeNo 
            ELSE 
              SemError2 
                ( RefNode . NodeNo , DeclNodeNo , AFT . E_DuplicateFsRule ) 
            END (* IF *) 
          END LinkFsRule 

      ; BEGIN (* VisitFsLhs *) 
          VAR LWasFound : BOOLEAN 
        ; VAR LDeclSemRef : LdlSemantics . SemTyp 

        ; BEGIN (* Block *) 
            TextIntSymbolTable . Find 
              ( LangInfo . SymbolTable 
              , SharedStrings . ToText ( RefNode . NodeRef ) 
              , LWasFound 
              , DeclNodeNo 
              ) 
          ; IF LWasFound 
            THEN 
              WITH WDeclSemRef = LangInfo . SemMapRef ^ [ DeclNodeNo ] . SemRef 
              DO TYPECASE WDeclSemRef 
                 OF LdlSemantics . SemDeclAsNodeTyp ( TAsNodeSemRef ) 
                 => (* An Fs rule for a known abstract node. *) 
                    LinkFsRule ( TAsNodeSemRef ) 
                 | LdlSemantics . SemFirstOccClassTyp ( TFirstOccClass ) 
                 => LazyComputeTokSet ( TFirstOccClass ) 
                 ; IF TFirstOccClass . SingletonTok = LbeStd . Tok__Null 
                   THEN 
                     SemError2 
                       ( RefNode . NodeNo 
                       , TFirstOccClass . NodeNo 
                       , AFT . E_FormatRuleForPluralClass 
                       ) 
                   ELSE (* Indirect, through singleton class. *) 
                     LDeclSemRef 
                       := LdlSemantics . SemDeclOfTok 
                            ( LangInfo , TFirstOccClass . SingletonTok ) 
                   ; TYPECASE LDeclSemRef 
                     OF NULL => 
                     | LdlSemantics . SemDeclAsNodeTyp ( TAsNodeSemRef ) 
                     => (* An Fs rule for a known abstract node. *) 
                        LinkFsRule ( TAsNodeSemRef ) 
                     ELSE 
                       SemError2 
                         ( RefNode . NodeNo 
                         , DeclNodeNo 
                         , AFT . E_FormatRuleForNonAsNode 
                         ) 
                     END (* TYPECASE *) 
                   END (* IF *) 
                 ELSE 
                   SemError2 
                     ( RefNode . NodeNo 
                     , DeclNodeNo 
                     , AFT . E_FormatRuleForNonAsNode 
                     ) 
                 END (* TYPECASE *) 
              END (* WITH *) 
            ELSE 
              SemError 
                ( RefNode . NodeNo , AFT . E_FormatRuleForUndeclaredSymbol ) 
            END (* IF *) 
          END (* Block *) 
        END VisitFsLhs 

    ; BEGIN (* VisitRule *) 
        VAR LChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 

      ; BEGIN (* Block *) 
          IF RuleNode . NodeRef # NIL 
          THEN 
            AstView . GetChildren 
              ( RuleNode , (* VAR *) LChildren , LangInfo . LdlLang ) 
          ; CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
            OF Ldl1Tok . StartRule 
            => StartRule 
                 ( LChildren [ Ldl1Child . StartRule_StartName ] 
                 , LChildren [ Ldl1Child . StartRule_Suffixes ] 
                 ) 
            | Ldl1Tok . AsFixedRule 
            => EstChildNo := 0 
            ; <* FATAL ANY *> 
              BEGIN 
                AstView . TraverseChildren 
                  ( LChildren [ Ldl1Child . AsFixedRule_Children ] 
                  , Visit := AsChild 
                  ) 
              END (* Block *) 
              
            | Ldl1Tok . AsStarRule 
            => EstChildNo := 0 
            ; AsChildClass ( LChildren [ Ldl1Child . AsStarRule_ChildClass ] ) 

            | Ldl1Tok . AsPlusRule 
            => EstChildNo := 0 
            ; AsChildClass ( LChildren [ Ldl1Child . AsPlusRule_ChildClass ] ) 

            | Ldl1Tok . AsClassRule 
            , Ldl1Tok . AsCsClassRule 
            => AsClassRule ( LChildren [ Ldl1Child . AsClassRule_ClassName ] ) 
            | Ldl1Tok . CsFixedRule 
            => CsRule 
                 ( DeclKindTyp . CsFixedRule 
                 , LangInfo . CsFixedToks 
                 , TokClassCsFixed 
                 , LChildren [ Ldl1Child . CsFixedRule_Lhs ] 
                 , LChildren [ Ldl1Child . CsFixedRule_Build ] 
                 , LChildren [ Ldl1Child . CsFixedRule_Rhs ] 
                 ) 
            | Ldl1Tok . CsStarRule 
            , Ldl1Tok . CsStarTrailRule 
            => CsRule 
                 ( DeclKindTyp . CsStarRule 
                 , LangInfo . CsStarToks 
                 , TokClassCsStar 
                 , LChildren [ Ldl1Child . CsStarRule_Lhs ] 
                 , LChildren [ Ldl1Child . CsStarRule_Build ] 
                 ) 
            ; INC ( LangInfo . Productions , 4 ) 
            | Ldl1Tok . CsPlusRule 
            , Ldl1Tok . CsPlusTrailRule 
            => CsRule 
                 ( DeclKindTyp . CsPlusRule 
                 , LangInfo . CsPlusToks 
                 , TokClassCsPlus  
                 , LChildren [ Ldl1Child . CsPlusRule_Lhs ] 
                 , LChildren [ Ldl1Child . CsPlusRule_Build ] 
                 ) 
            ; INC ( LangInfo . Productions , 3 ) 
            | Ldl1Tok . CsPluralRule 
            , Ldl1Tok . CsPluralTrailRule 
            => CsRule 
                 ( DeclKindTyp . CsPluralRule 
                 , LangInfo . CsPluralToks 
                 , TokClassCsPlural 
                 , LChildren [ Ldl1Child . CsPluralRule_Lhs ] 
                 , LChildren [ Ldl1Child . CsPluralRule_Build ] 
                 ) 
            ; INC ( LangInfo . Productions , 3 ) 
            | Ldl1Tok . CsAltRule 
            => CsRule 
                 ( DeclKindTyp . CsAltRule 
                 , LangInfo . CsAltToks 
                 , TokClassCsClass 
                 , LChildren [ Ldl1Child . CsAltRule_Lhs ] 
                 , LChildren [ Ldl1Child . CsAltRule_Build ] 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseChildren 
                  ( LChildren [ Ldl1Child . CsAltRule_Alternatives ] 
                  , VisitCsAlternative 
                  ) 
              END (* Block *) 

            | Ldl1Tok . FsFixedDefaultRule 
            , Ldl1Tok . FsFixedHorizRule 
            , Ldl1Tok . FsFixedVertRule 
            , Ldl1Tok . FsFixedFillRule 
            , Ldl1Tok . FsListDefaultRule 
            , Ldl1Tok . FsListHorizRule 
            , Ldl1Tok . FsListVertRule 
            , Ldl1Tok . FsListFillRule 
            , Ldl1Tok . FsListTrailDefaultRule 
            , Ldl1Tok . FsListTrailHorizRule 
            , Ldl1Tok . FsListTrailVertRule 
            , Ldl1Tok . FsListTrailFillRule 
            => <* FATAL ANY *> 
              BEGIN 
                AstView . TraverseChildren 
                  ( LChildren [ Ldl1Child . FsFixedHorizRule_Parents ] 
                    (* NOTE! The above hack depends on 
                       Ldl1Tok . Fs[Fixed|List][Horiz|Vert|Fill]Rule_Parents 
                       all being equal *) 
                  , VisitFsLhs 
                  ) 
              END (* Block *) 
            | Ldl1Tok . FsInlineRule 
            => SemError 
                 ( RuleNode . NodeNo 
                 , AFT . E_UnimplementedInlineRules
                 ) 
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END (* Block *) 
      END VisitRule 

  ; BEGIN (* Pass4 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl1Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        LangInfo . Productions := 0 
      ; AstView . GetChildren 
          ( LangInfo . Root , (* VAR *) LRootChildren , LangInfo . LdlLang ) 
      ; <* FATAL ANY *>
        BEGIN 
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl1Child . LanguageDefinition_Rules ] 
            , VisitRule 
            ) 
        END (* Block *) 
      END (* Block *) 
    END Pass4 

; PROCEDURE InsertPrecAndAssoc 
    ( VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; Gram : GrammarSubTyp 
    ) 
  RAISES { AssertionFailure } 

  = VAR InsPrecedence : LRTable . PrecedenceTyp 

  ; PROCEDURE VisitLevel ( LevelNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure } 

    = VAR LevAssoc : LRTable . AssocTyp 

    ; PROCEDURE VisitOperator ( OperatorNode : AstView . AstRefTyp ) 

      = PROCEDURE VoMember ( Tok : IntSets . ValidElemT ) 

        = BEGIN 
            IF LangUtilLo . TokClassFirstTok ( LangInfo , TokClassConstTerm ) 
               <= Tok
               AND Tok 
                   <= LangUtilLo . TokClassLastTok 
                        ( LangInfo , TokClassAsFixed ) 
            THEN 
              Automaton . AddOperator 
                ( Gram , Tok , InsPrecedence , LevAssoc ) 
            ELSIF LangInfo . AsPartialToks <= Tok 
               AND Tok < LangInfo . AsCsClassToks 
            THEN (* Ignore non-ground class member. *) 
            ELSE 
              SemErrorText 
                ( Gram . tokImage ( Tok ) 
                , AFT . E_Precedence_Rule_Operator_Must_Be_A_Declared_Token 
                ) 
            END (* IF *) 
          END VoMember 

      ; BEGIN (* VisitOperator *) 
          TYPECASE <* NOWARN *>
            LdlSemantics . DeclOfRef ( LangInfo , OperatorNode ) 
          OF NULL => 
          | LdlSemantics . SemFirstOccClassTyp ( TFirstOccClass ) 
          => (* A known class. *) 
            IF TFirstOccClass . SingletonTok # LbeStd . Tok__Null 
            THEN VoMember ( TFirstOccClass . SingletonTok ) 
            ELSE (* A plural class rule. *) 
              IntSets . ForAllDo ( TFirstOccClass . TokSet , VoMember ) 
            END (* IF *)
          | LdlSemantics . SemDeclTyp ( TSemDecl ) 
          => (* Some other kind of declaration. *) 
             VoMember ( TSemDecl . DeclTok ) 
          ELSE 
          END (* TYPECASE *) 
        END VisitOperator 

    ; BEGIN (* VisitLevel *) 
        CASE EstUtil . EstTok ( LevelNode . NodeRef ) <* NOWARN *>
        OF Ldl1Tok . PrecLevelNone 
        => LevAssoc := LRTable . AssocTyp . nonassoc 
        | Ldl1Tok . PrecLevelRight 
        => LevAssoc := LRTable . AssocTyp . right 
        | Ldl1Tok . PrecLevelLeft 
        => LevAssoc := LRTable . AssocTyp . left 
        END (* CASE *) 
      ; <* FATAL ANY *> 
        BEGIN 
          AstView . TraverseChildren 
            ( AstView . Child 
                ( LevelNode 
                , Ldl1Child . PrecLevelNone_Operators 
                , LangInfo . LdlLang 
                ) 
              (* ^This depends on PrecLevelNone_Operators 
                 = PrecLevelRight_Operators = PrecLevelLeft_Operators *) 
            , VisitOperator 
            ) 
        END (* Block *) 
      ; InsPrecedence 
          := Automaton . NextPrec ( Gram , InsPrecedence ) 
      END VisitLevel 

  ; BEGIN (* InsertPrecAndAssoc *) 
      IF LangInfo . PrecRule . NodeRef # NIL 
      THEN (* Insert precedence and associativity info into LALR *) 
        InsPrecedence := Automaton . FirstPrec ( Gram ) 
      ; <* FATAL ANY *> 
        BEGIN 
          AstView . TraverseChildren 
            ( AstView . Child 
                ( LangInfo . PrecRule , Ldl1Child . PrecRule_Levels 
                , LangInfo . LdlLang ) 
            , VisitLevel 
            ) 
        END (* Block *) 
      END (* IF *) 
    END InsertPrecAndAssoc 

; TYPE GrammarSubTyp 
    = LRTable . GrammarTyp 
        OBJECT 
          LangInfo : LdlSemantics . LangInfoRefTyp 
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

; PROCEDURE BetweenPass4And5 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Between passes 4 and 5: 
     - Rebias counts of concrete nonterminal kinds to prepare for 
       assigning actual token values during pass 5. 
     - Expand TokMap to hold the Cs tokens. 
     - Create an empty FsTreeMap big enough for all terminals and 
       abstract rules. 
     - Initialize for LALR table construction and insert the 
       precedence and associativaty information. 
     - Build the Fs trees for the constant terminals. 
     - Initialize the LALR generation. 
  *) 

  = VAR LCsToks : LbeStd . TokTyp 
  ; VAR LGram : GrammarSubTyp 

  ; BEGIN (* BetweenPass4And5 *) 
      LCsToks (* Total, including all previous, plus CS tokens. *) 
        := LangInfo . AsCsClassToks 
           + LangInfo . CsAltToks 
           + LangInfo . CsPlusToks 
           + LangInfo . CsPluralToks 
           + LangInfo . CsStarToks 
           + LangInfo . CsFixedToks 
    ; LangInfo . CsFixedToks (* Beginning of range for CsFixedToks *) 
        := LangInfo . AsCsClassToks 
           + LangInfo . CsAltToks 
           + LangInfo . CsPlusToks 
           + LangInfo . CsPluralToks 
           + LangInfo . CsStarToks 
    ; LangInfo . CsStarToks (* Beginning of range for CsStarToks *) 
        := LangInfo . AsCsClassToks 
           + LangInfo . CsAltToks 
           + LangInfo . CsPlusToks 
           + LangInfo . CsPluralToks 
    ; LangInfo . CsPluralToks (* Beginning of range for CsPluralToks *) 
        := LangInfo . AsCsClassToks 
           + LangInfo . CsAltToks 
           + LangInfo . CsPlusToks 
    ; LangInfo . CsPlusToks (* Beginning of range for CsPlusToks *) 
        := LangInfo . AsCsClassToks + LangInfo . CsAltToks 
    ; LangInfo . CsAltToks (* Beginning of range for CsAltToks *) 
        := LangInfo . AsCsClassToks 
(* TOKPART: Compute LCsToks a new way:
    ; LCsToks := LangInfo . TokPart ^ [ TokClassCsFixed ] 
*) 
    ; LdlSemantics . StackTokCounts2 ( LangInfo ) 

    ; LdlSemantics . ExpandTokMap 
        ( LangInfo . TokMapRef , LCsToks - LdlSemantics . TokMapBias ) 
    ; LangInfo . FsTreeMapRef 
        := LdlSemantics . NewFsTreeMap 
              ( LangInfo . AsListCardToks - LbeStd . Tok__FirstLangDep ) 
    ; FOR RTok := LbeStd . Tok__FirstLangDep TO LangInfo . StringToks - 1 
      DO Ldl1FsTrees . BuildFixedTerm ( LangInfo , RTok ) 
      END (* FOR *) 
    ; IF Messages . MessageCount ( MessageCodes . KindTyp . MkError ) = 0 
      THEN 
        LGram := NEW ( GrammarSubTyp )
      ; LGram . IsGenerated := FALSE  
      ; LangInfo . Gram := LGram 
      ; Automaton . InitAutomaton 
          ( Gram := LangInfo . Gram 
          , ProdCt := LangInfo . Productions 
          , FirstTerminal := LbeStd . Tok__BegOfImage 
          , LastTerminal := LangInfo . VarTermToks - 1 
          , FirstAstNonterminal := LangInfo . VarTermModToks 
          , LastAstNonterminal := LangInfo . AsSublistToks - 1 
       (* , FirstNonterminal := LangInfo . AsCsClassToks *) 
(* TODO: Rework all the LALR stuff so it doesn't try to work on
         tokens in the range AsPartialToks .. AsCsClassToks - 1, 
         (which are ClassOnly tokens), so we can take advantage of the fact 
         that they aren't used in the concrete syntax, and shorten the
         table NNextRef on the left.
*) 
          , FirstNonterminal := LangInfo . AsSublistToks 
          , LastNonterminal := LCsToks - 1 
          , StartSymbol := LangInfo . StartTok 
          ) 
        ; LGram . LangInfo := LangInfo 
        ; LdlSemantics . NoteStandardNontermKinds ( LangInfo , LGram ) 
        ; LdlSemantics . NoteNontermKindForRange 
            ( LGram 
            , FromTok := LangInfo . VarTermModToks 
            , ToTok := LangInfo . AsStarToks 
            , Kind := NontermKindTyp . NtkAbstractAndConcrete 
            ) 
        ; LdlSemantics . NoteNontermKindForRange 
            ( LGram 
            , FromTok := LangInfo . AsSublistToks 
            , ToTok := LangInfo . AsListCardToks 
            , Kind := NontermKindTyp . NtkUnused  
            ) 
        ; LdlSemantics . NoteNontermKindForRange 
            ( LGram 
            , FromTok := LangInfo . AsListCardToks 
            , ToTok := LangInfo . AsPartialToks 
            , Kind := NontermKindTyp . NtkUnused  
            ) 
        ; LdlSemantics . NoteNontermKindForRange 
            ( LGram 
            , FromTok := LangInfo . AsPartialToks 
            , ToTok := LangInfo . AsClassOnlyToks 
            , Kind := NontermKindTyp . NtkAbstractOnly 
            ) 
        ; LdlSemantics . NoteNontermKindForRange 
            ( LGram 
            , FromTok := LangInfo . AsClassOnlyToks 
            , ToTok := LangInfo . AsCsClassToks 
            , Kind := NontermKindTyp . NtkAbstractAndConcrete
            ) 
        ; LdlSemantics . NoteNontermKindForRange 
            ( LGram 
            , FromTok := LangInfo . AsCsClassToks 
            , ToTok := LangInfo . CsFixedToks 
            , Kind := NontermKindTyp . NtkConcreteOnly 
            ) 
(* TODO: ^The above token classes are now identical for Ldl0 and Ldl1. *) 
        ; InsertPrecAndAssoc ( LangInfo , LangInfo . Gram ) 
      END (* IF *) 
    END BetweenPass4And5 

; PROCEDURE Pass5 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Pass 5: 
       - Assign Cs tokens. 
       - Warn if an As node has no building Cs rule. 
       - Check that each As list node has no more than one Cs rule. 
       - Check that each As node has an Fs rule. 
       - Build the Fs trees. 
  *) 

  = PROCEDURE VisitRule ( RuleNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure } 

    = PROCEDURE VarTerm ( ) 
      RAISES { AssertionFailure } 

      = VAR LIdentNode : AstView . AstRefTyp 

      ; BEGIN (* VarTerm *) 
          LIdentNode 
            := AstView . Child 
                 ( RuleNode 
                 , Ldl1Child . AsVarTermRule_Name 
                 , LangInfo . LdlLang 
                 ) 
        ; TYPECASE LangInfo . SemMapRef ^ [ LIdentNode . NodeNo ] . SemRef 
          OF LdlSemantics . SemDeclTyp ( TDeclSemRef ) 
          => Ldl1FsTrees . BuildVarTerm 
               ( LangInfo 
               , RuleNode 
               , TDeclSemRef . DeclTok 
               , TDeclSemRef . DeclTok 
                 - LangInfo . StringToks 
                 + LangInfo . VarTermToks 
               ) 
          ELSE 
          END (* TYPECASE *) 
        END VarTerm 

    ; PROCEDURE MultiAsNodeRule ( ) 
      RAISES { AssertionFailure } 

      = PROCEDURE VisitAsNodeRule ( DeclNode : AstView . AstRefTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* VisitAsNodeRule *) 
            TYPECASE LangInfo . SemMapRef ^ [ DeclNode . NodeNo ] . SemRef 
            OF NULL => 
            | LdlSemantics . SemDeclAsNodeTyp ( TAsNode ) 
            => IF TAsNode . CsRuleCt = 0 
               THEN 
                 SemError 
                   ( DeclNode . NodeNo , AFT . E_NoConcreteRuleBuilds_ ) 
               END (* IF *) 
            ; IF TAsNode . FsIdentNodeNo = LbeStd . EstNodeNoNull 
              THEN 
                SemError 
                  ( DeclNode . NodeNo , AFT . E_NoFormatRuleForThisNode ) 
              END (* IF *) 
            ELSE 
              CantHappen ( AFT . A_Pass5VisitAsNodeRule_BadSemRef ) 
            END (* TYPECASE *) 
          ; TYPECASE LangInfo . SemMapRef ^ [ DeclNode . NodeNo ] . SemRef 
            OF NULL => 
            | LdlSemantics . SemDeclAsListNodeTyp ( TAsNode ) 
            => IF TAsNode . CsRuleCt > 1  
               THEN 
                 SemError 
                   ( DeclNode . NodeNo 
                   , AFT . W_ListNodeHasMultipleConcreteRules
                   ) 
               END (* IF *) 
            ELSE 
            END 
          END VisitAsNodeRule 

      ; BEGIN (* MultiAsNodeRule *) 
          VAR LChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 

        ; BEGIN (* Block *) 
            AstView . GetChildren 
              ( RuleNode , (* VAR *) LChildren , LangInfo . LdlLang ) 
          ; <* FATAL ANY *> 
            BEGIN 
              AstView . TraverseChildren 
                ( AstView . Child 
                    ( RuleNode , Ldl1Child . AsFixedRule_Parents 
                    , LangInfo . LdlLang ) 
                , VisitAsNodeRule 
                ) 
            END (* Block *) 
          (* NOTE! The above hack depends on Ldl1Child . AsFixedRule_Parents 
                   = AsStarRule_Parents = AsPlusRule_Parents *) 
          END (* Block *) 
        END MultiAsNodeRule 

    ; PROCEDURE CsRule 
        ( VAR (* IN OUT *) DeclCt : LbeStd . TokTyp 
        ; TokClass : TokClassTyp 
        ) 
      RAISES { AssertionFailure } 

      = VAR LIdNode : AstView . AstRefTyp 

      ; BEGIN (* CsRule *) 
          LIdNode 
            := AstView . Child 
                 ( RuleNode , Ldl1Child . CsFixedRule_Lhs , LangInfo . LdlLang )
            (* ^This depends on CsFixedRule_Lhs, CsStarRule_Lhs, 
                CsPlusRule_Lhs, CsPluralRule_Lhs, and CsAltRule_Lhs all 
                being equal. 
            *) 
        ; WITH WSemRef = LangInfo . SemMapRef ^ [ LIdNode . NodeNo ] . SemRef 
          DO TYPECASE WSemRef 
             OF NULL 
             => 
             | LdlSemantics . SemFirstOccCsTyp ( TFirstOcc ) 
               (* Assign a token *) 
             => TFirstOcc . DeclTok := DeclCt 
             ; LangInfo . TokMapRef ^ [ DeclCt - LdlSemantics . TokMapBias ] 
                 := LIdNode . NodeNo 
             ; INC ( DeclCt ) 
             ; INC ( LangInfo . TokPart ^ [ TokClass ] )  
             | LdlSemantics . SemAddlDefTyp ( TAddlDef ) 
               (* Copy the token from 1st occ. *) 
             => TAddlDef . RefTok 
                  := FirstOcc ( LangInfo , TAddlDef ) . DeclTok 
             ELSE (* Is an abstract node. *) 
             END (* TYPECASE *) 
          END (* WITH *) 
        END CsRule 

    ; PROCEDURE MultiFsRule ( ) 
      RAISES { AssertionFailure } 

      = PROCEDURE VisitFsIdent ( RefNode : AstView . AstRefTyp ) 
        RAISES { AssertionFailure } 

        = VAR LIsStart : BOOLEAN 

        ; BEGIN (* VisitFsIdent *) 
            TYPECASE LangInfo . SemMapRef ^ [ RefNode . NodeNo ] . SemRef 
            OF NULL => 
            | LdlSemantics . SemRefTyp ( TFsIdentSemRef ) 
            => TYPECASE 
                 LangInfo . SemMapRef ^ [ TFsIdentSemRef . RefDeclId ] . SemRef 
               OF NULL => 
               | LdlSemantics . SemDeclAsNodeTyp ( TAsIdentSemRef ) 
               => TYPECASE 
                    LangInfo . SemMapRef ^ [ LangInfo . StartIdentNodeNo ] 
                    . SemRef  
                  OF NULL => LIsStart := FALSE 
                  | LdlSemantics . SemFirstOccClassTyp ( TClass ) 
                  => LIsStart 
                       := IntSets . IsElement 
                           ( TAsIdentSemRef . DeclTok , TClass . TokSet )
                  ELSE LIsStart := FALSE   
                  END (* TYPECASE *) 
               ; LangInfo . FsTreeMapRef ^ 
                    [ TAsIdentSemRef . DeclTok - LbeStd . Tok__FirstLangDep ] 
                    := Ldl1FsTrees . Build 
                         ( LangInfo 
                         , TFsIdentSemRef 
                         , TAsIdentSemRef 
                         , IsStart := LIsStart  
                         ) 
               ELSE 
               END (* TYPECASE *) 
            ELSE 
            END (* TYPECASE *) 
          END VisitFsIdent 

      ; <* FATAL ANY *> 
        BEGIN (* MultiFsRule *) 
          AstView . TraverseChildren 
            ( AstView . Child 
                ( RuleNode , Ldl1Child . FsFixedHorizRule_Parents 
                , LangInfo . LdlLang 
                ) 
            , VisitFsIdent 
            ) 
          (* NOTE! The above hack depends on 
             Ldl1Tok . Fs[Fixed|List][Horiz|Vert|Fill]Rule_Parents 
             all being equal 
          *) 
        END MultiFsRule 

    ; BEGIN (* VisitRule *) 
        IF RuleNode . NodeRef # NIL 
        THEN 
          CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
          OF Ldl1Tok . AsVarTermRule 
          => VarTerm ( ) 
          | Ldl1Tok . AsFixedRule , Ldl1Tok . AsStarRule , Ldl1Tok . AsPlusRule 
          => MultiAsNodeRule ( ) 
          | Ldl1Tok . CsFixedRule 
          => CsRule ( LangInfo . CsFixedToks , TokClassCsFixed ) 
          | Ldl1Tok . CsStarRule 
          , Ldl1Tok . CsStarTrailRule 
          => CsRule ( LangInfo . CsStarToks , TokClassCsStar ) 
          | Ldl1Tok . CsPlusRule 
          , Ldl1Tok . CsPlusTrailRule 
          => CsRule ( LangInfo . CsPlusToks , TokClassCsPlus ) 
          | Ldl1Tok . CsPluralRule 
          , Ldl1Tok . CsPluralTrailRule 
          => CsRule ( LangInfo . CsPluralToks , TokClassCsPlural )
          | Ldl1Tok . CsAltRule 
          => CsRule ( LangInfo . CsAltToks , TokClassCsClass ) 


          | Ldl1Tok . FsFixedDefaultRule 
          , Ldl1Tok . FsFixedHorizRule 
          , Ldl1Tok . FsFixedVertRule 
          , Ldl1Tok . FsFixedFillRule 
          , Ldl1Tok . FsListDefaultRule 
          , Ldl1Tok . FsListHorizRule 
          , Ldl1Tok . FsListVertRule 
          , Ldl1Tok . FsListFillRule 
          , Ldl1Tok . FsListTrailDefaultRule 
          , Ldl1Tok . FsListTrailHorizRule 
          , Ldl1Tok . FsListTrailVertRule 
          , Ldl1Tok . FsListTrailFillRule 
          => MultiFsRule ( ) 
          | Ldl1Tok . FsInlineRule 
          => 
          ELSE 
          END (* CASE *) 
        END (* IF *) 
      END VisitRule 

  ; BEGIN (* Pass5 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl1Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        AstView . GetChildren 
          ( LangInfo . Root , (* VAR *) LRootChildren , LangInfo . LdlLang ) 
      ; <* FATAL ANY *> 
        BEGIN 
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl1Child . LanguageDefinition_Rules ] 
            , VisitRule 
            ) 
        END (* Block *) 
      END (* Block *) 
    END Pass5 

; PROCEDURE BetweenPass5And6 
    ( LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure }
  (* - Do finish processing for Fs trees. 
     - Close the FormatsEmpty property.
  *) 

  = BEGIN (* BetweenPass5And6 *) 
      FOR RTok := LangUtilLo . TokClassFirstTok 
                    ( LangInfo , TokClassTyp . TokClassAsPlus )
          TO LangUtilLo . TokClassLastTok 
               ( LangInfo , TokClassTyp . TokClassAsFixed )

      DO FsTreeUtils . FinishFsRule 
           ( LangInfo 
           , LangInfo . FsTreeMapRef ^ [ RTok - LdlSemantics . FsTreeMapBias ]
           ) 
      END (* FOR *) 
    ; FsTreeUtils . CloseFormatsEmpty ( LangInfo ) 
    ; LdlSemantics . CheckTokPart ( LangInfo ) 
    END BetweenPass5And6 

; PROCEDURE Pass6 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Pass 6: 
       - Insert productions into the LALR table builder. 
  *) 

  = PROCEDURE VisitRule ( RuleNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure } 

    = PROCEDURE AsCsClassRule ( )   
      RAISES { AssertionFailure } 

      = BEGIN (* AsCsClassRule *) 
          TYPECASE 
            LangInfo . SemMapRef ^ 
              [ AstView . Child 
                  ( RuleNode , Ldl1Child . AsClassRule_ClassName 
                  , LangInfo . LdlLang 
                  ) 
                . NodeNo 
              ] 
            . SemRef 
          OF NULL => 
          | LdlSemantics . SemFirstOccClassTyp ( TFirstOcc ) 
          => LdlSemantics . GenNonclassTokSetProductions 
               ( LangInfo 
               , LangInfo . Gram
               , TFirstOcc . DeclTok 
               , TFirstOcc . TokSet 
               ) 
          ELSE 
          END (* TYPECASE *) 
        END AsCsClassRule 

    ; PROCEDURE BuildTok 
        ( LhsTok : LbeStd . TokTyp 
        ; BuildNode : AstView . AstRefTyp 
        ) 
      : LbeStd . TokTyp 

      = VAR LResult : LbeStd . TokTyp 
      ; VAR LBuildExists : BOOLEAN := FALSE 

      ; BEGIN 
          IF BuildNode . NodeRef = NIL 
          THEN LResult := LhsTok  
(* RIXME: ^This looks like it always builds Lhs in absence of BUILD. *) 
          ELSE 
            TYPECASE LangInfo . SemMapRef ^ [ BuildNode . NodeNo ] . SemRef 
            OF NULL 
            => LResult := LbeStd . Tok__Null  

            | LdlSemantics . SemAddlDefCsTyp ( TSemAddlDef ) 
            => LResult := TSemAddlDef . RefTok 
            ; LBuildExists := TRUE 

            ELSE LResult := LbeStd . Tok__Null  
            END (* TYPECASE *) 
          END (* IF *) 
        ; IF LResult # LbeStd . Tok__Null 
          THEN 
            TYPECASE LdlSemantics . SemDeclOfTok ( LangInfo , LResult ) 
            OF NULL => LResult := LbeStd . Tok__Null 

            | LdlSemantics . SemDeclAsNodeTyp 
            => (* LResult is an As node. *) 

            ELSE LResult := LbeStd . Tok__Null 
            END (* TYPECASE *) 
          END (* IF *) 
        ; RETURN LResult 
        END BuildTok 

    ; PROCEDURE CsFixedRule ( ) 
       RAISES { AssertionFailure } 

      = VAR FixedAsChildList : AstView . AstRefTyp 
      ; VAR FixedAsChildNode : AstView . AstRefTyp 
      ; VAR FixedAsChildWasFound : BOOLEAN 
      ; VAR FixedFsEstChildrenMap : LdlSemantics . FsNodeArrayRefTyp 

      ; PROCEDURE Rhs 
          ( LhsTok : LbeStd . TokTyp 
          ; BuildTok : LbeStd . TokTyp 
          ; CsRhs : AstView . AstRefTyp 
          ; OptionCt : LRTable . OptionIdTyp 
          ; RhsCt : LbeStd . TokNoTyp 
          ) 
        RAISES { AssertionFailure } 

        = VAR Rhs_Right : LRTable . TokArrayRefTyp 
        ; VAR Rhs_NextRightSs : PortTypes . Int32Typ 
        ; VAR Rhs_OptionIdSet : LRTable . OptionIdSetTyp 
        ; VAR Rhs_NestingDepth : PortTypes . Int32Typ 
        ; VAR Rhs_NextFsEstChildSs : CARDINAL 

        ; PROCEDURE VisitChild ( CsChildNode : AstView . AstRefTyp ) 
          RAISES { AssertionFailure } 

          = VAR LCsSemDecl : LdlSemantics . SemDeclTyp 

          ; BEGIN (* VisitChild *) 
              CASE EstUtil . EstTok ( CsChildNode . NodeRef ) 
              OF Ldl1Tok . Ident , Ldl1Tok . String 
              => LCsSemDecl 
                   := LdlSemantics . DeclOfRef ( LangInfo , CsChildNode ) 
              ; IF FixedAsChildList . NodeNo # LbeStd . EstNodeNoNull 
                   (* LCsSemDecl corresponds to an abstract child *) 
                   AND FALSE (* We don't have AS-CS correspondence *) 
                THEN 
                  IF FixedAsChildWasFound 
                  THEN (* Any checks against corresponding abstract 
                          child go here. *) 
                    IF EstUtil . EstTok ( FixedAsChildNode . NodeRef ) 
                        = Ldl1Tok . AsOptChild 
                     THEN
                       SemError2 
                         ( CsChildNode . NodeNo 
                         , FixedAsChildNode . NodeNo 
                         , AFT . E_ConcreteChildMustMatchRequiredAbstractChild
                         ) 
                     END (* IF *) 
                   ; AstView . NextChild 
                      ( FixedAsChildList 
                      , FixedAsChildNode 
                      , FixedAsChildWasFound 
                      ) 
                  ELSE 
                    SemError 
                      ( CsChildNode . NodeNo 
                      , AFT . E_NoCorrespondingAbstractChild 
                      ) 
                  END (* IF *) 
                END (* IF *) 
              ; IF LCsSemDecl # NIL 
                THEN 
                  Rhs_Right ^ [ Rhs_NextRightSs ] := LCsSemDecl . DeclTok 
                ; INC ( Rhs_NextRightSs ) 
                END (* IF *) 

              | Ldl1Tok . Concatenation 
              => IF FixedAsChildList . NodeNo # LbeStd . EstNodeNoNull 
                    (* LCsSemDecl corresponds to an abstract child *) 
                    AND FALSE (* We don't have AS-CS correspondence *) 
                 THEN (* Any checks against corresponding abstract 
                             child go here. *) 
                   IF FixedAsChildWasFound 
                   THEN (* This is always OK too. *) 
                     IF EstUtil . EstTok ( FixedAsChildNode . NodeRef ) 
                        # Ldl1Tok . AsOptChild 
                     THEN
                       SemError2 
                         ( CsChildNode . NodeNo 
                         , FixedAsChildNode . NodeNo 
                         , AFT . E_OptionalChildrenMustMatchOptionalAbstractChild
                         ) 
                     END (* IF *) 
                   ; AstView . NextChild 
                       ( FixedAsChildList 
                       , FixedAsChildNode 
                       , FixedAsChildWasFound 
                       ) 
                   ELSE 
                     SemError 
                       ( CsChildNode . NodeNo 
                       , AFT . E_NoCorrespondingAbstractOptionalChild 
                       ) 
                   END (* IF *) 
                 END (* IF *) 
              ; IF Rhs_NestingDepth > 1 
                THEN 
                  SemError2 
                    ( CsChildNode . NodeNo 
                    , RuleNode . NodeNo 
                    , AFT . E_NestedOptionalChildrenOfConcreteRule 
                    ) 
                END (* IF *) 
              ; IF FixedFsEstChildrenMap = NIL 
                THEN
                  SemError2 
                    ( CsChildNode . NodeNo 
                    , RuleNode . NodeNo 
                    , AFT . E_AbstractRuleHasNoOptionalChildren  
                    ) 
                ELSE
                  IF FixedFsEstChildrenMap ^ [ Rhs_NextFsEstChildSs ]
                       . FsOptionIds [ FALSE ]   
                       IN Rhs_OptionIdSet 
                  THEN 
                    INC ( Rhs_NestingDepth ) 
                  ; <* FATAL ANY *> 
                    BEGIN 
                      AstView . TraverseChildren ( CsChildNode , VisitChild ) 
                    END (* Block *)
                  ; DEC ( Rhs_NestingDepth ) 
                  END (* IF *) 
                END (* IF *) 
              ; INC ( Rhs_NextFsEstChildSs ) 

              | Ldl1Tok . Nil 
              => 
(* TODO: Complete this. *) 
              ELSE 
                CantHappen 
                  ( AFT . A_Pass6_Visit_Rule_CsFixedRule_Rhs_VisitChild_BadChildKind 
                  ) 
              END (* CASE *) 
            END VisitChild 

        ; BEGIN (* Rhs *) 
            VAR LRight : LRTable . TokArrayRefTyp 

          ; BEGIN (* Block Rhs. *) 
              Rhs_Right := NEW ( LRTable . TokArrayRefTyp , RhsCt ) 
              (* ^Possibly slightly extravagant. *) 
            ; FOR RI := 0 TO Word . LeftShift ( 1 , OptionCt ) - 1 
(* FIXME: ^This probably won't work if OptionCt = BITSIZE ( Word.T ) *) 
              DO Rhs_OptionIdSet := LRTable . OptionIdSetEmpty 
              ; FOR ROption := 0 TO OptionCt - 1 
                DO IF Word . Extract ( RI , ROption , 1 ) # 0 
                   THEN 
                     Rhs_OptionIdSet 
                       := Rhs_OptionIdSet 
                          + LRTable . OptionIdSetTyp { ROption } 
                   END (* IF *) 
                END (* FOR *) 
(* TODO: ^See if this can be done faster by bit twiddling, yet 
          without introducing implementation dependencies. *) 
              ; Rhs_NextRightSs := 0 
              ; Rhs_NextFsEstChildSs := 0 
              ; Rhs_NestingDepth := 1 
              ; <* FATAL ANY *>  
                BEGIN 
                  AstView . TraverseChildren ( CsRhs , VisitChild ) 
                END (* Block *) 
              ; Assert 
                  ( Rhs_NextFsEstChildSs = OptionCt 
                  , AFT . A_Ldl1Semantics_Pass6_VisitRule_CsFixedRule_Rhs_NextFsEstChildNo_Bad 
                  )
              ; LRight := NEW ( LRTable . TokArrayRefTyp , Rhs_NextRightSs ) 
              ; LRight ^ := SUBARRAY ( Rhs_Right ^ , 0 , Rhs_NextRightSs ) 
              ; Automaton . AddExternalProduction 
                  ( LangInfo . Gram 
                  , LhsTok 
                  , LRight 
                  , OptionIdSet := Rhs_OptionIdSet 
                  , BuildTok := BuildTok 
                  ) 
              END (* FOR *) 
            ; Rhs_Right := NIL 
            END (* Block *) 
          END Rhs 

      ; BEGIN (* CsFixedRule *) 
          VAR LAsRuleNode : AstView . AstRefTyp 
        ; VAR LBuildNodeNo : LbeStd . EstNodeNoTyp 
        ; VAR LLhsTok : LbeStd . TokTyp 
        ; VAR LBuildTok : LbeStd . TokTyp 
        ; VAR LOptionCt : LRTable . OptionIdTyp := 0 
        ; VAR LRhsCt : LbeStd . TokNoTyp := 0 

        ; VAR LChildren 
            : ARRAY [ 0 .. Ldl1Child . CsFixedRule_Rhs ] 
              OF AstView . AstRefTyp 

        ; BEGIN (* Block CsFixedRule *) 
            AstView . GetChildren 
              ( RuleNode , (* VAR *) LChildren , LangInfo . LdlLang ) 
          ; TYPECASE 
              LangInfo . SemMapRef ^ 
                [ LChildren [ Ldl1Child . CsFixedRule_Lhs ] . NodeNo ] 
              . SemRef 
            OF NULL => LLhsTok := LbeStd . Tok__Null 
            | LdlSemantics . SemFirstOccCsTyp ( TFirstOcc ) 
            => LLhsTok := TFirstOcc . DeclTok 
            ; LOptionCt := TFirstOcc . OptionCt 
            ; LRhsCt := TFirstOcc . RhsCt 

            | LdlSemantics . SemAddlDefCsTyp ( TAddlDef ) 
            => LLhsTok := TAddlDef . RefTok 
            ; LOptionCt := TAddlDef . OptionCt 
            ; LRhsCt := TAddlDef . RhsCt 
            ELSE LLhsTok := LbeStd . Tok__Null 
            END (* TYPECASE *) 
          ; LBuildTok 
              := BuildTok 
                   ( LLhsTok  
                   , LChildren [ Ldl1Child . CsFixedRule_Build ] 
                   )     
          ; IF LBuildTok = LbeStd . Tok__Null  
            THEN 
              FixedAsChildList := AstView . AstRefNull 
            ; FixedFsEstChildrenMap := NIL 
            ELSE 
              LBuildNodeNo 
                := LangInfo . TokMapRef ^ 
                     [ LBuildTok - LdlSemantics . TokMapBias ]
            ; LAsRuleNode 
                := AstView . AstRefTyp 
                     { NodeNo := LBuildNodeNo 
                     , NodeRef 
                         := LangInfo . SemMapRef ^ [ LBuildNodeNo ] . EstRef 
                     , ChildNo := LbeStd . EstChildNoNull 
                     } 
            ; FixedAsChildList 
                := AstView . Child 
                     ( LAsRuleNode 
                     , Ldl1Child . AsFixedRule_Children 
                     , LangInfo . LdlLang 
                     ) 
            ; AstView . FirstChild 
                ( FixedAsChildList 
                , FixedAsChildNode 
                , FixedAsChildWasFound 
                ) 
            ; FixedFsEstChildrenMap 
                := LdlSemantics . MapFsEstChildren 
                     ( LangInfo . FsTreeMapRef ^ 
                         [ LBuildTok - LbeStd . Tok__FirstLangDep ]
                     ) 
            END (* IF *) 
          ; IF LLhsTok # LbeStd . Tok__Null 
            THEN 
              Rhs 
                ( LLhsTok 
                , LBuildTok 
                , LChildren [ Ldl1Child . CsFixedRule_Rhs ] 
                , LOptionCt 
                , LRhsCt 
                ) 
            END (* IF *) 
          END (* Block *) 
        END CsFixedRule 

    ; PROCEDURE CsListRule 
        ( Kind : [ DeclKindTyp . CsStarRule .. DeclKindTyp . CsPluralRule ]
        ; Trailing : BOOLEAN 
        ) 
      RAISES { AssertionFailure } 

      = VAR CsListSeps : LRTable . TokArrayRefTyp 
      ; VAR CsListSepCt : CARDINAL 
      ; VAR CsListRight : LRTable . TokArrayRefTyp 
      ; VAR CsListNextRightSs : PortTypes . Int32Typ 

      ; PROCEDURE InsertSep ( SepNode : AstView . AstRefTyp ) 

        = VAR LSepSemDecl : LdlSemantics . SemDeclTyp 

        ; BEGIN (* InsertSep *) 
            LSepSemDecl := LdlSemantics . DeclOfRef ( LangInfo , SepNode ) 
          ; IF LSepSemDecl # NIL 
            THEN 
              CsListRight ^ [ CsListNextRightSs ] := LSepSemDecl . DeclTok 
            ; INC ( CsListNextRightSs ) 
            END (* IF *) 
          END InsertSep 

      ; BEGIN (* CsListRule *) 
          VAR LChildren 
            : ARRAY [ 0 .. Ldl1Child . CsStarRule_Separators ] 
              OF AstView . AstRefTyp 
        ; VAR LLhsTok : LbeStd . TokTyp 
        ; VAR LSublistTok : LbeStd . TokTyp 
        ; VAR LLhsSemDecl : LdlSemantics . SemDeclTyp 
        ; VAR LElemSemDecl : LdlSemantics . SemDeclTyp 
        ; VAR LElemTok : LbeStd . TokTyp 
        ; VAR LBuildTok : LbeStd . TokTyp 
        ; VAR LRight : LRTable . TokArrayRefTyp

        ; BEGIN (* Block CsListRule. *) 
            AstView . GetChildren 
              ( RuleNode , (* VAR *) LChildren , LangInfo . LdlLang ) 
          ; LLhsSemDecl
              := LdlSemantics . DeclOfRef 
                   ( LangInfo , LChildren [ Ldl1Child . CsStarRule_Lhs ] ) 
          ; IF LLhsSemDecl = NIL 
            THEN
              LLhsTok := LbeStd . Tok__Null 
            ELSE 
              LLhsTok := LLhsSemDecl . DeclTok 
            END 
          ; LBuildTok 
              := BuildTok 
                   ( LLhsTok , LChildren [ Ldl1Child . CsStarRule_Build ] ) 
          ; IF LBuildTok = LbeStd . Tok__Null 
            THEN 
              SemError 
                ( RuleNode . NodeNo 
                , AFT . E_ConcreteListRuleMustBuildANode
                ) 
            ; LSublistTok := LbeStd . Tok__Null 
            ELSE 
              TYPECASE LdlSemantics . SemDeclOfTok ( LangInfo , LBuildTok ) 
              OF NULL => LSublistTok := LbeStd . Tok__Null 
              | LdlSemantics . SemDeclAsListNodeTyp ( TSemList ) 
              => LSublistTok := TSemList . SublistTok 
              ELSE 
                LSublistTok := LbeStd . Tok__Null 
              ; SemError 
                  ( RuleNode . NodeNo 
                  , AFT . E_ConcreteListRuleMustBuildAnAbstractListNode
                  ) 
              END (* TYPECASE *) 
            END (* IF *) 
          ; LElemSemDecl 
              := LdlSemantics . DeclOfRef 
                   ( LangInfo , LChildren [ Ldl1Child . CsStarRule_ListChild ] )
          ; IF LElemSemDecl # NIL 
            THEN 
              LElemTok := LElemSemDecl . DeclTok 
            ELSE 
              LElemTok := LbeStd . Tok__Null 
            END (* IF *) 
          ; IF LLhsTok # LbeStd . Tok__Null 
               AND LElemTok # LbeStd . Tok__Null 
               AND LSublistTok # LbeStd . Tok__Null 
            THEN (* All is normal.  Generate concrete productions. *) 
            (* L is the list token. 
               L' is the sublist token. 
               E is the element token.
               <seps> is the string of separator tokens. 
            *)

            (* Fill CsListSeps with <seps>.  If no there are no separators, 
               still make it an allocated array of zero elements. *)
              CsListRight 
                := NEW ( LRTable . TokArrayRefTyp 
                       , AstView . FastExtravagantChildCt 
                           ( LChildren [ Ldl1Child . CsStarRule_Separators ] ) 
                       ) 
            ; CsListNextRightSs := 0 
            ; AstView . TraverseChildren 
                ( LChildren [ Ldl1Child . CsStarRule_Separators ] 
                , InsertSep 
                ) 
            ; IF CsListNextRightSs = NUMBER ( CsListRight ^ ) 
              THEN CsListSeps := CsListRight 
              ELSE 
                CsListSeps 
                  := NEW ( LRTable . TokArrayRefTyp , CsListNextRightSs ) 
              ; CsListSeps ^ 
                  := SUBARRAY ( CsListRight ^ , 0 , CsListNextRightSs ) 
              END (* IF *) 
            ; CsListSepCt := NUMBER ( CsListSeps ^ ) 

            (* For any star rule, add L BUILD L ::= <empty> *) 
            ; IF Kind = DeclKindTyp . CsStarRule 
              THEN 
                Automaton . AddExternalProduction 
                  ( LangInfo . Gram 
                  , LLhsTok 
                  , Right := LRTable . EmptyString ( ) 
                  , BuildTok := LBuildTok 
                  ) 
              (* Empty list never has trailing separators. *) 
              ; IF FALSE AND Trailing 
                (* Empty list never has trailing separators, but if we ever
                   want to allow it, here is how. *) 
                THEN (* Add L BUILD L ::= <seps> *) 
                  Automaton . AddExternalProduction 
                    ( LangInfo . Gram 
                    , LLhsTok 
                    , Right := CsListSeps 
                    , OptionIdSet := LRTable . OptionIdSetForListTrail 
                    , BuildTok := LBuildTok 
                    ) 
                END (* IF *) 
              END (* IF *) 

            (* For any star or plus rule, add L BUILD L ::= E  
               and for star or plus trailing rule, 
               add L BUILD L ::= E [ <seps> ] *) 
            ; CASE Kind 
              OF DeclKindTyp . CsStarRule , DeclKindTyp . CsPlusRule 
              => LRight := NEW ( LRTable . TokArrayRefTyp , 1 ) 
              ; LRight ^ [ 0 ] := LElemTok 
              ; Automaton . AddExternalProduction 
                  ( LangInfo . Gram 
                  , LLhsTok 
                  , LRight 
                  , BuildTok := LBuildTok 
                  ) 
              ; IF Trailing 
                THEN 
                  LRight := NEW ( LRTable . TokArrayRefTyp , 1 + CsListSepCt )  
                ; LRight ^ [ 0 ] := LElemTok 
                ; SUBARRAY ( LRight ^ , 1 , CsListSepCt ) := CsListSeps ^ 
                ; Automaton . AddExternalProduction 
                    ( LangInfo . Gram 
                    , LLhsTok 
                    , LRight 
                    , OptionIdSet := LRTable . OptionIdSetForListTrail 
                    , BuildTok := LBuildTok 
                    ) 
                END (* IF *) 
              ELSE 
              END (* CASE *) 

            (* For all (list) rules, add L BUILD L ::= L' *) 
            ; LRight := NEW ( LRTable . TokArrayRefTyp , 1 )  
            ; LRight ^ [ 0 ] := LSublistTok 
            ; Automaton . AddExternalProduction 
                ( LangInfo . Gram 
                , LLhsTok 
                , LRight 
                , BuildTok := LBuildTok  
                ) 
            (* For all trail rules, add L BUILD L ::= L' <seps> *) 
            ; IF Trailing 
              THEN 
                LRight := NEW ( LRTable . TokArrayRefTyp , 1 + CsListSepCt )  
              ; LRight ^ [ 0 ] := LSublistTok 
              ; SUBARRAY ( LRight ^ , 1 , CsListSepCt ) := CsListSeps ^ 
              ; Automaton . AddExternalProduction 
                  ( LangInfo . Gram 
                  , LLhsTok 
                  , LRight 
                  , OptionIdSet := LRTable . OptionIdSetForListTrail 
                  , BuildTok := LBuildTok 
                  ) 
              END (* IF *) 

            (* For all rules, add L' ::= E <seps> E *) 
            ; LRight := NEW ( LRTable . TokArrayRefTyp , 2 + CsListSepCt )  
            ; LRight ^ [ 0 ] := LElemTok 
            ; SUBARRAY ( LRight ^ , 1 , CsListSepCt ) := CsListSeps ^ 
            ; LRight ^ [ CsListSepCt + 1 ] := LElemTok 
            ; Automaton . AddExternalProduction 
                ( LangInfo . Gram 
                , Left := LSublistTok 
                , Right := LRight 
                , BuildTok := LbeStd . Tok__Null  
                ) 

            (* For all list rules, add L' ::= E <seps> L' *) 
            ; LRight := NEW ( LRTable . TokArrayRefTyp , 2 + CsListSepCt )  
            ; LRight ^ [ 0 ] := LElemTok 
            ; SUBARRAY ( LRight ^ , 1 , CsListSepCt ) := CsListSeps ^ 
            ; LRight ^ [ CsListSepCt + 1 ] := LSublistTok 
            ; Automaton . AddExternalProduction 
                ( LangInfo . Gram 
                , Left := LSublistTok 
                , Right := LRight 
                , BuildTok := LbeStd . Tok__Null  
                ) 

            (* For all list rules, add L' ::= L' <seps> E *) 
            ; LRight := NEW ( LRTable . TokArrayRefTyp , 2 + CsListSepCt )  
            ; LRight ^ [ 0 ] := LSublistTok 
            ; SUBARRAY ( LRight ^ , 1 , CsListSepCt ) := CsListSeps ^ 
            ; LRight ^ [ CsListSepCt + 1 ] := LElemTok 
            ; Automaton . AddExternalProduction 
                ( LangInfo . Gram 
                , Left := LSublistTok 
                , Right := LRight 
                , BuildTok := LbeStd . Tok__Null  
                ) 

            (* For all rules, add L' ::= L' <seps> L' *) 
            ; LRight := NEW ( LRTable . TokArrayRefTyp , 2 + CsListSepCt ) 
            ; LRight ^ [ 0 ] := LSublistTok 
            ; SUBARRAY ( LRight ^ , 1 , CsListSepCt ) := CsListSeps ^ 
            ; LRight ^ [ CsListSepCt + 1 ] := LSublistTok 
            ; Automaton . AddExternalProduction 
                ( LangInfo . Gram 
                , Left := LSublistTok 
                , Right := LRight 
                , IsList := TRUE 
                , BuildTok := LbeStd . Tok__Null 
                ) 

            END (* IF *) 
          END (* Block *) 
        END CsListRule 

    ; PROCEDURE CsAltRule ( ) 
      RAISES { AssertionFailure } 

      = VAR AltLhsTok : LbeStd . TokTyp 
      ; VAR AltBuildTok : LbeStd . TokTyp 

      ; PROCEDURE VisitAlternative ( AltNode : AstView . AstRefTyp ) 
        RAISES { AssertionFailure } 

        = VAR LElemSemDecl : LdlSemantics . SemDeclTyp 
        ; VAR LRight : LRTable . TokArrayRefTyp 

        ; BEGIN (* VisitAlternative *) 
            LElemSemDecl := LdlSemantics . DeclOfRef ( LangInfo , AltNode ) 
          ; IF LElemSemDecl # NIL 
            THEN 
              LRight := NEW ( LRTable . TokArrayRefTyp , 1 ) 
            ; LRight ^ [ 0 ] := LElemSemDecl . DeclTok 
            ; Automaton . AddExternalProduction 
                ( LangInfo . Gram 
                , AltLhsTok 
                , LRight 
                , BuildTok := AltBuildTok   
                ) 
            END (* IF *) 
          END VisitAlternative 

      ; BEGIN (* CsAltRule *) 
          VAR LChildren 
            : ARRAY [ 0 .. Ldl1Child . CsAltRule_Alternatives ] 
              OF AstView . AstRefTyp 

        ; BEGIN (* Block  CsAltRule body block. *) 
            AstView . GetChildren 
              ( RuleNode , (* VAR *) LChildren , LangInfo . LdlLang ) 
          ; TYPECASE 
              LdlSemantics . DeclOfRef 
                ( LangInfo , LChildren [ Ldl1Child . CsAltRule_Lhs ] ) 
            OF NULL 
            => 
            | LdlSemantics . SemDeclTyp ( TSemDecl ) 
            => AltLhsTok := TSemDecl . DeclTok 
            ; AltBuildTok 
                := BuildTok 
                     ( AltLhsTok 
(* CHECK: ^This allows for what is really a CS fixed rule with
   singleton RHS that was parsed as an Alt rule with singleton
   RHS, to be treated as the former.  Its effects spill over to the
   case where it is an Alt rule with multiple RHS members, and the LHS
   is an abstract node.  In this case, it will generate building
   productions for all the alternatives.  Is this what we want?
   Perhaps.  But I doubt it will ever happen before handwritten
   grammars are retired.
*)  
                     , LChildren [ Ldl1Child . CsAltRule_Build ] 
                     ) 
            ; <* FATAL ANY *> 
              BEGIN 
                AstView . TraverseChildren 
                  ( LChildren [ Ldl1Child . CsAltRule_Alternatives ] 
                  , VisitAlternative 
                  ) 
              END (* Block *) 
            END (* TYPECASE *) 
          END (* Block *) 
        END CsAltRule 

    ; BEGIN (* VisitRule *) 
        CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
        OF Ldl1Tok . AsClassRule 
        => (* Ignore these. *)  
        | Ldl1Tok . AsCsClassRule 
        => AsCsClassRule ( ) 
        | Ldl1Tok . CsFixedRule 
        => CsFixedRule ( ) 
        | Ldl1Tok . CsStarRule 
        => CsListRule ( DeclKindTyp . CsStarRule , Trailing := FALSE ) 
        | Ldl1Tok . CsPlusRule 
        => CsListRule ( DeclKindTyp . CsPlusRule , Trailing := FALSE ) 
        | Ldl1Tok . CsPluralRule 
        => CsListRule ( DeclKindTyp . CsPluralRule , Trailing := FALSE ) 
        | Ldl1Tok . CsStarTrailRule 
        => CsListRule ( DeclKindTyp . CsStarRule , Trailing := TRUE ) 
        | Ldl1Tok . CsPlusTrailRule 
        => CsListRule ( DeclKindTyp . CsPlusRule , Trailing := TRUE ) 
        | Ldl1Tok . CsPluralTrailRule 
        => CsListRule ( DeclKindTyp . CsPluralRule , Trailing := TRUE ) 
        | Ldl1Tok . CsAltRule 
        => CsAltRule ( ) 
        ELSE 
        END (* CASE *) 
      END VisitRule 

  ; BEGIN (* Pass6 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl1Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        AstView . GetChildren 
          ( LangInfo . Root , (* VAR *) LRootChildren , LangInfo . LdlLang ) 
      ; <* FATAL ANY *> 
        BEGIN 
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl1Child . LanguageDefinition_Rules ] 
            , VisitRule 
            ) 
        END (* Block *) 
      END (* Block *) 
    END Pass6 

; PROCEDURE AfterPass6 ( LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 

  = BEGIN (* AfterPass6 *) 
      Assert 
        ( LangInfo . CsFixedToks - LdlSemantics . TokMapBias 
          = NUMBER ( LangInfo . TokMapRef ^ ) 
        , AFT . A_AfterPass6_TokenCountMismatch 
        ) 
      (* It would be better to check each range of tokens separately, 
         but that would require several extra fields. 
      *) 
    END AfterPass6 

; PROCEDURE MakeLangId ( Info : LdlSemantics . LangInfoRefTyp ) 

  = VAR LShortName : TEXT := SharedStrings . ToText ( Info . LanguageName ) 
  ; VAR LFullName := "Schutz Language Definition Language " & LShortName 
  ; VAR LCt : PortTypes . Int32Typ 
  ; VAR LVersionIncompatible := 0 
  ; VAR LVersionUpwardCompatible := 0 
  ; VAR LVersionCompatible := 0 
  ; VAR LVersionMinor := 0 

  ; BEGIN (* MakeLangId *) 
      IF Text . Equal ( LShortName , "Ldl1" ) 
      THEN LVersionIncompatible := 1 
      END (* IF *) 
    ; IF Info . VersionNos # NIL 
      THEN 
        LCt := NUMBER ( Info . VersionNos ^ ) 
      ; IF LCt > 0 
        THEN LVersionIncompatible := Info . VersionNos ^ [ 0 ] 
        END (* IF *) 
      ; IF LCt > 1 
        THEN LVersionUpwardCompatible := Info . VersionNos ^ [ 1 ] 
        END (* IF *) 
      ; IF LCt > 2 
        THEN LVersionCompatible := Info . VersionNos ^ [ 2 ] 
        END (* IF *) 
      ; IF LCt > 3 
        THEN LVersionMinor := Info . VersionNos ^ [ 3 ] 
        END (* IF *)
      END (* IF *) 
    ; Info . DefLangIdRef 
        := NEW 
             ( LangUtil . LangIdRefTyp 
             , LangName := LFullName 
             , LangShortName := LShortName 
             , LangVersion 
                 := LangUtil . LangVersionTyp 
                      { Incompatible := LVersionIncompatible 
                      , UpwardCompatible := LVersionUpwardCompatible 
                      , Compatible := LVersionCompatible 
                      , Minor := LVersionMinor 
                      } 
             , LangDate 
                 := Date . FromTime ( Time . Now ( ) , z := Date . UTC ) 
             , LangFingerprint 
                 := Fingerprint . Combine 
                      ( Fingerprint . FromText ( LFullName) 
                      , Fingerprint . FromText 
                          ( Fmt . Int ( LVersionIncompatible , 13 ) & "."  
                            & Fmt . Int ( LVersionUpwardCompatible , 13 ) & "."
                            & Fmt . Int ( LVersionCompatible , 13 ) & "."  
                            & Fmt . Int ( LVersionMinor , 13 ) & "."  
                          ) 
                      ) 
(* TODO: Make a better fingerprint *) 
             ) 
    ; Info . LdlLangIdRef := LangUtil . LangIdRef ( LbeStd . LangLdl0 )
    END MakeLangId 

(* VISIBLE: *) 
; PROCEDURE Analyze 
    ( Est : LbeStd . EstRootTyp 
    ; LdlLang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  : LdlSemantics . LangInfoRefTyp 
  RAISES { AssertionFailure } 
  (* Does not do LALR Generation. *) 

  = VAR LLangInfo : LdlSemantics . LangInfoRefTyp 
  ; VAR LEstTravInfo : TravUtil . EstTravInfoTyp 
  ; VAR LPreConcreteErrorCt : CARDINAL := 0 

  ; BEGIN (* Analyze *) 
      TYPECASE Est 
      OF NULL 
      => RETURN NIL 
      | EstHs . EstRefTyp ( TEstRef )  
      => IF EstUtil . HasSyntErrors ( TEstRef ) 
         THEN 
           SemError ( 0 , AFT . E_NotSyntacticallyCorrect ) 
         ; RETURN NIL 
         ELSE
           IF TEstRef . EstTok = LbeStd . Tok__Augment 
(* TODO: Do this in Ldl0Semantics too? *) 
           THEN (* Move downward from augmenting node. *) 
             TravUtil . InitEstTravInfoFwd 
               ( LEstTravInfo 
               , TEstRef 
               , KindSet := EstHs . EstChildKindSetEmpty
               , ParentAbsNodeNo := 0 
               ) 
           ; TravUtil . SetToNextInKindSet
               ( (* IN OUT *) LEstTravInfo  
               , StartChildNo := 0 
               , KindSet := EstHs . EstChildKindSetEstChild  
               ) 
           ; Est := LEstTravInfo . EtiChildLeafElem . LeChildRef 
           END (* IF *)  
         END (* IF *) 
      ELSE (* It could be a placeholder. *) 
        RETURN NIL 
      END (* TYPECASE *) 
    ; TYPECASE Est 
      OF NULL 
      => RETURN NIL 
      | EstHs . EstRefTyp ( TEstRef )  
      => LLangInfo := NEW ( LdlSemantics . LangInfoRefTyp ) 
      ; Assert
          ( LdlLang = LbeStd . LangNull 
            OR LdlLang = LbeStd . LangLdl1 
          , AFT . A_Ldl1Semantics_Analyze_WrongLanguage  
          ) 
      ; LdlSemantics . InitLangInfo ( TEstRef , LdlLang , LLangInfo ) 
      ; LangMap . AddOrChange ( LbeStd . LangLdlAnalyzed , LLangInfo ) 
      ; LLangInfo . VersionNos := VersionNos ( LdlLang , TEstRef ) 
      ; Pass1 ( LLangInfo ) 
      ; BetweenPass1And2 ( LLangInfo ) 
      ; Pass2 ( LLangInfo ) 
      ; BetweenPass2And3 ( LLangInfo ) 
      ; Pass3 ( LLangInfo ) 
      ; BetweenPass3And4 ( LLangInfo ) 
      ; Pass4 ( LLangInfo ) 
      ; BetweenPass4And5 ( LLangInfo ) 
      ; LPreConcreteErrorCt 
          := Messages . MessageCount ( MessageCodes . KindTyp . MkError )  
      ; IF LPreConcreteErrorCt = 0 
        THEN 
          Pass5 ( LLangInfo ) 
        ; BetweenPass5And6 ( LLangInfo ) 
        ; Pass6 ( LLangInfo ) 
        ; AfterPass6 ( LLangInfo ) 
        END (* IF *) 
      ; MakeLangId ( LLangInfo ) 
      ; Messages . Flush ( )  
      ; RETURN LLangInfo 
      ELSE (* It could be a placeholder. *) 
        RETURN NIL 
      END (* TYPECASE *) 
    END Analyze 
 
; PROCEDURE Check ( ) 

  = <* FATAL AssertionFailure *> 
    BEGIN 
(* HMM.  Is this all overly pedantic?  These equalities will be ensured by
         the .ldl1 definition's using, in the relevant AS rule,  the 
         multiple "X | Y :: field ."  Form of rule.  Surely, this code is
         critically dependent on many other properties of the .ldl1 
         definition as well, far more that it would ever be feasible to
         assert here. *)  
      Assert 
        ( Ldl1Child . AsFixedRule_Parents = Ldl1Child . AsStarRule_Parents 
          AND Ldl1Child . AsFixedRule_Parents = Ldl1Child . AsPlusRule_Parents 
        , AFT . A_Ldl1Semantics_DifferentAsParents 
        ) 
      (* ^ Pass1 . MultiDecl depends on this equality. *) 
    ; Assert 
        ( Ldl1Child . FsFixedHorizRule_Parents 
          = Ldl1Child . FsFixedVertRule_Parents 
          AND Ldl1Child . FsFixedHorizRule_Parents 
              = Ldl1Child . FsFixedDefaultRule_Parents 
          AND Ldl1Child . FsFixedHorizRule_Parents 
              = Ldl1Child . FsFixedFillRule_Parents 

          AND Ldl1Child . FsFixedHorizRule_Parents 
              = Ldl1Child . FsListDefaultRule_Parents 
          AND Ldl1Child . FsFixedHorizRule_Parents 
              = Ldl1Child . FsListHorizRule_Parents 
          AND Ldl1Child . FsFixedHorizRule_Parents 
              = Ldl1Child . FsListVertRule_Parents 
          AND Ldl1Child . FsFixedHorizRule_Parents 
              = Ldl1Child . FsListFillRule_Parents 
          AND Ldl1Child . FsFixedHorizRule_Parents 
              = Ldl1Child . FsListTrailDefaultRule_Parents 
          AND Ldl1Child . FsFixedHorizRule_Parents 
              = Ldl1Child . FsListTrailHorizRule_Parents 
          AND Ldl1Child . FsFixedHorizRule_Parents 
              = Ldl1Child . FsListTrailVertRule_Parents 
          AND Ldl1Child . FsFixedHorizRule_Parents 
              = Ldl1Child . FsListTrailFillRule_Parents 

        , AFT . A_Ldl1Semantics_DifferentFsParents 
        ) 
      (* ^ Pass4 and Pass5 depend on this equality. *) 
    ; Assert 
        ( Ldl1Child . FsFixedHorizRule_Children 
          = Ldl1Child . FsFixedVertRule_Children 
          AND Ldl1Child . FsFixedHorizRule_Children 
              = Ldl1Child . FsFixedFillRule_Children 
          AND Ldl1Child . FsFixedHorizRule_Children 
              = Ldl1Child . FsFixedDefaultRule_Children 
        , AFT . A_Ldl1Semantics_DifferentFsFixedChildren 
        ) 
      (* ^ Pass1 and Pass2 depend on this equality. *) 
    ; Assert 
        ( Ldl1Child . FsListHorizRule_ListChild 
          = Ldl1Child . FsListVertRule_ListChild 
          AND Ldl1Child . FsListHorizRule_ListChild 
              = Ldl1Child . FsListFillRule_ListChild 
          AND Ldl1Child . FsListHorizRule_ListChild 
              = Ldl1Child . FsListDefaultRule_ListChild 

          AND Ldl1Child . FsListHorizRule_ListChild 
              = Ldl1Child . FsListTrailHorizRule_ListChild 
          AND Ldl1Child . FsListHorizRule_ListChild 
              = Ldl1Child . FsListTrailVertRule_ListChild 
          AND Ldl1Child . FsListHorizRule_ListChild 
              = Ldl1Child . FsListTrailFillRule_ListChild 
          AND Ldl1Child . FsListHorizRule_ListChild 
              = Ldl1Child . FsListTrailDefaultRule_ListChild 
        , AFT . A_Ldl1Semantics_DifferentFsListChild 
        ) 
      (* ^ Pass1 and Pass2 depend on this equality. *) 
    ; Assert 
        ( Ldl1Child . FsListHorizRule_Formatters 
          = Ldl1Child . FsListVertRule_Formatters 
          AND Ldl1Child . FsListHorizRule_Formatters 
              = Ldl1Child . FsListFillRule_Formatters 
          AND Ldl1Child . FsListHorizRule_Formatters 
              = Ldl1Child . FsListDefaultRule_Formatters 
          AND Ldl1Child . FsListHorizRule_Formatters 
              = Ldl1Child . FsListTrailHorizRule_Formatters 
          AND Ldl1Child . FsListHorizRule_Formatters 
              = Ldl1Child . FsListTrailVertRule_Formatters 
          AND Ldl1Child . FsListHorizRule_Formatters 
              = Ldl1Child . FsListTrailFillRule_Formatters 
          AND Ldl1Child . FsListHorizRule_Formatters 
              = Ldl1Child . FsListTrailDefaultRule_Formatters 
        , AFT . A_Ldl1Semantics_DifferentFsFormatters 
        ) 
      (* ^ Pass1 and Pass2 depend on this equality. *) 
    ; Assert 
        ( Ldl1Child . PrecLevelNone_Operators 
          = Ldl1Child . PrecLevelRight_Operators 
          AND Ldl1Child . PrecLevelNone_Operators 
              = Ldl1Child . PrecLevelLeft_Operators 
        , AFT . A_Ldl1Semantics_DifferentPrecLevelOperators 
        ) 
      (* ^InsertPrecAndAssoc depends in this equality. *) 
    ; Assert 
        ( Ldl1Child . CsStarRule_Lhs = Ldl1Child . CsFixedRule_Lhs 
          AND Ldl1Child . CsPlusRule_Lhs = Ldl1Child . CsFixedRule_Lhs 
          AND Ldl1Child . CsPluralRule_Lhs = Ldl1Child . CsFixedRule_Lhs 
          AND Ldl1Child . CsStarTrailRule_Lhs = Ldl1Child . CsFixedRule_Lhs 
          AND Ldl1Child . CsPlusTrailRule_Lhs = Ldl1Child . CsFixedRule_Lhs 
          AND Ldl1Child . CsPluralTrailRule_Lhs = Ldl1Child . CsFixedRule_Lhs 
          AND Ldl1Child . CsAltRule_Lhs = Ldl1Child . CsFixedRule_Lhs 
        , AFT . A_Ldl1Semantics_DifferentCsLhs 
        ) 
      (* ^ Pass5 . CsRule depends on this equality. 
           Also Pass6 . CsListRule needs part of it. 
      *) 
    ; Assert 
        ( Ldl1Child . CsStarRule_Separators 
          = Ldl1Child . CsPluralRule_Separators 
          AND Ldl1Child . CsStarRule_Separators 
              = Ldl1Child . CsPlusRule_Separators 
          AND Ldl1Child . CsStarRule_Separators 
              = Ldl1Child . CsStarTrailRule_Separators 
          AND Ldl1Child . CsStarRule_Separators 
              = Ldl1Child . CsPlusTrailRule_Separators 
          AND Ldl1Child . CsStarRule_Separators 
              = Ldl1Child . CsPluralTrailRule_Separators 
        , AFT . A_Ldl1Semantics_DifferentCsSeparators 
        ) 
    ; Assert 
        ( Ldl1Child . CsStarRule_ListChild = Ldl1Child . CsPluralRule_ListChild 
          AND Ldl1Child . CsStarRule_ListChild 
              = Ldl1Child . CsPlusRule_ListChild 
          AND Ldl1Child . CsStarRule_ListChild 
              = Ldl1Child . CsStarTrailRule_ListChild 
          AND Ldl1Child . CsStarRule_ListChild 
              = Ldl1Child . CsPlusTrailRule_ListChild 
          AND Ldl1Child . CsStarRule_ListChild 
              = Ldl1Child . CsPluralTrailRule_ListChild 
        , AFT . A_Ldl1Semantics_DifferentCsListChild 
        ) 
    (* ^ Pass6 . CsListRule depends in these equalities. *) 
    ; LdlSemantics . RegisterAnalyzer ( LbeStd . LangLdl1 , Analyze ) 
    END Check 

; BEGIN (* Ldl1Semantics *) 
    Check ( ) 
  END Ldl1Semantics 
. 
