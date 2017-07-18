
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Ldl0Semantics 

; IMPORT Date 
; IMPORT Fingerprint 
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
; IMPORT Ldl0Child 
; IMPORT Ldl0FsTrees 
; IMPORT Ldl0Tok 
; IMPORT LdlSemantics 
; IMPORT LRTable 
; FROM LRTable IMPORT NontermKindTyp
; IMPORT MessageCodes 
; IMPORT PortTypes 
; IMPORT SharedStrings 
; IMPORT TextIntSymbolTable 
; IMPORT TokRelation 
; IMPORT TravUtil 
; IMPORT UncertainBool 

; IMPORT Messages 

; FROM Messages IMPORT SemError , SemError2 , SemErrorText 

; TYPE AFT = MessageCodes . T 

; TYPE DeclKindTyp = LdlSemantics . DeclKindTyp 

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
              CantHappen ( AFT . A_Ldl0SemanticsClassFirstOcc_BadFirstCsOcc ) 
            ; RETURN NIL 
            END (* TYPECASE *) 
         END (* WITH *) 
      | LdlSemantics . SemAddlDefClassTyp ( TAddlDef ) 
      => WITH WDecl = LangInfo . SemMapRef ^ [ TAddlDef . RefDeclId ] 
         DO TYPECASE WDecl . SemRef 
            OF LdlSemantics . SemFirstOccClassTyp ( TSemDecl ) 
            => RETURN TSemDecl 
            ELSE 
              CantHappen ( AFT . A_Ldl0SemanticsClassFirstOcc_BadFirstClassOcc )
            ; RETURN NIL 
            END (* TYPECASE *) 
         END (* WITH *) 
      | LdlSemantics . SemAddlDefStringTyp ( TAddlDef ) 
      => WITH WDecl = LangInfo . SemMapRef ^ [ TAddlDef . RefDeclId ] 
         DO TYPECASE WDecl . SemRef 
            OF LdlSemantics . SemFirstOccStringTyp ( TSemDecl ) 
            => RETURN TSemDecl 
            ELSE 
              CantHappen ( AFT . A_Ldl0SemanticsClassFirstOcc_BadStringDef ) 
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
      => SemError ( SuperNodeNo , AFT . E_BadSuperReference ) 
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
(* TODO: Try to merge this with same-named proc in Ldl1Semantics. *)
  = BEGIN (* CsDeclKindMatchesAsDeclKind *) 
      CASE AsDeclKind 
      OF DeclKindTyp . AsClassRule 
      => RETURN TRUE 
      | DeclKindTyp . FixedRule 
      => RETURN CsDeclKind = DeclKindTyp . CsFixedRule 
      | DeclKindTyp . StarRule 
      => RETURN CsDeclKind = DeclKindTyp . CsStarRule 
      | DeclKindTyp . PlusRule 
      => RETURN CsDeclKind = DeclKindTyp . CsPlusRule 
         (* CsPluralRule can't happen in Ldl0. *) 
      ELSE 
        RETURN FALSE 
      END (* CASE *) 
    END CsDeclKindMatchesAsDeclKind 

; VAR GAsClassIsAlsoCsClass : BOOLEAN := TRUE 

; PROCEDURE Pass1 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Pass 1: 
       - Find the start and precedence rules and check that there is 
         at most one occurrence of each. 
       - Declare all As identifiers. 
       - Link up all additional occurrences of AsClass identifiers 
       - Declare all VARTERM rules. 
       - Count the VARTERM, AsFixed, AsStar, AsPlus, and AsClass identifiers.
       - Count the ListCard tokens needed.   
       - Locate the language name and check that it matches the name 
         at the end. 
       - Traverse all As, Fs and Cs rules, declaring all strings, 
         assigning tokens to strings, and linking up subsequent 
         occurrences of the same string. 
       - Fill in the SemMap for every Ast node. 
  *) 

  = PROCEDURE VisitRule ( RuleNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure } 

    = PROCEDURE VisitRhsNode ( Node : AstView . AstRefTyp ) 
      RAISES { AssertionFailure } 

      = VAR LWasFound : BOOLEAN 
      ; VAR LOldDeclNodeNo : LbeStd . EstNodeNoTyp 
      ; VAR LNewSem : LdlSemantics . SemAddlDefStringTyp 

      ; BEGIN (* VisitRhsNode *) 
          IF NOT Node . IsOptSingletonList
          THEN 
            LdlSemantics . MapAstRef ( LangInfo , Node ) 
          ; CASE EstUtil . EstTok ( Node . NodeRef ) 
            OF Ldl0Tok . String 
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
                ; LangInfo . SemMapRef ^ [ Node . NodeNo ] . SemRef := LNewSem 
                (* Link new LdlSemantics . SemAddlDefClassTyp onto the first-seen
                   LdlSemantics . SemFirstOccClassTyp *) 
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
                  ( LangInfo . TokCounts ^ [ TokClassTyp . TokClassConstTerm ] )
              ; INC 
                  ( LangInfo . TokPart ^ [ TokClassTyp . TokClassConstTerm ] )
              END (* IF *) 
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
          ; INC ( LangInfo . TokCounts ^ [ TokClassTyp . TokClassVarTerm ] )
          END (* IF *) 
        END VarTermRule 

    ; PROCEDURE MultiAsNodeDecl 
        ( DeclKind : DeclKindTyp 
        ; VAR (* IN OUT *) DeclCt : LbeStd . TokTyp 
        ; TokClass : TokClassTyp 
        ; Parents : AstView . AstRefTyp 
        ) 
      RAISES { AssertionFailure } 
      (* Pass 1 processing of any AS rule, with possibly multiple LHSs. *) 

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
                     ( LdlSemantics . SemDeclAsNodeTyp 
                     , NodeNo := DeclNode . NodeNo 
                     , DeclKind := DeclKind 
                     , DeclRuleNodeNo := RuleNode . NodeNo 
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
                    , 3 (* Emmpty, Singleton and Plural card tokens. *) 
                    )

              | DeclKindTyp . PlusRule 
              => INC ( LangInfo . AsListCardToks 
                     , 2 (* Singleton and Plural card tokens. *) 
                     ) 
              ; INC ( LangInfo . TokCounts ^ [ TokClassTyp . TokClassListCard ]
                    , 2 (* Singleton and Plural card tokens. *) 
                    ) 
              END (* CASE *) 
            END (* IF *) 
          END VisitAsNodeDecl 

      ; <* FATAL ANY *> 
        BEGIN (* MultiAsNodeDecl *) 
          AstView . TraverseChildren ( Parents , VisitAsNodeDecl ) 
        END MultiAsNodeDecl 

    ; PROCEDURE AsClassDecl ( DeclNode : AstView . AstRefTyp ) 

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
            (* Link new LdlSemantics . SemAddlDefClassTyp onto the first-seen 
               LdlSemantics . SemFirstOccClassTyp *) 
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
          ; IF GAsClassIsAlsoCsClass 
            THEN 
              INC ( LangInfo . AsCsClassToks ) 
            ; INC ( LangInfo . TokCounts ^ [ TokClassTyp . TokClassAsCsClass ] )
            ELSE 
              INC ( LangInfo . AsClassOnlyToks ) 
            ; INC ( LangInfo . TokCounts ^ [ TokClassTyp . TokClassAsClass ] )
            END (* IF *)
          END (* IF *) 
        END AsClassDecl 

    ; BEGIN (* VisitRule *) 
        VAR LChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 

      ; BEGIN (* Block *) 
          IF RuleNode . NodeRef # NIL 
          THEN 
            LdlSemantics . MapAstRef ( LangInfo , RuleNode ) 
          ; AstView . GetChildren 
              ( RuleNode , LChildren , LangInfo . LdlLang ) 
          ; LdlSemantics . MapAstRefs ( LangInfo , LChildren ) 
          ; CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
            OF Ldl0Tok . StartRule 
            => IF LangInfo . StartRule . NodeRef = NIL 
               THEN 
                 LangInfo . StartRule := RuleNode 
               ELSE 
                 SemError ( RuleNode . NodeNo , AFT . E_MultipleStartRules ) 
               END (* IF *) 
            ; VisitRhsNode ( LChildren [ Ldl0Child . StartRule_StartName ] ) 
            | Ldl0Tok . PrecRule 
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
                  ( LChildren [ Ldl0Child . PrecRule_Levels ] , VisitRhsNode ) 
              END (* Block *) 
            | Ldl0Tok . AsVarTermRule 
            => VarTermRule ( LChildren [ Ldl0Child . AsVarTermRule_Name ] ) 
            | Ldl0Tok . AsFixedRule 
            => MultiAsNodeDecl 
                 ( DeclKindTyp . FixedRule 
                 , LangInfo . AsFixedToks 
                 , TokClassTyp . TokClassAsFixed 
                 , LChildren [ Ldl0Child . AsFixedRule_Parents ] 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . AsFixedRule_Children ] 
                  , VisitRhsNode 
                  ) 
              END (* Block *) 
            | Ldl0Tok . AsStarRule 
            => MultiAsNodeDecl 
                 ( DeclKindTyp . StarRule 
                 , LangInfo . AsStarToks 
                 , TokClassTyp . TokClassAsStar 
                 , LChildren [ Ldl0Child . AsStarRule_Parents ] 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . AsStarRule_ChildName ] 
                  , VisitRhsNode 
                  ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . AsStarRule_ChildClass ] 
                  , VisitRhsNode 
                  ) 
              END (* Block *) 
            | Ldl0Tok . AsPlusRule 
            => MultiAsNodeDecl 
                 ( DeclKindTyp . PlusRule 
                 , LangInfo . AsPlusToks 
                 , TokClassTyp . TokClassAsPlus 
                 , LChildren [ Ldl0Child . AsPlusRule_Parents ] 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . AsPlusRule_ChildName ] 
                  , VisitRhsNode 
                  ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . AsPlusRule_ChildClass ] 
                  , VisitRhsNode 
                  ) 
              END (* Block *) 
            | Ldl0Tok . AsClassRule 
            => AsClassDecl ( LChildren [ Ldl0Child . AsClassRule_ClassName ] ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . AsClassRule_ClassMembers ] 
                  , VisitRhsNode 
                  ) 
              END (* Block *) 
            | Ldl0Tok . CsAltRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . CsAltRule_Alternatives ] 
                  , VisitRhsNode 
                  ) 
              END (* Block *) 
            | Ldl0Tok . CsFixedRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . CsFixedRule_Rhs ] , VisitRhsNode ) 
              END (* Block *) 
            | Ldl0Tok . CsStarRule , Ldl0Tok . CsPlusRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . CsStarRule_ListChild ] 
                  , VisitRhsNode 
                  ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . CsStarRule_Separators ] 
                  , VisitRhsNode 
                  ) 
              END (* Block *) 
            | Ldl0Tok . FsFixedDefaultRule 
            , Ldl0Tok . FsFixedHorizRule 
            , Ldl0Tok . FsFixedVertRule 
            , Ldl0Tok . FsFixedFillRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . FsFixedHorizRule_Children ] 
                  , VisitRhsNode 
                  ) 
              END (* Block *) 
            | Ldl0Tok . FsListHorizRule 
            , Ldl0Tok . FsListDefaultRule 
            , Ldl0Tok . FsListVertRule 
            , Ldl0Tok . FsListFillRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . FsListHorizRule_ListChild ] 
                  , VisitRhsNode 
                  ) 
                ; AstView . TraverseTree 
                    ( LChildren [ Ldl0Child . FsListHorizRule_Formatters ] 
                    , VisitRhsNode 
                    ) 
              END (* Block *) 
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END (* Block *) 
      END VisitRule 

  ; BEGIN (* Pass1 *) 
      VAR LRootChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        LangInfo . SemMapRef ^ [ LangInfo . Root . NodeNo ] . EstRef 
          := LangInfo . Root . NodeRef 
      ; AstView . GetChildren 
          ( LangInfo . Root , LRootChildren , LangInfo . LdlLang ) 
      ; LdlSemantics . MapAstRefs ( LangInfo , LRootChildren ) 
      ; LangInfo . LanguageName 
          := LRootChildren [ Ldl0Child . LanguageDefinition_LanguageName ] 
             . NodeRef 
      ; IF SharedStrings . StringNo ( LangInfo . LanguageName ) 
           # SharedStrings . StringNo 
               ( LRootChildren 
                   [ Ldl0Child . LanguageDefinition_ClosingName ] . NodeRef 
               ) 
        THEN 
          SemError 
            ( LRootChildren [ Ldl0Child . LanguageDefinition_ClosingName ] 
              . NodeNo 
            , AFT . E_LanguageClosingNameMismatch 
            ) 
        END (* IF *) 
      ; <* FATAL ANY *>  
        BEGIN 
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl0Child . LanguageDefinition_Rules ] 
            , VisitRule 
            ) 
        END (* Block *) 
      END (* Block *) 
    END Pass1 

; PROCEDURE BetweenPass1And2 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  (* Between passes 1 and 2: 
     - Rebias counts of varterm, abstract, and generated list tokens to prepare
       for assigning actual token values in pass 2.  Varterms get two 
       tokens each.  Partial and sublist toks are one-for-one for list toks.
     - Create an initialized token map big enough for the 
       string, varterm, abstract, and AsSublist tokens. 
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
    ; LTokCt (* Total for string, varterm, AsSublist, and all abstract 
                tokens. *) 
        := LangInfo . StringToks 
           + LangInfo . VarTermToks * 2 
           + LangInfo . AsPlusToks 
           + LangInfo . AsStarToks 
           + LangInfo . AsFixedToks 
           + LangInfo . AsSublistToks 
           + LangInfo . AsListCardToks 
           + LangInfo . AsPartialToks 
           + LangInfo . AsClassOnlyToks 
           + LangInfo . AsCsClassToks (* Currently, this is zero. *)  
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
       - Generate sublist, partial, and list cardinality tokens.  
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
            OF Ldl0Tok . String 
            => TYPECASE LangInfo . SemMapRef ^ [ Node . NodeNo ] . SemRef 
               OF LdlSemantics . SemFirstOccStringTyp ( TSemRef ) 
               => LangInfo . TokMapRef ^ 
                  [ TSemRef . DeclTok - LdlSemantics . TokMapBias ] 
                    := Node . NodeNo 
               ; LangInfo . StringMapRef ^ 
                 [ TSemRef . DeclTok - LdlSemantics . StringMapBias ] 
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
          ; LangInfo . TokMapRef  
              ^ [ TDecl . SublistTok - LdlSemantics . TokMapBias ] 
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
            ; INC ( LangInfo . TokPart ^ [ TokClassTyp . TokClassListCard ] )
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
            ; INC ( LangInfo . TokPart ^ [ TokClassTyp . TokClassListCard ] )
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
          ; INC ( LangInfo . TokPart ^ [ TokClassTyp . TokClassListCard ] )

          (* Assign partial token. *) 
          ; TDecl . PartialTok := TDecl . DeclTok + LangInfo . AsPartialToks  
          ; LangInfo . TokMapRef  
              ^ [ TDecl . PartialTok - LdlSemantics . TokMapBias ] 
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

    ; PROCEDURE AsClassRule ( ClassName : AstView . AstRefTyp ) 
      RAISES { AssertionFailure } 

      = BEGIN (* AsClassRule *) 
          TYPECASE LangInfo . SemMapRef ^ [ ClassName . NodeNo ] . SemRef 
          OF LdlSemantics . SemFirstOccClassTyp 
          => IF GAsClassIsAlsoCsClass 
            THEN 
              AsNodeDecl 
                ( ClassName 
                , (* IN OUT *) LangInfo . AsCsClassToks 
                , TokClassTyp . TokClassAsCsClass 
                ) 
            ELSE 
              AsNodeDecl 
                ( ClassName 
                , (* IN OUT *) LangInfo . AsClassOnlyToks 
                , TokClassTyp . TokClassAsClass 
                ) 
            END (* IF *) 
          | LdlSemantics . SemAddlDefTyp ( TSemDef ) 
          => TSemDef . RefTok := FirstOcc ( LangInfo , TSemDef ) . DeclTok 
          ELSE 
            CantHappen ( AFT . A_Ldl0SemanticsPass2VisitRule_BadClassAttribute )
          END (* TYPECASE *) 
        END AsClassRule 

    ; BEGIN (* VisitRule *) 
        VAR LChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 

      ; BEGIN (* Block *) 
          IF RuleNode . NodeRef # NIL 
          THEN 
            AstView . GetChildren 
              ( RuleNode , LChildren , LangInfo . LdlLang ) 
          ; CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
            OF Ldl0Tok . AsFixedRule 
            => MultiAsNodeDecl 
                 ( LChildren [ Ldl0Child . AsFixedRule_Parents ] 
                 , LangInfo . AsFixedToks 
                 , TokClassTyp . TokClassAsFixed
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . AsFixedRule_Children ] 
                  , VisitMaybeString 
                  ) 
              END (* Block *) 
            | Ldl0Tok . AsVarTermRule 
            => AsNodeDecl 
                 ( LChildren [ Ldl0Child . AsVarTermRule_Name ] 
                 , LangInfo . VarTermToks 
                 , TokClassTyp . TokClassVarTerm 
                 ) 
            ; INC ( LangInfo . VarTermModToks ) 
            ; INC ( LangInfo . TokPart ^ [ TokClassTyp . TokClassVarTermMod ] )
              (* Extra token value for a ModTok subtree. *) 
            | Ldl0Tok . AsStarRule 
            => MultiAsNodeDecl 
                 ( LChildren [ Ldl0Child . AsStarRule_Parents ] 
                 , LangInfo . AsStarToks 
                 , TokClassTyp . TokClassAsStar 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . AsStarRule_ChildClass ] 
                  , VisitMaybeString 
                  ) 
              END (* Block *) 
            | Ldl0Tok . AsPlusRule 
            => MultiAsNodeDecl 
                 ( LChildren [ Ldl0Child . AsPlusRule_Parents ] 
                 , LangInfo . AsPlusToks 
                 , TokClassTyp . TokClassAsPlus 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . AsPlusRule_ChildClass ] 
                  , VisitMaybeString 
                  ) 
              END (* Block *) 
            | Ldl0Tok . AsClassRule 
            => AsClassRule ( LChildren [ Ldl0Child . AsClassRule_ClassName ] ) 
            | Ldl0Tok . CsAltRule 
            => IF AstView . ChildCt 
                    ( LChildren [ Ldl0Child . CsAltRule_Alternatives ] , 2 ) 
                  = 1 
(* YUCK: *) 
              THEN (* This terrible kludge is a workaround for the fact that
                      Ldl0 has an ambiguity between a CsFixedRule and a
                      CsAltRule with exactly one RHS member.  This is very
                      hard to fix with Ldl0 being defined in Ldl0, for either
                      handwritten CS rules or for a grammar generated from 
                      FS rules.  Lalr's state repair happens to be making 
                      the choice we want for handwritten, but not for 
                      generated.  We need a CsAltRule to have at least two 
                      alternatives.  So we patch a singleton CsAltRule into 
                      a CsFixedRule here, and its child to a Concatenation,
                      if it was not singleton-optimized.   
                   *) 
                NARROW ( RuleNode . NodeRef , EstHs . EstRefTyp ) . EstTok 
                  := Ldl0Tok . CsFixedRule 
              ; WITH WAlternatives 
                     = LChildren [ Ldl0Child . CsAltRule_Alternatives ]
                DO 
                  IF NOT WAlternatives . IsOptSingletonList 
                  THEN
                    NARROW ( WAlternatives . NodeRef , EstHs . EstRefTyp ) 
                      . EstTok 
                      := Ldl0Tok . Concatenation
                  END (* IF *) 
                END (* WITH *) 
              ; SemError 
                  ( RuleNode . NodeNo  
                  , AFT . I_Changed_singleton_CsAltRule_to_CsFixedRule
                  ) 
              END (* IF *)  
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . CsAltRule_Alternatives ] 
                  , VisitMaybeString 
                  ) 
              END (* Block *) 
            | Ldl0Tok . CsFixedRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . CsFixedRule_Rhs ] 
                  , VisitMaybeString 
                  ) 
              END (* Block *) 
            | Ldl0Tok . CsStarRule , Ldl0Tok . CsPlusRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . CsStarRule_ListChild ] 
                  , VisitMaybeString 
                  ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . CsStarRule_Separators ] 
                  , VisitMaybeString 
                  ) 
              END (* Block *) 
            | Ldl0Tok . FsFixedDefaultRule 
            , Ldl0Tok . FsFixedHorizRule 
            , Ldl0Tok . FsFixedVertRule 
            , Ldl0Tok . FsFixedFillRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . FsFixedHorizRule_Children ] 
                  , VisitMaybeString 
                  ) 
              END (* Block *) 
            | Ldl0Tok . FsListDefaultRule 
            , Ldl0Tok . FsListHorizRule 
            , Ldl0Tok . FsListVertRule 
            , Ldl0Tok . FsListFillRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . FsListHorizRule_ListChild ] 
                  , VisitMaybeString 
                  ) 
              ; AstView . TraverseTree 
                  ( LChildren [ Ldl0Child . FsListHorizRule_Formatters ] 
                  , VisitMaybeString 
                  ) 
              END (* Block *) 
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END (* Block *) 
      END VisitRule 

  ; BEGIN (* Pass2 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl0Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        P2VarTermTokCt := LangInfo . VarTermModToks - LangInfo . VarTermToks 
      ; AstView . GetChildren 
          ( LangInfo . Root , LRootChildren , LangInfo . LdlLang ) 
      ; <* FATAL ANY *> 
        BEGIN 
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl0Child . LanguageDefinition_Rules ] 
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
               OF Ldl0Tok . String 
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
               | Ldl0Tok . Ident 
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
                       ( AFT . A_Ldl0SemanticsPass3VisitRuleAsClassRuleVisitMember_NotDeclKindTyp 
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
            CantHappen ( AFT . A_Ldl0SemanticsPass3VisitRule_BadClassAttribute )
          END (* TYPECASE *) 
        END AsClassRule 

    ; BEGIN (* VisitRule *) 
        VAR LChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 

      ; BEGIN (* Block *) 
          IF RuleNode . NodeRef # NIL 
          THEN 
            AstView . GetChildren 
              ( RuleNode , LChildren , LangInfo . LdlLang ) 
          ; CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
            OF Ldl0Tok . AsClassRule 
            => AsClassRule 
                 ( LChildren [ Ldl0Child . AsClassRule_ClassName ] 
                 , LChildren [ Ldl0Child . AsClassRule_ClassMembers ] 
                 ) 
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END (* Block *) 
      END VisitRule 

  ; BEGIN (* Pass3 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl0Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        AstView . GetChildren 
          ( LangInfo . Root , LRootChildren , LangInfo . LdlLang ) 
      ; <* FATAL ANY *>  
        BEGIN 
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl0Child . LanguageDefinition_Rules ] 
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

; PROCEDURE Pass4 ( VAR LangInfo : LdlSemantics . LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Pass 4: 
       - Use ClassRelation to compute token sets of As classes and 
         store them in the LdlSemantics . SemFirstOccClassTyp node.  If the 
         class is a singleton, note this. Do all this the first time we 
         encounter either the class rule, or a reference thereto.  This obviates
         doing it in yet another pass. 
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

  ; PROCEDURE StartRule ( StartIdNode : AstView . AstRefTyp ) 

    = VAR LWasFound : BOOLEAN 
    ; VAR LDeclNodeNo : LbeStd . EstNodeNoTyp 

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
             OF LdlSemantics . SemDeclAsNodeTyp ( TDeclSemRef ) 
             => (* A known abstract node. *) 
                LangInfo . StartIdentNodeNo := LDeclNodeNo 
             ; LangInfo . SemMapRef ^ [ StartIdNode . NodeNo ] . SemRef 
                 := NEW 
                      ( LdlSemantics . SemRefTyp 
                      , NodeNo := StartIdNode . NodeNo 
                      , RefDeclId := LDeclNodeNo 
                      , RefTok := TDeclSemRef . DeclTok 
                      ) 
             ; LangInfo . StartTok := TDeclSemRef . DeclTok 
             | LdlSemantics . SemFirstOccClassTyp ( TFirstOccClass ) 
             => (* A known class. *) 
                LazyComputeTokSet ( TFirstOccClass ) 
             ; IF TFirstOccClass . SingletonTok = LbeStd . Tok__Null 
               THEN 
                 SemError 
                   ( StartIdNode . NodeNo 
                   , AFT . E_OnlyASingleStartSymbolIsAllowed 
                   ) 
               ELSE 
                 LangInfo . StartIdentNodeNo 
                   := LangInfo . TokMapRef ^ 
                        [ TFirstOccClass . SingletonTok 
                          - LdlSemantics . TokMapBias 
                        ] 
               ; LangInfo . SemMapRef ^ [ StartIdNode . NodeNo ] . SemRef 
                   := NEW 
                        ( LdlSemantics . SemRefTyp 
                        , NodeNo := StartIdNode . NodeNo 
                        , RefDeclId := LangInfo . StartIdentNodeNo 
                        , RefTok := TFirstOccClass . SingletonTok 
                        ) 
               ; LangInfo . StartTok := TFirstOccClass . SingletonTok 
               END (* IF *) 
             ELSE 
               SemError 
                 ( StartIdNode . NodeNo 
                 , AFT . E_StartSymbolNotAnAbstractOrStringToken 
                 ) 
             END (* TYPECASE *) 
          END (* WITH *) 
        ELSE 
          SemError ( StartIdNode . NodeNo , AFT . E_UndeclaredStartSymbol ) 
        END (* IF *) 
      END StartRule 

  ; PROCEDURE VisitRule ( RuleNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure } 

    = VAR EstChildNo : LbeStd . EstChildNoTyp 

    ; PROCEDURE AsChildClass ( ChildClassNode : AstView . AstRefTyp ) 

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
            TextIntSymbolTable . FindOrAdd 
              ( LangInfo . SymbolTable 
              , SharedStrings . ToText ( ChildClassNode . NodeRef ) 
              , ChildClassNode . NodeNo 
              , LWasFound 
              , LDeclNodeNo 
              ) 
          ; IF LWasFound 
            THEN 
              WITH 
                WDeclSemRef = LangInfo . SemMapRef ^ [ LDeclNodeNo ] . SemRef 
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
                 END (* TYPECASE  CASE *) 
              END (* WITH *) 
            ELSE 
              SemError 
                ( ChildClassNode . NodeNo , AFT . E_UndeclaredAbstractChild ) 
            END (* IF *) 
          ; INC ( EstChildNo ) 
          END (* Block *) 
        END AsChildClass 

    ; PROCEDURE AsChild ( ChildNode : AstView . AstRefTyp ) 
      RAISES { AssertionFailure } 

      = VAR LChildren : ARRAY [ 0 .. 1 ] OF AstView . AstRefTyp 

      ; BEGIN (* AsChild *) 
          AstView . GetChildren ( ChildNode , LChildren , LangInfo . LdlLang ) 
        ; CASE EstUtil . EstTok ( ChildNode . NodeRef ) 
          OF Ldl0Tok . AsReqdChild , Ldl0Tok . AsOptChild 
          => AsChildClass ( LChildren [ Ldl0Child . AsReqdChild_ChildClass ] ) 
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
          ; LSet 
              := IntSets . Project 
                   ( TFirstOcc . TokSet 
                   , LbeStd . Tok__BegOfImage 
                   , LangUtilLo . TokClassLastTok ( LangInfo , TokClassAsFixed )
                   ) 
          ; IF GAsClassIsAlsoCsClass 
            THEN 
              INC ( LangInfo . Productions , IntSets . Card ( LSet ) ) 
            END (* IF *) 
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
          (* CsPluralRule is inside this range, but can't happen in Ldl0. *)  
        ; VAR (* IN OUT *) DeclCt : LbeStd . TokTyp 
        ; TokClass : TokClassTyp 
        ; IdNode : AstView . AstRefTyp 
        ; RhsNode : AstView . AstRefTyp 
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
                OF Ldl0Tok . Concatenation 
                => IF OptionCt - 1 >= LAST ( LRTable . OptionIdRealTyp ) 
                   THEN 
                     SemError2 
                       ( RuleNode . NodeNo 
                       , Node . NodeNo 
                       , AFT . E_TooManyOptionalSubstrings 
                       ) 
                   ELSE 
                     INC ( OptionCt ) 
                   END (* IF *) 
                ELSE 
                  INC ( RhsCt ) 
                END (* CASE *) 
              END (* IF *) 
            END VisitCsFixedDescendent 

        ; BEGIN (* TraverseRhs *) 
            IF DeclKind = DeclKindTyp . CsFixedRule 
            THEN 
              OptionCt := 0 
            ; RhsCt := 0 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseTree ( RhsNode , VisitCsFixedDescendent ) 
              END (* Block *) 
            ; OptionCt := MAX ( 0 , OptionCt - 1 ) 
              (* This is rather crude, but sure easier to code. The fixed 
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
        ; VAR LNewSemAddlDefCs : LdlSemantics . SemAddlDefCsTyp 
        ; VAR LNewSemFirstOccCs : LdlSemantics . SemFirstOccCsTyp 

        ; BEGIN (* Block for CsRule *) 
            TextIntSymbolTable . FindOrAdd 
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
                 => (* A building rule for a known abstract node. *) 
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
                 ; LNewSemAddlDefCs 
                     := NEW 
                          ( LdlSemantics . SemAddlDefCsTyp 
                          , NodeNo := IdNode . NodeNo 
                          , RefDeclId := LOldDeclNodeNo 
                          , RefTok := TOldSemRef . DeclTok 
                          ) 
                 ; LangInfo . SemMapRef ^ [ IdNode . NodeNo ] . SemRef 
                     := LNewSemAddlDefCs 
                 ; TraverseRhs 
                     ( (* VAR *) LNewSemAddlDefCs . OptionCt 
                     , (* VAR *) LNewSemAddlDefCs . RhsCt 
                     )
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
                 ; TraverseRhs 
                     ( (* VAR *) LNewSemAddlDefCs . OptionCt 
                     , (* VAR *) LNewSemAddlDefCs . RhsCt 
                     )
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
                   ; TYPECASE LangInfo . SemMapRef ^ [ LDeclIdNodeNo ] . SemRef 
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
                         , AFT . E_ConcreteIdentAlreadyDeclaredAndNotAbstract 
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
                 ; TraverseRhs 
                     ( (* VAR *) LNewSemAddlDefCs . OptionCt 
                     , (* VAR *) LNewSemAddlDefCs . RhsCt 
                     ) 
                 ELSE 
                   SemError2 
                     ( IdNode . NodeNo 
                     , LOldDeclNodeNo 
                     , AFT . E_ConcreteIdentAlreadyDeclaredAndNotAbstract 
                     ) 
                 END (* TYPECASE  CASE *) 
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
            ; TraverseRhs 
                ( (* VAR *) LNewSemFirstOccCs . OptionCt 
                , (* VAR *) LNewSemFirstOccCs . RhsCt 
                ) 
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
              WITH 
                WDeclSemRef = LangInfo . SemMapRef ^ [ DeclNodeNo ] . SemRef 
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
              ( RuleNode , LChildren , LangInfo . LdlLang ) 
          ; CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
            OF Ldl0Tok . StartRule 
            => StartRule ( LChildren [ Ldl0Child . StartRule_StartName ] ) 
            | Ldl0Tok . AsFixedRule 
            => EstChildNo := 0 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseChildren 
                  ( LChildren [ Ldl0Child . AsFixedRule_Children ] 
                  , Visit := AsChild 
                  ) 
              END (* Block *) 
            | Ldl0Tok . AsStarRule 
            => EstChildNo := 0 
            ; AsChildClass ( LChildren [ Ldl0Child . AsStarRule_ChildClass ] ) 
            | Ldl0Tok . AsPlusRule 
            => EstChildNo := 0 
            ; AsChildClass ( LChildren [ Ldl0Child . AsPlusRule_ChildClass ] ) 
            | Ldl0Tok . AsClassRule 
            => AsClassRule ( LChildren [ Ldl0Child . AsClassRule_ClassName ] ) 
            | Ldl0Tok . CsFixedRule 
            => CsRule 
                 ( DeclKindTyp . CsFixedRule 
                 , LangInfo . CsFixedToks 
                 , TokClassTyp . TokClassCsFixed 
                 , LChildren [ Ldl0Child . CsFixedRule_Lhs ] 
                 , LChildren [ Ldl0Child . CsFixedRule_Rhs ] 
                 ) 
            | Ldl0Tok . CsStarRule 
            => CsRule 
                 ( DeclKindTyp . CsStarRule 
                 , LangInfo . CsStarToks 
                 , TokClassTyp . TokClassCsStar 
                 , LChildren [ Ldl0Child . CsStarRule_Lhs ] 
                 , AstView . AstRefNull 
                 ) 
            ; INC ( LangInfo . Productions , 4 ) 
            | Ldl0Tok . CsPlusRule 
            => CsRule 
                 ( DeclKindTyp . CsPlusRule 
                 , LangInfo . CsPlusToks 
                 , TokClassTyp . TokClassCsPlus 
                 , LChildren [ Ldl0Child . CsPlusRule_Lhs ] 
                 , AstView . AstRefNull 
                 ) 
            ; INC ( LangInfo . Productions , 3 ) 
            | Ldl0Tok . CsAltRule 
            => CsRule 
                 ( DeclKindTyp . CsAltRule 
                 , LangInfo . CsAltToks 
                 , TokClassTyp . TokClassCsClass 
                 , LChildren [ Ldl0Child . CsAltRule_Lhs ] 
                 , AstView . AstRefNull 
                 ) 
            ; <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseChildren 
                  ( LChildren [ Ldl0Child . CsAltRule_Alternatives ] 
                  , VisitCsAlternative 
                  ) 
              END (* Block *) 
            | Ldl0Tok . FsFixedDefaultRule 
            , Ldl0Tok . FsFixedHorizRule 
            , Ldl0Tok . FsFixedVertRule 
            , Ldl0Tok . FsFixedFillRule 
            , Ldl0Tok . FsListDefaultRule 
            , Ldl0Tok . FsListHorizRule 
            , Ldl0Tok . FsListVertRule 
            , Ldl0Tok . FsListFillRule 
            => <* FATAL ANY *>  
              BEGIN 
                AstView . TraverseChildren 
                  ( LChildren [ Ldl0Child . FsFixedHorizRule_Parents ] 
                    (* NOTE! The above hack depends on 
                       Ldl0Tok . Fs[Fixed|List][Horiz|Vert|Fill]Rule_Parents 
                       all being equal *) 
                  , VisitFsLhs 
                  ) 
              END (* Block *) 
            ELSE 
            END (* CASE *) 
          END (* IF *) 
        END (* Block *) 
      END VisitRule 

  ; BEGIN (* Pass4 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl0Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        AstView . GetChildren 
          ( LangInfo . Root , LRootChildren , LangInfo . LdlLang ) 
      ; <* FATAL ANY *>  
        BEGIN 
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl0Child . LanguageDefinition_Rules ] 
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
                , AFT . E_Precedence_Rule_Operator_Must_Be_A_Declared_Single_Token 
                ) 
            END (* IF *) 
          END VoMember 

      ; BEGIN (* VisitOperator *) 
          TYPECASE <* NOWARN *>
            LdlSemantics . DeclOfRef ( LangInfo , OperatorNode ) 
          OF NULL => 
          | LdlSemantics . SemFirstOccStringTyp ( TDeclSemRef ) 
          => (* A string. *) 
             VoMember ( TDeclSemRef . DeclTok ) 
          | LdlSemantics . SemFirstOccClassTyp ( TFirstOccClass ) 
          => (* A known class. *) 
            IF TFirstOccClass . SingletonTok # LbeStd . Tok__Null 
            THEN VoMember ( TFirstOccClass . SingletonTok ) 
            ELSE (* A plural class rule. *) 
              IntSets . ForAllDo ( TFirstOccClass . TokSet , VoMember ) 
            END (* IF *) 
          ELSE 
          END (* TYPECASE *) 
        END VisitOperator 

    ; BEGIN (* VisitLevel *) 
        CASE EstUtil . EstTok ( LevelNode . NodeRef ) <* NOWARN *>
        OF Ldl0Tok . PrecLevelNone 
        => LevAssoc := LRTable . AssocTyp . nonassoc 
        | Ldl0Tok . PrecLevelRight 
        => LevAssoc := LRTable . AssocTyp . right 
        | Ldl0Tok . PrecLevelLeft 
        => LevAssoc := LRTable . AssocTyp . left 
        END (* CASE *) 
      ; <* FATAL ANY *> 
        BEGIN 
          AstView . TraverseChildren 
            ( AstView . Child 
                ( LevelNode 
                , Ldl0Child . PrecLevelNone_Operators 
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
                ( LangInfo . PrecRule , Ldl0Child . PrecRule_Levels 
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
      DO Ldl0FsTrees . BuildFixedTerm ( LangInfo , RTok ) 
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
(* Setting nonterminal kind for As class tokens will be redone in Pass 7, when
   we know whether they are used as concrete or not. *) 
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
                 ( RuleNode , Ldl0Child . AsVarTermRule_Name 
                 , LangInfo . LdlLang ) 
        ; TYPECASE LangInfo . SemMapRef ^ [ LIdentNode . NodeNo ] . SemRef 
          OF LdlSemantics . SemDeclTyp ( TDeclSemRef ) 
          => Ldl0FsTrees . BuildVarTerm 
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
                   , AFT . E_ListNodeHasMultipleConcreteRules
                   ) 
               END (* IF *) 
            ELSE 
            END 
          END VisitAsNodeRule 

      ; BEGIN (* MultiAsNodeRule *) 
          VAR LChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 

        ; BEGIN (* Block *) 
            AstView . GetChildren 
              ( RuleNode , LChildren , LangInfo . LdlLang ) 
          ; <* FATAL ANY *> 
            BEGIN
              AstView . TraverseChildren 
                ( AstView . Child 
                    ( RuleNode , Ldl0Child . AsFixedRule_Parents 
                    , LangInfo . LdlLang 
                    ) 
                , VisitAsNodeRule 
                ) 
            (* NOTE! The above hack depends on Ldl0Child . AsFixedRule_Parents 
                     = AsStarRule_Parents = AsPlusRule_Parents *) 
            END (* Block. *) 
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
                 ( RuleNode , Ldl0Child . CsFixedRule_Lhs , LangInfo . LdlLang )
            (* ^This depends on CsFixedRule_Lhs, CsStarRule_Lhs, 
                CsPlusRule_Lhs, and CsAltRule_Lhs all being equal. *) 
        ; WITH WSemRef = LangInfo . SemMapRef ^ [ LIdNode . NodeNo ] . SemRef 
          DO TYPECASE WSemRef 
             OF NULL 
             => 

             | LdlSemantics . SemFirstOccCsTyp ( TFirstOcc ) (* Assign a tok. *)
             => TFirstOcc . DeclTok := DeclCt 
             ; LangInfo . TokMapRef ^ [ DeclCt - LdlSemantics . TokMapBias ] 
                 := LIdNode . NodeNo 
             ; INC ( DeclCt ) 
             ; INC ( LangInfo . TokPart ^ [ TokClass ] )  

             | LdlSemantics . SemAddlDefTyp ( TAddlDef ) 
             => TAddlDef . RefTok := FirstOcc ( LangInfo , TAddlDef ) . DeclTok 
               (* ^Copy the token from 1st occ. *) 
             ELSE (* Is an abstract node. *) 
             END (* TYPECASE *) 
          END (* WITH *) 
        END CsRule 

    ; PROCEDURE MultiFsRule ( ) 
      RAISES { AssertionFailure } 

      = PROCEDURE VisitFsIdent ( RefNode : AstView . AstRefTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* VisitFsIdent *) 
            TYPECASE LangInfo . SemMapRef ^ [ RefNode . NodeNo ] . SemRef 
            OF NULL => 
            | LdlSemantics . SemRefTyp ( TFsIdentSemRef ) 
            => TYPECASE 
                 LangInfo . SemMapRef ^ [ TFsIdentSemRef . RefDeclId ] . SemRef 
               OF NULL => 
               | LdlSemantics . SemDeclAsNodeTyp ( TAsIdentSemRef ) 
               => LangInfo . FsTreeMapRef ^ 
                    [ TAsIdentSemRef . DeclTok - LbeStd . Tok__FirstLangDep ] 
                    := Ldl0FsTrees . Build 
                         ( LangInfo 
                         , TFsIdentSemRef 
                         , TAsIdentSemRef 
                         , IsStart 
                             := TAsIdentSemRef . NodeNo 
                                = LangInfo . StartIdentNodeNo 
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
                ( RuleNode , Ldl0Child . FsFixedHorizRule_Parents 
                , LangInfo . LdlLang ) 
            , VisitFsIdent 
            ) 
        (* NOTE! The above hack depends on 
           Ldl0Tok . Fs[Fixed|List][Horiz|Vert|Fill]Rule_Parents 
           all being equal *) 
        END MultiFsRule 

    ; BEGIN (* VisitRule *) 
        IF RuleNode . NodeRef # NIL 
        THEN 
          CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
          OF Ldl0Tok . AsVarTermRule 
          => VarTerm ( ) 
          | Ldl0Tok . AsFixedRule , Ldl0Tok . AsStarRule , Ldl0Tok . AsPlusRule 
          => MultiAsNodeRule ( ) 
          | Ldl0Tok . CsFixedRule 
          => CsRule ( LangInfo . CsFixedToks , TokClassTyp . TokClassCsFixed ) 
          | Ldl0Tok . CsStarRule 
          => CsRule ( LangInfo . CsStarToks , TokClassTyp . TokClassCsStar ) 
          | Ldl0Tok . CsPlusRule 
          => CsRule ( LangInfo . CsPlusToks , TokClassTyp . TokClassCsPlus ) 
          | Ldl0Tok . CsAltRule 
          => CsRule ( LangInfo . CsAltToks , TokClassTyp . TokClassCsClass ) 
          | Ldl0Tok . FsFixedDefaultRule 
          , Ldl0Tok . FsFixedHorizRule 
          , Ldl0Tok . FsFixedVertRule 
          , Ldl0Tok . FsFixedFillRule 
          , Ldl0Tok . FsListDefaultRule 
          , Ldl0Tok . FsListHorizRule 
          , Ldl0Tok . FsListVertRule 
          , Ldl0Tok . FsListFillRule 
          => MultiFsRule ( ) 
          ELSE 
          END (* CASE *) 
        END (* IF *) 
      END VisitRule 

  ; BEGIN (* Pass5 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl0Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        AstView . GetChildren 
          ( LangInfo . Root , LRootChildren , LangInfo . LdlLang ) 
      ; <* FATAL ANY *> 
        BEGIN
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl0Child . LanguageDefinition_Rules ] 
            , VisitRule 
            ) 
        END (* Block. *) 
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
(* FIXME: Handle the Trailing list tokens. *) 
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

    = PROCEDURE CsFixedRule ( ) 
      RAISES { AssertionFailure } 

      = VAR FixedAsChildList : AstView . AstRefTyp 
      ; VAR FixedAsChildNode : AstView . AstRefTyp 
      ; VAR FixedAsChildWasFound : BOOLEAN 
      ; VAR FixedFsEstChildrenMap : LdlSemantics . FsNodeArrayRefTyp 
      ; VAR FixedBuildTok : LbeStd . TokTyp := LbeStd . Tok__Null 

      ; PROCEDURE Rhs 
          ( LhsTok : LbeStd . TokTyp 
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

              OF Ldl0Tok . Ident , Ldl0Tok . String 
              => LCsSemDecl 
                   := LdlSemantics . DeclOfRef ( LangInfo , CsChildNode ) 
              ; IF FixedAsChildList . NodeNo # LbeStd . EstNodeNoNull 
                   (* LCsSemDecl corresponds to an abstract child *) 
                    AND FALSE (* We dont have AS-CS correspondence *) 
                THEN 
                  IF FixedAsChildWasFound 
                  THEN (* Any checks against corresponding abstract 
                          child go here. *) 
                    IF EstUtil . EstTok ( FixedAsChildNode . NodeRef ) 
                        = Ldl0Tok . AsOptChild 
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
              ; TYPECASE LCsSemDecl 
                OF NULL => 
 
                | LdlSemantics . SemFirstOccClassTyp ( TSemClass ) 
                => (* An abstract class.  In Ldl0, we treat it as a concrete
                      class too.  Note here that it is used as such. *) 
                  INC ( TSemClass . CsRefCt ) 
                ; Rhs_Right ^ [ Rhs_NextRightSs ] := LCsSemDecl . DeclTok 
                ; INC ( Rhs_NextRightSs ) 

                ELSE 
                  Rhs_Right ^ [ Rhs_NextRightSs ] := LCsSemDecl . DeclTok 
                ; INC ( Rhs_NextRightSs ) 
                END (* TYPECASE *) 

              | Ldl0Tok . Concatenation 
              => IF FixedAsChildList . NodeNo # LbeStd . EstNodeNoNull 
                    (* LCsSemDecl corresponds to an abstract child *) 
                    AND FALSE (* We dont have AS-CS correspondence *) 
                 THEN (* Any checks against corresponding abstract 
                             child go here. *) 
                   IF FixedAsChildWasFound 
                   THEN (* This is always OK too. *) 
                     IF EstUtil . EstTok ( FixedAsChildNode . NodeRef ) 
                        # Ldl0Tok . AsOptChild 
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

              ELSE 
                CantHappen 
                  ( AFT . A_Pass6_Visit_Rule_CsFixedRule_Rhs_VisitChild_BadChildKind 
                  ) 
              END (* CASE *) 
            END VisitChild 

        ; BEGIN (* Rhs *) 
            VAR LRight : LRTable . TokArrayRefTyp 

          ; BEGIN (* Block  Rhs body block. *) 
              Rhs_Right := NEW ( LRTable . TokArrayRefTyp , RhsCt ) 
              (* ^Possibly slightly extravagant. *) 
            ; FOR RI := 0 TO Word . LeftShift ( 1 , OptionCt ) - 1 
              (* Cartesian product of all the optional substrings. *) 
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
(* TODO: ^See if this can be done faster by bit twiddling, yet without
          introducing implementation dependencies. It's abstractly just a copy
          of RI to Rhs_OptionIdSet.  Do the representations match? *) 
              ; Rhs_NextRightSs := 0 
              ; Rhs_NextFsEstChildSs := 0 
              ; Rhs_NestingDepth := 1 
              ; <* FATAL ANY *> 
                BEGIN
                  AstView . TraverseChildren ( CsRhs , VisitChild ) 
                END (* Block. *) 
              ; Assert 
                  ( Rhs_NextFsEstChildSs = OptionCt 
                  , AFT . A_Ldl0Semantics_Pass6_VisitRule_CsFixedRule_Rhs_NextFsEstChildNo_Bad 
                  )
              ; IF Rhs_NextRightSs = RhsCt 
                THEN LRight := Rhs_Right 
                ELSE 
                  LRight := NEW ( LRTable . TokArrayRefTyp , Rhs_NextRightSs ) 
                ; LRight ^ := SUBARRAY ( Rhs_Right ^ , 0 , Rhs_NextRightSs ) 
                END (* IF *) 
              ; Automaton . AddExternalProduction 
                  ( LangInfo . Gram 
                  , LhsTok 
                  , LRight 
                  , OptionIdSet := Rhs_OptionIdSet 
                  , BuildTok := FixedBuildTok 
                  ) 
              END (* FOR *) 
            ; Rhs_Right := NIL 
            END (* Block *) 
          END Rhs 

      ; BEGIN (* CsFixedRule *) 
          VAR LAsRuleNode : AstView . AstRefTyp 
        ; VAR LSemDecl : LdlSemantics . SemDeclTyp 
        ; VAR LChildren 
            : ARRAY [ 0 .. Ldl0Child . CsFixedRule_Rhs ] 
              OF AstView . AstRefTyp 

        ; BEGIN (* Block  CsFixedRule body block. *) 
            AstView . GetChildren 
              ( RuleNode , LChildren , LangInfo . LdlLang ) 
          ; FixedAsChildList := AstView . AstRefNull (* Could change. *) 
          ; FixedFsEstChildrenMap := NIL (* Could change. *)
          ; TYPECASE <* NOWARN *>  
              LangInfo . SemMapRef 
                ^ [ LChildren [ Ldl0Child . CsFixedRule_Lhs ] . NodeNo ] 
                . SemRef 
            OF NULL (* Earlier-detected error. *) 
            => 

            | LdlSemantics . SemFirstOccCsTyp ( TFirstOcc ) 
              (* ^First occurrence of Lhs.  It's not an abstract token. *) 
            => FixedBuildTok := LbeStd . Tok__Null  
            ; Rhs 
                ( TFirstOcc . DeclTok 
                , LChildren [ Ldl0Child . CsFixedRule_Rhs ] 
                , TFirstOcc . OptionCt 
                , TFirstOcc . RhsCt 
                ) 

            | LdlSemantics . SemAddlDefCsTyp ( TAddlDef ) 
              (* Subsequent occurrence of Lhs. *) 
            => LSemDecl := FirstOcc ( LangInfo , TAddlDef ) 
            ; IF LSemDecl . DeclKind = DeclKindTyp . FixedRule 
              THEN (* Lhs was declared as an As fixed rule. *)  
                LAsRuleNode 
                  := AstView . AstRefTyp 
                       { NodeNo := LSemDecl . DeclRuleNodeNo 
                       , NodeRef 
                           := LangInfo . SemMapRef ^ 
                                [ LSemDecl . DeclRuleNodeNo ] 
                              . SemRef 
                       , ChildNo := LbeStd . EstChildNoNull 
                       } 
              ; FixedAsChildList 
                  := AstView . Child 
                       ( LAsRuleNode , Ldl0Child . AsFixedRule_Children 
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
                           [ TAddlDef . RefTok - LbeStd . Tok__FirstLangDep ]
                       ) 
              ; FixedBuildTok := LSemDecl . DeclTok 

              ELSE (* A subsequent Cs rule for LHS.  Not an abstract token. *) 
                FixedBuildTok := LbeStd . Tok__Null 
              END (* IF *) 
            ; Rhs 
                ( TAddlDef . RefTok 
                , LChildren [ Ldl0Child . CsFixedRule_Rhs ] 
                , TAddlDef . OptionCt 
                , TAddlDef . RhsCt 
                ) 
            ELSE (* Earlier-detected error. *) 
            END (* TYPECASE *) 
          END (* Block *) 
        END CsFixedRule 

    ; PROCEDURE CsListRule 
        ( Kind : [ DeclKindTyp . CsStarRule .. DeclKindTyp . CsPlusRule ] ) 
      RAISES { AssertionFailure } 

      = VAR CsListRight : LRTable . TokArrayRefTyp 
      ; VAR CsListNextRightSs : PortTypes . Int32Typ 

      ; PROCEDURE VisitSeparator ( SepNode : AstView . AstRefTyp ) 

        = VAR LSepSemDecl : LdlSemantics . SemDeclTyp 

        ; BEGIN (* VisitSeparator *) 
            LSepSemDecl := LdlSemantics . DeclOfRef ( LangInfo , SepNode ) 
          ; IF LSepSemDecl # NIL 
            THEN 
              CsListRight ^ [ CsListNextRightSs ] := LSepSemDecl . DeclTok 
            ; INC ( CsListNextRightSs ) 
            END (* IF *) 
          END VisitSeparator 

      ; BEGIN (* CsListRule *) 
          VAR LChildren 
            : ARRAY [ 0 .. Ldl0Child . CsStarRule_Separators ] 
              OF AstView . AstRefTyp 
        ; VAR LSemDeclAsListNode : LdlSemantics . SemDeclAsListNodeTyp 
        ; VAR LLhsTok : LbeStd . TokTyp 
        ; VAR LElemSemDecl : LdlSemantics . SemDeclTyp 
        ; VAR LElemTok : LbeStd . TokTyp 
        ; VAR LRight : LRTable . TokArrayRefTyp 

        ; BEGIN (* Block  CsListRule body block. *) 
            AstView . GetChildren 
              ( RuleNode , LChildren , LangInfo . LdlLang ) 
          ; LLhsTok := LbeStd . Tok__Null
          ; LSemDeclAsListNode
              := LdlSemantics . DeclOfRef 
                   ( LangInfo , LChildren [ Ldl0Child . CsStarRule_Lhs ] ) 
          ; CASE LSemDeclAsListNode . DeclKind 
            OF DeclKindTyp . PlusRule 
            => IF Kind = DeclKindTyp . CsStarRule 
               THEN 
                 SemError2 
                   ( LChildren [ Ldl0Child . CsStarRule_Lhs ] . NodeNo 
                   , LSemDeclAsListNode . DeclRuleNodeNo 
(* TODO: ^Make this identify the ident node instead of the rule node. *) 
                   , AFT . E_ConcreteStarRuleMustNameAbstractStarRule 
                   ) 
               ELSE 
                 LLhsTok := LSemDeclAsListNode . DeclTok 
               END (* IF *) 

            | DeclKindTyp . StarRule 
            => LLhsTok := LSemDeclAsListNode . DeclTok 

            ELSE 
              SemError2 
                ( LChildren [ Ldl0Child . CsStarRule_Lhs ] . NodeNo 
                , LSemDeclAsListNode . DeclRuleNodeNo 
(* TODO: ^Make this identify the ident node instead of the rule node. *) 
                , AFT . E_ConcreteListRuleMustNameAbstractListRule 
                ) 
            ; RETURN 
            END (* CASE *) 
          ; IF LLhsTok # LbeStd . Tok__Null 
            THEN 
              IF Kind = DeclKindTyp . CsStarRule 
              THEN (* Add L BUILD L ::= <empty> *) 
                Automaton . AddExternalProduction 
                  ( LangInfo . Gram , LLhsTok , NIL , BuildTok := LLhsTok ) 
              END (* IF *) 
            END (* IF *) 
          ; LElemSemDecl 
              := LdlSemantics . DeclOfRef 
                   ( LangInfo , LChildren [ Ldl0Child . CsStarRule_ListChild ] )
          ; IF LElemSemDecl # NIL 
            THEN 
              LElemTok := LElemSemDecl . DeclTok 
            ; TYPECASE LElemSemDecl 
              OF LdlSemantics . SemFirstOccClassTyp ( TSemClass ) 
              => (* An abstract class.  In Ldl0, we treat it as a concrete
                    class too.  Note here that it is used as such. *) 
                 INC ( TSemClass . CsRefCt ) 
              ELSE 
              END (* TYPECASE *) 

            (* Add L BUILD L ::= L' *) 
            ; LRight := NEW ( LRTable . TokArrayRefTyp , 1 ) 
            ; LRight ^ [ 0 ] := LSemDeclAsListNode . SublistTok 
            ; Automaton . AddExternalProduction 
                ( LangInfo . Gram , LLhsTok , LRight , BuildTok := LLhsTok ) 

            (* Add L' ::= E *) 
            ; LRight := NEW ( LRTable . TokArrayRefTyp , 1 ) 
            ; LRight ^ [ 0 ] := LElemTok 
            ; Automaton . AddExternalProduction 
                ( LangInfo . Gram 
                , LSemDeclAsListNode . SublistTok 
                , LRight 
                ) 

            (* Add L' ::= L' <separators> L' *) 
            ; CsListRight 
                := NEW 
                     ( LRTable . TokArrayRefTyp 
                     , 2 
                       + AstView . FastExtravagantChildCt 
                           ( LChildren [ Ldl0Child . CsStarRule_Separators ] ) 
                     ) 
            ; CsListRight ^ [ 0 ] := LSemDeclAsListNode . SublistTok 
            ; CsListNextRightSs := 1 
            ; <* FATAL ANY *> 
              BEGIN
                AstView . TraverseChildren 
                  ( LChildren [ Ldl0Child . CsStarRule_Separators ] 
                  , VisitSeparator 
                  ) 
              END (* Block. *) 
            ; CsListRight ^ [ CsListNextRightSs ] 
                := LSemDeclAsListNode . SublistTok 
            ; INC ( CsListNextRightSs ) 
            ; IF CsListNextRightSs = NUMBER ( CsListRight ^ ) 
              THEN LRight := CsListRight 
              ELSE 
                LRight := NEW ( LRTable . TokArrayRefTyp , CsListNextRightSs ) 
              ; LRight ^ := SUBARRAY ( CsListRight ^ , 0 , CsListNextRightSs ) 
              END (* IF *) 
            ; Automaton . AddExternalProduction 
                ( LangInfo . Gram 
                , LSemDeclAsListNode . SublistTok 
                , LRight 
                , IsList := TRUE 
                ) 
            ; CsListRight := NIL 
            END (* IF *) 
          END (* Block *) 
        END CsListRule 

    ; PROCEDURE CsAltRule ( ) 
      RAISES { AssertionFailure } 

      = VAR AltLhsTok : LbeStd . TokTyp := LbeStd . Tok__Null
      ; VAR AltBuildTok : LbeStd . TokTyp := LbeStd . Tok__Null 

      ; PROCEDURE VisitAlternative ( AltNode : AstView . AstRefTyp ) 
        RAISES { AssertionFailure } 

        = VAR LRight : LRTable . TokArrayRefTyp 
        ; VAR LElemSemDecl : LdlSemantics . SemDeclTyp 

        ; BEGIN (* VisitAlternative *) 
            LElemSemDecl := LdlSemantics . DeclOfRef ( LangInfo , AltNode ) 
          ; IF LElemSemDecl # NIL 
            THEN 
              TYPECASE LElemSemDecl 
              OF LdlSemantics . SemFirstOccClassTyp ( TSemClass ) 
              => (* An abstract class.  In Ldl0, we treat it as a concrete
                    class too.  Note here that it is used as such. *) 
                 INC ( TSemClass . CsRefCt ) 
              ELSE 
              END (* TYPECASE *) 

            ; LRight := NEW ( LRTable . TokArrayRefTyp , 1 ) 
            ; LRight ^ [ 0 ] := LElemSemDecl . DeclTok 
            ; Automaton . AddExternalProduction 
                ( LangInfo . Gram 
                , AltLhsTok 
                , LRight 
                , BuildTok := AltLhsTok 
                ) 
            END (* IF *) 
          END VisitAlternative 

      ; BEGIN (* CsAltRule *) 
          VAR LChildren 
            : ARRAY [ 0 .. Ldl0Child . CsAltRule_Alternatives ] 
              OF AstView . AstRefTyp 

        ; BEGIN (* Block  CsAltRule body block. *) 
            AstView . GetChildren 
              ( RuleNode , LChildren , LangInfo . LdlLang ) 
          ; AltBuildTok := LbeStd . Tok__Null
          ; TYPECASE 
              LdlSemantics . DeclOfRef 
                ( LangInfo , LChildren [ Ldl0Child . CsAltRule_Lhs ] ) 
            OF NULL 
            => 

            | LdlSemantics . SemDeclAsNodeTyp ( TSemDecl ) 
            => AltLhsTok := TSemDecl . DeclTok 
            ; AltBuildTok := TSemDecl . DeclTok 
            ; <* FATAL ANY *> 
              BEGIN
                AstView . TraverseChildren 
                  ( LChildren [ Ldl0Child . CsAltRule_Alternatives ] 
                  , VisitAlternative 
                  ) 
              END (* Block. *) 

            | LdlSemantics . SemDeclTyp ( TSemDecl ) 
            => AltLhsTok := TSemDecl . DeclTok 
            ; AltBuildTok := LbeStd . Tok__Null 
            ; <* FATAL ANY *> 
              BEGIN
                AstView . TraverseChildren 
                  ( LChildren [ Ldl0Child . CsAltRule_Alternatives ] 
                  , VisitAlternative 
                  ) 
              END (* Block. *) 
            END (* TYPECASE *) 
          END (* Block *) 
        END CsAltRule 

    ; BEGIN (* VisitRule *) 
        CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
        OF Ldl0Tok . AsClassRule 
        => (* Leave these until Pass 7. *) 
        | Ldl0Tok . CsFixedRule 
        => CsFixedRule ( ) 
        | Ldl0Tok . CsStarRule 
        => CsListRule ( DeclKindTyp . CsStarRule ) 
        | Ldl0Tok . CsPlusRule 
        => CsListRule ( DeclKindTyp . CsPlusRule ) 
        | Ldl0Tok . CsAltRule 
        => CsAltRule ( ) 
        ELSE 
        END (* CASE *) 
      END VisitRule 

  ; BEGIN (* Pass6 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl0Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        AstView . GetChildren 
          ( LangInfo . Root , LRootChildren , LangInfo . LdlLang ) 
      ; <* FATAL ANY *> 
        BEGIN
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl0Child . LanguageDefinition_Rules ] 
            , VisitRule 
            )
        END (* Block. *) 
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
         but that would require several extra fields. *) 
    END AfterPass6 

; PROCEDURE Pass7 
    ( VAR LangInfo : LdlSemantics . LangInfoRefTyp 
    ; VAR OmittedProdCt : CARDINAL 
    ) 
  RAISES { AssertionFailure } 
  (* Pass 6: 
       - Insert productions for As class rules that were previously found to
         be used in RHS of concrete rules. 
       - Set their nonterminal kind to NtkAbstractAndConcrete.  
  *) 

  = PROCEDURE VisitRule ( RuleNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure } 

    = PROCEDURE AsClassRule ( ) 
      RAISES { AssertionFailure } 

      = BEGIN (* AsClassRule *) 
          TYPECASE 
            LangInfo . SemMapRef ^ 
              [ AstView . Child 
                  ( RuleNode , Ldl0Child . AsClassRule_ClassName 
                  , LangInfo . LdlLang 
                  ) 
                . NodeNo 
              ] 
            . SemRef 
          OF LdlSemantics . SemFirstOccClassTyp ( TFirstOcc ) 
          => PROCEDURE VisitMember ( ClassMemberTok : IntSets . ValidElemT ) 
             RAISES { AssertionFailure } 

            = VAR LRhs : LRTable . TokArrayRefTyp 

            ; BEGIN 
                IF ClassMemberTok 
                   < LangUtilLo . TokClassFirstTok 
                       ( LangInfo , TokClassAsClass )
                THEN (* Not a class name.  Omit these to avoid ambiguity 
                        from transitive class naming. *) 
                  LRhs := NEW ( LRTable . TokArrayRefTyp , 1 ) 
                ; LRhs ^ [ 0 ] := ClassMemberTok 
                ; Automaton . AddExternalProduction 
                    ( LangInfo . Gram 
                    , TFirstOcc . DeclTok 
                    , LRhs 
                    , BuildTok := TFirstOcc . DeclTok 
                    ) 
                END (* IF *) 
              END VisitMember 

          ; <* FATAL ANY *> 
            BEGIN
              IF GAsClassIsAlsoCsClass  
              THEN (* Previously made every As class also be a Cs class too. *)
                IF TFirstOcc . CsRefCt > 0 
                THEN (* It is used in a concrete RHS, so really treat as a
                        Cs class.  Generate concrete productions for it and
                        set its kind. *)
                  IntSets . ForAllDo ( TFirstOcc . TokSet , VisitMember ) 
                ; Automaton . NoteNontermKind 
                    ( LangInfo . Gram 
                    , TFirstOcc . DeclTok 
                    , NontermKindTyp . NtkAbstractAndConcrete 
                    )
                ELSE (* Omit productions and tell LALR generation it is not
                        a concrete production. *) 
                  Automaton . NoteNontermKind 
                    ( LangInfo . Gram 
                    , TFirstOcc . DeclTok 
                    , NontermKindTyp . NtkAbstractOnly  
                    )
                ; INC ( OmittedProdCt , IntSets . Card ( TFirstOcc . TokSet ) )
                END (* IF *) 
              END (* IF *) 
            END (* Block *) 
             
          ELSE 
          END (* TYPECASE *) 
        END AsClassRule 

    ; BEGIN (* VisitRule *) 
(* TODO: This is silly.  merge it into AsClassRule. *) 
        IF EstUtil . EstTok ( RuleNode . NodeRef ) = Ldl0Tok . AsClassRule 
        THEN AsClassRule ( ) 
        END (* IF  *) 
      END VisitRule 

  ; BEGIN (* Pass7 *) 
      VAR LRootChildren 
        : ARRAY [ 0 .. Ldl0Child . LanguageDefinition_Rules ] 
          OF AstView . AstRefTyp 

    ; BEGIN (* Block *) 
        AstView . GetChildren 
          ( LangInfo . Root , LRootChildren , LangInfo . LdlLang ) 
      ; OmittedProdCt := 0 
      ; <* FATAL ANY *> 
        BEGIN
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl0Child . LanguageDefinition_Rules ] 
            , VisitRule 
            )
        END (* Block. *) 
      END (* Block *) 
    END Pass7 

; CONST LdlVersionUpwardCompatible = 0 
; CONST LdlVersionCompatible = 0 
; CONST LdlVersionMinor = 0 

; PROCEDURE MakeLangId ( Info : LdlSemantics . LangInfoRefTyp ) 

  = VAR LShortName : TEXT := SharedStrings . ToText ( Info . LanguageName ) 
  ; VAR LFullName := "Schutz Language Definition Language " & LShortName 
  ; VAR LVersionIncompatible := 0 
  ; VAR LLdlLangIdRef : LangUtil . LangIdRefTyp 

  ; BEGIN (* MakeLangId *) 
(* TODO: Make more of this information variable, maybe some of it 
         from the Ldl source? *) 
      IF Text . Equal ( LShortName , "Ldl1" ) 
         OR Text . Equal ( LShortName , "ldl1" ) 
      THEN LVersionIncompatible := 1 
      END (* IF *) 
    ; Info . DefLangIdRef 
        := NEW 
             ( LangUtil . LangIdRefTyp 
             , LangName := LFullName 
             , LangShortName := LShortName 
             , LangVersion 
                 := LangUtil . LangVersionTyp 
                      { Incompatible := LVersionIncompatible 
                      , UpwardCompatible := LdlVersionUpwardCompatible 
                      , Compatible := LdlVersionCompatible 
                      , Minor := LdlVersionMinor 
                      } 
             , LangDate 
                 := Date . FromTime ( Time . Now ( ) , z := Date . UTC ) 
             , LangFingerprint 
                 := Fingerprint . Combine 
                      ( Fingerprint . FromText ( LFullName ) 
                      , Fingerprint . Zero 
                      ) 
(* TODO: Make a better fingerprint *) 
             ) 
    ; LLdlLangIdRef := LangUtil . LangIdRef ( LbeStd . LangLdl0 )
    ; IF LLdlLangIdRef = NIL 
         AND ( Text . Equal ( LShortName , "Ldl0" ) 
               OR Text . Equal ( LShortName , "ldl0" ) 
             ) 
      THEN (* This can happen during bootstrapping. *)  
        LLdlLangIdRef := Info . DefLangIdRef 
      END (* IF *)   
    ; Info . LdlLangIdRef := LLdlLangIdRef 
    END MakeLangId 

; PROCEDURE SetSuffixes ( LangInfo : LdlSemantics . LangInfoRefTyp ) 

  = VAR LShortName : TEXT 
          := SharedStrings . ToText ( LangInfo . LanguageName ) 
  ; VAR LSuffix : TEXT 

  ; BEGIN 
      IF Text . Equal ( LShortName , "Ldl1" ) 
      THEN LSuffix := "ldl1" 
      ELSE LSuffix := "ldl0"
      END (* IF *) 
    ; LangInfo . Suffixes 
        := NEW ( LdlSemantics . SuffixListRefTyp , 1 ) 
    ; LangInfo . Suffixes ^ [ 0 ] 
        := LdlSemantics . SuffixPairTyp 
             { Tok := LangInfo . StartTok 
             , Suffix := LSuffix  
             } 
    END SetSuffixes 

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
  ; VAR LOmittedProdCt : CARDINAL := 0 

  ; BEGIN (* Analyze *) 
      TYPECASE Est 
      OF NULL 
      => RETURN NIL 
      | EstHs . EstRefTyp ( TEstRef )  
      => IF EstHs . EstChildKindSetNotSyntacticallyClean 
            * TEstRef . EstChildKindSet 
            # EstHs . EstChildKindSetEmpty 
         THEN 
           SemError ( 0 , AFT . E_NotSyntacticallyCorrect ) 
         ; RETURN NIL 
         ELSE
           IF TEstRef . EstTok = LbeStd . Tok__Augment 
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
            OR LdlLang = LbeStd . LangLdl0 
          , AFT . A_Ldl0Semantics_Analyze_WrongLanguage  
          ) 
      ; LdlSemantics . InitLangInfo ( TEstRef , LdlLang , LLangInfo ) 
      ; LangMap . AddOrChange ( LbeStd . LangLdlAnalyzed , LLangInfo ) 
      ; Pass1 ( LLangInfo ) 
      ; BetweenPass1And2 ( LLangInfo ) 
      ; Pass2 ( LLangInfo ) 
      ; BetweenPass2And3 ( LLangInfo ) 
      ; Pass3 ( LLangInfo ) 
      ; BetweenPass3And4 ( LLangInfo ) 
      ; Pass4 ( LLangInfo ) 
      ; BetweenPass4And5 ( LLangInfo ) 
      ; Pass5 ( LLangInfo ) 
      ; BetweenPass5And6 ( LLangInfo ) 
      ; IF Messages . MessageCount ( MessageCodes . KindTyp . MkError ) = 0 
        THEN 
          Pass6 ( LLangInfo ) 
        ; AfterPass6 ( LLangInfo ) 
        ; Pass7 ( LLangInfo , (* VAR *) LOmittedProdCt ) 
        END (* IF *) 
      ; IF Messages . MessageCount ( MessageCodes . KindTyp . MkError ) = 0 
        THEN 
(* FIXME: We can't precount the productions, because Pass7 may omit productions 
          for As classes that are not used in any CS RHS.  We can only detect 
          this in Pass6, after we have already allocated a production array. 
          And yet another pass seems downright ridiculous.  This will all go
          away if/when we use VarArray for the productions. *) 
(* FIXME: Something is wrong with even this accounting.  It scarcely matters
          for now, because we are intentionally allocating extra production
          space anyway. *) 
          Assert 
            ( TRUE 
              OR Automaton . ActualProdCt ( LLangInfo . Gram ) + LOmittedProdCt 
                 = LLangInfo . Productions 
            , AFT . A_Ldl0Semantics_Analyze_LowProductionCount 
            ) 
        END (* IF *) 
      ; MakeLangId ( LLangInfo ) 
      ; SetSuffixes ( LLangInfo ) 
      ; Messages . Flush ( )  
      ; RETURN LLangInfo 
      ELSE (* It could be a placeholder. *) 
        RETURN NIL 
      END (* TYPECASE *) 
    END Analyze

; PROCEDURE Check ( ) 

  = <* FATAL AssertionFailure *> 
    BEGIN 
      Assert 
        ( Ldl0Child . AsFixedRule_Parents = Ldl0Child . AsStarRule_Parents 
          AND Ldl0Child . AsFixedRule_Parents = Ldl0Child . AsPlusRule_Parents 
        , AFT . A_Ldl0Semantics_DifferentAsParents 
        ) 
      (* ^ Pass1 . MultiDecl depends on this equality. *) 
    ; Assert 
        ( Ldl0Child . FsFixedHorizRule_Parents 
          = Ldl0Child . FsFixedVertRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsFixedDefaultRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsFixedFillRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsListDefaultRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsListHorizRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsListVertRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsListFillRule_Parents 
        , AFT . A_Ldl0Semantics_DifferentFsParents 
        ) 
      (* ^ Pass4 and Pass5 depend on this equality. *) 
    ; Assert 
        ( Ldl0Child . FsFixedHorizRule_Children 
          = Ldl0Child . FsFixedVertRule_Children 
          AND Ldl0Child . FsFixedHorizRule_Children 
              = Ldl0Child . FsFixedDefaultRule_Children 
          AND Ldl0Child . FsFixedHorizRule_Children 
              = Ldl0Child . FsFixedFillRule_Children 
        , AFT . A_Ldl0Semantics_DifferentFsFixedChildren 
        ) 
      (* ^ Pass1 and Pass2 depend on this equality. *) 
    ; Assert 
        ( Ldl0Child . FsListHorizRule_ListChild 
          = Ldl0Child . FsListVertRule_ListChild 
          AND Ldl0Child . FsListHorizRule_ListChild 
              = Ldl0Child . FsListDefaultRule_ListChild 
          AND Ldl0Child . FsListHorizRule_ListChild 
              = Ldl0Child . FsListFillRule_ListChild 
        , AFT . A_Ldl0Semantics_DifferentFsListChild 
        ) 
      (* ^ Pass1 and Pass2 depend on this equality. *) 
    ; Assert 
        ( Ldl0Child . FsListHorizRule_Formatters 
          = Ldl0Child . FsListVertRule_Formatters 
          AND Ldl0Child . FsListHorizRule_Formatters 
              = Ldl0Child . FsListDefaultRule_Formatters 
          AND Ldl0Child . FsListHorizRule_Formatters 
              = Ldl0Child . FsListFillRule_Formatters 
        , AFT . A_Ldl0Semantics_DifferentFsFormatters 
        ) 
      (* ^ Pass1 and Pass2 depend on this equality. *) 
    ; Assert 
        ( Ldl0Child . PrecLevelNone_Operators 
          = Ldl0Child . PrecLevelRight_Operators 
          AND Ldl0Child . PrecLevelNone_Operators 
              = Ldl0Child . PrecLevelLeft_Operators 
        , AFT . A_Ldl0Semantics_DifferentPrecLevelOperators 
        ) 
      (* ^InsertPrecAndAssoc depends in this equality. *) 
    ; Assert 
        ( Ldl0Child . FsFixedHorizRule_Parents 
          = Ldl0Child . FsFixedVertRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsFixedFillRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsFixedDefaultRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsListDefaultRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsListHorizRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsListVertRule_Parents 
          AND Ldl0Child . FsFixedHorizRule_Parents 
              = Ldl0Child . FsListFillRule_Parents 
        , AFT . A_Ldl0Semantics_DifferentFsParents 
        ) 
      (* ^ Pass4 . MultiDecl depends on this equality. *) 
    ; Assert 
        ( Ldl0Child . CsStarRule_Lhs = Ldl0Child . CsFixedRule_Lhs 
          AND Ldl0Child . CsPlusRule_Lhs = Ldl0Child . CsFixedRule_Lhs 
          AND Ldl0Child . CsAltRule_Lhs = Ldl0Child . CsFixedRule_Lhs 
        , AFT . A_Ldl0Semantics_DifferentCsLhs 
        ) 
      (* ^ Pass5 . CsRule depends on this equality. 
           Also Pass6 . CsListRule needs part of it. *) 
    ; Assert 
        ( Ldl0Child . CsStarRule_Separators = Ldl0Child . CsPlusRule_Separators 
        , AFT . A_Ldl0Semantics_DifferentCsSeparators 
        ) 
    ; Assert 
        ( Ldl0Child . CsStarRule_ListChild = Ldl0Child . CsPlusRule_ListChild 
        , AFT . A_Ldl0Semantics_DifferentCsListChild 
        ) 
    (* ^ Pass6 . CsListRule depends in these equalities. *) 
    ; LdlSemantics . RegisterAnalyzer ( LbeStd . LangLdl0 , Analyze ) 
    END Check 

; BEGIN (* Ldl0Semantics *) 
    Check ( ) 
  END Ldl0Semantics 
. 
