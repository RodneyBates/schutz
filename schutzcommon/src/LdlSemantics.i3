
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LdlSemantics 

(* Lots of stuff common to Ldl0 and Ldl1 semantic processing.
   It is intended to include only things that are needed only
   for Ldl semantic processing, not things needed during ordinary
   editing.  But it seems to have gotten a little mixed up with
   LangUtil.
*) 

; IMPORT Thread 
; IMPORT Wr 

; IMPORT Assertions 
; FROM Failures IMPORT Backout    
; IMPORT AstView 
; IMPORT EstHs 
; IMPORT IntSets 
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT PaintHs 
; IMPORT PortTypes 
; IMPORT SetClassInfoTbl 
; IMPORT SharedStrings 
; IMPORT TextIntSymbolTable 
; IMPORT TokRelation 
; IMPORT TokStringInfoTbl 
; IMPORT UncertainBool 

; TYPE TokSetTyp = IntSets . T 

(* The semantic map uses a heap-allocated array of records, subscripted 
   by Est node numbers, containing a pointer to the Est node and a pointer to 
   a semantic description object.  Not all elements are necessarily filled in. 
*) 

; TYPE SemMapElemTyp 
    = RECORD 
        EstRef : LbeStd . EstRootTyp 
      ; SemRef : SemTyp 
(* CHECK ^Does this duplicate the EstNode SemRef field? *) 
      END (* RECORD *) 

; CONST SemMapElemNull = SemMapElemTyp { NIL , NIL } 

; TYPE SemMapTyp = ARRAY (* LbeStd . EstNodeNoTyp *) OF SemMapElemTyp 
; TYPE SemMapRefTyp = REF SemMapTyp 

; PROCEDURE InitSemMap ( Est : EstHs . EstRefTyp ) : SemMapRefTyp 
  (* A newly allocated SemMap, with enough elements for all the
     nodes of Est, each initialized to SemMapElemNull
  *) 

(* The token map maps a token of the defined language back to the Est node 
   number of its defining identifier in the definition.  It contains all tokens
   of all kinds defined by the Ldl description, except child names of rules. 
   It omits tokens less than LbeStd . Tok__FirstLangDep and is 
   so biased. 
*) 

; TYPE TokMapTyp = ARRAY (* LbeStd . TokTyp *) OF LbeStd . EstNodeNoTyp 
; TYPE TokMapRefTyp = REF TokMapTyp 

; CONST TokMapBias = LbeStd . Tok__FirstLangDep 
  (* A bias.  Subract it from a token to get a subscript. *) 

; PROCEDURE NewTokMap ( Size : LbeStd . TokTyp ) : TokMapRefTyp 
  (* A newly allocated TokMap with Size elements, each initialized to 
     LbeStd . EstNodeNoNull
  *) 

; PROCEDURE ExpandTokMap 
    ( VAR MapRef : TokMapRefTyp ; Size : LbeStd . TokTyp ) 
  (* Expand MapRef ^ to Size, if it is not already that big.
     Copy existing elements and initialize new ones to LbeStd . EstNodeNoNull.
     PRE: MapRef # NIL
  *) 

(* The FsTreeMap maps a token of the defined language to the root of its 
   Fs tree.  It contains entries only for ConstTerm, VarTerm, AsPlus , AsStar, 
   and AsFixed tokens.  Its subscripts are biased relative to ConstTerm tokens 
   (the first of which is LbeStd . Tok__FirstLangDep, which is the bias). 
*) 

; TYPE FsTreeMapTyp = ARRAY OF LangUtil . FsNodeRefTyp 
; TYPE FsTreeMapRefTyp = REF FsTreeMapTyp 

; CONST FsTreeMapBias =  LbeStd . Tok__FirstLangDep

(* TODO: Declare and use FsTreeMapBias. *) 

; PROCEDURE NewFsTreeMap ( Size : LbeStd . TokTyp ) : FsTreeMapRefTyp 
  (* A newly allocated FsTreeMap with Size elements, each initialized to NIL. *)

; PROCEDURE ExpandFsTreeMap 
    ( VAR MapRef : FsTreeMapRefTyp ; Size : LbeStd . TokTyp ) 
  (* Expand MapRef ^ to Size, if it is not already that big.
     Copy existing elements and initialize new ones to NIL.
     PRE: MapRef # NIL
  *) 

(* The StringMap maps a token of the defined language to its String.  It 
   contains entries only for String, VarTerm, AsStar, AsPlus, AsFixed, 
   and AsClass tokens.  Its subscripts are biased relative to String tokens, 
   the first of which is LbeStd . Tok__FirstLangDep.  
*) 

; TYPE StringMapTyp = ARRAY OF SharedStrings . T 
; TYPE StringMapRefTyp = REF StringMapTyp 

; CONST StringMapBias = LbeStd . Tok__FirstLangDep 
  (* A bias.  Subtract it from a token to get a subscript. *) 

; PROCEDURE NewStringMap ( Size : LbeStd . TokTyp ) : StringMapRefTyp 
  (* A newly allocated StringMap with Size elements, each initialized to NIL. *)

(* A mapping between source file suffix strings, e.g., ".m3", and
   abstract tokens of top level AST tree nodes.  There can be
   more than one different AST root token in a language, with
   different suffixes, e.g., a MODULE or an INTERFACE.
*)  

; TYPE SuffixPairTyp 
    = RECORD 
        Suffix : TEXT 
      ; Tok : LbeStd . TokTyp := LbeStd . Tok__Null 
      END (* RECORD *) 
; TYPE SuffixListTyp = ARRAY OF SuffixPairTyp 
; TYPE SuffixListRefTyp = REF SuffixListTyp 

(* Just a list of integers.  Used to internally represent versions. *) 

; TYPE IntListTyp = ARRAY OF PortTypes . Int32Typ 
; TYPE IntListRefTyp = REF IntListTyp 

; TYPE TokPartitionTyp = ARRAY LbeStd . TokClassTyp OF LbeStd . TokTyp 
; TYPE TokPartitionRefTyp = REF TokPartitionTyp 

(* A record holding lots of things for the overall semantic analysis 
   of an Ldl description, including some that are temporary. 
*) 

; TYPE LangInfoTyp 
    = RECORD 
        DefLangIdRef : LangUtil . LangIdRefTyp := NIL 
        (* ^For the language being defined. *) 
      ; LdlLangIdRef : LangUtil . LangIdRefTyp := NIL 
        (* ^For the language the specification is written in. *) 
      ; LdlLang : LbeStd . LangTyp := LbeStd . LangNull 
        (* ^The Ldl language this language is specified in.  
           Can be null, in which case semantics assumes one-to-one
           mapping between actual Est children and the numbering in
           LdlChild.  This happens during initial bootstrap.  
           Otherwise, it is used to find AS children. *) 
      ; Root : AstView . AstRefTyp := AstView . AstRefNull 
      ; SemMapRef : SemMapRefTyp := NIL 
      ; TokMapRef : TokMapRefTyp := NIL 
      ; FsTreeMapRef : FsTreeMapRefTyp := NIL 
      ; StringMapRef : StringMapRefTyp := NIL 
      ; SymbolTable : TextIntSymbolTable . T 
      ; LanguageName : SharedStrings . T 
        (* ^For the language being defined. *) 
      ; StartRule : AstView . AstRefTyp 
      ; StartIdentNodeNo : LbeStd . EstNodeNoTyp := LbeStd . EstNodeNoNull 
      ; PrecRule : AstView . AstRefTyp 
      ; TokCounts : TokPartitionRefTyp 
      ; TokPart : TokPartitionRefTyp 
      ; StringToks : LbeStd . TokTyp 
      (* StringToks is set to the lower bound of its range prior to 
         pass 1 and used to allocate tokens during pass 1, after which, it 
         is the upper bound + 1 of the fixed string token range. *) 
      ; VarTermToks : LbeStd . TokTyp 
      ; VarTermModToks : LbeStd . TokTyp 
      ; AsPlusToks : LbeStd . TokTyp 
      ; AsStarToks : LbeStd . TokTyp 
      ; AsFixedToks : LbeStd . TokTyp 
      ; AsSublistToks : LbeStd . TokTyp 
      ; AsListCardToks : LbeStd . TokTyp 
      ; AsPartialToks : LbeStd . TokTyp 
(* TODO:  We need the FsTreeMap to cover up through the
          list cardinality tokens only.  Partial tokens also need to have
          their FsNode looked up, but we can do this by fixed biasing of
          a partial tok down to its list tok.  Putting Partial next opens
          up the possibility of putting them in the FsTreeMap too, just for
          a bit of speed.  I don't think there is a need to look up the
          FsNode for a sublist tok, but that could also be supported in
          either of the same two ways.
*) 
      ; AsClassOnlyToks : LbeStd . TokTyp 
      ; AsCsClassToks : LbeStd . TokTyp 
      (* During Pass 1, VarTermToks, AsFixedToks, AsStarToks,
         AsPlusToks, AsClassOnlyToks, and AsCsClassToks count the
         declared identifiers of each kind respectively.  AsSublistToks
         and AsPartialToks are the numbers of the two categories of
         internally generated tokens for lists, and just set to
         AsStarToks+AsPlusToks.

         At the beginning of pass 2, these are adjusted to be the
         beginnings of the ranges of each kind. During pass 2, they
         are incremented as declarations are found, so that all are
         numbered compactly, with each kind numbered contiguously.
         Thus, at the end of pass 2 and thereafter, they are the upper
         bounds + 1 of the corresponding token ranges. AsSublistToks
         is where it is, because these are input sentential form
         nonterminals (and thus need to be contiguous with AsPlusToks,
         AsStarToks, and AsFixedToks, which have this property too),
         but do not build Est nodes or have FsTrees (thus need to be
         after the other 3 ).
      *) 
      ; CsAltToks : LbeStd . TokTyp 
      ; CsPlusToks : LbeStd . TokTyp 
      ; CsPluralToks : LbeStd . TokTyp 
      ; CsStarToks : LbeStd . TokTyp 
      ; CsFixedToks : LbeStd . TokTyp 
      (* The Cs*Toks are concrete nonterminals not declared as abstract. 
         The counting/numbering of them works like the above, but using 
         pass 3 and pass 4, since we need to know they were not declared 
         as abstract. *) 
      ; GcsChildToks : LbeStd . TokTyp := LbeStd . Tok__Null  
        (* GcsChildToks are concrete nonterminals generated during 
           mechanical generation of a concrete syntax.
        *) 
      ; Productions : LRTable . ProdNoTyp 
      ; GenProductions : LRTable . ProdNoTyp 
      ; StartTok : LbeStd . TokTyp := LbeStd . Tok__Null 
      ; ClassRelation : TokRelation . T := NIL 
      ; Gram : LRTable . GrammarTyp := NIL 
        (* ^Language-definer-written grammar. *) 
      ; GenGram : LRTable . GrammarTyp := NIL 
        (* ^Automatically generated grammar. *) 
      ; Suffixes : SuffixListRefTyp := NIL 
      ; VersionNos : IntListRefTyp := NIL 
      ; ClassTable : SetClassInfoTbl . T 
      ; TokStringTable : TokStringInfoTbl . T 
      END (* RECORD *) 

; TYPE LangInfoRefTyp = REF LangInfoTyp 

; PROCEDURE InitLangInfo 
    ( Est : EstHs . EstRefTyp 
    ; LdlLang : LbeStd . LangTyp := LbeStd . LangNull 
    ; VAR Info : LangInfoRefTyp 
    ) 
  RAISES { Backout } 
  (* Set initial values of fields of Info^. *) 

(* Semantic attributes for individual tree nodes, attached via 
   the semantic map. *) 

; TYPE DeclKindTyp 
    = { Null , VarTermRule , FixedRule , StarRule , PlusRule , AsClassRule 
      , AsCsClassRule , CsFixedRule , CsStarRule , CsPlusRule , CsPluralRule  
      , CsAltRule , String 
      } 

; TYPE DeclKindSetTyp = SET OF DeclKindTyp 

; CONST DeclKindSetStarOrPlus 
    = DeclKindSetTyp { DeclKindTyp . StarRule , DeclKindTyp . PlusRule } 

; TYPE SemTyp = OBJECT NodeNo := LbeStd . EstNodeNoNull END (* OBJECT *) 

; TYPE SemDeclTyp (* ABSTRACT *) 
    = SemTyp 
        OBJECT 
          DeclKind := DeclKindTyp . Null 
        ; DeclRuleNodeNo := LbeStd . EstNodeNoNull (* The rule node *) 
        ; DeclTok : LbeStd . TokTyp := LbeStd . Tok__Null 
          (* ^The declared token. *)
        END (* OBJECT *) 
  (* ^Objects of subtypes of this go on the declaring occurrence of an 
      identifier or string. 
  *) 

; TYPE SemDeclAsNodeTyp 
    = SemDeclTyp 
        BRANDED "SemDeclAsNodeTyp" OBJECT 
          FsIdentNodeNo := LbeStd . EstNodeNoNull 
        ; CsRuleCt : PortTypes . Int32Typ := 0 
        END (* OBJECT *) 

; TYPE SemDeclAsFixedNodeTyp 
    = SemDeclAsNodeTyp 
        BRANDED "SemDeclAsFixedNodeTyp" OBJECT 
          OptionalChildCt : PortTypes . Card32Typ := 0 
        END 
  (* ^This goes on the declaring ident of an As fixed node rule. *) 

; TYPE ListCardTyp = { ListCardEmpty , ListCardSingleton , ListCardPlural } 

; TYPE ListCardToksTyp = ARRAY ListCardTyp OF LbeStd . TokTyp 
; TYPE ListCardUsedTyp = ARRAY ListCardTyp OF BOOLEAN  

; TYPE SemDeclAsListNodeTyp 
    = SemDeclAsNodeTyp 
        BRANDED "SemDeclAsListNodeTyp" OBJECT
          SublistTok : LbeStd . TokTyp := LbeStd . Tok__Null 
        ; PartialTok : LbeStd . TokTyp := LbeStd . Tok__Null 
        ; ListCardToks := ListCardToksTyp { LbeStd . Tok__Null , .. } 
        ; ListCardUsed := ListCardUsedTyp { FALSE , .. } 
        END 
  (* ^This goes on the declaring ident of an As list node rule. *) 

; TYPE SemFirstOccTyp (* ABSTRACT *) 
    = SemDeclTyp 
        OBJECT NextNodeNo := LbeStd . EstNodeNoNull END (* OBJECT *) 
  (* ^Objects of subtypes of this go on the first occurence of a string 
     or of an identifier on the LHS of a class rule or Cs rule. 
     Field NextNodeNo leads to the next occurence anywhere of the same 
     string or the next LHS occurrence of the same identifier. This will 
     have a semantic attribute of a subtype of SemAddlDefTyp. 
  *) 

; TYPE SemFirstOccStringTyp 
    = SemFirstOccTyp 
        BRANDED "SemFirstOccStringTyp" OBJECT 
          AltName := LbeStd . EstNodeNoNull 
        ; InsertionString : SharedStrings . T 
          (* ^The string with quotes and escapes removed, as it should appear 
              in an FsKindInsTok node. 
          *) 
        END (* OBJECT *) 
  (* ^This goes on the first occurrence of a string. 
     AltName is the first, if any, class or CsAlt name 
     equated to this string. 
  *) 

; TYPE SemFirstOccClassTyp 
    = SemFirstOccTyp 
        BRANDED "SemFirstOccClassTyp" OBJECT 
          TokSet : IntSets . T := NIL 
          (* ^Transitively closed set of tokens this class contains. *) 
        ; CsRefCt : PortTypes . Int32Typ := 0 
          (* ^Count of occurences of this class name in the Rhs of a Cs rule. *)
        ; SingletonTok := LbeStd . Tok__Null 
          (* ^Non-null IFF this is a singleton class.  The single token 
             in the class, if so. 
          *) 
        ; FormatsEmpty := UncertainBool . T . Unknown 
        END (* OBJECT *) 
  (* ^This goes on the first occurence of an identifier for an AsClass rule.
     Additional occurences of this identifier on the LHS of another rule of 
     the same kind can add more alternatives.  
  *) 

; TYPE SemFirstOccCsTyp 
    = SemFirstOccTyp 
        BRANDED 
          "SemFirstOccCsTyp" 
          OBJECT 
            OptionCt : LRTable . OptionIdTyp := 0 
(* CHECK: ^What purpose does this field serve? *) 
          ; RhsCt : LbeStd . TokNoTyp := 0 
          END (* OBJECT *) 
  (* ^ This goes on the first occurence of a Cs identifier 
       that is not declared as an AsNode. *) 

; TYPE SemRefTyp (* ABSTRACT *) 
    = SemTyp 
        OBJECT 
          RefDeclId := LbeStd . EstNodeNoNull 
        ; RefTok : LbeStd . TokTyp := LbeStd . Tok__Null 
        END (* OBJECT *) 
  (* ^This goes on a reference occurrence of an identifier *) 

; TYPE SemChildClassRefTyp 
    = SemRefTyp 
        BRANDED "SemChildClassRefTyp" OBJECT
          ChildNo : LbeStd . EstChildNoTyp 
        END (* OBJECT *) 
  (* Objects of subtypes of this go on references in an abstract fixed rule 
     to a child's class or abstract node kind. 
  *)  

; TYPE SemFsRefTyp 
    = SemRefTyp 
        BRANDED "SemFsRefTyp" OBJECT 
          RefRuleNodeNo := LbeStd . EstNodeNoNull (* The rule node. *) 
        END (* OBJECT *) 
  (* ^This goes on a LHS identifier of an Fs rule. *) 

; TYPE SemAddlDefTyp (* ABSTRACT *) 
    = SemRefTyp 
        OBJECT 
          NextNodeNo := LbeStd . EstNodeNoNull 
        END (* OBJECT *)
  (* Objects of subtypes of this go on nodes refer to a previously defined 
     identifier, but add additional attributes to it. 
  *) 

; TYPE SemAddlDefClassTyp 
    = SemAddlDefTyp 
        BRANDED "SemAddlDefClassTyp" OBJECT 
        END (* OBJECT *) 
  (* ^This goes on the ClassName node of a AsClassRule or AsCsClassRule
     that adds more members to a previously-declared class. 
  *) 
  
; TYPE SemAddlDefCsTyp 
    = SemAddlDefTyp 
        BRANDED 
          "SemAddlDefCsTyp" 
          OBJECT 
            OptionCt : LRTable . OptionIdTyp := 0 
          ; RhsCt : LbeStd . TokNoTyp := 0 
          END (* OBJECT *) 
  (* ^This goes on a Cs Lhs or Build identifier that has occurred before, 
     either in an As rule or a Cs rule. 
  *) 

; TYPE SemAddlDefStringTyp 
    = SemAddlDefTyp BRANDED "SemAddlDefStringTyp" OBJECT END (* OBJECT *) 
  (* ^This goes on any string node that is a subsequent reoccurrence of
     a string that has appeared earlier.
  *) 

; PROCEDURE MkAstRef 
    ( LangInfo : LangInfoRefTyp ; NodeNo : LbeStd . EstNodeNoTyp ) 
  : AstView . AstRefTyp 
  RAISES { Backout } 

; PROCEDURE MapAstRef 
    ( LangInfo : LangInfoRefTyp ; AstRef : AstView . AstRefTyp ) 
  RAISES { Backout } 
  (* Store a rule in the SemMap of LangInfo, mapping the node number
     of AstRef to its node pointer, if non-NIL.
     PRE: the SemMap must be big enough.
     Can be recalled, but all calls must agree.
  *) 

; PROCEDURE MapAstRefs 
    ( LangInfo : LangInfoRefTyp 
    ; READONLY AstRefs : AstView . AstRefArrayTyp 
    ) 
  RAISES { Backout } 
  (* For all element of AstRefs, call MapAstRef. *) 

; PROCEDURE SemDeclOfTok ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  : SemDeclTyp 
  (* The SemDecl of the declaration of Tok.  NIL if anything wrong. *) 

; PROCEDURE TokSetOfTok 
    ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  : IntSets . T  
  (* Set of all tokens denoted by Tok.  Could be plural if Tok is a class.  
     Empty if anything wrong.  
  *) 

; PROCEDURE AsTokSetOfTok 
    ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  : IntSets . T 
  (* Set of AS tokens denoted by Tok.  Could be plural if Tok is a class.  
     Omit everything that is not an AS token. Empty if anything wrong.  
  *) 

; PROCEDURE AsTokSetOfAstRef 
    ( LangInfo : LangInfoRefTyp ; AstRef : AstView . AstRefTyp ) 
  : IntSets . T 
  (* If AstRef denotes a class or token, return its token set after removing
     everything that is not an AS token, else Empty. 
  *) 

; PROCEDURE FsElemOfFsList 
    ( FsListNodeRef : LangUtil . FsNodeRefTyp ) : LangUtil . FsNodeRefTyp 
  (* If FsListNodeRef is a list node, return its immediate child Fs node that
     contains its list element.  Do not verify FsKind of result.  
     NIL if anything is wrong. 
  *) 

; PROCEDURE FsCondOrEstOfFsList 
    ( FsListNodeRef : LangUtil . FsNodeRefTyp ) : LangUtil . FsNodeRefTyp 
  (* If FsListNodeRef is a list node, return its descendant that is a
     conditional construct, if any, (there can be at most one, and it 
     surrounds the FsEstChildOfList node), or if not, the FsEstChildOfList 
     node (there is always exactly one of these).  This amounts to skipping
     down through any sublist nodes, whose non-Est children will be
     bookmarks.   
     May be NIL if something is wrong. 
  *) 

; PROCEDURE EstNodeNoOfTokDecl 
    ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  : LbeStd . EstNodeNoTyp 
  (* The EstNodeNo of the LDL declaration of Tok.  
     LbeStd . EstNodeNoNull if anything wrong. 
  *) 

; PROCEDURE FsRuleForTokUsingLangInfo  
    ( LangInfoRef : LangInfoRefTyp 
    ; EstTok : LbeStd . TokTyp 
    ; IsPlaceholder : BOOLEAN := FALSE 
    ) 
  : LangUtil . FsNodeRefTyp 
  (* The FsNodeRef of the root of the format tree for an Est node. 
     Also works when EstTok is a list partial token. 
     Does NOT handle standard tokens. 
  *)
(* TODO: ^This really should be in LangUtil, but it needs LangInfoRefTyp,
         which is declared here in LdlSemantics, and simply moving it leads
         to cyclic imports.  So move LangInfoRefTyp somewhere else, probably
         LangUtil too.  The whole LangUtil/LdlSemantics split needs review.
*) 

; EXCEPTION NodeNoOverflow ( PortTypes . Card32Typ ) 

; PROCEDURE PostNumberFsTree 
    ( Root : LangUtil . FsNodeRefTyp 
    ; InitialNo : LangUtil . FsNodeNoTyp := 0 
    ) 
  RAISES { NodeNoOverflow }  
  (* Fill in the FsPostNodeNo fields of an entire tree,
     as postorder node numbers, starting with InitialNo.
  *) 

; PROCEDURE InsertionString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Backout } 
  (* Convert a quoated string, as in an Ldl definition, into the string
     to be displayed, by removing quotes and escape sequences.
     PRE: LdlString is quoted. 
  *) 

; PROCEDURE RemovePlaceholderDelimsText ( FText : TEXT ) : TEXT 
  (* Remove placeholder delimiters, if any, from FText. *) 

; PROCEDURE PlaceholderString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Backout } 
  (* Prepend and append placeholder delimiters to LdlString, if they
     are not already there.
  *) 

(* The following procedures append a standard suffix to a token string,
   to produce a token string for a generated token.   
   But "append" the suffix inside placeholder delimiters, if any.
*) 

; PROCEDURE ModTokString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Backout }

; PROCEDURE ModTokText ( FString : SharedStrings . T ) : TEXT 

; PROCEDURE SublistString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Backout } 

; PROCEDURE EmptyListString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Backout } 

; PROCEDURE SingletonListString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Backout } 

; PROCEDURE PluralListString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Backout } 

; PROCEDURE PartialString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Backout } 

; TYPE AnalyzeProcTyp 
    = PROCEDURE  
        ( Est : LbeStd . EstRootTyp 
        ; LdlLang : LbeStd . LangTyp := LbeStd . LangNull 
        ) 
      : LangInfoRefTyp  
      RAISES { Backout , Thread . Alerted } 

; PROCEDURE RegisterAnalyzer 
    ( Lang : LbeStd . LangBuiltinTyp ; Analyzer : AnalyzeProcTyp ) 
  (* Register Analyzer as the analyzer procedure for language Lang. *) 

; PROCEDURE Analyze 
    ( Est : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  : LangInfoRefTyp 
  RAISES { Backout , Thread . Alerted } 
  (* If Lang is a builtin language, call its registered analyzer,
     passing Est and Lang to it.
  *) 

; PROCEDURE ManLRGen  
    ( LangInfo : LangInfoRefTyp ) 
  RAISES { Backout } 
  (* Do LR generation for handwritten grammar. *) 

; PROCEDURE CountOfNonclassTokSetProductions 
    ( LangInfo : LangInfoRefTyp ; ClassMembers : IntSets . T ) 
  : CARDINAL 
  (* Number of members of ClassMembers that are not class tokens themselves. *) 

; PROCEDURE GenNonclassTokSetProductions  
    ( LangInfo : LangInfoRefTyp 
    ; Gram : LRTable . GrammarTyp 
    ; ClassTok : LbeStd . TokTyp 
    ; ClassMembers : IntSets . T 
    ; ReplaceListCardToks : BOOLEAN := FALSE 
    ) 
  RAISES { Backout } 
  (* For all members M of the class identified by ClassTok that are not 
     classes themselves, generate a singleton production ClassTok ::= M.
  *) 

; PROCEDURE ForAllAsMembersDo   
    ( LangInfo : LangInfoRefTyp 
    ; TokSet : IntSets . T 
    ; Visit : IntSets . ProcOfValidElem 
    ) 
  RAISES ANY  
  (* Call back Visit for every member of TokSet that is an AS node. *) 

; PROCEDURE IncIndentCode 
    ( VAR (* IN OUT *) Code : LangUtil . IndentCodeTyp 
    ; Incr : PortTypes . Int32Typ 
    ) 
  (* Saturates at LangUtil . IndentCodeFirstNormal and 
     LangUtil . IndentCodeLastNormal 
  *) 
  (* Ignores negative Incr.  Saturates at LangUtil . IndentCodeLastNormal *) 

; PROCEDURE PatchListFields
    ( FsNodeRef : LangUtil . FsNodeRefTyp 
    ; FsListNodeRef : LangUtil . FsNodeRefTyp 
    ; IndentCode : LangUtil . IndentCodeTyp 
    )  
  (* Copy fields FsListSliceThruFmtNo
                 FsSublistTok
                 FsPartialTok
                 FsEmptyListTok
                 FsSingletonListTok
                 FsPluralListTok 
     down thru all descendents.
     Also set FsIndentCode of any FsEstChildOfList nodes to IndentCode. 
  *) 

; PROCEDURE WriteLdlTokInterfaceToStream 
    ( Writer : Wr . T 
    ; LangInfo : LangInfoRefTyp 
    ; InterfaceName : TEXT 
    ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a token-declaring interface to an already-open Wr.T *) 

; PROCEDURE WriteLdlTokInterfaceForName 
    ( LangInfo : LangInfoRefTyp ; LangName : TEXT ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a token-declaring interface to a created file. *) 

; PROCEDURE WriteLdlChildInterfaceToStream 
    ( Writer : Wr . T ; LangInfo : LangInfoRefTyp ; InterfaceName : TEXT ) 
  RAISES { Backout , Thread . Alerted , Wr . Failure }
  (* Write a child-name-declaring interface to an already-open Wr.T *) 

; PROCEDURE WriteLdlChildInterfaceForName 
    ( LangInfo : LangInfoRefTyp ; LangName : TEXT ) 
  RAISES { Backout , Thread . Alerted , Wr . Failure }
  (* Write a child-name-declaring interface to a created file. *) 

; PROCEDURE WriteInitTokStringsModuleToStream  
    ( Writer : Wr . T 
    ; LangInfo : LangInfoRefTyp 
    ; LangName : TEXT 
    ; ModuleName : TEXT 
    ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a module that initializes reserved word and placeholder strings
     to an already-open Wr.T. 
  *) 

; PROCEDURE WriteInitTokStringsModuleForName 
    ( LangInfo : LangInfoRefTyp ; LangName : TEXT ) 
  RAISES { Wr . Failure , Thread . Alerted } 
  (* Write a module that initializes reserved word and placeholder strings
     to a created file. 
  *) 

; VAR Bootstrapping : BOOLEAN := FALSE 

; PROCEDURE DumpAllFsTreesStream 
    ( WrT : Wr . T 
    ; Lang : LbeStd . LangTyp 
    ; IncludeDefaultValuedFields : BOOLEAN := FALSE 
    ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a readable dump of all Fs trees to an already-open Wr.T. *) 

; PROCEDURE FinishLRGeneration 
    ( LangInfo : LangInfoRefTyp 
    ; Gram : LRTable . GrammarTyp 
    ; ProdCount : LRTable . ProdNoTyp 
    ) 
  RAISES { Backout } 
  (* Do the LR generation on Gram of LangInfo, with ProdCount productions. *) 

; PROCEDURE AssignCosts 
    ( LangInfo : LangInfoRefTyp ; Gram : LRTable . GrammarTyp ) 
  (* Set insertion and deletion costs for tokens. *) 

; PROCEDURE NoteNontermKindForRange 
    ( Gram : LRTable . GrammarTyp
    ; FromTok : LbeStd . TokTyp 
    ; ToTok : LbeStd . TokTyp 
    ; Kind : LRTable . NontermKindTyp 
    ) 
  (* For all toks in [FromTok,ToTok), set the nonterminal kind to Kind, *) 

; PROCEDURE NoteStandardNontermKinds 
    ( LangInfo : LangInfoRefTyp ; Gram : LRTable . GrammarTyp ) 
  (* Set standard nonterminal kinds from token ranges.  Only sets Kind 
     if not previously other than NtkNull. Only does cases that are the 
     same for handwritten and generated grammars.
  *) 

; PROCEDURE DeclOfRef 
    ( LangInfo : LangInfoRefTyp 
    ; RefNode : AstView . AstRefTyp 
    )
  : SemDeclTyp 
  (* This can be called only after Pass 4 of semantics is complete. 
     Follows a singleton class to its ground member. 
     Use a SemRef if available, otherwise it look the 
     identifier up in the symbol table and insert a SemRef. 
  *) 

; PROCEDURE EachMemberIsInSomeRange 
    ( LangInfo : LangInfoRefTyp 
    ; Node : AstView . AstRefTyp 
    ; LowerTok : LbeStd . TokTyp 
    ; UpperTok : LbeStd . TokTyp 
    ; SecondLowerTok : LbeStd . TokTyp := 1 
    ; SecondUpperTok : LbeStd . TokTyp := 0 (* Any empty range will do. *) 
    ) 
  : BOOLEAN 
  (* Node should be an identifier referring to a declared token.  Return TRUE
     if it is not, or if it refers to a token in one of the ranges or to
     a class, each of whose members is in one of the ranges.    
  *) 

; PROCEDURE AMemberIsInSomeRange 
    ( LangInfo : LangInfoRefTyp 
    ; Node : AstView . AstRefTyp 
    ; LowerTok : LbeStd . TokTyp 
    ; UpperTok : LbeStd . TokTyp 
    ; SecondLowerTok : LbeStd . TokTyp := 1 
    ; SecondUpperTok : LbeStd . TokTyp := 0 (* Any empty range will do. *) 
    ) 
  : BOOLEAN 
  (* Node should be an identifier referring to a declared token.  Return TRUE
     if it is not, or if it refers to a token in one of the ranges or to
     a class, at least one of whose members is in one of the ranges.    
  *) 

; TYPE FsNodeArrayTyp = ARRAY OF LangUtil . FsNodeRefTyp 
; TYPE FsNodeArrayRefTyp = REF FsNodeArrayTyp 

; PROCEDURE MapFsEstChildren ( RootFsNodeRef : LangUtil . FsNodeRefTyp ) 
  : FsNodeArrayRefTyp 
  (* Return a map from left-to-right ordinal to FsNodeRefTyp pointers,
     for each Est child in the FsTree rooted at RootFsNodeRef.
  *) 

(* Generated concrete tokens: *) 

; PROCEDURE NewInternalConcreteTok 
    ( LangInfo : LangInfoRefTyp ; FsNodeRef : LangUtil . FsNodeRefTyp ) 
  : LbeStd . TokTyp 
  (* Allocate an internally-generated token. *) 

(* Concrete tokens for combinations of list cardinalities. *) 

; TYPE ListCardSetTyp = SET OF ListCardTyp 

; PROCEDURE ListCardTok  
    ( LangInfo : LangInfoRefTyp 
    ; ListTok : LbeStd . TokTyp 
    ; Card : ListCardTyp 
    ) 
  : LbeStd . TokTyp  
  RAISES { Backout } 
  (* The list cardinality token for Card of list ListTok. 
     Identity if ListTok is not known to be an abstract list token. 
  *) 

; PROCEDURE ListCardSetAtomTok
    ( LangInfo : LangInfoRefTyp 
    ; ListAsTok : LbeStd . TokTyp 
    ; CardSet : ListCardSetTyp 
    ; MustBePresent : BOOLEAN := FALSE  
    ) 
  : LbeStd . TokTyp 
  RAISES { Backout } 
  (* An atom (WRT LangInfo) token for the pair (ListAsTok,CardSet),
     allocated in the token space of GcsChildToks.
  *) 

(* Atomic productions for classes. *) 

; PROCEDURE TokForAtomTokSet 
    ( LangInfo : LangInfoRefTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; TokSet : TokSetTyp 
    ; UseCountIncrement : CARDINAL := 0
    ; MustBePresent : BOOLEAN := FALSE  
    ) 
  : LbeStd . TokTyp 
  RAISES { Backout } 
  (* WARNING: This function has SIDE EFFECTS! 
              1) Adds a rule to LangInfo ^ . ClassTable, if not there
              2) Increments its UseCount by UseCountIncrement 
              3) If its UseCount goes 0-to-positive, increments the grammar
                 production count by the set cardinality. 
  *) 
  (* PRE: FsNodeRef has FsKind EstChildOf*, Subtree*, or CondFmt. *) 

(* Atomic productions for RHS strings. *) 

; PROCEDURE TokForAtomTokString
    ( LangInfo : LangInfoRefTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; Right : LRTable . TokArrayRefTyp 
      (* ^Must be immutable. May be NIL to denote the empty string. *) 
    ; UseCountIncrement : CARDINAL := 0
    ; MustBePresent : BOOLEAN := FALSE  
    ) 
  : LbeStd . TokTyp 
  RAISES { Backout } 
  (* An atom (WRT LangInfo) token for the token string Right,
     allocated in the token space of GcsChildToks.
  *) 

; PROCEDURE LdlModuleName ( ImageRef : PaintHs . ImageTransientTyp ) : TEXT 
  (* The name following "LDL" in an ldl description. *) 

; PROCEDURE StackTokCounts1 ( LangInfo : LangInfoRefTyp ) 
  RAISES { Backout } 
  (* Stack up the counts of token kinds in LangInto.TokCounts, into
     LangInfo.TokPart, with each member of the partition set to the 
     starting token number for its namesake tokens.  Do this for
     tokens in kinds ConstTerm through AsCsClass.  Except, for 
     AsSublist and AsPartial, set TokPart to a bias to be added to
     the list token, to get its sublist/partial token.  These two members
     don't change during the next pass.   
  *) 

; PROCEDURE StackTokCounts2 ( LangInfo : LangInfoRefTyp ) 
  (* Stack up the counts of token kinds in LangInto.TokCounts, into
     LangInfo.TokPart, with each member of the partition set to the 
     starting token number for its namesake tokens.  Do this for
     tokens in kinds CsClass through  CsGen.  Also set elements for
     Unused. 
  *) 

; PROCEDURE CheckTokPart ( LangInfo : LangInfoRefTyp ) 
  RAISES { Backout } 

; END LdlSemantics 
. 
