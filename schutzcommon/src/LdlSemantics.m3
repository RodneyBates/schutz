
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LdlSemantics 

(* Lots of stuff common to Ldl0 and Ldl1 semantic processing.
   It is intended to include only things that are needed only
   for Ldl semantic processing, not things needed during ordinary
   editing.  But it seems to have gotten a little mixed up with
   LangUtil.
*) 

; IMPORT Date 
; IMPORT Fmt 
; IMPORT Stdio 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Time 
; IMPORT Wr 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , AssertionFailure , CantHappen   
; IMPORT AstView 
; IMPORT Automaton 
; IMPORT Check 
; IMPORT ClassInfo 
; IMPORT Debug  
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT Gen 
; IMPORT Infos 
; IMPORT IntSets 
; IMPORT LALRLookahead 
; IMPORT LangMap 
; IMPORT LangUtil 
; IMPORT LangUtilLo 
; FROM LangUtil IMPORT FsKindTyp 
; IMPORT LangUtilRep 
; IMPORT Ldl0Child 
; IMPORT Ldl0Tok 
(* ; IMPORT Ldl1Tok -- Commented out because won't bootstrap with it in. *) 
; IMPORT LbeStd 
; IMPORT LRTable 
; FROM LRTable IMPORT NontermKindTyp
; IMPORT LRUtils 
; IMPORT Messages 
; FROM Messages IMPORT SemError0 , SemError , SemError2 
; IMPORT MessageCodes 
; IMPORT Misc 
; IMPORT Reduce 
; IMPORT SharedStrings 
; IMPORT Strings 
; IMPORT TextIntSymbolTable 
; IMPORT TokStringInfo 
; IMPORT PaintHs 
; IMPORT PortTypes 
; IMPORT TokRelation 
; IMPORT UncertainBool 
; IMPORT VersionedFiles 

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

(* The semantic map uses a heap-allocated array of records, subscripted 
   by node numbers, containing a pointer to the Ast node and a semantic 
   description object.  Not all elements are filled in. *) 

(* VISIBLE: *) 
; PROCEDURE InitSemMap ( Est : EstHs . EstRefTyp ) : SemMapRefTyp 
  (* A newly allocated SemMap, with enough elements for all the
     nodes of Est, each initialized to SemMapElemNull
  *) 

  = VAR LNumber : LbeStd . EstNodeNoTyp 
  ; VAR LResult : SemMapRefTyp 

  ; BEGIN (* InitSemMap *) 
      LNumber := EstUtil . EstNodeCt ( Est ) 
    ; LResult := NEW ( SemMapRefTyp , LNumber ) 
    ; FOR I := 0 TO LNumber - 1 
      DO LResult ^ [ I ] := SemMapElemNull 
      END (* FOR *) 
    ; RETURN LResult 
    END InitSemMap 

(* VISIBLE: *) 
; PROCEDURE NewTokMap ( Size : LbeStd . TokTyp ) : TokMapRefTyp 
  (* A newly allocated TokMap with Size elements, each initialized to 
     LbeStd . EstNodeNoNull
  *) 

  = VAR LResult : TokMapRefTyp 

  ; BEGIN (* NewTokMap *) 
      LResult := NEW ( TokMapRefTyp , Size ) 
    ; FOR I := 0 TO Size - 1 
      DO LResult ^ [ I ] := LbeStd . EstNodeNoNull 
      END (* FOR *) 
    ; RETURN LResult 
    END NewTokMap 

(* VISIBLE: *) 
; PROCEDURE ExpandTokMap 
    ( VAR MapRef : TokMapRefTyp ; Size : LbeStd . TokTyp ) 
  (* Expand MapRef ^ to Size, if it is not already that big.
     Copy existing elements and initialize new ones to LbeStd . EstNodeNoNull.
     PRE: MapRef # NIL
  *) 

  = VAR LOldSize : LbeStd . TokTyp 
  ; VAR LResult : TokMapRefTyp 

  ; BEGIN (* ExpandTokMap *) 
      LOldSize := NUMBER ( MapRef ^ ) 
    ; IF Size > LOldSize 
      THEN 
        LResult := NEW ( TokMapRefTyp , Size ) 
      ; SUBARRAY ( LResult ^ , 0 , LOldSize ) := MapRef ^ 
      ; FOR I := LOldSize TO Size - 1 
        DO LResult ^ [ I ] := LbeStd . EstNodeNoNull 
        END (* FOR *) 
      ; MapRef := LResult 
      END (* IF *) 
    END ExpandTokMap 

(* VISIBLE: *) 
; PROCEDURE NewFsTreeMap ( Size : LbeStd . TokTyp ) : FsTreeMapRefTyp 
  (* A newly allocated FsTreeMap with Size elements, each initialized to NIL. *)

  = VAR LResult : FsTreeMapRefTyp 

  ; BEGIN (* NewFsTreeMap *) 
      LResult := NEW ( FsTreeMapRefTyp , Size ) 
    ; FOR I := 0 TO Size - 1 DO LResult ^ [ I ] := NIL END (* FOR *) 
    ; RETURN LResult 
    END NewFsTreeMap 

(* VISIBLE: *) 
; PROCEDURE ExpandFsTreeMap 
    ( VAR MapRef : FsTreeMapRefTyp ; Size : LbeStd . TokTyp ) 
  (* Expand MapRef ^ to Size, if it is not already that big.
     Copy existing elements and initialize new ones to NIL.
     PRE: MapRef # NIL
  *) 

  = VAR LOldSize : LbeStd . TokTyp 
  ; VAR LResult : FsTreeMapRefTyp 

  ; BEGIN (* ExpandFsTreeMap *) 
      LOldSize := NUMBER ( MapRef ^ ) 
    ; IF Size > LOldSize 
      THEN 
        LResult := NEW ( FsTreeMapRefTyp , Size ) 
      ; SUBARRAY ( LResult ^ , 0 , LOldSize ) := MapRef ^ 
      ; FOR I := LOldSize TO Size - 1 
        DO LResult ^ [ I ] := NIL  
        END (* FOR *) 
      ; MapRef := LResult 
      END (* IF *) 
    END ExpandFsTreeMap 

(* VISIBLE: *) 
; PROCEDURE NewStringMap ( Size : LbeStd . TokTyp ) : StringMapRefTyp 
  (* A newly allocated StringMap with Size elements, each initialized to NIL. *)

  = VAR LResult : StringMapRefTyp 

  ; BEGIN (* NewStringMap *) 
      LResult := NEW ( StringMapRefTyp , Size ) 
    ; FOR I := 0 TO Size - 1 DO LResult ^ [ I ] := NIL END (* FOR *) 
    ; RETURN LResult 
    END NewStringMap 

(* VISIBLE: *) 
; PROCEDURE InitLangInfo 
    ( Est : EstHs . EstRefTyp 
    ; LdlLang : LbeStd . LangTyp := LbeStd . LangNull 
    ; VAR Info : LangInfoRefTyp 
    ) 
  RAISES { Assertions . AssertionFailure } 
  (* Set initial values of fields of Info^. *) 

  = BEGIN (* InitLangInfo *) 
      Info . Root . NodeRef := Est 
    ; Info . Root . NodeNo := 0 
    ; Info . LdlLang := LdlLang 
    ; Assert 
        ( TRUE OR EstUtil . EstTok ( Info . Root . NodeRef ) 
          = Ldl0Tok . LanguageDefinition 
       (* OR EstUtil . EstTok ( Info . Root . NodeRef ) 
             = Ldl1Tok . LanguageDefinition *) 
(* FIX: *) 
        , AFT . A_Ldl0SemanticsInitLangInfo_WrongRoot 
        ) 
    ; Info . SemMapRef := InitSemMap ( Est ) 
    ; Info . StartRule := AstView . AstRefNull 
    ; Info . StartIdentNodeNo := LbeStd . EstNodeNoNull 
    ; Info . StartTok := LbeStd . Tok__Null 
    ; Info . PrecRule := AstView . AstRefNull 
    ; Info . SymbolTable := TextIntSymbolTable . New ( ) 
    ; Info . DefLangIdRef := NIL 
    ; Info . LdlLangIdRef := NIL 
    ; Info . TokMapRef := NIL 
    ; Info . FsTreeMapRef := NIL 
    ; Info . StringMapRef := NIL 
    ; Info . LanguageName := NIL 
    ; Info . TokCounts := NEW ( TokPartitionRefTyp ) 
    ; FOR RI := FIRST ( Info . TokCounts ^ ) TO LAST ( Info . TokCounts ^ ) 
      DO Info . TokCounts ^ [ RI ] := 0 
      END (* FOR *) 
    ; Info . TokCounts ^ [ TokClassBuiltin ] := LbeStd . Tok__FirstLangDep 
      (* ^Precount these. *) 
    ; Info . TokPart := NEW ( TokPartitionRefTyp ) 
    ; FOR RI := FIRST ( Info . TokPart ^ ) TO LAST ( Info . TokPart ^ ) 
      DO Info . TokPart ^ [ RI ] := LbeStd . Tok__Null  
      END (* FOR *) 
    ; Info . TokPart ^ [ TokClassBuiltin ] 
        := Info . TokCounts ^ [ TokClassBuiltin ] 
        (* ^Preload this. *) 
    ; Info . StringToks := LbeStd . Tok__FirstLangDep 
    ; Info . TokPart ^ [ TokClassConstTerm ] := LbeStd . Tok__FirstLangDep 
      (* ^Set up to assign token numbers in the first pass. *) 
    ; Info . VarTermToks := 0 
    ; Info . VarTermModToks := 0 
    ; Info . AsPlusToks := 0 
    ; Info . AsStarToks := 0 
    ; Info . AsFixedToks := 0 
    ; Info . AsSublistToks := 0 
    ; Info . AsListCardToks := 0 
    ; Info . AsPartialToks := 0 
    ; Info . AsClassOnlyToks := 0 
    ; Info . AsCsClassToks := 0 
    ; Info . CsAltToks := 0 
    ; Info . CsPlusToks := 0 
    ; Info . CsPluralToks := 0 
    ; Info . CsStarToks := 0 
    ; Info . CsFixedToks := 0 
    ; Info . GcsChildToks := 0 
    ; Info . Productions := 0 
      (* ^Other --Toks just count the tokens the earliest pass. *) 
    END InitLangInfo 

(* VISIBLE: *) 
; PROCEDURE MkAstRef 
    ( LangInfo : LangInfoRefTyp ; NodeNo : LbeStd . EstNodeNoTyp ) 
  : AstView . AstRefTyp 

(* CHECK: Not used as of 2002-1-15.  Check later and possibly eliminate. *) 

  = BEGIN (* MkAstRef *) 
      RETURN 
        AstView . AstRefTyp 
          { NodeNo := NodeNo 
          , NodeRef := LangInfo . SemMapRef ^ [ NodeNo ] . EstRef 
          , ChildNo := LbeStd . EstChildNoNull 
          } 
    END MkAstRef 

(* VISIBLE: *) 
; PROCEDURE MapAstRef 
    ( LangInfo : LangInfoRefTyp ; AstRef : AstView . AstRefTyp ) 
  RAISES { Assertions . AssertionFailure } 
  (* Store a rule in the SemMap of LangInfo, mapping the node number
     of AstRef to its node pointer, if non-NIL.
     PRE: the SemMap must be big enough.
     Can be recalled, but all calls must agree.
  *) 

  = BEGIN (* MapAstRef *) 
      IF AstRef . NodeRef # NIL 
      THEN 
        WITH WSemMapElem = LangInfo . SemMapRef ^ [ AstRef . NodeNo ] 
        DO IF WSemMapElem . EstRef = NIL 
           THEN 
             WSemMapElem . EstRef := AstRef . NodeRef 
           ELSE 
             Assert 
               ( WSemMapElem . EstRef = AstRef . NodeRef 
               , AFT . A_MapAstRef_ContradictoryNodeRef 
               ) 
           END (* IF *) 
        END (* WITH *) 
      END (* IF *) 
    END MapAstRef 

(* VISIBLE: *) 
; PROCEDURE MapAstRefs 
    ( LangInfo : LangInfoRefTyp 
    ; READONLY AstRefs : AstView . AstRefArrayTyp 
    ) 
  RAISES { Assertions . AssertionFailure } 
  (* For all element of AstRefs, call MapAstRef. *) 

  = BEGIN (* MapAstRefs *) 
      FOR I := 0 TO NUMBER ( AstRefs ) - 1 
      DO MapAstRef ( LangInfo , AstRefs [ I ] ) 
      END (* FOR *) 
    END MapAstRefs 

(* VISIBLE: *) 
; PROCEDURE SemDeclOfTok ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  : SemDeclTyp 
  (* The SemDecl of the declaration of Tok.  NIL if anything wrong. *) 

  = VAR LBiasedTok : PortTypes . Int32Typ 
  ; LEstNodeNo : LbeStd . EstNodeNoTyp 
  ; LResult : SemDeclTyp 

  ; BEGIN 
      LBiasedTok := Tok - TokMapBias 
    ; IF LBiasedTok < 0 OR LBiasedTok >= NUMBER ( LangInfo . TokMapRef ^ ) 
      THEN RETURN NIL 
      ELSE 
        LEstNodeNo := LangInfo . TokMapRef ^ [ LBiasedTok ] 
      ; IF LEstNodeNo = LbeStd . EstNodeNoNull 
           OR LEstNodeNo < 0 
           OR LEstNodeNo >= NUMBER ( LangInfo . SemMapRef ^ ) 
        THEN RETURN NIL 
        ELSE 
          LResult := LangInfo . SemMapRef ^ [ LEstNodeNo ] . SemRef  
        ; RETURN LResult 
        END (* IF *) 
      END (* IF *) 
    END SemDeclOfTok 

; PROCEDURE FilterForAsToks 
    ( LangInfo : LangInfoRefTyp ; VAR Set : IntSets . T  ) 

  = VAR LSet1 : IntSets . T  
  ; VAR LSet2 : IntSets . T  

  ; BEGIN 
      LSet1 := IntSets . Project 
        ( Set 
        , LangUtilLo . TokClassFirstTok ( LangInfo , TokClassVarTerm )
        , LangUtilLo . TokClassLastTok ( LangInfo , TokClassVarTerm )
        )
    ; LSet2 := IntSets . Project 
        ( Set 
        , LangUtilLo . TokClassFirstTok ( LangInfo , TokClassAsPlus )
        , LangUtilLo . TokClassLastTok ( LangInfo , TokClassAsFixed ) 
        )
    ; LSet1 := IntSets . Union ( LSet1, LSet2 ) 
    ; IF NOT IntSets . Equal ( LSet1 , Set ) 
      THEN Set := LSet1 
      END (* IF *) 
    END FilterForAsToks 

; PROCEDURE AsTokSetWTrailing 
    ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  : IntSets . T  
  (* If Tok as an As list token for a list that can have trailing separators, 
     include the trailing list tok.  Tok is always a member. 
  *) 
(* TODO: ^Put this in LangUtilLo.  Also, make it work both ways. *) 

  = VAR LTrailingCounterpart : LbeStd . TokTyp 

  ; BEGIN 
      LTrailingCounterpart 
        := LangUtilLo . TrailingCounterpart ( LangInfo , Tok ) 
    ; IF LTrailingCounterpart = Tok 
      THEN RETURN IntSets . Singleton ( Tok ) 
      ELSE 
        RETURN 
          IntSets . FromArray 
            ( ARRAY OF IntSets . ElemT { Tok , LTrailingCounterpart } )
      END (* FI *) 
    END AsTokSetWTrailing 

(* VISIBLE: *) 
; PROCEDURE TokSetOfTok 
    ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  : IntSets . T  
  (* Set of all tokens denoted by Tok.  Could be plural if Tok is a class.  
     Empty if anything wrong.  
  *) 

  = VAR LResult : IntSets . T  

  ; BEGIN 
      TYPECASE SemDeclOfTok ( LangInfo , Tok )  
      OF NULL => RETURN NIL 

      | SemFirstOccClassTyp ( TSemClassDecl ) 
      => LResult := TSemClassDecl . TokSet  
      ; RETURN LResult 

      | SemDeclTyp 
      => LResult := AsTokSetWTrailing ( LangInfo , Tok ) 
      ; RETURN LResult 

      END (* TYPECASE *) 
    END TokSetOfTok

(* VISIBLE: *) 
; PROCEDURE AsTokSetOfTok 
    ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  : IntSets . T  
  (* Set of AS tokens denoted by Tok.  Could be plural if Tok is a class.  
     Omit everything that is not an AS token. Empty if anything wrong.  
  *) 

  = VAR LResult : IntSets . T  

  ; BEGIN 
      TYPECASE SemDeclOfTok ( LangInfo , Tok )  
      OF NULL => RETURN NIL 

      | SemFirstOccClassTyp ( TSemClassDecl ) 
      => LResult := TSemClassDecl . TokSet  
      ; FilterForAsToks ( LangInfo , (* VAR *) LResult ) 
      ; RETURN LResult 

      | SemDeclTyp 
      => LResult := AsTokSetWTrailing ( LangInfo , Tok ) 
      ; RETURN LResult 

      END (* TYPECASE *) 
    END AsTokSetOfTok

(* VISIBLE: *) 
; PROCEDURE AsTokSetOfAstRef 
    ( LangInfo : LangInfoRefTyp ; AstRef : AstView . AstRefTyp ) 
  : IntSets . T  
  (* If AstRef denotes a class or token, return its token set after removing
     everything that is not an AS token, else Empty. 
  *) 

  = VAR LSemDecl : SemDeclTyp 
  ; VAR LResult : IntSets . T  

  ; BEGIN 
      LSemDecl := DeclOfRef ( LangInfo , AstRef ) 
    ; TYPECASE LSemDecl 
      OF NULL => RETURN NIL 

      | SemFirstOccClassTyp ( TSemClassDecl ) 
      => LResult := TSemClassDecl . TokSet  
      ; FilterForAsToks ( LangInfo , (* VAR *) LResult ) 
      ; RETURN LResult 

      | SemDeclTyp ( TSemDecl ) 
      => LResult := AsTokSetWTrailing ( LangInfo , TSemDecl . DeclTok ) 
      ; RETURN LResult 

      END (* TYPECASE *) 
    END AsTokSetOfAstRef 

(* VISIBLE: *) 
; PROCEDURE FsElemOfFsList 
    ( FsListNodeRef : LangUtil . FsNodeRefTyp ) : LangUtil . FsNodeRefTyp 
  (* If FsListNodeRef is a list node, return its immediate child Fs node that
     contains its list element.  Do not verify FsKind of result.  
     NIL if anything is wrong. 
  *) 

(* CHECK: Do we still need this, or has field FsEstDescendantRef 
          obviated it? *)

  = BEGIN
      IF FsListNodeRef = NIL 
      THEN RETURN NIL
      ELSE 
        CASE FsListNodeRef . FsKind 
        OF FsKindTyp . FsKindEstListHoriz  
        , FsKindTyp . FsKindEstListVert  
        , FsKindTyp . FsKindEstListFill   
        , FsKindTyp . FsKindEstListTrailHoriz  
        , FsKindTyp . FsKindEstListTrailVert  
        , FsKindTyp . FsKindEstListTrailFill   
        => IF FsListNodeRef . FsChildren = NIL 
          THEN RETURN NIL 
          ELSIF NUMBER ( FsListNodeRef . FsChildren ^ ) < 1 
          THEN RETURN NIL 
          ELSE RETURN FsListNodeRef . FsChildren ^ [ 0 ]  
          END (* IF *) 
        ELSE RETURN NIL
        END (* CASE *) 
      END (* IF *) 
    END FsElemOfFsList 

(* VISIBLE: *) 
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

  = VAR LResult : LangUtil . FsNodeRefTyp

  ; BEGIN
      LResult := FsElemOfFsList ( FsListNodeRef ) 
    ; IF LResult # NIL 
      THEN 
        LOOP 
          CASE LResult . FsKind <* NOWARN *>  
          OF FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill 
          => LResult := LResult . FsEstDescendantRef
             (* And loop. *) 

          | FsKindTyp . FsKindCondFmt 
          , FsKindTyp . FsKindEstChildOfFixed 
          , FsKindTyp . FsKindEstChildOfList 
          => EXIT 

       (* ELSE can't happen. *)  
          END (* CASE *) 
        END (* LOOP *) 
      END (* IF *) 
    ; RETURN LResult 
    END FsCondOrEstOfFsList 

(* VISIBLE: *) 
; PROCEDURE EstNodeNoOfTokDecl  
    ( LangInfo : LangInfoRefTyp ; Tok : LbeStd . TokTyp ) 
  : LbeStd . EstNodeNoTyp 
  (* The EstNodeNo of the LDL declaration of Tok.  
     LbeStd . EstNodeNoNull if anything wrong. 
  *) 

  = VAR LBiasedTok : PortTypes . Int32Typ 

  ; BEGIN 
      LBiasedTok := Tok - TokMapBias 
    ; IF LBiasedTok < 0 OR LBiasedTok >= NUMBER ( LangInfo . TokMapRef ^ ) 
      THEN RETURN LbeStd . EstNodeNoNull  
      ELSE 
        RETURN LangInfo . TokMapRef ^ [ LBiasedTok ] 
      END (* IF *) 
    END EstNodeNoOfTokDecl  

(* VISIBLE: *)
; PROCEDURE FsRuleForTokUsingLangInfo  
    ( LangInfo : LangInfoRefTyp 
    ; EstTok : LbeStd . TokTyp 
    ; IsPlaceholder : BOOLEAN := FALSE 
    ) 
  : LangUtil . FsNodeRefTyp 
  (* The FsNodeRef of the root of the format tree for an Est node. 
     Also works when EstTok is a list partial token. 
     Does NOT handle standard tokens. 
  *)

  = VAR LEstTok : LbeStd . TokTyp 
  ; VAR LResult : LangUtil . FsNodeRefTyp 

  ; BEGIN 
      IF LangInfo = NIL
      THEN (* This happens during Ldl bootstrap *)
        RETURN NIL
      ELSE
(* TODO: Get the partial tokens into FsTreeMapRef^ and eliminate the
       following special case.
*) 
        LEstTok := LangUtilLo . ListTokOfPartialTok ( LangInfo , EstTok ) 
      ; IF LEstTok < LangInfo ^ . AsPartialToks
        THEN
          LResult 
            := LangInfo ^ . FsTreeMapRef ^
                 [ LEstTok - LbeStd . Tok__FirstLangDep ]
        ; IF IsPlaceholder 
             AND LResult . FsKind IN LangUtil . FsKindSetEstInterior 
          THEN 
            RETURN LResult . FsPlaceholder 
          ELSE RETURN LResult 
          END (* IF *) 
        ELSE
          RETURN NIL
        END (* IF *)
      END (* IF *)
    END FsRuleForTokUsingLangInfo 

(* VISIBLE: *) 
; PROCEDURE PostNumberFsTree 
    ( Root : LangUtil . FsNodeRefTyp 
    ; InitialNo : LangUtil . FsNodeNoTyp := 0 
    ) 
  RAISES { NodeNoOverflow }  
  (* Fill in the FsPostNodeNo fields of an entire tree,
     as postorder node numbers, starting with InitialNo.
  *) 

  = VAR PnMaxAssigned : PortTypes . Int32Typ := InitialNo - 1  

  ; PROCEDURE Recurse ( Node : LangUtil . FsNodeRefTyp ) 

    = VAR LChildren : LangUtil . FsChildrenArrayRefTyp 

    ; BEGIN 
        IF Node # NIL AND Node . FsPostNodeNo = LangUtil . FsNodeNoNull 
        THEN 
          (* Do any alternatives. *) 
          IF Node . FsKind = LangUtil . FsKindTyp . FsKindCondFmt 
             AND LangUtil . FsEstChildRef ( Node ) # Node . FsCondAltRef 
          THEN
            Recurse ( Node . FsCondAltRef ) 
          END (* IF *) 

        (* Do the children. *) 
        ; LChildren := Node . FsChildren 
        ; IF LChildren # NIL 
          THEN 
            FOR RI := 0 TO NUMBER ( LChildren ^ ) - 1 
            DO 
              Recurse ( LChildren ^ [ RI ] ) 
            END (* FOR *) 
          END (* IF *) 

        (* Do this node. *) 
        ; INC ( PnMaxAssigned ) 
        ; IF PnMaxAssigned <= LangUtil . FsNodeNoMax 
          THEN 
            Node . FsPostNodeNo := PnMaxAssigned  
          ELSE
            Node . FsPostNodeNo := LangUtil . FsNodeNoOvflo 
          END (* IF *) 
        END (* IF *) 
      END Recurse 

  ; BEGIN 
      Recurse ( Root ) 
    ; IF PnMaxAssigned > LangUtil . FsNodeNoMax 
      THEN RAISE NodeNoOverflow ( PnMaxAssigned ) 
      END (* IF *) 
    END PostNumberFsTree

(* VISIBLE: *) 
; PROCEDURE InsertionString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Assertions . AssertionFailure } 
  (* Convert a quoated string, as in an Ldl definition, into the string
     to be displayed, by removing quotes and escape sequences.
     PRE: LdlString is quoted. 
  *) 

  = VAR LLength := SharedStrings . Length ( LdlString ) 
  ; VAR LLeftQuote : CHAR 
  ; VAR LRightQuote : CHAR 
  ; VAR LText : TEXT 
  ; VAR LUnescapedText : TEXT 

  ; BEGIN (* InsertionString *) 
      Assert ( LLength > 2 , AFT . A_InsertionStringTooShort ) 
    ; TRY 
        LLeftQuote := SharedStrings . IthChar ( LdlString , 0 ) 
      ; LRightQuote := SharedStrings . IthChar ( LdlString , LLength - 1 ) 
      EXCEPT SharedStrings . SsOutOfBounds 
      => RAISE AssertionFailure ( "SharedStrings.SsOutOfBounds" )  
      END (* TRY EXCEPT *) 
    ; Assert 
        ( LLeftQuote = '"' AND LRightQuote = '"' 
        , AFT . A_InsertionStringNotQuoted 
        ) 
(* TODO: Sheesh.  There has to be a more direct way. *) 
    ; LText 
        := Strings . ToText 
             ( Strings . Substring 
                 ( SharedStrings . ToString ( LdlString ) , 1 , LLength - 2 ) 
             ) 
    ; LUnescapedText := Misc . UnescapeText ( LText ) 
    ; RETURN 
        SharedStrings . FromText ( LUnescapedText , Tok ) 
    END InsertionString 

; VAR LeftPlaceholderDelimLen 
        := Text . Length ( LbeStd . LeftPlaceholderDelimText )
; VAR RightPlaceholderDelimLen 
        := Text . Length ( LbeStd . RightPlaceholderDelimText )

; PROCEDURE IsPlaceholderText ( TString : TEXT ) : BOOLEAN 
  (* Result is TString begins and ends with placeholder delimiters. 
     PRE: TString # NIL.
  *) 

  = VAR LStringLen : CARDINAL 

  ; BEGIN 
      LStringLen := Text . Length ( TString ) 
    ; RETURN 
        LStringLen >= LeftPlaceholderDelimLen + RightPlaceholderDelimLen 
        AND Text . Equal 
              ( Text . Sub ( TString , 0 , LeftPlaceholderDelimLen ) 
              , LbeStd . LeftPlaceholderDelimText 
              ) 
        AND Text . Equal 
              ( Text . Sub 
                  ( TString 
                  , LStringLen - RightPlaceholderDelimLen  
                  , RightPlaceholderDelimLen 
                  ) 
              , LbeStd . RightPlaceholderDelimText 
              ) 
    END IsPlaceholderText 

(* VISIBLE: *) 
; PROCEDURE RemovePlaceholderDelimsText ( FText : TEXT ) : TEXT 
  (* Remove placeholder delimiters, if any, from FText. *) 

  = VAR LTextLen : CARDINAL 
  ; VAR LResult : TEXT 

  ; BEGIN 
      IF IsPlaceholderText ( FText ) 
      THEN 
        LTextLen := Text . Length ( FText ) 
      ; LResult 
          := Text . Sub 
               ( FText 
               , LeftPlaceholderDelimLen 
               , LTextLen - LeftPlaceholderDelimLen - LeftPlaceholderDelimLen 
               ) 
      ; RETURN LResult 
      ELSE 
        RETURN FText 
      END (* IF *) 
    END RemovePlaceholderDelimsText

(* VISIBLE: *) 
; PROCEDURE PlaceholderString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Assertions . AssertionFailure } 
  (* Prepend and append placeholder delimiters to LdlString, if they
     are not already there.
  *) 

  = VAR LText : TEXT 
  ; VAR LString : Strings . T 

  ; BEGIN (* PlaceholderString *) 
      LText := SharedStrings . ToText ( LdlString ) 
    ; IF IsPlaceholderText ( LText ) 
      THEN 
        RETURN LdlString 
      ELSE 
        LString 
          := Strings . FromText  
               ( LbeStd . LeftPlaceholderDelimText  
               , EventualLengthHint 
                   := SharedStrings . Length ( LdlString )   
                      + LeftPlaceholderDelimLen 
                      + RightPlaceholderDelimLen 
               ) 
      ; Strings . AppendTextInPlace ( LString , LText ) 
      ; Strings . AppendTextInPlace 
          ( LString , LbeStd . RightPlaceholderDelimText ) 
      ; RETURN SharedStrings . FromString ( LString , Tok ) 
      END (* IF *) 
    END PlaceholderString 

; PROCEDURE GeneratedString 
    ( LdlString : SharedStrings . T ; Suffix : TEXT ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Assertions . AssertionFailure } 
  (* Append Suffix to LdlString, but inside placeholder delimiters, if any. *) 

  = VAR LName : TEXT 
  ; VAR LResult : TEXT 
  ; VAR LLen : INTEGER   

  ; BEGIN (* GeneratedString *) 
      LName := SharedStrings . ToText ( LdlString ) 
    ; LLen := Text . Length ( LName )
    ; IF IsPlaceholderText ( LName ) 
      THEN 
        LName 
          := Text . Sub 
               ( LName 
               , LeftPlaceholderDelimLen 
               , LLen - LeftPlaceholderDelimLen - RightPlaceholderDelimLen
               ) 
      ; LLen := Text . Length ( LName )
      END (* IF *) 
    ; LResult 
        := LbeStd . LeftPlaceholderDelimText
           & LName  
           & Suffix 
           & LbeStd . RightPlaceholderDelimText   
    ; RETURN SharedStrings . FromText ( LResult , Tok ) 
    END GeneratedString 

(* The following procedures append a standard suffix to a token string,
   to produce a token string for a generated token.   
   But "append" the suffix inside placeholder delimiters, if any.
*) 

; CONST ModTokSuffix = "__ModTok" 

(* VISIBLE: *) 
; PROCEDURE ModTokString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { AssertionFailure }

  = BEGIN 
      RETURN GeneratedString ( LdlString , ModTokSuffix , Tok )   
    END ModTokString  

(* VISIBLE: *)
; PROCEDURE ModTokText ( FString : SharedStrings . T ) : TEXT 
(* TODO: Unify this with LdlSemantics.GeneratedString. *) 

  = VAR LName : TEXT 
  ; VAR LLen : INTEGER 
  ; VAR LResult : TEXT 

  ; BEGIN 
      LName := SharedStrings . ToText ( FString ) 
    ; LLen := Text . Length ( LName )
    ; IF IsPlaceholderText ( LName ) 
      THEN 
        LResult 
          := Text . Sub 
               ( LName 
               , 0  
               , LLen - RightPlaceholderDelimLen 
               ) 
             & ModTokSuffix 
             & LbeStd . RightPlaceholderDelimText  
      ELSE
        LResult 
          := LbeStd . LeftPlaceholderDelimText  
             & LName 
             & ModTokSuffix 
             & LbeStd . RightPlaceholderDelimText   
      END (* IF *) 
    ; RETURN LResult 
    END ModTokText  

; CONST SublistSuffix = "__Sub" 

(* VISIBLE: *) 
; PROCEDURE SublistString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Assertions . AssertionFailure }

  = BEGIN 
      RETURN GeneratedString ( LdlString , SublistSuffix , Tok ) 
    END SublistString  

; CONST EmptyListSuffix = "__0" 

(* VISIBLE: *) 
; PROCEDURE EmptyListString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Assertions . AssertionFailure } 

  = BEGIN 
      RETURN GeneratedString ( LdlString , EmptyListSuffix , Tok ) 
    END EmptyListString  

; CONST SingletonListSuffix = "__1" 

(* VISIBLE: *) 
; PROCEDURE SingletonListString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Assertions . AssertionFailure } 

  = BEGIN 
      RETURN GeneratedString ( LdlString , SingletonListSuffix , Tok ) 
    END SingletonListString  

; CONST PluralListSuffix = "__2" 

(* VISIBLE: *) 
; PROCEDURE PluralListString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Assertions . AssertionFailure } 

  = BEGIN 
      RETURN GeneratedString ( LdlString , PluralListSuffix , Tok ) 
    END PluralListString  

; CONST PartialSuffix = "__Part" 

(* VISIBLE: *) 
; PROCEDURE PartialString 
    ( LdlString : SharedStrings . T ; Tok : LbeStd . TokTyp ) 
  : SharedStrings . T 
  RAISES { Assertions . AssertionFailure } 

  = BEGIN 
      RETURN GeneratedString ( LdlString , PartialSuffix , Tok ) 
    END PartialString  

(* VISIBLE: *) 
; PROCEDURE IncIndentCode 
    ( VAR (* IN OUT *) Code : LangUtil . IndentCodeTyp 
    ; Incr : PortTypes . Int32Typ 
    ) 
  (* Saturates at LangUtil . IndentCodeFirstNormal and 
     LangUtil . IndentCodeLastNormal 
  *) 

  = BEGIN (* IncIndentCode *) 
      IF Incr < 0 
      THEN 
        IF Code >= LangUtil . IndentCodeFirstNormal - Incr 
        THEN 
          INC ( Code , Incr ) 
        ELSE 
          Code := LangUtil . IndentCodeFirstNormal
        END (* IF *) 
      ELSE
        IF Code <= LangUtil . IndentCodeLastNormal - Incr 
        THEN 
          INC ( Code , Incr ) 
        ELSE 
          Code := LangUtil . IndentCodeLastNormal 
        END (* IF *) 
      END (* IF *) 
    END IncIndentCode 

(* VISIBLE: *) 
; PROCEDURE PatchListFields
    ( FsNodeRef : LangUtil . FsNodeRefTyp 
    ; FsListNodeRef : LangUtil . FsNodeRefTyp 
    ; IndentCode : LangUtil . IndentCodeTyp 
    )  
  (* Copy fields FsListSliceThruFmtNo
                 FsSublistTok
                 FsPartialTok
                 FsTrailingTok 
                 FsEmptyListTok
                 FsSingletonListTok
                 FsPluralListTok 
     down thru all descendents.
     Also set FsIndentCode of any FsEstChildOfList nodes to IndentCode. 
  *) 

  = BEGIN 
      IF FsNodeRef # NIL 
      THEN 
        FsNodeRef . FsListSliceThruFmtNo 
          := FsListNodeRef . FsListSliceThruFmtNo 
      ; FsNodeRef . FsSublistTok := FsListNodeRef . FsSublistTok  
      ; FsNodeRef . FsPartialTok := FsListNodeRef . FsPartialTok  
      ; FsNodeRef . FsSublistTok := FsListNodeRef . FsSublistTok  
      ; FsNodeRef . FsEmptyListTok := FsListNodeRef . FsEmptyListTok  
      ; FsNodeRef . FsSingletonListTok := FsListNodeRef . FsSingletonListTok 
      ; FsNodeRef . FsPluralListTok := FsListNodeRef . FsPluralListTok  
      ; IF FsNodeRef . FsKind = LangUtil . FsKindTyp . FsKindEstChildOfList 
        THEN 
          FsNodeRef . FsIndentCode := IndentCode 
        END (* IF *) 
      ; IF FsNodeRef . FsChildren # NIL 
        THEN
          FOR RFsChildSs := 0 TO NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
          DO
            WITH WFsChildRef = FsNodeRef . FsChildren ^ [ RFsChildSs ] 
            DO
              PatchListFields ( WFsChildRef ,  FsListNodeRef , IndentCode ) 
            END (* WITH *) 
          END (* FOR *) 
        END (* IF *) 
      ; PatchListFields 
         ( FsNodeRef . FsCondAltRef ,  FsListNodeRef , IndentCode )
      END (* IF *) 
    END PatchListFields 

(* VISIBLE: *) 
; PROCEDURE WriteLdlTokInterfaceToStream  
    ( Writer : Wr . T 
    ; LangInfo : LangInfoRefTyp 
    ; InterfaceName : TEXT 
    ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a token-declaring interface to an already-open Wr.T *) 

  = PROCEDURE PT ( Txt : TEXT ) 
    RAISES { Thread . Alerted , Wr . Failure } 

    = BEGIN (* PT *) 
        Wr . PutText ( Writer , Txt ) 
      END PT 

  ; PROCEDURE PTL ( Txt : TEXT ) 
    RAISES { Thread . Alerted , Wr . Failure } 

    = BEGIN (* PTL *) 
        PT ( Txt ) 
      ; Wr . PutText ( Writer , Wr . EOL ) 
      END PTL 

  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR MinTok : PortTypes . Int32Typ 
  ; VAR MaxTok : PortTypes . Int32Typ 

  ; BEGIN (* WriteLdlTokInterfaceToStream *) 
      IF Writer # NIL AND LangInfo # NIL AND InterfaceName # NIL 
      THEN 
        PTL ( "" )     
      ; PTL ( "(* INTERFACE " & InterfaceName 
              & ", Ldl tokens, mechanically generated by LdlSemantics. " 
            ) 
      ; PTL ( "     Generated on:  " 
              & Misc . DateImage 
                  ( Date . FromTime ( Time . Now ( ) , z := Date . UTC ) ) 
            ) 
      ; IF LangInfo . DefLangIdRef # NIL 
        THEN
          PTL ( "     " 
                & LangUtil . LangIdImage 
                    ( LangInfo . DefLangIdRef ^ , Indent := 5 ) 
              )  
        END 
      ; PTL ( "*) " ) 
      ; PTL ( "" ) 
      ; PTL ( "INTERFACE " & InterfaceName & " " ) 
      ; PTL ( "" ) 
      ; PTL ( "; IMPORT LbeStd  " ) 
      ; MinTok := LAST ( PortTypes . Int32Typ ) 
      ; MaxTok := FIRST ( PortTypes . Int32Typ ) 
      ; WITH WTokMap = LangInfo . TokMapRef ^ 
        DO FOR FTok := LbeStd . Tok__FirstLangDep TO LangInfo . CsFixedToks - 1 
           DO IF FTok = LbeStd . Tok__FirstLangDep 
              THEN 
                PTL ( "" ) 
              ; PTL ( "(* Names for string tokens: *) " ) 
              END (* IF *) 
           ; IF FTok = LangInfo . StringToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Variable terminal tokens: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . VarTermToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Tokens for ModTok nodes for variable terminals: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . VarTermModToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Abstract Plus Nodes: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . AsPlusToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Abstract Star Nodes: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . AsStarToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Abstract Fixed Nodes: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . AsFixedToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Sublist Tokens: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . AsSublistToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* List Cardinality Tokens: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . AsListCardToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Partial List Tokens: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . AsPartialToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Abstract-only Classes: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . AsClassOnlyToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Classes that are also Concrete alternations: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . AsCsClassToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Concrete Alternation Tokens: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . CsAltToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Concrete Plus Tokens: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . CsPlusToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Concrete Plural Tokens: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . CsPluralToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Concrete Star Tokens: *) " ) 
             END (* IF *) 
           ; IF FTok = LangInfo . CsStarToks 
             THEN 
               PTL ( "" ) 
             ; PTL ( "(* Concrete Fixed Tokens: *) " ) 
             END (* IF *) 
           ; IF LangInfo . VarTermToks <= FTok 
                AND FTok < LangInfo . VarTermModToks 
             THEN (* This is a ModTok subtree token. *) 
               LNodeNo 
                 := WTokMap 
                      [ FTok - LangInfo . VarTermToks + LangInfo . StringToks
                        - TokMapBias 
                      ] 
             ; TYPECASE LangInfo . SemMapRef ^ [ LNodeNo ] . SemRef 
               OF NULL => 
  (* TODO: Now that VarTermModToks are in the StringMap, make this work like the
           other generated tokens and then remove ModTokText 
  *) 
  (* TODO: Make an Ldl[0|1] rule that two successive underscores are not allowed
           in an Ldl identifier.  Then generated names with two underscores will
           be both legal in M3 and unable to be spoofed by Ldl code.  
           Then remove the placeholder delimiters from generated token names
           here, and the comment delimiters, so they become real M3 identifiers
           in the generated *Tok.i3 interface.  Just to be thorough.
  *) 
               | SemDeclTyp ( TSemDecl ) 
               => TYPECASE 
                    LangInfo . SemMapRef ^ [ TSemDecl . NodeNo ] . EstRef 
                  OF SharedStrings . T ( TSharedString ) 
                  => PT ( "(* " ) 
                  ; PT ( ModTokText ( TSharedString ) ) 
                  ; PT ( " = " ) 
                  ; PT ( Fmt . Int ( FTok ) ) 
                  ; PTL ( " *)" ) 
                  ELSE 
                  END (* TYPECASE *) 
               ; MinTok := MIN ( MinTok , TSemDecl . DeclTok ) 
               ; MaxTok := MAX ( MinTok , TSemDecl . DeclTok ) 
               ELSE 
               END (* TYPECASE *) 
             ELSIF LangUtilLo . TokHasClass 
                     ( LangInfo , FTok , TokClassSublist )
             THEN (* This is a sublist token. *) 
               LNodeNo := WTokMap [ FTok - TokMapBias ] 
             ; TYPECASE LangInfo . SemMapRef ^ [ LNodeNo ] . SemRef 
               OF NULL => 
               | SemDeclAsListNodeTyp ( TSemDecl ) 
               => PT ( "(* " ) 
               ; PT 
                   ( SharedStrings . ToText 
                       ( LangInfo . StringMapRef ^ 
                           [ TSemDecl . SublistTok - StringMapBias ] 
                       ) 
                   ) 
               ; PT ( " = " ) 
               ; PT ( Fmt . Int ( TSemDecl . SublistTok ) ) 
               ; TYPECASE 
                    LangInfo . SemMapRef ^ [ TSemDecl . NodeNo ] . EstRef 
                  OF NULL => 
                  | SharedStrings . T ( TSharedString ) 
                  => PT ( ", Sublist nonterminal for " ) 
                  ; PT ( SharedStrings . ToText ( TSharedString ) ) 
                  ELSE 
                  END (* TYPECASE *) 
               ; PTL ( " *) " ) 
               ; MinTok := MIN ( MinTok , TSemDecl . SublistTok ) 
               ; MaxTok := MAX ( MinTok , TSemDecl . SublistTok ) 
               ELSE 
               END (* TYPECASE *) 
             ELSIF LangInfo . AsSublistToks <= FTok 
                   AND FTok < LangInfo . AsListCardToks 
             THEN (* This is a list cardinality token. *) 
               LNodeNo := WTokMap [ FTok - TokMapBias ] 
             ; TYPECASE LangInfo . SemMapRef ^ [ LNodeNo ] . SemRef 
               OF NULL => 
               | SemDeclAsListNodeTyp ( TSemDecl ) 
               => PT ( "(* " ) 
               ; PT 
                   ( SharedStrings . ToText 
                       ( LangInfo . StringMapRef ^ 
                           [ TSemDecl . DeclTok - StringMapBias ] 
                       ) 
                   ) 
               ; IF FTok 
                    = TSemDecl . ListCardToks [ ListCardTyp . ListCardEmpty ] 
                 THEN PT ( EmptyListSuffix ) 
                 ELSIF FTok 
                       = TSemDecl . ListCardToks 
                           [ ListCardTyp . ListCardSingleton ]
                 THEN PT ( SingletonListSuffix ) 
                 ELSIF FTok 
                       = TSemDecl . ListCardToks 
                           [ ListCardTyp . ListCardPlural ]
                 THEN PT ( PluralListSuffix ) 
                 END (* IF  *) 
               ; PT ( " = " ) 
               ; PT ( Fmt . Int ( FTok ) ) 
               ; PTL ( " *) " ) 
               ; MinTok := MIN ( MinTok , FTok ) 
               ; MaxTok := MAX ( MinTok , FTok ) 
               ELSE 
               END (* TYPECASE *) 
             ELSIF LangInfo . AsListCardToks <= FTok 
                   AND FTok < LangInfo . AsPartialToks 
             THEN (* This is a partial list token. *) 
               LNodeNo := WTokMap [ FTok - TokMapBias ] 
             ; TYPECASE LangInfo . SemMapRef ^ [ LNodeNo ] . SemRef 
               OF NULL => 
               | SemDeclAsListNodeTyp ( TSemDecl ) 
               => PT ( "(* " ) 
               ; PT 
                   ( SharedStrings . ToText 
                       ( LangInfo . StringMapRef ^ 
                           [ TSemDecl . PartialTok - StringMapBias ] 
                       ) 
                   ) 
               ; PT ( " = " ) 
               ; PT ( Fmt . Int ( TSemDecl . PartialTok ) ) 
               ; TYPECASE 
                    LangInfo . SemMapRef ^ [ TSemDecl . NodeNo ] . EstRef 
                  OF NULL => 
                  | SharedStrings . T ( TSharedString ) 
                  => PT ( ", Partial list nonterminal for " ) 
                  ; PT ( SharedStrings . ToText ( TSharedString ) ) 
                  ELSE 
                  END (* TYPECASE *) 
               ; PTL ( " *) " ) 
               ; MinTok := MIN ( MinTok , TSemDecl . PartialTok ) 
               ; MaxTok := MAX ( MinTok , TSemDecl . PartialTok ) 
               ELSE 
               END (* TYPECASE *) 
             ELSE 
               LNodeNo := WTokMap [ FTok - TokMapBias ] 
             ; TYPECASE LangInfo . SemMapRef ^ [ LNodeNo ] . SemRef 
               OF SemFirstOccStringTyp ( TStringDecl ) 
               => TYPECASE 
                    LangInfo . SemMapRef ^ [ TStringDecl . NodeNo ] . EstRef 
                  OF SharedStrings . T ( TString ) 
                  => IF TStringDecl . AltName # LbeStd . EstNodeNoNull 
                     THEN 
                       TYPECASE 
                         LangInfo . SemMapRef ^ [ TStringDecl . AltName ] 
                         . SemRef 
                       OF SemDeclTyp ( TNameDecl ) 
                       => TYPECASE 
                            LangInfo . SemMapRef ^ [ TNameDecl . NodeNo ] 
                            . EstRef 
                          OF SharedStrings . T ( TName ) 
                          => PT ( "; CONST " ) 
                          ; PT ( SharedStrings . ToText ( TName ) ) 
                          ; PT ( "_Tok = " ) 
                          ; PT ( Fmt . Int ( TStringDecl . DeclTok ) ) 
                          ; PT ( " (* = " ) 
                          ; PT ( SharedStrings . ToText ( TString ) ) 
                          ; PTL ( " *) " ) 
                          ELSE 
                          END (* TYPECASE *) 
                       ELSE 
                       END (* TYPECASE *) 
                     ELSE 
                       PT ( "(* Anonymous string token " ) 
                     ; PT ( SharedStrings . ToText ( TString ) ) 
                     ; PT ( " = " ) 
                     ; PT ( Fmt . Int ( TStringDecl . DeclTok ) ) 
                     ; PTL ( " *) " ) 
                     END (* IF *) 
                  ELSE 
                  END (* TYPECASE *) 
               ; MinTok := MIN ( MinTok , TStringDecl . DeclTok ) 
               ; MaxTok := MAX ( MinTok , TStringDecl . DeclTok ) 
               | SemDeclTyp ( TSemDecl ) 
               => TYPECASE 
                    LangInfo . SemMapRef ^ [ TSemDecl . NodeNo ] . EstRef 
                  OF SharedStrings . T ( TSharedString ) 
                  => PT ( "; CONST " ) 
                  ; PT ( SharedStrings . ToText ( TSharedString ) ) 
                  ; PT ( " = " ) 
                  ; PT ( Fmt . Int ( TSemDecl . DeclTok ) ) 
                  ; TYPECASE TSemDecl 
                    OF SemFirstOccClassTyp ( TClassDecl ) 
                    => IF LbeStd . Tok__FirstLangDep 
                          <= TClassDecl . SingletonTok 
                          AND TClassDecl . SingletonTok 
                              < LangInfo . StringToks 
                       THEN (* A singleton class rename for a string. *) 
                         PT ( " (* Name for " ) 
                       ; PT 
                           ( Misc . QuoteText 
                               ( SharedStrings . ToText 
                                   ( LangInfo . StringMapRef ^ 
                                       [ TClassDecl . SingletonTok 
                                         - StringMapBias 
                                       ]    
                                   ) 
                               ) 
                           ) 
                       ; PT ( " *) " ) 
                       END (* IF *) 
                    ELSE 
                    END (* TYPECASE *) 
                  ; PTL ( "" ) 
                  ELSE 
                  END (* TYPECASE *) 
               ; MinTok := MIN ( MinTok , TSemDecl . DeclTok ) 
               ; MaxTok := MAX ( MinTok , TSemDecl . DeclTok ) 
               ELSE 
               END (* TYPECASE *) 
             END (* IF *) 
           END (* FOR *) 
        END (* WITH *) 
      ; PTL ( "" ) 
      ; PTL ( "(* Limits: *) " ) 
      ; PT ( "; CONST MinTok = " ) 
      ; PTL ( Fmt . Int ( MinTok ) ) 
      ; PT ( "; CONST MaxTok = " ) 
      ; PTL ( Fmt . Int ( MaxTok ) ) 
      ; PTL ( "" ) 
      ; PTL ( "; PROCEDURE ToText ( Tok : LbeStd . TokTyp ) : TEXT " ) 
      ; PTL ( "" ) 
      ; PTL ( "; END " & InterfaceName & " . " ) 
      ; PTL ( "" ) 
      END (* IF *) 
    END WriteLdlTokInterfaceToStream  

(* VISIBLE: *) 
; PROCEDURE WriteLdlTokInterfaceForName 
    ( LangInfo : LangInfoRefTyp ; LangName : TEXT ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a token-declaring interface to a created file. *) 

  = VAR Writer : Wr . T 
  ; VAR LInterfaceName : TEXT 
  ; VAR LFileName : TEXT 

  ; BEGIN 
      LInterfaceName := LangName & "Tok" 
    ; LFileName := LInterfaceName & ".i3" 
    ; TRY 
        Writer := VersionedFiles . OpenWrite ( LFileName ) 
      EXCEPT 
        VersionedFiles . Error ( EMessage ) 
        => Wr . PutText 
             ( Stdio . stderr 
             , EMessage & "while trying to open \"" 
               & LFileName & "\"" & Wr . EOL 
             ) 
      ; RETURN 
      END 
    ; WriteLdlTokInterfaceToStream ( Writer , LangInfo , LInterfaceName ) 
    ; Wr . Close ( Writer ) 
    ; Wr . PutText 
        ( Stdio . stderr , "Wrote  \"" & LFileName & "\"" & Wr . EOL ) 
    END WriteLdlTokInterfaceForName 

(* VISIBLE: *) 
; PROCEDURE WriteLdlChildInterfaceToStream 
    ( Writer : Wr . T ; LangInfo : LangInfoRefTyp ; InterfaceName : TEXT ) 
  RAISES { AssertionFailure , Thread . Alerted , Wr . Failure } 
  (* Write a child-name  declaring interface to an already-open Wr.T *) 

  = PROCEDURE PT ( Txt : TEXT ) 
    RAISES { Thread . Alerted , Wr . Failure } 

    = BEGIN (* PT *) 
        Wr . PutText ( Writer , Txt ) 
      END PT 

  ; PROCEDURE PTL ( Txt : TEXT ) 
    RAISES { Thread . Alerted , Wr . Failure } 

    = BEGIN (* PTL *) 
        PT ( Txt ) 
      ; Wr . PutText ( Writer , Wr . EOL ) 
      END PTL 

  ; PROCEDURE VisitRule ( RuleNode : AstView . AstRefTyp ) 
    RAISES { AssertionFailure , Thread . Alerted , Wr . Failure } 

    = VAR RuleChildren : ARRAY [ 0 .. 2 ] OF AstView . AstRefTyp 

    ; PROCEDURE VisitRuleParent ( ParentNode : AstView . AstRefTyp ) 
      RAISES { AssertionFailure , Thread . Alerted , Wr . Failure } 

      = VAR ChildNo : LbeStd . EstChildNoTyp 
      ; VAR ParentString : SharedStrings . T 

      ; PROCEDURE VisitFixedChild ( ChildNode : AstView . AstRefTyp ) 
        RAISES { AssertionFailure , Thread . Alerted , Wr . Failure } 

        = BEGIN (* VisitFixedChild *) 
            WITH 
              WChildNameNode 
              = AstView . Child 
                  ( ChildNode , Ldl0Child . AsReqdChild_ChildName 
                  , LangInfo . LdlLang ) 
            DO TYPECASE WChildNameNode . NodeRef 
               OF NULL 
               => 
               | SharedStrings . T ( TChildString ) 
               => PT ( "; CONST " ) 
               ; PT ( SharedStrings . ToText ( ParentString ) ) 
               ; PT ( "_" ) 
               ; PT ( SharedStrings . ToText ( TChildString ) ) 
               ; PT ( " = " ) 
               ; PTL ( Fmt . Int ( ChildNo ) ) 
               ELSE 
               END (* TYPECASE *) 
            END (* WITH *) 
          ; INC ( ChildNo ) 
          END VisitFixedChild 

      ; BEGIN (* VisitRuleParent *) 
          ChildNo := 0 
        ; TYPECASE ParentNode . NodeRef <* NOWARN *>
          OF SharedStrings . T ( TParentString ) 
          => ParentString := TParentString 
          ; <* FATAL ANY *> (* Most can't happen, from VisitFixedChild  *) BEGIN
            AstView . TraverseChildren 
              ( RuleChildren [ Ldl0Child . AsFixedRule_Children ] 
              , VisitFixedChild 
              ) 
            END (* Block *) 
          END (* TYPECASE *) 
        END VisitRuleParent 

    ; BEGIN (* VisitRule *) 
        AstView . GetChildren 
          ( RuleNode , RuleChildren , LangInfo . LdlLang ) 
      ; CASE EstUtil . EstTok ( RuleNode . NodeRef ) 
        OF Ldl0Tok . AsFixedRule 
        => <* FATAL ANY *> (* Most can't happen, from VisitFixedChild  *) BEGIN
           AstView . TraverseChildren 
             ( RuleChildren [ Ldl0Child . AsFixedRule_Parents ] 
             , VisitRuleParent 
             ) 
           END (* Block *) 
        ELSE 
        END (* CASE *) 
      END VisitRule 

  ; BEGIN (* WriteLdlChildInterfaceToStream *) 
      IF Writer # NIL AND LangInfo # NIL 
      THEN 
        PTL ( "" ) 
      ; PTL 
          ( "(* INTERFACE " & InterfaceName 
            & ", Ldl Children names, mechanically generated by LdlSemantics. "
          ) 
      ; PTL ( "     Generated on:  " 
              & Misc . DateImage 
                  ( Date . FromTime ( Time . Now ( ) , z := Date . UTC ) ) 
            ) 
      ; IF LangInfo . DefLangIdRef # NIL 
        THEN
          PTL ( "     " 
                & LangUtil . LangIdImage 
                    ( LangInfo . DefLangIdRef ^ , Indent := 5 ) 
              )  
        END 
      ; PTL ( "*) " ) 
      ; PTL ( "" ) 
      ; PTL ( "INTERFACE " & InterfaceName & " " ) 
      ; VAR LRootChildren 
          : ARRAY [ 0 .. Ldl0Child . LanguageDefinition_Rules ] 
            OF AstView . AstRefTyp 

      ; BEGIN (* Block *) 
          AstView . GetChildren 
            ( LangInfo . Root , LRootChildren , LangInfo . LdlLang ) 
        ; <* FATAL ANY *> (* Most can't happen, from VisitFixedChild  *) BEGIN
          AstView . TraverseChildren 
            ( LRootChildren [ Ldl0Child . LanguageDefinition_Rules ] 
            , VisitRule 
            ) 
          END (* Block *) 
        END (* Block *) 

      ; PTL ( "" ) 
      ; PTL ( "; END " & InterfaceName & " . " ) 
      ; PTL ( "" ) 
      END (* IF *) 
    END WriteLdlChildInterfaceToStream  

(* VISIBLE: *) 
; PROCEDURE WriteLdlChildInterfaceForName 
    ( LangInfo : LangInfoRefTyp ; LangName : TEXT ) 
  RAISES { Thread . Alerted , Wr . Failure , AssertionFailure } 
  (* Write a child-name-declaring interface to a created file. *) 

  = VAR Writer : Wr . T 
  ; VAR LInterfaceName : TEXT 
  ; VAR LFileName : TEXT 

  ; BEGIN 
      LInterfaceName := LangName & "Child" 
    ; LFileName := LInterfaceName & ".i3" 
    ; TRY 
        Writer := VersionedFiles . OpenWrite ( LFileName ) 
      EXCEPT 
        VersionedFiles . Error ( EMessage ) 
        => Wr . PutText 
             ( Stdio . stderr 
             , EMessage & "while trying to open \"" 
               & LFileName & "\"" & Wr . EOL 
             ) 
      ; RETURN 
      END 
    ; WriteLdlChildInterfaceToStream ( Writer , LangInfo , LInterfaceName ) 
    ; Wr . Close ( Writer ) 
    ; Wr . PutText 
        ( Stdio . stderr , "Wrote  \"" & LFileName & "\"" & Wr . EOL ) 
    END WriteLdlChildInterfaceForName 

; CONST IdentStartChars = SET OF CHAR { '_' , 'a' .. 'z' , 'A' .. 'Z' } 
; CONST IdentTailChars = IdentStartChars + SET OF CHAR { '0' .. '9' } 
(* TODO: ^Something about the fact that these could be slightly wrong for
          the reserved word set in some language. 
*) 

; PROCEDURE LooksLikeAnIdent ( String : TEXT ) : BOOLEAN 

  = VAR LToPos : INTEGER 
  ; VAR LSs : INTEGER 

  ; BEGIN 
      IF String = NIL 
      THEN RETURN FALSE 
      ELSE
        LToPos := Text . Length ( String ) 
      ; IF LToPos = 0 
        THEN (* Empty string. *) 
          RETURN FALSE 
        ELSE (* 1 <= LToPos *)  
          IF Text . GetChar ( String , 0 ) = '\"' 
          THEN (* Starts with quote. *)  
            IF Text . GetChar ( String , LToPos - 1 ) = '\"' 
            THEN (* Starts and ends with quote. *) 
              LSs := 1 
            ; DEC ( LToPos ) 
            ELSE RETURN FALSE (* Inconsistent about quotes. *) 
            END (* IF *) 
          ELSE (* No leading quote. *) 
            IF Text . GetChar ( String , LToPos - 1 ) = '\"' 
            THEN RETURN FALSE (* Inconsistent about quotes. *) 
            ELSE LSs := 0 
            END (* IF *) 
          END (* IF *) 
        (* Here, String [ LSs .. LToPos - 1 ] has any quotes removed. *) 
        ; IF LSs < LToPos 
          THEN (* At least one char between any quotes. *) 
            IF Text . GetChar ( String , LSs ) IN IdentStartChars 
            THEN 
              INC ( LSs ) 
            ; LOOP 
                IF LSs >= LToPos 
                THEN RETURN TRUE 
                ELSIF Text . GetChar ( String , LSs ) IN IdentTailChars 
                THEN INC ( LSs ) 
                ELSE RETURN FALSE 
                END (* IF*) 
              END (* LOOP *) 
            ELSE RETURN FALSE 
            END (* IF *) 
          ELSE RETURN FALSE 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END LooksLikeAnIdent 

(* VISIBLE: *) 
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

  = PROCEDURE PT ( Txt : TEXT ) 
    RAISES { Thread . Alerted , Wr . Failure } 

    = BEGIN (* PT *) 
        Wr . PutText ( Writer , Txt ) 
      END PT 

  ; PROCEDURE PTL ( Txt : TEXT ) 
    RAISES { Thread . Alerted , Wr . Failure } 

    = BEGIN (* PTL *) 
        PT ( Txt ) 
      ; Wr . PutText ( Writer , Wr . EOL ) 
      END PTL 

  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LTokIfName : TEXT 
  ; VAR LEmitIt : BOOLEAN 
  ; VAR LText : TEXT 

  ; BEGIN (* WriteInitTokStringsModuleToStream *) 
      LTokIfName := LangName & "Tok" 
    ; PTL ( "" )     
    ; PTL ( "(* MODULE " & ModuleName 
            & ", Initialization calls for placeholder names, " ) 
    ; PTL ( "     mechanically generated by LdlSemantics. " ) 
    ; PTL ( "     Generated on:  " 
            & Misc . DateImage 
                ( Date . FromTime ( Time . Now ( ) , z := Date . UTC ) ) 
          ) 
    ; IF LangInfo . DefLangIdRef # NIL 
      THEN
        PTL ( "     " 
              & LangUtil . LangIdImage 
                  ( LangInfo . DefLangIdRef ^ , Indent := 5 ) 
            )  
      END 
    ; PTL ( "*) " ) 
    ; PTL ( "" ) 
    ; PTL ( "MODULE " & ModuleName & " " ) 
    ; PTL ( "" ) 
 (* ; PTL ( "; FROM LbeStd  IMPORT TokClassTyp " ) Seems always unused. *) 
    ; PTL ( "; IMPORT TextIntTbl " ) 
    ; PTL ( "; IMPORT " & LTokIfName  ) 
    ; PTL ( "" ) 
    ; WITH WTokMap = LangInfo . TokMapRef ^ 
      DO 
        PTL ( "(* VISIBLE: *) " ) 
      ; PTL ( "; PROCEDURE InitRw ( Table : TextIntTbl . Default ) " ) 
      ; PTL ( "" ) 
      ; PTL ( "  = BEGIN " ) 
      ; PTL ( "      EVAL 0 (* Just to make generation easier. *) " ) 
      ; FOR FTok := LbeStd . Tok__FirstLangDep TO LangInfo . StringToks - 1 
        DO 
          LNodeNo := WTokMap [ FTok - TokMapBias ] 
        ; TYPECASE LangInfo . SemMapRef ^ [ LNodeNo ] . SemRef 
          OF SemFirstOccStringTyp ( TStringDecl ) 
          => TYPECASE 
               LangInfo . SemMapRef ^ [ TStringDecl . NodeNo ] . EstRef 
             OF SharedStrings . T ( TString ) 
             => LText := SharedStrings . ToText ( TString ) 
             ; IF TStringDecl . AltName # LbeStd . EstNodeNoNull 
                  AND LooksLikeAnIdent ( LText ) 
               THEN 
                 TYPECASE 
                   LangInfo . SemMapRef ^ [ TStringDecl . AltName ] . SemRef 
                 OF SemDeclTyp ( TNameDecl ) 
                 => TYPECASE 
                      LangInfo . SemMapRef ^ [ TNameDecl . NodeNo ] . EstRef 
                    OF SharedStrings . T ( TName ) 
                    => PT ( "    ; EVAL Table . put ( " ) 
                    ; PT  ( LText ) (* Already has quotes around it. *)  
                    ; PT  ( " , " ) 
                    ; PT  ( LTokIfName ) 
                    ; PT  ( " . " ) 
                    ; PT  ( SharedStrings . ToText ( TName ) ) 
                    ; PTL  ( "_Tok ) " )
                    ELSE 
                    END (* TYPECASE *) 
                 ELSE 
                 END (* TYPECASE *) 
               END (* IF *) 
             ELSE 
             END (* TYPECASE *) 
          ELSE 
          END (* TYPECASE *) 
        END (* FOR *) 
      ; PTL ( "    END InitRw " )   
      ; PTL ( "" )   
      ; PTL ( "(* VISIBLE: *) " ) 
      ; PTL ( "; PROCEDURE InitPh ( Table : TextIntTbl . Default ) " ) 
      ; PTL ( "" ) 
      ; PTL ( "  = BEGIN " ) 
      ; PTL ( "      EVAL 0 (* Just to make generation easier. *) " ) 
      ; FOR FTok := LangInfo . StringToks TO LangInfo . AsCsClassToks - 1 
        DO IF LangInfo . VarTermToks <= FTok  
              AND FTok < LangInfo . VarTermModToks 
          THEN (* This is a VarTermModTok token--ignore it. *) 
          ELSIF LangInfo . AsSublistToks <= FTok 
                AND FTok < LangInfo . AsPartialToks 
          THEN (* This is a list cardinality or partial list token--ignore it. *) 
          ELSE 
            LNodeNo := WTokMap [ FTok - TokMapBias ] 
          ; TYPECASE LangInfo . SemMapRef ^ [ LNodeNo ] . SemRef 
            OF NULL => 
            | SemDeclTyp ( TSemDecl ) 
            => TYPECASE 
                 LangInfo . SemMapRef ^ [ TSemDecl . NodeNo ] . EstRef 
               OF SharedStrings . T ( TName ) 
               => TYPECASE TSemDecl 
                  OF SemFirstOccClassTyp ( TClassDecl ) 
                  => IF LbeStd . Tok__FirstLangDep <= TClassDecl . SingletonTok 
                          AND TClassDecl . SingletonTok < LangInfo . StringToks 
                     THEN (* A synonym for a string token. *) 
                       LEmitIt := FALSE 
                     ELSE 
                       LEmitIt := TRUE 
                     END (* IF *) 
                  ELSE LEmitIt := TRUE  
                  END (* TYPECASE *) 
               ; IF LEmitIt 
                 THEN 
                   PT ( "    ; EVAL Table . put ( \"" )
                 ; PT ( LbeStd . LeftPlaceholderDelimText )  
                 ; PT ( SharedStrings . ToText ( TName ) ) 
                 ; PT ( LbeStd . RightPlaceholderDelimText )  
                 ; PT ( "\" , " ) 
                 ; PT ( LTokIfName ) 
                 ; PT ( " . " ) 
                 ; PT ( SharedStrings . ToText ( TName ) ) 
                 ; PTL ( " ) " ) 
                 END (* IF *) 
               ELSE 
               END (* TYPECASE *) 
            ELSE 
            END (* TYPECASE *) 
          END (* IF *) 
        END (* FOR *) 
      ; PTL ( "    END InitPh " )   
      ; PTL ( "" )   
      END (* WITH *) 
    ; PTL ( "; BEGIN (* " & ModuleName & " *) " ) 
    ; PTL ( "  END " & ModuleName & " " ) 
    ; PTL ( ". " ) 
    ; PTL ( "" ) 
    END WriteInitTokStringsModuleToStream  

(* VISIBLE: *) 
; PROCEDURE WriteInitTokStringsModuleForName 
    ( LangInfo : LangInfoRefTyp ; LangName : TEXT ) 
  RAISES { Wr . Failure , Thread . Alerted } 
  (* Write a module that initializes reserved word and placeholder strings
     to a created file. 
  *) 

  = VAR Writer : Wr . T 
  ; VAR LModuleName : TEXT 
  ; VAR LFileName : TEXT 

  ; BEGIN 
      LModuleName := LangName & "InitTokStrings" 
    ; LFileName := LModuleName & ".m3" 
    ; TRY 
        Writer := VersionedFiles . OpenWrite ( LFileName ) 
      EXCEPT 
        VersionedFiles . Error ( EMessage ) 
        => Wr . PutText 
             ( Stdio . stderr 
             , EMessage & "while trying to open \"" 
               & LFileName & "\"" & Wr . EOL 
             ) 
      ; RETURN 
      END 
    ; WriteInitTokStringsModuleToStream 
        ( Writer , LangInfo , LangName , LModuleName ) 
    ; Wr . Close ( Writer ) 
    ; Wr . PutText 
        ( Stdio . stderr , "Wrote  \"" & LFileName & "\"" & Wr . EOL ) 
    END WriteInitTokStringsModuleForName 

(* TODO: ^WriteInitTokStrings... only generate the module.  Could
          also generate the interface too, because one per language
          scanner will be needed.  For now, this is more trouble than
          it is worth, because the interfaces are trivial, and differ
          only in the interface name.  *)

; TYPE RegisteredAnalyzersTyp 
       = ARRAY LbeStd . LangBuiltinTyp OF AnalyzeProcTyp 

; VAR RegisteredAnalyzers := RegisteredAnalyzersTyp { NIL , .. } 

(* VISIBLE: *) 
; PROCEDURE RegisterAnalyzer 
    ( Lang : LbeStd . LangBuiltinTyp ; Analyzer : AnalyzeProcTyp ) 
  (* Register Analyzer as the analyzer procedure for language Lang. *) 

  = BEGIN 
      RegisteredAnalyzers [ Lang ] := Analyzer 
    END RegisterAnalyzer 
(* TODO: This mechanism, the one in LangUtil for registering scanners,
         the stuff in Lbe for loading language pickles, etc. all need
         to be handled in a uniform place and way.  It also needs to
         work even for non-builtin languages. Maybe it belongs in
         LangMap. 
*) 

(* VISIBLE: *) 
; PROCEDURE Analyze 
    ( Est : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  : LangInfoRefTyp 
  RAISES { AssertionFailure , Thread . Alerted } 
  (* If Lang is a builtin language, call its registered analyzer,
     passing Est and Lang to it.
  *) 

  = BEGIN (* Analyze *) 
      IF FIRST ( LbeStd . LangBuiltinTyp ) <= Lang 
         AND Lang <= LAST ( LbeStd . LangBuiltinTyp ) 
      THEN 
        WITH WAnalyzer = RegisteredAnalyzers [ Lang ] 
        DO IF WAnalyzer # NIL 
           THEN 
             RETURN WAnalyzer ( Est , Lang ) 
           ELSE 
             RETURN NIL 
           END (* IF *) 
        END (* WITH *) 
      ELSE 
        RETURN NIL 
      END (* IF *) 
    END Analyze 

(* VISIBLE: *) 
; PROCEDURE ManLRGen 
    ( LangInfo : LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Do LR generation for handwritten grammar. *) 

  = BEGIN 
      IF LangInfo . Gram # NIL 
      THEN 
        FinishLRGeneration 
          ( LangInfo , LangInfo . Gram , LangInfo . Productions ) 
      END (* IF *) 
    END ManLRGen  
 
(* VISIBLE: *) 
; PROCEDURE CountOfNonclassTokSetProductions 
     ( LangInfo : LangInfoRefTyp ; ClassMembers : IntSets . T  ) 
  : CARDINAL 
  (* Number of members of ClassMembers that are not class tokens themselves. *) 

  = VAR LSet1 , LSet2 : IntSets . T  
  ; VAR LCard1 , LCard2 : CARDINAL 

  ; BEGIN 
    (* Eliminate class tokens from ClassMembers. *)  
      LSet1 
        := IntSets . Project ( ClassMembers , 0 , LangInfo . AsPartialToks - 1 )
    ; LCard1 := IntSets . Card ( LSet1 ) 
    ; LSet2 := IntSets . Project 
        ( ClassMembers , LangInfo . AsCsClassToks 
        , LangInfo . GcsChildToks - 1 
        )
    ; LCard2 := IntSets . Card ( LSet2 ) 
    ; RETURN LCard1 + LCard2 
    END CountOfNonclassTokSetProductions 

(* VISIBLE: *) 
; PROCEDURE GenNonclassTokSetProductions  
    ( LangInfo : LangInfoRefTyp 
    ; Gram : LRTable . GrammarTyp 
    ; ClassTok : LbeStd . TokTyp 
    ; ClassMembers : IntSets . T  
    ; ReplaceListCardToks : BOOLEAN := FALSE 
    ) 
  RAISES { AssertionFailure } 
  (* For all members M of the class identified by ClassTok that are not 
     classes themselves, generate a singleton production ClassTok ::= M.
  *) 

  = PROCEDURE Visit ( Tok : IntSets . ValidElemT )

    = BEGIN 
        IF Tok < LangInfo . VarTermModToks 
        THEN (* StringToks, VarTermToks, VarTermModToks *) 
          Automaton . AddExternalProduction 
            ( Gram 
            , ClassTok 
            , Right := LRTable . SingletonString ( Tok ) 
            , BuildTok := LbeStd . Tok__Null  
            ) 
        ELSIF Tok < LangInfo . AsPlusToks AND ReplaceListCardToks 
        THEN (* AsPlusToks *)
          Automaton . AddExternalProduction 
            ( Gram 
            , ClassTok 
            , Right 
                := LRTable . SingletonString 
                     ( ListCardTok 
                         ( LangInfo , Tok , ListCardTyp . ListCardSingleton ) 
                     ) 
            , BuildTok := LbeStd . Tok__Null  
            ) 
        ; Automaton . AddExternalProduction 
            ( Gram 
            , ClassTok 
            , Right 
                := LRTable . SingletonString 
                     ( ListCardTok 
                         ( LangInfo , Tok , ListCardTyp . ListCardPlural ) 
                     ) 
            , BuildTok := LbeStd . Tok__Null  
            ) 
        ELSIF Tok < LangInfo . AsStarToks AND ReplaceListCardToks 
        THEN (* AsStarToks *)
          Automaton . AddExternalProduction 
            ( Gram 
            , ClassTok 
            , Right 
                := LRTable . SingletonString 
                     ( ListCardTok 
                         ( LangInfo , Tok , ListCardTyp . ListCardEmpty ) 
                     ) 
            , BuildTok := LbeStd . Tok__Null  
            ) 
        ; Automaton . AddExternalProduction 
            ( Gram 
            , ClassTok 
            , Right 
                := LRTable . SingletonString 
                     ( ListCardTok 
                         ( LangInfo , Tok , ListCardTyp . ListCardSingleton ) 
                     ) 
            , BuildTok := LbeStd . Tok__Null  
            ) 
        ; Automaton . AddExternalProduction 
            ( Gram 
            , ClassTok 
            , Right 
                := LRTable . SingletonString 
                     ( ListCardTok 
                         ( LangInfo , Tok , ListCardTyp . ListCardPlural ) 
                     ) 
            , BuildTok := LbeStd . Tok__Null  
            ) 
(* TODO: Trailing tokens? *) 
        ELSIF Tok < LangInfo . AsPartialToks  
        THEN (* AsFixedToks, AsSublistToks, AsListCardToks, AsPartialToks.
                Also, AsPlusToks & AsStarToks if NOT ReplaceListCardToks. *)
          Automaton . AddExternalProduction 
            ( Gram 
            , ClassTok 
            , Right := LRTable . SingletonString ( Tok ) 
            , BuildTok := LbeStd . Tok__Null  
            ) 
        ELSIF Tok < LangInfo . AsCsClassToks  
        THEN (* AsClassOnlyToks, AsCsClassToks -- omit these. *) 
        ELSIF Tok < LangInfo . CsFixedToks 
        THEN (* CsAltToks, CsPlusToks, CsPluralToks, CsStarToks, CsFixedToks,
                GcsChildToks *)
          Automaton . AddExternalProduction 
            ( Gram 
            , ClassTok 
            , Right := LRTable . SingletonString ( Tok ) 
            , BuildTok := LbeStd . Tok__Null  
            ) 
        ELSE (* GcsChildToks *)
          Automaton . AddProduction 
            ( Gram 
            , ClassTok 
            , Right := LRTable . SingletonString ( Tok ) 
            , Precedence := LRTable . PrecedenceMax 
            , BuildTok := LbeStd . Tok__Null  
            ) 
         END (* IF *)  
      END Visit  

  ; <* FATAL ANY *> 
    BEGIN (* GenNonclassTokSetProductions *)  
      IntSets . ForAllDo ( ClassMembers , Visit ) 
    END GenNonclassTokSetProductions 

(* VISIBLE: *) 
; PROCEDURE ForAllAsMembersDo   
    ( LangInfo : LangInfoRefTyp 
    ; TokSet : IntSets . T  
    ; Visit : IntSets . ProcOfValidElem  
    ) 
  RAISES ANY  
  (* Call back Visit for every member of TokSet that is an AS node. *) 
(* TODO: Check if we really need this.  It is now (2012-7-30) unused.
         Its caller could about as well incorporate its filtering
         directly. *) 

  = PROCEDURE FaVisit ( Tok : IntSets . ValidElemT ) 
    RAISES ANY  

    = BEGIN 
        IF LangUtilLo . TokIsAbstract ( LangInfo , Tok ) 
        THEN 
          Visit ( Tok ) 
        END (* IF *)  
      END FaVisit  

  ; <* FATAL ANY *> 
    BEGIN (* ForAllAsMembersDo *)  
      IntSets . ForAllDo ( TokSet , FaVisit ) 
    END ForAllAsMembersDo  

; PROCEDURE PathToDescendant 
    ( FsRootNodeRef : LangUtil . FsNodeRefTyp 
    ; FsDescendantNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  : TEXT
  (* A string for use in an FsTree dump, describing the path from FsRootNodeRef 
     to FsDescendantNodeRef, which should be among the descendants of 
     FsRootNodeRef. 
  *)  

  = PROCEDURE Recurse 
      ( FsRootNodeRef : LangUtil . FsNodeRefTyp 
      ; FsDescendantNodeRef : LangUtil . FsNodeRefTyp 
      ) 
    : TEXT 
    (* NIL result means FsDescendantNodeRef is not a descendant of FsRootNodeRef.
       Otherwise, result is the sequence of child numbers leading from
       FsRootNodeRef to FsDescendantNodeRef 
    *) 

    = VAR LChildCt : LangUtil . FsChildNoTyp 
    ; VAR LChildNo : LangUtil . FsChildNoTyp 
    ; VAR LChildPath : TEXT 

    ; BEGIN 
        IF FsRootNodeRef = FsDescendantNodeRef 
        THEN RETURN "" 
        ELSIF FsRootNodeRef . FsChildren = NIL  
        THEN RETURN NIL 
        ELSE 
          LChildCt := NUMBER ( FsRootNodeRef . FsChildren ^ ) 
        ; LChildNo := 0 
        ; LOOP 
            IF LChildNo >= LChildCt 
            THEN RETURN NIL 
            ELSE 
              LChildPath 
                 := Recurse  
                      ( FsRootNodeRef . FsChildren ^ [ LChildNo ] 
                      , FsDescendantNodeRef 
                      ) 
            ; IF LChildPath = NIL 
              THEN INC ( LChildNo ) 
              ELSE 
                RETURN 
                  "C" 
                  & LangUtil . FsChildNoImage 
                      ( LChildNo , MakeShort := TRUE ) 
                  & "," 
                  & LChildPath 
              END (* IF *)
            END (* IF *) 
          END (* LOOP *) 
        END (* IF *) 
      END Recurse  

  ; BEGIN 
      IF FsDescendantNodeRef = NIL 
      THEN RETURN "NIL"
      ELSE 
        WITH WResult = Recurse ( FsRootNodeRef , FsDescendantNodeRef )  
        DO
          IF WResult = NIL 
          THEN RETURN "NonExistent"
          ELSE RETURN WResult
          END (* IF *) 
        END (* WITH *) 
      END (* IF *) 
    END PathToDescendant 

; VAR GDefaultValueFsNode : LangUtil . FsNodeRefTyp 
        := NEW ( LangUtil . FsNodeRefTyp ) 

; PROCEDURE DumpFsTree  
    ( WrT : Wr . T
    ; Lang : LbeStd . LangTyp  
    ; Prefix : TEXT := "" (* Prepend to every line. *) 
    ; FsRootNodeRef : LangUtil . FsNodeRefTyp 
    ; IncludeDefaultValuedFields : BOOLEAN := FALSE 
    ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a readable dump of one Fs tree. *) 

  = PROCEDURE DfsTokImage ( Tok : IntSets . ValidElemT ) : TEXT 

    = BEGIN 
        RETURN LangUtil . TokImage ( Tok , Lang ) 
      END DfsTokImage 
 
  ; PROCEDURE DumpNode 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; Prefix : TEXT (* Prepend to every line. *) 
      ) 
    RAISES { Thread . Alerted , Wr . Failure } 

    = PROCEDURE InnerDumpNode 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; Prefix : TEXT (* Prepend to every line. *) 
        ) 
      RAISES { Thread . Alerted , Wr . Failure } 
      (* Assumes FsNodeRef # NIL. Doesn't do anything with FsCondAltRef. *)  

      = PROCEDURE WL ( Value : TEXT ) 
        RAISES { Thread . Alerted , Wr . Failure } 

        = BEGIN 
            Wr . PutText ( WrT , Prefix & Value & Wr . EOL ) 
          END WL 

      ; PROCEDURE WF ( Label : TEXT ; Value : TEXT ) 
        RAISES { Thread . Alerted , Wr . Failure } 

        = BEGIN 
            Wr . PutText ( WrT , Prefix & Label & " = " & Value & Wr . EOL ) 
          END WF 

      ; BEGIN (* InnerDumpNode *) 
          IF FsNodeRef = NIL 
          THEN 
            WL ( "NIL" ) 
          ELSE 
            (* Fields printed together: *) 
            IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsKind # GDefaultValueFsNode . FsKind 
            THEN 
              WF ( "FsKind" , LangUtil . FsKindImage ( FsNodeRef . FsKind ) ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsPostNodeNo 
                  # GDefaultValueFsNode . FsPostNodeNo 
            THEN 
              WF ( "FsPostNodeNo" 
                 , LangUtil . FsNodeNoImage ( FsNodeRef . FsPostNodeNo ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsLdlNodeNo 
                  # GDefaultValueFsNode . FsLdlNodeNo 
            THEN 
              WF ( "FsLdlNodeNo" 
                 , LbeStd . EstNodeNoImage ( FsNodeRef . FsLdlNodeNo ) 
                 ) 
            END (* IF *) 
          
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsSingletonOptMapRef 
                  # GDefaultValueFsNode . FsSingletonOptMapRef  
            THEN 
              IF FsNodeRef . FsSingletonOptMapRef = NIL 
              THEN WF ( "FsSingletonOptMapRef" , "NIL" ) 
              ELSIF NUMBER ( FsNodeRef . FsSingletonOptMapRef ^ ) = 0 
              THEN WF ( "FsSingletonOptMapRef" , "{ }" ) 
              ELSE 
                VAR LFirst := FIRST ( FsNodeRef . FsSingletonOptMapRef ^ ) 
              ; VAR LLast := LAST ( FsNodeRef . FsSingletonOptMapRef ^ ) 
              ; VAR LI : INTEGER 
              ; VAR LLabel : TEXT := Prefix & "FsSingletonOptMapRef -> { "
              ; VAR LLabel2 : TEXT  
                      := Misc . Blanks ( Text . Length ( Prefix ) ) 
                         & "                        , "
              ; VAR LFsEstChildRef := LangUtil . FsEstChildRef ( FsNodeRef ) 
              ; BEGIN (* Block *) 
                  IF LFsEstChildRef # NIL 
                     AND LFsEstChildRef # FsNodeRef 
                     AND FsNodeRef . FsSingletonOptMapRef 
                         = LFsEstChildRef . FsSingletonOptMapRef 
                  THEN (* The map is reference-equal to the map of the Est 
                          child.  Don't redundantly print it here. *) 
                    WF ( "FsSingletonOptMapRef" 
                       , "{ <Reference-equal to the Est Child. > }" 
                       ) 
                  ELSE (* Print contents of FsSingletonOptMapRef ^ *) 
                    LI := LFirst 
                  ; LOOP 
                      IF LI = LFirst OR LI = LLast 
                         OR FsNodeRef . FsSingletonOptMapRef ^ [ LI ] 
                            # LbeStd . Tok__Null
                      THEN 
                        Wr . PutText ( WrT , LLabel ) 
                      ; Wr . PutText 
                          ( WrT 
                          , LangUtil . TokImage 
                              ( LI + FsNodeRef . FsSingletonOptMin ) 
                          ) 
                      ; Wr . PutText ( WrT , " -> " ) 
                      ; Wr . PutText 
                          ( WrT 
                          , LangUtil . TokImage 
                              ( FsNodeRef . FsSingletonOptMapRef ^ [ LI ] ) 
                          ) 
                      ; IF LI = LLast 
                        THEN 
                          Wr . PutText ( WrT , " }" ) 
                        ; Wr . PutText ( WrT , Wr . EOL ) 
                        ; EXIT 
                        ELSE 
                          Wr . PutText ( WrT , Wr . EOL ) 
                        ; LLabel := LLabel2  
                        END (* IF *) 
                      END (* IF *) 
                    ; INC ( LI ) 
                    END (* LOOP *) 
                  END (* IF *) 
                END (* Block *) 
              END (* IF *) 
            END (* IF *) 

          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsFmtNo # GDefaultValueFsNode . FsFmtNo 
            THEN 
              WF ( "FsFmtNo" , EstHs . FmtNoImage ( FsNodeRef . FsFmtNo ) ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsLeftFmtNo # GDefaultValueFsNode . FsLeftFmtNo 
            THEN 
              WF ( "FsLeftFmtNo" 
                 , EstHs . FmtNoImage ( FsNodeRef . FsLeftFmtNo ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsRightFmtNo 
                  # GDefaultValueFsNode . FsRightFmtNo 
            THEN 
              WF ( "FsRightFmtNo" 
                 , EstHs . FmtNoImage ( FsNodeRef . FsRightFmtNo ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsIsInsideList 
                  # GDefaultValueFsNode . FsIsInsideList 
            THEN 
              WF 
               ( "FsIsInsideList" , Fmt . Bool ( FsNodeRef . FsIsInsideList ) )
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsIsInsideCondFmt #
                   GDefaultValueFsNode . FsIsInsideCondFmt 
            THEN 
              WF ( "FsIsInsideCondFmt" 
                 , Fmt . Bool ( FsNodeRef . FsIsInsideCondFmt ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsIsInsideFill 
                  # GDefaultValueFsNode . FsIsInsideFill 
            THEN 
              WF ( "FsIsInsideFill" 
                 , Fmt . Bool ( FsNodeRef . FsIsInsideFill ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsIsAutonomous 
                  # GDefaultValueFsNode . FsIsAutonomous 
            THEN 
              WF ( "FsIsAutonomous" 
                 , Fmt . Bool ( FsNodeRef . FsIsAutonomous ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsContainsLineBreak 
                  # GDefaultValueFsNode . FsContainsLineBreak 
            THEN 
              WF ( "FsContainsLineBreak" 
                 , Fmt . Bool ( FsNodeRef . FsContainsLineBreak ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsIndentCode 
                  # GDefaultValueFsNode . FsIndentCode 
            THEN 
              WF ( "FsIndentCode" 
                 , LangUtil . IndentCodeImage ( FsNodeRef . FsIndentCode ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsTok # GDefaultValueFsNode . FsTok 
            THEN 
              WF ( "FsTok" 
                 , LangUtil . TokImage ( FsNodeRef . FsTok , Lang ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsListSliceThruFmtNo 
                  # GDefaultValueFsNode . FsListSliceThruFmtNo 
            THEN 
              WF ( "FsListSliceThruFmtNo" 
                 , EstHs . FmtNoImage ( FsNodeRef . FsListSliceThruFmtNo ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsFormatsEmpty  
                  # GDefaultValueFsNode . FsFormatsEmpty 
            THEN 
              WF ( "FsFormatsEmpty" 
                 , UncertainBool . Image ( FsNodeRef . FsFormatsEmpty )
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsHasLineBreak   
                  # GDefaultValueFsNode . FsHasLineBreak 
            THEN 
              WF ( "FsHasLineBreak " 
                 , LangUtil . FormatsEmptyImage ( FsNodeRef . FsHasLineBreak )
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsSublistTok 
                  # GDefaultValueFsNode . FsSublistTok 
            THEN 
              WF ( "FsSublistTok" 
                 , LangUtil . TokImage ( FsNodeRef . FsSublistTok , Lang ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsPartialTok 
                  # GDefaultValueFsNode . FsPartialTok 
            THEN 
              WF ( "FsPartialTok" 
                 , LangUtil . TokImage ( FsNodeRef . FsPartialTok , Lang ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsTrailingTok 
                  # GDefaultValueFsNode . FsTrailingTok 
            THEN 
              WF ( "FsTrailingTok" 
                 , LangUtil . TokImage ( FsNodeRef . FsTrailingTok , Lang ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsEmptyListTok 
                  # GDefaultValueFsNode . FsEmptyListTok 
            THEN 
              WF ( "FsEmptyListTok" 
                 , LangUtil . TokImage ( FsNodeRef . FsEmptyListTok , Lang ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsSingletonListTok 
                  # GDefaultValueFsNode . FsSingletonListTok 
            THEN 
              WF ( "FsSingletonListTok" 
                 , LangUtil . TokImage 
                     ( FsNodeRef . FsSingletonListTok , Lang ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsPluralListTok 
                  # GDefaultValueFsNode . FsPluralListTok 
            THEN 
              WF ( "FsPluralListTok" 
                 , LangUtil . TokImage ( FsNodeRef . FsPluralListTok , Lang ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsOptionIds 
                  # GDefaultValueFsNode . FsOptionIds  
            THEN 
(* TODO: Abstract this into an Image function somewhere: *) 
              WF ( "FsOptionIds" 
                 ,  "{ " 
                    & LRTable . OptionIdImage 
                        ( FsNodeRef . FsOptionIds [ FALSE ] ) 
                    &  " , " 
                    & LRTable . OptionIdImage 
                        ( FsNodeRef . FsOptionIds [ TRUE ] ) 
                    &  " }" 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsInsTokRef # GDefaultValueFsNode . FsInsTokRef 
            THEN 
              WF ( "FsInsTokRef" 
                 , SharedStrings . Image 
                     ( FsNodeRef . FsInsTokRef , Indent := 0 ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsEstChildNo 
                  # GDefaultValueFsNode . FsEstChildNo 
            THEN 
              WF ( "FsEstChildNo" 
                 , LbeStd . EstChildNoImage ( FsNodeRef . FsEstChildNo ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsEstDescendantRef 
                  # GDefaultValueFsNode . FsEstDescendantRef 
            THEN 
              WF ( "FsEstDescendantRef" 
                 , PathToDescendant 
                     ( FsNodeRef , FsNodeRef . FsEstDescendantRef ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsChildIndentCode 
                  # GDefaultValueFsNode . FsChildIndentCode 
            THEN 
              WF ( "FsChildIndentCode" 
                 , LangUtil . IndentCodeImage 
                     ( FsNodeRef . FsChildIndentCode ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsFirstListElemIndentCode 
                  # GDefaultValueFsNode . FsFirstListElemIndentCode 
            THEN 
              WF ( "FsFirstListElemIndentCode" 
                 , LangUtil . IndentCodeImage 
                     ( FsNodeRef . FsFirstListElemIndentCode ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsLeadingChildCt 
                  # GDefaultValueFsNode . FsLeadingChildCt 
            THEN 
              WF ( "FsLeadingChildCt" 
                 , LangUtil . FsChildNoImage ( FsNodeRef . FsLeadingChildCt ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsSameLineEstChildFmtNo 
                  # GDefaultValueFsNode . FsSameLineEstChildFmtNo 
            THEN 
              WF ( "FsSameLineEstChildFmtNo" 
                 , EstHs . FmtNoImage 
                    ( FsNodeRef . FsSameLineEstChildFmtNo ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsEstChildOpt 
                  # GDefaultValueFsNode . FsEstChildOpt 
            THEN 
              WF ( "FsEstChildOpt" 
                 , LangUtil . ChildOptImage ( FsNodeRef . FsEstChildOpt ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsEstChildIsOptional 
                  # GDefaultValueFsNode . FsEstChildIsOptional   
            THEN 
              WF ( "FsEstChildIsOptional" 
                 , Fmt . Bool ( FsNodeRef . FsEstChildIsOptional ) 
                 ) 
            END (* IF *) 
          ; IF TRUE (* For now, lets always display this one. *) 
               OR IncludeDefaultValuedFields 
               OR FsNodeRef . FsIsInFirstLine 
                  # GDefaultValueFsNode . FsIsInFirstLine 
            THEN 
              WF ( "FsIsInFirstLine" 
                 , Fmt . Bool ( FsNodeRef . FsIsInFirstLine ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsIsRightOfEstListChild 
                  # GDefaultValueFsNode . FsIsRightOfEstListChild 
            THEN 
              WF ( "FsIsRightOfEstListChild" 
                 , Fmt . Bool ( FsNodeRef . FsIsRightOfEstListChild ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsLineBreakToReachFmtNo 
                  # GDefaultValueFsNode . FsLineBreakToReachFmtNo 
            THEN 
              WF ( "FsLineBreakToReachFmtNo" 
                 , EstHs . FmtNoImage 
                     ( FsNodeRef . FsLineBreakToReachFmtNo ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsCondPredicate . PredicateKind 
                  # GDefaultValueFsNode . FsCondPredicate . PredicateKind  
            THEN 
              WF ( "FsCondPredicate.PredicateKind" 
                 , LangUtil . PredicateKindImage 
                     ( FsNodeRef . FsCondPredicate . PredicateKind ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsCondPredicate . PredicateKind 
                  = LangUtil . PredicateKindTyp . PredicateKindInClass 
            THEN 
              WF ( "FsCondPredicate.PredicateClass" 
                 , LangUtil . TokImage 
                     ( FsNodeRef . FsCondPredicate . PredicateClass , Lang ) 
                 )  
            END (* IF *) 

          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsAltListTokSet 
                  # GDefaultValueFsNode . FsAltListTokSet 
            THEN 
              WF ( "FsAltListTokSet" 
                 , IntSets . Image 
                     ( FsNodeRef . FsAltListTokSet 
                     , DfsTokImage 
                     , Misc . Blanks ( Text . Length ( Prefix ) ) 
                       & "               " & "   " 
                     ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsAltStarListTokSet 
                  # GDefaultValueFsNode . FsAltStarListTokSet 
            THEN 
              WF ( "FsAltStarListTokSet" 
                 , IntSets . Image 
                     ( FsNodeRef . FsAltStarListTokSet 
                     , DfsTokImage 
                     , Misc . Blanks ( Text . Length ( Prefix ) ) 
                       & "                   " & "   "
                     ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsAltFixedTokSet 
                  # GDefaultValueFsNode . FsAltFixedTokSet 
            THEN 
              WF ( "FsAltFixedTokSet" 
                 , IntSets . Image 
                     ( FsNodeRef . FsAltFixedTokSet 
                     , DfsTokImage 
                     , Misc . Blanks ( Text . Length ( Prefix ) ) 
                       & "                " & "   "
                     ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsAltCardSet # GDefaultValueFsNode . FsAltCardSet 
            THEN 
              WF ( "FsAltCardSet" 
                 , LangUtil . CardSetImage ( FsNodeRef . FsAltCardSet ) 
                 ) 
            END (* IF *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsAltTok # GDefaultValueFsNode . FsAltTok 
            THEN 
              WF ( "FsAltTok" 
                 , LangUtil . TokImage ( FsNodeRef . FsAltTok , Lang ) 
                 ) 
            END (* IF *) 

          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsAltFragCt # GDefaultValueFsNode . FsAltFragCt 
            THEN 
              WF ( "FsAltFragCt" 
                 , Fmt . Int  ( FsNodeRef . FsAltFragCt ) 
                 ) 
            END (* IF *) 

          ; IF IncludeDefaultValuedFields 
               AND FsNodeRef . FsCondAltRef = NIL 
            THEN 
              WF ( "FsCondAltRef" , "NIL" )  
            END (* IF *) 
          ; IF FsNodeRef . FsFmtNoMapRef # NIL 
            THEN 
              VAR LCt := NUMBER ( FsNodeRef . FsFmtNoMapRef ^ ) 
            ; BEGIN 
                Wr . PutText ( WrT , Prefix & "FsFmtNoMapRef -> FmtNo:" ) 
              ; FOR RF := 0 TO LCt - 1 
                DO 
                  Wr . PutText ( WrT , Fmt . Pad ( Fmt . Int ( RF ) , 4 ) )  
                END (* FOR *) 
              ; Wr . PutText ( WrT , Wr . EOL ) 
              ; Wr . PutText 
                  ( WrT , Misc . Blanks ( Text . Length ( Prefix ) ) )
              ; Wr . PutText ( WrT , "               ChildNo:" ) 
                                     
              ; FOR RF := 0 TO LCt - 1 
                DO 
                  Wr . PutText 
                    ( WrT 
                    , Fmt . Pad 
                        ( LangUtil . FsChildNoImage 
                            ( FsNodeRef . FsFmtNoMapRef ^ [ RF ] 
                            , MakeShort := TRUE 
                            ) 
                        , 4 
                        ) 
                    ) 
                END (* FOR *) 
              ; Wr . PutText ( WrT , Wr . EOL ) 
              END (* Block *) 
            ELSIF IncludeDefaultValuedFields 
            THEN WF ( "FsFmtNoMapRef" , "NIL" ) 
            END (* IF *) 

          (* Write the children: *) 
          ; IF IncludeDefaultValuedFields 
               OR FsNodeRef . FsPlaceholder 
                  # GDefaultValueFsNode . FsPlaceholder 
            THEN 
              WF ( "FsPlaceholder" , "" ) 
            ; Wr . PutText ( WrT , Wr . EOL ) 
            ; DumpNode ( FsNodeRef . FsPlaceholder , Prefix & "PH   " ) 
            END (* IF *) 
          ; IF FsNodeRef . FsChildren # NIL 
            THEN 
              FOR RChild := 0 TO NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
              DO 
                Wr . PutText ( WrT , Wr . EOL ) 
              ; DumpNode 
                  ( FsNodeRef . FsChildren ^ [ RChild ] 
                  , Prefix 
                    & Fmt . Pad 
                        ( "C" 
                          & LangUtil . FsChildNoImage 
                              ( RChild , MakeShort := TRUE ) 
                        , 5 
                        , align := Fmt . Align . Left 
                        )  
                  ) 
              END (* FOR *) 
            ELSIF IncludeDefaultValuedFields 
            THEN WF ( "FsChildren" , "NIL" ) 
            END (* IF *) 
          END (* IF *) 
        END InnerDumpNode 

    ; BEGIN (* DumpNode *)  
        IF FsNodeRef # NIL AND FsNodeRef . FsCondAltRef # NIL 
        THEN 
          VAR LAltNo := 0 
        ; VAR LAltRef := FsNodeRef 
        ; BEGIN 
            LOOP 
              InnerDumpNode  
                ( LAltRef 
                , Prefix 
                  & Fmt . Pad 
                      ( "A" & Fmt . Int ( LAltNo ) 
                      , 4 
                      , align := Fmt . Align . Left 
                      ) 
                ) 
            ; INC ( LAltNo ) 
            ; LAltRef := LAltRef . FsCondAltRef 
            ; IF LAltRef = NIL 
              THEN EXIT 
              ELSE
                Wr . PutText ( WrT , Wr . EOL ) 
              END (* IF *) 
            END (* LOOP *) 
          END (* Block *) 
        ELSE 
          InnerDumpNode ( FsNodeRef , Prefix ) 
        END (* IF *) 
      END DumpNode 

  ; BEGIN 
      DumpNode ( FsRootNodeRef , Prefix ) 
    END DumpFsTree 

(* VISIBLE: *)
; PROCEDURE DumpAllFsTreesStream  
    ( WrT : Wr . T 
    ; Lang : LbeStd . LangTyp 
    ; IncludeDefaultValuedFields : BOOLEAN := FALSE 
    ) 
  RAISES { Thread . Alerted , Wr . Failure } 
  (* Write a readable dump of all Fs trees to an already-open Wr.T. *) 

  = VAR LLangInfo : LangInfoRefTyp 
  ; VAR LFsNodeRef : LangUtil . FsNodeRefTyp 

  ; BEGIN (* DumpAllFsTreesStream *)
      LLangInfo := LangMap . LangInfo ( Lang )
    ; IF LLangInfo # NIL
      THEN 
        FOR RTok := LbeStd . Tok__FirstLangDep 
            TO LangUtilLo . TokClassLastTok ( LLangInfo , TokClassAsFixed ) 
        DO 
          LFsNodeRef  
            := LLangInfo ^ . FsTreeMapRef ^
                 [ RTok - LbeStd . Tok__FirstLangDep ] 
        ; Wr . PutText 
            ( WrT , LangUtil . TokImage ( RTok , Lang ) & ":" & Wr . EOL ) 
        ; DumpFsTree 
            ( WrT 
            , Lang 
            , Fmt . Pad 
                ( "T" & Fmt . Int ( RTok ) , 6 , align := Fmt . Align . Left ) 
            , LFsNodeRef 
            , IncludeDefaultValuedFields 
            ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        END (* FOR *) 
      END (* IF *)
    END DumpAllFsTreesStream 

(* VISIBLE: *) 
; PROCEDURE AssignCosts 
    ( LangInfo : LangInfoRefTyp ; Gram : LRTable . GrammarTyp ) 
  (* Set insertion and deletion costs for tokens. *) 

  = BEGIN 
      IF LangInfo # NIL AND Gram # NIL 
      THEN 
(* TODO: Something sensible about insertion and deletion costs. *) 
        IF Gram . DeletionCostRef # NIL 
        THEN 
          FOR RTok := LangInfo . StringToks 
                   TO LangInfo . VarTermToks - 1  
          DO
            WITH WCost 
                 = Gram . DeletionCostRef ^ 
                     [ RTok - Gram . FirstTerminal ] 
            DO
              WCost := WCost * 4 
            END (* WITH *) 
          END (* FOR *) 
        END (* IF *) 
      ; IF Gram . InsertionCostRef # NIL 
        THEN 
          FOR RTok := LangInfo . StringToks 
                      TO LangInfo . VarTermToks - 1 
          DO
            WITH WCost 
                 = Gram . InsertionCostRef ^ 
                     [ RTok - Gram . FirstTerminal ] 
            DO
              WCost := WCost * 2 
            END (* WITH *) 
          END (* FOR *) 
        END (* IF *) 
      END (* IF *) 
    END AssignCosts 

; VAR GDevelWritePath : TEXT := "" 
(* TODO: ^Unify this with its counterpart/namesake in LdlBatch and maybe 
          elsewhere. *) 

(* VISIBLE: *) 
; PROCEDURE FinishLRGeneration 
    ( LangInfo : LangInfoRefTyp 
    ; Gram : LRTable . GrammarTyp 
    ; ProdCount : LRTable . ProdNoTyp 
    ) 
  RAISES { AssertionFailure } 
  (* Do the LR generation on Gram of LangInfo, with ProdCount productions. *) 

  = VAR LMessageCt : PortTypes . Card32Typ  
  ; VAR LMessageText : TEXT 
  ; VAR LDebugFileSuffix : TEXT 
  ; VAR LFileName : TEXT 
  ; VAR LNoConflict : BOOLEAN 


  ; BEGIN 
      IF Gram # NIL 
      THEN 
        IF Gram = LangInfo . Gram 
        THEN 
          LMessageText := "For HANDWRITTEN concrete grammar:" 
        ; LDebugFileSuffix := "LRDebugMan" 
        ELSIF Gram = LangInfo . GenGram 
        THEN 
          LMessageText := "For GENERATED concrete grammar:" 
        ; LDebugFileSuffix := "LRDebugGen" 
        ELSE (* This shouldn't happen. *) 
          LMessageText := "For OTHER concrete grammar:" 
        ; LDebugFileSuffix := "LRDebugOther" 
        END (* IF *) 
      ; LMessageCt 
          := Messages . MessageCount ( MessageCodes . KindTyp . MkError ) 
      ; Messages . ZeroMessageCounts ( ) 
(* FIXME: ^This is a mess.  We probably should have a mechanism in Messages
        to push/pop message counts.  We need to avoid having, e.g., errors
        during manual grammar processing to torpedo generated grammar 
        processing, yet still avoid forging ahead too much after errors and
        crashing because of uncomputed data. *) 
      ; Check . Verbose := FALSE (* May change. *) 
(* TODO: Somehow get an option into here whether to write LR debug info. *) 
      ; LFileName 
          := Misc . JoinPath
               ( GDevelWritePath 
               , SharedStrings . ToText ( LangInfo . LanguageName ) 
                 & LDebugFileSuffix 
               ) 
      ; TRY 
          Debug . DebugWr  := VersionedFiles . OpenWrite ( LFileName )
        ; Check . Verbose := TRUE 
        EXCEPT 
        VersionedFiles . Error ( EMessage ) 
        => Messages . StdErrLine 
             ( EMessage & Wr . EOL 
               & ",  while opening LR debug file \"" & LFileName & "\"" 
             ) 
        ELSE
          Messages . StdErrLine 
            ( " Unable to write LR debug information to file \"" 
              & LFileName & "\"" 
            ) 
        END (* TRY EXCEPT *)  
      ; Automaton . AfterProductions ( Gram ) 
      ; Automaton . Build ( Gram ) 
      ; LALRLookahead . ComputeLALR ( Gram ) 
      ; Check . CheckForConflicts ( Gram , LNoConflict ) 
      ; IF Reduce . IsReduced ( Gram ) 
        THEN 
          Gen . GenTables ( Gram ) 
        ELSE 
          SemError0 
            ( AFT . E_Grammar_in_improper_form_not_generating_item_lists )
        END (* IF *) 
      ; IF Messages . MessageCount ( MessageCodes . KindTyp . MkError ) = 0   
        THEN 
          IF Automaton . ActualProdCt ( Gram ) < ProdCount + 1 
          THEN 
            SemError2 
              ( Automaton . ActualProdCt ( Gram ) 
              , ProdCount + 1 
                (* How cheap!  The type of these parameters to SemError2
                   is supposed to be an Est node number, but we should
                   get away with using a different kind of integer.
                *) 
              , AFT . I_CS_Lower_production_count_than_predicted 
              ) 
          END 
        END (* IF *) 
      ; AssignCosts ( LangInfo , Gram ) 
      ; Messages . TextOnly 
          ( LMessageText , MessageCodes . KindTyp . MkInformation ) 
      ; Infos . WriteInfo ( Gram ) 
      ; IF Check . Verbose 
        THEN 
          Wr . Close ( Debug . DebugWr ) 
        ; Check . Verbose := FALSE 
        END (* IF *) 
      END (* IF *) 
    END FinishLRGeneration  

(* VISIBLE: *) 
; PROCEDURE NoteNontermKindForRange 
    ( Gram : LRTable . GrammarTyp
    ; FromTok : LbeStd . TokTyp 
    ; ToTok : LbeStd . TokTyp 
    ; Kind : NontermKindTyp 
    ) 
  (* For all toks in [FromTok,ToTok), set the nonterminal kind to Kind, *) 

  = BEGIN
      FOR RTok := FromTok TO ToTok - 1 
      DO Automaton . NoteNontermKind ( Gram , RTok , Kind ) 
      END (* FOR *)  
    END NoteNontermKindForRange  

(* VISIBLE: *) 
; PROCEDURE NoteStandardNontermKinds 
    ( LangInfo : LangInfoRefTyp ; Gram : LRTable . GrammarTyp) 
  (* Set standard nonterminal kinds from token ranges.  Only sets Kind 
     if not previously other than NtkNull. Only does cases that are the 
     same for handwritten and generated grammars.
  *) 

  = BEGIN
      FOR RTok := LangUtilLo . TokClassFirstTok ( LangInfo , TokClassAsFixed ) 
          TO LangUtilLo . TokClassLastTok ( LangInfo , TokClassAsFixed ) 
      DO Automaton . NoteNontermKind 
           ( Gram 
           , RTok 
           , NontermKindTyp . NtkAbstractAndConcrete 
           , DefaultOnly := TRUE 
           )
      END (* FOR *)  
    ; FOR RTok := LangUtilLo . TokClassFirstTok ( LangInfo , TokClassSublist ) 
          TO LangUtilLo . TokClassLastTok ( LangInfo , TokClassSublist ) 
      DO Automaton . NoteNontermKind 
           ( Gram , RTok , NontermKindTyp . NtkSublist , DefaultOnly := TRUE ) 
      END (* FOR *)  
    ; FOR RTok := LangUtilLo . TokClassFirstTok ( LangInfo , TokClassAsClass ) 
          TO LangUtilLo . TokClassLastTok ( LangInfo , TokClassAsClass ) 
      DO Automaton . NoteNontermKind 
           ( Gram , RTok , NontermKindTyp . NtkUnused , DefaultOnly := TRUE ) 
      END (* FOR *)  
    END NoteStandardNontermKinds 

(* VISIBLE: *) 
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

  = VAR LDeclNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LWasFound : BOOLEAN 

  ; BEGIN (* DeclOfRef *) 
      IF AstView . IsNull ( RefNode ) 
      THEN RETURN NIL 
      ELSE 
        WITH WSemRef = LangInfo . SemMapRef ^ [ RefNode . NodeNo ] . SemRef 
        DO TYPECASE WSemRef <* NOWARN *>
           OF NULL 
           => TextIntSymbolTable . FindOrAdd 
                ( LangInfo . SymbolTable 
                , SharedStrings . ToText ( RefNode . NodeRef ) 
                , RefNode . NodeNo 
                , LWasFound 
                , LDeclNodeNo 
                ) 
           ; IF LWasFound 
             THEN 
               TYPECASE LangInfo . SemMapRef ^ [ LDeclNodeNo ] . SemRef 
               OF NULL => 
               | SemDeclTyp ( TSemDecl ) 
               => WSemRef 
                    := NEW 
                         ( SemRefTyp 
                         , NodeNo := RefNode . NodeNo 
                         , RefDeclId := TSemDecl . NodeNo 
                         , RefTok := TSemDecl . DeclTok 
                         ) 
               ELSE 
               END (* TYPECASE *) 
             ELSE 
               SemError ( RefNode . NodeNo , AFT . E_UndeclaredIdentifier ) 
             ; RETURN NIL 
             END (* IF *) 
           | SemRefTyp ( TSemRef ) 
           => LDeclNodeNo := TSemRef . RefDeclId 
           | SemFirstOccTyp ( TSemDecl ) 
           => LDeclNodeNo := TSemDecl . NodeNo 
           END (* TYPECASE *) 
        ; TYPECASE LangInfo . SemMapRef ^ [ LDeclNodeNo ] . SemRef 
          OF NULL => 
          | SemFirstOccClassTyp ( TFirstOccClass ) 
          => (* A known class. *) 
             IF TFirstOccClass . SingletonTok # LbeStd . Tok__Null 
             THEN 
               LDeclNodeNo 
                 := LangInfo . TokMapRef ^ 
                      [ TFirstOccClass . SingletonTok 
                        - TokMapBias 
                      ] 
             END (* IF *) 
          ELSE 
          END (* TYPECASE *) 
        ; RETURN LangInfo . SemMapRef ^ [ LDeclNodeNo ] . SemRef 
        END (* WITH *) 
      END (* IF *) 
    END DeclOfRef 

; EXCEPTION Done 

(* VISIBLE: *) 
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

  = VAR Result : BOOLEAN 

  ; PROCEDURE VisitMember ( MemberTok : LbeStd . TokTyp ) 
    RAISES { Done } 

    = BEGIN (* VisitMember *) 
        IF ( LowerTok <= MemberTok AND MemberTok <= UpperTok ) 
           OR ( SecondLowerTok <= MemberTok AND MemberTok <= SecondUpperTok ) 
        THEN (* The class member is in one of the ranges.  Keep checking. *) 
        ELSE (* The class member is not in either range. *) 
          Result := FALSE 
        ; RAISE Done 
        END (* IF *) 
      END VisitMember 

  ; VAR LAsDeclNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LSemDecl : SemDeclTyp 
  ; VAR LTok : LbeStd . TokTyp 
  ; VAR LWasFound : BOOLEAN 

  ; BEGIN (* EachMemberIsInSomeRange *) 
      TYPECASE Node . NodeRef 
      OF NULL 
      => RETURN TRUE
                (* All these defective data cases return TRUE, to avoid
                   producing derived errors. *)  
      | SharedStrings . T ( TString ) 
      => TextIntSymbolTable . Find 
           ( LangInfo . SymbolTable 
           , SharedStrings . ToText ( TString ) 
           , LWasFound 
           , LAsDeclNodeNo 
           ) 
      ; IF LWasFound 
        THEN 
          LSemDecl := LangInfo . SemMapRef ^ [ LAsDeclNodeNo ] . SemRef 
        ; IF LSemDecl = NIL
          THEN RETURN TRUE 
          ELSE 
            LTok := LSemDecl . DeclTok 
          ; IF ( LowerTok <= LTok AND LTok <= UpperTok ) 
               OR ( SecondLowerTok <= LTok AND LTok <= SecondUpperTok ) 
            THEN (* The token itself is in one of the ranges. *) 
              RETURN TRUE 
            ELSIF LangUtilLo . TokHasClassRange 
                    ( LangInfo , LTok , TokClassAsClass , TokClassCsClass )
(* BEWARE: ^This previously, probably wrongly, included sublist, listcard, and
           partial toks as well.  Probably because they were added later. *) 
            THEN (* The token is a class.  Check its class members. *) 
              Result := TRUE 
            ; <* FATAL ANY *> BEGIN 
                TRY 
                  TokRelation . ForAllGroundMembers 
                    ( LangInfo . ClassRelation , LTok , VisitMember ) 
                EXCEPT Done => 
                END (* TRY EXCEPT *) 
              END (* Block *) 
            ; RETURN Result 
            ELSE 
              RETURN FALSE 
            END (* IF *) 
          END (* IF *) 
        ELSE 
          RETURN TRUE  
        END (* IF *) 
      ELSE 
        RETURN TRUE 
      END (* TYPECASE *) 
    END EachMemberIsInSomeRange 

(* VISIBLE: *) 
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

  = VAR Result : BOOLEAN 

  ; PROCEDURE VisitMember ( MemberTok : LbeStd . TokTyp ) 
    RAISES { Done } 

    = BEGIN (* VisitMember *) 
        IF ( LowerTok <= MemberTok AND MemberTok <= UpperTok ) 
           OR ( SecondLowerTok <= MemberTok AND MemberTok <= SecondUpperTok ) 
        THEN (* The class member is in one of the ranges.  We are done. *) 
          Result := TRUE 
        ; RAISE Done 
        ELSE (* The class member is not in either range.  Keep checking. *) 
        END (* IF *) 
      END VisitMember 

  ; VAR LAsDeclNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LSemDecl : SemDeclTyp 
  ; VAR LTok : LbeStd . TokTyp 
  ; VAR LWasFound : BOOLEAN 

  ; BEGIN (* AMemberIsInSomeRange *) 
      TYPECASE Node . NodeRef 
      OF NULL 
      => RETURN TRUE
                (* All these defective data cases return TRUE, to avoid
                   producing derived errors. *)  
      | SharedStrings . T ( TString ) 
      => TextIntSymbolTable . Find 
           ( LangInfo . SymbolTable 
           , SharedStrings . ToText ( TString ) 
           , LWasFound 
           , LAsDeclNodeNo 
           ) 
      ; IF LWasFound 
        THEN 
          LSemDecl := LangInfo . SemMapRef ^ [ LAsDeclNodeNo ] . SemRef 
        ; IF LSemDecl = NIL
          THEN RETURN TRUE 
          ELSE 
            LTok := LSemDecl . DeclTok 
          ; IF ( LowerTok <= LTok AND LTok <= UpperTok ) 
               OR ( SecondLowerTok <= LTok AND LTok <= SecondUpperTok ) 
            THEN (* The token itself is in one of the ranges. *) 
              RETURN TRUE 
            ELSIF LangUtilLo . TokHasClassRange 
                    ( LangInfo , LTok , TokClassAsClass , TokClassCsClass )
(* BEWARE: ^This previously, probably wrongly, included sublist, listcard, and
           partial toks as well.  Probably because they were added later. *) 
            THEN (* The token is a class.  Check its class members. *) 
              Result := FALSE 
            ; <* FATAL ANY *> BEGIN 
                TRY 
                  TokRelation . ForAllGroundMembers 
                    ( LangInfo . ClassRelation , LTok , VisitMember ) 
                EXCEPT Done => 
                END (* TRY EXCEPT *) 
              END (* Block *) 
            ; RETURN Result 
            ELSE 
              RETURN FALSE 
            END (* IF *) 
          END (* IF *) 
        ELSE 
          RETURN TRUE  
        END (* IF *) 
      ELSE 
        RETURN TRUE 
      END (* TYPECASE *) 
    END AMemberIsInSomeRange 

; CONST FsNodeArrayInitial = 20 

(* VISIBLE: *) 
; PROCEDURE MapFsEstChildren ( RootFsNodeRef : LangUtil . FsNodeRefTyp ) 
  : FsNodeArrayRefTyp 
  (* Return a map from left-to-right ordinal to FsNodeRefTyp pointers,
     for each Est child in the FsTree rooted at RootFsNodeRef.
  *) 

  = VAR MfcTempResult : FsNodeArrayRefTyp 
  ; VAR MfcNext : CARDINAL

  ; PROCEDURE MfcAppend ( FsEstChildRef : LangUtil . FsNodeRefTyp )   
 
    = VAR LNew : FsNodeArrayRefTyp  

    ; BEGIN 
        IF LAST ( MfcTempResult ^ ) < MfcNext 
        THEN 
          LNew := NEW ( FsNodeArrayRefTyp , 2 * NUMBER ( MfcTempResult ^ ) )  
        ; SUBARRAY ( LNew^ , 0 , NUMBER ( MfcTempResult ^ ) ) := MfcTempResult ^
        ; MfcTempResult := LNew 
        END (* IF *)
      ; MfcTempResult ^ [ MfcNext ] := FsEstChildRef  
      ; INC ( MfcNext ) 
      END MfcAppend  

  ; PROCEDURE MfcRecurse ( FsNodeRef : LangUtil . FsNodeRefTyp ) 

    = VAR LFsNodeRef : LangUtil . FsNodeRefTyp  

    ; BEGIN 
        CASE FsNodeRef . FsKind 
        OF FsKindTyp . FsKindEstChildOfFixed
        , FsKindTyp . FsKindEstChildOfList 
        => IF FsNodeRef . FsEstChildIsOptional 
          THEN MfcAppend ( FsNodeRef ) 
          END (* IF *) 

        | FsKindTyp . FsKindEstFixedVert 
        , FsKindTyp . FsKindEstFixedHoriz 
        , FsKindTyp . FsKindEstFixedFill 
        , FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeHoriz 
        , FsKindTyp . FsKindSubtreeFill 
        => FOR RI := FIRST ( FsNodeRef . FsChildren ^ ) 
                     TO LAST ( FsNodeRef . FsChildren ^ )                    
           DO 
             MfcRecurse ( FsNodeRef . FsChildren ^ [ RI ] ) 
           END (* FOR *) 

        | FsKindTyp . FsKindCondFmt 
        => LFsNodeRef := FsNodeRef 
        ; WHILE LFsNodeRef . FsCondAltRef # NIL 
          DO LFsNodeRef := LFsNodeRef . FsCondAltRef 
          END (* WHILE *) 
        ; IF LFsNodeRef . FsKind = FsKindTyp . FsKindCondFmt 
          THEN MfcRecurse ( LFsNodeRef . FsEstDescendantRef ) 
          ELSE MfcRecurse ( LFsNodeRef ) 
          END (* IF *) 
        
        ELSE (* Ignore other leaf nodes. *) 
        END (* CASE *) 
      END MfcRecurse 

  ; BEGIN 
      VAR LResult : FsNodeArrayRefTyp 
    ; BEGIN (* Block *) 
        MfcTempResult := NEW ( FsNodeArrayRefTyp , FsNodeArrayInitial )  
      ; MfcNext := 0 
      ; MfcRecurse ( RootFsNodeRef ) 
      ; IF LAST ( MfcTempResult ^ ) < MfcNext 
        THEN 
          RETURN MfcTempResult 
        ELSE 
          LResult := NEW ( FsNodeArrayRefTyp , MfcNext )  
        ; LResult ^ := SUBARRAY ( MfcTempResult ^ , 0 , MfcNext )  
        ; RETURN LResult 
        END (* IF *) 
      END (* Block *) 
    END MapFsEstChildren 

(* Generated concrete tokens: *) 

(* VISIBLE: *) 
; PROCEDURE NewInternalConcreteTok 
    ( LangInfo : LangInfoRefTyp ; FsNodeRef : LangUtil . FsNodeRefTyp ) 
  : LbeStd . TokTyp 
  (* Allocate an internally-generated token. *) 

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
    END NewInternalConcreteTok 

(* Concrete tokens for combinations of list cardinalities. *) 

(* VISIBLE: *) 
; PROCEDURE ListCardTok  
    ( LangInfo : LangInfoRefTyp 
    ; ListTok : LbeStd . TokTyp 
    ; Card : ListCardTyp 
    ) 
  : LbeStd . TokTyp  
  RAISES { AssertionFailure } 
  (* The list cardinality token for Card of list ListTok. 
     Identity if ListTok is not known to be an abstract list token. 
  *) 

  = VAR LListSemDecl : SemDeclTyp 

  ; BEGIN 
      LListSemDecl  := SemDeclOfTok ( LangInfo , ListTok ) 
    ; TYPECASE LListSemDecl 
      OF NULL => RETURN ListTok 

      | SemDeclAsListNodeTyp ( TSemDeclAsList )  
      => IF Card = ListCardTyp . ListCardEmpty 
            AND TSemDeclAsList . DeclKind = DeclKindTyp . PlusRule 
         THEN (* Shouldn't happen, but be paranoid. *) 
           RETURN ListTok 
         ELSE 
           Assert 
             ( TSemDeclAsList . ListCardToks [ Card ] 
               >= LbeStd . Tok__FirstLangDep 
             , AFT . A_ListCardTok_No_cardinality_token 
             ) 
         ; RETURN TSemDeclAsList . ListCardToks [ Card ] 
         END (* IF *) 

      ELSE RETURN ListTok  
      END (* TYPECASE *) 
    END ListCardTok 

(* VISIBLE: *) 
; PROCEDURE ListCardSetAtomTok
    ( LangInfo : LangInfoRefTyp 
    ; ListAsTok : LbeStd . TokTyp 
    ; CardSet : ListCardSetTyp 
    ; MustBePresent : BOOLEAN := FALSE  
    ) 
  : LbeStd . TokTyp 
  RAISES { AssertionFailure } 
  (* An atom (WRT LangInfo) token for the pair (ListAsTok,CardSet),
     allocated in the token space of GcsChildToks.
  *) 

  = BEGIN 
      CantHappen ( AFT . A_ListCardSetAtomTok_Unimplemented ) 
    ; RETURN LbeStd . Tok__Null  
    END ListCardSetAtomTok 

(* Atomic productions for classes. *) 

(* VISIBLE: *) 
; PROCEDURE TokForAtomTokSet 
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
              2) Increments its UseCount by UseCountIncrement 
              3) If its UseCount goes 0-to-positive, increments the grammar
                 production count by the set cardinality. 
  *) 

(* TODO: Unravel the possibility that this could someday apply to 
         either concrete grammar.  Right now, it has builtin references to 
         the generated grammar only (e.g. GenProductions).  *) 

(* TODO: Do we want to split the case UseCountIncrement=0,MustBePresent
         (done in pass 4) from otherwise (done in pass 1)?
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
        THEN (* There will have to be productions for it. *)
          INC ( LangInfo ^ . GenProductions , LCard ) 
        END (* IF *)  
      ; INC ( LClassInfoRef ^ . UseCount , UseCountIncrement ) 
      ; RETURN LClassInfoRef ^ . ClassTok
      ELSIF MustBePresent 
      THEN 
        CantHappen ( AFT . A_LdlSemantics_TokForAtomTokSet__Not_in_table )  
      ; RETURN LbeStd . Tok__Null (* Just to suppress compiler warning. *) 
      ELSE (* Add a new entry to the ClassTable. *) 
        LClassInfoRef := NEW ( ClassInfo . ClassInfoRefTyp ) 
      ; LClassTok := NewInternalConcreteTok ( LangInfo , FsNodeRef )  
      ; LClassInfoRef ^ . ClassTok := LClassTok 
      ; IF UseCountIncrement > 0 
        THEN (* There will have to be productions for it. *)
          INC ( LangInfo ^ . GenProductions , LCard ) 
        END (* IF *)  
      ; LClassInfoRef ^ . UseCount := UseCountIncrement 
      ; EVAL LangInfo ^ . ClassTable . put ( TokSet , LClassInfoRef ) 
      ; RETURN LClassTok 
      END (* IF *) 
    END TokForAtomTokSet 

(* Atomic productions for RHS strings. *) 

(* VISIBLE: *) 
; PROCEDURE TokForAtomTokString
    ( LangInfo : LangInfoRefTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; Right : LRTable . TokArrayRefTyp 
      (* ^Must be immutable. May be NIL to denote the empty string. *) 
    ; UseCountIncrement : CARDINAL := 0
    ; MustBePresent : BOOLEAN := FALSE  
    ) 
  : LbeStd . TokTyp 
  RAISES { AssertionFailure } 
  (* An atom (WRT LangInfo) token for the token string Right,
     allocated in the token space of GcsChildToks.
     
(* TODO: Document side effects, similar to TokForAtomTokSet. *) 
  *) 

(* TODO: Unravel the possibility that this could someday apply to 
         either concrete grammar.  Right now, it has builtin references to 
         the generated grammar only (e.g. GenProductions).  *) 

  = VAR LTokStringInfoRef : TokStringInfo . TokStringInfoRefTyp 
  ; VAR LLHSTok : LbeStd . TokTyp 
  ; VAR LStringNumber : CARDINAL 
  
  ; BEGIN 
      IF Right = NIL 
      THEN RETURN LbeStd . Tok__Empty 
      ELSE 
        LStringNumber := NUMBER ( Right ^ ) 
      ; IF LStringNumber = 1 
        THEN RETURN Right ^ [ 0 ] 
        ELSIF LangInfo ^ . TokStringTable . get 
                ( Right , (* VAR *) LTokStringInfoRef ) 
        THEN (* It's already in the TokStringTable. *) 
          IF LTokStringInfoRef ^ . UseCount = 0 AND UseCountIncrement > 0 
          THEN (* There will have to be a production for it. *) 
            INC ( LangInfo ^ . GenProductions ) 
          END (* IF *)  
        ; INC ( LTokStringInfoRef ^ . UseCount , UseCountIncrement ) 
        ; RETURN LTokStringInfoRef ^ . LHSTok
        ELSIF MustBePresent 
        THEN 
          CantHappen ( AFT . A_LdlSemantics_TokForAtomTokString__Not_in_table )
        ; RETURN LbeStd . Tok__Null (* Just to suppress compiler warning. *) 
        ELSE (* Add a new entry to the TokStringTable. *) 
          LTokStringInfoRef := NEW ( TokStringInfo . TokStringInfoRefTyp ) 
        ; IF FALSE AND LStringNumber = 0 
          THEN 
(* REVIEW: It would be nicer to use LbeStd.Tok__Empty here, but we have 
           Automaton, etc. set up to allocate arrays that don't have space
           for the language-independent tokens in LbeStd.  For now, we go
           ahead and generate a unique (for this grammar) new token that
           derives <empty>.  Think about what to do about this. 
*) 
            LLHSTok := LbeStd . Tok__Empty 
          ELSE 
            LLHSTok := NewInternalConcreteTok ( LangInfo , FsNodeRef )  
          END (* IF *) 
        ; LTokStringInfoRef ^ . LHSTok := LLHSTok 
        ; IF UseCountIncrement > 0 
          THEN (* There will have to be a production for it. *) 
            INC ( LangInfo ^ . GenProductions ) 
          END (* IF *)  
        ; LTokStringInfoRef ^ . UseCount := UseCountIncrement 
        ; EVAL LangInfo ^ . TokStringTable . put ( Right , LTokStringInfoRef ) 
        ; RETURN LLHSTok 
        END (* IF *) 
      END (* IF *) 
    END TokForAtomTokString 

(* VISIBLE: *) 
; PROCEDURE LdlModuleName ( ImageRef : PaintHs . ImageTransientTyp ) : TEXT 
  (* The name following "LDL" in an ldl description. *) 

  = BEGIN 
      TYPECASE ImageRef . ItPers . IpSemRoot 
      OF NULL 
      => RETURN ImageRef . ItPers . IpImageName  
      | LangInfoRefTyp ( TLangInfoRef )  
      => RETURN SharedStrings . ToText ( TLangInfoRef . LanguageName ) 
      ELSE RETURN ""
      END (* IF *) 
    END LdlModuleName 

(* VISIBLE: *) 
; PROCEDURE StackTokCounts1 ( LangInfo : LangInfoRefTyp ) 
  RAISES { AssertionFailure } 
  (* Stack up the counts of token kinds in LangInto.TokCounts, into
     LangInfo.TokPart, with each member of the partition set to the 
     starting token number for its namesake tokens.  Do this for
     tokens in kinds ConstTerm through AsCsClass.  Except, for 
     AsSublist and AsPartial, set TokPart to a bias to be added to
     the list token, to get its sublist/partial token.  These two members
     don't change during the next pass.   
  *) 

  = VAR LTokCt : LbeStd . TokTyp 

  ; BEGIN 
      WITH WTokCounts = LangInfo . TokCounts ^ 
           , WTokPart = LangInfo . TokPart ^ 
      DO
        WTokCounts [ TokClassVarTermMod ] 
          := WTokCounts [ TokClassVarTerm ] 
        (* One VarTermMod token for every VarTerm token. *) 
      ; WTokCounts [ TokClassSublist ] 
          := WTokCounts [ TokClassAsPlus ] + WTokCounts [ TokClassAsStar ] 
      ; WTokCounts [ TokClassPartial ] 
          := WTokCounts [ TokClassAsPlus ] + WTokCounts [ TokClassAsStar ] 
        (* One Sublist tok and one Partial tok for each list. *) 
      ; LTokCt := WTokPart [ TokClassConstTerm ]
      ; Assert 
          ( LTokCt 
            = WTokCounts [ TokClassNull ] 
              + WTokCounts [ TokClassBuiltin ]
              + WTokCounts [ TokClassConstTerm ]
            (* These were assigned in Pass1. *)    
          , AFT . A_LdlSemantics_StackTokCounts1_BadConstTerm
          ) 
      ; FOR RTokClass := TokClassVarTerm TO TokClassAsCsClass 
        DO 
          WTokPart [ RTokClass ] := LTokCt 
          (* Beginning of range for RTokClass. *)
        ; INC ( LTokCt , WTokCounts [ RTokClass ] ) 
        END (* FOR *) 
      ; DEC ( WTokPart [ TokClassSublist ] , WTokPart [ TokClassAsPlus ] ) 
      ; DEC ( WTokPart [ TokClassPartial ] , WTokPart [ TokClassAsPlus ] ) 
      (* ^Change these to biases to be added to a Plus or Star token, to get the
         Sublist/Partial token. *) 
      END (* WITH *) 
    END StackTokCounts1 

(* VISIBLE: *) 
; PROCEDURE StackTokCounts2 ( LangInfo : LangInfoRefTyp ) 
  (* Stack up the counts of token kinds in LangInto.TokCounts, into
     LangInfo.TokPart, with each member of the partition set to the 
     starting token number for its namesake tokens.  Do this for
     tokens in kinds CsClass through  CsGen.  Also set elements for
     Unused. 
  *) 

  = VAR LTokCt : LbeStd . TokTyp 

  ; BEGIN 
      WITH WTokCounts = LangInfo . TokCounts ^ 
           , WTokPart = LangInfo . TokPart ^ 
      DO
        LTokCt := WTokPart [ TokClassAsCsClass ]
      ; FOR RTokClass := TokClassCsClass TO TokClassCsFixed 
        DO 
          WTokPart [ RTokClass ] := LTokCt 
          (* Beginning of range for RTokClass. *)
        ; INC ( LTokCt , WTokCounts [ RTokClass ] )
        END (* FOR *) 
      ; WTokPart [ TokClassUnused ] := LbeStd . Tok__LastReal  
      ; WTokCounts [ TokClassUnused ] := LbeStd . Tok__LastReal - LTokCt 
      END (* WITH *) 
    END StackTokCounts2 

(* VISIBLE: *) 
; PROCEDURE CheckTokPart ( LangInfo : LangInfoRefTyp ) 
  RAISES { AssertionFailure } 

  = PROCEDURE Error ( Msg : TEXT ) 
    RAISES { AssertionFailure } 

    = BEGIN 
        Assertions . CantHappen ( AFT . A_LdlSemantics_CheckTokPart_Mismatch ) 
      END Error 

  ; BEGIN 
      WITH WPart = LangInfo . TokPart ^ 
      DO 
        IF LangInfo . StringToks # WPart [ TokClassConstTerm ] 
        THEN Error ( "StringToks" ) 
        END (* IF *) 
      ; IF LangInfo . VarTermToks # WPart [ TokClassVarTerm ] 
        THEN Error ( "VarTermToks" ) 
        END (* IF *) 
      ; IF LangInfo . VarTermModToks # WPart [ TokClassVarTermMod ] 
        THEN Error ( "VarTermModToks" ) 
        END (* IF *) 
      ; IF LangInfo . AsPlusToks # WPart [ TokClassAsPlus ] 
        THEN Error ( "AsPlusToks" ) 
        END (* IF *) 
      ; IF LangInfo . AsStarToks # WPart [ TokClassAsStar ] 
        THEN Error ( "AsStarToks" ) 
        END (* IF *) 
      ; IF LangInfo . AsFixedToks # WPart [ TokClassAsFixed ] 
        THEN Error ( "AsFixedToks" ) 
        END (* IF *) 
      ; IF LangInfo . AsSublistToks # WPart [ TokClassSublist ] 
        THEN Error ( "AsSublistToks" ) 
        END (* IF *) 
      ; IF LangInfo . AsListCardToks # WPart [ TokClassListCard ] 
        THEN Error ( "AsListCardToks" ) 
        END (* IF *) 
      ; IF LangInfo . AsPartialToks # WPart [ TokClassPartial ] 
        THEN Error ( "AsPartialToks" ) 
        END (* IF *) 
      ; IF LangInfo . AsClassOnlyToks # WPart [ TokClassAsClass ] 
        THEN Error ( "AsClassOnlyToks" ) 
        END (* IF *) 
      ; IF LangInfo . AsCsClassToks # WPart [ TokClassAsCsClass ] 
        THEN Error ( "AsCsClassToks" ) 
        END (* IF *) 
      ; IF LangInfo . CsAltToks # WPart [ TokClassCsClass ] 
        THEN Error ( "CsAltToks" ) 
        END (* IF *) 
      ; IF LangInfo . CsPlusToks # WPart [ TokClassCsPlus ] 
        THEN Error ( "CsPlusToks" ) 
        END (* IF *) 
      ; IF LangInfo . CsPluralToks # WPart [ TokClassCsPlural ] 
        THEN Error ( "CsPluralToks" ) 
        END (* IF *) 
      ; IF LangInfo . CsStarToks # WPart [ TokClassCsStar ] 
        THEN Error ( "CsStarToks" ) 
        END (* IF *) 
      ; IF LangInfo . CsFixedToks # WPart [ TokClassCsFixed ] 
        THEN Error ( "CsFixedToks" ) 
        END (* IF *) 
      ; IF LangInfo . GcsChildToks # WPart [ TokClassCsGen ] 
        THEN Error ( "GcsChildToks" ) 
        END (* IF *) 
      END (* IF *) 
    END CheckTokPart 

; BEGIN (* LdlSemantics *) 
    RegisteredAnalyzers := RegisteredAnalyzersTyp { NIL , .. } 
  END LdlSemantics 
. 
