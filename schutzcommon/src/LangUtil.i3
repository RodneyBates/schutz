
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LangUtil 

(* Utilities concerning the definition of the language being edited.  
   This is intended to contain things that are needed for actual editing
   and not just language generation.  But it seems to have gotten a little
   mixed up with things in LdlSemantics. 
*) 

; IMPORT Date 
; IMPORT Fingerprint 
; IMPORT Thread 

; IMPORT Assertions 
; IMPORT EstHs 
; FROM Failures IMPORT Backout 
; IMPORT IntSets 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT PortTypes 
; IMPORT ScannerIf 
; IMPORT SchutzCoroutine 
; IMPORT SharedStrings 
; IMPORT SuffixInfo 
; IMPORT UncertainBool 

; EXCEPTION LangError ( TEXT ) 

(* Identification of languages: *) 

; TYPE LangVersionTyp = LbeStd . VersionTyp  

; PROCEDURE LangIdRef ( Lang : LbeStd . LangTyp ) : LangIdRefTyp  
  (* NIL, if language is not in LangMap. *) 

; TYPE LangIdTyp 
    = RECORD 
        LangName : TEXT 
      ; LangShortName : TEXT 
      ; LangVersion : LangVersionTyp 
      ; LangDate : Date . T 
      ; LangFingerprint : Fingerprint . T 
      END (* RECORD *) 

; TYPE LangIdRefTyp = REF LangIdTyp 

; PROCEDURE LangIdImage 
    ( READONLY LangId : LangIdTyp ; Indent : PortTypes . Int32Typ := 0 ) 
    : TEXT 

(* File suffixes: *) 

; PROCEDURE LangSuffixOfFileName ( FileName : TEXT ) : TEXT 
(* Can be a full path name.  From the right, removes version suffixes and 
   underscore suffixes.  From the left, removes all but the characters 
   following the last dot.
*)  
   
; EXCEPTION DuplicateSuffix 

; PROCEDURE RetSuffixInfo ( Suffix : TEXT ) : SuffixInfo . T 

; PROCEDURE Suffix ( Info : SuffixInfo . T ) : TEXT 

; PROCEDURE MergeSuffixes 
    ( Lang : LbeStd . LangTyp ) RAISES { DuplicateSuffix } 
  (* ^Call this after adding a new language to LangMap. *) 

(* Language definitions. *) 

; PROCEDURE LoadLanguage ( Suffix : TEXT ) 
  RAISES { LangError , Thread . Alerted } 
  (* Ensure the language definition for the file suffix Suffix is loaded. *) 

(* Managing lexical scanners. *) 

; PROCEDURE RegisterScanner 
    ( Lang : LbeStd . LangBuiltinTyp ; Scanner : SchutzCoroutine . ProcOfT ) 

; PROCEDURE ScannerIfForLang ( Lang : LbeStd . LangTyp ) 
  : ScannerIf . ScanIfTyp  

(* Separators between tokens *) 
  (* Unless something changes, the following is not needed anymore. *) 
  (* The following property is no longer needed. 4-10-92. 
  (* NeedsSep tells whether two successive tokens need a 
     blank between to prevent their becoming one token. 
     Works on terminals, nonterminals, and mixtures, 
     with the following, generator-enforced 
 
     RULE: (a,b are any terminals, 
            X,Y are any grammar symbols, 
            A,B,C,D are any strings) 
 
           for all a,b, 
 
           There exists p in boolean, such that 
 
             for all X & Y, such that 
                       S =>* AXYB 
                  and  AX =>* Ca 
                  and  YB =>* bD 
 
             needsSep ( X , Y ) = p. 
 
     corrolary: needsSep ( a , b ) = p 
 
  *) 
  *) 
  (* NeedsSep ( L , LbeStd . Tok__Null , T ) is always false. *) 
  (* NeedsSep ( L , T , LbeStd . Tok__Null ) is always false. *) 

; PROCEDURE NeedsSep 
    ( Lang : LbeStd . LangTyp ; Left , Right : LbeStd . TokTyp ) : BOOLEAN 

(* Predicates, used in conditional formatting *) 

; TYPE PredicateKindTyp 
    = { PredicateKindNull
      , PredicateKindFalse
      , PredicateKindTrue 
      , PredicateKindAbsent 
      , PredicateKindPresent 
      , PredicateKindEmptyList 
      , PredicateKindNonemptyList 
      , PredicateKindPluralList 
      , PredicateKindNonpluralList 
        (* ^For the list cardinality predicates, an absent list node is 
           considered empty, as is a present list node with no children. 
           When the list is a required child of a fixed parent, the Est-
           building in the Parser optimizes empty list into absent list.
           Otherwise, these cases are distinct Ests. 
        *) 
      , PredicateKindInClass 
        (* ^An absent node is not in any class, not even a universal one. *) 
      } 
; TYPE PredicateKindPackedTyp = (*BITS 16 FOR*) PredicateKindTyp 

; PROCEDURE PredicateKindImage ( Value : PredicateKindTyp ) : TEXT 

; TYPE PredicateKindSetTyp = SET OF PredicateKindTyp 

; CONST PredicateKindSetEmpty = PredicateKindSetTyp { } 
; CONST PredicateKindSetHasOptionIdSet 
    = PredicateKindSetTyp 
        { PredicateKindTyp . PredicateKindAbsent 
        , PredicateKindTyp . PredicateKindPresent 
        , PredicateKindTyp . PredicateKindEmptyList 
        , PredicateKindTyp . PredicateKindNonemptyList 
        } 

(* A flattened variant record: *) 
; TYPE PredicateTyp 
    = RECORD 
        PredicateKind : PredicateKindTyp 
        := PredicateKindTyp . PredicateKindNull 
      ; PredicateClass : LbeStd . TokTyp := LbeStd . Tok__Null 
        (* ^Only meaningful for PredicateKindInClass *) 
      END (* RECORD  PredicateTyp *) 

; CONST PredicateNull 
    = PredicateTyp 
        { PredicateKind := PredicateKindTyp . PredicateKindNull 
        , PredicateClass := LbeStd . Tok__Null 
        } 

; TYPE FsChildNoTyp = PortTypes . Card8Typ 

; CONST FsChildNoNull = LAST ( FsChildNoTyp ) 
; CONST FsChildNoAlt = FsChildNoNull - 1
  (* This is a pseudo-child number that refers not to a regular child,
     but to FsCondAltRef. *)  
; CONST FsChildNoMax = FsChildNoAlt - 1  
; CONST FsChildNoListEstChild = 0 

; PROCEDURE FsChildNoImage 
    ( Value : FsChildNoSignedTyp 
    ; IsInsideList : BOOLEAN := FALSE 
    ; MakeShort : BOOLEAN := FALSE 
    ) 
  : TEXT 

; TYPE FsChildNoSignedTyp = PortTypes . Int16Typ 

(* Indent codes *) 

; TYPE IndentCodeTyp = PortTypes . Int16Typ 

; CONST IndentCodeNull = VAL ( FIRST ( IndentCodeTyp ) , IndentCodeTyp ) 
; CONST IndentCodeFirstNormal = IndentCodeNull + 1   
; CONST IndentCodeLastNormal = VAL ( LAST ( IndentCodeTyp ) , IndentCodeTyp ) 
; CONST IndentCodeInitial = 0 
; CONST IndentCodeModTok = 1 (* Used for ModTok Est child. *) 

; CONST IndentCodeImage = PortTypes . Int32Image 

; PROCEDURE IndentAmt 
    ( Lang : LbeStd . LangTyp ; IndentCode : IndentCodeTyp ) 
  : LbeStd . LimitedCharNoSignedTyp 

(* Format syntax rules *) 
(* Rules which format syntax must satisfy: 
   For each Est node kind, it must be unconditional whether the 
   format of the entire Est ends with a line break. 
*) 

; TYPE FsKindTyp 
    = { FsKindNull 
      , FsKindBegOfImage 
      , FsKindEndOfImage 
      , FsKindInsTok 
      , FsKindEstChildOfFixed 
      , FsKindEstChildOfList 
      , FsKindLineBreakOpt 
      , FsKindLineBreakReqd 
      , FsKindAstString 
      , FsKindEstFixedVert 
      , FsKindEstFixedHoriz 
      , FsKindEstFixedFill 
      , FsKindSubtreeVert 
      , FsKindSubtreeHoriz 
      , FsKindSubtreeFill 
      , FsKindEstListVert 
      , FsKindEstListHoriz 
      , FsKindEstListFill 
      , FsKindEstListTrailHoriz  
      , FsKindEstListTrailVert  
      , FsKindEstListTrailFill   
      , FsKindCondFmt 
      } 

; PROCEDURE FsKindImage ( Value : FsKindTyp ) : TEXT 

(* Sets of FsKinds *) 

; TYPE FsKindSetTyp = SET OF FsKindTyp 

; CONST FsKindSetEmpty = FsKindSetTyp { } 
    (* ^Not used 10-97 *) 

; CONST FsKindSetNull = FsKindSetTyp { FsKindTyp . FsKindNull } 

; CONST FsKindSetLineBreak 
    = FsKindSetTyp 
        { FsKindTyp . FsKindLineBreakOpt
        , FsKindTyp . FsKindLineBreakReqd 
        } 

; CONST FsKindSetVert 
    = FsKindSetTyp 
        { FsKindTyp . FsKindEstFixedVert 
        , FsKindTyp . FsKindEstListVert  
        , FsKindTyp . FsKindEstListTrailVert  
        , FsKindTyp . FsKindSubtreeVert  
        } 

; CONST FsKindSetEstFill 
    = FsKindSetTyp 
        { FsKindTyp . FsKindEstFixedFill  
        , FsKindTyp . FsKindEstListFill  
        , FsKindTyp . FsKindEstListTrailFill  
        } 

; CONST FsKindSetEstFixed 
    = FsKindSetTyp 
        { FsKindTyp . FsKindEstFixedVert 
        , FsKindTyp . FsKindEstFixedHoriz 
        , FsKindTyp . FsKindEstFixedFill 
        } 

; CONST FsKindSetSubtree 
    = FsKindSetTyp 
        { FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeHoriz 
        , FsKindTyp . FsKindSubtreeFill 
        } 

; CONST FsKindSetEstListTrail 
    = FsKindSetTyp 
        { FsKindTyp . FsKindEstListTrailHoriz  
        , FsKindTyp . FsKindEstListTrailVert  
        , FsKindTyp . FsKindEstListTrailFill   
        } 

; CONST FsKindSetEstList 
    = FsKindSetTyp 
        { FsKindTyp . FsKindEstListVert 
        , FsKindTyp . FsKindEstListHoriz 
        , FsKindTyp . FsKindEstListFill 
        , FsKindTyp . FsKindEstListTrailHoriz  
        , FsKindTyp . FsKindEstListTrailVert  
        , FsKindTyp . FsKindEstListTrailFill   
        } 

; CONST FsKindSetAstString = FsKindSetTyp { FsKindTyp . FsKindAstString } 
 
; CONST FsKindSetInsTok = FsKindSetTyp { FsKindTyp . FsKindInsTok } 

; CONST FsKindSetEstInterior 
    = FsKindSetEstFixed + FsKindSetEstList 

; CONST FsKindSetEst 
    = FsKindSetEstFixed + FsKindSetEstList + FsKindSetAstString 

; CONST FsKindSetFsRoot 
    = FsKindSetEstFixed + FsKindSetEstList + FsKindSetAstString 
      + FsKindSetInsTok + FsKindSetNull  

; CONST FsKindSetFormattingFixed 
    = FsKindSetTyp 
        { FsKindTyp . FsKindBegOfImage 
        , FsKindTyp . FsKindEndOfImage 
        , FsKindTyp . FsKindInsTok 
        , FsKindTyp . FsKindLineBreakOpt 
        , FsKindTyp . FsKindLineBreakReqd  
        } 

; CONST FsKindSetFormattingList 
    = FsKindSetTyp 
        { FsKindTyp . FsKindInsTok 
        , FsKindTyp . FsKindLineBreakOpt 
        , FsKindTyp . FsKindLineBreakReqd  
        } 

; CONST FsKindSetFormatting 
    = FsKindSetFormattingFixed + FsKindSetFormattingList 

; CONST FsKindSetChildOfFixed 
        (* This can be inside a list FS tree. *) 
    = FsKindSetFormatting 
      + FsKindSetSubtree 
      + FsKindSetTyp 
          { FsKindTyp . FsKindAstString 
          , FsKindTyp . FsKindEstChildOfFixed 
          , FsKindTyp . FsKindEstChildOfList 
          , FsKindTyp . FsKindCondFmt 
          } 

; CONST FsKindSetEstChild 
    = FsKindSetTyp 
        { FsKindTyp . FsKindEstChildOfFixed 
        , FsKindTyp . FsKindEstChildOfList 
        } 

; CONST FsKindSetChildOfCondFmt  
    = FsKindSetFormatting 
      + FsKindSetSubtree 
      + FsKindSetEstChild 
      + FsKindSetAstString  

; CONST FsKindSetModTok 
    = FsKindSetTyp { FsKindTyp . FsKindAstString , FsKindTyp . FsKindInsTok } 
    (* ^Not used 10-97 *) 

; CONST FsKindSet0thChildOfList 
    = FsKindSetSubtree 
      + FsKindSetTyp 
          { FsKindTyp . FsKindEstChildOfList , FsKindTyp . FsKindCondFmt } 

; CONST FsKindSetDeletable 
    = FsKindSetTyp 
        { FsKindTyp . FsKindInsTok 
        , FsKindTyp . FsKindLineBreakOpt 
        , FsKindTyp . FsKindLineBreakReqd 
        , FsKindTyp . FsKindEstChildOfFixed (* Only if NIL or EstDummyTyp. *) 
        , FsKindTyp . FsKindEstChildOfList (* Only if NIL or EstDummyTyp. *) 
        } 

; CONST FsKindSetHasChildren 
    = FsKindSetEstFixed 
      + FsKindSetSubtree
      + FsKindSetEstList 
      + FsKindSetTyp { FsKindTyp . FsKindCondFmt } 

; CONST FsKindSetLeaf 
    = FsKindSetFormatting 
      + FsKindSetEstChild 
      + FsKindSetTyp { FsKindTyp . FsKindAstString } 

(* Fs Node numbers.  These are used to number the nodes in an FsTree. *) 
; TYPE FsNodeNoTyp = PortTypes . Card8Typ 

; CONST FsNodeNoNull = LAST ( FsNodeNoTyp ) 
; CONST FsNodeNoOvflo = FsNodeNoNull - 1 
; CONST FsNodeNoMax = FsNodeNoOvflo - 1 

; PROCEDURE FsNodeNoImage ( Value : FsNodeNoTyp ) : TEXT 

(* Format kinds *) 

; TYPE FmtKindTyp 
    = { FmtKindUnknown , FmtKindVert , FmtKindHoriz , FmtKindFill } 

; PROCEDURE FmtKindImage ( Value : FmtKindTyp ) : TEXT 

(* Can an FsTree node format to the empty string? *) 

; TYPE FormatsEmptyTyp 
    = { FeUnknown (* No children/alternatives exist. (This occurs only
                     temporarily, during computation of one of the other
                     values for either a concatenation or an alternation. *)  
      , FeNo      (* Known never to format to the empty string. *)  
      , FeMaybe   (* Depends on this property of at least one child/alt. *)  
      , FeYes     (* Known to sometimes format to empty string. *)   
      } 

; TYPE FormatsEmptySetTyp = SET OF FormatsEmptyTyp 

; CONST FormatsEmptySetUncertain 
    = FormatsEmptySetTyp 
        { FormatsEmptyTyp . FeUnknown , FormatsEmptyTyp . FeMaybe } 

; CONST FormatsEmptySetKnown 
    = FormatsEmptySetTyp { FormatsEmptyTyp . FeNo , FormatsEmptyTyp . FeYes } 

; CONST FormatsEmptySetPossiblyYes 
    = FormatsEmptySetTyp 
        { FormatsEmptyTyp . FeMaybe , FormatsEmptyTyp . FeYes } 

; PROCEDURE FormatsEmptyImage ( Value : FormatsEmptyTyp ) : TEXT  
  
; PROCEDURE FormatsEmptyCat ( Left , Right : FormatsEmptyTyp ) 
    : FormatsEmptyTyp 
  (* Composite of this property for two concatenated subtrees. *) 

; PROCEDURE FormatsEmptyAlt ( Left , Right : FormatsEmptyTyp ) 
    : FormatsEmptyTyp 
  (* Composite of this property for two alternative subtrees. *) 

(* Required/optional Est children. *) 

; TYPE ChildOptTyp 
    = { OptRequired (* Est child is required by abstract syntax. *) 
      , OptOptional (* Optional by AS and by predicate(s). *) 
      , OptAbsent   (* Optional by AS, but known absent by predicate(s). *) 
      , OptPresent  (* Optional by AS, but known present by predicate(s). *) 
      } 

; TYPE ChildOptSetTyp = SET OF ChildOptTyp 

; CONST ChildOptSetOptByAS
    = ChildOptSetTyp
        { ChildOptTyp . OptOptional 
        , ChildOptTyp . OptAbsent 
        , ChildOptTyp . OptPresent  
        } 

; CONST ChildOptSetCouldBeAbsent
    = ChildOptSetTyp { ChildOptTyp . OptOptional , ChildOptTyp . OptAbsent } 

; PROCEDURE ChildOptImage ( Value : ChildOptTyp ) : TEXT

; PROCEDURE ChildOpt ( IsOptional : BOOLEAN ) : ChildOptTyp  

(* Child predicate properties and their sets. *) 

; TYPE CardTyp = { Absent , EmptyList , SingletonList , PluralList } 
; TYPE CardSetTyp = SET OF CardTyp 
; CONST CardSetEmpty = CardSetTyp { } 
; CONST CardSetUniverse = CardSetTyp { FIRST ( CardTyp ) .. LAST ( CardTyp ) } 
; CONST CardSetList 
    = CardSetTyp 
        { CardTyp . EmptyList , CardTyp . SingletonList , CardTyp . PluralList }
; CONST CardSetAbsent = CardSetTyp { CardTyp . Absent } 
; CONST CardSetEmptyList = CardSetTyp { CardTyp . EmptyList } 
; CONST CardSetNonemptyList 
    = CardSetTyp { CardTyp . SingletonList , CardTyp . PluralList } 
; CONST CardSetPluralList = CardSetTyp { CardTyp . PluralList } 
; CONST CardSetNonpluralList 
    = CardSetTyp { CardTyp . EmptyList , CardTyp . SingletonList } 

; PROCEDURE CardImage ( Value : CardTyp ) : TEXT 

; PROCEDURE CardSetImage ( Value : CardSetTyp ) : TEXT 

(* Children of format syntax tree nodes. *) 

; TYPE FsChildrenArrayTyp = ARRAY OF FsNodeRefTyp 
; TYPE FsChildrenArrayRefTyp = REF FsChildrenArrayTyp 

(* The format syntax tree node.  This was once a variant record, with FsKind 
   as tag field.  There are more FsKind values than variants, so FsKind is 
   retained.  The record is just flattened for ease of conversion.  Someday, 
   it would be nice to make this an object hierarchy, with lots of 
   NARROWs/TYPECASEs in the code.  But there are only a fixed number of 
   these nodes for a given language, so it is probably not a huge space 
   issue. *) 

(* OptionIds, for manual and generated grammars: *) 

; TYPE OptionIdArrayTyp = ARRAY BOOLEAN OF LRTable . OptionIdTyp 
                                (* ^FALSE for manual grammar
                                    TRUE for generated.
                                *) 
; CONST OptionIdArrayNull = OptionIdArrayTyp { LRTable . OptionIdNull , .. }

(* Format syntax trees: *) 

; TYPE FsNodeRefTyp <: FsNodeRefPublicTyp 
; TYPE FsNodeRefPublicTyp 
    = LbeStd . LbeRootTyp 
        OBJECT 
          FsPostNodeNo : FsNodeNoTyp := FsNodeNoNull 
        ; FsFmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull 
          (* ^Meaningful for all Fs tree leaves , FsKindEstList* and interior
             nodes inside it, and FsKindCondFmt. 
             For the interior nodes, the FmtNo of the distinguished Est child
             of the list and/or the CondFmt. 
          *) 
        ; FsLeftFmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull 
          (* ^ = FmtNo of child 0, or = FsFmtNo, for a leaf. *) 
        ; FsRightFmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull 
          (* ^ = FmtNo of child NUMBER ( FsChildren ^ ) - 1, 
               or = FsFmtNo, for a leaf. 
          *) 
        ; FsIsInsideList : (*BITS 1 FOR*) BOOLEAN := FALSE 
        ; FsIsInsideCondFmt : (*BITS 1 FOR*) BOOLEAN := FALSE  
        ; FsIsInsideFill : (*BITS 1 FOR*) BOOLEAN := FALSE 
          (* ^Meaningful for all FsKinds. *) 
        ; FsIsAutonomous : (*BITS 1 FOR*) BOOLEAN := FALSE 
        ; FsContainsLineBreak : (*BITS 1 FOR*) BOOLEAN := FALSE 
          (* For EstList/Fixed nodes, has BOI/EOI *) 
        ; FsDeletableItemsAreToRight : (*BITS 1 FOR*) BOOLEAN := TRUE 
        ; FsPad1 : (*BITS 2 FOR*) [ 0 .. 0 ] := 0 
        ; FsKind : FsKindTyp := FsKindTyp . FsKindNull 
          (* ^This is the tag field for a flattened variant record. *) 
        ; FsIndentCode : IndentCodeTyp := IndentCodeNull 
          (* ^Meaningful for anything that can have leading mods, and for 
              FsKindAstString.  Adds to IndentPos of parent Est node to give 
              IndentPos for non-absolute-position items. 
          *)
        ; FsListSliceThruFmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull 
          (* Meaningful only for FsKindEstList* and every descendant thereof.
             The format number of the rightmost item of a whole list slice. 
             This is the rightmost CondFmt child, if any, or the Est child. 
          *) 
        ; FsFormatsEmpty : (*BITS 2 FOR*) UncertainBool . T 
                         := UncertainBool . T . Unknown 
          (* ^For valid format syntax, will not end up Unknown. *) 
          (* ^For FsKindAsList*, this applies to the conditional construct,
             if any, surrounding the FsKindEstChildOfList, or if none, the
             FsKindEstChildOfList itself. 
          *) 
        ; FsHasLineBreak : (*BITS 2 FOR*) FormatsEmptyTyp 
            := FormatsEmptyTyp . FeUnknown 
        ; FsPad2 : (*BITS 4 FOR*) [ 0 .. 0 ] := 0 
        ; FsTok : LbeStd . TokTyp := LbeStd . Tok__Null 
          (* ^Meaningful for all FsKinds. *) 
        ; FsSublistTok : LbeStd . TokTyp := LbeStd . Tok__Null 
          (* ^Meaningful only for FsKindEstList* and any descendant thereof. 
             The internally-generated grammar symbol for sublist slices.
             It is nonterminal-like in having a slice of an Est list attached.
             It is terminal-like in the CFG in that it can only come from
             ParseTrv.  It is never reduced-to in the CFG.
             With a handwritten CS, this may correspond to a build stack
             sequence mixing list slices and individual elements, with 
             insertion tokens as needed.  
             With machine-generated CS, this corresponds only to a list slice. 
          *) 
        ; FsPartialTok : LbeStd . TokTyp := LbeStd . Tok__Null 
          (* ^Meaningful only for FsKindEstList* and any descendant thereof. 
             Used only with machine-generated CS.  
             The internally-generated nonterminal for a partially reduced
             sublist.  Corresponds to a single element (with appropriate
             per-element insertion tokens, a single slice, or an open
             merge state.  In the CFG, it is a pure nonterminal.  ParseTrv 
             never supplies it. 
          *) 
        ; FsTrailingTok : LbeStd . TokTyp := LbeStd . Tok__Null 
          (* ^Meaningful only for FsKindEstListTrail* and any descendant 
             thereof.  The internally-generated nonterminal for a complete list,
             with optional trailing separator (which is represented by an Ast
             child with kind bit EstChildKindTrailingSep and Tok__Empty.  
             Non-recursive.  
          *) 
        ; FsEmptyListTok : LbeStd . TokTyp := LbeStd . Tok__Null 
        ; FsSingletonListTok : LbeStd . TokTyp := LbeStd . Tok__Null 
        ; FsPluralListTok : LbeStd . TokTyp := LbeStd . Tok__Null 
        ; FsOptionIds := OptionIdArrayNull 
          (* ^FALSE is for manual grammar, TRUE for generated grammar.
             Meaningful on every optional principal child FsNode.  Here, 
             it identifies which element of the OptionId set from the 
             parse table tells whether the child is present.  This value
             is also copied up through every FS subtree that contains an
             optional principal child.     
             Also may be meaningful on any predicate node.  Here, if it 
             is _not_ OptionIdNull, it identifies the member of the 
             OptionIdSet that gives the value of the predicate.  
             Otherwise, the Parser will have to figure out some other 
             way what the value of the predicate is. 
          *)  
        ; FsChildren : FsChildrenArrayRefTyp := NIL 
          (* ^Meaningful for FsKindSetHasChildren. NIL Otherwise. 
             A list of other FsNodes as children. 
          *) 
        ; FsLdlNodeNo : LbeStd . EstNodeNoTyp := LbeStd . EstNodeNoNull 
          (* ^Node in LDL definition of corresponding FS rule part. *) 
        ; FsPlaceholder : FsNodeRefTyp := NIL  
        ; FsSingletonOptMin : LbeStd . TokTyp := LbeStd . Tok__Null 
        ; FsSingletonOptMapRef : LRTable . TokArrayRefTyp := NIL  
(* TODO:                         ^Put this type somewhere more general. *) 
          (* FsSingletonOpt* are meaningful for FsKindEstChildOf* and 
             anything that is a predicate or else alternative, i.e.,  
             FsKindCondFmt, and sometimes FsKindSubtree*.
             Together, they map list element tokens that can have the singleton
             list optimization applied, to the corresponding list token. 
             <ElemTok> -> <ListTok> 
             IFF FsSingletonOptMapRef ^ [ <ElemTok> - FsSingletonOptMin ] 
                 exists and = <ListTok>    
          *)
 
      (* FsKind = FsKindBegOfImage , FsKindEndOfImage: 
           No additional fields. *) 
        (* FsTok = Tok__BegOfImage or Tok__EndOfImage *) 

      (* FsKind = FsKindInsTok 
         ^ A constant terminal, i.e. an inserted token, 
           not present in the Est *) 
        ; FsInsTokRef : SharedStrings . T := NIL 

      (* FsKind = FsKindEstChildOfFixed , FsKindEstChildOfList: 
           Est children.  These are leaves of an Fs tree which 
           correspond to Est children of the Est node associated 
           with the root of the Fs tree. The FsKind indicates 
           (redundantly, but more conveniently) whether the Est 
           parent was a fixed node or a list node. 
      *) 
          (* FsTok is the nonterminal for the descendant. *) 
        ; FsEstChildNo : LbeStd . EstChildNoTyp := LbeStd . EstChildNoNull 
          (* Child number of the corresponding Abstract child in the Est
             for the LDL definition this Fs tree was built from. 
          *) 
        ; FsLeadingChildCt : FsChildNoTyp := FsChildNoNull 
          (* For FsKindEstList* or any interior subtree inside it, or any
             FsKindCondFmt, or any interior subtree inside it.  Gives the 
             number of children before (and also the child number of) the 
             distinguished Est child. 
          *) 
        ; FsChildIndentCode : IndentCodeTyp := IndentCodeNull 
          (* ^Used to compute IndentPos to pass down to the 
              subtree.  This can be different from FsIndentCode, 
              which reflects only indentations within 
              this Est level and is used to compute the 
              indent position for a leading mod on this child. 
          *) 
        ; FsFirstListElemIndentCode : IndentCodeTyp := IndentCodeNull 
          (* The first Est child of either a fixed or list node needs a
             different indent code from the remaining children.  For a
             fixed node, each child has a different Fs node of kind 
             FsKindEstChildOfFixed, and these just have diffent values in
             field FsIndentCode.  For a list, there is only one Fs node
             of kind FsKindEstChildOfList.  Its FsFirstListElemIndentCode
             holds the indent code value for the first list child and its
             FsIndentCode holds the value for subsequent children. 
          *) 
        ; FsSameLineEstChildFmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull 
          (* For FsKindEstChildOfFixed: If not FmtNoNull, then those Est
             children, to the left of this one,  whose format numbers lie in 
             [ FsSameLineEstChildFmtNo .. FmtNo ) are on the same line, i.e.
             are not separated from this one by a line break. 
(* CHECK: What if there is a conditional line break? *) 
          *) 
        ; FsEstChildIsOptional : (*BITS 1 FOR*) BOOLEAN := FALSE 
          (* ^ For FsKindEstChildOfFixed: 
             (Always FALSE for FsKindEstChildOfList.
             Also copied up into FsKindCondFmt nodes. 
          *) 
        ; FsIsInFirstLine : (*BITS 1 FOR*) BOOLEAN := FALSE 
          (* Counting only line breaks (which are assumed taken), is this
             node in the first line of formatted text.  For a list, this
             assumes we are within the first list child and its insertion
             tokens. 
          *)
        ; FsIsRightOfEstListChild : (*BITS 1 FOR*) BOOLEAN := FALSE 
          (* Inside the tree for an Est list, are we in the part to the
             right of the Est child. 
          *)   
        ; FsEstChildOpt : (*BITS 2 FOR*) ChildOptTyp := ChildOptTyp . OptRequired
        ; FsPadChild : (*BITS 3 FOR*) [ 0 .. 0 ] := 0 
        ; FsEstDescendantRef : FsNodeRefTyp := NIL  
          (* For an FsKindEstList* or any interior node descendant thereof,
             this points to the leaf FsKindEstChildOf* descendant node. 
          *) 

      (* FsKind = FsKindLineBreakOpt , FsKindLineBreakReqd: 
         ^ Line breaks. 
           In the format syntax, line breaks can introduce a new 
           indent code.  But in Fs trees, it won't be used in this node. 
           Instead, it is propagated to other Fs nodes where it is 
           needed. 
      *) 
        ; FsLineBreakToReachFmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull 
          (* The FmtNo of the item through which stuff must fit on the same
             line, in order to not take this line break when filling. 
          *) 
        (* FsTok is Tok__BegOfLine. *) 

      (* FsKind = FsKindAstString: A variable terminal, i.e. an ast node that
         has a string value (and IS a string object). 
      *) 
        (* FsTok is for the class of string. *) 

          (* In the format syntax, an Est can introduce a new 
             indent code.  But in Fs trees, it won't be used at this node. 
             Instead, it is propagated to Fs nodes where it is 
             needed. 
          *) 

      (* FsKind = FsKindSubtreeVert, FsKindSubtreeHoriz , FsKindSubtreeFill  
             The purpose of subtrees whose roots have these kinds 
             is to delimit groups of things that can change their 
             horizontal/fill/vertical state independently. *)  

      (* FsKind = FsKindEstFixedVert , FsKindEstFixedHoriz , FsKindEstFixedFill 
             Somewhere in the entire subtree for an Est interior node, 
             there must be exactly one fsNode for each possible 
             Est child.  This will always be FsKindEstChildOfFixed. 
             The entire Fs tree corresponding to an EstFixed subtree will 
             have one of these kinds as its root.  They can also appear 
             properly inside the Fs tree for either an EstFixed or EstList 
             subtree. *)  
        (* FsTok is the nonterminal. *)
        (* FsPlaceholder points to an FsKindAstString placeholder node. *)  

      (* FsKind = FsKindEstList*
           ^ The root of an entire Fs tree for an Est list. 
             The Fs node will have several children. 
             Child 0 is for the Est Child, and has FsKindEstChildOfList. 
             Children 1 .. NUMBER ( FsChildren ^ ) - 1 are separators, and 
             are in FsKindSetListFormatting. 
        *) 
        (* FsTok is the nonterminal.  *) 
        (* FsPlaceholder points to an FsKindAstString placeholder node. *)  
        ; FsEstListHasSeps : BOOLEAN 
        ; FsEstListHasBookends : BOOLEAN 

      (* FsKind = FsKindCondFmt 
           ^ Conditional formatting. 
             If the predicate on the Est child corresponding to FmtNo 
             is true, certain formatters preceed and follow the Est 
             child.  They have FsKinds only in FsKindSetFormatting. 
             Children 0 .. FsLeadingChildCt - 1 are leading format tokens. 
             Child FsLeadingChildCt is for the Est Child. 
             Children FsLeadingChildCt + 1 .. NUMBER ( FsChildren ^ ) 
             are trailing format tokens. 
      *) 
        ; FsCondPredicate : PredicateTyp := PredicateNull 
        ; FsCondAltRef : FsNodeRefTyp 
          (* ^Points to the alternative, if the predicate fails. 
             May be: 1) Direct pointer to FsEstDescendantRef (ldl0 only) 
                     2) Pointer to another FsKindCondFmt node for another
                        alternative CASE predicate.  An ELSE alternative is
                        just FsKindCondFmt with PredicateKindTrue. (ldl1 only.)
                     3) Pointer to a copy of what FsEstDescendantRef points
                        to, but with different indentation.  This is in a
                        conditional format construct.  (ldl1 only)
                     4) NIL, if in an FsKindCondFmt node having 
                        PredicateKindTrue. (ldl1 only)
             Will also be NIL in the FsKindEstChildOf* that is the last 
             alternative in a conditional format construct. 
          *) 
        ; FsCondDoParse : BOOLEAN := TRUE 
          (* The Fs rule did NOT specify NOPARSE in this condition . *) 
        (* FsTok is the same as that of child numbered FsLeadingChildCt. *) 
        (* FsFmtNo = FmtNo of child FsLeadingChildCt *) 

      (* Fields meaningful for FsKindCondFmt and anything that can be an else 
         alternative, i.e. FsKindSubtree* and FsKindEstChildOf* 
      *)
        ; FsAltListTokSet : IntSets . T 
        ; FsAltStarListTokSet : IntSets . T 
        ; FsAltFixedTokSet : IntSets . T 
        ; FsAltCardSet : CardSetTyp := CardSetEmpty  
        ; FsAltParseTokSets : IntSets . T 
          (* ^For generated grammar only.  Whatever could be on the parse 
             stack for this child, which will include list cardinality tokens
             and fixed tokens. *) 

        (* Things that are used only temporarily during analysis: *) 
        ; FsAltTok : LbeStd . TokTyp := LbeStd . Tok__Null  
          (* ^If not Tok__Null, derives exactly the sublanguage of this 
             alternative. *)  
        ; FsAltFragCt : PortTypes . Card16Typ := 0  
          (* ^Number of CS fragments to be generated from this node. *) 
        ; FsAbsentEstChildFormatsEmpty : BOOLEAN 
          (* For any predicate, else alternative, or EstChild, no insertion
             tokens surround the Est child. *) 
        END (* OBJECT  FsNodeRefPublicTyp *) 

(* For each language (and also for each tree-building NT, which can 
   be the root of a disconnected tree), there is an augmented NT, 
   as in A' -> A$.  $ is the inner eof symbol (there is an outer 
   eof following $, which stops parsing).  $ is there to provide 
   a place to attach trailing comments etc.  There is also a 
   Est node kind for A' and a format syntax tree for that. 
   The Fs tree always has two children; the first is the 
   Est for A (possibly optional or a list) and the second 
   is $. 
*) 

; PROCEDURE CopyOfFsNode ( FsNodeRef : FsNodeRefTyp ) : FsNodeRefTyp 

; PROCEDURE FsRuleForTok 
    ( Lang : LbeStd . LangTyp 
    ; EstTok : LbeStd . TokTyp 
    ; IsPlaceholder : BOOLEAN := FALSE 
    ) 
  : FsNodeRefTyp 
  (* The FsNodeRef of the root of the format tree for an Est node. 
     Also works when EstTok is a list partial token. 
  *)
  (* NOTE: For every CFG terminal, fixed or variable, there is an 
           EstTok, as returned by EstUtil . EstTok.  Furthermore, 
           it has an FsRule, as returned by FsRule, which consists 
           of a single FsNode of FsKindInsTok or FsKindAstString, 
           respectively, and 
  *) 

; PROCEDURE FsRuleForEstChild  
    ( Lang : LbeStd . LangTyp 
    ; FsEstChildNode : FsNodeRefTyp 
    ; READONLY LeafElem : EstHs . LeafElemTyp 
    ) 
  : FsNodeRefTyp 
  RAISES { Backout } 
  (* Gives the FsNodeRef of the root of the appropriate format tree for an Est 
     node that is a child corresponding to FsEstChildNode, taking into account
     the singleton-list optimization.  If LeafElem leads to a singleton-list
     element for a list child of FsEstChildNode, then this will be the FsRule
     for that list.  Otherwise, same as EstUtil . FsRuleForEstNode.   
     PRE: FsEstChildNode . FsKind IN FsKindSetEstChild. 
  *) 

(* 
; PROCEDURE GetFsRuleForEstChild  
    ( Lang : LbeStd . LangTyp 
    ; FsEstChildNode : FsNodeRefTyp 
    ; READONLY LeafElem : EstHs . LeafElemTyp 
    ; VAR FsNode : FsNodeRefTyp 
    ; VAR IsSingletonList : BOOLEAN   
    ) 
    RAISES { Backout } 
  (* Gives the FsNodeRef of the root of the appropriate format tree for an Est 
     node, that is a child corresponding to FsEstChildNode, taking into account
     the singleton-list optimization.  If EstNodeRef is a singleton-list
     element for a list child of FsEstChildNode, then this will be the FsRule
     for that list and IsSingletonList will be TRUE .  Otherwise, same as 
     FsRuleForEstNode.   
     PRE: FsEstChildNode . FsKind IN FsKindSetEstChild. 
  *)
*)  

; PROCEDURE FsChildNoOfFmtNo 
    ( FsNodeRef : FsNodeRefTyp ; FmtNo : EstHs . FmtNoTyp ) : FsChildNoTyp 
  (* Given an Fs (sub)tree and a format number, return the child 
     number which has or contains this format number. If the FmtNo
     is for a subsequent alternative and is not for the EstChild, 
     returns FsChildNoAlt.  If the FmtNo is for the EstChild, returns 
     the child number that is/leads to the FsEstChild of this alternative. 
     Returns FsChildNoNull if anything is wrong. 
  *)

; PROCEDURE FsChildRefOfFmtNo 
    ( FsNodeRef : FsNodeRefTyp ; FmtNo : EstHs . FmtNoTyp ) : FsNodeRefTyp 
  (* Given an Fs (sub)tree and a format number, return its direct Fs child 
     that has or contains this format number.  If the FmtNo is for a
     different alternative and not for the EstChild, return the next 
     alternative (which might not be the right one either).  If the 
     FmtNo is for the EstChild, return the child that is/leads to the 
     FsEstChild of this alternative.  Return NIL if anything is wrong. 
  *)

; PROCEDURE FsLeafRefOfFmtNo 
    ( FsNodeRef : FsNodeRefTyp ; FmtNo : EstHs . FmtNoTyp ) : FsNodeRefTyp 
  (* Given an Fs (sub)tree and a format number, return the Fs leaf 
     that has this format number.  Will find the child having the FmtNo 
     in an alternative, except, for the EstChild, when it will
     return the FsEstChild of the first alternative. 
  *)

; PROCEDURE GetParentAndChildNoOfFmtNo  
    ( FsRootNodeRef : FsNodeRefTyp 
    ; FmtNo : EstHs . FmtNoTyp 
    ; VAR ParentFsNodeRef : FsNodeRefTyp 
    ; VAR ChildNo : FsChildNoTyp 
    ) 
  (* Starting with an FsSubtree, get the immediate parent
     Fs node and its child number, that lead to the leaf node 
     that has this format number. Will find the child having the FmtNo 
     in an alternative, except, for the EstChild, when it will
     return the FsEstChild of the first alternative. 
  *)

; PROCEDURE FsEstChildRef ( FsNodeRef : FsNodeRefTyp ) 
  : FsNodeRefTyp  
  (* If FsNodeRef is or is inside an FsKind*EstList*, FsKindCondFmt, or
     FsKindEstChildOf* node, return the unique leaf descendent of kind 
     FsKindEstChildOf*.  If this process leads to an FsKindCondFmt, 
     ignore alternatives and descend into it.  Otherwise, return NIL. 
  *) 

; PROCEDURE TopmostAltRef ( FsNodeRef : FsNodeRefTyp ) 
  : FsNodeRefTyp  
  (* If FsNodeRef is or is inside an FsKind*EstList*, FsKindCondFmt, or
     FsKindEstChildOf* node, return the unique descendent of kind 
     FsKindEstChildOf* or FsKindCondFmt.  Otherwise, return NIL. 
  *) 

; PROCEDURE OptSingletonListAsTok
    ( FsEstChildNode : FsNodeRefTyp ; ElemTok : LbeStd . TokTyp ) 
  : LbeStd . TokTyp 
  (* PRE: FsEstChildNode is either an FsEstChild or an FsCondFmt. *)  
  (* If ElemTok is a token that could be singleton-list optimized as a child 
     of FsEstChildNode, the abstract list token for the containing list.  
     Otherwise, LbeStd . Tok__Null.
  *) 

; PROCEDURE UnoptimizedSingletonListTok  
    ( FsEstChildNode : FsNodeRefTyp 
    ; MaybeElemTok : LbeStd . TokTyp 
    ; KindSet : EstHs . EstChildKindSetTyp
    ) 
  : LbeStd . TokTyp 
  (* PRE: FsEstChildNode is either an FsEstChild or an FsCondFmt. *)  
  (* If MaybeElemTok is a singleton-optimized list element in the context of
     FsEstChildNode and KindSet, the corresponding list token.  Otherwise,
     identity.
  *) 

; PROCEDURE TokImage 
    ( Tok : LbeStd . TokTyp ; Lang : LbeStd . LangTyp := LbeStd . LangNull ) 
    : TEXT 
   (* Possibly a token identifier, always followed by a numeric token in
      parentheses. 
   *) 

(* See also: LbeStd . *Tok*Image* and ParseHs . TokInfoImage. *)

; PROCEDURE TextForTok 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp )
  : TEXT 
  (* The display string for a token.  This is a token text for constant 
    terminals.  It is a placeholder for nonterminals that have Ests
    and for variable terminals.  It also gives a value for a
    VarTermModTok token.  
    It is "" otherwise. *)

; PROCEDURE DisplayStringForTok 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) : SharedStrings . T 
  (* The display string for a token, as it appears in an edit window.  
     For constant terminals, this is a token text, with quotes and escapes
     removed.  For variable terminals and nonterminals, including generated
     nonterminals, it is enclosed in placeholder delimiters.   
     It could be NIL if no string is available in the StringMap. 
  *) 

(* Est classes: *) 
(* Each named Est class also has a value of type LbeStd . TokTyp. *) 
(* Class inclusion.  Kind can also be another class. *) 

; PROCEDURE IsInClass 
    ( Lang : LbeStd . LangTyp 
    ; Tok : LbeStd . TokTyp 
    ; Class : LbeStd . TokTyp 
    ) 
    : BOOLEAN 
(* Class inclusion.  Tok can also be another class. *)

; PROCEDURE VarTermModTok 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) : LbeStd . TokTyp 
  (* For Varterm token Tok, the token that appears in a 
     ModTok Est root for it. Otherwise, identity. 
  *) 

; PROCEDURE VarTermTok 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) : LbeStd . TokTyp 
  (* For token Tok that appears in a ModTok Est root, the regular VarTerm
     token. Otherwise, identity. Inverse of VarTermModTok. 
  *) 

(* Classes of tokens: *) 

; PROCEDURE TokClass 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) 
  : LbeStd . TokClassTyp
  (* The TokClass that contains token Tok.  Works on builtin tokens too. *) 

; PROCEDURE IsTrailingTok 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) : BOOLEAN 
  (* Tok is a list token that has trailing separators. *) 

; PROCEDURE NontrailingCounterpart 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) 
  : LbeStd . TokTyp
  (* If Tok is a list token that has trailing separators, the
     token that does not have them.  Otherwise, identity. 
  *) 

; PROCEDURE IsGeneratedClassTok 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp )
  : BOOLEAN 

; PROCEDURE IsLiteral ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) 
  : BOOLEAN  
  RAISES { Backout } 

; PROCEDURE IsReserved ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) 
  : BOOLEAN  
  RAISES { Backout } 

; PROCEDURE Gram ( Lang : LbeStd . LangTyp ) : LRTable . GrammarTyp 

; PROCEDURE TokClassFirstTok  
    ( Lang : LbeStd . LangTyp ; TokClass : LbeStd . TokClassTyp ) 
  : LbeStd . TokTyp 
  (* Lowest numbered token belonging to TokClass, not counting LbeStd
     builtin tokens. 
  *) 

; PROCEDURE TokClassLastTok  
    ( Lang : LbeStd . LangTyp ; TokClass : LbeStd . TokClassTyp ) 
  : LbeStd . TokTyp 
  (* Highest numbered token belonging to TokClass, not counting LbeStd
     builtin tokens.  Not that if the class is empty, this will be one
     less than the result of TokClassFirstTok.  
  *) 

; PROCEDURE TopFsNodeRef ( ) : FsNodeRefTyp 
  (* A parentless node of kind FsKindEstChildOfFixed, needed to get
     some traversers started.  The whole Est is considered a child
     of this. *) 

; TYPE ScannerInitCallbackTyp 
    = PROCEDURE 
        ( String : TEXT 
        ; Tok : LbeStd . TokTyp 
        ; TokClass : LbeStd . TokClassTyp 
        ) 
  (* ^Mechanically generated initialization procedures make calls-back using
      this type.  A scanner would call the initialization procedure and 
      provide the callbacks, to initialize its tables of tokens.
  *)  

; VAR UniqueFsTreeTrailingSep : FsNodeRefTyp 
  (* Treat as CONST.  A single, unique Fs tree for formatting 
     EstHs.UniqueEstNodeTrailingSep. *) 
(* CHECK ^Do we even need this global variable, or will it always be accessed
         thru StdTokFsTreeMap? *) 

; END LangUtil 
. 
