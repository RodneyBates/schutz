
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE ParseHs 

(* Data structures for (re)parsing. *) 

; IMPORT Rd 

; IMPORT PortTypes 
; IMPORT LbeStd 
; IMPORT Marks 
; IMPORT EstHs 
; IMPORT LangUtil 
; IMPORT TravUtil 
; IMPORT SharedStrings 
; IMPORT Strings 
; IMPORT LRTable 
; IMPORT ScannerIf 

(* Temporary marks: *) 

; TYPE TempMarkTyp 
    = RECORD 
        TokMark : Marks . TokMarkTyp := Marks . TokMarkNull  
      ; EstRef : LbeStd . EstRootTyp := NIL 
      ; LineNo : LbeStd . LineNoTyp := LbeStd . LineNoNull 
        (* ^1-origin when in a blank line mod or off EndOfImage.  
           Zero-valued otherwise. 
        *)  
      ; CharPos : LbeStd . LimitedCharNoSignedTyp 
          := LbeStd . LimitedCharNoUnknown  
      END (* RECORD  TempMarkTyp *) 

; TYPE TempMarkArrayTyp = ARRAY OF TempMarkTyp 
; TYPE TempMarkArrayRefTyp = REF TempMarkArrayTyp 

; PROCEDURE CopyOfTempMarkList 
    ( OldTempMarkList : TempMarkArrayRefTyp 
    ; ToSs : LbeStd . MarkNoTyp 
      (* In the copy, marks at and above this subscript will be null. *) 
    ) 
  : TempMarkArrayRefTyp 

; TYPE TempMarkRangeTyp 
    = RECORD 
        From : LbeStd . MarkNoTyp 
      ; To : LbeStd . MarkNoTyp 
      END (* RECORD  TempMarkRangeTyp *) 

; CONST TempMarkRangeNull 
    = TempMarkRangeTyp 
        { From := LbeStd . MarkNoNull , To := LbeStd . MarkNoNull } 

; PROCEDURE TempMarkRangeImage ( Range : TempMarkRangeTyp ) : TEXT 

(* Information about a token delivered to the parser. *) 

; TYPE TokInfoTyp 
    = RECORD 
        TiSliceListRMRoot : SliceListElemRefTyp := NIL
        (* This is only to simplify debugging.  It duplcates TiInfo
           whenever ISTYPE(TiInfo,SliceListElemRefTyp), otherwise NIL.
        *)  
      ; TiInfo : REFANY := NIL 
        (* This will either be SliceListElemRefTyp, in most cases, or
           Parser.MergeInfoTyp when inside a BuildStackElemTyp for a token
           of TokClassPartial. 
        *) 
      ; TiFullTempMarkRange : TempMarkRangeTyp := TempMarkRangeNull 
        (* TiFullTempMarkRange is the range of temp mark subscripts of
           all temp marks in this token, including its attached mods. 
           When empty, it will = TempMarkRangeNull. 
        *) 
      ; TiPatchTempMarkRange : TempMarkRangeTyp := TempMarkRangeNull 
        (* TiPatchTempMarkRange gives a range of TempMarks that denote 
           points within an InsTok token that is not inside a subtree or 
           list slice, for which the parser must patch the TempMarks 
           to point to a newly constructed Est node when the token is 
           reduced in a building reduction.  ParseTrv also uses it to
           temporarily keep TempMark ranges that it later patches, before
           returning a token. Same convention for an empty range. 
        *) 
      ; TiTok : LbeStd . TokTyp := LbeStd . Tok__Null 
      ; TiSyntTokCt : LbeStd . LimitedTokCtTyp := 0 
      ; TiIsInterior : BITS 1 FOR BOOLEAN := FALSE 
      ; TiIsInsertionRepair : BITS 1 FOR BOOLEAN := FALSE 
      END (* RECORD TokInfoTyp *) 

(* Info needed for both Est traversal and file/keyboard scanning *) 
(* Everything in ScanInfoTyp is relative to NEW TOKENS. *) 

; TYPE ScanInfoTyp 
    = RECORD 
        SiCharPos : Strings . StringSsSignedTyp 
      ; SiLeftTokToPos : Strings . StringSsSignedTyp  
      (* SiLeftTokToPos is used to classify comments and detect blank lines. 
         It is the last char position of the token before the 
         current one on the same line. 
      *) 
      (* During parse traverse of an Est, SiCharPos and SiLeftTokToPos 
         are maintained only when 
           ( SiScanState # LbeStd . SsIdle ) 
           or PtseModTextIsToLeftOnLine 
      *) 
      ; SiTokBegPos : LbeStd . LimitedCharNoTyp 
(* CHECK: Although SiCharPos and SiTokBegPos have wide value ranges to handle, 
          e.g., very long comments, I am leaving SiTokBegPos limited.  I hope 
          it doesn't need the range.  If it should need it, this will interact 
          with other LimitedCharNoTyp values. 
*) 
      ; SiTokBegScanState : LbeStd . ScanStateTyp 
      ; SiScanState : LbeStd . ScanStateTyp 
      END (* RECORD  ScanInfoTyp *) 

(* Parse state.  Used in traversing for a parse. A single state of 
   such a traversal has one of these, and a stack of stackTyp objects *) 

(* TODO: Move this and its sets to ParseTrv, which is the only module
         that uses them.  Also, rename as PtsStateKindTyp. 
*) 
; TYPE ParseTravStateKindTyp 
    = { PtsKindBlanksThenLeadingMods 
      , PtsKindBlanksThenLexErrChars 
      , PtsKindBlanksThenAstString 
      , PtsKindBlanksThenModCmnt 
      , PtsKindBlanksThenInsTok 
      , PtsKindBlanksThenRescanModText 
      , PtsKindDoneWithEstTraversed 
      , PtsKindDoneWithEstUntraversed 
      , PtsKindDoneWithListSliceTraversed
      , PtsKindDoneWithListSliceUntraversed 
      , PtsKindDoneWithFsNode 
      , PtsKindEndOfImage 
      , PtsKindLeadingMods 
      , PtsKindTrailingMods 
      , PtsKindNewEst 
      , PtsKindRevisitNewEst 
      , PtsKindNewFsNode 
      , PtsKindRescanInsTok 
      , PtsKindRescanAstString 
      , PtsKindRescanLexErrChars  
      , PtsKindInsideInsTok 
      , PtsKindInsideLexErrChars 
      , PtsKindInsideAstString 
      , PtsKindInsideModCmnt 
      , PtsKindInsideModText 
      } 

; PROCEDURE ParseTravStateKindImage ( Kind : ParseTravStateKindTyp ) : TEXT 

; TYPE PtsKindSetTyp = SET OF ParseTravStateKindTyp 

; CONST PtsKindSetAttachedMods 
    = PtsKindSetTyp 
        { ParseTravStateKindTyp . PtsKindLeadingMods 
        , ParseTravStateKindTyp . PtsKindTrailingMods 
        } 

; CONST PtsKindSetDescendable 
    = PtsKindSetTyp 
        { ParseTravStateKindTyp . PtsKindDoneWithEstUntraversed
        , ParseTravStateKindTyp . PtsKindDoneWithListSliceUntraversed  
        } 

; CONST PtsKindSetTrailingNl  
    = PtsKindSetTyp 
        { ParseTravStateKindTyp . PtsKindInsideModCmnt 
        , ParseTravStateKindTyp . PtsKindInsideModText 
        } 

; TYPE ParseTravStateRefTyp 
    = OBJECT 
       PtsAdvanceStateRef : ParseTravStateRefTyp 
      (* ^If an advance operation from this state has been done, 
          this will point to the state it leads to, so that state does not 
          have to be recomputed, if the advance is repeated. *) 
      ; PtsTokInfo : TokInfoTyp 
      ; PtsPrevTokAfter : LbeStd . TokTyp (* After any scanning. *) 
      ; PtsNonCmntSeenOnLine : BOOLEAN 
      ; PtsSeqNo : PortTypes.Int32Typ 
      ; PtsScanInfo : ScanInfoTyp 
      END (* OBJECT  ParseTravStateRefTyp *) 

; TYPE ParseTravStateStreamRefTyp 
    = ParseTravStateRefTyp 
        OBJECT 
          PtssTokSeqNo : PortTypes . Int32Typ 
        ; PtssPosRelTo : LbeStd . LimitedCharNoTyp 
        END (* OBJECT  ParseTravStateSimpleTyp *) 

; TYPE ParseTravStateEstRefTyp 
    = ParseTravStateRefTyp 
        OBJECT 
          PtseStackEstRef : StackElemEstTyp 
        ; PtseStackFsRef : StackElemFsTyp 
        ; PtseDescendStateRef : ParseTravStateEstRefTyp 
          (* ^Like PtsAdvanceState, but for Descend operation. *) 
        ; PtseStringRef : SharedStrings . T 
          (* ^When rescanning a string, this points to it. *) 
        ; PtseDeferredInfoRef : DeferredInfoRefTyp 
        ; PtseEstListChildrenToPass : LbeStd . EstChildNoTyp := 0 
        ; PtsePrevTokBefore : LbeStd . TokTyp 
          (* ^Before any rescanning. *) 
        ; PtseTokTempMarkSs : LbeStd . MarkNoTyp 
        ; PtseStringFromPos : LbeStd . LimitedCharNoTyp
          (* ^When rescanning a string, its beginning position on line. *) 
        ; PtseRescanToPos : LbeStd . LimitedCharNoTyp 
          (* ^When rescanning a string or blanks, its position on line. *) 
        ; PtseLastFmtNoOnLine : EstHs . FmtNoTyp := EstHs . FmtNoNull 
        ; PtseStateKind : ParseTravStateKindTyp 
        ; PtseModTextIsToLeftOnLine : BOOLEAN 
        END (* OBJECT  ParseTravStateGenerarRefTyp *) 

(* Parse Traverse Stacks. *) 

; TYPE StackElemEstTyp 
    = OBJECT 
        SeEstLink : StackElemEstTyp 
      ; SeEstAdvanceState : ParseTravStateEstRefTyp 
        (* If an advance operation has been done from the state in 
          which either this Est node is visited as a whole or 
          the state where this Est node was descended from and 
          its last token is visited, then this points to the 
          state it leads to.  This allows these two traversals 
          to merge without recomputing the successors of the 
          merge point. *) 
      ; SeEstRootFsNodeRef : LangUtil . FsNodeRefTyp 
      ; SeEstTravInfo : TravUtil . EstTravInfoTyp 
      ; SeEstWholeSliceRMEstChildNo : LbeStd . EstChildNoTyp 
      ; SeEstWholeSliceToChildNo : LbeStd . EstChildNoTyp 
      ; SeEstIndentPos1 : LbeStd . LimitedCharNoTyp 
      ; SeEstIndentPosN : LbeStd . LimitedCharNoTyp 
      ; SeEstIndentPos : LbeStd . LimitedCharNoTyp 
        (* ^The current indent position, for whatever line we are on. *) 
      ; SeEstIsFirstLine : BITS 1 FOR BOOLEAN := FALSE 
      ; SeEstIsSublist : BITS 1 FOR BOOLEAN := FALSE 
        (* SeEstWholeSliceRMEstChildNo, SeEstWholeSliceToChildNo,
           have somewhat different meanings for a normal list and a sublist.
           For a normal list, they are for the sublist starting at the
           current place in the list. They get reset each time we have finished
           a previous sublist.  They keep their values between, to avoid
           recomputing them all the time.  
           For a sublist, they are computed when the node is created,
           (actually, copied from the parent full list), 
           represent the end of the sublist, and never change. *) 
      END (* OBJECT  StackElemEstTyp *) 

; TYPE StackElemFsTyp 
    = OBJECT 
        SeFsLink : StackElemFsTyp 
      ; SeFsNodeRef : LangUtil . FsNodeRefTyp 
      ; SeFsSeEstRef : StackElemEstTyp 
      ; SeFsIndentPos : LbeStd . LimitedCharNoTyp 
      ; SeFsSelfFsChildNo : LangUtil . FsChildNoTyp 
      ; SeFsFmtKind : LangUtil . FmtKindTyp 
      ; SeFsPredicate : BITS 1 FOR BOOLEAN 
      ; SeFsIsEst : BITS 1 FOR BOOLEAN 
(* TODO: Change the Fs trees so that they distinguish topmost from inner
        EstFixed Fs nodes, so this field is not needed. *) 
      END (* OBJECT  StackElemFsTyp *) 

(* The slice list.  A right-to-left, linear linked list of nodes 
   which hold slices of real Est lists, or just single Est subtrees. *) 

; TYPE SliceListElemRefTyp = REF SliceListElemTyp 
; TYPE SliceListElemTyp 
    = RECORD 
        SlePredLink : SliceListElemRefTyp 
      ; SleNodeRef : LbeStd . EstRootTyp 
      ; SleKindSet 
        : EstHs . EstChildKindSetTyp (* Meaningful IFF NOT SleIsSlice *) 
      ; SleFrom : LbeStd . EstChildNoTyp (* Meaningful IFF SleIsSlice *) 
      ; SleTo : LbeStd . EstChildNoTyp (* Meaningful IFF SleIsSlice *) 
      ; SleIsSlice : BOOLEAN (* otherwise, single element *) 
      END (* RECORD  SliceListElemTyp *) 

(* The build stack *) 

(* The build stack works about the same as a bottom up parsing stack. 
   It differs in that all nonterminals which appear on it are tree-building 
   nonterminals.  It is reduced at the same time as the parse stack is 
   reduced, but only if the parse stack is being reduced to such a NT. 
   At that time, it will contain an element for every terminal and 
   nonterminal that contribute to the abstract node to be built. 
 
   A subset of these will be used to build an Est node. 
   Constant terminals will, of course, not be put into the Est, as 
   they are insertion tokens there. 
   If any token has BseWasInsertedByParser, then the token is present in the 
   (repaired) parse, but should not appear in the built Est node. 
   Either it will be omitted (if a nonterminal or variable terminal) 
   or covered by a ModDel (if a constant terminal). 
 
   In addition, tokens with BseWasDeletedByParser my be present.  These are 
   not present in the (repaired) parse, but should be inserted into the 
   built tree. 
*) 

; TYPE BuildStackElemRefTyp = REF BuildStackElemTyp 
; TYPE BuildStackElemTyp 
    = RECORD 
        BseLink : BuildStackElemRefTyp 
      ; BseTokInfo : TokInfoTyp 
      ; BseWasInsertedByParser : BOOLEAN 
      ; BseWasDeletedByParser : BOOLEAN 
      END (* RECORD  BuildStackElemTyp *) 

(* The parse stack *) 

; TYPE ParseStackElemRefTyp = REF ParseStackElemTyp 
; TYPE ParseStackElemTyp 
    = RECORD 
        PseLink : ParseStackElemRefTyp 
      ; PseDeepestBuildStackElemRef : BuildStackElemRefTyp 
      ; PseLRState : LbeStd . LRStateTyp 
        (* ^LR state after scanning PseTok *) 
      ; PseTok : LbeStd . TokTyp 
      END (* RECORD  ParseStackElemTyp *) 

(* The DeferredInfo fields hold a rescanned token or mod which 
   belongs to the following parser delivery.  ParseTrav had 
   to scan through it in order to determine that the current 
   delivery has no more trailing mods. 
   Tok # LbeStd . Tok__Null means there is a deferred token,
   and IsInsertionRepair, IsInterior, and TempMarkRange apply to it. 
   ObjRef # LbeStd . ObjRefNil means there is a deferred 
     item to be accumulated and KindSet is its kind set 
     (otherwise KindSet is meaningless).  The item could 
     be a leading mod or the string of an Ast string token.
     Tok tells what.  *) 

; TYPE DeferredInfoTyp 
    = RECORD 
        ObjRef : LbeStd . EstRootTyp := NIL 
      ; KindSet : EstHs . EstChildKindSetTyp := EstHs . EstChildKindSetEmpty  
      ; FullTempMarkRange : TempMarkRangeTyp := TempMarkRangeNull 
      ; PatchTempMarkRange : TempMarkRangeTyp := TempMarkRangeNull 
      ; Tok : LbeStd . TokTyp := LbeStd . Tok__Null 
      ; SyntTokCt : LbeStd . LimitedTokCtTyp := 0 
      ; IsInsertionRepair : BOOLEAN := FALSE 
      ; IsInterior : BOOLEAN := FALSE 
      END 

; TYPE DeferredInfoRefTyp = REF DeferredInfoTyp 

; TYPE ParseInfoTyp 
    = RECORD 
        PiFile : Rd . T 
      ; PiScanIf : ScannerIf . ScanIfTyp 
      ; PiTempMarkListRef : TempMarkArrayRefTyp 
      ; PiString : Strings . StringTyp 
      ; PiAttemptedRepairCt : CARDINAL := 0 
      ; PiAttemptedRepairActionCt : CARDINAL := 0 
      ; PiGram : LRTable . GrammarTyp 
      ; PiDeferredInfoRef : DeferredInfoRefTyp := NIL 
      ; PiLineCtIncr : LbeStd . LineNoSignedTyp := 0 
        (* An approximation to the change in the number of lines,
           computed during reparsing. *) 
      ; PiSeqNo : PortTypes.Int32Typ 
      ; PiLineNo : PortTypes . Int32Typ 
      ; PiRegularTempMarkCt : LbeStd . MarkNoTyp 
        (* Count of temp marks that use EstRef values and child kind
           bits to locate them.  The remainder are implicitly on the
           end of image. *)  
      ; PiLang : LbeStd . LangTyp 
      ; PiParseKind : LbeStd . ParseKindTyp 
      ; PiEof : BOOLEAN 
      ; PiNlIsWaiting : BOOLEAN 
      ; PiInsertNilFixedChildren : BOOLEAN := FALSE 
      END (* RECORD ParseInfoTyp *) 

; END ParseHs 
. 
