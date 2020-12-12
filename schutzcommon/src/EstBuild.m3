
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE EstBuild 

(* Routines to build an Est node. *) 

; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LangUtil 
; FROM LangUtil IMPORT FsKindTyp 
; IMPORT LbeStd 
; IMPORT MessageCodes 
; IMPORT PortTypes 
; IMPORT SharedStrings 

; IMPORT Assertions 

; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 

; TYPE AFT = MessageCodes . T 

; TYPE MsNonleafRowTyp 
    = RECORD 
        MsNlrObjRef : EstHs . KTreeRefTyp 
      ; MsNlrKindSet : EstHs . EstChildKindSetTyp 
      ; MsNlrSliceEdgeInfoPair : EstHs . SliceEdgeInfoPairTyp 
      ; MsNlrEstChildCtLeftOfNl : LbeStd . EstChildNoTyp 
      ; MsNlrWidthInfo : EstHs . WidthInfoTyp 
      ; MsNlrShortWidthInfo : EstHs . WidthInfoTyp 
        (* MsNlrShortWidthInfo and MsNlrShortSyntTokCt hold the values
           as they were before the currently leftmost child was prepended.
           In case this child must be removed later, they can be used to 
           reset MsNlrWidthInfo and MsNlrSyntTokCt.  In cases where this
           information is unknown, MsNlrSyntTokCt = LbeStd.ParseCheckInfinity.
        *) 
      ; MsNlrSyntTokCt : LbeStd . LimitedTokCtTyp 
      ; MsNlrShortSyntTokCt : LbeStd . LimitedTokCtTyp 
      ; MsNlrElemCt : EstHs . ElemNoTyp 
      ; MsNlrElems : EstHs . FullNonleafArrayTyp 
      ; MsNlrShortInfoIsKnown : BOOLEAN := FALSE 
      END (* RECORD *) 

; TYPE MsNonleafRowRefTyp = REF MsNonleafRowTyp 

; TYPE MsLeafRowTyp 
    = RECORD 
        MsLrObjRef : EstHs . KTreeRefTyp 
      ; MsLrKindSet : EstHs . EstChildKindSetTyp 
      ; MsLrLeftTok : LbeStd . TokTyp 
      ; MsLrRightTok : LbeStd . TokTyp 
      (* ^MsLrLeftTok and MsLrRightTok are not stored in a KTree leaf 
          element.  They are only maintained to avoid repeated calls 
          to time-consuming EstUtil . ComputeWholeLeafSliceEdgeInfoPair, when 
          prepending to the next level up. *) 
      (* ^It will take some reworking of procedure interfaces, etc. to 
          exploit these fields.  This is an optimization, left undone 
          for now. *) 
      ; MsLrEstChildCtLeftOfNl : LbeStd . EstChildNoTyp 
      ; MsLrWidthInfo : EstHs . WidthInfoTyp 
      ; MsLrSyntTokCt : LbeStd . LimitedTokCtTyp 
      ; MsLrElemCt : EstHs . ElemNoTyp 
      ; MsLrElems : EstHs . FullLeafArrayTyp 
      END (* RECORD *) 

; REVEAL MergeStateTyp 
    = MergeStatePublicTyp
      BRANDED Brand 
      OBJECT 
        MsFsNodeRef : LangUtil . FsNodeRefTyp 
      ; MsLastEstRef : LbeStd . EstRootTyp 
      ; MsLastFromChildNo : LbeStd . EstChildNoTyp 
      ; MsLastToChildNo : LbeStd . EstChildNoTyp 
        (* ^MsLast... are the original form of the most recent slice or
           single child that was prepended. MergeState collects info on 
           the left edge of the new, being-built list of children, so the 
           contents of this slice are included, possibly with nodes 
           repacked, from the orginal. 
        *)  
      ; MsEstRefToInheritFrom : EstHs . EstRefTyp 
      (* ^KTreeSyntTokCt comes from this node, if nonNIL. 
          Otherwise, as computed for the newly-built node, when complete. 
      *) 
      ; MsHeight : BITS 16 FOR EstHs . KTreeHeightTyp 
      (* Subscripts on MsNonleafRows go from 2 (lowest in tree) up to MsHeight 
         (highest in tree).  Think of MsLeafRow as being of height 1. 
      *) 
      ; MsLeadingSyntTokCt : LbeStd . LimitedTokCtTyp 
      ; MsTrailingSyntTokCt : LbeStd . LimitedTokCtTyp 
      ; MsTrailingNeedsSep : BOOLEAN 
      ; MsLeftTok : LbeStd . TokTyp 
      ; MsLang : LbeStd . LangTyp 

      ; MsTrailingWidthInfo : EstHs . WidthInfoTyp 
        (* ^Width contributions of things with no explicit presence in the
            being-built Est and which are to the right of everything that
            does have an explicit presence.  Accumulated while nothing explicit
            has been seen yet.  Concatenated at the right when the Est is 
            being finished. *) 
      ; MsLeadingWidthInfo : EstHs . WidthInfoTyp
        (* ^Stored width contributions not yet included into any row. *)  
      ; MsHasLeadingNl : BOOLEAN
        (* ^There is a leading Nl (either conditional or unconditional) that 
           has not yet had its effect merged into a row. *)  
      ; MsWaitingRightmostKindSet : EstHs . EstChildKindSetTyp 
      ; MsLeafRow : MsLeafRowTyp 
      ; MsLink : MergeStateTyp 
        (* Used to keep a pool of these around for reuse, since they are very
           big and would otherwise get allocated/collected very often. *) 
      ; MsNonleafRowRefs 
        := ARRAY [ 2 .. EstHs . KTreeHeightMax ] OF MsNonleafRowRefTyp 
             { NIL , .. } 
(* TODO: Make this auto-expanding ^*) 
      END (* OBJECT  MergeStateTyp *) 

(* In both MsNonleafRowTyp and MsLeafRowTyp, either Ms*ObjRef = NIL
   and all other fields contain the information for a partially
   constructed node, (this is called "unpacked") or Ms*ObjRef points
   to a K-tree node which (so far) can be used unaltered.  (This is
   called "packed").  When the row is packed, Ms*KindSet is a cached
   copy of the kind set of this node, but all other fields are
   meaningless.  Furthermore, unless this is the highest level, the
   level above (which can be of either form) already contains a
   reference to this K-tree node.  When this row is unpacked, the node
   above does not contain any such pointer (it couldn't, since no
   actual K-tree node has been allocated yet for this level), but if
   the level above exists at all, then it is guaranteed to have space
   for the pointer when it is finally created.  Furthermore, the level
   above has had its pointer deleted, that pointed to the old node
   below (if there was one), from which the info at this level was
   unpacked.  When a row is unpacked, so are all higher rows too.  *)

; VAR FreePool : MergeStateTyp := NIL 
; VAR FreePoolCt : PortTypes . Int32Typ 
; VAR AllocatedCt : PortTypes . Int32Typ 

; PROCEDURE UneToParentKindSet 
    ( VAR Left : EstHs . EstChildKindSetTyp 
    ; Right : EstHs . EstChildKindSetTyp 
    ) 

  = BEGIN 
      Left := Left + ( Right * EstHs . EstChildKindSetCopyAcross ) 
    END UneToParentKindSet

; PROCEDURE LeftmostTokForLeafNode 
    ( Lang : LbeStd . LangTyp ; LeafRef : EstHs . KTreeRefTyp ) 
  : LbeStd . TokTyp 
  RAISES { AssertionFailure } 

  = VAR LEstMiscInfo : EstHs . EstMiscInfoTyp 

  ; BEGIN (* LeftmostTokForLeafNode *) 
      LEstMiscInfo 
        := EstUtil . EstMiscInfo 
             ( Lang 
             , LeafRef . LeafArrayRef ( ) ^ [ LeafRef . KTreeElemCt - 1 ] 
               . LeChildRef 
             ) 
    ; RETURN LEstMiscInfo . EmiLeftTok 
    END LeftmostTokForLeafNode 

(* VISIBLE: *) 
; PROCEDURE InitMergeState 
    ( MergeState : MergeStateTyp 
    ; Lang : LbeStd . LangTyp 
    ; EstTok : LbeStd . TokTyp 
    ; EstRefToInheritFrom : EstHs . EstRefTyp 
    ) 
  : MergeStateTyp
  (* Initialize the fields of MergeState and return it. *) 

  = BEGIN (* InitMergeState *) 
      MergeState . MsLang := Lang 
    ; MergeState . MsHeight := 0 
    ; EstHs . MakeWidthInfoNull ( MergeState . MsLeadingWidthInfo ) 
    ; MergeState . MsHasLeadingNl := FALSE 
    ; MergeState . MsLeadingSyntTokCt := 0 
    ; EstHs . MakeWidthInfoNull ( MergeState . MsTrailingWidthInfo ) 
    ; MergeState . MsTrailingSyntTokCt := 0 
    ; MergeState . MsTrailingNeedsSep := FALSE 
    ; MergeState . MsLeftTok := LbeStd . Tok__Null 
    ; MergeState . MsEstTok := EstTok 
    ; MergeState . MsFsNodeRef := LangUtil . FsRuleForTok ( Lang , EstTok ) 
    ; MergeState . MsLastEstRef := NIL (* Defensive. *) 
    ; MergeState . MsLastFromChildNo := LbeStd . EstChildNoNull (* Defensive. *)
    ; MergeState . MsLastToChildNo := LbeStd . EstChildNoNull (* Defensive. *) 
    ; MergeState . MsEstRefToInheritFrom := EstRefToInheritFrom 
    ; MergeState . MsWaitingRightmostKindSet := EstHs . EstChildKindSetEmpty 
(* CHECK: ^Check completeness *) 
    ; RETURN MergeState 
    END InitMergeState 

(* VISIBLE: *) 
; PROCEDURE NewMergeState 
    ( Lang : LbeStd . LangTyp 
    ; EstTok : LbeStd . TokTyp 
    ; EstRefToInheritFrom : EstHs . EstRefTyp 
    ) 
  : MergeStateTyp 
  (* Allocate, initialize, and return a MergeStateTyp object. *) 

  = VAR LResult : MergeStateTyp 

  ; BEGIN (* NweMerge *) 
      IF FreePool = NIL 
      THEN 
        LResult := NEW ( MergeStateTyp ) 
      ; INC ( AllocatedCt ) 
      ELSE 
        LResult := FreePool 
      ; FreePool := LResult . MsLink 
      ; DEC ( FreePoolCt ) 
      END (* IF *) 
    ; RETURN InitMergeState ( LResult , Lang , EstTok , EstRefToInheritFrom ) 
    END NewMergeState 

(* VISIBLE: *) 
; PROCEDURE PrependTokInfo 
    ( MergeState : MergeStateTyp ; Tok : LbeStd . TokTyp ) 
  RAISES { AssertionFailure } 
  (* Prepend token count and width for an insertion token. *) 

  = VAR LWidthInfo : EstHs . WidthInfoTyp 

  ; BEGIN (* PrependTokInfo *) 
      EstHs . MakeWidthInfoNull ( LWidthInfo ) 
    ; LWidthInfo . WiWidth 
        := SharedStrings . Length 
             ( LangUtil . DisplayStringForTok ( MergeState . MsLang , Tok ) )
    ; LWidthInfo . WiIsNull := LWidthInfo . WiWidth = 0   
    ; LWidthInfo . WiNlTrigger := LbeStd . LimitedCharNoInfinity 
    ; IF MergeState . MsHeight = 0 
      THEN (* Nothing explicit in the Est has been provided yet. *) 
        MergeState . MsTrailingWidthInfo 
          := EstUtil . WidthInfoCat 
               ( LWidthInfo 
               , LangUtil . NeedsSep 
                   ( MergeState . MsLang , Tok , MergeState . MsLeftTok ) 
               , MergeState . MsTrailingWidthInfo 
               ) 
      ; LbeStd . IncLimitedTokCt ( MergeState . MsTrailingSyntTokCt ) 
      ELSE 
        MergeState . MsLeadingWidthInfo 
          := EstUtil . WidthInfoCat 
               ( LWidthInfo 
               , LangUtil . NeedsSep 
                   ( MergeState . MsLang , Tok , MergeState . MsLeftTok ) 
               , MergeState . MsLeadingWidthInfo 
               ) 
      ; LbeStd . IncLimitedTokCt ( MergeState . MsLeadingSyntTokCt ) 
      END (* IF *) 
    ; MergeState . MsLeftTok := Tok 
    END PrependTokInfo 

(* VISIBLE: *) 
; PROCEDURE PrependNl 
    ( MergeState : MergeStateTyp ; IsConditional : BOOLEAN ) 
  (* Prepend a new line to the being-merged Est. *) 

  = BEGIN (* PrependNl *) 
      IF MergeState . MsHeight = 0 
      THEN (* Nothing explicit in the Est has been provided yet. *) 
        IF NOT IsConditional 
        THEN (* WidthInfo assumes horizontal, if that's possible. *) 
          MergeState . MsTrailingWidthInfo 
            := EstUtil . WidthInfoCat 
                 ( Left := EstHs . WidthInfoLineBreakReqd 
                 , NeedsSep := FALSE 
                 , Right := MergeState . MsTrailingWidthInfo 
                 ) 
        END (* IF *) 
      (* No need to remember trailing unconditional Nl, as it will have no 
         effect on *EstChildCtLeftOfNl, which is zero regardless. *) 
      ELSE 
        IF NOT IsConditional 
        THEN (* WidthInfo assumes horizontal, if that's possible. *) 
          MergeState . MsLeadingWidthInfo 
            := EstUtil . WidthInfoCat 
                 ( Left := EstHs . WidthInfoLineBreakReqd 
                 , NeedsSep := FALSE 
                 , Right := MergeState . MsLeadingWidthInfo 
                 ) 
        END (* IF *) 
      ; MergeState . MsHasLeadingNl := TRUE  
        (* ^But Leading Nl assumes vertical. *) 
      END (* IF *) 
    END PrependNl  

(* General Utility procedures *) 

; PROCEDURE PrependSliceTokPair 
    ( PrependLeftTok : LbeStd . TokTyp 
    ; PrependRightTok : LbeStd . TokTyp 
    ; VAR (* IN OUT *) ExistingLeftTok : LbeStd . TokTyp 
    ; VAR (* IN OUT *) ExistingRightTok : LbeStd . TokTyp 
    ) 
  RAISES { AssertionFailure } 

  = BEGIN (* PrependSliceTokPair *) 
      Assert 
        ( ( PrependLeftTok = LbeStd . Tok__Null ) 
          = ( PrependRightTok = LbeStd . Tok__Null ) 
        , AFT . A_PrependSliceTokPairBadPrependNullness 
        ) 
    ; Assert 
        ( ( ExistingLeftTok = LbeStd . Tok__Null ) 
          = ( ExistingRightTok = LbeStd . Tok__Null ) 
        , AFT . A_PrependSliceTokPairBadExistingNullness 
        ) 
    ; IF PrependLeftTok # LbeStd . Tok__Null 
      THEN 
        IF ExistingRightTok = LbeStd . Tok__Null 
        THEN 
          ExistingRightTok := PrependRightTok 
        END (* IF *) 
      ; ExistingLeftTok := PrependLeftTok 
      END (* IF *) 
    END PrependSliceTokPair 

; PROCEDURE ReincludeListSepTokInfo 
    ( MergeState : MergeStateTyp 
    ; LeftGapEdgeInfo : EstHs . EdgeInfoTyp 
    ; RightGapEdgeInfo : EstHs . EdgeInfoTyp 
      (* Here, "Gap" refers to the space within which are the insertion
         tokens whose width info needs to be reinserted.  These EdgeInfo
         variables are for the explicit Est children that bracket the gap
         on its left and right.
      *) 
    ; OrigEstRef : LbeStd . EstRootTyp  
    ; OrigFromChildNo : LbeStd . EstChildNoTyp 
    ; OrigToChildNo : LbeStd . EstChildNoTyp
      (* ^Child Nos in original Est of the LM and RM+1 children of the K-tree
         node whose list separator info we are reinserting. *) 
    ; RightGapOrigChildNo : LbeStd . EstChildNoTyp 
      (* ^Child No in original Est of the child to right of the gap. *)
    ) 
  RAISES { AssertionFailure } 
  (* Include token count and width for insertion tokens that are separators
     and conditional insertions between successive Est children of an Est
     list.  This is only used to reinsert info where a list has been sliced
     and is being respliced without parsing the separators.  If parsed, the
     separators are handled by PrependTokInfo.  Trailing separators are always
     parsed, so are irrelevant here. 
  *) 

  = VAR RlsRightTok : LbeStd . TokTyp 
  ; VAR RlsLeftAstChildCached : LbeStd . EstRootTyp 
  ; VAR RlsLeftAstChildKindSetCached : EstHs . EstChildKindSetTyp 
  ; VAR RlsLeftAstChildIsCached : BOOLEAN := FALSE 
  ; VAR RlsRightAstChildCached : LbeStd . EstRootTyp 
  ; VAR RlsRightAstChildKindSetCached : EstHs . EstChildKindSetTyp 
  ; VAR RlsRightAstChildIsCached : BOOLEAN := FALSE 

  (* These parent nodes are only filled when --FsNodeRef # MsFsNodeRef *) 

  ; PROCEDURE RlsLeftAstChild ( ) 
    RAISES { AssertionFailure } 
    (* Lazily find and cache the left-of-gap Ast child. *) 

    = VAR LEstChildNo : LbeStd . EstChildNoTyp 
    ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LLeafElem : EstHs . LeafElemTyp 

    ; BEGIN (* RlsLeftAstChild *) 
        IF NOT RlsLeftAstChildIsCached 
        THEN 
          EstUtil . PrevInKindSet 
            ( OrigEstRef 
            , RightGapOrigChildNo - 1 
            , EstHs . EstChildKindSetEstChild 
            , (* VAR *) LEstChildNo 
            , (* VAR *) LNodeNo 
            , (* VAR *) LLeafElem 
            ) 
        ; RlsLeftAstChildCached := LLeafElem . LeChildRef  
        ; RlsLeftAstChildKindSetCached := LLeafElem . LeKindSet   
        ; RlsLeftAstChildIsCached := TRUE  
        END (* IF *) 
      END RlsLeftAstChild 

  ; PROCEDURE RlsRightAstChild ( ) 
    RAISES { AssertionFailure } 
    (* Lazily find and cache the right-of-gap Ast child. *) 

    = VAR LEstChildNo : LbeStd . EstChildNoTyp 
    ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LLeafElem : EstHs . LeafElemTyp 

    ; BEGIN (* RlsRightAstChild *) 
        IF NOT RlsRightAstChildIsCached 
        THEN 
          EstUtil . NextInKindSet 
            ( OrigEstRef 
            , RightGapOrigChildNo 
            , EstHs . EstChildKindSetEstChild 
            , (* VAR *) LEstChildNo 
            , (* VAR *) LNodeNo 
            , (* VAR *) LLeafElem 
            ) 
        ; RlsRightAstChildCached := LLeafElem . LeChildRef  
        ; RlsRightAstChildKindSetCached := LLeafElem . LeKindSet   
        ; RlsRightAstChildIsCached := TRUE  
        END (* IF *) 
      END RlsRightAstChild 

  ; PROCEDURE RlsCountFsLeaf ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
    (* FsNodeRef is for a Fs leaf included in the range.  If it is an
       insertion token, count its width and token as a list separator. 
    *) 

    = VAR LTok : LbeStd . TokTyp 
    ; VAR LWidthInfo : EstHs . WidthInfoTyp 

    ; BEGIN (* RlsCountFsLeaf *) 
        CASE FsNodeRef . FsKind 
        OF FsKindTyp . FsKindInsTok 

        => LTok := FsNodeRef . FsTok 
          ; EstHs . MakeWidthInfoNull ( LWidthInfo ) 
          ; LWidthInfo . WiWidth 
              := SharedStrings . Length 
                   ( LangUtil . DisplayStringForTok 
                       ( MergeState . MsLang , LTok ) 
                   ) 
          ; LWidthInfo . WiIsNull := LWidthInfo . WiWidth = 0 
          ; LWidthInfo . WiNlTrigger := LbeStd . LimitedCharNoInfinity 
          ; IF MergeState . MsHeight = 1 
            THEN 
              MergeState . MsLeafRow . MsLrWidthInfo 
                := EstUtil . WidthInfoCat 
                     ( LWidthInfo 
                     , LangUtil . NeedsSep 
                         ( MergeState . MsLang , LTok , RlsRightTok ) 
                     , MergeState . MsLeafRow . MsLrWidthInfo 
                     ) 
            ; LbeStd . IncLimitedTokCt 
                ( MergeState . MsLeafRow . MsLrSyntTokCt ) 
            ELSE 
              WITH 
                WNonleafRow 
                = MergeState . MsNonleafRowRefs [ MergeState . MsHeight ] ^ 
              DO WNonleafRow . MsNlrWidthInfo 
                   := EstUtil . WidthInfoCat 
                        ( LWidthInfo 
                        , LangUtil . NeedsSep 
                            ( MergeState . MsLang , LTok , RlsRightTok ) 
                        , WNonleafRow . MsNlrWidthInfo 
                        ) 
              ; LbeStd . IncLimitedTokCt ( WNonleafRow . MsNlrSyntTokCt ) 
              END (* WITH *) 
            END (* IF *) 
          ; RlsRightTok := LTok 

        | FsKindTyp . FsKindLineBreakReqd 
        => EstHs . MakeWidthInfoInfinity ( LWidthInfo ) 
          ; LWidthInfo . WiHasNlBefore := TRUE 
          ; LWidthInfo . WiHasNlAfter := TRUE 
          ; IF MergeState . MsHeight = 1 
            THEN 
              MergeState . MsLeafRow . MsLrWidthInfo 
                := EstUtil . WidthInfoCat 
                     ( Left := LWidthInfo 
                     , NeedsSep := FALSE 
                     , Right := MergeState . MsLeafRow . MsLrWidthInfo 
                     ) 
            ELSE 
              WITH 
                WNonleafRow 
                = MergeState . MsNonleafRowRefs [ MergeState . MsHeight ] ^ 
              DO WNonleafRow . MsNlrWidthInfo 
                   := EstUtil . WidthInfoCat 
                        ( Left := LWidthInfo 
                        , NeedsSep := FALSE 
                        , Right := WNonleafRow . MsNlrWidthInfo 
                        ) 
              END (* WITH *) 
            END (* IF *) 
        ELSE 
        END (* CASE *) 
      END RlsCountFsLeaf 

  ; PROCEDURE RlsTraverse 
      ( LeftFmtNo : EstHs . FmtNoTyp 
      ; RightFmtNo : EstHs . FmtNoTyp 
      ; DoSkipLeftmost : BOOLEAN 
      ; DoSkipRightmost : BOOLEAN 
      ) 
    RAISES { AssertionFailure } 
    (* Traverse Leftward, from RightFmtNo to LeftFmtNo, inserting list
       separators' width and token counts.  DoSkipRightmost means skip 
       RightFmtNo and start with the next one to its left.
    *)  

    = TYPE RlsTStateTyp = { Descend , Traverse , Done } 

    ; VAR RlsTState : RlsTStateTyp := RlsTStateTyp . Descend 

    ; PROCEDURE RlsTRecurse 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR FmtNo : EstHs . FmtNoTyp 
          (* Moves R to L while traversing. *)
(* TODO:  ^Probably this should be a non-local variable instead of a formal. *)
        ) 
      RAISES { AssertionFailure } 

      = VAR LFsChildNo : LangUtil . FsChildNoTyp 
      ; VAR LFsNodeRef : LangUtil . FsNodeRefTyp 

      ; BEGIN (* RlsTRecurse *) 
          LFsNodeRef := FsNodeRef 
        ; CASE LFsNodeRef . FsKind <* NOWARN *> 

          (* Fs tree leaf nodes. *)
          OF FsKindTyp . FsKindInsTok 
          , FsKindTyp . FsKindEstChildOfList 
          , FsKindTyp . FsKindLineBreakOpt
          , FsKindTyp . FsKindLineBreakReqd 
          =>  CASE RlsTState <* NOWARN *> 
              OF RlsTStateTyp . Descend 
              => Assert 
                   ( FmtNo = LFsNodeRef . FsFmtNo 
                   , AFT . A_RlsTRecurse_DescendedToWrongFmtNumber
                   ) 
              ; IF DoSkipRightmost 
                THEN 
                  RlsTState := RlsTStateTyp . Traverse   
                ELSIF FmtNo = LeftFmtNo 
                THEN (* This is both the beginning and ending Fs node. *) 
                  IF NOT DoSkipLeftmost 
                  THEN RlsCountFsLeaf ( LFsNodeRef ) 
                  END (* IF *) 
                ; RlsTState := RlsTStateTyp . Done 
                ELSE 
                  RlsCountFsLeaf ( LFsNodeRef ) 
                ; RlsTState := RlsTStateTyp . Traverse 
                END (* IF *) 
              | RlsTStateTyp . Traverse 
              =>  IF FmtNo # LeftFmtNo OR NOT DoSkipLeftmost 
                  THEN RlsCountFsLeaf ( LFsNodeRef ) 
                  END (* IF *) 
                ; IF FmtNo = LeftFmtNo 
                  THEN
                    RlsTState := RlsTStateTyp . Done 
                  END (* IF *) 
              END (* CASE *) 

          (* Fs tree nodes with children. *) 
          | FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill 
          , FsKindTyp . FsKindEstListVert 
          , FsKindTyp . FsKindEstListHoriz 
          , FsKindTyp . FsKindEstListFill 
          , FsKindTyp . FsKindEstListTrailVert 
          , FsKindTyp . FsKindEstListTrailHoriz 
          , FsKindTyp . FsKindEstListTrailFill 
          , FsKindTyp . FsKindCondFmt 
          =>  (* Nodes with children. *) 
              CASE RlsTState <* NOWARN *> 
              OF RlsTStateTyp . Descend 
              =>  IF FmtNo = EstHs . FmtNoListEstChild 
                     AND LFsNodeRef . FsKind 
                         = FsKindTyp . FsKindCondFmt
                  THEN (* Must evaluate predicates. *) 
                    LOOP  
                      RlsRightAstChild ( ) 
                    ; IF EstUtil . EvalPredicate 
                           ( MergeState . MsLang 
                           , LFsNodeRef 
                           , RlsRightAstChildCached  
                           , RlsRightAstChildKindSetCached  
                           )
                      THEN (* This is the correct alternative. *) 
                        LFsChildNo := LFsNodeRef . FsLeadingChildCt 
                      ; EXIT 
                      ELSE (* Look for another alternative. *) 
                        LFsNodeRef := LFsNodeRef . FsCondAltRef 
                      ; IF LFsNodeRef . FsKind = FsKindTyp . FsKindCondFmt 
                        THEN (* Loop to test this new predicate. *) 
                        ELSE (* Unconditional. Recurse just to CASE on it. *)
                          RlsTRecurse ( LFsNodeRef , (* IN OUT *) FmtNo )  
                        ; RETURN 
                        END (* IF *) 
                      END (* IF *) 
                    END (* LOOP *) 
                  ELSE (* FmtNo suffices to select correct alternative. *) 
                    LOOP 
                      LFsChildNo 
                        := LangUtil . FsChildNoOfFmtNo ( LFsNodeRef , FmtNo ) 
                    ; IF LFsChildNo # LangUtil . FsChildNoAlt 
                      THEN (* This is the correct alternative. *) 
                        EXIT 
                      ELSE (* Try another alternative. *) 
                        LFsNodeRef := LFsNodeRef . FsCondAltRef 
                      ; IF LFsNodeRef . FsKind 
                           = FsKindTyp . FsKindCondFmt 
                        THEN (* Loop to test this new predicate. *) 
                        ELSE (* Unconditional. Recurse just to CASE on it. *)
                          RlsTRecurse ( LFsNodeRef , (* IN OUT *) FmtNo )  
                        ; RETURN 
                        END (* IF *) 
                      END (* IF *) 
                    END (* LOOP *) 
                  END (* IF *) 
                ; RlsTRecurse 
                    ( LFsNodeRef . FsChildren ^ [ LFsChildNo ] 
                    , (* IN OUT *) FmtNo 
                    ) 
              | RlsTStateTyp . Traverse 
              =>  IF FmtNo = EstHs . FmtNoListEstChild 
                     AND LFsNodeRef . FsKind 
                         = FsKindTyp . FsKindCondFmt
                  THEN (* Must evaluate predicates. *) 
                    LOOP  
                      RlsLeftAstChild ( ) 
                    ; IF EstUtil . EvalPredicate 
                           ( MergeState . MsLang 
                           , LFsNodeRef 
                           , RlsLeftAstChildCached  
                           , RlsLeftAstChildKindSetCached  
                             (* We are moving leftward. *)  
                           )
                      THEN (* This is the right alternative. *) 
                        EXIT 
                      ELSE (* Look for another alternative. *)
                        LFsNodeRef := LFsNodeRef . FsCondAltRef 
                      ; IF LFsNodeRef . FsKind 
                           = FsKindTyp . FsKindCondFmt 
                        THEN (* Loop to test this new predicate. *)
                        ELSE (* Unconditional. Recurse just to CASE on it. *)
                          RlsTRecurse ( LFsNodeRef , (* IN OUT *) FmtNo )  
                        ; RETURN 
                        END (* IF *) 
                      END (* IF *) 
                    END (* LOOP *) 
                  ELSE (* FmtNo suffices to select correct alternative. *) 
                    LOOP 
                      IF LangUtil . FsChildNoOfFmtNo ( LFsNodeRef , FmtNo ) 
                         # LangUtil . FsChildNoAlt 
                      THEN (* This is the right alternative. *) 
                        EXIT 
                      ELSE (* Look for another alternative. *) 
                        LFsNodeRef := LFsNodeRef . FsCondAltRef 
                      ; IF LFsNodeRef . FsKind 
                           = FsKindTyp . FsKindCondFmt 
                        THEN (* Loop to test this new alternative. *) 
                        ELSE (* Unconditional. Recurse just to CASE on it. *)
                          RlsTRecurse ( LFsNodeRef , (* IN OUT *) FmtNo )  
                        ; RETURN 
                        END (* IF *) 
                      END (* IF *) 
                    END (* LOOP *) 
                  END (* IF*) 
                ; LFsChildNo := NUMBER ( LFsNodeRef . FsChildren ^ ) - 1 
                ; FmtNo := LFsNodeRef . FsChildren ^ [ LFsChildNo ] . FsFmtNo
                ; RlsTRecurse 
                    ( LFsNodeRef . FsChildren ^ [ LFsChildNo ] 
                    , (* IN OUT *) FmtNo 
                    ) 
              END (* CASE *) 

            (* A recursive call has returned.  *) 

            ; CASE LFsNodeRef . FsKind  
              OF FsKindTyp . FsKindEstListVert 
              , FsKindTyp . FsKindEstListHoriz 
              , FsKindTyp . FsKindEstListFill 
              , FsKindTyp . FsKindEstListTrailVert 
              , FsKindTyp . FsKindEstListTrailHoriz 
              , FsKindTyp . FsKindEstListTrailFill 
              =>  WHILE RlsTState # RlsTStateTyp . Done 
                  DO LFsChildNo 
                       := ( LFsChildNo - 1 ) 
                          MOD NUMBER ( LFsNodeRef . FsChildren ^ ) 
                  ; FmtNo := LFsNodeRef . FsChildren ^ [ LFsChildNo ] . FsFmtNo
                  ; RlsTRecurse 
                      ( LFsNodeRef . FsChildren ^ [ LFsChildNo ] 
                      , (* IN OUT *) FmtNo 
                      ) 
                  END (* WHILE *) 
              ELSE 
                WHILE RlsTState # RlsTStateTyp . Done 
                      AND LFsChildNo > 0 
                DO 
                  DEC ( LFsChildNo ) 
                ; FmtNo := LFsNodeRef . FsChildren ^ [ LFsChildNo ] . FsFmtNo
                ; RlsTRecurse 
                    ( LFsNodeRef . FsChildren ^ [ LFsChildNo ] 
                    , (* IN OUT *) FmtNo 
                    ) 
                END (* WHILE *) 
              END (* CASE *) 
          END (* CASE LFsNodeRef . FsKind *) 
        END RlsTRecurse

    ; BEGIN (* RlsTraverse *) 
        RlsTState := RlsTStateTyp . Descend 
      ; RlsTRecurse ( MergeState . MsFsNodeRef , (* IN OUT *) RightFmtNo ) 
      END RlsTraverse  

  ; BEGIN (* ReincludeListSepTokInfo *) 
      VAR LLeftFmtNo : EstHs . FmtNoTyp 
    ; VAR LRightFmtNo : EstHs . FmtNoTyp 
    ; VAR LEstChildNo : LbeStd . EstChildNoTyp 
    ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LLeafElem : EstHs . LeafElemTyp 
    ; VAR LDoSkipLeftmost : BOOLEAN := FALSE 
    ; VAR LDoSkipRightmost : BOOLEAN := FALSE 

    ; BEGIN (* Block ReincludeListSepTokInfo *) 
        IF MergeState . MsFsNodeRef # NIL 
           (* ^NIL happens during Ldl Bootstrap. *) 
           AND MergeState . MsFsNodeRef . FsKind 
               IN LangUtil . FsKindSetEstList 
               (* ^Only do this for a list node. *) 
           AND OrigFromChildNo # LbeStd . EstChildNoNull 
               (* ^This also ensures OrigEstRef can be narrowed to
                  EstHs . EstRefTyp. *) 
           AND OrigFromChildNo < RightGapOrigChildNo 
           AND RightGapOrigChildNo < OrigToChildNo  
               (* ^Gap is properly inside the K-tree node, on both ends. *) 
        THEN 
          LLeftFmtNo := LeftGapEdgeInfo . EiFmtNo 
        ; LRightFmtNo := RightGapEdgeInfo . EiFmtNo 

        (* Initialize left side Fs info, and maybe the right side too, if it
           is easy. *) 
        ; IF LLeftFmtNo = EstHs . FmtNoNull 
          THEN (* There are a number of cases where we can determine the 
                  FmtNos, or that we don't need to do anything, without 
                  actually finding the FmtNo by searching for a 
                  FirstOfGroup child. *) 
            IF LeftGapEdgeInfo . EiEdgeKind 
               = EstHs . EdgeKindTyp . EdgeKindEstChild 
            THEN (* It can only be FmtNoListEstChild. *) 
              LLeftFmtNo := EstHs . FmtNoListEstChild 
            ELSIF LRightFmtNo = EstHs . FmtNoNull 
            THEN (* Both FmtNos are unknown, which means the real values, 
                    whatever they are, are equal. *) 
              IF RightGapEdgeInfo . EiEdgeKind 
                 = EstHs . EdgeKindTyp . EdgeKindEstChild 
                 OR ( LeftGapEdgeInfo . EiEdgeKind 
                      = EstHs . EdgeKindTyp . EdgeKindTrailingMod 
                      AND RightGapEdgeInfo . EiEdgeKind 
                          = EstHs . EdgeKindTyp . EdgeKindLeadingMod 
                    ) 
              THEN (* They can only be FmtNoListEstChild. *)  
                LLeftFmtNo := EstHs . FmtNoListEstChild 
              ; LRightFmtNo := EstHs . FmtNoListEstChild 
              ELSIF LeftGapEdgeInfo . EiEdgeKind 
                    = EstHs . EdgeKindTyp . EdgeKindModDel 
                    OR LeftGapEdgeInfo . EiEdgeKind 
                       = RightGapEdgeInfo . EiEdgeKind 
              THEN (* No separators to insert. *) 
                RETURN 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        ; IF LLeftFmtNo = EstHs . FmtNoNull 
          THEN (* All else failed.  Must search for FirstOfGroup. *) 
            EstUtil . PrevInKindSet 
              ( OrigEstRef 
              , RightGapOrigChildNo - 1 
              , EstHs . EstChildKindSetFirstOfGroup 
              , (* VAR *) LEstChildNo 
              , (* VAR *) LNodeNo 
              , (* VAR *) LLeafElem 
              ) 
          ; LLeftFmtNo := LLeafElem . LeFmtNo 
          END (* IF *) 

        (* Initialize right side Fs info. *) 
        ; IF LRightFmtNo = EstHs . FmtNoNull 
          THEN 
            LRightFmtNo := LLeftFmtNo 
          END (* IF *) 

        (* Arrange to skip boundary format numbers that do not enclose 
           separators.  Fs traversal will always visit them, but may not 
           count their width.  
        *) 
        ; IF LeftGapEdgeInfo . EiEdgeKind 
             = EstHs . EdgeKindTyp . EdgeKindLeadingMod 
          THEN (* LLeftFmtNo needs to be included. *) 
            IF RightGapEdgeInfo . EiEdgeKind 
               = EstHs . EdgeKindTyp . EdgeKindTrailingMod 
            THEN (* LLeftFmtNo and LRightFmtNo both need to be included. *)  
            ELSIF LLeftFmtNo = LRightFmtNo 
            THEN (* No separators at all *) 
              RETURN 
            ELSE 
              LDoSkipRightmost := TRUE 
            END (* IF *) 
          ELSE (* Must exclude LLeftFmtNo. *) 
            IF RightGapEdgeInfo . EiEdgeKind 
               = EstHs . EdgeKindTyp . EdgeKindTrailingMod 
            THEN (* Exclude LLeftFmtNo only. *) 
              IF LLeftFmtNo = LRightFmtNo 
              THEN (* No separators at all *) 
                RETURN 
              ELSE 
                LDoSkipLeftmost := TRUE 
              END (* IF *) 
            ELSE (* Exclude both LLeftFmtNo and LRightFmtNo. *) 
                 (* In the special case where left and right are both Est 
                    children, they have the same format number but are
                    different list children.  Skipping both will traverse
                    through all separators. 
                 *) 
              LDoSkipLeftmost := TRUE 
            ; LDoSkipRightmost := TRUE 
            END (* IF *) 
          END (* IF *) 

        (* Finally, traverse relevant Fs children, adding separator info. *) 
        ; RlsRightTok := RightGapEdgeInfo . EiTok 
        ; RlsTraverse 
            ( LLeftFmtNo 
            , LRightFmtNo 
            , DoSkipLeftmost := LDoSkipLeftmost 
            , DoSkipRightmost := LDoSkipRightmost 
            ) 
        END (* IF *) 
      END (* Block *) 
    END ReincludeListSepTokInfo 

(* Leaf row utilities. *) 

; PROCEDURE InitLeafRow ( VAR LeafRow : MsLeafRowTyp ) 

  = BEGIN (* InitLeafRow *) 
      LeafRow . MsLrObjRef := NIL 
    ; LeafRow . MsLrKindSet := EstHs . EstChildKindSetEmpty 
    ; LeafRow . MsLrLeftTok := LbeStd . Tok__Null 
    ; LeafRow . MsLrRightTok := LbeStd . Tok__Null 
    ; EstHs . MakeWidthInfoNull ( LeafRow . MsLrWidthInfo ) 
    ; LeafRow . MsLrSyntTokCt := 0 
    ; LeafRow . MsLrElemCt := 0 
    END InitLeafRow 

; PROCEDURE FlushLeadingTokInfoToLeaf ( MergeState : MergeStateTyp ) 
  RAISES { AssertionFailure } 

  = BEGIN (* FlushLeadingTokInfoToLeaf *) 
      Assert 
        ( MergeState . MsLeafRow . MsLrObjRef = NIL 
        , AFT . A_FlushLeadingTokInfoToLeafPackedRow 
        ) 
    ; MergeState . MsLeafRow . MsLrWidthInfo 
        := EstUtil . WidthInfoCat 
             ( Left := MergeState . MsLeadingWidthInfo 
             , NeedsSep := FALSE 
               (* Separator was already taken into account in PrependTokInfo. *)
             , Right := MergeState . MsLeafRow . MsLrWidthInfo 
             ) 
    ; EstHs . MakeWidthInfoNull ( MergeState . MsLeadingWidthInfo ) 
    ; LbeStd . IncLimitedTokCt 
        ( MergeState . MsLeafRow . MsLrSyntTokCt 
        , MergeState . MsLeadingSyntTokCt 
        ) 
    ; MergeState . MsLeadingSyntTokCt := 0 
    ; IF MergeState . MsHasLeadingNl 
      THEN 
        MergeState . MsLeafRow . MsLrEstChildCtLeftOfNl := 0 
      ; MergeState . MsHasLeadingNl := FALSE 
      END (* IF *) 
    END FlushLeadingTokInfoToLeaf 

; PROCEDURE PrependToLeafRow 
    ( MergeState : MergeStateTyp 
    ; InsertionRef : LbeStd . EstRootTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; FmtNo : EstHs . FmtNoTyp 
    ; IsLeftOfSeam : BOOLEAN 
    ) 
  RAISES { AssertionFailure } 
  (* PRE: MergeState . MsLeafRow is unpacked and has space for at least one
          additional child. 
  *) 

  = VAR LEstMiscInfo : EstHs . EstMiscInfoTyp 
  ; VAR LCumNodeCt : LbeStd . EstNodeNoTyp 

  ; BEGIN (* PrependToLeafRow *) 
      Assert 
        ( MergeState . MsLeafRow . MsLrElemCt < EstHs . LeafArrayElemCtMax 
        , AFT . A_PrependToLeafRowFullArray 
        ) 
    ; IF MergeState . MsLeafRow . MsLrElemCt = 0 
      THEN 
        LCumNodeCt := 0 
      ELSE 
        LCumNodeCt 
          := MergeState . MsLeafRow . MsLrElems 
               [ MergeState . MsLeafRow . MsLrElemCt - 1 ] 
             . LeCumNodeCt 
      ; IF IsLeftOfSeam 
        THEN 
          FlushLeadingTokInfoToLeaf ( MergeState ) 
        END (* IF *) 
      END (* IF *) 
    ; WITH 
        WLeafElem 
        = MergeState . MsLeafRow . MsLrElems 
            [ MergeState . MsLeafRow . MsLrElemCt ] 
      DO WLeafElem . LeCumNodeCt 
           := LCumNodeCt 
              + EstUtil . EstNodeCt ( InsertionRef ) 
              + ORD ( EstHs . EstChildKindOptSingletonList IN KindSet ) 
      ; WLeafElem . LeChildRef := InsertionRef 
      ; WLeafElem . LeKindSet := KindSet 
      ; UneToParentKindSet ( MergeState . MsLeafRow . MsLrKindSet , KindSet )  
      ; WLeafElem . LeFmtNo := FmtNo 
      END (* WITH WLeafElem *) 
    ; IF InsertionRef # NIL 
      THEN 
        LEstMiscInfo 
          := EstUtil . EstMiscInfo ( MergeState . MsLang , InsertionRef ) 
      ; IF MergeState . MsLeafRow . MsLrElemCt = 0 
        THEN 
          MergeState . MsLeafRow . MsLrWidthInfo 
            := LEstMiscInfo . EmiWidthInfo 
        ; IF LEstMiscInfo . EmiWidthInfo . WiHasNlBefore 
          THEN MergeState . MsLeafRow . MsLrEstChildCtLeftOfNl := 0 
          ELSE MergeState . MsLeafRow . MsLrEstChildCtLeftOfNl := 1
          END (* IF *) 
        ELSE 
          MergeState . MsLeafRow . MsLrWidthInfo 
            := EstUtil . WidthInfoCat 
                 ( Left := LEstMiscInfo . EmiWidthInfo 
                 , NeedsSep 
                     := LangUtil . NeedsSep 
                          ( MergeState . MsLang 
                          , LEstMiscInfo . EmiRightTok 
                          , MergeState . MsLeftTok 
                          ) 
                 , Right := MergeState . MsLeafRow . MsLrWidthInfo 
                 ) 
        ; IF LEstMiscInfo . EmiWidthInfo . WiHasNlBefore 
          THEN MergeState . MsLeafRow . MsLrEstChildCtLeftOfNl := 0 
          ELSIF LEstMiscInfo . EmiWidthInfo . WiHasNlWithin  
                OR LEstMiscInfo . EmiWidthInfo . WiHasNlAfter 
          THEN MergeState . MsLeafRow . MsLrEstChildCtLeftOfNl := 1 
          ELSE INC ( MergeState . MsLeafRow . MsLrEstChildCtLeftOfNl ) 
               (* This is an Est child.  Its contribution is one. *) 
          END (* IF *) 
        END (* IF *) 
      ; LbeStd . IncLimitedTokCt 
          ( MergeState . MsLeafRow . MsLrSyntTokCt 
          , LEstMiscInfo . EmiSyntTokCt 
          ) 
      ; PrependSliceTokPair 
          ( LEstMiscInfo . EmiLeftTok 
          , LEstMiscInfo . EmiRightTok 
          , (* VAR *) MergeState . MsLeafRow . MsLrLeftTok 
          , (* VAR *) MergeState . MsLeafRow . MsLrRightTok 
          ) 
      ; MergeState . MsLeftTok := LEstMiscInfo . EmiLeftTok 
      END (* IF *) 
    ; INC ( MergeState . MsLeafRow . MsLrElemCt ) 
    END PrependToLeafRow 

; PROCEDURE UnpackLeafRow ( MergeState : MergeStateTyp ) 

  = BEGIN (* UnpackLeafRow *) 
      MergeState . MsLeafRow . MsLrWidthInfo 
        := MergeState . MsLeafRow . MsLrObjRef . KTreeWidthInfo 
    ; MergeState . MsLeafRow . MsLrSyntTokCt 
        := MergeState . MsLeafRow . MsLrObjRef . KTreeSyntTokCt 
    ; MergeState . MsLeafRow . MsLrElemCt 
        := MergeState . MsLeafRow . MsLrObjRef . KTreeElemCt 
    ; MergeState . MsLeafRow . MsLrEstChildCtLeftOfNl 
        := MergeState . MsLeafRow . MsLrObjRef . KTreeEstChildCtLeftOfNl 
    ; MergeState . MsLeafRow . MsLrObjRef . FetchLeafArray 
        ( MergeState . MsLeafRow . MsLrElems ) 
    ; MergeState . MsLeafRow . MsLrObjRef := NIL 
    (* MsLrKindSet is already set. *) 
    END UnpackLeafRow 

; PROCEDURE FlushLeafRow 
    ( MergeState : MergeStateTyp 
    ; VAR ResultRef : EstHs . KTreeRefTyp 
    ; VAR ResultKindSet : EstHs . EstChildKindSetTyp 
    ) 
  RAISES { AssertionFailure } 

  = BEGIN (* FlushLeafRow *) 
      Assert 
        ( MergeState . MsLeafRow . MsLrObjRef = NIL 
        , AFT . A_FlushLeafRowWithObjRef 
        ) 
    ; IF MergeState . MsLeafRow . MsLrElemCt > 0 
      THEN 
        ResultRef 
          := NEW 
               ( EstHs . KTreeLeafRefTyp 
               , KTreeWidthInfo := MergeState . MsLeafRow . MsLrWidthInfo 
               , KTreeSyntTokCt := MergeState . MsLeafRow . MsLrSyntTokCt 
               , KTreeElemCt := MergeState . MsLeafRow . MsLrElemCt 
               , KTreeEstChildCtLeftOfNl 
                   := MergeState . MsLeafRow . MsLrEstChildCtLeftOfNl 
               , KTreeLeafArrayRef 
                   := EstHs . RefToNewLeafArray 
                        ( MergeState . MsLeafRow . MsLrElemCt 
                        , MergeState . MsLeafRow . MsLrElems 
                        ) 
               ) 
      ; ResultKindSet := MergeState . MsLeafRow . MsLrKindSet 
      ; InitLeafRow ( MergeState . MsLeafRow ) 
      (* At this point, it is possible that MsLeafRow will next be reused on 
         the other side of the slice, with unrelated contents. *) 
      ELSE 
        ResultRef := NIL 
      ; ResultKindSet := EstHs . EstChildKindSetEmpty 
      END (* IF *) 
    END FlushLeafRow 

(* Nonleaf row utilities. *) 

; PROCEDURE InitNonleafRow ( VAR NonleafRow : MsNonleafRowTyp ) 

  = BEGIN (* InitNonleafRow *) 
      NonleafRow . MsNlrObjRef := NIL 
    ; NonleafRow . MsNlrKindSet := EstHs . EstChildKindSetEmpty 
    ; NonleafRow . MsNlrSliceEdgeInfoPair . SeiLeftEdgeInfo 
        := EstHs . EdgeInfoNull 
    ; NonleafRow . MsNlrSliceEdgeInfoPair . SeiRightEdgeInfo 
        := EstHs . EdgeInfoNull 
    ; EstHs . MakeWidthInfoNull ( NonleafRow . MsNlrWidthInfo ) 
    ; EstHs . MakeWidthInfoNull ( NonleafRow . MsNlrShortWidthInfo ) 
    ; NonleafRow . MsNlrSyntTokCt := 0 
    ; NonleafRow . MsNlrShortSyntTokCt := 0 
    ; NonleafRow . MsNlrShortInfoIsKnown := TRUE 
    ; NonleafRow . MsNlrElemCt := 0 
    END InitNonleafRow 

; PROCEDURE FlushLeadingTokInfoToNonleaf 
    ( MergeState : MergeStateTyp ; VAR NonleafRow : MsNonleafRowTyp ) 
  RAISES { AssertionFailure } 

  = BEGIN (* FlushLeadingTokInfoToNonleaf *) 
      Assert 
        ( NonleafRow . MsNlrObjRef = NIL 
        , AFT . A_FlushLeadingTokInfoToNonleafPackedRow 
        ) 
    ; NonleafRow . MsNlrWidthInfo 
        := EstUtil . WidthInfoCat 
             ( Left := MergeState . MsLeadingWidthInfo 
             , NeedsSep := FALSE 
               (* Separator was already taken into account in PrependTokInfo. *)
             , Right := NonleafRow . MsNlrWidthInfo 
             ) 
    ; EstHs . MakeWidthInfoNull ( MergeState . MsLeadingWidthInfo ) 
    ; LbeStd . IncLimitedTokCt 
        ( NonleafRow . MsNlrSyntTokCt , MergeState . MsLeadingSyntTokCt ) 
    ; MergeState . MsLeadingSyntTokCt := 0 
    ; IF MergeState . MsHasLeadingNl 
      THEN 
        NonleafRow . MsNlrEstChildCtLeftOfNl := 0 
      ; MergeState . MsHasLeadingNl := FALSE 
      END (* IF *) 
    END FlushLeadingTokInfoToNonleaf 

; PROCEDURE PrependToNonleafRow 
    ( MergeState : MergeStateTyp 
    ; VAR NonleafRow : MsNonleafRowTyp 
    ; InsertionRef : EstHs . KTreeRefTyp 
    ; READONLY SliceEdgeInfoPair : EstHs . SliceEdgeInfoPairTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; IsLeftOfSeam : BOOLEAN 
    ) 
  RAISES { AssertionFailure } 
  (* PRE: NonleafRow is unpacked and has space for at least one more child. *) 

  = VAR LCumNodeCt : LbeStd . EstNodeNoTyp 
  ; VAR LCumChildCt : LbeStd . EstChildNoTyp 

  ; BEGIN (* PrependToNonleafRow *) 
      Assert 
        ( NonleafRow . MsNlrElemCt < EstHs . NonleafArrayElemCtMax 
        , AFT . A_PrependToNonleafRow_Full_array 
        ) 
    ; IF NonleafRow . MsNlrElemCt = 0 
      THEN 
        LCumNodeCt := 0 
      ; LCumChildCt := 0 
      ; NonleafRow . MsNlrSliceEdgeInfoPair := SliceEdgeInfoPair 
      ELSE 
        WITH WPrevNonleafElem 
             = NonleafRow . MsNlrElems [ NonleafRow . MsNlrElemCt - 1 ] 
        DO 
          LCumNodeCt := WPrevNonleafElem . NleCumNodeCt 
        ; LCumChildCt := WPrevNonleafElem . NleCumChildCt 
        END (* WITH *) 
      ; NonleafRow . MsNlrSliceEdgeInfoPair . SeiLeftEdgeInfo . EiFmtNo 
          := SliceEdgeInfoPair . SeiLeftEdgeInfo . EiFmtNo 
      ; NonleafRow . MsNlrSliceEdgeInfoPair . SeiLeftEdgeInfo . EiEdgeKind 
          := SliceEdgeInfoPair . SeiLeftEdgeInfo . EiEdgeKind 
      ; PrependSliceTokPair 
          ( SliceEdgeInfoPair . SeiLeftEdgeInfo . EiTok 
          , SliceEdgeInfoPair . SeiRightEdgeInfo . EiTok 
          , (* VAR *) NonleafRow . MsNlrSliceEdgeInfoPair . SeiLeftEdgeInfo 
                      . EiTok 
          , (* VAR *) NonleafRow . MsNlrSliceEdgeInfoPair . SeiRightEdgeInfo 
                      . EiTok 
          ) 
      ; IF IsLeftOfSeam 
        THEN 
          FlushLeadingTokInfoToNonleaf ( MergeState , NonleafRow ) 
        END (* IF *) 
      END (* IF *) 
    ; WITH 
        WNonleafElem = NonleafRow . MsNlrElems [ NonleafRow . MsNlrElemCt ] 
      DO WNonleafElem . NleCumChildCt 
           := LCumChildCt + InsertionRef . KTreeChildCt ( ) 
      ; WNonleafElem . NleCumNodeCt 
          := LCumNodeCt 
             + InsertionRef . KTreeNodeCt ( ) 
             + ORD ( EstHs . EstChildKindOptSingletonList IN KindSet ) 
      ; WNonleafElem . NleChildRef := InsertionRef 
      ; WNonleafElem . NleKindSet := KindSet 
      ; UneToParentKindSet ( (* IN OUT *) NonleafRow . MsNlrKindSet , KindSet ) 
      END (* WITH WNonleafElem *) 
    ; NonleafRow . MsNlrShortWidthInfo := NonleafRow . MsNlrWidthInfo 
    ; NonleafRow . MsNlrShortSyntTokCt := NonleafRow . MsNlrSyntTokCt  
    ; NonleafRow . MsNlrShortInfoIsKnown := TRUE 
    ; IF NonleafRow . MsNlrElemCt = 0 
      THEN 
        NonleafRow . MsNlrWidthInfo := InsertionRef . KTreeWidthInfo 
      ; NonleafRow . MsNlrSyntTokCt := InsertionRef . KTreeSyntTokCt 
      ; NonleafRow . MsNlrEstChildCtLeftOfNl 
          := InsertionRef . KTreeEstChildCtLeftOfNl
      ELSE 
        NonleafRow . MsNlrWidthInfo 
          := EstUtil . WidthInfoCat 
               ( InsertionRef . KTreeWidthInfo 
               , LangUtil . NeedsSep 
                   ( MergeState . MsLang 
                   , SliceEdgeInfoPair . SeiRightEdgeInfo . EiTok 
                   , NonleafRow . MsNlrSliceEdgeInfoPair . SeiLeftEdgeInfo 
                     . EiTok 
                   ) 
               , NonleafRow . MsNlrWidthInfo 
               ) 
      ; LbeStd . IncLimitedTokCt 
          ( NonleafRow . MsNlrSyntTokCt , InsertionRef . KTreeSyntTokCt ) 
      ; INC ( NonleafRow . MsNlrEstChildCtLeftOfNl 
            , InsertionRef . KTreeEstChildCtLeftOfNl 
            )
      END (* IF *) 
    ; INC ( NonleafRow . MsNlrElemCt ) 
    END PrependToNonleafRow 

; PROCEDURE ShortenUnpackedNonleafRow ( VAR NonleafRow : MsNonleafRowTyp ) 
  RAISES { AssertionFailure } 

  = BEGIN 
      Assert 
        ( NonleafRow . MsNlrElemCt > 0 
        , AFT . A_ShortenUnpackedNonleafRow_Empty_row 
        ) 
    ; Assert 
        ( NonleafRow . MsNlrShortInfoIsKnown 
        , AFT . A_ShortenUnpackedNonleafRow_Unknown_short_info
        ) 
    ; DEC ( NonleafRow . MsNlrElemCt ) 
    ; NonleafRow . MsNlrWidthInfo := NonleafRow . MsNlrShortWidthInfo 
    ; NonleafRow . MsNlrSyntTokCt := NonleafRow . MsNlrShortSyntTokCt  
    END ShortenUnpackedNonleafRow 

; PROCEDURE UnpackNonleafRow 
    ( MergeState : MergeStateTyp 
    ; VAR NonleafRow : MsNonleafRowTyp 
    ; DoDeleteLeftmostElem : BOOLEAN 
    ; VAR ReplacedChildCt : LbeStd . EstChildNoTyp 
    ) 
  RAISES { AssertionFailure } 

  = VAR LNewElemCt : EstHs . ElemNoTyp 
  ; VAR LOrigToChildNo : LbeStd . EstChildNoTyp 
  ; VAR LOrigChildCt : LbeStd . EstChildNoTyp 
  ; VAR LCumChildCt : LbeStd . EstChildNoTyp 
  ; VAR LElemNo : EstHs . ElemNoTyp 
  ; VAR LRightGapEdgeInfo : EstHs . EdgeInfoTyp 
  ; VAR LSliceEdgeInfoPair : EstHs . SliceEdgeInfoPairTyp 

  ; BEGIN (* UnpackNonleafRow *) 
      ReplacedChildCt := 0 
    ; IF NonleafRow . MsNlrObjRef # NIL 
      THEN 
        NonleafRow . MsNlrObjRef . FetchNonleafArray 
          ( NonleafRow . MsNlrElems ) 
      (* MsNlrKindSet is already set. *) 
      ; NonleafRow . MsNlrWidthInfo 
          := NonleafRow . MsNlrObjRef . KTreeWidthInfo 
      ; NonleafRow . MsNlrSyntTokCt 
          := NonleafRow . MsNlrObjRef . KTreeSyntTokCt 
      ; NonleafRow . MsNlrElemCt := NonleafRow . MsNlrObjRef . KTreeElemCt 
      ; NonleafRow . MsNlrEstChildCtLeftOfNl 
          := NonleafRow . MsNlrObjRef . KTreeEstChildCtLeftOfNl 
      ; Assert 
          ( NonleafRow . MsNlrElemCt > 0 , AFT . A_UnpackNonleafRowEmpty ) 
      ; IF DoDeleteLeftmostElem 
        THEN (* Repack elements to recompute MsNlrSyntTokCt, 
                MsNlrWidthInfo, MsNlrKindSet, and MsNlrSliceEdgeInfoPair. 
                We will rebuild NonleafRow, while simultaneously pulling 
                the old elements out of it.  We only use MsNlrElems of the 
                old value of NonleafRow.  We fetch the Nle-- fields before 
                calling PrependToNonleafRow, to put the new ones in. 
                (They will be set back the same values anyway.) *) 
          LNewElemCt := NonleafRow . MsNlrElemCt - 1 
        ; InitNonleafRow ( NonleafRow ) 
        ; LOrigChildCt 
            := NonleafRow . MsNlrElems [ LNewElemCt ] . NleCumChildCt 
        ; LOrigToChildNo := MergeState . MsLastFromChildNo + LOrigChildCt 
          (* Since we are unpacking this node, it and all its 
             descendents were packed, which implies that 
             MsLastFromChildNo is the original child no of 
             the leftmost descendent of this node. 
          *) 
        ; IF LNewElemCt > 0 
          THEN 
            LElemNo := 0 
          ; EstUtil . GetKTreeSliceEdgeInfoPair 
              ( MergeState . MsLang 
              , NonleafRow . MsNlrElems [ LElemNo ] . NleChildRef 
              , LSliceEdgeInfoPair 
              ) 
          ; EstHs . MakeWidthInfoNull ( NonleafRow . MsNlrShortWidthInfo )  
          ; NonleafRow . MsNlrShortSyntTokCt := 0 
          ; NonleafRow . MsNlrShortInfoIsKnown := TRUE 
          ; LOOP 
              WITH WNonleafElem = NonleafRow . MsNlrElems [ LElemNo ] 
              DO PrependToNonleafRow 
                   ( MergeState 
                   , NonleafRow 
                   , WNonleafElem . NleChildRef 
                   , LSliceEdgeInfoPair 
                   , WNonleafElem . NleKindSet 
                   , IsLeftOfSeam := FALSE 
                   ) 
              ; LCumChildCt := WNonleafElem . NleCumChildCt 
              END (* WITH WNonleafElem *) 
            ; INC ( LElemNo ) 
            ; WITH WNonleafElem = NonleafRow . MsNlrElems [ LElemNo ] 
              DO 
                LRightGapEdgeInfo := LSliceEdgeInfoPair . SeiLeftEdgeInfo 
              ; EstUtil . GetKTreeSliceEdgeInfoPair 
                  ( MergeState . MsLang 
                  , WNonleafElem . NleChildRef 
                  , LSliceEdgeInfoPair 
                  ) 
              ; ReincludeListSepTokInfo 
                  ( MergeState 
                  , LeftGapEdgeInfo 
                      := LSliceEdgeInfoPair . SeiRightEdgeInfo 
                  , RightGapEdgeInfo := LRightGapEdgeInfo 
                  , OrigEstRef := MergeState . MsLastEstRef 
                  , OrigFromChildNo := MergeState . MsLastFromChildNo 
                  , OrigToChildNo := MergeState . MsLastToChildNo 
                  , RightGapOrigChildNo := LOrigToChildNo - LCumChildCt 
                  ) 
              END (* WITH WNonleafElem *) 
            ; IF LElemNo >= LNewElemCt 
              THEN 
                ReplacedChildCt := LOrigChildCt - LCumChildCt 
              ; EXIT 
              ELSE 
                NonleafRow . MsNlrShortWidthInfo 
                  := NonleafRow . MsNlrWidthInfo 
              ; NonleafRow . MsNlrShortSyntTokCt 
                  := NonleafRow . MsNlrSyntTokCt  
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *) 
        ELSE (* NOT DoDeleteLeftmostElem. *) 
          EstUtil . GetKTreeSliceEdgeInfoPair 
            ( MergeState . MsLang 
            , NonleafRow . MsNlrObjRef 
            , NonleafRow . MsNlrSliceEdgeInfoPair 
            ) 
        ; NonleafRow . MsNlrShortInfoIsKnown := FALSE  
        ; EstHs . MakeWidthInfoNull ( NonleafRow . MsNlrShortWidthInfo )  
        ; NonleafRow . MsNlrShortSyntTokCt := LbeStd . ParseCheckInfinity  
        (* ^The above two fields are unknown. *) 
        END (* IF *) 
      ; NonleafRow . MsNlrObjRef := NIL 
      END (* IF *) 
    END UnpackNonleafRow 

; PROCEDURE FlushNonleafRow 
    ( VAR NonleafRow : MsNonleafRowTyp 
    ; VAR ResultRef : EstHs . KTreeRefTyp 
    ; VAR ResultKindSet : EstHs . EstChildKindSetTyp 
    ) 
  RAISES { AssertionFailure } 

  = BEGIN (* FlushNonleafRow *) 
      Assert 
        ( NonleafRow . MsNlrObjRef = NIL , AFT . A_FlushNonleafRowWithObjRef ) 
    ; IF NonleafRow . MsNlrElemCt > 0 
      THEN 
        ResultRef 
          := NEW 
               ( EstHs . KTreeNonleafRefTyp 
               , KTreeWidthInfo := NonleafRow . MsNlrWidthInfo 
               , KTreeSyntTokCt := NonleafRow . MsNlrSyntTokCt 
               , KTreeElemCt := NonleafRow . MsNlrElemCt 
               , KTreeEstChildCtLeftOfNl 
                   := NonleafRow . MsNlrEstChildCtLeftOfNl 
               , KTreeNonleafSliceEdgeInfoPair 
                   := NonleafRow . MsNlrSliceEdgeInfoPair 
               , KTreeNonleafArrayRef 
                   := EstHs . RefToNewNonleafArray 
                        ( NonleafRow . MsNlrElemCt , NonleafRow . MsNlrElems ) 
               ) 
      ; ResultKindSet := NonleafRow . MsNlrKindSet 
      ; InitNonleafRow ( NonleafRow ) 
      (* At this point, it is possible that NonleafRow will next be reused on 
         the other side of the slice, with unrelated contents. *) 
      ELSE 
        ResultRef := NIL 
      ; ResultKindSet := EstHs . EstChildKindSetEmpty 
      END (* IF *) 
    END FlushNonleafRow 

; PROCEDURE PropagateChangesUpward 
    ( MergeState : MergeStateTyp 
    ; StartingHeight : EstHs . KTreeHeightTyp 
    ; ReplaceFarRight : BOOLEAN 
    ; InsertionRef : EstHs . KTreeRefTyp 
    ; InsertionKindSet : EstHs . EstChildKindSetTyp 
    ) 
  RAISES { AssertionFailure } 

  = VAR LHeight : EstHs . KTreeHeightTyp 
  ; VAR LInsertionRef : EstHs . KTreeRefTyp 
  ; VAR LUpwardInsertionRef : EstHs . KTreeRefTyp 
  ; VAR LKindSet : EstHs . EstChildKindSetTyp 
  ; VAR LUpwardKindSet : EstHs . EstChildKindSetTyp 
  ; VAR LReplacedChildCt : LbeStd . EstChildNoTyp 
  ; VAR LSliceEdgeInfoPair : EstHs . SliceEdgeInfoPairTyp 
  ; VAR LReplaceFarRight : BOOLEAN 
  ; VAR LLevelBelowIsUnpacked : BOOLEAN 

  ; BEGIN (* PropagateChangesUpward *) 
      Assert 
        ( StartingHeight >= 1 
        , AFT . A_PropagateChangesUpwardStartingHeightLt1 
        ) 
    ; LHeight := StartingHeight 
    ; LReplaceFarRight := ReplaceFarRight 
    ; LInsertionRef := InsertionRef 
    ; LKindSet := InsertionKindSet 
    ; IF LHeight = 1 
      THEN
        LLevelBelowIsUnpacked := MergeState . MsLeafRow . MsLrObjRef = NIL 
      ELSE 
        LLevelBelowIsUnpacked 
          := MergeState . MsNonleafRowRefs [ LHeight ] ^ . MsNlrObjRef = NIL 
      END (* IF *) 

    (* Merge with higher levels of MergeState that already exist. *) 
    ; LOOP
        IF LHeight >= MergeState . MsHeight 
        THEN EXIT 
        ELSE 
          INC ( LHeight ) 
        ; WITH WNonleafRow = MergeState . MsNonleafRowRefs [ LHeight ] ^  
          DO 
            LUpwardInsertionRef := NIL 
          ; IF LInsertionRef # NIL OR LReplaceFarRight OR LLevelBelowIsUnpacked   
            THEN (* Row needs to be unpacked. *)  
              IF WNonleafRow . MsNlrObjRef # NIL 
              THEN (* But it's packed, so unpack it. *) 
                UnpackNonleafRow 
                  ( MergeState 
                  , WNonleafRow 
                  , DoDeleteLeftmostElem := LReplaceFarRight 
                  , (* VAR *) ReplacedChildCt := LReplacedChildCt 
                  ) 
              ; LReplaceFarRight := TRUE (* New value for level above. *) 
              ELSE (* It's already unpacked. *)  
                IF LReplaceFarRight 
                THEN 
                  ShortenUnpackedNonleafRow ( WNonleafRow ) 
                ; LReplacedChildCt := 1 
                ELSE 
                  LReplacedChildCt := 0 
                END (* IF *) 
              ; LReplaceFarRight := FALSE (* New value for level above. *) 
              END (* IF *) 

            ; IF WNonleafRow . MsNlrElemCt 
                 + ORD ( LInsertionRef # NIL ) + ORD ( LLevelBelowIsUnpacked  ) 
                 >= EstHs . NonleafArrayElemCtMax 
              THEN (* This will leave enough space for both. *) 
                FlushNonleafRow 
                  ( WNonleafRow , LUpwardInsertionRef , LUpwardKindSet ) 
              END (* IF *) 
            ; IF LInsertionRef # NIL  
              THEN (* Pack in new child from below. *)
                EstUtil . GetKTreeSliceEdgeInfoPair 
                  ( MergeState . MsLang 
                  , LInsertionRef 
                  , (* VAR *) LSliceEdgeInfoPair 
                  ) 
              ; PrependToNonleafRow 
                  ( MergeState 
                  , WNonleafRow 
                  , LInsertionRef 
                  , LSliceEdgeInfoPair 
                  , LKindSet 
                  , IsLeftOfSeam := TRUE 
                  ) 
              END (* IF *) 
            ; LLevelBelowIsUnpacked := TRUE (* New value for level above. *)  
            ELSE
              TYPECASE WNonleafRow . MsNlrObjRef <* NOWARN *> 
              OF NULL  
              => (* Unpacked, kind set bits will propagate, We're done. *) 
                 LLevelBelowIsUnpacked := TRUE 
              | EstHs . KTreeNonleafRefTyp ( TKTreeNonleafRef ) 
              => UneToParentKindSet 
                  ( (* IN OUT *)  
                    TKTreeNonleafRef . KTreeNonleafArrayRef 
                      ^ [ NUMBER 
                            ( TKTreeNonleafRef . KTreeNonleafArrayRef ^ ) 
                          - 1 
                        ] 
                      . NleKindSet 
                  , MergeState . MsWaitingRightmostKindSet
                  )  
              ; WNonleafRow . MsNlrKindSet 
                  := WNonleafRow . MsNlrKindSet 
                     + MergeState . MsWaitingRightmostKindSet 
              ; LReplaceFarRight := FALSE 
              ; LLevelBelowIsUnpacked := FALSE 
           (* ELSE Can't happen. *) 
              END (* TYPECASE *) 
            END (* IF *) 
          ; LInsertionRef := LUpwardInsertionRef 
          ; LKindSet := LUpwardKindSet 
          END (* WITH WNonleafRow *) 
        END (* IF *) 
      END (* LOOP *) 

    (* See if an additional level is needed in MergeState *) 
    ; IF LInsertionRef # NIL 
      THEN (* construct new MergeState level with the 
              remaining insertion in it. *) 
        Assert 
          ( MergeState . MsHeight < EstHs . KTreeHeightMax 
          , AFT . A_PropagateChangesUpwardKTreeTooHigh 
          ) 
      ; INC ( MergeState . MsHeight ) 
      ; WITH 
          WNonleafRowRef 
          = MergeState . MsNonleafRowRefs [ MergeState . MsHeight ] 
        DO 
          IF WNonleafRowRef = NIL 
          THEN WNonleafRowRef := NEW ( MsNonleafRowRefTyp ) 
          END (* IF *)  
        ; InitNonleafRow ( WNonleafRowRef ^  ) 
        ; EstUtil . GetKTreeSliceEdgeInfoPair 
            ( MergeState . MsLang 
            , LInsertionRef 
            , LSliceEdgeInfoPair 
            ) 
        ; PrependToNonleafRow 
            ( MergeState 
            , WNonleafRowRef ^  
            , LInsertionRef 
            , LSliceEdgeInfoPair 
            , LKindSet 
            , IsLeftOfSeam := TRUE  
            ) 
        END (* WITH *) 
      END (* IF *) 
    END PropagateChangesUpward 

(* VISIBLE: *) 
; PROCEDURE AlterLMChild 
    ( MergeState : MergeStateTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; NewEstChildRef : LbeStd . EstRootTyp := NIL 
    ) 
  RAISES { AssertionFailure } 
  (* Alter the leftmost item in the being-constructed Est node, as follows:
     Include KindSet in its kind set. 
     IF NewEstChildRef is non-NIL, change the child to NewEstChildRef. 
  *) 

  = VAR LLeafArrayRef : EstHs . LeafArrayRefTyp 
  ; VAR LReplaceChildCt : LbeStd . EstChildNoTyp 
  ; VAR LHeight : EstHs . KTreeHeightTyp 
  ; VAR LEstChildMustChange : BOOLEAN 

  ; BEGIN (* AlterLMChild *) 
      Assert 
        ( MergeState . MsHeight > 0 
        , AFT . A_AlterLMChild_EmptyMergeState  
        ) 
    ; IF MergeState . MsLeafRow . MsLrObjRef = NIL 
      THEN (* Leaf row is unpacked. *) 
        WITH 
          WLeafElem 
          = MergeState . MsLeafRow . MsLrElems 
              [ MergeState . MsLeafRow . MsLrElemCt - 1 ] 
        DO 
          WLeafElem . LeKindSet := WLeafElem . LeKindSet + KindSet 
        ; MergeState . MsLeafRow . MsLrKindSet 
            := MergeState . MsLeafRow . MsLrKindSet + KindSet 
        ; IF NewEstChildRef # NIL 
          THEN WLeafElem . LeChildRef := NewEstChildRef 
          END (* IF *) 
        END (* WITH *) 
      ELSE (* Leaf row is packed. *) 
        LLeafArrayRef 
          := MergeState . MsLeafRow . MsLrObjRef . LeafArrayRef ( ) 
      ; WITH WPackedLeafElem 
             = LLeafArrayRef ^ [ NUMBER ( LLeafArrayRef ^ ) - 1 ] 
        DO 
          LEstChildMustChange 
            := NewEstChildRef # NIL 
               AND NewEstChildRef # WPackedLeafElem . LeChildRef 
        ; IF LEstChildMustChange 
             OR NOT KindSet <= WPackedLeafElem . LeKindSet 
          THEN (* There are changes to make in the packed leaf row. *) 
            IF LEstChildMustChange 
               OR KindSet * EstHs . EstChildKindSetImmutable 
                  # EstHs . EstChildKindSetEmpty
               
            THEN (* Something besides immutable bits is changing. *)   
              UnpackLeafRow ( MergeState ) 
            ; WITH 
                WLeafElem 
                = MergeState . MsLeafRow . MsLrElems 
                    [ MergeState . MsLeafRow . MsLrElemCt - 1 ] 
              DO 
                WLeafElem . LeKindSet := WLeafElem . LeKindSet + KindSet 
              ; MergeState . MsLeafRow . MsLrKindSet 
                  := MergeState . MsLeafRow . MsLrKindSet + KindSet 
              ; WLeafElem . LeChildRef := NewEstChildRef 
              END (* WITH *) 
           (* We unpacked the leaf level, so have to unpack all higher 
              levels too. *) 
            ; FOR RHeight := 2 TO MergeState . MsHeight 
              DO UnpackNonleafRow 
                   ( MergeState 
                   , MergeState . MsNonleafRowRefs [ RHeight ] ^  
                   , DoDeleteLeftmostElem := TRUE 
                   , (* VAR *) ReplacedChildCt := LReplaceChildCt (* Dead. *) 
                   ) 
              END (* FOR *) 
            ELSE (* Only mutable bits to set. We can do this in place, in
                    the packed rows. *) 
              WITH WKindSet 
                   = LLeafArrayRef ^ [ NUMBER ( LLeafArrayRef ^ ) - 1 ] 
                     . LeKindSet 
              DO WKindSet := WKindSet + KindSet 
              END (* WITH *) 
            ; UneToParentKindSet 
                ( (* IN OUT *) MergeState . MsLeafRow . MsLrKindSet , KindSet ) 
            ; MergeState . MsLeafRow . MsLrKindSet 
                := MergeState . MsLeafRow . MsLrKindSet + KindSet  
            ; LHeight := 2 
            ; LOOP 
                IF LHeight > MergeState . MsHeight 
                THEN EXIT 
                ELSE 
                  WITH WNonleafRow 
                       = MergeState . MsNonleafRowRefs [ LHeight ] ^ 
                  DO
                    TYPECASE WNonleafRow . MsNlrObjRef <* NOWARN *> 
                    OF NULL 
                    => (* This level is unpacked. *) 
                       EXIT 
                    | EstHs . EstNonleafRefTyp ( TEstNonleafRef ) 
                    => UneToParentKindSet 
                         ( (* IN OUT *) 
                           TEstNonleafRef . EstNonleafArrayRef 
                           ^ [ NUMBER 
                                 ( TEstNonleafRef . EstNonleafArrayRef ^ ) 
                               - 1 
                             ] 
                           . NleKindSet 
                         , KindSet
                         ) 
                    ; WNonleafRow . MsNlrKindSet 
                        := WNonleafRow . MsNlrKindSet + KindSet
                    ; TEstNonleafRef . EstChildKindSet 
                        := TEstNonleafRef . EstChildKindSet + KindSet 
                    ; EXIT  
                    | EstHs . KTreeNonleafRefTyp ( TKTreeNonleafRef ) 
                    => UneToParentKindSet  
                         ( (* IN OUT *)  
                           TKTreeNonleafRef . KTreeNonleafArrayRef 
                           ^ [ NUMBER 
                                 ( TKTreeNonleafRef . KTreeNonleafArrayRef ^ ) 
                               - 1 
                             ] 
                           . NleKindSet 
                         , KindSet 
                         ) 
                    ; WNonleafRow . MsNlrKindSet 
                        := WNonleafRow . MsNlrKindSet + KindSet 
                 (* ELSE Cant happen. *) 
                    END (* TYPECASE *) 
                  END (* WITH *) 
                ; INC ( LHeight ) 
                END (* IF *) 
              END (* LOOP *) 
            END (* IF *) 
          END (* IF *) 
        END (* WITH *) 
      END (* IF *) 
    END AlterLMChild 

(* VISIBLE: *) 
; PROCEDURE InclNextRightmostChildKindSet 
    ( MergeState : MergeStateTyp ; KindSet : EstHs . EstChildKindSetTyp ) 
  (* Arrange to include KindSet in the child kind set of the rightmost 
     element of the next-to-be-merged child or slice of MergeState. 
     FinishMerge asserts something was merged since this was called with
     a nonempty kind set*) 

  = BEGIN (* InclNextRightmostChildKindSet *) 
      MergeState . MsWaitingRightmostKindSet 
        := MergeState . MsWaitingRightmostKindSet + KindSet 
    END InclNextRightmostChildKindSet 

(* VISIBLE: *) 
; PROCEDURE MergeChild 
    ( MergeState : MergeStateTyp 
    ; EstRef : LbeStd . EstRootTyp 
    (* ^Must a leaf of an Est, including a mod. *) 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; IsFirstOfGroup : BOOLEAN 
      (* ^ IsFirstOfGroup will always be stored in 
           EstHs . EstChildKindFirstOfGroup and, if true, additionally causes 
           storing LeFmtNo of the child. 
      *) 
    ; GroupFmtNo : EstHs . FmtNoTyp 
      (* ^GroupFmtNo is used only if IsFirstOfGroup *) 
    ) 
  RAISES { AssertionFailure } 
(* Merging proceeds right to left. *) 

  = VAR LInsertionRef : EstHs . KTreeRefTyp 
  ; VAR LUpwardKindSet : EstHs . EstChildKindSetTyp 
  ; VAR LReplaceFarRight : BOOLEAN 
  ; VAR LFmtNo : EstHs . FmtNoTyp 
  ; VAR LLeafKindSet : EstHs . EstChildKindSetTyp 

  ; BEGIN (* MergeChild *) 
      Assert 
        ( ( EstRef # NIL ) = ( EstHs . EstChildKindNonNIL IN KindSet ) 
        , AFT . A_MergeChildBadEstChildKindNonNIL 
        ) 
    ; LInsertionRef := NIL 
    (* Ensure MsHeight is at least one. *) 
    ; IF MergeState . MsHeight = 0 
      THEN 
        MergeState . MsHeight := 1 
      ; InitLeafRow ( MergeState . MsLeafRow ) 
      ; MergeState . MsTrailingNeedsSep 
          := LangUtil . NeedsSep 
               ( MergeState . MsLang 
               , EstUtil . RightTokForEst ( MergeState . MsLang , EstRef ) 
               , MergeState . MsLeftTok 
               ) 
      END (* IF *) 

    (* Make sure there is space to pack in EstRef. *) 
    ; IF MergeState . MsLeafRow . MsLrObjRef # NIL 
      THEN (* An allocated node exists. *) 
        IF MergeState . MsLeafRow . MsLrObjRef . KTreeElemCt 
           = EstHs . LeafArrayElemCtMax 
        THEN (* Allocated node is full.  Include it and set up an empty, 
                unpacked array. *) 
          IF MergeState . MsHeight = 1 
          THEN (* We are at the top. *) 
            LInsertionRef := MergeState . MsLeafRow . MsLrObjRef 
          ; LUpwardKindSet := MergeState . MsLeafRow . MsLrKindSet 
          END (* IF *) 
        ; InitLeafRow ( MergeState . MsLeafRow ) 
        ; LReplaceFarRight := FALSE 
        ELSE (* Unpack less-than-full leaf node.  *) 
          UnpackLeafRow ( MergeState ) 
        ; LReplaceFarRight := TRUE 
        END (* IF allocated node is full. *) 
      ELSE (* Node is already unpacked. *) 
        IF MergeState . MsLeafRow . MsLrElemCt = EstHs . LeafArrayElemCtMax 
        THEN (* Node is now full. Flush it, to leave space for 
                child to be merged. *) 
          FlushLeafRow ( MergeState , LInsertionRef , LUpwardKindSet ) 
        END (* IF unpacked node is full. *) 
      ; LReplaceFarRight := FALSE 
      END (* IF node is allocated. *) 

    (* Pack in new child. *) 
    ; LLeafKindSet := KindSet + MergeState . MsWaitingRightmostKindSet 
    ; IF IsFirstOfGroup 
      THEN 
        LLeafKindSet := LLeafKindSet + EstHs . EstChildKindSetFirstOfGroup 
      ; LFmtNo := GroupFmtNo 
      ELSE 
        LFmtNo := EstHs . FmtNoNull 
      END (* IF *) 
    ; PrependToLeafRow 
        ( MergeState , EstRef , LLeafKindSet , LFmtNo , IsLeftOfSeam := TRUE ) 

    (* Do upward propagation of changes, if necessary *) 
    ; PropagateChangesUpward 
        ( MergeState 
        , StartingHeight := 1 
        , ReplaceFarRight := LReplaceFarRight 
        , InsertionRef := LInsertionRef 
        , InsertionKindSet := LUpwardKindSet 
        ) 

    (* Save things for next time. *) 
    ; MergeState . MsLastEstRef := EstRef 
    ; MergeState . MsLastFromChildNo := LbeStd . EstChildNoNull 
    ; MergeState . MsLastToChildNo := LbeStd . EstChildNoNull 
    ; MergeState . MsWaitingRightmostKindSet := EstHs . EstChildKindSetEmpty 
    END MergeChild 

(* VISIBLE: *) 
; PROCEDURE MergeSlice 
    ( MergeState : MergeStateTyp 
    ; EstRef : EstHs . EstRefTyp (* Est from which slice is taken. *) 
    ; FromChildNo : LbeStd . EstChildNoTyp 
    ; ToChildNo : LbeStd . EstChildNoTyp 
    ; SetFirstOfGroupAndFmtNo : BOOLEAN 
      (* ^EstChildKindFirstOfGroup and FmtNo of new leftmost child 
         come from the following parameters, instead of from the 
         old leftmost child. *) 
    ; IsFirstOfGroup : BOOLEAN 
      (* ^ The value of IsFirstOfGroup to be stored in 
          EstHs . EstChildKindFirstOfGroup of leftmost child of 
          slice. (Only meaningful if SetFirstOfGroupAndFmtNo *) 
    ; GroupFmtNo : EstHs . FmtNoTyp 
      (* ^GroupFmtNo is stored only if SetFirstOfGroupAndFmtNo 
         AND IsFirstOfGroup *) 
    ; VAR LeftmostNewChildRef : LbeStd . EstRootTyp 
    ; VAR LeftmostNewKindSet : EstHs . EstChildKindSetTyp 
    ) 
  RAISES { AssertionFailure }

  = TYPE AdjKindTyp 
      = { AdjKindSame (* Left and right arrays are really the same array. *) 
        , AdjKindAdj  (* Left and right are adjacent. *) 
        , AdjKindSep  (* Left and right have (sibs|cousins)* between. *) 
        } 
  (* Package all the values passed downward by recursive calls: *) 
  ; TYPE DownInfoTyp 
      = RECORD 
          DiLeftObjRef : EstHs . KTreeRefTyp 
        ; DiRightObjRef : EstHs . KTreeRefTyp 
        ; DiLeftKindSet : EstHs . EstChildKindSetTyp 
        ; DiRightKindSet : EstHs . EstChildKindSetTyp 
        ; DiFromChildNo : LbeStd . EstChildNoTyp 
        ; DiThruChildNo : LbeStd . EstChildNoTyp 
        ; DiHeight : EstHs . KTreeHeightTyp 
        ; DiAdjKind : AdjKindTyp 
        END (* RECORD *) 
  (* Package all the values returned by recursive calls: *) 
  ; TYPE ResultInfoTyp 
      = RECORD 
          RiLeftRef : EstHs . KTreeRefTyp 
        ; RiSeamInsertionRef1 : EstHs . KTreeRefTyp 
        ; RiSeamInsertionRef2 : EstHs . KTreeRefTyp 
        ; RiLeftKindSet : EstHs . EstChildKindSetTyp 
        ; RiSeamInsertionKindSet1 : EstHs . EstChildKindSetTyp 
        ; RiSeamInsertionKindSet2 : EstHs . EstChildKindSetTyp 
        ; RiRightGapOrigChildNo : LbeStd . EstChildNoTyp 
        (* ^When LbeStd . EstChildNoNull, no list seps are to 
            be inserted between RiSeamInsertionRef1 and 2. 
            Otherwise, value of RightGapOrigChildNo to 
            use in inserting list separators. *) 
        ; RiReplaceLeft : BOOLEAN 
        ; RiReplaceRight : BOOLEAN 
        ; RiReplaceFarRight : BOOLEAN 
        END (* RECORD *) 

  ; VAR MslEstHeight : EstHs . KTreeHeightTyp 
  ; VAR MslMsStartingHeight : EstHs . KTreeHeightTyp 

  ; PROCEDURE MslLeaf 
      ( DownInfo : DownInfoTyp ; VAR ResultInfo : ResultInfoTyp ) 
    RAISES { AssertionFailure } 

    = VAR MslLSeamInsertionCt1 : PortTypes . Card16Typ 
    ; VAR MslLSeamInsertionCt2 : PortTypes . Card16Typ 

          (* ^MslLSeamInsertionCt(1|2) are the numbers of 
             children planned for the left and right (resp.) 
             new nodes being built along the seam. --1 is 
             shifted to --2 when we finish the right new node, 
             so --2 is always the current one to use when filling. *) 

    ; PROCEDURE MslLFlush ( ) RAISES { AssertionFailure } 

      = VAR LInsertionRef : EstHs . KTreeRefTyp 
      ; VAR LKindSet : EstHs . EstChildKindSetTyp 

      ; BEGIN (* MslLFlush *) 
          IF MergeState . MsLeafRow . MsLrElemCt > 0 
          THEN 
            Assert 
              ( ResultInfo . RiSeamInsertionRef1 = NIL 
              , AFT . A_MslLFlushNoSpace 
              ) 
          ; FlushLeafRow ( MergeState , LInsertionRef , LKindSet ) 
          ; IF ResultInfo . RiSeamInsertionRef2 = NIL 
            THEN 
              ResultInfo . RiSeamInsertionRef2 := LInsertionRef 
            ; ResultInfo . RiSeamInsertionKindSet2 := LKindSet 
            ; MslLSeamInsertionCt2 := MslLSeamInsertionCt1 
            ELSE 
              ResultInfo . RiSeamInsertionRef1 := LInsertionRef 
            ; ResultInfo . RiSeamInsertionKindSet1 := LKindSet 
            ; MslLSeamInsertionCt2 := EstHs . LeafArrayElemCtMax 
            END (* IF *) 
          END (* IF *) 
        END MslLFlush 

    ; PROCEDURE MslLPrepend 
        ( InsertionRef : LbeStd . EstRootTyp 
        ; KindSet : EstHs . EstChildKindSetTyp 
        ; FmtNo : EstHs . FmtNoTyp 
        ; IsLeftOfSeam : BOOLEAN 
        ) 
      RAISES { AssertionFailure } 

      = BEGIN (* MslLPrepend *) 
          IF MergeState . MsLeafRow . MsLrElemCt >= MslLSeamInsertionCt2 
          THEN 
            MslLFlush ( ) 
          END (* IF *) 
        ; PrependToLeafRow 
            ( MergeState , InsertionRef , KindSet , FmtNo , IsLeftOfSeam ) 
        END MslLPrepend 

    ; PROCEDURE MslLReincludeListSepTokInfo 
        ( LeftGapEdgeInfo : EstHs . EdgeInfoTyp 
        ; RightGapEdgeInfo : EstHs . EdgeInfoTyp 
        ; RightGapOrigChildNo : LbeStd . EstChildNoTyp 
        ) 
      RAISES { AssertionFailure } 

      = BEGIN (* MslLReincludeListSepTokInfo *) 
          IF ResultInfo . RiSeamInsertionRef2 = NIL 
             (* ^We are still in the rightmost insertion, *) 
             AND MergeState . MsLeafRow . MsLrElemCt = MslLSeamInsertionCt2 
                 (* but it is full. *) 
          THEN (* Postpone list seps to next level up. *) 
            ResultInfo . RiRightGapOrigChildNo := RightGapOrigChildNo 
          ELSE 
            ReincludeListSepTokInfo 
              ( MergeState := MergeState 
              , LeftGapEdgeInfo := LeftGapEdgeInfo 
              , RightGapEdgeInfo := RightGapEdgeInfo 
              , OrigEstRef := EstRef 
              , OrigFromChildNo := FromChildNo 
              , OrigToChildNo := ToChildNo 
              , RightGapOrigChildNo := RightGapOrigChildNo 
              ) 
          END (* IF *) 
        END MslLReincludeListSepTokInfo 

    ; BEGIN (* MslLeaf *) 

        VAR LLeftElemCt : EstHs . ElemNoTyp 
      ; VAR LRightElemCt : EstHs . ElemNoTyp 
      ; VAR LLeftNewElemCt : EstHs . ElemNoTyp 
      ; VAR LRightNewElemCt : EstHs . ElemNoTyp 
      ; VAR LFromElemNo : EstHs . ElemNoTyp 
      ; VAR LLeftElemNo : EstHs . ElemNoTyp 
      ; VAR LThruElemNo : EstHs . ElemNoTyp 
      ; VAR LElemNo : EstHs . ElemNoTyp 
      ; VAR LLeftMustChange : BOOLEAN 
      ; VAR LRightMustChange : BOOLEAN 
      ; VAR LFarRightMustChange : BOOLEAN 
      ; VAR LForceRepack : BOOLEAN 
      ; VAR LFarRightElemCt : EstHs . ElemNoTyp 
      ; VAR LCombinedElemCt : PortTypes . Int32Typ 
      ; VAR LCombinedElemCt2 : PortTypes . Int32Typ 
      ; VAR LLeftmostKindSet : EstHs . EstChildKindSetTyp 
      ; VAR LLeftmostFmtNo : EstHs . FmtNoTyp 
      ; <* UNUSED *> VAR LRightmostFmtNo : EstHs . FmtNoTyp 
(* CHECK: ^Should this really be unused? *) 
      ; VAR LKindSet : EstHs . EstChildKindSetTyp 
      ; VAR LEstMiscInfo : EstHs . EstMiscInfoTyp 
      ; VAR LRightGapEdgeInfo : EstHs . EdgeInfoTyp 
      ; VAR LLeftGapEdgeInfo : EstHs . EdgeInfoTyp 
      ; VAR LRightLeafArray : EstHs . FullLeafArrayTyp 
      ; VAR LLeftLeafArray : EstHs . FullLeafArrayTyp 

      ; BEGIN (* Block MslLeaf body. *) 

        (* Compute initial locations within K-tree nodes. *) 
          LRightElemCt := DownInfo . DiRightObjRef . KTreeElemCt 
        ; DownInfo . DiRightObjRef . FetchLeafArray ( LRightLeafArray ) 
        ; Assert 
            ( DownInfo . DiThruChildNo < LRightElemCt 
            , AFT . A_MslLeafThruBeyondKTreeEnd 
            ) 
        ; LThruElemNo := LRightElemCt - 1 - DownInfo . DiThruChildNo 
        ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
          THEN (* Use same array again *) 
            Assert 
              ( DownInfo . DiFromChildNo <= DownInfo . DiThruChildNo 
              , AFT . A_MslLeafMergeEmptyRange 
              ) 
          ; LLeftElemCt := LRightElemCt 
          ELSE (* DownInfo.DiLeftObjRef is a different object *) 
            LLeftElemCt := DownInfo . DiLeftObjRef . KTreeElemCt 
          ; Assert 
              ( DownInfo . DiFromChildNo < LLeftElemCt 
              , AFT . A_MslLeafMergeEmptyRange 
              ) 
          ; DownInfo . DiLeftObjRef . FetchLeafArray ( LLeftLeafArray ) 
          END (* IF *) 
        ; LFromElemNo := LLeftElemCt - 1 - DownInfo . DiFromChildNo 

        (* Compute new Kind set and format number. *) 
        ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
          THEN 
            LLeftmostKindSet := LRightLeafArray [ LFromElemNo ] . LeKindSet 
          ; LLeftmostFmtNo := LRightLeafArray [ LFromElemNo ] . LeFmtNo 
          ; LeftmostNewChildRef 
              := LRightLeafArray [ LFromElemNo ] . LeChildRef 
          ; LeftmostNewKindSet := LLeftmostKindSet 
          ELSE 
            LLeftmostKindSet := LLeftLeafArray [ LFromElemNo ] . LeKindSet 
          ; LLeftmostFmtNo := LLeftLeafArray [ LFromElemNo ] . LeFmtNo 
          ; LeftmostNewChildRef 
              := LLeftLeafArray [ LFromElemNo ] . LeChildRef 
          ; LeftmostNewKindSet := LLeftmostKindSet 
          END (* IF *) 

        (* Increase MsHeight from 0 to 1 if necessary. *) 
        ; IF MergeState . MsHeight = 0 
          THEN 
            MergeState . MsHeight := 1 
          ; InitLeafRow ( MergeState . MsLeafRow ) 
          ; MergeState . MsTrailingNeedsSep 
              := LangUtil . NeedsSep 
                   ( MergeState . MsLang 
                   , EstUtil . RightTokForEst 
                       ( MergeState . MsLang 
                       , LRightLeafArray [ LThruElemNo ] . LeChildRef 
                       ) 
                   , MergeState . MsLeftTok 
                   ) 
          END (* IF *) 

        (* Compute which nodes must change. *) 
        ; LFarRightMustChange 
            := MslMsStartingHeight = 1 
               OR ( MergeState . MsLeafRow . MsLrObjRef = NIL 
                    AND MergeState . MsLeafRow . MsLrElemCt > 0 
                    (* It's already unpacked.  Treat as must change. *)  
                  ) 
        ; MergeState . MsWaitingRightmostKindSet 
            := MergeState . MsWaitingRightmostKindSet 
               - LRightLeafArray [ LThruElemNo ] . LeKindSet 
        ; LRightMustChange 
            := LThruElemNo > 0 
               OR ( MslEstHeight = 1 AND MergeState . MsLastEstRef # NIL )  
               OR MergeState . MsWaitingRightmostKindSet 
                  * EstHs . EstChildKindSetImmutable 
                  # EstHs . EstChildKindSetEmpty
        ; IF SetFirstOfGroupAndFmtNo 
          THEN 
            IF IsFirstOfGroup 
            THEN 
              IF NOT EstHs . EstChildKindFirstOfGroup IN LLeftmostKindSet 
              THEN 
                LLeftMustChange := TRUE 
              ; LLeftmostKindSet 
                  := LLeftmostKindSet + EstHs . EstChildKindSetFirstOfGroup 
              ; LLeftmostFmtNo := GroupFmtNo 
              ELSIF LLeftmostFmtNo # GroupFmtNo 
              THEN 
                LLeftMustChange := TRUE 
              ; LLeftmostFmtNo := GroupFmtNo 
              ELSE LLeftMustChange := LFromElemNo < LLeftElemCt - 1 
              END (* IF *) 
            ELSE 
              IF EstHs . EstChildKindFirstOfGroup IN LLeftmostKindSet 
              THEN 
                LLeftMustChange := TRUE 
              ; LLeftmostKindSet 
                  := LLeftmostKindSet - EstHs . EstChildKindSetFirstOfGroup  
              ELSE LLeftMustChange := LFromElemNo < LLeftElemCt - 1 
              END (* IF *) 
            END (* IF *) 
          ELSE LLeftMustChange := LFromElemNo < LLeftElemCt - 1 
          END (* IF *) 
        ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
          THEN 
            LRightMustChange := LLeftMustChange OR LRightMustChange 
          ; LLeftMustChange := LRightMustChange 
          ; LLeftNewElemCt := 0 
          ; LRightNewElemCt := LFromElemNo + 1 - LThruElemNo 
          ELSE 
            LLeftNewElemCt := LFromElemNo + 1 
          ; LRightNewElemCt := LRightElemCt - LThruElemNo 
          END (* IF *) 

        (* Count elements and plan how to repack. *) 
        ; IF MergeState . MsLeafRow . MsLrObjRef = NIL 
          THEN 
            LFarRightElemCt := MergeState . MsLeafRow . MsLrElemCt 
          ELSE 
            LFarRightElemCt 
              := MergeState . MsLeafRow . MsLrObjRef . KTreeElemCt 
          END (* IF *) 
        ; MslLSeamInsertionCt2 := EstHs . LeafArrayElemCtMax 
        ; MslLSeamInsertionCt1 := EstHs . LeafArrayElemCtMax 
        ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindAdj 
          THEN 
            LCombinedElemCt 
              := LLeftNewElemCt + LRightNewElemCt + LFarRightElemCt 
          ; CASE 
              ( LCombinedElemCt + EstHs . LeafArrayElemCtMax - 1 ) 
              DIV EstHs . LeafArrayElemCtMax 
              (* Number of nodes to hold LCombinedElemCt *) 
            OF 1 (* Plan to repack all three into one node. *) 
            => LForceRepack := TRUE 
            | 2 
            => IF LLeftMustChange = LFarRightMustChange 
               THEN (* Plan to repack all three into two nodes. *) 
                 LForceRepack := TRUE 
               ; MslLSeamInsertionCt2 := ( LCombinedElemCt + 1 ) DIV 2 
               (* If MergeState already has more than this many elements 
                  accumulated, it will just flush immediately, and the 
                  rest will go into another node.  This would result in
                  the elements' not being evenly divided between the nodes.
                  If this matters, it would be possible to take some elements
                  out of MergeState and into the other node, right at this
                  spot. This applies in several places where we compute 
                  MslLSeamInsertionCt2 this way. *) 
               ELSIF LRightMustChange AND LFarRightMustChange 
               THEN 
                 LCombinedElemCt2 := LRightNewElemCt + LFarRightElemCt 
               ; IF ( LCombinedElemCt2 + EstHs . LeafArrayElemCtMax - 1 ) 
                    DIV EstHs . LeafArrayElemCtMax 
                    (* ^Number of nodes needed to hold right and far right. *) 
                    = 1 
                 THEN (* Plan to repack Right and FarRight into one node. *) 
                   LForceRepack := FALSE 
                 ELSE (* Plan to repack all three into two nodes. *) 
                   LForceRepack := TRUE 
                 ; MslLSeamInsertionCt2 := ( LCombinedElemCt + 1 ) DIV 2 
                 END (* IF *) 
               ELSIF LLeftMustChange AND LRightMustChange 
               THEN 
                 LCombinedElemCt2 := LLeftNewElemCt + LRightNewElemCt 
               ; IF ( LCombinedElemCt2 + EstHs . LeafArrayElemCtMax - 1 ) 
                    DIV EstHs . LeafArrayElemCtMax 
                    (* ^Number of nodes needed to hold left and right. *) 
                    = 1 
                 THEN (* Plan to repack Left and Right into one node. *) 
                   LForceRepack := FALSE 
                 ELSE (* Plan to repack all three into two nodes. *) 
                   LForceRepack := TRUE 
                 ; MslLSeamInsertionCt2 := ( LCombinedElemCt + 1 ) DIV 2 
                 END (* IF *) 
               ELSE (* Either only left or only FarRight must change. 
                       Plan to let the one changed node fill up. *) 
                 LForceRepack := FALSE 
               END (* IF *) 
            | 3 
            => LForceRepack := FALSE 
            ; IF LLeftMustChange AND LRightMustChange AND LFarRightMustChange 
              THEN (* plan to repack all three into three nodes. *) 
                MslLSeamInsertionCt2 := LCombinedElemCt DIV 3 
              ; MslLSeamInsertionCt1 
                  := ( LCombinedElemCt 
                       - MAX 
                           ( MslLSeamInsertionCt2 
                           , MergeState . MsLeafRow . MsLrElemCt 
                           ) 
                     ) 
                     DIV 2 
                (* ^It is possible that there are already more than 
                   MslLSeamInsertionCt2 elements accumulated in 
                   MergeState.  Just divide what's left over in two *) 
              ELSIF LLeftMustChange AND LRightMustChange 
                    (* IMPLIES NOT LFarRightMustChange *) 
              THEN (* Plan to repack left and right *) 
                MslLSeamInsertionCt2 
                  := ( LLeftNewElemCt + LRightNewElemCt ) DIV 2 
              ELSIF LRightMustChange AND LFarRightMustChange 
                    (* IMPLIES NOT LLeftMustChange *) 
              THEN (* Plan to repack right and FarRight *) 
                MslLSeamInsertionCt2 
                  := ( LRightNewElemCt + LFarRightElemCt ) DIV 2 
           (* ELSE Let new nodes fill up *) 
              END (* IF *) 
            ELSE 
              CantHappen ( AFT . A_MslLeaf_TooManyNewNodes ) 
            END (* CASE *) 
          ELSE (* Either AdjKindSame or AdjKindSep. 
                  Left is not involved. *) 
            LCombinedElemCt := LRightNewElemCt + LFarRightElemCt 
          ; IF ( LCombinedElemCt + EstHs . LeafArrayElemCtMax - 1 ) 
               DIV EstHs . LeafArrayElemCtMax 
               (* ^Number of nodes needed to hold right and FarRight. *) 
               = 1 
            THEN (* Plan to repack FarRight and right into one node. *) 
              LForceRepack := TRUE 
            ELSIF LFarRightMustChange AND LRightMustChange 
            THEN (* Plan to repack FarRight and right into two nodes. *) 
              MslLSeamInsertionCt2 := ( LCombinedElemCt + 1 ) DIV 2 
            ; LForceRepack := FALSE 
            ELSE (* Just let the one that must change be repacked. *) 
              LForceRepack := FALSE 
            END (* IF *) 
          END (* IF *) 

        ; ResultInfo . RiSeamInsertionRef1 := NIL 
        ; ResultInfo . RiSeamInsertionRef2 := NIL 
        ; ResultInfo . RiRightGapOrigChildNo := LbeStd . EstChildNoNull 

        (* Possibly unpack FarRight (left node of old KTree). *) 
        ; IF MergeState . MsLeafRow . MsLrObjRef # NIL 
          THEN (* The leaf row is packed. *)  
            IF LForceRepack OR LFarRightMustChange 
            THEN (* Unpack *) 
              MergeState . MsLeafRow . MsLrWidthInfo 
                := MergeState . MsLeafRow . MsLrObjRef . KTreeWidthInfo 
            ; MergeState . MsLeafRow . MsLrSyntTokCt 
                := MergeState . MsLeafRow . MsLrObjRef . KTreeSyntTokCt 
            ; MergeState . MsLeafRow . MsLrElemCt 
                := MergeState . MsLeafRow . MsLrObjRef . KTreeElemCt 
            ; Assert 
                ( MergeState . MsLeafRow . MsLrElemCt > 0 
                , AFT . A_MslLeafUnpackedNoElements 
                ) 
            ; MergeState . MsLeafRow . MsLrObjRef . FetchLeafArray 
                ( MergeState . MsLeafRow . MsLrElems ) 
            (* MsLrKindSet is already set. *) 
            ; MergeState . MsLeafRow . MsLrObjRef := NIL 
            ; ResultInfo . RiReplaceFarRight := TRUE 
            ELSE 
              IF 1 = MergeState . MsHeight 
              THEN 
                ResultInfo . RiSeamInsertionRef2 
                  := MergeState . MsLeafRow . MsLrObjRef 
                    (* ^This is tricky. It will cause Any further 
                       node built to have its info placed into 
                       ResultInfo.RiSeam(InsertionRef|KindSet)1. 
                       But the building will use, as always, 
                       MslLSeamInsertionCt for the node size. 
                       However, in every case where we can 
                       get here, MslLSeamInsertionCt2 is already 
                       computed for the node to be rebuilt after 
                       (to the left) of this node. *) 
              ; ResultInfo . RiSeamInsertionKindSet2 
                  := MergeState . MsLeafRow . MsLrKindSet 
              END (* IF *) 
            ; InitLeafRow ( MergeState . MsLeafRow ) 
            ; ResultInfo . RiReplaceFarRight := FALSE 
            END (* IF *) 
          ELSE (* The nonleaf row is unpacked. *) 
            ResultInfo . RiReplaceFarRight := FALSE 
          END (* IF *) 

        (* Take care of right side: *) 
        ; IF LRightMustChange OR LForceRepack 
          THEN (* Re-collect elements from right side: *) 
            IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
            THEN 
              LLeftElemNo := LFromElemNo 
            ELSE 
              LLeftElemNo := LRightElemCt - 1 
            END (* IF *) 
          ; LElemNo := LThruElemNo 
          ; Assert ( LElemNo <= LLeftElemNo , AFT . A_MslLeaf_RightNoElements ) 
          ; WITH WRightLeafElem = LRightLeafArray [ LElemNo ] 
            DO LEstMiscInfo 
                 := EstUtil . EstMiscInfo 
                      ( MergeState . MsLang , WRightLeafElem . LeChildRef ) 
            ; LRightGapEdgeInfo . EiTok := LEstMiscInfo . EmiLeftTok 
            ; LRightGapEdgeInfo . EiEdgeKind := LEstMiscInfo . EmiEdgeKind 
            ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
                 AND LElemNo = LFromElemNo 
              THEN 
                LRightGapEdgeInfo . EiFmtNo := LLeftmostFmtNo 
              ELSIF EstHs . EstChildKindFirstOfGroup 
                    IN WRightLeafElem . LeKindSet 
              THEN 
                LRightGapEdgeInfo . EiFmtNo := WRightLeafElem . LeFmtNo 
              ELSE 
                LRightGapEdgeInfo . EiFmtNo := EstHs . FmtNoNull 
              END (* IF *) 
            END (* WITH *) 
          ; WHILE LElemNo < LLeftElemNo 
            (* INVARIANT: - We are about to pack in LElemNo. 
                          - It is not the leftmost element (of the right side )
                            to be packed. 
                          - LEstMiscInfo describes LElemNo. 
                          - LRightGapEdgeInfo describes the left 
                            side of LElemNo. 
            *) 
            DO WITH WRightLeafElem = LRightLeafArray [ LElemNo ] 
               DO 
                 LKindSet := WRightLeafElem . LeKindSet  
               ; IF LElemNo = LThruElemNo 
                 THEN 
                   LKindSet 
                     := LKindSet + MergeState . MsWaitingRightmostKindSet 
                 END (* IF *) 
               ; MslLPrepend 
                   ( WRightLeafElem . LeChildRef 
                   , LKindSet 
                   , WRightLeafElem . LeFmtNo 
                   , IsLeftOfSeam := TRUE 
                   ) 
               ; MergeState . MsLeftTok := LEstMiscInfo . EmiLeftTok 
               END (* WITH WRightLeafElem *) 
            ; INC ( LElemNo ) (* Which moves to the left. *)  
            ; WITH WRightLeafElem = LRightLeafArray [ LElemNo ] 
              DO LEstMiscInfo 
                   := EstUtil . EstMiscInfo 
                        ( MergeState . MsLang , WRightLeafElem . LeChildRef ) 
              ; LLeftGapEdgeInfo . EiTok := LEstMiscInfo . EmiRightTok 
              ; LLeftGapEdgeInfo . EiEdgeKind := LEstMiscInfo . EmiEdgeKind 
              ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
                   AND LElemNo = LFromElemNo 
                THEN 
                  LLeftGapEdgeInfo . EiFmtNo := LLeftmostFmtNo 
                ELSIF EstHs . EstChildKindFirstOfGroup 
                      IN WRightLeafElem . LeKindSet 
                THEN 
                  LLeftGapEdgeInfo . EiFmtNo := WRightLeafElem . LeFmtNo 
                ELSE 
                  LLeftGapEdgeInfo . EiFmtNo := EstHs . FmtNoNull 
                END (* IF *) 
              ; MslLReincludeListSepTokInfo 
                  ( LLeftGapEdgeInfo 
                  , LRightGapEdgeInfo 
                  , RightGapOrigChildNo 
                      := ToChildNo - ( LElemNo - LThruElemNo ) 
                  ) 
              ; LRightGapEdgeInfo := LLeftGapEdgeInfo 
              ; LRightGapEdgeInfo . EiTok := LEstMiscInfo . EmiLeftTok 
              END (* WITH WRightLeafElem *) 
            END (* WHILE *) 

          (* Pack last element of right side. *) 
          ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
            THEN (* This is the leftmost element of the slice. *) 
              LKindSet := LLeftmostKindSet  
            ; IF LElemNo = LThruElemNo 
              THEN (* This is also the rightmost element of the slice. *) 
                LKindSet 
                  := LKindSet + MergeState . MsWaitingRightmostKindSet 
              END (* IF *) 
            ; MslLPrepend 
                ( LeftmostNewChildRef 
                , LKindSet 
                , LLeftmostFmtNo 
                , IsLeftOfSeam := TRUE 
                ) 
            ; MergeState . MsLeftTok 
                := EstUtil . LeftTokForEst 
                     ( MergeState . MsLang , LeftmostNewChildRef ) 
            ; ResultInfo . RiReplaceLeft := TRUE  
            ELSE 
              WITH WRightLeafElem = LRightLeafArray [ LElemNo ] 
              DO 
                 LKindSet := WRightLeafElem . LeKindSet  
               ; IF LElemNo = LThruElemNo 
                 THEN 
                   LKindSet 
                     := LKindSet + MergeState . MsWaitingRightmostKindSet 
                 END (* IF *) 
              ; MslLPrepend 
                   ( WRightLeafElem . LeChildRef 
                   , LKindSet 
                   , WRightLeafElem . LeFmtNo 
                   , IsLeftOfSeam := TRUE 
                   ) 
              ; LEstMiscInfo 
                  := EstUtil . EstMiscInfo 
                       ( MergeState . MsLang , WRightLeafElem . LeChildRef ) 
              ; MergeState . MsLeftTok := LEstMiscInfo . EmiLeftTok 
              ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindAdj 
                THEN (* Compute gap edge info while we have this child. *) 
                  LRightGapEdgeInfo . EiTok := LEstMiscInfo . EmiLeftTok 
                ; LRightGapEdgeInfo . EiEdgeKind 
                    := LEstMiscInfo . EmiEdgeKind 
                ; IF EstHs . EstChildKindFirstOfGroup 
                     IN WRightLeafElem . LeKindSet 
                  THEN 
                    LRightGapEdgeInfo . EiFmtNo := WRightLeafElem . LeFmtNo 
                  ELSE 
                    LLeftGapEdgeInfo . EiFmtNo := EstHs . FmtNoNull 
                  END (* IF *) 
                END (* IF *) 
              END (* WITH WRightLeafElem *) 
            END (* IF *) 
          ; ResultInfo . RiReplaceRight := TRUE 
          ELSE (* Pass Right up unchanged, except for mutable kind bits. *) 
            MslLFlush ( ) 
          ; ResultInfo . RiReplaceRight := FALSE 
          ; IF MergeState . MsWaitingRightmostKindSet 
               # EstHs . EstChildKindSetEmpty 
            THEN
              WITH WKindSet 
                   = DownInfo . DiRightObjRef . LeafArrayRef ( ) 
                     ^ [ LThruElemNo ] . LeKindSet 
              DO WKindSet := WKindSet + MergeState . MsWaitingRightmostKindSet 
              END (* WITH *) 
            END (* IF *) 
          ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
            THEN (* Do left side work on right side, which is same. *) 
              MergeState . MsLeafRow . MsLrObjRef := DownInfo . DiRightObjRef 
            ; MergeState . MsLeafRow . MsLrKindSet 
                := DownInfo . DiRightKindSet 
                   + MergeState . MsWaitingRightmostKindSet 
            ; TYPECASE MergeState . MsLeafRow . MsLrObjRef 
              OF NULL => 
              | EstHs . EstRefTyp ( TEstRef ) 
              => TEstRef . EstChildKindSet 
                   := TEstRef . EstChildKindSet 
                      + MergeState . MsWaitingRightmostKindSet
              ELSE 
              END (* TYPECASE *)  
            ; MergeState . MsLeftTok 
                := LeftmostTokForLeafNode 
                     ( MergeState . MsLang , DownInfo . DiRightObjRef ) 
            ; ResultInfo . RiReplaceLeft := FALSE 
            END (* IF *) 
          END (* IF *) 

        (* Handle left side: *) 
        ; IF DownInfo . DiAdjKind # AdjKindTyp . AdjKindSame 
          THEN 
            IF LLeftMustChange 
               OR LForceRepack 
                  AND DownInfo . DiAdjKind = AdjKindTyp . AdjKindAdj 
            THEN 
              IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSep 
              THEN 
                MslLFlush ( ) 
              END (* IF *) 
            ; LElemNo := 0 
            ; Assert 
                ( LElemNo <= LFromElemNo , AFT . A_MslLeaf_LeftNoElements ) 
            ; WITH WLeftLeafElem = LLeftLeafArray [ LElemNo ] 
              DO LEstMiscInfo 
                   := EstUtil . EstMiscInfo 
                        ( MergeState . MsLang , WLeftLeafElem . LeChildRef ) 
              ; LLeftGapEdgeInfo . EiEdgeKind := LEstMiscInfo . EmiEdgeKind 
              ; IF LElemNo = LFromElemNo 
                THEN 
                  LLeftGapEdgeInfo . EiFmtNo := LLeftmostFmtNo 
                ELSIF EstHs . EstChildKindFirstOfGroup 
                      IN WLeftLeafElem . LeKindSet 
                THEN 
                  LLeftGapEdgeInfo . EiFmtNo := WLeftLeafElem . LeFmtNo 
                ELSE 
                  LLeftGapEdgeInfo . EiFmtNo := EstHs . FmtNoNull 
                END (* IF *) 
              END (* WITH *) 
            ; IF ( LRightMustChange OR LForceRepack ) 
                 AND DownInfo . DiAdjKind = AdjKindTyp . AdjKindAdj 
              THEN (* Insert the separators between what was repacked from 
                      right and what is about to be repacked from left. *) 
                LLeftGapEdgeInfo . EiTok := LEstMiscInfo . EmiRightTok 
              ; MslLReincludeListSepTokInfo 
                  ( LLeftGapEdgeInfo 
                  , LRightGapEdgeInfo 
                  , RightGapOrigChildNo := FromChildNo + LFromElemNo + 1 
                  ) 
              END (* IF *) 
            ; LRightGapEdgeInfo := LLeftGapEdgeInfo 
            ; LRightGapEdgeInfo . EiTok := LEstMiscInfo . EmiLeftTok 
            ; WHILE LElemNo < LFromElemNo 
              (* INVARIANT: - We are about to pack in LElemNo. 
                            - It is not the leftmost element to be packed. 
                            - LEstMiscInfo describes LElemNo. 
                            - LRightGapEdgeInfo describes the left 
                              side of LElemNo *) 
              DO WITH WLeftLeafElem = LLeftLeafArray [ LElemNo ] 
                 DO MslLPrepend 
                      ( WLeftLeafElem . LeChildRef 
                      , WLeftLeafElem . LeKindSet 
                      , WLeftLeafElem . LeFmtNo 
                      , IsLeftOfSeam := TRUE 
                      ) 
                 ; MergeState . MsLeftTok := LEstMiscInfo . EmiLeftTok 
                 END (* WITH WLeftLeafElem *) 
              ; INC ( LElemNo ) 
              ; WITH WLeftLeafElem = LLeftLeafArray [ LElemNo ] 
                DO LEstMiscInfo 
                     := EstUtil . EstMiscInfo 
                          ( MergeState . MsLang , WLeftLeafElem . LeChildRef ) 
                ; LLeftGapEdgeInfo . EiTok := LEstMiscInfo . EmiRightTok 
                ; LLeftGapEdgeInfo . EiEdgeKind := LEstMiscInfo . EmiEdgeKind 
                ; IF LElemNo = LFromElemNo 
                  THEN 
                    LLeftGapEdgeInfo . EiFmtNo := LLeftmostFmtNo 
                  ELSIF EstHs . EstChildKindFirstOfGroup 
                        IN WLeftLeafElem . LeKindSet 
                  THEN 
                    LLeftGapEdgeInfo . EiFmtNo := WLeftLeafElem . LeFmtNo 
                  ELSE 
                    LLeftGapEdgeInfo . EiFmtNo := EstHs . FmtNoNull 
                  END (* IF *) 
                ; MslLReincludeListSepTokInfo 
                    ( LLeftGapEdgeInfo 
                    , LRightGapEdgeInfo 
                    , RightGapOrigChildNo 
                        := FromChildNo + ( LFromElemNo - LElemNo ) + 1 
                    ) 
                ; LRightGapEdgeInfo := LLeftGapEdgeInfo 
                ; LRightGapEdgeInfo . EiTok := LEstMiscInfo . EmiLeftTok 
                END (* WITH WLeftLeafElem *) 
              END (* WHILE *) 

            (* Pack the leftmost element. *) 
            ; MslLPrepend 
                ( LeftmostNewChildRef 
                , LLeftmostKindSet 
                , LLeftmostFmtNo 
                , IsLeftOfSeam := TRUE 
                ) 
            ; MergeState . MsLeftTok 
                := EstUtil . LeftTokForEst 
                     ( MergeState . MsLang , LeftmostNewChildRef ) 
            ; ResultInfo . RiReplaceLeft := TRUE 
            ELSE (* Pass Left up unchanged. *)   
              MslLFlush ( ) 
            ; MergeState . MsLeafRow . MsLrObjRef := DownInfo . DiLeftObjRef 
            ; MergeState . MsLeafRow . MsLrKindSet 
                := DownInfo . DiLeftKindSet 
            ; MergeState . MsLeftTok 
                := LeftmostTokForLeafNode 
                     ( MergeState . MsLang , DownInfo . DiLeftObjRef ) 
            ; ResultInfo . RiReplaceLeft := FALSE 
            END (* IF *) 
          END (* IF *) 
        ; ResultInfo . RiLeftRef := MergeState . MsLeafRow . MsLrObjRef 
        ; ResultInfo . RiLeftKindSet 
            := MergeState . MsLeafRow . MsLrKindSet  
        END (* Block *) 
      END MslLeaf 

  ; PROCEDURE MslNonleaf 
      ( DownInfo : DownInfoTyp ; VAR ResultInfo : ResultInfoTyp ) 
    RAISES { AssertionFailure }   

    = VAR MslNlSeamInsertionCt1 : PortTypes . Card16Typ 
    ; VAR MslNlSeamInsertionCt2 : PortTypes . Card16Typ 

      (* ^MslNlSeamInsertionCt(1|2) are the numbers of 
         children planned for the left and right (resp.) 
         new nodes being built along the seam. --1 is 
         shifted to --2 when we finish the right new node, 
         so --2 is always the current one to use when filling. *) 

    ; PROCEDURE MslNlFlush ( VAR NonleafRow : MsNonleafRowTyp ) 
      RAISES { AssertionFailure } 

      = VAR LInsertionRef : EstHs . KTreeRefTyp 
      ; VAR LKindSet : EstHs . EstChildKindSetTyp 

      ; BEGIN (* MslNlFlush *) 
          IF NonleafRow . MsNlrElemCt > 0 
          THEN 
            Assert 
              ( ResultInfo . RiSeamInsertionRef1 = NIL 
              , AFT . A_MslNlFlushNoSpace 
              ) 
          ; FlushNonleafRow 
              ( NonleafRow , (* VAR *) LInsertionRef , (* VAR *) LKindSet ) 
          ; IF ResultInfo . RiSeamInsertionRef2 = NIL 
            THEN 
              ResultInfo . RiSeamInsertionRef2 := LInsertionRef 
            ; ResultInfo . RiSeamInsertionKindSet2 := LKindSet 
            ; MslNlSeamInsertionCt2 := MslNlSeamInsertionCt1 
            ELSE 
              ResultInfo . RiSeamInsertionRef1 := LInsertionRef 
            ; ResultInfo . RiSeamInsertionKindSet1 := LKindSet 
            ; MslNlSeamInsertionCt2 := EstHs . NonleafArrayElemCtMax 
            END (* IF *) 
          END (* IF *) 
        END MslNlFlush 

    ; PROCEDURE MslNlPrepend 
        ( VAR NonleafRow : MsNonleafRowTyp 
        ; InsertionRef : EstHs . KTreeRefTyp 
        ; VAR SliceEdgeInfoPair : EstHs . SliceEdgeInfoPairTyp 
        ; KindSet : EstHs . EstChildKindSetTyp 
        ; IsLeftOfSeam : BOOLEAN 
        ) 
      RAISES { AssertionFailure } 

      = BEGIN (* MslNlPrepend *) 
          IF NonleafRow . MsNlrElemCt >= MslNlSeamInsertionCt2 
          THEN 
            MslNlFlush ( NonleafRow ) 
          END (* IF *) 
        ; PrependToNonleafRow 
            ( MergeState 
            , NonleafRow 
            , InsertionRef 
            , SliceEdgeInfoPair 
            , KindSet 
            , IsLeftOfSeam 
            ) 
        END MslNlPrepend 

    ; PROCEDURE MslNlReincludeListSepTokInfo 
        ( LeftGapEdgeInfo : EstHs . EdgeInfoTyp 
        ; RightGapEdgeInfo : EstHs . EdgeInfoTyp 
        ; RightGapOrigChildNo : LbeStd . EstChildNoTyp 
        ; VAR NonleafRow : MsNonleafRowTyp 
        ) 
      RAISES { AssertionFailure } 

      = BEGIN (* MslNlReincludeListSepTokInfo *) 
          IF ResultInfo . RiSeamInsertionRef2 = NIL 
             (* ^We are still in the rightmost insertion, *) 
             AND NonleafRow . MsNlrElemCt = MslNlSeamInsertionCt2 
                 (* ^but it is full. *) 
          THEN (* Postpone list seps to next level up. *) 
            ResultInfo . RiRightGapOrigChildNo := RightGapOrigChildNo 
          ELSE 
            ReincludeListSepTokInfo 
              ( MergeState 
              , LeftGapEdgeInfo 
              , RightGapEdgeInfo 
              , EstRef 
              , FromChildNo 
              , ToChildNo 
              , RightGapOrigChildNo 
              ) 
          END (* IF *) 
        END MslNlReincludeListSepTokInfo 

    ; BEGIN (* MslNonleaf *) 
        VAR LLeftElemNo : PortTypes . Int32Typ 
      ; VAR LRightElemNo : PortTypes . Int32Typ 
      ; VAR LElemNo : PortTypes . Int32Typ 
      ; VAR LFromElemNo : EstHs . ElemNoTyp 
      ; VAR LThruElemNo : EstHs . ElemNoTyp 
      ; VAR LLeftMaxChildCt : LbeStd . EstChildNoTyp 
      ; VAR LRightMaxChildCt : LbeStd . EstChildNoTyp 
      ; VAR LOrigToChildNo : LbeStd . EstChildNoTyp 
      ; VAR LCumChildCt : LbeStd . EstChildNoTyp 
      ; VAR LLeftMustChange : BOOLEAN 
      ; VAR LRightMustChange : BOOLEAN 
      ; VAR LFarRightMustChange : BOOLEAN 
      ; VAR LForceRepack : BOOLEAN 
      ; VAR LRightNewElemCt : PortTypes . Int32Typ  
      ; VAR LLeftNewElemCt : PortTypes . Int32Typ   
      ; VAR LLeftElemCt : EstHs . ElemNoTyp 
      ; VAR LRightElemCt : EstHs . ElemNoTyp 
      ; VAR LFarRightElemCt : EstHs . ElemNoTyp 
      ; VAR LCombinedElemCt : PortTypes . Int32Typ 
      ; VAR LCombinedElemCt2 : PortTypes . Int32Typ 
      ; VAR LKindSet : EstHs . EstChildKindSetTyp 
      ; VAR LDownInfo : DownInfoTyp 
      ; VAR LResultInfo : ResultInfoTyp 
      ; VAR LSliceEdgeInfoPair : EstHs . SliceEdgeInfoPairTyp 
      ; VAR LLeftNonleafArray : EstHs . FullNonleafArrayTyp 
      ; VAR LRightNonleafArray : EstHs . FullNonleafArrayTyp 

      ; BEGIN (* Block for MslNonleaf *) 
          Assert ( DownInfo . DiHeight > 1 , AFT . A_MslNonleafHeightLE1 ) 

        (* Search right K-tree node for element to descend to *) 
        ; LRightElemCt := DownInfo . DiRightObjRef . KTreeElemCt 
        ; DownInfo . DiRightObjRef . FetchNonleafArray ( LRightNonleafArray ) 
        ; LRightMaxChildCt 
            := LRightNonleafArray [ LRightElemCt - 1 ] . NleCumChildCt 
        ; Assert 
            ( DownInfo . DiThruChildNo < LRightMaxChildCt 
            , AFT . A_MslNonleafThruBeyondKTreeEnd 
            ) 
        ; LThruElemNo 
            := EstUtil . SearchNonleafArrayForChild 
                 ( SUBARRAY ( LRightNonleafArray , 0 , LRightElemCt ) 
                 , LRightMaxChildCt 
                 , 0 
                 , DownInfo . DiThruChildNo 
                 ) 
        ; LDownInfo . DiThruChildNo 
            := DownInfo . DiThruChildNo 
               - ( LRightMaxChildCt 
                   - LRightNonleafArray [ LThruElemNo ] . NleCumChildCt 
                 ) 
        ; LDownInfo . DiRightObjRef 
            := LRightNonleafArray [ LThruElemNo ] . NleChildRef 
        ; LDownInfo . DiRightKindSet 
            := LRightNonleafArray [ LThruElemNo ] . NleKindSet 

        (* Search left K-tree node for element to descend to *) 
        ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
          THEN (* search again in the same array *) 
            Assert 
              ( DownInfo . DiFromChildNo <= DownInfo . DiThruChildNo 
              , AFT . A_MslNonleafMergeEmptyRange 
              ) 
          ; LLeftElemCt := LRightElemCt 
          ; LFromElemNo 
              := EstUtil . SearchNonleafArrayForChild 
                   ( SUBARRAY ( LRightNonleafArray , 0 , LRightElemCt ) 
                   , LRightMaxChildCt 
                   , LThruElemNo 
                   , DownInfo . DiFromChildNo 
                   ) 
          ; LDownInfo . DiFromChildNo 
              := DownInfo . DiFromChildNo 
                 - ( LRightMaxChildCt 
                     - LRightNonleafArray [ LFromElemNo ] . NleCumChildCt 
                   ) 
          ; LDownInfo . DiLeftObjRef 
              := LRightNonleafArray [ LFromElemNo ] . NleChildRef 
          ; LDownInfo . DiLeftKindSet 
              := LRightNonleafArray [ LFromElemNo ] . NleKindSet 
          ELSE (* DownInfo.DiLeftObjRef is a different object *) 
            LLeftElemCt := DownInfo . DiLeftObjRef . KTreeElemCt 
          ; DownInfo . DiLeftObjRef . FetchNonleafArray ( LLeftNonleafArray ) 
          ; LLeftMaxChildCt 
              := LLeftNonleafArray [ LLeftElemCt - 1 ] . NleCumChildCt 
          ; Assert 
              ( DownInfo . DiFromChildNo < LLeftMaxChildCt 
              , AFT . A_MslNonleafFromBeyondKTreeEnd 
              ) 
          ; LFromElemNo 
              := EstUtil . SearchNonleafArrayForChild 
                   ( SUBARRAY ( LLeftNonleafArray , 0 , LLeftElemCt ) 
                   , LLeftMaxChildCt 
                   , 0 
                   , DownInfo . DiFromChildNo 
                   ) 
          ; LDownInfo . DiFromChildNo 
              := DownInfo . DiFromChildNo 
                 - ( LLeftMaxChildCt 
                     - LLeftNonleafArray [ LFromElemNo ] . NleCumChildCt 
                   ) 
          ; LDownInfo . DiLeftObjRef 
              := LLeftNonleafArray [ LFromElemNo ] . NleChildRef 
          ; LDownInfo . DiLeftKindSet 
              := LLeftNonleafArray [ LFromElemNo ] . NleKindSet 
          END (* IF *) 

        (* Compute AdjKind to pass down *) 
        ; CASE DownInfo . DiAdjKind 
          OF AdjKindTyp . AdjKindSame 
          => CASE LFromElemNo - LThruElemNo 
             OF 0 
             => LDownInfo . DiAdjKind := AdjKindTyp . AdjKindSame 
             | 1 
             => LDownInfo . DiAdjKind := AdjKindTyp . AdjKindAdj 
             ELSE 
               LDownInfo . DiAdjKind := AdjKindTyp . AdjKindSep 
             END (* CASE *) 
          | AdjKindTyp . AdjKindAdj 
          => IF LFromElemNo = 0 AND LThruElemNo = LRightElemCt - 1 
             THEN 
               LDownInfo . DiAdjKind := AdjKindTyp . AdjKindAdj 
             ELSE 
               LDownInfo . DiAdjKind := AdjKindTyp . AdjKindSep 
             END (* IF *) 
          | AdjKindTyp . AdjKindSep 
          => LDownInfo . DiAdjKind := AdjKindTyp . AdjKindSep 
          END (* CASE *) 

        (* Descend recursively to next level *) 
        ; LDownInfo . DiHeight := DownInfo . DiHeight - 1 
        ; IF LDownInfo . DiHeight > 1 
          THEN 
            MslNonleaf ( LDownInfo , LResultInfo ) 
          ELSE 
            MslLeaf ( LDownInfo , LResultInfo ) 
          END (* IF *) 

        (* Now we are on the way back up. *) 

        (* Increase MsHeight if necessary. *) 
        ; IF MergeState . MsHeight < DownInfo . DiHeight 
          THEN 
            INC ( MergeState . MsHeight ) 
          ; Assert 
              ( MergeState . MsHeight = DownInfo . DiHeight 
              , AFT . A_MslNonleafSkippingHeightLevels 
              )
          ; WITH 
              WNonleafRowRef 
              = MergeState . MsNonleafRowRefs [ DownInfo . DiHeight ] 
            DO 
              IF WNonleafRowRef = NIL 
              THEN WNonleafRowRef := NEW ( MsNonleafRowRefTyp ) 
              END (* IF *)  
            ; InitNonleafRow ( WNonleafRowRef ^ ) 
            END (* WITH *) 
          END (* IF *) 
          
        ; WITH 
            WNonleafRow 
            = MergeState . MsNonleafRowRefs [ DownInfo . DiHeight ] ^ 
          DO 

          (* Compute which nodes must change. *) 
            LFarRightMustChange 
              := LResultInfo . RiReplaceFarRight 
                 OR DownInfo . DiHeight = MslMsStartingHeight 
                 OR ( WNonleafRow . MsNlrObjRef = NIL 
                      AND WNonleafRow . MsNlrElemCt > 0 
                    ) 
          ; LRightMustChange 
              := LResultInfo . RiReplaceRight 
                 OR LThruElemNo > 0 
                 OR ( DownInfo . DiHeight = MslEstHeight 
                      AND MergeState . MsLastEstRef # NIL 
                    )   
          ; LLeftMustChange 
              := LResultInfo . RiReplaceLeft OR LFromElemNo < LLeftElemCt - 1 

          (* Plan how to repack nodes. *) 
          ; IF WNonleafRow . MsNlrObjRef = NIL 
            THEN 
              LFarRightElemCt := WNonleafRow . MsNlrElemCt 
            ELSE 
              LFarRightElemCt := WNonleafRow . MsNlrObjRef . KTreeElemCt 
            END (* IF *) 
          ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
            THEN 
              LRightMustChange := LLeftMustChange OR LRightMustChange 
            ; LLeftMustChange := LRightMustChange 
            ; LLeftNewElemCt := 0 
            ; LRightNewElemCt := LFromElemNo + 1 - LThruElemNo 
                 + ORD ( LResultInfo . RiSeamInsertionRef2 # NIL ) 
                 + ORD ( LResultInfo . RiSeamInsertionRef1 # NIL ) 
                 - ORD ( LResultInfo . RiReplaceFarRight ) 
              (* LResultInfo . RiReplaceRight refers to the the same node as 
                 RiReplaceLeft, and RiReplaceLeft means we both remove a child
                 and must leave space for one from below later. *) 
            ELSE 
              LLeftNewElemCt := LFromElemNo + 1   
              (* Iff we replace left, we also have to leave a space for the
                 new pointer, that will come up in a later merge. *) 
            ; LRightNewElemCt := LRightElemCt - LThruElemNo 
              + ORD ( LResultInfo . RiSeamInsertionRef2 # NIL ) 
              + ORD ( LResultInfo . RiSeamInsertionRef1 # NIL ) 
              - ORD ( LResultInfo . RiReplaceFarRight ) 
              - ORD ( LResultInfo . RiReplaceRight ) 
              (* ^This can never increase LRightNewElemCt. *) 
            END (* IF *) 
          ; MslNlSeamInsertionCt2 := EstHs . NonleafArrayElemCtMax 
          ; MslNlSeamInsertionCt1 := EstHs . NonleafArrayElemCtMax 
          ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindAdj 
            THEN 
              LCombinedElemCt 
                := LLeftNewElemCt + LRightNewElemCt + LFarRightElemCt 
            ; CASE 
                ( LCombinedElemCt + EstHs . NonleafArrayElemCtMax - 1 ) 
                DIV EstHs . NonleafArrayElemCtMax 
                   (* ^Number of nodes needed to hold everything. *) 
              OF 1 (* Plan to repack everything into one node. *) 
              => LForceRepack := TRUE 
              | 2 
              => IF LLeftMustChange = LFarRightMustChange 
                 THEN (* Plan to repack everything into two nodes. *) 
                   LForceRepack := TRUE 
                 ; MslNlSeamInsertionCt2 := ( LCombinedElemCt + 1 ) DIV 2 
                 (* If MergeState already has more than this many elements 
                    accumulated, it will just flush immediately, and the 
                    rest will go into another node.  This applies in 
                    many places where we compute MslNlSeamInsertionCt2 
                    this way. *) 
                 ELSIF LRightMustChange AND LFarRightMustChange 
                 THEN 
                   LCombinedElemCt2 := LRightNewElemCt + LFarRightElemCt 
                 ; IF ( ( LCombinedElemCt2 
                          + EstHs . NonleafArrayElemCtMax 
                          - 1 
                        ) 
                        DIV EstHs . NonleafArrayElemCtMax 
                      ) 
                       (* ^Number of nodes needed to hold right, 
                          insertions, and FarRight. *) 
                      = 1 
                   THEN (* Plan to repack Right, FarRight, and insertions 
                              into one node. *) 
                     LForceRepack := FALSE 
                   ELSE (* Plan to repack everything into two nodes. *) 
                     LForceRepack := TRUE 
                   ; MslNlSeamInsertionCt2 := ( LCombinedElemCt + 1 ) DIV 2 
                   END (* IF *) 
                 ELSIF LLeftMustChange AND LRightMustChange 
                 THEN 
                   LCombinedElemCt2 := LLeftNewElemCt + LRightNewElemCt 
                 ; IF ( ( LCombinedElemCt2 
                          + EstHs . NonleafArrayElemCtMax 
                          - 1 
                        ) 
                        DIV EstHs . NonleafArrayElemCtMax 
                      ) 
                       (* ^Number of nodes needed to hold left, right, 
                          and insertions. *) 
                      = 1 
                   THEN (* Plan to repack Left, Right, and insertions 
                            into one node. *) 
                     LForceRepack := FALSE 
                   ELSE (* Plan to repack everything into two nodes. *) 
                     LForceRepack := TRUE 
                   ; MslNlSeamInsertionCt2 := ( LCombinedElemCt + 1 ) DIV 2 
                   END (* IF *) 
                 ELSE (* Either only left or only FarRight must change. 
                          Plan to let the one changed node fill up. *) 
                   LForceRepack := FALSE 
                 END (* IF *) 
              | 3 
              => LForceRepack := FALSE 
              ; IF LLeftMustChange 
                   AND LRightMustChange 
                   AND LFarRightMustChange 
                THEN (* plan to repack into three nodes uniformly. *) 
                  MslNlSeamInsertionCt2 := LCombinedElemCt DIV 3 
                ; MslNlSeamInsertionCt1 
                    := ( LCombinedElemCt 
                         - MAX 
                             ( MslNlSeamInsertionCt2 
                             , WNonleafRow . MsNlrElemCt 
                             ) 
                       ) 
                       DIV 2 
                  (* ^It is possible that there are already more than 
                     MslNlSeamInsertionCt2 elements accumulated in 
                     MergeState.  Just divide what's left over in two *) 
                ELSIF LLeftMustChange AND LRightMustChange 
                      (* IMPLIES NOT LFarRightMustChange *) 
                THEN (* Plan to repack left and right *) 
                  MslNlSeamInsertionCt2 
                    := ( LLeftNewElemCt + LRightNewElemCt ) DIV 2 
                ELSIF LRightMustChange AND LFarRightMustChange 
                        (* IMPLIES NOT LLeftMustChange *) 
                THEN (* Plan to repack right and FarRight *) 
                  MslNlSeamInsertionCt2 
                    := ( LRightNewElemCt + LFarRightElemCt ) DIV 2 
                ELSE (* let each new node fill up independently. 
                        No case has two ajacent, changing nodes. *) 
                END (* IF *) 
              ELSE 
                CantHappen ( AFT . A_MslNonleaf_TooManyNewNodes ) 
              END (* CASE *) 
            ELSE (* Either AdjKindSame or AdjKindSep *) 
              LCombinedElemCt := LRightNewElemCt + LFarRightElemCt 
            ; IF ( ( LCombinedElemCt + EstHs . NonleafArrayElemCtMax - 1 ) 
                   DIV EstHs . NonleafArrayElemCtMax 
                 ) 
                 (* ^Number of nodes needed to hold right, 
                    insertions, and FarRight. *) 
                 = 1 
              THEN (* plan to repack right, insertions, and FarRight 
                      into one node. *) 
                LForceRepack := TRUE 
              ELSIF LFarRightMustChange AND LRightMustChange 
              THEN (* Plan to repack right, insertions, and FarRight 
                        into two nodes. *) 
                MslNlSeamInsertionCt2 := ( LCombinedElemCt + 1 ) DIV 2 
              ELSE 
                LForceRepack := FALSE 
              END (* IF *) 
            END (* IF *) 
          ; ResultInfo . RiSeamInsertionRef1 := NIL 
          ; ResultInfo . RiSeamInsertionRef2 := NIL 
          ; ResultInfo . RiRightGapOrigChildNo := LbeStd . EstChildNoNull 

          (* Possibly unpack FarRight and possibly delete its 
             leftmost element. *) 
          ; LCumChildCt := 0 
          ; IF WNonleafRow . MsNlrObjRef # NIL 
            THEN (* Far right is packed. *)         
              IF LForceRepack OR LFarRightMustChange 
              THEN (* unpack *) 
                UnpackNonleafRow 
                  ( MergeState 
                  , WNonleafRow 
                  , DoDeleteLeftmostElem := LResultInfo . RiReplaceFarRight 
                  , (* VAR *) ReplacedChildCt := LCumChildCt 
                  ) 
              ; ResultInfo . RiReplaceFarRight := TRUE 
              ELSE (* Leave it packed.  *) 
                IF DownInfo . DiHeight = MergeState . MsHeight 
                THEN 
                  ResultInfo . RiSeamInsertionRef2 
                    := WNonleafRow . MsNlrObjRef 
                    (* ^This is tricky.  Any further node built will have 
                       its info placed into 
                       ResultInfo.RiSeam(InsertionRef|KindSet)1. 
                       However, in every case where we can 
                       get here, MslNlSeamInsertionCt2 is already 
                       computed for the node to be rebuilt after 
                       (to the left) of this node. *) 
                ; ResultInfo . RiSeamInsertionKindSet2 
                    := WNonleafRow . MsNlrKindSet 
                END (* IF *) 
              (* We are done with packed far right node.  Set up WNonleafRow 
                 for things from right side. *)   
              ; InitNonleafRow ( WNonleafRow ) 
              ; ResultInfo . RiReplaceFarRight := FALSE 
              END (* IF *) 
            ELSE (* Already unpacked. *) 
              IF LResultInfo . RiReplaceFarRight 
              THEN 
                ShortenUnpackedNonleafRow ( WNonleafRow ) 
              END (* IF *) 
            ; ResultInfo . RiReplaceFarRight := FALSE 
            END (* IF *) 

          (* Here, WNonleafRow is unpacked, ready for things from right. *)

          (* At this point, if WNonleafRow . MsNlrObjRef = NIL AND
             MslNlSeamInsertionCt2 < LFarRightElemCt, WNonleafRow is
             already fuller than we planned.  We might want to split
             it.  But this would involve recomputing all the separator
             info, etc.  So instead, we allow it to remain as is.  We
             may have to flush it at the end, leaving an empty far
             right. *)
           
          (* Collect any insertions that came from below. *) 
          ; IF LResultInfo . RiSeamInsertionRef2 # NIL 
            THEN 
              EstUtil . GetKTreeSliceEdgeInfoPair 
                ( MergeState . MsLang 
                , LResultInfo . RiSeamInsertionRef2 
                , LSliceEdgeInfoPair 
                ) 
            ; MslNlPrepend 
                ( WNonleafRow 
                , LResultInfo . RiSeamInsertionRef2 
                , LSliceEdgeInfoPair 
                , LResultInfo . RiSeamInsertionKindSet2 
                , IsLeftOfSeam := FALSE 
                    (* ^Tricky.  The seam could actually be inside either 
                        the node pointed to by RiSeamInsertionRef2 or
                        RiSeamInsertionRef1.  However, in both of
                        these cases, the lower level will have flushed
                        leading tok info, so the value of IsLeftOfSeam
                        won't matter.  Otherwise, the seam is between
                        these two nodes, in which case the values of
                        IsLeftOfSeam here and below will get the
                        leading tok info flushed at the right place. *)
                ) 
            END (* IF *) 
          ; IF LResultInfo . RiSeamInsertionRef1 # NIL 
            THEN 
              EstUtil . GetKTreeSliceEdgeInfoPair 
                ( MergeState . MsLang 
                , LResultInfo . RiSeamInsertionRef1 
                , LSliceEdgeInfoPair 
                ) 
            ; IF LResultInfo . RiRightGapOrigChildNo 
                 # LbeStd . EstChildNoNull 
              THEN 
                MslNlReincludeListSepTokInfo 
                  ( LeftGapEdgeInfo := LSliceEdgeInfoPair . SeiRightEdgeInfo 
                  , RightGapEdgeInfo 
                      := WNonleafRow . MsNlrSliceEdgeInfoPair . SeiLeftEdgeInfo 
                  , RightGapOrigChildNo 
                      := LResultInfo . RiRightGapOrigChildNo 
                  , NonleafRow := WNonleafRow 
                  ) 
              END (* IF *) 
            ; MslNlPrepend 
                ( WNonleafRow 
                , LResultInfo . RiSeamInsertionRef1 
                , LSliceEdgeInfoPair 
                , LResultInfo . RiSeamInsertionKindSet1 
                , IsLeftOfSeam := TRUE 
                ) 
            END (* IF *) 

          (* Take care of right side: *) 
          ; IF LRightMustChange OR LForceRepack 
            THEN (* Recollect elements from right side: *) 
              LRightElemNo 
                := LThruElemNo + ORD ( LResultInfo . RiReplaceRight ) 
            ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
              THEN 
                LLeftElemNo 
                  := LFromElemNo - ORD ( LResultInfo . RiReplaceLeft ) 
              ELSE 
                LLeftElemNo := LRightElemCt - 1 
              END (* IF *) 
            ; IF LRightElemNo <= LLeftElemNo 
              THEN 
                LElemNo := LRightElemNo 
              ; EstUtil . GetKTreeSliceEdgeInfoPair 
                  ( MergeState . MsLang 
                  , LRightNonleafArray [ LElemNo ] . NleChildRef 
                  , LSliceEdgeInfoPair 
                  ) 
              ; LOrigToChildNo 
                  := ToChildNo 
                     - 1 
                     - DownInfo . DiThruChildNo 
                     + LRightMaxChildCt 
                    (* ^OrigToChildNo of entire Right node. *) 
              ; IF LResultInfo . RiReplaceRight 
                THEN 
                  MslNlReincludeListSepTokInfo 
                    ( LeftGapEdgeInfo 
                        := LSliceEdgeInfoPair . SeiRightEdgeInfo 
                    , RightGapEdgeInfo 
                        := WNonleafRow . MsNlrSliceEdgeInfoPair 
                           . SeiLeftEdgeInfo 
                    , RightGapOrigChildNo 
                        := LOrigToChildNo 
                           - LRightNonleafArray [ LRightElemNo - 1 ] 
                             . NleCumChildCt 
                    , NonleafRow := WNonleafRow 
                    ) 
                END (* IF *) 
              ; LOOP 
                  WITH WRightNonleafElem = LRightNonleafArray [ LElemNo ] 
                  DO 
                    LKindSet := WRightNonleafElem . NleKindSet 
                  ; IF LRightElemNo = LThruElemNo 
                    THEN
                      LKindSet 
                        := LKindSet + MergeState . MsWaitingRightmostKindSet 
                    END (* IF *)  
                  ; MslNlPrepend 
                      ( WNonleafRow 
                      , WRightNonleafElem . NleChildRef 
                      , LSliceEdgeInfoPair 
                      , WRightNonleafElem . NleKindSet 
                      , IsLeftOfSeam := TRUE 
                      ) 
                  ; LCumChildCt := WRightNonleafElem . NleCumChildCt 
                  END (* WITH WRightNonleafElem *) 
                ; IF LElemNo >= LLeftElemNo 
                  THEN 
                    EXIT 
                  ELSE 
                    INC ( LElemNo ) 
                  ; WITH WRightNonleafElem = LRightNonleafArray [ LElemNo ] 
                    DO EstUtil . GetKTreeSliceEdgeInfoPair 
                         ( MergeState . MsLang 
                         , WRightNonleafElem . NleChildRef 
                         , LSliceEdgeInfoPair 
                         ) 
                    ; MslNlReincludeListSepTokInfo 
                        ( LeftGapEdgeInfo 
                            := LSliceEdgeInfoPair . SeiRightEdgeInfo 
                        , RightGapEdgeInfo 
                            := WNonleafRow . MsNlrSliceEdgeInfoPair 
                               . SeiLeftEdgeInfo 
                        , RightGapOrigChildNo := LOrigToChildNo - LCumChildCt 
                        , NonleafRow := WNonleafRow 
                        ) 
                    END (* WITH WRightNonleafElem *) 
                  END (* IF *) 
                END (* LOOP *) 
              END (* IF *) 
            ; ResultInfo . RiReplaceRight := TRUE 
            ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
              THEN (* This is also the left side of the new seam. *) 
                ResultInfo . RiReplaceLeft := TRUE  
              END (* IF *) 
            ELSE (* Pass Right up unchanged, except for mutable kind bits. *) 
              Assert 
                ( LResultInfo . RiRightGapOrigChildNo 
                  = LbeStd . EstChildNoNull 
                , AFT . A_MslNonleafListSepsNoRepackRight 
                ) 
            ; MslNlFlush ( WNonleafRow ) 
            ; IF MergeState . MsWaitingRightmostKindSet 
                 # EstHs . EstChildKindSetEmpty 
              THEN
                WITH WKindSet 
                     = DownInfo . DiRightObjRef . NonleafArrayRef ( ) 
                       ^ [ LThruElemNo ] . NleKindSet 
                DO
                  WKindSet 
                    := WKindSet + MergeState . MsWaitingRightmostKindSet 
                END (* WITH *) 
              END (* IF *) 
            ; ResultInfo . RiReplaceRight := FALSE 
            ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSame 
              THEN (* Make this also the left side of the new seam. *) 
                WNonleafRow . MsNlrObjRef := DownInfo . DiRightObjRef 
              ; WNonleafRow . MsNlrKindSet 
                  := DownInfo . DiRightKindSet 
                     + MergeState . MsWaitingRightmostKindSet
              ; TYPECASE WNonleafRow . MsNlrObjRef 
                OF NULL => 
                | EstHs . EstRefTyp ( TEstRef ) 
                => TEstRef . EstChildKindSet 
                     := TEstRef . EstChildKindSet 
                        + MergeState . MsWaitingRightmostKindSet
                ELSE 
                END (* TYPECASE *)  
              ; ResultInfo . RiReplaceLeft := FALSE 
              END (* IF *) 
            END (* IF *) 

          (* Handle left side: *) 
          ; IF DownInfo . DiAdjKind # AdjKindTyp . AdjKindSame 
            THEN (* A distinct left side exists. *)  
              IF LLeftMustChange 
                 OR LForceRepack 
                    AND DownInfo . DiAdjKind = AdjKindTyp . AdjKindAdj 
              THEN 
                IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindSep 
                THEN 
                  MslNlFlush ( WNonleafRow ) 
                END (* IF *) 
              ; LLeftElemNo 
                  := LFromElemNo - ORD ( LResultInfo . RiReplaceLeft ) 
              ; IF LLeftElemNo >= 0 
                THEN 
                  LElemNo := 0 
                ; LOrigToChildNo 
                    := FromChildNo 
                       - DownInfo . DiFromChildNo 
                       + LLeftMaxChildCt 
                  (* ^OrigToChildNo of entire left node. *) 
                ; EstUtil . GetKTreeSliceEdgeInfoPair 
                    ( MergeState . MsLang 
                    , LLeftNonleafArray [ LElemNo ] . NleChildRef 
                    , LSliceEdgeInfoPair 
                    ) 
                ; IF DownInfo . DiAdjKind = AdjKindTyp . AdjKindAdj  
                  THEN 
                    MslNlReincludeListSepTokInfo 
                      ( LeftGapEdgeInfo 
                          := LSliceEdgeInfoPair . SeiRightEdgeInfo 
                      , RightGapEdgeInfo 
                          := WNonleafRow . MsNlrSliceEdgeInfoPair 
                             . SeiLeftEdgeInfo 
                      , RightGapOrigChildNo := LOrigToChildNo 
                      , NonleafRow := WNonleafRow 
                      ) 
                  END (* IF *) 
                ; LOOP 
                    WITH WLeftNonleafElem = LLeftNonleafArray [ LElemNo ] 
                    DO MslNlPrepend 
                         ( WNonleafRow 
                         , WLeftNonleafElem . NleChildRef 
                         , LSliceEdgeInfoPair 
                         , WLeftNonleafElem . NleKindSet 
                         , IsLeftOfSeam := TRUE 
                         ) 
                    ; LCumChildCt := WLeftNonleafElem . NleCumChildCt 
                    END (* WITH WLeftNonleafElem *) 
                  ; IF LElemNo >= LLeftElemNo 
                    THEN 
                      EXIT 
                    ELSE 
                      INC ( LElemNo ) 
                    ; WITH WLeftNonleafElem = LLeftNonleafArray [ LElemNo ] 
                      DO EstUtil . GetKTreeSliceEdgeInfoPair 
                           ( MergeState . MsLang 
                           , WLeftNonleafElem . NleChildRef 
                           , LSliceEdgeInfoPair 
                           ) 
                      ; MslNlReincludeListSepTokInfo 
                          ( LeftGapEdgeInfo 
                              := LSliceEdgeInfoPair . SeiRightEdgeInfo 
                          , RightGapEdgeInfo 
                              := WNonleafRow . MsNlrSliceEdgeInfoPair 
                                 . SeiLeftEdgeInfo 
                          , RightGapOrigChildNo 
                              := LOrigToChildNo - LCumChildCt 
                          , NonleafRow := WNonleafRow 
                          ) 
                      END (* WITH WLeftNonleafElem *) 
                    END (* IF *) 
                  END (* LOOP *) 
                END (*IF *) 
              ; ResultInfo . RiReplaceLeft := TRUE 
              ELSE (* Pass Left up unchanged. *) 
                MslNlFlush ( WNonleafRow ) 
              ; WNonleafRow . MsNlrObjRef := DownInfo . DiLeftObjRef 
              ; WNonleafRow . MsNlrKindSet := DownInfo . DiLeftKindSet 
              ; ResultInfo . RiReplaceLeft := FALSE 
              END (* IF *) 
            END (* IF *)
          ; Assert 
              ( ( LResultInfo . RiLeftRef = NIL ) = LResultInfo . RiReplaceLeft 
              , AFT . A_MslNonleaf_RiLeftRef
              ) 
          ; IF LResultInfo . RiReplaceLeft  
            THEN (* Level below is unpacked. *) 
              Assert
                ( WNonleafRow . MsNlrObjRef = NIL (* This level is unpacked too. *) 
                , AFT . A_MslNonleaf_UnpackedInvariant  
                ) 
            ; IF WNonleafRow . MsNlrElemCt >= EstHs . NonleafArrayElemCtMax 
              THEN (* Need one more space for an eventual pointer from below. *) 
                Assert 
                  ( DownInfo . DiAdjKind # AdjKindTyp . AdjKindSep 
                  , AFT . A_MslNonleaf_FlushingSeparateLeft
                  ) 
              ; MslNlFlush ( WNonleafRow ) 
              END (* IF *) 
            ELSE
              Assert 
                ( WNonleafRow . MsNlrObjRef # NIL 
                  OR LResultInfo . RiLeftRef 
                     = WNonleafRow . MsNlrElems 
                         [ WNonleafRow . MsNlrElemCt - 1 ] . NleChildRef  
                , AFT . A_MslNonleaf_PackedInvariant  
                ) 
            END (* IF *) 
          ; ResultInfo . RiLeftRef := WNonleafRow . MsNlrObjRef 
          ; ResultInfo . RiLeftKindSet := WNonleafRow . MsNlrKindSet  
          END (* WITH WNonleafRow *) 
        END (* Block *) 
      END MslNonleaf 

  ; BEGIN (* MergeSlice *) 
      VAR LDownInfo : DownInfoTyp 
    ; VAR LResultInfo : ResultInfoTyp 

    ; BEGIN (* Block MergeSlice body. *) 
        IF EstRef # NIL AND FromChildNo < ToChildNo 
        THEN 
          MslEstHeight := EstRef . EstHeight 
        ; MslMsStartingHeight := MergeState . MsHeight 
        ; LDownInfo . DiLeftObjRef := EstRef 
        ; LDownInfo . DiLeftKindSet := EstRef . EstChildKindSet 
        ; LDownInfo . DiFromChildNo := FromChildNo 
        ; LDownInfo . DiRightObjRef := EstRef 
        ; LDownInfo . DiRightKindSet := EstRef . EstChildKindSet 
        ; LDownInfo . DiThruChildNo := ToChildNo - 1 
        ; LDownInfo . DiHeight := EstRef . EstHeight 
        ; LDownInfo . DiAdjKind := AdjKindTyp . AdjKindSame 
        ; IF EstRef . EstHeight > 1 
          THEN 
            MslNonleaf ( LDownInfo , LResultInfo ) 
          ELSE 
            MslLeaf ( LDownInfo , LResultInfo ) 
          END (* IF *) 
        ; Assert 
            ( TRUE OR NOT LResultInfo . RiReplaceLeft 
            , AFT . A_MergeSliceReplaceLeftAtTopLevel 
            ) 
        ; Assert 
            ( LResultInfo . RiSeamInsertionRef1 = NIL 
            , AFT . A_MergeSliceInsertionAtTopLevel 
            ) 
        ; PropagateChangesUpward 
            ( MergeState 
            , EstRef . EstHeight 
            , ReplaceFarRight := LResultInfo . RiReplaceFarRight 
            , InsertionRef := LResultInfo . RiSeamInsertionRef2 
            , InsertionKindSet := LResultInfo . RiSeamInsertionKindSet2 
            ) 
        ; MergeState . MsWaitingRightmostKindSet 
            := EstHs . EstChildKindSetEmpty
        END (* IF *) 
      (* Save things for next time. *) 
      ; MergeState . MsLastEstRef := EstRef
(* FIXME: ^This can't be right, if FromChildNo > 0 *)
      ; MergeState . MsLastFromChildNo := FromChildNo 
      ; MergeState . MsLastToChildNo := ToChildNo 
      END (* Block *) 
    END MergeSlice 

(* VISIBLE: *) 
; PROCEDURE FinishMerge 
    ( MergeState : MergeStateTyp 
    ; ResultEstNodeKind : EstHs . EstNodeKindTyp 
    ; VAR ResultEstRef : EstHs . EstRefTyp 
    ) 
  RAISES { AssertionFailure }   

  = VAR LHeight : EstHs . KTreeHeightTyp 
  ; VAR LInsertionRef : EstHs . KTreeRefTyp 
  ; VAR LKindSet : EstHs . EstChildKindSetTyp 
  ; VAR LResultRef : EstHs . KTreeRefTyp 
  ; VAR LSliceEdgeInfoPair : EstHs . SliceEdgeInfoPairTyp 

  ; BEGIN (* FinishMerge *) 

    (* Delete any empty levels at top. *) 
      LOOP 
        IF MergeState . MsHeight <= 1 
        THEN EXIT 
        ELSE 
          WITH WNonleafRow 
               = MergeState . MsNonleafRowRefs [ MergeState . MsHeight ] ^ 
          DO IF WNonleafRow . MsNlrObjRef = NIL 
                AND WNonleafRow . MsNlrElemCt = 0 
             THEN
               DEC ( MergeState . MsHeight ) 
             ELSE 
               EXIT
             END (* IF *) 
          END (* WITH *)  
        END (* IF *) 
      END (* LOOP *) 
    ; IF MergeState . MsHeight = 1 
         AND MergeState . MsLeafRow . MsLrObjRef = NIL 
         AND MergeState . MsLeafRow . MsLrElemCt = 0 
      THEN
        DEC ( MergeState . MsHeight ) 
      END (* IF *) 
    ; IF MergeState . MsHeight = 0 
      THEN (* Construct empty node *) 
        LResultRef 
          := NEW 
               ( EstHs . EstLeafRefTyp 
               , EstNodeKind := ResultEstNodeKind 
               , EstTok := MergeState . MsEstTok 
               , KTreeElemCt := 0 
               , EstChildKindSet := EstHs . EstChildKindSetEstChildNonNIL
               , EstHeight := 1 
               , EstLeftTok := MergeState . MsEstTok 
               , EstRightTok := MergeState . MsEstTok 
               ) 
      ; IF MergeState . MsEstRefToInheritFrom = NIL 
        THEN (* Use syntactic token count computed for the new node. *) 
          LResultRef . KTreeSyntTokCt := MergeState . MsTrailingSyntTokCt 
        ELSE (* Take it from the node we inherit from. *) 
          LResultRef . KTreeSyntTokCt 
            := MergeState . MsEstRefToInheritFrom . KTreeSyntTokCt 
        END (* IF *) 
      ; LResultRef . KTreeWidthInfo := MergeState . MsTrailingWidthInfo 
      ELSE (* Construct nonempty node. *)  
        LHeight := 1 

      (* Skip rows already in allocated nodes. *) 
      ; LInsertionRef := NIL 
      ; IF MergeState . MsLeafRow . MsLrObjRef # NIL 
        THEN 
          Assert 
            ( 1 < MergeState . MsHeight 
            , AFT . A_FinishMergeObjRefAtTopLeafLevel 
            ) 
(* CHECK: ^Why can't this happen if we merge a whole slice? *) 
        ; LHeight := 2 
        ; WHILE MergeState . MsNonleafRowRefs [ LHeight ] ^ . MsNlrObjRef # NIL 
          DO Assert 
               ( LHeight < MergeState . MsHeight 
               , AFT . A_FinishMergeObjRefAtTopNonleafLevel 
               ) 
          ; INC ( LHeight ) 
          END (* WHILE *) 
        END (* IF *) 

      (* Flush lowest unflushed row, below top row *) 
      ; IF LHeight < MergeState . MsHeight 
        THEN 
          IF LHeight = 1 
          THEN 
            FlushLeafRow ( MergeState , LInsertionRef , LKindSet ) 
          ELSE 
            FlushNonleafRow 
              ( MergeState . MsNonleafRowRefs [ LHeight ] ^  
              , LInsertionRef 
              , LKindSet 
              ) 
          END (* IF *) 
        ; INC ( LHeight ) 
        END (* IF *) 

      (* Move up, but staying below the top row, prepending new 
         pointer from below and then finishing row. *) 
      ; WHILE LHeight < MergeState . MsHeight 
        DO EstUtil . GetKTreeSliceEdgeInfoPair 
             ( MergeState . MsLang , LInsertionRef , LSliceEdgeInfoPair ) 
        ; PrependToNonleafRow 
            ( MergeState 
            , MergeState . MsNonleafRowRefs [ LHeight ] ^ 
            , LInsertionRef 
            , LSliceEdgeInfoPair 
            , LKindSet 
            , IsLeftOfSeam := FALSE 
            ) 
        ; FlushNonleafRow 
            ( MergeState . MsNonleafRowRefs [ LHeight ] ^ 
            , LInsertionRef 
            , LKindSet 
            ) 
        ; INC ( LHeight ) 
        END (* WHILE *) 

      (* Prepend to top row. *) 
      ; IF LInsertionRef # NIL 
        THEN 
          EstUtil . GetKTreeSliceEdgeInfoPair 
            ( MergeState . MsLang , LInsertionRef , LSliceEdgeInfoPair ) 
        ; PrependToNonleafRow 
            ( MergeState 
            , MergeState . MsNonleafRowRefs [ LHeight ] ^ 
            , LInsertionRef 
            , LSliceEdgeInfoPair 
            , LKindSet 
            , IsLeftOfSeam := FALSE 
            ) 
        END (* IF *) 

      (* Allocate and fill topmost node. *) 
      ; IF MergeState . MsHeight = 1 
        THEN (* It's a leaf node. *)  
          FlushLeadingTokInfoToLeaf ( MergeState ) 
        ; LResultRef 
            := NEW 
                 ( EstHs . EstLeafRefTyp 
                 , EstNodeKind := ResultEstNodeKind 
                 , EstTok := MergeState . MsEstTok 
                 , EstLeftTok := MergeState . MsLeafRow . MsLrLeftTok 
                 , EstRightTok := MergeState . MsLeafRow . MsLrRightTok 
                 , EstHeight := MergeState . MsHeight 
                 , KTreeElemCt := MergeState . MsLeafRow . MsLrElemCt 
                 , EstChildKindSet 
                     := MergeState . MsLeafRow . MsLrKindSet 
                        + EstHs . EstChildKindSetEstChildNonNIL 
                 , EstLeafArrayRef 
                     := EstHs . RefToNewLeafArray 
                          ( MergeState . MsLeafRow . MsLrElemCt 
                          , MergeState . MsLeafRow . MsLrElems 
                          ) 
                 ) 
        ; IF MergeState . MsEstRefToInheritFrom = NIL 
          THEN (* Use syntactic token count computed for the new node. *) 
            LResultRef . KTreeSyntTokCt 
              := LbeStd . LimitedTokCtSum 
                   ( MergeState . MsLeafRow . MsLrSyntTokCt 
                   , MergeState . MsTrailingSyntTokCt 
                   ) 
          ELSE (* Take it from the node we inherit from. *) 
            LResultRef . KTreeSyntTokCt 
              := MergeState . MsEstRefToInheritFrom . KTreeSyntTokCt 
          END (* IF *) 
        ; LResultRef . KTreeWidthInfo 
            := EstUtil . WidthInfoCat 
                 ( MergeState . MsLeafRow . MsLrWidthInfo 
                 , MergeState . MsTrailingNeedsSep 
                 , MergeState . MsTrailingWidthInfo 
                 ) 
        ; LResultRef . KTreeEstChildCtLeftOfNl 
            := MergeState . MsLeafRow . MsLrEstChildCtLeftOfNl 
        ELSE (* It's a nonleaf node. *)
          WITH WNonleafRow = MergeState . MsNonleafRowRefs [ LHeight ] ^ 
          DO 
            FlushLeadingTokInfoToNonleaf ( MergeState , WNonleafRow ) 
          ; LResultRef 
              := NEW 
                   ( EstHs . EstNonleafRefTyp 
                   , EstNodeKind := ResultEstNodeKind 
                   , EstTok := MergeState . MsEstTok 
                   , EstLeftTok 
                       := WNonleafRow . MsNlrSliceEdgeInfoPair 
                          . SeiLeftEdgeInfo . EiTok 
                   , EstRightTok 
                       := WNonleafRow . MsNlrSliceEdgeInfoPair 
                          . SeiRightEdgeInfo . EiTok 
                   , EstHeight := MergeState . MsHeight 
                   , KTreeElemCt := WNonleafRow . MsNlrElemCt 
                   , EstChildKindSet 
                       := WNonleafRow . MsNlrKindSet 
                          + EstHs . EstChildKindSetEstChildNonNIL 
                   , EstNonleafArrayRef 
                       := EstHs . RefToNewNonleafArray 
                            ( WNonleafRow . MsNlrElemCt 
                            , WNonleafRow . MsNlrElems 
                            ) 
                   ) 
          ; IF MergeState . MsEstRefToInheritFrom = NIL 
            THEN (* Use syntactic token count computed for the new node. *) 
              LResultRef . KTreeSyntTokCt 
                := LbeStd . LimitedTokCtSum 
                     ( WNonleafRow . MsNlrSyntTokCt 
                     , MergeState . MsTrailingSyntTokCt 
                     ) 
            ELSE (* Take it from the node we inherit from. *) 
              LResultRef . KTreeSyntTokCt 
                := MergeState . MsEstRefToInheritFrom . KTreeSyntTokCt 
            END (* IF *) 
          ; LResultRef . KTreeWidthInfo 
              := EstUtil . WidthInfoCat 
                   ( WNonleafRow . MsNlrWidthInfo 
                   , MergeState . MsTrailingNeedsSep 
                   , MergeState . MsTrailingWidthInfo 
                   ) 
          ; LResultRef . KTreeEstChildCtLeftOfNl 
              := WNonleafRow . MsNlrEstChildCtLeftOfNl 
          END (* WITH WNonleafRow *) 
        END (* IF *) 
      END (* IF *) 

    ; ResultEstRef := LResultRef 
    ; Assert 
        ( MergeState . MsWaitingRightmostKindSet 
          = EstHs . EstChildKindSetEmpty 
        , AFT . A_FinishMerge_WaitingRightmostKindSet 
        ) 
    ; MergeState . MsLink := FreePool 
    ; FreePool := MergeState
    ; INC ( FreePoolCt ) 
    ; MergeState := NIL (* Decrease chances of client reusing. *) 
    END FinishMerge 

; BEGIN (* EstBuild *) 
    FreePool := NIL 
  ; FreePoolCt := 0 
  ; AllocatedCt := 0 
  END EstBuild 
. 
