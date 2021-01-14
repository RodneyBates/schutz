
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE TravUtil 

(* Common procedures used by several tree traversers. *) 

; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT EstHs 
; IMPORT SharedStrings 
; IMPORT Marks 

; FROM Assertions IMPORT AssertionFailure 

; PROCEDURE IndentPos 
    ( Lang : LbeStd . LangTyp 
    ; EstIndentPos : LbeStd . LimitedCharNoTyp 
    ; IndentCode : LangUtil . IndentCodeTyp 
    ) 
    : LbeStd . LimitedCharNoTyp 
  (* The indent position to be used for floating items. *) 

; PROCEDURE PosForTok 
    ( Lang : LbeStd . LangTyp 
    ; FmtKind : LangUtil . FmtKindTyp 
    ; ModTextIsToLeftOnLine : BOOLEAN 
    ; CharPos : LbeStd . LimitedCharNoSignedTyp 
    ; TokIndentPos : LbeStd . LimitedCharNoTyp 
    ; IndentCode : LangUtil . IndentCodeTyp 
    ; PrevTok : LbeStd . TokTyp 
    ; Tok : LbeStd . TokTyp 
    ) 
    : LbeStd . LimitedCharNoTyp 

(* Traversing Est children: *) 

; TYPE EstTravInfoTyp 
    = RECORD 
        EtiNodeRef : LbeStd . EstRootTyp := NIL 
      ; EtiParentRef : EstHs . EstRefTyp := NIL 
        (* ^= EtiNodeRef if ISTYPE ( EtiNodeRef , EstHs . EstRefTyp ) 
                         else NIL. *) 
      ; EtiStringRef : SharedStrings . T := NIL 
        (* ^= EtiNodeRef if ISTYPE ( EtiNodeRef , SharedSrings . T ) 
                         else NIL. *) 
      ; EtiChildCt : LbeStd . EstChildNoTyp := 0 
      ; EtiChildNo : LbeStd . EstChildNoTyp := LbeStd . EstChildNoNull  
      ; EtiChildRelNodeNo : LbeStd . EstNodeNoTyp := LbeStd . EstNodeNoNull
        (* ^The node number of the root of the current child, 
            relative to the Est rooted at EtiParentRef. *) 
      ; EtiChildLeafElem : EstHs . LeafElemTyp := EstHs . LeafElemNull 
      ; EtiAbsNodeNo : LbeStd . EstNodeNoTyp := LbeStd . EstNodeNoNull
      ; EtiChildFmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull  
      ; EtiIsOptSingletonList : BOOLEAN := FALSE 
      END (* RECORD  EstTravInfoTyp *) 
(* CONSIDER: Can we eliminate some of these default values?  EtiStringRef
             and EtiChildCt are really needed to distinguish a valid
             singleton-list optmized value, but the rest are questionable. 
*) 

; PROCEDURE InitEstTravInfo 
    ( VAR EstTravInfo : EstTravInfoTyp 
    ; EstNodeRef : LbeStd . EstRootTyp 
    ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ; IsOptSingletonList : BOOLEAN := FALSE 
    ) 
  RAISES { AssertionFailure } 
  (* Sets EtiNodeRef , EtiParentRef , EtiStringRef , EtiChildCt.
     and EtiIsOptSingletonList. 
  *) 
(* TODO: When the calls are gone, take this out of this interface. *) 

; PROCEDURE InitEstTravInfoFwd 
    ( VAR EstTravInfo : EstTravInfoTyp 
    ; EstNodeRef : LbeStd . EstRootTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ) 
  RAISES { AssertionFailure } 
  (* For an element of an optimized singleton list, trump up a suitable
     EstTravInfo.  Otherwise, set it up for forward traversal of the
     children of EstNodeRef with AbsNodeNo.
  *) 

; PROCEDURE InitEstTravInfoBwd 
    ( VAR EstTravInfo : EstTravInfoTyp 
    ; EstNodeRef : LbeStd . EstRootTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ) 
  RAISES { AssertionFailure } 
  (* For an element of an optimized singleton list, trump up a suitable
     EstTravInfo.  Otherwise, set it up for backward traversal of the
     children of EstNodeRef with AbsNodeNo.
  *) 

; PROCEDURE InitToChildContainingNodeNo 
    ( VAR EstTravInfo : EstTravInfoTyp 
    ; EstNodeRef : LbeStd . EstRootTyp 
    ; EstRelNodeNo : LbeStd . EstNodeNoTyp 
      (* ^Node number relative to EstNodeRef. *) 
    ; KindSet : EstHs . EstChildKindSetTyp := EstHs . EstChildKindSetEmpty  
    ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ) 
  RAISES { AssertionFailure }  

; PROCEDURE GetLMEstChild 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp ) 
  RAISES { AssertionFailure } 
  (* Set EtiChildCt from EtiParentRef and EtiIsOptSingletonList.  
     Set EtiChildNo to the leftmost child number (always zero).  
     If this child exists, set EtiChildLeafElem and EtiChildFmtNo accordingly. 
  *) 

; PROCEDURE GetRMEstChild 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp ) 
  RAISES { AssertionFailure } 
  (* Set EtiChildCt from EtiParentRef and EtiIsOptSingletonList.  
     Set EtiChildNo to the rightmost child number.  
     If this child exists, set EtiChildLeafElem and EtiChildFmtNo accordingly. 
  *) 

; PROCEDURE SetToIthChild 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp 
    ; I : LbeStd . EstChildNoTyp 
    ) 
  RAISES { AssertionFailure } 
  (* PRE: EstTravInfo is initialized. *) 
  (* Move to Ith child. Set ChildNo, ChildRelNodeNo, ChildLeafElem, 
     and ChildFmtNo. 
  *) 

; PROCEDURE SetToChildContainingNodeNo 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp 
    ; EstRelNodeNo : LbeStd . EstNodeNoTyp 
      (* ^Node number relative to the parent of EstTravInfo. *) 
    ) 
  RAISES { AssertionFailure } 

; PROCEDURE IncEstChild 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp ) 
  RAISES { AssertionFailure } 
  (* Increase ChildNo. If there is another child, set 
     ChildRelNodeNo, ChildLeafElem and ChildFmtNo accordingly. 
  *) 

; PROCEDURE DecEstChild 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp ) 
  RAISES { AssertionFailure } 
  (* Decrease ChildNo. If there is an earlier child, set 
     ChildRelNodeNo, ChildLeafElem and ChildFmtNo accordingly. 
  *) 

; PROCEDURE SetToNextInKindSet 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp 
    ; StartChildNo : LbeStd . EstChildNoTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ) 
  RAISES { AssertionFailure } 

; PROCEDURE SetToPrevInKindSet 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp 
    ; StartChildNo : LbeStd . EstChildNoTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ) 
  RAISES { AssertionFailure } 

(* Utility: *) 

; PROCEDURE ChildIndentPositions 
    ( Lang : LbeStd . LangTyp  
    ; FsEstChildNodeRef : LangUtil . FsNodeRefTyp 
      (* ^Must have FsKindEstChildOfFixed or FsKindEstChildOfList. *) 
    ; EstIndentPos1 : LbeStd . LimitedCharNoTyp 
    ; EstIndentPosN : LbeStd . LimitedCharNoTyp 
    ; VAR ChildIndentPos1 : LbeStd . LimitedCharNoTyp 
    ; VAR ChildIndentPosN : LbeStd . LimitedCharNoTyp 
    ; IsFirstLine : BOOLEAN := FALSE 
    ) 
  RAISES { AssertionFailure } 

; PROCEDURE IsInFirstLine  
    ( FsNodeRef : LangUtil . FsNodeRefTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ; Bwd : BOOLEAN 
    )  
  : BOOLEAN 
  RAISES { AssertionFailure } 
  (* "First line" in the layout sense that does not consider New lines 
     inserted by modifiers. 
  *) 

; PROCEDURE CheckModFwd 
    ( READONLY EstTravInfo : EstTravInfoTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR (* OUT *) RelevantModRef : LbeStd . EstRootTyp 
    ; VAR (* OUT *) ModDelIsFinished : BOOLEAN 
    )
  RAISES { AssertionFailure }  
  (* For forward traversals. 
     RelevantModRef # NIL iff the next child is a mod for this fs item, 
     in which case it points to the relevant mod. 
     If it is a delete mod, then ModDelIsFinished means the Fs item is 
     the rightmost one it covers. 
  *) 

; PROCEDURE DoCondFmtFwd 
    ( Lang : LbeStd . LangTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
      (* ^Predicate is evaluated on the current child herein. *) 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  : BOOLEAN
  RAISES { AssertionFailure }  
  (* When traversing forwards and FsNodeRef is a CondFmtNode, 
     ascertain whether the conditional format insertions 
     are to be done. *) 

; PROCEDURE DoCondFmtBwd 
    ( Lang : LbeStd . LangTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
      (* ^Predicate is evaluated on the current child herein. *) 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  : BOOLEAN 
  RAISES { AssertionFailure } 
  (* When traversing backwards and FsNodeRef is a CondFmtNode, 
     ascertain whether the conditional format insertions 
     are to be done. *) 

; PROCEDURE GetDescendantWithNodeNo 
    ( Root : LbeStd . EstRootTyp 
    ; EstNodeNo : LbeStd . EstNodeNoTyp 
      (* ^Node number relative to Root. *) 
    ; VAR EstRef : LbeStd . EstRootTyp 
    ; VAR KindSet : EstHs . EstChildKindSetTyp 
    ; VAR IsOptSingletonList : BOOLEAN 
    ) 
  RAISES { AssertionFailure } 
  (* IsOptSingletonList means EstNodeNo is for an optimized-away singleton list,
     and EstRef and KindSet denote its one element, whose actual NodeNo is 
     EstNodeNo + 1.
  *) 

; PROCEDURE NodeCtOfDescendantWithNodeNo 
    ( Root : LbeStd . EstRootTyp 
    ; EstNodeNo : LbeStd . EstNodeNoTyp 
      (* ^Node number relative to Root. *) 
    ) 
  : LbeStd . EstNodeNoTyp 
  RAISES { AssertionFailure } 

; PROCEDURE DescendCondFmt 
    ( Lang : LbeStd . LangTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; FmtNo : EstHs . FmtNoTyp
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ; VAR Predicate : BOOLEAN 
    ; VAR FsChildNo : LangUtil . FsChildNoTyp 
    ; Bwd : BOOLEAN := FALSE
    ) 
  RAISES { AssertionFailure } 
  (* When descending through a CondFmt node, call this to check the predicate
     and find the appropriate immediate child number. *) 

; PROCEDURE FsLeafRefOfFmtNo 
    ( Lang : LbeStd . LangTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; FmtNo : EstHs . FmtNoTyp 
    ; ChildEstRef : LbeStd . EstRootTyp
    ; ChildKindSet : EstHs . EstChildKindSetTyp 
    )
  : LangUtil . FsNodeRefTyp
  RAISES { AssertionFailure } 
  (* Return the Fs leaf that has FmtNo.  Will evaluate predicates if
     necessary, to get to the correct alternative FsEstChild. 
  *)
(* TODO: ^Doesn't this belong somewhere else, associated with Fs trees? *) 

; PROCEDURE IndentPosOfBolTokMark 
    ( Lang : LbeStd . LangTyp 
    ; EstRoot : LbeStd . EstRootTyp 
    ; TokMark : Marks . TokMarkTyp 
    ) 
  : LbeStd . LimitedCharNoTyp 
  RAISES { AssertionFailure } 
  (* PRE: TokMark denotes a new line. *) 
  (* The actual indent position for the text beginning on a new line at the 
     point TokMark denotes, in the tree rooted at EstRoot.  
  *) 

; PROCEDURE GetFsEstDescendant 
    ( FsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR ParentRef : LangUtil . FsNodeRefTyp 
    ; VAR DescendantRef : LangUtil . FsNodeRefTyp 
    ; VAR FromFsChildNo : LangUtil . FsChildNoTyp 
    ) 
(* TODO: ^Doesn't this belong somewhere else, associated with Fs trees? *) 

; PROCEDURE FmtKindForEstDescending 
    ( FsKind : LangUtil . FsKindTyp 
      (* ^Must be in LangUtil . FsKindSetFsRoot *) 
    ; ParentFmtKind : LangUtil . FmtKindTyp 
    ; FirstLineIndentPos : LbeStd . LimitedCharNoTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; READONLY StartMark : Marks . TokMarkTyp 
      (* ^Used only if StartMarkIsKnownNl. *) 
    ; StartMarkIsKnownNl : BOOLEAN 
      (* ^FALSE merely means we don't know. *)
    ) 
  : LangUtil . FmtKindTyp 
  RAISES { AssertionFailure } 
  (* Use when descending into an Est, toward a line mark. *)

; PROCEDURE FmtKindForEstTraversing 
    ( Lang : LbeStd . LangTyp 
    ; CharPos : LbeStd . LimitedCharNoSignedTyp 
      (* ^Can be unknown, and this could propagate to result. *)  
    ; ModTextIsToLeftOnLine : BOOLEAN 
      (* ^Irrelevant if CharPos = LbeStd . LimitedCharNoUnknown. *) 
    ; PrevTok : LbeStd . TokTyp 
    ; FsKind : LangUtil . FsKindTyp 
      (* ^Must be in LangUtil . FsKindSetFsRoot *) 
    ; ParentFmtKind : LangUtil . FmtKindTyp 
    ; FirstLineIndentPos : LbeStd . LimitedCharNoTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ) 
  : LangUtil . FmtKindTyp 
  RAISES { AssertionFailure } 
  (* Use when info about the current line is known. *) 

; PROCEDURE FmtKindForFsSubtreeDescending   
    ( Lang : LbeStd . LangTyp 
    ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
      (* PRE: RootFsNodeRef.FsKind IN TravUtil.FsKindSetFsRoot. *)
    ; SubtreeFsNodeRef : LangUtil . FsNodeRefTyp 
      (* PRE: SubtreeFsNodeRef.FsKind IN TravUtil.FsKindSetSubtree. *)
    ; ParentFmtKind : LangUtil . FmtKindTyp 
    ; IndentPosIfVert : LbeStd . LimitedCharNoTyp 
      (* ^Within the parent Est, assuming it is vertical. *) 
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ; READONLY StartMark : Marks . TokMarkTyp 
      (* ^Used only if StartMarkIsKnownNl. *) 
    ; StartMarkIsKnownNl : BOOLEAN 
      (* ^FALSE merely means we don't know. *)
    ) 
  : LangUtil . FmtKindTyp 
  RAISES { AssertionFailure } 
  (* Use when descending into an FsSubtree, preferably toward a line mark. *)

; PROCEDURE FmtKindForFsSubtreeTraversing  
    ( Lang : LbeStd . LangTyp 
    ; CharPos : LbeStd . LimitedCharNoSignedTyp 
    ; ModTextIsToLeftOnLine : BOOLEAN 
    ; PrevTok : LbeStd . TokTyp 
    ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
      (* PRE: RootFsNodeRef.FsKind IN TravUtil.FsKindSetFsRoot. *)
    ; SubtreeFsNodeRef : LangUtil . FsNodeRefTyp 
      (* PRE: SubtreeFsNodeRef.FsKind IN TravUtil.FsKindSetSubtree. *)
    ; ParentFmtKind : LangUtil . FmtKindTyp 
    ; CurrentLineIndentPos : LbeStd . LimitedCharNoTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ; VAR (* IN OUT *) LastFmtNoOnLine : EstHs . FmtNoTyp 
    ; VAR (* IN OUT *) EstListChildrenToPass : LbeStd . EstChildNoTyp 
    ) 
  : LangUtil . FmtKindTyp 
  RAISES { AssertionFailure } 
  (* Use when traversing into the Fs subtree from one end or the other,
     when info about the current line is known. *) 
(* CHECK: Works Bwd?  Do we need it to? *) 

; PROCEDURE PassEstListChild 
    ( VAR (* IN OUT *) EstListChildrenToPass : LbeStd . EstChildNoTyp ) 
  (* Call this when passing an EstListChild *) 

; PROCEDURE DoTakeLineBreak 
    ( Lang : LbeStd . LangTyp 
    ; CharPos : LbeStd . LimitedCharNoSignedTyp 
    ; ModTextIsToLeftOnLine : BOOLEAN 
    ; PrevTok : LbeStd . TokTyp 
    ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
    ; LineBreakFsNodeRef : LangUtil . FsNodeRefTyp    
    ; ParentFmtKind : LangUtil . FmtKindTyp 
    ; CurrentLineIndentPos : LbeStd . LimitedCharNoTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
      (* ^Of the line we are in. *) 
    ; VAR (* IN OUT *) LastFmtNoOnLine : EstHs . FmtNoTyp 
    ; VAR (* IN OUT *) EstListChildrenToPass : LbeStd . EstChildNoTyp 
    ) 
  : BOOLEAN 
  RAISES { AssertionFailure }
  (* The line break must be undeleted. 
     This function has a SIDE EFFECT.  If the line break is not to be taken,
     notes that it has been passed, which must happen exactly once, so that
     future calls on DoTakeLineBreak will work. *)  

; PROCEDURE AssertFwdNoLostFixedChild 
    ( FsNodeRef : LangUtil . FsNodeRefTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ) 
  RAISES { AssertionFailure } 

; PROCEDURE AssertBwdNoLostFixedChild 
    ( FsNodeRef : LangUtil . FsNodeRefTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ) 
  RAISES { AssertionFailure }

; PROCEDURE NodeNoOfNodeRef 
    ( RootNodeRef : LbeStd . EstRootTyp ; SoughtNodeRef : REFANY ) 
  : LbeStd . EstNodeNoTyp
  (* LbeStd.EstNodNoNull, if not found. *) 
  RAISES { AssertionFailure } 
  
; END TravUtil 
. 
