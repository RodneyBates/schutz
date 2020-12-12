
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE EstUtil 

(* Many collected lowish-level utilities for manipulating Est nodes. *) 

; IMPORT PortTypes 
; IMPORT LbeStd 
; IMPORT EstHs 
; IMPORT LangUtil 
; IMPORT ModHs 

; FROM Assertions IMPORT AssertionFailure 

(* Information about Est nodes of any subtype: *) 

; PROCEDURE HasSyntErrors ( NodeRef : LbeStd . EstRootTyp ) : BOOLEAN 

; PROCEDURE EstTok ( NodeRef : LbeStd . EstRootTyp ) 
  : LbeStd . TokTyp 
  RAISES { AssertionFailure } 
  (* If NodeRef is a string object, returns its token. 
     Otherwise, the EstTok stored in the node. 
  *) 

; PROCEDURE FsRuleForEstNode  
    ( Lang : LbeStd . LangTyp ; NodeRef : LbeStd . EstRootTyp ) 
  : LangUtil . FsNodeRefTyp 
  RAISES { AssertionFailure } 
  (* The FsNodeRef of the root of the format tree for an Est node,
     or an FsNode of FsKindAstString, for an AstString. 
  *) 

; PROCEDURE EstChildKindSet ( NodeRef : LbeStd . EstRootTyp ) 
  : EstHs . EstChildKindSetTyp 
  RAISES { AssertionFailure } 
  (* WARNING: EstChildKindContainsInsertionRepair and 
              EstChildKindContainsSyntMod can't be ascertained from
              the node itself, for ModDel and AstString, nor can
              EstChildKindOptSingletonList for an Est node or AstString.   
              These are omitted in the result. 
  *)  

; PROCEDURE EstIsPlaceholder 
    ( Lang : LbeStd . LangTyp ; NodeRef : LbeStd . EstRootTyp ) 
  : BOOLEAN 
  RAISES { AssertionFailure } 
  (* Either from a SharedString . T or an Est node. *) 

; PROCEDURE EstChildCt 
    ( NodeRef : LbeStd . EstRootTyp ) : LbeStd . EstChildNoTyp 
  (* ^Also works on NIL, giving zero *) 

; PROCEDURE EstNodeCt 
    ( NodeRef : LbeStd . EstRootTyp ) : LbeStd . EstNodeNoTyp 
  (* ^Also works on NIL, giving 1 *) 

; PROCEDURE IsModTok ( NodeRef : LbeStd . EstRootTyp ) : BOOLEAN 

; PROCEDURE IsZeroWidthModText ( ItemRef : LbeStd . EstRootTyp ) : BOOLEAN

; PROCEDURE EstMiscInfo 
    ( Lang : LbeStd . LangTyp ; ItemRef : LbeStd . EstRootTyp ) 
  : EstHs . EstMiscInfoTyp 
  RAISES { AssertionFailure }
(* MAYBE: Make this result a VAR parameter? *)

; PROCEDURE EstEdgeKind  
    ( Lang : LbeStd . LangTyp ; ItemRef : LbeStd . EstRootTyp ) 
  : EstHs . EdgeKindTyp 
  RAISES { AssertionFailure } 

; PROCEDURE EstIthChildEdgeKind  
    ( Lang : LbeStd . LangTyp 
    ; EstRef : EstHs . EstRefTyp 
    ; I : LbeStd . EstChildNoTyp 
    ) 
  : EstHs . EdgeKindTyp 
  RAISES { AssertionFailure } 

; PROCEDURE LeftTokForEst 
    ( Lang : LbeStd . LangTyp ; ItemRef : LbeStd . EstRootTyp ) 
  : LbeStd . TokTyp 
  RAISES { AssertionFailure } 

; PROCEDURE RightTokForEst 
    ( Lang : LbeStd . LangTyp ; ItemRef : LbeStd . EstRootTyp ) 
  : LbeStd . TokTyp 
  RAISES { AssertionFailure } 

; PROCEDURE GetKTreeSliceEdgeInfoPair 
    ( Lang : LbeStd . LangTyp 
    ; KTreeRef : EstHs . KTreeRefTyp 
    ; VAR SliceEdgeInfoPair : EstHs . SliceEdgeInfoPairTyp 
    ) 
  RAISES { AssertionFailure }

; PROCEDURE VarTermImage
    ( NodeRef : LbeStd . EstRootTyp
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    )
  : TEXT
  (* Token, StringNo, and the string, with escapes. 
     "", if NodRef is not a non-NIL SharedString.T. *)

; PROCEDURE EstNodeImageBrief 
    ( NodeRef : LbeStd . EstRootTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  : TEXT 
  RAISES { AssertionFailure } 
  (* ^Also works on NIL, giving "NIL" *) 

; PROCEDURE EstNodeImage 
    ( NodeRef : LbeStd . EstRootTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ; Mnemonic : BOOLEAN := FALSE 
    ) 
  : TEXT 
  RAISES { AssertionFailure } 
  (* ^Also works on NIL, giving "NIL" *) 

; PROCEDURE EstLeavesImage
    ( TreeRef : LbeStd . EstRootTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp
    ; Indent := LbeStd . StdIndent 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  : TEXT
  (* If TreeRef is an Est interior node, Flatten all its leaf elements and display
     then in a compact, usually one-line form, prefixed
     by node and child numbers.  Otherwise "" *) 

(* Display widths.  These are the number of chars it takes to 
   display the entire subtree of an Est node, except that if 
   it gets to be more than a full line, then we don't care 
   about further information.  This is CharNoInfinity *) 

; PROCEDURE WidthValue 
    ( Arg : PortTypes . Int32Typ ) : LbeStd . LimitedCharNoTyp 

; PROCEDURE WidthSum 
    ( Left , Right : LbeStd . LimitedCharNoSignedTyp ) 
  : LbeStd . LimitedCharNoTyp 
  (* Saturates at LbeStd . LimitedCharNoInfinity. 
     Limits at zero.
  *) 

; PROCEDURE WidthSumSigned 
    ( Left , Right : LbeStd . LimitedCharNoSignedTyp ) 
  : LbeStd . LimitedCharNoSignedTyp 
  (* Saturates at LbeStd . LimitedCharNoInfinity. 
     Also saturates at LbeStd . LimitedCharNoMinusInfinity. 
  *) 

; PROCEDURE WidthSum3 
    ( W , X , Y : LbeStd . LimitedCharNoSignedTyp ) 
  : LbeStd . LimitedCharNoTyp 
  (* Saturates at LbeStd . LimitedCharNoInfinity. 
     Limits at zero.
  *) 

; PROCEDURE WidthSumSigned3 
    ( W , X , Y : LbeStd . LimitedCharNoSignedTyp ) 
  : LbeStd . LimitedCharNoSignedTyp 
  (* Saturates at LbeStd . LimitedCharNoInfinity. 
     Also saturates at LbeStd . LimitedCharNoMinusInfinity. 
  *) 

; PROCEDURE WidthInfoCat 
    ( Left : EstHs . WidthInfoTyp 
    ; NeedsSep : BOOLEAN 
    ; Right : EstHs . WidthInfoTyp 
    ) 
    : EstHs . WidthInfoTyp 
  (* Compute the width info for the concatenation of two texts whose width 
     info values are Left and Right.  NeedsSep tells whether a token separator
     is needed between. 
  *) 

; PROCEDURE CharPosPlusWidthInfo 
    ( Left : LbeStd . LimitedCharNoSignedTyp 
    ; READONLY Right : EstHs . WidthInfoTyp 
    ) 
  : LbeStd . LimitedCharNoTyp 
  RAISES { AssertionFailure } 
  (* Prepend a left starting CharPos to a right WidthInfo, giving an ending
     CharPos. *) 
  (* Return infinity if won't fit on a full-length line. *) 

(* Misc. *) 

; PROCEDURE SearchNonleafArrayForChild 
    ( READONLY NonleafArray : EstHs . NonleafArrayTyp 
    ; ChildCt : LbeStd . EstChildNoTyp 
    ; HiElemNo : EstHs . ElemNoTyp 
      (* ^Caller knows no further left element (actually lowest 
          value of element number, since elements are numbered 
          right to left) could contain ChildNo. *) 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
  : EstHs . ElemNoTyp 
  RAISES { AssertionFailure } 

(* Operations on child lists *) 

; PROCEDURE GetEstChildContainingRelNodeNo 
    ( NodeRef : EstHs . KTreeRefTyp 
    ; RelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultChildNo : LbeStd . EstChildNoTyp 
      (* The ChildNo of the child gotten. *) 
    ; VAR ResultChildRelNodeNo : LbeStd . EstNodeNoTyp 
      (* ^The node number of the root of the subtree at ChildNo, 
         relative to the parent rooted at NodeRef. *) 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    ) 
  RAISES { AssertionFailure } 

; PROCEDURE IthChildRef 
    ( EstRef : EstHs . EstRefTyp ; I : LbeStd . EstChildNoTyp ) 
  : LbeStd . EstRootTyp   
  RAISES { AssertionFailure }  

; PROCEDURE GetIthChild 
    ( EstRef : EstHs . EstRefTyp 
    ; I : LbeStd . EstChildNoTyp 
    ; VAR ResultChildRelNodeNo : LbeStd . EstNodeNoTyp 
      (* NodeNo of root of gotten child relative to EstRef.  If this is an
         optimized-away singleton list, this will be the NodeNo of its 
         one list element, corresponding to ResultLeafElem. 
      *) 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    ) 
  RAISES { AssertionFailure } 

; PROCEDURE GetParent 
    ( RootRef : EstHs . EstRefTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp 
      (* ^Of the node whose parent is to be found, relative to RootRef. *)  
    ; VAR ResultNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultNodeRef : EstHs . EstRefTyp 
    ) 
  RAISES { AssertionFailure } 
  (* If NodeNo is either an optimized singleton list or its element, will 
     give the parent of the list. *)  
  (* ^This is pretty pedantic.  Prefer to use it only in maintenance. *) 

; PROCEDURE GetLeafElem 
    ( RootRef : EstHs . EstRefTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    ) 
  RAISES { AssertionFailure } 
  (* If NodeNo is an optimized singleton list, will give its element. *)  
  (* ^This is pretty pedantic.  Prefer to use it only in maintenance. *) 

; PROCEDURE SetChildKindBitFALSE 
    ( EstRef : EstHs . EstRefTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; KindBit : EstHs . EstChildKindTyp 
    ; VAR WasAlreadyFALSE : BOOLEAN 
      (* ^The value there was already FALSE. *) 
    ) 
  RAISES { AssertionFailure } 

; PROCEDURE SetChildKindBitTRUE 
    ( EstRef : EstHs . EstRefTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; KindBit : EstHs . EstChildKindTyp 
    ; VAR WasAlreadyTRUE : BOOLEAN 
      (* ^The value there was already TRUE. *) 
    ) 
  RAISES { AssertionFailure } 

; PROCEDURE SetDescendentKindBitTRUE 
    ( EstRef : EstHs . EstRefTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; KindBit : EstHs . EstChildKindTyp 
    ) 
  RAISES { AssertionFailure } 
  (* Also copies KindBit up, if KindBit IN EstChildKindSetCopyUp *) 

; PROCEDURE SetChildRef 
    ( EstRef : EstHs . EstRefTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; NewChildRef : LbeStd . EstRootTyp  
    ) 
  RAISES { AssertionFailure } 
  (* Violently change a child ref in place, without any changes to kind sets, 
     width info, etc.  Violates the usual immutability rule.  Use on your
     own recognizance.  
  *) 

; PROCEDURE NextInKindSet 
    ( EstRef : EstHs . EstRefTyp 
    ; StartChildNo : LbeStd . EstChildNoTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; VAR ResultChildNo : LbeStd . EstChildNoTyp 
    ; VAR ResultChildRelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    ) 
  RAISES { AssertionFailure } 
(* Beginning with the StartChildNo-th element (inclusive), 
    search for and return the subscript, root node number, 
    and leaf elem of the next element whose kind set intersects 
    KindSet. ResultChildRelNodeNo includes the Est root node itself. 
    If none found, ResultChildNo = EstChildCt ( EstRef ) 
    and other results undefined. 
*) 

; PROCEDURE PrevInKindSet 
    ( EstRef : EstHs . EstRefTyp 
    ; StartChildNo : LbeStd . EstChildNoTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; VAR ResultChildNo : LbeStd . EstChildNoTyp 
    ; VAR ResultChildRelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    ) 
  RAISES { AssertionFailure } 
  (* PrevInKindSet is like NextInKinds, but search backwards. *) 
  (* If not found, ResultChildNo = - 1 *) 

; PROCEDURE ApproxChildCt ( Root : LbeStd . EstRootTyp ) 
  : ModHs . EstApproxChildCtTyp 
  RAISES { AssertionFailure } 

(* Predicates, used in conditional formatting *) 

; PROCEDURE EvalPredicate 
    ( Lang : LbeStd . LangTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp
    ; EstRef : LbeStd . EstRootTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ) 
  : BOOLEAN 
  RAISES { AssertionFailure } 

; PROCEDURE UnmarkContainsTempMark ( EstRef : LbeStd . EstRootTyp ) 

; TYPE StatisticsTyp 
    = RECORD 
        EstLeafCt : PortTypes . Int32Typ 
      ; EstNonleafCt : PortTypes . Int32Typ 
      ; KTreeLeafCt : PortTypes . Int32Typ 
      ; KTreeNonleafCt : PortTypes . Int32Typ 
      ; LeafArrayCt : PortTypes . Int32Typ 
      ; LeafArrayElemCt : PortTypes . Int32Typ 
      ; LeafArraySize : PortTypes . Int32Typ 
      ; NonleafArrayCt : PortTypes . Int32Typ 
      ; NonleafArrayElemCt : PortTypes . Int32Typ 
      ; NonleafArraySize : PortTypes . Int32Typ 
      ; ModBlankLineCt : PortTypes . Int32Typ 
      ; ModLexErrCt : PortTypes . Int32Typ 
      ; ModSyntErrCt : PortTypes . Int32Typ 
      ; ModDelCt : PortTypes . Int32Typ 
      ; ModCmntTrailingFixedCt : PortTypes . Int32Typ 
      ; ModCmntTrailingRelativeCt : PortTypes . Int32Typ 
      ; ModCmntLeadingFixedCt : PortTypes . Int32Typ 
      ; ModCmntLeadingRelativeCt : PortTypes . Int32Typ 
      ; ModTextCt : PortTypes . Int32Typ 
      ; AstCt : PortTypes . Int32Typ 
      ; DummyTokCt : PortTypes . Int32Typ 
      ; DummyTempMarkCt : PortTypes . Int32Typ 
      ; NilCt : PortTypes . Int32Typ 
      END (* RECORD *) 

; PROCEDURE Statistics 
    ( Parent : EstHs . KTreeRefTyp ; VAR Result : StatisticsTyp ) 
  RAISES { AssertionFailure } 

; END EstUtil 
. 
