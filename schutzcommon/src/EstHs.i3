
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE EstHs 

(* Data structure for interior Est nodes. 
   Because of a some cartesian products of fields, there are accessor and
   mutator procedures in here.  
*) 

; FROM Failures IMPORT Backout 
; IMPORT PortTypes 
; IMPORT LbeStd 

(* Format numbers. 
   These distinguish the various items in a format syntax 
   rule for a single Est node kind *) 

; TYPE FmtNoTyp = PortTypes . Card8Typ 

; CONST FmtNoNull = VAL ( LAST ( FmtNoTyp ) , FmtNoTyp ) 
; CONST FmtNoUnknown = FmtNoNull - 1 
; CONST FmtNoMax = FmtNoUnknown - 1
; CONST FmtNoPad = 3 

; CONST FmtNoEstTop = 0 
  (* Used to get parse traverse started:  The top Est has this format number 
     in its (actually nonexistent) parent: *) 

; CONST FmtNoAstString = 0 (* Ast string Fs nodes have this format number. *)

; CONST FmtNoListEstChild = 0 
  (* The Est child of a list node always has this FmtNo, even when it 
     is the child of a CondFmt. *) 

; CONST FmtNoModTok = 0 
  (* ^Fs nodes that are children of FsRules for terminals used in ModTok 
      subtrees have this FmtNo. *) 

; CONST FmtNoBOIChildOfAugment = 0 
; CONST FmtNoEstChildOfAugment = 1 
  (* Augment Est nodes have one Est child, and this is its FmtNo. *) 
; CONST FmtNoEOIChildOfAugment = 2 

; CONST FmtNoInitial = 0

; PROCEDURE FmtNoImage ( FmtNo : FmtNoTyp ; Pad := FmtNoPad ) : TEXT

(* Note on FmtNo values: 
   The Est child of a list always has FmtNoListEstChild, which 
   is less than all other real FmtNos.  FmtNos of frontier leaves of 
   an Fs tree increase left to right, but need not be compact. 
   Exception:  For an Est list Fs node, FmtNos start with the Est child,
   increase rightward thru the Fs leaves to the right, then wrap to the
   leftmost leaf and increase rightward thru the leaves to the left of
   the Est Child lists.  The order of FmtNos only matters for Delete mods, 
   which can cover a range of FmtNos.  This range will never cover 
   an Est child of a list. *) 

(* Width info for subtrees. *) 

(* WidthInfoTyp distils information about a whole Est subtree that makes it
   possible to decide, without traversing it, if its text will fit on a line.

   - It assumes no line breaks are taken (i.e, the subtree is horizontal). 
   - It considers new lines only for comment, text, and blank line mods.
   - WiNlBefore and WiNlAfter tell whether there is an unconditional such 
     new line at the beginning/end of the subtree's text.  Each is 
     independent of other fields. 
   - WiHasAbsFromPos means the subtree has an absolute from position at the 
     very beginning, not one properly embedded within.  It is independent
     of other fields.  
   - If WiNlTrigger = LbeStd . LimitedCharNoInfinity, the horizontal width
     of the subtree is relative to its starting position on the line,
     and WiWidth is its horizontally formatted width.  Otherwise, there 
     are absolute positions in the subtree that can trigger implicit new lines.
     In this case, if it were to be formatted starting at an absolute 
     position <= WiNlTrigger, then WiWidth is the absolute to-position of 
     the subtree's text.  For larger starting positions, at least one implied 
     new line will be inserted in its proper interior, thus it will have
     multiple lines and infinite width.
   - However, WiWholeLineModsOnly means the entire subtree consists only of one
     or more blank lines, whole-line comments, and/or whole-line text mods.
     If such a group appears only at the left or right end of a containing
     subtree, it is as if it were a single new line, i.e., it does not
     contribute to the containing subtree's width.  Its interior part will
     be formatted as if these groups were absent.  Concatenation of WidthInfo
     values is associative, I confidently believe.
*)  

; TYPE WidthInfoTyp 
    = RECORD 
        WiNlTrigger : LbeStd . LimitedCharNoTyp 
        (* ^If the subtree begins at a position 
           > WiNlTrigger, an internal absolute 
           position will trigger an implicit new line. 
        *) 
      ; WiWidth : LbeStd . LimitedCharNoTyp 
        (* ^If WiNlTrigger = LbeStd . LimitedCharNoInfinity, 
           the subtree contains no absolute position, 
           and WiWidth is its relative (to the from- 
           position) width.  Otherwise, there are one 
           or more absolute positions in the subtree.
           In this case, if the subtree starts at absolute position
           <= WiNlTrigger, then WiWidth is the absolute to-position 
           of the subtree. For Larger starting positions, it will 
           have multiple lines and infinite width.  
        *) 
      ; WiIsNull : BITS 1 FOR BOOLEAN := TRUE (* No width info at all. *) 
      ; WiHasAbsFromPos : BITS 1 FOR BOOLEAN 
        (* ^At the very beginning, not properly embedded within. *) 
      ; WiHasNlBefore : BITS 1 FOR BOOLEAN 
      ; WiHasNlAfter : BITS 1 FOR BOOLEAN 
      ; WiHasNlWithin : BITS 1 FOR BOOLEAN 
        (* ^*Properly* embedded within. *) 
      ; WiWholeLineModsOnly : BITS 1 FOR BOOLEAN 
      END (* RECORD  WidthInfoTyp *) 

; PROCEDURE WidthInfoImage ( READONLY Value : WidthInfoTyp ) : TEXT 

(* Use the procedure below to assign to WidthInfoNull, because 
   Pm3-1.1.15, LINUXLIBC6, generates bad code for := WidthInfoNull *) 
; CONST WidthInfoNull 
    = WidthInfoTyp 
        { WiNlTrigger := LbeStd . LimitedCharNoInfinity 
        , WiWidth := 0 
        , WiIsNull := TRUE 
        , WiHasAbsFromPos := FALSE 
        , WiHasNlBefore := FALSE 
        , WiHasNlAfter := FALSE 
        , WiHasNlWithin := FALSE 
        , WiWholeLineModsOnly := FALSE 
        } 

; PROCEDURE MakeWidthInfoNull ( VAR WidthInfo : WidthInfoTyp ) 

; PROCEDURE WidthInfoIsNull ( WidthInfo : WidthInfoTyp ) : BOOLEAN 
  RAISES { Backout } 

(* Use MakeWidthInfoInfinity to assign WidthInfoInfinity, because 
   Pm3-1.1.15, LINUXLIBC6, generates bad code for := WidthInfoNull, 
   and I suspect if of being capable of the same for WidthInfoInfinity. *) 
; CONST WidthInfoInfinity 
    = WidthInfoTyp 
        { WiNlTrigger := 0 
        , WiWidth := LbeStd . LimitedCharNoInfinity 
        , WiIsNull := FALSE 
        , WiHasAbsFromPos := FALSE 
        , WiHasNlBefore := FALSE 
        , WiHasNlAfter := FALSE 
        , WiHasNlWithin := FALSE 
        , WiWholeLineModsOnly := FALSE 
        } 

; PROCEDURE MakeWidthInfoInfinity ( VAR WidthInfo : WidthInfoTyp ) 

; CONST WidthInfoSep 
    = WidthInfoTyp 
        { WiNlTrigger := LbeStd . LimitedCharNoInfinity 
        , WiWidth := LbeStd . SepWidth  
        , WiIsNull := FALSE
        , WiHasAbsFromPos := FALSE 
        , WiHasNlBefore := FALSE 
        , WiHasNlAfter := FALSE 
        , WiHasNlWithin := FALSE 
        , WiWholeLineModsOnly := FALSE 
        } 

; CONST WidthInfoLineBreakReqd  
    = WidthInfoTyp 
        { WiNlTrigger := 0 
        , WiWidth := LbeStd . LimitedCharNoInfinity 
        , WiIsNull := FALSE 
        , WiHasAbsFromPos := FALSE 
        , WiHasNlBefore := TRUE
        , WiHasNlAfter := TRUE 
        , WiHasNlWithin := TRUE  
        , WiWholeLineModsOnly := FALSE 
        } 

(* Est child kinds and their sets. *) 

; CONST EstChildKindMin = 0 
; CONST EstChildKindEstChild = 0 
        (* True Est child *) 
; CONST EstChildKindFirstOfGroup = 1 
        (* First of a group of items all 
           attached to the same format syntax item *) 
; CONST EstChildKindContainsSyntMod = 2 
        (* ModText and ModDel inserted by MergeTextEdit, on the Mod itself 
           and all containing Ests. *) 
; CONST EstChildKindContainsInsertionRepair = 3 
        (* Either an AstString for a placeholder, or a ModDel
           placed by the parser.  On the item itself and containing Ests. *) 
; CONST EstChildKindContainsDeletionRepair = 4 
        (* ModTok.  On the Mod itself and containing Ests. *) 
; CONST EstChildKindDisplayComputable = 5 
        (* A non-syntactic mod, from which the 
           position on the current line and 
           other similar information can be 
           computed spontaneously, i.e. without 
           knowledge of position information 
           about previous tokens. 
           This comprises comments with either 
           Nl before or Nl after. *) 
; CONST EstChildKindNonNIL = 6 
        (* A Nil child can exist as a child of a list node, 
           when no child is actually present, but something is 
           needed to keep adjacent separators straight. 
           This bit means NOT NIL. *) 
; CONST EstChildKindContainsErr = 7 
        (* Is a lexical or syntactic error or subtree contains one. *) 
; CONST EstChildKindContainsNoKnownNl = 8 
        (* But could contain an implied Nl that depends on StartPos. *) 
; CONST EstChildKindContainsTempMark = 9 
        (* Used to locate active TempMarks during reparsing *) 
; CONST EstChildKindTrailingMod = 10 
        (* Same line comments. *) 
; CONST EstChildKindOptSingletonList = 11 
; CONST EstChildKindTrailingSep = 12 
        (* ^The empty Est node that follows a trailing separator. *) 
(* TODO: As of 2012-8-3, this mechanism is unused, being replaced by
         EstNodeKindTrail.  But traversers are still checking for
         it, in case it should be reinstated. *) 
; CONST EstChildKindMax = 12 

; TYPE EstChildKindTyp = [ EstChildKindMin .. EstChildKindMax ] 

; CONST ImageRightMargin = 78 

; TYPE ImageKindTyp
    = { Decimal    (* Need I elaborate? *)
      , Hex        (* Hexadecimal *) 
      , Short      (* E.g. FirstOfGroup *) 
      , Ident      (* E.g. EstChildKindFirstOfGroup *)
      , Qualified  (* E.g. EstHs.EstChildKindFirstOfGroup *)
      } 

; PROCEDURE EstChildKindImage
    ( Value : EstChildKindTyp ; ImageKind : ImageKindTyp ) : TEXT 
  
; PROCEDURE EstChildKindSetFixedImage  ( Value : EstChildKindSetTyp ) : TEXT 
  (* Fixed size image, by leaving blank spaces for absent kinds and
     denoting each present kind by its hexadecimal value. *) 

; TYPE EstChildKindSetTyp = SET OF EstChildKindTyp 
; TYPE EstChildKindSetPackedTyp 
       = BITS BITSIZE ( INTEGER ) FOR EstChildKindSetTyp 
       (* ^Can't assume 100% self-adapting. *) 
(* CHECK: ^What to do about this? *) 

; PROCEDURE EstChildKindSetImage 
    ( Value : EstChildKindSetTyp 
    ; ImageKind := ImageKindTyp . Ident (* Of the set members. *)  
    ; Indent := LbeStd . StdIndent
    ; RightMargin : CARDINAL := ImageRightMargin 
    ) 
    : TEXT 

; CONST EstChildKindSetEmpty = EstChildKindSetTyp { } 

; CONST EstChildKindSetUniverse 
    = EstChildKindSetTyp 
        { FIRST ( EstChildKindTyp ) .. LAST ( EstChildKindTyp ) } 

; CONST EstChildKindSetTrailingMod 
    = EstChildKindSetTyp { EstChildKindTrailingMod } 

; CONST EstChildKindSetNonNIL = EstChildKindSetTyp { EstChildKindNonNIL } 

; CONST EstChildKindSetEstChildNonNIL 
    = EstChildKindSetTyp { EstChildKindEstChild , EstChildKindNonNIL } 

; CONST EstChildKindSetEstChildOrFirstOfGroup 
    = EstChildKindSetTyp { EstChildKindEstChild , EstChildKindFirstOfGroup } 

; CONST EstChildKindSetContainsSyntMod 
    = EstChildKindSetTyp { EstChildKindContainsSyntMod }  

; CONST EstChildKindSetContainsNoKnownNl 
    = EstChildKindSetTyp { EstChildKindContainsNoKnownNl }  

; CONST EstChildKindSetContainsInsertionRepair 
    = EstChildKindSetTyp { EstChildKindContainsInsertionRepair }  

; CONST EstChildKindSetContainsDeletionRepair 
    = EstChildKindSetTyp { EstChildKindContainsDeletionRepair }  

; CONST EstChildKindSetOptSingletonList 
    = EstChildKindSetTyp { EstChildKindOptSingletonList } 

; CONST EstChildKindSetTrailingSep  
    = EstChildKindSetTyp { EstChildKindTrailingSep } 

; CONST EstChildKindSetMustReparse 
    = EstChildKindSetTyp 
        { EstChildKindContainsSyntMod 
        , EstChildKindContainsInsertionRepair 
        , EstChildKindContainsDeletionRepair   
(* TODO: Remove EstChildKindContainsInsertionRepair 
         and EstChildKindContainsDeletionRepair
         from this set to not reparse whole subtrees with repairs inside them.
         This will require keeping the cost of the repairs in the Est node,
         in order for reparse not to make a different repair. 
*)   
        } 

; CONST EstChildKindSetNotSyntacticallyClean 
    = EstChildKindSetTyp 
        { EstChildKindContainsSyntMod 
        , EstChildKindContainsInsertionRepair 
        , EstChildKindContainsDeletionRepair  
        , EstChildKindContainsErr 
        } 

; CONST EstChildKindSetRepairPlaceholder  
    = EstChildKindSetTyp 
        { EstChildKindContainsInsertionRepair 
        , EstChildKindEstChild 
        , EstChildKindNonNIL 
        } 

; CONST EstChildKindSetFirstOfGroup 
    = EstChildKindSetTyp { EstChildKindFirstOfGroup } 

; CONST EstChildKindSetDisplayComputable 
    = EstChildKindSetTyp { EstChildKindDisplayComputable } 

; CONST EstChildKindSetContainsTempMark 
    = EstChildKindSetTyp { EstChildKindContainsTempMark } 

; CONST EstChildKindSetCopyAcross   
    = EstChildKindSetUniverse - EstChildKindSetOptSingletonList  
      (* These kinds may be used for finding an Est child and are
         copied up through the K-tree levels of a single Est node.
         Nonmembers must not be copied in this way.
      *) 

; CONST EstChildKindSetCopyUp 
    = EstChildKindSetTyp 
        { EstChildKindContainsSyntMod 
        , EstChildKindContainsInsertionRepair 
        , EstChildKindContainsDeletionRepair 
        , EstChildKindContainsTempMark 
        , EstChildKindContainsErr 
        , EstChildKindTrailingMod 
        } 
      (* These kinds are used for finding an Est descendant and are copied 
         up from an Est node to its parent. 
      *)  

; CONST EstChildKindSetMutable  
    = EstChildKindSetTyp 
        { EstChildKindContainsTempMark } 
  (* Est nodes can have this kind bit changed, without need for copying. *) 

; CONST EstChildKindSetImmutable  
    = EstChildKindSetUniverse - EstChildKindSetMutable 
      (* Assume immutable, if not specified otherwise. *) 

(* In each of the following EstChildKindSetTyp constants, element 
   EstChildKindFirstOfGroup is absent. It must be computed dynamically. *) 

; CONST EstChildKindSetModCmnt 
    = EstChildKindSetTyp 
        { EstChildKindNonNIL 
          (* EstChildKindDisplayComputable is variable *) 
        } 

; CONST EstChildKindSetEditModDel 
    = EstChildKindSetTyp 
        { EstChildKindContainsSyntMod 
        , EstChildKindNonNIL 
        , EstChildKindContainsNoKnownNl 
        } 

; CONST EstChildKindSetInsertionRepairModDel 
    = EstChildKindSetTyp 
        { EstChildKindContainsInsertionRepair 
        , EstChildKindNonNIL 
        , EstChildKindContainsNoKnownNl 
        } 

; CONST EstChildKindSetModText 
    = EstChildKindSetTyp { EstChildKindContainsSyntMod , EstChildKindNonNIL } 

; CONST EstChildKindSetModTok 
    = EstChildKindSetTyp 
        { EstChildKindContainsDeletionRepair , EstChildKindNonNIL } 

; CONST EstChildKindSetEstChild 
    = EstChildKindSetTyp 
        { EstChildKindEstChild 
          (* EstChildKindNonNil is variable. *) 
        } 

; CONST EstChildKindSetLexErrChars  
    = EstChildKindSetTyp 
        { EstChildKindNonNIL 
        , EstChildKindContainsErr 
(* CHECK: ^Do we really want this? *) 
        , EstChildKindContainsNoKnownNl 
        } 

; CONST EstChildKindSetModLexErr 
    = EstChildKindSetTyp 
        { EstChildKindNonNIL 
        , EstChildKindContainsErr 
        , EstChildKindContainsNoKnownNl
        } 

; CONST EstChildKindSetModSyntErr 
    = EstChildKindSetTyp { EstChildKindNonNIL , EstChildKindContainsErr } 

; CONST EstChildKindSetModBlankLine 
    = EstChildKindSetTyp 
        { EstChildKindNonNIL , EstChildKindDisplayComputable } 

; CONST EstChildKindSetDummy  
    = EstChildKindSetTyp 
        { EstChildKindEstChild 
        , EstChildKindNonNIL 
        , EstChildKindContainsTempMark 
        } 

; CONST EstChildKindSetSingletonListChild  
    = EstChildKindSetTyp 
        { EstChildKindEstChild 
        , EstChildKindNonNIL 
        , EstChildKindFirstOfGroup 
          (* Members of EstChildKindSetCopyUp come from the child. *) 
        }

; PROCEDURE IsFirstOfGroup 
    ( LeftFmtNo : FmtNoTyp 
    ; LeftEdgeKind : EdgeKindTyp 
    ; RightFmtNo : FmtNoTyp 
    ; RightEdgeKind : EdgeKindTyp 
    ) 
  : BOOLEAN 

(* K-trees *) 

(* Abstractly, an Est node has a 'list' of 'children'. 
   More concretely, its children (if there are enough of them) 
   are held in an inner kind of tree called a K-tree'. 
   Immediate children of a K-tree node are called 'elements'. 
   K-tree nodes have a small maximum number of elements, but are 
   allocated to hold only as many as are actually used. 
   An Est node has the top level K-tree node embedded within it. 
   An element can be a child, or a reference to a deeper K-subtree. 
   All leaves of a K-tree are of the same depth. 
   Leaf elements (i.e. elements of a leaf node of a K-tree ) 
   have a kind set and a format number.  Nonleaf elements have a kind 
   set, which is the union of the kind sets of all descendents, 
   and a cumulative count of the descendents of this element 
   and all its YOUNGER sibs in the K-tree node. 
   Note that an Est node could have either a leaf or nonleaf 
   K-tree node embedded within. 
 
   An Ast child of a node can be missing for these reasons: 
   1) It is optional in the abstract syntax of the parent 
      (implying the parent is a fixed node) and absent in this tree. 
   2) It is a required child in the abstract syntax, but the child 
      is a star-list in the abstract syntax and empty in this tree.  
      (This is the empty list optimization, q.v.) 
   3) It was deleted by text editing. 
   4) It was redeleted because the parser inserted it to correct 
      a syntax error. 

   If the parent is an Ast fixed node, a child missing for any of these 
   reasons is omitted entirely from the child list. 
   If the parent is an Ast list node, then in cases 2), 3), and 4), 
   there is an explicit child present, but its LeChildRef is NIL. 
   In case 2), this is necessary to preserve nested lists. 
   In cases 3) and 4), it is necessary so the Fs tree traversing 
   can tell where to start a new cycle of its format numbers. 
 
   Note that if the child is optional in the AS of the parent and
   can be a star-list, then absent and present but empty are distinct
   cases.   

   There are two space optimizations that are made by language-
   independent code, when valid, called the "empty-list optimization" 
   and the "singleton-list optimization" 

   The empty-list optimization represents an empty list as an absent
   child of its parent node in a tree.  This can be done unambiguously
   only if it is a required child of the parent.  

   The singleton-list optimization represents a list with exactly
   one Ast as a direct child of the list's parent node, without 
   a list Est node in between.  This can be done unambiguously
   only if all of:   
   1) The list element must not be a legal abstract child 
      of the parent (of the list) node.
   2) The list node must not be a legal abstract child of itself.
   3) In the specific tree, the one child of the list must be an 
      Ast child, i.e., not a modifier.    
 
*) 

(* Element numbers/counts *) 

; CONST ElemCtMax = 8 
; TYPE ElemNoTyp = [ 0 .. ElemCtMax ] 
; TYPE ElemSsTyp = [ 0 .. ElemCtMax - 1 ] 

; CONST ElemNoImage = PortTypes . Int32Image 

(* K-tree heights *) 

; CONST KTreeHeightMax = 31 

; TYPE KTreeHeightTyp = [ 0 .. KTreeHeightMax ] 
; TYPE KTreeHeightPackedTyp = BITS 5 FOR KTreeHeightTyp 
       (* ^Can't assume 100% self-adapting. *) 

; CONST KTreeHeightImage = PortTypes . Int32Image 

(* Leaf Elements and Arrays thereof *) 

; TYPE LeafElemTyp 
    = RECORD 
        LeChildRef : LbeStd . EstRootTyp 
      ; LeCumNodeCt : LbeStd . EstNodeNoTyp 
        (* ^Node count of Est descendents of this subtree, plus all the 
            subtrees to the right (which have lower numbered subscripts). *) 
      ; LeKindSet : EstChildKindSetTyp 
      ; LeFmtNo : BITS 8 FOR FmtNoTyp 
      END (* RECORD *) 

; CONST LeafElemNull 
    = LeafElemTyp 
        { LeCumNodeCt := 0 
        , LeChildRef := NIL 
        , LeKindSet := EstChildKindSetEmpty 
        , LeFmtNo := FmtNoNull
        } 

; CONST LeafArrayElemCtMax = ElemCtMax 
; CONST LeafArraySsMax = LeafArrayElemCtMax - 1 

; PROCEDURE LeafElemImage 
    ( READONLY LeafArray : LeafArrayTyp 
    ; Indent := LbeStd . StdIndent 
    ; Number : [ 0 .. LeafArrayElemCtMax ] 
    ; Subscript : [ 0 .. LeafArrayElemCtMax ] 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
    : TEXT 

; TYPE LeafArrayTyp = ARRAY OF LeafElemTyp 
; TYPE LeafArrayRefTyp = REF LeafArrayTyp 
; TYPE FullLeafArrayTyp = ARRAY [ 0 .. LeafArraySsMax ] OF LeafElemTyp 
       (* Element 0 is on the right. *) 

; PROCEDURE LeafArrayImage 
    ( READONLY Value : LeafArrayRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
    : TEXT 

; PROCEDURE RefToNewLeafArray 
    ( Length : ElemNoTyp ; READONLY FullLeafArray : FullLeafArrayTyp ) 
    : LeafArrayRefTyp 
  (* Allocate and return the ref to a new leaf array of exactly 
     Length elements, initializing them from the first Length 
     elements of FullLeafArray *) 

(* Nonleaf Elements and Arrays thereof *) 

; TYPE NonleafElemTyp 
    = RECORD 
        NleChildRef : KTreeRefTyp 
      ; NleCumChildCt : LbeStd . EstChildNoTyp 
        (* ^Child count of direct Est children of this nonleaf node, 
           plus all the subtrees to the right (which have lower 
           numbered subscripts). *) 
      ; NleCumNodeCt : LbeStd . EstNodeNoTyp 
        (* ^Node count of Est descendents of this subtree, plus all the 
           subtrees to the right (which have lower numbered subscripts). *) 
      ; NleKindSet : EstChildKindSetTyp 
      END (* RECORD *) 

; CONST NonleafElemNull 
    = NonleafElemTyp 
        { NleCumNodeCt := 0 
        , NleCumChildCt := 0 
        , NleChildRef := NIL 
        , NleKindSet := EstChildKindSetEmpty 
        } 

; CONST NonleafArrayElemCtMax = ElemCtMax 
; CONST NonleafArraySsMax = NonleafArrayElemCtMax - 1 

; PROCEDURE NonleafElemImage 
    ( READONLY NonleafArray : NonleafArrayTyp 
    ; Indent := LbeStd . StdIndent 
    ; Number : [ 0 .. NonleafArrayElemCtMax ] 
    ; Subscript : [ 0 .. NonleafArrayElemCtMax ] 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
    : TEXT 

; TYPE NonleafArrayTyp = ARRAY OF NonleafElemTyp 
; TYPE NonleafArrayRefTyp = REF NonleafArrayTyp 
; TYPE FullNonleafArrayTyp 
    = ARRAY [ 0 .. NonleafArraySsMax ] OF NonleafElemTyp 
      (* Element 0 is on the right. *) 

; PROCEDURE NonleafArrayImage 
    ( READONLY Value : NonleafArrayRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
    : TEXT 

; PROCEDURE RefToNewNonleafArray 
    ( Length : ElemNoTyp ; READONLY FullNonleafArray : FullNonleafArrayTyp ) 
    : NonleafArrayRefTyp 
  (* Allocate and return the ref to a new nonleaf array of exactly 
     Length elements, initializing them from the first Length 
     elements of FullNonleafArray *) 

(* Edge Info.  This is information collected from 
   the leftmost (First) and rightmost (Last) descendent 
   leaf element.  Only a nonleaf node has this stored. 
   For leaf nodes, it is computed when needed. 
*) 

; TYPE EdgeKindTyp 
    = { EdgeKindLeadingMod 
      , EdgeKindTrailingMod  
      , EdgeKindModDel 
      , EdgeKindEstChild 
      } 
  (* NOTE: EdgeKindTyp is partially  redundant, since 
           1) There is no situation where either a ModDel or EstChild 
              could occur.  The FmtNo/FsChild either denote an InsTok, 
              which can have a ModDel, or an EstChild, which can have 
              an EstChild. 
           2) We can distinguish a TrailingMod by looking at the Tok. *) 

; PROCEDURE EdgeKindImage ( READONLY Value : EdgeKindTyp ) : TEXT 

; TYPE EdgeKindSetTyp = SET OF EdgeKindTyp 
; TYPE EdgeKindSetPackedTyp = BITS BITSIZE ( INTEGER ) FOR EdgeKindSetTyp 
       (* ^Can't assume 100% self-adapting. *) 
(* CHECK: ^What to do about this? *) 

; CONST EdgeKindSetLeadingOrModDel 
    = EdgeKindSetTyp 
        { EdgeKindTyp . EdgeKindLeadingMod , EdgeKindTyp . EdgeKindModDel } 
; CONST EdgeKindSetTrailingOrModDel 
    = EdgeKindSetTyp 
        { EdgeKindTyp . EdgeKindTrailingMod , EdgeKindTyp . EdgeKindModDel } 
; CONST EdgeKindSetLeadingModOrEstChild 
    = EdgeKindSetTyp 
        { EdgeKindTyp . EdgeKindLeadingMod , EdgeKindTyp . EdgeKindEstChild } 

; CONST EdgeKindSetTrailingModOrEstChild 
    = EdgeKindSetTyp 
        { EdgeKindTyp . EdgeKindTrailingMod , EdgeKindTyp . EdgeKindEstChild } 

; TYPE EdgeInfoTyp 
    = RECORD 
        EiTok : LbeStd . TokTyp 
      ; EiFmtNo : FmtNoTyp 
      ; EiEdgeKind : EdgeKindTyp 
      END (* RECORD *) 

; PROCEDURE EdgeInfoImage ( READONLY Value : EdgeInfoTyp ) : TEXT 

; CONST EdgeInfoNull 
    = EdgeInfoTyp 
        { EiTok := LbeStd . Tok__Null 
        , EiFmtNo := FmtNoNull 
        , EiEdgeKind := EdgeKindTyp . EdgeKindEstChild 
        } 

; TYPE SliceEdgeInfoPairTyp 
    = RECORD 
        SeiLeftEdgeInfo : EdgeInfoTyp 
      ; SeiRightEdgeInfo : EdgeInfoTyp 
      END (* RECORD *) 
      (* Fields EiFmtNo and EiEdgeKind are for the leftmost/rightmost 
         Est child of the K-subtree. 
         Differently, EiTok is the leftmost/rightmost _nonNull_ Tok in 
         the K-subtree. 
 
         For Nonleaf nodes, this is all stored in the node. 
         For a Leaf node, left and right edgeInfo is not stored in the 
         leaf, but computed when needed. 
         Fields EiFmtNo and EiEdgeKind are taken from the 
         leftmost/rightmost Est child of the LeafArray, which can be done 
         in fixed time.  For EiTok fields, we must search from the two 
         ends for nonNull left/right Tok.  Compared to storing just the 
         nonNull Toks in the Leaf node, this could take, worst case, 
         ElemCtMax**2 visits, by the time we go thru ElemCtMax elements 
         of the Nonleaf node at height 2 and for each of these, 
         ElemCtMax elements of the leaf node.  This is a bad number, 
         but it is still a constant factor. 
 
         Mainly, I don't expect much searching to ever happen in 
         practice.  The only Est subtrees which can contribute a 
         null Tok are ModDel and ModErr.  Since successive deleted 
         insertion tokens are always covered by one ModDel, the 
         largest number of ModDels we could have in a row would come 
         from every other insertion token being deleted.  The alternate 
         insertion tokens could each have >1 error mod.  There are not 
         going to be all that many insertion tokens in any particular 
         format syntax tree. 
      *) 

; PROCEDURE SliceEdgeInfoPairImage 
    ( READONLY Value : SliceEdgeInfoPairTyp ; Indent := LbeStd . StdIndent ) 
    : TEXT 

(* Est misc. info.  This is not uniformly stored in permanent data structure, 
   but is computed when needed for an Est.  Computation of the various 
   fields follows similar case analysis, so we do them all together, 
   while we are at it. The values are packaged in a record for convenience. 
   Some users may need only one or a few fields.  This avoids passing 
   lots of DontCare parameters. *) 

; TYPE EstMiscInfoTyp 
    = RECORD 
        EmiLeftTok : LbeStd . TokTyp 
      ; EmiRightTok : LbeStd . TokTyp 
      ; EmiTok : LbeStd . TokTyp 
      ; EmiSyntTokCt : LbeStd . LimitedTokCtTyp 
      ; EmiEdgeKind : EdgeKindTyp 
      ; EmiWidthInfo : WidthInfoTyp 
      END (* RECORD EstMiscInfoTyp *) 

(* There is a Cartesian product of node formats: 
   { Est , KTree } X { Leaf , Nonleaf }. 
   Est is topmost in a K-tree and KTree is non-topmost. 
   Leaf is bottommost and Nonleaf is non-bottommost. 
 
   KTreeRefTyp is the topmost object type and EstRefTyp is a subtype 
   of KTreeRefTyp, which takes care of the first distinction. The 
   second is handled by subtypes KTreeLeafRefTyp, KTreeNonleafRefTyp, 
   EstLeafRefTyp, and EstNonleafRefTyp.  Access to fields common to 
   both leaf types or to both nonleaf types is by method of KTreeRefTyp. 
*) 

(* K-tree nodes. These are fields common to leaf/nonleaf, 
   root/nonroot nodes. *) 

; TYPE KTreeRefTyp 
    = LbeStd . EstRootTyp 
        OBJECT (* ABSTRACT *) 
          KTreeEstChildCtLeftOfNl : LbeStd . EstChildNoTyp 
          (* ^Number of Est children left of any new line.  New line includes
             both Nls in mods and line breaks, which, for this purpose, are 
             always considered taken.  Includes counts from k-tree descendents 
             down thru k-tree leaves, but counts an Est subtree as one child.  
          *) 
        ; KTreeWidthInfo : WidthInfoTyp 
        ; KTreeSyntTokCt : LbeStd . LimitedTokCtTyp 
        ; KTreeElemCt : ElemNoTyp 
          (* ^This field is partially redundant (some places would need a 
             method in its place).  Maybe someday remove it.  Can use 
             NUMBER ( LeafArrayRef ( ) ^ ) or NUMBER ( NonleafArrayRef ( ) ^ ) 
             Some places already have this array ref fetched and narrowed.
             However, if we ever pull some unsafe hack to allocate nodes
             with variable child counts, we will need it.  
          *) 
        METHODS 
          KTreeChildCt ( ) : LbeStd . EstChildNoTyp 
          (* ^Direct children, at leaves of this K-tree. *) 
        ; KTreeNodeCt ( ) : LbeStd . EstNodeNoTyp 
          (* ^Est descendents *) 
        ; LeafArrayRef ( ) : LeafArrayRefTyp := NIL 
        ; NonleafArrayRef ( ) : NonleafArrayRefTyp := NIL 
        (* These will only fetch elements that actually exist: *) 
        ; FetchLeafArray ( VAR ResultLeafArray : FullLeafArrayTyp ) 
        ; FetchNonleafArray ( VAR ResultNonleafArray : FullNonleafArrayTyp ) 
        ; Image 
            ( Indent := LbeStd . StdIndent 
            ; NodeNo : LbeStd . EstNodeNoTyp 
            ; ChildNo : LbeStd . EstChildNoTyp 
            ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
            ) 
          : TEXT 
          := KTreeRefImage 
        END (* OBJECT *) 

; PROCEDURE KTreeRefImage 
    ( Self : KTreeRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

(* Est nodes *) 

; TYPE EstNodeKindTyp 
    = { EstNodeKindPlain  (* Ordinary Est node, with abstract children. *)  
      , EstNodeKindTrail  (* Ordinary, but for an abstract list node that
                             can and does have trailing separators. *) 
      , EstNodeKindModTok (* A token the parser skipped, reinserted, 
                             along with its leading and trailing mods. *) 
      } 
; TYPE EstNodeKindPackedTyp = BITS 2 FOR EstNodeKindTyp 
       (* ^Can't assume 100% self-adapting. *) 

; PROCEDURE EstNodeKindImage ( Value : EstNodeKindTyp ) : TEXT 

; TYPE EstRefTyp 
    = KTreeRefTyp 
        OBJECT 
          EstSemRef : ROOT := NIL (* For use by semantics. *) 
        ; EstTok : LbeStd . TokTyp 
        ; EstLeftTok : LbeStd . TokTyp 
        ; EstRightTok : LbeStd . TokTyp 
        ; EstNodeKind : EstNodeKindPackedTyp 
        (* EstHeight = 0 => no children at all. *) 
        ; EstHeight : KTreeHeightPackedTyp
        ; EstRepairCost : LbeStd . RepairCostTyp  
(* TODO: ^Add this field, use it, review packing of this type. *) 
        ; EstChildKindSet : EstChildKindSetTyp 
        OVERRIDES 
          Image := EstRefImage 
        END (* OBJECT EstRefTyp *) 

; PROCEDURE EstRefImageBrief 
    ( Self : EstRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

; PROCEDURE EstRefImage 
    ( Self : EstRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

; TYPE KTreeLeafRefTyp (* Nonroot, leaf *) 
    = KTreeRefTyp 
        OBJECT 
          KTreeLeafArrayRef : LeafArrayRefTyp 
        OVERRIDES 
          KTreeChildCt := KTreeLeafChildCt 
        ; KTreeNodeCt := KTreeLeafNodeCt 
        ; LeafArrayRef := KTreeLeafArrayRef 
        ; NonleafArrayRef := NIL 
        ; FetchLeafArray := FetchKTreeLeafArray 
        ; FetchNonleafArray := NIL 
        ; Image := KTreeLeafRefImage 
        END (* OBJECT *) 

; PROCEDURE KTreeLeafRefImage 
    ( Self : KTreeLeafRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

; TYPE KTreeNonleafRefTyp (* Nonroot, nonleaf *) 
    = KTreeRefTyp 
        OBJECT 
          KTreeNonleafArrayRef : NonleafArrayRefTyp 
        ; KTreeNonleafSliceEdgeInfoPair : SliceEdgeInfoPairTyp 
        OVERRIDES 
          KTreeChildCt := KTreeNonleafChildCt 
        ; KTreeNodeCt := KTreeNonleafNodeCt 
        ; LeafArrayRef := NIL 
        ; NonleafArrayRef := KTreeNonleafArrayRef 
        ; FetchLeafArray := NIL 
        ; FetchNonleafArray := FetchKTreeNonleafArray 
        ; Image := KTreeNonleafRefImage 
        END (* OBJECT *) 

; PROCEDURE KTreeNonleafRefImage 
    ( Self : KTreeNonleafRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

; TYPE EstLeafRefTyp (* Root, leaf *) 
    = EstRefTyp 
        OBJECT 
          EstLeafArrayRef : LeafArrayRefTyp 
        OVERRIDES 
          KTreeChildCt := EstLeafChildCt 
        ; KTreeNodeCt := EstLeafNodeCt 
        ; LeafArrayRef := EstLeafArrayRef 
        ; NonleafArrayRef := NIL 
        ; FetchLeafArray := FetchEstLeafArray 
        ; FetchNonleafArray := NIL 
        ; Image := EstLeafRefImage 
        END (* OBJECT *) 

; PROCEDURE EstLeafRefImage 
    ( Self : EstLeafRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

; TYPE EstNonleafRefTyp (* Root, nonleaf *) 
    = EstRefTyp 
        OBJECT 
          EstNonleafArrayRef : NonleafArrayRefTyp 
        ; EstNonleafSliceEdgeInfoPair : SliceEdgeInfoPairTyp 
        OVERRIDES 
          KTreeChildCt := EstNonleafChildCt 
        ; KTreeNodeCt := EstNonleafNodeCt 
        ; LeafArrayRef := NIL 
        ; NonleafArrayRef := EstNonleafArrayRef 
        ; FetchLeafArray := NIL 
        ; FetchNonleafArray := FetchEstNonleafArray 
        ; Image := EstNonleafRefImage 
        END (* OBJECT *) 

; PROCEDURE EstNonleafRefImage 
    ( Self : EstNonleafRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

; PROCEDURE KTreeLeafChildCt 
    ( Self : KTreeLeafRefTyp ) : LbeStd . EstChildNoTyp 

; PROCEDURE KTreeLeafNodeCt 
    ( Self : KTreeLeafRefTyp ) : LbeStd . EstNodeNoTyp 

; PROCEDURE KTreeLeafArrayRef ( Self : KTreeLeafRefTyp ) : LeafArrayRefTyp 

; PROCEDURE FetchKTreeLeafArray 
    ( Self : KTreeLeafRefTyp ; VAR ResultLeafArray : FullLeafArrayTyp ) 

; PROCEDURE KTreeNonleafChildCt 
    ( Self : KTreeNonleafRefTyp ) : LbeStd . EstChildNoTyp 

; PROCEDURE KTreeNonleafNodeCt 
    ( Self : KTreeNonleafRefTyp ) : LbeStd . EstNodeNoTyp 

; PROCEDURE KTreeNonleafArrayRef 
    ( Self : KTreeNonleafRefTyp ) : NonleafArrayRefTyp 

; PROCEDURE FetchKTreeNonleafArray 
    ( Self : KTreeNonleafRefTyp 
    ; VAR ResultNonleafArray : FullNonleafArrayTyp 
    ) 

; PROCEDURE EstLeafChildCt ( Self : EstLeafRefTyp ) : LbeStd . EstChildNoTyp 

; PROCEDURE EstLeafNodeCt ( Self : EstLeafRefTyp ) : LbeStd . EstNodeNoTyp 

; PROCEDURE EstLeafArrayRef ( Self : EstLeafRefTyp ) : LeafArrayRefTyp 

; PROCEDURE FetchEstLeafArray 
    ( Self : EstLeafRefTyp ; VAR ResultLeafArray : FullLeafArrayTyp ) 

; PROCEDURE EstNonleafChildCt 
    ( Self : EstNonleafRefTyp ) : LbeStd . EstChildNoTyp 

; PROCEDURE EstNonleafNodeCt 
    ( Self : EstNonleafRefTyp ) : LbeStd . EstNodeNoTyp 

; PROCEDURE EstNonleafArrayRef 
    ( Self : EstNonleafRefTyp ) : NonleafArrayRefTyp 

; PROCEDURE FetchEstNonleafArray 
    ( Self : EstNonleafRefTyp 
    ; VAR ResultNonleafArray : FullNonleafArrayTyp 
    ) 

; VAR UniqueEstNodeTrailingSep : EstRefTyp 
  (* Treat as CONST.
     A single global node, shared everywhere, that formats empty,
     has Tok__Empty, and is placed as the last child of an Est
     list that can and does have a trailing separator. *) 

(* Is the following still true? I think trailing mods make it 
   unnecessary. *) 

(* The topmost node of every tree (for an image) has builtin 
   kind EstNodeKindImage.  This always has two children: 
     FmtNo: FmtNoCode 
       : The tree  for the code--a language dependent Est node. 
         It could be a KTree, if several units are acceptable 
         in one file. 
     FmtNo: FmtNoEndOfImage 
       : a format syntax node of LangUtil.FsKindTyp.FsKindInsTok 
         with SharedStrings . Tok ( FsInsTokRef ) = LbeStd . Tok__EndOfImage. 
         Mods after all code are attached here. 
*) 

; END EstHs 
. 
