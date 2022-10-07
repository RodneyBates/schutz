
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE MergeTxt 

(* This module merges a localized set of text edit changes into an Est. *) 

; IMPORT Thread 

; IMPORT EstBuild 
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LangUtil 
; FROM LangUtil IMPORT FsKindTyp 
; IMPORT LbeStd 
; IMPORT Marks 
; FROM Marks IMPORT MarkKindTyp 
; IMPORT MessageCodes 
; IMPORT ModHs 
; IMPORT Options 
; IMPORT PortTypes 
; IMPORT SharedStrings 
; IMPORT Strings 
; IMPORT TravUtil 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , AssertionFailure , CantHappen  
; FROM Failures IMPORT Backout  

; TYPE AFT = MessageCodes . T 

(* EXPORTED: *) 
; PROCEDURE MergeTextEdit 
    ( Lang : LbeStd . LangTyp 
    ; EstRootRef : EstHs . EstRefTyp 
    ; StartTokMark : Marks . TokMarkTyp 
    ; EndTokMark : Marks . TokMarkTyp 
    ; BlankLineNo : LbeStd . LineNoTyp 
      (* Line number within a blank line mod or after EndOfImage. *) 
    ; DelFromPos : LbeStd . LimitedCharNoTyp 
    ; DelToPos : LbeStd . LimitedCharNoTyp 
      (* ^Chars in [ DelFromPos .. DelToPos - 1 ] are deleted and replaced 
          by those in InsText [ DelFromPos .. DelFromPos + InsLen - 1 ] *) 
    ; READONLY InsText : Strings . StringTyp 
      (* ^This is an image of the entire line, after the edits to be merged. *) 
    ; InsLen : LbeStd . LimitedCharNoTyp 
    ; InsNlPos : LbeStd . LimitedCharNoTyp 
      (* ^Position in the edited line before which a new line goes. 
          LbeStd . LimitedCharNoInfinity if no new line at all. 
          Otherwise, must line in the interval 
          [ DelFromPos , DelFromPos + InsLen ] *) 
    ; NlIndentPos : LbeStd . LimitedCharNoTyp 
      (* ^If InsNlPos # LbeStd . LimitedCharNoInfinity, this the amount 
          of indentation of the new text line, after the inserted Nl. 
          Ignored otherwise. *) 
    ; DelNlShift : LbeStd . LimitedCharNoTyp 
      (* ^When LbeStd . LimitedCharNoInfinity, no new line is deleted. 
          Otherwise, the new line at the end of the line denoted by 
          BolTokMark/BlankLineNo is deleted, and DelNlShift is the 
          amount to add to character positions on the formerly following 
          line to get it appended to the first line. 
          InsNlPos and DelNlShift cannot both be unequal to 
          LbeStd . LimitedCharNoInfinity. 
      *) 
    ; VAR NewEstRootRef : EstHs . EstRefTyp 
    ; VAR NodeNoChange : LbeStd . EstNodeNoTyp   
    ; VAR MaxTouchedNodeNo : LbeStd . EstNodeNoTyp
      (* ^Nodes in the old Est with node numbers > MaxTouchedNodeNo will have
          node numbers in the new tree that differ by NodeNoChange, which can
          be negative or positive.  Any references to them need to be adjusted
          to match.  MergeTextEdit doesn't know where they are, so caller
          must do it.  OTOH, MergeTextEdit does adjust node numbers in
          marks it produces.
      *) 
    ; VAR NewBolTokMark : Marks . TokMarkTyp 
    ; VAR NewLinesCt : LbeStd . LineNoTyp 
    ; VAR LeadingBlankLinesIncluded : LbeStd . LineNoTyp  
      (* ^This many lines from leading blank mods were incorporated into the 
         resulting set of tree children. *)  
    ; VAR TrailingBlankLinesIncluded : LbeStd . LineNoTyp  
      (* ^Similarly, for trailing mods. *)
    ) 
    RAISES { Backout , Thread . Alerted } 

  (* MergeTextEdit puts a single-location group of text 
     modifications into an Est. *) 

  (* It begins in state MteStateStartAtBeg or MteStateStartAtEnd, where 
     it descends through the tree to the place StartTokMark denotes.  If 
     this is a blank line mod or off the end of image, all the text edits 
     are handled in one routine, without further traversing.  Otherwise, 
     it goes into MteStatePassingNl and starts traversing forward, 
     counting characters.  It remembers the location of the 
     most recent blank, so when it reaches DelFromPos, it can 
     compute the number of characters of leading, touched tokens, 
     which will have to be prepended to the inserted text.

     Whenever it is in a group of adjacent new lines, it is in state
     MteStatePassingNl.  This group is the dividing line boundary 
     between two, being-joined lines, iff MtePassedDelNl, otherwise,
     it is the starting line boundary.  When traversing forward but
     not inside a group of adjacent new lines, it is in state 
     MteStateFwd.  
 
     When traversing forward, it counts most of the things it sees 
     in MteItemCt. 
 
     It also develops an array (named MteBlanks) whose elements
     correspond to all comment, token insertion and text insertion
     mods, insertion toks, and Ast strings in the line and whose
     element values give the actual number of blanks inserted ahead of
     the item, and, in the case of a text insertion, after it as well
     (in a separate element of the array).  This will be needed to
     reconstruct the line when going backwards.
 
     When it reaches DelToPos, it starts looking for a blank, still
     going forwards.  The token before the next blank is the last one
     "touched" by the modification.
 
     When it reaches EOL, it goes into MteStateBwdEdit, where it does
     reconstruction of the Est nodes.  It appends/prepends characters
     of tokens touched but not deleted to the insertion text.  It
     deletes all touched tokens from the Est, in the manner
     appropriate to the token kind.  Finally, it constructs a new text
     insertion mod and inserts it.
 
     It reconstructs Ests right to left during its backward pass,
     using EstBuild to build the new child list at each level.
 
     When it gets back to DelFromPos, it goes into MteStateBwdNl,
     where it continues to traverse to the beginning of line, (since
     the Nl could be implicit, it detects this by MteItemCt's going to
     zero), rebuilding Ests at each level.
 
     At the end, it builds a new TokMark (or TokMarks, if there are
     multiple lines/blank lines) and differentiates the rest of the
     Est.  
  *)

  = TYPE MteStateTyp 
      = { MteStateStartAtBeg    (* Start at the beginning of the token 
                                   identified by StartTokMark. *) 
        , MteStateStartAtEnd    (* Start at the end of the token 
                                   identified by StartTokMark. *) 
        , MteStateFwd           (* Looking for the new line at the end of 
                                   the edited line. *) 
        , MteStatePassingNl     (* Going forward, passing over a new line 
                                   (in the old Est), either the starting Nl
                                   or the middle one. if we pass it. *) 
        , MteStateRightNlFound  (* Have hit the ending Nl, but still going Fwd.
                                   We don't need to go all the way to the RM
                                   Nl, but we must pass a NlAfter of a 
                                   trailing mod, because to-be-constructed
                                   leading mods wouldn't have the right
                                   mark parameters, otherwise. *) 
        , MteStateBwdEdit       (* Going backward, in or to the right of 
                                   the touched region. *) 
        , MteStateBwdNl         (* Have finished the touched region.  Now 
                                   heading backwards for the new line where 
                                   we started. *) 
        , MteStateDone          (* Found the new line at the beginning, but 
                                   still have to return from all the 
                                   traversals of Fs trees and Ests at this 
                                   level and higher to finish merging or to 
                                   differentiate the relevant child. *) 
        } 
    (* In MteStateStartAtBeg and MteStateStartAtEnd, TraverseEst and
       TraverseFs jump into the middle of their respective trees, 
       using the start of line mark to decide where.  Once the starting
       point is reached, we go in to other states and traverse linearly,
       first left-to-right, then right-to-left. *) 

  ; TYPE MteStateSetTyp = SET OF MteStateTyp 

  ; CONST MteStateSetStart   
      = MteStateSetTyp 
          { MteStateTyp . MteStateStartAtBeg
          , MteStateTyp . MteStateStartAtEnd  
          } 
  ; CONST MteStateSetBwdFinished  
      = MteStateSetTyp 
          { MteStateTyp . MteStateBwdNl 
          , MteStateTyp . MteStateDone 
          } 
        (* ^MteTeMaybeFinishBwdEdit has done its work. *) 
  ; CONST MteStateSetBwdOrDone 
      = MteStateSetTyp 
          { MteStateTyp . MteStateBwdEdit 
          , MteStateTyp . MteStateBwdNl 
          , MteStateTyp . MteStateDone 
          } 
  ; CONST MteStateSetStartOrFwd 
      = MteStateSetTyp 
          { MteStateTyp . MteStatePassingNl 
          , MteStateTyp . MteStateStartAtBeg 
          , MteStateTyp . MteStateStartAtEnd 
          , MteStateTyp . MteStateFwd 
          , MteStateTyp . MteStateRightNlFound 
          } 

  ; VAR MteState : MteStateTyp 
  ; VAR MteCharPos : LbeStd . LimitedCharNoSignedTyp 
        (* Unlike the other variables here, MteCharPos 
           reflects the view that the deleted new line, if any, 
           has occurred and the formerly-two lines are now one. *) 
  ; VAR MteFwdLeftTokToPos : LbeStd . LimitedCharNoSignedTyp 
  ; VAR MteFwdPrevTok : LbeStd . TokTyp 
  ; VAR MteItemCt : PortTypes . Card32Typ 
        (* MteItemCt is used to decide, while traversing backwards, when 
           we are back to where we started.  Many relevant things which 
           are visited while going forward are counted in MteItemCt. 
           These same things are counted down in MteItemCt while going 
           backwards.  Things that are counted are: 
             ModBlankLine 
             ModCmnt 
             ModText 
             ModTok 
             Nl after of ModBlankLine, ModCmnt, or ModText 
             LexError 
             InsTok 
             AstStringTok 
             BOI 
             LineBreak, either started-at or passed. 
        *) 
  ; VAR MteFinishItemNo : PortTypes . Card32Typ 
    (* When MteItemCt gets down to this value, it is time for 
       MteTeMaybeFinishBwdEdit to do its work.  When starting at a
       ModTok, line break, or Nl after, this will be one, because by the
       time we get back to the ModTok, line break, or Nl after, we could
       have ascended to a higher tree and lost the correct place
       to insert the new mods. Otherwise, it is zero. *) 
  ; VAR MteNlItemNo : PortTypes . Card32Typ 
    (* The value of MteItemCt when we hit the leftmost Nl of a group of them
       that we pass. *) 
 
  ; VAR MteBlankSs : LbeStd . LimitedCharNoSignedTyp 
  (* Most variables local to MergeTextEdit describe a "joined line",
     which is as it was before inserting a Nl, but after deleting 
     an Nl.  Vars with "New" in the name are for the joined, edited
     line.  Others are unedited. *) 
  ; VAR MteTouchedFromPos : LbeStd . LimitedCharNoSignedTyp 
  ; VAR MteTouchedToPos : LbeStd . LimitedCharNoSignedTyp 
  ; VAR MteLineShift : LbeStd . LimitedCharNoTyp 
        (* Amount to add to an original character position, to get the
           corresponding position in the pair of lines joined by a passed,
           deleted new line.  Add this to a position given by the old 
           Est to get a position in the unedited, joined line. It changes 
           during forward and backward traversals.  It is always zero when
           we are left of the deleted Nl or if there is none.  Otherwise,
           it is the number of characters in the original first line.
           Thus, it can be used unconditionally. *) 
  ; VAR MteSuffixShift : LbeStd . LimitedCharNoSignedTyp 
        (* Amount by which characters originally located at or right
           of DelToPos are shifted by character insertions and
           deletions, not counting any Nl insertion or deletion.  
           Computed before any traversal.  Add to convert a position in
           the old joined line, at or to the right of MteTouchedToPos, 
           to a position in the new joined line. *) 
  ; VAR MteModShift : LbeStd . LimitedCharNoSignedTyp 
        (* Like MteSuffixShift, but also takes into account, inserted
           or deleted Nl. Computed before any traversal.  Add to convert a
           position to the right of MteTouchedToPos found in the old tree
           to a position in the final new tree. *) 
  ; VAR MteFwdPrevItemCouldHaveCondNlAfter : BOOLEAN 
    (* Only maintained during forward pass, up to beginning of 
       deleted region. *) 
  ; VAR MteNlChanged : BOOLEAN 
  ; VAR MteNlOrTransparencyChanged : BOOLEAN 
  ; VAR MtePassedDelNl : BOOLEAN 
  ; VAR MteNewDelFromFmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull  
  ; VAR MteNewDelThruFmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull  
  ; VAR MteNewDelIsRepair : BOOLEAN := FALSE 
  ; VAR MteNewDelIsNotRepair : BOOLEAN := FALSE 
(* CHECK:  Review to see if deletes or blank lines are ever waited 
           at a lower Est level, to be merged in at a higher level. 
           If not, then MteNewDelFromFmtNo, MteNewDelThruFmtNo,
           MteNewDelIsRepair, MteNewDelIsNotRepair,  
           MteNewBlankLineFmtNo and MteNewBlankLineCt should be 
           local to MteTraverseEst, and renamed accordingly. 
           I think this is the case.  But also review how text editing 
           can start at a lower level and finish at a higher level. 
           This should be possible, to get things flattened into one 
           ModText.  This is why e.g. MteNewTextLen, etc. are here. 
           But the code in MteTeTfsEstSubtree, after doing the subtree: 
              IF LWasGoingFwd 
              THEN (* A reversal happened deeper in the Est.  Start 
                      backwards processing at this level. *) 
                MteTeWaitChildrenFartherToRight ( ) 
              END (* IF *) 
           Seems wrong for this case. 
 
*) 
  ; VAR MteNewTextLen : LbeStd . LimitedCharNoSignedTyp 
  ; VAR MteNewTextPos : LbeStd . LimitedCharNoSignedTyp 
  ; VAR MteNewTextToPos : LbeStd . LimitedCharNoTyp 
  ; VAR MteNewTextLeftTokToPos : LbeStd . LimitedCharNoSignedTyp 
  ; VAR MteNewTextRelNlPos : LbeStd . LimitedCharNoTyp 
  ; VAR MteBlankLinesBefore : LbeStd . LineNoTyp 
  ; VAR MteBlankLinesAfter : LbeStd . LineNoTyp 
  ; VAR MteStartingModBlankLineRef : ModHs . ModBlankLineTyp 
  ; VAR MteNewBlankLineCt : LbeStd . LineNoTyp 
  ; VAR MteNewBlankLineFmtNo : EstHs . FmtNoTyp 
  ; VAR MteImbeddedNlModRef : ModHs . ModRefTyp 
  ; VAR MteEstNodeCt : LbeStd . EstNodeNoTyp 
  ; VAR MteModTextIsToLeftOnLine : BOOLEAN 
  ; VAR MteLastFmtNoOnLine : EstHs . FmtNoTyp
  ; VAR MteEstListChildrenToPass : LbeStd . EstChildNoTyp 
  ; VAR MteBlanks 
      : ARRAY LbeStd . LimitedCharNoTyp OF LbeStd . LimitedCharNoSignedTyp 

  ; VAR MteNewChars1 : ARRAY LbeStd . LimitedCharNoTyp OF CHAR 
  ; VAR MteNewChars2 : ARRAY LbeStd . LimitedCharNoTyp OF CHAR

  ; VAR MteMarkCt : INTEGER := 0
    (* As of 2022-9-26, this is computed but not used.  But it may have
       value for debugging, so it's left in.  It's the number of times
       a new mark is built, during backwards traversal, only the temporally
       last (leftmost) of which will be used, the others being overlaid
       in NewBolTokMark. 
    *) 

  (* Recursive traversal of format syntax trees. *) 

  ; PROCEDURE MteTraverseEst 
      ( EstRef : LbeStd . EstRootTyp 
      ; KindSet : EstHs . EstChildKindSetTyp 
      ; EstAbsNodeNo : LbeStd . EstNodeNoTyp 
      ; ParentFsNodeRef : LangUtil . FsNodeRefTyp 
      ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
      ; EstFmtKind : LangUtil . FmtKindTyp 
        (* ^Only meaningful during forward traversal. *) 
      ; EstIndentPos1 : LbeStd . LimitedCharNoTyp 
      ; EstIndentPosN : LbeStd . LimitedCharNoTyp 
      (* EstIndentPos1 and EstIndentPosN are maintained assuming
         the Est subtree is formatted vertically. *) 
      ; VAR NewEstRef : LbeStd . EstRootTyp 
      ; VAR NewSingletonOptKindSet : EstHs . EstChildKindSetTyp 
            (* ^This is always either empty or contains only 
               EstChildKindOptSingletonList.  Its only function is to signal
               whether NewEstRef is for a singleton-optimized list, in which
               case, it is really the one list element.  Using a KindSet here
               is handier than a BOOLEAN.  
            *) 
      ; VAR LMCharPos : LbeStd . LimitedCharNoTyp 
        (* ^Position of leftmost nonblank of the subtree.  
           LimitedCharNoUnknown, if none exists. *) 
      ) 
      RAISES { Backout , Thread . Alerted } 

    (* New Est building. *) 

    (* New Ests are built right to left, during the backwards pass.
       There are several levels of laziness in building new Ests.  We
       use "wait" as a transitive verb to mean put into a waiting
       state.
 
       Initiating a merge is postponed while a suffix of the old Est
       is being collected.  MteTeMergeIsStarted tells when a merge has
       been started.  Each child to be merged must wait until its
       temporal successor (spatial predecessor) is known, in order to
       determine whether the current child is the start of a group of
       Est children all with the same format number.

       When MteTeChildIsWaiting, MteTeWaitingRef is a single child in
       this postponed state, MteTeWaitingKindSet is its kind set,
       MteTeWaitingEdgeKind is its edge kind, and MteTeWaitingFmtNo is
       its format number. This is only done for a newly constructed
       Est child or an unchanged element of a singleton-optimized list. 
       We need MteTeChildIsWaiting too, because a NIL can be waiting to 
       be merged.
 
       If MteTeWaitingFromChildNo # LbeStd . EstChildNoNull, the slice
       MteTeWaitingFromChildNo TO MteTeWaitingToChildNo of
       MteTeEstTravInfo . EtiParentRef are waiting to be merged.
       MteTeWaitingKindSet is a cached copy of the kind set of the
       leftmost child of this range, i.e. the one with
       MteTeWaitingFromChildNo, and MteTeWaitingEdgeKind is a cached
       copy of the edge kind of the same child.  MteTeWaitingFmtNo is
       the format number of this leftmost child.

       MteTeWaitingFromChildNo # LbeStd . EstChildNoNull and
       MteTeChildIsWaiting are mutually exclusive.
 
       MteNewDelThruFmtNo # LbeStd . FmtNoNull implies format numbers
       MteNewDelFromFmtNo .. MteNewDelThruFmtNo need an as-yet
       unconstructed new deletion mod.  This will go to the left of
       any waiting child or slice.  Building the deletion mod is
       postponed so as many format numbers as possible can be included
       in one delete mod.  When it finally needs to be constructed,
       there is still a possibility that the left sib of a waiting
       slice will happen to match the needed delete mod.  In this
       case, the waiting slice is just extended on its left, to reuse
       the sib.  MteNewDelIsRepair means set ModDelIsRepair TRUE in
       the new ModDel, while MteNewDelIsNotRepair means set
       ModDelIsRepair to FALSE.  Neither of these means it doesn't
       matter, which happens for a delete of a line break.  Both can't
       happen.
 
       Alternatively, MteNewBlankLineFmtNo # LbeStd . FmtNoNull
       implies an unconstructed blank line mod with FmtNo
       MteNewBlankLineFmtNo and line count MteNewBlankLineCt is
       waiting.  This blank line mode will go to the left of any
       waiting item or slice.  Building it is postponed so as many
       blank lines as possible can be included in one blank line mod.
       When it finally needs to be constructed, there is still a
       possibility that the left sib of a waiting slice will happen to
       match the needed blank line mod.  In this case, the waiting
       slice is just extended on its left, to reuse the sib.
       
       MteNewDelThruFmtNo # LbeStd . FmtNoNull and
       MteNewBlankLineFmtNo # LbeStd . FmtNoNull are mutually
       exclusive.

       When no children are waiting, MteTeWaitingFmtNo is the value
       for the leftmost child already merged into the being-built new
       Est node, or EstHs . FmtNoNull, if none.
 
       The text mods and/or blank line mods to be inserted for the new
       text are waited (for merging) when built.  In all cases but
       one, building of these happens to be followed immediately by a
       call to wait them.  However, when building occurs while inside
       an Est subtree that is an Ast leaf, building/waiting of these
       is postponed until we are back up a level in the Est, where
       they will be merged not as children of the Ast leaf, but as
       (replacement) sibs of it.
 
    *) 

    = VAR MteTeNewEstRef : LbeStd . EstRootTyp 
    ; VAR MteTeStartFmtNo : EstHs . FmtNoTyp 
    ; VAR MteTeEstTravInfo : TravUtil . EstTravInfoTyp 
    ; VAR MteTeRMChildRef : LbeStd . EstRootTyp 
    ; VAR MteTeRMChildRelNodeNo : LbeStd . EstNodeNoTyp 
          (* MteTeRMChildRef and MteTeRMChildRelNodeNo both trail
             behind the current child of MteTeEstTravInfo, during
             forward movement, whenever that child moves
             rightward. The only time they are guaranteed to denote
             the rightmost child is when we are going forward and the
             current child is off the right end, at which time, they
             denote the rightmost real child, hence the name RM. *)
    ; VAR MteTeRightChildRelNodeNo : LbeStd . EstNodeNoTyp 
          (* MteTeRightChildRelNodeNo temporally trails behind the current 
             child of the old Est node, when in state 
             MteStateBwdNl, and thus identifies the child to the right of 
             the current one. It is used to construct TokMarks. *)
    ; VAR MteTeRightChildRef : LbeStd . EstRootTyp
          (* ^The child that has MteTeRightChildRelNodeNo. *) 
    ; VAR MteTeLMNewChildRef : LbeStd . EstRootTyp 
    ; VAR MteTeLMNewKindSet : EstHs . EstChildKindSetTyp 
          (* ^During backwards traversal, the currently leftmost child
             and its kind set, already merged into the being-constructed
             new Est node. 
          *) 
    ; VAR MteTeContainsFill : BOOLEAN 
    ; VAR MteTeInterestingChildrenExist : BOOLEAN 
          (* This is set TRUE by any real Est child other than a delete mod 
             or a dummy.  It is also set TRUE by a non-deleted insertion token,
             since these represent visible children of the expanded tree.
             If there are no interesting children, an entire Est can be
             NIL or omitted. *) 
    ; VAR MteTeNILChildrenExist : BOOLEAN 
          (* This is only accurate if the new Est has exactly one child. 
             This is the only case we care about, and it is a lot easier 
             to compute. *) 
    ; VAR MteTeSibFmtNoMarkExists : BOOLEAN  
    ; VAR MteTeDoPatchLeftSibFmtNoMark : BOOLEAN 
          (* ^When we construct a LeftSibFmtNo mark, the actual node it leads
             to is not constructed/identified yet.  This triggers patching 
             the EstNodeCt field of the mark later, when the node is known.
             (Its NodeNo can be anticipated, so is set at the time of creation.)
          *)  
    ; VAR MteTeMergeIsStarted : BOOLEAN 
    ; VAR MteTeChildIsWaiting : BOOLEAN 
    ; VAR MteTeMergeState : EstBuild . MergeStateTyp 
    ; VAR MteTeWaitingRef : LbeStd . EstRootTyp 
    ; VAR MteTeWaitingFmtNo : EstHs . FmtNoTyp 
    ; VAR MteTeWaitingEdgeKind : EstHs . EdgeKindTyp 
    ; VAR MteTeWaitingFromChildNo : LbeStd . EstChildNoTyp 
      (* ^ = LbeStd . EstChildNoNull, if no children of EtiParentRef
             are waiting. *) 
    ; VAR MteTeWaitingToChildNo : LbeStd . EstChildNoTyp 
    ; VAR MteTeWaitingKindSet : EstHs . EstChildKindSetTyp 
      (* ^For the leftmost child, if MteTeWaitingFromChildNo 
          # LbeStd . EstChildNoNull . *) 
    ; VAR MteTeCfInsTokFromPos : LbeStd . LimitedCharNoSignedTyp 
    ; VAR MteTeCfInsTokToPos : LbeStd . LimitedCharNoSignedTyp 
    ; VAR MteTeCfEstChildFromPos : LbeStd . LimitedCharNoSignedTyp 
    ; VAR MteTeCfEstChildToPos : LbeStd . LimitedCharNoSignedTyp 
      (* MteTeCf* variables allow expanding the touched region to
         cover all insertion tokens of a conditional format subtree,
         if there is a possibility that its Est child could change
         from non-empty to empty.  If this happens, the predicate
(* CHECK:     ^ Does this need to be generalized for full predicate system? *)
         can change, causing the insertion tokens to be omitted.
         Touching them makes them get included in the ModText.
         MteTeCfInsTokFromPos is the from position of the leftmost
         conditional insertion.  MteTeCfInsTokToPos is the to position
         of the rightmost conditional insertion. MteTeCfEstChildFromPos 
         and MteTeCfEstChildToPos are for the text given by the Est
         child.  These values are saved during forward traversing.  
         When the end of the parent Est is reached, or an additional
         conditional format subtree is reached, they are used to 
         do the expand.  Waiting as long as possible avoids some cases
         of expanding unnecessarily, by giving a chance for MteTouchedToPos
         to be set by blanks, after the end of the last conditional
         format insertion token.  MteTeCfInsTokFromPos=CharNoInfinity
         means this mechanism is not being used.  It is not used for
         conditional format subtrees that do not insert their tokens. 
         MteTeCfInsTokFromPos=CharNoUnknown means we are waiting to
         forward scan the leading blanks of the LM conditional insertion. *)   
    ; VAR MteTeRMFsChildNo : LangUtil . FsChildNoSignedTyp 
          (* ^Only used when we have an FsList rule. *) 
    ; VAR MteTeIsFirstLine : BOOLEAN := TRUE   
    ; VAR MteTeIndentPos : LbeStd . LimitedCharNoTyp 
          := LbeStd . LimitedCharNoUnknown  
    (* MteTeIsFirstLine and MteTeIndentPos are consistently maintained only 
       when in descending and forward states, and as if the Est subtree were 
       formatted vertically.
    *)
    ; VAR MteTeParentIsTouched : BOOLEAN := FALSE 

    ; PROCEDURE MteTeIncEstChild ( ) 
      RAISES { Backout } 

      = BEGIN (* MteTeIncEstChild *) 
          MteTeRMChildRef := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
        ; MteTeRMChildRelNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
        ; TravUtil . IncEstChild ( MteTeEstTravInfo ) 
        END MteTeIncEstChild 

    ; PROCEDURE MteTeDecEstChild ( ) 
      RAISES { Backout } 

      = BEGIN (* MteTeDecEstChild *) 
          MteTeRightChildRelNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo
        ; MteTeRightChildRef := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef
        ; TravUtil . DecEstChild ( MteTeEstTravInfo ) 
        END MteTeDecEstChild 

    ; PROCEDURE MteTeEstChildrenAreToRightInNewEst ( ) : BOOLEAN 

      = BEGIN (* MteTeEstChildrenAreToRightInNewEst *) 
          RETURN 
            MteTeMergeIsStarted 
            OR MteTeChildIsWaiting 
            OR MteTeWaitingFromChildNo # LbeStd . EstChildNoNull 
            OR MteNewDelThruFmtNo # EstHs . FmtNoNull 
            OR MteNewBlankLineFmtNo # EstHs . FmtNoNull 
        END MteTeEstChildrenAreToRightInNewEst 

    ; <* UNUSED *> PROCEDURE MteTeBwdLMNewFmtNo ( ) : EstHs . FmtNoTyp 
      (* During backwards traversal, the format number of the item
         in the being-built Est child list to the right of where we
         are, i.e. leftmost of what has been waited or merged so far. *)  

      = BEGIN
          IF MteNewDelThruFmtNo # EstHs . FmtNoNull
          THEN RETURN MteNewDelFromFmtNo 
          ELSIF MteNewBlankLineFmtNo # EstHs . FmtNoNull 
          THEN RETURN MteNewBlankLineFmtNo 
          ELSE RETURN MteTeWaitingFmtNo 
          END 
        END MteTeBwdLMNewFmtNo

    (* Flushing waiting items for rebuilt Ests. *) 

    ; PROCEDURE MteTeFlushChild 
        ( NewFmtNo : EstHs . FmtNoTyp ; NewEdgeKind : EstHs . EdgeKindTyp ) 
      RAISES { Backout } 

      = BEGIN (* MteTeFlushChild *) 
          IF MteTeChildIsWaiting 
          THEN  
            IF NOT MteTeMergeIsStarted 
            THEN 
              MteTeMergeState 
                := EstBuild . NewMergeState 
                     ( Lang
                     , RootFsNodeRef . FsTok 
                     , EstRefToInheritFrom := MteTeEstTravInfo . EtiParentRef 
                       (* Do not recompute WidthInfo or SyntTokCt, so trees
                          still format the same. *) 
                     ) 
            ; MteTeMergeIsStarted := TRUE 
            END (* IF *) 
          ; EstBuild . MergeChild 
              ( MteTeMergeState 
              , MteTeWaitingRef 
              , MteTeWaitingKindSet 
              , IsFirstOfGroup 
                  := EstHs . IsFirstOfGroup 
                       ( NewFmtNo 
                       , NewEdgeKind 
                       , MteTeWaitingFmtNo 
                       , MteTeWaitingEdgeKind  
                       )  
              , GroupFmtNo := MteTeWaitingFmtNo 
              )
          ; MteTeLMNewChildRef := MteTeWaitingRef  
          ; MteTeLMNewKindSet := MteTeWaitingKindSet 
          ; IF MteTeDoPatchLeftSibFmtNoMark 
            THEN 
              NewBolTokMark . EstNodeCt 
                := EstUtil . EstNodeCt ( MteTeWaitingRef ) 
                   + ORD ( EstHs . EstChildKindOptSingletonList 
                           IN MteTeWaitingKindSet 
                         ) 
            ; NewBolTokMark . TkmEstRef := MteTeWaitingRef  
            ; MteTeDoPatchLeftSibFmtNoMark := FALSE 
            END (* IF *) 
          ; MteTeWaitingRef := NIL (* Defensive *)  
          ; MteTeChildIsWaiting := FALSE 
          END (* IF *) 
        END MteTeFlushChild 

    ; PROCEDURE MteTeFlushSliceOrChild 
        ( NewFmtNo : EstHs . FmtNoTyp ; NewEdgeKind : EstHs . EdgeKindTyp ) 
      RAISES { Backout } 

      = BEGIN (* MteTeFlushSliceOrChild *) 
          IF MteTeWaitingFromChildNo # LbeStd . EstChildNoNull  
          THEN 
            IF NOT MteTeMergeIsStarted 
            THEN 
              MteTeMergeState 
                := EstBuild . NewMergeState 
                     ( Lang 
                     , RootFsNodeRef . FsTok 
                     , EstRefToInheritFrom := MteTeEstTravInfo . EtiParentRef 
                       (* Do not recompute WidthInfo or SyntTokCt, so trees
                          still format the same. *) 
                     ) 
            ; MteTeMergeIsStarted := TRUE 
            END (* IF *) 
          ; IF MteTeEstTravInfo . EtiIsOptSingletonList 
            THEN
              Assert
                ( MteTeWaitingFromChildNo = 0
                  AND MteTeWaitingToChildNo = 1  
                , AFT . A_MteTeFlushSliceOrChild_bad_singleton_opt_list_slice
                ) 
            ; EstBuild . MergeChild 
                ( MteTeMergeState 
                , MteTeEstTravInfo . EtiChildLeafElem . LeChildRef  
                , MteTeEstTravInfo . EtiChildLeafElem . LeKindSet
                , IsFirstOfGroup := TRUE 
                , GroupFmtNo := EstHs . FmtNoListEstChild 
                )
            ; MteTeLMNewChildRef 
                := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef
            ; MteTeLMNewKindSet
                := MteTeEstTravInfo . EtiChildLeafElem . LeKindSet
            ELSE 
              EstBuild . MergeSlice 
                ( MteTeMergeState 
                , MteTeEstTravInfo . EtiParentRef  
                , MteTeWaitingFromChildNo 
                , MteTeWaitingToChildNo 
                , SetFirstOfGroupAndFmtNo := TRUE  
                , IsFirstOfGroup 
                    := EstHs . IsFirstOfGroup 
                         ( NewFmtNo 
                         , NewEdgeKind 
                         , MteTeWaitingFmtNo 
                         , MteTeWaitingEdgeKind  
                         )  
                , GroupFmtNo := MteTeWaitingFmtNo 
                , LeftmostNewChildRef := MteTeLMNewChildRef  
                , LeftmostNewKindSet := MteTeLMNewKindSet 
                ) 
            END (* IF *) 
          ; IF MteTeDoPatchLeftSibFmtNoMark 
            THEN 
              NewBolTokMark . EstNodeCt 
                := EstUtil . EstNodeCt ( MteTeLMNewChildRef ) 
                   + ORD ( EstHs . EstChildKindOptSingletonList 
                           IN MteTeLMNewKindSet 
                         )
            ; NewBolTokMark . TkmEstRef := MteTeLMNewChildRef  
            ; MteTeDoPatchLeftSibFmtNoMark := FALSE 
            END (* IF *) 
          ; MteTeWaitingFromChildNo := LbeStd . EstChildNoNull 
          ; MteTeWaitingToChildNo := LbeStd . EstChildNoNull (* Defensive *) 
          ELSE 
            MteTeFlushChild ( NewFmtNo , NewEdgeKind ) 
          END (* IF *) 
        END MteTeFlushSliceOrChild 

    ; PROCEDURE MteTeFindLeftSib 
        ( VAR Exists : BOOLEAN 
        ; VAR EstChildNo : LbeStd . EstChildNoTyp 
        ; VAR ChildLeafElem : EstHs . LeafElemTyp 
        ; VAR FmtNo : EstHs . FmtNoTyp 
        ) 
      RAISES { Backout } 
      (* Find the left sib of the leftmost already waiting Est child. *) 

      = VAR LNodeNo : LbeStd . EstNodeNoTyp 
      ; VAR LFirstOfGroupChildNo : LbeStd . EstChildNoTyp 
      ; VAR LFirstOfGroupLeafElem : EstHs . LeafElemTyp 

      ; BEGIN (* MteTeFindLeftSib *) 
          IF MteTeWaitingFromChildNo > 0 
          THEN (* A slice that has a left sib is waiting. *) 
            Exists := TRUE 
          ; EstChildNo := MteTeWaitingFromChildNo - 1 
          ; IF EstChildNo = MteTeEstTravInfo . EtiChildNo 
            THEN (* The current child is the left sib, reuse its values. *) 
              ChildLeafElem := MteTeEstTravInfo . EtiChildLeafElem 
            ; FmtNo := MteTeEstTravInfo . EtiChildFmtNo 
            ELSE (* Must get leaf elem and format number. *) 
              EstUtil . GetIthChild 
                ( MteTeEstTravInfo . EtiParentRef 
                , EstChildNo 
                , (* VAR *) ResultChildRelNodeNo := LNodeNo (* Dead. *)  
                , (* VAR *) ResultLeafElem := ChildLeafElem 
                ) 
            ; IF EstHs . EstChildKindFirstOfGroup IN MteTeWaitingKindSet 
              THEN 
                EstUtil . PrevInKindSet 
                  ( MteTeEstTravInfo . EtiParentRef 
                  , EstChildNo 
                  , EstHs . EstChildKindSetFirstOfGroup 
                  , (* VAR *) LFirstOfGroupChildNo 
                  , (* VAR *) LNodeNo (* Dead. *) 
                  , (* VAR *) LFirstOfGroupLeafElem 
                  ) 
              ; Assert 
                  ( 0 <= LFirstOfGroupChildNo 
                  , AFT . A_MteTeFindLeftSibNotFirstOfGroup 
                  ) 
              ; FmtNo := LFirstOfGroupLeafElem . LeFmtNo 
              ELSE 
                FmtNo := MteTeWaitingFmtNo 
              END (* IF *) 
            END (* IF *) 
          ELSE 
            Exists := FALSE 
          END (* IF *) 
        END MteTeFindLeftSib 

    ; PROCEDURE MteTeFlushDels ( ) RAISES { Backout } 

      = VAR LExists : BOOLEAN 
      ; VAR LEstChildNo : LbeStd . EstChildNoTyp 
      ; VAR LModDelRef : ModHs . ModDelTyp 
      ; VAR LFmtNo : EstHs . FmtNoTyp 
      ; VAR LChildLeafElem : EstHs . LeafElemTyp 
      ; VAR LDoPatchLeftSibFmtNoMark : BOOLEAN 

      ; BEGIN (* MteTeFlushDels *) 
          IF MteNewDelThruFmtNo # EstHs . FmtNoNull 
          THEN (* Some deleted format numbers are waiting. 
                  Turn them into a new or reused real ModDel. *) 
            MteTeFindLeftSib 
              ( LExists , LEstChildNo , LChildLeafElem , LFmtNo ) 
          ; IF LExists 
            THEN 
              TYPECASE LChildLeafElem . LeChildRef 
              OF NULL 
              => 
              | ModHs . ModDelTyp ( TModDel ) 
              => (* Left sib is a delete mod. *) 
                 IF LFmtNo = MteNewDelFromFmtNo 
                    AND TModDel . ModDelThruFmtNo = MteNewDelThruFmtNo 
                    AND MteNewDelIsRepair 
                        <= (* IMPLIES *) TModDel . ModDelIsRepair  
                    AND MteNewDelIsNotRepair 
                        <= (* IMPLIES *) ( NOT TModDel . ModDelIsRepair ) 
                 THEN (* The left sib is the needed delete mod. 
                         Include it in the slice to reuse it. *) 
                   MteTeWaitingFromChildNo := LEstChildNo 
                 ; MteTeWaitingKindSet := LChildLeafElem . LeKindSet 
                 ; MteTeWaitingFmtNo := LFmtNo 
                 ; MteTeWaitingEdgeKind 
                     := EstHs . EdgeKindTyp . EdgeKindLeadingMod  
                 ; MteNewDelThruFmtNo := EstHs . FmtNoNull 
                 ; MteNewDelFromFmtNo 
                     := EstHs . FmtNoNull (* Defensive *) 
                 ; IF MteTeDoPatchLeftSibFmtNoMark 
                   THEN 
                     NewBolTokMark . EstNodeCt := 1  
                   ; NewBolTokMark . TkmEstRef := TModDel  
                   ; MteTeDoPatchLeftSibFmtNoMark := FALSE 
                   END (* IF *) 
                 ; MteNewDelIsRepair := FALSE (* Defensive *) 
                 ; MteNewDelIsNotRepair := FALSE (* Defensive *) 
                 ; RETURN 
                 END (* IF *) 
              ELSE 
              END (* TYPECASE *) 
            END (* IF LExists *) 
          (* Finding a sib of a slice didn't work. 
             Construct a new deletion mod and wait it. *) 
          ; LDoPatchLeftSibFmtNoMark := MteTeDoPatchLeftSibFmtNoMark 
          ; MteTeDoPatchLeftSibFmtNoMark := FALSE 
          ; MteTeFlushSliceOrChild 
              ( MteNewDelFromFmtNo , EstHs . EdgeKindTyp . EdgeKindModDel ) 
          ; LModDelRef := NEW ( ModHs . ModDelTyp ) 
          ; LModDelRef . ModDelThruFmtNo := MteNewDelThruFmtNo 
          ; Assert
              ( NOT ( MteNewDelIsRepair AND MteNewDelIsNotRepair ) 
              , AFT . A_MteTeFlushDels_ContradictoryIsRepair  
              ) 
          ; LModDelRef . ModDelIsRepair := MteNewDelIsRepair  
          ; MteTeWaitingRef := LModDelRef 
          ; MteTeChildIsWaiting := TRUE 
          ; IF MteNewDelIsRepair 
            THEN 
              MteTeWaitingKindSet 
                := EstHs . EstChildKindSetInsertionRepairModDel 
            ELSE MteTeWaitingKindSet := EstHs . EstChildKindSetEditModDel 
            END (* IF *) 
          ; MteTeWaitingFmtNo := MteNewDelFromFmtNo 
          ; MteTeWaitingEdgeKind := EstHs . EdgeKindTyp . EdgeKindLeadingMod  
          ; IF LDoPatchLeftSibFmtNoMark 
            THEN 
              NewBolTokMark . EstNodeCt := 1  
            ; NewBolTokMark . TkmEstRef := LModDelRef 
            END (* IF *) 
          ; MteNewDelThruFmtNo := EstHs . FmtNoNull 
          ; MteNewDelFromFmtNo := EstHs . FmtNoNull (* Defensive *) 
          ; MteNewDelIsRepair := FALSE (* Defensive *) 
          ; MteNewDelIsNotRepair := FALSE (* Defensive *) 
          END (* IF *) 
        END MteTeFlushDels 

    ; PROCEDURE MteTeFlushBlankLines ( ) : ModHs . ModBlankLineTyp
      RAISES { Backout } 

      = VAR LExists : BOOLEAN 
      ; VAR LEstChildNo : LbeStd . EstChildNoTyp 
      ; VAR LModBlankLineRef : ModHs . ModBlankLineTyp 
      ; VAR LFmtNo : EstHs . FmtNoTyp 
      ; VAR LChildLeafElem : EstHs . LeafElemTyp 
      ; VAR LDoPatchLeftSibFmtNoMark : BOOLEAN 

      ; BEGIN (* MteTeFlushBlankLines *) 
          IF MteNewBlankLineFmtNo = EstHs . FmtNoNull
          THEN RETURN NIL 
          ELSE (* Some unconstructed blank lines are waiting.
                  Turn them into a new or reused ModBlankLine. *) 
            MteTeFindLeftSib (* Of the leftmost already waiting Est child. *) 
              ( LExists , LEstChildNo , LChildLeafElem , LFmtNo ) 
          ; IF LExists 
            THEN 
              TYPECASE LChildLeafElem . LeChildRef 
              OF NULL 
              => 
              | ModHs . ModBlankLineTyp ( TModBlankLine ) 
              => (* Left sib is a blank line mod *) 
                 IF LFmtNo = MteNewBlankLineFmtNo 
                    AND TModBlankLine . ModBlankLineCt = MteNewBlankLineCt 
                 THEN (* The left sib is the needed blank line mod. 
                         Include it in the slice to reuse it. *) 
                   MteTeWaitingFromChildNo := LEstChildNo 
                 ; MteTeWaitingKindSet := LChildLeafElem . LeKindSet 
                 ; MteTeWaitingFmtNo := LFmtNo 
                 ; MteTeWaitingEdgeKind 
                     := EstHs . EdgeKindTyp . EdgeKindLeadingMod  
                 ; IF MteTeDoPatchLeftSibFmtNoMark 
                   THEN 
                     NewBolTokMark . EstNodeCt := 1
                   ; NewBolTokMark . TkmEstRef := TModBlankLine 
                   ; MteTeDoPatchLeftSibFmtNoMark := FALSE 
                   END (* IF *) 
                 ; MteNewBlankLineFmtNo := EstHs . FmtNoNull 
                 ; MteNewBlankLineCt := 0 (* Defensive. *) 
                 ; RETURN TModBlankLine 
                 END (* IF *) 
              ELSE 
              END (* TYPECASE *) 
            END (* IF LExists *) 
          (* Finding a sib of a slice didn't work. 
             Construct a new blank line mod and wait it. *) 
          ; LDoPatchLeftSibFmtNoMark := MteTeDoPatchLeftSibFmtNoMark 
          ; MteTeDoPatchLeftSibFmtNoMark := FALSE 
          ; MteTeFlushSliceOrChild 
              ( MteNewBlankLineFmtNo 
              , EstHs . EdgeKindTyp . EdgeKindLeadingMod 
              ) 
          ; LModBlankLineRef := NEW ( ModHs . ModBlankLineTyp ) 
          ; LModBlankLineRef . ModBlankLineCt := MteNewBlankLineCt 
          ; MteTeWaitingRef := LModBlankLineRef 
          ; MteTeChildIsWaiting := TRUE 
          ; MteTeWaitingKindSet := EstHs . EstChildKindSetModBlankLine 
          ; MteTeWaitingFmtNo := MteNewBlankLineFmtNo 
          ; MteTeWaitingEdgeKind := EstHs . EdgeKindTyp . EdgeKindLeadingMod  
          ; MteTeInterestingChildrenExist := TRUE 
          ; IF LDoPatchLeftSibFmtNoMark 
            THEN
              NewBolTokMark . EstNodeCt := 1
            ; NewBolTokMark . TkmEstRef := LModBlankLineRef 
            END (* IF *) 
          ; MteNewBlankLineFmtNo := EstHs . FmtNoNull 
          ; MteNewBlankLineCt := 0 (* Defensive. *)
          ; RETURN LModBlankLineRef 
          END (* IF *) 
        END MteTeFlushBlankLines 

    (* Waiting items for rebuilt Ests. *) 

    ; PROCEDURE MteTeWaitBlankLines 
        ( BlankLineCt : LbeStd . LineNoTyp ; FmtNo : EstHs . FmtNoTyp ) 
      RAISES { Backout } 

      = BEGIN (* MteTeWaitBlankLines *) 
          IF BlankLineCt > 0 
          THEN 
            MteTeFlushDels ( ) 
          ; IF MteNewBlankLineFmtNo # FmtNo 
            THEN 
              EVAL MteTeFlushBlankLines ( ) 
            ; MteNewBlankLineCt := BlankLineCt 
            ; MteNewBlankLineFmtNo := FmtNo 
            ELSE 
              INC ( MteNewBlankLineCt , BlankLineCt ) 
            END (* IF *) 
          END (* IF *) 
        END MteTeWaitBlankLines 

    ; PROCEDURE MteTeIncludeInModDel 
        ( FmtNo : EstHs . FmtNoTyp 
        ; IsRepair := FALSE 
        ; IsNotRepair := FALSE 
        (* Neither IsRepair nor IsNotRepair means it doesn't matter *) 
        ) 
      RAISES { Backout } 
      (* Arrange for this item to be included in a delete mod. *) 

      = BEGIN (* MteTeIncludeInModDel *) 
          IF MteNewDelThruFmtNo # EstHs . FmtNoNull 
             AND ( IsRepair AND MteNewDelIsNotRepair 
                   OR IsNotRepair AND MteNewDelIsRepair
                 ) 
          THEN (* Range with conflicting IsRepair is waiting. *) 
            MteTeFlushDels ( ) 
          END (* IF *) 
        ; IF MteNewDelThruFmtNo = EstHs . FmtNoNull 
          THEN (* No range is waiting, start one. *)  
            EVAL MteTeFlushBlankLines ( ) 
          ; MteNewDelThruFmtNo := FmtNo 
          ; MteNewDelFromFmtNo := FmtNo 
          ; MteNewDelIsRepair := IsRepair 
          ; MteNewDelIsNotRepair := IsNotRepair 
          ELSE (* Extend existing range *) 
            MteNewDelFromFmtNo := FmtNo 
          ; MteNewDelIsRepair := MteNewDelIsRepair OR IsRepair 
          ; MteNewDelIsNotRepair := MteNewDelIsNotRepair OR IsNotRepair 
          END (* IF *) 
        END MteTeIncludeInModDel 

    ; PROCEDURE MteTeWaitChild 
        ( EstRef : LbeStd . EstRootTyp 
        ; KindSet : EstHs . EstChildKindSetTyp 
        ; FmtNo : EstHs . FmtNoTyp 
        ; EdgeKind : EstHs . EdgeKindTyp  
        ) 
      RAISES { Backout } 
      (* Wait a new Est child. *) 

      = BEGIN (* MteTeWaitChild *) 
          IF EstRef = NIL 
          THEN 
            MteTeNILChildrenExist := TRUE 
          ELSE 
            Assert 
              ( NOT ISTYPE ( EstRef , ModHs . ModBlankLineTyp ) 
                AND NOT ISTYPE ( EstRef , ModHs . ModDelTyp ) 
              , AFT . A_MteTeWaitChildBadObjKind 
              ) 
          END (* IF *) 
        ; MteTeFlushDels ( ) 
        ; EVAL MteTeFlushBlankLines ( ) 
        ; MteTeFlushSliceOrChild ( FmtNo , EdgeKind ) 
        ; MteTeWaitingRef := EstRef 
        ; MteTeChildIsWaiting := TRUE 
        ; MteTeWaitingKindSet 
            := KindSet - EstHs . EstChildKindSetFirstOfGroup 
        ; MteTeWaitingFmtNo := FmtNo 
        ; MteTeWaitingEdgeKind := EdgeKind 
        ; MteTeInterestingChildrenExist := TRUE 
        END MteTeWaitChild 

    ; PROCEDURE MteTeFlushExceptSlice 
        ( NewFmtNo : EstHs . FmtNoTyp ; NewEdgeKind : EstHs . EdgeKindTyp ) 
      RAISES { Backout } 
      (* Flush anything that is waiting, except for a slice as the
         leftmost waiting thing. *) 

      = BEGIN
          IF MteNewDelThruFmtNo # EstHs . FmtNoNull 
             OR MteNewBlankLineFmtNo # EstHs . FmtNoNull 
             OR MteTeChildIsWaiting 
          THEN 
            MteTeFlushDels ( ) 
          ; EVAL MteTeFlushBlankLines ( ) 
          ; MteTeFlushChild ( NewFmtNo , NewEdgeKind ) 
          END 
        END MteTeFlushExceptSlice 

    ; PROCEDURE MteTeWaitExistingEstChild 
        ( EdgeKind : EstHs . EdgeKindTyp 
        ; IsInteresting : BOOLEAN 
        )  
      RAISES { Backout } 
      (* Wait the current Est child. *) 

      = VAR LNewEdgeKind : EstHs . EdgeKindTyp 

      ; BEGIN (* MteTeWaitExistingEstChild *) 
          IF MteTeEstTravInfo . EtiIsOptSingletonList 
          THEN 
            Assert 
              ( MteTeEstTravInfo . EtiChildNo = 0 
              , AFT . A_MteTeWaitExistingEstChild_singleton_opt_bad_child_no
              ) 
          ; MteTeWaitChild 
              ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              , MteTeEstTravInfo . EtiChildLeafElem . LeKindSet
              , EstHs . FmtNoListEstChild 
              , EstHs . EdgeKindTyp . EdgeKindEstChild
              ) 
          ; Assert 
              ( NOT MteTeDoPatchLeftSibFmtNoMark 
              , AFT . A_MteTeWaitExistingEstChild_Patch_left_sib_singleton_opt
              ) 
          ELSIF 0 <= MteTeEstTravInfo . EtiChildNo 
                AND MteTeEstTravInfo . EtiChildNo 
                    < MteTeEstTravInfo . EtiChildCt 
          THEN 
            LNewEdgeKind 
              := EstUtil . EstEdgeKind 
                   ( Lang , MteTeEstTravInfo . EtiChildLeafElem . LeChildRef ) 
          ; MteTeFlushExceptSlice 
              ( MteTeEstTravInfo . EtiChildFmtNo , LNewEdgeKind ) 
          ; IF MteTeWaitingFromChildNo # MteTeEstTravInfo . EtiChildNo + 1 
            THEN (* Must start new waiting slice. *) 
              MteTeFlushSliceOrChild 
                ( MteTeEstTravInfo . EtiChildFmtNo , LNewEdgeKind ) 
            ; MteTeWaitingToChildNo := MteTeEstTravInfo . EtiChildNo + 1 
            ELSIF MteTeDoPatchLeftSibFmtNoMark 
            THEN
              NewBolTokMark . TkmEstRef 
                := EstUtil . IthChildRef 
                     ( MteTeWaitingRef , MteTeWaitingFromChildNo ) 
            ; NewBolTokMark . EstNodeCt 
                := EstUtil . EstNodeCt ( NewBolTokMark . TkmEstRef )  
                   + ORD ( EstHs . EstChildKindOptSingletonList 
                           IN MteTeWaitingKindSet 
                         ) 
            ; MteTeDoPatchLeftSibFmtNoMark := FALSE 
            END (* IF *) 
          ; MteTeWaitingFromChildNo := MteTeEstTravInfo . EtiChildNo 
          ; MteTeWaitingFmtNo := MteTeEstTravInfo . EtiChildFmtNo 
          ; MteTeWaitingEdgeKind := EdgeKind 
          ; MteTeWaitingKindSet 
              := MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
          ; MteTeNILChildrenExist 
              := MteTeNILChildrenExist 
                 OR MteTeEstTravInfo . EtiChildLeafElem . LeChildRef = NIL 
          ; MteTeInterestingChildrenExist 
              := MteTeInterestingChildrenExist OR IsInteresting 
          END 
        END MteTeWaitExistingEstChild 

    ; PROCEDURE MteTeWaitChildrenToLeft ( ) RAISES { Backout } 
      (* Wait the range of existing Est children of the current parent node, 
         from the leftmost child through the current child. *) 

      = VAR LNewEdgeKind : EstHs . EdgeKindTyp 
      ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
      ; VAR LLeafElem : EstHs . LeafElemTyp 

      ; BEGIN (* MteTeWaitChildrenToLeft *) 
          IF MteTeEstTravInfo . EtiIsOptSingletonList 
          THEN 
            IF MteTeEstTravInfo . EtiChildNo >= 0 
            THEN 
              MteTeWaitChild 
                ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                , MteTeEstTravInfo . EtiChildLeafElem . LeKindSet
                , EstHs . FmtNoListEstChild 
                , EstHs . EdgeKindTyp . EdgeKindEstChild
                ) 
            ; Assert 
                ( NOT MteTeDoPatchLeftSibFmtNoMark 
                , AFT . A_MteTeWaitChildrenToLeft_Patch_left_sib_singleton_opt
                ) 
            END (* IF *) 
          ELSIF 0 <= MteTeEstTravInfo . EtiChildNo  
                AND MteTeEstTravInfo . EtiChildNo 
                    < MteTeEstTravInfo . EtiChildCt 
          THEN (* Children to left exist. *)  
            LNewEdgeKind 
              := EstUtil . EstEdgeKind 
                   ( Lang , MteTeEstTravInfo . EtiChildLeafElem . LeChildRef )
          ; MteTeFlushExceptSlice 
              ( MteTeEstTravInfo . EtiChildFmtNo , LNewEdgeKind )  
          ; IF MteTeWaitingFromChildNo # MteTeEstTravInfo . EtiChildNo + 1 
            THEN (* Must start new waiting slice. *) 
              MteTeFlushSliceOrChild 
                ( MteTeEstTravInfo . EtiChildFmtNo , LNewEdgeKind ) 
            ; MteTeWaitingToChildNo := MteTeEstTravInfo . EtiChildNo + 1 
            ELSIF MteTeDoPatchLeftSibFmtNoMark 
            THEN 
              NewBolTokMark . TkmEstRef 
                := EstUtil . IthChildRef 
                     ( MteTeWaitingRef , MteTeWaitingFromChildNo ) 
            ; NewBolTokMark . EstNodeCt 
                := EstUtil . EstNodeCt ( NewBolTokMark . TkmEstRef ) 
                   + ORD ( EstHs . EstChildKindOptSingletonList 
                           IN MteTeWaitingKindSet 
                         )
            ; MteTeDoPatchLeftSibFmtNoMark := FALSE 
            END (* IF *) 
          ; MteTeWaitingFromChildNo := 0 
          ; EstUtil . GetIthChild 
              ( MteTeEstTravInfo . EtiParentRef 
              , 0 
              , (* VAR *) ResultChildRelNodeNo := LNodeNo (* Dead. *) 
              , (* VAR *) ResultLeafElem := LLeafElem 
              ) 
          ; Assert 
              ( EstHs . EstChildKindFirstOfGroup IN LLeafElem . LeKindSet 
              , AFT . A_MteTeWaitChildrenToLeftNotFirstOfGroup 
              ) 
          ; MteTeWaitingFmtNo := LLeafElem . LeFmtNo 
          ; MteTeWaitingEdgeKind 
              := EstUtil . EstEdgeKind ( Lang , LLeafElem . LeChildRef )   
          ; MteTeWaitingKindSet := LLeafElem . LeKindSet 
          ; MteTeNILChildrenExist 
              := MteTeNILChildrenExist OR LLeafElem . LeChildRef = NIL 
            (* This could miss a NIL farther to the right, but we only 
               care about the presence of a NIL when there is exactly 
               one child in the new Est. *) 
          ; MteTeInterestingChildrenExist := TRUE 
          ELSE (* There are no children of the original Est subtree to our 
                  left, but we must flush anything waiting, to become new 
                  children to the left.  
               *) 
            MteTeFlushDels ( ) 
          ; EVAL MteTeFlushBlankLines ( ) 
          ; MteTeFlushSliceOrChild  
              ( EstHs . FmtNoNull 
              , EstHs . EdgeKindTyp . EdgeKindLeadingMod (* Irrelevant. *) 
              ) 
          END (* IF *) 
        END MteTeWaitChildrenToLeft 

    ; PROCEDURE MteTeWaitChildrenToRight ( ) RAISES { Backout } 
      (* Wait the range of existing Est children of the current parent node, 
         from the current child through the rightmost child. *) 

      = BEGIN (* MteTeWaitChildrenToRight *) 
          Assert 
            ( NOT MteTeEstChildrenAreToRightInNewEst ( ) 
              (* Because we only do this prior to making any changes to 
                 Est children. 
              *) 
            , AFT . A_MteTeWaitChildrenToRightAlreadyStarted 
            ) 
        ; IF MteTeEstTravInfo . EtiIsOptSingletonList 
          THEN 
            IF MteTeEstTravInfo . EtiChildNo <= 0 
            THEN 
              MteTeWaitChild 
                ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                , MteTeEstTravInfo . EtiChildLeafElem . LeKindSet
                , EstHs . FmtNoListEstChild 
                , EstHs . EdgeKindTyp . EdgeKindEstChild
                ) 
            ; Assert 
                ( NOT MteTeDoPatchLeftSibFmtNoMark 
                , AFT . A_MteTeWaitChildrenToRight_Patch_left_sib_singleton_opt
                ) 
            END (* IF *) 
          ELSIF 0 <= MteTeEstTravInfo . EtiChildNo 
                AND MteTeEstTravInfo . EtiChildNo 
                    < MteTeEstTravInfo . EtiChildCt 
          THEN 
            MteTeWaitingFromChildNo := MteTeEstTravInfo . EtiChildNo 
          ; MteTeWaitingToChildNo := MteTeEstTravInfo . EtiChildCt 
          ; MteTeWaitingFmtNo := MteTeEstTravInfo . EtiChildFmtNo 
          ; MteTeWaitingEdgeKind 
              := EstUtil . EstEdgeKind 
                   ( Lang , MteTeEstTravInfo . EtiChildLeafElem . LeChildRef ) 
          ; MteTeWaitingKindSet 
              := MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
          ; MteTeNILChildrenExist 
              := MteTeNILChildrenExist 
                 OR MteTeEstTravInfo . EtiChildLeafElem . LeChildRef = NIL 
            (* This could miss a NIL farther to the right, but we only 
               care about the presence of a NIL when there is exactly 
               one child in the new Est. *) 
          ; MteTeInterestingChildrenExist := TRUE 
          END (* IF *) 
        END MteTeWaitChildrenToRight 

    ; PROCEDURE MteTeWaitChildrenFartherToRight ( ) 
      RAISES { Backout } 
      (* Wait the range of existing Est children of the current parent node, 
         from the child to the right of the current child through the 
         rightmost child. *) 

      = VAR LEstChildNo : LbeStd . EstChildNoTyp 
      ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
      ; VAR LLeafElem : EstHs . LeafElemTyp 

      ; BEGIN (* MteTeWaitChildrenFartherToRight *) 
          Assert 
            ( NOT MteTeEstChildrenAreToRightInNewEst ( ) 
              (* Because we only do this prior to making any changes to 
                 Est children. 
              *) 
            , AFT . A_MteTeWaitChildrenFartherToRightAlreadyStarted 
            ) 
        ; IF MteTeEstTravInfo . EtiIsOptSingletonList 
          THEN 
            IF MteTeEstTravInfo . EtiChildNo < 0 
            THEN 
              MteTeWaitChild 
                ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                , MteTeEstTravInfo . EtiChildLeafElem . LeKindSet
                , EstHs . FmtNoListEstChild 
                , EstHs . EdgeKindTyp . EdgeKindEstChild
                ) 
             END (* IF *) 
          ELSE 
            LEstChildNo := MteTeEstTravInfo . EtiChildNo + 1 
          ; IF LEstChildNo < MteTeEstTravInfo . EtiChildCt 
            THEN 
              MteTeWaitingFromChildNo := LEstChildNo 
            ; MteTeWaitingToChildNo := MteTeEstTravInfo . EtiChildCt 
            ; EstUtil . GetIthChild 
                ( MteTeEstTravInfo . EtiParentRef 
                , LEstChildNo 
                , (* VAR *) ResultChildRelNodeNo := LNodeNo (* Dead. *)  
                , (* VAR *) ResultLeafElem := LLeafElem 
                ) 
            ; IF EstHs . EstChildKindFirstOfGroup IN LLeafElem . LeKindSet 
              THEN 
                MteTeWaitingFmtNo := LLeafElem . LeFmtNo 
              ELSE 
                MteTeWaitingFmtNo := MteTeEstTravInfo . EtiChildFmtNo 
              END (* IF *) 
            ; MteTeWaitingEdgeKind 
                := EstUtil . EstEdgeKind ( Lang , LLeafElem . LeChildRef )   
            ; MteTeWaitingKindSet := LLeafElem . LeKindSet 
            ; MteTeNILChildrenExist 
                := MteTeNILChildrenExist OR LLeafElem . LeChildRef = NIL 
              (* This could miss a NIL farther to the right, but we only 
                 care about the presence of a NIL when there is exactly 
                 one child in the new Est. *) 
            ; MteTeInterestingChildrenExist := TRUE 
            END (* IF *) 
          END (* IF *) 
        END MteTeWaitChildrenFartherToRight 

    (* Actual Est rebuilding *) 

    ; PROCEDURE MteTeNewModText 
        ( FromPos : LbeStd . LimitedCharNoTyp 
        ; ToPos : LbeStd . LimitedCharNoSignedTyp 
        ; LeftTokToPos : LbeStd . LimitedCharNoTyp 
        ; OrigToPos : LbeStd . LimitedCharNoTyp 
        ; READONLY Chars : ARRAY OF CHAR 
        ; TextLen : LbeStd . LimitedCharNoTyp 
        ) 
      : ModHs . ModTextTyp (* NIL => was a blank line. *) 
      RAISES { Backout } 

      = VAR LNewModRef : ModHs . ModTextTyp 
      ; VAR LFromSs : PortTypes . Int32Typ 
      ; VAR LThruSs : PortTypes . Int32Typ 

      ; BEGIN (* MteTeNewModText *) 
          LFromSs := 0 
        ; LThruSs := TextLen - 1 
        ; WHILE LFromSs <= LThruSs AND Chars [ LFromSs ] = ' ' 
          DO INC ( LFromSs ) 
          END (* WHILE *) 
        ; WHILE LFromSs <= LThruSs AND Chars [ LThruSs ] = ' ' 
          DO DEC ( LThruSs ) 
          END (* WHILE *) 
        ; IF LeftTokToPos = 0 
             AND ToPos = LbeStd . LimitedCharNoInfinity 
             AND LFromSs > LThruSs  
          THEN (* This is a blank line, possibly with some explicit blanks. *) 
           RETURN NIL 
          ELSE 
            LNewModRef := NEW ( ModHs . ModTextTyp ) 
          ; LNewModRef . ModTextFromPos := FromPos + LFromSs  
          ; LNewModRef . ModTextToPos := ToPos 
          ; LNewModRef . ModTextLeftTokToPos := LeftTokToPos 
          ; LNewModRef . ModTextOrigToPos := OrigToPos 
          ; IF LFromSs > LThruSs 
            THEN (* String is empty. *) 
              LNewModRef . ModTextStringRef := NIL 
            ; IF LNewModRef . ModTextToPos # LbeStd . LimitedCharNoInfinity 
              THEN
                LNewModRef . ModTextFromPos := LNewModRef . ModTextToPos 
              END 
            ELSE 
              LNewModRef . ModTextStringRef 
                := SharedStrings . FromArrayOfChar 
                     ( SUBARRAY ( Chars , LFromSs , LThruSs - LFromSs + 1 ) 
                     , LbeStd . Tok__ModText 
                     )
            END (* IF *) 
          ; RETURN LNewModRef 
          END (* IF *) 
        END MteTeNewModText 

    ; PROCEDURE MteTeMaybeFinishBwdEdit 
        ( FmtNo : EstHs . FmtNoTyp
        ; MarkNodeNo : LbeStd . EstNodeNoTyp
        ) 
      RAISES { Backout } 
      (* If now is the time to do it, go out of MteStateBwdEdit, build
         and wait whatever new Mods are required, count NewLinesCt for
         the built mods, set NewBolTokMark, set state to BwdNl or
         Done. *)

      = VAR LNewToPos : LbeStd . LimitedCharNoTyp 
      ; VAR LLeftNewModRef : ModHs . ModTextTyp 
            (* ^Leftmost new text mod, NIL if it turned out a blank line. *) 
      ; VAR LRightNewModRef : ModHs . ModTextTyp (* Rightmost. *)
      ; VAR LBlankLineModRef : ModHs . ModBlankLineTyp 
      ; VAR LNewBlankLinesBeforeCt : LbeStd . LineNoTyp 
      ; VAR LNewBlankLinesAfterCt : LbeStd . LineNoTyp 
      ; VAR LTextLen : LbeStd . CharNoTyp 

      ; BEGIN (* MteTeMaybeFinishBwdEdit *) 
          IF MteState = MteStateTyp . MteStateBwdEdit  
             (* ^Don't finish more than once. *) 
             AND ( MteItemCt <= MteFinishItemNo 
                 (* Finish if back to starting place. *) 
                   OR ( MteNewTextLeftTokToPos > 0 
                        (* Finish if there are nonblank things from the old 
                           Est to the left of the new mods (which will also 
                           need to be in the new Est), 
                        *)
                        AND MteCharPos <= MteTouchedFromPos 
                            (* and we are left of the new mods. *) 
                      ) 
                 ) 
          THEN (* Do the finish. *) 

          (* Build needed text modifiers. *) 
            IF InsNlPos = LbeStd . LimitedCharNoInfinity 
            THEN (* No inserted new line, only one ModText insertion. *) 
              LLeftNewModRef (* Will be NIL if it's all blanks. *)  
                := MteTeNewModText 
                     ( FromPos := MteTouchedFromPos 
                     , ToPos := MteNewTextToPos 
                     , LeftTokToPos := MteNewTextLeftTokToPos 
                     , OrigToPos := MteTouchedToPos 
                     , Chars := MteNewChars1 
                     , TextLen := MteNewTextLen 
                     ) 
            ; LNewBlankLinesBeforeCt 
                := MteBlankLinesBefore + ORD ( LLeftNewModRef = NIL ) 
            ; LNewBlankLinesAfterCt := MteBlankLinesAfter 
            ; LRightNewModRef := NIL 
            ; IF LLeftNewModRef = NIL 
                 AND DelNlShift # LbeStd . LimitedCharNoInfinity 
                 AND DelNlShift # 0 
                 AND MteBlankLinesAfter = 0 
              THEN (* There is a deleted Nl at the end of blank line mod. 
                      We must create a degenerate ModText in order 
                      to preserve the starting position DelNlShift, 
                      which controls the position of the text following. *) 
                LLeftNewModRef := NEW ( ModHs . ModTextTyp ) 
              ; LLeftNewModRef . ModTextFromPos := DelNlShift 
              ; LLeftNewModRef . ModTextToPos := DelNlShift 
              ; LLeftNewModRef . ModTextLeftTokToPos := MteNewTextLeftTokToPos 
              ; LLeftNewModRef . ModTextOrigToPos := DelNlShift 
              ; LLeftNewModRef . ModTextStringRef := NIL 
              END (* IF *) 
            ELSE (* There is a new line somewhere in the inserted text. *) 
              IF MteNewTextToPos = LbeStd . LimitedCharNoInfinity 
              THEN 
                LNewToPos := LbeStd . LimitedCharNoInfinity 
              ELSE 
                LNewToPos 
                  := EstUtil . WidthValue 
                       ( MteNewTextToPos 
                         - ( MteTouchedFromPos + MteNewTextRelNlPos ) 
                         + NlIndentPos 
                       ) 
              END (* IF *) 

            (* Build left new text mod. *) 
            ; LNewBlankLinesBeforeCt := MteBlankLinesBefore 
            ; IF MteNewTextRelNlPos > 0 (* Nonempty text before the Nl *) 
              THEN (* A ModText is needed for the 1st line. *) 
                LLeftNewModRef 
                  := MteTeNewModText 
                       ( FromPos := MteTouchedFromPos 
                       , ToPos := LbeStd . LimitedCharNoInfinity 
                                  (* ^Means new line after. *) 
                       , LeftTokToPos := MteNewTextLeftTokToPos 
                       , OrigToPos := MteTouchedToPos 
(* CHECK:                ^ What do we want here? Does it matter? *)  
                       , Chars := MteNewChars1 
                       , TextLen := MteNewTextRelNlPos 
                       )
              ; INC ( LNewBlankLinesBeforeCt , ORD ( LLeftNewModRef = NIL ) ) 
                (* ^The left text was, however, all blank. *) 
              ELSE (* The added Nl is at the left of the edited region. *) 
                INC ( LNewBlankLinesBeforeCt
                    , ORD ( MteNewTextLeftTokToPos = 0 ) 
                    ) 
              ; LLeftNewModRef := NIL 
              END (* IF *) 
            ; LNewBlankLinesAfterCt := MteBlankLinesAfter 
         (* ; IF MteNewTextRelNlPos = MteNewTextLen 
                 (* Inserted Nl at end of region. *) 
                 AND LLeftNewModRef # NIL 
                     (* We did not fold the inserted Nl into 
                        LNewBlankLinesBeforeCt *) 
              THEN
                IF LNewToPos = LbeStd . LimitedCharNoInfinity  
                THEN (* Need an additional blank line after *) 
                  INC ( LNewBlankLinesAfterCt ) 
                ELSE (* Change right new mod to have Nl after. *)  
                  LNewToPos := LbeStd . LimitedCharNoInfinity  
                END 
              END (* IF *) 
         *) 
            ; LTextLen := MteNewTextLen - MteNewTextRelNlPos 
            ; LRightNewModRef 
                := MteTeNewModText 
                     ( FromPos := NlIndentPos  
                     , ToPos := LNewToPos 
                     , LeftTokToPos := 0 (* Implies Nl before. *) 
                     , OrigToPos := MteTouchedToPos 
(* CHECK:              ^ What do we want here? Does it matter? *)  
                     , Chars := MteNewChars2 
                     , TextLen := LTextLen 
                     ) 
            ; INC ( LNewBlankLinesAfterCt , ORD ( LRightNewModRef = NIL ) ) 
            END (* IF *) 

          (* Insert new tree children and build a new mark. *) 
          ; IF LLeftNewModRef = NIL AND LRightNewModRef = NIL 
            THEN (* Edited text consists of blank lines only.  At most, a 
                    single blank line mod is needed. 
                 *) 
              IF LNewBlankLinesBeforeCt + LNewBlankLinesAfterCt > 0 
              THEN 
                MteTeWaitBlankLines 
                  ( LNewBlankLinesBeforeCt + LNewBlankLinesAfterCt , FmtNo ) 
              ; LBlankLineModRef := MteTeFlushBlankLines ( ) 
              ; NewBolTokMark 
                  := Marks . TokMarkTyp 
                       { EstNodeNo := MarkNodeNo
                       , EstNodeCt := 1 
                       , TkmEstRef := LBlankLineModRef 
                       , Kind := MarkKindTyp . BlankLine 
                       , FmtNo := FmtNo 
                       , StartAtEnd := FALSE 
                       , IsImpliedNewLine := FALSE 
                       , TmTok := LbeStd . Tok__BlankLine  
                       }
              ; INC ( MteMarkCt) 
              ; INC ( NewLinesCt ) 
              ; IF LNewBlankLinesBeforeCt = 0 
                THEN (* This mod starts at an inserted Nl. *)  
                  MteState := MteStateTyp . MteStateBwdNl  
                ELSE (* This is the replacement for the start Nl. *)  
                  MteState := MteStateTyp . MteStateDone 
                END 
              ELSE (* New edited text is empty. *) 
                MteState := MteStateTyp . MteStateBwdNl  
              END 
            ELSE (* We have at least one ModText *) 

            (* If necessary, insert trailing blank line mod. *) 
              IF LNewBlankLinesAfterCt # 0 
              THEN 
                MteTeWaitBlankLines ( LNewBlankLinesAfterCt , FmtNo ) 
              ; EVAL MteTeFlushBlankLines ( ) 
              (* No need to build a mark.  There will always be another
                 Nl to the left. *) 
              ; INC ( NewLinesCt ) 
              END (* IF *) 

            (* If necessary, insert the right text mod. *) 
            ; IF LRightNewModRef # NIL 
              THEN (* Insert the right text mod. *) 
                MteTeWaitChild 
                  ( LRightNewModRef 
                  , EstHs . EstChildKindSetModText 
                  , FmtNo 
                  , EstHs . EdgeKindTyp . EdgeKindLeadingMod  
                  )
              ; IF LRightNewModRef . ModTextLeftTokToPos = 0 (* Nl before *) 
                THEN (* This Nl is user-inserted, not the starting one. *)
                  INC ( NewLinesCt ) 
               (* NewBolTokMark (* Will always get overlaid. *) 
                    := Marks . TokMarkTyp 
                         { EstNodeNo := MarkNodeNo 
                           (* ^This could be the wrong value, but if
                               so, the whole mark will be overlaid. *) 
                         , EstNodeCt := 1   
                         , TkmEstRef := LRightNewModRef  
                         , Kind := MarkKindTyp . Plain 
                         , FmtNo := FmtNo 
                         , StartAtEnd := FALSE 
                         , IsImpliedNewLine := FALSE 
                         , TmTok := LbeStd . Tok__ModText 
                         } 
                  ; INC ( MteMarkCt) 
               *) 
                END (* IF *) 
              ; MteState := MteStateTyp . MteStateBwdNl (* Could change. *)
              END (* IF *) 

            (* If necessary, insert the left text mod. *) 
            ; IF LLeftNewModRef # NIL 
              THEN 
                MteTeWaitChild 
                  ( LLeftNewModRef 
                  , EstHs . EstChildKindSetModText 
                  , FmtNo 
                  , EstHs . EdgeKindTyp . EdgeKindLeadingMod  
                  ) 
              ; INC ( NewLinesCt ) 
              ; IF LLeftNewModRef . ModTextLeftTokToPos = 0 (* Nl before *)
                   AND LNewBlankLinesBeforeCt = 0
                THEN (* This replaces the original starting Nl. *)
                  NewBolTokMark 
                    := Marks . TokMarkTyp 
                         { EstNodeNo := MarkNodeNo 
                           (* ^This could be the wrong value, but if
                               so, the whole mark will be overlaid. *) 
                         , EstNodeCt := 1 
                         , TkmEstRef := LLeftNewModRef  
                         , Kind := MarkKindTyp . Plain 
                         , FmtNo := FmtNo 
                         , StartAtEnd := FALSE 
                         , IsImpliedNewLine := FALSE 
                         , TmTok := LbeStd . Tok__ModText 
                         } 
                ; INC ( MteMarkCt) 
                ; MteState := MteStateTyp . MteStateDone 
                ELSE   
               (* NewBolTokMark (* Will always get overlaid. *)
                    := Marks . TokMarkTyp 
                         { EstNodeNo := MarkNodeNo 
                           (* ^This could be the wrong value, but if
                               so, the whole mark will be overlaid. *) 
                         , EstNodeCt := 1 
                         , TkmEstRef := LLeftNewModRef  
                         , Kind := MarkKindTyp . Plain 
                         , FmtNo := FmtNo 
                         , StartAtEnd := FALSE 
                         , IsImpliedNewLine := FALSE 
                         , TmTok := LbeStd . Tok__ModText 
                         }
                ; INC ( MteMarkCt) 
                *)
                  MteState := MteStateTyp . MteStateBwdNl (* Could change. *)
                END (* IF *) 
              END (* IF *) 

            (* If necessary, insert the leading blank line mod. *) 
            ; IF LNewBlankLinesBeforeCt # 0 
              THEN (* Insert leading blank line mod. *) 
                MteTeWaitBlankLines ( LNewBlankLinesBeforeCt , FmtNo ) 
              ; LBlankLineModRef := MteTeFlushBlankLines ( ) 
              ; NewBolTokMark 
                  := Marks . TokMarkTyp 
                       { EstNodeNo := MarkNodeNo 
                       , EstNodeCt := 1
                       , TkmEstRef := LBlankLineModRef  
                       , Kind := MarkKindTyp . BlankLine 
                       , FmtNo := FmtNo 
                       , StartAtEnd := FALSE 
                       , IsImpliedNewLine := FALSE 
                       , TmTok := LbeStd . Tok__BlankLine 
                       } 
              ; INC ( MteMarkCt) 
              ; INC ( NewLinesCt ) 
              ; MteState := MteStateTyp . MteStateDone 
              (* This is the replacement for the starting Nl. *) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END MteTeMaybeFinishBwdEdit 

    ; PROCEDURE MteTeFinishMerge ( ) RAISES { Backout } 

      = VAR LNewEstRef : EstHs . EstRefTyp 
      ; VAR LEstChildRef : LbeStd . EstRootTyp 
      ; VAR LListTok : LbeStd . TokTyp 
      ; VAR LListMiscInfo , LElemMiscInfo : EstHs . EstMiscInfoTyp  

      ; BEGIN (* MteTeFinishMerge *) 
          IF MteTeEstTravInfo . EtiParentRef = NIL 
          THEN (* This is an Ast String node.  We never merge these 
                  at their own level, but postpone upward. *) 
          ELSE (* This is an interior Est node. *) 
            MteTeWaitChildrenToLeft ( ) 
          ; MteTeFlushExceptSlice 
              ( EstHs . FmtNoNull , EstHs . EdgeKindTyp . EdgeKindEstChild )
          ; IF MteTeWaitingFromChildNo # LbeStd . EstChildNoNull  
               AND MteTeDoPatchLeftSibFmtNoMark 
            THEN
              NewBolTokMark . TkmEstRef
                := EstUtil . IthChildRef 
                     ( MteTeWaitingRef , MteTeWaitingFromChildNo )  
            ; NewBolTokMark . EstNodeCt 
                := EstUtil . EstNodeCt ( NewBolTokMark . TkmEstRef )  
                   + ORD ( EstHs . EstChildKindOptSingletonList 
                           IN MteTeWaitingKindSet 
                         ) 
            ; MteTeDoPatchLeftSibFmtNoMark := FALSE 
            END (* IF *) 
          ; IF MteTeEstTravInfo . EtiIsOptSingletonList 
               AND NOT MteTeMergeIsStarted 
               AND MteTeWaitingFromChildNo = LbeStd . EstChildNoNull 
            THEN (* Unmodified optimized singleton list, let it pass up. *) 
              NewSingletonOptKindSet := EstHs . EstChildKindSetOptSingletonList 
            ELSIF MteTeWaitingFromChildNo = 0 
                  AND MteTeWaitingToChildNo = MteTeEstTravInfo . EtiChildCt 
                  AND NOT MteTeMergeIsStarted 
            THEN (* The entire original subtree is waiting, let it pass up. *) 
              EVAL ( MteTeNewEstRef ) (* For breakpoint. *) 
            ELSE  
              IF MteTeInterestingChildrenExist 
              THEN 
                MteTeFlushSliceOrChild 
                  ( EstHs . FmtNoNull 
                  , EstHs . EdgeKindTyp . EdgeKindEstChild 
                  )
              ; IF MteTeEstTravInfo . EtiChildCt > 0 
                   AND NOT MteTeMergeIsStarted 
                THEN (* Children were all removed.  Create an empty node. *) 
                  MteTeMergeState 
                    := EstBuild . NewMergeState 
                         ( Lang 
                         , RootFsNodeRef . FsTok 
                         , EstRefToInheritFrom 
                             := MteTeEstTravInfo . EtiParentRef 
                           (* Do not recompute WidthInfo or SyntTokCt, so trees
                              still format the same. *) 
                         ) 
                ; MteTeMergeIsStarted := TRUE 
                END (* IF *) 
              ; IF MteTeMergeIsStarted 
                THEN  
                  EstBuild . FinishMerge 
                    ( MteTeMergeState 
                    , ResultEstNodeKind 
                        := MteTeEstTravInfo . EtiParentRef . EstNodeKind 
                    , (* VAR *) ResultTreeRef := LNewEstRef 
                    ) 
                ; MteTeNewEstRef := LNewEstRef (* May change soon. *) 
                ; IF RootFsNodeRef . FsIsInsideList 
                     AND EstUtil . EstChildCt ( LNewEstRef ) = 1 
                  THEN (* We just built a list with exactly one child. *)  
                    IF MteTeNILChildrenExist
                       AND NOT MteTeSibFmtNoMarkExists  
                    THEN (* Replace a list with a single NIL child by a NIL *) 
                      MteTeNewEstRef := NIL 
                    ELSIF Options . DoOptimizeSingletonLists 
                    THEN 
                      LEstChildRef := EstUtil . IthChildRef ( LNewEstRef , 0 ) 
                    ; LListTok 
                        := LangUtil . OptSingletonListAsTok
                             ( ParentFsNodeRef 
                             , EstUtil . EstTok ( LEstChildRef ) 
                             ) 
                    ; IF LListTok # LbeStd . Tok__Null
                      THEN (* Singleton list optimizable by syntactic rules. *)
                        LListMiscInfo 
                          := EstUtil . EstMiscInfo ( Lang , MteTeNewEstRef )  
                      ; LElemMiscInfo 
                          := EstUtil . EstMiscInfo ( Lang , LEstChildRef )  
                      ; IF LListMiscInfo . EmiWidthInfo 
                           = LElemMiscInfo . EmiWidthInfo 
                        THEN (* Singleton list is optimizable. *)  
                          MteTeNewEstRef := LEstChildRef 
                        ; NewSingletonOptKindSet 
                            := EstHs . EstChildKindSetOptSingletonList 
                        ; Assertions . MessageText 
                            ( "MergeTxt optimized singlton list." ) 
                        ELSE (* Not optimizable, but only on account of 
                                unequal WidthInfo. 
                             *) 
                          Assertions . MessageText  
                            ( "Singleton list not optimized in MergeTxt because of unequal WidthInfo" ) 
                        END (* IF *) 
                      END (* IF *) 
                    END (* IF *) 
                  END (* IF *) 
                ELSE (* There were only insertion tokens.  This can happen
                        for a ModTok with no mods or an Ast node with only
                        insertion toks. 
                     *)
                  IF FALSE AND 
                     LangUtil . TokClass ( Lang , EstUtil . EstTok ( EstRef ) )
                     = LbeStd . TokClassTyp . TokClassVarTermMod 
                  THEN (* ModTok for with no children *) 
                    MteTeNewEstRef := NIL 
                  END (* IF *) 
                END (* IF *) 
              ELSE (* There are only ModDels and Dummies.  Replace this Est 
                      subtree by a NIL *)  
                MteTeNewEstRef := NIL 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END MteTeFinishMerge 

    (* String moving procedures. *) 

    ; PROCEDURE MteTeMoveSubstring
        ( READONLY SourceString : Strings . StringTyp 
        ; ModTextStartSs : LbeStd . LimitedCharNoTyp 
        ) 
      RAISES { Backout } 

      = VAR MteTeMsStartSs : LbeStd . LimitedCharNoTyp 

      ; PROCEDURE MteTeMs1 ( READONLY Chars : ARRAY OF CHAR ) 

        = BEGIN (* MteTeMs1 *) 
            SUBARRAY ( MteNewChars1 , ModTextStartSs , NUMBER ( Chars ) ) 
              := Chars 
          END MteTeMs1 

      ; PROCEDURE MteTeMs2 ( READONLY Chars : ARRAY OF CHAR ) 

        = BEGIN (* MteTeMs2 *) 
            SUBARRAY ( MteNewChars2 , MteTeMsStartSs , NUMBER ( Chars ) ) 
              := Chars 
          END MteTeMs2 

      ; BEGIN (* MteTeMoveSubstring *) 
          VAR LLen : LbeStd . LimitedCharNoTyp 
        ; VAR L1stSuffixLen : LbeStd . LimitedCharNoTyp 

        ; BEGIN (* Block *) 
            IF InsNlPos = LbeStd . LimitedCharNoInfinity 
            THEN (* It all goes into the first & only text mod. *) 
              Strings . InvokeWithArrayOfChar ( SourceString , MteTeMs1 ) 
            ELSE 
              IF ModTextStartSs < MteNewTextRelNlPos 
              THEN (* A portion goes into the first text mod. *) 
                L1stSuffixLen := MteNewTextRelNlPos - ModTextStartSs 
              ; LLen := Strings . Length ( SourceString ) 
              ; IF LLen <= L1stSuffixLen 
                THEN (* It all goes into the first text mod. *) 
                  Strings . InvokeWithArrayOfChar ( SourceString , MteTeMs1 ) 
                ELSE (* Split across the two text mods. *) 
                  Strings . InvokeWithArrayOfChar 
                    ( Strings . Substring ( SourceString , 0 , L1stSuffixLen ) 
                    , MteTeMs1 
                    ) 
                ; MteTeMsStartSs := 0 
                ; Strings . InvokeWithArrayOfChar 
                    ( Strings . Substring 
                        ( SourceString , L1stSuffixLen , LLen - L1stSuffixLen )
                    , MteTeMs2 
                    ) 
                END (* IF *) 
              ELSE (* Only the second text mod gets any. *) 
                MteTeMsStartSs := ModTextStartSs - MteNewTextRelNlPos 
              ; Strings . InvokeWithArrayOfChar ( SourceString , MteTeMs2 ) 
              END (* IF *) 
            END (* IF *) 
          END (* Block *) 
        END MteTeMoveSubstring 

    (* Utility procedures *) 

    ; PROCEDURE MteTeFlushCondFmt ( )  

      = BEGIN 
          IF MteTeCfInsTokFromPos # LbeStd . LimitedCharNoInfinity 
          THEN 
            IF MteTeCfInsTokFromPos = LbeStd . LimitedCharNoUnknown 
            THEN 
(* CHECK: This has been hastily thought through.  I think this whole
          mechanism is obviated by dummy est nodes, so it will probably
          just be deleted. *) 
              MteTeCfInsTokFromPos := MteCharPos  
            ; MteTeCfEstChildFromPos := MteCharPos 
            END (* IF *) 
          ; IF MteTeCfEstChildFromPos # LbeStd . LimitedCharNoUnknown  
            THEN
              IF MteTouchedFromPos > MteTeCfEstChildFromPos 
              THEN (* Est child has an untouched child on left. No need 
                      to expand. *) 
              ELSIF MteTouchedToPos # LbeStd . LimitedCharNoInfinity 
                    AND MteTouchedToPos < MteTeCfEstChildToPos 
              THEN (* Est child has an untouched child on right. No need 
                     to expand. *) 
              ELSIF TRUE THEN (* Temporarily disable this. *) 
              ELSE (* Expand the touched region to cover all the conditional 
                      insertion tokens. *) 
                MteTouchedFromPos 
                  := MIN ( MteTouchedFromPos , MteTeCfInsTokFromPos ) 
              ; MteTouchedToPos 
                  := MAX ( MteTouchedToPos , MteTeCfInsTokToPos ) 
              END (* IF *) 
            END (*IF *) 
          ; MteTeCfInsTokFromPos := LbeStd . LimitedCharNoInfinity 
          END (* IF *) 
        END MteTeFlushCondFmt 

    ; PROCEDURE MteTeReverse ( ) RAISES { Backout } 

      (* Make transition into MteStateBwdEdit. *) 

      = VAR LPrefixLen : LbeStd . LimitedCharNoTyp 
      ; VAR LSuffixLen : LbeStd . LimitedCharNoTyp 

      ; BEGIN (* MteTeReverse *) 
          MteTeFwdBlanks ( DelToPos , ExtendTouchedRightward := TRUE )  
          (* ^Edited region can lie beyond right end of existing line. *)
        ; IF MteTouchedToPos = LbeStd . LimitedCharNoInfinity 
          THEN MteTouchedToPos := MteCharPos 
          END (* IF *) 
        ; MteTeFlushCondFmt ( )  
        ; LPrefixLen := DelFromPos - MteTouchedFromPos 
        ; LSuffixLen := MteTouchedToPos - DelToPos 
        ; MteNewTextLen := LPrefixLen + InsLen + LSuffixLen 
        ; IF InsNlPos # LbeStd . LimitedCharNoInfinity 
          THEN MteNewTextRelNlPos := InsNlPos - MteTouchedFromPos 
          END (* IF *) 
        ; MteTeMoveSubstring 
            ( Strings . Substring ( InsText , DelFromPos , InsLen ) 
            , ModTextStartSs := LPrefixLen 
            ) 
        ; MteNewTextPos := MteNewTextLen 
        ; MteState := MteStateTyp . MteStateBwdEdit 
        ; MteTeBwdBlanks ( ) 
        END MteTeReverse 

    ; PROCEDURE MteTePassNl ( ) 

      = BEGIN
          MtePassedDelNl := TRUE 
        ; MteLineShift := MteCharPos 
        ; MteNlItemNo := MteItemCt 
        ; MteState := MteStateTyp . MteStatePassingNl 
        ; MteFwdPrevTok := LbeStd . Tok__BegOfLine 
        ; MteLastFmtNoOnLine := EstHs . FmtNoUnknown  
        ; MteEstListChildrenToPass := 0 (* Dead *) 
        ; MteModTextIsToLeftOnLine := FALSE 
(* CHECK: Should we do anything with MteFwdPrevItemCouldHaveCondNlAfter? 
          If so, should be done at call sites.  NlBefore is one possibility. *) 
        END MteTePassNl 

    ; PROCEDURE MteTeReverseOrPassNlBefore ( ) 
      : BOOLEAN 
      (* ^True if did reverse. *) 
      RAISES { Backout } 
      (* Used for ordinary Fs tree line breaks too. *) 
      (* This FUNCTION has numerous SIDE EFFECTS. *) 
      (* PRE: MteState is not a backward state. *)
      (* If reverses, will not revisit the current Est child. *) 
      (* If does not reverse, stays at the same Est child. *) 

      = BEGIN (* MteTeReverseOrPassNlBefore *) 
          IF MteState 
             IN MteStateSetTyp 
                  { MteStateTyp . MteStateStartAtBeg 
                  , MteStateTyp . MteStatePassingNl 
                  } 
          THEN (* Don't reverse.  Don't pass middle Nl, which, if needed,
                  will have happened earlier. *) 
            RETURN FALSE 
          ELSIF NOT MtePassedDelNl 
                AND DelNlShift # LbeStd . LimitedCharNoInfinity 
          THEN (* Pass this Nl. *) 
            MteTePassNl ( ) 
          (* Don't consume an Est child, callers don't want that. *) 
          ; RETURN FALSE 
          ELSE (* Reverse.  First wait Est children starting 
                  with the current one, and further to right. *) 
            Assert 
              ( MteItemCt > 0 
              , AFT . A_MteTeReverseOrPassNlBefore_At_beginning 
              ) 
          ; MteTeWaitChildrenToRight ( ) 
          ; MteTeReverse ( ) 
          ; MteTeDecEstChild ( )  
          ; RETURN TRUE 
          END (* IF *) 
        END MteTeReverseOrPassNlBefore 

    ; PROCEDURE MteTeReverseOrPassNlAfter ( ) 
      : BOOLEAN 
      (* ^True if did reverse. *) 
      RAISES { Backout } 
      (* This FUNCTION has numerous SIDE EFFECTS. *) 
      (* PRE: MteState is not a backward state. *) 
      (* We are at the new line after of a BlankLine, ModCmnt, or ModText. 
         See whether we should reverse at this new line, and take care of 
         either reversing or passing the new line. *) 

      = BEGIN (* MteTeReverseOrPassNlAfter *) 
          (* We are not in MteStateStartAtEnd *) 
          IF NOT MtePassedDelNl 
             AND DelNlShift # LbeStd . LimitedCharNoInfinity 
          THEN (* Pass this Nl *) 
            MteTePassNl ( ) 
          ; RETURN FALSE 
          ELSE (* Reverse.  First wait Est children to right. *) 
            MteTeWaitChildrenFartherToRight ( ) 
          ; MteTeReverse ( ) 
          (* No net change to EstChild, will revisit it in the new state. 
             But we have to go through Inc/Dec, just to get 
             MteTeRightChildRelNodeNo and MteTeRightChildRef set. 
          *) 
          ; MteTeIncEstChild ( ) 
          ; MteTeDecEstChild ( ) 
          ; RETURN TRUE 
          END (* IF *) 
        END MteTeReverseOrPassNlAfter 

    ; PROCEDURE MteTeFwdString 
        ( Tok : LbeStd . TokTyp 
        ; StringRef : SharedStrings . T 
        ; IsTrailingCmnt : BOOLEAN 
        ) 
      RAISES { Backout } 
      (* String of comment, text mod, or token. *) 

      = VAR LLength := SharedStrings . Length ( StringRef ) 

      ; BEGIN (* MteTeFwdString *) 
          TRY 
            IF LLength > 0 
            THEN 
              IF MteCharPos = DelToPos 
                 (* ^The string begins immediately right of changed region. *)  
                 AND Tok # LbeStd . Tok__ModText 
                     (* ^Touch ModText to merge it into others. *) 
                 AND NOT IsTrailingCmnt 
                 (* ^A sequence of trailing comments could be orphaned 
                     by having the token they are attached to unscanned. 
                     They would then have nothing to "trail".  This 
                     forces them to be touched and thus unscanned and 
                     later rescanned. *) 
                 AND NOT ( Tok IN LbeStd . StdTokSetCondNlBefore
                            AND MteNlOrTransparencyChanged  
                         ) 
                     (* ^Anything which, in some forms, (not just this 
                         particular item) can have a Nl before must be 
                         unscanned and rescanned if there is a Nl change 
                         because it could change to a different object. *) 
                 AND ( DelFromPos + InsLen = 0 
                       OR Strings . IthChar ( InsText , DelFromPos + InsLen - 1 )
                          = ' ' 
                       (* ^After editing, there is a BOL or blank immediately 
                          left of the string. *) 
                     ) 
              THEN 
                Assert 
                  ( MteTouchedToPos = LbeStd . LimitedCharNoInfinity 
                  , AFT . A_MteTeFwdStringTouchedToPosSetTwice 
                  ) 
              ; MteTouchedToPos := DelToPos 
              END (* IF *)
            ; IF MteTeCfInsTokFromPos = LbeStd . LimitedCharNoUnknown  
              THEN (* This is the LM string of a conditional format insertions,
                      left of its Est child. *) 
                MteTeCfInsTokFromPos := MteCharPos 
              END (* IF *)  
            ; IF LMCharPos = LbeStd . LimitedCharNoUnknown 
              THEN (* This is the LM String in the Est (sub)tree. *) 
                LMCharPos := MteCharPos 
              END (* IF *) 
            ; MteCharPos := EstUtil . WidthSumSigned ( MteCharPos , LLength ) 
            ; MteFwdLeftTokToPos := MteCharPos 
            ; IF (* FALSE AND *)    
  (* TODO: Figure out what was wrong with this case and either delete it,
           or strengthen it suitably and reinstate. *) 
                 MteCharPos = DelFromPos 
                 (* ^The string is immediately left of the changed region. *) 
                 AND Tok # LbeStd . Tok__ModText 
                     (* ^Touch ModText to merge it into others. *) 
                 AND NOT ( Tok IN LbeStd . StdTokSetCondNlAfter 
                           AND MteNlChanged OR MteNlOrTransparencyChanged  
                         ) 
                         (* ^Anything which, in some forms, (not just this 
                             particular item) can have a Nl after must be 
                             unscanned and rescanned if there is a Nl change 
                             because it could change to a different object. *) 
                 AND ( DelFromPos >= Strings . Length ( InsText ) 
                       OR Strings . IthChar ( InsText , DelFromPos ) = ' ' 
                     ) 
                     (* ^There is an EOL or blank immediately right of the 
                        string. *) 
              THEN 
                MteTouchedFromPos := DelFromPos 
              ; MteNewTextLeftTokToPos := DelFromPos 
              END (* IF *) 
            ; MteFwdPrevTok := Tok 
            END (* IF *) 
          EXCEPT Strings . SsOutOfBounds 
          => RAISE AssertionFailure ( "Strings.SsOutOfBounds" )  
          END (* TRY EXCEPT *) 
        END MteTeFwdString 

    ; PROCEDURE MteTeBwdEditTouchedString 
        ( StringRef : SharedStrings . T 
        ; BlanksLen : LbeStd . LimitedCharNoTyp := 0 
          (* If StringRef is NIL, instead use a string of blanks of 
             this length. (which can also be zero) *) 
        ) 
      RAISES { Backout } 
      (* Copy characters that are part of the touched but not the replaced 
         region into the new character arrays being filled for mod text(s). *) 

      = VAR LMoveLen : LbeStd . LimitedCharNoSignedTyp 
      ; VAR LStringLen : LbeStd . LimitedCharNoSignedTyp 
      ; VAR LToPos : LbeStd . LimitedCharNoSignedTyp 
      ; VAR LFragFromPos : LbeStd . LimitedCharNoSignedTyp 
      ; VAR LFragToPos : LbeStd . LimitedCharNoSignedTyp 
      ; VAR LStringFromPos : LbeStd . LimitedCharNoSignedTyp 
      ; VAR LString : Strings . T 

      ; BEGIN (* MteTeBwdEditTouchedString *) 
          Assert 
            ( MteState IN MteStateSetBwdOrDone  
            , AFT . A_MteTeBwdEditTouchedStringBadState 
            ) 
(* CHECK: Can come here in StateBwdNl between ModText w/ NlAfter (on the left)
          and ModText w/ Nl before, and TouchedFromPos = TouchedToPos
          = MteCharPos, from ModTextBwd, processing the left ModText.
           Should this be allowed to happen? *) 
        ; IF StringRef = NIL 
          THEN 
            LStringLen := BlanksLen 
          ; LString := Strings . AtLeastBlanks ( LStringLen ) 
          ELSE 
            LStringLen := SharedStrings . Length ( StringRef ) 
          ; LString := SharedStrings . ToString ( StringRef ) 
          END (* IF *) 
        ; LToPos := EstUtil . WidthSumSigned ( MteCharPos , LStringLen )   

        (* Handle contribution to suffix. *) 
        ; IF DelToPos <= MteCharPos 
          THEN
            LFragFromPos := MteCharPos 
          ; LStringFromPos := 0 
          ELSE 
            LFragFromPos := DelToPos 
          ; LStringFromPos := DelToPos - MteCharPos 
(* TODO: ^Something about the saturation arithmetic here. *) 
          END 
        ; LFragToPos := MIN ( LToPos , MteTouchedToPos ) 
        ; LMoveLen := LFragToPos - LFragFromPos 
        ; IF LMoveLen > 0 
          THEN 
            DEC ( MteNewTextPos , LMoveLen ) 
          ; MteTeMoveSubstring 
              ( Strings . Substring ( LString , LStringFromPos , LMoveLen ) 
              , ModTextStartSs := MteNewTextPos 
              ) 
          END 

        (* Handle contribution to prefix. *) 
        ; IF MteTouchedFromPos <= MteCharPos 
          THEN
            LFragFromPos := MteCharPos 
          ; LStringFromPos := 0 
          ELSE 
            LFragFromPos := MteTouchedFromPos 
          ; LStringFromPos := MteTouchedFromPos - MteCharPos 
(* TODO: ^Something about the saturation arithmetic here. *) 
          END 
        ; LFragToPos := MIN ( LToPos , DelFromPos ) 
        ; LMoveLen := LFragToPos - LFragFromPos 
        ; IF LMoveLen > 0 
          THEN 
            MteNewTextPos 
              := MIN ( MteNewTextPos , DelFromPos - MteTouchedFromPos ) 
            (* Skip inserted chars in new text, if not already done. *) 
          ; DEC ( MteNewTextPos , LMoveLen ) 
          ; MteTeMoveSubstring 
              ( Strings . Substring ( LString , LStringFromPos , LMoveLen ) 
              , ModTextStartSs := MteNewTextPos 
              ) 
          END 
        END MteTeBwdEditTouchedString 

    ; PROCEDURE MteTeFwdBlanks 
        ( BlanksToPos : LbeStd . LimitedCharNoTyp 
        ; ExtendTouchedRightward : BOOLEAN 
          (* ^If the right side of the touched region is still open,
             do not let the blanks close it. 
          *) 
        ; IsTrailingCmnt : BOOLEAN := FALSE 
        ) 

      = VAR LBlankCt : LbeStd . LimitedCharNoTyp 

      ; BEGIN (* MteTeFwdBlanks *) 
          IF BlanksToPos > MteCharPos 
          THEN (* At least one blank. *) 

          (* Check beginning/end of touched region. *) 
            IF MteFwdPrevTok = LbeStd . Tok__ModText 
               (* The previous ModText will be touched, if nothing else 
                  untouched intervenes. This merges into one ModText. *)
               OR MteFwdPrevTok = LbeStd . Tok__BegOfLine 
                  (* Bol will be touched, if nothing nonblank and untouched 
                     intervenes. This makes the new ModText denote that it
                     is first on the line. *)
               OR IsTrailingCmnt 
                  (* Touched region cannot begin with a trailing comment,
                     because the region will be converted to leading modifiers,
                     and a trailing mod is at the wrong place to do this. *) 
            THEN (* Don't update MteTouchedFromPos. *)  
            ELSIF BlanksToPos < DelFromPos 
            THEN (* If it doesn't get changed later, this will be the 
                    beginning of the touched region. *) 
              MteTouchedFromPos := BlanksToPos 
            ; MteNewTextLeftTokToPos := MteFwdLeftTokToPos 
            ELSIF MteCharPos < DelFromPos 
                  (* At least one blank preceeds the deleted region. 
                     These blanks are the transition to/into the 
                     deleted region and are at the left end of 
                     the touched region. *) 
                  AND NOT ( MteFwdPrevItemCouldHaveCondNlAfter 
                            AND MteNlOrTransparencyChanged  
                          ) 
                          (* The item to the left is a kind which can
                             have an optional Nl after and there is a
                             change in the presence/absence of a Nl
                             within the changed region that could be
                             visible to the last item.  In this case,
                             do not update MteTouchedFromPos.  Thus
                             the last item is touched, unscanned, and
                             will be rescanned eventually, at which
                             time its NlAfter property will be
                             recomputed. *)
            THEN 
              MteTouchedFromPos := DelFromPos 
            ; MteNewTextLeftTokToPos := MteFwdLeftTokToPos 
            END (* IF *) 
          ; IF NOT ExtendTouchedRightward 
               AND MteTouchedToPos = LbeStd . LimitedCharNoInfinity 
                   (* ^We have not done this before. *) 
               AND BlanksToPos > DelToPos 
                   (* Rightmost blank is after the deleted region. *) 
            THEN (* These blanks end the touched region. *) 
              MteTouchedToPos := MAX ( MteCharPos , DelToPos ) 
            END (* IF *) 
          ; LBlankCt := BlanksToPos - MteCharPos 
(* TODO: ^Something about the saturation arithmetic here. *) 
          ; MteCharPos := BlanksToPos 
          ; MteFwdPrevTok := LbeStd . Tok__Sep 
          ELSE 
            LBlankCt := 0 
          END (* IF *) 

        (* Store the inserted blank count for backward processing later. *) 
        ; MteBlanks [ MteBlankSs ] := LBlankCt 
        ; INC ( MteBlankSs ) 
        END MteTeFwdBlanks 

    ; PROCEDURE MteTeBwdBlanks ( ) RAISES { Backout } 
      (* Harmless, if we are already out of MteStateBwdEdit. *) 

      = VAR LBlanksLen : LbeStd . LimitedCharNoSignedTyp 

      ; BEGIN (* MteTeBwdBlanks *) 
          DEC ( MteBlankSs ) 
        ; LBlanksLen := MteBlanks [ MteBlankSs ] 
        ; DEC ( MteCharPos , LBlanksLen ) 
(* TODO: ^Something about the saturation arithmetic here. *) 
        ; MteTeBwdEditTouchedString ( NIL , LBlanksLen ) 
        END MteTeBwdBlanks 

    ; PROCEDURE MteTeSetIndentInfo ( )  

      = BEGIN 
          IF MteTeEstTravInfo . EtiParentRef = NIL
          THEN MteTeIsFirstLine := TRUE
          ELSE
            MteTeIsFirstLine 
              := MteTeEstTravInfo . EtiChildNo 
                 < MteTeEstTravInfo . EtiParentRef . KTreeEstChildCtLeftOfNl
          END (* IF *) 
        ; IF MteTeIsFirstLine 
          THEN
            MteTeIndentPos := EstIndentPos1 
          ELSE
            MteTeIndentPos := EstIndentPosN 
          END (* IF *) 
        END MteTeSetIndentInfo 

    ; PROCEDURE MteTeTok 
        ( Tok : LbeStd . TokTyp 
        ; StringRef : SharedStrings . T 
        ; FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : LangUtil . FmtKindTyp 
        ; VAR (* OUT *) DoExcludeIfBwd : BOOLEAN 
          (* Tells caller to exclude this item from the new Est, 
             if in a backwards state. *) 
        ) 
      RAISES { Backout } 
      (* PRE: MteState # MteStateTyp . MteStateRightNlFound. *) 

      = BEGIN (* MteTeTok *) 
          DoExcludeIfBwd := FALSE 
        ; CASE MteState 
          OF MteStateTyp . MteStatePassingNl 
          , MteStateTyp . MteStateFwd 
          => MteState := MteStateTyp . MteStateFwd 
          ; MteTeFwdBlanks
              ( TravUtil . PosForTok 
                  ( Lang 
                  , FmtKind 
                  , MteModTextIsToLeftOnLine 
                  , MteCharPos - MteLineShift 
(* TODO: ^Something about the saturation arithmetic here. *) 
                  , MteTeIndentPos 
                  , FsNodeRef . FsIndentCode 
                  , MteFwdPrevTok 
                  , Tok 
                  ) 
                + MteLineShift 
              , ExtendTouchedRightward := FALSE 
              ) 
          ; MteTeFwdString ( Tok , StringRef , IsTrailingCmnt := FALSE ) 
          ; INC ( MteItemCt ) 

          | MteStateTyp . MteStateBwdEdit 
          => DEC ( MteCharPos , SharedStrings . Length ( StringRef ) ) 
(* TODO: ^Something about the saturation arithmetic here. *) 
          ; IF MteCharPos >= MteTouchedToPos 
            THEN (* Entire string is to right of touched region. *) 
              MteNewTextToPos 
                := EstUtil . WidthSum ( MteCharPos , MteSuffixShift ) 
            ELSIF MteCharPos >= MteTouchedFromPos 
            THEN (* Entire string is inside touched region. *) 
              MteTeBwdEditTouchedString ( StringRef ) 
            ; DoExcludeIfBwd := TRUE 
            ELSE 
              CantHappen ( AFT . A_MteTeTokBwdEditLeftOfTouchedRegion ) 
            END (* IF *) 
          ; MteTeBwdBlanks ( ) 
          ; DEC ( MteItemCt )
          ; IF MteItemCt <= 0
            THEN MteState := MteStateTyp . MteStateDone 
            END (* IF *) 

          | MteStateTyp . MteStateBwdNl 
          , MteStateTyp . MteStateDone  
          => DEC ( MteCharPos , SharedStrings . Length ( StringRef ) ) 
(* TODO: ^Something about the saturation arithmetic here. *) 
          ; MteTeBwdBlanks ( ) 
          ; DEC ( MteItemCt ) 
          ; IF MteItemCt <= 0
            THEN MteState := MteStateTyp . MteStateDone 
            END (* IF *) 

          ELSE 
            CantHappen ( AFT . A_MteTeTokBadMteState ) 
          END (* CASE MteState *) 
        END MteTeTok 

    ; PROCEDURE MteTeAstString 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : LangUtil . FmtKindTyp 
        ) 
      RAISES { Backout } 

      = VAR LDoExcludeIfBwd : BOOLEAN 

      ; BEGIN (* MteTeAstString *) 
          CASE MteState 
          OF MteStateTyp . MteStateFwd 
          , MteStateTyp . MteStatePassingNl
          => MteTeTok 
               ( SharedStrings . Tok ( MteTeEstTravInfo . EtiStringRef ) 
               , MteTeEstTravInfo . EtiStringRef 
               , FsNodeRef 
               , FmtKind 
               , (* VAR *) DoExcludeIfBwd := LDoExcludeIfBwd 
               ) 
          ; MteFwdPrevItemCouldHaveCondNlAfter := FALSE 
          ; MteState := MteStateTyp . MteStateFwd 

          | MteStateTyp . MteStateRightNlFound  
          => MteTeReverse ( ) 
          (* Defer waiting children and MteTeMaybeFinishBwdEdit to next Est 
             level upward.  There are no EstChildren, so setting  
             MteTeRightChildRelNodeNo and MteRightChildRef is unnecessary. 
          *) 

          | MteStateTyp . MteStateBwdEdit 
          , MteStateTyp . MteStateBwdNl 
          , MteStateTyp . MteStateDone  
          => MteTeTok 
               ( SharedStrings . Tok ( MteTeEstTravInfo . EtiStringRef ) 
               , MteTeEstTravInfo . EtiStringRef 
               , FsNodeRef 
               , FmtKind 
               , (* VAR *) DoExcludeIfBwd := LDoExcludeIfBwd 
               ) 
          ; IF LDoExcludeIfBwd 
             THEN MteTeNewEstRef := NIL 
             ELSE MteTeNewEstRef := EstRef 
             END (* IF *) 
          (* Defer MteTeMaybeFinishBwdEdit to next Est level upward. *) 
          ; IF MteCharPos < MteTouchedToPos 
            THEN 
              MaxTouchedNodeNo := MAX ( MaxTouchedNodeNo , EstAbsNodeNo ) 
            END 

          ELSE 
            CantHappen ( AFT . A_MteTeAstStringBadMteState ) 
          END (* CASE MteState *) 
        END MteTeAstString 

    ; PROCEDURE MteTeTraverseFsFixedChildren 
        ( ParentFsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : LangUtil . FmtKindTyp 
        ; FsChildCt : LangUtil . FsChildNoTyp 
        ; InitialFsChildNo : LangUtil . FsChildNoSignedTyp 
        ) 
      RAISES { Backout , Thread . Alerted } 

      = VAR LFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* MteTeTraverseFsFixedChildren *) 
          IF FsChildCt > 0 
          THEN 
            LFsChildNo := InitialFsChildNo 
          ; LOOP 
              MteTeTraverseFs 
                ( ParentFsNodeRef . FsChildren ^ [ LFsChildNo ] , FmtKind ) 
            ; CASE MteState 
              OF MteStateTyp . MteStateStartAtBeg 
              , MteStateTyp . MteStateStartAtEnd 
              , MteStateTyp . MteStatePassingNl 
              , MteStateTyp . MteStateFwd 
              , MteStateTyp . MteStateRightNlFound  
              => INC ( LFsChildNo ) 
              ; IF LFsChildNo >= FsChildCt 
                THEN 
                  EXIT 
                END (* IF *) 
              | MteStateTyp . MteStateBwdEdit 
              , MteStateTyp . MteStateBwdNl 
              , MteStateTyp . MteStateDone 
              => IF MteItemCt <= 0 OR LFsChildNo <= 0 
                 THEN 
                   EXIT 
                 ELSE 
                   DEC ( LFsChildNo ) 
                 END (* IF *) 

              END (* CASE *) 
            END (* LOOP *) 
          END (* IF *) 
        END MteTeTraverseFsFixedChildren 

    ; PROCEDURE MteTeTraverseFsListChildren 
        ( ParentFsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : LangUtil . FmtKindTyp 
        ; FsChildCt : LangUtil . FsChildNoTyp 
        ; InitialFsChildNo : LangUtil . FsChildNoTyp 
        ) 
      RAISES { Backout , Thread . Alerted } 

      = VAR LFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* MteTeTraverseFsListChildren *) 
          LFsChildNo := InitialFsChildNo 
        ; LOOP 
            MteTeTraverseFs 
              ( ParentFsNodeRef . FsChildren ^ [ LFsChildNo ] , FmtKind ) 
          ; CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 
            , MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStateRightNlFound  
            => IF LFsChildNo = MteTeRMFsChildNo 
                  AND MteTeEstTravInfo . EtiChildNo 
                      >= MteTeEstTravInfo . EtiChildCt 
               THEN 
                 EXIT 
               ELSE 
                 LFsChildNo := ( LFsChildNo + 1 ) MOD FsChildCt 
               END (* IF *) 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone 
            => IF MteItemCt <= 0 
                  OR ( LFsChildNo = 0 AND MteTeEstTravInfo . EtiChildNo < 0 )
               THEN 
                 EXIT 
               ELSE 
                 LFsChildNo := ( LFsChildNo - 1 ) MOD FsChildCt 
               END (* IF *)

            END (* CASE *) 
          END (* LOOP *) 
        END MteTeTraverseFsListChildren 

    ; PROCEDURE MteTeTraverseFs 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FsFmtKind : LangUtil . FmtKindTyp 
        ) 
      RAISES { Backout , Thread . Alerted } 

      (* Leading mods. *) 

      = PROCEDURE MteTeTfsModBlankLine1stBwd 
          ( <* UNUSED *> ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { Backout } 
        (* Backward processing inside a blank line that does not follow
           a deleted new line.  MteItemCt does not include the Nl after. 
        *) 

        = BEGIN (* MteTeTfsModBlankLine1stBwd *) 
            IF MteItemCt <= MteFinishItemNo  
            THEN (* Backward traverse ends at NlAfter of this ModBlankLine. *)
              MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeRightChildRelNodeNo
                ) 
            ; IF MteState # MteStateTyp . MteStateDone  
              THEN (* Mark the NlAfter of this ModBlankLine. *) 
                NewBolTokMark 
                  := Marks . TokMarkTyp 
                       { EstNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                       , EstNodeCt := 1   
                       , TkmEstRef
                           := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       , Kind := MarkKindTyp . BlankLine 
                       , FmtNo := FsNodeRef . FsFmtNo 
                       , StartAtEnd := TRUE 
                       , IsImpliedNewLine := FALSE 
                       , TmTok := LbeStd . Tok__BlankLine 
                       } 
              ; INC ( MteMarkCt ) (* For the Nl at the end. *) 
              ; MteState := MteStateTyp . MteStateDone 
              END 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , EstAbsNodeNo 
                         + MteTeEstTravInfo . EtiChildRelNodeNo 
                       ) 
            ; IF MteItemCt > 0 THEN MteTeDecEstChild ( ) END (* IF *) 
            ELSE (* Traverse backward into the blank line. *) 
              MteTeBwdBlanks ( ) (* Inside the deleted region. *)  
            ; MteTeBwdBlanks ( ) (* Left of the deleted region. *) 
            ; DEC ( MteItemCt ) 
            ; IF MteItemCt <= MteFinishItemNo 
              THEN (* Finish could happen to the right of the NlBefore. *) 
                MteTeMaybeFinishBwdEdit 
                  ( FsNodeRef . FsFmtNo 
                  , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo
                  ) 
              ; IF MteState # MteStateTyp . MteStateDone  
                THEN (* Mark the NlBefore of this ModBlankLine. *) 
                  NewBolTokMark 
                  := Marks . TokMarkTyp 
                       { EstNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                       , EstNodeCt := 1   
                       , TkmEstRef
                           := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef
                       , Kind := MarkKindTyp . BlankLine 
                       , FmtNo := FsNodeRef . FsFmtNo 
                       , StartAtEnd := FALSE  
                       , IsImpliedNewLine := FALSE 
                       , TmTok := LbeStd . Tok__BlankLine 
                       } 
                ; INC ( MteMarkCt ) (* For the Nl at the end. *) 
                ; MteState := MteStateTyp . MteStateDone 
                END 
              ELSE 
                CantHappen ( AFT . A_MteTeTfsModBlankLine1stBwd_NotDone ) 
              END 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , EstAbsNodeNo 
                         + MteTeEstTravInfo . EtiChildRelNodeNo 
                       ) 
            ; MteTeDecEstChild ( ) 
            END (* IF *) 
          END MteTeTfsModBlankLine1stBwd 

      ; PROCEDURE MteTeTfsModBlankLine1stFwd 
          ( ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { Backout } 
        (* Forward processing of a blank line that we are starting somewhere
           inside of. *) 

        = BEGIN (* MteTeTfsModBlankLine1stFwd *) 
            Assert 
               ( 0 <= BlankLineNo 
                 AND BlankLineNo < ModBlankLine . ModBlankLineCt 
               , AFT . A_MteTeTfsModBlankLine1stFwdLineNoRange 
               ) 
          ; Assert
              ( MteFwdLeftTokToPos = 0 
              , AFT . A_MteTeTfsModBlankLine1stFwdNotBol 
              ) 
          ; MteBlankLinesBefore := BlankLineNo 
          ; MteBlankLinesAfter 
              := ModBlankLine . ModBlankLineCt - 1 - BlankLineNo 
          ; MteStartingModBlankLineRef 
              := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
          ; INC ( MteItemCt ) (* Count the Nl before. *) 
          ; MteTeFwdBlanks (* Left of the deleted region. *) 
              ( DelFromPos , ExtendTouchedRightward := TRUE ) 
          ; IF DelNlShift = LbeStd . LimitedCharNoInfinity 
            THEN 
              MteTeFwdBlanks (* Inside the deleted region. *)  
                ( DelToPos , ExtendTouchedRightward := TRUE ) 
            ELSE
              MteTeFwdBlanks (* Inside the deleted region. *)  
                ( MIN ( DelNlShift , DelToPos ) 
                , ExtendTouchedRightward := TRUE 
                ) 
            END 
          ; IF MteTeReverseOrPassNlAfter ( ) (* Has SIDE EFFECTS. *)  
            THEN (* We reversed. *)
              MteTeInterestingChildrenExist 
                := MteTeInterestingChildrenExist 
                   OR FsNodeRef . FsDeletableItemsAreToRight 
                   OR FsNodeRef . FsKind = FsKindTyp. FsKindInsTok 
            ; MteTeMaybeFinishBwdEdit 
                ( FmtNo := FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                  (* ^We are inside the mod and have taken it apart. *) 
                ) 
            ; MteTeTfsModBlankLine1stBwd ( ModBlankLine )  
            ELSE (* Pass the Nl after. *)  
              IF MteBlankLinesAfter = 0 
              THEN (* Continue forward traversal. *) 
                INC ( MteItemCt ) (* Count the Nl after. *) 
              ; MteTeIncEstChild ( ) 
              ELSE (* Reduce following blank lines and reverse. *) 
                MteTeFwdBlanks ( DelToPos , ExtendTouchedRightward := TRUE ) 
              ; DEC ( MteBlankLinesAfter ) 
              ; MteTeWaitChildrenFartherToRight ( ) 
              ; MteTeReverse ( ) 
              ; MteTeInterestingChildrenExist 
                  := MteTeInterestingChildrenExist 
                     OR FsNodeRef . FsDeletableItemsAreToRight  
                     OR FsNodeRef . FsKind = FsKindTyp. FsKindInsTok 
              (* No net change to EstChild, will revisit it in the new state. 
                 But we have to go through Inc/Dec, just to get 
                MteTeRightChildRelNodeNo and MteTeRightChildRef set. 
              *) 
              ; MteTeIncEstChild ( ) 
              ; MteTeDecEstChild ( ) 
              ; MteTeMaybeFinishBwdEdit 
                  ( FmtNo := FsNodeRef . FsFmtNo 
                  , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                    (* ^We are inside the mod and have taken it apart. *) 
                  ) 
              ; MteTeBwdBlanks ( ) 
              ; IF MteItemCt = MteNlItemNo THEN MteLineShift := 0 END  
              ; MteTeTfsModBlankLine1stBwd ( ModBlankLine )  
              END 
            END (* IF *) 
          END MteTeTfsModBlankLine1stFwd 

      ; PROCEDURE MteTeTfsModBlankLine2nd 
          ( ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { Backout } 
        (* A ModBlankLine that follows a deleted new line.  This implies
           it is adjacent to the touched region. *) 

        = BEGIN (* MteTeTfsModBlankLine2nd *) 
            MteTeFwdBlanks ( DelToPos , ExtendTouchedRightward := TRUE ) 
          ; MteTeWaitChildrenFartherToRight ( ) 
          ; INC ( MteBlankLinesAfter , ModBlankLine . ModBlankLineCt - 1 ) 
          ; MteTeReverse ( ) 
          ; MteTeInterestingChildrenExist 
              := MteTeInterestingChildrenExist 
                 OR FsNodeRef . FsDeletableItemsAreToRight  
                 OR FsNodeRef . FsKind = FsKindTyp. FsKindInsTok 
          ; MteTeMaybeFinishBwdEdit 
              ( FmtNo := FsNodeRef . FsFmtNo 
              , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                (* ^We are inside the mod and have taken it apart. *) 
              ) 
          ; MteTeBwdBlanks ( ) 
          ; MaxTouchedNodeNo 
              := MAX ( MaxTouchedNodeNo 
                     , EstAbsNodeNo 
                       + MteTeEstTravInfo . EtiChildRelNodeNo 
                     ) 
          ; MteTeDecEstChild ( )  
          END MteTeTfsModBlankLine2nd 

      ; PROCEDURE MteTeTfsModBlankLine 
          ( ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { Backout } 

        = BEGIN (* MteTeTfsModBlankLine *) 
            CASE MteState 
            OF MteStateTyp . MteStateStartAtEnd 
            => MteFinishItemNo := 1 
            ; MteTeSetIndentInfo ( )  
            ; MteState := MteStateTyp . MteStatePassingNl 
            ; INC ( MteItemCt ) 
            ; MteTeIncEstChild ( ) 

            | MteStateTyp . MteStateStartAtBeg 
            => MteFinishItemNo := 0 
            ; MteTeSetIndentInfo ( )  
            ; MteState := MteStateTyp . MteStateFwd 
            ; MteTeTfsModBlankLine1stFwd ( ModBlankLine ) 

            | MteStateTyp . MteStatePassingNl 
            => IF MtePassedDelNl 
               THEN
                 MteTeTfsModBlankLine2nd ( ModBlankLine ) 
               ELSE
                 MteTeTfsModBlankLine1stFwd ( ModBlankLine ) 
               END 

            | MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStateRightNlFound  
            => IF NOT MtePassedDelNl 
                  AND DelNlShift # LbeStd . LimitedCharNoInfinity 
               THEN (* Pass the Nl *)  
                 MteTePassNl ( ) 
               ; MteTeTfsModBlankLine2nd ( ModBlankLine )  
               ELSE (* Reverse at the beginning of the blank line mod. *) 
                 IF MteTouchedToPos = LbeStd . LimitedCharNoInfinity 
                 THEN (* Touched region reaches here.  Deconstruct the blank 
                         lines and put them into the merge. *) 
                   MteTeWaitChildrenFartherToRight ( ) 
                 ; INC ( MteBlankLinesAfter , ModBlankLine . ModBlankLineCt ) 
                 ; TrailingBlankLinesIncluded := ModBlankLine . ModBlankLineCt 
                 ELSE 
                   MteTeWaitChildrenToRight ( ) 
                 END 
               ; MteTeReverse ( ) 
               ; MteTeInterestingChildrenExist 
                   := MteTeInterestingChildrenExist 
                      OR FsNodeRef . FsDeletableItemsAreToRight  
                      OR FsNodeRef . FsKind = FsKindTyp. FsKindInsTok 
               ; MteTeMaybeFinishBwdEdit 
                   ( FmtNo := FsNodeRef . FsFmtNo 
                   , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                   ) 
               ; MteTeDecEstChild ( )  
               END (* IF *) 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => DEC ( MteItemCt ) (* For the Nl after. *) 
            ; IF MteItemCt = MteNlItemNo THEN MteLineShift := 0 END  
            ; MteTeTfsModBlankLine1stBwd ( ModBlankLine )  

            END (* CASE MteState *) 
          END MteTeTfsModBlankLine 

      ; PROCEDURE MteTeTfsModCmntBwd ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { Backout } 

        (* Does not handle Nl after, which is handled differently
           at different call sites.  MteItemCt does not include 
           any Nl after. *) 

        = VAR LNewModCmntRef : ModHs . ModCmntTyp 
        ; VAR LTok : LbeStd . TokTyp 
        ; VAR LEdgeKind : EstHs . EdgeKindTyp 
        ; VAR LMustAdjustFromPos : BOOLEAN 

        ; BEGIN (* MteTeTfsModCmntBwd *) 

          (* Text of comment. *) 
            DEC 
              ( MteCharPos 
              , SharedStrings . Length ( ModCmnt . ModCmntStringRef ) 
              )
(* TODO: ^Something about the saturation arithmetic here. *) 
          ; Assert
              ( SharedStrings . Length ( ModCmnt . ModCmntStringRef ) > 0 
              , AFT . A_MteTeTfsModCmntBwd_ZeroLength  
              ) 
          ; IF MteCharPos >= MteTouchedToPos 
            THEN (* Entire string is to right of touched region. *) 
              TYPECASE ModCmnt <* NOWARN *> 
              OF ModHs . ModCmntLeadingFixedTyp 
              => LMustAdjustFromPos := MteModShift # 0 
              ; LEdgeKind := EstHs . EdgeKindTyp . EdgeKindLeadingMod  
              | ModHs . ModCmntLeadingRelativeTyp 
              => LMustAdjustFromPos := FALSE 
              ; LEdgeKind := EstHs . EdgeKindTyp . EdgeKindLeadingMod  
              | ModHs . ModCmntTrailingFixedTyp 
              => LMustAdjustFromPos := MteModShift # 0 
              ; LEdgeKind := EstHs . EdgeKindTyp . EdgeKindTrailingMod  
              | ModHs . ModCmntTrailingRelativeTyp 
              => LMustAdjustFromPos := FALSE 
              ; LEdgeKind := EstHs . EdgeKindTyp . EdgeKindTrailingMod  
              END (* TYPECASE *)  
            ; IF LMustAdjustFromPos (* OR ModCmnt . ModCmntNlAfter *) 
              THEN 
                LNewModCmntRef := ModHs . CopyOfModCmnt ( ModCmnt ) 
           (* ; LNewModCmntRef . ModCmntNlAfter := FALSE *) 
(* CHECK: ^Why did I once do this? We are beyond the touched region, so any
           NlAfter will be the end of the line/lines we are working in,
           and should stay as is. 
*) 
              ; IF LMustAdjustFromPos 
                THEN 
                  INC ( LNewModCmntRef . ModCmntFromPos , MteModShift ) 
                END (* IF *) 
              ; MteTeWaitChild 
                  ( LNewModCmntRef 
                  , MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                  , FsNodeRef . FsFmtNo 
                  , LEdgeKind
                  ) 
              ELSE 
                MteTeWaitExistingEstChild 
                  ( LEdgeKind 
                  , IsInteresting := TRUE 
                  )  
              END (* IF *) 
            ; MteNewTextToPos 
                := EstUtil . WidthSum ( MteCharPos , MteSuffixShift ) 

            ELSIF MteCharPos >= MteTouchedFromPos 
            THEN (* Entire string is inside touched region. *) 
              MteTeBwdEditTouchedString ( ModCmnt . ModCmntStringRef ) 
            ; MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                ) 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , EstAbsNodeNo 
                         + MteTeEstTravInfo . EtiChildRelNodeNo 
                       ) 
            ELSE (* Entire string is to left of touched region. *)  
              TYPECASE ModCmnt <* NOWARN *> 
              OF ModHs . ModCmntLeadingFixedTyp 
              , ModHs . ModCmntLeadingRelativeTyp 
              => LEdgeKind := EstHs . EdgeKindTyp . EdgeKindLeadingMod  
              | ModHs . ModCmntTrailingFixedTyp 
              , ModHs . ModCmntTrailingRelativeTyp 
              => LEdgeKind := EstHs . EdgeKindTyp . EdgeKindTrailingMod  
              END (* TYPECASE *)  
            ; IF ModCmnt . ModCmntNlAfter 
              THEN
                LNewModCmntRef := ModHs . CopyOfModCmnt ( ModCmnt ) 
              ; LNewModCmntRef . ModCmntNlAfter := FALSE 
              ; MteTeWaitChild 
                  ( LNewModCmntRef 
                  , MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                  , FsNodeRef . FsFmtNo 
                  , LEdgeKind 
                  ) 
              ELSE 
                MteTeWaitExistingEstChild 
                  ( LEdgeKind , IsInteresting := TRUE ) 
              END (* IF *) 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , EstAbsNodeNo 
                         + MteTeEstTravInfo . EtiChildRelNodeNo 
                       ) 
            END (* IF *) 

          (* Leading blanks of comment. *) 
          ; MteTeBwdBlanks ( ) 
          ; MteTeMaybeFinishBwdEdit 
              ( FsNodeRef . FsFmtNo 
              , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
              ) 
          ; DEC ( MteItemCt )
          ; IF MteItemCt <= 0 
            THEN (* Backward traverse ends at beginning of comment. *) 
              MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                ) 
            ; IF MteState # MteStateTyp . MteStateDone  
              THEN (* Mark the beginning of comment. *)
                (* It is possible that the comment is trailing and fixed, 
                   and NOT ModCmntNlBefore, but have a fixed start position.
                   In this case, there could still be a Nl here, not denoted
                   by ModCmnt, but implied by the character position to its
                   left.  Mark even it, just in case. *) 
                IF ModCmnt . ModCmntNlAfter 
                THEN LTok := LbeStd . Tok__CmntAtEndOfLine 
                ELSE LTok := LbeStd . Tok__Cmnt 
                END (* IF *) 
              ; NewBolTokMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                     , EstNodeCt := 1   
                     , TkmEstRef
                         := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef
                     , Kind := MarkKindTyp . Plain 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := FALSE
                     , IsImpliedNewLine := NOT ModCmnt . ModCmntNlBefore 
                     , TmTok := LTok  
                     } 
              ; INC ( MteMarkCt ) 
              ; MteState := MteStateTyp . MteStateDone 
              END 
            ELSIF ModCmnt . ModCmntNlBefore AND MteItemCt = MteNlItemNo 
            THEN MteLineShift := 0 
            END (* IF*) 
          ; MteTeDecEstChild ( ) 
          END MteTeTfsModCmntBwd 

      ; PROCEDURE MteTeTfsModCmntFwd ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { Backout } 

        = VAR LFromPos : LbeStd . LimitedCharNoSignedTyp 
        ; VAR LIsTrailingCmnt : BOOLEAN 
        ; VAR LIsNonLeftmostLeadingCmnt : BOOLEAN 

        ; BEGIN (* MteTeTfsModCmntFwd *) 

          (* Handle leading blanks of comment. *) 
            TYPECASE ModCmnt 
            OF ModHs . ModCmntLeadingFixedTyp 
            => LFromPos 
                 := EstUtil . WidthSumSigned  
                      ( ModCmnt . ModCmntFromPos , MteLineShift ) 
            ; LIsTrailingCmnt := FALSE 
            ; LIsNonLeftmostLeadingCmnt := FALSE 
            | ModHs . ModCmntTrailingFixedTyp 
            => LFromPos 
                 := EstUtil . WidthSumSigned  
                      ( ModCmnt . ModCmntFromPos , MteLineShift ) 
            ; LIsTrailingCmnt := TRUE  
            ; LIsNonLeftmostLeadingCmnt := FALSE 
            | ModHs . ModCmntLeadingRelativeTyp 
            => LFromPos 
                 := EstUtil . WidthSumSigned3 
                      ( TravUtil . IndentPos 
                          ( Lang 
                          , MteTeIndentPos 
                          , FsNodeRef . FsIndentCode 
                          ) 
                      , ModCmnt . ModCmntFromPos 
                      , MteLineShift 
                      )            
(* FIX: Investigate how we got a comment with ModCmntFromPos = -2, in a context
        where IndentPos was 1. 
*) 
            ; LIsTrailingCmnt := FALSE 
            ; LIsNonLeftmostLeadingCmnt := NOT ModCmnt . ModCmntNlBefore 
            | ModHs . ModCmntTrailingRelativeTyp 
            => LFromPos 
                 := EstUtil . WidthSumSigned  
                      ( MteCharPos , ModCmnt . ModCmntFromPos )  
               (* ^SameLine comment implies there is a token 
                   preceeding, so no need to consider IndentPos. *) 
            ; LIsTrailingCmnt := TRUE  
            ; LIsNonLeftmostLeadingCmnt := FALSE 
            ELSE 
              CantHappen ( AFT . A_MteTeTfsModCmnt_AbstractCommentType ) 
            END (* TYPECASE *) 
          ; LFromPos := MAX ( MteCharPos , LFromPos ) 
          ; MteTeFwdBlanks 
              ( LFromPos 
              , ExtendTouchedRightward 
                  := LIsTrailingCmnt 
                     (* ^A sequence of trailing comments could be orphaned 
                         by having the token they are attached to unscanned. 
                         They would then have nothing to "trail".  This 
                         forces them to be touched, unscanned, and 
                         rescanned. *) 
                     OR MteNlChanged  
                        (* ^The Nl before property of this comment could
                            change, so touch it. 
                        *) 
                     OR ModCmnt . ModCmntNlBefore 
                        AND DelFromPos < ModCmnt . ModCmntFromPos 
                        (* ^Another way the NlBefore property could change. *) 
                     OR LIsNonLeftmostLeadingCmnt AND MteSuffixShift # 0 
                        (* ^ Once, I tried avoiding this touching of a
                           leading, relative, comment when preceeded
                           by changes in the number of characters to
                           the left, by making this kind of comment be
                           relative to the preceeding item, instead of
                           relative to the indent position.  But,
                           alas, the relativity is different for a
                           preceding comment (intervening blanks have
                           to be counted in the relative position) and
                           a preceding ModText (intervening blanks
                           are already provided for by the ModText).
                           Since the one can change to the other, this
                           would have entailed even more complexity and
                           probably about as much computation, so I just 
                           abandoned the idea.
                        *)
              , IsTrailingCmnt := LIsTrailingCmnt 
              ) 

          (* Continue with text of comment. *) 
          ; MteTeFwdString 
              ( LbeStd . Tok__Cmnt 
              , ModCmnt . ModCmntStringRef 
              , IsTrailingCmnt := LIsTrailingCmnt 
              ) 
          ; IF ModCmnt . ModCmntNlAfter 
            THEN 
              IF LIsTrailingCmnt 
              THEN 
                IF NOT MtePassedDelNl 
                   AND DelNlShift # LbeStd . LimitedCharNoInfinity 
                THEN (* Pass this Nl *) 
                  MteTePassNl ( )
                ELSE (* This is the end of the last line we want to traverse,
                        but it is a trailing comment, and we need to traverse 
                        forward to the next item, in order to have the right
                        mark values for any new leading mods that are to be 
                        built. Pass it, but go into state MteStateRightNlFound.
                        Either another Nl will cause a reverse, or something 
                        space-occupying on the next line will cause it. 
                     *) 
                  MteFwdPrevTok := LbeStd . Tok__CmntAtEndOfLine  
                ; MteState := MteStateTyp . MteStateRightNlFound 
                END (* IF *)  
              ; INC ( MteItemCt ) (* Count the Nl after. *) 
              ; MteFwdPrevItemCouldHaveCondNlAfter := TRUE 
              ; MteTeIncEstChild ( ) 
              ELSE 
                IF MteTeReverseOrPassNlAfter ( ) (* Has SIDE EFFECTS *) 
                THEN (* We reversed. *) 
                  MteTeInterestingChildrenExist 
                    := MteTeInterestingChildrenExist 
                       OR FsNodeRef . FsDeletableItemsAreToRight  
                       OR ( FsNodeRef . FsKind = FsKindTyp. FsKindInsTok 
                            AND NOT LIsTrailingCmnt 
                          ) 
                ; MteTeMaybeFinishBwdEdit 
                    ( FmtNo := FsNodeRef . FsFmtNo 
                    , MarkNodeNo := MteTeRightChildRelNodeNo 
                    ) 
                ; MteTeTfsModCmntBwd ( ModCmnt ) 
                ELSE 
                  INC ( MteItemCt ) (* Count the Nl after. *) 
                ; MteTeIncEstChild ( ) 
                ; MteFwdPrevTok := LbeStd . Tok__CmntAtEndOfLine  
                ; MteFwdPrevItemCouldHaveCondNlAfter := TRUE 
                END (* IF *) 
              END (* IF *) 
            ELSE 
              MteTeIncEstChild ( ) 
            ; MteFwdPrevTok := LbeStd . Tok__Cmnt 
            ; MteFwdPrevItemCouldHaveCondNlAfter := TRUE 
            END (* IF *) 
          END MteTeTfsModCmntFwd

      ; PROCEDURE MteTeTfsModCmnt ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { Backout } 

        = VAR LNlBefore : BOOLEAN 
        ; VAR LIsTrailingCmnt : BOOLEAN 
   
        ; BEGIN (* MteTeTfsModCmnt *) 
            CASE MteState 
            OF MteStateTyp . MteStateStartAtEnd 
            => Assert 
                 ( ModCmnt . ModCmntNlAfter 
                 , AFT . A_MteTeTfsModCmntBadStartAtEndState 
                 ) 
            ; MteFinishItemNo := 1 
            ; INC ( MteItemCt ) (* Count the Nl after. *) 
            ; MteTeSetIndentInfo ( )  
            ; MteState := MteStateTyp . MteStatePassingNl  
            ; MteTeIncEstChild ( ) 

            | MteStateTyp . MteStateStartAtBeg 
            => MteFinishItemNo := 0 
            ; INC ( MteItemCt ) (* Count the comment. *) 
            ; MteTeSetIndentInfo ( )  
            ; MteState := MteStateTyp . MteStateFwd 
            ; MteTeTfsModCmntFwd ( ModCmnt ) 
 
            | MteStateTyp . MteStatePassingNl 
            => IF TRUE OR MtePassedDelNl 
               THEN 
                 INC ( MteItemCt ) (* Count the comment. *) 
            (* ELSE Make this the first item.  The starting item was a Nl, but 
                    not the rightmost of a group of adjacent Nls. This should 
                    not happen, but we self-repair if it does. *) 
               END 
            ; MteState := MteStateTyp . MteStateFwd 
            ; MteTeTfsModCmntFwd ( ModCmnt ) 

            | MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStateRightNlFound 
            => IF ModCmnt . ModCmntNlBefore 
               THEN 
                 LNlBefore := TRUE 
               ; LIsTrailingCmnt := FALSE 
               ELSE
                 TYPECASE ModCmnt 
                 OF ModHs . ModCmntLeadingFixedTyp 
                 => LNlBefore := ModCmnt . ModCmntFromPos < MteCharPos 
                    (* ^Implicit new line. *) 
                 ; LIsTrailingCmnt := FALSE 
                 | ModHs . ModCmntTrailingFixedTyp 
                 => LNlBefore := ModCmnt . ModCmntFromPos < MteCharPos 
                    (* ^Implicit new line. *) 
                 ; LIsTrailingCmnt := TRUE  
                 ELSE 
                   LNlBefore := FALSE 
                 ; LIsTrailingCmnt := FALSE (* Dead. *)  
                 END (* TYPECASE *) 
               END (* IF *) 
            ; Assert 
                ( ( MteState = MteStateTyp . MteStateRightNlFound ) 
                  <= (*IMPLIES*) LNlBefore  
               , AFT . A_MteTeTfsModCmntRightNlFoundNotNlBefore  
               ) 
            ; IF LNlBefore 
                 AND MteTeReverseOrPassNlBefore ( ) (* Has SIDE EFFECTS. *) 
               THEN (* We reversed. *) 
                 MteTeInterestingChildrenExist 
                   := MteTeInterestingChildrenExist 
                      OR FsNodeRef . FsDeletableItemsAreToRight  
                      OR ( FsNodeRef . FsKind = FsKindTyp. FsKindInsTok 
                           AND NOT LIsTrailingCmnt 
                         ) 
               ; MteTeMaybeFinishBwdEdit 
                   ( FmtNo := FsNodeRef . FsFmtNo 
                   , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                   ) 
               ELSE   
                 INC ( MteItemCt ) (* Count the comment. *) 
               ; MteTeTfsModCmntFwd ( ModCmnt ) 
               END (* IF *)

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => IF ModCmnt . ModCmntNlAfter 
               THEN 
                 DEC ( MteItemCt ) 
               ; IF MteItemCt = MteNlItemNo THEN MteLineShift := 0 END  
               ; IF MteItemCt <= 0 
                 THEN (* End backward traverse at the Nl after. *) 
                   MteTeMaybeFinishBwdEdit 
                     ( FsNodeRef . FsFmtNo 
                     , MarkNodeNo := MteTeRightChildRelNodeNo 
                     ) 
                 ; IF MteState # MteStateTyp . MteStateDone  
                   THEN (* Mark the Nl at end of comment. *) 
                     NewBolTokMark 
                       := Marks . TokMarkTyp 
                            { EstNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                            , EstNodeCt := 1   
                            , TkmEstRef
                                := MteTeEstTravInfo . EtiChildLeafElem
                                     . LeChildRef
                            , Kind := MarkKindTyp . Plain 
                            , FmtNo := FsNodeRef . FsFmtNo 
                            , StartAtEnd := TRUE 
                            , IsImpliedNewLine := FALSE 
                            , TmTok := LbeStd . Tok__CmntAtEndOfLine 
                            } 
                   ; INC ( MteMarkCt ) 
                   ; MteState := MteStateTyp . MteStateDone 
                   END 
                 ; MaxTouchedNodeNo 
                     := MAX ( MaxTouchedNodeNo 
                            , EstAbsNodeNo 
                              + MteTeEstTravInfo . EtiChildRelNodeNo 
                            ) 
                 ELSE 
                   MteTeTfsModCmntBwd ( ModCmnt ) 
                 END (* IF *) 
               ELSE 
                 MteTeTfsModCmntBwd ( ModCmnt ) 
               END (* IF *) 

            END (* CASE MteState *) 
          END MteTeTfsModCmnt 

      ; PROCEDURE MteTeTfsModTextBwd ( ModText : ModHs . ModTextTyp ) 
        RAISES { Backout } 
        (* Does not handle Nl after, nor trailing blanks. These are
           handled at various call sites, in different ways. *) 

        = VAR LStringToPos : LbeStd . LimitedCharNoSignedTyp 
        ; VAR LLength : SharedStrings . LengthTyp 
        ; VAR LNewModTextRef : ModHs . ModTextTyp 

        ; BEGIN (* MteTeTfsModTextBwd *) 
            LStringToPos := MteCharPos 
          ; LLength := SharedStrings . Length ( ModText . ModTextStringRef ) 
          ; DEC ( MteCharPos , LLength ) 
(* TODO: ^Something about the inverse saturation arithmetic here. *) 
          ; IF MteTouchedToPos 
               < ModText . ModTextLeftTokToPos + MteLineShift 
(* REVIEW: ^We should be able to do this if MteTouchedToPos < MteCharPos. *) 
            THEN (* Entire mod is to right of touched region. *) 
              IF MteModShift # 0 
              THEN (* Shift positions in the ModText. *) 
                LNewModTextRef := ModHs . CopyOfModText ( ModText ) 
              ; INC ( LNewModTextRef . ModTextFromPos , MteModShift ) 
              ; INC ( LNewModTextRef . ModTextLeftTokToPos , MteModShift ) 
              ; IF LNewModTextRef . ModTextToPos 
                   # LbeStd . LimitedCharNoInfinity 
                THEN 
                  INC ( LNewModTextRef . ModTextToPos , MteModShift ) 
                END (* IF*) 
              ; MteTeWaitChild 
                  ( LNewModTextRef 
                  , MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                  , FsNodeRef . FsFmtNo 
                  , EstHs . EdgeKindTyp . EdgeKindLeadingMod  
                  ) 
              ELSE (* Reuse the old ModText in the new Est. *) 
                MteTeWaitExistingEstChild 
                  ( EstHs . EdgeKindTyp . EdgeKindLeadingMod 
                  , IsInteresting := TRUE 
                  ) 
              END (* IF MteModShift # 0 *) 
            ; MteNewTextToPos 
                := EstUtil . WidthSum 
                     ( ModText . ModTextLeftTokToPos , MteSuffixShift ) 
            ELSIF MteTouchedFromPos <= MteCharPos 
            THEN (* Entire string is inside touched region. *) 
              MteTeBwdEditTouchedString ( ModText . ModTextStringRef ) 
            ; MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                ) 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , EstAbsNodeNo 
                         + MteTeEstTravInfo . EtiChildRelNodeNo 
                       ) 
            END 
          ; IF LStringToPos < MteTouchedFromPos 
            THEN (* Entire string, possibly empty, lies left of the touched 
                    region. 
                 *) 
              IF ModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
              THEN
                LNewModTextRef := ModHs . CopyOfModText ( ModText ) 
              ; LNewModTextRef . ModTextToPos := LStringToPos 
              ; MteTeWaitChild 
                  ( LNewModTextRef 
                  , MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                  , FsNodeRef . FsFmtNo 
                  , EstHs . EdgeKindTyp . EdgeKindLeadingMod  
                  ) 
              ELSE
                MteTeWaitExistingEstChild 
                  ( EstHs . EdgeKindTyp . EdgeKindLeadingMod 
                  , IsInteresting := TRUE 
                  ) 
              END (* IF *) 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , EstAbsNodeNo 
                         + MteTeEstTravInfo . EtiChildRelNodeNo 
                       ) 
            END (* IF *) 

          (* Leading blanks of the ModText. *) 
          ; MteTeBwdBlanks ( ) 
          ; DEC ( MteItemCt ) 
          ; MteTeMaybeFinishBwdEdit 
              ( FsNodeRef . FsFmtNo 
              , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
              ) 
          ; IF MteItemCt <= 0 
            THEN (* Backward traverse ends at beginning of the ModText. *) 
              Assert
                ( ModText . ModTextLeftTokToPos = 0 (* Nl before *) 
                , AFT . A_MteTeTfsModCmntBwd_EndNoNewLine 
                ) 
         (* ; MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                ) 
         *) 
            ; IF MteState # MteStateTyp . MteStateDone
                 AND ModText . ModTextLeftTokToPos = 0 (* Nl before. *) 
              THEN (* Mark the Nl at beginning of the text mod. *) 
                NewBolTokMark 
                  := Marks . TokMarkTyp 
                       { EstNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                       , EstNodeCt := 1
                       , TkmEstRef
                           := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       , Kind := MarkKindTyp . Plain 
                       , FmtNo := FsNodeRef . FsFmtNo 
                       , StartAtEnd := FALSE 
                       , IsImpliedNewLine := FALSE 
                       , TmTok := LbeStd . Tok__ModText 
                       } 
              ; INC ( MteMarkCt ) 
              ; MteState := MteStateTyp . MteStateDone 
              END (* IF*) 
            ELSIF ModText . ModTextLeftTokToPos = 0 (* Nl before *) 
                  AND MteItemCt = MteNlItemNo 
            THEN MteLineShift := 0 
            END 
          ; MteTeDecEstChild ( ) 
          END MteTeTfsModTextBwd 

      ; PROCEDURE MteTeTfsModTextFwd ( ModText : ModHs . ModTextTyp ) 
        RAISES { Backout } 
        (* This is a ModText in the old Est. *) 

        = BEGIN (* MteTeTfsModTextFwd *) 
            Assert 
               ( MteCharPos 
                 <= EstUtil . WidthSumSigned 
                      ( ModText . ModTextFromPos , MteLineShift ) 
               , AFT . A_MteTeTfsModTextForcedNl 
               ) 

          (* Handle Leading blanks of the ModText *) 
          ; MteTeFwdBlanks 
              ( ModText . ModTextFromPos + MteLineShift 
              , ExtendTouchedRightward := TRUE 
              ) 

          (* Continue with text of the ModText. *) 
          ; MteTeFwdString 
              ( LbeStd . Tok__ModText 
              , ModText . ModTextStringRef 
              , IsTrailingCmnt := FALSE 
              ) 
          ; MteFwdPrevTok := LbeStd . Tok__ModText (* Do even if text is empty. *) 
          ; MteModTextIsToLeftOnLine := TRUE 
          ; MteFwdPrevItemCouldHaveCondNlAfter := TRUE 
          ; IF ModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
            THEN (* Nl After *) 
              IF MteTeReverseOrPassNlAfter ( ) (* Has SIDE EFFECTS. *) 
              THEN (* We reversed at the end. Reprocess it backwards. *) 
                MteTeInterestingChildrenExist 
                  := MteTeInterestingChildrenExist 
                     OR FsNodeRef . FsDeletableItemsAreToRight  
                     OR FsNodeRef . FsKind = FsKindTyp. FsKindInsTok 
              ; MteTeMaybeFinishBwdEdit 
                  ( FmtNo := FsNodeRef . FsFmtNo 
                  , MarkNodeNo := MteTeRightChildRelNodeNo 
                  ) 
              ; MteTeTfsModTextBwd ( ModText ) 
              ELSE (* Move to the second line. *) 
                MteTeFwdBlanks 
                  ( MteLineShift , ExtendTouchedRightward := TRUE ) 
              ; INC ( MteItemCt ) (* Count the NlAfter. *) 
              ; MteTeIncEstChild ( ) 
              END (* IF *) 
            ELSE (* Handle the trailing blanks. *) 
              Assert 
                ( MteCharPos 
                  <= EstUtil . WidthSumSigned 
                       ( ModText . ModTextToPos , MteLineShift ) 
                , AFT . A_MteModTextNoTrailingSep 
                ) 
            ; MteFwdPrevTok := LbeStd . Tok__ModText 
            ; MteTeFwdBlanks 
                ( ModText . ModTextToPos + MteLineShift 
                , ExtendTouchedRightward := TRUE 
                ) 
            ; MteTeIncEstChild ( ) 
            END (* IF *) 
          END MteTeTfsModTextFwd 

      ; PROCEDURE MteTeTfsModText ( ModText : ModHs . ModTextTyp ) 
        RAISES { Backout } 
        (* This is a ModText in the old Est. *) 
        = VAR LNlBefore : BOOLEAN 

        ; BEGIN (* MteTeTfsModText *) 
            CASE MteState 
            OF MteStateTyp . MteStateStartAtEnd 
            => Assert 
                 ( ModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
                 , AFT . A_MteTeTfsModTextBadStartAtEnd 
                 ) 
            ; MteFinishItemNo := 1 
            ; INC ( MteItemCt ) (* For the Nl after *) 
            ; MteTeSetIndentInfo ( )  
            ; MteState := MteStateTyp . MteStatePassingNl 
            ; MteTeIncEstChild ( ) 

            | MteStateTyp . MteStateStartAtBeg 
            => MteFinishItemNo := 0 
            ; INC ( MteItemCt ) (* Count the ModText. *) 
            ; MteTeSetIndentInfo ( )  
            ; MteState := MteStateTyp . MteStateFwd 
            ; MteTeTfsModTextFwd ( ModText ) 
 
            | MteStateTyp . MteStatePassingNl 
            => IF FALSE AND 
                  NOT MtePassedDelNl 
                  AND MteItemCt = 1 
                  AND ( ModText . ModTextFromPos > 0 
                        OR ModText . ModTextToPos > 0 
                      ) 
               THEN (* Make this the first item.  The starting item was a Nl,
                       but not the rightmost of a group of adjacent Nls.
                       This should not happen, but we self-repair if it does. 
                       However, do count an unnecessary ModText, so it can 
                       be removed during backwards traversal. *) 
               ELSE  
                 INC ( MteItemCt ) (* Count the ModText. *) 
               END 
            ; MteState := MteStateTyp . MteStateFwd 
            ; MteTeTfsModTextFwd ( ModText ) 

            | MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStateRightNlFound 
            => LNlBefore := ModText . ModTextLeftTokToPos = 0 
            ; Assert 
                ( ( MteState = MteStateTyp . MteStateRightNlFound ) 
                  <= (*IMPLIES*) LNlBefore  
               , AFT . A_MteTeTfsModTextRightNlFoundNotNlBefore  
               ) 
            ; IF LNlBefore 
                 AND MteTeReverseOrPassNlBefore ( ) (* Has SIDE EFFECTS. *) 
               THEN (* We reversed. *) 
                 MteTeInterestingChildrenExist 
                   := MteTeInterestingChildrenExist 
                      OR FsNodeRef . FsDeletableItemsAreToRight  
                      OR FsNodeRef . FsKind = FsKindTyp. FsKindInsTok 
               ; MteTeMaybeFinishBwdEdit 
                   ( FmtNo := FsNodeRef . FsFmtNo 
                   , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                   ) 
               ELSE   
                 INC ( MteItemCt ) (* Count the ModText. *) 
               ; MteTeTfsModTextFwd ( ModText ) 
               END (* IF *)

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => IF ModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
               THEN (* Nl After. *) 
                 DEC ( MteItemCt ) 
               ; IF MteItemCt = MteNlItemNo THEN MteLineShift := 0 END  
               ; IF MteItemCt <= 0 
                 THEN (* End backward traverse at the Nl after. *) 
                     MteTeMaybeFinishBwdEdit 
                       ( FsNodeRef . FsFmtNo 
                       , MarkNodeNo := MteTeRightChildRelNodeNo 
                       ) 
                   ; IF MteState # MteStateTyp . MteStateDone
                        AND ModText . ModTextToPos
                            =  LbeStd . LimitedCharNoInfinity (* Nl after. *) 
                     THEN (* Mark the Nl at end of this ModText. *)
                       NewBolTokMark 
                         := Marks . TokMarkTyp 
                              { EstNodeNo
                                  := MteTeEstTravInfo . EtiChildRelNodeNo 
                              , EstNodeCt := 1  
                              , TkmEstRef
                                  := MteTeEstTravInfo . EtiChildLeafElem
                                     . LeChildRef 
                              , Kind := MarkKindTyp . Plain 
                              , FmtNo := FsNodeRef . FsFmtNo 
                              , StartAtEnd := TRUE 
                              , IsImpliedNewLine := FALSE 
                              , TmTok := LbeStd . Tok__ModText 
                              } 
                     ; INC ( MteMarkCt ) 
                     ; MteState := MteStateTyp . MteStateDone 
                     END
                 ELSE (* Handle the trailing blanks here. *) 
                   MteTeBwdBlanks ( ) 
                 ; MteTeMaybeFinishBwdEdit 
                     ( FsNodeRef . FsFmtNo 
                     , MarkNodeNo := MteTeRightChildRelNodeNo 
                     ) 
(* FIXME: May have to adjust ModTextToPos. *) 
                 ; MteTeTfsModTextBwd ( ModText ) 
                 END (* IF *) 
               ; MaxTouchedNodeNo 
                   := MAX ( MaxTouchedNodeNo 
                          , EstAbsNodeNo 
                            + MteTeEstTravInfo . EtiChildRelNodeNo 
                          ) 
               ELSE (* Handle the trailing blanks here. *) 
                 MteTeBwdBlanks ( ) 
               ; MteTeMaybeFinishBwdEdit 
                   ( FsNodeRef . FsFmtNo 
                   , MarkNodeNo := MteTeRightChildRelNodeNo 
                   ) 
(* FIXME: May have to adjust ModTextToPos. *) 
               ; MteTeTfsModTextBwd ( ModText ) 
               END (* IF *) 

            END (* CASE MteState *) 
          END MteTeTfsModText 

      ; PROCEDURE MteTeTfsModTok ( EstRef : EstHs . EstRefTyp ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LLMCharPos : LbeStd . LimitedCharNoTyp 

        ; BEGIN 
            CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 
            => MteTeSetIndentInfo ( )
               (* Stay in the start state, to reach Nl before of the leftmost
                  child of EstRef.  Will come back in MteStateDone. *)
         (* ; IF StartTokMark . EstNodeNo 
                 = EstAbsNodeNo + MteTeEstTravInfo . EtiChildRelNodeNo
                 AND StartTokMark . Kind = MarkKindTyp . Plain 
              THEN (* StartTokMark denotes this ModTok.  This would mean 
                      starting at an implied Nl for the ModTok, which can't
                      happen. *)  
                MteCharPos := MteTeIndentPos 
              ; MteState := MteStateTyp . MteStatePassingNl 
              ; INC ( MteItemCt ) (* Count the ModTok node itself. *)
              ; MteFinishItemNo := 1 
                (* ^Note the first thing to the right to take care of
                   FinishBwdEdit. *) 
              ELSE (* Descend through the ModTok, without counting it. *)      
              END (* IF *)
         *)

            | MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateFwd 
            => MteTeSetIndentInfo ( )  
            ; INC ( MteItemCt ) 
              (* ^This is unusual in counting a non-leaf node.  We will need
                 this to detect when we reach the ModTok node going backwards,
                 to see if it is the middle Nl. *)
            ; MteState := MteStateTyp . MteStateFwd 
         (* ; IF EstUtil . CharPosPlusWidthInfo 
                    ( MteCharPos , EstRef . KTreeWidthInfo ) 
                  > Options . RightMargin 
              THEN (* Implied Nl before the ModTok, which ends this line. *) 
                IF MteTeReverseOrPassNlBefore ( ) 
                THEN (* The ModTok has already been Decremented. *)
                  MteTeInterestingChildrenExist := TRUE 
                ; DEC ( MteItemCt )
                ; IF MteItemCt <= 0
                  THEN MteState := MteStateTyp . MteStateDone 
                  END (* IF *) 
                ; RETURN 
                END (* IF *) 
              END (* IF *)
         *)

            | MteStateTyp . MteStateRightNlFound 
            => (* Reverse short of this ModTok. *) 
              MteTeReverse ( )
            ; MteTeDecEstChild ( ) 
            ; MteTeInterestingChildrenExist := TRUE 
            ; RETURN 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => MteTeSetIndentInfo ( ) 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , EstAbsNodeNo 
                         + MteTeEstTravInfo . EtiChildRelNodeNo 
                       ) 
            END (* CASE MteState *)

          (* Recurse: *) 
          ; MteTeTfsEstSubtree 
              ( IsModTok := TRUE
              , (* VAR *) LMCharPos := LLMCharPos (* Dead. *) 
              )
              
          ; CASE MteState
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 
            => CantHappen ( AFT . A_MteTeTfsModTok__Bad_state_after ) 

            | MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStateRightNlFound 
            => (* Keep on traversing forwards. *) 

            | MteStateTyp . MteStateDone
            => Assert
                 ( MteItemCt <= 0 , AFT . A_MergeTxtEdit__ModTok_excess_count )
              (* Keep on traversing backwards (& up). *) 
            
            | MteStateTyp . MteStateBwdNl
            => Assert
                 ( MteItemCt >= 1 , AFT . A_MergeTxtEdit__ModTok_lost_count )
            ; DEC ( MteItemCt ) 
               (* And keep on traversing backwards (& up). *)
               
            | MteStateTyp . MteStateBwdEdit 
            => MteTeSetIndentInfo ( )
            ; IF MteItemCt = 1  
              THEN (* The starting Nl was to the right of the ModTok
                      subtree.  Keep on traversing backwards (& up). *)
              ELSE
(* 
                Assert
                  ( MteState IN MteStateSetBwdFinished  
                  , AFT . A_MteTeTfsModTok__Bad_state  
                  ) 
              ; IF MteState # MteStateTyp . MteStateDone  
                THEN  
                  NewBolTokMark 
                    := Marks . TokMarkTyp 
                         { EstNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo
                         , EstNodeCt 
                             := EstUtil . EstNodeCt 
                                  ( MteTeEstTravInfo . EtiChildLeafElem
                                    . LeChildRef
                                  ) 
                         , TkmEstRef
                             := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef
                         , Kind := MarkKindTyp . Plain
                         , FmtNo := FsNodeRef . FsFmtNo 
                         , StartAtEnd := FALSE 
                         , IsImpliedNewLine := FALSE 
                         , TmTok := EstRef . EstTok  
                         } 
                ; INC ( MteMarkCt) 
                ; MteState := MteStateTyp . MteStateDone 
                END
              ELSE
                Assert 
                  ( MteState # MteStateTyp . MteStateDone 
                  , AFT . A_MteTeTfsModTok__Done_before_starting_Nl  
                  ) 
              ; IF MteItemCt = MteNlItemNo THEN MteLineShift := 0 END  
              ; DEC ( MteItemCt )
              ; IF MteItemCt <= 0
                THEN (* Started at the ModTok node itself. *) 
                  NewBolTokMark 
                    := Marks . TokMarkTyp 
                         { EstNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo
                         , EstNodeCt 
                             := EstUtil . EstNodeCt 
                                  ( MteTeEstTravInfo . EtiChildLeafElem
                                    . LeChildRef
                                  ) 
                         , TkmEstRef
                             := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef
                         , Kind := MarkKindTyp . Plain
                         , FmtNo := FsNodeRef . FsFmtNo 
                         , StartAtEnd := FALSE 
                         , IsImpliedNewLine := FALSE 
                         , TmTok := EstRef . EstTok  
                         } 
                ; INC ( MteMarkCt) 
                ; MteState := MteStateTyp . MteStateDone 
                END (* IF *) 
*)
              END (* IF *)
            END (* CASE MteState *) 
          END MteTeTfsModTok

      ; PROCEDURE MteTeTfsLexErrChars ( String : SharedStrings . T ) 
        RAISES { Backout } 

        = VAR LDoExcludeIfBwd : BOOLEAN 
 
        ; BEGIN (* MteTeTfsLexErrChars *) 
            CASE MteState <* NOWARN *> 
            OF MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateFwd 
            => MteTeTok 
                 ( SharedStrings . Tok ( String ) 
                 , String 
                 , FsNodeRef 
                 , FsFmtKind 
                 , (* VAR *) DoExcludeIfBwd := LDoExcludeIfBwd (* Dead. *)
                 )  
            ; MteState := MteStateTyp . MteStateFwd 
            ; MteFwdPrevItemCouldHaveCondNlAfter := FALSE 
            ; INC ( MteItemCt ) 
            ; MteTeIncEstChild ( ) 

            | MteStateTyp . MteStateRightNlFound 
            => MteTeWaitChildrenToRight ( )  
            ; MteTeReverse ( ) 
            ; MteTeDecEstChild ( ) 
            ; MteTeInterestingChildrenExist 
                := MteTeInterestingChildrenExist 
                   OR FsNodeRef . FsDeletableItemsAreToRight  
                   OR FsNodeRef . FsKind = FsKindTyp. FsKindInsTok 
            ; MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeRightChildRelNodeNo 
                )

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone (* Shouldn't happen. *)  
            => MteTeTok 
                 ( SharedStrings . Tok ( String ) 
                 , String 
                 , FsNodeRef 
                 , FsFmtKind 
                 , (* VAR *) DoExcludeIfBwd := LDoExcludeIfBwd 
                 )  
            ; IF NOT LDoExcludeIfBwd 
              THEN (* Keep the LexErrChars mod. *)  
                MteTeWaitExistingEstChild 
                  ( EstHs . EdgeKindTyp . EdgeKindEstChild
                  , IsInteresting := TRUE 
                  ) 
              END (* IF *) 
            ; DEC ( MteItemCt ) 
            ; MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo
                ) 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , EstAbsNodeNo + MteTeEstTravInfo . EtiChildRelNodeNo 
                       ) 
            ; MteTeDecEstChild ( )
         (* ELSE Can't happen. *) 
            END (* CASE MteState *) 
          END MteTeTfsLexErrChars  

      ; PROCEDURE MteTeTfsModErr ( <* UNUSED *> ModErr : ModHs . ModErrTyp ) 
        RAISES { Backout } 

        = VAR LMarkNodeNo : LbeStd . EstNodeNoTyp
        ; VAR LMarkEstRef : LbeStd . EstRootTyp 

        ; BEGIN (* MteTeTfsModErr *) 
            CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
(* CHECK: How could we start at the beginning of one of these? *) 
            => MteFinishItemNo := 0 
            ; INC ( MteItemCt ) 
            ; MteTeSetIndentInfo ( )  
            ; MteState := MteStateTyp . MteStateFwd 
            ; MteTeIncEstChild ( ) 

            | MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateFwd 
            => INC ( MteItemCt ) 
            ; MteTeIncEstChild ( ) 

            | MteStateTyp . MteStateRightNlFound 
            => MteTeWaitChildrenToRight ( ) 
            ; MteTeReverse ( ) 
            ; MteTeDecEstChild ( ) 
            ; MteTeInterestingChildrenExist 
                := MteTeInterestingChildrenExist 
                   OR FsNodeRef . FsDeletableItemsAreToRight  
                   OR FsNodeRef . FsKind = FsKindTyp. FsKindInsTok 
            ; MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeRightChildRelNodeNo
                ) 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone (* Shouldn't happen. *) 
            => IF MteCharPos >= MteTouchedToPos 
                  OR MteState = MteStateTyp . MteStateBwdNl 
               THEN (* We are outside the touched region. *) 
                 MteTeWaitExistingEstChild 
                   ( EstHs . EdgeKindTyp . EdgeKindLeadingMod 
                   , IsInteresting := TRUE 
                   ) 
                 (* ^Keep the error mod. *) 
               ; LMarkNodeNo := MteTeRightChildRelNodeNo 
               ; LMarkEstRef := MteTeRightChildRef 
(* FIXME:        ^ *) 
               ELSE 
                 LMarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
               ; LMarkEstRef
                   := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef
               END (* IF *) 
            ; DEC ( MteItemCt ) 
            ; MteTeMaybeFinishBwdEdit 
                ( FmtNo := FsNodeRef . FsFmtNo
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo
                ) 
            ; IF MteItemCt <= 0 
(* CHECK ^In case error could be implicitly the first item on a line. 
          Can this happen? *) 
              THEN
                IF FALSE (* Should never be an implied Nl. *) 
                   AND MteState # MteStateTyp . MteStateDone  
                THEN 
                  NewBolTokMark 
                    := Marks . TokMarkTyp 
                         { EstNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo  
                         , EstNodeCt := 1   
                         , TkmEstRef
                             := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef
                         , Kind := MarkKindTyp . Plain 
                         , FmtNo := FsNodeRef . FsFmtNo , StartAtEnd := FALSE 
                         , IsImpliedNewLine := TRUE  
                         , TmTok := LbeStd . Tok__Null  
                         } 
                ; INC ( MteMarkCt ) 
                ; MteState := MteStateTyp . MteStateDone   
                END (* IF *) 
              END (* IF *) 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , EstAbsNodeNo + MteTeEstTravInfo . EtiChildRelNodeNo 
                       ) 
            ; MteTeDecEstChild ( ) 

            ELSE 
              CantHappen ( AFT . A_MteTeTfsModErrBadState ) 
            END (* CASE MteState *) 
          END MteTeTfsModErr 

      ; PROCEDURE MteTeTfsEstSubtree 
          ( IsModTok : BOOLEAN 
          ; VAR LMCharPos : LbeStd . LimitedCharNoTyp 
            (* ^Position of leftmost nonblank of the subtree.  
               LimitedCharNoUnknown, if none exists. *) 
          ) 
        RAISES { Backout , Thread . Alerted } 
        (* DOES handle ModTok. *) 

        = VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR LChildFmtKind : LangUtil . FmtKindTyp 
        ; VAR LNewEstRef : LbeStd . EstRootTyp 
        ; VAR LEstChildKindSet : EstHs . EstChildKindSetTyp 
        ; VAR LNewSingletonOptKindSet : EstHs . EstChildKindSetTyp 
        ; VAR LEstDummyTokRef : ModHs . EstDummyTokTyp 
        ; VAR LChildIndentPos1 : LbeStd . LimitedCharNoTyp 
        ; VAR LChildIndentPosN : LbeStd . LimitedCharNoTyp 
        ; VAR LWasGoingFwd : BOOLEAN := FALSE 

        ; BEGIN (* MteTeTfsEstSubtree *) 
            LChildFsNodeRef  
              := LangUtil . FsRuleForEstChild
                   ( Lang , FsNodeRef , MteTeEstTravInfo . EtiChildLeafElem ) 
          ; CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 
            => LWasGoingFwd := TRUE 
            ; MteTeSetIndentInfo ( )
            ; IF IsModTok 
              THEN 
                LChildIndentPos1 := MteTeIndentPos 
              ; LChildIndentPosN := MteTeIndentPos 
              ; LChildFmtKind := LangUtil . FmtKindTyp . FmtKindHoriz 
              ; MteCharPos := MteTeIndentPos 
              ELSE 
                TravUtil . ChildIndentPositions 
                  ( Lang 
                  , FsNodeRef 
                  , EstIndentPos1 
                  , EstIndentPosN
                  , (* VAR *) ChildIndentPos1 := LChildIndentPos1 
                  , (* VAR *) ChildIndentPosN := LChildIndentPosN 
                  , IsFirstLine := MteTeIsFirstLine 
                  ) 
              ; LChildFmtKind 
                  := TravUtil . FmtKindForEstDescending  
                       ( FsKind := LChildFsNodeRef . FsKind 
                       , ParentFmtKind := FsFmtKind 
                       , FirstLineIndentPos := LChildIndentPos1 
                       , EstRef 
                           := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       , StartMark := StartTokMark 
                       , StartMarkIsKnownNl := TRUE 
                       ) 
              END (* IF *) 

            | MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStateRightNlFound 
            => LWasGoingFwd := TRUE 
            ; IF IsModTok 
              THEN 
                LChildIndentPos1 := MteTeIndentPos 
              ; LChildIndentPosN := MteTeIndentPos 
              ; LChildFmtKind := LangUtil . FmtKindTyp . FmtKindHoriz 
              ELSE 
                TravUtil . ChildIndentPositions 
                  ( Lang 
                  , FsNodeRef 
                  , EstIndentPos1 
                  , EstIndentPosN
                  , (* VAR *) ChildIndentPos1 := LChildIndentPos1 
                  , (* VAR *) ChildIndentPosN := LChildIndentPosN 
                  , IsFirstLine := MteTeIsFirstLine 
                  ) 
              ; LChildFmtKind 
                  := TravUtil . FmtKindForEstTraversing 
                       ( Lang := Lang 
                       , CharPos := MteCharPos - MteLineShift 
(* TODO: ^Something about the saturation arithmetic here. *) 
                       , ModTextIsToLeftOnLine := MteModTextIsToLeftOnLine 
                       , PrevTok := MteFwdPrevTok  
                       , FsKind := LChildFsNodeRef . FsKind 
                       , ParentFmtKind := FsFmtKind 
                       , FirstLineIndentPos := LChildIndentPos1
                       , EstRef 
                           := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       ) 
              END (* IF *) 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => (* At this point, we have not yet backed up past the list 
                  separators, if any.  This will use MteCharPos without any
                  separator, which is right for this case. 
               *) 
              MteTeFlushDels ( ) 
            ; EVAL MteTeFlushBlankLines ( ) 
              (* Waiting things not yet closed will be safe because they 
                 use variables local to MergeTextEdit *) 
(* CHECK:        ^Huh? *) 
            ; LWasGoingFwd := FALSE 
            ; LChildFmtKind := LangUtil . FmtKindTyp . FmtKindUnknown 
            ; LChildIndentPos1 := LbeStd . LimitedCharNoUnknown 
            ; LChildIndentPosN := LbeStd . LimitedCharNoUnknown 

            END (* CASE *)

          ; MteTraverseEst 
              ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              , MteTeEstTravInfo . EtiChildLeafElem . LeKindSet  
              , EstAbsNodeNo + MteTeEstTravInfo . EtiChildRelNodeNo 
              , ParentFsNodeRef := FsNodeRef 
              , RootFsNodeRef := LChildFsNodeRef 
              , EstFmtKind := LChildFmtKind 
              , EstIndentPos1 := LChildIndentPos1 
              , EstIndentPosN := LChildIndentPosN 
              , (* VAR *) NewEstRef := LNewEstRef
              , (* VAR *) NewSingletonOptKindSet := LNewSingletonOptKindSet 
              , (* VAR *) LMCharPos := LMCharPos  
              ) 
          ; IF MteState = MteStateTyp . MteStateDone 
            THEN 
              INC ( NewBolTokMark . EstNodeNo 
                  , MteTeEstTravInfo . EtiChildRelNodeNo 
                  )
(*
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo , NewBolTokMark . EstNodeNo ) 
(* FIXME: There is a fundamental inconsistency in this.  This computation uses
          node numbers in the new est, but everywhere else, it is the old tree.
*)
*)
            END (* IF *) 
          ; CASE MteState (* Which may have changed since above. *)  
            OF MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateRightNlFound 
            => MteTeIncEstChild ( ) 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone 
            => IF LWasGoingFwd 
               THEN (* A reversal happened deeper in the Est.  Start 
                       backwards processing for this level. *) 
                 MteTeWaitChildrenFartherToRight ( ) 
               END (* IF *) 
            ; IF LNewEstRef 
                 # MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                 OR LNewSingletonOptKindSet 
                    # MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                      * EstHs . EstChildKindSetOptSingletonList 
              THEN (* Something has changed from below. *) 
                IF LNewEstRef # NIL 
                THEN 
                  TYPECASE LNewEstRef 
                  OF EstHs . EstRefTyp ( TEstRef ) 
                  => IF TEstRef . EstNodeKind 
                        = EstHs . EstNodeKindTyp . EstNodeKindModTok 
                    THEN (* Probably we always get here exactly when IsModTok, 
                            but this is more robust. *) 
                      LEstChildKindSet := EstHs . EstChildKindSetModTok  
                    ELSE 
                      LEstChildKindSet := EstHs . EstChildKindSetEstChildNonNIL 
                    END (* IF *) 
                  ELSE 
                    LEstChildKindSet := EstHs . EstChildKindSetEstChildNonNIL 
                  END (* TYPECASE *)  
                ; MteTeWaitChild 
                    ( LNewEstRef 
                    , EstUtil . EstChildKindSet ( LNewEstRef )  
                      * EstHs . EstChildKindSetCopyUp 
                      + LEstChildKindSet 
                      + EstHs . EstChildKindSetContainsSyntMod 
                      + LNewSingletonOptKindSet 
                    , FsNodeRef . FsFmtNo 
                    , EstHs . EdgeKindTyp . EdgeKindEstChild   
                    ) 
                ELSIF ( FsNodeRef . FsIsInsideCondFmt AND NOT IsModTok ) 
                      OR EstHs . EstChildKindOptSingletonList IN KindSet 
                THEN (* Merge a dummy est node. *) 
                  LEstDummyTokRef := NEW ( ModHs . EstDummyTokTyp )
                ; LEstDummyTokRef . Tok 
                    := EstUtil . EstTok 
                         ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef )  
                ; LEstDummyTokRef . ApproxChildCt 
                    := EstUtil . ApproxChildCt 
                         ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef )  
                ; MteTeWaitChild 
                    ( LEstDummyTokRef 
                    , EstHs . EstChildKindSetEstChildNonNIL  
                      + EstHs . EstChildKindSetContainsSyntMod 
                    , FsNodeRef . FsFmtNo 
                    , EstHs . EdgeKindTyp . EdgeKindEstChild   
                    ) 
                ELSIF NOT IsModTok AND FsNodeRef . FsIsInsideList 
                THEN (* In case of an Est list, there could be mods attached
                        by format number to this list element or its list 
                        separator tokens, even though the element has 
                        disappeared, so merge a NIL just in case. 
                      *) 
                  MteTeWaitChild 
                    ( LNewEstRef 
                    , EstHs . EstChildKindSetEstChild 
                      + EstHs . EstChildKindSetContainsSyntMod 
                    , FsNodeRef . FsFmtNo 
                    , EstHs . EdgeKindTyp . EdgeKindEstChild   
                    ) 
                ELSE (* Omit the old child. *) 
                  MteTeInterestingChildrenExist 
                    := MteTeInterestingChildrenExist 
                       OR ( LWasGoingFwd (* We reversed below *) 
                            AND FsNodeRef . FsDeletableItemsAreToRight  
                          ) 
                END (* IF *) 
              ELSIF EstHs . EstChildKindOptSingletonList IN KindSet 
              THEN (* We are traversing an optimized singleton list, and it is
                      unchanged.  Don't mess with children, as they are 
                      actually children of the one list element.  Do nothing, 
                      and the Est for the list element and its OptSingletonList
                      value will pass up unchanged.
                   *) 
              ELSE 
                MteTeWaitExistingEstChild 
                  ( EstHs . EdgeKindTyp . EdgeKindEstChild 
                  , IsInteresting := TRUE 
                  ) 
              END (* IF *) 
            ; MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                ) 
            ; MteTeDecEstChild ( ) 
            ELSE 
              CantHappen ( AFT . A_MteTeTfsEstSubtree_BadState ) 
            END (* CASE MteState *) 
          END MteTeTfsEstSubtree 

      ; PROCEDURE MteTeTfsLeadingModsNoDel ( ) 
        RAISES { Backout , Thread . Alerted } 
        (* Works in either direction. Does not handle a delete mod. *) 

        = BEGIN (* MteTeTfsLeadingModsNoDel *) 
            LOOP 
(* CHECK: Is there a good reason not to call TravUtil.CheckModFwd here?
          Or, would it be better to eliminate it everywhere, since it
          entails a more-or-less duplicated TYPECASE?
*)  
              IF MteTeEstTravInfo . EtiChildNo 
                 >= MteTeEstTravInfo . EtiChildCt 
                 OR MteTeEstTravInfo . EtiChildNo < 0 
                    (* No children at all. *) 
                 OR FsNodeRef . FsFmtNo # MteTeEstTravInfo . EtiChildFmtNo 
                    (* Not for this format number. This could miss a leading 
                       delete, but we check for those separately anyway. *) 
              THEN 
                EXIT 
              END (* IF *) 

            (* Found a child with matching FmtNo. *) 
            ; TYPECASE MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => EXIT 

              (* Blank line Mod. *) 
              | ModHs . ModBlankLineTyp ( TModBlankLine ) 
              => MteTeTfsModBlankLine ( TModBlankLine ) 

              (* Leading comment. *) 
              | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
              => MteTeTfsModCmnt ( TModCmnt ) 

              (* Text Mod. *) 
              | ModHs . ModTextTyp ( TModText ) 
              => MteTeTfsModText ( TModText ) 

              (* Token Mod. *) 
              | EstHs . EstRefTyp ( TEstRef ) 
              => IF TEstRef . EstNodeKind 
                    = EstHs . EstNodeKindTyp . EstNodeKindModTok 
                 THEN 
                   MteTeTfsModTok ( TEstRef ) 
                 ELSE (* Not a mod *) 
                   EXIT 
                 END (* IF *) 

              (* Lex error characters *) 
              | SharedStrings . T ( TString ) 
              => IF SharedStrings . Tok ( TString ) = LbeStd . Tok__LexErrChars 
                 THEN 
                   MteTeTfsLexErrChars ( TString ) 
                 ELSE (* Not a Mod. *) 
                   EXIT 
                 END (* IF *) 

              (* Errors. *) 
              | ModHs . ModErrTyp ( TModErr ) 
              => MteTeTfsModErr ( TModErr ) 

              ELSE (* A delete or not a leading mod. *) 
                EXIT 
              END (* TYPECASE *) 
            ; IF MteItemCt <= 0 THEN EXIT END (* IF *) 
            END (* LOOP *) 
          END MteTeTfsLeadingModsNoDel 

      ; PROCEDURE MteTeTfsTrailingMods ( ) RAISES { Backout } 
        (* Works in either direction. *) 

        = BEGIN (* MteTeTfsTrailingMods *) 
            LOOP 
              IF MteTeEstTravInfo . EtiChildNo 
                 >= MteTeEstTravInfo . EtiChildCt 
                 OR MteTeEstTravInfo . EtiChildNo < 0 
                    (* No children at all. *) 
                 OR FsNodeRef . FsFmtNo # MteTeEstTravInfo . EtiChildFmtNo 
                    (* Not for this format number. *) 
              THEN 
                EXIT 
              END (* IF *) 

            (* Found a child with matching FmtNo. *) 
            ; TYPECASE MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => (* Not a trailing mod. *) 
                 EXIT 
              | ModHs . ModCmntTrailingTyp ( TModCmnt ) 
              => MteTeTfsModCmnt ( TModCmnt ) 
              ELSE (* Not a trailing mod. *) 
                EXIT 
              END (* TYPECASE *) 
            ; IF MteItemCt <= 0 THEN EXIT END (* IF *) 
            END (* LOOP *) 
          END MteTeTfsTrailingMods 

      ; PROCEDURE MteTeTfsFwdLeadingDel 
          ( VAR Delete : BOOLEAN ; VAR IsRepair : BOOLEAN ) 
        RAISES { Backout } 

        = BEGIN (* MteTeTfsFwdLeadingDel *) 
            Delete := FALSE 
          ; IsRepair := FALSE 
          ; Assert 
              ( MteState IN MteStateSetStartOrFwd  
              , AFT . A_MteTeTfsFwdLeadingDelBadState 
              ) 
          ; IF MteTeEstTravInfo . EtiChildNo < MteTeEstTravInfo . EtiChildCt 
            THEN 
              TYPECASE MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => 
              | ModHs . ModDelTyp ( TModDel ) 
              => (* A Deletion mod is next. *) 
                 Assert 
                   ( FsNodeRef . FsFmtNo <= TModDel . ModDelThruFmtNo 
                   , AFT . A_MteTeTfsFwdLeadingDel_Skipped_Del_Mod 
                   ) 
              ; Assert 
                  ( NOT FsNodeRef . FsIsInsideList 
                    OR FsNodeRef . FsFmtNo # EstHs . FmtNoListEstChild
                  , AFT . A_MteTeTfsFwdLeadingDel_Deleted_Est_List_Child 
                  ) 
              ; IF FsNodeRef . FsFmtNo >= MteTeEstTravInfo . EtiChildFmtNo 
                THEN (* The delete mod applies to this token. *) 
                  Delete := TRUE 
                ; IsRepair 
                    := EstHs . EstChildKindContainsInsertionRepair 
                       IN MteTeEstTravInfo . EtiChildLeafElem . LeKindSet
                ; IF FsNodeRef . FsFmtNo = TModDel . ModDelThruFmtNo 
                  THEN (* consume the del mod. *) 
                    MteTeIncEstChild ( ) 
                  END (* IF *) 
                END (* IF *) 
              ELSE 
              END (* TYPECASE *) 
            END (* IF *) 
          END MteTeTfsFwdLeadingDel 

      ; PROCEDURE MteTeTfsBwdLeadingDel 
          ( VAR Delete : BOOLEAN ; VAR IsRepair : BOOLEAN ) 
        RAISES { Backout } 

        = BEGIN (* MteTeTfsBwdLeadingDel *) 
            Delete := FALSE 
          ; IsRepair := FALSE 
          ; Assert 
              ( MteState IN MteStateSetBwdOrDone  
              , AFT . A_MteTeTfsBwdLeadingDelBadState 
              ) 
          ; IF 0 <= MteTeEstTravInfo . EtiChildNo 
            THEN 
              TYPECASE MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => 
              | ModHs . ModDelTyp ( TModDel ) 
              => (* A Deletion mod is next. *) 
                 Assert 
                   ( FsNodeRef . FsFmtNo 
                     >= MteTeEstTravInfo . EtiChildFmtNo 
                   , AFT . A_MteTeTfsBwdLeadingDel_Skipped_Delete_Mod 
                   ) 
              ; Assert 
                  ( NOT FsNodeRef . FsIsInsideList 
                    OR FsNodeRef . FsFmtNo # EstHs . FmtNoListEstChild
                  , AFT . A_MteTeTfsBwdLeadingDel_Deleted_Est_List_Child 
                  ) 
              ; IF FsNodeRef . FsFmtNo <= TModDel . ModDelThruFmtNo 
                THEN (* The delete mod applies to this token. *) 
                  Delete := TRUE
                ; IF MteCharPos < MteTouchedToPos 
                  THEN
                    MaxTouchedNodeNo 
                      := MAX ( MaxTouchedNodeNo 
                             , EstAbsNodeNo 
                               + MteTeEstTravInfo . EtiChildRelNodeNo 
                             ) 
                  END (* IF *) 
                ; IsRepair 
                    := EstHs . EstChildKindContainsInsertionRepair 
                       IN MteTeEstTravInfo . EtiChildLeafElem . LeKindSet
                ; IF FsNodeRef . FsFmtNo = MteTeEstTravInfo . EtiChildFmtNo 
                  THEN (* This Fs item is the leftmost one deleted. 
                          Consume it. *) 
                    MteTeDecEstChild ( ) 
                  END (* IF *) 
                END (* IF *) 
              ELSE 
              END (* TYPECASE *) 
            END (* IF *) 
          END MteTeTfsBwdLeadingDel 

      ; PROCEDURE MteTeTfsBuildFmtNoMark ( ) 

        = VAR LMarkKind : MarkKindTyp
        ; VAR LMarkEstRef : LbeStd . EstRootTyp 
        ; VAR LEstNodeNo : LbeStd . EstNodeNoTyp 
        ; VAR LEstNodeCt : LbeStd . EstNodeNoTyp 

        ; BEGIN 
            IF MteTeEstChildrenAreToRightInNewEst ( ) 
            THEN (* There will be an Est child to the right, which
                    the mark will point to. *) 
              LMarkKind := MarkKindTyp . LeftSibFmtNo 
            ; LEstNodeNo := MteTeRightChildRelNodeNo 
              (* ^Tricky.  This will also be the correct relative node no in 
                 the yet-to-be-constructed new Est node. 
              *)
(* We don't have the needed value of  MteTeLMNewChildRef yet, so note to
   patch these two values later: 
            ; LEstNodeCt 
                := EstUtil . EstNodeCt ( MteTeLMNewChildRef ) 
                   + ORD ( EstHs . EstChildKindOptSingletonList 
                           IN MteTeLMNewKindSet 
                         ) 
            ; LMarkEstRef := MteTeLMNewChildRef
*)
            ; LEstNodeCt := - 1
            ; LMarkEstRef := NIL 
            ; MteTeDoPatchLeftSibFmtNoMark := TRUE 
            ; MteTeSibFmtNoMarkExists := TRUE 
            ELSIF 0 <= MteTeEstTravInfo . EtiChildNo  
                  AND MteTeEstTravInfo . EtiChildNo 
                      < MteTeEstTravInfo . EtiChildCt 
            THEN (* There will be an Est child to the left only. *) 
(* CHECK: Is this really true? *) 
              LMarkKind := MarkKindTyp . RightSibFmtNo 
            ; LEstNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
              (* ^Tricky.  This will also be the correct relative node no in 
                 the yet-to-be-constructed new Est node. 
              *)
            ; LEstNodeCt 
                := EstUtil . EstNodeCt
                     ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef ) 
              (* ^This is tricky too.  This child will be rightmost in the 
                  yet-to-be-constructed new EstNode. *)
(* REVIEW this. *) 
            ; LMarkEstRef := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef
            ; MteTeSibFmtNoMarkExists := TRUE 
            ELSE 
              LMarkKind := MarkKindTyp . ChildFmtNo 
            ; LEstNodeNo := 0 
            ; LEstNodeCt := 1 
            ; LMarkEstRef := MteTeEstTravInfo . EtiNodeRef 
(* REVIEW this. ^ The old node seems doubtful. *) 
            END (* IF *) 
          ; NewBolTokMark 
              := Marks . TokMarkTyp 
                   { EstNodeNo := LEstNodeNo 
                   , EstNodeCt := LEstNodeCt
                   , TkmEstRef := LMarkEstRef 
                   (* ^May be overlaid. *) 
                   , Kind := LMarkKind
                   , FmtNo := FsNodeRef . FsFmtNo 
                   , StartAtEnd := FALSE 
                   , IsImpliedNewLine := FALSE 
                   , TmTok := MteTeEstTravInfo . EtiParentRef . EstTok 
(* TODO: ^This probably needs a case for OptSingletonList, but that field
          is not being used anyway.
*) 
                   } 
          ; INC ( MteMarkCt ) 
       (* ; MteState := MteStateTyp . MteStateDone *) 
          END MteTeTfsBuildFmtNoMark 

      (* Format Syntax Trees: *) 

      ; PROCEDURE MteTeTfsBegOfImage ( ) RAISES { Backout } 

        = BEGIN (* MteTeTfsBegOfImage *) 
            IF MteState IN MteStateSetStart
               AND StartTokMark . Kind IN Marks . MarkKindSetEstLeaf 
            THEN (* Ignore the BOI and move on to its trailing mods. *) 
            ELSE 
              CASE MteState 
              OF MteStateTyp . MteStateStartAtBeg 
              => Assert 
                ( MteTeEstTravInfo . EtiChildNo = 0 
                , AFT . A_MteTeTfsBegOfImageFwdBadChildNo 
                ) 
              ; MteFinishItemNo := 1 
              ; INC ( MteItemCt ) 
              ; MteTeIsFirstLine := TRUE 
              ; MteTeIndentPos := EstIndentPos1 
              ; MteState := MteStateTyp . MteStatePassingNl 
              ; MteFwdPrevItemCouldHaveCondNlAfter := FALSE 
                (* ^Doubtless dead. *) 

              | MteStateTyp . MteStateBwdEdit 
              , MteStateTyp . MteStateBwdNl 
              , MteStateTyp . MteStateDone  
              => Assert 
                ( MteTeEstTravInfo . EtiChildNo = - 1  
                , AFT . A_MteTeTfsBegOfImageBwdBadChildNo 
                ) 
              ; DEC ( MteItemCt ) 
              ; Assert ( MteItemCt <= 0 , AFT . A_MteTeTfsBegOfImageNotDone ) 
              ; Assert 
                  ( EstAbsNodeNo = 0 
                  , AFT . A_MteTeTfsBegOfImageNotZeroNodeNo 
                  ) 
              ; MteTeMaybeFinishBwdEdit 
                  ( FsNodeRef . FsFmtNo 
                  , MarkNodeNo := MteTeRightChildRelNodeNo
                  ) 
              ; IF MteState # MteStateTyp . MteStateDone 
                THEN
                  MteTeTfsBuildFmtNoMark ( )
                ; MteState := MteStateTyp . MteStateDone
                END 
              ELSE 
                CantHappen ( AFT . A_MteTeTfsBegOfImageBadState ) 
              END (* CASE *) 
            END 
          END MteTeTfsBegOfImage 

      ; PROCEDURE MteTeTfsEndOfImage ( ) RAISES { Backout } 
        (* PRE: Leading mods already done and not deleted. *) 
        (* Text edits can lie beyond EOI. *) 

        = BEGIN (* MteTeTfsEndOfImage *) 
            Assert 
              ( MteTeEstTravInfo . EtiChildNo = MteTeEstTravInfo . EtiChildCt 
              , AFT . A_MteTeTfsEndOfImageChildrenLeft 
              ) 
          ; CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            => MteFinishItemNo := 0 
            ; MteBlankLinesBefore := BlankLineNo  
            ; MteBlankLinesAfter := 0 
            ; INC ( MteItemCt ) 
            ; MteTeFwdBlanks 
                ( BlanksToPos := DelFromPos 
                , ExtendTouchedRightward := FALSE 
                ) 
            ; MteTeFwdBlanks 
                ( BlanksToPos := DelToPos 
                , ExtendTouchedRightward := FALSE 
                ) 
(* TODO: ^Looks like this and its counterpart 1st MteTeBwdBlanks ( ) below
         duplicate work done inside MteTeReverse.  Try eliminating them
         from here.
*)  
            ; MteTeReverse ( ) 
            ; MteTeMaybeFinishBwdEdit 
                ( FmtNo := FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                ) 
            ; MteTeBwdBlanks ( ) 
            ; MteTeBwdBlanks ( ) 
            ; DEC ( MteItemCt ) 
            ; Assert ( MteItemCt <= 0 , AFT . A_MteTeTfsEndOfImageNotDone ) 
            ; MteTeMaybeFinishBwdEdit 
                ( FmtNo := FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                ) 
            ; Assert
                ( MteState = MteStateTyp . MteStateDone 
                , AFT . A_MteTeTfsEndOfImageNotDone  
                ) 
            ; MteTeDecEstChild ( ) 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , EstAbsNodeNo + MteTeEstTravInfo . EtiChildRelNodeNo 
                       ) 

            | MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateRightNlFound 
            => Assert 
                 ( MteState 
                   = MteStateTyp . MteStatePassingNl 
                   <= (* IMPLIES *) MtePassedDelNl 
                 , AFT . A_MteTeTfsEndOfImagePassingStartingNl 
                 ) 
            ; MteTeReverse ( ) 
            ; MteTeDecEstChild ( )  
            ; MteTeMaybeFinishBwdEdit 
                ( FmtNo := FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
                ) 

            ELSE 
              CantHappen ( AFT . A_MteTeTfsEndOfImageBadState ) 
            END (* CASE *) 
          END MteTeTfsEndOfImage 

      ; PROCEDURE MteTeTfsInsTok ( IsRepair : BOOLEAN ) 
        RAISES { Backout } 
        (* PRE: Leading mods already done;  The InsTok is not deleted. *) 

        = VAR LDoExcludeIfBwd : BOOLEAN 

        ; BEGIN (* MteTeTfsInsTok *)
            CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg  
            , MteStateTyp . MteStateStartAtEnd   
            => (* Ignore the InsTok and move on to its trailing mods. *) 
               Assert
                 ( StartTokMark . Kind IN Marks . MarkKindSetEstLeaf 
                 , AFT . A_MteTeTfsInsTok_StartAtInsTok 
                 ) 

            | MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStatePassingNl 
            => MteTeTok 
                 ( FsNodeRef . FsTok 
                 , FsNodeRef . FsInsTokRef 
                 , FsNodeRef 
                 , FsFmtKind 
                 , (* VAR *) DoExcludeIfBwd := LDoExcludeIfBwd 
                 ) 
            ; MteState := MteStateTyp . MteStateFwd 
            ; MteFwdPrevItemCouldHaveCondNlAfter := FALSE 

            | MteStateTyp . MteStateRightNlFound 
            => MteTeWaitChildrenToRight ( ) 
            ; MteTeReverse ( ) 
            ; MteTeDecEstChild ( ) 
            ; MteTeInterestingChildrenExist := TRUE 
            ; MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeRightChildRelNodeNo 
                ) 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
         (* , MteStateTyp . MteStateDone This shouldn't happen. *)  
            => MteTeTok 
                 ( FsNodeRef . FsTok 
                 , FsNodeRef . FsInsTokRef 
                 , FsNodeRef 
                 , FsFmtKind 
                 , (* VAR *) DoExcludeIfBwd := LDoExcludeIfBwd 
                 ) 
            ; IF LDoExcludeIfBwd 
              THEN 
                MteTeIncludeInModDel 
                  ( FsNodeRef . FsFmtNo , IsNotRepair := TRUE ) 
                (* ^Delete the insertion tok from new Est. It has been
                   touched, so it no longer matters if it was a repair. *) 
              ELSIF IsRepair 
              THEN 
                MteTeIncludeInModDel 
                  ( FsNodeRef . FsFmtNo , IsRepair := TRUE ) 
              ; MteTeInterestingChildrenExist := TRUE  
              ELSE (* Ensure this insertion tok is in new Est. *) 
                MteTeFlushDels ( ) 
              ; MteTeInterestingChildrenExist := TRUE 
              END (* IF *) 
            ; MteTeMaybeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := MteTeRightChildRelNodeNo 
                ) 
            ; IF MteCharPos < MteTouchedToPos 
              THEN 
                MteTeParentIsTouched := TRUE 
              END 

            ELSE 
              CantHappen ( AFT . A_MteTeTfsInsTokBadMteState ) 
            END (* CASE MteState *) 
          END MteTeTfsInsTok 

      ; PROCEDURE MteTeTfsLineBreak ( FsKind : FsKindTyp) 
        RAISES { Backout } 
        (* PRE: Leading mods already done; Line break is not deleted. *) 

        = BEGIN (* MteTeTfsLineBreak *) 
            IF MteState IN MteStateSetStart
               AND StartTokMark . Kind IN Marks . MarkKindSetEstLeaf 
            THEN (* Ignore the LineBreak and move on to its trailing mods,
                    staying in the start state. *) 
            ELSE 
              CASE MteState 
              OF MteStateTyp . MteStateStartAtBeg 
              => MteFinishItemNo := 1 
              ; INC ( MteItemCt ) 
              ; MteTeIsFirstLine := FALSE 
              ; MteTeIndentPos := EstIndentPosN
              ; MteState := MteStateTyp . MteStatePassingNl 

              | MteStateTyp . MteStatePassingNl 
              => INC ( MteItemCt ) 

              | MteStateTyp . MteStateFwd 
              , MteStateTyp . MteStateRightNlFound 
              => Assert 
                   ( MteFwdPrevTok # LbeStd . Tok__BegOfLine 
                   , AFT . A_MteTeTfsLineBreakFwdAtBOL 
                   ) 
              ; IF FsKind = FsKindTyp. FsKindLineBreakReqd 
                   OR TravUtil . DoTakeLineBreak 
                        ( Lang 
                        , MteCharPos - MteLineShift 
(* TODO: ^Something about the saturation arithmetic here. *) 
                        , MteModTextIsToLeftOnLine 
                        , MteFwdPrevTok 
                        , RootFsNodeRef 
                        , FsNodeRef  
                        , FsFmtKind 
                        , CurrentLineIndentPos := MteTeIndentPos 
                        , EstTravInfo := MteTeEstTravInfo 
                        , (* IN OUT *) LastFmtNoOnLine := MteLastFmtNoOnLine 
                        , (* IN OUT *) EstListChildrenToPass 
                             := MteEstListChildrenToPass 
                        ) 
                THEN (* Take the line break. *) 
                  IF MteTeReverseOrPassNlBefore ( ) (* Has SIDE EFFECTS. *) 
                  THEN (* We reversed *) 
                    MteTeInterestingChildrenExist 
                      := MteTeInterestingChildrenExist 
                         OR FsNodeRef . FsDeletableItemsAreToRight  
                  ; MteTeMaybeFinishBwdEdit
                      ( FmtNo := FsNodeRef . FsFmtNo 
                      , MarkNodeNo := MteTeRightChildRelNodeNo 
                      ) 
                  ELSE (* LB taken, but didn't reverse.  Pass it forwards. *) 
                    INC ( MteItemCt ) 
                  END (* IF *) 
                ELSE (* Line break not taken, forwards. *) 
                  INC ( MteItemCt ) 
                END (* IF *) 
              ; MteTeIsFirstLine := FALSE 
              ; MteTeIndentPos := EstIndentPosN

              | MteStateTyp . MteStateBwdEdit 
              , MteStateTyp . MteStateBwdNl 
              , MteStateTyp . MteStateDone  
              => DEC ( MteItemCt ) 
              ; IF MteItemCt <= 0 
                THEN (* This was the starting Nl. *) 
                  Assert
                    ( MteState IN MteStateSetBwdFinished  
                    , AFT . A_MteTeTfsLineBreakBadState  
                    ) 
                ; IF MteState # MteStateTyp . MteStateDone  
                  THEN  
                    MteTeTfsBuildFmtNoMark ( ) 
                  ; MteState := MteStateTyp . MteStateDone 
                  END
                ELSE
                  Assert 
                    ( MteState # MteStateTyp . MteStateDone 
                    , AFT . A_MteTeTfsLineBreak_DoneBeforeStartingNl  
                    ) 
                ; IF MteItemCt = MteNlItemNo 
                  THEN (* This is the Line break we passed. *) 
                    MteTeIncludeInModDel ( FsNodeRef . FsFmtNo )
                  ; MteLineShift := 0 
                  ELSIF FALSE AND MteState IN MteStateSetBwdFinished  
                  THEN (* Leave it alone, because whatever caused it not to be
                          taken will happen in the new tree too. *) 
                       (* This is not necessarily so. If we have Fill FmtKind,
                          DoTakeLineBreak will traverse past the new ModText
                          now and will have no idea how things were before.
                          So (at least in Fill), delete all line breaks on the
                          line to the left of the new ModText too. *) 
                    MteTeFlushDels ( ) (* Ensure it is in new Est. *) 
                  ELSE (* We are in or right of touched region, and the line
                          break was not taken. *) 
                    IF TRUE OR MteTeContainsFill 
(* FIX: ^This is a workaround for log 8, 181).  The method of computing
        FmtKind for trees we descend into is still not right. If it can
        be computed right, then deleting all these line breaks should
        not be necessary. 
   2005-11-28.  Still gives several failures when removed. 
*)
                    THEN 
                      MteTeIncludeInModDel ( FsNodeRef . FsFmtNo ) 
                    ELSE 
                      MteTeFlushDels ( ) (* Ensure it is in new Est. *) 
                    END (* IF *) 
                  END (* IF *) 
                END (* IF *) 
              ELSE 
                CantHappen ( AFT . A_MteTeTfsLineBreak_BadState ) 
              END (* CASE *) 
            END (* IF *) 
          END MteTeTfsLineBreak 

      ; PROCEDURE MteTeTfsEstChild ( IsEstList : BOOLEAN ) 
        RAISES { Backout , Thread . Alerted } 
        (* Only for an Fs-prescribed Est child. *) 

        = VAR LLMCharPos : LbeStd . LimitedCharNoTyp 

        ; BEGIN (* MteTeTfsEstChild *) 
            CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 
            => IF EstAbsNodeNo 
                  + ORD ( MteTeEstTravInfo . EtiIsOptSingletonList )  
                  + EstUtil . EstNodeCt ( MteTeEstTravInfo . EtiNodeRef ) 
                  <= StartTokMark . EstNodeNo 
              THEN (* Marked node is outside this Est subtree. *) 
                MteTeIncEstChild ( ) 
              ELSE
                Assert 
                  ( MteTeEstTravInfo . EtiChildNo 
                    < MteTeEstTravInfo . EtiChildCt 
                    AND MteTeEstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
                  , AFT . A_MteTeTfsEstChild_StartInsideMissingEstChild  
                  ) 
              ; IF EstHs . EstChildKindEstChild 
                   IN MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                THEN 
                  IF ISTYPE 
                       ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       , ModHs . EstDummyTyp 
                       ) (* Including NIL. *) 
                  THEN 
                    MteTeIncEstChild ( ) 
                  ELSIF EstHs . EstChildKindTrailingSep 
                        IN MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                  THEN (* Skip trailing sep. *) 
                    MteTeIncEstChild ( ) 
                  ELSE 
                    MteTeTfsEstSubtree 
                      ( IsModTok := FALSE  
                      , (* VAR *) LMCharPos := LLMCharPos (* Dead. *) 
                      ) 
                  END (* IF *) 
                END (* IF *) 
              END (* IF *) 
            ; IF IsEstList 
              THEN
                TravUtil . PassEstListChild ( MteEstListChildrenToPass ) 
              END (* IF *) 

            | MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateRightNlFound 
            => TravUtil . AssertFwdNoLostFixedChild 
                 ( FsNodeRef , MteTeEstTravInfo ) 
            ; IF MteTeEstTravInfo . EtiChildNo 
                  < MteTeEstTravInfo . EtiChildCt 
                  AND MteTeEstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
                  AND EstHs . EstChildKindEstChild 
                      IN MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
              THEN (* An Est child exists. *) 
                IF ISTYPE 
                      ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                      , ModHs . EstDummyTyp (* Including NIL. *) 
                      ) 
                THEN (* NIL or dummy.  Skip. *)  
                  MteTeIncEstChild ( ) 
                ELSIF EstHs . EstChildKindTrailingSep 
                      IN MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                THEN (* Skip trailing sep. *) 
                  MteTeIncEstChild ( ) 
                ELSE 
                  MteTeTfsEstSubtree 
                    ( IsModTok := FALSE 
                    , (* VAR *) LMCharPos := LLMCharPos 
                    )  
                END (* IF *) 
              ; IF IsEstList 
                THEN
                  TravUtil . PassEstListChild ( MteEstListChildrenToPass ) 
                END (* IF *) 
              END (* IF *) 
            ; IF MteTeCfInsTokFromPos # LbeStd . LimitedCharNoInfinity 
              THEN
                MteTeCfEstChildFromPos := LLMCharPos 
              ; IF MteTeCfInsTokFromPos = LbeStd . LimitedCharNoUnknown 
                THEN 
                  MteTeCfInsTokFromPos := LMCharPos  
                END (* IF *) 
              ; MteTeCfEstChildToPos := MteCharPos 
              END (* IF *)  

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => TravUtil . AssertBwdNoLostFixedChild 
                 ( FsNodeRef , MteTeEstTravInfo ) 
            ; IF 0 > MteTeEstTravInfo . EtiChildNo 
                 OR MteTeEstTravInfo . EtiChildFmtNo # FsNodeRef . FsFmtNo 
                 OR NOT EstHs . EstChildKindEstChild 
                        IN MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
              THEN (* No Est child exists. *) 
              ELSIF ISTYPE 
                      ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                      , ModHs . EstDummyTyp 
                      ) (* Including NIL. *) 
                    OR EstHs . EstChildKindTrailingSep 
                       IN MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
              THEN (* NIL, dummy Est child, or trailing sep. *) 
                MteTeWaitExistingEstChild 
                  ( EstHs . EdgeKindTyp . EdgeKindEstChild 
                  , IsInteresting := FALSE 
                  ) 
              ; IF MteCharPos < MteTouchedToPos 
                THEN 
                  MaxTouchedNodeNo 
                    := MAX ( MaxTouchedNodeNo 
                           , EstAbsNodeNo
                             + MteTeEstTravInfo . EtiChildRelNodeNo 
                           ) 
                END 
              ; MteTeDecEstChild ( ) 
              ELSE 
                MteTeTfsEstSubtree 
                  ( IsModTok := FALSE  
                  , (* VAR *) LMCharPos := LLMCharPos (* Dead. *) 
                  ) 
              END (* IF *) 
            END (* CASE *) 
          END MteTeTfsEstChild 

      ; PROCEDURE MteTeTfsFsSubtree ( ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LChildFmtKind : LangUtil . FmtKindTyp 
        ; VAR LFsChildCt : LangUtil . FsChildNoTyp 
        ; VAR LFsChildNo : LangUtil . FsChildNoSignedTyp 

        ; BEGIN (* MteTeTfsFsSubtree *) 
            IF FsNodeRef . FsChildren = NIL 
            THEN LFsChildCt := 0 
            ELSE LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ )  
            END (* IF *) 
          ; CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 
            => LChildFmtKind 
                := TravUtil . FmtKindForFsSubtreeDescending
                     ( Lang 
                     , RootFsNodeRef 
                     , FsNodeRef 
                     , FsFmtKind 
                     , EstIndentPosN 
                     , MteTeEstTravInfo 
                     , StartMark := StartTokMark 
                     , StartMarkIsKnownNl := TRUE 
                     )
            ; LFsChildNo 
                := LangUtil . FsChildNoOfFmtNo 
                     ( FsNodeRef , MteTeStartFmtNo ) 

            | MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStateRightNlFound 
            => LChildFmtKind 
                := TravUtil . FmtKindForFsSubtreeTraversing 
                     ( Lang 
                     , MteCharPos - MteLineShift 
(* TODO: ^Saturation arithmetic. *)  
                     , MteModTextIsToLeftOnLine 
                     , MteFwdPrevTok 
                     , RootFsNodeRef 
                     , FsNodeRef 
                     , FsFmtKind 
                     , MteTeIndentPos 
                     , MteTeEstTravInfo 
                     , (* IN OUT *) MteLastFmtNoOnLine 
                     , (* IN OUT *) MteEstListChildrenToPass 
                     )
            ; LFsChildNo := 0 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => LChildFmtKind := LangUtil . FmtKindTyp . FmtKindUnknown 
            ; LFsChildNo := LFsChildCt - 1 
            END (* CASE *) 
          ; MteTeTraverseFsFixedChildren 
              ( FsNodeRef , LChildFmtKind , LFsChildCt , LFsChildNo ) 
          END MteTeTfsFsSubtree 

      ; PROCEDURE MteTeTfsTraverseCondFmtChildren ( ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LFsChildCt : LangUtil . FsChildNoTyp 
        ; VAR LFsChildNo : LangUtil . FsChildNoTyp 
        ; VAR LFsChildNoSigned : LangUtil . FsChildNoSignedTyp 
        ; VAR LPredicate : BOOLEAN 

        ; BEGIN (* MteTeTfsTraverseCondFmtChildren *) 
            LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ )  
          ; CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 

            => TravUtil . DescendCondFmt 
                 ( Lang 
                 , FsNodeRef := FsNodeRef 
                 , FmtNo := MteTeStartFmtNo 
                 , EstTravInfo := MteTeEstTravInfo 
                 , (* VAR *) Predicate := LPredicate 
                 , (* VAR *) FsChildNo := LFsChildNo 
                 ) 
            ; LFsChildNoSigned := LFsChildNo 
            ; IF LPredicate 
              THEN
                MteTeFlushCondFmt ( ) 
              ; MteTeCfInsTokFromPos := LbeStd . LimitedCharNoUnknown 
              END (* IF *) 

            | MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStateRightNlFound 
            => LFsChildNoSigned := 0 
            ; LPredicate 
                 := TravUtil . DoCondFmtFwd ( Lang , MteTeEstTravInfo , FsNodeRef ) 
            ; IF LPredicate 
              THEN
                MteTeFlushCondFmt ( ) 
              ; MteTeCfInsTokFromPos := LbeStd . LimitedCharNoUnknown 
              END (* IF *) 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => LFsChildNoSigned := LFsChildCt - 1 
            ; LPredicate 
               := TravUtil . DoCondFmtBwd 
                    ( Lang , MteTeEstTravInfo , FsNodeRef ) 
            END (* CASE *) 
          ; IF LPredicate 
            THEN 
              MteTeTraverseFsFixedChildren 
                ( FsNodeRef , FsFmtKind , LFsChildCt , LFsChildNoSigned ) 
            ; MteTeCfInsTokToPos := MteCharPos 
            ELSE 
              MteTeTraverseFs ( FsNodeRef . FsCondAltRef , FsFmtKind ) 
            END (* IF *) 
          END MteTeTfsTraverseCondFmtChildren 

      ; TYPE WhatNextTyp = { LeadingMods , FsItem , TrailingMods } 

      ; PROCEDURE MteTeTfsWhatNextForDescend 
          ( EstAbsNodeNo : LbeStd . EstNodeNoTyp 
          ; READONLY EstTravInfo : TravUtil . EstTravInfoTyp 
          ; StartTokMark : Marks . TokMarkTyp 
          ) 
        : WhatNextTyp 

        = BEGIN   
            IF EstTravInfo . EtiChildNo >= EstTravInfo . EtiChildCt 
               OR EstTravInfo . EtiChildNo < 0 
            THEN RETURN WhatNextTyp . FsItem 
            ELSE 
              TYPECASE EstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => RETURN WhatNextTyp . FsItem 

              | EstHs . EstRefTyp ( TEstRef ) 
              => IF TEstRef . EstNodeKind 
                    = EstHs . EstNodeKindTyp . EstNodeKindModTok 
                 THEN 
                   CASE StartTokMark . Kind <* NOWARN *> 
                   OF MarkKindTyp . LeftSibFmtNo 
                   , MarkKindTyp . RightSibFmtNo 
                   => IF StartTokMark . EstNodeNo 
                         > EstAbsNodeNo + EstTravInfo . EtiChildRelNodeNo 
                      THEN RETURN WhatNextTyp . LeadingMods 
                      ELSE RETURN WhatNextTyp . FsItem 
                      END (* IF *) 

                   | MarkKindTyp . ChildFmtNo 
                   , MarkKindTyp . Plain 
                   , MarkKindTyp . BlankLine 
                   => IF StartTokMark . EstNodeNo 
                         >= EstAbsNodeNo + EstTravInfo . EtiChildRelNodeNo 
                      THEN RETURN WhatNextTyp . LeadingMods 
                      ELSE RETURN WhatNextTyp . FsItem 
                      END (* IF *) 
                   END (* CASE *) 
                ELSE RETURN WhatNextTyp . FsItem 
                END (* IF  *) 

              | ModHs . ModCmntTrailingTyp 
              => CASE StartTokMark . Kind <* NOWARN *> 
                 OF MarkKindTyp . LeftSibFmtNo 
                 , MarkKindTyp . RightSibFmtNo 
                 => RETURN WhatNextTyp . FsItem 

                 | MarkKindTyp . Plain 
                 => RETURN WhatNextTyp . TrailingMods 
                 END (* CASE *) 

              | ModHs . ModRefTyp (* Any leading Mod. *)  
              => CASE StartTokMark . Kind <* NOWARN *> 
                 OF MarkKindTyp . LeftSibFmtNo 
                 , MarkKindTyp . RightSibFmtNo 
                 => RETURN WhatNextTyp . FsItem 

                 | MarkKindTyp . Plain 
                 , MarkKindTyp . BlankLine 
                 => RETURN WhatNextTyp . LeadingMods 
                 END (* CASE *) 

              | SharedStrings . T ( TString ) 
              => IF SharedStrings . Tok ( TString ) 
                    = LbeStd . Tok__LexErrChars 
                 THEN RETURN WhatNextTyp . LeadingMods  
                 ELSE RETURN WhatNextTyp . FsItem 
                 END (* IF *) 

              ELSE RETURN WhatNextTyp . FsItem 
              END (* TYPECASE *) 
            END (* IF *) 
          END MteTeTfsWhatNextForDescend 

      ; BEGIN (* MteTeTraverseFs *) 
          VAR LWhatNext : WhatNextTyp 
        ; VAR LDelete : BOOLEAN := FALSE 
        ; VAR LIsRepair : BOOLEAN := FALSE 

        ; BEGIN (* Block for MteTeTraverseFs*) 
            CASE FsNodeRef . FsKind <* NOWARN *>

            (* FsKind cases which can have leading and/or trailing mods. *) 
            OF FsKindTyp. FsKindBegOfImage 
            , FsKindTyp. FsKindEndOfImage 
            , FsKindTyp. FsKindInsTok 
            , FsKindTyp. FsKindEstChildOfFixed 
            , FsKindTyp. FsKindEstChildOfList 
            , FsKindTyp. FsKindLineBreakOpt 
            , FsKindTyp. FsKindLineBreakReqd 
            => CASE MteState 
               OF MteStateTyp . MteStateStartAtBeg 
               , MteStateTyp . MteStateStartAtEnd  
               => LWhatNext 
                    := MteTeTfsWhatNextForDescend 
                         ( EstAbsNodeNo , MteTeEstTravInfo , StartTokMark ) 
               ; LDelete := FALSE 
               ; LIsRepair := FALSE 

               | MteStateTyp . MteStateFwd 
               , MteStateTyp . MteStatePassingNl 
               , MteStateTyp . MteStateRightNlFound 
               => LWhatNext := WhatNextTyp . LeadingMods 

               | MteStateTyp . MteStateBwdEdit 
               , MteStateTyp . MteStateBwdNl 
               , MteStateTyp . MteStateDone  
               => LWhatNext := WhatNextTyp . TrailingMods 
               END (* CASE *) 
            ; LOOP (* While sashaying between leading mods, the real Fs item, 
                      and trailing mods, in alternating directions. *) 
                CASE LWhatNext 
                OF WhatNextTyp . LeadingMods 
                => (* Leading mods, in whichever direction, are next. *) 
                   MteTeTfsLeadingModsNoDel ( ) 
                ; CASE MteState 
                  OF MteStateTyp . MteStateStartAtBeg 
                  , MteStateTyp . MteStateStartAtEnd  
                  , MteStateTyp . MteStateFwd 
                  , MteStateTyp . MteStatePassingNl 
                  , MteStateTyp . MteStateRightNlFound 
                  => MteTeTfsFwdLeadingDel 
                       ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
                  ; LWhatNext := WhatNextTyp . FsItem 
                  | MteStateTyp . MteStateBwdEdit 
                  , MteStateTyp . MteStateBwdNl 
                  , MteStateTyp . MteStateDone 
                  => EXIT 
                  END (* CASE *) 

                | WhatNextTyp . FsItem 
                (* The format syntax item, in whichever direction, is next. *) 
                (* INVARIANT: LDelete and LIsRepair are properly set. *) 
                => IF LDelete 
                      AND ( NOT LIsRepair 
                            OR FsNodeRef . FsKind 
                               IN LangUtil . FsKindSetLineBreak 
                          ) 
                   THEN (* The item is deleted. *)  
                     Assert 
                       ( FsNodeRef . FsKind IN LangUtil . FsKindSetDeletable 
                       , AFT . A_MteTeTraverseFsDeleteBadFsKind 
                       ) 
                   ; CASE MteState 
                     OF MteStateTyp . MteStateStartAtBeg 
                     , MteStateTyp . MteStateStartAtEnd 
                     , MteStateTyp . MteStateFwd 
                     , MteStateTyp . MteStatePassingNl 
                     , MteStateTyp . MteStateRightNlFound 
                     => LWhatNext := WhatNextTyp . TrailingMods 

                     | MteStateTyp . MteStateBwdEdit 
                     , MteStateTyp . MteStateBwdNl 
                     , MteStateTyp . MteStateDone    
                     => MteTeIncludeInModDel 
                          ( FsNodeRef . FsFmtNo , IsNotRepair := TRUE ) 
                        (* ^Delete the item from the new tree too. *) 
                     ; LWhatNext := WhatNextTyp . LeadingMods 
                     END (* CASE *) 
                   ELSE (* NOT LDelete OR LIsRepair. Treat FS item as present. *) 
                     CASE FsNodeRef . FsKind 
                     (* Beginning of Image. *) 
                     OF FsKindTyp. FsKindBegOfImage 
                     => MteTeTfsBegOfImage ( ) 
                     (* End of Image. *) 
                     | FsKindTyp. FsKindEndOfImage 
                     => MteTeTfsEndOfImage ( ) 
                     (* InsTok. *) 
                     | FsKindTyp. FsKindInsTok 
                     => MteTeTfsInsTok ( LIsRepair ) 
                     (* Line breaks. *) 
                     | FsKindTyp. FsKindLineBreakOpt 
                     , FsKindTyp. FsKindLineBreakReqd  
                     => MteTeTfsLineBreak ( FsNodeRef . FsKind ) 
                     (* Est child *) 
                     | FsKindTyp. FsKindEstChildOfFixed 
                     => MteTeTfsEstChild ( IsEstList := FALSE ) 
                     | FsKindTyp. FsKindEstChildOfList 
                     => MteTeTfsEstChild ( IsEstList := TRUE ) 
                     ELSE 
                       CantHappen 
                         ( AFT . A_MteTeTraverseFsFsItem_BadDeletedFsKind ) 
                     END (* CASE FsKind *) 
                   ; CASE MteState 
                     OF MteStateTyp . MteStateStartAtBeg 
                     , MteStateTyp . MteStateStartAtEnd  
                     , MteStateTyp . MteStatePassingNl 
                     , MteStateTyp . MteStateFwd 
                     , MteStateTyp . MteStateRightNlFound 
                     => LWhatNext := WhatNextTyp . TrailingMods 
                     | MteStateTyp . MteStateBwdEdit 
                     , MteStateTyp . MteStateBwdNl 
                     , MteStateTyp . MteStateDone 
                     => IF MteItemCt <= 0 
                        THEN EXIT 
                        ELSE LWhatNext := WhatNextTyp . LeadingMods 
                        END 
                     END (* CASE MteState *) 
                   END (* IF LDelete *) 

                | WhatNextTyp . TrailingMods 
                => MteTeTfsTrailingMods ( ) 
                ; CASE MteState 
                  OF MteStateTyp . MteStateStartAtBeg 
                  , MteStateTyp . MteStateStartAtEnd 
                  , MteStateTyp . MteStatePassingNl 
                  , MteStateTyp . MteStateFwd 
                  , MteStateTyp . MteStateRightNlFound 
                  => EXIT 
                  | MteStateTyp . MteStateBwdEdit 
                  , MteStateTyp . MteStateBwdNl 
                  , MteStateTyp . MteStateDone 
                  => IF MteItemCt <= 0 
                     THEN EXIT 
                     ELSE 
                       MteTeTfsBwdLeadingDel 
                          ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
                     ; LWhatNext := WhatNextTyp . FsItem 
                     END 
                  ELSE 
                    CantHappen ( AFT . A_MteTeTraverseFsFsItemBadMteState ) 
                  END (* CASE MteState *) 
                END (* CASE LWhatNext *) 
              END (* LOOP *) 

            (* Fs cases which do not have leading or trailing mods: *) 

            (* Subtree nodes. *) 
            | FsKindTyp. FsKindSubtreeVert 
            , FsKindTyp. FsKindSubtreeHoriz 
            => MaxTouchedNodeNo := MAX ( MaxTouchedNodeNo , EstAbsNodeNo ) 
            ; MteTeTfsFsSubtree ( ) 

            | FsKindTyp. FsKindSubtreeFill 
            => MaxTouchedNodeNo := MAX ( MaxTouchedNodeNo , EstAbsNodeNo ) 
            ; MteTeContainsFill := TRUE 
(* FIX: ^This isn't really right.  It will leave MteTeContainsFill TRUE
         if we go up a level in the fs tree, to a place that is not
         filled.  This is overly conservative, i.e. it will delete
         line breaks in a place where no fill computation will take
         place. *) 
            ; MteTeTfsFsSubtree ( ) 

            (* Conditional format. *) 
            | FsKindTyp. FsKindCondFmt 
            => MteTeTfsTraverseCondFmtChildren ( ) 

            END (* CASE *) 
          END (* Block *) 
        END MteTeTraverseFs 

    ; BEGIN (* MteTraverseEst *) 
        VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LInitialFsChildNo : LangUtil . FsChildNoSignedTyp 

      ; BEGIN (* Block for MteTraverseEst *) 
          IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        ; MteTeNewEstRef := EstRef (* The default.  May change. *)   
        ; NewSingletonOptKindSet := EstHs . EstChildKindSetEmpty (* Default. *) 
        ; LMCharPos := LbeStd . LimitedCharNoUnknown 
        ; IF EstRef # NIL 
          THEN 
            MteTeContainsFill
              := RootFsNodeRef . FsKind IN LangUtil . FsKindSetEstFill 
          ; MteTeInterestingChildrenExist := FALSE 
          ; MteTeNILChildrenExist := FALSE 
          ; MteTeSibFmtNoMarkExists := FALSE 
          ; MteTeDoPatchLeftSibFmtNoMark := FALSE 
          ; MteTeMergeIsStarted := FALSE 
          ; MteTeChildIsWaiting := FALSE 
          ; MteTeWaitingRef := NIL (* Defensive *)  
          ; MteTeWaitingFromChildNo := LbeStd . EstChildNoNull 
          ; MteTeWaitingToChildNo := LbeStd . EstChildNoNull (* Defensive. *)  
          ; MteTeWaitingFmtNo := EstHs . FmtNoNull 
          ; MteTeWaitingEdgeKind := EstHs . EdgeKindTyp . EdgeKindEstChild  
          ; MteTeRMChildRef := NIL 
          ; MteTeRMChildRelNodeNo := LbeStd . EstNodeNoNull 
          ; MteTeRightChildRelNodeNo := LbeStd . EstNodeNoNull
          ; MteTeRightChildRef := NIL 
          ; MteTeLMNewChildRef := NIL (* Dead. Defensive. *) 
          ; MteTeLMNewKindSet := EstHs . EstChildKindSetEmpty 
            (* Dead. Defensive. *)  
          ; MteTeCfInsTokFromPos := LbeStd . LimitedCharNoInfinity 
          ; MteTeParentIsTouched := FALSE 

          (* Choose the correct Est child. *) 
          ; CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 
            => IF EstAbsNodeNo = StartTokMark . EstNodeNo 
               THEN (* StartTokMark identifies this parent node. *)  
                 CASE StartTokMark . Kind <* NOWARN *> 
                 OF MarkKindTyp . Plain 
                 , MarkKindTyp . BlankLine 
                 => (* The mark denotes the Est root, which is a leaf. 
                       No need to set to a child. *) 
                    Assert 
                      ( MteTeEstTravInfo . EtiParentRef = NIL 
                      , AFT . A_MergeTxt_MergeTextEdit_NotLeaf 
                      ) 
                 ; TravUtil . InitEstTravInfo 
                     ( MteTeEstTravInfo , EstRef , EstAbsNodeNo ) 

                 | MarkKindTyp . ChildFmtNo 
                 => Assert
                      ( MteTeEstTravInfo . EtiChildCt = 0 
                      , AFT . A_MteTraverseEst_ChildFmtNo_Has_children
                      )  
                 ; TravUtil . InitEstTravInfoFwd 
                     ( MteTeEstTravInfo , EstRef , KindSet , EstAbsNodeNo ) 

                 | MarkKindTyp . LeftSibFmtNo 
                 , MarkKindTyp . RightSibFmtNo 
                 => CantHappen ( AFT . A_MteTraverseEst_SibFmtNo_on_parent ) 
                 END (* CASE  *) 
               ELSE 
                 TravUtil . InitToChildContainingNodeNo 
                   ( MteTeEstTravInfo 
                   , EstRef 
                   , StartTokMark . EstNodeNo - EstAbsNodeNo 
                   , KindSet
                   , EstAbsNodeNo 
                   ) 
               ; IF StartTokMark . Kind = MarkKindTyp . RightSibFmtNo 
                    AND EstAbsNodeNo + MteTeEstTravInfo . EtiChildRelNodeNo 
                        = StartTokMark . EstNodeNo 
                 THEN 
                   MteTeIncEstChild ( ) 
                 END (* IF *) 
               END (* IF *) 

            (* Choose the correct FmtNo. *) 
            ; MteTeStartFmtNo := MteTeEstTravInfo . EtiChildFmtNo (* Default. *)
            ; CASE StartTokMark . Kind 
              OF MarkKindTyp . ChildFmtNo 
              => IF EstAbsNodeNo = StartTokMark . EstNodeNo 
                 THEN MteTeStartFmtNo := StartTokMark . FmtNo 
                 END (* IF *) 

              | MarkKindTyp . LeftSibFmtNo 
              => IF EstAbsNodeNo + MteTeEstTravInfo . EtiChildRelNodeNo 
                    = StartTokMark . EstNodeNo 
                 THEN MteTeStartFmtNo := StartTokMark . FmtNo 
                 END (* IF *) 

              | MarkKindTyp . RightSibFmtNo 
              => IF MteTeEstTravInfo . EtiChildNo 
                    >= MteTeEstTravInfo . EtiChildCt 
                    AND EstAbsNodeNo + MteTeRMChildRelNodeNo 
                        = StartTokMark . EstNodeNo 
                 THEN MteTeStartFmtNo := StartTokMark . FmtNo 
                 END (* IF *) 
              ELSE 
              END (* CASE *) 

            | MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateRightNlFound 
            => TravUtil . InitEstTravInfoFwd 
                 ( MteTeEstTravInfo , EstRef , KindSet , EstAbsNodeNo ) 
            ; MteTeStartFmtNo := EstHs . FmtNoNull 
            ; MteTeIsFirstLine := TRUE  
            ; MteTeIndentPos := EstIndentPos1 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => TravUtil . InitEstTravInfoBwd 
                 ( MteTeEstTravInfo , EstRef , KindSet , EstAbsNodeNo ) 
            ; MteTeRMChildRef 
                := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
            ; MteTeRMChildRelNodeNo := MteTeEstTravInfo . EtiChildRelNodeNo 
            ; MteTeRightChildRelNodeNo := EstUtil . EstNodeCt ( EstRef )
            ; MteTeRightChildRef := NIL 
            ; MteTeStartFmtNo := EstHs . FmtNoNull 
            ELSE 
              CantHappen ( AFT . A_MteTraverseEstBadStateBefore ) 
            END (* CASE *) 
          ; IF RootFsNodeRef . FsChildren = NIL 
            THEN LFsChildCt := 0 
            ELSE LFsChildCt := NUMBER ( RootFsNodeRef . FsChildren ^ ) 
            END (* IF *) 
          ; IF RootFsNodeRef . FsKind 
               IN LangUtil . FsKindSetEstListTrail 
               AND MteTeEstTravInfo . EtiParentRef . EstNodeKind 
                   = EstHs . EstNodeKindTyp . EstNodeKindTrail
            THEN MteTeRMFsChildNo := LFsChildCt - 1 
            ELSE MteTeRMFsChildNo := 0 
            END (* IF *) 

          ; CASE RootFsNodeRef . FsKind <* NOWARN *>  

            (* Est fixed nodes. *) 
            OF FsKindTyp. FsKindEstFixedVert 
            , FsKindTyp. FsKindEstFixedHoriz 
            , FsKindTyp. FsKindEstFixedFill 
            => CASE MteState 
              OF MteStateTyp . MteStateStartAtBeg 
              , MteStateTyp . MteStateStartAtEnd 
              => LInitialFsChildNo 
                  := LangUtil . FsChildNoOfFmtNo 
                       ( RootFsNodeRef , MteTeStartFmtNo ) 
           (* ; MteTeSetIndentInfo ( RootFsNodeRef )  *) 

              | MteStateTyp . MteStateFwd 
              , MteStateTyp . MteStatePassingNl 
              , MteStateTyp . MteStateRightNlFound 
              => LInitialFsChildNo := 0 

              | MteStateTyp . MteStateBwdEdit 
              , MteStateTyp . MteStateBwdNl 
              , MteStateTyp . MteStateDone  
              => LInitialFsChildNo := LFsChildCt - 1 
              END (* CASE *) 
            ; MteTeTraverseFsFixedChildren 
                 ( RootFsNodeRef 
                 , EstFmtKind 
                 , LFsChildCt 
                 , LInitialFsChildNo 
                 ) 

            (* Est list nodes. *) 
            | FsKindTyp. FsKindEstListVert 
            , FsKindTyp. FsKindEstListHoriz 
            , FsKindTyp. FsKindEstListFill 
            , FsKindTyp . FsKindEstListTrailHoriz  
            , FsKindTyp . FsKindEstListTrailVert  
            , FsKindTyp . FsKindEstListTrailFill   
            => CASE MteState 
               OF MteStateTyp . MteStateStartAtBeg 
               , MteStateTyp . MteStateStartAtEnd 
               => LInitialFsChildNo 
                   := LangUtil . FsChildNoOfFmtNo 
                        ( RootFsNodeRef , MteTeStartFmtNo ) 

               | MteStateTyp . MteStateFwd 
               , MteStateTyp . MteStatePassingNl 
               , MteStateTyp . MteStateRightNlFound 
               => LInitialFsChildNo := 0 

               | MteStateTyp . MteStateBwdEdit 
               , MteStateTyp . MteStateBwdNl 
               , MteStateTyp . MteStateDone  
               => LInitialFsChildNo := MteTeRMFsChildNo  
               END (* CASE *) 
            ; MteTeTraverseFsListChildren 
                ( RootFsNodeRef 
                , EstFmtKind 
                , LFsChildCt 
                , LInitialFsChildNo 
                ) 

            (* Ast string, *) 
            | FsKindTyp. FsKindAstString 
            => MteTeAstString ( RootFsNodeRef , EstFmtKind ) 

         (* ELSE Cant happen. *) 
            END (* CASE *) 
          ; CASE MteState 
            OF MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateRightNlFound 
            => MteTeFlushCondFmt ( )  
            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone 
            => MteTeFinishMerge ( ) 
            ELSE 
              CantHappen ( AFT . A_MteTraverseEstBadStateAfter ) 
            END (* CASE MteState *) 
          END (* IF *) 
        ; IF MteTeParentIsTouched  
          THEN MaxTouchedNodeNo := MAX ( MaxTouchedNodeNo , EstAbsNodeNo ) 
          END 
        ; NewEstRef := MteTeNewEstRef 
        ; EVAL MteTeNewEstRef (* For breakpoint. *)  
        ; IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        END (* Block *) 
      END MteTraverseEst 

  ; PROCEDURE MteCheckTokMarks ( ) RAISES { Backout } 

    = VAR LEstRef : LbeStd . EstRootTyp 
    ; VAR LEstRef2 : LbeStd . EstRootTyp
    ; VAR LKindSet : EstHs . EstChildKindSetTyp  
    ; VAR LKindSet2 : EstHs . EstChildKindSetTyp  
    ; VAR LIsOptSingletonList : BOOLEAN 
    ; VAR LIsOptSingletonList2 : BOOLEAN 

    ; BEGIN (* MteCheckTokMarks *) 
        Assert 
          ( TRUE OR Marks . Equal ( NewBolTokMark , StartTokMark ) 
          , AFT . A_MteCheckTokMarksChanged1stTokMark 
          ) 
(* TODO: ^We cannot assert this in general.  An inserted (by MergeTextEdit) 
   Nl can be to the right of but adjacent to StartTokMark.  What we need is
   a mini-traverser that goes to the RM Nl of a group.  Apply this to
   both StartTokMark and NewBolTokMark, then assert the results are
   equal. *) 
      ; IF EndTokMark . EstNodeNo + 1 < EstUtil . EstNodeCt ( EstRootRef ) 
        THEN 
          TravUtil . GetDescendantWithNodeNo 
            ( EstRootRef 
            , EndTokMark . EstNodeNo + 1 
            , (* VAR *) LEstRef 
            , (* VAR *) LKindSet 
            , (* VAR *) LIsOptSingletonList 
            ) 
        ; TravUtil . GetDescendantWithNodeNo 
            ( NewEstRootRef 
            , EndTokMark . EstNodeNo + NodeNoChange + 1 
            , (* VAR *) LEstRef2 
            , (* VAR *) LKindSet2 
            , (* VAR *) LIsOptSingletonList2 
            ) 
        ; Assert 
            ( TRUE OR LEstRef = LEstRef2 
(* Nor can we assert this. Just adding to the node nos in marks doesn't
   take us to the right place e.g. a ChildFmtNo mark.  *) 
            , AFT . A_MteCheckTokMarks_MismatchedEndNodeNos 
            ) 
        END (* IF *) 
      END MteCheckTokMarks 

  ; BEGIN (* MergeTextEdit *) 

      VAR LLMCharPos : LbeStd . LimitedCharNoTyp 
    ; VAR LNewEstRootRef : LbeStd . EstRootTyp
    ; VAR LNewMarkedEstRef : LbeStd . EstRootTyp 
    ; VAR LNewSingletonOptKindSet : EstHs . EstChildKindSetTyp 
    ; VAR LKindSet : EstHs . EstChildKindSetTyp 
    ; VAR LIsOptSingletonList : BOOLEAN 

    ; BEGIN (* Block  MergeTextEdit body. *)
        IF DelFromPos = LbeStd . NodeNoNull 
           OR ( DelToPos = DelFromPos AND InsLen = 0 ) 
        THEN (* No changes. *)
          NewEstRootRef := EstRootRef
        ; NodeNoChange := 0
        ; MaxTouchedNodeNo := LbeStd . NodeNoNull
        ; NewBolTokMark := StartTokMark
        ; NewLinesCt := 0
        ; LeadingBlankLinesIncluded := 0 
        ; TrailingBlankLinesIncluded := 0
        ; RETURN 
        END (* IF *) 
      ; Assert 
          ( InsNlPos = LbeStd . LimitedCharNoInfinity 
            OR DelNlShift = LbeStd . LimitedCharNoInfinity 
          , AFT . A_MteBothInsAndDelNl 
          ) 
      ; Assert 
          ( InsNlPos = LbeStd . LimitedCharNoInfinity 
            OR DelFromPos <= InsNlPos AND InsNlPos <= DelFromPos + InsLen 
          , AFT . A_Mte_InsertedNewLineOutOfPlace 
          ) 
      ; MteImbeddedNlModRef := NIL 
      ; MteBlankSs := 0 
      ; MteTouchedFromPos := 0 
      ; MteTouchedToPos := LbeStd . LimitedCharNoInfinity 
      ; MteItemCt := 0 
      ; MteNlItemNo := LAST ( PortTypes . Card32Typ ) 
      ; MteCharPos := 0 
      ; MteLineShift := 0 
      ; MteSuffixShift := - ( DelToPos - DelFromPos ) + InsLen 
      ; MteModShift := MteSuffixShift 
      ; IF InsNlPos # LbeStd . LimitedCharNoInfinity 
        THEN 
          INC ( MteModShift , NlIndentPos - InsNlPos ) 
        ELSIF DelNlShift # LbeStd . LimitedCharNoInfinity 
        THEN 
          INC ( MteModShift , DelNlShift ) 
        END (* IF *) 
      ; MteFwdPrevTok := LbeStd . Tok__BegOfLine 
      ; MteLastFmtNoOnLine := EstHs . FmtNoUnknown  
      ; MteEstListChildrenToPass := 0 (* Dead *) 
      ; MteNewDelThruFmtNo := EstHs . FmtNoNull 
      ; MteNewDelFromFmtNo := EstHs . FmtNoNull (* Defensive *) 
      ; MteNewBlankLineFmtNo := EstHs . FmtNoNull 
      ; MteNewBlankLineCt := 0 (* Defensive. *) 
      ; MteModTextIsToLeftOnLine := FALSE 

      ; MteNewTextToPos := LbeStd . LimitedCharNoInfinity 
      ; MteFwdLeftTokToPos := 0 
      ; MteNewTextLeftTokToPos := 0 
      ; MtePassedDelNl := FALSE (* Dead *) 
      ; MteFwdPrevItemCouldHaveCondNlAfter := FALSE 
      ; MteBlankLinesBefore := 0 
      ; MteBlankLinesAfter := 0 
      ; MteStartingModBlankLineRef := NIL 
      ; MteNlChanged 
          := InsNlPos # LbeStd . LimitedCharNoInfinity 
             OR DelNlShift # LbeStd . LimitedCharNoInfinity 
      ; MteNlOrTransparencyChanged 
          := MteNlChanged 
             OR DelFromPos = DelToPos 
                (* Can see through changed region before the change. *) 
             OR InsLen = 0 
                (* Can see through changed region after the change. *) 
      ; MteEstNodeCt := EstUtil . EstNodeCt ( EstRootRef ) 
      ; NewBolTokMark := Marks . TokMarkNull 
      ; MteMarkCt := 0 
      ; NewLinesCt := 0 
      ; MaxTouchedNodeNo := LbeStd . EstNodeNoNull  
      ; LeadingBlankLinesIncluded := 0
      ; TrailingBlankLinesIncluded := 0
      ; IF StartTokMark . StartAtEnd 
        THEN 
          MteState := MteStateTyp . MteStateStartAtEnd 
        ; MteFinishItemNo := 1 (* Will be reassigned later, same value. *)  
        ELSE 
          MteState := MteStateTyp . MteStateStartAtBeg 
        ; MteFinishItemNo := 0 
          (* Will be reassigned later, mostly same value. *)  
        END (* IF *) 
      ; MteTraverseEst 
          ( EstRef := EstRootRef 
          , KindSet := EstHs . EstChildKindSetEmpty  
          , EstAbsNodeNo := 0 
          , ParentFsNodeRef := NIL 
          , RootFsNodeRef := EstUtil . FsRuleForEstNode ( Lang , EstRootRef ) 
          , EstFmtKind := LangUtil . FmtKindTyp . FmtKindVert 
          , EstIndentPos1 := Options . InitialIndentPos  
          , EstIndentPosN := Options . InitialIndentPos  
          , (* VAR *) NewEstRef := LNewEstRootRef 
          , (* VAR *) NewSingletonOptKindSet 
                        := LNewSingletonOptKindSet (* Dead. *) 
          , (* VAR *) LMCharPos := LLMCharPos (* Ignore. *) 
          ) 
      ; NewEstRootRef := LNewEstRootRef
      ; NodeNoChange 
          := EstUtil . EstNodeCt ( NewEstRootRef ) - MteEstNodeCt 
        (* All the changes we make to the Est are in one or two lines. 
           Those lines following the changes, to which NodeNoChange applies,
           all will have the same change, and it will also be the change 
           in the node count for the entire Est. *) 

      ; Assert
          ( MteState = MteStateTyp . MteStateDone 
          , AFT . A_MergeTextEdit_NotDone 
          )
      ; Assert ( MteBlankSs = 0 , AFT . A_MergeTextEditNotZeroBlankSs ) 
      ; Assert 
          ( MaxTouchedNodeNo # LbeStd . EstNodeNoNull  
          , AFT . A_MergeTextEdit_No_MaxTouchedNodeNo  
          )
      ; Assert ( MteMarkCt > 0 , AFT . A_MergeTextEdit_NoNewMark ) 

(* TODO: Since the following two assertions are a bit time-consuming, do them
        only if the debug level is elevated or there is a failure.
*) 
      ; TravUtil .  GetDescendantWithNodeNo
          ( NewEstRootRef
          , NewBolTokMark . EstNodeNo
          , (* VAR *) LNewMarkedEstRef 
          , (* VAR *) LKindSet 
          , (* VAR *) LIsOptSingletonList 
          ) 
      ; Assert 
          ( NewBolTokMark . EstNodeCt = EstUtil . EstNodeCt ( LNewMarkedEstRef )
          , AFT . A_MergeTextEdit_BadMarkEstNodeCt  
          ) 
      ; Assert 
          ( NewBolTokMark . TkmEstRef = LNewMarkedEstRef
          , AFT . A_MergeTextEdit_BadTkmEstRef   
          ) 
      ; MteCheckTokMarks ( ) 
      END (* Block *) 
    END MergeTextEdit 

; BEGIN (* MergeTxt *) 
  END MergeTxt 
. 
