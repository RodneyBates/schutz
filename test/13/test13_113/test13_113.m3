(* This module contains merges a localized set of text edit 
   changes into an Est. *) 
MODULE MergeTxt 

; IMPORT TreeBrowse 

; IMPORT PortTypes 
; IMPORT LbeStd 
; IMPORT Strings 
; IMPORT EstHs 
; IMPORT ModHs 
; IMPORT Marks 
; IMPORT Options 
; IMPORT SharedStrings 
; IMPORT EstUtil 
; IMPORT EstBuild 
; IMPORT LangUtil 
; IMPORT TravUtil 
; IMPORT Assertions 
; IMPORT MessageCodes 

; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 

; TYPE AFT = MessageCodes . T 

; TYPE MarkKindTyp = Marks . MarkKindTyp 

(* VISIBLE *) 

; PROCEDURE MergeTextEdit 
    ( Lang : LbeStd . LangTyp 
    ; EstRootRef : EstHs . EstRefTyp 
    ; StartTokMark : Marks . TokMarkTyp 
    ; EndTokMark : Marks . TokMarkTyp 
    ; BlankLineNo : LbeStd . LineNoTyp 
      (* Line number within a blank line mod or EndOfImage. *) 
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
    ; VAR NewEstRef : EstHs . EstRefTyp 
    ; VAR NodeNoChange : LbeStd . EstNodeNoTyp 
    ; VAR MaxTouchedNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR NewBolTokMark : Marks . TokMarkTyp 
    ; VAR NewLinesCt : LbeStd . LineNoTyp 
    ; VAR LeadingBlankLinesIncluded : LbeStd . LineNoTyp  
      (* ^This many lines from leading blank mods were incorporated into the 
         resulting set of tree children. *)  
    ; VAR TrailingBlankLinesIncluded : LbeStd . LineNoTyp  
      (* ^Similarly, for trailing mods. *) 
    ) 
    RAISES { AssertionFailure } 

  (* MergeTextEdit puts a single location group of text 
     modifications into an Est. *) 

  (* It begins in state MteStateStartAtBeg or MteStateStartAtEnd, where 
     it descends through the tree to where StartTokMark points.  If 
     this is a blank line mod or off the end of image, all the text edits 
     are handled in one routine, without further traversing. Otherwise, 
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
 
     It also develops an array (named MteBlanks) whose 
     elements correspond to all comment, token insertion 
     and text insertion mods, insertion toks, 
     and Ast strings in the line and whose element 
     values give the actual number of blanks inserted ahead of 
     the item, and, in the case of a text insertion, after it as 
     well (in a separate element of the array). This will be needed 
     to reconstruct the line when going backwards. 
 
     When it reaches DelToPos, it starts looking for a blank, still 
     going forwards.  The token before the next blank is the last 
     one "touched" by the modification. 
 
     When it reaches EOL, it goes into MteStateBwdEdit, where it 
     does reconstruction of the Est nodes.  It appends/prepends 
     characters of tokens touched but not deleted to the insertion 
     text.  It deletes all touched tokens from the Est, in the 
     manner appropriate to the token kind. Finally, it constructs 
     a new text insertion mod and inserts it. 
 
     It reconstructs Ests right to left during its backward pass, 
     using EstBuild to build the new child list at each level. 
 
     When it gets back to DelFromPos, it goes into MteStateBwdNl, 
     where it continues to traverse to the beginning of line, (since 
     the Nl could be implicit, it detects this by MteItemCt's going 
     to zero), rebuilding Ests at each level. 
 
     At the end, it builds a new TokMark (or TokMarks, if there are 
     multiple lines/blank lines) and differentiates the rest 
     of the Est. 
  *) 

  = TYPE MteStateTyp 
      = { MteStateStartAtBeg    (* Start at the beginning of the token 
                                   identified by StartTokMark. *) 
        , MteStateStartAtEnd    (* Start at the beginning of the token 
                                   identified by StartTokMark. *) 
        , MteStateFwd           (* Looking for the new line at the end of 
                                   the edited line. *) 
        , MteStatePassingNl     (* Going forward, passing over a new line 
                                   (in the old Est) *) 
        , MteStateRightNlFound  (* Hit the ending Nl, but still traversing.
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
       point is reached, we go in to another state to traverse left-
       to-right. *) 

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
        (* ^MteTeFinishBwdEdit has done its work. *) 
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
             Nl after of ModBlankLine, ModCmnt, or ModText 
             LexError 
             InsTok 
             AstString Tok 
             BOI 
             LineBreak, either started at or passed. 
        *) 
  ; VAR MteFinishItemNo : PortTypes . Card32Typ 
    (* When MteItemCt gets down to this value, it is time for 
       MteTeFinishBwdEdit to do its work.  When starting at a
       line break or Nl after, this will be one, because by the
       time we get back to the line break or Nl after, we could
       have ascended to a higher tree and lost the correct place
       to insert the new mods. Otherwise, it is zero. *) 
  ; VAR MteNlItemNo : PortTypes . Card32Typ 
    (* The value of MteItemCt when we hit the LM Nl that we pass. *) 
 
  ; VAR MteBlankSs : LbeStd . LimitedCharNoSignedTyp 
  (* Most variables local to MergeTextEdit describe a "joined line",
     which is as it was before inserting a Nl, but after deleting 
     an Nl.  Vars with "New" in the name are for the joined, edited
     line.  Others are unedited. *) 
  ; VAR MteTouchedFromPos : LbeStd . LimitedCharNoSignedTyp 
  ; VAR MteTouchedToPos : LbeStd . LimitedCharNoSignedTyp 
  ; VAR MteLineShift : LbeStd . LimitedCharNoTyp 
        (* Amount to add to an original character position, to get the
           corresponding position in the two lines joined by a passed,
           deleted new line.  Add this to a position given by the old 
           Est to get a position in the unedited, joined line. *) 
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
  ; VAR MteNewDelFromFmtNo : LangUtil . FmtNoTyp := LangUtil . FmtNoNull  
  ; VAR MteNewDelThruFmtNo : LangUtil . FmtNoTyp := LangUtil . FmtNoNull  
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
  ; VAR MteNewBlankLineFmtNo : LangUtil . FmtNoTyp 
  ; VAR MteImbeddedNlModRef : ModHs . ModRefTyp 
  ; VAR MteEstNodeCt : LbeStd . EstNodeNoTyp 
  ; VAR MteModTextIsToLeftOnLine : BOOLEAN 
  ; VAR MteLastFmtNoOnLine : LangUtil . FmtNoTyp
  ; VAR MteEstListChildrenToPass : LbeStd . EstChildNoTyp 
  ; VAR MteBlanks 
      : ARRAY LbeStd . LimitedCharNoTyp OF LbeStd . LimitedCharNoSignedTyp 

  ; VAR MteNewChars1 : ARRAY LbeStd . LimitedCharNoTyp OF CHAR 
  ; VAR MteNewChars2 : ARRAY LbeStd . LimitedCharNoTyp OF CHAR 

  (* Recursive traversal of format syntax trees. *) 

  ; PROCEDURE MteTraverseEst 
      ( EstRef : LbeStd . EstRootTyp 
      ; RootAbsNodeNo : LbeStd . EstNodeNoTyp 
      ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
      ; EstFmtKind : LangUtil . FmtKindTyp 
        (* ^Only meaningful during forward traversal. *) 
      ; EstIndentPos1 : LbeStd . LimitedCharNoTyp 
      ; EstIndentPosN : LbeStd . LimitedCharNoTyp 
      (* EstIndentPos1 and EstIndentPosN are maintained assuming
         the Est subtree is formatted vertically. *) 
      ; VAR NewEstRef : LbeStd . EstRootTyp 
      ; VAR LMCharPos : LbeStd . LimitedCharNoTyp 
        (* ^Position of leftmost nonblank of the subtree.  
           LimitedCharNoUnknown, if none exists. *) 
      ) 
      RAISES { AssertionFailure } 

    (* New Est building. *) 
    (* New Ests are built right to left, during 
       the backwards pass.  There are several levels of laziness 
       in building new Ests.  We use "wait" as a transitive verb to mean 
       put into a waiting state. 
 
       Initiating a merge is postponed while a suffix of the old 
       Est is being collected.  MteTeMergeIsStarted tells when a
       merge has been started.  Each child to be merged must wait 
       until its temporal successor (spatial predecessor) is known, 
       in order to determine whether the current child is the start of a 
       group of Est children all with the same format number. 

       When MteTeChildIsWaiting, MteWaitingRef is a single child in 
       this postponed state, MteWaitingKindSet is its kind set, 
       MteTeWaitingEdgeKind is its edge kind, and MteTeWaitingFmtNo 
       is its format number. This is only done for newly constructed 
       children. We need MteTeChildIsWaiting too, because a NIL can be 
       waiting to be merged.  
 
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
       any waiting child or slice. Building the deletion mod is postponed 
       so as many format numbers as possible can be included in one 
       delete mod.  When it finally needs to be constructed, there is 
       still a possibility that the left sib of a waiting slice will 
       happen to match the needed delete mod.  In this case, the 
       waiting slice is just extended on its left, to reuse the sib.
       MteNewDelIsRepair means set ModDelIsRepair TRUE in the new ModDel,  
       while MteNewDelIsNotRepair means set ModDelIsRepair to FALSE. 
       Neither of these means it doesn't matter, which happens for a delete 
       of a line break.  Both can't happen. 
 
       Alternatively, MteNewBlankLineFmtNo # LbeStd . FmtNoNull implies
       an unconstructed blank line mod with FmtNo MteNewBlankLineFmtNo and 
       line count MteNewBlankLineCt is waiting.  This blank line mode will go 
       to the left of any waiting item or slice. Building it is postponed 
       so as many blank lines as possible can be included in one blank 
       line mod.  When it finally needs to be constructed, there is 
       still a possibility that the left sib of a waiting slice will 
       happen to match the needed blank line mod.  In this case, the 
       waiting slice is just extended on its left, to reuse the sib. 
       
       MteNewDelThruFmtNo # LbeStd . FmtNoNull  and
       MteNewBlankLineFmtNo # LbeStd . FmtNoNull are mutually 
       exclusive. 

       When no children are waiting, MteTeWaitingFmtNo is the value
       for the leftmost child already merged into the being-built
       new Est node, or LangUtil . FmtNoNull, if none. 
 
       The text mods and/or blank line mods to be inserted for the 
       new text are waited (for merging) when built.  In all cases 
       but one, building of these happens to be followed immediately 
       by a call to wait them.  However, when building occurs while 
       inside an Est subtree which is an Ast leaf, building/waiting 
       of these is postponed until we are back up a level in the Est, 
       where they will be merged not as children of the Ast leaf, 
       but as (replacement) sibs of it. 
 
    *) 

    = VAR MteTeStartFmtNo : LangUtil . FmtNoTyp 
    ; VAR MteTeEstTravInfo : TravUtil . EstTravInfoTyp 
    ; VAR MteTeRMChildRef : LbeStd . EstRootTyp 
    ; VAR MteTeRMChildRootNodeNo : LbeStd . EstNodeNoTyp 
          (* MteTeRMChildRef and MteTeRMChildRootNodeNo 
             both trail behind the current child 
             of MteTeEstTravInfo, during forward movement, whenever 
             that child moves rightward. The only time it is guaranteed 
             to be set is when we are going forward and the current child 
             is off the right end, at which time, it denotes the rightmost 
             real child, hence the name RM. *) 
    ; VAR MteTeRightChildRootNodeNo : LbeStd . EstNodeNoTyp 
          (* MteTeRightChildRootNodeNo temporally trails behind the current 
             child of the newly constructed Est node, when in state 
             MteStateBwdNl, and thus identifies the child to the right of 
             the current one. It is used to construct TokMarks. *) 
(* CHECK: ^Review this. *) 

    ; VAR MteTeContainsFill : BOOLEAN 
    ; VAR MteTeInterestingChildrenExist : BOOLEAN 
          (* This is set TRUE by any real Est child other than a delete mod 
             or a dummy.  It is also set true by a non-deleted insertion token,
             since these represent visible children of the expanded tree.
             If there are no interesting children, an entire Est can be
             NIL or omitted. *) 
    ; VAR MteTeNILChildrenExist : BOOLEAN 
          (* This is only accurate if the new Est has exactly one child. 
             This is the only case we care about, and it is a lot 
             easier to compute. *) 
    ; VAR MteTeSibFmtNoMarkExists : BOOLEAN  
    ; VAR MteTeMergeIsStarted : BOOLEAN 
    ; VAR MteTeChildIsWaiting : BOOLEAN 
    ; VAR MteTeMergeState : EstBuild . MergeStateTyp 
    ; VAR MteTeWaitingRef : LbeStd . EstRootTyp 
    ; VAR MteTeWaitingFmtNo : LangUtil . FmtNoTyp 
    ; VAR MteTeWaitingEdgeKind : EstHs . EdgeKindTyp 
    ; VAR MteTeWaitingFromChildNo : LbeStd . EstChildNoTyp 
      (* ^ = LbeStd . EstChildNoNull, if no children of EtiParentRef
             are waiting. *) 
    ; VAR MteTeWaitingToChildNo : LbeStd . EstChildNoTyp 
    ; VAR MteTeWaitingKindSet : EstHs . EstChildKindSetTyp 
      (* ^For the leftmost child, if MteTeWaitingFromChildNo 
          # LbeStd . EstChildNoNull . *) 
    ; VAR MteTeCfInsTokFromPos : LbeStd . LimitedCharNoSignedTyp 
    ; VAR MteTeCfEstChildFromPos : LbeStd . LimitedCharNoSignedTyp 
    ; VAR MteTeCfEstChildToPos : LbeStd . LimitedCharNoSignedTyp 
    ; VAR MteTeCfInsTokToPos : LbeStd . LimitedCharNoSignedTyp 
      (* MteTeCf-- variables allow expanding the touched region to
         cover all insertion tokens of a conditional format subtree,
         if there is a possibility that its Est child could change
         from non-empty to empty.  If this happens, the predicate
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
    ; VAR MteTeIsFirstLine : BOOLEAN := TRUE   
    ; VAR MteTeIndentPos : LbeStd . LimitedCharNoTyp 
          := LbeStd . LimitedCharNoUnknown  
    (* MteTeIsFirstLine and MteTeIndentPos are maintained only during
       forward states, and assuming the Est subtree is formatted vertically. *)
    ; PROCEDURE MteTeIncEstChild ( ) 
      RAISES { AssertionFailure } 

      = BEGIN (* MteTeIncEstChild *) 
          MteTeRMChildRef := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
        ; MteTeRMChildRootNodeNo := MteTeEstTravInfo . EtiChildRootNodeNo 
        ; TravUtil . IncEstChild ( MteTeEstTravInfo ) 
        END MteTeIncEstChild 

    ; PROCEDURE MteTeDecEstChild ( ) 
      RAISES { AssertionFailure } 
(* TODO: Consider inlining this. *) 

      = BEGIN (* MteTeDecEstChild *) 
          MteTeRightChildRootNodeNo 
            := MteTeEstTravInfo . EtiChildRootNodeNo 
        ; TravUtil . DecEstChild ( MteTeEstTravInfo ) 
        END MteTeDecEstChild 

    (* Waiting items for rebuilt Ests. *) 

    ; PROCEDURE MteTeEstChildrenAreToRightInNewEst ( ) : BOOLEAN 

      = BEGIN (* MteTeEstChildrenAreToRightInNewEst *) 
          RETURN 
            MteTeMergeIsStarted 
            OR MteTeChildIsWaiting 
            OR MteTeWaitingFromChildNo # LbeStd . EstChildNoNull 
            OR MteNewDelThruFmtNo # LangUtil . FmtNoNull 
            OR MteNewBlankLineFmtNo # LangUtil . FmtNoNull 
        END MteTeEstChildrenAreToRightInNewEst 

    ; PROCEDURE MteTeBwdLMNewFmtNo ( ) : LangUtil . FmtNoTyp 
      (* During backwards traversal, the format number of the item
         in the being-built Est child list to the right of where we
         are, i.e. leftmost of what has been waited or merged so far. *)  

      = BEGIN
          IF MteNewDelThruFmtNo # LangUtil. FmtNoNull
          THEN RETURN MteNewDelFromFmtNo 
          ELSIF MteNewBlankLineFmtNo # LangUtil. FmtNoNull 
          THEN RETURN MteNewBlankLineFmtNo 
          ELSE RETURN MteTeWaitingFmtNo 
          END 
        END MteTeBwdLMNewFmtNo

    ; PROCEDURE MteTeFlushChild 
        ( NewFmtNo : LangUtil . FmtNoTyp ; NewEdgeKind : EstHs . EdgeKindTyp ) 
      RAISES { AssertionFailure } 

      = BEGIN (* MteTeFlushChild *) 
          IF MteTeChildIsWaiting 
          THEN  
            IF NOT MteTeMergeIsStarted 
            THEN 
              MteTeMergeState 
                := EstBuild . InitMerge 
                     ( Lang 
                     , MteTeEstTravInfo . EtiParentRef . EstTok 
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
          ; MteTeWaitingRef := NIL (* Defensive *)  
          ; MteTeChildIsWaiting := FALSE 
          END (* IF *) 
        END MteTeFlushChild 

    ; PROCEDURE MteTeFlushSliceOrChild 
        ( NewFmtNo : LangUtil . FmtNoTyp ; NewEdgeKind : EstHs . EdgeKindTyp ) 
      RAISES { AssertionFailure } 

      = VAR LDontCareRef : LbeStd . EstRootTyp 

      ; BEGIN (* MteTeFlushSliceOrChild *) 
          IF MteTeWaitingFromChildNo # LbeStd . EstChildNoNull  
          THEN 
            IF NOT MteTeMergeIsStarted 
            THEN 
              MteTeMergeState 
                := EstBuild . InitMerge 
                     ( Lang 
                     , MteTeEstTravInfo . EtiParentRef . EstTok 
                     , EstRefToInheritFrom := MteTeEstTravInfo . EtiParentRef 
                       (* Do not recompute WidthInfo or SyntTokCt, so trees
                          still format the same. *) 
                     ) 
            ; MteTeMergeIsStarted := TRUE 
            END (* IF *) 
          ; EstBuild . MergeSlice 
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
              , LeftmostNewChildRef := LDontCareRef 
              ) 
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
        ; VAR FmtNo : LangUtil . FmtNoTyp 
        ) 
      RAISES { AssertionFailure } 
      (* Find the left sib of the leftmost waiting Est child. *) 

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
                , EstChildNo , LNodeNo , ChildLeafElem 
                ) 
            ; IF EstHs . EstChildKindFirstOfGroup IN MteTeWaitingKindSet 
              THEN 
                EstUtil . PrevInKindSet 
                  ( MteTeEstTravInfo . EtiParentRef 
                  , EstChildNo 
                  , EstHs . EstChildKindSetFirstOfGroup 
                  , LFirstOfGroupChildNo 
                  , LNodeNo 
                  , LFirstOfGroupLeafElem 
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

    ; PROCEDURE MteTeFlushDels ( ) RAISES { AssertionFailure } 

      = VAR LExists : BOOLEAN 
      ; VAR LEstChildNo : LbeStd . EstChildNoTyp 
      ; VAR LModDelRef : ModHs . ModDelTyp 
      ; VAR LFmtNo : LangUtil . FmtNoTyp 
      ; VAR LChildLeafElem : EstHs . LeafElemTyp 

      ; BEGIN (* MteTeFlushDels *) 
          IF MteNewDelThruFmtNo # LangUtil . FmtNoNull 
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
                 ; MteNewDelThruFmtNo := LangUtil . FmtNoNull 
                 ; MteNewDelFromFmtNo 
                     := LangUtil . FmtNoNull (* Defensive *) 
                 ; MteNewDelIsRepair := FALSE (* Defensive *) 
                 ; MteNewDelIsNotRepair := FALSE (* Defensive *) 
                 ; RETURN 
                 END (* IF *) 
              ELSE 
              END (* TYPECASE *) 
            END (* IF LExists *) 
          (* Finding a sib of a slice didn't work. 
             Construct a new deletion mod and wait it. *) 
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
          ; MteNewDelThruFmtNo := LangUtil . FmtNoNull 
          ; MteNewDelFromFmtNo := LangUtil . FmtNoNull (* Defensive *) 
          ; MteNewDelIsRepair := FALSE (* Defensive *) 
          ; MteNewDelIsNotRepair := FALSE (* Defensive *) 
          END (* IF *) 
        END MteTeFlushDels 

    ; PROCEDURE MteTeFlushBlankLines ( ) RAISES { AssertionFailure } 

      = VAR LExists : BOOLEAN 
      ; VAR LEstChildNo : LbeStd . EstChildNoTyp 
      ; VAR LModBlankLineRef : ModHs . ModBlankLineTyp 
      ; VAR LFmtNo : LangUtil . FmtNoTyp 
      ; VAR LChildLeafElem : EstHs . LeafElemTyp 

      ; BEGIN (* MteTeFlushBlankLines *) 
          IF MteNewBlankLineFmtNo # LangUtil . FmtNoNull 
          THEN (* Some unconstructed blank lines are waiting.
                  Turn them into a new or reused read ModBlankLine. *) 
            MteTeFindLeftSib 
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
                 ; MteNewBlankLineFmtNo := LangUtil . FmtNoNull 
                 ; MteNewBlankLineCt := 0 (* Defensive. *) 
                 ; RETURN 
                 END (* IF *) 
              ELSE 
              END (* TYPECASE *) 
            END (* IF LExists *) 
          (* Finding a sib of a slice didn't work. 
             Construct a new blank line mod and wait it. *) 
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
          ; MteNewBlankLineFmtNo := LangUtil . FmtNoNull 
          ; MteNewBlankLineCt := 0 (* Defensive. *) 
          END (* IF *) 
        END MteTeFlushBlankLines 

    ; PROCEDURE MteTeWaitBlankLines 
        ( BlankLineCt : LbeStd . LineNoTyp ; FmtNo : LangUtil . FmtNoTyp ) 
      RAISES { AssertionFailure } 

      = BEGIN (* MteTeWaitBlankLines *) 
          IF BlankLineCt > 0 
          THEN 
            MteTeFlushDels ( ) 
          ; IF MteNewBlankLineFmtNo # FmtNo 
            THEN 
              MteTeFlushBlankLines ( ) 
            ; MteNewBlankLineCt := BlankLineCt 
            ; MteNewBlankLineFmtNo := FmtNo 
            ELSE 
              INC ( MteNewBlankLineCt , BlankLineCt ) 
            END (* IF *) 
          END (* IF *) 
        END MteTeWaitBlankLines 

    ; PROCEDURE MteTeIncludeInModDel 
        ( FmtNo : LangUtil . FmtNoTyp 
        ; IsRepair := FALSE 
        ; IsNotRepair := FALSE 
        (* Neither IsRepair nor IsNotRepair means it doesn't matter *) 
        ) 
      RAISES { AssertionFailure } 

      (* Arrange for this item to be included in a delete mod. *) 

      = BEGIN (* MteTeIncludeInModDel *) 
          IF MteNewDelThruFmtNo # LangUtil . FmtNoNull 
             AND ( IsRepair AND MteNewDelIsNotRepair 
                   OR IsNotRepair AND MteNewDelIsRepair
                 ) 
          THEN (* Range with conflicting IsRepair is waiting. *) 
            MteTeFlushDels ( ) 
          END (* IF *) 
        ; IF MteNewDelThruFmtNo = LangUtil . FmtNoNull 
          THEN (* No range is waiting, start one. *)  
            MteTeFlushBlankLines ( ) 
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
        ; FmtNo : LangUtil . FmtNoTyp 
        ; EdgeKind : EstHs . EdgeKindTyp  
        ) 
      RAISES { AssertionFailure } 
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
        ; MteTeFlushBlankLines ( ) 
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
        ( NewFmtNo : LangUtil . FmtNoTyp ; NewEdgeKind : EstHs . EdgeKindTyp ) 
      RAISES { AssertionFailure } 
      (* Flush anything that is waiting, except for a slice as the
         leftmost waiting thing. *) 

      = BEGIN
          IF MteNewDelThruFmtNo # LangUtil . FmtNoNull 
             OR MteNewBlankLineFmtNo # LangUtil . FmtNoNull 
             OR MteTeChildIsWaiting 
          THEN 
            MteTeFlushDels ( ) 
          ; MteTeFlushBlankLines ( ) 
          ; MteTeFlushChild ( NewFmtNo , NewEdgeKind ) 
          END 
        END MteTeFlushExceptSlice 

    ; PROCEDURE MteTeWaitExistingEstChild 
        ( EdgeKind : EstHs . EdgeKindTyp 
        ; IsInteresting : BOOLEAN 
        )  
      RAISES { AssertionFailure } 
      (* Wait the current Est child. *) 

      = VAR LNewEdgeKind : EstHs . EdgeKindTyp 

      ; BEGIN (* MteTeWaitExistingEstChild *) 
          IF 0 <= MteTeEstTravInfo . EtiChildNo 
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

    ; PROCEDURE MteTeWaitChildrenToLeft ( ) RAISES { AssertionFailure } 
      (* Wait the range of existing Est children of the current parent node, 
         from the leftmost child through the current child. *) 

      = VAR LNewEdgeKind : EstHs . EdgeKindTyp 
      ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
      ; VAR LLeafElem : EstHs . LeafElemTyp 

      ; BEGIN (* MteTeWaitChildrenToLeft *) 
          IF 0 <= MteTeEstTravInfo . EtiChildNo  
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
            END (* IF *) 
          ; MteTeWaitingFromChildNo := 0 
          ; EstUtil . GetIthChild 
              ( MteTeEstTravInfo . EtiParentRef , 0 , LNodeNo , LLeafElem ) 
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
          ELSE (* There are no children to left, but we must flush anything
                  else, as leftmost children
               *) 
            MteTeFlushDels ( ) 
          ; MteTeFlushBlankLines ( ) 
          ; MteTeFlushSliceOrChild  
              ( LangUtil . FmtNoNull 
              , EstHs . EdgeKindTyp . EdgeKindLeadingMod (* Irrelevant. *) 
              ) 
          END 
        END MteTeWaitChildrenToLeft 

    ; PROCEDURE MteTeWaitChildrenToRight ( ) RAISES { AssertionFailure } 
      (* Wait the range of existing Est children of the current parent node, 
         from the current child through the rightmost child. *) 

      = BEGIN (* MteTeWaitChildrenToRight *) 
          Assert 
            ( NOT MteTeEstChildrenAreToRightInNewEst ( ) 
              (* Because we only do this prior to making any changes to 
                 Est children. *) 
            , AFT . A_MteTeWaitChildrenToRightAlreadyStarted 
            ) 
        ; IF 0 <= MteTeEstTravInfo . EtiChildNo 
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
      RAISES { AssertionFailure } 
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
                 Est children. *) 
            , AFT . A_MteTeWaitChildrenFartherToRightAlreadyStarted 
            ) 
        ; LEstChildNo := MteTeEstTravInfo . EtiChildNo + 1 
        ; IF LEstChildNo < MteTeEstTravInfo . EtiChildCt 
          THEN 
            MteTeWaitingFromChildNo := LEstChildNo 
          ; MteTeWaitingToChildNo := MteTeEstTravInfo . EtiChildCt 
          ; EstUtil . GetIthChild 
              ( MteTeEstTravInfo . EtiParentRef 
              , LEstChildNo 
              , LNodeNo 
              , LLeafElem 
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
        END MteTeWaitChildrenFartherToRight 

    (* Actual Est rebuilding *) 

    ; PROCEDURE MteTeBuildNewModText 
        ( FromPos : LbeStd . LimitedCharNoTyp 
        ; ToPos : LbeStd . LimitedCharNoSignedTyp 
        ; LeftTokToPos : LbeStd . LimitedCharNoTyp 
        ; READONLY Chars : ARRAY OF CHAR 
        ; TextLen : LbeStd . LimitedCharNoTyp 
        ; VAR ResultRef : ModHs . ModTextTyp 
          (* NIL => was a blank line.  In any case, it has not been waited. *) 
        ) 
      RAISES { AssertionFailure } 

      = VAR LNewModRef : ModHs . ModTextTyp 
      ; VAR LFromSs : PortTypes . Int32Typ 
      ; VAR LThruSs : PortTypes . Int32Typ 

      ; BEGIN (* MteTeBuildNewModText *) 
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
            ResultRef := NIL 
          ELSE 
            LNewModRef := NEW ( ModHs . ModTextTyp ) 
          ; LNewModRef . ModTextFromPos := FromPos + LFromSs  
          ; LNewModRef . ModTextToPos := ToPos 
          ; LNewModRef . ModTextLeftTokToPos := LeftTokToPos 
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
                     , LbeStd . TokModText 
                     )
            END (* IF *) 
          ; ResultRef := LNewModRef 
          END (* IF *) 
        END MteTeBuildNewModText 

    ; PROCEDURE MteTeFinishBwdEdit 
        ( FmtNo : LangUtil . FmtNoTyp ; MarkNodeNo : LbeStd . EstNodeNoTyp ) 
      RAISES { AssertionFailure } 
      (* If now is the time to do it, go out of MteStateBwdEdit, build
         and wait whatever new Mods are required, count NewLinesCt for
         the built mods, set NewBolTokMark, set state to BwdNl or
         Done. *)

      = VAR LToPos : LbeStd . LimitedCharNoTyp 
      ; VAR LNewModRef1 : ModHs . ModTextTyp 
            (* ^Leftmost new text mod, NIL if it turned out a blank line. *) 
      ; VAR LNewModRef2 : ModHs . ModTextTyp (* Rightmost. *) 
      ; VAR LNewBlankLinesBefore : LbeStd . LineNoTyp 
      ; VAR LNewBlankLinesAfter : LbeStd . LineNoTyp 
      ; VAR LTextLen : LbeStd . CharNoTyp 

      ; BEGIN (* MteTeFinishBwdEdit *) 
          IF MteState = MteStateTyp . MteStateBwdEdit  
             (* ^Don't finish more than once. *) 
             AND ( MteItemCt <= MteFinishItemNo (* Finish if back to start *) 
                   OR ( MteNewTextLeftTokToPos > 0 
                        (* Finish if there are nonblank things from the old 
                           Est to the left of the new mods (these need to also 
                           be in the new Est), *)
                        AND MteCharPos <= MteTouchedFromPos 
                            (* and we are left of the new mods. *) 
                      ) 
                 ) 
          THEN 

          (* Build needed text modifiers. *) 
            IF InsNlPos = LbeStd . LimitedCharNoInfinity 
            THEN (* No inserted new line, only one ModText insertion. *) 
              MteTeBuildNewModText 
                ( FromPos := MteTouchedFromPos 
                , ToPos := MteNewTextToPos 
                , LeftTokToPos := MteNewTextLeftTokToPos 
                , Chars := MteNewChars1 
                , TextLen := MteNewTextLen 
                , ResultRef := LNewModRef1 
                ) 
            ; LNewBlankLinesBefore 
                := MteBlankLinesBefore + ORD ( LNewModRef1 = NIL ) 
            ; LNewBlankLinesAfter := MteBlankLinesAfter 
            ; LNewModRef2 := NIL 
            ; IF LNewModRef1 = NIL 
                 AND DelNlShift # LbeStd . LimitedCharNoInfinity 
                 AND DelNlShift # 0 
                 AND MteBlankLinesAfter = 0 
              THEN (* Deleted Nl is at end of blank line mod. 
                      We must create a degenerate ModText in order 
                      to preserve the starting position DelNlShift, 
                      which controls the position of the text following. *) 
                LNewModRef1 := NEW ( ModHs . ModTextTyp ) 
              ; LNewModRef1 . ModTextFromPos := DelNlShift 
              ; LNewModRef1 . ModTextToPos := DelNlShift 
              ; LNewModRef1 . ModTextLeftTokToPos := MteNewTextLeftTokToPos 
              ; LNewModRef1 . ModTextStringRef := NIL 
              END (* IF *) 
            ELSE (* There is a new line somewhere in the inserted text. *) 
              IF MteNewTextToPos = LbeStd . LimitedCharNoInfinity 
              THEN 
                LToPos := LbeStd . LimitedCharNoInfinity 
              ELSE 
                LToPos 
                  := EstUtil . WidthValue 
                       ( MteNewTextToPos 
                         - ( MteTouchedFromPos + MteNewTextRelNlPos ) 
                         + NlIndentPos 
                       ) 
              END (* IF *) 
            (* Build left new text mod. *) 
            ; LNewBlankLinesBefore := MteBlankLinesBefore 
            ; IF MteNewTextRelNlPos > 0 (* Nonempty text before the Nl *) 
              THEN (* A ModText is needed for the 1st line. *) 
                MteTeBuildNewModText 
                  ( FromPos := MteTouchedFromPos 
                  , ToPos := LbeStd . LimitedCharNoInfinity 
                             (* ^Means new line after. *) 
                  , LeftTokToPos := MteNewTextLeftTokToPos 
                  , Chars := MteNewChars1 
                  , TextLen := MteNewTextRelNlPos 
                  , ResultRef := LNewModRef1 
                  )
              ; INC ( LNewBlankLinesBefore , ORD ( LNewModRef1 = NIL ) ) 
                (* ^The left text was, however, all blank. *) 
              ELSE (* The added Nl is at the left of the edited region. *) 
                INC ( LNewBlankLinesBefore , ORD ( MteNewTextLeftTokToPos = 0 ) ) 
              ; LNewModRef1 := NIL 
              END (* IF *) 
            ; LNewBlankLinesAfter := MteBlankLinesAfter 
         (* ; IF MteNewTextRelNlPos = MteNewTextLen 
                 (* Inserted Nl at end of region. *) 
                 AND LNewModRef1 # NIL 
                     (* We did not fold the inserted Nl into 
                        LNewBlankLinesBefore *) 
              THEN
                IF LToPos = LbeStd . LimitedCharNoInfinity  
                THEN (* Need an additional blank line after *) 
                  INC ( LNewBlankLinesAfter ) 
                ELSE (* Change right new mod to have Nl after. *)  
                  LToPos := LbeStd . LimitedCharNoInfinity  
                END 
              END (* IF *) 
         *) 
            ; LTextLen := MteNewTextLen - MteNewTextRelNlPos 
            ; MteTeBuildNewModText 
                ( FromPos := NlIndentPos 
                , ToPos := LToPos 
                , LeftTokToPos := 0 (* Implies Nl before. *) 
                , Chars := MteNewChars2 
                , TextLen := LTextLen 
                , ResultRef := LNewModRef2 
                ) 
            ; INC ( LNewBlankLinesAfter , ORD ( LNewModRef2 = NIL ) ) 
            END (* IF *) 

          (* Insert new tree children and build a new mark. *) 
          ; IF LNewModRef1 = NIL AND LNewModRef2 = NIL 
            THEN (* Edited text consists of blank lines only. 
                    A single blank line mod will do. *) 
              IF LNewBlankLinesBefore + LNewBlankLinesAfter > 0 
              THEN 
                MteTeWaitBlankLines 
                  ( LNewBlankLinesBefore + LNewBlankLinesAfter , FmtNo ) 
              ; MteTeFlushBlankLines ( ) 
              ; IF DelNlShift # LbeStd . LimitedCharNoInfinity  
                THEN (* We would have passed the Nl at the end of the
                        new blank line.  Count a LinesRec. *) 
                  INC ( NewLinesCt ) 
                END 
              ; NewBolTokMark 
                  := Marks . TokMarkTyp 
                       { EstNodeNo := MarkNodeNo 
                       , Kind := MarkKindTyp . BlankLine 
                       , FmtNo := FmtNo 
                       , StartAtEnd := FALSE 
                       , IsImpliedNewLine := FALSE 
                       , Tok := LbeStd . TokBlankLine  
                       } 
              ; INC ( NewLinesCt ) 
              ; IF LNewBlankLinesBefore = 0 
                THEN (* This mod starts at an inserted Nl. *)  
                  MteState := MteStateTyp . MteStateBwdNl  
                ELSE (* This is the replacement for the start Nl. *)  
                  MteState := MteStateTyp . MteStateDone 
                END 
              ELSE 
                MteState := MteStateTyp . MteStateBwdNl  
              END 
            ELSE (* We have at least one ModText *) 

            (* If necessary, insert trailing blank line mod. *) 
              IF LNewBlankLinesAfter # 0 
              THEN 
                MteTeWaitBlankLines ( LNewBlankLinesAfter , FmtNo ) 
              ; MteTeFlushBlankLines ( ) 
              (* No need to build a mark.  There will always be another
                 Nl to the left. *) 
              ; INC ( NewLinesCt ) 
              END (* IF *) 

            (* If necessary, insert the right text mod. *) 
            ; IF LNewModRef2 # NIL 
              THEN (* Insert the 2nd text mod. *) 
                MteTeWaitChild 
                  ( LNewModRef2 
                  , EstHs . EstChildKindSetModText 
                  , FmtNo 
                  , EstHs . EdgeKindTyp . EdgeKindLeadingMod  
                  )
              ; IF LNewModRef2 . ModTextLeftTokToPos = 0 (* Nl before *) 
                THEN (* This Nl is inserted, not the starting one. *) 
               (* NewBolTokMark (* Will always get overlaid. *) 
                    := Marks . TokMarkTyp 
                         { EstNodeNo := MarkNodeNo 
                           (* ^This could be the wrong value, but if
                               so, the whole mark will be overlaid. *) 
                         , Kind := MarkKindTyp . Plain 
                         , FmtNo := FmtNo 
                         , StartAtEnd := FALSE 
                         , IsImpliedNewLine := FALSE 
                         , Tok := LbeStd . TokModText 
                         } 
                ; *) 
                  INC ( NewLinesCt ) 
                END 
              ; MteState := MteStateTyp . MteStateBwdNl (* Could change. *)
              END (* IF *) 

            (* If necessary, insert the left text mod. *) 
            ; IF LNewModRef1 # NIL 
              THEN 
                MteTeWaitChild 
                  ( LNewModRef1 
                  , EstHs . EstChildKindSetModText 
                  , FmtNo 
                  , EstHs . EdgeKindTyp . EdgeKindLeadingMod  
                  ) 
              ; IF LNewModRef1 . ModTextLeftTokToPos = 0 (* Nl before *) 
                THEN (* This is not the inserted Nl. *)
                  NewBolTokMark (* Could get overlaid. *) 
                    := Marks . TokMarkTyp 
                         { EstNodeNo := MarkNodeNo 
                           (* ^This could be the wrong value, but if
                               so, the whole mark will be overlaid. *) 
                         , Kind := MarkKindTyp . Plain 
                         , FmtNo := FmtNo 
                         , StartAtEnd := FALSE 
                         , IsImpliedNewLine := FALSE 
                         , Tok := LbeStd . TokModText 
                         } 
                ; INC ( NewLinesCt ) 
                ; MteState := MteStateTyp . MteStateDone 
                ELSE  
                  MteState := MteStateTyp . MteStateBwdNl (* Could change. *)
                END (* IF *) 
              END 

            (* If necessary, insert the leading blank line mod. *) 
            ; IF LNewBlankLinesBefore # 0 
              THEN (* Insert leading blank line mod. *) 
                MteTeWaitBlankLines ( LNewBlankLinesBefore , FmtNo ) 
              ; MteTeFlushBlankLines ( ) 
              ; NewBolTokMark 
                  := Marks . TokMarkTyp 
                       { EstNodeNo := MarkNodeNo 
                       , Kind := MarkKindTyp . BlankLine 
                       , FmtNo := FmtNo 
                       , StartAtEnd := FALSE 
                       , IsImpliedNewLine := FALSE 
                       , Tok := LbeStd . TokBlankLine 
                       } 
              ; INC ( NewLinesCt ) 
              ; MteState := MteStateTyp . MteStateDone 
              (* This is the replacement for the starting Nl. *) 
              END (* IF *) 
            END (* IF *) 
          END 
        END MteTeFinishBwdEdit 

    ; PROCEDURE MteTeFinishMerge ( ) RAISES { AssertionFailure } 

      = VAR LNewEstRef : EstHs . EstRefTyp 

      ; BEGIN (* MteTeFinishMerge *) 
          IF MteTeEstTravInfo . EtiParentRef = NIL 
          THEN (* This is an Ast String node.  We never merge these 
                  at their own level, but postpone upward. *) 
          ELSE (* This is an interior Est node. *) 
            MteTeWaitChildrenToLeft ( ) 
          ; MteTeFlushExceptSlice 
              ( LangUtil . FmtNoNull , EstHs . EdgeKindTyp . EdgeKindEstChild ) 
          ; IF MteTeWaitingFromChildNo = 0 
               AND MteTeWaitingToChildNo = MteTeEstTravInfo . EtiChildCt 
               AND NOT MteTeMergeIsStarted 
            THEN (* The whole original subtree is waiting, so just reuse it. *)
            ELSE  
              IF MteTeInterestingChildrenExist 
              THEN 
                MteTeFlushSliceOrChild 
                  ( LangUtil . FmtNoNull 
                  , EstHs . EdgeKindTyp . EdgeKindEstChild 
                  )
              ; IF MteTeMergeIsStarted 
                THEN  
                  EstBuild . FinishMerge 
                    ( MteTeMergeState 
                    , ResultEstKind 
                        := MteTeEstTravInfo . EtiParentRef . EstKind 
                    , (* VAR *) ResultTreeRef := LNewEstRef 
                    ) 
                ; IF RootFsNodeRef . FsIsInsideList 
                     AND EstUtil . EstChildCt ( LNewEstRef ) = 1 
                     AND MteTeNILChildrenExist
                     AND NOT MteTeSibFmtNoMarkExists  
                  THEN (* Replace a list with a single NIL child by a NIL *) 
                    NewEstRef := NIL 
                  ELSE 
                    NewEstRef := LNewEstRef 
                  END (* IF *) 
                ELSE (* There were only insertion tokens.  This can happen
                        for a ModTok with no mods. Reuse the old tree. *) 
                END (* IF *) 
              ELSE (* There are only ModDels.  Replace this by a NIL *)  
                NewEstRef := NIL 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END MteTeFinishMerge 

    (* String moving procedures. *) 

    ; PROCEDURE MteTeMoveSubstring
        ( READONLY SourceString : Strings . StringTyp 
        ; ModTextStartSs : LbeStd . LimitedCharNoTyp 
        ) 
      RAISES { AssertionFailure } 

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

    ; PROCEDURE MteTeReverse ( ) RAISES { AssertionFailure } 

      (* Make transition into MteStateBwdEdit. *) 

      = VAR LPrefixLen : LbeStd . LimitedCharNoTyp 
      ; VAR LSuffixLen : LbeStd . LimitedCharNoTyp 

      ; BEGIN (* MteTeReverse *) 
          MteTeFwdBlanks ( DelToPos , ExtendTouchedRightward := TRUE )  
          (* ^Edited region can lie beyond right end of existing line. *)
        ; IF MteTouchedToPos = LbeStd . LimitedCharNoInfinity 
          THEN 
            MteTouchedToPos := MteCharPos 
          END (* IF *) 
        ; MteTeFlushCondFmt ( )  
        ; LPrefixLen := DelFromPos - MteTouchedFromPos 
        ; LSuffixLen := MteTouchedToPos - DelToPos 
        ; MteNewTextLen := LPrefixLen + InsLen + LSuffixLen 
        ; IF InsNlPos # LbeStd . LimitedCharNoInfinity 
          THEN 
            MteNewTextRelNlPos := InsNlPos - MteTouchedFromPos 
          END (* IF *) 
        ; MteTeMoveSubstring 
            ( Strings . Substring ( InsText , DelFromPos , InsLen ) 
            , ModTextStartSs := LPrefixLen 
            ) 
        ; MteNewTextPos := MteNewTextLen 
        ; MteState := MteStateTyp . MteStateBwdEdit 
(* 
        ; IF MteTeEstTravInfo . EtiChildNo >= MteTeEstTravInfo . EtiChildCt 
          THEN (* The only way this can happen is if the starting mark 
                  pointed to the EOI. *) 
            MteTeRightChildRootNodeNo 
              := EstUtil . EstNodeCt ( MteTeEstTravInfo . EtiParentRef ) 
(* CHECK: ^Is this right? *) 
          ELSE (* All this, just to get MteTeRightChildRootNodeNo set. *) 
(* TODO: Many calls on MteTeReverse do a MteTeDecEstChild, either before 
         or after the call.  Pull this function out, for only those cases
         that don't get MteTeRightChildRootNodeNo set. *) 
            MteTeIncEstChild ( ) 
          ; MteTeDecEstChild ( ) 
          END (* IF *) 
*) 
        ; MteTeBwdBlanks ( ) 
        END MteTeReverse 

    ; PROCEDURE MteTePassNl ( ) 

      = BEGIN
          MtePassedDelNl := TRUE 
        ; MteLineShift := DelNlShift 
        ; MteNlItemNo := MteItemCt 
        ; MteState := MteStateTyp . MteStatePassingNl 
        ; MteFwdPrevTok := LbeStd . TokBegOfLine 
        ; MteLastFmtNoOnLine := LangUtil . FmtNoUnknown  
        ; MteEstListChildrenToPass := 0 (* Dead *) 
        ; MteModTextIsToLeftOnLine := FALSE 
(* CHECK: Should we do anything with MteFwdPrevItemCouldHaveCondNlAfter? 
          If so, should be done at call sites.  NlBefore is one possibility. *) 
        END MteTePassNl 

    ; PROCEDURE MteTeReverseOrPassNlBefore ( ) 
      : BOOLEAN 
      RAISES { AssertionFailure } 
      (* Used for ordinary Fs tree line breaks too. *) 
      (* ^True if did reverse. *) 
      (* This FUNCTION has numerous SIDE EFFECTS. *) 
      (* PRE: MteState is not a backward state. *) 

      = BEGIN (* MteTeReverseOrPassNlBefore *) 
          IF MteState 
             IN MteStateSetTyp 
                  { MteStateTyp . MteStateStartAtBeg 
                  , MteStateTyp . MteStatePassingNl 
                  } 
          THEN 
            RETURN FALSE 
          ELSIF NOT MtePassedDelNl 
                AND DelNlShift # LbeStd . LimitedCharNoInfinity 
          THEN (* Pass this Nl. *) 
            MteTePassNl ( ) 
          (* Don't consume an Est child, callers don't want it. *) 
          ; RETURN FALSE 
          ELSE (* Reverse.  First wait Est children starting 
                  with the current one, and further to right. *) 
            Assert ( MteItemCt > 0 , AFT . A_MteTeReverseOrPassNlBeforeNotBeg ) 
          ; MteTeWaitChildrenToRight ( ) 
          ; MteTeDecEstChild ( )  
          ; MteTeReverse ( ) 
          ; RETURN TRUE 
          END (* IF *) 
        END MteTeReverseOrPassNlBefore 

    ; PROCEDURE MteTeReverseOrPassNlAfter ( ) 
      : BOOLEAN 
      (* ^True if did reverse. *) 
      RAISES { AssertionFailure } 
      (* This FUNCTION has numerous SIDE EFFECTS. *) 
      (* PRE: MteState is not a backward state. *) 
      (* We are at the new line after something.  See whether we 
         should reverse at this new line, and take care of either 
         reversing or passing the new line. *) 

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
             MteTeRightChildRootNodeNo set. 
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
      RAISES { AssertionFailure } 
      (* String of comment, text mod, or token. *) 

      = VAR LLength := SharedStrings . Length ( StringRef ) 

      ; BEGIN (* MteTeFwdString *) 
          IF LLength > 0 
          THEN 
            IF MteCharPos = DelToPos 
               (* ^The string begins immediately right of changed region. *)  
               AND Tok # LbeStd . TokModText 
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
               AND Tok # LbeStd . TokModText 
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
        END MteTeFwdString 

    ; PROCEDURE MteTeBwdEditTouchedString 
        ( StringRef : SharedStrings . T 
        ; BlanksLen : LbeStd . LimitedCharNoTyp := 0 
          (* If StringRef is NIL, instead use a string of blanks of 
             this length. (which can also be zero) *) 
        ) 
      RAISES { AssertionFailure } 
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
             do not let the blanks close it. *) 
        ; IsTrailingCmnt : BOOLEAN := FALSE 
        ) 

      = VAR LBlankCt : LbeStd . LimitedCharNoTyp 

      ; BEGIN (* MteTeFwdBlanks *) 
          IF BlanksToPos > MteCharPos 
          THEN (* At least one blank. *) 

          (* Check beginning/end of touched region. *) 
            IF MteFwdPrevTok = LbeStd . TokModText 
               (* The previous ModText will be touched, if nothing else 
                  untouched intervenes. This merges into one ModText. *)
               OR MteFwdPrevTok = LbeStd . TokBegOfLine 
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
          ; MteFwdPrevTok := LbeStd . TokSep 
          ELSE 
            LBlankCt := 0 
          END (* IF *) 

        (* Store inserted blank count. *) 
        ; MteBlanks [ MteBlankSs ] := LBlankCt 
        ; INC ( MteBlankSs ) 
        END MteTeFwdBlanks 

    ; PROCEDURE MteTeBwdBlanks ( ) RAISES { AssertionFailure } 
      (* Harmless, if we are already out of MteStateBwdEdit. *) 

      = VAR LBlanksLen : LbeStd . LimitedCharNoSignedTyp 

      ; BEGIN (* MteTeBwdBlanks *) 
          DEC ( MteBlankSs ) 
        ; LBlanksLen := MteBlanks [ MteBlankSs ] 
        ; DEC ( MteCharPos , LBlanksLen ) 
(* TODO: ^Something about the saturation arithmetic here. *) 
        ; MteTeBwdEditTouchedString ( NIL , LBlanksLen ) 
        END MteTeBwdBlanks 

    ; PROCEDURE MteTeSetIndentInfo 
        ( FsNodeRef : LangUtil . FsNodeRefTyp )  
      RAISES { AssertionFailure } 

      = BEGIN 
          MteTeIsFirstLine 
            := TravUtil . IsInFirstLine 
                 ( FsNodeRef , MteTeEstTravInfo , Bwd := FALSE ) 
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
      RAISES { AssertionFailure } 
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

          | MteStateTyp . MteStateBwdNl 
          , MteStateTyp . MteStateDone  
          => DEC ( MteCharPos , SharedStrings . Length ( StringRef ) ) 
(* TODO: ^Something about the saturation arithmetic here. *) 
          ; MteTeBwdBlanks ( ) 
          ; DEC ( MteItemCt ) 

          ELSE 
            CantHappen ( AFT . A_MteTeTokBadMteState ) 
          END (* CASE MteState *) 
        END MteTeTok 

    ; PROCEDURE MteTeAstString 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : LangUtil . FmtKindTyp 
        ) 
      RAISES { AssertionFailure } 

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
          (* Defer waiting children and MteTeFinishBwdEdit to next Est 
             level upward.  There are no EstChildren, so setting  
             MteTeRightChildRootNodeNo is unnecessary. 
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
             THEN 
               NewEstRef := NIL 
               (* ^Delete the Ast string from the new Est. *) 
          (* ELSE, when going backwards, implicitly 
                   include the Ast string in the new Est by no 
                   action. (It _is_ the new Est already.) *) 
             END (* IF *) 
          (* Defer MteTeFinishBwdEdit to next Est level upward. *) 
          ; IF MteCharPos < MteTouchedToPos 
            THEN 
              MaxTouchedNodeNo := MAX ( MaxTouchedNodeNo , RootAbsNodeNo ) 
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
      RAISES { AssertionFailure } 

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
      RAISES { AssertionFailure } 

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
            => IF LFsChildNo = 0 
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
                  OR LFsChildNo = 0 AND MteTeEstTravInfo . EtiChildNo < 0 
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
      RAISES { AssertionFailure } 

      (* Leading mods. *) 

      = PROCEDURE MteTeTfsModBlankLine1stBwd 
          ( <* UNUSED *> ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { AssertionFailure } 
        (* Backward processing inside a blank line that does not follow
           a deleted new line. MteItemCt does not include the Nl after. *) 

        = BEGIN (* MteTeTfsModBlankLine1stBwd *) 
            IF MteItemCt <= MteFinishItemNo  
            THEN (* Backward traverse ends at the Nl after. *)  
              MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                ) 
            ; IF MteState # MteStateTyp . MteStateDone  
              THEN 
                NewBolTokMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo 
                         := RootAbsNodeNo 
                            + MteTeEstTravInfo . EtiChildRootNodeNo 
                     , Kind := MarkKindTyp . BlankLine 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := TRUE 
                     , IsImpliedNewLine := FALSE 
                     , Tok := LbeStd . TokBlankLine 
                     } 
              ; INC ( NewLinesCt ) (* For the Nl at the end. *) 
              ; MteState := MteStateTyp . MteStateDone 
              END 
            ; IF MteItemCt > 0 
              THEN
                MteTeDecEstChild ( ) 
              END 
            ELSE (* Traverse backward into the blank line. *) 
              MteTeBwdBlanks ( ) (* Inside the deleted region. *)  
            ; MteTeBwdBlanks ( ) (* Left of the deleted region. *) 
            ; DEC ( MteItemCt ) 
            ; IF MteItemCt <= MteFinishItemNo 
              THEN (* Backward traverse could end at the Nl before. *) 
                MteTeFinishBwdEdit 
                  ( FsNodeRef . FsFmtNo 
                  , MarkNodeNo 
                      := RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                  ) 
              ; IF MteState # MteStateTyp . MteStateDone  
                THEN 
                  NewBolTokMark 
                  := Marks . TokMarkTyp 
                       { EstNodeNo 
                           := RootAbsNodeNo 
                              + MteTeEstTravInfo . EtiChildRootNodeNo 
                       , Kind := MarkKindTyp . BlankLine 
                       , FmtNo := FsNodeRef . FsFmtNo 
                       , StartAtEnd := FALSE  
                       , IsImpliedNewLine := FALSE 
                       , Tok := LbeStd . TokBlankLine 
                       } 
                ; INC ( NewLinesCt ) (* For the Nl at the end. *) 
                ; MteState := MteStateTyp . MteStateDone 
                END 
              ELSE 
                CantHappen ( AFT . A_MteTfsModBlankLine1stBwd_NotDone ) 
              END 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                       ) 
            ; MteTeDecEstChild ( ) 
            END (* IF *) 
          END MteTeTfsModBlankLine1stBwd 

      ; PROCEDURE MteTeTfsModBlankLine1stFwd 
          ( ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { AssertionFailure } 
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
              MteTeFinishBwdEdit 
                ( FmtNo := FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo 
                       + MteTeEstTravInfo . EtiChildRootNodeNo 
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
              (* No net change to EstChild, will revisit it in the new state. 
                 But we have to go through Inc/Dec, just to get 
                MteTeRightChildRootNodeNo set. 
              *) 
              ; MteTeIncEstChild ( ) 
              ; MteTeDecEstChild ( ) 
              ; MteTeFinishBwdEdit 
                  ( FmtNo := FsNodeRef . FsFmtNo 
                  , MarkNodeNo 
                      := RootAbsNodeNo 
                         + MteTeEstTravInfo . EtiChildRootNodeNo 
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
        RAISES { AssertionFailure } 
        (* A ModBlankLine that follows a deleted new line.  This implies
           it is adjacent to the touched region. *) 

        = BEGIN (* MteTeTfsModBlankLine2nd *) 
            MteTeFwdBlanks ( DelToPos , ExtendTouchedRightward := TRUE ) 
          ; MteTeWaitChildrenFartherToRight ( ) 
          ; INC ( MteBlankLinesAfter , ModBlankLine . ModBlankLineCt - 1 ) 
          ; MteTeReverse ( ) 
          ; MteTeFinishBwdEdit 
              ( FmtNo := FsNodeRef . FsFmtNo 
              , MarkNodeNo 
                  := RootAbsNodeNo 
                     + MteTeEstTravInfo . EtiChildRootNodeNo 
                    (* ^We are inside the mod and have taken it apart. *) 
              ) 
          ; MteTeBwdBlanks ( ) 
          ; MaxTouchedNodeNo 
              := MAX ( MaxTouchedNodeNo 
                     , RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                     ) 
          ; MteTeDecEstChild ( )  
          END MteTeTfsModBlankLine2nd 

      ; PROCEDURE MteTeTfsModBlankLine 
          ( ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* MteTeTfsModBlankLine *) 
            CASE MteState 
            OF MteStateTyp . MteStateStartAtEnd 
            => MteFinishItemNo := 1 
            ; MteTeSetIndentInfo ( FsNodeRef )  
            ; MteState := MteStateTyp . MteStatePassingNl 
            ; INC ( MteItemCt ) 
            ; MteTeIncEstChild ( ) 

            | MteStateTyp . MteStateStartAtBeg 
            => MteFinishItemNo := 0 
            ; MteTeSetIndentInfo ( FsNodeRef )  
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
                 THEN (* Touched region reaches here.  Pull the blank line
                         into the merge. *) 
                   MteTeWaitChildrenFartherToRight ( ) 
                 ; INC ( MteBlankLinesAfter , ModBlankLine . ModBlankLineCt ) 
                 ; TrailingBlankLinesIncluded := ModBlankLine . ModBlankLineCt 
                 ELSE 
                   MteTeWaitChildrenToRight ( ) 
                 END 
               ; MteTeReverse ( ) 
               ; MteTeFinishBwdEdit 
                   ( FmtNo := FsNodeRef . FsFmtNo 
                   , MarkNodeNo 
                       := RootAbsNodeNo 
                          + MteTeEstTravInfo . EtiChildRootNodeNo 
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
        RAISES { AssertionFailure } 

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
           and should stay as is. *) 
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
              MteTeBwdEditTouchedString 
                ( ModCmnt . ModCmntStringRef ) 
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                ) 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
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
                       , RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                       ) 
            END (* IF *) 

          (* Leading blanks of comment. *) 
          ; MteTeBwdBlanks ( ) 
          ; MteTeFinishBwdEdit 
              ( FsNodeRef . FsFmtNo 
              , MarkNodeNo 
                  := RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
              ) 
          ; DEC ( MteItemCt )
          ; IF MteItemCt <= 0 
            THEN (* Backward traverse ends at beginning of comment. *) 
              MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                ) 
            ; IF MteState # MteStateTyp . MteStateDone  
              THEN 
                IF ModCmnt . ModCmntNlAfter 
                THEN LTok := LbeStd . TokCmntAtEndOfLine 
                ELSE LTok := LbeStd . TokCmnt 
                END (* IF *) 
              ; NewBolTokMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo 
                         := RootAbsNodeNo 
                            + MteTeEstTravInfo . EtiChildRootNodeNo 
                     , Kind := MarkKindTyp . Plain 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := FALSE
                     , IsImpliedNewLine := NOT ModCmnt . ModCmntNlBefore 
                     , Tok := LTok  
                     } 
              ; INC ( NewLinesCt ) 
              ; MteState := MteStateTyp . MteStateDone 
              END 
            ELSIF ModCmnt . ModCmntNlBefore AND MteItemCt = MteNlItemNo 
            THEN MteLineShift := 0 
            END (* IF*) 
          ; MteTeDecEstChild ( ) 
          END MteTeTfsModCmntBwd 

      ; PROCEDURE MteTeTfsModCmntFwd ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { AssertionFailure } 

        = VAR LFromPos : LbeStd . LimitedCharNoTyp 
        ; VAR LIsTrailingCmnt : BOOLEAN 
        ; VAR LIsNonLeftmostLeadingCmnt : BOOLEAN 

        ; BEGIN (* MteTeTfsModCmntFwd *) 

          (* Handle leading blanks of comment. *) 
            TYPECASE ModCmnt 
            OF ModHs . ModCmntLeadingFixedTyp 
            => LFromPos 
                 := EstUtil . WidthSum 
                      ( ModCmnt . ModCmntFromPos , MteLineShift ) 
            ; LIsTrailingCmnt := FALSE 
            ; LIsNonLeftmostLeadingCmnt := FALSE 
            | ModHs . ModCmntTrailingFixedTyp 
            => LFromPos 
                 := EstUtil . WidthSum 
                      ( ModCmnt . ModCmntFromPos , MteLineShift ) 
            ; LIsTrailingCmnt := TRUE  
            ; LIsNonLeftmostLeadingCmnt := FALSE 
            | ModHs . ModCmntLeadingRelativeTyp 
            => LFromPos 
                 := EstUtil . WidthSum3 
                      ( TravUtil . IndentPos 
                          ( Lang 
                          , MteTeIndentPos 
                          , FsFmtKind 
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
                 := EstUtil . WidthSum 
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
                            change, so touch it. *) 
                     OR LIsNonLeftmostLeadingCmnt AND MteSuffixShift # 0 
                        (* ^ Once, I tried avoiding this touching of a
                           leading, relative, comment when preceeded
                           by changes in the number of characters to
                           the left, by making this kind of comment be
                           relative to the preceeding item, instead of
                           relative to the indent position.  But,
                           alas, the relativity is different for a
                           preceeding comment (intervening blanks have
                           to be counted in the relative position) and
                           a preceeding ModText (intervening blanks
                           are already provided for by the ModText).
                           Since the one can change to the other, this
                           would have entailed even more complexity,
                           so I just abandoned the idea.
                        *) 
              , IsTrailingCmnt := LIsTrailingCmnt 
              ) 

          (* Continue with text of comment. *) 
          ; MteTeFwdString 
              ( LbeStd . TokCmnt 
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
                  MteFwdPrevTok := LbeStd . TokCmntAtEndOfLine  
                ; MteState := MteStateTyp . MteStateRightNlFound 
                END (* IF *)  
              ; INC ( MteItemCt ) (* Count the Nl after. *) 
              ; MteFwdPrevItemCouldHaveCondNlAfter := TRUE 
              ; MteTeIncEstChild ( ) 
              ELSE 
                IF MteTeReverseOrPassNlAfter ( ) (* Has SIDE EFFECTS *) 
                THEN (* We reversed. *) 
                  MteTeFinishBwdEdit 
                    ( FmtNo := FsNodeRef . FsFmtNo 
                    , MarkNodeNo := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                    ) 
                ; MteTeTfsModCmntBwd ( ModCmnt ) 
                ELSE 
                  INC ( MteItemCt ) (* Count the Nl after. *) 
                ; MteTeIncEstChild ( ) 
                ; MteFwdPrevTok := LbeStd . TokCmntAtEndOfLine  
                ; MteFwdPrevItemCouldHaveCondNlAfter := TRUE 
                END (* IF *) 
              END (* IF *) 
            ELSE 
              MteTeIncEstChild ( ) 
            ; MteFwdPrevTok := LbeStd . TokCmnt 
            ; MteFwdPrevItemCouldHaveCondNlAfter := TRUE 
            END (* IF *) 
          END MteTeTfsModCmntFwd

      ; PROCEDURE MteTeTfsModCmnt ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { AssertionFailure } 

        = VAR LNlBefore : BOOLEAN 
   
        ; BEGIN (* MteTeTfsModCmnt *) 
            CASE MteState 
            OF MteStateTyp . MteStateStartAtEnd 
            => Assert 
                 ( ModCmnt . ModCmntNlAfter 
                 , AFT . A_MteTeTfsModCmntBadStartAtEndState 
                 ) 
            ; MteFinishItemNo := 1 
            ; INC ( MteItemCt ) (* Count the Nl after. *) 
            ; MteTeSetIndentInfo ( FsNodeRef )  
            ; MteState := MteStateTyp . MteStatePassingNl  
            ; MteTeIncEstChild ( ) 

            | MteStateTyp . MteStateStartAtBeg 
            => MteFinishItemNo := 0 
            ; INC ( MteItemCt ) (* Count the comment. *) 
            ; MteTeSetIndentInfo ( FsNodeRef )  
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
               THEN LNlBefore := TRUE 
               ELSE
                 TYPECASE ModCmnt 
                 OF ModHs . ModCmntLeadingFixedTyp 
                 , ModHs . ModCmntTrailingFixedTyp 
                 => LNlBefore := ModCmnt . ModCmntFromPos < MteCharPos 
                    (* ^Implicit new line. *) 
                 ELSE LNlBefore := FALSE 
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
                 MteTeFinishBwdEdit 
                   ( FmtNo := FsNodeRef . FsFmtNo 
                   , MarkNodeNo 
                       := RootAbsNodeNo 
                          + MteTeEstTravInfo . EtiChildRootNodeNo 
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
                     MteTeFinishBwdEdit 
                       ( FsNodeRef . FsFmtNo 
                       , MarkNodeNo 
                           := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                       ) 
                   ; IF MteState # MteStateTyp . MteStateDone  
                     THEN 
                       NewBolTokMark 
                         := Marks . TokMarkTyp 
                              { EstNodeNo 
                                  := RootAbsNodeNo 
                                     + MteTeEstTravInfo . EtiChildRootNodeNo 
                              , Kind := MarkKindTyp . Plain 
                              , FmtNo := FsNodeRef . FsFmtNo 
                              , StartAtEnd := TRUE 
                              , IsImpliedNewLine := FALSE 
                              , Tok := LbeStd . TokCmntAtEndOfLine 
                              } 
                     ; INC ( NewLinesCt ) 
                     ; MteState := MteStateTyp . MteStateDone 
                     END 
                 ELSE 
                   MteTeTfsModCmntBwd ( ModCmnt ) 
                 END (* IF *) 
               ELSE 
                 MteTeTfsModCmntBwd ( ModCmnt ) 
               END (* IF *) 

            END (* CASE MteState *) 
          END MteTeTfsModCmnt 

      ; PROCEDURE MteTeTfsModTextBwd ( ModText : ModHs . ModTextTyp ) 
        RAISES { AssertionFailure } 
        (* Does not handle Nl after, nor trailing blanks. These are
           handled at various call sites, in different ways. *) 

        = VAR LStringToPos : LbeStd . LimitedCharNoSignedTyp 
        ; VAR LLength : SharedStrings . LengthTyp 
        ; VAR LNewModTextRef : ModHs . ModTextTyp 

        ; BEGIN (* MteTeTfsModTextBwd *) 
            LStringToPos := MteCharPos 
          ; LLength := SharedStrings . Length ( ModText . ModTextStringRef ) 
          ; DEC ( MteCharPos , LLength ) 
(* TODO: ^Something about the saturation arithmetic here. *) 
          ; IF MteTouchedToPos 
               < ModText . ModTextLeftTokToPos + MteLineShift 
            THEN (* Entire mod is to right of touched region. *) 
              IF MteModShift # 0 
              THEN (* Shift positions in the mod. *) 
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
              ELSE 
                MteTeWaitExistingEstChild 
                  ( EstHs . EdgeKindTyp . EdgeKindLeadingMod 
                  , IsInteresting := TRUE 
                  ) 
              END (* IF MteModShift # 0 *) 
            ; MteNewTextToPos 
                := EstUtil . WidthSum ( MteCharPos , MteSuffixShift ) 
            ELSIF MteTouchedFromPos <= MteCharPos 
            THEN (* Entire string is inside touched region. *) 
              MteTeBwdEditTouchedString ( ModText . ModTextStringRef ) 
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                ) 
            ; MaxTouchedNodeNo 
                := MAX ( MaxTouchedNodeNo 
                       , RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                       ) 
            END 
          ; IF LStringToPos < MteTouchedFromPos 
            THEN (* Entire string, possibly empty, lies left of touched 
                    region. *) 
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
                       , RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                       ) 
            END (* IF *) 

          (* Leading blanks of the ModText. *) 
          ; MteTeBwdBlanks ( ) 
          ; DEC ( MteItemCt ) 
          ; MteTeFinishBwdEdit 
              ( FsNodeRef . FsFmtNo 
              , MarkNodeNo 
                  := RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
              ) 
          ; IF MteItemCt <= 0 
            THEN (* Backward traverse ends at beginning of the ModText. *) 
              Assert
                ( ModText . ModTextLeftTokToPos = 0 (* Nl before *) 
                , AFT . A_MteTeTfsModCmntBwd_EndNoNewLine 
                ) 
         (* ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                ) 
         *) 
            ; IF MteState # MteStateTyp . MteStateDone  
              THEN 
                NewBolTokMark 
                  := Marks . TokMarkTyp 
                       { EstNodeNo 
                           := RootAbsNodeNo 
                              + MteTeEstTravInfo . EtiChildRootNodeNo 
                       , Kind := MarkKindTyp . Plain 
                       , FmtNo := FsNodeRef . FsFmtNo 
                       , StartAtEnd := FALSE 
                       , IsImpliedNewLine := FALSE 
                       , Tok := LbeStd . TokModText 
                       } 
              ; INC ( NewLinesCt ) 
              ; MteState := MteStateTyp . MteStateDone 
              END (* IF*) 
            ELSIF ModText . ModTextLeftTokToPos = 0 (* Nl before *) 
                  AND MteItemCt = MteNlItemNo 
            THEN MteLineShift := 0 
            END 
          ; MteTeDecEstChild ( ) 
          END MteTeTfsModTextBwd 

      ; PROCEDURE MteTeTfsModTextFwd ( ModText : ModHs . ModTextTyp ) 
        RAISES { AssertionFailure } 

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
              ( LbeStd . TokModText 
              , ModText . ModTextStringRef 
              , IsTrailingCmnt := FALSE 
              ) 
          ; MteFwdPrevTok := LbeStd . TokModText (* Do even if text is empty. *) 
          ; MteModTextIsToLeftOnLine := TRUE 
          ; MteFwdPrevItemCouldHaveCondNlAfter := TRUE 
          ; IF ModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
            THEN (* Nl After *) 
              IF MteTeReverseOrPassNlAfter ( ) (* Has SIDE EFFECTS. *) 
              THEN (* We reversed at the end. Reprocess it backwards. *) 
                MteTeFinishBwdEdit 
                  ( FmtNo := FsNodeRef . FsFmtNo 
                  , MarkNodeNo 
                      := RootAbsNodeNo + MteTeRightChildRootNodeNo 
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
            ; MteFwdPrevTok := LbeStd . TokModText 
            ; MteTeFwdBlanks 
                ( ModText . ModTextToPos + MteLineShift 
                , ExtendTouchedRightward := TRUE 
                ) 
            ; MteTeIncEstChild ( ) 
            END (* IF *) 
          END MteTeTfsModTextFwd 

      ; PROCEDURE MteTeTfsModText ( ModText : ModHs . ModTextTyp ) 
        RAISES { AssertionFailure } 

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
            ; MteTeSetIndentInfo ( FsNodeRef )  
            ; MteState := MteStateTyp . MteStatePassingNl 
            ; MteTeIncEstChild ( ) 

            | MteStateTyp . MteStateStartAtBeg 
            => MteFinishItemNo := 0 
            ; INC ( MteItemCt ) (* Count the ModText. *) 
            ; MteTeSetIndentInfo ( FsNodeRef )  
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
                 MteTeFinishBwdEdit 
                   ( FmtNo := FsNodeRef . FsFmtNo 
                   , MarkNodeNo 
                       := RootAbsNodeNo 
                          + MteTeEstTravInfo . EtiChildRootNodeNo 
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
                     MteTeFinishBwdEdit 
                       ( FsNodeRef . FsFmtNo 
                       , MarkNodeNo 
                           := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                       ) 
                   ; IF MteState # MteStateTyp . MteStateDone  
                     THEN 
                       NewBolTokMark 
                         := Marks . TokMarkTyp 
                              { EstNodeNo 
                                  := RootAbsNodeNo 
                                     + MteTeEstTravInfo . EtiChildRootNodeNo 
                              , Kind := MarkKindTyp . Plain 
                              , FmtNo := FsNodeRef . FsFmtNo 
                              , StartAtEnd := TRUE 
                              , IsImpliedNewLine := FALSE 
                              , Tok := LbeStd . TokModText 
                              } 
                     ; INC ( NewLinesCt ) 
                     ; MteState := MteStateTyp . MteStateDone 
                     END
                 ELSE (* Handle the trailing blanks here. *) 
                   MteTeBwdBlanks ( ) 
                 ; MteTeFinishBwdEdit 
                     ( FsNodeRef . FsFmtNo 
                     , MarkNodeNo 
                         := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                     ) 
(* FIX: May have to adjust ModTextToPos. *) 
                 ; MteTeTfsModTextBwd ( ModText ) 
                 END (* IF *) 
               ELSE (* Handle the trailing blanks here. *) 
                 MteTeBwdBlanks ( ) 
               ; MteTeFinishBwdEdit 
                   ( FsNodeRef . FsFmtNo 
                   , MarkNodeNo 
                       := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                   ) 
(* FIX: May have to adjust ModTextToPos. *) 
               ; MteTeTfsModTextBwd ( ModText ) 
               END (* IF *) 

            END (* CASE MteState *) 
          END MteTeTfsModText 

      ; PROCEDURE MteTeTfsLexErrChars ( String : SharedStrings . T ) 
        RAISES { AssertionFailure } 

        = VAR LDoExcludeIfBwd : BOOLEAN 
        ; VAR LTok : LbeStd . TokTyp 
 
        ; BEGIN (* MteTeTfsLexErrChars *) 
            CASE MteState <* NOWARN *> 
            OF MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateFwd 
            => MteTeTok 
                 ( SharedStrings . Tok ( String ) 
                 , String 
                 , FsNodeRef 
                 , FsFmtKind 
                 , (* VAR *) DoExcludeIfBwd := LDoExcludeIfBwd 
                 )  
            ; MteState := MteStateTyp . MteStateFwd 
            ; MteFwdPrevItemCouldHaveCondNlAfter := FALSE 
            ; MteTeIncEstChild ( ) 

            | MteStateTyp . MteStateRightNlFound 
            => MteTeWaitChildrenToRight ( )  
            ; MteTeReverse ( ) 
            ; MteTeDecEstChild ( ) 
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                ) 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => LTok := SharedStrings . Tok ( String ) 
            ; MteTeTok 
                 ( LTok 
                 , String 
                 , FsNodeRef 
                 , FsFmtKind 
                 , (* VAR *) DoExcludeIfBwd := LDoExcludeIfBwd 
                 )  
            ; IF NOT LDoExcludeIfBwd 
              THEN 
                 MteTeWaitExistingEstChild 
                   ( EstHs . EdgeKindTyp . EdgeKindEstChild  
                   , IsInteresting := TRUE 
                   ) 
               END (* IF *) 
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                ) 
            ; IF MteItemCt <= 0 
(* CHECK ^In case LexErrChars could be implicitly the first item on a line. 
          Can this happen? *) 
              THEN
                IF MteState # MteStateTyp . MteStateDone  
                THEN 
                  NewBolTokMark 
                    := Marks . TokMarkTyp 
                         { EstNodeNo 
                             := RootAbsNodeNo 
                                + MteTeEstTravInfo . EtiChildRootNodeNo 
                         , Kind := MarkKindTyp . Plain 
                         , FmtNo := FsNodeRef . FsFmtNo 
                         , StartAtEnd := FALSE 
                         , IsImpliedNewLine := FALSE 
                         , Tok := LTok 
                         } 
                ; INC ( NewLinesCt ) 
                ; MteState := MteStateTyp . MteStateDone 
                END
              END (* IF *) 
            ; MteTeDecEstChild ( ) 
            END (* CASE MteState *) 
          END MteTeTfsLexErrChars  

      ; PROCEDURE MteTeTfsModErr ( <* UNUSED *> ModErr : ModHs . ModErrTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* MteTeTfsModErr *) 
            CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
(* CHECK: How could we start at the beginning of one of these? *) 
            => MteFinishItemNo := 0 
            ; INC ( MteItemCt ) 
            ; MteTeSetIndentInfo ( FsNodeRef )  
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
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                ) 
            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => IF MteCharPos >= MteTouchedToPos 
                  OR MteState = MteStateTyp . MteStateBwdNl 
               THEN (* We are outside the touched region. *) 
                 MteTeWaitExistingEstChild 
                   ( EstHs . EdgeKindTyp . EdgeKindLeadingMod 
                   , IsInteresting := TRUE 
                   ) 
                 (* ^Keep the error mod. *) 
               END (* IF *) 
            ; DEC ( MteItemCt ) 
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                ) 
            ; IF MteItemCt <= 0 
(* CHECK ^In case error could be implicitly the first item on a line. 
          Can this happen? *) 
              THEN
                IF MteState # MteStateTyp . MteStateDone  
                THEN 
                  NewBolTokMark 
                    := Marks . TokMarkTyp 
                         { EstNodeNo 
                             := RootAbsNodeNo 
                                + MteTeEstTravInfo . EtiChildRootNodeNo 
                         , Kind := MarkKindTyp . Plain 
                         , FmtNo := FsNodeRef . FsFmtNo 
                         , StartAtEnd := FALSE 
                         , IsImpliedNewLine := FALSE 
                         , Tok := LbeStd . TokNull  
                         } 
                ; INC ( NewLinesCt ) 
                ; MteState := MteStateTyp . MteStateDone 
                END
              END (* IF *) 
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
        RAISES { AssertionFailure } 
        (* DOES handle ModTok. *) 

        = VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR LChildFmtKind : LangUtil . FmtKindTyp 
        ; VAR LNewEstRef : LbeStd . EstRootTyp 
        ; VAR LEstDummyTokRef : ModHs . EstDummyTokTyp 
        ; VAR LChildIndentPos1 : LbeStd . LimitedCharNoTyp 
        ; VAR LChildIndentPosN : LbeStd . LimitedCharNoTyp 
        ; VAR LEstChildKindSet : EstHs . EstChildKindSetTyp 
        ; VAR LWasGoingFwd : BOOLEAN := FALSE 

        ; BEGIN (* MteTeTfsEstSubtree *) 
            LChildFsNodeRef 
              := EstUtil . FsRule 
                   ( Lang , MteTeEstTravInfo . EtiChildLeafElem . LeChildRef ) 
          ; CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 
            => LWasGoingFwd := TRUE 
            ; MteTeSetIndentInfo ( FsNodeRef )
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
                  , IsListFirstLine := MteTeIsFirstLine 
                  ) 
              ; LChildFmtKind 
                  := TravUtil . FmtKindForEstDescending  
                       ( FsKind := LChildFsNodeRef . FsKind 
                       , ParentFmtKind := FsFmtKind 
                       , FirstLineIndentPos := LChildIndentPos1 
                       , EstRef 
                           := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       , StartMark := StartTokMark 
                       , StartMarkIsKnownLineBreak := TRUE 
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
                  , IsListFirstLine := MteTeIsFirstLine 
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
            => (* At this point, we have not yet backed up past the 
                  separator, if any.  This will use MteCharPos 
                  without any separator, which is right for this case. *) 
              MteTeFlushDels ( ) 
            ; MteTeFlushBlankLines ( ) 
              (* Waiting things not yet closed will be safe because they 
                 use variables local to MergeTextEdit *) 
(* CHECK: ^Huh? *) 
            ; LWasGoingFwd := FALSE 
            ; LChildFmtKind := LangUtil . FmtKindTyp . FmtKindUnknown 
            ; LChildIndentPos1 := LbeStd . LimitedCharNoUnknown 
            ; LChildIndentPosN := LbeStd . LimitedCharNoUnknown 

            END (* CASE *) 
          ; MteTraverseEst 
              ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              , RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
              , LChildFsNodeRef 
              , LChildFmtKind 
              , LChildIndentPos1 
              , LChildIndentPosN 
              , (* VAR *) NewEstRef := LNewEstRef
              , (* VAR *) LMCharPos := LMCharPos  
              ) 
          ; CASE MteState 
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
              THEN 
                IF LNewEstRef # NIL 
                THEN 
                  TYPECASE LNewEstRef 
                  OF EstHs . EstRefTyp ( TEstRef ) 
                  => IF TEstRef . EstKind = EstHs . EstKindTyp . EstKindModTok 
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
                    , ( NARROW ( LNewEstRef , EstHs . EstRefTyp ) 
                        . EstChildKindSet 
                        * EstHs . EstChildKindSetCopyUp 
                      ) 
                      + LEstChildKindSet 
                      + EstHs . EstChildKindSetContainsSyntMod 
                    , FsNodeRef . FsFmtNo 
                    , EstHs . EdgeKindTyp . EdgeKindEstChild   
                    ) 
                ELSIF FsNodeRef . FsIsInsideCondFmt AND NOT IsModTok 
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
                ELSIF NOT IsModTok 
                      AND FsNodeRef . FsIsInsideList 
                THEN (* In case of an Est list, there could be mods 
                        attached by format number to this list element 
                        or its separators, even though the element has 
                        disappeared, so merge a NIL just in case. *) 
                  MteTeWaitChild 
                    ( LNewEstRef 
                    , EstHs . EstChildKindSetEstChild 
                      + EstHs . EstChildKindSetContainsSyntMod 
                    , FsNodeRef . FsFmtNo 
                    , EstHs . EdgeKindTyp . EdgeKindEstChild   
                    ) 
                ELSE (* Omit the old child. *) 
                END (* IF *) 
              ELSE 
                MteTeWaitExistingEstChild 
                  ( EstHs . EdgeKindTyp . EdgeKindEstChild 
                  , IsInteresting := TRUE 
                  ) 
              END (* IF *) 
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                ) 
            ; MteTeDecEstChild ( ) 
            ELSE 
              CantHappen ( AFT . A_MteTeTfsEstSubtree_BadState ) 
            END (* CASE MteState *) 
          END MteTeTfsEstSubtree 

      ; PROCEDURE MteTeTfsLeadingModsNoDel ( ) RAISES { AssertionFailure } 
        (* Works in either direction. Does not handle a delete mod. *) 

        = VAR LLMCharPos : LbeStd . LimitedCharNoTyp 

        ; BEGIN (* MteTeTfsLeadingModsNoDel *) 
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
              => IF TEstRef . EstKind = EstHs . EstKindTyp . EstKindModTok 
                 THEN 
                   MteTeTfsEstSubtree 
                     ( IsModTok := TRUE  
                     , (* VAR *) LMCharPos := LLMCharPos (* Ignore *) 
                     ) 
                 ELSE (* Not a mod *) 
                   EXIT 
                 END (* IF *) 

              (* Lex error characters *) 
              | SharedStrings . T ( TString ) 
              => IF SharedStrings . Tok ( TString ) = LbeStd . TokLexErrChars 
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

      ; PROCEDURE MteTeTfsTrailingMods ( ) RAISES { AssertionFailure } 
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
        RAISES { AssertionFailure } 

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
                    OR FsNodeRef . FsFmtNo # LangUtil . FmtNoListEstChild
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
        RAISES { AssertionFailure } 

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
                    OR FsNodeRef . FsFmtNo # LangUtil . FmtNoListEstChild
                  , AFT . A_MteTeTfsBwdLeadingDel_Deleted_Est_List_Child 
                  ) 
              ; IF FsNodeRef . FsFmtNo <= TModDel . ModDelThruFmtNo 
                THEN (* The delete mod applies to this token. *) 
                  Delete := TRUE 
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
        ; VAR LEstNodeNo : LbeStd . EstNodeNoTyp 

        ; BEGIN 
            IF MteTeEstChildrenAreToRightInNewEst ( ) 
            THEN (* This Est node will have children. *) 
              LMarkKind := MarkKindTyp . LeftSibFmtNo 
            ; LEstNodeNo := RootAbsNodeNo + MteTeRightChildRootNodeNo 
              (* ^Tricky.  This will also be the right node no in the
                 yet-to-be-constructed new Est node. *)  
            ; MteTeSibFmtNoMarkExists := TRUE 
            ELSIF 0 <= MteTeEstTravInfo . EtiChildNo  
                  AND MteTeEstTravInfo . EtiChildNo 
                      < MteTeEstTravInfo . EtiChildCt 
            THEN (* There will children to the left. *) 
              LMarkKind := MarkKindTyp . RightSibFmtNo 
            ; LEstNodeNo 
                := RootAbsNodeNo 
                     + MteTeEstTravInfo . EtiChildRootNodeNo 
              (* ^Tricky.  This will also be the right node no in the
                 yet-to-be-constructed new Est node. *)  
            ; MteTeSibFmtNoMarkExists := TRUE 
            ELSE 
              LEstNodeNo := RootAbsNodeNo 
            ; LMarkKind := MarkKindTyp . ChildFmtNo 
            END (* IF *) 
          ; NewBolTokMark 
              := Marks . TokMarkTyp 
                   { EstNodeNo := LEstNodeNo 
                   , Kind := LMarkKind
                   , FmtNo := FsNodeRef . FsFmtNo 
                   , StartAtEnd := FALSE 
                   , Tok := MteTeEstTravInfo . EtiParentRef . EstTok 
                   } 
          ; INC ( NewLinesCt ) 
          ; MteState := MteStateTyp . MteStateDone 
          END MteTeTfsBuildFmtNoMark 

      (* Format Syntax Trees: *) 

      ; PROCEDURE MteTeTfsBegOfImage ( ) RAISES { AssertionFailure } 

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
                  ( RootAbsNodeNo = 0 
                  , AFT . A_MteTeTfsBegOfImageNotZeroNodeNo 
                  ) 
              ; MteTeFinishBwdEdit 
                  ( FsNodeRef . FsFmtNo , MarkNodeNo := RootAbsNodeNo + 1 ) 
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

      ; PROCEDURE MteTeTfsEndOfImage ( ) RAISES { AssertionFailure } 
        (* Leading mods already done and not deleted. *) 

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
            ; MteTeReverse ( ) 
            ; MteTeFinishBwdEdit 
                ( FmtNo := FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo 
                       + MteTeEstTravInfo . EtiChildRootNodeNo 
                ) 
            ; MteTeBwdBlanks ( ) 
            ; MteTeBwdBlanks ( ) 
            ; DEC ( MteItemCt ) 
            ; Assert ( MteItemCt <= 0 , AFT . A_MteTeTfsEndOfImageNotDone ) 
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo , MarkNodeNo := MteEstNodeCt ) 
            ; Assert
                ( MteState = MteStateTyp . MteStateDone 
                , AFT . A_MteTeTfsEndOfImageNotDone  
                ) 
            ; MteTeDecEstChild ( ) 

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
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo , MarkNodeNo := MteEstNodeCt ) 
            ; MteTeDecEstChild ( )  

            ELSE 
              CantHappen ( AFT . A_MteTeTfsEndOfImageBadState ) 
            END (* CASE *) 
          END MteTeTfsEndOfImage 

      ; PROCEDURE MteTeTfsInsTok ( IsRepair : BOOLEAN ) 
        RAISES { AssertionFailure } 
        (* Leading mods already done and not deleted. *) 

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
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                ) 
            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
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
            ; MteTeFinishBwdEdit 
                ( FsNodeRef . FsFmtNo 
                , MarkNodeNo 
                    := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                ) 
            ELSE 
              CantHappen ( AFT . A_MteTeTfsInsTokBadMteState ) 
            END (* CASE MteState *) 
          END MteTeTfsInsTok 

      ; PROCEDURE MteTeTfsLineBreak 
          ( FsKind : LangUtil . FsKindTyp ) 
        RAISES { AssertionFailure } 
        (* Leading mods already done and not deleted. *) 

        = BEGIN (* MteTeTfsLineBreak *) 
            IF MteState IN MteStateSetStart
               AND StartTokMark . Kind IN Marks . MarkKindSetEstLeaf 
            THEN (* Ignore the LineBreak and move on to its trailing mods,
                    staying in the start state. 
                 *) 
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
                   ( MteFwdPrevTok # LbeStd . TokBegOfLine 
                   , AFT . A_MteTeTfsLineBreakFwdAtBOL 
                   ) 
              ; IF FsKind = LangUtil . FsKindTyp . FsKindLineBreakReqd 
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
                    MteTeFinishBwdEdit
                      ( FmtNo := FsNodeRef . FsFmtNo 
                      , MarkNodeNo 
                          := RootAbsNodeNo + MteTeRightChildRootNodeNo 
                      ) 
                  ELSE (* Passing this line break forwards. *) 
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
                  END
                ELSIF MteItemCt = MteNlItemNo 
                THEN (* This is the Line break we passed. *) 
                  MteTeIncludeInModDel ( FsNodeRef . FsFmtNo )
                ; MteLineShift := 0 
                ELSIF FALSE AND MteState IN MteStateSetBwdFinished  
                THEN (* Leave it alone, because whatever caused it not to be
                        taken will happen in the new tree too. *) 
                     (* This is not necessarily so.  If we are in Fill FmtKind,
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
        not be necessary. *)
                  THEN 
                    MteTeIncludeInModDel ( FsNodeRef . FsFmtNo ) 
                  ELSE 
                    MteTeFlushDels ( ) (* Ensure it is in new Est. *) 
                  END (* IF *) 
                END (* IF *) 
              ELSE 
                CantHappen ( AFT . A_MteTeTfsLineBreakBadState ) 
              END (* CASE *) 
            END (* IF *) 
          END MteTeTfsLineBreak 

      ; PROCEDURE MteTeTfsEstChild ( IsEstList : BOOLEAN ) 
        RAISES { AssertionFailure } 
        (* Only for an Fs-prescribed Est child. *) 

        = VAR LLMCharPos : LbeStd . LimitedCharNoTyp 

        ; BEGIN (* MteTeTfsEstChild *) 
            CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 
            => IF RootAbsNodeNo 
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
                  ELSE 
                    MteTeTfsEstSubtree 
                      ( IsModTok := FALSE  
                      , (* VAR *) LMCharPos := LLMCharPos (* Ignore *) 
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
            => IF MteTeEstTravInfo . EtiChildNo 
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
                ELSE 
                  MteTeTfsEstSubtree 
                    ( IsModTok := FALSE , (* VAR *) LMCharPos := LLMCharPos )  
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
            => IF 0 > MteTeEstTravInfo . EtiChildNo 
                  OR MteTeEstTravInfo . EtiChildFmtNo # FsNodeRef . FsFmtNo 
                  OR NOT EstHs . EstChildKindEstChild 
                         IN MteTeEstTravInfo . EtiChildLeafElem . LeKindSet 
               THEN (* No Est child exists. *) 
               ELSIF ISTYPE 
                       ( MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       , ModHs . EstDummyTyp 
                       ) (* Including NIL. *) 
               THEN (* NIL or dummy Est child. *) 
                 MteTeWaitExistingEstChild 
                   ( EstHs . EdgeKindTyp . EdgeKindEstChild 
                   , IsInteresting := FALSE 
                   ) 
               ; MteTeDecEstChild ( ) 
               ELSE 
                 MteTeTfsEstSubtree 
                   ( IsModTok := FALSE  
                   , (* VAR *) LMCharPos := LLMCharPos (* Ignore *) 
                   ) 
               END (* IF *) 
            END (* CASE *) 
          END MteTeTfsEstChild 

      ; PROCEDURE MteTeTfsFsSubtree ( ) 
        RAISES { AssertionFailure } 

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
                     , FsNodeRef 
                     , FsFmtKind 
                     , MteTeEstTravInfo 
                     , StartMark := StartTokMark 
                     , StartMarkIsKnownLineBreak := TRUE 
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
        RAISES { AssertionFailure } 

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
          ( RootAbsNodeNo : LbeStd . EstNodeNoTyp 
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
              => IF TEstRef . EstKind = EstHs . EstKindTyp . EstKindModTok 
                 THEN 
                   CASE StartTokMark . Kind <* NOWARN *> 
                   OF MarkKindTyp . LeftSibFmtNo 
                   , MarkKindTyp . RightSibFmtNo 
                   => IF StartTokMark . EstNodeNo 
                         > RootAbsNodeNo + EstTravInfo . EtiChildRootNodeNo 
                      THEN RETURN WhatNextTyp . LeadingMods 
                      ELSE RETURN WhatNextTyp . FsItem 
                      END (* IF *) 
                   | MarkKindTyp . ChildFmtNo 
                   , MarkKindTyp . Plain 
                   , MarkKindTyp . BlankLine 
                   => IF StartTokMark . EstNodeNo 
                         >= RootAbsNodeNo + EstTravInfo . EtiChildRootNodeNo 
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
              | ModHs . ModRefTyp 
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
                    = LbeStd . TokLexErrChars 
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
            CASE FsNodeRef . FsKind 

            (* FsKind cases which can have leading and/or trailing mods 
               of some kind. *) 
            OF LangUtil . FsKindTyp . FsKindBegOfImage 
            , LangUtil . FsKindTyp . FsKindEndOfImage 
            , LangUtil . FsKindTyp . FsKindInsTok 
            , LangUtil . FsKindTyp . FsKindEstChildOfFixed 
            , LangUtil . FsKindTyp . FsKindEstChildOfList 
            , LangUtil . FsKindTyp . FsKindLineBreakOpt 
            , LangUtil . FsKindTyp . FsKindLineBreakReqd 
            => CASE MteState 
               OF MteStateTyp . MteStateStartAtBeg 
               , MteStateTyp . MteStateStartAtEnd  
               => LWhatNext 
                    := MteTeTfsWhatNextForDescend 
                         ( RootAbsNodeNo , MteTeEstTravInfo , StartTokMark ) 
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
            ; LOOP (* while sashaying between leading mods, the real Fs item, 
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
                     OF LangUtil . FsKindTyp . FsKindBegOfImage 
                     => MteTeTfsBegOfImage ( ) 
                     (* End of Image. *) 
                     | LangUtil . FsKindTyp . FsKindEndOfImage 
                     => MteTeTfsEndOfImage ( ) 
                     (* InsTok. *) 
                     | LangUtil . FsKindTyp . FsKindInsTok 
                     => MteTeTfsInsTok ( LIsRepair ) 
                     (* Line breaks. *) 
                     | LangUtil . FsKindTyp . FsKindLineBreakOpt 
                     , LangUtil . FsKindTyp . FsKindLineBreakReqd  
                     => MteTeTfsLineBreak ( FsNodeRef . FsKind ) 
                     (* Est child *) 
                     | LangUtil . FsKindTyp . FsKindEstChildOfFixed 
                     => MteTeTfsEstChild ( IsEstList := FALSE ) 
                     | LangUtil . FsKindTyp . FsKindEstChildOfList 
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
            | LangUtil . FsKindTyp . FsKindSubtreeVert 
            , LangUtil . FsKindTyp . FsKindSubtreeHoriz 
            => MaxTouchedNodeNo := MAX ( MaxTouchedNodeNo , RootAbsNodeNo ) 
            ; MteTeTfsFsSubtree ( ) 

            | LangUtil . FsKindTyp . FsKindSubtreeFill 
            => MaxTouchedNodeNo := MAX ( MaxTouchedNodeNo , RootAbsNodeNo ) 
            ; MteTeContainsFill := TRUE 
(* FIX: ^This isn't really right.  It will leave MteTeContainsFill TRUE
         if we go up a level in the fs tree, to a place that is not
         filled.  This is overly conservative, i.e. it will delete
         line breaks in a place where no fill computation will take
         place. *) 
            ; MteTeTfsFsSubtree ( ) 

            (* Conditional format. *) 
            | LangUtil . FsKindTyp . FsKindCondFmt 
            => MteTeTfsTraverseCondFmtChildren ( ) 

            END (* CASE *) 
          END (* Block *) 
        END MteTeTraverseFs 

    ; BEGIN (* MteTraverseEst *) 
        VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LInitialFsChildNo : LangUtil . FsChildNoSignedTyp 

      ; BEGIN (* Block for MteTraverseEst *) 
          NewEstRef := EstRef 
        ; LMCharPos := LbeStd . LimitedCharNoUnknown 
        ; IF EstRef # NIL 
          THEN 
            MteTeContainsFill
              := RootFsNodeRef . FsKind IN LangUtil . FsKindSetEstFill 
          ; MteTeInterestingChildrenExist := FALSE 
          ; MteTeNILChildrenExist := FALSE 
          ; MteTeSibFmtNoMarkExists := FALSE 
          ; MteTeMergeIsStarted := FALSE 
          ; MteTeChildIsWaiting := FALSE 
          ; MteTeWaitingRef := NIL (* Defensive *)  
          ; MteTeWaitingFromChildNo := LbeStd . EstChildNoNull 
          ; MteTeWaitingToChildNo := LbeStd . EstChildNoNull (* Defensive. *)  
          ; MteTeWaitingFmtNo := LangUtil . FmtNoNull 
          ; MteTeWaitingEdgeKind := EstHs . EdgeKindTyp . EdgeKindEstChild  
          ; TravUtil . InitEstTravInfo ( MteTeEstTravInfo , EstRef ) 
          ; MteTeRMChildRef := NIL 
          ; MteTeRMChildRootNodeNo := LbeStd . EstNodeNoNull 
          ; MteTeRightChildRootNodeNo := LbeStd . EstNodeNoNull 
          ; MteTeCfInsTokFromPos := LbeStd . LimitedCharNoInfinity 
          (* Choose the right Est child. *) 
          ; CASE MteState 
            OF MteStateTyp . MteStateStartAtBeg 
            , MteStateTyp . MteStateStartAtEnd 
(* REVIEW: I seem to recall there was some code that can't happen here. *) 
            => IF RootAbsNodeNo = StartTokMark . EstNodeNo 
               THEN 
                 CASE StartTokMark . Kind 
                 OF MarkKindTyp . Plain 
                 , MarkKindTyp . BlankLine 
                 => (* The mark denotes the Est root, which is a leaf. 
                       No need to set to a child. *) 
                    Assert 
                      ( MteTeEstTravInfo . EtiParentRef = NIL 
                      , AFT . A_MergeTxt_MergeTextEdit_NotLeaf 
                      ) 
                 | MarkKindTyp . ChildFmtNo 
                 => TravUtil . GetLMEstChild ( MteTeEstTravInfo ) 
  (* TODO: There is undoubtedly a faster way to do this, using the 
           format number.  Code it and put in TravUtil. 
           (Maybe TravUtil.SetToChildContainingFmtNo?) *) 
                 ; WHILE MteTeEstTravInfo . EtiChildNo 
                         < MteTeEstTravInfo . EtiChildCt 
                         AND ( MteTeEstTravInfo . EtiChildFmtNo 
                               < StartTokMark . FmtNo 
                               OR ( MteTeEstTravInfo . EtiChildFmtNo 
                                    = StartTokMark . FmtNo 
                                    AND NOT ISTYPE 
                                              ( MteTeEstTravInfo 
                                                . EtiChildLeafElem . LeChildRef  
                                              , ModHs . ModCmntTrailingTyp 
                                              ) 
                                  ) 
                             ) 
                   DO MteTeIncEstChild ( ) 
                   END (* WHILE *) 
                 | MarkKindTyp . LeftSibFmtNo 
                 => CantHappen ( AFT . A_MteTraverseEstLeftSib ) 
                 ; TravUtil . GetLMEstChild ( MteTeEstTravInfo ) 
                 | MarkKindTyp . RightSibFmtNo 
                 => CantHappen ( AFT . A_MteTraverseEstRightSib ) 
                 END (* CASE  *) 
               ELSE 
                 TravUtil . SetToChildContainingNodeNo 
                   ( MteTeEstTravInfo 
                   , StartTokMark . EstNodeNo - RootAbsNodeNo 
                   ) 
               ; IF RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                    = StartTokMark . EstNodeNo 
                    AND StartTokMark . Kind = MarkKindTyp . RightSibFmtNo 
                 THEN 
                   MteTeIncEstChild ( ) 
                 END (* IF *) 
               END (* IF *) 
            (* Choose the right FmtNo. *) 
            ; MteTeStartFmtNo := MteTeEstTravInfo . EtiChildFmtNo 
            ; CASE StartTokMark . Kind 
              OF MarkKindTyp . ChildFmtNo 
              => IF RootAbsNodeNo = StartTokMark . EstNodeNo 
                 THEN
                   MteTeStartFmtNo := StartTokMark . FmtNo 
                 END (* IF *) 
              | MarkKindTyp . LeftSibFmtNo 
              => IF RootAbsNodeNo + MteTeEstTravInfo . EtiChildRootNodeNo 
                    = StartTokMark . EstNodeNo 
                 THEN 
                   MteTeStartFmtNo := StartTokMark . FmtNo 
                 END (* IF *) 
              | MarkKindTyp . RightSibFmtNo 
              => IF MteTeEstTravInfo . EtiChildNo 
                    >= MteTeEstTravInfo . EtiChildCt 
                    AND RootAbsNodeNo + MteTeRMChildRootNodeNo 
                        = StartTokMark . EstNodeNo 
                 THEN 
                   MteTeStartFmtNo := StartTokMark . FmtNo 
                 END (* IF *) 
              ELSE 
              END (* CASE *) 

            | MteStateTyp . MteStateFwd 
            , MteStateTyp . MteStatePassingNl 
            , MteStateTyp . MteStateRightNlFound 
            => TravUtil . GetLMEstChild ( MteTeEstTravInfo ) 
            ; MteTeStartFmtNo := LangUtil . FmtNoNull 
            ; MteTeIsFirstLine := TRUE  
            ; MteTeIndentPos := EstIndentPos1 

            | MteStateTyp . MteStateBwdEdit 
            , MteStateTyp . MteStateBwdNl 
            , MteStateTyp . MteStateDone  
            => TravUtil . GetRMEstChild ( MteTeEstTravInfo ) 
            ; MteTeRMChildRef 
                := MteTeEstTravInfo . EtiChildLeafElem . LeChildRef 
            ; MteTeRMChildRootNodeNo := MteTeEstTravInfo . EtiChildRootNodeNo 
            ; MteTeRightChildRootNodeNo := EstUtil . EstNodeCt ( EstRef ) 
            ; MteTeStartFmtNo := LangUtil . FmtNoNull 
            ELSE 
              CantHappen ( AFT . A_MteTraverseEstBadStateBefore ) 
            END (* CASE *) 
          ; IF RootFsNodeRef . FsChildren = NIL 
            THEN LFsChildCt := 0 
            ELSE LFsChildCt := NUMBER ( RootFsNodeRef . FsChildren ^ ) 
            END (* IF *) 
          ; CASE RootFsNodeRef . FsKind 

            (* Est fixed nodes. *) 
            OF LangUtil . FsKindTyp . FsKindEstFixedVert 
            , LangUtil . FsKindTyp . FsKindEstFixedHoriz 
            , LangUtil . FsKindTyp . FsKindEstFixedFill 
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
            | LangUtil . FsKindTyp . FsKindEstListVert 
            , LangUtil . FsKindTyp . FsKindEstListHoriz 
            , LangUtil . FsKindTyp . FsKindEstListFill 
            => CASE MteState 
               OF MteStateTyp . MteStateStartAtBeg 
               , MteStateTyp . MteStateStartAtEnd 
               => LInitialFsChildNo 
                   := LangUtil . FsChildNoOfFmtNo 
                        ( RootFsNodeRef , MteTeStartFmtNo ) 

               | MteStateTyp . MteStateFwd 
               , MteStateTyp . MteStatePassingNl 
               , MteStateTyp . MteStateRightNlFound 
               , MteStateTyp . MteStateBwdEdit 
               , MteStateTyp . MteStateBwdNl 
               , MteStateTyp . MteStateDone  
               => LInitialFsChildNo := 0 
               END (* CASE *) 
            ; MteTeTraverseFsListChildren 
                ( RootFsNodeRef 
                , EstFmtKind 
                , LFsChildCt 
                , LInitialFsChildNo 
                ) 

            (* Ast string, *) 
            | LangUtil . FsKindTyp . FsKindAstString 
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
        ; EVAL NewEstRef (* For breakpoint. *)  
        END (* Block *) 
      END MteTraverseEst 

  ; PROCEDURE MteCheckTokMarks ( ) RAISES { AssertionFailure } 

    = VAR LEstRef : LbeStd . EstRootTyp 
    ; VAR LEstRef2 : LbeStd . EstRootTyp 

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
          LEstRef 
            := TravUtil . DescendantWithNodeNo 
                 ( EstRootRef , EndTokMark . EstNodeNo + 1 ) 
        ; LEstRef2 
            := TravUtil . DescendantWithNodeNo 
                 ( NewEstRef , EndTokMark . EstNodeNo + NodeNoChange + 1 ) 
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
    ; VAR LNewEstRef : LbeStd . EstRootTyp 

    ; BEGIN (* Block  MergeTextEdit body. *) 
        Assert 
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
      ; MteFwdPrevTok := LbeStd . TokBegOfLine 
      ; MteLastFmtNoOnLine := LangUtil . FmtNoUnknown  
      ; MteEstListChildrenToPass := 0 (* Dead *) 
      ; MteNewDelThruFmtNo := LangUtil . FmtNoNull 
      ; MteNewDelFromFmtNo := LangUtil . FmtNoNull (* Defensive *) 
      ; MteNewBlankLineFmtNo := LangUtil . FmtNoNull 
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
      ; NewLinesCt := 0 
      ; MaxTouchedNodeNo := 0 
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
          , RootAbsNodeNo := 0 
          , RootFsNodeRef := EstUtil . FsRule ( Lang , EstRootRef ) 
          , EstFmtKind := LangUtil . FmtKindTyp . FmtKindVert 
          , EstIndentPos1 := Options . InitialIndentPos  
          , EstIndentPosN := Options . InitialIndentPos  
          , (* VAR *) NewEstRef := LNewEstRef 
          , (* VAR *) LMCharPos := LLMCharPos (* Ignore. *) 
          ) 
      ; Assert
          ( MteState = MteStateTyp . MteStateDone 
          , AFT . A_MergeTextEditNotDone 
          ) 
      ; NewEstRef := LNewEstRef 
      ; IF Options . Debug 
        THEN 
          TreeBrowse . Browse ( NewEstRef , Lang , "After MergeText" ) 
        END (* IF *) 
      ; NodeNoChange 
          := EstUtil . EstNodeCt ( NewEstRef ) - MteEstNodeCt 
        (* All the changes we make to the Est are in one or two lines. 
           Those lines following the changes, to which NodeNoChange applies, 
           All will have the same change, and it will also be the change 
           in the node count for the entire Est. *) 
      ; Assert 
          ( MteBlankSs = 0 
          , AFT . A_MergeTextEditNotZeroBlankSs 
          ) 
      ; MteCheckTokMarks ( ) 
      END (* Block *) 
    END MergeTextEdit 

; BEGIN (* MergeTxt *) 
  END MergeTxt 
. 
