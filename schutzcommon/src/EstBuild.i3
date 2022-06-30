
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Routines to build an Est node. *) 

INTERFACE EstBuild 

(* Routines to build an Est node. *) 

; IMPORT LbeStd 
; IMPORT EstHs 

; FROM Failures IMPORT Backout 

(* Merging slices of Est nodes' children into a new Est node. *) 

; CONST Brand = "EstBuild.T" 

; TYPE T = MergeStateTyp 
; TYPE MergeStatePublicTyp = OBJECT 
         MsEstTok : LbeStd . TokTyp
       END (* MergeStatePublicTyp *) 

; TYPE MergeStateTyp <: MergeStatePublicTyp 

; PROCEDURE InitMergeState 
    ( MergeState : MergeStateTyp 
    ; Lang : LbeStd . LangTyp 
    ; EstTok : LbeStd . TokTyp 
    ; EstRefToInheritFrom : EstHs . EstRefTyp 
    ) 
  : MergeStateTyp
  (* Initialize the fields of MergeState and return it. *) 

; PROCEDURE NewMergeState 
    ( Lang : LbeStd . LangTyp 
    ; EstTok : LbeStd . TokTyp 
    ; EstRefToInheritFrom : EstHs . EstRefTyp 
      (* ^KTreeWidthInfo and KTreeSyntTokCt come from this node, if nonNIL. 
          Otherwise, they are as computed. *) 
    ) 
  : MergeStateTyp 
  (* Allocate, initialize, and return a MergeStateTyp object. *) 

(* Merging proceeds right to left. *) 

; PROCEDURE PrependNl 
    ( MergeState : MergeStateTyp ; IsConditional : BOOLEAN ) 
  (* Prepend a new line to the being-merged Est. *) 

; PROCEDURE PrependTokInfo 
    ( MergeState : MergeStateTyp ; Tok : LbeStd . TokTyp ) 
  RAISES { Backout } 
  (* Prepend token count and width for an insertion token. *) 

; PROCEDURE AlterLMChild 
    ( MergeState : MergeStateTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; NewEstChildRef : LbeStd . EstRootTyp := NIL 
    ) 
  RAISES { Backout } 
  (* Alter the leftmost item in the being-constructed Est node, as follows:
     Include KindSet in its kind set. 
     IF NewEstChildRef is non-NIL, change the child to NewEstChildRef. 
  *) 

; PROCEDURE InclNextRightmostChildKindSet 
    ( MergeState : MergeStateTyp ; KindSet : EstHs . EstChildKindSetTyp ) 
  RAISES { Backout } 
  (* Arrange to include KindSet in the child kind set of the rightmost 
     element of the next-to-be-merged child or slice of MergeState. 
     FinishMerge asserts something was merged since this was called with
     a nonempty kind set*) 

; PROCEDURE MergeChild 
    ( MergeState : MergeStateTyp 
    ; EstRef : LbeStd . EstRootTyp 
    (* ^Must a leaf of an Est, including a mod. *) 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; IsFirstOfGroup : BOOLEAN 
      (* ^ IsFirstOfGroup will be stored in EstHs . EstChildKindFirstOfGroup 
          and, if true, causes storing LeFmtNo of the child. *) 
(* TODO: Change MergeChild and MergeSlice so they figure out internally
         when there is a first of group.  Ignore the FOG bit in deciding
         whether left must change.  This avoids unpacking the left side
         if there is no other reason to.  When it is the far right (next
         time), we will know if FOG should be set.  If it differs from
         what is already there, then unpack at that time, if packed.  Then
         change it all the way up. *) 
    ; GroupFmtNo : EstHs . FmtNoTyp 
      (* ^GroupFmtNo is used only if IsFirstOfGroup *) 
    ) 
  RAISES { Backout } 

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
  RAISES { Backout } 

; PROCEDURE FinishMerge 
    ( MergeState : MergeStateTyp 
    ; ResultEstNodeKind : EstHs . EstNodeKindTyp 
    ; VAR ResultTreeRef : EstHs . EstRefTyp 
    ) 
  RAISES { Backout } 

; END EstBuild 
. 
