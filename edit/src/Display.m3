
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Display 

; IMPORT Fmt 
; IMPORT Integer 
; IMPORT Pathname 
; IMPORT Point
; IMPORT Stdio  
; IMPORT Thread 
; IMPORT Wr 

; IMPORT AssertDevel  
; IMPORT Assertions  
; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout  
; IMPORT EditWindow 
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT Errors 
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT LineMarks 
; IMPORT LineNumbers 
; IMPORT Marks 
; IMPORT MessageCodes 
; IMPORT Misc 
; IMPORT ModHs 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT ParseHs 
; IMPORT Parser
; IMPORT ParseTrv 
; IMPORT PortTypes 
; IMPORT ScannerIf 
; IMPORT Search 
; IMPORT Selection 
; IMPORT Strings 
; IMPORT TempMark 
; IMPORT TextEdit  
; IMPORT TravUtil 
; IMPORT TreeBrowse 

; TYPE AFT = MessageCodes . T 
; TYPE MarkKindTyp = Marks . MarkKindTyp 

<* PRAGMA LL *> 

; CONST Point1_1 = Point . T { 1 , 1 } 

(* VISIBLE: *) 
; PROCEDURE LinesRefIsBegOfImage 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ) 
    : BOOLEAN 

  = BEGIN (* LinesRefIsBegOfImage *) 
      RETURN 
        ImageRef # NIL 
        AND LinesRef # NIL 
        AND LinesRef . LrLeftLink = ImageRef . ItPers . IpLineHeaderRef 
        AND NOT ImageRef . ItPers . IpLineHeaderRef . LrGapAfter 
    END LinesRefIsBegOfImage 

(* VISIBLE: *) 
; PROCEDURE LinesRefIsEndOfImage 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ) 
  : BOOLEAN 

  = BEGIN (* LinesRefIsEndOfImage *) 
      RETURN 
        ImageRef # NIL 
        AND LinesRef # NIL 
        AND LinesRef . LrRightLink = ImageRef . ItPers . IpLineHeaderRef 
        AND NOT LinesRef . LrGapAfter 
    END LinesRefIsEndOfImage 

(* VISIBLE: *) 
; PROCEDURE LineCtOfBolTokMark 
    ( EstRoot : LbeStd . EstRootTyp ; TokMark : Marks . TokMarkTyp ) 
  : LbeStd . LineNoTyp 
  RAISES { Backout } 
  (* For a TokMark identifying a regular LinesRef (not Blank Lines), will be 
     zero.  Also zero for a blank line, start at end. 
     otherwise the count of blank lines. *) 

  = VAR LNode : LbeStd . EstRootTyp 
  ; VAR LKindSet : EstHs . EstChildKindSetTyp 
  ; VAR LIsOptSingletonList : BOOLEAN 

  ; BEGIN (* LineCtOfBolTokMark *) 
      CASE TokMark . Kind 
      OF MarkKindTyp . Plain 
      , MarkKindTyp . ChildFmtNo 
      , MarkKindTyp . LeftSibFmtNo 
      , MarkKindTyp . RightSibFmtNo 
      => RETURN 0 

      | MarkKindTyp . BlankLine 
      => IF TokMark . StartAtEnd 
         THEN 
           RETURN 0 
         ELSE 
           TravUtil . GetDescendantWithNodeNo 
             ( EstRoot 
             , TokMark . EstNodeNo 
             , (* VAR *) LNode 
             , (* VAR *) LKindSet (* Dead. *) 
             , (* VAR *) LIsOptSingletonList (* Dead, always FALSE. *)  
             ) 
         ; TYPECASE LNode 
           OF ModHs . ModBlankLineTyp ( TBlankLine ) 
           => RETURN TBlankLine . ModBlankLineCt 
           ELSE 
             CantHappen ( AFT . A_LineCtOfBolTokMarkNotBlankLine ) 
           ; RETURN 0 
           END (* TYPECASE *) 
         END (* IF *) 

      ELSE 
        CantHappen ( AFT . A_LineCtOfBolTokMarkBadKind ) 
      ; RETURN 0 
      END (* CASE *) 
    END LineCtOfBolTokMark 

(* VISIBLE: *) 
; PROCEDURE ActualNoOfLines 
    ( LineCt : LbeStd . LineNoTyp ) : LbeStd . LineNoTyp 

  (* LineCt values have the invariant that they 
     are zero for a text-containing LinesRef or EndOfImage.  This allows 
     Lines to be easily detected. But sometimes you just 
     want the actual LineCt, regardless of whether this is 
     a blank line or not.  This function gives this. *) 

  = BEGIN (* ActualNoOfLines *) 
      RETURN MAX ( 1 , LineCt ) 
    END ActualNoOfLines 

(* VISIBLE: *) 
; PROCEDURE ActualLineCtOfLinesRef 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ) 
    : LbeStd . LineNoTyp 

  = BEGIN (* ActualLineCtOfLinesRef *) 
      IF ImageRef = NIL OR LinesRef = NIL 
      THEN RETURN 0 
      ELSE 
        IF LinesRefIsEndOfImage ( ImageRef , LinesRef ) 
           AND ImageRef . ItPers . IpTempEditState 
               = PaintHs . TempEditStateTyp . TeStateText 
           AND ImageRef . ItPers . IpTempEditRef # NIL 
           AND ImageRef . ItPers . IpTempEditRef . TeLinesRef = LinesRef 
        THEN 
          RETURN ImageRef . ItPers . IpTempEditRef . TeLineNo + 1 
        ELSE 
          RETURN ActualNoOfLines ( LinesRef . LrLineCt ) 
        END (* IF *) 
      END (* IF *) 
    END ActualLineCtOfLinesRef 

(* VISIBLE: *) 
; PROCEDURE LineNoInWindow 
    ( (* UNCHANGED *) WindowRef : PaintHs . WindowRefTyp 
    ; DesiredLinesRef : PaintHs . LinesRefMeatTyp 
    ) 
  : LbeStd . LineNoSignedTyp 
  RAISES { Backout } 
  (* DesiredLinesRef points to a line rec known to be visible in 
     the window described by WindowRef. Return the window-relative 
     line number of the first line represented by DesiredLinesRef. 
     The first line visible in the window is number 0.  The 
     result could be < 0, since DesiredLinesRef could point to a 
     blank line with > 1 line and some of these are above the 
     top of the window. *) 

  = VAR LLinesRef : PaintHs . LinesRefTyp 
  ; VAR LResult : LbeStd . LineNoTyp 

  ; BEGIN (* LineNoInWindow *) 
      IF WindowRef # NIL AND WindowRef . WrImageRef # NIL 
      THEN 
        LLinesRef := WindowRef . WrFirstLineLinesRef 
      ; IF LLinesRef = DesiredLinesRef 
        THEN 
          RETURN - WindowRef . WrFirstLineLineNo 
        ELSE 
          LResult := - WindowRef . WrFirstLineLineNo 
        ; LOOP 
            Assert 
              ( NOT LLinesRef . LrGapAfter , AFT . A_LineNoInWindowGapAfter ) 
          ; INC 
              ( LResult 
              , ActualLineCtOfLinesRef ( WindowRef . WrImageRef , LLinesRef ) 
              ) 
          ; LLinesRef := LLinesRef . LrRightLink 
          ; Assert 
              ( LLinesRef # WindowRef . WrImageRef . ItPers . IpLineHeaderRef 
              , AFT . A_LineNoInWindowLineNotFound 
              ) 
          ; IF LLinesRef = DesiredLinesRef THEN EXIT END (* IF *) 
          END (* LOOP *) 
        ; RETURN LResult 
        END (* IF *) 
      ELSE RETURN 0 
      END (* IF *) 
    END LineNoInWindow 

; PROCEDURE GetLinesRefAndLineNoInWindow 
    ( (* UNCHANGED *) WindowRef : PaintHs . WindowRefTyp 
    ; LineNoInWindow : LbeStd . LineNoTyp 
    ; VAR (* OUT *) LinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR (* OUT *) LineNo : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout } 

  = VAR LLinesRef : PaintHs . LinesRefTyp 
  ; VAR LLineNoInWindow : LbeStd . LineNoSignedTyp 
  ; VAR LLineCt : LbeStd . LineNoTyp 
  ; VAR LResultLineNo : LbeStd . LineNoTyp 

  ; BEGIN (* GetLinesRefAndLineNoInWindow *) 
      Assert 
        ( LineNoInWindow < EditWindow . SouthEastToCorner ( WindowRef ) . v 
        , AFT . A_GetLinesRefAndLineNoInWindowBigLineNo 
        ) 
    ; LLineNoInWindow := - WindowRef . WrFirstLineLineNo 
    ; LLinesRef := WindowRef . WrFirstLineLinesRef 
    ; LOOP 
        LResultLineNo := LineNoInWindow - LLineNoInWindow 
      ; LLineCt 
          := ActualLineCtOfLinesRef ( WindowRef . WrImageRef , LLinesRef ) 
      ; IF LResultLineNo < LLineCt
           OR LinesRefIsEndOfImage ( WindowRef . WrImageRef , LLinesRef )  
        THEN 
          LinesRef := LLinesRef 
        ; LineNo := LResultLineNo 
        ; EXIT 
        END (* IF *) 
      ; INC ( LLineNoInWindow , LLineCt ) 
      ; LLinesRef := LLinesRef . LrRightLink 
      END (* LOOP *) 
    END GetLinesRefAndLineNoInWindow 

(* VISIBLE: *) 
; PROCEDURE SecureSucc 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

  (* Ensure that the successor of LinesRef is really the 
     following line, i.e. LinesRef does not have GapAfter. *) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LSuccLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LEolTokMark : Marks . TokMarkTyp 
  ; VAR LActualBolTokMark : Marks . TokMarkTyp 
  ; VAR LDidHitExistingMark : BOOLEAN 
  ; VAR LLineCt : LbeStd . LineNoTyp 
  ; VAR LLineString : Strings . StringTyp 
  ; VAR LTextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
  ; VAR LLineErrArrayRef : PaintHs . LineErrArrayRefTyp 
  ; VAR LAtEndOfImage : BOOLEAN 
  ; VAR LAtBegOfImage : BOOLEAN 
  ; VAR LMetSuccessors : BOOLEAN 

  ; BEGIN (* SecureSucc *) 
      IF ImageRef # NIL AND LinesRef # NIL AND LinesRef . LrGapAfter 
      THEN (* Must compute successor. *) 
        LImagePers := ImageRef . ItPers 
      ; TYPECASE LinesRef 
        OF PaintHs . LinesRefMeatTyp ( TLinesRefMeat )
        => LineMarks . GetNextLine 
            ( Lang := LImagePers . IpLang 
            , EstRef := LImagePers . IpEstRoot 
            , StartMark := TLinesRefMeat . LrBolTokMark 
            , ExistingMark := Marks . TokMarkNull 
            , (* VAR *) DidHitExistingMark:= LDidHitExistingMark (* Dead. *) 
            , (* VAR *) NewMark := LActualBolTokMark 
            , (* VAR *) AtEndOfImage := LAtEndOfImage 
            , (* VAR *) LineText := LLineString 
            , (* VAR *) BlankLineCt := LLineCt
            , (* VAR *) TextAttrArrayRef := LTextAttrArrayRef  
            , (* VAR *) LineErrArrayRef := LLineErrArrayRef  
            ) 
        ; IF LAtEndOfImage 
          THEN (* TLinesRefMeat . LrBolTokMark denoted EOI, but not necessarily
                  the rightmost Nl of EOI.  Complete TLinesRefMeat as EOI. *) 
            Assert 
              ( TLinesRefMeat . LrRightLink = LImagePers . IpLineHeaderRef 
              , AFT . A_SecureSuccEOIMismatch1 
              ) 
          ; LineMarks . GetEndOfImage 
              ( LImagePers . IpLang 
              , LImagePers . IpEstRoot 
              , (* VAR *) LEolTokMark 
              ) 
          ; Assert 
              ( Marks . Equal ( LActualBolTokMark , LEolTokMark ) 
              , AFT . A_SecureSuccEOIMismatch2 
              ) 
          ; TLinesRefMeat . LrBolTokMark := LEolTokMark 
          ; TLinesRefMeat . LrLineLen := 0 
          ; TLinesRefMeat . LrLineText := NIL 
          ; TLinesRefMeat . LrLineCt := LLineCt 
          ; TLinesRefMeat . LrFromPos := 0 
          ; TLinesRefMeat . LrTextAttrArrayRef := NIL 
          ; TLinesRefMeat . LrLineErrArrayRef := NIL 
          ; TLinesRefMeat . LrIsStopper := TRUE 
          ; TLinesRefMeat . LrGapAfter := FALSE 
          ELSE (* Complete the LinesRef *)  
            TLinesRefMeat . LrFromPos 
              := Strings . PosOf1stNonblank ( LLineString ) 
  (* TODO: It would doubtless be more efficient to have GetNextLine suppress 
           these leading blanks in its result parameter, or at least compute
           their location. 
  *) 
          ; TLinesRefMeat . LrLineLen := Strings . Length ( LLineString ) 
          ; TLinesRefMeat . LrLineText 
              := Strings . ToText ( LLineString , TLinesRefMeat . LrFromPos ) 
          ; TLinesRefMeat . LrLineCt := LLineCt 
          ; TLinesRefMeat . LrTextAttrArrayRef := LTextAttrArrayRef 
          ; TLinesRefMeat . LrLineErrArrayRef := LLineErrArrayRef 
          ; TLinesRefMeat . LrIsStopper := TRUE 
          ; TLinesRefMeat . LrGapAfter := FALSE 
          ; TYPECASE TLinesRefMeat . LrRightLink 
            OF PaintHs . LinesRefMeatTyp ( TRightLinesRef ) 
            => TRY 
                 CASE Marks . Compare 
                        ( LActualBolTokMark , TRightLinesRef . LrBolTokMark ) 
                 OF - 1 => LMetSuccessors := FALSE 
                 | 0 => LMetSuccessors := TRUE 
                 | 1 => CantHappen ( AFT . A_SecureSucc_Passed_next_LinesRef ) 
                 END (* CASE *) 
               EXCEPT Marks . Unordered 
               => CantHappen ( AFT . A_SecureSucc_Unordered_marks ) 
               END (* TRY EXCEPT *) 
            ELSE 
              LMetSuccessors := FALSE 
            END (* TYPECASE *) 
          ; IF LMetSuccessors 
            THEN (* Have met up with another group of lines. No more to do. *) 
            ELSE (* Build a new incomplete LinesRef with the new BolTokMark. *) 
              LSuccLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
            ; LSuccLinesRef . LrBolTokMark := LActualBolTokMark 
            ; LSuccLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
            ; LSuccLinesRef . LrFromPos := 0 
            ; LSuccLinesRef . LrLineLen := 0 
            ; LSuccLinesRef . LrLineText := NIL  
            ; LSuccLinesRef . LrGapAfter := TRUE 
            ; LSuccLinesRef . LrHasMark := FALSE 
            ; LSuccLinesRef . LrLineCt := 0 
            ; LSuccLinesRef . LrIsStopper := TRUE 
            ; PaintHs . InsertLinesRefToRight 
                ( InsertToRightOfRef := TLinesRefMeat 
                , RefToInsert := LSuccLinesRef 
                )
            END (* IF *) 
          END (* IF *) 
        ELSE (* LinesRef is header. *)  
          Assert 
            ( LinesRef = LImagePers . IpLineHeaderRef 
            , AFT . A_SecureSucc_Header_node_not_the_header
            ) 
        ; TYPECASE LImagePers . IpLineHeaderRef . LrRightLink 
          OF PaintHs . LinesRefMeatTyp ( TRightLinesRef ) 
          => LineMarks . GetPrevLine 
               ( Lang := LImagePers . IpLang 
               , EstRef := LImagePers . IpEstRoot 
               , StartMark := TRightLinesRef . LrBolTokMark 
               , ExistingMark := Marks . TokMarkNull 
               , (* VAR *) NewMark := LActualBolTokMark 
               , (* VAR *) AtBegOfImage := LAtBegOfImage 
               ) 
          ; IF LAtBegOfImage 
            THEN (* TRightLinesRef is the first line of the image, although 
                    its mark may not be the leftmost, BOI mark. We are done. 
                 *)
(* FIX: This case would not be needed if LineMarks . GetBegOfImage, called
        below, worked right and returned the rightmost mark of the first
        line in the image. 
*) 
              LImagePers . IpLineHeaderRef . LrGapAfter := FALSE
            ; RETURN  
            END (* IF *) 
          ELSE 
          END (* TYPECASE *) 
        (* Build an incomplete successor to the header. *) 
        ; LSuccLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
        ; LineMarks . GetRMBegOfImage 
            ( Lang := LImagePers . IpLang 
            , EstRef := LImagePers . IpEstRoot 
            , (* VAR *) NewMark := LSuccLinesRef . LrBolTokMark 
            ) 
        ; LSuccLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
        ; LSuccLinesRef . LrFromPos := 0 
        ; LSuccLinesRef . LrLineLen := 0 
        ; LSuccLinesRef . LrLineText := NIL 
        ; LSuccLinesRef . LrGapAfter := TRUE 
        ; LSuccLinesRef . LrHasMark := FALSE 
        ; LSuccLinesRef . LrLineCt := 0 
        ; LSuccLinesRef . LrIsStopper := TRUE 
        ; PaintHs . InsertLinesRefToRight 
            ( InsertToRightOfRef := LImagePers . IpLineHeaderRef
            , RefToInsert := LSuccLinesRef 
            )
        ; LImagePers . IpLineHeaderRef . LrGapAfter := FALSE 
        END (* TYPECASE *) 
      END (* IF *) 
    END SecureSucc 

; PROCEDURE DeleteRedundantLinesRef 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; LeftLinesRef : PaintHs . LinesRefMeatTyp 
    ) 
  RAISES { Backout } 
  (* PRE: LeftLinesRef is not the header. 
          LeftLinesRef . LrGapAfter.  
          LeftLinesRef and its successor denote the same line.
  *) 
  (* LeftLinesRef and its successor are redundant.  Unlink one or the other.
     It is possible that either or both have LineMarks pointing to them, 
     and any such LineMarks must not be undermined.
  *) 

  = VAR LRightLinesRef : PaintHs . LinesRefMeatTyp
  ; VAR LMark : PaintHs . LineMarkMeatTyp  

  ; BEGIN 
      LRightLinesRef := LeftLinesRef . LrRightLink 
    ; IF LeftLinesRef . LrHasMark 
      THEN 
        IF TRUE OR LRightLinesRef . LrHasMark 
        THEN (* Both have LineMarks.  Keep LRightLinesRef and delete 
                LeftLinesRef, to avoid copying fields.  Locate and patch
                all LineMarks pointing to LRightLinesRef. 
             *) 
          LMark := ImageRef . ItPers . IpMarkHeader . LmRightLink 
          (* ^ NARROW OK. *)  
        ; TRY 
            LOOP (* Thru marks *) 
              CASE Marks . Compare 
                     ( LMark . LmTokMark , LeftLinesRef . LrBolTokMark ) 
              OF - 1 
              => LMark := LMark . LmRightLink (* NARROW should be OK. *) 
              | 0 
              => IF LMark . LmLinesRef = LeftLinesRef 
                THEN (* This is a LineMark to patch. *) 
                  LMark . LmTokMark := LRightLinesRef . LrBolTokMark
                  (* ^Just in case they are different. *) 
                ; LMark . LmLinesRef := LRightLinesRef 
                ; TYPECASE LMark . LmRightLink  
                  OF PaintHs . LineMarkMeatTyp ( TRightMark ) (* Can't be NIL *) 
                  => LMark := TRightMark 
                  ELSE (* No more marks at all. *)  
                    EXIT 
                  END (* TYPECASE *) 
                ELSE (* No more marks pointing to LeftLinesRef. *) 
                  EXIT 
                END (* IF *) 
              | 1 
              => (* No more marks denoting this line. *) 
                EXIT 
              END (* CASE *) 
            END (* LOOP *) 
          EXCEPT Marks . Unordered 
          => CantHappen ( AFT . A_DeleteRedundantLinesRef_Unordered_marks ) 
          END (* TRY EXCEPT *) 
        ; PaintHs . UnlinkLinesRef ( LeftLinesRef ) 
        ELSE (* Can unlink LRightLinesRef, but must copy its fields to 
                LeftLinesRef. 
             *) 
(* TODO:  This won't work now, because caller(s) are holding copies of
          LRightLinesRef and assuming its predecessor is begin secured.
          But instead, it is unlinked!.  Maybe add an output parameter 
          that is the now-nonredundant LinesRef, propagate it back through
          all the necessary places, make them tolerant of the change, and
          then reinistate this case. 
*) 
          LeftLinesRef . LrBolTokMark := LRightLinesRef . LrBolTokMark 
          (* ^Maintain rightmost Nl in a group. *)  
        ; LeftLinesRef . LrVisibleIn := LRightLinesRef . LrVisibleIn  
        ; LeftLinesRef . LrGapAfter := LRightLinesRef . LrGapAfter  
        ; LeftLinesRef . LrFromPos := LRightLinesRef . LrFromPos 
        ; LeftLinesRef . LrLineText := LRightLinesRef . LrLineText 
        ; LeftLinesRef . LrLineLen := LRightLinesRef . LrLineLen 
        ; LeftLinesRef . LrLineCt := LRightLinesRef . LrLineCt  
        ; LeftLinesRef . LrTextAttrArrayRef 
            := LRightLinesRef . LrTextAttrArrayRef 
        ; LeftLinesRef . LrLineErrArrayRef 
            := LRightLinesRef . LrLineErrArrayRef  
        ; LeftLinesRef . LrIsStopper := LRightLinesRef . LrIsStopper  
        ; PaintHs . UnlinkLinesRef ( LRightLinesRef ) 
        END (* IF *) 
      ELSE (* The easy case. *) 
        PaintHs . UnlinkLinesRef ( LeftLinesRef ) 
      END (* IF *) 
    END DeleteRedundantLinesRef 

(* VISIBLE: *) 
; PROCEDURE SecurePred 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Ensure that the predecessor of LinesRef is really the 
     previous line, i.e. it does not have GapAfter. *) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LLeftLinesRef : PaintHs . LinesRefTyp 
  ; VAR LTempLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLeftBolTokMark : Marks . TokMarkTyp 
  ; VAR LBolTokMark1 : Marks . TokMarkTyp 
  ; VAR LBolTokMark2 : Marks . TokMarkTyp 
  ; VAR LLineCt : LbeStd . LineNoTyp 
  ; VAR LTextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
  ; VAR LLineErrArrayRef : PaintHs . LineErrArrayRefTyp 
  ; VAR LAtBegOfImage : BOOLEAN 
  ; VAR LAtEndOfImage : BOOLEAN 
  ; VAR LDidHitExistingMark : BOOLEAN 
  ; VAR LIsStopper : BOOLEAN 
  ; VAR LLineString : Strings . StringTyp 
  ; VAR LNewLinesRefCt : PortTypes . Int32Typ := 0 
        (* ^Evidence for debugging. *) 

  ; BEGIN (* SecurePred *) 
      IF ImageRef # NIL AND LinesRef # NIL 
      THEN 
        LImagePers := ImageRef . ItPers 
      ; TYPECASE LinesRef 
        OF PaintHs . LinesRefMeatTyp ( TLinesRefMeat ) 
        => LLeftLinesRef := TLinesRefMeat . LrLeftLink 
        ; IF LLeftLinesRef . LrGapAfter 
          THEN (* Must compute predecessor. *) 
            IF LLeftLinesRef = LImagePers . IpLineHeaderRef 
            THEN 
              LineMarks . GetLMBegOfImage 
                ( Lang := LImagePers . IpLang 
                , EstRef := LImagePers . IpEstRoot 
                , (* VAR *) NewMark := LLeftBolTokMark 
                ) 
            ELSE 
              LLeftBolTokMark 
                := NARROW ( LLeftLinesRef , PaintHs . LinesRefMeatTyp ) 
                   . LrBolTokMark 
            END (* IF *) 
          ; IF Marks . Equal ( LLeftBolTokMark , TLinesRefMeat . LrBolTokMark )
            THEN (* The predecessor was already there. *) 
              IF LLeftLinesRef = LImagePers . IpLineHeaderRef 
              THEN 
                LLeftLinesRef . LrGapAfter := FALSE 
              ELSE (* LLeftLinesRef ^ has LrGapAfter and is incomplete. *) 
                DeleteRedundantLinesRef ( ImageRef , LLeftLinesRef ) 
              END (* IF *) 
            ELSE (* Try computing the predecessor. *) 
              LineMarks . GetPrevLine 
                ( Lang := LImagePers . IpLang 
                , EstRef := LImagePers . IpEstRoot 
                , StartMark := TLinesRefMeat . LrBolTokMark 
                , ExistingMark := LLeftBolTokMark 
                , (* VAR *) NewMark := LBolTokMark1 
                , (* VAR *) AtBegOfImage := LAtBegOfImage 
                ) 
            ; IF LAtBegOfImage 
              THEN (* TLinesRefMeat . LrBolTokMark denotes the BOI line, but 
                      not the leftmost Nl of the adjacent group of Nls. 
                   *) 
                IF LLeftLinesRef # LImagePers . IpLineHeaderRef 
                THEN (* Can this happen? *) 
                  DeleteRedundantLinesRef ( ImageRef , LLeftLinesRef ) 
                END (* IF *) 
              ; LImagePers . IpLineHeaderRef . LrGapAfter := FALSE 
              ELSE 
                IF LLeftLinesRef # LImagePers . IpLineHeaderRef 
                   AND Marks . Equal ( LBolTokMark1 , LLeftBolTokMark ) 
                THEN (* The predecessor was already there, but we didn't know. 
                        LLeftLinesRef ^ has LrGapAfter and is incomplete. 
                        Arrange to complete the predecessor below. 
                     *) 
                  LTempLinesRef := LLeftLinesRef 
                ELSE (* Arrange to allocate a new predecessor LinesRef below. *)
                  LTempLinesRef := NIL 
                END (* IF *) 
              (* Now move forward, to get line text. *) 
              ; LIsStopper := TRUE 
              ; LOOP 
                  LineMarks . GetNextLine 
                    ( Lang := LImagePers . IpLang 
                    , EstRef := LImagePers . IpEstRoot 
                    , StartMark := LBolTokMark1 
                    , ExistingMark := TLinesRefMeat . LrBolTokMark  
                    , (* VAR *) DidHitExistingMark := LDidHitExistingMark 
                    , (* VAR *) NewMark := LBolTokMark2 
                    , (* VAR *) AtEndOfImage := LAtEndOfImage 
                    , (* VAR *) LineText := LLineString 
                    , (* VAR *) BlankLineCt := LLineCt 
                    , (* VAR *) TextAttrArrayRef := LTextAttrArrayRef  
                    , (* VAR *) LineErrArrayRef := LLineErrArrayRef  
                    ) 
                ; IF LTempLinesRef = NIL 
                  THEN 
                    LTempLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
                  ; LTempLinesRef . LrBolTokMark := LBolTokMark1 
                  ; LTempLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
                  ; LTempLinesRef . LrHasMark := FALSE 
                  ; PaintHs . InsertLinesRefToLeft 
                      ( InsertToLeftOfRef := TLinesRefMeat 
                      , RefToInsert := LTempLinesRef 
                      ) 
                  END (* IF *) 
                ; LTempLinesRef . LrGapAfter := FALSE 
                ; LTempLinesRef . LrFromPos 
                    := Strings . PosOf1stNonblank ( LLineString ) 
                ; LTempLinesRef . LrLineText 
                    := Strings . ToText 
                         ( LLineString , LTempLinesRef . LrFromPos ) 
                ; LTempLinesRef . LrLineLen := Strings . Length ( LLineString )
                ; LTempLinesRef . LrLineCt := LLineCt 
                ; LTempLinesRef . LrTextAttrArrayRef := LTextAttrArrayRef 
                ; LTempLinesRef . LrLineErrArrayRef := LLineErrArrayRef 
                ; LTempLinesRef . LrIsStopper := LIsStopper 
                ; INC ( LNewLinesRefCt ) 
                ; IF LDidHitExistingMark  
                     OR Marks . Equal 
                          ( LBolTokMark2 , TLinesRefMeat . LrBolTokMark ) 
                  THEN 
                    EXIT 
                ; ELSE 
                    TRY 
                      IF Marks . Compare 
                           ( LBolTokMark2 ,  TLinesRefMeat . LrBolTokMark )
                         # - 1 
                      THEN Assertions . CantHappenText 
                             ( "SecurePred passed starting mark." )
                      END (* IF *) 
                    EXCEPT Marks . Unordered 
                    => Assertions . CantHappenText ( "SecurePred Unordered" ) 
                    END (* EXCEPT *) 
                  ; IF LAtEndOfImage 
                    THEN 
                      Assertions . CantHappenText ( "SecurePred Hit EOI" ) 
                    ; EXIT 
                    ELSE 
                      LBolTokMark1 := LBolTokMark2 
                    ; LTempLinesRef := NIL 
                    ; LIsStopper := FALSE 
                    END (* IF *) 
                  END (* IF *) 
                END (* LOOP *) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        ELSE (* LinesRef is header.  Build the dummy successor. *) 
          LTempLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
        ; LineMarks . GetEndOfImage 
            ( LImagePers . IpLang 
            , LImagePers . IpEstRoot 
            , (* VAR *) LTempLinesRef . LrBolTokMark 
            ) 
        ; LTempLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
        ; LTempLinesRef . LrHasMark := FALSE 
        ; LTempLinesRef . LrLineLen := 0 
        ; LTempLinesRef . LrLineText := NIL 
        ; LTempLinesRef . LrLineCt := 0 
        ; LTempLinesRef . LrFromPos := 0 
        ; LTempLinesRef . LrTextAttrArrayRef := NIL 
        ; LTempLinesRef . LrLineErrArrayRef := NIL 
        ; LTempLinesRef . LrIsStopper := TRUE 
        ; LTempLinesRef . LrGapAfter := FALSE 
        ; PaintHs . InsertLinesRefToLeft 
            ( InsertToLeftOfRef := LImagePers . IpLineHeaderRef  
            , RefToInsert := LTempLinesRef 
            ) 
        END (* TYPECASE *) 
      END (* IF *) 
    END SecurePred 

(* VISIBLE: *) 
; PROCEDURE ClearWindow ( WindowRef : PaintHs . WindowRefTyp ) 
  <* LL.sup < Window *> 

  = VAR LLen : LbeStd . CharNoTyp 

  ; BEGIN (* ClearWindow *) 
      IF WindowRef # NIL 
      THEN 
        LLen := EditWindow . SouthEastToCorner( WindowRef ) . h 
      ; FOR RLineNoInWindow := 0 
            TO EditWindow . SouthEastToCorner ( WindowRef ) . v - 1 
        DO EditWindow . PaintBackground  
             ( Window := WindowRef 
             , LineNoInWindow := RLineNoInWindow  
             , FromPosInWindow := 0 
             , Len := LLen 
             , BgColor := PaintHs . TaBgColorPlain 
             ) 
        END (* FOR *) 
      ; WindowRef . WrVertScroll := 0 
      ; WindowRef . WrVertScrollIsExact := TRUE  
      ; WindowRef . WrHorizScroll := 0 
      ; EditWindow . PaintText ( WindowRef , "Fv_PathName" , "" )  
      ; EditWindow . PaintText 
          ( WindowRef , "Fv_ImageName" , "<No Image Loaded>" )  
      ; EditWindow . PaintText ( WindowRef , "Fv_LangName" , "" )  
      ; EditWindow . PaintWindowState ( WindowRef , "Fv_Modified" , 0 ) 
      ; EditWindow . PaintWindowState ( WindowRef , "Fv_Unparsed" , 0 ) 
      ; EditWindow . PaintWindowState ( WindowRef , "Fv_Unanalyzed" , 0 ) 
      ; EditWindow . UpdateVertScroller ( WindowRef ) 
      ; EditWindow . UpdateHorizScroller ( WindowRef ) 
      END (* IF *) 
    END ClearWindow 

; TYPE HiliteInfoTyp
    = RECORD
        HiIsIn : BOOLEAN 
      ; HiFirstCharPos : LbeStd . CharNoTyp 
        (* ^Next place to toggle in/out of highlighted area. *)  
      ; HiSecondCharPos : LbeStd . CharNoTyp 
        (* ^Second place to toggle in/out of highlighted area. *) 
      END 

; PROCEDURE InitHiliteInfo 
    ( LeftMark : PaintHs . LineMarkMeatTyp  
    ; RightMark : PaintHs . LineMarkMeatTyp  
    ; READONLY BolTokMark : Marks . TokMarkTyp 
    ; LineNo : LbeStd . LineNoTyp 
    ; LeftEdgePos : LbeStd . CharNoTyp 
    ; RightEdgePos : LbeStd . CharNoTyp 
    ; VAR Info : HiliteInfoTyp 
    ) 
  RAISES { Backout } 

  = VAR LLeftCompare : [ - 1 .. 1 ] 
  ; VAR LRightCompare : [ - 1 .. 1 ] 

  ; BEGIN 
      Info . HiIsIn := FALSE 
    ; Info . HiFirstCharPos := LbeStd . CharNoInfinity 
    ; Info . HiSecondCharPos := LbeStd . CharNoInfinity 
    ; IF LeftMark # NIL 
         AND RightMark # NIL 
      THEN (* A selection exists and is for this image. *) 
        TRY 
          LLeftCompare 
            := Marks . Compare ( LeftMark . LmTokMark , BolTokMark )   
        EXCEPT Marks . Unordered 
        => CantHappen ( AFT . A_InitHiliteInfo_Unordered_left_marks ) 
        END (* TRY EXCEPT *) 
      ; IF LLeftCompare = 0 
        THEN LLeftCompare 
               := Integer . Compare ( LeftMark . LmLineNo , LineNo ) 
        END (* IF *) 
      ; TRY 
          LRightCompare 
            := Marks . Compare ( RightMark . LmTokMark , BolTokMark )
        EXCEPT Marks . Unordered 
        => CantHappen ( AFT . A_InitHiliteInfo_Unordered_right_marks ) 
        END (* TRY EXCEPT *) 
      ; IF LRightCompare = 0 
        THEN LRightCompare 
               := Integer . Compare ( RightMark . LmLineNo , LineNo ) 
        END (* IF *) 

      (* Here, LLeftCompare is Line of left selection :: Line to print.
         AND RightCompare is Line of right selection :: Line to print. 
      *) 
      ; IF LRightCompare < 0 OR LLeftCompare > 0 
        THEN (* Selection lies entirely outside the visible part of this 
                line. Leave alone. 
             *)   
        ELSE 
          (* Now refine LLeftCompare to left selection character
             :: BOL of visible part of to-be-painted line 
             and LRightCompare to right selection character 
             :: EOL of visible part of to-be-painted line.
          *) 
          IF LLeftCompare = 0 
          THEN LLeftCompare 
                 := Integer . Compare ( LeftMark . LmCharPos , LeftEdgePos ) 
          END (* IF *) 
        ; IF LRightCompare = 0 
          THEN LRightCompare 
                 := Integer . Compare 
                      ( RightMark . LmCharPos , RightEdgePos  ) 
          END (* IF *) 
        ; IF LLeftCompare < 0 
          THEN (* Inside selection at beginning of displayable line. *) 
            Info . HiIsIn := TRUE 
          ; IF LRightCompare > 0 
            THEN (* Still inside selection at end. *) 
            ELSE 
              Info . HiFirstCharPos := RightMark . LmCharPos
            END (* IF *)  
          ELSE (* Selection begins after BOL. *) 
            Info . HiFirstCharPos := LeftMark . LmCharPos 
          ; IF LRightCompare > 0 
            THEN (* Still inside selection at EOL. *)
            ELSE  
              Info . HiSecondCharPos := RightMark . LmCharPos
            END (* IF *) 
          END (* IF *)  
        END (* IF *) 
      END (* IF *) 
    END InitHiliteInfo 

; PROCEDURE AdvanceHiliteInfo 
    ( CharPos : LbeStd . CharNoTyp 
    ; VAR Info : HiliteInfoTyp 
    ) 

  = BEGIN 
      IF Info . HiFirstCharPos <= CharPos 
      THEN
        Info . HiIsIn := NOT Info . HiIsIn 
      ; Info . HiFirstCharPos := Info . HiSecondCharPos 
      ; Info . HiSecondCharPos := LbeStd . CharNoInfinity  
      END (* IF *) 
    END AdvanceHiliteInfo 

; PROCEDURE InnerPaintLine 
    ( (* UNCHANGED *) WindowRef : PaintHs . WindowRefTyp 
    ; LineNoInWindow : LbeStd . LineNoSignedTyp 
    ; LineLen : LbeStd . LimitedCharNoTyp 
      (* ^Including the unstored leading blanks. *) 
    ; FromPos : LbeStd . LimitedCharNoTyp 
    ; LineText : TEXT (* May be NIL *) 
    ; READONLY BolTokMark : Marks . TokMarkTyp 
    ; LineNo : LbeStd . LineNoTyp 
    ; READONLY TextAttrArray : PaintHs . TextAttrArrayTyp 
    ) 
  RAISES { Backout } 

  = VAR LLeftSelMark : PaintHs . LineMarkMeatTyp  
  ; VAR LRightSelMark : PaintHs . LineMarkMeatTyp  
  ; VAR LSelInfo : HiliteInfoTyp 
  ; VAR LMatchInfo : HiliteInfoTyp 
  ; VAR LPaintLineLen : LbeStd . CharNoTyp 
  ; VAR LCharPos : LbeStd . CharNoTyp  
  ; VAR LCharToPos : LbeStd . CharNoTyp  
  ; VAR LPaintFromPos : LbeStd . CharNoTyp  
  ; VAR LRightEdgeToPos : LbeStd . CharNoTyp 
  ; VAR LAttrSs : PortTypes . Int32Typ 
  ; VAR LAttrFromArray : PaintHs . TextAttrTyp 
  ; VAR LAttrToPaint : PaintHs . TextAttrTyp 
  ; VAR LBgColor : PaintHs . TextAttrComponentTyp 

  ; BEGIN (* InnerPaintLine *) 
      IF WindowRef # NIL 
      THEN 
        LRightEdgeToPos 
          := EditWindow . SouthEastToCorner ( WindowRef ) . h 
             + WindowRef . WrHorizScroll 
      ; IF Selection . Current = NIL 
           OR Selection . Current . SelImage # WindowRef . WrImageRef  
        THEN 
          LLeftSelMark := NIL 
        ; LRightSelMark := NIL 
        ELSE 
          PaintHs . GetMarksInOrder 
            ( Selection . Current . SelStartMark 
            , Selection . Current . SelEndMark 
            , (* VAR *) LLeftSelMark 
            , (* VAR *) LRightSelMark 
            )  
        END (* IF *) 
      ; InitHiliteInfo 
          ( LLeftSelMark 
          , LRightSelMark 
          , BolTokMark := BolTokMark 
          , LineNo := LineNo 
          , LeftEdgePos := WindowRef . WrHorizScroll 
          , RightEdgePos := LRightEdgeToPos 
          , (* VAR *) Info := LSelInfo 
          ) 
      ; InitHiliteInfo 
          ( WindowRef . WrMatchStartMark 
          , WindowRef . WrMatchEndMark 
          , BolTokMark := BolTokMark 
          , LineNo := LineNo 
          , LeftEdgePos := WindowRef . WrHorizScroll 
          , RightEdgePos := LRightEdgeToPos 
          , (* VAR *) Info := LMatchInfo 
          ) 
      ; EditWindow . BeginPaintGroup ( WindowRef ) 

      (* Paint any background to left of actual text. *) 
      ; LCharPos := WindowRef . WrHorizScroll 
      ; WHILE LCharPos < FromPos 
        DO 
          IF LSelInfo . HiIsIn 
          THEN LBgColor := PaintHs . TaBgColorSelected  
          ELSIF LMatchInfo . HiIsIn 
          THEN LBgColor := PaintHs . TaBgColorMatched  
          ELSE LBgColor := PaintHs . TaBgColorPlain  
          END (* IF *)
        ; LCharToPos 
            := MIN ( FromPos 
                   , MIN ( LSelInfo . HiFirstCharPos 
                         , LMatchInfo . HiFirstCharPos 
                         ) 
                   ) 
        ; AdvanceHiliteInfo ( LCharToPos , LSelInfo ) 
        ; AdvanceHiliteInfo ( LCharToPos , LMatchInfo ) 
        ; IF LCharToPos > WindowRef . WrHorizScroll 
          THEN 
            LPaintFromPos := MAX ( LCharPos , WindowRef . WrHorizScroll )  
          ; EditWindow . PaintBackground  
              ( Window := WindowRef 
              , LineNoInWindow := LineNoInWindow  
              , FromPosInWindow := LPaintFromPos - WindowRef . WrHorizScroll 
              , Len := LCharToPos - LPaintFromPos 
              , BgColor := LBgColor
              ) 
          END (* IF*) 
        ; LCharPos := LCharToPos 
        END (* WHILE *) 

      (* Paint the nonblank portion. *) 
      ; LAttrSs := 0 
      ; LAttrFromArray := PaintHs . TextAttrDefault 
      ; LPaintLineLen := MIN ( LineLen , LRightEdgeToPos ) 
      ; WHILE LCharPos < LPaintLineLen 
        DO 
          IF LSelInfo . HiIsIn 
          THEN 
            LAttrToPaint := PaintHs . TextAttrSelected 
          ; LCharToPos := LPaintLineLen  
          ELSIF LMatchInfo . HiIsIn 
          THEN 
            LAttrToPaint := PaintHs . TextAttrMatched 
          ; LCharToPos := LPaintLineLen  
          ELSE (* Find attribute and ToPos from TextAttrArray. *) 
            LOOP (* Through elements of TextAttrArray *) 
              IF LAttrSs >= NUMBER ( TextAttrArray )  
              THEN 
                LCharToPos := LPaintLineLen 
              ; EXIT 
              ELSIF TextAttrArray [ LAttrSs ] . TaCharPos > LCharPos 
              THEN
                LCharToPos := TextAttrArray [ LAttrSs ] . TaCharPos  
              ; EXIT 
              ELSE 
                LAttrFromArray := TextAttrArray [ LAttrSs ] 
              ; INC ( LAttrSs ) 
              END (* IF *)  
            END (* LOOP *)  
          ; LAttrToPaint := LAttrFromArray 
          END (* IF *) 
        ; LCharToPos 
            := MIN ( LCharToPos 
                   , MIN ( LSelInfo . HiFirstCharPos 
                         , LMatchInfo . HiFirstCharPos 
                         ) 
                   ) 
        ; AdvanceHiliteInfo ( LCharToPos , LSelInfo ) 
        ; AdvanceHiliteInfo ( LCharToPos , LMatchInfo ) 
        ; IF LCharToPos > WindowRef . WrHorizScroll 
          THEN 
            LPaintFromPos := MAX ( LCharPos , WindowRef . WrHorizScroll )  
          ; EditWindow . PaintLine  
              ( Window := WindowRef 
              , FromPosInWindow := LPaintFromPos - WindowRef . WrHorizScroll 
              , LineNoInWindow := LineNoInWindow 
              , PaintText := LineText 
              , FromSsInString := LPaintFromPos - FromPos 
              , ToSsInString := LCharToPos - FromPos 
              , Attr := LAttrToPaint  
              ) 
          END (* IF *) 
        ; LCharPos := LCharToPos 
        END (* WHILE *) 

      (* Paint any trailing blank space in window. *) 
      ; WHILE LCharPos < LRightEdgeToPos  
        DO 
          IF LSelInfo . HiIsIn  
          THEN LBgColor := PaintHs . TaBgColorSelected  
          ELSIF LMatchInfo . HiIsIn 
          THEN LBgColor := PaintHs . TaBgColorMatched  
          ELSE LBgColor := PaintHs . TaBgColorPlain  
          END (* IF *) 
        ; LCharToPos 
            := MIN ( LRightEdgeToPos 
                   , MIN ( LSelInfo . HiFirstCharPos 
                         , LMatchInfo . HiFirstCharPos 
                         ) 
                   ) 
        ; AdvanceHiliteInfo ( LCharToPos , LSelInfo ) 
        ; AdvanceHiliteInfo ( LCharToPos , LMatchInfo ) 
        ; EditWindow . PaintBackground  
            ( Window := WindowRef 
            , LineNoInWindow := LineNoInWindow  
            , FromPosInWindow := LCharPos - WindowRef . WrHorizScroll 
            , Len := LCharToPos - LCharPos 
            , BgColor := LBgColor
            ) 
        ; LCharPos := LCharToPos 
        END (* WHILE *) 

(* TODO: Do we want to protect against there being blanks at the ends
         of LineText, in placing the arrows? 
*) 
      ; EditWindow . PaintLeftArrow 
          ( WindowRef 
          , LineNoInWindow 
          , IsVisible 
              := LineLen > FromPos 
                 AND WindowRef . WrHorizScroll > FromPos 
          ) 
      ; EditWindow . PaintRightArrow 
          ( WindowRef 
          , LineNoInWindow 
          , IsVisible := LineLen > FromPos AND LineLen > LRightEdgeToPos  
          ) 
      END (* IF *) 
    ; EditWindow . EndPaintGroup ( WindowRef ) 
    END InnerPaintLine
 
(* VISIBLE: *) 
; PROCEDURE PaintTempEditedLine 
    ( (* UNCHANGED *) WindowRef : PaintHs . WindowRefTyp 
    ; LineNoInWindow : LbeStd . LineNoSignedTyp 
    ; TempEditRef : PaintHs . TempEditRefTyp 
    ; READONLY BolTokMark : Marks . TokMarkTyp 
    ; LineNo : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout }  

  = BEGIN 
    (* All this trouble, just so we can pass a SUBARRAY to InnerPaintLine, 
       without copying! *) 
      IF TempEditRef . TeTextAttrArrayRef = NIL  
      THEN 
        InnerPaintLine 
          ( WindowRef := WindowRef 
          , LineNoInWindow := LineNoInWindow 
          , LineLen := Strings . Length ( TempEditRef . TeEditedString ) 
          , FromPos := 0 
          , LineText := Strings . ToText ( TempEditRef . TeEditedString ) 
          , BolTokMark := BolTokMark 
          , LineNo := LineNo 
          , TextAttrArray := PaintHs . TextAttrArrayEmpty 
          ) 
      ELSE 
        InnerPaintLine 
          ( WindowRef := WindowRef 
          , LineNoInWindow := LineNoInWindow 
          , LineLen := Strings . Length ( TempEditRef . TeEditedString ) 
          , FromPos := 0 
          , LineText := Strings . ToText ( TempEditRef . TeEditedString ) 
          , BolTokMark := BolTokMark 
          , LineNo := LineNo 
          , TextAttrArray 
              := SUBARRAY 
                   ( TempEditRef . TeTextAttrArrayRef ^    
                   , 0 
                   , TempEditRef . TeTextAttrActualSize
                   ) 
          )
      END (* IF *)  
    END PaintTempEditedLine 

(* VISIBLE: *) 
; PROCEDURE PaintUneditedNonblankLine 
    ( (* UNCHANGED *) WindowRef : PaintHs . WindowRefTyp 
    ; LineNoInWindow : LbeStd . LineNoSignedTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ) 
  RAISES { Backout } 

  = BEGIN (* PaintUneditedNonblankLine *) 
      IF WindowRef # NIL AND WindowRef . WrImageRef # NIL 
      THEN 
        Assert 
          ( LinesRef . LrLineCt = 0 , AFT . A_PaintUneditedNonblankLineBlank ) 
      ; IF LinesRef . LrTextAttrArrayRef = NIL 
        THEN 
          InnerPaintLine 
            ( WindowRef 
            , LineNoInWindow 
            , LinesRef . LrLineLen 
            , LinesRef . LrFromPos
            , LinesRef . LrLineText
            , BolTokMark := LinesRef . LrBolTokMark 
            , LineNo := 0 
            , TextAttrArray := PaintHs . TextAttrArrayEmpty 
            ) 
        ELSE 
          InnerPaintLine 
            ( WindowRef 
            , LineNoInWindow 
            , LinesRef . LrLineLen 
            , LinesRef . LrFromPos
            , LinesRef . LrLineText
            , BolTokMark := LinesRef . LrBolTokMark 
            , LineNo := 0 
            , TextAttrArray := LinesRef . LrTextAttrArrayRef ^  
            ) 
        END (* IF *) 
      END (* IF *) 
    END PaintUneditedNonblankLine 

; PROCEDURE InnerPaintLinesRange
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; Window : PaintHs . WindowRefTyp 
    ; TempEditRef : PaintHs . TempEditRefTyp 
    ; FromLinesRef : PaintHs . LinesRefMeatTyp  
    ; FromLineNo : LbeStd . LineNoTyp
    ; ThruLinesRef : PaintHs . LinesRefMeatTyp  
    ; ThruLineNo : LbeStd . LineNoTyp
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: ImageTrans # NIL 
          AND Window # NIL 
          AND FromLinesRef # NIL 
          AND ToLinesRef # NIL 
  *) 

  = VAR LLinesRef : PaintHs . LinesRefMeatTyp  
  ; VAR LLineNo : LbeStd . LineNoTyp 
  ; VAR LLineCt : LbeStd . LineNoTyp 
  ; VAR LLineNoInWindow : LbeStd . LineNoTyp 
  ; VAR LWindowThruLinesRef : PaintHs . LinesRefMeatTyp  
  ; VAR LWindowThruLineNo : LbeStd . LineNoTyp 
  ; VAR LLinesRefIsEndOfImage : BOOLEAN 

  ; BEGIN 
      LLinesRef := Window . WrFirstLineLinesRef
    ; LLinesRefIsEndOfImage 
        := LinesRefIsEndOfImage ( ImageTrans , LLinesRef )
    ; TRY 
        IF Marks . Compare 
             ( ThruLinesRef . LrBolTokMark , LLinesRef . LrBolTokMark ) 
            >= 0 
        THEN (* Range doesn't end before the window starts. Continue. *) 
          GetLinesRefAndNoOfEndOfWindow  
            ( Window 
            , (* VAR *) LWindowThruLinesRef 
            , (* VAR *) LWindowThruLineNo 
            ) 
        ; IF Marks . Compare 
               ( FromLinesRef . LrBolTokMark 
               , LWindowThruLinesRef . LrBolTokMark 
               ) 
             <= 0 
          THEN (* Window contains some lines to be painted. *) 
            LLineNoInWindow := 0 
          ; LLineCt := ActualLineCtOfLinesRef ( ImageTrans , LLinesRef ) 
          ; LLineNo := Window . WrFirstLineLineNo 
          ; CASE Marks . Compare 
                   ( FromLinesRef . LrBolTokMark , LLinesRef . LrBolTokMark )
            OF - 1  (* < *)  
            => (* Already set at top actual line in window. *) 
            | 0 (* = *) 
            => IF FromLineNo > LLineNo 
               THEN 
                 LLineNoInWindow := FromLineNo - LLineNo  
               ; LLineNo := FromLineNo 
               END (* IF *) 
            | 1 (* > *) 
            => (* Must skip some LinesRefs at top of window. *)  
              WHILE LLinesRef # FromLinesRef 
              DO 
                Assert
                  ( LLinesRef # LWindowThruLinesRef 
                  , AFT . A_PaintLinesRangeAllWindows_Missing_before
                  ) 
              ; INC ( LLineNoInWindow , LLineCt - LLineNo ) 
              ; LLinesRef := LLinesRef . LrRightLink 
              ; LLinesRefIsEndOfImage 
                  := LinesRefIsEndOfImage ( ImageTrans , LLinesRef )
              ; LLineCt := ActualLineCtOfLinesRef ( ImageTrans , LLinesRef ) 
              ; LLineNo := 0 
              END (* WHILE *) 
            ; INC ( LLineNoInWindow , FromLineNo ) 
            ; LLineNo := FromLineNo 
            END (* CASE  *) 
          (* Now start painting lines. *)
          ; LOOP 
              IF TempEditRef # NIL 
                 AND TempEditRef . TeLinesRef = LLinesRef 
                 AND TempEditRef . TeLineNo = LLineNo 
              THEN 
                PaintTempEditedLine 
                  ( WindowRef := Window 
                  , LineNoInWindow := LLineNoInWindow 
                  , TempEditRef := TempEditRef 
                  , BolTokMark := LLinesRef . LrBolTokMark 
                  , LineNo := TempEditRef . TeLineNo 
                  ) 
              ELSIF LLinesRef . LrLineCt > 0 (* Blank line LinesRef *) 
                    OR LLineNo > 0 (* In space after EOI. *) 
              THEN 
                InnerPaintLine 
                  ( Window 
                  , LLineNoInWindow 
                  , LineLen := 0 
                  , FromPos := 0 
                  , LineText := NIL  
                  , BolTokMark := LLinesRef . LrBolTokMark 
                  , LineNo := LLineNo 
                  , TextAttrArray := PaintHs . TextAttrArrayEmpty 
                  ) 
              ELSE 
                Assert
                  ( LLineNo = 0 
                  , AFT . A_PaintLinesRangeAllWindows_Nonzero_LLineNo 
                  ) 
              ; PaintUneditedNonblankLine 
                  ( Window 
                  , LLineNoInWindow 
                  , LLinesRef 
                  ) 
              END (* IF *) 
            ; IF LLinesRef = ThruLinesRef  AND LLineNo = ThruLineNo 
              THEN (* End of range of LinesRefs to paint. *) 
                EXIT 
              ELSE  
                INC ( LLineNoInWindow ) 
              ; IF LLineNoInWindow 
                   >= EditWindow . SouthEastToCorner ( Window ) . v 
                THEN (* At end of window *) 
                  EXIT 
                ELSE 
                  INC ( LLineNo )
                ; IF LLineNo >= LLineCt AND NOT LLinesRefIsEndOfImage 
                  THEN 
                    Assert
                      ( LLinesRef # LWindowThruLinesRef 
                      , AFT . A_PaintLinesRangeAllWindows_Missing_during
                      ) 
                  ; LLinesRef := LLinesRef . LrRightLink 
                  ; LLinesRefIsEndOfImage 
                      := LinesRefIsEndOfImage ( ImageTrans , LLinesRef )
                  ; LLineCt 
                      := ActualLineCtOfLinesRef ( ImageTrans , LLinesRef ) 
                  ; LLineNo := 0 
                  END (* IF *) 
                END (* IF *) 
              END (* IF *) 
            END (* LOOP *) 
          ; EditWindow . SetCursorPosition 
              ( Window 
              , Window . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
                . LmCharPos 
                - Window . WrHorizScroll 
              , Window . WrCursorLineNoInWindow 
              ) 
          ; EditWindow . PaintCursorCoordinates ( Window ) 
          END (* IF *) 
        END (* IF *) 
      EXCEPT Marks . Unordered 
      => CantHappen ( AFT . A_InnerPaintLinesRange_Unordered_marks ) 
      END (* TRY EXCEPT *) 
    END InnerPaintLinesRange 

(* VISIBLE: *) 
; PROCEDURE PaintLinesRangeOneWindow 
    ( Window : PaintHs . WindowRefTyp 
    ; FromLinesRef : PaintHs . LinesRefMeatTyp  
    ; FromLineNo : LbeStd . LineNoTyp
    ; ThruLinesRef : PaintHs . LinesRefMeatTyp  
    ; ThruLineNo : LbeStd . LineNoTyp
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LImageTrans : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LTempEditRef : PaintHs . TempEditRefTyp 

  ; BEGIN 
      IF Window # NIL AND FromLinesRef # NIL AND ThruLinesRef # NIL 
      THEN 
        LImageTrans := Window . WrImageRef  
      ; IF LImageTrans # NIL 
        THEN 
          LImagePers := LImageTrans . ItPers 
        ; LTempEditRef := LImagePers . IpTempEditRef 
        ; InnerPaintLinesRange 
            ( LImageTrans 
            , Window
            , LTempEditRef 
            , FromLinesRef 
            , FromLineNo 
            , ThruLinesRef 
            , ThruLineNo
            ) 
        END (* IF *)
      END (* IF *)
    END PaintLinesRangeOneWindow 

(* VISIBLE: *) 
; PROCEDURE PaintLinesRangeAllWindows 
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; FromLinesRef : PaintHs . LinesRefMeatTyp  
    ; FromLineNo : LbeStd . LineNoTyp
    ; ThruLinesRef : PaintHs . LinesRefMeatTyp  
    ; ThruLineNo : LbeStd . LineNoTyp
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LWindow : PaintHs . WindowRefTyp 

  ; BEGIN 
      IF ImageTrans # NIL AND FromLinesRef # NIL AND ThruLinesRef # NIL 
      THEN 
        LImagePers := ImageTrans . ItPers 
      ; LTempEditRef := LImagePers . IpTempEditRef 
      ; LWindow := ImageTrans . ItWindowList 
      ; WHILE LWindow # NIL 
        DO 
          InnerPaintLinesRange 
            ( ImageTrans 
            , LWindow
            , LTempEditRef 
            , FromLinesRef 
            , FromLineNo 
            , ThruLinesRef 
            , ThruLineNo
            ) 
        ; LWindow := LWindow . WrImageLink 
        END (* WHILE *) 
      END (* IF *)
    END PaintLinesRangeAllWindows 

; PROCEDURE AdjustEstimatesForEndOfImage 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; NonblankLineCtInWindow : LbeStd . LineNoSignedTyp 
    ) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LImageLineCt : LbeStd . LineNoSignedTyp 
  ; VAR LWindowRef : PaintHs . WindowRefTyp 

  ;  BEGIN 
      LImagePers := WindowRef . WrImageRef . ItPers 
    ; IF WindowRef . WrVertScrollIsExact 
      THEN
        IF LImagePers . IpLineCtIsExact 
        THEN 
(* FIX: The assertion below is not necessarily so, until we fix the FmtKind
        problem for line mark traversals.  
        Don't come here at all.  Do the adjustment below, instead. *) 
          IF WindowRef . WrVertScroll 
             + NonblankLineCtInWindow 
             # LImagePers . IpLineCtDisplay 
          THEN 
           Assertions . MessageText 
             ( "Supposedly exact line counts disagree" ) 
          END (* IF *) 
        ELSE (* Adjust IpLineCtDisplay *) 
          LImageLineCt := LImagePers . IpLineCtDisplay 
        ; LImagePers . IpLineCtDisplay 
            := WindowRef . WrVertScroll + NonblankLineCtInWindow  
        ; LImagePers . IpLineCtIsExact := TRUE
        (* Go thru other windows and proportion any estimated
           WrVertScroll values. *) 
        ; LWindowRef := WindowRef . WrImageRef . ItWindowList  
        ; WHILE LWindowRef # NIL 
          DO
            IF LWindowRef # WindowRef 
               AND NOT LWindowRef . WrVertScrollIsExact
            THEN
              LWindowRef . WrVertScroll 
                := ROUND (* Doing this in INTEGER would otherwise work, 
                            but might overflow. *) 
                     ( FLOAT ( LWindowRef . WrVertScroll ) 
                       / FLOAT ( LImageLineCt - NonblankLineCtInWindow ) 
                       * FLOAT ( LImagePers . IpLineCtDisplay 
                                 - NonblankLineCtInWindow 
                               )  
                     ) 
            ; EditWindow . UpdateVertScroller ( LWindowRef ) 
            ; EditWindow . PaintCursorCoordinates ( LWindowRef ) 
              (* ^Boy, will users think this is wierd!, if they notice. *) 
            END (* IF *) 
          ; LWindowRef := LWindowRef . WrImageLink 
          END (* WHILE *) 
        END (* IF *) 
      ELSE (* WrVertScroll is only estimated.  Even if IpLineCtDisplay is
              also only estimated, update WrVertScroll to match it,
              because adjusting IpLineCtDisplay would require adjustments
              to WrVertScroll of other windows. *) 
        WindowRef . WrVertScroll 
          := MAX ( 0 
                 , LImagePers . IpLineCtDisplay 
                   - NonblankLineCtInWindow  
                 ) 
      ; WindowRef . WrVertScrollIsExact
          := LImagePers . IpLineCtIsExact           
      END (* IF *) 
    END AdjustEstimatesForEndOfImage  

(* VISIBLE: *) 
; PROCEDURE PaintWindowFromLines 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; VAR BlankLinesAtEnd : LbeStd . LineNoTyp 
    ; VAR ToLinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR ToLineNo : LbeStd . LineNoSignedTyp 
    ) 

  RAISES { Backout , Thread . Alerted } 

  = VAR PwLineNoInWindow : LbeStd . LineNoSignedTyp 
  ; VAR PwLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR PwLineNo : LbeStd . LineNoSignedTyp 
  ; VAR PwTempEditRef : PaintHs . TempEditRefTyp 

  ; PROCEDURE PwTryTempEdit ( ) : BOOLEAN RAISES { Backout } 

    (* Has SIDE EFFECTS! If this is a temp edited line, paints it. 
       Result is whether this happened.. *) 
    = BEGIN (* PwTryTempEdit *) 
        IF PwTempEditRef # NIL 
           AND PwTempEditRef . TeLinesRef = PwLinesRef 
           AND PwTempEditRef . TeLineNo = PwLineNo 
	THEN 
	  PaintTempEditedLine 
	    ( WindowRef := WindowRef 
	    , LineNoInWindow := PwLineNoInWindow 
	    , TempEditRef := PwTempEditRef 
            , BolTokMark := PwLinesRef . LrBolTokMark 
            , LineNo := PwTempEditRef . TeLineNo 
	    ) 
	; RETURN TRUE 
	ELSE 
	  RETURN FALSE 
	END (* IF *) 
      END PwTryTempEdit 

  ; BEGIN (* PaintWindowFromLines *) 
      VAR LImagePers : PaintHs . ImagePersistentTyp 
    ; VAR LLineCt : LbeStd . LineNoSignedTyp 
    ; VAR LNonblankLineCtInWindow : LbeStd . LineNoSignedTyp 
    ; VAR LSouthEastToCorner := EditWindow . SouthEastToCorner ( WindowRef ) 

    ; BEGIN (* Block PaintWindowFromLines *)
        BlankLinesAtEnd := 0  
      ; IF WindowRef # NIL 
        THEN 
          EditWindow . PaintWindowBlank ( WindowRef ) 
        ; EditWindow . SetCursorPosition ( WindowRef , 0 , 0 ) 
        ; IF WindowRef . WrImageRef # NIL 
          THEN 
            LImagePers := WindowRef . WrImageRef . ItPers  
          ; IF LImagePers . IpTempEditState 
               = PaintHs . TempEditStateTyp . TeStateText 
            THEN 
              PwTempEditRef := LImagePers . IpTempEditRef 
            ELSE 
              PwTempEditRef := NIL 
            END (* IF *) 
          ; PwLinesRef := WindowRef . WrFirstLineLinesRef 
          ; SecureSucc ( WindowRef . WrImageRef , PwLinesRef ) 
          ; LLineCt := PwLinesRef . LrLineCt - WindowRef . WrFirstLineLineNo 
          ; PwLineNoInWindow := 0 
          ; LOOP 
              PwLineNo := 0 
            ; IF PwLineNoInWindow >= LSouthEastToCorner . v 
              THEN 
                EXIT 
              END (* IF *) 
            ; PaintHs . IncludeLrVisibleIn 
                ( PwLinesRef , WindowRef . WrWindowNo ) 
            ; IF LLineCt > 0 
              THEN (* Blank lines. *) 
                WHILE PwLineNo < LLineCt 
                      AND PwLineNoInWindow < LSouthEastToCorner . v 
                DO IF NOT PwTryTempEdit ( ) 
                   THEN 
                     InnerPaintLine 
                       ( WindowRef := WindowRef 
                       , LineNoInWindow := PwLineNoInWindow  
                       , LineLen := 0 
                       , FromPos := 0 
                       , LineText := NIL 
                       , BolTokMark := PwLinesRef . LrBolTokMark 
                       , LineNo := PwLineNo  
                       , TextAttrArray := PaintHs . TextAttrArrayEmpty 
                       ) 
                   END (* IF *) 
                ; INC ( PwLineNoInWindow ) 
                ; INC ( PwLineNo ) 
                END (* WHILE *) 
              ELSIF LinesRefIsEndOfImage 
                      ( WindowRef . WrImageRef , PwLinesRef ) 
              THEN (* Blank remainder of window. *) 
                LNonblankLineCtInWindow := PwLineNoInWindow   
              ; WHILE PwLineNoInWindow < LSouthEastToCorner . v 
                DO IF PwTryTempEdit ( ) 
                   THEN
                     INC ( PwLineNoInWindow ) 
                   ; LNonblankLineCtInWindow := PwLineNoInWindow 
                   ELSE  
                     InnerPaintLine 
                       ( WindowRef := WindowRef 
                       , LineNoInWindow := PwLineNoInWindow  
                       , LineLen := 0 
                       , FromPos := 0 
                       , LineText := NIL 
                       , BolTokMark := PwLinesRef . LrBolTokMark 
                       , LineNo := PwLineNo  
                       , TextAttrArray := PaintHs . TextAttrArrayEmpty 
                       ) 
                   ; INC ( BlankLinesAtEnd ) 
                   ; INC ( PwLineNoInWindow ) 
                   END (* IF *) 
                ; INC ( PwLineNo ) 
                END (* WHILE *) 
              ; AdjustEstimatesForEndOfImage 
                  ( WindowRef , LNonblankLineCtInWindow )
              ; EXIT 
              ELSE 
                IF NOT PwTryTempEdit ( ) 
                THEN 
                  PaintUneditedNonblankLine 
                    ( WindowRef , PwLineNoInWindow , PwLinesRef ) 
                END (* IF *) 
              ; INC ( PwLineNoInWindow ) 
              END (* IF *) 
            ; PwLinesRef := PwLinesRef . LrRightLink 
              (* ^SecureSucc has been called on PwLinesRef, which will ensure 
                 the implied NARROW will succeed. 
              *) 
            ; SecureSucc ( WindowRef . WrImageRef , PwLinesRef ) 
            ; LLineCt := PwLinesRef . LrLineCt 
            END (* LOOP *) 
          ; EditWindow . SetCursorPosition 
              ( WindowRef 
              , WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
                . LmCharPos 
                - WindowRef . WrHorizScroll 
              , WindowRef . WrCursorLineNoInWindow 
              ) 
          ; EditWindow . PaintWindowState 
              ( WindowRef 
              , "Fv_Modified" 
              , ORD ( NOT WindowRef . WrImageRef . ItIsSaved ) 
              ) 
          ; PaintImageParsedState ( WindowRef . WrImageRef ) 
          ; EditWindow . PaintWindowState 
              ( WindowRef 
              , "Fv_Unanalyzed" 
              , ORD ( NOT LImagePers . IpIsAnalyzed ) 
              ) 
          ; EditWindow . PaintText 
              ( WindowRef 
              , "Fv_PathName" 
              , Pathname . Prefix ( LImagePers . IpAbsPklFileName )
                & Misc . FilePathSeparator ( )  
              ) 
          ; EditWindow . PaintText 
              ( WindowRef 
              , "Fv_ImageName" 
              , LImagePers . IpImageName 
              ) 
          ; EditWindow . PaintText 
              ( WindowRef 
              , "Fv_LangName" 
              , LangUtil . LangIdRef ( LImagePers . IpLang ) 
                ^ . LangShortName  
              ) 
          ; ToLinesRef := PwLinesRef 
          ; ToLineNo := PwLineNo 
          ELSE (* No image is open. *) 
            EditWindow . PaintWindowState 
              ( WindowRef , "Fv_Modified" , 0 ) 
          ; EditWindow . PaintWindowState 
              ( WindowRef , "Fv_Unparsed" , 0 ) 
          ; EditWindow . PaintWindowState 
              ( WindowRef , "Fv_Unanalyzed" , 0 ) 
          ; EditWindow . PaintText ( WindowRef , "Fv_PathName" , "" ) 
          ; EditWindow . PaintText 
              ( WindowRef , "Fv_ImageName" , "<No Image Loaded>" ) 
          ; EditWindow . PaintText ( WindowRef , "Fv_LangName" , "" ) 
          ; ToLinesRef := NIL 
          ; ToLineNo := 0 
          END (* IF *) 
        ; EditWindow . PaintInsertMode ( WindowRef ) 
        ; EditWindow . UpdateVertScroller ( WindowRef ) 
        ; EditWindow . UpdateHorizScroller ( WindowRef ) 
        ; EditWindow . MakeCursorVisible ( WindowRef ) 
        ; EditWindow . PaintCursorCoordinates ( WindowRef ) 
        END (* IF *) 
      END (* Block *) 
    END PaintWindowFromLines 

(* VISIBLE: *) 
; PROCEDURE HorizScrollWindowRefToContainCursor 
    ( (* IN OUT *) WindowRef : PaintHs . WindowRefTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
      (* Set true if anything requiring a repaint happens. *) 
    ) 

  (* Absolute cursor position doesn't change.  Window moves 
     the minimum necessary to contain it. *) 

  = BEGIN (* HorizScrollWindowRefToContainCursor *) 
      IF WindowRef # NIL AND WindowRef . WrImageRef # NIL 
      THEN 
        WITH 
          WCursorMark 
          = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
        DO IF WCursorMark . LmCharPos < WindowRef . WrHorizScroll 
           THEN (* Cursor is outside window to the left. *) 
             WindowRef . WrHorizScroll := WCursorMark . LmCharPos 
           ; MustRepaint := TRUE 
           ELSIF WCursorMark . LmCharPos 
                 > ( EditWindow . SouthEastToCorner ( WindowRef ) . h 
                     + WindowRef . WrHorizScroll 
                   ) 
           THEN (* Cursor is outside window to the right. *) 
             WindowRef . WrHorizScroll 
               := WCursorMark . LmCharPos 
                  - EditWindow . SouthEastToCorner ( WindowRef ) . h 
           ; MustRepaint := TRUE 
           END (* IF *) 
        END (* WITH WCursorMark *) 
      END (* IF *) 
    END HorizScrollWindowRefToContainCursor 

; PROCEDURE HorizProjectCursorIntoWindow 
    ( (* IN OUT *) WindowRef : PaintHs . WindowRefTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
      (* Set true if anything requiring a repaint happens. *) 
    ) 

  = VAR LRightmostCharPos : LbeStd . LimitedCharNoSignedTyp 

  ; BEGIN (* HorizProjectCursorIntoWindow *) 
      WITH 
        WCursorMark 
          = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
      DO IF WCursorMark . LmCharPos < WindowRef . WrHorizScroll 
         THEN (* Cursor is outside window to the left. *) 
           WCursorMark . LmCharPos := WindowRef . WrHorizScroll 
         ; MustRepaint := TRUE 
         ELSE 
           LRightmostCharPos 
             := EditWindow . SouthEastToCorner ( WindowRef ) . h 
                + WindowRef . WrHorizScroll 
         ; IF WCursorMark . LmCharPos > LRightmostCharPos 
           THEN (* Cursor is outside window to the right. *) 
             WCursorMark . LmCharPos := LRightmostCharPos 
           ; MustRepaint := TRUE 
           END (* IF *) 
         END (* IF *) 
      END (* WITH WCursorMark *) 
    END HorizProjectCursorIntoWindow 

(* VISIBLE: *) 
; PROCEDURE HorizMoveCursorWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; CursorMovement : EditWindow . CharCoordTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
      (* Set true if anything requiring a repaint happens. *) 
    ) 
  RAISES { Backout } 
  (* Will scroll WindowRef if necessary to keep cursor inside. *) 

  = VAR LCursorMovement : EditWindow . CharCoordTyp 
  ; VAR LMark : PaintHs . LineMarkMeatTyp 

  ; BEGIN (* HorizMoveCursorWindowRef *) 
      IF WindowRef # NIL 
         AND WindowRef . WrImageRef # NIL 
         AND CursorMovement # 0 
      THEN 
        WITH 
          WCursorMark 
          = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
        DO IF WCursorMark # NIL 
           THEN 
             LCursorMovement 
               := MIN 
                    ( LbeStd . LimitedCharNoMax - WCursorMark . LmCharPos 
                    , MAX ( - WCursorMark . LmCharPos , CursorMovement ) 
                    ) 
           ; IF LCursorMovement < 0 
             THEN 
               TYPECASE WCursorMark . LmLeftLink 
               OF PaintHs . LineMarkMeatTyp ( TFirstMark ) 
               => LMark := TFirstMark 
               ; LOOP 
                   IF NOT Marks . Equal 
                            ( LMark . LmLinesRef . LrBolTokMark 
                            , WCursorMark . LmLinesRef . LrBolTokMark 
                            ) 
                      OR LMark . LmLineNo < WCursorMark . LmLineNo 
                   THEN (* This MarkRec is for a different line. *) 
                     EXIT 
                   ELSIF LMark . LmCharPos 
                      < ( WCursorMark . LmCharPos 
                          + LCursorMovement 
                          (* Don't exit, i.e. do pass other marks that
                             are exactly where we want the cursor to
                             stop (except for as below), so that we
                             will eventually hit and pass this
                             window's EndSel mark, if it is exactly
                             here.  
                          *)
                          + ORD 
                              ( LMark . LmMarkSs 
                                 = PaintHs . MarkSsTyp . MarkSsStartSel 
                                OR ( Selection . Current # NIL 
                                     AND LMark 
                                         = Selection . Current . SelStartMark
                                   ) 
                              ) 
                        ) 
                       (* Except, if we hit this window's StartSel, then 
                          1) We don't want to pass it, and 
                          2) This window's EndSel, if at this same position, 
                             will already have been passed. 
                       *) 
                   THEN (* This MarkRec is for a position beyond where we 
                           want to move. *) 
                     EXIT 
                   ELSE 
                     INC 
                       ( LCursorMovement 
                       , WCursorMark . LmCharPos - LMark . LmCharPos 
                       ) 
                   ; WCursorMark . LmCharPos := LMark . LmCharPos 
                   ; PaintHs . SwapMarkOrder ( LMark ) 
                   ; PaintHs . BruteForceVerifyLineMarks 
                       ( WindowRef . WrImageRef ) 
                   ; TYPECASE WCursorMark . LmLeftLink 
                     OF PaintHs . LineMarkMeatTyp ( TNextMark ) 
                     => LMark := TNextMark 
                     ELSE 
                       EXIT 
                     END (* TYPECASE *) 
                   END (* IF *) 
                 END (* LOOP *) 
               ELSE 
               END (* TYPECASE *) 
             ; INC ( WCursorMark . LmCharPos , LCursorMovement ) 
             ; Search . ClearMatch ( WindowRef , (* VAR *) MustRepaint ) 
             ELSIF LCursorMovement > 0 
             THEN 
               TYPECASE WCursorMark . LmRightLink 
               OF PaintHs . LineMarkMeatTyp ( TFirstMark ) 
               => LMark := TFirstMark 
               ; LOOP 
                   IF NOT Marks . Equal 
                            ( LMark . LmLinesRef . LrBolTokMark 
                            , WCursorMark . LmLinesRef . LrBolTokMark 
                            ) 
                      OR LMark . LmLineNo > WCursorMark . LmLineNo 
                   THEN (* This MarkRec is for a different line. *) 
                     EXIT 
                   ELSIF LMark . LmCharPos 
                      > ( WCursorMark . LmCharPos 
                          + LCursorMovement 
                         (* Don't exit, i.e. do pass other marks that
                            are exactly where we want the cursor to
                            stop (except as below ), so that we will
                            eventually hit and pass this window's
                            StartSel mark, if it is exactly here. 
                         *)
                          - ORD 
                              ( LMark . LmMarkSs 
                                = PaintHs . MarkSsTyp . MarkSsEndSel 
                                OR ( Selection . Current # NIL 
                                     AND LMark 
                                         = Selection . Current . SelEndMark
                                   ) 
                              ) 
                        ) 
                           (* Except, if we hit this window's EndSel, then 
                              1) We don't want to pass it, and 
                              2) This window's EndSel, if at this same 
                                 position, will already have been passed. 
                           *) 
                   THEN (* This MarkRec is for a position beyond where we 
                          want to move. *) 
                     EXIT
                   ELSE 
                     DEC 
                       ( LCursorMovement 
                       , LMark . LmCharPos - WCursorMark . LmCharPos 
                       ) 
                   ; WCursorMark . LmCharPos := LMark . LmCharPos 
                   ; PaintHs . SwapMarkOrder ( WCursorMark ) 
                   ; PaintHs . BruteForceVerifyLineMarks 
                       ( WindowRef . WrImageRef ) 
                   ; TYPECASE WCursorMark . LmRightLink 
                     OF PaintHs . LineMarkMeatTyp ( TNextMark ) 
                     => LMark := TNextMark 
                     ELSE 
                       EXIT 
                     END (* TYPECASE *) 
                   END (* IF *) 
                 END (* LOOP *) 
               ELSE 
               END (* TYPECASE *) 
             ; INC ( WCursorMark . LmCharPos , LCursorMovement ) 
             ; Search . ClearMatch ( WindowRef , (* VAR *) MustRepaint ) 
             ELSE (* LCursorMovement = 0 *) 
             END (* IF *) 
           ; HorizScrollWindowRefToContainCursor ( WindowRef , MustRepaint ) 
           ; IF NOT MustRepaint 
             THEN (* But we moved the cursor, so set it on the screen. *) 
               EditWindow . SetCursorPosition 
                 ( WindowRef 
                 , WCursorMark . LmCharPos - WindowRef . WrHorizScroll 
                 , WindowRef . WrCursorLineNoInWindow 
                 ) 
             ; EditWindow . PaintCursorCoordinates ( WindowRef ) 
             END (* IF *) 
           END (* IF *) 
        END (* WITH WCursorMark *) 
      END (* IF *) 
    END HorizMoveCursorWindowRef 

(* VISIBLE: *) 
; PROCEDURE HorizMoveCursorAndRepaintWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; CursorMovement : EditWindow . CharCoordTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Will scroll WindowRef if necessary to keep cursor inside. *) 

  = VAR LMustRepaint : BOOLEAN := FALSE 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 

  ; BEGIN (* HorizMoveCursorAndRepaintWindowRef *) 
      IF WindowRef # NIL AND WindowRef . WrImageRef # NIL 
      THEN 
        HorizMoveCursorWindowRef ( WindowRef , CursorMovement , LMustRepaint ) 
      ; IF LMustRepaint 
        THEN 
          PaintWindowFromLines 
            ( WindowRef 
            , (* VAR *) LTrailingBlankLines (* Dead. *) 
            , (* VAR *) LLinesRef (* Dead. *) 
            , (* VAR *) LLineNo  (* Dead. *) 
            ) 
(* CHECK: 1) can we only paint this line? 
          2) shouldn't we just mark the window for repainting? 
*)      
        END (* IF *) 
      END (* IF *) 
    END HorizMoveCursorAndRepaintWindowRef 

(* VISIBLE: *) 
; PROCEDURE NonblankLengthOfCurrentLine ( WindowRef : PaintHs . WindowRefTyp ) 
  : LbeStd . CharNoTyp 

  = VAR LImageRef : PaintHs . ImageTransientTyp 
  ; VAR LTempEditRef : PaintHs . TempEditRefTyp 

  ; BEGIN 
      IF WindowRef # NIL 
      THEN
        LImageRef := WindowRef . WrImageRef
      ; IF LImageRef # NIL 
        THEN 
          WITH 
            WCursorMark 
            = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
          DO IF WCursorMark # NIL AND WCursorMark . LmLinesRef # NIL 
             THEN 
               IF LImageRef . ItPers . IpTempEditState 
                  = PaintHs . TempEditStateTyp . TeStateText 
               THEN 
                 LTempEditRef := LImageRef . ItPers . IpTempEditRef 
               ; IF LTempEditRef # NIL 
                    AND LTempEditRef . TeLinesRef = WCursorMark . LmLinesRef 
                    AND LTempEditRef . TeLineNo = WCursorMark . LmLineNo 
                 THEN RETURN Strings . Length ( LTempEditRef . TeEditedString ) 
                 END (* IF *) 
               END (* IF *) 
             ; RETURN WCursorMark . LmLinesRef . LrLineLen
             END (* IF *) 
          END (* WITH *) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN 0 
    END NonblankLengthOfCurrentLine

(* VISIBLE: *) 
; PROCEDURE HorizMoveCursorToEndAndRepaint 
    ( WindowRef : PaintHs . WindowRefTyp ) 
  RAISES { Backout , Thread . Alerted } 

  = BEGIN 
      IF WindowRef # NIL AND WindowRef . WrImageRef# NIL 
      THEN 
        WITH 
          WCursorMark 
          = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
        DO
          HorizMoveCursorAndRepaintWindowRef 
            ( WindowRef 
            , NonblankLengthOfCurrentLine ( WindowRef ) 
              - WCursorMark . LmCharPos 
            ) 
        END (* WITH *) 
      END (* IF *) 
    END HorizMoveCursorToEndAndRepaint 

(* VISIBLE: *) 
; PROCEDURE HorizScrollWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; WindowMovement : LbeStd . LimitedCharNoSignedTyp 
      (* ^Of window frame, relative to contents. *) 
    ; DoDragCursor : BOOLEAN 
      (* ^Means keep cursor in same window-relative position. 
          Even if NOT DoDragCursor, it will be dragged to 
          prevent its getting outside the window. *) 
    ) 
  RAISES { Backout , Thread . Alerted } 

  (* Cursor moves along with window. *) 

  = VAR LMustRepaint : BOOLEAN 
  ; VAR LTrimmedWindowMovement : LbeStd . LimitedCharNoSignedTyp 
  ; VAR LCursorMovement : LbeStd . LimitedCharNoSignedTyp 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 

  ; BEGIN (* HorizScrollWindowRef *) 
      IF WindowRef # NIL AND WindowRef . WrImageRef # NIL 
      THEN
        LMustRepaint := FALSE 
      ; WITH 
          WCursorMark 
            = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
        DO IF WindowMovement < 0 
           THEN 
             LTrimmedWindowMovement 
               := MAX ( WindowMovement , - WindowRef . WrHorizScroll ) 
           (* LTrimmedWindowMovement <= 0 *) 
           ; IF DoDragCursor 
             THEN 
               LCursorMovement := LTrimmedWindowMovement 
             ELSE 
               LCursorMovement 
                 := MIN 
                      ( 0 
                      , EditWindow . SouthEastToCorner ( WindowRef ) . h 
                        + WindowRef . WrHorizScroll 
                        - 1 
                        + LTrimmedWindowMovement (* <= 0 *) 
                        - WCursorMark . LmCharPos 
                      ) 
             END (* IF *) 
           ELSE (* WindowMovement >= 0 *) 
             LTrimmedWindowMovement 
               := MIN 
                    ( WindowMovement 
                    , LbeStd . LimitedCharNoMax 
                      - EditWindow . SouthEastToCorner ( WindowRef ) . h 
                      + 1 
                      - WindowRef . WrHorizScroll 
                    ) 
              (* LTrimmedWindowMovement >= 0 *) 
           ; IF DoDragCursor 
             THEN 
               LCursorMovement := LTrimmedWindowMovement 
             ELSE 
               LCursorMovement 
                 := MAX 
                      ( 0 
                      , WindowRef . WrHorizScroll 
                        + LTrimmedWindowMovement 
                        - WCursorMark . LmCharPos 
                      ) 
             END (* IF *) 
           END (* IF *) 
        ; IF LTrimmedWindowMovement # 0 
          THEN 
            INC ( WindowRef . WrHorizScroll , LTrimmedWindowMovement ) 
          ; LMustRepaint := TRUE 
          END (* IF *) 
        ; HorizMoveCursorWindowRef 
            ( WindowRef , LCursorMovement , LMustRepaint ) 
        END (* WITH WCursorMark *) 
      ; IF LMustRepaint 
        THEN 
          PaintWindowFromLines 
            ( WindowRef 
            , (* VAR *) LTrailingBlankLines (* Dead. *) 
            , (* VAR *) LLinesRef (* Dead. *) 
            , (* VAR *) LLineNo  (* Dead. *) 
            ) 
(* CHECK: shouldn't we just mark the window for repainting? *) 
        END (* IF *) 
      END (* IF *) 
    END HorizScrollWindowRef 

; PROCEDURE MoveLinesRefAndNoUp 
    ( Window : PaintHs . WindowRefTyp 
    ; VAR (* IN OUT *) LinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR (* IN OUT *) LineNo : LbeStd . LineNoTyp 
    ; Movement : LbeStd . LineNoTyp 
    ; VAR (* OUT *) ActualMovement : LbeStd . LineNoSignedTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Adjust LinesRef and LineNo to denote the Movement-th predecessor of
     their original values.  Secure predecessors as needed.  Also, mark each
     LinesRef visited as visible in Window. 
  *) 

  = VAR LMovement : LbeStd . LineNoTyp 

  ; BEGIN (* MoveLinesRefAndNoUp *) 
      LMovement := Movement 
    ; LOOP (* Thru LinesRefs *) 
        PaintHs . IncludeLrVisibleIn ( LinesRef , Window . WrWindowNo ) 
      ; IF LMovement <= LineNo 
        THEN (* We can get to the final position w/in this LinesRef *) 
          DEC ( LineNo , LMovement ) 
        ; LMovement := 0 
        ; EXIT 
        ELSE 
          SecurePred ( Window . WrImageRef , LinesRef ) 
        ; IF LinesRefIsBegOfImage ( Window . WrImageRef , LinesRef ) 
          THEN (* We were already at BegOfImage. *) 
            DEC ( LMovement , LineNo ) 
          ; LineNo := 0 
          ; EXIT 
          ELSE 
            LinesRef := LinesRef . LrLeftLink 
          ; DEC ( LMovement , LineNo + 1 ) 
          ; LineNo 
              := ActualLineCtOfLinesRef ( Window . WrImageRef , LinesRef ) - 1 
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    ; ActualMovement := Movement - LMovement 
    END MoveLinesRefAndNoUp 

; PROCEDURE MoveLinesRefAndNoDown 
    ( Window : PaintHs . WindowRefTyp 
    ; VAR (* IN OUT *) LinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR (* IN OUT *) LineNo : LbeStd . LineNoTyp 
    ; Movement : LbeStd . LineNoTyp 
    ; VAR (* OUT *) ActualMovement : LbeStd . LineNoSignedTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Adjust LinesRef and LineNo to denote the Movement-th succcessor of
     their original values.  Secure succcessors as needed.  Also, mark each
     LinesRef visited as visible in Window. 
  *) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LMovement : LbeStd . LineNoTyp 
  ; VAR LLinesToNext : LbeStd . LineNoTyp 
  ; VAR LTempEditLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LTempEditLineNo : LbeStd . LineNoTyp 
  ; VAR LAtEndOfImage : BOOLEAN 

  ; BEGIN (* MoveLinesRefAndNoDown *) 
      LImagePers := Window . WrImageRef . ItPers 
    ; LMovement := Movement 
    ; IF LImagePers . IpTempEditState 
         = PaintHs . TempEditStateTyp . TeStateText 
         AND LImagePers . IpTempEditRef # NIL 
      THEN 
        LTempEditLinesRef := LImagePers . IpTempEditRef . TeLinesRef 
      ; LTempEditLineNo := LImagePers . IpTempEditRef . TeLineNo 
      ELSE
        LTempEditLinesRef := NIL 
      ; LTempEditLineNo :=  0 
      END (* IF *)  
    ; LOOP (* Thru LinesRefs *) 
        SecureSucc ( Window . WrImageRef , LinesRef ) 
      ; PaintHs . IncludeLrVisibleIn ( LinesRef , Window . WrWindowNo ) 
      ; LAtEndOfImage 
           := LinesRefIsEndOfImage ( Window . WrImageRef , LinesRef ) 
      ; IF LAtEndOfImage AND LinesRef = LTempEditLinesRef 
        THEN (* There is an edited line after the EOI lines ref. *)
          LLinesToNext := LTempEditLineNo + 1 
        ELSE 
          LLinesToNext  
            := ActualLineCtOfLinesRef ( Window . WrImageRef , LinesRef ) 
               - LineNo 
        END (* IF *) 
      ; IF LMovement < LLinesToNext 
        THEN (* We can get to the final position w/in this LinesRef *) 
          INC ( LineNo , LMovement ) 
        ; LMovement := 0 
        ; EXIT 
        ELSIF LinesRefIsEndOfImage ( Window . WrImageRef , LinesRef ) 
        THEN (* We were already at EndOfImage. *) 
          DEC ( LMovement , LLinesToNext - 1 ) 
        ; INC ( LineNo , LLinesToNext - 1 ) 
        ; EXIT 
        ELSE 
          LinesRef := LinesRef . LrRightLink 
        ; DEC ( LMovement , LLinesToNext ) 
        ; LineNo := 0 
        END (* IF *) 
      END (* LOOP *) 
    ; ActualMovement := Movement - LMovement 
    END MoveLinesRefAndNoDown 

; PROCEDURE GetLinesRefAndNoOfEndOfWindow  
    ( Window : PaintHs . WindowRefTyp 
    ; VAR LinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR LineNo : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LMovement : LbeStd . LineNoTyp 
  ; VAR LActualMovement : LbeStd . LineNoSignedTyp 
  
  ; BEGIN 
      LinesRef := Window . WrFirstLineLinesRef 
    ; LineNo := Window . WrFirstLineLineNo 
    ; LMovement := EditWindow . SouthEastToCorner ( Window ) . v - 1 
    ; MoveLinesRefAndNoDown 
        ( Window 
        , (* IN OUT *) LinesRef 
        , (* IN OUT *) LineNo 
        , LMovement 
        , (* VAR *) LActualMovement 
        ) 
    ; Assert 
        ( LActualMovement <= LMovement 
        , AFT . A_GetLinesRefAndNoOfEndOfWindow_Wrong_Movement
        ) 
    END GetLinesRefAndNoOfEndOfWindow  

(* VISIBLE: *) 
; PROCEDURE CreateEmptyLinesRefList 
    ( (* IN OUT *) ImageRef : PaintHs . ImageTransientTyp ) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 

  ; BEGIN (* CreateEmptyLinesRefList *) 
      LImagePers := ImageRef . ItPers 
    ; IF LImagePers . IpLineHeaderRef = NIL 
      THEN 
        LImagePers . IpLineHeaderRef := NEW ( PaintHs . LinesRefHeaderTyp ) 
      END (* IF *) 
    ; LImagePers . IpLineHeaderRef . LrLeftLink 
        := LImagePers . IpLineHeaderRef 
    ; LImagePers . IpLineHeaderRef . LrRightLink 
        := LImagePers . IpLineHeaderRef 
    ; LImagePers . IpLineHeaderRef . LrGapAfter := TRUE 
    END CreateEmptyLinesRefList 

(* FIX: Put some sanity into the willy-nilly mix of 
        PaintHs . WindowRefTyp and EditWindow . WindowTyp ) 
*) 

(* VISIBLE: *) 
; PROCEDURE InitImageFirstWindow ( ImageTrans : PaintHs . ImageTransientTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* Creates a mark list if there is none.  
     Creates a cursor at BOI if there is no cursor. 
     Leaves line list untouched. 
  *) 

  = VAR LLineHeader : PaintHs . LinesRefTyp 
  ; VAR LMarkHeader : PaintHs . LineMarkTyp 
  ; VAR LWindowRef : PaintHs . WindowRefTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LMark : PaintHs . LineMarkTyp 
  ; VAR LCursorMark : PaintHs . LineMarkMeatTyp 
  ; VAR LSavedCallback : Assertions . QueryProcTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LMarkCt : LbeStd . MarkNoTyp 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesListLooksOK : BOOLEAN 

  ; BEGIN (* InitImageFirstWindow *) 
      IF ImageTrans # NIL 
      THEN 
        LImagePers := ImageTrans . ItPers 
      ; LWindowRef := ImageTrans . ItWindowList 
      ; LLineHeader := LImagePers . IpLineHeaderRef 

      (* Handle Lines list. *) 
      ; IF LLineHeader = NIL 
           OR ( LLineHeader . LrRightLink = LLineHeader 
                AND LLineHeader . LrLeftLink = LLineHeader 
              ) 
        THEN (* Absent or empty lines list. *) 
          LLinesListLooksOK := FALSE  
        ELSE 
          TRY 
            LSavedCallback := Assertions . DefaultQueryProc 
          ; Assertions . DefaultQueryProc := Assertions . AlwaysRaise 
          ; TRY 
              TextEdit . BruteForceVerifyAllLinesRefs 
                ( ImageTrans , RepairIsOK := TRUE ) 
            ; LLinesListLooksOK := TRUE   
            EXCEPT Backout 
            => TRY 
                Wr . PutText 
                  ( Stdio . stderr 
                  , "Will rebuild lines list because loading." 
                  ) 
              ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
              ; Wr . Flush ( Stdio . stderr ) 
              EXCEPT Wr . Failure => (* Ignore. *) 
              END (* TRY EXCEPT *) 
            ; LLinesListLooksOK := FALSE  
            END (* TRY EXCEPT *) 
          FINALLY Assertions . DefaultQueryProc := LSavedCallback 
          END (* TRY FINALLY *) 
        END (* IF *) 

      (* Handle mark list. *) 
      ; LMarkHeader := LImagePers . IpMarkHeader 
      ; LCursorMark := NIL 
      ; IF LMarkHeader = NIL 
        THEN (* Absent mark list.  Construct an empty one. *) 
          LMarkHeader := NEW ( PaintHs . LineMarkHeaderTyp ) 
        ; LMarkHeader . LmLeftLink 
            := LMarkHeader 
        ; LMarkHeader . LmRightLink 
            := LMarkHeader 
        ; LImagePers . IpMarkCt := 0 
        ; LImagePers . IpMarkHeader := LMarkHeader 
        ELSIF ( LMarkHeader . LmRightLink = LMarkHeader 
                AND LMarkHeader . LmLeftLink = LMarkHeader 
              ) 
        THEN (* Empty mark list. *) 
          LImagePers . IpMarkCt := 0  
        ELSE 
          TRY 
            LSavedCallback := Assertions . DefaultQueryProc 
          ; Assertions . DefaultQueryProc := Assertions . AlwaysRaise 
          ; TRY 
              PaintHs . BruteForceVerifyLineMarks 
                ( ImageTrans , DoCheckOrder := TRUE ) 
            ; LMark := LMarkHeader 
            ; LMarkCt := 0 
            (* Patch LmWindowRef of all marks to this window. 
               Also look for a cursor mark. 
            *) 
            ; LOOP 
                TYPECASE LMark . LmRightLink 
                OF PaintHs . LineMarkMeatTyp ( TRightMark ) 
                   (* NIL can't happen. *) 
                => LMark := TRightMark 
                ; IF TRightMark . LmMarkSs 
                     = PaintHs . MarkSsTyp . MarkSsCursor
                  THEN (* Found a cursor mark. *) 
                    IF LCursorMark = NIL 
                    THEN (* And it's the first one. *) 
                      TRightMark . LmWindowRef := LWindowRef  
                    ; LCursorMark := TRightMark 
                    ELSE (* Subsequent cursor.  Get rid of it. *) 
                      IF LLinesListLooksOK 
                      THEN PaintHs . RecomputeLrHasMark ( TRightMark ) 
                      END (* IF *) 
                    ; PaintHs . UnlinkLineMark ( TRightMark )  
                    END (* IF *) 
                  END (* IF *) 
                ; IF LLinesListLooksOK 
                  THEN 
                    IF TRightMark . LmLinesRef = NIL 
                       OR NOT Marks . Equal 
                                ( TRightMark . LmTokMark 
                                , TRightMark . LmLinesRef . LrBolTokMark 
                                ) 
                    THEN (* Not linked to a correct LinesRef. *) 
                      LLinesListLooksOK := FALSE  
                    END (* IF *)  
                  END (* IF *) 
                ; INC ( LMarkCt )  
                ELSE 
                  EXIT 
                END (* TYPECASE *) 
              END (* LOOP *) 
            ; IF LMarkCt # LImagePers . IpMarkCt 
              THEN 
                Assertions . MessageText 
                  ( "Reparing IpMarkCt from " 
                    & Fmt . Int ( LImagePers . IpMarkCt ) 
                    & " to " 
                    & Fmt . Int ( LMarkCt ) 
                  ) 
              ; LImagePers . IpMarkCt := LMarkCt 
              END (* IF *) 
            EXCEPT Backout 
            => (* Something is wrong with the mark list. *) 
              TRY 
                Wr . PutText 
                  ( Stdio . stderr 
                  , "Will rebuild one cursor mark at BOI because loading." 
                  ) 
              ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
              ; Wr . Flush ( Stdio . stderr ) 
              EXCEPT Wr . Failure => (* Ignore *) 
              END (* TRY EXCEPT *) 

            (* This method of emptying the mark list does not depend on
               its being well-formed. 
            *) 
            ; LMarkHeader . LmRightLink := LMarkHeader 
            ; LMarkHeader . LmLeftLink := LMarkHeader 
            ; IF LLinesListLooksOK 
              THEN PaintHs . ResetAllLrHasMarkFields ( ImageTrans ) 
              END (* IF *) 
            ; LCursorMark := NIL 
            END (* TRY EXCEPT *) 
          FINALLY Assertions . DefaultQueryProc := LSavedCallback 
          END (* TRY FINALLY *) 
        END (* IF *) 
(* TODO: Generalize the above for other marks and multiple windows. *) 
      ; IF LCursorMark = NIL  
        THEN (* No cursor mark exists.  Create one cursor mark at BOI *) 
          LCursorMark := NEW ( PaintHs . LineMarkMeatTyp ) 
        ; LCursorMark . LmMarkSs := PaintHs . MarkSsTyp . MarkSsCursor 
        ; LineMarks . GetRMBegOfImage 
            ( LImagePers . IpLang 
            , LImagePers . IpEstRoot 
            , (* VAR *) LCursorMark . LmTokMark 
            ) 
        ; LCursorMark . LmCharPos := 0 
        ; LCursorMark . LmLineNo := 0 
        ; LCursorMark . LmWindowRef := LWindowRef 
        ; LCursorMark . LmLinesRef := NIL  
        ; PaintHs . LinkLineMarkToRight ( LMarkHeader , LCursorMark ) 
        ; IF LLinesListLooksOK 
          THEN 
            IF LLineHeader . LrGapAfter 
            THEN
              LLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
            ; LLinesRef . LrBolTokMark := LCursorMark . LmTokMark 
            ; LLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
            ; LLinesRef . LrFromPos := LbeStd . LimitedCharNoNull 
            ; LLinesRef . LrLineLen := 0 
            ; LLinesRef . LrLineText := NIL 
            ; LLinesRef . LrLineCt := 0 
            ; LLinesRef . LrGapAfter := TRUE 
            ; LLinesRef . LrIsStopper := TRUE 
            ; LLinesRef . LrHasMark := TRUE 
            ; PaintHs . InsertLinesRefToLeft 
                ( InsertToLeftOfRef := LLineHeader 
                , RefToInsert := LLinesRef 
                ) 
            ; LCursorMark . LmLinesRef := LLinesRef 
            ELSE 
              LCursorMark . LmLinesRef := LLineHeader . LrRightLink 
            ; NARROW ( LLineHeader . LrRightLink , PaintHs . LinesRefMeatTyp ) 
              . LrHasMark 
              := TRUE 
            END (* IF *) 
          END (* IF *) 
        ; INC ( LImagePers . IpMarkCt ) 
        ; LWindowRef . WrCursorLineNoInWindow := 0 
        ; PaintHs . BruteForceVerifyLineMarks ( ImageTrans ) 
        ELSE (* We found a cursor.  Put it in the middle of the window. *)
          LWindowRef . WrCursorLineNoInWindow
            := EditWindow . SouthEastToCorner ( LWindowRef ) . v DIV 2 
        END (* IF *) 
      ; LWindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
          := LCursorMark 
      ; IF LLinesListLooksOK 
        THEN 
          PaintWindowFromCursor 
            ( LWindowRef , (* VAR *) LTrailingBlankLines (* Dead. *) ) 
        ELSE 
          ReconstructLinesAndPaint ( ImageTrans ) 
        END (* IF *) 
      END (* IF *) 
    ; NoteImageSavedState ( ImageTrans , ImageTrans . ItIsSaved ) 
    ; NoteImageParsedState  
        ( ImageTrans , ImageTrans . ItPers . IpIsParsed )
    ; NoteImageAnalyzedState 
        ( ImageTrans , ImageTrans . ItPers . IpIsAnalyzed ) 
      (* ItIsSaved, IpIsParsed, and IpIsAnalyzed were set before we had 
         a window attached. *) 
    END InitImageFirstWindow 

; PROCEDURE AdjustEstimatesForMaybeBegOfImage 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
    ) 

  = BEGIN 
      IF NOT WindowRef . WrVertScrollIsExact
         AND LinesRefIsBegOfImage 
               ( WindowRef . WrImageRef , WindowRef . WrFirstLineLinesRef )  
      THEN
        IF WindowRef . WrVertScroll # WindowRef . WrFirstLineLineNo 
        THEN 
          WindowRef . WrVertScroll := WindowRef . WrFirstLineLineNo 
        ; MustRepaint := TRUE 
(* TODO: Separate repainting the vert. scroller from other repaints. *) 
        END (* IF *) 
      ; WindowRef . WrVertScrollIsExact := TRUE
      END (* IF *) 
    END AdjustEstimatesForMaybeBegOfImage  

(* VISIBLE: *) 
; PROCEDURE PaintWindowFromCursor 
    ( (* IN OUT *) WindowRef : PaintHs . WindowRefTyp 
    ; VAR BlankLinesAtEnd : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout , Thread . Alerted }   
  (* The cursor must point to a valid LinesRef. *) 

  = VAR LImageTrans : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LActualMovement : LbeStd . LineNoSignedTyp 
  ; VAR LMustRepaint : BOOLEAN 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LThruLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 

  ; BEGIN (* PaintWindowFromCursor *) 
      LMustRepaint := FALSE 
    ; IF WindowRef # NIL 
      THEN 
        LImageTrans := WindowRef . WrImageRef  
      ; IF LImageTrans # NIL 
        THEN 
          LImagePers := LImageTrans . ItPers 
        ; WITH 
            WCursorMark 
              = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
          DO
            LineNumbers . EstimateLineNo
              ( WCursorMark 
              , LImageTrans 
              , (* VAR *) WindowRef . WrVertScroll 
              , (* VAR *) WindowRef . WrVertScrollIsExact 
              )  
          ; SecureSucc ( LImageTrans , WCursorMark . LmLinesRef ) 
          ; WindowRef . WrFirstLineLinesRef := WCursorMark . LmLinesRef 
          ; WindowRef . WrFirstLineLineNo := WCursorMark . LmLineNo 
          ; PaintHs . IncludeLrVisibleIn 
              ( WindowRef . WrFirstLineLinesRef , WindowRef . WrWindowNo ) 
          ; MoveLinesRefAndNoUp 
              ( Window := WindowRef
              , (* IN OUT *) LinesRef := WindowRef . WrFirstLineLinesRef 
              , (* IN OUT *) LineNo := WindowRef . WrFirstLineLineNo 
              , Movement := WindowRef . WrCursorLineNoInWindow 
              , (* VAR *) ActualMovement := LActualMovement 
              ) 
          ; WindowRef . WrCursorLineNoInWindow := LActualMovement 
          ; WindowRef . WrVertScroll 
              := MAX ( 0 , WindowRef . WrVertScroll - LActualMovement ) 
          ; AdjustEstimatesForMaybeBegOfImage ( WindowRef , LMustRepaint ) 
          ; HorizScrollWindowRefToContainCursor ( WindowRef , LMustRepaint ) 
          ; PaintWindowFromLines 
              ( WindowRef 
              , (* VAR *) BlankLinesAtEnd 
              , (* VAR *) ToLinesRef := LLinesRef 
              , (* VAR *) ToLineNo := LLineNo 
              ) 
(* CHECK: shouldn't we just mark the window for repainting? *) 
          ; TYPECASE LImagePers . IpLineHeaderRef . LrRightLink 
            OF PaintHs . LinesRefMeatTyp ( TFromLinesRef )  
            => TYPECASE WindowRef . WrFirstLineLinesRef . LrLeftLink 
              OF PaintHs . LinesRefMeatTyp ( TThruLinesRef )  
              => MakeLinesRefsNotVisibleInWindow 
                   ( LImageTrans 
                   , (* VAR *) TFromLinesRef 
                   , (* VAR *) TThruLinesRef 
                   , WindowRef . WrWindowNo 
                   ) 
              ELSE  
              END (* TYPECASE *) 
            ELSE 
            END (* TYPECASE *) 
          ; IF LLineNo > 0 
               AND LLinesRef . LrRightLink # LImagePers . IpLineHeaderRef 
            THEN (* In the middle of a LinesRef. *)  
              LLinesRef := LLinesRef . LrRightLink 
              (* ^NARROW can't fail. *)
            END (* IF *) 
          ; LThruLinesRef := LImagePers . IpLineHeaderRef . LrLeftLink 
          ; MakeLinesRefsNotVisibleInWindow 
              ( LImageTrans 
              , (* VAR *) FromLinesRef := LLinesRef 
              , (* VAR *) ThruLinesRef := LThruLinesRef 
              , WindowNo := WindowRef . WrWindowNo 
              )  
          END (* IF *) 
        END (* WITH WCursorMark *) 
      END (* IF *) 
    END PaintWindowFromCursor 

(* VISIBLE: *) 
; PROCEDURE ReconstructLinesAndPaint 
    ( (* IN OUT *) ImageRef : PaintHs . ImageTransientTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Any temp edits have been flushed.  
     Depends only on the list of MarkRecs.  Completely rebuilds 
     LinesRefs for the image and all the pointers thereto from 
     the MarkRecs and WindowRecs. Then repaints all windows of 
     this image. 
  *) 

(* CHECK: When do we really need to do this drastic a rebuild of 
          lines refs? What about doing it without the throwaway 
          and rebuild of the marks in the 1st pass? *) 

  = VAR LMark : PaintHs . LineMarkMeatTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 

  ; BEGIN (* ReconstructLinesAndPaint *) 
      IF ImageRef # NIL 
      THEN 
        CreateEmptyLinesRefList ( ImageRef ) 
      ; LLinesRef := NIL 
      ; TYPECASE ImageRef . ItPers . IpMarkHeader . LmRightLink 
        OF PaintHs . LineMarkMeatTyp ( TFirstMark ) 
        => LMark := TFirstMark 
          (* Make a pass thru the MarkRecs of the image, creating a LinesRef 
             for each. This is necessary so that normal succ/pred building, 
             done during the 2nd pass, will properly connect to the LinesRefs 
             of other, newly marked points. *) 
        ; LOOP 
            IF LLinesRef = NIL 
               OR NOT Marks . Equal 
                        ( LMark . LmTokMark , LLinesRef . LrBolTokMark ) 
            THEN 
              LLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
            ; LLinesRef . LrBolTokMark := LMark . LmTokMark 
            ; LLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
            ; LLinesRef . LrFromPos := LbeStd . LimitedCharNoNull 
            ; LLinesRef . LrLineLen := 0 
            ; LLinesRef . LrLineText := NIL 
            ; LLinesRef . LrLineCt := 0 
            ; LLinesRef . LrGapAfter := TRUE 
            ; LLinesRef . LrIsStopper := TRUE 
            ; LLinesRef . LrHasMark := TRUE 
            ; PaintHs . InsertLinesRefToLeft 
                ( InsertToLeftOfRef := ImageRef . ItPers . IpLineHeaderRef 
                , RefToInsert := LLinesRef 
                ) 
            END (* IF *) 
          ; LMark . LmLinesRef := LLinesRef 
          ; TYPECASE LMark . LmRightLink 
            OF PaintHs . LineMarkMeatTyp ( TNextMark ) 
            => LMark := TNextMark 
            ELSE 
              EXIT 
            END (* TYPECASE *) 
          END (* LOOP *) 
        (* Make a second pass thru MarkRecs, calling 
           PaintWindowFromCursor for those that are cursors. *) 
        ; SecurePred ( ImageRef , TFirstMark . LmLinesRef ) 
        ; LMark := TFirstMark 
        ; LOOP 
            IF LMark . LmMarkSs = PaintHs . MarkSsTyp . MarkSsCursor 
            THEN              
              LMark . LmWindowRef . WrMarks 
                [ PaintHs . MarkSsTyp . MarkSsCursor ]
              := LMark (* Just in case *) 
(* TODO: In case of damage, there could be > 1 cursor mark for one window 
         in the marks list.  The window can identify only one.  This will
         keep setting the window to identify each in turn (and repainting).
         Better to get the duplicates out of the mark list altogether, 
         maybe in the first pass.
*)  
            ; LMark . LmWindowRef . WrHorizScroll := 0 
            ; PaintWindowFromCursor 
                ( LMark . LmWindowRef , (* VAR *) LTrailingBlankLines ) 
            ; IF LTrailingBlankLines > 0 
              THEN 
                VertScrollAndRepaint  
                  ( LMark . LmWindowRef 
                  , WantedMovement := - LTrailingBlankLines 
                  , DoDragCursor := FALSE 
                  ) 
              END (* IF *) 
            END (* IF *) 
          ; TYPECASE LMark . LmRightLink 
            OF PaintHs . LineMarkMeatTyp ( TNextMark ) 
            => LMark := TNextMark 
            ELSE 
              EXIT 
            END (* TYPECASE *) 
          END (* LOOP *) 
        ELSE 
          CantHappen ( AFT . A_ReconstructLinesAndPaint_EmptyLineMarkList ) 
        END (* TYPECASE *) 
      ; TextEdit . BruteForceVerifyAllLinesRefs 
          ( ImageRef 
          , RepairIsOK := TRUE 
(* TODO:    ^ Rbm can't tell, in some cases, when an optional line break is
            taken, because it can't ascertain FmtKind for FsSubtrees, while
            descending (and maybe also when traversing backward?).  This can
            result in Rbm's stopping and constructing a LineMark at a Nl that
            is not the Rightmost. (The rightmost is a conditional line break
            that Rbm does not know is taken.  Gnl will find the real 
            rightmost Nl and it will be patched. *) 
          ) 
      END (* IF *) 
; PaintHs . DisplayTextAttrStats ( ImageRef ) 
    END ReconstructLinesAndPaint 

; <* UNUSED *> 
  PROCEDURE VerifyLinesRefToUnlink 
    ( FromLinesRef : PaintHs . LinesRefMeatTyp 
    ; ThruLinesRef : PaintHs . LinesRefMeatTyp 
    ) 
  RAISES { Backout } 

  = VAR LLinesRef : PaintHs . LinesRefMeatTyp 

  ; BEGIN 
      LLinesRef := FromLinesRef 
    ; LOOP 
        Assert 
          ( NOT LLinesRef . LrGapAfter 
            OR LLinesRef = ThruLinesRef 
          , AFT . A_VerifyLinesRefToUnlink_GapAfter 
          ) 
      ; Assert 
          ( NOT ThruLinesRef . LrHasMark  
          , AFT . A_VerifyLinesRefToUnlink_HasMark  
          ) 
      ; Assert 
          ( NOT PaintHs . LineIsVisible ( LLinesRef ) 
          , AFT . A_VerifyLinesRefToUnlink_IsVisible  
          ) 
      ; IF LLinesRef = ThruLinesRef
        THEN EXIT
        ELSE LLinesRef := LLinesRef . LrRightLink 
        END (* IF *) 
      END (* LOOP *) 
    END VerifyLinesRefToUnlink 

(* VISIBLE: *) 
; PROCEDURE MakeLinesRefsNotVisible 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; VAR (* IN OUT *) FromLinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR (* IN OUT *) ThruLinesRef : PaintHs . LinesRefMeatTyp 
      (* ^After return, FromLinesRef, ThruLinesRef, and/or 
         any of the LinesRefs between might have been unlinked. 
         None of these pointers should be used or stored 
         by the caller. *) 
    ) 
  RAISES { Backout } 
  (* FromLinesRef .. ThruLinesRef, belonging to ImageRef, have 
     become invisible altogether.  Unlink any LinesRefs which 
     should no longer be retained. *) 

  = VAR LFromLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LThruLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LTempLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLinesRefCt : PortTypes . Card32Typ 
  ; VAR LStopperCt : PortTypes . Card32Typ 
  ; VAR LDidPassFromLinesRef : BOOLEAN 

  ; BEGIN (* MakeLinesRefsNotVisible *) 
      IF ImageRef # NIL AND FromLinesRef # NIL AND ThruLinesRef # NIL 
      THEN 
        Assert 
          ( NOT FromLinesRef . LrHasMark  
          , AFT . A_MakeLinesRefsNotVisible_FromHasMark 
          ) 
      ; Assert 
          ( NOT ThruLinesRef . LrHasMark  
          , AFT . A_MakeLinesRefsNotVisible_ThruHasMark 
          ) 
   (* ; VerifyLinesRefToUnlink ( FromLinesRef , ThruLinesRef ) 
        Not necessarily true *) 
      ; LDidPassFromLinesRef := FALSE 
      ; LThruLinesRef := ThruLinesRef 
        (* Look below the newly invisible LinesRefs. We will not discard 
           any that are within Options.LinesRefsToRetain of a visible line, 
           or similarly for Options.StoppersToRetain. *) 
      ; LOOP 
          IF LThruLinesRef . LrGapAfter 
             OR LThruLinesRef . LrRightLink 
                = ImageRef . ItPers . IpLineHeaderRef 
          THEN (* Leave LThruLinesRef right here. *) 
            EXIT 
          ELSE 
            LThruLinesRef := LThruLinesRef . LrRightLink 
          ; IF LThruLinesRef . LrHasMark 
            THEN (* Must stop short of LThruLinesRef *) 
              LThruLinesRef := LThruLinesRef . LrLeftLink 
            ; EXIT 
            ELSIF PaintHs . LineIsVisible ( LThruLinesRef ) 
            THEN (* We have hit a visible LinesRef.  Now start moving 
                    backwards, counting the LinesRefs that this visible 
                    LinesRef causes to be retained. *) 
              LLinesRefCt := 0 
            ; LStopperCt := 0 
            ; LOOP 
                (* INVARIANT LLinesRefCt and LStopperCt count the 
                             invisible/stopper LinesRefs, respectively. 
                   INVARIANT We still need more LinesRefs. *) 
                IF LThruLinesRef = FromLinesRef 
                THEN 
                  LDidPassFromLinesRef := TRUE 
                END (* IF *) 
              ; IF LThruLinesRef . LrLeftLink . LrGapAfter 
                   OR LThruLinesRef . LrLeftLink 
                      = ImageRef . ItPers . IpLineHeaderRef 
                   OR LThruLinesRef . LrHasMark 
                THEN (* There aren't enough LinesRefs above the visible 
                        one below, so retain everything. *) 
                  RETURN 
                ELSE 
                  LThruLinesRef := LThruLinesRef . LrLeftLink 
                ; IF LLinesRefCt >= Options . LinesRefsToRetain 
                     AND LStopperCt >= Options . StoppersToRetain 
                  THEN (* LThruLinesRef is the last to unlink. *) 
                    EXIT 
                  ELSE 
                    INC ( LLinesRefCt ) 
                  ; INC ( LStopperCt , ORD ( LThruLinesRef . LrIsStopper ) ) 
                  END (* IF *) 
                END (* IF *) 
              END (* LOOP *) 
            ; EXIT 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      (* Now search upward. The same story in that direction, except we are 
         not interested in stoppers. *) 
      ; IF LDidPassFromLinesRef 
        THEN 
          LFromLinesRef := LThruLinesRef 
        ELSE 
          LFromLinesRef := FromLinesRef 
        END (* IF *) 
      ; LOOP 
          IF LFromLinesRef . LrLeftLink . LrGapAfter 
             OR LFromLinesRef . LrLeftLink 
                = ImageRef . ItPers . IpLineHeaderRef 
          THEN (* This is the end of the region to unlink. *) 
            EXIT 
          ELSE 
            LFromLinesRef := LFromLinesRef . LrLeftLink 
          ; IF LFromLinesRef . LrHasMark 
            THEN (* Went one too far.  Must stop short of this one. *) 
              LFromLinesRef := LFromLinesRef . LrRightLink 
            ; EXIT 
            ELSIF PaintHs . LineIsVisible ( LFromLinesRef ) 
            THEN (* We have hit a visible LinesRef.  Now start moving 
                    forwards, counting the LinesRefs that this visible 
                    LinesRef causes to be retained. 
                 *) 
              LFromLinesRef := LFromLinesRef . LrRightLink 
            ; LLinesRefCt := 0 
            (* We don't count stoppers in this direction. *) 
            ; LOOP (* INVARIANT LLinesRefCt counts invisible LinesRefs. 
                      INVARIANT We still need more LinesRefs. *) 

                IF LLinesRefCt >= Options . LinesRefsToRetain 
                THEN (* LFromLinesRef is 1st to unlink. *) 
                  EXIT 
                ELSE 
                  IF LFromLinesRef = LThruLinesRef 
                  THEN (* We are about to pass LThruLinesRef. 
                          Don't unlink anything. *) 
                    RETURN 
                  ELSE 
                    LFromLinesRef := LFromLinesRef . LrRightLink 
                  ; INC ( LLinesRefCt ) 
                  END (* IF *) 
                END (* IF *) 
              END (* LOOP *) 
            ; EXIT 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
        (* If we get here, unlink LFromLinesRef .. LThruLinesRef *) 
   (* ; VerifyLinesRefToUnlink ( LFromLinesRef , LThruLinesRef ) 
        Not necessarily true *)
      ; IF LThruLinesRef . LrGapAfter 
           AND LThruLinesRef # LFromLinesRef 
        THEN (* We can reuse it in the now widened gap. *) 
          LThruLinesRef . LrBolTokMark := LFromLinesRef . LrBolTokMark 
        ; LThruLinesRef := LThruLinesRef . LrLeftLink 
        ELSE (* Create a new GapAfter LinesRef for the newly opened gap. *) 
          LTempLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
        ; LTempLinesRef . LrBolTokMark := LFromLinesRef . LrBolTokMark 
        ; LTempLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
        ; LTempLinesRef . LrLineCt := 0 
        ; LTempLinesRef . LrFromPos := LbeStd . LimitedCharNoNull  
        ; LTempLinesRef . LrLineText := "" 
        ; LTempLinesRef . LrLineLen := 0 
        ; LTempLinesRef . LrGapAfter := TRUE 
        ; LTempLinesRef . LrHasMark := FALSE 
        ; LTempLinesRef . LrIsStopper := FALSE 
        ; PaintHs . InsertLinesRefToLeft 
            ( InsertToLeftOfRef := LFromLinesRef 
            , RefToInsert := LTempLinesRef 
            ) 
        END (* IF *) 
      ; PaintHs . UnlinkLinesRefRange ( LFromLinesRef , LThruLinesRef ) 
      END (* IF *) 
    END MakeLinesRefsNotVisible 

(* VISIBLE: *) 
; PROCEDURE MakeLinesRefsNotVisibleInWindow 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; VAR (* IN OUT *) FromLinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR (* IN OUT *) ThruLinesRef : PaintHs . LinesRefMeatTyp 
      (* ^FromLinesRef, ThruLinesRef, and/or any of the lines 
         between might have been unlinked.  None of these 
         pointers should be used or stored by the caller. *) 
    ; WindowNo : PaintHs . WindowNoTyp 
    ) 
  RAISES { Backout } 
  (* FromLinesRef .. ThruLinesRef are no longer visible in WindowNo. 
     Note this in the LinesRefs' LrVisibleIn fields. 
     If some of the LinesRefs become invisible altogether, 
     call MakeLinesRefsNotVisible for them. *) 

  = VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LFromInvisibleRef : PaintHs . LinesRefMeatTyp 
  ; VAR LThruInvisibleRef : PaintHs . LinesRefMeatTyp 

  ; BEGIN (* MakeLinesRefsNotVisibleInWindow *) 
      IF ImageRef # NIL AND FromLinesRef # NIL AND ThruLinesRef # NIL 
      THEN 
        TRY 
          Assert 
            ( Marks . Compare 
                ( FromLinesRef . LrBolTokMark , ThruLinesRef . LrBolTokMark ) 
              <= 0 
            , AFT . A_MakeLinesRefsNotVisibleInWindow_Out_of_order_range 
            ) 
        EXCEPT Marks . Unordered 
        => CantHappen 
             ( AFT . A_MakeLinesRefsNotVisibleInWindow_Unordered_range) 
        END (* TRY EXCEPT *) 
      ; IF NOT FromLinesRef . LrGapAfter 
           AND FromLinesRef . LrRightLink 
               = ImageRef . ItPers . IpLineHeaderRef 
        THEN (* Do not unlink the EOI lines ref. *) 
          RETURN 
        ELSIF FromLinesRef . LrGapAfter 
              AND NOT FromLinesRef . LrLeftLink . LrGapAfter 
        THEN (* Do not unlink FromLinesRef *) 
          IF FromLinesRef = ThruLinesRef 
          THEN RETURN 
          ELSE LLinesRef := FromLinesRef . LrRightLink 
          END (* IF *) 
        ELSE LLinesRef := FromLinesRef 
        END (* IF *) 
      ; LFromInvisibleRef := NIL 
      ; LThruInvisibleRef := NIL 
      ; LOOP 
          PaintHs . ExcludeLrVisibleIn ( LLinesRef , WindowNo ) 
        ; IF PaintHs . LineIsVisible ( LLinesRef ) 
             OR LLinesRef . LrHasMark 
          THEN (* Do not unlink this line. *) 
            IF LFromInvisibleRef # NIL 
            THEN (* Some previous lines to unlink. *) 
              MakeLinesRefsNotVisible 
                ( ImageRef , LFromInvisibleRef , LThruInvisibleRef ) 
            ; LFromInvisibleRef := NIL 
            ; LThruInvisibleRef := NIL 
            END (* IF *) 
          ELSE (* This line can be unlinked. *) 
            IF LFromInvisibleRef = NIL 
            THEN (* Make it the beginning of a range. *) 
              LFromInvisibleRef := LLinesRef 
            END (* IF *) 
          ; LThruInvisibleRef := LLinesRef (* Make it the end of a range. *)  
          END (* IF *) 
        ; IF LLinesRef = ThruLinesRef 
          THEN EXIT 
          ELSE 
            LLinesRef := LLinesRef . LrRightLink 
          END (* IF *) 
        END (* LOOP*) 
      ; IF LFromInvisibleRef # NIL 
        THEN (* Some previous lines to unlink. *) 
          MakeLinesRefsNotVisible 
            ( ImageRef , LFromInvisibleRef , LThruInvisibleRef ) 
        END (* IF *) 
      END (* IF *) 
    END MakeLinesRefsNotVisibleInWindow 

; PROCEDURE MakeLinesRefsAndNosNotVisibleInWindow 
    ( (* UNCHANGED *) ImageRef : PaintHs . ImageTransientTyp 
    ; FromLinesRef : PaintHs . LinesRefMeatTyp 
    ; FromLineNo : LbeStd . LineNoTyp 
    ; ThruLinesRef : PaintHs . LinesRefMeatTyp 
    ; ThruLineNo : LbeStd . LineNoTyp 
      (* ^FromLinesRef, ThruLinesRef, and/or any of the lines 
         between might have been unlinked.  None of these 
         pointers should be used or stored by the caller. *) 
    ; WindowNo : PaintHs . WindowNoTyp 
    ) 
  RAISES { Backout } 

  = VAR LFromLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LThruLinesRef : PaintHs . LinesRefMeatTyp 

  ; BEGIN (* MakeLinesRefsAndNosNotVisibleInWindow *) 
      LFromLinesRef := FromLinesRef 
    ; LThruLinesRef := ThruLinesRef 
    ; IF FromLineNo > 0 
      THEN (* Some of LFromLinesRef's lines are not made invisible, 
              so remove LFromLinesRef from the range. *) 
        IF LFromLinesRef = LThruLinesRef 
        THEN 
          RETURN 
        ELSE 
          LFromLinesRef := LFromLinesRef . LrRightLink 
        END (* IF *) 
      END (* IF *) 
    ; IF ThruLineNo < ActualLineCtOfLinesRef ( ImageRef , LThruLinesRef ) - 1 
      THEN (* Some of LThruLinesRef's lines are not made invisible, 
              so remove LFromLinesRef from the range. *) 
        IF LFromLinesRef = LThruLinesRef 
        THEN 
          RETURN 
        ELSE 
          LThruLinesRef := LThruLinesRef . LrLeftLink 
        END (* IF *) 
      END (* IF *) 
    ; MakeLinesRefsNotVisibleInWindow 
        ( ImageRef 
        , (* VAR *) LFromLinesRef 
        , (* VAR *) LThruLinesRef 
        , WindowNo 
        ) 
    END MakeLinesRefsAndNosNotVisibleInWindow 

; PROCEDURE MoveCursorNoScroll 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; WantedMovement : EditWindow . CharPointTyp 
    ; VAR (* OUT *) ActualMovement : EditWindow . CharPointTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
      (* Set true if anything requiring a repaint happens. *) 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* Move thru the necessary LinesRefs and MarkRecs for the cursor move. 
     DOES NOT update WindowRef . WrCursorLineNoInWindow 
  *)

  = VAR McImageRef : PaintHs . ImageTransientTyp 
  ; VAR McVertMovementRemaining : LbeStd . LineNoSignedTyp 

  ; PROCEDURE McLinesToNext ( Mark : PaintHs . LineMarkMeatTyp ) 
    : LbeStd . LineNoSignedTyp  
    RAISES { Backout , Thread . Alerted } 
    (* Has SIDE EFFECT of calling SecureSucc. *) 

    = VAR LLinesToLastNonblankLine : LbeStd . LineNoSignedTyp 
    ; VAR LLinesToNext : LbeStd . LineNoSignedTyp 

    ; BEGIN 
        SecureSucc ( McImageRef , Mark . LmLinesRef ) 
      ; IF LinesRefIsEndOfImage ( McImageRef , Mark . LmLinesRef ) 
        THEN (* We are already at EndOfImage LinesRef. *) 
          IF McImageRef . ItPers . IpTempEditState 
             = PaintHs . TempEditStateTyp . TeStateText 
             AND McImageRef . ItPers . IpTempEditRef # NIL 
             AND McImageRef . ItPers . IpTempEditRef . TeLinesRef 
                 = Mark . LmLinesRef 
          THEN 
            LLinesToLastNonblankLine 
              := McImageRef . ItPers . IpTempEditRef . TeLineNo  
          ELSE 
            LLinesToLastNonblankLine := - 1 
          END (* IF *) 
        ; LLinesToNext 
            := EditWindow . SouthEastToCorner ( WindowRef ) . v 
               + LLinesToLastNonblankLine 
               - Mark . LmLineNo   
        ; McVertMovementRemaining 
            := MIN ( McVertMovementRemaining , LLinesToNext - 1 ) 
        ELSE 
          LLinesToNext 
            := ActualLineCtOfLinesRef 
                 ( McImageRef , Mark . LmLinesRef ) 
               - Mark . LmLineNo 
        END (* IF *) 
      ; RETURN LLinesToNext 
      END McLinesToNext

  ; BEGIN (* MoveCursorNoScroll *)   

      VAR LFinalHorizPos : LbeStd . LimitedCharNoTyp 
    ; VAR LLinesToNext : LbeStd . LineNoSignedTyp 
    ; VAR LAreWithinThisLinesRef : BOOLEAN 

    ; BEGIN (* Block for MoveCursorNoScroll *) 
        IF WindowRef # NIL 
        THEN 
          McImageRef := WindowRef . WrImageRef 
        ; IF McImageRef # NIL 
          THEN 
            WITH 
              WCursorMark 
              = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
            DO LFinalHorizPos 
                 := MAX 
                      ( 0 
                      , MIN 
                          ( LbeStd . LimitedCharNoMax 
                          , WCursorMark . LmCharPos + WantedMovement . h 
                          ) 
                      ) 
            ; ActualMovement . h := LFinalHorizPos - WCursorMark . LmCharPos 
            ; ActualMovement . v := 0 
            ; McVertMovementRemaining := WantedMovement . v 
            ; IF McVertMovementRemaining < 0 
              THEN 
                SecurePred ( McImageRef , WCursorMark . LmLinesRef ) 
              ; IF WCursorMark . LmLinesRef . LrLeftLink 
                   = McImageRef . ItPers . IpLineHeaderRef 
                THEN (* We are already at BegOfImage LinesRef. *) 
                  McVertMovementRemaining 
                    := MAX ( McVertMovementRemaining 
                           , - WCursorMark . LmLineNo 
                           ) 
                END (* IF *) 
              ; LAreWithinThisLinesRef 
                  := - McVertMovementRemaining <= WCursorMark . LmLineNo 
              ; IF NOT LAreWithinThisLinesRef 
                THEN
                  PaintHs . RecomputeLrHasMark ( WCursorMark ) 
                END (* IF *) 
              ; LOOP (* Thru LinesRefs *) 
                  IF LAreWithinThisLinesRef 
                  THEN (* We can get to final position w/in this LinesRef *) 
                    INC ( WCursorMark . LmLineNo 
                        , McVertMovementRemaining (* <= 0 *) 
                        )
                  ; INC ( ActualMovement . v , McVertMovementRemaining ) 
                  ; LOOP (* Thru all MarkRecs to left in skipped lines 
                            of this LinesRef. *) 
                      TYPECASE WCursorMark . LmLeftLink 
                      OF PaintHs . LineMarkMeatTyp ( TMark ) 
                      => IF TMark . LmLinesRef # WCursorMark . LmLinesRef 
                            OR TMark . LmLineNo <= WCursorMark . LmLineNo 
                         THEN 
                           EXIT (* Marks *) 
                         ELSE 
                          PaintHs . SwapMarkOrder ( TMark ) 
                         END (* IF *) 
                      ELSE 
                        EXIT (* Marks *) 
                      END (* TYPECASE *) 
                    END (* LOOP *) 
                  ; WCursorMark . LmCharPos := LbeStd . LimitedCharNoMax 
                  ; WCursorMark . LmLinesRef . LrHasMark := TRUE 
                  ; EXIT (* LinesRefs *)  
                  ELSE (* Consume a LinesRef. *) 
                    LOOP (* Thru all MarkRecs to left in this LinesRef. *) 
                      TYPECASE WCursorMark . LmLeftLink 
                      OF PaintHs . LineMarkMeatTyp ( TMark ) 
                      => IF TMark . LmLinesRef # WCursorMark . LmLinesRef 
                         THEN (* TMark is for a different LinesRef. *) 
                           EXIT (* Marks *) 
                         ELSE 
                           PaintHs . SwapMarkOrder ( TMark ) 
                         END (* IF *) 
                      ELSE 
                        EXIT (* Marks *) 
                      END (* TYPECASE *) 
                    END (* LOOP *) 
                  ; INC 
                      ( McVertMovementRemaining , WCursorMark . LmLineNo + 1 ) 
                  ; DEC ( ActualMovement . v , WCursorMark . LmLineNo + 1 ) 
                  ; PaintHs . RecomputeLrHasMark ( WCursorMark ) 
                  ; WCursorMark . LmLinesRef 
                      := WCursorMark . LmLinesRef . LrLeftLink 
                    (* ^This narrow can't fail because of logic above. *) 
                  ; WCursorMark . LmLinesRef . LrHasMark := TRUE 
                  ; WCursorMark . LmTokMark 
                      := WCursorMark . LmLinesRef . LrBolTokMark 
                  ; WCursorMark . LmLineNo 
                      := ActualLineCtOfLinesRef 
                           ( McImageRef , WCursorMark . LmLinesRef ) 
                         - 1 
                  ; WCursorMark . LmCharPos 
                      := LbeStd . LimitedCharNoMax (* Should be dead *) 
                      (* Set to Very EndOfLine *) 
                  ; SecurePred ( McImageRef , WCursorMark . LmLinesRef ) 
                  ; IF WCursorMark . LmLinesRef . LrLeftLink 
                       = McImageRef . ItPers . IpLineHeaderRef 
                    THEN (* We are already at BegOfImage LinesRef. *) 
                      McVertMovementRemaining 
                        := MAX ( McVertMovementRemaining 
                               , - WCursorMark . LmLineNo 
                               ) 
                    END (* IF *) 
                  ; LAreWithinThisLinesRef 
                      := - McVertMovementRemaining <= WCursorMark . LmLineNo 
                  END (* IF *) 
                END (* LOOP *) 
              ; Search . ClearMatch ( WindowRef , (* VAR *) MustRepaint ) 
              ELSIF McVertMovementRemaining > 0 
              THEN 
                LLinesToNext := McLinesToNext ( WCursorMark ) 
              ; LAreWithinThisLinesRef 
                  := McVertMovementRemaining < LLinesToNext 
              ; IF NOT LAreWithinThisLinesRef 
                THEN
                  PaintHs . RecomputeLrHasMark ( WCursorMark ) 
                END (* IF *) 
              ; LOOP (* Thru LinesRefs *) 
                  IF LAreWithinThisLinesRef 
                  THEN (* We can get to final position w/in this LinesRef *) 
                    INC ( WCursorMark . LmLineNo , McVertMovementRemaining ) 
                  ; INC ( ActualMovement . v , McVertMovementRemaining ) 
                  ; LOOP (* Thru all MarkRecs to right in skipped lines 
                           of this LinesRef. *) 
                      TYPECASE WCursorMark . LmRightLink 
                      OF PaintHs . LineMarkMeatTyp ( TMark ) 
                      => IF TMark . LmLinesRef # WCursorMark . LmLinesRef 
                            OR TMark . LmLineNo >= WCursorMark . LmLineNo 
                         THEN 
                           EXIT (* Marks *)  
                         ELSE 
                           PaintHs . SwapMarkOrder ( WCursorMark ) 
                         END (* IF *) 
                      ELSE 
                        EXIT (* Marks *) 
                      END (* TYPECASE  IF *) 
                    END (* LOOP *) 
                  ; WCursorMark . LmCharPos := 0 
                  ; WCursorMark . LmLinesRef . LrHasMark := TRUE 
                  ; EXIT (* LinesRefs *) 
                  ELSE (* Consume a LinesRef. *) 
                    LOOP (* Thru all MarkRecs to right in this LinesRef. *) 
                      TYPECASE WCursorMark . LmRightLink 
                      OF PaintHs . LineMarkMeatTyp ( TMark ) 
                      => IF TMark . LmLinesRef # WCursorMark . LmLinesRef 
                         THEN (* This MarkRec is for a different LinesRef. *) 
                           EXIT (* Marks *)  
                         ELSE 
                           PaintHs . SwapMarkOrder ( WCursorMark ) 
                         END (* IF *) 
                      ELSE 
                        EXIT (* Marks *) 
                      END (* TYPECASE *) 
                    END (* LOOP *) 
                  ; DEC ( McVertMovementRemaining , LLinesToNext ) 
                  ; INC ( ActualMovement . v , LLinesToNext ) 
                  ; SecureSucc ( McImageRef , WCursorMark . LmLinesRef ) 
                  ; PaintHs . RecomputeLrHasMark ( WCursorMark ) 
                  ; WCursorMark . LmLinesRef 
                      := WCursorMark . LmLinesRef . LrRightLink 
                    (* ^This narrow can't fail because of logic above. *) 
                  ; WCursorMark . LmLinesRef . LrHasMark := TRUE 
                  ; WCursorMark . LmTokMark 
                      := WCursorMark . LmLinesRef . LrBolTokMark 
                  ; WCursorMark . LmLineNo := 0 
                  ; WCursorMark . LmCharPos := 0 (* Should be dead *) 
                  ; LLinesToNext := McLinesToNext ( WCursorMark ) 
                  ; LAreWithinThisLinesRef 
                      := McVertMovementRemaining < LLinesToNext 
                  END (* IF *) 
                END (* LOOP *) 
              ; Search . ClearMatch ( WindowRef , (* VAR *) MustRepaint ) 
              END (* IF *) 
            ; HorizMoveCursorWindowRef 
                ( WindowRef 
                , LFinalHorizPos - WCursorMark . LmCharPos 
                , MustRepaint 
                ) 
            ; PaintHs . BruteForceVerifyLineMarks ( WindowRef . WrImageRef ) 
            END (* WITH WCursorMark *) 
          END (* IF *) 
        END (* IF *) 
      END (* Block *) 
    END MoveCursorNoScroll 

; PROCEDURE VertScrollNoMoveCursor 
    ( (* IN OUT *) WindowRef : PaintHs . WindowRefTyp 
    ; WantedMovement : LbeStd . LineNoSignedTyp 
      (* Positive means window frame moves down, relative to image. *) 
    ; VAR (* OUT *) ActualMovement : LbeStd . LineNoSignedTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
      (* Set true if anything requiring a repaint happens. *) 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* DOES NOT update WindowRef . WrCursorLineNoInWindow *) 

  = VAR LImageRef : PaintHs . ImageTransientTyp 
  ; VAR LFromLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LThruLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LFromLineNo : LbeStd . LineNoTyp 
  ; VAR LThruLineNo : LbeStd . LineNoTyp 
  ; VAR LActualMovement : LbeStd . LineNoSignedTyp 

  ; BEGIN (* VertScrollNoMoveCursor *) 
      LImageRef := WindowRef . WrImageRef 
    ; IF WantedMovement < 0 
      THEN (* Window moves up, relative to image, toward BOI . *) 
        GetLinesRefAndLineNoInWindow 
          ( WindowRef 
          , EditWindow . SouthEastToCorner ( WindowRef ) . v - 1 
          , LThruLinesRef 
          , LThruLineNo 
          ) 
      ; MoveLinesRefAndNoUp 
          ( WindowRef 
          , (* IN OUT *) WindowRef . WrFirstLineLinesRef 
          , (* IN OUT *) WindowRef . WrFirstLineLineNo 
          , Movement := - WantedMovement 
          , (* VAR *) ActualMovement := LActualMovement 
          ) 
      ; ActualMovement := - LActualMovement 
      ; IF ActualMovement > 0 
        THEN 
          LFromLinesRef := LThruLinesRef 
        ; LFromLineNo := LThruLineNo 
        ; MoveLinesRefAndNoUp 
            ( WindowRef  
            , (* IN OUT *) LFromLinesRef 
            , (* IN OUT *) LFromLineNo 
            , Movement := - ActualMovement - 1 
            , (* VAR *) ActualMovement := LActualMovement 
            ) 
        ; Assert 
            ( LActualMovement = ( - ActualMovement - 1 ) 
            , AFT . A_VertScrollNoMoveCursorUpScroll 
            ) 
        ; MakeLinesRefsAndNosNotVisibleInWindow 
            ( LImageRef 
            , LFromLinesRef 
            , LFromLineNo 
            , LThruLinesRef 
            , LThruLineNo 
            , WindowRef . WrWindowNo 
            ) 
        END (* IF *) 
      ; AdjustEstimatesForMaybeBegOfImage ( WindowRef , MustRepaint )  
      ELSIF WantedMovement > 0 
      THEN (* Window moves down, relative to image, toward EOI . *) 
        LFromLinesRef := WindowRef . WrFirstLineLinesRef 
      ; LFromLineNo := WindowRef . WrFirstLineLineNo 
      ; MoveLinesRefAndNoDown 
          ( WindowRef 
          , (* IN OUT *) WindowRef . WrFirstLineLinesRef 
          , (* IN OUT *) WindowRef . WrFirstLineLineNo 
          , Movement := WantedMovement 
          , (* VAR *) ActualMovement := ActualMovement 
          ) 
      ; IF LinesRefIsEndOfImage 
             ( LImageRef , WindowRef . WrFirstLineLinesRef ) 
           AND ( LImageRef . ItPers . IpTempEditState 
                 # PaintHs . TempEditStateTyp . TeStateText  
                 OR LImageRef . ItPers . IpTempEditRef = NIL 
                 OR LImageRef . ItPers . IpTempEditRef . TeLinesRef 
                    # WindowRef . WrFirstLineLinesRef  
                 OR LImageRef . ItPers . IpTempEditRef . TeLineNo 
                    # WindowRef . WrFirstLineLineNo
               )   
        THEN (* We hit the one invisible line at EOI.  Move back up one 
                line to last visible line. *) 
          MoveLinesRefAndNoUp 
            ( WindowRef 
            , (* IN OUT *) WindowRef . WrFirstLineLinesRef 
            , (* IN OUT *) WindowRef . WrFirstLineLineNo 
            , Movement := 1 
            , (* VAR *) ActualMovement := LActualMovement 
            ) 
        ; DEC ( ActualMovement , LActualMovement ) 
        END (* IF *) 
      ; IF ActualMovement > 0 
        THEN (* Make some LinesRefs invisible above the window. *)  
          LThruLinesRef := WindowRef . WrFirstLineLinesRef 
        ; LThruLineNo := WindowRef . WrFirstLineLineNo 
        ; MoveLinesRefAndNoUp 
            ( WindowRef 
            , (* IN OUT *) LThruLinesRef 
            , (* IN OUT *) LThruLineNo 
            , Movement := 1 
            , (* VAR *) ActualMovement := LActualMovement 
            ) 
        ; Assert 
            ( LActualMovement = 1 , AFT . A_VertScrollNoMoveCursorDownScroll ) 
        ; MakeLinesRefsAndNosNotVisibleInWindow 
            ( LImageRef 
            , LFromLinesRef 
            , LFromLineNo 
            , LThruLinesRef 
            , LThruLineNo 
            , WindowRef . WrWindowNo 
            ) 
        END (* IF *) 
      ELSE (* WantedMovement = 0 *) 
        ActualMovement := 0 
      END (* IF *) 
    ; WindowRef . WrVertScroll 
        := MAX ( 0 , WindowRef . WrVertScroll + ActualMovement ) 
    ; IF ActualMovement # 0 THEN MustRepaint := TRUE END (* IF *) 
    END VertScrollNoMoveCursor 

(* VISIBLE: *) 
; PROCEDURE MoveCursorWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; WantedMovement : EditWindow . CharPointTyp 
    ; VAR (* OUT *) ActualMovement : EditWindow . CharPointTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LCursorLineNoInWindow : LbeStd . LineNoSignedTyp 
  ; VAR LActualMovement : LbeStd . LineNoSignedTyp 

  ; BEGIN (* MoveCursorWindowRef *) 
      IF WindowRef # NIL AND WindowRef . WrImageRef # NIL 
      THEN 
        MoveCursorNoScroll 
          ( WindowRef , WantedMovement , ActualMovement , MustRepaint ) 
      ; HorizScrollWindowRefToContainCursor ( WindowRef , MustRepaint ) 
      ; LCursorLineNoInWindow 
          := WindowRef . WrCursorLineNoInWindow + ActualMovement . v 
      ; IF ActualMovement . v < 0 
        THEN (* Moved northward. *) 
          IF LCursorLineNoInWindow < 0 
          THEN 
            VertScrollNoMoveCursor 
              ( WindowRef 
              , LCursorLineNoInWindow 
              , LActualMovement 
              , MustRepaint 
              ) 
          ; Assert 
              ( LActualMovement = LCursorLineNoInWindow 
              , AFT . A_MoveCursorWindowRefUpScrollTop 
              ) 
          ; WindowRef . WrCursorLineNoInWindow := 0 
          ELSE 
            WindowRef . WrCursorLineNoInWindow := LCursorLineNoInWindow 
          END (* IF *) 
        ELSE (* Moved vertically zero or southward *) 
          IF LCursorLineNoInWindow 
             >= EditWindow . SouthEastToCorner ( WindowRef ) . v 
          THEN 
            VertScrollNoMoveCursor 
              ( WindowRef 
              , LCursorLineNoInWindow 
                - EditWindow . SouthEastToCorner ( WindowRef ) . v 
                + 1 
              , LActualMovement 
              , MustRepaint 
              ) 
          ; Assert 
              ( LActualMovement 
                = ( LCursorLineNoInWindow 
                    - EditWindow . SouthEastToCorner ( WindowRef ) . v 
                    + 1 
                  ) 
              , AFT . A_MoveCursorWindowRefDownScrollTop 
              ) 
          ; WindowRef . WrCursorLineNoInWindow 
              := EditWindow . SouthEastToCorner ( WindowRef ) . v - 1 
          ELSE 
            WindowRef . WrCursorLineNoInWindow := LCursorLineNoInWindow 
          END (* IF *) 
        END (* IF *) 
      ; IF NOT MustRepaint 
        THEN (* But we moved the cursor locally, so set it in the window. *) 
          EditWindow . SetCursorPosition 
            ( WindowRef 
            , WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
              . LmCharPos 
              - WindowRef . WrHorizScroll 
            , WindowRef . WrCursorLineNoInWindow 
            ) 
        ; EditWindow . PaintCursorCoordinates ( WindowRef ) 
        END (* IF *) 
      END (* IF *) 
    END MoveCursorWindowRef 

(* VISIBLE: *) 
; PROCEDURE MoveCursorAndRepaintWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; WantedMovement : EditWindow . CharPointTyp 
    )
  RAISES { Backout , Thread . Alerted }  

  = VAR LActualMovement : EditWindow . CharPointTyp 
  ; VAR LMustRepaint : BOOLEAN := FALSE 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 

  ; BEGIN (* MoveCursorAndRepaintWindowRef *) 
      IF WindowRef # NIL AND WindowRef . WrImageRef # NIL 
      THEN 
        MoveCursorWindowRef 
          ( WindowRef , WantedMovement , LActualMovement , LMustRepaint ) 
      ; IF LMustRepaint 
        THEN 
          PaintWindowFromLines 
            ( WindowRef 
            , (* VAR *) LTrailingBlankLines (* Dead. *) 
            , (* VAR *) LLinesRef (* Dead. *) 
            , (* VAR *) LLineNo  (* Dead. *) 
            ) 
(* CHECK: shouldn't we just mark the window for repainting? *) 
        END (* IF *) 
      END (* IF *) 
    END MoveCursorAndRepaintWindowRef 

(* VISIBLE: *) 
; PROCEDURE AbsoluteCursorPositionInWindow 
    ( WindowRef : PaintHs . WindowRefTyp ) 
  : EditWindow . CharPointTyp 
  RAISES { Backout , Thread . Alerted } <* NOWARN *> 
  (* AbsPosition must lie within the window. *) 

  = VAR LResult : EditWindow . CharPointTyp 
  ; LHorizPositionInWindow : EditWindow . CharCoordTyp 

  ; BEGIN 
      LResult := EditWindow . CharPointTyp { 0 , 0 }  
    ; IF WindowRef # NIL AND WindowRef . WrImageRef # NIL 
      THEN 
        WITH 
          WCursorMark 
          = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
        DO IF WCursorMark # NIL 
           THEN 
             LHorizPositionInWindow 
               := WCursorMark . LmCharPos - WindowRef . WrHorizScroll 
           ; LResult 
               := EditWindow . CharPointTyp 
                    { LHorizPositionInWindow 
                    , WindowRef . WrCursorLineNoInWindow 
                    } 
           END (* IF*) 
        END (* WITH *) 
      END (* IF *) 
    ; RETURN LResult 
    END AbsoluteCursorPositionInWindow  

(* VISIBLE: *) 
; PROCEDURE MoveCursorAbsoluteInsideWindow 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; AbsPosition : EditWindow . CharPointTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* AbsPosition must lie within the window. *) 

  = VAR LHorizPositionInWindow : EditWindow . CharCoordTyp 
  ; VAR LRelMovement : EditWindow . CharPointTyp 
  ; VAR LActualMovement : EditWindow . CharPointTyp 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 
  ; VAR LMustRepaint : BOOLEAN := FALSE 

  ; BEGIN (* MoveCursorAbsoluteInsideWindow *) 
      IF WindowRef # NIL AND WindowRef . WrImageRef # NIL 
      THEN 
        WITH 
          WCursorMark 
          = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
        DO IF WCursorMark # NIL 
           THEN 
             LHorizPositionInWindow 
               := WCursorMark . LmCharPos - WindowRef . WrHorizScroll 
           ; LRelMovement 
               := Point . Sub 
                    ( AbsPosition 
                    , EditWindow . CharPointTyp 
                        { LHorizPositionInWindow 
                        , WindowRef . WrCursorLineNoInWindow 
                        } 
                    ) 
           ; MoveCursorWindowRef 
               ( WindowRef , LRelMovement , LActualMovement , LMustRepaint ) 
           ; LHorizPositionInWindow 
               := WCursorMark . LmCharPos - WindowRef . WrHorizScroll 
           ; Assert 
               ( LActualMovement = LRelMovement 
                 AND AbsPosition 
                     = Point . T 
                         { LHorizPositionInWindow 
                         , WindowRef . WrCursorLineNoInWindow 
                         } 
               , AFT . A_MoveCursorAbsoluteInsideWindow_NotInWindow 
               ) 
           ; IF LMustRepaint 
             THEN 
               PaintWindowFromLines 
                 ( WindowRef 
                 , (* VAR *) LTrailingBlankLines (* Dead. *) 
                 , (* VAR *) LLinesRef (* Dead. *) 
                 , (* VAR *) LLineNo  (* Dead. *) 
                 ) 
             END (* IF *) 
           END (* IF *) 
        END (* WITH *) 
      END (* IF *) 
    END MoveCursorAbsoluteInsideWindow 

(* VISIBLE: *) 
; PROCEDURE VertScroll 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; WantedMovement : LbeStd . LineNoSignedTyp 
      (* Positive means window frame moves down, relative to image. *) 
    ; DoDragCursor : BOOLEAN 
      (* ^Means keep cursor in same window-relative position. 
          Even if NOT DoDragCursor, it will be dragged to 
          prevent its getting outside the window. *) 
    ; VAR (* OUT *) ActualMovement : LbeStd . LineNoSignedTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LMovementPoint : EditWindow . CharPointTyp 
  ; VAR LActualMovementPoint : EditWindow . CharPointTyp 

  ; BEGIN (* VertScroll *) 
      IF WindowRef # NIL AND WindowRef . WrImageRef # NIL 
      THEN 
        VertScrollNoMoveCursor 
          ( WindowRef , WantedMovement , ActualMovement , MustRepaint ) 
      ; LMovementPoint . h := 0 
      ; IF DoDragCursor 
        THEN 
          LMovementPoint . v := ActualMovement 
        ELSE 
          IF ActualMovement < 0 
          THEN 
            LMovementPoint . v 
              := MIN 
                   ( 0 
                   , ActualMovement 
                     + ( EditWindow . SouthEastToCorner ( WindowRef ) . v 
                         - 1 
                         - WindowRef . WrCursorLineNoInWindow 
                       ) 
                   ) 
          ELSE 
            LMovementPoint . v 
              := MAX 
                   ( 0 , ActualMovement - WindowRef . WrCursorLineNoInWindow ) 
          END (* IF *) 
        END (* IF *) 
      ; MoveCursorNoScroll 
          ( WindowRef , LMovementPoint , LActualMovementPoint , MustRepaint ) 
      ; Assert 
          ( LActualMovementPoint = LMovementPoint 
          , AFT . A_VertScroll_NotRequestedScroll  
          ) 
      ; INC ( WindowRef . WrCursorLineNoInWindow 
            , LActualMovementPoint . v - ActualMovement 
            ) 
      ; IF ActualMovement < WantedMovement 
        THEN (* We must have hit EOI.  Force repaint, to adjust estimates. *) 
          MustRepaint := TRUE 
        END (* IF *) 
      END (* IF *) 
    END VertScroll

; PROCEDURE VertScrollAndRepaint 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; WantedMovement : LbeStd . LineNoSignedTyp 
      (* Positive means window frame moves down, relative to image. *) 
    ; DoDragCursor : BOOLEAN 
      (* ^Means keep cursor in same window-relative position. 
          Even if NOT DoDragCursor, it will be dragged to 
          prevent its getting outside the window. *) 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LMustRepaint : BOOLEAN 
  ; VAR LActualMovement : LbeStd . LineNoSignedTyp 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 

  ; BEGIN (* VertScrollAndRepaint *) 
      LMustRepaint := FALSE 
    ; VertScroll 
        ( WindowRef 
        , WantedMovement 
        , DoDragCursor 
        , (* VAR *) ActualMovement := LActualMovement (* Dead *)  
        , (* VAR *) MustRepaint := LMustRepaint 
        ) 
    ; IF LMustRepaint 
      THEN 
        PaintWindowFromLines 
          ( WindowRef 
          , (* VAR *) LTrailingBlankLines (* Dead. *) 
          , (* VAR *) LLinesRef (* Dead. *) 
          , (* VAR *) LLineNo  (* Dead. *) 
          ) 
(* CHECK: shouldn't we just mark the window for repainting? *) 
      END (* IF *) 
    END VertScrollAndRepaint 

(* VISIBLE: *) 
; PROCEDURE ReshapeWindowRef 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; OldSize : EditWindow . CharPointTyp 
    ; NewSize : EditWindow . CharPointTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LFromLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LFromLineNo : LbeStd . LineNoTyp 
  ; VAR LThruLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LThruLineNo : LbeStd . LineNoTyp 
  ; VAR LLinesToMakeNotVisible : LbeStd . LineNoSignedTyp 
  ; VAR LActualMovementPoint : EditWindow . CharPointTyp 
  ; VAR LActualMovement : LbeStd . LineNoSignedTyp 
  ; VAR LMustRepaint : BOOLEAN := TRUE 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 

  ; BEGIN (* ReshapeWindowRef *) 
      IF WindowRef # NIL AND WindowRef . WrImageRef = NIL 
      THEN (* Can be NIL if no image has been opened *) 
        EditWindow . PaintWindowBlank ( WindowRef ) 
      ELSE 
        NewSize := Point . Max ( NewSize , Point1_1 ) 
      ; LLinesToMakeNotVisible := OldSize . v - NewSize . v 
      ; IF LLinesToMakeNotVisible > 0 
        THEN 
          IF WindowRef . WrCursorLineNoInWindow >= NewSize . v 
          THEN 
            MoveCursorNoScroll 
              ( WindowRef := WindowRef 
              , WantedMovement 
                  := EditWindow . CharPointTyp 
                       { h := 0 
                       , v := NewSize . v 
                              - 1 
                              - WindowRef . WrCursorLineNoInWindow 
                       } 
              , ActualMovement := LActualMovementPoint 
              , MustRepaint := LMustRepaint 
              ) 
          ; INC 
              ( WindowRef . WrCursorLineNoInWindow , LActualMovementPoint . v )
          END (* IF *) 
        ; LFromLinesRef := WindowRef . WrFirstLineLinesRef 
        ; LFromLineNo := WindowRef . WrFirstLineLineNo 
        ; MoveLinesRefAndNoDown 
            ( WindowRef 
            , (* IN OUT *) LFromLinesRef 
            , (* IN OUT *) LFromLineNo 
            , Movement := NewSize . v 
            , (* VAR *) ActualMovement := LActualMovement 
            ) 
        ; IF LActualMovement >= NewSize . v
          THEN 
            LThruLinesRef := LFromLinesRef 
          ; LThruLineNo := LFromLineNo 
          ; MoveLinesRefAndNoDown 
              ( WindowRef 
              , (* IN OUT *) LThruLinesRef 
              , (* IN OUT *) LThruLineNo 
              , Movement := LLinesToMakeNotVisible - 1 
              , (* VAR *) ActualMovement := LActualMovement 
              ) 
          ; IF LActualMovement > 0 
            THEN 
              MakeLinesRefsAndNosNotVisibleInWindow 
                ( WindowRef . WrImageRef 
                , LFromLinesRef 
                , LFromLineNo 
                , LThruLinesRef 
                , LThruLineNo 
                , WindowRef . WrWindowNo 
                ) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      ; HorizProjectCursorIntoWindow ( WindowRef , LMustRepaint ) 
      ; PaintWindowFromLines 
          ( WindowRef 
          , (* VAR *) LTrailingBlankLines (* Dead. *) 
          , (* VAR *) LLinesRef (* Dead. *) 
          , (* VAR *) LLineNo  (* Dead. *) 
          ) 
(* CHECK: shouldn't we just mark the window for repainting? *) 
      END (* IF *) 
    END ReshapeWindowRef 

(* VISIBLE: *) 
; PROCEDURE Beep ( Code : Errors . ErrorTyp ) 

  = BEGIN (* Beep *) 
      GMostRecentBeepCode := Code 
    ; EditWindow . Beep ( ) 
    END Beep 

; PROCEDURE DisplayImageState 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; VbtName : TEXT 
    ; State : INTEGER 
    ) 

  = VAR LWindowRef : PaintHs . WindowRefTyp 

  ; BEGIN (* DisplayImageState *) 
      LWindowRef := ImageRef . ItWindowList 
    ; WHILE LWindowRef # NIL 
      DO EditWindow . PaintWindowState ( LWindowRef , VbtName , State ) 
      ; LWindowRef := LWindowRef . WrImageLink 
      END (* WHILE *) 
    END DisplayImageState 

(* VISIBLE: *) 
; PROCEDURE NoteImageSavedState 
    ( ImageRef : PaintHs . ImageTransientTyp ; State : BOOLEAN ) 

  = BEGIN (* NoteImageSavedState *) 
      IF ImageRef # NIL 
      THEN 
        ImageRef . ItIsSaved := State 
      ; DisplayImageState ( ImageRef , "Fv_Modified" , ORD ( NOT State ) ) 
      END (* IF *) 
    END NoteImageSavedState 

(* VISIBLE: *) 
; PROCEDURE PaintImageParsedState 
    ( ImageRef : PaintHs . ImageTransientTyp ) 
  
  = VAR LVBTState : INTEGER 

  ; BEGIN (* PaintImageParsedState *)
(* TODO: Create some enumerations for these states *)  
      IF ImageRef # NIL 
      THEN 
        IF ImageRef . ItPers . IpIsParsed 
        THEN
          IF EstUtil . HasSyntErrors ( ImageRef . ItPers . IpEstRoot ) 
          THEN  
            LVBTState := 2  
          ELSE
            LVBTState := 0 
          END 
        ELSE (* Not parsed. *) 
          LVBTState := 1 
        END  
      ; DisplayImageState ( ImageRef , "Fv_Unparsed" , LVBTState ) 
      END (* IF *) 
    END PaintImageParsedState 

(* VISIBLE: *) 
; PROCEDURE NoteImageParsedState 
    ( ImageRef : PaintHs . ImageTransientTyp ; State : BOOLEAN ) 

  = BEGIN (* NoteImageParsedState *)
      IF ImageRef # NIL 
      THEN 
        ImageRef . ItPers . IpIsParsed := State 
      ; PaintImageParsedState ( ImageRef ) 
      END (* IF *) 
    END NoteImageParsedState 

(* VISIBLE: *) 
; PROCEDURE NoteImageAnalyzedState 
    ( ImageRef : PaintHs . ImageTransientTyp ; State : BOOLEAN ) 

  = BEGIN (* NoteImageAnalyzedState *) 
      IF ImageRef # NIL 
      THEN 
        ImageRef . ItPers . IpIsAnalyzed := State 
      ; DisplayImageState ( ImageRef , "Fv_Unanalyzed" , ORD ( NOT State ) ) 
      END (* IF *) 
    END NoteImageAnalyzedState 

(* VISIBLE: *) 
; PROCEDURE Reparse ( (* IN OUT *) ImageRef : PaintHs . ImageTransientTyp ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LSavedMarkListRef : ParseHs . TempMarkArrayRefTyp 
  ; VAR LParseInfo : ParseHs . ParseInfoTyp 
  ; VAR LOldEstRef : LbeStd . EstRootTyp 
  ; VAR LNewEstRef : LbeStd . EstRootTyp 
  ; VAR LScannerIf : ScannerIf . ScanIfTyp 
  ; VAR LInitialParseTravStateRef : ParseHs . ParseTravStateRefTyp 

  ; BEGIN (* Reparse *) 
      IF ImageRef # NIL 
      THEN 
        LImagePers := ImageRef . ItPers 
      ; TextEdit . FlushEdit ( ImageRef ) 
      ; TempMark . SavePermanentMarks 
          ( ImageRef , (* VAR *) LSavedMarkListRef )   
      ; TRY 
          LParseInfo . PiLang := LImagePers . IpLang 
        ; LScannerIf := LangUtil . ScannerIfForLang ( LImagePers . IpLang ) 
     (* ; ImageRef . ItScannerIf := LScannerIf These things choke pickles. *) 
        ; LParseInfo . PiScanIf := LScannerIf 
        ; LParseInfo . PiGram := LangUtil . Gram ( LImagePers . IpLang ) 
        ; LParseInfo . PiInsertNilFixedChildren 
            := Options . InsertNilFixedChildren
        ; LOldEstRef := LImagePers . IpEstRoot 
        ; PaintHs . BruteForceVerifyLineMarks ( ImageRef ) 
        ; IF Options . TreeBrowsing 
          THEN
            TreeBrowse . Browse 
              ( LOldEstRef , LImagePers . IpLang 
              , "Before BuildTempMarkList" 
              ) 
          END (* IF *) 
        ; TempMark . BuildTempMarkList ( ImageRef , LParseInfo )
          (* ^Sets LParseInfo.PiOrigTempMarkListRef. *)
        ; IF Options . TreeBrowsing 
          THEN
            TreeBrowse . Browse 
              ( LOldEstRef , LImagePers . IpLang 
              , "After BuildTempMarkList, before parse" 
              ) 
          END (* IF *) 
        ; LInitialParseTravStateRef
            := ParseTrv . InitParseEst ( LParseInfo , LOldEstRef )
        ; Parser . Parse 
            ( LParseInfo 
            , LInitialParseTravStateRef 
            , LNewEstRef  
            ) 
        ; IF Options . TreeBrowsing 
          THEN
            TreeBrowse . Browse 
              ( LNewEstRef , LImagePers . IpLang 
              , "After Parse, before RebuildMarkList" 
              ) 
          END (* IF *) 
        ; TempMark . RebuildMarkList 
            ( ParseInfo := LParseInfo 
            , ImageRef := ImageRef 
            , OldEstRef := LOldEstRef 
            , NewEstRef := LNewEstRef 
            ) 
        ; IF Options . TreeBrowsing 
          THEN 
            TreeBrowse . Browse 
              ( LNewEstRef , LImagePers . IpLang 
              , "After RebuildMarkList" 
              ) 
          END (* IF *)  

        ; EstUtil . UnmarkContainsTempMark ( LOldEstRef ) 
        ; EstUtil . UnmarkContainsTempMark ( LNewEstRef ) 
        ; LImagePers . IpEstRoot := LNewEstRef 
        ; PaintHs . BruteForceVerifyLineMarks ( ImageRef ) 
(* TODO: Note a version. *) 
        ; TempMark . DisconnectMarksFromLinesRefs ( ImageRef ) 
        ; ReconstructLinesAndPaint ( ImageRef ) 
        (* By here, we should be past assertion failures.  Make changes. *)
        ; LImagePers. IpLineCtDisplay 
            := MAX ( 0 
                   , LImagePers . IpLineCtDisplay + LParseInfo . PiLineCtIncr 
                   )   
        ; LImagePers . IpLineCtIsExact := FALSE 
        ; NoteImageSavedState ( ImageRef , FALSE ) 
        ; NoteImageParsedState ( ImageRef , TRUE ) 
        ; NoteImageAnalyzedState ( ImageRef , FALSE ) 
        EXCEPT Backout ( EMessage )  
        => LImagePers . IpEstRoot := LOldEstRef 
        ; TempMark . RestorePermanentMarks ( ImageRef , LSavedMarkListRef ) 
        (* Now we are in a mess.  I hope this works. *) 
        ; TempMark . DisconnectMarksFromLinesRefs ( ImageRef ) 
        ; EstUtil . UnmarkContainsTempMark ( LOldEstRef ) 
        ; EstUtil . UnmarkContainsTempMark ( LNewEstRef ) 
        ; TRY 
            ReconstructLinesAndPaint ( ImageRef ) 
          EXCEPT Backout => (* Ignore this one *)  
          END (* IF *) 
        ; AssertDevel . WriteCheckpoint 
            ( ImageRef := ImageRef 
            , Message := EMessage 
            , DoCreateVersion := FALSE 
            ) 
          (* ^This will rewrite the checkpoint that was already written from 
              inside AssertDevel, but with the changes undone. 
          *) 
        ; NoteImageSavedState ( ImageRef , FALSE ) 
        ; NoteImageParsedState ( ImageRef , FALSE ) 
        ; NoteImageAnalyzedState ( ImageRef , FALSE ) 
        ; RAISE Backout ( EMessage ) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END Reparse 

; BEGIN (* Display *) 
  END Display 
. 
