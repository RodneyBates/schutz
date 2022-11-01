
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE TempMark 
(* Package TempMark. 
   TempMark contains procedures for converting normal marks 
   to temporary marks and back, and for unmarking temporary 
   marks after parsing. 

   Temporary marks use a necessarily complicated scheme to keep
   them attached to any token or Mod during parse traversal and reparsing.
   Some of the tokens are implicit in the Est and some of the marks
   will be attached to text edits that are scanned, parsed, and put
   into the new Est in the normal way. 
*) 

; IMPORT Fmt 
; IMPORT Text 
; IMPORT Thread 

; IMPORT Assertions 

; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout   
; IMPORT Display  
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LangUtil 
; FROM LangUtil IMPORT FsKindTyp , FmtKindTyp  
; IMPORT LbeStd 
; IMPORT Marks 
; IMPORT MessageCodes 
; IMPORT ModHs 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT ParseHs 
; IMPORT SharedStrings 
; IMPORT TravUtil 

; TYPE AFT = MessageCodes . T 

; CONST FmtKindUnknown = FmtKindTyp . FmtKindUnknown  
; CONST FmtKindVert = FmtKindTyp . FmtKindVert 
; CONST FmtKindHoriz = FmtKindTyp . FmtKindHoriz  
; CONST FmtKindFill = FmtKindTyp . FmtKindFill 

; TYPE MarkKindTyp = Marks . MarkKindTyp 

(* CHECK: 
   review RbmTeTfsInsTok, RbmStateDescend, est list, off the right end. 
     Shouldn't the mark mechanism lead us to the RM child 
     for either left or right sib insertion tokens? 
*) 

(* EXPORTED: *) 
; PROCEDURE BuildTempMarkList 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; VAR ParseInfo : ParseHs . ParseInfoTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

  (* Convert the marks in an Est into a temp mark list 
     and mark bits in the Est nodes. 
  *) 

  (* NOTE:  BuildTempMarkList builds temp marks that have both a node number 
            in field TokMark.EstNodeNo and an EstRef in field 
            EstRef. ParseTrv. Parser can use either. *) 

  = TYPE BtmStateTyp 
      = { BtmStateStartAtBeg 
          (* ^Start a line at the first new line of the token. *) 
        , BtmStateStartAtEnd 
          (* ^Start a line at the last new line of a token 
             that has a possible new line at the end. 
             (comment or text insertion mod) *) 
        , BtmStatePassingNl 
          (* ^Passing through a group of adjacent new lines at the beginning 
              of the line being processed. *) 
        , BtmStateInLine 
          (* ^Inside a line. *) 
        , BtmStateDoneWithLine 
        } 
    (* In BtmStateStartAtBeg and BtmStateStartAtEnd, TraverseEst and 
       TraverseFs jump into the middle of their respective trees, 
       using the start of line mark to decide where.  Once the starting 
       point is reached, it goes into another state to traverse left- 
       to-right. *) 

  ; TYPE BtmStateSetTyp = SET OF BtmStateTyp 

  ; CONST BtmStateSetStart 
      = BtmStateSetTyp 
          { BtmStateTyp . BtmStateStartAtBeg 
          , BtmStateTyp . BtmStateStartAtEnd 
          } 

  ; VAR BtmState : BtmStateTyp 
  ; VAR BtmTempMarkCt : LbeStd . MarkNoTyp 
  ; VAR BtmTempMarkNextElemNo : LbeStd . MarkNoTyp 
  ; VAR BtmLineMarkMeat : PaintHs . LineMarkMeatTyp 
  ; VAR BtmLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR BtmCharPos : LbeStd . LimitedCharNoTyp 
  ; VAR BtmNonblankOnLineToPos : LbeStd . LimitedCharNoTyp 
  ; VAR BtmTokBegCharPos : LbeStd . LimitedCharNoTyp 
  ; VAR BtmPrevTok : LbeStd . TokTyp 
  ; VAR BtmModTextIsToLeftOnLine : BOOLEAN 
  ; VAR BtmTmEstRef : LbeStd . EstRootTyp 
  ; VAR BtmTmEstRefIsMarked : BOOLEAN 
  ; VAR BtmTmEstNodeNo : LbeStd . EstNodeNoTyp 
        (* ^Absolute in the tree. *) 
  ; VAR BtmTmKind : MarkKindTyp 
  ; VAR BtmTmFmtNo : EstHs . FmtNoTyp 
  ; VAR BtmLastFmtNoOnLine : EstHs . FmtNoTyp 
  ; VAR BtmEstListChildrenToPass : LbeStd . EstChildNoTyp 

  ; PROCEDURE BtmEnterLine ( LinesRef : PaintHs . LinesRefMeatTyp )  
    RAISES { Backout , Thread . Alerted } 

    = VAR LLineTextPos : LbeStd . CharNoTyp 

    ; BEGIN 
        Display . SecureSucc ( ImageRef , LinesRef ) 
      ; BtmLinesRef := LinesRef  
      ; IF LinesRef . LrLineText = NIL 
        THEN LLineTextPos := 0 
        ELSE LLineTextPos := Text . Length ( LinesRef . LrLineText )   
        END (* IF *) 
      ; BtmNonblankOnLineToPos := LinesRef . LrFromPos + LLineTextPos  
      ; DEC ( LLineTextPos ) 
      ; WHILE LLineTextPos >= 0 
              AND Text . GetChar ( LinesRef . LrLineText , LLineTextPos ) 
                  = ' ' 
        DO
          DEC ( BtmNonblankOnLineToPos ) 
        ; DEC ( LLineTextPos ) 
        END (* WHILE *)  
      ; BtmCharPos := 0 
      ; BtmTokBegCharPos := 0 
      ; BtmPrevTok := LbeStd . Tok__BegOfLine 
      ; BtmModTextIsToLeftOnLine := FALSE 
      ; BtmTmEstRef := NIL 
      ; BtmTmEstRefIsMarked := FALSE 
      ; BtmTmEstNodeNo := LbeStd . EstNodeNoNull 
      ; BtmTmKind := MarkKindTyp . Null 
      ; BtmTmFmtNo := EstHs . FmtNoNull 
      ; BtmLastFmtNoOnLine := EstHs . FmtNoUnknown  
      ; BtmEstListChildrenToPass := 0 (* Dead *) 
      END BtmEnterLine 

  ; PROCEDURE BtmOrphanedMarksExistAtEOL ( ) : BOOLEAN 
    RAISES { Backout } 
    (* PRE: We are at end of line.  *) 
    (* There is a mark in this line that did not get converted to a TempMark. 
       This happens if the line had nothing parser-visible that a TempMark
       could be applied to.
    *)  

    = VAR LResult : BOOLEAN 

    ; BEGIN 
        IF BtmTempMarkNextElemNo >= BtmTempMarkCt
        THEN LResult := FALSE 
        ELSE 
          TRY 
            LResult 
              := Marks . Compare 
                  ( BtmLineMarkMeat . LmTokMark , BtmLinesRef . LrBolTokMark ) 
                 <= 0 
          EXCEPT Marks . Unordered 
          => RAISE 
               Backout 
                 ( MessageCodes . Image 
                     ( AFT . A_BtmOrphanedMarksExistAtEOL_Unordered_marks ) 
                 ) 
          END (* TRY EXCEPT *) 
        END (* IF *) 
      ; RETURN LResult 
      END BtmOrphanedMarksExistAtEOL 

  ; PROCEDURE BtmTraverseEst 
      ( EstRef : LbeStd . EstRootTyp 
      ; KindSet : EstHs . EstChildKindSetTyp 
      ; EstChildNo : LbeStd . EstChildNoTyp 
        (* ^EstRef has this KindSet and ChildNo in GrandparentRef. *) 
      ; GrandparentEstRef : EstHs . EstRefTyp 
      ; EstAbsNodeNo : LbeStd . EstNodeNoTyp 
      ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
      ; EstFmtKind : FmtKindTyp 
      ; EstIndentPos1 : LbeStd . LimitedCharNoTyp 
      ; EstIndentPosN : LbeStd . LimitedCharNoTyp 
      (* EstIndentPos1 and EstIndentPosN are maintained assuming
         the Est subtree is formatted vertically. *) 
      ; VAR MarkFound : BOOLEAN 
      ) 
    RAISES { Backout , Thread . Alerted } 

    = VAR BtmTeStartFmtNo : EstHs . FmtNoTyp 
    ; VAR BtmTeEstTravInfo : TravUtil . EstTravInfoTyp 
    ; VAR BtmTeEstRefToPatch : EstHs . EstRefTyp 
    ; VAR BtmTeCurrentChildNoToPatch : LbeStd . EstChildNoTyp 
    ; VAR BtmTeRMChildNoToPatch : LbeStd . EstChildNoTyp 
      (* If we are a traversing a constructed EstTravInfo for a singleton-
         optimized list (which does not exist in the Est,) BtmTeEstRefToPatch,
         BtmTeCurrentChildNoToPatch, and BtmTeRMChildNoToPatch denote the  
         parent and child number of the list, which is actually the one
         list element.  Otherwise, they just duplicate 
         BtmTeEstTravInfo . EtiParentRef, BtmTeEstTravInfo . EtiChildNo, 
         and EstUtil . EstChildCt ( EstRef ) - 1.  Either way, they are what 
         are needed to make changes in the Est, from above, to the current or
         rightmost child pointer and to its KindSet.
      *)  
    ; VAR BtmTeRMChildRef : LbeStd . EstRootTyp 
    ; VAR BtmTeRMChildRelNodeNo : LbeStd . EstNodeNoTyp 
          (* ^Relative to the current Est node. *) 
    ; VAR BtmTeEstChildIsMarked : BOOLEAN 
          (* Either the current child, or, if we are off the right end of 
             the list of children, the rightmost child. 
          *) 
    ; VAR BtmTeIndentPos : LbeStd . LimitedCharNoTyp 
    ; VAR BtmTeIsFirstLine : BOOLEAN := FALSE 
    (* BtmTeIsFirstLine and BtmTeIndentPos are maintained only in descending  
       and forward states, and as if the Est subtree were formatted vertically.
    *)

    ; PROCEDURE BtmTeInitNodeToPatch ( )

      = BEGIN 
          IF BtmTeEstTravInfo . EtiIsOptSingletonList 
          THEN
            BtmTeEstRefToPatch := GrandparentEstRef  
          ; BtmTeCurrentChildNoToPatch := EstChildNo 
          ; BtmTeRMChildNoToPatch 
              := EstUtil . EstChildCt ( GrandparentEstRef ) - 1  
          ELSE
            BtmTeEstRefToPatch := BtmTeEstTravInfo . EtiParentRef 
          ; BtmTeCurrentChildNoToPatch := BtmTeEstTravInfo . EtiChildNo  
          ; BtmTeRMChildNoToPatch := BtmTeEstTravInfo . EtiChildCt - 1 
          END (* IF *) 
        END BtmTeInitNodeToPatch 

    ; PROCEDURE BtmTeInitEstTravInfoFwd 
        ( EstNodeRef : LbeStd . EstRootTyp 
        ; KindSet : EstHs . EstChildKindSetTyp 
        ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
        ) 
      RAISES { Backout } 

      = BEGIN 
          TravUtil . InitEstTravInfoFwd 
            ( BtmTeEstTravInfo , EstNodeRef , KindSet , ParentAbsNodeNo ) 
        ; BtmTeInitNodeToPatch ( ) 
        END BtmTeInitEstTravInfoFwd 

    ; PROCEDURE BtmTeInitToChildContainingNodeNo 
        ( EstNodeRef : LbeStd . EstRootTyp 
        ; RelNodeNo : LbeStd . EstNodeNoTyp 
          (* ^Sought Node number, relative to EstNodeRef. *) 
        ; KindSet : EstHs . EstChildKindSetTyp 
        ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
        ) 
      RAISES { Backout } 

      = BEGIN 
          TravUtil . InitToChildContainingNodeNo 
            ( BtmTeEstTravInfo
            , EstNodeRef 
            , RelNodeNo 
            , KindSet 
            , ParentAbsNodeNo  
            ) 
        ; BtmTeInitNodeToPatch ( ) 
        END BtmTeInitToChildContainingNodeNo 

    ; PROCEDURE BtmTeIncEstChild ( ) 
      RAISES { Backout } 

      = BEGIN (* BtmTeIncEstChild *) 
          BtmTeRMChildRef := BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
        ; BtmTeRMChildRelNodeNo := BtmTeEstTravInfo . EtiChildRelNodeNo 
        ; TravUtil . IncEstChild ( BtmTeEstTravInfo ) 
        ; IF NOT BtmTeEstTravInfo . EtiIsOptSingletonList 
          THEN 
            BtmTeCurrentChildNoToPatch := BtmTeEstTravInfo . EtiChildNo 
          END (* IF *) 
        ; BtmTeEstChildIsMarked := FALSE 
        END BtmTeIncEstChild 

    ; PROCEDURE BtmTePosForTok 
        ( Tok : LbeStd . TokTyp 
        ; StringRef : SharedStrings . T 
        ; FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : FmtKindTyp 
        ) 

      = BEGIN (* BtmTePosForTok *) 
          BtmState := BtmStateTyp . BtmStateInLine 
        ; BtmCharPos 
            := TravUtil . PosForTok 
                 ( ParseInfo . PiLang 
                 , FmtKind 
                 , BtmModTextIsToLeftOnLine 
                 , BtmCharPos 
                 , BtmTeIndentPos 
                 , FsNodeRef . FsIndentCode 
                 , BtmPrevTok 
                 , Tok 
                 ) 
        ; BtmTokBegCharPos := BtmCharPos 
        ; BtmCharPos := EstUtil . WidthSum 
             ( BtmCharPos , SharedStrings . Length ( StringRef ) ) 
        ; BtmPrevTok := Tok 
        END BtmTePosForTok 

    ; PROCEDURE BtmTeMarkEst ( ) 
      RAISES { Backout } 
      (* Mark node with absolute number BtmTmEstNodeNo as having a TempMark.
         This is messy.  In cases where we have hit a Nl and are putting 
         leftover marks onto some previous token, the previous token can 
         be just about anywhere in the tree.  (Well, not to the right.) 
         In the worst case, we might have to locate it from the top.  
         First, try various faster special cases (which are quite common). 
      *) 

      = VAR LWasAlreadyMarked : BOOLEAN 

      ; BEGIN 
          IF NOT BtmTmEstRefIsMarked 
          THEN 
            IF BtmTeEstTravInfo . EtiChildNo < BtmTeEstTravInfo . EtiChildCt 
               AND BtmTmEstNodeNo 
                   = EstAbsNodeNo + BtmTeEstTravInfo . EtiChildRelNodeNo  
            THEN (* Got lucky.  It's current child. *) 
              EstUtil . SetChildKindBitTRUE 
                ( BtmTeEstRefToPatch  
                , BtmTeCurrentChildNoToPatch  
                , EstHs . EstChildKindContainsTempMark 
                , LWasAlreadyMarked (* Dead. *) 
                ) 
            ELSIF BtmTeRMChildRelNodeNo # LbeStd . EstNodeNoNull  
                  AND BtmTmEstNodeNo = EstAbsNodeNo + BtmTeRMChildRelNodeNo 
            THEN (* Another lucky case: the rightmost child. *) 
              EstUtil . SetChildKindBitTRUE 
                ( BtmTeEstRefToPatch  
                , BtmTeRMChildNoToPatch  
                , EstHs . EstChildKindContainsTempMark 
                , LWasAlreadyMarked (* Dead. *)
                )
            ELSIF EstAbsNodeNo < BtmTmEstNodeNo 
                  AND BtmTmEstNodeNo < EstUtil . EstNodeCt ( EstRef ) 
            THEN (* Desired node is inside current subtree. *)
              Assert 
                ( NOT BtmTeEstTravInfo . EtiIsOptSingletonList  
                , AFT . A_BtmTeMarkEst_Non_singleton_singleton 
                ) 
            ; EstUtil . SetDescendentKindBitTRUE 
                ( BtmTeEstTravInfo . EtiParentRef 
                , BtmTmEstNodeNo - EstAbsNodeNo 
                , EstHs . EstChildKindContainsTempMark 
                ) 
            ELSE (* Worst case: it's outside our subtree.  Must start at top. *)
              EstUtil . SetDescendentKindBitTRUE 
                ( ImageRef . ItPers . IpEstRoot  
                , BtmTmEstNodeNo 
                , EstHs . EstChildKindContainsTempMark 
                ) 
            END (* IF *) 
          ; BtmTmEstRefIsMarked := TRUE 
          END (* IF *) 
        END BtmTeMarkEst 

    ; PROCEDURE BtmTeBuildTempMarksForTok 
        ( TokToPos : LbeStd . LimitedCharNoTyp ) 
      RAISES { Backout , Thread . Alerted } 
      (* Build temp marks for any marked points that are:
         1) In prior lines.  This can happen if they are in deleted insertion
            tokens and could not be moved to the left on their own line, or
         2) Within the current token, or
         3) On the current line and the current token is the last nonblank
            item on the line.  
      *) 

      = VAR LMarkCompare : [ - 1 .. 1 ] 

      ; BEGIN (* BtmTeBuildTempMarksForTok *) 
          IF BtmTempMarkNextElemNo < BtmTempMarkCt
             (* ^Another mark exists at all. *) 
             AND BtmTmEstRef # NIL 
                 (* ^There is something to attach a temp mark to. *) 
          THEN 
            TRY 
              LMarkCompare 
                := Marks . Compare 
                     ( BtmLineMarkMeat . LmTokMark 
                     , BtmLinesRef . LrBolTokMark 
                     )
            EXCEPT Marks . Unordered 
            => CantHappen ( AFT . A_BtmTeBuildTempMarksForTok_Unordered_marks ) 
            END (* TRY EXCEPT *) 
          ; IF LMarkCompare = - 1 (* Next mark is in a prior line. *) 
               OR LMarkCompare = 0 (* Next mark is on this line. *) 
                  AND ( BtmLineMarkMeat . LmCharPos <= TokToPos 
                        (* Next mark lies within current token. *) 
                        OR BtmNonblankOnLineToPos <= TokToPos 
                           (* Current token is last on this line. *) 
                      ) 
            THEN (* One or more TempMarks belong on this token. *) 
              LOOP 
                WITH 
                  WTempMark 
                  = ParseInfo . PiOrigTempMarkListRef
                    ^ [ BtmTempMarkNextElemNo ] 
                DO 
                  WTempMark . TokMark . TkmEstNodeNo := BtmTmEstNodeNo
                ; WTempMark . TokMark . TkmEstRef := BtmTmEstRef  
                ; WTempMark . TokMark . TkmKind := BtmTmKind 
                ; WTempMark . TokMark . TkmFmtNo := BtmTmFmtNo 
                ; IF LMarkCompare = - 1 
                  THEN WTempMark . CharPos := 0 
                  ELSE 
                    WTempMark . CharPos 
                      := BtmLineMarkMeat . LmCharPos - BtmTokBegCharPos 
(* TODO: ^Something about inverse saturation arithmetic. *) 
                  END (* IF *) 
                ; IF WTempMark . TokMark . TkmKind = MarkKindTyp . BlankLine 
                  THEN 
                    WTempMark . LineNo := BtmLineMarkMeat . LmLineNo 
                  ELSE 
                    WTempMark . LineNo := 0 
                  END (* IF *) 
                ; Assert 
                    ( WTempMark . TokMark . TkmKind # MarkKindTyp . Null 
                    , AFT . A_BtmTeBuildTempMarksForTok_Null_temp_mark 
                    ) 
                END (* WITH WTempMark *) 
              ; INC ( BtmTempMarkNextElemNo ) 
              ; IF BtmTempMarkNextElemNo >= BtmTempMarkCt 
                THEN (* No more marks at all, so quit. *) 
                  BtmState := BtmStateTyp . BtmStateDoneWithLine 
                ; Assert 
                    ( ISTYPE 
                        ( BtmLineMarkMeat . LmRightLink 
                        , PaintHs . LineMarkHeaderTyp 
                        ) 
                    , AFT . A_BtmTeBuildTempMarksForTok_ExtraMarks 
                    ) 
                ; EXIT 
                ELSE (* There is another LineMark. *) 
                  BtmLineMarkMeat := BtmLineMarkMeat . LmRightLink  
                ; Display . SecureSucc 
                    ( ImageRef , BtmLineMarkMeat . LmLinesRef ) 
                ; TRY 
                    LMarkCompare 
                      := Marks . Compare  
                           ( BtmLineMarkMeat . LmTokMark 
                           , BtmLinesRef . LrBolTokMark 
                           ) 
                  EXCEPT Marks . Unordered 
                  => RAISE 
                       Backout 
                         ( MessageCodes . Image 
                             ( AFT . A_BtmTeBuildTempMarksForTok_Unordered_marks ) 
                         ) 
                  END (* TRY EXCEPT *) 
                ; IF LMarkCompare = 1 (* Next mark is in a later line. *) 
                  THEN (* No more marks for this line, so quit. *) 
                    BtmState := BtmStateTyp . BtmStateDoneWithLine 
                  ; EXIT 
                  ELSIF LMarkCompare = 0 (* Next mark is on this line. *) 
                        AND BtmLineMarkMeat . LmCharPos > TokToPos
                            (* ^Next mark lies beyond current token. *) 
                        AND BtmNonblankOnLineToPos > TokToPos 
                            (* Current token is not last on this line. *) 
                  THEN (* Have moved out of this token. *) 
                    EXIT 
                  END (* IF *) 
                END (* IF *) 
              END (* LOOP *) 
            ; BtmTeMarkEst ( ) 
            ; MarkFound := TRUE 
            END (* IF *) 
          END (* IF *) 
        END BtmTeBuildTempMarksForTok 

    ; PROCEDURE BtmTeAstString 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : FmtKindTyp 
        ) 
      RAISES { Backout , Thread . Alerted } 

      = BEGIN (* BtmTeAstString *) 
          IF BtmTeEstTravInfo . EtiNodeRef # NIL 
          THEN 
            BtmTePosForTok 
              ( SharedStrings . Tok ( BtmTeEstTravInfo . EtiStringRef ) 
              , BtmTeEstTravInfo . EtiStringRef 
              , FsNodeRef 
              , FmtKind 
              ) 
          ; BtmTmEstRef := BtmTeEstTravInfo . EtiStringRef 
          ; BtmTmEstRefIsMarked := FALSE 
          ; BtmTmEstNodeNo := EstAbsNodeNo 
          ; BtmTmKind := MarkKindTyp . Plain 
          ; BtmTmFmtNo := FsNodeRef . FsFmtNo 
            (* ^Dead, but maybe useful in debugging. *) 
          ; BtmTeBuildTempMarksForTok ( BtmCharPos ) 
          (* This is a leaf.  Mark bit will be set at higher level. *) 
          END (* IF *) 
        END BtmTeAstString 

    ; PROCEDURE BtmTeTraverseFsFixedChildren 
        ( ParentFsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : FmtKindTyp
        ; InitialFsChildNo : LangUtil . FsChildNoTyp 
        ) 
      RAISES { Backout , Thread . Alerted } 

      = VAR LFsChildNo : LangUtil . FsChildNoTyp 
      ; VAR LFsChildCt : LangUtil . FsChildNoTyp 

      ; BEGIN (* BtmTeTfsTraverseFsFixedChildren *) 
          IF ParentFsNodeRef . FsChildren = NIL 
          THEN LFsChildCt := 0 
          ELSE LFsChildCt := NUMBER ( ParentFsNodeRef . FsChildren ^ ) 
          END (* IF*) 
        ; LFsChildNo := InitialFsChildNo  
        ; Assert 
            ( 0 <= LFsChildNo AND LFsChildNo < LFsChildCt 
            , AFT . A_BtmTeTraverseFsFixedChildren_InitialChildNumberOutOfRange  
            ) 
        ; LOOP 
            IF LFsChildNo >= LFsChildCt 
            THEN 
              EXIT 
            ELSE 
              BtmTeTraverseFs 
                ( ParentFsNodeRef . FsChildren ^ [ LFsChildNo ] , FmtKind ) 
            ; IF BtmState = BtmStateTyp . BtmStateDoneWithLine 
              THEN 
                EXIT 
              ELSE 
                INC ( LFsChildNo ) 
              END (* IF *) 
            END (* IF *) 
          END (* LOOP *) 
        END BtmTeTraverseFsFixedChildren 

    ; PROCEDURE BtmTeTraverseFsListChildren 
        ( ParentFsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : FmtKindTyp 
        ; InitialFsChildNo : LangUtil . FsChildNoTyp 
        ) 
      RAISES { Backout , Thread . Alerted } 

      = VAR LFsChildNo : LangUtil . FsChildNoTyp 
      ; VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LRMFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* BtmTeTfsTraverseFsListChildren *) 
          LFsChildCt := NUMBER ( ParentFsNodeRef . FsChildren ^ ) 
        ; LFsChildNo := InitialFsChildNo 
        ; Assert 
            ( 0 <= LFsChildNo AND LFsChildNo < LFsChildCt 
            , AFT . A_BtmTeTraverseFsListChildren_InitialChildNumberOutOfRange  
            ) 
        ; IF ParentFsNodeRef . FsKind IN LangUtil . FsKindSetEstListTrail 
             AND BtmTeEstTravInfo . EtiParentRef . EstNodeKind 
                 = EstHs . EstNodeKindTyp . EstNodeKindTrail
          THEN LRMFsChildNo := LFsChildCt - 1 
          ELSE LRMFsChildNo := 0 
          END (* IF *) 
        ; LOOP 
            BtmTeTraverseFs 
              ( ParentFsNodeRef . FsChildren ^ [ LFsChildNo ] , FmtKind ) 
          ; IF BtmState = BtmStateTyp . BtmStateDoneWithLine 
            THEN 
              EXIT 
            ELSIF LFsChildNo = LRMFsChildNo  
                  AND BtmTeEstTravInfo . EtiChildNo 
                      >= BtmTeEstTravInfo . EtiChildCt 
            THEN 
              EXIT 
            ELSE 
              LFsChildNo := ( LFsChildNo + 1 ) MOD LFsChildCt 
            END (* IF *) 
          END (* LOOP *) 
        END BtmTeTraverseFsListChildren 

    ; PROCEDURE BtmTeTraverseFs  
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FsFmtKind : FmtKindTyp 
        ) 
      RAISES { Backout , Thread . Alerted } 

      = PROCEDURE BtmTeTfsSetIndentInfo ( )  

(* MAYBE: This code occurs twice in TempMark and once in each of
          MergeTxt and LIneMarks.  Consider factoring it out into
          TravUtil.  It would require 4 parameters.
*)
        = BEGIN 
            IF BtmTeEstTravInfo . EtiParentRef = NIL
            THEN BtmTeIsFirstLine := TRUE
            ELSE 
              BtmTeIsFirstLine 
                := BtmTeEstTravInfo . EtiChildNo 
                   < BtmTeEstTravInfo . EtiParentRef . KTreeEstChildCtLeftOfNl
            END (* IF *) 
          ; IF BtmTeIsFirstLine 
            THEN
              BtmTeIndentPos := EstIndentPos1 
            ELSE
              BtmTeIndentPos := EstIndentPosN 
            END (* IF *) 
          END BtmTeTfsSetIndentInfo 

      ; PROCEDURE BtmTeMaybeInsertDummy 
          ( VAR (* IN OUT *) EstChildRef : LbeStd . EstRootTyp 
          ; EstRefToPatch : EstHs . EstRefTyp 
(* TODO:    ^This receives BtmTeEstRefToPatch in all calls.  
             De-parameterize it.
*)   
          ; EstChildNoToPatch : LbeStd . EstChildNoTyp 
          ; EstNodeNo : LbeStd . EstNodeNoTyp 
          ) 
        RAISES { Backout } 
        (* If EstChildRef is NIL, change it to point to a dummy.  There
           has to be something for the temp mark to point to.  Change 
           both cached pointer (EstChildRef) and the original in the Est.
        *) 

        = BEGIN 
            IF EstChildRef = NIL 
            THEN (* Must change both cached pointer and the original in Est. *)
              EstChildRef := NEW ( ModHs . EstDummyTempMarkTyp ) 
            ; EstUtil . SetChildRef 
                ( EstRef := EstRefToPatch  
                , ChildNo := EstChildNoToPatch  
                , NewChildRef := EstChildRef 
                ) 
            (* Leave EstChildKindNonNIL alone.  The dummy is only temporary.
               It will be set back to NIL after rebuild of LineMarks. 
            *) 
            ; Assertions . MessageText 
                ( "Inserted DummyTempMark for node no "
                  & LbeStd . EstNodeNoImage ( EstNodeNo ) 
                ) 
            END (* IF *) 
          END BtmTeMaybeInsertDummy 

      ; PROCEDURE BtmTeTfsPrepareLeafMark ( TmKind : MarkKindTyp )  

        = BEGIN 
            BtmTmEstRef 
              := BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
          ; BtmTmEstRefIsMarked := FALSE 
          ; BtmTmEstNodeNo 
              := EstAbsNodeNo + BtmTeEstTravInfo . EtiChildRelNodeNo 
          ; BtmTmKind := TmKind  
          ; BtmTmFmtNo := BtmTeEstTravInfo . EtiChildFmtNo 
            (* ^Dead, but maybe useful in debugging. *) 
          END BtmTeTfsPrepareLeafMark

      ; PROCEDURE BtmTeTfsPrepareFmtNoMark ( ) 
        RAISES { Backout }
        (* Set the following nonlocal variables, in preparation to
           create a temp mark to an item identified by a FmtNo: 
           BtmTmEstRef, BtmTeEstNodeNo, BtmTmKind, BtmTmFmtNo,
           and BtmTmEstRefIsMarked. 
        *)  

        = BEGIN 
            IF BtmTeEstTravInfo . EtiChildCt = 0 
            THEN (* ChildFmtNo mark. *) 
              BtmTmEstRef := BtmTeEstTravInfo . EtiParentRef 
            ; BtmTmEstNodeNo := EstAbsNodeNo 
            ; BtmTmKind := MarkKindTyp . ChildFmtNo 
            ELSIF BtmTeEstTravInfo . EtiChildNo 
                    < BtmTeEstTravInfo . EtiChildCt 
            THEN (* Left sib mark. *) 
              BtmTeMaybeInsertDummy 
                ( (* VAR *) BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                , BtmTeEstRefToPatch 
                , BtmTeCurrentChildNoToPatch 
                , BtmTmEstNodeNo 
                ) 
(* TODO: This could insert a dummy unnecessarily, if no TempMarks end up 
         being built in this FmtNo token.  Perhaps split 
         BtmBuildTempMarksForTok into a predicate (marks exist in this tok)
         and an action procedure (build the marks).  This is also needed 
         for the RightSib mark case below.
*) 
            ; BtmTmEstRef 
                := BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
            ; BtmTmEstNodeNo 
                := EstAbsNodeNo 
                   + BtmTeEstTravInfo . EtiChildRelNodeNo 
            ; BtmTmKind := MarkKindTyp . LeftSibFmtNo 
            ELSE (* Right sib mark. *) 
              BtmTeMaybeInsertDummy 
                ( (* VAR *) BtmTeRMChildRef 
                , BtmTeEstRefToPatch 
                , BtmTeRMChildNoToPatch 
                , BtmTmEstNodeNo 
                ) 
            ; BtmTmEstRef := BtmTeRMChildRef 
            ; BtmTmEstNodeNo := EstAbsNodeNo + BtmTeRMChildRelNodeNo 
            ; BtmTmKind := MarkKindTyp . RightSibFmtNo 
            END (* IF *) 
          ; BtmTmFmtNo := FsNodeRef . FsFmtNo 
          ; BtmTmEstRefIsMarked := FALSE 
          END BtmTeTfsPrepareFmtNoMark 

      ; PROCEDURE BtmTeTfsEstSubtree ( ) 
        RAISES { Backout , Thread . Alerted } 
        (* Does NOT handle ModTok. *) 

        = VAR LFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR LChildFmtKind : FmtKindTyp 
        ; VAR LChildIndentPos1 : LbeStd . LimitedCharNoTyp 
        ; VAR LChildIndentPosN : LbeStd . LimitedCharNoTyp 
        ; VAR LMarkFound : BOOLEAN 
        ; VAR LWasAlreadyMarked : BOOLEAN 

        ; BEGIN (* BtmTeTfsEstSubtree *) 
            LFsNodeRef 
              := LangUtil . FsRuleForEstChild  
                   ( ParseInfo . PiLang 
                   , FsNodeRef 
                   , BtmTeEstTravInfo . EtiChildLeafElem 
                   ) 
          ; CASE BtmState 
            OF BtmStateTyp . BtmStateStartAtEnd 
            , BtmStateTyp . BtmStateStartAtBeg 
            => BtmTeTfsSetIndentInfo ( )   
            ; TravUtil . ChildIndentPositions 
                ( ImageRef . ItPers . IpLang                 
                , FsNodeRef 
                , EstIndentPos1 
                , EstIndentPosN
                , (* VAR *) ChildIndentPos1 := LChildIndentPos1 
                , (* VAR *) ChildIndentPosN := LChildIndentPosN 
                , IsFirstLine := BtmTeIsFirstLine 
                ) 
            ; LChildFmtKind 
                := TravUtil . FmtKindForEstDescending 
                    ( FsKind := LFsNodeRef . FsKind
                    , ParentFmtKind := FsFmtKind 
                    , FirstLineIndentPos := LChildIndentPos1 
                    , EstRef 
                        := BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                    , StartMark := BtmLineMarkMeat . LmTokMark 
                    , StartMarkIsKnownNl := TRUE 
                    ) 

            ELSE 
              TravUtil . ChildIndentPositions 
                ( ImageRef . ItPers . IpLang 
                , FsNodeRef 
                , EstIndentPos1 
                , EstIndentPosN
                , (* VAR *) ChildIndentPos1 := LChildIndentPos1 
                , (* VAR *) ChildIndentPosN := LChildIndentPosN 
                , IsFirstLine := BtmTeIsFirstLine 
                ) 
            ; LChildFmtKind 
                := TravUtil . FmtKindForEstTraversing 
                    ( Lang := ParseInfo . PiLang 
                    , CharPos := BtmCharPos 
                    , ModTextIsToLeftOnLine := BtmModTextIsToLeftOnLine 
                    , PrevTok := BtmPrevTok 
                    , FsKind := LFsNodeRef . FsKind 
                    , ParentFmtKind := FsFmtKind 
                    , FirstLineIndentPos := LChildIndentPos1 
                    , EstRef 
                        := BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                    ) 
            END (* CASE *) 
          ; BtmTraverseEst 
              ( BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              , BtmTeEstTravInfo . EtiChildLeafElem . LeKindSet  
              , BtmTeEstTravInfo . EtiChildNo 
              , BtmTeEstTravInfo . EtiParentRef   
              , EstAbsNodeNo + BtmTeEstTravInfo . EtiChildRelNodeNo 
              , LFsNodeRef 
              , LChildFmtKind 
              , LChildIndentPos1 
              , LChildIndentPosN 
              , (* VAR *) MarkFound := LMarkFound 
              ) 
          ; IF LMarkFound AND NOT BtmTeEstChildIsMarked 
            THEN 
              EstUtil . SetChildKindBitTRUE 
                ( BtmTeEstRefToPatch 
                , BtmTeCurrentChildNoToPatch 
                , EstHs . EstChildKindContainsTempMark 
                , LWasAlreadyMarked 
                ) 
            ; BtmTeEstChildIsMarked := TRUE 
            END (* IF *) 
          ; MarkFound := MarkFound OR LMarkFound 
          ; BtmTeIncEstChild ( ) 
          END BtmTeTfsEstSubtree 

      (* Mods. *) 

      ; PROCEDURE BtmTeTfsModBlankLineInterior ( ) 
        RAISES { Backout , Thread . Alerted } 
        (* Handle marks inside the blank line mod. *) 

        = BEGIN
            BtmCharPos := 0 
          ; BtmTokBegCharPos := 0 
          ; BtmTeTfsPrepareLeafMark ( MarkKindTyp . BlankLine )
          ; BtmTeBuildTempMarksForTok ( LbeStd . LimitedCharNoInfinity ) 
          (* Now we are at the Nl after of the Bl mod. *) 
          ; TRY 
              Assert 
                ( BtmTempMarkNextElemNo >= BtmTempMarkCt 
                  OR Marks . Compare  
                       ( BtmLineMarkMeat . LmTokMark 
                       , BtmLinesRef . LrBolTokMark 
                       ) 
                     = 1
                , AFT . A_BtmTeTfsModBlankLineInterior_Leftover_Marks 
                ) 
              EXCEPT Marks . Unordered 
              => RAISE 
                   Backout 
                     ( MessageCodes . Image 
                         ( AFT . A_BtmTeTfsModBlankLineInterior_Unordered_marks ) 
                     ) 
              END (* TRY EXCEPT *) 
          ; BtmState := BtmStateTyp . BtmStateDoneWithLine 
          END BtmTeTfsModBlankLineInterior  

      ; PROCEDURE BtmTeTfsModBlankLine 
          ( <* UNUSED *> ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { Backout , Thread . Alerted } 

        = BEGIN (* BtmTeTfsModBlankLine  *) 
            CASE BtmState 

            OF BtmStateTyp . BtmStateStartAtEnd 
            => (* Go into the line following the blank line mod. *) 
              BtmTeTfsSetIndentInfo ( ) 
            ; BtmTeIncEstChild ( ) 
            ; BtmState := BtmStateTyp . BtmStatePassingNl 

            | BtmStateTyp . BtmStateInLine 
            => (* Terminates the line preceeding the blank line mod. *) 
              IF BtmOrphanedMarksExistAtEOL ( )  
              THEN 
                BtmEnterLine 
                  ( BtmLinesRef . LrRightLink (* NARROW can't fail. *) )
              ; BtmState := BtmStateTyp . BtmStateInLine 
              ; BtmTeTfsModBlankLineInterior ( ) 
              ELSE 
                BtmTeBuildTempMarksForTok ( LbeStd . LimitedCharNoInfinity ) 
                 (* ^Use leftover values of BtmTmEstRef, BtmTmEstNodeNo, 
                     BtmTmKind, and BtmTmFmtNo *) 
              ; BtmState := BtmStateTyp . BtmStateDoneWithLine 
              END (* IF *) 

            | BtmStateTyp . BtmStateStartAtBeg 
            => BtmTeTfsSetIndentInfo ( ) 
            ; BtmTeTfsModBlankLineInterior ( ) 

            | BtmStateTyp . BtmStatePassingNl 
            => BtmTeTfsModBlankLineInterior ( ) 

            ELSE 
              CantHappen ( AFT . A_BtmTeTfsModBlankLine_BadState ) 
            END (* CASE BtmState *) 
          END BtmTeTfsModBlankLine 

      ; PROCEDURE BtmTeTfsModCmnt ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { Backout , Thread . Alerted } 

        = BEGIN (* BtmTeTfsModCmnt *) 
            IF BtmState = BtmStateTyp . BtmStateStartAtEnd 
            THEN (* Consume the comment and start after it. *) 
              Assert 
                ( ModCmnt . ModCmntNlAfter 
                , AFT . A_BtmTeTfsModCmntStartAtEndNoNlAfter 
                ) 
            ; BtmTeTfsSetIndentInfo ( ) 
            ; BtmTeIncEstChild ( ) 
            ; BtmState := BtmStateTyp . BtmStatePassingNl 
            ELSE 
              TYPECASE ModCmnt 

              OF ModHs . ModCmntLeadingFixedTyp 
              => IF ModCmnt . ModCmntNlBefore 
                 THEN 
                   IF BtmState = BtmStateTyp . BtmStateInLine 
                   THEN BtmState := BtmStateTyp . BtmStateDoneWithLine 
                   ELSE 
                     IF BtmState = BtmStateTyp . BtmStateStartAtBeg 
                     THEN BtmTeTfsSetIndentInfo ( ) 
                     ELSIF BtmState = BtmStateTyp . BtmStatePassingNl  
                     THEN 
                     ELSE 
                       CantHappen ( AFT . A_BtmTeTfsModCmnt_BadStateFixed ) 
                     END (* IF *) 
                   ; BtmTokBegCharPos := ModCmnt . ModCmntFromPos 
                   END (* IF *) 
                 ELSE 
                   Assert 
                     ( BtmState = BtmStateTyp . BtmStateInLine 
                     , AFT . A_BtmTeTfsModCmnt_SameLineFixedNotInLine 
                     ) 
                 ; IF BtmCharPos > ModCmnt . ModCmntFromPos 
                   THEN (* Implied new line. *) 
                     BtmState := BtmStateTyp . BtmStateDoneWithLine 
                   ELSE 
                     BtmTokBegCharPos := ModCmnt . ModCmntFromPos 
                   END (* IF *) 
                 END (* IF *) 

              | ModHs . ModCmntLeadingRelativeTyp 
              => IF ModCmnt . ModCmntNlBefore 
                 THEN 
                   IF BtmState = BtmStateTyp . BtmStateInLine 
                   THEN BtmState := BtmStateTyp . BtmStateDoneWithLine 
                   ELSE 
                     IF BtmState = BtmStateTyp . BtmStateStartAtBeg 
                     THEN BtmTeTfsSetIndentInfo ( ) 
                     ELSIF BtmState = BtmStateTyp . BtmStatePassingNl  
                     THEN 
                     ELSE 
                       CantHappen ( AFT . A_BtmTeTfsModCmnt_BadStateRelative ) 
                     END (* IF *) 
                   ; BtmTokBegCharPos 
                       := EstUtil . WidthSum 
                            ( TravUtil . IndentPos 
                                ( ParseInfo . PiLang 
                                , BtmTeIndentPos 
                                , FsNodeRef . FsIndentCode 
                                ) 
                            , ModCmnt . ModCmntFromPos 
                            ) 
                   END (* IF *) 
                 ELSE
                   Assert 
                     ( BtmState = BtmStateTyp . BtmStateInLine 
                     , AFT . A_BtmTeTfsModCmnt_LeadingRelativeNotInLine 
                     ) 
                 ; BtmTokBegCharPos 
                     := EstUtil . WidthSum 
                          ( TravUtil . IndentPos 
                              ( ParseInfo . PiLang 
                              , BtmTeIndentPos 
                              , FsNodeRef . FsIndentCode 
                              ) 
                          , ModCmnt . ModCmntFromPos 
                          ) 
                 ; BtmTokBegCharPos := MAX ( BtmCharPos , BtmTokBegCharPos ) 
                 END (* IF*) 

              | ModHs . ModCmntTrailingFixedTyp 
              => Assert 
                   ( BtmState = BtmStateTyp . BtmStateInLine 
                   , AFT . A_BtmTeTfsModCmnt_SameLineFixedNotInLine 
                   ) 
              ; IF BtmCharPos > ModCmnt . ModCmntFromPos 
                THEN (* Implied new line. *)
                  BtmState := BtmStateTyp . BtmStateDoneWithLine 
                ELSE 
                  BtmTokBegCharPos := ModCmnt . ModCmntFromPos 
                END (* IF *) 

              | ModHs . ModCmntTrailingRelativeTyp 
              => Assert 
                   ( BtmState = BtmStateTyp . BtmStateInLine 
                   , AFT . A_BtmTeTfsModCmnt_SameLineRelativeNotInLine 
                   ) 
              ; BtmTokBegCharPos 
                  := EstUtil . WidthSum 
                       ( BtmCharPos , ModCmnt . ModCmntFromPos ) 
                    (* ^SameLine comment implies there is a token 
                       preceeding, so no need to consider IndentPos. *) 
              ELSE 
                CantHappen ( AFT . A_BtmTeTfsModCmnt_BadType ) 
              END (* TYPECASE *) 
            ; IF BtmState = BtmStateTyp . BtmStateDoneWithLine 
              THEN (* Current line ends at beginning of comment. *) 
                IF BtmOrphanedMarksExistAtEOL ( )  
                THEN 
                  BtmEnterLine 
                    ( BtmLinesRef . LrRightLink (* NARROW can't fail. *) )
                ; BtmState := BtmStateTyp . BtmStateInLine 
                ELSE (* There was a parser-visible token on the line.  *) 
                  BtmTeBuildTempMarksForTok ( LbeStd . LimitedCharNoInfinity ) 
                  (* ^Use leftover values of BtmTmEstRef, BtmTmEstNodeNo, 
                      BtmTmKind, and BtmTmFmtNo 
                  *) 
                END (* IF *) 
              END (* IF *) 
            ; IF BtmState # BtmStateTyp . BtmStateDoneWithLine 
              THEN 
                BtmCharPos 
                  := EstUtil . WidthSum 
                       ( BtmTokBegCharPos 
                       , SharedStrings . Length ( ModCmnt . ModCmntStringRef ) 
                       ) 
              ; BtmTeTfsPrepareLeafMark ( MarkKindTyp . Plain )  
              ; BtmTeBuildTempMarksForTok ( BtmCharPos ) 
              ; IF ModCmnt . ModCmntNlAfter 
                THEN 
                  BtmTeBuildTempMarksForTok ( LbeStd . LimitedCharNoInfinity ) 
                ; BtmState := BtmStateTyp . BtmStateDoneWithLine 
                ELSE 
                  BtmPrevTok := LbeStd . Tok__Cmnt 
                ; BtmTeIncEstChild ( ) 
                ; BtmState := BtmStateTyp . BtmStateInLine 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END BtmTeTfsModCmnt 

      ; PROCEDURE BtmTeTfsLeadingModText ( ModText : ModHs . ModTextTyp ) 
        RAISES { Backout , Thread . Alerted } 

        = BEGIN (* BtmTeTfsLeadingModText *) 
            IF BtmState = BtmStateTyp . BtmStateStartAtEnd 
            THEN (* Consume the ModText and enter the following line. *) 
              Assert 
                ( ModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
                , AFT . A_BtmTeTfsLeadingModTextStartAtEndNoNlAfter 
                ) 
            ; BtmTeTfsSetIndentInfo ( ) 
            ; BtmTeIncEstChild ( ) 
            ; BtmState := BtmStateTyp . BtmStatePassingNl 
            ELSE 
              IF BtmState = BtmStateTyp . BtmStateStartAtBeg 
              THEN
                BtmTeTfsSetIndentInfo ( ) 
              END (* IF *) 
            ; IF ModText . ModTextLeftTokToPos = 0 (* Nl before *) 
                 AND BtmState = BtmStateTyp . BtmStateInLine 
              THEN (* The current line terminates at the start of the mod. *) 
                IF BtmOrphanedMarksExistAtEOL ( ) 
                THEN 
                  BtmEnterLine 
                    ( BtmLinesRef . LrRightLink (* NARROW can't fail. *) )
                ; BtmState := BtmStateTyp . BtmStateInLine 
                ELSE (* There was a parser-visible token on the line.  *) 
                  BtmTeBuildTempMarksForTok ( LbeStd . LimitedCharNoInfinity ) 
                  (* ^Use leftover values of BtmTmEstRef, BtmTmEstNodeNo, 
                      BtmTmKind, and BtmTmFmtNo 
                  *) 
                ; BtmState := BtmStateTyp . BtmStateDoneWithLine 
                END (* IF *) 
              ELSE (* Process the mod as part of this line. *) 
                Assert 
                  ( BtmCharPos <= ModText . ModTextFromPos 
                  , AFT . A_BtmTeTfsLeadingModText_ForcedNl 
                  )
              ; BtmState := BtmStateTyp . BtmStateInLine 
              END (* IF *)  
            ; IF BtmState # BtmStateTyp . BtmStateDoneWithLine 
              THEN (* Continue into the ModText. *)  
                IF ( ModText . ModTextStringRef = NIL 
                     OR SharedStrings . Length ( ModText . ModTextStringRef ) 
                        = 0 
                   ) 
                   AND ModText . ModTextLeftTokToPos < BtmNonblankOnLineToPos 
                THEN (* ModText with empty string. *) 
                  (* Put no TempMark on an empty ModText,
                     because the TempMark could be orphaned in ParseTrv when
                     rescanning delivers nothing from within the ModText.
                  *) 
                  Assert
                    ( ModText . ModTextToPos <= BtmNonblankOnLineToPos 
                    , AFT . A_BtmTeTfsLeadingModText_Empty_ModText_straddles 
                    ) 
                
                ; BtmTeBuildTempMarksForTok ( ModText . ModTextFromPos ) 
                ; BtmTokBegCharPos := ModText . ModTextFromPos 
                ; BtmCharPos := ModText . ModTextToPos 
                ELSE  
                  BtmTokBegCharPos := ModText . ModTextFromPos 
                ; BtmCharPos 
                    := EstUtil . WidthSum 
                         ( ModText . ModTextFromPos 
                         , SharedStrings . Length 
                             ( ModText . ModTextStringRef ) 
                         ) 
                ; BtmTeTfsPrepareLeafMark ( MarkKindTyp . Plain ) 
                ; BtmTeBuildTempMarksForTok ( BtmCharPos ) 
                END (* IF *) 
              ; IF ModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
                THEN 
                  BtmTeBuildTempMarksForTok ( LbeStd . LimitedCharNoInfinity ) 
                ; BtmState := BtmStateTyp . BtmStateDoneWithLine 
                ELSE 
                  BtmPrevTok := LbeStd . Tok__ModText  
                ; BtmTeIncEstChild ( ) 
                ; BtmModTextIsToLeftOnLine := TRUE 
                ; BtmCharPos := ModText . ModTextToPos 
                ; BtmState := BtmStateTyp . BtmStateInLine 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END BtmTeTfsLeadingModText 

      ; PROCEDURE BtmTeTfsModTok ( EstRef : EstHs . EstRefTyp ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR LMarkFound : BOOLEAN 
        ; VAR LWasAlreadyMarked : BOOLEAN 

        ; BEGIN 
            CASE BtmState <* NOWARN *> 
            OF BtmStateTyp . BtmStateStartAtBeg 
            , BtmStateTyp . BtmStateStartAtEnd 
            => BtmTeTfsSetIndentInfo ( )
            ; IF BtmLineMarkMeat . LmTokMark . TkmEstNodeNo 
                  = EstAbsNodeNo + BtmTeEstTravInfo . EtiChildRelNodeNo 
                  AND BtmLineMarkMeat . LmTokMark . TkmKind = MarkKindTyp . Plain 
              THEN (* Starting mark denotes this ModTok.  This means we are
                      starting at an implied Nl for the ModTok. *)  
                BtmCharPos := BtmTeIndentPos
              ; BtmState := BtmStateTyp . BtmStatePassingNl
              END (* IF *) 

            | BtmStateTyp . BtmStatePassingNl 
              => BtmTeTfsSetIndentInfo ( ) 

            | BtmStateTyp . BtmStateInLine   
            => BtmTeTfsSetIndentInfo ( ) 
            ; IF EstUtil . CharPosPlusWidthInfo 
                    ( BtmCharPos , EstRef . KTreeWidthInfo ) 
                  > Options . RightMargin 
              THEN (* Implied Nl before the ModTok, which ends this line. *)
                IF BtmOrphanedMarksExistAtEOL ( )  
                THEN 
                  BtmEnterLine 
                    ( BtmLinesRef . LrRightLink (* NARROW can't fail. *) )
                (* And fall through to continue traversing LtoR. *) 
                ELSE 
                  BtmTeBuildTempMarksForTok ( LbeStd . LimitedCharNoInfinity ) 
                   (* ^Use leftover values of BtmTmEstRef, BtmTmEstNodeNo, 
                       BtmTmKind, and BtmTmFmtNo *) 
                ; BtmState := BtmStateTyp . BtmStateDoneWithLine 
                ; RETURN 
                END (* IF *) 
              ELSE (* Fall through. *) 
              END (* IF *) 
            END (* CASE *)

          (* Traverse the ModTok subtree. *)  
          ; LChildFsNodeRef 
              := EstUtil . FsRuleForEstNode 
                   ( ParseInfo . PiLang 
                   , BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                   ) 
          ; BtmTraverseEst 
              ( BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              , BtmTeEstTravInfo . EtiChildLeafElem . LeKindSet  
              , BtmTeEstTravInfo . EtiChildNo 
              , BtmTeEstTravInfo . EtiParentRef   
              , EstAbsNodeNo + BtmTeEstTravInfo . EtiChildRelNodeNo 
              , LChildFsNodeRef 
              , FmtKindHoriz  
              , BtmTeIndentPos 
              , BtmTeIndentPos 
              , (* VAR *) MarkFound := LMarkFound 
              ) 
          ; IF LMarkFound AND NOT BtmTeEstChildIsMarked 
            THEN 
              EstUtil . SetChildKindBitTRUE 
                ( BtmTeEstRefToPatch 
                , BtmTeCurrentChildNoToPatch  
                , EstHs . EstChildKindContainsTempMark 
                , (* VAR *) LWasAlreadyMarked (* Dead. *)  
                ) 
            ; BtmTeEstChildIsMarked := TRUE 
            END (* IF *) 
          ; MarkFound := MarkFound OR LMarkFound 
          ; BtmTeIncEstChild ( ) 
          END BtmTeTfsModTok 

      ; PROCEDURE BtmTeTfsLeadingMods 
          ( VAR Delete : BOOLEAN 
            (* ^A tok delete mod applies to next tok. *) 
          ; VAR IsRepair : BOOLEAN 
          ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LChildRef : LbeStd . EstRootTyp 
        ; VAR LModDelIsFinished : BOOLEAN 

        ; BEGIN (* BtmTeTfsLeadingMods *) 
            Delete := FALSE 
          ; IsRepair := FALSE 
          ; LOOP (* Thru leading mods. *) 
              TravUtil . CheckModFwd 
                ( EstTravInfo := BtmTeEstTravInfo 
                , FsNodeRef := FsNodeRef 
                , (* VAR *) RelevantModRef := LChildRef 
                , (* VAR *) ModDelIsFinished := LModDelIsFinished 
                ) 
            ; TYPECASE LChildRef 

              OF NULL 
              => EXIT 

              (* Deletion mod. *) 
              | ModHs . ModDelTyp  
              => Delete := TRUE 
              ; IsRepair 
                  := EstHs . EstChildKindContainsInsertionRepair 
                     IN BtmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
              ; IF LModDelIsFinished 
                THEN BtmTeIncEstChild ( ) 
                ELSE EXIT 
                END (* IF *) 

              (* Blank line. *) 
              | ModHs . ModBlankLineTyp ( TModBlankLine ) 
              => BtmTeTfsModBlankLine ( TModBlankLine ) 

              (* Comment. *) 
              | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
              => BtmTeTfsModCmnt ( TModCmnt ) 

              (* Text insertion. *) 
              | ModHs . ModTextTyp ( TModText ) 
              => BtmTeTfsLeadingModText ( TModText ) 

              (* Token insertion. *) 
              | EstHs . EstRefTyp ( TEstRef ) 
                (* It will have EstNodeKindModTok. *) 
              => BtmTeTfsModTok ( TEstRef ) 

              (* Lex error characters *) 
              | SharedStrings . T ( TString ) 
              => BtmTePosForTok 
                  ( SharedStrings . Tok ( TString ) 
                  , TString 
                  , FsNodeRef 
                  , FsFmtKind 
                  ) 
              ; BtmTmEstRef := TString 
              ; BtmTmEstRefIsMarked := FALSE 
              ; BtmTmEstNodeNo 
                  := EstAbsNodeNo + BtmTeEstTravInfo . EtiChildRelNodeNo 
              ; BtmTmKind := MarkKindTyp . Plain 
              ; BtmTmFmtNo := FsNodeRef . FsFmtNo 
                (* ^Dead, but maybe useful in debugging. *) 
              ; BtmTeBuildTempMarksForTok ( BtmCharPos ) 
              ; BtmTeIncEstChild ( ) 

              (* Error. *) 
              | ModHs . ModErrTyp 
              => (*  Ignore error. *) BtmTeIncEstChild ( ) 

              ELSE (* Not a leading mod. *) 
                EXIT 
              END (* TYPECASE *) 
            ; IF BtmState = BtmStateTyp . BtmStateDoneWithLine 
              THEN 
                EXIT 
              END (* IF *) 
            END (* LOOP *) 
          END BtmTeTfsLeadingMods 

      ; PROCEDURE BtmTeTfsTrailingMods ( ) 
        RAISES { Backout , Thread . Alerted } 

        = BEGIN (* BtmTeTfsTrailingMods *) 
            LOOP 
              IF BtmTeEstTravInfo . EtiChildNo 
                 >= BtmTeEstTravInfo . EtiChildCt 
                 (* No more Est children at all. *) 
                 OR FsNodeRef . FsFmtNo # BtmTeEstTravInfo . EtiChildFmtNo 
                    (* Next child for a different FmtNo. *) 
              THEN 
                EXIT 
              ELSE (* Found a child for the current FmtNo. *) 
                TYPECASE BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 

                OF NULL 
                => (* Not a trailing mod. *) 
                   EXIT 
                | ModHs . ModCmntTrailingTyp ( TModCmnt ) 
                => BtmTeTfsModCmnt ( TModCmnt ) 

                ELSE (* Not a trailing mod. *) 
                  EXIT 
                END (* TYPECASE *) 
              ; IF BtmState = BtmStateTyp . BtmStateDoneWithLine 
                THEN 
                  EXIT 
                END (* IF *) 
              END (* IF *) 
            END (* LOOP *) 
          END BtmTeTfsTrailingMods 

      (* Format syntax trees *) 

      ; PROCEDURE BtmTeTfsInsTok ( ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* BtmTeTfsInsTok *) 
            BtmTeTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; CASE BtmState 

            OF BtmStateTyp . BtmStateStartAtBeg 
            , BtmStateTyp . BtmStateStartAtEnd 
            => (* The actual start point must be in a trailing mod. *) 
               Assert 
                 ( BtmLinesRef . LrBolTokMark . TkmKind 
                   IN Marks . MarkKindSetEstLeaf 
                 , AFT . A_BtmTeTfsInsTok_StartAtInsTok 
                 ) 
            ; BtmTeTfsTrailingMods ( ) 

            | BtmStateTyp . BtmStatePassingNl 
            , BtmStateTyp . BtmStateInLine 
            => IF LIsRepair  
               THEN 
               (* Don't put a TempMark on a proposed repair insertion,
                  because ParseTrv won't return it to the parser.
                  Move the mark back to the previous item on the line,
                  if one exists and it is not also a proposed repair
                  insertion.  Otherwise, postpone putting the temp
                  mark until the following item.  If the entire line
                  lacks anything a temp mark can be attached to, will
                  have to continue onto the next line for something. 
               *)
                 BtmTePosForTok 
                   ( FsNodeRef . FsTok 
                   , FsNodeRef . FsInsTokRef 
                   , FsNodeRef 
                   , FsFmtKind 
                   ) 
               ; IF BtmTmEstRef # NIL 
                 THEN (* There was a preceeding item on the line that this
                         temp mark can be attached to. 
                      *) 
                   BtmTeBuildTempMarksForTok ( BtmCharPos ) 
                 END (* IF *)  
               ELSIF NOT LDelete 
               THEN 
                 BtmTePosForTok 
                   ( FsNodeRef . FsTok 
                   , FsNodeRef . FsInsTokRef 
                   , FsNodeRef 
                   , FsFmtKind 
                   ) 
               ; BtmTeTfsPrepareFmtNoMark ( ) 
               ; BtmTeBuildTempMarksForTok ( BtmCharPos ) 
               END (* IF *) 
            ; IF BtmState # BtmStateTyp . BtmStateDoneWithLine 
              THEN 
                BtmTeTfsTrailingMods ( ) 
              END (* IF *) 

            | BtmStateTyp . BtmStateDoneWithLine 
            => 
            END (* CASE *) 
          END BtmTeTfsInsTok 

      ; PROCEDURE BtmTeTfsLineBreak ( FsKind : FsKindTyp ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* BtmTeTfsLineBreak *) 
            BtmTeTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; CASE BtmState 

            OF BtmStateTyp . BtmStateStartAtBeg 
            => BtmTeIsFirstLine := FALSE 
            ; BtmTeIndentPos := EstIndentPosN
            ; IF BtmLinesRef . LrBolTokMark . TkmKind 
                 IN Marks . MarkKindSetEstLeaf 
               THEN (* The start point is in the Est leaf.  We aren't
                       actually visiting this LineBreak, so leave state alone. 
                    *) 
               ELSE 
                 BtmState := BtmStateTyp . BtmStatePassingNl 
               END (* IF *) 

            | BtmStateTyp . BtmStateStartAtEnd 
            => (* The actual start point must be in a trailing mod. 
                  We aren't actually visiting this LineBreak, so leave 
                  state alone. 
               *) 

            | BtmStateTyp . BtmStateDoneWithLine 
            , BtmStateTyp . BtmStatePassingNl 
            => (* Just keep going. *) 

            | BtmStateTyp . BtmStateInLine 
            => IF NOT LDelete 
               THEN 
                 IF FsKind = FsKindTyp . FsKindLineBreakReqd 
                    OR  TravUtil . DoTakeLineBreak 
                          ( ParseInfo . PiLang 
                          , BtmCharPos 
                          , BtmModTextIsToLeftOnLine 
                          , BtmPrevTok 
                          , RootFsNodeRef 
                          , FsNodeRef  
                          , FsFmtKind 
                          , BtmTeIndentPos 
                          , BtmTeEstTravInfo
                          , (* IN OUT *) BtmLastFmtNoOnLine 
                          , (* IN OUT *) BtmEstListChildrenToPass  
                          ) 
                 THEN (* Take the line break. *)  
                   IF BtmCharPos > 0 
                   THEN (* There were Tokens on this line. It could have 
                           some trailing marks. The previous token will 
                           have left BtmTokBegCharPos set. *) 
                     IF BtmOrphanedMarksExistAtEOL ( )  
                     THEN 
                       BtmEnterLine 
                         ( BtmLinesRef . LrRightLink (* NARROW can't fail. *) )
                     ; BtmState := BtmStateTyp . BtmStatePassingNl  
                     ELSE 
                       BtmTeBuildTempMarksForTok 
                         ( LbeStd . LimitedCharNoInfinity ) 
                       (* ^Use leftover values of BtmTmEstRef, BtmTmEstNodeNo, 
                          BtmTmKind, and BtmTmFmtNo 
                       *) 
                     END (* IF *) 
                   END (* IF *) 
                 ; BtmState := BtmStateTyp . BtmStateDoneWithLine 
                 END (* IF *) 
               END (* IF *) 
            ; BtmTeIsFirstLine := FALSE 
            ; BtmTeIndentPos := EstIndentPosN
            END (* CASE *) 
          END BtmTeTfsLineBreak 

      ; PROCEDURE BtmTeTfsEstChild ( ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* BtmTeTfsEstChild *) 
            BtmTeTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; CASE BtmState 

            OF BtmStateTyp . BtmStateStartAtBeg 
            , BtmStateTyp . BtmStateStartAtEnd 
            => IF EstAbsNodeNo 
                  + EstUtil . EstNodeCt ( BtmTeEstTravInfo . EtiNodeRef ) 
                  + ORD ( BtmTeEstTravInfo . EtiIsOptSingletonList )  
                  <= BtmLinesRef . LrBolTokMark . TkmEstNodeNo 
               THEN (* Marked node is outside this Est subtree. *) 
                 BtmTeIncEstChild ( ) 
               ELSE 
                 Assert 
                   ( BtmTeEstTravInfo . EtiChildNo 
                     < BtmTeEstTravInfo . EtiChildCt 
                     AND BtmTeEstTravInfo . EtiChildFmtNo 
                         = FsNodeRef . FsFmtNo 
                   , AFT . A_BtmTeTfsEstChild_StartInsideMissingEstChild 
                   ) 
               ; IF EstHs . EstChildKindEstChild 
                    IN BtmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                 THEN
                   IF ISTYPE 
                        ( BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                        , ModHs . EstDummyTyp (* Including NIL *)  
                        ) 
                   THEN
                     BtmTeIncEstChild ( ) 
                   ELSIF EstHs . EstChildKindTrailingSep 
                         IN BtmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                   THEN (* Skip trailing sep. *) 
                     BtmTeIncEstChild ( ) 
                   ELSE 
                     BtmTeTfsEstSubtree ( ) 
                   END (* IF *) 
                 END (*IF *) 
               END (* IF *) 

            | BtmStateTyp . BtmStatePassingNl 
            , BtmStateTyp . BtmStateInLine 
            => TravUtil . AssertFwdNoLostFixedChild 
                 ( FsNodeRef , BtmTeEstTravInfo ) 
            ; IF BtmTeEstTravInfo . EtiChildNo 
                 < BtmTeEstTravInfo . EtiChildCt 
                 AND BtmTeEstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
                 AND EstHs . EstChildKindEstChild 
                     IN BtmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
              THEN 
                IF ISTYPE 
                     ( BtmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     , ModHs . EstDummyTyp (* Including NIL *)
                     ) 
                THEN (* NIL or dummy. Skip. *) 
                  BtmTeIncEstChild ( ) 
                ELSIF EstHs . EstChildKindTrailingSep 
                      IN BtmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                THEN (* Skip trailing sep. *) 
                  BtmTeIncEstChild ( ) 
                ELSE 
                  Assert 
                    ( NOT LDelete 
                    , AFT . A_BtmTeTfsEstChild_DeletedEstChild 
                    ) 
                ; BtmTeTfsEstSubtree ( ) 
                END (* IF *) 
              END (* IF *) 

            | BtmStateTyp . BtmStateDoneWithLine => 

            END (* CASE *) 
          ; IF BtmState # BtmStateTyp . BtmStateDoneWithLine 
            THEN 
              BtmTeTfsTrailingMods ( ) 
            END (* IF *) 
          END BtmTeTfsEstChild 

      ; PROCEDURE BtmTeTfsFsSubtree ( ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LChildFmtKind : FmtKindTyp 
        ; VAR LInitialChildNo : LangUtil . FsChildNoTyp 

        ; BEGIN (* BtmTeTfsFsSubtree *) 
            CASE BtmState <* NOWARN *> 
            OF BtmStateTyp . BtmStateStartAtBeg 
            , BtmStateTyp . BtmStateStartAtEnd 
            => LChildFmtKind 
                := TravUtil . FmtKindForFsSubtreeDescending
                     ( ParseInfo . PiLang 
                     , RootFsNodeRef 
                     , FsNodeRef 
                     , FsFmtKind 
                     , EstIndentPosN 
                     , BtmTeEstTravInfo 
                     , StartMark := BtmLineMarkMeat . LmTokMark 
                     , StartMarkIsKnownNl := TRUE 
                     )
            ; LInitialChildNo 
                := LangUtil . FsChildNoOfFmtNo ( FsNodeRef , BtmTeStartFmtNo ) 

            | BtmStateTyp . BtmStatePassingNl 
            , BtmStateTyp . BtmStateInLine 
            => LChildFmtKind 
                := TravUtil . FmtKindForFsSubtreeTraversing 
                     ( ParseInfo . PiLang 
                     , BtmCharPos 
                     , BtmModTextIsToLeftOnLine 
                     , BtmPrevTok 
                     , RootFsNodeRef 
                     , FsNodeRef 
                     , FsFmtKind 
                     , BtmTeIndentPos 
                     , BtmTeEstTravInfo 
                     , (* IN OUT *) BtmLastFmtNoOnLine 
                     , (* IN OUT *) BtmEstListChildrenToPass 
                     )
            ; LInitialChildNo := 0 
            END (* CASE *) 
          ; BtmTeTraverseFsFixedChildren 
              ( FsNodeRef , LChildFmtKind , LInitialChildNo ) 
          END BtmTeTfsFsSubtree

      ; PROCEDURE BtmTeTfsTraverseCondFmtChildren ( ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LFsChildNo : LangUtil . FsChildNoTyp 
        ; VAR LPredicate : BOOLEAN 

        ; BEGIN (* BtmTeTfsTraverseCondFmtChildren *) 
            CASE BtmState <* NOWARN *> 
            OF BtmStateTyp . BtmStateStartAtBeg 
            , BtmStateTyp . BtmStateStartAtEnd 
            => TravUtil . DescendCondFmt 
                 ( Lang := ParseInfo . PiLang 
                 , FsNodeRef := FsNodeRef 
                 , FmtNo := BtmTeStartFmtNo 
                 , EstTravInfo := BtmTeEstTravInfo 
                 , (* VAR *) Predicate := LPredicate 
                 , (* VAR *) FsChildNo := LFsChildNo 
                 ) 

            | BtmStateTyp . BtmStatePassingNl 
            , BtmStateTyp . BtmStateInLine 
            => LFsChildNo := 0 
            ; LPredicate 
                 := TravUtil . DoCondFmtFwd 
                      ( ParseInfo . PiLang , BtmTeEstTravInfo , FsNodeRef ) 
            END (* CASE *) 
          ; IF LPredicate 
            THEN 
              BtmTeTraverseFsFixedChildren 
                ( FsNodeRef , FsFmtKind , LFsChildNo ) 
            ELSE 
              BtmTeTraverseFs ( FsNodeRef . FsCondAltRef , FsFmtKind ) 
            END (* IF *) 
          END BtmTeTfsTraverseCondFmtChildren 

      ; BEGIN (* BtmTeTraverseFs *) 

          VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* Block for BtmTeTraverseFs *) 
            CASE FsNodeRef . FsKind <* NOWARN *>

            (* Beginning of image. *) 
            OF FsKindTyp . FsKindBegOfImage 
            => IF BtmState IN BtmStateSetStart 
                  AND BtmLinesRef . LrBolTokMark . TkmKind 
                      IN Marks . MarkKindSetEstLeaf 
               THEN (* The start point must be in a trailing mod. *) 
               ELSE 
                 Assert 
                   ( BtmState = BtmStateTyp . BtmStateStartAtBeg 
                   , AFT . A_BtmTeTraverseFs_BOIBadState 
                   ) 
               ; BtmTeIsFirstLine := TRUE 
               ; BtmTeIndentPos := EstIndentPos1
               ; BtmState := BtmStateTyp . BtmStatePassingNl 
               END (* IF *) 
            ; BtmTeTfsTrailingMods ( ) 

            (* End of image. *) 
            | FsKindTyp . FsKindEndOfImage 
            => BtmTeTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
            ; Assert
                ( NOT LDelete 
                , AFT . A_BtmTeTraverseFs_DeletedEndOfImage 
                ) 
            ; IF ParseInfo . PiRegularTempMarkCt = LbeStd . MarkNoNull 
              THEN 
                ParseInfo . PiRegularTempMarkCt := BtmTempMarkNextElemNo  
              END (* IF *) 
            ; BtmTeTfsPrepareFmtNoMark ( ) 
            ; BtmTeBuildTempMarksForTok ( LbeStd . LimitedCharNoInfinity ) 
            ; BtmState := BtmStateTyp . BtmStateDoneWithLine 

            (* InsTok. *) 
            | FsKindTyp . FsKindInsTok 
            => BtmTeTfsInsTok ( ) 

            (* Line breaks. *) 
            | FsKindTyp . FsKindLineBreakOpt 
            , FsKindTyp . FsKindLineBreakReqd  
            => BtmTeTfsLineBreak ( FsNodeRef . FsKind ) 

            (* Est child cases. *) 
            | FsKindTyp . FsKindEstChildOfFixed 
            => BtmTeTfsEstChild ( ) 
 
            | FsKindTyp . FsKindEstChildOfList 
            => BtmTeTfsEstChild ( ) 
            ; IF BtmState # BtmStateTyp . BtmStateDoneWithLine 
              THEN 
                TravUtil . PassEstListChild ( BtmEstListChildrenToPass ) 
              END (* IF *) 

           (* Subtree nodes. *) 
            | FsKindTyp . FsKindSubtreeVert 
            , FsKindTyp . FsKindSubtreeHoriz 
            , FsKindTyp . FsKindSubtreeFill 
            => BtmTeTfsFsSubtree ( ) 

            (* Conditional format. *) 
            | FsKindTyp . FsKindCondFmt 
            => BtmTeTfsTraverseCondFmtChildren ( ) 
            END (* CASE FsKind *) 
          END (* Block  BtmTeTraverseFs local block *) 
        END BtmTeTraverseFs 

    ; BEGIN (* BtmTraverseEst *) 
        VAR LInitialFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* Block for BtmTraverseEst *) 
          IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        ; MarkFound := FALSE 
        ; IF EstRef # NIL 
          THEN 

          (* Choose the correct Est child. *) 
            BtmTeRMChildRef := NIL 
          ; BtmTeRMChildRelNodeNo := LbeStd . EstNodeNoNull 
          ; CASE BtmState 
            OF BtmStateTyp . BtmStateStartAtBeg 
            , BtmStateTyp . BtmStateStartAtEnd 
            => IF EstAbsNodeNo = BtmLineMarkMeat . LmTokMark . TkmEstNodeNo 
               THEN (* The mark denotes the Est root. *) 
                 BtmTeInitEstTravInfoFwd ( EstRef , KindSet , EstAbsNodeNo )
               ; CASE BtmLineMarkMeat . LmTokMark . TkmKind <* NOWARN *> 
                 OF MarkKindTyp . Plain 
                 , MarkKindTyp . BlankLine
                 => (* The mark denotes the Est root and it is a leaf. 
                       No child to choose.
                    *)  
                   Assert 
                     ( BtmTeEstTravInfo . EtiParentRef = NIL 
                     , AFT . A_BtmTraverseEst_Not_leaf
                     ) 

                 | MarkKindTyp . ChildFmtNo 
                 => Assert 
                      ( BtmTeEstTravInfo . EtiChildCt = 0 
                      , AFT . A_BtmTraverseEst_ChildFmtNo_has_children 
                      ) 

                 | MarkKindTyp . LeftSibFmtNo 
                 , MarkKindTyp . RightSibFmtNo 
                 => CantHappen ( AFT . A__SibFmtNo_on_parent ) 
                 END (* CASE *) 
         
               ELSE 
                 BtmTeInitToChildContainingNodeNo 
                   ( EstRef 
                   , BtmLineMarkMeat . LmTokMark . TkmEstNodeNo - EstAbsNodeNo 
                   , KindSet 
                   , EstAbsNodeNo  
                   ) 
               ; Assert
                   ( BtmTeEstTravInfo . EtiChildCt > 0
                   , AFT . AF_BtmTraverseEst_descend_thru_empty_node 
                   ) 
               ; IF BtmLineMarkMeat . LmTokMark . TkmKind 
                    = MarkKindTyp . RightSibFmtNo 
                    AND EstAbsNodeNo + BtmTeEstTravInfo . EtiChildRelNodeNo 
                        = BtmLineMarkMeat . LmTokMark . TkmEstNodeNo 
                 THEN 
                   Assert
                     ( BtmTeEstTravInfo . EtiChildNo + 1 
                       = BtmTeEstTravInfo . EtiChildCt 
                     , AFT . A_BtmTraverseEst_RightSib_not_rightmost
                     ) 
                 ; BtmTeIncEstChild ( ) 
                 END (* IF *) 
               END (* IF *) 
            ; BtmTeInitNodeToPatch ( )

            (* Choose the correct FmtNo. *) 
            ; BtmTeStartFmtNo := BtmTeEstTravInfo . EtiChildFmtNo (* Default. *)
            ; CASE BtmLineMarkMeat . LmTokMark . TkmKind 
              OF MarkKindTyp . ChildFmtNo 
              => IF EstAbsNodeNo = BtmLineMarkMeat . LmTokMark . TkmEstNodeNo 
                 THEN 
                   BtmTeStartFmtNo := BtmLineMarkMeat . LmTokMark . TkmFmtNo 
                 END (* IF *) 

              | MarkKindTyp . LeftSibFmtNo 
              => IF EstAbsNodeNo + BtmTeEstTravInfo . EtiChildRelNodeNo 
                    = BtmLineMarkMeat . LmTokMark . TkmEstNodeNo 
                 THEN 
                   BtmTeStartFmtNo := BtmLineMarkMeat . LmTokMark . TkmFmtNo 
                 END (* IF *) 

              | MarkKindTyp . RightSibFmtNo 
              => IF BtmTeEstTravInfo . EtiChildNo 
                    >= BtmTeEstTravInfo . EtiChildCt 
                    AND EstAbsNodeNo + BtmTeRMChildRelNodeNo 
                        = BtmLineMarkMeat . LmTokMark . TkmEstNodeNo 
                 THEN 
                   BtmTeStartFmtNo := BtmLineMarkMeat . LmTokMark . TkmFmtNo 
                 END (* IF *) 
              ELSE 
              END (* CASE *) 
            ; LInitialFsChildNo 
                := LangUtil . FsChildNoOfFmtNo 
                     ( RootFsNodeRef , BtmTeStartFmtNo ) 

            | BtmStateTyp . BtmStatePassingNl 
            , BtmStateTyp . BtmStateInLine 
            => BtmTeInitEstTravInfoFwd ( EstRef , KindSet , EstAbsNodeNo )
            ; BtmTeStartFmtNo := 0  
            ; LInitialFsChildNo := 0 
            ; BtmTeIsFirstLine := TRUE 
            ; BtmTeIndentPos := EstIndentPos1

            ELSE 
              CantHappen ( AFT . A_BtmTraverseEst_BadState ) 
            END (* CASE *) 
          ; BtmTeEstChildIsMarked := FALSE 
          ; CASE RootFsNodeRef . FsKind <* NOWARN *> 

            (* Est fixed nodes. *) 
            OF FsKindTyp . FsKindEstFixedVert 
            , FsKindTyp . FsKindEstFixedHoriz 
            , FsKindTyp . FsKindEstFixedFill 
            => BtmTeTraverseFsFixedChildren 
                 ( RootFsNodeRef 
                 , EstFmtKind 
                 , LInitialFsChildNo 
                 ) 

            (* Est list nodes. *) 
            | FsKindTyp . FsKindEstListVert 
            , FsKindTyp . FsKindEstListHoriz 
            , FsKindTyp . FsKindEstListFill 
            , FsKindTyp . FsKindEstListTrailVert 
            , FsKindTyp . FsKindEstListTrailHoriz 
            , FsKindTyp . FsKindEstListTrailFill 
            => BtmTeTraverseFsListChildren 
                 ( RootFsNodeRef 
                 , EstFmtKind 
                 , LInitialFsChildNo 
                 ) 

            (* Ast string, *) 
            | FsKindTyp . FsKindAstString 
            => BtmTeAstString ( RootFsNodeRef , EstFmtKind ) 

         (* ELSE Cant happen. *) 
            END (* CASE *) 
          END (* IF *) 
        ; IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        END (* Block *) 
      END BtmTraverseEst 

  ; BEGIN (* BuildTempMarkList *) 
      VAR LMarkFound : BOOLEAN 

    ; BEGIN (* Block  BuildTempMarkList *) 
        ParseInfo . PiOrigTempMarkListRef := NIL 
      ; ParseInfo . PiRegularTempMarkCt := LbeStd . MarkNoNull 
      ; IF ImageRef # NIL 
        THEN 
          BtmTempMarkCt := ImageRef . ItPers . IpMarkCt 
        ; IF BtmTempMarkCt > 0 
          THEN 
            ParseInfo . PiOrigTempMarkListRef 
              := NEW ( ParseHs . TempMarkArrayRefTyp , BtmTempMarkCt ) 
          ; IF BtmTempMarkCt > 0
            THEN ParseInfo . PiOrigTempMarkListRef ^ [ 0 ] . SeqNo := 0
            END (* IF *)
          ; FOR RI := 0 TO NUMBER ( ParseInfo . PiOrigTempMarkListRef ^ ) - 1 
            DO 
              WITH WTempMark = ParseInfo . PiOrigTempMarkListRef ^ [ RI ] 
              DO WTempMark . TokMark . TkmEstRef := NIL  
              ; WTempMark . TokMark . TkmKind := MarkKindTyp . Null 
              ; WTempMark . TokMark . TkmFmtNo := EstHs . FmtNoNull 
              END 
            END 
          ; BtmTempMarkNextElemNo := 0 
          ; BtmLineMarkMeat := ImageRef . ItPers . IpMarkHeader . LmRightLink  
          ; LOOP (* One iteration of this loop fills in a group of marks 
                    that are built from the same line mark.  Sometimes, it
                    traverses into the next line too, if marks based on
                    the line mark have to be turned into TempMarks on things
                    in the succeeding line. *) 
              Display . SecureSucc 
                ( ImageRef , BtmLineMarkMeat . LmLinesRef )
            ; Assert 
                ( Marks . Equal 
                    ( BtmLineMarkMeat . LmLinesRef . LrBolTokMark 
                    , BtmLineMarkMeat . LmTokMark 
                    ) 
                , AFT . A_BuildTempMarkList_Line_mark_and_LinesRef_unequal_marks 
                )  
            ; BtmEnterLine ( BtmLineMarkMeat . LmLinesRef ) 
            ; IF BtmLineMarkMeat . LmTokMark . TkmStartAtEnd 
              THEN 
                BtmState := BtmStateTyp . BtmStateStartAtEnd 
              ELSE 
                BtmState := BtmStateTyp . BtmStateStartAtBeg 
              END (* IF *) 
            ; BtmTraverseEst 
                ( EstRef := ImageRef . ItPers . IpEstRoot 
                , KindSet := EstHs . EstChildKindSetEmpty  
                , EstChildNo := 0 
                , GrandparentEstRef := NIL 
                , EstAbsNodeNo := 0 
                , RootFsNodeRef 
                    := EstUtil . FsRuleForEstNode  
                         ( ParseInfo . PiLang 
                         , ImageRef . ItPers . IpEstRoot 
                         ) 
                , EstFmtKind := FmtKindVert 
                , EstIndentPos1 := Options . InitialIndentPos 
                , EstIndentPosN := Options . InitialIndentPos 
                , (* VAR *) MarkFound := LMarkFound 
                ) 
            ; Assert ( LMarkFound , AFT . A_BuildTempMarkList_MarkNotFound ) 
            ; Assert 
                ( BtmState = BtmStateTyp . BtmStateDoneWithLine 
                , AFT . A_BuildTempMarkList_MarkNotDoneWithLine 
                ) 
            ; IF BtmTempMarkNextElemNo >= BtmTempMarkCt 
              THEN 
                EXIT 
              END (* IF *) 
            END (* LOOP through line marks. *) 
          ; IF ParseInfo . PiRegularTempMarkCt = LbeStd . MarkNoNull 
            THEN 
              ParseInfo . PiRegularTempMarkCt := BtmTempMarkCt 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* Block  BuildTempMarkList body block *) 
    END BuildTempMarkList 

(*********************************************************) 

; TYPE ProcTyp = PROCEDURE ( ) RAISES { Backout }  

; PROCEDURE ProcNoop ( ) = BEGIN (* ProcNoop *) END ProcNoop

; VAR GDoUnmark : BOOLEAN := TRUE 

(* VISIBLE *) 
; PROCEDURE RebuildMarkList 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; ImageRef : PaintHs . ImageTransientTyp 
    ; <* UNUSED *> OldEstRef : LbeStd . EstRootTyp 
    ; NewEstRef : LbeStd . EstRootTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

  (* Rebuild line marks from temporary marks after reparse. *) 

  (* This is a reversing, recursive traversal.  It begins in state
     RbmStateDescend, where it moves downward in the tree, directly to
     the next marked node, which may have a temp mark attached, or may
     just be on the path to a descendent that contains the next temp
     mark.  When it reaches an Est leaf, it goes into RbmStateBwdNl,
     where it moves backward, traversing every node, until it hits a
     known new line.  Then it constructs a TokMark
     (RbmCurrentLineTokMark) to the beginning of line and switches to
     RbmStateFwdNl, where it traverses forward through every node on
     the line, converting any temp marks to their normal,
     beginning-of-line-relative form, using the newly built
     RbmCurrentLineTokMark to the beginning of the line.
 
     When it reaches the end of the line, it goes back to
     RbmStateDescend, where it first returns to RbmTraverseEst, then
     skips to the next marked node again, or returns more levels.

     Explicit Nl before, the implicit Nl after of a blank line mod,
     and a line break when ApproxFmtKind is known vertical are the
     only forms of new line where we can reverse (backward to forward)
     and be sure that FmtKind can be computed for all nodes on the
     path to the TokMark.  Other possible new lines are passed over
     when going backwards.  Later, when going forwards, when one of these
     passed-over new lines occurs, traversing continues into the next
     line, with a new TokMark constructed.  This could waste some
     traversal time, or it could get lucky and pick up some marks on
     the next line.
 
     When it encounters a subsequent new line at the same line
     boundary, it just constructs a new TokMark which overlays the
     previously built one.  This way, it prefers the rightmost new
     line of a line boundary.
  *) 

  = TYPE RbmStateTyp 
      = { RbmStateDescend      (* Searching down/forward for marked node *) 
        , RbmStateBwdNl        (* Scanning backwards for rightmost 
                                  new line to left of a mark *) 
        , RbmStateFwdNl        (* Traversing forwards thru a line, 
                                  converting all marks thereon. *) 
        } 
  ; CONST RbmStateDescend = RbmStateTyp . RbmStateDescend
  ; CONST RbmStateBwdNl = RbmStateTyp . RbmStateBwdNl
  ; CONST RbmStateFwdNl = RbmStateTyp . RbmStateFwdNl

  ; TYPE RbmStateSetTyp = SET OF RbmStateTyp 

  ; VAR RbmImagePers : PaintHs . ImagePersistentTyp 
  ; VAR RbmState : RbmStateTyp 
  ; VAR RbmTempMarkElemCt : LbeStd . MarkNoTyp 
  ; VAR RbmTempMarkElemNo : LbeStd . MarkNoTyp 
  ; VAR RbmDescendTempMarkElemNo : LbeStd . MarkNoTyp 
  ; VAR RbmCharPos : LbeStd . LimitedCharNoSignedTyp 
  ; VAR RbmTokBegPos : LbeStd . LimitedCharNoSignedTyp 
  ; VAR RbmPrevTokToPos : LbeStd . LimitedCharNoSignedTyp 
  ; VAR RbmPrevTok : LbeStd . TokTyp 
  ; VAR RbmLineBreakCt : CARDINAL 
        (* Count them upward in RbmStateBwdNl, down in RbmStateFwdNl. *) 
  ; VAR RbmMaxLineBreakCt : CARDINAL (* Just for statistics. *) 
  ; VAR RbmCurrentLineTokMark : Marks . TokMarkTyp 
  ; VAR RbmEstRefForTrailingMarks : LbeStd . EstRootTyp 
        (* Non-NIL means a trailing mark was found.  *) 
  ; VAR RbmLineMarkMeat : PaintHs . LineMarkMeatTyp 
  ; VAR RbmLastFmtNoOnLine : EstHs . FmtNoTyp 
  ; VAR RbmEstListChildrenToPass : LbeStd . EstChildNoTyp 

  (* Statistics: *) 
  ; VAR RbmEstDescendFmtKindCt : CARDINAL 
  ; VAR RbmEstUpFmtKindCt : CARDINAL 
  ; VAR RbmEstFwdFmtKindCt : CARDINAL 
  ; VAR RbmFsDescendFmtKindCt : CARDINAL 
  ; VAR RbmFsUpFmtKindCt : CARDINAL 
  ; VAR RbmFsFwdFmtKindCt : CARDINAL 

  ; PROCEDURE RbmReenterStateDescend ( ) RAISES { Backout }  

    = BEGIN 
        IF RbmTempMarkElemNo <= RbmDescendTempMarkElemNo
        THEN (* No progress in converting temp marks was made during this
                cycle of states.  This should not happen.  It would cause
                looping. 
             *) 
          Assertions . MessageText 
            ( "No TempMarks rebuilt during a state cycle." ) 
        ; CantHappen ( AFT . A_RbmReenterStateDescend_NoProgress ) 
        ELSE 
          RbmState := RbmStateDescend 
        END (* IF *) 
      END RbmReenterStateDescend 

  ; PROCEDURE RbmIncTempMark 
      ( CurrentMarkEstRef : LbeStd . EstRootTyp ) 
    : BOOLEAN (* Finished with all temp marks in token. *) 
    RAISES { Backout } 

    = BEGIN (* RbmIncTempMark *) 
        INC ( RbmTempMarkElemNo ) 
      ; IF RbmTempMarkElemNo >= RbmTempMarkElemCt 
        THEN 
          Assert 
            ( ISTYPE 
                ( RbmLineMarkMeat . LmRightLink , PaintHs . LineMarkHeaderTyp ) 
            , AFT . A_RbmIncTempMark_ExtraMarks 
            ) 
        ; RETURN TRUE 
        ELSE 
          RbmLineMarkMeat := RbmLineMarkMeat . LmRightLink 
        ; WITH 
            WNewTempMark 
            = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo ] 
          , WOldTempMark 
            = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo - 1 ] 
          DO IF WNewTempMark . TokMark . TkmKind # WOldTempMark . TokMark . TkmKind
            THEN RETURN TRUE 
            ELSIF WNewTempMark . TokMark . TkmFmtNo # WOldTempMark . TokMark . TkmFmtNo 
            THEN RETURN TRUE 
            ELSIF WNewTempMark . TokMark . TkmEstRef # CurrentMarkEstRef 
(* CHECK: Is there any remaining reason to use CurrentMarkEstRef, or can we
          just compare to WOldTempMark.EstRef? 
*) 
            THEN RETURN TRUE 
            ELSIF WNewTempMark . TokMark . TkmEstNodeNo 
                  # WOldTempMark . TokMark . TkmEstNodeNo  
                  (* ^Since subtrees and strings can be shared, it is possible 
                     that the same node appears in two places in a tree,
                     getting us to here with them incorrectly looking the same.
                     The EstNodeNo fields are in the tree before parsing, and
                     are not adjusted for the changes due to reparsing, but 
                     they will still be equal iff the two nodes are at the same
                     place in the reparsed tree. 
                  *)
            THEN RETURN TRUE 
            ELSE RETURN FALSE
            END (* IF *) 
          END (* WITH *) 
        END (* IF *) 
      END RbmIncTempMark 

  (* Recursive traversal of Ests. *) 

  ; PROCEDURE RbmTraverseEst 
      ( EstRef : LbeStd . EstRootTyp  
      ; KindSet : EstHs . EstChildKindSetTyp 
      ; EstChildNo : LbeStd . EstChildNoTyp 
        (* ^EstRef has this KindSet and ChildNo in GrandparentRef. *) 
      ; GrandparentEstRef : EstHs . EstRefTyp 
      ; EstAbsNodeNo : LbeStd . EstNodeNoTyp 
      ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR (* IN OUT *)  EstApproxFmtKind : FmtKindTyp 
        (* ^EstApproxFmtKind could be FmtKindUnknown if the subtree has finite 
            width (which => no interior explicit Nl's), no explicit 
            NlBefore, and we entered this tree in a state other than 
            RbmStateFwdNl (which => CharPos = LbeStd . LimitedCharNoUnknown). 
            We only reverse from RbmStateBwdNl to RbmStateFwdNl 
            at an explicit NlBefore (which means the enclosing Est 
            has either infinite width or WiHasNlBefore) or at a line 
            break when EstApproxFmtKind = LbeStd . FmtKindVert. 
            Furthermore, we only construct a new TokMark at the point 
            of reversal, or while in RbmStateFwdNl.  Thus, when 
            we construct a TokMark, EstApproxFmtKind # LbeStd . FmtKindUnknown. 
            Furthermore, once EstApproxFmtKind = LbeStd . FmtKindUnknown, 
            it cannot become known in a subtree.  So we have known 
            FmtKind for all nodes of TokMarks built. *) 
      ; EstRecomputeFmtKindProc : ProcTyp 
        (* ^A procedure to compute FmtKind for this Est, helpful whenever we
           discover Either EstApproxFmtKind = FmtKindVert, or we are in
           RbmStateFwdNl. *) 
      ; EstIndentPos1 : LbeStd . LimitedCharNoTyp 
      ; EstIndentPosN : LbeStd . LimitedCharNoTyp 
        (* ^EstIndentPos1 and EstIndentPosN are maintained assuming
           the Est subtree is formatted vertically. *) 
      ) 
    RAISES { Backout , Thread . Alerted } 

    = VAR RbmTeRMChildRef : LbeStd . EstRootTyp 
    ; VAR RbmTeRMChildRelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR RbmTeRMChildKindSet : EstHs . EstChildKindSetTyp 
      (* ^RbmTeRMChildRef, RbmTeRMChildRelNodeNo, and RbmTeRMChildKindSet 
          trail behind the current Est child.  They are guaranteed to be set
          only when we are off the right end.
      *) 
    ; VAR RbmTeFmtNo : EstHs . FmtNoTyp 
    ; VAR RbmTeEstTravInfo : TravUtil . EstTravInfoTyp 
    ; VAR RbmTeEstRefToPatch : EstHs . EstRefTyp 
    ; VAR RbmTeCurrentChildNoToPatch : LbeStd . EstChildNoTyp 
    ; VAR RbmTeRMChildNoToPatch : LbeStd . EstChildNoTyp 
      (* If we are a traversing a constucted EstTravInfo for a singleton-
         optimized list (which does not exist in the Est,) RbmTeEstRefToPatch,
         RbmTeCurrentChildNoToPatch, and RbmTeRMChildNoToPatch denote the  
         parent and child number of the list, which is actually the one
         list element.  Otherwise, they just duplicate 
         RbmTeEstTravInfo . EtiParentRef, RbmTeEstTravInfo . EtiChildNo, 
         and EstUtil . EstChildCt ( EstRef ) - 1.  Either way, they are what 
         are needed to make changes in the Est, from above, to the current or
         rightmost child pointer and to its KindSet.
      *)  
    ; VAR RbmTeRMFsChildNo : LangUtil . FsChildNoSignedTyp 
          (* ^Only used when we have an FsList rule. *) 
    ; VAR RbmTeIndentPos : LbeStd . LimitedCharNoTyp 
    ; VAR RbmTeIsFirstLine : BOOLEAN := TRUE  
    (* RbmTeIsFirstLine and RbmTeIndentPos are maintained as we go, only in
       RbmStateFwdNl, but can be computed at any time by RbmTeTfsSetIndentInfo.
       They are maintained as if the Est subtree is formatted vertically.
    *) 
    ; VAR RbmTeDoneWEst : BOOLEAN 

    ; PROCEDURE RbmTeInitNodeToPatch ( )

      = BEGIN 
          IF RbmTeEstTravInfo . EtiIsOptSingletonList 
          THEN
            RbmTeEstRefToPatch := GrandparentEstRef  
          ; RbmTeCurrentChildNoToPatch := EstChildNo 
          ; RbmTeRMChildNoToPatch 
              := EstUtil . EstChildCt ( GrandparentEstRef ) - 1  
          ELSE
            RbmTeEstRefToPatch := RbmTeEstTravInfo . EtiParentRef 
          ; RbmTeCurrentChildNoToPatch := RbmTeEstTravInfo . EtiChildNo  
          ; RbmTeRMChildNoToPatch := RbmTeEstTravInfo . EtiChildCt - 1 
          END (* IF *) 
        END RbmTeInitNodeToPatch 

    ; PROCEDURE RbmTeSetToNextTempMarkedChild ( )
      RAISES { Backout } 

      = BEGIN 
          TravUtil . SetToNextInKindSet 
            ( (* IN OUT *) RbmTeEstTravInfo 
            , RbmTeEstTravInfo . EtiChildNo 
            , EstHs . EstChildKindSetContainsTempMark 
            ) 
        ; IF NOT RbmTeEstTravInfo . EtiIsOptSingletonList 
          THEN 
            RbmTeCurrentChildNoToPatch := RbmTeEstTravInfo . EtiChildNo
          END (* IF *) 
        END RbmTeSetToNextTempMarkedChild 

    ; PROCEDURE RbmTeInitEstTravInfoFwd 
        ( EstNodeRef : LbeStd . EstRootTyp 
        ; KindSet : EstHs . EstChildKindSetTyp 
        ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
        ) 
      RAISES { Backout } 

      = BEGIN 
          TravUtil . InitEstTravInfoFwd 
            ( RbmTeEstTravInfo , EstNodeRef , KindSet , ParentAbsNodeNo ) 
        ; RbmTeInitNodeToPatch ( ) 
        END RbmTeInitEstTravInfoFwd 

    ; PROCEDURE RbmTeInitEstTravInfoBwd 
        ( EstNodeRef : LbeStd . EstRootTyp 
        ; KindSet : EstHs . EstChildKindSetTyp 
        ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
        ) 
      RAISES { Backout } 

      = BEGIN 
          TravUtil . InitEstTravInfoBwd 
            ( RbmTeEstTravInfo , EstNodeRef , KindSet , ParentAbsNodeNo ) 
        ; RbmTeInitNodeToPatch ( ) 
        END RbmTeInitEstTravInfoBwd 

    ; PROCEDURE RbmTeIncEstChild ( ) 
      RAISES { Backout } 

      = BEGIN (* RbmTeIncEstChild *) 
          RbmTeRMChildRef := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
        ; RbmTeRMChildRelNodeNo := RbmTeEstTravInfo . EtiChildRelNodeNo 
        ; RbmTeRMChildKindSet 
            := RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
        ; TravUtil . IncEstChild ( RbmTeEstTravInfo ) 
        ; IF NOT RbmTeEstTravInfo . EtiIsOptSingletonList 
          THEN 
            RbmTeCurrentChildNoToPatch := RbmTeEstTravInfo . EtiChildNo 
          END (* IF *) 
        END RbmTeIncEstChild 

    ; PROCEDURE RbmTeDecEstChild ( ) 
      RAISES { Backout } 

      = BEGIN (* RbmTeDecEstChild *) 
          TravUtil . DecEstChild ( RbmTeEstTravInfo ) 
        ; IF NOT RbmTeEstTravInfo . EtiIsOptSingletonList 
          THEN 
            RbmTeCurrentChildNoToPatch := RbmTeEstTravInfo . EtiChildNo 
          END (* IF *) 
        END RbmTeDecEstChild

    ; PROCEDURE RbmTeUnmarkCurrentChild ( ) 

      = VAR LWasAlreadyUnmarked : BOOLEAN 

      ; BEGIN (* RbmTeUnmarkCurrentChild *) 
          WITH WKindSet = RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
          DO 
            IF EstHs . EstChildKindContainsTempMark IN WKindSet 
            THEN 
              EstUtil . SetChildKindBitFALSE 
                ( RbmTeEstRefToPatch 
                , RbmTeCurrentChildNoToPatch 
                , EstHs . EstChildKindContainsTempMark 
                , LWasAlreadyUnmarked (* Dead. *) 
                ) 
            ; WKindSet := WKindSet - EstHs . EstChildKindSetContainsTempMark 
            (* Update the cached copy too.  This may not matter, but as
               complicated as everything is here, it is good defensive
               consistency. *) 
            END (* IF *) 
          END (* WITH *) 
        END RbmTeUnmarkCurrentChild 

    ; PROCEDURE RbmTeSqueezeTrailingTempMarks 
        ( TokBegPos : LbeStd . LimitedCharNoTyp 
        ; ToPos : LbeStd . LimitedCharNoTyp 
        ) 
      RAISES { Backout } 
      (* POST: Finished with all temp marks in current token. *) 

      = BEGIN (* RbmTeSqueezeTrailingTempMarks *) 
          IF RbmEstRefForTrailingMarks # NIL  
          THEN (* A trailing mark was found. *) 
            LOOP 
              WITH 
                WTempMark 
                = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo ] 
              DO Assert 
                   ( WTempMark . TokMark . TkmKind # MarkKindTyp . BlankLine 
                   , AFT . A_RbmTeSqueezeTrailingTempMarks_BlankLine 
                   ) 
              ; RbmLineMarkMeat . LmCharPos 
                  := MIN ( TokBegPos + WTempMark . CharPos , ToPos ) 
              ; RbmLineMarkMeat . LmLineNo := 0 
              ; RbmLineMarkMeat . LmTokMark := RbmCurrentLineTokMark 
              ; RbmLineMarkMeat . LmLinesRef . LrHasMark := FALSE 
              ; RbmLineMarkMeat . LmLinesRef := NIL 
              END (* WITH WTempMark *) 
            ; IF RbmIncTempMark ( RbmEstRefForTrailingMarks ) 
              THEN EXIT 
              END (* IF *) 
            END (* LOOP *) 
          ; RbmEstRefForTrailingMarks := NIL 
          END (* IF *) 
        END RbmTeSqueezeTrailingTempMarks 

    ; PROCEDURE RbmTeBuildSomeMarksForTok 
        ( CurrentEstRef : LbeStd . EstRootTyp ) 
      RAISES { Backout } 

      (* RbmTeBuildSomeMarksForTok assumes caller has already 
         verified that the next mark belongs in the current token, 
         and thus that at least one mark will be built. *) 
      (* Call this when RbmTokBegPos is set for the token in question, 
         and RbmCharPos is the To-position of the token. *)

      = VAR LPos : LbeStd . LimitedCharNoSignedTyp 

      ; BEGIN (* RbmTeBuildSomeMarksForTok *) 
          RbmEstRefForTrailingMarks := NIL 
          (* Convert all temp marks in this token. *) 
        ; IF RbmTempMarkElemNo < RbmTempMarkElemCt 
          THEN 
            LOOP 
              WITH 
                WTempMark 
                = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo ] 
              DO Assert 
                   ( WTempMark . TokMark . TkmKind # MarkKindTyp . BlankLine 
                   , AFT . A_RbmTeBuildSomeMarksForTok_BlankLine 
                   ) 
              ; LPos := EstUtil . WidthSumSigned 
                  ( RbmTokBegPos , WTempMark . CharPos ) 
              ; IF LPos >= RbmCharPos 
                THEN (* This temp mark trails the token itself. *) 
                  RbmEstRefForTrailingMarks := CurrentEstRef 
                ; EXIT 
                ELSE 
                  RbmLineMarkMeat . LmCharPos 
                    := MIN 
                         ( MAX ( LPos , RbmPrevTokToPos ) 
                         , LbeStd . LimitedCharNoMax 
                         ) 
                ; RbmLineMarkMeat . LmLineNo := 0 
                ; RbmLineMarkMeat . LmTokMark := RbmCurrentLineTokMark 
                ; RbmLineMarkMeat . LmLinesRef . LrHasMark := FALSE 
                ; RbmLineMarkMeat . LmLinesRef := NIL 
                END (* IF *) 
              END (* WITH WTempMark *) 
            ; IF RbmIncTempMark ( CurrentEstRef ) THEN EXIT END (* IF *) 
            END (* LOOP *) 
          END (* IF *) 
        END RbmTeBuildSomeMarksForTok 

    ; PROCEDURE RbmTeBuildAnyMarksForPlainTok 
        ( CurrentEstRef : LbeStd . EstRootTyp ) 
      RAISES { Backout } 

      (* RbmTeBuildAnyMarksForPlainTok does not assume 
         that the next mark belongs in the current token, and thus 
         will build zero or more marks. *) 
      (* Call this when RbmTokBegPos is set for the token in question, 
         and RbmCharPos is the To position of the token. *) 

      = BEGIN (* RbmTeBuildAnyMarksForPlainTok *) 
        (* Check whether next temp mark is attached to this token. *) 
          IF RbmTempMarkElemNo < RbmTempMarkElemCt 
          THEN 
            WITH 
              WTempMark 
              = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo ] 
            DO 
               IF CurrentEstRef = WTempMark . TokMark . TkmEstRef  
               THEN 
                 IF WTempMark . TokMark . TkmKind = MarkKindTyp . Plain 
                 THEN 
                   RbmTeBuildSomeMarksForTok ( CurrentEstRef ) 
                 END (* IF *) 
               END (* IF *) 
            END (* WITH WTempMark *) 
          END (* IF *) 
        END RbmTeBuildAnyMarksForPlainTok 

    ; PROCEDURE RbmTeLeafString 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; ApproxFmtKind : FmtKindTyp 
        ; String : SharedStrings . T 
        ) 
      RAISES { Backout } 

      = BEGIN (* RbmTeLeafString *) 
          IF RbmTempMarkElemNo < RbmTempMarkElemCt 
          THEN 
            CASE RbmState 
            OF RbmStateDescend 
            => 
               Assert 
                 ( String 
                   = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo ] 
                     . TokMark . TkmEstRef 
                 , AFT . A_RbmTeLeafString_DescendNoTempMark 
                 ) 
            ; RbmState := RbmStateBwdNl 
            ; RbmLineBreakCt := 0 

            | RbmStateBwdNl 
            => (* just do nothing. *) 

            | RbmStateFwdNl 
            => IF String # NIL 
               THEN 
                 WITH WTok = SharedStrings . Tok ( String ) 
                 DO RbmCharPos 
                      := TravUtil . PosForTok 
                           ( ParseInfo . PiLang 
                           , ApproxFmtKind 
                           , (* ModTextIsToLeftOnLine := *) FALSE 
                           , RbmCharPos 
                           , RbmTeIndentPos 
                           , FsNodeRef . FsIndentCode 
                           , RbmPrevTok 
                           , WTok 
                           ) 
                 ; RbmTeSqueezeTrailingTempMarks ( RbmTokBegPos , RbmCharPos ) 
                 ; RbmTokBegPos := RbmCharPos 
                 ; RbmCharPos 
                     := EstUtil . WidthSumSigned  
                          ( RbmCharPos , SharedStrings . Length ( String ) ) 
                 ; RbmTeBuildAnyMarksForPlainTok ( String ) 
                 ; RbmPrevTok := WTok 
                 ; RbmPrevTokToPos := RbmCharPos 
                 END (* WITH *) 
               END (* IF *) 
            END (* CASE *) 
          END (* IF *) 
        END RbmTeLeafString 

    (* Recursive traversal of format syntax trees. *) 

    ; PROCEDURE RbmTeTraverseFsFixedChildren 
        ( ParentFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR (* IN OUT *) ApproxFmtKind : FmtKindTyp 
        ; RecomputeFmtKindProc : ProcTyp 
          (* ^A procedure to compute FmtKind for this list of Fs children, 
             helpful whenever we discover Either ApproxFmtKind = FmtKindVert, 
             or we are in RbmStateFwdNl. *) 
        ; FsChildCt : LangUtil . FsChildNoTyp 
        ; InitialFsChildNo : LangUtil . FsChildNoSignedTyp 
        ) 
      RAISES { Backout , Thread . Alerted } 

      = VAR LFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* RbmTeTraverseFsFixedChildren *) 
          IF FsChildCt > 0 
          THEN 
            LFsChildNo := InitialFsChildNo 
          ; Assert 
              ( 0 <= LFsChildNo AND LFsChildNo < FsChildCt 
              , AFT . A_RbmTeTraverseFsFixedChildren_InitialChildNumberOutOfRange  
              ) 
          ; LOOP 
              RbmTeTraverseFs 
                ( ParentFsNodeRef . FsChildren ^ [ LFsChildNo ] 
                , (* IN OUT *) FsApproxFmtKind := ApproxFmtKind 
                , FsRecomputeFmtKindProc := RecomputeFmtKindProc 
                ) 
            ; IF RbmTempMarkElemNo >= RbmTempMarkElemCt 
              THEN EXIT 
              ELSE 
                CASE RbmState 
                OF RbmStateDescend 
                => EXIT 
                | RbmStateBwdNl 
                => IF LFsChildNo <= 0 
                   THEN EXIT 
                   ELSE DEC ( LFsChildNo ) 
                   END (* IF *) 
                | RbmStateFwdNl 
                => INC ( LFsChildNo ) 
                ; IF LFsChildNo >= FsChildCt 
                  THEN EXIT 
                  END (* IF *) 
                END (* CASE *) 
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *) 
        END RbmTeTraverseFsFixedChildren 

    ; PROCEDURE RbmTeTraverseFsListChildren 
        ( ParentFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR (* IN OUT *) ApproxFmtKind : FmtKindTyp 
        ; RecomputeFmtKindProc : ProcTyp 
          (* ^A procedure to compute FmtKind for this list of Fs children, 
             helpful whenever we discover Either ApproxFmtKind = FmtKindVert, 
             or we are in RbmStateFwdNl. *) 
        ; FsChildCt : LangUtil . FsChildNoTyp 
        ; InitialFsChildNo : LangUtil . FsChildNoTyp 
        ) 
      RAISES { Backout , Thread . Alerted } 

      = VAR LFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* RbmTeTraverseFsListChildren *)  
          LFsChildNo := InitialFsChildNo 
        ; Assert 
            ( 0 <= LFsChildNo AND LFsChildNo < FsChildCt 
            , AFT . A_RbmTeTraverseFsListChildren_InitialChildNumberOutOfRange  
            ) 
        ; LOOP 
            RbmTeTraverseFs 
              ( ParentFsNodeRef . FsChildren ^ [ LFsChildNo ] 
              , (* IN OUT *) FsApproxFmtKind := ApproxFmtKind 
              , FsRecomputeFmtKindProc := RecomputeFmtKindProc 
              ) 
          ; IF RbmTempMarkElemNo >= RbmTempMarkElemCt 
            THEN EXIT 
(* CHECK: ^Is this unnecessarily strong?  Can we be in RbmStateDescend
          when this happens?  Or should we only check when in RbmStateFwdNl? *) 
            ELSE 
              CASE RbmState 
              OF RbmStateDescend 
              => EXIT 
              | RbmStateBwdNl 
              => IF LFsChildNo = 0 AND RbmTeEstTravInfo . EtiChildNo < 0 
                 THEN 
                   EXIT 
                 ELSE 
                   LFsChildNo := ( LFsChildNo - 1 ) MOD FsChildCt 
                 END (* IF *) 
              | RbmStateFwdNl 
              => IF LFsChildNo = RbmTeRMFsChildNo  
                    AND RbmTeEstTravInfo . EtiChildNo 
                        >= RbmTeEstTravInfo . EtiChildCt 
                 THEN 
                   EXIT 
                 ELSE 
                   LFsChildNo := ( LFsChildNo + 1 ) MOD FsChildCt 
                 END (* IF *) 
              END (* CASE *) 
            END (* IF *) 
          END (* LOOP *) 
        END RbmTeTraverseFsListChildren 

    ; PROCEDURE RbmTeTraverseFs 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR (* IN OUT *) FsApproxFmtKind : FmtKindTyp 
          (* May be unknown at entry. *) 
        ; FsRecomputeFmtKindProc : ProcTyp 
          (* ^A procedure to compute FmtKind for this Fs tree, helpful whenever
             we discover Either ApproxFmtKind = FmtKindVert, or we are in
             RbmStateFwdNl. *) 
        ) 
      RAISES { Backout , Thread . Alerted } 

      = PROCEDURE RbmTeTfsSetIndentInfo ( Bwd : BOOLEAN )  

        = BEGIN 
            IF RbmTeEstTravInfo . EtiParentRef = NIL
            THEN RbmTeIsFirstLine := TRUE
            ELSE 
              RbmTeIsFirstLine 
                := RbmTeEstTravInfo . EtiChildNo 
                   < RbmTeEstTravInfo . EtiParentRef . KTreeEstChildCtLeftOfNl
            END (* IF *) 
          ; IF RbmTeIsFirstLine 
            THEN
              RbmTeIndentPos := EstIndentPos1 
            ELSE
              RbmTeIndentPos := EstIndentPosN 
            END (* IF *) 
          END RbmTeTfsSetIndentInfo 

      ; PROCEDURE RbmTeTfsEnterStateFwdNl ( ) 
        (* PRE: RbmCurrentLineTokMark identifies the rightmost new line at
                beginnint of the line we are entering. 
        *) 

        = BEGIN (* RbmTeTfsEnterStateFwdNl  *)  
            RbmMaxLineBreakCt := MAX ( RbmMaxLineBreakCt , RbmLineBreakCt ) 
          ; RbmState := RbmStateFwdNl 
          ; FsRecomputeFmtKindProc ( ) 
          END RbmTeTfsEnterStateFwdNl  

      ; PROCEDURE RbmTeTfsEstSubtree ( ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR EssChildFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR EssChildIndentPos1 : LbeStd . LimitedCharNoTyp 
        ; VAR EssChildFmtKind : FmtKindTyp 
        ; VAR EssOrigChildFmtKind : FmtKindTyp 

        ; PROCEDURE RbmTeTfsEssRecomputeFmtKind ( ) 
          RAISES { Backout } 
          (* A callback.  Called when we start traversing Fwd (which becomes 
             known at a leaf site).  Uses environment of the Est subtree to 
             compute its FmtKind.  These callbacks are laced through and 
             recurse from the leaf site back up to the root, then return 
             downward.
          *) 

          = VAR LChildFmtKind : FmtKindTyp 

          ; BEGIN (* RbmTeTfsEssRecomputeFmtKind *)  

            (* Upward pass to root: *) 
              IF EssChildFmtKind = FmtKindVert 
              THEN (* Our subtree is vertical. *)  
                IF EstApproxFmtKind = FmtKindUnknown 
                THEN (* Propagate now-known vertical up to parent.  *) 
                  EstApproxFmtKind := FmtKindVert 
                ; INC ( RbmEstUpFmtKindCt ) 
                ELSE (* Consistency check. *) 
                  Assert 
                    ( EstApproxFmtKind = FmtKindVert 
                    , AFT . AF_RbmTeTfsEssRecomputeFmtKind_InconsistentEstVertFromBelow
                    ) 
                END (* IF *)
              END (* IF *)

            (* Recurse upward, towards root. *) 
            ; EstRecomputeFmtKindProc ( ) 
              (* Could change EstApproxFmtKind of _parent_. *) 

            (* Downward pass: *) 
            ; IF RbmState = RbmStateFwdNl  
              THEN (* Try again to compute FmtKind at this level. *)
                LChildFmtKind 
                  := TravUtil . FmtKindForEstDescending 
                      ( FsKind := EssChildFsNodeRef . FsKind 
                      , ParentFmtKind := FsApproxFmtKind 
                      , FirstLineIndentPos := EssChildIndentPos1 
                      , EstRef 
                          := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                      , StartMark := RbmCurrentLineTokMark 
                      , StartMarkIsKnownNl := TRUE 
                      ) 
                (* ^Even though we are now traversing forward and know a 
                    CharPos, we don't know that we are at the left edge of the 
                    Est, so we have to use *Descending, not *Traversing. *) 
              ; IF EssChildFmtKind = FmtKindUnknown 
                THEN 
                  IF LChildFmtKind # FmtKindUnknown 
                  THEN (* Our subtree just became known. *) 
                    EssChildFmtKind := LChildFmtKind 
                  ; INC ( RbmEstFwdFmtKindCt ) 
                  END (* IF *) 
                ELSE (* Consistency check. *) 
                  Assert  
                    ( LChildFmtKind = EssChildFmtKind
                    , AFT . AF_RbmTeTfsEssRecomputeFmtKind_InconsistentRecomputedFmtKind
                    ) 
                END (* IF *)      
              END (* IF *) 
            END RbmTeTfsEssRecomputeFmtKind

        ; BEGIN (* RbmTeTfsEstSubtree *) 
          (* DECLARE *) 
            VAR LChildIndentPosN : LbeStd . LimitedCharNoTyp 

          ; BEGIN (* RbmTeTfsEstSubtree *) 
              IF RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef # NIL 
              THEN 
                EssChildFsNodeRef 
                  := LangUtil . FsRuleForEstChild 
                       ( ParseInfo . PiLang 
                       , FsNodeRef 
                       , RbmTeEstTravInfo . EtiChildLeafElem  
                       ) 
              ; CASE RbmState 
                OF RbmStateDescend 
                => RbmTeTfsSetIndentInfo ( Bwd := FALSE )  
                ; TravUtil . ChildIndentPositions 
                    ( ParseInfo . PiLang 
                    , FsNodeRef 
                    , EstIndentPos1 
                    , EstIndentPosN
                    , (* VAR *) ChildIndentPos1 := EssChildIndentPos1 
                    , (* VAR *) ChildIndentPosN := LChildIndentPosN 
                    , IsFirstLine := RbmTeIsFirstLine  
                    ) 
                ; EssChildFmtKind 
                    := TravUtil . FmtKindForEstDescending 
                        ( FsKind := EssChildFsNodeRef . FsKind 
                        , ParentFmtKind := FsApproxFmtKind 
                        , FirstLineIndentPos := EssChildIndentPos1 
                        , EstRef 
                            := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                        , StartMark 
                            := ParseInfo . PiFinalTempMarkListRef 
                               ^ [ RbmTempMarkElemNo ]  
                                 . TokMark 
                        , StartMarkIsKnownNl := FALSE 
                        ) 
                ; IF EssChildFmtKind # FmtKindUnknown 
                  THEN INC ( RbmEstDescendFmtKindCt ) 
                  END (* IF *) 

                | RbmStateFwdNl 
                => TravUtil . ChildIndentPositions 
                     ( ParseInfo . PiLang 
                     , FsNodeRef 
                     , EstIndentPos1 
                     , EstIndentPosN
                     , (* VAR *) ChildIndentPos1 := EssChildIndentPos1 
                     , (* VAR *) ChildIndentPosN := LChildIndentPosN 
                     , IsFirstLine := RbmTeIsFirstLine   
                     ) 
                ; EssChildFmtKind 
                    := TravUtil . FmtKindForEstTraversing 
                         ( Lang := ParseInfo . PiLang 
                         , CharPos := RbmCharPos 
                         , ModTextIsToLeftOnLine := FALSE 
                         , PrevTok := RbmPrevTok 
                         , FsKind := EssChildFsNodeRef . FsKind 
                         , ParentFmtKind := FsApproxFmtKind 
                         , FirstLineIndentPos := EssChildIndentPos1
                         , EstRef 
                             := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef
                         ) 

                | RbmStateBwdNl 
                => RbmTeTfsSetIndentInfo ( Bwd := TRUE )  
                ;  TravUtil . ChildIndentPositions 
                     ( ParseInfo . PiLang 
                     , FsNodeRef 
                     , EstIndentPos1 
                     , EstIndentPosN
                     , (* VAR *) ChildIndentPos1 := EssChildIndentPos1 
                     , (* VAR *) ChildIndentPosN := LChildIndentPosN 
                     , IsFirstLine := RbmTeIsFirstLine   
                     ) 
                ; EssChildFmtKind 
                    := TravUtil . FmtKindForEstTraversing 
                         ( Lang := ParseInfo . PiLang 
                         , CharPos := LbeStd . LimitedCharNoUnknown  
                         , ModTextIsToLeftOnLine := FALSE 
                           (* ^Irrelevant, since CharPos is unknown. *) 
                         , PrevTok := LbeStd . Tok__BegOfLine 
                           (* ^Irrelevant, since CharPos is unknown. *) 
                         , FsKind := EssChildFsNodeRef . FsKind 
                         , ParentFmtKind := FsApproxFmtKind 
                         , FirstLineIndentPos := EssChildIndentPos1 
                         , EstRef 
                             := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef
                         )
                  (* ^This will often be Unknown, but we benefit from whatever
                     known values we get, by making some line breaks known 
                     to be taken, enabling going into FmtNl. *)  
                  
                END (* CASE *)    
              ; EssOrigChildFmtKind := EssChildFmtKind (* For debug help. *) 
              ; RbmTraverseEst 
                  ( RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                  , RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet  
                  , RbmTeEstTravInfo . EtiChildNo 
                  , RbmTeEstTravInfo . EtiParentRef   
                  , EstAbsNodeNo + RbmTeEstTravInfo . EtiChildRelNodeNo 
                  , EssChildFsNodeRef 
                  , (* IN OUT *) EssChildFmtKind  
                  , EstRecomputeFmtKindProc := RbmTeTfsEssRecomputeFmtKind
                  , EstIndentPos1 := EssChildIndentPos1 
                  , EstIndentPosN := LChildIndentPosN 
                  ) 
              END (* IF Child ref non-nil *) 
            ; CASE RbmState 
              OF RbmStateBwdNl 
              => RbmTeDecEstChild ( ) 

              | RbmStateDescend (* Here, it is actually ascend. *) 
              => RbmTeUnmarkCurrentChild ( )
                 (* No RbmTeIncEstChild, so we will be able, at the next Est
                    level up, to find a RightSib TempMark belonging in the 
                    parent Est's FsTree, but pointing to this Est node. *)
              ; RbmTeIncEstChild ( ) 

              | RbmStateFwdNl 
              => RbmTeUnmarkCurrentChild ( )
              ; RbmTeIncEstChild ( ) 
              END (* CASE *) 
            END (* Block for RbmTeTfsEstSubtree *)  
          END RbmTeTfsEstSubtree 

      (* Leading mods. *) 

      ; PROCEDURE RbmTeTfsBlankLine 
          ( ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { Backout } 

        = BEGIN (* RbmTeTfsBlankLine *) 
            IF RbmTempMarkElemNo < RbmTempMarkElemCt 
            THEN 
              CASE RbmState 
              OF RbmStateDescend 
              (* Since a blank line mod always begins 
                 and ends with new lines, skip going through 
                 RbmStateBwdNl and RbmStateFwdNl.  Instead, 
                 just handle the blank line mod completely 
                 here, starting in RbmStateDescend and 
                 ending in the same state, looking for the 
                 next mark. *) 
              => Assert 
                   ( EstHs . EstChildKindContainsTempMark 
                     IN RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                   , AFT . A_RbmTeTfsBlankLine_NoMarkOnMod 
                   ) 
              ; Assert 
                  ( ModBlankLine 
                    = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo ] 
                      . TokMark . TkmEstRef 
                  , AFT . A_RbmTeTfsBlankLine_MarkedModMismatch 
                  ) 
              ; RbmCurrentLineTokMark 
                  := Marks . TokMarkTyp 
                       { TkmEstNodeNo  
                           := EstAbsNodeNo 
                              + RbmTeEstTravInfo . EtiChildRelNodeNo 
                       , TkmEstNodeCt := 1 
                       , TkmEstRef
                           := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       , TkmKind := MarkKindTyp . BlankLine 
                       , TkmFmtNo := FsNodeRef . FsFmtNo 
                       , TkmStartAtEnd := FALSE 
                       , IsImpliedNewLine := FALSE 
                       , TkmTok := LbeStd . Tok__BlankLine 
                       } 
              ; LOOP (* Thru temp marks in this blank line *) 
                  WITH 
                    WTempMark 
                    = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo ] 
                  DO Assert 
                       ( RbmTempMarkElemNo < RbmTempMarkElemCt 
                         AND WTempMark . TokMark . TkmKind 
                             = MarkKindTyp . BlankLine 
                       , AFT . A_RbmTeTfsBlankLine_BadKind 
                       ) 
                  ; RbmLineMarkMeat . LmCharPos := WTempMark . CharPos 
                  ; RbmLineMarkMeat . LmLineNo := WTempMark . LineNo 
                  ; RbmLineMarkMeat . LmTokMark := RbmCurrentLineTokMark 
                  ; RbmLineMarkMeat . LmLinesRef . LrHasMark := FALSE 
                  ; RbmLineMarkMeat . LmLinesRef := NIL 
                  END (* WITH WTempMark *) 
                ; IF RbmIncTempMark ( ModBlankLine ) THEN EXIT END (* IF *) 
                END (* LOOP *) 
              ; RbmTeUnmarkCurrentChild ( )
              ; RbmTeIncEstChild ( ) 

              | RbmStateBwdNl 
              => RbmEstRefForTrailingMarks := NIL 
              ; RbmCharPos := 0 
              ; RbmPrevTok := LbeStd . Tok__BegOfLine 
              ; RbmPrevTokToPos := 0 
              ; RbmTokBegPos := 0 
              ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
              ; RbmEstListChildrenToPass := 0 (* Dead *) 
              ; RbmCurrentLineTokMark 
                  := Marks . TokMarkTyp 
                       { TkmEstNodeNo  
                           := EstAbsNodeNo 
                              + RbmTeEstTravInfo . EtiChildRelNodeNo 
                       , TkmEstNodeCt := 1 
                       , TkmEstRef
                           := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       , TkmKind := MarkKindTyp . BlankLine 
                       , TkmFmtNo := FsNodeRef . FsFmtNo 
                       , TkmStartAtEnd := TRUE 
                       , IsImpliedNewLine := FALSE 
                       , TkmTok := LbeStd . Tok__BlankLine 
                       }
              ; RbmTeTfsSetIndentInfo ( Bwd := TRUE )  
              ; RbmTeIncEstChild ( ) 
              ; RbmTeTfsEnterStateFwdNl ( ) 

              | RbmStateFwdNl 
              (* We have hit the end of the current line. 
                 Go into RbmStateDescend w/o processing the blank line. *) 
              => RbmTeSqueezeTrailingTempMarks 
                   ( RbmTokBegPos 
                   , LbeStd . LimitedCharNoInfinity 
                   ) 
              ; RbmCharPos := LbeStd . LimitedCharNoUnknown 
              ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
              ; RbmEstListChildrenToPass := 0 (* Dead *) 
              ; RbmReenterStateDescend ( ) 
              END (* CASE RbmState *) 
            END (* IF *) 
          END RbmTeTfsBlankLine 

      ; PROCEDURE RbmTeTfsCmntFwdNl ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { Backout } 

        = BEGIN 
            RbmCharPos 
              := EstUtil . WidthSumSigned 
                   ( RbmTokBegPos 
                   , SharedStrings . Length ( ModCmnt . ModCmntStringRef ) 
                   ) 
          ; RbmTeBuildAnyMarksForPlainTok 
              ( RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef ) 
          ; RbmTeUnmarkCurrentChild ( )
          ; IF ModCmnt . ModCmntNlAfter 
            THEN 
              RbmTeSqueezeTrailingTempMarks 
                ( RbmTokBegPos , LbeStd . LimitedCharNoInfinity ) 
            ; RbmCharPos := 0 
            ; RbmPrevTok := LbeStd . Tok__BegOfLine 
            ; RbmPrevTokToPos := 0 
            ; RbmTokBegPos := 0 
            ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
            ; RbmEstListChildrenToPass := 0 (* Dead *) 
            ; RbmCurrentLineTokMark 
                := Marks . TokMarkTyp 
                     { TkmEstNodeNo  
                         := EstAbsNodeNo 
                            + RbmTeEstTravInfo . EtiChildRelNodeNo 
                     , TkmEstNodeCt := 1 
                     , TkmEstRef
                         := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     , TkmKind := MarkKindTyp . Plain 
                     , TkmFmtNo := FsNodeRef . FsFmtNo 
                     , TkmStartAtEnd := TRUE 
                     , IsImpliedNewLine := FALSE 
                     , TkmTok := LbeStd . Tok__CmntAtEndOfLine 
                     } 
            ELSE 
              RbmPrevTok := LbeStd . Tok__Cmnt 
            ; RbmPrevTokToPos := RbmCharPos 
            END (* IF *) 
          END RbmTeTfsCmntFwdNl  

      ; PROCEDURE RbmTeTfsBolCmntIntoFwdNl ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { Backout } 
        (* Transition into FwdNl, at a comment known to have a NlBefore. *) 

        = VAR LTok : LbeStd . TokTyp 

        ; BEGIN (* RbmTeTfsBolCmntIntoFwdNl *) 
            RbmEstRefForTrailingMarks := NIL 
          ; RbmCharPos := 0 
          ; RbmPrevTok := LbeStd . Tok__BegOfLine 
          ; RbmPrevTokToPos := 0
          ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
          ; RbmEstListChildrenToPass := 0 (* Dead *) 
          ; IF ModCmnt . ModCmntNlAfter 
            THEN LTok := LbeStd . Tok__CmntAtEndOfLine 
            ELSE LTok := LbeStd . Tok__Cmnt 
            END (* IF *) 
          ; RbmTeTfsSetIndentInfo ( Bwd := TRUE )  
          ; RbmCurrentLineTokMark 
              := Marks . TokMarkTyp 
                   { TkmEstNodeNo  
                       := EstAbsNodeNo 
                          + RbmTeEstTravInfo . EtiChildRelNodeNo 
                   , TkmEstNodeCt := 1 
                   , TkmEstRef
                       := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                   , TkmKind := MarkKindTyp . Plain 
                   , TkmFmtNo := FsNodeRef . FsFmtNo 
                   , TkmStartAtEnd := FALSE 
                   , IsImpliedNewLine := FALSE 
                   , TkmTok := LTok 
                   } 

          ; RbmTeTfsEnterStateFwdNl ( ) 
          (* Compute position on line. *) 
          ; TYPECASE ModCmnt 
            OF ModHs . ModCmntLeadingFixedTyp 
            => RbmTokBegPos := ModCmnt . ModCmntFromPos 

            | ModHs . ModCmntLeadingRelativeTyp 
            => RbmTokBegPos 
                 := EstUtil . WidthSumSigned  
                      ( TravUtil . IndentPos 
                          ( ParseInfo . PiLang 
                          , RbmTeIndentPos 
                          , FsNodeRef . FsIndentCode 
                          ) 
                      , ModCmnt . ModCmntFromPos 
                      ) 
            ELSE 
              CantHappen ( AFT . A_RbmTeTfsBolCmntIntoFwdNl_BadCommentType ) 
            END (* TYPECASE *) 
          ; RbmTeTfsCmntFwdNl ( ModCmnt ) 
          ; RbmTeTfsSetIndentInfo ( Bwd := FALSE)  
          ; RbmTeIncEstChild ( ) 
          END RbmTeTfsBolCmntIntoFwdNl 

      ; PROCEDURE RbmTeTfsLeadingModCmnt ( ModCmnt : ModHs . ModCmntLeadingTyp )
        (* Handle a leading ModCmnt. *) 

        = VAR LTokBegPos : LbeStd . LimitedCharNoTyp 

        ; BEGIN (* RbmTeTfsLeadingModCmnt *) 
            IF ModCmnt . ModCmntNlBefore 
            THEN (* Explicit Nl before is a known reverse point. *)
              CASE RbmState 
              OF RbmStateDescend 
              (* Go directly to FwdNl and handle comment at NL. *) 
              => Assert 
                   ( EstHs . EstChildKindContainsTempMark 
                     IN RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                   , AFT . A_RbmTeTfsLeadingModsNoDel_NoMarkOnMod 
                   ) 
              ; Assert 
                  ( ModCmnt 
                    = ParseInfo . PiFinalTempMarkListRef ^ 
                      [ RbmTempMarkElemNo ] 
                      . TokMark . TkmEstRef  
                  , AFT . A_RbmTeTfsLeadingModsNoDel_MarkedModMismatch 
                  ) 
              ; RbmTeTfsBolCmntIntoFwdNl ( ModCmnt ) 

              | RbmStateBwdNl 
              (* StartFwdNl processing at the beginning of comment. *) 
              => RbmTeTfsBolCmntIntoFwdNl ( ModCmnt ) 

              | RbmStateFwdNl 
              (* End of current line at beginning of the comment. 
                 Go into RbmStateDescend w/o processing the comment. *) 
              => RbmTeSqueezeTrailingTempMarks 
                   ( RbmTokBegPos 
                   , LbeStd . LimitedCharNoInfinity 
                   ) 
              ; RbmCharPos := LbeStd . LimitedCharNoUnknown 
              ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
              ; RbmEstListChildrenToPass := 0 (* Dead *) 
              ; RbmReenterStateDescend ( ) 
              END (* CASE RbmState *) 
            ELSE
              CASE RbmState 
              OF RbmStateDescend 
              => RbmState := RbmStateBwdNl 
              ; RbmLineBreakCt := 0 
              ; RbmTeDecEstChild ( ) 
               (* NOTE: ModHs . ModCmntLeadingFixed 
                  could have an implicit new line at the beginning. We 
                  can't detect this here, arriving at a marked mod, so 
                  we ignore it and start backing up.  We will traverse 
                  through the implicit new line when in RbmStateFwdNl. *) 

              | RbmStateBwdNl 
              => RbmTeDecEstChild ( ) 
               (* NOTE: ModHs . ModCmntLeadingFixed could have an 
                  implicit new line at the beginning. We can't detect 
                  this going backwards like this, so we ignore it. We 
                  will traverse through the implicit new line when in 
                  RbmStateFwdNl. *) 

              | RbmStateFwdNl 
              => RbmPrevTokToPos := RbmCharPos 
              ; LTokBegPos := RbmTokBegPos 
              ; IF ISTYPE ( ModCmnt , ModHs . ModCmntLeadingFixedTyp ) 
                THEN 
                  RbmTokBegPos := ModCmnt . ModCmntFromPos 
                ELSE 
                  RbmTokBegPos := EstUtil . WidthSumSigned 
                    ( TravUtil . IndentPos 
                        ( ParseInfo . PiLang 
                        , RbmTeIndentPos 
                        , FsNodeRef . FsIndentCode 
                        ) 
                    , ModCmnt . ModCmntFromPos 
                    ) 
                ; RbmTokBegPos := MAX ( RbmCharPos , RbmTokBegPos ) 
                END (* IF *) 
              ; RbmTeSqueezeTrailingTempMarks ( LTokBegPos , RbmTokBegPos )
              ; RbmTeTfsCmntFwdNl ( ModCmnt ) 
              ; RbmTeIncEstChild ( ) 
              END (* CASE RbmState *) 
            END (* IF *) 
          END RbmTeTfsLeadingModCmnt

      ; PROCEDURE RbmTeTfsModTok ( EstRef : EstHs . EstRefTyp ) 
        : BOOLEAN (* Done with leading mods. *) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR LChildFmtKind : FmtKindTyp 

        ; BEGIN 
            IF EstRef . EstNodeKind 
               = EstHs . EstNodeKindTyp . EstNodeKindModTok 
            THEN 
              LFsNodeRef 
                := EstUtil . FsRuleForEstNode 
                     ( ParseInfo . PiLang 
                     , RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     )
            ; RbmTeTfsSetIndentInfo ( Bwd := RbmState = RbmStateBwdNl ) 
            ; LChildFmtKind := FmtKindHoriz 
            ; RbmTraverseEst 
                ( EstRef 
                , RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet  
                , RbmTeEstTravInfo . EtiChildNo 
                , RbmTeEstTravInfo . EtiParentRef   
                , EstAbsNodeNo + RbmTeEstTravInfo . EtiChildRelNodeNo 
                , LFsNodeRef 
                , (* IN OUT *) LChildFmtKind 
                , EstRecomputeFmtKindProc := FsRecomputeFmtKindProc  
                , EstIndentPos1 := RbmTeIndentPos 
                , EstIndentPosN := RbmTeIndentPos 
                ) 
            ; Assert 
                ( LChildFmtKind = FmtKindHoriz  
                , AFT . A_RbmTeTfsModTok_FmtKind_changed

                ) 
            ; CASE RbmState 
              OF RbmStateBwdNl 
              => RbmTeDecEstChild ( ) 

              | RbmStateDescend 
              , RbmStateFwdNl
              => RbmTeUnmarkCurrentChild ( ) 
              ; RbmTeIncEstChild ( ) 
              END (* CASE *) 
            ; RETURN FALSE 
            ELSE (* Not a leading mod. *) 
              RETURN TRUE 
            END (* IF *) 
          END RbmTeTfsModTok 

      ; PROCEDURE RbmTeTfsLeadingModsNoDel ( ) 
        RAISES { Backout , Thread . Alerted } 
        (* Handle any leading mods, except for a delete mod. *) 

        = BEGIN (* RbmTeTfsLeadingModsNoDel *)  
            LOOP 
              IF RbmTempMarkElemNo >= RbmTempMarkElemCt 
                 OR RbmTeEstTravInfo . EtiChildNo 
                    >= RbmTeEstTravInfo . EtiChildCt 
                 OR RbmTeEstTravInfo . EtiChildNo < 0 
                 (* No children remain at all. *) 
                 OR FsNodeRef . FsFmtNo # RbmTeEstTravInfo . EtiChildFmtNo 
                    (* Not for this format number. *) 
                    (* This could exit on a relevant ModDel, but we don't 
                       want those anyway. *) 
              THEN 
                EXIT 
              END (* IF *) 
            (* A child with the right FmtNo is next. *) 
            ; TYPECASE RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 

              OF NULL 
              => EXIT 

              (* Blank line. *) 
              | ModHs . ModBlankLineTyp ( TModBlankLine ) 
              => RbmTeTfsBlankLine ( TModBlankLine ) 

              (* Leading comment. *) 
              | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
              => RbmTeTfsLeadingModCmnt ( TModCmnt ) 

              (* ModText can't happen here, because we just reparsed, and that
                 removes them. *) 

              (* Token insertions. *) 
              | EstHs . EstRefTyp ( TEstRef ) 
              => IF RbmTeTfsModTok ( TEstRef ) (* SIDE EFFECTS! *) 
                 THEN EXIT 
                 END (* IF *) 

              (* Lex error characters *) 
              | SharedStrings . T ( TString ) 
              => IF SharedStrings . Tok ( TString ) = LbeStd . Tok__LexErrChars 
                 THEN  
                   RbmTeLeafString 
                      ( FsNodeRef , FsApproxFmtKind , TString ) 
                 ; CASE RbmState <* NOWARN *> 
                   OF RbmStateBwdNl 
                   => RbmTeDecEstChild ( ) 
                   | RbmStateFwdNl 
                   => RbmTeUnmarkCurrentChild ( )
                   ; RbmTeIncEstChild ( ) 
                   END (* CASE *) 
                 ELSE (* This is not a mod. *) 
                   EXIT 
                 END (* IF *) 

              (* Errors. *) 
              | ModHs . ModErrTyp 
              => CASE RbmState 
                 OF RbmStateDescend 
                 => RbmTeDecEstChild ( ) 
                 ; RbmState := RbmStateBwdNl 
                 ; RbmLineBreakCt := 0 
                 ; CantHappen  
                     ( AFT . A_RbmTeTfsLeadingModsNoDel_DescendToError )  
(* CHECK: If this can happen, then the error is marked, and we need to also 
   handle it in FwdNl.  Also check handling in Btm for this case. *) 

                 | RbmStateBwdNl 
                 => RbmTeDecEstChild ( ) 

                 | RbmStateFwdNl 
                 => RbmTeIncEstChild ( ) 
                 END (* CASE RbmState *) 

              (* Can't happen kinds: *) 
              | ModHs . ModTextTyp 
                (* We only do this after reparse, which leaves no text mods. *) 
              => CantHappen ( AFT . A_RbmTeTfsLeadingModsNoDel_BadObjKind ) 
              ; RbmTeIncEstChild ( ) 

              ELSE (* ModDel or not a leading mod. *) 
                EXIT 
              END (* TYPECASE *) 
            ; IF RbmState = RbmStateDescend 
              THEN 
                EXIT 
              END (* IF *) 
            END (* LOOP *) 
          END RbmTeTfsLeadingModsNoDel 

      (* Trailing mods. *) 

      ; PROCEDURE RbmTeTfsModCmntTrailingFwdNl 
          ( ModCmntTrailing : ModHs . ModCmntTrailingTyp ) 
        RAISES { Backout } 

        (* Handles ModHs . ModCmntTrailingTyp *) 

        = VAR LTokBegPos : LbeStd . LimitedCharNoTyp 
        ; VAR LTok : LbeStd . TokTyp 
        ; VAR LIsFixed : BOOLEAN 

        ; BEGIN (* RbmTeTfsModCmntTrailingFwdNl *) 
            LIsFixed 
              := ISTYPE ( ModCmntTrailing , ModHs . ModCmntTrailingFixedTyp )
          ; IF LIsFixed AND RbmCharPos > ModCmntTrailing . ModCmntFromPos 
            THEN (* This is an implicit new line, which could not be detected 
                    going backwards. It is possible that the marked spot which 
                    started this period in RbmStateFwdNl is to the right of 
                    this implicit new line. In case this is so, we just keep 
                    going through the next line. Even if not, we might get 
                    lucky and pick up some marks on it. *) 
              RbmTeSqueezeTrailingTempMarks 
                ( RbmTokBegPos 
                , LbeStd . LimitedCharNoInfinity 
                ) 
            ; RbmCharPos := 0 
            ; RbmPrevTok := LbeStd . Tok__BegOfLine 
            ; RbmPrevTokToPos := 0 
            ; RbmTokBegPos := 0 
            ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
            ; RbmEstListChildrenToPass := 0 (* Dead *) 
            ; IF ModCmntTrailing . ModCmntNlAfter 
              THEN LTok := LbeStd . Tok__CmntAtEndOfLine 
              ELSE LTok := LbeStd . Tok__Cmnt 
              END (* IF *) 
            ; RbmCurrentLineTokMark 
                := Marks . TokMarkTyp 
                     { TkmEstNodeNo  
                         := EstAbsNodeNo 
                            + RbmTeEstTravInfo . EtiChildRelNodeNo 
                     , TkmEstNodeCt := 1 
                     , TkmEstRef
                         := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     , TkmKind := MarkKindTyp . Plain 
                     , TkmFmtNo := FsNodeRef . FsFmtNo 
                     , TkmStartAtEnd := FALSE 
                     , IsImpliedNewLine := TRUE 
                     , TkmTok := LTok 
                     } 
            ELSE (* No implicit new line *) 
              RbmPrevTokToPos := RbmCharPos 
            ; LTokBegPos := RbmTokBegPos 
            ; IF LIsFixed 
              THEN 
                RbmTokBegPos := ModCmntTrailing . ModCmntFromPos 
              ELSE 
                RbmTokBegPos := EstUtil . WidthSumSigned 
                  ( RbmCharPos , ModCmntTrailing . ModCmntFromPos ) 
                (* ^SameLine comment implies there is a token preceeding, 
                    so no need to consider IndentPos. *) 
              END (* IF *) 
            ; RbmTeSqueezeTrailingTempMarks 
                ( LTokBegPos 
                , RbmTokBegPos 
                ) 
            END (* IF *) 
          (* Now process any marks in the comment. *) 
          ; RbmCharPos 
              := EstUtil . WidthSumSigned 
                   ( RbmTokBegPos 
                   , SharedStrings . Length 
                       ( ModCmntTrailing . ModCmntStringRef ) 
                   ) 
          ; RbmTeBuildAnyMarksForPlainTok ( ModCmntTrailing ) 
          ; RbmTeUnmarkCurrentChild ( )
          (* If new line after, process it, but stay in FwdNl. *) 
          ; IF ModCmntTrailing . ModCmntNlAfter 
            THEN 
              RbmTeSqueezeTrailingTempMarks 
                ( RbmTokBegPos 
                , LbeStd . LimitedCharNoInfinity 
                ) 
            ; RbmCharPos := 0 
            ; RbmPrevTok := LbeStd . Tok__BegOfLine 
            ; RbmPrevTokToPos := 0 
            ; RbmTokBegPos := 0 
            ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
            ; RbmEstListChildrenToPass := 0 (* Dead *) 
            ; RbmCurrentLineTokMark 
                := Marks . TokMarkTyp 
                     { TkmEstNodeNo  
                         := EstAbsNodeNo 
                            + RbmTeEstTravInfo . EtiChildRelNodeNo 
                     , TkmEstNodeCt := 1 
                     , TkmEstRef
                         := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     , TkmKind := MarkKindTyp . Plain 
                     , TkmFmtNo := FsNodeRef . FsFmtNo 
                     , TkmStartAtEnd := TRUE 
                     , IsImpliedNewLine := FALSE 
                     , TkmTok := LbeStd . Tok__CmntAtEndOfLine 
                     } 
            ELSE 
              RbmPrevTok := LbeStd . Tok__Cmnt 
            ; RbmPrevTokToPos := RbmCharPos 
            END (* IF *) 
          ; RbmTeIncEstChild ( ) 
          END RbmTeTfsModCmntTrailingFwdNl 

      ; PROCEDURE RbmTeTfsTrailingMods ( ) RAISES { Backout } 

        = BEGIN (* RbmTeTfsTrailingMods *) 
            LOOP 
              IF RbmTempMarkElemNo >= RbmTempMarkElemCt 
                 OR RbmTeEstTravInfo . EtiChildNo 
                 >= RbmTeEstTravInfo . EtiChildCt 
                 OR RbmTeEstTravInfo . EtiChildNo < 0 
                 (* No children remain at all. *) 
                 OR FsNodeRef . FsFmtNo # RbmTeEstTravInfo . EtiChildFmtNo 
                    (* Not for this format number. *) 
                    (* This could exit on a relevant ModDel, but we don't 
                       want those anyway. *) 
              THEN 
                EXIT 
              END (* IF *) 
            (* A child with matching FmtNo is next. *) 
            ; TYPECASE RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => (* Not a trailing mod. *) 
                 EXIT 
              | ModHs . ModCmntTrailingTyp ( TModCmntTrailing ) 
              => CASE RbmState 
                 OF RbmStateDescend 
                 => RbmState := RbmStateBwdNl 
                 ; RbmLineBreakCt := 0 
                 ; RbmTeDecEstChild ( ) 
                  (* NOTE: ModHs . ModCmntTrailingFixed 
                     could have an implicit new line at the beginning. We 
                     can't detect this here, arriving at a marked mod, so 
                     we ignore it and start backing up.  We will traverse 
                     through the implicit new line when in RbmStateFwdNl. *) 
                 | RbmStateBwdNl 
                 => RbmTeDecEstChild ( ) 
                  (* NOTE: ModHs . ModCmntTrailingFixed could have an 
                     implicit new line at the beginning. We can't detect this 
                     going backwards like this, so we ignore it. We will 
                     traverse through the implicit new line when in 
                     RbmStateFwdNl. *) 
                 | RbmStateFwdNl 
                 => RbmTeTfsModCmntTrailingFwdNl ( TModCmntTrailing ) 
                 END (* CASE RbmState *) 
              ELSE (* Not a trailing mod. *) 
                EXIT 
              END (* TYPECASE *) 
            ; IF RbmState = RbmStateDescend 
              THEN 
                EXIT 
              END (* IF *) 
            END (* LOOP *) 
          END RbmTeTfsTrailingMods 

      (* Delete mods. *) 

      ; PROCEDURE RbmTeTfsFwdLeadingDel 
          ( VAR Delete : BOOLEAN ; VAR IsRepair : BOOLEAN ) 
        RAISES { Backout } 
        (* When RebuildMarkList is used, the only delete mods should have
           IsRepair, and these are considered present.  So nothing is
           ever treated as deleted.  Still, for consistency and in
           case something about this ever changes, we return the full
           information about ModDels and let the caller decide what to
           do about it. *)  

        = BEGIN (* RbmTeTfsFwdLeadingDel *) 
            Delete := FALSE 
          ; IsRepair := FALSE 
          ; Assert 
              ( RbmState 
                IN RbmStateSetTyp 
                     { RbmStateFwdNl 
                     , RbmStateDescend 
                     } 
              , AFT . A_RbmTeTfsFwdLeadingDel_BadState 
              ) 
          ; IF RbmTeEstTravInfo . EtiChildNo < RbmTeEstTravInfo . EtiChildCt 
            THEN 
              TYPECASE RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => 
              | ModHs . ModDelTyp ( TModDel ) 
              => (* A Deletion mod is next. *) 
                 Assert 
                   ( FsNodeRef . FsFmtNo <= TModDel . ModDelThruFmtNo 
                   , AFT . A_RbmTeTfsFwdLeadingDel_Skipped_Delete_Mod 
                   ) 
              ; Assert 
                  ( NOT FsNodeRef . FsIsInsideList 
                    OR FsNodeRef . FsFmtNo # EstHs . FmtNoListEstChild
                  , AFT . A_RbmTeTfsFwdLeadingDel_Deleted_Est_List_Child 
                  ) 
              ; IF FsNodeRef . FsFmtNo >= RbmTeEstTravInfo . EtiChildFmtNo 
                THEN (* The delete mod applies to this token. *) 
                  Delete := TRUE 
                ; IsRepair 
                    := EstHs . EstChildKindContainsInsertionRepair 
                       IN RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                ; IF FsNodeRef . FsFmtNo = TModDel . ModDelThruFmtNo 
                  THEN (* consume the del mod. *) 
                    RbmTeIncEstChild ( ) 
                  END (* IF *) 
                END (* IF *) 
              ELSE 
              END (* TYPECASE *) 
            END (* IF *) 
          END RbmTeTfsFwdLeadingDel 

      ; PROCEDURE RbmTeTfsBwdLeadingDel 
          ( VAR Delete : BOOLEAN ; VAR IsRepair : BOOLEAN ) 
        RAISES { Backout } 
        (* When RebuildMarkList is used, the only delete mods should have
           IsRepair, and these are considered present.  So nothing is
           ever treated as deleted.  Still, for consistency and in
           case something about this ever changes, we return the full
           information about ModDels and let the caller decide what to
           do about it. *)  

        = BEGIN (* RbmTeTfsBwdLeadingDel *) 
            Delete := FALSE 
          ; IsRepair := FALSE 
          ; Assert 
              ( RbmState = RbmStateBwdNl 
              , AFT . A_RbmTeTfsBwdLeadingDel_BadState 
              ) 
          ; IF 0 <= RbmTeEstTravInfo . EtiChildNo 
            THEN 
              TYPECASE RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => 
              | ModHs . ModDelTyp ( TModDel ) 
              => (* A Deletion mod is next. *) Assert 
                   ( FsNodeRef . FsFmtNo 
                     >= RbmTeEstTravInfo . EtiChildFmtNo 
                   , AFT . A_RbmTeTfsBwdLeadingDel_Skipped_Delete_Mod 
                   ) 
              ; Assert 
                  (  FsNodeRef . FsIsInsideList 
                    OR FsNodeRef . FsFmtNo # EstHs . FmtNoListEstChild
                  , AFT . A_RbmTeTfsBwdLeadingDel_Deleted_Est_List_Child 
                  ) 
              ; IF FsNodeRef . FsFmtNo <= TModDel . ModDelThruFmtNo 
                THEN (* The delete mod applies to this token. *) 
                  Delete := TRUE 
                ; IsRepair 
                    := EstHs . EstChildKindContainsInsertionRepair 
                       IN RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                ; IF FsNodeRef . FsFmtNo = RbmTeEstTravInfo . EtiChildFmtNo 
                  THEN (* consume the del mod. *) 
                    RbmTeDecEstChild ( ) 
                  END (* IF *) 
                END (* IF *) 
              ELSE 
              END (* TYPECASE *) 
            END (* IF *) 
          END RbmTeTfsBwdLeadingDel 

      (* Format syntax trees *) 

      ; PROCEDURE RbmTeTfsInsTok ( ) RAISES { Backout } 

        = BEGIN (* RbmTeTfsInsTok *) 
            IF RbmTempMarkElemNo < RbmTempMarkElemCt 
            THEN 
              CASE RbmState 
              OF RbmStateDescend 
              => WITH 
                   WTempMark 
                   = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo ] 
                 DO (* The next temp mark should denote this InsTok. *) 
(* CHECK: Sheesh.  Is all this really necessary? *) 
                    CASE WTempMark . TokMark . TkmKind <* NOWARN *> 
                    OF MarkKindTyp . RightSibFmtNo 
                    => 
                      Assert 
                        ( RbmTeRMChildRef = WTempMark . TokMark . TkmEstRef 
                        , AFT . A_RbmTeTfsInsTok_ListInsTokWOTempMark 
                        ) 
                         
                    | MarkKindTyp . LeftSibFmtNo 
                    => Assert 
                        ( RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                          = WTempMark . TokMark . TkmEstRef 
                        , AFT . A_RbmTeTfsInsTok_ListInsTokWOTempMark 
                        ) 

                    | MarkKindTyp . ChildFmtNo 
                    => Assert 
                        ( RbmTeEstTravInfo . EtiParentRef
                          = WTempMark . TokMark . TkmEstRef 
                        , AFT . A_RbmTeTfsInsTok_InsTokWOTempMark 
                        ) 
                    | MarkKindTyp . Plain 
                    => Assert 
                        ( RbmTeEstTravInfo . EtiParentRef
                          = WTempMark . TokMark . TkmEstRef 
                        , AFT . A_RbmTeTfsInsTok_DeletedInsTokWOTempMark 
                        ) 
                    END (* CASE *) 
                 END (* WITH WTempMark *) 
              ; RbmTeDecEstChild ( ) 
              ; RbmState := RbmStateBwdNl 
              ; RbmLineBreakCt := 0 

              | RbmStateBwdNl 
              => (* Just fall through *) 

              | RbmStateFwdNl 
              => RbmCharPos 
                   := TravUtil . PosForTok 
                        ( ParseInfo . PiLang 
                        , FsApproxFmtKind 
                        , (* ModTextIsToLeftOnLine := *) FALSE 
                        , RbmCharPos 
                        , RbmTeIndentPos 
                        , FsNodeRef . FsIndentCode 
                        , RbmPrevTok 
                        , FsNodeRef . FsTok 
                        ) 
              ; RbmTeSqueezeTrailingTempMarks 
                  ( RbmTokBegPos , RbmCharPos ) 
              ; RbmTokBegPos := RbmCharPos 
              ; RbmCharPos := EstUtil . WidthSumSigned
                  ( RbmCharPos 
                  , SharedStrings . Length ( FsNodeRef . FsInsTokRef ) 
                  ) 
              ; IF RbmTempMarkElemNo < RbmTempMarkElemCt 
                THEN 
                  WITH 
                    WTempMark 
                    = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo ] 
                  DO (* If next temp mark applies to this InsTok, do the 
                        building of all marks within. *) 
                     CASE WTempMark . TokMark . TkmKind 
                     OF MarkKindTyp . LeftSibFmtNo 
                     => IF WTempMark . TokMark . TkmFmtNo = FsNodeRef . FsFmtNo  
                           AND RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef
                               = WTempMark . TokMark . TkmEstRef 
                        THEN 
                          RbmTeBuildSomeMarksForTok 
                            ( RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef )
                        END (* IF *) 
                     | MarkKindTyp . RightSibFmtNo 
                     => IF WTempMark . TokMark . TkmFmtNo = FsNodeRef . FsFmtNo 
                           AND RbmTeRMChildRef = WTempMark . TokMark . TkmEstRef
                        THEN 
                           Assert 
                             ( RbmTeRMChildRef # NIL 
                             , AFT . A_RbmTeTfsInsTok_NILRbmTeRMChildRef 
                             ) 
                        ; RbmTeBuildSomeMarksForTok ( RbmTeRMChildRef ) 
                        END (* IF *) 

                     | MarkKindTyp . ChildFmtNo 
                     => IF WTempMark . TokMark . TkmFmtNo = FsNodeRef . FsFmtNo 
                           AND RbmTeEstTravInfo . EtiParentRef 
                               = WTempMark . TokMark . TkmEstRef 
                        THEN 
                          RbmTeBuildSomeMarksForTok 
                            ( RbmTeEstTravInfo . EtiParentRef ) 
                        END (* IF *) 
                     | MarkKindTyp . Plain  
                     => IF RbmTeEstTravInfo . EtiParentRef = WTempMark . TokMark . TkmEstRef 
                        THEN 
                          RbmTeBuildSomeMarksForTok 
                            ( RbmTeEstTravInfo . EtiParentRef ) 
                        END (* IF *) 
                     ELSE 
                     END (* CASE *) 
                  END (* WITH WTempMark *) 
                END (* IF *) 
              ; RbmPrevTok := FsNodeRef . FsTok 
              ; RbmPrevTokToPos := RbmCharPos 
              END (* CASE RbmState *) 
            END (* IF *) 
          END RbmTeTfsInsTok 

      ; PROCEDURE RbmTeTfsEstChild ( IsEstList : BOOLEAN ) 
        RAISES { Backout , Thread . Alerted } 

        = BEGIN (* RbmTeTfsEstChild *) 
            CASE RbmState 

            OF RbmStateDescend 
            => RbmTeTfsEstSubtree ( ) 

            | RbmStateBwdNl 
            => IF 0 <= RbmTeEstTravInfo . EtiChildNo 
                  AND RbmTeEstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
                  AND EstHs . EstChildKindEstChild 
                      IN RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
               THEN (* There is an Est child. *)  
                 IF ISTYPE 
                      ( RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                      , ModHs . EstDummyTyp (* Including NIL *) 
                      ) 
                 THEN 
                   RbmTeDecEstChild ( ) 
                 ELSIF EstHs . EstChildKindTrailingSep 
                       IN RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                 THEN (* Skip trailing sep. *) 
                   RbmTeDecEstChild ( ) 
                 ELSE 
                   RbmTeTfsEstSubtree ( ) 
                 END (* TYPECASE *) 
               END (* IF *)

            | RbmStateFwdNl 
            => TravUtil . AssertFwdNoLostFixedChild 
                 ( FsNodeRef , RbmTeEstTravInfo ) 
            ; IF RbmTeEstTravInfo . EtiChildNo 
                 < RbmTeEstTravInfo . EtiChildCt 
                 AND RbmTeEstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
                 AND EstHs . EstChildKindEstChild 
                     IN RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
              THEN (* There is an Est child. *)  

                TYPECASE RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                OF NULL 
                => RbmTeIncEstChild ( )
                
                | ModHs . EstDummyTempMarkTyp 
                => IF RbmTeEstTravInfo . EtiChildNo + 1 
                      < RbmTeEstTravInfo . EtiChildCt 
                  THEN (* Dummy, not the rightmost child. NIL it now. *)
                    RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef := NIL  
                  ; EstUtil . SetChildRef 
                      ( RbmTeEstRefToPatch 
                      , RbmTeCurrentChildNoToPatch 
                      , NewChildRef := NIL 
                      ) 
                  ; Assertions . MessageText 
                      ( "NILed out internal DummyTempMark, in RebuildMarksList, Root node no "
                        & LbeStd . EstNodeNoImage ( EstAbsNodeNo ) 
                        & ", child no "
                        & LbeStd . EstChildNoImage 
                            ( RbmTeEstTravInfo . EtiChildCt - 1 ) 
                      ) 
                  END (* IF *)  
                ; RbmTeIncEstChild ( )
                
                | ModHs . EstDummyTyp 
                => RbmTeIncEstChild ( ) 
                ELSE 
                  IF EstHs . EstChildKindTrailingSep 
                     IN RbmTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                  THEN (* Skip trailing sep. *) 
                    RbmTeIncEstChild ( ) 
                  ELSE 
                    RbmTeTfsEstSubtree ( ) 
                  END (* IF *) 
                END (* TYPECASE *) 
              ; IF IsEstList 
                THEN
                  TravUtil . PassEstListChild ( RbmEstListChildrenToPass ) 
                END (* IF *) 
              END (* IF *) 
            END (* CASE *) 
          END RbmTeTfsEstChild 

      ; PROCEDURE RbmTeTfsBuildFmtNoMark ( ) RAISES { Backout } 
        (* Call this when EstTravInfo is going forward. *) 

        = VAR LTok : LbeStd . TokTyp 

        ; BEGIN (* RbmTeTfsBuildFmtNoMark *) 
            IF RbmTeEstTravInfo . EtiIsOptSingletonList 
            THEN LTok := LbeStd . Tok__OptSingletonList 
            ELSE LTok := EstUtil . EstTok ( RbmTeEstTravInfo . EtiNodeRef ) 
            END (* IF *) 
          ; IF RbmTeEstTravInfo . EtiChildCt = 0 (* No Est children. *)
            THEN 
              RbmCurrentLineTokMark 
                := Marks . TokMarkTyp 
                     { TkmEstNodeNo  := EstAbsNodeNo 
                     , TkmEstNodeCt 
                         := EstUtil . EstNodeCt
                              ( RbmTeEstTravInfo . EtiNodeRef )
                     , TkmEstRef := RbmTeEstTravInfo . EtiNodeRef 
                     , TkmKind := MarkKindTyp . ChildFmtNo 
                     , TkmFmtNo := FsNodeRef . FsFmtNo 
                     , TkmStartAtEnd := FALSE 
                     , IsImpliedNewLine := FALSE 
                     , TkmTok := LTok 
                     } 
            ELSIF RbmTeEstTravInfo . EtiChildNo 
                  < RbmTeEstTravInfo . EtiChildCt 
            THEN (* There is an Est child to the right of the FmtNo item. 
                    The FmtNo item is a LeftSib of the EstChild. 
                 *) 
              RbmCurrentLineTokMark 
                := Marks . TokMarkTyp 
                     { TkmEstNodeNo  
                         := EstAbsNodeNo 
                            + RbmTeEstTravInfo . EtiChildRelNodeNo 
                     , TkmEstNodeCt 
                         := EstUtil . EstNodeCt 
                              ( RbmTeEstTravInfo . EtiChildLeafElem 
                                . LeChildRef 
                              ) 
                            + ORD ( EstHs . EstChildKindOptSingletonList 
                                    IN RbmTeEstTravInfo . EtiChildLeafElem 
                                       . LeKindSet 
                                  )
                     , TkmEstRef
                         := RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     , TkmKind := MarkKindTyp . LeftSibFmtNo 
                     , TkmFmtNo := FsNodeRef . FsFmtNo 
                     , TkmStartAtEnd := FALSE 
                     , IsImpliedNewLine := FALSE 
                     , TkmTok := LTok 
                     } 
            ELSE (* There is an Est child to the left but not to the right. *)
              RbmCurrentLineTokMark 
                := Marks . TokMarkTyp 
                     { TkmEstNodeNo  := EstAbsNodeNo + RbmTeRMChildRelNodeNo 
                     , TkmEstNodeCt 
                         := EstUtil . EstNodeCt ( RbmTeRMChildRef )
                            + ORD ( EstHs . EstChildKindOptSingletonList 
                                    IN RbmTeRMChildKindSet 
                                  )
                     , TkmEstRef := RbmTeRMChildRef
                     , TkmKind := MarkKindTyp . RightSibFmtNo 
                     , TkmFmtNo := FsNodeRef . FsFmtNo 
                     , TkmStartAtEnd := FALSE 
                     , IsImpliedNewLine := FALSE 
                     , TkmTok := LTok 
                     } 
            END (* IF *) 
          END RbmTeTfsBuildFmtNoMark 

      ; PROCEDURE RbmTeTfsLineBreak ( FsKind : FsKindTyp ) 
        RAISES { Backout } 

        = BEGIN (* RbmTeTfsLineBreak *) 
            CASE RbmState 
            OF RbmStateBwdNl 
            => IF FsApproxFmtKind = FmtKindVert 
                  OR FsKind = FsKindTyp . FsKindLineBreakReqd 
                  (* ^These are the only cases we can be certain, going 
                      backwards, that the line break should be taken.  
                  *) 
               THEN (* Reverse into forward state at this line break. *) 
                 RbmEstRefForTrailingMarks := NIL 
               ; RbmCharPos := 0 
               ; RbmPrevTok := LbeStd . Tok__BegOfLine 
               ; RbmPrevTokToPos := 0 
               ; RbmTokBegPos := 0 
               ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
               ; RbmEstListChildrenToPass := 0 (* Dead *) 
               ; RbmTeTfsSetIndentInfo ( Bwd := TRUE )  
               ; RbmTeIncEstChild ( ) 
               ; FsApproxFmtKind := FmtKindVert 
               ; RbmTeTfsEnterStateFwdNl ( ) 
               ; RbmTeTfsBuildFmtNoMark ( )
               ELSE INC ( RbmLineBreakCt ) 
               END (* IF *) 

            | RbmStateFwdNl 
            => IF RbmLineBreakCt = 0 
               THEN (* Go into RbmStateDescend *) 
                 IF RbmPrevTok # LbeStd . Tok__BegOfLine 
                 THEN 
                  RbmTeSqueezeTrailingTempMarks 
                     ( RbmTokBegPos , LbeStd . LimitedCharNoInfinity ) 
                 END (* IF *) 
               ; RbmCharPos := LbeStd . LimitedCharNoUnknown 
               ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
               ; RbmEstListChildrenToPass := 0 (* Dead *) 
               ; RbmReenterStateDescend ( ) 
               ELSE (* Keep going forward. *)  
                 DEC ( RbmLineBreakCt ) 
               ; IF TravUtil . DoTakeLineBreak 
                      ( Lang := ParseInfo . PiLang 
                      , CharPos := RbmCharPos 
                      , ModTextIsToLeftOnLine := FALSE 
                      , PrevTok := RbmPrevTok 
                      , RootFsNodeRef := RootFsNodeRef 
                      , LineBreakFsNodeRef := FsNodeRef 
                      , ParentFmtKind := FsApproxFmtKind 
                      , CurrentLineIndentPos := RbmTeIndentPos 
                      , EstTravInfo := RbmTeEstTravInfo 
                      , (* IN OUT *) LastFmtNoOnLine := RbmLastFmtNoOnLine  
                      , (* IN OUT *) EstListChildrenToPass 
                          := RbmEstListChildrenToPass 
                      ) 
                 THEN (* Stay in State RbmStateFwdNl, but otherwise treat the 
                         line break as taken. *) 
                   IF RbmPrevTok # LbeStd . Tok__BegOfLine 
                   THEN 
                     RbmTeSqueezeTrailingTempMarks 
                       ( RbmTokBegPos , LbeStd . LimitedCharNoInfinity ) 
                   END (* IF *) 
                 ; RbmCharPos := 0 
                 ; RbmPrevTok := LbeStd . Tok__BegOfLine 
                 ; RbmPrevTokToPos := 0 
                 ; RbmTokBegPos := 0 
                 ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
                 ; RbmEstListChildrenToPass := 0 (* Dead *) 
                 ; RbmTeTfsBuildFmtNoMark ( ) 
                 END (* IF *) 
               ; RbmTeIsFirstLine := FALSE 
               ; RbmTeIndentPos := EstIndentPosN 
               END (* IF *) 
            ELSE 
              CantHappen ( AFT . A_RbmTeTfsLineBreak_BadState ) 
            END (* CASE *) 
          END RbmTeTfsLineBreak 

      ; PROCEDURE RbmTeTfsFsSubtree ( ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR FssChildFmtKind : FmtKindTyp 
        ; VAR FssOrigChildFmtKind : FmtKindTyp 

        ; PROCEDURE RbmTeTfsFssRecomputeFmtKind ( )  
          RAISES { Backout } 
          (* A callback.  Called when we start traversing fwd (which becomes 
             known at a leaf site).  Uses environment of the Fs subtree to 
             compute its FmtKind.  These callbacks are laced through and 
             recurse from the leaf site back up to the root, then return 
             downward.
          *) 

          = VAR LChildFmtKind : FmtKindTyp 

          ; BEGIN (* RbmTeTfsFssRecomputeFmtKind *)  

            (* Upward pass to root: *) 
              IF FssChildFmtKind = FmtKindVert 
              THEN (* Our subtree is vertical. *)  
                IF FsApproxFmtKind = FmtKindUnknown 
                THEN (* Propagate now-known vertical up to parent.  *) 
                  FsApproxFmtKind := FmtKindVert 
                ; INC ( RbmFsUpFmtKindCt ) 
                ELSE (* Consistency check. *) 
                  Assert 
                    ( FsApproxFmtKind = FmtKindVert 
                    , AFT . AF_RbmTeTfsFssRecomputeFmtKind_InconsistentFsVertFromBelow
                    ) 
                END (* IF *)
              END (* IF *)

            (* Recurse upward toward root. *) 
            ; FsRecomputeFmtKindProc ( ) 
              (* ^Could change FsApproxFmtKind of _parent_. *) 

            (* Downward pass: *) 
            ; IF RbmState = RbmStateFwdNl  
              THEN (* Try again to compute FmtKind at this level. *)
                LChildFmtKind 
                  := TravUtil . FmtKindForFsSubtreeDescending 
                       ( ParseInfo . PiLang 
                       , RootFsNodeRef 
                       , FsNodeRef 
                       , FsApproxFmtKind 
                       , EstIndentPosN 
                       , RbmTeEstTravInfo 
                       , StartMark := RbmCurrentLineTokMark 
                       , StartMarkIsKnownNl := TRUE 
                       )
                (* ^Even though we are now traversing forward and know a 
                    CharPos, we don't know that we are at the left edge of the 
                    Est, so we have to use *Descending, not *Traversing. *) 
              ; IF FssChildFmtKind = FmtKindUnknown 
                THEN 
                  IF LChildFmtKind # FmtKindUnknown 
                  THEN (* Our subtree just became known. *) 
                    FssChildFmtKind := LChildFmtKind 
                  ; INC ( RbmFsFwdFmtKindCt ) 
                  END (* IF *) 
                ELSE (* Consistency check. *) 
                  Assert  
                    ( LChildFmtKind = FssChildFmtKind
                    , AFT . AF_RbmTeTfsFssRecomputeFmtKind_InconsistentRecomputedFmtKind
                    ) 
                END (* IF *)      
              END (* IF *) 
            END RbmTeTfsFssRecomputeFmtKind

        ; BEGIN (* RbmTeTfsFsSubtree *) 
          (* DECLARE *) 
            VAR LInitialFsChildNo : LangUtil . FsChildNoSignedTyp 
          ; VAR LFsChildCt : LangUtil . FsChildNoTyp 

          ; BEGIN (* Block for RbmTeTfsFsSubtree *) 
              IF FsNodeRef . FsChildren = NIL 
              THEN LFsChildCt := 0 
              ELSE LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ ) 
              END (* IF *) 
            ; CASE RbmState 
              OF RbmStateDescend 
              => FssChildFmtKind 
                 (* := RbmTeTfsFmtKindForFsSubtreeDescending ( ) *) 
                   := TravUtil . FmtKindForFsSubtreeDescending 
                        ( ParseInfo . PiLang 
                        , RootFsNodeRef 
                        , FsNodeRef 
                        , FsApproxFmtKind 
                        , EstIndentPosN 
(* CHECK^ On this. *) 
                        , RbmTeEstTravInfo 
                        , StartMark 
                            := ParseInfo . PiFinalTempMarkListRef ^ 
                                 [ RbmTempMarkElemNo ]  
                               . TokMark 
                        , StartMarkIsKnownNl := FALSE 
                        )
              ; IF FssChildFmtKind # FmtKindUnknown
                THEN INC ( RbmFsDescendFmtKindCt ) 
                END (* IF *)  
              ; LInitialFsChildNo 
                  := LangUtil . FsChildNoOfFmtNo ( FsNodeRef , RbmTeFmtNo ) 

              | RbmStateBwdNl 
              => FssChildFmtKind := FmtKindUnknown 
                 (* ^Conservative.  We don't know for certain. *)    
              ; LInitialFsChildNo := LFsChildCt - 1 

              | RbmStateFwdNl 
              => FssChildFmtKind 
                   := TravUtil . FmtKindForFsSubtreeTraversing 
                        ( ParseInfo . PiLang 
                        , RbmCharPos 
                        , (* ModTextIsToLeftOnLine := *) FALSE 
                        , RbmPrevTok 
                        , RootFsNodeRef 
                        , FsNodeRef 
                        , FsApproxFmtKind 
                        , RbmTeIndentPos 
                        , RbmTeEstTravInfo 
                        , (* IN OUT *) RbmLastFmtNoOnLine 
                        , (* IN OUT *) RbmEstListChildrenToPass 
                        )
              ; LInitialFsChildNo := 0 
              END (* CASE *) 
            ; FssOrigChildFmtKind := FssChildFmtKind (* For debug help. *) 
            ; RbmTeTraverseFsFixedChildren 
                ( FsNodeRef 
                , (* IN OUT *) ApproxFmtKind := FssChildFmtKind 
                , RecomputeFmtKindProc := RbmTeTfsFssRecomputeFmtKind 
                , FsChildCt := LFsChildCt 
                , InitialFsChildNo := LInitialFsChildNo 
                ) 
            END (* Block. *) 
          END RbmTeTfsFsSubtree

      ; PROCEDURE RbmTeTfsTraverseCondFmtChildren ( ) 
        RAISES { Backout , Thread . Alerted } 

        = VAR LFsChildCt : LangUtil . FsChildNoTyp 
        ; VAR LFsChildNo : LangUtil . FsChildNoTyp 
        ; VAR LFsChildNoSigned : LangUtil . FsChildNoSignedTyp 
        ; VAR LPredicate : BOOLEAN 

        ; BEGIN (* RbmTeTfsTraverseCondFmtChildren *) 
            LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ ) 
          ; CASE RbmState 
            OF RbmStateDescend 

            => TravUtil . DescendCondFmt 
                 ( Lang := RbmImagePers . IpLang 
                 , FsNodeRef := FsNodeRef 
                 , FmtNo := RbmTeFmtNo 
                 , EstTravInfo := RbmTeEstTravInfo 
                 , (* VAR *) Predicate := LPredicate 
                 , (* VAR *) FsChildNo := LFsChildNo 
                 (* Descending, we are in Fwd invariant on EstTravInfo. *) 
                 ) 
            ; LFsChildNoSigned := LFsChildNo 

            | RbmStateFwdNl 
            => LPredicate 
                 := TravUtil . DoCondFmtFwd 
                      ( RbmImagePers . IpLang 
                      , RbmTeEstTravInfo , FsNodeRef 
                      ) 
            ; LFsChildNoSigned := 0 

            | RbmStateBwdNl 
            => LPredicate 
                 := TravUtil . DoCondFmtBwd 
                      ( RbmImagePers . IpLang 
                      , RbmTeEstTravInfo , FsNodeRef 
                      ) 
            ; LFsChildNoSigned := LFsChildCt - 1 
            END (* CASE *) 
          ; IF LPredicate 
            THEN 
              RbmTeTraverseFsFixedChildren 
                ( FsNodeRef 
                , (* IN OUT *) ApproxFmtKind := FsApproxFmtKind 
                , RecomputeFmtKindProc := FsRecomputeFmtKindProc   
                , FsChildCt := LFsChildCt 
                , InitialFsChildNo := LFsChildNoSigned
                ) 
            ELSE 
              RbmTeTraverseFs  
                ( FsNodeRef . FsCondAltRef 
                , (* IN OUT *) FsApproxFmtKind := FsApproxFmtKind 
                , FsRecomputeFmtKindProc := FsRecomputeFmtKindProc 
                ) 
            END (* IF *) 
          END RbmTeTfsTraverseCondFmtChildren  

      ; TYPE WhatNextTyp = { LeadingMods , FsItem , TrailingMods } 

      ; PROCEDURE RbmTeTfsWhatNextForDescend ( ) : WhatNextTyp 

        = BEGIN   
            WITH WTempMark 
                 = ParseInfo . PiFinalTempMarkListRef <* NOWARN *> 
                   ^ [ RbmTempMarkElemNo ] 
            DO 
              IF RbmTeEstTravInfo . EtiChildNo 
                 >= RbmTeEstTravInfo . EtiChildCt 
                 OR RbmTeEstTravInfo . EtiChildNo < 0 
              THEN RETURN WhatNextTyp . FsItem 
              ELSIF WTempMark . TokMark . TkmKind = MarkKindTyp . RightSibFmtNo 
                    AND WTempMark . TokMark . TkmEstRef = RbmTeRMChildRef 
              THEN RETURN WhatNextTyp . FsItem 
              ELSIF WTempMark . TokMark . TkmKind = MarkKindTyp . LeftSibFmtNo 
                    AND WTempMark . TokMark . TkmEstRef 
                        = RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef
              THEN RETURN WhatNextTyp . FsItem 
              ELSIF WTempMark . TokMark . TkmKind = MarkKindTyp . ChildFmtNo 
                    AND WTempMark . TokMark . TkmEstRef 
                        = RbmTeEstTravInfo . EtiParentRef
              THEN RETURN WhatNextTyp . FsItem 
              ELSE 
                TYPECASE <* NOWARN *> 
                  RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                OF ModHs . EstDummyTyp (* Including NIL. *)
(* CHECK ^Is this right?  Once we got here with the est child zero pointing
          to a dummy, with child kind contains temp mark, but no temp mark
          pointing there.  What does this mean? *)
                => RETURN WhatNextTyp . FsItem 
                | SharedStrings . T ( TString ) 
                  => IF SharedStrings . Tok ( TString ) 
                        = LbeStd . Tok__LexErrChars 
                     THEN RETURN WhatNextTyp . LeadingMods 
                     ELSE RETURN WhatNextTyp . FsItem 
                     END (* IF *) 
                | EstHs . EstRefTyp ( TEstRef ) 
                => IF TEstRef . EstNodeKind 
                      = EstHs . EstNodeKindTyp . EstNodeKindModTok 
                   THEN RETURN WhatNextTyp . LeadingMods 
                   ELSE RETURN WhatNextTyp . FsItem 
                   END (* IF *) 
                ELSE
                  CASE WTempMark . TokMark . TkmKind <* NOWARN *> 
                  OF MarkKindTyp . LeftSibFmtNo 
                  , MarkKindTyp . RightSibFmtNo 
                  , MarkKindTyp . ChildFmtNo 
                  => RETURN WhatNextTyp . FsItem 

                  | MarkKindTyp . Plain 
                  , MarkKindTyp . BlankLine 
                  => TYPECASE <* NOWARN *> 
                       RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef
                     OF ModHs . ModCmntTrailingTyp 
                     => RETURN WhatNextTyp . TrailingMods 
                     | ModHs . ModRefTyp 
                     => RETURN WhatNextTyp . LeadingMods 
                     END (* TYPECASE  *) 
                  END (* CASE *) 
                END (* IF *) 
              END (* IF *) 
            END (* WITH *) 
          END RbmTeTfsWhatNextForDescend 

      ; BEGIN (* RbmTeTraverseFs *)
          (* DECLARE *) 
          VAR LWhatNext : WhatNextTyp 
        ; VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* Block *) 
            CASE FsNodeRef . FsKind <* NOWARN *>  

            (* Cases that can have mods, leading and/or trailing. *) 
            OF FsKindTyp . FsKindBegOfImage 
            , FsKindTyp . FsKindEndOfImage 
            , FsKindTyp . FsKindInsTok 
            , FsKindTyp . FsKindEstChildOfFixed 
            , FsKindTyp . FsKindEstChildOfList 
            , FsKindTyp . FsKindLineBreakOpt 
            , FsKindTyp . FsKindLineBreakReqd 
            => CASE RbmState 
               OF RbmStateDescend 
               => LWhatNext := RbmTeTfsWhatNextForDescend ( ) 
               ; LDelete := FALSE 
               ; LIsRepair := FALSE 
               | RbmStateBwdNl 
               => LWhatNext := WhatNextTyp . TrailingMods 
               | RbmStateFwdNl 
               => LWhatNext := WhatNextTyp . LeadingMods 
               END (* CASE *) 
            ; LOOP (* while sashaying between leading mods, the real Fs item, 
                      and trailing mods, in alternating directions. *) 
                CASE LWhatNext 

                (* Leading mods, in whichever direction, are next. *) 
                OF WhatNextTyp . LeadingMods 
                => RbmTeTfsLeadingModsNoDel ( ) 
                ; CASE RbmState 
                  OF RbmStateDescend 
                  => EXIT 
                  | RbmStateBwdNl 
                  => EXIT 
                  | RbmStateFwdNl 
                  => RbmTeTfsFwdLeadingDel 
                       ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
                  ; LWhatNext := WhatNextTyp . FsItem 
                  END (* CASE *) 

                (* The format syntax item, in whichever direction, is next. *) 
                | WhatNextTyp . FsItem 
                  (* INVARIANT: LDelete and LIsRepair are properly set. *) 
                => IF LDelete 
                   THEN 
                     Assert 
                       ( FsNodeRef . FsKind IN LangUtil . FsKindSetDeletable
                         AND LIsRepair 
                             (* Since it's a repair, we will include it. *)  
                       , AFT . A_RbmTeTraverseFs_DeleteBadFsKind 
                       ) 
                   END (* IF *) 
                 (* The format syntax item is never treated as deleted. *) 
                 ; CASE FsNodeRef . FsKind 

                   (* Beginning of image. *) 
                   OF FsKindTyp . FsKindBegOfImage 
                   => Assert 
                        ( FsApproxFmtKind # FmtKindUnknown 
                          (* ^Initial startup of Rbm traversal for whole 
                             tree must supply a known FmtKind. *) 
                        , AFT . A_RbmTeTfs_UnkFmtKindAtBol 
                        ) 
                   ; Assert 
                       ( RbmState 
                         IN RbmStateSetTyp { RbmStateBwdNl , RbmStateDescend } 
                       , AFT . A_RbmTeTraverseFs_BOIBadState 
                       ) 
                   ; RbmEstRefForTrailingMarks := NIL 
                   ; RbmCharPos := 0 
                   ; RbmPrevTok := LbeStd . Tok__BegOfLine 
                   ; RbmPrevTokToPos := 0 
                   ; RbmTokBegPos := 0 
                   ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
                   ; RbmEstListChildrenToPass := 0 (* Dead *) 
                   ; RbmTeIsFirstLine := TRUE  
                   ; RbmTeIndentPos := EstIndentPos1 
                   ; CASE RbmState <* NOWARN *> 
                     OF RbmStateDescend
                     => 
                     | RbmStateBwdNl 
                     => RbmTeIncEstChild ( ) 
                     END (* CASE *) 
                   ; RbmTeTfsEnterStateFwdNl ( ) 
                   ; RbmTeTfsBuildFmtNoMark ( ) 

                   (* End of Image. *) 
                   | FsKindTyp . FsKindEndOfImage 
                   => CASE RbmState <* NOWARN *> 
                      OF RbmStateDescend
                      => RbmTeTfsBuildFmtNoMark ( ) 
                      ; RbmTeTfsEnterStateFwdNl ( ) 

                      | RbmStateFwdNl 
                      =>  

                      ELSE 
                        CantHappen ( AFT . A_RbmTeTraverseFs_EOIBadState ) 
                      END (* CASE *) 
                   ; RbmTeSqueezeTrailingTempMarks 
                       ( RbmTokBegPos , LbeStd . LimitedCharNoInfinity ) 
                   ; IF RbmTempMarkElemNo < RbmTempMarkElemCt 
                     THEN 
                       RbmEstRefForTrailingMarks := RbmTeRMChildRef 
                     ; RbmTeSqueezeTrailingTempMarks  
                         ( 0 , LbeStd . LimitedCharNoInfinity ) 
                     END (* IF *) 
                   ; RbmTokBegPos := 0 (* Dead? *)  
                   ; RbmCharPos := 0 (* Dead? *)  
                   ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown (* Dead? *) 
                   ; RbmEstListChildrenToPass := 0 (* Dead *) 
(* CHECK: Is the FmtNoMark we recently built OK here? *) 
                   ; RbmTeTfsEnterStateFwdNl ( ) 
                   ; EXIT 

                   (* InsTok. *) 
                   | FsKindTyp . FsKindInsTok 
                   => RbmTeTfsInsTok ( ) 
                   
                   (* Est child cases. *) 
                   | FsKindTyp . FsKindEstChildOfFixed 
                   => RbmTeTfsEstChild ( IsEstList := FALSE ) 

                   | FsKindTyp . FsKindEstChildOfList 
                   => RbmTeTfsEstChild ( IsEstList := TRUE ) 

                   (* Line breaks. *) 
                   | FsKindTyp . FsKindLineBreakOpt 
                   , FsKindTyp . FsKindLineBreakReqd  
                   => Assert 
                        ( NOT LDelete 
                        , AFT . A_RbmTeTraverseFs_DeletedLineBreak 
                        ) 
                   ; RbmTeTfsLineBreak ( FsNodeRef . FsKind ) 
                   ELSE 
                     CantHappen ( AFT . A_RbmTeTraverseFs_BadFsKindWithMods ) 
                   END (* CASE FsKind *) 
                ; CASE RbmState 
                  OF RbmStateDescend 
                  => EXIT 
                  | RbmStateBwdNl 
                  => LWhatNext := WhatNextTyp . LeadingMods 
                  | RbmStateFwdNl 
                  => LWhatNext := WhatNextTyp . TrailingMods 
                  END (* CASE *) 

                (* Trailing mods, in whichever direction, are next. *) 
                | WhatNextTyp . TrailingMods 
                => RbmTeTfsTrailingMods ( ) 
                ; CASE RbmState 
                  OF RbmStateDescend 
                  , RbmStateFwdNl 
                  => EXIT 
                  | RbmStateBwdNl 
                  => RbmTeTfsBwdLeadingDel 
                       ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
                  ; LWhatNext := WhatNextTyp . FsItem 
                  END (* CASE *) 
                END (* CASE LWhatNext *) 
              END (* LOOP *) 

            (* Subtree nodes. *) 
            | FsKindTyp . FsKindSubtreeVert 
            , FsKindTyp . FsKindSubtreeHoriz 
            , FsKindTyp . FsKindSubtreeFill 
            => RbmTeTfsFsSubtree ( ) 

            (* Conditional format. *) 
            | FsKindTyp . FsKindCondFmt 
            => RbmTeTfsTraverseCondFmtChildren ( ) 
            END (* CASE *) 
          END (* Block *) 
        END RbmTeTraverseFs 

    ; PROCEDURE RbmTeComputeEstAndFmtNoForDescend ( ) 
      RAISES { Backout } 
      (* Ascertain whether a marked descendent exists in this 
         Est subtree, (set RbmTeDoneWEst to NOT exists), and if it exists, 
         the format number which leads toward it (set RbmTeFmtNo to this). 
         Also set RbmTeEstTravInfo for the correct Est child. 
      *) 

(* REVIEW:  The code in RbmTeTfsWhatNextForDescend tests things in a 
            different order than in MteTeTfsWhatNextForDescend.  The
            problem is somewhat different, because this uses temp marks
            and that uses regular marks.  It interacts with computing the
            Est child, done earlier here in RbmTeComputeEstAndFmtNoForDescend,
            and in MteTraverseEst.  Compare these and see if some 
            consistifying is called for. 
*) 

      = BEGIN (* RbmTeComputeEstAndFmtNoForDescend *) 
          RbmTeFmtNo := EstHs . FmtNoNull (* Dead. *) 
        ; IF RbmTempMarkElemNo >= RbmTempMarkElemCt 
          THEN RbmTeDoneWEst := TRUE 
          ELSE 
            RbmTeDoneWEst := FALSE 
          ; WITH 
              WTempMark 
              = ParseInfo . PiFinalTempMarkListRef ^ [ RbmTempMarkElemNo ] 
            DO 
              IF RbmTeEstTravInfo . EtiNodeRef = WTempMark . TokMark . TkmEstRef 
              THEN (* The current temp mark denotes the Est parent node. *) 
                CASE WTempMark . TokMark . TkmKind 
                OF MarkKindTyp . Plain 
                , MarkKindTyp . BlankLine 
                => IF RbmTeEstTravInfo . EtiParentRef # NIL
                      AND RbmTeEstTravInfo . EtiParentRef . EstNodeKind
                          = EstHs . EstNodeKindTyp . EstNodeKindModTok
                   THEN
                     (* This should no longer happen.  This case should be
                        marked as *FmtNo. *)
                     CantHappen
                       ( AFT . AF_RbmTeComputeEstAndFmtNoForDescend_temp_makr_on_mod_tok ) 
                   ; RbmTeFmtNo := EstHs . FmtNoModTok (* Probably dead. *) 
                   ELSE
                     Assert 
                       ( RbmTeEstTravInfo . EtiParentRef = NIL 
                       , AFT . A_RbmTeComputeEstAndFmtNoForDescend_NotLeaf
                       ) 
                   ; RbmTeFmtNo := EstHs . FmtNoNull (* Defensive. *)
                   END (* IF *) 

                | MarkKindTyp . RightSibFmtNo 
                => (* This can happen if we went back to RbmStateDescend,
                      while inside this Est subtree. 
                   *) 
                  RbmTeDoneWEst := TRUE 

                | MarkKindTyp . ChildFmtNo 
                => TravUtil . GetLMEstChild ( RbmTeEstTravInfo ) 
                ; TravUtil . SetToChildWGreaterFmtNo
                    ( RbmTeEstTravInfo , WTempMark . TokMark . TkmFmtNo ) 
                ; RbmTeFmtNo := WTempMark . TokMark . TkmFmtNo 
                  (* ^Arrange to visit the insertion token. *)
                ELSE 
                  CantHappen 
                    ( AFT . A_RbmTeComputeEstAndFmtNoForDescend_BadMarkKind ) 
                  (* ^For a LeftSib mark, we would not descend into its tree. *) 
                END (* CASE *) 
              ELSIF RbmTeEstTravInfo . EtiChildCt <= 0 
              THEN 
                RbmTeDoneWEst := TRUE 
              ELSE (* Nonempy Est child list. *) 
                IF ( RbmTeEstTravInfo . EtiChildNo 
                     >= RbmTeEstTravInfo . EtiChildCt
                     AND WTempMark . TokMark . TkmEstRef = RbmTeRMChildRef 
                   ) 
                THEN (* We are off the right end, but WTempMark points
                        to the rightmost child.  It must be RightSib. 
                        If we are ascending, it may already be unmarked. *)
                  Assert 
                    ( WTempMark . TokMark . TkmKind = MarkKindTyp . RightSibFmtNo
                    , AFT . A_RbmTeComputeEstAndFmtNoForDescend_Youngest_not_RightSib 
                    ) 
                ; RbmTeFmtNo := WTempMark . TokMark . TkmFmtNo 
                ELSIF WTempMark . TokMark . TkmEstRef 
                      = RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef
                      AND WTempMark . TokMark . TkmKind = MarkKindTyp . LeftSibFmtNo
                THEN (* WTempMark points to the next elder child and is
                        LeftSib.  This can only happen when descending. *)   
                  RbmTeFmtNo := WTempMark . TokMark . TkmFmtNo 
                ELSE 
                  RbmTeSetToNextTempMarkedChild ( )
                ; IF RbmTeEstTravInfo . EtiChildNo 
                     >= RbmTeEstTravInfo . EtiChildCt 
                  THEN 
                    RbmTeDoneWEst := TRUE 
                  ELSE 
                    IF RbmTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       # WTempMark . TokMark . TkmEstRef  
                    THEN (* Arrange to descend into the marked child. *) 
                      RbmTeFmtNo := RbmTeEstTravInfo . EtiChildFmtNo 
                    ELSE 
                      CASE WTempMark . TokMark . TkmKind 
                      OF MarkKindTyp . LeftSibFmtNo 
                      => RbmTeFmtNo := WTempMark . TokMark . TkmFmtNo 

                      | MarkKindTyp . RightSibFmtNo 
                      => RbmTeUnmarkCurrentChild ( ) 
                      ; RbmTeIncEstChild ( ) 
                      ; RbmTeFmtNo := WTempMark . TokMark . TkmFmtNo 

                      ELSE (* Arrange to descend into the marked child. *) 
                        RbmTeFmtNo := RbmTeEstTravInfo . EtiChildFmtNo 
                      END (* CASE *) 
                    END (* IF *) 
                  END (* IF *) 
                END (* IF *) 
              END (* IF *) 
            END (* WITH WTempMark *) 
          END (* IF *) 
        END RbmTeComputeEstAndFmtNoForDescend 

    ; PROCEDURE RbmMaybeNilRMDummy ( ) 
      RAISES { Backout } 

      = BEGIN 
          IF RbmTeEstTravInfo . EtiChildCt > 0 
             AND RbmState = RbmStateFwdNl 
          THEN 
            TYPECASE RbmTeRMChildRef 
            OF NULL => 
            | ModHs . EstDummyTempMarkTyp 
            => RbmTeRMChildRef := NIL (* Dead. *) 
            ; EstUtil . SetChildRef 
                ( EstRef := RbmTeEstRefToPatch 
                , ChildNo := RbmTeRMChildNoToPatch 
                , NewChildRef := NIL 
                ) 
            ; Assertions . MessageText 
                ( "NILed out rightmost DummyTempMark, in RebuildMarksList, Root node no "
                  & LbeStd . EstNodeNoImage ( EstAbsNodeNo ) 
                  & ", child no "
                  & LbeStd . EstChildNoImage 
                      ( RbmTeEstTravInfo . EtiChildCt - 1 ) 
                ) 
            ELSE 
            END (* TYPECASE *) 
          END (* IF *) 
        END RbmMaybeNilRMDummy

    ; PROCEDURE RbmTeInitRMFsChildNo ( FsChildCt : LangUtil . FsChildNoTyp ) 

      = BEGIN 
          IF RootFsNodeRef . FsKind IN LangUtil . FsKindSetEstListTrail 
             AND RbmTeEstTravInfo . EtiParentRef . EstNodeKind 
                 = EstHs . EstNodeKindTyp . EstNodeKindTrail
          THEN RbmTeRMFsChildNo := FsChildCt - 1 
          ELSE RbmTeRMFsChildNo := 0 
          END (* IF *) 
        END RbmTeInitRMFsChildNo 

    ; BEGIN (* RbmTraverseEst *) 
        VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LInitialFsChildNo : LangUtil . FsChildNoSignedTyp 

      ; BEGIN (* Block for RbmTraverseEst *) 
          IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        ; IF EstRef # NIL 
          THEN 
            RbmTeRMChildRef := NIL 
          ; RbmTeRMChildRelNodeNo := LbeStd . EstNodeNoNull 
          ; RbmTeRMChildKindSet := EstHs . EstChildKindSetEmpty 
          ; RbmTeFmtNo := 0 
          ; RbmTeDoneWEst := FALSE 
          ; IF RootFsNodeRef = NIL OR RootFsNodeRef . FsChildren = NIL 
            THEN LFsChildCt := 0 
            ELSE LFsChildCt := NUMBER ( RootFsNodeRef . FsChildren ^ ) 
            END (* IF *) 
          ; CASE RbmState 
            OF RbmStateDescend 
            => RbmTeInitEstTravInfoFwd ( EstRef , KindSet , EstAbsNodeNo )
            ; RbmTeInitRMFsChildNo ( LFsChildCt ) 
            ; RbmTeComputeEstAndFmtNoForDescend ( ) 
            ; IF RbmTeDoneWEst 
              THEN RETURN 
              ELSE 
                LInitialFsChildNo 
                  := LangUtil . FsChildNoOfFmtNo ( RootFsNodeRef , RbmTeFmtNo ) 
              END (* IF *) 

            | RbmStateBwdNl 
            => RbmTeInitEstTravInfoBwd ( EstRef , KindSet , EstAbsNodeNo ) 
            ; RbmTeInitRMFsChildNo ( LFsChildCt ) 
            ; CASE RootFsNodeRef . FsKind <* NOWARN *> 

              (* Est fixed nodes. *) 
              OF FsKindTyp . FsKindEstFixedVert 
              , FsKindTyp . FsKindEstFixedHoriz 
              , FsKindTyp . FsKindEstFixedFill 
              => LInitialFsChildNo := LFsChildCt - 1 

              (* Plain Est list nodes. *) 
              | FsKindTyp . FsKindEstListVert 
              , FsKindTyp . FsKindEstListHoriz 
              , FsKindTyp . FsKindEstListFill 
              => LInitialFsChildNo := 0 

              (* Est list nodes with possibly trailing separators.  *) 
              | FsKindTyp . FsKindEstListTrailVert 
              , FsKindTyp . FsKindEstListTrailHoriz 
              , FsKindTyp . FsKindEstListTrailFill 
              => LInitialFsChildNo := RbmTeRMFsChildNo 

              (* Ast string, *) 
              | FsKindTyp . FsKindAstString 
              => RbmTeLeafString 
                   ( RootFsNodeRef 
                   , EstApproxFmtKind 
                   , RbmTeEstTravInfo . EtiStringRef 
                   ) 

           (* ELSE Cant happen. *) 
              END (* CASE *) 

            | RbmStateFwdNl 
            => RbmTeInitEstTravInfoFwd ( EstRef , KindSet , EstAbsNodeNo )
            ; RbmTeInitRMFsChildNo ( LFsChildCt ) 
            ; RbmTeInitNodeToPatch ( ) 
            ; LInitialFsChildNo := 0 
            ; RbmTeIsFirstLine := TRUE  
            ; RbmTeIndentPos := EstIndentPos1 

            END (* CASE *) 

          ; LOOP 
(* TODO: Consider taking this case outside the loop. Would have to duplicate
         some steps. *) 
              CASE RootFsNodeRef . FsKind <* NOWARN *> 

              (* Est fixed nodes. *) 
              OF FsKindTyp . FsKindEstFixedVert 
              , FsKindTyp . FsKindEstFixedHoriz 
              , FsKindTyp . FsKindEstFixedFill 
              => RbmTeTraverseFsFixedChildren 
                   ( RootFsNodeRef 
                   , (* IN OUT *) ApproxFmtKind := EstApproxFmtKind 
                   , RecomputeFmtKindProc := EstRecomputeFmtKindProc  
                   , FsChildCt := LFsChildCt 
                   , InitialFsChildNo := LInitialFsChildNo
                   ) 
              ; RbmMaybeNilRMDummy ( ) 

              (* Est list nodes. *) 
              | FsKindTyp . FsKindEstListVert 
              , FsKindTyp . FsKindEstListHoriz 
              , FsKindTyp . FsKindEstListFill 
              , FsKindTyp . FsKindEstListTrailVert 
              , FsKindTyp . FsKindEstListTrailHoriz 
              , FsKindTyp . FsKindEstListTrailFill 
              => RbmTeTraverseFsListChildren 
                   ( RootFsNodeRef 
                   , (* IN OUT *) ApproxFmtKind := EstApproxFmtKind 
                   , RecomputeFmtKindProc := EstRecomputeFmtKindProc  
                   , FsChildCt := LFsChildCt 
                   , InitialFsChildNo := LInitialFsChildNo 
                   ) 
              ; RbmMaybeNilRMDummy ( ) 

              (* Ast string, *) 
              | FsKindTyp . FsKindAstString 
              => RbmTeLeafString 
                   ( RootFsNodeRef 
                   , EstApproxFmtKind 
                   , RbmTeEstTravInfo . EtiStringRef
                   ) 

           (* ELSE Can't happen. *) 
              END (* CASE *) 
            ; IF RbmTempMarkElemNo >= RbmTempMarkElemCt 
              THEN 
                EXIT 
              ELSE 
                CASE RbmState 
                OF RbmStateDescend 
                => RbmTeComputeEstAndFmtNoForDescend ( ) 
                ; IF RbmTeDoneWEst 
                  THEN EXIT 
                  ELSE 
                    LInitialFsChildNo 
                      := LangUtil . FsChildNoOfFmtNo 
                           ( RootFsNodeRef , RbmTeFmtNo ) 
                  END (* IF *) 

                | RbmStateFwdNl 
                , RbmStateBwdNl 
                => EXIT  
                END (* CASE *) 
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *) 
        ; IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        END (* Block *) 
      END RbmTraverseEst 

  ; BEGIN (* RebuildMarkList *) 
      VAR LChildFmtKind : FmtKindTyp 

    ; BEGIN (* Block for RebuildMarkList *) 
        IF NewEstRef # NIL 
        THEN 
          RbmImagePers := ImageRef . ItPers 
        ; RbmTempMarkElemCt := NUMBER ( ParseInfo . PiFinalTempMarkListRef ^ ) 
        ; Assert 
            ( RbmTempMarkElemCt > 0 
            , AFT . A_RebuildMarkList_EmptyTempMarkList 
            ) 
        ; RbmTempMarkElemNo := 0 
        ; RbmDescendTempMarkElemNo := 0 
        ; RbmLineMarkMeat := RbmImagePers . IpMarkHeader . LmRightLink 
        ; RbmState := RbmStateDescend 
        ; RbmCharPos := LbeStd . LimitedCharNoUnknown 
        ; RbmTokBegPos := 0 
        ; RbmPrevTok := LbeStd . Tok__BegOfLine 
        ; RbmPrevTokToPos := 0 
        ; RbmCurrentLineTokMark := Marks . TokMarkNull 
        ; RbmEstRefForTrailingMarks := NIL 
        ; RbmLastFmtNoOnLine := EstHs . FmtNoUnknown  
        ; RbmEstListChildrenToPass := 0 (* Dead *) 
        ; RbmMaxLineBreakCt := 0 

        ; RbmEstDescendFmtKindCt := 0 
        ; RbmEstUpFmtKindCt := 0 
        ; RbmEstFwdFmtKindCt := 0 
        ; RbmFsDescendFmtKindCt := 0 
        ; RbmFsUpFmtKindCt := 0 
        ; RbmFsFwdFmtKindCt := 0 

        ; LChildFmtKind := FmtKindVert 
        ; RbmTraverseEst 
            ( NewEstRef 
            , EstHs . EstChildKindSetEmpty  
            , EstChildNo := 0 
            , GrandparentEstRef := NIL 
            , EstAbsNodeNo := 0           
            , RootFsNodeRef 
                := EstUtil . FsRuleForEstNode ( ParseInfo . PiLang , NewEstRef )
            , (* IN OUT *) EstApproxFmtKind := LChildFmtKind 
            , EstRecomputeFmtKindProc := ProcNoop  
            , EstIndentPos1 := Options . InitialIndentPos 
            , EstIndentPosN := Options . InitialIndentPos 
            ) 
        ; Assertions . MessageText 
            ( "Rbm FmtKind computations: EstDescend = " 
              & Fmt . Int ( RbmEstDescendFmtKindCt )
              & ", EstUp = " 
              & Fmt . Int ( RbmEstUpFmtKindCt )
              & ", EstFwd = " 
              & Fmt . Int ( RbmEstFwdFmtKindCt )
              & ", FsDescend = " 
              & Fmt . Int ( RbmFsDescendFmtKindCt )
              & ", FsUp = " 
              & Fmt . Int ( RbmFsUpFmtKindCt )
              & ", FsFwd = " 
              & Fmt . Int ( RbmFsFwdFmtKindCt )
            ) 
        ; Assertions . MessageText 
            ( "RbmMaxLineBreakCt = " & Fmt . Int ( RbmMaxLineBreakCt ) ) 
        ; Assert 
            ( RbmTempMarkElemNo 
              >= RbmTempMarkElemCt 
            , AFT . A_RbmTeTraverseFs_Unrebuilt_marks
            ) 
        END (* IF *) 
      END (* Block *) 
    END RebuildMarkList 

(***************************************************************) 

(* VISIBLE *) 
; PROCEDURE UnmarkEst 
    ( EstRef : LbeStd . EstRootTyp 
    ; EstAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ) 
  RAISES { Backout , Thread . Alerted } 

  (* Remove any leftover temp mark mark bits in the Est. 
     This is needed for the OLD Est, after reparsing. 
  *) 

  = VAR LWasAlreadyUnmarked : BOOLEAN 
  ; VAR LEstTravInfo : TravUtil . EstTravInfoTyp 

  ; BEGIN (* UnmarkEst *) 
      TYPECASE EstRef 
      OF NULL 
      => 

      | EstHs . EstRefTyp ( TEstTreeRef ) 
      => LEstTravInfo . EtiParentRef := TEstTreeRef 
      ; LEstTravInfo . EtiChildCt := TEstTreeRef . KTreeChildCt ( ) 
      ; LEstTravInfo . EtiChildNo := 0 
      ; LOOP 
          IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        ; EstUtil . NextInKindSet 
            ( TEstTreeRef 
            , LEstTravInfo . EtiChildNo 
            , EstHs . EstChildKindSetTyp 
                { EstHs . EstChildKindContainsTempMark } 
            , (* VAR *) LEstTravInfo . EtiChildNo 
            , (* VAR *) LEstTravInfo . EtiChildRelNodeNo 
            , (* VAR *) LEstTravInfo . EtiChildLeafElem 
            ) 
        ; IF LEstTravInfo . EtiChildNo >= LEstTravInfo . EtiChildCt 
          THEN 
            EXIT 
          END (* IF *) 
        ; TYPECASE LEstTravInfo . EtiChildLeafElem . LeChildRef 
          OF NULL => 

          | ModHs . EstDummyTempMarkTyp 
          => (* Must change both the cached pointer and the original in Est. *) 
            LEstTravInfo . EtiChildLeafElem . LeChildRef := NIL 
          ; EstUtil . SetChildRef 
              ( EstRef := LEstTravInfo . EtiParentRef 
              , ChildNo := LEstTravInfo . EtiChildNo 
              , NewChildRef := NIL 
              ) 
          ; Assertions . MessageText 
              ( "Removed DummyTempMark, in UnmarkEst, ode no "
                & LbeStd . EstNodeNoImage 
                    ( EstAbsNodeNo + LEstTravInfo . EtiChildRelNodeNo ) 
              ) 

          ELSE 
          END (* TYPECASE *) 
        ; IF EstHs . EstChildKindContainsTempMark 
             IN LEstTravInfo . EtiChildLeafElem . LeKindSet 
          THEN 
            UnmarkEst 
              ( LEstTravInfo . EtiChildLeafElem . LeChildRef 
              , EstAbsNodeNo + LEstTravInfo . EtiChildRelNodeNo 
              ) 
          ; EstUtil . SetChildKindBitFALSE 
              ( TEstTreeRef 
              , LEstTravInfo . EtiChildNo 
              , EstHs . EstChildKindContainsTempMark 
              , LWasAlreadyUnmarked (* Dead. *) 
              ) 
(* TODO: The above is grossly inefficient.  Each call of SetChildKindBitFALSE 
         assumes this could be the only one of the children that will have 
         this bit negated.  It does level-by-level scan of each KTree node 
         to look for other bits that are not negated. 
         Write a better routine that knows it is turning off the same bit 
         in all nodes.  Probably, it should go in EstUtil. 
*) 
          END (* IF *) 
        ; INC ( LEstTravInfo . EtiChildNo ) 
        END (* LOOP *) 

      ELSE 
      END (* TYPECASE *) 
    END UnmarkEst 

(* EXPORTED: *) 
; PROCEDURE DisconnectMarksFromLinesRefs 
    ( ImageTrans : PaintHs . ImageTransientTyp ) 
  (* For every mark, remove its reference to a LinesRef. *) 
(* CHECK: Does this really belong here?  Maybe in Display? *) 

  = VAR LLineMarkMeat : PaintHs . LineMarkMeatTyp 

  ; BEGIN 
      LLineMarkMeat := ImageTrans . ItPers . IpMarkHeader . LmRightLink 
    ; LOOP 
        IF LLineMarkMeat . LmLinesRef # NIL 
        THEN 
          LLineMarkMeat . LmLinesRef . LrHasMark := FALSE 
        ; LLineMarkMeat . LmLinesRef := NIL 
        END (* IF *) 
      ; TYPECASE LLineMarkMeat . LmRightLink 
        OF PaintHs . LineMarkMeatTyp ( TRightLink ) 
        => LLineMarkMeat := TRightLink 
        ELSE 
          EXIT 
        END (* TYPECASE *) 
      END (* LOOP *) 
    END DisconnectMarksFromLinesRefs 

(* EXPORTED: *) 
; PROCEDURE SavePermanentMarks 
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; VAR SavedMarkListRef : ParseHs . TempMarkArrayRefTyp 
    ) 
  RAISES { Backout } 

  = VAR LElemCt : LbeStd . MarkNoTyp 
  ; VAR LElemNo : LbeStd . MarkNoTyp 
  ; VAR LLineMarkMeat : PaintHs . LineMarkMeatTyp 

  ; BEGIN 
      LElemCt := ImageTrans . ItPers . IpMarkCt 
    ; SavedMarkListRef := NEW ( ParseHs . TempMarkArrayRefTyp, LElemCt )
    ; IF LElemCt > 0 THEN SavedMarkListRef ^ [ 0 ] . SeqNo := 0 END (* IF *)
    ; LElemNo := 0  
    ; LLineMarkMeat := ImageTrans . ItPers . IpMarkHeader . LmRightLink 
    ; LOOP 
        WITH WTempMark = SavedMarkListRef ^ [ LElemNo ] 
        DO 
          WTempMark . TokMark := LLineMarkMeat . LmTokMark 
        ; WTempMark . LineNo := LLineMarkMeat . LmLineNo  
        ; WTempMark . CharPos  := LLineMarkMeat . LmCharPos 
        ; WTempMark . TokMark . TkmEstRef := NIL (* Dead. *) 
        ; INC ( LElemNo ) 
        ; IF LElemNo >= LElemCt 
          THEN 
            Assert 
              ( LLineMarkMeat . LmRightLink = ImageTrans . ItPers . IpMarkHeader
              , AFT . A_SavePermanentMarks_Low_IpMarkCt 
              ) 
          ; EXIT (* The normal exit. *) 
          ELSE
            TYPECASE LLineMarkMeat . LmRightLink 
            OF NULL 
            => CantHappen  
                ( AFT . A_RestorePermanentMarks_NIL_terminated_mark_list ) 
            ; EXIT 
            | PaintHs . LineMarkMeatTyp ( TRightLink ) 
            => LLineMarkMeat := TRightLink 
            ELSE 
              CantHappen 
                ( AFT . A_RestorePermanentMarks_High_IpMarkCt ) 
            ; EXIT 
            END (* TYPECASE *) 
          END (* IF *) 
        END (* WITH *) 
      END (* LOOP *) 
    END SavePermanentMarks 

(* EXPORTED: *) 
; PROCEDURE RestorePermanentMarks 
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; SavedMarkListRef : ParseHs . TempMarkArrayRefTyp 
    ) 

  = VAR LElemCt : LbeStd . MarkNoTyp 
  ; VAR LElemNo : LbeStd . MarkNoTyp 
  ; VAR LLineMarkMeat : PaintHs . LineMarkMeatTyp 

  ; BEGIN 
      LElemCt := NUMBER ( SavedMarkListRef ^ ) 
    ; LElemNo := 0  
    ; LLineMarkMeat := ImageTrans . ItPers . IpMarkHeader . LmRightLink 
    ; LOOP 
        WITH WTempMark = SavedMarkListRef ^ [ LElemNo ] 
        DO 
          LLineMarkMeat . LmTokMark := WTempMark . TokMark 
        ; LLineMarkMeat . LmLineNo := WTempMark . LineNo  
        ; LLineMarkMeat . LmCharPos := WTempMark . CharPos  
        ; PaintHs . RecomputeLrHasMark ( LLineMarkMeat ) 
        ; LLineMarkMeat . LmLinesRef := NIL 
        ; INC ( LElemNo ) 
        ; IF LElemNo >= LElemCt 
          THEN 
            IF ISTYPE 
                 ( LLineMarkMeat . LmRightLink , PaintHs . LineMarkMeatTyp ) 
            THEN 
              Assertions . MessageText 
                ( "RestorePermanentMarks: leftover regular marks." ) 
            END (* IF *) 
          ; EXIT 
          ELSE 
            TYPECASE LLineMarkMeat . LmRightLink 
            OF NULL 
            => Assertions . MessageText 
                ( "RestorePermanentMarks: NIL-terminated mark list." ) 
            ; EXIT 
            | PaintHs . LineMarkMeatTyp ( TRightLink ) 
            => LLineMarkMeat := TRightLink 
            ELSE 
              Assertions . MessageText 
                ( "RestorePermanentMarks: leftover saved marks." ) 
            ; EXIT 
            END (* TYPECASE *) 
          END (* IF *) 
        END (* WITH *) 
      END (* LOOP *) 
    END RestorePermanentMarks 

; BEGIN (* TempMark *) 
  END TempMark 
. 
