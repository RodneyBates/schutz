
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

 MODULE WriteTrv 

(* Procedures for traversing Ests and generating text or token streams. *) 

; IMPORT Fmt 
; IMPORT Text 
; IMPORT Thread 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LangUtil 
; FROM LangUtil IMPORT FsKindTyp 
; IMPORT LbeStd 
; IMPORT MessageCodes 
; IMPORT ModHs 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT SharedStrings 
; IMPORT Strings 
; IMPORT TravUtil 

; TYPE AFT = MessageCodes . T 

(* VISIBLE: *) 

; PROCEDURE WriteText 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; DeliverLineProc : DeliverLineProcTyp 
    ; DoGenerateText : BOOLEAN := TRUE 
    ; DoGenerateErrors : BOOLEAN := FALSE 
    ) 
    RAISES { AssertionFailure , Thread . Alerted } 

  = VAR WtxCharPos : LbeStd . LimitedCharNoTyp 
  ; VAR WtxPrevTok : LbeStd . TokTyp 
  ; VAR WtxModTextIsToLeftOnLine : BOOLEAN 
  ; VAR WtxLang : LbeStd . LangTyp 
  ; VAR WtxLineImage : Strings . StringTyp 
  ; VAR WtxLastFmtNoOnLine : EstHs . FmtNoTyp 
  ; VAR WtxEstListChildrenToPass : LbeStd . EstChildNoTyp 

  ; PROCEDURE WtxAccumChar 
      ( Ch : CHAR ) RAISES { AssertionFailure } 

    = BEGIN (* WtxAccumChar *) 
        IF DoGenerateText 
        THEN 
          Strings . AppendCharInPlace ( WtxLineImage , Ch ) 
        ; WtxCharPos := EstUtil . WidthSum ( WtxCharPos , 1 ) 
        END 
      END WtxAccumChar 

  ; PROCEDURE WtxBlankFillTo 
      ( ToCharPos : LbeStd . LimitedCharNoSignedTyp ) 
    RAISES { AssertionFailure } 

    = BEGIN (* WtxBlankFillTo *) 
        IF ToCharPos < LbeStd . LimitedCharNoInfinity 
           AND WtxCharPos < ToCharPos 
        THEN 
          WtxPrevTok := LbeStd . Tok__Sep 
        ; WHILE WtxCharPos < ToCharPos 
          DO WtxAccumChar ( ' ' ) 
          END (* WHILE *) 
        END (* IF *) 
      END WtxBlankFillTo 

  ; PROCEDURE WtxAccumString 
      ( StringRef : SharedStrings . T ) 
      RAISES { AssertionFailure } 

    = BEGIN (* WtxAccumString *) 
        IF DoGenerateText AND StringRef # NIL 
        THEN 
          Strings . AppendInPlace 
            ( WtxLineImage , SharedStrings . ToString ( StringRef ) ) 
        ; WtxCharPos 
            := EstUtil . WidthSum 
                 ( WtxCharPos , SharedStrings . Length ( StringRef ) ) 
        END (* IF *) 
      END WtxAccumString 

  ; PROCEDURE WtxAccumText 
      ( FText : TEXT ) RAISES { AssertionFailure } 

    = BEGIN (* WtxAccumText *) 
        IF DoGenerateText AND FText # NIL 
        THEN 
          Strings . AppendTextInPlace ( WtxLineImage , FText ) 
        ; WtxCharPos 
            := EstUtil . WidthSum ( WtxCharPos , Text . Length ( FText ) ) 
        END (* IF *) 
      END WtxAccumText 

  ; PROCEDURE WtxDeliverLine ( ) 
    RAISES { AssertionFailure , Thread . Alerted } 

    = BEGIN (* WtxDeliverLine *) 
        DeliverLineProc ( ImageRef , WtxLineImage ) 
      ; Assert 
          ( ( WtxCharPos <= LbeStd . LimitedCharNoMax ) 
            <= (* IMPLIES *) 
               ( NOT DoGenerateText 
                 OR Strings . Length ( WtxLineImage ) = WtxCharPos 
               ) 
          , AFT . A_WtxDeliverLine_Length_mismatch 
          ) 
(* REVIEW ^how this works *) 
      ; WtxCharPos := 0 
      ; IF DoGenerateText 
        THEN 
          WtxLineImage := Strings . Empty ( ) 
        END 
      ; WtxPrevTok := LbeStd . Tok__BegOfLine 
      ; WtxModTextIsToLeftOnLine := FALSE 
      ; WtxLastFmtNoOnLine := EstHs . FmtNoUnknown  
      ; WtxEstListChildrenToPass := 0 (* Dead *) 
      END WtxDeliverLine 

  ; PROCEDURE WtxFlushLine ( ) 
    RAISES { AssertionFailure , Thread . Alerted } 

    = BEGIN (* WtxFlushLine *) 
        IF WtxPrevTok # LbeStd . Tok__BegOfLine 
        THEN 
          WtxDeliverLine ( ) 
        END (* IF *) 
      END WtxFlushLine 

  ; PROCEDURE WtxTraverseEst 
      ( EstRef : LbeStd . EstRootTyp 
      ; KindSet : EstHs . EstChildKindSetTyp 
      ; AbsNodeNo : LbeStd . EstNodeNoTyp 
      ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
      ; EstFmtKind : LangUtil . FmtKindTyp 
      ; EstIndentPos1 : LbeStd . LimitedCharNoTyp 
      ; EstIndentPosN : LbeStd . LimitedCharNoTyp 
      (* EstIndentPos1 and EstIndentPosN are maintained assuming
         the Est subtree is formatted vertically. *) 
      ) 
      RAISES { AssertionFailure , Thread . Alerted } 

    = VAR WtxTeEstTravInfo : TravUtil . EstTravInfoTyp 
    ; VAR WtxTeIndentPos : LbeStd . LimitedCharNoTyp 
    ; VAR WtxTeIsFirstLine : BOOLEAN := TRUE  
    (* WtxTeIsFirstLine and WtxTeIndentPos are maintained as if the Est
       subtree is formatted vertically and contains no mods with new lines. 
    *) 

    ; PROCEDURE WtxTeTraverseFsFixedChildren 
        ( ParentFsNodeRef : LangUtil . FsNodeRefTyp ; FmtKind : LangUtil . FmtKindTyp ) 
      RAISES { AssertionFailure , Thread . Alerted } 

      = BEGIN (* WtxTeTraverseFsFixedChildren *) 
          IF ParentFsNodeRef . FsChildren # NIL 
          THEN 
            FOR RFsChildNo := 0 TO NUMBER ( ParentFsNodeRef . FsChildren ^ ) - 1
            DO WtxTeTraverseFs 
                 ( ParentFsNodeRef . FsChildren ^ [ RFsChildNo ] , FmtKind ) 
            END (* FOR *) 
          END (* IF *) 
        END WtxTeTraverseFsFixedChildren 

    ; PROCEDURE WtxTeTraverseFsListChildren 
        ( ParentFsNodeRef : LangUtil . FsNodeRefTyp ; FmtKind : LangUtil . FmtKindTyp ) 
      RAISES { AssertionFailure , Thread . Alerted } 

      = VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LFsChildNo : LangUtil . FsChildNoTyp 
      ; VAR LRMFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* WtxTeTraverseFsListChildren *) 
          LFsChildNo := 0
        ; LFsChildCt := NUMBER ( ParentFsNodeRef . FsChildren ^ )   
        ; IF 
FALSE AND 
(* FIXME: Remove this.  It is temporary, to remove redundant trailing
          separators when exporting.  Actually, make this some kind of
          option.  But that would have to be either done on many traversers,
          or done during parsing. *) 

             ParentFsNodeRef . FsKind 
             IN LangUtil . FsKindSetEstListTrail 
                AND WtxTeEstTravInfo . EtiParentRef . EstNodeKind 
                    = EstHs . EstNodeKindTyp . EstNodeKindTrail
          THEN LRMFsChildNo := LFsChildCt - 1 
          ELSE LRMFsChildNo := 0 
          END (* IF *) 
        ; LOOP 
            WtxTeTraverseFs ( ParentFsNodeRef . FsChildren ^ [ LFsChildNo ] , FmtKind ) 
          ; IF LFsChildNo = LRMFsChildNo  
               AND WtxTeEstTravInfo . EtiChildNo >= WtxTeEstTravInfo . EtiChildCt 
            THEN EXIT 
            ELSE LFsChildNo := ( LFsChildNo + 1 ) MOD LFsChildCt 
            END (* IF *) 
          END (* LOOP *) 
        END WtxTeTraverseFsListChildren 

    ; PROCEDURE WtxTeAstString 
        ( FsNodeRef : LangUtil . FsNodeRefTyp ; FmtKind : LangUtil . FmtKindTyp ) 
      RAISES { AssertionFailure } 

      = BEGIN 
          IF WtxTeEstTravInfo . EtiNodeRef # NIL 
          THEN 
            WITH WTok = SharedStrings . Tok ( WtxTeEstTravInfo . EtiStringRef ) 
            DO IF EstHs . EstChildKindContainsInsertionRepair IN KindSet 
              THEN (* This is a repair placeholder that Parser proposes to 
                      insert.  Omit it from output. 
                   *)
              ELSE  
                WtxBlankFillTo 
                   ( TravUtil . PosForTok 
                       ( WtxLang 
                       , FmtKind 
                       , WtxModTextIsToLeftOnLine 
                       , WtxCharPos 
                       , WtxTeIndentPos 
                       , FsNodeRef . FsIndentCode 
                       , WtxPrevTok 
                       , WTok 
                       ) 
                   ) 
              ; WtxAccumString ( WtxTeEstTravInfo . EtiStringRef ) 
              ; WtxPrevTok := WTok 
              END (* IF *) 
            END (* WITH *) 
          END (* IF *) 
        END WtxTeAstString 

    ; PROCEDURE WtxTeTraverseFs 
        ( FsNodeRef : LangUtil . FsNodeRefTyp ; FsFmtKind : LangUtil . FmtKindTyp ) 
        RAISES { AssertionFailure , Thread . Alerted } 

      (* Leading and trailing mods. *) 

      = PROCEDURE WtxTeTfsModCmnt ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = BEGIN (* WtxTeTfsModCmnt *) 
            TYPECASE ModCmnt <* NOWARN *> 
            OF ModHs . ModCmntLeadingFixedTyp 
            => IF ModCmnt . ModCmntNlBefore OR WtxCharPos > ModCmnt . ModCmntFromPos 
               THEN WtxFlushLine ( ) 
               END (* IF *) 
            ; WtxBlankFillTo ( ModCmnt . ModCmntFromPos ) 

            | ModHs . ModCmntLeadingRelativeTyp 
            => IF ModCmnt . ModCmntNlBefore 
               THEN WtxFlushLine ( ) 
               END (* IF *) 
            ; WtxBlankFillTo 
                ( EstUtil . WidthSumSigned  
                    ( TravUtil . IndentPos 
                        ( WtxLang 
                        , WtxTeIndentPos 
                        , FsNodeRef . FsIndentCode 
                        ) 
                    , ModCmnt . ModCmntFromPos       
                    ) 
                ) 

            | ModHs . ModCmntTrailingFixedTyp 
            => IF WtxCharPos > ModCmnt . ModCmntFromPos 
               THEN (* Implicit new line. *) 
                 WtxFlushLine ( ) 
               END (* IF *) 
            ; WtxBlankFillTo ( ModCmnt . ModCmntFromPos ) 

            | ModHs . ModCmntTrailingRelativeTyp 
            => WtxBlankFillTo 
                 ( EstUtil . WidthSumSigned  
                     ( WtxCharPos , ModCmnt . ModCmntFromPos ) 
                       (* ^SameLine comment implies there is a token 
                           preceeding, so no need to consider IndentPos. *) 
                 ) 
            END (* TYPECASE *) 
(* TODO: This start position computation for ModCmnt occurs in most traversers.
         Factor it out into TravUtil. 
*) 
          ; WtxAccumString ( ModCmnt . ModCmntStringRef ) 
          ; IF ModCmnt . ModCmntNlAfter 
            THEN 
              WtxDeliverLine ( ) 
            ELSE 
              WtxPrevTok := LbeStd . Tok__Cmnt 
            END (* IF *) 
          ; TravUtil . IncEstChild ( WtxTeEstTravInfo ) 
          END WtxTeTfsModCmnt 

      ; PROCEDURE WtxTeTfsModText 
          ( ModText : ModHs . ModTextTyp ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = BEGIN (* WtxTeTfsModText *) 
            IF ModText . ModTextLeftTokToPos = 0 (* Nl before *) 
            THEN 
              WtxFlushLine ( ) 
            END (* IF *) 
          ; Assert 
              ( WtxCharPos <= ModText . ModTextFromPos 
              , AFT . A_WtxTeTfsModTextForcedNl 
              ) 
          ; WtxBlankFillTo ( ModText . ModTextFromPos ) 
          ; WtxAccumString ( ModText . ModTextStringRef ) 
          ; IF ModText . ModTextToPos = LbeStd . LimitedCharNoInfinity (* Nl after *) 
            THEN WtxDeliverLine ( ) 
            ELSE 
              Assert 
                ( WtxCharPos <= ModText . ModTextToPos 
                , AFT . A_WtxTeTfsModTextNoTrailingSep 
                ) 
            ; WtxBlankFillTo ( ModText . ModTextToPos ) 
            ; WtxPrevTok := LbeStd . Tok__ModText 
            ; WtxModTextIsToLeftOnLine := TRUE 
            END (* IF *) 
          ; TravUtil . IncEstChild ( WtxTeEstTravInfo ) 
          END WtxTeTfsModText 

      ; PROCEDURE WtxTeTfsLeadingMods 
          ( VAR Delete : BOOLEAN 
            (* ^A tok delete mod applies to next tok. *) 
          ) 
          RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LChildRef : LbeStd . EstRootTyp 
        ; VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR LModDelIsFinished : BOOLEAN 
        ; VAR LTok : LbeStd . TokTyp 

        ; BEGIN (* WtxTeTfsLeadingMods *) 
            Delete := FALSE 
          ; LOOP 
              TravUtil . CheckModFwd 
                ( WtxTeEstTravInfo 
                , FsNodeRef 
                , (* VAR *) LChildRef 
                , (* VAR *) LModDelIsFinished 
                ) 
            ; TYPECASE LChildRef 
              OF NULL 
              => EXIT 

              (* Deletion mod. *) 
              | ModHs . ModDelTyp 
              => Delete := TRUE 
              ; IF LModDelIsFinished 
                 THEN 
                   TravUtil . IncEstChild ( WtxTeEstTravInfo ) 
                 ELSE EXIT 
                 END (* IF *) 

              (* Blank line. *) 
              | ModHs . ModBlankLineTyp ( TModBlankLine ) 
              => WtxFlushLine ( ) 
              ; FOR I := 1 TO TModBlankLine . ModBlankLineCt 
(* CHECK: ^A review note says "rework this" why? how? *) 
                DO WtxDeliverLine ( ) 
                END (* FOR *) 
              ; TravUtil . IncEstChild ( WtxTeEstTravInfo ) 

              (* Leading comments. *) 
              | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
              => WtxTeTfsModCmnt ( TModCmnt ) 

              (* Text insertion. *) 
              | ModHs . ModTextTyp ( TModText ) 
              => WtxTeTfsModText ( TModText ) 

              (* Token insertion. *) 
              | EstHs . EstRefTyp ( TEstRef ) (* It will have EstKindModTok *) 
              => IF EstUtil . CharPosPlusWidthInfo 
                      ( WtxCharPos , TEstRef . KTreeWidthInfo ) 
                    > Options . RightMargin 
                THEN WtxDeliverLine ( ) 
                END (* IF *) 
              ; LChildFsNodeRef 
                  := EstUtil . FsRuleForEstNode 
                       ( WtxLang 
                       , WtxTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                       ) 
              ; WtxTraverseEst 
                  ( WtxTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                  , WtxTeEstTravInfo . EtiChildLeafElem . LeKindSet  
                  , WtxTeEstTravInfo . EtiAbsNodeNo 
                      + WtxTeEstTravInfo . EtiChildRelNodeNo 
                  , RootFsNodeRef := LChildFsNodeRef 
                  , EstFmtKind := LangUtil . FmtKindTyp . FmtKindHoriz 
                  , EstIndentPos1 := WtxTeIndentPos 
                  , EstIndentPosN := WtxTeIndentPos 
                  ) 
              ; TravUtil . IncEstChild ( WtxTeEstTravInfo ) 

              (* Lex error characters *) 
              | SharedStrings . T ( TString ) 
              => LTok := SharedStrings . Tok ( TString )   
              ; WtxBlankFillTo 
                   ( TravUtil . PosForTok 
                       ( WtxLang 
                       , FsFmtKind 
                       , WtxModTextIsToLeftOnLine 
                       , WtxCharPos 
                       , WtxTeIndentPos 
                       , FsNodeRef . FsIndentCode 
                       , WtxPrevTok 
                       , LTok 
                       ) 
                   ) 
              ; WtxAccumString ( TString ) 
              ; WtxPrevTok := LTok 
              ; TravUtil . IncEstChild ( WtxTeEstTravInfo ) 

              (* Error. *) 
              | ModHs . ModSyntErrTyp ( TModSyntErr ) 
              => IF DoGenerateErrors 
                 THEN
                   WtxPrevTok := LbeStd . Tok__Cmnt 
                 ; WtxAccumText 
                     ( " (*S" 
                       & Fmt . Int ( TModSyntErr . ModSyntErrCode ) 
                       & "*)" 
                     ) 
                  END (* IF *) 
              ; TravUtil . IncEstChild ( WtxTeEstTravInfo ) 
              | ModHs . ModErrTyp 
                (* Ignore error *) 
              => TravUtil . IncEstChild ( WtxTeEstTravInfo ) 

              ELSE (* Not a leading mod. *) 
                EXIT 
              END (* TYPECASE *) 
            END (* LOOP *) 
          END WtxTeTfsLeadingMods 

      ; PROCEDURE WtxTeTfsTrailingMods ( ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = BEGIN (* WtxTeTfsTrailingMods *) 
            LOOP 
              IF WtxTeEstTravInfo . EtiChildNo 
                 >= WtxTeEstTravInfo . EtiChildCt 
                 (* No more Est children at all. *) 
                 OR FsNodeRef . FsFmtNo # WtxTeEstTravInfo . EtiChildFmtNo 
                    (* Next child for a different FmtNo. *) 
              THEN 
                EXIT 
              END (* IF *) 
            (* Found a child for the current FmtNo. *) 
            ; TYPECASE WtxTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => (* Not a trailing mod. *) 
                 EXIT 
              | ModHs . ModCmntTrailingTyp ( TModCmnt ) 
              => WtxTeTfsModCmnt ( TModCmnt ) 
              ELSE (* Not a trailing mod. *) 
                EXIT 
              END (* TYPECASE *) 
            END (* LOOP *) 
          END WtxTeTfsTrailingMods 

      ; PROCEDURE WtxTeTfsEstChild ( ) 
        RAISES { AssertionFailure , Thread . Alerted } 
        (* PRE: FsNodeRef . FsKind 
                IN { FsKindEstChildOfFixed , FsKindEstChildOfList } 
        *) 
        (* Does NOT handle ModTok. *) 

        = VAR LDelete : BOOLEAN 
        ; VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR LChildFmtKind : LangUtil . FmtKindTyp 
        ; VAR LChildIndentPos1 : LbeStd . LimitedCharNoTyp 
        ; VAR LChildIndentPosN : LbeStd . LimitedCharNoTyp 

        ; BEGIN (* WtxTeTfsEstChild *) 
            WtxTeTfsLeadingMods ( LDelete ) 
          ; TravUtil . AssertFwdNoLostFixedChild 
              ( FsNodeRef , WtxTeEstTravInfo ) 
          ; IF WtxTeEstTravInfo . EtiChildNo < WtxTeEstTravInfo . EtiChildCt 
               AND WtxTeEstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
               AND EstHs . EstChildKindEstChild 
                   IN WtxTeEstTravInfo . EtiChildLeafElem . LeKindSet 
            THEN (* Next child exists, is for this FmtNo, and 
                    is an Est Child. *) 
              IF ISTYPE 
                   ( WtxTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                   , ModHs . EstDummyTyp (* Including NIL. *) 
                   ) 
              THEN (* NIL or dummy.  Skip. *) 
              ELSIF EstHs . EstChildKindTrailingSep 
                    IN WtxTeEstTravInfo . EtiChildLeafElem . LeKindSet 
              THEN (* Skip trailing sep. *) 
              ELSE
                Assert 
                  ( NOT LDelete , AFT . A_WtxTeTfsEstChildDeletedEstChild ) 
              ; LChildFsNodeRef 
                  := LangUtil . FsRuleForEstChild 
                       ( WtxLang 
                       , FsNodeRef 
                       , WtxTeEstTravInfo . EtiChildLeafElem 
                       ) 
              ; TravUtil . ChildIndentPositions 
                  ( WtxLang 
                  , FsNodeRef 
                  , EstIndentPos1 
                  , EstIndentPosN
                  , (* VAR *) ChildIndentPos1 := LChildIndentPos1 
                  , (* VAR *) ChildIndentPosN := LChildIndentPosN 
                  , IsFirstLine := WtxTeIsFirstLine 
                  ) 
              ; LChildFmtKind 
                  := TravUtil . FmtKindForEstTraversing 
                      ( Lang := WtxLang 
                      , CharPos := WtxCharPos 
                      , ModTextIsToLeftOnLine := WtxModTextIsToLeftOnLine 
                      , PrevTok := WtxPrevTok 
                      , FsKind := LChildFsNodeRef . FsKind 
                      , ParentFmtKind := FsFmtKind 
                      , FirstLineIndentPos := LChildIndentPos1
                      , EstRef 
                          := WtxTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                      ) 
              ; WtxTraverseEst 
                  ( WtxTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                  , WtxTeEstTravInfo . EtiChildLeafElem . LeKindSet  
                  , WtxTeEstTravInfo . EtiAbsNodeNo 
                    + WtxTeEstTravInfo . EtiChildRelNodeNo 
                  , RootFsNodeRef := LChildFsNodeRef 
                  , EstFmtKind := LChildFmtKind 
                  , EstIndentPos1 := LChildIndentPos1 
                  , EstIndentPosN := LChildIndentPosN 
                  ) 
              END (* IF *) 
            ; TravUtil . IncEstChild ( WtxTeEstTravInfo ) 
            END (* IF *) 
          ; WtxTeTfsTrailingMods ( ) 
          END WtxTeTfsEstChild 

      ; BEGIN (* WtxTeTraverseFs *) 
          VAR LDelete : BOOLEAN 
        ; VAR LChildFmtKind : LangUtil . FmtKindTyp 
        ; VAR LLastFmtNoOnLine : EstHs . FmtNoTyp 
        ; VAR LEstListChildrenToPass : LbeStd . EstChildNoTyp 

        ; BEGIN (* Block of WtxTeTraverseFs *) 
            CASE FsNodeRef . FsKind <* NOWARN *>

            (* Beginning of image. *) 
            OF FsKindTyp . FsKindBegOfImage 
            => WtxTeTfsTrailingMods ( ) 

            (* End of image. *) 
            | FsKindTyp . FsKindEndOfImage 
            => WtxTeTfsLeadingMods ( LDelete ) 
            ; Assert ( NOT LDelete , AFT . A_WtxTeTraverseFsDeletedEOI ) 

            (* InsTok. *) 
            | FsKindTyp . FsKindInsTok 
            => WtxTeTfsLeadingMods ( LDelete ) 
            ; IF NOT LDelete 
              THEN (* IsRepair tokens are not written to output, as they are
                      proposed insertions, but not accepted by the user. *) 
                WtxBlankFillTo 
                  ( TravUtil . PosForTok 
                      ( WtxLang  
                      , FsFmtKind 
                      , WtxModTextIsToLeftOnLine 
                      , WtxCharPos 
                      , WtxTeIndentPos 
                      , FsNodeRef . FsIndentCode 
                      , WtxPrevTok 
                      , FsNodeRef . FsTok 
                      ) 
                  ) 
              ; WtxAccumString ( FsNodeRef . FsInsTokRef ) 
              ; WtxPrevTok := FsNodeRef . FsTok 
              END (* IF *) 
            ; WtxTeTfsTrailingMods ( ) 

            (* Line breaks. *) 
            | FsKindTyp . FsKindLineBreakOpt 
            => WtxTeTfsLeadingMods ( LDelete ) 
            ; IF NOT LDelete AND WtxCharPos > 0 
              THEN 
                LLastFmtNoOnLine := WtxLastFmtNoOnLine (* Evidence. *) 
              ; LEstListChildrenToPass := WtxEstListChildrenToPass (* Ditto. *)
              ; IF TravUtil . DoTakeLineBreak 
                       ( WtxLang 
                       , WtxCharPos 
                       , WtxModTextIsToLeftOnLine 
                       , WtxPrevTok 
                       , RootFsNodeRef 
                       , FsNodeRef 
                       , FsFmtKind 
                       , WtxTeIndentPos 
                       , WtxTeEstTravInfo 
                       , (* IN OUT *) WtxLastFmtNoOnLine 
                       , (* IN OUT *) WtxEstListChildrenToPass 
                       ) 
                THEN 
                  WtxDeliverLine ( ) 
                END (* IF *) 
              END (* IF *) 
            ; WtxTeIsFirstLine := FALSE 
            ; WtxTeIndentPos := EstIndentPosN 

            | FsKindTyp . FsKindLineBreakReqd 
            => WtxTeTfsLeadingMods ( LDelete ) 
            ; IF NOT LDelete AND WtxCharPos > 0 
              THEN 
                WtxDeliverLine ( ) 
              END (* IF *) 
            ; WtxTeIsFirstLine := FALSE 
            ; WtxTeIndentPos := EstIndentPosN 

            (* Est child cases. *) 
            | FsKindTyp . FsKindEstChildOfFixed 
            => WtxTeTfsEstChild ( ) 

            | FsKindTyp . FsKindEstChildOfList 
            => WtxTeTfsEstChild ( ) 
            ; TravUtil . PassEstListChild 
                ( (* IN OUT *) WtxEstListChildrenToPass ) 

            (* Ast string, *) 
            | FsKindTyp . FsKindAstString 
            => CantHappen ( AFT . A_WtxTeTraverseFs_AstString ) 
            ; WtxTeAstString ( FsNodeRef , FsFmtKind ) 

            (* Est subtree nodes. *) 
            | FsKindTyp . FsKindSubtreeVert 
            , FsKindTyp . FsKindSubtreeHoriz 
            , FsKindTyp . FsKindSubtreeFill 
            =>  LLastFmtNoOnLine := WtxLastFmtNoOnLine (* Evidence. *) 
            ; LEstListChildrenToPass := WtxEstListChildrenToPass (* Ditto. *)
            ; LChildFmtKind 
                := TravUtil . FmtKindForFsSubtreeTraversing  
                     ( WtxLang 
                     , WtxCharPos 
                     , WtxModTextIsToLeftOnLine 
                     , WtxPrevTok 
                     , RootFsNodeRef 
                     , FsNodeRef 
                     , FsFmtKind 
                     , WtxTeIndentPos 
                     , WtxTeEstTravInfo 
                     , (* IN OUT *) WtxLastFmtNoOnLine 
                     , (* IN OUT *) WtxEstListChildrenToPass 
                     )  
            ; WtxTeTraverseFsFixedChildren ( FsNodeRef , LChildFmtKind ) 

            (* Conditional format. *) 
            | FsKindTyp . FsKindCondFmt 
            => IF TravUtil . DoCondFmtFwd 
                    ( WtxLang , WtxTeEstTravInfo , FsNodeRef ) 
               THEN 
                 WtxTeTraverseFsFixedChildren ( FsNodeRef , FsFmtKind ) 
               ELSE 
                 WtxTeTraverseFs ( FsNodeRef . FsCondAltRef  , FsFmtKind ) 
               END (* IF *) 

            END (* CASE FsKind *) 
          END (* Block *) 
        END WtxTeTraverseFs 

    ; BEGIN (* WtxTraverseEst *) 
        IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
      ; IF EstRef # NIL 
        THEN 
          TravUtil . InitEstTravInfoFwd 
            ( (* VAR *) WtxTeEstTravInfo , EstRef , KindSet , AbsNodeNo ) 
        ; WtxTeIsFirstLine := TRUE 
        ; WtxTeIndentPos := EstIndentPos1 
        ; CASE RootFsNodeRef . FsKind <* NOWARN *>

          (* Est fixed nodes. *) 
          OF FsKindTyp . FsKindEstFixedVert 
          , FsKindTyp . FsKindEstFixedHoriz 
          , FsKindTyp . FsKindEstFixedFill 
          => WtxTeTraverseFsFixedChildren ( RootFsNodeRef , EstFmtKind ) 

          (* Est list nodes. *) 
          | FsKindTyp . FsKindEstListVert 
          , FsKindTyp . FsKindEstListHoriz 
          , FsKindTyp . FsKindEstListFill 
          , FsKindTyp . FsKindEstListTrailVert 
          , FsKindTyp . FsKindEstListTrailHoriz 
          , FsKindTyp . FsKindEstListTrailFill 
          => WtxTeTraverseFsListChildren ( RootFsNodeRef , EstFmtKind ) 

          (* Ast string, *) 
          | FsKindTyp . FsKindAstString 
          => WtxTeAstString ( RootFsNodeRef , EstFmtKind ) 

       (* ELSE Can't happen. *) 
          END (* CASE *) 
        ; IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        END (* IF EstRef # NIL *) 
      END WtxTraverseEst 

  ; BEGIN (* WriteText *) 
      IF ImageRef # NIL AND ImageRef . ItPers . IpEstRoot # NIL 
      THEN 
        WtxCharPos := 0 
      ; WtxLineImage := Strings . Empty ( ) 
      ; WtxPrevTok := LbeStd . Tok__BegOfLine 
      ; WtxModTextIsToLeftOnLine := FALSE 
      ; WtxLastFmtNoOnLine := EstHs . FmtNoUnknown  
      ; WtxEstListChildrenToPass := 0 (* Dead *) 
      ; WtxLang := ImageRef . ItPers . IpLang 
      ; WtxTraverseEst 
          ( ImageRef . ItPers . IpEstRoot 
          , EstHs . EstChildKindSetEmpty  
          , AbsNodeNo := 0 
          , RootFsNodeRef 
              := EstUtil . FsRuleForEstNode 
                   ( WtxLang , ImageRef . ItPers . IpEstRoot ) 
          , EstFmtKind := LangUtil . FmtKindTyp . FmtKindVert 
          , EstIndentPos1 := Options . InitialIndentPos 
          , EstIndentPosN := Options . InitialIndentPos 
          ) 
      ; WtxFlushLine ( ) 
      END (* IF *)  
    END WriteText 

(*********************************************************) 

(* VISIBLE: *) 
; PROCEDURE WriteToks 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; DeliverTokProc : DeliverTokProcTyp 
    ; CmntsWanted : BOOLEAN 
    ) 
    RAISES { AssertionFailure , Thread . Alerted } 

  = VAR WtkLang : LbeStd . LangTyp 

  ; PROCEDURE WtkTraverseEst 
      ( EstRef : LbeStd . EstRootTyp 
      ; KindSet : EstHs . EstChildKindSetTyp 
      ; AbsNodeNo : LbeStd . EstNodeNoTyp 
      ) 
      RAISES { AssertionFailure , Thread . Alerted } 

    = VAR WtkTeEstTravInfo : TravUtil . EstTravInfoTyp 

    ; PROCEDURE WtkTeTraverseFs 
        ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
      RAISES { AssertionFailure , Thread . Alerted } 

      (* Leading mods. *) 

      = PROCEDURE WtkTeTfsModCmnt ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = BEGIN (* WtkTeTfsModCmnt *) 
            IF CmntsWanted 
            THEN 
              DeliverTokProc ( ImageRef , ModCmnt . ModCmntStringRef ) 
            END (* IF *) 
          ; TravUtil . IncEstChild ( WtkTeEstTravInfo ) 
          END WtkTeTfsModCmnt 

      ; PROCEDURE WtkTeTfsModText ( ModText : ModHs . ModTextTyp ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = BEGIN (* WtkTeTfsModText *) 
            DeliverTokProc ( ImageRef , ModText . ModTextStringRef ) 
          ; TravUtil . IncEstChild ( WtkTeEstTravInfo ) 
          END WtkTeTfsModText 

      ; PROCEDURE WtkTeTfsLeadingMods 
          ( VAR Delete : BOOLEAN 
            (* ^A tok delete mod applies to next tok. *) 
          ) 
          RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LChildRef : LbeStd . EstRootTyp 
        ; VAR LModDelIsFinished : BOOLEAN 

        ; BEGIN (* WtkTeTfsLeadingMods *) 
            Delete := FALSE 
          ; LOOP 
              TravUtil . CheckModFwd 
                ( WtkTeEstTravInfo 
                , FsNodeRef 
                , (* VAR *) LChildRef 
                , (* VAR *) LModDelIsFinished 
                ) 
            ; TYPECASE LChildRef 
              OF NULL 
              => EXIT 

              (* Deletion mod. *) 
              | ModHs . ModDelTyp 
              => Delete := TRUE 
              ; IF LModDelIsFinished 
                 THEN 
                   TravUtil . IncEstChild ( WtkTeEstTravInfo ) 
                 ELSE EXIT 
                 END (* IF *) 

              (* Blank line. *) 
              | ModHs . ModBlankLineTyp 
              => TravUtil . IncEstChild ( WtkTeEstTravInfo ) 

              (* Comment. *) 
              | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
              => WtkTeTfsModCmnt ( TModCmnt ) 

              (* Text insertion. *) 
              | ModHs . ModTextTyp ( TModText ) 
              => WtkTeTfsModText ( TModText ) 

              (* Token insertion. *) 
              | EstHs . EstRefTyp (* It will have EstKindModTok *) 
              => WtkTraverseEst 
                   ( WtkTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                   , WtkTeEstTravInfo . EtiChildLeafElem . LeKindSet
                   , WtkTeEstTravInfo . EtiAbsNodeNo 
                     + WtkTeEstTravInfo . EtiChildRelNodeNo 
                   ) 
              ; TravUtil . IncEstChild ( WtkTeEstTravInfo ) 

              (* Lex error characters *) 
              | SharedStrings . T ( TString ) 
              => DeliverTokProc ( ImageRef , TString ) 
              ; TravUtil . IncEstChild ( WtkTeEstTravInfo ) 

              (* Error. *) 
              | ModHs . ModErrTyp 
              => TravUtil . IncEstChild ( WtkTeEstTravInfo ) 
                (* Ignore error *) 

              ELSE (* Not a leading mod. *) 
                EXIT 
              END (* TYPECASE *) 
            END (* LOOP *) 
          END WtkTeTfsLeadingMods 

      ; PROCEDURE WtkTeTfsTrailingMods ( ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = BEGIN (* WtkTeTfsTrailingMods *) 
            LOOP 
              IF WtkTeEstTravInfo . EtiChildNo 
                 >= WtkTeEstTravInfo . EtiChildCt 
                 (* No more Est children at all. *) 
                 OR FsNodeRef . FsFmtNo # WtkTeEstTravInfo . EtiChildFmtNo 
                    (* Next child for a different FmtNo. *) 
              THEN 
                EXIT 
              END (* IF *) 
              (* Found a child for the current FmtNo. *) 
            ; TYPECASE WtkTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => (* Not a trailing mod. *) 
                 EXIT 
              | ModHs . ModCmntTrailingTyp ( TModCmnt ) 
              => WtkTeTfsModCmnt ( TModCmnt ) 
              ELSE (* Not a trailing mod. *) 
                EXIT 
              END (* TYPECASE *) 
            END (* LOOP *) 
          END WtkTeTfsTrailingMods 

      ; PROCEDURE WtkTeTfsTraverseFsFixedChildren 
          ( ) RAISES { AssertionFailure , Thread . Alerted } 

        = BEGIN (* WtkTeTfsTraverseFsFixedChildren *) 
            IF FsNodeRef . FsChildren # NIL 
            THEN 
              FOR RFsChildNo := 0 TO NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
              DO WtkTeTraverseFs ( FsNodeRef . FsChildren ^ [ RFsChildNo ] ) 
              END (* FOR *) 
            END (* IF *) 
          END WtkTeTfsTraverseFsFixedChildren 

      ; PROCEDURE WtkTeTfsTraverseFsListChildren 
          ( ) RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LFsChildCt : LangUtil . FsChildNoTyp 
        ; VAR LFsChildNo : LangUtil . FsChildNoTyp 
        ; VAR LRMFsChildNo : LangUtil . FsChildNoTyp 

        ; BEGIN (* WtkTeTfsTraverseFsListChildren *) 
            LFsChildNo := 0 
          ; LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ )   
          ; IF FsNodeRef . FsKind IN LangUtil . FsKindSetEstListTrail 
               AND WtkTeEstTravInfo . EtiParentRef . EstNodeKind 
                   = EstHs . EstNodeKindTyp . EstNodeKindTrail
            THEN LRMFsChildNo := LFsChildCt - 1 
            ELSE LRMFsChildNo := 0 
            END (* IF *) 
          ; LOOP 
              WtkTeTraverseFs ( FsNodeRef . FsChildren ^ [ LFsChildNo ] ) 
            ; IF LFsChildNo = LRMFsChildNo 
                 AND WtkTeEstTravInfo . EtiChildNo 
                     >= WtkTeEstTravInfo . EtiChildCt 
              THEN 
                EXIT 
              ELSE 
                LFsChildNo := ( LFsChildNo + 1 ) MOD LFsChildCt 
              END (* IF *) 
            END (* LOOP *) 
          END WtkTeTfsTraverseFsListChildren 

      ; BEGIN (* WtkTeTraverseFs *) 
          VAR LDelete : BOOLEAN 

        ; BEGIN (* Block for WtkTeTraverseFs *) 
            CASE FsNodeRef . FsKind <* NOWARN *>

            (* Beginning of image. *) 
            OF FsKindTyp . FsKindBegOfImage 
            => WtkTeTfsTrailingMods ( ) 

            (* End of image. *) 
            | FsKindTyp . FsKindEndOfImage 
            => WtkTeTfsLeadingMods ( LDelete ) 
            ; Assert ( NOT LDelete , AFT . A_WtkTeTraverseFsDeletedEOI ) 

            (* InsTok. *) 
            | FsKindTyp . FsKindInsTok 
            => WtkTeTfsLeadingMods ( LDelete ) 
            ; IF NOT LDelete 
              THEN (* IsRepair tokens are not written to output, as they are
                      proposed insertions, but not accepted by the user. *) 
                DeliverTokProc ( ImageRef , FsNodeRef . FsInsTokRef ) 
              END (* IF *) 
            ; WtkTeTfsTrailingMods ( ) 

            (* Line breaks. *) 
            | FsKindTyp . FsKindLineBreakOpt 
            , FsKindTyp . FsKindLineBreakReqd 
            => WtkTeTfsLeadingMods ( LDelete ) 

            (* Est child cases. *) 
            | FsKindTyp . FsKindEstChildOfFixed 
            , FsKindTyp . FsKindEstChildOfList 
            => WtkTeTfsLeadingMods ( LDelete ) 
            ; TravUtil . AssertFwdNoLostFixedChild 
                ( FsNodeRef , WtkTeEstTravInfo ) 
            ; IF WtkTeEstTravInfo . EtiChildNo 
                 < WtkTeEstTravInfo . EtiChildCt 
                 AND WtkTeEstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
                 AND EstHs . EstChildKindEstChild 
                     IN WtkTeEstTravInfo . EtiChildLeafElem . LeKindSet 
              THEN (* Next child exists, is for this FmtNo, and 
                      is an Est Child. *) 
                IF ISTYPE 
                     ( WtkTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     , ModHs . EstDummyTyp (* Including NIL. *) 
                     ) 
                THEN (* NIL or dummy.  Skip. *) 
                ELSIF EstHs . EstChildKindTrailingSep 
                      IN WtkTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                THEN (* Skip trailing sep. *) 
                ELSE 
                  Assert 
                    ( NOT LDelete , AFT . A_WtkTeTraverseFsDeletedEstChild ) 
                ; WtkTraverseEst 
                    ( WtkTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                    , WtkTeEstTravInfo . EtiChildLeafElem . LeKindSet
                    , WtkTeEstTravInfo . EtiAbsNodeNo 
                      + WtkTeEstTravInfo . EtiChildRelNodeNo 
                    ) 
                END (* IF *) 
              ; TravUtil . IncEstChild ( WtkTeEstTravInfo ) 
              END (* IF *) 
            ; WtkTeTfsTrailingMods ( ) 

            (* Ast string, *) 
            | FsKindTyp . FsKindAstString 
            => IF WtkTeEstTravInfo . EtiNodeRef # NIL 
(* CHECK: Is this a good enough check?  Don't we want to check that there
          is an Est child with a FmtNo match? 
*)  
               THEN 
                 IF EstHs . EstChildKindContainsInsertionRepair IN KindSet  
                 THEN (* This is a repair placeholder that Parser proposes 
                         to insert.  Omit it from output. 
                      *)
                 ELSE  
                   DeliverTokProc 
                     ( ImageRef , WtkTeEstTravInfo . EtiStringRef ) 
                 END (* IF *) 
               END (* IF *) 

            (* Est fixed and subtree nodes. *) 
            | FsKindTyp . FsKindEstFixedVert 
            , FsKindTyp . FsKindEstFixedHoriz 
            , FsKindTyp . FsKindEstFixedFill 
            , FsKindTyp . FsKindSubtreeVert 
            , FsKindTyp . FsKindSubtreeHoriz 
            , FsKindTyp . FsKindSubtreeFill 
            => WtkTeTfsTraverseFsFixedChildren ( ) 

            (* Est list nodes. *) 
            | FsKindTyp . FsKindEstListVert 
            , FsKindTyp . FsKindEstListHoriz 
            , FsKindTyp . FsKindEstListFill 
            , FsKindTyp . FsKindEstListTrailVert 
            , FsKindTyp . FsKindEstListTrailHoriz 
            , FsKindTyp . FsKindEstListTrailFill 
            => WtkTeTfsTraverseFsListChildren ( ) 

            (* Conditional format. *) 
            | FsKindTyp . FsKindCondFmt 
            => IF TravUtil . DoCondFmtFwd 
                    ( ImageRef . ItPers . IpLang 
                    , WtkTeEstTravInfo , FsNodeRef 
                    ) 
               THEN 
                 WtkTeTfsTraverseFsFixedChildren ( ) 
               ELSE 
                 WtkTeTraverseFs ( FsNodeRef . FsCondAltRef ) 
               END (* IF *) 

            END (* CASE *) 
          END (* Block  WtkTeTraverseFs block *) 
        END WtkTeTraverseFs 

    ; BEGIN (* WtkTraverseEst *) 
        IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
      ; IF EstRef # NIL 
        THEN 
          TravUtil . InitEstTravInfoFwd 
            ( WtkTeEstTravInfo , EstRef , KindSet , AbsNodeNo ) 
        ; WtkTeTraverseFs ( EstUtil . FsRuleForEstNode ( WtkLang , EstRef ) ) 
        ; IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        END (* IF EstRef # NIL *) 
      END WtkTraverseEst 

  ; BEGIN (* WriteToks *) 
      IF ImageRef # NIL AND ImageRef . ItPers . IpEstRoot # NIL 
      THEN 
        WtkLang := ImageRef . ItPers . IpLang 
      ; WtkTraverseEst 
          ( ImageRef . ItPers . IpEstRoot 
          , EstHs . EstChildKindSetEmpty (* Dead. *) 
          , AbsNodeNo := 0 
          ) 
      END (* IF *) 
    END WriteToks 

; BEGIN (* WriteTrv *) 
  END WriteTrv 
. 
