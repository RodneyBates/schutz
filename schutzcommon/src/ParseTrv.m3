
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE ParseTrv 

(* Parse traverser.  This module traverses a tree for purposes of reparsing. 
   It feeds the Parser with a stream of tokens with attached Est fragments 
   as semantics data.  Some of the tokens are nonterminals, with whole Est 
   subtrees attached.  It invokes scanning incrementally, as needed.  It 
   also reads characters from a file for batch parsing, presenting the same
   interface to the Parser as for reparsing an Est.   
*) 

EXPORTS ParseTrv , ScannerIf 

; IMPORT Fmt 
; IMPORT Rd 
; IMPORT Stdio 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout 
; IMPORT SchutzCoroutine 
; IMPORT EstHs 
; IMPORT EstUtil
; IMPORT LangUtil 
; FROM LangUtil IMPORT FsKindTyp
; IMPORT Layout 
; IMPORT LbeStd 
; IMPORT Marks 
; FROM Marks IMPORT MarkKindTyp 
; IMPORT Misc 
; FROM Misc IMPORT RefanyPad 
; IMPORT MessageCodes 
; IMPORT ModHs 
; IMPORT Options
; IMPORT OSError 
; IMPORT ParseHs 
; IMPORT PortTypes 
; IMPORT ScannerIfRep 
; FROM ScannerIfRep IMPORT ScanResumeKindTyp 
; IMPORT SharedStrings 
; IMPORT Strings 
; IMPORT TravUtil
; IMPORT VersionedFiles

; IMPORT TreeBrowse 

; TYPE AFT = MessageCodes . T 

; CONST BuildTokenLengthHint = 120 
(* Global string slice constants. *) 
; VAR StringEndOfImage : Strings . StringTyp 
    := Strings . FromChar ( LbeStd . CharEndOfImage ) 
; VAR StringNewLine : Strings . StringTyp 
    := Strings . FromChar ( LbeStd . CharNewLine ) 
; VAR StringAllBlanks : Strings . StringTyp 
    := Strings . FromText ( Fmt . Pad ( "" , LbeStd . LimitedCharNoInfinity ) )

; VAR DoInformState : BOOLEAN := TRUE 

(* Routines visible in ScannerIf: *) 
(* The scanner coroutine calls various visible 
   procedures provided here, passing them the ScanIf.  These 
   routines run on the scanner's coroutine but store and fetch various 
   fields in the scanner interface record.  Some also resume the 
   parse traverser coroutine, which examines the resume kind 
   field to see what to do.  It may also use other fields, depending 
   on the kind of resume. 
 
   At startup time of an editing thread, the scanner coroutine must 
   be given a ScanIf value, which it always returns when it calls 
   these routines. *) 

(* VISIBLE in ScannerIf *) 
; PROCEDURE GetInitialChars 
    ( ScanIf : ScanIfTyp 
    ; VAR ScanState : LbeStd . ScanStateTyp 
    ; VAR DeliverString : Strings . StringTyp 
    ; VAR AreAllBlanks : BOOLEAN 
    ) 
  RAISES { Backout } 
  (* Retrieve Initial information delivered to scanner. *) 

  = BEGIN (* GetInitialChars *) 
      Assert 
        ( ScanIf . SifScanState = LbeStd . SsIdle 
          OR ScanIf . SifScanState >= LbeStd . SsInCmnt  
        , AFT . A_ParseTrv_GetInitialChars_NotIdle 
        ) 
    ; ScanState := ScanIf . SifScanState 
    ; DeliverString := ScanIf . SifDeliverString 
    ; AreAllBlanks := ScanIf . SifAreAllBlanks 
    END GetInitialChars 

(* VISIBLE in ScannerIf *) 
; PROCEDURE AccumSlice ( ScanIf : ScanIfTyp ; Slice : Strings . StringTyp ) 
  RAISES { Backout } 

  (* Accumulate Slice as part of the token being scanned. *) 
  (* (Concatenate Slice onto end of SifAccumString...) *) 
  (* AccumSlice runs on the scanner's coroutine, but may also resume 
     parse traverser. *) 

  = BEGIN (* AccumSlice *) 
      IF Strings . Length ( ScanIf . SifAccumString ) 
         < LbeStd . LimitedCharNoInfinity 
      THEN 
        Strings . AppendInPlace ( ScanIf . SifAccumString , Slice ) 
      ; IF Strings . Length ( ScanIf . SifAccumString ) 
           > LbeStd . LimitedCharNoMax 
        THEN (* This depends on LimitedCharNoInfinity 
                = LimitedCharNoMax + 1 *) 
          Strings . TruncateInPlace 
            ( ScanIf . SifAccumString , LbeStd . LimitedCharNoInfinity ) 
        ; ScanIf . SifResumeKind := ScanResumeKindTyp . SrKindLexErr
        ; ScanIf . SifLexErrCode := LbeStd . LeTokTooLong 
        ; SchutzCoroutine . Resume ( ScanIf ) 
        END (* IF *) 
      END (* IF *) 
    END AccumSlice 

(* VISIBLE in ScannerIf *) 
; PROCEDURE ConsumeChars 
    ( ScanIf : ScanIfTyp 
    ; VAR (* IN OUT *) ScanState : LbeStd . ScanStateTyp 
          (* ParseTrav can change the state among LbeStd . SsIdle
             and the various comment states. *) 
    ; ConsumedCt : Strings . StringSsTyp 
      (* ^Number of characters consumed. *) 
    ; VAR DeliverString : Strings . StringTyp 
      (* ^NextSlice delivered to scanner. *) 
    ; VAR AreAllBlanks : BOOLEAN 
    ) 
  RAISES { Backout } 

  (* Consume the first ConsumedCt of the substring last delivered to 
     the scanner. *) 
  (* ConsumeChars runs on the scanner's coroutine, 
     but resumes parse traverser *) 

  = BEGIN (* ConsumeChars *) 
      ScanIf . SifResumeKind := ScanResumeKindTyp . SrKindConsumeChars 
    ; ScanIf . SifConsumedCt := ConsumedCt 
    ; ScanIf . SifScanState := ScanState 
    ; SchutzCoroutine . Resume ( ScanIf ) 
    ; ScanState := ScanIf . SifScanState 
    ; DeliverString := ScanIf . SifDeliverString 
    ; AreAllBlanks := ScanIf . SifAreAllBlanks 
    END ConsumeChars 

(* VISIBLE in ScannerIf *) 
; PROCEDURE DeliverTok 
    ( ScanIf : ScanIfTyp 
    ; VAR (* IN OUT *) ScanState : LbeStd . ScanStateTyp 
          (* ParseTrav can change the state among LbeStd . SsIdle, 
              and the various comment states. *) 
    ; ConsumedCt : Strings . StringSsTyp 
      (* ^Number of characters consumed. *) 
    ; Tok : LbeStd . TokTyp 
    ; VAR DeliverString : Strings . StringTyp 
      (* ^NextSlice delivered to scanner. *) 
    ; VAR AreAllBlanks : BOOLEAN 
    ; IsPlaceholder : BOOLEAN := FALSE 
    ) 
  RAISES { Backout } 
  (* Do what ConsumeChars does, plus deliver a token in Tok. *) 
  (* DeliverTok runs on the scanner's coroutine, 
     but resumes parse traverser *) 

  = BEGIN (* DeliverTok *) 
      ScanIf . SifResumeKind := ScanResumeKindTyp . SrKindDeliverTok 
    ; ScanIf . SifConsumedCt := ConsumedCt 
    ; ScanIf . SifScanState := ScanState 
    ; ScanIf . SifTok := Tok 
    ; ScanIf . SifIsPlaceholder := IsPlaceholder 
    ; SchutzCoroutine . Resume ( ScanIf ) 
    ; ScanState := ScanIf . SifScanState 
    ; DeliverString := ScanIf . SifDeliverString 
    ; AreAllBlanks := ScanIf . SifAreAllBlanks 
    ; ScanIf . SifBegNoted := FALSE 
    END DeliverTok 

(* VISIBLE in ScannerIf *) 
; PROCEDURE NoteBeg 
    ( ScanIf : ScanIfTyp ; ScanState : LbeStd . ScanStateTyp ) 
  RAISES { Backout } 

  (* Note that the current character is the first of a token *) 
  (* NoteBeg runs on the scanner's coroutine, 
     but resumes parse traverser *) 

  = BEGIN (* NoteBeg *) 
      ScanIf . SifResumeKind := ScanResumeKindTyp . SrKindNoteBeg 
    ; ScanIf . SifAccumString 
        := Strings . Empty ( EventualLengthHint := BuildTokenLengthHint ) 
    ; ScanIf . SifScanState := ScanState 
    ; SchutzCoroutine . Resume ( ScanIf ) 
    ; ScanIf . SifBegNoted := TRUE 
    END NoteBeg 

(* VISIBLE in ScannerIf *) 
; PROCEDURE NoteBegPrevChar 
    ( ScanIf : ScanIfTyp ; ScanState : LbeStd . ScanStateTyp ) 
  RAISES { Backout } 

  (* Note that the previous character is the first of a token *) 
  (* NoteBegPrevChar runs on the scanner's coroutine, 
     but resumes parse traverser *) 

  = BEGIN (* NoteBegPrevChar *) 
      ScanIf . SifResumeKind := ScanResumeKindTyp . SrKindNoteBegPrevChar 
    ; ScanIf . SifAccumString 
        := Strings . Empty ( EventualLengthHint := BuildTokenLengthHint ) 
    ; ScanIf . SifScanState := ScanState 
    ; SchutzCoroutine . Resume ( ScanIf ) 
    ; ScanIf . SifBegNoted := TRUE 
    END NoteBegPrevChar 

(* VISIBLE in ScannerIf *) 
; PROCEDURE LexErr ( ScanIf : ScanIfTyp ; ErrCode : LbeStd . ErrCodeTyp ) 
  RAISES { Backout } 

  (* Attach a lexical error message to the current token *) 
  (* LexErr runs on the scanner's coroutine, 
     but resumes parse traverser *) 

  = BEGIN (* LexErr *) 
      ScanIf . SifResumeKind := ScanResumeKindTyp . SrKindLexErr 
    ; ScanIf . SifLexErrCode := ErrCode 
    ; SchutzCoroutine . Resume ( ScanIf ) 
    END LexErr 

(* Interface to parser *) 

; PROCEDURE InitTokInfo ( VAR TokInfo : ParseHs . TokInfoTyp ) 

  = BEGIN (* InitTokInfo *) 
      TokInfo . TiTok := LbeStd . Tok__Null 
    ; TokInfo . TiSyntTokCt := 0 
    ; TokInfo . TiInfo := NIL
    ; TokInfo . TiSliceListRMRoot := NIL 
    ; TokInfo . TiIsInsertionRepair := FALSE 
    ; TokInfo . TiIsInterior := FALSE 
    ; TokInfo . TiFullTempMarkRange := ParseHs . TempMarkRangeEmpty 
    ; TokInfo . TiPatchTempMarkRange := ParseHs . TempMarkRangeEmpty 
    END InitTokInfo

; PROCEDURE InitDeferredInfo ( VAR Ref : ParseHs . DeferredInfoTyp ) 

  = BEGIN 
      Ref . Tok := LbeStd . Tok__Null
    ; Ref . SyntTokCt := 0  
    ; Ref . IsInsertionRepair := FALSE 
    ; Ref . IsInterior := FALSE 
    ; Ref . WaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
    ; Ref . FullTempMarkRange := ParseHs . TempMarkRangeEmpty 
    ; Ref . PatchTempMarkRange := ParseHs . TempMarkRangeEmpty 
    ; Ref . ObjRef := NIL 
    ; Ref . KindSet := EstHs . EstChildKindSetEmpty 
    END InitDeferredInfo 

; PROCEDURE InitScanInfo ( VAR ScanInfo : ParseHs . ScanInfoTyp ) 

  = BEGIN (* InitScanInfo *) 
      ScanInfo . SiScanState := LbeStd . SsIdle 
    ; ScanInfo . SiTokBegScanState := LbeStd . SsIdle 
    ; ScanInfo . SiTokBegPos := 0 
    ; ScanInfo . SiCharPos := 0 
    ; ScanInfo . SiLeftTokToPos := 0 
    END InitScanInfo 

(* VISIBLE *) 
; PROCEDURE PrependToken 
    ( <* UNUSED *> VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; ParseTravStateRef : ParseHs . ParseTravStateRefTyp 
    ; TokInfo : ParseHs . TokInfoTyp 
    ) 
  : ParseHs . ParseTravStateRefTyp 

  (* Return a parse traverse state with TokInfo added at left 
     end of input stream. *) 

  = BEGIN (* PrependToken *) 
      RETURN 
        NEW 
          ( ParseHs . ParseTravStateStreamRefTyp 
          , PtsTokInfo := TokInfo 
          , PtsAdvanceStateRef := ParseTravStateRef 
          , PtssPosRelTo := LbeStd . LimitedCharNoUnknown 
          ) 
    END PrependToken 

; PROCEDURE RefToNewCopyOfParseTravStateStream 
    ( OldRef : ParseHs . ParseTravStateStreamRefTyp ) 
  : ParseHs . ParseTravStateStreamRefTyp 

  = BEGIN (* RefToNewCopyOfParseTravStateStream *) 
      RETURN 
        NEW 
          ( ParseHs . ParseTravStateStreamRefTyp 
          , PtsTokInfo := OldRef . PtsTokInfo 
          , PtsScanInfo := OldRef . PtsScanInfo 
          , PtsAdvanceStateRef := NIL  
          , PtssPosRelTo := OldRef . PtssPosRelTo 
          , PtssTokSeqNo := OldRef . PtssTokSeqNo + 1 
          ) 
    END RefToNewCopyOfParseTravStateStream 

; PROCEDURE RefToNewCopyOfParseTravStateEst 
    ( OldRef : ParseHs . ParseTravStateEstRefTyp ) 
  : ParseHs . ParseTravStateEstRefTyp 

  = BEGIN (* RefToNewCopyOfParseTravStateEst *) 
      RETURN 
        NEW 
          ( ParseHs . ParseTravStateEstRefTyp 
          , PtsTokInfo := OldRef . PtsTokInfo 
          , PtsScanInfo := OldRef . PtsScanInfo 
          , PtsAdvanceStateRef := NIL 
          , PtsPrevTokAfter := OldRef . PtsPrevTokAfter 
          , PtsePrevTokBefore := OldRef . PtsePrevTokBefore 
          , PtseStateKind := OldRef . PtseStateKind 
          , PtseStackFsRef := OldRef . PtseStackFsRef 
          , PtseStackEstRef := OldRef . PtseStackEstRef 
          , PtseDescendStateRef := NIL  
          , PtseStringRef := OldRef . PtseStringRef 
          , PtseStringFromPos := OldRef . PtseStringFromPos 
          , PtseRescanToPos := OldRef . PtseRescanToPos 
          , PtseTokTempMarkSs := OldRef . PtseTokTempMarkSs 
          , PtseModTextIsToLeftOnLine := OldRef . PtseModTextIsToLeftOnLine 
          , PtseDeferredInfoRef := NIL (* Don't copy this. *)
          , PtseLastFmtNoOnLine := OldRef . PtseLastFmtNoOnLine 
          , PtseEstListChildrenToPass := OldRef . PtseEstListChildrenToPass
          ) 
    END RefToNewCopyOfParseTravStateEst 

; CONST Left = Fmt . Align . Left

; PROCEDURE CheckStacks ( PtsEstRef : ParseHs . ParseTravStateEstRefTyp )
  : BOOLEAN (* All is OK. *) 

  = VAR LSeFsTopRef , LSeFsRef : ParseHs . StackElemFsTyp  
  ; VAR LSeEstTopRef , LSeEstRef : ParseHs . StackElemEstTyp 
  ; VAR LFsDepth , LEstDepth : CARDINAL := 0
  ; VAR LSuccess : BOOLEAN := TRUE 

  ; PROCEDURE CsDepths ( ) : TEXT
    = VAR DFs , DEst : TEXT

    ; BEGIN
        DFs 
          := Fmt . Pad 
               ( "D" & Fmt . Int ( LFsDepth ) , DepthPad , align := Left )
      ; DEst 
          := Fmt . Pad 
               ( "D" & Fmt . Int ( LEstDepth ) , DepthPad , align := Left )
      ; RETURN DFs & DEst 
      END CsDepths 

  ; BEGIN
      LSeEstTopRef := PtsEstRef . PtseStackEstRef 
    ; LSeFsTopRef := PtsEstRef . PtseStackFsRef 
    ; LSeEstRef := LSeEstTopRef 
    ; CASE PtsEstRef . PtseStateKind  
      OF ParseHs . ParseTravStateKindTyp . PtsKindDoneWithEstUntraversed 
      , ParseHs . ParseTravStateKindTyp . PtsKindDoneWithEstTraversed 
      => LSeEstRef := LSeEstRef . SeEstLink 
      ; INC ( LEstDepth ) 
      ELSE 
      END (* CASE *) 
    ; LSeFsRef := LSeFsTopRef 
    ; LOOP (* Thru' Est stack elements. *) 
        IF LSeEstRef = NIL
        THEN (* This should never happen. *) 
          EXIT
        ELSE  
          IF LSeEstRef . SeEstIsSublist 
          THEN 
            LSeEstRef := LSeEstRef . SeEstLink 
          ; INC ( LEstDepth ) 
          END (* IF *) 
        ; LOOP (* Thru' Fs stack elements belonging to this Est stack elem. *)
            IF LSeFsRef = NIL 
            THEN 
              IF LSeEstRef . SeEstLink # NIL 
              THEN 
                Assertions . MessageText 
                  ( CsDepths ( ) & "Premature Fs Stack exhaustion" )
              ; LSuccess := FALSE 
              END (* IF *) 
            ; IF LSeEstRef . SeEstRootFsNodeRef # LangUtil . TopFsNodeRef ( )
              THEN 
                Assertions . MessageText
                ( CsDepths ( ) & "Mismatched Bottommost SeEstRootFsNodeRef." ) 
              ; LSuccess := FALSE 
              END (* IF *) 
            ; EXIT 
            ELSE 
              CASE LSeFsRef . SeFsNodeRef . FsKind 
              OF FsKindTyp . FsKindEstFixedVert 
              , FsKindTyp . FsKindEstFixedHoriz 
              , FsKindTyp . FsKindEstFixedFill 
              , FsKindTyp . FsKindEstListVert 
              , FsKindTyp . FsKindEstListHoriz 
              , FsKindTyp . FsKindEstListFill 
              , FsKindTyp . FsKindEstListTrailVert 
              , FsKindTyp . FsKindEstListTrailHoriz 
              , FsKindTyp . FsKindEstListTrailFill 
              , FsKindTyp . FsKindAstString  
              => IF LSeFsRef . SeFsNodeRef # LSeEstRef . SeEstRootFsNodeRef
                THEN 
                  Assertions . MessageText 
                    ( CsDepths ( ) & "Mismatched SeEstRootFsNodeRef." ) 
                ; LSuccess := FALSE 
                END (* IF *)
              ; IF LSeFsRef . SeFsSeEstRef # LSeEstRef  
                THEN 
                  Assertions . MessageText
                    ( CsDepths ( ) & "Mismatched SeFsSeEstRef." ) 
                ; LSuccess := FALSE 
                END (* IF *) 
              ; LSeFsRef := LSeFsRef . SeFsLink 
              ; INC ( LFsDepth ) 
              ; EXIT 
              ELSE 
                LSeFsRef := LSeFsRef . SeFsLink 
              ; INC ( LFsDepth ) 
              END (* CASE *) 
            END (* IF *) 
          END (* LOOP Fs stack elements. *) 
        END (* IF *) 
      ; LSeEstRef := LSeEstRef . SeEstLink 
      ; INC ( LEstDepth ) 
      END (* LOOP Est stack elements. *)
    ; IF NOT LSuccess
      THEN DumpStacks ( PtsEstRef )
      END (* IF *) 
    ; RETURN LSuccess 
    END CheckStacks 

; CONST TokPad = 5
; CONST DepthPad = 3
; CONST PostPad = 5

; VAR FsKindPrefixLen := Text . Length ( "FsKind" ) 
; VAR FsKindPad := Text . Length ( "EstChildOfFixed" )  
; VAR NoFsPrefixLen 
      := DepthPad + RefanyPad + 1 + FsKindPad + PostPad + TokPad + 4 
; VAR NoFsPrefix := Misc . Blanks ( NoFsPrefixLen ) 
; VAR NILPrefixLen := NoFsPrefixLen - DepthPad - 4
; VAR NILPrefix := Misc . Blanks ( NILPrefixLen )

(* Traversing and dumping the graph of parse traverse states. *) 

; TYPE StateVisitorTyp 
       = PROCEDURE
           ( READONLY ParseInfo : ParseHs . ParseInfoTyp
           ; StateRef : ParseHs . ParseTravStateRefTyp
           ; Depth : INTEGER 
           )
         RAISES { Backout }
         
; PROCEDURE TraverseStates
    ( READONLY ParseInfo : ParseHs . ParseInfoTyp ; Visitor : StateVisitorTyp ) 
  RAISES { Backout } 

  = PROCEDURE TsRecurse
      ( FromStateRef , ToStateRef : ParseHs . ParseTravStateEstRefTyp
      ; Depth : INTEGER
      )
    RAISES { Backout } 
  
    = VAR LStateRef , LAdvanceStateRef : ParseHs . ParseTravStateEstRefTyp 
    ; BEGIN
        LStateRef := FromStateRef
      ; IF LStateRef = NIL THEN RETURN END (* IF *) 
      ; LOOP (* Thru' advances at this level. *) 
          LAdvanceStateRef := LStateRef . PtsAdvanceStateRef
        ; Visitor ( ParseInfo , LStateRef , Depth ) 
        ; IF LAdvanceStateRef = NIL 
          THEN (* This is all we can do. *)
            EXIT  
          ELSIF LAdvanceStateRef = ToStateRef
          THEN (* This is all we were asked to do. *)
            Assert
              ( Depth > 0
              , AFT . A_TsRecurse_ToState_at_zero_depth
              )
          ; EXIT  
          ELSIF LAdvanceStateRef = LStateRef 
          THEN (* We've visited End of image. *)
            Assert
              ( Depth = 0
              , AFT . A_TsRecurse_EOI_at_depth_GT_0
              )
          ; EXIT 
          ELSE
            IF LStateRef . PtseDescendStateRef # NIL
            THEN
              TsRecurse
                ( LStateRef . PtseDescendStateRef , LAdvanceStateRef , Depth + 1 ) 
            END (* IF *)
          END (* IF *)
        ; LStateRef := LAdvanceStateRef 
        END (* LOOP *) 
      END TsRecurse 

  ; BEGIN (* TraverseStates *) 
      TsRecurse ( ParseInfo . PiInitTravStateRef , NIL , 0 ) 
    END TraverseStates

; PROCEDURE DumpState
    ( LayoutWrT : Layout . T
    ; StateRef : ParseHs . ParseTravStateEstRefTyp
    ; READONLY ParseInfo : ParseHs . ParseInfoTyp
    )

  = CONST SeqNoPad = 2
  ; VAR LStartPos : INTEGER 
  
  ; BEGIN
      LStartPos := Layout . CharNo ( LayoutWrT )
    ; Layout . PutText
        ( LayoutWrT , Fmt . Pad ( Fmt . Int ( StateRef . PtsSeqNo ) , SeqNoPad ) )
    ; Layout . PutChar ( LayoutWrT , ' ' )
    ; Layout . PutText
        ( LayoutWrT
        , ParseHs . TokInfoImage ( StateRef . PtsTokInfo , ParseInfo . PiLang )
        )
    ; Layout . PutChar ( LayoutWrT , ' ' )
    ; Layout . PutText
        ( LayoutWrT
        , ParseHs . TempMarkRangeImage
            ( StateRef . PtsTokInfo . TiFullTempMarkRange ) 
        )
    ; Layout . PutChar ( LayoutWrT , ' ' )
    ; Layout . PutText
        ( LayoutWrT
        , ParseHs . TempMarkRangeImage
           ( StateRef . PtsTokInfo . TiPatchTempMarkRange )
        )
    ; Layout . PutChar ( LayoutWrT , ' ' )
    ; Layout . PutChar ( LayoutWrT , '[' )
    ; Layout . PutText ( LayoutWrT , Fmt . Int ( StateRef . PtseTokTempMarkSs ) )
    ; Layout . PutChar ( LayoutWrT , ']' )
    ; Layout . PutChar ( LayoutWrT , ' ' )
    ; Layout . PutText
        ( LayoutWrT
        , Misc . BooleanImageShort ( StateRef . PtsTokInfo . TiIsInterior )
        )
    ; Layout . PutChar ( LayoutWrT , ' ' )
    ; Layout . PutText
        ( LayoutWrT
        , Misc . BooleanImageShort
            ( StateRef . PtsTokInfo . TiIsInsertionRepair )
        )
    ; Layout . PutEol ( LayoutWrT ) 
    END DumpState

(* This is here for debugging. *) 
; <* UNUSED *> PROCEDURE DumpStateGraph
    ( FileName : TEXT ; READONLY ParseInfo : ParseHs . ParseInfoTyp )
  RAISES { Backout } 

  = VAR DsgWrT : Wr . T
  ; VAR DsgLayoutWrT : Layout . T

  ; PROCEDURE Visit  
      ( READONLY ParseInfo : ParseHs . ParseInfoTyp
      ; StateRef : ParseHs . ParseTravStateRefTyp
      ; Depth : INTEGER 
      )
    RAISES { Backout }
    
    = CONST IndentAmt = 2
    ; CONST DepthPad = 2
    
    ; BEGIN
        Layout . PutText
          ( DsgLayoutWrT , Fmt . Pad ( Fmt . Int ( Depth ) , DepthPad ) )
      ; Layout . PutChar ( DsgLayoutWrT , ' ' ) 
      ; Layout . PutText
          ( DsgLayoutWrT , Fmt . Pad ( " " , Depth * IndentAmt ) )
      ; DumpState ( DsgLayoutWrT , StateRef , ParseInfo ) 
      END Visit 
  
  ; BEGIN (* DumpStateGraph *)
      TRY 
        DsgWrT := VersionedFiles . OpenWrite ( FileName )
      EXCEPT OSError . E ( Code ) 
      => Assertions . MessageText
           ( "DumpStateGraph: unable to open file \"" & FileName & "\".")
      ; RETURN 
      END (* EXCEPT *)
    ; DsgLayoutWrT := NEW ( Layout . T ) 
    ; DsgLayoutWrT := Layout . Init ( DsgLayoutWrT , DsgWrT )
    ; TraverseStates ( ParseInfo , Visit )
    ; Wr . Close ( DsgWrT ) 
    END DumpStateGraph 

(* Dumping the Est and Fs stacks of a parse traverse Est state. *) 

; PROCEDURE DumpStacks ( PtsEstRef : ParseHs . ParseTravStateEstRefTyp ) 
  (* This is here to be called from within a debugger. *) 

  = VAR DsWrT : Wr . T := Stdio . stderr 
  ; VAR DsEstDepth : CARDINAL := 0 
  ; VAR DsFsDepth : CARDINAL := 0 

  ; PROCEDURE DsDisplayFs ( SeFs : ParseHs . StackElemFsTyp )  

    = VAR LFsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR LText : TEXT 

    ; <* FATAL Thread . Alerted , Wr . Failure *>
      BEGIN
      (* Stack depth. *)  
        Wr . PutText 
          ( DsWrT 
          , Fmt . Pad 
              ( "D" & Fmt . Int ( DsFsDepth ) , DepthPad , align := Left ) 
          ) 

      (* SeFsRef. *) 
      ; Wr . PutText 
          ( DsWrT , Fmt . Pad ( Misc . RefanyImage ( SeFs ) , RefanyPad ) )

      (* FsKind. *) 
      ; LFsNodeRef := SeFs . SeFsNodeRef 
      ; LText 
          := Text . Sub 
               ( LangUtil . FsKindImage ( LFsNodeRef . FsKind ) 
               , FsKindPrefixLen 
               )
      ; LText := Fmt . Pad ( LText , FsKindPad , align := Left )  
      ; Wr . PutChar ( DsWrT , ' ' )  
      ; Wr . PutText ( DsWrT , LText )

      (* FsPostNodeNo. *) 
      ; Wr . PutText 
          ( DsWrT 
          , Fmt . Pad 
              ( " P" & Fmt . Int ( LFsNodeRef . FsPostNodeNo ) 
              , PostPad 
              , align := Left 
              ) 
          ) 

      (* FsTok. *) 
      ; Wr . PutText 
          ( DsWrT 
          , Fmt . Pad 
              ( " T" & Fmt . Int ( LFsNodeRef . FsTok ) 
              , TokPad 
              , align := Left 
              ) 
          ) 
      END DsDisplayFs  

  ; PROCEDURE DsDisplayEst 
      ( SeEst : ParseHs . StackElemEstTyp  
      ; <* UNUSED *> Prefix : TEXT
      ) 

    = VAR LEstNodeRef : LbeStd . EstRootTyp 
    ; VAR LChildEstRef : LbeStd . EstRootTyp 
    ; VAR LStringRef : SharedStrings . T 
    ; VAR LEstParentRef : EstHs . EstRefTyp 
    ; VAR LTok , LChildTok : LbeStd . TokTyp
(* TODO: Fix ParseHs.TokInfoImage so it will take a Tok and an EstRef
         as separate params, then use it here. *)

    ; <* FATAL Thread . Alerted , Wr . Failure *>
      BEGIN 
        TRY 
        (* Stack depth. *)  
          Wr . PutText 
            ( DsWrT 
            , Fmt . Pad 
                ( "D" & Fmt . Int ( DsEstDepth ) , DepthPad , align := Left )   
            ) 
  
        (* SeEstRef. *) 
        ; Wr . PutText 
            ( DsWrT , Fmt . Pad ( Misc . RefanyImage ( SeEst ) , RefanyPad ) ) 

        (* Actual Est node. *) 
        ; LEstNodeRef := SeEst . SeEstTravInfo . EtiNodeRef  
        ; LEstParentRef := SeEst . SeEstTravInfo . EtiParentRef  
        ; LStringRef := SeEst . SeEstTravInfo . EtiStringRef  
        ; IF LStringRef # NIL  
          THEN (* It's a SharedString.T. *) 
          (* SharedString.T address. *) 
            Wr . PutText 
              ( DsWrT 
              , Fmt . Pad ( Misc . RefanyImage ( LStringRef ) , RefanyPad ) 
              )

          (* Its token. *) 
          ; LTok := SharedStrings . Tok ( LStringRef ) 
          ; Wr . PutText 
              ( DsWrT 
              , Fmt . Pad ( " T" & Fmt . Int ( LTok ) , TokPad , align := Left )
              ) 

          (* The string. *) 
          ; Wr . PutChar ( DsWrT , ' ' )  
          ; Wr . PutText 
              ( DsWrT , SharedStrings . ToText ( LStringRef ) ) 

          ELSIF SeEst . SeEstTravInfo . EtiParentRef # NIL 
          THEN (* Interior Est node. *) 
          (* Node address. *) 
            Wr . PutText 
              ( DsWrT 
              , Fmt . Pad ( Misc . RefanyImage ( LEstParentRef ) , RefanyPad ) 
              )

          (* Flags for Optimized singleton list and sublist. *) 
          ; Wr . PutChar ( DsWrT , ' ' )  
          ; IF SeEst . SeEstTravInfo . EtiIsOptSingletonList 
            THEN
              Wr . PutChar ( DsWrT , 'S' )  
            ELSE
              Wr . PutChar ( DsWrT , '-' )  
            END (* IF *) 
          ; IF SeEst . SeEstIsSublist 
            THEN
              Wr . PutChar ( DsWrT , 'L' )  
            ELSE
              Wr . PutChar ( DsWrT , '-' )  
            END (* IF *) 

          (* Its token. *) 
          ; LTok := EstUtil . EstTok ( LEstParentRef ) 
          ; Wr . PutText 
              ( DsWrT 
              , Fmt . Pad ( " T" & Fmt . Int ( LTok ) , TokPad , align := Left )
              ) 

          (* ChildNo/ChildCt. *) 
          ; Wr . PutChar ( DsWrT , ' ' )  
          ; Wr . PutText 
              ( DsWrT , Fmt . Int ( SeEst . SeEstTravInfo . EtiChildNo ) ) 
          ; Wr . PutChar ( DsWrT , '/' )  
          ; Wr . PutText 
              ( DsWrT , Fmt . Int ( SeEst . SeEstTravInfo . EtiChildCt ) ) 

          (* Current Est child address. *) 
          ; LChildEstRef 
              := SeEst . SeEstTravInfo . EtiChildLeafElem . LeChildRef 
          ; Wr . PutText 
              ( DsWrT 
              , Fmt . Pad ( Misc . RefanyImage ( LChildEstRef ) , RefanyPad ) 
              )

          (* Current Estchild token. *) 
          ; LChildTok := EstUtil . EstTok ( LChildEstRef ) 
          ; Wr . PutText 
              ( DsWrT 
              , Fmt . Pad 
                  ( " T" & Fmt . Int ( LTok ) , TokPad , align := Left ) 
              ) 

          ELSE (* Other stuff. *)  
            Wr . PutText 
              ( DsWrT 
              , Fmt . Pad ( Misc . RefanyImage ( LEstParentRef ) , RefanyPad ) 
              )
          END (* IF *) 
        EXCEPT Backout ( Msg )  
          => Wr . PutText ( DsWrT , Wr . EOL ) 
          ; Wr . PutText ( DsWrT , "Assertion Failure in DsDisplayEst, " )  
          ; Wr . PutText ( DsWrT , Msg )  
          ; Wr . PutText ( DsWrT , Wr . EOL ) 
        END (* TRY EXCEPT *)    
      END DsDisplayEst

  ; BEGIN (* DumpStacks *) 
      VAR LSeFsTopRef , LSeFsRef , LXLinkSeFsRef : ParseHs . StackElemFsTyp  
    ; VAR LSeEstTopRef , LSeEstRef : ParseHs . StackElemEstTyp 

    ; <* FATAL Thread . Alerted , Wr . Failure *>
      BEGIN (* Block. *)
        IF PtsEstRef # NIL 
        THEN  
          LSeEstTopRef := PtsEstRef . PtseStackEstRef 
        ; LSeEstRef := LSeEstTopRef 
        ; LSeFsTopRef := PtsEstRef . PtseStackFsRef 
        ; LSeFsRef := LSeFsTopRef 

        ; Wr . PutText ( DsWrT , "Dump of SeFs and SeEst stacks, in state " ) 
        ; Wr . PutText 
            ( DsWrT 
            , ParseHs . ParseTravStateKindImage ( PtsEstRef . PtseStateKind ) 
            )
        ; Wr . PutChar ( DsWrT , ':' ) 
        ; Wr . PutText ( DsWrT , Wr . EOL ) 

        ; LOOP (* Thru' groups of related SeFs and SeEst nodes. *) 
            LXLinkSeFsRef := LSeFsRef 

          (* Find topmost SeFs with a non-NIL crosslink. *) 
          ; WHILE LXLinkSeFsRef # NIL AND LXLinkSeFsRef . SeFsSeEstRef = NIL 
            DO LXLinkSeFsRef := LXLinkSeFsRef . SeFsLink 
            END (* WHILE *) 

          (* Display any SeEst elements ahead of the crosslinked-to one. *) 
          ; WHILE LSeEstRef # NIL 
                  AND LXLinkSeFsRef # NIL 
                  AND LXLinkSeFsRef . SeFsSeEstRef # LSeEstRef 
            DO 
              Wr . PutText ( DsWrT , NoFsPrefix ) 
            ; DsDisplayEst ( LSeEstRef , NoFsPrefix ) 
            ; Wr . PutText ( DsWrT , Wr . EOL ) 
            ; INC ( DsEstDepth ) 
            ; LSeEstRef := LSeEstRef . SeEstLink 
            END (* WHILE *) 

          (* Display leading SeFs nodes, thru' the crosslinked pair. *) 
          ; LOOP 
              DsDisplayFs ( LSeFsRef ) 
            ; IF LXLinkSeFsRef # NIL AND LSeFsRef = LXLinkSeFsRef 
              THEN (* Matching SeFe/SeEst pair. *) 
                Wr . PutText ( DsWrT , " -> " )
              ; DsDisplayEst ( LSeEstRef , NoFsPrefix ) 
              ; Wr . PutText ( DsWrT , Wr . EOL ) 
              ; INC ( DsFsDepth ) 
              ; LSeFsRef := LSeFsRef . SeFsLink 
              ; INC ( DsEstDepth ) 
              ; LSeEstRef := LSeEstRef . SeEstLink 
              ; EXIT 
              ELSE 
                Wr . PutText ( DsWrT , Wr . EOL ) 
              ; INC ( DsFsDepth ) 
              ; LSeFsRef := LSeFsRef . SeFsLink 
              ; IF LSeFsRef = NIL THEN EXIT END (* IF *) 
              END (* IF *) 
            END (* LOOP *) 
          ; IF LSeFsRef = NIL THEN EXIT END (* IF *) 
          END (* LOOP *) 
        (* Display remaining SeEst elems, including the normal bottom one. *) 
        ; WHILE LSeEstRef # NIL 
          DO 
            Wr . PutText ( DsWrT , NoFsPrefix ) 
          ; DsDisplayEst ( LSeEstRef , NoFsPrefix ) 
          ; Wr . PutText ( DsWrT , Wr . EOL ) 
          ; INC ( DsEstDepth ) 
          ; LSeEstRef := LSeEstRef . SeEstLink 
          END (* WHILE *) 
        (* Show the NILs, just for completeness. *) 
        ; Wr . PutText 
            ( DsWrT 
            , Fmt . Pad 
                ( "D" & Fmt . Int ( DsFsDepth ) , DepthPad , align := Left ) 
            ) 
        ; Wr . PutText ( DsWrT , " NIL" ) 
        ; Wr . PutText ( DsWrT , NILPrefix ) 
        ; Wr . PutText 
            ( DsWrT 
            , Fmt . Pad 
                ( "D" & Fmt . Int ( DsEstDepth ) , DepthPad , align := Left )   
            ) 
        ; Wr . PutText ( DsWrT , " NIL" ) 
        ; Wr . PutText ( DsWrT , Wr . EOL ) 
        END (* IF *) 
      END (* Block DumpStacks *) 
    END DumpStacks 

(* VISIBLE *) 
; PROCEDURE NextParseTravState 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; FromStateRef : ParseHs . ParseTravStateRefTyp 
    ; SuccKind : SuccKindTyp 
    ) 
    : ParseHs . ParseTravStateRefTyp 
    RAISES { Thread . Alerted , Backout } 
  (* Get the next parse traversal state, after one in hand. 
     When the current state is a whole subtree, (which implies 
     it contains no syntactic modifications,) 
     SuccKindDescend will force the parse traverser to start 
     down through the children, whereas SuccKindAdvance will 
     move beyond the subtree.  SuccKindAdvance must be used 
     when the current state is a terminal. 
  *) 

  (* Control can be passed among various routines inside 
     NextParseTravState either by a direct call, or by setting 
     PtseStateKind to a value that leads to the desired routine, 
     while NpsDeliverState # DsDeliver.  This causes 
     NpsStateMachine to loop and call the desired routine. 
     This can create some state kinds that can't occur in 
     delivered states.  Even when NextParseTravState is not 
     returning, the state technique is always used, instead of 
     a direct call, in these cases: 
 
     1. Any transfer to NpsNewFsNode, NpsDoneWithFsNode, or NpsNewEst, 
 
     2. Any otherwise-would-be self-recursive transfer. 
 
     3. Any time there is a possibility that NpsDeliverState is DsDeliver. 
 
     4. Any time a reasonably complete CASE on PtseStateKind is needed. 
 
     Rules 1 and 2 prevent recursion. 
  *) 

  = TYPE DeliverStateTyp 
      = { DsStarting (* Start out here. *) 
        , DsTokFound (* Go here when a deliverable (to parser) token has been
                           found.  But continue traversing in order to get 
                           its trailing mods included with it. *) 
        , DsDeliver     (* Go here from DsTokFound when we know there can be
                           no more trailing mods.  This causes return. *) 
        } 

  ; VAR NpsResultStateRef : ParseHs . ParseTravStateRefTyp 
  ; VAR NpsResultStateStreamRef : ParseHs . ParseTravStateStreamRefTyp 
  ; VAR NpsResultStateEstRef : ParseHs . ParseTravStateEstRefTyp 
    (* One of the above = NpsResultStateRef and the other is NIL.  This 
       avoids doing massive numbers of NARROWs all over Nps, but will 
       still detect RT type errors. *) 
  ; VAR NpsSeFsRef : ParseHs . StackElemFsTyp := NIL 
        (* NpsSeFsRef = FromStateStateRef . PtseStackFsRef *) 
  ; VAR NpsSeEstRef : ParseHs . StackElemEstTyp := NIL 
        (* NpsSeEstRef = FromStateRef . PtseStackEstRef *) 
  ; VAR NpsNextTempMarkIsRelevant : BOOLEAN 
  ; VAR NpsDeliverState : DeliverStateTyp 
  ; VAR NpsSliceParentRef : EstHs . EstRefTyp 
        (* This is for lazily accumulated Est slices. 
           Non-NIL implies the following two variables are meaningful: *) 
  ; VAR NpsSliceFrom : LbeStd . EstChildNoTyp 
  ; VAR NpsSliceTo : LbeStd . EstChildNoTyp 
  ; VAR NpsEstPopCt : PortTypes . Int32Typ 
  ; VAR NpsInEstPopPhase : BOOLEAN 
    (* ^We could need to patch several SeEst elements to point to the new
        state.  NpsEstPopCt counts the number of SeEst elements popped,
        before any are pushed, using NpsInEstPopPhase to keep track of the
        latter.  At the end, this many SeEst elements of the stack of the
        old state have their SeEstAdvanceState pointers patched to point
        to the new state. *) 
  ; VAR NpsNextTempMarkSs : LbeStd . MarkNoTyp 
  ; VAR NpsWaitingTempMarkRange : ParseHs . TempMarkRangeTyp 
        := ParseHs . TempMarkRangeEmpty
        (* NpsWaitingTempMarkRange holds the subscripts of temp marks that belong to
           the current item, until it is clear that the current item will be
           part of the delivered token.  At that time, they will be appended
           to TiFullTempMarkRange and maybe to TiPatchTempMarkRange. *)

  ; PROCEDURE NpsResultStateKind ( ) : ParseHs . ParseTravStateKindTyp 
    = BEGIN
        IF NpsResultStateEstRef = NIL
        THEN RETURN ParseHs . ParseTravStateKindTyp . PtsKindNull 
        ELSE
IF NpsResultStateEstRef . PtseStateKind
   = ParseHs . ParseTravStateKindTyp . PtsKindInsideModText 
THEN
  NpsResultStateEstRef . PtseStateKind := NpsResultStateEstRef . PtseStateKind
END
;
          RETURN NpsResultStateEstRef . PtseStateKind 
        END (* IF *)
      END NpsResultStateKind
      
  ; PROCEDURE NpsBuildNode 
      ( NodeRef : LbeStd . EstRootTyp 
      ; EstChildKindSet : EstHs . EstChildKindSetTyp 
      ) 

    = VAR LSliceListElemRef : ParseHs . SliceListElemRefTyp 

    ; BEGIN 
        LSliceListElemRef := NEW ( ParseHs . SliceListElemRefTyp ) 
      ; LSliceListElemRef . SlePredLink 
          := NpsResultStateRef . PtsTokInfo . TiInfo  
        (* ^Implied NARROW, always OK. *)  
      ; LSliceListElemRef . SleNodeRef := NodeRef 
      ; LSliceListElemRef . SleIsSlice := FALSE 
      ; LSliceListElemRef . SleKindSet := EstChildKindSet 
      ; NpsResultStateRef . PtsTokInfo . TiInfo  
          := LSliceListElemRef 
      ; NpsResultStateRef . PtsTokInfo . TiSliceListRMRoot 
          := LSliceListElemRef
        (* ^Just to make debug easier. *) 
      END NpsBuildNode 

  ; PROCEDURE NpsAccumNode 
      ( NodeRef : LbeStd . EstRootTyp 
      ; EstChildKindSet : EstHs . EstChildKindSetTyp 
      ; SyntTokCt : LbeStd . LimitedTokCtTyp 
      ) 
    RAISES { Backout } 

    = BEGIN (* NpsAccumNode *)
        IF NodeRef # NIL 
        THEN  
          IF NpsSliceParentRef # NIL 
          THEN 
            NpsInnerFlushNonNILSlice ( ) 
          ; NpsSliceParentRef := NIL 
          END (* IF *) 
        ; NpsBuildNode ( NodeRef , EstChildKindSet ) 
        END (* IF *) 
      ; LbeStd . IncLimitedTokCt 
          ( NpsResultStateRef . PtsTokInfo . TiSyntTokCt , SyntTokCt ) 
      END NpsAccumNode 

  ; PROCEDURE NpsInnerFlushNonNILSlice ( ) 
    RAISES { Backout } 

    (* This is an optimized, inner procedure.  It is a bit 
       dangerous because it neither checks that NpsSliceParentRef 
       is not NIL nor sets it NIL afterwards. *) 

    = VAR LSliceListElemRef : ParseHs . SliceListElemRefTyp 
    ; VAR LChildRelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LLeafElem : EstHs . LeafElemTyp 

    ; BEGIN (* NpsInnerFlushNonNILSlice *) 
        IF NpsSliceFrom + 1 = NpsSliceTo 
        THEN (* It's just a singleton slice.  Make it a node. *) 
          EstUtil . GetIthChild 
            ( NpsSliceParentRef  
            , NpsSliceFrom 
            , (* VAR *) ResultChildRelNodeNo := LChildRelNodeNo (* Dead *) 
            , (* VAR *) ResultLeafElem := LLeafElem 
            ) 
        ; NpsBuildNode
            ( LLeafElem . LeChildRef 
            , LLeafElem . LeKindSet 
            ) 
        ELSE 
          LSliceListElemRef := NEW ( ParseHs . SliceListElemRefTyp ) 
        ; LSliceListElemRef . SlePredLink 
            := NpsResultStateRef . PtsTokInfo . TiInfo 
          (* ^Implied NARROW, always OK. *)  
        ; LSliceListElemRef . SleNodeRef := NpsSliceParentRef 
        ; LSliceListElemRef . SleIsSlice := TRUE 
        ; LSliceListElemRef . SleFrom := NpsSliceFrom 
        ; LSliceListElemRef . SleTo := NpsSliceTo 
        ; NpsResultStateRef . PtsTokInfo . TiInfo := LSliceListElemRef 
        ; NpsResultStateRef . PtsTokInfo . TiSliceListRMRoot 
            := LSliceListElemRef
          (* ^Just to simplify debugging. *) 
        END (* IF *) 
      END NpsInnerFlushNonNILSlice 

  ; PROCEDURE NpsFlushSlice ( ) 
    RAISES { Backout } 

    = BEGIN 
        IF NpsSliceParentRef # NIL 
        THEN 
          NpsInnerFlushNonNILSlice ( ) 
        ; NpsSliceParentRef := NIL 
        END (* IF *) 
      END NpsFlushSlice

  ; PROCEDURE NpsAccumSlice 
      ( EstRef : EstHs . EstRefTyp 
      ; FromEstChildNo : LbeStd . EstChildNoTyp 
      ; ToEstChildNo : LbeStd . EstChildNoTyp 
      ; SyntTokCt : LbeStd . LimitedTokCtTyp 
      ) 
    RAISES { Backout } 

    = BEGIN (* NpsAccumSlice *) 
        IF NpsSliceParentRef = NIL 
        THEN 
          NpsSliceParentRef := EstRef 
        ; NpsSliceFrom := FromEstChildNo 
        ELSIF NpsSliceParentRef # EstRef OR NpsSliceTo # FromEstChildNo 
        THEN 
          NpsInnerFlushNonNILSlice ( ) 
        ; NpsSliceParentRef := EstRef 
        ; NpsSliceFrom := FromEstChildNo 
        END (* IF *) 
      ; NpsSliceTo := ToEstChildNo 
      ; LbeStd . IncLimitedTokCt 
          ( NpsResultStateRef . PtsTokInfo . TiSyntTokCt , SyntTokCt ) 
      END NpsAccumSlice 

  ; PROCEDURE NpsCheckNextTempMarkForEstLeaf ( LeafRef : LbeStd . EstRootTyp ) 
  (* Sets NpsNextTempMarkIsRelevant *) 

    = BEGIN (* NpsCheckNextTempMarkForEstLeaf *) 
        IF NpsNextTempMarkSs 
           >= NUMBER ( ParseInfo . PiOrigTempMarkListRef ^ ) 
        THEN (* No TempMarks left *) 
          NpsNextTempMarkIsRelevant := FALSE 
        ELSE 
          WITH WTempMark 
               = ParseInfo . PiOrigTempMarkListRef ^ [ NpsNextTempMarkSs ]
          DO 
            NpsNextTempMarkIsRelevant
              := WTempMark . TokMark . Kind IN Marks . MarkKindSetEstLeaf 
                 AND LeafRef = WTempMark . TokMark . TkmEstRef 
          END (* WITH *) 
        END (* IF *)  
      END NpsCheckNextTempMarkForEstLeaf 

  ; PROCEDURE NpsCheckNextTempMarkForInsTok ( FmtNo : EstHs . FmtNoTyp ) 
    RAISES { Backout } 
  (* Sets NpsNextTempMarkIsRelevant. *) 

    = VAR LLeafElem : EstHs . LeafElemTyp 
    ; VAR LNodeNo : LbeStd . EstNodeNoTyp 

    ; BEGIN (* NpsCheckNextTempMarkForInsTok *) 
        NpsNextTempMarkIsRelevant := FALSE
      ; IF NpsNextTempMarkSs 
           < NUMBER ( ParseInfo . PiOrigTempMarkListRef ^ ) 
        THEN (* Some TempMarks left *) 
          WITH 
            WTempMark 
            = ParseInfo . PiOrigTempMarkListRef ^ [ NpsNextTempMarkSs ] 
          , WTravInfo = NpsSeEstRef . SeEstTravInfo  
          DO IF WTempMark . TokMark . TkmFmtNo = FmtNo 
            THEN
              CASE WTempMark . TokMark . Kind 
              OF MarkKindTyp . LeftSibFmtNo  
              => NpsNextTempMarkIsRelevant 
                   := WTravInfo . EtiChildLeafElem . LeChildRef 
                      = WTempMark . TokMark . TkmEstRef

              | MarkKindTyp . RightSibFmtNo 
              => IF WTravInfo . EtiChildCt > 0 
                 THEN 
                   IF WTravInfo . EtiChildNo >= WTravInfo . EtiChildCt 
                   THEN (* Already off the right end of Est children. *)  
                     EstUtil . GetIthChild 
                       ( WTravInfo . EtiParentRef 
                       , WTravInfo . EtiChildCt - 1 
                       , (* VAR *) ResultChildRelNodeNo := LNodeNo (* Dead. *)  
                       , (* VAR *) ResultLeafElem := LLeafElem 
                       ) 
                   ; NpsNextTempMarkIsRelevant 
                       := LLeafElem . LeChildRef = WTempMark . TokMark . TkmEstRef
                   ELSIF WTravInfo . EtiChildLeafElem . LeChildRef = NIL 
                         AND WTravInfo . EtiChildNo > 0 
                   THEN 
(* REVIEW: How can this happen? We are now inserting dummies in place of NILs, 
           although the current child need not be the one a RightSib TempMark
           points to.  And is this the right thing to do? *)  
                     EstUtil . GetIthChild 
                       ( WTravInfo . EtiParentRef 
                       , WTravInfo . EtiChildNo - 1 
                       , (* VAR *) ResultChildRelNodeNo := LNodeNo (* Dead. *)
                       , (* VAR *) ResultLeafElem := LLeafElem 
                       ) 
                   ; NpsNextTempMarkIsRelevant 
                       := LLeafElem . LeChildRef =  WTempMark . TokMark . TkmEstRef
                   END (* IF *) 
                 END (* IF *) 

              | MarkKindTyp . ChildFmtNo 
              => NpsNextTempMarkIsRelevant 
                   := WTravInfo . EtiParentRef = WTempMark . TokMark . TkmEstRef  

              ELSE 
              END (* CASE *) 
            END (* IF *) 
          END (* WITH *) 
        END (* IF *) 
      END NpsCheckNextTempMarkForInsTok 

  ; PROCEDURE NpsIncludeTempMark ( ) RAISES { Backout } 
    (* Include the next temp mark into NpsWaitingTempMarkRange and advance to the
       next temp mark. *)  

    = BEGIN (* NpsIncludeTempMark *) 
        IF ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange )
        THEN 
          NpsWaitingTempMarkRange . From := NpsNextTempMarkSs
        ELSE Assert
               ( NpsWaitingTempMarkRange . To = NpsNextTempMarkSs
               , AFT . A_NpsIncludeTempMark_Discontiguous 
               )
        END (* IF *)
      ; INC ( NpsNextTempMarkSs ) 
      ; NpsWaitingTempMarkRange . To := NpsNextTempMarkSs 
      END NpsIncludeTempMark

  ; PROCEDURE NpsBiasTempMarks ( TokBegPos : LbeStd . CharNoTyp ) 
    (* Convert CharPos of marks in NpsWaitingTempMarkRange from line-relative 
       to tok-relative. 
    *)

    = BEGIN 
        IF ParseInfo . PiParseKind # LbeStd . ParseKindTyp . ParseKindTrav
        THEN RETURN
        END (* IF *)
      ; IF TokBegPos = 0 THEN RETURN END (* IF *) 
      ; IF ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) THEN RETURN END (* IF *)
      
      ; FOR RTempMarkSs := NpsWaitingTempMarkRange . From 
            TO NpsWaitingTempMarkRange . To - 1  
        DO WITH 
             WCharPos  
             = ParseInfo . PiTravTempMarkListRef ^ [ RTempMarkSs ] . CharPos 
           DO WCharPos := WCharPos - TokBegPos 
(* TODO:                           ^Overflows/saturation. *) 
           END (* WITH *) 
        END (* FOR *) 
      END NpsBiasTempMarks 

  ; PROCEDURE NpsPatchTmEstRefs 
      ( NodeRef : LbeStd . EstRootTyp ; Kind : MarkKindTyp ) 
    (* For TempMarks in NpsWaitingTempMarkRange, Patch the EstRef field to
       NodeRef and the kind to Kind.  Only used for kinds for which 
       FmtNo is irrelevant, so leave it alone.  
    *)

    = BEGIN (* NpsPatchTmEstRefs *) 
        IF ParseInfo . PiParseKind = LbeStd . ParseKindTyp . ParseKindTrav 
           AND NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
        THEN
(* TODO: Assert we are not in descend-only-by-parser-request sequence. (How?) *)
          FOR RTempMarkSs := NpsWaitingTempMarkRange . From 
              TO NpsWaitingTempMarkRange . To - 1 
          DO WITH 
               WTempMark 
               = ParseInfo . PiTravTempMarkListRef ^ [ RTempMarkSs ] 
             DO WTempMark . TokMark . TkmEstRef := NodeRef 
             ; WTempMark . TokMark . Kind := Kind 
             END (* WITH *) 
          END (* WHILE *) 
        END (* IF *) 
      END NpsPatchTmEstRefs 

  ; PROCEDURE NpsAppendTempMarkRange 
      ( VAR ToRange : ParseHs . TempMarkRangeTyp ) 
    RAISES { Backout } 
    (* Append NpsWaitingTempMarkRange to ToRange. *) 

    = BEGIN 
        IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange )
        THEN
          IF ParseHs . RangeIsEmpty ( ToRange )
          THEN ToRange := NpsWaitingTempMarkRange
          ELSE
            Assert
              ( ToRange . To = NpsWaitingTempMarkRange . From 
              , AFT . A_NpsAppendTempMarkRange_Discontiguous 
              ) 
          ; ToRange . To := NpsWaitingTempMarkRange . To 
          END (* IF *) 
        END (* IF *) 
      END NpsAppendTempMarkRange 

  ; PROCEDURE NpsSkipTempMarksForSubtrees 
      ( EstRef : EstHs . EstRefTyp 
      ; FromChildNo : LbeStd . EstChildNoTyp 
      ; ToChildNo : LbeStd . EstChildNoTyp 
      ) 
    RAISES { Backout } 
    (* Skip temp marks on/in the subtrees rooted at the range of children
       [FromChildNo,ToChildNo) of EstRef.  Do so by calling NpsIncludeTempMark,
       which increments NpsNextTempMarkSs.  Do not skip RightSib marks on the 
       RM child of the range, but do skip them when properly buried inside 
       subtrees.
       Use this when: 
           1. There is no need to do any patching of temp marks, neither in
              ParseTrv nor to include in TiPatchTempMarkRange for Parser.
       AND 2. You want to skip marks deeper inside whole subtrees. 
    *) 

    = VAR NpsStmTempMarkNumber : INTEGER
    
    ; PROCEDURE NpsStmTraverse 
        ( EstRef : EstHs . EstRefTyp 
        ; TraverseFromChildNo : LbeStd . EstChildNoTyp 
        ; TraverseToChildNo : LbeStd . EstChildNoTyp 
        ; Depth : INTEGER 
        ) 
      RAISES { Backout } 

      = VAR NpsStmTLeafElem : EstHs . LeafElemTyp 
      ; VAR NpsStmTChildNo : LbeStd . EstChildNoTyp 
      ; VAR NpsStmTRightSibSkipped : BOOLEAN := FALSE 

      ; PROCEDURE NpsStmTSkipExactMatches ( ) 

        = BEGIN (* NpsStmTSkipExactMatches *) 
            LOOP (* Thru temp marks that match this child. *) 
              IF NpsNextTempMarkSs >= NpsStmTempMarkNumber 
              THEN EXIT 
              ELSE 
                WITH WTempMark 
                     = ParseInfo . PiOrigTempMarkListRef ^ [ NpsNextTempMarkSs ]
                DO 
                  IF NpsStmTLeafElem . LeChildRef # WTempMark . TokMark . TkmEstRef 
                  THEN 
                    EXIT 
                  ELSIF WTempMark .  TokMark . Kind 
                        = MarkKindTyp . RightSibFmtNo
                  THEN 
                    IF Depth = 0 AND NpsStmTChildNo + 1 >= ToChildNo 
                    THEN (* Do not skip RightSib marks attached to RM child
                            of entire range. 
                         *)
                      EXIT 
                    ELSE (* But skip other RightSib marks. *) 
                      NpsIncludeTempMark ( ) 
                    ; NpsStmTRightSibSkipped := TRUE 
                    END (* IF *) 
                  ELSE 
                    NpsIncludeTempMark ( ) 
                  END (* IF *) 
                END (* WITH *) 
              END (* IF *) 
            END (* LOOP *) 
          END NpsStmTSkipExactMatches 

      ; BEGIN (* NpsStmTraverse *) 

          VAR LNodeNo : LbeStd . EstNodeNoTyp 
        ; VAR LGrandchildCt : LbeStd . EstChildNoTyp 

        ; BEGIN (* Block for NpsStmTraverse *) 
            IF EstRef # NIL 
               AND TraverseFromChildNo < TraverseToChildNo  
               AND NpsNextTempMarkSs < NpsStmTempMarkNumber 
            THEN 
              NpsStmTChildNo := TraverseFromChildNo 
            ; LOOP (* Thru Est children. *) 
                EstUtil . NextInKindSet 
                  ( EstRef 
                  , NpsStmTChildNo 
                  , EstHs . EstChildKindSetContainsTempMark 
                  , (* VAR *) ResultChildNo := NpsStmTChildNo 
                  , (* VAR *) ResultChildRelNodeNo := LNodeNo 
                  , (* VAR *) ResultLeafElem := NpsStmTLeafElem 
                  ) 
              ; IF NpsStmTChildNo >= TraverseToChildNo THEN EXIT END (* IF *) 
              ; NpsStmTSkipExactMatches ( ) 
              ; IF NpsNextTempMarkSs >= NpsStmTempMarkNumber 
                THEN 
                  EXIT 
                END (* IF *) 
              ; IF NOT NpsStmTRightSibSkipped 
                THEN 
                  TYPECASE NpsStmTLeafElem . LeChildRef 
                  OF NULL 
                  => 
                  | EstHs . EstRefTyp ( TEstRef ) 
                  => LGrandchildCt := TEstRef . KTreeChildCt ( ) 
                  ; NpsStmTraverse ( TEstRef , 0 , LGrandchildCt , Depth + 1 ) 
                  ; IF NpsNextTempMarkSs >= NpsStmTempMarkNumber 
                    THEN 
                      EXIT 
                    END (* IF *) 
                  ELSE (* No grandchildren. *) 
                  END (* TYPECASE *) 
                ; NpsStmTSkipExactMatches ( ) 
                ; IF NpsNextTempMarkSs >= NpsStmTempMarkNumber 
                  THEN 
                    EXIT 
                  END (* IF *) 
                END (* IF *) 
              ; INC ( NpsStmTChildNo ) 
              END (* LOOP *) 
            END (* IF *) 
          END (* Block *) 
        END NpsStmTraverse 

    ; BEGIN (* NpsSkipTempMarksForSubtrees *) 
        Assert 
          ( ToChildNo <= EstUtil . EstChildCt ( EstRef ) 
          , AFT . A_NpsSkipTempMarksForSubtrees__ToChildNo_exceeds_child_count
          )
      ; NpsStmTempMarkNumber := NUMBER ( ParseInfo . PiOrigTempMarkListRef ^ ) 
      ; NpsStmTraverse ( EstRef , FromChildNo , ToChildNo , Depth := 0 ) 
      ; NpsAppendTempMarkRange 
          ( (* VAR *) ToRange 
            := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
          ) 
      END NpsSkipTempMarksForSubtrees 

  ; PROCEDURE NpsPatchCmntNlAfter ( ) 

    = BEGIN (* NpsPatchCmntNlAfter *) 
        IF NpsResultStateRef . PtsPrevTokAfter = LbeStd . Tok__Cmnt 
        THEN
          WITH WSliceListElem 
               = NARROW 
                   ( NpsResultStateRef . PtsTokInfo . TiInfo 
                   , ParseHs . SliceListElemRefTyp 
                   ) 
          DO 
            IF WSliceListElem # NIL 
            THEN 
              TYPECASE WSliceListElem . SleNodeRef 
              OF NULL 
              => 
              | ModHs . ModCmntTyp ( TMod ) 
              => (* This can only be a rescanned comment. *)  
                TMod . ModCmntNlAfter := TRUE 
              ; WSliceListElem . SleKindSet 
                  := WSliceListElem . SleKindSet 
                     + EstHs . EstChildKindSetDisplayComputable 
                     - EstHs . EstChildKindSetContainsNoKnownNl 
              ELSE 
              END (* TYPECASE *) 
            END (* IF *) 
          END (* WITH *) 
        END (* IF *) 
      END NpsPatchCmntNlAfter 

  ; PROCEDURE NpsGetDeferredInfoRef ( ) : ParseHs . DeferredInfoRefTyp 

    = BEGIN 
        IF NpsResultStateEstRef = NIL 
        THEN (* Not traversing an Est, take from ParseInfo. *)
          IF ParseInfo . PiDeferredInfoRef = NIL
          THEN
            ParseInfo . PiDeferredInfoRef
              := NEW ( ParseHs . DeferredInfoRefTyp )
          END (* IF *)
        ; InitDeferredInfo ( (* VAR *) ParseInfo . PiDeferredInfoRef ^ )
        ; RETURN ParseInfo . PiDeferredInfoRef
        ELSE
          Assert
            ( NpsResultStateEstRef . PtseDeferredInfoRef = NIL
            , AFT . A_NpsGetDeferredInfoRef_reuse  
            ) 
        ; NpsResultStateEstRef . PtseDeferredInfoRef  
            := NEW ( ParseHs . DeferredInfoRefTyp )
        ; InitDeferredInfo
            ( (* VAR *) NpsResultStateEstRef . PtseDeferredInfoRef ^ )
        ; RETURN NpsResultStateEstRef . PtseDeferredInfoRef 
        END (* IF *) 
      END NpsGetDeferredInfoRef

; VAR NpsPassNewLineCt := 0 
; VAR NpsPassNewLineStopCt := LAST ( INTEGER )  

  ; PROCEDURE NpsPassNewLine ( ) 

    = BEGIN 
        INC ( NpsPassNewLineCt ) 
      ; NpsResultStateRef . PtsScanInfo . SiCharPos := 0 
      ; NpsResultStateRef . PtsScanInfo . SiLeftTokToPos := 0 
      ; NpsResultStateRef . PtsPrevTokAfter := LbeStd . Tok__BegOfLine 
      ; IF ParseInfo . PiParseKind = LbeStd . ParseKindTyp . ParseKindTrav 
        THEN  
          NpsResultStateEstRef . PtsePrevTokBefore := LbeStd . Tok__BegOfLine  
        ; NpsResultStateEstRef . PtseLastFmtNoOnLine := EstHs . FmtNoUnknown
        ; NpsResultStateEstRef . PtseModTextIsToLeftOnLine := FALSE 
        ; NpsResultStateEstRef . PtseEstListChildrenToPass := 0 
        END (* IF *) 
      END NpsPassNewLine 

  ; PROCEDURE NpsAccumRescannedLexErrChars ( ) 
    RAISES { Backout } 

    = VAR LString : SharedStrings . T 
    ; VAR LChildKindSet : EstHs . EstChildKindSetTyp 
    ; VAR LDeferredInfo : ParseHs . DeferredInfoRefTyp 

    ; BEGIN (* NpsAccumRescannedLexErrChars *) 
        LString  
          := SharedStrings . FromString 
               ( ParseInfo . PiScanIf . SifAccumString 
               , LbeStd . Tok__LexErrChars  
               )
      ; LChildKindSet := EstHs . EstChildKindSetLexErrChars 
      ; IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange )  
        THEN 
          NpsPatchTmEstRefs ( LString , MarkKindTyp . Plain ) 
        ; LChildKindSet 
            := LChildKindSet + EstHs . EstChildKindSetContainsTempMark 
        END (* IF *) 
      ; CASE NpsDeliverState <* NOWARN *>
        OF DeliverStateTyp . DsStarting 
        => (* We are to the left of the to-be-returned token. 
              The LexErrChars are a leading mod to it. *) 
          NpsAccumNode ( LString , LChildKindSet , SyntTokCt := 0 ) 
        ; NpsAppendTempMarkRange 
            ( (* VAR *) ToRange 
              := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
            ) 
        ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 

        | DeliverStateTyp . DsTokFound 
        => (* Save LexErrChars for the next token. *) 
          LDeferredInfo := NpsGetDeferredInfoRef ( ) 
        ; LDeferredInfo . Tok := LbeStd . Tok__LexErrChars  
        ; LDeferredInfo . SyntTokCt := 0 
        ; LDeferredInfo . ObjRef := LString   
        ; LDeferredInfo . IsInsertionRepair := FALSE 
        ; LDeferredInfo . IsInterior := FALSE  
        ; LDeferredInfo . KindSet := LChildKindSet
        ; LDeferredInfo . FullTempMarkRange := NpsWaitingTempMarkRange 
        ; LDeferredInfo . PatchTempMarkRange := ParseHs . TempMarkRangeEmpty 
        ; LDeferredInfo . WaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
        ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty (* ^Dead. *)
        ; NpsDeliverState := DeliverStateTyp . DsDeliver 
        END (* CASE *) 
      END NpsAccumRescannedLexErrChars  

  ; PROCEDURE NpsAccumScannedBlankLine ( ) 
    RAISES { Backout } 

    = VAR LChildKindSet : EstHs . EstChildKindSetTyp 
    ; VAR LExistingModRef : ModHs . ModBlankLineTyp 
    ; VAR LNewModRef : ModHs . ModBlankLineTyp 
    ; VAR LDeferredInfo : ParseHs . DeferredInfoRefTyp 

    ; BEGIN (* NpsAccumScannedBlankLine *) 
        CASE NpsDeliverState <* NOWARN *>
        OF DeliverStateTyp . DsStarting 
        => (* We are to the left of the to-be-returned token. 
              This blank line belongs with it, as a leading mod. *) 
          WITH WSliceListElemRef 
               = NARROW 
                   ( NpsResultStateRef . PtsTokInfo . TiInfo 
                   , ParseHs . SliceListElemRefTyp 
                   ) 
          DO 
            IF WSliceListElemRef = NIL 
               OR WSliceListElemRef ^ . SleIsSlice 
            THEN LExistingModRef := NIL 
            ELSE  
              TYPECASE WSliceListElemRef ^ . SleNodeRef 
              OF NULL 
              => LExistingModRef := NIL 
              | ModHs . ModBlankLineTyp ( TModBlankLine ) 
              => LExistingModRef := TModBlankLine  
              ELSE 
                LExistingModRef := NIL 
              END (* TYPECASE *) 
            END (* IF *) 
          ; IF LExistingModRef = NIL 
            THEN (* Start a new ModBlankLine *)  
              LNewModRef 
                := NEW ( ModHs . ModBlankLineTyp , ModBlankLineCt := 1 ) 
            ; LChildKindSet := EstHs . EstChildKindSetModBlankLine 
            ; IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
              THEN 
                NpsPatchTmEstRefs ( LNewModRef , MarkKindTyp . BlankLine ) 
              ; LChildKindSet 
                  := LChildKindSet + EstHs . EstChildKindSetContainsTempMark 
              ; NpsAppendTempMarkRange 
                  ( (* VAR *) ToRange 
                    := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
                  ) 
              ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
              END (* IF *) 
            ; NpsAccumNode ( LNewModRef , LChildKindSet , SyntTokCt := 0 ) 
            ELSE (* Reuse the already-created ModBlankLine *)  
              INC ( LExistingModRef . ModBlankLineCt ) 
(* TODO: ^Protect this against overflows. *) 
            ; IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
              THEN 
                NpsPatchTmEstRefs ( LExistingModRef , MarkKindTyp . BlankLine )
              ; WSliceListElemRef ^ . SleKindSet 
                  := WSliceListElemRef ^ . SleKindSet 
                     + EstHs . EstChildKindSetContainsTempMark 
              ; NpsAppendTempMarkRange 
                  ( (* VAR *) ToRange 
                    := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
                  ) 
              ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
              END (* IF *) 
            END (* IF *) 
          END (* WITH *) 

        | DeliverStateTyp . DsTokFound 
        => (* Save this blank line for the next token. *) 
          LDeferredInfo := NpsGetDeferredInfoRef ( ) 
        ; LDeferredInfo . Tok := LbeStd . Tok__BlankLine 
        ; LDeferredInfo . SyntTokCt := 0 
        ; LDeferredInfo . ObjRef 
            := NEW ( ModHs . ModBlankLineTyp , ModBlankLineCt := 1 ) 
        ; LDeferredInfo . IsInsertionRepair := FALSE 
        ; LDeferredInfo . IsInterior := FALSE 
        ; LDeferredInfo . KindSet := EstHs . EstChildKindSetModBlankLine  
        ; IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
          THEN 
            NpsPatchTmEstRefs 
              ( LDeferredInfo . ObjRef , MarkKindTyp . BlankLine ) 
          ; LDeferredInfo . KindSet  
              := LDeferredInfo . KindSet 
                 + EstHs . EstChildKindSetContainsTempMark 
          END (* IF *) 
        ; LDeferredInfo . FullTempMarkRange := NpsWaitingTempMarkRange 
        ; LDeferredInfo . PatchTempMarkRange := ParseHs . TempMarkRangeEmpty 
        ; LDeferredInfo . WaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
        ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty (* Dead. *) 
        ; NpsDeliverState := DeliverStateTyp . DsDeliver 
        END (* CASE *) 
      END NpsAccumScannedBlankLine 

  ; PROCEDURE NpsAccumRescannedCmnt 
      ( StartPosRelTo : LbeStd . LimitedCharNoTyp ) 
    RAISES { Backout } 

    = VAR LChildKindSet : EstHs . EstChildKindSetTyp 
    ; VAR LModCmntRef : ModHs . ModCmntTyp 
    ; VAR LPosDifference : PortTypes . Card32Typ 
    ; VAR LDeferredInfo : ParseHs . DeferredInfoRefTyp 

    ; BEGIN (* NpsAccumRescannedCmnt *) 
        LChildKindSet := EstHs . EstChildKindSetModCmnt 

      (* Classify the newly scanned comment. *) 
      ; IF NpsDeliverState = DeliverStateTyp . DsStarting  
           (* ^We have already seen a syntactic token. *) 
           OR ( NpsResultStateRef . PtsScanInfo . SiLeftTokToPos = 0 
                (* ^This comment is the first thing on its line. *)  
                AND NpsResultStateRef . PtsScanInfo . SiTokBegPos 
                    <= ( Options . RightMargin DIV 2 ) 
                    (* But towards the left. *)  
              ) (* Treat it as a leading comment on the next token. *) 
        THEN (* This is a leading comment. *) 
          IF NpsResultStateRef . PtsScanInfo . SiTokBegPos = 0 
             (* ^One way this can happen is when stuff that was not comments  
                 has been commented out by changes above.  *) 
          THEN (* comment is first nonblank thing on line *) 
            LModCmntRef := NEW ( ModHs . ModCmntLeadingFixedTyp ) 
          ; LModCmntRef . ModCmntFromPos := 0 
          ; LModCmntRef . ModCmntNlBefore := TRUE 
          ; LChildKindSet 
              := LChildKindSet + EstHs . EstChildKindSetDisplayComputable 
          ELSE (* If we get to this test, we were not inside a comment at BOL,
                   so SiLeftTokToPos, which describes rescanned tokens, is
                   meaningful and must be < SiTokBegPos. *) 
               (* Comment has blanks to left, a movable comment at BOL. *) 
            LModCmntRef := NEW ( ModHs . ModCmntLeadingRelativeTyp ) 
          ; LModCmntRef . ModCmntFromPos 
              := NpsResultStateRef . PtsScanInfo . SiTokBegPos - StartPosRelTo 
(* TODO:         ^Something about the saturation arithmetic here. *) 
          ; LModCmntRef . ModCmntNlBefore 
              := NpsResultStateRef . PtsScanInfo . SiLeftTokToPos = 0 
          ; IF LModCmntRef . ModCmntNlBefore 
            THEN 
              LChildKindSet 
                := LChildKindSet + EstHs . EstChildKindSetDisplayComputable 
            ELSE 
              LChildKindSet 
                := LChildKindSet + EstHs . EstChildKindSetContainsNoKnownNl
                                   (* ^Removed later, if NlAfter. *) 
            END (* IF *) 
          END (* IF *) 
        ELSE (* This is a trailing comment. *) 
          IF NpsResultStateRef . PtsScanInfo . SiLeftTokToPos = 0 
          THEN (* But it's first on line, and we didn't decide above, that it 
                  was in the left half. Treat as trailing, with implied Nl. *) 
            LModCmntRef := NEW ( ModHs . ModCmntTrailingFixedTyp ) 
          ; LModCmntRef . ModCmntFromPos 
              := NpsResultStateRef . PtsScanInfo . SiTokBegPos 
          ELSE 
            LPosDifference 
              := NpsResultStateRef . PtsScanInfo . SiTokBegPos 
                 - NpsResultStateRef . PtsScanInfo . SiLeftTokToPos 
          ; IF LPosDifference = 0 OR LPosDifference = 1 
            THEN (* Zero or one blanks separate this comment from its 
                    predecessor, a movable comment attached to its 
                    predecessor *) 
              LModCmntRef := NEW ( ModHs . ModCmntTrailingRelativeTyp ) 
            ; LModCmntRef . ModCmntFromPos := LPosDifference 
            ELSE (* More than one blank, a fixed comment at left of tokens *) 
              LModCmntRef := NEW ( ModHs . ModCmntTrailingFixedTyp ) 
            ; LModCmntRef . ModCmntFromPos 
                := NpsResultStateRef . PtsScanInfo . SiTokBegPos 
            END (* IF *) 
          END (* IF *) 
        ; LChildKindSet 
            := LChildKindSet 
               + EstHs . EstChildKindSetTrailingMod 
               + EstHs . EstChildKindSetContainsNoKnownNl
                 (* ^Removed later, if NlAfter. *) 
        ; LModCmntRef . ModCmntNlBefore := FALSE  
        END (* IF *) 

      ; LModCmntRef . ModCmntNlAfter 
          := ParseInfo . PiScanIf . SifTok = LbeStd . Tok__CmntAtEndOfLine 
      ; IF LModCmntRef . ModCmntNlAfter 
        THEN 
          LChildKindSet 
            := LChildKindSet 
               + EstHs . EstChildKindSetDisplayComputable 
               - EstHs . EstChildKindSetContainsNoKnownNl
        END (* IF *) 
      ; LModCmntRef . ModCmntStringRef 
          := SharedStrings . FromString 
               ( ParseInfo . PiScanIf . SifAccumString 
               , ParseInfo . PiScanIf . SifTok 
               ) 
      ; LModCmntRef . ModCmntBegScanState 
          := NpsResultStateRef . PtsScanInfo . SiTokBegScanState 
      ; LModCmntRef . ModCmntEndScanState 
          := NpsResultStateRef . PtsScanInfo . SiScanState 

      (* Handle any TempMarks in the new comment. *) 
      ; IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
        THEN 
          NpsBiasTempMarks ( NpsResultStateRef . PtsScanInfo . SiTokBegPos ) 
        ; NpsPatchTmEstRefs ( LModCmntRef , MarkKindTyp . Plain ) 
        ; LChildKindSet 
            := LChildKindSet + EstHs . EstChildKindSetContainsTempMark 
        END (* IF *) 
      ; IF NpsDeliverState = DeliverStateTyp . DsStarting 
           (* ^We are to the left of the to-be-returned token. *) 
           OR ISTYPE ( LModCmntRef , ModHs . ModCmntTrailingTyp ) 
        THEN (* This comment belongs with the current token. *) 
          NpsAccumNode ( LModCmntRef , LChildKindSet , SyntTokCt := 0 ) 
        ; NpsAppendTempMarkRange 
            ( (* VAR *) ToRange 
              := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
            ) 
        ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
        ; NpsResultStateRef . PtsPrevTokAfter := ParseInfo . PiScanIf . SifTok
        
        ELSE (* Save this leading comment for the next token. *) 
          LDeferredInfo := NpsGetDeferredInfoRef ( ) 
        ; LDeferredInfo . Tok := ParseInfo . PiScanIf . SifTok 
        ; LDeferredInfo . SyntTokCt := 0 
        ; LDeferredInfo . ObjRef := LModCmntRef 
        ; LDeferredInfo . KindSet := LChildKindSet 
        ; LDeferredInfo . IsInsertionRepair := FALSE 
        ; LDeferredInfo . IsInterior := FALSE 
        ; IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
          THEN 
            NpsPatchTmEstRefs 
              ( LDeferredInfo . ObjRef , MarkKindTyp . Plain ) 
          ; LDeferredInfo . KindSet  
              := LDeferredInfo . KindSet 
                 + EstHs . EstChildKindSetContainsTempMark 
          END (* IF *) 
        ; LDeferredInfo . FullTempMarkRange := NpsWaitingTempMarkRange 
        ; LDeferredInfo . PatchTempMarkRange := ParseHs . TempMarkRangeEmpty 
        ; LDeferredInfo . WaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
        ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty (* Dead. *) 
        ; NpsDeliverState := DeliverStateTyp . DsDeliver 
        END (* IF *) 
      END NpsAccumRescannedCmnt 

  ; PROCEDURE NpsDeliverRescannedTok ( ) RAISES { Backout } 

    = VAR LStringRef : SharedStrings . T 
    ; VAR LChildKindSet : EstHs . EstChildKindSetTyp 
    ; VAR LDeferredInfo : ParseHs . DeferredInfoRefTyp 

    ; BEGIN (* NpsDeliverRescannedTok *) 
        NpsBiasTempMarks ( NpsResultStateRef . PtsScanInfo . SiTokBegPos )
        (* ^Any leftovers. *) 
      ; WITH WSif = ParseInfo . PiScanIf 
        DO IF LangUtil . TokClass ( ParseInfo . PiLang , WSif . SifTok ) 
              = LbeStd . TokClassTyp . TokClassConstTerm
           THEN (* Insertion token. *) 
             LStringRef := NIL 
           ; LChildKindSet := EstHs . EstChildKindSetEmpty 
           ; CASE NpsDeliverState <* NOWARN *>
             OF DeliverStateTyp . DsStarting 
             => (* This is the token we will deliver. *) 
               NpsAppendTempMarkRange 
                 ( (* VAR *) ToRange 
                   := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
                 ) 
             ; NpsAppendTempMarkRange 
                 ( (* VAR *) ToRange 
                   := NpsResultStateRef . PtsTokInfo . TiPatchTempMarkRange 
                 )
               (* ^Arrange for the Parser to patch leftovers as FmtNo
                   tempmarks. *)
             ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
             ; NpsResultStateRef . PtsTokInfo . TiTok := WSif . SifTok 
             ; NpsResultStateRef . PtsPrevTokAfter := WSif . SifTok  
             ; NpsAccumNode ( LStringRef , LChildKindSet , SyntTokCt := 1 ) 
             ; NpsDeliverState := DeliverStateTyp . DsTokFound

             | DeliverStateTyp . DsTokFound 
             => (* This is the following token.  It means no (more) trailing 
                   mods, so defer it and deliver what went before. *) 
               LDeferredInfo := NpsGetDeferredInfoRef ( ) 
             ; LDeferredInfo . Tok := WSif . SifTok 
             ; LDeferredInfo . SyntTokCt := 1 
             ; LDeferredInfo . FullTempMarkRange := NpsWaitingTempMarkRange 
             ; LDeferredInfo . PatchTempMarkRange := NpsWaitingTempMarkRange 
             ; LDeferredInfo . WaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
             ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty (* Dead. *) 
             ; LDeferredInfo . ObjRef := LStringRef  
             ; LDeferredInfo . KindSet := LChildKindSet 
             ; NpsDeliverState := DeliverStateTyp . DsDeliver 
             END (* CASE *) 

           ELSIF LangUtil . TokClass ( ParseInfo . PiLang , WSif . SifTok ) 
                  = LbeStd . TokClassTyp . TokClassVarTerm 
           THEN (* This is an Ast String token. 
                   Use the string supplied by scanner. *) 
             LStringRef 
               := SharedStrings . FromString
                    ( WSif . SifAccumString , WSif . SifTok ) 
           ; Assert 
               ( LStringRef # NIL 
               , AFT . A_NpsDeliverRescannedTokNILAstString 
               ) 
           ; LChildKindSet := EstHs . EstChildKindSetEstChildNonNIL
           ; IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
             THEN 
               NpsPatchTmEstRefs ( LStringRef , MarkKindTyp . Plain ) 
               (* ^Patch any leftover TempMarks in the new Ast String token. *) 
             ; LChildKindSet 
                 := LChildKindSet + EstHs . EstChildKindSetContainsTempMark 
             END (* IF *) 
           ; CASE NpsDeliverState <* NOWARN *>
             OF DeliverStateTyp . DsStarting 
             => (* This is the token we will deliver. *) 
               NpsAppendTempMarkRange 
                 ( (* VAR *) ToRange 
                   := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
                 ) 
             ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
             ; NpsResultStateRef . PtsTokInfo . TiTok := WSif . SifTok 
             ; NpsResultStateRef . PtsPrevTokAfter := WSif . SifTok  
             ; NpsAccumNode ( LStringRef , LChildKindSet , SyntTokCt := 1 ) 
             ; NpsDeliverState := DeliverStateTyp . DsTokFound
             
             | DeliverStateTyp . DsTokFound 
             => (* This is the following token.  It means no (more) trailing 
                   mods on the previous one, so defer this one and deliver
                   the previous one. *) 
               LDeferredInfo := NpsGetDeferredInfoRef ( ) 
             ; LDeferredInfo . Tok := WSif . SifTok 
             ; LDeferredInfo . SyntTokCt := 1 
             ; LDeferredInfo . FullTempMarkRange := NpsWaitingTempMarkRange 
             ; LDeferredInfo . PatchTempMarkRange
                 := ParseHs . TempMarkRangeEmpty 
             ; LDeferredInfo . WaitingTempMarkRange
                 := ParseHs . TempMarkRangeEmpty 
             ; NpsWaitingTempMarkRange
                 := ParseHs . TempMarkRangeEmpty (* Dead. *) 
             ; LDeferredInfo . ObjRef := LStringRef 
             ; LDeferredInfo . KindSet := LChildKindSet
             ; NpsDeliverState := DeliverStateTyp . DsDeliver 
             END (* CASE *)
             
           ELSIF LangUtil . TokClass ( ParseInfo . PiLang , WSif . SifTok )  
                 IN LbeStd . TokClassSetNTPlaceholder  
           THEN (* Nonterminal placeholder.  Use a language-supplied string
                   for the token. *)  
             LStringRef 
               := LangUtil . DisplayStringForTok 
                    ( ParseInfo . PiLang , WSif . SifTok ) 
           ; LChildKindSet := EstHs . EstChildKindSetEstChildNonNIL 
           ; IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
             THEN 
               NpsPatchTmEstRefs ( LStringRef , MarkKindTyp . Plain ) 
               (* ^Patch leftover TempMarks into the new NT placeholder. *)
             ; LChildKindSet 
                 := LChildKindSet + EstHs . EstChildKindSetContainsTempMark 
             END (* IF *)
           ; CASE NpsDeliverState <* NOWARN *>
             OF DeliverStateTyp . DsStarting 
             => (* This is the token we will deliver. *) 
               NpsAppendTempMarkRange 
                 ( (* VAR *) ToRange 
                   := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
                 ) 
             ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
             ; NpsResultStateRef . PtsTokInfo . TiTok := WSif . SifTok 
             ; NpsResultStateRef . PtsPrevTokAfter := WSif . SifTok  
             ; NpsAccumNode ( LStringRef , LChildKindSet , SyntTokCt := 1 ) 
             ; NpsDeliverState := DeliverStateTyp . DsTokFound

             | DeliverStateTyp . DsTokFound 
             => (* This is the following token.  It means no (more) trailing 
                   mods, so defer it and deliver what has come before. *) 
               LDeferredInfo := NpsGetDeferredInfoRef ( ) 
             ; LDeferredInfo . Tok := WSif . SifTok 
             ; LDeferredInfo . SyntTokCt := 1 
             ; LDeferredInfo . FullTempMarkRange := NpsWaitingTempMarkRange 
             ; LDeferredInfo . PatchTempMarkRange
                 := ParseHs . TempMarkRangeEmpty 
             ; LDeferredInfo . WaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
             ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty (* Dead. *) 
             ; LDeferredInfo . ObjRef := LStringRef  
             ; LDeferredInfo . KindSet := LChildKindSet 
             ; NpsDeliverState := DeliverStateTyp . DsDeliver 
             END (* CASE *)
             
           ELSE CantHappen
                 ( AFT . A_NpsDeliverRescannedTok_bad_tok_kind )
           END (* IF *)
        END (* WITH *) 
      END NpsDeliverRescannedTok 

  (* NpsDeliverStringToScanner is called from within the parse 
     traverser.  It resumes the scanner, with a new string 
     and possibly a new scanner state.  The parse traverser will 
     change the scanner state only when it is already 
     either idle, or in one of the comment states *) 

; VAR NpsConsumeCharsCt := 0 
; VAR NpsConsumeCharsStopCt := LAST ( INTEGER )  

  ; PROCEDURE NpsDeliverStringToScanner 
      ( String : Strings . StringTyp 
      ; StartPosRelTo : LbeStd . LimitedCharNoTyp 
      ; AreAllBlanks : BOOLEAN 
      ; VAR ConsumedCt : Strings . StringSsTyp   
      ) 
    RAISES { Backout } 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    (* Takes care of updating: 
         NpsResultStateRef . PtsScanInfo . SiCharPos 
         NpsResultStateRef . PtsScanInfo . SiLeftTokToPos 
         NpsResultStateRef . PtsScanInfo . SiTokBegPos 
         NpsResultStateRef . PtsScanInfo . SiTokBegScanState 
         NpsResultStateRef . PtsScanInfo . SiScanState 
         PtsPrevTokAfter  
         and, when traversing an Est, calling NpsPassNewLine 
    *) 

    = PROCEDURE NpsDssIncludeTempMarks 
        ( ToPos : LbeStd . LimitedCharNoTyp ) 
      (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 
      RAISES { Backout }
      (* Include any temp marks that are in the string delivered to the
         scanner, up to ToPos, and bias them line-relative, during reparsing.
         They will be rebiased after rescanning. *)

      = VAR LAbsCharPos : LbeStd . LimitedCharNoTyp  

      ; BEGIN
          LOOP 
            IF NOT NpsNextTempMarkIsRelevant 
            THEN 
              EXIT 
            ELSE 
              WITH 
                WTempMark 
                = ParseInfo . PiTravTempMarkListRef ^ [ NpsNextTempMarkSs ] 
              DO
(* TODO: The following 3 assertions are probably overly pedantic. *)
                Assert
                  ( WTempMark . TokMark . TkmFmtNo 
                    = ParseInfo . PiOrigTempMarkListRef ^ [ NpsNextTempMarkSs ]
                      . TokMark . TkmFmtNo 
                  , AFT . A_NpsDssIncludeTempMarks_fmtno_not_original 
                  ) 
              ; Assert
                  ( WTempMark . TokMark . Kind 
                    = ParseInfo . PiOrigTempMarkListRef ^ [ NpsNextTempMarkSs ]
                      . TokMark . Kind 
                  , AFT . A_NpsDssIncludeTempMarks_kind_not_original 
                  ) 
              ; Assert
                  ( WTempMark . TokMark . TkmEstRef 
                    = ParseInfo . PiOrigTempMarkListRef ^ [ NpsNextTempMarkSs ]
                      . TokMark . TkmEstRef
                  , AFT . A_NpsDssIncludeTempMarks_estref_not_original 
                  ) 
              ; LAbsCharPos 
                  := EstUtil . WidthSum 
                       ( WTempMark . CharPos 
                       , NpsResultStateEstRef . PtseStringFromPos 
                       )  
              ; IF LAbsCharPos >= ToPos  
                 THEN 
                   EXIT
                 ELSE  
                   WTempMark . CharPos := LAbsCharPos 
                   (* ^Make it absolute on line. *) 
                 ; NpsIncludeTempMark ( ) 
                 ; IF NpsResultStateKind ( ) 
                      = ParseHs . ParseTravStateKindTyp . PtsKindInsideInsTok 
                   THEN 
                     NpsCheckNextTempMarkForInsTok 
                       ( NpsSeFsRef . SeFsNodeRef . FsFmtNo ) 
                   ELSE 
                     NpsCheckNextTempMarkForEstLeaf 
                       ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                         . LeChildRef 
                       ) 
                   END (* IF *) 
                 END (* IF *) 
              END (* WITH WTempMark *) 
            END (* IF *) 
          END (* LOOP *) 
        END NpsDssIncludeTempMarks 

(* TODO: Move this to where the other constants of this type are. As of
         2004-10-17, they are in ParseHs, but they need to be moved into
         ParseTrv, as this is the only module that uses them.
*) 
    ; CONST PtsStateKindSetString 
        = ParseHs . PtsKindSetTyp 
            { ParseHs . ParseTravStateKindTyp . PtsKindInsideModText 
            , ParseHs . ParseTravStateKindTyp . PtsKindInsideModCmnt 
            , ParseHs . ParseTravStateKindTyp . PtsKindInsideLexErrChars 
            , ParseHs . ParseTravStateKindTyp . PtsKindInsideAstString
            , ParseHs . ParseTravStateKindTyp . PtsKindInsideInsTok 
            } 

    ; PROCEDURE NpsDssConsumeChars ( TokDelivered : BOOLEAN ) 
      RAISES { Backout } 
      (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

      = BEGIN (* NpsDssConsumeChars *) 
          TRY 
            NpsResultStateRef . PtsScanInfo . SiScanState 
              := ParseInfo . PiScanIf . SifScanState 
(* TODO: Somehow protect this code from the scanner's changing 
         ParseInfo . PiScanIf . SifDeliverString. *) 
          ; IF Strings . Length ( ParseInfo . PiScanIf . SifDeliverString ) 
               > 0 
            THEN 
              CASE 
                Strings . IthChar 
                  ( ParseInfo . PiScanIf . SifDeliverString , 0 ) 

              OF LbeStd . CharNewLine 
              => (* Handle consumed new line character. *) 
                 IF ParseInfo . PiScanIf . SifConsumedCt > 0 
                 THEN 
                   Assert 
                     ( TokDelivered OR NOT ParseInfo . PiScanIf . SifBegNoted 
                     , AFT . A_NpsDssConsumeCharsInTokenAfterNl 
                     ) 
                 ; Assert 
                     ( NpsResultStateRef . PtsScanInfo . SiScanState 
                       # LbeStd . SsInTok 
                     , AFT . A_NpsDssConsumeCharsScanningAfterNl 
                     ) 
                 ; Assert 
                     ( ParseInfo . PiScanIf . SifConsumedCt = 1 
                     , AFT . A_NpsDssConsumeChars_MoreThanOneNlConsumed 
                     ) 
(* TODO: ^Better isolate this problem into the scanner. *) 
                 ; NpsPatchCmntNlAfter ( ) 
                 ; IF ParseInfo . PiParseKind 
                      = LbeStd . ParseKindTyp . ParseKindTrav 
                   THEN 
                     INC ( ParseInfo . PiLineCtIncr 
                         , ORD ( NpsResultStateRef . PtsPrevTokAfter 
                                 # LbeStd . Tok__BegOfLine 
                               ) 
                         ) 
                   ; IF NpsResultStateKind ( ) 
                        IN ParseHs . PtsKindSetTrailingNl 
                     THEN (* Probably there can't be any unincluded TempMarks
                             because they would have been included when the
                             string of the item was fully consumed. *) 
                       NpsDssIncludeTempMarks 
                         ( LbeStd . LimitedCharNoInfinity ) 
                     END (* IF *) 
                   END (* IF *) 
                 ; IF NpsResultStateRef . PtsScanInfo . SiLeftTokToPos = 0 
                      AND ( ParseInfo . PiParseKind 
                            = LbeStd . ParseKindTyp . ParseKindFile 
                            (* ^When parsing from a file, two successive Nls 
                               always mean a blank line. *) 
                            OR NpsResultStateRef . PtsScanInfo . SiCharPos > 0 
                               (* When rescanning a tree, successive Nls must 
                                  have blanks between to mean a blank line. *) 
                          ) 
                      AND NOT TokDelivered 
                   THEN (* We have found a blank line. *) 
                     NpsAccumScannedBlankLine ( ) 
                   END (* IF *) 
                 ; NpsPassNewLine ( ) 
                   (* ^The variables set by NpsPassNewLine are all for the
                      unrescanned version, but new lines are in the same 
                      places in both old and new, and this is a convenient 
                      single place we get to for every new line, when 
                      rescanning. *)  
(* TODO: A few places call NpsPassNewLine redundantly, where new lines are 
         found in the traversed stuff.  Put calls in all such places, and then 
         delete the call here. *) 
                 END (* IF *) 

              | LbeStd . CharEndOfImage 
              => IF ParseInfo . PiScanIf . SifConsumedCt > 0 
                 THEN 
                   Assert 
                     ( TokDelivered OR NOT ParseInfo . PiScanIf . SifBegNoted 
                     , AFT . A_NpsDssConsumeCharsInTokenAfterEoi 
                     ) 
                 ; Assert 
                     ( NpsResultStateRef . PtsScanInfo . SiScanState 
                       = LbeStd . SsIdle 
                     , AFT . A_NpsDssConsumeCharsScanningAfterEoi 
                     ) 
                 ; Assert 
                     ( ParseInfo . PiScanIf . SifConsumedCt = 1 
                     , AFT . A_NpsDssConsumeChars_MoreThanOneEoiConsumed 
                     ) 
(* TODO: ^Better isolate this problem into the scanner. *) 
                 ; NpsDssIncludeTempMarks 
                     ( LbeStd . LimitedCharNoInfinity ) 
                 END (* IF *) 

              ELSE (* Ordinary characters consumed. *) 
                Assert 
                  ( ParseInfo . PiScanIf . SifConsumedCt 
                    <= Strings . Length ( String ) 
                  , AFT . A_NpsDssConsumeChars_TooManyCharsConsumed 
                  ) 
(* TODO: ^Better isolate this problem into the scanner. *) 
              ; INC ( NpsResultStateRef . PtsScanInfo . SiCharPos 
                    , ParseInfo . PiScanIf . SifConsumedCt 
                    ) 
              ; INC ( NpsConsumeCharsCt ) 
              ; IF ParseInfo . PiParseKind 
                   = LbeStd . ParseKindTyp . ParseKindTrav 
                THEN
                  IF NpsResultStateRef . PtsScanInfo . SiCharPos 
                     >= NpsResultStateEstRef . PtseStringFromPos 
                        + SharedStrings . Length 
                            ( NpsResultStateEstRef . PtseStringRef ) 
                     (* ^The string is used up *) 
                     AND NpsResultStateKind ( ) 
                         IN PtsStateKindSetString 
                         (* ^This was a real string, not just blanks. *) 
                  THEN (* Include all TempMarks on the item, even those to
                          the right of its string. This keeps them on the
                          chars BuildTempMark put them on.  It also ensures
                          TempMarks in trailing blanks of a ModText don't
                          get orphaned. *) 
                    NpsDssIncludeTempMarks ( LbeStd . LimitedCharNoInfinity ) 
                  ELSE (* Include only TempMarks up to immediately after the 
                          last consumed character. *)  
                    NpsDssIncludeTempMarks 
                      ( NpsResultStateRef . PtsScanInfo . SiCharPos  )
                  END (* IF *) 
                END (* IF *) 
              END (* CASE *) 
            END (* IF *) 
          ; INC ( ConsumedCt , ParseInfo . PiScanIf . SifConsumedCt ) 
          EXCEPT Strings . SsOutOfBounds 
          => RAISE Backout ( "SharedStrings.SsOutOfBounds" )  
          END (* TRY EXCEPT *) 
        END NpsDssConsumeChars 

    ; PROCEDURE NpsDssLexErr ( ) 
      RAISES { Backout }

      (* Just leave any leftover temp marks alone, to be put on a later 
         item that can have them. *)

      = VAR LModLexErrRef : ModHs . ModLexErrTyp 
      ; VAR LDeferredInfo : ParseHs . DeferredInfoRefTyp 

      ; BEGIN (* NpsDssLexErr *) 
          LModLexErrRef := NEW ( ModHs . ModLexErrTyp ) 
        ; LModLexErrRef . ModLexErrCode 
            := ParseInfo . PiScanIf . SifLexErrCode 
        ; LModLexErrRef . ModLexErrPos 
            := NpsResultStateRef . PtsScanInfo . SiCharPos 
               - NpsResultStateRef . PtsScanInfo . SiTokBegPos 
(* TODO: ^Something about the saturation arithmetic here. *) 
        ; CASE NpsDeliverState <* NOWARN *>
          OF DeliverStateTyp . DsStarting 
          => (* We are to the left of the to-be-returned token. 
                The LexErr is a leading mod to it. *) 
            NpsAccumNode 
              ( LModLexErrRef 
              , EstHs . EstChildKindSetModLexErr 
              , SyntTokCt := 0 
              ) 

          | DeliverStateTyp . DsTokFound 
          => (* Save ModLexErr for the next token. *) 
            LDeferredInfo := NpsGetDeferredInfoRef ( ) 
          ; LDeferredInfo . Tok := LbeStd . Tok__Null   
          ; LDeferredInfo . SyntTokCt := 0 (* Dead. *) 
          ; LDeferredInfo . ObjRef := LModLexErrRef     
          ; LDeferredInfo . IsInsertionRepair := FALSE (* Dead. *) 
          ; LDeferredInfo . IsInterior := FALSE (* Dead. *)  
          ; LDeferredInfo . KindSet := EstHs . EstChildKindSetModLexErr
          ; LDeferredInfo . WaitingTempMarkRange := NpsWaitingTempMarkRange 
          ; LDeferredInfo . FullTempMarkRange
              := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
          ; LDeferredInfo . PatchTempMarkRange := ParseHs . TempMarkRangeEmpty
          ; NpsDeliverState := DeliverStateTyp . DsDeliver 
          END (* CASE *) 
        END NpsDssLexErr 

    ; BEGIN (* NpsDeliverStringToScanner *) 
        ConsumedCt := 0 
      ; IF Strings . Length ( String ) > 0 
        THEN 
          ParseInfo . PiScanIf . SifDeliverString := String 
        ; ParseInfo . PiScanIf . SifScanState 
            := NpsResultStateRef . PtsScanInfo . SiScanState 
        ; ParseInfo . PiScanIf . SifAreAllBlanks := AreAllBlanks 
        ; ParseInfo . PiScanIf . SifPrevTok 
            := NpsResultStateRef . PtsPrevTokAfter 

        (* Keep resuming scanner until it consumes some characters 
           and/or delivers a token. *) 
        ; LOOP 
            SchutzCoroutine . Resume ( ParseInfo . PiScanIf ) 
          ; CASE ParseInfo . PiScanIf . SifResumeKind <* NOWARN *>

            OF ScanResumeKindTyp . SrKindDeliverTok 
            => (* Scanner has delivered a token. *) 
               Assert 
                 ( ParseInfo . PiScanIf . SifBegNoted 
                 , AFT . A_ParseTrv_NextParseState_NpsDeliverStringToScanner_NoTokenBeginning 
                 ) 
            ; NpsDssConsumeChars ( TokDelivered := TRUE ) 
            ; IF ParseInfo . PiScanIf . SifTok = LbeStd . Tok__Cmnt 
                 OR ParseInfo . PiScanIf . SifTok 
                    = LbeStd . Tok__CmntAtEndOfLine 
              THEN (* Delivered tok is a comment. *) 
                NpsAccumRescannedCmnt ( StartPosRelTo ) 
              ELSIF ParseInfo . PiScanIf . SifTok = LbeStd . Tok__LexErrChars 
              THEN 
                NpsAccumRescannedLexErrChars ( ) 
              ELSE (* Delivered tok is a valid syntactic token *) 
                NpsDeliverRescannedTok ( ) 
              END (* IF comment *) 
(* CHECK: It does not suffice to let SiCharPos, PtsePrevTokBefore, etc. go 
          unmaintained whenever SiScanState = SsIdle.  Even in SsIdle, we 
          could hit a comment whose start state is not SsIdle and need this 
          info.  Review whether it is maintained completely enough. 
*)  
            ; NpsResultStateRef . PtsScanInfo . SiLeftTokToPos 
                := NpsResultStateRef . PtsScanInfo . SiCharPos 
            ; EXIT 

            | ScanResumeKindTyp . SrKindConsumeChars 
            => (* Scanner has consumed the characters. *) 
              NpsDssConsumeChars ( TokDelivered := FALSE ) 
            ; IF ConsumedCt > 0 THEN EXIT END (* IF *) 

            | ScanResumeKindTyp . SrKindNoteBegPrevChar 
            => (* Scanner has noted beginning of tok at some previous char. *) 
               Assert 
                 ( NOT ParseInfo . PiScanIf . SifBegNoted 
                 , AFT . A_ParseTrv_NextParseState_NpsDeliverStringToScanner_TwoTokenBeginnings 
                 ) 
            ; NpsResultStateRef . PtsScanInfo . SiTokBegPos 
                := NpsResultStateRef . PtsScanInfo . SiCharPos - 1 
(* TODO: ^Something about the saturation arithmetic here. *) 
            ; NpsResultStateRef . PtsScanInfo . SiTokBegScanState 
                := NpsResultStateRef . PtsScanInfo . SiScanState 

            | ScanResumeKindTyp . SrKindNoteBeg 
           => (* Scanner has noted beginning of tok at the current char. *) 
               Assert 
                 ( NOT ParseInfo . PiScanIf . SifBegNoted 
                 , AFT . A_ParseTrv_NextParseState_NpsDeliverStringToScanner_TwoTokenBeginnings 
                 ) 
            ; NpsResultStateRef . PtsScanInfo . SiTokBegPos 
                := NpsResultStateRef . PtsScanInfo . SiCharPos 
            ; NpsResultStateRef . PtsScanInfo . SiTokBegScanState 
                := NpsResultStateRef . PtsScanInfo . SiScanState 

            | ScanResumeKindTyp . SrKindLexErr 
            => (* Scanner has noted a lexical error. *) 
               NpsDssLexErr ( ) 
            ; EXIT 
            END (* CASE *) 
          END (* LOOP *) 
        END (* IF *) 
      END NpsDeliverStringToScanner 

  ; PROCEDURE NpsModCmntAbsStartPos ( ModCmntRef : ModHs . ModCmntTyp ) 
    : LbeStd . LimitedCharNoTyp 
    RAISES { Backout } 
    (* PRE: We are scanning. *) 

    = VAR LPos : LbeStd . LimitedCharNoSignedTyp 

    ; BEGIN (* NpsModCmntAbsStartPos *) 
        TYPECASE ModCmntRef 
        OF ModHs . ModCmntLeadingFixedTyp 
        , ModHs . ModCmntTrailingFixedTyp 
        => RETURN ModCmntRef . ModCmntFromPos 
        | ModHs . ModCmntLeadingRelativeTyp 
        => LPos 
             := EstUtil . WidthSumSigned  
                  ( TravUtil . IndentPos 
                      ( ParseInfo . PiLang 
                      , NpsSeFsRef . SeFsIndentPos 
                      , NpsSeFsRef . SeFsNodeRef . FsIndentCode 
                      ) 
                  , ModCmntRef . ModCmntFromPos 
                  ) 
        ; IF NpsResultStateRef . PtsScanInfo . SiCharPos > LPos 
             AND NpsResultStateRef . PtsScanInfo . SiCharPos 
                 <= LbeStd . CharNoMax 
          THEN LPos := NpsResultStateRef . PtsScanInfo . SiCharPos 
          END (* IF *) 
        ; RETURN LbeStd . ProjectToLimitedCharNoTyp ( LPos ) 

        | ModHs . ModCmntTrailingRelativeTyp 
        => RETURN 
             EstUtil . WidthSum 
               ( NpsResultStateRef . PtsScanInfo . SiCharPos   
               , ModCmntRef . ModCmntFromPos 
               ) 
(* CHECK: Is there a range error possibility here? *) 
           (* ^SameLine comment IMPLIES there is a token 
               preceeding, so no need to consider IndentPos. *) 
        ELSE 
          CantHappen ( AFT . A_NpsModCmntAbsStartPos_BadCommentType ) 
        ; RETURN 0 
        END (* TYPECASE *) 
      END NpsModCmntAbsStartPos 

  ; PROCEDURE NpsRescanModCmntTrailers ( ModCmntRef : ModHs . ModCmntTyp ) 
    RAISES { Backout } 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = VAR LConsumedCt : Strings . StringSsTyp    

    ; BEGIN (* NpsRescanModCmntTrailers *) 
        Assert 
          ( NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo 
            = NpsSeFsRef . SeFsNodeRef . FsFmtNo 
          , AFT . A_NpsRescanModCmntTrailersLostMod 
          ) 
      ; IF ModCmntRef . ModCmntNlAfter 
        THEN 
          DEC ( ParseInfo . PiLineCtIncr 
              , ORD ( NpsResultStateRef . PtsPrevTokAfter 
                      # LbeStd . Tok__BegOfLine 
                    ) 
              ) 
        ; NpsDeliverStringToScanner 
            ( StringNewLine 
            , NpsSeFsRef . SeFsIndentPos 
            , AreAllBlanks := FALSE 
            , ConsumedCt := LConsumedCt (* Ignore. *) 
            ) 
        ; NpsPassNewLine ( ) 
          (* ^NpsDeliverStringToScanner will have done this already, but we
              want to someday eliminate it there. *) 
        ELSE 
          NpsResultStateEstRef . PtsePrevTokBefore := LbeStd . Tok__Cmnt
        END (* IF *) 
      (* Finish with the comment mod. *) 
      ; TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo  ) 
      ; IF ISTYPE ( ModCmntRef , ModHs . ModCmntTrailingTyp ) 
        THEN 
          NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
        ELSE 
          NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
        END (* IF *) 
      END NpsRescanModCmntTrailers 

  ; PROCEDURE NpsModTextTrailers ( ModTextRef : ModHs . ModTextTyp ) 
    RAISES { Backout } 
    (* PRE NpsNextTempMarkIsRelevant is meaningful. *) 

    = VAR LConsumedCt : Strings . StringSsTyp    

    ; BEGIN (* NpsModTextTrailers *) 
        Assert 
          ( NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo 
            = NpsSeFsRef . SeFsNodeRef . FsFmtNo 
          , AFT . A_NpsModTextTrailers_LostMod 
          )
      ; Assert 
          ( NOT NpsNextTempMarkIsRelevant 
          , AFT . A_NpsModTextTrailers_RemainingTempMarks
          )  
      (* Handle trailing blanks or trailing new line. *) 
      ; IF NpsResultStateRef . PtsScanInfo . SiScanState # LbeStd . SsIdle 
        THEN (* Scanning. *) 
          IF ModTextRef . ModTextToPos = LbeStd . LimitedCharNoInfinity 
          THEN (* Scanning at end of line. *) 
            DEC ( ParseInfo . PiLineCtIncr 
                , ORD 
                    ( NpsResultStateRef . PtsPrevTokAfter 
                      # LbeStd . Tok__BegOfLine 
                    ) 
                ) 
          ; NpsDeliverStringToScanner 
              ( StringNewLine 
              , NpsSeFsRef . SeFsIndentPos 
              , AreAllBlanks := FALSE 
              , ConsumedCt := LConsumedCt (* Ignore. *) 
              ) 
          ; NpsPassNewLine ( ) 
            (* ^NpsDeliverStringToScanner will have done this already, but we
                want to someday eliminate the call there. *) 
          ; NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
          ELSE (* Scan some blanks. *)  
            Assert 
              ( NpsResultStateRef . PtsScanInfo . SiCharPos 
                <= ModTextRef . ModTextToPos 
              , AFT . A_NpsModTextTrailersNoTrailingSep 
              ) 
          ; NpsResultStateEstRef . PtseRescanToPos  
              := ModTextRef . ModTextToPos 
          ; NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenLeadingMods
          ; NpsRescanBlanks ( ) 
          ; NpsResultStateEstRef . PtsePrevTokBefore := LbeStd . Tok__ModText
          ; NpsResultStateEstRef . PtseModTextIsToLeftOnLine := TRUE 
          END (* IF *) 
        ELSE (* Not scanning. *) 
          IF ModTextRef . ModTextToPos = LbeStd . LimitedCharNoInfinity 
          THEN (* At end of line. *) 
            NpsPatchCmntNlAfter ( ) 
          ; NpsPassNewLine ( ) 
          ELSE 
            NpsResultStateEstRef . PtsePrevTokBefore := LbeStd . Tok__ModText
          ; NpsResultStateEstRef . PtseModTextIsToLeftOnLine := TRUE 
          END (* IF *) 
        ; NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
        END (* IF *) 
      (* Finish with the text mod. *) 
      ; TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo ) 
      END NpsModTextTrailers 

  ; PROCEDURE NpsRescanString ( ) RAISES { Backout } 
    (* PRE NpsNextTempMarkIsRelevant is meaningful. *) 

    = VAR LConsumedCt : Strings . StringSsTyp   
    ; VAR LSubstring : Strings . T   

    ; BEGIN (* NpsRescanString *) 
        (* Loop thru the fragments of the string that the scanner 
           consumes, delivering them to scanner. *) 
        LOOP 
          IF NpsResultStateRef . PtsScanInfo . SiCharPos 
             >= NpsResultStateEstRef . PtseRescanToPos 
          THEN (* Done with existing string. Change state kind and exit. *) 
            Assert 
              ( NpsResultStateRef . PtsScanInfo . SiCharPos 
                = NpsResultStateEstRef . PtseRescanToPos 
              , AFT . A_NpsRescanStringTooFar 
              ) 
          ; CASE NpsResultStateKind ( ) 

            OF ParseHs . ParseTravStateKindTyp . PtsKindInsideLexErrChars
            => NpsResultStateEstRef . PtsePrevTokBefore 
                 := SharedStrings . Tok 
                      ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                        . LeChildRef  
                      ) 
            ; TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo  ) 
            ; NpsResultStateEstRef . PtseStateKind 
                := ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods  

            | ParseHs . ParseTravStateKindTyp . PtsKindInsideAstString 
            => NpsResultStateEstRef . PtsePrevTokBefore 
                 := SharedStrings . Tok 
                      ( NpsSeEstRef . SeEstTravInfo . EtiStringRef ) 
            ; NpsResultStateEstRef . PtseStateKind 
                := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 

            | ParseHs . ParseTravStateKindTyp . PtsKindInsideInsTok 
            => NpsResultStateEstRef . PtsePrevTokBefore 
                 := NpsSeFsRef . SeFsNodeRef . FsTok 
            ; NpsResultStateEstRef . PtseStateKind 
                 := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 

            | ParseHs . ParseTravStateKindTyp . PtsKindInsideModCmnt 
            => NpsRescanModCmntTrailers 
                 ( NARROW 
                     ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                       . LeChildRef 
                     , ModHs . ModCmntTyp 
                     ) 
                 ) 

            | ParseHs . ParseTravStateKindTyp . PtsKindInsideModText 
            => NpsModTextTrailers 
                 ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef )
               (* ^Implicit NARROW can't fail. *)
            ELSE 
              CantHappen ( AFT . A_NpsRescanStringBadStateKind ) 
            END (* CASE *) 
          ; EXIT 
          END (* IF *) 
        (* Give scanner the remaining suffix of PtseStringRef. *)  
        ; LSubstring 
            := Strings . Substring 
                  ( SharedStrings . ToString 
                      ( NpsResultStateEstRef . PtseStringRef ) 
                  , From := NpsResultStateRef . PtsScanInfo . SiCharPos 
                            - NpsResultStateEstRef . PtseStringFromPos 
                  , For := NpsResultStateEstRef . PtseRescanToPos 
                           - NpsResultStateRef . PtsScanInfo . SiCharPos 
                  ) 
(* TODO: ^Improve efficiency here. *) 
        ; IF Strings . Length ( LSubstring ) <= 0  
          THEN (* Shouldn't happen *) 
            EXIT 
          ELSE             
            NpsDeliverStringToScanner 
              ( LSubstring 
              , NpsSeFsRef . SeFsIndentPos 
              , AreAllBlanks := FALSE 
              , ConsumedCt := LConsumedCt (* Ignore. *) 
              ) 
          (* Exit if we are ready to deliver a token and its mods. *) 
          ; IF NpsDeliverState = DeliverStateTyp . DsDeliver 
            THEN 
              EXIT 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      END NpsRescanString 

  ; PROCEDURE NpsRescanModCmnt ( ) RAISES { Backout } 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = BEGIN (* NpsRescanModCmnt *) 
        Assert 
          ( ISTYPE 
              ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef 
              , ModHs . ModCmntTyp 
              ) 
          , AFT . A_NpsRescanModCmnt_NotModCmnt 
          ) 
      ; Assert 
          ( NpsResultStateEstRef . PtseStringFromPos 
            = NpsResultStateRef . PtsScanInfo . SiCharPos 
          , AFT . A_NpsRescanModCmnt_WrongCharPos 
          ) 
      ; NpsResultStateEstRef . PtseRescanToPos 
          := NpsResultStateRef . PtsScanInfo . SiCharPos 
             + SharedStrings . Length ( NpsResultStateEstRef . PtseStringRef )
      ; NpsResultStateEstRef . PtseStateKind 
          := ParseHs . ParseTravStateKindTyp . PtsKindInsideModCmnt 
      ; NpsRescanString ( ) 
      END NpsRescanModCmnt 

  ; PROCEDURE NpsRescanModText ( ModTextRef : ModHs . ModTextTyp ) 
    RAISES { Backout } 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = BEGIN (* NpsRescanModText *) 
        NpsResultStateRef . PtsScanInfo . SiCharPos 
          := ModTextRef . ModTextFromPos 
      ; IF NpsResultStateRef . PtsScanInfo . SiScanState = LbeStd . SsIdle 
        THEN NpsResultStateRef . PtsScanInfo . SiLeftTokToPos 
               := ModTextRef . ModTextLeftTokToPos 
        END (* IF *) 
      ; NpsResultStateEstRef . PtseStateKind 
          := ParseHs . ParseTravStateKindTyp . PtsKindInsideModText 
      ; IF NpsResultStateEstRef . PtseStringRef = NIL
           OR SharedStrings . Length ( NpsResultStateEstRef . PtseStringRef ) 
              = 0  
        THEN (* The ModText has an empty string. *) 
          (* Patch and include all TempMarks of the empty TextMod.
             BuildTempMarks will have ensured that they exist only on TextMods
             of a line containing nothing but empty TextMods.  In this
             special case, we will end up calling NpsAccumScannedBlankLine, 
             which will produce a BlankLine and patch the TempMarks to it. *)  
          WHILE NpsNextTempMarkIsRelevant 
          DO WITH WTempMark 
                  = ParseInfo . PiTravTempMarkListRef ^ [ NpsNextTempMarkSs ] 
            DO 
              WTempMark . CharPos 
                := EstUtil . WidthSumSigned  
                     ( WTempMark . CharPos , ModTextRef . ModTextFromPos ) 
            END (* WITH *) 
          ; NpsIncludeTempMark ( ) 
          ; NpsCheckNextTempMarkForEstLeaf 
              ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef ) 
          END (* WHILE *)  
        ; NpsModTextTrailers ( ModTextRef ) 
        ELSE 
          NpsResultStateEstRef . PtseRescanToPos 
            := NpsResultStateRef . PtsScanInfo . SiCharPos 
               + SharedStrings . Length 
                   ( NpsResultStateEstRef . PtseStringRef )
        ; NpsRescanString ( ) 
        END (* IF *) 
      END NpsRescanModText 

  ; PROCEDURE NpsRescanInsTok ( ) RAISES { Backout } 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = BEGIN (* NpsRescanInsTok *) 
        NpsResultStateEstRef . PtseRescanToPos 
          := NpsResultStateRef . PtsScanInfo . SiCharPos 
             + SharedStrings . Length ( NpsResultStateEstRef . PtseStringRef )
      ; NpsResultStateEstRef . PtseStateKind 
          := ParseHs . ParseTravStateKindTyp . PtsKindInsideInsTok 
      ; NpsRescanString ( ) 
      END NpsRescanInsTok 

  ; PROCEDURE NpsRescanLexErrChars ( ) RAISES { Backout }
    (* PRE: Not scanning the chars yet. *)
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = BEGIN (* NpsRescanLexErrChars *) 
        NpsResultStateEstRef . PtseRescanToPos 
          := NpsResultStateRef . PtsScanInfo . SiCharPos 
             + SharedStrings . Length ( NpsResultStateEstRef . PtseStringRef )
      ; NpsResultStateEstRef . PtseStateKind 
          := ParseHs . ParseTravStateKindTyp . PtsKindInsideLexErrChars 
      ; NpsRescanString ( ) 
      END NpsRescanLexErrChars 

  ; PROCEDURE NpsRescanAstString ( ) RAISES { Backout } 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = BEGIN (* NpsRescanAstString *) 
        NpsResultStateEstRef . PtseRescanToPos 
          := NpsResultStateRef . PtsScanInfo . SiCharPos 
             + SharedStrings . Length ( NpsResultStateEstRef . PtseStringRef )
      ; NpsResultStateEstRef . PtseStateKind 
          := ParseHs . ParseTravStateKindTyp . PtsKindInsideAstString 
      ; NpsRescanString ( ) 
      END NpsRescanAstString 

  ; PROCEDURE NpsRescanBlanks ( ) RAISES { Backout } 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = VAR LConsumedCt : Strings . StringSsTyp    
    ; VAR LModCmntRef : ModHs . ModCmntTyp 

    ; BEGIN (* NpsRescanBlanks *) 
(* TODO: Consider just replacing this with 4 different procedures, one
         for each case of what to do after the blanks are done. *) 
        LOOP 
          IF NpsDeliverState = DeliverStateTyp . DsDeliver 
          THEN 
            EXIT 
          ELSE 
            IF NpsResultStateRef . PtsScanInfo . SiScanState = LbeStd . SsIdle 
            THEN (* No further scan needed. Consume the rest of 
                    the blanks and handle the following token *) 
              NpsResultStateRef . PtsScanInfo . SiCharPos
                := NpsResultStateEstRef . PtseRescanToPos 
            END (* IF *) 
          ; IF NpsResultStateRef . PtsScanInfo . SiCharPos
               >= NpsResultStateEstRef . PtseRescanToPos  
            THEN (* Done with the blanks.  Decide what is next. *) 
              Assert 
                ( NpsResultStateRef . PtsScanInfo . SiCharPos
                  = NpsResultStateEstRef . PtseRescanToPos
                , AFT . A_NpsRescanBlanks_OverconsumedBlanks  
                ) 
            ; CASE NpsResultStateKind ( ) 
              OF ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenLeadingMods
              => NpsResultStateEstRef . PtseStateKind 
                   := ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
              | ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenModCmnt
              => LModCmntRef 
                  := NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef
                 (* ^Implicit NARROW can't fail. *)
              ; IF LModCmntRef . ModCmntBegScanState 
                   # NpsResultStateRef . PtsScanInfo . SiScanState 
                   OR NpsResultStateRef . PtsScanInfo . SiScanState 
                      = LbeStd . SsMaxCmnt 
                THEN (* Must scan the comment. *)  
                  NpsRescanModCmnt ( ) 
                ELSIF ISTYPE ( LModCmntRef , ModHs . ModCmntTrailingTyp )  
                THEN 
                  NpsResultStateEstRef . PtseStateKind 
                    := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
                  (* ^Revisit the comment. *)  
                ELSE
                  NpsResultStateEstRef . PtseStateKind 
                    := ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
                  (* ^Revisit the comment. *)  
                END (* IF *) 
              | ParseHs . ParseTravStateKindTyp 
                . PtsKindBlanksThenRescanModText 
              => (* Always rescan ModText. *) 
                NpsRescanModText 
                  ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                    . LeChildRef 
                    (* ^Implicit NARROW can't fail. *)
                  ) 
              | ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenInsTok
              => IF NpsResultStateRef . PtsScanInfo . SiScanState 
                    = LbeStd . SsIdle 
                 THEN 
                   NpsInsTok ( ) 
                 ELSE 
                   NpsRescanInsTok ( ) 
                 END (* IF *) 
              | ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenLexErrChars 
              => IF NpsResultStateRef . PtsScanInfo . SiScanState 
                    = LbeStd . SsIdle 
                 THEN 
                   NpsBeginLexErrChars ( ) 
                 ELSE 
                   NpsRescanLexErrChars ( ) 
                 END (* IF *) 
              | ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenAstString 
              => IF NpsResultStateRef . PtsScanInfo . SiScanState 
                    = LbeStd . SsIdle 
                 THEN 
                   NpsBeginAstString ( ) 
                 ELSE 
                   NpsRescanAstString ( ) 
                 END (* IF *) 
              ELSE 
                CantHappen ( AFT . A_NpsRescanBlanksBadStateKind ) 
              END (* CASE *) 
            ; EXIT 
            ELSE 
              NpsDeliverStringToScanner 
                ( Strings . Substring 
                    ( StringAllBlanks 
                    , 0 
                    , NpsResultStateEstRef . PtseRescanToPos  
                      - NpsResultStateRef . PtsScanInfo . SiCharPos
                    ) 
                , NpsSeFsRef . SeFsIndentPos 
                , AreAllBlanks := TRUE 
                , ConsumedCt := LConsumedCt (* Ignore. *)  
                ) 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      END NpsRescanBlanks 

  ; PROCEDURE NpsRescanTok 
      ( <* UNUSED *> Tok : LbeStd . TokTyp 
      ; StringRef : SharedStrings . T 
      ; StateKindForBlanks : ParseHs . ParseTravStateKindTyp 
      ; StateKindAfterBlanks : ParseHs . ParseTravStateKindTyp 
      ) 
    RAISES { Backout } 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = BEGIN (* NpsRescanTok *) 
        NpsResultStateEstRef . PtseStringRef := StringRef 
      ; NpsResultStateEstRef . PtseStringFromPos 
          := TravUtil . PosForTok 
               ( ParseInfo . PiLang 
               , NpsSeFsRef . SeFsFmtKind 
               , NpsResultStateEstRef . PtseModTextIsToLeftOnLine 
               , NpsResultStateRef . PtsScanInfo . SiCharPos 
               , NpsSeEstRef . SeEstIndentPos 
               , NpsSeFsRef . SeFsNodeRef . FsIndentCode 
               , NpsResultStateEstRef . PtsePrevTokBefore 
               , NpsSeFsRef . SeFsNodeRef . FsTok 
               ) 
      ; IF NpsResultStateRef . PtsScanInfo . SiCharPos 
           < NpsResultStateEstRef . PtseStringFromPos 
        THEN (* Blanks are needed *) 
          NpsResultStateEstRef . PtseStateKind := StateKindForBlanks 
        ; NpsResultStateEstRef . PtseRescanToPos 
            := NpsResultStateEstRef . PtseStringFromPos 
        ; NpsRescanBlanks ( ) 
        ELSE (* No blanks were needed. *) 
          NpsResultStateEstRef . PtseStateKind := StateKindAfterBlanks 
        END (* IF *) 
      END NpsRescanTok 

  ; PROCEDURE NpsInsTok ( ) RAISES { Backout } 

    = VAR LDeferredInfo : ParseHs . DeferredInfoRefTyp
    
    ; BEGIN (* NpsInsTok *) 
        (* NpsWaitingTempMarkRange may contain leftover TempMarks from proposed
           insertion repairs alone on a line. *) 
        IF NpsResultStateRef . PtsScanInfo . SiScanState = LbeStd . SsIdle 
        THEN (* Need not scan, deliver the token *) 
        (* Include any TempMarks in the token's range. 
           No rebias is necessary, since we will not rescan it. 
           Include them in the ranges and deliver this to Parser.
           Patching TempMarks for FmtNo tempmarks is all left to Parser. 
        *) 
          LOOP 
            NpsCheckNextTempMarkForInsTok 
              ( NpsSeFsRef . SeFsNodeRef . FsFmtNo ) 
          ; IF NOT NpsNextTempMarkIsRelevant 
            THEN EXIT 
            ELSE NpsIncludeTempMark ( ) 
            END (* IF *) 
          END (* LOOP *)
          
        ; CASE NpsDeliverState <* NOWARN *>
          OF DeliverStateTyp . DsStarting 
          => NpsResultStateRef . PtsTokInfo . TiTok 
               := NpsSeFsRef . SeFsNodeRef . FsTok 
          ; LbeStd . IncLimitedTokCt 
              ( NpsResultStateRef . PtsTokInfo . TiSyntTokCt ) 
          ; NpsResultStateRef . PtsPrevTokAfter  
              := NpsResultStateRef . PtsTokInfo . TiTok 
          ; NpsResultStateEstRef . PtsePrevTokBefore 
              := NpsResultStateRef . PtsTokInfo . TiTok 
          (* No EstRef to accumulate for this kind of token *) 
          ; NpsAppendTempMarkRange 
              ( (* VAR *) ToRange 
                := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
              ) 
          ; NpsAppendTempMarkRange 
              ( (* VAR *) ToRange 
                := NpsResultStateRef . PtsTokInfo . TiPatchTempMarkRange 
              ) 
          ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
          ; NpsDeliverState := DeliverStateTyp . DsTokFound
          
          | DeliverStateTyp . DsTokFound 
          => (* This is the following token.  It means no (more) trailing 
                mods, so defer it and deliver what went before. *) 
            LDeferredInfo := NpsGetDeferredInfoRef ( ) 
          ; LDeferredInfo . Tok := NpsSeFsRef . SeFsNodeRef . FsTok 
          ; LDeferredInfo . SyntTokCt := 1 
          ; LDeferredInfo . IsInsertionRepair := FALSE 
          ; LDeferredInfo . IsInterior := FALSE 
          ; LDeferredInfo . FullTempMarkRange := NpsWaitingTempMarkRange 
          ; LDeferredInfo . PatchTempMarkRange := NpsWaitingTempMarkRange 
          ; LDeferredInfo . WaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
          ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty (* Dead. *) 
          ; LDeferredInfo . ObjRef := NIL 
          ; LDeferredInfo . KindSet := EstHs . EstChildKindSetEmpty 
          ; NpsDeliverState := DeliverStateTyp . DsDeliver 
          END (* CASE *) 
        ; NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
        ELSE (* Rescanning. *) 
          NpsCheckNextTempMarkForInsTok 
            ( NpsSeFsRef . SeFsNodeRef . FsFmtNo ) 
        ; NpsRescanTok 
            ( NpsSeFsRef . SeFsNodeRef . FsTok
            , NpsSeFsRef . SeFsNodeRef . FsInsTokRef  
            , ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenInsTok 
            , ParseHs . ParseTravStateKindTyp . PtsKindRescanInsTok 
            ) 
        END (* IF *) 
      END NpsInsTok 

  ; PROCEDURE NpsLineBreak ( ) 
    RAISES { Backout } 

    = VAR LConsumedCt : Strings . StringSsTyp    

    ; BEGIN (* NpsLineBreak *) 
        NpsNextTempMarkIsRelevant := FALSE 
        (* LineBreaks never have TempMarks. *) 
      ; IF NpsSeFsRef . SeFsNodeRef . FsKind 
           = FsKindTyp . FsKindLineBreakReqd 
           OR TravUtil . DoTakeLineBreak 
                ( Lang := ParseInfo . PiLang 
                , CharPos := NpsResultStateRef . PtsScanInfo . SiCharPos 
                , ModTextIsToLeftOnLine 
                    := NpsResultStateEstRef . PtseModTextIsToLeftOnLine 
                , PrevTok := NpsResultStateEstRef . PtsePrevTokBefore 
                , RootFsNodeRef := NpsSeEstRef . SeEstRootFsNodeRef 
                , LineBreakFsNodeRef := NpsSeFsRef . SeFsNodeRef  
                , ParentFmtKind := NpsSeFsRef . SeFsFmtKind 
                , CurrentLineIndentPos := NpsSeEstRef . SeEstIndentPos 
                , EstTravInfo := NpsSeEstRef . SeEstTravInfo  
                , (* IN OUT *) LastFmtNoOnLine 
                    := NpsResultStateEstRef . PtseLastFmtNoOnLine
                , (*IN OUT *) EstListChildrenToPass 
                    := NpsResultStateEstRef . PtseEstListChildrenToPass
                ) 
        THEN (* Take the line break. *)  
          DEC ( ParseInfo . PiLineCtIncr 
              , ORD ( NpsResultStateRef . PtsPrevTokAfter 
                      # LbeStd . Tok__BegOfLine 
                    ) 
              ) 
        ; IF NpsResultStateRef . PtsScanInfo . SiScanState # LbeStd . SsIdle 
          THEN (* Scanning *) 
            IF NpsResultStateRef . PtsScanInfo . SiCharPos > 0 
               (* Not already at BegOfLine. *) 
            THEN 
              NpsDeliverStringToScanner 
                ( StringNewLine 
                , NpsSeFsRef . SeFsIndentPos 
                , AreAllBlanks := FALSE 
                , ConsumedCt := LConsumedCt (* Ignore. *) 
                ) 
            END (* IF *) 
          ELSE (* Not scanning *) 
            NpsResultStateRef . PtsPrevTokAfter := LbeStd . Tok__BegOfLine 
          ; IF NpsDeliverState = DeliverStateTyp . DsTokFound 
            THEN (* Still wait to see what's on the next line. *)
(*CHECK: Is this what we want? *)
              (* NpsDeliverState := DeliverStateTyp . DsDeliver *) 
            END (* IF *) 
          END (* IF *) 
        ; NpsPassNewLine ( ) 
        ; NpsSeEstRef . SeEstIsFirstLine := FALSE 
        ; NpsSeEstRef . SeEstIndentPos := NpsSeEstRef . SeEstIndentPosN 
        END (* IF *) 
      ; NpsResultStateEstRef . PtseStateKind 
          := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
      END NpsLineBreak 

  ; PROCEDURE NpsFsNodeAfterLeadingMods ( ) RAISES { Backout } 

    = VAR LConsumedCt : Strings . StringSsTyp   

    ; BEGIN (* NpsFsNodeAfterLeadingMods *) 
        CASE NpsSeFsRef . SeFsNodeRef . FsKind 
        OF FsKindTyp . FsKindEndOfImage 
        => IF NpsResultStateRef . PtsScanInfo . SiScanState # LbeStd . SsIdle 
          THEN (* End of Est, but still scanning, give scanner eof *) 
            NpsCheckNextTempMarkForInsTok 
              ( NpsSeFsRef . SeFsNodeRef . FsFmtNo ) 
(* CHECK: Do we need to deliver a new line first? *) 
          ; NpsDeliverStringToScanner 
              ( StringEndOfImage 
              , NpsSeFsRef . SeFsIndentPos 
              , AreAllBlanks := FALSE 
              , ConsumedCt := LConsumedCt (* Ignore. *) 
              ) 
(* CHECK: Do we need to call NpsPassNewLine? *) 
          ELSIF NpsDeliverState = DeliverStateTyp . DsTokFound  
          THEN 
            NpsDeliverState := DeliverStateTyp . DsDeliver 
          ELSE 
            NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindEndOfImage 
          END (* IF *) 

        | FsKindTyp . FsKindInsTok 
        => NpsInsTok ( ) 

        | FsKindTyp . FsKindEstChildOfFixed 
        => TravUtil . AssertFwdNoLostFixedChild 
             ( NpsSeFsRef . SeFsNodeRef , NpsSeEstRef . SeEstTravInfo ) 
        ; IF NpsSeEstRef . SeEstTravInfo . EtiChildNo 
             >= NpsSeEstRef . SeEstTravInfo . EtiChildCt 
             OR NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo 
                > NpsSeFsRef . SeFsNodeRef . FsFmtNo 
             OR NOT EstHs . EstChildKindEstChild 
                    IN NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                       . LeKindSet 
          THEN (* Est Child is absent. *) 
            NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
          ELSE
            Assert 
              ( NpsSeFsRef . SeFsNodeRef . FsFmtNo 
                = NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo 
              , AFT . A_NpsFsNodeAfterLeadingModsLowEstChildOfFixed 
              ) 
          ; IF ISTYPE ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                        . LeChildRef 
                      , ModHs . EstDummyTyp 
                      ) 
            THEN (* NIL or Dummy. Skip it. *) 
              TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo  ) 
            ; NpsResultStateEstRef . PtseStateKind 
                := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
            ELSE 
              NpsResultStateEstRef . PtseStateKind 
                := ParseHs . ParseTravStateKindTyp . PtsKindNewEst 
            END (* IF *) 
          END (* IF *) 

        | FsKindTyp . FsKindEstChildOfList 
        => Assert 
            ( NpsSeEstRef . SeEstTravInfo . EtiChildNo 
              < NpsSeEstRef . SeEstTravInfo . EtiChildCt 
            , AFT . A_NpsFsNodeAfterLeadingModsListNoChildAtAll 
            ) 
        ; Assert 
            ( NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo 
              = NpsSeFsRef . SeFsNodeRef . FsFmtNo 
            , AFT . A_NpsFsNodeAfterLeadingModsListFmtNoMismatch 
            ) 
        ; Assert 
            ( EstHs . EstChildKindEstChild 
              IN NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeKindSet 
            , AFT . A_NpsFsNodeAfterLeadingModsListNoEstChild 
            ) 
        ; IF ISTYPE ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                      . LeChildRef 
                    , ModHs . EstDummyTyp 
                    ) 
             (* ^Including NIL *) 
             OR EstHs . EstChildKindTrailingSep  
                IN NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeKindSet 
          THEN (* Est Child is a dummy, NIL, or a trailing sep.  Skip it. *) 
            TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo  ) 
          ; NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
          ELSE 
            NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindNewEst 
          END (* IF *) 

        | FsKindTyp . FsKindLineBreakOpt 
        , FsKindTyp . FsKindLineBreakReqd  
        => NpsLineBreak ( ) 

        ELSE 
          CantHappen ( AFT . A_NpsFsNodeAfterLeadingModsBadFsKind ) 
        END (* CASE *) 
      END NpsFsNodeAfterLeadingMods 

  ; PROCEDURE NpsRescanModBlankLine ( ) RAISES { Backout } 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = VAR LConsumedCt : Strings . StringSsTyp   

    ; BEGIN (* NpsRescanModBlankLine *) 
        Assert 
          ( NpsSeFsRef . SeFsNodeRef . FsFmtNo 
            = NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo 
          , AFT . A_NpsRescanModBlankLineLostMod 
          ) 
      ; DEC ( ParseInfo . PiLineCtIncr 
            , ORD ( NpsResultStateRef . PtsPrevTokAfter 
                    # LbeStd . Tok__BegOfLine 
                  ) 
            ) 
      ; NpsDeliverStringToScanner 
          ( StringNewLine 
          , NpsSeFsRef . SeFsIndentPos 
          , AreAllBlanks := FALSE 
          , ConsumedCt := LConsumedCt (* Ignore. *) 
          ) 
        (* Nl before of the blank line.  
           We will slice this blank line next time thru. *) 
(* CHECK: What happens if we get a blankline while in SsMaxCmnt? *) 
      ; NpsPassNewLine ( ) 
      END NpsRescanModBlankLine 

  ; PROCEDURE NpsRescanModCmntLeaders ( ModCmntRef : ModHs . ModCmntTyp ) 
    RAISES { Backout } 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = VAR LConsumedCt : Strings . StringSsTyp    
    ; VAR LMustDeliverNlBefore : BOOLEAN 

    ; BEGIN (* NpsRescanModCmntLeaders *) 
        Assert 
          ( NpsSeFsRef . SeFsNodeRef . FsFmtNo 
            = NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo 
          , AFT . A_NpsRescanModCmntLeadersLostMod 
          ) 
      (* We are rescanning at the beginning of this comment (otherwise, 
         it would have been part of a slice, with possible other non-syntactic 
         mods). However, the leading new line/blanks could put us into a state 
         where further rescanning of the comment text itself is unnecessary *) 
      ; Assert 
          ( ModCmntRef . ModCmntBegScanState 
            # NpsResultStateRef . PtsScanInfo . SiScanState 
            OR ModCmntRef . ModCmntBegScanState = LbeStd . SsMaxCmnt  
          , AFT . A_NpsRescanModCmntLeadersShouldBeSliced 
          ) 
      ; Assert 
          ( NpsResultStateRef . PtsScanInfo . SiCharPos 
            # FIRST ( Strings . StringSsSignedTyp ) 
          , AFT . A_NpsRescanModCmntLeadersUnknownSiCharPos 
          ) 

      (* Deliver new line before, if appropriate. *) 
      ; IF ModCmntRef . ModCmntNlBefore 
           AND NpsResultStateRef . PtsScanInfo . SiCharPos > 0 
        THEN LMustDeliverNlBefore := TRUE 
        ELSE
          TYPECASE ModCmntRef 
          OF ModHs . ModCmntLeadingFixedTyp 
          , ModHs . ModCmntTrailingFixedTyp 
          => LMustDeliverNlBefore 
                := EstUtil . WidthSumSigned  
                     ( NpsResultStateRef . PtsScanInfo . SiCharPos 
                     , ORD 
                         ( LangUtil . NeedsSep 
                             ( ParseInfo . PiLang 
                             , NpsResultStateRef . PtsPrevTokAfter 
                             , LbeStd . Tok__Cmnt 
                             ) 
                         ) 
                     ) 
                   > ModCmntRef . ModCmntFromPos 
                   (* ^Implicit new line. *) 
          ELSE LMustDeliverNlBefore := FALSE 
          END (* TYPECASE *) 
        END (* IF *) 
      ; IF LMustDeliverNlBefore  
        THEN (* Deliver a new line. *) 
          DEC ( ParseInfo . PiLineCtIncr 
              , ORD ( NpsResultStateRef . PtsPrevTokAfter 
                      # LbeStd . Tok__BegOfLine 
                    ) 
              ) 
        ; NpsDeliverStringToScanner 
            ( StringNewLine 
            , NpsSeFsRef . SeFsIndentPos 
            , AreAllBlanks := FALSE 
            , ConsumedCt := LConsumedCt (* Ignore. *) 
            ) 
        ; NpsPassNewLine ( ) 
          (* We could slice next time, or could come back thru here, skipping 
             this new line case, dropping to the ELSE clause below. *) 
        ELSE 
        (* The remaining steps must not make the condition of the 
           if-newline-needed statement above become true, because control needs
           to be able to come back here for the same mod. *) 
          NpsResultStateEstRef . PtseStringRef 
            := ModCmntRef . ModCmntStringRef 
        ; NpsResultStateEstRef . PtseStringFromPos 
            := NpsModCmntAbsStartPos ( ModCmntRef ) 
        ; NpsCheckNextTempMarkForEstLeaf 
            ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef ) 
        ; IF NpsResultStateRef . PtsScanInfo . SiCharPos 
             < NpsResultStateEstRef . PtseStringFromPos 
          THEN (* Leading blanks needed.  Rescan them. *) 
            NpsResultStateEstRef . PtseRescanToPos 
              := NpsResultStateEstRef . PtseStringFromPos 
          ; NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenModCmnt 
          ; NpsRescanBlanks ( ) 
          ELSE 
            NpsRescanModCmnt ( ) 
          END (* IF *) 
        END (* IF *) 
      END NpsRescanModCmntLeaders 

  ; PROCEDURE NpsModTextLeaders ( ModTextRef : ModHs . ModTextTyp ) 
    RAISES { Backout } 
    (* We always rescan these, even if we weren't scanning before. *) 
    (* PRE: NpsNextTempMarkIsRelevant is meaningful. *) 

    = VAR LConsumedCt : Strings . StringSsTyp   

    ; BEGIN (* NpsModTextLeaders *) 
        Assert 
          ( NpsSeFsRef . SeFsNodeRef . FsFmtNo 
            = NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo 
          , AFT . A_NpsModTextLeadersLostMod 
          ) 
      ; NpsResultStateEstRef . PtseStringRef := ModTextRef . ModTextStringRef 
      ; NpsResultStateEstRef . PtseStringFromPos 
          := ModTextRef . ModTextFromPos 
      ; NpsCheckNextTempMarkForEstLeaf 
          ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef ) 
      ; IF NpsResultStateRef . PtsScanInfo . SiScanState = LbeStd . SsIdle 
        THEN (* Not already scanning. *) 
          NpsRescanModText ( ModTextRef ) 
        ELSE (* We were already rescanning. *) 
          Assert 
            ( NpsResultStateRef . PtsScanInfo . SiCharPos 
              # FIRST ( Strings . StringSsSignedTyp )  
            , AFT . A_NpsModTextLeadersUnknownSiCharPos 
            )  
        ; IF NpsResultStateEstRef . PtseStateKind 
             = ParseHs . ParseTravStateKindTyp . PtsKindInsideModText
          THEN  (* We were already scanning inside the ModText, continue. *)  
            NpsRescanModText ( ModTextRef ) 
          ELSIF ModTextRef . ModTextLeftTokToPos = 0 
             AND NpsResultStateRef . PtsPrevTokAfter # LbeStd . Tok__BegOfLine 
                 (* ^Defensive, in case we start at non-rightmost Nl *) 
(* CHECK: Consistify.  Nl before of comment checks SiCharPos = 0, instead
          of PtsPrevTokAfter = Tok__BegOfLine, as does line break. *) 
          THEN (* New line needed. *) 
            DEC ( ParseInfo . PiLineCtIncr 
                , ORD 
                    ( NpsResultStateRef . PtsPrevTokAfter 
                    # LbeStd . Tok__BegOfLine 
                    ) 
                ) 
          ; NpsDeliverStringToScanner 
              ( StringNewLine 
              , NpsSeFsRef . SeFsIndentPos 
              , AreAllBlanks := FALSE 
              , ConsumedCt := LConsumedCt (* Ignore. *) 
              ) 
          ; NpsPassNewLine ( ) 
          (* We will come back here, next time with
             NpsResultStateRef . PtsPrevTokAfter = LbeStd . Tok__BegOfLine *)  
          ELSIF NpsResultStateRef . PtsScanInfo . SiCharPos 
                < NpsResultStateEstRef . PtseStringFromPos 
          THEN (* Leading blanks needed.  Rescan them. *) 
            NpsResultStateEstRef . PtseRescanToPos  
              := NpsResultStateEstRef . PtseStringFromPos 
          ; NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp 
                   . PtsKindBlanksThenRescanModText 
          ; NpsRescanBlanks ( ) 
          ELSE 
            NpsRescanModText ( ModTextRef ) 
          END (* IF *) 
        END (* IF *) 
      END NpsModTextLeaders 

  ; PROCEDURE NpsSliceMods 
      ( MustComputeDisplay : BOOLEAN 
(* TODO: ^All remaining calls now pass TRUE.  Can we fold this? *) 
      ; IncludeContainsTempMark : BOOLEAN 
      ; OnlyOneChild : BOOLEAN := TRUE 
      ) 
    RAISES { Backout } 
    (* Slice out as many leading or trailing mods in a group as possible. *) 

    = VAR LEstChildNo1 : LbeStd . EstChildNoTyp 
    ; VAR LEstChildNo2 : LbeStd . EstChildNoTyp 
    ; VAR LNodeNo2 : LbeStd . EstNodeNoTyp 
    ; VAR LKindSet : EstHs . EstChildKindSetTyp 
    ; VAR LEstChildLeafElem1 : EstHs . LeafElemTyp 
    ; VAR LEstChildLeafElem2 : EstHs . LeafElemTyp 
    ; VAR LCharPosIsInitialized : BOOLEAN 
    (*    ^ used only for an Assertion *) 
    ; VAR LScanStateIsInitialized : BOOLEAN 
    (*    ^ used only for an Assertion *) 
    ; VAR LMustAddContainsTempMark : BOOLEAN 

    ; BEGIN (* NpsSliceMods *) 
        Assert 
          ( NpsSeFsRef . SeFsNodeRef . FsFmtNo 
            = NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo 
          , AFT . A_NpsSliceModsLostMod 
          ) 

      (* Locate end of slicable group. *) 
      ; LEstChildNo1 := NpsSeEstRef . SeEstTravInfo . EtiChildNo 
      ; LEstChildLeafElem1 
          := NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
      ; LMustAddContainsTempMark 
          := IncludeContainsTempMark 
             AND NOT EstHs . EstChildKindContainsTempMark 
                     IN LEstChildLeafElem1 . LeKindSet 
      ; IF OnlyOneChild OR LMustAddContainsTempMark  
        THEN (* Select one child only. *) 
          TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo ) 
        ELSE 
          LKindSet 
            := EstHs . EstChildKindSetTyp 
                 { EstHs . EstChildKindEstChild 
                 , EstHs . EstChildKindContainsSyntMod 
                 , EstHs . EstChildKindContainsInsertionRepair 
                 , EstHs . EstChildKindContainsDeletionRepair 
                 , EstHs . EstChildKindContainsNoKnownNl  
                 , EstHs . EstChildKindFirstOfGroup 
                 } 
        ; IF NpsResultStateKind ( ) 
             = ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
          THEN (* Also stop on trailing mods. *) 
            LKindSet := LKindSet + EstHs . EstChildKindSetTrailingMod 
          END (* IF *) 
        ; TravUtil . SetToNextInKindSet 
            ( (* IN OUT *) NpsSeEstRef . SeEstTravInfo   
            , LEstChildNo1 + 1 
            , LKindSet 
            ) 
        END (* IF *) 

      (* Accumulate the sliced out mods. *)
      ; IF LMustAddContainsTempMark 
        THEN 
          NpsAccumNode 
            ( LEstChildLeafElem1 . LeChildRef 
            , LEstChildLeafElem1 . LeKindSet 
              + EstHs . EstChildKindSetContainsTempMark 
            , SyntTokCt := 0 
            ) 
        ELSE  
          IF EstHs . EstChildKindFirstOfGroup 
             IN LEstChildLeafElem1 . LeKindSet 
          THEN
            NpsFlushSlice ( ) 
          END (* IF *) 
        ; NpsAccumSlice 
            ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
            , FromEstChildNo := LEstChildNo1 
            , ToEstChildNo := NpsSeEstRef . SeEstTravInfo . EtiChildNo 
            , SyntTokCt := 0 
            ) 
        END (* IF *) 
      ; NpsSkipTempMarksForSubtrees 
          ( EstRef := NpsSeEstRef . SeEstTravInfo . EtiParentRef 
          , FromChildNo := LEstChildNo1 
          , ToChildNo := NpsSeEstRef . SeEstTravInfo . EtiChildNo
          ) 
        (* ^RightSib TempMarks pointing to RM child belong to a following 
            InsTok. *) 
(* Unnecessary? ; NpsResultStateEstRef . PtseTokTempMarkSs := NpsNextTempMarkSs *)
      ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
      ; NpsResultStateEstRef . PtseLastFmtNoOnLine := EstHs . FmtNoUnknown 
      ; NpsResultStateEstRef . PtseEstListChildrenToPass := 0 (* Dead. *) 
(* REVIEW: EstChildKindContainsNoKnownNl and EstChildKindContainsNoKnownNl
           are strictly opposites. *) 
(* Huh? *) 

      ; IF MustComputeDisplay 
           AND EstHs . EstChildKindContainsSyntMod  
               IN NpsSeEstRef . SeEstTravInfo . EtiParentRef . EstChildKindSet 
           (* We are below the locus of must-traverse nodes, iff 
              EstChildKindContainsSyntMod is absent from the containing
              Est.  There is only one set of ScanInfo for the whole 
              traverse, and it must be modified only linearly, over this
              locus.  Don't mess with it otherwise. *)  
        THEN 
          LCharPosIsInitialized := FALSE 
        ; LScanStateIsInitialized := FALSE 
        ; EstUtil . PrevInKindSet 
            ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
            , NpsSeEstRef . SeEstTravInfo . EtiChildNo - 1 
            , EstHs . EstChildKindSetTyp 
                { EstHs . EstChildKindDisplayComputable 
                , EstHs . EstChildKindFirstOfGroup 
                } 
            , (* VAR *) LEstChildNo2 
            , (* VAR *) LNodeNo2 
            , (* VAR *) LEstChildLeafElem2 
            ) 
        ; IF LEstChildNo2 < LEstChildNo1 
          THEN 
            LEstChildNo2 := LEstChildNo1 
          ; LEstChildLeafElem2 := LEstChildLeafElem1 
          END (* IF *) 
        ; Assert ( LEstChildNo2 >= 0 , AFT . A_NpsSliceModsNoEstChild2 ) 

        (* Loop thru the nonsyntactic mods at the end of the slice *) 
        ; LOOP 
            TYPECASE LEstChildLeafElem2 . LeChildRef 

            (* Comments *) 
            OF ModHs . ModCmntTyp ( TModCmntRef ) 
            => IF TModCmntRef . ModCmntNlAfter 
              THEN 
                NpsPassNewLine ( ) 
              ; LCharPosIsInitialized := TRUE 
              ELSE 
                NpsResultStateRef . PtsPrevTokAfter := LbeStd . Tok__Cmnt  
              ; NpsResultStateEstRef . PtsePrevTokBefore := LbeStd . Tok__Cmnt 
              ; TYPECASE TModCmntRef 
                OF ModHs . ModCmntLeadingFixedTyp 
                => NpsResultStateRef . PtsScanInfo . SiCharPos 
                     := TModCmntRef . ModCmntFromPos 
                ; LCharPosIsInitialized := TRUE 
                ; NpsResultStateEstRef . PtseModTextIsToLeftOnLine 
                    := NpsResultStateEstRef . PtseModTextIsToLeftOnLine 
                       AND NOT TModCmntRef . ModCmntNlBefore 
                | ModHs . ModCmntLeadingRelativeTyp 
                => NpsResultStateRef . PtsScanInfo . SiCharPos 
                     := EstUtil . WidthSumSigned  
                          ( NpsSeFsRef . SeFsIndentPos 
                          , TModCmntRef . ModCmntFromPos 
                          ) 
                ; LCharPosIsInitialized := TRUE 
                ; NpsResultStateEstRef . PtseModTextIsToLeftOnLine 
                    := NpsResultStateEstRef . PtseModTextIsToLeftOnLine 
                       AND NOT TModCmntRef . ModCmntNlBefore 
                | ModHs . ModCmntTrailingFixedTyp 
                => NpsResultStateRef . PtsScanInfo . SiCharPos 
                     := TModCmntRef . ModCmntFromPos 
                ; LCharPosIsInitialized := TRUE 
                | ModHs . ModCmntTrailingRelativeTyp 
                => Assert 
                     ( TRUE OR LCharPosIsInitialized 
                     , AFT . A_NpsSliceModsUninitializedSiCharPos 
                     ) 
(* REVIEW: It should not be necessary to initialize these values
           inside of one call to NpsSliceMods.  
           This is only called when not scanning, meaning it should not be
           necessary to maintain NpsResultStateRef . PtsScanInfo . SiCharPos.
           But NpsResultStateRef . PtsScanInfo . SiScanState always needs to be
           maintained.  
*) 
                ; NpsResultStateRef . PtsScanInfo . SiCharPos 
                    := EstUtil . WidthSumSigned  
                         ( NpsResultStateRef . PtsScanInfo . SiCharPos 
                         , TModCmntRef . ModCmntFromPos 
                         ) 
                       (* ^SameLine comment implies there is a token 
                           preceeding, so no need to consider IndentPos. *) 
                ELSE 
                  CantHappen ( AFT . A_NpsSliceMods_AbstractCommentTyp ) 
                END (* TYPECASE  CASE *) 
              ; INC ( NpsResultStateRef . PtsScanInfo . SiCharPos 
                    , SharedStrings . Length 
                        ( TModCmntRef . ModCmntStringRef ) 
                    ) 
              ; NpsResultStateRef . PtsScanInfo . SiLeftTokToPos 
                  := NpsResultStateRef . PtsScanInfo . SiCharPos 
              END (* IF *) 
            ; NpsResultStateRef . PtsScanInfo . SiScanState 
                := TModCmntRef . ModCmntEndScanState 
            ; LScanStateIsInitialized := TRUE 

            (* Blank lines *) 
            | ModHs . ModBlankLineTyp 
            => NpsPassNewLine ( ) 
            ; LCharPosIsInitialized := TRUE 

            (* Errors *) 
            | ModHs . ModErrTyp 
            => (* Ignore these. *) 

            ELSE 
              CantHappen ( AFT . A_NpsSliceMods_BadSlicedModKind ) 
            END (* TYPECASE *) 
          ; INC ( LEstChildNo2 ) 
          ; IF LEstChildNo2 = NpsSeEstRef . SeEstTravInfo . EtiChildNo 
            THEN 
              EXIT 
            END (* IF *) 
          ; EstUtil . GetIthChild 
              ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
              , LEstChildNo2 
              , (* VAR *) ResultChildRelNodeNo := LNodeNo2 (* Dead. *) 
              , (* VAR *) ResultLeafElem := LEstChildLeafElem2 
              ) 
          END (* LOOP thru mods at end of slice *) 
        ; Assert 
            ( TRUE OR LCharPosIsInitialized 
            , AFT . A_NpsSliceModsCharPosNotInitialized 
            ) 
        END (* IF MustComputeDisplay etc. *) 
      END NpsSliceMods 

  ; PROCEDURE NpsAttachedMods ( ) RAISES { Backout } 

    = VAR LChildRef : LbeStd . EstRootTyp 
    ; VAR LModDelIsFinished : BOOLEAN 
    ; VAR LIsRepair : BOOLEAN 
    ; VAR LConsumedCt : Strings . StringSsTyp    
    ; VAR LKindSet : EstHs . EstChildKindSetTyp 
    ; VAR LMustIncludeContainsTempMark : BOOLEAN 

    ; BEGIN (* NpsAttachedMods *) 
        Assert 
          ( NpsResultStateKind ( ) 
            IN ParseHs . PtsKindSetAttachedMods 
          , AFT . A_NpsAttachedModsBadStateKind 
          ) 
      ; LOOP (* Thru those mods that can be handled without returning. 
                This will be at most: 
                  (one slice or, if rescanning, many error mods) 
                  plus one unsliced mod. 
             *)
          TravUtil . CheckModFwd 
            ( NpsSeEstRef . SeEstTravInfo  
            , NpsSeFsRef . SeFsNodeRef 
            , (* VAR *) LChildRef 
            , (* VAR *) LModDelIsFinished 
            )
        ; IF NpsResultStateKind ( ) 
             = ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
             AND ISTYPE ( LChildRef , ModHs . ModDelTyp ) 
          THEN (* The ModDel covers this and a later FmtNo, but we are
                  done with its effect on this FmtNo. *) 
            LChildRef := NIL 
          END (* IF *) 

        (* See what kind of mod this is. 
           If sliceable, call NpsSliceMods. 
           If it needs to be ignored, consume it and loop. 
           Handle other cases on the spot and exit the loop. *) 
        ; TYPECASE LChildRef 
          OF NULL (* No relevant mod at all *) 
          => IF NpsResultStateKind ( ) 
                = ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
             THEN 
               NpsFsNodeAfterLeadingMods ( ) 
             ELSE (* We were looking for trailing mods, but there are none. *) 
               IF NpsDeliverState = DeliverStateTyp . DsTokFound 
                  AND NpsResultStateRef . PtsScanInfo . SiScanState 
                      = LbeStd . SsIdle 
                     (* This test could miss the case where the next item 
                        is a leading comment whose ModCmntBegScanState 
                        = NpsResultStateRef . PtsScanInfo . SiScanState 
                        # LbeStd . SsMaxCmnt.  This would 
                        also be occasion to change NpsDeliverState from 
                        DsTokFound to DsDeliver.  It will be checked 
                        separately when the leading comment is 
                        encountered as usual.  All other non-scanning 
                        cases of DsTokFound get caught here. *) 
               THEN 
                 NpsDeliverState := DeliverStateTyp . DsDeliver 
               END (* IF *) 
             ; NpsResultStateEstRef . PtseStateKind 
                 := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
             END (* IF *) 
          ; EXIT 

          | ModHs . ModBlankLineTyp 
          => (* Blank lines *) 
             IF NpsResultStateRef . PtsScanInfo . SiScanState = LbeStd . SsInTok
             THEN 
                NpsCheckNextTempMarkForEstLeaf 
                  ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                    . LeChildRef 
                  ) 
             ; NpsRescanModBlankLine ( ) 
             ELSIF NpsDeliverState = DeliverStateTyp . DsTokFound 
             THEN (* Nl before of blank line always means we can stop 
                     looking for new trailing comments. *) 
               NpsDeliverState := DeliverStateTyp . DsDeliver 
               (* Will revisit it here, in different state. *) 
             ELSE 
             (* Fix any leftover temp marks from an omitted repair item. *)  
               IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
               THEN 
                 NpsPatchTmEstRefs ( LChildRef , MarkKindTyp . BlankLine ) 
               ; LMustIncludeContainsTempMark := TRUE 
               ; NpsAppendTempMarkRange 
                   ( (* VAR *) ToRange 
                     := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
                   ) 
               ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
               ELSE 
                 LMustIncludeContainsTempMark := FALSE  
               END (* IF *) 
             ; NpsSliceMods 
                 ( MustComputeDisplay := TRUE 
                   (* ^Could be needed, if a comment with mismatching
                       scan state follows. *)  
                 , OnlyOneChild := TRUE 
(* TODO: Slice more things. *) 
                   (* ^Slice a blank line by itself.  This is hopefully 
                      temporary.  To slice more things, we need to be sure 
                      the blank line is not followed by a comment whose 
                      starting scan state doesn't agree with the current 
                      scan state. 
                   *) 
                 , IncludeContainsTempMark := LMustIncludeContainsTempMark 
                 ) 
             END (* IF *) 
          ; EXIT 

          | ModHs . ModCmntLeadingTyp ( TModCmntLeadingRef ) 
          => (* Leading comments *) 
             IF TModCmntLeadingRef . ModCmntBegScanState 
                # NpsResultStateRef . PtsScanInfo . SiScanState 
                OR NpsResultStateRef . PtsScanInfo . SiScanState 
                   = LbeStd . SsMaxCmnt 
             THEN (* Must scan. *)  
                NpsCheckNextTempMarkForEstLeaf 
                 ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                   . LeChildRef 
                 ) 
             ; NpsRescanModCmntLeaders ( TModCmntLeadingRef ) 
             ELSIF TModCmntLeadingRef . ModCmntNlBefore 
                   AND NpsDeliverState = DeliverStateTyp . DsTokFound 
             THEN (* No trailing mods possible. *)  
(* CHECK: Is this what we want? *)                  
               NpsDeliverState := DeliverStateTyp . DsDeliver  
             (* Will revisit it here, in different state. *) 
             ELSE (* Still wait and see what's on the next line. *)  
             (* Fix any leftover temp marks from an omitted repair item. *)  
               IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
               THEN 
                 NpsPatchTmEstRefs ( LChildRef , MarkKindTyp . Plain ) 
               ; LMustIncludeContainsTempMark := TRUE 
               ; NpsAppendTempMarkRange 
                   ( (* VAR *) ToRange 
                     := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
                   ) 
               ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
               ELSE 
                 LMustIncludeContainsTempMark := FALSE  
               END (* IF *) 
             ; NpsResultStateRef . PtsScanInfo . SiScanState 
                 := TModCmntLeadingRef . ModCmntEndScanState
(* FIX: ^This only works when OnlyOneChild is TRUE.  Fix NpsSliceMods to
      do it right, for both cases.  *) 
             ; NpsSliceMods 
                 ( MustComputeDisplay := TRUE 
                 , OnlyOneChild := TRUE 
                 , IncludeContainsTempMark := LMustIncludeContainsTempMark 
                 ) 
(* CHECK: Can we ever get by with MustComputeDisplay = FALSE?  Here, we need
        it to get the scan state set right, after the slice. *) 
             END (* IF *) 
          ; EXIT 

          | ModHs . ModCmntTrailingTyp ( TModCmntTrailingRef ) 
          => (* Trailing comments *) 
             IF NpsResultStateKind ( ) 
                = ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
             THEN (* Ignore trailing mod for now. *)  
               NpsFsNodeAfterLeadingMods ( ) 
             ELSIF TModCmntTrailingRef . ModCmntBegScanState 
                   # NpsResultStateRef . PtsScanInfo . SiScanState 
                   OR NpsResultStateRef . PtsScanInfo . SiScanState 
                      = LbeStd . SsMaxCmnt 
             THEN (* Must scan. *)  
               NpsCheckNextTempMarkForEstLeaf 
                 ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                   . LeChildRef 
                 ) 
             ; NpsRescanModCmntLeaders ( TModCmntTrailingRef ) 
             ELSE
               Assert
                 ( ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
                 , AFT . A_NpsAttachedMods_LeftoverTempMarksForTrailingCmnt 
                 ) 
             ; NpsResultStateRef . PtsScanInfo . SiScanState 
                 := TModCmntTrailingRef . ModCmntEndScanState
(* FIX: ^This only works when OnlyOneChild is TRUE.  Fix NpsSliceMods to
        do it right, for both cases.  *) 
             ; NpsSliceMods 
                 ( MustComputeDisplay := TRUE 
                 , IncludeContainsTempMark := FALSE 
                 ) 
(* CHECK: Can we ever get by with MustComputeDisplay = FALSE?  Here, we need
          it to get the scan state set right, after the slice. *) 
             END (* IF *) 
          ; EXIT 

          | ModHs . ModTextTyp ( TModTextRef ) 
          => (* Text Insertion *) 
            NpsCheckNextTempMarkForEstLeaf 
              ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef ) 
          ; NpsModTextLeaders ( TModTextRef ) 
          ; EXIT 

          | EstHs . EstRefTyp ( TEstRef ) (* It will have EstNodeKindModTok *)
          => (* Token Mod subtree. *) 
            IF NpsResultStateRef . PtsScanInfo . SiScanState 
               # LbeStd . SsIdle 
               AND EstUtil . CharPosPlusWidthInfo 
                     ( NpsResultStateRef . PtsScanInfo . SiCharPos 
                     , TEstRef . KTreeWidthInfo 
                     ) 
                   > Options . RightMargin 
            THEN (* Implicit new line. *) 
              DEC ( ParseInfo . PiLineCtIncr 
                  , ORD ( NpsResultStateRef . PtsPrevTokAfter 
                          # LbeStd . Tok__BegOfLine
                          (* and not othewise a new line. *)
                        ) 
                  )
            ; NpsCheckNextTempMarkForInsTok
                ( NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo )
            (* ^Could there ever be a temp mark on this? *)
            ; NpsDeliverStringToScanner 
                ( StringNewLine 
                , NpsSeFsRef . SeFsIndentPos 
                , AreAllBlanks := FALSE 
                , ConsumedCt := LConsumedCt (* Ignore. *) 
                ) 
            ; NpsPassNewLine ( ) 
            ; NpsSeEstRef . SeEstIsFirstLine := FALSE 
            ; NpsSeEstRef . SeEstIndentPos := NpsSeEstRef . SeEstIndentPosN 
            END (* IF *) 
          ; NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindNewEst 
          ; EXIT 

          | SharedStrings . T  
          => (* It's a Mod, so can only be lex error chars. *)
            NpsBeginLexErrChars ( ) 

          | ModHs . ModLexErrTyp
          => CASE NpsDeliverState <* NOWARN *>
             OF DeliverStateTyp . DsTokFound
             => NpsDeliverState := DeliverStateTyp . DsDeliver
             ; EXIT
             (* Will revisit it here, in different state. *) 
             | DeliverStateTyp . DsStarting
             => IF NpsResultStateRef . PtsScanInfo . SiScanState
                   = LbeStd . SsIdle
(* FIXME: We need a scan state in the lex error mod.  Will rescan if it and the
          current scan state are unequal. *)
               THEN (* Keep it among the leading mods of the to-be-found token. *)
                 Assert
                   ( NpsWaitingTempMarkRange = ParseHs . TempMarkRangeEmpty
                   , AFT . A_NpsAttachedMods_TempMarksOnModLexError 
                   )
               ; LKindSet 
                   := NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem
                      . LeKindSet
               ; IF NOT EstHs . EstChildKindFirstOfGroup IN LKindSet
                 THEN (* Use a slice of parent, to enable combining. *) 
                   NpsAccumSlice 
                     ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
                     , NpsSeEstRef . SeEstTravInfo . EtiChildNo 
                     , NpsSeEstRef . SeEstTravInfo . EtiChildNo + 1 
                     , SyntTokCt := 0 
                     )
                 ELSE (* Must use a single child with changed kind set. *) 
                   NpsAccumNode 
                     ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem
                       . LeChildRef  
                     , EstChildKindSet := LKindSet 
                     , SyntTokCt := 0 
                     )
                 END (* IF *) 
               ; TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo )
               (* And loop to look for more attached mods directly. *)
               ELSE (* Rescanning.  Just remove it. *) 
                 TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo )
               (* And look for more attached mods here. *)
               END (* IF *)
             END (* CASE *)

          | ModHs . ModErrTyp (* Other error mod *)
          => IF NpsDeliverState = DeliverStateTyp . DsTokFound 
             THEN 
               NpsDeliverState := DeliverStateTyp . DsDeliver
             ; EXIT
             (* Will revisit it here, in different state. *) 
             ELSE (* Just remove it.  Reparsing may create a new one. *)
               TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo )
             (* And look for more attached mods here. *)
             END (* IF *)

          | ModHs . ModDelTyp 
          => (* Deletion. Treat all tokens covered by a ModDel as absent, 
                even if IsRepair, so as to reparse without the earlier repair, 
                in case it needs to be repaired in a different way, (or even 
                not at all), due to edits to the left of this point. *) 
            Assert 
              ( NpsSeFsRef . SeFsNodeRef . FsKind 
                IN LangUtil . FsKindSetDeletable 
              , AFT . A_NpsModDelBadDeletedFsKind 
              ) 
          ; Assert 
              ( NpsResultStateKind ( ) 
                # ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
              , AFT . A_NpsAttachedMods_TrailingError 
              ) 
          ; IF FALSE (* We don't want to deliver yet.  There could be 
                        more trailing comments that need to attach to
                        the found tok, after the deletion. *) 
               AND NpsResultStateRef . PtsScanInfo . SiScanState 
                   = LbeStd . SsIdle
               AND NpsDeliverState = DeliverStateTyp . DsTokFound 
            THEN 
              Assert (* At the first item deleted by the Mod. *) 
                ( NpsSeEstRef . SeEstTravInfo . EtiChildFmtNo 
                  = NpsSeFsRef . SeFsNodeRef . FsFmtNo 
                , AFT . A_NpsModDel_MiddleOfModDel  
                ) 
            ; NpsDeliverState := DeliverStateTyp . DsDeliver 
            ; EXIT 
            ELSE
              LIsRepair 
                 := EstHs . EstChildKindContainsInsertionRepair 
                    IN NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                       . LeKindSet 
            ; IF NpsSeFsRef . SeFsNodeRef . FsKind 
                 IN LangUtil . FsKindSetLineBreak 
              THEN (* Must do this even for deleted line breaks. *) 
                NpsSeEstRef . SeEstIsFirstLine := FALSE 
              ; NpsSeEstRef . SeEstIndentPos := NpsSeEstRef . SeEstIndentPosN 
              END (* IF *) 
            ; IF LModDelIsFinished 
              THEN 
                TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo  ) 
              ; IF LIsRepair 
                THEN (* Proposed (by an earlier parse) insertion repair. *) 
                (* The repair could have temp marks.  BuildTempMarks will allow
                   this to happen only if the line has nothing but repairs
                   and this is the last one.  Make the temp marks absolute at
                   the beginning of the (next) line and include them in 
                   NpsWaitingTempMarkRange.  They will be applied to the next item. 
                *)
                  LOOP 
                    NpsCheckNextTempMarkForInsTok 
                      ( NpsSeFsRef . SeFsNodeRef . FsFmtNo ) 
                  ; IF NOT NpsNextTempMarkIsRelevant 
                    THEN 
                      EXIT 
                    ELSE 
                      ParseInfo . PiTravTempMarkListRef
                      ^ [ NpsNextTempMarkSs ] . CharPos
                      := 0
                    ; NpsIncludeTempMark ( ) 
                    END (* IF *) 
                  END (* LOOP *) 
                END (* IF *) 
              ; CASE NpsSeFsRef . SeFsNodeRef . FsKind <* NOWARN *>
                OF FsKindTyp . FsKindInsTok 
                => NpsResultStateEstRef . PtseStateKind 
                     := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
                  (* No exit, which is just a shortcut to the same place. *) 
                | FsKindTyp . FsKindLineBreakOpt 
                , FsKindTyp . FsKindLineBreakReqd  
                => NpsResultStateEstRef . PtseStateKind 
                     := ParseHs . ParseTravStateKindTyp 
                        . PtsKindDoneWithFsNode
                ; EXIT 
                END (* CASE *) 
              ELSE (* There can be no trailing mods. *) 
                NpsResultStateEstRef . PtseStateKind 
                  := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
              ; EXIT 
              END (* IF *) 
            END (* IF *) 
          ELSE 
            CantHappen ( AFT . A_NpsAttachedMods_BadModType ) 
          END (* TYPECASE *)
        ; IF NpsDeliverState = DeliverStateTyp . DsDeliver THEN EXIT END 
        END (* LOOP *) 
      END NpsAttachedMods 

  ; PROCEDURE NpsPushFsStackElem 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; SelfFsChildNo : LangUtil . FsChildNoTyp 
      ) 
    RAISES { Backout } 

    = VAR LStackElemFsNodeRef : ParseHs . StackElemFsTyp 

    ; BEGIN (* NpsPushFsStackElem *) 
        LStackElemFsNodeRef := NEW ( ParseHs . StackElemFsTyp ) 
      ; LStackElemFsNodeRef . SeFsLink 
          := NpsResultStateEstRef . PtseStackFsRef 
      ; Assert 
          ( FsNodeRef # NIL 
          , AFT . A_NpsPushFsStackElem_Nil_FsNodeRef 
          ) 
      ; CASE FsNodeRef . FsKind 
        OF FsKindTyp . FsKindEstFixedVert 
        , FsKindTyp . FsKindEstFixedHoriz 
        , FsKindTyp . FsKindEstFixedFill 
        , FsKindTyp . FsKindEstListVert 
        , FsKindTyp . FsKindEstListHoriz 
        , FsKindTyp . FsKindEstListFill 
        , FsKindTyp . FsKindEstListTrailVert 
        , FsKindTyp . FsKindEstListTrailHoriz 
        , FsKindTyp . FsKindEstListTrailFill 
        , FsKindTyp . FsKindAstString 
        => Assert 
             ( FsNodeRef = NpsSeEstRef . SeEstRootFsNodeRef 
             , AFT . A_NpsPushFsStackElem_Mismatched_SeEstRootFsNodeRef  
             ) 
        ; LStackElemFsNodeRef . SeFsSeEstRef := NpsSeEstRef 
        ELSE 
          LStackElemFsNodeRef . SeFsSeEstRef := NIL 
        END (* CASE *) 
      ; LStackElemFsNodeRef . SeFsNodeRef := FsNodeRef 
      ; LStackElemFsNodeRef . SeFsSelfFsChildNo := SelfFsChildNo 
      ; LStackElemFsNodeRef . SeFsFmtKind := NpsSeFsRef . SeFsFmtKind
      ; NpsResultStateEstRef . PtseStackFsRef := LStackElemFsNodeRef 
      ; NpsSeFsRef := LStackElemFsNodeRef 
      ; NpsResultStateEstRef . PtseStateKind 
          := ParseHs . ParseTravStateKindTyp . PtsKindNewFsNode 
      END NpsPushFsStackElem 

  ; PROCEDURE NpsPushFsStackElemForFsChild 
      ( ParentFsNodeRef : LangUtil . FsNodeRefTyp 
      ; FsChildNo : LangUtil . FsChildNoTyp 
      ; FsKindSet : LangUtil . FsKindSetTyp 
      ) 
    RAISES { Backout } 

    = VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 

    ; BEGIN (* NpsPushFsStackElemForFsChild *) 
        IF ParentFsNodeRef . FsChildren = NIL 
           OR NUMBER ( ParentFsNodeRef . FsChildren ^ ) <= 0 
        THEN
          NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
        ELSE 
          LChildFsNodeRef := ParentFsNodeRef . FsChildren ^ [ FsChildNo ] 
        ; Assert 
            ( LChildFsNodeRef . FsKind IN FsKindSet 
            , AFT . A_NpsPushFsStackElemForFsChildBadFsKind 
            ) 
        ; NpsPushFsStackElem ( LChildFsNodeRef , FsChildNo ) 
        END (* IF *) 
      END NpsPushFsStackElemForFsChild 

  ; PROCEDURE NpsParentContainsInsertionRepair ( ) : BOOLEAN 

    = BEGIN 
        IF NpsSeEstRef . SeEstLink = NIL  
        THEN RETURN FALSE 
(* TODO: Here is a place where a whole Est tree could be a placeholder. *) 
        ELSE
          RETURN  
            EstHs . EstChildKindContainsInsertionRepair 
            IN NpsSeEstRef . SeEstLink  
               . SeEstTravInfo . EtiChildLeafElem . LeKindSet 
        END (* IF *) 
      END NpsParentContainsInsertionRepair 

  ; PROCEDURE NpsChildContainsInsertionRepair ( ) : BOOLEAN 

    = BEGIN 
        RETURN  
          EstHs . EstChildKindContainsInsertionRepair 
          IN NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeKindSet 
      END NpsChildContainsInsertionRepair 

  ; PROCEDURE NpsBeginLexErrChars ( ) 
    RAISES { Backout } 

    = VAR LString : SharedStrings . T
    ; VAR LDeferredInfo : ParseHs . DeferredInfoRefTyp
    ; VAR LKindSet : EstHs . EstChildKindSetTyp 

    ; BEGIN (* NpsBeginLexErrChars *)
        Assert ( NOT NpsChildContainsInsertionRepair ( ) 
          , AFT . A_NpsBeginLexErrChars_is_insertion_repair 
          )
      ; LString := NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef
      ; IF NpsResultStateRef . PtsScanInfo . SiScanState = LbeStd . SsIdle
(* FIXME: We need a scan state in the lex error string.  Rescan if it and the
          current scan state are unequal. *)
        THEN (* No need to rescan.  If the characters were lexically bad
                last time, they will be the same this time.  Accumulate
                them, unchanged. *)
          LKindSet := NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeKindSet 
        ; IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
          THEN (* Patch leftover temp marks. *)  
  (* TODO: It would be nice to bias them relative to this LexErrChars String,
       but we are not keeping CharPos, and this is a very rare case. 
  *)  
            NpsPatchTmEstRefs ( LString , MarkKindTyp . Plain  ) 
          ; LKindSet := LKindSet + EstHs . EstChildKindSetContainsTempMark 
          END (* IF *)

        (* Skip relevant TempMarks.  No rebias is necessary, since we did 
           not rescan.  No patching is necessary.  MarkKinds Plain and
           BlankLine are possible. *)
        ; LOOP 
            NpsCheckNextTempMarkForEstLeaf ( LString ) 
          ; IF NpsNextTempMarkIsRelevant 
            THEN NpsIncludeTempMark ( ) 
            ELSE EXIT 
            END (* IF *) 
          END (* LOOP *) 

        ; CASE NpsDeliverState <* NOWARN *>
          OF DeliverStateTyp . DsStarting 
          => NpsAppendTempMarkRange 
              ( (* VAR *) ToRange 
                := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
              ) 
          ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty

            (* Include the lex error string in the result. *)
            ; IF NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeKindSet
                 = LKindSet 
                 AND NOT EstHs . EstChildKindFirstOfGroup IN LKindSet 
              THEN (* Use a slice of parent, to enable combining. *) 
                NpsAccumSlice 
                  ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
                  , NpsSeEstRef . SeEstTravInfo . EtiChildNo 
                  , NpsSeEstRef . SeEstTravInfo . EtiChildNo + 1 
                  , SyntTokCt := 0 
                  )
              ELSE (* Must use a single child with changed kind set. *) 
                NpsAccumNode 
                  ( LString , EstChildKindSet := LKindSet , SyntTokCt := 0 )
              END (* IF *) 
            (* This is a Mod.  DeliverState remains DsStarting. *)
            ; TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo )  

          | DeliverStateTyp . DsTokFound 
          => (* Defer the lex error string. *)
            LDeferredInfo := NpsGetDeferredInfoRef ( ) 
          ; LDeferredInfo . Tok := SharedStrings . Tok ( LString ) 
          ; LDeferredInfo . SyntTokCt := 0 
          ; LDeferredInfo . ObjRef := LString   
          ; LDeferredInfo . IsInsertionRepair := FALSE 
          ; LDeferredInfo . IsInterior := FALSE  
          ; LDeferredInfo . KindSet := LKindSet
          ; LDeferredInfo . FullTempMarkRange := NpsWaitingTempMarkRange 
          ; LDeferredInfo . PatchTempMarkRange := ParseHs . TempMarkRangeEmpty 
          ; LDeferredInfo . WaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
          ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty (* Dead. *)
          ; NpsDeliverState := DeliverStateTyp . DsDeliver 
          ; TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo )  
          END (* CASE *)

        ELSE (* Rescanning. *) 
          NpsCheckNextTempMarkForEstLeaf ( LString ) 
        ; NpsRescanTok 
            ( SharedStrings . Tok ( LString )  
            , LString 
            , ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenLexErrChars  
            , ParseHs . ParseTravStateKindTyp . PtsKindRescanLexErrChars  
            ) 
        END (* IF *)
      END NpsBeginLexErrChars 

  ; PROCEDURE NpsBeginAstString ( ) RAISES { Backout } 

    = VAR LStringRef : SharedStrings . T
    ; VAR LDeferredInfo : ParseHs . DeferredInfoRefTyp 
    ; VAR LKindSet : EstHs . EstChildKindSetTyp 

    ; BEGIN (* NpsBeginAstString *) 
        LStringRef := NpsSeEstRef . SeEstTravInfo . EtiStringRef
      ; IF LStringRef = NIL 
        THEN (* NIL Ast child. *) 
          NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
        ELSIF NpsParentContainsInsertionRepair ( )
        THEN (* This is a repair placeholder that Parser proposed, in the
                previous parse, to insert.  Skip it. 
             *) 
        (* The repair could have temp marks.  BuildTempMarks will allow
           this to happen only if the line has nothing but repairs
           and this is the last one.  Make the temp marks absolute at
           the beginning of the (next) line and include them in 
           NpsWaitingTempMarkRange.  They will be applied to the next item. 
        *)
          LOOP 
            NpsCheckNextTempMarkForEstLeaf 
              ( LStringRef ) 
          ; IF NOT NpsNextTempMarkIsRelevant 
            THEN EXIT 
            ELSE 
              ParseInfo . PiTravTempMarkListRef
              ^ [ NpsNextTempMarkSs ] . CharPos
              := 0 
            ; NpsIncludeTempMark ( ) 
            END (* IF *) 
          (* These temp marks will be held as additional leftovers in
             NpsWaitingTempMarkRange, and will be later attached to the next item. 
          *) 
          END (* LOOP *) 
        ; NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode
            
        ELSIF NpsResultStateRef . PtsScanInfo . SiScanState = LbeStd . SsIdle 
        THEN (* Need not scan, deliver the token *) 
          WITH WParentEstTravInfo = NpsSeEstRef . SeEstLink . SeEstTravInfo
          DO 
            LKindSet := WParentEstTravInfo . EtiChildLeafElem . LeKindSet
            
          (* Patch any leftover temp marks from an earlier omitted repair item. *)
(* TODO: It would be nice to bias them relative to this AstString, but
         we are not keeping CharPos, and this is a very rare case. 
*)  
          ; IF NOT ParseHs . RangeIsEmpty ( NpsWaitingTempMarkRange ) 
            THEN
              NpsPatchTmEstRefs ( LStringRef , MarkKindTyp . Plain ) 
            ; LKindSet := LKindSet + EstHs . EstChildKindSetContainsTempMark  
            END (* IF *)

          (* Include TempMarks in the Ast string.  No rebias is necessary,
             since we will not rescan.  No patching is necessary.  MarkKinds
             Plain and BlankLine are possible. 
          *)
          ; LOOP 
              NpsCheckNextTempMarkForEstLeaf 
                ( LStringRef ) 
            ; IF NpsNextTempMarkIsRelevant 
              THEN NpsIncludeTempMark ( ) 
              ELSE EXIT 
              END (* IF *) 
            END (* LOOP  *) 

          ; CASE NpsDeliverState <* NOWARN *>
            OF DeliverStateTyp . DsStarting 
            (* This is the token we will deliver. *) 
            => NpsAppendTempMarkRange 
                ( (* VAR *) ToRange 
                  := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
                )
                
            ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
            ; NpsResultStateRef . PtsTokInfo . TiTok 
                := SharedStrings . Tok ( LStringRef )
            ; LbeStd . IncLimitedTokCt
                ( NpsResultStateRef . PtsTokInfo . TiSyntTokCt ) 
            ; NpsResultStateRef . PtsPrevTokAfter 
                := NpsResultStateRef . PtsTokInfo . TiTok 
            ; NpsResultStateEstRef . PtsePrevTokBefore 
                := NpsResultStateRef . PtsTokInfo . TiTok 
            ; NpsResultStateRef . PtsTokInfo . TiIsInsertionRepair := FALSE  
            ; IF WParentEstTravInfo . EtiChildLeafElem . LeKindSet = LKindSet 
                 AND NOT EstHs . EstChildKindFirstOfGroup IN LKindSet 
              THEN (* Use a slice of the parent instead of 
                      LStringRef, so the slice
                      can be combined with leading/trailing mods. 
                   *) 
                 NpsAccumSlice 
                   ( EstRef := WParentEstTravInfo . EtiParentRef 
                   , FromEstChildNo := WParentEstTravInfo . EtiChildNo 
                   , ToEstChildNo := WParentEstTravInfo . EtiChildNo + 1 
                   , SyntTokCt := 1 
                   ) 
              ELSE (* Must use a single child with changed kind set. *) 
                NpsAccumNode 
                  ( LStringRef 
                  , EstChildKindSet := LKindSet 
                  , SyntTokCt := 1 
                  ) 
              END (* IF *) 
            ; TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo )  
            ; NpsResultStateEstRef . PtseStateKind 
                := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods
            ; NpsDeliverState := DeliverStateTyp . DsTokFound
            
            | DeliverStateTyp . DsTokFound 
            => LDeferredInfo := NpsGetDeferredInfoRef ( ) 
            ; LDeferredInfo . Tok 
                 := SharedStrings . Tok 
                      ( LStringRef ) 
            ; LDeferredInfo . SyntTokCt := 1 
            ; LDeferredInfo . IsInsertionRepair := FALSE 
            ; LDeferredInfo . IsInterior := FALSE
            ; LDeferredInfo . FullTempMarkRange := NpsWaitingTempMarkRange 
            ; LDeferredInfo . PatchTempMarkRange := ParseHs . TempMarkRangeEmpty 
            ; LDeferredInfo . WaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
            ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty (* Dead. *) 
            ; LDeferredInfo . ObjRef := LStringRef 
            ; LDeferredInfo . KindSet := LKindSet  
            ; TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo )  
            ; NpsResultStateEstRef . PtseStateKind 
                := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods
            ; NpsDeliverState := DeliverStateTyp . DsDeliver 
            END (* CASE *) 
          END (* WITH *) 
        ELSE (* Rescanning. *) 
          NpsCheckNextTempMarkForEstLeaf 
            ( LStringRef ) 
        ; NpsRescanTok 
            ( SharedStrings . Tok 
                ( LStringRef )  
            , LStringRef 
            , ParseHs . ParseTravStateKindTyp 
                . PtsKindBlanksThenAstString 
            , ParseHs . ParseTravStateKindTyp . PtsKindRescanAstString 
            ) 
        END (* IF *) 
      END NpsBeginAstString 

  ; PROCEDURE NpsCheckEstSuccessor ( ) 
    RAISES { Backout } 
    (* Check whether we have previously advanced beyond the end of this Est 
       (Possibly a list slice). This can happen when the Est/slice has no 
       syntactic mods and an advance is done after the whole 
       Est/slice is delivered, and, at a different time, a descend 
       was done from the same state (and a sequence of advances 
       and descends thereafter finally leads back to end of Est/slice). 
       If either happens, SeEstAdvanceState will be set to the 
       state after, and when the other happens, this pointer 
       is used to reuse the resultant new state.  In the latter case,
       NpsDeliverState := DeliverStateTyp . DsDeliver. 
       If we are rescanning at this point, none of this can happen. *) 

    =
VAR LI : INTEGER := 9
    ; BEGIN (* NpsCheckEstSuccessor *) 
        IF NpsResultStateRef . PtsScanInfo . SiScanState = LbeStd . SsIdle 
        THEN 
          IF NpsSeEstRef . SeEstAdvanceState # NIL 
          THEN (* Abandon the object we allocated. *) 
            Assert 
              (
TRUE OR         NpsSeEstRef . SeEstAdvanceState . PtsTokInfo 
                . TiFullTempMarkRange . From 
                = NpsResultStateEstRef . PtsTokInfo . TiFullTempMarkRange 
                  . From 
              , AFT . A_NpsCheckEstSuccessor_Mismatched_temp_mark_range
              ) 
; IF NpsSeEstRef . SeEstAdvanceState . PtseTokTempMarkSs 
    # NpsResultStateEstRef . PtseTokTempMarkSs
 THEN
   LI := 17 
 END 
          ; NpsResultStateRef := NpsSeEstRef . SeEstAdvanceState 
          ; NpsResultStateEstRef := NpsSeEstRef . SeEstAdvanceState
          ; NpsNextTempMarkSs := NpsResultStateEstRef . PtseTokTempMarkSs
            (* Update cached value in NpsNextTempMarkSs. *) 
           (* ^Defensive *) 
          ; NpsDeliverState := DeliverStateTyp . DsDeliver 
          END (* IF *) 
        END (* IF *) 
      END NpsCheckEstSuccessor 

  ; PROCEDURE NpsFindEndOfListSlice ( SliceThruFmtNo : EstHs . FmtNoTyp ) 
    RAISES { Backout } 
    (* Set NpsSeEstRef . SeEstWholeSliceToChildNo 
       and NpsSeEstRef . SeEstWholeSliceRMEstChildNo, for a list slice
       starting at the current Est child.  

       If nonempty, the slice includes at least one Ast child, any list
       element insertion tokens (not list separators), and any trailing 
       mods on the above.  All of this will contain no syntactic mods,
       nor will it contain any trailing list separators.  The slice is the 
       longest possible, starting from the current Est child.  The slice 
       can be an Ast singleton list. (Higher levels will not return this,
       however.)   

       SeEstWholeSliceToChildNo is for the Est child immeidately after the 
       entire slice, including any trailing insertion tokens and mods.
       SeEstWholeSliceRMEstChildNo is the RM Ast child in the slice.
       If the slice is empty, set the two fields to 0. *)     

    = VAR LEstChildLeafElem : EstHs . LeafElemTyp 
    ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LToChildNo : LbeStd . EstChildNoTyp 

    ; BEGIN (* NpsFindEndOfListSlice *) 
        IF NpsResultStateRef . PtsScanInfo . SiScanState # LbeStd . SsIdle 
        THEN (* Scanning.  Slice is empty. *) 
          NpsSeEstRef . SeEstWholeSliceToChildNo := 0 
        ; NpsSeEstRef . SeEstWholeSliceRMEstChildNo := 0 
        ELSE 
          EstUtil . NextInKindSet 
            ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
            , NpsSeEstRef . SeEstTravInfo . EtiChildNo 
            , EstHs . EstChildKindSetMustReparse  
              + EstHs . EstChildKindSetTrailingSep   
            , (* VAR *) LToChildNo 
            , (* VAR *) LNodeNo 
            , (* VAR *) LEstChildLeafElem 
            ) 
          (* ^LM Est child that cannnot be sliced, called a _stopper_. *) 
        ; IF LToChildNo = NpsSeEstRef . SeEstTravInfo . EtiChildNo 
          THEN (* We are starting at a stopper.  Make the slice empty. *) 
            NpsSeEstRef . SeEstWholeSliceToChildNo := 0 
          ; NpsSeEstRef . SeEstWholeSliceRMEstChildNo := 0 
          ELSE (* There is >= one child not in need of reparsing. 
                  Find the RM such child. *) 
            IF LToChildNo < NpsSeEstRef . SeEstTravInfo . EtiChildCt 
            THEN (* There is an explicit stopper. *) 
              EstUtil . PrevInKindSet 
                ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
                , LToChildNo  
                , EstHs . EstChildKindSetFirstOfGroup 
                , (* VAR *) LToChildNo 
                , (* VAR *) LNodeNo 
                , (* VAR *) LEstChildLeafElem 
                ) 
              (* ^Back up to the the start of the stopper's FmtNo group. *) 
            ELSE (* We are off the end of the list of children. *) 
            END (* IF *) 
          ; IF LToChildNo < NpsSeEstRef . SeEstTravInfo . EtiChildCt 
               AND EstHs . FmtNoListEstChild < LEstChildLeafElem . LeFmtNo 
               AND LEstChildLeafElem . LeFmtNo <= SliceThruFmtNo 
            THEN (* A trailing insertion tok of the list _element_ (not a list
                    separator).  Back up to its Ast node. *) 
              EstUtil . PrevInKindSet 
                ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
                , LToChildNo - 1  
                , EstHs . EstChildKindSetEstChild 
                , (* VAR *) LToChildNo 
                , (* VAR *) LNodeNo 
                , (* VAR *) LEstChildLeafElem 
                ) 
            END (* IF *) 
          (* LToChildNo is for the LM Ast child that cannot be in the slice.  
             Back up to the previous Ast child, which will be the last Ast 
             child of the slice. *) 
          ; EstUtil . PrevInKindSet 
              ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
              , LToChildNo - 1 
              , EstHs . EstChildKindSetEstChild 
              , (* VAR *) NpsSeEstRef . SeEstWholeSliceRMEstChildNo 
              , (* VAR *) LNodeNo 
              , (* VAR *) LEstChildLeafElem 
              ) 
          ; IF NpsSeEstRef . SeEstWholeSliceRMEstChildNo 
               < NpsSeEstRef . SeEstTravInfo . EtiChildNo 
            THEN (* No unvisited Ast child before the stopper/end of list. 
                    Make the slice empty. *) 
              NpsSeEstRef . SeEstWholeSliceToChildNo := 0 
            ; NpsSeEstRef . SeEstWholeSliceRMEstChildNo := 0 
            ELSE (* Find child to right of trailing mods on RM slicable 
                    Ast child. *)
              NpsSeEstRef . SeEstWholeSliceToChildNo 
                := NpsSeEstRef . SeEstWholeSliceRMEstChildNo + 1 
            ; LOOP (* This is one place where we do brute force linear 
                      searching of Est FmtNo groups.  It is limited to the 
                      number of right CondFmt insertion tokens of a list 
                      Ast child.  This will probably never exceed one. 
                      If it ever becomes important to speed this up, perhaps 
                      some additional EstChildKind value could be 
                      defined, or maybe a search on FmtNo. *) 
                IF NpsSeEstRef . SeEstWholeSliceToChildNo 
                   >= NpsSeEstRef . SeEstTravInfo . EtiChildCt 
                THEN EXIT
                ELSE 
                  EstUtil . NextInKindSet 
                    ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
                    , NpsSeEstRef . SeEstWholeSliceToChildNo 
                    , EstHs . EstChildKindSetFirstOfGroup 
                    , (* VAR *) NpsSeEstRef . SeEstWholeSliceToChildNo 
                    , (* VAR *) LNodeNo 
                    , (* VAR *) LEstChildLeafElem 
                    ) 
                ; IF LEstChildLeafElem . LeFmtNo = EstHs . FmtNoListEstChild  
                     (* Next Ast child to the right. *) 
                     OR LEstChildLeafElem . LeFmtNo > SliceThruFmtNo 
                        (* A list separator or left CondFmt item. *) 
                  THEN 
                    EXIT 
                  ELSE 
                    INC ( NpsSeEstRef . SeEstWholeSliceToChildNo ) 
                  END (* IF *) 
                END (* IF *) 
              END (* LOOP *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END NpsFindEndOfListSlice 

  ; PROCEDURE NpsPushEstSublist ( ) 

    = VAR LNewSeEstRef : ParseHs . StackElemEstTyp 

    ; BEGIN (* NpsPushEstSublist *) 
        LNewSeEstRef := NEW ( ParseHs . StackElemEstTyp ) 
      ; LNewSeEstRef . SeEstTravInfo := NpsSeEstRef . SeEstTravInfo 
      ; LNewSeEstRef . SeEstIndentPos1 := NpsSeEstRef . SeEstIndentPos1 
      ; LNewSeEstRef . SeEstIndentPosN := NpsSeEstRef . SeEstIndentPosN 
      ; LNewSeEstRef . SeEstIndentPos := NpsSeEstRef . SeEstIndentPos 
      ; LNewSeEstRef . SeEstIsFirstLine := NpsSeEstRef . SeEstIsFirstLine  
      ; LNewSeEstRef . SeEstAdvanceState := NIL 
      ; LNewSeEstRef . SeEstWholeSliceToChildNo 
          := NpsSeEstRef . SeEstWholeSliceToChildNo 
      ; LNewSeEstRef . SeEstWholeSliceRMEstChildNo 
          := NpsSeEstRef . SeEstWholeSliceRMEstChildNo 
      ; LNewSeEstRef . SeEstIsSublist := TRUE 
      ; LNewSeEstRef . SeEstRootFsNodeRef := NpsSeEstRef . SeEstRootFsNodeRef 
      ; LNewSeEstRef . SeEstLink := NpsSeEstRef 
      ; NpsSeEstRef := LNewSeEstRef 
      ; NpsResultStateEstRef . PtseStackEstRef := LNewSeEstRef 
      ; NpsInEstPopPhase := FALSE 
      END NpsPushEstSublist

  ; PROCEDURE NpsMaybeListSlice ( ) RAISES { Backout } 
    (* See whether there is a plural list slice we can return as a whole, 
       with a sublist token.  If so, set variables up for it. 
       If currently scanning, make the slice empty.  
    *) 

    = VAR LLMEstChildNo : LbeStd . EstChildNoTyp  
    ; VAR LEstChildLeafElem : EstHs . LeafElemTyp 
    ; VAR LNodeNo : LbeStd . EstNodeNoTyp 

    ; BEGIN
        IF NpsSeFsRef . SeFsNodeRef . FsIsInsideList 
           AND NOT NpsSeEstRef . SeEstTravInfo . EtiIsOptSingletonList 
           AND ( SuccKind = SuccKindTyp . SuccKindAdvance 
                 OR NOT NpsSeEstRef . SeEstIsSublist 
                    (* Descend from a list Est must do this.  
                       Descend from a list slice must not. *) 
               ) 
        THEN (* It's looking promising for a slice. *) 
          WITH WTravInfo = NpsSeEstRef . SeEstTravInfo 
          DO 
            IF NOT NpsSeEstRef . SeEstIsSublist 
               AND ( NpsSeEstRef . SeEstWholeSliceToChildNo = 0  
                     OR WTravInfo . EtiChildNo 
                        >= NpsSeEstRef . SeEstWholeSliceToChildNo 
                     (* ^We are not already inside a slice. *) 
                   ) 
            THEN 
              NpsFindEndOfListSlice 
                ( NpsSeFsRef . SeFsNodeRef . FsListSliceThruFmtNo ) 
              (* ^A SIDE EFFECT of NpsMaybeListSlice! *) 
            END (* IF *)  
          ; IF NpsResultStateRef . PtsScanInfo . SiScanState # LbeStd . SsIdle 
               OR NpsSeEstRef . SeEstWholeSliceToChildNo 
                  <= WTravInfo . EtiChildNo 
                  (* ^We are beyond the current slice.  This covers the empty 
                      slice case too. *) 
               (* We want a whole list slice after a whole subtree, 
                  because they have different nonterminals and may
                  parse differently. 
(* REVIEW: Do we really want this commented out? Remove or reinstate. *) 
               OR ( WTravInfo . EtiChildNo = 0  
                    AND NpsSeEstRef . SeEstWholeSliceToChildNo 
                        >= WTravInfo . EtiChildCt 
                  ) (* For this to happen, we would already have returned the 
                       entire list node as a nonterminal, but the parser 
                       requested a descend. We don't want to return the same
                       set of tokens as a sublist. *) 
               *) 
            THEN (* Do nothing here.  Unsliced traversal will happen. *)  
            ELSIF NpsDeliverState = DeliverStateTyp . DsTokFound 
            THEN (* We were only looking for trailing mods of some previous
                    token.  Now we know it has none, so deliver it. We will
                    come back here when NextParseState is called again to 
                    handle the slice. *) 
              NpsDeliverState := DeliverStateTyp . DsDeliver 
            ELSE 
              EstUtil . NextInKindSet 
                ( WTravInfo . EtiParentRef 
                , WTravInfo . EtiChildNo 
                , EstHs . EstChildKindSetEstChild 
                , (* VAR *) LLMEstChildNo 
                , (* VAR *) LNodeNo 
                , (* VAR *) LEstChildLeafElem 
                ) 
            ; IF LLMEstChildNo < NpsSeEstRef . SeEstWholeSliceRMEstChildNo 
              THEN (* Slice is not an Ast singleton.  Deliver it as sublist. *)
                IF NOT NpsSeEstRef . SeEstIsSublist 
                THEN 
                  NpsPushEstSublist ( ) 
                END (* IF *) 
              ; WITH WNewTravInfo = NpsSeEstRef . SeEstTravInfo   
                DO 
                  IF EstHs . EstChildKindFirstOfGroup 
                     IN WNewTravInfo . EtiChildLeafElem . LeKindSet 
                  THEN
                    NpsFlushSlice ( ) 
                  END (* IF *) 
                ; NpsAccumSlice 
                    ( WNewTravInfo . EtiParentRef 
                    , WNewTravInfo . EtiChildNo 
                    , NpsSeEstRef . SeEstWholeSliceToChildNo 
                    , SyntTokCt := LbeStd . ParseCheckInfinity 
                      (* ^This is a very crude approximation.  It will take 
                         some work to compute the exact value. *) 
(* TODO: ^Compute this properly. *) 
                    ) 
                ; NpsResultStateRef . PtsTokInfo . TiTok 
                    := NpsSeFsRef . SeFsNodeRef . FsSublistTok 
                ; NpsResultStateRef . PtsTokInfo . TiIsInterior := TRUE 
                (* Postpone setting PtsPrevTokAfter and PtsePrevTokBefore 
                   until after the slice, since it requires some extra work, 
                   and we could descend into the slice, which would make it 
                   unnecessary. 
                *) 
                ; NpsSkipTempMarksForSubtrees 
                    ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
                    , NpsSeEstRef . SeEstTravInfo . EtiChildNo 
                    , NpsSeEstRef . SeEstWholeSliceToChildNo 
                    ) 
                ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
                ; NpsResultStateEstRef . PtseStateKind 
                    := ParseHs . ParseTravStateKindTyp 
                       . PtsKindDoneWithListSliceUntraversed 
                ; NpsDeliverState := DeliverStateTyp . DsDeliver 
                  (* ^This will cause immediate return. Trailing conditional
                     format items and trailing mods on the last syntactic token
                     are already included in the slice. *)  
             (* ELSE this is an Est singleton slice.  Let regular traversal
                     handle it. *) 
                END (* IF *) 
              END (* WITH WNewTravInfo *) 
            END (* IF *) 
          END (* WITH WTravInfo *) 
        END (* IF *) 
      END NpsMaybeListSlice 

  ; PROCEDURE NpsNewFsNode ( ) RAISES { Backout } 

    = VAR LFsDescendantRef : LangUtil . FsNodeRefTyp 

    ; BEGIN (* NpsNewFsNode *) 
        NpsSeFsRef . SeFsIndentPos 
          := TravUtil . IndentPos 
               ( ParseInfo . PiLang 
               , NpsSeEstRef . SeEstIndentPos 
               , NpsSeFsRef . SeFsNodeRef . FsIndentCode 
               ) 
      ; CASE NpsSeFsRef . SeFsNodeRef . FsKind <* NOWARN *>

        (* Beginning of image. *) 
        OF FsKindTyp . FsKindBegOfImage 
        => NpsResultStateRef . PtsTokInfo . TiTok := LbeStd . Tok__BegOfImage 
        ; NpsPassNewLine ( ) 
        ; NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
         ; NpsAttachedMods ( ) 

        (* Est child of list. *) 
        | FsKindTyp . FsKindEstChildOfList 
        => NpsMaybeListSlice ( ) 
        ; IF NpsDeliverState # DeliverStateTyp . DsDeliver 
          THEN (* Traverse as usual. *) 
            NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
          ; NpsAttachedMods ( ) 
          END (* IF *) 

        (* Other things possibly having leading mods: *) 
        | FsKindTyp . FsKindEndOfImage 
        => NpsResultStateEstRef . PtseStateKind 
             := ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
        ; NpsAttachedMods ( ) 

        | FsKindTyp . FsKindInsTok 
        , FsKindTyp . FsKindEstChildOfFixed 
        , FsKindTyp . FsKindLineBreakOpt 
        , FsKindTyp . FsKindLineBreakReqd 
        => NpsResultStateEstRef . PtseStateKind 
             := ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
        ; NpsAttachedMods ( ) 

        (* Ast String *) 
        | FsKindTyp . FsKindAstString 
        => Assert
            ( NpsSeFsRef . SeFsSeEstRef = NpsSeEstRef
            , AFT . AF_NpsNewFsNode_ast_string_bad_cross_link 
            )
        ; NpsBeginAstString ( ) 

        (* Subtree nodes. *) 
        | FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeHoriz 
        , FsKindTyp . FsKindSubtreeFill 
        => NpsSeFsRef . SeFsFmtKind  
             := TravUtil . FmtKindForFsSubtreeTraversing 
                  ( Lang := ParseInfo . PiLang 
                  , CharPos := NpsResultStateRef . PtsScanInfo . SiCharPos 
                  , ModTextIsToLeftOnLine 
                      := NpsResultStateEstRef . PtseModTextIsToLeftOnLine 
                  , PrevTok := NpsResultStateEstRef . PtsePrevTokBefore 
                  , RootFsNodeRef := NpsSeEstRef . SeEstRootFsNodeRef
                  , SubtreeFsNodeRef := NpsSeFsRef . SeFsNodeRef 
                  , ParentFmtKind := NpsSeFsRef . SeFsLink . SeFsFmtKind 
                  , CurrentLineIndentPos := NpsSeEstRef . SeEstIndentPos 
                  , EstTravInfo := NpsSeEstRef . SeEstTravInfo 
                  , (* IN OUT *) LastFmtNoOnLine 
                      := NpsResultStateEstRef . PtseLastFmtNoOnLine
                  , (* IN OUT *) EstListChildrenToPass 
                      := NpsResultStateEstRef . PtseEstListChildrenToPass
                  ) 

        ;  NpsPushFsStackElemForFsChild 
             ( NpsSeFsRef . SeFsNodeRef 
             , 0 
             , LangUtil . FsKindSetChildOfFixed 
             ) 
        
        (* Fixed nodes. *) 
        | FsKindTyp . FsKindEstFixedVert 
        , FsKindTyp . FsKindEstFixedHoriz 
        , FsKindTyp . FsKindEstFixedFill 
        => Assert
            ( NpsSeFsRef . SeFsSeEstRef = NpsSeEstRef
            , AFT . AF_NpsNewFsNode_est_fixed_node_bad_cross_link 
            )
        ; NpsSeFsRef . SeFsFmtKind  
            := TravUtil . FmtKindForEstTraversing  
                 ( Lang := ParseInfo . PiLang 
                 , CharPos := NpsResultStateRef . PtsScanInfo . SiCharPos 
                 , ModTextIsToLeftOnLine 
                     := NpsResultStateEstRef . PtseModTextIsToLeftOnLine 
                 , PrevTok := NpsResultStateEstRef . PtsePrevTokBefore 
                 , FsKind := NpsSeFsRef . SeFsNodeRef . FsKind 
                 , ParentFmtKind := NpsSeFsRef . SeFsLink . SeFsFmtKind 
                 , FirstLineIndentPos := NpsSeEstRef . SeEstIndentPos1 
                 , EstRef := NpsSeEstRef . SeEstTravInfo . EtiNodeRef 
                 ) 
        ; NpsPushFsStackElemForFsChild 
            ( NpsSeFsRef . SeFsNodeRef 
            , 0 
            , LangUtil . FsKindSetChildOfFixed 
            ) 
        
        (* List nodes. *) 
        | FsKindTyp . FsKindEstListVert 
        , FsKindTyp . FsKindEstListHoriz 
        , FsKindTyp . FsKindEstListFill 
        , FsKindTyp . FsKindEstListTrailVert 
        , FsKindTyp . FsKindEstListTrailHoriz 
        , FsKindTyp . FsKindEstListTrailFill 
        => Assert
            ( NpsSeFsRef . SeFsSeEstRef = NpsSeEstRef
            , AFT . AF_NpsNewFsNode_est_list_node_bad_cross_link 
            )
        ; IF NpsSeEstRef . SeEstTravInfo . EtiChildCt > 0 
          THEN 
            NpsSeFsRef . SeFsFmtKind  
              := TravUtil . FmtKindForEstTraversing  
                   ( Lang := ParseInfo . PiLang 
                   , CharPos := NpsResultStateRef . PtsScanInfo . SiCharPos 
                   , ModTextIsToLeftOnLine 
                       := NpsResultStateEstRef . PtseModTextIsToLeftOnLine 
                   , PrevTok := NpsResultStateEstRef . PtsePrevTokBefore 
                   , FsKind := NpsSeFsRef . SeFsNodeRef . FsKind 
                   , ParentFmtKind := NpsSeFsRef . SeFsLink . SeFsFmtKind 
                   , FirstLineIndentPos := NpsSeEstRef . SeEstIndentPos1 
                   , EstRef := NpsSeEstRef . SeEstTravInfo . EtiNodeRef 
                   ) 
          ; NpsPushFsStackElemForFsChild 
              ( NpsSeFsRef . SeFsNodeRef 
              , 0 
              , LangUtil . FsKindSet0thChildOfList 
              ) 
          ELSE 
            NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
          END (* IF *) 

        (* Conditional formatting. *) 
        | FsKindTyp . FsKindCondFmt 
        => NpsMaybeListSlice ( ) 
        ; IF NpsDeliverState # DeliverStateTyp . DsDeliver 
          THEN (* Continue traversing. *) 
             NpsSeFsRef . SeFsPredicate 
               := TravUtil . DoCondFmtFwd 
                    ( ParseInfo . PiLang 
                    , NpsSeEstRef . SeEstTravInfo  
                    , NpsSeFsRef . SeFsNodeRef 
                    ) 
          ; IF NpsSeFsRef . SeFsPredicate 
            THEN (* Handle leading formatters. *) 
              NpsPushFsStackElemForFsChild 
                ( NpsSeFsRef . SeFsNodeRef 
                , 0 
                , LangUtil . FsKindSetChildOfCondFmt 
                ) 
            ELSE (* Try another alternative, which should always exist. *) 
              LFsDescendantRef 
                := NpsSeFsRef . SeFsNodeRef . FsCondAltRef
            ; NpsPushFsStackElem 
                ( LFsDescendantRef , LangUtil . FsChildNoAlt ) 
            END (* IF *) 
          END (* IF *) 

        END (* CASE *) 
      END NpsNewFsNode

  ; PROCEDURE NpsAfterFsChildOfFsList 
      ( FsChildNo : LangUtil . FsChildNoTyp ; TrailingSepsExist : BOOLEAN ) 

    = BEGIN 
        Assert 
           ( FsChildNo >= 0 
           , AFT . A_NpsDoneWithFsNodeListNegativeSelfFsNo 
           ) 
      ; IF FsChildNo = LangUtil . FsChildNoListEstChild  
        THEN (* Just finished a slice, the Est list child or a sublist or
                CondFmt node containing it. *) 
          IF NpsSeEstRef . SeEstIsSublist 
             AND NpsSeEstRef . SeEstTravInfo . EtiChildNo 
                 >= NpsSeEstRef . SeEstWholeSliceToChildNo 
          THEN (* We are at the end of a sublist *) 
            NpsCheckEstSuccessor ( )  
          ; IF NpsDeliverState = DeliverStateTyp . DsDeliver 
            THEN (* We went to a previously computed state. *) 
              RETURN
            ELSE 
              NpsComputeStateAfterListSlice ( ) 
            END (* IF *) 
          END (* IF *) 
        ; IF NpsSeEstRef . SeEstTravInfo . EtiChildNo 
             >= NpsSeEstRef . SeEstTravInfo . EtiChildCt 
          THEN (* No more Est children. *) 
            IF TrailingSepsExist 
            THEN (* Handle the first trailing separator. *) 
              NpsPushFsStackElemForFsChild 
                ( NpsSeFsRef . SeFsNodeRef 
                , FsChildNo + 1  
                , LangUtil . FsKindSetFormattingList 
                  + LangUtil . FsKindSetSubtree 
                ) 
            ; RETURN  
            ELSE (* No trailing separators.  Go around the state 
                    machine to finish with the parent Fs node too. *) 
              NpsResultStateEstRef . PtseStateKind 
                := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
            ; RETURN
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      ; INC ( FsChildNo ) 
      ; IF FsChildNo 
           < NUMBER ( NpsSeFsRef . SeFsNodeRef . FsChildren ^ ) 
        THEN (* Handle a(nother) list separator *) 
          NpsPushFsStackElemForFsChild 
            ( NpsSeFsRef . SeFsNodeRef 
            , FsChildNo 
            , LangUtil . FsKindSetFormattingList 
              + LangUtil . FsKindSetSubtree 
            ) 
        ELSIF NpsSeEstRef . SeEstTravInfo . EtiChildNo 
              >= NpsSeEstRef . SeEstTravInfo . EtiChildCt 
        THEN (* Finished the last trailing separator.  Go around the state 
                machine to finish with the parent Fs node too. *) 
          NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
        ELSE (* Handle another list Est child *) 
          NpsPushFsStackElemForFsChild 
            ( NpsSeFsRef . SeFsNodeRef 
            , LangUtil . FsChildNoListEstChild  
            , LangUtil . FsKindSet0thChildOfList 
            ) 
        END (* IF *) 

      END NpsAfterFsChildOfFsList 

  ; PROCEDURE NpsDoneWithFsNode ( ) RAISES { Backout } 

    = VAR LOldSeFsRef : ParseHs . StackElemFsTyp  
    ; VAR LFsChildNo : LangUtil . FsChildNoTyp 
    ; VAR LLeadingChildCt : LbeStd . EstChildNoTyp 

    ; BEGIN (* NpsDoneWithFsNode *) 
        LOldSeFsRef := NpsSeFsRef 

      (* Pop Fs stack element *) 
      ; NpsSeFsRef := NpsSeFsRef . SeFsLink 
      ; NpsResultStateEstRef . PtseStackFsRef := NpsSeFsRef 
 
      ; CASE LOldSeFsRef . SeFsNodeRef . FsKind 
        OF FsKindTyp . FsKindEstFixedVert 
        , FsKindTyp . FsKindEstFixedHoriz 
        , FsKindTyp . FsKindEstFixedFill 
        , FsKindTyp . FsKindEstListVert 
        , FsKindTyp . FsKindEstListHoriz 
        , FsKindTyp . FsKindEstListFill 
        , FsKindTyp . FsKindEstListTrailVert 
        , FsKindTyp . FsKindEstListTrailHoriz 
        , FsKindTyp . FsKindEstListTrailFill 
        , FsKindTyp . FsKindAstString  
        => (* We just popped the root of an Fs tree. *) 
          IF NpsSeEstRef . SeEstIsSublist 
          THEN 
            Assert
               ( LOldSeFsRef . SeFsSeEstRef = NpsSeEstRef . SeEstLink  
               , AFT . A_NpsDoneWithFsNode_sublist_crosslink_mismatch 
               ) 
          ; NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp 
                 . PtsKindDoneWithListSliceTraversed 
          ELSE 
            Assert
               ( LOldSeFsRef . SeFsSeEstRef = NpsSeEstRef 
               , AFT . A_NpsDoneWithFsNode_crosslink_mismatch  
               ) 
          ; NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithEstTraversed 
          END (* IF *) 
        ELSE (* Now examine the stack element beneath *) 
          IF LOldSeFsRef . SeFsNodeRef . FsKind 
             = FsKindTyp . FsKindEstChildOfList 
          THEN (* Est list child will always have been present. *) 
            TravUtil . PassEstListChild 
              ( NpsResultStateEstRef . PtseEstListChildrenToPass ) 
          END (* IF *) 
        ; LFsChildNo := LOldSeFsRef . SeFsSelfFsChildNo 
        ; CASE NpsSeFsRef . SeFsNodeRef . FsKind 

          OF FsKindTyp . FsKindEstFixedVert 
          , FsKindTyp . FsKindEstFixedHoriz 
          , FsKindTyp . FsKindEstFixedFill 
          , FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill 
          => INC ( LFsChildNo ) 
          ; IF LFsChildNo 
               < NUMBER ( NpsSeFsRef . SeFsNodeRef . FsChildren ^ ) 
            THEN 
              NpsPushFsStackElemForFsChild 
                ( NpsSeFsRef . SeFsNodeRef 
                , LFsChildNo 
                , LangUtil . FsKindSetChildOfFixed 
                ) 
            ELSE 
              NpsResultStateEstRef . PtseStateKind 
                := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
              (* ^Done with the parent too. *) 
            END (* IF *) 

          | FsKindTyp . FsKindEstListVert 
          , FsKindTyp . FsKindEstListHoriz 
          , FsKindTyp . FsKindEstListFill 
          => NpsAfterFsChildOfFsList ( LFsChildNo , TrailingSepsExist := FALSE )

          | FsKindTyp . FsKindEstListTrailVert 
          , FsKindTyp . FsKindEstListTrailHoriz 
          , FsKindTyp . FsKindEstListTrailFill 
          => NpsAfterFsChildOfFsList 
               ( LFsChildNo 
               , TrailingSepsExist 
                   := NpsSeEstRef . SeEstTravInfo . EtiParentRef . EstNodeKind 
                      = EstHs . EstNodeKindTyp . EstNodeKindTrail 
                      AND NUMBER ( NpsSeFsRef . SeFsNodeRef . FsChildren ^ ) > 1
               )

          | FsKindTyp . FsKindCondFmt 
          => IF NpsSeFsRef . SeFsPredicate 
            THEN (* We are doing all the formatters. *)  
              INC ( LFsChildNo ) 
            ; LLeadingChildCt 
                := NpsSeFsRef . SeFsNodeRef . FsLeadingChildCt 
            ; IF LFsChildNo < LLeadingChildCt 
              THEN (* Do another leading formatter *) 
                   (* Can't get here unless SeFsPredicate *) 
                NpsPushFsStackElemForFsChild 
                  ( NpsSeFsRef . SeFsNodeRef 
                  , LFsChildNo 
                  , LangUtil . FsKindSetFormatting 
                    + LangUtil . FsKindSetSubtree  
                  ) 
              ELSIF LFsChildNo = LLeadingChildCt 
              THEN (* Do the Est Child *) 
                NpsPushFsStackElemForFsChild 
                  ( NpsSeFsRef . SeFsNodeRef 
                  , LFsChildNo 
                  , LangUtil . FsKindSetEstChild + LangUtil . FsKindSetSubtree 
                  ) 
              ELSIF LFsChildNo 
                    < NUMBER ( NpsSeFsRef . SeFsNodeRef . FsChildren ^ ) 
              THEN (* More trailing separators *) 
                   (* Can't get here unless SeFsPredicate *) 
                NpsPushFsStackElemForFsChild 
                  ( NpsSeFsRef . SeFsNodeRef 
                  , LFsChildNo 
                  , LangUtil . FsKindSetFormatting 
                    + LangUtil . FsKindSetSubtree  
                  ) 
              ELSE (* Done with CondFmt node *) 
                NpsResultStateEstRef . PtseStateKind 
                  := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
              END (* IF *) 
            ELSE (* We only did the Est descendant.  Now we are done. *) 
              NpsResultStateEstRef . PtseStateKind 
                := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
            END (* IF*) 

          ELSE (* of CASE *) 
            CantHappen ( AFT . A_NpsDoneWithFsNodeBadFsKind ) 
          END (* CASE *) 
        END (* CASE *) 
      END NpsDoneWithFsNode 

  ; PROCEDURE NpsPushEstPlain 
      ( IsModTok : BOOLEAN ; VAR FsNodeRef : LangUtil . FsNodeRefTyp) 
    RAISES { Backout } 

    = VAR LNewSeEstRef : ParseHs . StackElemEstTyp 
    ; VAR LChildRef : LbeStd . EstRootTyp 
    ; VAR LFsNodeRef : LangUtil . FsNodeRefTyp 

    ; BEGIN (* NpsPushEstPlain *) 
        WITH WOldEstTravInfo = NpsSeEstRef . SeEstTravInfo 
        DO 
          LChildRef 
            := WOldEstTravInfo . EtiChildLeafElem . LeChildRef 
        ; LFsNodeRef 
            := LangUtil . FsRuleForEstChild 
                 ( ParseInfo . PiLang 
                 , NpsSeFsRef . SeFsNodeRef 
                 , WOldEstTravInfo . EtiChildLeafElem  
                 ) 
        ; FsNodeRef := LFsNodeRef 
        ; Assert 
            ( LFsNodeRef . FsKind IN LangUtil . FsKindSetEst 
            , AFT . A_NpsPushEstPlain_BadFsKind 
            ) 
        ; LNewSeEstRef := NEW ( ParseHs . StackElemEstTyp ) 
        ; TravUtil . InitEstTravInfoFwd 
            ( (* VAR *) LNewSeEstRef . SeEstTravInfo 
            , LChildRef 
            , WOldEstTravInfo . EtiChildLeafElem . LeKindSet 
            , WOldEstTravInfo . EtiAbsNodeNo 
              + WOldEstTravInfo . EtiChildRelNodeNo  
            ) 
        ; IF IsModTok 
          THEN 
            LNewSeEstRef . SeEstIndentPos1 := NpsSeEstRef . SeEstIndentPos
          ; LNewSeEstRef . SeEstIndentPosN := NpsSeEstRef . SeEstIndentPos
          ELSE 
            TravUtil . ChildIndentPositions 
              ( ParseInfo . PiLang 
              , NpsSeFsRef . SeFsNodeRef 
                (* ^Can be NIL for the topmost root node of an Est. *) 
              , NpsSeEstRef . SeEstIndentPos1 
              , NpsSeEstRef . SeEstIndentPosN
              , (* VAR *) ChildIndentPos1 := LNewSeEstRef . SeEstIndentPos1 
              , (* VAR *) ChildIndentPosN := LNewSeEstRef . SeEstIndentPosN 
              , IsFirstLine := NpsSeEstRef . SeEstIsFirstLine 
              ) 
          END (* IF *) 
        ; LNewSeEstRef . SeEstIsFirstLine := TRUE  
        ; LNewSeEstRef . SeEstIndentPos := LNewSeEstRef . SeEstIndentPos1  
        ; LNewSeEstRef . SeEstAdvanceState := NIL 
        ; LNewSeEstRef . SeEstWholeSliceToChildNo := 0 
        ; LNewSeEstRef . SeEstWholeSliceRMEstChildNo := 0 
        ; LNewSeEstRef . SeEstIsSublist := FALSE  
        ; LNewSeEstRef . SeEstRootFsNodeRef := LFsNodeRef 
        ; LNewSeEstRef . SeEstLink := NpsSeEstRef 
        ; NpsSeEstRef := LNewSeEstRef 
        ; NpsResultStateEstRef . PtseStackEstRef := LNewSeEstRef 
        ; NpsInEstPopPhase := FALSE 
        END (* WITH *) 
      END NpsPushEstPlain

  ; PROCEDURE NpsRangeTempMarksForModTok 
      ( ModTok : EstHs . EstRefTyp 
      ; Tok : LbeStd . TokTyp 
      ; RangeToSearch : ParseHs . TempMarkRangeTyp 
      ) 
    RAISES { Backout }
    (* If Tok is not a constant terminal, set TiPatchTempMarkRange to empty.
       Otherwise, skip any temp marks in RangeToSearch that are not ChildFmtNo
       marks, then set TiPatchTempMarkRange to the contiguous group of temp
       marks in RangeToSearch that are ChildFmtNo marks, each of which must
       point to ModTok.  This group could be empty. *)

    = VAR LTempMarkSs : LbeStd . MarkNoTyp 

    ; BEGIN
        WITH WPatchRange 
             = NpsResultStateRef . PtsTokInfo . TiPatchTempMarkRange
        DO 
          WPatchRange := ParseHs . TempMarkRangeEmpty 
        ; IF NOT ParseHs . RangeIsEmpty ( RangeToSearch )
             AND LangUtil . TokClass ( ParseInfo . PiLang , Tok )  
                 = LbeStd . TokClassTyp . TokClassConstTerm 
             AND ParseInfo . PiOrigTempMarkListRef # NIL 
          THEN 
            LTempMarkSs := RangeToSearch . From 
          ; LOOP (* Thru' marks that are not ChildFmtNo. *)
              IF LTempMarkSs >= RangeToSearch . To 
              THEN EXIT (* Outer loop. *)
              ELSE
                WITH W1stTempMark 
                     = ParseInfo . PiOrigTempMarkListRef ^ [ LTempMarkSs ]
                DO IF W1stTempMark . TokMark . Kind = MarkKindTyp . ChildFmtNo  
                   THEN
                     Assert
                       ( W1stTempMark . TokMark . TkmEstRef = ModTok 
                       , AFT . A_NpsRangeTempMarksForModTok_BadChildFmtNoOnModTok 
                       ) 
                   ; WPatchRange . From := LTempMarkSs 
                   ; INC ( LTempMarkSs ) 
                   ; LOOP (* Thru' additional ChildFmtNo marks. *)
                       IF LTempMarkSs >= RangeToSearch . To 
                       THEN EXIT (* Inner. *)
                       ELSE
                         WITH WLaterTempMark
                              = ParseInfo . PiOrigTempMarkListRef
                                ^ [ LTempMarkSs ]
                         DO
                           IF WLaterTempMark . TokMark . Kind 
                              = MarkKindTyp . ChildFmtNo  
                           THEN 
                             Assert
                               ( WLaterTempMark . TokMark . TkmEstRef = ModTok 
                               , AFT . A_NpsRangeTempMarksForModTok_BadChildFmtNoOnModTok2 
                               ) 
                           ; INC ( LTempMarkSs )
                           (* And go around inner loop. *)
                           ELSE EXIT (* Inner. *)
                           END (* IF *)
                         END (* WITH *) 
                       END (* IF *) 
                     END (* LOOP *) 
                   ; WPatchRange . To := LTempMarkSs 
                   ; EXIT (* Outer loop. *) 
                   ELSE INC ( LTempMarkSs ) 
                   (* And go around outer loop. *)
                   END (* IF *) 
                END (* WITH *) 
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *) 
        END (* WITH *) 
      END NpsRangeTempMarksForModTok 

  ; PROCEDURE NpsListCardTok 
      ( EstRef : LbeStd . EstRootTyp 
      ; FsNodeRef : LangUtil . FsNodeRefTyp 
      ; IsOptSingletonList : BOOLEAN 
      ) 
    : LbeStd . TokTyp 
    RAISES { Backout } 
    (* If appropriate, return the list cardinality token for the actual 
       complement of Est children.  Otherwise, just use the token from
       FsNodeRef 
    *) 

    = VAR LResult : LbeStd . TokTyp 
    ; VAR LChildNo : LbeStd . EstChildNoTyp 
    ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LLeafElem : EstHs . LeafElemTyp 

    ; BEGIN 
        LResult := FsNodeRef . FsTok (* Default. *) 
      ; IF ParseInfo . PiGram . IsGenerated 
           AND LangUtil . TokClass ( ParseInfo . PiLang , LResult ) 
               IN LbeStd . TokClassSetAsList  
        THEN 
          IF IsOptSingletonList 
          THEN (* Look for a singleton list token. *)  
            IF FsNodeRef . FsSingletonListTok # LbeStd . Tok__Null 
            THEN 
              LResult := FsNodeRef . FsSingletonListTok 
            END (* IF *) 
          ELSE 
            TYPECASE EstRef 
            OF NULL => 

            | EstHs . EstRefTyp ( TEstRef ) 
            => EstUtil . NextInKindSet 
                 ( TEstRef 
                 , 0  
                 , EstHs . EstChildKindSetEstChild  
                 , (* VAR *) ResultChildNo := LChildNo 
                 , (* VAR *) ResultChildRelNodeNo := LNodeNo 
                 , (* VAR *) ResultLeafElem := LLeafElem 
                 ) 
            ; IF LChildNo >= EstUtil . EstChildCt ( EstRef ) 
              THEN (* Zero Est children. *) 
                IF FsNodeRef . FsEmptyListTok # LbeStd . Tok__Null 
                THEN 
                  LResult := FsNodeRef . FsEmptyListTok 
                END (* IF *) 
              ELSE 
                EstUtil . NextInKindSet 
                  ( TEstRef 
                  , LChildNo + 1   
                  , EstHs . EstChildKindSetEstChild  
                  , (* VAR *) ResultChildNo := LChildNo 
                  , (* VAR *) ResultChildRelNodeNo := LNodeNo 
                  , (* VAR *) ResultLeafElem := LLeafElem 
                  ) 
              ; IF LChildNo >= EstUtil . EstChildCt ( EstRef ) 
                THEN (* One Est child. *) 
                  IF FsNodeRef . FsSingletonListTok # LbeStd . Tok__Null 
                  THEN 
                    LResult := FsNodeRef . FsSingletonListTok 
                  END (* IF *) 
                ELSE (* At least two Est children. *) 
                  IF FsNodeRef . FsPluralListTok # LbeStd . Tok__Null 
                  THEN 
                    LResult := FsNodeRef . FsPluralListTok 
                  END (* IF *) 
                END (* IF *) 
              END (* IF *) 

            ELSE 
            END (* TYPECASE *) 
          END (* IF *) 
        END (* IF *) 
      ; RETURN LResult 
      END NpsListCardTok 

  ; PROCEDURE NpsNewEst ( ) RAISES { Backout } 
    (* The new Est is EtiChildLeafElem . LeChildRef, or a slice beginning
       there. *)  

    = VAR LModTok : EstHs . EstRefTyp := NIL 
    ; VAR LTok : LbeStd . TokTyp 
    ; VAR LEstMiscInfo : EstHs . EstMiscInfoTyp 
    ; VAR LFsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR LIsAstString : BOOLEAN := FALSE 

    ; BEGIN (* NpsNewEst *) 
        WITH WParentTravInfo = NpsSeEstRef . SeEstTravInfo 
        DO 
          TYPECASE WParentTravInfo . EtiChildLeafElem . LeChildRef <* NOWARN *>
          OF NULL 
          => CantHappen ( AFT . A_NpsNewEst_NIL ) 

          | SharedStrings . T 
          => LIsAstString := TRUE  

          | EstHs . EstRefTyp ( TEstRef ) 
          => IF TEstRef . EstNodeKind 
                = EstHs . EstNodeKindTyp . EstNodeKindModTok 
            THEN 
              LModTok := TEstRef 
            END (* IF *) 
          END (* TYPECASE *) 
(* TODO: ^ Put the above code into a TravUtil or EstUtil procedure. *) 
        ; IF LIsAstString 
             (* ^For an Ast string, we need to go through the Fs tree steps,
                even if we are not rescanning, because it can have leading
                mods already accumulated and trailing mods to be found.
                Not so with proper Est subtrees. *)  
             OR NpsResultStateRef . PtsScanInfo . SiScanState 
                # LbeStd . SsIdle 
          THEN
            NpsPushEstPlain 
              ( IsModTok := LModTok # NIL 
              , (* VAR *) FsNodeRef := LFsNodeRef 
              ) 
          ; NpsPushFsStackElem ( LFsNodeRef , SelfFsChildNo := 0 ) 
          ELSIF EstHs . EstChildKindSetMustReparse 
                * WParentTravInfo . EtiChildLeafElem . LeKindSet 
                # EstHs . EstChildKindSetEmpty 
          THEN (* Descend into the Est subtree. *) 
            NpsPushEstPlain 
              ( IsModTok := LModTok # NIL 
              , (* VAR *) FsNodeRef := LFsNodeRef 
              ) 
          ; NpsPushFsStackElem ( LFsNodeRef , SelfFsChildNo := 0 ) 
          ELSIF NpsDeliverState = DeliverStateTyp . DsTokFound 
          THEN (* Stop short of this Est. *)  
            NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp . PtsKindRevisitNewEst 
          ; NpsDeliverState := DeliverStateTyp . DsDeliver 
          ELSIF LModTok # NIL 
          THEN (* Deliver the children of the ModTok, as if they constituted 
                  an ordinary token. *)   
            NpsAccumSlice
              ( LModTok , 0 , LModTok . KTreeChildCt ( ) , SyntTokCt := 1 ) 
          ; LTok 
              := LangUtil . VarTermTok 
                   ( ParseInfo . PiLang , EstUtil . EstTok ( LModTok ) ) 
          ; NpsResultStateRef . PtsTokInfo . TiTok := LTok 
          ; NpsSkipTempMarksForSubtrees 
              ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
              , NpsSeEstRef . SeEstTravInfo . EtiChildNo 
              , NpsSeEstRef . SeEstTravInfo . EtiChildNo + 1 
              ) 
          (* ^Any RightSib TempMarks pointing to the ModTok belong to a 
              following InsTok. *)
          ; NpsRangeTempMarksForModTok
              ( LModTok , LTok , RangeToSearch := NpsWaitingTempMarkRange )
            (* ^May put some into TiPatchTempMarkRange. *)
          ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
          ; NpsPushEstPlain 
              ( IsModTok := TRUE , (* VAR *) FsNodeRef := LFsNodeRef ) 
          ; NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp 
                 . PtsKindDoneWithEstUntraversed 
          ; NpsDeliverState := DeliverStateTyp . DsDeliver 
            (* ^ModTok has no trailing mods to its right, at this level.  
                They are all pulled down inside it. *) 
          ELSE (* Deliver whole Est subtree. *) 
            LEstMiscInfo 
              := EstUtil . EstMiscInfo 
                   ( ParseInfo . PiLang 
                   , WParentTravInfo . EtiChildLeafElem . LeChildRef 
                   ) 
          ; IF WParentTravInfo . EtiParentRef = NIL 
               (* ^It's the entire, root Est. *) 
               OR EstHs . EstChildKindFirstOfGroup 
                  IN WParentTravInfo . EtiChildLeafElem . LeKindSet 
            THEN (* LM child for its FmtNo, there are no mods to left of
                    the new Est. *) 
              NpsAccumNode
                ( WParentTravInfo . EtiChildLeafElem . LeChildRef 
                , WParentTravInfo . EtiChildLeafElem . LeKindSet 
                , LEstMiscInfo . EmiSyntTokCt 
                ) 
            ; NpsSkipTempMarksForSubtrees 
                ( EstRef := WParentTravInfo . EtiParentRef 
                , FromChildNo := WParentTravInfo . EtiChildNo 
                , ToChildNo := WParentTravInfo . EtiChildNo + 1 
                ) 
              (* ^RightSib TempMarks pointing to RM child belong to a following 
                 InsTok. *)
            ELSE (* Making this a singleton slice of the parent might create 
                    opportunities to combine it with other children into a 
                    longer slice, after parse and build. *) 
              NpsAccumSlice 
                ( WParentTravInfo . EtiParentRef 
                , WParentTravInfo . EtiChildNo 
                , WParentTravInfo . EtiChildNo + 1 
                , LEstMiscInfo . EmiSyntTokCt 
                ) 
            ; NpsSkipTempMarksForSubtrees 
                ( EstRef := WParentTravInfo . EtiParentRef 
                , FromChildNo := WParentTravInfo . EtiChildNo 
                , ToChildNo := WParentTravInfo . EtiChildNo + 1 
                ) 
              (* ^RightSib TempMarks pointing to RM child belong to a following 
                 InsTok. *)
            END (* IF *) 
          ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
          ; NpsResultStateRef . PtsTokInfo . TiIsInterior := TRUE 
          ; NpsPushEstPlain 
              ( IsModTok := FALSE , (* VAR *) FsNodeRef := LFsNodeRef ) 
          ; NpsResultStateRef . PtsTokInfo . TiTok 
              := NpsListCardTok 
                   ( WParentTravInfo . EtiChildLeafElem . LeChildRef 
                   , LFsNodeRef 
                   , EstHs . EstChildKindOptSingletonList 
                     IN WParentTravInfo . EtiChildLeafElem . LeKindSet  
                   )  
          ; NpsResultStateEstRef . PtseStateKind 
              := ParseHs . ParseTravStateKindTyp 
                 . PtsKindDoneWithEstUntraversed 
          ; NpsDeliverState := DeliverStateTyp . DsDeliver 
            (* ^This will cause immediate return, without traversing through
                trailing mods.  An untraversed whole subtree cannot have any 
                of these, either before or after rescanning.  If there should 
                accidentally be one, it should get a lost mod failure. *)  
          END (* IF *) 
        END (* WITH WParentTravInfo *) 
      END NpsNewEst 

  ; PROCEDURE NpsComputeStateAfterListSlice ( ) RAISES { Backout } 

    = VAR LLeafElem : EstHs . LeafElemTyp 
    ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LOldSeEstRef : ParseHs . StackElemEstTyp 

    ; BEGIN (* NpsComputeStateAfterListSlice *) 
        IF NpsResultStateKind ( ) 
           = ParseHs . ParseTravStateKindTyp 
             . PtsKindDoneWithListSliceUntraversed
        THEN 
          EstUtil . GetIthChild 
            ( NpsSeEstRef . SeEstTravInfo . EtiParentRef 
            , NpsSeEstRef . SeEstWholeSliceToChildNo - 1 
            , (* VAR *) ResultChildRelNodeNo := LNodeNo (* Dead. *) 
            , (* VAR *) ResultLeafElem := LLeafElem 
            ) 
        ; NpsResultStateRef . PtsPrevTokAfter 
            := EstUtil . RightTokForEst 
                 ( ParseInfo . PiLang , LLeafElem . LeChildRef ) 
        ; NpsResultStateEstRef . PtsePrevTokBefore 
            := NpsResultStateRef . PtsPrevTokAfter 
        END (* IF *) 
      ; Assert
          ( NpsSeEstRef . SeEstIsSublist  
          , AFT . A_NpsComputeStateAfterListSlice_NonlistEstStackElement  
          ) 
      ; LOldSeEstRef := NpsSeEstRef 
      (* Pop the Est stack: *) 
      ; NpsSeEstRef := NpsSeEstRef . SeEstLink 
      ; NpsResultStateEstRef . PtseStackEstRef := NpsSeEstRef 
      ; INC ( NpsEstPopCt , ORD ( NpsInEstPopPhase ) ) 
      ; NpsSeEstRef . SeEstIsFirstLine := FALSE 
      ; NpsSeEstRef . SeEstIndentPos := NpsSeEstRef . SeEstIndentPosN 
(* TODO: ^These assume a list always has a line break as a separator.
         This is a *really* good idea anyway.  Fix the Ldls to ensure it.
*) 
      ; Assert
          ( NpsSeEstRef # NIL 
          , AFT . A_NpsComputeStateAfterListSlice_FixedEmptyStack  
          ) 
      ; TravUtil . SetToIthChild 
          ( (* IN OUT *) NpsSeEstRef . SeEstTravInfo  
          , LOldSeEstRef . SeEstWholeSliceToChildNo 
          ) 
      END NpsComputeStateAfterListSlice  

  ; PROCEDURE NpsComputeStateAfterEst ( ) RAISES { Backout } 

    = BEGIN (* NpsComputeStateAfterEst *) 
        IF NpsResultStateKind ( ) 
           = ParseHs . ParseTravStateKindTyp . PtsKindDoneWithEstUntraversed 
        THEN 
          TYPECASE NpsSeEstRef . SeEstTravInfo . EtiNodeRef <* NOWARN *>
          OF NULL => 
          | EstHs . EstRefTyp ( TEstRef ) 
          => NpsResultStateRef . PtsPrevTokAfter  
              := EstUtil . RightTokForEst ( ParseInfo . PiLang , TEstRef ) 
          ; NpsResultStateEstRef . PtsePrevTokBefore 
              := NpsResultStateRef . PtsPrevTokAfter  
          ; IF TEstRef . KTreeWidthInfo . WiHasNlBefore 
               OR TEstRef . KTreeWidthInfo . WiHasNlAfter 
               OR TEstRef . KTreeWidthInfo . WiWidth  
                  = LbeStd . LimitedCharNoInfinity 
               OR EstUtil . CharPosPlusWidthInfo 
                    ( NpsSeEstRef . SeEstIndentPos  
                    , TEstRef . KTreeWidthInfo   
                    ) 
                  = LbeStd . LimitedCharNoInfinity 
            THEN
              NpsResultStateEstRef . PtseLastFmtNoOnLine 
                := EstHs . FmtNoUnknown 
            ; NpsResultStateEstRef . PtseEstListChildrenToPass := 0 (* Dead. *)
            END (* IF *) 
          | SharedStrings . T ( TStringRef ) 
          => NpsResultStateRef . PtsPrevTokAfter  
               := SharedStrings . Tok ( TStringRef ) 
          ; NpsResultStateEstRef . PtsePrevTokBefore  
              := NpsResultStateRef . PtsPrevTokAfter  
          ; IF EstUtil . WidthSum 
                 ( NpsSeEstRef . SeEstIndentPos 
                 , SharedStrings . Length ( TStringRef ) 
                 ) 
               = LbeStd . LimitedCharNoInfinity 
            THEN 
              NpsResultStateEstRef . PtseLastFmtNoOnLine 
                := EstHs . FmtNoUnknown 
            ; NpsResultStateEstRef . PtseEstListChildrenToPass := 0 (* Dead. *)
            END (* IF *) 
          END (* TYPECASE  *) 
        END (* IF *) 

      (* Pop the Est stack. *) 
      ; NpsSeEstRef := NpsSeEstRef . SeEstLink 
      ; NpsResultStateEstRef . PtseStackEstRef := NpsSeEstRef 
      ; INC ( NpsEstPopCt , ORD ( NpsInEstPopPhase ) ) 
      ; Assert
          ( NpsSeEstRef # NIL 
          , AFT . A_NpsComputeStateAfterEstFixedEmptyStack  
          ) 
      ; IF EstUtil . IsModTok 
             ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef ) 
        THEN (* There could be more leading mods. *) 
          NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods   
        ELSIF LangUtil . TokClass 
                ( ParseInfo . PiLang 
                , EstUtil . EstTok 
                    ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem 
                      . LeChildRef 
                    ) 
                ) 
              IN LbeStd . TokClassSetNoTrailingMods 
        THEN (* These can have no trailing mods. Just a shortcut. *) 
          NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode  
        ELSE 
          NpsResultStateEstRef . PtseStateKind 
            := ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
        END (* IF *) 
      ; TravUtil . IncEstChild ( NpsSeEstRef . SeEstTravInfo  ) 
      END NpsComputeStateAfterEst 

  ; PROCEDURE NpsDeliverEndOfImage ( ) 
    RAISES { Backout } 

    = BEGIN (* NpsDeliverEndOfImage *) 
        NpsResultStateRef . PtsTokInfo . TiTok := LbeStd . Tok__EndOfImage 
      ; LOOP 
          NpsCheckNextTempMarkForInsTok 
            ( NpsSeFsRef . SeFsNodeRef . FsFmtNo ) 
        ; IF NOT NpsNextTempMarkIsRelevant 
          THEN EXIT 
          ELSE NpsIncludeTempMark ( ) 
          END (* IF *) 
        END (* LOOP *) 
      ; NpsAppendTempMarkRange 
          ( (* VAR *) ToRange 
            := NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange 
          ) 
      ; NpsAppendTempMarkRange 
          ( (* VAR *) ToRange 
            := NpsResultStateRef . PtsTokInfo . TiPatchTempMarkRange 
          ) 
      ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
      (* Rest of deliver fields are irrelevant for LbeStd . Tok__EndOfImage *) 
      ; NpsResultStateEstRef . PtseDescendStateRef 
          := NpsResultStateEstRef (* Circular *) 
      ; NpsResultStateEstRef . PtsAdvanceStateRef 
          := NpsResultStateEstRef (* Circular *) 
      ; NpsSeFsRef := NpsSeFsRef . SeFsLink 
      ; NpsResultStateEstRef . PtseStackFsRef := NpsSeFsRef 
      ; NpsDeliverState := DeliverStateTyp . DsDeliver 
      ; Assert
          ( NpsNextTempMarkSs 
            = NUMBER ( ParseInfo . PiOrigTempMarkListRef ^ ) 
          , AFT . A_NpsDeliverEndOfImage_Wrong_temp_mark_count_at_Eoi
          ) 
      END NpsDeliverEndOfImage

  ; PROCEDURE NpsNewStateInform ( )

    = BEGIN
        IF DoInformState
        THEN
          Assertions . MessageText
            ( "In parse traverse state "
              & ParseHs . ParseTravStateKindImage ( NpsResultStateKind ( ) )
            )
        END (* IF *)
      END NpsNewStateInform
      
  ; PROCEDURE NpsStateMachine ( ) RAISES { Backout } 

    (* NpsStateMachine only returns when it is time to deliver 
       a state *) 

    = BEGIN (* NpsStateMachine *) 
        WHILE NpsDeliverState # DeliverStateTyp . DsDeliver 
        DO CASE NpsResultStateKind ( ) 
           OF ParseHs . ParseTravStateKindTyp . PtsKindEndOfImage 
           => NpsDeliverEndOfImage ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindNewEst 
           => NpsNewEst ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindRevisitNewEst  
           => NpsCheckEstSuccessor ( ) 
           ; IF NpsDeliverState # DeliverStateTyp . DsDeliver 
             THEN 
               NpsNewEst ( ) 
             END (* IF *) 
           | ParseHs . ParseTravStateKindTyp 
             . PtsKindDoneWithListSliceUntraversed 
           , ParseHs . ParseTravStateKindTyp 
             . PtsKindDoneWithListSliceTraversed 
           => NpsCheckEstSuccessor ( )  
           ; IF NpsDeliverState # DeliverStateTyp . DsDeliver 
             THEN 
               NpsComputeStateAfterListSlice ( ) 
             ; NpsResultStateEstRef . PtseStateKind 
                 := ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
             END (* IF *) 
           | ParseHs . ParseTravStateKindTyp . PtsKindDoneWithEstUntraversed 
           , ParseHs . ParseTravStateKindTyp . PtsKindDoneWithEstTraversed 
           => NpsCheckEstSuccessor ( ) 
           ; IF NpsDeliverState # DeliverStateTyp . DsDeliver 
             THEN 
               NpsComputeStateAfterEst ( ) 
             END (* IF *) 
           | ParseHs . ParseTravStateKindTyp . PtsKindNewFsNode 
           => NpsNewFsNode ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindDoneWithFsNode 
           => NpsDoneWithFsNode ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindLeadingMods 
           , ParseHs . ParseTravStateKindTyp . PtsKindTrailingMods 
           => NpsAttachedMods ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindRescanInsTok 
           => NpsCheckNextTempMarkForInsTok 
                ( NpsSeFsRef . SeFsNodeRef . FsFmtNo ) 
           ; NpsRescanInsTok ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenInsTok 
           => NpsCheckNextTempMarkForInsTok  
                ( NpsSeFsRef . SeFsNodeRef . FsFmtNo ) 
           ; NpsRescanBlanks ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindInsideInsTok 
           => NpsCheckNextTempMarkForInsTok 
                ( NpsSeFsRef . SeFsNodeRef . FsFmtNo ) 
           ; NpsRescanString ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindRescanLexErrChars  
           => NpsCheckNextTempMarkForEstLeaf 
                ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef )
           ; NpsRescanLexErrChars ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindRescanAstString 
           => NpsCheckNextTempMarkForEstLeaf 
                ( NpsSeEstRef . SeEstTravInfo . EtiStringRef ) 
           ; NpsRescanAstString ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindInsideLexErrChars  
           => NpsCheckNextTempMarkForEstLeaf 
                ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef )
           ; NpsRescanString ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindInsideAstString 
           => NpsCheckNextTempMarkForEstLeaf 
                ( NpsSeEstRef . SeEstTravInfo . EtiStringRef ) 
           ; NpsRescanString ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenLexErrChars 
           => NpsCheckNextTempMarkForEstLeaf 
                ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef )
           ; NpsRescanBlanks ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenAstString
           => NpsCheckNextTempMarkForEstLeaf 
                ( NpsSeEstRef . SeEstTravInfo . EtiStringRef ) 
           ; NpsRescanBlanks ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindInsideModCmnt 
           , ParseHs . ParseTravStateKindTyp . PtsKindInsideModText 
           => NpsCheckNextTempMarkForEstLeaf 
                ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef )
           ; NpsRescanString ( ) 
           | ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenModCmnt 
           , ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenRescanModText 
           , ParseHs . ParseTravStateKindTyp . PtsKindBlanksThenLeadingMods 
           => NpsCheckNextTempMarkForEstLeaf 
                ( NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeChildRef )
           ; NpsRescanBlanks ( ) 
           ELSE 
             CantHappen ( AFT . A_NpsStateMachineBadPtsKind ) 
           END (* CASE PtsStateKind *)
        ; IF NOT CheckStacks ( NpsResultStateEstRef )
          THEN
            NpsNewStateInform ( ) 
          END (*( IF *) 
        END (* WHILE *) 
      ; NpsFlushSlice ( ) 
      END NpsStateMachine 

  ; PROCEDURE NpsAccumPreviouslyDeferredItem 
      ( DeferredInfo : ParseHs . DeferredInfoRefTyp ) 
    RAISES { Backout } 
    = VAR LTokIsMeaningful : BOOLEAN

    ; BEGIN (* NpsAccumPreviouslyDeferredItem *) 
        IF DeferredInfo # NIL 
        THEN 
          IF DeferredInfo . Tok # LbeStd . Tok__Null 
          THEN LTokIsMeaningful := TRUE
          ELSE
(* TODO:  It would be nicer to detect that this is a ModLexErr (as opposed
          to a LexErrorString) with another standard token code, setting
          it in NpsDssLexErr, DsTokFound case. But that requires a lot of
          work to get the new token code.
          FOLLOWUP: Can steal Tok__BadChars. *) 
            TYPECASE DeferredInfo . ObjRef
            OF NULL => LTokIsMeaningful := FALSE
            | ModHs . ModLexErrTyp => LTokIsMeaningful := TRUE 
            ELSE LTokIsMeaningful := FALSE 
            END (* TYPECASE *)
          END (* IF *) 
        ; IF LTokIsMeaningful 
          THEN 
            IF DeferredInfo . Tok >= LbeStd . Tok__FirstLangDep
            THEN NpsDeliverState := DeliverStateTyp . DsTokFound 
            END (* IF *) 
          ; NpsResultStateRef . PtsTokInfo . TiTok := DeferredInfo . Tok 
          ; NpsResultStateRef . PtsTokInfo . TiSyntTokCt 
              := DeferredInfo . SyntTokCt 
          ; NpsResultStateRef . PtsPrevTokAfter := DeferredInfo . Tok 
          ; NpsResultStateRef . PtsTokInfo . TiIsInsertionRepair 
              := DeferredInfo . IsInsertionRepair 
          ; NpsResultStateRef . PtsTokInfo . TiIsInterior 
              := DeferredInfo . IsInterior
          END (* IF *)

        ; NpsWaitingTempMarkRange := DeferredInfo . WaitingTempMarkRange
        ; NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange
            := DeferredInfo . FullTempMarkRange 
        ; NpsResultStateRef . PtsTokInfo . TiPatchTempMarkRange 
            := DeferredInfo . PatchTempMarkRange

        ; IF DeferredInfo . ObjRef # NIL 
          THEN 
            NpsAccumNode 
              ( DeferredInfo . ObjRef 
              , DeferredInfo . KindSet 
              , SyntTokCt := 0 
              )
              
          END (* IF *)
        END (* IF *) 
      END NpsAccumPreviouslyDeferredItem 

  ; PROCEDURE NpsInitNps ( ) 

    = BEGIN (* NpsInitNps *) 
        NpsDeliverState := DeliverStateTyp . DsStarting 
      ; NpsSliceParentRef := NIL 
      ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
      ; NpsNextTempMarkIsRelevant := FALSE 
      END NpsInitNps 

; VAR SeqNoToStop : INTEGER := 9

  ; PROCEDURE NpsParseFromEst ( ) RAISES { Backout } 

    = VAR LFromStateEstRef 
        := NARROW ( FromStateRef , ParseHs . ParseTravStateEstRefTyp ) 
    ; VAR LSliceListElemRef : ParseHs . SliceListElemRefTyp 
    ; VAR LSeEstRef : ParseHs . StackElemEstTyp 
    ; VAR LPopCt : PortTypes . Int32Typ 

(* These are redundant, just to make finding things easier in a debugger. *) 
; VAR LToStateEstRef : ParseHs . ParseTravStateEstRefTyp 
; <* UNUSED *> VAR LSuccKind : SuccKindTyp := SuccKind 

    ; BEGIN (* NpsParseFromEst *) 
        NpsSeFsRef := LFromStateEstRef . PtseStackFsRef 
      ; NpsSeEstRef := LFromStateEstRef . PtseStackEstRef 
; EVAL CheckStacks ( LFromStateEstRef ) 
      ; NpsEstPopCt := 0 
      ; NpsInEstPopPhase := TRUE 
      ; IF SuccKind = SuccKindTyp . SuccKindDescend 
        THEN

          (* Descend. *) 
          IF LFromStateEstRef . PtseDescendStateRef # NIL 
          THEN (* Reuse previously-computed descend state. *) 
            NpsResultStateRef := LFromStateEstRef . PtseDescendStateRef 
          ; NpsResultStateEstRef := LFromStateEstRef . PtseDescendStateRef 
            (* Note: end of file state is a fixed point of descend *) 
; LToStateEstRef := NpsResultStateRef 
          ELSE (* Must actually compute the descend successor *) 
            Assert 
              ( LFromStateEstRef . PtseStateKind 
                IN ParseHs . PtsKindSetDescendable 
              , AFT . A_NpsParseFromEst_DescendBadState 
              ) 
          ; NpsResultStateEstRef 
              := RefToNewCopyOfParseTravStateEst ( LFromStateEstRef ) 
          ; NpsResultStateEstRef . PtsSeqNo := ParseInfo . PiSeqNo 
          ; INC ( ParseInfo . PiSeqNo ) 
          ; NpsResultStateRef := NpsResultStateEstRef 
          ; NpsResultStateStreamRef := NIL (* Force RT error if used. *) 
          ; InitTokInfo ( NpsResultStateRef . PtsTokInfo )  
          ; NpsInitNps ( )
          ; NpsResultStateEstRef . PtsTokInfo . TiFullTempMarkRange
              := ParseHs . TempMarkRangeEmpty 
          ; NpsResultStateEstRef . PtsTokInfo . TiPatchTempMarkRange
              := ParseHs . TempMarkRangeEmpty 
          ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
          ; NpsNextTempMarkSs := NpsResultStateEstRef . PtseTokTempMarkSs 

          ; LSliceListElemRef := LFromStateEstRef . PtsTokInfo . TiInfo  
            (* ^Implied NARROW, always OK. *)  
          ; IF LFromStateEstRef . PtseStateKind 
               = ParseHs . ParseTravStateKindTyp 
                 . PtsKindDoneWithListSliceUntraversed  
            THEN (* "Descend" from a list slice, i.e. dismantle the slice. *) 
            (* Restore leading things left of 1st Est child in the slice. *)
              NpsResultStateRef . PtsTokInfo . TiInfo  
                := LSliceListElemRef . SlePredLink 
            ; NpsResultStateRef . PtsTokInfo . TiSliceListRMRoot 
                := LSliceListElemRef . SlePredLink 
              (* ^Just to make debugging easier. *) 
            ; Assert
                ( LSliceListElemRef . SleNodeRef 
                  = NpsSeEstRef . SeEstTravInfo . EtiParentRef 
                  AND LSliceListElemRef . SleIsSlice 
                  AND LSliceListElemRef . SleFrom 
                      <= NpsSeEstRef . SeEstTravInfo . EtiChildNo 
                  AND LSliceListElemRef . SleTo 
                      > NpsSeEstRef . SeEstTravInfo . EtiChildNo 
                , AFT . A_NpsParseFromEst_DescendBadSliceList  
                ) 
            ; IF LSliceListElemRef . SleFrom 
                 < NpsSeEstRef . SeEstTravInfo . EtiChildNo 
              THEN 
(* CHECK: How can this happen?  *) 
                CantHappen 
                  ( AFT . A_NpsParseFromEst_DescendBadEstChild ) 
              ; IF EstHs . EstChildKindFirstOfGroup 
                   IN NpsSeEstRef . SeEstTravInfo . EtiChildLeafElem . LeKindSet 
                THEN
                  NpsFlushSlice ( ) 
                END (* IF *) 
              ; NpsAccumSlice
                  ( LSliceListElemRef . SleNodeRef 
                  , LSliceListElemRef . SleFrom 
                  , NpsSeEstRef . SeEstTravInfo . EtiChildNo 
                  , SyntTokCt := 0 
                  ) 
              END (* IF *) 
            ; NpsResultStateEstRef . PtseStateKind 
                := ParseHs . ParseTravStateKindTyp . PtsKindNewFsNode 
              (* ^Arrange to revisit the previously pushed FsNode for the 
                 list rule. *) 
            ELSE  (* Descend into an untraversed, entire Est subtree. *)  
            (* Restore leading things before the 1st Est child in the slice. *)
              NpsResultStateRef . PtsTokInfo . TiInfo  
                := LSliceListElemRef . SlePredLink 
            ; NpsResultStateRef . PtsTokInfo . TiSliceListRMRoot 
                := LSliceListElemRef . SlePredLink 
              (* ^Just to make debugging easier. *) 
            ; Assert 
                ( TRUE OR LSliceListElemRef . SlePredLink = NIL 
                , AFT . A_NpsParseFromEst_DescendEstLeftovers  
                )  
(* FIXME: This can happen.  A blank line is a mod on a deleted InsTok. E.g.,
          MergeTxt will do this if delete "THEN", when it is on a line alone.
          Then the slice list will have the blank line and a proper Est subtree.
          In the case where it came up, the parser wanted to descend into the
          Est subtree, and this code will in that case, move the blank line
          on to the first descended-to token. In general, this could be an 
          Est subtree too.  If such a slice list gets accepted by the parser,
          we will have a mod on an Est subtree.  None of the traversers are
          set up to accept this. 
*) 
(* The following won't work:  By now,  NpsSeEstRef . SeEstLink . SeEstTravInfo
   will have been advanced to at least the next child. 
            ; LFsNodeRef 
                  := LangUtil . FsRuleForEstChild 
                       ( ParseInfo . PiLang 
                       , NpsSeFsRef . SeFsNodeRef 
                       , NpsSeEstRef . SeEstLink . SeEstTravInfo 
                         . EtiChildLeafElem 
                       ) 
            ; Assert 
                ( LFsNodeRef = NpsSeEstRef . SeEstRootFsNodeRef 
                , AFT . A_NpsParseFromEst_Fs_node_mismatch
                ) 
*) 
            ; NpsPushFsStackElem 
                ( NpsSeEstRef . SeEstRootFsNodeRef , SelfFsChildNo := 0 ) 
            END (* IF *) 
          ; NpsStateMachine ( ) 
          ; NpsResultStateEstRef . PtseTokTempMarkSs := NpsNextTempMarkSs 
          ; LFromStateEstRef . PtseDescendStateRef := NpsResultStateRef 
; LToStateEstRef := NpsResultStateRef 
          END (* IF *)
          
        ELSE (* Advance *) 
          IF LFromStateEstRef . PtsAdvanceStateRef # NIL 
          THEN 
            NpsResultStateRef := LFromStateEstRef . PtsAdvanceStateRef 
          ; NpsResultStateEstRef := LFromStateEstRef . PtsAdvanceStateRef 
            (* Note: end of file state is a fixed point of Advance *) 
; LToStateEstRef := NpsResultStateRef 
          ELSE (* Must actually compute the advance successor *) 
            NpsResultStateEstRef 
              := RefToNewCopyOfParseTravStateEst ( LFromStateEstRef )
; IF LFromStateEstRef . PtsSeqNo = SeqNoToStop
  THEN
    VAR I := 0
  ; BEGIN
      I := 1
    END
  END
          ; NpsResultStateRef := NpsResultStateEstRef 
          ; NpsResultStateStreamRef := NIL (* Force RT error if used. *) 
          ; InitTokInfo ( NpsResultStateRef . PtsTokInfo )  
          ; NpsInitNps ( )
          ; NpsResultStateRef . PtsTokInfo . TiFullTempMarkRange
              := ParseHs . TempMarkRangeEmpty 
          ; NpsResultStateEstRef . PtsTokInfo . TiPatchTempMarkRange
              := ParseHs . TempMarkRangeEmpty 
          ; NpsWaitingTempMarkRange := ParseHs . TempMarkRangeEmpty 
          ; NpsNextTempMarkSs := NpsResultStateEstRef . PtseTokTempMarkSs 
          ; NpsAccumPreviouslyDeferredItem 
              ( LFromStateEstRef . PtseDeferredInfoRef )
(* Let's keep the deferred info around for now, to aid debugging. 
          ; LFromStateEstRef . PtseDeferredInfoRef := NIL (* GC bait. *)
*)
          ; NpsResultStateEstRef . PtsSeqNo := ParseInfo . PiSeqNo 
          ; INC ( ParseInfo . PiSeqNo ) 
          ; NpsStateMachine ( ) 
          ; NpsResultStateEstRef . PtseTokTempMarkSs := NpsNextTempMarkSs 
          ; LFromStateEstRef . PtsAdvanceStateRef := NpsResultStateRef 
; LToStateEstRef := NpsResultStateRef 
          END (* IF *) 
        END (* IF *)
      ; WITH WTokInfo = NpsResultStateRef . PtsTokInfo
        DO 
          IF NOT ParseHs . RangeIsEmpty ( WTokInfo . TiPatchTempMarkRange ) 
          THEN
             Assert 
               ( NOT ParseHs . RangeIsEmpty ( WTokInfo . TiFullTempMarkRange )
                 AND WTokInfo . TiFullTempMarkRange . From  
                 <= WTokInfo . TiPatchTempMarkRange . From 
                 AND WTokInfo . TiPatchTempMarkRange . To  
                     <= WTokInfo . TiFullTempMarkRange . To
               , AFT . A_NpsParseFromEst_Patch_temp_mark_range_not_a_subrange
               )
          END (* IF *)
        END (* WITH *) 
      ; LSeEstRef := LFromStateEstRef . PtseStackEstRef 
      ; LPopCt := NpsEstPopCt 
      ; WHILE LPopCt > 0 
        DO
          LSeEstRef . SeEstAdvanceState := NpsResultStateRef 
        ; LSeEstRef := LSeEstRef . SeEstLink 
        ; DEC ( LPopCt ) 
        END (* WHILE *) 
; EVAL CheckStacks ( NpsResultStateEstRef ) 
; EVAL Fmt . Int ( NpsSliceFrom ) 
      END NpsParseFromEst 

  ; PROCEDURE NpsParseFromFile ( ) 
    RAISES { Thread . Alerted , Backout } 

    = <* FATAL Rd . Failure *> 
      VAR LConsumedCt : Strings . StringSsTyp   
    ; VAR LTotalConsumedCt : Strings . StringSsTyp 
    ; VAR LLeadingBlankCt : LbeStd . LimitedCharNoTyp 

(* This is redundant, just to make finding things easier in a debugger. *) 
; VAR LToStateEstRef : ParseHs . ParseTravStateStreamRefTyp 

    ; BEGIN (* NpsParseFromFile *) 
        Assert 
          ( SuccKind # SuccKindTyp . SuccKindDescend 
          , AFT . A_NpsParseFromFileDescend 
          ) 
      ; IF FromStateRef . PtsAdvanceStateRef # NIL 
        THEN 
          NpsResultStateRef := FromStateRef . PtsAdvanceStateRef 
; LToStateEstRef := NpsResultStateRef 
        ELSE 
          NpsResultStateStreamRef 
            := RefToNewCopyOfParseTravStateStream ( FromStateRef ) 
        ; NpsResultStateStreamRef . PtsSeqNo := ParseInfo . PiSeqNo 
        ; INC ( ParseInfo . PiSeqNo ) 
        ; NpsResultStateRef := NpsResultStateStreamRef 
; LToStateEstRef := NpsResultStateRef 
        ; NpsResultStateEstRef := NIL (* Force RT errors if used. *) 
        ; InitTokInfo ( NpsResultStateRef . PtsTokInfo )  
        ; NpsInitNps ( ) 
        ; NpsAccumPreviouslyDeferredItem ( ParseInfo . PiDeferredInfoRef )
        ; LLeadingBlankCt := 0 
        ; LTotalConsumedCt := 1 (* Force no action on PtssPosRelTo *) 
        ; WHILE NpsDeliverState # DeliverStateTyp . DsDeliver 
          DO IF ParseInfo . PiEof 
             THEN 
               NpsDeliverStringToScanner 
                 ( StringEndOfImage 
                 , NpsResultStateStreamRef . PtssPosRelTo 
                    (* ^BolRelativeCmnts will be relative 
                        to this.  We really have no very 
                        good value to use, since we are parsing 
                        bottom up and there is no way to compute 
                        the usual indent position.  PtssPosRelTo 
                        is maintained as the beginning position of 
                        the first non-comment token on the last 
                        line.  Prettyprinting could undoubtedly 
                        mess things up a bit. *) 
                 , AreAllBlanks := FALSE 
                 , ConsumedCt := LConsumedCt (* Ignore *) 
                 ) 
             ELSIF Strings . Length ( ParseInfo . PiString ) > 0 
             THEN (* There is text for the current line. *) 
               NpsDeliverStringToScanner 
                 ( ParseInfo . PiString 
                 , NpsResultStateStreamRef . PtssPosRelTo 
                 , AreAllBlanks := FALSE 
                 , ConsumedCt := LConsumedCt 
                 ) 
            (* The following computation of PtssPosRelTo is quite fragile. 
               In principle, we want the location of the first nonblank 
               on the previous line.  To do this properly would require 
               keeping two positions stored in the state: one for the 
               previous line, to actually use, and one for the current line, 
               to copy to the previous when we pass a Nl. 
                 In fact, we take advantage of the fact that the value for the 
               previous line is used only in NpsAccumRescannedCmnt, and only 
               if there is a comment that is not at the beginning of the line 
               but is the first nonblank thing on the line.  This allows 
               setting PtssPosRelTo as soon as the first nonblank of the 
               current line is consumed, or immediately after reading a line, 
               if the first nonblank is at the beginning.  It also depends 
               on the fact that, once a line is read, this loop will not 
               exit until something is found, so we don't have to keep info 
               about the current line across calls to here.  Finally, 
               an all-blank line does not set PtssPosRelTo at all, leaving 
               it as before. *) 
             ; IF LTotalConsumedCt <= LLeadingBlankCt 
                  AND LLeadingBlankCt < LTotalConsumedCt + LConsumedCt 
               THEN 
                 NpsResultStateStreamRef . PtssPosRelTo := LLeadingBlankCt 
               END (* IF *) 
             ; Strings . LeftTruncateInPlace 
                 ( ParseInfo . PiString 
                 , Strings . Length ( ParseInfo . PiString ) - LConsumedCt 
                 ) 
             ; INC ( LTotalConsumedCt , LConsumedCt ) 
             ELSIF ParseInfo . PiNlIsWaiting 
             THEN (* Deliver a Nl *) 
               NpsDeliverStringToScanner 
                 ( StringNewLine 
                 , NpsResultStateStreamRef . PtssPosRelTo 
                 , AreAllBlanks := FALSE 
                 , ConsumedCt := LConsumedCt 
                 ) 
             ; IF LConsumedCt > 0 
               THEN
                 ParseInfo . PiNlIsWaiting := FALSE 
               END (* IF *) 
             ELSE (* Read another line. *) 
               TRY 
                 ParseInfo . PiString 
                   := Strings . FromText 
                        ( Rd . GetLine ( ParseInfo . PiFile ) ) 
               ; ParseInfo . PiNlIsWaiting := TRUE 
               ; INC ( ParseInfo . PiLineNo ) 
               ; LLeadingBlankCt 
                   := Strings . PosOf1stNonblank ( ParseInfo . PiString ) 
               ; LTotalConsumedCt := 0 
               EXCEPT Rd . EndOfFile , Rd . Failure 
                 => ParseInfo . PiEof := TRUE 
                 ; Rd . Close ( ParseInfo . PiFile ) 
                 ; ParseInfo . PiString := Strings . Empty ( ) 
               END (* EXCEPT *) 
             END (* IF *) 
          END (* WHILE *) 
        ; FromStateRef . PtsAdvanceStateRef := NpsResultStateRef 
        END (* IF *) 
; EVAL Fmt . Int ( NpsSliceFrom ) 
      END NpsParseFromFile 

  ; BEGIN (* NextParseTravState *) 
      CASE ParseInfo . PiParseKind 
      (* Parsing character input *) 
      OF LbeStd . ParseKindTyp . ParseKindFile 
      => NpsParseFromFile ( ) 
      | LbeStd . ParseKindTyp . ParseKindKbd 
      => (* Not implemented. *) 
         (* Reparsing an Est *) 
      | LbeStd . ParseKindTyp . ParseKindTrav 
      => NpsParseFromEst ( ) 
      END (* CASE *) 
    ; RETURN NpsResultStateRef 
    END NextParseTravState 

; CONST EstChildKindSetParseWholeImage 
    = EstHs . EstChildKindSetTyp 
        { EstHs . EstChildKindEstChild 
        , EstHs . EstChildKindFirstOfGroup 
        , EstHs . EstChildKindContainsSyntMod 
          (* ^This is to force descent into top level node. Even if 
              it contains no syntactic mods, in the current system,
              its Fs tree will have the BegOfImage and EndOfImage
              nodes, and we need at least the latter to get ParseTrv
              to terminate properly.  (Note: the EndOfImage can have
              leading mods, too.) *) 
(* TODO: Rework the use of Fs trees containing FsEndOfImage so any
         subtree, not just the start symbol, can be an autonomous
         Est. *) 
        } 

; PROCEDURE InitialStackElemEst ( EstRef : LbeStd . EstRootTyp ) 
  : ParseHs . StackElemEstTyp 
  RAISES { Backout } 
  (* Return a dummy StackElemEstTyp node to go on the bottom of the Est
     stack.  It has no parent EstNode, but its current child node is
     the parameter EstRef.
  *) 

  = VAR LResult : ParseHs . StackElemEstTyp 

  ; BEGIN (* InitialStackElemEst *) 
      LResult := NEW ( ParseHs . StackElemEstTyp ) 
    ; TravUtil . InitEstTravInfo ( LResult . SeEstTravInfo , NIL ) 
    ; LResult . SeEstTravInfo . EtiChildNo := 0 
    ; LResult . SeEstTravInfo . EtiChildRelNodeNo := 0 
    ; LResult . SeEstTravInfo . EtiChildLeafElem . LeChildRef := EstRef 
    ; LResult . SeEstTravInfo . EtiChildLeafElem . LeKindSet
        := EstChildKindSetParseWholeImage 
           + ( EstHs . EstChildKindSetMustReparse 
               * EstUtil . EstChildKindSet ( EstRef ) 
             ) 
    ; LResult . SeEstTravInfo . EtiChildFmtNo := EstHs . FmtNoEstTop 
    ; LResult . SeEstIndentPos1 := Options . InitialIndentPos 
    ; LResult . SeEstIndentPosN := Options . InitialIndentPos 
    ; LResult . SeEstIsFirstLine := TRUE  
    ; LResult . SeEstIndentPos := Options . InitialIndentPos 
    ; LResult . SeEstWholeSliceToChildNo := 0 
    ; LResult . SeEstWholeSliceRMEstChildNo := 0 
    ; LResult . SeEstRootFsNodeRef := LangUtil . TopFsNodeRef ( ) 
    ; LResult . SeEstIsSublist := FALSE 
    ; LResult . SeEstLink := NIL  
    ; RETURN LResult 
    END InitialStackElemEst 

; VAR GStateCt : CARDINAL := 0 

(* VISIBLE *) 
; PROCEDURE InitParseEst 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp ; EstRef : LbeStd . EstRootTyp ) 
    : ParseHs . ParseTravStateRefTyp 
  RAISES { Backout } 

  (* Get an initial parse traversal state for a tree. *) 

  = VAR LStateRef : ParseHs . ParseTravStateEstRefTyp 
  ; VAR LStackElemFsNodeRef : ParseHs . StackElemFsTyp
  ; VAR LStackElemEstNodeRef : ParseHs . StackElemEstTyp 

  ; BEGIN (* InitParseEst *) 
      Assert ( EstRef # NIL , AFT . A_InitParseEstReparseNILEst )

(* ; TreeBrowse.Browse (EstRef,ParseInfo.PiLang,"In InitParseEst" ) *)

    ; ParseInfo . PiParseKind := LbeStd . ParseKindTyp . ParseKindTrav
    ; ParseInfo . PiTravTempMarkListRef
        := ParseHs . CopyOfTempMarkList ( ParseInfo . PiOrigTempMarkListRef )
    ; ParseInfo . PiDeferredInfoRef := NIL 
    ; ParseInfo . PiLineCtIncr := 0 
    ; ParseInfo . PiSeqNo := 1 
    ; ParseInfo . PiLineNo := 0 
    ; LStateRef := NEW ( ParseHs . ParseTravStateEstRefTyp ) 
    ; LStateRef . PtsSeqNo := 0 
    ; InitTokInfo ( LStateRef . PtsTokInfo )
    ; LStateRef . PtsTokInfo . TiFullTempMarkRange
        := ParseHs . TempMarkRangeEmpty
    ; LStateRef . PtsTokInfo . TiPatchTempMarkRange
        := ParseHs . TempMarkRangeEmpty
    ; LStateRef . PtsAdvanceStateRef := NIL 
    ; LStateRef . PtseDescendStateRef := NIL 
    ; InitScanInfo ( LStateRef . PtsScanInfo ) 
    ; LStateRef . PtsPrevTokAfter := LbeStd . Tok__Null 
    ; LStateRef . PtsePrevTokBefore := LbeStd . Tok__Null 
    ; LStateRef . PtseModTextIsToLeftOnLine := FALSE 
    ; LStateRef . PtseTokTempMarkSs := 0 
    ; LStateRef . PtseStateKind 
        := ParseHs . ParseTravStateKindTyp . PtsKindNewEst
    ; LStateRef . PtseDeferredInfoRef := NIL  
    ; LStackElemEstNodeRef := InitialStackElemEst ( EstRef ) 
    ; LStateRef . PtseStackEstRef := LStackElemEstNodeRef  

    (* Defensive initializations: *) 
    ; LStateRef . PtseStringFromPos := 0 
    ; LStateRef . PtseRescanToPos := 0 
    ; LStateRef . PtseStringRef := NIL 

    (* Push Fs stack element *) 
    ; LStackElemFsNodeRef := NEW ( ParseHs . StackElemFsTyp ) 
    ; LStackElemFsNodeRef . SeFsNodeRef
        := LStackElemEstNodeRef . SeEstRootFsNodeRef
    ; LStackElemFsNodeRef . SeFsSelfFsChildNo := 0 
    ; LStackElemFsNodeRef . SeFsFmtKind
        := LangUtil . FmtKindTyp . FmtKindVert
(* TODO: Take topmost FmtKind from the FsNode, many places. *)
    ; LStackElemFsNodeRef . SeFsPredicate := FALSE
    ; LStackElemFsNodeRef . SeFsSeEstRef := LStackElemEstNodeRef 
    ; LStateRef . PtseStackFsRef := LStackElemFsNodeRef 
; GStateCt := 0
    ; ParseInfo . PiInitTravStateRef := LStateRef 
    ; RETURN LStateRef 
    END InitParseEst 

(* VISIBLE *) 
; PROCEDURE InitParseFile 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp ; File : Rd . T ) 
    : ParseHs . ParseTravStateRefTyp 

  (* Get an initial parse traversal state for a file. *) 

  = VAR LParseTravStateStreamRef : ParseHs . ParseTravStateStreamRefTyp 

  ; BEGIN (* InitParseFile *) 
      ParseInfo . PiParseKind := LbeStd . ParseKindTyp . ParseKindFile 
    ; ParseInfo . PiDeferredInfoRef := NIL 
    ; ParseInfo . PiFile := File 
    ; ParseInfo . PiEof := FALSE 
    ; ParseInfo . PiNlIsWaiting := FALSE 
    ; ParseInfo . PiString := Strings . Empty ( ) 
    ; ParseInfo . PiLineCtIncr := 0 
    ; ParseInfo . PiSeqNo := 1 
    ; ParseInfo . PiLineNo := 0 
    ; LParseTravStateStreamRef 
        := NEW ( ParseHs . ParseTravStateStreamRefTyp ) 
    ; LParseTravStateStreamRef . PtsSeqNo := 0 
    ; InitTokInfo ( LParseTravStateStreamRef . PtsTokInfo ) 
    ; LParseTravStateStreamRef . PtsTokInfo . TiFullTempMarkRange
        := ParseHs . TempMarkRangeEmpty
    ; LParseTravStateStreamRef . PtsTokInfo . TiPatchTempMarkRange
        := ParseHs . TempMarkRangeEmpty
    ; LParseTravStateStreamRef . PtsAdvanceStateRef := NIL 
    ; LParseTravStateStreamRef . PtssPosRelTo := 0 
    ; LParseTravStateStreamRef . PtssTokSeqNo := 0 
    ; InitScanInfo ( LParseTravStateStreamRef . PtsScanInfo ) 
    ; RETURN LParseTravStateStreamRef 
    END InitParseFile 

(* VISIBLE *) 
; PROCEDURE InitParseKbd 
    ( <* UNUSED *> VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; <* UNUSED *> PosRelTo : LbeStd . LimitedCharNoTyp 
    ) 
    : ParseHs . ParseTravStateRefTyp 

  (* Get an initial parse traversal state to parse from keyboard. *) 

  = <* UNUSED *> VAR LParseTravStateStreamRef 
        : ParseHs . ParseTravStateStreamRefTyp 

  ; BEGIN (* InitParseKbd *) 
(* 
      ParseInfo . PiParseKind := LbeStd . ParseKindKbd 
    ; ParseInfo . PiDeferredInfoRef := NIL 
    ; ParseInfo . PiFile := NIL  
    ; ParseInfo . PiEof := FALSE 
    ; ParseInfo . PiNlIsWaiting := FALSE 
    ; ParseInfo . PiString := Strings . Empty ( ) 
    ; ParseInfo . PiLineCtIncr := 0 
    ; ParseInfo . PiLineNo := 0 
    ; LParseTravStateStreamRef 
        := NEW ( ParseHs.ParseTravStateStreamRef ) 
    ; WITH LParseTravState 
      DO 
        InitTokInfo ( LParseTravStateStreamRef.PtsTokInfo ) 
      ; LParseTravStateStreamRef.PtsAdvanceStateRef := NIL 
      ; LParseTravStateStreamRef.PtssPosRelTo := PosRelTo 
      END (* WITH *) 
    ; InitScanInfo ( LParseTrvStateStreamRef . PtsScanInfo ) 
    ; RETURN  LParseTravStateStreamRef 
 
*)  
      RETURN NIL 
    END InitParseKbd 

; BEGIN (* ParseTrv *) 
  END ParseTrv 
. 
