
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* TextEdit carries out text editing the Est and text painting. *) 

MODULE TextEdit 

; IMPORT Fmt 
; IMPORT Text 
; IMPORT Thread 

; IMPORT Ascii 
; IMPORT AssertDevel 
; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure  
; FROM Failures IMPORT Backout , Ignore 
; IMPORT Display 
; IMPORT EditWindow 
; IMPORT Errors 
; IMPORT EstHs 
; IMPORT LbeStd 
; IMPORT LineMarks 
; IMPORT Marks 
; IMPORT MergeTxt 
; IMPORT MessageCodes 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT PortTypes 
; IMPORT Strings 
; IMPORT SyntEdit 
; IMPORT TravUtil 
; IMPORT TreeBrowse 

; TYPE AFT = MessageCodes . T 
; TYPE MarkKindTyp = Marks . MarkKindTyp 

; PROCEDURE PaintTempEditedLineInAllWindows 
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ; TempEditRef : PaintHs . TempEditRefTyp 
    ) 
  RAISES { Backout } 

  = VAR LWindowRef : PaintHs . WindowRefTyp 

  ; BEGIN (* PaintTempEditedLineInAllWindows *) 
      LWindowRef := ImageTrans . ItWindowList 
    ; WHILE LWindowRef # NIL 
      DO IF LWindowRef . WrWindowNo IN LinesRef . LrVisibleIn 
         THEN 
           WITH 
             WLoopMarkRec 
             = LWindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ]
           DO 
             Display . PaintTempEditedLine 
               ( WindowRef := LWindowRef 
               , LineNoInWindow 
                   := Display . LineNoInWindow 
                        ( LWindowRef , TempEditRef . TeLinesRef )
                      + TempEditRef . TeLineNo 
               , TempEditRef := TempEditRef 
               , BolTokMark := LinesRef . LrBolTokMark  
               , LineNo := TempEditRef . TeLineNo 
               ) 
           ; EditWindow . SetCursorPosition 
               ( LWindowRef 
               , WLoopMarkRec . LmCharPos - LWindowRef . WrHorizScroll 
               , LWindowRef . WrCursorLineNoInWindow 
               ) 
           ; EditWindow . PaintCursorCoordinates ( LWindowRef ) 
           END (* WITH WLoopMarkRec *) 
         END (* IF *) 
      ; LWindowRef := LWindowRef . WrImageLink 
      END (* WHILE *) 
    END PaintTempEditedLineInAllWindows 

; PROCEDURE InitTempEditTextOnly 
    ( (* OUT *) TempEditRef : PaintHs . TempEditRefTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ) 
  RAISES { Backout } 

  = BEGIN 
      TempEditRef . TeEditedString 
        := Strings . Empty ( EventualLengthHint := Options . RightMargin ) 
    ; IF LinesRef # NIL 
         AND LinesRef . LrLineCt = 0 
      THEN 
        TRY 
          Strings . InsertBlanksInPlace 
            ( TempEditRef . TeEditedString 
            , PrefixLength := 0 
            , BlankCount := LinesRef . LrFromPos 
            ) 
        EXCEPT Strings . SsOutOfBounds
        => CantHappen 
             ( AFT . A_InitTempEditTextOnly_String_subscript_out_of_bounds ) 
        END (* TRY EXCEPT *) 
      ; Strings . AppendTextInPlace 
          ( TempEditRef . TeEditedString , LinesRef . LrLineText ) 
      ; Assert 
          ( Strings . Length ( TempEditRef . TeEditedString ) 
            = LinesRef . LrLineLen 
          , AFT . A_InitTempEditForTextBadLength 
          ) 
      ; TempEditRef . TeTextAttrArrayRef := LinesRef . LrTextAttrArrayRef 
      ; IF LinesRef . LrTextAttrArrayRef # NIL 
        THEN 
          TempEditRef . TeTextAttrActualSize 
            := NUMBER ( LinesRef . LrTextAttrArrayRef ^ ) 
        END (* IF *) 
      ; TempEditRef . TeLineErrArrayRef := LinesRef . LrLineErrArrayRef 
      ELSE 
        TempEditRef . TeTextAttrArrayRef := NIL 
      ; TempEditRef . TeTextAttrActualSize := 0  
      ; TempEditRef . TeLineErrArrayRef := NIL 
      END (* IF *) 
    END InitTempEditTextOnly  

; PROCEDURE InitTempEditForText 
    ( (* OUT *) TempEditRef : PaintHs . TempEditRefTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ; LineNo : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout } 

  = BEGIN (* InitTempEditForText *) 
      TempEditRef . TeLinesRef := LinesRef 
    ; TempEditRef . TeLineNo := LineNo 
    ; TempEditRef . TeDelFromPos := LbeStd . LimitedCharNoInfinity 
    ; TempEditRef . TeDelToPos := 0 
    ; TempEditRef . TeInsLen := 0 
    ; InitTempEditTextOnly ( TempEditRef , LinesRef ) 
    END InitTempEditForText 

; PROCEDURE RegenerateEditedLine 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; EstRoot : LbeStd . EstRootTyp 
    ; StartBolTokMark : Marks . TokMarkTyp 
    ; VAR RegeneratedString : Strings . StringTyp 
    ; VAR RegeneratedTextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
    ; VAR RegeneratedLineErrArrayRef : PaintHs . LineErrArrayRefTyp 
    ; VAR EndBolTokMark : Marks . TokMarkTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
(* TODO: Maybe inline this.  It was just a quick coding expedient. *) 

  = VAR LLineCt : LbeStd . LineNoTyp 
  ; VAR LAtEndOfImage : BOOLEAN 
  ; VAR LDidHitExistingMark : BOOLEAN 

  ; BEGIN (* RegenerateEditedLine *) 
      LineMarks . GetNextLine 
        ( Lang := ImageRef . ItPers . IpLang 
        , EstRef := EstRoot 
        , StartMark := StartBolTokMark 
        , ExistingMark := Marks . TokMarkNull 
        , (* VAR *) DidHitExistingMark:= LDidHitExistingMark (* Dead. *) 
        , (* VAR *) NewMark := EndBolTokMark 
        , (* VAR *) AtEndOfImage := LAtEndOfImage (* Dead *)  
        , (* VAR *) LineText := RegeneratedString 
        , (* VAR *) BlankLineCt := LLineCt (* Dead *) 
        , (* VAR *) TextAttrArrayRef := RegeneratedTextAttrArrayRef  
        , (* VAR *) LineErrArrayRef := RegeneratedLineErrArrayRef  
        ) 
    END RegenerateEditedLine 

; PROCEDURE VerifyEditedLine 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; EstRoot : LbeStd . EstRootTyp 
    ; StartBolTokMark : Marks . TokMarkTyp 
    ; ExpectedLineCt : LbeStd . LineNoTyp 
    ; ExpectedAtEndOfImage : BOOLEAN 
    ; READONLY ExpectedString : Strings . StringTyp 
    ; VAR RegeneratedString : Strings . StringTyp 
    ; VAR RegeneratedTextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
    ; VAR RegeneratedLineErrArrayRef : PaintHs . LineErrArrayRefTyp 
    ; VAR EndBolTokMark : Marks . TokMarkTyp 
    ; VAR FailureOccurred : BOOLEAN
          (* ^Which would have been ignored, if VerifyEditedLine returns
             without raising Backout.
          *)   
    ) 
  RAISES { Backout , Ignore , AssertionFailure , Thread . Alerted } 

  = VAR LLineCt : LbeStd . LineNoTyp 
  ; VAR LAtEndOfImage : BOOLEAN 
  ; VAR LDidHitExistingMark : BOOLEAN 

  ; BEGIN (* VerifyEditedLine *) 
      IF Assertions . Checking 
      THEN 
        LineMarks . GetNextLine 
          ( Lang := ImageRef . ItPers . IpLang 
          , EstRef := EstRoot 
          , StartMark := StartBolTokMark 
          , ExistingMark := Marks . TokMarkNull 
          , (* VAR *) DidHitExistingMark:= LDidHitExistingMark (* Dead. *) 
          , (* VAR *) NewMark := EndBolTokMark 
          , (* VAR *) AtEndOfImage := LAtEndOfImage 
          , (* VAR *) LineText := RegeneratedString 
          , (* VAR *) BlankLineCt := LLineCt 
          , (* VAR *) TextAttrArrayRef := RegeneratedTextAttrArrayRef  
          , (* VAR *) LineErrArrayRef := RegeneratedLineErrArrayRef  
          ) 
      ; Assert ( LLineCt = ExpectedLineCt , AFT . A_VerifyEditedLineBadLineCt ) 
      ; IF ExpectedLineCt = 0 
        THEN 
          IF NOT Strings . AreEqualButForTrailingBlanks 
                   ( ExpectedString , RegeneratedString ) 
          THEN 
            IF FailureOccurred (* Previously. *) 
            THEN Assertions . Message ( AFT . A_VerifyEditedLineStringMismatch ) 
            ELSE 
              FailureOccurred := TRUE 
            ; CantHappen ( AFT . A_VerifyEditedLineStringMismatch ) 
            END (* IF *) 
          END (* IF *) 
        ELSE 
          IF Strings . Length ( RegeneratedString ) # 0 
          THEN 
            IF FailureOccurred (* Previously. *) 
            THEN Assertions . Message ( AFT . A_VerifyEditedLineTextOnLine ) 
            ELSE 
              FailureOccurred := TRUE 
            ; CantHappen ( AFT . A_VerifyEditedLineTextOnLine ) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      ; IF FALSE AND LAtEndOfImage # ExpectedAtEndOfImage 
(* TODO:   ^ This assertion is not right, as we can be at EOI, but not
          know it yet, without another call on GetNextLine. 
*) 
        THEN 
          IF FailureOccurred (* Previously. *) 
          THEN Assertions . Message ( AFT . A_VerifyEditedLineEOIMismatch ) 
          ELSE 
            FailureOccurred := TRUE 
          ; CantHappen ( AFT . A_VerifyEditedLineEOIMismatch ) 
          END (* IF *) 
        END (* IF *) 
      ELSE 
        IF ExpectedLineCt = 0 
        THEN 
          RegeneratedString := Strings . Copy ( ExpectedString ) 
        ELSE 
          RegeneratedString := Strings . Empty ( ) 
        END (* IF *) 
      END (* IF *) 
    END VerifyEditedLine 

; PROCEDURE IsAncestorMark 
    ( LeftNodeNo : LbeStd . EstNodeNoTyp 
    ; LeftNodeCt : LbeStd . EstNodeNoTyp 
    ; RightNodeNo : LbeStd . EstNodeNoTyp 
    ) 
  : BOOLEAN 

  = BEGIN 
      RETURN LeftNodeNo < RightNodeNo 
             AND LeftNodeNo + LeftNodeCt > RightNodeNo  
                 (* >= RightNodeNo + RightNodeCt - 1 *) 
    END IsAncestorMark 

; PROCEDURE AdjustLinesRefsNodeNos 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; FirstLinesRefToAdjust : PaintHs . LinesRefTyp 
    ; MinNodeNoToAdjust : LbeStd . EstNodeNoTyp 
    ; Bias : LbeStd . EstNodeNoTyp 
    ) 
  RAISES { Backout } 

  = VAR LLinesHeader : PaintHs . LinesRefTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LPastFirstLinesRef : BOOLEAN 

  ; BEGIN (* AdjustLinesRefsNodeNos *) 
      LLinesHeader := ImageRef . ItPers . IpLineHeaderRef 
    ; IF LLinesHeader # NIL 
         AND LLinesHeader . LrRightLink # LLinesHeader 
      THEN 
        LLinesRef := LLinesHeader . LrRightLink 
      END (* IF *) 
    ; LPastFirstLinesRef := FALSE 
    ; LOOP
        IF LLinesRef = FirstLinesRefToAdjust 
        THEN LPastFirstLinesRef := TRUE 
        END (* IF *)  
      ; IF IsAncestorMark 
             ( LLinesRef . LrBolTokMark . EstNodeNo  
             , LLinesRef . LrBolTokMark . EstNodeCt 
             , MinNodeNoToAdjust 
             ) 
        THEN 
          INC ( LLinesRef . LrBolTokMark . EstNodeCt , Bias ) 
        END (* IF *)   
      ; IF LLinesRef . LrBolTokMark . EstNodeNo >= MinNodeNoToAdjust 
        THEN 
          Assert 
            ( LPastFirstLinesRef 
            , AFT . A_AdjustLinesRefsNodeNos_Adjusting_too_early 
            ) 
        ; INC ( LLinesRef . LrBolTokMark . EstNodeNo , Bias ) 
        END (* IF *)
      ; IF LLinesRef . LrRightLink = ImageRef . ItPers . IpLineHeaderRef 
        THEN 
          EXIT 
        ELSE 
          LLinesRef := LLinesRef . LrRightLink 
        END (* IF *) 
      END (* LOOP *) 
    END AdjustLinesRefsNodeNos 

; PROCEDURE UnadjustLinesRefsNodeNos 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; FirstLinesRefToAdjust : PaintHs . LinesRefTyp 
    ; MinNodeNoToAdjust : LbeStd . EstNodeNoTyp 
    ; Bias : LbeStd . EstNodeNoTyp 
    ) 

  = VAR LLinesHeader : PaintHs . LinesRefTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LPastFirstLinesRef : BOOLEAN 

  ; BEGIN (* UnadjustLinesRefsNodeNos *) 
      LLinesHeader := ImageRef . ItPers . IpLineHeaderRef 
    ; IF LLinesHeader # NIL 
         AND LLinesHeader . LrRightLink # LLinesHeader 
      THEN 
        LLinesRef := LLinesHeader . LrRightLink 
      END (* IF *) 
    ; LPastFirstLinesRef := FALSE 
    ; LOOP 
        IF LLinesRef = FirstLinesRefToAdjust 
        THEN LPastFirstLinesRef := TRUE 
        END (* IF *)  
      ; IF LLinesRef . LrBolTokMark . EstNodeNo - Bias  
           >= MinNodeNoToAdjust 
        THEN 
          DEC ( LLinesRef . LrBolTokMark . EstNodeNo , Bias ) 
        END (* IF *) 
      ; IF IsAncestorMark 
             ( LLinesRef . LrBolTokMark . EstNodeNo (* Already unadjusted. *) 
             , LLinesRef . LrBolTokMark . EstNodeCt 
             , MinNodeNoToAdjust  
             ) 
        THEN 
          DEC ( LLinesRef . LrBolTokMark . EstNodeCt , Bias ) 
        END (* IF *)   
      ; IF LLinesRef . LrRightLink = ImageRef . ItPers . IpLineHeaderRef 
        THEN 
          EXIT 
        ELSE 
          LLinesRef := LLinesRef . LrRightLink 
        END (* IF *) 
      END (* LOOP *) 
    END UnadjustLinesRefsNodeNos 

; PROCEDURE UnadjustLineMarksNodeNos 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; MinNodeNoToAdjust : LbeStd . EstNodeNoTyp 
    ; Bias : LbeStd . EstNodeNoTyp 
    ) 
  (* Boy, is this fragile.  It assumes that if Bias is negative,
     it will not have reduced any previously biased EstNodeNo field 
     enough to misidentify what needs to have its bias removed. *) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LMark : PaintHs . LineMarkMeatTyp 

  ; BEGIN (* UnadjustLineMarksNodeNos *) 
      LImagePers := ImageRef . ItPers 
    ; IF LImagePers . IpMarkHeader # NIL 
         AND LImagePers . IpMarkHeader . LmRightLink 
             # LImagePers . IpMarkHeader 
      THEN 
        LMark := LImagePers . IpMarkHeader . LmRightLink 
      ; LOOP 
          IF LMark . LmLinesRef # NIL 
          THEN LMark . LmTokMark := LMark . LmLinesRef . LrBolTokMark 
          ELSE 
            IF IsAncestorMark 
                 ( LMark . LmTokMark . EstNodeNo  
                 , LMark . LmTokMark . EstNodeCt 
                 , MinNodeNoToAdjust  
                 ) 
            THEN 
              DEC ( LMark . LmTokMark . EstNodeCt , Bias ) 
            END (* IF *)   
          ; IF LMark . LmTokMark . EstNodeNo >= MinNodeNoToAdjust + Bias  
            THEN 
              DEC ( LMark . LmTokMark . EstNodeNo , Bias ) 
            END (* IF *) 
          END (* IF *) 
        ; IF LMark . LmRightLink = LImagePers . IpMarkHeader 
          THEN (* End of list. *) 
            EXIT 
          ELSE 
            LMark := LMark . LmRightLink 
          END (* IF *) 
        END (* LOOP *) 
      END (* IF *) 
    END UnadjustLineMarksNodeNos 

; PROCEDURE BruteForceRepairLineMarks 
    ( ImageRef : PaintHs . ImageTransientTyp ) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LMark : PaintHs . LineMarkMeatTyp 

  ; BEGIN (* BruteForceRepairLineMarks *) 
      LImagePers := ImageRef . ItPers 
    ; IF LImagePers . IpMarkHeader # NIL 
         AND LImagePers . IpMarkHeader . LmRightLink 
             # LImagePers . IpMarkHeader 
      THEN 
        LMark := LImagePers . IpMarkHeader . LmRightLink 
      ; LOOP 
          IF LMark . LmLinesRef # NIL 
             AND ( NOT Marks . Equal 
                         ( LMark . LmTokMark 
                         , LMark . LmLinesRef . LrBolTokMark 
                        ) 
                   OR LMark . LmTokMark . EstNodeCt 
                      # LMark . LmLinesRef . LrBolTokMark . EstNodeCt 
                      (* Marks . Equal doesn't check EstNodeCt field, because
                         it is redundant, if properly set, and if not, due
                         to incomplete updating, we want other compares to
                         succeed anyway.
                      *)
                 )  
          THEN (* This can happen if BruteForceVerifyAllLinesRefs repaired
                  the LrBolTokMark after AdjustLineMarksNodeNos was done.
               *) 
            Assertions . MessageText 
              ( "Repairing Marklist Mark " 
                & Marks . MarkImage ( LMark . LmTokMark )  
                & " to " 
                & Marks . MarkImage ( LMark . LmLinesRef . LrBolTokMark  )  
              ) 
          ; LMark . LmTokMark := LMark . LmLinesRef . LrBolTokMark 
          END (* IF *) 
        ; IF LMark . LmRightLink = LImagePers . IpMarkHeader 
          THEN (* End of list. *) 
            EXIT 
          ELSE 
            LMark := LMark . LmRightLink 
          END (* IF *) 
        END (* LOOP *) 
      END (* IF *) 
    END BruteForceRepairLineMarks 

; PROCEDURE LineString 
    ( LinesRef : PaintHs . LinesRefMeatTyp ) : Strings . StringTyp 

  = BEGIN (* LineString *) 
      IF LinesRef . LrLineText = NIL 
      THEN RETURN Strings . FromText ( "" )  
      ELSE 
	RETURN 
	  Strings . FromText 
	    ( Fmt . Pad ( "" , LinesRef . LrFromPos ) 
              & LinesRef . LrLineText 
            ) 
      END (* IF *) 
    END LineString 

(* EXPORTED: *)
; PROCEDURE BruteForceVerifyAllLinesRefs 
    ( ImageRef : PaintHs . ImageTransientTyp ; RepairIsOK : BOOLEAN ) 
  RAISES { Backout , Thread . Alerted } 
  (* Absent header is OK.
     Empty list is OK.
  *) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LHeader : PaintHs . LinesRefHeaderTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LBegOfImageMark : Marks . TokMarkTyp 
  ; VAR LTextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
  ; VAR LLineErrArrayRef : PaintHs . LineErrArrayRefTyp 
  ; VAR LRegeneratedString : Strings . StringTyp 
  ; VAR LPrevMark : Marks . TokMarkTyp 
  ; VAR LNextMark : Marks . TokMarkTyp 
  ; VAR LFailureOccurred : BOOLEAN 

  ; BEGIN (* BruteForceVerifyAllLinesRefs *) 
      IF ImageRef # NIL 
      THEN
        TRY (* EXCEPT *) 
          LImagePers := ImageRef . ItPers 
        ; IF LImagePers # NIL 
          THEN 
            LFailureOccurred := FALSE 
          ; LHeader := LImagePers . IpLineHeaderRef 
          ; IF LHeader # NIL 
            THEN 
              TYPECASE LHeader . LrRightLink 
              OF NULL 
              => CantHappen 
                   ( AFT . A_BruteForceVerifyAllLinesRefs_NIL_header_link )  
              | PaintHs . LinesRefMeatTyp ( TFirstLinesRef ) 
              => LLinesRef := TFirstLinesRef 
              ; IF NOT LHeader . LrGapAfter 
                   AND NOT TFirstLinesRef . LrGapAfter 
                THEN (* Verify this is the first line of the file. *) 
                  LineMarks . GetLMBegOfImage 
                    ( LImagePers . IpLang 
                    , LImagePers . IpEstRoot 
                    , (* VAR *) LBegOfImageMark 
                    ) 
                ; VerifyEditedLine 
                    ( ImageRef 
                    , EstRoot := LImagePers . IpEstRoot 
                    , StartBolTokMark := LBegOfImageMark 
                    , ExpectedLineCt := TFirstLinesRef . LrLineCt 
                    , ExpectedAtEndOfImage 
                        := TFirstLinesRef . LrRightLink = LHeader 
                    , ExpectedString := LineString ( TFirstLinesRef ) 
                    , (* VAR *) RegeneratedString := LRegeneratedString 
                    , (* VAR *) RegeneratedTextAttrArrayRef 
                        := LTextAttrArrayRef 
                    , (* VAR *) RegeneratedLineErrArrayRef := LLineErrArrayRef 
                    , (* VAR *) EndBolTokMark := LNextMark 
                    , (* IN OUT *) FailureOccurred := LFailureOccurred 
                    ) 
                END (* IF *) 
              ; LOOP 
                  LPrevMark := LLinesRef . LrBolTokMark 
                ; IF NOT LLinesRef . LrGapAfter 
                  THEN 
                    VerifyEditedLine 
                      ( ImageRef 
                      , EstRoot := LImagePers . IpEstRoot 
                      , StartBolTokMark := LLinesRef . LrBolTokMark 
                      , ExpectedLineCt := LLinesRef . LrLineCt 
                      , ExpectedAtEndOfImage 
                          := LLinesRef . LrRightLink 
                             = LHeader 
                      , ExpectedString := LineString ( LLinesRef ) 
                      , (* VAR *) RegeneratedString := LRegeneratedString 
                      , (* VAR *) RegeneratedTextAttrArrayRef 
                           := LTextAttrArrayRef 
                      , (* VAR *) RegeneratedLineErrArrayRef 
                           := LLineErrArrayRef 
                      , (* VAR *) EndBolTokMark := LNextMark 
                      , (* IN OUT *) FailureOccurred := LFailureOccurred 
                      ) 
                  ; TYPECASE LLinesRef . LrRightLink 
                    OF PaintHs . LinesRefMeatTyp ( TRightLinesRef ) 
                    => LLinesRef := TRightLinesRef 
                    ; IF NOT Marks . Equal 
                               ( LLinesRef . LrBolTokMark , LNextMark ) 
                         OR LLinesRef . LrBolTokMark . EstNodeCt 
                            # LNextMark . EstNodeCt 
                         (* Marks . Equal doesn't check EstNodeCt field, because
                            it is redundant, if properly set, and if not, due
                            to incomplete updating, we want other compares to
                            succeed anyway.
                         *) 
                      THEN
                        IF RepairIsOK 
                        THEN 
                          Assertions . MessageText 
                            ( "Repairing LinesRef Mark " 
                              & Marks . MarkImage ( LLinesRef . LrBolTokMark )  
                              & " to " & Marks . MarkImage ( LNextMark )  
                            ) 
                        ; LLinesRef . LrBolTokMark := LNextMark 
                        ELSE 
                          Assertions . CantHappenText 
                            ( "Incorrect LinesRef Mark " 
                              & Marks . MarkImage ( LLinesRef . LrBolTokMark )  
                              & " should be " & Marks . MarkImage ( LNextMark )  
                            ) 
                        END (* IF *) 
                      END 
                    ELSE 
                      EXIT 
                    END (* TYPECASE *) 
                  ELSE 
                    TYPECASE LLinesRef . LrRightLink 
                    OF PaintHs . LinesRefMeatTyp ( TRightLinesRef ) 
                    => LLinesRef := TRightLinesRef 
                    ELSE 
                      EXIT 
                    END (* TYPECASE *) 
                  END (* IF *) 
                ; TRY 
                    Assert 
                      ( Marks . Compare ( LPrevMark , LLinesRef . LrBolTokMark )  
                        = - 1 
                      , AFT . A_BruteForceVerifyAllLinesRefs_OutOfOrderMark 
                      ) 
                  EXCEPT Marks . Unordered 
                  => CantHappen 
                       ( AFT . A_BruteForceVerifyAllLinesRefs_Unordered_marks ) 
                  END (* TRY EXCEPT *) 
                END (* LOOP *)
              ELSE (* No Meat nodes. *) 
                Assert 
                  ( LHeader . LrLeftLink = LHeader 
                    AND LHeader . LrRightLink = LHeader 
                  , AFT . A_BruteForceVerifyAllLinesRefs_Bad_empty_list
                  ) 
              END (* TYPECASE *) 
            END (* IF *) 
          END (* IF *)
        EXCEPT
        | Ignore =>
        | AssertionFailure => (* Should we do some repair here? *)  
        END (* EXCEPT *) 
      END (* IF *) 
    END BruteForceVerifyAllLinesRefs 

; PROCEDURE SkipBlankLinesToLeft 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; VAR LinesRefMeat : PaintHs . LinesRefMeatTyp 
    ; BlankLineCt : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = BEGIN 
      LOOP 
        IF BlankLineCt <= 0 
        THEN EXIT 
        ELSE 
          Display . SecurePred ( ImageRef , LinesRefMeat ) 
        ; TYPECASE LinesRefMeat . LrLeftLink  
          OF PaintHs . LinesRefMeatTyp ( TNextLinesRefMeat ) 
          => IF TNextLinesRefMeat . LrLineCt = 0 
             THEN (* Not a blank line mod. *) 
               EXIT 
             ELSE
               DEC ( BlankLineCt , TNextLinesRefMeat . LrLineCt ) 
             ; LinesRefMeat := TNextLinesRefMeat 
             END (* IF *)  
          ELSE (* List header. *) 
            EXIT 
          END (* TYPECASE *) 
        END (* IF *) 
      END (* LOOP *)  
    ; Assert 
        ( BlankLineCt = 0 
        , AFT . A_SkipBlankLinesToLeft_TooFewBlankLineMods 
        ) 
    END SkipBlankLinesToLeft 

; PROCEDURE SkipBlankLinesToRight 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; VAR LinesRefMeat : PaintHs . LinesRefMeatTyp 
    ; BlankLineCt : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = BEGIN 
      LOOP 
        IF BlankLineCt <= 0 
        THEN EXIT 
        ELSE 
          Display . SecureSucc ( ImageRef , LinesRefMeat ) 
        ; TYPECASE LinesRefMeat . LrRightLink  
          OF PaintHs . LinesRefMeatTyp ( TNextLinesRefMeat ) 
          => IF TNextLinesRefMeat . LrLineCt = 0 
             THEN (* Not a blank line mod. *) 
               EXIT 
             ELSE
               DEC ( BlankLineCt , TNextLinesRefMeat . LrLineCt ) 
             ; LinesRefMeat := TNextLinesRefMeat 
             END (* IF *)  
          ELSE (* List header. *) 
            EXIT 
          END (* TYPECASE *) 
        END (* IF *) 
      END (* LOOP *)  
    ; Assert 
        ( BlankLineCt = 0 
        , AFT . A_SkipBlankLinesToRight_TooFewBlankLineMods 
        ) 
    END SkipBlankLinesToRight 

; PROCEDURE CopyOfTempEditRef ( OldValue : PaintHs . TempEditRefTyp ) 
  : PaintHs . TempEditRefTyp

  = VAR LNewValue : PaintHs . TempEditRefTyp 

  ; BEGIN 
      LNewValue := NEW ( PaintHs . TempEditRefTyp ) 
    ; LNewValue . TeLinesRef := OldValue . TeLinesRef  
    ; LNewValue . TeDelFromPos := OldValue . TeDelFromPos  
    ; LNewValue . TeDelToPos := OldValue . TeDelToPos 
    ; LNewValue . TeInsLen := OldValue . TeInsLen 
    ; LNewValue . TeLineNo := OldValue . TeLineNo  
    ; Strings . VerbatimCopy 
        ( OldValue . TeEditedString 
        , (* VAR *) LNewValue . TeEditedString 
        ) 
    ; IF OldValue . TeTextAttrArrayRef = NIL 
      THEN 
        LNewValue . TeTextAttrArrayRef := NIL 
      ELSE 
        LNewValue . TeTextAttrArrayRef 
          := NEW ( PaintHs . TextAttrArrayRefTyp 
                 , NUMBER ( OldValue . TeTextAttrArrayRef ^ ) 
                 )   
      ; LNewValue . TeTextAttrArrayRef ^ 
          := OldValue . TeTextAttrArrayRef ^  
      END (* IF *) 
    ; LNewValue . TeTextAttrActualSize := OldValue . TeTextAttrActualSize  
    (* This might be unnecessary: *) 
    ;  IF OldValue . TeLineErrArrayRef = NIL 
       THEN 
         LNewValue . TeLineErrArrayRef := NIL 
       ELSE 
         LNewValue . TeLineErrArrayRef 
           := NEW ( PaintHs . LineErrArrayRefTyp 
                  , NUMBER ( OldValue . TeLineErrArrayRef ^ ) 
                  )  
      ; LNewValue . TeLineErrArrayRef ^ := OldValue . TeLineErrArrayRef ^ 
      END (* IF *) 
    ; RETURN LNewValue 
    END CopyOfTempEditRef 

; TYPE MarkIdTyp 
  = { MiLeadingLine , MiFirstText , MiSecondText , MiTrailingLine } 

; TYPE LinesRefArrayTyp = ARRAY MarkIdTyp OF PaintHs . LinesRefMeatTyp 

; VAR (* CONST *) LinesRefArrayNull : LinesRefArrayTyp 

; PROCEDURE InnerFlushTempEdit 
    ( ImageRef : PaintHs . ImageTransientTyp  
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ; TempEditRef : PaintHs . TempEditRefTyp 
    ; InsNlPos : LbeStd . LimitedCharNoTyp 
        := LbeStd . LimitedCharNoInfinity 
      (* ^Position within InsText before which a new line goes. 
          LbeStd . LimitedCharNoInfinity if no new line at all. *) 
    ; NlIndentPos : LbeStd . LimitedCharNoTyp 
      := LbeStd . LimitedCharNoInfinity 
      (* ^IfInsNlPos # LbeStd . LimitedCharNoInfinity, this the amount 
          of indentation of the new text line, after the inserted Nl. *) 
    ; DelNlShift : LbeStd . LimitedCharNoTyp 
      := LbeStd . LimitedCharNoInfinity 
      (* ^When LbeStd . LimitedCharNoInfinity, no new line is deleted. 
          Otherwise, the new line at the end of the line 
          starting at BolTokMark is deleted, and DelNlShift is 
          the amount to add to character positions on the 
          formerly following line to get it appended to the 
          first line.  Also, NOT LinesRef . LrRightLink . LrGapAfter.  
          InsNlPos and DelNlShift cannot both be unequal to 
          LbeStd . LimitedCharNoInfinity. 
      *) 
    )
  RAISES { Backout , Thread . Alerted }  
  (* Call this only if it is known that a MergeTextEdit is needed. 
     This could happen if in state TeStateText, or if some line 
     splitting or merging is needed. *) 

  (* Repaint kinds.  Values of this type can be different for 
     each window. *) 

  = TYPE RepaintKindTyp 
      = { RepaintKindNoShift 
          (* ^No Nl is either inserted or deleted.  Although the text portion
              of this line will be unchanged by the flush, the display attributes
              may change, so it still needs to be repainted. *) 
        , RepaintKindPseudoShift 
          (* ^The top line visible in the window is appended to the 
              line just above the window. The new joined line will 
              be repainted in the top line of the window. 
              No shifting of other lines is needed. *) 
        , RepaintKindShiftUp 
          (* ^A visible line has had its successor appended to it. 
              The original line must be repainted with the composite 
              line, and all lines starting with the Second successor of 
              the original line must be moved up one line in the window. 
              This means an additional line which was below the 
              window will have to have a line rec created.  The first 
              successor of the original line need not be visible, in 
              which case the set of lines to shift up is empty. *) 
        , RepaintKindShiftDown 
          (* ^The original line is split.  The two new lines must be 
              painted and the successors of the original line must be 
              shifted down one in the window.  A LinesRef at the 
              bottom will become invisible in this window and may 
              need to be unlinked and abandoned. *) 
        } 
  ; TYPE RepaintKindSetTyp = SET OF RepaintKindTyp 

  ; VAR IfteImagePers : PaintHs . ImagePersistentTyp 
  ; VAR IfteLinesRefArray : LinesRefArrayTyp 
  ; VAR IfteNewEstRoot : EstHs . EstRefTyp 
  ; VAR IfteStartBolTokMark : Marks . TokMarkTyp 
  ; VAR IfteLinesAccountedFor : LbeStd . LineNoTyp 
  ; VAR IfteFirstOldActualLineCt : LbeStd . LineNoTyp 
  ; VAR IfteNewLinesCt : LbeStd . LineNoTyp 
  ; VAR IfteNewEndBolTokMark : Marks . TokMarkTyp 
  ; VAR IfteSecondOldLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR IfteOldFromLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR IfteOldThruLinesRef : PaintHs . LinesRefMeatTyp 
        (* IfteOld[From|Thru]LinesRef are the range of LinesRefs in the
           old list that are to be unlinked.  This could include blank
           lines before and/or after the line(s) edited, if MergeTxt
           pulled them into its revised set of LinesRefs.
           It could also be empty (denoted by IfteOldFromLinesRef = NIL)
           when editing "beyond" EOI. 
        *) 
  ; VAR IfteSuccLinesRef : PaintHs . LinesRefTyp 
        (* Points to the LinesRef following _all_ new LinesRefs. *) 
  ; VAR IfteSuccLinesRefMeat : PaintHs . LinesRefMeatTyp 
        (* Same as IfteSuccLinesRef, if it's a Meat record, else NIL *) 
  ; VAR IfteMaxTouchedNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR IfteNodeNoChange : LbeStd . EstNodeNoTyp 
  ; VAR IfteOldSuccBolTokMark : Marks . TokMarkTyp 
  ; VAR IfteExpectedString1 : Strings . StringTyp 
  ; VAR IfteExpectedString2 : Strings . StringTyp 
  ; VAR IfteExpectedBlankLinesBefore : LbeStd . LineNoTyp 
  ; VAR IfteExpectedBlankLinesAfter : LbeStd . LineNoTyp 
  ; VAR IfteActualBlankLinesBefore : LbeStd . LineNoTyp 
  ; VAR IfteActualBlankLinesAfter : LbeStd . LineNoTyp 
  ; VAR IfteOldEstRoot : LbeStd . EstRootTyp 

; PROCEDURE IfteAdjustLineMarks 
    ( MinNodeNoToAdjust : LbeStd . EstNodeNoTyp 
    ; Bias : LbeStd . EstNodeNoTyp 
    ) 
  RAISES { Backout } 

  = VAR LMark : PaintHs . LineMarkMeatTyp 
  ; VAR LNextOldLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LNextNewLinesSs : MarkIdTyp 
  ; VAR LNextNewLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LNextOldLineNo : LbeStd . EstNodeNoTyp 
  ; VAR LNextNewLineNo : LbeStd . EstNodeNoTyp 
    (* The actual lines denoted by IfteOldFromLinesRef through
       IfteOldThruLinesRef correspond almost one-to-one to those
       denoted by elements in IfteLinesRefArray.  Exceptions are when
       a Nl is inserted or deleted.  In each group, we count line
       numbers from zero.  LNextOldLineNo is the number of the first
       actual line denoted by LNextOldLinesRef.  LNextNewLineNo is the
       number of the first actual line denoted by
       IfteLinesRefArray[LNextNewLinesSs].  LNextOldLinesRef = NIL iff
       this list is exhausted (including initially empty.) *)
  ; VAR LOldToNewLineNoBias : [ - 1 .. 1 ] 
    (* Add this to LNextOldLineNo to get its equivalent line number in 
       IfteLinesRefArray, taking into account an inserted or deleted
       Nl that has been passed. *) 
  ; VAR LNextNewLineCt : LbeStd . EstNodeNoTyp 
  ; VAR LNewLineNo : LbeStd . EstNodeNoTyp 

  ; BEGIN (* IfteAdjustLineMarks *) 
      IF IfteImagePers . IpMarkHeader # NIL 
         AND IfteImagePers . IpMarkHeader . LmRightLink 
             # IfteImagePers . IpMarkHeader 
      THEN 
        LMark := IfteImagePers . IpMarkHeader . LmRightLink 
      ; LNextOldLinesRef := IfteOldFromLinesRef
      ; LNextOldLineNo := 0 
      ; LNextNewLinesSs := MarkIdTyp . MiLeadingLine  
      ; LNextNewLineNo := 0 
      ; LOldToNewLineNoBias := 0 
      ; LOOP (* Through the marks. *) 
          TRY 
            WHILE (* LNextOldLinesRef is behind. *) 
                  LNextOldLinesRef # NIL 
                  AND Marks . Compare 
                        ( LNextOldLinesRef . LrBolTokMark , LMark . LmTokMark ) 
                      = - 1 (* < *) 
            DO
              IF LNextOldLinesRef = LinesRef 
              THEN 
                IF InsNlPos # LbeStd . LimitedCharNoInfinity 
                THEN LOldToNewLineNoBias := 1 
                ELSIF DelNlShift # LbeStd . LimitedCharNoInfinity 
                THEN LOldToNewLineNoBias := - 1 
                END (* IF *) 
              END (* IF *) 
            ; INC ( LNextOldLineNo 
                  , Display . ActualNoOfLines 
                      ( LNextOldLinesRef . LrLineCt ) 
                  )
            ; IF LNextOldLinesRef = IfteOldThruLinesRef 
              THEN LNextOldLinesRef := NIL 
              ELSE LNextOldLinesRef := LNextOldLinesRef . LrRightLink  
              END (* IF *) 
            END (* WHILE *) 
          EXCEPT Marks . Unordered 
          => CantHappen 
               ( AFT . A_IfteAdjustLineMarks_Unordered_markss ) 
          END (* TRY EXCEPT *) 

        (* Now adjust the mark. *) 
        ; IF ( LNextOldLinesRef # NIL 
               (* ^Which implies IfteOldFromLinesRef # NIL *) 
               AND Marks . Equal 
                     ( LMark . LmTokMark , LNextOldLinesRef . LrBolTokMark ) 
             ) 
            OR ( IfteOldFromLinesRef = NIL 
                 AND LMark . LmLineNo <= TempEditRef . TeLineNo 
               ) 
          THEN (* Adjustment involves the newly created LinesRefs. *) 
            IF LNextOldLinesRef = LinesRef OR IfteOldFromLinesRef = NIL 
            THEN (* In the the edited region. *) 
              IF InsNlPos # LbeStd . LimitedCharNoInfinity 
              THEN (* We are in a newly split, edited LinesRef. *)  
                IF LMark . LmLineNo = TempEditRef . TeLineNo 
                   AND LMark . LmCharPos >= InsNlPos  
                THEN (* In the edited line, at or past the split. *) 
                  LOldToNewLineNoBias := 1 
                ; LMark . LmCharPos 
                    := LMark . LmCharPos - InsNlPos + NlIndentPos   
                ELSIF LMark . LmLineNo > TempEditRef . TeLineNo 
                THEN (* Past the edited line. *) 
                  LOldToNewLineNoBias := 1 
                END (* IF *) 
              ELSIF DelNlShift # LbeStd . LimitedCharNoInfinity 
              THEN 
                IF LMark . LmLineNo = TempEditRef . TeLineNo 
                THEN (* Squeeze LmCharPos leftward to end of line. *) 
                  LMark . LmCharPos := MIN ( LMark . LmCharPos , DelNlShift ) 
                ELSIF LMark . LmLineNo > TempEditRef . TeLineNo 
                THEN (* In newly joined part of edited LinesRef. *) 
                  LOldToNewLineNoBias := - 1 
                ; IF LMark . LmLineNo = TempEditRef . TeLineNo + 1 
                  THEN INC ( LMark . LmCharPos , DelNlShift ) 
                  END (* IF *) 
                END (* IF *) 
              END (* IF *) 
            ELSIF IfteSecondOldLinesRef # NIL 
                  AND LNextOldLinesRef = IfteSecondOldLinesRef 
                  AND DelNlShift # LbeStd . LimitedCharNoInfinity 
                  AND LMark . LmLineNo = 0 
                  AND TempEditRef . TeLineNo = IfteFirstOldActualLineCt - 1 
            THEN INC ( LMark . LmCharPos , DelNlShift ) 
            END (* IF *) 
          ; LNewLineNo 
              := LNextOldLineNo + LOldToNewLineNoBias + LMark . LmLineNo  
          ; LOOP (* While LNextNewLinesSs is behind. *) 
              LNextNewLinesRef := IfteLinesRefArray [ LNextNewLinesSs ] 
            ; IF LNextNewLinesRef = NIL 
              THEN INC ( LNextNewLinesSs ) (* Can't overflow. *) 
              ELSE
                LNextNewLineCt 
                  := Display . ActualNoOfLines ( LNextNewLinesRef . LrLineCt)
              ; IF LNewLineNo >= LNextNewLineNo + LNextNewLineCt 
                THEN 
                  INC ( LNextNewLineNo , LNextNewLineCt ) 
                ; INC ( LNextNewLinesSs ) (* Can't overflow. *) 
                ELSE EXIT 
                END (* IF *) 
              END (* IF *) 
            END (* LOOP *) 
          ; LMark . LmTokMark := LNextNewLinesRef . LrBolTokMark  
          ; LMark . LmLineNo := LNewLineNo - LNextNewLineNo  
          ; LMark . LmLinesRef := LNextNewLinesRef 
          ; IF LMark . LmMarkSs = PaintHs . MarkSsTyp . MarkSsCursor 
            THEN 
              INC ( LMark . LmWindowRef . WrCursorLineNoInWindow 
                  , LOldToNewLineNoBias 
                  ) 
            END (* IF *) 
          ELSE (* Outside the new LinesRefs. *) 
            IF LMark . LmLinesRef = NIL 
            THEN (* Doesn't point to a LinesRef at all.  Adjust numerically *) 
              IF IsAncestorMark 
                   ( LMark . LmTokMark . EstNodeNo  
                   , LMark . LmTokMark . EstNodeCt 
                   , MinNodeNoToAdjust 
                   ) 
              THEN 
                INC ( LMark . LmTokMark . EstNodeCt , Bias ) 
              END (* IF *)   
            ; IF LMark . LmTokMark . EstNodeNo >= MinNodeNoToAdjust 
              THEN 
                INC ( LMark . LmTokMark . EstNodeNo , Bias ) 
              END (* IF *) 
            ELSE (* Just copy the mark from the LinesRef (where it could 
                    have changed). 
                 *)  
              LMark . LmTokMark := LMark . LmLinesRef . LrBolTokMark 
            END (* IF *) 
          END (* IF *) 

        (* Advance to the next mark. *) 
        ; IF LMark . LmRightLink = IfteImagePers . IpMarkHeader 
          THEN (* End of list. *) 
            EXIT 
          ELSE 
            Assert 
              ( LMark . LmLinesRef = NIL 
                OR ( LMark . LmLinesRef . LrLeftLink # NIL 
                     AND LMark . LmLinesRef . LrRightLink # NIL 
                   ) 
              , AFT . A_IfteAdjustLineMarks_unlinked_LinesRef 
              ) 
          ; LMark := LMark . LmRightLink
          END (* IF *) 
        END (* LOOP *) 
      END (* IF *) 
    END IfteAdjustLineMarks 

  ; PROCEDURE IfteMakeChanges ( ) 
    RAISES { Backout }  
    (* Make changes to non-functional data structures.  Hopefully, we
       won't crash during these.  If a crash occurs later, we can undo
       them, with the same hope, before writing a checkpoint and for
       ignoring the failing operation. 
    *) 

    = BEGIN 
      (* Modify the list of LinesRefs. *)
        PaintHs . UnlinkLinesRefRange 
          ( IfteOldFromLinesRef , IfteOldThruLinesRef ) 
        (* NOTE: There can be marks that point into these LinesRefs.  
                 Patch them later. 
        *) 
      (* Do AdjustLinesRefsAndNodeNos while neither old nor new LinesRefs are
         linked in.  The new ones are done separately. 
      *) 
(* FIX: AdjustLinesRefsNodeNos can raise assertion failures.
        Do something about it. 
*) 
      ; AdjustLinesRefsNodeNos 
          ( ImageRef 
          , IfteSuccLinesRefMeat 
          , IfteMaxTouchedNodeNo + 1  
          , IfteNodeNoChange 
          ) 

      ; FOR RLinesRefSs := MarkIdTyp . MiLeadingLine 
            TO MarkIdTyp . MiTrailingLine 
	DO WITH WLinesRef = IfteLinesRefArray [ RLinesRefSs ] 
	   DO IF WLinesRef # NIL 
	      THEN 
                PaintHs . InsertLinesRefToLeft 
                  ( InsertToLeftOfRef := IfteSuccLinesRef 
                  , RefToInsert := WLinesRef 
                  ) 
              END (* IF *) 
           END (* WITH *) 
        END (* FOR *) 
      ; IF IfteSuccLinesRefMeat # NIL 
        THEN 
          IfteOldSuccBolTokMark := IfteSuccLinesRefMeat . LrBolTokMark 
        ; IfteSuccLinesRefMeat . LrBolTokMark := IfteNewEndBolTokMark 
        END (* IF *) 
      ; IfteAdjustLineMarks 
          ( IfteMaxTouchedNodeNo + 1  
          , IfteNodeNoChange 
          ) 
      (* Switch to the new Est and adjust the marks in LinesRefs 
         beyond the point of change. *) 
(* TODO: do versions here: Also altered tree, although that will 
         mainly happen when chars are edited. *) 
      ; ImageRef . ItPers . IpEstRoot := IfteNewEstRoot 
      END IfteMakeChanges 

  ; PROCEDURE IfteUndoChanges ( ) 

    = VAR LSavedCallback : Assertions . QueryProcTyp 

    ; BEGIN 
        FOR RLinesRefSs := MarkIdTyp . MiLeadingLine 
            TO MarkIdTyp . MiTrailingLine 
        DO WITH WLinesRef = IfteLinesRefArray [ RLinesRefSs ] 
           DO IF WLinesRef # NIL 
              THEN 
                PaintHs . UnlinkLinesRef ( WLinesRef ) 
              END (* IF *) 
           END (* WITH *) 
        END (* FOR *) 
      ; UnadjustLinesRefsNodeNos 
          ( ImageRef 
          , IfteSuccLinesRefMeat 
          , IfteMaxTouchedNodeNo + 1  
          , IfteNodeNoChange 
          ) 

      ; PaintHs . InsertLinesRefRangeToLeft 
          ( InsertToLeftOfRef := IfteSuccLinesRef 
          , FromLinesRef := IfteOldFromLinesRef 
          , ThruLinesRef := IfteOldThruLinesRef 
          ) 
      ; IF IfteSuccLinesRefMeat # NIL 
        THEN 
           IfteSuccLinesRefMeat . LrBolTokMark := IfteOldSuccBolTokMark
        END (* IF *) 
      ; ImageRef . ItPers . IpEstRoot := IfteOldEstRoot

      (* Damn the torpedos! Full speed ahead! 
         We need to call BruteForceVerifyAllLinesRefs, just to re-repair any
         marks that might have been changed for the new list of lines
         refs.  But its usual function ccould include raising assertion
         failures if anything is wrong.  Hopefully, we really did put
         things back together the way they were, in which case,
         nothing will go wron...noting woll goo worn... nthig wl
         go wg...  In any case, forge ahead, since this is the only
         chance of getting everything back the way it was before Ifte
         was called.
      *)
      ; LSavedCallback := Assertions . DefaultQueryProc 
      ; Assertions . DefaultQueryProc := Assertions . NeverRaise 
      ; TRY   
          BruteForceVerifyAllLinesRefs ( ImageRef , RepairIsOK := TRUE ) 
        EXCEPT
        | AssertionFailure => (* Squelch this, it's too late *)
        ELSE 
        END (* TRY EXCEPT *) 
      ; Assertions . DefaultQueryProc := LSavedCallback 

      ; UnadjustLineMarksNodeNos 
          ( ImageRef 
          , IfteMaxTouchedNodeNo + 1  
          , IfteNodeNoChange 
          ) 
      END IfteUndoChanges 

  ; PROCEDURE IfteBuildNewLines ( ) 
    RAISES { Backout , Thread . Alerted } 

    = VAR LLineCt : LbeStd . LineNoTyp 
    ; VAR LStartBolTokMark : Marks . TokMarkTyp 
    ; VAR LEditedString : Strings . StringTyp 
    ; VAR LTextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
    ; VAR LLineErrArrayRef : PaintHs . LineErrArrayRefTyp 
    ; VAR LNewLinesNo : LbeStd . LineNoTyp 

    ; BEGIN (* IfteBuildNewLines *) 
        IfteLinesRefArray := LinesRefArrayNull 
      ; LNewLinesNo := 0 
      ; LStartBolTokMark := IfteStartBolTokMark 
      ; LLineCt 
          := Display . LineCtOfBolTokMark 
               ( IfteNewEstRoot , LStartBolTokMark ) 

      (* Look for MiLeadingLine *) 
      ; IF IfteLinesAccountedFor < IfteNewLinesCt  
        THEN 
          IF LLineCt > 0 
          THEN (* We have a new blank line. It will be element 
                  MiLeadingLine. *) 
            WITH WLinesRef 
                 = IfteLinesRefArray [ MarkIdTyp . MiLeadingLine ] 
            DO RegenerateEditedLine 
                 ( ImageRef := ImageRef 
                 , EstRoot := IfteNewEstRoot 
                 , StartBolTokMark := LStartBolTokMark 
                 , (* VAR *) RegeneratedString := LEditedString 
                 , (* VAR *) RegeneratedTextAttrArrayRef 
                     := LTextAttrArrayRef 
                 , (* VAR *) RegeneratedLineErrArrayRef := LLineErrArrayRef 
                 , (* VAR *) EndBolTokMark := IfteNewEndBolTokMark 
                 ) 
            ; IfteActualBlankLinesBefore := LLineCt 
            ; IF LNewLinesNo < IfteNewLinesCt 
              THEN 
                WLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
              ; WLinesRef . LrBolTokMark := LStartBolTokMark 
              ; WLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
              ; WLinesRef . LrFromPos := 0 
              ; WLinesRef . LrLineLen := 0 
              ; WLinesRef . LrLineCt := LLineCt 
              ; WLinesRef . LrGapAfter := FALSE 
              ; WLinesRef . LrHasMark := FALSE 
              ; WLinesRef . LrIsStopper := FALSE 
              ; WLinesRef . LrLineText := "" 
(* CHECK: ^Can we get away with a NIL here instead? (and below, too.) 
             Places in Display do it, but for LrGapAfter = TRUE. *) 
              ; WLinesRef . LrTextAttrArrayRef := NIL 
              ; WLinesRef . LrLineErrArrayRef := NIL 
              END (* IF *) 
            END (* WITH WLinesRef *) 
          ; INC ( IfteLinesAccountedFor (* , LLineCt *) ) 
          ; LStartBolTokMark := IfteNewEndBolTokMark 
          ; LLineCt 
              := Display . LineCtOfBolTokMark 
                   ( IfteNewEstRoot , LStartBolTokMark ) 
          ; INC ( LNewLinesNo ) 
          END (* IF *) 
        END (* IF *) 

      (* Look for MiFirstText *) 
      ; IF IfteLinesAccountedFor < IfteNewLinesCt  
        THEN 
          IF LLineCt = 0 
          THEN (* This is the first text line. *) 
            WITH WLinesRef = IfteLinesRefArray [ MarkIdTyp . MiFirstText ] 
            DO RegenerateEditedLine 
                 ( ImageRef := ImageRef 
                 , EstRoot := IfteNewEstRoot 
                 , StartBolTokMark := LStartBolTokMark 
                 , (* VAR *) RegeneratedString := LEditedString 
                 , (* VAR *) RegeneratedTextAttrArrayRef 
                     := LTextAttrArrayRef 
                 , (* VAR *) RegeneratedLineErrArrayRef := LLineErrArrayRef 
                 , (* VAR *) EndBolTokMark := IfteNewEndBolTokMark 
                 ) 
            ; IF LNewLinesNo < IfteNewLinesCt 
              THEN 
                WLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
              ; WLinesRef . LrBolTokMark := LStartBolTokMark 
              ; WLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
              ; WLinesRef . LrFromPos 
                  := Strings . PosOf1stNonblank ( LEditedString ) 
              ; WLinesRef . LrLineLen := Strings . Length ( LEditedString ) 
              ; WLinesRef . LrLineCt := 0 
              ; WLinesRef . LrGapAfter := FALSE 
              ; WLinesRef . LrHasMark := FALSE 
              ; WLinesRef . LrIsStopper := FALSE 
              ; WLinesRef . LrLineText 
                  := Strings . ToText 
                       ( LEditedString , WLinesRef . LrFromPos ) 
              ; WLinesRef . LrTextAttrArrayRef := LTextAttrArrayRef  
              ; WLinesRef . LrLineErrArrayRef := LLineErrArrayRef  
              END (* IF *) 
            END (* WITH WLinesRef *) 
          ; INC ( IfteLinesAccountedFor ) 
          ; LStartBolTokMark := IfteNewEndBolTokMark 
          ; LLineCt 
              := Display . LineCtOfBolTokMark 
                   ( IfteNewEstRoot , LStartBolTokMark ) 
          ; INC ( LNewLinesNo ) 
          END (* IF *) 
        END (* IF *) 

      (* Look for MiSecondText *) 
      ; IF IfteLinesAccountedFor < IfteNewLinesCt 
        THEN 
          IF LLineCt = 0 
          THEN (* This is the second text line. *) 
            WITH WLinesRef = IfteLinesRefArray [ MarkIdTyp . MiSecondText ] 
            DO RegenerateEditedLine 
                 ( ImageRef := ImageRef 
                 , EstRoot := IfteNewEstRoot 
                 , StartBolTokMark := LStartBolTokMark 
                 , (* VAR *) RegeneratedString := LEditedString  
                 , (* VAR *) RegeneratedTextAttrArrayRef 
                      := LTextAttrArrayRef 
                 , (* VAR *) RegeneratedLineErrArrayRef := LLineErrArrayRef 
                 , (* VAR *) EndBolTokMark := IfteNewEndBolTokMark 
                 ) 
            ; IF LNewLinesNo < IfteNewLinesCt 
              THEN 
                WLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
              ; WLinesRef . LrBolTokMark := LStartBolTokMark 
              ; WLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
              ; WLinesRef . LrFromPos 
                  := Strings . PosOf1stNonblank ( LEditedString ) 
              ; WLinesRef . LrLineLen := Strings . Length ( LEditedString ) 
              ; WLinesRef . LrLineCt := 0 
              ; WLinesRef . LrGapAfter := FALSE 
              ; WLinesRef . LrHasMark := FALSE 
              ; WLinesRef . LrIsStopper := FALSE 
              ; WLinesRef . LrLineText 
                  := Strings . ToText 
                       ( LEditedString , WLinesRef . LrFromPos ) 
              ; WLinesRef . LrTextAttrArrayRef := LTextAttrArrayRef  
              ; WLinesRef . LrLineErrArrayRef := LLineErrArrayRef  
              END (* IF *) 
            END (* WITH WLinesRef *) 
          ; INC ( IfteLinesAccountedFor ) 
          ; LStartBolTokMark := IfteNewEndBolTokMark 
          ; LLineCt 
              := Display . LineCtOfBolTokMark 
                   ( IfteNewEstRoot , LStartBolTokMark ) 
          ; INC ( LNewLinesNo ) 
          END (* IF *) 
        END (* IF *) 

      (* Look for MiTrailingLine *) 
      ; IF IfteLinesAccountedFor < IfteNewLinesCt  
        THEN 
          WITH WLinesRef = IfteLinesRefArray [ MarkIdTyp . MiTrailingLine ] 
          DO RegenerateEditedLine 
               ( ImageRef := ImageRef 
               , EstRoot := IfteNewEstRoot 
               , StartBolTokMark := LStartBolTokMark 
               , (* VAR *) RegeneratedString := LEditedString 
               , (* VAR *) RegeneratedTextAttrArrayRef := LTextAttrArrayRef 
               , (* VAR *) RegeneratedLineErrArrayRef := LLineErrArrayRef 
               , (* VAR *) EndBolTokMark := IfteNewEndBolTokMark 
               ) 
          ; IfteActualBlankLinesAfter := LLineCt 
          ; IF LNewLinesNo < IfteNewLinesCt 
            THEN 
              WLinesRef := NEW ( PaintHs . LinesRefMeatTyp ) 
            ; WLinesRef . LrBolTokMark := LStartBolTokMark 
            ; WLinesRef . LrVisibleIn := PaintHs . WindowNoSetEmpty 
            ; WLinesRef . LrFromPos := 0 
            ; WLinesRef . LrLineLen := 0 
            ; WLinesRef . LrLineCt := LLineCt 
            ; WLinesRef . LrGapAfter := FALSE 
            ; WLinesRef . LrHasMark := FALSE 
            ; WLinesRef . LrIsStopper := FALSE 
            ; WLinesRef . LrLineText := "" 
            ; WLinesRef . LrTextAttrArrayRef := NIL 
            ; WLinesRef . LrLineErrArrayRef := NIL 
            END (* IF *) 
          END (* WITH WLinesRef *) 
        ; INC ( IfteLinesAccountedFor (* , LLineCt *) ) 
        END (* IF *) 
      END IfteBuildNewLines 

  ; PROCEDURE IfteVerifyNewLines ( ) 
    RAISES { Backout , Thread . Alerted } 

    = VAR LStartBolTokMark : Marks . TokMarkTyp 
    ; VAR LEndBolTokMark : Marks . TokMarkTyp 
    ; VAR LEditedString : Strings . StringTyp 
    ; VAR LFailureOccurred : BOOLEAN 

    ; BEGIN (* IfteVerifyNewLines *) 
        LFailureOccurred := FALSE 
      ; LStartBolTokMark := IfteStartBolTokMark 

      (* Look for MiLeadingLine *) 
      ; WITH WLinesRef = IfteLinesRefArray [ MarkIdTyp . MiLeadingLine ] 
        DO IF WLinesRef = NIL 
          THEN 
            Assert 
              ( IfteExpectedBlankLinesBefore = 0 
              , AFT . A_IfteVerifyNewLines_ExpectedBlankLinesBefore 
              ) 
          ELSE 
            VerifyEditedLine 
              ( ImageRef := ImageRef 
              , EstRoot := IfteNewEstRoot 
              , StartBolTokMark := LStartBolTokMark 
              , ExpectedLineCt 
                  := Display . LineCtOfBolTokMark 
                       ( IfteNewEstRoot , LStartBolTokMark ) 
              , ExpectedAtEndOfImage := FALSE 
              , ExpectedString := Strings . Empty ( ) 
              , (* VAR *) RegeneratedString := LEditedString 
              , (* VAR *) RegeneratedTextAttrArrayRef 
                            := WLinesRef . LrTextAttrArrayRef 
              , (* VAR *) RegeneratedLineErrArrayRef 
                            := WLinesRef . LrLineErrArrayRef 
              , (* VAR *) EndBolTokMark := LEndBolTokMark 
              , (* IN OUT *) FailureOccurred := LFailureOccurred 
              ) 
          ; WLinesRef . LrLineText 
              := Strings . ToText ( LEditedString , WLinesRef . LrFromPos ) 
          ; LStartBolTokMark := LEndBolTokMark 
          END (* IF *) 
        END (* WITH WLinesRef *) 

      (* Look for MiFirstText *) 
      ; WITH WLinesRef = IfteLinesRefArray [ MarkIdTyp . MiFirstText ] 
        DO IF WLinesRef = NIL 
          THEN
            Assert 
              ( Strings . Length ( IfteExpectedString1 ) = 0 
              , AFT . A_IfteVerifyNewLines_ExpectedString1 
              ) 
          ELSE 
             VerifyEditedLine 
                 ( ImageRef := ImageRef 
                 , EstRoot := IfteNewEstRoot 
                 , StartBolTokMark := LStartBolTokMark 
                 , ExpectedLineCt := 0 
                 , ExpectedAtEndOfImage := FALSE 
                 , ExpectedString := IfteExpectedString1 
                 , (* VAR *) RegeneratedString := LEditedString  
                 , (* VAR *) RegeneratedTextAttrArrayRef 
                     := WLinesRef . LrTextAttrArrayRef 
                 , (* VAR *) RegeneratedLineErrArrayRef 
                     := WLinesRef . LrLineErrArrayRef 
                 , (* VAR *) EndBolTokMark := LEndBolTokMark 
                 , (* IN OUT *) FailureOccurred := LFailureOccurred 
                 ) 
          ; WLinesRef . LrLineText 
              := Strings . ToText ( LEditedString , WLinesRef . LrFromPos ) 
          ; LStartBolTokMark := LEndBolTokMark 
          END (* IF *) 
        END (* WITH WLinesRef *) 

      (* Look for MiSecondText *) 
      ; WITH WLinesRef = IfteLinesRefArray [ MarkIdTyp . MiSecondText ] 
        DO IF WLinesRef = NIL 
          THEN 
            Assert 
              ( Strings . Length ( IfteExpectedString2 ) = 0 
              , AFT . A_IfteVerifyNewLines_ExpectedString2 
              ) 
          ELSE 
            Assert 
              ( InsNlPos # LbeStd . LimitedCharNoInfinity 
              , AFT . A_IfteVerifyNewLines_SecondLineWithoutNl 
              ) 
          ; VerifyEditedLine 
              ( ImageRef := ImageRef 
              , EstRoot := IfteNewEstRoot 
              , StartBolTokMark := LStartBolTokMark 
              , ExpectedLineCt := 0 
              , ExpectedAtEndOfImage := FALSE 
              , ExpectedString := IfteExpectedString2  
              , (* VAR *) RegeneratedString := LEditedString   
              , (* VAR *) RegeneratedTextAttrArrayRef 
                  := WLinesRef . LrTextAttrArrayRef 
              , (* VAR *) RegeneratedLineErrArrayRef 
                  := WLinesRef . LrLineErrArrayRef 
              , (* VAR *) EndBolTokMark := LEndBolTokMark 
              , (* IN OUT *) FailureOccurred := LFailureOccurred 
              ) 
          ; WLinesRef . LrLineText 
              := Strings . ToText ( LEditedString , WLinesRef . LrFromPos ) 
          ; LStartBolTokMark := LEndBolTokMark 
          END (* IF *) 
        END (* WITH WLinesRef *) 

      (* Look for MiTrailingLine *) 
      ; WITH WLinesRef = IfteLinesRefArray [ MarkIdTyp . MiTrailingLine ] 
        DO IF WLinesRef = NIL  
          THEN
            Assert 
              ( IfteExpectedBlankLinesAfter = 0 
              , AFT . A_IfteVerifyNewLines_ExpectedBlankLinesAfter 
              ) 
          ELSE 
             VerifyEditedLine 
               ( ImageRef := ImageRef 
               , EstRoot := IfteNewEstRoot 
               , StartBolTokMark := LStartBolTokMark 
               , ExpectedLineCt 
                  := Display . LineCtOfBolTokMark 
                       ( IfteNewEstRoot , LStartBolTokMark ) 
               , ExpectedAtEndOfImage := FALSE 
               , ExpectedString := Strings . Empty ( ) 
               , (* VAR *) RegeneratedString := LEditedString  
               , (* VAR *) RegeneratedTextAttrArrayRef 
                   := WLinesRef . LrTextAttrArrayRef 
               , (* VAR *) RegeneratedLineErrArrayRef 
                   := WLinesRef . LrLineErrArrayRef 
               , (* VAR *) EndBolTokMark := LEndBolTokMark 
               , (* IN OUT *) FailureOccurred := LFailureOccurred 
               ) 
          ; WLinesRef . LrLineText 
              := Strings . ToText ( LEditedString , WLinesRef . LrFromPos ) 
          END (* IF *) 
        END (* WITH WLinesRef *) 
      END IfteVerifyNewLines 

  ; BEGIN (* InnerFlushTempEdit *) 
      VAR LEditedLinesRefMeat : PaintHs . LinesRefMeatTyp 
    ; VAR LPredOfEditedLinesRefs : PaintHs . LinesRefTyp 
    ; VAR LSuccOfEditedLinesRefs : PaintHs . LinesRefTyp 
      (* If two LinesRefs joined, successor of the second joined one. *)  
    ; VAR LOldEndBolTokMark : Marks . TokMarkTyp 
    ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
    ; VAR LSecondOldActualLineCt : LbeStd . LineNoTyp 
    ; VAR LLineNo : LbeStd . LineNoTyp 
    ; VAR LThruLineNo : LbeStd . LineNoTyp 
    ; VAR LLineCt : LbeStd . LineNoTyp 
    ; VAR LLineNoInWindow : LbeStd . LineNoSignedTyp 
    ; VAR LWindowRef : PaintHs . WindowRefTyp 
    ; VAR LRepaintKind : RepaintKindTyp 
    ; VAR LBlankPrefixLen : Strings . StringSsTyp 
    ; VAR LLeadingBlankLinesIncluded : LbeStd . LineNoTyp  
    ; VAR LTrailingBlankLinesIncluded : LbeStd . LineNoTyp  
    ; VAR LWindowLineLen : LbeStd . CharNoTyp 

    ; BEGIN (* Block for InnerFlushTempEdit *)

      (* I think it is now always true that LinesRef 
         = TempEditRef . TeLinesRef. 
         But for some calls, the argument is rather tangled, so leave these 
         as separate parameters.  Eventually remove the LinesRef parameter 
         and this assertion.
      *) 
        Assert 
          ( LinesRef = TempEditRef . TeLinesRef 
          , AFT . A_TextEdit_InnerFlushTempEdit_ParameterMismatch 
          ) 

      ; IF TempEditRef . TeDelFromPos # LbeStd . LimitedCharNoInfinity 
        THEN (* There is some temp editing to flush. *)   
          IfteImagePers := ImageRef . ItPers 
        ; LEditedLinesRefMeat := TempEditRef . TeLinesRef 
        ; Assert 
            ( NOT LEditedLinesRefMeat . LrGapAfter 
            , AFT . A_InnerFlushTempEdit_EditingWithGapAfter   
            ) 
        ; LPredOfEditedLinesRefs := LEditedLinesRefMeat . LrLeftLink 
        ; IfteSecondOldLinesRef := NIL 
        ; LSecondOldActualLineCt := 0 
        ; IF Display . LinesRefIsEndOfImage 
               ( ImageRef , LEditedLinesRefMeat ) 
          THEN (* LEditedLinesRefMeat is the empty EOI one. *) 
          (* Do not unlink anything, not even LinesRef. *) 
            IfteOldFromLinesRef := NIL 
          ; IfteOldThruLinesRef := NIL 
          ; IfteFirstOldActualLineCt := TempEditRef . TeLineNo + 1 
          ; LSuccOfEditedLinesRefs := LEditedLinesRefMeat . LrRightLink   
            (* ^Which will be the list header. *) 
          ; DelNlShift := LbeStd . LimitedCharNoInfinity 
            (* ^Callers may ensure this, but if not, it's robust. *) 
          ; IfteSuccLinesRef := LEditedLinesRefMeat
          ; IfteSuccLinesRefMeat := LEditedLinesRefMeat 
          ELSE (* Not editing in EOI. *) 
            IfteOldFromLinesRef := LEditedLinesRefMeat  
          ; IfteFirstOldActualLineCt 
              := Display . ActualNoOfLines 
                   ( LEditedLinesRefMeat . LrLineCt ) 
          ; Assert 
              ( TempEditRef . TeLineNo < IfteFirstOldActualLineCt 
              , AFT . A_InnerFlushTempEdit_EditingBeyondLineCt   
              ) 
          ; TYPECASE LEditedLinesRefMeat . LrRightLink 
            OF PaintHs . LinesRefMeatTyp ( TSecondOldLinesRef ) 
            => IF DelNlShift # LbeStd . LimitedCharNoInfinity 
              THEN (* Two lines are joined. *) 
               (* Although this case could end up being
                  RepaintKindPseudoShift in some windows, at least one
                  window has the original line visible, because this is
                  a change to the original first line.  So the new
                  LEditedLinesRefMeat we are about to construct will never 
                  be unnecessary garbage. 
               *)
                IfteOldThruLinesRef := TSecondOldLinesRef 
              ; IfteSecondOldLinesRef := TSecondOldLinesRef 
              ; Display . SecureSucc ( ImageRef , IfteSecondOldLinesRef ) 
              ; LSecondOldActualLineCt 
                  := Display . ActualNoOfLines 
                       ( IfteSecondOldLinesRef . LrLineCt )
              ; Assert 
                  ( NOT Display . LinesRefIsEndOfImage 
                          ( ImageRef , IfteSecondOldLinesRef ) 
                  , AFT . A_InnerFlushTempEditNlDeletedAtEOI 
                  ) 
              ; LSuccOfEditedLinesRefs := IfteSecondOldLinesRef . LrRightLink 
              ELSE (* No joined lines. *) 
                IfteOldThruLinesRef := IfteOldFromLinesRef
              ; LSuccOfEditedLinesRefs := LEditedLinesRefMeat . LrRightLink 
              END (* IF *) 
            ELSE 
              CantHappen 
                ( AFT . A_InnerFlushTempEdit_NonEoiLinesRefIsLast ) 
            ; IfteOldThruLinesRef := IfteOldFromLinesRef
            ; LSuccOfEditedLinesRefs := LEditedLinesRefMeat . LrRightLink 
            END (* TYPECASE *) 
          END (* IF *) 

        ; IfteOldEstRoot := IfteImagePers . IpEstRoot 
        ; TYPECASE LSuccOfEditedLinesRefs 
          OF NULL (* Can happen with damaged checkpoint files. *) 
          => LineMarks . GetEndOfImage 
               ( IfteImagePers . IpLang , IfteOldEstRoot , LOldEndBolTokMark ) 
          | PaintHs . LinesRefMeatTyp ( TSuccLinesRefMeat ) 
          => LOldEndBolTokMark := TSuccLinesRefMeat . LrBolTokMark 
          ELSE 
            LineMarks . GetEndOfImage 
              ( IfteImagePers . IpLang , IfteOldEstRoot , LOldEndBolTokMark ) 
          END (* TYPECASE *) 
        ; IfteStartBolTokMark := Marks . TokMarkNull  
        ; IF Options . TreeBrowsing 
          THEN
            TreeBrowse . Browse 
              ( IfteOldEstRoot , IfteImagePers . IpLang 
              , "Before MergeTextEdit " 
              ) 
          END (* IF *) 
        ; MergeTxt . MergeTextEdit 
            ( Lang := IfteImagePers . IpLang 
            , EstRootRef := IfteOldEstRoot 
            , StartTokMark := LEditedLinesRefMeat . LrBolTokMark 
            , EndTokMark := LOldEndBolTokMark 
            , BlankLineNo := TempEditRef . TeLineNo 
            , DelFromPos := TempEditRef . TeDelFromPos 
            , DelToPos := TempEditRef . TeDelToPos 
            , InsText := TempEditRef . TeEditedString 
            , InsLen := TempEditRef . TeInsLen 
            , InsNlPos := InsNlPos 
            , NlIndentPos := NlIndentPos 
            , DelNlShift := DelNlShift 
            , (* VAR *) NewEstRootRef := IfteNewEstRoot 
            , (* VAR *) NodeNoChange := IfteNodeNoChange 
            , (* VAR *) MaxTouchedNodeNo := IfteMaxTouchedNodeNo 
            , (* VAR *) NewBolTokMark := IfteStartBolTokMark 
            , (* VAR *) NewLinesCt := IfteNewLinesCt 
            , (* VAR *) LeadingBlankLinesIncluded 
                          := LLeadingBlankLinesIncluded 
            , (* VAR *) TrailingBlankLinesIncluded 
                          := LTrailingBlankLinesIncluded 
            ) 
        ; IF Options . TreeBrowsing 
          THEN
            TreeBrowse . Browse 
              ( IfteNewEstRoot , IfteImagePers . IpLang 
              , "After MergeTextEdit " 
              ) 
          END (* IF *) 

        ; Assert 
            ( NOT Marks . IsNull ( IfteStartBolTokMark ) 
            , AFT . A_InnerFlushTempEditNoNewTokMark 
            ) 

        ; SkipBlankLinesToLeft 
            ( ImageRef 
            , (* VAR *) IfteOldFromLinesRef 
            , LLeadingBlankLinesIncluded 
            ) 
        ; SkipBlankLinesToRight 
            ( ImageRef 
            , (* VAR *) IfteOldThruLinesRef 
            , LTrailingBlankLinesIncluded 
            ) 

        (* Compute what is expected, from the text editing. *) 
        ; IfteExpectedBlankLinesBefore := 0 
        ; IfteExpectedString1 := Strings . Empty ( ) 
        ; IfteExpectedString2 := Strings . Empty ( ) 
        ; IfteExpectedBlankLinesAfter := 0 
        ; LBlankPrefixLen 
            := Strings . PosOf1stNonblank ( TempEditRef . TeEditedString ) 
        ; IF DelNlShift # LbeStd . LimitedCharNoInfinity 
          THEN (* Deleted new line, two lines are joined. *) 
            IF LBlankPrefixLen 
               = Strings . Length ( TempEditRef . TeEditedString ) 
            THEN (* Joined lines are entirely blank *) 
              IfteExpectedBlankLinesBefore 
                := LLeadingBlankLinesIncluded 
                   + IfteFirstOldActualLineCt 
                   - 1 
                   + LSecondOldActualLineCt  
                   + LTrailingBlankLinesIncluded 
            ELSE (* Joined lines contain nonblank text. *) 
              IfteExpectedBlankLinesBefore 
                := LLeadingBlankLinesIncluded + IfteFirstOldActualLineCt - 1 
            ; IfteExpectedString1 := TempEditRef . TeEditedString 
            ; IfteExpectedBlankLinesAfter 
                := LSecondOldActualLineCt - 1 + LTrailingBlankLinesIncluded 
            END (* IF *) 
          ELSIF InsNlPos # LbeStd . LimitedCharNoInfinity 
          THEN (* One line split into two new edited lines *) 
            IF LBlankPrefixLen 
               = Strings . Length ( TempEditRef . TeEditedString ) 
            THEN (* Both edited lines are all blank *) 
              IfteExpectedBlankLinesBefore 
                := LLeadingBlankLinesIncluded 
                   + IfteFirstOldActualLineCt 
                   + 1 
                   + LTrailingBlankLinesIncluded 
            ELSIF InsNlPos <= LBlankPrefixLen 
            THEN (* Edited lines are blank, nonblank. *) 
              IfteExpectedBlankLinesBefore 
                := LLeadingBlankLinesIncluded + TempEditRef . TeLineNo + 1 
            ; IfteExpectedBlankLinesAfter 
                := ( IfteFirstOldActualLineCt - 1 - TempEditRef . TeLineNo )  
                   + LTrailingBlankLinesIncluded 
            ; IfteExpectedString1 
                := Strings . Substring 
                     ( TempEditRef . TeEditedString , InsNlPos ) 
            ; TRY 
                Strings . InsertBlanksInPlace 
                  ( IfteExpectedString1 , 0 , NlIndentPos ) 
              EXCEPT Strings . SsOutOfBounds
              => CantHappen 
                   ( AFT . A_InnerFlushTempEdit_String_subscript_out_of_bounds1 ) 
              END (* TRY EXCEPT *) 
            ELSIF Strings . PosOfLastNonblank 
                    ( TempEditRef . TeEditedString ) 
                  < InsNlPos 
            THEN (* Edited is nonblank, blank. *) 
              IfteExpectedBlankLinesBefore 
                := LLeadingBlankLinesIncluded + TempEditRef . TeLineNo 
            ; IfteExpectedBlankLinesAfter 
                := ( IfteFirstOldActualLineCt - TempEditRef . TeLineNo ) 
                   + LTrailingBlankLinesIncluded  
            ; IfteExpectedString1 
                := Strings . Substring 
                     ( TempEditRef . TeEditedString , 0 , InsNlPos ) 
            ELSE (* Two nonblank edited lines. *) 
              IfteExpectedBlankLinesBefore 
                := LLeadingBlankLinesIncluded + TempEditRef . TeLineNo 
            ; IfteExpectedBlankLinesAfter 
                := ( IfteFirstOldActualLineCt - 1 - TempEditRef . TeLineNo ) 
                   + LTrailingBlankLinesIncluded  
            ; IfteExpectedString1 
                := Strings . Substring 
                     ( TempEditRef . TeEditedString , 0 , InsNlPos ) 
            ; IfteExpectedString2 
                := Strings . Substring 
                     ( TempEditRef . TeEditedString , InsNlPos ) 
            ; TRY 
                Strings . InsertBlanksInPlace 
                  ( IfteExpectedString2 , 0 , NlIndentPos ) 
              EXCEPT Strings . SsOutOfBounds
              => CantHappen 
                   ( AFT . A_InnerFlushTempEdit_String_subscript_out_of_bounds2 ) 
              END (* TRY EXCEPT *) 
            END (* IF *) 
          ELSE (* One line, before and after edits. *) 
            IF LBlankPrefixLen 
               = Strings . Length ( TempEditRef . TeEditedString ) 
            THEN (* Edited line is now entirely blank *) 
              IfteExpectedBlankLinesBefore 
                := LLeadingBlankLinesIncluded 
                   + IfteFirstOldActualLineCt 
                   + LTrailingBlankLinesIncluded 
            ELSE  (* Edited line contains nonblanks. *)  
              IfteExpectedBlankLinesBefore 
                := LLeadingBlankLinesIncluded + TempEditRef . TeLineNo 
            ; IfteExpectedString1 := TempEditRef . TeEditedString 
            ; IfteExpectedBlankLinesAfter 
                := ( IfteFirstOldActualLineCt - 1 - TempEditRef . TeLineNo ) 
                   + LTrailingBlankLinesIncluded  
            END (* IF *) 
          END (* IF *) 
        ; IF IfteOldThruLinesRef # NIL 
          THEN 
            IfteSuccLinesRef := IfteOldThruLinesRef . LrRightLink 
          ; TYPECASE IfteSuccLinesRef 
            OF PaintHs . LinesRefMeatTyp ( TSuccLinesRefMeat ) 
            => IfteSuccLinesRefMeat := TSuccLinesRefMeat
            ELSE IfteSuccLinesRefMeat := NIL 
            END (* TYPECASE *) 
          END (* IF *) 

        ; IfteLinesAccountedFor := 0 
        ; IfteBuildNewLines ( ) 
        ; IfteVerifyNewLines ( ) 
        ; IF IfteLinesRefArray [ MarkIdTyp . MiFirstText ] = NIL 
             AND IfteLinesRefArray [ MarkIdTyp . MiSecondText ] = NIL 
          THEN (* There are some cases where two successive all blank 
                  lines, as seen by TextEdit are combined into one, but 
                  MergeTxt can't combine the corresponding blank line mods 
                  in the Est, because they are in disjoint subtrees. 
                  So, if there are no intervening text mods, we insist 
                  only that the total number of blank lines be as expected. 
               *) 
            Assert 
              ( IfteActualBlankLinesBefore + IfteActualBlankLinesAfter 
                = IfteExpectedBlankLinesBefore 
                  + IfteExpectedBlankLinesAfter 
              , AFT . A_InnerFlushTempEdit_UnequalTotalBlankLines 
              ) 
          ELSE 
            Assert 
              ( IfteActualBlankLinesBefore = IfteExpectedBlankLinesBefore 
              , AFT . A_InnerFlushTempEdit_UnequalBlankLinesBefore 
              ) 
          ; Assert 
              ( IfteActualBlankLinesAfter = IfteExpectedBlankLinesAfter 
              , AFT . A_InnerFlushTempEdit_UnequalBlankLinesAfter 
              ) 
          END (* IF *) 
        ; Assert 
            ( IfteLinesAccountedFor = IfteNewLinesCt  
            , AFT . A_InnerFlushTempEditLineCtMismatch 
            ) 

        (* Nothing has been changed yet, so any assertion failures raised
           before here will just propagate out.  The state of everything
           will be as before the action. 
        *)

        (* There are several changes to the list of lines refs and the
           Est, that have to be atomic WRT whether we get an assertion
           failure, so that any checkpoint file written is consistent.
           First, we do all the changes here, with nothing that raises an
           assertion failure.  
        *)

        (* Modify the list of LinesRefs, hopefully without failures during
           the process. *)
        ; IfteMakeChanges ( ) 

        (* Now do checks that have to be done on the modified Est and
           LinesRefs list.  If something fails, catch the exception and
           undo the changes. *)

        ; TRY 
            BruteForceVerifyAllLinesRefs ( ImageRef , RepairIsOK := TRUE ) 

          (* Now go thru all windows, updating their fields and 
             repainting, as needed. *) 
          ; LWindowRef := ImageRef . ItWindowList 
          ; WHILE LWindowRef # NIL 
            DO IF LWindowRef . WrWindowNo 
                  IN LEditedLinesRefMeat . LrVisibleIn 
                  OR IfteSecondOldLinesRef # NIL 
                     AND LWindowRef . WrWindowNo 
                         IN IfteSecondOldLinesRef . LrVisibleIn 
                     (* This will be RepaintKindPseudoShift. *)  
              THEN (* We need LinesRefs and possibly some repainting 
                      for this window. 
                   *) 
                IF InsNlPos # LbeStd . LimitedCharNoInfinity 
                THEN 
                  LRepaintKind := RepaintKindTyp . RepaintKindShiftDown 
                ELSIF DelNlShift # LbeStd . LimitedCharNoInfinity 
                THEN 
                  IF NOT LWindowRef . WrWindowNo 
                         IN LEditedLinesRefMeat . LrVisibleIn 
                  THEN 
                    LRepaintKind 
                      := RepaintKindTyp . RepaintKindPseudoShift 
                  ELSE 
                    LRepaintKind := RepaintKindTyp . RepaintKindShiftUp 
                  END (* IF *) 
                ELSE 
                  LRepaintKind := RepaintKindTyp . RepaintKindNoShift  
                END (* IF *) 
              (* Display . LineNoInWindow depends on WrFirstLineLinesRef 
                 and WrFirstLineLineNo being set.  We use it only in 
                 cases where they are not changing. 
              *)    
              ; IF LWindowRef . WrFirstLineLinesRef 
                = LEditedLinesRefMeat 
                THEN 
                  LLineNoInWindow := - LWindowRef . WrFirstLineLineNo 
                ELSIF LWindowRef . WrFirstLineLinesRef 
                      = IfteSecondOldLinesRef 
                THEN 
                  LLineNoInWindow 
                    := - LWindowRef . WrFirstLineLineNo 
                       - Display . ActualNoOfLines 
                           ( LEditedLinesRefMeat . LrLineCt ) 
                ELSE 
                  LLineNoInWindow 
                    := Display . LineNoInWindow 
                         ( LWindowRef 
                         , LPredOfEditedLinesRefs . LrRightLink 
                         )
                END (* IF *) 
              (* INVARIANT: LLineNoInWindow is the window-relative line 
                   number of the first line represented by the current
                   new LinesRef. 
              *) 
              ; IF LRepaintKind 
                   = RepaintKindTyp . RepaintKindPseudoShift 
                THEN (* Tricky!  We in effect shift the top of the 
                        new set of lines downward one line 
                        (relative to the window) 
                        so that the previously invisible first 
                        of the two joined lines is now just 
                        visible at the top of the window. *) 
                  INC ( LLineNoInWindow ) 
                END (* IF *) 
              ; LWindowLineLen 
                  := EditWindow . SouthEastToCorner ( LWindowRef ) . h
              ; FOR RLinesRefSs := MarkIdTyp . MiLeadingLine 
                      TO MarkIdTyp . MiTrailingLine 
                DO WITH WLinesRef = IfteLinesRefArray [ RLinesRefSs ] 
                   DO IF WLinesRef # NIL 
                      THEN 
                        LLineCt 
                          := Display . ActualNoOfLines 
                               ( WLinesRef . LrLineCt ) 
                      ; IF LLineNoInWindow + LLineCt > 0 
                           AND LLineNoInWindow 
                               < EditWindow . SouthEastToCorner 
                                   ( LWindowRef ) 
                                 . v 
                        THEN (* Some portion of new line is visible. *) 
                          PaintHs . IncludeLrVisibleIn 
                            ( WLinesRef , LWindowRef . WrWindowNo ) 
                        ; IF LLineNoInWindow <= 0 
                          THEN (* This will be the first item visible 
                                  in the window. *) 
                            LWindowRef . WrFirstLineLinesRef 
                              := WLinesRef 
                          ; LWindowRef . WrFirstLineLineNo 
                              := - LLineNoInWindow 
                          END (* IF *) 

                        (* Do any needed repainting for new LinesRef. *)
                        ; IF TRUE 
(* CHECK: is this needed or not? *) 
                             OR LRepaintKind 
                                # RepaintKindTyp . RepaintKindNoShift 
                          THEN 
                            CASE RLinesRefSs 
                            OF MarkIdTyp . MiLeadingLine 
                            , MarkIdTyp . MiTrailingLine 
                            => LLineNo := MAX ( LLineNoInWindow , 0 ) 
                            ; LThruLineNo 
                                := MIN 
                                     ( LLineNoInWindow + LLineCt 
                                     , EditWindow . SouthEastToCorner 
                                         ( LWindowRef ) 
                                       . v 
                                     ) 
                                   - 1 
                            ; FOR RI := LLineNo TO LThruLineNo 
                              DO EditWindow . PaintBackground  
                                   ( Window := LWindowRef 
                                   , LineNoInWindow := RI 
                                   , FromPosInWindow := 0 
                                   , Len := LWindowLineLen 
                                   , BgColor := PaintHs . TaBgColorPlain 
                                   ) 
                              END (* FOR *) 
                            | MarkIdTyp . MiFirstText 
                            , MarkIdTyp . MiSecondText 
                            => Display . PaintUneditedNonblankLine 
                                 ( WindowRef := LWindowRef 
                                 , LineNoInWindow := LLineNoInWindow 
                                 , LinesRef := WLinesRef 
                                 ) 
                            END (* CASE *) 
                          END (* IF *) 
                        END (* IF *) 
                      ; INC ( LLineNoInWindow , LLineCt ) 
                      END (* IF *) 
                   END (* WITH WLinesRef *) 
                END (* FOR *) 

              (* Do any needed repainting of shifted lines below the 
                 site of the new LinesRefs. *) 
              ; IF IfteSuccLinesRefMeat # NIL 
                   AND LRepaintKind 
                       IN RepaintKindSetTyp 
                            { RepaintKindTyp . RepaintKindShiftUp 
                            , RepaintKindTyp . RepaintKindShiftDown 
                            } 
                   AND LLineNoInWindow 
                       < EditWindow . SouthEastToCorner ( LWindowRef ) . v 
                THEN 
                  LLinesRef := IfteSuccLinesRefMeat  
                ; LOOP 
                    IF LLineNoInWindow 
                       >= EditWindow . SouthEastToCorner ( LWindowRef ) . v 
                    THEN EXIT 
                    ELSE 
                      Display . SecureSucc ( ImageRef , LLinesRef ) 
                    ; IF LLinesRef . LrRightLink 
                         = IfteImagePers . IpLineHeaderRef 
                      THEN (* We know NOT PaintHs.LrGapAfter( LLinesRef ), 
                              so this means LLinesRef is to EndOfImage. *) 
                        WHILE LLineNoInWindow 
                              < EditWindow . SouthEastToCorner ( LWindowRef )
                                . v 
                        DO EditWindow . PaintBackground  
                             ( Window := LWindowRef 
                             , LineNoInWindow := LLineNoInWindow 
                             , FromPosInWindow := 0 
                             , Len := LWindowLineLen 
                             , BgColor := PaintHs . TaBgColorPlain 
                             ) 
                        ; INC ( LLineNoInWindow ) 
                        END (* WHILE *) 
                      ; EXIT 
                      ELSE 
                        LLineCt := LLinesRef . LrLineCt 
                      ; IF LLineCt = 0 
                        THEN 
                          Display . PaintUneditedNonblankLine 
                            ( LWindowRef , LLineNoInWindow , LLinesRef ) 
                        ; INC ( LLineNoInWindow ) 
                        ELSE 
                          LThruLineNo 
                            := MIN 
                                 ( LLineNoInWindow + LLineCt 
                                 , EditWindow . SouthEastToCorner 
                                     ( LWindowRef ) 
                                   . v 
                                 ) 
                               - 1 
                        ; FOR RI := LLineNoInWindow TO LThruLineNo 
                          DO EditWindow . PaintBackground  
                               ( Window := LWindowRef 
                               , LineNoInWindow := RI 
                               , FromPosInWindow := 0 
                               , Len := LWindowLineLen 
                               , BgColor := PaintHs . TaBgColorPlain 
                               ) 
                          END (* FOR *) 
                        ; INC ( LLineNoInWindow , LLineCt ) 
                        END (* IF *) 
                      ; LLinesRef := LLinesRef . LrRightLink 
                      END (* IF *) 
                    END (* IF *) 
                  END (* LOOP still visible in new window state *) 
                ; IF LRepaintKind = RepaintKindTyp . RepaintKindShiftDown 
                  THEN (* At most one LinesRef can become 
                                not visible in window. *) 
                    Display . MakeLinesRefsNotVisibleInWindow 
                      ( ImageRef 
                      , LLinesRef 
                      , LLinesRef 
                      , LWindowRef . WrWindowNo 
                      ) 
                  END (* IF *) 
                END (* IF *) 
              END (* IF *) 
            ; LWindowRef := LWindowRef . WrImageLink 
            END (* WHILE LWindowRef # NIL *) 
          ; BruteForceRepairLineMarks ( ImageRef )
          ; IF IfteSecondOldLinesRef # NIL 
            THEN IfteSecondOldLinesRef . LrHasMark := FALSE 
                 (* ^Why? Isn't it garbage? *) 
            END (* IF *) 
          ; Display . NoteImageSavedState ( ImageRef , FALSE ) 
          ; Display . NoteImageParsedState ( ImageRef , FALSE ) 
          ; Display . NoteImageAnalyzedState ( ImageRef , FALSE ) 
          EXCEPT Backout ( EMessage ) 
          => (* Undo changes to the LinesRefs list and the Est root. *) 
            IfteUndoChanges ( ) 
          ; AssertDevel . WriteCheckpoint 
              ( ImageRef := ImageRef 
              , Message := EMessage 
              , DoCreateVersion := FALSE 
              ) 
            (* ^This will rewrite the checkpoint that was already written from 
                inside AssertDevel, but with the changes undone. 
            *) 
(* TODO: This is pretty inefficient.  Find some not-too-inelegant way to 
         communicate to AssertDevel not to write a checkpoint. 
*) 
          ; RAISE Backout ( EMessage ) (* Pass it on up. *) 
          END (* TRY EXCEPT *) 
        ; EVAL LThruLineNo (* Place for a breakpoint. *) 
        END (* IF *) 
      END (* Block *) 
    END InnerFlushTempEdit 

(* EXPORTED: *)
; PROCEDURE FlushEdit ( ImageRef : PaintHs . ImageTransientTyp ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 

  ; BEGIN (* FlushEdit *) 
      IF ImageRef # NIL 
      THEN 
        LImagePers := ImageRef . ItPers 
      ; CASE LImagePers . IpTempEditState 
        OF PaintHs . TempEditStateTyp . TeStateSynt 
        => SyntEdit . FlushEdit ( ImageRef ) 
        | PaintHs . TempEditStateTyp . TeStateText 
        => InnerFlushTempEdit 
             ( ImageRef 
             , LImagePers . IpTempEditRef . TeLinesRef 
             , LImagePers . IpTempEditRef 
             ) 
        ; InitTempEditForText ( LImagePers . IpTempEditRef , NIL , 0 ) 
        ; LImagePers . IpTempEditState 
            := PaintHs . TempEditStateTyp . TeStateIdle 
        ELSE 
        END (* CASE *) 
      END (* IF *) 
    END FlushEdit 

; CONST TextAttrArrayTempGrowthSize = 5 

; PROCEDURE EnsureTextAttrArraySize 
    ( VAR (* IN OUT *) ArrayRef : PaintHs . TextAttrArrayRefTyp 
    ; MinSize : PortTypes . Int32Typ := 0 
    ) 

  = VAR LOldSize : PortTypes . Int32Typ 
  ; VAR LNewSize : PortTypes . Int32Typ 
  ; VAR LOldRef : PaintHs . TextAttrArrayRefTyp 

  ; BEGIN
      IF ArrayRef = NIL 
      THEN 
        ArrayRef 
          := NEW ( PaintHs . TextAttrArrayRefTyp 
                 , MinSize + TextAttrArrayTempGrowthSize 
                 ) 
      ; FOR RI := 0 TO NUMBER ( ArrayRef ^ ) - 1 
        DO 
          ArrayRef ^ [ RI ] := PaintHs . TextAttrDefault 
        END (* FOR *)  
      ELSE
        LOldSize := NUMBER ( ArrayRef ^ ) 
      ; IF LOldSize  < MinSize 
        THEN 
          LOldRef := ArrayRef
        ; LNewSize := MinSize + TextAttrArrayTempGrowthSize 
          (* Only grow linearly. I don't expect major overall growth here. *) 
        ; ArrayRef := NEW ( PaintHs . TextAttrArrayRefTyp , LNewSize ) 
        ; SUBARRAY ( ArrayRef ^ , 0 , LOldSize ) := LOldRef ^ 
        ; FOR RI := LOldSize TO LNewSize - 1 
          DO 
            ArrayRef ^ [ RI ] := PaintHs . TextAttrDefault 
          END (* FOR *)  
        END (* IF *) 
      END (* IF *) 
    END EnsureTextAttrArraySize 

; PROCEDURE SetTextAttr 
    ( VAR (* IN OUT *) ArrayRef : PaintHs . TextAttrArrayRefTyp
    ; VAR (* IN OUT *) ActualLength : PortTypes . Int32Typ  
    ; StartPos : LbeStd . LimitedCharNoTyp
    ; Len : LbeStd . LimitedCharNoTyp 
    ; LineLen : LbeStd . LimitedCharNoTyp 
    ; NewAttr : PaintHs . TextAttrTyp 
    ) 

  = VAR LToPos : LbeStd . LimitedCharNoTyp 
  ; VAR LSs : PortTypes . Int32Typ  
  ; VAR LSs2 : PortTypes . Int32Typ  

  ; BEGIN 
      IF Len > 0 
      THEN 
        IF ArrayRef = NIL 
        THEN
          EnsureTextAttrArraySize ( ArrayRef , 2 )
        END (* IF *) 
      ; LToPos := StartPos + Len 
      ; LSs := 0 
(* TODO: Maybe make this a binary search. *) 
      ; LOOP 
          IF LSs >= ActualLength 
          THEN (* Off the end of attrs. *) 
            IF LSs > 0 
               AND PaintHs . TextAttrIsEqual 
                     ( ArrayRef ^ [ LSs - 1 ] , NewAttr ) 
            THEN (* Just let the last element cover the change. *) 
              RETURN 
            ELSE (* Append an element at the end. *)  
              INC ( ActualLength )
            ; EnsureTextAttrArraySize 
                ( ArrayRef , ActualLength + ORD ( LToPos < LineLen ) )
            ; ArrayRef ^ [ LSs ] := NewAttr  
            ; ArrayRef ^ [ LSs ] . TaCharPos := StartPos  
            ; IF LToPos < LineLen 
              THEN (* Back to previous attribute, for the rest of the line. *) 
                INC ( ActualLength )
              ; ArrayRef ^ [ LSs + 1 ] := ArrayRef ^ [ LSs - 1 ] 
              ; ArrayRef ^ [ LSs + 1 ] . TaCharPos := LToPos 
              END (* IF *) 
            ; EXIT
            END (* IF *) 
          ELSIF ArrayRef ^ [ LSs ] . TaCharPos < StartPos   
          THEN (* Keep searching. *) 
            INC ( LSs ) 
          ELSE (* LSs < ActualLength 
                  AND LSs is the least value such that 
                      ArrayRef ^ [ LSs ] . TaCharPos >= StartPos *) 
            IF LSs > 0 AND LToPos < ArrayRef [ LSs ] . TaCharPos 
            THEN (* The entire range to be changed lies, properly at each end,
                     within the one segment starting with LSs - 1. *) 
              IF NOT PaintHs . TextAttrIsEqual 
                       ( NewAttr , ArrayRef ^ [ LSs - 1 ] )
              THEN (* Split segment starting at LSs - 1 into three segments. *)
                INC ( ActualLength , 2 ) 
              ; EnsureTextAttrArraySize ( ArrayRef , ActualLength )
              ; FOR RI := ActualLength - 1 TO LSs + 1 BY - 1 
                DO
                  ArrayRef ^ [ RI ] := ArrayRef ^ [ RI - 2 ] 
                END (* FOR *) 
              (* Element LSs is empty and LSs + 1 is a copy of LSs - 1 . *) 
              ; ArrayRef ^ [ LSs ] := NewAttr 
              ; ArrayRef ^ [ LSs ] . TaCharPos := StartPos  
              ; ArrayRef ^ [ LSs + 1 ] . TaCharPos := LToPos  
              END (* IF*) 
            ELSE         
              LSs2 := LSs 
            ; WHILE LSs2 + 1 < ActualLength 
                    AND LToPos >= ArrayRef ^ [ LSs2 + 1 ] . TaCharPos 
              DO INC ( LSs2 ) 
              END (* WHILE *) 
            (* LSs2 is the least value such that 
               LSs2 = ActualLength - 1 
               OR LToPos < ArrayRef ^ [ LSs2 + 1 ] . TaCharPos *) 
            ; DEC ( ActualLength , LSs2 - LSs ) 
                  (* will remove these elements later. *) 
            ; IF LSs > 0 
                 AND PaintHs . TextAttrIsEqual 
                       ( NewAttr , ArrayRef ^ [ LSs - 1 ] ) 
              THEN (* Attr to the left is same as the NewAttr. *) 
                IF PaintHs . TextAttrIsEqual 
                     ( NewAttr , ArrayRef ^ [ LSs2 ] ) 
                THEN (* Attr to both right and left are same as NewAttr *) 
                (* Remove element at LSs2 *) 
                  INC ( LSs2 ) 
                ; DEC ( ActualLength ) 
                ELSE (* Only attr to left is same as NewAttr. *) 
                  ArrayRef ^ [ LSs2 ] . TaCharPos := LToPos 
                END (* IF *) 
              ELSE (* There is no equal attribute to the left. *) 
                IF PaintHs . TextAttrIsEqual 
                     ( NewAttr , ArrayRef ^ [ LSs2 ] ) 
                THEN  (* Only attr to right is same . *)
                  ArrayRef ^ [ LSs2 ] . TaCharPos := StartPos 
                ELSE (* Attrs to left and right both differ from NewAttr *) 
                  INC ( ActualLength ) 
                ; IF LSs = LSs2 
                  THEN (* Must shift things one element to the right. *) 
                    EnsureTextAttrArraySize ( ArrayRef , ActualLength )
                  ; INC ( LSs2 ) 
                  ; FOR RI := ActualLength - 1 TO LSs2 BY - 1 
                    DO 
                      ArrayRef ^ [ RI ] := ArrayRef ^ [ RI - 1 ]  
                    END (* FOR *) 
                  END (* IF *) 
                ; ArrayRef ^ [ LSs ] := NewAttr 
                ; ArrayRef ^ [ LSs ] . TaCharPos := StartPos 
                ; INC ( LSs ) 
                ; ArrayRef ^ [ LSs2 ] . TaCharPos := LToPos 
                END (* IF *) 
              END (* IF *) 
            (* Shift element at LSs2 and its successors into LSs *) 
            ; IF LSs < LSs2 
              THEN 
                FOR RI := LSs TO ActualLength - 1 
                DO ArrayRef ^ [ RI ] := ArrayRef ^ [ LSs2 ] 
                ; INC ( LSs2 )
                END (* FOR *) 
              END (* IF *) 
            END (* IF *) 
          ; EXIT 
          END (* IF *) 
        END (* LOOP *) 
      END (* IF *) 
    END SetTextAttr 

; PROCEDURE InsertTextAttr 
    ( VAR (* IN OUT *) ArrayRef : PaintHs . TextAttrArrayRefTyp
    ; VAR (* IN OUT *) ActualLength : PortTypes . Int32Typ  
    ; InsPos : LbeStd . LimitedCharNoTyp
    ; InsLen : LbeStd . LimitedCharNoTyp 
    ; LineLen : LbeStd . LimitedCharNoTyp 
    ; InsAttr : PaintHs . TextAttrTyp 
    ) 

  = VAR LSs : PortTypes . Int32Typ  
  ; VAR LSs2 : PortTypes . Int32Typ  

  ; BEGIN
      IF ArrayRef = NIL 
      THEN
        EnsureTextAttrArraySize ( ArrayRef , 2 )
      END (* IF *) 
    ; LSs := 0 
(* TODO: Maybe make this a binary search. *) 
    ; LOOP 
        IF LSs >= ActualLength 
        THEN (* Off the end of attrs.  Append a new element. *) 
          IF LSs > 0 
             AND PaintHs . TextAttrIsEqual 
                   ( ArrayRef ^ [ LSs - 1 ] , InsAttr ) 
          THEN (* Just let the last element cover the insertion. *) 
            EXIT 
          ELSE (* Append an element at the end. *)  
            INC ( ActualLength )
          ; EnsureTextAttrArraySize 
              ( ArrayRef , ActualLength + ORD ( InsPos < LineLen ) )
          ; ArrayRef ^ [ LSs ] := InsAttr  
          ; ArrayRef ^ [ LSs ] . TaCharPos := InsPos  
          ; IF InsPos < LineLen 
               AND LSs > 0 
                   (* If LSs = 0, we were in default attribute and don't need
                      to change it back to anything.
                   *) 
            THEN (* Back to previous attribute, for the rest of the line. *) 
              INC ( ActualLength )
            ; ArrayRef ^ [ LSs + 1 ] := ArrayRef ^ [ LSs - 1 ] 
            ; ArrayRef ^ [ LSs + 1 ] . TaCharPos := InsPos + InsLen  
            END (* IF *) 
          ; EXIT
          END (* IF *) 
        ELSIF InsPos > ArrayRef ^ [ LSs ] . TaCharPos  
        THEN (* Keep searching. *) 
          INC ( LSs ) 
        ELSIF ( LSs > 0 
                AND PaintHs . TextAttrIsEqual 
                      ( ArrayRef ^ [ LSs - 1 ] , InsAttr ) 
              ) OR ( LSs = 0 
                     AND PaintHs . TextAttrIsEqual 
                           ( PaintHs . TextAttrDefault , InsAttr ) 
                   ) 
        THEN (* Extend the range of element at LSs - 1 *) 
          WHILE LSs < ActualLength 
          DO INC ( ArrayRef ^ [ LSs ] . TaCharPos , InsLen ) 
          ; INC ( LSs ) 
          END 
        ; EXIT 
        ELSIF InsPos = ArrayRef ^ [ LSs ] . TaCharPos  
        THEN (* Inserting at the beginning of LSs *) 
          IF PaintHs . TextAttrIsEqual ( ArrayRef ^ [ LSs ] , InsAttr ) 
          THEN (* Extend the range of element at LSs. *) 
            INC ( LSs ) 
          ; WHILE LSs < ActualLength 
            DO INC ( ArrayRef ^ [ LSs ] . TaCharPos , InsLen ) 
            ; INC ( LSs ) 
            END 
          ; EXIT 
          ELSE (* Insert one new element, before the one at LSs. *) 
            LSs2 := ActualLength 
          ; INC ( ActualLength )
          ; EnsureTextAttrArraySize ( ArrayRef , ActualLength )
          ; WHILE LSs2 > LSs 
            DO 
              ArrayRef ^ [ LSs2 ] := ArrayRef ^ [ LSs2 - 1 ] 
            ; INC ( ArrayRef ^ [ LSs2 ] . TaCharPos , InsLen ) 
            ; DEC ( LSs2 ) 
            END 
          ; ArrayRef ^ [ LSs ] := InsAttr  
          ; ArrayRef ^ [ LSs ] . TaCharPos := InsPos  
          ; EXIT
          END (* IF *) 
        ELSE (* Must split the element at LSs - 1 into three. *) 
          INC ( ActualLength , 2 )
        ; EnsureTextAttrArraySize ( ArrayRef , ActualLength )
        ; LSs2 := ActualLength - 1  
        ; WHILE LSs2 > LSs + 1 
          DO 
            ArrayRef ^ [ LSs2 ] := ArrayRef ^ [ LSs2 - 2 ] 
          ; INC ( ArrayRef ^ [ LSs2 ] . TaCharPos , InsLen ) 
          ; DEC ( LSs2 ) 
          END 
        ; IF LSs = 0 
          THEN ArrayRef ^ [ LSs + 1 ] := PaintHs . TextAttrDefault 
          ELSE ArrayRef ^ [ LSs + 1 ] := ArrayRef ^ [ LSs - 1 ]  
          END (* IF *) 
        ; ArrayRef ^ [ LSs + 1 ] . TaCharPos := InsPos + InsLen   
        ; ArrayRef ^ [ LSs ] := InsAttr  
        ; ArrayRef ^ [ LSs ] . TaCharPos := InsPos  
        ; EXIT
        END (* IF *) 
      END (* LOOP *) 
    END InsertTextAttr 

; PROCEDURE DeleteTextAttr 
    ( ArrayRef : PaintHs . TextAttrArrayRefTyp
    ; VAR (* IN OUT *) ActualLength : PortTypes . Int32Typ  
    ; DelPos : LbeStd . LimitedCharNoTyp 
    ; DelLen : LbeStd . LimitedCharNoTyp 
    ; LineLen : LbeStd . LimitedCharNoTyp 
    ) 

  = VAR LNewSs : PortTypes . Int32Typ  
  ; VAR LOldSs : PortTypes . Int32Typ  
  ; VAR LDelPos : LbeStd . LimitedCharNoTyp 
  ; VAR LDelLen : LbeStd . LimitedCharNoTyp 
  ; VAR LReduction : LbeStd . LimitedCharNoTyp 

  ; BEGIN 
      IF ArrayRef # NIL AND ActualLength > 0 
      THEN 
        LDelPos := DelPos 
      ; LDelLen := DelLen 
      ; LOldSs := 0 
      ; WHILE LOldSs + 1 < ActualLength  
              AND LDelPos >= ArrayRef ^ [ LOldSs + 1 ] . TaCharPos 
        DO INC ( LOldSs ) 
        END (* WHILE *) 
      ; LNewSs := LOldSs 
      ; LOOP 
          IF LOldSs + 1 >= ActualLength 
          THEN (* Inside the last range. *) 
            IF LDelPos = ArrayRef ^ [ LOldSs ] . TaCharPos 
               AND LDelPos + LDelLen = LineLen 
            THEN (* All of the range is deleted. *) 
              ActualLength := LOldSs 
            ; EXIT (* Without putting this range into the result. *) 
            ELSE
              ArrayRef ^ [ LNewSs ] := ArrayRef ^ [ LOldSs ] 
            ; DEC ( ArrayRef ^ [ LNewSs ] . TaCharPos , DelLen - LDelLen ) 
            ; ActualLength := LNewSs + 1  
            ; EXIT 
            END (* IF *)   
          ELSE (* There is a range at LOldSs + 1 *) 
            IF LDelLen <= 0 
            THEN (* Deletions were finished earlier. *) 
              ArrayRef ^ [ LNewSs ] := ArrayRef ^ [ LOldSs ] 
            ; DEC ( ArrayRef ^ [ LNewSs ] . TaCharPos , DelLen - LDelLen ) 
            ; INC ( LNewSs ) 
            ; INC ( LOldSs ) 
            ELSE (* More chars to delete. *) 
              LReduction 
                := MIN ( LDelLen 
                       , ArrayRef ^ [ LOldSs + 1 ] . TaCharPos - LDelPos 
                       ) 
            ; IF LReduction 
                 = ArrayRef ^ [ LOldSs + 1 ] . TaCharPos 
                   - ArrayRef ^ [ LOldSs ] . TaCharPos 
              THEN (* This entire range is deleted. *) 
                IF LOldSs >= 1 
                   AND PaintHs . TextAttrIsEqual
                         ( ArrayRef ^ [ LOldSs - 1 ] 
                         , ArrayRef ^ [ LOldSs + 1 ] 
                         ) 
                THEN (* Ranges to either side have same attributes. *)  
                  INC ( LOldSs , 2 ) 
                ; DEC ( LDelLen , LReduction ) 
                ; INC ( LDelPos , LReduction ) 
                ; IF LOldSs >= ActualLength 
                  THEN
                    ActualLength := LNewSs 
                  ; EXIT 
                  END (* IF *)  
                ELSE (* Delete just this range. *) 
                  INC ( LOldSs ) 
                ; DEC ( LDelLen , LReduction ) 
                ; INC ( LDelPos , LReduction ) 
                END (* IF *) 
              ELSE (* Shorten this range *) 
                ArrayRef ^ [ LNewSs ] := ArrayRef ^ [ LOldSs ] 
              ; DEC ( ArrayRef ^ [ LNewSs ] . TaCharPos , DelLen - LDelLen ) 
              ; INC ( LNewSs ) 
              ; INC ( LOldSs ) 
              ; DEC ( LDelLen , LReduction ) 
              ; INC ( LDelPos , LReduction ) 
              END (* IF *) 
            END (* IF *) 
          END (* IF*)  
        END (* LOOP*) 
      END (* IF *) 
    END DeleteTextAttr 

; PROCEDURE GrabTempEditRef 
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; Mark : PaintHs . LineMarkMeatTyp 
    ; RmFromPos : LbeStd . CharNoTyp 
      (* The FromPos of the previously-edited region, if any, of the returned 
         TempEditRef, must be no farther right than this.  *) 
    ; LmToPos : LbeStd . CharNoTyp 
      (* The ToPos of the previously-edited region, if any, of the returned 
         TempEditRef, must be no farther left than this.  *) 
    ; VAR (* OUT *) TempEditRef : PaintHs . TempEditRefTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* GrapTempEditRef could cause the contents of some marks to change, as one
     effect of calling IfteFlushTempEdit.  This could change the LinesRef that
     callers want to initialize TempEditRef with.  So it just sets 
     TempEditRef . TeLinesRef to NIL, to indicate to the caller that it must
     do this.  Caller can use MaybeInitTempEditRef to do it.
  *) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 

  ; BEGIN (* GrabTempEditRef *) 
      LImagePers := ImageTrans . ItPers 
    ; IF LImagePers . IpTempEditRef = NIL 
      THEN 
        LImagePers . IpTempEditRef := NEW ( PaintHs . TempEditRefTyp ) 
      ; TempEditRef := LImagePers . IpTempEditRef 
      ; InitTempEditForText 
          ( TempEditRef , Mark . LmLinesRef , Mark . LmLineNo ) 
      ELSE 
        TempEditRef := LImagePers . IpTempEditRef 
      ; CASE LImagePers . IpTempEditState 
        OF PaintHs . TempEditStateTyp . TeStateSynt 
        => SyntEdit . FlushEdit ( ImageTrans ) 
        ; InitTempEditForText 
            ( TempEditRef , Mark . LmLinesRef , Mark . LmLineNo ) 
        | PaintHs . TempEditStateTyp . TeStateText 
        => IF TempEditRef . TeLinesRef # Mark . LmLinesRef 
              (* Different LinesRef *) 
              OR TempEditRef . TeLineNo # Mark . LmLineNo 
                 (* Different line w/in block of blank lines. *) 
              OR TempEditRef . TeDelFromPos 
                 # LbeStd . LimitedCharNoInfinity 
                 AND ( TempEditRef . TeDelFromPos > RmFromPos  
                       OR TempEditRef . TeDelFromPos 
                          + TempEditRef . TeInsLen 
                          < LmToPos 
                     ) 
                 (* Outside the already edited region. *) 
           THEN 
             InnerFlushTempEdit 
               ( ImageTrans , TempEditRef . TeLinesRef , TempEditRef ) 
           ; InitTempEditForText 
               ( TempEditRef , Mark . LmLinesRef , Mark . LmLineNo ) 
           END (* IF *) 
        | PaintHs . TempEditStateTyp . TeStateIdle 
        => InitTempEditForText 
             ( TempEditRef , Mark . LmLinesRef , Mark . LmLineNo ) 
        END (* CASE IpTempEditState *) 
      END (* IF *) 
    ; LImagePers . IpTempEditState 
        := PaintHs . TempEditStateTyp . TeStateText 
    ; Display . NoteImageSavedState ( ImageTrans , FALSE ) 
    ; Display . NoteImageParsedState ( ImageTrans , FALSE ) 
    ; Display . NoteImageAnalyzedState ( ImageTrans , FALSE ) 
    END GrabTempEditRef 

; PROCEDURE AdjustMarksOnLine 
    ( Mark : PaintHs . LineMarkMeatTyp 
    ; Adjustment : LbeStd . CharNoTyp 
    ) 
  (* All marks on the same line but with CharPos greater than that of 
     Mark have Adjustment added to their CharPos field.
  *) 

  = VAR LMark : PaintHs . LineMarkMeatTyp 

  ; BEGIN 
      IF Mark # NIL (* Paranoia. *) 
      THEN
        LMark := Mark 
      ; LOOP (* Thru irrelevant marks on this line. *) 
        (* INVARIANT: LMark is not a mark to be adjusted. *) 
          TYPECASE LMark . LmRightLink <* NOWARN *> 
          OF PaintHs . LineMarkHeaderTyp 
          => (* No marks remain at all. *) 
             EXIT 
          | PaintHs . LineMarkMeatTyp ( TMark ) 
          => LMark := TMark  
          ; IF NOT Marks . Equal ( LMark . LmTokMark , Mark . LmTokMark ) 
               OR LMark . LmLineNo > Mark . LmLineNo 
            THEN (* No relevant marks remain on this line. *) 
              EXIT 
            ELSIF LMark . LmCharPos > Mark . LmCharPos 
            THEN (* This mark is on same line and greater LmCharPos. *) 
              LOOP (* Thru remaining marks on this line. *) 
                INC ( LMark . LmCharPos , Adjustment ) 
              ; TYPECASE LMark . LmRightLink <* NOWARN *> 
                OF PaintHs . LineMarkHeaderTyp 
                => (* No marks remain at all. *) 
                   EXIT 
                | PaintHs . LineMarkMeatTyp ( TMark2 ) 
                => LMark := TMark2  
                ; IF NOT Marks . Equal 
                           ( LMark . LmTokMark , Mark . LmTokMark ) 
                     OR LMark . LmLineNo > Mark . LmLineNo 
                  THEN (* No relevant marks remain on this line. *) 
                    EXIT 
                  ELSE (* Loop. *) 
                  END (* IF *) 
                END (* TYPECASE *) 
              END (* LOOP *) 
            ; EXIT 
            ELSE (* This is a mark on the same line, too low an LmCharPos. *) 
            END (* IF *) 
          END (* TYPECASE *) 
        END (* LOOP *) 
      END (* IF *) 
    END AdjustMarksOnLine 

; PROCEDURE AssertTempEdit 
    ( TempEditRef : PaintHs . TempEditRefTyp 
    ; SuccLinesRef : PaintHs . LinesRefMeatTyp := NIL  
    ) 
  RAISES { Backout } 
  (* Assert that length of TempEditRef is consistent. *) 

  = VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LSuccLength : LbeStd . CharNoTyp 

  ; BEGIN 
      LLinesRef := TempEditRef . TeLinesRef 
    ; IF SuccLinesRef # NIL AND NOT SuccLinesRef . LrGapAfter 
      THEN
        LSuccLength := SuccLinesRef . LrLineLen 
      ELSE 
        LSuccLength := 0 
      END (* IF *) 
    ; Assert
        ( LLinesRef . LrLineLen 
          - ( TempEditRef . TeDelToPos - TempEditRef . TeDelFromPos ) 
          + TempEditRef . TeInsLen 
          + LSuccLength 
          = Strings . Length ( TempEditRef . TeEditedString ) 
        , AFT . A_AssertTempEdit_Wrong_length
        )  
    END AssertTempEdit 

; CONST CharPoint01 = EditWindow . CharPointTyp { 0 , 1 } 
; CONST CharPoint0Minus1 = EditWindow . CharPointTyp { 0 , - 1 } 

; PROCEDURE SimpleDeleteChar 
    ( ImageTrans : PaintHs . ImageTransientTyp  
    ; Mark : PaintHs . LineMarkMeatTyp 
    ; VAR (* IN OUT *) MustRepaintWindow : BOOLEAN 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LTempEditLength : LbeStd . LimitedCharNoTyp 
  ; VAR LSuccLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LSavedTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LSavedTempEditState : PaintHs . TempEditStateTyp 
  ; VAR LTrailingBlankCount : Strings . StringSsTyp 

  ; BEGIN (* SimpleDeleteChar *) 
      LImagePers := ImageTrans . ItPers 
    ; GrabTempEditRef 
        ( ImageTrans 
        , Mark 
        , RmFromPos := Mark . LmCharPos + 1 
        , LmToPos := Mark . LmCharPos 
        , (* VAR *) TempEditRef := LTempEditRef 
        ) 
    ; LTempEditLength 
        := Strings . Length ( LTempEditRef . TeEditedString ) 
    ; IF Mark . LmCharPos >= LTempEditLength 
      THEN (* Past last char of line, delete the Nl at end of line. *) 
        IF Display . LinesRefIsEndOfImage ( ImageTrans , Mark . LmLinesRef ) 
        THEN (* Deleting Nl beyond EOI.  Do nothing. *) 
        ELSE 
          Display . SecureSucc ( ImageTrans , Mark . LmLinesRef ) 
        ; LSuccLinesRef := Mark . LmLinesRef . LrRightLink 
          (* ^NARROW can't fail. *) 
        ; IF Display . LinesRefIsEndOfImage ( ImageTrans , LSuccLinesRef ) 
          THEN (* Deleting Nl at EOI.  Do nothing. *) 
          ELSIF LTempEditLength + LSuccLinesRef . LrLineLen 
                > LbeStd . LimitedCharNoMax 
          THEN (* The joined line would be too long. Refuse. *) 
            Display . Beep ( Errors . ErrorTyp . EJoinLinesLineTooLong ) 
(* TODO: It would be good to rework so we make this test before grabbing the 
 temp edit rec and before moving the cursor in DeleteChar in the backwards 
 case. 
*)   
          ELSE (* Delete Nl at end of line, appending successor line. *) 
            (* We could be in TeStateIdle here. *) 
            LSavedTempEditRef := CopyOfTempEditRef ( LTempEditRef ) 
          ; LSavedTempEditState := LImagePers . IpTempEditState 
          ; TRY 
              IF LTempEditRef . TeDelFromPos 
                 = LbeStd . LimitedCharNoInfinity 
              THEN (* No changes have been made yet to this line. *) 
                LTempEditRef . TeDelFromPos := LTempEditLength 
              ; LTempEditRef . TeDelToPos := LTempEditLength 
              END (* IF *) 
            ; Assert 
                ( LTempEditRef . TeDelToPos = Mark . LmLinesRef . LrLineLen 
                , AFT . A_SimpleDeleteChar_DelNlOutOfRange 
                ) 
            ; LTrailingBlankCount  
                := MAX ( 0 
                       , Mark . LmCharPos - LTempEditLength 
                       )  
            ; TRY 
                Strings . InsertBlanksInPlace 
                  ( LTempEditRef . TeEditedString 
                  , PrefixLength := LTempEditLength 
                  , BlankCount 
                      := LTrailingBlankCount + LSuccLinesRef . LrFromPos 
                  , EventualLengthHint 
                      := LTempEditLength + LSuccLinesRef . LrLineLen 
                  )
              EXCEPT Strings . SsOutOfBounds
              => CantHappen 
                   ( AFT . A_SimpleDeleteChar_String_subscript_out_of_bounds ) 
              END (* TRY EXCEPT *) 
            ; INC ( LTempEditRef . TeInsLen , LTrailingBlankCount )  
            ; Strings . AppendTextInPlace 
                ( LTempEditRef . TeEditedString 
                , LSuccLinesRef . LrLineText 
                ) 
         (* ; INC ( LTempEditRef . TeInsLen 
                  , Text . Length ( LSuccLinesRef . LrLineText ) 
                  ) 
         *) 
            ; AssertTempEdit ( LTempEditRef , LSuccLinesRef ) 
            ; InnerFlushTempEdit 
                ( ImageTrans 
                , Mark . LmLinesRef 
                , LTempEditRef 
                , DelNlShift := Mark . LmCharPos 
                ) 
            ; MustRepaintWindow := TRUE 
            ; InitTempEditForText ( LTempEditRef , NIL , 0 ) 
            ; LImagePers . IpTempEditState 
                := PaintHs . TempEditStateTyp . TeStateIdle 
            ; DEC ( LImagePers . IpLineCtDisplay ) 
            EXCEPT 
            Backout ( EMessage ) 
            => (* Rollback changes to temp edit. *)
              LImagePers . IpTempEditRef := LSavedTempEditRef 
            ; LImagePers . IpTempEditState 
                := LSavedTempEditState  
            ; AssertDevel . WriteCheckpoint 
                ( ImageRef := ImageTrans 
                , Message := EMessage  
                , DoCreateVersion := FALSE 
                ) 
              (* ^This will rewrite the checkpoint already written from 
                  inside AssertDevel, but with the changes undone. *) 
            ; RAISE Backout ( EMessage ) 
            END (* TRY EXCEPT *) 
          END (* IF *) 
        END (* IF *) 
      ELSE (* Delete char within the line. *) 
        LSavedTempEditRef := CopyOfTempEditRef ( LTempEditRef ) 
      ; LSavedTempEditState := LImagePers . IpTempEditState 
      ; TRY 
          IF LTempEditRef . TeDelFromPos = LbeStd . LimitedCharNoInfinity 
          THEN (* No changes have been made yet to this line. *) 
            LTempEditRef . TeDelFromPos := Mark . LmCharPos 
          ; LTempEditRef . TeDelToPos := Mark . LmCharPos + 1 
          ELSIF Mark . LmCharPos + 1 = LTempEditRef . TeDelFromPos 
          THEN (* Deleting char immediately to left of current deleted 
                  region. *) 
            DEC ( LTempEditRef . TeDelFromPos ) 
          ELSIF Mark . LmCharPos 
                = ( LTempEditRef . TeDelFromPos + LTempEditRef . TeInsLen )
          THEN (* Deleting char immediately to right of current deleted 
                  region. *) 
            INC ( LTempEditRef . TeDelToPos ) 
          ELSIF Mark . LmCharPos >= LTempEditRef . TeDelFromPos 
                AND Mark . LmCharPos 
                    < LTempEditRef . TeDelFromPos + LTempEditRef . TeInsLen 
          THEN (* Deleting within the inserted text. *) 
            DEC ( LTempEditRef . TeInsLen ) 
          ELSE (* Outside current region.  Flush and start over. *) 
            InnerFlushTempEdit 
              ( ImageTrans 
              , Mark . LmLinesRef 
              , LTempEditRef 
              ) 
          ; InitTempEditForText 
              ( LTempEditRef 
              , LTempEditRef . TeLinesRef 
              , LTempEditRef . TeLineNo 
              ) 
          ; LTempEditRef . TeDelFromPos := Mark . LmCharPos 
          ; LTempEditRef . TeDelToPos := Mark . LmCharPos + 1 
          END (* IF *) 
        ; TRY 
            Strings . DeleteCharsInPlace 
              ( LTempEditRef . TeEditedString , Mark . LmCharPos , 1 )
          EXCEPT Strings . SsOutOfBounds
          => CantHappen 
               ( AFT . A_SimpleDeleteChar_String_subscript_out_of_bounds ) 
          END (* TRY EXCEPT *) 
        ; DeleteTextAttr 
            ( LTempEditRef . TeTextAttrArrayRef 
            , LTempEditRef . TeTextAttrActualSize 
            , Mark . LmCharPos 
            , 1 
            , LTempEditLength 
            ) 
        ; AssertTempEdit ( LTempEditRef ) 
        ; AdjustMarksOnLine ( Mark , - 1 ) 
        ; LImagePers . IpTempEditState 
            := PaintHs . TempEditStateTyp . TeStateText 
        ; Display . NoteImageSavedState ( ImageTrans , FALSE ) 
        ; Display . NoteImageParsedState ( ImageTrans , FALSE ) 
        ; Display . NoteImageAnalyzedState ( ImageTrans , FALSE ) 
        ; IF NOT MustRepaintWindow 
          THEN 
            PaintTempEditedLineInAllWindows 
              ( ImageTrans , Mark . LmLinesRef , LTempEditRef ) 
          END (* IF *) 
        EXCEPT 
        Backout ( EMessage ) 
        => (* Rollback changes to temp edit. *)
          LImagePers . IpTempEditRef := LSavedTempEditRef 
        ; LImagePers . IpTempEditState := LSavedTempEditState  
        ; AssertDevel . WriteCheckpoint 
            ( ImageRef := ImageTrans 
            , Message := EMessage  
            , DoCreateVersion := FALSE 
            ) 
          (* ^This will rewrite the checkpoint already written from 
              inside AssertDevel, but with the changes undone. *) 
        ; RAISE Backout ( EMessage ) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END SimpleDeleteChar 

(* EXPORTED: *)
; PROCEDURE DeleteChar 
    ( WindowRef : PaintHs . WindowRefTyp ; DeletingBwd : BOOLEAN ) 
  RAISES { Backout , Thread . Alerted } 

(* TODO: Update versions when no new tree is built.  
         (InnerFlushTempEdit does it when tree _is_ rebuilt.) 
*) 

  = VAR LMustRepaintWindow : BOOLEAN 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LActualMovement : EditWindow . CharPointTyp 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 

  ; BEGIN (* DeleteChar *) 
      LMustRepaintWindow := FALSE 
    ; IF WindowRef . WrImageRef # NIL 
      THEN 
        LImagePers := WindowRef . WrImageRef . ItPers  
      ; WITH 
          WCursorMark 
          = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
        DO 
          IF WCursorMark . LmLinesRef # NIL 
          THEN (* An image is open in the window *) 
            (* The following tests will prevent grabbing the TempEditRef 
               away from some other line, or creating a new one, in cases 
               where we can tell beforehand that the delete doesn't do 
               anything anyway. *) 
            IF DeletingBwd 
            THEN 
              IF WCursorMark . LmCharPos = 0 
              THEN (* Trying to join with previous line. *) 
                IF Options . DelNlAtBOL 
                   AND NOT Display . LinesRefIsBegOfImage 
                             ( WindowRef . WrImageRef 
                             , WCursorMark . LmLinesRef 
                             ) 
                THEN (* Can do the join. Simulate delete the char at the 
                        end of the previous line. *) 
(* TODO: Surely, this can all be done in one call on MoveCursorWindowRef: *) 
                  Display . MoveCursorWindowRef 
                    ( WindowRef 
                    , CharPoint0Minus1 
                    , LActualMovement 
                    , LMustRepaintWindow 
                    ) 
                (* WCursorMark will have contents changed, but same object. *) 
                ; Assert 
                    ( LActualMovement = CharPoint0Minus1 
                    , AFT . A_DeleteCharDelNlAtBol 
                    ) 
                ; Display . HorizMoveCursorWindowRef 
                    ( WindowRef 
                    , WCursorMark . LmLinesRef . LrLineLen 
                    , LMustRepaintWindow 
                    ) 
                ; TRY 
                    SimpleDeleteChar 
                      ( WindowRef . WrImageRef  
                      , WCursorMark 
                      , LMustRepaintWindow 
                      ) 
                  EXCEPT 
                  Backout ( EMessage ) 
                  => (* Rollback cursor move. *)
                    Display . HorizMoveCursorWindowRef 
                      ( WindowRef 
                      , - WCursorMark . LmLinesRef . LrLineLen 
                      , LMustRepaintWindow 
                      ) 
                  ; Display . MoveCursorWindowRef 
                      ( WindowRef 
                      , CharPoint01 
                      , LActualMovement 
                      , LMustRepaintWindow 
                      ) 
                  ; AssertDevel . WriteCheckpoint 
                      ( ImageRef := WindowRef . WrImageRef 
                      , Message := EMessage  
                      , DoCreateVersion := FALSE 
                      ) 
                    (* ^We're getting pretty extravagant here, as the 
                       checkpoint file could already have been written 
                       twice, this making three.  *)
                  ; LMustRepaintWindow := TRUE 
                  ; RAISE Backout ( EMessage ) 
                  END (* TRY EXCEPT *) 
                ELSE (* Can't join lines backward. *) 
                  Display . Beep ( Errors . ErrorTyp . EJoinLinesAtBOL ) 
                ; RETURN (* Nothing has changed. *) 
                END (* IF *) 
              ELSIF ( LImagePers . IpTempEditState 
                      = PaintHs . TempEditStateTyp . TeStateIdle 
                      OR LImagePers . IpTempEditRef . TeLinesRef 
                         # WCursorMark . LmLinesRef 
                      OR LImagePers . IpTempEditRef . TeLineNo 
                         # WCursorMark . LmLineNo 
                    ) 
                    AND WCursorMark . LmCharPos 
                        > WCursorMark . LmLinesRef . LrLineLen 
              THEN (* Beyond EOL. No effect except cursor movement. *) 
                Display . HorizMoveCursorWindowRef 
                  ( WindowRef , - 1 , LMustRepaintWindow ) 
              ; IF NOT LMustRepaintWindow 
                THEN 
                  EditWindow . SetCursorPosition 
                    ( WindowRef 
                    , WCursorMark . LmCharPos - WindowRef . WrHorizScroll 
                    , WindowRef . WrCursorLineNoInWindow 
                    ) 
                ; EditWindow . PaintCursorCoordinates ( WindowRef ) 
                END (* IF *) 
              ELSE (* Plain delete backward. *) 
                Display . HorizMoveCursorWindowRef 
                  ( WindowRef , - 1 , LMustRepaintWindow ) 
              ; TRY 
                  SimpleDeleteChar 
                    ( WindowRef . WrImageRef  
                    , WCursorMark 
                    , LMustRepaintWindow 
                    ) 
                EXCEPT 
                Backout ( EMessage ) 
                => (* Rollback cursor move. *)
                  Display . HorizMoveCursorWindowRef 
                    ( WindowRef , 1 , LMustRepaintWindow ) 
                ; AssertDevel . WriteCheckpoint 
                    ( ImageRef := WindowRef . WrImageRef 
                    , Message := EMessage  
                    , DoCreateVersion := FALSE 
                    ) 
                  (* We're getting pretty extravagant here, as the checkpoint
                     file could already have been written twice, this
                     making three.  *)
                ; LMustRepaintWindow := TRUE 
                ; RAISE Backout ( EMessage ) 
                END (* TRY EXCEPT *)               
              END (* IF *) 
            ELSE (* Deleting forward *) 
              IF Display . LinesRefIsEndOfImage 
                   ( WindowRef . WrImageRef , WCursorMark . LmLinesRef ) 
                 AND ( LImagePers . IpTempEditState 
                       = PaintHs . TempEditStateTyp . TeStateIdle 
                       OR LImagePers . IpTempEditRef . TeLinesRef 
                          # WCursorMark . LmLinesRef 
                       OR WCursorMark . LmLineNo 
                          > LImagePers . IpTempEditRef . TeLineNo 
                     ) 
              THEN (* We know we are out beyond all text of the image. 
                      there is no action. *) 
                RETURN 
              ELSE 
                SimpleDeleteChar 
                  ( WindowRef . WrImageRef  
                  , WCursorMark 
                  , LMustRepaintWindow 
                  ) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* WITH WCursorMark *) 
      END (* IF *) 
    ; IF LMustRepaintWindow 
      THEN 
        Display . PaintWindowFromLines 
          ( WindowRef  
          , (* VAR *) LTrailingBlankLines (* Dead. *) 
          , (* VAR *) LLinesRef (* Dead. *) 
          , (* VAR *) LLineNo  (* Dead. *) 
          )
(* CHECK: 1) can we paint only this line? 
        2) shouldn't we just mark the window for repainting? 
*)      
      END (* IF *) 
    END DeleteChar 

(* EXPORTED: *)
; PROCEDURE InsertOrOverlayChar 
    ( WindowRef : PaintHs . WindowRefTyp 
    ; NewChar : CHAR 
    ; IsInsert : BOOLEAN (* Otherwise Overlay *) 
    ) 
    RAISES { Backout , Thread . Alerted } 

(* TODO: versions when no new tree is built.  (InnerFlushTempEdit does it when 
   tree _is_.) *) 

  = VAR LImageTrans : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LIndentPos : LbeStd . LimitedCharNoTyp 
  ; VAR LActualMovement : EditWindow . CharPointTyp 
  ; VAR LLength : Strings . StringSsTyp 
  ; VAR LInsLength : Strings . StringSsTyp 
  ; VAR LTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LMustRepaintWindow : BOOLEAN 
  ; VAR LDoInBlankLineBefore : BOOLEAN 
  ; VAR LSavedTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LSavedTempEditState : PaintHs . TempEditStateTyp 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 

  ; BEGIN (* InsertOrOverlayChar *) 
      LMustRepaintWindow := FALSE 
    ; IF WindowRef # NIL 
      THEN 
        LImageTrans := WindowRef . WrImageRef 
      ; IF LImageTrans # NIL 
        THEN 
          LImagePers := LImageTrans . ItPers 
; PaintHs . BruteForceVerifyLineMarks 
                       ( WindowRef . WrImageRef ) 
        ; WITH 
            WCursorMark 
            = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
          DO IF ( NewChar = ' ' OR NewChar = Ascii . ht ) 
                AND ( LImagePers . IpTempEditState 
                      = PaintHs . TempEditStateTyp . TeStateIdle 
                      OR LImagePers . IpTempEditRef . TeLinesRef 
                         # WCursorMark . LmLinesRef 
                      OR LImagePers . IpTempEditRef . TeLineNo 
                         # WCursorMark . LmLineNo 
                    ) 
                AND WCursorMark . LmCharPos 
                    >= WCursorMark . LmLinesRef . LrLineLen 
             THEN (* Insert/overlay with blank(s), beyond EOL. *) 
               Display . HorizMoveCursorWindowRef 
                 ( WindowRef , 1 , LMustRepaintWindow ) 
             ELSE 
               IF NewChar = LbeStd . CharNewLine 
                  OR NewChar = LbeStd . CharReturn  
               THEN (* Inserted new line. *) 
                 LDoInBlankLineBefore := FALSE 
               ; IF WCursorMark . LmLinesRef . LrBolTokMark . Kind 
                    # MarkKindTyp . BlankLine 
                    AND WCursorMark . LmCharPos 
                        <= WCursorMark . LmLinesRef . LrFromPos 
                 THEN 
                   TYPECASE WCursorMark . LmLinesRef . LrLeftLink 
                   OF PaintHs . LinesRefMeatTyp ( TPredLinesRef ) 
                   => IF NOT TPredLinesRef . LrGapAfter 
                         AND TPredLinesRef . LrBolTokMark . Kind 
                             = MarkKindTyp . BlankLine 
                         AND NOT TPredLinesRef . LrBolTokMark . StartAtEnd 
                      THEN 
                        LDoInBlankLineBefore := TRUE 
                      END (* IF *) 
                   ELSE 
                   END (* TYPECASE *) 
                 END (* IF *) 
               ; IF LDoInBlankLineBefore 
                 THEN (* Avoid multiple, successive blank line mods. 
                         Move cursor up a line. *) 
                   Display . MoveCursorWindowRef 
                     ( WindowRef 
                     , CharPoint0Minus1 
                     , LActualMovement 
                     , LMustRepaintWindow 
                     ) 
                   (* Contents of WCursorMark will change, but same object *) 
                 ; Assert 
                     ( LActualMovement = CharPoint0Minus1 
                     , AFT . A_InsertOrOverlayChar_MoveCursorUp 
                     ) 
                 END (* IF *) 

               ; GrabTempEditRef 
                   ( LImageTrans 
                   , WCursorMark 
                   , RmFromPos 
                       := WCursorMark . LmCharPos + ORD ( NOT IsInsert ) 
                   , LmToPos := WCursorMark . LmCharPos 
                   , (* VAR *) TempEditRef := LTempEditRef 
                   ) 
               ; LSavedTempEditRef := CopyOfTempEditRef ( LTempEditRef ) 
               ; LSavedTempEditState := LImagePers . IpTempEditState 
               ; TRY 
                   IF LTempEditRef . TeDelFromPos 
                      = LbeStd . LimitedCharNoInfinity 
                   THEN (* No changes have been made yet to this line. *) 
                     LTempEditRef . TeDelFromPos := WCursorMark . LmCharPos 
                   ; LTempEditRef . TeDelToPos := WCursorMark . LmCharPos 
                   ; IF NewChar = LbeStd . CharReturn
                     THEN LIndentPos := LTempEditRef . TeDelFromPos 
(* FIX: ^Decide what the rules here should really be. *) 
                     ELSE LIndentPos := 0 
                     END (* IF *)
                   ELSIF WCursorMark . LmCharPos < LTempEditRef . TeDelFromPos 
                         OR WCursorMark . LmCharPos 
                            > LTempEditRef . TeDelFromPos 
                              + LTempEditRef . TeInsLen 
                   THEN (* Inserted Nl can not be combined with existing 
                           temp edits. *) 
                     InnerFlushTempEdit 
                       ( LImageTrans 
                       , WCursorMark . LmLinesRef 
                       , LTempEditRef 
                       ) 
                   ; InitTempEditForText 
                       ( LTempEditRef 
                       , LTempEditRef . TeLinesRef 
                       , LTempEditRef . TeLineNo 
                       ) 
                   ; LTempEditRef . TeDelFromPos := WCursorMark . LmCharPos 
                   ; LTempEditRef . TeDelToPos := WCursorMark . LmCharPos 
                   ; IF NewChar = LbeStd . CharReturn 
                     THEN LIndentPos := LTempEditRef . TeDelFromPos 
(* FIX: ^Decide what the rules here should really be. *) 
                     ELSE LIndentPos := 0 
                     END (* IF *)
                   ELSE (* Can combine inserted Nl with existing edits. *)  
                     IF WCursorMark . LmCharPos 
                        <= WCursorMark . LmLinesRef . LrFromPos 
                     THEN (* Cursor in white space on left of line. *)  
                       LIndentPos := WCursorMark . LmCharPos 
                     ELSIF WCursorMark . LmLinesRef . LrBolTokMark . Kind 
                           = MarkKindTyp . BlankLine 
                           AND NOT WCursorMark . LmLinesRef . LrBolTokMark 
                                   . StartAtEnd 
                     THEN (* Cursor inside blank line(s). *) 
                       IF LTempEditRef . TeDelFromPos 
                          = LbeStd . LimitedCharNoInfinity 
                       THEN (* No changes have been made yet to this line. *) 
                         LIndentPos := WCursorMark . LmCharPos 
                       ELSE 
                         LIndentPos := LTempEditRef . TeDelFromPos 
                       END (* IF *) 
                     ELSE 
                       LIndentPos 
                         := TravUtil . IndentPosOfBolTokMark 
                              ( LImagePers . IpLang 
                              , LImagePers . IpEstRoot 
                              , WCursorMark . LmLinesRef . LrBolTokMark 
                              ) 
                     END (* IF *) 
                   ; LIndentPos := LTempEditRef . TeDelFromPos 
(* FIX: ^Decide what the rules here should really be. *) 
                   END (* IF *) 
                 ; InnerFlushTempEdit 
                     ( LImageTrans 
                     , WCursorMark . LmLinesRef 
                     , LTempEditRef 
                     , InsNlPos := WCursorMark . LmCharPos 
                     , NlIndentPos := LIndentPos 
                     ) 

                 ; LMustRepaintWindow := TRUE 
                 ; InitTempEditForText ( LTempEditRef , NIL , 0 ) 
                 ; LImagePers . IpTempEditState 
                     := PaintHs . TempEditStateTyp . TeStateIdle 
(*               ; LWantedMovement . h := LIndentPos - WCursorMark . LmCharPos 
                 ; LWantedMovement . v := 1 + ORD ( LDoInBlankLineBefore ) 
                 ; Display . MoveCursorWindowRef 
                     ( WindowRef 
                     , LWantedMovement 
                     , LActualMovement 
                     , LMustRepaintWindow 
                     ) 
                 ; Assert 
                     ( LActualMovement = LWantedMovement 
                     , AFT . A_InsertOrOverlayChar_MoveCursorNewLine 
                     ) 
*) 
                 ; INC ( LImagePers . IpLineCtDisplay ) 
                 EXCEPT 
                 Backout ( EMessage ) 
                 => (* Rollback changes to temp edit. *)
                   LImagePers . IpTempEditRef := LSavedTempEditRef 
                 ; LImagePers . IpTempEditState 
                     := LSavedTempEditState  
                 ; IF LDoInBlankLineBefore 
                   THEN 
                     Display . MoveCursorWindowRef 
                       ( WindowRef 
                       , CharPoint01 
                       , LActualMovement 
                       , LMustRepaintWindow 
                       ) 
                   END (* IF *) 
                 ; AssertDevel . WriteCheckpoint 
                     ( ImageRef := LImageTrans 
                     , Message := EMessage  
                     , DoCreateVersion := FALSE 
                     ) 
                   (* ^This will rewrite the checkpoint already written from 
                       inside AssertDevel, but with the changes undone. *) 
                 ; RAISE Backout ( EMessage ) 
                 END (* TRY EXCEPT *) 
               ELSE (* Not a new line inserted. *) 
                 GrabTempEditRef 
                   ( LImageTrans 
                   , WCursorMark 
                   , RmFromPos 
                       := WCursorMark . LmCharPos + ORD ( NOT IsInsert ) 
                   , LmToPos := WCursorMark . LmCharPos 
                   , (* VAR *) TempEditRef := LTempEditRef 
                   ) 
               ; LLength := Strings . Length ( LTempEditRef . TeEditedString ) 
               ; IF IsInsert AND LLength = LbeStd . LimitedCharNoMax 
                 THEN (* Inserted Char won't fit on a maximum length line. *) 
                   Display . Beep 
                     ( Errors . ErrorTyp . EInsertCharLineTooLong ) 
                 ELSE 
                   LSavedTempEditRef := CopyOfTempEditRef ( LTempEditRef ) 
                 ; LSavedTempEditState := LImagePers . IpTempEditState 
                 ; TRY 
                     IF LTempEditRef . TeDelFromPos 
                        = LbeStd . LimitedCharNoInfinity 
                     THEN (* No changes have been made yet to this line. *) 
                       LTempEditRef . TeDelFromPos 
                         := MIN ( LLength , WCursorMark . LmCharPos ) 
                     ; LTempEditRef . TeDelToPos 
                         := LTempEditRef . TeDelFromPos  
                     ELSIF WCursorMark . LmCharPos 
                           < LTempEditRef . TeDelFromPos - ORD ( NOT IsInsert )
                           OR WCursorMark . LmCharPos 
                              > LTempEditRef . TeDelFromPos 
                                + LTempEditRef . TeInsLen 
                     THEN (* Insertion cannot be combined with existing 
                                  temp edits. *) 
                       InnerFlushTempEdit 
                         ( LImageTrans 
                         , WCursorMark . LmLinesRef 
                         , LTempEditRef 
                         ) 
                     ; InitTempEditForText 
                         ( LTempEditRef 
                         , LTempEditRef . TeLinesRef 
                         , LTempEditRef . TeLineNo 
                         ) 
                     ; LTempEditRef . TeDelFromPos 
                         := MIN ( LLength , WCursorMark . LmCharPos ) 
                     ; LTempEditRef . TeDelToPos 
                         := LTempEditRef . TeDelFromPos  
                     END (* IF *) 
                   ; IF WCursorMark . LmCharPos >= LLength 
                     THEN (* Inserting/overlaying beyond end of line. *) 
                       LInsLength := WCursorMark . LmCharPos - LLength + 1 
                       (* + 1 to allow for to-be-inserted char. *) 
                     ; TRY 
                         Strings . InsertBlanksInPlace 
                           ( LTempEditRef . TeEditedString 
                           , LLength 
                           , LInsLength 
                           ) 
                       EXCEPT Strings . SsOutOfBounds
                       => CantHappen 
                            ( AFT . A_InsertOrOverlayChar_String_subscript_out_of_bounds_blanks_eol ) 
                       END (* TRY EXCEPT *) 
                     ; InsertTextAttr 
                         ( LTempEditRef . TeTextAttrArrayRef 
                         , LTempEditRef . TeTextAttrActualSize 
                         , WCursorMark . LmCharPos 
                         , LInsLength  
                         , LLength 
                         , PaintHs . TextAttrTyped  
                         ) 
                     ; INC ( LTempEditRef . TeInsLen , LInsLength ) 
                     ; AdjustMarksOnLine ( WCursorMark , 1 ) 
                       (* ^Can this matter beyond end of line? *) 
                     ELSIF IsInsert 
                     THEN 
                       TRY 
                         Strings . InsertBlanksInPlace 
                           ( LTempEditRef . TeEditedString 
                           , WCursorMark . LmCharPos 
                           , 1 
                           ) 
                       EXCEPT Strings . SsOutOfBounds
                       => CantHappen 
                            ( AFT . A_InsertOrOverlayChar_String_subscript_out_of_bounds_for_blanks ) 
                       END (* TRY EXCEPT *) 
                     ; InsertTextAttr 
                         ( LTempEditRef . TeTextAttrArrayRef 
                         , LTempEditRef . TeTextAttrActualSize 
                         , WCursorMark . LmCharPos 
                         , 1 
                         , LLength 
                         , PaintHs . TextAttrTyped  
                         ) 
                     ; INC ( LTempEditRef . TeInsLen ) 
                     ; AdjustMarksOnLine ( WCursorMark , 1 ) 
                     ELSE (* Overlay char within the previously edited line. *)
                       SetTextAttr 
                         ( LTempEditRef . TeTextAttrArrayRef 
                         , LTempEditRef . TeTextAttrActualSize 
                         , WCursorMark . LmCharPos 
                         , 1 
                         , LineLen := LLength 
                         , NewAttr := PaintHs . TextAttrTyped  
                         ) 
                     ; IF WCursorMark . LmCharPos 
                          = LTempEditRef . TeDelFromPos - 1 
                       THEN (* Char immediately left of previous edited region. *) 
                         DEC ( LTempEditRef . TeDelFromPos ) 
                       ; INC ( LTempEditRef . TeInsLen ) 
                       ELSIF WCursorMark . LmCharPos 
                              = LTempEditRef . TeDelFromPos 
                                + LTempEditRef . TeInsLen 
                       THEN (* Char immediately right of previous edited region. *) 
                         INC ( LTempEditRef . TeDelToPos ) 
                       ; INC ( LTempEditRef . TeInsLen ) 
                       END (* IF *) 
                     END (* IF *) 
                   ; TRY 
                       Strings . StoreIthChar 
                         ( LTempEditRef . TeEditedString 
                         , WCursorMark . LmCharPos 
                         , NewChar 
                         ) 
                     EXCEPT Strings . SsOutOfBounds
                     => CantHappen 
                          ( AFT . A_InsertOrOverlayChar_String_subscript_out_of_bounds ) 
                     END (* TRY EXCEPT *) 
                   ; AssertTempEdit ( LTempEditRef ) 
                   ; LImagePers . IpTempEditState 
                       := PaintHs . TempEditStateTyp . TeStateText 
                   ; Display . NoteImageSavedState ( LImageTrans , FALSE ) 
                   ; Display . NoteImageParsedState ( LImageTrans , FALSE ) 
                   ; Display . NoteImageAnalyzedState ( LImageTrans , FALSE ) 
                   ; Display . HorizMoveCursorWindowRef 
                       ( WindowRef , 1 , LMustRepaintWindow ) 
                   EXCEPT 
                   Backout ( EMessage ) 
                   => (* Rollback changes to temp edit. *)
                     LImagePers . IpTempEditRef := LSavedTempEditRef 
                   ; LImagePers . IpTempEditState 
                       := LSavedTempEditState  
                   ; AssertDevel . WriteCheckpoint 
                       ( ImageRef := LImageTrans 
                       , Message := EMessage  
                       , DoCreateVersion := FALSE 
                       ) 
                     (* ^This will rewrite the checkpoint already written from 
                         inside AssertDevel, but with the changes undone. *) 
                   ; RAISE Backout ( EMessage ) 
                   END (* TRY EXCEPT *) 
                 END (* IF *) 
               END (* IF *) 
          (* ; TouchVer ( LImageTrans ) *) 
(* TODO: ^TouchVer not declared *) 
             ; IF LMustRepaintWindow 
               THEN 
                  Display . PaintWindowFromLines 
                    ( WindowRef  
                    , (* VAR *) LTrailingBlankLines (* Dead. *) 
                    , (* VAR *) LLinesRef (* Dead. *) 
                    , (* VAR *) LLineNo  (* Dead. *) 
                    )
(* CHECK: 1) can we only paint this line? 
          2) shouldn't we just mark the window for repainting? 
*)             
               ELSE 
                 PaintTempEditedLineInAllWindows 
                   ( LImageTrans , WCursorMark . LmLinesRef , LTempEditRef ) 
               END (* IF *) 
             END (* IF blank typed beyond EOL *) 
; PaintHs . BruteForceVerifyLineMarks ( WindowRef . WrImageRef ) 
          END (* WITH WCursorMark *) 
        END (* IF *) 
      END (* IF *) 
    END InsertOrOverlayChar 

(* EXPORTED: *)
; PROCEDURE InsertOrOverlayString  
    ( WindowRef : PaintHs . WindowRefTyp 
    ; String : TEXT 
    ; IsInsert : BOOLEAN (* Otherwise Overlay *) 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LLen : CARDINAL 
  ; VAR LMustRepaint : BOOLEAN 

  ; BEGIN 
(* TODO: This a bit less brutishly.  Build a whole TempEdit for each
         line maybe, or at least don't repaint after every char.
*) 
      LLen := Text . Length ( String ) 
    ; FOR RI := 0 TO LLen - 1 
      DO 
        WITH WCh = Text . GetChar ( String , RI ) 
        DO 
          CASE WCh 
          OF LbeStd . CharNewLine 
          => InsertOrOverlayChar ( WindowRef , WCh , IsInsert ) 
          ; Display . HorizMoveCursorWindowRef 
              ( WindowRef 
              , - WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
                  . LmCharPos 
              , (* VAR *) MustRepaint := LMustRepaint (* Dead *) 
              )  
          | LbeStd . CharTab 
          , LbeStd . CharFirstPrintable .. LbeStd . CharLastPrintable 
          => InsertOrOverlayChar ( WindowRef , WCh , IsInsert ) 
          ELSE 
            InsertOrOverlayChar ( WindowRef , LbeStd . CharBlank , IsInsert ) 
          END (* CASE *) 
        END (* WITH *) 
      END (* FOR *) 
    END InsertOrOverlayString  

(* EXPORTED: *)
; PROCEDURE TransposeChars ( WindowRef : PaintHs . WindowRefTyp ) 
    RAISES { Backout , Thread . Alerted } 

  = VAR LImageTrans : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LTempEditRef  : PaintHs . TempEditRefTyp 
  ; VAR LLength : Strings . StringSsTyp 
  ; VAR LSavedTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LSavedTempEditState : PaintHs . TempEditStateTyp 
  ; VAR LMustRepaintWindow : BOOLEAN 
  ; VAR LLeftCharCt : LbeStd . CharNoTyp 
  ; VAR LRightCharCt : LbeStd . CharNoTyp 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 
  ; VAR LCh1 : CHAR 
  ; VAR LCh2 : CHAR 

  ; BEGIN (* TransposeChars *) 
      IF WindowRef # NIL 
      THEN 
        LImageTrans := WindowRef . WrImageRef 
      ; IF LImageTrans # NIL 
        THEN 
          LImagePers := LImageTrans . ItPers
        ; WITH 
            WCursorMark 
            = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
          DO
            IF WCursorMark # NIL AND WCursorMark . LmLinesRef # NIL 
            THEN
              IF WCursorMark . LmCharPos = 0 
              THEN 
                Display . Beep ( Errors . ErrorTyp . ETransposeCharsAtBOL ) 
              ELSIF WCursorMark . LmCharPos 
                    > Display . NonblankLengthOfCurrentLine ( WindowRef ) 
              THEN (* Transpose in blank space to right of line, no action. *) 
                Display . HorizMoveCursorWindowRef 
                  ( WindowRef , 1 , LMustRepaintWindow ) 
              ELSE 
                GrabTempEditRef 
                  ( LImageTrans 
                  , WCursorMark 
                  , RmFromPos := WCursorMark . LmCharPos + 1 
                  , LmToPos := WCursorMark . LmCharPos - 1 
                  , (* VAR *) TempEditRef := LTempEditRef 
                  ) 
              ; LLength := Strings . Length ( LTempEditRef . TeEditedString ) 
              ; LSavedTempEditRef := CopyOfTempEditRef ( LTempEditRef ) 
              ; LSavedTempEditState := LImagePers . IpTempEditState 
              ; TRY (* For Backout *) 
                  TRY (* For Strings . SsOutOfBounds *) 
                    IF LTempEditRef . TeDelFromPos 
                       = LbeStd . LimitedCharNoInfinity 
                    THEN 
                      LTempEditRef . TeDelFromPos 
                        := WCursorMark . LmCharPos - 1 
                    ; LTempEditRef . TeDelToPos 
                        := MIN ( LLength , WCursorMark . LmCharPos + 1 ) 
                    ; LTempEditRef . TeInsLen := 2 
                    ; LLeftCharCt := 0 
                    ; LRightCharCt := 2 
                    ELSE 
                      LLeftCharCt 
                        := MAX ( LTempEditRef . TeDelFromPos 
                                 - WCursorMark . LmCharPos  
                                 + 1 
                               , 0 
                               ) 
                      (* No of affected chars to right of previosly edited. *)
                    ; DEC ( LTempEditRef . TeDelFromPos , LLeftCharCt ) 
                    ; INC ( LTempEditRef . TeInsLen , LLeftCharCt ) 
                    ; LRightCharCt 
                        := MAX ( WCursorMark . LmCharPos 
                                 - ( LTempEditRef . TeDelFromPos 
                                     + LTempEditRef . TeInsLen 
                                   ) 
                                 + 1 
                               , 0 
                               ) 
                      (* No of affected chars to left of previosly edited. *)
                    ; LTempEditRef . TeDelToPos 
                        := MIN ( LTempEditRef . TeDelToPos + LRightCharCt 
                               , LLength
                               ) 
                    ; INC ( LTempEditRef . TeInsLen , LRightCharCt ) 
                    END (* IF *) 
                  ; LCh1 := Strings . IthChar 
                              ( LTempEditRef . TeEditedString 
                              , WCursorMark . LmCharPos - 1 
                              ) 
                  ; IF WCursorMark . LmCharPos = LLength 
                    THEN 
                      LCh2 := ' ' 
                    ; Strings . AppendCharInPlace 
                        ( LTempEditRef . TeEditedString , ' ' ) 
                    ELSE 
                      LCh2 := Strings . IthChar 
                                ( LTempEditRef . TeEditedString 
                                , WCursorMark . LmCharPos  
                                ) 
                    END (* IF *) 
                  ; Strings . StoreIthChar 
                      ( LTempEditRef . TeEditedString 
                      , WCursorMark . LmCharPos - 1  
                      , LCh2 
                      ) 
                  ; Strings . StoreIthChar 
                      ( LTempEditRef . TeEditedString 
                      , WCursorMark . LmCharPos   
                      , LCh1 
                      ) 
                  ; SetTextAttr 
                      ( LTempEditRef . TeTextAttrArrayRef 
                      , LTempEditRef . TeTextAttrActualSize 
                      , WCursorMark . LmCharPos - 1 
                      , 2
                      , LLength 
                      , PaintHs . TextAttrTyped  
                      ) 
                  ; AssertTempEdit ( LTempEditRef ) 
                  ; LImagePers . IpTempEditState 
                      := PaintHs . TempEditStateTyp . TeStateText 
                  ; Display . NoteImageSavedState 
                      ( LImageTrans , FALSE ) 
                  ; Display . NoteImageParsedState 
                      ( LImageTrans , FALSE ) 
                  ; Display . NoteImageAnalyzedState 
                      ( LImageTrans , FALSE ) 
                  ; Display . HorizMoveCursorWindowRef 
                      ( WindowRef , 1 , LMustRepaintWindow ) 
                  ; IF LMustRepaintWindow  
                    THEN 
                      Display . PaintWindowFromLines 
                        ( WindowRef  
                        , (* VAR *) LTrailingBlankLines (* Dead. *) 
                        , (* VAR *) LLinesRef (* Dead. *) 
                        , (* VAR *) LLineNo  (* Dead. *) 
                        )
                    ELSIF LLeftCharCt > 0 OR LRightCharCt > 0 OR LCh1 # LCh2 
                    THEN 
                      PaintTempEditedLineInAllWindows 
                        ( LImageTrans 
                        , WCursorMark . LmLinesRef 
                        , LTempEditRef 
                        ) 
                    END (* IF *) 
                  EXCEPT Strings . SsOutOfBounds  
                  => (* Translate this to an assertion failure. *) 
                     RAISE 
                       Backout 
                         ( MessageCodes . Image 
                             ( AFT . A_TransposeChars_Strings_SsOutOfBounds ) 
                         )    
                  END (* TRY EXCEPT *) 
                EXCEPT Backout ( EMessage ) 
                => (* Rollback changes to temp edit. *)
                  LImagePers . IpTempEditRef := LSavedTempEditRef 
                ; LImagePers . IpTempEditState 
                    := LSavedTempEditState  
                ; AssertDevel . WriteCheckpoint 
                    ( ImageRef := LImageTrans 
                    , Message := EMessage  
                    , DoCreateVersion := FALSE 
                    ) 
                  (* ^This will rewrite the checkpoint already written from 
                      inside AssertDevel, but with the changes undone. *) 
                ; RAISE Backout ( EMessage ) 
                END (* TRY EXCEPT *) 
              END (* IF *) 
            END (* IF *) 
          END (* WITH *) 
        END (* IF *) 
      END (* IF *) 
    END TransposeChars 

(* EXPORTED: *)
; PROCEDURE DeleteRestOfLine ( WindowRef : PaintHs . WindowRefTyp ) 
    RAISES { Backout , Thread . Alerted } 

  = VAR LImageTrans : PaintHs . ImageTransientTyp  
  ; VAR LImagePers : PaintHs . ImagePersistentTyp  
  ; VAR LLength : Strings . StringSsTyp 
  ; VAR LDeleteFromPos : LbeStd . CharNoTyp 
  ; VAR LTempEditRef  : PaintHs . TempEditRefTyp 
  ; VAR LSavedTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LSavedTempEditState : PaintHs . TempEditStateTyp 
  ; VAR LActualMovement : EditWindow . CharPointTyp 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 
  ; VAR LMustRepaint : BOOLEAN 

  ; BEGIN 
      IF WindowRef # NIL 
      THEN 
        LImageTrans := WindowRef . WrImageRef 
      ; IF LImageTrans # NIL 
        THEN 
          LImagePers := LImageTrans . ItPers  
        ; WITH 
            WCursorMark 
            = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
          DO
            IF WCursorMark # NIL AND WCursorMark . LmLinesRef # NIL 
            THEN
              LLength := Display . NonblankLengthOfCurrentLine ( WindowRef ) 
            ; IF LLength = 0 AND WCursorMark . LmCharPos = 0 
              THEN (* Remove the line entirely. *) 
                DeleteChar ( WindowRef , DeletingBwd := TRUE ) 
              ; Display . MoveCursorWindowRef 
                  ( WindowRef 
                  , EditWindow . CharPointTyp 
                      { - LbeStd . LimitedCharNoMax , 1 } 
                  , (* VAR *) LActualMovement 
                  , (* VAR *) LMustRepaint 
                  ) 
(* FIX:  This probably won't put the cursor back where it started, if an
           exception is raised.
*) 
              ; IF LMustRepaint 
                THEN 
                  Display . PaintWindowFromLines 
                    ( WindowRef  
                    , (* VAR *) LTrailingBlankLines (* Dead. *) 
                    , (* VAR *) LLinesRef (* Dead. *) 
                    , (* VAR *) LLineNo  (* Dead. *) 
                    )
                END (* IF *) 
              ELSIF WCursorMark . LmCharPos >= LLength  
              THEN (* Deleting in white space to right. No action. *) 
              ELSE 
                GrabTempEditRef 
                  ( LImageTrans 
                  , WCursorMark 
                  , RmFromPos := LbeStd . CharNoInfinity 
                  , LmToPos := WCursorMark . LmCharPos 
                  , (* VAR *) TempEditRef := LTempEditRef 
                  ) 
              ; LSavedTempEditRef := CopyOfTempEditRef ( LTempEditRef ) 
              ; LSavedTempEditState := LImagePers . IpTempEditState 
              ; LDeleteFromPos := WCursorMark . LmCharPos 
              ; TRY
                  TRY  
                    WHILE LDeleteFromPos > 0 
                          AND Strings . IthChar 
                                ( LTempEditRef . TeEditedString 
                                , LDeleteFromPos - 1 
                                ) 
                               = ' ' 
                    DO DEC ( LDeleteFromPos ) 
                    END (* WHILE *) 
                  EXCEPT Strings . SsOutOfBounds
                  => CantHappen 
                       ( AFT . A_DeleteRestOfLine_String_subscript_out_of_bounds ) 
                  END (* TRY EXCEPT *) 
                ; IF LTempEditRef . TeDelFromPos 
                     = LbeStd . LimitedCharNoInfinity 
                  THEN 
                    LTempEditRef . TeDelFromPos := LDeleteFromPos 
                  ; LTempEditRef . TeInsLen := 0  
                  ELSE 
                    LTempEditRef . TeDelFromPos 
                      := MIN ( LTempEditRef . TeDelFromPos 
                             , LDeleteFromPos  
                             ) 
                  ; LTempEditRef . TeInsLen 
                      := MAX ( 0 
                             , LDeleteFromPos 
                               - LTempEditRef . TeDelFromPos
                             ) 
                  END (* IF *) 
                ; LTempEditRef . TeDelToPos 
                    := LTempEditRef . TeLinesRef . LrLineLen   
                ; Strings . TruncateInPlace
                    ( LTempEditRef . TeEditedString , LDeleteFromPos ) 
                ; DeleteTextAttr 
                    ( LTempEditRef . TeTextAttrArrayRef 
                    , LTempEditRef . TeTextAttrActualSize 
                    , LDeleteFromPos 
                    , LLength - LDeleteFromPos  
                    , LLength 
                    ) 
                ; AssertTempEdit ( LTempEditRef ) 
                ; LImagePers . IpTempEditState 
                    := PaintHs . TempEditStateTyp . TeStateText 
                ; Display . NoteImageSavedState  ( LImageTrans , FALSE ) 
                ; Display . NoteImageParsedState ( LImageTrans , FALSE ) 
                ; Display . NoteImageAnalyzedState ( LImageTrans , FALSE ) 
                ; PaintTempEditedLineInAllWindows 
                    ( LImageTrans  
                    , WCursorMark . LmLinesRef 
                    , LTempEditRef 
                    ) 
                EXCEPT Backout ( EMessage ) 
                => (* Rollback changes to temp edit. *)
                  LImagePers . IpTempEditRef := LSavedTempEditRef 
                ; LImagePers . IpTempEditState := LSavedTempEditState  
                ; AssertDevel . WriteCheckpoint 
                    ( ImageRef := LImageTrans 
                    , Message := EMessage  
                    , DoCreateVersion := FALSE 
                    ) 
                  (* ^This will rewrite the checkpoint already written from 
                      inside AssertDevel, but with the changes undone. *) 
                ; RAISE Backout ( EMessage ) 
                END (* TRY EXCEPT *) 
              END (* IF *) 
            END (* IF *) 
          END (* WITH *) 
        END (* IF *) 
      END (* IF *) 
    END DeleteRestOfLine 

; PROCEDURE DeleteTempEditedCharRange 
    ( ImageTrans : PaintHs . ImageTransientTyp 
    ; FromCharPos : LbeStd . CharNoTyp 
    ; ToCharPos : LbeStd . CharNoTyp 
    (* FromCharPos and ToCharPos in the as-possibly-already-edited line. *) 
    ) 
  RAISES { Backout } 
  (* PRE: GrabTempEditRef has been done. *) 

  = VAR LImagePers : PaintHs . ImagePersistentTyp  
  ; VAR LTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LChangedToPos : LbeStd . CharNoTyp 
  ; VAR LPrefixLen : LbeStd . CharNoTyp 
  ; VAR LMiddleLen : LbeStd . CharNoTyp 
  ; VAR LSuffixLen : LbeStd . CharNoTyp 
  ; VAR LSavedTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LSavedTempEditState : PaintHs . TempEditStateTyp 

  ; BEGIN 
      LImagePers := ImageTrans . ItPers  
    ; LTempEditRef := LImagePers . IpTempEditRef 
    ; LSavedTempEditRef := CopyOfTempEditRef ( LTempEditRef ) 
    ; LSavedTempEditState := LImagePers . IpTempEditState 
    ; TRY 
        IF LTempEditRef . TeDelFromPos = LbeStd . LimitedCharNoInfinity 
        THEN (* No changes have been made yet to this line. *) 
        (* The following is the state _before_ the deletion: *) 
          LTempEditRef . TeDelFromPos := FromCharPos 
        ; LTempEditRef . TeDelToPos := FromCharPos  
        ; LTempEditRef . TeInsLen := 0 
        END (* IF *) 
      ; LChangedToPos 
          := LTempEditRef . TeDelFromPos + LTempEditRef . TeInsLen   
      ; Assert 
          ( LTempEditRef . TeDelFromPos <= ToCharPos  
            AND  FromCharPos <= LChangedToPos  
          , AFT . A_DeleteTempEditedCharRange_Outside_Already_Edited_Region 
          ) 
      ; LPrefixLen := MAX ( LTempEditRef . TeDelFromPos - FromCharPos , 0 ) 
      ; LMiddleLen 
          := MAX ( MIN ( LChangedToPos , ToCharPos ) 
                   - MAX ( LTempEditRef . TeDelFromPos , FromCharPos ) 
                 , 0 
                 ) 
      ; LSuffixLen := MAX ( ToCharPos - LChangedToPos , 0 ) 
      ; DEC ( LTempEditRef . TeDelFromPos , LPrefixLen ) 
      ; DEC ( LTempEditRef . TeInsLen , LMiddleLen ) 
      ; INC ( LTempEditRef . TeDelToPos , LSuffixLen ) 
      ; TRY 
          Strings . DeleteCharsInPlace 
            ( LTempEditRef . TeEditedString 
            , FromCharPos 
            , ToCharPos - FromCharPos 
            ) 
          EXCEPT Strings . SsOutOfBounds
          => CantHappen 
               ( AFT . A_DeleteTempEditedCharRange_String_subscript_out_of_bounds ) 
          END (* TRY EXCEPT *) 
      ; DeleteTextAttr 
          ( LTempEditRef . TeTextAttrArrayRef 
          , LTempEditRef . TeTextAttrActualSize 
          , FromCharPos 
          , ToCharPos - FromCharPos 
          , Strings . Length ( LTempEditRef . TeEditedString ) 
          ) 
      ; LImagePers . IpTempEditState 
          := PaintHs . TempEditStateTyp . TeStateText 
      ; AssertTempEdit ( LTempEditRef ) 
      ; Display . NoteImageSavedState ( ImageTrans , FALSE ) 
      ; Display . NoteImageParsedState 
          ( ImageTrans , FALSE ) 
      ; Display . NoteImageAnalyzedState ( ImageTrans , FALSE ) 
      EXCEPT 
      Backout ( EMessage ) 
      => (* Rollback changes to temp edit. *)
        LImagePers . IpTempEditRef := LSavedTempEditRef 
      ; LImagePers . IpTempEditState := LSavedTempEditState  
      ; AssertDevel . WriteCheckpoint 
          ( ImageRef := ImageTrans 
          , Message := EMessage  
          , DoCreateVersion := FALSE 
          ) 
        (* ^This will rewrite the checkpoint already written from 
            inside AssertDevel, but with the changes undone. *) 
      ; RAISE Backout ( EMessage ) 
      END (* TRY EXCEPT *) 
    END DeleteTempEditedCharRange 

(* EXPORTED: *)
; PROCEDURE DeleteBetweenMarks 
    ( ImageTrans : PaintHs . ImageTransientTyp   
    ; FromMark : PaintHs . LineMarkMeatTyp 
    ; ThruMark : PaintHs . LineMarkMeatTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LTempEditRef : PaintHs . TempEditRefTyp   
  ; VAR LFromPos : LbeStd . CharNoTyp 
  ; VAR LLength : LbeStd . CharNoTyp 
  ; VAR LLinesInvolved : [ 1 .. 3 ]  (* 3 denotes >= 3 *) 
  ; VAR LMark : PaintHs . LineMarkMeatTyp 
  ; VAR LPredMark : PaintHs . LineMarkTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineShift : LbeStd . LineNoTyp 
  ; VAR LPredLinesRef : PaintHs . LinesRefMeatTyp 

  ; BEGIN 
      IF ImageTrans # NIL 
      THEN
        Display . SecureSucc ( ImageTrans , FromMark . LmLinesRef ) 
      ; Display . SecurePred ( ImageTrans , ThruMark . LmLinesRef ) 
      ; Display . SecureSucc ( ImageTrans , ThruMark . LmLinesRef ) 
      ; IF Marks . Equal ( FromMark . LmTokMark , ThruMark . LmTokMark ) 
        THEN 
          LFromPos := FromMark . LmCharPos 
        ; LLinesInvolved := 1 
        ELSE 
          LFromPos := 0 
        ; IF ThruMark . LmLinesRef . LrLeftLink = FromMark . LmLinesRef 
          THEN LLinesInvolved := 2 
          ELSE LLinesInvolved := 3 (* Or more. *) 
          END (* IF *) 
        END (* IF *) 

      (* Adjust any included marks to point to start of selection. *) 
      ; LLinesRef := FromMark . LmLinesRef 
      ; LLineShift := 0  
      ; LMark := FromMark . LmRightLink 
      ; WHILE LMark # ThruMark 
        DO 
          TRY 
            Assert 
              ( Marks . Compare ( FromMark . LmTokMark , LMark . LmTokMark ) 
                <= 0 
              , AFT . A_DeleteSelection_Out_of_order_mark 
              ) 
          EXCEPT Marks . Unordered 
          => CantHappen 
               ( AFT . A_DeleteBetweenMarks_Unordered_marks ) 
          END (* TRY EXCEPT *) 
        ; IF LMark . LmMarkSs = PaintHs . MarkSsTyp . MarkSsCursor 
          THEN
            WHILE LLinesRef # LMark . LmLinesRef 
            DO 
              INC ( LLineShift 
                  , Display . ActualNoOfLines ( LLinesRef . LrLineCt ) 
                  )
            ; LLinesRef := LLinesRef . LrRightLink 
            END (* WHILE *) 
          ; DEC ( LMark . LmWindowRef . WrCursorLineNoInWindow 
                , LLineShift + LMark . LmLineNo 
                ) 
          END (* IF *)
        ; LMark . LmTokMark := FromMark . LmTokMark 
        ; LMark . LmLinesRef := FromMark . LmLinesRef 
        ; LMark . LmCharPos := FromMark . LmCharPos 
        ; LMark . LmLineNo := FromMark . LmLineNo 
        ; LMark := LMark . LmRightLink 
        END (* WHILE *) 

      (* Move any included marks out of selected range. *) 
      ; IF FromMark . LmRightLink # ThruMark 
        THEN
          PaintHs . UnlinkLineMark ( FromMark ) 
        ; PaintHs . LinkLineMarkToLeft 
            ( InsertToLeftOfRef := ThruMark , RefToInsert := FromMark ) 
        ; PaintHs . BruteForceVerifyLineMarks ( ImageTrans ) 
        END (* IF *) 

      (* Delete selected portion of last included line. *) 
      ; GrabTempEditRef 
          ( ImageTrans 
          , Mark := ThruMark 
          , RmFromPos := ThruMark . LmCharPos 
          , LmToPos := LFromPos  
          , (* VAR *) TempEditRef := LTempEditRef 
          ) 
      ; LLength := Strings . Length ( LTempEditRef . TeEditedString ) 
      ; IF ThruMark . LmCharPos >= LLength  
        THEN (* Delete Nl at end too. *) 
          DeleteTempEditedCharRange ( ImageTrans , LFromPos , LLength ) 
        ; TYPECASE ThruMark . LmLinesRef . LrRightLink 
          OF PaintHs . LinesRefMeatTyp ( TRightLinesRef ) 
          => Display . SecureSucc ( ImageTrans , TRightLinesRef ) 
          ; TRY 
              Strings . InsertBlanksInPlace 
                ( LTempEditRef . TeEditedString 
                , PrefixLength := LFromPos 
                , BlankCount := TRightLinesRef . LrFromPos 
                , EventualLengthHint 
                    := LFromPos + TRightLinesRef . LrLineLen 
                )
            EXCEPT Strings . SsOutOfBounds
            => CantHappen 
                 ( AFT . A_DeleteBetweenMarks_String_subscript_out_of_bounds_last_line ) 
          END (* TRY EXCEPT *) 
          ; Strings . AppendTextInPlace 
              ( LTempEditRef . TeEditedString 
              , TRightLinesRef . LrLineText 
              ) 
          ; AssertTempEdit ( LTempEditRef , TRightLinesRef ) 
          ELSE 
            AssertTempEdit ( LTempEditRef ) 
          END (* TYPECASE *) 
        ; InnerFlushTempEdit 
            ( ImageTrans 
            , ThruMark . LmLinesRef 
            , LTempEditRef 
            , DelNlShift := LFromPos   
            ) 
        ; InitTempEditForText ( LTempEditRef , NIL , 0 ) 
        ELSE 
          DeleteTempEditedCharRange 
            ( ImageTrans , LFromPos , ThruMark . LmCharPos ) 
        END (* IF *) 

      (* Delete any full lines properly between FromMark and ThruMark. *)
      ; IF LLinesInvolved = 3 (* Or more *) 
        THEN 
          LLinesRef := ThruMark . LmLinesRef . LrLeftLink 
        ; LMark := NEW ( PaintHs . LineMarkMeatTyp ) 
        ; LMark . LmWindowRef := NIL 
        ; LMark . LmMarkSs := PaintHs . MarkSsTyp . MarkSsNull 
        ; LMark . LmLinesRef := LLinesRef  
        ; LMark . LmTokMark := LLinesRef . LrBolTokMark 
        ; LMark . LmCharPos := 0 
        ; LMark . LmLineNo 
            := Display . ActualLineCtOfLinesRef ( ImageTrans , LLinesRef ) - 1 
        ; PaintHs . InsertLineMarkToLeft 
            ( InsertMark := LMark 
            , SuccMark := ThruMark 
            , Image := ImageTrans 
            ) 
        ; PaintHs . BruteForceVerifyLineMarks ( ImageTrans ) 
        ; LOOP 
            Display . SecurePred ( ImageTrans , LLinesRef ) 
          ; GrabTempEditRef 
              ( ImageTrans 
              , Mark := LMark 
              , RmFromPos := LbeStd . CharNoInfinity 
              , LmToPos := 0 
              , (* VAR *) TempEditRef := LTempEditRef 
              ) 
          ; LTempEditRef . TeDelFromPos := 0 
          ; LTempEditRef . TeDelToPos 
              := Strings . Length ( LTempEditRef . TeEditedString ) 
          ; LTempEditRef . TeInsLen := 0 
          ; InitTempEditTextOnly ( LTempEditRef , ThruMark . LmLinesRef ) 
          ; LPredLinesRef := LLinesRef . LrLeftLink 
          ; AssertTempEdit ( LTempEditRef , LLinesRef . LrRightLink ) 
          ; InnerFlushTempEdit 
              ( ImageTrans 
              , LLinesRef 
              , LTempEditRef 
              , DelNlShift := 0  
              ) 
          ; InitTempEditForText ( LTempEditRef , NIL , 0 ) 
          ; IF LMark . LmLineNo > 0 
            THEN DEC ( LMark . LmLineNo ) 
            ELSE 
              LLinesRef := LPredLinesRef 
            ; IF LLinesRef = FromMark . LmLinesRef 
              THEN EXIT 
              ELSE 
                LMark . LmLinesRef := LLinesRef 
              ; LLinesRef . LrHasMark := TRUE 
              ; LMark . LmTokMark := LLinesRef . LrBolTokMark 
              ; LMark . LmCharPos := 0 
              ; LMark . LmLineNo 
                  := Display . ActualLineCtOfLinesRef 
                       ( ImageTrans , LLinesRef ) 
                     - 1 
              END (* IF *) 
            END (* IF *)  
          END (* LOOP *) 
        ; PaintHs . DeleteLineMark 
            ( Mark := LMark 
            , (* VAR *) PredMark := LPredMark (* Dead *) 
            , Image := ImageTrans 
            ) 
        END (* IF *) 

      (* Delete relevant portion first line of selection. *) 
      ; IF LLinesInvolved >= 2 
        THEN
          GrabTempEditRef 
            ( ImageTrans 
            , Mark := FromMark 
            , RmFromPos := LbeStd . CharNoInfinity 
            , LmToPos := FromMark . LmCharPos 
            , (* VAR *) TempEditRef := LTempEditRef 
            ) 
        ; DeleteTempEditedCharRange 
            ( ImageTrans 
            , FromCharPos := FromMark . LmCharPos 
            , ToCharPos := Strings . Length ( LTempEditRef . TeEditedString )
            ) 
        ; TRY 
            Strings . InsertBlanksInPlace 
              ( LTempEditRef . TeEditedString 
              , PrefixLength 
                  := Strings . Length ( LTempEditRef . TeEditedString )
              , BlankCount := ThruMark . LmLinesRef . LrFromPos 
              , EventualLengthHint 
                  := LLength + ThruMark . LmLinesRef . LrLineLen 
              )
          EXCEPT Strings . SsOutOfBounds
          => CantHappen 
               ( AFT . A_DeleteBetweenMarks_String_subscript_out_of_bounds_first_line ) 
          END (* TRY EXCEPT *) 
        ; Strings . AppendTextInPlace 
            ( LTempEditRef . TeEditedString 
            , ThruMark . LmLinesRef . LrLineText 
            ) 
        ; AssertTempEdit ( LTempEditRef , FromMark . LmLinesRef . LrRightLink )
        ; InnerFlushTempEdit 
            ( ImageTrans 
            , FromMark . LmLinesRef 
            , LTempEditRef 
            , DelNlShift := FromMark . LmCharPos  
            ) 
        ; InitTempEditForText ( LTempEditRef , NIL , 0 ) 
        END (* IF *) 
      END (* IF *) 
    END DeleteBetweenMarks 

(* EXPORTED: *) 
; PROCEDURE AcceptRepairUnderCursor ( WindowRef : PaintHs . WindowRefTyp ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR ArTempEditRef : PaintHs . TempEditRefTyp 

  ; PROCEDURE ArTouchTempEditedChar 
      ( TempEditRef : PaintHs . TempEditRefTyp 
      ; CharPos : LbeStd . CharNoTyp  
      ) 
    RAISES { Backout } 

    = VAR LLength : LbeStd . CharNoTyp 
    ; VAR LSavedTempEditRef : PaintHs . TempEditRefTyp 

    ; BEGIN (* ArTouchTempEditedChar *) 
        LSavedTempEditRef := CopyOfTempEditRef ( TempEditRef ) 
      ; TRY 
          IF TempEditRef . TeDelFromPos = LbeStd . LimitedCharNoInfinity 
          THEN
            TempEditRef . TeDelFromPos := CharPos 
          ; TempEditRef . TeDelToPos := CharPos + 1 
          ; TempEditRef . TeInsLen := 1  
          ELSIF CharPos + 1 = TempEditRef . TeDelFromPos 
          THEN 
            DEC ( TempEditRef . TeDelFromPos ) 
          ; INC ( TempEditRef . TeInsLen ) 
          ELSIF CharPos 
                = TempEditRef . TeDelFromPos + TempEditRef . TeInsLen 
          THEN 
            INC ( TempEditRef . TeDelToPos ) 
          ; INC ( TempEditRef . TeInsLen ) 
          ELSE (* The char at the cursor is already in the edited region, 
                  no action needed. *) 
          END (* IF *) 
        ; LLength  := Strings . Length ( TempEditRef . TeEditedString ) 
        ; IF CharPos >= LLength  
          THEN 
            TRY 
              Strings . InsertBlanksInPlace 
                ( TempEditRef . TeEditedString 
                , PrefixLength := LLength 
                , BlankCount := CharPos - ( LLength - 1 ) 
                )  
            EXCEPT Strings . SsOutOfBounds
            => CantHappen 
                 ( AFT . A_ArTouchTempEditedChar_String_subscript_out_of_bounds ) 
            END (* TRY EXCEPT *) 
          END (* IF *) 
        ; AssertTempEdit ( TempEditRef ) 
        EXCEPT 
        Backout ( EMessage ) 
        => (* Rollback changes to temp edit. *)
          WindowRef . WrImageRef . ItPers . IpTempEditRef := LSavedTempEditRef 
        ; AssertDevel . WriteCheckpoint 
            ( ImageRef := WindowRef . WrImageRef 
            , Message := EMessage  
            , DoCreateVersion := FALSE 
            ) 
          (* ^This will rewrite the checkpoint already written from 
              inside AssertDevel, but with the changes undone. *) 
        ; RAISE Backout ( EMessage ) 
        END (* TRY EXCEPT *) 
      END ArTouchTempEditedChar 

  ; PROCEDURE ArRight 
       ( RightDecoration : PaintHs . TextAttrComponentTyp 
       ; RightAttrSs : INTEGER 
       ) 
    RAISES { Backout } 
    (* Do the right region without worrying about interference from left. *) 

    = VAR LToCharPos : LbeStd . CharNoTyp 

    ; BEGIN 
        CASE RightDecoration 
        OF PaintHs . TaDecStrikeout  
        => IF RightAttrSs = ArTempEditRef . TeTextAttrActualSize - 1 
          THEN 
            LToCharPos 
              := Strings . Length ( ArTempEditRef . TeEditedString )
          ELSE 
            LToCharPos 
              := ArTempEditRef . TeTextAttrArrayRef ^ [ RightAttrSs + 1 ]
                 . TaCharPos 
          END (* IF *) 
        ; DeleteTempEditedCharRange 
            ( WindowRef . WrImageRef 
            , ArTempEditRef . TeTextAttrArrayRef ^ [ RightAttrSs ] . TaCharPos 
            , LToCharPos 
            ) 
        | PaintHs . TaDecCaret 
        => ArTouchTempEditedChar 
            ( ArTempEditRef 
            , ArTempEditRef . TeTextAttrArrayRef ^ [ RightAttrSs ]  
              . TaCharPos 
            )  
        ELSE (* Do nothing. *) 
        END (* CASE *)  
      END ArRight 

  ; BEGIN (* AcceptRepairUnderCursor *) 
      VAR LAttrSs : INTEGER  
    ; VAR LLeftAttrSs : INTEGER 
    ; VAR LRightAttrSs : INTEGER 
    ; VAR LLeftDecoration : PaintHs . TextAttrComponentTyp 
    ; VAR LRightDecoration : PaintHs . TextAttrComponentTyp 
    ; VAR LToCharPos : LbeStd . CharNoTyp 

    ; BEGIN (* Block AcceptRepairUnderCursor *)
        IF WindowRef . WrImageRef # NIL 
        THEN 
          WITH 
            WCursorMark 
            = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
          DO 
            GrabTempEditRef 
              ( WindowRef . WrImageRef 
              , WCursorMark 
              , RmFromPos := WCursorMark . LmCharPos  
              , LmToPos := WCursorMark . LmCharPos 
              , (* VAR *) TempEditRef := ArTempEditRef 
              ) 
          ; IF ArTempEditRef . TeTextAttrArrayRef # NIL 
               AND ArTempEditRef . TeTextAttrActualSize > 0 
            THEN 
            (* First, find subscripts in the text attribute list of the region
               properly containing the cursor (LRightAttrSs) or of the two
               regions the cursor sits between (LLeftAttrSs and LRightAttrSs) 
            *) 
              LLeftAttrSs := - 1 
            ; LRightAttrSs := - 1 
            ; WITH 
                WCursorMark 
                = WindowRef . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
              DO 
                IF WCursorMark . LmCharPos <= 0 
                THEN 
                  LRightAttrSs := 0 
                ELSE 
                  LAttrSs := 0 
                ; LOOP 
                    IF LAttrSs >= ArTempEditRef . TeTextAttrActualSize 
                    THEN EXIT 
                    ELSE 
                      WITH 
                        WAttr 
                          = ArTempEditRef . TeTextAttrArrayRef ^ [ LAttrSs ] 
                      DO 
                        IF WAttr . TaCharPos > WCursorMark . LmCharPos  
                        THEN EXIT 
                        ELSE 
                          LRightAttrSs := LAttrSs  
                        ; IF WAttr . TaCharPos < WCursorMark . LmCharPos 
                          THEN 
                            LLeftAttrSs := LAttrSs
                          END (* IF *) 
                        ; INC ( LAttrSs ) 
                        END (* IF *) 
                      END (* WITH *) 
                    END (* IF *) 
                  END (* LOOP *) 
                END (* IF *) 
              (* This is tricky.  If the cursor sits between two regions of 
                 text attributes, we want to accept each region, if it is a 
                 repair.
                 Accepting a suggested deletion (strikeout) can change the
                 text attribute list in complex ways, making it unreasonable 
                 to make further tests attributes of the original list 
                 afterwards. 
                 One delete must be done last, and two deletes need to be 
                 combined into one.  
                 Here is a more-or-less complete cartesion product of cases for
                 what is to the left/right of the cursor.
              *) 

              ; IF 0 <= LRightAttrSs 
                   AND LRightAttrSs < ArTempEditRef . TeTextAttrActualSize    
                THEN
                  LRightDecoration 
                    := ArTempEditRef . TeTextAttrArrayRef ^ [ LRightAttrSs ] 
                       . TaDecoration
                ELSE
                  LRightDecoration := PaintHs . TaDecPlain 
                END (* IF *) 
              ; IF 0 <= LLeftAttrSs AND LLeftAttrSs + 1 = LRightAttrSs 
                THEN
                  LLeftDecoration 
                    := ArTempEditRef . TeTextAttrArrayRef ^ [ LLeftAttrSs ] 
                       . TaDecoration
                ELSE
                  LLeftDecoration := PaintHs . TaDecPlain 
                END (* IF *) 

              ; CASE LLeftDecoration 
                OF PaintHs . TaDecStrikeout 
                => CASE LRightDecoration 
                   OF PaintHs . TaDecStrikeout 
                   => (* Two strikeouts.  Combine into one delete. *) 
                     IF LRightAttrSs 
                        = ArTempEditRef . TeTextAttrActualSize - 1 
                     THEN 
                       LToCharPos 
                         := Strings . Length ( ArTempEditRef . TeEditedString )
                     ELSE 
                       LToCharPos 
                         := ArTempEditRef . TeTextAttrArrayRef 
                            ^ [ LRightAttrSs + 1 ] . TaCharPos 
                     END (* IF *) 
                   ; DeleteTempEditedCharRange 
                       ( WindowRef . WrImageRef 
                       , ArTempEditRef . TeTextAttrArrayRef ^ [ LLeftAttrSs ]
                         . TaCharPos 
                       , LToCharPos 
                       ) 
                   | PaintHs . TaDecCaret 
                   => (* Strikeout, Insert. Do the insert first. *) 
                      ArTouchTempEditedChar 
                        ( ArTempEditRef 
                        , ArTempEditRef . TeTextAttrArrayRef 
                          ^ [ LRightAttrSs ]  . TaCharPos 
                        )  
                   ; DeleteTempEditedCharRange 
                       ( WindowRef . WrImageRef 
                       , ArTempEditRef . TeTextAttrArrayRef ^ [ LLeftAttrSs ]
                         . TaCharPos 
                       , ArTempEditRef . TeTextAttrArrayRef ^ [ LRightAttrSs ]
                         . TaCharPos 
                       )
                   ELSE (* Strikeout, nothing. *) 
                     DeleteTempEditedCharRange 
                       ( WindowRef . WrImageRef 
                       , ArTempEditRef . TeTextAttrArrayRef ^ [ LLeftAttrSs ]
                         . TaCharPos 
                       , ArTempEditRef . TeTextAttrArrayRef ^ [ LRightAttrSs ]
                         . TaCharPos 
                       )
                   END (* CASE *) 
                | PaintHs . TaDecCaret 
                => (* Insert, any. Do the left insert first. *) 
                   ArTouchTempEditedChar 
                     ( ArTempEditRef 
                     , ArTempEditRef . TeTextAttrArrayRef ^ [ LLeftAttrSs ]  
                       . TaCharPos 
                     )  
                ; ArRight ( LRightDecoration , LRightAttrSs ) 
                ELSE (* Nothing, any *) 
                  ArRight ( LRightDecoration , LRightAttrSs ) 
                END (* CASE *) 

              ; FlushEdit ( WindowRef . WrImageRef ) 
              END (* WITH *) 
            END (* IF *) 
          END (* WITH *) 
        END (* IF *) 
      END (* Block *) 
    END AcceptRepairUnderCursor 

(* EXPORTED: *) 
; PROCEDURE ToggleInsertMode ( Window : PaintHs . WindowRefTyp ) 
  : BOOLEAN (* Now Is insert. *) 

  = BEGIN 
      Window . WrInsertMode := NOT Window . WrInsertMode 
    ; EditWindow . PaintInsertMode ( Window ) 
    ; RETURN Window . WrInsertMode 
    END ToggleInsertMode 

(* EXPORTED: *) 
; PROCEDURE SetInsertMode 
    ( Window : PaintHs . WindowRefTyp ; Value : BOOLEAN ) 

  = BEGIN 
      Window . WrInsertMode := Value  
    ; EditWindow . PaintInsertMode ( Window ) 
    END SetInsertMode 

; PROCEDURE InitTextEdit ( ) 

  = BEGIN (* InitTextEdit *) 
      FOR RLinesRefSs := MarkIdTyp . MiLeadingLine 
            TO MarkIdTyp . MiTrailingLine 
      DO WITH WLinesRef = LinesRefArrayNull [ RLinesRefSs ] 
         DO WLinesRef := NIL 
         END (* WITH WLinesRef *) 
      END (* FOR *) 
    END InitTextEdit 

; BEGIN (* TextEdit *) 
    InitTextEdit ( ) 
  END TextEdit 
. 
