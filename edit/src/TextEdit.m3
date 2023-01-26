
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2023, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* TextEdit carries out text editing the Est and text painting. *) 

MODULE TextEdit 

; IMPORT Compiler 
; IMPORT Fmt
; IMPORT Stdio 
; IMPORT Text 
; IMPORT TextWr 
; IMPORT Thread
; IMPORT Wr 

; IMPORT Ascii 
; IMPORT AssertDevel 
; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure  
; FROM Failures IMPORT Backout , Ignore 
; IMPORT Display 
; IMPORT EditWindow 
; IMPORT Errors
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LbeStd 
; IMPORT LineMarks 
; IMPORT Marks 
; IMPORT MergeTxt 
; IMPORT MessageCodes 
; IMPORT Options 
; IMPORT PaintHs
; FROM PaintHs IMPORT MarkSsTyp
; IMPORT PortTypes 
; IMPORT Strings 
; IMPORT SyntEdit 
; IMPORT TravUtil 
; IMPORT TreeBrowse

; IMPORT EstDump 

; TYPE AFT = MessageCodes . T
; CONST MCI = MessageCodes . Image 
; CONST TF = Compiler . ThisFile ( ) 
; CONST CTL = Compiler . ThisLine
; CONST FI = Fmt . Int 

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
             = LWindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ]
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
    ( (* IN OUT ^ *) TempEditRef : PaintHs . TempEditRefTyp 
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
        THEN RegeneratedString := Strings . Copy ( ExpectedString ) 
        ELSE RegeneratedString := Strings . Empty ( ) 
        END (* IF *) 
      END (* IF *) 
    END VerifyEditedLine 

; PROCEDURE IsAncestorNodeNo 
    ( LeftNodeNo : LbeStd . EstNodeNoTyp 
    ; LeftNodeCt : LbeStd . EstNodeNoTyp 
    ; RightNodeNo : LbeStd . EstNodeNoTyp 
    ) 
  : BOOLEAN 

  = BEGIN 
      RETURN LeftNodeNo < RightNodeNo 
             AND LeftNodeNo + LeftNodeCt > RightNodeNo  
                 (* >= RightNodeNo + RightNodeCt - 1 *) 
    END IsAncestorNodeNo 

; PROCEDURE BruteForceRepairLineMarks 
    ( MarkHeader : PaintHs . LineMarkTyp ) 

  = VAR LMark : PaintHs . LineMarkMeatTyp
  ; VAR LLineText : TEXT 

  ; BEGIN (* BruteForceRepairLineMarks *) 
      IF MarkHeader # NIL AND MarkHeader . LmRightLink # MarkHeader 
      THEN 
        LMark := MarkHeader . LmRightLink 
      ; LOOP 
          IF LMark . LmLinesRef # NIL 
             AND ( NOT Marks . Equal 
                         ( LMark . LmTokMark 
                         , LMark . LmLinesRef . LrBolTokMark 
                        ) 
                   OR LMark . LmTokMark . TkmEstNodeCt 
                      # LMark . LmLinesRef . LrBolTokMark . TkmEstNodeCt 
                      (* Marks . Equal doesn't check EstNodeCt field, because
                         it is redundant, if properly set, and if not, due
                         to incomplete updating, we want other compares to
                         succeed anyway.
                      *)
                 )  
          THEN (* This can happen if BruteForceVerifyAllLinesRefs repaired
                  the LrBolTokMark after AdjustLineMarksNodeNos was done.
               *)
            IF LMark . LmLinesRef . LrLineText = NIL
            THEN LLineText := " NIL" 
            ELSE
              LLineText
                := Wr . EOL & "  \"" & LMark . LmLinesRef . LrLineText & "\""
            END (* IF *) 
          ; Assertions . MessageText 
              ( "Repairing Marklist Mark "
                & PaintHs . MarkSsImage ( LMark . LmMarkSs ) 
                & " for line:"
                & LLineText 
                & Wr . EOL 
                & "  from " 
                & Marks . MarkImage ( LMark . LmTokMark )
                & Wr . EOL 
                & "  to   " 
                & Marks . MarkImage ( LMark . LmLinesRef . LrBolTokMark ) 
                & Wr . EOL 
                & Wr . EOL 
              ) 
          ; LMark . LmTokMark := LMark . LmLinesRef . LrBolTokMark 
          END (* IF *) 
        ; IF LMark . LmRightLink = MarkHeader 
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
                         OR LLinesRef . LrBolTokMark . TkmEstNodeCt 
                            # LNextMark . TkmEstNodeCt 
                         (* Marks . Equal doesn't check EstNodeCt field, because
                            it is redundant, if properly set, and if not, due
                            to incomplete updating, we want other compares to
                            succeed anyway.
                         *) 
                      THEN
                        IF RepairIsOK 
                        THEN 
                          Assertions . MessageText 
                            ( "Repairing LinesRef Mark in line:"
                              & Wr . EOL 
                              & "  \"" 
                              & LLinesRef . LrLineText  
                              & "\""  
                              & Wr . EOL 
                              & "  from "
                              & Marks . MarkImage ( LLinesRef . LrBolTokMark )
                              & Wr . EOL 
                              & "  to   "
                              & Marks . MarkImage ( LNextMark )  
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
    ; VAR (* IN OUT *) LinesRefMeat : PaintHs . LinesRefMeatTyp 
    ; BlankLineCt : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout , Thread . Alerted }
  (* In whatever LinesRef list is current in ImageRef. *) 

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
    ; VAR (* IN OUT *) LinesRefMeat : PaintHs . LinesRefMeatTyp 
    ; BlankLineCt : LbeStd . LineNoTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 
  (* In whatever LinesRef list is current in ImageRef. *) 

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

; VAR GNodeNoToCatch := LAST ( INTEGER ) 
; VAR GCatchAll := FALSE

; PROCEDURE NotePatchedTokMark
    ( READONLY TokMark : Marks . TokMarkTyp ; KindLabel : TEXT )
  = VAR LBreakNodeNo : INTEGER := 0
  ; VAR LImage : TEXT 

  ; BEGIN
      LBreakNodeNo := TokMark . TkmEstNodeNo 
    ; IF GCatchAll OR LBreakNodeNo = GNodeNoToCatch
      THEN
        LImage := Marks . MarkImage ( TokMark ) 
      ; Wr . PutText ( Stdio . stderr , "Patched TokMark for " ) 
      ; Wr . PutText ( Stdio . stderr , KindLabel ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
      ; Wr . PutText ( Stdio . stderr , "  " ) 
      ; Wr . PutText ( Stdio . stderr , LImage ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL )
      ; Wr . Flush ( Stdio . stderr )
      END (* IF *) 
    END NotePatchedTokMark  

; PROCEDURE NoteNewLinesRef ( LinesRefMeat : PaintHs . LinesRefMeatTyp )

  = VAR LBreakNodeNo : INTEGER := 0
  ; VAR LImage : TEXT 

  ; BEGIN
      LBreakNodeNo := LinesRefMeat . LrBolTokMark . TkmEstNodeNo 
    ; IF GCatchAll OR LBreakNodeNo = GNodeNoToCatch
      THEN
        LImage := PaintHs . LinesRefImage ( LinesRefMeat , LineIndent := 4 )
      ; Wr . PutText ( Stdio . stderr , "NewLinesRef:" ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
      ; Wr . PutText ( Stdio . stderr , LImage ) 
      ; Wr . PutText ( Stdio . stderr , Wr . EOL )
      ; Wr . Flush ( Stdio . stderr )
      END (* IF *) 
    END NoteNewLinesRef 

; TYPE MarkIdTyp 
  = { MiLeadingLine , MiFirstText , MiSecondText , MiTrailingLine } 

; TYPE LinesRefArrayTyp = ARRAY MarkIdTyp OF PaintHs . LinesRefMeatTyp 

; VAR (* CONST *) LinesRefArrayNull : LinesRefArrayTyp

; TYPE LineMapRowTyp
    = RECORD
        MrOldLinesRef , MrNewLinesRef : PaintHs . LinesRefMeatTyp := NIL 
      ; MrOldLineNo , MrNewLineNo : LbeStd . LineNoTyp  
      END (* RECORD *) 
; TYPE LineMapTyp
    = ARRAY [ PaintHs . WindowNoMin .. PaintHs . WindowNoMax ] OF LineMapRowTyp  

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
  ; VAR IfteOldLinesRefHeader : PaintHs . LinesRefTyp 
  ; VAR IfteNewLinesRefHeader : PaintHs . LinesRefTyp 
  ; VAR IfteOldMarkHeader : PaintHs . LineMarkTyp
  ; VAR IfteNewMarkHeader : PaintHs . LineMarkTyp 
  ; VAR IfteLinesRefArray : LinesRefArrayTyp 
  ; VAR IfteNewEstRoot : EstHs . EstRefTyp 
  ; VAR IfteStartBolTokMark : Marks . TokMarkTyp 
  ; VAR IfteLinesRefsAccountedFor : LbeStd . LineNoTyp 
  ; VAR IfteFirstOldActualLineCt : LbeStd . LineNoTyp 
  ; VAR IfteNewLinesRefCt : LbeStd . LineNoTyp 
  ; VAR IfteOldEndBolTokMark : Marks . TokMarkTyp 
  ; VAR IfteNewEndBolTokMark : Marks . TokMarkTyp 
  ; VAR IfteOldFromLinesRefMeat : PaintHs . LinesRefMeatTyp 
  ; VAR IfteSecondOldLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR IfteOldThruLinesRef : PaintHs . LinesRefMeatTyp 
        (* IfteOld[From|Thru]LinesRef are the range of LinesRefs in the
           old list that are to be unlinked.  This could include blank
           lines before and/or after the line(s) edited, if MergeTxt
           pulled them into its revised set of LinesRefs.
           It could also be empty (denoted by IfteOldFromLinesRefMeat = NIL)
           when editing "beyond" EOI. 
        *) 
  ; VAR IfteRMNewLinesRef : PaintHs . LinesRefHeaderTyp
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
        (* ^Every *displayed* blank line counts in *BlankLines*. *) 
  ; VAR IfteOldEstRoot : LbeStd . EstRootTyp
  ; VAR IfteNewMarks : PaintHs . MarkArrayTyp

  ; VAR Ifte1stLineMap : LineMapTyp 

  ; PROCEDURE IfteMapLines ( OldLinesRef , NewLinesRef : PaintHs . LinesRefMeatTyp )

    = VAR LWindowRef : PaintHs . WindowRefTyp
    
    ; BEGIN 
      ; LWindowRef := ImageTrans . ItWindowList 
      ; WHILE LWindowRef # NIL 
        DO IF LWindowRef . WrFirstLineLinesRef = OldLinesRef
              AND LWindowRef . WrFirstLineNo = OldLineNo
          THEN WITH WMapRow = Ifte1stLineMap [ LWindowRef . WrWindowNo ]
          DO <* ASSERT WMapRow . MrOldLinesRef = NIL *> 
           THEN
             WMapRow . MrOldLinesRef := LWindowRef . WrFirstLineLinesRef 
           ; WMapRow . MrOldLineNo := LWindowRef . WrFirstLineLineNo 
           ; WMapRow . MrNewLinesRef := NewLinesRef  
           ; WMapRow . MrNewLineNo := NewLineNo 

           END (* IF *) 
          END (* WITH *)
        END (* WHILE *)
      END IfteMapLines 

; PROCEDURE IfteMakeChanges ( ) 
    RAISES { Backout }  
    (* Make changes to non-functional data structures.  Hopefully, we
       won't crash during these.  If a crash occurs later, we can undo
       them, with the same hope, before writing a checkpoint and for
       ignoring the failing operation. 
    *) 

    = VAR LWindowRef : PaintHs . WindowRefTyp
    
    ; BEGIN 
      (* Switch to the new Est and adjust the marks in LinesRefs 
         beyond the point of change. *) 
(* TODO: do versions here: Also altered tree, although that will 
         mainly happen when chars are edited. *)
      ; ImageRef . ItPers . IpEstRoot := IfteNewEstRoot
      ; ImageRef . ItPers . IpLineHeaderRef := IfteNewLinesRefHeader 
      ; ImageRef . ItPers . IpMarkHeader := IfteNewMarkHeader
      
      ; LWindowRef := ImageTrans . ItWindowList 
      ; WHILE LWindowRef # NIL 
        DO WITH WMapRow = Ifte1stLineMap [ LWindowRef . WrWindowNo ]
          DO IF WMapRow . MrOldLinesRef # NIL 
           THEN
             LWindowRef . WrFirstLineLinesRef := WMapRow . MrNewLinesRef 
           ; LWindowRef . WrFirstLineLinesNo := WMapRow . MrNewLineNo  
           END (* IF *) 
          END (* WITH *)
        END (* WHILE *) 
      END IfteMakeChanges 

  ; PROCEDURE IfteUndoChanges ( )
  
    = VAR LWindowRef : PaintHs . WindowRefTyp
    
    ; BEGIN 
        ImageRef . ItPers . IpEstRoot := IfteOldEstRoot
      ; ImageRef . ItPers . IpLineHeaderRef := IfteOldLinesRefHeader 
      ; ImageRef . ItPers . IpMarkHeader := IfteOldMarkHeader
      
      ; LWindowRef := ImageTrans . ItWindowList 
      ; WHILE LWindowRef # NIL 
        DO WITH WMapRow = Ifte1stLineMap [ LWindowRef . WrWindowNo ]
          DO IF WMapRow . MrOldLinesRef # NIL 
           THEN
             LWindowRef . WrFirstLineLinesRef := WMapRow . MrOldLinesRef 
           ; LWindowRef . WrFirstLineLinesNo := WMapRow . MrOldLineNo  
           END (* IF *) 
          END (* WITH *)
        END (* WHILE *) 
      END IfteUndoChanges

  ; PROCEDURE IfteBuildMergedLinesRefs
      ( VAR LinesRefArray : LinesRefArrayTyp ) 
    RAISES { Backout , Thread . Alerted }
    (* POST:
         LinesRefArray is filled in with refs to new LinesRefs, some
           possibly NIL.
         IfteLinesRefsAccountedFor = number of non-NIL elements of
           LinesRefArray.  
         IfteNewEndBolTokMark advanced to leftmost Nl after the merged region.
    *) 

    = VAR LLineCt : LbeStd . LineNoTyp 
    ; VAR LStartBolTokMark : Marks . TokMarkTyp 
    ; VAR LEditedString : Strings . StringTyp 
    ; VAR LTextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
    ; VAR LLineErrArrayRef : PaintHs . LineErrArrayRefTyp 
    ; VAR LNewLinesNo : LbeStd . LineNoTyp 

    ; BEGIN (* IfteBuildMergedLinesRefs *) 
        LinesRefArray := LinesRefArrayNull 
      ; IfteLinesRefsAccountedFor := 0 
      ; LNewLinesNo := 0 
      ; LStartBolTokMark := IfteStartBolTokMark 
      ; LLineCt 
          := Display . LineCtOfBolTokMark 
               ( IfteNewEstRoot , LStartBolTokMark ) 

      (* Look for MiLeadingLine *) 
      ; IF IfteLinesRefsAccountedFor < IfteNewLinesRefCt  
        THEN 
          IF LLineCt > 0 
          THEN (* We have a new blank line. It will be element 
                  MiLeadingLine. *) 
            WITH WLinesRefMeat 
                 = LinesRefArray [ MarkIdTyp . MiLeadingLine ] 
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
            ; IF LNewLinesNo < IfteNewLinesRefCt 
              THEN 
                WLinesRefMeat := NEW ( PaintHs . LinesRefMeatTyp ) 
              ; WLinesRefMeat . LrBolTokMark := LStartBolTokMark 
              ; WLinesRefMeat . LrVisibleIn := PaintHs . WindowNoSetEmpty 
              ; WLinesRefMeat . LrFromPos := 0 
              ; WLinesRefMeat . LrLineLen := 0 
              ; WLinesRefMeat . LrLineCt := LLineCt 
              ; WLinesRefMeat . LrGapAfter := FALSE 
              ; WLinesRefMeat . LrHasMark := FALSE 
              ; WLinesRefMeat . LrIsStopper := FALSE 
              ; WLinesRefMeat . LrLineText := "" 
(* CHECK: ^Can we get away with a NIL here instead? (and below, too.) 
             Places in Display do it, but for LrGapAfter = TRUE. *) 
              ; WLinesRefMeat . LrTextAttrArrayRef := NIL 
              ; WLinesRefMeat . LrLineErrArrayRef := NIL 
              END (* IF *) 
            END (* WITH WLinesRefMeat *) 
          ; INC ( IfteLinesRefsAccountedFor ) 
          ; LStartBolTokMark := IfteNewEndBolTokMark 
          ; LLineCt 
              := Display . LineCtOfBolTokMark 
                   ( IfteNewEstRoot , LStartBolTokMark ) 
          ; INC ( LNewLinesNo ) 
          END (* IF *) 
        END (* IF *) 

      (* Look for MiFirstText *) 
      ; IF IfteLinesRefsAccountedFor < IfteNewLinesRefCt  
        THEN 
          IF LLineCt = 0 
          THEN (* This is the first text line. *) 
            WITH WLinesRefMeat = LinesRefArray [ MarkIdTyp . MiFirstText ] 
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
            ; IF LNewLinesNo < IfteNewLinesRefCt 
              THEN 
                WLinesRefMeat := NEW ( PaintHs . LinesRefMeatTyp ) 
              ; WLinesRefMeat . LrBolTokMark := LStartBolTokMark 
              ; WLinesRefMeat . LrVisibleIn := PaintHs . WindowNoSetEmpty 
              ; WLinesRefMeat . LrFromPos 
                  := Strings . PosOf1stNonblank ( LEditedString ) 
              ; WLinesRefMeat . LrLineLen := Strings . Length ( LEditedString ) 
              ; WLinesRefMeat . LrLineCt := 0 
              ; WLinesRefMeat . LrGapAfter := FALSE 
              ; WLinesRefMeat . LrHasMark := FALSE 
              ; WLinesRefMeat . LrIsStopper := FALSE 
              ; WLinesRefMeat . LrLineText 
                  := Strings . ToText 
                       ( LEditedString , WLinesRefMeat . LrFromPos ) 
              ; WLinesRefMeat . LrTextAttrArrayRef := LTextAttrArrayRef  
              ; WLinesRefMeat . LrLineErrArrayRef := LLineErrArrayRef  
              END (* IF *) 
            END (* WITH WLinesRefMeat *) 
          ; INC ( IfteLinesRefsAccountedFor ) 
          ; LStartBolTokMark := IfteNewEndBolTokMark 
          ; LLineCt 
              := Display . LineCtOfBolTokMark 
                   ( IfteNewEstRoot , LStartBolTokMark ) 
          ; INC ( LNewLinesNo ) 
          END (* IF *) 
        END (* IF *) 

      (* Look for MiSecondText *) 
      ; IF IfteLinesRefsAccountedFor < IfteNewLinesRefCt 
        THEN 
          IF LLineCt = 0 
          THEN (* This is the second text line. *) 
            WITH WLinesRefMeat = LinesRefArray [ MarkIdTyp . MiSecondText ] 
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
            ; IF LNewLinesNo < IfteNewLinesRefCt 
              THEN 
                WLinesRefMeat := NEW ( PaintHs . LinesRefMeatTyp ) 
              ; WLinesRefMeat . LrBolTokMark := LStartBolTokMark 
              ; WLinesRefMeat . LrVisibleIn := PaintHs . WindowNoSetEmpty 
              ; WLinesRefMeat . LrFromPos 
                  := Strings . PosOf1stNonblank ( LEditedString ) 
              ; WLinesRefMeat . LrLineLen := Strings . Length ( LEditedString ) 
              ; WLinesRefMeat . LrLineCt := 0 
              ; WLinesRefMeat . LrGapAfter := FALSE 
              ; WLinesRefMeat . LrHasMark := FALSE 
              ; WLinesRefMeat . LrIsStopper := FALSE 
              ; WLinesRefMeat . LrLineText 
                  := Strings . ToText 
                       ( LEditedString , WLinesRefMeat . LrFromPos ) 
              ; WLinesRefMeat . LrTextAttrArrayRef := LTextAttrArrayRef  
              ; WLinesRefMeat . LrLineErrArrayRef := LLineErrArrayRef  
              END (* IF *) 
            END (* WITH WLinesRefMeat *) 
          ; INC ( IfteLinesRefsAccountedFor ) 
          ; LStartBolTokMark := IfteNewEndBolTokMark 
          ; LLineCt 
              := Display . LineCtOfBolTokMark 
                   ( IfteNewEstRoot , LStartBolTokMark ) 
          ; INC ( LNewLinesNo ) 
          END (* IF *) 
        END (* IF *) 

      (* Look for MiTrailingLine *) 
      ; IF IfteLinesRefsAccountedFor < IfteNewLinesRefCt  
        THEN 
          WITH WLinesRefMeat = LinesRefArray [ MarkIdTyp . MiTrailingLine ] 
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
          ; IF LNewLinesNo < IfteNewLinesRefCt 
            THEN 
              WLinesRefMeat := NEW ( PaintHs . LinesRefMeatTyp ) 
            ; WLinesRefMeat . LrBolTokMark := LStartBolTokMark 
            ; WLinesRefMeat . LrVisibleIn := PaintHs . WindowNoSetEmpty 
            ; WLinesRefMeat . LrFromPos := 0 
            ; WLinesRefMeat . LrLineLen := 0 
            ; WLinesRefMeat . LrLineCt := LLineCt 
            ; WLinesRefMeat . LrGapAfter := FALSE 
            ; WLinesRefMeat . LrHasMark := FALSE 
            ; WLinesRefMeat . LrIsStopper := FALSE 
            ; WLinesRefMeat . LrLineText := "" 
            ; WLinesRefMeat . LrTextAttrArrayRef := NIL 
            ; WLinesRefMeat . LrLineErrArrayRef := NIL 
            ; IfteCheckNodeCtMismatch
                ( IfteNewEstRoot , WLinesRefMeat , "New merged:" , TRUE ) 
            END (* IF *) 
          END (* WITH WLinesRefMeat *) 
        ; INC ( IfteLinesRefsAccountedFor ) 
        END (* IF *) 
      END IfteBuildMergedLinesRefs 

  ; PROCEDURE IfteVerifyMergedLinesRefs
      ( READONLY LinesRefArray : LinesRefArrayTyp ) 
    RAISES { Backout , Thread . Alerted }
    (* Verify that the elements of LinesRefArray can be regenerated
       by Est traversal. *) 

    = VAR LStartBolTokMark : Marks . TokMarkTyp 
    ; VAR LEndBolTokMark : Marks . TokMarkTyp 
    ; VAR LEditedString : Strings . StringTyp 
    ; VAR LFailureOccurred : BOOLEAN 

    ; BEGIN (* IfteVerifyMergedLinesRefs *) 
        LFailureOccurred := FALSE 
      ; LStartBolTokMark := IfteStartBolTokMark 

      (* Look for MiLeadingLine *) 
      ; WITH WLinesRefMeat = LinesRefArray [ MarkIdTyp . MiLeadingLine ] 
        DO IF WLinesRefMeat = NIL 
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
                            := WLinesRefMeat . LrTextAttrArrayRef 
              , (* VAR *) RegeneratedLineErrArrayRef 
                            := WLinesRefMeat . LrLineErrArrayRef 
              , (* VAR *) EndBolTokMark := LEndBolTokMark 
              , (* IN OUT *) FailureOccurred := LFailureOccurred 
              ) 
          ; WLinesRefMeat . LrLineText 
              := Strings . ToText ( LEditedString , WLinesRefMeat . LrFromPos ) 
          ; LStartBolTokMark := LEndBolTokMark 
          END (* IF *) 
        END (* WITH WLinesRefMeat *) 

      (* Look for MiFirstText *) 
      ; WITH WLinesRefMeat = LinesRefArray [ MarkIdTyp . MiFirstText ] 
        DO IF WLinesRefMeat = NIL 
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
                     := WLinesRefMeat . LrTextAttrArrayRef 
                 , (* VAR *) RegeneratedLineErrArrayRef 
                     := WLinesRefMeat . LrLineErrArrayRef 
                 , (* VAR *) EndBolTokMark := LEndBolTokMark 
                 , (* IN OUT *) FailureOccurred := LFailureOccurred 
                 ) 
          ; WLinesRefMeat . LrLineText 
              := Strings . ToText ( LEditedString , WLinesRefMeat . LrFromPos ) 
          ; LStartBolTokMark := LEndBolTokMark 
          END (* IF *) 
        END (* WITH WLinesRefMeat *) 

      (* Look for MiSecondText *) 
      ; WITH WLinesRefMeat = LinesRefArray [ MarkIdTyp . MiSecondText ] 
        DO IF WLinesRefMeat = NIL 
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
                  := WLinesRefMeat . LrTextAttrArrayRef 
              , (* VAR *) RegeneratedLineErrArrayRef 
                  := WLinesRefMeat . LrLineErrArrayRef 
              , (* VAR *) EndBolTokMark := LEndBolTokMark 
              , (* IN OUT *) FailureOccurred := LFailureOccurred 
              ) 
          ; WLinesRefMeat . LrLineText 
              := Strings . ToText ( LEditedString , WLinesRefMeat . LrFromPos ) 
          ; LStartBolTokMark := LEndBolTokMark 
          END (* IF *) 
        END (* WITH WLinesRefMeat *) 

      (* Look for MiTrailingLine *) 
      ; WITH WLinesRefMeat = LinesRefArray [ MarkIdTyp . MiTrailingLine ] 
        DO IF WLinesRefMeat = NIL  
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
                   := WLinesRefMeat . LrTextAttrArrayRef 
               , (* VAR *) RegeneratedLineErrArrayRef 
                   := WLinesRefMeat . LrLineErrArrayRef 
               , (* VAR *) EndBolTokMark := LEndBolTokMark 
               , (* IN OUT *) FailureOccurred := LFailureOccurred 
               ) 
          ; WLinesRefMeat . LrLineText 
              := Strings . ToText ( LEditedString , WLinesRefMeat . LrFromPos ) 
          END (* IF *) 
        END (* WITH WLinesRefMeat *) 
      END IfteVerifyMergedLinesRefs



(* NEW CODE: *)

  ; PROCEDURE IfteCheckNodeCtMismatch
      ( EstRoot : EstHs . EstRefTyp
      ; LinesRef : PaintHs . LinesRefTyp
      ; Label : TEXT := ""
      ; DoRepair : BOOLEAN := FALSE 
      )

    = VAR LLinesRefMeat : PaintHs . LinesRefMeatTyp
    ; VAR LEstNodeCt : LbeStd . EstNodeNoTyp
    ; VAR LMsg : TEXT 
    ; VAR LWrT : Wr . T 

    ; BEGIN
        IF EstRoot = NIL THEN RETURN END (* IF *) 
      ; TYPECASE LinesRef
        OF NULL => RETURN
        | PaintHs . LinesRefMeatTyp ( TLinesRefMeat )
        => WITH WTokMark = TLinesRefMeat . LrBolTokMark 
           DO
             LEstNodeCt
              := TravUtil . NodeCtOfDescendantWithNodeNo
                   ( EstRoot , WTokMark . TkmEstNodeNo ) 
           ; IF WTokMark . TkmEstNodeCt # LEstNodeCt
             THEN
               LWrT := TextWr . New ( )
             ; Wr . PutText ( LWrT , Label ) 
             ; Wr . PutText ( LWrT , " LinesRef:" ) 
             ; Wr . PutText ( LWrT , Wr . EOL ) 
             ; Wr . PutText ( LWrT , "  LrLineText=" ) 
             ; Wr . PutText ( LWrT , "  \"" ) 
             ; Wr . PutText ( LWrT , TLinesRefMeat . LrLineText ) 
             ; Wr . PutChar ( LWrT , '\"' ) 
             ; Wr . PutText ( LWrT , Wr . EOL ) 
             ; Wr . PutText ( LWrT , "  TokMark={" ) 
             ; Wr . PutText ( LWrT , Marks . MarkImage ( WTokMark ) ) 
             ; Wr . PutChar ( LWrT , '}' ) 
             ; Wr . PutText ( LWrT , Wr . EOL ) 
             ; Wr . PutText ( LWrT , "  NodeCt mismatch: EstNodeCt:" ) 
             ; Wr . PutText ( LWrT , Fmt . Int ( LEstNodeCt ) ) 
             ; Wr . PutText ( LWrT , ", TokMark.TkmEstNodeCt:" ) 
             ; Wr . PutText ( LWrT , Fmt . Int ( WTokMark . TkmEstNodeCt ) ) 
             ; Wr . PutText ( LWrT , Wr . EOL )
             ; IF DoRepair
               THEN
                 WTokMark . TkmEstNodeCt := LEstNodeCt 
               ; Wr . PutText ( LWrT , "  Repaired to " )
               ; Wr . PutText ( LWrT , Fmt . Int ( LEstNodeCt ) ) 
               ; Wr . PutText ( LWrT , Wr . EOL )
               END (* IF *) 
             ; LMsg := TextWr . ToText ( LWrT )
             ; Assertions . MessageText ( LMsg ) 
             END (* IF *)
           END (* WITH *) 
        ELSE RETURN
        END (* TYPECASE *)
      END IfteCheckNodeCtMismatch

  ; PROCEDURE IfteRebuildLists 
      ( OldEstRoot : EstHs . EstRefTyp
      ; READONLY OldFromTokMark : Marks . TokMarkTyp 
      ; READONLY OldToTokMark : Marks . TokMarkTyp
        (* ^To the Nl at the right of the edited line(s). *) 
      ; NewEstRoot : EstHs . EstRefTyp 
      ; READONLY NewFromTokMark : Marks . TokMarkTyp
      ; VAR NewLinesRefHeader : PaintHs . LinesRefHeaderTyp 
      ; VAR NewMarkHeader : PaintHs . LineMarkTyp 
      ) 
     RAISES { Backout } 

    = VAR IfteRblCurOldLinesRefMeat : PaintHs . LinesRefMeatTyp
    ; VAR IfteRblMarkLinesRefMeat : PaintHs . LinesRefMeatTyp

    ; VAR IfteRblPrevOldMarkMeat : PaintHs . LineMarkMeatTyp 
    ; VAR IfteRblCurOldMarkMeat : PaintHs . LineMarkMeatTyp 
    ; VAR IfteRblRMNewMark : PaintHs . LineMarkHeaderTyp 
    ; VAR IfteRblCurNewMarkMeat : PaintHs . LineMarkMeatTyp 

    ; VAR IfteRblMergeLineNo : LbeStd . LineNoTyp 
    ; VAR IfteRblLastLineNoOf1stOldLinesRef : LbeStd . LineNoTyp 
    ; VAR IfteRblPrevNewMergeToLineNo : LbeStd . LineNoTyp 
    ; VAR IfteRblCurNewMergeToLineNo : LbeStd . LineNoTyp
    ; VAR IfteRbl0LineNoOfCurOldLinesRef : LbeStd . LineNoTyp
    ; VAR IfteRblToNodeNoOfFromTokMark : INTEGER 
    ; VAR IfteRblToNodeNoOfToTokMark : INTEGER 

    ; VAR IfteRblNewCharPos : LbeStd . LimitedCharNoSignedTyp 

    ; VAR IfteRblCmpMarkToEnd : [ - 1 .. 1 ] 
    ; VAR IfteRblCmpLinesToEnd : [ - 1 .. 1 ] 
    ; VAR IfteRblCmpLinesToMark : [ - 1 .. 1 ]

    ; VAR IfteRblIn2ndOldLinesRef : BOOLEAN

    ; VAR LIsOnOrRightOfRightPath
          , LIsOnOrRightOfLeftPath
          , LIsOnOrLeftOfLeftPath
          , LIsOnOrLeftOfRightPath
          : BOOLEAN
          (* Interpretation: Concerning an Est node N, identified by its
             node number, LIsOnOrXOfYPath, for X and Y IN {Left, Right},
             means N is on or to the X side of the path from the Y node to
             the root.  Y will be the from- or to-node of the merged region. *)

    ; PROCEDURE IfteRblAdvanceOldLinesRef ( )

      = BEGIN
          IF IfteRblCurOldLinesRefMeat . LrRightLink = IfteOldLinesRefHeader
          THEN IfteRblCurOldLinesRefMeat := NIL 
          ELSE IfteRblCurOldLinesRefMeat 
                 := NARROW 
                      ( IfteRblCurOldLinesRefMeat . LrRightLink 
                      , PaintHs . LinesRefMeatTyp 
                      ) (* Ca'nt faail. *) 
          END (* IF *) 
        END IfteRblAdvanceOldLinesRef
        
    ; PROCEDURE IfteRblPatchTokMark
        ( VAR TokMark : Marks . TokMarkTyp ; KindLabel : TEXT )

      = VAR LOldNodeCt : INTEGER 
      ; VAR LIsOnOrRightOfRightPath
            , LIsOnOrRightOfLeftPath
            , LIsOnOrLeftOfLeftPath
            , LIsOnOrLeftOfRightPath
            : BOOLEAN
            (* Interpretation: Concerning an Est node N, identified by its
               node number, LIsOnOrXOfYPath, for X and Y IN {Left, Right},
               means N is on or to the X side of the path from the Y node to
               the root.  Y will be the from- or to-node of the merged region. *)
               
      ; BEGIN 
          IF OldFromTokMark . TkmEstNodeNo <= TokMark . TkmEstNodeNo
             AND TokMark . TkmEstNodeNo
                 < OldFromTokMark . TkmEstNodeNo
                   + OldFromTokMark . TkmEstNodeCt
             (* Mark denotes a descendent of OldFromTokMark's node *) 
          THEN (* No patching needed. *)
  NotePatchedTokMark ( TokMark , KindLabel )  
          ELSIF OldToTokMark . TkmEstNodeNo <= TokMark . TkmEstNodeNo
                AND TokMark . TkmEstNodeNo
                    < OldToTokMark . TkmEstNodeNo
                      + OldToTokMark . TkmEstNodeCt
             (* Mark denotes a descendent of OldToTokMark's node *) 
          THEN (* Patch NodeNo  *) 
            INC ( TokMark . TkmEstNodeNo , IfteNodeNoChange )
; NotePatchedTokMark ( TokMark , KindLabel ) 
          ELSE 
            LIsOnOrRightOfRightPath
              := TokMark . TkmEstNodeNo + TokMark . TkmEstNodeCt
                 >= OldToTokMark . TkmEstNodeNo + OldToTokMark . TkmEstNodeCt 
          ; LIsOnOrRightOfLeftPath
              := TokMark . TkmEstNodeNo + TokMark . TkmEstNodeCt
                 >= OldFromTokMark . TkmEstNodeNo + OldFromTokMark . TkmEstNodeCt 
          ; LIsOnOrLeftOfRightPath
              := TokMark . TkmEstNodeNo <= OldToTokMark . TkmEstNodeNo
          ; LIsOnOrLeftOfLeftPath
              := TokMark . TkmEstNodeNo <= OldFromTokMark . TkmEstNodeNo

          ; IF LIsOnOrRightOfRightPath AND LIsOnOrLeftOfLeftPath
            THEN (* On the common path to the root.  Patch node count. *) 
              INC ( TokMark . TkmEstNodeCt , IfteNodeNoChange )
; NotePatchedTokMark ( TokMark , KindLabel ) 
            ELSIF NOT LIsOnOrLeftOfRightPath
            THEN (* To right of merged region.  Patch node nuber. *)
              INC ( TokMark . TkmEstNodeNo , IfteNodeNoChange ) 
; NotePatchedTokMark ( TokMark , KindLabel )  
            ELSIF NOT LIsOnOrRightOfLeftPath
            THEN (* To left of merged region.  No patch needed. *)
  NotePatchedTokMark ( TokMark , KindLabel )  
            ELSIF Marks . Compare ( TokMark , OldToTokMark ) = 0
                  AND ( TokMark . TkmKind
                        = MarkKindTyp . LeftSibFmtNo
                        OR TokMark . TkmKind
                           IN Marks . MarkKindSetEstLeaf
                           AND NOT TokMark . TkmStartAtEnd
                      )
(* REVIEW: ^Doubtful that this is needed at all.  This would have been handled
      the right descendent* check of this node, done earlier. *) 
            THEN (* It's the To mark, on the right path, but the merge
                    region stops short of it, so treat it as right of
                    right. *) 
              INC ( TokMark . TkmEstNodeNo , IfteNodeNoChange ) 
; NotePatchedTokMark ( TokMark , KindLabel ) 
            ELSE (* Between the forks in paths, inclusive.  Must get
                    the node count brute force from the Est. *)
              LOldNodeCt := TokMark . TkmEstNodeCt 
            ; TokMark . TkmEstNodeCt
                := TravUtil . NodeCtOfDescendantWithNodeNo
                     ( NewEstRoot , TokMark . TkmEstNodeNo )
; NotePatchedTokMark ( TokMark , KindLabel ) 
            END (* IF *) 
          END (* IF *)
        END IfteRblPatchTokMark 

    ; TYPE SideTyp = { Left , Right } 

    ; PROCEDURE IfteRblCopyNodes
        ( READONLY CopyToTokMark : Marks . TokMarkTyp ; Side : SideTyp )
      (* Copy some matching Mark and LinesRef nodes. *)

      = VAR LNewLinesRefMeat : PaintHs . LinesRefMeatTyp
      ; VAR LNewMarkMeat : PaintHs . LineMarkMeatTyp 
      ; VAR LBreakpoint : INTEGER 

      ; BEGIN
          LOOP (* Thru' lists of LinesRefs and Marks, while matching
                  pairs with equal TokMarks. *)

            (* Set up the next old LinesRef. *) 
            IF IfteRblCurOldLinesRefMeat = NIL 
            THEN (* Beyond the end of the old LinesRef list. *) 
              IfteRblCmpLinesToEnd := 1 
            ELSIF Side = SideTyp . Right THEN IfteRblCmpLinesToEnd := - 1 
            ELSE
              TRY 
                IfteRblCmpLinesToEnd
                  := Marks . Compare
                       ( IfteRblCurOldLinesRefMeat . LrBolTokMark
                       , CopyToTokMark
                       )
              EXCEPT Marks . Unordered
              => <* ASSERT FALSE , "Marks.Unordered." *>
(* TODO: Wouldn't it be nicer to just make Unordered another integer value,
         rather than have all these exception catchers?  If it were -2, the
         whole range would still fit in two bits, should that ever matter.
*) 
              END (* EXCEPT *)
            END (* IF *)
          ; IF IfteRblCmpLinesToEnd = 0
               AND ( CopyToTokMark . TkmKind = MarkKindTyp . RightSibFmtNo
                     OR CopyToTokMark . TkmKind IN Marks . MarkKindSetEstLeaf
                        AND CopyToTokMark . TkmStartAtEnd  
                   ) 
            THEN (* MergeTextEdit will have skipped this Est Node, so copy it . *) 
              IfteRblCmpLinesToEnd := - 1
            END (* IF *)

            (* Set up the next old Mark. *) 
          ; IF IfteRblCurOldMarkMeat = NIL 
            THEN (* Beyond end of Marks list. *)
              IfteRblCmpMarkToEnd := 1
            ELSIF Side = SideTyp . Right THEN IfteRblCmpMarkToEnd := - 1 
            ELSE 
              TRY 
                IfteRblCmpMarkToEnd
                  := Marks . Compare
                       ( IfteRblCurOldMarkMeat . LmTokMark , CopyToTokMark ) 
              EXCEPT Marks . Unordered 
              => <* ASSERT FALSE , "Marks.Unordered." *>
              END (* EXCEPT *)
            END (* IF *)
          ; IF IfteRblCmpMarkToEnd = 0
               AND ( CopyToTokMark . TkmKind = MarkKindTyp . RightSibFmtNo
                     OR CopyToTokMark . TkmKind IN Marks . MarkKindSetEstLeaf
                        AND CopyToTokMark . TkmStartAtEnd  
                   ) 
            THEN (* MergeTextEdit will have skipped this Est Node, so copy it. *) 
              IfteRblCmpMarkToEnd := - 1
            END (* IF *) 

          (* Which list node is next? *) 
          ; IF IfteRblCmpLinesToEnd >= 0 AND IfteRblCmpMarkToEnd >= 0 
            THEN (* Done with this portion of both liste. *)
              EXIT
            ELSIF IfteRblCmpMarkToEnd >= 0 
            THEN (* IMPLIES IfteRblCmpLinesToMark := - 1 *)
              IfteRblCmpLinesToMark := - 1 (* Current LinesRef is behind. *) 
            ELSIF IfteRblCmpLinesToEnd >= 0 
            THEN IfteRblCmpLinesToMark := 1 (* Current Mark is behind. *) 
            ELSE
              TRY 
                IfteRblCmpLinesToMark
                  := Marks . Compare
                       ( IfteRblCurOldLinesRefMeat . LrBolTokMark
                       , IfteRblCurOldMarkMeat . LmTokMark
                       )
              EXCEPT Marks . Unordered
              => <* ASSERT FALSE , "Marks.Unordered." *> 
              END (* EXCEPT *)
            END (* IF *) 

          (* If current LinesRef is behind the current Mark, copy LinesRef
             and advance to the next LinesRef. *) 
          ; IF IfteRblCmpLinesToMark = - 1 (* LinesRef's TokMark is <= Mark's. *)   
            THEN (* Copy a LinesRef. *)

  IF  IfteRblCurOldLinesRefMeat . LrBolTokMark . TkmEstNodeNo = 62
  THEN
    LBreakpoint := LBreakpoint (* Breakpoint. *) 
  END
; 

              LNewLinesRefMeat := NEW ( PaintHs . LinesRefMeatTyp )
            ; LNewLinesRefMeat . LrBolTokMark
                := IfteRblCurOldLinesRefMeat . LrBolTokMark
            ; IfteRblPatchTokMark ( LNewLinesRefMeat . LrBolTokMark , "LinesRef" ) 






            ; LNewLinesRefMeat . LrTextAttrArrayRef
                := IfteRblCurOldLinesRefMeat . LrTextAttrArrayRef
            ; LNewLinesRefMeat . LrLineErrArrayRef
                := IfteRblCurOldLinesRefMeat . LrLineErrArrayRef
            ; LNewLinesRefMeat . LrLineCt
                := IfteRblCurOldLinesRefMeat . LrLineCt 
            ; LNewLinesRefMeat . LrVisibleIn
                := IfteRblCurOldLinesRefMeat . LrVisibleIn 
            ; LNewLinesRefMeat . LrIsStopper
                := IfteRblCurOldLinesRefMeat . LrIsStopper
            ; LNewLinesRefMeat . LrHasMark
                := IfteRblCurOldLinesRefMeat . LrHasMark
            ; LNewLinesRefMeat . LrFromPos
                := IfteRblCurOldLinesRefMeat . LrFromPos
            ; LNewLinesRefMeat . LrLineLen
                := IfteRblCurOldLinesRefMeat . LrLineLen
            ; LNewLinesRefMeat . LrLineText
                := IfteRblCurOldLinesRefMeat . LrLineText
            ; IfteIfteMapLines
                ( IfteRblCurOldLinesRefMeat
                , LNewLinesRefMeat
                ) 
             
(* CHECK: Do we want to assert  TkmNodeCt = node ct in new tree? *)

            ; IfteRMNewLinesRef . LrRightLink := LNewLinesRefMeat
            ; LNewLinesRefMeat . LrLeftLink := IfteRMNewLinesRef
            ; IfteRMNewLinesRef := LNewLinesRefMeat
            ; IfteCheckNodeCtMismatch
                ( IfteNewEstRoot , LNewLinesRefMeat , "New copied" , TRUE ) 
            ; IfteRblAdvanceOldLinesRef ( )
            ; IfteCheckNodeCtMismatch
                ( IfteOldEstRoot , IfteRblCurOldLinesRefMeat , "Old copied:" )
              (* And loop, since we copied a LinesRef.  Copy Marks only if we
                 did not copy a LinesRef. *) 
            END (* IF copy a LinesRef. *)

          ; IF IfteRblCurOldMarkMeat # NIL
               AND IfteRblCmpLinesToMark # - 1 (* >= *) 
               (* Mark's TokMark is <= LinesRef's. *)
            THEN

              (* Copy this Mark and all Marks having equal LmTokMark. *) 
              IF IfteRblCmpLinesToMark = 0
              THEN IfteRblMarkLinesRefMeat 
                     := NARROW 
                          ( IfteRMNewLinesRef 
                          , PaintHs . LinesRefMeatTyp 
                          ) (* Nothik can goo wroong. *) 
              ELSE IfteRblMarkLinesRefMeat := NIL 
              END (* IF *) 
            ; LOOP 
                LNewMarkMeat := NEW ( PaintHs . LineMarkMeatTyp )
              ; LNewMarkMeat . LmLinesRef := IfteRblMarkLinesRefMeat 
              ; LNewMarkMeat . LmWindowRef
                  := IfteRblCurOldMarkMeat . LmWindowRef 
              ; LNewMarkMeat . LmMarkSs
                  := IfteRblCurOldMarkMeat . LmMarkSs
              ; IF IfteRblCurOldMarkMeat . LmWindowRef # NIL 
                   AND MarkSsTyp . MarkSsStartSel
                   <= IfteRblCurOldMarkMeat . LmMarkSs
                   AND IfteRblCurOldMarkMeat . LmMarkSs
                       <= MarkSsTyp . MarkSsEndSel
                THEN IfteNewMarks [ IfteRblCurOldMarkMeat . LmMarkSs ] 
                       := IfteRblCurOldMarkMeat . LmWindowRef
                            . WrMarks [ IfteRblCurOldMarkMeat . LmMarkSs ]
(* REVIEW       ^ *) 
                END (* IF *)

              ; IF IfteRblMarkLinesRefMeat = NIL
                THEN
                  LNewMarkMeat . LmTokMark
                    := IfteRblCurOldMarkMeat . LmTokMark
                ; IfteRblPatchTokMark ( LNewMarkMeat . LmTokMark , "Mark" ) 
                ELSE 
                  LNewMarkMeat . LmTokMark
                    := IfteRblMarkLinesRefMeat . LrBolTokMark
                END (* IF *) 

              ; LNewMarkMeat . LmCharPos
                  := IfteRblCurOldMarkMeat . LmCharPos 
              ; LNewMarkMeat . LmLineNo
                  := IfteRblCurOldMarkMeat . LmLineNo

(* CHECK: Do we want to assert .TmNodeCt = node ct in new tree? *)

              ; IfteRblRMNewMark . LmRightLink := LNewMarkMeat
              ; LNewMarkMeat . LmLeftLink := IfteRblRMNewMark
              ; IfteRblRMNewMark := LNewMarkMeat 
              ; IfteRblPrevOldMarkMeat := IfteRblCurOldMarkMeat
              ; IF IfteRblCurOldMarkMeat . LmRightLink = IfteOldMarkHeader
                THEN
                  IfteRblCurOldMarkMeat := NIL
                ; EXIT (* Inner LOOP *) 
                ELSE
                  IfteRblCurOldMarkMeat 
                    := NARROW 
                         ( IfteRblCurOldMarkMeat . LmRightLink 
                         , PaintHs . LineMarkMeatTyp 
                         ) (* ^NARROW w'oont phail. *)
                ; IF NOT Marks . Equal
                           ( IfteRblPrevOldMarkMeat . LmTokMark
                           , IfteRblCurOldMarkMeat . LmTokMark
                           )
                  THEN EXIT (* Inner LOOP. *) 
                  ELSE (* And go around inner LOOP. *) 
                  END (* IF *) 
                END (* IF *) 
              END (* Inner LOOP *) 
            (* And go around outer LOOP. *) 
            END (* IF *) 
          END (* LOOP *)
        END IfteRblCopyNodes
        
    ; BEGIN (* IfteRebuildLists *)
        VAR LNewLinesRefMeat : PaintHs . LinesRefMeatTyp
      ; VAR LNewMarkMeat : PaintHs . LineMarkMeatTyp  
      ; VAR LCmpMarks : [ - 1 .. 1 ]
      ; BEGIN (* Block. *) 
          IfteNewMarks := PaintHs . MarkArrayTyp { NIL , .. }

        (* Set up new LinesRef listHeader: *) 
        ; IF IfteOldLinesRefHeader = NIL
          THEN (* No old LinesRef list. *) 
            NewLinesRefHeader := NIL
          ; IfteRblCurOldLinesRefMeat := NIL
(* Check: How do we get out of this? *) 
          ELSE 
            NewLinesRefHeader := NEW ( PaintHs . LinesRefHeaderTyp )
          ; IF IfteOldLinesRefHeader = IfteOldLinesRefHeader . LrRightLink 
            THEN (* Old LinesRefList is empty. *) 
              NewLinesRefHeader . LrLeftLink := NewLinesRefHeader 
            ; NewLinesRefHeader . LrRightLink := NewLinesRefHeader 
            ; IfteRblCurOldLinesRefMeat := NIL
            ELSE IfteRblCurOldLinesRefMeat
                   := IfteOldLinesRefHeader . LrRightLink
            END (* IF *) 
          END (* IF *) 
        ; IfteRMNewLinesRef := NewLinesRefHeader 

        (* Set up new Mark list header: *) 
        ; IF IfteOldMarkHeader = NIL
          THEN (* No old Marks list. *) 
            NewMarkHeader := NIL
          ; IfteRblCurOldMarkMeat := NIL 
(* Check: How do we get out of this? *) 
          ELSE
            NewMarkHeader := NEW ( PaintHs . LineMarkHeaderTyp )
          ; IF IfteOldMarkHeader = IfteOldMarkHeader . LmRightLink 
            THEN (* Empty old Marks list. *) 
              NewMarkHeader . LmLeftLink := NewMarkHeader 
            ; NewMarkHeader . LmRightLink := NewMarkHeader 
            ; IfteRblCurOldMarkMeat := NIL
            ELSE 
              IfteRblCurOldMarkMeat := IfteOldMarkHeader . LmRightLink
            END (* IF *) 
          END (* IF *) 
        ; IfteRblRMNewMark := NewMarkHeader

        ; IfteRblToNodeNoOfFromTokMark
            := OldFromTokMark . TkmEstNodeNo + OldFromTokMark . TkmEstNodeCt
        ; IfteRblToNodeNoOfToTokMark
            := OldToTokMark . TkmEstNodeNo + OldToTokMark . TkmEstNodeCt

        (* Copy things that are to the left of the merged region. *)
        ; IfteRblCopyNodes
            ( CopyToTokMark := OldFromTokMark , Side := SideTyp . Left ) 

        (* Rebuild LinesRefs and Marks  inside the merged region. *)
        ; IfteBuildMergedLinesRefs ( IfteLinesRefArray )
          (* ^Lines for the merged region of the Est. *) 
        ; IfteVerifyMergedLinesRefs ( IfteLinesRefArray )
          (* ^Verify that they can be regenerated from the Est. *) 

        (* Link the merged LinesRefs into the new list. *)
        ; LNewLinesRefMeat := NIL
(* CHECK: What if there are no new linesRefs? *) 
        ; FOR RLinesRefSs := MarkIdTyp . MiLeadingLine
              TO MarkIdTyp . MiTrailingLine 
          DO WITH WNewLinesRefMeat = IfteLinesRefArray [ RLinesRefSs ]
            DO IF WNewLinesRefMeat # NIL
              THEN
                IF LNewLinesRefMeat = NIL
                THEN LNewLinesRefMeat := WNewLinesRefMeat
                     (* ^The leftmost merged region LinesRef to update.
                        IfteRMNewLinesRef will soon be the rightmost.
                     *) 
                END (* IF *) 
              ; IfteRMNewLinesRef . LrRightLink := WNewLinesRefMeat 
              ; WNewLinesRefMeat . LrLeftLink := IfteRMNewLinesRef
              ; WNewLinesRefMeat . LrRightLink := NIL
                (* ^To detect going off end of Marks list. *) 
              ; IfteRMNewLinesRef := WNewLinesRefMeat
; NoteNewLinesRef ( WNewLinesRefMeat ) 
              END (* IF *) 
            END (* WITH *) 
          END (* FOR *)
          (* All the new LinesRefs in the merged region are now linked
             on, and IfteRMNewLinesRef is the rightmost.
             LNewLinesRefMeat is the leftmost, and will move rightward.
          *) 
        ; IfteCheckNodeCtMismatch
            ( IfteOldEstRoot , IfteRblCurOldLinesRefMeat , "Old merged:" ) 

        (* Construct new LinesRefs and Marks within the merged region. *) 
        ; IfteRbl0LineNoOfCurOldLinesRef := 0 
        ; IfteRblIn2ndOldLinesRef := FALSE 
        ; IfteRblPrevNewMergeToLineNo := 0 
        ; IF IfteRblCurOldLinesRefMeat = NIL (* Off end of old LinesRef list. *)
          THEN IfteRblLastLineNoOf1stOldLinesRef := 0 
          ELSE IfteRblLastLineNoOf1stOldLinesRef  
                 := Display . ActualNoOfLines
                      ( IfteRblCurOldLinesRefMeat . LrLineCt )
                    - 1
            (* ^Last line no of 1st old LinesRef. *) 
          END (* IF *) 
        ; IfteRblCurNewMergeToLineNo
            := Display . ActualNoOfLines ( LNewLinesRefMeat . LrLineCt ) 

        ; LOOP (* Thru marks in the merged region. *) 
            IF IfteRblCurOldMarkMeat = NIL 
            THEN LCmpMarks := 1 
            ELSE  
              TRY 
                LCmpMarks 
                  := Marks . Compare
                       ( IfteRblCurOldMarkMeat . LmTokMark
                       , IfteOldThruLinesRef . LrBolTokMark
                       )
              EXCEPT Marks . Unordered 
              => <* ASSERT FALSE , "Marks.Unordered." *> 
              END (* EXCEPT *)
            END (* IF *)

          ; IF LCmpMarks = 1 (* Next Mark is to right of merge region. *)
            THEN EXIT (* No more marks in the merge region. *) 
            ELSE
              IF NOT IfteRblIn2ndOldLinesRef
                 AND DelNlShift # LbeStd . LimitedCharNoInfinity
                 AND Marks . Compare
                       ( IfteRblCurOldMarkMeat . LmTokMark
                       , IfteRblCurOldLinesRefMeat . LrBolTokMark
                       )
                     = 1 (* > *)
(* TODO: Catch Marks . Unordered. *) 
              THEN (* Advance to 2nd old LinesRef. *) 
                IfteRbl0LineNoOfCurOldLinesRef   
                  := Display . ActualNoOfLines
                       ( IfteRblCurOldLinesRefMeat . LrLineCt )
                     - 1 
              ; IfteRblAdvanceOldLinesRef ( )
              ; IfteCheckNodeCtMismatch
                  ( IfteOldEstRoot , IfteRblCurOldLinesRefMeat , "Old merged:" )
              ; IfteRblIn2ndOldLinesRef := TRUE 
              END (* IF *)

            (* Compute new line no. and character position within the merged
               region for this Mark: *) 
            ; IfteRblMergeLineNo
                := IfteRbl0LineNoOfCurOldLinesRef
                     + IfteRblCurOldMarkMeat . LmLineNo
            ; IF IfteRblMergeLineNo = IfteRblLastLineNoOf1stOldLinesRef  
              THEN IF IfteRblIn2ndOldLinesRef
                THEN IfteRblNewCharPos
                       := DelNlShift + IfteRblCurOldMarkMeat . LmCharPos
                ELSE IfteRblNewCharPos
                       := MIN ( IfteRblCurOldMarkMeat . LmCharPos , DelNlShift )
                END (*IF *) 
              ELSE IfteRblNewCharPos := IfteRblCurOldMarkMeat . LmCharPos
              END (* IF *)

              (* Skip any already linked on new LinesRefs to left of
                 IfteRblMergeLineNo. *)
            ; WHILE IfteRblCurNewMergeToLineNo <= IfteRblMergeLineNo 
              DO LNewLinesRefMeat := LNewLinesRefMeat . LrRightLink

; IF LNewLinesRefMeat = NIL
  THEN
    DelNlShift := DelNlShift (* Breakpoint. *) 
  END (* IF *) 



              ; <* ASSERT LNewLinesRefMeat # NIL
                , "Missing new lines ref."
                *>
                IfteRblPrevNewMergeToLineNo := IfteRblCurNewMergeToLineNo
              ; INC ( IfteRblCurNewMergeToLineNo
                    , Display . ActualNoOfLines
                        ( LNewLinesRefMeat . LrLineCt )
                    )
;  NoteNewLinesRef ( LNewLinesRefMeat ) 
              END (* WHILE *)

            (* Construct a new Mark: *) 
            ; LNewMarkMeat := NEW ( PaintHs . LineMarkMeatTyp )
            ; LNewMarkMeat . LmLinesRef := LNewLinesRefMeat 
            ; LNewMarkMeat . LmWindowRef
                := IfteRblCurOldMarkMeat . LmWindowRef 
            ; LNewMarkMeat . LmMarkSs
                := IfteRblCurOldMarkMeat . LmMarkSs 
            ; IF IfteRblCurOldMarkMeat . LmWindowRef # NIL 
                 AND MarkSsTyp . MarkSsStartSel
                     <= IfteRblCurOldMarkMeat . LmMarkSs
                 AND IfteRblCurOldMarkMeat . LmMarkSs
                     <= MarkSsTyp . MarkSsEndSel 
              THEN IfteNewMarks [ IfteRblCurOldMarkMeat . LmMarkSs ] 
                     := IfteRblCurOldMarkMeat . LmWindowRef
                        . WrMarks [ IfteRblCurOldMarkMeat . LmMarkSs ]
              END (* IF *)
(* Check: Is there anything to be done to VisibleIn set? *) 
            ; LNewMarkMeat . LmTokMark
                := LNewLinesRefMeat . LrBolTokMark 
            ; LNewMarkMeat . LmCharPos := IfteRblNewCharPos  
            ; LNewMarkMeat . LmLineNo
                := IfteRblMergeLineNo - IfteRblPrevNewMergeToLineNo 

            (* Advance to next old Mark: *) 
            ; IfteRblPrevOldMarkMeat := IfteRblCurOldMarkMeat
            ; IF IfteRblCurOldMarkMeat . LmRightLink = IfteOldMarkHeader
              THEN IfteRblCurOldMarkMeat := NIL
              ELSE
                IfteRblCurOldMarkMeat 
                  := NARROW 
                       ( IfteRblCurOldMarkMeat . LmRightLink 
                       , PaintHs . LineMarkMeatTyp 
                       ) (* ^NARROW shur too werk. *)
              END (* IF *) 

            (* Link in the new mark: *) 
            ; IfteRblRMNewMark . LmRightLink := LNewMarkMeat
            ; LNewMarkMeat . LmLeftLink := IfteRblRMNewMark
            ; IfteRblRMNewMark := LNewMarkMeat

            END (* IF *)
          END (* LOOP *)

        (* Advance old LinesRef beyond the merged region. *) 
        ; IF NOT IfteRblIn2ndOldLinesRef
             AND DelNlShift # LbeStd . LimitedCharNoInfinity
          THEN (* There are two LinesRefs in the merged region.  The first was 
                  not advanced-over previously, for lack of a Mark pointing
                  to the second, so advance over it now.  *) 
            IfteRblAdvanceOldLinesRef ( )
          END (* IF *) 
        ; IfteRblAdvanceOldLinesRef ( ) 
        ; IfteCheckNodeCtMismatch
            ( IfteOldEstRoot , IfteRblCurOldLinesRefMeat , "Old merged:" )

        (* Adjust things right of the merged region: *)
        ; IfteRblCopyNodes
            ( CopyToTokMark := Marks . TokMarkEOI , Side := SideTyp . Right ) 

        (* Complete the lists *) 
        ; IF NewLinesRefHeader # NIL
          THEN
            IfteRMNewLinesRef . LrRightLink := NewLinesRefHeader 
          ; NewLinesRefHeader . LrLeftLink := IfteRMNewLinesRef
          END (* IF *) 

        ; IF NewMarkHeader # NIL
          THEN
            IfteRblRMNewMark . LmRightLink := NewMarkHeader 
          ; NewMarkHeader . LmLeftLink := IfteRblRMNewMark
          END (* IF *)
        END (* Block for IfteRebuildLists *) 
      END IfteRebuildLists

  ; BEGIN (* InnerFlushTempEdit *) 
      VAR LEditedLinesRefMeat : PaintHs . LinesRefMeatTyp 
    ; VAR LPredOfEditedLinesRefs : PaintHs . LinesRefTyp 
    ; VAR LSuccOfEditedLinesRefs : PaintHs . LinesRefTyp 
      (* If two LinesRefs joined, successor of the second joined one. *)  
    ; VAR LLinesRefMeat : PaintHs . LinesRefMeatTyp 
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
        IF TempEditRef . TeDelFromPos = LbeStd . LimitedCharNoInfinity 
           (* There is no temp editing to flush. *) 
        THEN RETURN
        END (* IF *)
        
      (* I think it is now always true that LinesRef 
         = TempEditRef . TeLinesRef. 
         But for some calls, the argument is rather tangled, so leave these 
         as separate parameters.  Eventually remove the LinesRef parameter 
         and this assertion.
      *) 
      ; Assert 
          ( LinesRef = TempEditRef . TeLinesRef 
          , AFT . A_TextEdit_InnerFlushTempEdit_ParameterMismatch 
          ) 

      ; IfteImagePers := ImageRef . ItPers 
      ; IfteOldEstRoot := IfteImagePers . IpEstRoot 
      ; IfteOldLinesRefHeader := IfteImagePers . IpLineHeaderRef
      ; IfteOldMarkHeader := IfteImagePers . IpMarkHeader
      ; LEditedLinesRefMeat := TempEditRef . TeLinesRef 
      ; Assert 
          ( NOT LEditedLinesRefMeat . LrGapAfter 
          , AFT . A_InnerFlushTempEdit_EditingWithGapAfter   
          ) 
      ; LPredOfEditedLinesRefs := LEditedLinesRefMeat . LrLeftLink 
      ; IfteSecondOldLinesRef := NIL 
      ; LSecondOldActualLineCt := 0 
      ; IF Display . LinesRefIsEndOfImage ( ImageRef , LEditedLinesRefMeat ) 
        THEN (* LEditedLinesRefMeat is the empty EOI one. *) 
        (* Do not unlink anything, not even LinesRef. *) 
          IfteOldFromLinesRefMeat := NIL 
        ; IfteOldThruLinesRef := NIL 
        ; IfteFirstOldActualLineCt := TempEditRef . TeLineNo + 1 
        ; LSuccOfEditedLinesRefs := LEditedLinesRefMeat . LrRightLink   
          (* ^Which will be the list header. *) 
        ; DelNlShift := LbeStd . LimitedCharNoInfinity 
          (* ^Callers may ensure this, but if not, it's robust. *) 
        ; IfteSuccLinesRef := LEditedLinesRefMeat
        ; IfteSuccLinesRefMeat := LEditedLinesRefMeat 
        ELSE (* Not editing in EOI. *) 
          IfteOldFromLinesRefMeat := LEditedLinesRefMeat  
        ; IfteFirstOldActualLineCt 
            := Display . ActualNoOfLines ( LEditedLinesRefMeat . LrLineCt ) 
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
              IfteOldThruLinesRef := IfteOldFromLinesRefMeat
            ; LSuccOfEditedLinesRefs := LEditedLinesRefMeat . LrRightLink 
            END (* IF *) 
          ELSE 
            CantHappen 
              ( AFT . A_InnerFlushTempEdit_NonEoiLinesRefIsLast ) 
          ; IfteOldThruLinesRef := IfteOldFromLinesRefMeat
          ; LSuccOfEditedLinesRefs := LEditedLinesRefMeat . LrRightLink 
          END (* TYPECASE *) 
        END (* IF *) 

      ; TYPECASE LSuccOfEditedLinesRefs 
        OF NULL (* Can happen with damaged checkpoint files. *) 
        => LineMarks . GetEndOfImage 
             ( IfteImagePers . IpLang
             , IfteOldEstRoot
             , (*OUT*) NewMark := IfteOldEndBolTokMark
             ) 
        | PaintHs . LinesRefMeatTyp ( TSuccLinesRefMeat ) 
        => IfteOldEndBolTokMark := TSuccLinesRefMeat . LrBolTokMark 
        ELSE 
          LineMarks . GetEndOfImage 
            ( IfteImagePers . IpLang
            , IfteOldEstRoot
            , (*OUT*) NewMark := IfteOldEndBolTokMark
            ) 
        END (* TYPECASE *) 
      ; IfteStartBolTokMark := Marks . TokMarkNull  
      ; IF Options . TreeBrowsing 
        THEN
          TreeBrowse . Browse 
            ( IfteOldEstRoot , IfteImagePers . IpLang 
            , "Before MergeTextEdit " 
            ) 
        END (* IF *)

      ; IF DelNlShift = LbeStd . LimitedCharNoInfinity
         AND InsNlPos = LbeStd . LimitedCharNoInfinity
         AND ( TempEditRef . TeDelFromPos = LbeStd . LimitedCharNoInfinity 
               OR TempEditRef . TeDelToPos = TempEditRef . TeDelFromPos
                  AND TempEditRef . TeInsLen = 0
             )
(* TODO: Check how this ever happened.  test14_142, in Edit_Cut. *)  
        THEN RETURN
        END (* IF *) 

      ; MergeTxt . MergeTextEdit 
          ( Lang := IfteImagePers . IpLang 
          , EstRootRef := IfteOldEstRoot 
          , StartTokMark := LEditedLinesRefMeat . LrBolTokMark 
          , EndTokMark := IfteOldEndBolTokMark 
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
          , (* VAR *) NewLinesCt := IfteNewLinesRefCt 
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
          , (* IN OUT *) IfteOldFromLinesRefMeat 
          , LLeadingBlankLinesIncluded 
          ) 
      ; SkipBlankLinesToRight 
          ( ImageRef 
          , (* IN OUT *) IfteOldThruLinesRef 
          , LTrailingBlankLinesIncluded 
          ) 

      (* From the edited text, compute what the new Est is expected to
         produce. *) 
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
(* FIXME: Must account for whether each old line was already blank. *) 
            IfteExpectedBlankLinesBefore 
              := LLeadingBlankLinesIncluded 
                 + IfteFirstOldActualLineCt 
                 - 2 
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
        ; IfteSuccLinesRefMeat := NIL 
        ; TYPECASE IfteSuccLinesRef 
          OF NULL =>
          | PaintHs . LinesRefMeatTyp ( TSuccLinesRefMeat ) 
          => IfteSuccLinesRefMeat := TSuccLinesRefMeat
          ELSE 
          END (* TYPECASE *) 
        END (* IF *) 

      ; IfteRebuildLists
          ( OldEstRoot := IfteOldEstRoot
          , OldFromTokMark := LEditedLinesRefMeat . LrBolTokMark 
          , OldToTokMark := IfteOldEndBolTokMark 
          , NewEstRoot := IfteNewEstRoot
          , NewFromTokMark := IfteStartBolTokMark 
          , NewLinesRefHeader := (*VAR*) IfteNewLinesRefHeader
          , NewMarkHeader := (*VAR*) IfteNewMarkHeader 
          ) 

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
          ( IfteLinesRefsAccountedFor = IfteNewLinesRefCt  
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
            ; IF LWindowRef . WrFirstLineLinesRef = LEditedLinesRefMeat 
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
              DO WITH WNewLinesRefMeat = IfteLinesRefArray [ RLinesRefSs ] 
                 DO IF WNewLinesRefMeat # NIL 
                    THEN 
                      LLineCt 
                        := Display . ActualNoOfLines ( WNewLinesRefMeat . LrLineCt )
                    ; IF LLineNoInWindow + LLineCt > 0 
                         AND LLineNoInWindow 
                             < EditWindow . SouthEastToCorner 
                                 ( LWindowRef ) 
                               . v 
                      THEN (* Some portion of new line is visible. *) 
                        PaintHs . IncludeLrVisibleIn 
                          ( WNewLinesRefMeat , LWindowRef . WrWindowNo ) 
                      ; IF LLineNoInWindow <= 0 
                        THEN (* This will be the first item visible 
                                in the window. *) 
                          LWindowRef . WrFirstLineLinesRef 
                            := WNewLinesRefMeat 
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
                               , LinesRef := WNewLinesRefMeat 
                               ) 
                          END (* CASE *) 
                        END (* IF *) 
                      END (* IF *) 
                    ; INC ( LLineNoInWindow , LLineCt ) 
                    END (* IF *) 
                 END (* WITH WNewLinesRefMeat *) 
              END (* FOR *) 

            (* Do any needed repainting of shifted lines below the 
               site of the new LinesRefs. *) 
            ; IF IfteRMNewLinesRef . LrRightLink # IfteNewLinesRefHeader 
                 AND LRepaintKind 
                     IN RepaintKindSetTyp 
                          { RepaintKindTyp . RepaintKindShiftUp 
                          , RepaintKindTyp . RepaintKindShiftDown 
                          } 
                 AND LLineNoInWindow 
                     < EditWindow . SouthEastToCorner ( LWindowRef ) . v 
              THEN 
                LLinesRefMeat := IfteRMNewLinesRef . LrRightLink 
              ; LOOP 
                  IF LLineNoInWindow 
                     >= EditWindow . SouthEastToCorner ( LWindowRef ) . v 
                  THEN EXIT 
                  ELSE 
                    Display . SecureSucc ( ImageRef , LLinesRefMeat ) 
                  ; IF LLinesRefMeat . LrRightLink = IfteNewLinesRefHeader   
                    THEN (* We know NOT PaintHs.LrGapAfter( LLinesRefMeat ), 
                            so this means LLinesRefMeat is to EndOfImage. *) 
                      WHILE LLineNoInWindow 
                            < EditWindow . SouthEastToCorner ( LWindowRef ) . v 
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
                      LLineCt := LLinesRefMeat . LrLineCt 
                    ; IF LLineCt = 0 
                      THEN 
                        Display . PaintUneditedNonblankLine 
                          ( LWindowRef , LLineNoInWindow , LLinesRefMeat ) 
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
                    ; IF LLinesRefMeat . LrRightLink = IfteNewLinesRefHeader
                      THEN EXIT
ELSIF NOT ISTYPE ( LLinesRefMeat . LrRightLink , PaintHs . LinesRefMeatTyp )
THEN
EVAL LLinesRefMeat

                      ELSE LLinesRefMeat := LLinesRefMeat . LrRightLink
                      END (* IF *) 
                    END (* IF *) 
                  END (* IF *) 
                END (* LOOP still visible in new window state *) 
              ; IF LRepaintKind = RepaintKindTyp . RepaintKindShiftDown 
                THEN (* At most one LinesRef can become 
                        not visible in window. *) 
                  Display . MakeLinesRefsNotVisibleInWindow 
                    ( ImageRef 
                    , LLinesRefMeat 
                    , LLinesRefMeat 
                    , LWindowRef . WrWindowNo 
                    ) 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          ; LWindowRef := LWindowRef . WrImageLink 
          END (* WHILE LWindowRef # NIL *) 
        ; BruteForceRepairLineMarks ( IfteNewMarkHeader )
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
      END (* Block for InnerFlushTempEdit *) 
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
    ; MinSize : INTEGER := 0 
    ) 

  = VAR LOldSize : INTEGER 
  ; VAR LNewSize : INTEGER 
  ; VAR LOldRef : PaintHs . TextAttrArrayRefTyp 

  ; BEGIN
      IF ArrayRef = NIL 
      THEN 
        ArrayRef 
          := NEW ( PaintHs . TextAttrArrayRefTyp 
                 , MinSize + TextAttrArrayTempGrowthSize 
                 ) 
      ; FOR RI := 0 TO NUMBER ( ArrayRef ^ ) - 1 
        DO ArrayRef ^ [ RI ] := PaintHs . TextAttrDefault 
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
          DO ArrayRef ^ [ RI ] := PaintHs . TextAttrDefault 
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
    ; RmAllowedFromPos : LbeStd . CharNoTyp 
      (* The FromPos of the previously-edited region, if any, of the returned 
         TempEditRef, must be no farther right than this.  *) 
    ; LmAllowedToPos : LbeStd . CharNoTyp 
      (* The ToPos of the previously-edited region, if any, of the returned 
         TempEditRef, must be no farther left than this.  *)
      (* This is very confusing.  The '*Allowed*' positions are limits on
         what can be already edited, in order to ensure the future
         contiguity of caller's intended edits to the range
         from LmAllowedToPos to RmAllowedFromPos. *) 
(* TODO: Someday figure out a less confusing way to rename these formals. *)
    ; VAR (* OUT *) TempEditRef : PaintHs . TempEditRefTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

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
                 AND ( TempEditRef . TeDelFromPos > RmAllowedFromPos  
                       OR TempEditRef . TeDelFromPos 
                          + TempEditRef . TeInsLen 
                          < LmAllowedToPos 
                     ) 
                 (* Outside the already edited region. *) 
           THEN 
             InnerFlushTempEdit 
               ( ImageTrans , TempEditRef . TeLinesRef , TempEditRef ) 
           ; TempEditRef := LImagePers . IpTempEditRef 
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
  ; VAR LSuccLen : LbeStd . CharNoTyp 
  ; VAR LDelLen : LbeStd . CharNoTyp 

  ; BEGIN 
      LLinesRef := TempEditRef . TeLinesRef 
    ; IF SuccLinesRef # NIL AND NOT SuccLinesRef . LrGapAfter 
      THEN LSuccLen := SuccLinesRef . LrLineLen 
      ELSE LSuccLen := 0 
      END (* IF *) 
    ; IF TempEditRef . TeDelFromPos = LbeStd . LimitedCharNoInfinity 
      THEN LDelLen := 0 
      ELSE LDelLen := TempEditRef . TeDelToPos - TempEditRef . TeDelFromPos 
      END (* IF *) 
    ; Assert
        ( LLinesRef . LrLineLen - LDelLen + TempEditRef . TeInsLen + LSuccLen 
          = Strings . Length ( TempEditRef . TeEditedString ) 
        , AFT . A_AssertTempEdit_Wrong_length
        )  
    END AssertTempEdit 

; CONST CharPoint01 = EditWindow . CharPointTyp { 0 , 1 } 
; CONST CharPoint0Minus1 = EditWindow . CharPointTyp { 0 , - 1 } 

; PROCEDURE SimpleDeleteChar 
    ( WindowRef : PaintHs . WindowRefTyp
      (* ^PRE: WindowRef.WrImageRef.ItPers are all Non-NIL *)
    ; MarkSs : PaintHs . MarkSsTyp 
    ; VAR (* IN OUT *) MustRepaintWindow : BOOLEAN 
    ) 
  RAISES { Backout , Thread . Alerted }
  (* Delete the character at te cursor location. *) 

  = VAR LCursorMark : PaintHs . LineMarkMeatTyp
  ; VAR LImageTrans : PaintHs . ImageTransientTyp  
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LTempEditLength : LbeStd . LimitedCharNoTyp 
  ; VAR LSuccLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LSavedTempEditRef : PaintHs . TempEditRefTyp 
  ; VAR LSavedTempEditState : PaintHs . TempEditStateTyp 
  ; VAR LTrailingBlankCount : Strings . StringSsTyp 

  ; BEGIN (* SimpleDeleteChar *)
      LImageTrans := WindowRef . WrImageRef
    ; LImagePers := LImageTrans . ItPers  
    ; LCursorMark := WindowRef . WrMarks [ MarkSs ] 
    ; GrabTempEditRef 
        ( LImageTrans 
        , LCursorMark 
        , RmAllowedFromPos := LCursorMark . LmCharPos + 1 
        , LmAllowedToPos := LCursorMark . LmCharPos 
        , (* VAR *) TempEditRef := LTempEditRef 
        ) 
    ; LTempEditLength 
        := Strings . Length ( LTempEditRef . TeEditedString ) 
    ; IF LCursorMark . LmCharPos >= LTempEditLength 
      THEN (* Past last char of line, delete the Nl at end of line. *) 
        IF Display . LinesRefIsEndOfImage
             ( LImageTrans , LCursorMark . LmLinesRef ) 
        THEN (* Deleting Nl beyond EOI.  Do nothing. *) 
        ELSE 
          Display . SecureSucc ( LImageTrans , LCursorMark . LmLinesRef ) 
        ; LSuccLinesRef := LCursorMark . LmLinesRef . LrRightLink 
          (* ^NARROW can't fail. *) 
        ; IF Display . LinesRefIsEndOfImage ( LImageTrans , LSuccLinesRef ) 
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
                ( LTempEditRef . TeDelToPos
                  = LCursorMark . LmLinesRef . LrLineLen 
                , AFT . A_SimpleDeleteChar_DelNlOutOfRange 
                ) 
            ; LTrailingBlankCount  
                := MAX ( 0 
                       , LCursorMark . LmCharPos - LTempEditLength 
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
                ( LImageTrans 
                , LCursorMark . LmLinesRef 
                , LTempEditRef 
                , DelNlShift := LCursorMark . LmCharPos 
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
      ELSE (* Delete char within the line. *) 
        LSavedTempEditRef := CopyOfTempEditRef ( LTempEditRef ) 
      ; LSavedTempEditState := LImagePers . IpTempEditState 
      ; TRY 
          IF LTempEditRef . TeDelFromPos = LbeStd . LimitedCharNoInfinity 
          THEN (* No changes have been made yet to this line. *) 
            LTempEditRef . TeDelFromPos := LCursorMark . LmCharPos 
          ; LTempEditRef . TeDelToPos := LCursorMark . LmCharPos + 1 
          ELSIF LCursorMark . LmCharPos + 1 = LTempEditRef . TeDelFromPos 
          THEN (* Deleting char immediately to left of current deleted 
                  region. *) 
            DEC ( LTempEditRef . TeDelFromPos ) 
          ELSIF LCursorMark . LmCharPos 
                = ( LTempEditRef . TeDelFromPos + LTempEditRef . TeInsLen )
          THEN (* Deleting char immediately to right of current deleted 
                  region. *) 
            INC ( LTempEditRef . TeDelToPos ) 
          ELSIF LCursorMark . LmCharPos >= LTempEditRef . TeDelFromPos 
                AND LCursorMark . LmCharPos 
                    < LTempEditRef . TeDelFromPos + LTempEditRef . TeInsLen 
          THEN (* Deleting within the inserted text. *) 
            DEC ( LTempEditRef . TeInsLen ) 
          ELSE (* Outside current region.  Flush and start over. *) 
            InnerFlushTempEdit 
              ( LImageTrans 
              , LCursorMark . LmLinesRef 
              , LTempEditRef 
              ) 
          ; InitTempEditForText 
              ( LTempEditRef 
              , LTempEditRef . TeLinesRef 
              , LTempEditRef . TeLineNo 
              ) 
          ; LTempEditRef . TeDelFromPos := LCursorMark . LmCharPos 
          ; LTempEditRef . TeDelToPos := LCursorMark . LmCharPos + 1 
          END (* IF *) 
        ; TRY 
            Strings . DeleteCharsInPlace 
              ( LTempEditRef . TeEditedString , LCursorMark . LmCharPos , 1 )
          EXCEPT Strings . SsOutOfBounds
          => CantHappen 
               ( AFT . A_SimpleDeleteChar_String_subscript_out_of_bounds ) 
          END (* TRY EXCEPT *) 
        ; DeleteTextAttr 
            ( LTempEditRef . TeTextAttrArrayRef 
            , LTempEditRef . TeTextAttrActualSize 
            , LCursorMark . LmCharPos 
            , 1 
            , LTempEditLength 
            ) 
        ; AssertTempEdit ( LTempEditRef ) 
        ; AdjustMarksOnLine ( LCursorMark , - 1 ) 
        ; LImagePers . IpTempEditState 
            := PaintHs . TempEditStateTyp . TeStateText 
        ; Display . NoteImageSavedState ( LImageTrans , FALSE ) 
        ; Display . NoteImageParsedState ( LImageTrans , FALSE ) 
        ; Display . NoteImageAnalyzedState ( LImageTrans , FALSE ) 
        ; IF NOT MustRepaintWindow 
          THEN 
            PaintTempEditedLineInAllWindows 
              ( LImageTrans , LCursorMark . LmLinesRef , LTempEditRef ) 
          END (* IF *) 
        EXCEPT 
        Backout ( EMessage ) 
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
    END SimpleDeleteChar 

(* EXPORTED: *)
; PROCEDURE DeleteChar 
    ( WindowRef : PaintHs . WindowRefTyp ; DeletingBwd : BOOLEAN ) 
  RAISES { Backout , Thread . Alerted } 

(* TODO: Update versions when no new tree is built.  
         (InnerFlushTempEdit does it when tree _is_ rebuilt.) 
*) 

  = VAR LCursorMark : PaintHs . LineMarkMeatTyp
  ; VAR LImageTrans : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LActualMovement : EditWindow . CharPointTyp 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 
  ; VAR LMustRepaintWindow : BOOLEAN 
  
  ; BEGIN (* DeleteChar *) 
      LMustRepaintWindow := FALSE
    ; IF WindowRef = NIL THEN RETURN END (* IF *)
    ; LImageTrans := WindowRef . WrImageRef
    ; IF LImageTrans = NIL THEN RETURN END (* IF *)
    ; LImagePers := LImageTrans . ItPers  
    ; LCursorMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ] 

    ; IF LCursorMark . LmLinesRef # NIL 
      THEN (* An image is open in the window *) 
        (* The following tests will prevent grabbing the TempEditRef 
           away from some other line, or creating a new one, in cases 
           where we can tell beforehand that the delete doesn't do 
           anything anyway. *) 
        IF DeletingBwd 
        THEN 
          IF LCursorMark . LmCharPos = 0 
          THEN (* Trying to join with previous line. *) 
            IF Options . DelNlAtBOL 
               AND NOT Display . LinesRefIsBegOfImage 
                         ( WindowRef . WrImageRef 
                         , LCursorMark . LmLinesRef 
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
            (* LCursorMark will have contents changed, but same object. *) 
            ; Assert 
                ( LActualMovement = CharPoint0Minus1 
                , AFT . A_DeleteCharDelNlAtBol 
                ) 
            ; Display . HorizMoveCursorWindowRef 
                ( WindowRef 
                , LCursorMark . LmLinesRef . LrLineLen 
                , LMustRepaintWindow 
                ) 
            ; TRY 
                SimpleDeleteChar 
                  ( WindowRef
                  , MarkSsTyp . MarkSsCursor
                  , (* IN OUT *) LMustRepaintWindow
                  ) 
              EXCEPT 
              Backout ( EMessage ) 
              => (* Rollback cursor move. *)
                Display . HorizMoveCursorWindowRef 
                  ( WindowRef 
                  , - LCursorMark . LmLinesRef . LrLineLen 
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
                     # LCursorMark . LmLinesRef 
                  OR LImagePers . IpTempEditRef . TeLineNo 
                     # LCursorMark . LmLineNo 
                ) 
                AND LCursorMark . LmCharPos 
                    > LCursorMark . LmLinesRef . LrLineLen 
          THEN (* Beyond EOL. No effect except cursor movement. *) 
            Display . HorizMoveCursorWindowRef 
              ( WindowRef , - 1 , LMustRepaintWindow ) 
          ; IF NOT LMustRepaintWindow 
            THEN 
              EditWindow . SetCursorPosition 
                ( WindowRef 
                , LCursorMark . LmCharPos - WindowRef . WrHorizScroll 
                , WindowRef . WrCursorLineNoInWindow 
                ) 
            ; EditWindow . PaintCursorCoordinates ( WindowRef ) 
            END (* IF *) 
          ELSE (* Plain delete backward. *) 
            Display . HorizMoveCursorWindowRef 
              ( WindowRef , - 1 , LMustRepaintWindow ) 
          ; TRY 
              SimpleDeleteChar 
                ( WindowRef
                , MarkSsTyp . MarkSsCursor
                , (* IN OUT *) LMustRepaintWindow
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
               ( WindowRef . WrImageRef , LCursorMark . LmLinesRef ) 
             AND ( LImagePers . IpTempEditState 
                   = PaintHs . TempEditStateTyp . TeStateIdle 
                   OR LImagePers . IpTempEditRef . TeLinesRef 
                      # LCursorMark . LmLinesRef 
                   OR LCursorMark . LmLineNo 
                      > LImagePers . IpTempEditRef . TeLineNo 
                 ) 
          THEN (* We know we are out beyond all text of the image. 
                  there is no action. *) 
            RETURN 
          ELSE 
            SimpleDeleteChar 
              ( WindowRef
              , MarkSsTyp . MarkSsCursor
              , (* IN OUT *) LMustRepaintWindow
              ) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *)

    ; IF LMustRepaintWindow 
      THEN 
        Display . PaintWindowFromLines 
          ( WindowRef  
          , (* VAR *) LTrailingBlankLines (* Dead. *) 
          , (* VAR *) LLinesRef (* Dead. *) 
          , (* VAR *) LLineNo  (* Dead. *) 
          )
(* CHECK: 1) Can we paint only this line? 
          2) Shouldn't we just mark the window for repainting? 
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

  = VAR LCursorMark : PaintHs . LineMarkMeatTyp
  ; VAR LImageTrans : PaintHs . ImageTransientTyp  
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

    ; IF WindowRef = NIL THEN RETURN END (* IF *)
    ; LImageTrans := WindowRef . WrImageRef
    ; IF LImageTrans = NIL THEN RETURN END (* IF *)
    ; LImagePers := LImageTrans . ItPers  
    ; LCursorMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ] 

; PaintHs . BruteForceVerifyLineMarks ( LImageTrans )

    ; IF ( NewChar = ' ' OR NewChar = Ascii . ht ) 
         AND ( LImagePers . IpTempEditState 
               = PaintHs . TempEditStateTyp . TeStateIdle 
               OR LImagePers . IpTempEditRef . TeLinesRef 
                  # LCursorMark . LmLinesRef 
               OR LImagePers . IpTempEditRef . TeLineNo 
                  # LCursorMark . LmLineNo 
             ) 
         AND LCursorMark . LmCharPos 
             >= LCursorMark . LmLinesRef . LrLineLen 
      THEN (* Insert/overlay with blank(s), beyond EOL. *) 
        Display . HorizMoveCursorWindowRef 
          ( WindowRef , 1 , LMustRepaintWindow ) 
      ELSE 
        IF NewChar = LbeStd . CharNewLine 
           OR NewChar = LbeStd . CharReturn  
        THEN (* Inserted new line. *) 
          LDoInBlankLineBefore := FALSE 
        ; IF LCursorMark . LmLinesRef . LrBolTokMark . TkmKind 
             # MarkKindTyp . BlankLine 
             AND LCursorMark . LmCharPos 
                 <= LCursorMark . LmLinesRef . LrFromPos 
          THEN 
            TYPECASE LCursorMark . LmLinesRef . LrLeftLink 
            OF PaintHs . LinesRefMeatTyp ( TPredLinesRef ) 
            => IF NOT TPredLinesRef . LrGapAfter 
                  AND TPredLinesRef . LrBolTokMark . TkmKind 
                      = MarkKindTyp . BlankLine 
                  AND NOT TPredLinesRef . LrBolTokMark . TkmStartAtEnd 
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
            (* Contents of LCursorMark will change, but same object *) 
          ; Assert 
              ( LActualMovement = CharPoint0Minus1 
              , AFT . A_InsertOrOverlayChar_MoveCursorUp 
              ) 
          END (* IF *) 

        ; GrabTempEditRef 
            ( LImageTrans 
            , LCursorMark 
            , RmAllowedFromPos 
                := LCursorMark . LmCharPos + ORD ( NOT IsInsert ) 
            , LmAllowedToPos := LCursorMark . LmCharPos 
            , (* VAR *) TempEditRef := LTempEditRef 
            ) 
        ; LSavedTempEditRef := CopyOfTempEditRef ( LTempEditRef ) 
        ; LSavedTempEditState := LImagePers . IpTempEditState 
        ; TRY 
            IF LTempEditRef . TeDelFromPos 
               = LbeStd . LimitedCharNoInfinity 
            THEN (* No changes have been made yet to this line. *) 
              LTempEditRef . TeDelFromPos := LCursorMark . LmCharPos 
            ; LTempEditRef . TeDelToPos := LCursorMark . LmCharPos 
            ; IF NewChar = LbeStd . CharReturn
              THEN LIndentPos := LTempEditRef . TeDelFromPos 
(* FIX: ^Decide what the rules here should really be. *) 
              ELSE LIndentPos := 0 
              END (* IF *)
            ELSIF LCursorMark . LmCharPos < LTempEditRef . TeDelFromPos 
                  OR LCursorMark . LmCharPos 
                     > LTempEditRef . TeDelFromPos 
                       + LTempEditRef . TeInsLen 
            THEN (* Inserted Nl can not be combined with existing 
                    temp edits. *) 
              InnerFlushTempEdit 
                ( LImageTrans 
                , LCursorMark . LmLinesRef 
                , LTempEditRef 
                ) 
            ; InitTempEditForText 
                ( LTempEditRef 
                , LTempEditRef . TeLinesRef 
                , LTempEditRef . TeLineNo 
                ) 
            ; LTempEditRef . TeDelFromPos := LCursorMark . LmCharPos 
            ; LTempEditRef . TeDelToPos := LCursorMark . LmCharPos 
            ; LCursorMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ] 
            ; IF NewChar = LbeStd . CharReturn 
              THEN LIndentPos := LTempEditRef . TeDelFromPos 
(* FIX: ^Decide what the rules here should really be. *) 
              ELSE LIndentPos := 0 
              END (* IF *)
            ELSE (* Can combine inserted Nl with existing edits. *)  
              IF LCursorMark . LmCharPos 
                 <= LCursorMark . LmLinesRef . LrFromPos 
              THEN (* Cursor in white space on left of line. *)  
                LIndentPos := LCursorMark . LmCharPos 
              ELSIF LCursorMark . LmLinesRef . LrBolTokMark . TkmKind 
                    = MarkKindTyp . BlankLine 
                    AND NOT LCursorMark . LmLinesRef . LrBolTokMark 
                            . TkmStartAtEnd 
              THEN (* Cursor inside blank line(s). *) 
                IF LTempEditRef . TeDelFromPos 
                   = LbeStd . LimitedCharNoInfinity 
                THEN (* No changes have been made yet to this line. *) 
                  LIndentPos := LCursorMark . LmCharPos 
                ELSE 
                  LIndentPos := LTempEditRef . TeDelFromPos 
                END (* IF *) 
              ELSE 
                LIndentPos 
                  := TravUtil . IndentPosOfBolTokMark 
                       ( LImagePers . IpLang 
                       , LImagePers . IpEstRoot 
                       , LCursorMark . LmLinesRef . LrBolTokMark 
                       ) 
              END (* IF *) 
            ; LIndentPos := LTempEditRef . TeDelFromPos 
(* FIX: ^Decide what the rules here should really be. *) 
            END (* IF *) 
          ; InnerFlushTempEdit 
              ( LImageTrans 
              , LCursorMark . LmLinesRef 
              , LTempEditRef 
              , InsNlPos := LCursorMark . LmCharPos 
              , NlIndentPos := LIndentPos 
              ) 

          ; LMustRepaintWindow := TRUE 
          ; InitTempEditForText ( LTempEditRef , NIL , 0 ) 
          ; LImagePers . IpTempEditState 
              := PaintHs . TempEditStateTyp . TeStateIdle
          ; LCursorMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ] 

(*        ; LWantedMovement . h := LIndentPos - LCursorMark . LmCharPos 
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
            , LCursorMark 
            , RmAllowedFromPos 
                := LCursorMark . LmCharPos + ORD ( NOT IsInsert ) 
            , LmAllowedToPos := LCursorMark . LmCharPos 
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
                  := MIN ( LLength , LCursorMark . LmCharPos ) 
              ; LTempEditRef . TeDelToPos 
                  := LTempEditRef . TeDelFromPos  
              ELSIF LCursorMark . LmCharPos 
                    < LTempEditRef . TeDelFromPos - ORD ( NOT IsInsert )
                    OR LCursorMark . LmCharPos 
                       > LTempEditRef . TeDelFromPos 
                         + LTempEditRef . TeInsLen 
              THEN (* Insertion cannot be combined with existing 
                           temp edits. *) 
                InnerFlushTempEdit 
                  ( LImageTrans 
                  , LCursorMark . LmLinesRef 
                  , LTempEditRef 
                  ) 
              ; InitTempEditForText 
                  ( LTempEditRef 
                  , LTempEditRef . TeLinesRef 
                  , LTempEditRef . TeLineNo 
                  ) 
              ; LTempEditRef . TeDelFromPos 
                  := MIN ( LLength , LCursorMark . LmCharPos ) 
              ; LTempEditRef . TeDelToPos 
                  := LTempEditRef . TeDelFromPos 
              ; LCursorMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ] 
              END (* IF *) 
            ; IF LCursorMark . LmCharPos >= LLength 
              THEN (* Inserting/overlaying beyond end of line. *) 
                LInsLength := LCursorMark . LmCharPos - LLength + 1 
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
                  , LCursorMark . LmCharPos 
                  , LInsLength  
                  , LLength 
                  , PaintHs . TextAttrTyped  
                  ) 
              ; INC ( LTempEditRef . TeInsLen , LInsLength ) 
              ; AdjustMarksOnLine ( LCursorMark , 1 ) 
                (* ^Can this matter beyond end of line? *) 
              ELSIF IsInsert 
              THEN 
                TRY 
                  Strings . InsertBlanksInPlace 
                    ( LTempEditRef . TeEditedString 
                    , LCursorMark . LmCharPos 
                    , 1 
                    ) 
                EXCEPT Strings . SsOutOfBounds
                => CantHappen 
                     ( AFT . A_InsertOrOverlayChar_String_subscript_out_of_bounds_for_blanks ) 
                END (* TRY EXCEPT *) 
              ; InsertTextAttr 
                  ( LTempEditRef . TeTextAttrArrayRef 
                  , LTempEditRef . TeTextAttrActualSize 
                  , LCursorMark . LmCharPos 
                  , 1 
                  , LLength 
                  , PaintHs . TextAttrTyped  
                  ) 
              ; INC ( LTempEditRef . TeInsLen ) 
              ; AdjustMarksOnLine ( LCursorMark , 1 ) 
              ELSE (* Overlay char within the previously edited line. *)
                SetTextAttr 
                  ( LTempEditRef . TeTextAttrArrayRef 
                  , LTempEditRef . TeTextAttrActualSize 
                  , LCursorMark . LmCharPos 
                  , 1 
                  , LineLen := LLength 
                  , NewAttr := PaintHs . TextAttrTyped  
                  ) 
              ; IF LCursorMark . LmCharPos 
                   = LTempEditRef . TeDelFromPos - 1 
                THEN (* Char immediately left of previous edited region. *) 
                  DEC ( LTempEditRef . TeDelFromPos ) 
                ; INC ( LTempEditRef . TeInsLen ) 
                ELSIF LCursorMark . LmCharPos 
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
                  , LCursorMark . LmCharPos 
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
(* CHECK: 1) Can we only paint this line? 
          2) Shouldn't we just mark the window for repainting? 
*)             
        ELSE 
          PaintTempEditedLineInAllWindows 
            ( LImageTrans , LCursorMark . LmLinesRef , LTempEditRef ) 
        END (* IF *) 
      END (* IF blank typed beyond EOL *) 

; PaintHs . BruteForceVerifyLineMarks ( LImageTrans ) 

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
      IF WindowRef = NIL THEN RETURN END (* IF *)
      
(* TODO: Do this less brutishly.  Build a whole TempEdit for each
         line maybe, or at least don't repaint after every char.
*) 
    ; LLen := Text . Length ( String ) 
    ; FOR RI := 0 TO LLen - 1 
      DO 
        WITH WCh = Text . GetChar ( String , RI ) 
        DO 
          CASE WCh 
          OF LbeStd . CharNewLine 
          => InsertOrOverlayChar ( WindowRef , WCh , IsInsert ) 
          ; Display . HorizMoveCursorWindowRef 
              ( WindowRef 
              , - WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ] 
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

  = VAR LCursorMark : PaintHs . LineMarkMeatTyp
  ; VAR LImageTrans : PaintHs . ImageTransientTyp 
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
      IF WindowRef = NIL THEN RETURN END (* IF *)
    ; LImageTrans := WindowRef . WrImageRef
    ; IF LImageTrans = NIL THEN RETURN END (* IF *)
    ; LImagePers := LImageTrans . ItPers  
    ; LCursorMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ]

    ; IF LCursorMark # NIL AND LCursorMark . LmLinesRef # NIL 
      THEN
        IF LCursorMark . LmCharPos = 0 
        THEN 
          Display . Beep ( Errors . ErrorTyp . ETransposeCharsAtBOL ) 
        ELSIF LCursorMark . LmCharPos 
              > Display . NonblankLengthOfCurrentLine ( WindowRef ) 
        THEN (* Transpose in blank space to right of line, no action. *) 
          Display . HorizMoveCursorWindowRef 
            ( WindowRef , 1 , LMustRepaintWindow ) 
        ELSE 
          GrabTempEditRef 
            ( LImageTrans 
            , LCursorMark 
            , RmAllowedFromPos := LCursorMark . LmCharPos + 1 
            , LmAllowedToPos := LCursorMark . LmCharPos - 1 
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
                  := LCursorMark . LmCharPos - 1 
              ; LTempEditRef . TeDelToPos 
                  := MIN ( LLength , LCursorMark . LmCharPos + 1 ) 
              ; LTempEditRef . TeInsLen := 2 
              ; LLeftCharCt := 0 
              ; LRightCharCt := 2 
              ELSE 
                LLeftCharCt 
                  := MAX ( LTempEditRef . TeDelFromPos 
                           - LCursorMark . LmCharPos  
                           + 1 
                         , 0 
                         ) 
                (* No of affected chars to right of previosly edited. *)
              ; DEC ( LTempEditRef . TeDelFromPos , LLeftCharCt ) 
              ; INC ( LTempEditRef . TeInsLen , LLeftCharCt ) 
              ; LRightCharCt 
                  := MAX ( LCursorMark . LmCharPos 
                           - ( LTempEditRef . TeDelFromPos 
                               + LTempEditRef . TeInsLen 
                             ) 
                           + 1 
                         , 0 
                         ) 
                (* ^No of affected chars to left of previosly edited. *)
              ; LTempEditRef . TeDelToPos 
                  := MIN ( LTempEditRef . TeDelToPos + LRightCharCt 
                         , LLength
                         ) 
              ; INC ( LTempEditRef . TeInsLen , LRightCharCt ) 
              END (* IF *) 
            ; LCh1 := Strings . IthChar 
                        ( LTempEditRef . TeEditedString 
                        , LCursorMark . LmCharPos - 1 
                        ) 
            ; IF LCursorMark . LmCharPos = LLength 
              THEN 
                LCh2 := ' ' 
              ; Strings . AppendCharInPlace 
                  ( LTempEditRef . TeEditedString , ' ' ) 
              ELSE 
                LCh2 := Strings . IthChar 
                          ( LTempEditRef . TeEditedString 
                          , LCursorMark . LmCharPos  
                          ) 
              END (* IF *) 
            ; Strings . StoreIthChar 
                ( LTempEditRef . TeEditedString 
                , LCursorMark . LmCharPos - 1  
                , LCh2 
                ) 
            ; Strings . StoreIthChar 
                ( LTempEditRef . TeEditedString 
                , LCursorMark . LmCharPos   
                , LCh1 
                ) 
            ; SetTextAttr 
                ( LTempEditRef . TeTextAttrArrayRef 
                , LTempEditRef . TeTextAttrActualSize 
                , LCursorMark . LmCharPos - 1 
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
                  , LCursorMark . LmLinesRef 
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
            LCursorMark 
            = WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ] 
          DO
            IF LCursorMark # NIL AND LCursorMark . LmLinesRef # NIL 
            THEN
              LLength := Display . NonblankLengthOfCurrentLine ( WindowRef ) 
            ; IF LLength = 0 AND LCursorMark . LmCharPos = 0 
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
              ELSIF LCursorMark . LmCharPos >= LLength  
              THEN (* Deleting in white space to right. No action. *) 
              ELSE 
                GrabTempEditRef 
                  ( LImageTrans 
                  , LCursorMark 
                  , RmAllowedFromPos := LbeStd . CharNoInfinity 
                  , LmAllowedToPos := LCursorMark . LmCharPos 
                  , (* VAR *) TempEditRef := LTempEditRef 
                  ) 
              ; LSavedTempEditRef := CopyOfTempEditRef ( LTempEditRef ) 
              ; LSavedTempEditState := LImagePers . IpTempEditState 
              ; LDeleteFromPos := LCursorMark . LmCharPos 
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
                    , LCursorMark . LmLinesRef 
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
      IF ToCharPos = LbeStd . LimitedCharNoInfinity THEN RETURN END (* IF *) 
    ; IF ToCharPos <= FromCharPos THEN RETURN END (* IF *) 
    ; LImagePers := ImageTrans . ItPers  
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
      => (* Rollback changes to temp edit on the way out. *)
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
    ( WindowRef : PaintHs . WindowRefTyp
    ; MarkSs1 : PaintHs . MarkSsTyp 
    ; MarkSs2 : PaintHs . MarkSsTyp
      (* Delete from the leftmore of the MarkSs's *thru* the line beginning at
         the other (including its left new line), but only *to* its LmCharPos. *)  
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LCursorMark : PaintHs . LineMarkMeatTyp
  ; VAR LFromMarkSs : PaintHs . MarkSsTyp 
  ; VAR LThruMarkSs : PaintHs . MarkSsTyp
  ; VAR LFromMark : PaintHs . LineMarkMeatTyp 
  ; VAR LThruMark : PaintHs . LineMarkMeatTyp
  ; VAR LImageTrans : PaintHs . ImageTransientTyp   
  ; VAR LTempEditRef : PaintHs . TempEditRefTyp   
  ; VAR LFromPosInLine : LbeStd . CharNoTyp 
  ; VAR LLength : LbeStd . CharNoTyp 
  ; VAR LLinesInvolved : [ 1 .. 3 ]  (* 3 denotes >= 3 *)
  ; VAR LLinesToLeft : INTEGER
  ; VAR LNewCursorLineNo : INTEGER 
  ; VAR LTempMark : PaintHs . LineMarkMeatTyp 
  ; VAR LPredMark : PaintHs . LineMarkTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineShift : LbeStd . LineNoTyp 
  ; VAR LRMLineEditedString : Strings . T 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 
  ; VAR LMustRepaintWindow : BOOLEAN 

  ; BEGIN 
      IF WindowRef = NIL THEN RETURN END (* IF *)
    ; LImageTrans := WindowRef . WrImageRef
    ; IF LImageTrans = NIL THEN RETURN END (* IF *)

    ; LMustRepaintWindow := FALSE 
    ; CASE
        PaintHs . CompareLineMarks
          ( WindowRef . WrMarks [ MarkSs1 ] , WindowRef . WrMarks [ MarkSs2 ] ) 
      OF - 1
      =>  LFromMarkSs := MarkSs1
        ; LThruMarkSs := MarkSs2
        
      | 0
      =>  SimpleDeleteChar
           ( WindowRef , MarkSs1 , (* IN OUT *) LMustRepaintWindow )
          (* The following duplicates code in DeleteChar. *) 
        ; IF LMustRepaintWindow 
          THEN 
            Display . PaintWindowFromLines 
              ( WindowRef  
              , (* VAR *) LTrailingBlankLines (* Dead. *) 
              , (* VAR *) LLinesRef (* Dead. *) 
              , (* VAR *) LLineNo  (* Dead. *) 
              )
(* CHECK: 1) Can we paint only this line? 
          2) Shouldn't we just mark the window for repainting? 
*)      
          END (* IF *) 

      | 1
      =>  LFromMarkSs := MarkSs2
        ; LThruMarkSs := MarkSs1
      END (* CASE *)
    ; LFromMark := WindowRef . WrMarks [ LFromMarkSs ] 
    ; LThruMark := WindowRef . WrMarks [ LThruMarkSs ] 

    ; Display . SecureSucc ( LImageTrans , LFromMark . LmLinesRef ) 
    ; Display . SecurePred ( LImageTrans , LThruMark . LmLinesRef ) 
    ; Display . SecureSucc ( LImageTrans , LThruMark . LmLinesRef ) 
    ; IF Marks . Equal ( LFromMark . LmTokMark , LThruMark . LmTokMark ) 
      THEN 
        LFromPosInLine := LFromMark . LmCharPos 
      ; LLinesInvolved := 1 
      ELSE 
        LFromPosInLine := 0 (* Prepare to do lines right-to-left. *) 
      ; IF LThruMark . LmLinesRef . LrLeftLink = LFromMark . LmLinesRef 
        THEN LLinesInvolved := 2 
        ELSE LLinesInvolved := 3 (* Or more. *) 
        END (* IF *) 
      END (* IF *) 

    (* Adjust any LineMarks in the to-be-deleted text range, to point
       to the beginning of the deletion. *) 
    ; LLinesRef := LFromMark . LmLinesRef
    ; LLinesToLeft := 0  
    ; LTempMark := LFromMark . LmRightLink
    ; WindowRef . WrMarks [ MarkSsTyp . MarkSsTemp ] := LTempMark 
    ; LOOP (* Thru the involved Marks. *) 
        IF LTempMark = LThruMark THEN EXIT END (* IF *)
      ; IF LTempMark . LmMarkSs = MarkSsTyp . MarkSsCursor 
        THEN (* It's the cursor.  Adjust WrCursorLineNoInWindow. *)
(* FIXME ^ Do _all_ windows. *) 
          WITH WCursorLineNo
               = LTempMark . LmWindowRef . WrCursorLineNoInWindow
          DO LNewCursorLineNo 
               := MAX ( 0
                      , WCursorLineNo
                        - LLinesToLeft
                        - LTempMark . LmLineNo
                        + LFromMark . LmLineNo
                      ) 
          ; WCursorLineNo := LNewCursorLineNo
          END (* WITH *)
        END (* IF *)

      (* Patch LTempMark's fields in place. *) 
      ; LTempMark . LmTokMark := LFromMark . LmTokMark 
      ; LTempMark . LmLinesRef := LFromMark . LmLinesRef 
      ; LTempMark . LmLineNo := LFromMark . LmLineNo
      ; LTempMark . LmCharPos := LFromMark . LmCharPos

      ; TYPECASE LTempMark . LmRightLink 
        OF NULL => (* Shouldn't happen. *) EXIT (* Marks loop. *) 
        | PaintHs . LineMarkMeatTyp ( TLineMarkMeat )
        =>  LTempMark := TLineMarkMeat
          ; WindowRef . WrMarks [ MarkSsTyp . MarkSsTemp ] := LTempMark 
          (* And loop. *)
        ELSE EXIT (* Marks loop. *) 
        END (* TYPECASE *) 

      (* Count displayed lines between LFromMark and short of LTempMark. *) 
      ; LOOP (* Thru LinesRefs about to be left behind. *) 
          IF LLinesRef = LTempMark . LmLinesRef THEN EXIT END (* IF *) 
        ; INC ( LLinesToLeft 
              , Display . ActualNoOfLines ( LLinesRef . LrLineCt ) 
              )
        ; TYPECASE LLinesRef . LrRightLink
          OF NULL => (* Shouldn't happen. *) EXIT (* LinesRefs loop. *) 
          | PaintHs . LinesRefMeatTyp ( TLinesRefMeat )
          => LLinesRef := TLinesRefMeat (* And loop. *)
          ELSE EXIT (* LinesRefs loop. *) 
          END (* TYPECASE *) 
        END (* LOOP Thru LinesRefs. *) 

      END (* LOOP Thru Marks. *)

    (* Relink any adjusted marks to left of LFromMark. These now all point to
       the same place as LFromMark, but have different addresses.  This will
       restore LFromMark.LmTokMark's status as the rightmost of a group of
       joined new lines. *)
    ; IF LFromMark . LmRightLink # LThruMark
      THEN (* There are adjusted marks to move. *) 
        PaintHs . UnlinkLineMark ( LFromMark )
      ; PaintHs . LinkLineMarkToLeft
          ( InsertToLeftOfRef := LThruMark , RefToInsert := LFromMark )
      ; PaintHs . BruteForceVerifyLineMarks ( LImageTrans ) 
      END (* IF *) 

    (* Delete selected portion of rightmost included line. *)
    ; IF FALSE AND LThruMark . LmCharPos = 0
      THEN  (* For LThruMark, only the new line at its beginning is to be
               deleted.  Do nothing for LThruMark.  The Mark to the left
               will delete LThruMark's new line. *) 
      ELSE (* Some characters to delete. *) 
        GrabTempEditRef 
          ( LImageTrans 
          , Mark := LThruMark 
          , RmAllowedFromPos := LThruMark . LmCharPos 
          , LmAllowedToPos := LFromPosInLine
            (* ^Zero, unless this is also the 1st line. *) 
          , (* OUT *) TempEditRef := LTempEditRef 
          ) 
      ; DeleteTempEditedCharRange 
          ( LImageTrans , LFromPosInLine , ToCharPos := LThruMark . LmCharPos ) 
      END (* IF *) 
    ; LRMLineEditedString := LTempEditRef . TeEditedString 

    (* Delete any full lines properly between LFromMark and LThruMark. *)
    ; IF LLinesInvolved = 3 (* Or more *) 
      THEN 
        LLinesRef := LThruMark . LmLinesRef . LrLeftLink 
      ; LTempMark := NEW ( PaintHs . LineMarkMeatTyp ) 
      ; LTempMark . LmWindowRef := NIL 
      ; LTempMark . LmMarkSs := MarkSsTyp . MarkSsTemp  
      ; WindowRef . WrMarks [ MarkSsTyp . MarkSsTemp ] := LTempMark 
      ; LTempMark . LmLinesRef := LLinesRef  
      ; LTempMark . LmTokMark := LLinesRef . LrBolTokMark 
      ; LTempMark . LmCharPos := 0 
      ; LTempMark . LmLineNo 
          := Display . ActualLineCtOfLinesRef ( LImageTrans , LLinesRef ) - 1 
      ; PaintHs . InsertLineMarkToLeft 
          ( InsertMark := LTempMark 
          , SuccMark := LThruMark 
          , Image := LImageTrans 
          ) 

      ; LOOP (* Thru LinesRefs and lines thereof. *) 
          Display . SecurePred ( LImageTrans , LLinesRef ) 
        ; LTempEditRef . TeLinesRef := LLinesRef 
        ; GrabTempEditRef 
            ( LImageTrans 
            , Mark := LTempMark 
            , RmAllowedFromPos := LbeStd . CharNoInfinity 
            , LmAllowedToPos := 0 
            , (* VAR *) TempEditRef := LTempEditRef 
            ) 
        ; LTempEditRef . TeEditedString := LRMLineEditedString  
        ; LTempEditRef . TeDelFromPos := 0 
        ; LTempEditRef . TeDelToPos 
            := Strings . Length ( LTempEditRef . TeEditedString ) 
        ; LTempEditRef . TeInsLen := 0 
        ; LTempEditRef . TeDelToPos := LLinesRef . LrLineLen 
; PaintHs . BruteForceVerifyLineMarks ( LImageTrans ) 
        ; InnerFlushTempEdit 
            ( LImageTrans , LLinesRef , LTempEditRef , DelNlShift := 0 ) 
; PaintHs . BruteForceVerifyLineMarks ( LImageTrans ) 
        ; InitTempEditForText ( LTempEditRef , NIL , 0 ) 
        ; LFromMark := WindowRef . WrMarks [ LFromMarkSs ] 
        ; LThruMark := WindowRef . WrMarks [ LThruMarkSs ] 
        ; LCursorMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ]
        ; LTempMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsTemp ]
        ; IF LTempMark . LmLineNo > 0 
          THEN DEC ( LTempMark . LmLineNo )
               (* And loop to do another blank line of LLinesRef. *) 
          ELSE 
            LLinesRef := LLinesRef . LrLeftLink 
          ; IF LLinesRef = LFromMark . LmLinesRef 
            THEN EXIT 
            ELSE 
              LTempMark . LmLinesRef := LLinesRef 
            ; LLinesRef . LrHasMark := TRUE
              (* ^LLinesRef will soon become garbage, so no need to reset
                 this FALSE when done. *) 
            ; LTempMark . LmTokMark := LLinesRef . LrBolTokMark 
            ; LTempMark . LmCharPos := 0 
            ; LTempMark . LmLineNo 
                := Display . ActualLineCtOfLinesRef ( LImageTrans , LLinesRef )
                   - 1
            (* And loop for this new LLinesRef. *) 
            END (* IF *) 
          END (* IF *)  
        END (* LOOP Thru LinesRefs and lines thereof. *) 
      ; WindowRef . WrMarks [ MarkSsTyp . MarkSsTemp ] := NIL 
      ; PaintHs . DeleteLineMark 
          ( Mark := LTempMark 
          , (* VAR *) PredMark := LPredMark (* Dead *) 
          , Image := LImageTrans 
          ) 
      ; PaintHs . BruteForceVerifyLineMarks ( LImageTrans ) 
      END (* IF *) 

    (* Delete the selected portion of leftmost line of selection. *) 
    ; IF LLinesInvolved >= 2 
      THEN
        GrabTempEditRef 
          ( LImageTrans 
          , Mark := LFromMark 
          , RmAllowedFromPos := LbeStd . CharNoInfinity 
          , LmAllowedToPos := LFromMark . LmCharPos 
          , (* OUT *) TempEditRef := LTempEditRef 
          ) 
      ; DeleteTempEditedCharRange 
          ( LImageTrans 
          , FromCharPos := LFromMark . LmCharPos 
          , ToCharPos := Strings . Length ( LTempEditRef . TeEditedString )
          ) 
      ; LTempMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsTemp ]
      ; LTempEditRef := LImageTrans . ItPers . IpTempEditRef
        (* ^Address could have changed. *) 
      ; TRY 
          Strings . InsertBlanksInPlace 
            ( LTempEditRef . TeEditedString 
            , PrefixLength 
                := Strings . Length ( LTempEditRef . TeEditedString )
            , BlankCount := LThruMark . LmLinesRef . LrFromPos 
            , EventualLengthHint 
                := LFromMark . LmCharPos + LThruMark . LmLinesRef . LrLineLen 
            )
        EXCEPT Strings . SsOutOfBounds
        => CantHappen 
             ( AFT . A_DeleteBetweenMarks_String_subscript_out_of_bounds_first_line ) 
        END (* TRY EXCEPT *) 
      ; Strings . AppendTextInPlace 
          ( LTempEditRef . TeEditedString 
          , LThruMark . LmLinesRef . LrLineText 
          ) 
      ; AssertTempEdit ( LTempEditRef , LThruMark . LmLinesRef )
      ; InnerFlushTempEdit 
          ( LImageTrans 
          , LFromMark . LmLinesRef 
          , LTempEditRef 
          , DelNlShift := LFromMark . LmCharPos  
          ) 
      ; LTempMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsTemp ]
      ; InitTempEditForText ( LTempEditRef , NIL , 0 ) 
      ; LFromMark := WindowRef . WrMarks [ LFromMarkSs ] 
      ; LThruMark := WindowRef . WrMarks [ LThruMarkSs ] 
      ; LCursorMark := WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ]
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
            LCursorMark 
            = WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ] 
          DO 
            GrabTempEditRef 
              ( WindowRef . WrImageRef 
              , LCursorMark 
              , RmAllowedFromPos := LCursorMark . LmCharPos  
              , LmAllowedToPos := LCursorMark . LmCharPos 
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
                LCursorMark 
                = WindowRef . WrMarks [ MarkSsTyp . MarkSsCursor ] 
              DO 
                IF LCursorMark . LmCharPos <= 0 
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
                        IF WAttr . TaCharPos > LCursorMark . LmCharPos  
                        THEN EXIT 
                        ELSE 
                          LRightAttrSs := LAttrSs  
                        ; IF WAttr . TaCharPos < LCursorMark . LmCharPos 
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
      DO WITH WNewLinesRefMeat = LinesRefArrayNull [ RLinesRefSs ] 
         DO WNewLinesRefMeat := NIL 
         END (* WITH WNewLinesRefMeat *) 
      END (* FOR *)
    END InitTextEdit 

; BEGIN (* TextEdit *) 
    InitTextEdit ( ) 
  END TextEdit 
. 
