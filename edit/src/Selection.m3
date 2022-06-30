
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Selection 

; IMPORT Fmt 
; IMPORT Point 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr 

; IMPORT AssertDevel 
; IMPORT Assertions 
; FROM Assertions IMPORT Assert 
; FROM Failures IMPORT Backout  
; IMPORT Display 
; IMPORT EditWindow 
; IMPORT LbeStd 
; IMPORT MessageCodes 
; IMPORT PaintHs 
; IMPORT UiRecPlay 

; TYPE AFT = MessageCodes . T 

(* VISIBLE: *) 
; PROCEDURE ClearSelection ( ) 
  RAISES { Backout , Thread . Alerted } 
  (* Does not clear preselection or SelText. *) 

  = VAR LOldSel : SelectionTyp 
  ; VAR LNewSel : SelectionTyp 
  ; VAR LLeftMark : PaintHs . LineMarkMeatTyp 
  ; VAR LRightMark : PaintHs . LineMarkMeatTyp 
  ; VAR LLeftMarkPred : PaintHs . LineMarkTyp 
  ; VAR LRightMarkPred : PaintHs . LineMarkTyp 

  ; BEGIN (* ClearSelection *) 
      LOldSel := Current 
    ; IF LOldSel # NIL 
      THEN 
        PaintHs . GetMarksInOrder 
          ( Mark1 := LOldSel . SelStartMark 
          , Mark2 := LOldSel . SelEndMark  
          , (* VAR *) Left := LLeftMark 
          , (* VAR *) Right := LRightMark 
          ) 
      ; LNewSel := NEW ( SelectionTyp ) 
      ; LNewSel . SelImage := NIL 
      ; LNewSel . SelStartMark := NIL 
      ; LNewSel . SelEndMark := NIL 
      ; LNewSel . PreselImage := LOldSel . PreselImage 
      ; LNewSel . PreselMark := LOldSel . PreselMark 
      ; LNewSel . SelText := NIL 
      ; PaintHs . DeleteLineMark 
          ( Mark := LLeftMark 
          , (* VAR *) PredMark := LLeftMarkPred 
          , Image := LOldSel . SelImage
          ) 
      ; Current := LNewSel 
      ; TRY 
          PaintHs . BruteForceVerifyLineMarks ( LOldSel . SelImage ) 
        ; IF LRightMark # LLeftMark 
          THEN (* Just defensive.  Shouldn't happen. *) 
            PaintHs . DeleteLineMark 
              ( Mark := LRightMark 
              , (* VAR *) PredMark := LRightMarkPred 
              , Image := LOldSel . SelImage 
              )  
          END (* IF *) 
        ; TRY 
            PaintHs . BruteForceVerifyLineMarks ( LOldSel . SelImage ) 
          ; IF LOldSel . SelImage # NIL 
               AND LLeftMark # NIL 
               AND LRightMark # NIL 
            THEN 
              Display . PaintLinesRangeAllWindows 
                ( ImageTrans := LOldSel . SelImage  
                , FromLinesRef := LLeftMark . LmLinesRef 
                , FromLineNo := LLeftMark . LmLineNo 
                , ThruLinesRef := LRightMark . LmLinesRef 
                , ThruLineNo := LRightMark . LmLineNo 
                ) 
            END (* IF *) 
          EXCEPT Thread . Alerted 
          => PaintHs . InsertLineMarkToRight 
              ( InsertMark := LRightMark 
              , PredMark := LRightMarkPred 
              , Image := LOldSel . SelImage 
              ) 
          ; RAISE Thread . Alerted 
          | Backout ( EArg ) 
          => PaintHs . InsertLineMarkToRight 
              ( InsertMark := LRightMark 
              , PredMark := LRightMarkPred 
              , Image := LOldSel . SelImage 
              ) 
          ; AssertDevel . WriteCheckpoint 
              ( ImageRef := LOldSel . SelImage 
              , Message := EArg  
              , DoCreateVersion := FALSE 
              ) 
            (* ^This will rewrite the checkpoint already written from inside
                AssertDevel, but with the changes undone. 
            *) 
          ; RAISE Backout ( EArg ) 
          END (* TRY EXCEPT *) 
        EXCEPT Thread . Alerted 
        => PaintHs . InsertLineMarkToRight 
            ( InsertMark := LLeftMark 
            , PredMark := LLeftMarkPred 
            , Image := LOldSel . SelImage 
            ) 
        ; Current := LOldSel 
        ; RAISE Thread . Alerted 
        | Backout ( EArg ) 
        => PaintHs . InsertLineMarkToRight 
            ( InsertMark := LLeftMark 
            , PredMark := LLeftMarkPred 
            , Image := LOldSel . SelImage 
            ) 
        ; Current := LOldSel 
        ; AssertDevel . WriteCheckpoint 
            ( ImageRef := LOldSel . SelImage 
            , Message := EArg  
            , DoCreateVersion := FALSE 
            ) 
          (* ^This will rewrite the checkpoint already written from inside
              AssertDevel, but with the changes undone. 
          *) 
        ; RAISE Backout ( EArg ) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END ClearSelection  

(* VISIBLE: *) 
; PROCEDURE Preselect ( Window : PaintHs . WindowRefTyp ) 
  RAISES { Backout , Thread . Alerted } <* NOWARN *>
  (* Notes the current cursor location as preselection.  Leaves any 
     actual selection alone, in case there is no drag done. 
  *) 

  = VAR LOldSel : SelectionTyp 
  ; VAR LNewSel : SelectionTyp 
  ; VAR LPreselPredMark : PaintHs . LineMarkTyp 
  ; VAR LPredMark : PaintHs . LineMarkTyp 

  ; BEGIN 
      IF Window # NIL AND Window . WrImageRef # NIL 
      THEN 
        LOldSel := Current 
      ; WITH WCursorMark 
             = Window . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
        DO 
          LNewSel := NEW ( SelectionTyp ) 
        ; LNewSel . PreselImage := Window . WrImageRef  
        ; LNewSel . PreselMark := NEW ( PaintHs . LineMarkMeatTyp )
        ; LNewSel . PreselMark . LmTokMark := WCursorMark . LmTokMark  
        ; LNewSel . PreselMark . LmLinesRef := WCursorMark . LmLinesRef  
        ; LNewSel . PreselMark . LmCharPos := WCursorMark . LmCharPos  
        ; LNewSel . PreselMark . LmLineNo := WCursorMark . LmLineNo   
        ; LNewSel . PreselMark . LmWindowRef := NIL 
        ; LNewSel . PreselMark . LmMarkSs 
            := PaintHs . MarkSsTyp . MarkSsStartSel 
        ; IF LOldSel = NIL 
          THEN 
            LNewSel . SelImage := NIL 
          ; LNewSel . SelStartMark := NIL 
          ; LNewSel . SelEndMark := NIL 
          ; LNewSel . SelText := NIL 
          ELSE 
            LNewSel . SelImage := LOldSel . SelImage 
          ; LNewSel . SelStartMark := LOldSel . SelStartMark 
          ; LNewSel . SelEndMark := LOldSel . SelEndMark 
          ; LNewSel . SelText := LOldSel . SelText 
          ; IF LOldSel . PreselMark # NIL 
               AND NOT KeepLeftoverMarks 
            THEN
              PaintHs . DeleteLineMark 
                ( Mark := LOldSel . PreselMark 
                , (* VAR *) PredMark := LPreselPredMark (* Dead *)  
                , Image := LOldSel . PreselImage
                ) 
            END (* IF *) 
          END (* IF *) 
        (* Must insert now, to prevent the LinesRef from being discarded: *) 
        ; PaintHs . InsertLineMarkToRight 
            ( InsertMark := LNewSel . PreselMark 
            , PredMark := WCursorMark 
            , Image := Window . WrImageRef 
            ) 
        ; Current := LNewSel 
        ; TRY 
            PaintHs . BruteForceVerifyLineMarks ( Window . WrImageRef ) 
          EXCEPT Backout ( EArg ) 
          => PaintHs . DeleteLineMark 
              ( Mark := LNewSel . PreselMark 
              , (* VAR *) PredMark := LPredMark (* Dead *) 
              , Image := Window . WrImageRef 
              )
          ; IF LOldSel # NIL 
               AND LOldSel . PreselMark # NIL 
               AND NOT KeepLeftoverMarks
            THEN 
              PaintHs . InsertLineMarkToRight 
                ( InsertMark := LOldSel . PreselMark 
                , PredMark := LPreselPredMark 
                , Image := Window . WrImageRef  
                ) 
            END (* IF *) 
          ; Current := LOldSel 
          ; AssertDevel . WriteCheckpoint 
              ( ImageRef := Window . WrImageRef  
              , Message := EArg  
              , DoCreateVersion := FALSE 
              ) 
            (* ^This will rewrite the checkpoint already written from inside
                AssertDevel, but with the changes undone. 
            *) 
          ; RAISE Backout ( EArg ) 
          END (* TRY EXCEPT *) 
        END (* WITH *) 
      END (* IF *) 
    END Preselect 

(* VISIBLE: *) 
; PROCEDURE DragSelection 
    ( Window : PaintHs . WindowRefTyp 
    ; AbsPosition : EditWindow . CharPointTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LCommandString : TEXT 
  ; VAR LOldEndPredMark : PaintHs . LineMarkTyp 
  ; VAR LNewEndPredMark : PaintHs . LineMarkTyp 

  ; VAR LOldSel : SelectionTyp 
  ; VAR LNewSel : SelectionTyp 
  ; VAR LPaintMark1 : PaintHs . LineMarkMeatTyp  
  ; VAR LLeftPaintMark : PaintHs . LineMarkMeatTyp  
  ; VAR LRightPaintMark : PaintHs . LineMarkMeatTyp  

  ; BEGIN 
      IF Window # NIL 
         AND Window . WrImageRef # NIL 
         AND NOT Point . Equal 
              ( EditWindow . CharPointTyp 
                  { Window . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
                    . LmCharPos 
                    - Window . WrHorizScroll 
                  , Window . WrCursorLineNoInWindow 
                  } 
              , AbsPosition
              ) (* Have really moved to a different character. *)  
      THEN 
        LCommandString 
         := UiRecPlay . BeginCommandPlusInt2 
              ( UiRecPlay . CommandTyp . SweepSelection 
              , AbsPosition . h 
              , AbsPosition . v 
              ) 
      ; Display . MoveCursorAbsoluteInsideWindow ( Window , AbsPosition ) 
      ; LNewSel := NEW ( SelectionTyp ) 
      ; LNewSel . SelText := NIL 
      ; LNewSel . PreselImage := NIL 
      ; LNewSel . PreselMark := NIL 
      ; IF Current . PreselImage # NIL 
        THEN (* This is the first drag of the sweep. *) 
          ClearSelection ( ) 
        ; LOldSel := Current 
        ; Assert 
            ( LOldSel . PreselImage = Window . WrImageRef 
            , AFT . A_DragSelection_Preselect_Image_changed  
            ) 
        ; LNewSel . SelImage := LOldSel . PreselImage
        ; Assert 
            ( LOldSel . PreselMark # NIL  
            , AFT . A_DragSelection_Preselect_No_Start  
            ) 
        ; LNewSel . SelStartMark := LOldSel . PreselMark 
        ; LPaintMark1 := LNewSel . SelStartMark 
        ; LOldEndPredMark := NIL 
        ELSE 
          LOldSel := Current 
        ; Assert 
            ( LOldSel . SelImage = Window . WrImageRef 
            , AFT . A_DragSelection_Image_changed  
            ) 
        ; LNewSel . SelImage := LOldSel . SelImage 
        ; Assert 
            ( LOldSel . SelStartMark # NIL  
            , AFT . A_DragSelection_No_Start  
            ) 
        ; LNewSel . SelStartMark := LOldSel . SelStartMark 
        ; LPaintMark1 := LOldSel . SelEndMark 
        ; PaintHs . DeleteLineMark 
            ( LOldSel . SelEndMark 
            , (* VAR *) PredMark := LOldEndPredMark 
            , Image := Window . WrImageRef 
            ) 
        ; TRY 
            PaintHs . BruteForceVerifyLineMarks ( Window . WrImageRef ) 
          EXCEPT Backout ( EArg ) 
          => PaintHs . InsertLineMarkToRight 
               ( InsertMark := LOldSel . SelEndMark 
               , PredMark := LOldEndPredMark 
               , Image := Window . WrImageRef 
               ) 
          ; AssertDevel . WriteCheckpoint 
              ( ImageRef := Window . WrImageRef  
              , Message := EArg  
              , DoCreateVersion := FALSE 
              ) 
            (* ^This will rewrite the checkpoint already written from 
                inside AssertDevel, but with the changes undone. 
            *) 
          ; RAISE Backout ( EArg ) 
          END (* TRY EXCEPT *) 
        END (* IF *) 
      ; WITH WCursorMark 
             = Window . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
        DO 
          IF PaintHs . CompareLineMarks 
               ( WCursorMark , LNewSel . SelStartMark ) = 0 
          THEN (* Selection is now empty. *) 
            LNewSel . PreselImage := LNewSel . SelImage 
          ; LNewSel . PreselMark := LNewSel . SelStartMark 
          ; LNewSel . SelStartMark := NIL 
          ; LNewSel . SelEndMark := NIL 
          ; LNewSel . SelImage := NIL 
          ; Current := LNewSel 
          ; TRY 
              PaintHs . GetMarksInOrder 
                ( Mark1 := LPaintMark1   
                , Mark2 := LNewSel . PreselMark  
                , (* VAR *) Left := LLeftPaintMark 
                , (* VAR *) Right := LRightPaintMark 
                ) 
            ; IF LLeftPaintMark # NIL AND LRightPaintMark # NIL 
              THEN 
                Display . PaintLinesRangeAllWindows 
                  ( ImageTrans := Window . WrImageRef 
                  , FromLinesRef := LLeftPaintMark . LmLinesRef 
                  , FromLineNo := LLeftPaintMark . LmLineNo 
                  , ThruLinesRef := LRightPaintMark . LmLinesRef 
                  , ThruLineNo := LRightPaintMark . LmLineNo 
                  ) 
              END (* IF *) 
            EXCEPT Thread . Alerted 
            => PaintHs . InsertLineMarkToRight 
               ( InsertMark := LOldSel . SelEndMark 
               , PredMark := LOldEndPredMark 
               , Image := Window . WrImageRef 
               ) 
            ; Current := LOldSel 
            ; RAISE Thread . Alerted 
            | Backout ( EArg ) 
            => PaintHs . InsertLineMarkToRight 
               ( InsertMark := LOldSel . SelEndMark 
               , PredMark := LOldEndPredMark 
               , Image := Window . WrImageRef 
               ) 
            ; Current := LOldSel 
            ; AssertDevel . WriteCheckpoint 
                ( ImageRef := Window . WrImageRef  
                , Message := EArg  
                , DoCreateVersion := FALSE 
                ) 
              (* ^This will rewrite the checkpoint already written from 
                  inside AssertDevel, but with the changes undone. 
              *) 
            ; RAISE Backout ( EArg ) 
            END (* TRY EXCEPT *) 
          ELSE 
            LNewSel . SelEndMark := NEW ( PaintHs . LineMarkMeatTyp ) 
          ; LNewSel . SelEndMark . LmTokMark := WCursorMark . LmTokMark  
          ; LNewSel . SelEndMark . LmLinesRef := WCursorMark . LmLinesRef  
          ; LNewSel . SelEndMark . LmCharPos := WCursorMark . LmCharPos  
          ; LNewSel . SelEndMark . LmLineNo := WCursorMark . LmLineNo   
          ; LNewSel . SelEndMark . LmWindowRef := NIL 
          ; LNewSel . SelEndMark . LmMarkSs 
              := PaintHs . MarkSsTyp . MarkSsEndSel 
          ; PaintHs . InsertLineMarkToRight 
              ( InsertMark := LNewSel . SelEndMark  
              , PredMark := WCursorMark 
              , Image := Window . WrImageRef 
              ) 
          ; Current := LNewSel 
          ; TRY 
              PaintHs . BruteForceVerifyLineMarks ( Window . WrImageRef ) 
            ; PaintHs . GetMarksInOrder 
                ( Mark1 := LPaintMark1   
                , Mark2 := LNewSel . SelEndMark 
                , (* VAR *) Left := LLeftPaintMark 
                , (* VAR *) Right := LRightPaintMark 
                ) 
            ; IF LLeftPaintMark # NIL AND LRightPaintMark # NIL 
              THEN 
                Display . PaintLinesRangeAllWindows 
                  ( ImageTrans := Window . WrImageRef 
                  , FromLinesRef := LLeftPaintMark . LmLinesRef 
                  , FromLineNo := LLeftPaintMark . LmLineNo 
                  , ThruLinesRef := LRightPaintMark . LmLinesRef 
                  , ThruLineNo := LRightPaintMark . LmLineNo 
                  ) 
              END (* IF *) 
            EXCEPT Thread . Alerted 
            => PaintHs . InsertLineMarkToRight 
                ( InsertMark := LOldSel . SelEndMark 
                , PredMark := LOldEndPredMark 
                , Image := Window . WrImageRef 
                ) 
            ; PaintHs . DeleteLineMark 
                ( Mark := LNewSel . SelEndMark 
                , (* VAR *) PredMark := LNewEndPredMark (* Dead *) 
                , Image := Window . WrImageRef 
                ) 
            ; Current := LOldSel 
            ; RAISE Thread . Alerted 
            | Backout ( EArg ) 
            => PaintHs . InsertLineMarkToRight 
                ( InsertMark := LOldSel . SelEndMark 
                , PredMark := LOldEndPredMark 
                , Image := Window . WrImageRef 
                ) 
            ; PaintHs . DeleteLineMark 
                ( Mark := LNewSel . SelEndMark 
                , (* VAR *) PredMark := LNewEndPredMark (* Dead *) 
                , Image := Window . WrImageRef 
                ) 
            ; Current := LOldSel 
            ; AssertDevel . WriteCheckpoint 
                ( ImageRef := Window . WrImageRef  
                , Message := EArg  
                , DoCreateVersion := FALSE 
                ) 
              (* ^This will rewrite the checkpoint already written from 
                  inside AssertDevel, but with the changes undone. 
              *) 
            ; RAISE Backout ( EArg ) 
            END (* TRY EXCEPT *) 
          END (* IF *) 
        END (* WITH *) 
      ; UiRecPlay . RecordString ( LCommandString ) 
      END (* IF *) 
    END DragSelection 

(* VISIBLE: *) 
; PROCEDURE ManifestSelectionAsText ( ) : TEXT 
  RAISES { Backout , Thread . Alerted } <* NOWARN *> 

  = VAR LSel : SelectionTyp 
  ; VAR LLeftMark : PaintHs . LineMarkMeatTyp 
  ; VAR LRightMark : PaintHs . LineMarkMeatTyp 
  ; VAR LThruLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoTyp 
  ; VAR LLineCt : LbeStd . LineNoTyp 
  ; VAR LFromPos : LbeStd . CharNoTyp 
  ; VAR LToPos : LbeStd . CharNoTyp 
  ; VAR LFromPosUsingText : LbeStd . CharNoTyp 
  ; VAR LResult : TEXT 

  ; BEGIN 
      LSel := Current 
    ; IF LSel # NIL 
         AND LSel . SelImage # NIL 
         AND LSel . SelStartMark # NIL 
         AND LSel . SelEndMark # NIL  
      THEN
        PaintHs . GetMarksInOrder 
          ( Mark1 := LSel . SelStartMark 
          , Mark2 := LSel . SelEndMark  
          , (* VAR *) Left := LLeftMark 
          , (* VAR *) Right := LRightMark 
          ) 
      ; LThruLinesRef := LRightMark . LmLinesRef 
      ; LLinesRef := LLeftMark . LmLinesRef 
      ; LLineNo := LLeftMark . LmLineNo 
      ; LLineCt := LLinesRef . LrLineCt 
      ; LFromPos := LLeftMark . LmCharPos 
      ; LResult := "" 
      ; LOOP 
          IF LLinesRef = LThruLinesRef 
          THEN 
            LToPos := LRightMark . LmCharPos 
          ELSE LToPos := LLinesRef . LrLineLen 
          END (* IF *)
        ; IF LLineCt > 0 (* In blank lines. *) 
             OR LLinesRef . LrLineText = NIL (* Otherwise blank. *) 
          THEN  
            IF LLinesRef = LThruLinesRef 
               AND LLineNo = LRightMark . LmLineNo 
            THEN 
              LResult := LResult & Fmt . Pad ( "" , LToPos - LFromPos ) 
            ; EXIT 
            ELSE 
              LResult := LResult & Wr . EOL 
            END (* IF *) 
          ELSE 
            IF LFromPos < LLinesRef . LrFromPos 
            THEN 
              LResult 
                := LResult 
                   & Fmt . Pad 
                       ( "" 
                       , MIN ( LLinesRef . LrFromPos , LToPos ) - LFromPos 
                       ) 
            END (* IF *) 
          ; IF LToPos > LLinesRef . LrFromPos 
            THEN
              LFromPosUsingText := MAX ( LFromPos , LLinesRef . LrFromPos ) 
            ; LResult
                := LResult 
                   & Text . Sub 
                       ( LLinesRef . LrLineText 
                       , LFromPosUsingText - LLinesRef . LrFromPos 
                       , LToPos - LFromPosUsingText 
                       ) 
            END (* IF *) 
          END (* IF *)  
        ; INC ( LLineNo ) 
        ; LFromPos := 0 
        ; IF LLinesRef = LThruLinesRef 
          THEN
            IF LLineNo > LRightMark . LmLineNo 
            THEN EXIT 
            END (* IF *) 
          ELSIF LLineNo >= LLineCt 
          THEN
            LLinesRef := LLinesRef . LrRightLink (* NARROW can't fail. *)  
          ; LLineNo := 0 
          ; LLineCt := LLinesRef . LrLineCt 
          END (* IF *) 
        ; LResult := LResult & Wr . EOL 
        END (* LOOP *) 
      ; RETURN LResult 
      ELSE RETURN NIL 
      END (* IF *) 
    END ManifestSelectionAsText  

; BEGIN (* Selection *) 
  END Selection 
. 