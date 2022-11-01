
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Search 

; IMPORT Text 
; IMPORT Thread 

; FROM Failures IMPORT Backout 
; IMPORT Display 
; IMPORT EditWindow 
; IMPORT LbeStd 
; IMPORT Marks 
; IMPORT Misc 
; IMPORT PaintHs
; IMPORT Strings 
; IMPORT TextEdit 

; CONST MarkKindBlankLine = Marks . MarkKindTyp . BlankLine  

(* VISIBLE: *) 
; PROCEDURE ClearMatch 
    ( Window : PaintHs . WindowRefTyp 
    ; VAR (* IN OUT *) MustRepaint : BOOLEAN 
      (* Set to TRUE if anything happens that requires a repaint. *) 
    ) 

  = VAR LImageTrans : PaintHs . ImageTransientTyp 
  ; VAR LPredMark : PaintHs . LineMarkTyp 

  ; BEGIN
      IF Window # NIL 
      THEN 
        LImageTrans := Window . WrImageRef 
      ; IF LImageTrans # NIL 
        THEN 
          IF Window . WrMatchStartMark # NIL 
          THEN 
            PaintHs . DeleteLineMark 
              ( Window . WrMatchStartMark 
              , (* VAR *) LPredMark (* Dead. *) 
              , LImageTrans 
              ) 
          ; Window . WrMatchStartMark := NIL 
          ; MustRepaint := TRUE 
          END (* IF *) 
        ; IF Window . WrMatchEndMark # NIL 
          THEN 
            PaintHs . DeleteLineMark 
              ( Window . WrMatchEndMark 
              , (* VAR *) LPredMark (* Dead. *) 
              , LImageTrans 
              ) 
          ; Window . WrMatchEndMark := NIL 
          ; MustRepaint := TRUE 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END ClearMatch 

(* VISIBLE: *) 
; PROCEDURE SetMatchAndRepaint 
    ( Window : PaintHs . WindowRefTyp 
    ; LinesRef : PaintHs . LinesRefMeatTyp 
    ; LineNo : LbeStd . LineNoTyp 
    ; FromPos : LbeStd . CharNoTyp 
    ; ToPos : LbeStd . CharNoTyp 
    ; NewCursorLineNoInWindow : LbeStd . LineNoSignedTyp 
      (* ^Not just where it is wanted.  Must be where it would be if there
          were no scrolling, which could be outside the window.
      *) 
    ; Forward : BOOLEAN 
    ; PredHintMark : PaintHs . LineMarkMeatTyp := NIL 
    ; SuccHintMark : PaintHs . LineMarkMeatTyp := NIL 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LImageTrans : PaintHs . ImageTransientTyp  
  ; VAR LOldCursorMark : PaintHs . LineMarkMeatTyp 
  ; VAR LNewCursorMark : PaintHs . LineMarkMeatTyp 
  ; VAR LMatchStartMark : PaintHs . LineMarkMeatTyp 
  ; VAR LMatchEndMark : PaintHs . LineMarkMeatTyp 
  ; VAR LPredMark : PaintHs . LineMarkTyp 
  ; VAR LBlankLinesAtEnd : LbeStd . LineNoTyp 
  ; VAR LMustRepaint : BOOLEAN 

  ; BEGIN 
      WITH 
        WCursorMark 
        = Window . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
      DO 
        LImageTrans := Window . WrImageRef 
      ; LOldCursorMark := WCursorMark 

      ; LMatchStartMark := NEW ( PaintHs . LineMarkMeatTyp )  
      ; LMatchStartMark . LmWindowRef := Window   
      ; LMatchStartMark . LmMarkSs := PaintHs . MarkSsTyp . MarkSsStartMatch   
      ; LMatchStartMark . LmLinesRef := LinesRef  
      ; LMatchStartMark . LmTokMark := LinesRef . LrBolTokMark  
      ; LMatchStartMark . LmLineNo := LineNo  
      ; LMatchStartMark . LmCharPos := FromPos  

      ; LMatchEndMark := NEW ( PaintHs . LineMarkMeatTyp )  
      ; LMatchEndMark . LmWindowRef := Window   
      ; LMatchEndMark . LmMarkSs := PaintHs . MarkSsTyp . MarkSsEndMatch   
      ; LMatchEndMark . LmLinesRef := LinesRef  
      ; LMatchEndMark . LmTokMark := LinesRef . LrBolTokMark  
      ; LMatchEndMark . LmLineNo := LineNo  
      ; LMatchEndMark . LmCharPos := ToPos 

      ; LNewCursorMark := NEW ( PaintHs . LineMarkMeatTyp )  
      ; LNewCursorMark . LmWindowRef := Window   
      ; LNewCursorMark . LmMarkSs := WCursorMark . LmMarkSs  
      ; LNewCursorMark . LmLinesRef := LinesRef  
      ; LNewCursorMark . LmTokMark := LinesRef . LrBolTokMark  
      ; LNewCursorMark . LmLineNo := LineNo  
      ; IF Forward 
        THEN LNewCursorMark . LmCharPos := ToPos 
        ELSE LNewCursorMark . LmCharPos := FromPos 
        END (* IF*) 

      (* Make changes, hopefully without failures: *) 
      ; LMustRepaint := FALSE 
      ; ClearMatch ( Window , (* VAR *) LMustRepaint ) 
      ; EVAL PaintHs . InsertLineMark 
          ( LMatchStartMark 
          , LImageTrans 
          , LeftIfEqual := NOT Forward   
          , PredHintMark := PredHintMark 
            (* Which could = WCursorMark, which must not be
               deleted until later. 
            *)  
          , SuccHintMark := SuccHintMark 
          ) 
      ; Window . WrMatchStartMark := LMatchStartMark 
      ; EVAL PaintHs . InsertLineMark 
          ( LMatchEndMark 
          , LImageTrans 
          , LeftIfEqual := Forward 
          , PredHintMark := LMatchStartMark 
          , SuccHintMark := SuccHintMark 
          ) 
      ; Window . WrMatchEndMark := LMatchEndMark 
      ; PaintHs . DeleteLineMark 
          ( WCursorMark 
          , (* VAR *) LPredMark (* Dead. *) 
          , LImageTrans 
          ) 
      ; EVAL PaintHs . InsertLineMark 
          ( LNewCursorMark 
          , LImageTrans 
          , LeftIfEqual := Forward 
          , PredHintMark := LMatchStartMark 
          , SuccHintMark := LMatchEndMark 
          ) 
      ; WCursorMark := LNewCursorMark 
      ; LinesRef . LrHasMark := TRUE 
      ; IF 0 <= NewCursorLineNoInWindow 
           AND NewCursorLineNoInWindow 
               < EditWindow . SouthEastToCorner ( Window ) . v 
        THEN (* New location is in unscrolled window. 
                Do low-glitch painting.
             *)  
          IF Window . WrCursorLineNoInWindow 
             # NewCursorLineNoInWindow
          THEN 
            Window . WrCursorLineNoInWindow 
              := NewCursorLineNoInWindow
          ; Display . PaintLinesRangeOneWindow 
              ( Window 
              , LOldCursorMark . LmLinesRef 
              , LOldCursorMark . LmLineNo 
              , LOldCursorMark . LmLinesRef 
              , LOldCursorMark . LmLineNo 
              ) 
          END (* IF *) 
        ; Display . PaintLinesRangeOneWindow 
            ( Window 
            , LNewCursorMark . LmLinesRef 
            , LNewCursorMark . LmLineNo 
            , LNewCursorMark . LmLinesRef 
            , LNewCursorMark . LmLineNo 
            ) 
        ELSE 
(* TODO: Decide where we want the cursor in the window when found string 
         requires scrolling.  Implement by setting WrCursorLineNoInWindow
         to the desired place. 
*) 
          Display . PaintWindowFromCursor 
            ( Window , (* VAR *) LBlankLinesAtEnd (* UNUSED *) ) 
        END (* IF *) 
      END (* WITH WCursorMark *) 
    END SetMatchAndRepaint 

(* Forward string searching: *) 

; PROCEDURE SearchBlankLineFwd 
    ( SearchString : TEXT ; SearchStartPos : LbeStd . CharNoTyp  ) 
  : LbeStd . CharNoTyp (* LbeStd . CharNoUnknown, if string not found. *) 

  = VAR LLength : CARDINAL 
  ; VAR LPos : CARDINAL 

  ; BEGIN
      IF SearchString = NIL 
      THEN RETURN SearchStartPos 
      ELSE 
        LLength := Text . Length ( SearchString ) 
      ; IF LLength = 0 
        THEN RETURN SearchStartPos 
        ELSIF SearchStartPos + LLength - 1 > LbeStd . LimitedCharNoMax 
        THEN RETURN LbeStd . CharNoUnknown 
        ELSE 
          LPos := 0  
        ; LOOP 
            IF LPos >= LLength 
            THEN (* SearchString was all blanks. *) 
              RETURN SearchStartPos 
            ELSIF Text . GetChar ( SearchString , LPos ) # ' ' 
            THEN RETURN LbeStd . CharNoUnknown 
            ELSE INC ( LPos ) 
            END (* IF *) 
          END (* LOOP *) 
        END (* IF *)  
      END (* IF *)  
    END SearchBlankLineFwd 

; PROCEDURE SearchNonblankLineFwd 
    ( PatternString : TEXT 
    ; TargetString : TEXT 
    ; TargetFromPos : LbeStd . CharNoTyp 
      (* ^This many blanks preceed TargetString. *)
    ; SearchStartPos : LbeStd . CharNoTyp  
    ; CaseSensitive : BOOLEAN 
    ) 
  : LbeStd . CharNoTyp (* LbeStd . CharNoUnknown, if string not found. *) 

(* NOTE: This treats the target string as having infinitely many trailing
         blanks. 
*) 

  = VAR SnlTargetLen : CARDINAL 

  ; PROCEDURE SnlCharMatches ( PatternCh : CHAR ; TargetPos : CARDINAL ) 
    : BOOLEAN 

    = BEGIN 
        IF TargetPos < TargetFromPos 
        THEN RETURN PatternCh = ' ' 
        ELSIF TargetPos >= SnlTargetLen 
        THEN RETURN PatternCh = ' ' 
        ELSE 
          RETURN 
            Misc . CharIsEqual 
              ( PatternCh 
              , Text . GetChar ( TargetString , TargetPos - TargetFromPos ) 
              , CaseSensitive 
              ) 
        END (* IF *) 
      END SnlCharMatches 

  ; VAR LPatternLen : CARDINAL 
  ; VAR LPatternNonblankLen : INTEGER  
  ; VAR LMatchPos : CARDINAL 
  ; VAR LPatternPos : CARDINAL 
  ; VAR LTargetPos : CARDINAL 

  ; BEGIN 
      IF PatternString = NIL 
      THEN RETURN SearchStartPos  
      ELSE 
        LPatternLen := Text . Length ( PatternString ) 
      ; IF LPatternLen <= 0 
        THEN RETURN SearchStartPos 
        ELSIF TargetString = NIL 
        THEN RETURN LbeStd . CharNoUnknown 
        ELSE 
          SnlTargetLen := TargetFromPos + Text . Length ( TargetString )  
        ; IF SnlTargetLen = 0 
          THEN RETURN LbeStd . CharNoUnknown 
          ELSE 
            LPatternNonblankLen := LPatternLen - 1 
          ; WHILE LPatternNonblankLen >= 0 
                  AND Text . GetChar ( PatternString , LPatternNonblankLen )
                      = ' '  
            DO DEC ( LPatternNonblankLen ) 
            END (* WHILE *) 
          ; INC ( LPatternNonblankLen ) 
          ; LMatchPos := MAX ( SearchStartPos , 0 )   
          ; LOOP (* Thru match positions. *) 
              IF LMatchPos >= SnlTargetLen 
              THEN 
                IF LPatternNonblankLen = 0 
                THEN 
                  RETURN LMatchPos 
                ELSE 
                  RETURN LbeStd . CharNoUnknown 
                END (* IF *) 
              ELSE 
                LPatternPos := 0 
              ; LOOP (* Thru characters of pattern. *) 
                  IF LPatternPos >= LPatternLen 
                  THEN (* Match succeeded at LMatchPos. *) 
                    RETURN LMatchPos 
                  ELSE 
                    LTargetPos := LMatchPos + LPatternPos 
                  ; IF LPatternPos >= LPatternNonblankLen 
                       AND LTargetPos >= SnlTargetLen 
                    THEN (* Any number of trailing blanks of pattern match 
                            trailing blanks of target. *) 
                      RETURN LMatchPos 
                    ELSE 
                      IF SnlCharMatches 
                           ( Text . GetChar ( PatternString , LPatternPos ) 
                           , LTargetPos 
                           ) 
                      THEN
                        INC ( LPatternPos ) 
                      ELSE (* Match at this LMatchPos failed. *) 
                        EXIT (* Inner loop on characters of pattern. *) 
                      END (* IF *) 
                    END (* IF *) 
                  END (* IF *) 
                END (* LOOP *) 
              (* Match at this LMatchPos failed. *) 
              ; INC ( LMatchPos ) 
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END SearchNonblankLineFwd 

(* VISIBLE: *) 
; PROCEDURE StringSearchFwd  
    ( Window : PaintHs . WindowRefTyp 
    ; SearchString : TEXT 
    ; CaseSensitive : BOOLEAN 
    ; StartAtBOI : BOOLEAN := FALSE 
    ) 
  : BOOLEAN (* String was found (and cursor moved to it.) *) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LImageTrans : PaintHs . ImageTransientTyp  
  ; VAR LImagePers : PaintHs . ImagePersistentTyp  
  ; VAR LLinesHeader : PaintHs . LinesRefTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoTyp 
  ; VAR LNewCursorLineNoInWindow : LbeStd . LineNoSignedTyp 
  ; VAR LCharPos : LbeStd . CharNoTyp 
  ; VAR LMatchPosition : LbeStd . CharNoTyp 
  ; VAR LHintMark : PaintHs . LineMarkMeatTyp 

  ; BEGIN 
      IF Window # NIL 
      THEN 
        LImageTrans := Window . WrImageRef 
      ; IF LImageTrans # NIL 
        THEN
          LImagePers := LImageTrans . ItPers 
        ; LLinesHeader := LImagePers . IpLineHeaderRef 
        ; WITH 
            WCursorMark 
            = Window . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
          DO 
            IF WCursorMark # NIL 
            THEN 
              IF StartAtBOI 
              THEN 
                Display . SecureSucc ( LImageTrans , LLinesHeader ) 
              ; LLinesRef := LLinesHeader . LrRightLink  
              ; LCharPos := 0 
              ; LLineNo := 0 
              ; LHintMark := NIL 
              ; IF Window . WrVertScrollIsExact 
                THEN LNewCursorLineNoInWindow := - Window . WrVertScroll 
                ELSE LNewCursorLineNoInWindow := LbeStd . LineNoMax 
                END (* IF *) 
              ELSE 
                LLinesRef := WCursorMark . LmLinesRef 
              ; LCharPos := WCursorMark . LmCharPos 
              ; LLineNo := WCursorMark . LmLineNo 
              ; LHintMark := WCursorMark 
              ; LNewCursorLineNoInWindow := Window . WrCursorLineNoInWindow 
              END (* IF *) 
            ; Display . SecureSucc ( LImageTrans , LLinesRef ) 
            ; LOOP (* Through lines, to find string. *) 
                IF LLinesRef . LrRightLink = LLinesHeader 
                THEN (* This is the EOI LinesRef. *) 
                  RETURN FALSE 
                ELSE 
                  IF LImagePers . IpTempEditState 
                     = PaintHs . TempEditStateTyp . TeStateText 
                     AND LImagePers . IpTempEditRef # NIL 
                     AND LImagePers . IpTempEditRef . TeLinesRef = LLinesRef
                     AND LImagePers . IpTempEditRef . TeLineNo = LLineNo 
                  THEN 
                    LMatchPosition  
                      := SearchNonblankLineFwd 
                           ( SearchString 
                           , Strings . ToText 
                               ( LImagePers . IpTempEditRef . TeEditedString ) 
                           , TargetFromPos := 0 
                           , SearchStartPos := LCharPos 
                           , CaseSensitive := CaseSensitive  
                           ) 
                  ELSIF ( LLinesRef . LrBolTokMark . TkmKind = MarkKindBlankLine 
                          AND NOT LLinesRef . LrBolTokMark . TkmStartAtEnd 
                        ) 
                        OR LLinesRef . LrLineText = NIL 
                        OR Text . Equal ( LLinesRef . LrLineText , "" ) 
                  THEN 
                    LMatchPosition 
                      := SearchBlankLineFwd ( SearchString , LCharPos ) 
                  ELSE 
                    LMatchPosition  
                      := SearchNonblankLineFwd 
                           ( SearchString 
                           , LLinesRef . LrLineText 
                           , TargetFromPos := LLinesRef . LrFromPos  
                           , SearchStartPos := LCharPos 
                           , CaseSensitive := CaseSensitive  
                           ) 
                  END (* IF *) 
                ; IF LMatchPosition = LbeStd . CharNoUnknown 
                  THEN (* Match failed on this line. *) 
                    IF LNewCursorLineNoInWindow < LbeStd . LineNoMax 
                    THEN INC ( LNewCursorLineNoInWindow ) 
                    END (* IF *) 
                  ; IF LLinesRef . LrBolTokMark . TkmKind = MarkKindBlankLine 
                       AND LLineNo < LLinesRef . LrLineCt - 1 
                    THEN 
                      INC ( LLineNo ) (* And loop. *) 
                    ELSE 
(* TODO: Are there cases where we can advance LHintMark here? *) 
                      LLinesRef := LLinesRef . LrRightLink 
                      (* ^Implied NARROW can't fail. *) 
                    ; Display . SecureSucc ( LImageTrans , LLinesRef ) 
                    ; LLineNo := 0 
                    ; LCharPos := 0 
                    (* And loop. *) 
                    END (* IF *) 
                  ELSE (* Match succeeded. *) 
                    SetMatchAndRepaint 
                      ( Window 
                      , LinesRef := LLinesRef 
                      , LineNo := LLineNo 
                      , FromPos := LMatchPosition 
                      , ToPos 
                          := LMatchPosition + Text . Length ( SearchString )   
                      , NewCursorLineNoInWindow := LNewCursorLineNoInWindow 
                      , Forward := TRUE 
                      , PredHintMark := LHintMark 
                      ) 
                  ; RETURN TRUE 
                  END (* IF *) 
                END (* IF *) 
              END (* LOOP *) 
            END (* IF WCursorMark # NIL *) 
          END (* WITH WCursorMark *) 
        END (* IF LImageTrans # NIL *) 
      END (* IF Window # NIL *) 
    ; RETURN FALSE 
    END StringSearchFwd 

; PROCEDURE SearchBlankLineBwd 
    ( SearchString : TEXT 
    ; SearchStartPos : LbeStd . CharNoTyp  
      (* ^Position of rightmost char of expected match. *) 
    ) 
  : LbeStd . CharNoTyp (* LbeStd . CharNoUnknown, if string not found. *) 

  = VAR LLength : CARDINAL 
  ; VAR LPos : INTEGER 

  ; BEGIN
      IF SearchString = NIL 
      THEN RETURN SearchStartPos 
      ELSE 
        LLength := Text . Length ( SearchString ) 
      ; IF LLength = 0 
        THEN RETURN SearchStartPos 
        ELSIF SearchStartPos < LLength - 1 
        THEN RETURN LbeStd . CharNoUnknown 
        ELSE 
          LPos := LLength - 1   
        ; LOOP 
            IF LPos < 0 
            THEN (* SearchString was all blanks. *) 
              RETURN SearchStartPos 
            ELSIF Text . GetChar ( SearchString , LPos ) # ' ' 
            THEN RETURN LbeStd . CharNoUnknown 
            ELSE DEC ( LPos ) 
            END (* IF *) 
          END (* LOOP *) 
        END (* IF *)  
      END (* IF *)  
    END SearchBlankLineBwd 

; PROCEDURE SearchNonblankLineBwd 
    ( PatternString : TEXT 
    ; TargetString : TEXT 
    ; TargetFromPos : LbeStd . CharNoTyp 
      (* ^This many blanks preceed TargetString. *)
    ; SearchStartPos : LbeStd . CharNoTyp  
      (* ^Position of rightmost char of expected match. *) 
    ; CaseSensitive : BOOLEAN 
    ) 
  : LbeStd . CharNoTyp (* LbeStd . CharNoUnknown, if string not found. *) 

(* NOTE: This treats the target string as having infinitely many trailing
         blanks. 
*) 

  = VAR SnlTargetLen : CARDINAL 

  ; PROCEDURE SnlCharMatches ( PatternCh : CHAR ; TargetPos : CARDINAL ) 
    : BOOLEAN 

    = BEGIN 
        IF TargetPos < TargetFromPos 
        THEN RETURN PatternCh = ' ' 
        ELSIF TargetPos >= SnlTargetLen 
        THEN RETURN PatternCh = ' ' 
        ELSE 
          RETURN 
            Misc . CharIsEqual 
              ( PatternCh 
              , Text . GetChar ( TargetString , TargetPos - TargetFromPos ) 
              , CaseSensitive 
              ) 
        END (* IF *) 
      END SnlCharMatches 

  ; VAR LPatternThruPos : LbeStd . CharNoTyp
  ; VAR LPatternLeadingBlankCt : LbeStd . CharNoTyp
  ; VAR LMatchPos : LbeStd . CharNoTyp  
  ; VAR LPatternPos : LbeStd . CharNoTyp 
  ; VAR LTargetPos : LbeStd . CharNoTyp 

  ; BEGIN 
      IF PatternString = NIL 
      THEN RETURN SearchStartPos  
      ELSE 
        LPatternThruPos := Text . Length ( PatternString ) - 1 
      ; IF LPatternThruPos < 0 
        THEN RETURN SearchStartPos 
        ELSIF TargetString = NIL 
        THEN RETURN LbeStd . CharNoUnknown 
        ELSE 
          SnlTargetLen := TargetFromPos + Text . Length ( TargetString )  
        ; IF SnlTargetLen = 0 
          THEN RETURN LbeStd . CharNoUnknown 
          ELSE 
            LPatternLeadingBlankCt := 0 
          ; WHILE LPatternLeadingBlankCt <= LPatternThruPos  
                  AND Text . GetChar ( PatternString , LPatternLeadingBlankCt )
                      = ' '  
            DO INC ( LPatternLeadingBlankCt ) 
            END (* WHILE *) 
          ; LMatchPos 
              := MIN ( SearchStartPos , SnlTargetLen + LPatternThruPos )   
          ; LOOP (* Thru match positions. *) 
              IF LMatchPos < LPatternThruPos 
              THEN RETURN LbeStd . CharNoUnknown 
              ELSIF LMatchPos < TargetFromPos 
              THEN 
                IF LPatternLeadingBlankCt = LPatternThruPos + 1 
                THEN RETURN LMatchPos 
                ELSE RETURN LbeStd . CharNoUnknown 
                END (* IF *) 
              ELSE 
                LPatternPos := LPatternThruPos  
              ; LOOP (* Thru characters of pattern. *) 
                  IF LPatternPos < 0 
                  THEN (* Match succeeded at LMatchPos. *) 
                    RETURN LMatchPos 
                  ELSE 
                    LTargetPos := LMatchPos - ( LPatternThruPos - LPatternPos )
                  ; IF LPatternPos < LPatternLeadingBlankCt 
                       AND LTargetPos < TargetFromPos 
                    THEN (* Leading blanks match. *) 
                      RETURN LMatchPos 
                    ELSE 
                      IF SnlCharMatches 
                           ( Text . GetChar ( PatternString , LPatternPos ) 
                           , LTargetPos 
                           ) 
                      THEN
                        DEC ( LPatternPos ) 
                      ELSE (* Match at this LMatchPos failed. *) 
                        EXIT (* Inner loop on characters of pattern. *) 
                      END (* IF *) 
                    END (* IF *) 
                  END (* IF *) 
                END (* LOOP *) 
              (* Match at this LMatchPos failed. *) 
              ; DEC ( LMatchPos ) 
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END SearchNonblankLineBwd 

(* VISIBLE: *) 
; PROCEDURE StringSearchBwd  
    ( Window : PaintHs . WindowRefTyp 
    ; SearchString : TEXT 
    ; CaseSensitive : BOOLEAN 
    ; StartAtEOI : BOOLEAN := FALSE 
    ) 
  : BOOLEAN (* String was found (and cursor moved to it.) *) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LImageTrans : PaintHs . ImageTransientTyp  
  ; VAR LImagePers : PaintHs . ImagePersistentTyp  
  ; VAR LLinesHeader : PaintHs . LinesRefTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoTyp 
  ; VAR LNewCursorLineNoInWindow : LbeStd . LineNoSignedTyp 
  ; VAR LCharPos : LbeStd . CharNoTyp 
  ; VAR LMatchPosition : LbeStd . CharNoTyp 
  ; VAR LHintMark : PaintHs . LineMarkMeatTyp 

  ; BEGIN 
      IF Window # NIL 
      THEN 
        LImageTrans := Window . WrImageRef 
      ; IF LImageTrans # NIL 
        THEN
          LImagePers := LImageTrans . ItPers 
        ; LLinesHeader := LImagePers . IpLineHeaderRef 
        ; WITH 
            WCursorMark 
            = Window . WrMarks [ PaintHs . MarkSsTyp . MarkSsCursor ] 
          DO 
            IF WCursorMark # NIL 
            THEN 
              IF StartAtEOI 
              THEN 
                Display . SecurePred ( LImageTrans , LLinesHeader ) 
              ; LLinesRef := LLinesHeader . LrLeftLink 
                (* ^This will be the empty linesRef at EOI. *) 
              ; Display . SecurePred ( LImageTrans , LLinesRef ) 
              ; TYPECASE LLinesRef . LrLeftLink  
                OF PaintHs . LinesRefMeatTyp ( TPredLinesRefMeat ) 
                => LLinesRef := TPredLinesRefMeat 
                ELSE RETURN FALSE 
                END (* TYPECASE *) 
              ; LLineNo 
                  := Display . ActualLineCtOfLinesRef 
                       ( LImageTrans , LLinesRef ) 
                     - 1
              ; LCharPos := LLinesRef . LrLineLen - 1 
              ; LHintMark := NIL 
              ; IF LImagePers . IpLineCtIsExact 
                   AND Window . WrVertScrollIsExact 
                THEN 
                  LNewCursorLineNoInWindow 
                    := LImagePers . IpLineCtDisplay - Window . WrVertScroll - 1 
                ELSE LNewCursorLineNoInWindow := - LbeStd . LineNoMax 
                END (* IF *) 
              ELSE 
                LLinesRef := WCursorMark . LmLinesRef 
              ; LCharPos := WCursorMark . LmCharPos - 1  
              ; LLineNo := WCursorMark . LmLineNo 
              ; LHintMark := WCursorMark 
              ; LNewCursorLineNoInWindow := Window . WrCursorLineNoInWindow 
              END (* IF *) 
            ; LOOP (* Through lines, to find string. *) 
                IF LLinesRef . LrBolTokMark . TkmKind = MarkKindBlankLine 
                   OR LLinesRef . LrLineText = NIL 
                   OR Text . Equal ( LLinesRef . LrLineText , "" ) 
                THEN 
                  LMatchPosition 
                    := SearchBlankLineBwd ( SearchString , LCharPos ) 
                ELSE 
                  LMatchPosition  
                    := SearchNonblankLineBwd 
                         ( SearchString 
                         , LLinesRef . LrLineText 
                         , TargetFromPos := LLinesRef . LrFromPos  
                         , SearchStartPos := LCharPos 
                         , CaseSensitive := CaseSensitive 
                         ) 
                END (* IF *) 
              ; IF LMatchPosition = LbeStd . CharNoUnknown 
                THEN (* Match failed on this line. *) 
                  IF LNewCursorLineNoInWindow > - LbeStd . LineNoMax 
                  THEN DEC ( LNewCursorLineNoInWindow ) 
                  END (* IF *) 
                ; IF LLinesRef . LrBolTokMark . TkmKind = MarkKindBlankLine 
                     AND LLineNo > 0 
                  THEN 
                    DEC ( LLineNo ) (* And loop. *) 
                  ELSE 
(* TODO: Are there cases where we can set LHintMark back here? *) 
                    Display . SecurePred ( LImageTrans , LLinesRef ) 
                  ; TYPECASE LLinesRef . LrLeftLink 
                    OF PaintHs . LinesRefMeatTyp ( TPredLinesRefMeat ) 
                    => LLinesRef := TPredLinesRefMeat 
                    ; LLineNo 
                        := Display . ActualLineCtOfLinesRef 
                             ( LImageTrans , LLinesRef ) 
                           - 1
                    ; LCharPos := LLinesRef . LrLineLen - 1 
                    (* And loop. *) 
                    ELSE RETURN FALSE 
                    END (* TYPECASE *) 
                  END (* IF *) 
                ELSE (* Match succeeded. *) 
                  SetMatchAndRepaint 
                    ( Window 
                    , LinesRef := LLinesRef 
                    , LineNo := LLineNo 
                    , FromPos 
                        := LMatchPosition - Text . Length ( SearchString ) + 1 
                    , ToPos := LMatchPosition + 1 
                    , NewCursorLineNoInWindow := LNewCursorLineNoInWindow 
                    , Forward := FALSE  
                    , SuccHintMark := LHintMark 
                    ) 
                ; RETURN TRUE 
                END (* IF *) 
              END (* LOOP *) 
            END (* IF WCursorMark # NIL *) 
          END (* WITH WCursorMark *) 
        END (* IF LImageTrans # NIL *) 
      END (* IF Window # NIL *) 
    ; RETURN FALSE 
    END StringSearchBwd 

(* VISIBLE: *) 
; PROCEDURE Replace 
    ( Window : PaintHs . WindowRefTyp 
    ; SearchString : TEXT 
    ; ReplaceString : TEXT 
    ; CaseSensitive : BOOLEAN 
    ; ReplaceKind : ReplaceKindTyp 
    ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LImageTrans : PaintHs . ImageTransientTyp 
  ; VAR LMustRepaint : BOOLEAN 

  ; BEGIN 
      IF Window # NIL 
      THEN 
        LImageTrans := Window . WrImageRef 
      ; IF LImageTrans # NIL 
        THEN 
          IF Window . WrMatchStartMark = NIL 
             OR Window . WrMatchEndMark = NIL 
          THEN 
            EditWindow . Beep ( ) 
(* WHYNOT: ^No matched text exists. *) 
          ELSIF ReplaceKind = ReplaceKindTyp . All 
          THEN (* Redundant.  UiSearch checks for this and pops a message. *)
            EditWindow . Beep ( ) 
(* WHYNOT: ^Replace all unimplemented  *) 
          ELSE 
            LOOP 
              TextEdit . DeleteBetweenMarks  
                ( LImageTrans 
                , Window . WrMatchStartMark 
                , Window . WrMatchEndMark 
                ) 
            ; ClearMatch ( Window , (* VAR *) LMustRepaint (* Dead. *) ) 
            ; TextEdit . InsertOrOverlayString 
                ( Window 
                , ReplaceString 
                , IsInsert := TRUE 
                ) 
            ; CASE ReplaceKind 
              OF ReplaceKindTyp . Once 
              => (* We are done. *) 
                 EXIT  
              | ReplaceKindTyp . Next 
              => EVAL StringSearchFwd  
                        ( Window 
                        , SearchString 
                        , CaseSensitive 
                        , StartAtBOI := FALSE 
                        ) 
              ; EXIT 
              | ReplaceKindTyp . Rest 
              => IF StringSearchFwd  
                      ( Window 
                      , SearchString 
                      , CaseSensitive 
                      , StartAtBOI := FALSE 
                      ) 
                 THEN (* Loop *) 
                 ELSE EXIT 
                 END (* IF *) 
              | ReplaceKindTyp . All 
              => 
              END (* CASE *) 
            END (* LOOP *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END Replace 

; BEGIN 
  END Search 
. 

