
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE ParseHs 

(* Data structures for (re)parsing. *) 

; IMPORT EstHs
; IMPORT EstUtil
; IMPORT LangUtil 
; IMPORT LbeStd
; IMPORT SharedStrings 

(* VISIBLE: *) 
; PROCEDURE CopyOfTempMarkList 
    ( OldTempMarkList : TempMarkArrayRefTyp 
    ; CopyNumber : LbeStd . MarkNoTyp := LbeStd . MarkNoMax 
      (* In the copy, marks beyond CopyNumber will be null. *) 
    ) 
  : TempMarkArrayRefTyp 

  = VAR LNewTempMarkListRef : TempMarkArrayRefTyp
  ; VAR LNumber , LCopyNumber: INTEGER 

  ; BEGIN 
      IF OldTempMarkList = NIL 
      THEN RETURN NIL 
      ELSE
        LNumber := NUMBER ( OldTempMarkList ^ )
      ; LNewTempMarkListRef := NEW ( TempMarkArrayRefTyp , LNumber )
      ; LCopyNumber := MIN ( CopyNumber , LNumber ) 
      ; SUBARRAY ( LNewTempMarkListRef ^ , 0 , LCopyNumber ) 
	  := SUBARRAY ( OldTempMarkList ^ , 0 , LCopyNumber ) 
      ; RETURN LNewTempMarkListRef 
      END (* IF *) 
    END CopyOfTempMarkList 

; PROCEDURE RangeIsEmpty ( Range : TempMarkRangeTyp ) : BOOLEAN
  (* It can be empty either by From = MarkNoNull, or by To <= From *)

  = BEGIN
      IF Range . From = LbeStd . MarkNoNull
      THEN RETURN TRUE
      ELSIF Range . To <= Range . From
      THEN RETURN TRUE
      ELSE RETURN FALSE
      END (* IF *) 
    END RangeIsEmpty 

(* VISIBLE: *) 
; PROCEDURE TempMarkRangeImage ( Range : TempMarkRangeTyp ) : TEXT 

  = BEGIN 
      RETURN 
        "[" 
        & LbeStd . MarkNoImage ( Range . From ) 
        & "," 
        & LbeStd . MarkNoImage ( Range . To ) 
        & ")"
    END TempMarkRangeImage 

(* VISIBLE: *) 
; PROCEDURE TokInfoSharedString
    ( READONLY TokInfo : TokInfoTyp ; Lang : LbeStd . LangTyp )
  : SharedStrings . T
  (* NIL if TokInfo is not for a VarTerm. *)

  = VAR LSliceListElemRef : SliceListElemRefTyp
  ; VAR LStartChildNo : LbeStd . EstChildNoTyp 
  ; VAR LChildNo : LbeStd . EstChildNoTyp 
  ; VAR LChildRelNodeNo : LbeStd . EstNodeNoTyp
  ; VAR LTok : LbeStd . TokTyp 
  ; VAR LLeafElem : EstHs . LeafElemTyp 

  ; BEGIN
      LTok := LangUtil . VarTermTok ( Lang , TokInfo . TiTok )
    ; LSliceListElemRef := TokInfo . TiSliceListRMRoot
    ; WHILE LSliceListElemRef # NIL
      DO IF LSliceListElemRef . SleIsSlice
        THEN
          LStartChildNo := LSliceListElemRef . SleFrom
        ; LOOP 
            EstUtil . NextInKindSet
               ( LSliceListElemRef . SleNodeRef
               , LSliceListElemRef . SleFrom 
               , EstHs . EstChildKindSetEstChild
               , (* VAR *) LChildNo 
               , (* VAR *) LChildRelNodeNo 
               , (* VAR *) LLeafElem
               )
          ; IF LChildRelNodeNo >=LSliceListElemRef . SleTo
            THEN EXIT
            ELSE 
              TYPECASE LLeafElem . LeChildRef 
              OF NULL =>
              | SharedStrings . T ( TSharedString )
              => IF SharedStrings . Tok ( TSharedString ) = LTok
(* FIXME: Check for a modtok, here and sigle est case. *) 
                THEN RETURN TSharedString
                END (* IF *)
              ELSE
              END (* TYPECASE *)
            ; LStartChildNo := LChildNo
            END (* IF *) 
          END (* LOOP *)
        ELSE (* Single Est. *) 
          TYPECASE LSliceListElemRef . SleNodeRef
          OF NULL =>
          | SharedStrings . T ( TSharedString )
          => IF SharedStrings . Tok ( TSharedString ) = LTok
             THEN RETURN TSharedString
             END (* IF *) 
          ELSE
          END (* TYPECASE *) 
        END (* IF *)
      ; LSliceListElemRef := LSliceListElemRef . SlePredLink
      END (* LOOP *)
    ; RETURN NIL 
    END TokInfoSharedString

(* VISIBLE: *) 
; PROCEDURE TokInfoImage
    ( READONLY TokInfo : TokInfoTyp ; Lang : LbeStd . LangTyp )
  : TEXT

  = VAR LSharedString : SharedStrings . T

  ; BEGIN
      LSharedString := TokInfoSharedString ( TokInfo , Lang )
    ; IF LSharedString = NIL
      THEN RETURN LangUtil . TokImage ( TokInfo . TiTok , Lang )
      ELSE RETURN EstUtil . VarTermImage ( LSharedString , Lang ) 
      END (* END *) 
    END TokInfoImage 

(* VISIBLE: *) 
; PROCEDURE ParseTravStateKindImage ( Kind : ParseTravStateKindTyp ) : TEXT 

  = TYPE T = ParseTravStateKindTyp 

  ; BEGIN 
      CASE Kind 
      OF T . PtsKindBlanksThenLeadingMods => RETURN "PtsKindBlanksThenLeadingMods" 
      | T . PtsKindBlanksThenLexErrChars => RETURN "PtsKindBlanksThenLexErrChars" 
      | T . PtsKindBlanksThenAstString => RETURN "PtsKindBlanksThenAstString" 
      | T . PtsKindBlanksThenModCmnt => RETURN "PtsKindBlanksThenModCmnt" 
      | T . PtsKindBlanksThenInsTok => RETURN "PtsKindBlanksThenInsTok" 
      | T . PtsKindBlanksThenRescanModText => RETURN "PtsKindBlanksThenRescanModText" 
      | T . PtsKindDoneWithEstTraversed => RETURN "PtsKindDoneWithEstTraversed" 
      | T . PtsKindDoneWithEstUntraversed => RETURN "PtsKindDoneWithEstUntraversed" 
      | T . PtsKindDoneWithListSliceTraversed => RETURN "PtsKindDoneWithListSliceTraversed" 
      | T . PtsKindDoneWithListSliceUntraversed => RETURN "PtsKindDoneWithListSliceUntraversed" 
      | T . PtsKindDoneWithFsNode => RETURN "PtsKindDoneWithFsNode" 
      | T . PtsKindEndOfImage => RETURN "PtsKindEndOfImage" 
      | T . PtsKindLeadingMods => RETURN "PtsKindLeadingMods" 
      | T . PtsKindTrailingMods => RETURN "PtsKindTrailingMods" 
      | T . PtsKindNewEst => RETURN "PtsKindNewEst" 
      | T . PtsKindRevisitNewEst => RETURN "PtsKindRevisitNewEst" 
      | T . PtsKindNewFsNode => RETURN "PtsKindNewFsNode" 
      | T . PtsKindRescanInsTok => RETURN "PtsKindRescanInsTok" 
      | T . PtsKindRescanAstString => RETURN "PtsKindRescanAstString" 
      | T . PtsKindRescanLexErrChars => RETURN "PtsKindRescanLexErrChars" 
      | T . PtsKindInsideInsTok => RETURN "PtsKindInsideInsTok" 
      | T . PtsKindInsideLexErrChars => RETURN "PtsKindInsideLexErrChars" 
      | T . PtsKindInsideAstString => RETURN "PtsKindInsideAstString" 
      | T . PtsKindInsideModCmnt => RETURN "PtsKindInsideModCmnt" 
      | T . PtsKindInsideModText => RETURN "PtsKindInsideModText" 
      END (* CASE *) 
    END ParseTravStateKindImage 

; BEGIN (* ParseHs *) 
  END ParseHs 
. 
