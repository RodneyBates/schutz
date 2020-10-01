
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE ParseHs 

(* Data structures for (re)parsing. *) 

; IMPORT LbeStd 

(* VISIBLE: *) 
; PROCEDURE CopyOfTempMarkList 
    ( OldTempMarkList : TempMarkArrayRefTyp 
    ; ToSs : LbeStd . MarkNoTyp 
      (* In the copy, marks at and above this subscript will be null. *) 
    ) 
  : TempMarkArrayRefTyp 

  = VAR LNewTempMarkList : TempMarkArrayRefTyp 

  ; BEGIN 
      IF OldTempMarkList = NIL 
      THEN RETURN NIL 
      ELSE 
	LNewTempMarkList 
	  := NEW ( TempMarkArrayRefTyp , NUMBER ( OldTempMarkList ^ ) )
      (* Values specified in types will initialize as needed. *) 
      ; SUBARRAY ( LNewTempMarkList ^ , 0 , ToSs ) 
	  := SUBARRAY ( OldTempMarkList ^ , 0 , ToSs ) 
      ; RETURN LNewTempMarkList 
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
