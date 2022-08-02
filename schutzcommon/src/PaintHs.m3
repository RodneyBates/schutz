
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE PaintHs 

(* Data structures for painting the screen. *) 

; IMPORT Fmt 
; IMPORT Integer 

; IMPORT WindowPrivate 
; IMPORT LbeStd 
; IMPORT MessageCodes 
; IMPORT Marks 
; IMPORT Options 
; IMPORT LineMarks 
; IMPORT PaintHs 
; IMPORT Version 

; IMPORT Assertions 

; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout  

; TYPE AFT = MessageCodes . T 

; REVEAL WindowRefTyp 
    = WindowPublic BRANDED "WindowRefTyp" OBJECT 
      OVERRIDES
        init := InitWindowRef 
      END (* OBJECT *) 

; PROCEDURE InitWindowRef ( Self : WindowRefTyp ) : WindowRefTyp 
  <* LL.sup = Self *> 

  = BEGIN      
      Self . WrImageLink := NIL 
    ; Self . WrFirstLineLinesRef := NIL 
    ; Self . WrFirstLineLineNo := 0 
    ; Self . WrImageRef := NIL  
    ; Self . WrStackLink := NIL 
    ; Self . WrCursorLineNoInWindow := 0 
    ; Self . WrHorizScroll := 0 
    ; Self . WrVertScroll := 0 
    ; Self . WrVertScrollIsExact := TRUE  
    ; Self . WrMarks [ MarkSsTyp . MarkSsStartSel ] := NIL 
    ; Self . WrMarks [ MarkSsTyp . MarkSsEndSel ] := NIL 
    ; Self . WrMarks [ MarkSsTyp . MarkSsCursor ] := NIL  
    ; Self . WrInsertMode := TRUE 
    ; Self . WrMatchStartMark := NIL 
    ; Self . WrMatchEndMark := NIL 
    ; Self . WrMatchedString := NIL 
    ; Self . WrSearchForm := NIL 
    ; RETURN Self 
    END InitWindowRef 

(* VISIBLE: *) 
; PROCEDURE TextAttrIsEqual ( VALUE Left , Right : TextAttrTyp ) : BOOLEAN 
  (* Equal, except for the TaCharPos field. *) 

  = BEGIN 
      Left . TaCharPos := 0 
    ; Right . TaCharPos := 0 
    ; RETURN Left = Right 
    END TextAttrIsEqual 

(* VISIBLE: *) 
; PROCEDURE IncludeLrVisibleIn 
    ( Ref : LinesRefMeatTyp ; Value : WindowNoTyp ) 

  = BEGIN (* IncludeLrVisibleIn *) 
      Ref . LrVisibleIn := Ref . LrVisibleIn + WindowNoSetTyp { Value } 
    END IncludeLrVisibleIn 

(* VISIBLE: *) 
; PROCEDURE ExcludeLrVisibleIn 
    ( Ref : LinesRefMeatTyp ; Value : WindowNoTyp ) 

  = BEGIN (* ExcludeLrVisibleIn *) 
      Ref . LrVisibleIn := Ref . LrVisibleIn - WindowNoSetTyp { Value } 
    END ExcludeLrVisibleIn 

(* VISIBLE: *) 
; PROCEDURE LineIsVisible ( Ref : LinesRefMeatTyp ) : BOOLEAN 

  = BEGIN (* LineIsVisible *) 
      RETURN Ref . LrVisibleIn # WindowNoSetEmpty 
    END LineIsVisible 

(* VISIBLE: *) 
; PROCEDURE InsertLinesRefToRight 
    ( InsertToRightOfRef : LinesRefTyp ; RefToInsert : LinesRefMeatTyp ) 

  = VAR LRightRef : LinesRefTyp 

  ; BEGIN (* InsertLinesRefToRight *) 
      LRightRef := InsertToRightOfRef . LrRightLink 
    ; InsertToRightOfRef . LrRightLink := RefToInsert 
    ; RefToInsert . LrLeftLink := InsertToRightOfRef 
    ; RefToInsert . LrRightLink := LRightRef 
    ; LRightRef . LrLeftLink := RefToInsert 
    END InsertLinesRefToRight 

(* VISIBLE: *) 
; PROCEDURE InsertLinesRefToLeft 
    ( InsertToLeftOfRef : LinesRefTyp ; RefToInsert : LinesRefMeatTyp ) 

  = VAR LLeftRef : LinesRefTyp 

  ; BEGIN (* InsertLinesRefToLeft *) 
      LLeftRef := InsertToLeftOfRef . LrLeftLink 
    ; InsertToLeftOfRef . LrLeftLink := RefToInsert 
    ; RefToInsert . LrRightLink := InsertToLeftOfRef 
    ; RefToInsert . LrLeftLink := LLeftRef 
    ; LLeftRef . LrRightLink := RefToInsert 
    END InsertLinesRefToLeft 

(* VISIBLE: *) 
; PROCEDURE UnlinkLinesRef ( LinesRef : LinesRefMeatTyp ) 
  (* Delete LinesRef from whatever list it is in. 
     No action if LinesRef is NIL. 
  *) 

  = BEGIN (* UnlinkLinesRef *) 
      UnlinkLinesRefRange ( LinesRef , LinesRef ) 
    END UnlinkLinesRef 

(* VISIBLE: *) 
; PROCEDURE InsertLinesRefRangeToLeft 
    ( InsertToLeftOfRef : LinesRefTyp 
    ; FromLinesRef : LinesRefMeatTyp 
    ; ThruLinesRef : LinesRefMeatTyp 
    ) 
  (* No action if any of the pointers is NIL.  Otherwise, assumes 
     InsertToLeftOfRef is in a double, circular linked list and 
     FromLinesRef .. ThruLinesRef are doubly-linked within, ends don't matter. 
  *) 

  = VAR LLeftRef : LinesRefTyp 

  ; BEGIN (* InsertLinesRefToLeft *) 
      IF InsertToLeftOfRef # NIL AND FromLinesRef # NIL AND ThruLinesRef # NIL 
      THEN 
        LLeftRef := InsertToLeftOfRef . LrLeftLink 
      ; InsertToLeftOfRef . LrLeftLink := ThruLinesRef 
      ; ThruLinesRef . LrRightLink := InsertToLeftOfRef 
      ; FromLinesRef . LrLeftLink := LLeftRef 
      ; LLeftRef . LrRightLink := FromLinesRef  
      END (* IF *) 
    END InsertLinesRefRangeToLeft 

(* VISIBLE: *) 
; PROCEDURE UnlinkLinesRefRange 
    ( FromLinesRef : LinesRefMeatTyp ; ThruLinesRef : LinesRefMeatTyp ) 
  (* Delete FromLinesRef .. ThruLinesRef from whatever list they are in. 
     No action if either pointer is NIL.  Otherwise, assumes they are in a
     double, circular linked list. Leave the nodes linked among themselves, 
     so they can be reinserted somewhere later. 
  *) 

  = BEGIN (* UnlinkLinesRefRange *) 
      IF FromLinesRef # NIL  
         AND FromLinesRef . LrLeftLink # NIL 
         AND FromLinesRef . LrRightLink # NIL 
         AND ThruLinesRef # NIL  
         AND ThruLinesRef . LrLeftLink # NIL 
         AND ThruLinesRef . LrRightLink # NIL 
         (* Immunity to damaged checkpoint files. *) 
      THEN 
        FromLinesRef . LrLeftLink . LrRightLink := ThruLinesRef . LrRightLink 
      ; ThruLinesRef . LrRightLink . LrLeftLink := FromLinesRef . LrLeftLink 
      ; FromLinesRef . LrLeftLink := NIL 
      ; ThruLinesRef . LrRightLink := NIL 
      END (* IF *) 
    END UnlinkLinesRefRange 

(* VISIBLE: *) 
; PROCEDURE ResetAllLrHasMarkFields ( ImageTrans : ImageTransientTyp ) 

  = VAR LImagePers : ImagePersistentTyp 
  ; VAR LHeader : LinesRefTyp 
  ; VAR LLine : LinesRefTyp 

  ; BEGIN 
      IF ImageTrans # NIL 
      THEN
        LImagePers := ImageTrans . ItPers 
      ; IF LImagePers # NIL 
        THEN
          LHeader := LImagePers . IpLineHeaderRef 
        ; IF LHeader # NIL 
          THEN
            LLine := LHeader . LrRightLink 
          ; LOOP 
              IF LLine = LHeader 
              THEN EXIT 
              ELSE 
                TYPECASE LLine
                OF LinesRefMeatTyp ( TLinesRef ) 
                => TLinesRef . LrHasMark := FALSE 
                ELSE (* Just paranoia, in case of damaged data structure. *) 
                END (* TYPECASE *) 
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END ResetAllLrHasMarkFields 

(* VISIBLE: *) 
; PROCEDURE RecomputeLrHasMark ( Mark : LineMarkMeatTyp ) 
  (* Mark is about to be removed from this line. If it points to a LinesRec,
     maybe reset the LrHasMark field of the LinesRec. *)  

  = BEGIN (* RecomputeLrHasMark *)
      IF Mark # NIL AND Mark . LmLinesRef # NIL 
      THEN
        TYPECASE Mark . LmLeftLink 
        OF NULL =>  
        | LineMarkMeatTyp ( TLeftMark ) 
        => IF TLeftMark . LmLinesRef = Mark . LmLinesRef 
           THEN (* Another Mark pointing to the same LinesRec exists, so
                   leave LrHasMark alone. *)  
             RETURN 
           END (* IF *) 
        ELSE 
        END (* TYPECASE *) 
      ; TYPECASE Mark . LmRightLink 
        OF NULL =>  
        | LineMarkMeatTyp ( TNextMark ) 
        => IF TNextMark . LmLinesRef = Mark . LmLinesRef 
           THEN (* Another Mark pointing to the same LinesRec exists, so
                   leave LrHasMark alone.  It will be TRUE. *)  
             RETURN 
           END (* IF *) 
        ELSE 
        END (* TYPECASE *)
      ; Mark . LmLinesRef . LrHasMark := FALSE  
      END (* IF *) 
    END RecomputeLrHasMark  

(* VISIBLE: *) 
; PROCEDURE LinkLineMarkToRight 
    ( InsertToRightOfRef : LineMarkTyp ; RefToInsert : LineMarkMeatTyp ) 
  RAISES { Backout } 

  = VAR LRightRef : LineMarkTyp 

  ; BEGIN (* LinkLineMarkToRight *) 
      Assert 
        ( RefToInsert . LmLeftLink = NIL AND RefToInsert . LmRightLink = NIL 
        , AFT . A_LinkLineMarkToRight_Reinserting_node
        ) 
    ; LRightRef := InsertToRightOfRef . LmRightLink 
    ; InsertToRightOfRef . LmRightLink := RefToInsert 
    ; RefToInsert . LmLeftLink := InsertToRightOfRef 
    ; RefToInsert . LmRightLink := LRightRef 
    ; LRightRef . LmLeftLink := RefToInsert 
    END LinkLineMarkToRight 

(* VISIBLE: *) 
; PROCEDURE LinkLineMarkToLeft 
    ( InsertToLeftOfRef : LineMarkTyp ; RefToInsert : LineMarkMeatTyp ) 
  RAISES { Backout } 

  = VAR LLeftRef : LineMarkTyp 

  ; BEGIN (* LinkLineMarkToLeft *) 
      Assert 
        ( RefToInsert . LmLeftLink = NIL AND RefToInsert . LmRightLink = NIL 
        , AFT . A_LinkLineMarkToLeft_Reinserting_node
        ) 
    ; LLeftRef := InsertToLeftOfRef . LmLeftLink 
    ; InsertToLeftOfRef . LmLeftLink := RefToInsert 
    ; RefToInsert . LmRightLink := InsertToLeftOfRef 
    ; RefToInsert . LmLeftLink := LLeftRef 
    ; LLeftRef . LmRightLink := RefToInsert 
    END LinkLineMarkToLeft 

(* VISIBLE: *) 
; PROCEDURE UnlinkLineMark ( LineMark : LineMarkMeatTyp ) 
  (* Delete LineMark from whatever list it is in. 
     No action if LineMark or either of its links is NIL. *) 

  = BEGIN (* UnlinkLineMark *) 
      IF LineMark # NIL 
         AND LineMark . LmLeftLink # NIL 
         AND LineMark . LmRightLink # NIL 
      THEN
        LineMark . LmLeftLink . LmRightLink := LineMark . LmRightLink 
      ; LineMark . LmRightLink . LmLeftLink := LineMark . LmLeftLink 
      ; LineMark . LmLeftLink := NIL 
      ; LineMark . LmRightLink := NIL 
      END (* IF *) 
    END UnlinkLineMark 

; PROCEDURE UnlinkLineMarkRange 
    ( FromMark : LineMarkMeatTyp ; ThruMark : LineMarkMeatTyp ) 
  (* Delete FromMark .. ThruMark from whatever list they are in. 
     No action if either pointer is NIL.  Otherwise, assumes they are in a
     double, circular linked list. Leave the nodes linked among themselves, 
     so they can be reinserted somewhere later. *) 
  (* NOTE: Does not update IpMarkCt! *) 

  = BEGIN (* UnlinkLineMarkRange *) 
      IF FromMark # NIL AND ThruMark # NIL  
      THEN 
        FromMark . LmLeftLink . LmRightLink := ThruMark . LmRightLink 
      ; ThruMark . LmRightLink . LmLeftLink := FromMark . LmLeftLink 
      ; FromMark . LmLeftLink := NIL 
      ; ThruMark . LmRightLink := NIL 
      END (* IF *) 
    END UnlinkLineMarkRange 

(* VISIBLE: *) 
; PROCEDURE InsertLineMarkToLeft 
    ( InsertMark : LineMarkMeatTyp 
    ; SuccMark : LineMarkTyp 
    ; Image : ImageTransientTyp := NIL 
    )
  RAISES { Backout } 
  (* Higher-level.  Links in and also takes care of IpMarkCt and LrHasMark. *) 

  = BEGIN 
      IF InsertMark # NIL AND SuccMark # NIL 
      THEN 
        LinkLineMarkToLeft 
          ( InsertToLeftOfRef := SuccMark 
          , RefToInsert := InsertMark   
          ) 
      ; InsertMark . LmLinesRef . LrHasMark := TRUE 
      ; IF Image # NIL 
        THEN INC ( Image . ItPers . IpMarkCt ) 
        END (* IF *) 
      END (* IF *) 
    END InsertLineMarkToLeft 

(* VISIBLE: *) 
; PROCEDURE InsertLineMarkToRight 
    ( InsertMark : LineMarkMeatTyp 
    ; PredMark : LineMarkTyp 
    ; Image : ImageTransientTyp := NIL 
    )
  RAISES { Backout } 
  (* Higher-level.  Links in and also takes care of IpMarkCt and LrHasMark. *) 

  = BEGIN 
      IF InsertMark # NIL AND PredMark # NIL 
      THEN 
        LinkLineMarkToRight 
          ( InsertToRightOfRef := PredMark 
          , RefToInsert := InsertMark   
          ) 
      ; InsertMark . LmLinesRef . LrHasMark := TRUE 
      ; IF Image # NIL 
        THEN INC ( Image . ItPers . IpMarkCt ) 
        END (* IF *) 
      END (* IF *) 
    END InsertLineMarkToRight 

(* VISIBLE: *) 
; PROCEDURE InsertLineMark
    ( InsertMark : LineMarkMeatTyp 
    ; Image : ImageTransientTyp 
    ; LeftIfEqual : BOOLEAN 
      (* ^Insert to left of any equal marks, otherwise to right of same. *) 
    ; PredHintMark : LineMarkMeatTyp := NIL 
    ; SuccHintMark : LineMarkMeatTyp := NIL 
      (* PredHintMark and SuccHintMark are Optional. If caller knows of a mark
         that should be a predecessor/successor to the spot where InsertMark
         goes, it can supply it for better performance.
      *) 
    )
  : BOOLEAN (* Success. *) 
  RAISES { Backout } 
  (* Even higher-level yet.  Sorts into the right place. *) 

  = VAR IlmImagePers : ImagePersistentTyp 
  ; VAR IlmMarkHeader : PaintHs . LineMarkTyp 

  ; PROCEDURE IlmSearch ( Pred : LineMarkTyp ; Succ : LineMarkTyp ) 
    : BOOLEAN (* Success. *) 
    RAISES { Backout } 
  
    = VAR LPred : LineMarkTyp 
    ; VAR LSucc : LineMarkTyp 
    ; VAR LCompare : [ - 1 .. 1 ] 

    ; BEGIN 
        LPred := Pred 
      ; LSucc := Succ 
      ; IF LPred # IlmMarkHeader 
        THEN 
          LCompare := CompareLineMarks ( InsertMark , Pred ) 
        ; IF LCompare < 0 
          THEN (* InsertMark goes left of Pred. *) 
            RETURN FALSE 
          ELSIF LCompare = 0 
                AND LeftIfEqual 
                AND Pred . LmLeftLink # IlmMarkHeader 
                AND CompareLineMarks ( Pred . LmLeftLink , Pred ) = 0 
(* TODO:  Write a LineMarksEqual and use it here and below. *) 
          THEN (* Another way InsertMark goes left of Pred. *) 
            RETURN FALSE 
          END (* IF *) 
        END (* IF *) 
      ; IF Succ # IlmMarkHeader 
        THEN 
          LCompare := CompareLineMarks ( InsertMark , Succ ) 
        ; IF LCompare > 0 
          THEN (* InsertMark goes right of Succ. *) 
            RETURN FALSE 
          ELSIF LCompare = 0 
                AND NOT LeftIfEqual 
                AND Succ . LmRightLink # IlmMarkHeader 
                AND CompareLineMarks ( Succ . LmRightLink , Succ ) = 0 
          THEN (* Another way InsertMark goes right of Succ. *) 
            RETURN FALSE 
          END (* IF *) 
        END (* IF *) 
      ; LOOP (* Trying each end of range LPred .. LSucc alternately. *) 
          (* INVARIANT: LPred <= InsertMark <= LSucc *) 
          IF LPred = LSucc AND LPred # IlmMarkHeader 
          THEN (* This should never happen, unless we have some bad TokMarks
                  or something. 
               *) 
            RETURN FALSE 
          ELSE 
          (* Try increasing LPred *) 
            IF LPred . LmRightLink = IlmMarkHeader 
            THEN 
              InsertLineMarkToLeft 
                ( InsertMark , SuccMark := IlmMarkHeader , Image := Image ) 
            ; RETURN TRUE 
            ELSE 
              LPred := LPred . LmRightLink (* Implied NARROW can't fail. *) 
            ; LCompare := CompareLineMarks ( InsertMark , LPred ) 
            ; IF LCompare < 0 
              THEN (* We passed InsertMark.  It goes before LPred. *) 
                InsertLineMarkToLeft 
                  ( InsertMark , SuccMark := LPred , Image := Image ) 
              ; RETURN TRUE 
              ELSIF LCompare = 0 AND LeftIfEqual 
              THEN (* Another way it goes before LPred. *) 
                InsertLineMarkToLeft 
                  ( InsertMark , SuccMark := LPred , Image := Image ) 
              ; RETURN TRUE 
              END (* IF *) 
            END (* IF *) 
          (* Now try decreasing LSucc *) 
          (* If we get here, LSucc's left neighbor is not the list header. *)
          ; LSucc := LSucc . LmLeftLink (* Implied NARROW can't fail. *) 
          ; LCompare := CompareLineMarks ( InsertMark , LSucc ) 
          ; IF LCompare > 0 
            THEN (* We passed InsertMark.  It goes after LSucc. *) 
              InsertLineMarkToRight 
                ( InsertMark , PredMark := LSucc , Image := Image ) 
            ; RETURN TRUE 
            ELSIF LCompare = 0 AND NOT LeftIfEqual 
            THEN (* Another way it goes after LSucc. *) 
              InsertLineMarkToRight 
                ( InsertMark , PredMark := LSucc , Image := Image ) 
            ; RETURN TRUE 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      END IlmSearch 

  ; BEGIN (* InsertLineMark *) 
      IF Image # NIL AND InsertMark # NIL 
      THEN 
        IlmImagePers := Image . ItPers 
      ; IlmMarkHeader := IlmImagePers . IpMarkHeader
      ; IF PredHintMark = NIL 
        THEN
          IF SuccHintMark = NIL 
          THEN (* Neither hint given.  Fall through. *) 
          ELSE (* SuccHintMark only. *) 
            IF IlmSearch ( IlmMarkHeader , SuccHintMark ) 
            THEN RETURN TRUE
            ELSIF (* That failed.  Try the other range. *) 
              IlmSearch ( SuccHintMark , IlmMarkHeader ) 
            THEN RETURN TRUE 
            END (* IF *) 
          END (* IF *) 
        ELSE
          IF SuccHintMark = NIL 
          THEN (* PredHintMark only. *) 
            IF IlmSearch ( PredHintMark , IlmMarkHeader ) 
            THEN RETURN TRUE
            ELSIF (* That failed.  Try the other range. *) 
              IlmSearch ( IlmMarkHeader , PredHintMark ) 
            THEN RETURN TRUE
            END (* IF *) 
          ELSE (* Both PredHintMark and SuccHintMark. *) 
            IF IlmSearch ( PredHintMark , SuccHintMark ) 
            THEN RETURN TRUE
            ELSIF (* That failed.  Try the upper range. *) 
              IlmSearch ( IlmMarkHeader , PredHintMark ) 
            THEN RETURN TRUE 
            ELSIF (* That failed.  Try the lower range. *) 
              IlmSearch ( SuccHintMark , IlmMarkHeader ) 
            THEN RETURN TRUE
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      (* Last resort.  Try searching the entire range. *) 
      ; RETURN IlmSearch ( IlmMarkHeader , IlmMarkHeader ) 
      ELSE RETURN FALSE 
      END (* IF *) 
    END InsertLineMark  

(* VISIBLE: *) 
; PROCEDURE DeleteLineMark
    ( Mark : LineMarkMeatTyp 
    ; VAR PredMark : LineMarkTyp 
      (* For caller to keep in case it needs to undo the deletion. *)  
    ; Image : ImageTransientTyp := NIL 
    ) 
  (* Higher-level.  Unlinks and also takes care of IpMarkCt and LrHasMark. *) 

  = BEGIN 
      IF Mark # NIL 
      THEN 
        PredMark := Mark . LmLeftLink 
      ; RecomputeLrHasMark ( Mark ) 
      ; UnlinkLineMark ( Mark ) 
      ; IF Image # NIL 
        THEN DEC ( Image . ItPers . IpMarkCt ) 
        END (* IF *) 
      END (* IF *) 
    END DeleteLineMark 

(* VISIBLE: *) 
; PROCEDURE SwapMarkOrder ( LeftMark : LineMarkTyp ) 
  (* Swap LeftMark with its right neighbor. *) 

  = VAR LRightMark : LineMarkTyp 
  ; VAR LFarLeftMark : LineMarkTyp 
  ; VAR LFarRightMark : LineMarkTyp 

  ; BEGIN (* SwapMarkOrder *) 
      IF LeftMark # NIL 
      THEN 
        LRightMark := LeftMark . LmRightLink 
      ; IF LRightMark # NIL AND LRightMark # LeftMark 
        THEN
          LFarLeftMark := LeftMark . LmLeftLink 
        ; IF LFarLeftMark # LRightMark 
          THEN 
            LFarRightMark := LRightMark . LmRightLink 
          ; LFarLeftMark . LmRightLink := LRightMark 
          ; LRightMark . LmRightLink := LeftMark 
          ; LeftMark . LmRightLink := LFarRightMark 
          ; LFarRightMark . LmLeftLink := LeftMark 
          ; LeftMark . LmLeftLink := LRightMark 
          ; LRightMark . LmLeftLink := LFarLeftMark 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END SwapMarkOrder 

(* VISIBLE: *) 
; PROCEDURE CompareLineMarks ( Left , Right : LineMarkMeatTyp ) 
  : [ - 1 .. 1 ] 
  RAISES { Backout } 
  (* Takes into account LmTokMark , LmLineNo, and LmCharPos *) 

  = VAR LResult : [ - 1 .. 1 ] 

  ; BEGIN 
      TRY 
        LResult := Marks . Compare ( Left . LmTokMark , Right . LmTokMark ) 
      EXCEPT Marks . Unordered 
      => CantHappen ( AFT . A_CompareLineMarks_Tok_marks_unordered ) 
      END (* TRY EXCEPT *) 
    ; IF LResult = 0 
      THEN 
        LResult := Integer . Compare ( Left . LmLineNo , Right . LmLineNo ) 
      ; IF LResult = 0 
        THEN 
          RETURN Integer . Compare ( Left . LmCharPos , Right . LmCharPos )
        ELSE RETURN LResult 
        END (* IF *)  
      ELSE RETURN LResult 
      END (* IF *)  
    END CompareLineMarks 

(* VISIBLE: *) 
; PROCEDURE GetMarksInOrder 
    ( Mark1 : LineMarkMeatTyp
    ; Mark2 : LineMarkMeatTyp
    ; VAR Left : LineMarkMeatTyp
    ; VAR Right : LineMarkMeatTyp
    ) 
  RAISES { Backout } 
  (* Put Mark1 and Mark2 into nondescending order. *) 

  = BEGIN
      IF Mark1 # NIL AND Mark2 # NIL 
      THEN 
        CASE CompareLineMarks ( Mark1 , Mark2 ) 
        OF - 1 
        => Left := Mark1 
        ; Right := Mark2  
        | 0 (* Treat as no selection. *) 
        => Left := NIL 
        ; Right := NIL
        | 1 
        => Left := Mark2 
        ; Right := Mark1  
        END (* CASE *) 
      ELSE 
        Left := NIL 
      ; Right := NIL
      END (* IF *) 
    END GetMarksInOrder 

(* VISIBLE: *) 
; PROCEDURE BruteForceVerifyLineMarks 
    ( ImageTrans : ImageTransientTyp ; DoCheckOrder : BOOLEAN := TRUE ) 
  RAISES { Backout } 

  = VAR LImagePers : PaintHs . ImagePersistentTyp  
  ; VAR LHeader : LineMarkTyp 
  ; VAR LPrevMark : LineMarkTyp 
  ; VAR LMark : LineMarkMeatTyp 
  ; VAR LMarkCt : LbeStd . MarkNoTyp 
  ; VAR LBegOfImageMark : Marks . TokMarkTyp 
  ; VAR LEndOfImageMark : Marks . TokMarkTyp 

  ; BEGIN
      IF ImageTrans # NIL AND Options . DebugLevel >= 1  
      THEN 
        LImagePers := ImageTrans . ItPers 
      ; LMarkCt := 0 
      ; LHeader := LImagePers . IpMarkHeader 
      ; Assert
          ( LHeader # NIL 
          , AFT . A_BruteForceVerifyLineMarks_No_header
          ) 
      ; Assert 
          ( LHeader . LmLeftLink # NIL AND LHeader . LmRightLink # NIL 
          , AFT . A_BruteForceVerifyLineMarks_NIL_header_links
          ) 
      ; IF LHeader . LmLeftLink = LHeader AND LHeader . LmRightLink = LHeader
        THEN (* OK, empty list. *) 
        ELSE 
          LPrevMark := LHeader 
        ; LMark := LHeader . LmRightLink 
        ; IF DoCheckOrder 
          THEN 
            LineMarks . GetLMBegOfImage 
              ( LImagePers . IpLang 
              , LImagePers . IpEstRoot 
              , (* VAR *) LBegOfImageMark 
              ) 
          ; TRY 
              Assert 
                ( Marks . Compare ( LBegOfImageMark , LMark . LmTokMark ) <= 0 
                , AFT . A_BruteForceVerifyLineMarks_First_mark_not_after_BOI 
                ) 
            EXCEPT Marks . Unordered 
            => CantHappen 
                 ( AFT . A_BruteForceVerifyLineMarks_First_mark_unordered_WRT_BOI ) 
            END (* TRY EXCEPT *) 
          END (* IF *) 
        ; LOOP 
            INC ( LMarkCt ) 
          ; Assert 
              ( LMark . LmLeftLink # NIL AND LMark . LmRightLink # NIL 
              , AFT . A_BruteForceVerifyLineMarks_NIL_meat_links
              ) 
          ; Assert 
              ( LMark . LmLeftLink # LMark AND LMark . LmRightLink # LMark 
              , AFT . A_BruteForceVerifyLineMarks_cyclic_meat_links
              ) 
          ; Assert 
              ( LMark . LmLeftLink = LPrevMark 
              , AFT . A_BruteForceVerifyLineMarks_inconstent_left_link
              ) 
          ; TYPECASE LMark . LmRightLink 
            OF LineMarkMeatTyp ( TRightMark )  
            => IF DoCheckOrder 
               THEN 
                 Assert 
                   ( CompareLineMarks ( LMark , TRightMark ) <= 0 
                   , AFT . A_BruteForceVerifyLineMarks_LineMark_out_of_order 
                   ) 
              END (* IF *) 
            ; LPrevMark := LMark 
            ; LMark := TRightMark 
            ELSE 
              Assert 
                ( LMark . LmRightLink = LHeader 
                  AND LHeader . LmLeftLink = LMark 
                , AFT . A_BruteForceVerifyLineMarks_bad_closing 
                ) 
            ; IF DoCheckOrder 
              THEN 
                LineMarks . GetEndOfImage 
                  ( LImagePers . IpLang 
                  , LImagePers . IpEstRoot 
                  , (* VAR *) LEndOfImageMark 
                  ) 
              ; TRY
                  Assert 
                    ( Marks . Compare ( LMark . LmTokMark , LEndOfImageMark ) 
                      <= 0 
                    , AFT . A_BruteForceVerifyLineMarks_LastMarkNotBeforeEOI 
                    ) 
                EXCEPT Marks . Unordered  
                => CantHappen 
                     ( AFT . A_BruteForceVerifyLineMarks_LastMarkUnorderedReEOI ) 
                END (* TRY EXCEPT *) 
              END (* IF *) 
            ; EXIT 
            END (* TYPECASE *) 
          END (* LOOP *) 
        END (* IF *) 
      ; Assert 
          ( LMarkCt = LImagePers . IpMarkCt 
          , AFT . A_BruteForceVerifyLineMarks_Wrong_mark_count_in_image 
          ) 
      ; LImagePers . IpMarkCt := LMarkCt 
        (* Repair it.  This can help when loading a damaged checkpoint file. *)
      END (* IF *) 
    END BruteForceVerifyLineMarks 

(* VISIBLE: *) 
; PROCEDURE TextAttrArrayCt 
    ( ImageTrans : ImageTransientTyp ) : CARDINAL 
(* NOTE: TextAttrArrayCt and TextAttrElemCt are coded as separate functions
         to make them easy to call from a debugger.
*) 

  = VAR LImagePers : ImagePersistentTyp 
  ; VAR LLinesRef : LinesRefTyp
  ; VAR LResult : CARDINAL
  ; VAR LTextAttrArrayRef : TextAttrArrayRefTyp  

  ; BEGIN 
      LImagePers := ImageTrans . ItPers 
    ; LResult := 0 
    ; LLinesRef := LImagePers . IpLineHeaderRef . LrRightLink  
    ; LOOP 
        TYPECASE LLinesRef  
        OF LinesRefMeatTyp ( TLRMeat ) 
        => LTextAttrArrayRef := TLRMeat . LrTextAttrArrayRef 
        ; IF LTextAttrArrayRef # NIL 
          THEN  
            INC ( LResult ) 
          END (* IF *) 
        ; LLinesRef := TLRMeat . LrRightLink 
        ELSE EXIT 
        END (* TYPECASE *) 
      END (* LOOP *) 
    ; RETURN LResult 
    END TextAttrArrayCt 

(* VISIBLE: *) 
; PROCEDURE TextAttrElemCt 
    ( ImageTrans : ImageTransientTyp ) : CARDINAL 

  = VAR LImagePers : ImagePersistentTyp 
  ; VAR LLinesRef : LinesRefTyp
  ; VAR LResult : CARDINAL
  ; VAR LTextAttrArrayRef : TextAttrArrayRefTyp  

  ; BEGIN 
      LImagePers := ImageTrans . ItPers 
    ; LResult := 0 
    ; LLinesRef := LImagePers . IpLineHeaderRef . LrRightLink  
    ; LOOP 
        TYPECASE LLinesRef  
        OF LinesRefMeatTyp ( TLRMeat ) 
        => LTextAttrArrayRef := TLRMeat . LrTextAttrArrayRef 
        ; IF LTextAttrArrayRef # NIL 
          THEN  
            INC ( LResult , NUMBER ( LTextAttrArrayRef ^ ) ) 
          END (* IF *) 
        ; LLinesRef := TLRMeat . LrRightLink 
        ELSE EXIT 
        END (* TYPECASE *) 
      END (* LOOP *) 
    ; RETURN LResult 
    END TextAttrElemCt 

(* VISIBLE: *) 
; PROCEDURE DisplayTextAttrStats ( ImageTrans : ImageTransientTyp ) 

  = VAR LArrayCt : CARDINAL 
  ; VAR LElemCt : CARDINAL 
  ; VAR LFactor : REAL  

  ; BEGIN 
      LArrayCt := TextAttrArrayCt ( ImageTrans ) 
    ; LElemCt := TextAttrElemCt ( ImageTrans ) 
    ; LFactor := FLOAT ( LElemCt ) / FLOAT ( LArrayCt ) 
    ; Assertions . MessageText 
        ( Fmt . Int ( LArrayCt ) 
          & " TextAttr arrays, "  
          & Fmt . Int ( LElemCt ) 
          & " TextAttr elements, "  
          & Fmt . Real ( LFactor , style := Fmt . Style . Fix , prec := 2 ) 
          & " times."  
        ) 
    END DisplayTextAttrStats 

(* VISIBLE: *) 
; PROCEDURE InitDefaults ( ImageRef : ImageRefTyp ) : ImageRefTyp 

  = BEGIN (* InitDefaults *) 
      (* Display . CreateEmptyLinesRefList ( ImageRef ) *) 
(* CHECK: What about IpMarkHeader? *) 
      RETURN ImageRef 
    END InitDefaults 

(* VISIBLE: *) 
; PROCEDURE IpInitDefaults ( Ip : ImagePersistentTyp ) 
  : ImagePersistentTyp 

  = BEGIN (* IpInitDefaults *) 
      Ip . IpMagic := IpMagicNo 
    ; Ip . IpLang := LbeStd . LangNull 
    ; Ip . IpLineHeaderRef := NIL 
    ; Ip . IpEstRoot := NIL 
    ; Ip . IpSemRoot := NIL 
    ; Ip . IpMarkCt := 0 
    ; Ip . IpMarkHeader := NIL 
    ; Ip . IpTempEditState := TempEditStateTyp . TeStateIdle 
    ; Ip . IpTempEditRef := NIL 
    ; Ip . IpImageName := NIL 
    ; Ip . IpAbsPklFileName := NIL 
    ; Ip . IpAbsTextFileName := NIL 
    ; Ip . IpVerNo := 0 
    ; Ip . IpVerUpdKind := 0 
    ; Ip . IpVerState := VerStateTyp . VerStateNull 
    ; Ip . IpHistoryText := NIL  
    ; Ip . IpLineCtDisplay := 0 
    ; Ip . IpLineCtIsExact := FALSE  
    ; Ip . IpLastCommand := NIL 
    ; Ip . IpCrashCommand := NIL 
    ; Ip . IpCrashCode := ORD ( MessageCodes . T . NullCode ) 
    ; Ip . IpSchutzDate := Version . DateString   
    ; Ip . IpIsParsed := TRUE 
    ; Ip . IpIsAnalyzed := TRUE 
    ; Ip . IpIsMutatedSinceCommandStarted := FALSE 
    ; RETURN Ip 
    END IpInitDefaults 

(* VISIBLE: *) 
; PROCEDURE ItInitDefaults ( It : ImageTransientTyp ) 
  : ImageTransientTyp 

  = BEGIN (* ItInitDefaults *) 
      It . ItPers := NIL 
    ; It . ItLangIdRef := NIL 
    ; It . ItVisibleIn := WindowNoSetEmpty 
    ; It . ItWindowList := NIL 
    ; It . ItHistoryWrT := NIL  
    ; It . ItScannerIf := NIL 
    ; It . ItIsSaved := TRUE 
    ; RETURN It 
    END ItInitDefaults 

; BEGIN (* PaintHs *) 
  END PaintHs 
. 
