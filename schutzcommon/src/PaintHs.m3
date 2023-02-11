
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2023, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE PaintHs 

(* Data structures for painting the screen. *) 

(* Library: *) 
; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen 
; IMPORT Fmt 
; IMPORT Integer
; IMPORT Stdio 
; IMPORT TextWr
; IMPORT Thread 
; IMPORT Wr 

(* Schutz: *) 
; FROM Failures IMPORT Backout  
; IMPORT LbeStd 
; IMPORT LineMarks 
; IMPORT Marks
; IMPORT MessageCodes 
; IMPORT Misc 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT Version 
; IMPORT VersionedFiles  
; IMPORT WindowPrivate 

<* PRAGMA LL *> 

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

(* EXPORTED: *) 
; PROCEDURE TextAttrIsEqual ( VALUE Left , Right : TextAttrTyp ) : BOOLEAN 
  (* Equal, except for the TaCharPos field. *) 

  = BEGIN 
      Left . TaCharPos := 0 
    ; Right . TaCharPos := 0 
    ; RETURN Left = Right 
    END TextAttrIsEqual 

(* EXPORTED: *) 
; PROCEDURE IncludeLrVisibleIn 
    ( Ref : LinesRefMeatTyp ; Value : WindowNoTyp ) 

  = BEGIN (* IncludeLrVisibleIn *) 
      Ref . LrVisibleIn := Ref . LrVisibleIn + WindowNoSetTyp { Value } 
    END IncludeLrVisibleIn 

(* EXPORTED: *) 
; PROCEDURE ExcludeLrVisibleIn 
    ( Ref : LinesRefMeatTyp ; Value : WindowNoTyp ) 

  = BEGIN (* ExcludeLrVisibleIn *) 
      Ref . LrVisibleIn := Ref . LrVisibleIn - WindowNoSetTyp { Value } 
    END ExcludeLrVisibleIn 

(* EXPORTED: *) 
; PROCEDURE LineIsVisible ( Ref : LinesRefMeatTyp ) : BOOLEAN 

  = BEGIN (* LineIsVisible *) 
      RETURN Ref . LrVisibleIn # WindowNoSetEmpty 
    END LineIsVisible 

; VAR GNextLinesRefListNo := 0 

(* EXPORTED: *) 
; PROCEDURE NewLinesRefHeader ( ) : LinesRefHeaderTyp
  (* Allocate a LinesRef list header node and give it a new list number. *) 
  (* Does not assign anything to link fields. *) 

  = VAR LResult : LinesRefHeaderTyp

  ; BEGIN
      LResult := NEW ( LinesRefHeaderTyp )
    ; LResult . LrListNo := GNextLinesRefListNo
    ; LResult . LrUpdateRef := NIL 
    ; INC ( GNextLinesRefListNo )
    ; RETURN LResult 
    END NewLinesRefHeader 

(* EXPORTED: *) 
; PROCEDURE NewLinesRefMeat ( Header : LinesRefHeaderTyp  ) : LinesRefMeatTyp
  (* Allocate a LinesRef meat node, giving it the same list number as Header. *)

  = VAR LResult : LinesRefMeatTyp

  ; BEGIN
      LResult := NEW ( LinesRefMeatTyp )
    ; LResult . LrListNo := Header . LrListNo
    ; LResult . LrUpdateRef := NIL 
    ; RETURN LResult 
    END NewLinesRefMeat  

(* EXPORTED: *) 
; PROCEDURE UpdateLinesRefMeat
    ( Image : PaintHs . ImageTransientTyp
    ; VAR (* IN OUT *) LinesRef : LinesRefMeatTyp
    )
  (* Alter LinesRef by following update links until we reach the one
     with the same list no as the header from Image. *) 

  = VAR LHeader : PaintHs . LinesRefHeaderTyp 

  ; BEGIN
      IF Image = NIL THEN RETURN END (* IF *) 
    ; IF LinesRef = NIL THEN RETURN END (* IF *) 
    ; LHeader := Image . ItPers . IpLineHeaderRef 
    ; WHILE LinesRef . LrListNo < LHeader . LrListNo 
      DO <* ASSERT LinesRef . LrUpdateRef # NIL *>
        LinesRef := LinesRef . LrUpdateRef 
      END (* WHILE *) 
    END UpdateLinesRefMeat 

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
; PROCEDURE UnlinkLinesRef ( LinesRef : LinesRefMeatTyp ) 
  (* Delete LinesRef from whatever list it is in. 
     No action if LinesRef is NIL. 
  *) 

  = BEGIN (* UnlinkLinesRef *) 
      UnlinkLinesRefRange ( LinesRef , LinesRef ) 
    END UnlinkLinesRef 

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

; VAR GNextMarkListNo := 0 

(* EXPORTED: *) 
; PROCEDURE NewLineMarkHeader ( ) : LineMarkHeaderTyp
  (* Allocate a Mark list header node and give it a new list number. *)
  (* Does not assign anything to link fields. *) 

  = VAR LResult : LineMarkHeaderTyp

  ; BEGIN
      LResult := NEW ( LineMarkHeaderTyp )
    ; LResult . LmListNo := GNextMarkListNo
    ; LResult . LmUpdateRef := NIL 
    ; INC ( GNextMarkListNo )
    ; RETURN LResult 
    END NewLineMarkHeader 

(* EXPORTED: *) 
; PROCEDURE NewLineMarkMeat ( Header : LineMarkHeaderTyp ) : LineMarkMeatTyp
  (* Allocate a Mark meat node, giving it the same list number as Header. *)

  = VAR LResult : LineMarkMeatTyp

  ; BEGIN
      LResult := NEW ( LineMarkMeatTyp )
    ; LResult . LmListNo := Header . LmListNo
    ; LResult . LmUpdateRef := NIL 
    ; RETURN LResult 
    END NewLineMarkMeat

(* EXPORTED: *) 
; PROCEDURE UpdateLineMarkMeat
    ( Image : PaintHs . ImageTransientTyp
    ; VAR (* IN OUT *) Mark : LineMarkMeatTyp
    )
  (* Alter Mark by following update links until we reach the one
     with the same lint no as the header from Image.
     Additionally, update the LmLinesRef field of the updated Mark.
  *) 

  = VAR LHeader : LineMarkHeaderTyp 

  ; BEGIN
      IF Image = NIL THEN RETURN END (* IF *) 
    ; IF Mark = NIL THEN RETURN END (* IF *) 
    ; LHeader := Image . ItPers . IpMarkHeader  
    ; WHILE Mark . LmListNo < LHeader . LmListNo 
      DO
        <* ASSERT Mark . LmUpdateRef # NIL *>
        Mark := Mark . LmUpdateRef 
      END (* WHILE *)
    ; UpdateLinesRefMeat ( Image , (* IN OUT *) Mark . LmLinesRef ) 
    END UpdateLineMarkMeat 

(* EXPORTED: *) 
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

(* EXPORTED: *) 
; PROCEDURE MarkSsImage ( MarkSsValue : MarkSsTyp ) : TEXT
  = BEGIN
      CASE MarkSsValue 
      OF MarkSsTyp . MarkSsNull => RETURN "MarkSsNull"
      | MarkSsTyp . MarkSsStartSel => RETURN "MarkSsStartSel"
      | MarkSsTyp . MarkSsCursor => RETURN "MarkSsCursor"
      | MarkSsTyp . MarkSsEndSel => RETURN "MarkSsEndSel"
      | MarkSsTyp . MarkSsStartMatch => RETURN "MarkSsStartMatch"
      | MarkSsTyp . MarkSsEndMatch => RETURN "MarkSsEndMatch"
      | MarkSsTyp . MarkSsTemp => RETURN "MarkSsTemp"
      END (* CASE *) 
    END MarkSsImage 

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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
        OF - 1 (* < *) 
        => Left := Mark1 
        ; Right := Mark2  
        | 0 (* = *) (* Treat as no selection. *) 
        => Left := NIL 
        ; Right := NIL
        | 1 (* > *) 
        => Left := Mark2 
        ; Right := Mark1  
        END (* CASE *) 
      ELSE 
        Left := NIL 
      ; Right := NIL
      END (* IF *) 
    END GetMarksInOrder 

(* EXPORTED: *) 
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
(* LineMark counts can change.   TextEdit.DeleteBetweenMarks adds a temporary one.    
      ; Assert 
          ( LMarkCt = LImagePers . IpMarkCt 
          , AFT . A_BruteForceVerifyLineMarks_Wrong_mark_count_in_image 
          )
*)
      ; LImagePers . IpMarkCt := LMarkCt 
        (* Repair it.  This can help when loading a damaged checkpoint file. *)
      END (* IF *) 
    END BruteForceVerifyLineMarks

; CONST BoolImage = Misc . BooleanImageShort
; CONST RefanyImage = Misc . RefanyImage

(* EXPORTED: *)
; PROCEDURE WindowNoSetImage ( Set : WindowNoSetTyp ; Indent : INTEGER ) : TEXT

  = BEGIN
      RETURN "{?? NYI ??>"
(* TODO: Implement this.  And genericize it.  There are a few other
         *SetImage functions hanging around. *) 
    END WindowNoSetImage

; PROCEDURE NonNilText ( T : TEXT ) : TEXT

  = BEGIN
      IF T = NIL THEN RETURN "" ELSE RETURN T END (* IF *) 
    END NonNilText 

(* EXPORTED: *)
; PROCEDURE LinesRefImage ( FLinesRef : LinesRefTyp ; LineIndent : INTEGER )
  : TEXT 
  RAISES { Thread . Alerted , Wr . Failure } 
    
  = VAR LWrT : Wr . T
  ; VAR LBlanks : TEXT 
  ; VAR LResult : TEXT 
  ; VAR LAddlIndent := 4
  ; VAR LFullIndent := LineIndent + LAddlIndent 
  
  ; BEGIN
      IF FLinesRef = NIL
      THEN RETURN "NIL"
      ELSE 
        LWrT := TextWr . New ( ) 
      ; Wr . PutText ( LWrT , RefanyImage ( FLinesRef ) )  
      ; Wr . PutText ( LWrT , " LrLeftLink:" )
      ; Wr . PutText ( LWrT , RefanyImage ( FLinesRef . LrLeftLink ) ) 
      ; Wr . PutText ( LWrT , " LrRightLink:" )
      ; Wr . PutText ( LWrT , RefanyImage ( FLinesRef . LrRightLink ) ) 
      ; Wr . PutText ( LWrT , " LrListNo:" )
      ; Wr . PutText ( LWrT , PaintHs . ListNoImage ( FLinesRef . LrListNo ) ) 
      ; Wr . PutText ( LWrT , " LrGapAfter:" )
      ; Wr . PutText ( LWrT , BoolImage ( FLinesRef . LrGapAfter ) ) 
      ; TYPECASE FLinesRef
        OF LinesRefMeatTyp ( TLinesRefMeat )
        => LBlanks := Misc . Blanks ( LFullIndent ) 
        ; Wr . PutText ( LWrT , Wr . EOL )
        ; Wr . PutText ( LWrT , LBlanks )
        ; Wr . PutText ( LWrT , "LrBolTokMark:{" )
        ; Wr . PutText
            ( LWrT , Marks . MarkImage ( TLinesRefMeat . LrBolTokMark ) ) 
            
        ; Wr . PutText ( LWrT , Wr . EOL )
        ; Wr . PutText ( LWrT , LBlanks )
        ; Wr . PutText ( LWrT , "LrTextAddrArray:" ) 
        ; Wr . PutText
            ( LWrT , RefanyImage ( TLinesRefMeat . LrTextAttrArrayRef ) )
(* TODO:  Emit the elements. *) 
        ; Wr . PutText ( LWrT , "{ }" )
        
        ; Wr . PutText ( LWrT , Wr . EOL )
        ; Wr . PutText ( LWrT , LBlanks )
        ; Wr . PutText ( LWrT , "LrLineErrArray:" ) 
        ; Wr . PutText
            ( LWrT , RefanyImage ( TLinesRefMeat . LrLineErrArrayRef ) ) 
(* TODO:  Emit the elements. *) 
        
        ; Wr . PutText ( LWrT , Wr . EOL )
        ; Wr . PutText ( LWrT , LBlanks )
        ; Wr . PutText ( LWrT , "LrVisibleIn:" ) 
        ; Wr . PutText
            ( LWrT , WindowNoSetImage 
                ( TLinesRefMeat . LrVisibleIn , LFullIndent )
            ) 
        ; Wr . PutText ( LWrT , " LrLineCt:" )
        ; Wr . PutText ( LWrT , Fmt . Int ( TLinesRefMeat . LrLineCt ) ) 
        ; Wr . PutText ( LWrT , " LrIsStopper:" ) 
        ; Wr . PutText
            ( LWrT , BoolImage ( TLinesRefMeat . LrIsStopper ) ) 
        ; Wr . PutText ( LWrT , " LrHasMark:" ) 
        ; Wr . PutText ( LWrT , BoolImage ( TLinesRefMeat . LrHasMark ) )
        ; Wr . PutText ( LWrT , " LrFromPos:" ) 
        ; Wr . PutText ( LWrT , Fmt . Int ( TLinesRefMeat . LrFromPos ) ) 
        ; Wr . PutText ( LWrT , " LrLineLen:" ) 
        ; Wr . PutText ( LWrT , Fmt . Int ( TLinesRefMeat . LrLineLen ) ) 
        
        ; Wr . PutText ( LWrT , Wr . EOL )
        ; Wr . PutText ( LWrT , LBlanks )
        ; Wr . PutText ( LWrT , "LrLineText: " ) 
        ; Wr . PutText ( LWrT , RefanyImage ( TLinesRefMeat . LrLineText ) )
        ; Wr . PutChar ( LWrT , ' ' ) 
        ; Wr . PutText
            ( LWrT
            , Misc . QuoteText ( NonNilText ( TLinesRefMeat . LrLineText ) )
            )

        ELSE 
        END (* TYPECASE *)
      ; LResult := TextWr . ToText ( LWrT )
      ; RETURN LResult 
      END (* IF *) 
    END LinesRefImage 


(*
  ; PROCEDURE DumpHeader ( FHeader : LinesRefTyp ; NodeNo : INTEGER ) )

    = BEGIN
        Wr . PutText ( WrT , "NodeNo:" )
      ; WI ( NodeNo )
      ; Wr . PutChar ( WrT , ' ' )
      ; IF FHeader = NIL
        THEN Wr . PutText ( WrT , "NIL" )
        ELSE 
          WX ( FHeader ) 
        ; Wr . PutText ( WrT , " LrLeftLink: " )
        ; WX ( FHeader . LrLeftLink ) 
        ; Wr . PutText ( WrT , " LrRightLink: " )
        ; WX ( FHeader . LrRightLink )
        ; Wr . PutText ( WrT , " LrListNo:" )
        ; Wr . PutText ( WrT , PaintHs . ListNoImage ( FHeader . LrLineNo ) ) 
        ; Wr . PutText ( WrT , " LrGapAfter:" )
        ; Wr . PutText ( WrT , BoolImage ( FHeader . LrGapAfter ) ) 
        END (* IF *) 
      ; Wr . PutText ( Wr . EOL )
      END DumpHeader

  ; PROCEDURE Dump ( FLinesRef : LinesRefTyp ; NodeNo : INTEGER ) )

    = BEGIN
        DumpHeader ( FLinesRef , NodeNo )
      ; TYPECASE FLinesRef
        OF NIL =>
        | LinesRefMeatTyp ( TLinesRefMeat )
        
        ELSE
        END (* TYPECASE *) 
      END Dump
*)

(* EXPORTED: *)
; PROCEDURE WriteLinesListToStdout  
    ( StartLinesRef : LinesRefTyp ; Lang : LbeStd . LangTyp )

  = BEGIN
      WriteLinesListWr
        ( Stdio . stdout , StartLinesRef , Lang , "Stdout:" ) 
    END WriteLinesListToStdout 

(* EXPORTED: *)
; PROCEDURE WriteLinesListToFile 
    ( FileName : TEXT ; StartLinesRef : LinesRefTyp ; Lang : LbeStd . LangTyp )

  = VAR LWrT : Wr . T

  ; BEGIN
      LWrT := VersionedFiles . OpenWrite ( FileName ) 
    ; WriteLinesListWr
        ( LWrT , StartLinesRef
        , Lang
        , "LinesListDump file: \"" & FileName & "\""
        )
    ; Wr . Close ( LWrT ) 
    END WriteLinesListToFile 

(* EXPORTED: *)
; PROCEDURE WriteLinesListWr
    ( WrT : Wr . T
    ; StartLinesRef : LinesRefTyp
    ; Lang : LbeStd . LangTyp
    ; FileLabel : TEXT
    )
  RAISES { Nonlinear , Thread . Alerted , Wr . Failure } 
  (* May be circular, with or without a header, or linear with NILs at ends. *)

  = VAR LLinesRef : LinesRefTyp
  ; VAR LHeader : LinesRefTyp
  ; VAR LIndent : INTEGER := 0  

  ; PROCEDURE WllMoveLeft ( VAR FLinesRef : LinesRefTyp )
    : BOOLEAN (* Succeeded. *)
    RAISES { Thread . Alerted , Wr . Failure } 
    
    = BEGIN
        IF FLinesRef . LrLeftLink = NIL
        THEN   
          Wr . PutText ( WrT , "NIL LrLeftLink: " ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , LinesRefImage ( FLinesRef , LIndent ) ) 
        ; RETURN FALSE 
        ELSIF FLinesRef . LrLeftLink . LrRightLink # FLinesRef
        THEN
          Wr . PutText ( WrT , "Links to left don't match:" ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , LinesRefImage ( FLinesRef . LrLeftLink , LIndent ) ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , LinesRefImage ( FLinesRef , LIndent ) ) 
        ; RETURN FALSE 
        ELSE
          FLinesRef := FLinesRef . LrLeftLink
        ; RETURN TRUE 
        END (* IF *) 
      END WllMoveLeft 

  ; PROCEDURE WllMoveRight ( VAR FLinesRef : LinesRefTyp )
    : BOOLEAN (* Succeeded. *) 
    RAISES { Thread . Alerted , Wr . Failure } 
    = BEGIN
        IF FLinesRef . LrRightLink = NIL
        THEN   
          Wr . PutText ( WrT , "NIL LrRightLink:" ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , LinesRefImage ( FLinesRef , LIndent ) ) 
        ; RETURN FALSE 
        ELSIF FLinesRef . LrRightLink . LrLeftLink # FLinesRef
        THEN
          Wr . PutText ( WrT , "Links to right don't match:" ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , LinesRefImage ( FLinesRef , LIndent ) ) 
        ; Wr . PutText ( WrT , Wr . EOL ) 
        ; Wr . PutText ( WrT , LinesRefImage ( FLinesRef . LrRightLink , LIndent ) ) 
        ; RETURN FALSE 
        ELSE
          FLinesRef := FLinesRef . LrRightLink
        ; RETURN TRUE 
        END (* IF *) 
      END WllMoveRight

  ; BEGIN (* WriteLinesListWr *)
  
    (* Ascertain what sort of cobweb we have. *) 
      LLinesRef := StartLinesRef
    ; TYPECASE LLinesRef
      OF NULL
      => Wr . PutText ( WrT , "NIL" )
      ; Wr . PutText ( WrT , Wr . EOL )
      ; RETURN
      
      | LinesRefMeatTyp ( TLinesRefMeat )
      => (* Starting in the middle of a list. *)
        (* Look for left end. *) 
        LOOP
          TYPECASE LLinesRef . LrLeftLink
          OF NULL 
          =>  Wr . PutText ( WrT , "No header on left." )
            ; Wr . PutText ( WrT , Wr . EOL )
            ; LHeader := NIL 
            ; EXIT
          
          | LinesRefMeatTyp ( TLeftLinesRefMeat )
           => IF TLeftLinesRefMeat = StartLinesRef
            THEN
              Wr . PutText ( WrT , "Circular with no header." ) 
            ; Wr . PutText ( WrT , Wr . EOL )
            ; LHeader := NIL 
            ; LLinesRef := StartLinesRef 
            ; EXIT 
            ELSE
              IF NOT WllMoveLeft ( LLinesRef ) THEN EXIT 
           (* ELSE loop. *)
              END (* IF *) 
            END (*( IF *)
            
          ELSE (* List header is to the left:*)
            LHeader := LLinesRef . LrLeftLink 
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . PutText ( WrT , LinesRefImage ( LHeader , LIndent ) ) 
          ; EXIT 
          END (* TYPECASE *) 
        END (* LOOP *)
        
      ELSE (* Starting with a list header. *)
        LHeader := LLinesRef
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , LinesRefImage ( LHeader , LIndent ) ) 
      ; IF NOT WllMoveRight ( LLinesRef ) THEN RETURN END (* IF *)
      END (* TYPECASE *)
      
    (* Write meat nodes left-to-right, starting with LLinesRef. *)
    ; LOOP 
        Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      ; Wr . PutText ( WrT , LinesRefImage ( LLinesRef , LIndent ) ) 
      ; TYPECASE LLinesRef . LrRightLink
        OF NULL
        => Wr . PutText ( WrT , "No header on right." )
        ; Wr . PutText ( WrT , Wr . EOL )
        ; EXIT 

        | LinesRefMeatTyp ( TLinesRefMeat )
        =>  IF LLinesRef = StartLinesRef
            THEN 
              Wr . PutText ( WrT , "Started here:" ) 
            ; Wr . PutText ( WrT , Wr . EOL )
            ; Wr . PutText ( WrT , LinesRefImage ( LLinesRef , LIndent ) ) 
            ; EXIT 
            END (* IF *)
          ; IF NOT WllMoveRight ( LLinesRef ) THEN EXIT
         (* ELSE loop. *) 
            END (* IF *) 
          
        ELSE (* Right node is a header. *) 
          IF LLinesRef # LHeader
          THEN
            Wr . PutText ( WrT , "Extra header." ) 
          ; Wr . PutText ( WrT , Wr . EOL )
          ; Wr . PutText ( WrT , LinesRefImage ( LLinesRef , LIndent ) ) 
          ; IF NOT WllMoveRight ( LLinesRef ) THEN EXIT
         (* ELSE loop. *) 
            END (* IF *) 
          ELSE
            Wr . PutText ( WrT , "Back to header:" ) 
          ; Wr . PutText ( WrT , Wr . EOL )
          ; Wr . PutText ( WrT , LinesRefImage ( LLinesRef , LIndent ) ) 
          ; EXIT 
          END (* IF *) 
        END (* TYPECASE *)
      END (* LOOP *) 
    END WriteLinesListWr 

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
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

(* EXPORTED: *) 
; PROCEDURE InitDefaults ( ImageRef : ImageRefTyp ) : ImageRefTyp 

  = BEGIN (* InitDefaults *) 
      (* Display . CreateEmptyLinesRefList ( ImageRef ) *) 
(* CHECK: What about IpMarkHeader? *) 
      RETURN ImageRef 
    END InitDefaults 

(* EXPORTED: *) 
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
    ; Ip . IpVisibleIn := WindowNoSetEmpty 
    ; Ip . IpWindowList := NIL 
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

(* EXPORTED: *) 
; PROCEDURE ItInitDefaults ( It : ImageTransientTyp ) 
  : ImageTransientTyp 

  = BEGIN (* ItInitDefaults *) 
      It . ItPers := NIL 
    ; It . ItLangIdRef := NIL 
    ; It . ItHistoryWrT := NIL  
    ; It . ItScannerIf := NIL 
    ; It . ItIsSaved := TRUE 
    ; RETURN It 
    END ItInitDefaults 

; BEGIN (* PaintHs *) 
  END PaintHs 
. 
