(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE TravUtil 

(* Common procedures used by several tree traversers. *) 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen 
; FROM Failures IMPORT Backout  
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LangUtil 
; FROM LangUtil IMPORT FsKindTyp , PredicateKindTyp 
; IMPORT LbeStd 
; IMPORT Marks  
; FROM Marks IMPORT MarkKindTyp 
; IMPORT MessageCodes 
; IMPORT ModHs 
; IMPORT Options 
; IMPORT SharedStrings 

; TYPE AFT = MessageCodes . T 

(* EXPORTED: *) 
; PROCEDURE IndentPos 
    ( Lang : LbeStd . LangTyp 
    ; EstIndentPos : LbeStd . LimitedCharNoTyp 
    ; IndentCode : LangUtil . IndentCodeTyp 
    ) 
    : LbeStd . LimitedCharNoTyp 
  (* The indent position to be used for floating items. *) 

  = BEGIN (* IndentPos *) 
      IF IndentCode = LangUtil . IndentCodeNull 
      THEN
        RETURN EstIndentPos 
      ELSE 
        RETURN 
          EstUtil . WidthSum 
            ( EstIndentPos , LangUtil . IndentAmt ( Lang , IndentCode ) ) 
      END (* IF *) 
    END IndentPos 

(* EXPORTED: *) 
; PROCEDURE PosForTok 
    ( Lang : LbeStd . LangTyp 
    ; FmtKind : LangUtil . FmtKindTyp 
    ; ModTextIsToLeftOnLine : BOOLEAN 
    ; CharPos : LbeStd . LimitedCharNoSignedTyp 
    ; TokIndentPos : LbeStd . LimitedCharNoTyp 
    ; IndentCode : LangUtil . IndentCodeTyp 
    ; PrevTok : LbeStd . TokTyp 
    ; Tok : LbeStd . TokTyp 
    ) 
    : LbeStd . LimitedCharNoTyp 

  = VAR LIndentPos : LbeStd . LimitedCharNoTyp 

  ; BEGIN (* PosForTok *) 
    (* Negative character positions are possible, from MergeTxt *) 
      IF CharPos < 0 
      THEN 
        CharPos := 0
      ; PrevTok := LbeStd . Tok__BegOfLine
      END 
    ; IF ModTextIsToLeftOnLine 
      THEN
        RETURN 
          EstUtil . WidthSum 
            ( CharPos 
            , ORD ( LangUtil . NeedsSep ( Lang , PrevTok , Tok ) ) 
              * LbeStd . SepWidth 
            ) 
      ELSE 
        LIndentPos := IndentPos ( Lang , TokIndentPos , IndentCode ) 
      ; IF LIndentPos > CharPos 
        THEN 
          RETURN LIndentPos 
        ELSIF LangUtil . NeedsSep ( Lang , PrevTok , Tok ) 
        THEN RETURN EstUtil . WidthSum ( CharPos , 1 ) 
        ELSE RETURN CharPos 
        END (* IF *) 
      END 
    END PosForTok 

(* Traversing Est children: *) 

; PROCEDURE BuildEstTravInfoForSingletonList  
    ( VAR EstTravInfo : EstTravInfoTyp 
    ; EstChildRef : LbeStd . EstRootTyp 
    ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ) 
  RAISES { Backout } 
  (* EstChildRef is the single list child of an Est list node omitted by the
     singleton-list optimization.  Construct a fake EstTravInfo for this list,
     setting EstChildRef as its only element.  
  *) 

  = BEGIN (* BuildEstTravInfoForSingletonList *) 
      EstTravInfo . EtiIsOptSingletonList := TRUE 
    ; EstTravInfo . EtiNodeRef := EstChildRef   
    ; TYPECASE EstChildRef 
      OF EstHs . EstRefTyp ( TEstRef ) 
      => EstTravInfo . EtiParentRef := TEstRef 
      ; EstTravInfo . EtiStringRef := NIL 

      | ModHs . EstDummyTyp (* Can this happen? *) 
      => EstTravInfo . EtiParentRef := NIL  
      ; EstTravInfo . EtiStringRef := NIL 

      | SharedStrings . T ( TStringRef ) 
      => EstTravInfo . EtiParentRef := NIL 
      ; EstTravInfo . EtiStringRef := TStringRef 
      ELSE 
      END (* TYPECASE *) 
    ; EstTravInfo . EtiChildCt := 1 
    ; EstTravInfo . EtiChildNo := 0 
    ; EstTravInfo . EtiChildRelNodeNo := 1 
    ; EstTravInfo . EtiAbsNodeNo := ParentAbsNodeNo 
    ; EstTravInfo . EtiChildLeafElem . LeChildRef := EstChildRef  
    ; EstTravInfo . EtiChildLeafElem . LeFmtNo := EstHs . FmtNoListEstChild  
    ; EstTravInfo . EtiChildFmtNo := EstHs . FmtNoListEstChild  
    ; EstTravInfo . EtiChildLeafElem . LeKindSet 
        := EstUtil . EstChildKindSet ( EstChildRef )   
           + EstHs . EstChildKindSetFirstOfGroup 
    END BuildEstTravInfoForSingletonList 

; PROCEDURE SetEstChildFmtNo 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp 
    ; GroupFwdMove : BOOLEAN := FALSE  
      (* ^TRUE if caller knows it has made a forward move and has not 
         skipped a child with FirstOfGroup.  This implies that, if 
         we are not at a FirstOfGroup now, then the old value of 
         EtiChildFmtNo is correct. *) 
    ) 
  RAISES { Backout } 
  (* PRE: EstTravInfo is initialized.  
     PRE: EtiChildNo of EtiParentRef exists and EtiChildLeafElem is set. 
     PRE: NOT EtiIsOptSingletonList 
     Set EtiChildFmtNo appropriately. 
  *) 

  = VAR LFirstOfGroupChildNo : LbeStd . EstChildNoTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LFirstOfGroupLeafElem : EstHs . LeafElemTyp 

  ; BEGIN (* SetEstChildFmtNo *)
      IF EstTravInfo . EtiChildNo >= EstTravInfo . EtiChildCt 
         OR EstTravInfo . EtiChildNo < 0 
      THEN 
        EstTravInfo . EtiChildFmtNo := EstHs . FmtNoNull 
      ELSIF EstHs . EstChildKindFirstOfGroup 
            IN EstTravInfo . EtiChildLeafElem . LeKindSet 
      THEN 
        EstTravInfo . EtiChildFmtNo 
          := EstTravInfo . EtiChildLeafElem . LeFmtNo 
      ; Assert 
          ( EstTravInfo . EtiChildFmtNo <= EstHs . FmtNoMax 
          , AFT . A_TravUtil_SetEstChildFmtNo_BadFormatNumberHere 
          ) 
      ELSIF NOT GroupFwdMove 
      THEN 
        EstUtil . PrevInKindSet 
          ( EstTravInfo . EtiParentRef 
          , EstTravInfo . EtiChildNo 
          , EstHs . EstChildKindSetFirstOfGroup 
          , (* VAR *) LFirstOfGroupChildNo 
          , (* VAR *) LNodeNo 
          , (* VAR *) LFirstOfGroupLeafElem 
          ) 
      ; Assert 
          ( 0 <= LFirstOfGroupChildNo 
          , AFT . A_SetEstChildFmtNoNoFirstOfGroup 
          ) 
      ; EstTravInfo . EtiChildFmtNo := LFirstOfGroupLeafElem . LeFmtNo 
      ; Assert 
          ( EstTravInfo . EtiChildFmtNo <= EstHs . FmtNoMax 
          , AFT . A_TravUtil_SetEstChildFmtNo_BadFormatNumberPrev 
          ) 
      END (* IF *) 
    END SetEstChildFmtNo 

(* EXPORTED: *) 
; PROCEDURE InitEstTravInfo 
    ( VAR EstTravInfo : EstTravInfoTyp 
    ; EstNodeRef : LbeStd . EstRootTyp 
    ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ; IsOptSingletonList : BOOLEAN := FALSE 
    ) 
  RAISES { Backout } 
  (* Sets EtiNodeRef , EtiParentRef , EtiStringRef , EtiChildCt.
     and EtiIsOptSingletonList. *) 

  = BEGIN (* InitEstTravInfo *) 
      EstTravInfo . EtiIsOptSingletonList := IsOptSingletonList  
    ; EstTravInfo . EtiNodeRef := EstNodeRef 
    ; EstTravInfo . EtiAbsNodeNo := ParentAbsNodeNo 
    ; TYPECASE EstNodeRef 
      OF NULL 
      => EstTravInfo . EtiParentRef := NIL 
      ; EstTravInfo . EtiChildCt := 0 
      ; EstTravInfo . EtiStringRef := NIL 
      | ModHs . EstDummyTyp (* Can this happen? *) 
      => EstTravInfo . EtiParentRef := NIL  
      ; EstTravInfo . EtiChildCt := 0 
      ; EstTravInfo . EtiStringRef := NIL 
      | EstHs . EstRefTyp ( TEstRef ) 
      => EstTravInfo . EtiParentRef := TEstRef 
      ; EstTravInfo . EtiChildCt := TEstRef . KTreeChildCt ( ) 
      ; EstTravInfo . EtiStringRef := NIL 
      | SharedStrings . T ( TStringRef ) 
      => EstTravInfo . EtiParentRef := NIL 
      ; EstTravInfo . EtiChildCt := 0 
      ; EstTravInfo . EtiStringRef := TStringRef 
      | ModHs . ModRefTyp (* But not a ModTok, caught by EstRefTyp, above. *) 
      => CantHappen ( AFT . A_InitEstTravInfoMod ) 
         (* We never descend to a mod, but always process it from above. *) 
      ELSE 
        CantHappen ( AFT . A_InitEstTravInfoBadType ) 
      END (* TYPECASE *) 
    END InitEstTravInfo 

(* EXPORTED: *) 
; PROCEDURE InitEstTravInfoFwd 
    ( VAR EstTravInfo : EstTravInfoTyp 
    ; EstNodeRef : LbeStd . EstRootTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ) 
  RAISES { Backout } 
  (* For an element of an optimized singleton list, trump up a suitable
     EstTravInfo.  Otherwise, set it up for forward traversal of the
     children of EstNodeRef with ParentAbsNodeNo.
  *) 

  = BEGIN (* InitEstTravInfoFwd *) 
      IF EstHs . EstChildKindOptSingletonList IN KindSet 
      THEN 
        BuildEstTravInfoForSingletonList 
          ( (* VAR *) EstTravInfo , EstNodeRef , ParentAbsNodeNo ) 
      ELSE 
        InitEstTravInfo 
          ( (* VAR *) EstTravInfo , EstNodeRef , ParentAbsNodeNo ) 
      ; GetLMEstChild ( (* IN OUT *) EstTravInfo ) 
      END (* IF *) 
    END InitEstTravInfoFwd  

(* EXPORTED: *) 
; PROCEDURE InitEstTravInfoBwd 
    ( VAR EstTravInfo : EstTravInfoTyp 
    ; EstNodeRef : LbeStd . EstRootTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ) 
  RAISES { Backout } 
  (* For an element of an optimized singleton list, trump up a suitable
     EstTravInfo.  Otherwise, set it up for backward traversal of the
     children of EstNodeRef with ParentAbsNodeNo.
  *) 

  = BEGIN (* InitEstTravInfoBwd *) 
      IF EstHs . EstChildKindOptSingletonList IN KindSet 
      THEN 
        BuildEstTravInfoForSingletonList 
          ( (* VAR *) EstTravInfo , EstNodeRef , ParentAbsNodeNo ) 
      ELSE 
        InitEstTravInfo 
          ( (* VAR *) EstTravInfo , EstNodeRef , ParentAbsNodeNo ) 
      ; GetRMEstChild ( (* IN OUT *) EstTravInfo ) 
      END (* IF *) 
    END InitEstTravInfoBwd  

(* EXPORTED: *) 
; PROCEDURE InitToChildContainingNodeNo 
    ( VAR EstTravInfo : EstTravInfoTyp 
    ; EstNodeRef : LbeStd . EstRootTyp 
    ; EstRelNodeNo : LbeStd . EstNodeNoTyp 
      (* ^Node number relative to EstNodeRef. *) 
    ; KindSet : EstHs . EstChildKindSetTyp := EstHs . EstChildKindSetEmpty  
    ; ParentAbsNodeNo : LbeStd . EstNodeNoTyp := 0 
    ) 
  RAISES { Backout }  

  = BEGIN 
      IF EstHs . EstChildKindOptSingletonList IN KindSet 
      THEN 
        BuildEstTravInfoForSingletonList 
          ( (* VAR *) EstTravInfo , EstNodeRef , ParentAbsNodeNo )
      ; Assert
          ( EstRelNodeNo = 1 (* In the fictitious unoptimized list. *) 
          , AFT . A_InitToChildContainingNodeNo_nonzero_node_no
          )
      ELSE 
        InitEstTravInfo 
          ( (* VAR *) EstTravInfo , EstNodeRef , ParentAbsNodeNo ) 
      ; SetToChildContainingNodeNo ( (* IN OUT *) EstTravInfo , EstRelNodeNo ) 
      END (* IF *) 
    END InitToChildContainingNodeNo

(* EXPORTED: *) 
; PROCEDURE SetToChildWGreaterFmtNo 
    ( VAR EstTravInfo : EstTravInfoTyp ; FmtNo : EstHs . FmtNoTyp ) 
  RAISES { Backout }
  (* Leftmost Est child whose format no is > FmtNo. *)
  (* PRE: EstTravInfo is for a nonlist Est node. *) 

  = BEGIN
      GetLMEstChild ( EstTravInfo ) 
    ; LOOP
        IF EstTravInfo . EtiChildNo >= EstTravInfo . EtiChildCt
        THEN (* Off right end. *)
          RETURN 
        ELSIF EstTravInfo . EtiChildLeafElem . LeFmtNo > FmtNo
        THEN (* Found it. *) RETURN
        ELSE IncEstChild ( EstTravInfo ) 
        END (* IF *) 
      END (* LOOP *) 
    END SetToChildWGreaterFmtNo 

(* EXPORTED: *) 
; PROCEDURE GetLMEstChild ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp )
  RAISES { Backout }  
  (* PRE: EstTravInfo is initialized. *) 
  (* Set EtiChildCt from EtiParentRef and EtiIsOptSingletonList.  
     Set EtiChildNo to the leftmost child number (always zero).  
     If this child exists, set EtiChildLeafElem and EtiChildFmtNo accordingly. 
  *) 

  = BEGIN (* GetLMEstChild *) 
      IF EstTravInfo . EtiIsOptSingletonList 
      THEN (* Singleton-optimized list. Reset to the only child. *) 
        Assert
          ( EstTravInfo . EtiChildCt = 1 
          , AFT . A_TravUtil_GetLMEstChild_BadSingletonList 
          ) 
      ; EstTravInfo . EtiChildNo := 0
      ; EstTravInfo . EtiChildRelNodeNo := 1
      (* Remaining fields will have been set initially and unmolested since. *) 
      ELSE (* Not singleton-optimized list. *)  
        TYPECASE EstTravInfo . EtiParentRef 
        OF NULL   
        => (* Treat as off the right end: Defensive: *) 
          EstTravInfo . EtiChildNo := 0
        ; EstTravInfo . EtiChildRelNodeNo := 0
        ; EstTravInfo . EtiChildLeafElem := EstHs . LeafElemNull 
        ; EstTravInfo . EtiChildFmtNo := EstHs . FmtNoNull 

        | EstHs . EstRefTyp ( TEstParentRef ) 
        => EstTravInfo . EtiChildNo := 0 
        ; IF EstTravInfo . EtiChildNo < EstTravInfo . EtiChildCt 
          THEN 
            EstUtil . GetIthChild 
              ( TEstParentRef 
              , 0 
              , (* VAR *) EstTravInfo . EtiChildRelNodeNo 
              , (* VAR *) EstTravInfo . EtiChildLeafElem 
              ) 
          ; Assert 
              ( EstHs . EstChildKindFirstOfGroup 
                IN EstTravInfo . EtiChildLeafElem . LeKindSet 
              , AFT . A_GetLMEstChildNotFirstOfGroup 
              ) 
          ; EstTravInfo . EtiChildFmtNo 
              := EstTravInfo . EtiChildLeafElem . LeFmtNo 
          ; Assert 
              ( EstTravInfo . EtiChildFmtNo <= EstHs . FmtNoMax 
              , AFT . A_TravUtil_GetLMEstChild_BadFormatNumber 
              ) 

          ELSE (* Treat as off the right end: Defensive: *) 
            EstTravInfo . EtiChildRelNodeNo 
              := EstUtil . EstNodeCt ( TEstParentRef ) 
          ; EstTravInfo . EtiChildLeafElem := EstHs . LeafElemNull 
          ; EstTravInfo . EtiChildFmtNo := EstHs . FmtNoNull 
          END (* IF *) 
        END (* TYPECASE *) 
      END (* IF *) 
    END GetLMEstChild 

(* EXPORTED: *) 
; PROCEDURE GetRMEstChild ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp )
  RAISES { Backout }  
  (* PRE: EstTravInfo is initialized. *) 
  (* Set EtiChildCt from EtiParentRef and EtiIsOptSingletonList.  
     Set EtiChildNo to the rightmost child number.  
     If this child exists, set EtiChildLeafElem and EtiChildFmtNo accordingly. 
  *) 

  = VAR LFirstOfGroupChildNo : LbeStd . EstChildNoTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LFirstOfGroupLeafElem : EstHs . LeafElemTyp 

  ; BEGIN (* GetRMEstChild *) 
      IF EstTravInfo . EtiIsOptSingletonList  
      THEN (* Singleton-optimized list. Reset to the only child. *) 
        Assert
          ( EstTravInfo . EtiChildCt = 1 
          , AFT . A_TravUtil_GetRMEstChild_BadSingletonList 
          ) 
      ; EstTravInfo . EtiChildNo := 0
      ; EstTravInfo . EtiChildRelNodeNo := 1
      (* Remaining fields will have been set initially and unmolested since. *) 
      ELSE 
        TYPECASE EstTravInfo . EtiParentRef 
        OF NULL  
        => (* Treat as off the left end: Defensive: *) 
          EstTravInfo . EtiChildNo := - 1 
        ; EstTravInfo . EtiChildRelNodeNo := 0 
        ; EstTravInfo . EtiChildLeafElem := EstHs . LeafElemNull 
        ; EstTravInfo . EtiChildFmtNo := EstHs . FmtNoNull 

        | EstHs . EstRefTyp ( TEstParentRef ) 
        => EstTravInfo . EtiChildNo := EstTravInfo . EtiChildCt - 1 
        ; IF EstTravInfo . EtiChildNo >= 0 
          THEN 
            EstUtil . GetIthChild 
              ( TEstParentRef 
              , EstTravInfo . EtiChildNo 
              , (* VAR *) EstTravInfo . EtiChildRelNodeNo 
              , (* VAR *) EstTravInfo . EtiChildLeafElem 
              ) 
          ; EstUtil . PrevInKindSet 
              ( TEstParentRef 
              , EstTravInfo . EtiChildNo 
              , EstHs . EstChildKindSetFirstOfGroup 
              , (* VAR *) LFirstOfGroupChildNo 
              , (* VAR *) LNodeNo 
              , (* VAR *) LFirstOfGroupLeafElem 
              ) 
          ; Assert 
              ( 0 <= LFirstOfGroupChildNo 
              , AFT . A_GetRMEstChildNoFirstOfGroup 
              ) 
          ; EstTravInfo . EtiChildFmtNo := LFirstOfGroupLeafElem . LeFmtNo 
          ; Assert 
              ( EstTravInfo . EtiChildFmtNo <= EstHs . FmtNoMax 
              , AFT . A_TravUtil_GetRMEstChild_BadFormatNumber 
              ) 

          ELSE (* Treat as off the left end: Defensive: *) 
            EstTravInfo . EtiChildNo := - 1 
          ; EstTravInfo . EtiChildRelNodeNo := 0 
          ; EstTravInfo . EtiChildLeafElem := EstHs . LeafElemNull 
          ; EstTravInfo . EtiChildFmtNo := EstHs . FmtNoNull 
          END (* IF *) 
        END (* TYPECASE *) 
      END (* IF *) 
    END GetRMEstChild 

(* EXPORTED: *) 
; PROCEDURE SetToIthChild 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp 
    ; I : LbeStd . EstChildNoTyp 
    ) 
  RAISES { Backout } 
  (* PRE: EstTravInfo is initialized. *) 
  (* Move to Ith child. Set ChildNo, ChildRelNodeNo, ChildLeafElem, 
     and ChildFmtNo. 
  *) 

  = BEGIN (* SetToIthChild *) 
      IF I < 0  
      THEN (* Treat as off the left end: Defensive: *) 
        EstTravInfo . EtiChildNo := - 1  
      ; EstTravInfo . EtiChildRelNodeNo := 0 
      (* Leave other fields alone in case it's a singleton-optimized list
         and is later reset to child 0. *) 
      ELSE 
        IF EstTravInfo . EtiIsOptSingletonList  
        THEN (* Singleton-optimized list. *)  
          Assert
            ( EstTravInfo . EtiChildCt = 1 
            , AFT . A_TravUtil_SetToIthChild_BadSingletonList 
            ) 
        ; IF I = 1 
          THEN (* The caller wants the one and only list element. *) 
            EstTravInfo . EtiChildNo := 0
          ; EstTravInfo . EtiChildRelNodeNo := 1  
          (* Remaining fields will have been set initially and unmolested since. *) 
          ELSE (* Treat as off the right end: Defensive: *) 
            EstTravInfo . EtiChildNo := 1 
          ; EstTravInfo . EtiChildRelNodeNo 
              := 1 
                 + EstUtil . EstNodeCt 
                     ( EstTravInfo . EtiChildLeafElem . LeChildRef )  
                 + ORD ( EstHs . EstChildKindOptSingletonList 
                         IN EstTravInfo . EtiChildLeafElem . LeKindSet 
                       ) 
          (* Leave other fields alone in case it is later reset to child 0. *) 
          END (* IF *) 
        ELSE (* Not an optimized-away singleton list. *) 
          IF I >= EstTravInfo . EtiChildCt   
          THEN (* Treat as off the right end: Defensive: *) 
            EstTravInfo . EtiChildNo := EstTravInfo . EtiChildCt 
          ; EstTravInfo . EtiChildRelNodeNo 
              := 1 + EstUtil . EstNodeCt ( EstTravInfo . EtiParentRef )   
          ELSE 
            EstUtil . GetIthChild 
              ( EstTravInfo . EtiParentRef 
              , I 
              , (* VAR *) EstTravInfo . EtiChildRelNodeNo 
              , (* VAR *) EstTravInfo . EtiChildLeafElem 
              ) 
          ; EstTravInfo . EtiChildNo := I 
          ; SetEstChildFmtNo ( EstTravInfo ) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END SetToIthChild 

; PROCEDURE SetToChildContainingNodeNo 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp 
    ; EstRelNodeNo : LbeStd . EstNodeNoTyp 
      (* ^Node number relative to the parent of EstTravInfo. *) 
    )
  RAISES { Backout }  
  (* PRE: EstTravInfo is initialized. *) 

  = BEGIN (* SetToChildContainingNodeNo *) 
      IF EstTravInfo . EtiIsOptSingletonList  
      THEN (* Singleton-optimized list. *)  
        Assert
          ( EstTravInfo . EtiChildCt = 1 
          , AFT . A_TravUtil_SetToChildContainingNodeNo_BadSingletonList 
          ) 
      ; IF EstRelNodeNo < 0 
        THEN (* Treat as off the left end: Defensive: *) 
          EstTravInfo . EtiChildNo := - 1  
        ; EstTravInfo . EtiChildRelNodeNo := 0 
        (* Leave other fields alone in case it is later reset to child 0. *) 
      ; ELSIF EstRelNodeNo 
              - 1 (* Discount the optimized-away list node. *) 
              < EstUtil . EstNodeCt 
                  ( EstTravInfo . EtiChildLeafElem . LeChildRef )  
        THEN (* The caller wants the one and only list element. *) 
          EstTravInfo . EtiChildNo := 0
        ; EstTravInfo . EtiChildRelNodeNo := 1
        (* Remaining fields will have been set initially and unmolested since. *) 
        ELSE (* Treat as off the right end: Defensive: *) 
          EstTravInfo . EtiChildNo := 1 
        ; EstTravInfo . EtiChildRelNodeNo 
            := 1 
               + EstUtil . EstNodeCt 
                   ( EstTravInfo . EtiChildLeafElem . LeChildRef )  
               + ORD ( EstHs . EstChildKindOptSingletonList 
                       IN EstTravInfo . EtiChildLeafElem . LeKindSet 
                     ) 
        (* Leave other fields alone in case it is later reset to child 0. *) 
        END (* IF *) 
      ELSE 
        EstUtil . GetEstChildContainingRelNodeNo 
          ( EstTravInfo . EtiParentRef 
          , EstRelNodeNo 
          , (* VAR *) EstTravInfo . EtiChildNo 
          , (* VAR *) EstTravInfo . EtiChildRelNodeNo 
          , (* VAR *) EstTravInfo . EtiChildLeafElem 
          ) 
      ; SetEstChildFmtNo ( EstTravInfo ) 
      END (* IF *) 
    END SetToChildContainingNodeNo 

(* EXPORTED: *) 
; PROCEDURE IncEstChild ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp ) 
  RAISES { Backout } 
  (* PRE: EstTravInfo is initialized. *) 
  (* Increase ChildNo. If there is another child, set 
     ChildLeafElem and ChildFmtNo accordingly. 
  *) 

  = BEGIN (* IncEstChild *) 
      IF EstTravInfo . EtiIsOptSingletonList 
      THEN (* Singleton-optimized list. This puts us off the right end. *)
        Assert
          ( EstTravInfo . EtiChildCt = 1 
          , AFT . A_TravUtil_IncEstChild_BadSingletonList 
          ) 
      ; EstTravInfo . EtiChildNo := 1 
      ; EstTravInfo . EtiChildRelNodeNo 
          := 1 
             + EstUtil . EstNodeCt 
                 ( EstTravInfo . EtiChildLeafElem . LeChildRef )
             + ORD ( EstHs . EstChildKindOptSingletonList 
                     IN EstTravInfo . EtiChildLeafElem . LeKindSet 
                   ) 
      (* Leave other fields alone in case of reset to child 0. *) 
      ELSIF EstTravInfo . EtiChildNo < EstTravInfo . EtiChildCt 
      THEN 
        INC 
          ( EstTravInfo . EtiChildRelNodeNo 
          , EstUtil . EstNodeCt ( EstTravInfo . EtiChildLeafElem . LeChildRef )
            + ORD ( EstHs . EstChildKindOptSingletonList 
                    IN EstTravInfo . EtiChildLeafElem . LeKindSet 
                  ) 
          ) 
      ; INC ( EstTravInfo . EtiChildNo ) 
      ; IF EstTravInfo . EtiChildNo < EstTravInfo . EtiChildCt 
        THEN (* There is another child. *)  
          EstUtil . GetIthChild 
            ( EstTravInfo . EtiParentRef 
            , EstTravInfo . EtiChildNo 
            , (* VAR *) EstTravInfo . EtiChildRelNodeNo 
            , (* VAR *) EstTravInfo . EtiChildLeafElem 
            ) 
        ; IF EstHs . EstChildKindFirstOfGroup 
             IN EstTravInfo . EtiChildLeafElem . LeKindSet 
          THEN 
            EstTravInfo . EtiChildFmtNo 
              := EstTravInfo . EtiChildLeafElem . LeFmtNo 
          END (* IF *) 
        ; Assert 
            ( EstTravInfo . EtiChildFmtNo <= EstHs . FmtNoMax 
            , AFT . A_TravUtil_IncEstChild_BadFormatNumber 
            ) 
        ELSE (* We just ran off the right end. *) 
          IF EstTravInfo . EtiParentRef # NIL 
          THEN 
            Assert 
              ( EstTravInfo . EtiChildRelNodeNo 
                = EstUtil . EstNodeCt ( EstTravInfo . EtiParentRef ) 
              , AFT . A_IncEstChildBadChildRelNodeNoOffEnd 
              ) 
          END (* IF *) 
        (* Defensive: *) 
        ; EstTravInfo . EtiChildLeafElem := EstHs . LeafElemNull 
        ; EstTravInfo . EtiChildFmtNo := EstHs . FmtNoNull 
        END (* IF *) 
      END (* IF *) 
    END IncEstChild 

(* EXPORTED: *) 
; PROCEDURE DecEstChild ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp ) 
  RAISES { Backout } 
  (* PRE: EstTravInfo is initialized. *) 
  (* Decrease ChildNo. If there is an earlier child, set 
     ChildLeafElem and ChildFmtNo accordingly. 
  *) 

  = VAR LNeedNewFmtNo : BOOLEAN 
  ; VAR LFirstOfGroupChildNo : LbeStd . EstChildNoTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LFirstOfGroupLeafElem : EstHs . LeafElemTyp 

  ; BEGIN (* DecEstChild *) 
      IF EstTravInfo . EtiIsOptSingletonList 
      THEN (* Singleton-optimized list. This puts us off the left end. *) 
        Assert
          ( EstTravInfo . EtiChildCt = 1 
          , AFT . A_TravUtil_DecEstChild_BadSingletonList 
          ) 
      ; EstTravInfo . EtiChildNo := - 1  
      ; EstTravInfo . EtiChildRelNodeNo := 0 
      (* Leave other fields alone in case of reset to child 0. *) 
      ELSIF 0 <= EstTravInfo . EtiChildNo 
      THEN 
        LNeedNewFmtNo 
          := EstTravInfo . EtiChildNo >= EstTravInfo . EtiChildCt 
             OR EstHs . EstChildKindFirstOfGroup 
                IN EstTravInfo . EtiChildLeafElem . LeKindSet 
      ; DEC ( EstTravInfo . EtiChildNo ) 
      ; IF 0 <= EstTravInfo . EtiChildNo 
        THEN (* There is another child. *)  
          EstUtil . GetIthChild 
            ( EstTravInfo . EtiParentRef 
            , EstTravInfo . EtiChildNo 
            , (* VAR *) EstTravInfo . EtiChildRelNodeNo 
            , (* VAR *) EstTravInfo . EtiChildLeafElem 
            ) 
        ; IF LNeedNewFmtNo 
          THEN 
            EstUtil . PrevInKindSet 
              ( EstTravInfo . EtiParentRef 
              , EstTravInfo . EtiChildNo 
              , EstHs . EstChildKindSetFirstOfGroup 
              , (* VAR *) LFirstOfGroupChildNo 
              , (* VAR *) LNodeNo 
              , (* VAR *) LFirstOfGroupLeafElem 
              ) 
          ; Assert 
              ( 0 <= LFirstOfGroupChildNo 
              , AFT . A_DecEstChildNotFirstOfGroup 
              ) 
          ; EstTravInfo . EtiChildFmtNo := LFirstOfGroupLeafElem . LeFmtNo 
          END (* IF *) 
        ; Assert 
            ( EstTravInfo . EtiChildFmtNo <= EstHs . FmtNoMax 
            , AFT . A_TravUtil_DecEstChild_BadFormatNumber 
            ) 
        ELSE (* Defensive: *) 
          EstTravInfo . EtiChildRelNodeNo := 0 
        ; EstTravInfo . EtiChildLeafElem := EstHs . LeafElemNull 
        ; EstTravInfo . EtiChildFmtNo := EstHs . FmtNoNull 
        END (* IF *) 
      END (* IF *) 
    END DecEstChild 

(* EXPORTED: *) 
; PROCEDURE SetToNextInKindSet 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp 
    ; StartChildNo : LbeStd . EstChildNoTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ) 
  RAISES { Backout } 
  (* PRE: EstTravInfo is initialized. *) 

  = BEGIN (* SetToNextInKindSet *) 
      IF EstTravInfo . EtiIsOptSingletonList  
      THEN (* Singleton-optimized list. *) 
        Assert
          ( EstTravInfo . EtiChildCt = 1 
          , AFT . A_TravUtil_SetToNextInKindSet_BadSingletonList 
          ) 
      ; IF StartChildNo <= EstTravInfo . EtiChildNo
           AND EstTravInfo . EtiChildNo <= 0  
           AND KindSet * EstTravInfo . EtiChildLeafElem . LeKindSet 
               # EstHs . EstChildKindSetEmpty 
        THEN (* We are already where wanted. *) 
        ELSE (* We won't find it.  Treat as off the right end. *) 
          EstTravInfo . EtiChildNo := 1 
        ; EstTravInfo . EtiChildRelNodeNo 
            := EstUtil . EstNodeCt 
                 ( EstTravInfo . EtiChildLeafElem . LeChildRef ) 
               + ORD ( EstHs . EstChildKindOptSingletonList 
                       IN EstTravInfo . EtiChildLeafElem . LeKindSet 
                     ) 
        (* Leave other fields alone in case it is later reset to child 0. *) 
        END (* IF *) 
      ELSE 
        EstUtil . NextInKindSet 
          ( EstTravInfo . EtiParentRef 
          , StartChildNo 
          , KindSet 
          , (* VAR *) EstTravInfo . EtiChildNo 
          , (* VAR *) EstTravInfo . EtiChildRelNodeNo 
          , (* VAR *) EstTravInfo . EtiChildLeafElem 
          ) 
      ; SetEstChildFmtNo 
          ( EstTravInfo 
          , GroupFwdMove := EstHs . EstChildKindFirstOfGroup IN KindSet 
          ) 
      END (* IF *) 
    END SetToNextInKindSet 

(* EXPORTED: *) 
; PROCEDURE SetToPrevInKindSet 
    ( VAR (* IN OUT *) EstTravInfo : EstTravInfoTyp 
    ; StartChildNo : LbeStd . EstChildNoTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ) 
  RAISES { Backout } 
  (* PRE: EstTravInfo is initialized. *) 

  = BEGIN (* SetToPrevInKindSet *) 
      IF EstTravInfo . EtiIsOptSingletonList 
      THEN (* Singleton-optimized list. *) 
        Assert
          ( EstTravInfo . EtiChildCt = 1 
          , AFT . A_TravUtil_SetToPrevInKindSet_BadSingletonList 
          ) 
      ; IF StartChildNo >= EstTravInfo . EtiChildNo 
           AND EstTravInfo . EtiChildNo >= 0  
           AND KindSet * EstTravInfo . EtiChildLeafElem . LeKindSet 
               # EstHs . EstChildKindSetEmpty 
        THEN (* We are already where wanted. *) 
        ELSE (* We won't find it.  Treat as off the left end. *) 
          EstTravInfo . EtiChildNo := - 1  
        ; EstTravInfo . EtiChildRelNodeNo := 0 
        (* Leave other fields alone in case it is later reset to child 0. *) 
        END (* IF *) 
      ELSE 
        EstUtil . PrevInKindSet 
          ( EstTravInfo . EtiParentRef 
          , StartChildNo 
          , KindSet 
          , (* VAR *) EstTravInfo . EtiChildNo 
          , (* VAR *) EstTravInfo . EtiChildRelNodeNo 
          , (* VAR *) EstTravInfo . EtiChildLeafElem 
          ) 
      ; SetEstChildFmtNo ( EstTravInfo ) 
      END (* IF *) 
    END SetToPrevInKindSet 

(* Utility: *) 

(* EXPORTED: *) 
; PROCEDURE ChildIndentPositions 
    ( Lang : LbeStd . LangTyp  
    ; FsEstChildNodeRef : LangUtil . FsNodeRefTyp 
      (* ^Must have FsKindEstChildOfFixed or FsKindEstChildOfList. *) 
    ; EstIndentPos1 : LbeStd . LimitedCharNoTyp 
    ; EstIndentPosN : LbeStd . LimitedCharNoTyp 
    ; VAR ChildIndentPos1 : LbeStd . LimitedCharNoTyp 
    ; VAR ChildIndentPosN : LbeStd . LimitedCharNoTyp 
    ; IsFirstLine : BOOLEAN := FALSE 
    ) 
  RAISES { Backout } 

  = BEGIN    
      IF FsEstChildNodeRef = NIL 
      THEN
        ChildIndentPos1 
          := EstUtil . WidthSum 
               ( EstIndentPos1 
               , LangUtil . IndentAmt 
                   ( Lang , LangUtil . IndentCodeInitial ) 
               ) 
      ; ChildIndentPosN 
          := EstUtil . WidthSum 
               ( EstIndentPosN 
               , LangUtil . IndentAmt 
                   ( Lang , LangUtil . IndentCodeInitial ) 
               ) 
      ELSE 
        CASE FsEstChildNodeRef . FsKind 
        OF FsKindTyp . FsKindEstChildOfFixed 
        => IF IsFirstLine 
           THEN
             ChildIndentPos1 
               := EstUtil . WidthSum 
                   ( EstIndentPos1 
                   , LangUtil . IndentAmt 
                       ( Lang , FsEstChildNodeRef . FsIndentCode ) 
                   ) 
           ELSE
             ChildIndentPos1 
               := EstUtil . WidthSum 
                   ( EstIndentPosN 
                   , LangUtil . IndentAmt 
                       ( Lang , FsEstChildNodeRef . FsIndentCode ) 
                   ) 
           END (* IF *)     

        | FsKindTyp . FsKindEstChildOfList 
        => IF IsFirstLine 
           THEN
             ChildIndentPos1 
               := EstUtil . WidthSum 
                   ( EstIndentPos1 
                   , LangUtil . IndentAmt 
                       ( Lang , FsEstChildNodeRef . FsFirstListElemIndentCode )
                   ) 
           ELSE
             ChildIndentPos1 
               := EstUtil . WidthSum 
                   ( EstIndentPosN 
                   , LangUtil . IndentAmt 
                       ( Lang , FsEstChildNodeRef . FsIndentCode ) 
                   ) 
           END (* IF *)     

        ELSE 
          CantHappen 
            ( AFT . A_ChildIndentPositions_BadFsKind ) 
        END (* CASE *)
      ; ChildIndentPosN 
          := EstUtil . WidthSum 
              ( EstIndentPosN 
              , LangUtil . IndentAmt 
                  ( Lang , FsEstChildNodeRef . FsChildIndentCode ) 
              ) 
(* FIXME: These saturations can happen legitimately: *) 
      ; Assert 
          ( ChildIndentPos1 # LbeStd . LimitedCharNoInfinity 
          , AFT . A_ChildIndentPositions_IndentPos1IsInfinite 
          ) 
      ; Assert 
          ( ChildIndentPosN # LbeStd . LimitedCharNoInfinity 
          , AFT . A_ChildIndentPositions_IndentPosNIsInfinite 
          ) 
      END (* IF *) 
    END ChildIndentPositions 

(* EXPORTED: *) 
; PROCEDURE IsInFirstLine  
    ( FsNodeRef : LangUtil . FsNodeRefTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ; Bwd : BOOLEAN 
    )  
  : BOOLEAN 
  RAISES { Backout } 
  (* "First line" in the layout sense that does not consider New lines 
     inserted by modifiers.
  *) 

  = VAR LIsInFirstLine : BOOLEAN 
  ; VAR LEstChildNo : LbeStd . EstChildNoTyp 
  ; VAR LEstChildCtNeeded : [ 1 .. 2 ] 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildLeafElem : EstHs . LeafElemTyp 

  ; BEGIN 
      IF FsNodeRef . FsIsInsideList  
      THEN
        IF FsNodeRef . FsIsInFirstLine 
        THEN (* We can still be in a subsequent line, if another Est list 
                child exists to the left.  Plan how to check this. 
                Set variables such that, starting at LEstChildNo and
                searching leftward, we need LEstChildCtNeeded Est children
                in order for the Fs child denoted by FsNodeRef NOT to be in 
                the first line of the complete construct in EstTravInfo. 
             *) 
          IF EstTravInfo . EtiIsOptSingletonList 
          THEN 
            LIsInFirstLine := TRUE 
          ELSE 
            IF FsNodeRef . FsKind 
               = FsKindTyp . FsKindEstChildOfList 
            THEN (* Fs node is for an Est list child. *) 
              IF EstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
              THEN (* Est child is the EstChildOfList or a mod thereon. *)  
                IF EstHs . EstChildKindTrailingMod 
                   IN EstTravInfo . EtiChildLeafElem . LeKindSet 
                THEN
                  LEstChildNo := EstTravInfo . EtiChildNo - 1 
                ; LEstChildCtNeeded := 2               
                ELSE
                  LEstChildNo := EstTravInfo . EtiChildNo - 1 
                ; LEstChildCtNeeded := 1               
                END (* IF *) 
              ELSE (* Est child is a mod on an insertion token or line break
                      attached to something other than the list Est child. 
                   *)
                LEstChildNo := EstTravInfo . EtiChildNo - 1 
              ; IF Bwd 
                THEN
                  LEstChildCtNeeded := 1               
                ELSE
                  LEstChildCtNeeded := 2               
                END (* IF *) 
              END (* IF *) 
            ELSE (* Fs node is for an insertion token or line break. *) 
              IF Bwd 
              THEN
                LEstChildNo := EstTravInfo . EtiChildNo  
              ELSE
                LEstChildNo := EstTravInfo . EtiChildNo - 1 
              END (* IF *) 
            ; IF FsNodeRef . FsIsRightOfEstListChild  
              THEN
                LEstChildCtNeeded := 2               
              ELSE
                LEstChildCtNeeded := 1               
              END (* IF *) 
            END (* IF *) 
          ; CASE LEstChildCtNeeded        
            OF 1 
            => EstUtil . PrevInKindSet 
                ( EstTravInfo . EtiParentRef 
                , LEstChildNo  
                , EstHs . EstChildKindSetEstChild  
                , (* VAR *) LEstChildNo 
                , (* VAR *) LNodeNo 
                , (* VAR *) LChildLeafElem 
                ) 
            ; LIsInFirstLine := LEstChildNo < 0 

            | 2 
            => EstUtil . PrevInKindSet 
                ( EstTravInfo . EtiParentRef 
                , LEstChildNo  
                , EstHs . EstChildKindSetEstChild  
                , (* VAR *) LEstChildNo 
                , (* VAR *) LNodeNo 
                , (* VAR *) LChildLeafElem 
                ) 
            ; IF LEstChildNo < 0 
              THEN 
                LIsInFirstLine := TRUE 
              ELSE 
                EstUtil . PrevInKindSet 
                  ( EstTravInfo . EtiParentRef 
                  , LEstChildNo - 1 
                  , EstHs . EstChildKindSetEstChild  
                  , (* VAR *) LEstChildNo 
                  , (* VAR *) LNodeNo 
                  , (* VAR *) LChildLeafElem 
                  ) 
              ; LIsInFirstLine := LEstChildNo < 0 
              END (* IF *) 
            END (* CASE *) 
          END (* IF *) 
        ELSE (* Even if we are among the tokens for the first list
                Est child, we are not on the first line. *) 
          LIsInFirstLine := FALSE 
        END (* IF *)  
      ELSE (* Est fixed node.  FsIsInFirstLine answers the question. *)  
        LIsInFirstLine := FsNodeRef . FsIsInFirstLine 
      END (* IF *) 
    ; RETURN LIsInFirstLine 
    END IsInFirstLine 

(* EXPORTED: *) 
; PROCEDURE CheckModFwd 
    ( READONLY EstTravInfo : EstTravInfoTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR (* OUT *) RelevantModRef : LbeStd . EstRootTyp 
    ; VAR (* OUT *) ModDelIsFinished : BOOLEAN 
    )
  RAISES { Backout }  
  (* For forward traversals. 
     RelevantModRef # NIL iff the next child is a mod for this fs item, 
     in which case it points to the relevant mod. 
     If it is a delete mod, then ModDelIsFinished means the Fs item is 
     the rightmost one it covers. 
  *) 

  = BEGIN (* CheckModFwd *) 
      ModDelIsFinished := FALSE 
    ; RelevantModRef := NIL 
    ; IF EstTravInfo . EtiChildNo < EstTravInfo . EtiChildCt 
      THEN (* An Est exists. *) 
        TYPECASE EstTravInfo . EtiChildLeafElem . LeChildRef 
        OF NULL 
        => (* No relevant mod. *) 

        | ModHs . ModDelTyp ( TModDelRef ) 
        => IF FsNodeRef . FsFmtNo >= EstTravInfo . EtiChildFmtNo 
           THEN 
             Assert 
               ( FsNodeRef . FsFmtNo <= TModDelRef . ModDelThruFmtNo 
               , AFT . A_CheckModFwd_Lost_ModDel 
               ) 
           ; Assert 
               ( NOT FsNodeRef . FsIsInsideList 
                 OR FsNodeRef . FsFmtNo # EstHs . FmtNoListEstChild
               , AFT . A_CheckModFwd_Deleted_Est_List_Child 
               ) 
           ; RelevantModRef := TModDelRef 
           ; IF FsNodeRef . FsFmtNo = TModDelRef . ModDelThruFmtNo 
             THEN 
               ModDelIsFinished := TRUE 
             END (* IF *) 
           END (* IF *) 

        | ModHs . ModRefTyp ( TModRef ) 
        => IF FsNodeRef . FsFmtNo = EstTravInfo . EtiChildFmtNo 
           THEN 
             RelevantModRef := TModRef 
           END (* IF *) 

        | SharedStrings . T ( TString ) 
        => IF FsNodeRef . FsFmtNo = EstTravInfo . EtiChildFmtNo 
              AND SharedStrings . Tok ( TString ) = LbeStd . Tok__LexErrChars 
           THEN 
             RelevantModRef := TString 
           END (* IF *) 

        | EstHs . EstRefTyp ( TEstRef ) 
        => IF FsNodeRef . FsFmtNo = EstTravInfo . EtiChildFmtNo 
              AND TEstRef . EstNodeKind 
                  = EstHs . EstNodeKindTyp . EstNodeKindModTok 
           THEN 
             RelevantModRef := TEstRef 
           END (* IF *) 

        ELSE 
        END (* TYPECASE *) 
      END (* IF *) 
    END CheckModFwd 

(* EXPORTED: *) 
; PROCEDURE DoCondFmtFwd 
    ( Lang : LbeStd . LangTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
      (* ^Predicate is evaluated on the current child herein. *) 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  : BOOLEAN 
  RAISES { Backout } 

  (* When traversing forwards and FsNode is a CondFmtNode, 
     ascertain whether the conditional format insertions 
     are to be done. *) 

  = VAR LEstTravInfo : EstTravInfoTyp 
  ; VAR LResultIfAbsent : BOOLEAN 

  ; BEGIN (* DoCondFmtFwd *) 
      Assert 
        ( FsNodeRef . FsKind = FsKindTyp . FsKindCondFmt 
        , AFT . A_DoCondFmtFwdFsNodeNotCondFmt 
        ) 
    ; CASE FsNodeRef . FsCondPredicate . PredicateKind  
      OF PredicateKindTyp . PredicateKindFalse 
      => RETURN FALSE  

      | PredicateKindTyp . PredicateKindTrue 
      => RETURN TRUE  

      | PredicateKindTyp . PredicateKindAbsent 
      , PredicateKindTyp . PredicateKindEmptyList 
      , PredicateKindTyp . PredicateKindNonpluralList 
      => LResultIfAbsent := TRUE 
(* REVIEW: An Ldl semantic question.  Considering an absent list to be empty 
   seems peculiar when an absent list and empty list are distinguished in 
   general, but so would the alternative, which would make an absent list 
   nonempty.  *)  

      | PredicateKindTyp . PredicateKindPresent 
      , PredicateKindTyp . PredicateKindNonemptyList 
      , PredicateKindTyp . PredicateKindPluralList 
      , PredicateKindTyp . PredicateKindInClass 
      => LResultIfAbsent := FALSE  

      | PredicateKindTyp . PredicateKindNull 
      => RETURN FALSE (* Or should we fail in this case? *) 
      END (* CASE *) 
    ; IF EstTravInfo . EtiChildNo < 0 
         OR EstTravInfo . EtiChildNo >= EstTravInfo . EtiChildCt  
      THEN (* Absent Est child. *) 
        RETURN LResultIfAbsent 
      ELSIF EstTravInfo . EtiChildFmtNo > FsNodeRef . FsFmtNo 
            AND NOT FsNodeRef . FsIsInsideList 
      THEN (* Another way to be absent. *) 
        RETURN LResultIfAbsent 
      ELSIF EstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
            AND EstHs . EstChildKindEstChild 
                IN EstTravInfo . EtiChildLeafElem . LeKindSet 
      THEN (* Fast case: We already have the Est child.  
              Optimized singleton list will always come here. 
           *) 
        RETURN 
          EstUtil . EvalPredicate 
            ( Lang 
            , FsNodeRef 
            , EstTravInfo . EtiChildLeafElem . LeChildRef 
            , EstTravInfo . EtiChildLeafElem . LeKindSet  
            ) 
      ELSE (* Have to get the Est child. *) 
        Assert 
          ( NOT EstTravInfo . EtiIsOptSingletonList 
          , AFT . A_DoCondFmtFwd_Optimized_singleton_list  
          ) 
      ; LEstTravInfo := EstTravInfo 
      ; IF EstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
           AND EstHs . EstChildKindTrailingMod  
               IN EstTravInfo . EtiChildLeafElem . LeKindSet 
        THEN (* We are in a trailing mod of the Est child.  This can happen
                when RebuildMarks descends to a trailing mod. *) 
          SetToPrevInKindSet 
            ( (* IN OUT *) LEstTravInfo 
            , LEstTravInfo . EtiChildNo 
            , EstHs . EstChildKindSetEstChild 
            ) 
        ; IF LEstTravInfo . EtiChildNo < 0 
             OR LEstTravInfo . EtiChildFmtNo # FsNodeRef . FsFmtNo 
          THEN (* Another way to be absent. *) 
            RETURN LResultIfAbsent 
          END (* IF *) 
        ELSE 
          SetToNextInKindSet 
            ( (* IN OUT *) LEstTravInfo 
            , LEstTravInfo . EtiChildNo 
            , EstHs . EstChildKindSetEstChild 
            ) 
        ; IF LEstTravInfo . EtiChildNo >= LEstTravInfo . EtiChildCt  
             OR LEstTravInfo . EtiChildFmtNo # FsNodeRef . FsFmtNo 
          THEN (* Another way to be absent. *) 
            RETURN LResultIfAbsent 
          END (* IF *) 
        END (* IF *) 
      ; RETURN 
          EstUtil . EvalPredicate 
            ( Lang 
            , FsNodeRef 
            , LEstTravInfo . EtiChildLeafElem . LeChildRef 
            , EstTravInfo . EtiChildLeafElem . LeKindSet  
            ) 
      END 
    END DoCondFmtFwd 

(* EXPORTED: *) 
; PROCEDURE DoCondFmtBwd 
    ( Lang : LbeStd . LangTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
      (* ^Predicate is evaluated on the current child herein. *) 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ) 
  : BOOLEAN 
  RAISES { Backout } 
  (* When traversing backwards and FsNode is a CondFmtNode, 
     ascertain whether the conditional format insertions 
     are to be done. *) 

  = VAR LEstTravInfo : EstTravInfoTyp 
  ; VAR LResultIfAbsent : BOOLEAN 

  ; BEGIN (* DoCondFmtBwd *) 
      Assert 
        ( FsNodeRef . FsKind = FsKindTyp . FsKindCondFmt 
        , AFT . A_DoCondFmtBwdFsNodeNotCondFmt 
        )
    ; CASE FsNodeRef . FsCondPredicate . PredicateKind  
      OF PredicateKindTyp . PredicateKindFalse 
      => RETURN FALSE  
      | PredicateKindTyp . PredicateKindTrue 
      => RETURN TRUE  
      | PredicateKindTyp . PredicateKindAbsent 
      , PredicateKindTyp . PredicateKindEmptyList 
      , PredicateKindTyp . PredicateKindNonpluralList 
      => LResultIfAbsent := TRUE 
      | PredicateKindTyp . PredicateKindPresent 
      , PredicateKindTyp . PredicateKindNonemptyList 
      , PredicateKindTyp . PredicateKindPluralList 
      , PredicateKindTyp . PredicateKindInClass 
      => LResultIfAbsent := FALSE  
      | PredicateKindTyp . PredicateKindNull 
      => RETURN FALSE (* Or should we fail in this case? *) 
      END (* CASE *) 
    ; IF EstTravInfo . EtiChildNo < 0 
         OR EstTravInfo . EtiChildNo >= EstTravInfo . EtiChildCt  
      THEN (* Absent Est child. *) 
        RETURN LResultIfAbsent  
      ELSIF EstTravInfo . EtiChildFmtNo < FsNodeRef . FsFmtNo 
            AND NOT FsNodeRef . FsIsInsideList 
      THEN (* Another way to be absent. *) 
        RETURN LResultIfAbsent 
      ELSIF EstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
            AND EstHs . EstChildKindEstChild 
                IN EstTravInfo . EtiChildLeafElem . LeKindSet 
      THEN (* Fast case: We already have the Est child. *) 
           (* Optimized singleton list will always come here. *) 
        RETURN 
          EstUtil . EvalPredicate 
            ( Lang 
            , FsNodeRef 
            , EstTravInfo . EtiChildLeafElem . LeChildRef 
            , EstTravInfo . EtiChildLeafElem . LeKindSet  
            ) 
      ELSE 
        Assert 
          ( NOT EstTravInfo . EtiIsOptSingletonList 
          , AFT . A_DoCondFmtBwd_Optimized_singleton_list  
          ) 
      ; LEstTravInfo := EstTravInfo 
      ; EstUtil . PrevInKindSet 
          ( LEstTravInfo . EtiParentRef 
          , LEstTravInfo . EtiChildNo 
          , EstHs . EstChildKindSetEstChild 
          , (* VAR *) LEstTravInfo . EtiChildNo 
          , (* VAR *) LEstTravInfo . EtiChildRelNodeNo 
          , (* VAR *) LEstTravInfo . EtiChildLeafElem 
          ) 
      ; SetEstChildFmtNo ( LEstTravInfo , GroupFwdMove := FALSE ) 
      ; IF LEstTravInfo . EtiChildNo < 0 
           OR LEstTravInfo . EtiChildFmtNo < FsNodeRef . FsFmtNo 
        THEN (* Another way to be absent. *) 
          RETURN LResultIfAbsent  
        ELSE
          Assert
            ( LEstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
            , AFT . A_DoCondFmtBwdLeftoverEstChild 
            ) 
        ; RETURN 
            EstUtil . EvalPredicate 
              ( Lang 
              , FsNodeRef 
              , LEstTravInfo . EtiChildLeafElem . LeChildRef 
              , EstTravInfo . EtiChildLeafElem . LeKindSet  
              ) 
        END (* IF *) 
      END (* IF *)  
    END DoCondFmtBwd 

(* EXPORTED: *) 
; PROCEDURE GetDescendantWithNodeNo 
    ( Root : LbeStd . EstRootTyp 
    ; EstNodeNo : LbeStd . EstNodeNoTyp 
      (* ^Node number relative to Root. *) 
    ; VAR EstRef : LbeStd . EstRootTyp 
    ; VAR KindSet : EstHs . EstChildKindSetTyp 
    ; VAR IsOptSingletonList : BOOLEAN 
    ) 
  RAISES { Backout } 
  (* IsOptSingletonList means EstNodeNo is for an optimized-away singleton list,
     and EstRef and KindSet denote its one element, whose actual NodeNo is 
     EstNodeNo + 1.
  *) 

  = VAR LRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LEstTravInfo : EstTravInfoTyp 

  ; BEGIN (* DescendantWithNodeNo *) 
      IF Root = NIL 
         OR EstNodeNo < 0 
         OR EstNodeNo >= EstUtil . EstNodeCt ( Root ) 
      THEN 
        EstRef := Root 
      ; KindSet := EstHs . EstChildKindSetEmpty 
      ; IsOptSingletonList := FALSE 
      ELSE 
        LRelNodeNo := EstNodeNo 
      ; LEstTravInfo . EtiChildLeafElem . LeChildRef := Root 
      ; LEstTravInfo . EtiChildLeafElem . LeKindSet
          := EstHs . EstChildKindSetEmpty 
      ; LOOP 
          (* INVARIANT: We are at the current child of LEstTravInfo. *) 
          IF LRelNodeNo = 1  
             AND EstHs . EstChildKindOptSingletonList  
                 IN LEstTravInfo . EtiChildLeafElem . LeKindSet 
          THEN (* The sought node number is the one element of an
                  optimized-away singleton list.  
               *) 
            EstRef := LEstTravInfo . EtiChildLeafElem . LeChildRef 
          ; KindSet 
              := EstUtil . EstChildKindSet 
                   ( LEstTravInfo . EtiChildLeafElem . LeChildRef ) 
          ; IsOptSingletonList := FALSE (* This is the element, not the list. *)
          ; EXIT  
          ELSIF LRelNodeNo = 0 
          THEN (* We are at the sought node number.  If it is an optimized-away
                  singleton list, LEstTravInfo . EtiChildLeafElem . LeChildRef 
                  and LEstTravInfo . EtiChildLeafElem . LeKindSet denote its
                  one list element, whose NodeNo  = EstNodeNo + 1. 
               *) 
            EstRef := LEstTravInfo . EtiChildLeafElem . LeChildRef 
          ; KindSet := LEstTravInfo . EtiChildLeafElem . LeKindSet 
          ; IsOptSingletonList 
              := EstHs . EstChildKindOptSingletonList  
                 IN LEstTravInfo . EtiChildLeafElem . LeKindSet 
          ; EXIT  
          ELSE 
            Assert 
              ( 0 < LRelNodeNo 
                AND LRelNodeNo 
                    < EstUtil . EstNodeCt 
                        ( LEstTravInfo . EtiChildLeafElem . LeChildRef ) 
                      + ORD ( EstHs . EstChildKindOptSingletonList 
                              IN LEstTravInfo . EtiChildLeafElem . LeKindSet 
                            )
              , AFT . A_GetDescendantWithNodeNo__NodeNo_out_of_range 
              ) 
          ; InitToChildContainingNodeNo 
              ( (* VAR *) LEstTravInfo 
              , LEstTravInfo . EtiChildLeafElem . LeChildRef 
              , LRelNodeNo 
              , LEstTravInfo . EtiChildLeafElem . LeKindSet 
              ) 
          ; DEC ( LRelNodeNo , LEstTravInfo . EtiChildRelNodeNo ) 
          END (* IF *) 
        END (* LOOP *) 
      END (* IF *) 
    END GetDescendantWithNodeNo 

(* EXPORTED: *) 
; PROCEDURE NodeCtOfDescendantWithNodeNo 
    ( Root : LbeStd . EstRootTyp 
    ; EstNodeNo : LbeStd . EstNodeNoTyp 
      (* ^Node number relative to Root. *) 
    ) 
  : LbeStd . EstNodeNoTyp 
  RAISES { Backout } 

  = VAR LEstRef : LbeStd . EstRootTyp
  ; VAR LKindSet : EstHs . EstChildKindSetTyp  
  ; VAR LResult : LbeStd . EstNodeNoTyp  
  ; VAR LIsOptSingletonList : BOOLEAN 

  ; BEGIN 
      GetDescendantWithNodeNo 
        ( Root 
        , EstNodeNo 
        , (* VAR *) EstRef := LEstRef 
        , (* VAR *) KindSet := LKindSet 
        , (* VAR *) IsOptSingletonList := LIsOptSingletonList  
        ) 
    ; LResult := EstUtil . EstNodeCt ( LEstRef ) 
    ; INC ( LResult , ORD ( LIsOptSingletonList ) )
    ; RETURN LResult 
    END NodeCtOfDescendantWithNodeNo 

(* EXPORTED: *) 
; PROCEDURE DescendCondFmt 
    ( Lang : LbeStd . LangTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; FmtNo : EstHs . FmtNoTyp
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ; VAR Predicate : BOOLEAN 
    ; VAR FsChildNo : LangUtil . FsChildNoTyp 
    ; Bwd : BOOLEAN := FALSE
    ) 
   RAISES { Backout } 
  (* When descending through a CondFmt node, call this to check the predicate
     and find the appropriate immediate child number. *) 

  = VAR LFsChildNo : LangUtil . FsChildNoSignedTyp 
  ; VAR LFsNodeRef : LangUtil . FsNodeRefTyp 

  ; BEGIN (* DescendCondFmt *) 
      LFsNodeRef := FsNodeRef 
    ; LFsChildNo := LangUtil . FsChildNoOfFmtNo ( LFsNodeRef , FmtNo ) 
    ; FsChildNo := LFsChildNo 
    ; LOOP 
        IF LFsChildNo = LangUtil . FsChildNoAlt 
        THEN 
          Predicate := FALSE 
        ; EXIT 
        ELSE 
          WITH WFsChildRef = LFsNodeRef . FsChildren ^ [ LFsChildNo ] 
          DO 
            CASE WFsChildRef . FsKind <* NOWARN *>
            OF FsKindTyp . FsKindBegOfImage 
            , FsKindTyp . FsKindEndOfImage 
            , FsKindTyp . FsKindInsTok 
            , FsKindTyp . FsKindLineBreakOpt 
            , FsKindTyp . FsKindLineBreakReqd  
            => Predicate := TRUE 
            ; EXIT

            | FsKindTyp . FsKindSubtreeVert 
            , FsKindTyp . FsKindSubtreeHoriz 
            , FsKindTyp . FsKindSubtreeFill 
            => LFsNodeRef := WFsChildRef 
            ; LFsChildNo := LangUtil . FsChildNoOfFmtNo ( LFsNodeRef , FmtNo ) 

            | FsKindTyp . FsKindEstChildOfFixed 
            , FsKindTyp . FsKindEstChildOfList 
            => IF Bwd 
              THEN 
                Predicate := DoCondFmtBwd ( Lang , EstTravInfo , FsNodeRef ) 
              ELSE 
                Predicate := DoCondFmtFwd ( Lang , EstTravInfo , FsNodeRef ) 
              END (* IF *) 
            ; EXIT 
         (* ELSE CantHappen *) 
            END (* CASE *)  
          END (* WITH *) 
        END (* IF *) 
      END (* LOOP *) 
    END DescendCondFmt 

(* EXPORTED: *) 
; PROCEDURE FsLeafRefOfFmtNo 
    ( Lang : LbeStd . LangTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp 
    ; FmtNo : EstHs . FmtNoTyp 
    ; ChildEstRef : LbeStd . EstRootTyp
    ; ChildKindSet : EstHs . EstChildKindSetTyp 
    )
  : LangUtil . FsNodeRefTyp
  RAISES { Backout } 
  (* Return the Fs leaf that has FmtNo.  Will evaluate predicates if
     necessary, to get to the correct alternative FsEstChild. 
  *)

  = VAR LFsNodeRef : LangUtil . FsNodeRefTyp 

  ; BEGIN (* FsLeafRefOfFmtNo *) 
      LFsNodeRef := FsNodeRef
    ; LOOP 
        IF LFsNodeRef = NIL 
        THEN EXIT 
        ELSE 
          CASE LFsNodeRef . FsKind 
          OF FsKindTyp . FsKindEstFixedVert  
          , FsKindTyp . FsKindEstFixedHoriz  
          , FsKindTyp . FsKindEstFixedFill  
          , FsKindTyp . FsKindSubtreeVert  
          , FsKindTyp . FsKindSubtreeHoriz  
          , FsKindTyp . FsKindSubtreeFill  
          , FsKindTyp . FsKindEstListVert  
          , FsKindTyp . FsKindEstListHoriz  
          , FsKindTyp . FsKindEstListFill  
          , FsKindTyp . FsKindEstListTrailVert 
          , FsKindTyp . FsKindEstListTrailHoriz 
          , FsKindTyp . FsKindEstListTrailFill 
          => (* Things that have Fs children. *) 
            LFsNodeRef := LangUtil . FsChildRefOfFmtNo ( LFsNodeRef , FmtNo ) 

          | FsKindTyp . FsKindCondFmt   
          => IF EstUtil . EvalPredicate 
                 ( Lang 
                 , LFsNodeRef 
                 , ChildEstRef 
                 , ChildKindSet 
                 ) 
            THEN 
              LFsNodeRef 
                := LFsNodeRef . FsChildren ^ [ LFsNodeRef . FsLeadingChildCt ]
            ELSE (* Try the next alternative. *) 
              LFsNodeRef := LFsNodeRef . FsCondAltRef 
            END (* IF *) 

          ELSE (* We are at the desired Fs leaf node. *) 
            EXIT 
          END (* CASE *) 
        END (* IF *) 
      END (* LOOP *) 
    ; RETURN LFsNodeRef
    END FsLeafRefOfFmtNo

; PROCEDURE GetNodeInfo  
    ( Lang : LbeStd . LangTyp 
    ; EstRoot : LbeStd . EstRootTyp 
    ; StartMark : Marks . TokMarkTyp 
    ; VAR EstTravInfo : EstTravInfoTyp 
    ; VAR ParentFsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR ParentIndentPos : LbeStd . LimitedCharNoTyp 
    ) 
  RAISES { Backout } 
 
  = VAR LParentKindSet : EstHs . EstChildKindSetTyp 
  ; VAR LParentIndentPos1 , LParentIndentPosN : LbeStd . LimitedCharNoTyp 
  ; VAR LChildIndentPos1 , LChildIndentPosN : LbeStd . LimitedCharNoTyp 
  ; VAR LRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LParentFsNodeRef : LangUtil . FsNodeRefTyp 
  ; VAR LChildFsRuleNodeRef , LChildFsLeafNodeRef : LangUtil . FsNodeRefTyp 
  ; VAR LIsFirstLine : BOOLEAN 

  ; BEGIN (* GetNodeInfo *) 
      IF EstRoot = NIL OR StartMark . TkmEstNodeNo = 0 
      THEN
        EstTravInfo . EtiNodeRef := NIL  
      ; EstTravInfo . EtiParentRef := NIL  
      ; EstTravInfo . EtiStringRef := NIL  
      ; EstTravInfo . EtiChildCt := 1 
      ; EstTravInfo . EtiChildNo := 0 
      ; EstTravInfo . EtiChildRelNodeNo := 1 
      ; LParentFsNodeRef := NIL 
      ; ParentIndentPos := Options . InitialIndentPos  
      ; EstTravInfo . EtiChildLeafElem . LeChildRef := EstRoot  
      ; EstTravInfo . EtiChildLeafElem . LeKindSet 
          := EstUtil . EstChildKindSet ( EstRoot ) 
      ; EstTravInfo . EtiChildLeafElem . LeCumNodeCt 
          := EstUtil . EstNodeCt ( EstRoot ) 
      ; EstTravInfo . EtiChildLeafElem . LeFmtNo := EstHs . FmtNoNull 
      ; EstTravInfo . EtiChildFmtNo := EstHs . FmtNoNull 
      ; EstTravInfo . EtiAbsNodeNo := 0 
      ; EstTravInfo . EtiIsOptSingletonList := FALSE  
      ELSE 
        EstTravInfo . EtiNodeRef := EstRoot 
      ; LParentKindSet := EstUtil . EstChildKindSet ( EstRoot ) 
      ; LParentFsNodeRef 
          := EstUtil . FsRuleForEstNode ( Lang , EstTravInfo . EtiNodeRef ) 
      ; LParentIndentPos1 := Options . InitialIndentPos    
      ; LParentIndentPosN := Options . InitialIndentPos    
      ; ParentIndentPos := LParentIndentPos1 
      ; LRelNodeNo := StartMark . TkmEstNodeNo 
      ; LOOP (* INVARIANT: the following are set for the parent node: 
                    EstTravInfo . EtiNodeRef 
                    LParentFsNodeRef 
                    ParentIndentPos 
                    LParentIndentPos1
                    LParentIndentPosN
                    LParentKindSet 
                AND EstTravInfo . EtiNodeRef is not the sought node. 
             *) 
          InitToChildContainingNodeNo 
            ( (* VAR *) EstTravInfo 
            , EstTravInfo . EtiNodeRef 
            , LRelNodeNo 
            , LParentKindSet 
            ) 
        ; DEC ( LRelNodeNo , EstTravInfo . EtiChildRelNodeNo ) 
        ; IF LRelNodeNo = 0 
          THEN (* We are done *) 
            EXIT 
          ELSE (* Descend. *) 
            Assert 
              ( EstTravInfo . EtiChildLeafElem . LeChildRef # NIL 
              , AFT . A_GetInfoForNodeNoAtBolNILEstChild 
              ) 
          ; LChildFsLeafNodeRef 
              := FsLeafRefOfFmtNo
                   ( Lang 
                   , LParentFsNodeRef 
                   , EstTravInfo . EtiChildFmtNo 
                   , EstTravInfo . EtiChildLeafElem . LeChildRef 
                   , EstTravInfo . EtiChildLeafElem . LeKindSet  
                   ) 
          ; LChildFsRuleNodeRef 
              := LangUtil . FsRuleForEstChild 
                   ( Lang 
                   , LChildFsLeafNodeRef 
                   , EstTravInfo . EtiChildLeafElem 
                   ) 
          ; LIsFirstLine  
              := EstTravInfo . EtiChildNo 
                 < EstTravInfo . EtiParentRef . KTreeEstChildCtLeftOfNl 
          ; TYPECASE EstTravInfo . EtiChildLeafElem . LeChildRef <* NOWARN *>
            OF EstHs . EstRefTyp ( TEstRef ) (* NIL OK here too. *) 
            => IF TEstRef # NIL 
                 AND TEstRef . EstNodeKind 
                     = EstHs . EstNodeKindTyp . EstNodeKindModTok 
              THEN 
                LChildIndentPos1 
                  := EstUtil . WidthSum 
                      ( LParentIndentPos1 
                      , LangUtil . IndentAmt 
                          ( Lang , LangUtil . IndentCodeModTok ) 
                      ) 
              ; LChildIndentPosN 
                  := EstUtil . WidthSum 
                      ( LParentIndentPosN 
                      , LangUtil . IndentAmt 
                          ( Lang , LangUtil . IndentCodeModTok ) 
                      ) 
              ELSE 
                Assert 
                  ( LChildFsLeafNodeRef . FsKind 
                    IN LangUtil . FsKindSetEstChild 
                  , AFT . A_GetInfoForNodeNoAtBolNotEstChild 
                  ) 
              ; ChildIndentPositions 
                  ( Lang 
                  , LChildFsLeafNodeRef
                  , LParentIndentPos1
                  , LParentIndentPosN 
                  , (* VAR *) LChildIndentPos1
                  , (* VAR *) LChildIndentPosN
                  , IsFirstLine := LIsFirstLine 
                  ) 
              END (* IF *) 
            END (* TYPECASE *) 

          ; EstTravInfo . EtiNodeRef 
              := EstTravInfo . EtiChildLeafElem . LeChildRef 
          ; LParentFsNodeRef := LChildFsRuleNodeRef 
          ; IF LIsFirstLine 
            THEN ParentIndentPos := LChildIndentPos1 
            ELSE ParentIndentPos := LChildIndentPosN
            END (* IF *)  
          ; LParentIndentPos1 := LChildIndentPos1 
          ; LParentIndentPosN := LChildIndentPosN 
          ; LParentKindSet := EstTravInfo . EtiChildLeafElem . LeKindSet 
          END (* LOOP *) 
        END (* IF *) 
      END (* IF*)
    ; ParentFsNodeRef := LParentFsNodeRef 
      (* m3gdb fails to print ParentFsNodeRef, hence the local. *)  
    END GetNodeInfo 

(* EXPORTED: *) 
; PROCEDURE IndentPosOfBolTokMark 
    ( Lang : LbeStd . LangTyp 
    ; EstRoot : LbeStd . EstRootTyp 
    ; TokMark : Marks . TokMarkTyp 
    ) 
  : LbeStd . LimitedCharNoTyp 
  RAISES { Backout } 
  (* PRE: TokMark denotes a new line. *) 
  (* The actual indent position for the text beginning on a new line at the 
     point TokMark denotes, in the tree rooted at EstRoot.  
  *) 

  (* This is a mini-traverser that only descends, to the marked point. *)  

  = VAR LParentFsNodeRef : LangUtil . FsNodeRefTyp
  ; VAR LFsRuleNodeRef : LangUtil . FsNodeRefTyp
  ; VAR LFsLeafNodeRef : LangUtil . FsNodeRefTyp 
  ; VAR LEstTravInfo : EstTravInfoTyp 
  ; VAR LParentIndentPos : LbeStd . LimitedCharNoTyp  

  ; BEGIN (* IndentPosOfBolTokMark *) 
      CASE TokMark . Kind <* NOWARN *> 
      OF MarkKindTyp . Null 
      => RETURN Options . InitialIndentPos 

      | MarkKindTyp . Plain 
      , MarkKindTyp . BlankLine 
      => GetNodeInfo 
           ( Lang := Lang 
           , EstRoot := EstRoot 
           , StartMark := TokMark 
           , (* VAR *) EstTravInfo := LEstTravInfo  
           , (* VAR *) ParentFsNodeRef := LParentFsNodeRef 
           , (* VAR *) ParentIndentPos := LParentIndentPos 
           ) 
      ; IF TokMark . StartAtEnd 
        THEN 
          RETURN LParentIndentPos 
        ELSE 
          TYPECASE LEstTravInfo . EtiChildLeafElem . LeChildRef 

          OF NULL 
          => CantHappen ( AFT . A_IndentPosOfBolTokMarkNilNode ) 
          ; RETURN 0 (* Quash compile warning. *) 

          | ModHs . ModCmntLeadingFixedTyp ( TModCmnt ) 
          => RETURN TModCmnt . ModCmntFromPos 

          | ModHs . ModCmntLeadingRelativeTyp ( TModCmnt ) 
          => IF LEstTravInfo . EtiNodeRef = NIL 
            THEN (* Stand alone Mod Comment. Unlikely, but be thorough. *) 
              RETURN TModCmnt . ModCmntFromPos 
            ELSE (* Must find the Est child this comment is attached to. *)
              SetToNextInKindSet 
                ( LEstTravInfo 
                , LEstTravInfo . EtiChildNo 
                , EstHs . EstChildKindSetEstChild 
                ) 
            ; IF LEstTravInfo . EtiChildNo >= LEstTravInfo . EtiChildCt 
                 OR LEstTravInfo . EtiChildFmtNo # TokMark . FmtNo 
              THEN (* Est child is absent. *) 
                LFsLeafNodeRef 
                  := FsLeafRefOfFmtNo   
                       ( Lang 
                       , LParentFsNodeRef 
                       , TokMark . FmtNo 
                       , ChildEstRef := NIL 
                       , ChildKindSet := EstHs . EstChildKindSetEmpty 
                       )
              ELSE 
                LFsLeafNodeRef 
                  := FsLeafRefOfFmtNo   
                       ( Lang 
                       , LParentFsNodeRef 
                       , TokMark . FmtNo 
                       , ChildEstRef 
                           := LEstTravInfo . EtiChildLeafElem . LeChildRef 
                       , ChildKindSet 
                           := LEstTravInfo . EtiChildLeafElem . LeKindSet 
                       )
              END (* IF *) 
            ; RETURN 
                EstUtil . WidthSum   
                  ( IndentPos 
                      ( Lang 
                      , LParentIndentPos 
                      , LFsLeafNodeRef . FsIndentCode 
                      ) 
                  , TModCmnt . ModCmntFromPos 
                  ) 
            END (* IF *) 

          | ModHs . ModTextTyp ( TModText ) 
          => RETURN TModText . ModTextFromPos 

          ELSE 
            CantHappen ( AFT . A_IndentPosOfBolTokMarkBadMod ) 
          ; RETURN 0 (* Quash compile warning. *) 
          END (* TYPECASE *) 
        END (* IF *) 

      | MarkKindTyp . LeftSibFmtNo 
      , MarkKindTyp . RightSibFmtNo 
      => GetNodeInfo 
           ( Lang := Lang 
           , EstRoot := EstRoot 
           , StartMark := TokMark 
           , (* VAR *) EstTravInfo := LEstTravInfo (* Dead. *) 
           , (* VAR *) ParentFsNodeRef := LParentFsNodeRef 
           , (* VAR *) ParentIndentPos := LParentIndentPos 
           ) 
      ; LFsLeafNodeRef 
          := LangUtil . FsLeafRefOfFmtNo ( LParentFsNodeRef , TokMark . FmtNo )
      ; RETURN 
          IndentPos 
            ( Lang 
            , LParentIndentPos 
            , LFsLeafNodeRef . FsIndentCode 
            ) 

      | MarkKindTyp . ChildFmtNo 
      => GetNodeInfo 
           ( Lang := Lang 
           , EstRoot := EstRoot 
           , StartMark := TokMark 
           , (* VAR *) EstTravInfo := LEstTravInfo  
           , (* VAR *) ParentFsNodeRef := LParentFsNodeRef (* Dead. *) 
           , (* VAR *) ParentIndentPos := LParentIndentPos 
           ) 
      ; LFsRuleNodeRef 
          := EstUtil . FsRuleForEstNode 
               ( Lang , LEstTravInfo . EtiChildLeafElem . LeChildRef ) 
      ; LFsLeafNodeRef 
          := LangUtil . FsLeafRefOfFmtNo ( LFsRuleNodeRef , TokMark . FmtNo )
      ; RETURN 
          IndentPos 
            ( Lang 
            , LParentIndentPos 
            , LFsLeafNodeRef . FsIndentCode 
            ) 
      END (* CASE *) 
    END IndentPosOfBolTokMark 

(* EXPORTED: *) 
; PROCEDURE GetFsEstDescendant 
    ( FsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR ParentRef : LangUtil . FsNodeRefTyp 
    ; VAR DescendantRef : LangUtil . FsNodeRefTyp 
    ; VAR FromFsChildNo : LangUtil . FsChildNoTyp 
    ) 

  = VAR LFsNodeRef : LangUtil . FsNodeRefTyp 

  ; BEGIN 
      LFsNodeRef := FsNodeRef 
    ; DescendantRef := NIL 
    ; ParentRef := NIL 
    (* We never want this to return its starting Fs node.  Otherwise, a
       typical caller would go into infinite recursion. *) 
    ; FromFsChildNo := LangUtil . FsChildNoNull 
    ; LOOP 
        IF LFsNodeRef = NIL 
           OR LFsNodeRef . FsKind IN LangUtil . FsKindSetEstChild 
           OR LFsNodeRef . FsChildren = NIL 
           OR LFsNodeRef . FsLeadingChildCt < 0 
           OR LFsNodeRef . FsLeadingChildCt 
              >= NUMBER ( LFsNodeRef . FsChildren ^ )   
        THEN EXIT 
        ELSE 
          FromFsChildNo := LFsNodeRef . FsLeadingChildCt
        ; ParentRef := LFsNodeRef 
        ; LFsNodeRef := ParentRef . FsChildren ^ [ FromFsChildNo ] 
        ; DescendantRef := LFsNodeRef 
        END (* IF *) 
      END (* LOOP *) 
    END GetFsEstDescendant

; PROCEDURE PreTraverseLine  
    ( Lang : LbeStd . LangTyp 
    ; ModTextIsToLeftOnLine : BOOLEAN 
    ; <*NOWARN*> VALUE EstTravInfo : EstTravInfoTyp 
      (* ^Yes, I *really* do want this passed by value. *) 
    ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
    ; StartFmtNo : EstHs . FmtNoTyp 
    ; CharPos : LbeStd . LimitedCharNoSignedTyp 
    ; CurrentLineIndentPos : LbeStd . LimitedCharNoTyp 
    ; PrevTok : LbeStd . TokTyp 
    ; VAR LastFmtNoOnLine : EstHs . FmtNoTyp 
          (* ^Items through LastFmtNoOnLine will fit.  EstHs . FmtNoNull, means 
             not even StartFmtNo will fit. *) 
    ; VAR EstListChildrenToPass : LbeStd . EstChildNoTyp 
          (* ^This many Est list children will fit on the line, before looking
             for LastFmtNoOnLine. *)
    ) 
  RAISES { Backout } 
  (* Ascertain how much text of this FsTree we can get onto the current line, 
     starting at the spot given by the parameters and assuming optional line
     breaks are NOT taken.  The last item that fits includes 
     EstListChildrenToPass list children, plus through the item with FmtNo
     LastFmtNoOnLine.    
  *)

  (* This descends from the root of an Fs tree, to StartFmtNo, then begins
     Forward traversal from there, using Charpos, and PrevTok, until it fills
     the line, which could bring it up higher in the Fs Tree than where it
     began.  But it still won't go into a containing Est, if that is what is
     needed to fill the line.  So why not just start at the FsSubtree 
     containing StartFmtNo and stay within it, even if we don't fill the
     line.  *)  

  = TYPE PtlStateTyp 
      = { PtlStateStart    (* At start of traversed range. *) 
        , PtlStateInside   (* Inside traversed range. *) 
        , PtlStateDone     (* Done traversing. *) 
        } 

  ; VAR PtlPrevTok : LbeStd . TokTyp 
  ; VAR PtlCharPos : LbeStd . LimitedCharNoTyp 
  ; VAR PtlState : PtlStateTyp 

  ; PROCEDURE PtlFloatingItemFromPos 
      ( Tok : LbeStd . TokTyp ) : LbeStd . LimitedCharNoTyp 

    = BEGIN (* PtlFloatingItemFromPos *) 
        IF ModTextIsToLeftOnLine 
           OR CurrentLineIndentPos <= PtlCharPos 
        THEN 
          RETURN 
            EstUtil . WidthSum 
              ( PtlCharPos 
              , ORD ( LangUtil . NeedsSep ( Lang , PtlPrevTok , Tok ) ) 
              ) 
        ELSE 
          RETURN CurrentLineIndentPos (* This should never happen. *)   
        END (* IF *) 
      END PtlFloatingItemFromPos 

  ; PROCEDURE PtlCountToPos ( ToCharPos : LbeStd . LimitedCharNoSignedTyp ) 

    = BEGIN (* PtlCountToPos *) 
        IF NOT ModTextIsToLeftOnLine AND ToCharPos > Options . RightMargin 
        THEN PtlState := PtlStateTyp . PtlStateDone 
        ELSIF ToCharPos > PtlCharPos 
        THEN 
          PtlCharPos := ToCharPos 
        ; PtlPrevTok := LbeStd . Tok__Sep 
        END (* IF *) 
      END PtlCountToPos 

  ; PROCEDURE PtlCountWidth ( Width : LbeStd . LimitedCharNoTyp ) 

    = BEGIN (* PtlCountWidth *) 
        PtlCharPos := EstUtil . WidthSum ( PtlCharPos , Width ) 
      ; IF NOT ModTextIsToLeftOnLine AND PtlCharPos > Options . RightMargin 
        THEN 
          PtlState := PtlStateTyp . PtlStateDone 
        END (* IF *) 
      END PtlCountWidth 

  ; PROCEDURE PtlCountFloatingTok 
      ( Tok : LbeStd . TokTyp ; StringRef : SharedStrings . T ) 

    = BEGIN (* PtlCountFloatingTok *) 
        PtlCountToPos ( PtlFloatingItemFromPos ( Tok ) ) 
      ; IF PtlState # PtlStateTyp . PtlStateDone 
        THEN 
          PtlCountWidth ( SharedStrings . Length ( StringRef ) ) 
        ; PtlPrevTok := Tok 
        END (* IF *) 
      END PtlCountFloatingTok 

  ; PROCEDURE PtlTraverseFs ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Backout } 
    (* PRE: PtlState # PtlStateTyp . PtlStateDone *) 

    = PROCEDURE PtlTfsEstSubtree ( ) RAISES { Backout } 

      (* Do not descend into the child.  Just use its width. *) 

      = VAR LFromPos : LbeStd . LimitedCharNoTyp 
      ; VAR LEstMiscInfo : EstHs . EstMiscInfoTyp 

      ; BEGIN (* PtlTfsEstSubtree *) 
          IF PtlState # PtlStateTyp . PtlStateDone 
          THEN 
            PtlState := PtlStateTyp . PtlStateInside 
          ; LEstMiscInfo 
              := EstUtil . EstMiscInfo 
                   ( Lang , EstTravInfo . EtiChildLeafElem . LeChildRef ) 
          ; WITH WWidthInfo = LEstMiscInfo . EmiWidthInfo 
            DO Assert
                 ( NOT WWidthInfo . WiWholeLineModsOnly 
                 , AFT . A_PtlTfsEstSubtree_Whole_line_mods_only 
                 ) 
            ; IF WWidthInfo . WiWidth = LbeStd . LimitedCharNoInfinity 
              THEN 
                PtlState := PtlStateTyp . PtlStateDone 
              ELSE  
                LFromPos 
                   := PtlFloatingItemFromPos ( LEstMiscInfo . EmiLeftTok ) 
              ; IF LFromPos > WWidthInfo . WiNlTrigger 
                THEN 
                  PtlState := PtlStateTyp . PtlStateDone 
                ELSE 
                  IF WWidthInfo . WiNlTrigger = LbeStd . LimitedCharNoInfinity 
                  THEN (* Relative WiWidth. *) 
                    PtlCountToPos ( LFromPos ) 
                  ; IF PtlState # PtlStateTyp . PtlStateDone 
                    THEN 
                      PtlCountWidth ( WWidthInfo . WiWidth ) 
                    END (* IF *) 
                  ELSE (* Absolute WiWidth. *) 
                    PtlCountToPos ( WWidthInfo . WiWidth ) 
                  END (* IF *) 
                ; PtlPrevTok := LEstMiscInfo . EmiRightTok 
                END (* IF *) 
              END (* IF *) 
            END (* WITH WWidthInfo *) 
          END (* IF *) 
        ; IncEstChild ( EstTravInfo ) 
        END PtlTfsEstSubtree 

    (* Leading mods. *) 

    ; PROCEDURE PtlTfsModCmnt ( ModCmntRef : ModHs . ModCmntTyp ) 
      RAISES { Backout } 
      (* PRE: PtlState # PtlStateTyp . PtlStateDone *) 

      = BEGIN (* PtlTfsModCmnt *) 
          TYPECASE ModCmntRef <* NOWARN *> 
          OF ModHs . ModCmntLeadingFixedTyp 
          => IF ModCmntRef . ModCmntNlBefore 
                OR PtlCharPos > ModCmntRef . ModCmntFromPos 
             THEN (* Explicit or implicit Nl at beginning of comment. *) 
               IF PtlPrevTok = LbeStd . Tok__BegOfLine 
               THEN (* Pass over the NlBefore. *) 
                 IF ModCmntRef . ModCmntNlAfter 
                 THEN (* Continue at beginning of next line. *) 
                   PtlCharPos := 0 
                 ELSE  
                   PtlCharPos 
                     := EstUtil . WidthSum 
                          ( ModCmntRef . ModCmntFromPos  
                          , LbeStd . ProjectToLimitedCharNoTyp
                              ( SharedStrings . Length 
                                  ( ModCmntRef . ModCmntStringRef ) 
                              ) 
                          )
                 ; PtlPrevTok := LbeStd . Tok__Cmnt  
                 END (* IF *) 
               ; PtlState := PtlStateTyp . PtlStateInside 
               ; IncEstChild ( EstTravInfo ) 
               ELSE (* Stop at the NlBefore. *) 
                 PtlState := PtlStateTyp . PtlStateDone  
               END (* IF *)  
             ; RETURN (* Skip steps at end of this proc. *) 
             ELSE (* Fixed, no NlBefore. *) 
               PtlCountToPos ( ModCmntRef . ModCmntFromPos ) 
             ; PtlState := PtlStateTyp . PtlStateInside 
             END (* IF *) 
 
          | ModHs . ModCmntLeadingRelativeTyp 
          => IF ModCmntRef . ModCmntNlBefore 
             THEN 
               IF PtlPrevTok = LbeStd . Tok__BegOfLine 
               THEN (* Pass over the NlBefore. *) 
                 IF ModCmntRef . ModCmntNlAfter 
                 THEN (* Continue at beginning of next line. *)  
                   PtlCharPos := 0 
                 ELSE  
                   PtlCharPos 
                     := EstUtil . WidthSum3
                          ( CurrentLineIndentPos 
                          , ModCmntRef . ModCmntFromPos  
                          , LbeStd . ProjectToLimitedCharNoTyp
                              ( SharedStrings . Length 
                                  ( ModCmntRef . ModCmntStringRef ) 
                              ) 
                          )
                 ; PtlPrevTok := LbeStd . Tok__Cmnt  
                 END (* IF *) 
               ; PtlState := PtlStateTyp . PtlStateInside 
               ; IncEstChild ( EstTravInfo ) 
               ELSE (* Stop at the NlBefore. *) 
                 PtlState := PtlStateTyp . PtlStateDone  
               END (* IF *)  
             ; RETURN (* Skip steps at end. *) 
             ELSE (* Relative, no Nl before. *)  
               PtlCountToPos 
                 ( EstUtil . WidthSumSigned  
                     ( IndentPos 
                         ( Lang 
                         , CurrentLineIndentPos 
                         , FsNodeRef . FsIndentCode 
                         ) 
                     , ModCmntRef . ModCmntFromPos 
                     ) 
                 ) 
             ; PtlState := PtlStateTyp . PtlStateInside 
             END (* IF *) 

          | ModHs . ModCmntTrailingFixedTyp 
          => (* ModCmntFromPos is absolute *) 
            IF PtlCharPos > ModCmntRef . ModCmntFromPos 
            THEN (* Implicit new line before. *) 
              Assert 
                ( PtlPrevTok # LbeStd . Tok__BegOfLine 
                , AFT . A_PtlTfsModCmnt_Implied_Nl_at_BOL
                ) 
            ; PtlState := PtlStateTyp . PtlStateDone  
            ELSE 
              PtlCountToPos ( ModCmntRef . ModCmntFromPos ) 
            ; PtlState := PtlStateTyp . PtlStateInside 
            END (* IF *) 

          | ModHs . ModCmntTrailingRelativeTyp 
          => (* ModCmntFromPos is relative to previous token. *) 
            PtlCountToPos 
              ( EstUtil . WidthSumSigned  
                  ( PtlCharPos , ModCmntRef . ModCmntFromPos ) 
              ) 
          ; PtlState := PtlStateTyp . PtlStateInside 
          END (* TYPECASE *) 

        ; IF PtlState # PtlStateTyp . PtlStateDone 
          THEN 
            PtlCountWidth 
              ( SharedStrings . Length ( ModCmntRef . ModCmntStringRef ) ) 
          ; IF PtlState # PtlStateTyp . PtlStateDone 
            THEN 
              PtlPrevTok := LbeStd . Tok__Cmnt 
            ; IncEstChild ( EstTravInfo ) 
            ; IF ModCmntRef . ModCmntNlAfter 
              THEN 
                PtlState := PtlStateTyp . PtlStateDone  
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END PtlTfsModCmnt 

    ; PROCEDURE PtlTfsModText ( ModTextRef : ModHs . ModTextTyp ) 
      RAISES { Backout } 
      (* PRE: PtlState # PtlStateTyp . PtlStateDone *) 

      = BEGIN (* PtlTfsModText *) 
          IF ModTextRef . ModTextLeftTokToPos = 0 
          THEN (* Nl before. *)   
            IF PtlPrevTok = LbeStd . Tok__BegOfLine 
            THEN (* Pass over the Nl before. *)  
              IF ModTextRef . ModTextToPos = LbeStd . LimitedCharNoInfinity  
              THEN (* Nl after too.  Continue at beginning of next line. *) 
                PtlCharPos := 0 
              ELSE 
                PtlCharPos := ModTextRef . ModTextToPos 
              ; PtlPrevTok := LbeStd . Tok__ModText 
              END (* IF *) 
            ; PtlState := PtlStateTyp . PtlStateInside  
            ; IncEstChild ( EstTravInfo ) 
            ELSE (* Stop at the Nl before. *) 
              PtlState := PtlStateTyp . PtlStateDone  
            END (* IF *)  
          ELSE (* No Nl before. *) 
            ModTextIsToLeftOnLine := TRUE 
          ; PtlPrevTok := LbeStd . Tok__ModText  
          ; IF ModTextRef . ModTextToPos = LbeStd . LimitedCharNoInfinity  
            THEN (* Nl after.  Stop thereat. *) 
              PtlState := PtlStateTyp . PtlStateDone  
            ELSE 
              PtlCountToPos ( ModTextRef . ModTextToPos ) 
            ; PtlState := PtlStateTyp . PtlStateInside  
            ; IncEstChild ( EstTravInfo ) 
            END (* IF *) 
          END (* IF *) 
        END PtlTfsModText 

    ; PROCEDURE PtlTfsLeadingMods 
        ( VAR Delete : BOOLEAN 
          (* ^A delete mod applies to next token. *) 
        ; VAR IsRepair : BOOLEAN 
        ) 
      RAISES { Backout } 
      (* PRE: PtlState # PtlStateTyp . PtlStateDone *) 

      = VAR LModRef : LbeStd . EstRootTyp 
      ; VAR LModDelIsFinished : BOOLEAN 

      ; BEGIN (* PtlTfsLeadingMods *) 
          Delete := FALSE 
        ; LOOP 
            CheckModFwd 
              ( EstTravInfo 
              , FsNodeRef 
              , (* VAR *) LModRef 
              , (* VAR *) LModDelIsFinished 
              ) 
          ; TYPECASE LModRef 
            OF NULL 
            => EXIT 

            (* Deletion mod. *) 
            | ModHs . ModDelTyp 
            => Delete := TRUE 
            ; IsRepair 
                := EstHs . EstChildKindContainsInsertionRepair 
                   IN EstTravInfo . EtiChildLeafElem . LeKindSet 
            ; IF LModDelIsFinished 
               THEN 
                 IncEstChild ( EstTravInfo ) 
               ELSE EXIT 
               END (* IF *) 

            (* Blank line. *) 
            | ModHs . ModBlankLineTyp 
            => IF PtlPrevTok = LbeStd . Tok__BegOfLine 
               THEN (* Pass over the blank line. *) 
                 PtlCharPos := 0 
               ; IncEstChild ( EstTravInfo ) 
               ; PtlState := PtlStateTyp . PtlStateInside 
               ELSE
                 PtlState := PtlStateTyp . PtlStateDone  
               END (* IF *)  

            (* Comment. *) 
            | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
            => PtlTfsModCmnt ( TModCmnt ) 

            (* ModText. *) 
            | ModHs . ModTextTyp ( TModText ) 
            => PtlTfsModText ( TModText ) 

            (* Token insertion. *) 
            | EstHs . EstRefTyp ( TEstRef )  
            => (* Will have EstNodeKindModTok *) 
              IF EstUtil . CharPosPlusWidthInfo 
                   ( PtlCharPos , TEstRef . KTreeWidthInfo ) 
                 > Options . RightMargin 
                 AND ( PtlState # PtlStateTyp . PtlStateStart 
                       OR PtlPrevTok # LbeStd . Tok__BegOfLine  
                     ) 
(* REVIEW: Would just calling PtlTfsEstSubtree do the same thing? *) 
              THEN PtlState := PtlStateTyp . PtlStateDone 
              ELSE 
                PtlTfsEstSubtree ( ) 
              END (* IF *) 

            (* Lex error characters. *) 
            | SharedStrings . T ( TString ) 
            => PtlState := PtlStateTyp . PtlStateInside 
            ; PtlCountFloatingTok 
                ( SharedStrings . Tok ( TString ) , TString ) 
            ; IncEstChild ( EstTravInfo ) 

            (* Error. *) 
            | ModHs . ModErrTyp 
            => IncEstChild ( EstTravInfo ) 
               (* Ignore error *) 

            ELSE (* Not a leading mod. *) 
              EXIT 
            END (* TYPECASE *) 
          ; IF PtlState = PtlStateTyp . PtlStateDone THEN EXIT END (* IF *) 
          END (* LOOP *) 
        END PtlTfsLeadingMods 

    (* Trailing mods: *) 

    ; PROCEDURE PtlTfsTrailingMods ( ) RAISES { Backout } 
      (* PRE: PtlState # PtlStateTyp . PtlStateDone *) 

      = VAR LHangingTrailingMods : BOOLEAN 
            (* There is a trailing mod (for the FmtNo) that won't fit on
               the line.  We have to loop through these to distinguish 
               becoming Done at the NlAfter of the last trailing mod, 
               vs. becoming done earlier than that. *) 

      ; BEGIN (* PtlTfsTrailingMods *) 
          LHangingTrailingMods := FALSE 
        ; LOOP 
            IF EstTravInfo . EtiChildNo >= EstTravInfo . EtiChildCt 
               (* No more Est children at all. *) 
               OR FsNodeRef . FsFmtNo # EstTravInfo . EtiChildFmtNo 
                  (* Next child for a different FmtNo. *) 
            THEN 
              EXIT 
            ELSE (* Found a mod for the current FmtNo. *) 
              TYPECASE EstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => (* Not a trailing mod. *) 
                 EXIT 
              | ModHs . ModCmntTrailingTyp ( TModCmnt ) 
              => LHangingTrailingMods 
                   := LHangingTrailingMods 
                      OR PtlState = PtlStateTyp . PtlStateDone
              ; PtlTfsModCmnt ( TModCmnt ) 
              ELSE (* Not a trailing mod. *) 
                EXIT 
              END (* TYPECASE *) 
            END (* IF *) 
          END (* LOOP *) 
        ; IF NOT LHangingTrailingMods 
          THEN LastFmtNoOnLine := FsNodeRef . FsFmtNo 
          END (* IF *) 
        END PtlTfsTrailingMods 

    (* Format syntax trees *) 

    ; PROCEDURE PtlTfsInsTok ( ) RAISES { Backout } 
      (* PRE: PtlState # PtlStateTyp . PtlStateDone *) 

      = VAR LDelete : BOOLEAN 
      ; VAR LIsRepair : BOOLEAN 

      ; BEGIN (* PtlTfsInsTok *) 
          PtlTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
        ; IF PtlState # PtlStateTyp . PtlStateDone 
          THEN 
            IF NOT LDelete OR LIsRepair 
               (* PreTraverseLine can be called when writing text, in which
                  case repair tokens are not emitted.  But to have consistent
                  formatting, we always consider them present.  Text files 
                  may have more liberal insertion of line breaks than if
                  they were allowed to be inconsistent in this respect with
                  on-screen displays. *)  
            THEN 
              PtlState := PtlStateTyp . PtlStateInside 
            ; PtlCountFloatingTok 
                ( FsNodeRef . FsTok , FsNodeRef . FsInsTokRef ) 
            END (* IF *) 
          ; IF PtlState # PtlStateTyp . PtlStateDone 
            THEN 
              PtlTfsTrailingMods ( ) 
            END (* IF *) 
          END (* IF*) 
        END PtlTfsInsTok 

    ; PROCEDURE PtlTfsEstChild ( ) RAISES { Backout } 
      (* PRE: PtlState # PtlStateTyp . PtlStateDone *) 

      = VAR LDelete : BOOLEAN 
      ; VAR LIsRepair : BOOLEAN 

      ; BEGIN (* PtlTfsEstChild *) 
          PtlTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
        ; IF PtlState # PtlStateTyp . PtlStateDone 
          THEN
            IF EstTravInfo . EtiChildNo < EstTravInfo . EtiChildCt 
               AND EstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
               AND EstHs . EstChildKindEstChild 
                   IN EstTravInfo . EtiChildLeafElem . LeKindSet 
            THEN (* Est child exists. *)  
              IF ISTYPE 
                   ( EstTravInfo . EtiChildLeafElem . LeChildRef
                   , ModHs . EstDummyTyp (* Including NIL. *) 
                   ) 
                  OR EstHs . EstChildKindTrailingSep 
                     IN EstTravInfo . EtiChildLeafElem . LeKindSet 
              THEN (* NIL, dummy, or trailing sep.  Skip. *)  
                IncEstChild ( EstTravInfo ) 
              ; PtlState := PtlStateTyp . PtlStateInside 
              ELSE 
                Assert ( NOT LDelete , AFT . A_PtlTfsEstChild_Deleted ) 
              ; PtlTfsEstSubtree ( ) 
              END (* IF *) 
            ELSE (* Even when descending, this traverser can descend toward an
                    Est child that is absent.  Just act like we found it. *) 
              PtlState := PtlStateTyp . PtlStateInside 
            END (* IF *) 
          ; IF PtlState # PtlStateTyp . PtlStateDone 
            THEN 
              PtlTfsTrailingMods ( ) 
            END (* IF *) 
          END (* IF *) 
        END PtlTfsEstChild 

    ; PROCEDURE PtlTfsInitialFsChildNo ( ) 
      : LangUtil . FsChildNoTyp 

      = BEGIN (* PtlTfsInitialFsChildNo *) 
          CASE PtlState<* NOWARN *> 
          OF PtlStateTyp . PtlStateStart
          => RETURN LangUtil . FsChildNoOfFmtNo ( FsNodeRef , StartFmtNo ) 
          | PtlStateTyp . PtlStateInside 
          => RETURN 0 
          END (* CASE *) 
        END PtlTfsInitialFsChildNo 

    ; PROCEDURE PtlTfsTraverseFsFixedChildren 
        ( FromFsChildNo : LangUtil . FsChildNoTyp ) 
      RAISES { Backout } 
      (* PRE: PtlState # PtlStateTyp . PtlStateDone *) 

      = VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* PtlTfsTraverseFsFixedChildren *) 
          IF FsNodeRef . FsChildren = NIL 
          THEN LFsChildCt := 0 
          ELSE LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ ) 
          END (* IF *) 
        ; LFsChildNo := FromFsChildNo 
        ; LOOP 
            IF LFsChildNo >= LFsChildCt THEN EXIT END (* IF *) 
          ; PtlTraverseFs ( FsNodeRef . FsChildren ^ [ LFsChildNo ] ) 
          ; IF PtlState = PtlStateTyp . PtlStateDone 
            THEN EXIT 
            ELSE 
              INC ( LFsChildNo ) 
            END (* IF *) 
          END (* LOOP *) 
        END PtlTfsTraverseFsFixedChildren 

    ; PROCEDURE PtlTfsTraverseFsListChildren 
        ( FromFsChildNo : LangUtil . FsChildNoTyp ) 
      RAISES { Backout } 
      (* PRE: PtlState # PtlStateTyp . PtlStateDone *) 

      = VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LFsChildNo : LangUtil . FsChildNoTyp 
      ; VAR LRMFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* PtlTfsTraverseFsListChildren *) 
          LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ ) 
        ; LFsChildNo := FromFsChildNo 
        ; IF FsNodeRef . FsKind 
             IN LangUtil . FsKindSetEstListTrail 
                AND EstTravInfo . EtiParentRef . EstNodeKind 
                    = EstHs . EstNodeKindTyp . EstNodeKindTrail
          THEN LRMFsChildNo := LFsChildCt - 1 
          ELSE LRMFsChildNo := 0 
          END (* IF *) 
        ; LOOP 
            PtlTraverseFs ( FsNodeRef . FsChildren ^ [ LFsChildNo ] ) 
          ; IF PtlState = PtlStateTyp . PtlStateDone 
            THEN 
              EXIT 
            ELSIF LFsChildNo = LRMFsChildNo  
                  AND EstTravInfo . EtiChildNo >= EstTravInfo . EtiChildCt 
            THEN 
              EXIT 
            ELSE 
              LFsChildNo := ( LFsChildNo + 1 ) MOD LFsChildCt 
            END (* IF *) 
          END (* LOOP *) 
        END PtlTfsTraverseFsListChildren 

    ; PROCEDURE PtlTfsTraverseCondFmtChildren ( ) 
      RAISES { Backout } 
      (* PRE: PtlState # PtlStateTyp . PtlStateDone *) 

      = VAR LFsChildNo : LangUtil . FsChildNoTyp 
      ; VAR LPredicate : BOOLEAN 

      ; BEGIN (* PtlTfsTraverseCondFmtChildren *) 
          CASE PtlState <* NOWARN *> 
          OF PtlStateTyp . PtlStateStart
          => DescendCondFmt 
               ( Lang 
               , FsNodeRef := FsNodeRef 
               , FmtNo := StartFmtNo 
               , EstTravInfo := EstTravInfo 
               , (* VAR *) Predicate := LPredicate 
               , (* VAR *) FsChildNo := LFsChildNo 
               ) 

          | PtlStateTyp . PtlStateInside 
          => LFsChildNo := 0 
          ; LPredicate := DoCondFmtFwd ( Lang , EstTravInfo , FsNodeRef ) 
          END (* CASE *) 
        ; IF LPredicate 
          THEN 
            PtlTfsTraverseFsFixedChildren ( LFsChildNo ) 
          ELSE 
            PtlTraverseFs ( FsNodeRef . FsCondAltRef ) 
          END (* IF *) 
        END PtlTfsTraverseCondFmtChildren 

    ; BEGIN (* PtlTraverseFs *) 
        VAR LInitialFsChildNo : LangUtil . FsChildNoTyp 
      ; VAR LDelete : BOOLEAN 
      ; VAR LIsRepair : BOOLEAN
 
      ; BEGIN (* Block of PtlTraverseFs *) 
          CASE FsNodeRef . FsKind <* NOWARN *>

          (* Beginning of Image *) 
          OF FsKindTyp . FsKindBegOfImage 
          => PtlState := PtlStateTyp . PtlStateInside  
          ; PtlTfsTrailingMods ( ) (* Probably can't happen. *) 

          (* EndOfImage *) 
          | FsKindTyp . FsKindEndOfImage 
          => PtlTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; Assert ( NOT LDelete , AFT . A_PtlTraverseFs_DeleteEndOfImage ) 
          ; LastFmtNoOnLine := FsNodeRef . FsFmtNo 
          ; PtlState := PtlStateTyp . PtlStateDone 

          (* InsTok. *) 
          | FsKindTyp . FsKindInsTok 
          => PtlTfsInsTok ( ) 

          (* Est child cases. *) 
          | FsKindTyp . FsKindEstChildOfFixed 
          => PtlTfsEstChild ( ) 

          | FsKindTyp . FsKindEstChildOfList 
          => PtlTfsEstChild ( ) 
          ; IF PtlState # PtlStateTyp . PtlStateDone 
            THEN
              INC ( EstListChildrenToPass ) 
            END (* IF *) 

          (* Line breaks. *) 
          | FsKindTyp . FsKindLineBreakOpt 
          , FsKindTyp . FsKindLineBreakReqd  
          => PtlTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; CASE PtlState 
            OF PtlStateTyp . PtlStateStart 
            , PtlStateTyp . PtlStateInside   
            => PtlState := PtlStateTyp . PtlStateInside 
            ; IF NOT LDelete 
              THEN
                IF ModTextIsToLeftOnLine 
                   OR FsNodeRef . FsKind = FsKindTyp . FsKindLineBreakReqd 
                THEN
                  PtlState := PtlStateTyp . PtlStateDone 
                END (* IF *) 
              END (* IF *) 
            ; LastFmtNoOnLine := FsNodeRef . FsFmtNo  
            | PtlStateTyp . PtlStateDone 
            => 
            END (* CASE *) 

          (* Ast string, *) 
          | FsKindTyp . FsKindAstString 
          => (* Usually, these will have been handled from above, at the 
                FsKindEstChildOf{List|Node}.  But we get here if we start 
                with the AstString. *) 
            IF EstTravInfo . EtiNodeRef # NIL
            THEN
              PtlState := PtlStateTyp . PtlStateInside 
            ; PtlCountFloatingTok 
                ( SharedStrings . Tok ( EstTravInfo . EtiStringRef ) 
                , EstTravInfo . EtiStringRef 
                ) 
            ; IF PtlState # PtlStateTyp . PtlStateDone 
              THEN
                LastFmtNoOnLine := FsNodeRef . FsFmtNo 
              END (* IF *) 
            END (* IF *)  

          (* Est fixed and subtree nodes. *) 
          | FsKindTyp . FsKindEstFixedVert 
          , FsKindTyp . FsKindEstFixedHoriz 
          , FsKindTyp . FsKindEstFixedFill 
          , FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill 
          => LInitialFsChildNo := PtlTfsInitialFsChildNo ( )  
          ; PtlTfsTraverseFsFixedChildren ( LInitialFsChildNo ) 

          (* Est list nodes. *) 
          | FsKindTyp . FsKindEstListVert 
          , FsKindTyp . FsKindEstListHoriz 
          , FsKindTyp . FsKindEstListFill 
          , FsKindTyp . FsKindEstListTrailVert 
          , FsKindTyp . FsKindEstListTrailHoriz 
          , FsKindTyp . FsKindEstListTrailFill 
          => LInitialFsChildNo := PtlTfsInitialFsChildNo ( )  
          ; PtlTfsTraverseFsListChildren ( LInitialFsChildNo ) 

          (* Conditional format. *) 
          | FsKindTyp . FsKindCondFmt 
          => PtlTfsTraverseCondFmtChildren ( )     
          END (* CASE *) 
        END (* Block PtlTraverseFs*) 
      END PtlTraverseFs 

    ; BEGIN (* PreTraverseLine *) 
        LastFmtNoOnLine := EstHs . FmtNoNull  
      ; EstListChildrenToPass := 0 
      ; PtlState := PtlStateTyp . PtlStateStart 
      ; Assert 
          ( CharPos # LbeStd . LimitedCharNoUnknown 
          , AFT . A_PreTraverseLine_CharNoUnknown 
          ) 
      ; PtlCharPos := CharPos 
      ; PtlPrevTok := PrevTok 
      (* Negative character positions are possible, from MergeTxt *) 
      ; IF PtlCharPos < 0 
        THEN 
          PtlCharPos := 0 
        ; PtlPrevTok := LbeStd . Tok__BegOfLine
        END 
      ; Assert 
          ( PtlPrevTok # LbeStd . Tok__Unknown 
          , AFT . A_PreTraverseLine_Tok__Unknown 
          ) 
      ; PtlTraverseFs ( RootFsNodeRef ) 
      END PreTraverseLine 

; PROCEDURE EstChildHasZeroWidth 
     ( Lang : LbeStd . LangTyp ; READONLY EstTravInfo : EstTravInfoTyp ) 
  : BOOLEAN 
  RAISES { Backout } 

  = VAR LEstMiscInfo : EstHs . EstMiscInfoTyp 

  ; BEGIN 
      IF EstHs . EstChildKindTrailingSep 
         IN EstTravInfo . EtiChildLeafElem . LeKindSet 
      THEN RETURN TRUE 
      ELSE
        LEstMiscInfo 
          := EstUtil . EstMiscInfo 
               ( Lang , EstTravInfo . EtiChildLeafElem . LeChildRef )  
      ; RETURN LEstMiscInfo . EmiWidthInfo . WiWidth = 0 
      END (* IF *) 
    END EstChildHasZeroWidth 

(* Sigh.  I tried so hard to avoid having two separate traversers for finding
   a nonzero-width item to the left and to the right, but it was just turning 
   into a lot bigger pain that having two separate traversers, and maybe would 
   not have saved that much code anyway.
*) 

; PROCEDURE NonzeroWidthItemIsToRight
    ( Lang : LbeStd . LangTyp 
    ; SubtreeFsNodeRef : LangUtil . FsNodeRefTyp 
      (* ^Not a topmost FsRule node. *) 
    ; <* NOWARN *> VALUE EstTravInfo : EstTravInfoTyp 
      (* ^Yes, I *really* want this passed by value. *) 
    ) 
  : BOOLEAN 
  (* True iff any item of nonzero width is found to the right on the line
     and within the stuff formatted by SubtreeFsNodeRef. *)
  (* PRE: We are descending towards a Nl. *) 
  RAISES { Backout } 

  = TYPE NzwrStateTyp 
         = { NzwrStateDescending 
             (* Descending towards EstTravInfo . EtiChildLeafElem . LeChildRef,
                prior to traversing to its right. *)  
           , NzwrStateFwd 
           , NzwrStateDone 
           } 
  ; VAR NzwrState : NzwrStateTyp 
  ; VAR NzwrCenterFmtNo : EstHs . FmtNoTyp 
  ; VAR NzwrResult : BOOLEAN 

  ; PROCEDURE NzwrTraverseFs ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Backout } 

    = PROCEDURE NzwrTfsEstOrMod ( IsZeroWidth : BOOLEAN ) 
      RAISES { Backout } 

      = BEGIN 
          CASE NzwrState <* NOWARN *> 
          OF NzwrStateTyp . NzwrStateDescending 
          => Assert 
               ( EstTravInfo . EtiChildFmtNo = NzwrCenterFmtNo  
               , AFT . A_NzwTfsEstOrMod_DescendedToWrongPlace 
               ) 
          ; NzwrState := NzwrStateTyp . NzwrStateFwd  
          ; IncEstChild ( EstTravInfo )

          | NzwrStateTyp . NzwrStateFwd 
          => IF IsZeroWidth 
             THEN IncEstChild ( EstTravInfo ) 
             ELSE 
               NzwrResult := TRUE 
             ; NzwrState := NzwrStateTyp . NzwrStateDone 
             END (* IF *) 
          END 
        END NzwrTfsEstOrMod  

    ; PROCEDURE NzwrTfsLeadingMods 
        ( VAR Delete : BOOLEAN 
          (* ^A tok delete mod applies to next tok. *) 
        ; VAR IsRepair : BOOLEAN 
        ) 
      RAISES { Backout } 
      (* PRE: NzwrState # NzwrStateTyp . NzwrStateDone *) 

      = VAR LModRef : LbeStd . EstRootTyp 
      ; VAR LModDelIsFinished : BOOLEAN 

      ; BEGIN (* NzwrTfsLeadingMods *) 
          Delete := FALSE 
        ; LOOP 
            CheckModFwd 
              ( EstTravInfo 
              , FsNodeRef 
              , (* VAR *) LModRef 
              , (* VAR *) LModDelIsFinished 
              ) 
          ; TYPECASE LModRef 
            OF NULL (* Not a mod. *)  
            => EXIT 

            (* Deletion mod. *) 
            | ModHs . ModDelTyp 
            => Delete := TRUE 
            ; IsRepair 
                := EstHs . EstChildKindContainsInsertionRepair 
                   IN EstTravInfo . EtiChildLeafElem . LeKindSet 
            ; IF LModDelIsFinished 
              THEN 
                IncEstChild ( EstTravInfo ) 
              ELSE EXIT 
              END (* IF *) 

            (* Blank line. *) 
            | ModHs . ModBlankLineTyp 
            => NzwrTfsEstOrMod ( IsZeroWidth := TRUE ) 

            (* Comment. *) 
            | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
            => NzwrTfsEstOrMod 
                 ( IsZeroWidth 
                     := TModCmnt . ModCmntNlBefore 
                        AND TModCmnt . ModCmntNlAfter 
                 ) 

            (* ModText. *) 
            | ModHs . ModTextTyp ( TModText )  
            => NzwrTfsEstOrMod
                 ( IsZeroWidth := EstUtil . IsZeroWidthModText ( TModText ) ) 

            (* Token insertion. *) 
            | EstHs . EstRefTyp 
            => NzwrTfsEstOrMod ( IsZeroWidth := FALSE ) 

            (* Lex error characters. *) 
            | SharedStrings . T  
            => Assert 
                 ( NzwrState # NzwrStateTyp . NzwrStateDescending  
                 , AFT . A_NzwrTfsLeadingMods_DescendingToLexErrChars 
                 ) 
            ; NzwrResult := TRUE 
            ; NzwrState := NzwrStateTyp . NzwrStateDone  

            (* Error. *) 
            | ModHs . ModErrTyp 
            => IncEstChild ( EstTravInfo ) 

            ELSE (* Not a leading mod. *)
              EXIT 
            END (* TYPECASE *) 
          ; IF NzwrState = NzwrStateTyp . NzwrStateDone 
            THEN EXIT 
            END (* IF *) 
          END (* LOOP *) 
        END NzwrTfsLeadingMods 

    (* Trailing mods: *) 

    ; PROCEDURE NzwrTfsTrailingMods ( ) RAISES { Backout } 
      (* PRE: NzwrState # NzwrStateTyp . NzwrStateDone *) 

      = BEGIN (* NzwrTfsTrailingMods *) 
          LOOP 
            IF EstTravInfo . EtiChildNo >= EstTravInfo . EtiChildCt 
               (* No more Est children at all. *) 
               OR FsNodeRef . FsFmtNo # EstTravInfo . EtiChildFmtNo 
                  (* Next child is for a different FmtNo. *) 
            THEN 
              EXIT 
            ELSE (* Found a child for the current FmtNo. *) 
              TYPECASE EstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL (* Not a trailing mod. *) 
              => EXIT 

              | ModHs . ModCmntTrailingTyp  
              => NzwrTfsEstOrMod ( IsZeroWidth := FALSE )  
              ; IF NzwrState = NzwrStateTyp . NzwrStateDone 
                THEN EXIT 
                END (* IF *) 

              ELSE (* Not a trailing mod. *) 
                EXIT 
              END (* TYPECASE *) 
            END (* IF *) 
          END (* LOOP *) 
        END NzwrTfsTrailingMods 

    ; PROCEDURE NzwrTfsTraverseFsFixedChildren 
        ( FromFsChildNo : LangUtil . FsChildNoTyp ) 
      RAISES { Backout } 
      (* PRE: NzwrState # NzwrStateTyp . NzwrStateDone *) 

      = VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN 
          IF FsNodeRef . FsChildren = NIL 
          THEN LFsChildCt := 0 
          ELSE LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ ) 
          END (* IF *) 
        ; LFsChildNo := FromFsChildNo 
        ; LOOP 
            IF NzwrState = NzwrStateTyp . NzwrStateDone 
            THEN EXIT
            ELSIF LFsChildNo >= LFsChildCt 
            THEN EXIT 
            ELSE 
              NzwrTraverseFs ( FsNodeRef . FsChildren ^ [ LFsChildNo ] ) 
            ; IF NzwrState = NzwrStateTyp . NzwrStateDone 
              THEN EXIT 
              ELSE INC ( LFsChildNo ) 
              END (* IF *) 
            END (* IF *) 
          END (* LOOP *) 
        END NzwrTfsTraverseFsFixedChildren 

    ; PROCEDURE NzwrTfsTraverseCondFmtChildren ( ) 
      RAISES { Backout } 
      (* PRE: NzwrState # NzwrStateTyp . NzwrStateDone *) 

      = VAR LFsChildNo : LangUtil . FsChildNoTyp 
      ; VAR LPredicate : BOOLEAN 

      ; BEGIN (* NzwrTfsTraverseCondFmtChildren *) 
          CASE NzwrState <* NOWARN *>
          OF NzwrStateTyp . NzwrStateDescending 
          => DescendCondFmt 
               ( Lang 
               , FsNodeRef := FsNodeRef 
               , FmtNo := NzwrCenterFmtNo 
               , EstTravInfo := EstTravInfo 
               , (* VAR *) Predicate := LPredicate 
               , (* VAR *) FsChildNo := LFsChildNo 
               ) 

          | NzwrStateTyp . NzwrStateFwd
          => LFsChildNo := 0 
          ; LPredicate := DoCondFmtFwd ( Lang , EstTravInfo , FsNodeRef ) 
          END (* CASE *) 
        ; IF LPredicate 
          THEN 
            NzwrTfsTraverseFsFixedChildren ( FromFsChildNo := LFsChildNo ) 
(* CHECK: This once started from zero, even when descending.  ^ *) 
          ELSE 
            NzwrTraverseFs ( FsNodeRef . FsCondAltRef ) 
          END (* IF *) 
        END NzwrTfsTraverseCondFmtChildren 

    ; BEGIN (* NzwrTraverseFs *) 
        VAR LDelete : BOOLEAN 
      ; VAR LIsRepair : BOOLEAN
 
      ; BEGIN (* Block of NzwrTraverseFs *) 
          CASE FsNodeRef . FsKind <* NOWARN *>

          (* Beginning of Image *) 
          OF FsKindTyp . FsKindBegOfImage 
          => NzwrTfsTrailingMods ( ) 

          (* EndOfImage *) 
          | FsKindTyp . FsKindEndOfImage 
          => NzwrTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; NzwrState := NzwrStateTyp . NzwrStateDone 

          (* InsTok. *) 
          | FsKindTyp . FsKindInsTok 
          => NzwrTfsLeadingMods 
               ( (* VAR *) LDelete , (* VAR *) LIsRepair )
          ; CASE NzwrState 
            OF NzwrStateTyp . NzwrStateDone => 

            | NzwrStateTyp . NzwrStateDescending 
            => (* We don't care if it's visible or not, just skip it. *) 
              NzwrTfsTrailingMods ( ) 

            | NzwrStateTyp . NzwrStateFwd 
            => IF NOT LDelete OR LIsRepair   
              THEN (* The InsTok is visible. *) 
                NzwrResult := TRUE 
              ; NzwrState := NzwrStateTyp . NzwrStateDone 
              ELSE 
                NzwrTfsTrailingMods ( ) 
              END (* IF *) 
            END (* CASE *) 

          (* Est child cases. *) 
          | FsKindTyp . FsKindEstChildOfFixed 
          , FsKindTyp . FsKindEstChildOfList 
          => NzwrTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; IF NzwrState # NzwrStateTyp . NzwrStateDone 
            THEN 
              Assert 
                ( NOT LDelete 
                , AFT . A_NzwrTraverseFs_Deleted_Est_child 
                ) 
            ; NzwrTfsEstOrMod 
                ( IsZeroWidth := EstChildHasZeroWidth ( Lang , EstTravInfo ) ) 
            ; IF NzwrState # NzwrStateTyp . NzwrStateDone 
              THEN NzwrTfsTrailingMods ( ) 
              END (* IF *) 
            END (* IF *) 

          (* Ast string, *) 
          | FsKindTyp . FsKindAstString 
          => CASE NzwrState <* NOWARN *> 
            OF NzwrStateTyp . NzwrStateDescending  
            => IncEstChild ( EstTravInfo ) 

            | NzwrStateTyp . NzwrStateFwd
            => IF EstTravInfo . EtiNodeRef = NIL 
              THEN IncEstChild ( EstTravInfo ) 
              ELSE 
                NzwrResult := TRUE 
              ; NzwrState := NzwrStateTyp . NzwrStateDone  
              END (* IF *)  
            END (* CASE *) 

          (* Subtree nodes. *) 
          | FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill 
          => CASE NzwrState <* NOWARN *>
             OF NzwrStateTyp . NzwrStateDescending 
             => NzwrTfsTraverseFsFixedChildren 
                  ( LangUtil . FsChildNoOfFmtNo ( FsNodeRef , NzwrCenterFmtNo )
                  )
             | NzwrStateTyp . NzwrStateFwd 
             => NzwrTfsTraverseFsFixedChildren ( FromFsChildNo := 0 ) 
             END (* CASE *) 

          (* Conditional format. *) 
          | FsKindTyp . FsKindCondFmt 
          => NzwrTfsTraverseCondFmtChildren ( )     
          END (* CASE *) 
        END (* Block NzwrTraverseFs*) 
      END NzwrTraverseFs 

  ; BEGIN (* NonzeroWidthItemIsToRight *) 
      NzwrResult := FALSE 
    ; NzwrCenterFmtNo := EstTravInfo . EtiChildFmtNo 
    ; NzwrState := NzwrStateTyp . NzwrStateDescending 
    ; NzwrTraverseFs ( SubtreeFsNodeRef ) 
    ; RETURN NzwrResult 
    END NonzeroWidthItemIsToRight 

; PROCEDURE NonzeroWidthItemIsToLeft
    ( Lang : LbeStd . LangTyp 
    ; SubtreeFsNodeRef : LangUtil . FsNodeRefTyp 
      (* PRE: Must be an FS subtree kind. *) 
    ; <* NOWARN *> VALUE EstTravInfo : EstTravInfoTyp  
      (* ^Yes, I *really* want this passed by value. *) 
    ) 
  : BOOLEAN 
  (* True iff any item of nonzero width is found to the left of 
     (but not including) the current Est child in EstTravInfo, within 
     the line and within the stuff formatted by SubtreeFsNodeRef. *)
  (* PRE: We are descending towards a Nl. *) 
  RAISES { Backout } 

  = TYPE NzwlStateTyp 
         = { NzwlStateDescending 
             (* Descending towards EstTravInfo . EtiChildLeafElem . LeChildRef,
                prior to traversing to its left. *)  
           , NzwlStateBwd (* Inside the left section. *) 
           , NzwlStateDone 
           } 
  ; VAR NzwlState : NzwlStateTyp 
  ; VAR NzwlCenterFmtNo : EstHs . FmtNoTyp 
  ; VAR NzwlResult : BOOLEAN 

  ; PROCEDURE NzwlTraverseFs ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { Backout } 

    = PROCEDURE NzwlTfsEstOrMod ( IsZeroWidth : BOOLEAN ) 
      RAISES { Backout } 

      = BEGIN 
          CASE NzwlState <* NOWARN *> 
          OF NzwlStateTyp . NzwlStateDescending 
          => Assert 
               ( EstTravInfo . EtiChildFmtNo = NzwlCenterFmtNo  
               , AFT . A_NzwTfsEstOrMod_DescendedToWrongPlace 
               ) 
          ; DecEstChild ( EstTravInfo ) 
          ; NzwlState := NzwlStateTyp . NzwlStateBwd 

          | NzwlStateTyp . NzwlStateBwd
          => IF IsZeroWidth 
             THEN DecEstChild ( EstTravInfo ) 
             ELSE 
               NzwlResult := TRUE 
             ; NzwlState := NzwlStateTyp . NzwlStateDone 
             END (* IF *) 
          END 
        END NzwlTfsEstOrMod  

    ; PROCEDURE NzwlTfsLeadingModsNoDel ( ) 
      RAISES { Backout } 
      (* PRE: NzwlState # NzwlStateTyp . NzwlStateDone *) 

      = BEGIN (* NzwlTfsLeadingModsNoDel *) 
          LOOP 
            IF ( EstTravInfo . EtiChildNo < 0 ) 
               (* No more Est children at all. *) 
               OR ( FsNodeRef . FsFmtNo # EstTravInfo . EtiChildFmtNo ) 
                  (* Next child is for a different FmtNo *) 
            THEN 
              EXIT 
            END (* IF *) 
          ; TYPECASE EstTravInfo . EtiChildLeafElem . LeChildRef 
            OF NULL (* No mod remains. *)  
            => EXIT 

            (* Blank line. *) 
            | ModHs . ModBlankLineTyp 
            => NzwlTfsEstOrMod ( IsZeroWidth := TRUE ) 

            (* Comment. *) 
            | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
            => NzwlTfsEstOrMod 
                 ( IsZeroWidth 
                     := TModCmnt . ModCmntNlBefore 
                        AND  TModCmnt . ModCmntNlAfter 
                 ) 

            (* ModText. *) 
            | ModHs . ModTextTyp ( TModText ) 
            => NzwlTfsEstOrMod
                 ( IsZeroWidth := EstUtil . IsZeroWidthModText ( TModText ) ) 

            (* Token insertion. *) 
            | EstHs . EstRefTyp 
            => NzwlTfsEstOrMod ( IsZeroWidth := FALSE ) 

            (* Lex error characters. *) 
            | SharedStrings . T 
            => Assert 
                 ( NzwlState # NzwlStateTyp . NzwlStateDescending  
                 , AFT . A_NzwlTfsLeadingModsNoDel_DescendingToLexErrChars 
                 ) 
            ; NzwlResult := TRUE 
            ; NzwlState := NzwlStateTyp . NzwlStateDone  

            (* Error. *) 
            | ModHs . ModErrTyp 
            => DecEstChild ( EstTravInfo ) 

            (* Impossible. *) 
            | ModHs . ModDelTyp 
            , ModHs . ModCmntTrailingTyp 
            => CantHappen ( AFT . A_NzwlTfsLeadingModsNoDel_Bad_mod_kind ) 
               (* These should have been found earlier. *) 

            ELSE (* A ModDel, or not a leading mod. *)
              EXIT 
            END (* TYPECASE *) 
          ; IF NzwlState = NzwlStateTyp . NzwlStateDone 
            THEN EXIT 
            END (* IF *) 
          END (* LOOP *) 
        END NzwlTfsLeadingModsNoDel 

    ; PROCEDURE NzwlTfsCheckLeadingDel 
        ( VAR Delete : BOOLEAN ; VAR IsRepair : BOOLEAN ) 
      RAISES { Backout } 
      (* Caller must ensure only a deletable FsNode is current. *) 

      = BEGIN (* NzwlTfsCheckLeadingDel *) 
          Delete := FALSE 
        ; IsRepair := FALSE 
        ; IF 0 <= EstTravInfo . EtiChildNo 
          THEN (* There is a tree child. *) 
            TYPECASE EstTravInfo . EtiChildLeafElem . LeChildRef 
            OF NULL (* It's not a mod. *)  
            => 

            | ModHs . ModDelTyp ( TModDel ) 
            => (* A Deletion mod is next. *) 
               Assert 
                 ( FsNodeRef . FsIsInsideList 
                   OR FsNodeRef . FsFmtNo 
                      >= EstTravInfo . EtiChildFmtNo 
                 , AFT . A_NzwlTfsCheckLeadingDel_UnconsumedModDel 
                 ) 
            ; IF FsNodeRef . FsFmtNo <= TModDel . ModDelThruFmtNo 
              THEN (* Delete mod applies. *) 
                Delete := TRUE 
              ; IsRepair 
                  := EstHs . EstChildKindContainsInsertionRepair 
                    IN EstTravInfo . EtiChildLeafElem . LeKindSet 
              ; IF FsNodeRef . FsFmtNo = EstTravInfo . EtiChildFmtNo 
                THEN (* We are at the leftmost deleted token, so 
                        consume the ModDel. *) 
                  DecEstChild ( EstTravInfo ) 
                END (* IF *) 
              END (* IF *) 
            ELSE (* Not a Deletion mod. *)  
            END (* TYPECASE *) 
          END (* IF *) 
        END NzwlTfsCheckLeadingDel 

    (* Trailing mods: *) 

    ; PROCEDURE NzwlTfsTrailingMods ( ) RAISES { Backout } 
      (* PRE: NzwlState # NzwlStateTyp . NzwlStateDone *) 

      = BEGIN (* NzwlTfsTrailingMods *) 
          LOOP 
            IF 0 <= EstTravInfo . EtiChildNo 
               (* No more Est children at all. *) 
               OR FsNodeRef . FsFmtNo # EstTravInfo . EtiChildFmtNo 
                  (* Next child is for a different FmtNo. *) 
            THEN 
              EXIT 
            ELSE (* Found a child for the current FmtNo. *) 
              TYPECASE EstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL (* Not a trailing mod. *) 
              => EXIT 

              | ModHs . ModCmntTrailingTyp  
              => NzwlTfsEstOrMod ( IsZeroWidth := FALSE )  
              ; IF NzwlState = NzwlStateTyp . NzwlStateDone 
                THEN EXIT 
                END (* IF *) 

              ELSE (* Not a trailing mod. *) 
                EXIT 
              END (* TYPECASE *) 
            END (* IF *) 
          END (* LOOP *) 
        END NzwlTfsTrailingMods 

    ; PROCEDURE NzwlTfsTraverseFsFixedChildren 
        ( FromFsChildNo : LangUtil . FsChildNoSignedTyp ) 
      RAISES { Backout } 
      (* PRE: NzwlState # NzwlStateTyp . NzwlStateDone *) 

      = VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LFsChildNo : LangUtil . FsChildNoSignedTyp 

      ; BEGIN 
          IF FsNodeRef . FsChildren = NIL 
          THEN LFsChildCt := 0 
          ELSE LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ ) 
          END (* IF *) 
        ; LFsChildNo := FromFsChildNo   
        ; LOOP 
            IF NzwlState = NzwlStateTyp . NzwlStateDone 
            THEN EXIT
            ELSIF LFsChildNo < 0  
            THEN EXIT 
            ELSE 
              NzwlTraverseFs ( FsNodeRef . FsChildren ^ [ LFsChildNo ] ) 
            ; IF NzwlState = NzwlStateTyp . NzwlStateDone 
              THEN EXIT 
              ELSE 
                DEC ( LFsChildNo ) 
              END (* IF *) 
            END (* IF *) 
          END (* LOOP *) 
        END NzwlTfsTraverseFsFixedChildren 

    ; PROCEDURE NzwlTfsTraverseCondFmtChildren ( ) 
      RAISES { Backout } 
      (* PRE: NzwlState # NzwlStateTyp . NzwlStateDone *) 

      = VAR LFsChildNo : LangUtil . FsChildNoTyp 
      ; VAR LPredicate : BOOLEAN 

      ; BEGIN (* NzwlTfsTraverseCondFmtChildren *) 
          CASE NzwlState <* NOWARN *>
          OF NzwlStateTyp . NzwlStateDescending 
          => DescendCondFmt 
               ( Lang 
               , FsNodeRef := FsNodeRef 
               , FmtNo := NzwlCenterFmtNo 
               , EstTravInfo := EstTravInfo 
               , (* VAR *) Predicate := LPredicate 
               , (* VAR *) FsChildNo := LFsChildNo 
               , Bwd := TRUE 
               ) 

          | NzwlStateTyp . NzwlStateBwd
          => LFsChildNo := NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
          ; LPredicate := DoCondFmtBwd ( Lang , EstTravInfo , FsNodeRef ) 
          END (* CASE *) 
        ; IF LPredicate 
          THEN 
            NzwlTfsTraverseFsFixedChildren ( LFsChildNo ) 
(* CHECK: This once started at the right end, ^ even if descending. *) 
          ELSE 
            NzwlTraverseFs ( FsNodeRef . FsCondAltRef ) 
          END (* IF *) 
        END NzwlTfsTraverseCondFmtChildren 

    ; BEGIN (* NzwlTraverseFs *) 
        VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LDelete : BOOLEAN 
      ; VAR LIsRepair : BOOLEAN
 
      ; BEGIN (* Block of NzwlTraverseFs *) 
          CASE FsNodeRef . FsKind <* NOWARN *>

          (* Beginning of Image *) 
          OF FsKindTyp . FsKindBegOfImage 
          => NzwlTfsTrailingMods ( ) 
          ; NzwlState := NzwlStateTyp . NzwlStateDone 

          (* EndOfImage *) 
          | FsKindTyp . FsKindEndOfImage 
          => NzwlTfsCheckLeadingDel 
               ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; Assert 
              ( NOT LDelete 
              , AFT . A_NzwTraverseFs_DeletedEOI 
              ) 
          ; NzwlTfsLeadingModsNoDel ( ) 

          (* InsTok. *) 
          | FsKindTyp . FsKindInsTok 
          => NzwlTfsTrailingMods ( ) 
          ; CASE NzwlState 
            OF NzwlStateTyp . NzwlStateDone => 

            | NzwlStateTyp . NzwlStateDescending 
            => NzwlTfsCheckLeadingDel 
                 ( (* VAR *) LDelete , (* VAR *) LIsRepair )
              (* We don't care if it's visible or not, just skip it. *) 
            ; NzwlTfsLeadingModsNoDel () 

            | NzwlStateTyp . NzwlStateBwd 
            => NzwlTfsCheckLeadingDel 
                 ( (* VAR *) LDelete , (* VAR *) LIsRepair )
            ; IF NOT LDelete OR LIsRepair   
              THEN (* The InsTok is visible. *) 
                NzwlResult := TRUE 
              ; NzwlState := NzwlStateTyp . NzwlStateDone 
              ELSE 
                NzwlTfsLeadingModsNoDel ( ) 
              END (* IF *) 
            END (* CASE *) 

          (* Est child cases. *) 
          | FsKindTyp . FsKindEstChildOfFixed 
          , FsKindTyp . FsKindEstChildOfList 
          => NzwlTfsTrailingMods ( ) 
          ; IF NzwlState # NzwlStateTyp . NzwlStateDone 
            THEN 
              NzwlTfsEstOrMod 
                ( IsZeroWidth := EstChildHasZeroWidth ( Lang , EstTravInfo ) ) 
            ; IF NzwlState # NzwlStateTyp . NzwlStateDone 
              THEN 
                NzwlTfsCheckLeadingDel 
                  ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
              ; Assert 
                  ( TRUE OR NOT LDelete 
                  , AFT . A_NzwTraverseFs_Deleted_Est_child 
                  ) 
              ; NzwlTfsTrailingMods ( ) 
              END (* IF *) 
            END (* IF *) 

          (* Line breaks. *) 
          | FsKindTyp . FsKindLineBreakOpt 
          , FsKindTyp . FsKindLineBreakReqd  
          => NzwlTfsCheckLeadingDel 
               ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; NzwlTfsLeadingModsNoDel ( ) 
          (* If the line break is taken, it is part of the group of Nls and 
             whole-line mods we are starting at.  Either way, ignore it. *)

          (* Ast string, *) 
          | FsKindTyp . FsKindAstString 
          => CASE NzwlState <* NOWARN *> 
            OF NzwlStateTyp . NzwlStateDescending  
            => DecEstChild ( EstTravInfo ) 

            | NzwlStateTyp . NzwlStateBwd
            => IF EstTravInfo . EtiNodeRef = NIL 
              THEN DecEstChild ( EstTravInfo ) 
              ELSE 
                NzwlResult := TRUE 
              ; NzwlState := NzwlStateTyp . NzwlStateDone  
              END (* IF *)  
            END (* CASE *) 

          (* Subtree nodes. *) 
          | FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill 
          => CASE NzwlState <* NOWARN *> 
             OF NzwlStateTyp . NzwlStateDescending 
             => NzwlTfsTraverseFsFixedChildren 
                  ( LangUtil . FsChildNoOfFmtNo 
                      ( FsNodeRef , NzwlCenterFmtNo ) 
                  ) 

             | NzwlStateTyp . NzwlStateBwd
             => IF FsNodeRef . FsChildren = NIL 
                THEN LFsChildCt := 0 
                ELSE LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ ) 
                END (* IF *) 
             ; NzwlTfsTraverseFsFixedChildren ( LFsChildCt - 1 ) 
             END (* CASE *) 

          (* Conditional format. *) 
          | FsKindTyp . FsKindCondFmt 
          => NzwlTfsTraverseCondFmtChildren ( )     
          END (* CASE *) 
        END (* Block NzwlTraverseFs*) 
      END NzwlTraverseFs 

  ; BEGIN (* NonzeroWidthItemIsToLeft *) 
      NzwlResult := FALSE 
    ; NzwlCenterFmtNo := EstTravInfo . EtiChildFmtNo 
    ; NzwlState := NzwlStateTyp . NzwlStateDescending 
    ; NzwlTraverseFs ( SubtreeFsNodeRef ) 
    ; RETURN NzwlResult 
    END NonzeroWidthItemIsToLeft

; PROCEDURE PreTraversedFsSubtreeFits 
    ( SubtreeFsNodeRef : LangUtil . FsNodeRefTyp 
    ; LastFmtNoOnLine : EstHs . FmtNoTyp 
    ; EstListChildrenToPass : LbeStd . EstChildNoTyp 
    ) 
  : BOOLEAN 

  = VAR LFits : BOOLEAN 

  ; BEGIN (* PreTraversedFsSubtreeFits *) 
      IF LastFmtNoOnLine = EstHs . FmtNoNull 
      THEN (* Not even one FmtNo's worth of stuff fits. *) 
        RETURN FALSE 
      ELSE 
        LFits := SubtreeFsNodeRef . FsRightFmtNo <= LastFmtNoOnLine  
      ; IF SubtreeFsNodeRef . FsIsInsideList
           AND ( SubtreeFsNodeRef . FsRightFmtNo = EstHs . FmtNoListEstChild
                 OR SubtreeFsNodeRef . FsLeftFmtNo = EstHs . FmtNoListEstChild
                 OR SubtreeFsNodeRef . FsLeftFmtNo 
                    > SubtreeFsNodeRef . FsRightFmtNo 
               ) 
        THEN (* The Fs subtree covers the Est list child. *)  
          LFits := LFits AND EstListChildrenToPass > 0 
        END (* IF *)
      ; RETURN LFits
      END (* IF *) 
    END PreTraversedFsSubtreeFits 

(* EXPORTED: *) 
; PROCEDURE FmtKindForEstDescending 
    ( FsKind : FsKindTyp 
      (* ^Must be in LangUtil . FsKindSetFsRoot *) 
    ; ParentFmtKind : LangUtil . FmtKindTyp 
      (* ^Can be Unknown.  An optional help, and only if it's FmtKindHoriz. *) 
    ; FirstLineIndentPos : LbeStd . LimitedCharNoTyp (* For EstRef. *) 
    ; EstRef : LbeStd . EstRootTyp 
    ; READONLY StartMark : Marks . TokMarkTyp 
      (* ^Used only if StartMarkIsKnownNl. *) 
    ; StartMarkIsKnownNl : BOOLEAN 
      (* ^FALSE merely means we don't know. *)
    ) 
  : LangUtil . FmtKindTyp 
  RAISES { Backout } 
  (* Use when descending into an Est, toward a line mark. *)

  = VAR LResultIfNotHoriz : LangUtil . FmtKindTyp 

  ; BEGIN (* FmtKindForEstDescending *) 
      IF ParentFmtKind = LangUtil . FmtKindTyp . FmtKindHoriz 
      THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz  
      ELSE 
        TYPECASE EstRef 
        OF ModHs . ModRefTyp (* Including NIL *)  
        , SharedStrings . T 
        => (* These can never contain line breaks, so the result is dead. *)
           RETURN LangUtil . FmtKindTyp . FmtKindHoriz 

        | EstHs . EstRefTyp ( TEstRef )  
        => CASE FsKind <* NOWARN *>
           OF FsKindTyp . FsKindInsTok 
           , FsKindTyp . FsKindAstString 
           (* FsKindInsTok and FsKindAstString could occur as the root of 
              the Fs tree for a ModTok, if we ever make these trees have no 
              FsKindEstFixed* node on top.  Since such trees cannot 
              have line breaks, the FmtKind will be dead here. *) 
           => RETURN LangUtil . FmtKindTyp . FmtKindHoriz 

           | FsKindTyp . FsKindEstFixedVert 
           , FsKindTyp . FsKindEstListVert
           , FsKindTyp . FsKindEstListTrailVert 
             (* If one of these has no line breaks at all (there are none in the
                Fs rule), or all are deleted, we will let code below give it
                FmtKindHoriz, which will speed computation of FmtKind for its
                descendents. *) 
           => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindVert 

           | FsKindTyp . FsKindEstFixedHoriz 
           , FsKindTyp . FsKindEstListHoriz 
           , FsKindTyp . FsKindEstListTrailHoriz 
           => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindVert 

           | FsKindTyp . FsKindEstFixedFill 
           , FsKindTyp . FsKindEstListFill  
           , FsKindTyp . FsKindEstListTrailFill 
           => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindFill  
           END (* CASE *)

        ; IF TEstRef . KTreeWidthInfo . WiWidth > Options . RightMargin 
          THEN (* This includes infinite width, which in turn includes 
                  containment of any properly interior Nl of a mod.  If WiWidth
                  is relative, it is conservatively correct in assuming 
                  CharPos = 0 *) 
            RETURN LResultIfNotHoriz 

          (* Hereafter, there is no properly interior Nl of a mod in EstRef. *) 
          ELSIF TEstRef . KTreeWidthInfo . WiHasNlBefore 
          THEN (* Est begins on an unconditional Nl.  CharPos is unneeded. *)
            IF TEstRef . KTreeWidthInfo . WiHasAbsFromPos 
            THEN (* WiHasNlBefore precludes its being triggered, and WiWidth,
                    previously found to be <= RightMargin, is the absolute
                    ending position. *)
              RETURN LangUtil . FmtKindTyp . FmtKindHoriz  
            ELSE (* WiHasNlBefore AND NOT WiHasAbsFromPos.  This can happen when
                    the Est starts with a BolRelativeCmnt.  WidthInfo could 
                    still have a NlTrigger, if something with an absolute
                    from-position follows the comment. *) 
              IF FirstLineIndentPos = LbeStd . LimitedCharNoUnknown 
              THEN (* This should no longer be possible. *) 
                RETURN LangUtil . FmtKindTyp . FmtKindUnknown 
              ELSIF EstUtil . CharPosPlusWidthInfo (* No separator needed. *) 
                      ( FirstLineIndentPos , TEstRef . KTreeWidthInfo ) 
                    <= Options . RightMargin 
              THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz  
              ELSE RETURN LResultIfNotHoriz 
              END (* IF *) 
            END (* IF *) 
          ELSIF StartMarkIsKnownNl 
          THEN (* From above, no Nl before. *)
            IF StartMark . Kind IN Marks . MarkKindSetFmtNo 
            THEN (* Descending towards a line break, which implies it is taken,
                    and all containing Est & Fs Subtrees are non-horizontal. *)
              RETURN LResultIfNotHoriz 

            ELSE (* The Nl we are descending to can only be at the end of the 
                  Est, including inside a zero-width item. *) 
              Assert 
                ( 
TRUE OR          
(* FIXME: ^Restore this.  Temporary for removing redundant trailing seps. *) 
                 TEstRef . KTreeWidthInfo . WiHasNlAfter 
                , AFT . A_FmtKindForEstDescending_no_Nl_anywhere 
                ) 
            ; RETURN LangUtil . FmtKindTyp . FmtKindUnknown (* Dead *) 
            END (* CASE *) 
          ELSE 
            RETURN LangUtil . FmtKindTyp . FmtKindUnknown (* Possibly live. *)
          END (* IF *) 

        ELSE RETURN LangUtil . FmtKindTyp . FmtKindHoriz (* No doubt dead. *) 
        END (* TYPECASE *) 
      END (*( IF *) 
    END FmtKindForEstDescending 

(* EXPORTED: *) 
; PROCEDURE FmtKindForEstTraversing 
    ( Lang : LbeStd . LangTyp 
    ; CharPos : LbeStd . LimitedCharNoSignedTyp 
      (* ^Can be unknown, and this could propagate to result. *)  
    ; ModTextIsToLeftOnLine : BOOLEAN 
      (* ^Irrelevant if CharPos = LbeStd . LimitedCharNoUnknown. *) 
    ; PrevTok : LbeStd . TokTyp 
    ; FsKind : FsKindTyp 
      (* ^Must be in LangUtil . FsKindSetFsRoot *) 
    ; ParentFmtKind : LangUtil . FmtKindTyp 
    ; FirstLineIndentPos : LbeStd . LimitedCharNoTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ) 
  : LangUtil . FmtKindTyp 
  RAISES { Backout } 
  (* Use when traversing into the Est from one end or the other. *) 
(* CHECK: Works Bwd?  Do we need it to? *) 

  = VAR LResultIfNotHoriz : LangUtil . FmtKindTyp 
  ; VAR LStartPos : LbeStd . LimitedCharNoTyp 

  ; BEGIN (* FmtKindForEstTraversing *) 
      IF ParentFmtKind = LangUtil . FmtKindTyp . FmtKindHoriz 
      THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz  
      ELSE 
        TYPECASE EstRef 
        OF ModHs . ModRefTyp (* Including NIL *)  
           , SharedStrings . T 
        => (* These can't contain line breaks, so the result will be dead. *) 
           RETURN LangUtil . FmtKindTyp . FmtKindHoriz 

        | EstHs . EstRefTyp ( TEstRef )  
        => CASE FsKind <* NOWARN *>
           OF FsKindTyp . FsKindInsTok 
           , FsKindTyp . FsKindAstString 
          (* FsKindInsTok and FsKindAstString could occur as the root of the Fs 
             tree for a ModTok, if we make these trees have no 
             FsKindEstFixed node on top.  Since such trees cannot 
             contain line breaks, the FmtKind will be dead here. *) 
           => RETURN LangUtil . FmtKindTyp . FmtKindHoriz 

           | FsKindTyp . FsKindEstListVert 
           , FsKindTyp . FsKindEstFixedVert 
           , FsKindTyp . FsKindEstListTrailVert 
             (* If one of these has no line breaks at all (there are none in the
                Fs rule), or all are deleted, we will let code below give it
                FmtKindHoriz, which will speed computation of FmtKind for its
                descendents. *) 
           => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindVert 

           | FsKindTyp . FsKindEstFixedHoriz 
           , FsKindTyp . FsKindEstListHoriz 
           , FsKindTyp . FsKindEstListTrailHoriz 
           => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindVert 

           | FsKindTyp . FsKindEstFixedFill 
           , FsKindTyp . FsKindEstListFill  
           , FsKindTyp . FsKindEstListTrailFill 
           => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindFill  
           END (* CASE *) 

        ; IF TEstRef . KTreeWidthInfo . WiWidth > Options . RightMargin 
          THEN (* This includes infinite width, which in turn includes 
                  containment of any properly interior Nl of a Mod.  If 
                  WiWidth is relative, it is conservatively correct in assuming
                  CharPos = 0 *) 
            RETURN LResultIfNotHoriz 

          (* Hereafter, we have ruled out a properly interior Nl. *) 
          ELSIF TEstRef . KTreeWidthInfo . WiHasNlBefore 
          THEN (* The Est begins with an unconditional new line, so CharPos
                  is unneeded. *) 
            IF TEstRef . KTreeWidthInfo . WiHasAbsFromPos 
            THEN (* WiHasNlBefore precludes its being triggered, so WiWidth
                    is the absolute ending position, and we previously found
                    that that fits. *) 
              RETURN LangUtil . FmtKindTyp . FmtKindHoriz 
            ELSE (* WiHasNlBefore AND NOT WiHasAbsFromPos.  This can happen when
                    the Est starts with a BolRelativeCmnt.  WidthInfo could 
                    still have a NlTrigger, if something with an absolute
                    from-position follows the comment. *)
              IF FirstLineIndentPos = LbeStd . LimitedCharNoUnknown 
              THEN (* This should no longer be possible. *) 
                RETURN LangUtil . FmtKindTyp . FmtKindUnknown 
              ELSE 
                IF EstUtil . CharPosPlusWidthInfo (* No separator needed. *) 
                     ( FirstLineIndentPos , TEstRef . KTreeWidthInfo ) 
                   <= Options . RightMargin 
                THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz 
                ELSE RETURN LResultIfNotHoriz
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          ELSIF ModTextIsToLeftOnLine 
                (* From above, Est does not start with an initial Nl or 
                   zero-width item. *) 
          THEN RETURN LResultIfNotHoriz 
               (* ^ In this case, things could have shifted left or right when
                    the ModText was inserted, and CharPos no longer can be
                    relied on to duplicate whether the Est did fit before it was
                    inserted.  But all the line breaks left of the ModText
                    will have been deleted, so we can treat it as not fitting.
                    If TEstRef does contain any undeleted line breaks, this 
                    assumption will be justified.  If not, the FmtKind will
                    be dead. 
               *) 
          ELSIF CharPos = LbeStd . LimitedCharNoUnknown 
          THEN (* When traversing forward, this should not happen. *) 
            RETURN LangUtil . FmtKindTyp . FmtKindUnknown 
          ELSE
            IF FirstLineIndentPos > CharPos 
            THEN LStartPos := FirstLineIndentPos 
            ELSE 
              LStartPos 
                := EstUtil . WidthSum 
                     ( CharPos 
                     , ORD 
                         ( LangUtil . NeedsSep 
                             ( Lang , PrevTok , TEstRef . EstLeftTok )
                         ) 
                     ) 
            END (* IF *)
          ; IF EstUtil . CharPosPlusWidthInfo 
                 ( LStartPos , TEstRef . KTreeWidthInfo ) 
                 (* Any separator already taken into account. *)  
               <= Options . RightMargin 
            THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz 
            ELSE RETURN LResultIfNotHoriz
            END (* IF *) 
          END (* IF *) 

        ELSE RETURN LangUtil . FmtKindTyp . FmtKindHoriz (* No doubt dead. *) 
        END (* TYPECASE *) 
      END (* IF *) 
    END FmtKindForEstTraversing 

(* EXPORTED: *) 
; PROCEDURE FmtKindForFsSubtreeDescending   
    ( Lang : LbeStd . LangTyp 
    ; RootFsNodeRef : LangUtil . FsNodeRefTyp
      (* PRE: RootFsNodeRef.FsKind IN TravUtil.FsKindSetFsRoot. *)
    ; SubtreeFsNodeRef : LangUtil . FsNodeRefTyp 
      (* PRE: Must be an FS subtree kind. *) 
    ; ParentFmtKind : LangUtil . FmtKindTyp 
    ; IndentPosIfVert : LbeStd . LimitedCharNoTyp 
      (* ^Within the parent Est, assuming it is vertical. *) 
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ; READONLY StartMark : Marks . TokMarkTyp 
      (* ^Used only if StartMarkIsKnownNl. *) 
    ; StartMarkIsKnownNl : BOOLEAN 
      (* ^FALSE merely means we don't know. *)
    ) 
  : LangUtil . FmtKindTyp 
  RAISES { Backout } 
  (* Use when descending into an FsSubtree, preferably toward a line mark. *)

  = VAR LResultIfNotHoriz : LangUtil . FmtKindTyp 
  ; VAR LEstChildMiscInfo : EstHs . EstMiscInfoTyp 
  ; VAR LLastFmtNoOnLine : EstHs . FmtNoTyp 
  ; VAR LEstListChildrenToPass : LbeStd . EstChildNoTyp 

  ; BEGIN (* FmtKindForFsSubtreeDescending *) 
      IF ParentFmtKind = LangUtil . FmtKindTyp . FmtKindHoriz 
      THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz  
      ELSE 
        CASE SubtreeFsNodeRef . FsKind <* NOWARN *>
        OF FsKindTyp . FsKindSubtreeVert
        => (* If one of these has no line breaks at all (there are none in the
              Fs rule), or all are deleted, we will let code below give it
              FmtKindHoriz, which will speed computation of FmtKind for its
              descendents. *) 
          LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindVert 

        | FsKindTyp . FsKindSubtreeHoriz 
        => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindVert 

        | FsKindTyp . FsKindSubtreeFill 
        => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindFill  
        END (* CASE *) 

      ; IF StartMarkIsKnownNl 
        THEN 
          IF StartMark . Kind IN Marks . MarkKindSetFmtNo 
          THEN (* Descending towards an undeleted line break, which implies it
          is taken, and all containing Est & Fs Subtrees are non-horizontal. *)
            RETURN LResultIfNotHoriz 
          ELSE (* We are descending towards a Nl of a Mod, somewhere in the 
                  current Est child subtree. *)
(* CHEDK: Could it be a zero-width mod? *)
            LEstChildMiscInfo 
              := EstUtil . EstMiscInfo 
                   ( Lang , EstTravInfo . EtiChildLeafElem . LeChildRef ) 
          ; IF LEstChildMiscInfo . EmiWidthInfo . WiWidth > Options . RightMargin 
            THEN (* This case includes infinite width, which in turn includes 
                    containment of any properly interior Nl in the 
                    descended-into Est child.  If WiWidth is relative, this is 
                    conservatively correct in assuming CharPos = 0. *) 
              RETURN LResultIfNotHoriz 
            ELSE (* The descended-to Est *child* would fit on a line, starting
                    at zero.  This implies all Nls within are at one end or 
                    the other. *)  
              IF LEstChildMiscInfo . EmiWidthInfo . WiHasNlBefore
              THEN 
                IF LEstChildMiscInfo . EmiWidthInfo . WiHasNlAfter 
                THEN (* WiHasNlBefore AND WiHasNlAfter. *)  
                  IF LEstChildMiscInfo . EmiWidthInfo . WiWidth = 0 
                  THEN (* Est child contains only whole-line mods: 
                          [Nl, whole-line-mod*, Nl] *) 
                    IF NonzeroWidthItemIsToLeft 
                         ( Lang , SubtreeFsNodeRef , EstTravInfo )
                       (* i.e., left of the Est child. *) 
                    THEN (* FsSubtree: Stuff, [Nl, zero-width, Nl], ? *) 
                      RETURN LResultIfNotHoriz
                      (* ^Correct if NonzeroWidthItemIsToRight, else dead. *) 
                    ELSE (* FsSubtree: empty, [Nl, whole-line-mod*, Nl], ?  
                            Must & can pretraverse, starting at left of the 
                            descended-into Est child, which is at the left of 
                            the FsSubtree. *)
                    (* NOTE on IndentPosIfVert: This is triiicky.  If we are in
                       a subsequent line assuming vertical, but it's actually
                       fill, the descended-into Est subtree's having a Nl will
                       ensure that filling will take the same line break that
                       vertical did.  All other uses here of IndentPosIfVert
                       have this property. *)  
                      PreTraverseLine 
                        ( Lang 
                        , ModTextIsToLeftOnLine := FALSE 
                        , RootFsNodeRef := RootFsNodeRef 
                        , EstTravInfo := EstTravInfo 
                        , StartFmtNo := EstTravInfo . EtiChildFmtNo 
                        , CharPos := 0 
                        , CurrentLineIndentPos := IndentPosIfVert 
                        , PrevTok := LbeStd . Tok__BegOfLine 
                        , (* VAR *) LastFmtNoOnLine := LLastFmtNoOnLine 
                        , (* VAR *) EstListChildrenToPass 
                             := LEstListChildrenToPass 
                        ) 
                    ; IF PreTraversedFsSubtreeFits 
                           ( SubtreeFsNodeRef 
                           , LLastFmtNoOnLine 
                           , LEstListChildrenToPass 
                           ) 
                      THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz  
                      ELSE RETURN LResultIfNotHoriz 
                      END (* IF *) 
                    END (* IF *) 
                  ELSE (* Descended-into child Est has nonzero width:
                          [Nl, stuff, Nl] *) 
                    IF NonzeroWidthItemIsToLeft 
                         ( Lang , SubtreeFsNodeRef , EstTravInfo )
                       OR NonzeroWidthItemIsToRight 
                            ( Lang , SubtreeFsNodeRef , EstTravInfo )
                    THEN (* In combination with WiWidth > 0,
                            FsSubtree: empty, [Nl, stuff, Nl], stuff 
                                    or stuff, [Nl, stuff, Nl], empty 
                                    or stuff, [Nl, stuff, Nl], stuff *) 
                      RETURN LResultIfNotHoriz 
                    ELSE (* FsSubtree: empty, [ Nl, stuff, Nl], empty.
                            Only the interior of the Est child matters. *) 
                      IF LEstChildMiscInfo . EmiWidthInfo . WiNlTrigger 
                         # LbeStd . LimitedCharNoInfinity
                         (* Absolute width, following a Nl.  We already 
                            verified that this fits. *) 
                         OR EstUtil . CharPosPlusWidthInfo 
                              ( IndentPosIfVert
                              , LEstChildMiscInfo . EmiWidthInfo
                              ) 
                            <= Options . RightMargin
                            (* Relative width fits after IndentPosIfVert. *)   
                      THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz 
                      ELSE RETURN LResultIfNotHoriz 
                      END (* IF *) 
                    END (* IF *) 
                  END (* IF *) 

                ELSE (* WiHasNlBefore AND NOT WiHasNlAfter. *) 
                  IF LEstChildMiscInfo . EmiWidthInfo . WiWidth = 0 
                  THEN (* Descended-into Est child: [Nl, empty]. *) 
                    IF NonzeroWidthItemIsToLeft 
                         ( Lang , SubtreeFsNodeRef , EstTravInfo )
                    THEN (* FsSubtree: Stuff, [Nl, empty], ? *) 
                      RETURN LResultIfNotHoriz 
                      (* ^IF NonzeroWidthItemIsToRight too, this is correct. 
                          ELSE, the result is dead. *)                 
                    ELSE (* FsSubtree: empty, [Nl, empty], ? 
                            We are set up to pretraverse, starting at left of 
                            the descended-into Est child, which is at the left 
                            of the FsSubtree. *)
                      PreTraverseLine 
                        ( Lang 
                        , ModTextIsToLeftOnLine := FALSE 
                        , RootFsNodeRef := RootFsNodeRef 
                        , EstTravInfo := EstTravInfo 
                        , StartFmtNo := SubtreeFsNodeRef . FsLeftFmtNo 
                        , CharPos := 0 
                        , CurrentLineIndentPos := IndentPosIfVert 
                        , PrevTok := LbeStd . Tok__BegOfLine 
                        , (* VAR *) LastFmtNoOnLine := LLastFmtNoOnLine 
                        , (* VAR *) EstListChildrenToPass 
                             := LEstListChildrenToPass 
                        ) 
                    ; IF PreTraversedFsSubtreeFits 
                           ( SubtreeFsNodeRef 
                           , LLastFmtNoOnLine 
                           , LEstListChildrenToPass 
                           ) 
                      THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz  
                      ELSE RETURN LResultIfNotHoriz 
                      END (* IF *) 
                    END (* IF *) 

                  ELSE (* NlBefore AND nonzero width AND NOT NlAfter. 
                          Descended-into Est child: [Nl, stuff]. *) 
                    IF NonzeroWidthItemIsToLeft 
                         ( Lang , SubtreeFsNodeRef , EstTravInfo )
                    THEN (* Fs subtree: stuff, [Nl, stuff], ? *) 
                      RETURN LResultIfNotHoriz 
                    ELSE (* Fs subtree: empty, [Nl, stuff], ? 
                            Only the interior of the descended-into Est child 
                            matters. *) 
                      IF LEstChildMiscInfo . EmiWidthInfo . WiNlTrigger 
                         # LbeStd . LimitedCharNoInfinity
                         (* Absolute width, following a Nl.  We already 
                            verified that this fits. *) 
                         OR EstUtil . CharPosPlusWidthInfo 
                              ( IndentPosIfVert , LEstChildMiscInfo
                              . EmiWidthInfo
                              ) 
                             <= Options . RightMargin
                            (* Relative width fits after IndentPosIfVert. *)   
                      THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz 
                      ELSE RETURN LResultIfNotHoriz 
                      END (* IF *) 
                    END (* IF *) 
                  END (* IF *) 
                END (* IF NlAfter *) 

              ELSE (* NOT NlBefore (which => NlAfter, because of non-infinite
                      width) AND nonzero width. 
                      Fs subtree: ?, [stuff, Nl], ? *) 
                RETURN LResultIfNotHoriz  
                (* ^ IF NonzeroWidthItemIsToRight, this is correct. 
                     ELSE, the result is dead. *)
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        ELSE (* Not descending towards a new line.  Shouldn't happen. *) 
          RETURN LangUtil . FmtKindTyp . FmtKindUnknown  
        END (* IF *) 
      END (* IF *) 
    END FmtKindForFsSubtreeDescending

(* EXPORTED: *) 
; PROCEDURE FmtKindForFsSubtreeTraversing  
    ( Lang : LbeStd . LangTyp 
    ; CharPos : LbeStd . LimitedCharNoSignedTyp 
    ; ModTextIsToLeftOnLine : BOOLEAN 
    ; PrevTok : LbeStd . TokTyp 
    ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
      (* PRE: RootFsNodeRef.FsKind IN TravUtil.FsKindSetFsRoot. *)
    ; SubtreeFsNodeRef : LangUtil . FsNodeRefTyp 
      (* PRE: SubtreeFsNodeRef.FsKind IN TravUtil.FsKindSetSubtree. *)
    ; ParentFmtKind : LangUtil . FmtKindTyp 
    ; CurrentLineIndentPos : LbeStd . LimitedCharNoTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ; VAR (* IN OUT *) LastFmtNoOnLine : EstHs . FmtNoTyp 
    ; VAR (* IN OUT *) EstListChildrenToPass : LbeStd . EstChildNoTyp 
    ) 
  : LangUtil . FmtKindTyp 
  RAISES { Backout } 
  (* Use when traversing into the Fs subtree from one end or the other,
     when info about the current line is known. *) 
(* CHECK: Works Bwd?  Do we need it to? *) 

  = VAR LResultIfNotHoriz : LangUtil . FmtKindTyp 

  ; BEGIN (* FmtKindForFsSubtreeTraversing *) 
      IF ParentFmtKind = LangUtil . FmtKindTyp . FmtKindHoriz 
      THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz  
      ELSE
        CASE SubtreeFsNodeRef . FsKind <* NOWARN *> 
        OF FsKindTyp . FsKindSubtreeVert 
          (* If one of these has no line breaks at all (there are none in the
             Fs rule), or all are deleted, we will let code below give it
             FmtKindHoriz, which will speed computation of FmtKind for its
             descendents. *) 
        => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindVert 

        | FsKindTyp . FsKindSubtreeHoriz 
        => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindVert 

        | FsKindTyp . FsKindSubtreeFill 
        => LResultIfNotHoriz := LangUtil . FmtKindTyp . FmtKindFill  
        END (* CASE *) 

      ; IF LastFmtNoOnLine = EstHs . FmtNoUnknown 
        THEN 
          PreTraverseLine
            ( Lang 
            , ModTextIsToLeftOnLine 
            , EstTravInfo 
            , RootFsNodeRef 
            , EstTravInfo . EtiChildFmtNo 
            , CharPos 
            , CurrentLineIndentPos
            , PrevTok 

            , (* VAR *) LastFmtNoOnLine 
            , (* VAR *) EstListChildrenToPass 
            )
        END (* IF *)    
      ; IF PreTraversedFsSubtreeFits 
             ( SubtreeFsNodeRef 
             , LastFmtNoOnLine 
             , EstListChildrenToPass 
             ) 
        THEN RETURN LangUtil . FmtKindTyp . FmtKindHoriz  
        ELSE RETURN LResultIfNotHoriz 
        END (* IF *) 
      END (* if *) 
    END FmtKindForFsSubtreeTraversing

(* EXPORTED: *) 
; PROCEDURE PassEstListChild 
    ( VAR (* IN OUT *) EstListChildrenToPass : LbeStd . EstChildNoTyp ) 
  (* Call this when passing an EstListChild *) 

  = BEGIN 
      IF EstListChildrenToPass > 0 
      THEN DEC ( EstListChildrenToPass ) 
      END (* IF *) 
    END PassEstListChild

(* EXPORTED: *) 
; PROCEDURE DoTakeLineBreak 
    ( Lang : LbeStd . LangTyp 
    ; CharPos : LbeStd . LimitedCharNoSignedTyp 
    ; ModTextIsToLeftOnLine : BOOLEAN 
    ; PrevTok : LbeStd . TokTyp 
    ; RootFsNodeRef : LangUtil . FsNodeRefTyp 
    ; LineBreakFsNodeRef : LangUtil . FsNodeRefTyp    
    ; ParentFmtKind : LangUtil . FmtKindTyp 
    ; CurrentLineIndentPos : LbeStd . LimitedCharNoTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
      (* ^Of the line we are in. *) 
    ; VAR (* IN OUT *) LastFmtNoOnLine : EstHs . FmtNoTyp 
    ; VAR (* IN OUT *) EstListChildrenToPass : LbeStd . EstChildNoTyp 
    ) 
  : BOOLEAN 
  RAISES { Backout }
  (* The line break must be undeleted. 
     This function has a SIDE EFFECT.  If the line break is not to be taken,
     notes that it has been passed, which must happen exactly once, so that
     future calls on DoTakeLineBreak will work. *)  

  = BEGIN (* DoTakeLineBreak *) 
      IF ModTextIsToLeftOnLine 
      THEN
        (* We want line breaks to be taken as they were before any text
           modifications were made.  The presence of a ModText to the left
           means things may have shifted left or right, but it also means
           any originally untaken line breaks will have been deleted.  So
           this undeleted line break is the one that was taken at the end 
           of the line, before the ModText was inserted in the line. *)  
        RETURN TRUE 
      ELSE 
        CASE ParentFmtKind 
        OF LangUtil . FmtKindTyp . FmtKindUnknown 
        => CantHappen ( AFT . A_DoTakeLineBreak_FmtKindUnknown ) 
        ; RETURN TRUE (* Suppress compiler warning. *) 
        | LangUtil . FmtKindTyp . FmtKindHoriz 
        => RETURN FALSE 
        | LangUtil . FmtKindTyp . FmtKindVert 
        => RETURN TRUE 
        | LangUtil . FmtKindTyp . FmtKindFill 
        => IF LastFmtNoOnLine = EstHs . FmtNoUnknown 
           THEN 
             PreTraverseLine
               ( Lang 
               , ModTextIsToLeftOnLine (* Identically FALSE. *)  
               , EstTravInfo 
               , RootFsNodeRef 
               , LineBreakFsNodeRef . FsFmtNo 
               , CharPos 
               , CurrentLineIndentPos 
               , PrevTok 
               , (* VAR *) LastFmtNoOnLine 
               , (* VAR *) EstListChildrenToPass 
               )
           END (* IF *)
        ; IF LastFmtNoOnLine = EstHs . FmtNoNull 
          THEN (* Not even one FmtNo's worth of stuff fits. *) 
            RETURN TRUE 
          ELSE  
(* FIXME: This is really a mess.  It only works right for simple cases. 
          When this line break is among plural CF choices, and the one
          we will need to reach is also among plural CF choices 
          (For Fixed, this would be a separate Est child to the right,
           for list, a different Est list child, same Fs tree.)
          It probably needs more help in the way of Fs node fields too. 
*) 
            IF LineBreakFsNodeRef . FsIsInsideList 
            THEN
              RETURN EstListChildrenToPass = 0 
                     OR EstListChildrenToPass = 1 
                        AND 0 < LastFmtNoOnLine 
                        AND LastFmtNoOnLine < LineBreakFsNodeRef . FsFmtNo 
(* FIXME:            ^ This only works for one LB between list children. *) 
            ELSE (* AsFixed node. *)  
              RETURN
                LineBreakFsNodeRef . FsLineBreakToReachFmtNo = EstHs . FmtNoNull
                OR LastFmtNoOnLine 
                   < LineBreakFsNodeRef . FsLineBreakToReachFmtNo 
            END (* IF *) 
          END (* IF *) 
        END (* CASE *) 
      END (* IF*) 
    END DoTakeLineBreak 

(* EXPORTED: *) 
; PROCEDURE AssertFwdNoLostFixedChild 
    ( FsNodeRef : LangUtil . FsNodeRefTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ) 
  RAISES { Backout } 

  = VAR LTreeFmtNo : EstHs . FmtNoTyp 

  ; BEGIN 
      IF NOT FsNodeRef . FsIsInsideList 
         AND EstTravInfo . EtiChildNo < EstTravInfo . EtiChildCt 
      THEN (* A child remains and is a child of a fixed node. *) 
        TYPECASE EstTravInfo . EtiChildLeafElem . LeChildRef 
        OF NULL 
        => LTreeFmtNo := EstTravInfo . EtiChildFmtNo 
        | ModHs . ModDelTyp ( TModDel ) 
        => LTreeFmtNo := TModDel . ModDelThruFmtNo 
        ELSE 
          LTreeFmtNo := EstTravInfo . EtiChildFmtNo 
        END (* TYPECASE *) 
      ; Assert  
          ( FsNodeRef . FsFmtNo <= LTreeFmtNo 
          , AFT . A_AssertFwdNoLostFixedChild__Lost_fixed_child
          ) 
      END (* IF *) 
    END AssertFwdNoLostFixedChild 

(* EXPORTED: *) 
; PROCEDURE AssertBwdNoLostFixedChild 
    ( FsNodeRef : LangUtil . FsNodeRefTyp 
    ; READONLY EstTravInfo : EstTravInfoTyp 
    ) 
  RAISES { Backout } 

  = BEGIN 
      IF NOT FsNodeRef . FsIsInsideList 
         AND 0 <= EstTravInfo . EtiChildNo 
      THEN 
        Assert 
          ( FsNodeRef . FsFmtNo >= EstTravInfo . EtiChildFmtNo 
          , AFT . A_AssertBwdNoLostFixedChild__Lost_fixed_child
          ) 
      END (* IF *) 
    END AssertBwdNoLostFixedChild

; EXCEPTION NodeNoFound ( LbeStd . EstNodeNoTyp ) 

(* EXPORTED: *) 
; PROCEDURE NodeNoOfNodeRef
    ( RootNodeRef : LbeStd . EstRootTyp ; SoughtNodeRef : REFANY ) 
  : LbeStd . EstNodeNoTyp
  (* LbeStd.EstNodNoNull, if not found. *)
  RAISES { Backout } 

  = PROCEDURE NnnrRecurse
      ( NodeRef : LbeStd . EstRootTyp ; AbsNodeNo : LbeStd . EstNodeNoTyp )
    RAISES { NodeNoFound , Backout } 

    = VAR LChildAbsNodeNo : LbeStd . EstNodeNoTyp
    ; VAR LEstTravInfo : EstTravInfoTyp

    ; BEGIN
        IF NodeRef = SoughtNodeRef
        THEN RAISE NodeNoFound ( AbsNodeNo )
        END (* IF *) 
      ; TYPECASE NodeRef
        OF NULL =>
        
        | EstHs . EstRefTyp ( TEstRef ) 
        => InitEstTravInfoFwd
             ( LEstTravInfo
             , TEstRef
             , EstHs . EstChildKindSetEmpty
             , AbsNodeNo
             )
        ; IF LEstTravInfo . EtiChildCt = 0 THEN RETURN END (* IF *)
        ; WHILE LEstTravInfo . EtiChildNo < LEstTravInfo . EtiChildCt
          DO
            LChildAbsNodeNo := AbsNodeNo + LEstTravInfo . EtiChildRelNodeNo 
          ; IF LEstTravInfo . EtiChildLeafElem . LeChildRef = SoughtNodeRef
            THEN RAISE NodeNoFound ( LChildAbsNodeNo ) 
            END (* IF *)
          ; NnnrRecurse
              ( LEstTravInfo . EtiChildLeafElem . LeChildRef , LChildAbsNodeNo )
          ; IncEstChild ( (* VAR *) LEstTravInfo ) 
          END (* WHILE *)
        ELSE 
        END (* TYPECASE *) 
      END NnnrRecurse

  ; BEGIN 
      TRY
        NnnrRecurse ( RootNodeRef , AbsNodeNo := 0 )
      ; RETURN LbeStd . EstNodeNoNull 
      EXCEPT
      | NodeNoFound ( ENodeNo )
        => RETURN ENodeNo 
      END (* EXCEPT *) 
    END NodeNoOfNodeRef 
  
; BEGIN (* TravUtil *)
  END TravUtil 
. 
