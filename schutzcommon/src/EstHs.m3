
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE EstHs 

(* Data structure for interior Est nodes. 
   Because of a some cartesian products of fields, there are accessor and
   mutator procedures in here.  
*) 

; IMPORT Fmt 
(* Except for swapping the strings "Pickle" and "Pickle2 AS Pickle", make no
   changes to the following line, as it is recognized/edited by scripts.
   (see scripts/topickle.sh and scripts/topickle2.sh)
*) 
; IMPORT Pickle2 AS Pickle (* The Pickle2 mess. *)
; IMPORT Rd 
; IMPORT Text
; IMPORT TextWr 
; IMPORT Thread
; IMPORT Wr 

; FROM Assertions IMPORT Assert 
; FROM Failures IMPORT Backout   
; IMPORT LbeStd 
; IMPORT MessageCodes 
; IMPORT PortTypes 
; IMPORT Misc 
; IMPORT LangUtil 

; TYPE AFT = MessageCodes . T

(* EXPORTED: *) 
; PROCEDURE FmtNoImage ( FmtNo : FmtNoTyp ; Pad := FmtNoPad ) : TEXT

  = VAR LShort , LResult : TEXT

  ; BEGIN
      IF FmtNo = FmtNoNull
      THEN LShort := "Nul" 
      ELSIF FmtNo = FmtNoUnknown
      THEN LShort := "Unk"
      ELSE LShort := Fmt . Int ( FmtNo ) 
      END (* IF *)
    ; IF Pad > 0 
      THEN LResult := Fmt . Pad ( LShort , Pad )
      ELSE LResult := LShort
      END (* IF *)
    ; RETURN LResult 
    END FmtNoImage 

(* Use the procedure below to assign to WidthInfoNull, because 
   PM3-1.1.15, LINUXLIBC6, generates bad code for := WidthInfoNull 
   Later: This is probably the integrated back end byte assignment 
   bug. *) 
(* EXPORTED: *) 
; PROCEDURE MakeWidthInfoNull ( VAR WidthInfo : WidthInfoTyp ) 

  = BEGIN (* MakeWidthInfoNull *) 
      WidthInfo . WiNlTrigger := LbeStd . LimitedCharNoInfinity 
    ; WidthInfo . WiWidth := 0 
    ; WidthInfo . WiIsNull := TRUE  
    ; WidthInfo . WiHasAbsFromPos := FALSE 
    ; WidthInfo . WiHasNlBefore := FALSE 
    ; WidthInfo . WiHasNlAfter := FALSE 
    ; WidthInfo . WiHasNlWithin := FALSE 
    ; WidthInfo . WiWholeLineModsOnly := FALSE 
    END MakeWidthInfoNull 

(* Use the procedure below to assign to WidthInfoInfinity, because 
   Pm3-1.1.15, LINUXLIBC6, generates bad code for := WidthInfoNull, 
   and I suspect if of being capable of the same for WidthInfoInfinity. *) 
(* EXPORTED: *) 
; PROCEDURE MakeWidthInfoInfinity ( VAR WidthInfo : WidthInfoTyp ) 

  = BEGIN (* MakeWidthInfoInfinity *) 
      WidthInfo . WiNlTrigger := 0 
    ; WidthInfo . WiWidth := LbeStd . LimitedCharNoInfinity 
    ; WidthInfo . WiIsNull := FALSE 
    ; WidthInfo . WiHasAbsFromPos := FALSE 
    ; WidthInfo . WiHasNlBefore := FALSE 
    ; WidthInfo . WiHasNlAfter := FALSE 
    ; WidthInfo . WiHasNlWithin := FALSE 
    ; WidthInfo . WiWholeLineModsOnly := FALSE 
    END MakeWidthInfoInfinity 

(* EXPORTED: *) 
; PROCEDURE RefToNewLeafArray 
    ( Length : ElemNoTyp ; READONLY FullLeafArray : FullLeafArrayTyp ) 
    : LeafArrayRefTyp 
  (* Allocate and return the ref to a new leaf array of exactly 
     Length elements, initializing them from the first Length 
     elements of FullLeafArray *) 

  = BEGIN (* RefToNewLeafArray *) 
      WITH WResult = NEW ( LeafArrayRefTyp , Length ) 
      DO SUBARRAY ( WResult ^ , 0 , Length ) 
           := SUBARRAY ( FullLeafArray , 0 , Length ) 
      ; RETURN WResult 
      END (* WITH *) 
    END RefToNewLeafArray 

(* EXPORTED: *) 
; PROCEDURE RefToNewNonleafArray 
    ( Length : ElemNoTyp ; READONLY FullNonleafArray : FullNonleafArrayTyp ) 
    : NonleafArrayRefTyp 
  (* Allocate and return the ref to a new nonleaf array of exactly 
     Length elements, initializing them from the first Length 
     elements of FullNonleafArray *) 

  = BEGIN (* RefToNewNonleafArray *) 
      WITH WResult = NEW ( NonleafArrayRefTyp , Length ) 
      DO SUBARRAY ( WResult ^ , 0 , Length ) 
           := SUBARRAY ( FullNonleafArray , 0 , Length ) 
      ; RETURN WResult 
      END (* WITH *) 
    END RefToNewNonleafArray 

(* EXPORTED: *) 
; PROCEDURE KTreeLeafChildCt 
    ( Self : KTreeLeafRefTyp ) : LbeStd . EstChildNoTyp 

  = BEGIN (* KTreeLeafChildCt *) 
      WITH WArrayRef = KTreeLeafArrayRef ( Self ) 
      DO IF WArrayRef = NIL 
         THEN 
           RETURN 0 
         ELSE 
           RETURN NUMBER ( WArrayRef ^ ) 
         END (* IF *) 
      END (* WITH *) 
    END KTreeLeafChildCt 

(* EXPORTED: *) 
; PROCEDURE KTreeLeafNodeCt 
    ( Self : KTreeLeafRefTyp ) : LbeStd . EstNodeNoTyp 

  = BEGIN (* KTreeLeafNodeCt *) 
      WITH WArrayRef = KTreeLeafArrayRef ( Self ) 
      DO IF WArrayRef = NIL 
         THEN 
           RETURN 0 
         ELSE 
           RETURN WArrayRef [ NUMBER ( WArrayRef ^ ) - 1 ] . LeCumNodeCt 
         END (* IF *) 
      END (* WITH *) 
    END KTreeLeafNodeCt 

(* EXPORTED: *) 
; PROCEDURE KTreeLeafArrayRef ( Self : KTreeLeafRefTyp ) : LeafArrayRefTyp 

  = BEGIN (* KTreeLeafArrayRef *) 
      RETURN Self . KTreeLeafArrayRef 
    END KTreeLeafArrayRef 

(* EXPORTED: *) 
; PROCEDURE FetchKTreeLeafArray 
    ( Self : KTreeLeafRefTyp ; VAR ResultLeafArray : FullLeafArrayTyp ) 

  = BEGIN (* FetchKTreeLeafArray *) 
      WITH WArrayRef = KTreeLeafArrayRef ( Self ) 
      DO IF WArrayRef = NIL 
         THEN 
           FOR RI := 0 TO NUMBER ( ResultLeafArray ) - 1 
           DO ResultLeafArray [ RI ] := LeafElemNull 
           END (* FOR *) 
         ELSE 
           WITH WElemCt = NUMBER ( WArrayRef ^ ) 
           DO SUBARRAY ( ResultLeafArray , 0 , WElemCt ) 
                := SUBARRAY ( WArrayRef ^ , 0 , WElemCt ) 
           ; FOR RI := WElemCt TO NUMBER ( ResultLeafArray ) - 1 
             DO ResultLeafArray [ RI ] := LeafElemNull 
             END (* FOR *) 
           END (* WITH *) 
         END (* IF *) 
      END (* WITH *) 
    END FetchKTreeLeafArray 

(* EXPORTED: *) 
; PROCEDURE KTreeNonleafChildCt 
    ( Self : KTreeNonleafRefTyp ) : LbeStd . EstChildNoTyp 

  = BEGIN (* KTreeNonleafChildCt *) 
      WITH 
        WArrayRef = KTreeNonleafArrayRef ( Self ) 
      , WElem = WArrayRef [ NUMBER ( WArrayRef ^ ) - 1 ] 
      DO RETURN WElem . NleCumChildCt 
      END (* WITH *) 
    END KTreeNonleafChildCt 

(* EXPORTED: *) 
; PROCEDURE KTreeNonleafNodeCt 
    ( Self : KTreeNonleafRefTyp ) : LbeStd . EstNodeNoTyp 

  = BEGIN (* KTreeNonleafNodeCt *) 
      WITH 
        WArrayRef = KTreeNonleafArrayRef ( Self ) 
      , WElem = WArrayRef [ NUMBER ( WArrayRef ^ ) - 1 ] 
      DO RETURN WElem . NleCumNodeCt 
      END (* WITH *) 
    END KTreeNonleafNodeCt 

(* EXPORTED: *) 
; PROCEDURE KTreeNonleafArrayRef 
    ( Self : KTreeNonleafRefTyp ) : NonleafArrayRefTyp 

  = BEGIN (* KTreeNonleafArrayRef *) 
      RETURN Self . KTreeNonleafArrayRef 
    END KTreeNonleafArrayRef 

(* EXPORTED: *) 
; PROCEDURE FetchKTreeNonleafArray 
    ( Self : KTreeNonleafRefTyp 
    ; VAR ResultNonleafArray : FullNonleafArrayTyp 
    ) 

  = BEGIN (* FetchKTreeNonleafArray *) 
      WITH 
        WArrayRef = KTreeNonleafArrayRef ( Self ) 
      , WElemCt = NUMBER ( WArrayRef ^ ) 
      DO SUBARRAY ( ResultNonleafArray , 0 , WElemCt ) 
           := SUBARRAY ( WArrayRef ^ , 0 , WElemCt ) 
      ; FOR RI := WElemCt TO NonleafArrayElemCtMax - 1 
        DO ResultNonleafArray [ RI ] := NonleafElemNull 
        END (* FOR *) 
      END (* WITH *) 
    END FetchKTreeNonleafArray 

(* EXPORTED: *) 
; PROCEDURE EstLeafChildCt ( Self : EstLeafRefTyp ) : LbeStd . EstChildNoTyp 

  = BEGIN (* EstLeafChildCt *) 
      WITH WArrayRef = EstLeafArrayRef ( Self ) 
      DO IF WArrayRef = NIL 
         THEN 
           RETURN 0 
         ELSE 
           RETURN NUMBER ( WArrayRef ^ ) 
         END (* IF *) 
      END (* WITH *) 
    END EstLeafChildCt 

(* EXPORTED: *) 
; PROCEDURE EstLeafNodeCt ( Self : EstLeafRefTyp ) : LbeStd . EstNodeNoTyp 

  = BEGIN (* EstLeafNodeCt *) 
      WITH WArrayRef = EstLeafArrayRef ( Self ) 
      DO IF WArrayRef = NIL 
         THEN RETURN 0
         ELSIF NUMBER ( WArrayRef ^ ) <= 0 
         THEN RETURN 0
         ELSE 
           WITH WElem = WArrayRef [ NUMBER ( WArrayRef ^ ) - 1 ] 
           DO RETURN WElem . LeCumNodeCt 
           END (* WITH *) 
         END (* IF *) 
      END (* WITH *) 
    END EstLeafNodeCt 

(* EXPORTED: *) 
; PROCEDURE EstLeafArrayRef ( Self : EstLeafRefTyp ) : LeafArrayRefTyp 

  = BEGIN (* EstLeafArrayRef *) 
      RETURN Self . EstLeafArrayRef 
    END EstLeafArrayRef 

(* EXPORTED: *) 
; PROCEDURE FetchEstLeafArray 
    ( Self : EstLeafRefTyp ; VAR ResultLeafArray : FullLeafArrayTyp ) 

  = BEGIN (* FetchEstLeafArray *) 
      WITH WArrayRef = EstLeafArrayRef ( Self ) 
      DO IF WArrayRef = NIL 
         THEN 
           FOR RI := 0 TO NUMBER ( ResultLeafArray ) - 1 
           DO ResultLeafArray [ RI ] := LeafElemNull 
           END (* FOR *) 
         ELSE 
           WITH WElemCt = NUMBER ( WArrayRef ^ ) 
           DO SUBARRAY ( ResultLeafArray , 0 , WElemCt ) 
                := SUBARRAY ( WArrayRef ^ , 0 , WElemCt ) 
           ; FOR RI := WElemCt TO NUMBER ( ResultLeafArray ) - 1 
             DO ResultLeafArray [ RI ] := LeafElemNull 
             END (* FOR *) 
           END (* WITH *) 
         END (* IF *) 
      END (* WITH *) 
    END FetchEstLeafArray 

(* EXPORTED: *) 
; PROCEDURE EstNonleafChildCt 
    ( Self : EstNonleafRefTyp ) : LbeStd . EstChildNoTyp 

  = BEGIN (* EstNonleafChildCt *) 
      WITH 
        WArrayRef = EstNonleafArrayRef ( Self ) 
      , WElem = WArrayRef [ NUMBER ( WArrayRef ^ ) - 1 ] 
      DO RETURN WElem . NleCumChildCt 
      END (* WITH *) 
    END EstNonleafChildCt 

(* EXPORTED: *) 
; PROCEDURE EstNonleafNodeCt 
    ( Self : EstNonleafRefTyp ) : LbeStd . EstNodeNoTyp 

  = BEGIN (* EstNonleafNodeCt *) 
      WITH 
        WArrayRef = EstNonleafArrayRef ( Self ) 
      , WElem = WArrayRef [ NUMBER ( WArrayRef ^ ) - 1 ] 
      DO RETURN WElem . NleCumNodeCt 
      END (* WITH *) 
    END EstNonleafNodeCt 

(* EXPORTED: *) 
; PROCEDURE EstNonleafArrayRef 
    ( Self : EstNonleafRefTyp ) : NonleafArrayRefTyp 

  = BEGIN (* EstNonleafArrayRef *) 
      RETURN Self . EstNonleafArrayRef 
    END EstNonleafArrayRef 

(* EXPORTED: *) 
; PROCEDURE FetchEstNonleafArray 
    ( Self : EstNonleafRefTyp 
    ; VAR ResultNonleafArray : FullNonleafArrayTyp 
    ) 

  = BEGIN (* FetchEstNonleafArray *) 
      WITH 
        WArrayRef = EstNonleafArrayRef ( Self ) 
      , WElemCt = NUMBER ( WArrayRef ^ ) 
      DO SUBARRAY ( ResultNonleafArray , 0 , WElemCt ) 
           := SUBARRAY ( WArrayRef ^ , 0 , WElemCt ) 
      ; FOR RI := WElemCt TO NonleafArrayElemCtMax - 1 
        DO ResultNonleafArray [ RI ] := NonleafElemNull 
        END (* FOR *) 
      END (* WITH *) 
    END FetchEstNonleafArray 

(* EXPORTED: *) 
; PROCEDURE EstNodeKindImage ( Value : EstNodeKindTyp ) : TEXT
(* TODO: Give this an ImageKind parameter. *) 

  = BEGIN (* EstNodeKindImage *) 
      CASE Value 
      OF EstNodeKindTyp . EstNodeKindPlain 
      => RETURN "EstNodeKindPlain" 
      | EstNodeKindTyp . EstNodeKindTrail 
      => RETURN "EstNodeKindTrail" 
      | EstNodeKindTyp . EstNodeKindModTok 
      => RETURN "EstNodeKindModTok" 
      END (* CASE *) 
    END EstNodeKindImage 

(* EXPORTED: *) 
; PROCEDURE WidthInfoImage ( READONLY Value : WidthInfoTyp ) : TEXT 

  = BEGIN (* WidthInfoImage *) 
      RETURN 
        "{WiNlTrigger=" 
        & LbeStd . LimitedCharNoImage ( Value . WiNlTrigger ) 
        & ", WiWidth=" 
        & LbeStd . LimitedCharNoImage ( Value . WiWidth ) 
        & ", WiIsNull=" 
        & Misc . BooleanImageShort ( Value . WiIsNull ) 
        & ", WiHasAbsFromPos=" 
        & Misc . BooleanImageShort ( Value . WiHasAbsFromPos ) 
        & ", WiHasNlBefore=" 
        & Misc . BooleanImageShort ( Value . WiHasNlBefore ) 
        & ", WiHasNlAfter=" 
        & Misc . BooleanImageShort ( Value . WiHasNlAfter ) 
        & ", WiHasNlWithin=" 
        & Misc . BooleanImageShort ( Value . WiHasNlWithin ) 
        & ", WiWholeLineModsOnly=" 
        & Misc . BooleanImageShort ( Value . WiWholeLineModsOnly ) 
        & "}" 
    END WidthInfoImage 

(* EXPORTED: *) 
; PROCEDURE WidthInfoIsNull ( WidthInfo : WidthInfoTyp ) : BOOLEAN 
  RAISES { Backout } 

  = VAR LResult : BOOLEAN 

  ; BEGIN 
      LResult  
        := NOT WidthInfo . WiHasNlBefore  
           AND NOT WidthInfo . WiHasNlAfter  
           AND NOT WidthInfo . WiHasNlWithin
           AND WidthInfo . WiWidth = 0   
    ; Assert
        ( LResult = WidthInfo . WiIsNull 
        , AFT . A_WidthInfoIsNull_Disagreement 
        ) 
    ; RETURN LResult 
    END WidthInfoIsNull

(* EXPORTED: *) 
; PROCEDURE EstChildKindImage
    ( Value : EstChildKindTyp ; ImageKind : ImageKindTyp ) : TEXT 

  = BEGIN (* EstChildKindImage *)
      CASE ImageKind OF
      | ImageKindTyp . Decimal
        => RETURN Fmt . Int ( Value )  
      | ImageKindTyp . Hex 
        => RETURN Fmt . Int ( Value , 16 )
      | ImageKindTyp . Short 
        => RETURN EstChildKindImageShort ( Value ) 
      | ImageKindTyp . Ident 
        => RETURN "EstChildKind" & EstChildKindImageShort ( Value ) 
      | ImageKindTyp . Qualified 
        => RETURN "EstHs.EstChildKind" & EstChildKindImageShort ( Value ) 
      END (* CASE *) 
    END EstChildKindImage 

; PROCEDURE EstChildKindImageShort ( Value : EstChildKindTyp ) : TEXT
  (* Only the distinct part, e.g. "FirstOfGroup" *)

  = BEGIN (* EstChildKindImageShort *) 
      CASE Value 
      OF EstChildKindEstChild 
      => RETURN "EstChild" 
      | EstChildKindFirstOfGroup
      => RETURN "FirstOfGroup" 
      | EstChildKindContainsSyntMod 
      => RETURN "ContainsSyntMod" 
      | EstChildKindContainsInsertionRepair 
      => RETURN "ContainsInsertionRepair" 
      | EstChildKindContainsDeletionRepair 
      => RETURN "ContainsDeletionRepair" 
      | EstChildKindDisplayComputable 
      => RETURN "DisplayComputable" 
      | EstChildKindNonNIL 
      => RETURN "NonNIL" 
      | EstChildKindContainsErr 
      => RETURN "ContainsErr" 
      | EstChildKindContainsNoKnownNl 
      => RETURN "ContainsNoKnownNl" 
      | EstChildKindContainsTempMark 
      => RETURN "ContainsTempMark" 
      | EstChildKindTrailingMod 
      => RETURN "TrailingMod" 
      | EstChildKindOptSingletonList 
      => RETURN "OptSingletonList"
      | EstChildKindTrailingSep 
      => RETURN "TrailingSep"
      END (* CASE *) 
    END EstChildKindImageShort

(* EXPORTED: *) 
; PROCEDURE EstChildKindSetImage 
    ( Value : EstChildKindSetTyp 
    ; ImageKind := ImageKindTyp . Ident (* Of the set members. *)  
    ; Indent := LbeStd . StdIndent
    ; RightMargin : CARDINAL := ImageRightMargin 
    ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 
  ; VAR LResult : TEXT 
  ; VAR LPos : INTEGER 
  ; VAR LElementImage : TEXT 
  ; VAR LElementLength : INTEGER 
  ; VAR LElementCt := 0 
  ; VAR LLineCt := 1 
  ; VAR LWrT : Wr . T

  ; BEGIN (* EstChildKindSetImage *) 
      LWrT := TextWr . New ( ) 
    ; Wr . PutChar ( LWrT , '{' )  
    ; LPos := Indent + 1 
    ; FOR I := EstChildKindMin TO EstChildKindMax 
      DO IF I IN Value 
         THEN
           LElementImage := EstChildKindImage ( I , ImageKind ) 
         ; LElementLength := Text . Length ( LElementImage ) 
         ; IF LElementCt > 0 
           THEN 
             IF LPos + LElementLength > RightMargin 
             THEN
               Wr . PutText ( LWrT , Wr . EOL ) 
             ; Wr . PutText ( LWrT , LID ) 
             ; LPos := Indent
             ; INC ( LLineCt ) 
             END (* IF *) 
           ; Wr . PutChar ( LWrT , ',' )
           ; INC ( LPos )
           END (* IF *) 
         ; Wr . PutText ( LWrT , LElementImage ) 
         ; INC ( LPos , LElementLength ) 
         ; INC ( LElementCt ) 
         END (* IF *) 
      END (* FOR *) 
    ; IF LLineCt > 1
      THEN
        Wr . PutText ( LWrT , Wr . EOL ) 
      ; Wr . PutText ( LWrT , LID ) 
      END (* IF *) 
    ; Wr . PutChar ( LWrT , '}' )  
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END EstChildKindSetImage 

(* EXPORTED: *) 
; PROCEDURE EstChildKindSetFixedImage  ( Value : EstChildKindSetTyp ) : TEXT 
  (* Fixed size image, by leaving blank spaces for absent kinds and
     denoting each present kind by its hexadecimal value. *) 

  = VAR LResult : TEXT 
  ; VAR LWrT : Wr . T

  ; BEGIN (* EstChildKindSetFixedImage *) 
      LWrT := TextWr . New ( ) 
    ; Wr . PutChar ( LWrT , '{' ) 
    ; FOR I := EstChildKindMin TO EstChildKindMax 
      DO IF I IN Value 
        THEN Wr . PutText ( LWrT , Fmt . Int ( I , base := 16 ) ) 
        ELSE Wr . PutChar ( LWrT , ' ' ) 
        END (* IF *)
      END (* FOR *) 
    ; Wr . PutChar ( LWrT , '}' )  
    ; LResult := TextWr . ToText ( LWrT )
    ; RETURN LResult 
    END EstChildKindSetFixedImage 

(* EXPORTED: *) 
; PROCEDURE IsFirstOfGroup 
    ( LeftFmtNo : FmtNoTyp 
    ; LeftEdgeKind : EdgeKindTyp 
    ; RightFmtNo : FmtNoTyp 
    ; RightEdgeKind : EdgeKindTyp 
    ) 
  : BOOLEAN 

  = BEGIN 
      IF LeftFmtNo = FmtNoNull 
         OR LeftFmtNo # RightFmtNo 
      THEN RETURN TRUE 
      ELSIF LeftEdgeKind 
            IN EdgeKindSetTrailingModOrEstChild  
            AND RightEdgeKind 
                IN EdgeKindSetLeadingModOrEstChild  
      THEN RETURN TRUE 
      ELSE RETURN FALSE 
      END 
    END IsFirstOfGroup 

(* EXPORTED: *) 
; PROCEDURE EdgeKindImage ( READONLY Value : EdgeKindTyp ) : TEXT 

  = BEGIN (* EdgeKindImage *) 
      CASE Value 
      OF EdgeKindTyp . EdgeKindLeadingMod 
      => RETURN "EdgeKindLeadingMod" 
      | EdgeKindTyp . EdgeKindTrailingMod 
      => RETURN "EdgeKindTrailingMod" 
      | EdgeKindTyp . EdgeKindModDel 
      => RETURN "EdgeKindModDel" 
      | EdgeKindTyp . EdgeKindEstChild 
      => RETURN "EdgeKindEstChild" 
      END (* CASE *) 
    END EdgeKindImage 

(* EXPORTED: *) 
; PROCEDURE EdgeInfoImage ( READONLY Value : EdgeInfoTyp ) : TEXT 

  = BEGIN (* EdgeInfoImage *) 
      RETURN 
        "{EiTok=" 
        & LbeStd . NumIdTokImage ( Value . EiTok ) 
        & ", EiFmtNo=" 
        & FmtNoImage ( Value . EiFmtNo ) 
        & ", EiEdgeKind=" 
        & EdgeKindImage ( Value . EiEdgeKind ) 
        & "}" 
    END EdgeInfoImage 

(* EXPORTED: *) 
; PROCEDURE SliceEdgeInfoPairImage 
    ( READONLY Value : SliceEdgeInfoPairTyp ; Indent := LbeStd . StdIndent ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 

  ; BEGIN (* SliceEdgeInfoPairImage *) 
      RETURN 
        "{SeiLeftEdgeInfo=" 
        & EdgeInfoImage ( Value . SeiLeftEdgeInfo ) 
        & Wr . EOL 
        & LID 
        & ", SeiRightEdgeInfo=" 
        & EdgeInfoImage ( Value . SeiRightEdgeInfo ) 
        & "}" 
    END SliceEdgeInfoPairImage 

(* EXPORTED: *) 
; PROCEDURE LeafElemImage 
    ( READONLY LeafArray : LeafArrayTyp 
    ; Indent := LbeStd . StdIndent 
    ; Number : [ 0 .. LeafArrayElemCtMax ] 
    ; Subscript : [ 0 .. LeafArrayElemCtMax ] 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 
  ; VAR LRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LRelChildNo : LbeStd . EstChildNoTyp 

  ; BEGIN (* LeafElemImage *) 
      WITH WLeftElem = LeafArray [ Number - 1 ] 
      DO WITH WElem = LeafArray [ Subscript ] 
         DO LRelNodeNo 
              := WLeftElem . LeCumNodeCt 
                 - WElem . LeCumNodeCt 
                 + 1 (* For the parent. *) 
         ; LRelChildNo := Number - 1 - Subscript 
         ; RETURN 
             "{ LeCumNodeCt=" 
             & LbeStd . EstNodeNoImage ( WElem . LeCumNodeCt ) 
             & ", LeFmtNo=" 
             & FmtNoImage ( WElem . LeFmtNo ) 
             & Wr . EOL 
             & LID 
             & ", LeChildRef Addr "
             & Misc . RefanyImage ( WElem . LeChildRef ) 
             & " ={ AbsNodeNo=" 
             & LbeStd . EstNodeNoImage ( NodeNo + LRelNodeNo ) 
             & ", RelNodeNo=" 
             & LbeStd . EstNodeNoImage ( LRelNodeNo ) 
             & ", AbsChildNo=" 
             & LbeStd . EstChildNoImage ( ChildNo + LRelChildNo ) 
             & ", RelChildNo=" 
             & LbeStd . EstChildNoImage ( LRelChildNo ) 
             & "}," 
             & Wr . EOL 
             & LID 
             & ", LeKindSet=" 
             & EstChildKindSetImage 
                 ( WElem . LeKindSet 
                 , Indent := Indent + 12
                 , ImageKind := ImageKindTyp . Decimal  
(* TODO: Pass ImageKind all the way down through EstUtil . EstNodeImage, 
         the 6 method overrides of EstHs . KTreeRefTyp . Image, LeafArrayImage,
         whatever, to LeafElemImage.
*) 
                 ) 
             & Wr . EOL 
             & LID 
             & "}" 
         END (* WITH *) 
      END (* WITH *) 
    END LeafElemImage 

(* EXPORTED: *) 
; PROCEDURE LeafArrayImage 
    ( READONLY Value : LeafArrayRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 
  ; VAR LNumber : PortTypes . Int32Typ 
  ; VAR LResult : TEXT 

  ; BEGIN (* LeafArrayImage *) 
      IF Value = NIL 
      THEN 
        RETURN "NIL" 
      ELSE 
        LNumber := NUMBER ( Value ^ ) 
      ; IF LNumber <= 0 
        THEN 
          RETURN "" 
        ELSE 
          LResult 
            := "[" 
               & PortTypes . Int32Image ( LNumber - 1 ) 
               & "]=" 
               & LeafElemImage 
                   ( Value ^ 
                   , Indent + 4 
                   , LNumber 
                   , LNumber - 1 
                   , NodeNo 
                   , ChildNo 
                   ) 
        ; FOR I := LNumber - 2 TO 0 BY - 1 
          DO LResult 
               := LResult 
                  & Wr . EOL 
                  & LID 
                  & "[" 
                  & PortTypes . Int32Image ( I ) 
                  & "]:" 
                  & LeafElemImage 
                      ( Value ^ , Indent + 4 , LNumber , I , NodeNo , ChildNo ) 
          END (* FOR *) 
        ; RETURN LResult 
        END (* IF *) 
      END (* IF *) 
    END LeafArrayImage 

(* EXPORTED: *) 
; PROCEDURE NonleafElemImage 
    ( READONLY NonleafArray : NonleafArrayTyp 
    ; Indent := LbeStd . StdIndent 
    ; Number : [ 0 .. NonleafArrayElemCtMax ] 
    ; Subscript : [ 0 .. NonleafArrayElemCtMax ] 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 
  ; VAR LRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LRelChildNo : LbeStd . EstChildNoTyp 

  ; BEGIN (* NonleafElemImage *) 
      WITH WLeftElem = NonleafArray [ Number - 1 ] 
      DO WITH WElem = NonleafArray [ Subscript ] 
         DO LRelNodeNo 
              := WLeftElem . NleCumNodeCt 
                 - WElem . NleCumNodeCt 
                 + 1 (* For the parent. *) 
         ; LRelChildNo := WLeftElem . NleCumChildCt - WElem . NleCumChildCt 
         ; RETURN 
             "{NleCumNodeCt=" 
             & LbeStd . EstNodeNoImage ( WElem . NleCumNodeCt ) 
             & ", NleCumChildCt=" 
             & LbeStd . EstChildNoImage ( WElem . NleCumChildCt ) 
             & Wr . EOL 
             & LID 
             & " , NleChildRef:=(AbsNodeNo=" 
             & LbeStd . EstNodeNoImage ( NodeNo + LRelNodeNo ) 
             & ",RelNodeNo=" 
             & LbeStd . EstNodeNoImage ( LRelNodeNo ) 
             & ",AbsChildNo=" 
             & LbeStd . EstChildNoImage ( ChildNo + LRelChildNo ) 
             & ",RelChildNo=" 
             & LbeStd . EstChildNoImage ( LRelChildNo ) 
             & ")" 
             & Wr . EOL 
             & LID 
             & " , NleChildKindSet=" 
             & EstChildKindSetImage
                 ( WElem . NleKindSet , ImageKindTyp . Decimal ) 
             & "}" 
         END (* WITH *) 
      END (* WITH *) 
    END NonleafElemImage 

(* EXPORTED: *) 
; PROCEDURE NonleafArrayImage 
    ( READONLY Value : NonleafArrayRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 
  ; VAR LNumber : PortTypes . Int32Typ 
  ; VAR LResult : TEXT 

  ; BEGIN (* NonleafArrayImage *) 
      IF Value = NIL 
      THEN 
        RETURN "NIL" 
      ELSE 
        LNumber := NUMBER ( Value ^ ) 
      ; IF LNumber <= 0 
        THEN 
          RETURN "" 
        ELSE 
          LResult 
            := "[" 
               & PortTypes . Int32Image ( LNumber - 1 ) 
               & "]=" 
               & NonleafElemImage 
                   ( Value ^ 
                   , Indent + 4 
                   , LNumber 
                   , LNumber - 1 
                   , NodeNo 
                   , ChildNo 
                   ) 
        ; FOR I := LNumber - 2 TO 0 BY - 1 
          DO LResult 
               := LResult 
                  & Wr . EOL 
                  & LID 
                  & "[" 
                  & PortTypes . Int32Image ( I ) 
                  & "]=" 
                  & NonleafElemImage 
                      ( Value ^ , Indent + 4 , LNumber , I , NodeNo , ChildNo ) 
          END (* FOR *) 
        ; RETURN LResult 
        END (* IF *) 
      END (* IF *) 
    END NonleafArrayImage

(* EXPORTED: *) 
; PROCEDURE KTreeRefImage 
    ( Self : KTreeRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; <* UNUSED *> NodeNo : LbeStd . EstNodeNoTyp 
    ; <* UNUSED *> ChildNo : LbeStd . EstChildNoTyp 
    ; <* UNUSED *> Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 

  ; BEGIN (* KTreeRefImage *) 
      RETURN 
        LID 
        & "WidthInfo=" 
        & WidthInfoImage ( Self . KTreeWidthInfo ) 
        & Wr . EOL 
        & LID 
        & "KTreeEstChildCtLeftOfNl=" 
        & LbeStd . EstChildNoImage ( Self . KTreeEstChildCtLeftOfNl ) 
        & ", KTreeSyntTokCt=" 
        & LbeStd . LimitedTokCtImage ( Self . KTreeSyntTokCt ) 
        & ", KTreeElemCt=" 
        & ElemNoImage ( Self . KTreeElemCt ) 
    END KTreeRefImage 

(* EXPORTED: *) 
; PROCEDURE EstRefImageBrief 
    ( Self : EstRefTyp 
    ; <* UNUSED *> Indent := LbeStd . StdIndent 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

  = BEGIN (* EstRefImageBrief *) 
      RETURN 
        "Tokens={" 
        & LangUtil . TokImage ( Self . EstLeftTok , Lang )
      (*
        & "(" 
        & LbeStd . NumIdTokImage ( Self . EstLeftTok ) 
        & ")"
      *)
        & "," 
        & LangUtil . TokImage ( Self . EstTok , Lang ) 
      (*
        & "(" 
        & LbeStd . NumIdTokImage ( Self . EstTok ) 
        & ")" 
      *)
        & "," 
        & LangUtil . TokImage ( Self . EstRightTok , Lang ) 
      (*
        & "(" 
        & LbeStd . NumIdTokImage ( Self . EstRightTok ) 
        & ")" 
      *)
        & "} " 
        & EstNodeKindImage ( Self . EstNodeKind )
        & " EstNodeCt="
        & LbeStd . EstNodeNoImage ( Self . KTreeNodeCt ( ) + 1 ) 
        & " EstHeight=" 
        & KTreeHeightImage ( Self . EstHeight ) 
    END EstRefImageBrief

(* EXPORTED: *) 
; PROCEDURE EstRefImage 
    ( Self : EstRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

  = VAR LPrefix , LRest , LResult : TEXT
  ; VAR LID := Misc . Blanks ( Indent ) 

  ; BEGIN (* EstRefImage *)
      LPrefix := EstRefImageBrief ( Self , Indent , Lang )
    ; LRest := KTreeRefTyp . Image ( Self , Indent , NodeNo , ChildNo , Lang )
    ; LResult := LPrefix & LID & LRest
    ; RETURN LResult 
    END EstRefImage 

(* EXPORTED: *) 
; PROCEDURE KTreeLeafRefImage 
    ( Self : KTreeLeafRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 
  ; VAR LResult : TEXT 

  ; BEGIN (* KTreeLeafRefImage *) 
      LResult 
        := KTreeRefTyp . Image ( Self , Indent , NodeNo , ChildNo , Lang ) 
           & Wr . EOL 
           & LID 
           & "leaf array=" 
    ; IF Self . KTreeLeafArrayRef = NIL 
      THEN 
        LResult := LResult & "NIL" 
      ELSE 
        LResult 
          := LResult 
             & LeafArrayImage 
                 ( Self . KTreeLeafArrayRef , Indent + 11 , NodeNo , ChildNo ) 
      END (* IF *) 
    ; RETURN LResult 
    END KTreeLeafRefImage 

(* EXPORTED: *) 
; PROCEDURE KTreeNonleafRefImage 
    ( Self : KTreeNonleafRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 
  ; VAR LResult : TEXT 

  ; BEGIN (* KTreeNonleafRefImage *) 
      LResult 
        := KTreeRefTyp . Image ( Self , Indent , NodeNo , ChildNo , Lang ) 
           & Wr . EOL 
           & LID 
           & ",SliceEdgeInfoPair=" 
           & SliceEdgeInfoPairImage 
               ( Self . KTreeNonleafSliceEdgeInfoPair , Indent + 20 ) 
           & Wr . EOL 
           & LID 
           & "nonleaf array=" 
    ; IF Self . KTreeNonleafArrayRef = NIL 
      THEN 
        LResult := LResult & "NIL" 
      ELSE 
        LResult 
          := LResult 
             & NonleafArrayImage 
                 ( Self . KTreeNonleafArrayRef 
                 , Indent + 14 
                 , NodeNo 
                 , ChildNo 
                 ) 
      END (* IF *) 
    ; RETURN LResult 
    END KTreeNonleafRefImage 

(* EXPORTED: *) 
; PROCEDURE EstLeafRefImage 
    ( Self : EstLeafRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 

  ; BEGIN (* EstLeafRefImage *) 
      RETURN 
        EstRefTyp . Image ( Self , Indent , NodeNo , ChildNo , Lang ) 
        & Wr . EOL 
        & LID 
        & "leaf array=" 
        & LeafArrayImage 
            ( Self . EstLeafArrayRef , Indent + 11 , NodeNo , ChildNo ) 
    END EstLeafRefImage 

(* EXPORTED: *) 
; PROCEDURE EstNonleafRefImage 
    ( Self : EstNonleafRefTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : TEXT 

  = VAR LID := Misc . Blanks ( Indent ) 

  ; BEGIN (* EstNonleafRefImage *) 
      RETURN 
        EstRefTyp . Image ( Self , Indent , NodeNo , ChildNo , Lang ) 
        & Wr . EOL 
        & LID 
        & ",SliceEdgeInfoPair=" 
        & SliceEdgeInfoPairImage 
            ( Self . EstNonleafSliceEdgeInfoPair , Indent + 20 ) 
        & Wr . EOL 
        & LID 
        & "nonleaf array=" 
        & NonleafArrayImage 
            ( Self . EstNonleafArrayRef , Indent + 14 , NodeNo , ChildNo ) 
    END EstNonleafRefImage

; TYPE EstUniqueEmptyNodeTyp = EstLeafRefTyp OBJECT END  
  (* Use a proper subtype, to have a unique typecode, so the specials will not
     be called for and have to pass through, other Est nodes. *) 

; PROCEDURE EstTrailingSepSpecialWrite 
    ( <* UNUSED *> Self : Pickle . SpecialPublic 
    ; ref : REFANY 
    ; writer : Pickle . Writer 
    ) 
  RAISES { Pickle . Error } 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* EstTrailingSepSpecialWrite *) 
      IF TYPECODE ( ref ) # TYPECODE ( EstUniqueEmptyNodeTyp ) 
      THEN 
        RAISE 
          Pickle . Error ( "EstHs.EstTrailingSepSpecialWrite, bad TYPECODE" ) 
      ELSE 
        writer . write ( ref ) 
(* REVIEW: This is probably way too pedantic to write the node to the pickle
           and read it back, but just writing empty seems awfully delicate. *) 
      END (* IF *) 
    END EstTrailingSepSpecialWrite 

; PROCEDURE EstTrailingSepSpecialRead 
    ( <* UNUSED *> Self : Pickle . SpecialPublic 
    ; reader : Pickle . Reader 
    ; <* UNUSED *> id : Pickle . RefID 
    ) 
  : REFANY 
  RAISES { Pickle . Error } 

  = <* FATAL Rd . Failure *> 
    <* FATAL Rd . EndOfFile *> 
    <* FATAL Thread . Alerted *> 
    VAR LNode : EstUniqueEmptyNodeTyp 

  ; BEGIN (* EstTrailingSepSpecialRead *) 
      LNode := reader . read ( ) (* And throw it away. *) 
    ; RETURN UniqueEstNodeTrailingSep 
    END EstTrailingSepSpecialRead 

; BEGIN (* EstHs *) 
    UniqueEstNodeTrailingSep 
      := NEW 
           ( EstUniqueEmptyNodeTyp 
           , KTreeWidthInfo := WidthInfoNull 
           , KTreeSyntTokCt := 0 
           , KTreeElemCt := 0 
           , EstChildKindSet 
               := EstChildKindSetTyp 
                    { EstChildKindEstChild 
                    , EstChildKindNonNIL 
                    , EstChildKindFirstOfGroup 
                    , EstChildKindTrailingSep 
                    } 
           , EstTok := LbeStd . Tok__Empty 
           , EstLeftTok := LbeStd . Tok__Empty
           , EstRightTok := LbeStd . Tok__Empty 
           , EstNodeKind := EstNodeKindTyp . EstNodeKindPlain 
           , EstHeight := 0 
           , EstSemRef := NIL 
           , EstRepairCost := 0 (* CHECK: Is this what we want? *) 
           , EstLeafArrayRef := NIL
           ) 
  ; Pickle . RegisterSpecial 
      ( NEW 
          ( Pickle . Special 
          , sc := TYPECODE ( EstUniqueEmptyNodeTyp ) 
          , write := EstTrailingSepSpecialWrite 
          , read := EstTrailingSepSpecialRead 
          ) 
      ) 
  END EstHs 
. 
