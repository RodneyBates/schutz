
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LineMarks 

(* This module contains procedures for constructing a line mark 
   to the previous or the next new line, given an existing line 
   mark.  GetPrevLine can start with a mark to 
   any point within a line.  GetNextLine must begin 
   at a new line, because it also constructs the text image of 
   the line as it traverses it, and this requires knowing the 
   positions on the line. 
 
   GetPrevLine cannot always stop at the new line immediately to 
   the left, because for some line breaks and implicit new lines, 
   it can't tell.  It takes a mark which the caller 
   presumably believes lies to the left of the start mark. If 
   GetPrevLine hits this, it will stop there, even if it otherwise 
   wouldn't. 
*) 

; IMPORT Text 
; IMPORT Fmt 
; IMPORT Thread 

; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LangUtil 
; FROM LangUtil IMPORT FsKindTyp 
; IMPORT LbeStd 
; IMPORT Marks 
; FROM Marks IMPORT MarkKindTyp 
; IMPORT ModHs 
; IMPORT MessageCodes 
; IMPORT Options 
; IMPORT PaintHs  
; IMPORT PortTypes 
; IMPORT SharedStrings 
; IMPORT Strings 
; IMPORT TravUtil 

; IMPORT Assertions 

; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 

; TYPE AFT = MessageCodes . T 

(* VISIBLE: *) 
; PROCEDURE GetNextLine 
    ( Lang : LbeStd . LangTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; READONLY StartMark : Marks . TokMarkTyp 
    ; READONLY ExistingMark : Marks . TokMarkTyp 
    ; VAR DidHitExistingMark : BOOLEAN 
    ; VAR NewMark : Marks . TokMarkTyp 
    ; VAR AtEndOfImage : BOOLEAN 
      (* ^TRUE if StartMark was at EOI, though not necessarily the 
         rightmost Nl of an adjacent group. *) 
    ; VAR LineText : Strings . StringTyp 
    ; VAR BlankLineCt : LbeStd . LineNoTyp 
    ; VAR TextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
    ; VAR LineErrArrayRef : PaintHs . LineErrArrayRefTyp 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

  = CONST DefaultLineLen = 80 

  ; TYPE GnlStateTyp 
      = { GnlStateStartAtBeg 
          (* ^Start a line at the first new line of the token. *) 
        , GnlStateStartAtEnd 
          (* ^Start a line at the last new line of a token 
             that has a new line at the end. 
             (blank line, comment or text mod) *) 
        , GnlStatePassingNl 
          (* Passing a group of Nl's at the left end of the line. *) 
        , GnlStateInLine 
          (* ^Inside a line. *) 
        , GnlStateRightNlFound 
          (* ^We have found a new line at the right end, but 
              don't yet know that it is the rightmost new line 
              of the line boundary. *) 
        , GnlStateDoneWithLine 
        } 
    (* In GnlStateStartAtBeg and GnlStateStartAtEnd, TraverseEst and 
       TraverseFs jump into the middle of their respective trees, 
       using StartMark to decide where.  Once the starting 
       point is reached, must go in to another state to traverse left- 
       to-right. *) 
  ; TYPE GnlStateSetTyp = SET OF GnlStateTyp 

  ; CONST GnlStateSetStart 
      = GnlStateSetTyp 
          { GnlStateTyp . GnlStateStartAtBeg 
          , GnlStateTyp . GnlStateStartAtEnd 
          } 

  ; CONST GnlStateSetAtStartingNl 
      = GnlStateSetTyp 
          { GnlStateTyp . GnlStateStartAtBeg 
          , GnlStateTyp . GnlStatePassingNl 
          } 

  ; VAR GnlState : GnlStateTyp 
  ; VAR GnlCharPos : LbeStd . CharNoTyp 
  ; VAR GnlPrevTok : LbeStd . TokTyp 
  ; VAR GnlModTextIsToLeftOnLine : BOOLEAN 
  ; VAR GnlNextTextAttr : LbeStd . LimitedCharNoTyp 
  ; VAR GnlNextLineErr : PortTypes . Card32Typ 
  ; VAR GnlLastFmtNoOnLine : EstHs . FmtNoTyp 
  ; VAR GnlEstListChildrenToPass : LbeStd . EstChildNoTyp 
  ; VAR GnlTextAttrs : ARRAY [ 0 .. LbeStd . LimitedCharNoMax ] 
          OF PaintHs . TextAttrTyp
  ; VAR GnlLineErrs <* NOWARN *> 
        : ARRAY [ 0 .. LbeStd . LimitedCharNoMax * 10 ] 
          OF PaintHs . LineErrTyp 

(* Not needed anymore: 
  ; PROCEDURE GnlAccumChar ( Ch : CHAR ) RAISES { AssertionFailure } 
 
    = BEGIN (* GnlAccumChar *) 
        Assert 
          ( GnlCharPos < 4000 (* LbeStd . LimitedCharNoInfinity *)  
          , AFT . A_GnlAccumChar_OverfullLine 
          ) 
      ; Strings . AppendCharInPlace ( LineText , Ch ) 
      ; GnlCharPos := EstUtil . WidthSum ( GnlCharPos , 1 ) 
      END GnlAccumChar 
*) 

  ; PROCEDURE GnlBlankFillTo 
      ( ToCharPos : LbeStd . LimitedCharNoSignedTyp ) 
    RAISES { AssertionFailure } 

    = BEGIN (* GnlBlankFillTo *) 
        IF GnlCharPos # 4000 (* LbeStd . LimitedCharNoInfinity *)  
           AND GnlCharPos < ToCharPos 
        THEN 
          GnlPrevTok := LbeStd . Tok__Sep 
        ; IF GnlNextTextAttr = 0 
             OR GnlTextAttrs [ GnlNextTextAttr - 1 ] . TaDecoration 
                # PaintHs . TaDecPlain  
          THEN 
            GnlTextAttrs [ GnlNextTextAttr ] 
              := PaintHs . TextAttrTyp 
                   { TaCharPos := GnlCharPos 
                   , TaBgColor := PaintHs . TaBgColorPlain 
                   , TaFgColor := PaintHs . TaFgColorPlain  
                   , TaFont := PaintHs . TaFontPlain  
                   , TaDecoration := PaintHs . TaDecPlain  
                   } 
          ; INC ( GnlNextTextAttr ) 
          END (* IF *)
(* TODO: Fix so a subsequent, nonblank, text item can reuse the above
         attribute, by changing its FgColor and/or Font. *)  
        ; WHILE GnlCharPos < ToCharPos 
          DO Strings . AppendCharInPlace ( LineText , ' ' ) 
          ; GnlCharPos := EstUtil . WidthSum ( GnlCharPos , 1 ) 
          END (* WHILE *) 
        END (* IF *) 
      END GnlBlankFillTo

  ; CONST TextAttrModCmnt 
      = PaintHs . TextAttrTyp  
          { TaCharPos := 0  
          , TaBgColor := PaintHs . TaBgColorCmnt  
          , TaFgColor := PaintHs . TaFgColorCmnt   
          , TaFont := PaintHs . TaFontItalic   
          , TaDecoration := PaintHs . TaDecPlain  
          } 

  ; CONST TextAttrInsTok  
      = PaintHs . TextAttrTyp  
          { TaCharPos := 0  
          , TaBgColor := PaintHs . TaBgColorPlain    
            (* ^May be changed dynamically *) 
          , TaFgColor := PaintHs . TaFgColorPlain   
          , TaFont := PaintHs . TaFontPlain 
            (* ^May be changed dynamically *)    
          , TaDecoration := PaintHs . TaDecPlain  
            (* ^May be changed dynamically *) 
          } 

  ; CONST TextAttrAstString   
      = PaintHs . TextAttrTyp  
          { TaCharPos := 0  
          , TaBgColor := PaintHs . TaBgColorPlain  
            (* ^May be changed dynamically *) 
          , TaFgColor := PaintHs . TaFgColorIdent     
            (* ^May be changed dynamically *) 
          , TaFont := PaintHs . TaFontPlain    
          , TaDecoration := PaintHs . TaDecPlain  
            (* ^May be changed dynamically *) 
          } 

  ; CONST TextAttrInsertionRepair    
      = PaintHs . TextAttrTyp  
          { TaCharPos := 0  
          , TaBgColor := PaintHs . TaBgColorPlain   
          , TaFgColor := PaintHs . TaFgColorPlain     
          , TaFont := PaintHs . TaFontPlain    
          , TaDecoration := PaintHs . TaDecCaret   
          } 

  ; CONST TextAttrLexErrChars    
      = PaintHs . TextAttrTyp  
          { TaCharPos := 0  
          , TaBgColor := PaintHs . TaBgColorPlain   
          , TaFgColor := PaintHs . TaFgColorPlain     
          , TaFont := PaintHs . TaFontPlain    
          , TaDecoration := PaintHs . TaDecStrikeout   
          } 

  ; PROCEDURE GnlAccumString 
      ( StringRef : SharedStrings . T 
      ; TextAttr : PaintHs . TextAttrTyp := PaintHs . TextAttrDefault 
      ) 
    RAISES { AssertionFailure } 

    = VAR LStringLen : SharedStrings . LengthTyp 

    ; BEGIN (* GnlAccumString *) 
        IF StringRef # NIL 
        THEN 
          LStringLen := SharedStrings . Length ( StringRef ) 
        ; IF EstUtil . WidthSum ( GnlCharPos , LStringLen ) 
             < 4000 (* LbeStd . LimitedCharNoInfinity *)  
          THEN
            IF GnlNextTextAttr = 0 
               OR NOT PaintHs . TextAttrIsEqual 
                        ( GnlTextAttrs [ GnlNextTextAttr - 1 ] , TextAttr ) 
            THEN 
              WITH WAttr = GnlTextAttrs [ GnlNextTextAttr ] 
              DO 
                WAttr := TextAttr 
              ; WAttr . TaCharPos := GnlCharPos 
              END 
            ; INC ( GnlNextTextAttr ) 
            END (* IF *) 
          ; TRY 
              FOR I := 0 TO LStringLen - 1 
              DO Strings . AppendCharInPlace 
                   ( LineText , SharedStrings . IthChar ( StringRef , I ) ) 
              ; GnlCharPos := EstUtil . WidthSum ( GnlCharPos , 1 ) 
              END (* FOR *) 
            EXCEPT SharedStrings . SsOutOfBounds 
            => RAISE AssertionFailure ( "SharedStrings.SsOutOfBounds" )  
            END (* TRY EXCEPT *) 
          ELSE 
            CantHappen ( AFT . A_GnlAccumString_OverfullLine ) 
          END (* IF *) 
        END (* IF *) 
      END GnlAccumString 

  ; PROCEDURE GnlAccumText 
      ( TextValue : TEXT  
      ; TextAttr : PaintHs . TextAttrTyp := PaintHs . TextAttrDefault 
      ) 
    RAISES { AssertionFailure } 

    = VAR LStringLen : SharedStrings . LengthTyp 

    ; BEGIN (* GnlAccumText *) 
        IF TextValue # NIL 
        THEN 
          LStringLen := Text . Length ( TextValue ) 
        ; IF EstUtil . WidthSum ( GnlCharPos , LStringLen ) 
             < 4000 (* LbeStd . LimitedCharNoInfinity *)  
          THEN
            IF GnlNextTextAttr = 0 
               OR NOT PaintHs . TextAttrIsEqual 
                        ( GnlTextAttrs [ GnlNextTextAttr - 1 ] , TextAttr ) 
            THEN 
              WITH WAttr = GnlTextAttrs [ GnlNextTextAttr ] 
              DO 
                WAttr := TextAttr 
              ; WAttr . TaCharPos := GnlCharPos 
              END 
            ; INC ( GnlNextTextAttr ) 
            END (* IF *) 
          ; FOR I := 0 TO LStringLen - 1 
            DO Strings . AppendCharInPlace 
                 ( LineText , Text . GetChar ( TextValue , I ) ) 
            ; GnlCharPos := EstUtil . WidthSum ( GnlCharPos , 1 ) 
            END (* FOR *) 
          ELSE 
            CantHappen ( AFT . A_GnlAccumText_OverfullLine ) 
          END (* IF *) 
        END (* IF *) 
      END GnlAccumText  

  ; PROCEDURE GnlTraverseEst 
      ( EstRef : LbeStd . EstRootTyp 
      ; KindSet : EstHs . EstChildKindSetTyp 
      ; RootAbsNodeNo : LbeStd . EstNodeNoTyp 
      ; FsRuleNodeRef : LangUtil . FsNodeRefTyp 
      ; EstFmtKind : LangUtil . FmtKindTyp 
      ; EstIndentPos1 : LbeStd . LimitedCharNoTyp 
      ; EstIndentPosN : LbeStd . LimitedCharNoTyp 
      (* EstIndentPos1 and EstIndentPosN are maintained assuming
         the Est subtree is formatted vertically. 
      *) 
      ; IsModTok : BOOLEAN 
      ) 
    RAISES { AssertionFailure , Thread . Alerted } 

    = VAR GnlTeStartFmtNo : EstHs . FmtNoTyp 
    ; VAR GnlTeEstTravInfo : TravUtil . EstTravInfoTyp 
    ; VAR GnlTeRMChildRelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR GnlTeRMChildRef : LbeStd . EstRootTyp 
    ; VAR GnlTeRMChildKindSet : EstHs . EstChildKindSetTyp 
          (* ^GnlTeRMChildRelNodeNo, GnlTeRMChildRef, and GnlTeRMChildKindSet 
             trail behind the current child of GnlTeEstTravInfo, whenever that 
             child moves rightward.  The only time they are guaranteed to be 
             set is when the current child is off the right end, at which time, 
             they denotes the rightmost existing child, hence the name RM. 
          *) 
    ; VAR GnlTeInitialFsLeafRef : LangUtil . FsNodeRefTyp := NIL  
    ; VAR GnlTeIndentPos : LbeStd . LimitedCharNoTyp 
    ; VAR GnlTeIsFirstLine : BOOLEAN := TRUE   
    (* GnlTeIsFirstLine and GnlTeIndentPos are maintained only in descending 
       and forward states, and as if the Est subtree were formatted vertically. 
    *) 
    ; VAR GnlTeDescendedInto : BOOLEAN 

    ; PROCEDURE GnlTeIncEstChild ( ) 
      RAISES { AssertionFailure } 

      = BEGIN 
          GnlTeRMChildRelNodeNo := GnlTeEstTravInfo . EtiChildRelNodeNo 
        ; GnlTeRMChildRef := GnlTeEstTravInfo . EtiChildLeafElem . LeChildRef 
        ; GnlTeRMChildKindSet 
            := GnlTeEstTravInfo . EtiChildLeafElem . LeKindSet 
        ; TravUtil . IncEstChild ( GnlTeEstTravInfo ) 
        END GnlTeIncEstChild 

    ; PROCEDURE GnlTeSetIndentInfo ( )  

      = BEGIN 
          GnlTeIsFirstLine 
            := GnlTeEstTravInfo . EtiChildNo 
               < GnlTeEstTravInfo . EtiParentRef . KTreeEstChildCtLeftOfNl 
        ; IF GnlTeIsFirstLine 
          THEN
            GnlTeIndentPos := EstIndentPos1 
          ELSE
            GnlTeIndentPos := EstIndentPosN 
          END (* IF *) 
        END GnlTeSetIndentInfo 

(*
(* All calls on this were either trivially true or possibly false. *) 
    ; PROCEDURE GnlTeCheckIndentInfo 
        ( FsNodeRef : LangUtil . FsNodeRefTyp )  
      RAISES { AssertionFailure } 

      = VAR LIsFirstLine : BOOLEAN 

      ; BEGIN 
          Assert 
            ( ( GnlTeInitialFsLeafRef # NIL ) = GnlTeDescendedInto 
            , AFT . A_GnlTeCheckIndentInfo_DescendedInto_Mismatch
            ) 
        ; IF GnlTeInitialFsLeafRef # NIL 
          THEN 
            Assert 
              ( FsNodeRef . FsKind = GnlTeInitialFsLeafRef . FsKind  
              , AFT . A_GnlTeCheckIndentInfo_FsKind_Mismatch
              ) 
          ; Assert 
              ( FsNodeRef . FsFmtNo = GnlTeInitialFsLeafRef . FsFmtNo  
              , AFT . A_GnlTeCheckIndentInfo_FsFmtNo_Mismatch
              ) 
          END (* IF *) 
        ; LIsFirstLine 
            := GnlTeEstTravInfo . EtiChildNo 
               < GnlTeEstTravInfo . EtiParentRef . KTreeEstChildCtLeftOfNl 
        ; Assert 
            ( LIsFirstLine = GnlTeIsFirstLine 
            , AFT . A_GnlTeCheckIndentInfo_IsFirstLine_mismatch
            ) 
        END GnlTeCheckIndentInfo 
*) 

    ; PROCEDURE GnlTeBlanksBeforeTok 
        ( Tok : LbeStd . TokTyp 
        ; FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : LangUtil . FmtKindTyp 
        ) 
      RAISES { AssertionFailure } 

      = BEGIN (* GnlTeBlanksBeforeTok *) 
          IF TRUE OR GnlState = GnlStateTyp . GnlStateStartAtBeg 
          THEN 
          (* StartAtBeg can only happen if we began at the end 
             of a mod with a new line after, in 
             GnlStateStartAtEnd. 
          *) 
            GnlBlankFillTo 
              ( TravUtil . PosForTok 
                  ( Lang 
                  , FmtKind 
                  , GnlModTextIsToLeftOnLine 
                  , GnlCharPos 
                  , GnlTeIndentPos 
                  , FsNodeRef . FsIndentCode 
                  , GnlPrevTok 
                  , Tok 
                  ) 
              ) 
          END (* IF *) 
(* TODO: Maybe just inline this. *) 
        END GnlTeBlanksBeforeTok 

    ; PROCEDURE GnlTeAstString 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : LangUtil . FmtKindTyp 
        ) 
      RAISES { AssertionFailure } 

      = VAR LTok : LbeStd . TokTyp 
      ; VAR LTextAttr : PaintHs . TextAttrTyp 

      ; BEGIN 
          CASE GnlState <* NOWARN *>
          OF GnlStateTyp . GnlStatePassingNl  
          , GnlStateTyp . GnlStateInLine   
          => IF GnlTeEstTravInfo . EtiNodeRef # NIL 
             THEN 
               LTok := SharedStrings . Tok ( GnlTeEstTravInfo . EtiStringRef ) 
             ; GnlTeBlanksBeforeTok ( LTok , FsNodeRef , FmtKind ) 
             ; IF EstHs . EstChildKindContainsInsertionRepair IN KindSet 
               THEN (* Repair placeholder that Parser proposes to insert. *) 
                 LTextAttr := TextAttrInsertionRepair 
               ; LTextAttr . TaFgColor := PaintHs . TaFgColorPlaceholder  
               ELSE 
                 LTextAttr := TextAttrAstString 
               ; IF EstUtil . EstIsPlaceholder 
                      ( Lang , GnlTeEstTravInfo . EtiStringRef ) 
                 THEN 
                   LTextAttr . TaFgColor := PaintHs . TaFgColorPlaceholder 
                 ELSIF LangUtil . IsLiteral ( Lang , LTok ) 
(* TODO: Distinguish literals that could contain "white" space, and thus
         benefit from a distinct background color from those that do not.
*) 
                 THEN
                   LTextAttr . TaBgColor := PaintHs . TaBgColorLiteral
                 ; LTextAttr . TaFgColor := PaintHs . TaFgColorLiteral 
                 END (* IF *) 
               ; IF IsModTok 
                 THEN 
                   LTextAttr . TaDecoration := PaintHs . TaDecStrikeout 
                 END (* IF *) 
               END (* IF *) 
             ; GnlAccumString ( GnlTeEstTravInfo . EtiStringRef , LTextAttr  ) 
             ; GnlState := GnlStateTyp . GnlStateInLine 
             ; GnlPrevTok := LTok 
             END (* IF *) 
          | GnlStateTyp . GnlStateRightNlFound 
          => GnlState := GnlStateTyp . GnlStateDoneWithLine 
          END (* CASE *) 
        END GnlTeAstString 

    ; PROCEDURE GnlTeTraverseFsFixedChildren 
        ( ParentFsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : LangUtil . FmtKindTyp 
        ; InitialFsChildNo : LangUtil . FsChildNoTyp 
        ) 
      RAISES { AssertionFailure , Thread . Alerted } 

      = VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* GnlTeTraverseFsFixedChildren *) 
          LFsChildNo := InitialFsChildNo 
        ; IF ParentFsNodeRef . FsChildren = NIL 
          THEN LFsChildCt := 0 
          ELSE LFsChildCt := NUMBER ( ParentFsNodeRef . FsChildren ^ ) 
          END (* IF *) 
        ; LOOP 
            IF LFsChildNo >= LFsChildCt  
            THEN 
              EXIT 
            ELSE 
              GnlTeTraverseFs 
                ( ParentFsNodeRef . FsChildren ^ [ LFsChildNo ] , FmtKind ) 
            ; IF GnlState = GnlStateTyp . GnlStateDoneWithLine 
              THEN 
                EXIT 
              ELSE 
                INC ( LFsChildNo ) 
              END (* IF *) 
            END (* IF *) 
          END (* LOOP *) 
        END GnlTeTraverseFsFixedChildren 

    ; PROCEDURE GnlTeTraverseFsListChildren 
        ( ParentFsNodeRef : LangUtil . FsNodeRefTyp 
        ; FmtKind : LangUtil . FmtKindTyp 
        ; InitialFsChildNo : LangUtil . FsChildNoTyp 
        ) 
      RAISES { AssertionFailure , Thread . Alerted } 

      = VAR LFsChildCt : LangUtil . FsChildNoTyp 
      ; VAR LFsChildNo : LangUtil . FsChildNoTyp 
      ; VAR LFinalFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* GnlTeTraverseFsListChildren *) 
          LFsChildCt := NUMBER ( ParentFsNodeRef . FsChildren ^ ) 
        ; LFsChildNo := InitialFsChildNo 
        ; IF ParentFsNodeRef . FsKind 
             IN LangUtil . FsKindSetEstListTrail 
             AND GnlTeEstTravInfo . EtiParentRef . EstNodeKind 
                 = EstHs . EstNodeKindTyp . EstNodeKindTrail
          THEN LFinalFsChildNo := LFsChildCt - 1 
          ELSE LFinalFsChildNo := 0 
          END (* IF *) 
        ; LOOP 
            GnlTeTraverseFs 
              ( ParentFsNodeRef . FsChildren ^ [ LFsChildNo ] , FmtKind ) 
          ; IF GnlState = GnlStateTyp . GnlStateDoneWithLine 
            THEN 
              EXIT 
            ELSIF LFsChildNo = LFinalFsChildNo  
                  AND GnlTeEstTravInfo . EtiChildNo 
                      >= GnlTeEstTravInfo . EtiChildCt 
            THEN 
              EXIT 
            ELSE 
              LFsChildNo := ( LFsChildNo + 1 ) MOD LFsChildCt 
            END (* IF *) 
          END (* LOOP *) 
        END GnlTeTraverseFsListChildren 

    ; PROCEDURE GnlTeTraverseFs 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FsFmtKind : LangUtil . FmtKindTyp 
        ) 
      RAISES { AssertionFailure , Thread . Alerted }
      (* Does not handle Fs tree root nodes. *) 

      = PROCEDURE GnlTeTfsEstSubtree ( IsModTok : BOOLEAN ) 
        RAISES { AssertionFailure , Thread . Alerted } 
        (* Does NOT handle ModTok. *) 

        = VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR LChildFmtKind : LangUtil . FmtKindTyp 
        ; VAR LChildIndentPos1 : LbeStd . LimitedCharNoTyp 
        ; VAR LChildIndentPosN : LbeStd . LimitedCharNoTyp 

        ; BEGIN (* GnlTeTfsEstSubtree *) 
            LChildFsNodeRef 
              := LangUtil . FsRuleForEstChild  
                   ( Lang , FsNodeRef , GnlTeEstTravInfo . EtiChildLeafElem ) 
          ; CASE GnlState 
            OF GnlStateTyp . GnlStateStartAtEnd 
            , GnlStateTyp . GnlStateStartAtBeg 
            => TravUtil . ChildIndentPositions 
                ( Lang 
                , FsNodeRef 
                , EstIndentPos1 
                , EstIndentPosN
                , (* VAR *) ChildIndentPos1 := LChildIndentPos1 
                , (* VAR *) ChildIndentPosN := LChildIndentPosN 
                , IsFirstLine := GnlTeIsFirstLine 
                ) 
            ; LChildFmtKind 
                := TravUtil . FmtKindForEstDescending 
                    ( FsKind := LChildFsNodeRef . FsKind 
                    , ParentFmtKind := FsFmtKind 
                    , FirstLineIndentPos := LChildIndentPos1 
                    , EstRef 
                        := GnlTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                    , StartMark := StartMark 
                    , StartMarkIsKnownNl := TRUE 
                    ) 

            ELSE 
              TravUtil . ChildIndentPositions 
                ( Lang 
                , FsNodeRef 
                , EstIndentPos1 
                , EstIndentPosN
                , (* VAR *) ChildIndentPos1 := LChildIndentPos1 
                , (* VAR *) ChildIndentPosN := LChildIndentPosN 
                , IsFirstLine := GnlTeIsFirstLine 
                ) 
            ; LChildFmtKind 
                := TravUtil . FmtKindForEstTraversing 
                    ( Lang := Lang 
                    , CharPos := GnlCharPos 
                    , ModTextIsToLeftOnLine := GnlModTextIsToLeftOnLine 
                    , PrevTok := GnlPrevTok 
                    , FsKind := LChildFsNodeRef . FsKind 
                    , ParentFmtKind := FsFmtKind 
                    , FirstLineIndentPos := LChildIndentPos1 
                    , EstRef 
                        := GnlTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                    ) 
            END (* CASE *) 
          ; GnlTraverseEst 
              ( GnlTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              , GnlTeEstTravInfo . EtiChildLeafElem . LeKindSet  
              , RootAbsNodeNo + GnlTeEstTravInfo . EtiChildRelNodeNo 
              , FsRuleNodeRef := LChildFsNodeRef 
              , EstFmtKind := LChildFmtKind
              , EstIndentPos1 := LChildIndentPos1 
              , EstIndentPosN := LChildIndentPosN 
              , IsModTok := IsModTok   
              ) 
          ; IF GnlState # GnlStateTyp . GnlStateDoneWithLine 
            THEN 
              GnlTeIncEstChild ( ) 
            END (* IF *) 
          END GnlTeTfsEstSubtree 

      (* Leading mods. *) 

      ; PROCEDURE GnlTeTfsModBlankLineInterior 
          ( ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* GnlTeTfsModBlankLineInterior *) 
            NewMark 
              := Marks . TokMarkTyp 
                   { EstNodeNo 
                       := RootAbsNodeNo 
                          + GnlTeEstTravInfo . EtiChildRelNodeNo 
                   , EstNodeCt := 1 
                   , Kind := MarkKindTyp . BlankLine 
                   , FmtNo := FsNodeRef . FsFmtNo 
                   , StartAtEnd := TRUE
                   , IsImpliedNewLine := FALSE 
                   , Tok := LbeStd . Tok__BlankLine  
                   } 
          ; IF Marks . Equal ( ExistingMark , NewMark ) 
            THEN DidHitExistingMark := TRUE 
            END (* IF *) 
          ; BlankLineCt := ModBlankLine . ModBlankLineCt 
          ; LineText 
              := Strings . Empty ( EventualLengthHint := DefaultLineLen ) 
          ; GnlTeIncEstChild ( ) 
          ; GnlCharPos := 0 
          ; GnlModTextIsToLeftOnLine := FALSE 
          ; GnlPrevTok := LbeStd . Tok__BegOfLine 
          ; GnlLastFmtNoOnLine := EstHs . FmtNoUnknown  
          ; GnlEstListChildrenToPass := 0 (* Dead *) 
          ; GnlState := GnlStateTyp . GnlStateRightNlFound 
          END GnlTeTfsModBlankLineInterior 

      ; PROCEDURE GnlTeTfsModBlankLine 
          ( ModBlankLine : ModHs . ModBlankLineTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* GnlTeTfsModBlankLine *) 
            CASE GnlState 

            OF GnlStateTyp . GnlStateStartAtEnd 
            => GnlCharPos := 0  
            ; GnlTeIncEstChild ( ) 
            ; GnlState := GnlStateTyp . GnlStatePassingNl 

            | GnlStateTyp . GnlStateStartAtBeg 
            => GnlCharPos := 0  
            ; GnlTeTfsModBlankLineInterior ( ModBlankLine ) 

            | GnlStateTyp . GnlStatePassingNl 
            => (* We started at a non-rightmost Nl of a group, 
                  and this is the rightmost. *) 
               GnlTeTfsModBlankLineInterior ( ModBlankLine ) 

            | GnlStateTyp . GnlStateInLine 
            => NewMark 
                 := Marks . TokMarkTyp 
                      { EstNodeNo 
                          := RootAbsNodeNo 
                             + GnlTeEstTravInfo . EtiChildRelNodeNo 
                      , EstNodeCt := 1 
                      , Kind := MarkKindTyp . BlankLine 
                      , FmtNo := FsNodeRef . FsFmtNo 
                      , StartAtEnd := FALSE 
                      , IsImpliedNewLine := FALSE 
                      , Tok := LbeStd . Tok__BlankLine 
                      } 
            ; IF Marks . Equal ( ExistingMark , NewMark ) 
              THEN DidHitExistingMark := TRUE 
              END (* IF *) 
            ; GnlState := GnlStateTyp . GnlStateDoneWithLine 
              (* There is at least one blank line, so this is the 
                 rightmost adjacent Nl. *) 

            | GnlStateTyp . GnlStateRightNlFound 
            => NewMark 
                 := Marks . TokMarkTyp 
                      { EstNodeNo 
                          := RootAbsNodeNo 
                             + GnlTeEstTravInfo . EtiChildRelNodeNo 
                      , EstNodeCt := 1 
                      , Kind := MarkKindTyp . BlankLine 
                      , FmtNo := FsNodeRef . FsFmtNo 
                      , StartAtEnd := FALSE 
                      , IsImpliedNewLine := FALSE 
                      , Tok := LbeStd . Tok__BlankLine 
                      } 
            ; IF Marks . Equal ( ExistingMark , NewMark ) 
              THEN DidHitExistingMark := TRUE 
              END (* IF *) 
            ; GnlState := GnlStateTyp . GnlStateDoneWithLine 
              (* There is at least one blank line, so this is the 
                 rightmost adjacent Nl. *) 
            ELSE 
              CantHappen ( AFT . A_GnlTeTfsModBlankLine_BadState ) 
            END (* CASE GnlState *) 
          END GnlTeTfsModBlankLine 

      ; PROCEDURE GnlTeTfsModCmntInterior ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* GnlTeTfsModCmntInterior *) 
            TYPECASE ModCmnt <* NOWARN *>  
            OF ModHs . ModCmntLeadingFixedTyp 
            , ModHs . ModCmntTrailingFixedTyp 
            => GnlBlankFillTo ( ModCmnt . ModCmntFromPos ) 
            | ModHs . ModCmntLeadingRelativeTyp 
              (* ModCmntFromPos is relative to indent pos. *) 
            => GnlBlankFillTo
                 ( EstUtil . WidthSumSigned   
                     ( TravUtil . IndentPos 
                         ( Lang 
                         , GnlTeIndentPos 
                         , FsNodeRef . FsIndentCode 
                         ) 
                    , ModCmnt . ModCmntFromPos 
                    ) 
                 ) 
            | ModHs . ModCmntTrailingRelativeTyp 
              (* ModCmntFromPos is relative to previous token. *) 
            => GnlBlankFillTo 
                 ( EstUtil . WidthSumSigned  
                     ( GnlCharPos , ModCmnt . ModCmntFromPos )  
                     (* ^SameLine comment implies there is a token 
                        preceeding, so no need to consider IndentPos. *) 
                 ) 
            END (* TYPECASE *) 
          ; GnlAccumString 
              ( ModCmnt . ModCmntStringRef , TextAttrModCmnt ) 
          ; IF ModCmnt . ModCmntNlAfter 
            THEN 
              NewMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo 
                         := RootAbsNodeNo 
                            + GnlTeEstTravInfo . EtiChildRelNodeNo 
                     , EstNodeCt := 1 
                     , Kind := MarkKindTyp . Plain 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := TRUE 
                     , IsImpliedNewLine := FALSE 
                     , Tok := LbeStd . Tok__CmntAtEndOfLine  
                     } 
            ; IF Marks . Equal ( ExistingMark , NewMark ) 
              THEN DidHitExistingMark := TRUE 
              END (* IF *) 
            ; GnlCharPos := 0 
            ; GnlModTextIsToLeftOnLine := FALSE 
            ; GnlPrevTok := LbeStd . Tok__BegOfLine 
            ; GnlLastFmtNoOnLine := EstHs . FmtNoUnknown  
            ; GnlEstListChildrenToPass := 0 (* Dead *) 
            ; GnlState := GnlStateTyp . GnlStateRightNlFound 
            ELSE 
              GnlState := GnlStateTyp . GnlStateInLine 
            END (* IF *)
          ; GnlPrevTok := LbeStd . Tok__Cmnt 
          ; GnlTeIncEstChild ( ) 
          END GnlTeTfsModCmntInterior 

      ; PROCEDURE GnlTeTfsModCmnt ( ModCmnt : ModHs . ModCmntTyp ) 
        RAISES { AssertionFailure } 

        = VAR LNlBefore : BOOLEAN 
        ; VAR LIsImpliedNewLine : BOOLEAN 

        ; BEGIN (* GnlTeTfsModCmnt *) 
            IF GnlState = GnlStateTyp . GnlStateStartAtEnd 
            THEN (* Consume the mod and start after it. *) 
              Assert 
                ( ModCmnt . ModCmntNlAfter 
                , AFT . A_GnlTeTfsModCmnt_StartAtEndNoNlAfter 
                ) 
            ; GnlCharPos := 0  
            ; GnlTeIncEstChild ( ) 
            ; GnlState := GnlStateTyp . GnlStatePassingNl 
            ELSE 
              IF ModCmnt . ModCmntNlBefore 
              THEN 
                LNlBefore := TRUE 
              ; LIsImpliedNewLine := FALSE 
              ELSE 
                TYPECASE ModCmnt 
                OF ModHs . ModCmntLeadingFixedTyp 
                , ModHs . ModCmntTrailingFixedTyp 
                => LNlBefore := GnlCharPos > ModCmnt . ModCmntFromPos 
                ; LIsImpliedNewLine := LNlBefore 
                ELSE 
                  LNlBefore := FALSE  
                ; LIsImpliedNewLine := FALSE 
                END (* TYPECASE *) 
              END (* IF *) 
            ; IF GnlState = GnlStateTyp . GnlStateStartAtBeg 
              THEN
                GnlCharPos := 0  
              ; Assert
                  ( LIsImpliedNewLine = StartMark . IsImpliedNewLine 
                  , AFT . A_GnlTeTfsModCmnt_ImpliedNewLineDisagreement  
                  ) 
              END (* IF *) 
            ; IF LNlBefore 
              THEN 
                CASE GnlState 
                OF GnlStateTyp . GnlStateStartAtBeg 
                => GnlTeTfsModCmntInterior ( ModCmnt ) 

                | GnlStateTyp . GnlStatePassingNl 
                => GnlTeTfsModCmntInterior ( ModCmnt ) 

                | GnlStateTyp . GnlStateInLine 
                , GnlStateTyp . GnlStateRightNlFound 
                => NewMark 
                     := Marks . TokMarkTyp 
                          { EstNodeNo 
                              := RootAbsNodeNo 
                                 + GnlTeEstTravInfo . EtiChildRelNodeNo 
                          , EstNodeCt := 1 
                          , Kind := MarkKindTyp . Plain 
                          , FmtNo := FsNodeRef . FsFmtNo 
                          , StartAtEnd := FALSE 
                          , IsImpliedNewLine := LIsImpliedNewLine  
                          , Tok := LbeStd . Tok__Cmnt  
                          } 
                ; IF Marks . Equal ( ExistingMark , NewMark ) 
                  THEN DidHitExistingMark := TRUE 
                  END (* IF *) 
                ; GnlState := GnlStateTyp . GnlStateDoneWithLine 
                  (* A comment always has non-empty text, so there can be 
                     no adjacent Nl further to the right. *) 

                ELSE 
                  CantHappen ( AFT . A_GnlTeTfsModCmnt_BadStateNlBefore ) 
                END (* CASE *) 
              ELSE (* No Nl Before *) 
                CASE GnlState 
                OF GnlStateTyp . GnlStateRightNlFound 
                => GnlState := GnlStateTyp . GnlStateDoneWithLine 
                | GnlStateTyp . GnlStatePassingNl 
                , GnlStateTyp . GnlStateInLine 
                => GnlTeTfsModCmntInterior ( ModCmnt ) 
                ELSE 
                  CantHappen ( AFT . A_GnlTeTfsModCmnt_BadState ) 
                END (* CASE *) 
              END (* IF *) 
            END (* IF *) 
          END GnlTeTfsModCmnt 

      ; PROCEDURE GnlTeTfsModTextInterior 
          ( ModText : ModHs . ModTextTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* GnlTeTfsModTextInterior *) 
            Assert 
              ( GnlCharPos <= ModText . ModTextFromPos 
              , AFT . A_GnlTeTfsModTextInterior_ForcedNl 
              ) 
          ; GnlBlankFillTo ( ModText . ModTextFromPos ) 
          ; GnlAccumString 
              ( ModText . ModTextStringRef , PaintHs . TextAttrTouched ) 
          ; IF ModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
            THEN (* New line after. *) 
              NewMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo 
                         := RootAbsNodeNo 
                            + GnlTeEstTravInfo . EtiChildRelNodeNo 
                     , EstNodeCt := 1 
                     , Kind := MarkKindTyp . Plain 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := TRUE 
                     , IsImpliedNewLine := FALSE 
                     , Tok := LbeStd . Tok__ModText  
                     } 
            ; IF Marks . Equal ( ExistingMark , NewMark ) 
              THEN DidHitExistingMark := TRUE 
              END (* IF *) 
            ; GnlCharPos := 0 
            ; GnlModTextIsToLeftOnLine := FALSE 
            ; GnlPrevTok := LbeStd . Tok__BegOfLine 
            ; GnlLastFmtNoOnLine := EstHs . FmtNoUnknown  
            ; GnlEstListChildrenToPass := 0 (* Dead *) 
            ; GnlState := GnlStateTyp . GnlStateRightNlFound 
            ELSE 
              Assert 
                ( GnlCharPos <= ModText . ModTextToPos 
                , AFT . A_GnlTeTfsModText_NoTrailingSep 
(* CHECK: This was once GnlCharPos < ModText . ModTextToPos. 
        inserting a Nl immediately before a token created a ModText 
        that violated this.  We need such a ModText to give a FromPos, 
        even tho' it is otherwise empty.  This assertion is 
        in 5 places, all messages ending in NoTrailingSep *) 
                ) 
            ; GnlPrevTok := LbeStd . Tok__ModText 
            ; GnlBlankFillTo ( ModText . ModTextToPos ) 
            ; GnlModTextIsToLeftOnLine := TRUE 
            ; GnlState := GnlStateTyp . GnlStateInLine 
            END (* IF *) 
          ; GnlTeIncEstChild ( ) 
          END GnlTeTfsModTextInterior 

      ; PROCEDURE GnlTeTfsModText ( ModText : ModHs . ModTextTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* GnlTeTfsModText *) 
            IF GnlState = GnlStateTyp . GnlStateStartAtEnd 
            THEN (* Consume the mod and start after it. *) 
              Assert 
                ( ModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
                , AFT . A_GnlTeTfsModTextStartAtEnd_NoNlAfter 
                ) 
            ; GnlCharPos := 0  
            ; GnlTeIncEstChild ( ) 
            ; GnlState := GnlStateTyp . GnlStatePassingNl 
            ELSIF ModText . ModTextLeftTokToPos = 0 
            THEN (* Nl before *) 
              CASE GnlState 
              OF GnlStateTyp . GnlStateStartAtBeg 
              => GnlCharPos := 0  
              ; GnlTeTfsModTextInterior ( ModText ) 
              | GnlStateTyp . GnlStatePassingNl 
              => GnlCharPos := 0  
              ; GnlTeTfsModTextInterior ( ModText ) 
              | GnlStateTyp . GnlStateInLine 
              , GnlStateTyp . GnlStateRightNlFound 
              => (* We are at the end of the line. *) 
                 NewMark 
                   := Marks . TokMarkTyp 
                        { EstNodeNo 
                            := RootAbsNodeNo 
                               + GnlTeEstTravInfo . EtiChildRelNodeNo 
                        , EstNodeCt := 1 
                        , Kind := MarkKindTyp . Plain 
                        , IsImpliedNewLine := FALSE 
                        , FmtNo := FsNodeRef . FsFmtNo 
                        , StartAtEnd := FALSE 
                        , Tok := LbeStd . Tok__ModText 
                        } 
              ; IF Marks . Equal ( ExistingMark , NewMark ) 
                THEN DidHitExistingMark := TRUE 
                END (* IF *) 
              ; IF ModText . ModTextFromPos = 0 
                   AND ModText . ModTextToPos = 0 
                THEN (* Text is empty.  There could be another adjacent Nl 
                        further to the left. *) 
                  GnlPrevTok := LbeStd . Tok__ModText 
                ; GnlModTextIsToLeftOnLine := TRUE 
                ; GnlTeIncEstChild ( ) 
                ; GnlCharPos := 0 
                ; GnlModTextIsToLeftOnLine := FALSE 
                ; GnlPrevTok := LbeStd . Tok__BegOfLine 
                ; GnlLastFmtNoOnLine := EstHs . FmtNoUnknown  
                ; GnlEstListChildrenToPass := 0 (* Dead *) 
                ; GnlState := GnlStateTyp . GnlStateRightNlFound 
                ELSE (* Stop at the Nl before. *) 
                  GnlState := GnlStateTyp . GnlStateDoneWithLine 
                END (* IF *) 
              ELSE 
                CantHappen ( AFT . A_GnlTeTfsModText_BadState ) 
              END (* CASE *) 
            ELSE (* No Nl Before *) 
              CASE GnlState 
              OF GnlStateTyp . GnlStateRightNlFound 
              => GnlState := GnlStateTyp . GnlStateDoneWithLine 
              | GnlStateTyp . GnlStateInLine 
              , GnlStateTyp . GnlStatePassingNl 
              => GnlTeTfsModTextInterior ( ModText ) 
              ELSE 
                CantHappen ( AFT . A_GnlTeTfsModText_BadState ) 
              END (* CASE *) 
            END (* IF *) 
          END GnlTeTfsModText 

      ; PROCEDURE GnlTeTfsModTok ( EstRef : EstHs . EstRefTyp ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 

        ; BEGIN 
            CASE GnlState <* NOWARN *> 
            OF GnlStateTyp . GnlStateStartAtBeg 
            , GnlStateTyp . GnlStateStartAtEnd 
            => IF StartMark . EstNodeNo 
                  = RootAbsNodeNo + GnlTeEstTravInfo . EtiChildRelNodeNo 
                  AND StartMark . Kind = MarkKindTyp . Plain 
              THEN (* StartMark denotes this ModTok.  This means we are
                      starting at an implied Nl for the ModTok. *)  
                GnlCharPos := GnlTeIndentPos 
              ; GnlState := GnlStateTyp . GnlStatePassingNl 
              ELSE (* Stay in GnlState to descend through the ModTok. *) 
              END (* IF *) 

            | GnlStateTyp . GnlStatePassingNl 
            => 

            | GnlStateTyp . GnlStateInLine   
            , GnlStateTyp . GnlStateRightNlFound    
            => IF EstUtil . CharPosPlusWidthInfo 
                    ( GnlCharPos , EstRef . KTreeWidthInfo ) 
                  > Options . RightMargin 
              THEN (* Implied Nl before the ModTok, which ends this line. 
                      However, it may not be the RM Nl of the group, so
                      keep traversing. *)
                NewMark 
                  := Marks . TokMarkTyp 
                       { EstNodeNo 
                           := RootAbsNodeNo 
                              + GnlTeEstTravInfo . EtiChildRelNodeNo 
                       , EstNodeCt := EstUtil . EstNodeCt ( EstRef ) 
                       , Kind := MarkKindTyp . Plain 
                       , IsImpliedNewLine := FALSE 
                       , FmtNo := FsNodeRef . FsFmtNo 
                       , StartAtEnd := FALSE 
                       , Tok := EstRef . EstTok 
                       } 
              ; IF Marks . Equal ( ExistingMark , NewMark ) 
                THEN DidHitExistingMark := TRUE 
(* REVIEW: What happens if we hit the existing mark here, but keep going
           to another Nl? 
*) 
                END (* IF *) 
              ; GnlState := GnlStateTyp . GnlStateRightNlFound 
              ELSE (* Fall through to process the ModTok in this line. *) 
              END (* IF *) 
            END (* CASE *) 
          ; LChildFsNodeRef 
              := EstUtil . FsRuleForEstNode 
                   ( Lang 
                   , GnlTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                   ) 
          ; GnlTraverseEst 
              ( GnlTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              , GnlTeEstTravInfo . EtiChildLeafElem . LeKindSet  
              , RootAbsNodeNo + GnlTeEstTravInfo . EtiChildRelNodeNo 
              , FsRuleNodeRef := LChildFsNodeRef 
              , EstFmtKind := LangUtil . FmtKindTyp . FmtKindHoriz 
              , EstIndentPos1 := GnlTeIndentPos 
              , EstIndentPosN := GnlTeIndentPos 
              , IsModTok := TRUE  
              ) 
          ; IF GnlState # GnlStateTyp . GnlStateDoneWithLine 
            THEN 
              GnlTeIncEstChild ( ) 
            END (* IF *) 
          END GnlTeTfsModTok 

      ; PROCEDURE GnlTeTfsLeadingMods 
          ( VAR Delete : BOOLEAN 
            (* ^A tok delete mod applies to next tok. *) 
          ; VAR IsRepair : BOOLEAN 
          ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LChildRef : LbeStd . EstRootTyp 
        ; VAR LModDelIsFinished : BOOLEAN 
        ; VAR LTok : LbeStd . TokTyp 

        ; BEGIN (* GnlTeTfsLeadingMods *) 
            Delete := FALSE 
          ; IsRepair := FALSE 
          ; LOOP 
              TravUtil . CheckModFwd 
                ( GnlTeEstTravInfo 
                , FsNodeRef 
                , (* VAR *) LChildRef 
                , (* VAR *) LModDelIsFinished 
                ) 
            ; TYPECASE LChildRef 
              OF NULL 
              => EXIT 

              (* Deletion mod. *) 
              | ModHs . ModDelTyp  
              => Delete := TRUE
              ; IsRepair 
                  := EstHs . EstChildKindContainsInsertionRepair 
                     IN GnlTeEstTravInfo . EtiChildLeafElem . LeKindSet
              ; IF LModDelIsFinished 
                THEN 
                  GnlTeIncEstChild ( ) 
                ELSE (* There can be no more mods for this FmtNo *) 
                  EXIT 
                END (* IF *) 

              (* Blank line. *) 
              | ModHs . ModBlankLineTyp ( TModBlankLine ) 
              => GnlTeTfsModBlankLine ( TModBlankLine ) 

              (* Leading Comment. *) 
              | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
              => GnlTeTfsModCmnt ( TModCmnt ) 

              (* Text Mod. *) 
              | ModHs . ModTextTyp ( TModText ) 
              => GnlTeTfsModText ( TModText ) 

              (* Token Mod. *) 
              | EstHs . EstRefTyp ( TEstRef ) 
                (* It will have EstNodeKindModTok *) 
              => GnlTeTfsModTok ( TEstRef ) 

              (* Lex error characters *) 
              | SharedStrings . T ( TString ) 
(* TODO: Make lex error characters work like ModTok in doing a forced Nl if
         they won't fit on a line.  All Traversers. *) 
              => CASE GnlState <* NOWARN *>  
                 OF GnlStateTyp . GnlStatePassingNl  
                 , GnlStateTyp . GnlStateInLine   
                 => LTok := SharedStrings . Tok ( TString ) 
                 ; GnlTeBlanksBeforeTok ( LTok , FsNodeRef , FsFmtKind ) 
                 ; GnlAccumString ( TString , TextAttrLexErrChars ) 
                 ; GnlState := GnlStateTyp . GnlStateInLine 
                 ; GnlPrevTok := LTok 

                 | GnlStateTyp . GnlStateRightNlFound 
                 => GnlState := GnlStateTyp . GnlStateDoneWithLine 
                 END (* CASE *) 
              ; GnlTeIncEstChild ( ) 

              (* Errors. *) 
              | ModHs . ModLexErrTyp ( TModLexErr ) 
              => IF Options . DoDisplayErrors 
                 THEN 
(* TODO: Fix MergeTxt and TextEdit so they can cope with edited-out
         error messages. *) 
                   GnlBlankFillTo ( GnlCharPos + 1 ) 
                 ; GnlAccumText ( "(*LexErr " , PaintHs . TextAttrErr ) 
                 ; GnlAccumText 
                     ( Fmt . Int ( TModLexErr . ModLexErrCode ) 
                     , PaintHs . TextAttrErr 
                     ) 
                 ; GnlAccumText ( "*)" , PaintHs . TextAttrErr ) 
                 ; GnlPrevTok := LbeStd . Tok__ForceSep 
                 END (* IF *) 
              ; GnlTeIncEstChild ( ) 

              | ModHs . ModSyntErrTyp ( TModSyntErr ) 
              => IF Options . DoDisplayErrors 
                 THEN 
                   GnlBlankFillTo ( GnlCharPos + 1 ) 
                 ; GnlAccumText ( "(*SyntErr " , PaintHs . TextAttrErr ) 
                 ; GnlAccumText 
                     ( Fmt . Int ( TModSyntErr . ModSyntErrCode ) 
                     , PaintHs . TextAttrErr 
                     ) 
                 ; GnlAccumText ( "*)" , PaintHs . TextAttrErr ) 
                 ; GnlPrevTok := LbeStd . Tok__ForceSep 
                 END (* IF *) 
              ; GnlTeIncEstChild ( ) 

              ELSE (* Not a leading mod. *) 
                EXIT 
              END (* TYPECASE *) 
            ; IF GnlState = GnlStateTyp . GnlStateDoneWithLine 
              THEN 
                EXIT 
              END (* IF *) 
            END (* LOOP *) 
          END GnlTeTfsLeadingMods 

      ; PROCEDURE GnlTeTfsTrailingMods ( ) RAISES { AssertionFailure } 

        = BEGIN (* GnlTeTfsTrailingMods *) 
            LOOP 
              IF GnlTeEstTravInfo . EtiChildNo 
                 >= GnlTeEstTravInfo . EtiChildCt 
                 (* No more Est children at all. *) 
                 OR FsNodeRef . FsFmtNo # GnlTeEstTravInfo . EtiChildFmtNo 
                    (* Next child for a different FmtNo *) 
              THEN 
                EXIT 
              ELSE (* Found a mod or child for the current FmtNo. *) 
                TYPECASE GnlTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                OF NULL 
                => (* Not a trailing mod. *) 
                   EXIT 
                | ModHs . ModCmntTrailingTyp ( TModCmnt ) 
                => GnlTeTfsModCmnt ( TModCmnt ) 
                ELSE (* Not a trailing mod. *) 
                  EXIT 
                END (* TYPECASE *) 
              ; IF GnlState = GnlStateTyp . GnlStateDoneWithLine 
                THEN 
                  EXIT 
                END (* IF *) 
              END (* IF *) 
            END (* LOOP *) 
          END GnlTeTfsTrailingMods 

      (* Format syntax trees *) 

      ; PROCEDURE GnlTeTfsInsTok ( ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LTextAttr : PaintHs . TextAttrTyp 
        ; VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* GnlTeTfsInsTok *) 
            GnlTeTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; CASE GnlState 

            OF GnlStateTyp . GnlStateStartAtBeg 
            , GnlStateTyp . GnlStateStartAtEnd 
            => (* The actual start point must be in a trailing mod. *) 
               Assert 
                 ( StartMark . Kind IN Marks . MarkKindSetEstLeaf 
                 , AFT . A_GnlTeTfsInsTok_StartAtInsTok 
                 ) 
            ; GnlTeTfsTrailingMods ( ) 

            | GnlStateTyp . GnlStatePassingNl 
            , GnlStateTyp . GnlStateInLine 
            => IF NOT LDelete OR LIsRepair  
               THEN 
                 GnlTeBlanksBeforeTok 
                   ( FsNodeRef . FsTok , FsNodeRef , FsFmtKind ) 
               ; LTextAttr := TextAttrInsTok
               ; IF TRUE OR LangUtil . IsReserved ( Lang , FsNodeRef . FsTok ) 
(* TODO: ^Decide on this. *) 
                 THEN
                   LTextAttr . TaFont := PaintHs . TaFontBold  
                 END  
               ; IF LDelete 
                 THEN
                   LTextAttr . TaBgColor := PaintHs . TaBgColorPlain 
                 ; LTextAttr . TaDecoration := PaintHs . TaDecCaret 
                 END (* IF *) 
               ; IF IsModTok 
                 THEN 
                   LTextAttr . TaBgColor := PaintHs . TaBgColorPlain 
                 ; LTextAttr . TaDecoration := PaintHs . TaDecStrikeout 
                 END (* IF *) 
               ; GnlAccumString ( FsNodeRef . FsInsTokRef , LTextAttr ) 
               ; GnlPrevTok := FsNodeRef . FsTok 
               ; GnlState := GnlStateTyp . GnlStateInLine 
               ; GnlTeTfsTrailingMods ( ) 
               END (* IF *) 

            | GnlStateTyp . GnlStateRightNlFound 
            => IF NOT LDelete OR LIsRepair  
               THEN 
                 GnlState := GnlStateTyp . GnlStateDoneWithLine 
               END (* IF *) 

            | GnlStateTyp . GnlStateDoneWithLine 
            => 
            END (* CASE *) 
          END GnlTeTfsInsTok 

      ; PROCEDURE GnlTeTfsBuildFmtNoMark ( ) RAISES { AssertionFailure } 

        = VAR LTok : LbeStd . TokTyp 

        ; BEGIN
            IF GnlTeEstTravInfo . EtiIsOptSingletonList 
            THEN LTok := LbeStd . Tok__OptSingletonList
            ELSE 
              LTok := EstUtil . EstTok ( GnlTeEstTravInfo . EtiNodeRef ) 
            END (* IF *) 
          ; IF GnlTeEstTravInfo . EtiChildCt = 0 
            THEN (* No Est children, build ChildFmtNo mark. *) 
              NewMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo := RootAbsNodeNo  
                     , EstNodeCt := 1
                     , Kind := MarkKindTyp . ChildFmtNo 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := FALSE 
                     , IsImpliedNewLine := FALSE 
                     , Tok := LTok 
                     } 
            ELSIF GnlTeEstTravInfo . EtiChildNo 
                  < GnlTeEstTravInfo . EtiChildCt 
            THEN (* There is an Est child to the right.  The FmtNo item is a
                    LeftSib of the that Est child. *) 
              NewMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo 
                         := RootAbsNodeNo 
                            + GnlTeEstTravInfo . EtiChildRelNodeNo  
                     , EstNodeCt 
                         := EstUtil . EstNodeCt 
                              ( GnlTeEstTravInfo . EtiChildLeafElem 
                                . LeChildRef 
                              ) 
                            + ORD ( EstHs . EstChildKindOptSingletonList 
                                    IN GnlTeEstTravInfo . EtiChildLeafElem 
                                       . LeKindSet 
                                  ) 
                     , Kind := MarkKindTyp . LeftSibFmtNo 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := FALSE 
                     , IsImpliedNewLine := FALSE 
                     , Tok := LTok 
                     } 
            ELSE (* There is an Est child to the left but not to the right. 
                    Build a RightSibFmtNo mark. *) 
              NewMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo := RootAbsNodeNo + GnlTeRMChildRelNodeNo 
                     , EstNodeCt 
                         := EstUtil . EstNodeCt ( GnlTeRMChildRef ) 
                            + ORD ( EstHs . EstChildKindOptSingletonList 
                                    IN GnlTeRMChildKindSet 
                                  ) 
                     , Kind := MarkKindTyp . RightSibFmtNo 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := FALSE 
                     , IsImpliedNewLine := FALSE 
                     , Tok := LTok 
                     } 
            END (* IF *) 
          ; IF Marks . Equal ( ExistingMark , NewMark ) 
            THEN DidHitExistingMark := TRUE 
            END (* IF *) 
          END GnlTeTfsBuildFmtNoMark 

      ; PROCEDURE GnlTeTfsLineBreakInterior ( FsKind : FsKindTyp ) 
        RAISES { AssertionFailure } 

        = BEGIN (* GnlTeTfsLineBreakInterior *) 
            IF FsKind = FsKindTyp . FsKindLineBreakReqd 
               OR TravUtil . DoTakeLineBreak 
                    ( Lang 
                    , GnlCharPos 
                    , GnlModTextIsToLeftOnLine 
                    , GnlPrevTok 
                    , FsRuleNodeRef 
                    , FsNodeRef 
                    , FsFmtKind 
                    , GnlTeIndentPos 
                    , GnlTeEstTravInfo 
                    , (* IN OUT *) GnlLastFmtNoOnLine 
                    , (* IN OUT *) GnlEstListChildrenToPass 
                    ) 
            THEN (* The line break is taken. *) 
              GnlTeTfsBuildFmtNoMark ( ) 
            ; GnlCharPos := 0 
            ; GnlModTextIsToLeftOnLine := FALSE 
            ; GnlPrevTok := LbeStd . Tok__BegOfLine 
            ; GnlLastFmtNoOnLine := EstHs . FmtNoUnknown  
            ; GnlEstListChildrenToPass := 0 (* Dead *) 
            ; GnlState := GnlStateTyp . GnlStateRightNlFound 
              (* Keep going.  There could be another ajacent Nl to the right. *)
            END (* IF *) 
          END GnlTeTfsLineBreakInterior 

      ; PROCEDURE GnlTeTfsLineBreak ( FsKind : FsKindTyp )
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* GnlTeTfsLineBreak *) 
            GnlTeTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; CASE GnlState 
            OF GnlStateTyp . GnlStateStartAtBeg 
            => IF StartMark . Kind IN Marks . MarkKindSetEstLeaf 
               THEN (* The actual starting point must have been in a leading 
                       mod on this line break. *) 
               ELSE (* This is the starting point. *) 
                 GnlCharPos := 0 
               ; GnlTeIsFirstLine := FALSE 
               ; GnlTeIndentPos := EstIndentPosN
               ; GnlState := GnlStateTyp . GnlStatePassingNl
               END (* IF *) 

            | GnlStateTyp . GnlStateStartAtEnd 
            => (* The actual starting point must be in a trailing mod. 
                  We aren't actually visiting this LineBreak, so leave 
                  state alone. *) 

            | GnlStateTyp . GnlStateDoneWithLine 
            , GnlStateTyp . GnlStatePassingNl 
            => (* Just keep going. *) 

            | GnlStateTyp . GnlStateInLine 
            => IF NOT LDelete 
               THEN 
                 IF GnlPrevTok = LbeStd . Tok__BegOfLine 
(* TODO: ^Replace this with a GnlStatePassingNl, similar to other traversers, 
          for consistency. 
*) 
                 THEN (* Just stay in GnlStateInLine, ignoring the LineBreak. *)
                 ELSE 
                   GnlTeTfsLineBreakInterior ( FsKind ) 
                 END (* IF *) 
               END (* IF *) 
            ; GnlTeIsFirstLine := FALSE 
            ; GnlTeIndentPos := EstIndentPosN 

            | GnlStateTyp . GnlStateRightNlFound 
            => IF NOT LDelete 
               THEN GnlTeTfsLineBreakInterior ( FsKind ) 
               END (* IF *) 

            END (* CASE *) 
          END GnlTeTfsLineBreak 

      ; PROCEDURE GnlTeTfsEstChild ( IsEstList : BOOLEAN ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* GnlTeTfsEstChild *) 
            GnlTeTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair) 

          ; CASE GnlState 
            OF GnlStateTyp . GnlStateStartAtBeg 
            , GnlStateTyp . GnlStateStartAtEnd 
            => IF RootAbsNodeNo 
                  + EstUtil . EstNodeCt ( GnlTeEstTravInfo . EtiNodeRef ) 
                  + ORD ( GnlTeEstTravInfo . EtiIsOptSingletonList )  
                  <= StartMark . EstNodeNo 
               THEN (* Marked node is outside this Est subtree. *) 
(* CHECK: How can this happen? Check other traversers too.  
          Shouldn't we be checking for beyond the end of the current
          child, instead of the entire parent node?
 *) 
                 GnlTeIncEstChild ( ) 
               ; IF IsEstList 
                 THEN
                   TravUtil . PassEstListChild ( GnlEstListChildrenToPass ) 
                 END (* IF *) 
               ELSE 
                 Assert 
                   ( GnlTeEstTravInfo . EtiChildNo 
                     < GnlTeEstTravInfo . EtiChildCt 
                     AND GnlTeEstTravInfo . EtiChildFmtNo 
                         = FsNodeRef . FsFmtNo 
                  , AFT . A_GnlTeTfsEstChild_StartInsideMissingEstChild 
                  ) 
               ; IF EstHs . EstChildKindEstChild 
                    IN GnlTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                 THEN 
                   IF ISTYPE 
                        ( GnlTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                        , ModHs . EstDummyTyp (* Including NIL *) 
                        ) 
                   THEN 
                     GnlTeIncEstChild ( ) 
                   ELSIF EstHs . EstChildKindTrailingSep 
                         IN GnlTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                   THEN (* Skip trailing sep. *) 
                     GnlTeIncEstChild ( ) 
                   ELSE 
                     GnlTeTfsEstSubtree ( IsModTok := IsModTok ) 
                   END (* IF *) 
                 ; IF IsEstList 
                   THEN
                     TravUtil . PassEstListChild ( GnlEstListChildrenToPass ) 
                   END (* IF *) 
                 END (* IF *) 
               END (* IF *) 

            | GnlStateTyp . GnlStatePassingNl 
            , GnlStateTyp . GnlStateInLine 
            , GnlStateTyp . GnlStateRightNlFound 
            => TravUtil . AssertFwdNoLostFixedChild 
                 ( FsNodeRef , GnlTeEstTravInfo ) 
            ; IF GnlTeEstTravInfo . EtiChildNo 
                 < GnlTeEstTravInfo . EtiChildCt 
                 AND GnlTeEstTravInfo . EtiChildFmtNo = FsNodeRef . FsFmtNo 
                 AND EstHs . EstChildKindEstChild 
                     IN GnlTeEstTravInfo . EtiChildLeafElem . LeKindSet 
              THEN 
                IF ISTYPE 
                     ( GnlTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     , ModHs . EstDummyTyp 
                     ) (* Including NIL *)  
                THEN 
                  GnlTeIncEstChild ( ) 
                ELSIF EstHs . EstChildKindTrailingSep 
                      IN GnlTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                THEN (* Skip trailing sep. *) 
                  GnlTeIncEstChild ( ) 
                ELSE 
                  Assert 
                    ( NOT LDelete 
                    , AFT . A_GnlTeTfsEstChild_DeletedEstChild 
                    ) 
                ; GnlTeTfsEstSubtree ( IsModTok := IsModTok ) 
                END (* IF *) 
              ; IF IsEstList 
                THEN
                  TravUtil . PassEstListChild ( GnlEstListChildrenToPass ) 
                END (* IF *) 
              END (* IF *) 

            | GnlStateTyp . GnlStateDoneWithLine 
            => 

            END (* CASE *) 
          ; IF GnlState # GnlStateTyp . GnlStateDoneWithLine 
            THEN 
              GnlTeTfsTrailingMods ( ) 
            END (* IF *) 
          END GnlTeTfsEstChild 

      ; PROCEDURE GnlTeTfsFsSubtree ( ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LChildFmtKind : LangUtil . FmtKindTyp 
        ; VAR LInitialChildNo : LangUtil . FsChildNoTyp 

        ; BEGIN (* GnlTeTfsFsSubtree *) 
            CASE GnlState <* NOWARN *> 
            OF GnlStateTyp . GnlStateStartAtBeg 
            , GnlStateTyp . GnlStateStartAtEnd 
            => LChildFmtKind 
                := TravUtil . FmtKindForFsSubtreeDescending
                     ( Lang 
                     , FsRuleNodeRef 
                     , FsNodeRef 
                     , FsFmtKind 
                     , EstIndentPosN 
                     , GnlTeEstTravInfo 
                     , StartMark 
                     , StartMarkIsKnownNl := TRUE 
                     )
            ; LInitialChildNo 
                := LangUtil . FsChildNoOfFmtNo ( FsNodeRef , GnlTeStartFmtNo ) 
            | GnlStateTyp . GnlStatePassingNl 
            , GnlStateTyp . GnlStateInLine 
            , GnlStateTyp . GnlStateRightNlFound 
            => LChildFmtKind 
                := TravUtil . FmtKindForFsSubtreeTraversing 
                     ( Lang 
                     , GnlCharPos 
                     , GnlModTextIsToLeftOnLine 
                     , GnlPrevTok 
                     , FsRuleNodeRef 
                     , FsNodeRef 
                     , FsFmtKind 
                     , GnlTeIndentPos 
                     , GnlTeEstTravInfo 
                     , (* IN OUT *) GnlLastFmtNoOnLine 
                     , (* IN OUT *) GnlEstListChildrenToPass 
                     )
            ; LInitialChildNo := 0 
            END (* CASE *) 
          ; GnlTeTraverseFsFixedChildren 
              ( FsNodeRef , LChildFmtKind , LInitialChildNo ) 
          END GnlTeTfsFsSubtree

      ; PROCEDURE GnlTeTfsTraverseCondFmtChildren ( ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LFsChildNo : LangUtil . FsChildNoTyp 
        ; VAR LPredicate : BOOLEAN 

        ; BEGIN (* GnlTeTfsTraverseCondFmtChildren *) 
            CASE GnlState <* NOWARN *> 
            OF GnlStateTyp . GnlStateStartAtBeg 
            , GnlStateTyp . GnlStateStartAtEnd 
            => TravUtil . DescendCondFmt 
                 ( Lang 
                 , FsNodeRef := FsNodeRef 
                 , FmtNo := GnlTeStartFmtNo 
                 , EstTravInfo := GnlTeEstTravInfo 
                 , (* VAR *) Predicate := LPredicate 
                 , (* VAR *) FsChildNo := LFsChildNo 
                 ) 

            | GnlStateTyp . GnlStatePassingNl 
            , GnlStateTyp . GnlStateInLine 
            , GnlStateTyp . GnlStateRightNlFound 
            => LFsChildNo := 0 
            ; LPredicate 
               := TravUtil . DoCondFmtFwd 
                    ( Lang , GnlTeEstTravInfo , FsNodeRef ) 
            END (* CASE *) 
          ; IF LPredicate 
            THEN 
              GnlTeTraverseFsFixedChildren 
                ( FsNodeRef , FsFmtKind , LFsChildNo ) 
            ELSE 
              GnlTeTraverseFs ( FsNodeRef . FsCondAltRef , FsFmtKind ) 
            END (* IF *) 
          END GnlTeTfsTraverseCondFmtChildren 

      ; BEGIN (* GnlTeTraverseFs *) 
          VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* Block  GnlTeTraverseFs *) 
            CASE FsNodeRef . FsKind <* NOWARN *> 

            (* Beginning of Image *) 
            OF FsKindTyp . FsKindBegOfImage 
            => IF GnlState IN GnlStateSetStart 
                  AND StartMark . Kind IN Marks . MarkKindSetEstLeaf 
               THEN (* The start point must be in a trailing mod. *) 
               ELSE 
                 Assert 
                   ( GnlState = GnlStateTyp . GnlStateStartAtBeg 
                   , AFT . A_GnlTeTraverseFs_BOIBadState 
                   ) 
               ; GnlCharPos := 0 
               ; GnlTeIsFirstLine := TRUE 
               ; GnlTeIndentPos := EstIndentPos1
               ; GnlState := GnlStateTyp . GnlStatePassingNl 
(* CHECK: ^Do trailing mods on BOI make any sense? Not all traversers 
           handle them. *) 
               END (* IF *) 
            ; GnlTeTfsTrailingMods ( ) 

            (* EndOfImage *) 
            | FsKindTyp . FsKindEndOfImage 
            => GnlTeTfsLeadingMods ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
            ; Assert ( NOT LDelete , AFT . A_GnlTeTraverseFs_DeletedEOI ) 
            ; IF GnlState # GnlStateTyp . GnlStateDoneWithLine 
              THEN 
                Assert 
                  ( GnlState # GnlStateTyp . GnlStateStartAtEnd 
                  , AFT . A_GnlTeTraverseFs_StartAtEndOfEOI 
                  ) 
              ; GnlTeTfsBuildFmtNoMark ( ) 
              ; AtEndOfImage := GnlState IN GnlStateSetAtStartingNl 
              ; GnlState := GnlStateTyp . GnlStateDoneWithLine 
              END (* IF *) 

            (* InsTok. *) 
            | FsKindTyp . FsKindInsTok 
            => GnlTeTfsInsTok ( ) 

            (* Line breaks. *) 
            | FsKindTyp . FsKindLineBreakOpt 
            , FsKindTyp . FsKindLineBreakReqd  
            => GnlTeTfsLineBreak ( FsNodeRef . FsKind ) 

            (* Est child cases. *) 
            | FsKindTyp . FsKindEstChildOfFixed 
            => GnlTeTfsEstChild ( IsEstList := FALSE ) 

            | FsKindTyp . FsKindEstChildOfList 
            => GnlTeTfsEstChild ( IsEstList := TRUE ) 

            (* Subtree nodes. *) 
            | FsKindTyp . FsKindSubtreeVert 
            , FsKindTyp . FsKindSubtreeHoriz 
            , FsKindTyp . FsKindSubtreeFill 
            => GnlTeTfsFsSubtree ( ) 

            (* Conditional format. *) 
            | FsKindTyp . FsKindCondFmt 
            => GnlTeTfsTraverseCondFmtChildren ( ) 

            END (* CASE FsKind *) 
          END (* Block  GnlTeTraverseFs *) 
        END GnlTeTraverseFs 

    ; BEGIN (* GnlTraverseEst *) 
        VAR LInitialFsChildNo : LangUtil . FsChildNoTyp 

      ; BEGIN (* Block for GnlTraverseEst *) 
          IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        ; IF EstRef # NIL 
          THEN 
            GnlTeRMChildRelNodeNo := LbeStd . EstNodeNoNull 
          ; GnlTeRMChildRef := NIL 
          ; GnlTeRMChildKindSet := EstHs . EstChildKindSetEmpty  
          ; CASE GnlState 

            OF GnlStateTyp . GnlStateStartAtBeg 
            , GnlStateTyp . GnlStateStartAtEnd 
            => (* Choose the Est child to descend to. *) 
               IF RootAbsNodeNo = StartMark . EstNodeNo 
               THEN (* Mark denotes the Est root. *)  
                 TravUtil . InitEstTravInfoFwd 
                   ( GnlTeEstTravInfo , EstRef , KindSet , RootAbsNodeNo ) 
               ; CASE StartMark . Kind <* NOWARN *> 
                 OF MarkKindTyp . Plain 
                 , MarkKindTyp . BlankLine 
                 => (* The mark denotes the Est root, which is either a leaf
                       or a ModTok For an insertion token. 
                       No need to set to a child. *) 

                 | MarkKindTyp . LeftSibFmtNo 
                 , MarkKindTyp . RightSibFmtNo 
                 => (* NodeRef for these is handled when it is LeChildRef. *)
                   CantHappen ( AFT . A_GnlTraverseEst_SibFmtNo_on_parent ) 
                 END (* CASE  *) 
               ELSE (* Mark does not lead to this (parent) EstNode. *) 
                 TravUtil . InitToChildContainingNodeNo 
                   ( GnlTeEstTravInfo 
                   , EstRef 
                   , StartMark . EstNodeNo - RootAbsNodeNo 
                   , KindSet 
                   , RootAbsNodeNo 
                   ) 
               ; IF StartMark . Kind = MarkKindTyp . RightSibFmtNo 
                    AND RootAbsNodeNo + GnlTeEstTravInfo . EtiChildRelNodeNo 
                        = StartMark . EstNodeNo 
                 THEN 
                   Assert 
                     ( GnlTeEstTravInfo . EtiChildNo + 1 
                       = GnlTeEstTravInfo . EtiChildCt 
                     , AFT . A_GnlTraverseEst_RightSib_not_rightmost
                     ) 
                 ; GnlTeIncEstChild ( )
                   (* Go off right end.  This will set GnlTeRMChildRelNodeNo
                      to the RM child. *)
                 END (* IF *) 
               END (* IF *) 

            (* Choose the FmtNo to descend to. *) 
            ; GnlTeStartFmtNo := GnlTeEstTravInfo . EtiChildFmtNo (* default *)
            ; CASE StartMark . Kind 
              OF MarkKindTyp . ChildFmtNo 
              => IF RootAbsNodeNo = StartMark . EstNodeNo 
                 THEN GnlTeStartFmtNo := StartMark . FmtNo 
                 END (* IF *)
                 
              | MarkKindTyp . LeftSibFmtNo 
              => IF RootAbsNodeNo + GnlTeEstTravInfo . EtiChildRelNodeNo 
                    = StartMark . EstNodeNo 
                 THEN GnlTeStartFmtNo := StartMark . FmtNo 
                 END (* IF *) 

              | MarkKindTyp . RightSibFmtNo 
              => IF GnlTeEstTravInfo . EtiChildNo 
                    >= GnlTeEstTravInfo . EtiChildCt 
                    AND RootAbsNodeNo + GnlTeRMChildRelNodeNo 
                        = StartMark . EstNodeNo 
                 THEN GnlTeStartFmtNo := StartMark . FmtNo 
                 END (* IF *) 

              ELSE 
              END (* CASE StartMark . Kind. *) 
            ; LInitialFsChildNo 
                := LangUtil . FsChildNoOfFmtNo 
                     ( FsRuleNodeRef , GnlTeStartFmtNo ) 
            ; GnlTeInitialFsLeafRef
                := LangUtil . FsLeafRefOfFmtNo 
                     ( FsRuleNodeRef, GnlTeStartFmtNo ) 
            ; GnlTeSetIndentInfo ( ) 
            ; GnlTeDescendedInto := TRUE 

            | GnlStateTyp . GnlStatePassingNl 
            , GnlStateTyp . GnlStateInLine 
            , GnlStateTyp . GnlStateRightNlFound 
            => TravUtil . InitEstTravInfoFwd 
                 ( GnlTeEstTravInfo , EstRef , KindSet , RootAbsNodeNo ) 
            ; GnlTeStartFmtNo := FsRuleNodeRef . FsLeftFmtNo 
            ; LInitialFsChildNo := 0 
            ; GnlTeInitialFsLeafRef
                := LangUtil . FsLeafRefOfFmtNo 
                     ( FsRuleNodeRef, GnlTeStartFmtNo ) 
            ; GnlTeIsFirstLine := TRUE 
            ; GnlTeIndentPos := EstIndentPos1
            ; GnlTeDescendedInto := FALSE  

            ELSE 
              CantHappen ( AFT . A_GnlTraverseEst_Bad_state ) 
            END (* CASE FsRuleNodeRef . FsKind. *) 
(* CHECK: No other traverser has this assertion.  Do we really need it? *) 
          ; Assert 
              ( FsRuleNodeRef . FsChildren = NIL 
                OR NUMBER ( FsRuleNodeRef . FsChildren ^ ) = 0 
                OR ( 0 <= LInitialFsChildNo 
                     AND LInitialFsChildNo 
                         < NUMBER ( FsRuleNodeRef . FsChildren ^ )  
                   ) 
              , AFT . A_GnlTraverseEst_Bad_FsChildNo 
              ) 

          (* Now traverse the Fs tree. *) 
          ; CASE FsRuleNodeRef . FsKind <* NOWARN *> 

            (* Est fixed nodes. *) 
            OF FsKindTyp . FsKindEstFixedVert 
            , FsKindTyp . FsKindEstFixedHoriz 
            , FsKindTyp . FsKindEstFixedFill 
            => GnlTeTraverseFsFixedChildren 
                 ( FsRuleNodeRef , EstFmtKind , LInitialFsChildNo ) 

            (* Est list nodes. *) 
            | FsKindTyp . FsKindEstListVert 
            , FsKindTyp . FsKindEstListHoriz 
            , FsKindTyp . FsKindEstListFill 
            , FsKindTyp . FsKindEstListTrailVert 
            , FsKindTyp . FsKindEstListTrailHoriz 
            , FsKindTyp . FsKindEstListTrailFill 
            => GnlTeTraverseFsListChildren 
                 ( FsRuleNodeRef , EstFmtKind , LInitialFsChildNo ) 

            (* Ast string, *) 
            | FsKindTyp . FsKindAstString 
            => GnlTeAstString ( FsRuleNodeRef , EstFmtKind ) 

         (* ELSE Cant happen. *) 
            END (* CASE FsRuleNodeRef . FsKind. *) 
          ; IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
          END (* IF *) 
        END (* Block *) 
      END GnlTraverseEst 

  ; BEGIN (* GetNextLine *) 
      GnlCharPos := LbeStd . LimitedCharNoUnknown 
    ; GnlModTextIsToLeftOnLine := FALSE 
    ; GnlPrevTok := LbeStd . Tok__BegOfLine 
    ; GnlLastFmtNoOnLine := EstHs . FmtNoUnknown  
    ; GnlEstListChildrenToPass := 0 (* Dead *) 
    ; GnlNextTextAttr := 0 
    ; GnlNextLineErr := 0 
    ; DidHitExistingMark := FALSE 
    ; AtEndOfImage := FALSE 
    ; BlankLineCt := 0 
    ; LineText := Strings . Empty ( EventualLengthHint := DefaultLineLen ) 
    ; IF StartMark . StartAtEnd 
      THEN 
        GnlState := GnlStateTyp . GnlStateStartAtEnd 
      ELSE 
        GnlState := GnlStateTyp . GnlStateStartAtBeg 
      END (* IF *)
    ; GnlTraverseEst 
        ( EstRef 
        , EstHs . EstChildKindSetEmpty  
        , RootAbsNodeNo := 0 
        , FsRuleNodeRef := EstUtil . FsRuleForEstNode ( Lang , EstRef ) 
        , EstFmtKind := LangUtil . FmtKindTyp . FmtKindVert 
        , EstIndentPos1 := Options . InitialIndentPos 
        , EstIndentPosN := Options . InitialIndentPos 
        , IsModTok := FALSE 
(* TODO: We probably want some way to be able to have an insertion repair 
         placeholder be a whole Est. 
*) 
        ) 
    ; IF GnlNextTextAttr > 0 
      THEN 
        TextAttrArrayRef 
          := NEW ( PaintHs . TextAttrArrayRefTyp , GnlNextTextAttr ) 
      ; TextAttrArrayRef ^ 
          := SUBARRAY ( GnlTextAttrs , 0 , GnlNextTextAttr ) 
      ELSE 
        TextAttrArrayRef := NIL  
      END (* IF *) 
    ; IF GnlNextLineErr > 0 
      THEN 
        LineErrArrayRef 
          := NEW ( PaintHs . LineErrArrayRefTyp , GnlNextLineErr ) 
      ; LineErrArrayRef ^ 
          := SUBARRAY ( GnlLineErrs , 0 , GnlNextLineErr ) 
      ELSE 
        LineErrArrayRef := NIL  
      END (* IF *) 
    ; Assert
        ( NewMark . Kind # MarkKindTyp . Null 
        , AFT . A_GetNextLine_NullNewMarkKind 
        ) 
    END GetNextLine 

(****************************************************************************) 

(* VISIBLE *) 
; PROCEDURE GetPrevLine 
    ( Lang : LbeStd . LangTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; READONLY StartMark : Marks . TokMarkTyp 
    ; READONLY ExistingMark : Marks . TokMarkTyp 
    ; VAR NewMark : Marks . TokMarkTyp 
    ; VAR AtBegOfImage : BOOLEAN 
      (* ^TRUE if StartMark was at BOI, though not necessarily the 
         leftmost Nl of an adjacent group. *) 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

  = TYPE GplStateTyp 
      = { GplStateStartAtBeg 
          (* ^Start a line at the first new line of the token. *) 
        , GplStateStartAtEnd 
          (* ^Start a line at the last new line of a token 
             that has a possible new line at the end. 
             (comment, text mod, implied Nl) *) 
        , GplStatePassingNl 
          (* Passing a group of Nl's at the right end of the line. *) 
        , GplStateInLine 
          (* ^Inside a line. *) 
        , GplStateDoneWithLine 
        } 
  ; TYPE GplStateSetTyp = SET OF GplStateTyp 

  ; CONST GplStateSetStart 
      = GplStateSetTyp 
          { GplStateTyp . GplStateStartAtBeg 
          , GplStateTyp . GplStateStartAtEnd 
          } 

  ; CONST GplStateSetBegOfImage 
      = GplStateSetTyp 
          { GplStateTyp . GplStateInLine , GplStateTyp . GplStatePassingNl 
          , GplStateTyp . GplStateStartAtBeg 
          } 

  ; CONST GplStateSetAtStartingNl 
      = GplStateSetTyp 
          { GplStateTyp . GplStatePassingNl 
          , GplStateTyp . GplStateStartAtBeg 
          } 

  ; VAR GplState : GplStateTyp 

  ; PROCEDURE GplTraverseEst 
      ( EstRef : LbeStd . EstRootTyp 
      ; KindSet : EstHs . EstChildKindSetTyp 
      ; RootAbsNodeNo : LbeStd . EstNodeNoTyp 
      ; FsRuleNodeRef : LangUtil . FsNodeRefTyp 
      ; EstApproxFmtKind : LangUtil . FmtKindTyp 
        (* ^EstApproxFmtKind could be unknown if the subtree has finite 
            width (which => no interior explicit Nl's), no explicit 
            NlBefore.  We only stop and build a TokMark at an explicit 
            NlBefore (which means the enclosing Est has either 
            infinite width or WiHasNlBefore) or at a line 
            break when EstApproxFmtKind = LbeStd . FmtKindVert. 
            Thus, when we stop, EstApproxFmtKind # LbeStd . FmtKindUnknown. 
            Furthermore, once EstApproxFmtKind = LbeStd . FmtKindUnknown, 
            it cannot become known in a subtree.  So we have known FmtKind 
            for all ancestors. *) 
      ; EstIndentPos1 : LbeStd . LimitedCharNoTyp 
      ; EstIndentPosN : LbeStd . LimitedCharNoTyp 
      ) 
    RAISES { AssertionFailure , Thread . Alerted } 

    = VAR GplTeStartFmtNo : EstHs . FmtNoTyp 
    ; VAR GplTeEstTravInfo : TravUtil . EstTravInfoTyp 
    ; VAR GplTeRightwardChildRelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR GplTeRightwardChildRef : LbeStd . EstRootTyp  
    ; VAR GplTeRightwardChildKindSet : EstHs . EstChildKindSetTyp 
      (* GplTeRightwardChildRelNodeNo, GplTeRightwardChildRef, and 
         GplTeRightwardChildKindSet trail temporally behind the current child 
         of GplTeEstTravInfo, whenever it moves to the left.  Since we are 
         traversing right-to-left, they are guaranteed to be set properly only 
         when the current child is left of the rightmost child. 
      *) 

    ; PROCEDURE GplTeDecEstChild ( ) 
      RAISES { AssertionFailure } 

      = BEGIN 
          GplTeRightwardChildRelNodeNo 
            := GplTeEstTravInfo . EtiChildRelNodeNo 
        ; GplTeRightwardChildRef 
            := GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
        ; GplTeRightwardChildKindSet 
            := GplTeEstTravInfo . EtiChildLeafElem . LeKindSet  
        ; TravUtil . DecEstChild ( GplTeEstTravInfo ) 
        END GplTeDecEstChild 

    ; PROCEDURE GplTeTraverseFs 
        ( FsNodeRef : LangUtil . FsNodeRefTyp 
        ; FsApproxFmtKind : LangUtil . FmtKindTyp 
        ) 
      RAISES { AssertionFailure , Thread . Alerted }
      (* DOES handle Fs root nodes. *)

      = PROCEDURE GplTeTfsEstSubtree ( ) 
        RAISES { AssertionFailure , Thread . Alerted } 
        (* Does NOT handle ModTok. *) 

        = VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 
        ; VAR LChildFmtKind : LangUtil . FmtKindTyp 
        ; VAR LChildIndentPos1 : LbeStd . LimitedCharNoTyp 
        ; VAR LChildIndentPosN : LbeStd . LimitedCharNoTyp 

        ; BEGIN (* GplTeTfsEstSubtree *) 
            LChildFsNodeRef 
              := LangUtil . FsRuleForEstChild  
                   ( Lang , FsNodeRef , GplTeEstTravInfo . EtiChildLeafElem ) 
          ; TravUtil . ChildIndentPositions 
              ( Lang 
              , FsNodeRef 
              , EstIndentPos1 
              , EstIndentPosN
              , (* VAR *) ChildIndentPos1 := LChildIndentPos1 
              , (* VAR *) ChildIndentPosN := LChildIndentPosN 
              , IsFirstLine 
                  := GplTeEstTravInfo . EtiChildNo 
                     < GplTeEstTravInfo . EtiParentRef 
                       . KTreeEstChildCtLeftOfNl 
              ) 
          ; CASE GplState 
            OF GplStateTyp . GplStateStartAtEnd 
            , GplStateTyp . GplStateStartAtBeg 
            => LChildFmtKind 
                := TravUtil . FmtKindForEstDescending 
                     ( FsKind := LChildFsNodeRef . FsKind 
                     , ParentFmtKind := FsApproxFmtKind 
                     , FirstLineIndentPos := LChildIndentPos1 
                     , EstRef 
                         := GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     , StartMark := StartMark 
                     , StartMarkIsKnownNl := TRUE 
                     ) 
            ELSE 
              LChildFmtKind 
                := TravUtil . FmtKindForEstTraversing 
                     ( Lang := Lang 
                     , CharPos := LbeStd . LimitedCharNoUnknown  
                     , ModTextIsToLeftOnLine := FALSE 
                       (* ^Irrelevant, since CharPos is unknown. *) 
                     , PrevTok := LbeStd . Tok__BegOfLine 
                       (* ^Irrelevant, since CharPos is unknown. *) 
                     , FsKind := LChildFsNodeRef . FsKind 
                     , ParentFmtKind := FsApproxFmtKind 
                     , FirstLineIndentPos := LChildIndentPos1 
                     , EstRef 
                         := GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     ) 
            END (* CASE *) 
          ; GplTraverseEst           
              ( GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              , GplTeEstTravInfo . EtiChildLeafElem . LeKindSet  
              , RootAbsNodeNo + GplTeEstTravInfo . EtiChildRelNodeNo 
              , LChildFsNodeRef 
              , LChildFmtKind 
              , LChildIndentPos1 
              , LChildIndentPosN 
              ) 
          ; IF GplState # GplStateTyp . GplStateDoneWithLine 
            THEN 
              GplTeDecEstChild ( ) 
            END (* IF *) 
          END GplTeTfsEstSubtree
 
      (* Leading mods. *)
 
      ; PROCEDURE GplTeTfsModBlankLine ( ) RAISES { AssertionFailure } 

        = BEGIN (* GplTeTfsModBlankLine *) 
            CASE GplState 

            OF GplStateTyp . GplStateStartAtBeg 
            => GplTeDecEstChild ( ) 
            ; GplState := GplStateTyp . GplStatePassingNl 

            | GplStateTyp . GplStateStartAtEnd 
            , GplStateTyp . GplStatePassingNl 
            => NewMark 
                 := Marks . TokMarkTyp 
                      { EstNodeNo 
                          := RootAbsNodeNo 
                             + GplTeEstTravInfo . EtiChildRelNodeNo 
                      , EstNodeCt := 1 
                      , Kind := MarkKindTyp . BlankLine 
                      , FmtNo := FsNodeRef . FsFmtNo 
                      , StartAtEnd := FALSE 
                      , IsImpliedNewLine := FALSE 
                      , Tok := LbeStd . Tok__BlankLine 
                      } 
            ; GplState := GplStateTyp . GplStateDoneWithLine 

            | GplStateTyp . GplStateInLine 
            => (* Blank line has both NlBefore and infinite 
                  width, so FmtKindForEstTraversing can always do without 
                  a CharPos for the blank line and all ancestors. *) 
(* FIXME: I don't think so.  Bl is viewed as zero-width by the containing text. *)
               NewMark 
                 := Marks . TokMarkTyp 
                      { EstNodeNo 
                          := RootAbsNodeNo 
                             + GplTeEstTravInfo . EtiChildRelNodeNo 
                      , EstNodeCt := 1 
                      , Kind := MarkKindTyp . BlankLine 
                      , FmtNo := FsNodeRef . FsFmtNo 
                      , StartAtEnd := TRUE 
                      , IsImpliedNewLine := FALSE 
                      , Tok := LbeStd . Tok__BlankLine 
                      } 
            ; GplState := GplStateTyp . GplStateDoneWithLine 

            ELSE 
              CantHappen 
                ( AFT . A_GplTeTfsLeadingModsNoDel_BlankLineBadState ) 
            END (* CASE GplState *) 
          END GplTeTfsModBlankLine 

      ; PROCEDURE GplTeTfsStringModAtEndingMark 
          ( StartAtEnd : BOOLEAN ) : BOOLEAN 

        = BEGIN (* GplTeTfsStringModAtEndingMark *) 
            RETURN 
              ExistingMark . Kind = MarkKindTyp . Plain 
              AND RootAbsNodeNo + GplTeEstTravInfo . EtiChildRelNodeNo 
                  = ExistingMark . EstNodeNo 
              AND StartAtEnd = ExistingMark . StartAtEnd 
          END GplTeTfsStringModAtEndingMark 

      ; PROCEDURE GplTeTfsStringMod 
          ( Tok: LbeStd . TokTyp ; NlBefore : BOOLEAN ; NlAfter : BOOLEAN ) 
        RAISES { AssertionFailure } 

        = BEGIN (* GplTeTfsStringMod *) 
            CASE GplState <* NOWARN *> 

            OF GplStateTyp . GplStateStartAtBeg 
            => (* Traverse leftward, passing the Nl. *) 
              GplTeDecEstChild ( ) 
            ; GplState := GplStateTyp . GplStatePassingNl 

            | GplStateTyp . GplStateStartAtEnd 
            , GplStateTyp . GplStatePassingNl 
            => (* NlAfter, if any, is the right end of the line. *) 
               IF NlBefore 
                  OR GplTeTfsStringModAtEndingMark ( StartAtEnd := FALSE ) 
                    (* ^There could be an implicit Nl, with EndingMark 
                         identifying it. *) 
               THEN (* Stop at the beginning of the mod. *) 
                 NewMark 
                   := Marks . TokMarkTyp 
                        { EstNodeNo 
                            := RootAbsNodeNo 
                               + GplTeEstTravInfo . EtiChildRelNodeNo 
                        , EstNodeCt := 1 
                        , Kind := MarkKindTyp . Plain 
                        , FmtNo := FsNodeRef . FsFmtNo 
                        , StartAtEnd := FALSE 
                        , IsImpliedNewLine := NOT NlBefore  
                        , Tok := Tok 
                        } 
               ; GplState := GplStateTyp . GplStateDoneWithLine 
               ELSE (* Traverse backwards through the mod. *) 
                 GplTeDecEstChild ( ) 
               ; GplState := GplStateTyp . GplStateInLine 
               END (* IF *) 

            | GplStateTyp . GplStateInLine 
            => IF NlAfter 
                  AND ( NlBefore 
                        OR GplTeTfsStringModAtEndingMark ( StartAtEnd := TRUE )
                           (* ^Don't stop at a mod with NlAfter, but no
                               NlBefore, and non-infinite width (which we 
                               assume, for comment and text mods) because 
                               FmtKindForEstTraversing of some ancestor could 
                               have needed CharPos, which we did't know going 
                               backwards, and so FmtKind would be unknown.*) 
(* CHECK: ^Wasn't this requirement obviated when we went to node numbers 
           for marks? *) 
                      ) 
               THEN (* Stop at the end of the mod. *) 
                 NewMark 
                   := Marks . TokMarkTyp 
                        { EstNodeNo 
                            := RootAbsNodeNo 
                               + GplTeEstTravInfo . EtiChildRelNodeNo 
                        , EstNodeCt := 1 
                        , Kind := MarkKindTyp . Plain 
                        , FmtNo := FsNodeRef . FsFmtNo 
                        , StartAtEnd := TRUE 
                        , IsImpliedNewLine := FALSE 
                        , Tok := Tok 
                        } 
               ; GplState := GplStateTyp . GplStateDoneWithLine 
               ELSIF NlBefore 
                     OR GplTeTfsStringModAtEndingMark ( StartAtEnd := FALSE ) 
                       (* ^There could be an implicit Nl, with EndingMark 
                          identifying it. *) 
               THEN (* Stop at the beginning of the mod. *) 
                 NewMark 
                   := Marks . TokMarkTyp 
                        { EstNodeNo 
                            := RootAbsNodeNo 
                               + GplTeEstTravInfo . EtiChildRelNodeNo 
                        , EstNodeCt := 1 
                        , Kind := MarkKindTyp . Plain 
                        , FmtNo := FsNodeRef . FsFmtNo 
                        , StartAtEnd := FALSE 
                        , IsImpliedNewLine := NOT NlBefore 
                        , Tok := Tok 
                        } 
               ; GplState := GplStateTyp . GplStateDoneWithLine 
               ELSE (* Traverse backwards through the mod. *) 
                 GplTeDecEstChild ( ) 
               END (* IF *) 
            END (* CASE *) 
          END GplTeTfsStringMod 

      ; PROCEDURE GplTeTfsLeadingModsNoDel ( ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LChildIndentPos : LbeStd . LimitedCharNoTyp 

        ; BEGIN (* GplTeTfsLeadingModsNoDel *) 
            LOOP 
              IF ( GplTeEstTravInfo . EtiChildNo < 0 ) 
                 (* No more Est children at all. *) 
                 OR ( FsNodeRef . FsFmtNo # GplTeEstTravInfo . EtiChildFmtNo ) 
                    (* Next child for a different FmtNo *) 
              THEN 
                EXIT 
              END (* IF *) 
            (* Found a child for this FmtNo. *) 
            ; TYPECASE GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => EXIT 

              (* Blank line. *) 
              | ModHs . ModBlankLineTyp 
              => GplTeTfsModBlankLine ( ) 

              (* Leading comment. *) 
              | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
              => IF TModCmnt . ModCmntNlAfter 
                 THEN 
                   GplTeTfsStringMod 
                     ( Tok := LbeStd . Tok__CmntAtEndOfLine 
                     , NlBefore := TModCmnt . ModCmntNlBefore  
                     , NlAfter := TRUE
                     ) 
                  ELSE 
                   GplTeTfsStringMod 
                     ( Tok := LbeStd . Tok__Cmnt
                     , NlBefore := TModCmnt . ModCmntNlBefore  
                     , NlAfter := FALSE 
                     ) 
                  END (* IF *) 

              (* Text insertion. *) 
              | ModHs . ModTextTyp ( TModText ) 
              => GplTeTfsStringMod 
                   ( Tok := LbeStd . Tok__ModText 
                   , NlBefore := TModText . ModTextLeftTokToPos = 0 
                   , NlAfter 
                       := TModText . ModTextToPos 
                          = LbeStd . LimitedCharNoInfinity 
                   ) 

              (* Token insertion. *) 
              | EstHs . EstRefTyp ( TEstRef ) 
              => IF TEstRef . EstNodeKind 
                    = EstHs . EstNodeKindTyp . EstNodeKindModTok 
                 THEN 
                   IF GplTeEstTravInfo . EtiChildNo 
                      < GplTeEstTravInfo . EtiParentRef 
                        . KTreeEstChildCtLeftOfNl 
                   THEN
                     LChildIndentPos := EstIndentPos1 
                   ELSE
                     LChildIndentPos := EstIndentPosN 
                   END (* IF *) 
                 ; GplTraverseEst           
                     ( GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     , GplTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                     , RootAbsNodeNo + GplTeEstTravInfo . EtiChildRelNodeNo 
                     , EstUtil . FsRuleForEstNode 
                         ( Lang 
                         , GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                         ) 
                     , LangUtil . FmtKindTyp . FmtKindHoriz 
                     , LChildIndentPos 
                     , LChildIndentPos 
                     ) 
                 ; IF GplState # GplStateTyp . GplStateDoneWithLine 
                   THEN 
                     GplTeDecEstChild ( ) 
                   END (* IF *) 
                 ELSE (* Not a mod. *) 
                   EXIT 
                 END (* IF *)  

              (* Lex error characters. *) 
              | SharedStrings . T ( TString ) 
              => IF SharedStrings . Tok ( TString ) = LbeStd . Tok__LexErrChars 
                 THEN (* Just skip these. *)  
                   GplTeDecEstChild ( ) 
                 ELSE (* Not a Mod at all. *) 
                   EXIT 
                 END (* IF *) 

              (* Error messages. *) 
              | ModHs . ModErrTyp 
              => (* Just skip these. *)  
                GplTeDecEstChild ( ) 

              (* Impossible. *) 
              | ModHs . ModDelTyp 
              , ModHs . ModCmntTrailingTyp 
              => CantHappen ( AFT . A_GplTeTfsLeadingModsNoDel_Bad_mod_kind ) 
                 (* These should have been found earlier. *) 

              ELSE (* A ModDel, or not a leading mod. *) 
                EXIT 
              END (* TYPECASE  CASE *) 
            ; IF GplState = GplStateTyp . GplStateDoneWithLine 
              THEN 
                EXIT 
              END (* IF *) 
            END (* LOOP *) 
          END GplTeTfsLeadingModsNoDel 

      ; PROCEDURE GplTeTfsCheckLeadingDel 
          ( VAR Delete : BOOLEAN ; VAR IsRepair : BOOLEAN ) 
        RAISES { AssertionFailure } 

        (* Caller must ensure only a deletable FsNode is current. *) 

        = BEGIN (* GplTeTfsCheckLeadingDel *) 
            Delete := FALSE 
          ; IsRepair := FALSE 
          ; IF 0 <= GplTeEstTravInfo . EtiChildNo 
            THEN (* There is a tree child. *) 
              TYPECASE GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => 
              | ModHs . ModDelTyp ( TModDel ) 
              => (* A Deletion mod is next. *) 
                 Assert 
                   ( FsNodeRef . FsIsInsideList 
                     OR FsNodeRef . FsFmtNo 
                        >= GplTeEstTravInfo . EtiChildFmtNo 
                   , AFT . A_GplTeTfsCheckLeadingDel_UnconsumedModDel 
                   ) 
              ; IF FsNodeRef . FsFmtNo <= TModDel . ModDelThruFmtNo 
                THEN (* Delete mod applies. *) 
                  Delete := TRUE 
                ; IsRepair 
                    := EstHs . EstChildKindContainsInsertionRepair 
                       IN GplTeEstTravInfo . EtiChildLeafElem . LeKindSet
                ; IF FsNodeRef . FsFmtNo = GplTeEstTravInfo . EtiChildFmtNo 
                  THEN (* We are at the leftmost deleted token, so 
                          consume the ModDel. *) 
                    GplTeDecEstChild ( ) 
                  END (* IF *) 
                END (* IF *) 
              ELSE 
              END (* TYPECASE *) 
            END (* IF *) 
          END GplTeTfsCheckLeadingDel 

      ; PROCEDURE GplTeTfsTrailingMods ( ) 
        RAISES { AssertionFailure } 

        = BEGIN (* GplTeTfsTrailingMods *) 
            LOOP 
              IF ( GplTeEstTravInfo . EtiChildNo < 0 ) 
                 (* No more Est children at all. *) 
                 OR ( FsNodeRef . FsFmtNo # GplTeEstTravInfo . EtiChildFmtNo ) 
                    (* Next child for a different FmtNo *) 
              THEN 
                EXIT 
              END (* IF *) 
            (* Found a child for this FmtNo. *) 
            ; TYPECASE GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
              OF NULL 
              => (* Not a trailing mod. *) 
                 EXIT 
              | ModHs . ModCmntTrailingTyp ( TModCmnt ) 
              => GplTeTfsStringMod 
                   ( Tok := LbeStd . Tok__Cmnt 
                   , NlBefore := FALSE 
                     (* For ModHs . ModCmntTrailingFixedTyp, there could 
                        be an implicit new line before, but we can't 
                        tell, so act as if there isn't. *) 
                   , NlAfter := TModCmnt . ModCmntNlAfter 
                   ) 
              ELSE (* Not a trailing mod. *) 
                EXIT 
              END (* TYPECASE  IF *) 
            ; IF GplState = GplStateTyp . GplStateDoneWithLine 
              THEN 
                EXIT 
              END (* IF *) 
            END (* LOOP *) 
          END GplTeTfsTrailingMods 

      (* Format syntax trees *) 

      ; PROCEDURE GplTeTfsIsAtEndingFmtNoMark ( ) : BOOLEAN 

        = BEGIN (* GplTeTfsIsAtEndingFmtNoMark *) 
            CASE ExistingMark . Kind 
            OF MarkKindTyp . ChildFmtNo 
            => RETURN 
                 RootAbsNodeNo = ExistingMark . EstNodeNo 
                 AND FsNodeRef . FsFmtNo = ExistingMark . FmtNo 
            | MarkKindTyp . LeftSibFmtNo 
            => RETURN 
                 GplTeEstTravInfo . EtiChildNo + 1 
                 < GplTeEstTravInfo . EtiChildCt 
                 AND RootAbsNodeNo + GplTeRightwardChildRelNodeNo 
                     = ExistingMark . EstNodeNo 
                 AND FsNodeRef . FsFmtNo = ExistingMark . FmtNo 
            | MarkKindTyp . RightSibFmtNo 
            => RETURN 
                 RootAbsNodeNo + GplTeEstTravInfo . EtiChildRelNodeNo 
                 = ExistingMark . EstNodeNo 
                 AND FsNodeRef . FsFmtNo = ExistingMark . FmtNo 
            ELSE 
              RETURN FALSE 
            END (* CASE *) 
          END GplTeTfsIsAtEndingFmtNoMark 

      ; PROCEDURE GplTeTfsBuildFmtNoMark ( ) RAISES { AssertionFailure } 

        = VAR LTok : LbeStd . TokTyp 

        ; BEGIN (* GplTeTfsBuildFmtNoMark *) 
            IF GplTeEstTravInfo . EtiIsOptSingletonList 
            THEN LTok := LbeStd . Tok__OptSingletonList
            ELSE 
              LTok := EstUtil . EstTok ( GplTeEstTravInfo . EtiNodeRef ) 
            END (* IF *) 
          ; IF GplTeEstTravInfo . EtiChildCt = 0 
            THEN (* No children, make it ChildFmtNo *) 
              NewMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo := RootAbsNodeNo 
                     , EstNodeCt := 1 
                     , Kind := MarkKindTyp . ChildFmtNo 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := FALSE 
                     , IsImpliedNewLine := FALSE 
                     , Tok := LTok 
                     } 
            ELSIF GplTeEstTravInfo . EtiChildNo 
                  >= GplTeEstTravInfo . EtiChildCt - 1 
            THEN (* We are right of RM Est child.  Make it RightSibFmtNo *)
              NewMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo 
                         := RootAbsNodeNo 
                            + GplTeEstTravInfo . EtiChildRelNodeNo 
                     , EstNodeCt 
                          := EstUtil . EstNodeCt 
                               ( GplTeEstTravInfo . EtiChildLeafElem 
                                 . LeChildRef 
                               ) 
                            + ORD ( EstHs . EstChildKindOptSingletonList 
                                    IN GplTeEstTravInfo . EtiChildLeafElem 
                                       . LeKindSet 
                                  ) 
                     , Kind := MarkKindTyp . RightSibFmtNo 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := FALSE 
                     , IsImpliedNewLine := FALSE 
                     , Tok := LTok 
                     } 
            ELSE (* Make it LeftSibFmtNo *) 
              NewMark 
                := Marks . TokMarkTyp 
                     { EstNodeNo 
                         := RootAbsNodeNo + GplTeRightwardChildRelNodeNo 
                     , EstNodeCt 
                          := EstUtil . EstNodeCt ( GplTeRightwardChildRef )  
                            + ORD ( EstHs . EstChildKindOptSingletonList 
                                    IN GplTeRightwardChildKindSet 
                                  ) 
                     , Kind := MarkKindTyp . LeftSibFmtNo 
                     , FmtNo := FsNodeRef . FsFmtNo 
                     , StartAtEnd := FALSE 
                     , IsImpliedNewLine := FALSE 
                     , Tok := LTok 
                     } 
            END (* IF *) 
          END GplTeTfsBuildFmtNoMark 

      ; PROCEDURE GplTeTfsLineBreak ( FsKind : FsKindTyp ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* GplTeTfsLineBreak *) 
            GplTeTfsCheckLeadingDel 
              ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
          ; CASE GplState 

            OF GplStateTyp . GplStateStartAtBeg 
            => IF StartMark . Kind IN Marks . MarkKindSetEstLeaf 
               THEN (* Starting point must be in a leading mod on this LB. *) 
               ELSE (* This LineBreak is the starting point. *) 
                 Assert 
                   ( NOT LDelete 
                   , AFT . A_GplTeTfsLineBreak_StartDeleted 
                   ) 
               ; GplState := GplStateTyp . GplStatePassingNl 
               END (* IF *) 
            ; GplTeTfsLeadingModsNoDel ( ) 

            | GplStateTyp . GplStateStartAtEnd 
            => (* Starting point must be in a leading mod  on this LB. *) 
               GplTeTfsLeadingModsNoDel ( ) 

            | GplStateTyp . GplStateInLine 
            => IF NOT LDelete 
               THEN 
                 IF FsKind = FsKindTyp . FsKindLineBreakReqd 
                    OR FsApproxFmtKind = LangUtil . FmtKindTyp . FmtKindVert 
                    OR GplTeTfsIsAtEndingFmtNoMark ( ) 
                 THEN (* We know the line break is taken. *) 
                   GplTeTfsBuildFmtNoMark ( ) 
                 ; GplState := GplStateTyp . GplStateDoneWithLine 
                 ELSE (* else, either FsApproxFmtKind = LbeStd . FmtKindHoriz, 
                         (which could be wrong), but right or wrong, 
                         we don't take the line break; or FsApproxFmtKind 
                         = LangUtil.FmtKindFill (which is exact) and 
                         we can't tell, going backwards whether to take 
                         the line break or not, so don't take it. *) 
                   GplTeTfsLeadingModsNoDel ( ) 
                 END (* IF *) 
               END (* IF *) 

            | GplStateTyp . GplStatePassingNl 
            , GplStateTyp . GplStateDoneWithLine 
            => 

            END (* CASE *) 
          END GplTeTfsLineBreak

      ; PROCEDURE GplTeTfsEstChild ( ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 
        ; VAR LChildIsPresent : BOOLEAN := FALSE  

        ; BEGIN (* GplTeTfsEstChild *) 
            GplTeTfsTrailingMods ( ) 
          ; GplTeTfsCheckLeadingDel 
              ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 

          ; CASE GplState 
            OF GplStateTyp . GplStateStartAtBeg 
            , GplStateTyp . GplStateStartAtEnd 
            => IF RootAbsNodeNo > StartMark . EstNodeNo 
               THEN (* Marked node is outside this Est subtree. *) 
(* CHECK: How can this happen? (and in Gnl too.) *) 
                 GplTeDecEstChild ( ) 
               ELSE 
                 Assert 
                   ( 0 <= GplTeEstTravInfo . EtiChildNo 
                     AND GplTeEstTravInfo . EtiChildFmtNo 
                         = FsNodeRef . FsFmtNo 
                   , AFT . A_GplTeTfsEstChild_StartInsideMissingEstChild 
                   ) 
               ; IF EstHs . EstChildKindEstChild 
                    IN GplTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                 THEN 
                   IF ISTYPE 
                        ( GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                        , ModHs . EstDummyTyp 
                        ) (* Including NIL *)  
                   THEN 
(* CHECK: This shouldn't happen either. *) 
                     GplTeDecEstChild ( ) 
                   ELSIF EstHs . EstChildKindTrailingSep 
                         IN GplTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                   THEN (* Skip trailing sep. *) 
                     GplTeDecEstChild ( ) 
                   ELSE 
                     LChildIsPresent := TRUE 
                   ; Assert
                       ( NOT LDelete 
                       , AFT . A_GplTeTfsEstChild_DescendingIntoDeletedChild 
                       ) 
                   ; GplTeTfsEstSubtree ( ) 
                   END (* IF *) 
                 END (* IF *) 
               END (* IF *) 

            | GplStateTyp . GplStatePassingNl , GplStateTyp . GplStateInLine 
            => TravUtil . AssertBwdNoLostFixedChild 
                 ( FsNodeRef , GplTeEstTravInfo ) 
            ; LChildIsPresent 
                 := ( NOT LDelete OR LIsRepair )
                    AND 0 <= GplTeEstTravInfo . EtiChildNo 
                    AND GplTeEstTravInfo . EtiChildFmtNo 
                        = FsNodeRef . FsFmtNo 
                    AND EstHs . EstChildKindEstChild 
                        IN GplTeEstTravInfo . EtiChildLeafElem . LeKindSet 
            ; IF LChildIsPresent 
              THEN 
                IF ISTYPE 
                     ( GplTeEstTravInfo . EtiChildLeafElem . LeChildRef 
                     , ModHs . EstDummyTyp 
                     ) (* Including NIL *)  
                THEN (* NIL or dummy, skip it. *) 
                  GplTeDecEstChild ( ) 
                ELSIF EstHs . EstChildKindTrailingSep 
                      IN GplTeEstTravInfo . EtiChildLeafElem . LeKindSet 
                THEN (* Skip trailing sep. *) 
                  GplTeDecEstChild ( ) 
                ELSE 
                  GplTeTfsEstSubtree ( ) 
                END (* IF *) 
              END (* IF *) 

            | GplStateTyp . GplStateDoneWithLine 
            => 
            END (* CASE *) 

          ; IF GplState # GplStateTyp . GplStateDoneWithLine 
            THEN 
              GplTeTfsLeadingModsNoDel ( ) 
            END (* IF *) 
          END GplTeTfsEstChild 

      ; PROCEDURE GplTeTfsTraverseFsFixedChildren 
          ( FmtKind : LangUtil . FmtKindTyp )  
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LFsChildNo : LangUtil . FsChildNoSignedTyp 

        ; BEGIN (* GplTeTfsTraverseFsFixedChildren *)
            CASE GplState <* NOWARN *> 
            OF GplStateTyp . GplStateStartAtBeg 
            , GplStateTyp . GplStateStartAtEnd 
            => LFsChildNo  
                 := LangUtil . FsChildNoOfFmtNo ( FsNodeRef , GplTeStartFmtNo ) 
            | GplStateTyp . GplStateInLine 
            , GplStateTyp . GplStatePassingNl 
            => IF FsNodeRef . FsChildren = NIL 
               THEN LFsChildNo := - 1 
               ELSE LFsChildNo := NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
               END (* IF *) 
            END (* CASE *) 
          ; LOOP 
              IF LFsChildNo < 0 
              THEN EXIT 
              ELSE 
                GplTeTraverseFs 
                  ( FsNodeRef . FsChildren ^ [ LFsChildNo ] , FmtKind ) 
              ; Assert 
                  ( NOT GplState IN GplStateSetStart 
                  , AFT . A_GplTeTfsTraverseFsFixedChildren_StartingState 
                  ) 
              ; IF GplState = GplStateTyp . GplStateDoneWithLine 
                THEN 
                  EXIT 
                ELSE 
                  DEC ( LFsChildNo ) 
                END (* IF *) 
              END (* IF *) 
            END (* LOOP *) 
          END GplTeTfsTraverseFsFixedChildren 

      ; PROCEDURE GplTeTfsTraverseFsListChildren 
          ( FmtKind : LangUtil . FmtKindTyp )  
        RAISES { AssertionFailure , Thread . Alerted } 
 
        = VAR LFsChildCt : LangUtil . FsChildNoTyp 
        ; VAR LFsChildNo : LangUtil . FsChildNoTyp 

        ; BEGIN (* GplTeTfsTraverseFsListChildren *) 
            LFsChildCt := NUMBER ( FsNodeRef . FsChildren ^ ) 
          ; CASE GplState <* NOWARN *> 
            OF GplStateTyp . GplStateStartAtBeg 
            , GplStateTyp . GplStateStartAtEnd 
            => LFsChildNo 
                 := LangUtil . FsChildNoOfFmtNo ( FsNodeRef , GplTeStartFmtNo ) 
            | GplStateTyp . GplStateInLine 
            , GplStateTyp . GplStatePassingNl 
            => IF FsRuleNodeRef . FsKind 
                  IN LangUtil . FsKindSetEstListTrail 
                     AND GplTeEstTravInfo . EtiParentRef . EstNodeKind 
                         = EstHs . EstNodeKindTyp . EstNodeKindTrail
               THEN (* Start with trailing separators. *) 
                 LFsChildNo := LFsChildCt - 1 
               ELSE (* Start with Ast child. *) 
                 LFsChildNo := 0 
               END (* IF *) 
            END (* CASE *) 
          ; LOOP 
              GplTeTraverseFs 
                ( FsNodeRef . FsChildren ^ [ LFsChildNo ] , FmtKind ) 
            ; Assert 
                ( NOT GplState IN GplStateSetStart 
                , AFT . A_GplTeTfsTraverseFsListChildren_StartingState 
                ) 
            ; IF GplState = GplStateTyp . GplStateDoneWithLine 
              THEN 
                EXIT 
              ELSIF LFsChildNo = 0 AND GplTeEstTravInfo . EtiChildNo < 0 
              THEN 
                EXIT 
              ELSE 
                LFsChildNo := ( LFsChildNo - 1 ) MOD LFsChildCt 
              END (* IF *) 
            END (* LOOP *) 
          END GplTeTfsTraverseFsListChildren 

      ; PROCEDURE GplTeTfsTraverseCondFmtChildren ( ) 
        RAISES { AssertionFailure , Thread . Alerted } 

        = VAR LFsChildNo : LangUtil . FsChildNoTyp 
        ; VAR LFsChildNoSigned : LangUtil . FsChildNoSignedTyp 
        ; VAR LPredicate : BOOLEAN 

        ; BEGIN (* GplTeTfsTraverseCondFmtChildren *) 
            CASE GplState <* NOWARN *> 
            OF GplStateTyp . GplStateStartAtBeg 
            , GplStateTyp . GplStateStartAtEnd 
            => TravUtil . DescendCondFmt 
                 ( Lang 
                 , FsNodeRef := FsNodeRef 
                 , FmtNo := GplTeStartFmtNo 
                 , EstTravInfo := GplTeEstTravInfo 
                 , (* VAR *) Predicate := LPredicate 
                 , (* VAR *) FsChildNo := LFsChildNo 
                 , Bwd := TRUE 
                 ) 
            ; LFsChildNoSigned := LFsChildNo 

            | GplStateTyp . GplStatePassingNl 
            , GplStateTyp . GplStateInLine 
            => LFsChildNoSigned := NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
            ; LPredicate 
                 := TravUtil . DoCondFmtBwd ( Lang , GplTeEstTravInfo , FsNodeRef ) 
            END (* CASE *) 
          ; IF LPredicate 
            THEN 
              LOOP 
                IF LFsChildNoSigned < 0  
                THEN 
                  EXIT 
                ELSE 
                  GplTeTraverseFs 
                    ( FsNodeRef . FsChildren ^ [ LFsChildNoSigned ] 
                    , FsApproxFmtKind 
                    ) 
                ; IF GplState = GplStateTyp . GplStateDoneWithLine 
                  THEN 
                    EXIT 
                  ELSE 
                    DEC ( LFsChildNoSigned ) 
                  END (* IF *) 
                END (* IF *) 
              END (* LOOP *) 
            ELSE 
              GplTeTraverseFs ( FsNodeRef . FsCondAltRef , FsApproxFmtKind ) 
            END (* IF *) 
          END GplTeTfsTraverseCondFmtChildren 

      ; BEGIN (* GplTeTraverseFs *) 
          VAR LDelete : BOOLEAN 
        ; VAR LIsRepair : BOOLEAN 

        ; BEGIN (* Block  GplTeTraverseFs *) 
            CASE FsNodeRef . FsKind <* NOWARN *> 

            (* Beginning of Image *) 
            OF FsKindTyp . FsKindBegOfImage 
            => GplTeTfsTrailingMods ( ) 
            ; Assert 
                ( GplState IN GplStateSetBegOfImage 
                , AFT . A_GplTeTraverseFs_BOIBadState 
                ) 
            ; Assert 
                ( ExistingMark . Kind = MarkKindTyp . Null 
                  OR GplTeTfsIsAtEndingFmtNoMark ( ) 
                , AFT . A_GplTeTraverseFs_BOI_without_hitting_existing 
                ) 
            ; GplTeTfsBuildFmtNoMark ( ) 
            ; AtBegOfImage := GplState IN GplStateSetAtStartingNl 
            ; GplState := GplStateTyp . GplStateDoneWithLine 

            (* EndOfImage *) 
            | FsKindTyp . FsKindEndOfImage 
            => IF GplState IN GplStateSetStart 
                  AND StartMark . Kind IN Marks . MarkKindSetEstLeaf 
               THEN (* Starting point must be in a leading mod. *) 
               ELSE (* This is the starting point. *) 
                 GplState := GplStateTyp . GplStatePassingNl 
               END (* IF *) 
            ; GplTeTfsCheckLeadingDel 
               ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
            ; Assert ( NOT LDelete , AFT . A_GplTeTraverse_DeletedEOI ) 
            ; GplTeTfsLeadingModsNoDel ( ) 

            (* InsTok. *) 
            | FsKindTyp . FsKindInsTok 
            => GplTeTfsTrailingMods ( ) 
            ; GplTeTfsCheckLeadingDel 
                ( (* VAR *) LDelete , (* VAR *) LIsRepair ) 
            ; IF GplState IN GplStateSetStart 
                 AND StartMark . Kind IN Marks . MarkKindSetEstLeaf 
              THEN (* Starting point must be in a leading mod. *) 
              ELSIF NOT LDelete OR LIsRepair 
                    AND GplState = GplStateTyp . GplStatePassingNl 
              THEN 
                GplState := GplStateTyp . GplStateInLine 
              END (* IF *) 
            ; GplTeTfsLeadingModsNoDel ( ) 

            (* Line breaks. *) 
            | FsKindTyp . FsKindLineBreakOpt 
            , FsKindTyp . FsKindLineBreakReqd 
            => GplTeTfsLineBreak ( FsNodeRef . FsKind ) 

            (* Est child cases. *) 
            | FsKindTyp . FsKindEstChildOfFixed 
            , FsKindTyp . FsKindEstChildOfList 
            => GplTeTfsEstChild ( ) 

            (* Ast string, *) 
            | FsKindTyp . FsKindAstString 
            => GplState := GplStateTyp . GplStateInLine 
(* CHECK: I think we really do want to go into GplStateInLine even if the 
          Ast child is Absent or NIL, but check this. *) 

            (* Est fixed and subtree nodes. *) 
            | FsKindTyp . FsKindEstFixedVert 
            , FsKindTyp . FsKindEstFixedHoriz 
            , FsKindTyp . FsKindEstFixedFill 
            => GplTeTfsTraverseFsFixedChildren ( EstApproxFmtKind ) 

            | FsKindTyp . FsKindSubtreeVert 
            => GplTeTfsTraverseFsFixedChildren 
                 ( LangUtil . FmtKindTyp . FmtKindVert ) 

            | FsKindTyp . FsKindSubtreeHoriz 
            , FsKindTyp . FsKindSubtreeFill 
            => GplTeTfsTraverseFsFixedChildren 
                 ( LangUtil . FmtKindTyp . FmtKindUnknown ) 

            (* Est list nodes. *) 
            | FsKindTyp . FsKindEstListVert 
            , FsKindTyp . FsKindEstListHoriz 
            , FsKindTyp . FsKindEstListFill 
            , FsKindTyp . FsKindEstListTrailVert 
            , FsKindTyp . FsKindEstListTrailHoriz 
            , FsKindTyp . FsKindEstListTrailFill 
            => GplTeTfsTraverseFsListChildren ( EstApproxFmtKind ) 

            (* Conditional format. *) 
            | FsKindTyp . FsKindCondFmt 
            => GplTeTfsTraverseCondFmtChildren ( ) 
            END (* CASE FsKind *) 
          END (* Block  GplTeTraverseFs *) 
        END GplTeTraverseFs 

    ; BEGIN (* GplTraverseEst *) 
        IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
      ; IF EstRef # NIL 
        THEN 
          GplTeRightwardChildRelNodeNo := LbeStd . EstNodeNoNull 
        ; GplTeRightwardChildRef := NIL 
        ; GplTeRightwardChildKindSet := EstHs . EstChildKindSetEmpty

; IF RootAbsNodeNo = 1
     AND StartMark . EstNodeNo = 27
  THEN RootAbsNodeNo := 1 
  END

        ; CASE GplState 
          OF GplStateTyp . GplStateStartAtBeg 
          , GplStateTyp . GplStateStartAtEnd 
          => (* Choose the Est child to descend to. *) 
            IF RootAbsNodeNo = StartMark . EstNodeNo 
            THEN (* The mark denotes the root of this Est and an fs child
                    thereof.  This can only be a line break, and those don't
                    have trailing mods.  Also, there are no Est children. *) 
              TravUtil . InitEstTravInfoBwd 
                ( GplTeEstTravInfo , EstRef , KindSet , RootAbsNodeNo )
; IF GplTeEstTravInfo.EtiChildNo < 0
 THEN EstIndentPosN := EstIndentPosN
 END
            ; CASE StartMark . Kind <* NOWARN *> 
              OF MarkKindTyp . Plain 
              , MarkKindTyp . BlankLine 
              => (* The mark denotes the Est root, which is either a leaf
                    or a ModTok For an insertion token. 
                    No need to set to a child. *) 

              | MarkKindTyp . LeftSibFmtNo 
              , MarkKindTyp . RightSibFmtNo 
              => (* NodeRef for these is handled when it is LeChildRef. *)
                CantHappen ( AFT . A_GplTraverseEst_SibFmtNo_on_parent ) 
              END (* CASE StartMark . Kind. *) 
            ELSE (* Mark does not lead to this (parent) EstNode. *) 
              TravUtil . InitToChildContainingNodeNo 
                ( GplTeEstTravInfo 
                , EstRef 
                , StartMark . EstNodeNo - RootAbsNodeNo 
                , KindSet 
                , RootAbsNodeNo 
                )
; IF GplTeEstTravInfo.EtiChildNo < 0
 THEN EstIndentPosN := EstIndentPosN
 END
            ; IF RootAbsNodeNo + GplTeEstTravInfo . EtiChildRelNodeNo 
                 = StartMark . EstNodeNo 
              THEN 
                CASE StartMark . Kind 
                OF MarkKindTyp . LeftSibFmtNo
                => GplTeDecEstChild ( ) 
                | MarkKindTyp . RightSibFmtNo
                => Assert 
                     ( GplTeEstTravInfo . EtiChildNo + 1 
                       = GplTeEstTravInfo . EtiChildCt  
                     , AFT . A_GplTraverseEst_RightSibFmtNo_mark_on_nonrightmost_child
                     ) 
                ELSE 
                END (* CASE StartMark . Kind. *) 
              END (* IF *) 
            END (* IF *)
             
          (* Choose the FmtNo to descend to. *) 
          ; GplTeStartFmtNo := GplTeEstTravInfo . EtiChildFmtNo (* default *)
          ; CASE StartMark . Kind 
            OF MarkKindTyp . ChildFmtNo 
            => IF RootAbsNodeNo = StartMark . EstNodeNo 
               THEN GplTeStartFmtNo := StartMark . FmtNo 
               END (* IF *) 

            | MarkKindTyp . LeftSibFmtNo 
            => IF GplTeEstTravInfo . EtiChildNo + 1 
                  < GplTeEstTravInfo . EtiChildCt 
                  AND RootAbsNodeNo + GplTeRightwardChildRelNodeNo 
                      = StartMark . EstNodeNo 
               THEN 
                 GplTeStartFmtNo := StartMark . FmtNo 
               END (* IF *) 

            | MarkKindTyp . RightSibFmtNo 
            => IF RootAbsNodeNo + GplTeEstTravInfo . EtiChildRelNodeNo 
                  = StartMark . EstNodeNo 
               THEN 
                 GplTeStartFmtNo := StartMark . FmtNo 
               END (* IF *) 

            ELSE 
            END (* CASE StartMark . Kind. *) 

          | GplStateTyp . GplStateInLine 
          , GplStateTyp . GplStatePassingNl 
          => TravUtil . InitEstTravInfoBwd 
               ( GplTeEstTravInfo , EstRef , KindSet , RootAbsNodeNo )
          ; GplTeStartFmtNo := FsRuleNodeRef . FsRightFmtNo 
; IF GplTeEstTravInfo.EtiChildNo < 0
  THEN EstIndentPosN := EstIndentPosN
  END
          END (* CASE GplState *) 
        ; GplTeTraverseFs ( FsRuleNodeRef , EstApproxFmtKind ) 
        ; IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
        END (* IF *) 
      END GplTraverseEst 

  ; BEGIN (* GetPrevLine *) 
      AtBegOfImage := FALSE 
    ; IF StartMark . StartAtEnd 
      THEN 
        GplState := GplStateTyp . GplStateStartAtEnd 
      ELSE 
        GplState := GplStateTyp . GplStateStartAtBeg 
      END (* IF *) 
    ; GplTraverseEst 
        ( EstRef 
        , EstHs . EstChildKindSetEmpty  
        , 0 
        , EstUtil . FsRuleForEstNode ( Lang , EstRef ) 
        , LangUtil . FmtKindTyp . FmtKindVert 
        , EstIndentPos1 := Options . InitialIndentPos 
        , EstIndentPosN := Options . InitialIndentPos 
        ) 
    END GetPrevLine 

(**************************************************************) 

(* VISIBLE: *) 
; PROCEDURE GetLMBegOfImage 
    ( Lang : LbeStd . LangTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; VAR NewMark : Marks . TokMarkTyp 
    ) 
  RAISES { AssertionFailure } 
  (* This will always be the BOI FS item, which is not necessarily the
     normal rightmost of all marks that lead to the same Nl. *) 

  = VAR LFsNodeRef : LangUtil . FsNodeRefTyp 
  ; VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 
  ; VAR LEstTravInfo : TravUtil . EstTravInfoTyp 

  ; BEGIN (* GetLMBegOfImage *) 
(* FIX: This is undermining Display . SecureSucc, applied to the Lines
        header, when the first line of the image is down into the file,
        and has, e.g., a mark with node no 3, and a NlBefore.
*) 
      LFsNodeRef := EstUtil . FsRuleForEstNode ( Lang , EstRef ) 
    ; Assert 
        ( LFsNodeRef . FsKind IN LangUtil . FsKindSetEstFixed 
        , AFT . A_GetLMBegOfImage_BadFsKind 
        ) 
    ; Assert 
        ( NUMBER ( LFsNodeRef . FsChildren ^ ) > 0 
        , AFT . A_GetLMBegOfImage_NoFsChildren 
        ) 
    ; LChildFsNodeRef := LFsNodeRef . FsChildren ^ [ 0 ] 
    ; Assert 
        ( LChildFsNodeRef . FsKind = FsKindTyp . FsKindBegOfImage 
        , AFT . A_GetLMBegOfImage_NotBOI 
        ) 
    ; IF EstRef # NIL AND ISTYPE ( EstRef , EstHs . EstRefTyp ) 
      THEN  
        TravUtil . InitEstTravInfoFwd 
          ( LEstTravInfo 
          , EstRef 
          , EstHs . EstChildKindSetEmpty 
          , ParentAbsNodeNo := 0 
          ) 
      ; NewMark 
          := Marks . TokMarkTyp 
               { EstNodeNo := LEstTravInfo . EtiChildRelNodeNo 
               , EstNodeCt 
                   := EstUtil . EstNodeCt 
                        ( LEstTravInfo . EtiChildLeafElem . LeChildRef )  
                      + ORD ( EstHs . EstChildKindOptSingletonList 
                              IN LEstTravInfo . EtiChildLeafElem . LeKindSet 
                              (* ^Probably can't happen. *) 
                            ) 
               , Kind := MarkKindTyp . LeftSibFmtNo 
               , FmtNo := LChildFsNodeRef . FsFmtNo 
               , StartAtEnd := FALSE 
               , IsImpliedNewLine := FALSE 
               , Tok := EstUtil . EstTok 
                          ( LEstTravInfo . EtiChildLeafElem . LeChildRef ) 
               } 
      ELSE 
        NewMark 
          := Marks . TokMarkTyp 
               { EstNodeNo := 0 
               , EstNodeCt := 1   
               , Kind := MarkKindTyp . ChildFmtNo 
               , FmtNo := LChildFsNodeRef . FsFmtNo 
               , StartAtEnd := FALSE 
               , IsImpliedNewLine := FALSE 
               , Tok := EstUtil . EstTok ( EstRef )  
               } 
      END (* IF *) 
    END GetLMBegOfImage 

(* VISIBLE: *) 
; PROCEDURE GetRMBegOfImage 
    ( Lang : LbeStd . LangTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; VAR NewMark : Marks . TokMarkTyp 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 
  (* The normal rightmost of all marks that lead to the BOI Nl. *) 

  = VAR LLMMark : Marks . TokMarkTyp 
  ; VAR L2ndMark : Marks . TokMarkTyp 
  ; VAR LLineText : Strings . StringTyp 
  ; VAR LTextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
  ; VAR LLineErrArrayRef : PaintHs . LineErrArrayRefTyp 
  ; VAR LBlankLineCt : LbeStd . LineNoTyp 
  ; VAR LDidHitExistingMark : BOOLEAN 
  ; VAR LAtEndOfImage : BOOLEAN 
  ; VAR LAtBegOfImage : BOOLEAN 

  ;  BEGIN (* GetRMBegOfImage *) 
      GetLMBegOfImage ( Lang , EstRef , (* VAR *) LLMMark ) 
(* TODO: Do this in a better way.  Generalize GetNextLine so it can be
         made to stop where we want, instead of going to the line beyond,
         then coming back with GetPrevLine.
*) 
    ; GetNextLine 
        ( Lang  
        , EstRef  
        , StartMark := LLMMark 
        , ExistingMark := Marks . TokMarkNull 
        , (* VAR *) DidHitExistingMark:= LDidHitExistingMark (* Dead. *) 
        , (* VAR *) NewMark := L2ndMark 
        , (* VAR *) AtEndOfImage := LAtEndOfImage 
        , (* VAR *) LineText := LLineText (* Dead. *) 
        , (* VAR *) BlankLineCt := LBlankLineCt (* Dead. *) 
        , (* VAR *) TextAttrArrayRef := LTextAttrArrayRef (* Dead. *) 
        , (* VAR *) LineErrArrayRef := LLineErrArrayRef (* Dead. *) 
        ) 
    ; IF LAtEndOfImage 
      THEN 
        NewMark := LLMMark 
      ELSE 
        GetPrevLine 
          ( Lang 
          , EstRef 
          , StartMark := L2ndMark 
          , ExistingMark := LLMMark 
          , (* VAR *) NewMark := NewMark 
          , (* VAR *) AtBegOfImage := LAtBegOfImage (* Dead *)  
          ) 
      END (* IF *) 
    END GetRMBegOfImage 

(* VISIBLE: *) 
; PROCEDURE GetEndOfImage 
    ( Lang : LbeStd . LangTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ; VAR NewMark : Marks . TokMarkTyp 
    ) 
  RAISES { AssertionFailure } 

  = VAR LFsNodeRef : LangUtil . FsNodeRefTyp 
  ; VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 
  ; VAR LFsChildNo : LangUtil . FsChildNoTyp 
  ; VAR LEstTravInfo : TravUtil . EstTravInfoTyp 

  ; BEGIN (* GetEndOfImage *) 
      LFsNodeRef := EstUtil . FsRuleForEstNode ( Lang , EstRef ) 
    ; Assert 
        ( LFsNodeRef . FsKind IN LangUtil . FsKindSetEstFixed 
        , AFT . A_GetEndOfImage_BadFsKind 
        ) 
    ; LFsChildNo := NUMBER ( LFsNodeRef . FsChildren ^ ) - 1 
    ; Assert ( LFsChildNo >= 0 , AFT . A_GetEndOfImage_NoFsChildren ) 
    ; LChildFsNodeRef := LFsNodeRef . FsChildren ^ [ LFsChildNo ] 
    ; Assert 
        ( LChildFsNodeRef . FsKind = FsKindTyp . FsKindEndOfImage 
        , AFT . A_GetEndOfImage_NotEndOfImage 
        ) 
    ; IF EstRef # NIL AND ISTYPE ( EstRef , EstHs . EstRefTyp ) 
      THEN  
        TravUtil . InitEstTravInfoBwd 
          ( LEstTravInfo 
          , EstRef 
          , EstHs . EstChildKindSetEmpty 
          , ParentAbsNodeNo := 0 
          ) 
      ; NewMark 
          := Marks . TokMarkTyp 
               { EstNodeNo := LEstTravInfo . EtiChildRelNodeNo 
               , EstNodeCt 
                   := EstUtil . EstNodeCt 
                        ( LEstTravInfo . EtiChildLeafElem . LeChildRef )  
                      + ORD ( EstHs . EstChildKindOptSingletonList 
                              IN LEstTravInfo . EtiChildLeafElem . LeKindSet 
                              (* ^Probably can't happen. *) 
                            ) 
               , Kind := MarkKindTyp . RightSibFmtNo 
               , FmtNo := LChildFsNodeRef . FsFmtNo 
               , StartAtEnd := FALSE 
               , IsImpliedNewLine := FALSE 
               , Tok := EstUtil . EstTok  
                          ( LEstTravInfo . EtiChildLeafElem . LeChildRef ) 
               } 
      ELSE 
        NewMark 
          := Marks . TokMarkTyp 
               { EstNodeNo := 0 
               , EstNodeCt := 1   
               , Kind := MarkKindTyp . ChildFmtNo 
               , FmtNo := LChildFsNodeRef . FsFmtNo 
               , StartAtEnd := FALSE 
               , IsImpliedNewLine := FALSE 
               , Tok := EstUtil . EstTok ( EstRef )   
               } 
      END (* IF *) 
    END GetEndOfImage 

; BEGIN (* LineMarks *) 
  END LineMarks 
. 
