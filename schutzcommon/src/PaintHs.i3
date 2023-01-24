
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE PaintHs 

(* Data structures for painting the screen. *) 

; IMPORT FormsVBT 
; IMPORT VBT 
; IMPORT VBTClass (* VBT.T is a MUTEX *)  
; IMPORT Wr 

; IMPORT Assertions 
; FROM Failures IMPORT Backout 
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT Marks 
; IMPORT MessageCodes 
; IMPORT ModHs 
; IMPORT PortTypes 
; IMPORT ScannerIf 
; IMPORT Strings
; IMPORT Thread 

<* PRAGMA LL *> 

(* Window sets (of windows into a single image) *) 

; CONST WindowNoMin = 0 
; CONST WindowNoMax = 7 

; TYPE WindowNoTyp = [ WindowNoMin .. WindowNoMax ] 
; TYPE WindowNoSetTyp = SET OF WindowNoTyp 

; CONST WindowNoSetEmpty = WindowNoSetTyp { } 
; CONST WindowNoSetFull = WindowNoSetTyp { WindowNoMin .. WindowNoMax } 

; PROCEDURE WindowNoSetImage ( Set : WindowNoSetTyp ; Indent : INTEGER ) : TEXT

(* Text attributes *) 

; TYPE TextAttrComponentTyp = [ 0 .. 5 ] 
  (* Values of type TextAttrComponentTyp are editor-oriented. They will be 
     mapped before painting using a map that is screen-dependent and 
     option-dependent. *)
(* TODO: Replace this by distinct enumerations for the 4 components. *)   

; TYPE TextAttrTyp = RECORD 
    TaCharPos : LbeStd . LimitedCharNoTyp 
    (* These attributes apply to character positions starting with
       this one, and extending to (not thru') this field of the array
       element to the left, or the rest of the line, if this is the
       last array element. *) 
  ; TaBgColor : BITS 4 FOR TextAttrComponentTyp 
  ; TaFgColor : BITS 4 FOR TextAttrComponentTyp 
  ; TaFont : BITS 4 FOR TextAttrComponentTyp 
  ; TaDecoration : BITS 4 FOR TextAttrComponentTyp 
  END 

; PROCEDURE TextAttrIsEqual ( Left , Right : TextAttrTyp ) : BOOLEAN 
  (* Equal, except for the TaCharPos field. *) 

; CONST TaBgColorPlain = 0 
; CONST TaBgColorCmnt = 1
; CONST TaBgColorLiteral = 2
; CONST TaBgColorSelected = 3
; CONST TaBgColorMatched = 4

; CONST TaFgColorPlain = 0 
; CONST TaFgColorCmnt = 1 
; CONST TaFgColorLiteral = 2 
; CONST TaFgColorIdent = 3 
; CONST TaFgColorPlaceholder = 4 

; CONST TaFontPlain = 0 
; CONST TaFontBold = 1 
; CONST TaFontItalic = 2 
; CONST TaFontBoldItalic = 3 

; CONST TaDecPlain = 0 
; CONST TaDecStrikeout = 1 
; CONST TaDecUnderline1 = 2 
; CONST TaDecUnderline2 = 3 
; CONST TaDecCaret = 4 

; CONST TextAttrDefault 
    = TextAttrTyp  
        { TaCharPos := 0  
        , TaBgColor := TaBgColorPlain 
        , TaFgColor := TaFgColorPlain  
        , TaFont := TaFontPlain  
        , TaDecoration := TaDecPlain  
        } 

; CONST TextAttrSelected  
    = TextAttrTyp  
        { TaCharPos := 0  
        , TaBgColor := TaBgColorSelected  
        , TaFgColor := TaFgColorPlain  
        , TaFont := TaFontPlain  
        , TaDecoration := TaDecPlain  
        } 

; CONST TextAttrMatched  
    = TextAttrTyp  
        { TaCharPos := 0  
        , TaBgColor := TaBgColorMatched 
        , TaFgColor := TaFgColorPlain  
        , TaFont := TaFontPlain  
        , TaDecoration := TaDecPlain  
        } 

; CONST TextAttrTouched  
    = TextAttrTyp  
               { TaCharPos := 0  
               , TaBgColor := TaBgColorPlain
               , TaFgColor := TaFgColorPlain  
               , TaFont := TaFontPlain  
               , TaDecoration := TaDecUnderline2  
               } 

; CONST TextAttrTyped   
    = TextAttrTyp  
               { TaCharPos := 0  
               , TaBgColor := TaBgColorPlain  
               , TaFgColor := TaFgColorPlain  
               , TaFont := TaFontPlain  
               , TaDecoration := TaDecUnderline1  
               } 

; CONST TextAttrErr 
    = TextAttrTyp  
        { TaCharPos := 0  
        , TaBgColor := TaBgColorPlain 
        , TaFgColor := TaFgColorCmnt  
        , TaFont := TaFontBoldItalic  
        , TaDecoration := TaDecPlain  
        } 

; TYPE TextAttrArrayTyp = ARRAY OF TextAttrTyp 
; CONST TextAttrArrayEmpty = TextAttrArrayTyp { } 

; TYPE TextAttrArrayRefTyp = REF TextAttrArrayTyp 

(* Error node references. *) 

; TYPE LineErrTyp = RECORD 
    LeModRef : ModHs . ModErrTyp 
  ; LeCharPos : LbeStd . LimitedCharNoTyp 
  END 

; TYPE LineErrArrayTyp = ARRAY OF LineErrTyp 
; TYPE LineErrArrayRefTyp = REF LineErrArrayTyp 

(* Display lines. *) 

  (* LinesRefs are linked into a double, circular list, with a list
     header pointed to by IpLineHeaderRef.  The header has type
     LinesRefHeaderTyp.  All other nodes on the list have type
     LinesRefMeatTyp.

     Only a subset of the lines in the image necessarily have
     LinesRefs at a given time.  This will include all LinesRefs which
     are visible in any window of the image, plus possibly some extras
     that are leftover from earlier, that are being kept around to
     prevent excessive reconstruction of LinesRefs (and thus a lot of
     garbage) when scrolling up and down.
 
     It takes N+1 LinesRefs to hold the TokMarks to the N+1 new lines
     surrounding N lines of visible text.  The (N+1)st LinesRef in a
     contiguous group has LrGapAfter = TRUE, no line text and no
     meaningful value of LrLineCt.

     For the header, LrGapAfter = FALSE means the following LinesRef
     is known to be for the first line of the image.  For a
     LinesRefMeatTyp, LrGapAfter = FALSE means the next LinesRef in
     the list is known to be for the next line and have a LrBolTokMark
     that denotes the next line.  The LinesRefMeat with LrGapAfter =
     FALSE is "complete".
     
     LrGapAfter = TRUE only means we don't know about what follows in
     the list.  If it is of type LinesRefMeatTyp, it is "incomplete".
     An incomplete LinesRef has empty text, zero LrLineCt, and can't
     be visible, since we don't have its text.
 
     The last non-header LinesRef either has LrGapAfter = TRUE, or it
     denotes EOI, has zero LrLineCt, no LrLineText, and its
     LrBolTokMark is the rightmost Nl of EOI.

  INVARIANT: 
     NOT IpLineHeaderRef . LrGapAfter 
     => IpLineHeaderRef . LrRightLink holds the BegOfImage TokMark. 
 
  INVARIANT: 
     NOT IpLineHeaderRef . LrLeftLink . LrGapAfter 
     => IpLineHeaderRef . LrLeftLink holds the EndOfImage TokMark. 
 
   *) 
; TYPE LinesRefTyp 
    = OBJECT 
        LrLeftLink : LinesRefTyp := NIL 
      ; LrRightLink : LinesRefTyp := NIL 
      ; LrGapAfter : BOOLEAN 
      END (* OBJECT *) 

; TYPE LinesRefHeaderTyp = LinesRefTyp 

; TYPE LinesRefMeatTyp 
    = LinesRefTyp 
        OBJECT 
          LrBolTokMark : Marks . TokMarkTyp
        ; LrTextAttrArrayRef : TextAttrArrayRefTyp := NIL 
        ; LrLineErrArrayRef : LineErrArrayRefTyp := NIL 
        ; LrVisibleIn : WindowNoSetTyp := WindowNoSetEmpty 
        ; LrLineCt : LbeStd . LineNoTyp := 0 
          (* INVARIANT: ( LrLineCt > 0 ) IMPLIES ( LrLineLen = 0 ) *) 
        ; LrIsStopper : BOOLEAN := FALSE 
          (* A LinesRef which backwards traversal was able to 
             stop at.  Someday, we might like to compute this properly 
             for LinesRefs constructed going forward too.  For now, it 
             is approximated in this case by TRUE. 
          *) 
        ; LrHasMark : BOOLEAN := FALSE 
          (* ^A LineMarkMeatTyp points to this. *)         
        ; LrFromPos : LbeStd . LimitedCharNoTyp := 0  
          (* Leading blanks (to left of here) are not stored in LrLineText. *) 
        ; LrLineLen : LbeStd . LimitedCharNoTyp := 0 
          (* Including the unstored leading blanks. *) 
        ; LrLineText : TEXT := NIL (* Leading blanks removed. *) 
        END (* OBJECT LinesRefMeatTyp *) 

; PROCEDURE LinesRefImage ( FLinesRef : LinesRefTyp ; LineIndent : INTEGER )
  : TEXT 
  RAISES { Thread . Alerted , Wr . Failure } 

; PROCEDURE IncludeLrVisibleIn 
    ( Ref : LinesRefMeatTyp ; Value : WindowNoTyp ) 

; PROCEDURE ExcludeLrVisibleIn 
    ( Ref : LinesRefMeatTyp ; Value : WindowNoTyp ) 

; PROCEDURE LineIsVisible ( Ref : LinesRefMeatTyp ) : BOOLEAN 

; PROCEDURE InsertLinesRefToRight 
    ( InsertToRightOfRef : LinesRefTyp ; RefToInsert : LinesRefMeatTyp ) 

; PROCEDURE InsertLinesRefToLeft 
    ( InsertToLeftOfRef : LinesRefTyp ; RefToInsert : LinesRefMeatTyp ) 

; PROCEDURE UnlinkLinesRef ( LinesRef : LinesRefMeatTyp ) 
  (* Delete LinesRef from whatever list it is in. 
     No action if LinesRef is NIL. 
  *) 

; PROCEDURE InsertLinesRefRangeToLeft 
    ( InsertToLeftOfRef : LinesRefTyp 
    ; FromLinesRef : LinesRefMeatTyp 
    ; ThruLinesRef : LinesRefMeatTyp 
    ) 
  (* No action if any of the pointers is NIL.  Otherwise, assumes 
     InsertToLeftOfRef is in a double, circular linked list and 
     FromLinesRef .. ThruLinesRef are doubly-linked within, ends don't matter. 
  *) 

; PROCEDURE UnlinkLinesRefRange 
    ( FromLinesRef : LinesRefMeatTyp ; ThruLinesRef : LinesRefMeatTyp ) 
  (* Delete FromLinesRef .. ThruLinesRef from whatever list they are in. 
     No action if either pointer is NIL.  Otherwise, assumes they are in a
     double, circular linked list. Leave the nodes linked among themselves, 
     so they can be reinserted somewhere later. 
  *) 

; PROCEDURE ResetAllLrHasMarkFields ( ImageTrans : ImageTransientTyp ) 

(* Marks *) 
(* Each image has a doubly-linked list of LineMarks. The header 
   of this list is non-information-bearing, of type 
   LineMarkHeaderTyp.  The ImageTransientTyp object has a ref 
   to the header, and it also points back to the image. 
 
   Each window can have five LineMarks of LineMarkMeatTyp, one 
   of each kind in [MarkSsStartSel .. MarkSsEndMatch].
   The WindowRef has an array of pointers to these, and each 
   points back to the window and also notes which kind of mark 
   it is. 
 
   The LineMarks are linked in the order of their marked 
   locations within the image. 
*) 
; TYPE MarkSsTyp 
    = { MarkSsNull 
      , MarkSsStartSel   (* Start point of selected text. *) 
      , MarkSsCursor     (* The true cursor. *) 
      , MarkSsEndSel     (* End point of selected text. *) 
      , MarkSsStartMatch (* Start point of matched text. *)  
      , MarkSsEndMatch   (* End point of matched text. *) 
      , MarkSsTemp       (* Used temporarily by a single operation. *) 
      }

; PROCEDURE MarkSsImage ( MarkSsValue : MarkSsTyp ) : TEXT 

; TYPE LineMarkTyp 
    = OBJECT  
        LmLeftLink : LineMarkTyp := NIL 
      ; LmRightLink : LineMarkTyp := NIL 
      END (* OBJECT *) 

; TYPE LineMarkHeaderTyp 
    = LineMarkTyp OBJECT END (* OBJECT *)
(* TODO ^ Eliminate uses of this, in favor of LineMarkTyp. *) 

; TYPE LineMarkMeatTyp 
    = LineMarkHeaderTyp 
        OBJECT 
          LmWindowRef : WindowRefTyp 
        ; LmLinesRef : LinesRefMeatTyp 
        ; LmMarkSs : MarkSsTyp 
        ; LmTokMark : Marks . TokMarkTyp 
        ; LmCharPos : LbeStd . LimitedCharNoSignedTyp 
        ; LmLineNo : LbeStd . LineNoTyp 
        END (* OBJECT *) 

; TYPE MarkArrayTyp 
    = ARRAY [ MarkSsTyp . MarkSsStartSel .. MarkSsTyp . MarkSsTemp ] 
      OF LineMarkMeatTyp 

; PROCEDURE RecomputeLrHasMark ( Mark : LineMarkMeatTyp ) 
  (* Mark is about to be removed from this line. If it points to a LinesRec,
     maybe reset the LrHasMark field of the LinesRec. *)  

; PROCEDURE LinkLineMarkToLeft 
    ( InsertToLeftOfRef : LineMarkTyp ; RefToInsert : LineMarkMeatTyp ) 
  RAISES { Backout } 

; PROCEDURE LinkLineMarkToRight 
    ( InsertToRightOfRef : LineMarkTyp ; RefToInsert : LineMarkMeatTyp ) 
  RAISES { Backout } 

; PROCEDURE UnlinkLineMark ( LineMark : LineMarkMeatTyp ) 
  (* Delete LineMark from whatever list it is in. 
     No action if LineMark or either of its links is NIL. *) 

; PROCEDURE UnlinkLineMarkRange 
    ( FromMark : LineMarkMeatTyp ; ThruMark : LineMarkMeatTyp ) 
  (* Delete FromMark .. ThruMark from whatever list they are in. 
     No action if either pointer is NIL.  Otherwise, assumes they are in a
     double, circular linked list. Leave the nodes linked among themselves, 
     so they can be reinserted somewhere later. *) 
  (* NOTE: Does not update IpMarkCt! *) 

; PROCEDURE InsertLineMarkToRight 
    ( InsertMark : LineMarkMeatTyp 
    ; PredMark : LineMarkTyp 
    ; Image : ImageTransientTyp := NIL 
    )
  RAISES { Backout } 
  (* Higher-level.  Links in and also takes care of IpMarkCt and LrHasMark. *) 

; PROCEDURE InsertLineMarkToLeft 
    ( InsertMark : LineMarkMeatTyp 
    ; SuccMark : LineMarkTyp 
    ; Image : ImageTransientTyp := NIL 
    )
  RAISES { Backout } 
  (* Higher-level.  Links in and also takes care of IpMarkCt and LrHasMark. *) 

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

; PROCEDURE DeleteLineMark
    ( Mark : LineMarkMeatTyp 
    ; VAR PredMark : LineMarkTyp 
      (* For caller to keep in case it needs to undo the deletion. *)  
    ; Image : ImageTransientTyp := NIL 
    ) 
  (* Higher-level.  Unlinks and also takes care of IpMarkCt and LrHasMark. *) 

; PROCEDURE SwapMarkOrder ( LeftMark : LineMarkTyp ) 
  (* Swap LeftMark with its right neighbor. *) 

; PROCEDURE CompareLineMarks ( Left , Right : LineMarkMeatTyp ) 
  : [ - 1 .. 1 ] 
  RAISES { Backout } 
  (* Takes into account LmTokMark , LmLineNo, and LmCharPos *) 

; PROCEDURE GetMarksInOrder 
    ( Mark1 : LineMarkMeatTyp
    ; Mark2 : LineMarkMeatTyp
    ; VAR Left : LineMarkMeatTyp
    ; VAR Right : LineMarkMeatTyp
    ) 
  RAISES { Backout } 
  (* Put Mark1 and Mark2 into nondescending order. *) 

; PROCEDURE BruteForceVerifyLineMarks 
    ( ImageTrans : ImageTransientTyp ; DoCheckOrder : BOOLEAN := TRUE ) 
  RAISES { Backout } 

; EXCEPTION Nonlinear 

; PROCEDURE WriteLinesListToStdout  
    ( StartLinesRef : LinesRefTyp ; Lang : LbeStd . LangTyp )

; PROCEDURE WriteLinesListToFile 
    ( FileName : TEXT ; StartLinesRef : LinesRefTyp ; Lang : LbeStd . LangTyp )

; PROCEDURE WriteLinesListWr
    ( WrT : Wr . T
    ; StartLinesRef : LinesRefTyp
    ; Lang : LbeStd . LangTyp
    ; FileLabel : TEXT
    )
  RAISES { Nonlinear , Thread . Alerted , Wr . Failure } 
  (* May be circular, with or without a header, or linear with NILs at ends. *)

; PROCEDURE TextAttrArrayCt 
    ( ImageTrans : ImageTransientTyp ) : CARDINAL 
  RAISES { Backout } 

; PROCEDURE TextAttrElemCt 
    ( ImageTrans : ImageTransientTyp ) : CARDINAL 
  RAISES { Backout } 

; PROCEDURE DisplayTextAttrStats ( ImageTrans : ImageTransientTyp ) 

; TYPE TempEditRefTyp 
    = OBJECT 
        TeLinesRef : LinesRefMeatTyp 
      ; TeEditedString : Strings . T 
        (* ^Contains leading blanks, in case they are involved in editing. *) 
      ; TeTextAttrArrayRef : TextAttrArrayRefTyp := NIL  
      ; TeLineErrArrayRef : LineErrArrayRefTyp := NIL 
      ; TeTextAttrActualSize : PortTypes . Int32Typ 
      ; TeDelFromPos : LbeStd . LimitedCharNoTyp 
      ; TeDelToPos : LbeStd . LimitedCharNoTyp 
      ; TeInsLen : LbeStd . LimitedCharNoTyp 
      ; TeLineNo : LbeStd . LineNoTyp 
      END (* OBJECT TempEditReFTyp *) 

; TYPE TempEditStateTyp = { TeStateIdle , TeStateText , TeStateSynt } 

(* Versions *) 

; TYPE VerNoTyp = PortTypes . Card32Typ 
; TYPE VerUpdKindTyp = PortTypes . Card8Typ 
(* CHECK: ^What is this for? 2002-05-23, only an unused field in 
          PaintHs . ImageTransientTyp *) 
; TYPE VerStateTyp 
    = { VerStateNull 
      , VerStateTextEdited 
      , VerStateCleanParsed 
      , VerStateErrorParsed 
      , VerStateSemanticized 
      } 

(* Images *) 

; CONST IrMagicNo = 16_C0C4C8CC 

(* TODO: Delete this unused type and its initialization procedure. *) 
; TYPE ImageRefTyp 
    = LbeStd . LbeRootTyp 
        OBJECT
          IrMagic : INTEGER := IrMagicNo 
        ; IrLang : LbeStd . LangTyp := LbeStd . LangNull 
        ; IrLangIdRef : LangUtil . LangIdRefTyp := NIL 
          (* ^For the language this image is in. 
             Equals LangUtil . LangIdRef ( IrLang ) (Eases debugging.)  
          *) 
        ; IrVisibleIn : WindowNoSetTyp  
        ; IrWindowList : WindowRefTyp := NIL 
        ; IrLineHeaderRef : LinesRefTyp := NIL 
        (* ^Contentless list header of doubly-, circularly-linked 
            list of LinesRefs for the image. *) 
        ; IrEstRoot : LbeStd . EstRootTyp := NIL 
        ; IrSemRoot : REFANY 
        ; IrMarkCt : LbeStd . MarkNoTyp := 0 
        ; IrMarkHeader : LineMarkHeaderTyp := NIL
        ; IrTempEditRef : TempEditRefTyp := NIL 
        ; IrImageName : TEXT := NIL 
        (* ^Simple name, with text file suffix. *) 
        ; IrAbsPklFileName : TEXT := NIL 
        (* ^Canonical file system path name, with pickle suffix. *) 
        ; IrAbsTextFileName : TEXT := NIL 
        ; IrHistoryWrT : Wr . T := NIL  
        ; IrHistoryText : TEXT := NIL  
        ; IrLastCommand : TEXT := NIL 
        ; IrCrashCommand : TEXT := NIL 
        ; IrTempEditState : TempEditStateTyp := TempEditStateTyp . TeStateIdle 
        ; IrVerNo : VerNoTyp := 0 
        ; IrVerUpdKind : VerUpdKindTyp := 0 
        ; IrVerState : VerStateTyp := VerStateTyp . VerStateNull 
        ; IrLineCt : LbeStd . LineNoTyp := 0 
          (* This is maintained only approximately during incremental
             reparsing.  It is used only for scrollbar thumb positioning. *)
        ; IrCrashCode : MessageCodes . T := MessageCodes . T . NullCode  
        ; IrLineCtIsExact : BOOLEAN := FALSE  
        ; IrIsMutatedSinceCommandStarted : BOOLEAN := FALSE 
        ; IrScannerIf : ScannerIf . ScanIfTyp 
        ; IrIsSaved : BOOLEAN := TRUE 
        ; IrIsParsed : BOOLEAN := TRUE 
        ; IrIsAnalyzed : BOOLEAN := TRUE 
        METHODS 
          InitDefaults ( ) : ImageRefTyp := InitDefaults 
        END (* OBJECT *) 

; PROCEDURE InitDefaults ( ImageRef : ImageRefTyp ) : ImageRefTyp 

; CONST IpMagicNo = 16_C4C8CCC0 (* Easy to spot. *) 

; TYPE ImagePersistentTyp 
    = LbeStd . LbeRootTyp 
        OBJECT
          IpMagic : INTEGER := IpMagicNo 
        ; IpLang : LbeStd . LangTyp := LbeStd . LangNull 
        ; IpLineHeaderRef : LinesRefTyp := NIL 
        (* ^Contentless list header of doubly-, circularly-linked 
            list of LinesRefs for the image. *) 
        ; IpEstRoot : LbeStd . EstRootTyp := NIL 
        ; IpSemRoot : REFANY := NIL 
        ; IpMarkCt : LbeStd . MarkNoTyp := 0 
        ; IpMarkHeader : LineMarkHeaderTyp := NIL 
        ; IpTempEditState : TempEditStateTyp 
            := TempEditStateTyp . TeStateIdle 
        ; IpTempEditRef : TempEditRefTyp := NIL 
        ; IpImageName : TEXT := NIL 
        (* ^Simple name, with text file suffix. *) 
        ; IpAbsPklFileName : TEXT := NIL 
        (* ^Canonical file system path name, with pickle suffix. *) 
        ; IpAbsTextFileName : TEXT := NIL 
        ; IpVerNo : VerNoTyp := 0 
        ; IpVerUpdKind : VerUpdKindTyp := 0 
        ; IpVerState : VerStateTyp := VerStateTyp . VerStateNull 
        ; IpHistoryText : TEXT := NIL  
        ; IpLineCtDisplay : LbeStd . LineNoTyp := 0 
          (* ^This is maintained only approximately during incremental
             reparsing.  It is used only for scrollbar thumb positioning. 
          *)
        ; IpLineCtIsExact : BOOLEAN := FALSE  
        ; IpLastCommand : TEXT := NIL 
        ; IpCrashCommand : TEXT := NIL 
        ; IpCrashCode : INTEGER := ORD ( MessageCodes . T . NullCode )  
          (* Actually, MessageCodes . T, but that would make pickles
             sensitive in any change to the complement of messages,
             so it's converted to an integer here.
          *)  
        ; IpSchutzDate : TEXT 
        ; IpIsParsed : BOOLEAN := TRUE 
        ; IpIsAnalyzed : BOOLEAN := TRUE 
        ; IpIsMutatedSinceCommandStarted : BOOLEAN := FALSE 
        METHODS 
          initDefaults ( ) : ImagePersistentTyp := IpInitDefaults 
        END (* OBJECT *) 

; PROCEDURE IpInitDefaults ( Ip : ImagePersistentTyp ) 
  : ImagePersistentTyp 

; TYPE ImageTransientTyp 
    = LbeStd . LbeRootTyp 
        OBJECT
          ItPers : ImagePersistentTyp := NIL 
        ; ItLangIdRef : LangUtil . LangIdRefTyp := NIL 
          (* ^For the language this image is in. 
             A cache of LangUtil . LangIdRef ( ItPersistent . IpLang ) 
             (Simplifies debugging.)  
          *) 
        ; ItVisibleIn : WindowNoSetTyp  := WindowNoSetEmpty 
        ; ItWindowList : WindowRefTyp := NIL 
        ; ItHistoryWrT : Wr . T := NIL  
        ; ItScannerIf : ScannerIf . ScanIfTyp := NIL 
        ; ItIsSaved : BOOLEAN := TRUE 
        METHODS 
          initDefaults ( ) : ImageTransientTyp := ItInitDefaults 
        END (* OBJECT *) 

; PROCEDURE ItInitDefaults ( It : ImageTransientTyp ) 
  : ImageTransientTyp 

(* Windows *) 

; TYPE WindowPrivateTyp <: VBT . T

; TYPE WindowPublic 
    = WindowPrivateTyp 
        OBJECT 
          WrFirstLineLinesRef : LinesRefMeatTyp 
        ; WrFirstLineLineNo : LbeStd . LineNoTyp 
     (* ; WrLastLinesRef : LinesRefTyp *) 
        ; WrImageRef : ImageTransientTyp 
        ; WrImageLink : WindowRefTyp  (* Links all windows into this 
                                         window's image. *) 
        ; WrStackLink : WindowRefTyp  (* Links all windows into a desktop 
                                         stack.  (shouldn't this be done in 
                                         a lower level module? *) 
        ; WrWindowNo : WindowNoTyp    (* Among the windows into its image. *) 
        ; WrCursorLineNoInWindow : LbeStd . LineNoTyp 
        ; WrHorizScroll 
          : LbeStd . LimitedCharNoTyp
            (* ^This many chars are left of leftmost visible char. *) 
        ; WrVertScroll : LbeStd . LineNoTyp := 0 
          (* This many lines are above the window.  This value is
             sometimes estimated. *) 
        ; WrVertScrollIsExact : BOOLEAN := TRUE 
        ; WrMarks : MarkArrayTyp 
        (* WrMarks [ MarkSsCursor ] 
           gives the location of the cursor in this window. *) 
        ; WrInsertMode : BOOLEAN := TRUE 
        ; WrMatchStartMark : LineMarkMeatTyp := NIL 
        ; WrMatchEndMark : LineMarkMeatTyp := NIL 
        ; WrMatchedString : TEXT := NIL 
        ; WrSearchForm : FormsVBT . T := NIL (* For separate search window. *) 
        METHODS
          init ( ) : WindowRefTyp 
          <* LL.sup = SELF *> 
        END (* OBJECT WindowPublic *) 

; TYPE WindowRefTyp <: WindowPublic 

; PROCEDURE InitWindowRef ( WindowRef : WindowRefTyp ) : WindowRefTyp 

; END PaintHs 
. 
