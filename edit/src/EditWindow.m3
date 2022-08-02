
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE EditWindow 

; IMPORT Axis 
; IMPORT Color  
; IMPORT Fmt 
; IMPORT Font 
; IMPORT FormsVBT 
; IMPORT KeyboardKey 
; IMPORT KeyTrans 
; IMPORT PaintOp 
; IMPORT Palette 
; IMPORT Path 
; IMPORT Pixmap
; IMPORT PlttFrnds 
; IMPORT Point 
; IMPORT Rect 
; IMPORT Region
; IMPORT RTIO 
; IMPORT ScrnFont 
; IMPORT ScrnPaintOp  
; IMPORT ScrnPixmap 
; IMPORT Stdio  
; IMPORT Text 
; IMPORT Thread 
; IMPORT VBT 
; IMPORT VBTRep 
; IMPORT Wr 

; IMPORT Assertions 
; IMPORT Display 
; IMPORT Errors 
; FROM Failures IMPORT Backout  
; IMPORT LbeStd 
; IMPORT MessageCodes 
; IMPORT Options 
; IMPORT PaintHs  
; IMPORT Selection 
; IMPORT Strings 
; IMPORT TextEdit 
; IMPORT Ui
; IMPORT UiRecPlay   
; IMPORT WindowPrivate 
; IMPORT Worker 

; TYPE AFT = MessageCodes . T 

<* PRAGMA LL *> 

; CONST Point1_1 = Point . T { 1 , 1 } 

; TYPE CursorStoredStateTyp 
    = [ CursorStateTyp . CsBlinkingOff .. CursorStateTyp . CsSolid ] 

; TYPE SerifRectsTyp = ARRAY [ 0 .. 3 ] OF Rect . T 
; TYPE CursorTyp 
    = RECORD 
        State : CursorStoredStateTyp := CursorStateTyp . CsInvisible 
      ; SerifRects := SerifRectsTyp { Rect . Empty , .. } 
      ; TranslatedSerifRects := SerifRectsTyp { Rect . Empty , .. } 
      ; VertRect : Rect . T := Rect . Empty 
      ; TranslatedVertRect : Rect . T := Rect . Empty 
      ; OriginSaveRect : Rect . T := Rect . Empty 
      ; SavedRect : Rect . T := Rect . Empty 
      ; SavedPixmap : ScrnPixmap . T := NIL 
      ; CharPoint : CharPointTyp 
          := CharPointTyp { FIRST ( INTEGER ) , FIRST ( INTEGER ) } 
      END (* RECORD *) 

; TYPE PaintOps2DTyp  
    = ARRAY PaintHs . TextAttrComponentTyp , PaintHs . TextAttrComponentTyp
      OF PaintOp . T 

; TYPE PaintOpsTyp  
    = ARRAY PaintHs . TextAttrComponentTyp 
      OF PaintOp . T 

; TYPE FontsTyp = ARRAY PaintHs . TextAttrComponentTyp OF Font . T 

; TYPE RedoStateTyp 
    = { Starting 
      , Rescreening 
      , Reshaping
      , Repainting
      , Done 
      } 

; REVEAL T 
    = WindowPublic 
        BRANDED 
          "EditWindow" 
          OBJECT 
            EwForm : FormsVBT . T := NIL 

          (* Independent info: *) 
          ; EwFonts : FontsTyp 
          ; EwDomain : Rect . T := Rect . Empty 
          ; EwScreenType : VBT . ScreenType := NIL 
          ; EwMargin : Point . T := Point . Origin 
          ; EwVertGap : PixelCoordTyp 
            := MinVertGap (* pixels between rows. *) 

          (* Derived info: *) 
          ; EwCharSeparation : Point . T 
          ; EwCharOrigin : Point . T 
          ; EwCharSize : Point . T (* Size of one character cell. *) 
          ; EwOriginCharBoundingBox : Rect . T 
            (* ^Bounding box in pixel coordinates of the character at the 
               character coordinate origin. *) 
          ; EwOriginCellBoundingBox : Rect . T 
            (* ^Expanded with pixels around the font's cell. *)
(*REMOVE:             
          ; EwPaintOpBg : PaintOp . T 
          ; EwPaintOpBorder : PaintOp . T 
          ; EwPaintOpFg : PaintOp . T 
          ; EwPaintOpBgFg : PaintOp . T 
          ; EwPaintOps2D : PaintOps2DTyp 
          ; EwPaintOpsBg : PaintOpsTyp 
          ; EwPaintOpsDec : PaintOpsTyp 
          ; EwPaintOpsChar : PaintOpsTyp
*)
          
          ; EwCharsRect 
            : Rect . T (* Where actual characters are displayed. *) 
          ; EwSouthEastChar : CharPointTyp 
            (* ^Uses the TO-invariant. *) 
          ; EwPathLeftArrow : Path . T := NIL 
          ; EwPathRightArrow : Path . T := NIL 
          ; EwArrowBoundingBox : Rect . T := Rect . Empty 

          (* Decoration points for the cell at char coordinate origin: *) 
          ; EwStrikeoutFrom : Point . T 
          ; EwStrikeoutThru : Point . T 
          ; EwUnderlineFrom : Point . T 
          ; EwUnderlineThru : Point . T 
          ; EwCaretFrom : Point . T 
          ; EwCaretMiddle : Point . T 
          ; EwCaretThru : Point . T 

          (* Normal operating state info: *) 
          ; EwOwnsFocus : BOOLEAN := FALSE 
          ; EwCursor : CursorTyp 

          (* Info about pending rescreen, reshape, redisplay , and repaint: *)
          ; EwRedoState := RedoStateTyp . Done 
          ; EwHasRescreenPending : BOOLEAN := FALSE  
          ; EwHasReshapePending : BOOLEAN := FALSE  
          ; EwHasRedisplayPending : BOOLEAN := FALSE  
          ; EwHasRepaintPending : BOOLEAN := FALSE  
          ; EwPendingRescreenRec : VBT . RescreenRec 
          ; EwPendingReshapeRec : VBT . ReshapeRec 
          ; EwPendingRepaintRegion : Region . T 
          ; EwRedoClosure : Worker . ClosureTyp := NIL 

          METHODS 
            init 
              ( Form : FormsVBT . T 
              ; font : Font . T 
              ; Margin : Point . T := Point . Origin 
                (* Which means some other default. *) 
              ; VertGap : PixelCoordTyp := MinVertGap 
              ) 
            : T 
            RAISES { Backout } 
            := Init 

          OVERRIDES 
            mouse := Mouse 
          ; position := Position 
          ; redisplay := Redisplay 
          ; misc := Misc 
          ; key := Key 
          ; discard := Discard 
          ; reshape := Reshape 
          ; rescreen := Rescreen 
          ; repaint := Repaint 
          ; shape := Shape 
          ; read := Read 
          ; write := Write 
          END (* OBJECT *) 

; PROCEDURE Form ( Window : T ) : FormsVBT . T 
  (* The FormsVBT.T that this window is inside of. *)    

  = BEGIN
      LOCK Window
      DO RETURN Window . EwForm
      END (* LOCK *) 
    END Form 

; VAR DefaultFont := Font . BuiltIn 

; TYPE BlanksRefTyp = REF ARRAY OF CHAR 

<* LL >= {VBT.mu} *> 
; VAR BlanksRef : BlanksRefTyp := NIL 

; PROCEDURE ExpandBlanks ( Size : CharCoordTyp ) 
  <* LL >= VBT.mu *> 

  = BEGIN (* ExpandBlanks *) 
      IF BlanksRef = NIL OR NUMBER ( BlanksRef ^ ) < Size 
      THEN 
        BlanksRef := NEW ( BlanksRefTyp , Size * 2 ) 
      ; FOR I := 0 TO NUMBER ( BlanksRef ^ ) - 1 
        DO BlanksRef ^ [ I ] := ' ' 
        END (* FOR *) 
      END (* IF *) 
    END ExpandBlanks 

; PROCEDURE CharToPixel 
    ( Window : T ; CharPoint : CharPointTyp ) : Point . T 

  = BEGIN (* CharToPixel *)
      LOCK Window DO 
        RETURN 
          Point . T 
            { Window . EwCharOrigin . h 
              + CharPoint . h * Window . EwCharSeparation . h 
            , Window . EwCharOrigin . v 
              + CharPoint . v * Window . EwCharSeparation . v 
            } 
      END (* LOCK *) 
    END CharToPixel 

; PROCEDURE PixelOffset 
    ( Window : T ; CharPoint : CharPointTyp ) : Point . T 
  (* Convert a character point to a pixel point by simple scaling. *) 

  = BEGIN (* PixelOffset *) 
      LOCK Window DO 
        RETURN 
          Point . T 
            { CharPoint . h * Window . EwCharSeparation . h 
            , CharPoint . v * Window . EwCharSeparation . v 
            } 
      END (* LOCK *) 
    END PixelOffset 

; <* UNUSED *> 
(* This is just an experimental procedure to see what we can get from
   the fonts available. *) 
  PROCEDURE GetFonts ( ST : VBT . ScreenType ) 

  = VAR LPlainFont : Font . T  
  ; VAR LBoldFont : Font . T  
  ; VAR LSlantFont : Font . T  
  ; VAR LFixedFont : Font . T  
  ; VAR LDefaultFont : Font . T  

  ; VAR LPlainScrnFont : ScrnFont . T 
  ; VAR LBoldScrnFont : ScrnFont . T 
  ; VAR LSlantScrnFont : ScrnFont . T 
  ; VAR LFixedScrnFont : ScrnFont . T 
  ; VAR LDefaultScrnFont : ScrnFont . T 

  ; VAR LPlainMetrics : ScrnFont . Metrics 
  ; VAR LBoldMetrics : ScrnFont . Metrics 
  ; VAR LSlantMetrics : ScrnFont . Metrics 
  ; VAR LFixedMetrics : ScrnFont . Metrics 
  ; VAR LDefaultMetrics : ScrnFont . Metrics 

  ; BEGIN 
      LPlainFont := Font . FromName 
                 ( ARRAY OF TEXT 
                    { "-*-fixed-medium-r-*-*-13-*-*-*-*-*-*-*" } 
                 ) 
    ; LBoldFont := Font . FromName 
                 ( ARRAY OF TEXT 
                    { "-*-fixed-bold-r-*-*-13-*-*-*-*-*-*-*" } 
                 ) 
    ; LSlantFont := Font . FromName 
                 ( ARRAY OF TEXT 
                    { "-*-fixed-medium-o-*-*-13-*-*-*-*-*-*-*" } 
                 ) 
    ; LFixedFont := Font . FromName 
                 ( ARRAY OF TEXT 
                    { "Fixed" } 
                 ) 
    ; LDefaultFont := DefaultFont 

    ; LPlainScrnFont 
        := Palette . ResolveFont ( ST , LPlainFont ) 
    ; LBoldScrnFont 
        := Palette . ResolveFont ( ST , LBoldFont ) 
    ; LSlantScrnFont 
        := Palette . ResolveFont ( ST , LSlantFont ) 
    ; LFixedScrnFont 
        := Palette . ResolveFont ( ST , LFixedFont ) 
    ; LDefaultScrnFont 
        := Palette . ResolveFont ( ST , LDefaultFont ) 

    ; LPlainMetrics := LPlainScrnFont . metrics 
    ; LBoldMetrics := LBoldScrnFont . metrics 
    ; LSlantMetrics := LSlantScrnFont . metrics 
    ; LFixedMetrics := LFixedScrnFont . metrics 
    ; LDefaultMetrics := LDefaultScrnFont . metrics 

    ; EVAL Point . T { 0 , 0 } 

    END GetFonts

; PROCEDURE PrimaryToReal ( Primary : Options . PrimaryTyp ) : REAL 

  =

  VAR R1 , R2 , R3 , R4 : REAL
;

  BEGIN

  R1 := FLOAT ( Primary ) 
; R2 := FLOAT ( Options . PrimaryMax ) 
; R3 := R1 / R2  

; R4 := FLOAT ( Primary ) / FLOAT ( Options . PrimaryMax )
; 
      RETURN FLOAT ( Primary ) / FLOAT ( Options . PrimaryMax ) 
    END PrimaryToReal 

; <* UNUSED *> (* But maybe needed someday. *) 
  PROCEDURE RealColor 
    ( Color : Options . ColorTyp 
    ; VAR Red : REAL 
    ; VAR Green : REAL 
    ; VAR Blue : REAL 
    ) 

  = BEGIN 
      Red := PrimaryToReal ( Color . Red ) 
    ; Green := PrimaryToReal ( Color . Green ) 
    ; Blue := PrimaryToReal ( Color . Blue )  
    END RealColor 

; PROCEDURE PaintOpFromColor ( Color : Options . ColorTyp ) : PaintOp . T 

  = VAR LRed : REAL 
  ; VAR LGreen : REAL 
  ; VAR LBlue : REAL 

  ; BEGIN 
      LRed := PrimaryToReal ( Color . Red ) 
    ; LGreen := PrimaryToReal ( Color . Green ) 
    ; LBlue := PrimaryToReal ( Color . Blue )  
    ; RETURN PaintOp . FromRGB ( LRed , LGreen , LBlue )  
    END PaintOpFromColor 

; PROCEDURE SetBgOps ( VAR Ops : PaintOpsTyp ) 
  (* Initializes Ops with tint PaintOps for background colors. *) 

  = BEGIN 
      Ops [ PaintHs . TaBgColorPlain ] 
        := PaintOpFromColor ( Options . BgColorPlain ) 
    ; Ops [ PaintHs . TaBgColorCmnt ] 
        := PaintOpFromColor ( Options . BgColorCmnt ) 
    ; Ops [ PaintHs . TaBgColorLiteral ] 
        := PaintOpFromColor ( Options . BgColorLiteral ) 
    ; Ops [ PaintHs . TaBgColorSelected ] 
        := PaintOpFromColor ( Options . BgColorSelected ) 
    ; Ops [ PaintHs . TaBgColorMatched ] 
        := PaintOpFromColor ( Options . BgColorMatched ) 
    END SetBgOps 

; PROCEDURE SetDecOps ( VAR Ops : PaintOpsTyp ) 
  (* Initialize Ops with transparent/Fg PaintOps for decorations. *) 

  = BEGIN 
      Ops [ PaintHs . TaDecPlain ] 
        := PaintOp . Pair ( PaintOp . Transparent , PaintOp . Transparent ) 
      (* ^Defensive.  Probably won't be used. *) 
    ; Ops [ PaintHs . TaDecStrikeout ] 
        := PaintOp . Pair 
             ( PaintOp . Transparent 
             , PaintOpFromColor ( Options . DecColorErr ) 
             ) 
    ; Ops [ PaintHs . TaDecCaret ] 
        := PaintOp . Pair 
             ( PaintOp . Transparent 
             , PaintOpFromColor ( Options . DecColorErr ) 
             ) 
    ; Ops [ PaintHs . TaDecUnderline1 ] 
        := PaintOp . Pair 
             ( PaintOp . Transparent 
             , PaintOpFromColor ( Options . DecColorTyped ) 
             ) 
    ; Ops [ PaintHs . TaDecUnderline2 ] 
        := PaintOp . Pair 
             ( PaintOp . Transparent 
             , PaintOpFromColor ( Options . DecColorTouched ) 
             ) 
    END SetDecOps 

; PROCEDURE SetCharOps ( VAR Ops : PaintOpsTyp ) 
  (* Initialize Ops with transparent/Fg PaintOps for characters. *) 

  = BEGIN 
      Ops [ PaintHs . TaFgColorPlain ] 
        := PaintOp . Pair 
             ( PaintOp . Transparent 
             , PaintOpFromColor ( Options . FgColorPlain ) 
             ) 
    ; Ops [ PaintHs . TaFgColorIdent ] 
        := PaintOp . Pair 
             ( PaintOp . Transparent 
             , PaintOpFromColor ( Options . FgColorIdent ) 
             ) 
    ; Ops [ PaintHs . TaFgColorLiteral ] 
        := PaintOp . Pair 
             ( PaintOp . Transparent 
             , PaintOpFromColor ( Options . FgColorLiteral ) 
             ) 
    ; Ops [ PaintHs . TaFgColorCmnt ] 
        := PaintOp . Pair 
             ( PaintOp . Transparent 
             , PaintOpFromColor ( Options . FgColorCmnt ) 
             ) 
    ; Ops [ PaintHs . TaFgColorPlaceholder ] 
        := PaintOp . Pair 
             ( PaintOp . Transparent 
             , PaintOpFromColor ( Options . FgColorPlaceholder ) 
             ) 
    END SetCharOps 

(*
; PROCEDURE TextPaintOp ( Bg , Fg  : PaintHs . TextAttrComponentTyp ) 
  : PaintOp . T 

  = VAR LBgColor : Options . ColorTyp  
  ; VAR LFgColor : Options . ColorTyp 

  ; VAR LRed : REAL 
  ; VAR LGreen : REAL 
  ; VAR LBlue : REAL 

  ; VAR LBgOp : PaintOp . T 
  ; VAR LFgOp : PaintOp . T 
  ; VAR LResultOp : PaintOp . T 

  ; BEGIN 
      CASE Bg 
      OF PaintHs . TaBgColorPlain
      => LBgColor := Options . BgColorPlain
      | PaintHs . TaBgColorTyped 
      => LBgColor := Options . BgColorTyped
      | PaintHs . TaBgColorTouched 
      => LBgColor := Options . BgColorTouched
      | PaintHs . TaBgColorErr 
      => LBgColor := Options . BgColorErr
      END (* CASE *) 
    ; LRed := PrimaryToReal ( LBgColor . Red ) 
    ; LGreen := PrimaryToReal ( LBgColor . Green ) 
    ; LBlue := PrimaryToReal ( LBgColor . Blue )  
    ; LBgOp := PaintOp . FromRGB ( LRed , LGreen , LBlue ) 
    ; CASE Fg
      OF PaintHs . TaFgColorPlain 
      => LFgColor := Options . FgColorPlain
      | PaintHs . TaFgColorIdent 
      => LFgColor := Options . FgColorIdent
      | PaintHs . TaFgColorLiteral 
      => LFgColor := Options . FgColorLiteral
      | PaintHs . TaFgColorCmnt 
      => LFgColor := Options . FgColorCmnt
      END (* CASE *) 
    ; LRed := PrimaryToReal ( LFgColor . Red ) 
    ; LGreen := PrimaryToReal ( LFgColor . Green ) 
    ; LBlue := PrimaryToReal ( LFgColor . Blue )  
    ; LFgOp := PaintOp . FromRGB ( LRed , LGreen , LBlue ) 
    ; LResultOp := PaintOp . Pair ( LBgOp , LFgOp ) 
    ; RETURN LResultOp 
    END TextPaintOp 
*)

(*
; PROCEDURE BgDecPaintOp 
    ( Bg : PaintHs . TextAttrComponentTyp ; FgColor : Options . ColorTyp ) 
  : PaintOp . T 

  = VAR LBgColor : Options . ColorTyp  

  ; VAR LRed : REAL 
  ; VAR LGreen : REAL 
  ; VAR LBlue : REAL 

  ; VAR LBgOp : PaintOp . T 
  ; VAR LFgOp : PaintOp . T 
  ; VAR LResultOp : PaintOp . T 

  ; BEGIN 
      CASE Bg 
      OF PaintHs . TaBgColorPlain
      => LBgColor := Options . BgColorPlain
      | PaintHs . TaBgColorTyped 
      => LBgColor := Options . BgColorTyped
      | PaintHs . TaBgColorTouched 
      => LBgColor := Options . BgColorTouched
      | PaintHs . TaBgColorErr 
      => LBgColor := Options . BgColorErr
      END (* CASE *) 
    ; LRed := PrimaryToReal ( LBgColor . Red ) 
    ; LGreen := PrimaryToReal ( LBgColor . Green ) 
    ; LBlue := PrimaryToReal ( LBgColor . Blue )  
    ; LBgOp := PaintOp . FromRGB ( LRed , LGreen , LBlue ) 
    ; LRed := PrimaryToReal ( FgColor . Red ) 
    ; LGreen := PrimaryToReal ( FgColor . Green ) 
    ; LBlue := PrimaryToReal ( FgColor . Blue )  
    ; LFgOp := PaintOp . FromRGB ( LRed , LGreen , LBlue ) 
    ; LResultOp := PaintOp . Pair ( LBgOp , LFgOp ) 
    ; RETURN LResultOp 
    END BgDecPaintOp 
*) 

; PROCEDURE ComputeDecorationPoints ( Window : T ) 
  <* LL.sup = Window *> 

  = VAR Lh , Lw , Lv : INTEGER 

  ; BEGIN 
      Lv := ( Window . EwOriginCharBoundingBox . south 
              - Window . EwOriginCharBoundingBox . north 
            ) DIV 2  
            + Window . EwOriginCharBoundingBox . north
    ; Window . EwStrikeoutFrom . h 
       := Window . EwOriginCharBoundingBox . west - 1    
    ; Window . EwStrikeoutThru . h 
       := Window . EwOriginCharBoundingBox . east   
    ; Lw := Window . EwStrikeoutThru . h   
            - Window . EwStrikeoutFrom . h 
    ; Window . EwStrikeoutFrom . v 
        := Lv 
           + Lw DIV 2 (* For diagonal strikeout. *) 
    ; Window . EwStrikeoutThru . v 
        := Window . EwStrikeoutFrom . v 
           - Lw (* For diagonal strikeout. *)  

    ; Window . EwUnderlineFrom . v 
        := Window . EwOriginCharBoundingBox . south + 1  
    ; Window . EwUnderlineFrom . h 
       := Window . EwOriginCharBoundingBox . west 
    ; Window . EwUnderlineThru . v 
        := Window . EwOriginCharBoundingBox . south + 1  
    ; Window . EwUnderlineThru . h 
       := Window . EwOriginCharBoundingBox . east - 1  

    ; Lh := ( Window . EwOriginCharBoundingBox . east  
              - Window . EwOriginCharBoundingBox . west 
            ) DIV 2 
            + Window . EwOriginCharBoundingBox . west
    ; Window . EwCaretFrom . v 
        := Window . EwOriginCharBoundingBox . south + 2  
    ; Window . EwCaretFrom . h := Lh - 2 
    ; Window . EwCaretMiddle . v 
        := Window . EwOriginCharBoundingBox . south   
    ; Window . EwCaretMiddle . h := Lh  
    ; Window . EwCaretThru . v 
        := Window . EwOriginCharBoundingBox . south + 2  
    ; Window . EwCaretThru . h := Lh + 2 
    END ComputeDecorationPoints 

; PROCEDURE InitArrows ( Window : T ) 
  <* LL.sup = Window *> 

  = VAR LSouth : INTEGER 

  ; BEGIN 
    (* Use rectangles 6 pixels wide, whose north coordinate is zero and whose
       south edge aligns with the south edge of a character cell, thus both
       the arrow rectangle and the character cell have the same height.  
       To paint the arrow, shift so its north edge and the north edge of
       characters on the line are the same. 
    *) 
      LSouth := Window . EwCharSize . v 
    ; Window . EwArrowBoundingBox 
        := Rect . FromEdges ( w := 0 , e := 6 , n := 0 , s := LSouth ) 

    ; Window . EwPathLeftArrow := NEW ( Path . T ) 
    ; Path . MoveTo 
        ( Window . EwPathLeftArrow , Point . T { 5 , LSouth - 10 } )
    ; Path . LineTo 
        ( Window . EwPathLeftArrow , Point . T { 1 , LSouth - 6 } )
    ; Path . LineTo 
        ( Window . EwPathLeftArrow , Point . T { 5 , LSouth - 2 } )
    ; Path . Close ( Window . EwPathLeftArrow ) 

    ; Window . EwPathRightArrow := NEW ( Path . T ) 
    ; Path . MoveTo 
        ( Window . EwPathRightArrow , Point . T { 1 , LSouth - 10 } )
    ; Path . LineTo 
        ( Window . EwPathRightArrow , Point . T { 5 , LSouth - 6 } )
    ; Path . LineTo 
        ( Window . EwPathRightArrow , Point . T { 1 , LSouth - 2 } )
    ; Path . Close ( Window . EwPathRightArrow ) 
    END InitArrows 

; PROCEDURE ComputeDerivedWindowInfo ( Window : T ) 
  RAISES { Backout } 
  <* LL.sup = Window *> 

  = VAR LMetrics : ScrnFont . Metrics 
  ; VAR LFontBoundingBox : Rect . T 
  ; VAR LCharsSize : Point . T 

  ; BEGIN (* ComputeDerivedWindowInfo *) 
      IF Window . EwScreenType # NIL 
      THEN 
        Window . EwFonts [ PaintHs . TaFontPlain ]  
          := Font . FromName
               ( ARRAY OF TEXT { Options . FontNamePlain } , useXft := TRUE ) 
      ; Window . EwFonts [ PaintHs . TaFontBold ]  
          := Font . FromName
               ( ARRAY OF TEXT { Options . FontNameBold } , useXft := TRUE ) 
      ; Window . EwFonts [ PaintHs . TaFontItalic ]  
          := Font . FromName
               ( ARRAY OF TEXT { Options . FontNameItalic } , useXft := TRUE ) 
      ; Window . EwFonts [ PaintHs . TaFontBoldItalic ]  
          := Window . EwFonts [ PaintHs . TaFontItalic ]  

      ; LMetrics 
          := Palette . ResolveFont 
               ( Window . EwScreenType 
               , Window . EwFonts [ PaintHs . TaFontPlain ]  
               ) 
             . metrics 
(* TODO: Checks on consistency of font metrics. *) 
      ; CASE LMetrics . spacing 
        OF ScrnFont . Spacing . Monospaced
        , ScrnFont . Spacing . CharCell 
        , ScrnFont . Spacing . Any   
        , ScrnFont . Spacing . Proportional    
        => LFontBoundingBox := LMetrics . maxBounds . boundingBox 
        ; Window . EwCharSize 
            := Point . Sub 
                 ( Rect . SouthEast ( LFontBoundingBox ) 
                 , Rect . NorthWest ( LFontBoundingBox ) 
                 ) 
        ; Window . EwCharSeparation 
            := Point . T 
                 { LMetrics . maxBounds . printWidth 
                 , Window . EwCharSize . v + Window . EwVertGap 
                 } 
        ; IF NOT Rect . IsEmpty ( Window . EwDomain ) 
          THEN 
            Window . EwCharOrigin 
              := Point . Add 
                   ( Rect . NorthWest ( Window . EwDomain ) 
                   , Point . Sub 
                       ( Window . EwMargin 
                       , Rect . NorthWest ( LFontBoundingBox ) 
                       ) 
                   ) 
          ; Window . EwOriginCharBoundingBox 
              := Rect . Add ( LFontBoundingBox , Window . EwCharOrigin ) 
          ; Window . EwOriginCellBoundingBox 
              := Rect . Change 
                   ( Window . EwOriginCharBoundingBox 
                   , dw := 0 
                   , de := 0
                   , dn := 0
                   , ds := Window . EwVertGap 
                   ) 
          ; ComputeDecorationPoints ( Window ) 
          ; Window . EwCharsRect 
              := Rect . Change 
                   ( Window . EwDomain 
                   , Window . EwMargin . h 
                   , - Window . EwMargin . h 
                   , Window . EwMargin . v 
                   , - Window . EwMargin . v 
                   ) 
          ; LCharsSize 
              := Point . Sub 
                   ( Rect . SouthEast ( Window . EwCharsRect ) 
                   , Rect . NorthWest ( Window . EwCharsRect ) 
                   ) 
          ; DEC 
              ( Window . EwCharsRect . east 
              , LCharsSize . h MOD Window . EwCharSeparation . h 
              ) 
          ; DEC 
              ( Window . EwCharsRect . south 
              , ( LCharsSize . v + Window . EwVertGap ) 
                MOD Window . EwCharSeparation . v 
              ) 
          ; LCharsSize 
              := Point . Sub 
                   ( Rect . SouthEast ( Window . EwCharsRect ) 
                   , Rect . NorthWest ( Window . EwCharsRect ) 
                   ) 
          ; Window . EwSouthEastChar . h 
              := ( LCharsSize . h - 1 ) DIV Window . EwCharSize . h + 1 
          ; Window . EwSouthEastChar . v 
              := ( LCharsSize . v - 1 ) 
                 DIV ( Window . EwCharSize . v + Window . EwVertGap ) 
                 + 1 
          ; ExpandBlanks ( Window . EwSouthEastChar . h ) 
          ; Window . EwCursor . VertRect 
              := Rect . FromEdges 
                   ( w := Window . EwOriginCharBoundingBox . west - 1 
                   , e := Window . EwOriginCharBoundingBox . west 
                   , n := Window . EwOriginCharBoundingBox . north - 1 
                   , s := Window . EwOriginCharBoundingBox . south + 1 
                   ) 
          ; Window . EwCursor . SerifRects 
              := SerifRectsTyp 
                   { Rect . FromPoint 
                       ( Point . T 
                           { h := Window . EwOriginCharBoundingBox . west - 2 
                           , v := Window . EwOriginCharBoundingBox . north - 2 
                           } 
                       ) 
                   , Rect . FromPoint 
                       ( Point . T 
                           { h := Window . EwOriginCharBoundingBox . west 
                           , v := Window . EwOriginCharBoundingBox . north 
                                  - 2 
                           } 
                       ) 
                   , Rect . FromPoint 
                       ( Point . T 
                           { h := Window . EwOriginCharBoundingBox . west - 2 
                           , v := Window . EwOriginCharBoundingBox . south + 1 
                           } 
                       ) 
                   , Rect . FromPoint 
                       ( Point . T 
                           { h := Window . EwOriginCharBoundingBox . west 
                           , v := Window . EwOriginCharBoundingBox . south 
                                  + 1 
                           } 
                       ) 
                   } 
          ; Window . EwCursor . OriginSaveRect 
              := Rect . Join 
                   ( Window . EwCursor . SerifRects [ 0 ] 
                   , Window . EwCursor . SerifRects [ 3 ]  
                   ) 
          ; Window . EwCursor . CharPoint 
              := Point . Min 
                   ( Window . EwCursor . CharPoint 
                   , Point . Sub ( Window . EwSouthEastChar , Point1_1 ) 
                   ) 
          END (* IF *) 
        ELSE
          Assertions . Message ( AFT . E_NoFixedFontFound )
        ; Assertions . DoTerminate := TRUE 
        ; RAISE Backout
                  ( MessageCodes . Image ( AFT . E_NoFixedFontFound ) ) 
        END (* CASE *)
(*
      ; Window . EwPaintOpBg 
          := PaintOp . FromRGB
               ( PrimaryToReal ( Options . BgColorPlain . Red )  
               , PrimaryToReal ( Options . BgColorPlain . Green )  
               , PrimaryToReal ( Options . BgColorPlain . Blue )  
               ) 
     ; Window . EwPaintOpFg 
          := PaintOp . FromRGB
               ( PrimaryToReal ( Options . FgColorPlain . Red )  
               , PrimaryToReal ( Options . FgColorPlain . Green )  
               , PrimaryToReal ( Options . FgColorPlain . Blue )  
               ) 
      ; Window . EwPaintOpBgFg 
          := PaintOp . Pair 
               ( Window . EwPaintOpBg , Window . EwPaintOpFg ) 
      ; Window . EwPaintOpBorder  
          := PaintOp . FromRGB
               ( PrimaryToReal ( Options . BgColorBorder . Red )  
               , PrimaryToReal ( Options . BgColorBorder . Green )  
               , PrimaryToReal ( Options . BgColorBorder . Blue )  
               ) 
      ; SetBgOps ( Window . EwPaintOpsBg ) 
      ; SetDecOps ( Window . EwPaintOpsDec ) 
      ; SetCharOps ( Window . EwPaintOpsChar ) 
(* 
      ; FOR RBg := FIRST ( PaintHs . TextAttrComponentTyp ) 
            TO LAST ( PaintHs . TextAttrComponentTyp ) 
        DO 
          Window . EwBgDecOpsError 
            := BgDecPaintOp ( RBg , Options . FgColorError ) 
        END (* FOR *)   
      ; FOR RBg := FIRST ( PaintHs . TextAttrComponentTyp ) 
            TO LAST ( PaintHs . TextAttrComponentTyp ) 
        DO 
          Window . EwBgDecOpsTyped 
            := BgDecPaintOp ( RBg , Options . FgColorTyped ) 
        END (* FOR *)   
      ; FOR RBg := FIRST ( PaintHs . TextAttrComponentTyp ) 
            TO LAST ( PaintHs . TextAttrComponentTyp ) 
        DO 
          Window . EwBgDecOpsTouched
            := BgDecPaintOp ( RBg , Options . FgColorTouched 
        END (* FOR *)   
*) 
(* 
      ; FOR RBg := FIRST ( PaintHs . TextAttrComponentTyp ) 
            TO LAST ( PaintHs . TextAttrComponentTyp ) 
        DO
          FOR RFg := FIRST ( PaintHs . TextAttrComponentTyp ) 
              TO LAST ( PaintHs . TextAttrComponentTyp ) 
          DO
            Window . EwPaintOps2D [ RBg , RFg ] 
              := TextPaintOp ( RBg , RFg ) 
          END 
        END 
*)

*)
      ; InitArrows ( Window ) 
      END (* IF *) 
    END ComputeDerivedWindowInfo 

; VAR MinMargin := Point . T { 6 , 3 } 

(* EXPORTED *) 
; PROCEDURE Init 
    ( Window : T  
    ; Form : FormsVBT . T 
    ; font : Font . T 
    ; Margin : Point . T := Point . Origin 
      (* Which means some other default. *) 
    ; VertGap : PixelCoordTyp := MinVertGap 
    ) 
    : T 
  RAISES { Backout } 
  <* LL.sup < Window *> 

  = BEGIN (* Init *)
      IF Window # NIL
      THEN
        LOCK Window
        DO 
          EVAL PaintHs . WindowRefTyp . init ( Window )  
          (* Apparently doesn't exist: VBT . Leaf . init ( Window ) *)
        ; Window . EwForm := Form 
        ; Window . EwFonts [ PaintHs . TaFontPlain ] := font 
        ; Window . EwMargin := Point . Max ( Margin , MinMargin ) 
        ; Window . EwVertGap := MAX ( VertGap , MinVertGap ) 
        ; Window . EwDomain := Rect . Empty 
        ; ComputeDerivedWindowInfo ( Window ) 
        ; Window . EwOwnsFocus := FALSE 
        ; Window . EwRedoState := RedoStateTyp . Done 
        ; Window . EwHasRescreenPending := FALSE  
        ; Window . EwHasReshapePending := FALSE  
        ; Window . EwHasRedisplayPending := FALSE  
        ; Window . EwHasRepaintPending := FALSE  
        ; Window . EwRedoClosure (* We only need one of these per window. *)  
            := NEW ( Worker . ClosureTyp 
                   , Window := Window 
                   , apply := RedoWorkProc 
                   )
        END (* LOCK *) 
      ; Display . ClearWindow ( Window ) 
      END (* IF *) 
    ; RETURN Window 
    END Init 

(* EXPORTED *) 
; PROCEDURE SouthEastToCorner ( Window : WindowTyp ) : CharPointTyp 
  (* Follows the usual _to_ invariant, i.e. the first char _not_ included. *) 

  = BEGIN (* SouthEastToCorner *) 
      LOCK Window DO 
        RETURN Window . EwSouthEastChar 
      END (* LOCK *) 
    END SouthEastToCorner 

; PROCEDURE PaintCursor ( Window : WindowTyp ; State : CursorStoredStateTyp ) 
  <* LL.sup < Window . mu *> 

  = BEGIN (* PaintCursor *) 
(* TODO: Consistify cursor paint ops with character paintops *) 
      WITH WCursor = Window . EwCursor 
      DO 
        CASE State 
        OF CursorStateTyp . CsBlinkingOff , CursorStateTyp . CsInvisible 
        => IF WCursor . SavedPixmap = NIL 
          THEN 
            VBT . PolyTint 
              ( Window 
              , WCursor . TranslatedSerifRects
              , Ui . GDerivedInfoRef ^ . DiPaintOpBg 
              ) 
          ; VBT . PaintTint 
              ( Window 
              , WCursor . TranslatedVertRect 
              , Ui . GDerivedInfoRef ^ . DiPaintOpBg 
              ) 
          ELSE 
            VBT . PaintScrnPixmap 
              ( v := Window 
              , clip := WCursor . SavedRect 
              , src := WCursor . SavedPixmap 
              , delta := Rect . NorthWest ( WCursor . SavedRect ) 
              ) 
          END (* IF *) 

        | CursorStateTyp . CsBlinkingOn 
        , CursorStateTyp . CsSolid 
        => VBT . PolyTint 
            ( Window , WCursor . TranslatedSerifRects , Ui . GDerivedInfoRef ^ . DiPaintOpFg ) 
        ; VBT . PaintTint 
            ( Window , WCursor . TranslatedVertRect , Ui . GDerivedInfoRef ^ . DiPaintOpFg ) 

        | CursorStateTyp . CsGrayed 
        => VBT . PolyTint 
            ( Window , WCursor . TranslatedSerifRects , Ui . GDerivedInfoRef ^ . DiPaintOpFg ) 
        ; VBT . PaintTexture 
            ( Window 
            , WCursor . TranslatedVertRect 
            , Ui . GDerivedInfoRef ^ . DiPaintOpBgFg 
            , Pixmap . Gray 
            ) 
        END (* CASE *) 
      END (* WITH *)  
    END PaintCursor 

; PROCEDURE PaintOutCursor ( Window : WindowTyp ) 
  <* LL . sup < Window . mu *> 

  = BEGIN (* PaintOutCursor *) 
      CASE Window . EwCursor . State 
      OF CursorStateTyp . CsSolid 
      , CursorStateTyp . CsGrayed 
      , CursorStateTyp . CsBlinkingOn 
      => PaintCursor ( Window , CursorStateTyp . CsInvisible ) 
      ELSE 
      END (* CASE *) 
    END PaintOutCursor 

; PROCEDURE PaintInCursor ( Window : WindowTyp ) 
  <* LL . sup < Window . mu *> 

  = <* UNUSED *> VAR LMissedRegion : Region . T 

  ; BEGIN (* PaintInCursor *) 
      CASE Window . EwCursor . State 
      OF CursorStateTyp . CsSolid 
      , CursorStateTyp . CsGrayed 
      , CursorStateTyp . CsBlinkingOn 
      => 
(* VBT . Capture does not return, apparently not getting notification
   events (GraphicsExpose and NoExpose) from X that XCopyArea has 
   completed.  
  Assertions . MessageText ( "Capturing" ) 
;
         Window . EwCursor . SavedPixmap 
          := VBT . Capture 
               ( Window 
               , Window . EwCursor . SavedRect 
               , (* VAR *) LMissedRegion  
               )  
; Assertions . MessageText ( "Done capturing" ) 
      ; IF NOT Region . IsEmpty ( LMissedRegion ) 
        THEN Window . EwCursor . SavedPixmap := NIL 
        END (* IF *)
; *)   
        PaintCursor ( Window , Window . EwCursor . State ) 
      ELSE 
      END (* CASE *) 
    END PaintInCursor 

(* EXPORTED *) 
; PROCEDURE MakeCursorVisible ( Window : WindowTyp ) 
  <* LL . sup < BlinkerLock *> 

  = BEGIN (* MakeCursorVisible *) 
      LOCK BlinkerLock 
      DO IF WindowWithFocus = Window 
         THEN 
           SetCursorState ( Window , CursorStateTyp . CsBlinking ) 
         ELSE 
           SetCursorState ( Window , CursorStateTyp . CsSolid ) 
         END (* IF *) 
      END (* LOCK *) 
    END MakeCursorVisible 

(* EXPORTED *) 
; PROCEDURE PaintWindowBlank ( Window : WindowTyp ) 
  <* LL . sup < Window . mu *> 

  = VAR LOpBorder : PaintOp . T 
  ; VAR LOpBg : PaintOp . T 
  ; VAR LCharsRect : Rect . T 

  ; BEGIN (* PaintWindowBlank *)
      LOCK Window
      DO LOpBorder := Ui . GDerivedInfoRef ^ . DiPaintOpBorder
      ; LCharsRect := Window . EwCharsRect
      ; LOpBg := Ui . GDerivedInfoRef ^ . DiPaintOpBg
      END (* LOCK *) 
    ; VBT . PaintTint ( Window , Rect . Full , LOpBorder ) 
    ; VBT . PaintTint ( Window , LCharsRect , LOpBg ) 
    END PaintWindowBlank 

(* EXPORTED *) 
; PROCEDURE PaintLine  
    ( Window : WindowTyp 
    ; FromPosInWindow : CharCoordTyp 
    ; LineNoInWindow : CharCoordTyp 
    ; PaintText : TEXT (* May be NIL *) 
    ; FromSsInString : Strings . StringSsTyp 
    ; ToSsInString : Strings . StringSsTyp 
    ; Attr : PaintHs . TextAttrTyp  
    ) 
  <* LL . sup < Window . mu *> 

  = VAR PtSubstringLength : Strings . StringSsTyp 
  ; VAR PtBoundingBox : Rect . T 
  ; VAR PtPixelOffset : Point . T 

  ; PROCEDURE PtDecorationLine 
      ( From , Thru : Point . T 
      ; Op := PaintOp . BgFg 
      ; Width : CARDINAL := 0 
      ) 

    = VAR LPixelOffset : Point . T 

    ; BEGIN
        LPixelOffset := PtPixelOffset 
      ; FOR RI := 0 TO PtSubstringLength - 1 
        DO 
          VBT . Line 
            ( v := Window 
            , clip := Window . EwCharsRect (* PtBoundingBox *) 
            , p := Point . Add ( From , LPixelOffset ) 
            , q := Point . Add ( Thru , LPixelOffset ) 
            , width := Width 
            , op := Op 
            ) 
        ; INC ( LPixelOffset . h , Window . EwCharSeparation . h ) 
        END (* FOR *) 
      END PtDecorationLine 

  ; BEGIN (* PaintLine *) 
      VAR LCharPoint : CharPointTyp
    ; VAR LOp : PaintOp . T
    ; VAR LScrnOp : ScrnPaintOp . T 

    ; BEGIN (* Block for PaintLine *) 
        IF PaintText = NIL 
        THEN PaintText := ""
        END (* IF *) 
      ; IF FromPosInWindow < 0 
        THEN 
          DEC ( FromSsInString , FromPosInWindow ) 
        ; FromPosInWindow := 0 
        END
      ; LCharPoint := CharPointTyp { FromPosInWindow , LineNoInWindow } 
      ; PtPixelOffset := PixelOffset ( Window , LCharPoint ) 
      ; ToSsInString 
          := MIN ( ToSsInString 
                 , FromSsInString 
                   - FromPosInWindow 
                   + Window . EwSouthEastChar . h
                 ) 
      ; IF FromSsInString < ToSsInString 
        THEN 
          PtSubstringLength := ToSsInString - FromSsInString 
        ; IF FromSsInString < Text . Length ( PaintText )  
          THEN 
            PaintOutCursor ( Window ) 
          ; PtBoundingBox 
              := Rect . Add 
                   ( Window . EwOriginCellBoundingBox 
                   , PtPixelOffset 
                   ) 
          ; INC ( PtBoundingBox . east 
                , Window . EwCharSeparation . h 
                  * ( PtSubstringLength - 1 ) 
                ) 

          (* Paint background. *) 
          ; VBT . PaintTint 
              ( v := Window 
              , clip := PtBoundingBox 
              , op := Ui . GDerivedInfoRef ^ . DiPaintOpsBg [ Attr . TaBgColor ] 
              ) 

          (* Paint decoration, under the characters. *) 
          ; CASE Attr . TaDecoration 
            OF PaintHs . TaDecPlain 
            => (* Nothing. *)    
            | PaintHs . TaDecStrikeout
            => PtDecorationLine 
                 ( Window . EwStrikeoutFrom 
                 , Window . EwStrikeoutThru 
                 , Ui . GDerivedInfoRef ^ . DiPaintOpsDec [ Attr . TaDecoration ] 
                 , Width := 2 
                 ) 
            | PaintHs . TaDecUnderline1   
            , PaintHs . TaDecUnderline2   
            => PtDecorationLine 
                 ( Window . EwUnderlineFrom 
                 , Window . EwUnderlineThru 
                 , Ui . GDerivedInfoRef ^ . DiPaintOpsDec [ Attr . TaDecoration ] 
                 , Width := 2 
                 ) 
            | PaintHs . TaDecCaret   
            => WITH WPaintOp = Ui . GDerivedInfoRef ^ . DiPaintOpsDec [ Attr . TaDecoration ] 
               DO 
                 PtDecorationLine 
                   ( Window . EwCaretFrom 
                   , Window . EwCaretMiddle 
                   , WPaintOp
                   , Width := 2 
                   ) 
              ; PtDecorationLine 
                   ( Window . EwCaretMiddle 
                   , Window . EwCaretThru 
                   , WPaintOp
                   , Width := 2 
                   ) 
              END (* WITH *) 
            ELSE (* No decoration to add. *) 
            END (* CASE *) 

          (* Paint the characters.*)

          
       (* ; LOp := Ui . GDerivedInfoRef ^ . DiPaintOpsChar [ PaintHs . TaFgColorPlain ] experimental *)
          ; LOp := Ui . GDerivedInfoRef ^ . DiPaintOpsChar [ Attr . TaFgColor ] 

; LScrnOp := Palette . ResolveOp ( Window . st , LOp )
; Assertions . AssertText
    ( LScrnOp # NIL , "NIL screen op for " & Fmt . Int ( LOp . op ) ) 
; Assertions . AssertText
    ( LScrnOp # PlttFrnds . noOp , "screen noOp for " & Fmt . Int ( LOp . op ) )

          ; VBT . PaintText 
              ( v := Window  
              , clip := PtBoundingBox    
              , pt := CharToPixel ( Window , LCharPoint ) 
           (* , fnt := Window . EwFonts [ PaintHs . TaFontBold ] experimental. *) 
              , fnt := Window . EwFonts [ Attr . TaFont ] 
              , t := Text . Sub 
                       ( PaintText , FromSsInString , PtSubstringLength )
              , op := LOp 
              ) 
          ; PaintInCursor ( Window ) 
          END (* IF *) 
        END (* IF *) 
      END (* Block *) 
    END PaintLine  

(* EXPORTED *) 
; PROCEDURE ClearLine 
    ( Window : WindowTyp 
    ; FromPosInWindow : CharCoordTyp 
    ; LineNoInWindow : CharCoordTyp 
    ) 
  <* LL . sup < Window . mu *> 

  = VAR LBoundingBox : Rect . T 

  ; BEGIN (* ClearLine *) 
      PaintOutCursor ( Window ) 
    ; LBoundingBox 
        := Rect . Add 
             ( Window . EwOriginCellBoundingBox 
             , PixelOffset 
                 ( Window , CharPointTyp { FromPosInWindow , LineNoInWindow } )
             ) 
    ; INC ( LBoundingBox . east 
          , Window . EwCharSeparation . h 
            * ( Window . EwSouthEastChar . h - 1 - FromPosInWindow ) 
          ) 
    ; VBT . PaintTint 
        ( v := Window 
        , clip := LBoundingBox 
        , op := Ui . GDerivedInfoRef ^ . DiPaintOpBg 
        ) 
    ; PaintInCursor ( Window ) 
    END ClearLine 

; PROCEDURE PaintArrow   
    ( Window : WindowTyp 
    ; LineNoInWindow : CharCoordTyp 
    ; ArrowPath : Path . T 
    ; DeltaH : INTEGER 
    ; IsVisible : BOOLEAN 
    ) 
  <* LL . sup < Window . mu *> 

  = VAR LDelta : Point . T 
  ; VAR LBoundingBox : Rect . T 

  ; <* FATAL Path . Malformed *> 
    BEGIN 
      LDelta 
        := Point . T 
             { DeltaH 
             , Window . EwDomain . north 
               + Window . EwMargin . v  
               + LineNoInWindow * Window . EwCharSeparation . v 
             } 
    ; IF IsVisible 
      THEN 
        VBT . Fill 
          ( v := Window 
          , clip := Window . EwDomain 
          , path := Path . Translate ( ArrowPath , LDelta )  
          , op := PaintOpFromColor ( Options . FgColorPlain ) 
          )   
      ELSE 
        LBoundingBox 
          := Rect . Add ( Window . EwArrowBoundingBox , LDelta ) 
      ; VBT . PaintTint 
          ( v := Window 
          , clip := LBoundingBox 
          , op := Ui . GDerivedInfoRef ^ . DiPaintOpBorder
          ) 
      END (* IF *) 
    END PaintArrow 

(* EXPORTED *) 
; PROCEDURE PaintLeftArrow
    ( Window : WindowTyp 
    ; LineNoInWindow : CharCoordTyp 
    ; IsVisible : BOOLEAN 
    ) 

  <* LL . sup < Window . mu *> 

  = BEGIN 
      PaintArrow 
        ( Window 
        , LineNoInWindow 
        , ArrowPath := Window . EwPathLeftArrow 
        , DeltaH := Window . EwDomain . west  
        , IsVisible := IsVisible 
        ) 
    END PaintLeftArrow

(* EXPORTED *) 
; PROCEDURE PaintRightArrow 
    ( Window : WindowTyp 
    ; LineNoInWindow : CharCoordTyp 
    ; IsVisible : BOOLEAN 
    ) 

  <* LL . sup < Window . mu *> 

  = BEGIN 
      PaintArrow 
        ( Window 
        , LineNoInWindow 
        , ArrowPath := Window . EwPathRightArrow 
        , DeltaH := Window . EwCharsRect. east  
        , IsVisible := IsVisible 
        ) 
    END PaintRightArrow 

(* EXPORTED *) 
; PROCEDURE PaintBackground 
    ( Window : WindowTyp 
    ; LineNoInWindow : CharCoordTyp 
    ; FromPosInWindow : CharCoordTyp 
    ; Len : CharCoordTyp 
    ; BgColor : PaintHs . TextAttrComponentTyp 
    ) 
  <* LL . sup < Window . mu *> 

  = VAR LBoundingBox : Rect . T 

  ; BEGIN (* PaintBackground *) 
      PaintOutCursor ( Window ) 
    ; LBoundingBox 
        := Rect . Add 
             ( Window . EwOriginCellBoundingBox 
             , PixelOffset 
                 ( Window , CharPointTyp { FromPosInWindow , LineNoInWindow } )
             ) 
    ; INC ( LBoundingBox . east 
          , Window . EwCharSeparation . h * ( Len - 1 ) 
          ) 
    ; VBT . PaintTint 
        ( v := Window 
        , clip := LBoundingBox 
        , op := Ui . GDerivedInfoRef ^ . DiPaintOpsBg [ BgColor ] 
        ) 
    ; PaintInCursor ( Window ) 
    END PaintBackground  

(* EXPORTED *) 
; PROCEDURE SetCursorPosition 
    ( Window : WindowTyp 
    ; CharPosInWindow : CharCoordTyp 
    ; LineNoInWindow : CharCoordTyp 
    ) 
  <* LL . sup < Window . mu *> 

  = VAR LCharPoint : CharPointTyp 
  ; VAR LPixelOffset : Point . T 

  ; BEGIN (* SetCursorPosition *) 
      LCharPoint 
        := CharPointTyp 
             { h := MIN 
                      ( Window . EwSouthEastChar . h 
                          (* Can be right of rightmost char. *) 
                      , MAX ( 0 , CharPosInWindow ) 
                      ) 
             , v := MIN 
                      ( Window . EwSouthEastChar . v - 1 
                      , MAX ( 0 , LineNoInWindow ) 
                      ) 
             } 
    ; IF Window . EwCursor . CharPoint # LCharPoint 
      THEN 
        PaintOutCursor ( Window ) 
      ; Window . EwCursor . CharPoint := LCharPoint 
      ; LPixelOffset 
          := PixelOffset ( Window , Window . EwCursor . CharPoint ) 
      ; FOR RI := FIRST ( Window . EwCursor . SerifRects ) 
              TO LAST ( Window . EwCursor . SerifRects ) 
        DO Window . EwCursor . TranslatedSerifRects [ RI ] 
             := Rect . Add 
                  ( Window . EwCursor . SerifRects [ RI ] 
                  , LPixelOffset  
                  ) 
        END (* FOR *) 
      ; Window . EwCursor . TranslatedVertRect 
          := Rect . Add 
               ( Window . EwCursor . VertRect 
               , LPixelOffset   
               ) 
      ; Window . EwCursor . SavedRect 
          := Rect . Add 
               ( Window . EwCursor . OriginSaveRect 
               , LPixelOffset   
               ) 
      ; PaintInCursor ( Window ) 
      END (* IF *) 
    END SetCursorPosition 

(* EXPORTED *) 
; PROCEDURE PaintCursorCoordinates ( Window : WindowTyp ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN (* PaintCursorCoordinates *) 
      FormsVBT . PutText
        ( Options . MainForm 
        , "Fv_CursorCoords" 
        , Fmt . Pad 
            ( Fmt . Int 
                ( Window . WrVertScroll 
                  + Window . EwCursor . CharPoint . v 
                  + 1 
                ) 
            , 6 
            , align := Fmt . Align . Left 
            ) 
          & "," 
          & Fmt . Pad 
               ( Fmt . Int 
                  ( Window . WrHorizScroll 
                    + Window . EwCursor . CharPoint . h 
                    + 1 
                  ) 
               , 3 
               ) 
        )
    END PaintCursorCoordinates 

(* EXPORTED *) 
; PROCEDURE PaintInsertMode ( Window : WindowTyp ) 

  = VAR LValue : TEXT 

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      IF Window # NIL 
      THEN 
        IF Window . WrInsertMode 
        THEN LValue := "Insert" 
        ELSE LValue := "Ovrwrt"
        END (* IF *) 
      ; FormsVBT . PutText
          ( Options . MainForm , "Fv_InsertMode" , LValue )
      END (* IF *) 
    END PaintInsertMode 

; PROCEDURE RedGreenBoolean ( Value : BOOLEAN ) : Color . T 

  = BEGIN
      IF Value
      THEN RETURN Color . Green 
      ELSE RETURN Color . Red 
      END (* IF *) 
    END RedGreenBoolean

(* EXPORTED *) 
; PROCEDURE UpdateVertScroller ( Window : WindowTyp ) 
  <* LL.sup < Window *> 

  = VAR LLineCt : LbeStd . LineNoTyp 
  ; VAR LLineCtIsExact : BOOLEAN  

  ; <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      IF Window # NIL 
      THEN 
        IF Window . WrImageRef = NIL 
        THEN LLineCt := MAX ( 0 , Window . EwSouthEastChar . v ) 
        ELSE 
          LLineCt := Window . WrImageRef . ItPers . IpLineCtDisplay  
        END (*IF *) 
      (* Set properties of a scroller in the order:
         Min, Max, Thumb, Value.  Min is left at its default of zero. *) 
      ; FormsVBT . PutIntegerProperty 
          ( Options . MainForm 
          , "Fv_VertScroller" 
          , "Max" 
          , MAX ( LLineCt + Window . EwSouthEastChar . v - 2 
                , Window . EwSouthEastChar . v  
                ) 
            (* Allow for full window minus one line of blank lines. *)   
          )
      ; FormsVBT . PutIntegerProperty 
          ( Options . MainForm 
          , "Fv_VertScroller" 
          , "Thumb" 
          , MAX ( 0 , Window . EwSouthEastChar . v - 1 )  
          )
      ; FormsVBT . PutInteger (* Value *) 
          ( Options . MainForm 
          , "Fv_VertScroller" 
          , Window . WrVertScroll 
          )
      ; FormsVBT . PutColorProperty 
         ( Options . MainForm 
         , "Fv_EstCursor" 
         , "Color" 
         , RedGreenBoolean ( Window . WrVertScrollIsExact ) 
         ) 
      ; IF Window . WrImageRef = NIL 
        THEN 
          LLineCtIsExact := TRUE 
        ELSE 
          LLineCtIsExact := Window . WrImageRef . ItPers . IpLineCtIsExact
        END (* IF *) 
      ; FormsVBT . PutColorProperty 
         ( Options . MainForm 
         , "Fv_EstLineCt" 
         , "Color" 
         , RedGreenBoolean ( LLineCtIsExact ) 
         ) 
      END (* IF *) 
    END UpdateVertScroller 

(* EXPORTED *) 
; PROCEDURE UpdateHorizScroller ( Window : WindowTyp ) 
  <* LL.sup < Window *> 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      IF Window # NIL 
      THEN 
        FormsVBT . PutIntegerProperty 
          ( Options . MainForm 
          , "Fv_HorizScroller" 
          , "Max" 
          , LbeStd . LimitedCharNoMax 
(* TODO: Figure out a better way to set Max of horiz. scroller. *) 
          )
      ; FormsVBT . PutIntegerProperty 
          ( Options . MainForm 
          , "Fv_HorizScroller" 
          , "Thumb" 
          , MAX ( 0 , Window . EwSouthEastChar . h - 1 )  
          )
      ; FormsVBT . PutInteger (* Value *) 
          ( Options . MainForm 
          , "Fv_HorizScroller" 
          , Window . WrHorizScroll 
          )
      END (* IF *) 
    END UpdateHorizScroller 

(* There is a single blinking mechanism for all windows of all images. 
   These global variables control it.  This ensures blinking stays in 
   phase as the focus moves, repaints happen, etc. *) 

; VAR BlinkerLock : MUTEX := NEW ( MUTEX ) 
  (* In the lock order, VBT.mu < BlinkerLock < Window . mu, 
     forall Window : WindowTyp. *) 

(* BlinkerLock protects the following variables: *) 
; VAR BlinkerThread : Thread . T := NIL 
; VAR BlinkerState : CursorStateTyp := CursorStateTyp . CsInvisible 
; VAR WindowWithFocus : WindowTyp := NIL 
; VAR IdleBlinkCt : INTEGER := 0 
(* End of variables protected by BlinkerLock. *) 

; CONST IdleLifetime = 20 (* Half cycles *) 
; CONST HalfCycleDuration = 0.5D0 (* Seconds *) 

; TYPE BlinkerClosureTyp 
    = Thread . Closure OBJECT OVERRIDES apply := Blink END (* OBJECT *) 

; PROCEDURE Blink ( <* UNUSED *> Arg : BlinkerClosureTyp ) : REFANY 
  <* LL . sup < VBT . mu*> 

  = BEGIN (* Blink *) 
      LOOP 
        Thread . Pause ( HalfCycleDuration ) 
      ; LOCK VBT . mu 
(* REVIEW ^ Why is this needed? *) 
        DO LOCK BlinkerLock 
          DO IF WindowWithFocus = NIL 
             THEN 
               INC ( IdleBlinkCt ) 
             ELSE 
               PaintOutCursor ( WindowWithFocus ) 
             ; CASE WindowWithFocus . EwCursor . State 
               OF CursorStateTyp . CsBlinkingOff 
               => WindowWithFocus . EwCursor . State 
                    := CursorStateTyp . CsBlinkingOn 
               ; PaintCursor 
                   ( WindowWithFocus , WindowWithFocus . EwCursor . State ) 
               ; IdleBlinkCt := 0 
               | CursorStateTyp . CsBlinkingOn 
               => WindowWithFocus . EwCursor . State 
                    := CursorStateTyp . CsBlinkingOff 
               ; PaintCursor 
                   ( WindowWithFocus , WindowWithFocus . EwCursor . State ) 
               ; IdleBlinkCt := 0 
               ELSE (* Leave it alone. *) 
                 INC ( IdleBlinkCt ) 
               END (* CASE *) 
             END (* IF *) 
          ; IF IdleBlinkCt >= IdleLifetime 
            THEN (* Commit suicide. *) 
              BlinkerThread := NIL 
            ; BlinkerState := CursorStateTyp . CsInvisible 
            ; IdleBlinkCt := 0 
            ; RETURN NIL 
            END (* IF *) 
          END (* LOCK *) 
        END (* LOCK *) 
      END (* LOOP *) 
    END Blink 

; PROCEDURE SetCursorState 
    ( Window : WindowTyp ; State : CursorSetStateTyp ) 
  <* LL >= { BlinkerLock } *>
  <* LL . sup < Window . mu *> 

  = BEGIN (* SetCursorState *) 
      IF State # Window . EwCursor . State 
      THEN 
        PaintOutCursor ( Window ) 
      ; CASE State 
        OF CursorStateTyp . CsSolid 
        , CursorStateTyp . CsGrayed 
        , CursorStateTyp . CsInvisible 
        => Window . EwCursor . State := State 
        ; PaintCursor ( Window , State ) 
        | CursorStateTyp . CsBlinking 
        => IF BlinkerThread = NIL 
           THEN 
             BlinkerThread := Thread . Fork ( NEW ( BlinkerClosureTyp ) ) 
           ; BlinkerState := CursorStateTyp . CsBlinkingOn 
           ; IdleBlinkCt := 0 
           END (* IF *) 
        ; Window . EwCursor . State := BlinkerState 
        ; PaintCursor ( Window , BlinkerState ) 
        END (* CASE *) 
      END (* IF *) 
    END SetCursorState 

(* EXPORTED *) 
; PROCEDURE PaintText 
    ( <*UNUSED*> Window : WindowTyp ; VbtName : TEXT ; Value : TEXT ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      FormsVBT . PutText ( Options . MainForm , VbtName , Value ) 
    END PaintText  

(* EXPORTED *) 
; PROCEDURE PaintWindowState 
    ( Window : WindowTyp ; VbtName : TEXT ; State : INTEGER ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      FormsVBT . PutInteger 
        ( Form ( Window )  , VbtName , ORD ( State ) ) 
    END PaintWindowState 

(* EXPORTED *) 
; PROCEDURE PaintWindowSavedState ( Window : WindowTyp ; State : BOOLEAN ) 

  = <* FATAL FormsVBT . Error *>
    <* FATAL FormsVBT . Unimplemented *>
    BEGIN 
      FormsVBT . PutInteger 
        ( Form ( Window ) , "Fv_Modified" , ORD ( NOT State ) ) 
    END PaintWindowSavedState 

(* EXPORTED *) 
; PROCEDURE Beep ( ) 

  = BEGIN (* Beep *) 
      TRY 
        Wr . PutChar ( Stdio . stderr , LbeStd . CharBell ) 
      ; Wr . PutText ( Stdio . stderr , "Beep" & Wr . EOL ) 
      EXCEPT 
      | Thread . Alerted => (* Ignore. *) 
      | Wr . Failure => (* Ignore. *) 
      END (* TRY EXCEPT *) 
      
    END Beep 

(* VBT callbacks of EditWindow . T *) 

; VAR MaxWindowSize := 1000000000 (* Very large *) 
; VAR PreferredCharSize := Point . T { 80 , 24 } 

; PROCEDURE TakeKBFocusLocked 
    ( Window : WindowTyp ; TimeStamp : VBT . TimeStamp ) 
  <* LL >= { BlinkerLock } *>
  <* LL . sup < Window . mu *> 

  = BEGIN (* TakeKBFocusLocked *) 
      IF Window # WindowWithFocus 
      THEN 
        TRY 
          VBT . Acquire ( Window , VBT . KBFocus , TimeStamp ) 
        ; WindowWithFocus := Window 
        ; SetCursorState ( Window , CursorStateTyp . CsBlinking ) 
        EXCEPT VBT . Error ( ECode )    
        => EVAL ECode (* For breakpoint. *)  
        END (* EXCEPT *) 
      END (* IF *) 
    END TakeKBFocusLocked 

(* EXPORTED *) 
; PROCEDURE TakeKBFocus 
   ( Window : WindowTyp ; TimeStamp : VBT . TimeStamp ) 
  <* LL . sup < Window . mu *> 

  = BEGIN (* TakeKBFocus *) 
      LOCK BlinkerLock 
      DO TakeKBFocusLocked ( Window , TimeStamp ) 
      END (* LOCK *) 
    END TakeKBFocus 

(* EXPORTED *) 
; PROCEDURE ScreenSelection ( ) : VBT . Selection 

  = BEGIN 
(* TODO:  Figure out when VBT . Source, etc. are initialized and cache this
          in a variable. 
*) 
      RETURN VBT . Source  
    END ScreenSelection 

; PROCEDURE NearestCharPoint 
    ( Window : WindowTyp ; READONLY PixelPoint : Point . T ) : Point . T 

  = VAR LNorthWest := Rect . NorthWest ( Window . EwOriginCharBoundingBox ) 

  ; BEGIN (* NearestCharPoint *) 
      RETURN 
        Point . T 
          { ( PixelPoint . h - LNorthWest . h + Window . EwCharSize . h 
              DIV 2 
            ) 
            DIV Window . EwCharSeparation . h 
          , ( PixelPoint . v - LNorthWest . v + Window . EwVertGap DIV 2 ) 
            DIV ( Window . EwCharSeparation . v ) 
          } 
    END NearestCharPoint 

; TYPE WorkerClosurePointTyp
    = Worker . ClosureTyp OBJECT PointParam : Point . T END 

; PROCEDURE MouseClickWorkProc ( Closure : WorkerClosurePointTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. Closure . PointParam is char point. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      IF Rect . Member 
           ( Closure . PointParam 
           , Rect . FromCorners 
               ( Point . Origin , Closure . Window . EwSouthEastChar ) 
           ) 
      THEN 
        LCommandString 
          := UiRecPlay . BeginCommandPlusInt2 
               ( UiRecPlay . CommandTyp . MouseClickChar 
               , Closure . PointParam . h 
               , Closure . PointParam . v 
               ) 
      ; Display . MoveCursorAbsoluteInsideWindow 
          ( Closure . Window , Closure . PointParam ) 
      ; Selection . Preselect ( Closure . Window ) 
      ; UiRecPlay . RecordString ( LCommandString ) 
      END (* IF *) 
    END MouseClickWorkProc 

(* EXPORTED *) 
; PROCEDURE ReplayMouseClickPixel 
    ( Window : WindowTyp ; H : INTEGER ; V : INTEGER ) 

  = VAR LPoint : Point . T 
  ; VAR LCharPoint : CharPointTyp 

  ; BEGIN 
      LPoint := Point . T { H , V } 
    ; LCharPoint := NearestCharPoint ( Window , LPoint ) 
    ; TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosurePointTyp 
                     , Window := Window 
                     , PointParam := LCharPoint 
                     , apply := MouseClickWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayMouseClickPixel  

(* EXPORTED *) 
; PROCEDURE ReplayMouseClickChar 
    ( Window : WindowTyp ; H : INTEGER ; V : INTEGER ) 

  = BEGIN 
(* REVIEW: This does not acquire the focus.  It is difficult to get the
           event time, which is needed to do so.  It is also not clear
           we want to take the focus anyway. If things are the same as
           when recorded, getting the focus then would have succeeded,
           or we would not have recorded this.  Same considerations for
           ReplayMouseClickPixel. 
*) 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosurePointTyp 
                     , Window := Window 
                     , PointParam := Point . T { H , V }  
                     , apply := MouseClickWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayMouseClickChar

; VAR GSweeping : BOOLEAN := FALSE   
; VAR GOwnScreenSelection := FALSE 
; VAR GExtraneousLostCodesExpected : INTEGER := 0 

; PROCEDURE MouseClick 
    ( Window : WindowTyp 
    ; Time : VBT . TimeStamp 
    ; CursorPosition : VBT . CursorPosition 
    )
  <* LL . sup < Window . mu *> 

  = VAR LCharPoint : CharPointTyp 
  ; VAR LGotFocus : BOOLEAN := FALSE 

  ; BEGIN (* MouseClick *) 
      LCharPoint := NearestCharPoint ( Window , CursorPosition . pt ) 
    ; LOCK BlinkerLock 
      DO TakeKBFocusLocked ( Window , Time ) 
      ; LGotFocus := Window = WindowWithFocus 
      END (* LOCK *) 
    ; IF LGotFocus 
      THEN 
        (* VBT . Release ( Window , ScreenSelection ( ) ) *) 
        (* This does:  
           1.  If this window already owns the selection, it will send a
               Lost Misc. code, which in turn will clear the old selection.
           2.  If not, no Misc code is sent, so the old selection will
               stay visible, either in another Schutz window or a different
               application, until later when the sweep is finished.
           3.  It ensures that this window does not currently own the
               selection, so when the sweep is done and we Acquire it,
               a Lost code may be sent to some other window, but not to
               this one, which would wrongly clear this new selection.
        *)   
        EVAL Worker . RequestWorkInteractive 
               ( NEW ( WorkerClosurePointTyp 
                     , Window := Window 
                     , PointParam := LCharPoint  
                     , apply := MouseClickWorkProc 
                     ) 
               ) 
      ; VBT . SetCage 
          ( Window 
          , VBT . Cage 
              { rect 
                  := Rect . Add 
                       ( Window . EwOriginCellBoundingBox 
                       , PixelOffset ( Window , LCharPoint ) 
                       ) 
              , inOut := VBT . InOut { FALSE } 
              , screen := CursorPosition . screen 
              } 
          ) 
      END (* IF *) 
    END MouseClick 

; PROCEDURE ClearSelectionWorkProc 
    ( <* UNUSED *> Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . ClearSelection )
    ; Selection . ClearSelection ( )
    ; IF Selection . Current # NIL 
      THEN 
        Selection . Current . SelText := NIL   
      END (* IF *) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END ClearSelectionWorkProc 

(* EXPORTED *) 
; PROCEDURE ReplayClearSelection ( ) 

  = BEGIN 
      IF NOT GSweeping 
      THEN 
        TRY 
          EVAL Worker . RequestWork 
                 ( NEW ( Worker . ClosureTyp  
                       , Window := NIL (* Irrelevant. *)  
                       , apply := ClearSelectionWorkProc 
                       ) 
                 ) 
        EXCEPT Thread . Alerted => 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END ReplayClearSelection 

; PROCEDURE ClearSelection ( ) 

  = BEGIN 
      TRY 
        EVAL Worker . AwaitIdle ( ) 
        (* ^Be sure not to lose previous sweeping.  Mouse events could 
            come fast. 
        *)
      ; EVAL Worker . RequestWorkInteractive 
               ( NEW ( Worker . ClosureTyp 
                     , Window := NIL (* Irrelevant. *)  
                     , apply := ClearSelectionWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted 
      => (* Ignore.  Probably this thread can't be alerted, but be thorough.*) 
      END (* TRY EXCEPT *) 
    END ClearSelection 

; VAR GEndSweepMiscCode : VBT . MiscCodeType 

; PROCEDURE EndSweepSelection ( Window : WindowTyp ; Time : VBT . TimeStamp ) 
  <* LL . sup < Window . mu *> 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand 
             ( UiRecPlay . CommandTyp . EndSweepSelection ) 
    ; TRY 
        INC ( GExtraneousLostCodesExpected , ORD ( GOwnScreenSelection ) ) 
      ; VBT . Acquire ( Window , ScreenSelection ( ) , Time ) 
      ; GOwnScreenSelection := TRUE 
      ; UiRecPlay . RecordString ( LCommandString ) 
      EXCEPT VBT . Error 
      => DEC ( GExtraneousLostCodesExpected , ORD ( GOwnScreenSelection ) ) 
      END (* TRY EXCEPT *) 
    ; GSweeping := FALSE 
    END EndSweepSelection 

(* EXPORTED *) 
; PROCEDURE ReplayEndSweepSelection ( Window : WindowTyp ) 
  <* LL . sup < Window . mu *> 

  = <* FATAL VBT . Error *> 
    BEGIN 
      (* This is awkward.  We need to acquire the screen selection. 
         That requires an event time, which requires forging, which
         only works by sending a misc code, so we have to do the 
         real work in the misc method.  It will call EndSweepSelection. 
      *)  
      VBT . Forge ( Window , GEndSweepMiscCode )
    END ReplayEndSweepSelection 

; PROCEDURE Mouse 
    ( Window : WindowTyp ; READONLY MouseRec : VBT . MouseRec ) 
  <* LL . sup <= VBT . mu *> 
  (* This is the callback for the VBT mouse method, which is called with 
     LL . sup = VBT . mu
  *) 

  = BEGIN (* Mouse *) 
      TRY 
        IF MouseRec . whatChanged = VBT . Modifier . MouseL 
        THEN 
          IF MouseRec . clickType = VBT . ClickType . FirstDown 
             AND NOT MouseRec . cp . gone 
             AND NOT MouseRec . cp . offScreen 
             AND Rect . Member 
                   ( MouseRec . cp . pt , VBT . Domain ( Window ) ) 
          THEN  
            MouseClick ( Window , MouseRec . time , MouseRec . cp ) 
          ELSIF MouseRec . clickType = VBT . ClickType . LastUp  
          THEN 
            EVAL Worker . AwaitIdle ( ) 
          ; IF Selection . Current # NIL 
               AND Selection . Current . SelEndMark # NIL 
  (* CHECK: ^This seems like a questionable way to get this info. *) 
            THEN 
              EndSweepSelection ( Window , MouseRec . time ) 
            END (* IF *) 
          END (* IF *) 
        ELSIF MouseRec . whatChanged = VBT . Modifier . MouseM 
        THEN 
          IF MouseRec . clickType = VBT . ClickType . FirstDown 
             AND NOT MouseRec . cp . gone 
             AND NOT MouseRec . cp . offScreen 
             AND Rect . Member 
                   ( MouseRec . cp . pt , VBT . Domain ( Window ) ) 
          THEN  
            Ui . Paste ( Window , MouseRec . time ) 
          END (* IF *) 
        ELSIF MouseRec . whatChanged = VBT . Modifier . MouseR  
        THEN
          IF MouseRec . clickType = VBT . ClickType . OtherDown 
          THEN 
            ClearSelection ( ) 
          ; VBT . SetCage ( Window , VBT . EverywhereCage ) 
          END (* IF *) 
        END (* IF *) 
      EXCEPT Thread . Alerted 
      => (* Ignore.  Probably this thread can't be alerted, but be thorough.*) 
      END (* TRY EXCEPT *) 
    END Mouse 

; PROCEDURE SweepSelectionWorkProc ( Closure : WorkerClosurePointTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. Closure . PointParam is char point. *) 
  (* On Worker thread. *) 

  = BEGIN 
      IF Rect . Member 
           ( Closure . PointParam 
           , Rect . FromCorners 
               ( Point . Origin , Closure . Window . EwSouthEastChar ) 
           ) 
      THEN 
        Selection . DragSelection ( Closure . Window , Closure . PointParam ) 
      END (* IF *) 
    END SweepSelectionWorkProc 

(* EXPORTED *) 
; PROCEDURE ReplaySweepSelection 
    ( Window : WindowTyp ; H : INTEGER ; V : INTEGER ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosurePointTyp  
                     , Window := Window   
                     , PointParam := Point . T { H , V }  
                     , apply := SweepSelectionWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplaySweepSelection 

; PROCEDURE Position 
    ( Window : WindowTyp 
    ; READONLY PositionRec : VBT . PositionRec 
    ) 
  <* LL . sup < Window . mu *> 
  (* This is the callback for the VBT position method, which is called with 
     LL . sup = VBT . mu 
  *)  

  = VAR LCharPoint : CharPointTyp 

  ; BEGIN (* Position *) 
      TRY 
        IF VBT . Modifier . MouseL IN PositionRec . modifiers 
           AND NOT VBT . Modifier . MouseR IN PositionRec . modifiers 
           AND NOT PositionRec . cp . gone 
           AND NOT PositionRec . cp . offScreen  
           AND Rect . Member 
                 ( PositionRec . cp . pt , VBT . Domain ( Window ) ) 
        THEN 
          LCharPoint := NearestCharPoint ( Window , PositionRec . cp . pt ) 
        ; EVAL Worker . AwaitIdle ( ) 
          (* ^Be sure not to lose previous sweeping. *)
        ; EVAL Worker . RequestWorkInteractive 
                 ( NEW ( WorkerClosurePointTyp 
                       , Window := Window 
                       , PointParam := LCharPoint  
                       , apply := SweepSelectionWorkProc 
                       ) 
                 ) 
        ; VBT . SetCage 
            ( Window 
            , VBT . Cage 
                { rect 
                    := Rect . Add 
                         ( Window . EwOriginCellBoundingBox 
                         , PixelOffset ( Window , LCharPoint ) 
                         ) 
                , inOut := VBT . InOut { FALSE } 
                , screen := PositionRec . cp . screen 
                } 
            ) 
        ; GSweeping := TRUE 
        END (* IF *) 
      EXCEPT Thread . Alerted 
      => (* Ignore.  Probably this thread can't be alerted, but be thorough.*) 
      END (* TRY EXCEPT *) 
    END Position 

; TYPE WorkerClosureBoolTyp 
  = Worker . ClosureTyp OBJECT BoolParam : BOOLEAN END 

; PROCEDURE PromptAndCloseWorkProc ( Closure : WorkerClosureBoolTyp ) 
  RAISES { Backout , Thread . Alerted } 

  = BEGIN 
      Ui . PromptAndCloseAllImages 
        ( Closure , QuitAfter := Closure . BoolParam ) 
    END PromptAndCloseWorkProc

; PROCEDURE Misc ( Window : T ; READONLY MiscRec : VBT . MiscRec ) 
  <* LL . sup < Window . mu *> 
  (* This is the callback for the VBT misc. method, which is called with 
     LL . sup = VBT . mu
  *) 

  = BEGIN (* Misc *) 
      IF MiscRec . type = VBT . TakeSelection 
      THEN 
        IF MiscRec . selection = VBT . KBFocus 
        THEN 
          LOCK BlinkerLock 
          DO TakeKBFocusLocked ( Window , MiscRec . time ) 
          END (* LOCK *) 
        END (* IF *)
      ELSIF MiscRec . type = VBT . Lost 
      THEN 
  Assertions . MessageText 
    ( "Lost code: " & VBT . SelectionName ( MiscRec . selection ) ) 
; 
        IF MiscRec . selection = VBT . KBFocus 
        THEN 
          LOCK BlinkerLock 
          DO IF Window = WindowWithFocus 
             THEN 
               WindowWithFocus := NIL 
             END (* IF *) 
          ; SetCursorState ( Window , CursorStateTyp . CsSolid ) 
          END (* LOCK *) 
        ELSIF MiscRec . selection = ScreenSelection ( ) 
        THEN 
          IF GExtraneousLostCodesExpected > 0 
          THEN DEC ( GExtraneousLostCodesExpected ) 
          ELSE 
            GOwnScreenSelection := FALSE 
          ; ClearSelection ( ) 
          END (* IF *) 
        ELSIF MiscRec . selection = VBT . Target 
        THEN 
          Assertions . DoNothing ( )  
        END (* IF *) 
      ELSIF MiscRec . type = VBT . Deleted 
            OR MiscRec . type = VBT . Disconnected 
      THEN 
(* FIX: Figure out why we aren't getting this when Schutz is closed by
        the window manager. 
*) 
        TRY 
          EVAL Worker . RequestWork 
                 ( NEW ( WorkerClosureBoolTyp 
                       , BoolParam := FALSE  
                         (* ^We will presumably quit anyway. *) 
                       , apply := PromptAndCloseWorkProc 
                       ) 
                 , Interactive := FALSE 
                 , WaitToStart := TRUE 
                 , WaitToFinish := TRUE 
                 ) 
        EXCEPT Thread . Alerted => 
        END (* TRY EXCEPT *) 
      ELSIF MiscRec . type = GEndSweepMiscCode
      THEN 
        EndSweepSelection ( Window , MiscRec . time ) 
      END (* IF *) 
    END Misc 

; PROCEDURE BeginKeyWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . BeginKey ) 
    ; Display . HorizMoveCursorAndRepaintWindowRef 
         ( Closure . Window , - LbeStd . LimitedCharNoMax ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END BeginKeyWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayBeginKey ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker. ClosureTyp 
                     , Window := Window 
                     , apply := BeginKeyWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayBeginKey 

; PROCEDURE EndKeyWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . EndKey ) 
    ; Display . HorizMoveCursorToEndAndRepaint ( Closure . Window ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END EndKeyWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayEndKey ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := EndKeyWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayEndKey 

; PROCEDURE CharDelFwdWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . CharDelFwd ) 
    ; TextEdit . DeleteChar ( Closure . Window , DeletingBwd := FALSE ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END CharDelFwdWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayCharDelFwd ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := CharDelFwdWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayCharDelFwd 

; PROCEDURE CharDelBwdWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . CharDelBwd ) 
    ; TextEdit . DeleteChar ( Closure . Window , DeletingBwd := TRUE ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END CharDelBwdWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayCharDelBwd ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker. ClosureTyp 
                     , Window := Window 
                     , apply := CharDelBwdWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayCharDelBwd 

; PROCEDURE DeleteRestOfLineWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand
             ( UiRecPlay . CommandTyp . DeleteRestOfLine ) 
    ; TextEdit . DeleteRestOfLine ( Closure . Window ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END DeleteRestOfLineWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayDeleteRestOfLine ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := DeleteRestOfLineWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayDeleteRestOfLine 

; PROCEDURE CharTransposeWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . CharTranspose ) 
    ; TextEdit . TransposeChars ( Closure . Window ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END CharTransposeWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayCharTranspose ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := CharTransposeWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayCharTranspose 

; PROCEDURE CursorLeftWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . CursorLeft ) 
    ; Display . MoveCursorAndRepaintWindowRef 
        ( Closure . Window , Point . T { - 1 , 0 } ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END CursorLeftWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayCursorLeft ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := CursorLeftWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayCursorLeft 

; PROCEDURE CursorRightWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . CursorRight ) 
    ; Display . MoveCursorAndRepaintWindowRef 
        ( Closure . Window , Point . T { 1 , 0 } ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END CursorRightWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayCursorRight ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := CursorRightWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayCursorRight 

; PROCEDURE CursorUpWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . CursorUp ) 
    ; Display . MoveCursorAndRepaintWindowRef 
        ( Closure . Window , Point . T { 0 , - 1 } ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END CursorUpWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayCursorUp ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := CursorUpWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayCursorUp 

; PROCEDURE CursorDownWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . CursorDown ) 
    ; Display . MoveCursorAndRepaintWindowRef 
        ( Closure . Window , Point . T { 0 , 1 } ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END CursorDownWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayCursorDown ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := CursorDownWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayCursorDown 

; PROCEDURE PriorKeyWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . PriorKey ) 
    ; Display . VertScrollAndRepaint
         ( WindowRef := Closure . Window 
         , WantedMovement 
             := MIN ( 0 , - Closure . Window . EwSouthEastChar . v + 1 ) 
         , DoDragCursor := FALSE 
         ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END PriorKeyWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayPriorKey ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := PriorKeyWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayPriorKey 

; PROCEDURE NextKeyWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . NextKey ) 
    ; Display . VertScrollAndRepaint
         ( WindowRef := Closure . Window 
         , WantedMovement 
             := MAX ( 0 , Closure . Window . EwSouthEastChar . v - 1 ) 
         , DoDragCursor := FALSE 
         ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END NextKeyWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayNextKey ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := NextKeyWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayNextKey 

; PROCEDURE HomeKeyWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommand ( UiRecPlay . CommandTyp . HomeKey ) 
    ; Display . VertScrollAndRepaint
         ( WindowRef := Closure . Window 
         , WantedMovement := FIRST ( LbeStd . LineNoSignedTyp ) 
         , DoDragCursor := FALSE 
         ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END HomeKeyWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayHomeKey ( Window : WindowTyp ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( Worker . ClosureTyp 
                     , Window := Window 
                     , apply := HomeKeyWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayHomeKey

; PROCEDURE RecordInsertMode ( InsertMode : BOOLEAN ) 

  = BEGIN 
      IF InsertMode 
      THEN
        UiRecPlay . Record ( UiRecPlay . CommandTyp . InsertModeOn ) 
      ELSE 
        UiRecPlay . Record ( UiRecPlay . CommandTyp . InsertModeOff ) 
      END (* IF *) 
    END RecordInsertMode  

; PROCEDURE SetInsertModeWorkProc 
    ( Closure : WorkerClosureBoolTyp )  
  (* PRE: Closure . Window is set. Closure . BoolParam is value to set. *) 
  (* On Worker thread. *) 

  = BEGIN 
    (* No UiRecPlay . BeginCommand.  Probably not needed. *) 
      TextEdit . SetInsertMode 
        ( Closure . Window , Closure . BoolParam )
    ; RecordInsertMode ( Closure . BoolParam ) 
    END SetInsertModeWorkProc  

(* EXPORTED *) 
; PROCEDURE ReplaySetInsertMode ( Window : WindowTyp ; Value : BOOLEAN ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork  
               ( NEW ( WorkerClosureBoolTyp 
                     , Window := Window 
                     , BoolParam := Value  
                     , apply := SetInsertModeWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplaySetInsertMode  

; PROCEDURE ToggleInsertModeWorkProc ( Closure : Worker . ClosureTyp )  
  (* PRE: Closure . Window is set. *) 
  (* On Worker thread. *) 

  = VAR LIsInsert : BOOLEAN

  ; BEGIN 
    (* No UiRecPlay . BeginCommand.  Probably not needed. *) 
      LIsInsert 
        := TextEdit . ToggleInsertMode ( Closure . Window )
    ; RecordInsertMode ( LIsInsert ) 
    END ToggleInsertModeWorkProc  

(* EXPORTED *) 
; PROCEDURE ToggleInsertMode ( Window : WindowTyp ) 

  = BEGIN
      EVAL Worker . RequestWorkInteractive   
             ( NEW ( Worker . ClosureTyp 
                   , Window := Window  
                   , apply := ToggleInsertModeWorkProc 
                   ) 
             ) 
    END ToggleInsertMode

; TYPE WorkerClosureCharTyp
    = Worker . ClosureTyp OBJECT CharParam : CHAR END 

; PROCEDURE CharTypeWorkProc ( Closure : WorkerClosureCharTyp ) 
  RAISES { Backout , Thread . Alerted } 
  (* PRE: Closure . Window is set.  Closure . CharParam is char typed. *) 
  (* On Worker thread. *) 

  = VAR LCommandString : TEXT 

  ; BEGIN 
      LCommandString 
        := UiRecPlay . BeginCommandPlusChar  
             ( UiRecPlay . CommandTyp . CharType , Closure . CharParam ) 
    ; TextEdit . InsertOrOverlayChar 
        ( Closure . Window 
        , Closure . CharParam 
        , Closure . Window . WrInsertMode 
        ) 
    ; UiRecPlay . RecordString ( LCommandString ) 
    END CharTypeWorkProc 

(* VISIBLE *) 
; PROCEDURE ReplayCharType ( Window : WindowTyp ; Ch : CHAR ) 

  = BEGIN 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureCharTyp 
                     , Window := Window 
                     , CharParam := Ch 
                     , apply := CharTypeWorkProc 
                     ) 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END ReplayCharType 

; PROCEDURE Key ( Window : WindowTyp ; READONLY KeyRec : VBT . KeyRec ) 
  <* LL >= { } *> 
  (* This is the callback for the VBT key method, which is called with
     LL . sup = VBT . mu 
  *)  

  = CONST ControlOption 
      = SET OF VBT . Modifier 
          { VBT . Modifier . Control , VBT . Modifier . Option } 

  ; VAR LCh : CHAR 

  ; BEGIN (* Key *) 
      IF KeyRec . wentDown AND KeyRec . whatChanged # VBT . NoKey 
      THEN 
        LCh := KeyTrans . Latin1 ( KeyRec . whatChanged ) 
      ; IF NOT Rect . IsEmpty ( Window . EwDomain ) 
        THEN 
          IF KeyRec . modifiers * ControlOption = ControlOption 
          THEN (* Control and Option both held. *) 
            CASE LCh 
            OF 
            ELSE 
            END (* CASE *) 
          ELSIF VBT . Modifier . Control IN KeyRec . modifiers 
          THEN (* Control only held. *) 
            CASE LCh 
            OF (* Ctl/ *) 'a' , 'A' 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := BeginKeyWorkProc 
                              ) 
                        ) 

            | (* Ctl/ *) 'c' , 'C' 
            => TRY 
                 EVAL Worker . CancelImmedWork ( WaitToFinish := FALSE ) 
               EXCEPT Thread . Alerted => 
               END (* TRY EXCEPT *) 

            | (* Ctl/ *) 'd' , 'D' 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := CharDelFwdWorkProc 
                              ) 
                        ) 

            | (* Ctl/ *) 'e' , 'E' 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := EndKeyWorkProc 
                              ) 
                        ) 

            | (* Ctl/ *) 'k' , 'K' 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := DeleteRestOfLineWorkProc 
                              ) 
                        ) 

            | (* Ctl/ *) 't' , 'T' 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := CharTransposeWorkProc 
                              ) 
                        ) 
            ELSE 
            END (* CASE *) 
          ELSIF VBT . Modifier . Option IN KeyRec . modifiers 
          THEN (* Option only held *) 
            CASE LCh 
            OF 
            ELSE 
            END (* CASE *) 
          ELSE (* Plain key, no Control or Option *) 
            CASE KeyRec . whatChanged 
            OF KeyboardKey . BackSpace 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := CharDelBwdWorkProc 
                              ) 
                        ) 

            | KeyboardKey . Delete 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := CharDelFwdWorkProc 
                              ) 
                        ) 

            | KeyboardKey . Linefeed , KeyboardKey . Return 
            => EVAL Worker . RequestWorkInteractive 
                 ( NEW ( WorkerClosureCharTyp 
                       , Window := Window 
                       , CharParam :=  LbeStd . CharNewLine  
                       , apply := CharTypeWorkProc 
                       ) 
                   ) 

            | KeyboardKey . Tab 
            => EVAL Worker . RequestWorkInteractive 
                 ( NEW ( WorkerClosureCharTyp 
                       , Window := Window 
                       , CharParam :=  LbeStd . CharTab 
                       , apply := CharTypeWorkProc 
                       ) 
                   ) 

            | KeyboardKey . Left 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := CursorLeftWorkProc 
                              ) 
                        ) 

            | KeyboardKey . Right 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := CursorRightWorkProc 
                              ) 
                        ) 

            | KeyboardKey . Up 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := CursorUpWorkProc 
                              ) 
                        ) 

            | KeyboardKey . Down 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := CursorDownWorkProc 
                              ) 
                        ) 

            | KeyboardKey . Begin 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := BeginKeyWorkProc 
                              ) 
                        ) 

            | KeyboardKey . End 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := EndKeyWorkProc 
                              ) 
                        ) 

            | KeyboardKey . Prior
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := PriorKeyWorkProc 
                              ) 
                        ) 

            | KeyboardKey . Next 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := NextKeyWorkProc 
                              ) 
                        ) 

            | KeyboardKey . Home 
            => EVAL Worker . RequestWorkInteractive 
                        ( NEW ( Worker . ClosureTyp 
                              , Window := Window 
                              , apply := HomeKeyWorkProc 
                              ) 
                        ) 

            | KeyboardKey . Insert
            => ToggleInsertMode ( Window )  

            | KeyboardKey . Shift_L 
            , KeyboardKey . Shift_R 
            , KeyboardKey . Control_L 
            , KeyboardKey . Control_R 
            , KeyboardKey . Caps_Lock 
            , KeyboardKey . Shift_Lock 
            , KeyboardKey . Meta_L 
            , KeyboardKey . Meta_R 
            , KeyboardKey . Alt_L 
            , KeyboardKey . Alt_R 
            , 16_FE03 (* This is what Alt_R comes through as. *)  
            , KeyboardKey . Super_L 
            , KeyboardKey . Super_R 
            , KeyboardKey . Hyper_L 
            , KeyboardKey . Hyper_R 
            => (* Ignore when these go down. *) 

            ELSE 
              CASE LCh 
              OF LbeStd . CharReturn , LbeStd . CharNewLine   
              => EVAL Worker . RequestWorkInteractive 
                   ( NEW ( WorkerClosureCharTyp 
                         , Window := Window 
                         , CharParam := LbeStd . CharNewLine 
                         , apply := CharTypeWorkProc 
                         ) 
                   ) 

              | LbeStd . CharFirstPrintable .. LbeStd . CharLastPrintable 
              => EVAL Worker . RequestWorkInteractive 
                   ( NEW ( WorkerClosureCharTyp 
                         , Window := Window 
                         , CharParam := LCh 
                         , apply := CharTypeWorkProc 
                         ) 
                   ) 
              ELSE 
                Display . Beep ( Errors . ErrorTyp . EBadChar ) 
              END (* CASE *) 
            END (* CASE *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END Key 

(* =================== Rescreen, reshape, repaint, etc.================= *)

; VAR GLogEvents : BOOLEAN := TRUE 

; PROCEDURE RedoWorkProc ( Closure : Worker . ClosureTyp ) 
  RAISES { Backout , Thread . Alerted } 

  = VAR LDoRescreen : BOOLEAN 
  ; VAR LDoReshape : BOOLEAN 
  ; VAR LDoRepaint : BOOLEAN 
  ; VAR LOldSize : Point . T 
  ; VAR LNewSize : Point . T 
  ; VAR LRegion : Region . T 
  ; VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp 

  ; BEGIN
      IF GLogEvents THEN RTIO . PutText ( "In RedoWorkProc: " ) END (* IF *) 
    ; LOCK Closure . Window 
      DO LDoRescreen := Closure . Window . EwHasRescreenPending
      ; IF LDoRescreen
        THEN 
          IF GLogEvents THEN RTIO . PutText ( " rescreening" ) END (* IF *) 
        ; Closure . Window . EwRedoState := RedoStateTyp . Rescreening  
        ; Closure . Window . EwScreenType 
            := Closure . Window . EwPendingRescreenRec . st
        (* Let's assume Trestle won't rescreen unless st actually changes. *)
        ; Closure . Window . EwDomain := Rect . Empty 
        ; Closure . Window . EwHasRescreenPending := FALSE 
        END (* IF *) 
      END (* LOCK *) 
    ; IF LDoRescreen 
      THEN 
(* CHECK: Do we want to record and replay rescreen? *) 
      END (* IF *)
      
    ; LOCK Closure . Window 
      DO IF Closure . Window . EwHasReshapePending 
        THEN 
          IF GLogEvents THEN RTIO . PutText ( " reshaping" ) END (* IF *) 
        ; Closure . Window . EwRedoState := RedoStateTyp . Reshaping  
        ; Closure . Window . EwDomain 
            := Closure . Window . EwPendingReshapeRec . new 
        ; LOldSize := Closure . Window . EwSouthEastChar 
        ; ComputeDerivedWindowInfo ( Closure . Window ) 
        ; LNewSize := Closure . Window . EwSouthEastChar
        ; LDoReshape := LNewSize # LOldSize
        ; Closure . Window . EwHasReshapePending := FALSE 
        END (* IF *) 
      END (* LOCK *) 
    ; IF LDoReshape 
      THEN (* Do the reshape on the worker thread, without the lock. *) 
        IF GLogEvents THEN RTIO . PutText ( "(changed)" ) END (* IF *) 
      ; Display . ReshapeWindowRef ( Closure . Window , LOldSize , LNewSize ) 
(* CHECK: Do we want to record and replay reshape? *) 
      END (* IF *)
      
    ; LOCK Closure . Window 
      DO IF Closure . Window . EwHasRepaintPending 
        THEN 
          IF GLogEvents THEN RTIO . PutText ( " repainting" ) END (* IF *) 
        ; Closure . Window . EwRedoState := RedoStateTyp . Repainting  
        ; LRegion := Closure . Window . EwPendingRepaintRegion 
          (* Maybe someday repaint will want this. *) 
        ; LDoRepaint
            := Closure . Window . EwScreenType # NIL 
               AND NOT Rect . IsEmpty ( Closure . Window . EwDomain ) 
        ; Closure . Window . EwHasRepaintPending := FALSE
        END (* IF *) 
      END (* LOCK *) 
    ; IF LDoRepaint 
      THEN (* Do the repaint on the worker thread, without the lock. *)
        IF GLogEvents THEN RTIO . PutText ( "(nonempty)" ) END (* IF *) 
      ; Display . PaintWindowFromLines 
          ( Closure . Window 
          , (* VAR *) LTrailingBlankLines (* Dead. *) 
          , (* VAR *) LLinesRef (* Dead. *) 
          , (* VAR *) LLineNo  (* Dead. *) 
          )
(* CHECK: Do we want to record and replay repaint? *) 
      END (* IF *)
     
    ; LOCK Closure . Window 
      DO Closure . Window . EwRedoState := RedoStateTyp . Done   
      END (* LOCK *) 
    ; IF GLogEvents
      THEN
        RTIO . PutText ( " end." )
      ; RTIO . PutText ( Wr . EOL ) 
      ; RTIO . Flush ( )
      END (* IF *)
    END RedoWorkProc 

; TYPE WorkerClosureRescreenTyp
    = Worker . ClosureTyp OBJECT Rescreen : VBT . RescreenRec END 

; <* UNUSED *> 
(* CHECK: Will we need it someday? *) 
  PROCEDURE RescreenWorkProc ( Closure : WorkerClosureRescreenTyp )  
  <* LL.sup = VBT.mu.Window *> 
  (* PRE: Closure . Window, RescreenRec are set. *) 
  (* On worker thread. *) 

  = BEGIN (* RescreenWorkProc *) 
      Closure . Window . EwScreenType := Closure . Rescreen . st 
    ; Closure . Window . EwDomain := Rect . Empty 
(* CHECK: Do we want to record and replay rescreen? *) 
    END RescreenWorkProc  

; PROCEDURE Rescreen 
    ( Window : WindowTyp ; READONLY RescreenRec : VBT . RescreenRec ) 
    RAISES { } 
  <* LL.sup = VBT.mu.Window *> 

  = BEGIN (* Rescreen *) 
      IF GLogEvents
      THEN
        RTIO . PutText ( "Rescreen Callback. " )
      ; RTIO . PutText ( Wr . EOL ) 
      ; RTIO . Flush ( ) 
      END (* IF *) 
    ; LOCK Window 
      DO
        Window . EwHasRescreenPending := TRUE  
      ; Window . EwPendingRescreenRec := RescreenRec 
      ; Window . EwHasReshapePending := FALSE (* Cancel *) 
      ; Window . EwHasRepaintPending := FALSE (* Cancel *) 
      ; CASE Window . EwRedoState 
        OF RedoStateTyp . Starting 
        => (* Let RedoWorkProc proceed. It will get to the Rescreen. *) 
        | RedoStateTyp . Rescreening  
        , RedoStateTyp . Reshaping 
        , RedoStateTyp . Repainting 
        , RedoStateTyp . Done 
        => (* Start it over.  This will cancel it if already running. *)  
          Window . EwRedoState := RedoStateTyp . Starting  
        ; TRY 
            EVAL Worker . RequestQueuedWork 
                   ( Window . EwRedoClosure
                   , Worker . GranularityTyp . Image  
                   ) 
          EXCEPT Thread . Alerted => (* Ignore. *) 
          END (* TRY EXCEPT *) 
        END (* CASE *) 
      END (* LOCK *) 
    END Rescreen 

; TYPE WorkerClosureReshapeTyp
    = Worker . ClosureTyp OBJECT Reshape : VBT . ReshapeRec END 

; <* UNUSED *> 
(* CHECK: Will we need it someday? *) 
  PROCEDURE ReshapeWorkProc ( Closure : WorkerClosureReshapeTyp )  
  RAISES { Backout , Thread . Alerted } 
  <* LL.sup = VBT.mu.Window *> 
  (* PRE: Closure . Window, ReshapeRec are set. *) 
  (* On worker thread. *) 

  = VAR LOldSize : Point . T 
  ; VAR LNewSize : Point . T 

  ; BEGIN (* ReshapeWorkProc *)
      LOCK Closure . Window
      DO 
        LOldSize := Closure . Window . EwSouthEastChar 
      ; Closure . Window . EwDomain := Closure . Reshape . new 
      ; ComputeDerivedWindowInfo ( Closure . Window ) 
      ; LNewSize := Closure . Window . EwSouthEastChar
      END (* LOCK *) 
    ; IF LNewSize # LOldSize 
      THEN 
        Display . ReshapeWindowRef ( Closure . Window , LOldSize , LNewSize ) 
      END (* IF *) 
(* CHECK: Do we want to record and replay reshape? *) 
    END ReshapeWorkProc 

; PROCEDURE Reshape 
    ( Window : WindowTyp ; READONLY ReshapeRec : VBT . ReshapeRec ) 
  RAISES { } 
  <* LL.sup = VBT.mu.Window *> 

  = BEGIN (* Reshape   *) 
      IF GLogEvents
      THEN
        RTIO . PutText ( "Reshape Callback. " )
      ; RTIO . PutText ( Wr . EOL ) 
      ; RTIO . Flush ( ) 
      END (* IF *) 
    ; LOCK Window 
      DO
        Window . EwHasReshapePending := TRUE 
      ; Window . EwPendingReshapeRec := ReshapeRec 
      ; Window . EwHasRepaintPending := FALSE (* Cancel *) 
      ; CASE Window . EwRedoState 
        OF RedoStateTyp . Starting 
        , RedoStateTyp . Rescreening  
        => (* Let RedoWorkProc proceed. It will get to the Reshape. *) 
        | RedoStateTyp . Reshaping 
        , RedoStateTyp . Repainting 
        , RedoStateTyp . Done 
        => (* Start it over.  This will cancel it if already running. *)  
          Window . EwRedoState := RedoStateTyp . Starting  
        ; TRY 
            EVAL Worker . RequestQueuedWork 
                   ( Window . EwRedoClosure
                   , Worker . GranularityTyp . Image  
                   ) 
          EXCEPT Thread . Alerted => (* Ignore. *) 
          END (* TRY EXCEPT *) 
        END (* CASE *) 
      END (* LOCK *) 
    END Reshape 

; <* UNUSED *> 
(* CHECK: Will we need it someday? *) 
  PROCEDURE RepaintWorkProc ( Closure : Worker . ClosureTyp )   
  RAISES { Backout , Thread . Alerted } 
  <* LL.sup = VBT.mu.Window *> 
  (* PRE: Closure . Window is set. *) 
  (* On worker thread. *) 

  = VAR LTrailingBlankLines : LbeStd . LineNoTyp 
  ; VAR LLinesRef : PaintHs . LinesRefMeatTyp 
  ; VAR LLineNo : LbeStd . LineNoSignedTyp
  ; VAR LDoRepaint : BOOLEAN 

  ; BEGIN (* RepaintWorkProc *) 
      LOCK Closure . Window
      DO LDoRepaint
           := Closure . Window . EwScreenType # NIL 
              AND NOT Rect . IsEmpty ( Closure . Window . EwDomain )
      END (* LOCK *)
    ; IF LDoRepaint 
      THEN 
        Display . PaintWindowFromLines 
          ( Closure . Window 
          , (* VAR *) LTrailingBlankLines (* Dead. *) 
          , (* VAR *) LLinesRef (* Dead. *) 
          , (* VAR *) LLineNo  (* Dead. *) 
          )
      END (* IF *) 
(* CHECK: Do we want to record and replay repaint? *) 
    END RepaintWorkProc 

; PROCEDURE Repaint 
    ( Window : WindowTyp ; READONLY Region : Region . T ) 
    RAISES { } 
  <* LL.sup = VBT.mu.Window *> 

  = BEGIN (* Repaint   *) 
      IF GLogEvents
      THEN
        RTIO . PutText ( "Repaint Callback. " )
      ; RTIO . PutText ( Wr . EOL ) 
      ; RTIO . Flush ( ) 
      END (* IF *) 
    ; LOCK Window 
      DO
        Window . EwHasRepaintPending := TRUE 
      ; Window . EwPendingRepaintRegion := Region 
      ; CASE Window . EwRedoState 
        OF RedoStateTyp . Starting 
        , RedoStateTyp . Rescreening  
        , RedoStateTyp . Reshaping 
        => (* Let RedoWorkProc proceed. It will get to the Repaint. *) 
        | RedoStateTyp . Repainting 
        , RedoStateTyp . Done 
        => (* Start it over.  This will cancel it if already running. *)  
          Window . EwRedoState := RedoStateTyp . Starting  
        ; TRY 
            EVAL Worker . RequestQueuedWork 
                   ( Window . EwRedoClosure
                   , Worker . GranularityTyp . Image  
                   ) 
          EXCEPT Thread . Alerted => (* Ignore. *) 
          END (* TRY EXCEPT *) 
        END (* CASE *) 
      END (* LOCK *) 
    END Repaint 

; PROCEDURE Redisplay ( Window : WindowTyp ) RAISES { } 
  <* LL . sup < Window . mu *> 
  (* This is the callback for the VBT redisplay method, which is called with 
     LL.sup = VBT.mu 
  *) 

  = BEGIN (* Redisplay   *) 
      VBT . ForceRepaint ( Window , Region . Full ) 
    END Redisplay 

; PROCEDURE Shape 
    ( Window : WindowTyp ; ax : Axis . T ; <*UNUSED*> n : CARDINAL ) 
    : VBT . SizeRange 
    RAISES { } 
  <* LL.sup = VBT.mu.Window *> 

  = BEGIN (* Shape *) 
      CASE ax 
      OF Axis . T . Hor 
      => RETURN 
           VBT . SizeRange 
             { lo (* Big enough for one character *) 
                 := Window . EwCharSeparation . h + Window . EwMargin . h * 2 
             , pref 
                 := Window . EwCharSeparation . h * PreferredCharSize . h 
                    + Window . EwMargin . h * 2 
             , hi := MaxWindowSize 
             } 
      | Axis . T . Ver 
      => RETURN 
           VBT . SizeRange 
             { lo (* Big enough for one character *) 
                 := Window . EwCharSeparation . v + Window . EwMargin . v * 2 
             , pref 
                 := Window . EwCharSeparation . v * PreferredCharSize . v 
                    + Window . EwMargin . v * 2 
             , hi := MaxWindowSize 
             } 
      END (* CASE *) 
    END Shape 

; PROCEDURE Read 
    ( <*UNUSED*> Window : WindowTyp 
    ; <*UNUSED*> sel : VBT . Selection 
    ; <*UNUSED*> tc : CARDINAL 
    ) 
  : VBT . Value 
  <* LL.sup <= VBT.mu *> 

  = VAR LSel : Selection . SelectionTyp 
  ; LText : TEXT 

  ; BEGIN 
      LSel := Selection . Current 
    ; IF LSel = NIL 
      THEN LText := NIL 
      ELSE
        LText := LSel . SelText  
      ; IF LText = NIL 
        THEN 
          TRY 
            LSel . SelText := Selection . ManifestSelectionAsText ( ) 
          ; LText := LSel . SelText 
          EXCEPT 
          | Thread . Alerted => LText := "" 
          | Backout => LText := "" 
          END (* TRY EXCEPT *) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN VBT . FromRef ( LText ) 
    END Read 

; PROCEDURE Write 
    ( <*UNUSED*> Window : WindowTyp 
    ; <*UNUSED*> sel : VBT . Selection 
    ; <*UNUSED*> val : VBT . Value 
    ; <*UNUSED*> tc : CARDINAL 
    ) 
  <* LL.sup <= VBT.mu *> 

  = BEGIN (* Write   *) 
    END Write 

; PROCEDURE Discard ( <*UNUSED*> Window : WindowTyp ) 
  <* LL >= { } *> 
  (* This is the callback for the VBT discard method, which is called with
     LL . sup = VBT . mu 
  *)  

  = BEGIN (* Discard *) 
(* FIX: Apparently, this does not get invoked when the window is closed
        by the user.  So when does it get called? *) 
      TRY 
        EVAL Worker . RequestWork 
               ( NEW ( WorkerClosureBoolTyp 
                     , BoolParam := TRUE 
                     , apply := PromptAndCloseWorkProc 
                     ) 
               , Interactive := FALSE 
               , WaitToStart := TRUE 
               , WaitToFinish := TRUE 
               ) 
      EXCEPT Thread . Alerted => 
      END (* TRY EXCEPT *) 
    END Discard 

(* EXPORTED *) 
; PROCEDURE BeginPaintGroup ( Window : WindowTyp ) 
  <* LL . sup < Window . mu *> 

  = BEGIN 
      VBT . BeginGroup ( Window ) 
    END BeginPaintGroup 

(* EXPORTED *) 
; PROCEDURE EndPaintGroup ( Window : WindowTyp ) 
  <* LL . sup < Window . mu *> 

  = BEGIN 
      VBT . EndGroup ( Window ) 
    END EndPaintGroup 

; BEGIN (* EditWindow *) 
    GEndSweepMiscCode := VBT . GetMiscCodeType ( "SchutzEndSweep" ) 
  END EditWindow 
. 
