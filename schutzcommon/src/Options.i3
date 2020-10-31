
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Options 

(* Global options. *) 

; IMPORT FormsVBT 
; IMPORT Rsrc 
; IMPORT Rd 
; IMPORT Wr 

; IMPORT PortTypes 
; IMPORT LbeStd 
; IMPORT PaintHs 

; VAR Display : TEXT 
; VAR Geometry : TEXT 
; VAR EditFileName : TEXT 

; CONST MaxDebugLevel = 5 
; TYPE DebugLevelTyp = [ 0 .. MaxDebugLevel ] 
; VAR DebugLevel : DebugLevelTyp 

; VAR AssertionChecking : BOOLEAN 
; VAR ExpensiveChecking : BOOLEAN 
; VAR LogMessages : BOOLEAN 
; VAR TreeBrowsing : BOOLEAN 
; VAR ParseTracing : BOOLEAN 
; VAR AllowProceedAfterAssert : BOOLEAN 
; VAR AllowTerminateAfterAssert : BOOLEAN 
; VAR PreferGeneratedGrammar : BOOLEAN := FALSE
; VAR Crash : BOOLEAN := FALSE 

; PROCEDURE SetDerivedDebugOptions ( ) 

; VAR EnablePickleWrite : BOOLEAN 
; VAR ResourcePath : Rsrc . Path 

; VAR ParseCheckMin : LbeStd . LimitedTokCtTyp := 4 
; VAR ParseCheckMax : LbeStd . LimitedTokCtTyp := 10 
; VAR SimpleRepairPointCt : PortTypes . Card16Typ 
; VAR InitialIndentPos : LbeStd . LimitedCharNoTyp := 0 
      (* ^Indent pos of top level Est tree. *) 
; VAR RightMargin : LbeStd . LimitedCharNoTyp := 80 
  (* ^ char position in line up to which text can appear *) 
; VAR LineMidpoint : LbeStd . LimitedCharNoTyp := 40 
  (* ^Used to classify comments.  Changing this would probably make 
     a mess of existing comments. *) 
; VAR LinesRefsToRetain : LbeStd . LineNoTyp := 1000 
; VAR StoppersToRetain : LbeStd . LineNoTyp := 50 
; VAR DelNlAtBOL : BOOLEAN := TRUE 

; VAR FontNamePlain := "-*-fixed-medium-r-*-*-13-*-*-*-*-*-*-*" 
; VAR FontNameBold := "-*-fixed-bold-r-*-*-13-*-*-*-*-*-*-*" 
; VAR FontNameItalic := "-*-fixed-medium-o-*-*-13-*-*-*-*-*-*-*" 

; CONST PrimaryMax = 16_FFFF
; TYPE PrimaryTyp = [ 0 .. PrimaryMax ] 

; TYPE ColorTyp 
    = RECORD 
        Red : PrimaryTyp 
      ; Green : PrimaryTyp 
      ; Blue : PrimaryTyp 
      END 

(* Some captured colors to choose from: *) 
; CONST Black = ColorTyp { 0 , 0 , 0 } (* Black *)  
; CONST White = ColorTyp { 16_FFFF , 16_FFFF , 16_FFFF } (* White *)  

; CONST Butterscotch = ColorTyp { 16_D2D2, 16_AFAF , 16_0000 }
; CONST Yellow = ColorTyp { 16_D2D2, 16_D2D2 , 16_0000 }
; CONST Yellow2 = ColorTyp { 16_e6e6 , 16_e6e6 , 16_0b0b } 
; CONST Olive = ColorTyp { 16_7c7c , 16_7c7c , 16_0606 } 
; CONST LightSage = ColorTyp { 16_D2D2, 16_DFDF , 16_B8B8 }
; CONST DarkSage = ColorTyp { 16_C0C0, 16_D2D2 , 16_9595 }
; CONST Gray = ColorTyp { 16_CECE, 16_CECE , 16_CECE }
; CONST VeryLightGray = ColorTyp { 16_e0e0, 16_e0e0 , 16_e0e0 }
; CONST LightGray = ColorTyp { 16_c8c8, 16_c8c8 , 16_c8c8 }
; CONST MedGray = ColorTyp { 16_a0a0, 16_a0a0 , 16_a0a0 }
; CONST Pink = ColorTyp { 16_FFFF, 16_7A7A , 16_7575 } (* Pink *)   
; CONST Lime = ColorTyp { 16_EFEF , 16_FFFF , 16_EFEF } (* Lime *)  
; CONST Periwinkle = ColorTyp { 16_EFEF , 16_EFEF , 16_FFFF } (* Peri *) 
; CONST Turquoise = ColorTyp { 16_0202 , 16_B3B3 , 16_B9B9 } 
; CONST VeryVeryPaleBlue = ColorTyp { 16_eded , 16_eded , 16_eded } 
 
; CONST MediumGreen = ColorTyp { 16_0000 , 16_7D7D , 16_1D1D } (* Forest *)  
; CONST DarkGreen = ColorTyp { 16_0000 , 16_5959 , 16_1515 } (* Darker forest *)
; CONST GrayBrown = ColorTyp { 16_6666 , 16_4242 , 16_0000 } (* Brown *)   
; CONST RichBrown = ColorTyp { 16_9e9e , 16_5252 , 16_0000 } 
; CONST DarkBrown = ColorTyp { 16_6d6d , 16_3838 , 16_0000 } 
; CONST UglyTan = ColorTyp { 16_ffff , 16_d2d2 , 16_9292 } 
; CONST Tan2 = ColorTyp { 16_dcdc , 16_dbdb , 16_a4a4 } 
; CONST RoyalBlue = ColorTyp { 0 , 0 , 16_8080 } (* Royal *)   
; CONST SchoolBus = ColorTyp { 16_E600 , 16_C700 , 0 } 
; CONST Red = ColorTyp { 16_E600 , 0 , 0 } 
; CONST ErrorRed = ColorTyp { 16_E0E0 , 16_2A2A , 16_2D2D } 
; CONST PaleRed = ColorTyp { 16_FBFB , 16_5E5E , 16_6161 } 
; CONST BloodRed = ColorTyp { 16_9191 , 16_1919 , 16_2929 } 
; CONST RobinsEgg = ColorTyp { 16_aaaa , 16_e4e4 , 16_fbfb } 
; CONST LightSkyBlue1 = ColorTyp { 16_b0b0 , 16_e2e2 , 16_ffff } 

(* Colors plagiarized from Emacs colorization: *)
; CONST ETan = ColorTyp { 16_bcbc , 16_8f8f , 16_8f8f }  
; CONST EBrown = ColorTyp { 16_b8b8 , 16_8b8b , 16_0b0b }  
; CONST ELavender = ColorTyp { 16_dada , 16_7070 , 16_d6d6 }  
; CONST EGreen = ColorTyp { 16_2222 , 16_8b8b , 16_2222 }  
; CONST EBlue = ColorTyp { 16_0000 , 16_0000 , 16_ffff }  
; CONST ERed = ColorTyp { 16_b2b2 , 16_2222 , 16_2222 }  
; CONST EPurple = ColorTyp { 16_a0a0 , 16_2020 , 16_f0f0 }  
; CONST ETurquoise = ColorTyp { 16_5f5f , 16_9e9e , 16_a0a0 }  
; CONST ESelectionBlue = ColorTyp { 16_4646 , 16_8282 , 16_b4b4 }  

; CONST GimpOrange = ColorTyp { 16_ffff , 16_8c8c , 16_0000 } 
; CONST GimpRedX = ColorTyp { 16_b5b5 , 16_4444 , 16_4e4e } 

; VAR BgColorCharsArea := VeryVeryPaleBlue  
; VAR BgColorBorder := DarkSage 

; VAR BgColorPlain := VeryVeryPaleBlue
; VAR BgColorCmnt := VeryVeryPaleBlue  
; VAR BgColorLiteral := VeryVeryPaleBlue    
; VAR BgColorSelected := MedGray       
; VAR BgColorMatched := LightSkyBlue1        

; VAR FgColorPlain := Black 
; VAR FgColorIdent := DarkGreen 
; VAR FgColorLiteral := Turquoise  
; VAR FgColorCmnt := RoyalBlue 
; VAR FgColorPlaceholder := EPurple  

; VAR DecColorErr := ErrorRed 
; VAR DecColorTyped := Butterscotch 
; VAR DecColorTouched := GrayBrown  

; VAR MainForm : FormsVBT . T 
; VAR MainWindow : PaintHs . WindowRefTyp 
(* TOTO:  MainWindow is only here because, in AssertDevel.Checkpoint, we need
   it, but can't call FormsVBT . GetGeneric ( MainForm , "Fv_LbeWindow" ),
   because it can attempt to reacquire a mutex that is often already held.
   When there is > 1 window, something will have to be done. 
*) 

; VAR DevelWritePath : TEXT 

; VAR InsertNilFixedChildren : BOOLEAN := FALSE 

; VAR DoDisplayErrors : BOOLEAN := FALSE   
; VAR DoOptimizeSingletonLists := FALSE        

; VAR RecPlayForm : FormsVBT . T := NIL 

; VAR RecordFileName : TEXT := "" 
; VAR RecordWrT : Wr . T := NIL 

; VAR PlaybackFileName : TEXT := "" 
; VAR PlaybackRdT : Rd . T := NIL 

; VAR TraceFileName : TEXT := "" 
; VAR TraceWrT : Wr . T := NIL 

; VAR OpeningImageRef : PaintHs . ImageTransientTyp := NIL 
  (* Used temporarily during opening of files, to communicate to
     assertion/runtime failure routines, before the ImageRef is put
     into a window.  We don't want to put it in a window prematurely.
  *) 

; END Options 
. 
