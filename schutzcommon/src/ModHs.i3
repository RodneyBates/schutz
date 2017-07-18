
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE ModHs 

(* Data structures for modifications to Ests, e.g. comments, errors, 
   unparsed text edits, etc.  These appear as nodes inside an Est.  
 *) 

; IMPORT EstHs 
; IMPORT PortTypes  
; IMPORT LbeStd 
; IMPORT SharedStrings 

; FROM Assertions IMPORT AssertionFailure 

(* Rules of modification lists: *) 
(*  On a single token, all Lex errors must come after all 
    comments and blank lines. 
*) 

(* Supertype of all Est modifiers: *) 
; TYPE ModRefTyp 
    = LbeStd . EstRootTyp 
        OBJECT 
        METHODS 
          Image ( Indent := LbeStd . StdIndent ) : TEXT 
        END (* OBJECT *) 

(* Blank lines> *) 
; TYPE ModBlankLineTyp 
    = ModRefTyp 
        OBJECT 
          ModBlankLineCt : LbeStd . LineNoTyp 
        OVERRIDES 
          Image := ModBlankLineImage 
        END (* OBJECT  ModBlankLineTyp *) 

; PROCEDURE ModBlankLineImage 
    ( Self : ModBlankLineTyp ; Indent := LbeStd . StdIndent ) : TEXT 

(* Detected errors. *) 
; TYPE ModErrTyp = ModRefTyp BRANDED "ModErrTyp" OBJECT END (* OBJECT *) 

; TYPE ModLexErrTyp 
    = ModErrTyp 
        OBJECT 
          ModLexErrCode : LbeStd . ErrCodeTyp 
        ; ModLexErrPos : LbeStd . LimitedCharNoTyp 
        OVERRIDES 
          Image := ModLexErrImage 
        END (* OBJECT  ModLexErrTyp *) 

; PROCEDURE ModLexErrImage 
    ( Self : ModLexErrTyp ; Indent := LbeStd . StdIndent ) : TEXT 

; TYPE ModSyntErrTyp 
    = ModErrTyp 
        OBJECT 
          ModSyntErrCode : LbeStd . ErrCodeTyp 
        OVERRIDES 
          Image := ModSyntErrImage 
        END (* OBJECT  ModSyntErrTyp *) 

; PROCEDURE ModSyntErrImage 
    ( Self : ModSyntErrTyp ; Indent := LbeStd . StdIndent ) : TEXT 

(* Deletions.  This denote that an item that is not explicit in the Est,
   but rather, normally inserted by an Fs tree, is actually omitted. 
*) 
; TYPE ModDelTyp 
    = ModRefTyp 
        OBJECT 
          ModDelThruFmtNo : EstHs . FmtNoTyp 
        ; ModDelIsRepair : BOOLEAN := FALSE 
        OVERRIDES 
          Image := ModDelImage 
        END (* OBJECT  ModDelTyp *) 

; PROCEDURE ModDelImage 
    ( Self : ModDelTyp ; Indent := LbeStd . StdIndent ) : TEXT 

(* Comments. *)
; TYPE ModCmntTyp 
    = ModRefTyp 
        OBJECT 
          ModCmntBegScanState : LbeStd . ScanStateTyp 
        ; ModCmntEndScanState : LbeStd . ScanStateTyp 
        ; ModCmntFromPos : LbeStd . LimitedCharNoSignedTyp 
        ; ModCmntNlBefore : BOOLEAN := FALSE 
        ; ModCmntNlAfter : BOOLEAN := FALSE 
        ; ModCmntStringRef : SharedStrings . T 
        METHODS 
          KindImage ( ) : TEXT := ModCmntKindImage 
        OVERRIDES 
          Image := ModCmntImage 
        END (* OBJECT ModCmntTyp *) 

; PROCEDURE ModCmntKindImage ( Self : ModCmntTyp ) : TEXT 

; PROCEDURE ModCmntImage 
    ( Self : ModCmntTyp ; Indent := LbeStd . StdIndent ) : TEXT 

; TYPE ModCmntLeadingTyp 
    = ModCmntTyp BRANDED "ModCmntLeadingTyp" OBJECT END (* OBJECT *) 

; TYPE ModCmntTrailingTyp 
    = ModCmntTyp BRANDED "ModCmntTrailingTyp" OBJECT END (* OBJECT *) 

; TYPE ModCmntLeadingFixedTyp 
    = ModCmntLeadingTyp BRANDED "ModCmntLeadingFixedTyp" OBJECT END 
  (* ^Comment at beginning of line, with absolute start postion. 
      INVARIANT: ModCmntNlBefore is always TRUE. 
  *) 

; TYPE ModCmntLeadingRelativeTyp 
    = ModCmntLeadingTyp BRANDED "ModCmntLeadingRelativeTyp" OBJECT END 
  (* ^Comment at beginning of line, with IndentPos-relative 
     start position. *) 

; TYPE ModCmntTrailingFixedTyp 
    = ModCmntTrailingTyp 
        BRANDED "ModCmntTrailingFixedTyp" OBJECT END (* OBJECT *) 
  (* ^Comment on same line as any preceeding items, with fixed start 
      position. 
      INVARIANT: ModCmntNlBefore is always FALSE. 
  *) 

; TYPE ModCmntTrailingRelativeTyp 
    = ModCmntTrailingTyp 
        BRANDED "ModCmntTrailingRelativeTyp" OBJECT END (* OBJECT *) 
  (* ^Comment attached as trailing mod to preceeding item.  On 
     same line, at relative position. 
     INVARIANT: ModCmntNlBefore is always FALSE. 
  *) 

; PROCEDURE CopyOfModCmnt ( Old : ModCmntTyp ) : ModCmntTyp 
  RAISES { AssertionFailure } 
  (* Make a copy. *) 

(* Unreparsed textual edits. *) 
; TYPE ModTextTyp 
    = ModRefTyp 
        OBJECT 
          ModTextLeftTokToPos : LbeStd . LimitedCharNoTyp 
        ; ModTextFromPos : LbeStd . LimitedCharNoTyp 
        ; ModTextToPos : LbeStd . LimitedCharNoTyp 
        (* ^ModTextToPos 
           = LbeStd . LimitedCharNoInfinity means new line after. *) 
        ; ModTextOrigToPos : LbeStd . LimitedCharNoTyp 
          (* Absolute to position of the original text that this replaces. *) 
        ; ModTextStringRef : SharedStrings . T 
        OVERRIDES 
          Image := ModTextImage 
        END (* OBJECT ModTextTyp *) 

; PROCEDURE ModTextImage 
    ( Self : ModTextTyp ; Indent := LbeStd . StdIndent ) : TEXT 

; PROCEDURE CopyOfModText ( Old : ModTextTyp ) : ModTextTyp 
  (* Make a copy. *) 

(* Dummies.  These denote nothing visible, but contain sometimes needed 
   internal information. 
*) 
; TYPE EstApproxChildCtTyp 
    = [ 0 .. 2 ] (* 2 means at least 2. *) 

; CONST EstApproxChildCtImage = PortTypes . Int32Image 

; TYPE EstDummyTyp = LbeStd . EstRootTyp OBJECT 
       END (* OBJECT EstDummyTyp *) 

; TYPE EstDummyTokTyp = EstDummyTyp OBJECT 
         Tok : LbeStd . TokTyp 
       ; ApproxChildCt : BITS 2 FOR EstApproxChildCtTyp 
       END (* OBJECT EstDummyTokTyp *) 
  (* These replace whole Est subtrees that are removed during text editing. 
     They contain enough information to evaluate any conditional format
     predicate, duplicating the original node in this respect. *) 

; TYPE EstDummyTempMarkTyp = EstDummyTyp OBJECT END 
  (* These temporarily replace NILs in the Est, when there is a need to
     have TempMarks pointing to them. *) 

; PROCEDURE EstDummyImage 
    ( Self : EstDummyTyp ; Indent := LbeStd . StdIndent ) : TEXT 

; END ModHs 
. 
