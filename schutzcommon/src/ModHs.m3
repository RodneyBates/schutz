
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE ModHs 

(* Data structures for modifications to Ests, e.g. comments, errors, 
   unparsed text edits, etc.  These appear as nodes inside an Est.  
 *) 

; IMPORT EstHs  
; IMPORT LbeStd 
; IMPORT Misc 
; IMPORT SharedStrings 
; IMPORT MessageCodes 

; FROM Assertions IMPORT CantHappen , AssertionFailure 

; TYPE AFT = MessageCodes . T 

(* VISIBLE: *) 
; PROCEDURE ModBlankLineImage 
    ( Self : ModBlankLineTyp ; <* UNUSED *> Indent := LbeStd . StdIndent ) : TEXT 

  = BEGIN (* ModBlankLineImage *) 
      RETURN 
        "ModBlankLine{" 
        & LbeStd . LineNoImage ( Self . ModBlankLineCt ) 
        & "}" 
    END ModBlankLineImage 

(* VISIBLE: *) 
; PROCEDURE ModLexErrImage 
    ( Self : ModLexErrTyp ; <* UNUSED *> Indent := LbeStd . StdIndent ) : TEXT 

  = BEGIN (* ModLexErrImage *) 
      RETURN 
        "ModLexErr{" 
        & LbeStd . ErrCodeImage ( Self . ModLexErrCode ) 
        & "," 
        & LbeStd . LimitedCharNoImage ( Self . ModLexErrPos ) 
        & "}" 
    END ModLexErrImage 

(* VISIBLE: *) 
; PROCEDURE ModSyntErrImage 
    ( Self : ModSyntErrTyp ; <* UNUSED *> Indent := LbeStd . StdIndent ) : TEXT 

  = BEGIN (* ModSyntErrImage *) 
      RETURN 
        "ModSyntErr{" & LbeStd . ErrCodeImage ( Self . ModSyntErrCode ) & "}" 
    END ModSyntErrImage 

(* VISIBLE: *) 
; PROCEDURE ModDelImage 
    ( Self : ModDelTyp ; <* UNUSED *> Indent := LbeStd . StdIndent ) : TEXT 

  = BEGIN (* ModDelImage *) 
      RETURN 
        "ModDel{" 
        & EstHs . FmtNoImage ( Self . ModDelThruFmtNo ) 
        & "," & Misc . BooleanImageShort ( Self . ModDelIsRepair ) 
        & "}" 
    END ModDelImage 

(* VISIBLE: *) 
; PROCEDURE ModCmntKindImage ( Self : ModCmntTyp ) : TEXT 

  = BEGIN (* ModCmntKindImage *) 
      TYPECASE Self 
      OF ModCmntLeadingFixedTyp 
      => RETURN "ModCmntLeadingFixed" 
      | ModCmntLeadingRelativeTyp 
      => RETURN "ModCmntLeadingRelative" 
      | ModCmntTrailingFixedTyp 
      => RETURN "ModCmntTrailingFixed" 
      | ModCmntTrailingRelativeTyp 
      => RETURN "ModCmntTrailingRelative" 
      ELSE 
        RETURN "ModCmntAbstract" 
      END (* TYPECASE *) 
    END ModCmntKindImage 

(* VISIBLE: *) 
; PROCEDURE ModCmntImage 
    ( Self : ModCmntTyp ; <* UNUSED *> Indent := LbeStd . StdIndent ) : TEXT 

  = BEGIN (* ModCmntImage *) 
      RETURN 
        Self . KindImage ( ) 
        & "{" 
        & LbeStd . ScanStateImage ( Self . ModCmntBegScanState ) 
        & "," 
        & LbeStd . ScanStateImage ( Self . ModCmntEndScanState ) 
        & "," 
        & LbeStd . LimitedCharNoImage ( Self . ModCmntFromPos ) 
        & "," 
        & Misc . BooleanImageShort ( Self . ModCmntNlBefore ) 
        & "," 
        & Misc . BooleanImageShort ( Self . ModCmntNlAfter ) 
        & "," 
        & SharedStrings . Image ( Self . ModCmntStringRef , 0 ) 
        & "}" 
    END ModCmntImage 

(* VISIBLE: *) 
; PROCEDURE CopyOfModCmnt ( Old : ModCmntTyp ) : ModCmntTyp 
  RAISES { AssertionFailure } 
  (* Make a copy. *) 

  = VAR LResult : ModCmntTyp 

  ; BEGIN (* CopyOfModCmnt *) 
      TYPECASE Old 
      OF NULL 
      => RETURN NIL 
      | ModCmntLeadingFixedTyp 
      => LResult := NEW ( ModCmntLeadingFixedTyp ) 
      | ModCmntLeadingRelativeTyp 
      => LResult := NEW ( ModCmntLeadingRelativeTyp ) 
      | ModCmntTrailingFixedTyp 
      => LResult := NEW ( ModCmntTrailingFixedTyp ) 
      | ModCmntTrailingRelativeTyp 
      => LResult := NEW ( ModCmntTrailingRelativeTyp ) 
      ELSE 
        CantHappen ( AFT . A_ModHsCopyOfModCmnt_AbstractModCmnt ) 
      END (* TYPECASE *) 
    ; LResult . ModCmntBegScanState := Old . ModCmntBegScanState 
    ; LResult . ModCmntEndScanState := Old . ModCmntEndScanState 
    ; LResult . ModCmntFromPos := Old . ModCmntFromPos 
    ; LResult . ModCmntNlBefore := Old . ModCmntNlBefore  
    ; LResult . ModCmntNlAfter := Old . ModCmntNlAfter 
    ; LResult . ModCmntStringRef := Old . ModCmntStringRef 
    ; RETURN LResult 
    END CopyOfModCmnt 

(* VISIBLE: *) 
; PROCEDURE ModTextImage 
    ( Self : ModTextTyp ; <* UNUSED *> Indent := LbeStd . StdIndent ) : TEXT 

  = BEGIN (* ModTextImage *) 
      RETURN 
        "ModText{" 
        & LbeStd . LimitedCharNoImage ( Self . ModTextLeftTokToPos ) 
        & "," 
        & LbeStd . LimitedCharNoImage ( Self . ModTextFromPos ) 
        & "," 
        & LbeStd . LimitedCharNoImage ( Self . ModTextToPos ) 
        & "," 
        & LbeStd . LimitedCharNoImage ( Self . ModTextOrigToPos ) 
        & "," 
        & SharedStrings . Image ( Self . ModTextStringRef , 0 ) 
        & "}" 
    END ModTextImage 

(* VISIBLE: *) 
; PROCEDURE CopyOfModText ( Old : ModTextTyp ) : ModTextTyp 
  (* Make a copy. *) 

  = BEGIN (* CopyOfModText *) 
      TYPECASE Old 
      OF NULL 
      => RETURN NIL 
      | ModTextTyp 
      => RETURN 
           NEW 
             ( ModTextTyp 
             , ModTextFromPos := Old . ModTextFromPos 
             , ModTextToPos := Old . ModTextToPos 
             , ModTextLeftTokToPos := Old . ModTextLeftTokToPos 
             , ModTextOrigToPos := Old . ModTextOrigToPos 
             , ModTextStringRef := Old . ModTextStringRef 
             ) 
      END (* TYPECASE *) 
    END CopyOfModText 

(* VISIBLE: *) 
; PROCEDURE EstDummyImage 
    ( Self : EstDummyTyp ; <* UNUSED *> Indent := LbeStd . StdIndent ) : TEXT 

  = BEGIN (* EstDummyImage *) 
      TYPECASE Self
      OF NULL 
      => RETURN "NIL"
      | EstDummyTokTyp ( TEstDummyTok ) 
      => RETURN 
          "EstDummyTok{" 
          & LbeStd . NumIdTokImage ( TEstDummyTok . Tok ) 
          & "," 
          & EstApproxChildCtImage ( TEstDummyTok . ApproxChildCt ) 
          & "}" 
      | EstDummyTempMarkTyp  
      => RETURN "EstDummyTempMark{} " 
      ELSE RETURN "EstDummy{} " 
      END (* TYPECASE *) 
    END EstDummyImage 

; BEGIN (* ModHs *) 
  END ModHs 
. 

