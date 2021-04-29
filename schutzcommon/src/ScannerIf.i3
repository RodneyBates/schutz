
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE ScannerIf 

(* Interface to language-specific lexical scanners for different languages.
   It's a bit strange, but allows incremental scanning to skip around.
   There are some assumptions about restartability of scanning, which not
   all languages conform to.  But all reasonable languages do. :-)
*)

(* These are all exported by ParseTrav.m3. *) 

; IMPORT LbeStd 
; IMPORT Strings 
; IMPORT SchutzCoroutine 

; FROM Assertions IMPORT AssertionFailure 

; TYPE Public = SchutzCoroutine . T OBJECT END (* OBJECT *) 

; TYPE ScanIfTyp <: Public 

(* Scanner calls these routines. *) 

(* ScanIf parameters in the scanner interface routines are 
   just a courtesy on the part of the scanner, which should 
   always just pass back what it got when it was initialized. *) 

; PROCEDURE GetInitialChars 
    ( ScanIf : ScanIfTyp 
    ; VAR ScanState : LbeStd . ScanStateTyp 
    ; VAR DeliverString : Strings . StringTyp 
    ; VAR AreAllBlanks : BOOLEAN 
    ) 
  RAISES { AssertionFailure } 
  (* Retrieve Initial information delivered to scanner. *) 

; PROCEDURE AccumSlice ( ScanIf : ScanIfTyp ; Slice : Strings . StringTyp ) 
  RAISES { AssertionFailure } 
  (* Accumulate Slice as part of the token being scanned. *) 
  (* AccumSlice runs on the scanner's coroutine, but may also resume 
     parse traverser. *) 

; PROCEDURE ConsumeChars 
    ( ScanIf : ScanIfTyp 
    ; VAR (* IN OUT *) ScanState : LbeStd . ScanStateTyp 
          (* ParseTrav can change the state among LbeStd . SsIdle
             and the various comment states. *) 
    ; ConsumedCt : Strings . StringSsTyp 
      (* ^Number of characters consumed. *) 
    ; VAR DeliverString : Strings . StringTyp 
      (* ^NextSlice delivered to scanner. *) 
    ; VAR AreAllBlanks : BOOLEAN 
    ) 
  RAISES { AssertionFailure } 
  (* Consume the first ConsumedCt of the substring last delivered to 
     the scanner. *) 
  (* ConsumeChars runs on the scanner's coroutine, 
     but resumes parse traverser *) 

; PROCEDURE DeliverTok 
    ( ScanIf : ScanIfTyp 
    ; VAR (* IN OUT *) ScanState : LbeStd . ScanStateTyp 
          (* ParseTrav can change the state among LbeStd . SsIdle, 
              and the various comment states. *) 
    ; ConsumedCt : Strings . StringSsTyp 
      (* ^Number of characters consumed. *) 
    ; Tok : LbeStd . TokTyp 
    ; VAR DeliverString : Strings . StringTyp 
      (* ^NextSlice delivered to scanner. *) 
    ; VAR AreAllBlanks : BOOLEAN 
    ; IsPlaceholder : BOOLEAN := FALSE 
    ) 
  RAISES { AssertionFailure } 
  (* Do what ConsumeChars does, plus deliver a token in Tok. *) 
  (* DeliverTok runs on the scanner's coroutine, 
     but resumes parse traverser *) 

; PROCEDURE NoteBeg 
    ( ScanIf : ScanIfTyp ; ScanState : LbeStd . ScanStateTyp ) 
  RAISES { AssertionFailure } 
  (* Note that the current character is the first of a token *) 
  (* NoteBeg runs on the scanner's coroutine, 
     but resumes parse traverser *) 

; PROCEDURE NoteBegPrevChar 
    ( ScanIf : ScanIfTyp ; ScanState : LbeStd . ScanStateTyp ) 
  RAISES { AssertionFailure } 
  (* Note that the previous character is the first of a token *) 
  (* NoteBegPrevChar runs on the scanner's coroutine, 
     but resumes parse traverser *) 

; PROCEDURE LexErr ( ScanIf : ScanIfTyp ; ErrCode : LbeStd . ErrCodeTyp ) 
  RAISES { AssertionFailure } 
  (* Attach a lexical error message to the current token *) 
  (* LexErr runs on the scanner's coroutine, 
     but resumes parse traverser *) 

; END ScannerIf 
. 
