
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE ScannerIfRep 

(* Selectively revealable info about ScannerIf data structure. *) 

; IMPORT LbeStd 
; IMPORT ScannerIf 
; IMPORT Strings 

(* Interface to Scanner *) 

; TYPE ScanResumeKindTyp 
    = { SrKindInitial , SrKindConsumeChars , SrKindNoteBeg 
      , SrKindNoteBegPrevChar 
      , SrKindLexErr , SrKindDeliverTok (* and also consume chars *) 
      } 

; CONST Brand = "ScannerIfTyp" 

; REVEAL ScannerIf . ScanIfTyp 
    = ScannerIf . Public 
        BRANDED 
          Brand 
          OBJECT 
            SifResumeKind : ScanResumeKindTyp 
          ; SifScanState : LbeStd . ScanStateTyp 
          ; SifConsumedCt : Strings . StringSsTyp 
          ; SifAccumStringTo : Strings . StringSsTyp 
          ; SifTok : LbeStd . TokTyp 
          ; SifPrevTok : LbeStd . TokTyp 
          ; SifDeliverString : Strings . StringTyp 
            (* ^String delivered to scanner. *) 
          ; SifLexErrCode : LbeStd . ErrCodeTyp 
          ; SifAccumString : Strings . StringTyp 
            (* ^Scanner puts chars of lexeme here. *) 
          ; SifAreAllBlanks : BOOLEAN 
            (* ^Redundant info about SifDeliverString. *) 
          ; SifIsPlaceholder : BOOLEAN 
          ; SifBegNoted : BOOLEAN 
          END (* OBJECT ScanIfTyp *) 

; END ScannerIfRep 
. 
