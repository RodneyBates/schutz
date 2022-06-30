
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LangUtilLo 

(* Used to break up potential import cycle for LangUtil. *) 

; FROM Failures IMPORT Backout 
; FROM LbeStd IMPORT TokTyp , TokClassTyp 
; FROM LdlSemantics IMPORT LangInfoRefTyp 

; PROCEDURE TokClassFirstTok  
    ( LangInfo : LangInfoRefTyp ; TokClass : TokClassTyp ) : TokTyp 
  (* Lowest numbered token belonging to TokClass, not counting LbeStd
     builtin tokens. 
  *) 

; PROCEDURE TokClassLastTok  
    ( LangInfo : LangInfoRefTyp ; TokClass : TokClassTyp ) : TokTyp 
  (* Highest numbered token belonging to TokClass, not counting LbeStd
     builtin tokens.  Note that if the class is empty, this will be one
     less than the result of TokClassFirstTok.  
  *) 

; PROCEDURE TokHasClass 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ; TokClass : TokClassTyp ) 
  : BOOLEAN 

; PROCEDURE TokHasClassRange 
    ( LangInfo : LangInfoRefTyp 
    ; Tok : TokTyp 
    ; TokClassLo , TokClassHi : TokClassTyp 
    ) 
  : BOOLEAN 
  (* The class of Tok is in the range TokClassLo..TokClassHi. *) 

; PROCEDURE TokClass 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) : TokClassTyp 
  RAISES { Backout } 
  (* The TokClass that contains token Tok.  Works on builtin tokens too. *) 

; PROCEDURE ListTokOfPartialTok 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) : TokTyp 
  (* If Tok is a partial token, its corresponding list token
     (not trailing separator list token.) Otherwise, identity. 
  *) 

; PROCEDURE TokIsAbstract 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) : BOOLEAN  
  (* Includes VarTerm, AsPlus, AsStar, AsPlusTrailing, AsStarTrailing,
     and AsFixed.
  *) 

; PROCEDURE TrailingCounterpart 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) 
  : TokTyp
  (* If Tok has one, its trailing separator counterpart. Otherwise, identity. *)

; PROCEDURE NontrailingCounterpart 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) 
  : TokTyp
  (* If Tok is a list token that has trailing separators, the
     token that does not have them.  Otherwise, identity. 
  *) 

; END LangUtilLo 
. 

