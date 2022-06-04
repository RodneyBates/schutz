
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LangUtilLo 

(* Used to break up potential import cycle for LangUtil. *) 

; IMPORT Assertions 
; IMPORT LbeStd 
; FROM LbeStd IMPORT 
    TokTyp , TokClassTyp 
    , TokClassVarTermPred , TokClassAsPlusPred 
; IMPORT MessageCodes 
      
; FROM LdlSemantics IMPORT LangInfoRefTyp

; TYPE AFT = MessageCodes . T 

(* Unqualified renames of enumeration values. *) 

; CONST TokClassNull = LbeStd . TokClassTyp . TokClassNull
; CONST TokClassBuiltin = LbeStd . TokClassTyp . TokClassBuiltin
; CONST TokClassMisc = LbeStd . TokClassTyp . TokClassMisc
; CONST TokClassConstTerm = LbeStd . TokClassTyp . TokClassConstTerm
; CONST TokClassVarTerm = LbeStd . TokClassTyp . TokClassVarTerm
; CONST TokClassVarTermMod = LbeStd . TokClassTyp . TokClassVarTermMod
; CONST TokClassAsPlus = LbeStd . TokClassTyp . TokClassAsPlus
; CONST TokClassAsPlusTrailing = LbeStd . TokClassTyp . TokClassAsPlusTrailing
; CONST TokClassAsStarTrailing = LbeStd . TokClassTyp . TokClassAsStarTrailing
; CONST TokClassAsStar = LbeStd . TokClassTyp . TokClassAsStar
; CONST TokClassAsFixed = LbeStd . TokClassTyp . TokClassAsFixed
; CONST TokClassSublist = LbeStd . TokClassTyp . TokClassSublist
; CONST TokClassListCard = LbeStd . TokClassTyp . TokClassListCard
; CONST TokClassPartial = LbeStd . TokClassTyp . TokClassPartial
; CONST TokClassAsClass = LbeStd . TokClassTyp . TokClassAsClass
; CONST TokClassAsCsClass = LbeStd . TokClassTyp . TokClassAsCsClass
; CONST TokClassCsClass = LbeStd . TokClassTyp . TokClassCsClass
; CONST TokClassCsPlus = LbeStd . TokClassTyp . TokClassCsPlus
; CONST TokClassCsPlural = LbeStd . TokClassTyp . TokClassCsPlural
; CONST TokClassCsStar = LbeStd . TokClassTyp . TokClassCsStar
; CONST TokClassCsFixed = LbeStd . TokClassTyp . TokClassCsFixed
; CONST TokClassCsGen = LbeStd . TokClassTyp . TokClassCsGen
; CONST TokClassUnused = LbeStd . TokClassTyp . TokClassUnused

(* VISIBLE: *) 
; PROCEDURE TokClassFirstTok  
    ( LangInfo : LangInfoRefTyp ; TokClass : TokClassTyp ) : TokTyp 
  (* Lowest numbered token belonging to TokClass, not counting LbeStd
     builtin tokens. 
  *) 

  = BEGIN 
      RETURN LangInfo . TokPart ^ [ LbeStd . TokClassPred ( TokClass ) ] 
    END TokClassFirstTok 

(* VISIBLE: *) 
; PROCEDURE TokClassLastTok  
    ( LangInfo : LangInfoRefTyp ; TokClass : TokClassTyp ) : TokTyp 
  (* Highest numbered token belonging to TokClass, not counting LbeStd
     builtin tokens.  Note that if the class is empty, this will be one
     less than the result of TokClassFirstTok.  
  *) 

  = BEGIN 
      RETURN LangInfo . TokPart ^ [ TokClass ] - 1 
    END TokClassLastTok 

(* VISIBLE: *) 
; PROCEDURE TokHasClass 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ; TokClass : TokClassTyp ) 
  : BOOLEAN 

  = BEGIN 
      CASE Tok 
      OF 
(* FIXME: It may not matter, but get builtin tokens into this. *) 
      ELSE 
        RETURN 
          LangInfo . TokPart ^ [ LbeStd . TokClassPred ( TokClass ) ] <= Tok 
          AND Tok < LangInfo . TokPart ^ [ TokClass ] 
      END (* CASE *) 
    END TokHasClass

(* VISIBLE: *) 
; PROCEDURE TokHasClassRange 
    ( LangInfo : LangInfoRefTyp 
    ; Tok : TokTyp 
    ; TokClassLo , TokClassHi : TokClassTyp 
    ) 
  : BOOLEAN 
  (* The class of Tok is in the range TokClassLo..TokClassHi. *) 

  = BEGIN 
      CASE Tok 
      OF 
(* FIXME: It may not matter, but get builtin tokens into this. *) 
      ELSE 
        RETURN 
          LangInfo . TokPart ^ [ LbeStd . TokClassPred ( TokClassLo ) ] <= Tok 
          AND Tok < LangInfo . TokPart ^ [ TokClassHi ] 
      END (* CASE *) 
    END TokHasClassRange 

(* VISIBLE: *) 
; PROCEDURE TokClass 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) : TokClassTyp 
  RAISES { Assertions . AssertionFailure } 
  (* The TokClass that contains token Tok.  Works on builtin tokens too. *) 

  = VAR LLo , LHi , LProbe , LPred : TokClassTyp 

  ; BEGIN 
      CASE Tok  
      OF LbeStd . Tok__Null 
      => RETURN TokClassNull 
      | LbeStd . Tok__Ident 
      , LbeStd . Tok__StringLit 
      , LbeStd . Tok__CharLit 
      , LbeStd . Tok__IntLit 
      , LbeStd . Tok__LongintLit 
      , LbeStd . Tok__RealLit 
      , LbeStd . Tok__LongrealLit 
      , LbeStd . Tok__ExtendedLit 
      => RETURN TokClassVarTerm 
      | LbeStd . Tok__IdentModTok 
      , LbeStd . Tok__StringLitModTok 
      , LbeStd . Tok__CharLitModTok 
      , LbeStd . Tok__IntLitModTok 
      , LbeStd . Tok__LongintLitModTok 
      , LbeStd . Tok__RealLitModTok 
      , LbeStd . Tok__LongrealLitModTok 
      , LbeStd . Tok__ExtendedLitModTok 
      => RETURN TokClassVarTermMod 
      | LbeStd . Tok__LexErrChars 
      , LbeStd . Tok__ForceSep 
      , LbeStd . Tok__Sep 
      , LbeStd . Tok__BadChars 
      , LbeStd . Tok__Cmnt 
      , LbeStd . Tok__CmntAtEndOfLine 
      , LbeStd . Tok__ModText 
      , LbeStd . Tok__BlankLine 
      , LbeStd . Tok__Unknown 
      , LbeStd . Tok__BegOfLine 
      => RETURN TokClassMisc 
      | LbeStd . Tok__BegOfImage 
      , LbeStd . Tok__EndOfImage 
      => RETURN TokClassConstTerm 
      | LbeStd . Tok__Augment 
      => RETURN TokClassAsFixed 
      | LbeStd . Tok__UniversalClass
      => RETURN TokClassAsCsClass 
      ELSE 
        LLo := FIRST ( TokClassTyp ) 
      ; LHi := LAST ( TokClassTyp ) 
      ; WITH WPart = LangInfo . TokPart ^ 
        DO LOOP 
            IF LLo > LHi 
            THEN Assertions . CantHappen 
                   ( AFT . A_LangUtilLo_TokClass_NotFound ) 
            ELSE 
              LProbe 
                := VAL ( ( ORD ( LLo ) + ORD ( LHi ) ) DIV 2 , TokClassTyp ) 
            ; IF Tok >= WPart [ LProbe ] 
              THEN (* Tok is in a higher class. *) 
                LLo := LbeStd . TokClassSucc ( LProbe )  
              ELSIF LProbe = FIRST ( TokClassTyp )
              THEN (* This is the lowest class, Tok has to be in it. *) 
                RETURN LProbe 
              ELSE
                LPred := LbeStd . TokClassPred ( LProbe ) 
              ; IF Tok < WPart [ LPred ] 
                THEN (* Tok is in a lower class. *) 
                  LHi := LPred 
                ELSE (* Tok is in this class. *)  
                  RETURN LProbe 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END (* LOOP *) 
        END(* WITH *) 
      END (* IF *) 
    END TokClass 

(* VISIBLE: *) 
; PROCEDURE ListTokOfPartialTok 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) : TokTyp 
  (* If Tok is a partial token, its corresponding list token
     Otherwise, identity. 
  *) 

  = VAR LResultTok : TokTyp 

  ; BEGIN 
      IF TokHasClass ( LangInfo , Tok , TokClassPartial )
      THEN (* It's a partial token.  Get the corresponding list token. *)
        LResultTok 
          := Tok 
             - TokClassFirstTok ( LangInfo , TokClassPartial ) 
             + TokClassFirstTok ( LangInfo , TokClassAsPlus ) 
      ELSE LResultTok := Tok 
      END (* IF *) 
    ; RETURN LResultTok 
    END ListTokOfPartialTok 

(* VISIBLE: *) 
; PROCEDURE TokIsAbstract 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) : BOOLEAN  
  (* Includes VarTerm, AsPlus, AsStar, AsPlusTrailing, AsStarTrailing,
     and AsFixed.
  *) 

  = BEGIN 
      IF LangInfo . TokPart ^ [ TokClassVarTermPred ] <= Tok 
         AND Tok < LangInfo . TokPart ^ [ TokClassVarTerm ] 
      THEN (* VarTerm *) 
        RETURN TRUE 
      ELSIF LangInfo . TokPart ^ [ TokClassAsPlusPred ] <= Tok 
            AND Tok < LangInfo . TokPart ^ [ TokClassAsFixed ] 
      THEN (* Other Ast classes. *) 
        RETURN TRUE 
      ELSE RETURN FALSE
      END (* IF *) 
    END TokIsAbstract

(* VISIBLE: *)
; PROCEDURE IsTrailingTok 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) : BOOLEAN 
  (* Tok is a list token that has trailing separators. *) 

  = BEGIN 
      RETURN 
        TokHasClassRange 
          ( LangInfo , Tok , TokClassAsPlusTrailing , TokClassAsStarTrailing ) 
    END IsTrailingTok 

(* VISIBLE: *)
; PROCEDURE TrailingCounterpart 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) 
  : LbeStd . TokTyp
  (* If Tok has one, its trailing separator counterpart. Otherwise, identity. *)

  = VAR LPFirst , LSFirst , LPTFirst , LResultTok : TokTyp 

  ; BEGIN 
      LPFirst := TokClassFirstTok ( LangInfo , TokClassAsPlus )
    ; IF Tok < LPFirst 
      THEN (* Not a list token. *) 
        RETURN Tok 
      ELSE
        LSFirst := TokClassFirstTok ( LangInfo , TokClassAsStar )
      ; LPTFirst := TokClassFirstTok ( LangInfo , TokClassAsPlusTrailing )
      ; IF Tok < LSFirst 
        THEN (* TokClassAsPlus *) 
          LResultTok := Tok - LPFirst + LPTFirst 
        ; IF LResultTok <= TokClassLastTok ( LangInfo , TokClassAsPlusTrailing )
          THEN (* This is an AsPlusTrailing token. *) 
            RETURN LResultTok 
          ELSE RETURN Tok 
          END (* IF *) 
        ELSIF Tok < LPTFirst 
        THEN (* TokClassAsStar *) 
          LResultTok 
            := Tok 
               - LSFirst 
               + TokClassFirstTok ( LangInfo , TokClassAsStarTrailing )
        ; IF LResultTok <= TokClassLastTok ( LangInfo , TokClassAsStarTrailing )
          THEN (* This is an AsStarTrailing token. *) 
            RETURN LResultTok 
          ELSE RETURN Tok 
          END (* IF *) 
        ELSE RETURN Tok 
        END (* IF *) 
      END (* IF *) 
    END TrailingCounterpart 

(* VISIBLE: *)
; PROCEDURE NontrailingCounterpart 
    ( LangInfo : LangInfoRefTyp ; Tok : TokTyp ) 
  : LbeStd . TokTyp
  (* If Tok is a list token that has trailing separators, the
     token that does not have them.  Otherwise, identity. 
  *) 

  = VAR LPTFirst , LSTFirst : TokTyp 

  ; BEGIN 
      LPTFirst := TokClassFirstTok ( LangInfo , TokClassAsPlusTrailing )
    ; IF Tok < LPTFirst 
      THEN (* Not a trailing-separator token. *) 
        RETURN Tok 
      ELSE
        LSTFirst := TokClassFirstTok ( LangInfo , TokClassAsStarTrailing )
      ; IF Tok < LSTFirst 
        THEN (* TokClassAsPlusTrailing *) 
          RETURN Tok - LPTFirst + TokClassFirstTok ( LangInfo , TokClassAsPlus )
        ELSIF Tok <= TokClassLastTok ( LangInfo , TokClassAsStarTrailing ) 
        THEN (* TokClassAsStarTrailing *) 
          RETURN Tok - LSTFirst + TokClassFirstTok ( LangInfo , TokClassAsStar )
        ELSE (* Not a trailing-separator token. *) 
          RETURN Tok 
        END (* IF *) 
      END (* IF *) 
    END NontrailingCounterpart 

; BEGIN 
  END LangUtilLo 
. 

