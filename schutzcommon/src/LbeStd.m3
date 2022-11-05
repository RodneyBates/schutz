
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* "File LbeStd.m3". Rodney M. Bates.  July 2001 *) 

MODULE LbeStd 

(* Shared declarations used by the whole lbe *) 

; IMPORT Fmt 

; IMPORT PortTypes 
; IMPORT Version 

(* EXPORTED: *) 
; PROCEDURE StdTokImage ( Tok : StdTokTyp ) : TEXT 
  (* Image of a standard token.  It is a valid Modula-3 identifier, but
     not a valid Ldl identifier.
  *) 

  = BEGIN 
      CASE Tok 
      OF Tok__Null => RETURN "Tok__Null" 
      | Tok__Ident => RETURN "Tok__Ident"  
      | Tok__StringLit => RETURN "Tok__StringLit"  
      | Tok__CharLit => RETURN "Tok__CharLit" 
      | Tok__IntLit => RETURN "Tok__IntLit" 
      | Tok__LongintLit => RETURN "Tok__LongintLit" 
      | Tok__RealLit => RETURN "Tok__RealLit" 
      | Tok__LongrealLit => RETURN "Tok__LongrealLit" 
      | Tok__ExtendedLit => RETURN "Tok__ExtendedLit" 
      | Tok__IdentModTok => RETURN "Tok__IdentModTok"  
      | Tok__StringLitModTok => RETURN "Tok__StringLitModTok" 
      | Tok__CharLitModTok => RETURN "Tok__CharLitModTok" 
      | Tok__IntLitModTok => RETURN "Tok__IntLitModTok" 
      | Tok__LongintLitModTok => RETURN "Tok__LongintLitModTok"  
      | Tok__RealLitModTok => RETURN "Tok__RealLitModTok"  
      | Tok__LongrealLitModTok => RETURN "Tok__LongrealLitModTok"  
      | Tok__ExtendedLitModTok => RETURN "Tok__ExtendedLitTok"  
      | Tok__LexErrChars => RETURN "Tok__LexErrChars" 
      | Tok__ForceSep => RETURN "Tok__ForceSep" 
      | Tok__Sep => RETURN "Tok__Sep" 
      | Tok__BadChars => RETURN "Tok__BadChars" 
      | Tok__Cmnt => RETURN "Tok__Cmnt" 
      | Tok__CmntAtEndOfLine => RETURN "Tok__CmntAtEndOfLine" 
      | Tok__ModText => RETURN "Tok__ModText" 
      | Tok__BlankLine => RETURN "Tok__BlankLine" 
      | Tok__Unknown => RETURN "Tok__Unknown" 
      | Tok__BegOfLine => RETURN "Tok__BegOfLine" 
      | Tok__BegOfImage => RETURN "Tok__BegOfImage" 
      | Tok__EndOfImage => RETURN "Tok__EndOfImage" 
      | Tok__Augment => RETURN "Tok__Augment" 
      | Tok__UniversalClass => RETURN "Tok__UniversalClass" 
      | Tok__Empty => RETURN "Tok__Empty" 
      | Tok__OptSingletonList => RETURN "Tok__OptSingletonList"
      END (* CASE *) 
    END StdTokImage 

(* EXPORTED: *) 
; PROCEDURE StdTokPlaceholderImage ( Tok : StdTokTyp ) : TEXT 

  = BEGIN 
      CASE Tok 
      OF Tok__Null => RETURN "«Tok__Null»" 
      | Tok__Ident => RETURN "«Tok__Ident»"  
      | Tok__StringLit => RETURN "«Tok__StringLit»"  
      | Tok__CharLit => RETURN "«Tok__CharLit»" 
      | Tok__IntLit => RETURN "«Tok__IntLit»" 
      | Tok__LongintLit => RETURN "«Tok__LongintLit»" 
      | Tok__RealLit => RETURN "«Tok__RealLit»" 
      | Tok__LongrealLit => RETURN "«Tok__LongrealLit»" 
      | Tok__ExtendedLit => RETURN "«Tok__ExtendedLit»" 
      | Tok__IdentModTok => RETURN "«Tok__IdentModTok»"  
      | Tok__StringLitModTok => RETURN "«Tok__StringLitModTok»" 
      | Tok__CharLitModTok => RETURN "«Tok__CharLitModTok»" 
      | Tok__IntLitModTok => RETURN "«Tok__IntLitModTok»" 
      | Tok__LongintLitModTok => RETURN "«Tok__LongintLitModTok»"  
      | Tok__RealLitModTok => RETURN "«Tok__RealLitModTok»"  
      | Tok__LongrealLitModTok => RETURN "«Tok__LongrealLitModTok»"  
      | Tok__ExtendedLitModTok => RETURN "«Tok__ExtendedLitTok»"  
      | Tok__LexErrChars => RETURN "«Tok__LexErrChars»" 
      | Tok__ForceSep => RETURN "«Tok__ForceSep»" 
      | Tok__Sep => RETURN "«Tok__Sep»" 
      | Tok__BadChars => RETURN "«Tok__BadChars»" 
      | Tok__Cmnt => RETURN "«Tok__Cmnt»" 
      | Tok__CmntAtEndOfLine => RETURN "«Tok__CmntAtEndOfLine»" 
      | Tok__ModText => RETURN "«Tok__ModText»" 
      | Tok__BlankLine => RETURN "«Tok__BlankLine»" 
      | Tok__Unknown => RETURN "«Tok__Unknown»" 
      | Tok__BegOfLine => RETURN "«Tok__BegOfLine»" 
      | Tok__BegOfImage => RETURN "«Tok__BegOfImage»" 
      | Tok__EndOfImage => RETURN "«Tok__EndOfImage»" 
      | Tok__Augment => RETURN "«Tok__Augment»" 
      | Tok__UniversalClass => RETURN "«Tok__UniversalClass»" 
      | Tok__Empty => RETURN "«Tok__Empty»" 
      | Tok__OptSingletonList => RETURN "«Tok__OptSingletonList»"
      END (* CASE *) 
    END StdTokPlaceholderImage 

(* EXPORTED: *) 
; PROCEDURE TokClassPred ( Class : TokClassTyp ) : TokClassTyp 
  (* Predecessor function on TokClassTyp.  Will crash on FIRST(TokClassTyp) *) 

  = BEGIN 
      RETURN VAL ( ORD ( Class ) - 1 , TokClassTyp ) 
    END TokClassPred 

(* EXPORTED: *) 
; PROCEDURE TokClassSucc ( Class : TokClassTyp ) : TokClassTyp 
  (* Successor function on TokClassTyp.  Will crash on LAST(TokClassTyp) *) 

  = BEGIN 
      RETURN VAL ( ORD ( Class ) + 1 , TokClassTyp ) 
    END TokClassSucc 

(* EXPORTED: *) 
; PROCEDURE TokClassImage ( TokClass : TokClassTyp ) : TEXT 
  (* Unqualified enumeration constant identifier of TokClass. *) 

  = BEGIN 
      CASE TokClass 
      OF TokClassTyp . TokClassNull => RETURN "TokClassNull"
      | TokClassTyp . TokClassMisc => RETURN "TokClassMisc"
      | TokClassTyp . TokClassBuiltin => RETURN "TokClassBuiltin"
      | TokClassTyp . TokClassConstTerm => RETURN "TokClassConstTerm"
      | TokClassTyp . TokClassVarTerm => RETURN "TokClassVarTerm"
      | TokClassTyp . TokClassVarTermMod => RETURN "TokClassVarTermMod"
      | TokClassTyp . TokClassAsPlus => RETURN "TokClassAsPlus"
      | TokClassTyp . TokClassAsStar => RETURN "TokClassAsStar"
      | TokClassTyp . TokClassAsPlusTrailing => RETURN "TokClassAsPlusTrailing"
      | TokClassTyp . TokClassAsStarTrailing => RETURN "TokClassAsStarTrailing"
      | TokClassTyp . TokClassAsFixed => RETURN "TokClassAsFixed"
      | TokClassTyp . TokClassSublist => RETURN "TokClassSublist"
      | TokClassTyp . TokClassListCard => RETURN "TokClassListCard"
      | TokClassTyp . TokClassPartial => RETURN "TokClassPartial"
      | TokClassTyp . TokClassAsClass => RETURN "TokClassAsClass"
      | TokClassTyp . TokClassAsCsClass => RETURN "TokClassAsCsClass"
      | TokClassTyp . TokClassCsClass => RETURN "TokClassCsClass"
      | TokClassTyp . TokClassCsPlus => RETURN "TokClassCsPlus"
      | TokClassTyp . TokClassCsPlural => RETURN "TokClassCsPlural"
      | TokClassTyp . TokClassCsStar => RETURN "TokClassCsStar"
      | TokClassTyp . TokClassCsFixed => RETURN "TokClassCsFixed"
      | TokClassTyp . TokClassCsGen => RETURN "TokClassCsGen"
      | TokClassTyp . TokClassUnused => RETURN "TokClassUnused"
      END (* CASE *) 
   END TokClassImage 

(* EXPORTED: *) 
; PROCEDURE LimitedTokCtSum 
    ( Left , Right : LimitedTokCtTyp ) : LimitedTokCtTyp 

  = BEGIN (* LimitedTokCtSum *) 
      IF Left = ParseCheckInfinity 
         OR Right = ParseCheckInfinity 
         OR ParseCheckInfinity - Left <= Right 
      THEN 
        RETURN ParseCheckInfinity 
      ELSE 
        RETURN Left + Right 
      END (* IF *) 
    END LimitedTokCtSum 

(* EXPORTED: *) 
; PROCEDURE IncLimitedTokCt 
    ( VAR Left : LimitedTokCtTyp 
    ; Right : LimitedTokCtTyp := 1 
    ) 

  = BEGIN (* IncLimitedTokCt *) 
      Left := LimitedTokCtSum ( Left , Right ) 
    END IncLimitedTokCt 

(* EXPORTED: *) 
; PROCEDURE ProjectToLimitedCharNoTyp  ( Value : PortTypes . Int32Typ ) 
  : LimitedCharNoTyp 

  = BEGIN
      RETURN 
        MIN ( MAX ( FIRST ( LimitedCharNoTyp ) , Value ) , LimitedCharNoMax ) 
    END ProjectToLimitedCharNoTyp  

(* EXPORTED: *) 
; PROCEDURE NumIdTokImage ( Tok : TokTyp ) : TEXT 

  = BEGIN 
      RETURN "Tok__" & NumTokImage ( Tok ) 
    END NumIdTokImage 

(* EXPORTED: *) 
; PROCEDURE ScanStateImage ( Value : ScanStateTyp ) : TEXT 

  = BEGIN (* ScanStateImage *) 
      CASE Value 
      OF SsIdle 
      => RETURN "SsIdle" 
      | SsInTok 
      => RETURN "SsInTok" 
      | SsInCmnt 
      => RETURN "SsInCmnt" 
      | SsMax 
      => RETURN "SsMax" 
      | SsMaxCmnt 
      => RETURN "SsMaxCmnt" 
      ELSE 
        RETURN PortTypes . Int32Image ( Value ) 
      END (* CASE *) 
    END ScanStateImage 

(* EXPORTED: *)
; PROCEDURE VersionImage ( Version : VersionTyp ) : TEXT
 
  = BEGIN (* VersionImage *)
      RETURN
        Fmt . Int ( Version . Incompatible ) & "."
        & Fmt . Int ( Version . UpwardCompatible )
        & "."
        & Fmt . Int ( Version . Compatible )
        & "."
        & Fmt . Int ( Version . Minor )
    END VersionImage

(* EXPORTED: *)
; PROCEDURE PickleKindImage ( Kind : PickleKindTyp ) : TEXT 
 
  = BEGIN (* PickleKindImage *)
      CASE Kind 
      OF PickleKindTyp . Null => RETURN "Null" 
      | PickleKindTyp . Est => RETURN "Tree" 
      | PickleKindTyp . Lang => RETURN "Language" 
      | PickleKindTyp . Sem => RETURN "Semantic" 
      | PickleKindTyp . Image => RETURN "Image" 
      | PickleKindTyp . Checkpoint => RETURN "Checkpoint" 
      END (* CASE *)        
    END PickleKindImage

; BEGIN (* LbeStd *) 
    EstPickleIdInfoRef 
      := NEW ( PickleIdInfoRefTyp 
             , Magic := SchutzMagic 
             , DSVersion := Version . DataStructureVersion 
             , Kind := PickleKindTyp . Est  
             ) 
  ; SemPickleIdInfoRef 
      := NEW ( PickleIdInfoRefTyp 
             , Magic := SchutzMagic 
             , DSVersion := Version . DataStructureVersion 
             , Kind := PickleKindTyp . Sem  
             ) 
  ; LangPickleIdInfoRef 
      := NEW ( PickleIdInfoRefTyp 
             , Magic := SchutzMagic 
             , DSVersion := Version . DataStructureVersion 
             , Kind := PickleKindTyp . Lang  
             ) 
  ; ImagePickleIdInfoRef 
      := NEW ( PickleIdInfoRefTyp 
             , Magic := SchutzMagic 
             , DSVersion := Version . DataStructureVersion 
             , Kind := PickleKindTyp . Image 
             ) 
  ; CheckpointPickleIdInfoRef 
      := NEW ( PickleIdInfoRefTyp 
             , Magic := SchutzMagic 
             , DSVersion := Version . DataStructureVersion 
             , Kind := PickleKindTyp . Checkpoint 
             ) 
  END LbeStd 
. 
