
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LangUtil

(* Utilities concerning the definition of the language being edited.  
   This is intended to contain things that are needed for actual editing
   and not just language generation.  But it seems to have gotten a little
   mixed up with things in LdlSemantics. 
*) 

; IMPORT Fmt
; IMPORT Pathname 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr
; FROM Wr IMPORT EOL 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , AssertionFailure 
; IMPORT Coroutine
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT Files 
; IMPORT LangMap
; IMPORT LangUtilLo 
; IMPORT LangUtilRep
; IMPORT LbeStd
; IMPORT LdlSemantics
; IMPORT LRTable
; IMPORT MessageCodes 
; IMPORT Messages 
; IMPORT Misc
; IMPORT Options 
; IMPORT PortTypes
; IMPORT ScannerIf
; IMPORT SharedStrings
; IMPORT SuffixInfo
; IMPORT TokRelation
; IMPORT TextSuffixInfoTbl
; IMPORT UncertainBool 

; TYPE AFT = MessageCodes . T 

; CONST DL = Messages . StdErrLine 

; CONST UbTrue = UncertainBool . T . True  

(* Unqualified renames of TokClass values. *) 

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

(* Identification of languages: *)

(* VISIBLE: *)
; PROCEDURE LangIdRef ( Lang : LbeStd . LangTyp ) : LangIdRefTyp 
  (* NIL, if language is not in LangMap. *) 

  = VAR LLangInfoRef
    : LdlSemantics . LangInfoRefTyp

  ; BEGIN
      LLangInfoRef := LangMap . LangInfo ( Lang )
    ; IF LLangInfoRef = NIL
      THEN (* This happens during Ldl bootstrap *)
        RETURN NIL
      ELSE
        RETURN LLangInfoRef . DefLangIdRef
      END (* IF *)
    END LangIdRef

; PROCEDURE LangIdImage
    ( READONLY LangId : LangIdTyp ; Indent : PortTypes . Int32Typ := 0
    )
  : TEXT

  = BEGIN (* LangIdImage *)
      RETURN
        "Language    " & LangId . LangName & EOL
        & Fmt . Pad ( "" , Indent )
        & "Version     "
        & LbeStd . VersionImage ( LangId . LangVersion )
        & EOL
        & Fmt . Pad ( "" , Indent )
        & "Date        "
        & Misc . DateImage ( LangId . LangDate )
        & EOL
        & Fmt . Pad ( "" , Indent )
        & "Fingerprint "
        & Misc . FingerprintImage ( LangId . LangFingerprint )
    END LangIdImage

(* File suffixes: *)

(* VISIBLE: *)
; PROCEDURE LangSuffixOfFileName ( FileName : TEXT ) : TEXT 
(* Can be a full path name.  From the right, removes version suffixes and 
   underscore suffixes.  From the left, removes all but the characters 
   following the last dot.
*)  
   
  = VAR LResult : TEXT 

  ; BEGIN 
      LResult 
        := Misc . TextName 
             ( Pathname . LastExt ( Misc . WoVersionSuffix ( FileName ) ) ) 
    ; RETURN LResult 
    END LangSuffixOfFileName

(* We have an awkward catch-22 on file extensions and the languages they
   correspond to.  We want the language definition, starting with LDL, to
   specify the suffix(es) of the language.  But we also want to avoid
   eagerly loading every known language specification.  We want to load
   them lazily when the user opens a file that needs the language.  But
   we don't know its suffixes.  

   So, the main mechanism for file suffixes uses the LDL specification,
   but if we try to open a file with an unknown suffix, we fall back on
   a separate, hard-coded map of suffixes to choose a language description
   file name, try loading it, and hope it defines the suffix we wanted.

   If the user wants to use a language whose suffixes are not hard-coded,
   she will have to manually load the language definition.
*)  

; VAR SuffixMap : TextSuffixInfoTbl . T
  (* ^The map of suffixes as specified by language definitions. 
     Only fields Tok and Lang are used.
  *) 

; VAR HardSuffixMap : TextSuffixInfoTbl . T 
  (* Map of hard-coded suffixes for languages pre-known to Schutz. 
     Only fields FileName and Lang are used. 
  *) 

; PROCEDURE InitOneHardLang 
    ( Suffix , FileName : TEXT ; Lang : LbeStd . LangTyp ) 

  = VAR LInfo : SuffixInfo . T 

  ; BEGIN
      LInfo := SuffixInfo . Null 
    ; LInfo . FileName := FileName 
    ; LInfo . Lang := Lang 
    ; EVAL ( HardSuffixMap . put ( Suffix , LInfo ) )   
    END InitOneHardLang

; PROCEDURE InitHardSuffixMap ( ) 

  = BEGIN 
      HardSuffixMap 
        := NEW ( TextSuffixInfoTbl . Default ) . init ( sizeHint := 6 )
    ; InitOneHardLang ( "ldl0" , "Ldl0Sem.pkl" , LbeStd . LangLdl0 ) 
    ; InitOneHardLang ( "ldl1" , "Ldl1Sem.pkl" , LbeStd . LangLdl1 ) 
    ; InitOneHardLang ( "i3" , "M3Sem.pkl" , LbeStd . LangM3 ) 
    ; InitOneHardLang ( "m3" , "M3Sem.pkl" , LbeStd . LangM3 ) 
    ; InitOneHardLang ( "ig" , "M3Sem.pkl" , LbeStd . LangM3 ) 
    ; InitOneHardLang ( "mg" , "M3Sem.pkl" , LbeStd . LangM3 ) 
    END InitHardSuffixMap 

; PROCEDURE GetHardSuffixInfo 
    ( Suffix : TEXT ; VAR FileName : TEXT ; VAR Lang : LbeStd . LangTyp ) 
  : BOOLEAN (* Suffix found. *) 

  = VAR LInfo : SuffixInfo . T 

  ; BEGIN 
      IF HardSuffixMap . get ( Suffix , (* VAR *) LInfo ) 
      THEN
        FileName := LInfo . FileName 
      ; Lang := LInfo . Lang 
      ; RETURN TRUE  
      ELSE 
        RETURN FALSE 
      END (* IF *) 
    END GetHardSuffixInfo 

(* VISIBLE: *)
; PROCEDURE RetSuffixInfo ( Suffix : TEXT ) : SuffixInfo . T

  = VAR LInfo := SuffixInfo . Null

  ; BEGIN (* RetSuffixInfo *)
      EVAL SuffixMap . get ( Misc . TextName ( Suffix ) , LInfo )
    ; LInfo . IsPickle := Misc . IsPickleName ( Suffix )
    ; RETURN LInfo
    END RetSuffixInfo

(* VISIBLE: *)
; PROCEDURE Suffix ( Info : SuffixInfo . T ) : TEXT

  = VAR LI : INTEGER

  ; BEGIN (* Suffix *)
      WITH WLangInfoRef = LangMap . LangInfo ( Info . Lang )
      DO
        IF WLangInfoRef # NIL AND WLangInfoRef . Suffixes # NIL
        THEN
          LI := 0
        ; LOOP
            IF LI = NUMBER ( WLangInfoRef . Suffixes ^ )
            THEN
              RETURN ""
            ELSE
              WITH WPair = WLangInfoRef . Suffixes ^ [ LI ]
              DO
                IF WPair . Tok = Info . Tok
                THEN
                  IF Info . IsPickle
                  THEN
                    RETURN Misc . PickleName ( WPair . Suffix )
                  ELSE
                    RETURN Misc . TextName ( WPair . Suffix )
                  END (* IF *)
                ELSE
                  INC ( LI )
                END (* IF *)
              END (* WITH *)
            END (* IF *)
          END (* LOOP *)
        ELSE
          RETURN ""
        END (* IF *)
      END (* WITH *)
    END Suffix

(* VISIBLE: *)
; PROCEDURE MergeSuffixes ( Lang : LbeStd . LangTyp )
  RAISES { DuplicateSuffix }
(* ^Call this after adding a new language to LangMap. *)

  = VAR LInfo : SuffixInfo . T

  ; BEGIN (* MergeSuffixes *)
      WITH WLangInfoRef = LangMap . LangInfo ( Lang )
      DO IF WLangInfoRef # NIL AND WLangInfoRef ^ . Suffixes # NIL
        THEN
          FOR RI := 0 TO NUMBER ( WLangInfoRef . Suffixes ^ ) - 1
          DO WITH WPair = WLangInfoRef . Suffixes ^ [ RI ]
            DO IF SuffixMap . get ( WPair . Suffix , LInfo )
              THEN
                RAISE DuplicateSuffix
              ELSE
                LInfo . Lang := Lang
              ; LInfo . Tok := WPair . Tok
              ; LInfo . IsPickle := FALSE
              ; EVAL SuffixMap . put
                       ( Misc . TextName ( WPair . Suffix ) , LInfo )
              END (* IF *)
            END (* WITH *)
          END (* FOR *)
        END (* IF *)
      END (* WITH *)
    END MergeSuffixes

(* Language definitions. *) 

(* VISIBLE: *)
; PROCEDURE LoadLanguage ( Suffix : TEXT ) 
  RAISES { LangError , Thread . Alerted } 
  (* Ensure the language definition for the file suffix Suffix is loaded. *) 

  = VAR LInfo := SuffixInfo . Null
  ; VAR LPklFileName : TEXT 
  ; VAR LLangInfoRef : LdlSemantics . LangInfoRefTyp
  ; VAR LMsg : TEXT 
  ; VAR LLang : LbeStd . LangTyp 

  ; BEGIN (* LoadLanguage *)
      IF NOT SuffixMap . get ( Misc . TextName ( Suffix ) , (* VAR *) LInfo )
      THEN (* Language not already loaded.  Try to load a known language. *) 
        IF NOT GetHardSuffixInfo 
                 ( Suffix , (* VAR *) LPklFileName , (* VAR *) LLang ) 
        THEN 
          LMsg := "Unknown language for suffix \"" & Suffix & "\"" 
        ; RAISE LangError ( LMsg )
        ELSE 
          TRY 
            LLangInfoRef 
              := Files . ReadLangPickle 
                   ( LPklFileName , Options . ResourcePath ) 
          EXCEPT 
            Files . Error ( EMsg ) 
          => LMsg := EMsg & "," & EOL & "  for language with suffix \"" 
                     & Suffix & "\"" 
          ; RAISE LangError ( LMsg ) 
          END (* TRY EXCEPT *) 
        ; IF LLangInfoRef = NIL 
          THEN
            LMsg := "Unusable language file \"" & LPklFileName 
                    & "\", for suffix \"" & Suffix & "\""  
          ; RAISE LangError ( LMsg ) 
          ELSE 
            LangMap . AddOrChange ( LLang , LLangInfoRef ) 
          ; TRY 
              MergeSuffixes ( LLang ) 
            EXCEPT DuplicateSuffix 
            => LMsg := "Language " 
                       & LangIdImage ( LLangInfoRef ^ . DefLangIdRef ^ , 2 ) 
                       & "Duplicates existing file suffixes."
            ; RAISE LangError ( LMsg ) 
            END (* TRY EXCEPT *) 
          ; IF Options . LogMessages  
            THEN 
              DL ( "Language loaded:" ) 
            ; DL ( LangIdImage ( LLangInfoRef ^ . DefLangIdRef ^ , 2 ) ) 
            ; DL 
                ( "Unique shared strings: " 
                  & Fmt . Int ( SharedStrings . UniqueStringCt ( ) ) 
                ) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END LoadLanguage 

(* Managing lexical scanners. *) 

; TYPE RegisteredScannerTyp
  = RECORD
      ScannerProc : Coroutine . ProcOfT ; ScannerIf : ScannerIf . ScanIfTyp
    END

; CONST RegisteredScannerNull
    = RegisteredScannerTyp { ScannerProc := NIL , ScannerIf := NIL }

; TYPE RegisteredScannersTyp
  = ARRAY LbeStd . LangBuiltinTyp OF RegisteredScannerTyp

; VAR RegisteredScanners
  := RegisteredScannersTyp { RegisteredScannerNull , .. }

(* VISIBLE: *)
; PROCEDURE RegisterScanner
    ( Lang : LbeStd . LangBuiltinTyp ; Scanner : Coroutine . ProcOfT )

  = BEGIN
      WITH WRS = RegisteredScanners [ Lang ]
      DO
        WRS . ScannerProc := Scanner ; WRS . ScannerIf := NIL
      END (* WITH *)
    END RegisterScanner

(* VISIBLE: *)
; PROCEDURE ScannerIfForLang
    ( Lang : LbeStd . LangTyp
    )
  : ScannerIf . ScanIfTyp

  = BEGIN (* ScannerIfForLang *)
      IF FIRST ( LbeStd . LangBuiltinTyp ) <= Lang
         AND Lang <= LAST ( LbeStd . LangBuiltinTyp )
      THEN
        WITH WRS = RegisteredScanners [ Lang ]
        DO
          IF TRUE OR WRS . ScannerIf = NIL
             (* Must always reinitialize a coroutine, because now different
                threads can initiate a parse. *) 
          THEN
            WRS . ScannerIf
              := Coroutine . Init
                   ( NEW ( ScannerIf . ScanIfTyp ) , WRS . ScannerProc
                   )
          END (* IF *)
        ; RETURN WRS . ScannerIf
        END (* WITH *)
      ELSE
        RETURN NIL
      END (* IF *)
    END ScannerIfForLang

(* VISIBLE: *)
; PROCEDURE PredicateKindImage ( Value : PredicateKindTyp ) : TEXT 

  = BEGIN 
      CASE Value  
      OF PredicateKindTyp . PredicateKindNull   => RETURN "PredicateKindNull"
      | PredicateKindTyp . PredicateKindFalse   => RETURN "PredicateKindFalse"
      | PredicateKindTyp . PredicateKindTrue    => RETURN "PredicateKindTrue"
      | PredicateKindTyp . PredicateKindAbsent  => RETURN "PredicateKindAbsent"
      | PredicateKindTyp . PredicateKindPresent => RETURN "PredicateKindPresent"
      | PredicateKindTyp . PredicateKindEmptyList  
        => RETURN "PredicateKindEmptyList"
      | PredicateKindTyp . PredicateKindNonemptyList  
        => RETURN "PredicateKindNonemptyList"
      | PredicateKindTyp . PredicateKindPluralList  
        => RETURN "PredicateKindPluralList"
      | PredicateKindTyp . PredicateKindNonpluralList  
        => RETURN "PredicateKindNonpluralList"
      | PredicateKindTyp . PredicateKindInClass => RETURN "PredicateKindInClass"
      END (* CASE *) 
    END PredicateKindImage 

(* VISIBLE: *)
; PROCEDURE FsChildNoImage 
    ( Value : FsChildNoSignedTyp 
    ; IsInsideList : BOOLEAN := FALSE 
    ; MakeShort : BOOLEAN := FALSE 
    ) 
  : TEXT 

  = BEGIN 
      IF Value = FsChildNoNull 
      THEN
        IF MakeShort 
        THEN RETURN "N" 
        ELSE 
          RETURN "FsChildNoNull(" & PortTypes . Int32Image ( Value ) & ")" 
        END (* IF *) 
      ELSIF Value = FsChildNoAlt 
      THEN 
        IF MakeShort 
        THEN RETURN "A" 
        ELSE 
          RETURN "FsChildNoAlt(" & PortTypes . Int32Image ( Value ) & ")" 
        END (* IF *) 
      ELSIF IsInsideList AND Value = FsChildNoListEstChild 
      THEN 
        IF MakeShort 
        THEN RETURN "E" 
        ELSE 
          RETURN 
            "FsChildNoEstListChild(" & PortTypes . Int32Image ( Value ) & ")" 
        END (* IF *) 
      ELSE 
        RETURN PortTypes . Int32Image ( Value )  
      END (* IF *) 
    END FsChildNoImage 

(* VISIBLE: *)
; PROCEDURE FsKindImage ( Value : FsKindTyp ) : TEXT 

  = BEGIN 
      CASE Value 
      OF FsKindTyp . FsKindNull => RETURN "FsKindNull"
      | FsKindTyp . FsKindBegOfImage => RETURN "FsKindBegOfImage"
      | FsKindTyp . FsKindEndOfImage => RETURN "FsKindEndOfImage"
      | FsKindTyp . FsKindInsTok => RETURN "FsKindInsTok"
      | FsKindTyp . FsKindEstChildOfFixed => RETURN "FsKindEstChildOfFixed"
      | FsKindTyp . FsKindEstChildOfList => RETURN "FsKindEstChildOfList"
      | FsKindTyp . FsKindLineBreakOpt => RETURN "FsKindLineBreakOpt"
      | FsKindTyp . FsKindLineBreakReqd => RETURN "FsKindLineBreakReqd"
      | FsKindTyp . FsKindAstString => RETURN "FsKindAstString"
      | FsKindTyp . FsKindEstFixedVert => RETURN "FsKindEstFixedVert"
      | FsKindTyp . FsKindEstFixedHoriz => RETURN "FsKindEstFixedHoriz"
      | FsKindTyp . FsKindEstFixedFill => RETURN "FsKindEstFixedFill"
      | FsKindTyp . FsKindSubtreeVert => RETURN "FsKindSubtreeVert"
      | FsKindTyp . FsKindSubtreeHoriz => RETURN "FsKindSubtreeHoriz"
      | FsKindTyp . FsKindSubtreeFill => RETURN "FsKindSubtreeFill"
      | FsKindTyp . FsKindEstListVert => RETURN "FsKindEstListVert"
      | FsKindTyp . FsKindEstListHoriz => RETURN "FsKindEstListHoriz"
      | FsKindTyp . FsKindEstListFill => RETURN "FsKindEstListFill"
      | FsKindTyp . FsKindEstListTrailVert => RETURN "FsKindEstListTrailVert"
      | FsKindTyp . FsKindEstListTrailHoriz => RETURN "FsKindEstListTrailHoriz"
      | FsKindTyp . FsKindEstListTrailFill => RETURN "FsKindEstListTrailFill"
      | FsKindTyp . FsKindCondFmt => RETURN "FsKindCondFmt"
      END (* CASE *) 
    END FsKindImage 

(* VISIBLE: *)
; PROCEDURE FsNodeNoImage ( Value : FsNodeNoTyp ) : TEXT 

  = BEGIN 
      IF Value = FsNodeNoNull 
      THEN
        RETURN "FsNodeNoNull(" & PortTypes . Int32Image ( Value ) & ")" 
      ELSE 
        RETURN PortTypes . Int32Image ( Value )  
      END (* IF *) 
    END FsNodeNoImage 

(* TODO: Rename the FormatsEmpty things to reflect a general algebra.
         It is used for FsFormatsEmpty and FsHasLineBreak.  It behaves
         a bit like BOOLEAN, AND, and OR, with unknowns and maybes
         added.
*) 

(* VISIBLE: *)
; PROCEDURE FormatsEmptyImage ( Value : FormatsEmptyTyp ) : TEXT  
  
  = BEGIN 
      CASE Value 
      OF FormatsEmptyTyp . FeUnknown => RETURN "FeUnknown" 
      | FormatsEmptyTyp . FeNo => RETURN "FeNo"
      | FormatsEmptyTyp . FeMaybe => RETURN "FeMaybe"
      | FormatsEmptyTyp . FeYes => RETURN "FeYes"
      END (* CASE *) 
    END FormatsEmptyImage  

(* VISIBLE: *)
; PROCEDURE FormatsEmptyCat ( Left , Right : FormatsEmptyTyp ) 
    : FormatsEmptyTyp 
  (* Composite of this property for two concatenated subtrees. *) 

  (* "Truth" table for FormatsEmptyCat
                  FeUnknown FeNo      FeMaybe   FeYes 
                  ----------------------------------------     
      FeUnknown | FeUnknown FeNo      FeMaybe   FeYes 
      FeNo      | FeNo      FeNo      FeNo      FeNo  
      FeMaybe   | FeMaybe   FeNo      FeMaybe   FeMaybe
      FeYes     | FeYes     FeNo      FeMaybe   FeYes 
  
      FeUnknown is always superceded by the other operand.  
      Otherwise, for combinations not involving FeUnkown, 
      this happens, by clever design of the ordering of values, to be
      a MIN function. 
  *) 

  = BEGIN 
      IF Left = FormatsEmptyTyp . FeUnknown 
      THEN RETURN Right 
      ELSIF Right = FormatsEmptyTyp . FeUnknown
      THEN RETURN Left 
      ELSE RETURN MIN ( Left , Right ) 
      END (* IF *) 
    END FormatsEmptyCat 

(* VISIBLE: *)
; PROCEDURE FormatsEmptyAlt ( Left , Right : FormatsEmptyTyp ) 
    : FormatsEmptyTyp 
  (* Composite of this property for two alternative subtrees. *) 

  (* "Truth" table for FormatsEmptyAlt
                  FeUnknown FeNo      FeMaybe   FeYes 
                  ----------------------------------------     
      FeUnknown | FeUnknown FeNo      FeMaybe   FeYes 
      FeNo      | FeNo      FeNo      FeMaybe   FeYes 
      FeMaybe   | FeMaybe   FeMaybe   FeMaybe   FeYes 
      FeYes     | FeYes     FeYes     FeYes     FeYes 
  
      This happens, by clever design of the ordering of values, to be
      a MAX function. 
  *) 

  = BEGIN 
      RETURN MAX ( Left , Right ) 
    END FormatsEmptyAlt 

(* VISIBLE: *)
; PROCEDURE FmtKindImage ( Value : FmtKindTyp ) : TEXT

  = BEGIN (* FmtKindImage *)
      CASE Value
      OF FmtKindTyp . FmtKindUnknown => RETURN "FmtKindUnknown"
      |  FmtKindTyp . FmtKindVert => RETURN "FmtKindVert"
      |  FmtKindTyp . FmtKindHoriz => RETURN "FmtKindHoriz"
      |  FmtKindTyp . FmtKindFill => RETURN "FmtKindFill"
      END (* CASE *)
    END FmtKindImage

(* VISIBLE: *)
; PROCEDURE ChildOptImage ( Value : ChildOptTyp ) : TEXT 

  = BEGIN 
      CASE Value 
      OF ChildOptTyp . OptRequired => RETURN "OptRequired" 
      | ChildOptTyp . OptOptional => RETURN "OptOptional"
      | ChildOptTyp . OptAbsent => RETURN "OptAbsent"  
      | ChildOptTyp . OptPresent => RETURN "OptPresent"  
      END 
    END ChildOptImage 

(* VISIBLE: *)
; PROCEDURE ChildOpt ( IsOptional : BOOLEAN ) : ChildOptTyp  

  = BEGIN 
      IF IsOptional
      THEN
        RETURN ChildOptTyp . OptOptional 
      ELSE 
        RETURN ChildOptTyp . OptRequired 
      END (* IF *) 
    END ChildOpt 

(* VISIBLE: *)
; PROCEDURE CardImage ( Value : CardTyp ) : TEXT 
 
  = BEGIN 
      CASE Value 
      OF CardTyp . Absent => RETURN "Absent"
      | CardTyp . EmptyList => RETURN "EmptyList"
      | CardTyp . SingletonList => RETURN "SingletonList"
      | CardTyp . PluralList => RETURN "PluralList"
      END (* CASE *) 
    END CardImage 

(* VISIBLE: *)
; PROCEDURE CardSetImage ( Value : CardSetTyp ) : TEXT 

  = VAR LResult : TEXT := "{" 
  ; VAR LSep : TEXT := "" 

  ; BEGIN 
      FOR RE := FIRST ( CardTyp ) TO LAST ( CardTyp ) 
      DO 
        IF RE IN Value 
        THEN
          LResult := LResult & LSep & CardImage ( RE ) 
        ; LSep := "," 
        END (* IF *) 
      END (* FOR *) 
    ; LResult := LResult & "}"
    ; RETURN LResult 
    END CardSetImage 

(* Separators between tokens *)

(* VISIBLE: *)
; PROCEDURE NeedsSep
    ( <* UNUSED *> Lang : LbeStd . LangTyp ; Left , Right : LbeStd . TokTyp )
  : BOOLEAN

  = BEGIN (* NeedsSep *)
(* TODO: Improve this someday: *)
      IF Left = LbeStd . Tok__Null OR Right = LbeStd . Tok__Null
      THEN
        RETURN FALSE
      ELSIF Left = LbeStd . Tok__Cmnt
      THEN
        RETURN TRUE
      ELSIF LbeStd . Tok__LastNeedsSep < Left AND Left <= LbeStd . Tok__LastStd
            OR LbeStd . Tok__LastNeedsSep < Right 
               AND Right <= LbeStd . Tok__LastStd
      THEN
        RETURN FALSE
      ELSE
        RETURN TRUE
      END (* IF *)
    END NeedsSep

; PROCEDURE IndentAmt
    ( <* UNUSED *> Lang : LbeStd . LangTyp ; IndentCode : IndentCodeTyp )
  : LbeStd . LimitedCharNoSignedTyp

  = BEGIN (* IndentAmt *)
      IF IndentCode = IndentCodeNull 
      THEN 
        RETURN 0 
      ELSE 
        RETURN IndentCode
      END (* IF *) 
    END IndentAmt

(* For each language (and also for each tree-building NT, which can 
  be the root of a disconnected tree), there is an augmented NT, 
  as in A' -> A$.  $ is the inner eof symbol (there is an outer 
  eof following $, which stops parsing).  $ is there to provide 
  a place to attach trailing comments etc.  There is also a 
  Est node kind for A' and a format syntax tree for that. 
  The Fs tree always has two children; the first is the 
  Est for A (possibly optional or a list) and the second 
  is $. 
*)

(* VISIBLE: *)
; PROCEDURE CopyOfFsNode ( FsNodeRef : FsNodeRefTyp ) : FsNodeRefTyp

  = VAR LResult
    : FsNodeRefTyp

  ; BEGIN
      IF FsNodeRef = NIL 
      THEN LResult := NIL 
      ELSE 
        LResult
          := NEW
               ( FsNodeRefTyp
               , FsFmtNo := FsNodeRef . FsFmtNo
               , FsLeftFmtNo := FsNodeRef . FsLeftFmtNo
               , FsRightFmtNo := FsNodeRef . FsRightFmtNo
               , FsIndentCode := FsNodeRef . FsIndentCode
               , FsIsInsideList := FsNodeRef . FsIsInsideList
               , FsIsInsideCondFmt := FsNodeRef . FsIsInsideCondFmt
               , FsIsInsideFill := FsNodeRef . FsIsInsideFill 
               , FsIsAutonomous := FsNodeRef . FsIsAutonomous
               , FsContainsLineBreak := FsNodeRef . FsContainsLineBreak
               , FsDeletableItemsAreToRight 
                   := FsNodeRef . FsDeletableItemsAreToRight
               , FsTok := FsNodeRef . FsTok
               , FsOptionIds := FsNodeRef . FsOptionIds  
               , FsChildren := FsNodeRef . FsChildren
               , FsLdlNodeNo := FsNodeRef . FsLdlNodeNo 
               , FsPlaceholder := FsNodeRef . FsPlaceholder 
               , FsSingletonOptMin := FsNodeRef . FsSingletonOptMin 
               , FsSingletonOptMapRef := FsNodeRef . FsSingletonOptMapRef 
               , FsKind := FsNodeRef . FsKind
               , FsListSliceThruFmtNo := FsNodeRef . FsListSliceThruFmtNo
               , FsFormatsEmpty := FsNodeRef . FsFormatsEmpty 
               , FsHasLineBreak := FsNodeRef . FsHasLineBreak 
               , FsSublistTok := FsNodeRef . FsSublistTok
               , FsPartialTok := FsNodeRef . FsPartialTok
               , FsTrailingTok := FsNodeRef . FsTrailingTok
               , FsPluralListTok := FsNodeRef . FsPluralListTok
               , FsEmptyListTok := FsNodeRef . FsEmptyListTok
               , FsSingletonListTok := FsNodeRef . FsSingletonListTok
               , FsInsTokRef := FsNodeRef . FsInsTokRef
               , FsEstChildNo := FsNodeRef . FsEstChildNo
               , FsChildIndentCode := FsNodeRef . FsChildIndentCode
               , FsFirstListElemIndentCode
                   := FsNodeRef . FsFirstListElemIndentCode
               , FsLeadingChildCt := FsNodeRef . FsLeadingChildCt
               , FsEstDescendantRef := FsNodeRef . FsEstDescendantRef
               , FsEstChildOpt := FsNodeRef . FsEstChildOpt
               , FsEstChildIsOptional := FsNodeRef . FsEstChildIsOptional
               , FsCondPredicate := FsNodeRef . FsCondPredicate
               , FsCondAltRef := FsNodeRef . FsCondAltRef 
               , FsAltListTokSet := FsNodeRef . FsAltListTokSet 
               , FsAltStarListTokSet := FsNodeRef . FsAltStarListTokSet
               , FsAltFixedTokSet := FsNodeRef . FsAltFixedTokSet
               , FsAltCardSet := FsNodeRef . FsAltCardSet
               , FsAltTok := FsNodeRef . FsAltTok
               , FsAltFragCt := FsNodeRef . FsAltFragCt 
               )
      END (* IF *) 
    ; RETURN LResult
    END CopyOfFsNode

; VAR StdTokFsTreeMap
  := ARRAY LbeStd . StdTokTyp OF FsNodeRefTyp { NIL , .. }
; VAR StdTokStringMap
  := ARRAY LbeStd . StdTokTyp OF SharedStrings . T { NIL , .. }

(* VISIBLE: *)
; PROCEDURE FsRuleForTok 
    ( Lang : LbeStd . LangTyp 
    ; EstTok : LbeStd . TokTyp 
    ; IsPlaceholder : BOOLEAN := FALSE 
    ) 
  : FsNodeRefTyp 
  (* The FsNodeRef of the root of the format tree for an Est node. 
     Also works when EstTok is a list partial token. 
  *)

  = VAR LLangInfoRef : LdlSemantics . LangInfoRefTyp 

  ; BEGIN (* FsRuleForTok *)
      IF EstTok <= LAST ( LbeStd . StdTokTyp )
      THEN
        RETURN StdTokFsTreeMap [ EstTok ]
      ELSE
        LLangInfoRef := LangMap . LangInfo ( Lang )
      ; RETURN LdlSemantics . FsRuleForTokUsingLangInfo 
                 ( LLangInfoRef , EstTok , IsPlaceholder ) 
      END (* IF *)
    END FsRuleForTok

(* VISIBLE: *) 
; PROCEDURE FsRuleForEstChild  
    ( Lang : LbeStd . LangTyp 
    ; FsEstChildNode : FsNodeRefTyp 
    ; READONLY LeafElem : EstHs . LeafElemTyp 
    ) 
  : FsNodeRefTyp 
  RAISES { AssertionFailure } 
  (* Gives the FsNodeRef of the root of the appropriate format tree for an Est 
     node that is a child corresponding to FsEstChildNode, taking into account
     the singleton-list optimization.  If LeafElem leads to a singleton-list
     element for a list child of FsEstChildNode, then this will be the FsRule
     for that list.  Otherwise, same as EstUtil . FsRuleForEstNode.   
     PRE: FsEstChildNode . FsKind IN FsKindSetEstChild. 
  *) 

  = VAR LElemTok : LbeStd . TokTyp 
  ; VAR LListTok : LbeStd . TokTyp 
  ; VAR LFsNode : FsNodeRefTyp 

  ; BEGIN 
      IF EstHs . EstChildKindOptSingletonList IN LeafElem . LeKindSet 
      THEN 
        LElemTok := EstUtil . EstTok ( LeafElem . LeChildRef ) 
      ; LListTok := OptSingletonListAsTok ( FsEstChildNode , LElemTok ) 
      ; Assert 
          ( LListTok # LbeStd . Tok__Null 
          , AFT . A_GetFsRuleForEstChild_NoListTok 
          ) 
      ; LFsNode := FsRuleForTok ( Lang , LListTok , IsPlaceholder := FALSE )  
      ELSE 
        LFsNode := EstUtil . FsRuleForEstNode ( Lang , LeafElem . LeChildRef ) 
      END (* IF *) 
    ; RETURN LFsNode 
    END FsRuleForEstChild 

(* VISIBLE: *)
; PROCEDURE FsChildNoOfFmtNo 
    ( FsNodeRef : FsNodeRefTyp ; FmtNo : EstHs . FmtNoTyp )
  : FsChildNoTyp
  (* Given an Fs (sub)tree and a format number, return the child 
     number which has or contains this format number.  If the FmtNo
     is for a subsequent alternative and is not for the EstChild, 
     returns FsChildNoAlt.  If the FmtNo is for the EstChild, returns 
     the child number that is/leads to the FsEstChild of this alternative. 
     Returns FsChildNoNull if anything is wrong. 
  *)

  = VAR LFmtNoMapCt : EstHs . FmtNoTyp

  ; BEGIN (* FsChildNoOfFmtNo *)
      IF FsNodeRef . FsFmtNoMapRef = NIL
      THEN
        RETURN FsChildNoNull
      ELSE
        LFmtNoMapCt := NUMBER ( FsNodeRef . FsFmtNoMapRef ^ )
      ; IF 0 <= FmtNo AND FmtNo < LFmtNoMapCt
        THEN
          RETURN FsNodeRef . FsFmtNoMapRef ^ [ FmtNo ]
        ELSE
          RETURN FsChildNoNull
        END (* IF *)
      END (* IF *)
    END FsChildNoOfFmtNo 

(* VISIBLE: *)
; PROCEDURE FsChildRefOfFmtNo 
    ( FsNodeRef : FsNodeRefTyp ; FmtNo : EstHs . FmtNoTyp )
  : FsNodeRefTyp
  (* Given an Fs (sub)tree and a format number, return its direct Fs child 
     that has or contains this format number.  If the FmtNo is for a
     different alternative and not for the EstChild, return the next 
     alternative (which might not be the right one either).  If the 
     FmtNo is for the EstChild, return the child that is/leads to the 
     FsEstChild of this alternative.  Return NIL if anything is wrong. 
  *)

 = VAR LChildNo : FsChildNoTyp 

 ; BEGIN (* FsChildRefOfFmtNo *)
     IF FsNodeRef . FsChildren = NIL
     THEN RETURN NIL
     ELSE 
       LChildNo := FsChildNoOfFmtNo ( FsNodeRef , FmtNo )  
     ; IF LChildNo = FsChildNoAlt 
       THEN RETURN FsNodeRef . FsCondAltRef 
       ELSIF LChildNo >= NUMBER ( FsNodeRef . FsChildren ^ ) 
       THEN RETURN NIL 
       ELSE 
         RETURN FsNodeRef . FsChildren ^ [ LChildNo ]
       END (* IF *) 
     END (* IF *)  
   END FsChildRefOfFmtNo 

; PROCEDURE FsLeafRefOfFmtNo 
    ( FsNodeRef : FsNodeRefTyp ; FmtNo : EstHs . FmtNoTyp ) 
 : FsNodeRefTyp 
  (* Given an Fs (sub)tree and a format number, return the Fs leaf 
     that has this format number.  Will find the child having the FmtNo 
     in an alternative, except, for the EstChild, when it will
     return the FsEstChild of the first alternative. 
  *)

 = VAR LFsNodeRef : FsNodeRefTyp 

 ; BEGIN (* FsLeafRefOfFmtNo *) 
     LFsNodeRef := FsNodeRef 
   ; WHILE LFsNodeRef # NIL AND LFsNodeRef . FsKind IN FsKindSetHasChildren
     DO
       LFsNodeRef := FsChildRefOfFmtNo ( LFsNodeRef , FmtNo ) 
     END (* WHILE *)
   ; RETURN LFsNodeRef 
   END FsLeafRefOfFmtNo  

; PROCEDURE GetParentAndChildNoOfFmtNo  
    ( FsRootNodeRef : FsNodeRefTyp 
    ; FmtNo : EstHs . FmtNoTyp 
    ; VAR ParentFsNodeRef : FsNodeRefTyp 
    ; VAR ChildNo : FsChildNoTyp 
    ) 
  (* Starting with an FsSubtree, get the immediate parent
     Fs node and its child number, that lead to the leaf node 
     that has this format number. Will find the child having the FmtNo 
     in an alternative, except, for the EstChild, when it will
     return the FsEstChild of the first alternative. 
  *)

 = VAR LFsNodeRef : FsNodeRefTyp 

 ; BEGIN (* GetParentAndChildNoOfFmtNo *) 
     LFsNodeRef := FsRootNodeRef 
   ; ParentFsNodeRef := NIL 
   ; ChildNo := FsChildNoNull 
   ; WHILE LFsNodeRef # NIL AND LFsNodeRef . FsKind IN FsKindSetHasChildren
     DO
       ParentFsNodeRef := LFsNodeRef 
     ; ChildNo := FsChildNoOfFmtNo ( LFsNodeRef , FmtNo )  
     ; IF ChildNo = FsChildNoAlt 
       THEN LFsNodeRef := LFsNodeRef . FsCondAltRef 
       ELSIF ChildNo >= NUMBER ( LFsNodeRef . FsChildren ^ ) 
       THEN 
         ChildNo := FsChildNoNull 
       ; LFsNodeRef := NIL 
       ELSE 
         LFsNodeRef := LFsNodeRef . FsChildren ^ [ ChildNo ]
       END (* IF *) 
     END (* WHILE *)
   END GetParentAndChildNoOfFmtNo  

(* VISIBLE: *)
; PROCEDURE FsEstChildRef ( FsNodeRef : FsNodeRefTyp ) 
  : FsNodeRefTyp  
  (* If FsNodeRef is or is inside an FsKind*EstList*, FsKindCondFmt, or
     FsKindEstChildOf* node, return the unique leaf descendent of kind 
     FsKindEstChildOf*.  If this process leads to an FsKindCondFmt, 
     ignore alternatives and descend into it.  Otherwise, return NIL. 
  *) 

  = VAR LFsNodeRef : FsNodeRefTyp 

  ; BEGIN 
      LFsNodeRef := FsNodeRef 
    ; LOOP 
        IF FsNodeRef = NIL
        THEN RETURN NIL
        ELSE
          CASE LFsNodeRef . FsKind 
          OF FsKindTyp . FsKindEstChildOfFixed 
          , FsKindTyp . FsKindEstChildOfList 
          => RETURN LFsNodeRef 
          | FsKindTyp . FsKindEstListVert 
          , FsKindTyp . FsKindEstListHoriz 
          , FsKindTyp . FsKindEstListFill
          , FsKindTyp . FsKindEstListTrailHoriz  
          , FsKindTyp . FsKindEstListTrailVert  
          , FsKindTyp . FsKindEstListTrailFill   
          => LFsNodeRef 
               := FsChildRefOfFmtNo ( LFsNodeRef , EstHs . FmtNoListEstChild ) 
          | FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill 
          => IF LFsNodeRef . FsIsInsideList OR LFsNodeRef . FsIsInsideCondFmt 
             THEN 
               LFsNodeRef 
                 := LFsNodeRef . FsChildren ^ [ LFsNodeRef . FsLeadingChildCt ]
             ELSE 
               RETURN NIL
             END (* IF *) 
          | FsKindTyp . FsKindCondFmt
          => LFsNodeRef 
               := LFsNodeRef . FsChildren ^ [ LFsNodeRef . FsLeadingChildCt ]  
          ELSE RETURN NIL 
          END (* CASE *) 
        END (* IF *) 
      END (* LOOP *) 
    END FsEstChildRef 

(* VISIBLE: *)
; PROCEDURE TopmostAltRef ( FsNodeRef : FsNodeRefTyp ) 
  : FsNodeRefTyp  
  (* If FsNodeRef is or is inside an FsKind*EstList*, FsKindCondFmt, or
     FsKindEstChildOf* node, return the unique descendent of kind 
     FsKindEstChildOf* or FsKindCondFmt.  Otherwise, return NIL. 
  *) 

  = VAR LFsNodeRef : FsNodeRefTyp 

  ; BEGIN 
      LFsNodeRef := FsNodeRef 
    ; LOOP 
        IF FsNodeRef = NIL
        THEN RETURN NIL
        ELSE
          CASE LFsNodeRef . FsKind 
          OF FsKindTyp . FsKindEstChildOfFixed 
          , FsKindTyp . FsKindEstChildOfList 
          , FsKindTyp . FsKindCondFmt
          => RETURN LFsNodeRef 
          | FsKindTyp . FsKindEstListVert 
          , FsKindTyp . FsKindEstListHoriz 
          , FsKindTyp . FsKindEstListFill
          , FsKindTyp . FsKindEstListTrailHoriz  
          , FsKindTyp . FsKindEstListTrailVert  
          , FsKindTyp . FsKindEstListTrailFill   
          => LFsNodeRef 
               := FsChildRefOfFmtNo ( LFsNodeRef , EstHs . FmtNoListEstChild ) 
          | FsKindTyp . FsKindSubtreeVert 
          , FsKindTyp . FsKindSubtreeHoriz 
          , FsKindTyp . FsKindSubtreeFill 
          => IF LFsNodeRef . FsIsInsideList OR LFsNodeRef . FsIsInsideCondFmt 
             THEN 
               LFsNodeRef 
                 := LFsNodeRef . FsChildren ^ [ LFsNodeRef . FsLeadingChildCt ]
             ELSE 
               RETURN NIL
             END (* IF *) 
          ELSE RETURN NIL 
          END (* CASE *) 
        END (* IF *) 
      END (* LOOP *) 
    END TopmostAltRef 

(* VISIBLE: *)
; PROCEDURE OptSingletonListAsTok
    ( FsEstChildNode : FsNodeRefTyp ; ElemTok : LbeStd . TokTyp ) 
  : LbeStd . TokTyp 
  (* PRE: FsEstChildNode is either an FsEstChild or an FsCondFmt. *)  
  (* If ElemTok is a token that could be singleton-list optimized as a child 
     of FsEstChildNode, the abstract list token for the containing list.  
     Otherwise, LbeStd . Tok__Null.
  *) 

  = VAR LLength : CARDINAL 
  ; VAR LBiasedTok : CARDINAL 

  ; BEGIN 
      IF ElemTok >= LbeStd . Tok__FirstLangDep 
         AND FsEstChildNode # NIL 
         AND FsEstChildNode . FsSingletonOptMapRef # NIL 
         AND FsEstChildNode . FsSingletonOptMin <= ElemTok 
      THEN 
        LLength := NUMBER ( FsEstChildNode . FsSingletonOptMapRef ^ ) 
      ; LBiasedTok := ElemTok - FsEstChildNode . FsSingletonOptMin 
      ; IF LBiasedTok < LLength 
        THEN 
          RETURN FsEstChildNode . FsSingletonOptMapRef ^ [ LBiasedTok ] 
        ELSE 
          RETURN LbeStd . Tok__Null 
        END (* IF *) 
      ELSE 
        RETURN LbeStd . Tok__Null 
      END (* IF *) 
    END OptSingletonListAsTok

(* VISIBLE: *)
; PROCEDURE UnoptimizedSingletonListTok  
    ( FsEstChildNode : FsNodeRefTyp 
    ; MaybeElemTok : LbeStd . TokTyp 
    ; KindSet : EstHs . EstChildKindSetTyp
    ) 
  : LbeStd . TokTyp 
  (* PRE: FsEstChildNode is either an FsEstChild or an FsCondFmt. *)  
  (* If MaybeElemTok is a singleton-optimized list element in the context of
     FsEstChildNode and KindSet, the corresponding list token.  Otherwise,
     identity.
  *) 

  = VAR LListTok : LbeStd . TokTyp 

  ; BEGIN 
      IF EstHs . EstChildKindOptSingletonList IN KindSet 
      THEN 
        LListTok := OptSingletonListAsTok ( FsEstChildNode , MaybeElemTok ) 
      ; IF LListTok # LbeStd . Tok__Null 
        THEN
          RETURN LListTok 
        END (* IF *) 
      END (* IF *) 
    ; RETURN MaybeElemTok 
    END UnoptimizedSingletonListTok 

(* VISIBLE: *)
; PROCEDURE TokImage
    ( Tok : LbeStd . TokTyp ; Lang : LbeStd . LangTyp := LbeStd . LangNull )
  : TEXT
  (* A Text for Tok, in form used in Ldl[0|1], and suitable for informational
     diagnostic output regarding a language definition.  
     For a fixed terminal, it will have quotes and escapes.  
     For a variable terminal or nonterminal of any kind: 
       - It will be an Ldl identifier, or a generated nonterminal name 
         that is like an Ldl indentifier except it will have underscore(s) 
         where they would be illegal in Ldl.
       - This implies it does not have placeholder delimiters. 
       - It could contain a character-coded numeric value, if no Ldl-declared
         identifier to base it on is known. 
       - It will be a valid Modula-3 identifier. 
  *) 

  = VAR LSpelling : TEXT := ""

  ; BEGIN (* TokImage *)
      IF FIRST ( LbeStd . StdTokTyp ) <= Tok 
         AND Tok <= LAST ( LbeStd . StdTokTyp ) 
      THEN RETURN LbeStd . StdTokImage ( Tok ) 
      ELSIF Lang # LbeStd . LangNull
      THEN 
        LSpelling := TextForTok ( Lang , Tok ) 
      ; IF LSpelling = NIL OR Text . Equal ( LSpelling , "" ) 
        THEN 
          RETURN LbeStd . NumIdTokImage ( Tok ) 
        ELSE 
          RETURN LSpelling & "(" & LbeStd . NumTokImage ( Tok ) & ")"
        END (* IF *) 
      ELSE 
        RETURN
          LbeStd . NumIdTokImage ( Tok )
          & "(" & LbeStd . NumTokImage ( Tok ) & ")"
      END (* IF *)
    END TokImage

(* VISIBLE: *)
; PROCEDURE TextForTok ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp )
  : TEXT 
  (* The display Text for an Ldl-defined token.  "" if anything goes wrong. *)

  = VAR LBiasedTok : LbeStd . TokTyp 
  ; VAR LEstNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LEstRef : LbeStd . EstRootTyp 
  ; VAR LResult : TEXT 

  ; BEGIN (* TextForTok *)
      IF Tok < LbeStd . Tok__FirstLangDep 
      THEN RETURN ""
      END (* IF *) 
    ; WITH WLangInfo = LangMap . LangInfo ( Lang ) ^
      DO 

      (* Generate an image, if it's a generated class token. *) 
        IF WLangInfo . CsFixedToks <= Tok        
           AND Tok < WLangInfo . GcsChildToks       
        THEN 
          RETURN "GenClass__" & LbeStd . NumIdTokImage ( Tok )
        END (* IF *) 

      (* Try to get the value from the StringMap. *) 
      ; IF WLangInfo . StringMapRef # NIL 
           AND Tok >= LdlSemantics . StringMapBias
        THEN 
          LBiasedTok := Tok - LdlSemantics . StringMapBias 
        ; IF LBiasedTok <= LAST ( WLangInfo . StringMapRef ^ ) 
          THEN (* The StringMap has an entry for it. *) 
            LResult 
              := SharedStrings . ToText 
                   ( WLangInfo . StringMapRef ^ [ LBiasedTok ] ) 
          ; IF Tok < WLangInfo . StringToks 
            THEN 
              LResult := Misc . QuoteText ( LResult ) 
            ELSIF Tok < WLangInfo . AsCsClassToks       
            THEN 
              LResult := LdlSemantics . RemovePlaceholderDelimsText ( LResult ) 
            END (* IF *) 
          ; RETURN LResult 
          END (* IF *) 
        END (* IF *) 

      (* Try to use actual Ldl node.  This is questionable, because some Ldl
         nodes have multiple tokens pointing to them. 
      *) 
      ; IF WLangInfo . TokMapRef # NIL 
           AND WLangInfo . SemMapRef # NIL 
           AND Tok >= LdlSemantics . TokMapBias
           
        THEN 
(* TODO: Much of this duplicates LdlSemantics.SemDeclOfTok, but we
         don't want things that use LangUtil to be necessarily dependent
         on LdlSemantics.  Unravel this. *) 
          LBiasedTok := Tok - LdlSemantics . TokMapBias 
        ; IF LBiasedTok <= LAST ( WLangInfo . TokMapRef ^ ) 
          THEN (* We can get an Ldl node number for the declaring node. *) 
            LEstNodeNo := WLangInfo . TokMapRef ^ [ LBiasedTok ] 
          ; IF 0 <= LEstNodeNo  
               AND LEstNodeNo <= LAST ( WLangInfo . SemMapRef ^ ) 
            THEN (* And we can also get an EstRef for the declaring node. *)  
              LEstRef := WLangInfo . SemMapRef ^ [ LEstNodeNo ] . EstRef 
            ; TYPECASE LEstRef 
              OF NULL => (* Fall through. *) 
              | SharedStrings . T ( TString ) 
              => RETURN SharedStrings . ToText ( TString )  
              ELSE (* Fall through. *) 
              END (* TYPECASE *) 
            END (* IF *) 
          END (* IF *)
        END (* IF *)

      (* Give up. *) 
      ; RETURN ""
      END (* WITH *)
    END TextForTok 

(* VISIBLE: *)
; PROCEDURE DisplayStringForTok
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp
    )
  : SharedStrings . T
  (* The display string for a token, as it appears in an edit window.  
     For constant terminals, this is a token text, with quotes and escapes
     removed.  For variable terminals and nonterminals, including generated
     nonterminals, it is enclosed in placeholder delimiters.   
     It could be NIL if no string is available in the StringMap. 
  *) 

  = VAR LBiasedTok : LbeStd . TokTyp 

  ; BEGIN (* DisplayStringForTok *)
      WITH WLangInfo = LangMap . LangInfo ( Lang ) ^
      DO
        IF Tok < LdlSemantics . StringMapBias
        THEN
          RETURN NIL
        ELSE
          LBiasedTok := Tok - LdlSemantics . StringMapBias 
        ; IF LBiasedTok <= LAST ( WLangInfo . StringMapRef ^ ) 
          THEN
            RETURN WLangInfo . StringMapRef ^ [ LBiasedTok ]
          ELSE
            RETURN NIL
          END (* IF *)
        END (* IF *)
      END (* WITH *)
    END DisplayStringForTok

(* Est classes *)
(* Each named Est class also has a value of type LbeStd . TokTyp. *)

(* VISIBLE: *)
; PROCEDURE IsInClass
    ( Lang : LbeStd . LangTyp
    ; Tok : LbeStd . TokTyp
    ; Class : LbeStd . TokTyp
    )

  : BOOLEAN 
(* Class inclusion.  Tok can also be another class. *)

  = VAR LLangInfoRef : LdlSemantics . LangInfoRefTyp 
  ; VAR LListTok : LbeStd . TokTyp 
  ; VAR LListFsNodeRef : FsNodeRefTyp 

  ; BEGIN (* IsInClass *)
      IF Class = Tok 
      THEN 
        RETURN TRUE
      ELSIF Class = LbeStd . Tok__UniversalClass 
      THEN 
        RETURN TRUE 
      ELSE
        LLangInfoRef := LangMap . LangInfo ( Lang )
      ; IF TokRelation . IsElement
             ( LLangInfoRef ^ . ClassRelation , Class , Tok ) 
        THEN
          RETURN TRUE
        ELSE 
          IF LLangInfoRef ^ . AsListCardToks <= Tok 
             AND Tok < LLangInfoRef ^ . AsPartialToks 
          THEN (* It's a partial token.  Get the corresponding list token. *)
(* TODO: Get the partial tokens into the class relation and eliminate this 
         case: *) 
            LListTok 
              := Tok 
                 - LLangInfoRef ^ . AsPartialToks (* End range of partial. *)
                 + LLangInfoRef ^ . AsStarToks (* End range of list toks. *)

          ; RETURN 
              LListTok = Class 
              OR TokRelation . IsElement
                   ( LLangInfoRef ^ . ClassRelation , Class , LListTok ) 
          ELSIF LLangInfoRef . AsSublistToks <= Tok 
                AND Tok < LLangInfoRef ^ . AsListCardToks 
          THEN (* It's a list cardinality token. *) 
(* TODO:  Get the list cardinality tokens into the class relation and eliminate
          this case: *) 
            LListFsNodeRef 
              := LLangInfoRef ^ . FsTreeMapRef ^
                   [ Tok - LbeStd . Tok__FirstLangDep ]
          ; IF Tok = LListFsNodeRef . FsEmptyListTok  
               OR Tok = LListFsNodeRef . FsSingletonListTok  
               OR Tok = LListFsNodeRef . FsPluralListTok  
            THEN 
              LListTok := LListFsNodeRef . FsTok 
            ; RETURN 
                LListTok = Class 
                OR TokRelation . IsElement
                     ( LLangInfoRef ^ . ClassRelation , Class , LListTok ) 
            ELSE 
              RETURN FALSE 
            END (* IF *) 
          ELSE 
            RETURN FALSE 
          END (* IF *) 
        END (* IF *)
      END (* IF *)
    END IsInClass 

(* VISIBLE: *)
; PROCEDURE VarTermModTok
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) : LbeStd . TokTyp
  (* For Varterm token Tok, the token that appears in a 
     ModTok Est root for it. Otherwise, identity. 
  *)

  = BEGIN (* VarTermModTok *)
      WITH WLangInfo = LangMap . LangInfo ( Lang ) ^
      DO
        IF WLangInfo . StringToks <= Tok AND Tok < WLangInfo . VarTermToks
        THEN
          RETURN Tok - WLangInfo . StringToks + WLangInfo . VarTermToks
        ELSE
          RETURN Tok
        END (* IF *)
      END (* WITH *)
    END VarTermModTok 

(* VISIBLE: *)
; PROCEDURE VarTermTok
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) : LbeStd . TokTyp
  (* For token Tok that appears in a ModTok Est root, the regular VarTerm
     token. Otherwise, identity. Inverse of VarTermModTok. 
  *)

  = BEGIN (* VarTermTok *)
      WITH WLangInfo = LangMap . LangInfo ( Lang ) ^
      DO
        IF WLangInfo . VarTermToks <= Tok AND Tok < WLangInfo . VarTermModToks
        THEN
          RETURN Tok - WLangInfo . VarTermToks + WLangInfo . StringToks
        ELSE
          RETURN Tok
        END (* IF *)
      END (* WITH *)
    END VarTermTok 

(* Classes of tokens. *)

(* VISIBLE: *)
; PROCEDURE TokClass ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp )
  : LbeStd . TokClassTyp
  (* The TokClass that contains token Tok.  Works on builtin tokens too. *) 

  = BEGIN (* TokClass *)
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
        WITH WLangInfo = LangMap . LangInfo ( Lang ) 
        DO
          RETURN LangUtilLo . TokClass ( WLangInfo , Tok ) 
        END (* WITH *)
      END (* CASE *) 
    END TokClass

(* VISIBLE: *)
; PROCEDURE IsTrailingTok 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) : BOOLEAN 
  (* Tok is a list token that has trailing separators. *) 

  = BEGIN 
      IF Tok <= LbeStd . Tok__LastStd 
      THEN RETURN FALSE 
      ELSE
        WITH WLangInfo = LangMap . LangInfo ( Lang )  
        DO 
          RETURN 
(* FIXME: This. *) 
            WLangInfo . TokPart ^ [ LbeStd . TokClassAsPlusTrailingPred ] 
            <= Tok 
            AND Tok < WLangInfo . TokPart ^ [ TokClassAsStarTrailing ]
          (* ASSUMPTION: ^AsPlusTrailing+1=AsStarTrailing. *) 
        END (* WITH *) 
      END (* IF *) 
    END IsTrailingTok 

(* VISIBLE: *)
; PROCEDURE NontrailingCounterpart 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp ) 
  : LbeStd . TokTyp
  (* If Tok is a list token that has trailing separators, the
     token that does not have them.  Otherwise, identity. 
  *) 

  = BEGIN 
(* FIXME: Finish this or determine it is unneeded. *) 
      IF Tok <= LbeStd . Tok__LastStd 
      THEN RETURN Tok  
      ELSE
        WITH WLI = LangMap . LangInfo ( Lang ) ^ 
        DO 
          IF WLI . TokPart ^ [ LbeStd . TokClassAsPlusTrailingPred ] <= Tok 
          THEN 
            IF Tok < WLI . TokPart ^ [ TokClassAsPlusTrailing ]
            THEN (* It's a plus trailing token. *) 
              RETURN Tok 
                     - WLI . TokPart ^ [ TokClassAsPlusTrailing ]  
                     + WLI . TokPart ^ [ TokClassAsPlus ]  
            ELSIF Tok < WLI . TokPart ^ [ TokClassAsStarTrailing ]
                  (* ASSUMPTION: ^AsPlusTrailing+1=AsStarTrailing. *) 
            THEN (* It's a star trailing token. *) 
              RETURN Tok 
                     - WLI . TokPart ^ [ TokClassAsStarTrailing ]  
                     + WLI . TokPart ^ [ TokClassAsStar ]  
            ELSE RETURN Tok 
            END (* IF *) 
          ELSE RETURN Tok 
          END (* IF *) 
        END (* WITH *) 
      END (* IF *) 
    END NontrailingCounterpart 

(* VISIBLE: *)
; PROCEDURE IsGeneratedClassTok 
    ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp )
  : BOOLEAN 

(* TODO: Add new token classes for generated child and alt tokens, then
         replace this procedure by checks on token classes.
*) 

  = BEGIN 
      WITH WLangInfo = LangMap . LangInfo ( Lang ) ^ 
      DO
        RETURN 
          WLangInfo . GenGram # NIL 
          AND WLangInfo . GenGram . IsGenerated 
          AND WLangInfo . CsFixedToks <= Tok 
          AND Tok < WLangInfo . GcsChildToks 
      END (* WITH *) 
    END IsGeneratedClassTok 
  
(* VISIBLE: *)
; PROCEDURE IsLiteral ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp )
  : BOOLEAN
  RAISES { AssertionFailure } 

  = VAR LString : SharedStrings . T
  ; VAR LChar : CHAR 

  ; BEGIN
      WITH WLI = LangMap . LangInfo ( Lang ) ^
      DO
        IF WLI . TokPart ^ [ LbeStd . TokClassConstTermPred ] <= Tok 
           AND Tok < WLI . TokPart ^ [ TokClassConstTerm ] 
        THEN
          LString := DisplayStringForTok ( Lang , Tok )
        ; IF SharedStrings . Length ( LString ) > 0 
          THEN
            TRY  
              LChar := SharedStrings . IthChar ( LString , 0 )
            ; RETURN LChar IN SET OF CHAR { '"' , '\'' , '0' .. '9' }
(* TODO: Complete this.  Ldl needs to specify this property of a constterm. *)
            EXCEPT SharedStrings . SsOutOfBounds 
            => RAISE AssertionFailure ( "SharedStrings.SsOutOfBounds" )  
            END (* TRY EXCEPT *) 
          END (* IF *)
        END (* IF *)
      ; RETURN FALSE
      END (* WITH *) 
    END IsLiteral

(* VISIBLE: *)
; PROCEDURE IsReserved ( Lang : LbeStd . LangTyp ; Tok : LbeStd . TokTyp )
  : BOOLEAN
  RAISES { AssertionFailure } 

  = VAR LString : SharedStrings . T
  ; VAR LChar : CHAR 

  ; BEGIN
      WITH WLI = LangMap . LangInfo ( Lang ) ^
      DO
        IF WLI . TokPart ^ [ LbeStd . TokClassConstTermPred ] <= Tok 
           AND Tok < WLI . TokPart ^ [ TokClassConstTerm ] 
        THEN
          LString := DisplayStringForTok ( Lang , Tok )
        ; IF SharedStrings . Length ( LString ) > 0 
          THEN
            TRY  
              LChar := SharedStrings . IthChar ( LString , 0 )
            ; RETURN LChar IN SET OF CHAR { 'a' .. 'z' , 'A' .. 'Z' }
(* TODO: Complete this.  Ldl needs to specify this property of an instok. *)
            EXCEPT SharedStrings . SsOutOfBounds 
            => RAISE AssertionFailure ( "IsReservedEmptyString" )  
            END (* TRY EXCEPT *) 
          END (* IF *)
        END (* IF *)
      ; RETURN FALSE
      END (* WITH *) 
    END IsReserved

(* VISIBLE: *) 
; PROCEDURE TokClassFirstTok  
    ( Lang : LbeStd . LangTyp ; TokClass : LbeStd . TokClassTyp ) 
  : LbeStd . TokTyp 
  (* Lowest numbered token belonging to TokClass, not counting LbeStd
     builtin tokens. 
  *) 

  = BEGIN 
      WITH WLangInfo = LangMap . LangInfo ( Lang ) 
      DO
        RETURN LangUtilLo . TokClassFirstTok ( WLangInfo , TokClass ) 
      END (* WITH *) 
    END TokClassFirstTok 

(* VISIBLE: *) 
; PROCEDURE TokClassLastTok  
    ( Lang : LbeStd . LangTyp ; TokClass : LbeStd . TokClassTyp ) 
  : LbeStd . TokTyp 
  (* Highest numbered token belonging to TokClass, not counting LbeStd
     builtin tokens.  Not that if the class is empty, this will be one
     less than the result of TokClassFirstTok.  
  *) 

  = BEGIN 
      WITH WLangInfo = LangMap . LangInfo ( Lang ) 
      DO
        RETURN LangUtilLo . TokClassLastTok ( WLangInfo , TokClass ) 
      END (* WITH *) 
    END TokClassLastTok 

(* VISIBLE: *)
; PROCEDURE Gram ( Lang : LbeStd . LangTyp ) : LRTable . GrammarTyp

  = BEGIN (* Gram *)
      WITH WLangInfo = LangMap . LangInfo ( Lang ) ^
      DO
        IF Options . PreferGeneratedGrammar  
        THEN 
          IF WLangInfo . GenGram = NIL
          THEN 
            Assertions . MessageText 
              ( "-----------  No generated grammar exists ----------------" )
          ELSE 
            Assertions . MessageText 
              ( "--------------- Using generated grammar -----------------" )
          ; RETURN WLangInfo . GenGram
          END (* IF *) 
        END (* IF *) 
      (* Either handwritten is preferred or there is no generated. *) 
      ; IF WLangInfo . Gram = NIL
        THEN 
          Assertions . MessageText 
            ( "----------  No handwritten grammar exists ---------------" )
        ; IF WLangInfo . GenGram = NIL 
          THEN (* No grammar at all. *) 
            RETURN NIL 
          ELSE 
            Assertions . MessageText 
              ( "--------------- Using generated grammar -----------------" )
          ; RETURN WLangInfo . GenGram
          END (* IF *) 
        ELSE 
          Assertions . MessageText 
            ( "--------------- Using handwritten grammar ---------------" )
        ; RETURN WLangInfo . Gram 
        END (* IF *) 
      END (* WITH *)
    END Gram

(* FIXME: The following is wrong.  It 
        doesn't have the two tokens for varterms that 
        are needed for ModTok subtrees. *)

; VAR GTopFsNodeRef : FsNodeRefTyp := NIL

; PROCEDURE InitTopFsNodeRef ( )

  = BEGIN
      IF GTopFsNodeRef = NIL
      THEN
        GTopFsNodeRef := NEW ( FsNodeRefTyp )
      ; GTopFsNodeRef . FsFmtNo := EstHs . FmtNoEstTop
      ; GTopFsNodeRef . FsLeftFmtNo := EstHs . FmtNoEstTop
      ; GTopFsNodeRef . FsRightFmtNo := EstHs . FmtNoEstTop
      ; GTopFsNodeRef . FsIndentCode := IndentCodeInitial
      ; GTopFsNodeRef . FsChildIndentCode := IndentCodeInitial
      ; GTopFsNodeRef . FsKind := FsKindTyp . FsKindEstChildOfFixed
      ; GTopFsNodeRef . FsLdlNodeNo := LbeStd . EstNodeNoNull 
      ; GTopFsNodeRef . FsIsInFirstLine := TRUE 
      ; GTopFsNodeRef . FsIsAutonomous := TRUE 
      END
    END InitTopFsNodeRef

(* VISIBLE: *)
; PROCEDURE TopFsNodeRef ( ) : FsNodeRefTyp
  (* A parentless node of kind FsKindEstChildOfFixed, needed to get
    some traversers started.  The whole Est is considered a child
    of this. *)

  = BEGIN
      RETURN GTopFsNodeRef
    END TopFsNodeRef

; PROCEDURE StdStuffForVarTerminal
    ( Tok : LbeStd . TokTyp ; PlaceholderText : TEXT
    )

  = <* FATAL AssertionFailure *> 
    BEGIN (* StdStuffForVarTerminal *)
      StdTokFsTreeMap [ Tok ]
        := NEW
             ( FsNodeRefTyp
             , FsIndentCode := IndentCodeInitial
             , FsTok := Tok
             , FsKind := FsKindTyp . FsKindAstString 
             , FsLdlNodeNo := LbeStd . EstNodeNoNull 
             )
    ; StdTokStringMap [ Tok ]
        := SharedStrings . FromText ( PlaceholderText , Tok )
    END StdStuffForVarTerminal

; PROCEDURE StdStuffForVarTerminals ( )

  = BEGIN (* StdStuffForVarTerminals *)
      StdStuffForVarTerminal ( LbeStd . Tok__Ident , "<Identifier>" )
    ; StdStuffForVarTerminal ( LbeStd . Tok__StringLit , "<StringLiteral>" )
    ; StdStuffForVarTerminal ( LbeStd . Tok__CharLit , "<CharacterLiteral>" )
    ; StdStuffForVarTerminal ( LbeStd . Tok__IntLit , "<IntegerLiteral>" )
    ; StdStuffForVarTerminal ( LbeStd . Tok__LongintLit , "<LongintLiteral>" )
    ; StdStuffForVarTerminal ( LbeStd . Tok__RealLit , "<RealLiteral>" )
    ; StdStuffForVarTerminal ( LbeStd . Tok__LongrealLit , "<RealLiteral>" )
    ; StdStuffForVarTerminal ( LbeStd . Tok__ExtendedLit , "<ExtendedLiteral>" )
    ; StdStuffForVarTerminal ( LbeStd . Tok__LexErrChars , "<LexErrChars>" )
    END StdStuffForVarTerminals

; PROCEDURE StdStuffForLexErrModTok ( ) 

  = <* FATAL AssertionFailure *> 
    VAR LFsChild : FsNodeRefTyp 
  ; VAR LFsRoot : FsNodeRefTyp 
  ; VAR LFmtNoMapRef : LangUtilRep . FsFmtNoMapRefTyp
  ; VAR LFsChildrenRef : FsChildrenArrayRefTyp 

  ; BEGIN 
      LFsChild 
        := NEW 
             ( FsNodeRefTyp 
             , FsFmtNo := EstHs . FmtNoModTok 
             , FsLeftFmtNo := EstHs . FmtNoModTok 
             , FsRightFmtNo := EstHs . FmtNoModTok  
             , FsIndentCode := IndentCodeInitial 
             , FsTok := LbeStd . Tok__LexErrChars  
             , FsKind := FsKindTyp . FsKindEstChildOfFixed 
             , FsLdlNodeNo := LbeStd . EstNodeNoNull 
             , FsEstChildNo := 0 
             , FsEstChildOpt := ChildOptTyp . OptRequired 
             , FsEstChildIsOptional := FALSE 
             , FsChildIndentCode := IndentCodeInitial 
             ) 
    ; LFsChildrenRef := NEW ( FsChildrenArrayRefTyp , 1 ) 
    ; LFsChildrenRef ^ [ 0 ] := LFsChild 
    ; LFmtNoMapRef := NEW ( LangUtilRep . FsFmtNoMapRefTyp , 1 )
    ; LFmtNoMapRef ^ [ 0 ] := 0  
    ; LFsRoot 
        := NEW 
             ( FsNodeRefTyp 
             , FsFmtNo := EstHs . FmtNoModTok 
             , FsLeftFmtNo := EstHs . FmtNoModTok 
             , FsRightFmtNo := EstHs . FmtNoModTok  
             , FsTok := LbeStd . Tok__LexErrChars   
             , FsIndentCode := IndentCodeInitial 
             , FsKind := FsKindTyp . FsKindEstFixedHoriz 
             , FsChildren := LFsChildrenRef 
             , FsLdlNodeNo := LbeStd . EstNodeNoNull 
             , FsFmtNoMapRef := LFmtNoMapRef 
             ) 
    ; StdTokFsTreeMap [ LbeStd . Tok__LexErrChars ] := LFsRoot 
    ; StdTokStringMap [ LbeStd . Tok__LexErrChars ]
        := SharedStrings . FromText 
             ( "<LexErrModTok>" , LbeStd . Tok__LexErrChars )
    END StdStuffForLexErrModTok 

; PROCEDURE StdStuffForAugment ( )

  = <* FATAL AssertionFailure *> 
    BEGIN
      WITH WRoot = StdTokFsTreeMap [ LbeStd . Tok__Augment ]
      DO
        WRoot
          := NEW
               ( FsNodeRefTyp
               , FsLeftFmtNo := EstHs . FmtNoBOIChildOfAugment
               , FsRightFmtNo := EstHs . FmtNoEOIChildOfAugment
               , FsIndentCode := IndentCodeInitial
               , FsChildren := NEW ( FsChildrenArrayRefTyp , 3 )
               , FsTok := LbeStd . Tok__Augment
               , FsKind := FsKindTyp . FsKindEstFixedHoriz
               , FsLdlNodeNo := LbeStd . EstNodeNoNull 
               , FsFmtNoMapRef := NEW ( LangUtilRep . FsFmtNoMapRefTyp , 3 ) 
               )
      ; WRoot . FsChildren ^ [ 0 ]
          := NEW
               ( FsNodeRefTyp
               , FsFmtNo := EstHs . FmtNoBOIChildOfAugment 
               , FsIndentCode := IndentCodeInitial
               , FsTok := LbeStd . Tok__BegOfImage
               , FsKind := FsKindTyp . FsKindBegOfImage
               , FsFormatsEmpty := UbTrue 
               , FsLdlNodeNo := LbeStd . EstNodeNoNull 
               )
      ; WRoot . FsChildren ^ [ 1 ]
          := NEW
               ( FsNodeRefTyp
               , FsFmtNo := EstHs . FmtNoEstChildOfAugment 
               , FsIndentCode := IndentCodeInitial
               , FsTok := LbeStd . Tok__UniversalClass
               , FsKind := FsKindTyp . FsKindEstChildOfFixed
               , FsLdlNodeNo := LbeStd . EstNodeNoNull 
               , FsChildIndentCode := IndentCodeInitial
               )
      ; WRoot . FsChildren ^ [ 2 ]
          := NEW
               ( FsNodeRefTyp
               , FsFmtNo := EstHs . FmtNoEOIChildOfAugment  
               , FsIndentCode := IndentCodeInitial
               , FsTok := LbeStd . Tok__EndOfImage
               , FsKind := FsKindTyp . FsKindEndOfImage
               , FsFormatsEmpty := UbTrue 
               , FsLdlNodeNo := LbeStd . EstNodeNoNull 
               )
      ; WRoot . FsFmtNoMapRef ^ [ EstHs . FmtNoBOIChildOfAugment ] := 0 
      ; WRoot . FsFmtNoMapRef ^ [ EstHs . FmtNoEstChildOfAugment ] := 1 
      ; WRoot . FsFmtNoMapRef ^ [ EstHs . FmtNoEOIChildOfAugment ] := 2 
      END (* WITH *)
    ; StdTokStringMap [ LbeStd . Tok__Augment ]
        := SharedStrings . FromText ( "<_SPrime_>" , LbeStd . Tok__Augment )
    END StdStuffForAugment 

; PROCEDURE StdStuffForOther ( )

  = BEGIN
      StdStuffForLexErrModTok ( ) 
    ; StdStuffForAugment ( ) 
    END StdStuffForOther 

; BEGIN (* LangUtil *)
    StdStuffForVarTerminals ( )
  ; StdStuffForOther ( )
  ; SuffixMap := NEW ( TextSuffixInfoTbl . Default ) . init ( sizeHint := 40 )
  ; InitHardSuffixMap ( ) 
  ; InitTopFsNodeRef ( )
  ; UniqueFsTreeTrailingSep  
      := NEW ( FsNodeRefTyp 
             , FsFmtNo := EstHs . FmtNoListEstChild 
             , FsLeftFmtNo := EstHs . FmtNoListEstChild 
             , FsRightFmtNo := EstHs . FmtNoListEstChild 
             , FsDeletableItemsAreToRight := FALSE 
             , FsKind := FsKindTyp . FsKindEstFixedVert 
             , FsFormatsEmpty := UbTrue 
             , FsHasLineBreak := FormatsEmptyTyp . FeNo  
             , FsTok := LbeStd . Tok__Empty 
             , FsChildren := NIL  
             )    
  ; StdTokFsTreeMap [ LbeStd . Tok__Empty ] := UniqueFsTreeTrailingSep  

  END LangUtil
.
