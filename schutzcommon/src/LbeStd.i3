
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2021, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE LbeStd 

(* Shared declarations used by the whole lbe *) 

(* Conventions: 
 
   Module names ending in HS (heap structure) are interfaces 
   to structures which reside in the managed heap. 
   Clients of heap structure modules know when a type is really 
   a pointer into the heap. 
   All pointers to things in the heap are named ---Ref or ---RefTyp. 
   ---Nil is always used for nil values of these. 
   All allocators of heap objects use result parameters rather 
   than function results, so that the pointer can be immediately 
   stored where the scavenger can find it, in case concurrent 
   scavenging is done. 
 
*) 

; IMPORT PortTypes 
; IMPORT Ascii 

; CONST AppName = "Schutz" 
; CONST CommandName = "schutz" 

; CONST UseAugmentTrees = TRUE  

(* Item numbers *) 

; TYPE ItemNoTyp = PortTypes . Card32Typ 

(* Characters *) 

; CONST CharNull = Ascii . null 
  ; CharBlank = ' '  
  ; CharNewLine = Ascii . lf 
  ; CharTab = Ascii . ht 
  ; CharReturn = Ascii . cr 
  ; CharEndOfImage = Ascii . em 
  ; CharBell = Ascii . bel 
  ; CharFirstPrintable = Ascii . FirstPrintable  
  ; CharLastPrintable = Ascii . LastPrintable  
(* TODO: Substitute these values inline? *) 

; CONST LeftPlaceholderDelimChar = '\253' 
; CONST LeftPlaceholderDelimText = "\253" 
; CONST RightPlaceholderDelimChar = '\273' 
; CONST RightPlaceholderDelimText = "\273" 
  (* NOTE: If we ever make these variables, then LeftPlaceholderDelimLen and
           RightPlaceholderDelimLen global variables in LdlSemantics.m3 need
           to be updated when they change.
  *) 

(* Error codes *) 

; TYPE ErrCodeTyp = PortTypes . Card16Typ 

; CONST ErrCodeNull = 0 
; CONST ErrCodeImage = PortTypes . Card16Image 

(* Lexical error codes *) 

; CONST LeNoError = 0 
  ; LeUnderscoreInIdent = 1 
  ; LeUnclosedString = 2 
  ; LeUnclosedChar = 3 
  ; LeBadOctalDigit = 4 
  ; LeBadDecDigit = 5 
  ; LeNoExponentDigit = 6 
  ; LeNoFractionalDigit = 7 
  ; LeNoHexDigit = 8 
  ; LeTokTooLong = 9 (* => remaining chars ignored *) 
  ; LeBadChars = 10 
  ; LeIncompleteNumber = 11  
  ; LeUnclosedPlaceholder = 12 
  ; LeUnknownPlaceholder = 13 

(* Languages *) 

; TYPE LangTyp = PortTypes . Int8Typ 

; CONST LangNull = LAST ( LangTyp ) 
; CONST LangFirstLdl = 0 
; CONST LangLdl0 = 0 
; CONST LangLdl1 = 1 
; CONST LangLdl2 = 2 
; CONST LangLdl3 = 3 
; CONST LangLdl4 = 4 
; CONST LangLastLdl = 4 
; CONST LangLdlAnalyzed = 5 
  (* Used to put an Ldl being analyzed temporarily into a map. *) 
; CONST LangLastBuiltin = 9 
; CONST LangFirstNonBuiltin = LangLastBuiltin + 1  
; CONST LangM3 = 10 
; CONST LangImage = PortTypes . Int8Image 

; TYPE LangBuiltinTyp = [ LangLdl0 .. LangFirstNonBuiltin ] 

(* Scanner states *) 

; TYPE ScanStateTyp = PortTypes . Card8Typ 

; CONST SsIdle = 0   (* Between tokens *) 
  ; SsInTok = 1      (* Scanner must not be interrupted.  Scanner could 
                        hold its own internal substate of this, and it 
                        could be between tokens but needing to restart 
                        in a non-standard start state.  e.g. it has 
                        already consumed the first dot of '1..' in order 
                        to determine that '1' is an integer and not 
                        the integer part of a real literal. *) 
  ; SsInLexError = 2 (* Inside a lex error *) 
  ; SsInCmnt = 3     (* inside outermost level of comment *) 
  ; SsMax = LAST ( ScanStateTyp ) 
                     (* Scanner must use values >= SsInCmnt for nested 
                        comments. *) 
  ; SsMaxCmnt = SsMax - 1 
                     (* Scanner can use SsMaxCmnt for all levels of 
                        comment nesting >= SsMaxCmnt.  ParseTrv considers 
                        this to be inside a comment, but it never matches 
                        any other comment. *) 

; PROCEDURE ScanStateImage ( Value : ScanStateTyp ) : TEXT 

(* Syntax error repairs. *) 

; TYPE RepairCostTyp = PortTypes . Card32Typ 

; CONST RepairCostInfinity = LAST ( RepairCostTyp ) 
; CONST RepairCostMax = RepairCostInfinity - 1 

; CONST RepairCostImage = PortTypes . Card32Image 

; TYPE LimitedTokCtTyp = PortTypes . Card8Typ 

; CONST LimitedTokCtImage = PortTypes . Int8Image 
; CONST ParseCheckInfinity = LAST ( LimitedTokCtTyp ) 
; CONST ParseCheckMax = ParseCheckInfinity - 1 

; PROCEDURE LimitedTokCtSum 
    ( Left , Right : LimitedTokCtTyp ) : LimitedTokCtTyp 

; PROCEDURE IncLimitedTokCt 
    ( VAR Left : LimitedTokCtTyp 
    ; Right : LimitedTokCtTyp := 1 
    ) 

(* LR Parse states *) 

; TYPE LRStateTyp = PortTypes . Card16Typ 
; CONST LRStateImage = PortTypes . Card16Image 
; CONST LRStateNull = LAST ( LRStateTyp ) 
; TYPE ProdNoTyp = PortTypes . Card16Typ 

(* Common procedure types *) 

; TYPE PROC = PROCEDURE ( ) 
; TYPE CharProc = PROCEDURE ( p0 : CHAR ) 
; TYPE RetCharFunc = PROCEDURE ( ) : CHAR 
; TYPE ErrorProc = PROCEDURE ( p0 : ErrCodeTyp ) 

(* Text coordinates *) 

; TYPE LineNoTyp = PortTypes . Card32Typ 

; CONST LineNoNull = LAST ( LineNoTyp ) 
; CONST LineNoMax = LineNoNull - 1 

; TYPE LineNoSignedTyp = [ - LineNoMax .. LineNoMax ] 

; CONST LineNoImage = PortTypes . Card32Image 

; TYPE CharNoTyp = PortTypes . Int32Typ 

; CONST CharNoImage = PortTypes . Int32Image 
; CONST CharNoUnknown = LAST ( CharNoTyp ) 
; CONST CharNoInfinity = CharNoUnknown - 1 
; CONST CharNoMax = CharNoInfinity - 1 
        (* ^A Thru value. *) 

; TYPE LimitedCharNoTyp = PortTypes . Card8Typ 
  (* WARNING: Code elsewhere (At least EstUtil.WidthSum3 and WidthSumSigned3)
     dependes on builtin integer arithmetic being done in fields at least
     3 times the upper limit of LimitedCharNoTyp, in both directions.
  *) 

; CONST LimitedCharNoImage = PortTypes . Card8Image 
; CONST LimitedCharNoNull = LAST ( LimitedCharNoTyp ) 
; CONST LimitedCharNoUnknown = LimitedCharNoNull 
; CONST LimitedCharNoInfinity = LimitedCharNoUnknown - 1 
; CONST LimitedCharNoMax = LimitedCharNoInfinity - 1 
        (* ^A Thru value. *) 

; PROCEDURE ProjectToLimitedCharNoTyp  ( Value : PortTypes . Int32Typ ) 
  : LimitedCharNoTyp 

; TYPE LimitedCharNoSignedTyp 
    = [ - LAST ( LimitedCharNoTyp ) .. LAST ( LimitedCharNoTyp ) ] 

; CONST LimitedCharNoMinusInfinity = FIRST ( LimitedCharNoSignedTyp ) 

(* Limited length string *) 

; TYPE LineTyp = ARRAY [ 0 .. LimitedCharNoMax ] OF CHAR 

(* Est references *) 

; TYPE LbeRootTyp = ROOT 
; TYPE EstRootTyp = LbeRootTyp 

(* Parse kinds *) 

; TYPE ParseKindTyp = { ParseKindFile , ParseKindKbd , ParseKindTrav } 

(* Est child numbers *) 
(* These number the Est children of an Est interior node, from 
   zero.  These are the subscripts of the sequence which K-trees 
   implement. *) 
(* When est node numbering is implemented, see if we can do away with these. *) 

; TYPE EstChildNoTyp = PortTypes . Int16Typ 

; CONST EstChildNoImage = PortTypes . Int16Image 
; CONST EstChildNoNull = FIRST ( EstChildNoTyp ) 
; CONST EstChildNoMax = LAST ( EstChildNoTyp ) 

(* Est node numbers. *) 
(* All nodes in an Est are numbered sequentially by these, 
   in preorder.  *) 

; TYPE EstNodeNoTyp = PortTypes . Int32Typ 

; CONST EstNodeNoImage = PortTypes . Int32Image 
; CONST EstNodeNoNull = FIRST ( EstNodeNoTyp ) 
; CONST EstNodeNoMax = LAST ( EstNodeNoTyp ) 

(* Lexical token codes. *) 

; TYPE TokTyp = PortTypes . Card16Typ 
(* ; TYPE TokPackedTyp = BITS 16 FOR TokTyp *)

(* Tokens which are likely to be format tokens in various languages 
   are defined here. 
   Tokens which are likely to be present but with different rules 
   ( such as real literals ) have declarations in unique language 
   dependent modules. 
   These tokens have names containing "__" so it is impossible for
   an Ldl definition to spoof them.
*) 

(* Separators between tokens, sometimes needed to avoid run-on tokens becoming
   a single token. *) 

; CONST SepWidth = 1 
; CONST SepChar = ' ' (* A single blank. *)  
; CONST SepText = " " 
(* CHECK: ^That these are used consistently. *) 

; CONST 
  (* Pseudo tokens: *) 
    Tok__Null = 0 

  (* The VarTerminal tokens in this range are probably not going 
     to be used.  Instead, LdlSemantics will assign token values 
     for these for each language, above Tok__LastStd. *) 

  ; Tok__FirstVarTerminal = 1 
  ; Tok__Ident = 1 
  ; Tok__StringLit = 2 
  ; Tok__CharLit = 3 
  ; Tok__IntLit = 4 
  ; Tok__LongintLit = 5
  ; Tok__RealLit = 6 
  ; Tok__LongrealLit = 7 
  ; Tok__ExtendedLit = 8 
  ; Tok__IdentModTok = 9 
  ; Tok__StringLitModTok = 10 
  ; Tok__CharLitModTok = 11 
  ; Tok__IntLitModTok = 12 
  ; Tok__LongintLitModTok = 13 
  ; Tok__RealLitModTok = 14 
  ; Tok__LongrealLitModTok = 15 
  ; Tok__ExtendedLitModTok = 16 
  ; Tok__LastVarTerminal = 16 

  ; Tok__LexErrChars = 17 
  ; Tok__ForceSep = 18 (* Used for errors, deletions, etc. to ensure 
                          that a separator character is adjacent. *) 
  ; Tok__LastNeedsSep = 18 

  ; Tok__Sep = 19 
  ; Tok__BadChars = 20 
  ; Tok__Cmnt = 21 
  ; Tok__CmntAtEndOfLine = 22 
  ; Tok__ModText = 23 
  ; Tok__BlankLine = 24 
  ; Tok__Unknown = 25 
  ; Tok__BegOfLine = 26 
  ; Tok__BegOfImage = 27 
  ; Tok__EndOfImage = 28 
  ; Tok__Augment = 29 
  ; Tok__UniversalClass = 30
  ; Tok__Empty = 31 
  ; Tok__OptSingletonList = 32
  ; Tok__LastStd = 32 
  ; Tok__FirstLangDep = 33  

  ; Tok__LastReal = LAST ( TokTyp ) - 1 
    (* We don't use the very last value as a real token, so we can have it
       for a TO value. *) 

(* Standard tokens are builtin and language-independent. *) 

; TYPE StdTokTyp = [ Tok__Null .. Tok__LastStd ] 

; CONST NumTokImage = PortTypes . Int16Image 
        (* Purely digits. *) 

; PROCEDURE NumIdTokImage ( Tok : TokTyp ) : TEXT 
  (* An identifier with numeric value inside.  A valid Modula-3 identifier, but
     not a valid Ldl identifier. *)

; PROCEDURE StdTokImage ( Tok : StdTokTyp ) : TEXT 
  (* Image of a standard tok.  A valid Modula-3 identifier, but
     not a valid Ldl identifier.
  *) 

; PROCEDURE StdTokPlaceholderImage ( Tok : StdTokTyp ) : TEXT 
  (* ^Has nonterminal delimiter characters. *) 

(* See also: LangUtil.TokImage and ParseHs . TokInfoImage. *)

; TYPE StdTokSetTyp = SET OF StdTokTyp 

; CONST StdTokSetNoSep 
    = StdTokSetTyp 
        { Tok__Sep , Tok__BadChars , Tok__Cmnt , Tok__CmntAtEndOfLine 
        , Tok__ModText , Tok__BlankLine , Tok__BegOfLine , Tok__BegOfImage 
        , Tok__EndOfImage 
        } 
  (* ^These do not need a separating blank on either side. *) 

; CONST StdTokSetCondNlAfter 
    = StdTokSetTyp { Tok__Cmnt , Tok__CmntAtEndOfLine , Tok__ModText } 
; CONST StdTokSetCondNlBefore 
    = StdTokSetTyp { Tok__Cmnt , Tok__CmntAtEndOfLine , Tok__ModText } 

(* Token classes. *) 

(*
; TYPE TokClassTyp 
    = { TokClassNull 
      , TokClassMisc 
      , TokClassConstTerminal       (* Terminal with single spelling, 
                                       e.g. "<=" *) 
      , TokClassVarTerminal         (* Terminal with multiple spellings, 
                                       e.g. identifier. *) 
      , TokClassVarTerminalMod      (* Used in a ModTok for a VarTerminal *) 
      , TokClassEstListNonterminal  (* For an Est List node. *) 
      , TokClassEstFixedNonterminal (* For an Est Fixed node. *) 
      , TokClassSublistNonterminal  (* Internally-generated nonterminal for 
                                       list slicing. Doesn't build an Est node,
                                       but can be returned by ParseTrv. *) 
      , TokClassListCardNonterminal (* Internally-generated nonterminal, for 
                                       machine-generated list cardinalities. *)
      , TokClassPartialNonterminal  (* Internally-generated nonterminal, for 
                                       machine-generated CS lists. Corresponds 
                                       to a partial merge. *) 
      , TokClassClassOnly           (* Ldl classes that are not concrete NTs. *)
      , TokClassClassNonterminal    (* Ldl classes that are concrete NTs. *) 
      , TokClassReduceNonterminal   (* Nonterminal generated only by reductions,
                                       Doesn't build an Est node and is not
                                       returned by ParseTrv. *) 
      } 
*)

; TYPE TokClassTyp 
    = { TokClassNull 
      , TokClassMisc            (* Certain builtin pseudo-tokens.  Not used for
                                   token space partitioning, where these are 
                                   actually inside the partition of 
                                   TokClassBuiltin . *) 
      , TokClassBuiltin         (* Schutz-builtin, language independent. *) 
      , TokClassConstTerm       (* Terminal with single spelling, e.g. "<=" *) 
      , TokClassVarTerm         (* Terminal with multiple spellings, 
                                   e.g. identifier. *) 
      , TokClassVarTermMod      (* Used in a ModTok for a VarTerminal *) 
      , TokClassAsPlus          (* Abstract plus list node. *) 
      , TokClassAsStar          (* Abstract star list node. *) 
      , TokClassAsPlusTrailing  (* Abstract plus list node with optional 
                                   trailing separators. *) 
      , TokClassAsStarTrailing  (* Abstract star list node with optional 
                                   trailing separators. *) 
      , TokClassAsFixed         (* Abstract Fixed node. *) 
      , TokClassSublist         (* Internally-generated nonterminal for list 
                                   list slicing.  Can be returned by ParseTrv
                                   (and will have an attacted As list slice). 
                                   Never built nor reduced-to. *) 
      , TokClassListCard        (* Internally-generated nonterminal, for list
                                   cardinalities. *)
      , TokClassPartial         (* Internally-generated nonterminal, for 
                                   machine-generated CS lists.  Corresponds 
                                   to a partial merge.  Created by a reduction. 
                                   Never built nor returned by ParseTrv.  *) 
      , TokClassAsClass         (* Abstract-only class. *) 
      , TokClassAsCsClass       (* Both abstract class and concrete class. *) 
      , TokClassCsClass         (* Concrete-only class nonterminal. *) 
      , TokClassCsPlus          (* Concrete plus list nonterminal. *) 
      , TokClassCsPlural        (* Concrete plural list nonterminal. *) 
      , TokClassCsStar          (* Concrete star list nonterminal. *) 
      , TokClassCsFixed         (* Concrete fixed production nonterminal. *) 
      , TokClassCsGen           (* Internally-generated concrete nonterminal. *)
      , TokClassUnused          (* For remaining unused token space. *) 
      } 

(* Constant predecessors of token classes: *) 

; CONST TokClassBuiltinPred 
    = VAL ( ORD ( TokClassTyp . TokClassBuiltin ) - 1 , TokClassTyp )
; CONST TokClassMiscPred 
    = VAL ( ORD ( TokClassTyp . TokClassMisc ) - 1 , TokClassTyp )
; CONST TokClassConstTermPred 
    = VAL ( ORD ( TokClassTyp . TokClassConstTerm ) - 1 , TokClassTyp )
; CONST TokClassVarTermPred 
    = VAL ( ORD ( TokClassTyp . TokClassVarTerm ) - 1 , TokClassTyp )
; CONST TokClassVarTermModPred 
    = VAL ( ORD ( TokClassTyp . TokClassVarTermMod ) - 1 , TokClassTyp )
; CONST TokClassAsPlusPred 
    = VAL ( ORD ( TokClassTyp . TokClassAsPlus ) - 1 , TokClassTyp )
; CONST TokClassAsPlusTrailingPred 
    = VAL ( ORD ( TokClassTyp . TokClassAsPlusTrailing ) - 1 , TokClassTyp )
; CONST TokClassAsStarTrailingPred 
    = VAL ( ORD ( TokClassTyp . TokClassAsStarTrailing ) - 1 , TokClassTyp )
; CONST TokClassAsStarPred 
    = VAL ( ORD ( TokClassTyp . TokClassAsStar ) - 1 , TokClassTyp )
; CONST TokClassAsFixedPred 
    = VAL ( ORD ( TokClassTyp . TokClassAsFixed ) - 1 , TokClassTyp )
; CONST TokClassSublistPred 
    = VAL ( ORD ( TokClassTyp . TokClassSublist ) - 1 , TokClassTyp )
; CONST TokClassListCardPred 
    = VAL ( ORD ( TokClassTyp . TokClassListCard ) - 1 , TokClassTyp )
; CONST TokClassPartialPred 
    = VAL ( ORD ( TokClassTyp . TokClassPartial ) - 1 , TokClassTyp )
; CONST TokClassAsClassPred 
    = VAL ( ORD ( TokClassTyp . TokClassAsClass ) - 1 , TokClassTyp )
; CONST TokClassAsCsClassPred 
    = VAL ( ORD ( TokClassTyp . TokClassAsCsClass ) - 1 , TokClassTyp )
; CONST TokClassCsClassPred 
    = VAL ( ORD ( TokClassTyp . TokClassCsClass ) - 1 , TokClassTyp )
; CONST TokClassCsPlusPred 
    = VAL ( ORD ( TokClassTyp . TokClassCsPlus ) - 1 , TokClassTyp )
; CONST TokClassCsPluralPred 
    = VAL ( ORD ( TokClassTyp . TokClassCsPlural ) - 1 , TokClassTyp )
; CONST TokClassCsStarPred 
    = VAL ( ORD ( TokClassTyp . TokClassCsStar ) - 1 , TokClassTyp )
; CONST TokClassCsFixedPred 
    = VAL ( ORD ( TokClassTyp . TokClassCsFixed ) - 1 , TokClassTyp )
; CONST TokClassCsGenPred 
    = VAL ( ORD ( TokClassTyp . TokClassCsGen ) - 1 , TokClassTyp )
; CONST TokClassUnusedPred 
    = VAL ( ORD ( TokClassTyp . TokClassUnused ) - 1 , TokClassTyp )

; PROCEDURE TokClassPred ( Class : TokClassTyp ) : TokClassTyp 
  (* Predecessor function on TokClassTyp.  Will crash on FIRST(TokClassTyp) *) 

; PROCEDURE TokClassSucc ( Class : TokClassTyp ) : TokClassTyp 
  (* Successor function on TokClassTyp.  Will crash on LAST(TokClassTyp) *) 

; PROCEDURE TokClassImage ( TokClass : TokClassTyp ) : TEXT 
  (* Unqualified enumeration constant identifier of TokClass. *) 

; TYPE TokClassSetTyp = SET OF TokClassTyp 

; CONST TokClassSetTerm 
    = TokClassSetTyp 
        { TokClassTyp . TokClassConstTerm 
        , TokClassTyp . TokClassVarTerm
        } 

; CONST TokClassSetNoTrailingMods 
    = TokClassSetTyp 
        { TokClassTyp . TokClassAsPlus
        , TokClassTyp . TokClassAsPlusTrailing
        , TokClassTyp . TokClassAsStarTrailing
        , TokClassTyp . TokClassAsStar
        , TokClassTyp . TokClassAsFixed
        , TokClassTyp . TokClassSublist
        , TokClassTyp . TokClassListCard
        , TokClassTyp . TokClassPartial
        , TokClassTyp . TokClassAsClass
        , TokClassTyp . TokClassAsCsClass
        , TokClassTyp . TokClassCsClass
        , TokClassTyp . TokClassCsPlus
        , TokClassTyp . TokClassCsPlural
        , TokClassTyp . TokClassCsStar
        , TokClassTyp . TokClassCsFixed
        , TokClassTyp . TokClassCsGen
        } 

; CONST TokClassSetAsList 
    = TokClassSetTyp 
        { TokClassTyp . TokClassAsPlus 
        , TokClassTyp . TokClassAsPlusTrailing 
        , TokClassTyp . TokClassAsStarTrailing 
        , TokClassTyp . TokClassAsStar 
        } 

; CONST TokClassSetEstSubtree 
    = TokClassSetTyp 
        { TokClassTyp . TokClassVarTerm
        , TokClassTyp . TokClassAsPlus 
        , TokClassTyp . TokClassAsPlusTrailing 
        , TokClassTyp . TokClassAsStarTrailing 
        , TokClassTyp . TokClassAsStar 
        , TokClassTyp . TokClassAsFixed 
        } 

; CONST TokClassSetNTPlaceholder  
    = TokClassSetTyp 
        { TokClassTyp . TokClassAsPlus 
        , TokClassTyp . TokClassAsPlusTrailing 
        , TokClassTyp . TokClassAsStarTrailing 
        , TokClassTyp . TokClassAsStar 
        , TokClassTyp . TokClassAsFixed 
        , TokClassTyp . TokClassSublist 
        } 

; CONST TokClassSetEstSentential  
    = TokClassSetTyp 
        { TokClassTyp . TokClassVarTerm 
        , TokClassTyp . TokClassAsPlus 
        , TokClassTyp . TokClassAsPlusTrailing 
        , TokClassTyp . TokClassAsStarTrailing 
        , TokClassTyp . TokClassAsStar 
        , TokClassTyp . TokClassAsFixed 
        , TokClassTyp . TokClassSublist
        , TokClassTyp . TokClassListCard
        } 
  (* ^Have an Est and can occur in sentential forms. *) 

(* Token numbers/counts. *) 
; TYPE TokNoTyp = PortTypes . Int16Typ 

(* Marked points *) 

; CONST MarkNoMax = 32767 

; TYPE MarkNoTyp = [ 0 .. MarkNoMax ] 
; TYPE MarkNoSignedTyp = [ - 1 .. MarkNoMax ] 

; CONST MarkNoNull = LAST ( MarkNoTyp ) 

; CONST MarkNoImage = PortTypes . Card16Image  

(* Displaying info: *) 
; CONST StdIndent = 2 

(* Versions (of various things) *) 
; TYPE VersionComponentTyp = [ 0 .. 255 ] 
; TYPE VersionComponentPackedTyp = BITS 8 FOR VersionComponentTyp 

; TYPE VersionTyp 
    = RECORD 
        Incompatible : VersionComponentTyp 
      ; UpwardCompatible : VersionComponentTyp 
      ; Compatible : VersionComponentTyp 
      ; Minor : VersionComponentTyp 
      END (* RECORD *) 

; PROCEDURE VersionImage ( Version : VersionTyp ) : TEXT 

(* Info for Schutz-specific files: *) 

; CONST SchutzMagic 
  = ORD ( 'S' ) * 256 * 256 * 256 
    + ORD ( 'c' ) * 256 * 256 
    + ORD ( 'h' ) * 256 
    + ORD ( 'e' ) 

; TYPE PickleKindTyp 
    = { Null , Est , Lang , Sem , Image , Checkpoint } 

; PROCEDURE PickleKindImage ( Kind : PickleKindTyp ) : TEXT 

; TYPE PickleKindSetTyp = SET OF PickleKindTyp  

; CONST PickleKindSetImageYielding  
    = PickleKindSetTyp 
        { PickleKindTyp . Est 
        , PickleKindTyp . Image  
        , PickleKindTyp . Checkpoint 
        }  

; TYPE PickleIdInfoTyp 
    = RECORD 
        Magic : INTEGER := SchutzMagic 
      ; DSVersion : VersionTyp 
      ; Kind : PickleKindTyp 
      END 
; TYPE PickleIdInfoRefTyp = REF PickleIdInfoTyp

(* These will be initialized to point to fixed PickleIdTyp records. 
   They are in the heap, so they can be written/read using the 
   machine-independent mechanism of Pickle. *) 
; VAR EstPickleIdInfoRef : PickleIdInfoRefTyp := NIL 
; VAR LangPickleIdInfoRef : PickleIdInfoRefTyp := NIL 
; VAR SemPickleIdInfoRef : PickleIdInfoRefTyp := NIL 
; VAR ImagePickleIdInfoRef : PickleIdInfoRefTyp := NIL 
; VAR CheckpointPickleIdInfoRef : PickleIdInfoRefTyp := NIL 

; END LbeStd 
. 



