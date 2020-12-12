
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE EstUtil 

(* Many collected medium-level utilities for manipulating Est nodes. *) 

; IMPORT Fmt 
; IMPORT TextWr 
; IMPORT Thread
; IMPORT Wr 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 
; IMPORT EstHs 
; IMPORT LangUtil 
; FROM LangUtil IMPORT PredicateKindTyp 
; IMPORT LbeStd 
; IMPORT MessageCodes
; IMPORT Misc 
; IMPORT ModHs 
; IMPORT PortTypes 
; IMPORT SharedStrings 

; TYPE AFT = MessageCodes . T 

(* EXPORTED *) 
; PROCEDURE HasSyntErrors ( NodeRef : LbeStd . EstRootTyp ) : BOOLEAN 

  = BEGIN 
      TYPECASE NodeRef  
      OF NULL 
      => RETURN FALSE 

      | EstHs . EstRefTyp ( TEstRef )  
      => RETURN EstHs . EstChildKindSetNotSyntacticallyClean 
                * TEstRef . EstChildKindSet 
                # EstHs . EstChildKindSetEmpty 

      ELSE RETURN FALSE
      END (* TYPECASE *)
    END HasSyntErrors 


(* EXPORTED *) 
; PROCEDURE EstTok ( NodeRef : LbeStd . EstRootTyp ) 
  : LbeStd . TokTyp 
  RAISES { AssertionFailure } 
  (* If NodeRef is a string object, returns its token. 
     Otherwise, the EstTok stored in the node. *) 

(* TODO: See if we can turn this into a dispatching method.
         It would require the type hierarchy to be right. 
*) 
  = BEGIN (* EstTok *) 
      TYPECASE NodeRef 
      OF NULL 
      => RETURN LbeStd . Tok__Null 

      | SharedStrings . T ( TSharedString ) 
      => RETURN SharedStrings . Tok ( TSharedString ) 

      | ModHs . EstDummyTokTyp ( TEstDummyTok ) 
      => RETURN TEstDummyTok . Tok  

      | ModHs . EstDummyTyp 
      => RETURN LbeStd . Tok__Null 

      | EstHs . EstRefTyp ( TEstRef ) 
      => RETURN TEstRef . EstTok 

      | ModHs . ModCmntTyp ( TModCmnt ) 
      => IF TModCmnt . ModCmntNlAfter 
         THEN 
           RETURN LbeStd . Tok__CmntAtEndOfLine 
         ELSE 
           RETURN LbeStd . Tok__Cmnt 
         END (* IF *) 

      | ModHs . ModTextTyp 
      => RETURN LbeStd . Tok__ModText 

      | ModHs . ModBlankLineTyp 
      => RETURN LbeStd . Tok__BlankLine 

      | ModHs . ModErrTyp 
      => RETURN LbeStd . Tok__Null 

      ELSE 
        CantHappen ( AFT . A_EstUtilDotEstTokBadObjType ) 
      ; RETURN LbeStd . Tok__Null 
      END (* TYPECASE *) 
    END EstTok 

(* EXPORTED *) 
; PROCEDURE FsRuleForEstNode 
    ( Lang : LbeStd . LangTyp ; NodeRef : LbeStd . EstRootTyp ) 
  : LangUtil . FsNodeRefTyp 
  RAISES { AssertionFailure } 
  (* The FsNodeRef of the root of the format tree for an Est node,
     or an FsNode of FsKindAstString, for an AstString. 
  *) 

  = VAR LTok : LbeStd . TokTyp
  ; VAR LIsPlaceholder : BOOLEAN := FALSE  

  ; BEGIN 
      TYPECASE NodeRef 
      OF ModHs . ModRefTyp (* Including NIL *)  
      => RETURN NIL 

      | SharedStrings . T ( TSharedString ) 
      => LTok := SharedStrings . Tok ( TSharedString ) 
      ; LIsPlaceholder := TRUE 

      | ModHs . EstDummyTokTyp ( TEstDummyTok ) 
      => LTok := TEstDummyTok . Tok  

      | EstHs . EstRefTyp ( TEstRef ) 
      => LTok := TEstRef . EstTok 

      | ModHs . EstDummyTyp 
      => RETURN NIL 

      ELSE 
        CantHappen ( AFT . A_EstUtilDotEstTokBadObjType ) 
      END (* TYPECASE *) 
    ; RETURN LangUtil . FsRuleForTok ( Lang , LTok , LIsPlaceholder ) 
    END FsRuleForEstNode 

(* EXPORTED *) 
; PROCEDURE EstChildKindSet ( NodeRef : LbeStd . EstRootTyp ) 
  : EstHs . EstChildKindSetTyp 
  RAISES { AssertionFailure } 
  (* WARNING: EstChildKindContainsInsertionRepair and 
              EstChildKindContainsSyntMod can't be ascertained from
              the node itself, for ModDel and AstString, nor can
              EstChildKindOptSingletonList for an Est node or AstString.   
              These are omitted in the result. 
  *)  

  = BEGIN (* EstChildKindSet *) 
      TYPECASE NodeRef 
      OF ModHs . EstDummyTyp (* Including NIL *)  
      => RETURN EstHs . EstChildKindSetEmpty  

      | SharedStrings . T 
      => RETURN EstHs . EstChildKindSetEstChildNonNIL  
         (* NOTE: We can't tell just by looking at it whether it should
                  have EstChildKindInsertionRepair. 
         *)  

      | EstHs . EstRefTyp ( TEstRef ) 
      => RETURN TEstRef . EstChildKindSet 

      | ModHs . ModCmntLeadingTyp ( TModCmnt ) 
      => IF TModCmnt . ModCmntNlBefore OR TModCmnt . ModCmntNlAfter 
         THEN 
           RETURN EstHs . EstChildKindSetModCmnt 
                  + EstHs . EstChildKindSetDisplayComputable
         ELSE 
           RETURN EstHs . EstChildKindSetModCmnt 
         END (* IF *) 

      | ModHs . ModCmntTrailingTyp ( TModCmnt ) 
      => IF TModCmnt . ModCmntNlBefore OR TModCmnt . ModCmntNlAfter 
         THEN 
           RETURN EstHs . EstChildKindSetModCmnt 
                  + EstHs . EstChildKindSetDisplayComputable
                  + EstHs . EstChildKindSetTrailingMod
         ELSE 
           RETURN EstHs . EstChildKindSetModCmnt 
                  + EstHs . EstChildKindSetTrailingMod
         END (* IF *) 

      | ModHs . ModTextTyp 
      => RETURN EstHs . EstChildKindSetModText 

      | ModHs . ModBlankLineTyp 
      => RETURN EstHs . EstChildKindSetModBlankLine  

      | ModHs . ModDelTyp 
      => RETURN EstHs . EstChildKindSetNonNIL    
         (* NOTE: We can't tell just by looking at it whether it should
                  have EstChildKindContainsSyntMod or 
                  EstChildKindInsertionRepair. 
         *)  

      | ModHs . ModLexErrTyp 
      => RETURN EstHs . EstChildKindSetModLexErr

      | ModHs . ModSyntErrTyp 
      => RETURN EstHs . EstChildKindSetModSyntErr

      ELSE 
        CantHappen ( AFT . A_EstUtilDotEstTokBadObjType ) 
      ; RETURN EstHs . EstChildKindSetEmpty  
      END (* TYPECASE  CASE *) 
    END EstChildKindSet 

(* EXPORTED *) 
; PROCEDURE EstIsPlaceholder 
    ( Lang : LbeStd . LangTyp ; NodeRef : LbeStd . EstRootTyp ) 
  : BOOLEAN 
  RAISES { AssertionFailure } 
  (* Either from a SharedString . T or an Est node. *) 

  = VAR LTok : LbeStd . TokTyp 
  ; VAR LTokClass : LbeStd . TokClassTyp 

  ; BEGIN (* EstIsPlaceholder *) 
      TYPECASE NodeRef 
      OF ModHs . EstDummyTyp (* Including NIL *) 
      => RETURN FALSE 
      | SharedStrings . T ( TSharedString ) 
      => LTok := SharedStrings . Tok ( TSharedString ) 
      ; LTokClass := LangUtil . TokClass ( Lang , LTok )
      ; CASE LTokClass 
        OF LbeStd . TokClassTyp . TokClassAsPlus
        , LbeStd . TokClassTyp . TokClassAsPlusTrailing 
        , LbeStd . TokClassTyp . TokClassAsStarTrailing 
        , LbeStd . TokClassTyp . TokClassAsStar 
        , LbeStd . TokClassTyp . TokClassAsFixed 
        , LbeStd . TokClassTyp . TokClassSublist 

        => RETURN TRUE 
        | LbeStd . TokClassTyp . TokClassVarTerm 
        => RETURN 
             SharedStrings . Equal 
               ( TSharedString 
               , LangUtil . DisplayStringForTok ( Lang , LTok ) 
               ) 
        | LbeStd . TokClassTyp . TokClassVarTermMod 
        => RETURN 
             SharedStrings . Equal 
               ( TSharedString 
               , LangUtil . DisplayStringForTok 
                   ( Lang , LangUtil . VarTermTok ( Lang , LTok ) ) 
               ) 
        ELSE RETURN FALSE 
        END (* CASE *) 
      | EstHs . EstRefTyp 
      => RETURN FALSE 
      | ModHs . ModRefTyp 
      => RETURN FALSE 
      ELSE 
        CantHappen ( AFT . A_EstUtilDotEstTokBadObjType ) 
      ; RETURN FALSE 
      END (* TYPECASE  CASE *) 
    END EstIsPlaceholder 

(* EXPORTED *) 
; PROCEDURE EstNodeCt 
    ( NodeRef : LbeStd . EstRootTyp ) : LbeStd . EstNodeNoTyp 

  (* We need this to work on NIL too, so it can't be a dispatched method. *) 
  = BEGIN (* EstNodeCt *) 
      TYPECASE NodeRef 
      OF ModHs . EstDummyTyp (* Including NIL *) 
      => RETURN 1 (* NILs must have node numbers so they can be referred 
                       to by a SibFmtNo mark. *) 
      | EstHs . KTreeRefTyp ( TKTreeRef ) 
      => RETURN 1 (* For the root *) + TKTreeRef . KTreeNodeCt ( ) 
(* TODO^ perhaps do this directly here, to avoid checking type twice. *) 
      ELSE 
        RETURN 1 
      END (* TYPECASE *) 
    END EstNodeCt 

(* EXPORTED *) 
; PROCEDURE EstChildCt 
    ( NodeRef : LbeStd . EstRootTyp ) : LbeStd . EstChildNoTyp 

  (* We need this to work on NIL too, so it can't be a dispatched method. *) 
  = BEGIN (* EstChildCt *) 
      TYPECASE NodeRef 
      OF ModHs . EstDummyTyp (* Including NIL *) 
      => RETURN 0 
      | EstHs . KTreeRefTyp ( TKTreeRef ) 
      => RETURN TKTreeRef . KTreeChildCt ( ) 
(* TODO: ^Perhaps do this directly here, to avoid checking type twice. *) 
      ELSE 
        RETURN 0 
      END (* TYPECASE *) 
    END EstChildCt 

(* EXPORTED *) 
; PROCEDURE IsModTok ( NodeRef : LbeStd . EstRootTyp ) : BOOLEAN 

  = BEGIN 
      TYPECASE NodeRef 
      OF ModHs . EstDummyTyp (* Including NIL *) 
      => RETURN FALSE 
      | EstHs . EstRefTyp ( TEstRef )  
      => RETURN TEstRef . EstNodeKind 
         = EstHs . EstNodeKindTyp . EstNodeKindModTok 
      ELSE RETURN FALSE 
      END 
    END IsModTok 

(* EXPORTED *) 
; PROCEDURE VarTermImage
    ( NodeRef : LbeStd . EstRootTyp
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    )
  : TEXT
  (* Token, StringNo, and the string, with escapes. 
     "", if NodRef is not a non-NIL SharedString.T. *)

  = BEGIN
      TYPECASE NodeRef
      OF NULL => RETURN "" 
      | SharedStrings . T ( TSharedString )
      => RETURN
          "{"
          & LangUtil . TokImage
              ( SharedStrings . Tok ( TSharedString ) , Lang ) 
          & "," 
          & SharedStrings . StringNoImage
              ( SharedStrings . StringNo ( TSharedString ) ) 
          & ","
          & Misc . QuoteText ( SharedStrings . ToText ( TSharedString ) ) 
          & "}" 
      ELSE RETURN ""
      END (* TYPECASE *) 
    END VarTermImage 

(* EXPORTED *) 
; PROCEDURE EstNodeImageBrief 
    ( NodeRef : LbeStd . EstRootTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  : TEXT 
  RAISES { AssertionFailure } 
  (* ^Also works on NIL, giving "NIL" *) 

  = VAR LNodeImage , LChildrenImage , LResult : TEXT

  ; BEGIN (* EstNodeImageBrief *) 
      TYPECASE NodeRef 
      OF NULL 
      => RETURN "NIL" 

      | SharedStrings . T ( TSharedString ) 
      => (* Lex error characters are a special case of this, identified
            by Tok__LexErrChars. *)
        RETURN SharedStrings . Image ( TSharedString , Indent (* , Lang *) ) 

      | ModHs . EstDummyTyp ( TDummyRef )  
      => RETURN ModHs . EstDummyImage ( TDummyRef ) 

      | EstHs . EstRefTyp ( TEstRef ) 
      => LResult := EstHs . EstRefImageBrief ( TEstRef , Indent , Lang )
      ; RETURN LResult 

      | ModHs . ModCmntTyp ( TModCmnt ) 
      => RETURN ModHs . ModCmntImage ( TModCmnt , Indent ) 

      | ModHs . ModTextTyp ( TModText ) 
      => RETURN ModHs . ModTextImage ( TModText , Indent ) 

      | ModHs . ModBlankLineTyp ( TModBlankLine ) 
      => RETURN ModHs . ModBlankLineImage ( TModBlankLine , Indent ) 

      | ModHs . ModLexErrTyp ( TModLexErr ) 
      => RETURN ModHs . ModLexErrImage ( TModLexErr , Indent ) 

      | ModHs . ModSyntErrTyp ( TModSyntErr ) 
      => RETURN ModHs . ModSyntErrImage ( TModSyntErr , Indent ) 

      | ModHs . ModDelTyp ( TModDel ) 
      => RETURN ModHs . ModDelImage ( TModDel , Indent ) 

      ELSE 
        CantHappen ( AFT . A_EstUtilDotEstNodeImageBadObjType ) 
      ; RETURN "" 
      END (* TYPECASE  CASE *) 
    END EstNodeImageBrief 

(* EXPORTED *) 
; PROCEDURE EstNodeImage 
    ( NodeRef : LbeStd . EstRootTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ; <* UNUSED *> Mnemonic : BOOLEAN := FALSE 
    ) 
  : TEXT 
  RAISES { AssertionFailure } 
  (* ^Also works on NIL, giving "NIL" *) 

  = BEGIN (* EstNodeImage *) 
      TYPECASE NodeRef 
      OF NULL 
      => RETURN "NIL" 

      | SharedStrings . T ( TSharedString ) 
      => (* Lex error characters are a special case of this, identified
            by Tok__LexErrChars. *)
        RETURN SharedStrings . Image ( TSharedString , Indent (* , Lang *) ) 

      | ModHs . EstDummyTyp ( TDummyRef )  
      => RETURN ModHs . EstDummyImage ( TDummyRef ) 

      | EstHs . EstRefTyp ( TEstRef ) 
      => RETURN TEstRef . Image ( Indent , NodeNo , ChildNo , Lang ) 

      | ModHs . ModCmntTyp ( TModCmnt ) 
      => RETURN ModHs . ModCmntImage ( TModCmnt , Indent ) 

      | ModHs . ModTextTyp ( TModText ) 
      => RETURN ModHs . ModTextImage ( TModText , Indent ) 

      | ModHs . ModBlankLineTyp ( TModBlankLine ) 
      => RETURN ModHs . ModBlankLineImage ( TModBlankLine , Indent ) 

      | ModHs . ModLexErrTyp ( TModLexErr ) 
      => RETURN ModHs . ModLexErrImage ( TModLexErr , Indent ) 

      | ModHs . ModSyntErrTyp ( TModSyntErr ) 
      => RETURN ModHs . ModSyntErrImage ( TModSyntErr , Indent ) 

      | ModHs . ModDelTyp ( TModDel ) 
      => RETURN ModHs . ModDelImage ( TModDel , Indent ) 

      ELSE 
        CantHappen ( AFT . A_EstUtilDotEstNodeImageBadObjType ) 
      ; RETURN "" 
      END (* TYPECASE  CASE *) 
    END EstNodeImage 

(* EXPORTED *)
; PROCEDURE EstLeavesImage
    ( TreeRef : LbeStd . EstRootTyp
    ; NodeNo : LbeStd . EstNodeNoTyp
    ; Indent := LbeStd . StdIndent 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  : TEXT
  (* If TreeRef is an Est interior node, Flatten all its leaf elements and display
     then in a compact, usually one-line form, prefixed
     by node and child numbers.  Otherwise "" *) 

  = CONST NodePrefix = "    " 
  ; CONST NodeIndent = 4 (* = Text . Length ( NodePrefix ) *)

  ; VAR EliEstRef : EstHs . EstRefTyp 
  ; VAR EliNodeNoPad := 7
  ; VAR EliChildNoPad := 4
  ; VAR EliNodeCt : INTEGER 
  ; VAR EliChildCt , EliChildNo : INTEGER
  ; VAR EliResult : TEXT
  ; VAR EliWrT : Wr . T
  ; VAR EliIndentBlanks := ""
  
  ; PROCEDURE EliNonleafArray
      ( ArrayRef : EstHs . NonleafArrayRefTyp 
      ; NodeNo : LbeStd . EstNodeNoTyp 
      ; ChildNo : LbeStd . EstChildNoTyp 
      )

    = VAR LNumber : INTEGER
    ; VAR LSs : INTEGER 
    ; VAR LNodeCt , LNodeNo : INTEGER 
    ; VAR LChildCt , LChildNo : INTEGER 

    ; BEGIN
        IF ArrayRef = NIL THEN RETURN END (* IF *)
      ; LNumber := NUMBER ( ArrayRef ^ )
      ; IF LNumber <= 0 THEN RETURN END (* IF *)
      ; LSs := LAST ( ArrayRef ^ )
      ; WITH WLMElem = ArrayRef ^ [ LSs ]
        DO
          LNodeCt := WLMElem . NleCumNodeCt 
        ; LChildCt := WLMElem . NleCumChildCt
        END (* WITH *) 
      ; WHILE LNumber > 0 
        DO WITH WElem = ArrayRef ^ [ LSs ]
          DO
            LNodeNo := NodeNo + LNodeCt - WElem . NleCumNodeCt  
          ; LChildNo := ChildNo + LChildCt - WElem . NleCumChildCt  
          ; TYPECASE WElem . NleChildRef  
            OF NULL =>

            | EstHs . EstLeafRefTyp ( TLeafRef )
            => EliLeafArray 
                 ( TLeafRef . EstLeafArrayRef , LNodeNo , LChildNo )

            | EstHs . EstNonleafRefTyp ( TNonleafRef )
            => EliNonleafArray
                 ( TNonleafRef . EstNonleafArrayRef , LNodeNo , LChildNo )

            | EstHs . KTreeLeafRefTyp ( TLeafRef )
            => EliLeafArray 
                 ( TLeafRef . KTreeLeafArrayRef , LNodeNo , LChildNo )

            | EstHs . KTreeNonleafRefTyp ( TNonleafRef )
            => EliNonleafArray
                 ( TNonleafRef . KTreeNonleafArrayRef , LNodeNo , LChildNo )

            ELSE
            END (* TYPECASE *)
          END (* WITH *) 
        ; DEC ( LSs ) 
        ; DEC ( LNumber ) 
        END (* WHILE *) 
      END EliNonleafArray

  ; PROCEDURE EliLeafArray
      ( ArrayRef : EstHs . LeafArrayRefTyp 
      ; NodeNo : LbeStd . EstNodeNoTyp 
      ; ChildNo : LbeStd . EstChildNoTyp 
      ) 

    = VAR LNumber : INTEGER
    ; VAR LSs : INTEGER
    ; VAR LNodeCt , LNodeNo : INTEGER
    ; VAR LChildCt , LChildNo : INTEGER

    ; BEGIN
        IF ArrayRef = NIL THEN RETURN END (* IF *)
      ; LNumber := NUMBER ( ArrayRef ^ )
      ; IF LNumber <= 0 THEN RETURN END (* IF *)
      ; LSs := LAST ( ArrayRef ^ )
      ; LNodeCt := ArrayRef ^ [ LSs ] . LeCumNodeCt 
      ; LChildCt := LNumber
      ; LOOP 
          WITH WElem = ArrayRef ^ [ LSs ]
          DO
            LNodeNo := NodeNo + LNodeCt - WElem . LeCumNodeCt  
          ; LChildNo := ChildNo + LChildCt - LNumber
          ; Wr . PutText ( EliWrT , "NodeNo " )
          ; Wr . PutText
              ( EliWrT
              , Fmt . Pad ( LbeStd . EstNodeNoImage ( LNodeNo ) , EliNodeNoPad )
              )
          ; Wr . PutText ( EliWrT , " ChildNo " )
          ; Wr . PutText
              ( EliWrT
              , Fmt . Pad ( LbeStd . EstNodeNoImage ( LChildNo ) , EliChildNoPad )
              )
          ; Wr . PutChar ( EliWrT , ' ' )
          ; Wr . PutText ( EliWrT , Misc . RefanyImage ( WElem . LeChildRef ) )
          ; Wr . PutText ( EliWrT , " FmtNo " )
          ; Wr . PutText ( EliWrT , EstHs . FmtNoImage ( WElem . LeFmtNo ) ) 
          ; Wr . PutText ( EliWrT , " KindSet " )
          ; Wr . PutText
              ( EliWrT
              , EstHs . EstChildKindSetImage
                  ( WElem . LeKindSet
                  , ImageKind := EstHs . ImageKindTyp . Decimal 
                  , RightMargin := 10000 (* Prevent wrap. *)
                  )
              )
          ; Wr . PutText ( EliWrT , Wr . EOL )
          ; Wr . PutText ( EliWrT , EliIndentBlanks )
          ; Wr . PutText ( EliWrT , NodePrefix  )
          ; Wr . PutText
              ( EliWrT
              , EstNodeImageBrief
                  ( WElem . LeChildRef , Indent + NodeIndent , LNodeNo , Lang )
              )
          END (* WITH *)
        ; INC ( EliChildNo )
        ; IF EliChildNo < EliChildCt
          THEN
            Wr . PutText ( EliWrT , Wr . EOL )
          ; Wr . PutText ( EliWrT , EliIndentBlanks )
          ; Wr . PutText ( EliWrT , ", " )
          END (* IF *)
        ; DEC ( LNumber )
        ; IF LNumber <= 0
          THEN EXIT
          ELSE DEC ( LSs ) 
          END (* IF *) 
        END (* LOOP *)
      END EliLeafArray 

  ; BEGIN (* EstLeavesImage *)
      TYPECASE TreeRef OF
      | NULL => RETURN ""
      | EstHs . EstRefTyp ( TEstRef )
      => EliEstRef := TEstRef 
      ELSE RETURN ""
      END (* TYPECASE *) 
    ; EliChildCt := EstChildCt ( EliEstRef ) 
    ; IF EliChildCt <= 0
      THEN RETURN "{ }"
      ELSE
        EliNodeCt := EstNodeCt ( EliEstRef )
      ; EliNodeNoPad := Misc . CeilLog10 ( EliNodeCt ) 
      ; EliChildNoPad := Misc . CeilLog10 ( EliChildCt )
      ; EliChildNo := 0 
      ; EliWrT := TextWr . New ( ) 
      ; IF EliChildCt > 1
        THEN EliIndentBlanks := Misc . Blanks ( Indent )
        END (* IF *)

      ; Wr . PutText ( EliWrT , "{ " )
      ; TYPECASE EliEstRef 
        OF NULL => 
        | EstHs . EstLeafRefTyp ( TEstLeaf ) 
        => EliLeafArray ( TEstLeaf . EstLeafArrayRef , NodeNo , 0 )
        | EstHs . EstNonleafRefTyp ( TEstNonleaf ) 
        => EliNonleafArray ( TEstNonleaf . EstNonleafArrayRef , NodeNo , 0 )
        ELSE
        END (* TYPECASE *)
      ; IF EliChildCt > 1
        THEN
          Wr . PutText ( EliWrT , Wr . EOL )
        ; Wr . PutText ( EliWrT , EliIndentBlanks )
        END (* IF *)
      ; Wr . PutChar ( EliWrT , '}' )
      ; EliResult := TextWr . ToText ( EliWrT )
      ; RETURN EliResult
      END (* IF *)
    END EstLeavesImage 

; PROCEDURE WidthValue 
    ( Arg : PortTypes . Int32Typ ) : LbeStd . LimitedCharNoTyp 

  = BEGIN (* WidthValue *) 
      IF Arg < 0 
      THEN 
        RETURN 0 
      ELSIF Arg > LbeStd . LimitedCharNoInfinity 
      THEN 
        RETURN LbeStd . LimitedCharNoInfinity 
      ELSE 
        RETURN Arg 
      END (* IF *) 
    END WidthValue 

(* EXPORTED *) 
; PROCEDURE WidthSum 
    ( Left , Right : LbeStd . LimitedCharNoSignedTyp ) 
  : LbeStd . LimitedCharNoTyp 
  (* Saturates at LbeStd . LimitedCharNoInfinity. 
     Limits at zero.
  *) 

  = BEGIN (* WidthSum *) 
      IF Left >= LbeStd . LimitedCharNoInfinity 
         OR Right >= LbeStd . LimitedCharNoInfinity 
         OR LbeStd . LimitedCharNoInfinity - Left <= Right 
      THEN 
        RETURN LbeStd . LimitedCharNoInfinity 
      ELSIF - Left >= Right 
      THEN 
        RETURN 0 
      ELSE 
        RETURN Left + Right 
      END (* IF *) 
    END WidthSum 

(* EXPORTED *) 
; PROCEDURE WidthSumSigned 
    ( Left , Right : LbeStd . LimitedCharNoSignedTyp ) 
  : LbeStd . LimitedCharNoSignedTyp 
  (* Saturates at LbeStd . LimitedCharNoInfinity. 
     Also saturates at LbeStd . LimitedCharNoMinusInfinity. 
  *) 

  = BEGIN (* WidthSumSigned *) 
      IF Left >= LbeStd . LimitedCharNoInfinity 
         OR Right >= LbeStd . LimitedCharNoInfinity 
         OR LbeStd . LimitedCharNoInfinity - Left <= Right 
      THEN 
        RETURN LbeStd . LimitedCharNoInfinity 
      ELSIF Left <= LbeStd . LimitedCharNoMinusInfinity 
            OR Right <= LbeStd . LimitedCharNoMinusInfinity 
            OR LbeStd . LimitedCharNoMinusInfinity - Left >= Right 
      THEN 
        RETURN LbeStd . LimitedCharNoMinusInfinity 
      ELSE 
        RETURN Left + Right 
      END (* IF *) 
    END WidthSumSigned 

(* EXPORTED *) 
; PROCEDURE WidthSum3 
    ( W , X , Y : LbeStd . LimitedCharNoSignedTyp ) 
  : LbeStd . LimitedCharNoTyp 
  (* Saturates at LbeStd . LimitedCharNoInfinity. 
     Limits at zero.
  *) 

  = BEGIN (* WidthSum3 *) 
      IF W = LbeStd . LimitedCharNoInfinity 
         OR X = LbeStd . LimitedCharNoInfinity 
         OR Y = LbeStd . LimitedCharNoInfinity 
      THEN 
        RETURN LbeStd . LimitedCharNoInfinity 
      ELSE 
        RETURN MIN ( LbeStd . LimitedCharNoInfinity , MAX ( 0 , W + X + Y ) ) 
(* NOTE: ^This will not be overflow-proof if the range of builtin 
         integer arithmetic is not at least 3 times 
         LbeStd . LimitedCharNoSignedTyp 
*)  
      END (* IF *) 
    END WidthSum3 

(* EXPORTED *) 
; PROCEDURE WidthSumSigned3 
    ( W , X , Y : LbeStd . LimitedCharNoSignedTyp ) 
  : LbeStd . LimitedCharNoSignedTyp 
  (* Saturates at LbeStd . LimitedCharNoInfinity. 
     Also saturates at LbeStd . LimitedCharNoMinusInfinity. 
  *) 

  = BEGIN (* WidthSum3 *) 
      IF W = LbeStd . LimitedCharNoInfinity 
         OR X = LbeStd . LimitedCharNoInfinity 
         OR Y = LbeStd . LimitedCharNoInfinity 
      THEN 
        RETURN LbeStd . LimitedCharNoInfinity 
      ELSIF W = LbeStd . LimitedCharNoMinusInfinity 
         OR X = LbeStd . LimitedCharNoMinusInfinity 
         OR Y = LbeStd . LimitedCharNoMinusInfinity 
      THEN 
        RETURN LbeStd . LimitedCharNoMinusInfinity 
      ELSE 
        RETURN 
          MIN ( LbeStd . LimitedCharNoInfinity 
              , MAX ( LbeStd . LimitedCharNoMinusInfinity  , W + X + Y ) 
              ) 
(* NOTE: ^This will not be overflow-proof if the range of builtin 
         integer arithmetic is not at least 3 times 
         LbeStd . LimitedCharNoSignedTyp 
*)  
      END (* IF *) 
    END WidthSumSigned3 

(* EXPORTED *) 
; PROCEDURE CharPosPlusWidthInfo 
    ( Left : LbeStd . LimitedCharNoSignedTyp 
    ; READONLY Right : EstHs . WidthInfoTyp 
    ) 
  : LbeStd . LimitedCharNoTyp 
  RAISES { AssertionFailure } 
  (* Prepend a left starting CharPos to a right WidthInfo, giving an ending
     CharPos. 
     Return infinity if won't fit on a full-length line. 
  *) 

  = BEGIN (* CharPosPlusWidthInfo *) 
(* CHECK or FIXME: What if WiHasNlBefore? Would need a PrevChar. *) 
      IF Right . WiNlTrigger = LbeStd . LimitedCharNoInfinity 
      THEN RETURN WidthSum ( Left , Right . WiWidth ) 
      ELSIF Left <= Right . WiNlTrigger 
      THEN RETURN Right . WiWidth 
      ELSE RETURN LbeStd . LimitedCharNoInfinity 
      END (* IF *) 
    END CharPosPlusWidthInfo 

(* EXPORTED *) 
; PROCEDURE WidthInfoCat 
    ( Left : EstHs . WidthInfoTyp 
    ; NeedsSep : BOOLEAN 
    ; Right : EstHs . WidthInfoTyp 
    ) 
    : EstHs . WidthInfoTyp 
  (* Compute the width info for the concatenation of two texts whose width 
     info values are Left and Right.  NeedsSep tells whether a token separator
     is needed between. 
  *) 

  = VAR LResult : EstHs . WidthInfoTyp 
  ; VAR LLeftEffectiveWidth , LRightEffectiveWidth : LbeStd . CharNoTyp 
  ; VAR LNlWithinAtJoin : BOOLEAN (* Not necessarily properly inside. *) 

  ; BEGIN (* WidthInfoCat *) 
(* TODO: Prove this is associative. *) 

      IF EstHs . WidthInfoIsNull ( Left ) 
      THEN 
        IF EstHs . WidthInfoIsNull ( Right ) 

        THEN (* Left and Right are both Null. *) 
          IF NeedsSep 
          THEN RETURN EstHs. WidthInfoSep 
          ELSE RETURN EstHs . WidthInfoNull 
          END (* IF *) 

        ELSE (* Left is Null, Right is non-Null. *) 
          IF NeedsSep 
          THEN (* Cut down on combinatorics with a little recursion. *) 
            RETURN WidthInfoCat ( EstHs . WidthInfoSep , FALSE , Right ) 
          ELSE RETURN Right
          END (* IF *) 
        END (* IF *) 

      ELSIF EstHs . WidthInfoIsNull ( Right ) 
      THEN (* Left is non-Null, Right is Null. *) 
        IF NeedsSep 
        THEN (* Cut down on combinatorics with a little recursion. *) 
          RETURN WidthInfoCat ( Left , FALSE , EstHs . WidthInfoSep ) 
        ELSE RETURN Left
        END (* IF *) 

      ELSE (* Left and Right are both non-Null. *) 
        LLeftEffectiveWidth := Left . WiWidth 
      ; LRightEffectiveWidth := Right . WiWidth 
      ; IF LLeftEffectiveWidth > 0  
           AND Left . WiHasNlAfter 
           AND ( NeedsSep OR LRightEffectiveWidth > 0 ) 
        THEN LNlWithinAtJoin := TRUE 
        ELSIF ( LLeftEffectiveWidth > 0 OR NeedsSep )  
              AND Right . WiHasNlBefore 
              AND LRightEffectiveWidth > 0   
        THEN LNlWithinAtJoin := TRUE 
        ELSE LNlWithinAtJoin := FALSE 
        END (* IF *) 
      ; IF LLeftEffectiveWidth = LbeStd . LimitedCharNoInfinity 
              OR LRightEffectiveWidth = LbeStd . LimitedCharNoInfinity 
              OR LNlWithinAtJoin 
        THEN (* Infinite width result. *) 
          LResult . WiNlTrigger := 0
(* CHEDK: ^Do we really want to do this? *)  
        ; LResult . WiWidth := LbeStd . LimitedCharNoInfinity

        ELSE (* Both widths are finite. *) 
          IF Right . WiNlTrigger = LbeStd . LimitedCharNoInfinity 
          THEN (* Right has relative width. *)  
            LResult . WiWidth 
              := WidthSum3 
                   ( LLeftEffectiveWidth 
                   , ORD ( NeedsSep ) 
                   , LRightEffectiveWidth 
                   )
          ; IF LResult . WiWidth = LbeStd . LimitedCharNoInfinity 
            THEN LResult . WiNlTrigger := 0
            ELSE LResult . WiNlTrigger := Left . WiNlTrigger 
            END (* IF *) 
          ELSIF LLeftEffectiveWidth + LbeStd . SepWidth * ORD ( NeedsSep ) 
                > Right . WiNlTrigger 

          THEN (* Right has absolute width and left will trigger it, regardless
                  whether it its width is relative or absolute. *) 
            LResult . WiWidth := LbeStd . LimitedCharNoInfinity 
          ; LResult . WiNlTrigger := 0 

          ELSE (* Right has absolute width, not necessarily triggered. *) 
            LResult . WiWidth := LRightEffectiveWidth (* Absolute end point. *) 
          ; IF Left . WiNlTrigger = LbeStd . LimitedCharNoInfinity 
            THEN (* Left has relative width. *) 
              LResult . WiNlTrigger 
                := Right . WiNlTrigger 
                   - LbeStd . SepWidth * ORD ( NeedsSep ) 
                   - LLeftEffectiveWidth 

            ELSE (* Left has absolute width.  From above, it won't trigger 
                    Right. *) 
              LResult . WiNlTrigger := Left . WiNlTrigger 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 

      ; LResult . WiIsNull := FALSE 
      ; LResult . WiHasAbsFromPos := Left . WiHasAbsFromPos 
      ; LResult . WiHasNlBefore := Left . WiHasNlBefore 
      ; LResult . WiHasNlAfter := Right . WiHasNlAfter 
      ; LResult . WiHasNlWithin 
          := Left . WiHasNlWithin OR Right . WiHasNlWithin OR LNlWithinAtJoin  
      ; LResult . WiWholeLineModsOnly 
          := Left . WiWholeLineModsOnly AND Right . WiWholeLineModsOnly 
      END (* IF *) 

    ; RETURN LResult 
    END WidthInfoCat 

; PROCEDURE SearchLeafArrayForNodeNo 
    ( READONLY LeafArray : EstHs . LeafArrayTyp 
    ; NodeCt : LbeStd . EstNodeNoTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp 
      (* ^NodeCt and NodeNo need only be consistent about whether they include 
         the root node. If they do and NodeNo is 0, will return LM Element. *) 
    ) 
  : [ 0 .. EstHs . ElemCtMax - 1 ] 
  RAISES { AssertionFailure } 

  = VAR LLoElemNo : EstHs . ElemNoTyp 
    (* LLoElemNo is for the element containing the low numbered 
       child, but it is the high valued subscript, because elements 
       are subscripted right to left. *) 
  ; VAR LHiElemNo : EstHs . ElemNoTyp 
  ; VAR LProbeElemNo : EstHs . ElemNoTyp 

  ; BEGIN (* SearchLeafArrayForNodeNo *) 
      Assert 
        ( NodeNo <= NodeCt , AFT . A_SearchLeafArrayForNodeNoExcessiveNodeNo ) 
    ; LLoElemNo := NUMBER ( LeafArray ) - 1 
    ; LHiElemNo := 0 
      (* INVARIANT: Child containing NodeNo belongs inside 
                    [ LLoElemNo .. LHiElemNo ] *) 
    ; WHILE LLoElemNo > LHiElemNo 
      DO LProbeElemNo := ( LLoElemNo + LHiElemNo ) DIV 2 
      ; IF NodeNo < ( NodeCt - LeafArray [ LProbeElemNo ] . LeCumNodeCt ) 
        THEN 
          LHiElemNo := LProbeElemNo + 1 
        ELSE 
          LLoElemNo := LProbeElemNo 
        END (* IF *) 
      END (* WHILE *) 
    ; RETURN LLoElemNo 
    END SearchLeafArrayForNodeNo 

; PROCEDURE SearchNonleafArrayForNodeNo 
    ( READONLY NonleafArray : EstHs . NonleafArrayTyp 
    ; NodeCt : LbeStd . EstNodeNoTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp 
      (* ^NodeCt and NodeNo need only be consistent about whether they include 
         the root node. If they do and NodeNo is 0, will return LM Element. *) 
    ) 
  : [ 0 .. EstHs . ElemCtMax - 1 ] 
  RAISES { AssertionFailure }  

  = VAR LLoElemNo : EstHs . ElemNoTyp 
    (* LLoElemNo is for the element containing the low numbered 
       child, but it is the high valued subscript, because elements 
       are subscripted right to left. *) 
  ; VAR LHiElemNo : EstHs . ElemNoTyp 
  ; VAR LProbeElemNo : EstHs . ElemNoTyp 

  ; BEGIN (* SearchNonleafArrayForNodeNo *) 
      Assert 
        ( NodeNo <= NodeCt 
        , AFT . A_SearchNonleafArrayForNodeNoExcessiveNodeNo 
        ) 
    ; LLoElemNo := NUMBER ( NonleafArray ) - 1 
    ; LHiElemNo := 0 
      (* invariant: Child containing NodeNo belongs inside 
                    [ LLoElemNo .. LHiElemNo ] *) 
    ; WHILE LLoElemNo > LHiElemNo 
      DO LProbeElemNo := ( LLoElemNo + LHiElemNo ) DIV 2 
      ; IF NodeNo < ( NodeCt - NonleafArray [ LProbeElemNo ] . NleCumNodeCt ) 
        THEN 
          LHiElemNo := LProbeElemNo + 1 
        ELSE 
          LLoElemNo := LProbeElemNo 
        END (* IF *) 
      END (* WHILE *) 
    ; RETURN LLoElemNo 
    END SearchNonleafArrayForNodeNo 

(* EXPORTED *) 
; PROCEDURE GetEstChildContainingRelNodeNo 
    ( NodeRef : EstHs . KTreeRefTyp 
    ; RelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultChildNo : LbeStd . EstChildNoTyp 
      (* The ChildNo of the child gotten. *) 
    ; VAR ResultChildRelNodeNo : LbeStd . EstNodeNoTyp 
      (* ^The node number of the root of child subtree number ResultChildNo, 
         relative to the parent rooted at NodeRef. *) 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    ) 
  RAISES { AssertionFailure } 

  = VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LNodeCt : LbeStd . EstNodeNoTyp 
  ; VAR LEstNodeCt : LbeStd . EstNodeNoTyp 
  ; VAR LNodeRef : EstHs . KTreeRefTyp 
  ; VAR LLeafArrayRef : EstHs . LeafArrayRefTyp 
  ; VAR LNonleafArrayRef : EstHs . NonleafArrayRefTyp 
  ; VAR LElemSs : EstHs . ElemSsTyp 
  ; VAR LElemCt : EstHs . ElemNoTyp 

  ; BEGIN (* GetEstChildContainingRelNodeNo *) 
      ResultLeafElem := EstHs . LeafElemNull 
    ; IF NodeRef = NIL OR RelNodeNo <= 0 
      THEN (* We are off the Left end. *) 
        ResultChildNo := - 1 
      ; ResultChildRelNodeNo := 0  
      ELSE 
        LEstNodeCt := EstNodeCt ( NodeRef ) 
      ; IF RelNodeNo >= LEstNodeCt 
        THEN (* We are off the right end. *) 
          ResultChildNo := EstChildCt ( NodeRef ) 
        ; ResultChildRelNodeNo := LEstNodeCt 
        ELSE 
          ResultChildNo := 0 
        ; LNodeNo := RelNodeNo - 1 (* Don't include the Est root. *) 
        ; LNodeRef := NodeRef 
        ; LOOP 
            TYPECASE LNodeRef 
            OF EstHs . KTreeLeafRefTyp , EstHs . EstLeafRefTyp 
            => LLeafArrayRef := LNodeRef . LeafArrayRef ( ) 
            ; LElemCt := NUMBER ( LLeafArrayRef ^ ) 
            ; LNodeCt := LLeafArrayRef ^ [ LElemCt - 1 ] . LeCumNodeCt 
            ; LElemSs 
                := SearchLeafArrayForNodeNo 
                     ( LLeafArrayRef ^ , LNodeCt , LNodeNo ) 
            ; INC ( ResultChildNo , LElemCt - LElemSs - 1 ) 
            ; DEC 
                ( LNodeNo , LNodeCt - LLeafArrayRef ^ [ LElemSs ] . LeCumNodeCt ) 
            ; ResultChildRelNodeNo := RelNodeNo - LNodeNo 
            ; ResultLeafElem := LLeafArrayRef ^ [ LElemSs ] 
            ; EXIT 
            | EstHs . KTreeNonleafRefTyp , EstHs . EstNonleafRefTyp 
            => LNonleafArrayRef := LNodeRef . NonleafArrayRef ( ) 
            ; LElemCt := NUMBER ( LNonleafArrayRef ^ ) 
            ; LNodeCt := LNonleafArrayRef ^ [ LElemCt - 1 ] . NleCumNodeCt 
            ; LElemSs 
                := SearchNonleafArrayForNodeNo 
                     ( LNonleafArrayRef ^ , LNodeCt , LNodeNo ) 
            ; INC 
                ( ResultChildNo 
                , LNonleafArrayRef ^ [ LElemCt - 1 ] . NleCumChildCt 
                  - LNonleafArrayRef ^ [ LElemSs ] . NleCumChildCt 
                ) 
            ; DEC 
                ( LNodeNo 
                , LNodeCt - LNonleafArrayRef ^ [ LElemSs ] . NleCumNodeCt 
                ) 
            ; LNodeRef := LNonleafArrayRef ^ [ LElemSs ] . NleChildRef 
            ELSE 
              CantHappen ( AFT . A_GetEstChildContainingRelNodeNo_BadNodeType ) 
            END (* TYPECASE *) 
          END (* LOOP *) 
        END (* IF *) 
      END (* IF *) 
    END GetEstChildContainingRelNodeNo 

(* EXPORTED *) 
; PROCEDURE SearchNonleafArrayForChild 
    ( READONLY NonleafArray : EstHs . NonleafArrayTyp 
    ; ChildCt : LbeStd . EstChildNoTyp 
    ; HiElemNo : EstHs . ElemNoTyp 
      (* ^Caller knows no further left element (actually lowest 
          value of element number, since elements are numbered 
          right to left) could contain ChildNo. *) 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
  : EstHs . ElemNoTyp 
  RAISES { AssertionFailure } 

  = VAR LLoElemNo : EstHs . ElemNoTyp 
    (* LLoElemNo is for the element containing the low numbered 
       child, but it is the high valued subscript, because elements 
       are subscripted right to left. *) 
  ; VAR LHiElemNo : EstHs . ElemNoTyp 
  ; VAR LProbeElemNo : EstHs . ElemNoTyp 

  ; BEGIN (* SearchNonleafArrayForChild *) 
      Assert 
        ( ChildNo <= ChildCt 
        , AFT . A_SearchNonleafArrayForChildExcessiveChildNo 
        ) 
    ; LLoElemNo := NUMBER ( NonleafArray ) - 1 
    ; LHiElemNo := HiElemNo 
      (* INVARIANT: ChildNo belongs inside 
                    [ LLoElemNo .. LHiElemNo ] *) 
    ; WHILE LLoElemNo > LHiElemNo 
      DO LProbeElemNo := ( LLoElemNo + LHiElemNo ) DIV 2 
      ; IF ChildNo 
           < ( ChildCt - NonleafArray [ LProbeElemNo ] . NleCumChildCt ) 
        THEN 
          LHiElemNo := LProbeElemNo + 1 
        ELSE 
          LLoElemNo := LProbeElemNo 
        END (* IF *) 
      END (* WHILE *) 
    ; RETURN LLoElemNo 
    END SearchNonleafArrayForChild 

; TYPE LevelInfoTyp 
    = RECORD 
        LiNodeRef : EstHs . KTreeRefTyp 
      ; LiChildNo : LbeStd . EstChildNoTyp 
        (* ^Only these two need be initialized when calling. *) 
      ; LiChildCt : LbeStd . EstChildNoTyp 
      ; LiRootNodeNo : LbeStd . EstNodeNoTyp 
      ; LiHeight : EstHs . KTreeHeightTyp 
      ; LiElemNo : EstHs . ElemNoTyp 
      ; LiLeafArrayRef : EstHs . LeafArrayRefTyp 
        (* ^NIL if not a leaf. *) 
      END (* RECORD  LevelInfoTyp *) 

; TYPE VisitNonleafLevelProcTyp 
    = PROCEDURE ( VAR NonleafElem : EstHs . NonleafElemTyp ) 


; PROCEDURE DescendThruNonleafLevels 
    ( VAR (* IN OUT *) LevelInfo : LevelInfoTyp 
    ; Visit : VisitNonleafLevelProcTyp 
    ) 
  RAISES { AssertionFailure } 
 (* PRE: LevelInfo . LiNodeRef and LiHeight are set for a KTreeRefTyp, and
         LiChildNo is set for a child of it that the caller wants to get to. 
    Descend through any nonleaf levels toward this child.  Call Visit on
    the NonLeafElemTyp of any nonleaf level descended through. 
    POST: LiChildCt, LiRootNodeNo, LiElemNo, LiLeafArrayRef are properly
          set for the leaf level and desired original LiChildNo.  
          LiRootNodeNo is the child NodeNo relative to the original LiNodeRef.
 *)  

  = VAR LNonleafArrayRef : EstHs . NonleafArrayRefTyp 

  ; BEGIN (* DescendThruNonleafLevels *)
      LevelInfo . LiChildCt := LevelInfo . LiNodeRef . KTreeChildCt ( ) 
    ; Assert 
        ( 0 <= LevelInfo . LiChildNo 
          AND LevelInfo . LiChildNo < LevelInfo . LiChildCt 
        , AFT . A_DescendThruNonleafLevels_Bad_child_number 
        ) 
    ; LevelInfo . LiRootNodeNo := 1 
    ; LOOP (* Down thru nonleaf levels. *) 
        IF LevelInfo . LiHeight <= 1 
        THEN (* The leaf level *) 
          LevelInfo . LiElemNo 
            := LevelInfo . LiNodeRef . KTreeElemCt 
               - 1 
               - LevelInfo . LiChildNo 
        ; LevelInfo . LiLeafArrayRef 
            := LevelInfo . LiNodeRef . LeafArrayRef ( ) 
        ; WITH 
            WLeafElem = LevelInfo . LiLeafArrayRef ^ [ LevelInfo . LiElemNo ] 
          DO INC 
               ( LevelInfo . LiRootNodeNo 
               , LevelInfo . LiNodeRef . KTreeNodeCt ( ) 
                 - WLeafElem . LeCumNodeCt 
               ) 
          END (* WITH *) 
        ; EXIT 
        ELSE 
          LNonleafArrayRef := LevelInfo . LiNodeRef . NonleafArrayRef ( ) 
        ; LevelInfo . LiElemNo 
            := SearchNonleafArrayForChild 
                 ( LNonleafArrayRef ^ 
                 , LevelInfo . LiChildCt 
                 , 0 
                 , LevelInfo . LiChildNo 
                 ) 
        ; WITH WNonleafElem = LNonleafArrayRef ^ [ LevelInfo . LiElemNo ] 
          DO INC 
               ( LevelInfo . LiRootNodeNo 
               , LevelInfo . LiNodeRef . KTreeNodeCt ( ) 
                 - WNonleafElem . NleCumNodeCt 
               ) 
          ; DEC 
              ( LevelInfo . LiChildNo 
              , LevelInfo . LiChildCt - WNonleafElem . NleCumChildCt 
              ) 
          ; LevelInfo . LiNodeRef := WNonleafElem . NleChildRef 
          ; LevelInfo . LiChildCt := LevelInfo . LiNodeRef . KTreeChildCt ( ) 
          ; Visit ( WNonleafElem ) 
          END (* WITH WNonleafElem *) 
        ; DEC ( LevelInfo . LiHeight ) 
        END (* IF *) 
      END (* LOOP Down thru nonleaf levels. *) 
    END DescendThruNonleafLevels 

(* Operations on child lists *) 

; PROCEDURE VisitNoop 
    ( <* UNUSED *> VAR NonleafElem : EstHs . NonleafElemTyp ) 

  = BEGIN (* VisitNoop *) 
    END VisitNoop 

(* EXPORTED *) 
; PROCEDURE IthChildRef 
    ( EstRef : EstHs . EstRefTyp ; I : LbeStd . EstChildNoTyp ) 
  : LbeStd . EstRootTyp  
  RAISES { AssertionFailure }  

(* TODO: Inline DescendThruNonleafLevels and its noop callback Visit. *) 
  = VAR LLevelInfo : LevelInfoTyp 

  ; BEGIN (* IthChildRef *) 
      IF EstRef = NIL OR I < 0 
      THEN
        RETURN NIL 
      ELSE 
        LLevelInfo . LiNodeRef := EstRef 
      ; LLevelInfo . LiChildNo := I 
      ; LLevelInfo . LiHeight := EstRef . EstHeight 
      ; DescendThruNonleafLevels ( LLevelInfo , VisitNoop ) 
      ; Assert ( LLevelInfo . LiHeight = 1 , AFT . A_IthChildRefHeightNe1 ) 
      ; RETURN 
          LLevelInfo . LiLeafArrayRef ^ [ LLevelInfo . LiElemNo ] . LeChildRef 
      END (* IF *) 
    END IthChildRef 

(* EXPORTED *) 
; PROCEDURE GetIthChild 
    ( EstRef : EstHs . EstRefTyp 
    ; I : LbeStd . EstChildNoTyp 
    ; VAR ResultChildRelNodeNo : LbeStd . EstNodeNoTyp 
      (* NodeNo of root of gotten child relative to EstRef.  If this is an
         optimized-away singleton list, this will be the NodeNo of its 
         one list element, corresponding to ResultLeafElem. 
      *) 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    )
  RAISES { AssertionFailure }  

(* TODO: Inline DescendThruNonleafLevels and its noop callback Visit. *) 
  = VAR LLevelInfo : LevelInfoTyp 

  ; BEGIN (* GetIthChild *) 
      IF EstRef = NIL OR I < 0 
      THEN
        ResultChildRelNodeNo := 0 
      ; ResultLeafElem := EstHs . LeafElemNull 
      ELSE 
        LLevelInfo . LiNodeRef := EstRef 
      ; LLevelInfo . LiChildNo := I 
      ; LLevelInfo . LiHeight := EstRef . EstHeight 
      ; DescendThruNonleafLevels ( LLevelInfo , VisitNoop ) 
      ; Assert ( LLevelInfo . LiHeight = 1 , AFT . A_GetIthChildHeightNe1 ) 
      ; ResultLeafElem 
          := LLevelInfo . LiLeafArrayRef ^ [ LLevelInfo . LiElemNo ] 
(* TODO: Can this be changed to avoid copying a leaf element? 
         It would probably involve substantial rework. 
*) 
      ; ResultChildRelNodeNo := LLevelInfo . LiRootNodeNo 
      END (* IF *) 
    END GetIthChild 

(* EXPORTED *) 
; PROCEDURE GetParent 
    ( RootRef : EstHs . EstRefTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp 
      (* ^Of the node whose parent is to be found, relative to RootRef. *)  
    ; VAR ResultNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultNodeRef : EstHs . EstRefTyp 
    ) 
  RAISES { AssertionFailure } 
  (* If NodeNo is either an optimized singleton list or its element, will 
     give the parent of the list. *)  
  (* Will not give any kind of pseudo-node for an optimized singleton list. *) 

  = VAR LNodeRef : EstHs . EstRefTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildNo : LbeStd . EstChildNoTyp 
  ; VAR LChildNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LLeafElem : EstHs . LeafElemTyp 

  ; BEGIN (* GetParent *) 
      IF RootRef # NIL AND 0 < NodeNo AND NodeNo < EstNodeCt ( RootRef ) 
      THEN 
        LNodeRef := RootRef 
      ; LNodeNo := NodeNo 
      ; LOOP 
          GetEstChildContainingRelNodeNo 
            ( NodeRef := LNodeRef 
            , RelNodeNo := LNodeNo 
            , (* VAR *) ResultChildNo := LChildNo 
            , (* VAR *) ResultChildRelNodeNo := LChildNodeNo 
            , (* VAR *) ResultLeafElem := LLeafElem 
            ) 
        ; IF LChildNodeNo = LNodeNo 
          THEN (* The immediate child has exactly NodeNo. *) 
            EXIT 
          ELSIF LChildNodeNo + 1 = LNodeNo 
                AND EstHs . EstChildKindOptSingletonList 
                    IN LLeafElem . LeKindSet 
          THEN (* The child is the element of an optimized singleton list. *) 
            EXIT 
          ELSE (* The is a subtree properly containing node NodeNo *) 
            DEC ( LNodeNo , LChildNodeNo ) 
          ; LNodeRef := LLeafElem . LeChildRef 
          END (* IF *) 
        END (* LOOP *) 
      ; ResultNodeNo := NodeNo - LNodeNo 
      ; ResultNodeRef := LNodeRef 
      ELSE 
        ResultNodeNo := LbeStd . EstNodeNoNull 
      ; ResultNodeRef := NIL 
      END (* IF *) 
    END GetParent 

(* EXPORTED *) 
; PROCEDURE GetLeafElem 
    ( RootRef : EstHs . EstRefTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    ) 
  RAISES { AssertionFailure }
  (* If NodeNo is an optimized singleton list, will give its element. *)  

  = VAR LNodeRef : EstHs . EstRefTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildNo : LbeStd . EstChildNoTyp 
  ; VAR LChildNodeNo : LbeStd . EstNodeNoTyp 

  ; BEGIN (* GetLeafElem *) 
      IF RootRef # NIL AND 0 < NodeNo AND NodeNo < EstNodeCt ( RootRef ) 
      THEN 
        LNodeRef := RootRef 
      ; LNodeNo := NodeNo 
      ; LOOP 
          GetEstChildContainingRelNodeNo 
            ( NodeRef := LNodeRef 
            , RelNodeNo := LNodeNo 
            , (* VAR *) ResultChildNo := LChildNo 
            , (* VAR *) ResultChildRelNodeNo := LChildNodeNo 
            , (* VAR *) ResultLeafElem := ResultLeafElem 
            ) 
        ; IF LChildNodeNo = LNodeNo 
          THEN (* The immediate child has NodeNo. *) 
            EXIT 
          ELSIF LChildNodeNo + 1 = LNodeNo 
                AND EstHs . EstChildKindOptSingletonList 
                    IN ResultLeafElem . LeKindSet 
          THEN (* The child is the element of an optimized singleton list. *) 
            EXIT 
          ELSE (* The immediate child is a subtree that properly 
                  contains node NodeNo *) 
            DEC ( LNodeNo , LChildNodeNo ) 
          ; LNodeRef := ResultLeafElem . LeChildRef 
          END (* IF *) 
        END (* LOOP *) 
      ELSE 
        ResultLeafElem := EstHs . LeafElemNull 
      END (* IF *) 
    END GetLeafElem 

(* EXPORTED *) 
; PROCEDURE SetChildKindBitTRUE 
    ( EstRef : EstHs . EstRefTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; KindBit : EstHs . EstChildKindTyp 
    ; VAR WasAlreadyTRUE : BOOLEAN 
      (* ^The value there was already TRUE. *) 
    ) 
  RAISES { AssertionFailure } 
(* TODO: Inline DescendThruNonleafLevels and its callback Visit. *) 

  = VAR StBitSet := EstHs . EstChildKindSetTyp { KindBit } 

  ; PROCEDURE STVisit ( VAR NonleafElem : EstHs . NonleafElemTyp ) 

    = BEGIN (* STVisit *) 
        NonleafElem . NleKindSet := NonleafElem . NleKindSet + StBitSet 
      END STVisit 

  ; VAR LLevelInfo : LevelInfoTyp 

  ; BEGIN (* SetChildKindBitTRUE *) 
      LLevelInfo . LiNodeRef := EstRef 
    ; LLevelInfo . LiChildNo := ChildNo 
    ; LLevelInfo . LiHeight := EstRef . EstHeight 
    ; DescendThruNonleafLevels ( LLevelInfo , STVisit ) 
    ; Assert 
        ( LLevelInfo . LiHeight = 1 , AFT . A_SetChildKindBitTRUEHeightNe1 ) 
    ; WITH 
        WLeafElem = LLevelInfo . LiLeafArrayRef ^ [ LLevelInfo . LiElemNo ] 
      DO IF KindBit IN WLeafElem . LeKindSet 
         THEN 
           WasAlreadyTRUE := TRUE 
         ELSE 
           WasAlreadyTRUE := FALSE  
         ; WLeafElem . LeKindSet := WLeafElem . LeKindSet + StBitSet 
         ; EstRef . EstChildKindSet := EstRef . EstChildKindSet + StBitSet 
         END (* IF *) 
      END (* WITH WLeafElem *) 
    END SetChildKindBitTRUE 

(* EXPORTED *) 
; PROCEDURE SetDescendentKindBitTRUE 
    ( EstRef : EstHs . EstRefTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; KindBit : EstHs . EstChildKindTyp 
    ) 
  RAISES { AssertionFailure } 
  (* Also copies KindBit up, if KindBit IN EstChildKindSetCopyUp *) 

  = VAR LParentRef : EstHs . EstRefTyp 
  ; VAR LParentNodeNo : LbeStd . EstNodeNoTyp 
     (* ^Maintained just to aid debugging. *) 
  ; VAR LDescendentNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildNo : LbeStd . EstChildNoTyp 
  ; VAR LChildRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LLeafElem : EstHs . LeafElemTyp 
  ; VAR LWasAlreadyTRUE : BOOLEAN 
  ; VAR LChildIsOptSingletonList : BOOLEAN 

  ; BEGIN 
      Assert 
        ( 0 < NodeNo AND NodeNo < EstNodeCt ( EstRef )  
        , AFT . A_SetDescendentKindBitTRUE_NodeNo_not_in_tree 
        ) 
    ; LParentRef := EstRef 
    ; LParentNodeNo := 0 
    ; LDescendentNodeNo := NodeNo 
    ; LOOP 
        GetEstChildContainingRelNodeNo 
          ( LParentRef 
          , LDescendentNodeNo 
          , (* VAR *) LChildNo 
          , (* VAR *) LChildRelNodeNo 
          , (* VAR *) LLeafElem  
          ) 
      ; IF LChildRelNodeNo = LDescendentNodeNo 
        THEN 
          IF NOT KindBit IN LLeafElem . LeKindSet 
          THEN 
            SetChildKindBitTRUE 
              ( LParentRef 
              , LChildNo 
              , KindBit 
              , (* VAR *) LWasAlreadyTRUE (* Dead. *) 
              ) 
          END (* IF *) 
        ; EXIT 
        ELSIF LChildRelNodeNo + 1 = LDescendentNodeNo 
              AND EstHs . EstChildKindOptSingletonList 
                  IN LLeafElem . LeKindSet 
        THEN (* The child is the element of an optimized singleton list. *) 
          IF NOT KindBit IN LLeafElem . LeKindSet 
          THEN 
            SetChildKindBitTRUE 
              ( LParentRef 
              , LChildNo 
              , KindBit 
              , (* VAR *) LWasAlreadyTRUE (* Dead. *) 
              ) 
          END (* IF *) 
        ; EXIT 
        ELSE 
          IF NOT KindBit IN LLeafElem . LeKindSet 
             AND KindBit IN EstHs . EstChildKindSetCopyUp 
          THEN 
            SetChildKindBitTRUE 
              ( LParentRef 
              , LChildNo 
              , KindBit 
              , (* VAR *) LWasAlreadyTRUE (* Dead. *) 
              ) 
          END (* IF *) 
        ; LParentRef := LLeafElem . LeChildRef 
        ; LChildIsOptSingletonList 
            := EstHs . EstChildKindOptSingletonList 
               IN LLeafElem . LeKindSet
        ; INC ( LParentNodeNo , LChildRelNodeNo ) 
        ; INC ( LParentNodeNo , ORD ( LChildIsOptSingletonList ) ) 
        ; DEC ( LDescendentNodeNo , LChildRelNodeNo ) 
        ; DEC ( LDescendentNodeNo , ORD ( LChildIsOptSingletonList ) ) 
        END (* IF *) 
      END (* LOOP *) 
    END SetDescendentKindBitTRUE 

(* EXPORTED *) 
; PROCEDURE SetChildRef 
    ( EstRef : EstHs . EstRefTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; NewChildRef : LbeStd . EstRootTyp  
    ) 
  RAISES { AssertionFailure } 
  (* Violently change a child ref in place, without any changes to kind sets, 
     width info, etc.  Violates the usual immutability rule.  Use on your
     own recognizance.  
  *) 

(* TODO: Inline DescendThruNonleafLevels and its noop callback Visit. *) 

  = VAR LLevelInfo : LevelInfoTyp 

  ; BEGIN (* SetChildRef *) 
      LLevelInfo . LiNodeRef := EstRef 
    ; LLevelInfo . LiChildNo := ChildNo 
    ; LLevelInfo . LiHeight := EstRef . EstHeight 
    ; DescendThruNonleafLevels ( LLevelInfo , VisitNoop ) 
    ; Assert ( LLevelInfo . LiHeight = 1 , AFT . A_SetChildRef_HeightNe1 ) 
    ; LLevelInfo . LiLeafArrayRef ^ [ LLevelInfo . LiElemNo ] . LeChildRef 
        := NewChildRef 
    END SetChildRef  

; EXCEPTION SfBailout 

(* EXPORTED *) 
; PROCEDURE SetChildKindBitFALSE 
    ( EstRef : EstHs . EstRefTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; KindBit : EstHs . EstChildKindTyp 
    ; VAR WasAlreadyFALSE : BOOLEAN 
      (* ^The value there was already FALSE. *) 
    ) 
  RAISES { AssertionFailure } 

  = VAR SfBitSet := EstHs . EstChildKindSetTyp { KindBit } 

  ; PROCEDURE SfRecurse 
      ( KTreeRef : EstHs . KTreeRefTyp 
      ; RelChildNo : LbeStd . EstChildNoTyp 
      ; VAR DoNegateParentBit : BOOLEAN 
            (* ^KindBit no longer belongs in the set for KTreeRef *) 
      ) 
    RAISES { SfBailout , AssertionFailure } 

    = VAR LLeafArrayRef : EstHs . LeafArrayRefTyp 
    ; VAR LNonleafArrayRef : EstHs . NonleafArrayRefTyp 
    ; VAR LElemSs : EstHs . ElemSsTyp 
    ; VAR LElemSs2 : EstHs . ElemNoTyp 
    ; VAR LElemCt : EstHs . ElemNoTyp 
    ; VAR LChildCt : LbeStd . EstChildNoTyp 
    ; VAR LDoNegateChildBit : BOOLEAN 

    ; BEGIN (* SfRecurse *) 
        TYPECASE KTreeRef 
        OF EstHs . KTreeLeafRefTyp , EstHs . EstLeafRefTyp 
        => LLeafArrayRef := KTreeRef . LeafArrayRef ( ) 
        ; LElemCt := NUMBER ( LLeafArrayRef ^ ) 
        ; LElemSs := LElemCt - 1 - RelChildNo 
        ; WITH WLeafElem = LLeafArrayRef ^ [ LElemSs ] 
          DO IF KindBit IN WLeafElem . LeKindSet 
             THEN 
               WLeafElem . LeKindSet := WLeafElem . LeKindSet - SfBitSet 
             ; LElemSs2 := 0 
             ; LOOP 
                 IF LElemSs2 >= LElemCt 
                 THEN 
                   DoNegateParentBit := TRUE 
                 ; EXIT 
                 ELSIF KindBit IN LLeafArrayRef ^ [ LElemSs2 ] . LeKindSet 
                 THEN 
                   DoNegateParentBit := FALSE 
                 ; EXIT 
                 ELSE 
                   INC ( LElemSs2 ) 
                 END (* IF *) 
               END (* LOOP *) 
             ELSE 
               WasAlreadyFALSE := TRUE 
             ; RAISE SfBailout 
             END (* IF *) 
          END (* WITH WLeafElem *) 
        | EstHs . KTreeNonleafRefTyp , EstHs . EstNonleafRefTyp 
        => LNonleafArrayRef := KTreeRef . NonleafArrayRef ( ) 
        ; LElemCt := NUMBER ( LNonleafArrayRef ^ ) 
        ; LChildCt := LNonleafArrayRef ^ [ LElemCt - 1 ] . NleCumChildCt 
        ; LElemSs 
            := SearchNonleafArrayForChild 
                 ( LNonleafArrayRef ^ , LChildCt , 0 , RelChildNo ) 
        ; WITH WNonleafElem = LNonleafArrayRef ^ [ LElemSs ] 
          DO IF KindBit IN WNonleafElem . NleKindSet 
             THEN 
               SfRecurse 
                 ( LNonleafArrayRef ^ [ LElemSs ] . NleChildRef 
                 , RelChildNo 
                   - ( LChildCt 
                       - LNonleafArrayRef ^ [ LElemSs ] . NleCumChildCt 
                     ) 
                 , LDoNegateChildBit 
                 ) 
             ; IF LDoNegateChildBit 
               THEN 
                 WNonleafElem . NleKindSet 
                   := WNonleafElem . NleKindSet - SfBitSet 
               ; LElemSs2 := 0 
               ; LOOP 
                   IF LElemSs2 >= LElemCt 
                   THEN 
                     DoNegateParentBit := TRUE 
                   ; EXIT 
                   ELSIF KindBit 
                         IN LNonleafArrayRef ^ [ LElemSs2 ] . NleKindSet 
                   THEN 
                     DoNegateParentBit := FALSE 
                   ; EXIT 
                   ELSE 
                     INC ( LElemSs2 ) 
                   END (* IF *) 
                 END (* LOOP *) 
               END (* IF *) 
             ELSE 
               WasAlreadyFALSE := TRUE 
             ; RAISE SfBailout 
             END (* IF *) 
          END (* WITH WNonleafElem *) 
        ELSE 
          CantHappen ( AFT . A_SfRecurse_BadNodeType ) 
        END (* TYPECASE *) 
      END SfRecurse 

  ; BEGIN (* SetChildKindBitFALSE *) 
      VAR LNegateParentBit : BOOLEAN 

    ; BEGIN (* Block *) 
        IF KindBit IN EstRef . EstChildKindSet 
        THEN 
          WasAlreadyFALSE := FALSE 
        ; TRY 
            SfRecurse ( EstRef , ChildNo , LNegateParentBit ) 
          ; IF LNegateParentBit 
            THEN 
              EstRef . EstChildKindSet := EstRef . EstChildKindSet - SfBitSet 
            END (* IF *) 
          EXCEPT SfBailout 
            => RETURN 
          END (* EXCEPT *) 
        ELSE 
          WasAlreadyFALSE := TRUE 
        END (* IF *) 
      END (* Block *) 
    END SetChildKindBitFALSE 

; PROCEDURE SearchForKindSet 
    ( EstRef : EstHs . EstRefTyp 
    ; StartChildNo : LbeStd . EstChildNoTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; SearchRightward : BOOLEAN 
    ; VAR ResultChildNo : LbeStd . EstChildNoTyp 
    ; VAR ResultChildRelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    ) 
  RAISES { AssertionFailure } 

  = VAR SksDone : BOOLEAN 
  ; VAR SksElemNoDec : PortTypes . Int16Typ 

  ; PROCEDURE SksRecursePhase1 
      ( EstOrKTreeRef : EstHs . KTreeRefTyp 
      ; ChildNo : LbeStd . EstChildNoTyp 
      ; Height : EstHs . KTreeHeightTyp 
      )
    RAISES { AssertionFailure }  
    (* Go down through the leaves of the levels of the K-tree, towards
       ChildNo as long as we have a bit match, then sideways when we
       don't.  Return when we either find the leaf element we want
       (reflected by SksDone) or run out of room to search sideways.
       If we find a bit match on an element to the side of the one on
       the path to the starting child, go into Phase 2.  Al1 calls to
       SksRecursePhase1 are on the path to the starting child, down
       and up.  *)

    = VAR LElemNo : EstHs . ElemNoTyp 
    ; VAR LChildCt : LbeStd . EstChildNoTyp 
    ; VAR LChildCtToLeft : LbeStd . EstChildNoTyp 
    ; VAR LLeafArrayRef : EstHs . LeafArrayRefTyp 
    ; VAR LNonleafArrayRef : EstHs . NonleafArrayRefTyp 

    ; BEGIN (* SksRecursePhase1 *) 
        IF Height <= 1 
        THEN (* K-tree leaf element. *) 
          LLeafArrayRef := EstOrKTreeRef . LeafArrayRef ( ) 
        ; Assert 
            ( ChildNo < EstOrKTreeRef . KTreeElemCt 
            , AFT . A_SksRecursePhase1ExcessiveLeafChildNo 
            ) 
        ; LElemNo := EstOrKTreeRef . KTreeElemCt - 1 - ChildNo 
        ; IF ( LLeafArrayRef ^ [ LElemNo ] . LeKindSet * KindSet ) 
             # EstHs . EstChildKindSetEmpty 
          THEN (* StartChildNo is the one we seek.  We are done. *) 
            ResultChildNo := ChildNo 
          ; ResultLeafElem := LLeafArrayRef ^ [ LElemNo ] 
          ; ResultChildRelNodeNo 
              := EstOrKTreeRef . KTreeNodeCt ( ) 
                 - ResultLeafElem . LeCumNodeCt 
          ; SksDone := TRUE 
          ELSE (* Empty intersection, search forward/up. *) 
            LOOP (* Searching forward. *) 
            (* INVARIANT: LLeafArrayRef ^ [ LElemNo ] has empty intersection. *)
              IF SearchRightward AND LElemNo > 0 
                 OR NOT SearchRightward 
                    AND LElemNo < EstOrKTreeRef . KTreeElemCt - 1 
              THEN (* There is a child in the search direction. Move to it. *) 
                DEC ( LElemNo , SksElemNoDec ) 
              ; IF ( LLeafArrayRef ^ [ LElemNo ] . LeKindSet * KindSet ) 
                   # EstHs . EstChildKindSetEmpty 
                THEN (* Found a nonempty intersection. We are done. *) 
                  ResultChildNo := EstOrKTreeRef . KTreeElemCt - 1 - LElemNo 
                ; ResultLeafElem := LLeafArrayRef ^ [ LElemNo ] 
                ; ResultChildRelNodeNo 
                    := EstOrKTreeRef . KTreeNodeCt ( ) 
                       - ResultLeafElem . LeCumNodeCt 
                ; SksDone := TRUE 
                ; EXIT 
             (* ELSE keep searching at this level. *) 
                END (* IF *) 
              ELSE (* We are at the end. Just return with SksDone FALSE. 
                      Caller will attempt to move sideways at a higher level. 
                   *) 
                EXIT 
              END (* IF *) 
            END (* LOOP Searching forward. *) 
          END (* IF *) 
        ELSE (* Height > 1, K-tree nonleaf element.  *) 
          LNonleafArrayRef := EstOrKTreeRef . NonleafArrayRef ( ) 
        ; LChildCt 
            := LNonleafArrayRef ^ [ EstOrKTreeRef . KTreeElemCt - 1 ] 
               . NleCumChildCt 
        ; Assert 
            ( ChildNo < LChildCt 
            , AFT . A_SksRecursePhase1ExcessiveNonleafChildNo 
            ) 
        ; LElemNo 
            := SearchNonleafArrayForChild 
                 ( LNonleafArrayRef ^ , LChildCt , 0 (* HiElemNo *) , ChildNo ) 
        ; IF ( LNonleafArrayRef ^ [ LElemNo ] . NleKindSet * KindSet ) 
             # EstHs . EstChildKindSetEmpty 
          THEN (* Continue down in Phase 1. *) 
            LChildCtToLeft 
              := LChildCt - LNonleafArrayRef ^ [ LElemNo ] . NleCumChildCt 
          ; SksRecursePhase1 
              ( LNonleafArrayRef ^ [ LElemNo ] . NleChildRef 
              , ChildNo - LChildCtToLeft 
              , Height - 1 
              ) 
          ; IF SksDone 
            THEN (* Compute contribution to Results. *) 
              INC ( ResultChildNo , LChildCtToLeft ) 
            ; INC 
                ( ResultChildRelNodeNo 
                , EstOrKTreeRef . KTreeNodeCt ( ) 
                  - LNonleafArrayRef ^ [ LElemNo ] . NleCumNodeCt 
                ) 
            ; RETURN 
         (* ELSE fall thru to search forward/up *) 
            END (* IF *) 
          END (* IF *) 
        ; LOOP (* Searching forward. *) 
            (* INVARIANT: LNonleafArrayRef ^  [ LElemNo ] has 
               empty intersection. *) 
            IF SearchRightward AND LElemNo > 0 
               OR NOT SearchRightward 
                  AND LElemNo < EstOrKTreeRef . KTreeElemCt - 1 
            THEN (* There is a child in the search direction, move to it. *) 
              DEC ( LElemNo , SksElemNoDec ) 
            ; IF ( LNonleafArrayRef ^ [ LElemNo ] . NleKindSet * KindSet ) 
                 # EstHs . EstChildKindSetEmpty 
              THEN (* Found a nonempty intersection. The sought Est child 
                      exists and lies in the K-tree child.   
                      Go into phase 2 and start down. 
                   *) 
                LChildCtToLeft 
                  := LChildCt 
                     - LNonleafArrayRef ^ [ LElemNo ] . NleCumChildCt 
              ; SksRecursePhase2 
                  ( LNonleafArrayRef ^ [ LElemNo ] . NleChildRef , Height - 1 ) 
              (* Compute contribution to Results. *)
              ; INC ( ResultChildNo , LChildCtToLeft ) 
              ; INC 
                  ( ResultChildRelNodeNo 
                  , EstOrKTreeRef . KTreeNodeCt ( ) 
                    - LNonleafArrayRef ^ [ LElemNo ] . NleCumNodeCt 
                  ) 
              ; SksDone := TRUE 
              ; EXIT 
           (* ELSE keep searching at this level. *) 
              END (* IF *) 
            ELSE (* We are at the end in the search direction. Just return with 
                    SksDone FALSE. Caller will attempt to move forward. *) 
              EXIT 
            END (* IF *) 
          END (* LOOP Searching forward. *) 
        END (* IF Height <= 1 *) 
      END SksRecursePhase1 

  ; PROCEDURE SksRecursePhase2 
      ( EstOrKTreeRef : EstHs . KTreeRefTyp 
      ; Height : EstHs . KTreeHeightTyp 
      ) 
    (* Phase 1 has moved sideways off the path to the starting child
       and found a bit match.  From here, phase 2 moves down only and
       will find the desired leaf.  It always starts at one end of the
       K-tree node.  *)

    = VAR LElemNo : EstHs . ElemNoTyp 
    ; VAR LChildCt : LbeStd . EstChildNoTyp 
    ; VAR LChildCtToLeft : LbeStd . EstChildNoTyp 
    ; VAR LLeafArrayRef : EstHs . LeafArrayRefTyp 
    ; VAR LNonleafArrayRef : EstHs . NonleafArrayRefTyp 

    ; BEGIN (* SksRecursePhase2 *) 
        IF Height <= 1 
        THEN (* K-tree leaf element. *) 
          LLeafArrayRef := EstOrKTreeRef . LeafArrayRef ( ) 
        ; IF SearchRightward 
          THEN 
            LElemNo := EstOrKTreeRef . KTreeElemCt - 1 
          ELSE 
            LElemNo := 0 
          END (* IF *) 
        ; WHILE ( LLeafArrayRef ^ [ LElemNo ] . LeKindSet * KindSet ) 
                = EstHs . EstChildKindSetEmpty 
          DO DEC ( LElemNo , SksElemNoDec ) 
          END (* WHILE *) 
        ; ResultChildNo := EstOrKTreeRef . KTreeElemCt - 1 - LElemNo 
        ; ResultLeafElem := LLeafArrayRef ^ [ LElemNo ] 
        ; ResultChildRelNodeNo 
            := EstOrKTreeRef . KTreeNodeCt ( ) - ResultLeafElem . LeCumNodeCt 
        ELSE (* Height > 1 , K-tree nonleaf element. *) 
          LNonleafArrayRef := EstOrKTreeRef . NonleafArrayRef ( ) 
        ; LChildCt 
            := LNonleafArrayRef ^ [ EstOrKTreeRef . KTreeElemCt - 1 ] 
               . NleCumChildCt 
        ; IF SearchRightward 
          THEN 
            LElemNo := EstOrKTreeRef . KTreeElemCt - 1 
          ELSE 
            LElemNo := 0 
          END (* IF *) 
        ; WHILE ( LNonleafArrayRef ^ [ LElemNo ] . NleKindSet * KindSet ) 
                = EstHs . EstChildKindSetEmpty 
          DO DEC ( LElemNo , SksElemNoDec ) 
          END (* WHILE *) 
        ; LChildCtToLeft 
            := LChildCt - LNonleafArrayRef ^ [ LElemNo ] . NleCumChildCt 
        ; SksRecursePhase2 
            ( LNonleafArrayRef ^ [ LElemNo ] . NleChildRef , Height - 1 ) 
        (* Compute contributions to results. *) 
        ; INC ( ResultChildNo , LChildCtToLeft ) 
        ; INC 
            ( ResultChildRelNodeNo 
            , EstOrKTreeRef . KTreeNodeCt ( ) 
              - LNonleafArrayRef ^ [ LElemNo ] . NleCumNodeCt 
            ) 
        END (* IF Height <= 1 *) 
      END SksRecursePhase2 

  ; BEGIN (* SearchForKindSet *) 
      VAR LEstChildCt : LbeStd . EstChildNoTyp 

    ; BEGIN (* Block  SearchForKindSet block *) 
        ResultLeafElem := EstHs . LeafElemNull 
      ; ResultChildNo := 0 
      ; ResultChildRelNodeNo := 1 
      ; IF EstRef # NIL 
        THEN 
          LEstChildCt := EstRef . KTreeChildCt ( ) 
        ; IF StartChildNo >= LEstChildCt 
          THEN (* Start off right end. *) 
            IF SearchRightward 
            THEN  
              ResultChildNo := LEstChildCt 
            ; ResultChildRelNodeNo := EstNodeCt ( EstRef ) 
              (* ^Defensive. *) 
            ; RETURN
            ELSE 
              StartChildNo := LEstChildCt - 1 
            END 
          ELSIF StartChildNo < 0 
          THEN (* Start off left end. *)
            IF SearchRightward 
            THEN
              StartChildNo := 0 
            ELSE 
              ResultChildNo := - 1 
            ; ResultChildRelNodeNo := 0 
              (* ^Defensive. *) 
            ; RETURN
            END 
          END (* IF *) 

        ; SksDone := FALSE 
        ; SksElemNoDec := ( ORD ( SearchRightward ) * 2 ) - 1 
          (* + 1 if SearchRightward, else - 1 We move "forward" by decrementing 
             element numbers by this. *) 
        ; SksRecursePhase1 ( EstRef , StartChildNo , EstRef . EstHeight ) 
        ; IF NOT SksDone
          THEN (* Didn't find what we wanted. *) 
            IF SearchRightward 
            THEN
              ResultChildNo := LEstChildCt 
            ; ResultChildRelNodeNo := EstNodeCt ( EstRef ) 
              (* ^Defensive. *) 
            ELSE 
              ResultChildNo := - 1 
            ; ResultChildRelNodeNo := 0 
              (* ^Defensive. *) 
            END 
          END 
        ; INC ( ResultChildRelNodeNo ) (* Count Est parent root. *) 
        END (* IF EstRef = NIL *) 
      END (* Block  SearchForKindSet block *) 
    END SearchForKindSet 

(* EXPORTED *) 
; PROCEDURE NextInKindSet 
    ( EstRef : EstHs . EstRefTyp 
    ; StartChildNo : LbeStd . EstChildNoTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; VAR ResultChildNo : LbeStd . EstChildNoTyp 
    ; VAR ResultChildRelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    ) 
  RAISES { AssertionFailure } 
(* Beginning with the StartChildNo-th element (inclusive), 
    search for and return the subscript, root node number, 
    and leaf elem of the next element whose kind set intersects 
    KindSet. ResultChildRelNodeNo includes the Est root node itself. 
    If none found, ResultChildNo = EstChildCt ( EstRef ) 
    and other results undefined. 
*) 

  = BEGIN (* NextInKindSet *) 
      SearchForKindSet 
        ( EstRef 
        , StartChildNo 
        , KindSet 
        , SearchRightward := TRUE 
        , (* VAR *) ResultChildNo := ResultChildNo 
        , (* VAR *) ResultChildRelNodeNo := ResultChildRelNodeNo 
        , (* VAR *) ResultLeafElem := ResultLeafElem 
        ) 
    END NextInKindSet 

(* EXPORTED *) 
; PROCEDURE PrevInKindSet 
    ( EstRef : EstHs . EstRefTyp 
    ; StartChildNo : LbeStd . EstChildNoTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ; VAR ResultChildNo : LbeStd . EstChildNoTyp 
    ; VAR ResultChildRelNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR ResultLeafElem : EstHs . LeafElemTyp 
    ) 
  RAISES { AssertionFailure } 
(* PrevInKindSet is like NextInKindSet, but search backwards. *) 

  = BEGIN (* PrevInKindSet *) 
      SearchForKindSet 
        ( EstRef 
        , StartChildNo 
        , KindSet 
        , SearchRightward := FALSE 
        , (* VAR *) ResultChildNo := ResultChildNo 
        , (* VAR *) ResultChildRelNodeNo := ResultChildRelNodeNo 
        , (* VAR *) ResultLeafElem := ResultLeafElem 
        ) 
    END PrevInKindSet 

(* EXPORTED *) 
; PROCEDURE ApproxChildCt ( Root : LbeStd . EstRootTyp ) 
  : ModHs . EstApproxChildCtTyp 
  RAISES { AssertionFailure } 

  = VAR LChildNo : LbeStd . EstChildNoTyp 
  ; VAR LChildCt : LbeStd . EstChildNoTyp 
  ; VAR LRootNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildLeafElem : EstHs . LeafElemTyp 

  ; BEGIN 
      TYPECASE Root 
      OF NULL => RETURN 0 
      | EstHs . EstRefTyp ( TEstRef ) 
      => LChildCt := TEstRef . KTreeChildCt ( ) 
      ; NextInKindSet 
          ( TEstRef 
          , 0 
          , EstHs . EstChildKindSetEstChild 
          , (* VAR *) LChildNo 
          , (* VAR *) LRootNodeNo (* Ignore *) 
          , (* VAR *) LChildLeafElem (* Ignore *) 
          ) 
      ; IF LChildNo >= LChildCt 
        THEN 
          RETURN 0 
        ELSE 
          NextInKindSet 
            ( TEstRef 
            , LChildNo  
            , EstHs . EstChildKindSetEstChild 
            , (* VAR *) LChildNo 
            , (* VAR *) LRootNodeNo (* Ignore *) 
            , (* VAR *) LChildLeafElem (* Ignore *) 
            ) 
        ; IF LChildNo >= LChildCt 
          THEN RETURN 1 
          ELSE RETURN 2 
          END (* IF *) 
        END (* IF *) 
      ELSE
        RETURN 0 
      END (* TYPECASE *) 
    END ApproxChildCt 

(* EXPORTED *) 
; PROCEDURE EvalPredicate 
    ( Lang : LbeStd . LangTyp 
    ; FsNodeRef : LangUtil . FsNodeRefTyp
    ; EstRef : LbeStd . EstRootTyp 
    ; KindSet : EstHs . EstChildKindSetTyp 
    ) 
  : BOOLEAN 
  RAISES { AssertionFailure } 

  = VAR LChildNo : LbeStd . EstChildNoTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildCt : LbeStd . EstChildNoTyp 
  ; VAR LChildRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildLeafElem : EstHs . LeafElemTyp 
  ; VAR LTok : LbeStd . TokTyp 
  ; VAR LSenseIsEmpty : BOOLEAN 
  ; VAR LSenseIsPlural : BOOLEAN 

  ; BEGIN (* EvalPredicate *) 
      CASE FsNodeRef . FsCondPredicate . PredicateKind 
      OF PredicateKindTyp . PredicateKindNull 
      => RETURN TRUE 

      | PredicateKindTyp . PredicateKindFalse 
      => RETURN FALSE  

      | PredicateKindTyp . PredicateKindTrue 
      => RETURN TRUE 

      | PredicateKindTyp . PredicateKindAbsent 
      => TYPECASE EstRef 
        OF NULL => RETURN TRUE 
        | ModHs . EstDummyTokTyp => RETURN FALSE 
        | ModHs . EstDummyTyp => RETURN TRUE 
        ELSE RETURN FALSE 
        END (* TYPECASE *) 

      | PredicateKindTyp . PredicateKindPresent 
      => TYPECASE EstRef 
        OF NULL => RETURN FALSE  
        | ModHs . EstDummyTokTyp => RETURN TRUE 
        | ModHs . EstDummyTyp => RETURN FALSE 
        ELSE RETURN TRUE  
        END (* TYPECASE *) 

      | PredicateKindTyp . PredicateKindEmptyList 
      , PredicateKindTyp . PredicateKindNonemptyList 
      => LSenseIsEmpty 
           := FsNodeRef . FsCondPredicate . PredicateKind 
              = PredicateKindTyp . PredicateKindEmptyList 
      ; IF EstHs . EstChildKindOptSingletonList IN KindSet 
        THEN RETURN NOT LSenseIsEmpty
        ELSE 
          TYPECASE EstRef 
          OF ModHs . EstDummyTempMarkTyp (* Including NIL *)  
          => RETURN LSenseIsEmpty
          | ModHs . EstDummyTokTyp ( TEstDummyTok ) 
          => RETURN LSenseIsEmpty = ( TEstDummyTok . ApproxChildCt <= 0 )  
          | SharedStrings . T ( TString ) 
          => RETURN 
               LSenseIsEmpty 
               = ( LangUtil . TokClass 
                     ( Lang , SharedStrings . Tok ( TString ) ) 
                   IN LbeStd . TokClassSetNTPlaceholder
                 ) 
          | EstHs . EstRefTyp ( TEstRef ) 
          => Assert 
               ( LangUtil . TokClass ( Lang , TEstRef . EstTok ) 
                 IN LbeStd . TokClassSetAsList  
               , AFT . A_EvalPredicateNonEmptyListPredicateOnNonList 
               ) 
          ; NextInKindSet 
              ( TEstRef 
              , 0 
              , EstHs . EstChildKindSetEstChild 
              , LChildNo 
              , LNodeNo 
              , LChildLeafElem 
              ) 
          ; RETURN LSenseIsEmpty = ( LChildNo >= TEstRef . KTreeChildCt ( ) )  
          ELSE 
            CantHappen ( AFT . A_EvalPredicateNonEmptyListPredicateOnNonEst ) 
          ; RETURN FALSE 
          END (* TYPECASE *) 
        END (* IF *) 

      | PredicateKindTyp . PredicateKindPluralList 
      , PredicateKindTyp . PredicateKindNonpluralList 
      => LSenseIsPlural 
           := FsNodeRef . FsCondPredicate . PredicateKind 
              = PredicateKindTyp . PredicateKindPluralList 
      ; IF EstHs . EstChildKindOptSingletonList IN KindSet 
        THEN RETURN NOT LSenseIsPlural 
        ELSE 
          TYPECASE EstRef 
          OF ModHs . EstDummyTempMarkTyp (* Including NIL *)  
          => RETURN NOT LSenseIsPlural 
          | ModHs . EstDummyTokTyp ( TEstDummyTok ) 
          => RETURN LSenseIsPlural = ( TEstDummyTok . ApproxChildCt > 1 )  
          | SharedStrings . T 
          => RETURN NOT LSenseIsPlural  
          | EstHs . EstRefTyp ( TEstRef ) 
          => Assert 
               ( LangUtil . TokClass ( Lang , TEstRef . EstTok ) 
                 IN LbeStd . TokClassSetAsList  
               , AFT . A_EvalPredicate_PluralListPredicateOnNonList 
               ) 
          ; LChildCt := TEstRef . KTreeChildCt ( ) 
          ; NextInKindSet 
              ( TEstRef 
              , 0 
              , EstHs . EstChildKindSetEstChild 
              , LChildNo 
              , LNodeNo 
              , LChildLeafElem 
              ) 
          ; IF LChildNo >= LChildCt THEN RETURN NOT LSenseIsPlural END (* IF *) 
          ; NextInKindSet 
              ( TEstRef 
              , LChildNo 
              , EstHs . EstChildKindSetEstChild 
              , LChildNo 
              , LChildRelNodeNo 
              , LChildLeafElem 
              ) 
          ; RETURN LSenseIsPlural = ( LChildNo < LChildCt ) 
          ELSE 
            CantHappen ( AFT . A_EvalPredicateNonEmptyListPredicateOnNonEst ) 
          ; RETURN FALSE 
          END (* TYPECASE *) 
        END (* IF *) 

      | PredicateKindTyp . PredicateKindInClass 
      => LTok 
           := LangUtil . UnoptimizedSingletonListTok  
                ( FsNodeRef , EstTok ( EstRef ) , KindSet ) 
      ; RETURN 
           LangUtil . IsInClass 
             ( Lang , LTok , FsNodeRef . FsCondPredicate . PredicateClass )
      END (* CASE *) 
    END EvalPredicate

(* EXPORTED *) 
; PROCEDURE IsZeroWidthModText ( ItemRef : LbeStd . EstRootTyp ) : BOOLEAN

  = VAR LHasNlBefore , LHasNlAfter : BOOLEAN

  ; BEGIN
      TYPECASE ItemRef
      OF NULL => RETURN FALSE  
      | ModHs . ModTextTyp ( TModText ) 
      => LHasNlBefore := TModText . ModTextLeftTokToPos = 0 
      ; LHasNlAfter := TModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
      ; IF LHasNlBefore AND LHasNlAfter   
        THEN (* It's a whole-line ModText.  It gets zero width. *)
          RETURN TRUE 
        ELSE RETURN FALSE 
        END (* IF *)
      ELSE RETURN FALSE 
      END (* TYPECASE *)
    END IsZeroWidthModText 

(* EXPORTED *) 
; PROCEDURE EstMiscInfo 
    ( <* UNUSED *> Lang : LbeStd . LangTyp ; ItemRef : LbeStd . EstRootTyp ) 
  : EstHs . EstMiscInfoTyp 
  RAISES { AssertionFailure } 

  = VAR LResult : EstHs . EstMiscInfoTyp 

  ; BEGIN (* EstMiscInfo *) 
      LResult . EmiSyntTokCt := 0 
    (* EmiEdgeKind, EmiTok, EmiLeftTok, EmiRightTok, no default. *) 
    ; LResult . EmiWidthInfo . WiIsNull := FALSE 
    ; LResult . EmiWidthInfo . WiHasAbsFromPos := FALSE 
    ; LResult . EmiWidthInfo . WiHasNlBefore := FALSE 
    ; LResult . EmiWidthInfo . WiHasNlAfter := FALSE 
    ; LResult . EmiWidthInfo . WiHasNlWithin := FALSE 
    ; LResult . EmiWidthInfo . WiWholeLineModsOnly := FALSE 
    (* WiWidth, no default. *) 
    ; LResult . EmiWidthInfo . WiNlTrigger := LbeStd . LimitedCharNoInfinity 
    ; TYPECASE ItemRef 

      OF ModHs . EstDummyTyp (* Including NIL *) 
      => LResult . EmiWidthInfo := EstHs . WidthInfoNull 
      ; LResult . EmiTok := LbeStd . Tok__Null  
      ; LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindEstChild 

      | EstHs . EstRefTyp ( TEstRef ) 
      => LResult . EmiWidthInfo := TEstRef . KTreeWidthInfo 
      ; LResult . EmiSyntTokCt := TEstRef . KTreeSyntTokCt 
      ; LResult . EmiTok := TEstRef . EstTok 
      ; LResult . EmiLeftTok := TEstRef . EstLeftTok 
      ; LResult . EmiRightTok := TEstRef . EstRightTok 
      ; LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindEstChild 
      ; RETURN LResult (* To avoid steps at end. *) 

      | ModHs . ModCmntLeadingFixedTyp ( TModCmnt ) 
      => LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindLeadingMod 
      ; LResult . EmiWidthInfo . WiHasAbsFromPos := TRUE 
      ; LResult . EmiWidthInfo . WiHasNlBefore := TModCmnt . ModCmntNlBefore  
      ; LResult . EmiWidthInfo . WiHasNlAfter := TModCmnt . ModCmntNlAfter
      ; IF TModCmnt . ModCmntNlBefore AND TModCmnt . ModCmntNlAfter  
        THEN (* It's a whole-line item.  It gets zero width. *) 
          LResult . EmiWidthInfo . WiWidth := 0
        ; LResult . EmiWidthInfo . WiWholeLineModsOnly := TRUE 
        ELSE 
          LResult . EmiWidthInfo . WiWidth (* Absolute. *)  
            := WidthSum 
                 ( TModCmnt . ModCmntFromPos 
                 , SharedStrings . Length ( TModCmnt . ModCmntStringRef ) 
                 ) 
        ; LResult . EmiWidthInfo . WiNlTrigger := TModCmnt . ModCmntFromPos 
        END (* (IF *) 
      ; IF TModCmnt . ModCmntNlAfter 
        THEN LResult . EmiTok := LbeStd . Tok__CmntAtEndOfLine 
        ELSE LResult . EmiTok := LbeStd . Tok__Cmnt 
        END (* IF *) 

      | ModHs . ModCmntLeadingRelativeTyp ( TModCmnt )  
      => LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindLeadingMod 
      ; LResult . EmiWidthInfo . WiHasNlBefore := TModCmnt . ModCmntNlBefore 
      ; LResult . EmiWidthInfo . WiHasNlAfter := TModCmnt . ModCmntNlAfter
      ; IF TModCmnt . ModCmntNlBefore AND TModCmnt . ModCmntNlAfter  
        THEN (* It's a whole-line item.  It gets zero width. *)
          LResult . EmiWidthInfo . WiWidth := 0
        ; LResult . EmiWidthInfo . WiWholeLineModsOnly := TRUE 
        ELSE 
          LResult . EmiWidthInfo . WiWidth (* Relative. *) 
            := SharedStrings . Length ( TModCmnt . ModCmntStringRef )  
        END (* (IF *) 
      ; IF TModCmnt . ModCmntNlAfter  
        THEN LResult . EmiTok := LbeStd . Tok__CmntAtEndOfLine 
        ELSE LResult . EmiTok := LbeStd . Tok__Cmnt 
        END (* IF *) 

      | ModHs . ModCmntTrailingFixedTyp ( TModCmnt ) 
      => LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindTrailingMod 
      ; LResult . EmiWidthInfo . WiHasAbsFromPos := TRUE 
      ; LResult . EmiWidthInfo . WiHasNlAfter := TModCmnt . ModCmntNlAfter
      ; LResult . EmiWidthInfo . WiNlTrigger := TModCmnt . ModCmntFromPos 
      ; LResult . EmiWidthInfo . WiWidth (* Absolute. *)  
           := WidthSum 
                ( TModCmnt . ModCmntFromPos 
                , SharedStrings . Length ( TModCmnt . ModCmntStringRef ) 
                ) 
      ; IF TModCmnt . ModCmntNlAfter 
        THEN LResult . EmiTok := LbeStd . Tok__CmntAtEndOfLine 
        ELSE LResult . EmiTok := LbeStd . Tok__Cmnt 
        END (* IF *) 

      | ModHs . ModCmntTrailingRelativeTyp ( TModCmnt ) 
      => LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindTrailingMod 
      ; LResult . EmiWidthInfo . WiHasNlAfter := TModCmnt . ModCmntNlAfter
      ; LResult . EmiWidthInfo . WiWidth (* Relative. *)
          := SharedStrings . Length ( TModCmnt . ModCmntStringRef ) 
      ; IF TModCmnt . ModCmntNlAfter 
        THEN LResult . EmiTok := LbeStd . Tok__CmntAtEndOfLine 
        ELSE LResult . EmiTok := LbeStd . Tok__Cmnt 
        END (* IF *) 

      | ModHs . ModTextTyp ( TModText ) 
      => LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindLeadingMod 
      ; LResult . EmiWidthInfo . WiHasAbsFromPos := TRUE 
      ; LResult . EmiWidthInfo . WiHasNlBefore 
          := TModText . ModTextLeftTokToPos = 0 
      ; LResult . EmiWidthInfo . WiHasNlAfter 
          := TModText . ModTextToPos = LbeStd . LimitedCharNoInfinity 
      ; IF LResult . EmiWidthInfo . WiHasNlBefore
           AND LResult . EmiWidthInfo . WiHasNlAfter   
        THEN (* It's a whole-line item.  It gets zero width. *) 
          LResult . EmiWidthInfo . WiWidth := 0
        ; LResult . EmiWidthInfo . WiWholeLineModsOnly := TRUE 
        ELSE 
          LResult . EmiWidthInfo . WiWidth := TModText . ModTextOrigToPos
        ; LResult . EmiWidthInfo . WiNlTrigger := TModText . ModTextFromPos
        END (* IF *) 
      ; LResult . EmiTok := LbeStd . Tok__ModText 

      | ModHs . ModBlankLineTyp 
      => LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindLeadingMod 
      ; LResult . EmiWidthInfo . WiHasNlBefore := TRUE 
      ; LResult . EmiWidthInfo . WiHasNlAfter := TRUE 
      (* Blank line is always a whole-line item.  It gets zero width. *)
      ; LResult . EmiWidthInfo . WiWidth := 0 
      ; LResult . EmiWidthInfo . WiWholeLineModsOnly := TRUE  
      ; LResult . EmiTok := LbeStd . Tok__BlankLine 

      | ModHs . ModDelTyp 
      => LResult . EmiWidthInfo := EstHs . WidthInfoNull 
      ; LResult . EmiTok := LbeStd . Tok__ForceSep 
      ; LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindModDel 

      | ModHs . ModErrTyp 
      => LResult . EmiWidthInfo := EstHs . WidthInfoNull 
      ; LResult . EmiTok := LbeStd . Tok__ForceSep 
      ; LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindLeadingMod 

      | SharedStrings . T ( TSharedString ) 
      => LResult . EmiTok := SharedStrings . Tok ( TSharedString ) 
      ; LResult . EmiWidthInfo . WiWidth 
          := SharedStrings . Length ( TSharedString ) 
      ; LResult . EmiEdgeKind := EstHs . EdgeKindTyp . EdgeKindEstChild 
      ; LResult . EmiSyntTokCt := 1 

      ELSE 
        CantHappen ( AFT . A_EstMiscInfoBadObject ) 
      END (* TYPECASE *) 

    (* Cases that don't want the following to happen return directly: *) 
    ; LResult . EmiLeftTok := LResult . EmiTok 
    ; LResult . EmiRightTok := LResult . EmiTok 
    ; RETURN LResult 
    END EstMiscInfo 

(* EXPORTED *) 
; PROCEDURE EstEdgeKind  
    ( Lang : LbeStd . LangTyp ; ItemRef : LbeStd . EstRootTyp ) 
  : EstHs . EdgeKindTyp 
  RAISES { AssertionFailure } 

  = VAR LEstMiscInfo  : EstHs . EstMiscInfoTyp 

  ; BEGIN
      LEstMiscInfo := EstMiscInfo ( Lang , ItemRef ) 
    ; RETURN LEstMiscInfo . EmiEdgeKind 
    END EstEdgeKind  

(* EXPORTED *) 
; PROCEDURE EstIthChildEdgeKind  
    ( Lang : LbeStd . LangTyp 
    ; EstRef : EstHs . EstRefTyp 
    ; I : LbeStd . EstChildNoTyp 
    ) 
  : EstHs . EdgeKindTyp 
  RAISES { AssertionFailure } 

  = VAR LEstChildRef : EstHs . EstRefTyp 
  ; VAR LEstMiscInfo  : EstHs . EstMiscInfoTyp 

  ; BEGIN 
      LEstChildRef := IthChildRef ( EstRef , I ) 
    ; LEstMiscInfo := EstMiscInfo ( Lang , LEstChildRef ) 
    ; RETURN LEstMiscInfo . EmiEdgeKind 
    END EstIthChildEdgeKind  

(* EXPORTED *) 
; PROCEDURE LeftTokForEst 
    ( Lang : LbeStd . LangTyp ; ItemRef : LbeStd . EstRootTyp ) 
  : LbeStd . TokTyp 
  RAISES { AssertionFailure } 

  = VAR LEstMiscInfo : EstHs . EstMiscInfoTyp 

  ; BEGIN (* LeftTokForEst *) 
      IF ItemRef = NIL 
      THEN 
        RETURN LbeStd . Tok__Null 
      ELSE 
        LEstMiscInfo := EstMiscInfo ( Lang , ItemRef ) 
      ; RETURN LEstMiscInfo . EmiLeftTok 
      END (* IF *) 
    END LeftTokForEst 

(* EXPORTED *) 
; PROCEDURE RightTokForEst 
    ( Lang : LbeStd . LangTyp ; ItemRef : LbeStd . EstRootTyp ) 
  : LbeStd . TokTyp 
  RAISES { AssertionFailure } 

  = VAR LEstMiscInfo : EstHs . EstMiscInfoTyp 

  ; BEGIN (* RightTokForEst *) 
      IF ItemRef = NIL 
      THEN 
        RETURN LbeStd . Tok__Null 
      ELSE 
        LEstMiscInfo := EstMiscInfo ( Lang , ItemRef ) 
      ; RETURN LEstMiscInfo . EmiRightTok 
      END (* IF *) 
    END RightTokForEst 

; PROCEDURE ComputeWholeLeafSliceEdgeInfoPair 
    ( Lang : LbeStd . LangTyp 
    ; LeafRef : EstHs . KTreeLeafRefTyp 
    ; VAR Result : EstHs . SliceEdgeInfoPairTyp 
    )
  RAISES { AssertionFailure }  

  = VAR LChildCtMinus1 : LbeStd . EstChildNoTyp 
  ; VAR LLeafArrayRef : EstHs . LeafArrayRefTyp 
  ; VAR LRightChildNo : LbeStd . EstChildNoTyp 
  ; VAR LLeftChildNo : LbeStd . EstChildNoTyp 
  ; VAR LEstMiscInfo : EstHs . EstMiscInfoTyp 

  ; BEGIN (* ComputeWholeLeafSliceEdgeInfoPair *) 
      LChildCtMinus1 := LeafRef . KTreeElemCt - 1 
    ; LLeafArrayRef := LeafRef . LeafArrayRef ( ) 
    ; WITH WLeafElem0 = LLeafArrayRef ^ [ 0 ] 
      DO
        TYPECASE WLeafElem0 . LeChildRef 
        OF NULL => Result . SeiRightEdgeInfo . EiFmtNo := EstHs . FmtNoNull 
        | ModHs . ModDelTyp ( TModDel ) 
        => Result . SeiRightEdgeInfo . EiFmtNo := TModDel . ModDelThruFmtNo 
        ELSE Result . SeiRightEdgeInfo . EiFmtNo := EstHs . FmtNoNull 
        END (* TYPECASE *) 
      ; IF Result . SeiRightEdgeInfo . EiFmtNo = EstHs . FmtNoNull  
           AND EstHs . EstChildKindFirstOfGroup IN WLeafElem0 . LeKindSet 
        THEN 
          Result . SeiRightEdgeInfo . EiFmtNo := WLeafElem0 . LeFmtNo 
        END (* IF *) 
      ; LEstMiscInfo := EstMiscInfo ( Lang , WLeafElem0 . LeChildRef ) 
      ; Result . SeiRightEdgeInfo . EiTok := LEstMiscInfo . EmiRightTok 
      ; Result . SeiRightEdgeInfo . EiEdgeKind := LEstMiscInfo . EmiEdgeKind 
      END (* WITH WLeafElem0 *) 
    ; WITH WLeafElemLast = LLeafArrayRef ^ [ LChildCtMinus1 ] 
      DO IF EstHs . EstChildKindFirstOfGroup IN WLeafElemLast . LeKindSet 
         THEN 
           Result . SeiLeftEdgeInfo . EiFmtNo := WLeafElemLast . LeFmtNo 
         ELSE 
           Result . SeiLeftEdgeInfo . EiFmtNo := EstHs . FmtNoNull 
         END (* IF *) 
      ; LEstMiscInfo := EstMiscInfo ( Lang , WLeafElemLast . LeChildRef ) 
      ; Result . SeiLeftEdgeInfo . EiTok := LEstMiscInfo . EmiLeftTok 
      ; Result . SeiLeftEdgeInfo . EiEdgeKind := LEstMiscInfo . EmiEdgeKind 
      END (* WITH WLeafElemLast *) 
    ; IF Result . SeiRightEdgeInfo . EiTok = LbeStd . Tok__Null 
      THEN (* Search leftward for rightmost child with a nonNull Tok. *) 
        LRightChildNo := 1 
      ; LOOP 
          IF LRightChildNo > LChildCtMinus1 
          THEN (* Searched the whole KTree leaf without finding 
                    a nonNull Tok. *) 
            Assert 
              ( Result . SeiLeftEdgeInfo . EiTok = LbeStd . Tok__Null 
              , AFT . A_ComputeWholeLeafSliceEdgeInfoPairMissedLeftEnd 
              ) 
          ; RETURN  
          ELSE (* Look at this element. *) 
            LEstMiscInfo 
              := EstMiscInfo 
                   ( Lang 
                   , LeafRef . LeafArrayRef ( ) ^ [ LRightChildNo ] . LeChildRef 
                   ) 
          ; IF LEstMiscInfo . EmiRightTok # LbeStd . Tok__Null 
            THEN (* Found a nonNull. *) 
              Result . SeiRightEdgeInfo . EiTok := LEstMiscInfo . EmiRightTok 
            ; EXIT 
            ELSE (* Move left. *) 
              INC ( LRightChildNo ) 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      ELSE 
        LRightChildNo := 0 
      END (* IF *) 
    ; IF Result . SeiLeftEdgeInfo . EiTok = LbeStd . Tok__Null 
      THEN (* Now search rightward from left end for leftmost 
              nonNull. *) 
        LLeftChildNo := LChildCtMinus1 - 1 
      ; LOOP (* Looking for non-Null token, which is opaque. *) 
          Assert 
            ( LLeftChildNo >= LRightChildNo 
            , AFT . A_ComputeWholeLeafSliceEdgeInfoPairMissedRightEnd 
            ) 
        ; LEstMiscInfo 
            := EstMiscInfo 
                 ( Lang 
                 , LeafRef . LeafArrayRef ( ) ^ [ LLeftChildNo ] . LeChildRef 
                 ) 
        ; IF LEstMiscInfo . EmiLeftTok # LbeStd . Tok__Null 
          THEN (* Found nonNull. *) 
            Result . SeiLeftEdgeInfo . EiTok := LEstMiscInfo . EmiLeftTok 
          ; EXIT 
          ELSE 
            DEC ( LLeftChildNo ) 
          END (* IF *) 
        END (* LOOP Looking for non-Null token. *) 
      END (* IF *) 
    END ComputeWholeLeafSliceEdgeInfoPair 

(* EXPORTED *) 
; PROCEDURE GetKTreeSliceEdgeInfoPair 
    ( Lang : LbeStd . LangTyp 
    ; KTreeRef : EstHs . KTreeRefTyp 
    ; VAR SliceEdgeInfoPair : EstHs . SliceEdgeInfoPairTyp 
    ) 
  RAISES { AssertionFailure } 

  = BEGIN (* GetKTreeSliceEdgeInfoPair *) 
      TYPECASE KTreeRef 
      OF EstHs . KTreeNonleafRefTyp ( TNonleafRef ) 
      => SliceEdgeInfoPair := TNonleafRef . KTreeNonleafSliceEdgeInfoPair 
      | EstHs . EstNonleafRefTyp ( TNonleafRef ) 
      => SliceEdgeInfoPair := TNonleafRef . EstNonleafSliceEdgeInfoPair 
      | EstHs . KTreeLeafRefTyp , EstHs . EstLeafRefTyp 
      => ComputeWholeLeafSliceEdgeInfoPair 
           ( Lang , KTreeRef , SliceEdgeInfoPair ) 
      ELSE 
        CantHappen ( AFT . A_GetKTreeSliceEdgeInfoPairBadType ) 
      END (* TYPECASE *) 
    END GetKTreeSliceEdgeInfoPair 

(* EXPORTED *) 
; PROCEDURE UnmarkContainsTempMark ( EstRef : LbeStd . EstRootTyp ) 

  = PROCEDURE UmChildren 
      ( EstOrKTreeRef : EstHs . KTreeRefTyp 
      ; Height : EstHs . KTreeHeightTyp 
      ) 

    = VAR LLeafArrayRef : EstHs . LeafArrayRefTyp 
    ; VAR LNonleafArrayRef : EstHs . NonleafArrayRefTyp 

    ; BEGIN (* UmChildren *) 
        IF Height = 1 
        THEN 
          LLeafArrayRef := EstOrKTreeRef . LeafArrayRef ( ) 
        ; FOR RElemNo := EstOrKTreeRef . KTreeElemCt - 1 TO 0 BY - 1 
          DO 
            WITH WElem = LLeafArrayRef ^ [ RElemNo ]  
            DO 
              IF EstHs . EstChildKindContainsTempMark IN WElem . LeKindSet 
              THEN  
                WElem . LeKindSet 
                  := WElem . LeKindSet 
                     - EstHs . EstChildKindSetContainsTempMark 
              ; UnmarkContainsTempMark ( WElem . LeChildRef ) 
              END (* IF *) 
            END (* WITH *) 
          END (* FOR *) 
        ELSE 
          LNonleafArrayRef := EstOrKTreeRef . NonleafArrayRef ( ) 
        ; FOR RElemNo := EstOrKTreeRef . KTreeElemCt - 1 TO 0 BY - 1 
          DO 
            WITH WElem = LNonleafArrayRef ^ [ RElemNo ]  
            DO 
              IF EstHs . EstChildKindContainsTempMark IN WElem . NleKindSet 
              THEN  
                WElem . NleKindSet 
                  := WElem . NleKindSet 
                     - EstHs . EstChildKindSetContainsTempMark 
              ; UmChildren ( WElem . NleChildRef , Height - 1  ) 
              END (* IF *) 
            END (* WITH *) 
          END (* FOR *) 
        END (* IF *) 
      END UmChildren 

  ; BEGIN (* UnmarkContainsTempMark *)
      TYPECASE EstRef 
      OF ModHs . EstDummyTyp (* Including NIL *) 
      => 

      | EstHs . EstRefTyp ( TEstRef ) 
      => IF EstHs . EstChildKindContainsTempMark IN TEstRef . EstChildKindSet
         THEN 
           TEstRef . EstChildKindSet 
             := TEstRef . EstChildKindSet 
                  - EstHs . EstChildKindSetContainsTempMark 
         ; UmChildren ( EstRef , Height := TEstRef . EstHeight ) 
         END (* IF *) 

      ELSE 
      END (* TYPECASE *) 
    END  UnmarkContainsTempMark 

(* EXPORTED *) 
; PROCEDURE Statistics 
    ( Parent : EstHs . KTreeRefTyp ; VAR Result : StatisticsTyp ) 
  RAISES { AssertionFailure } 

  = PROCEDURE Init ( VAR Stats : StatisticsTyp ) 

    = BEGIN (* Init *) 
        Stats . EstLeafCt := 0 
      ; Stats . EstNonleafCt := 0 
      ; Stats . KTreeLeafCt := 0 
      ; Stats . KTreeNonleafCt := 0 
      ; Stats . LeafArrayCt := 0 
      ; Stats . LeafArrayElemCt := 0 
      ; Stats . LeafArraySize := 0 
      ; Stats . NonleafArrayCt := 0 
      ; Stats . NonleafArrayElemCt := 0 
      ; Stats . NonleafArraySize := 0 
      ; Stats . ModBlankLineCt := 0 
      ; Stats . ModLexErrCt := 0 
      ; Stats . ModSyntErrCt := 0 
      ; Stats . ModDelCt := 0 
      ; Stats . ModCmntTrailingFixedCt := 0 
      ; Stats . ModCmntTrailingRelativeCt := 0 
      ; Stats . ModCmntLeadingFixedCt := 0 
      ; Stats . ModCmntLeadingRelativeCt := 0 
      ; Stats . ModTextCt := 0 
      ; Stats . AstCt := 0 
      ; Stats . DummyTokCt := 0 
      ; Stats . DummyTempMarkCt := 0 
      ; Stats . NilCt := 0 
      END Init 

  ; PROCEDURE TraverseLeaf ( LeafRef : EstHs . LeafArrayRefTyp ) 
    RAISES { AssertionFailure } 

    = BEGIN (* TraverseLeaf *) 
        IF LeafRef # NIL 
        THEN 
          INC ( Result . LeafArrayCt ) 
        ; INC ( Result . LeafArrayElemCt , NUMBER ( LeafRef ^ ) ) 
        ; INC ( Result . LeafArraySize , BYTESIZE ( LeafRef ^ ) ) 
        ; FOR I := NUMBER ( LeafRef ^ ) - 1 TO 0 BY - 1 
          DO Recurse ( LeafRef ^ [ I ] . LeChildRef ) 
          END (* FOR *) 
        END (* IF *) 
      END TraverseLeaf 

  ; PROCEDURE TraverseNonleaf ( NonleafRef : EstHs . NonleafArrayRefTyp ) 
    RAISES { AssertionFailure } 

    = BEGIN (* TraverseNonleaf *) 
        IF NonleafRef # NIL 
        THEN 
          INC ( Result . NonleafArrayCt ) 
        ; INC ( Result . NonleafArrayElemCt , NUMBER ( NonleafRef ^ ) ) 
        ; INC ( Result . NonleafArraySize , BYTESIZE ( NonleafRef ^ ) ) 
        ; FOR I := NUMBER ( NonleafRef ^ ) - 1 TO 0 BY - 1 
          DO Recurse ( NonleafRef ^ [ I ] . NleChildRef ) 
          END (* FOR *) 
        END (* IF *) 
      END TraverseNonleaf 

  ; PROCEDURE Recurse ( Parent : LbeStd . EstRootTyp ) 
    RAISES { AssertionFailure } 

    = BEGIN (* Recurse *) 
        TYPECASE Parent 
        OF NULL 
        => INC ( Result . NilCt ) 
        | ModHs . EstDummyTokTyp 
        => INC ( Result . DummyTokCt )  
        | ModHs . EstDummyTempMarkTyp 
        => INC ( Result . DummyTempMarkCt )  
        | EstHs . EstLeafRefTyp ( TKTreeRef ) 
        => INC ( Result . EstLeafCt ) 
        ; TraverseLeaf ( TKTreeRef . LeafArrayRef ( ) ) 
        | EstHs . EstNonleafRefTyp ( TKTreeRef ) 
        => INC ( Result . EstNonleafCt ) 
        ; TraverseNonleaf ( TKTreeRef . NonleafArrayRef ( ) ) 
        | EstHs . KTreeLeafRefTyp ( TKTreeRef ) 
        => INC ( Result . KTreeLeafCt ) 
        ; TraverseLeaf ( TKTreeRef . LeafArrayRef ( ) ) 
        | EstHs . KTreeNonleafRefTyp ( TKTreeRef ) 
        => INC ( Result . KTreeNonleafCt ) 
        ; TraverseNonleaf ( TKTreeRef . NonleafArrayRef ( ) ) 
        | ModHs . ModBlankLineTyp 
        => INC ( Result . ModBlankLineCt ) 
        | ModHs . ModLexErrTyp 
        => INC ( Result . ModLexErrCt ) 
        | ModHs . ModSyntErrTyp 
        => INC ( Result . ModSyntErrCt ) 
        | ModHs . ModDelTyp 
        => INC ( Result . ModDelCt ) 
        | ModHs . ModCmntTrailingFixedTyp 
        => INC ( Result . ModCmntTrailingFixedCt ) 
        | ModHs . ModCmntTrailingRelativeTyp 
        => INC ( Result . ModCmntTrailingRelativeCt ) 
        | ModHs . ModCmntLeadingFixedTyp 
        => INC ( Result . ModCmntLeadingFixedCt ) 
        | ModHs . ModCmntLeadingRelativeTyp 
        => INC ( Result . ModCmntLeadingRelativeCt ) 
        | ModHs . ModTextTyp 
        => INC ( Result . ModTextCt ) 
        | SharedStrings . T 
        => INC ( Result . AstCt ) 
        ELSE 
          CantHappen ( AFT . A_EstUtilDotStatisticBadObjType ) 
        END (* TYPECASE *) 
      END Recurse 

  ; BEGIN (* Statistics *) 
      Init ( Result ) 
    ; Recurse ( Parent ) 
    END Statistics 

; BEGIN (* EstUtil *) 
    IF FIRST ( INTEGER ) DIV 3 > FIRST ( LbeStd . LimitedCharNoSignedTyp ) 
       OR LAST ( INTEGER ) DIV 3 < LAST ( LbeStd . LimitedCharNoSignedTyp )  
       OR LAST ( INTEGER ) DIV 3 < LAST ( LbeStd . LimitedCharNoTyp )  
    THEN (* Force RT error. *) 
      EVAL NARROW ( NIL , REF INTEGER ) ^ 
    END (* IF *) 
  END EstUtil 
. 
