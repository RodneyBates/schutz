
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Marks 

(* Marks that locate points within the text represented by an Est. *) 

; IMPORT EstHs 
; IMPORT LbeStd 

(* Marks represent points within the text represented by an Est. 
   They give an EstNodeNo to locate a node and additional fields 
   to locate points in format syntax tokens. 
 
   For most purposes, a mark is line-relative.  It leads to a 
   new line  and gives a character position within that line. 
   One of these is called a LineMark. 
 
   During reparsing, each LineMark is converted to token-relative form. 
   This leads to a token (anything which has text associated with 
   it) and gives a character position relative to that.  The character 
   position could lie outside the actual token, if it is off the 
   left or right end of the nonblank text on a line.  One of these 
   is called a TempMark. 
 
*) 

; TYPE MarkKindTyp 
    = { Null 
      , Changed (* Used only temporarily, to avoid unnecessary copying. *) 
      , Plain 
        (* A Plain mark leads to an Est leaf node that is an AstString 
           or a mod. *) 
      , BlankLine 
        (* A BlankLine mark works like Plain, except that it uses the 
           BlankLineNo and BlCharPos fields. *) 
      , ChildFmtNo 
        (* A ChildFmtNo mark is used to indicate a point in an InsTok 
           that belongs to the format syntax tree for an Est node that
           has no children.  It leads to the parent Est node and gives
           the format number of the Fs child it refers to. *)
      , LeftSibFmtNo 
      , RightSibFmtNo 
        (* These kinds are used to indicate a point in an InsTok that
           belongs to the format syntax tree for an Est node with one
           or more children.  The EstNodeNo or EstRef leads to the child
           Est node of which the Fs child is a left/right sibling.  Only
           InsToks to the right of all Est children use RightSib.

           *SibFmtNo marks allow whole slices of a list that contains
           Marks within to be sliced and respliced more easily.  This
           depends on the fact that a list slice is denoted to the
           parser by a sublist nonterminal uniquely associated with the
           full list nonterminal, and therefore, a list built from a list
           slice will have the same parent Est node kind after resplicing
           as before, and with the same format syntax tree. *)
      } 

; PROCEDURE MarkKindImage ( Value : MarkKindTyp ) : TEXT 

; PROCEDURE MarkKindImageShort ( Value : MarkKindTyp ) : TEXT 

; TYPE MarkKindSetTyp = SET OF MarkKindTyp 

; CONST MarkKindSetEmpty = MarkKindSetTyp { } 
; CONST MarkKindSetEstLeaf 
    = MarkKindSetTyp { MarkKindTyp . Plain , MarkKindTyp . BlankLine } 
; CONST MarkKindSetSibFmtNo 
    = MarkKindSetTyp 
        { MarkKindTyp . RightSibFmtNo , MarkKindTyp . LeftSibFmtNo } 
; CONST MarkKindSetFmtNo 
    = MarkKindSetTyp 
        { MarkKindTyp . ChildFmtNo 
        , MarkKindTyp . RightSibFmtNo 
        , MarkKindTyp . LeftSibFmtNo 
        } 

(* TokMarks are used in three places:  
   1. In LineMarks, (PaintHs . LineMarkMeatTyp,) which give locations 
      of cursors, ends of selections, bookmarks, etc. 
   2. In TempMarks.  (ParseHs.TempMarkTyp.)  LineMarks are converted 
      temporarily to TempMarks during reparsing and accepting syntactic 
      repairs, then converted back.
   3. In LinesRefs, (PaintHs . LinesRefMeatTyp) which hold a displayed 
      line of text.  These can be very numerous. 
   The set of fields used in the three do not fit any hierarchy.
   Tok is maintained, but not currently used. 
   Kind and FmtNo are used in all three.  
   EstNodeNo, EstCount, StartAtEnd and IsImpliedNewLine are used in 
   LineMarks and LinesRefs.
   CharPos and LineNo are used in TempMarks and LinesRefs and are duplicated
   fields, copied as necessary.
   EstRef is used only in TempMarks.  
   This breakdown of fields in records seems not a lot worse than any other.
*) 

; TYPE TokMarkTyp 
  (* A TokMark denotes a token within the expansion of an Est. 
     "token" includes insertion tokens, new lines, blankline mods, 
     text mods, and comment mods.  It also includes interior 
     nonterminals in the Est. *) 
    = RECORD 
        EstNodeNo : LbeStd . EstNodeNoTyp := LbeStd . EstNodeNoNull 
      ; EstNodeCt : LbeStd . EstNodeNoTyp := 0 
      ; BlCharPos : LbeStd . CharNoTyp := LbeStd . CharNoUnknown 
        (* Maintained only when Kind = BlankLine.  The CharPos at the end of
           the line before the blank lines. *)  
      ; Tok : LbeStd . TokTyp := LbeStd . Tok__Null  
        (* For ChildFmtNo, the token in node number EstNodeNo.
           For LeftSibFmtNo and RightSibFmtNo, the token in the
           parent of node number EstNodeNo, i.e., the list node. 
           For BlankLine, the rightmost token on the nonblank line
           before the blank lines.  
           For Plain, I also put Tok__ModText , Tok__Cmnt, and 
           Tok__CmntAtEndOfLine in here, as appropriate, just for consistency, 
           but as of this writing, these are not used. *) 
      ; FmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull 
          (* ^Meaningful IFF 
              Kind 
              IN MarkKindTyp { LeftSibFmtNo , RightSibFmtNo , ChildFmtNo } *) 
      ; Kind : MarkKindTyp := MarkKindTyp . Null 
      ; StartAtEnd : BOOLEAN := FALSE 
        (* Allows denoting the new line at the end of the token. 
           This happens only for blankline, text, and comment mods. Must be
           FALSE in marks to other things (i.e. that can never have a 
           Nl after. *) 
      ; IsImpliedNewLine : BOOLEAN := FALSE  
        (* An implied new line.  Can only happen at the beginning of a
           ModCmntSameLineFixedTyp, to which the mark leads. *) 
      END (* RECORD  TokMarkTyp *) 

; PROCEDURE MarkImage
    ( Mark : TokMarkTyp ; Lang : LbeStd . LangTyp := LbeStd . LangNull )
  : TEXT 

; PROCEDURE Equal ( Left , Right : TokMarkTyp ) : BOOLEAN 
  (* Returns FALSE if unordered. *) 

; EXCEPTION Unordered 

; PROCEDURE Compare ( Left , Right : TokMarkTyp ) : [ - 1 .. 1 ] 
  RAISES { Unordered } 

; PROCEDURE IsNull ( Mark : TokMarkTyp ) : BOOLEAN 

; CONST TokMarkNull 
    = TokMarkTyp 
        { EstNodeNo := LbeStd . EstNodeNoNull 
        , EstNodeCt := 0 
        , BlCharPos := LbeStd . CharNoUnknown 
        , Kind := MarkKindTyp . Null 
        , FmtNo := EstHs . FmtNoNull  
        , StartAtEnd := FALSE 
        , IsImpliedNewLine := FALSE 
        } 

; END Marks 
. 
