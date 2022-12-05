
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Marks 

(* Marks that locate points within the text represented by an Est. *) 

(* Library: *) 
; IMPORT Fmt 
; IMPORT Integer 
; IMPORT Boolean  

; IMPORT EstHs
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT Misc
; FROM Misc IMPORT RefanyPad 

(* EXPORTED: *) 
; PROCEDURE MarkKindImage ( Value : MarkKindTyp ) : TEXT 

  = BEGIN 
      CASE Value 
      OF MarkKindTyp . Null  => RETURN "Null"
      | MarkKindTyp . Changed  => RETURN "Changed"
      | MarkKindTyp . Plain  => RETURN "Plain"
      | MarkKindTyp . BlankLine  => RETURN "BlankLine"
      | MarkKindTyp . ChildFmtNo  => RETURN "ChildFmtNo"
      | MarkKindTyp . LeftSibFmtNo  => RETURN "LeftSibFmtNo"
      | MarkKindTyp . RightSibFmtNo  => RETURN "RightSibFmtNo"
      END (* CASE *) 
    END MarkKindImage 

(* EXPORTED: *) 
; PROCEDURE MarkKindImageShort ( Value : MarkKindTyp ) : TEXT 

  = BEGIN 
      CASE Value 
      OF MarkKindTyp . Null  => RETURN "NL"
      | MarkKindTyp . Changed  => RETURN "CH"
      | MarkKindTyp . Plain  => RETURN "PL"
      | MarkKindTyp . BlankLine  => RETURN "BL"
      | MarkKindTyp . ChildFmtNo  => RETURN "CF"
      | MarkKindTyp . LeftSibFmtNo  => RETURN "LF"
      | MarkKindTyp . RightSibFmtNo  => RETURN "RF"
      END (* CASE *) 
    END MarkKindImageShort 

(* EXPORTED: *) 
; PROCEDURE MarkImage
    ( Mark : TokMarkTyp ; Lang : LbeStd . LangTyp := LbeStd . LangNull )
  : TEXT 

  = VAR LBlCharPosImage : TEXT := ""  
  ; VAR LTokImage : TEXT
  
  ; BEGIN
      IF Lang = LbeStd . LangNull
      THEN LTokImage := LbeStd . NumIdTokImage ( Mark . TkmTok ) 
      ELSE LTokImage := LangUtil . TokImage ( Mark . TkmTok , Lang ) 
      END (* IF *) 
    ; IF Mark . TkmKind = MarkKindTyp . BlankLine
      THEN
        LBlCharPosImage
          := "TkmBlCharPos=" & LbeStd . CharNoImage ( Mark . TkmBlCharPos )
      ELSE LBlCharPosImage := ""
      END (* IF*)
    ; RETURN
        LbeStd . EstNodeNoImage ( Mark . TkmEstNodeNo ) 
        & "(" & LbeStd . EstNodeNoImage ( Mark . TkmEstNodeCt ) & ")"
        & " parent" & LbeStd . EstNodeNoImage ( Mark . TkmParentNodeNo )
        & Fmt . Pad ( Misc . RefanyImage ( Mark . TkmEstRef ) , RefanyPad )
        & ", "
        & MarkKindImageShort ( Mark . TkmKind ) 
        & EstHs . FmtNoImage ( Mark . TkmFmtNo ) 
        & Misc . BooleanImageShort ( Mark . TkmStartAtEnd ) 
        & Misc . BooleanImageShort ( Mark . TkmIsImpliedNewLine )
        & ",TkmTok={" & LTokImage & "} "
        & LBlCharPosImage 
    END MarkImage 

(* EXPORTED: *) 
; PROCEDURE Equal ( Left , Right : TokMarkTyp ) : BOOLEAN 
  (* Returns FALSE if unordered. *) 

  = BEGIN (* Equal *) 
      IF Left . TkmEstNodeNo # Right . TkmEstNodeNo 
         OR Left . TkmEstNodeCt # Right . TkmEstNodeCt  
            (* Don't check this.  If things are properly formed, it is
               redundant, and if not, because it didn't get patched right
               after changes, we want compares to be equal anyway. 
            *) 
         OR Left . TkmKind # Right . TkmKind 
      THEN 
        RETURN FALSE 
      ELSE 
        CASE Left . TkmKind 
        OF MarkKindTyp . Null 
        , MarkKindTyp . Changed 
        => RETURN TRUE 

        | MarkKindTyp . Plain 
        , MarkKindTyp . BlankLine 
        => RETURN Left . TkmStartAtEnd = Right . TkmStartAtEnd 

        | MarkKindTyp . LeftSibFmtNo 
        , MarkKindTyp . RightSibFmtNo 
        , MarkKindTyp . ChildFmtNo 
        => RETURN Left . TkmFmtNo = Right . TkmFmtNo 
        END (* CASE *) 
      END (* IF *) 
    END Equal 

(* EXPORTED: *) 
; PROCEDURE Compare ( Left , Right : TokMarkTyp ) : [ - 1 .. 1 ] 
  RAISES { Unordered } 

  = VAR LResult : [ - 1 .. 1 ] 

  ; BEGIN 
      CASE Left . TkmKind 
      OF MarkKindTyp . Null 
      , MarkKindTyp . Changed   
      => RAISE Unordered 

      | MarkKindTyp . LeftSibFmtNo  
      => CASE Right . TkmKind 
         OF MarkKindTyp . Null 
         , MarkKindTyp . Changed   
         => RAISE Unordered 

         | MarkKindTyp . LeftSibFmtNo  
         => (* LeftSib, LeftSib *)  
           LResult 
             := Integer . Compare ( Left . TkmEstNodeNo , Right . TkmEstNodeNo ) 
         ; IF LResult = 0 
           THEN RETURN Integer . Compare ( Left . TkmFmtNo , Right . TkmFmtNo )  
           ELSE RETURN LResult 
           END (* IF *) 

         | MarkKindTyp . Plain 
         , MarkKindTyp . BlankLine 
         , MarkKindTyp . ChildFmtNo 
         => (* LeftSib, Center. *)  
           LResult 
             := Integer . Compare ( Left . TkmEstNodeNo , Right . TkmEstNodeNo ) 
         ; IF LResult = 0 
           THEN RETURN - 1 
           ELSE RETURN LResult 
           END (* IF *) 

         | MarkKindTyp . RightSibFmtNo 
         => (* LeftSib, RightSib *)  
           LResult 
             := Integer . Compare 
                  ( Left . TkmEstNodeNo 
                  , Right . TkmEstNodeNo + Right . TkmEstNodeCt - 1 
                  ) 
         ; IF LResult = 0 
           THEN RETURN - 1 
           ELSE RETURN LResult 
           END (* IF *) 
         END (* CASE *) 

      | MarkKindTyp . Plain 
      , MarkKindTyp . BlankLine 
      , MarkKindTyp . ChildFmtNo  
      => CASE Right . TkmKind 
         OF MarkKindTyp . Null 
         , MarkKindTyp . Changed   
         => RAISE Unordered 

         | MarkKindTyp . LeftSibFmtNo  
         => (* Center, LeftSib *)  
           LResult 
             := Integer . Compare ( Left . TkmEstNodeNo , Right . TkmEstNodeNo ) 
         ; IF LResult = 0 
           THEN RETURN 1 
           ELSE RETURN LResult 
           END (* IF *) 

         | MarkKindTyp . Plain
         , MarkKindTyp . BlankLine 
         , MarkKindTyp . ChildFmtNo 
         => (* Center, Center *) 
           LResult 
             := Integer . Compare ( Left . TkmEstNodeNo , Right . TkmEstNodeNo ) 
         ; IF LResult = 0 
           THEN (* I could avoid retesting Kind fields, but it would be a
                   cartesian explosion of code.
                *) 
             IF Left . TkmKind # Right . TkmKind 
             THEN RAISE Unordered 
             ELSIF Left . TkmKind = MarkKindTyp . ChildFmtNo 
             THEN
               RETURN Integer . Compare ( Left . TkmFmtNo , Right . TkmFmtNo )
             ELSE 
               RETURN 
                 Boolean . Compare
                   ( Left . TkmStartAtEnd , Right . TkmStartAtEnd )
             END (* IF *) 
           ELSE RETURN LResult 
           END (* IF *) 

         | MarkKindTyp . RightSibFmtNo 
         => (* Center, RightSib *)  
           LResult 
             := Integer . Compare 
                  ( Left . TkmEstNodeNo 
                  , Right . TkmEstNodeNo + Right . TkmEstNodeCt - 1 
                  ) 
         ; IF LResult = 0 
           THEN RETURN - 1 
           ELSE RETURN LResult 
           END (* IF *) 
         END (* CASE *) 

      | MarkKindTyp . RightSibFmtNo 
      => CASE Right . TkmKind 
         OF MarkKindTyp . Null 
         , MarkKindTyp . Changed   
         => RAISE Unordered 
         | MarkKindTyp . LeftSibFmtNo 
         , MarkKindTyp . Plain  
         , MarkKindTyp . BlankLine 
         , MarkKindTyp . ChildFmtNo 
         => (* RightSib, (LeftSib or Center). *)  
           LResult 
             := Integer . Compare 
                  ( Left . TkmEstNodeNo + Left . TkmEstNodeCt - 1  
                  , Right . TkmEstNodeNo  
                  ) 
         ; IF LResult = 0 
           THEN RETURN 1 
           ELSE RETURN LResult 
           END (* IF *) 

         | MarkKindTyp . RightSibFmtNo  
         => (* RightSib, RightSib *) 
           LResult 
             := Integer . Compare 
                  ( Left . TkmEstNodeNo + Left . TkmEstNodeCt - 1  
                  , Right . TkmEstNodeNo + Right . TkmEstNodeCt - 1  
                  ) 
         ; IF LResult = 0 
           THEN (* Because we don't count a node a second time in the node
                   numbering order for its right side, all the nodes up a
                   right side of a subtree have the same value of LResult
                   at this point.  We can disambiguate by using the 
                   original node number as a lower-significance component,
                   with ordering reversed, since higher is larger. 
                   Two LeftSib marks is the only case where this matters.
                *) 
             LResult 
               := Integer . Compare ( Left . TkmEstNodeNo , Right . TkmEstNodeNo )  
           ; IF LResult = 0 
             THEN (* It's really the same node. *) 
               RETURN Integer . Compare ( Left . TkmFmtNo , Right . TkmFmtNo )  
             ELSE RETURN - LResult 
             END (* IF *) 
           ELSE RETURN LResult 
           END (* IF *) 

         END (* CASE *) 
      END (* CASE *) 
    END Compare 

(* EXPORTED: *) 
; PROCEDURE IsNull ( Mark : TokMarkTyp ) : BOOLEAN 

  = BEGIN (* IsNull *) 
      RETURN Mark . TkmKind = MarkKindTyp . Null 
    END IsNull 

; BEGIN (* Marks *) 
  END Marks 
. 
