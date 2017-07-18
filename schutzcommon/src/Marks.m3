
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Marks 

(* Marks that locate points within the text represented by an Est. *) 

; IMPORT Integer 
; IMPORT Boolean  

; IMPORT EstHs 
; IMPORT LbeStd 
; IMPORT Misc 

(* VISIBLE: *) 
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

(* VISIBLE: *) 
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

(* VISIBLE: *) 
; PROCEDURE MarkImage ( Mark : TokMarkTyp ) : TEXT 

  = BEGIN 
      RETURN
        LbeStd . EstNodeNoImage ( Mark . EstNodeNo ) 
        & "(" & LbeStd . EstNodeNoImage ( Mark . EstNodeCt ) & ")" 
        & MarkKindImageShort ( Mark . Kind ) 
        & EstHs . FmtNoImage ( Mark . FmtNo ) 
        & Misc . BooleanImageShort ( Mark . StartAtEnd ) 
        & Misc . BooleanImageShort ( Mark . IsImpliedNewLine )
        & "(" & LbeStd . NumIdTokImage ( Mark . Tok ) & ")"  
        & LbeStd . CharNoImage ( Mark . BlCharPos ) 
    END MarkImage 

(* VISIBLE: *) 
; PROCEDURE Equal ( Left , Right : TokMarkTyp ) : BOOLEAN 
  (* Returns FALSE if unordered. *) 

  = BEGIN (* Equal *) 
      IF Left . EstNodeNo # Right . EstNodeNo 
         OR Left . EstNodeCt # Right . EstNodeCt  
            (* Don't check this.  If things are properly formed, it is
               redundant, and if not, because it didn't get patched right
               after changes, we want compares to be equal anyway. 
            *) 
         OR Left . Kind # Right . Kind 
      THEN 
        RETURN FALSE 
      ELSE 
        CASE Left . Kind 
        OF MarkKindTyp . Null 
        , MarkKindTyp . Changed 
        => RETURN TRUE 

        | MarkKindTyp . Plain 
        , MarkKindTyp . BlankLine 
        => RETURN Left . StartAtEnd = Right . StartAtEnd 

        | MarkKindTyp . LeftSibFmtNo 
        , MarkKindTyp . RightSibFmtNo 
        , MarkKindTyp . ChildFmtNo 
        => RETURN Left . FmtNo = Right . FmtNo 
        END (* CASE *) 
      END (* IF *) 
    END Equal 

(* VISIBLE: *) 
; PROCEDURE Compare ( Left , Right : TokMarkTyp ) : [ - 1 .. 1 ] 
  RAISES { Unordered } 

  = VAR LResult : [ - 1 .. 1 ] 

  ; BEGIN 
      CASE Left . Kind 
      OF MarkKindTyp . Null 
      , MarkKindTyp . Changed   
      => RAISE Unordered 

      | MarkKindTyp . LeftSibFmtNo  
      => CASE Right . Kind 
         OF MarkKindTyp . Null 
         , MarkKindTyp . Changed   
         => RAISE Unordered 

         | MarkKindTyp . LeftSibFmtNo  
         => (* LeftSib, LeftSib *)  
           LResult 
             := Integer . Compare ( Left . EstNodeNo , Right . EstNodeNo ) 
         ; IF LResult = 0 
           THEN RETURN Integer . Compare ( Left . FmtNo , Right . FmtNo )  
           ELSE RETURN LResult 
           END (* IF *) 

         | MarkKindTyp . Plain 
         , MarkKindTyp . BlankLine 
         , MarkKindTyp . ChildFmtNo 
         => (* LeftSib, Center. *)  
           LResult 
             := Integer . Compare ( Left . EstNodeNo , Right . EstNodeNo ) 
         ; IF LResult = 0 
           THEN RETURN - 1 
           ELSE RETURN LResult 
           END (* IF *) 

         | MarkKindTyp . RightSibFmtNo 
         => (* LeftSib, RightSib *)  
           LResult 
             := Integer . Compare 
                  ( Left . EstNodeNo 
                  , Right . EstNodeNo + Right . EstNodeCt - 1 
                  ) 
         ; IF LResult = 0 
           THEN RETURN - 1 
           ELSE RETURN LResult 
           END (* IF *) 
         END (* CASE *) 

      | MarkKindTyp . Plain 
      , MarkKindTyp . BlankLine 
      , MarkKindTyp . ChildFmtNo  
      => CASE Right . Kind 
         OF MarkKindTyp . Null 
         , MarkKindTyp . Changed   
         => RAISE Unordered 

         | MarkKindTyp . LeftSibFmtNo  
         => (* Center, LeftSib *)  
           LResult 
             := Integer . Compare ( Left . EstNodeNo , Right . EstNodeNo ) 
         ; IF LResult = 0 
           THEN RETURN 1 
           ELSE RETURN LResult 
           END (* IF *) 

         | MarkKindTyp . Plain
         , MarkKindTyp . BlankLine 
         , MarkKindTyp . ChildFmtNo 
         => (* Center, Center *) 
           LResult 
             := Integer . Compare ( Left . EstNodeNo , Right . EstNodeNo ) 
         ; IF LResult = 0 
           THEN (* I could avoid retesting Kind fields, but it would be a
                   cartesian explosion of code.
                *) 
             IF Left . Kind # Right . Kind 
             THEN RAISE Unordered 
             ELSIF Left . Kind = MarkKindTyp . ChildFmtNo 
             THEN RETURN Integer . Compare ( Left . FmtNo , Right . FmtNo )  
             ELSE 
               RETURN 
                 Boolean . Compare ( Left . StartAtEnd , Right . StartAtEnd )  
             END (* IF *) 
           ELSE RETURN LResult 
           END (* IF *) 

         | MarkKindTyp . RightSibFmtNo 
         => (* Center, RightSib *)  
           LResult 
             := Integer . Compare 
                  ( Left . EstNodeNo 
                  , Right . EstNodeNo + Right . EstNodeCt - 1 
                  ) 
         ; IF LResult = 0 
           THEN RETURN - 1 
           ELSE RETURN LResult 
           END (* IF *) 
         END (* CASE *) 

      | MarkKindTyp . RightSibFmtNo 
      => CASE Right . Kind 
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
                  ( Left . EstNodeNo + Left . EstNodeCt - 1  
                  , Right . EstNodeNo  
                  ) 
         ; IF LResult = 0 
           THEN RETURN 1 
           ELSE RETURN LResult 
           END (* IF *) 

         | MarkKindTyp . RightSibFmtNo  
         => (* RightSib, RightSib *) 
           LResult 
             := Integer . Compare 
                  ( Left . EstNodeNo + Left . EstNodeCt - 1  
                  , Right . EstNodeNo + Right . EstNodeCt - 1  
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
               := Integer . Compare ( Left . EstNodeNo , Right . EstNodeNo )  
           ; IF LResult = 0 
             THEN (* It's really the same node. *) 
               RETURN Integer . Compare ( Left . FmtNo , Right . FmtNo )  
             ELSE RETURN - LResult 
             END (* IF *) 
           ELSE RETURN LResult 
           END (* IF *) 

         END (* CASE *) 
      END (* CASE *) 
    END Compare 

(* VISIBLE: *) 
; PROCEDURE IsNull ( Mark : TokMarkTyp ) : BOOLEAN 

  = BEGIN (* IsNull *) 
      RETURN Mark . Kind = MarkKindTyp . Null 
    END IsNull 

; BEGIN (* Marks *) 
  END Marks 
. 
