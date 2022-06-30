
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE AstView 

(* AstView provides an abstraction layer over an Est that makes it look like
   a pure Ast, in the tree grammar given by the abstract syntax in the language
   definition.  It hides all modifiers and shows only the Ast skeleton.  
   Although not the work of this package, the Parser will have ensured that
   the Ast so viewed is syntactically correct.
*) 

(* NOTE: On singleton-optimized lists.  This package creates an AstRefTyp
   value representing a list, even if it is singleton-optimized out of the Est.
   Such AstRefTyp value will have IsOptSingletonList = TRUE, but ChildRef will
   point to the single list child.  NodeNo will reflect the convention that
   a singleton-optimized list has a node number, even though it is not present
   in the Est. Clients will have to figure out for themselves what the Ast
   token is for this list.  It depends on both the language and Fs predicates
   surrounding the FsEstChild that the list is a child of.  

   Fetching the child of this AstRefTyp value will return another value
   representing the list element, with IsOptSingletonList = FALSE and other
   fields as for the list. 
*) 

(* WARNING: Currently not thread-safe. *) 

; FROM Failures IMPORT Backout 
; IMPORT LbeStd 
; IMPORT PortTypes 

; TYPE AstRefTyp (* Treat as opaque. *)  
    = RECORD 
        NodeRef : LbeStd . EstRootTyp 
      ; NodeNo : LbeStd . EstNodeNoTyp 
      (* ^Absolute node number of this node in its containing Est *) 
      ; ChildNo : LbeStd . EstChildNoTyp 
      (* The current child of this node, for when this type is used
         as an iterator. *) 
      ; IsOptSingletonList : BOOLEAN := FALSE 
      END (* RECORD *) 

; CONST AstRefNull 
    = AstRefTyp 
        { NodeRef := NIL 
        , NodeNo := LbeStd . EstNodeNoNull 
        , ChildNo := LbeStd . EstChildNoNull 
        , IsOptSingletonList := FALSE 
        } 

; PROCEDURE IsNull ( READONLY AstRef : AstRefTyp ) : BOOLEAN 
  (* AstRef is Null. *) 

; PROCEDURE AstRef 
    ( READONLY RootRef : AstRefTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; IsOptSingletonList : BOOLEAN := FALSE  
    ) 
  : AstRefTyp 
  RAISES { Backout } 
  (* Construct a new node handle having AstRefTyp. *) 

; TYPE AstRefArrayTyp = ARRAY OF AstRefTyp 

; TYPE AstRefArrayRefTyp = REF AstRefArrayTyp 

; PROCEDURE FirstChild 
    ( VAR Parent (* IN OUT *) : AstRefTyp 
    ; VAR ChildRef : AstRefTyp 
    ; VAR WasFound : BOOLEAN 
    ) 
  RAISES { Backout } 
  (* Set ChildRef to denote the leftmost Ast child of Parent and record in 
     Parent that this is its current child.  NOT WasFound if there is no child.
  *) 

; PROCEDURE NextChild 
    ( VAR Parent (* IN OUT *) : AstRefTyp 
    ; VAR ChildRef : AstRefTyp 
    ; VAR WasFound : BOOLEAN 
    ) 
  RAISES { Backout } 
  (* Advance the current Ast child of Parent one to the right, set ChildRef 
     to denote this Ast child, Parent and record in Parent that this is its 
     current child.  NOT WasFound if there is no child to the right.
  *) 

; PROCEDURE GetChildren 
    ( Parent : AstRefTyp 
    ; VAR Children : AstRefArrayTyp 
    ; LdlLang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  RAISES { Backout } 
  (* Fill Children with as many of the children of Parent as will fit,
     padding any extra elements with AstRefNull
  *) 

; PROCEDURE Child 
    ( READONLY Parent : AstRefTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; LdlLang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  : AstRefTyp 
  RAISES { Backout } 
  (* The ChildNo-th child of Parent.  AstRefNull if it doesn't exist. *) 

; PROCEDURE IntValue ( READONLY Node : AstRefTyp ) : PortTypes . Int32Typ 
  (* If Node is a shared string, its Lex.Int value.  Else FIRST() *) 

; PROCEDURE ChildCt 
    ( Parent : AstRefTyp ; Max : CARDINAL := LAST ( CARDINAL ) ) 
  : CARDINAL 
  RAISES { Backout } 
  (* Count of Ast children of Parent, but not more than Max.  This is 
     O(ChildCt), and the Max limitation merely allows a caller to make it
     faster, if it is not interested whether the count exceeds Max.
  *) 

; PROCEDURE FastExtravagantChildCt ( READONLY Parent : AstRefTyp ) : CARDINAL 
  (* O(1), faster than ChildCt, but extravagant by counting mods. *) 

; TYPE VisitAstProcTyp 
  = PROCEDURE ( Node : AstRefTyp ) 
    RAISES ANY 

; PROCEDURE TraverseChildren 
    ( Parent : AstRefTyp ; Visit : VisitAstProcTyp ) 
  RAISES ANY 
  (* Call back Visit for each Ast child of Parent, left-to-right. *) 

; PROCEDURE TraverseTree 
    ( READONLY Parent : AstRefTyp ; Visit : VisitAstProcTyp ) 
  RAISES ANY
  (* Call back Visit for each node of the Ast rooted at Parent, node first,
     then its children, left-to-right. 
  *) 

; END AstView 
. 
