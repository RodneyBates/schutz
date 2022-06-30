
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE AstView 

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

; IMPORT Lex 
; IMPORT TextRd 

; FROM Failures IMPORT Backout 
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT PortTypes 
; IMPORT SharedStrings 
; IMPORT TravUtil 

(* VISIBLE: *) 
; PROCEDURE IsNull ( READONLY AstRef : AstRefTyp ) : BOOLEAN 

  = BEGIN (* IsNull *) 
      RETURN AstRef . NodeRef = NIL 
    END IsNull 
  (* AstRef is Null. *) 

(* VISIBLE: *) 
; PROCEDURE AstRef 
    ( READONLY RootRef : AstRefTyp 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; IsOptSingletonList : BOOLEAN := FALSE  
    ) 
  : AstRefTyp 
  RAISES { Backout } 
  (* Construct a new node handle having AstRefTyp. *) 

  = VAR LResult : AstRefTyp := AstRefNull 
  ; VAR LKindSet : EstHs . EstChildKindSetTyp 
  ; VAR LIsOptSingletonList : BOOLEAN 

  ; BEGIN (* AstRef *) 
      TravUtil . GetDescendantWithNodeNo 
        ( RootRef . NodeRef 
        , NodeNo 
        , (* VAR *) LResult . NodeRef 
        , (* VAR *) LKindSet 
        , (* VAR *) LIsOptSingletonList 
        ) 
(* TODO: Create a temporary actual Est List node when LIsOptSingletonList. *) 
    ; LResult . NodeNo := NodeNo + ORD ( LIsOptSingletonList ) 
    ; LResult . ChildNo := 0 
    ; LResult . IsOptSingletonList := IsOptSingletonList 
    ; RETURN LResult 
    END AstRef 

(* VISIBLE: *) 
; PROCEDURE FirstChild 
    ( VAR Parent (* IN OUT *) : AstRefTyp 
    ; VAR ChildRef : AstRefTyp 
    ; VAR WasFound : BOOLEAN 
    ) 
  RAISES { Backout } 
  (* Set ChildRef to denote the leftmost Ast child of Parent and record in 
     Parent that this is its current child.  NOT WasFound if there is no child.
  *) 

  = BEGIN (* FirstChild *) 
      Parent . ChildNo := 0 
    ; IF Parent . IsOptSingletonList 
      THEN 
        ChildRef . NodeRef := Parent . NodeRef  
      ; ChildRef . NodeNo := Parent . NodeNo + 1 
      ; ChildRef . ChildNo := 0 (* Actually not meaningful. *)    
      ; ChildRef . IsOptSingletonList := FALSE  
(* CHECK: ^Is this always right?  Can we ever have two in a row? *) 
      ; WasFound := TRUE 
      ELSE  
        NextChild 
          ( (* IN OUT *) Parent , (* VAR *) ChildRef , (* VAR *) WasFound ) 
      END (* IF *) 
    END FirstChild 

(* VISIBLE: *) 
; PROCEDURE NextChild 
    ( VAR Parent (* IN OUT *) : AstRefTyp 
    ; VAR ChildRef : AstRefTyp 
    ; VAR WasFound : BOOLEAN 
    ) 
  RAISES { Backout } 
  (* Advance the current Ast child of Parent one to the right, set ChildRef 
     to denote this Ast child, and record in Parent that this is its 
     current child.  NOT WasFound if there is no child to the right.
  *) 

  = VAR LEstChildNo : LbeStd . EstChildNoTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LLeafElem : EstHs . LeafElemTyp 

  ; BEGIN (* NextChild *) 
      IF Parent . IsOptSingletonList 
      THEN (* There is exactly one child, and we are moving beyond it. *) 
        Parent . ChildNo := 1  
      ; ChildRef := AstRefNull 
      ; WasFound := FALSE 
      ELSE  
        TYPECASE Parent . NodeRef 
        OF NULL 
        => ChildRef := AstRefNull 
        ; WasFound := FALSE 

        | EstHs . EstRefTyp 
        => EstUtil . NextInKindSet 
             ( Parent . NodeRef 
             , StartChildNo := Parent . ChildNo 
             , KindSet := EstHs . EstChildKindSetEstChild 
             , (* VAR *) ResultChildNo := LEstChildNo 
             , (* VAR *) ResultChildRelNodeNo := LNodeNo 
             , (* VAR *) ResultLeafElem := LLeafElem 
             ) 
        ; IF LEstChildNo < EstUtil . EstChildCt ( Parent . NodeRef ) 
          THEN 
            ChildRef . NodeRef := LLeafElem . LeChildRef 
          ; ChildRef . NodeNo := Parent . NodeNo + LNodeNo 
          ; ChildRef . ChildNo := 0 
          ; ChildRef . IsOptSingletonList 
              := EstHs . EstChildKindOptSingletonList IN LLeafElem . LeKindSet 
          ; Parent . ChildNo := LEstChildNo + 1 (* to start next search *) 
          ; WasFound := TRUE 
          ELSE 
            ChildRef := AstRefNull 
          ; WasFound := FALSE 
          END (* IF *) 

        ELSE 
          ChildRef := AstRefNull 
        ; WasFound := FALSE 
        END (* TYPECASE *) 
      END (* IF *) 
    END NextChild 

; PROCEDURE FirstAstLeafElem  
    ( VAR Parent (* IN OUT *) : AstRefTyp 
    ; VAR LeafElem : EstHs . LeafElemTyp  
    ; VAR NodeNo : LbeStd . EstNodeNoTyp 
    ; VAR WasFound : BOOLEAN 
    ) 
  RAISES { Backout } 
  (* Set the output formals for the leftmost child of Parent.  
     Record this as the current child in Parent. 
     WasFound = it exists. 
  *) 

  = BEGIN (* FirstAstLeafElem *) 
      Parent . ChildNo := 0 
    ; IF Parent . IsOptSingletonList 
      THEN 
        LeafElem . LeChildRef := Parent . NodeRef 
      ; LeafElem . LeKindSet 
          := EstUtil . EstChildKindSet ( LeafElem . LeChildRef )  
      ; LeafElem . LeFmtNo := EstHs . FmtNoListEstChild  
      ; NodeNo := Parent . NodeNo + 1 
      ; WasFound := TRUE 
      ELSE 
        NextAstLeafElem 
          ( (* IN OUT *) Parent 
          , (* VAR *) LeafElem 
          , (* VAR *) NodeNo 
          , (* VAR *) WasFound 
          ) 
      END (* IF *) 
    END FirstAstLeafElem 

; PROCEDURE NextAstLeafElem 
    ( VAR Parent (* IN OUT *) : AstRefTyp 
    ; VAR LeafElem : EstHs . LeafElemTyp  
    ; VAR NodeNo : LbeStd . EstNodeNoTyp 
    ; VAR WasFound : BOOLEAN 
    ) 
  RAISES { Backout } 
  (* Set the output formals for the next Ast child of Parent, left-to-right,
     after the current child, and advance the current child of Parent to it.  
     WasFound = it exists. 
  *) 

  = VAR LEstChildNo : LbeStd . EstChildNoTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LLeafElem : EstHs . LeafElemTyp  

  ; BEGIN (* NextAstLeafElem *) 
      IF Parent . IsOptSingletonList 
      THEN (* There is exactly one child, and we are moving beyond it. *) 
        Parent . ChildNo := 1  
      ; LeafElem := EstHs . LeafElemNull 
      ; NodeNo 
          := Parent . NodeNo + 1 + EstUtil . EstNodeCt ( Parent . NodeRef ) 
      ; WasFound := FALSE 
      ELSE  
        TYPECASE Parent . NodeRef 
        OF NULL 
        => LeafElem := EstHs . LeafElemNull 
        ; WasFound := FALSE 

        | EstHs . EstRefTyp 
        => EstUtil . NextInKindSet 
             ( Parent . NodeRef 
             , StartChildNo := Parent . ChildNo 
             , KindSet := EstHs . EstChildKindSetEstChild 
             , (* VAR *) ResultChildNo := LEstChildNo 
             , (* VAR *) ResultChildRelNodeNo := LNodeNo 
             , (* VAR *) ResultLeafElem := LeafElem 
             ) 
        ; IF LEstChildNo < EstUtil . EstChildCt ( Parent . NodeRef ) 
          THEN 
            NodeNo := Parent . NodeNo + LNodeNo 
          ; Parent . ChildNo := LEstChildNo + 1 (* to start next search *) 
          ; WasFound := TRUE 
          ; IF LeafElem . LeFmtNo = EstHs . FmtNoNull 
            THEN
              EstUtil . PrevInKindSet 
                ( Parent . NodeRef 
                , StartChildNo := LEstChildNo 
                , KindSet := EstHs . EstChildKindSetFirstOfGroup  
                , (* VAR *) ResultChildNo := LEstChildNo 
                , (* VAR *) ResultChildRelNodeNo := LNodeNo 
                , (* VAR *) ResultLeafElem := LLeafElem 
                ) 
            ; LeafElem . LeFmtNo := LLeafElem . LeFmtNo 
            END (* IF *) 
          ELSE 
            LeafElem := EstHs . LeafElemNull 
          ; WasFound := FALSE 
          END (* IF *) 

        ELSE 
          LeafElem := EstHs . LeafElemNull 
        ; WasFound := FALSE 
        END (* TYPECASE *) 
      END (* IF *) 
    END NextAstLeafElem 

(* VISIBLE: *) 
; PROCEDURE GetChildren 
    ( Parent : AstRefTyp 
    ; VAR Children : AstRefArrayTyp 
    ; LdlLang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  RAISES { Backout } 
  (* Fill Children with as many of the children of Parent as will fit,
     padding any extra elements with AstRefNull
  *) 

  = VAR LSs : PortTypes . Int32Typ 
  ; VAR LLeafElem : EstHs . LeafElemTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LFsRootRef : LangUtil . FsNodeRefTyp 
  ; VAR LEstChildNo : LbeStd . EstChildNoTyp 
  ; VAR LNumberChildren : CARDINAL 
  ; VAR LWasFound : BOOLEAN 

  ; BEGIN (* GetChildren *) 
      LSs := 0 
    ; LNumberChildren := NUMBER ( Children ) 
    ; IF LNumberChildren > 0 
      THEN 
        IF NOT IsNull ( Parent ) 
        THEN 
          FirstAstLeafElem 
            ( (* IN OUT *) Parent 
            , (* VAR *) LLeafElem 
            , (* VAR *) LNodeNo 
            , (* VAR *) LWasFound 
            ) 
          (* ^This will take care of a singleton-optimized list as Parent. *) 
        ; LFsRootRef 
            := EstUtil . FsRuleForEstNode ( LdlLang , Parent . NodeRef )  
        ; IF LFsRootRef = NIL 
          THEN (* This happens during bootstrap.  In this case, there will be
                  NILs for omitted children and no mods, so we can dispense 
                  with FmtNos. 
               *) 
            LOOP 
              IF LWasFound 
              THEN 
                Children [ LSs ] 
                  := AstRefTyp 
                       { NodeRef := LLeafElem . LeChildRef 
                       , NodeNo := LNodeNo 
                       , ChildNo := 0 
                       , IsOptSingletonList 
                           := EstHs . EstChildKindOptSingletonList 
                              IN LLeafElem . LeKindSet 
                       } 
              ; INC ( LSs ) 
              ; IF LSs >= LNumberChildren 
                THEN RETURN 
                ELSE 
                  NextAstLeafElem 
                    ( (* IN OUT *) Parent 
                    , (* VAR *) LLeafElem 
                    , (* VAR *) LNodeNo 
                    , (* VAR *) LWasFound 
                    ) 
                  (* ^Will take care of a singleton-optimized list Parent. *) 
                END (* IF *) 
              ELSE 
                EXIT 
              END (* IF *) 
            END (* LOOP *) 
          ELSE (* We have an Fs tree. *) 
            LOOP 
              IF LWasFound 
              THEN 
                LEstChildNo 
                  := LangUtil . FsLeafRefOfFmtNo 
                       ( LFsRootRef , LLeafElem . LeFmtNo ) 
                     . FsEstChildNo 
                     (* If there are multiple alternative FsCondFmt nodes,
                        this will give the FsEstChild node of the first
                        alternative, but that is OK, because they all have
                        the same FsEstChildNo.
                     *)  
              ; WHILE LSs < LNumberChildren  AND LSs < LEstChildNo 
                DO Children [ LSs ] := AstRefNull 
                ; INC ( LSs ) 
                END (* WHILE *) 
              ; IF LSs >= LNumberChildren 
                THEN RETURN 
                ELSE 
                  Children [ LSs ] 
                    := AstRefTyp 
                         { NodeRef := LLeafElem . LeChildRef 
                         , NodeNo := LNodeNo 
                         , ChildNo := 0 
                         , IsOptSingletonList 
                             := EstHs . EstChildKindOptSingletonList 
                                IN LLeafElem . LeKindSet 
                         } 
                ; INC ( LSs ) 
                ; IF LSs >= LNumberChildren 
                  THEN RETURN 
                  ELSE 
                    NextAstLeafElem 
                      ( (* IN OUT *) Parent 
                      , (* VAR *) LLeafElem 
                      , (* VAR *) LNodeNo 
                      , (* VAR *) LWasFound 
                      ) 
                    (* ^Will take care of a singleton-optimized list Parent. *) 
                  END (* IF *) 
                END (* IF *) 
              ELSE 
                EXIT 
              END (* IF *) 
            END (* LOOP *) 
          END (* IF *) 
        END (* IF *) 
      ; WHILE LSs < LNumberChildren 
        DO Children [ LSs ] := AstRefNull 
        ; INC ( LSs ) 
        END (* WHILE *) 
      END (* IF *) 
    END GetChildren 

; VAR ChildrenRef : AstRefArrayRefTyp := NIL 
(* FIXME: Having a global ChildrenRef like this is not thread-safe. *) 

; CONST InitialChildren = 10 

; PROCEDURE ExpandChildren ( Count : PortTypes . Int32Typ ) 
(* FIXME: Having a global ChildrenRef like this is not thread-safe. *) 

  = VAR LOldCount : PortTypes . Int32Typ 
  ; VAR LNewCount : PortTypes . Int32Typ 
  ; VAR LNewRef : AstRefArrayRefTyp 

  ; BEGIN (* ExpandChildren *) 
      IF ChildrenRef = NIL 
      THEN 
        ChildrenRef 
          := NEW ( AstRefArrayRefTyp , MAX ( Count , InitialChildren ) )
      ELSE 
        LOldCount := NUMBER ( ChildrenRef ^ ) 
      ; IF LOldCount < Count 
        THEN 
          LNewCount := MAX ( Count , 2 * LOldCount ) 
        ; LNewRef := NEW ( AstRefArrayRefTyp , LNewCount ) 
        ; SUBARRAY ( LNewRef ^ , 0 , LOldCount ) := ChildrenRef ^ 
        ; ChildrenRef := LNewRef 
        END (* IF *) 
      END (* IF *) 
    END ExpandChildren 

(* VISIBLE: *) 
; PROCEDURE Child 
    ( READONLY Parent : AstRefTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ; LdlLang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
  : AstRefTyp 
  RAISES { Backout } 
  (* The ChildNo-th child of Parent.  AstRefNull if it doesn't exist. *) 

  = BEGIN (* Child *) 
      IF ChildCt ( Parent ) = 0  
      THEN 
        RETURN AstRefNull 
      ELSE 
        ExpandChildren ( ChildNo + 1 ) 
      ; GetChildren 
          ( Parent 
          , (* VAR *) SUBARRAY ( ChildrenRef ^ , 0 , ChildNo + 1 ) 
          , LdlLang 
          ) 
      ; RETURN ChildrenRef ^ [ ChildNo ] 
      END (* IF *) 
    END Child 

(* VISIBLE: *) 
; PROCEDURE IntValue ( READONLY Node : AstRefTyp ) : PortTypes . Int32Typ 
  (* If Node is a shared string, its Lex.Int value.  Else FIRST() *) 

(* TODO: Fix this for 32/64-bit portability. *) 
(* TODO: It seems like there must be a better place to put this. *) 

  = VAR LResult : PortTypes . Int32Typ 

  ; BEGIN (* IntValue *) 
      TYPECASE Node . NodeRef 
      OF SharedStrings . T ( TString ) 
      => TRY 
           LResult := 
             Lex . Int ( TextRd . New ( SharedStrings . ToText ( TString ) ) ) 
         EXCEPT ELSE 
           RETURN FIRST ( PortTypes . Int32Typ ) 
         END 
      ; RETURN LResult 

      ELSE 
        RETURN FIRST ( PortTypes . Int32Typ ) 
      END (* TYPECASE *) 
    END IntValue 

(* VISIBLE: *) 
; PROCEDURE ChildCt 
    ( Parent : AstRefTyp ; Max : CARDINAL := LAST ( CARDINAL ) ) 
  : CARDINAL 
  RAISES { Backout } 
  (* Count of Ast children of Parent, but not more than Max.  This is 
     O(ChildCt), and the Max limitation merely allows a caller to make it
     faster, if it is not interested whether the count exceeds Max.
  *) 

  = VAR LChildRef : AstRefTyp 
  ; VAR LWasFound : BOOLEAN 
  ; VAR LResult : CARDINAL 

  ; BEGIN (* ChildCt *) 
      IF Parent . IsOptSingletonList
      THEN RETURN 1 
      ELSE 
        TYPECASE Parent . NodeRef 
        OF NULL 
        => RETURN 0 

        | EstHs . EstRefTyp 
        => LResult := 0 
        ; FirstChild ( Parent , LChildRef , LWasFound ) 
        ; LOOP 
            IF LResult >= Max
            THEN EXIT 
            ELSIF LWasFound 
            THEN 
              INC ( LResult ) 
            ; NextChild 
                ( (* IN OUT *) Parent 
                , (* VAR *) LChildRef 
                , (* VAR *) LWasFound 
                ) 
            ELSE 
              EXIT 
            END (* IF *) 
          END (* LOOP *) 
        ; RETURN LResult 

        ELSE RETURN 0  
        END (* TYPECASE *) 
      END (* IF *) 
    END ChildCt  

(* VISIBLE: *) 
; PROCEDURE FastExtravagantChildCt ( READONLY Parent : AstRefTyp ) : CARDINAL 
  (* O(1), faster than ChildCt, but extravagant by counting mods. *) 

  = BEGIN 
      IF Parent . IsOptSingletonList 
      THEN RETURN 1 
      ELSE RETURN EstUtil . EstChildCt ( Parent . NodeRef ) 
      END (* IF *) 
    END FastExtravagantChildCt 

(* VISIBLE: *) 
; PROCEDURE TraverseChildren 
    ( Parent : AstRefTyp ; Visit : VisitAstProcTyp ) 
  RAISES ANY 
  (* Call back Visit for each Ast child of Parent, left-to-right. *) 

  = VAR LChildRef : AstRefTyp 
  ; VAR LWasFound : BOOLEAN 

  ; BEGIN (* TraverseChildren *) 
      IF Parent . IsOptSingletonList
      THEN (* Parent points to the one child. *) 
        LChildRef := Parent 
      ; INC ( LChildRef . NodeNo ) 
      ; LChildRef . IsOptSingletonList := FALSE 
      ; Visit ( LChildRef ) 
      ELSE 
        TYPECASE Parent . NodeRef 
        OF NULL 
        => 

        | EstHs . EstRefTyp 
        => FirstChild ( Parent , LChildRef , LWasFound ) 
        ; LOOP 
            IF LWasFound 
            THEN 
              Visit ( LChildRef ) 
            ; NextChild 
                ( (* IN OUT *) Parent 
                , (* VAR *) LChildRef 
                , (* VAR *) LWasFound 
                ) 
            ELSE 
              EXIT 
            END (* IF *) 
          END (* LOOP *) 

        ELSE 
        END (* TYPECASE *) 
      END (* IF *) 
    END TraverseChildren 

(* VISIBLE: *) 
; PROCEDURE TraverseTree 
    ( READONLY Parent : AstRefTyp ; Visit : VisitAstProcTyp ) 
  RAISES ANY 
  (* Call back Visit for each node of the Ast rooted at Parent, node first,
     then its children, left-to-right. 
  *) 

  = PROCEDURE TTRecurse ( Node : AstRefTyp ) 
    RAISES ANY 

    = BEGIN (* TTRecurse *) 
        Visit ( Node ) 
      ; TraverseChildren ( Node , TTRecurse ) 
      END TTRecurse 

  ; BEGIN (* TraverseTree *) 
      TTRecurse ( Parent ) 
    END TraverseTree 

; BEGIN (* AstView *) 
    ExpandChildren ( InitialChildren ) 
  END AstView 
. 
