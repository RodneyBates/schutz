
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE ManualAsTrees 
(* Package ManualAsTrees. 
   Hardcoded Est builder for the initial version of Ldl0.
*)  

; IMPORT Text 

; FROM Assertions IMPORT Assert 
; FROM Failures IMPORT Backout  
; IMPORT EstBuild 
; IMPORT EstHs 
; IMPORT LbeStd 
; IMPORT Ldl0Tok AS LT 
; IMPORT Ldl0ChildTok AS LCT 
; IMPORT Ldl0Semantics 
; IMPORT MessageCodes 
; IMPORT SharedStrings 

; TYPE AFT = MessageCodes . T 

; CONST ManualKindSetNonNIL 
    = EstHs . EstChildKindSetTyp 
        { EstHs . EstChildKindEstChild , EstHs . EstChildKindNonNIL 
        , EstHs . EstChildKindFirstOfGroup 
        } 

; CONST ManualKindSetNIL 
    = EstHs . EstChildKindSetTyp 
        { EstHs . EstChildKindEstChild , EstHs . EstChildKindFirstOfGroup } 

; PROCEDURE KindSet 
    ( Child : LbeStd . EstRootTyp ) : EstHs . EstChildKindSetTyp 

  = BEGIN (* KindSet *) 
      IF Child = NIL 
      THEN 
        RETURN ManualKindSetNIL 
      ELSE 
        RETURN ManualKindSetNonNIL 
      END (* IF *) 
    END KindSet 

; VAR GLang : LbeStd . LangTyp 

; PROCEDURE MkNode 
    ( Tok : LbeStd . TokTyp 
    ; Child0 : LbeStd . EstRootTyp := NIL 
    ; FmtNo0 : EstHs . FmtNoTyp := EstHs . FmtNoNull 
    ; Child1 : LbeStd . EstRootTyp := NIL 
    ; FmtNo1 : EstHs . FmtNoTyp := EstHs . FmtNoNull 
    ; Child2 : LbeStd . EstRootTyp := NIL 
    ; FmtNo2 : EstHs . FmtNoTyp := EstHs . FmtNoNull 
    ; Child3 : LbeStd . EstRootTyp := NIL 
    ; FmtNo3 : EstHs . FmtNoTyp := EstHs . FmtNoNull 
    ; Child4 : LbeStd . EstRootTyp := NIL 
    ; FmtNo4 : EstHs . FmtNoTyp := EstHs . FmtNoNull 
    ; Child5 : LbeStd . EstRootTyp := NIL 
    ; FmtNo5 : EstHs . FmtNoTyp := EstHs . FmtNoNull 
    ; Child6 : LbeStd . EstRootTyp := NIL 
    ; FmtNo6 : EstHs . FmtNoTyp := EstHs . FmtNoNull 
    ; Child7 : LbeStd . EstRootTyp := NIL 
    ; FmtNo7 : EstHs . FmtNoTyp := EstHs . FmtNoNull 
    ; Child8 : LbeStd . EstRootTyp := NIL 
    ; FmtNo8 : EstHs . FmtNoTyp := EstHs . FmtNoNull 
    ) 
  : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = VAR Node_MergeState : EstBuild . MergeStateTyp 

  ; PROCEDURE MergeChild 
      ( Child : LbeStd . EstRootTyp := NIL 
      ; FmtNo : EstHs . FmtNoTyp := EstHs . FmtNoNull 
      ) 
    RAISES { Backout } 

    = BEGIN (* MergeChild *) 
        IF FmtNo = EstHs . FmtNoNull 
        THEN 
          Assert 
            ( Child = NIL , AFT . A_MkNodeMergeChild_NilChildWithNonNullFmtNo ) 
        ELSE 
          EstBuild . MergeChild 
            ( Node_MergeState 
            , Child 
            , KindSet ( Child ) 
            , IsFirstOfGroup := TRUE 
            , GroupFmtNo := FmtNo 
            ) 
        END (* IF *) 
      END MergeChild 

  ; VAR LResult : EstHs . EstRefTyp 

  ; BEGIN (* MkNode *) 
      Node_MergeState 
        := EstBuild . NewMergeState 
             ( Lang := GLang , EstTok := Tok , EstRefToInheritFrom := NIL ) 
    ; MergeChild ( Child8 , FmtNo8 ) 
    ; MergeChild ( Child7 , FmtNo7 ) 
    ; MergeChild ( Child6 , FmtNo6 ) 
    ; MergeChild ( Child5 , FmtNo5 ) 
    ; MergeChild ( Child4 , FmtNo4 ) 
    ; MergeChild ( Child3 , FmtNo3 ) 
    ; MergeChild ( Child2 , FmtNo2 ) 
    ; MergeChild ( Child1 , FmtNo1 ) 
    ; MergeChild ( Child0 , FmtNo0 ) 
    ; EstBuild . FinishMerge 
        ( Node_MergeState 
        , ResultEstNodeKind := EstHs . EstNodeKindTyp . EstNodeKindPlain 
        , ResultTreeRef := LResult 
        ) 
    ; RETURN LResult 
    END MkNode 

; PROCEDURE MkString ( String : TEXT ) : SharedStrings . T 
  RAISES { Backout } 

  = BEGIN (* MkString *) 
      RETURN 
        SharedStrings . FromText ( "\"" & String & "\"" , Tok := LT . String ) 
    END MkString 

; PROCEDURE MkStringOrFormatter ( Param : TEXT ) : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = BEGIN (* MkStringOrFormatter *) 
      IF Text . Equal ( Param , "BANG" ) 
      THEN 
        RETURN MkNode ( LT . LineBreak , NIL , 2 ) 
      ELSIF Text . Equal ( Text . Sub ( Param , 0 , 5 ) , "BANG-" ) 
      THEN 
        RETURN 
          MkNode 
            ( LT . LineBreak 
            , SharedStrings . FromText 
                ( Text . Sub ( Param , 5 ) , Tok := LT . Integer ) 
            , 2 
            ) 
      ELSE 
        RETURN MkString ( Param ) 
      END (* IF *) 
    END MkStringOrFormatter 

; PROCEDURE MkId ( Tok : LbeStd . TokTyp ) : SharedStrings . T 
  RAISES { Backout } 

  = BEGIN (* MkId *) 
      RETURN 
        SharedStrings . FromText 
          ( String := LT . ToText ( Tok ) 
          , Tok := LT . Ident 
          ) 
    END MkId 

; PROCEDURE MkChildId ( Tok : LbeStd . TokTyp ) : SharedStrings . T 
  RAISES { Backout } 
    
  = BEGIN (* MkChildId *) 
      RETURN 
        SharedStrings . FromText 
          ( String := LCT . ToText ( Tok ) 
          , Tok := LT . Ident 
          ) 
    END MkChildId 

; PROCEDURE GrandChildRef ( Param : REFANY ) : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = BEGIN (* GrandChildRef *) 
      TYPECASE Param <* NOWARN *>
      OF NULL 
      => RETURN NIL 
      | TEXT ( TText ) 
      => RETURN MkStringOrFormatter ( TText ) 
      | LbeStd . EstRootTyp ( TEst ) 
      => RETURN TEst 
      END (* TYPECASE *) 
    END GrandChildRef 

; PROCEDURE GrandChildFmtNo ( Param : REFANY ) : EstHs . FmtNoTyp 

  = BEGIN (* GrandChildFmtNo *) 
      IF Param = NIL 
      THEN 
        RETURN EstHs . FmtNoNull 
      ELSE 
        RETURN 0 
      END (* IF *) 
    END GrandChildFmtNo 

; PROCEDURE MkAsTokNameRule 
    ( Lhs : LbeStd . TokTyp ; Rhs : TEXT ) : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = BEGIN (* MkAsTokNameRule *) 
      RETURN 
        MkNode 
          ( LT . AsClassRule 
          , MkId ( Lhs ) 
          , 0 
          , MkNode ( LT . Alternation , MkString ( Rhs ) , 0 ) 
          , 3 
          ) 
    END MkAsTokNameRule 

; PROCEDURE MkCsListRule 
    ( RuleTok : LbeStd . TokTyp 
    ; Lhs : LbeStd . TokTyp 
    ; ListChildTok : LbeStd . TokTyp 
    ; Sep1 : TEXT := NIL 
    ) 
  : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = VAR LSeparators : EstHs . EstRefTyp 

  ; BEGIN (* MkCsListRule *) 
      IF Sep1 = NIL 
      THEN 
        LSeparators := NIL (* MkNode ( LT . CsAtomList ) *)  
      ELSE 
        LSeparators 
          := MkNode 
               ( LT . CsAtomList 
               , GrandChildRef ( Sep1 ) 
               , GrandChildFmtNo ( Sep1 ) 
               ) 
      END (* IF *) 
    ; RETURN 
        MkNode 
          ( RuleTok 
          , MkId ( Lhs ) 
          , 0 
          , MkId ( ListChildTok ) 
          , 4 
          , LSeparators 
          , 7 
          ) 
    END MkCsListRule 

; PROCEDURE MkCsFixedRule 
    ( Lhs : LbeStd . TokTyp 
    ; Child1 : REFANY := NIL 
    ; Child2 : REFANY := NIL 
    ; Child3 : REFANY := NIL 
    ; Child4 : REFANY := NIL 
    ; Child5 : REFANY := NIL 
    ; Child6 : REFANY := NIL 
    ; Child7 : REFANY := NIL 
    ; Child8 : REFANY := NIL 
    ; Child9 : REFANY := NIL 
    ) 
  : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = VAR LRhs : LbeStd . EstRootTyp 

  ; BEGIN (* MkCsFixedRule *) 
      LRhs 
        := MkNode 
             ( LT . Concatenation 
             , GrandChildRef ( Child1 ) 
             , GrandChildFmtNo ( Child1 ) 
             , GrandChildRef ( Child2 ) 
             , GrandChildFmtNo ( Child2 ) 
             , GrandChildRef ( Child3 ) 
             , GrandChildFmtNo ( Child3 ) 
             , GrandChildRef ( Child4 ) 
             , GrandChildFmtNo ( Child4 ) 
             , GrandChildRef ( Child5 ) 
             , GrandChildFmtNo ( Child5 ) 
             , GrandChildRef ( Child6 ) 
             , GrandChildFmtNo ( Child6 ) 
             , GrandChildRef ( Child7 ) 
             , GrandChildFmtNo ( Child7 ) 
             , GrandChildRef ( Child8 ) 
             , GrandChildFmtNo ( Child8 ) 
             , GrandChildRef ( Child9 ) 
             , GrandChildFmtNo ( Child9 ) 
             ) 
    ; RETURN MkNode ( LT . CsFixedRule , MkId ( Lhs ) , 0 , LRhs , 3 ) 
    END MkCsFixedRule 

; PROCEDURE MkCsOptChild 
    ( Child1 : REFANY := NIL ; Child2 : REFANY := NIL ) : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = BEGIN (* MkCsOptChild *) 
      RETURN 
        MkNode 
          ( LT . Concatenation 
          , GrandChildRef ( Child1 ) 
          , GrandChildFmtNo ( Child1 ) 
          , GrandChildRef ( Child2 ) 
          , GrandChildFmtNo ( Child2 ) 
          ) 
    END MkCsOptChild 

; PROCEDURE MkFsListRule 
    ( Lhs : LbeStd . TokTyp 
    ; FmtKind : FmtKindTyp 
    ; ListChild : LbeStd . EstRootTyp 
    ; Sep1 : TEXT := NIL 
    ; Sep2 : TEXT := NIL 
    ) 
  : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = VAR LNodeTok : LbeStd . TokTyp 
  ; VAR LFormatters : EstHs . EstRefTyp 

  ; BEGIN (* MkFsListRule *) 
      CASE FmtKind 
      OF FmtKindTyp . VERT 
      => LNodeTok := LT . FsListVertRule 
      | FmtKindTyp . HORIZ 
      => LNodeTok := LT . FsListHorizRule 
      | FmtKindTyp . FILL 
      => LNodeTok := LT . FsListFillRule 
      END (* CASE *) 
    ; IF Sep1 = NIL AND Sep2 = NIL 
      THEN 
        LFormatters := NIL  
      ELSIF Sep2 = NIL 
      THEN 
        LFormatters 
          := MkNode ( LT . FormatterList , MkStringOrFormatter ( Sep1 ) , 0 ) 
      ELSE 
        LFormatters 
          := MkNode 
               ( LT . FormatterList 
               , MkStringOrFormatter ( Sep1 ) 
               , 0 
               , MkStringOrFormatter ( Sep2 ) 
               , 0 
               ) 
      END (* IF *) 
    ; RETURN 
        MkNode 
          ( LNodeTok 
          , MkNode ( LT . IdentPlusList , MkId ( Lhs ) , 0 ) 
          , 0 
          , ListChild 
          , 6 
          , LFormatters 
          , 8 
          ) 
    END MkFsListRule 

; PROCEDURE MkFsFixedRule 
    ( Lhs : LbeStd . TokTyp 
    ; FmtKind : FmtKindTyp 
    ; Child1 : REFANY := NIL 
    ; Child2 : REFANY := NIL 
    ; Child3 : REFANY := NIL 
    ; Child4 : REFANY := NIL 
    ; Child5 : REFANY := NIL 
    ; Child6 : REFANY := NIL 
    ; Child7 : REFANY := NIL 
    ; Child8 : REFANY := NIL 
    ) 
  : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = VAR LNodeTok : LbeStd . TokTyp 
  ; VAR LChildren : LbeStd . EstRootTyp 
  ; VAR LFmtNo : EstHs . FmtNoTyp 

  ; BEGIN (* MkFsFixedRule *) 
      CASE FmtKind 
      OF FmtKindTyp . VERT 
      => LNodeTok := LT . FsFixedVertRule 
      ; LFmtNo := 4 
      | FmtKindTyp . HORIZ 
      => LNodeTok := LT . FsFixedHorizRule 
      ; LFmtNo := 5 
      | FmtKindTyp . FILL 
      => LNodeTok := LT . FsFixedFillRule 
      ; LFmtNo := 4 
      END (* CASE *) 
    ; LChildren 
        := MkNode 
             ( LT . FsFixedChildList 
             , GrandChildRef ( Child1 ) 
             , GrandChildFmtNo ( Child1 ) 
             , GrandChildRef ( Child2 ) 
             , GrandChildFmtNo ( Child2 ) 
             , GrandChildRef ( Child3 ) 
             , GrandChildFmtNo ( Child3 ) 
             , GrandChildRef ( Child4 ) 
             , GrandChildFmtNo ( Child4 ) 
             , GrandChildRef ( Child5 ) 
             , GrandChildFmtNo ( Child5 ) 
             , GrandChildRef ( Child6 ) 
             , GrandChildFmtNo ( Child6 ) 
             , GrandChildRef ( Child7 ) 
             , GrandChildFmtNo ( Child7 ) 
             , GrandChildRef ( Child8 ) 
             , GrandChildFmtNo ( Child8 ) 
             ) 
    ; RETURN 
        MkNode 
          ( LNodeTok 
          , MkNode ( LT . IdentPlusList , MkId ( Lhs ) , 0 ) 
          , 0 
          , LChildren 
          , LFmtNo 
          ) 
    END MkFsFixedRule 

; PROCEDURE MkFsHorizSubtree 
    ( Child1 : REFANY := NIL 
    ; Child2 : REFANY := NIL 
    ; Child3 : REFANY := NIL 
    ; Child4 : REFANY := NIL 
    ; Child5 : REFANY := NIL 
    ; Child6 : REFANY := NIL 
    ; Child7 : REFANY := NIL 
    ; Child8 : REFANY := NIL 
    ) 
  : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = VAR LChildren : LbeStd . EstRootTyp 

  ; BEGIN (* MkFsHorizSubtree *) 
      LChildren 
        := MkNode 
             ( LT . FsFixedChildList 
             , GrandChildRef ( Child1 ) 
             , GrandChildFmtNo ( Child1 ) 
             , GrandChildRef ( Child2 ) 
             , GrandChildFmtNo ( Child2 ) 
             , GrandChildRef ( Child3 ) 
             , GrandChildFmtNo ( Child3 ) 
             , GrandChildRef ( Child4 ) 
             , GrandChildFmtNo ( Child4 ) 
             , GrandChildRef ( Child5 ) 
             , GrandChildFmtNo ( Child5 ) 
             , GrandChildRef ( Child6 ) 
             , GrandChildFmtNo ( Child6 ) 
             , GrandChildRef ( Child7 ) 
             , GrandChildFmtNo ( Child7 ) 
             , GrandChildRef ( Child8 ) 
             , GrandChildFmtNo ( Child8 ) 
             ) 
    ; RETURN MkNode ( LT . FsHorizSubtree , LChildren , 3 ) 
    END MkFsHorizSubtree 

; PROCEDURE MkFsCondPresent 
    ( Child1 : REFANY := NIL 
    ; Child2 : REFANY := NIL 
    ; Child3 : REFANY := NIL 
    ; Child4 : REFANY := NIL 
    ; Child5 : REFANY := NIL 
    ; Child6 : REFANY := NIL 
    ; Child7 : REFANY := NIL 
    ; Child8 : REFANY := NIL 
    ) 
  : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = VAR LChildren : LbeStd . EstRootTyp 

  ; BEGIN (* MkFsCondPresent *) 
      LChildren 
        := MkNode 
             ( LT . FsFixedChildList 
             , GrandChildRef ( Child1 ) 
             , GrandChildFmtNo ( Child1 ) 
             , GrandChildRef ( Child2 ) 
             , GrandChildFmtNo ( Child2 ) 
             , GrandChildRef ( Child3 ) 
             , GrandChildFmtNo ( Child3 ) 
             , GrandChildRef ( Child4 ) 
             , GrandChildFmtNo ( Child4 ) 
             , GrandChildRef ( Child5 ) 
             , GrandChildFmtNo ( Child5 ) 
             , GrandChildRef ( Child6 ) 
             , GrandChildFmtNo ( Child6 ) 
             , GrandChildRef ( Child7 ) 
             , GrandChildFmtNo ( Child7 ) 
             , GrandChildRef ( Child8 ) 
             , GrandChildFmtNo ( Child8 ) 
             ) 
    ; RETURN MkNode ( LT . FsCondPresent , LChildren , 3 ) 
    END MkFsCondPresent 

; PROCEDURE MkFsCondNonempty 
    ( Child1 : REFANY := NIL 
    ; Child2 : REFANY := NIL 
    ; Child3 : REFANY := NIL 
    ; Child4 : REFANY := NIL 
    ; Child5 : REFANY := NIL 
    ; Child6 : REFANY := NIL 
    ; Child7 : REFANY := NIL 
    ; Child8 : REFANY := NIL 
    ) 
  : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = VAR LChildren : LbeStd . EstRootTyp 

  ; BEGIN (* MkFsCondNonempty *) 
      LChildren 
        := MkNode 
             ( LT . FsFixedChildList 
             , GrandChildRef ( Child1 ) 
             , GrandChildFmtNo ( Child1 ) 
             , GrandChildRef ( Child2 ) 
             , GrandChildFmtNo ( Child2 ) 
             , GrandChildRef ( Child3 ) 
             , GrandChildFmtNo ( Child3 ) 
             , GrandChildRef ( Child4 ) 
             , GrandChildFmtNo ( Child4 ) 
             , GrandChildRef ( Child5 ) 
             , GrandChildFmtNo ( Child5 ) 
             , GrandChildRef ( Child6 ) 
             , GrandChildFmtNo ( Child6 ) 
             , GrandChildRef ( Child7 ) 
             , GrandChildFmtNo ( Child7 ) 
             , GrandChildRef ( Child8 ) 
             , GrandChildFmtNo ( Child8 ) 
             ) 
    ; RETURN MkNode ( LT . FsCondNonempty , LChildren , 3 ) 
    END MkFsCondNonempty 

; PROCEDURE MkFsCondMember 
    ( ClassId : LbeStd . TokTyp 
    ; Child1 : REFANY := NIL 
    ; Child2 : REFANY := NIL 
    ; Child3 : REFANY := NIL 
    ; Child4 : REFANY := NIL 
    ; Child5 : REFANY := NIL 
    ; Child6 : REFANY := NIL 
    ; Child7 : REFANY := NIL 
    ; Child8 : REFANY := NIL 
    ) 
  : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = VAR LChildren : LbeStd . EstRootTyp 

  ; BEGIN (* MkFsCondMember *) 
      LChildren 
        := MkNode 
             ( LT . FsFixedChildList 
             , GrandChildRef ( Child1 ) 
             , GrandChildFmtNo ( Child1 ) 
             , GrandChildRef ( Child2 ) 
             , GrandChildFmtNo ( Child2 ) 
             , GrandChildRef ( Child3 ) 
             , GrandChildFmtNo ( Child3 ) 
             , GrandChildRef ( Child4 ) 
             , GrandChildFmtNo ( Child4 ) 
             , GrandChildRef ( Child5 ) 
             , GrandChildFmtNo ( Child5 ) 
             , GrandChildRef ( Child6 ) 
             , GrandChildFmtNo ( Child6 ) 
             , GrandChildRef ( Child7 ) 
             , GrandChildFmtNo ( Child7 ) 
             , GrandChildRef ( Child8 ) 
             , GrandChildFmtNo ( Child8 ) 
             ) 
    ; RETURN 
        MkNode ( LT . FsCondMember , MkId ( ClassId ) , 1 , LChildren , 4 ) 
    END MkFsCondMember 

; PROCEDURE MkFsChildPlain 
    ( ChildName : LbeStd . TokTyp ; ChildClass : LbeStd . TokTyp ) 
  : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = BEGIN (* MkFsChildPlain *) 
      RETURN 
        MkNode 
          ( LT . FsChildPlain 
          , MkChildId ( ChildName ) 
          , 0 
          , MkId ( ChildClass ) 
          , 3 
          ) 
    END MkFsChildPlain 

; PROCEDURE MkFsChildPlain0 
    ( ChildClass : LbeStd . TokTyp ) : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = BEGIN (* MkFsChildPlain0 *) 
      RETURN 
        MkNode 
          ( LT . FsChildPlain , NIL , 0 , MkId ( ChildClass ) , 3 ) 
    END MkFsChildPlain0 

; TYPE FmtKindTyp = { VERT , HORIZ , FILL } 

; CONST VERT = FmtKindTyp . VERT 
; CONST HORIZ = FmtKindTyp . HORIZ 
; CONST FILL = FmtKindTyp . FILL 

; PROCEDURE RuleList ( ) : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = VAR RuleList_MergeState : EstBuild . MergeStateTyp 

  (* Utility procedures for adding rules to the list. *) 

  ; PROCEDURE Child ( ChildTree : LbeStd . EstRootTyp ) 
    RAISES { Backout } 

    = BEGIN (* Child *) 
        EstBuild . MergeChild 
          ( RuleList_MergeState 
          , ChildTree 
          , KindSet ( ChildTree ) 
          , IsFirstOfGroup := TRUE 
          , GroupFmtNo := EstHs . FmtNoListEstChild 
          ) 
      END Child 

  ; PROCEDURE AddAsTokNameRule ( Lhs : LbeStd . TokTyp ; Rhs : TEXT ) 
    RAISES { Backout } 

    = BEGIN (* AddAsTokNameRule *) 
        Child ( MkAsTokNameRule ( Lhs , Rhs ) ) 
      END AddAsTokNameRule 

  ; PROCEDURE AddCsStarRule 
      ( Lhs : LbeStd . TokTyp 
      ; ListChildTok : LbeStd . TokTyp 
      ; Sep1 : TEXT := NIL 
      ) 
    RAISES { Backout } 

    = BEGIN (* AddCsStarRule *) 
        Child ( MkCsListRule ( LT . CsStarRule , Lhs , ListChildTok , Sep1 ) ) 
      END AddCsStarRule 

  ; PROCEDURE AddCsPlusRule 
      ( Lhs : LbeStd . TokTyp 
      ; ListChildTok : LbeStd . TokTyp 
      ; Sep1 : TEXT := NIL 
      ) 
    RAISES { Backout } 

    = BEGIN (* AddCsPlusRule *) 
        Child ( MkCsListRule ( LT . CsPlusRule , Lhs , ListChildTok , Sep1 ) ) 
      END AddCsPlusRule 

  ; PROCEDURE AddCsFixedRule 
      ( Lhs : LbeStd . TokTyp 
      ; Child1 : REFANY := NIL 
      ; Child2 : REFANY := NIL 
      ; Child3 : REFANY := NIL 
      ; Child4 : REFANY := NIL 
      ; Child5 : REFANY := NIL 
      ; Child6 : REFANY := NIL 
      ; Child7 : REFANY := NIL 
      ; Child8 : REFANY := NIL 
      ; Child9 : REFANY := NIL 
      ) 
    RAISES { Backout } 

    = BEGIN (* AddCsFixedRule *) 
        Child 
          ( MkCsFixedRule 
              ( Lhs 
              , Child1 
              , Child2 
              , Child3 
              , Child4 
              , Child5 
              , Child6 
              , Child7 
              , Child8 
              , Child9 
              ) 
          ) 
      END AddCsFixedRule 

  ; PROCEDURE AddFsListRuleTok 
      ( Lhs : LbeStd . TokTyp 
      ; FmtKind : FmtKindTyp 
      ; ListChildTok : LbeStd . TokTyp 
      ; Sep1 : TEXT := NIL 
      ; Sep2 : TEXT := NIL 
      ) 
    RAISES { Backout } 

    = BEGIN (* AddFsListRuleTok *) 
        Child 
          ( MkFsListRule 
              ( Lhs 
              , FmtKind 
              , MkNode 
                  ( LT . FsChildPlain 
                  , MkChildId ( LCT . Elements ) 
                  , 0 
                  , MkId ( ListChildTok ) 
                  , 3 
                  ) 
              , Sep1 
              , Sep2 
              ) 
          ) 
      END AddFsListRuleTok 

  ; PROCEDURE AddFsListRule 
      ( Lhs : LbeStd . TokTyp 
      ; FmtKind : FmtKindTyp 
      ; ListChild : LbeStd . EstRootTyp 
      ; Sep1 : TEXT := NIL 
      ; Sep2 : TEXT := NIL 
      ) 
    RAISES { Backout } 

    = BEGIN (* AddFsListRule *) 
        Child ( MkFsListRule ( Lhs , FmtKind , ListChild , Sep1 , Sep2 ) ) 
      END AddFsListRule 

  ; PROCEDURE AddFsFixedRule 
      ( Lhs : LbeStd . TokTyp 
      ; FmtKind : FmtKindTyp 
      ; Child1 : REFANY := NIL 
      ; Child2 : REFANY := NIL 
      ; Child3 : REFANY := NIL 
      ; Child4 : REFANY := NIL 
      ; Child5 : REFANY := NIL 
      ; Child6 : REFANY := NIL 
      ; Child7 : REFANY := NIL 
      ; Child8 : REFANY := NIL 
      ) 
    RAISES { Backout } 

    = BEGIN (* AddFsFixedRule *) 
        Child 
          ( MkFsFixedRule 
              ( Lhs 
              , FmtKind 
              , Child1 
              , Child2 
              , Child3 
              , Child4 
              , Child5 
              , Child6 
              , Child7 
              , Child8 
              ) 
          ) 
      END AddFsFixedRule 

  ; VAR LResult : EstHs . EstRefTyp 

  ; BEGIN (* RuleList *) 
      RuleList_MergeState 
        := EstBuild . NewMergeState 
             ( Lang := GLang 
             , EstTok := LT . RuleList 
             , EstRefToInheritFrom := NIL 
             ) 

(* The Concrete Syntax of Ldl0: *) 

  (* Concrete Syntax of Entire language definition. *) 

    (* LanguageDefinition ::= "LDL" Ident RuleList "END" Ident . *) 
    ; AddCsFixedRule 
        ( LT . LanguageDefinition 
        , "LDL" 
        , MkId ( LT . Ident ) 
        , MkId ( LT . RuleList ) 
        , "END" 
        , MkId ( LT . Ident ) 
        ) 

  (* Concrete Syntax of Start symbol specification: *) 

    (* StartRule ::= "START" Ident "." . *) 
    ; AddCsFixedRule ( LT . StartRule , "START" , MkId ( LT . Ident ) , "." ) 

  (* Concrete Syntax of Abstract Syntax Rules: *) 

    (* AsVarTermRule ::= Ident "VARTERM" "." . *) 
    ; AddCsFixedRule 
        ( LT . AsVarTermRule , MkId ( LT . Ident ) , "VARTERM" , "." ) 

    (* AsFixedRule ::= IdentPlusList ":=" AsChildList "." . *) 
    ; AddCsFixedRule 
        ( LT . AsFixedRule 
        , MkId ( LT . IdentPlusList ) 
        , ":=" 
        , MkId ( LT . AsChildList ) 
        , "." 
        ) 

    (* AsReqdChild ::= [ Ident ":" ] Ident . *) 
    ; AddCsFixedRule 
        ( LT . AsReqdChild 
        , MkCsOptChild ( MkId ( LT . Ident ) , ":" ) 
        , MkId ( LT . Ident ) 
        ) 

    (* AsOptChild ::= [ Ident ":" ] "[" Ident "]" . *) 
    ; AddCsFixedRule 
        ( LT . AsOptChild 
        , MkCsOptChild ( MkId ( LT . Ident ) , ":" ) 
        , "[" 
        , MkId ( LT . Ident ) 
        , "]" 
        ) 

    (* AsStarRule ::= IdentPlusList ":=" [ Ident ":" ] "{" Ident "}" "." . *) 
    ; AddCsFixedRule 
        ( LT . AsStarRule 
        , MkId ( LT . IdentPlusList ) 
        , ":=" 
        , MkCsOptChild ( MkId ( LT . Ident ) , ":" ) 
        , "{" 
        , MkId ( LT . Ident ) 
        , "}" 
        , "." 
        ) 

    (* AsPlusRule ::= IdentPlusList ":=" [ Ident ":" ] "{" Ident "}+" "." . *) 
    ; AddCsFixedRule 
        ( LT . AsPlusRule 
        , MkId ( LT . IdentPlusList ) 
        , ":=" 
        , MkCsOptChild ( MkId ( LT . Ident ) , ":" ) 
        , "{" 
        , MkId ( LT . Ident ) 
        , "}+" 
        , "." 
        ) 

    (* AsClassRule ::= Ident "=" Alternation "." . *) 
    ; AddCsFixedRule 
        ( LT . AsClassRule 
        , MkId ( LT . Ident ) 
        , "=" 
        , MkId ( LT . Alternation ) 
        , "." 
        ) 

  (* Concrete Syntax of Productions of concrete syntax: *) 

    (* CsAltRule  ::= Ident "::=" Alternation "." . *) 
    ; AddCsFixedRule 
        ( LT . CsAltRule 
        , MkId ( LT . Ident ) 
        , "::=" 
        , MkId ( LT . Alternation ) 
        , "." 
        ) 

    (* CsFixedRule ::= Ident "::=" Concatenation "." . *) 
    ; AddCsFixedRule 
        ( LT . CsFixedRule 
        , MkId ( LT . Ident ) 
        , "::=" 
        , MkId ( LT . Concatenation ) 
        , "." 
        ) 

    (* CsChildCs ::= CsReqdChild | CsOptChildCs *) 
    ; Child 
        ( MkNode 
            ( LT . CsAltRule 
            , MkId ( LT . CsChildCs ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . CsReqdChild ) 
                , 0 
                , MkId ( LT . CsOptChildCs ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* CsOptChildCs ::= "[" Concatentation "]" . *) 
    ; AddCsFixedRule 
        ( LT . CsOptChildCs , "[" , MkId ( LT . Concatenation ) , "]" ) 

    (* CsStarRule ::= Ident "::=" "{" CsAtom [ "||" CsAtomList ] "}" "." . *) 
    ; AddCsFixedRule 
        ( LT . CsStarRule 
        , MkId ( LT . Ident ) 
        , "::=" 
        , "{" 
        , MkId ( LT . CsAtom ) 
        , MkCsOptChild ( "||" , MkId ( LT . CsAtomList ) ) 
        , "}" 
        , "." 
        ) 

    (* CsPlusRule ::= Ident "::=" "{" CsAtom [ "||" CsAtomList ] "}+" "." . *) 
    ; AddCsFixedRule 
        ( LT . CsPlusRule 
        , MkId ( LT . Ident ) 
        , "::=" 
        , "{" 
        , MkId ( LT . CsAtom ) 
        , MkCsOptChild ( "||" , MkId ( LT . CsAtomList ) ) 
        , "}+" 
        , "." 
        ) 

  (* Concrete Syntax of Precedence and associativity specification: *) 

    (* PrecRule ::= "PREC" PrecLevels "." . *) 
    ; AddCsFixedRule 
        ( LT . PrecRule , "PREC" , MkId ( LT . PrecLevels ) , "." ) 

    (* PrecLevelNone ::= "NONE" CsAtomList . *) 
    ; AddCsFixedRule ( LT . PrecLevelNone , "NONE" , MkId ( LT . CsAtomList ) ) 

    (* PrecLevelLeft ::= "LEFT" CsAtomList . *) 
    ; AddCsFixedRule ( LT . PrecLevelLeft , "LEFT" , MkId ( LT . CsAtomList ) ) 

    (* PrecLevelRight ::= "RIGHT" CsAtomList . *) 
    ; AddCsFixedRule 
        ( LT . PrecLevelRight , "RIGHT" , MkId ( LT . CsAtomList ) ) 

  (* Concrete Syntax of Format Syntax Rules: *) 

    (* FsFixedDefaultRule ::= IdentPlusList "->" FsFixedChildList "." . *) 
    ; AddCsFixedRule 
        ( LT . FsFixedDefaultRule 
        , MkId ( LT . IdentPlusList ) 
        , "->" 
        , MkId ( LT . FsFixedChildList ) 
        , "." 
        ) 

    (* FsFixedHorizRule ::= IdentPlusList "->" "HORIZ" FsFixedChildList "." . *) 
    ; AddCsFixedRule 
        ( LT . FsFixedHorizRule 
        , MkId ( LT . IdentPlusList ) 
        , "->" 
        , "HORIZ"
        , MkId ( LT . FsFixedChildList ) 
        , "." 
        ) 

    (* FsFixedVertRule ::= IdentPlusList "->" "VERT" FsFixedChildList "." . *) 
    ; AddCsFixedRule 
        ( LT . FsFixedVertRule 
        , MkId ( LT . IdentPlusList ) 
        , "->" 
        , "VERT" 
        , MkId ( LT . FsFixedChildList ) 
        , "." 
        ) 

    (* FsFixedFillRule ::= IdentPlusList "->" "FILL" FsFixedChildList "." . *) 
    ; AddCsFixedRule 
        ( LT . FsFixedFillRule 
        , MkId ( LT . IdentPlusList ) 
        , "->" 
        , "FILL" 
        , MkId ( LT . FsFixedChildList ) 
        , "." 
        ) 

    (* DontCare ::= "_" . *) 
    ; AddCsFixedRule ( LT . DontCare , "_" ) 

    (* LineBreak ::= "!" [ "@" Integer ] . *) 
    ; AddCsFixedRule 
        ( LT . LineBreak , "!" , MkCsOptChild ( "@" , MkId ( LT . Integer ) ) ) 

    (* FsDefaultSubtree ::= "(" FsFixedChildList ")" . *) 
    ; AddCsFixedRule 
        ( LT . FsDefaultSubtree 
        , "(" 
        , MkId ( LT . FsFixedChildList ) 
        , ")" 
        ) 

    (* FsHorizSubtree ::= "HORIZ" "(" FsFixedChildList ")" . *) 
    ; AddCsFixedRule 
        ( LT . FsHorizSubtree 
        , "HORIZ" 
        , "(" 
        , MkId ( LT . FsFixedChildList ) 
        , ")" 
        ) 

    (* FsVertSubtree ::= "VERT" "(" FsFixedChildList ")" . *) 
    ; AddCsFixedRule 
        ( LT . FsVertSubtree 
        , "VERT" 
        , "(" 
        , MkId ( LT . FsFixedChildList ) 
        , ")" 
        ) 

    (* FsFillSubtree ::= "FILL" "(" FsFixedChildList ")" . *) 
    ; AddCsFixedRule 
        ( LT . FsFillSubtree 
        , "FILL" 
        , "(" 
        , MkId ( LT . FsFixedChildList ) 
        , ")" 
        ) 

    (* FsChildPlain ::= [ Ident ":" ] IdentOrDontCare . *) 
    ; AddCsFixedRule 
        ( LT . FsChildPlain 
        , MkCsOptChild ( MkId ( LT . Ident ) , ":" ) 
        , MkId ( LT . IdentOrDontCare ) 
        ) 

    (* FsCondPresent ::= "PRESENT" "(" FsFixedChildList ")" . *) 
    ; AddCsFixedRule 
        ( LT . FsCondPresent 
        , "PRESENT" 
        , "(" 
        , MkId ( LT . FsFixedChildList ) 
        , ")" 
        ) 

    (* FsCondNonempty ::= "NONEMPTY" "(" FsFixedChildList ")" . *) 
    ; AddCsFixedRule 
        ( LT . FsCondNonempty 
        , "NONEMPTY" 
        , "(" 
        , MkId ( LT . FsFixedChildList ) 
        , ")" 
        ) 

    (* FsCondPlural ::= "PLURAL" "(" FsFixedChildList ")" . *) 
    ; AddCsFixedRule 
        ( LT . FsCondPlural 
        , "PLURAL" 
        , "(" 
        , MkId ( LT . FsFixedChildList ) 
        , ")" 
        ) 

    (* FsCondMember ::= "MEMBER" Ident "(" FsFixedChildList ")" . *) 
    ; AddCsFixedRule 
        ( LT . FsCondMember 
        , "MEMBER" 
        , MkId ( LT . Ident ) 
        , "(" 
        , MkId ( LT . FsFixedChildList ) 
        , ")" 
        ) 

    (* FsListDefaultRule 
      ::= IdentPlusList "->" 
          "{" FsChild [ "||" FormatterList ] "}" "." . *) 
    ; AddCsFixedRule 
        ( LT . FsListDefaultRule 
        , MkId ( LT . IdentPlusList ) 
        , "->" 
        , "{" 
        , MkId ( LT . FsChild ) 
        , MkCsOptChild ( "||" , MkId ( LT . FormatterList ) ) 
        , "}" 
        , "." 
        ) 

    (* FsListHorizRule 
      ::= IdentPlusList "->" "HORIZ" 
          "{" FsChild [ "||" FormatterList ] "}" "." . *) 
    ; AddCsFixedRule 
        ( LT . FsListHorizRule 
        , MkId ( LT . IdentPlusList ) 
        , "->" 
        , "HORIZ"
        , "{" 
        , MkId ( LT . FsChild ) 
        , MkCsOptChild ( "||" , MkId ( LT . FormatterList ) ) 
        , "}" 
        , "." 
        ) 

    (* FsListVertRule 
      ::= IdentPlusList "->" "VERT" 
          "{" FsChild [ "||" FormatterList ] "}" "." . *) 
    ; AddCsFixedRule 
        ( LT . FsListVertRule 
        , MkId ( LT . IdentPlusList ) 
        , "->" 
        , "VERT" 
        , "{" 
        , MkId ( LT . FsChild ) 
        , MkCsOptChild ( "||" , MkId ( LT . FormatterList ) ) 
        , "}" 
        , "." 
        ) 

    (* FsListFillRule 
      ::= IdentPlusList "->" "FILL" 
          "{" FsChild [ "||" FormatterList ] "}" "." . *) 
    ; AddCsFixedRule 
        ( LT . FsListFillRule 
        , MkId ( LT . IdentPlusList ) 
        , "->" 
        , "FILL" 
        , "{" 
        , MkId ( LT . FsChild ) 
        , MkCsOptChild ( "||" , MkId ( LT . FormatterList ) ) 
        , "}" 
        , "." 
        ) 

  (* Concrete syntax of AsList rules: *) 

    (* RuleList ::= { Rule } . *) 
    ; AddCsStarRule ( LT . RuleList , LT . Rule ) 

    (* AsChildList ::= { AsChild || ";" } . *) 
    ; AddCsStarRule ( LT . AsChildList , LT . AsChild , ";" ) 

    (* IdentPlusList ::= { Ident || "|" }+ . *) 
    ; AddCsPlusRule ( LT . IdentPlusList , LT . Ident , "|" ) 

    (* Alternation ::= { CsAtom || "|" }+ . *) 
    ; AddCsPlusRule ( LT . Alternation , LT . CsAtom , "|" ) 

    (* Concatenation ::= { CsChildCs } . *) 
    ; AddCsStarRule ( LT . Concatenation , LT . CsChildCs ) 

    (* CsAtomList ::= { CsAtom } . *) 
    ; AddCsStarRule ( LT . CsAtomList , LT . CsAtom ) 

    (* PrecLevels ::= { PrecLevel } . *) 
    ; AddCsStarRule ( LT . PrecLevels , LT . PrecLevel ) 

    (* FsFixedChildList ::= { FsFixedChild } . *) 
    ; AddCsStarRule ( LT . FsFixedChildList , LT . FsFixedChild ) 

    (* FormatterList ::= { Formatter } . *) 
    ; AddCsStarRule ( LT . FormatterList , LT . Formatter ) 

  (* Format syntax of FsList rules: *) 

    (* RuleList -> VERT { Rule || ! } . *) 
    ; AddFsListRuleTok ( LT . RuleList , VERT , LT . Rule , "BANG" ) 

    (* AsChildList -> { AsChild || ! ";" } . *) 
    ; AddFsListRuleTok 
        ( LT . AsChildList , HORIZ , LT . AsChild , "BANG" , ";" ) 

    (* IdentPlusList -> { Ident || ! "|" } . *) 
    ; AddFsListRuleTok 
        ( LT . IdentPlusList , HORIZ , LT . Ident , "BANG" , "|" ) 

    (* Alternation -> FILL { CsAtom || ! "|" } . *) 
    ; AddFsListRuleTok ( LT . Alternation , FILL , LT . CsAtom , "BANG" , "|" ) 
    (* Concatenation 
       -> HORIZ { MEMBER Concatenation ( "[" CsChild "]" ) || ! } . *) 
    ; AddFsListRule 
        ( LT . Concatenation 
        , HORIZ 
        , MkFsCondMember 
            ( LT . Concatenation 
            , "[" 
            , MkFsChildPlain0 ( LT . CsChild ) 
            , "]" 
            ) 
        , "BANG" 
        ) 

    (* CsAtomList -> { CsAtom } . *) 
    ; AddFsListRuleTok ( LT . CsAtomList , HORIZ , LT . CsAtom ) 

    (* PrecLevels -> VERT { PrecLevel } . *) 
    ; AddFsListRuleTok ( LT . PrecLevels , VERT , LT . PrecLevel ) 

    (* FsFixedChildList -> { FsFixedChild || ! } . *) 
    ; AddFsListRuleTok 
        ( LT . FsFixedChildList , HORIZ , LT . FsFixedChild , "BANG" ) 

    (* FormatterList -> { Formatter || ! } . *) 
    ; AddFsListRuleTok ( LT . FormatterList , HORIZ , LT . Formatter , "BANG" ) 
  (* Format Syntax of Entire language definition. *) 

    (* LanguageDefinition 
         -> VERT "LDL" LanguageName : Ident 
            ! @ 1  Rules : RuleList 
            !    "END" ClosingName : Ident 
         . *) 
    ; AddFsFixedRule 
        ( LT . LanguageDefinition 
        , VERT 
        , "LDL" 
        , MkFsChildPlain ( LCT . LanguageName , LT . Ident ) 
        , "BANG-1" 
        , MkFsChildPlain ( LCT . Rules , LT . RuleList ) 
        , "BANG" 
        , "END" 
        , MkFsChildPlain ( LCT . ClosingName , LT . Ident ) 
        ) 

    (* Format Syntax of Start symbol specification: *) 

    (* StartRule -> "START" StartName : Ident "." . *) 
    ; AddFsFixedRule 
        ( LT . StartRule 
        , HORIZ 
        , "START" 
        , MkFsChildPlain ( LCT . StartName , LT . Ident ) 
        , "." 
        ) 

     (* Format Syntax of Abstract Syntax Rules: *) 

    (* AsVarTermRule 
         -> HORIZ Name : Ident 
            ! @ 2 "VARTERM" "." . *) 
    ; AddFsFixedRule 
        ( LT . AsVarTermRule 
        , HORIZ 
        , MkFsChildPlain ( LCT . Name , LT . Ident ) 
        , "BANG-2" 
        , "VARTERM" 
        , "." 
        ) 

    (* AsFixedRule 
         -> HORIZ IdentPlusList 
            ! @ 2 ":=" AsChildList ! @ 2 "." . *) 
    ; AddFsFixedRule 
        ( LT . AsFixedRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , ":=" 
        , MkFsChildPlain0 ( LT . AsChildList ) 
        , "BANG-2" 
        , "." 
        ) 

    (* AsReqdChild -> PRESENT ( Ident ":" ) ! @ 2 Ident . *) 
    ; AddFsFixedRule 
        ( LT . AsReqdChild 
        , HORIZ 
        , MkFsCondPresent ( MkFsChildPlain0 ( LT . Ident ) , ":" ) 
        , "BANG-2" 
        , MkFsChildPlain0 ( LT . Ident ) 
        ) 

    (* AsOptChild -> PRESENT ( Ident ":" ) ! @ 2 "[" Ident "]" . *) 
    ; AddFsFixedRule 
        ( LT . AsOptChild 
        , HORIZ 
        , MkFsCondPresent ( MkFsChildPlain0 ( LT . Ident ) , ":" ) 
        , "BANG-2" 
        , "[" 
        , MkFsChildPlain0 ( LT . Ident ) 
        , "]" 
        ) 

    (* AsStarRule 
         -> IdentPlusList 
            ! @ 2 ":=" ( PRESENT ( Ident ":" ) ! @ 2 "{" Ident "}" ) ! @ 2 "." . *) 
    ; AddFsFixedRule 
        ( LT . AsStarRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , ":=" 
        , MkFsHorizSubtree 
            ( MkFsCondPresent ( MkFsChildPlain0 ( LT . Ident ) , ":" ) 
            , "BANG-2" 
            , "{" 
            , MkFsChildPlain0 ( LT . Ident ) 
            , "}" 
            ) 
        , "BANG-2" 
        , "." 
        ) 

    (* AsPlusRule 
         -> IdentPlusList 
            ! @ 2 ":=" ( PRESENT ( Ident ":" ) ! @ 2 "{" Ident "}+" ) ! @ 2 "." . *) 
    ; AddFsFixedRule 
        ( LT . AsPlusRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , ":=" 
        , MkFsHorizSubtree 
            ( MkFsCondPresent ( MkFsChildPlain0 ( LT . Ident ) , ":" ) 
            , "BANG-2" 
            , "{" 
            , MkFsChildPlain0 ( LT . Ident ) 
            , "}+" 
            ) 
        , "BANG-2" 
        , "." 
        ) 

    (* AsClassRule 
         -> Ident 
            ! @ 2 "=" Alternation ! @ 2 "." . *) 
    ; AddFsFixedRule 
        ( LT . AsClassRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . Ident ) 
        , "BANG-2" 
        , "=" 
        , MkFsChildPlain0 ( LT . Alternation ) 
        , "BANG-2" 
        , "." 
        ) 

     (* Format Syntax of Productions of concrete syntax: *) 

    (* CsAltRule 
         -> HORIZ Ident 
            ! @ 2   "::=" Alternation "." 
         . *) 
    ; AddFsFixedRule 
        ( LT . CsAltRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . Ident ) 
        , "BANG-2" 
        , "::=" 
        , MkFsChildPlain0 ( LT . Alternation ) 
        , "." 
        ) 

    (* CsFixedRule -> HORIZ Ident ! @ 2 "::=" Concatenation "." . *) 
    ; AddFsFixedRule 
        ( LT . CsFixedRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . Ident ) 
        , "BANG-2" 
        , "::=" 
        , MkFsChildPlain0 ( LT . Concatenation ) 
        , "." 
        ) 

    (* CsStarRule 
         -> HORIZ Ident 
            ! @ 2   "::=" 
                 HORIZ ( "{" CsAtom NONEMPTY ( ! @ 3 "||" CsAtomList ) ! "}" ) "." 
         . *) 
    ; AddFsFixedRule 
        ( LT . CsStarRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . Ident ) 
        , "BANG-2" 
        , "::=" 
        , MkFsHorizSubtree 
            ( "{" 
            , MkFsChildPlain0 ( LT . CsAtom ) 
            , MkFsCondPresent 
                ( "BANG-3" , "||" , MkFsChildPlain0 ( LT . CsAtomList ) ) 
            , "BANG" 
            , "}" 
            ) 
        , "." 
        ) 

    (* CsPlusRule 
         -> HORIZ Ident 
            ! @ 2   "::=" 
                 HORIZ ( "{" CsAtom NONEMPTY ( ! @ 3 "||" CsAtomList ) ! "}+" )  "." 
         . *) 
    ; AddFsFixedRule 
        ( LT . CsPlusRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . Ident ) 
        , "BANG-2" 
        , "::=" 
        , MkFsHorizSubtree 
            ( "{" 
            , MkFsChildPlain0 ( LT . CsAtom ) 
            , MkFsCondPresent 
                ( "BANG-3" , "||" , MkFsChildPlain0 ( LT . CsAtomList ) ) 
            , "BANG" 
            , "}+" 
            ) 
        , "." 
        ) 

     (* Format Syntax of Precedence and associativity specification: *) 

    (* PrecRule -> VERT "PREC" PrecLevels ! "." . *) 
    ; AddFsFixedRule 
        ( LT . PrecRule 
        , VERT 
        , "PREC" 
        , MkFsChildPlain0 ( LT . PrecLevels ) 
        , "BANG" 
        , "." 
        ) 

    (* PrecLevelNone -> "NONE" CsAtomList . *) 
    ; AddFsFixedRule 
        ( LT . PrecLevelNone 
        , HORIZ 
        , "NONE" 
        , MkFsChildPlain0 ( LT . CsAtomList ) 
        ) 

    (* PrecLevelLeft -> "LEFT" CsAtomList . *) 
    ; AddFsFixedRule 
        ( LT . PrecLevelLeft 
        , HORIZ 
        , "LEFT" 
        , MkFsChildPlain0 ( LT . CsAtomList ) 
        ) 

    (* PrecLevelRight -> "RIGHT" CsAtomList . *) 
    ; AddFsFixedRule 
        ( LT . PrecLevelRight 
        , HORIZ 
        , "RIGHT" 
        , MkFsChildPlain0 ( LT . CsAtomList ) 
        ) 

     (* Format Syntax of Format Syntax Rules: *) 

    (* FsFixedDefaultRule 
         -> IdentPlusList 
            ! @ 2 "->" HORIZ ( ! @ 2 FsFixedChildList ) 
            ! @ 2 "." 
         . *) 
    ; AddFsFixedRule 
        ( LT . FsFixedDefaultRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , "->" 
        , MkFsHorizSubtree 
            ( "BANG-2" , MkFsChildPlain0 ( LT . FsFixedChildList ) ) 
        , "BANG-2" 
        , "." 
        ) 

    (* FsFixedHorizRule 
         -> IdentPlusList 
            ! @ 2 "->" HORIZ ( "HORIZ" ! @ 2 FsFixedChildList ) 
            ! @ 2 "." 
         . *) 
    ; AddFsFixedRule 
        ( LT . FsFixedHorizRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , "->" 
        , MkFsHorizSubtree 
            ( "HORIZ" , "BANG-2" , MkFsChildPlain0 ( LT . FsFixedChildList ) ) 
        , "BANG-2" 
        , "." 
        ) 

    (* FsFixedVertRule 
         -> IdentPlusList 
            ! @ 2 "->" "VERT" FsFixedChildList ! @ 2 "." . *) 
    ; AddFsFixedRule 
        ( LT . FsFixedVertRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , "->" 
        , "VERT" 
        , MkFsChildPlain0 ( LT . FsFixedChildList ) 
        , "BANG-2" 
        , "." 
        ) 

    (* FsFixedFillRule 
         -> IdentPlusList 
            ! @ 2 "->" "FILL" FsFixedChildList ! @ 2 "." . *) 
    ; AddFsFixedRule 
        ( LT . FsFixedFillRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , "->" 
        , "FILL" 
        , MkFsChildPlain0 ( LT . FsFixedChildList ) 
        , "BANG-2" 
        , "." 
        ) 

    (* LineBreak -> "!" PRESENT ( "@" Integer ) . *) 
    ; AddFsFixedRule 
        ( LT . LineBreak 
        , HORIZ 
        , "!" 
        , MkFsCondPresent ( "@" , MkFsChildPlain0 ( LT . Integer ) ) 
        ) 

    (* FsDefaultSubtree 
         -> "(" FsFixedChildList ! ")" . *) 
    ; AddFsFixedRule 
        ( LT . FsDefaultSubtree 
        , HORIZ 
        , "(" 
        , MkFsChildPlain0 ( LT . FsFixedChildList ) 
        , "BANG" 
        , ")" 
        ) 

    (* FsHorizSubtree 
         -> "HORIZ" ! @ 2 "(" FsFixedChildList ! @ 2 ")" . *) 
    ; AddFsFixedRule 
        ( LT . FsHorizSubtree 
        , HORIZ 
        , "HORIZ" 
        , "BANG-2" 
        , "(" 
        , MkFsChildPlain0 ( LT . FsFixedChildList ) 
        , "BANG-2" 
        , ")" 
        ) 

    (* FsVertSubtree 
         -> "VERT" ! @ 2 "(" FsFixedChildList ! @ 2 ")" . *) 
    ; AddFsFixedRule 
        ( LT . FsVertSubtree 
        , HORIZ 
        , "VERT" 
        , "BANG-2" 
        , "(" 
        , MkFsChildPlain0 ( LT . FsFixedChildList ) 
        , "BANG-2" 
        , ")" 
        ) 

    (* FsFillSubtree 
         -> "FILL" ! @ 2 "(" FsFixedChildList ! @ 2 ")" . *) 
    ; AddFsFixedRule 
        ( LT . FsFillSubtree 
        , HORIZ 
        , "FILL" 
        , "BANG-2" 
        , "(" 
        , MkFsChildPlain0 ( LT . FsFixedChildList ) 
        , "BANG-2" 
        , ")" 
        ) 

    (* FsChildPlain -> PRESENT ( Ident ":" ) ! @ 2 IdentOrDontCare . *) 
    ; AddFsFixedRule 
        ( LT . FsChildPlain 
        , HORIZ 
        , MkFsCondPresent ( MkFsChildPlain0 ( LT . Ident ) , ":" ) 
        , "BANG-2" 
        , MkFsChildPlain0 ( LT . IdentOrDontCare ) 
        ) 

    (* DontCare -> "_" . *) 
    ; AddFsFixedRule ( LT . DontCare , HORIZ , "_" ) 

    (* FsCondPresent -> "PRESENT" ! @ 2 "(" FsFixedChildList ! @ 2 ")" . *) 
    ; AddFsFixedRule 
        ( LT . FsCondPresent 
        , HORIZ 
        , "PRESENT" 
        , "BANG-2" 
        , "(" 
        , MkFsChildPlain0 ( LT . FsFixedChildList ) 
        , "BANG-2" 
        , ")" 
        ) 

    (* FsCondNonempty -> "NONEMPTY" ! @ 2 "(" FsFixedChildList ! @ 2 ")" . *) 
    ; AddFsFixedRule 
        ( LT . FsCondNonempty 
        , HORIZ 
        , "NONEMPTY" 
        , "BANG-2" 
        , "(" 
        , MkFsChildPlain0 ( LT . FsFixedChildList ) 
        , "BANG-2" 
        , ")" 
        ) 

    (* FsCondPlural -> "PLURAL" ! @ 2 "(" FsFixedChildList ! @ 2 ")" . *) 
    ; AddFsFixedRule 
        ( LT . FsCondPlural 
        , HORIZ 
        , "PLURAL" 
        , "BANG-2" 
        , "(" 
        , MkFsChildPlain0 ( LT . FsFixedChildList ) 
        , "BANG-2" 
        , ")" 
        ) 

    (* FsCondMember -> "MEMBER" Ident ! @ 2 "(" FsFixedChildList ! @ 2 ")" . *) 
    ; AddFsFixedRule 
        ( LT . FsCondMember 
        , HORIZ 
        , "MEMBER" 
        , MkFsChildPlain0 ( LT . Ident ) 
        , "BANG-2" 
        , "(" 
        , MkFsChildPlain0 ( LT . FsFixedChildList ) 
        , "BANG-2" 
        , ")" 
        ) 

    (* FsListDefaultRule 
         -> IdentPlusList 
            ! @ 2 "->" 
              ( ! @ 2 "{" FsChild NONEMPTY ( "||" FormatterList ) "}" ) 
            ! @ 2 "." 
         . *) 
    ; AddFsFixedRule 
        ( LT . FsListDefaultRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , "->" 
        , MkFsHorizSubtree 
            ( "BANG-2" 
            , "{" 
            , MkFsChildPlain0 ( LT . FsChild ) 
            , MkFsCondPresent 
                ( "||" , MkFsChildPlain0 ( LT . FormatterList ) ) 
            , "}" 
            ) 
        , "BANG-2" 
        , "." 
        ) 

    (* FsListHorizRule 
         -> IdentPlusList 
            ! @ 2 "->" 
              ( "HORIZ" ! @ 2 "{" FsChild NONEMPTY ( "||" FormatterList ) "}" ) 
            ! @ 2 "." 
         . *) 
    ; AddFsFixedRule 
        ( LT . FsListHorizRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , "->" 
        , MkFsHorizSubtree 
            ( "HORIZ" 
            , "BANG-2" 
            , "{" 
            , MkFsChildPlain0 ( LT . FsChild ) 
            , MkFsCondPresent 
                ( "||" , MkFsChildPlain0 ( LT . FormatterList ) ) 
            , "}" 
            ) 
        , "BANG-2" 
        , "." 
        ) 

    (* FsListVertRule 
         -> IdentPlusList 
            ! @ 2 "->" 
              ( "VERT" ! @ 2 "{" FsChild NONEMPTY ( "||" FormatterList ) "}" ) 
            ! @ 2 "." 
         . *) 
    ; AddFsFixedRule 
        ( LT . FsListVertRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , "->" 
        , MkFsHorizSubtree 
            ( "VERT" 
            , "BANG-2" 
            , "{" 
            , MkFsChildPlain0 ( LT . FsChild ) 
            , MkFsCondPresent 
                ( "||" , MkFsChildPlain0 ( LT . FormatterList ) ) 
            , "}" 
            ) 
        , "BANG-2" 
        , "." 
        ) 

    (* FsListFillRule 
         -> IdentPlusList 
            ! @ 2 "->" 
              ( "FILL" ! @ 2 "{" FsChild NONEMPTY ( "||" FormatterList ) "}" ) 
            ! @ 2 "." 
         . *) 
    ; AddFsFixedRule 
        ( LT . FsListFillRule 
        , HORIZ 
        , MkFsChildPlain0 ( LT . IdentPlusList ) 
        , "BANG-2" 
        , "->" 
        , MkFsHorizSubtree 
            ( "FILL" 
            , "BANG-2" 
            , "{" 
            , MkFsChildPlain0 ( LT . FsChild ) 
            , MkFsCondPresent 
                ( "||" , MkFsChildPlain0 ( LT . FormatterList ) ) 
            , "}" 
            ) 
        , "BANG-2" 
        , "." 
        ) 


    (* Abstract Syntax of Ldl: *) 

    (* FsListDefaultRule | FsListHorizRule | FsListVertRule | FsListFillRule 
         := Parents : IdentPlusList 
            ; ListChild : FsChild 
            ; Formatters : [ FormatterList ]  
         . 
    *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode 
                ( LT . IdentPlusList 
                , MkId ( LT . FsListDefaultRule ) 
                , 0 
                , MkId ( LT . FsListHorizRule ) 
                , 0 
                , MkId ( LT . FsListVertRule ) 
                , 0 
                , MkId ( LT . FsListFillRule ) 
                , 0 
                ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Parents ) 
                    , 0 
                    , MkId ( LT . IdentPlusList ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . ListChild ) 
                    , 0 
                    , MkId ( LT . FsChild ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsOptChild 
                    , MkChildId ( LCT . Formatters ) 
                    , 0 
                    , MkId ( LT . FormatterList ) 
                    , 4 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsListRule 
         = FsListDefaultRule | FsListHorizRule | FsListVertRule | FsListFillRule . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . FsListRule ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . FsListDefaultRule ) 
                , 0 
                , MkId ( LT . FsListHorizRule ) 
                , 0 
                , MkId ( LT . FsListVertRule ) 
                , 0 
                , MkId ( LT . FsListFillRule ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsCondMember := Class : Ident ; Children : FsFixedChildList . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . FsCondMember ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Class ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Children ) 
                    , 0 
                    , MkId ( LT . FsFixedChildList ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsCondPresent | FsCondNonempty | FsCondPlural 
         := Children : FsFixedChildList . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode 
                ( LT . IdentPlusList 
                , MkId ( LT . FsCondPresent ) 
                , 0 
                , MkId ( LT . FsCondNonempty ) 
                , 0 
                , MkId ( LT . FsCondPlural ) 
                , 0 
                ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Children ) 
                    , 0 
                    , MkId ( LT . FsFixedChildList ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsChildCondFmt 
        = FsCondPresent | FsCondNonempty | FsCondPlural | FsCondMember . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . FsChildCondFmt ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . FsCondPresent ) 
                , 0 
                , MkId ( LT . FsCondNonempty ) 
                , 0 
                , MkId ( LT . FsCondPlural ) 
                , 0 
                , MkId ( LT . FsCondMember ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* DontCare := . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . DontCare ) , 0 ) 
            , 0 
            , NIL 
            , 3 
            ) 
        ) 

    (* IdentOrDontCare = Ident | DontCare . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . IdentOrDontCare ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . Ident ) 
                , 0 
                , MkId ( LT . DontCare ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsChildPlain 
         := ChildName : [ Ident ] ; ChildClass : IdentOrDontCare . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . FsChildPlain ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsOptChild 
                    , MkChildId ( LCT . ChildName ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 4 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . ChildClass ) 
                    , 0 
                    , MkId ( LT . IdentOrDontCare ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsChild = FsChildPlain | FsSubtree | FsChildCondFmt . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . FsChild ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . FsChildPlain ) 
                , 0 
                , MkId ( LT . FsSubtree ) 
                , 0 
                , MkId ( LT . FsChildCondFmt ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsDefaultSubtree | FsHorizSubtree | FsVertSubtree | FsFillSubtree 
         := Children : FsFixedChildList . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode 
                ( LT . IdentPlusList 
                , MkId ( LT . FsDefaultSubtree ) 
                , 0 
                , MkId ( LT . FsHorizSubtree ) 
                , 0 
                , MkId ( LT . FsVertSubtree ) 
                , 0 
                , MkId ( LT . FsFillSubtree ) 
                , 0 
                ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Children ) 
                    , 0 
                    , MkId ( LT . FsFixedChildList ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsSubtree 
         = FsDefaultSubtree | FsHorizSubtree | FsVertSubtree | FsFillSubtree . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . FsSubtree ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . FsDefaultSubtree ) 
                , 0 
                , MkId ( LT . FsHorizSubtree ) 
                , 0 
                , MkId ( LT . FsVertSubtree ) 
                , 0 
                , MkId ( LT . FsFillSubtree ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* LineBreak := IndentCode : [ Integer ] . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . LineBreak ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsOptChild 
                    , MkChildId ( LCT . IndentCode ) 
                    , 0 
                    , MkId ( LT . Integer ) 
                    , 4 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* Formatter = LineBreak | Ident | String . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . Formatter ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . LineBreak ) 
                , 0 
                , MkId ( LT . Ident ) 
                , 0 
                , MkId ( LT . String ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FormatterList := Elements : { Formatter } . *) 
    ; Child 
        ( MkNode 
            ( LT . AsStarRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . FormatterList ) , 0 ) 
            , 0 
            , MkChildId ( LCT . Elements ) 
            , 3 
            , MkId ( LT . Formatter ) 
            , 7 
            ) 
        ) 

    (* FsFixedChild = FsChild | LineBreak | String . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . FsFixedChild ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . FsChild ) 
                , 0 
                , MkId ( LT . LineBreak ) 
                , 0 
                , MkId ( LT . String ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsFixedChildList := Elements : { FsFixedChild } . *) 
    ; Child 
        ( MkNode 
            ( LT . AsStarRule 
            , MkNode 
                ( LT . IdentPlusList , MkId ( LT . FsFixedChildList ) , 0 ) 
            , 0 
            , MkChildId ( LCT . Elements ) 
            , 3 
            , MkId ( LT . FsFixedChild ) 
            , 7 
            ) 
        ) 

    (* FsFixedDefaultRule | FsFixedHorizRule | FsFixedVertRule | FsFixedFillRule 
         := Parents : IdentPlusList ; Children : FsFixedChildList . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode 
                ( LT . IdentPlusList 
                , MkId ( LT . FsFixedDefaultRule ) 
                , 0 
                , MkId ( LT . FsFixedHorizRule ) 
                , 0 
                , MkId ( LT . FsFixedVertRule ) 
                , 0 
                , MkId ( LT . FsFixedFillRule ) 
                , 0 
                ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Parents ) 
                    , 0 
                    , MkId ( LT . IdentPlusList ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Children ) 
                    , 0 
                    , MkId ( LT . FsFixedChildList ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsFixedRule 
         = FsFixedDefaultRule | FsFixedHorizRule | FsFixedVertRule | FsFixedFillRule . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . FsFixedRule ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . FsFixedDefaultRule ) 
                , 0 
                , MkId ( LT . FsFixedHorizRule ) 
                , 0 
                , MkId ( LT . FsFixedVertRule ) 
                , 0 
                , MkId ( LT . FsFixedFillRule ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* FsRule = FsFixedRule | FsListRule . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . FsRule ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . FsFixedRule ) 
                , 0 
                , MkId ( LT . FsListRule ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* PrecLevelNone | PrecLevelLeft | PrecLevelRight 
         := Operators : CsAtomList . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode 
                ( LT . IdentPlusList 
                , MkId ( LT . PrecLevelNone ) 
                , 0 
                , MkId ( LT . PrecLevelLeft ) 
                , 0 
                , MkId ( LT . PrecLevelRight ) 
                , 0 
                ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Operators ) 
                    , 0 
                    , MkId ( LT . CsAtomList ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* PrecLevel = PrecLevelNone | PrecLevelLeft | PrecLevelRight . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . PrecLevel ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . PrecLevelNone ) 
                , 0 
                , MkId ( LT . PrecLevelLeft ) 
                , 0 
                , MkId ( LT . PrecLevelRight ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* PrecLevels := Elements : { PrecLevel } . *) 
    ; Child 
        ( MkNode 
            ( LT . AsStarRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . PrecLevels ) , 0 ) 
            , 0 
            , MkChildId ( LCT . Elements ) 
            , 3 
            , MkId ( LT . PrecLevel ) 
            , 7 
            ) 
        ) 

    (* PrecRule := Levels : PrecLevels . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . PrecRule ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Levels ) 
                    , 0 
                    , MkId ( LT . PrecLevels ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* CsAtom = Ident | String . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . CsAtom ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . Ident ) 
                , 0 
                , MkId ( LT . String ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* CsAtomList := Elements : { Atom } . *) 
    ; Child 
        ( MkNode 
            ( LT . AsStarRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . CsAtomList ) , 0 ) 
            , 0 
            , MkChildId ( LCT . Elements ) 
            , 3 
            , MkId ( LT . CsAtom ) 
            , 7 
            ) 
        ) 

    (* CsStarRule | CsPlusRule 
         := Lhs : Ident ; ListChild : CsAtom ; Separators : [ CsAtomList ]  . 
    *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode 
                ( LT . IdentPlusList 
                , MkId ( LT . CsStarRule ) 
                , 0 
                , MkId ( LT . CsPlusRule ) 
                , 0 
                ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Lhs ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . ListChild ) 
                    , 0 
                    , MkId ( LT . CsAtom ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsOptChild 
                    , MkChildId ( LCT . Separators ) 
                    , 0 
                    , MkId ( LT . CsAtomList ) 
                    , 4 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* CsListRule = CsStarRule | CsPlusRule . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . CsListRule ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . CsStarRule ) 
                , 0 
                , MkId ( LT . CsPlusRule ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* CsOptChild = Concatenation . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . CsOptChild ) 
            , 0 
            , MkNode ( LT . Alternation , MkId ( LT . Concatenation ) , 0 ) 
            , 3 
            ) 
        ) 

    (* CsReqdChild = CsAtom . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . CsReqdChild ) 
            , 0 
            , MkNode ( LT . Alternation , MkId ( LT . CsAtom ) , 0 ) 
            , 3 
            ) 
        ) 

    (* CsChild = CsOptChild | CsReqdChild . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . CsChild ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . CsOptChild ) 
                , 0 
                , MkId ( LT . CsReqdChild ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* Concatenation := Elements : { CsChild } . *) 
    ; Child 
        ( MkNode 
            ( LT . AsStarRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . Concatenation ) , 0 ) 
            , 0 
            , MkChildId ( LCT . Elements ) 
            , 3 
            , MkId ( LT . CsChild ) 
            , 7 
            ) 
        ) 

    (* CsFixedRule := Lhs : Ident ; Rhs : Concatenation "." . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . CsFixedRule ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Lhs ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Rhs ) 
                    , 0 
                    , MkId ( LT . Concatenation ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* Alternation := Elements : { CsAtom }+ . *) 
    ; Child 
        ( MkNode 
            ( LT . AsPlusRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . Alternation ) , 0 ) 
            , 0 
            , MkChildId ( LCT . Elements ) 
            , 3 
            , MkId ( LT . CsAtom ) 
            , 7 
            ) 
        ) 

    (* CsAltRule  := Lhs : Ident ; Alternatives : Alternation . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . CsAltRule ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Lhs ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Alternatives ) 
                    , 0 
                    , MkId ( LT . Alternation ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* CsRule = CsFixedRule | CsListRule | CsAltRule . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . CsRule ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . CsFixedRule ) 
                , 0 
                , MkId ( LT . CsListRule ) 
                , 0 
                , MkId ( LT . CsAltRule ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* IdentPlusList := Elements : { Ident }+ . *) 
    ; Child 
        ( MkNode 
            ( LT . AsPlusRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . IdentPlusList ) , 0 ) 
            , 0 
            , MkChildId ( LCT . Elements ) 
            , 3 
            , MkId ( LT . Ident ) 
            , 7 
            ) 
        ) 

    (* AsClassRule := ClassName : Ident ; ClassMembers : Alternation . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . AsClassRule ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . ClassName ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . ClassMembers ) 
                    , 0 
                    , MkId ( LT . Alternation ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* AsStarRule | AsPlusRule 
        := Parents 
            : IdentPlusList ; ChildName : [ Ident ] ; ChildClass : Ident . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode 
                ( LT . IdentPlusList 
                , MkId ( LT . AsStarRule ) 
                , 0 
                , MkId ( LT . AsPlusRule ) 
                , 0 
                ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Parents ) 
                    , 0 
                    , MkId ( LT . IdentPlusList ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsOptChild 
                    , MkChildId ( LCT . ChildName ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 4 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . ChildClass ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* AsListRule = AsStarRule | AsPlusRule . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . AsListRule ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . AsStarRule ) 
                , 0 
                , MkId ( LT . AsPlusRule ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 


    (* AsReqdChild | AsOptChild 
         := ChildName : [ Ident ] ; ChildClass : Ident . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode 
                ( LT . IdentPlusList 
                , MkId ( LT . AsReqdChild ) 
                , 0 
                , MkId ( LT . AsOptChild ) 
                , 0 
                ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsOptChild 
                    , MkChildId ( LCT . ChildName ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 4 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . ChildClass ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* AsChild = AsReqdChild | AsOptChild . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . AsChild ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . AsReqdChild ) 
                , 0 
                , MkId ( LT . AsOptChild ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* AsChildList := Elements : { AsChild } . *) 
    ; Child 
        ( MkNode 
            ( LT . AsStarRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . AsChildList ) , 0 ) 
            , 0 
            , MkChildId ( LCT . Elements ) 
            , 3 
            , MkId ( LT . AsChild ) 
            , 7 
            ) 
        ) 

    (* AsFixedRule := Parents : IdentPlusList ; Children : AsChildList . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . AsFixedRule ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Parents ) 
                    , 0 
                    , MkId ( LT . IdentPlusList ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Children ) 
                    , 0 
                    , MkId ( LT . AsChildList ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* AsVarTermRule := Name : Ident . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . AsVarTermRule ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Name ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* AsRule = AsVarTermRule | AsFixedRule | AsListRule | AsClassRule . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . AsRule ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . AsVarTermRule ) 
                , 0 
                , MkId ( LT . AsFixedRule ) 
                , 0 
                , MkId ( LT . AsListRule ) 
                , 0 
                , MkId ( LT . AsClassRule ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* StartRule := StartName : Ident . *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . StartRule ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . StartName ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* Rule = StartRule | CsRule | PrecRule | AsRule | FsRule . *) 
    ; Child 
        ( MkNode 
            ( LT . AsClassRule 
            , MkId ( LT . Rule ) 
            , 0 
            , MkNode 
                ( LT . Alternation 
                , MkId ( LT . StartRule ) 
                , 0 
                , MkId ( LT . CsRule ) 
                , 0 
                , MkId ( LT . PrecRule ) 
                , 0 
                , MkId ( LT . AsRule ) 
                , 0 
                , MkId ( LT . FsRule ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* RuleList := Element : { Rule } . *) 
    ; Child 
        ( MkNode 
            ( LT . AsStarRule 
            , MkNode ( LT . IdentPlusList , MkId ( LT . RuleList ) , 0 ) 
            , 0 
            , MkChildId ( LCT . Elements ) 
            , 3 
            , MkId ( LT . Rule ) 
            , 7 
            ) 
        ) 

    (* Integer VARTERM . *) 
    ; Child ( MkNode ( LT . AsVarTermRule , MkId ( LT . Integer ) , 0 ) ) 

    (* String VARTERM . *) 
    ; Child ( MkNode ( LT . AsVarTermRule , MkId ( LT . String ) , 0 ) ) 

    (* Ident VARTERM . *) 
    ; Child ( MkNode ( LT . AsVarTermRule , MkId ( LT . Ident ) , 0 ) ) 

    (* LanguageDefinition 
         := LanguageName : Ident 
            ; Rules : RuleList 
            ; ClosingName : Ident 
         . 
    *) 
    ; Child 
        ( MkNode 
            ( LT . AsFixedRule 
            , MkNode 
                ( LT . IdentPlusList , MkId ( LT . LanguageDefinition ) , 0 ) 
            , 0 
            , MkNode 
                ( LT . AsChildList 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . LanguageName ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . Rules ) 
                    , 0 
                    , MkId ( LT . RuleList ) 
                    , 3 
                    ) 
                , 0 
                , MkNode 
                    ( LT . AsReqdChild 
                    , MkChildId ( LCT . ClosingName ) 
                    , 0 
                    , MkId ( LT . Ident ) 
                    , 3 
                    ) 
                , 0 
                ) 
            , 3 
            ) 
        ) 

    (* START LanguageDefinition . *) 
    ; Child 
        ( MkNode ( LT . StartRule , MkId ( LT . LanguageDefinition ) , 1 ) ) 

    (* Mnemonic synonyms for insertion token strings.  Needed by scanner. *) 
    ; AddAsTokNameRule ( LT . Colon , ":" ) 
    ; AddAsTokNameRule ( LT . Dot , "." ) 
    ; AddAsTokNameRule ( LT . ColonEquals , ":=" ) 
    ; AddAsTokNameRule ( LT . ColonColonEquals , "::=" ) 
    ; AddAsTokNameRule ( LT . Semicolon , ";" ) 
    ; AddAsTokNameRule ( LT . OpenBracket , "[" ) 
    ; AddAsTokNameRule ( LT . CloseBracket , "]" ) 
    ; AddAsTokNameRule ( LT . OpenBrace , "{" ) 
    ; AddAsTokNameRule ( LT . CloseBrace , "}" ) 
    ; AddAsTokNameRule ( LT . CloseBracePlus , "}+" ) 
    ; AddAsTokNameRule ( LT . Equals , "=" ) 
    ; AddAsTokNameRule ( LT . Stroke , "|" ) 
    ; AddAsTokNameRule ( LT . DoubleStroke , "||" ) 
    ; AddAsTokNameRule ( LT . Arrow , "->" ) 
    ; AddAsTokNameRule ( LT . Underscore , "_" ) 
    ; AddAsTokNameRule ( LT . Bang , "!" ) 
    ; AddAsTokNameRule ( LT . At , "@" ) 
    ; AddAsTokNameRule ( LT . OpenParen , "(" ) 
    ; AddAsTokNameRule ( LT . CloseParen , ")" ) 

    ; AddAsTokNameRule ( LT . RwEND , "END" ) 
    ; AddAsTokNameRule ( LT . RwFILL , "FILL" ) 
    ; AddAsTokNameRule ( LT . RwHORIZ , "HORIZ" ) 
    ; AddAsTokNameRule ( LT . RwNONE , "NONE" ) 
    ; AddAsTokNameRule ( LT . RwNONEMPTY , "NONEMPTY" ) 
    ; AddAsTokNameRule ( LT . RwLEFT , "LEFT" ) 
    ; AddAsTokNameRule ( LT . RwLDL , "LDL" ) 
    ; AddAsTokNameRule ( LT . RwMEMBER , "MEMBER" ) 
    ; AddAsTokNameRule ( LT . RwPLURAL , "PLURAL" ) 
    ; AddAsTokNameRule ( LT . RwPREC , "PREC" ) 
    ; AddAsTokNameRule ( LT . RwPRESENT , "PRESENT" ) 
    ; AddAsTokNameRule ( LT . RwRIGHT , "RIGHT" ) 
    ; AddAsTokNameRule ( LT . RwSTART , "START" ) 
    ; AddAsTokNameRule ( LT . RwVARTERM , "VARTERM" ) 
    ; AddAsTokNameRule ( LT . RwVERT , "VERT" ) 

    ; EstBuild . FinishMerge 
        ( RuleList_MergeState 
        , ResultEstNodeKind := EstHs . EstNodeKindTyp . EstNodeKindPlain 
        , ResultTreeRef := LResult 
        ) 
    ; RETURN LResult 
    END RuleList 

(* VISIBLE: *) 
; PROCEDURE LanguageDefinition 
    ( Lang : LbeStd . LangTyp := LbeStd . LangNull ) : LbeStd . EstRootTyp 
  RAISES { Backout } 

  = BEGIN (* LanguageDefinition *) 
      GLang := Lang 
    ; IF LbeStd . UseAugmentTrees 
      THEN 
        RETURN 
          MkNode 
            ( LbeStd . Tok__Augment 
            , MkNode 
                ( LT . LanguageDefinition 
                , MkChildId ( LCT . Ldl0 ) 
                , 1 
                , RuleList ( ) 
                , 3 
                , MkChildId ( LCT . Ldl0 ) 
                , 6 
                ) 
            , EstHs . FmtNoEstChildOfAugment 
            ) 
      ELSE (* FmtNos are one greater than you might expect because 
              of the added BOI formatter. *) 
        RETURN 
          MkNode 
            ( LT . LanguageDefinition 
            , MkChildId ( LCT . Ldl0 ) 
            , 2 
            , RuleList ( ) 
            , 4 
            , MkChildId ( LCT . Ldl0 ) 
            , 7 
            )
      END (* IF *)  
    END LanguageDefinition 

; BEGIN (* ManualAsTrees *) 
    Ldl0Semantics . Bootstrapping := TRUE 
  END ManualAsTrees 
. 
