
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE TreeBrowse 

(* An interactive, command-line browser for an Est. *) 

; IMPORT Lex 
; IMPORT Rd 
; IMPORT Stdio 
; IMPORT Text 
; IMPORT TextRd 
; IMPORT Thread 
; IMPORT Wr 

; FROM Assertions IMPORT AssertionFailure 
; IMPORT AstView 
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LbeStd 
; IMPORT Misc 
; IMPORT PortTypes 

(* REVIEW: Is there some confusion here over whether this as an Ast-only
           browser or a full Est browser?  It seems to behave as Est,
           showing mods, etc.  But it has some uses of AstView, which is
           for Ast-only access.
*) 

; REVEAL T 
    = Public 
        BRANDED 
          Brand 
          OBJECT 
            WrT : Wr . T 
          ; RdT : Rd . T 
          ; Prompt : TEXT 
          ; RootRef : AstView . AstRefTyp (* Of the entire Est. *) 
          ; CurrentRef : AstView . AstRefTyp (* Current Est node. *) 
          ; ParentRef : AstView . AstRefTyp (* Of CurrentRef. *) 
          ; ChildNoOfParent : LbeStd . EstChildNoTyp 
          ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
          ; IsReady : BOOLEAN := FALSE 
          OVERRIDES 
            initStdSession := InitStdSession 
          ; initSessionRdWr := InitSessionRdWr 
          END (* OBJECT *) 

; TYPE CommandTyp 
    = { Invalid , Help , Quit , ToRoot , ToNode , ToRelNode , ToParent 
      , ToEldestChild , ToYoungestChild , ToChild , ToElderSib , ToYoungerSib 
      , Redisplay 
      } 

; PROCEDURE DisplayHelp ( Session : SessionTyp ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* DisplayHelp *) 
      Wr . PutText ( Session . WrT , "'h' Help." & Wr . EOL ) 
    ; Wr . PutText ( Session . WrT , "'q' Quit." & Wr . EOL ) 
    ; Wr . PutText ( Session . WrT , "'r' To Root." & Wr . EOL ) 
    ; Wr . PutText 
        ( Session . WrT , "'N' <NodeNo> To Absolute Node." & Wr . EOL ) 
    ; Wr . PutText 
        ( Session . WrT , "'n' <NodeNo> To Relative Node." & Wr . EOL ) 
    ; Wr . PutText ( Session . WrT , "'p' To Parent." & Wr . EOL ) 
    ; Wr . PutText ( Session . WrT , "'E' To Eldest Child." & Wr . EOL ) 
    ; Wr . PutText ( Session . WrT , "'Y' To Youngest Child." & Wr . EOL ) 
    ; Wr . PutText ( Session . WrT , "'C' <ChildNo> To Child." & Wr . EOL ) 
    ; Wr . PutText ( Session . WrT , "'e' To Elder Sib." & Wr . EOL ) 
    ; Wr . PutText ( Session . WrT , "'y' To Younger Sib." & Wr . EOL ) 
    ; Wr . PutText ( Session . WrT , "'c' Redisplay current node." & Wr . EOL ) 
    ; Wr . PutText ( Session . WrT , "<CR> Repeat previous command." & Wr . EOL ) 
    ; Wr . Flush ( Session . WrT ) 
    END DisplayHelp 

; PROCEDURE ParseCommand 
    ( Line : TEXT ; VAR Command : CommandTyp ; VAR RemainingText : TEXT ) 

  = VAR LLength : PortTypes . Int32Typ 

  ; BEGIN (* ParseCommand *) 
      LLength := Text . Length ( Line ) 
    ; RemainingText := Line 
    ; IF LLength > 0 
      THEN 
        RemainingText := Text . Sub ( Line , 1 ) 
      ; CASE Text . GetChar ( Line , 0 ) 
        OF 'h' 
        => Command := CommandTyp . Help 
        | 'q' 
        => Command := CommandTyp . Quit 
        | 'r' 
        => Command := CommandTyp . ToRoot 
        | 'N' 
        => Command := CommandTyp . ToNode 
        | 'n' 
        => Command := CommandTyp . ToRelNode 
        | 'p' 
        => Command := CommandTyp . ToParent 
        | 'E' 
        => Command := CommandTyp . ToEldestChild 
        | 'Y' 
        => Command := CommandTyp . ToYoungestChild 
        | 'C' 
        => Command := CommandTyp . ToChild 
        | 'e' 
        => Command := CommandTyp . ToElderSib 
        | 'y' 
        => Command := CommandTyp . ToYoungerSib 
        | 'c' 
        => Command := CommandTyp . Redisplay  
        ELSE 
          Command := CommandTyp . Invalid 
        END (* CASE *) 
      ELSE 
        Command := CommandTyp . Invalid 
      END (* IF *) 
    END ParseCommand 

; PROCEDURE Display ( Session : SessionTyp ) 
  RAISES { AssertionFailure } 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LEstRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildLeafElem : EstHs . LeafElemTyp 
  ; VAR LText : TEXT 

  ; BEGIN (* Display *) 
      IF Session . CurrentRef . NodeNo = 0 
      THEN 
        LChildLeafElem := EstHs . LeafElemNull 
      ELSE 
        EstUtil . GetIthChild 
          ( Session . ParentRef . NodeRef  
          , Session . ParentRef . ChildNo   
          , (* VAR *) ResultChildRelNodeNo := LEstRelNodeNo (* Dead. *) 
          , (* VAR *) ResultLeafElem := LChildLeafElem 
          ) 
(* TODO^ Maybe someday.  This is unnecessarily inefficient.  We could cache this
         info for the current node in the session, when other stuff that is
         already there is cached. *) 
      END (* IF *) 
    ; LText 
        := "NodeNo " 
           & LbeStd . EstNodeNoImage ( Session . CurrentRef . NodeNo ) 
           & " Addr " 
           & Misc . RefanyImage ( Session . CurrentRef . NodeRef ) 
           & " ChildNo = " 
           & LbeStd . EstChildNoImage ( Session . ParentRef . ChildNo ) 
           & " FmtNo = " & EstHs . FmtNoImage ( LChildLeafElem . LeFmtNo ) 
           & " KindSet = " 
    ; Wr . PutText 
        ( Session . WrT 
        , LText 
          & EstHs . EstChildKindSetImage 
              ( LChildLeafElem . LeKindSet 
              , Text . Length ( LText ) 
              , Mnemonic := FALSE 
              , RightMargin := 110 
              )  
          & Wr . EOL 
        ) 
    ; Wr . PutText 
        ( Session . WrT 
        , EstUtil . EstNodeImage 
            ( Session . CurrentRef . NodeRef 
            , Indent := 0 
            , NodeNo := Session . CurrentRef . NodeNo 
            , ChildNo := 0 
            , Lang := Session . Lang 
            , Mnemonic := TRUE 
            ) 
          & Wr . EOL 
        ) 
    END Display 

; PROCEDURE GetInt ( String : TEXT ) : PortTypes . Int32Typ 

  = VAR LResult : PortTypes . Int32Typ 

  ; BEGIN (* GetInt *) 
      TRY 
        LResult := Lex . Int ( TextRd . New ( String ) ) 
      EXCEPT 

      ELSE 
        LResult := FIRST ( PortTypes . Int32Typ ) 
      END (* EXCEPT *) 
    ; RETURN LResult 
    END GetInt 

; CONST InvalidCommandMessage = "Invalid command" 
; CONST InvalidNodeNoMessage = "Invalid node number" 
; CONST NoChildMessage = "No such child" 
; CONST NoElderSibMessage = "No elder sib" 
; CONST NoYoungerSibMessage = "No younger sib" 
; CONST NoParentMessage = "No parent" 

; PROCEDURE ToRoot 
    ( Session : SessionTyp ; <* UNUSED *> RemainingLine : TEXT ) 
  RAISES { AssertionFailure } 

  = BEGIN (* ToRoot *) 
      Session . CurrentRef := Session . RootRef 
    ; Session . ParentRef := AstView . AstRefNull 
    ; Display ( Session ) 
    END ToRoot 

; PROCEDURE ToAbsNodeNo 
    ( Session : SessionTyp ; NodeNo : LbeStd . EstNodeNoTyp ) 
  RAISES { AssertionFailure } 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildLeafElem : EstHs . LeafElemTyp 
  ; VAR LParentNodeRef : EstHs . EstRefTyp 

  ; BEGIN (* ToAbsNodeNo *) 
      IF NodeNo < 0 
         OR NodeNo >= EstUtil . EstNodeCt ( Session . RootRef . NodeRef ) 
      THEN 
        Wr . PutText ( Session . WrT , InvalidNodeNoMessage ) 
      ; Wr . PutText ( Session . WrT , Wr . EOL ) 
      ELSE 
        Session . CurrentRef := AstView . AstRef ( Session . RootRef , NodeNo ) 
      ; Session . ParentRef := AstView . AstRefNull 
      ; IF NodeNo # 0 
        THEN 
          EstUtil . GetParent 
            ( RootRef := Session . RootRef . NodeRef 
            , NodeNo := Session . CurrentRef . NodeNo 
            , (* VAR *) ResultNodeNo := Session . ParentRef . NodeNo 
            , (* VAR *) ResultNodeRef := LParentNodeRef 
            ) 
        ; Session . ParentRef . NodeRef := LParentNodeRef (* WIDEN. *)  
        ; LRelNodeNo := NodeNo - Session . ParentRef . NodeNo
        ; EstUtil . GetEstChildContainingRelNodeNo 
            ( Session . ParentRef . NodeRef 
            , LRelNodeNo
            , (* VAR *) Session . ParentRef . ChildNo 
            , (* VAR *) LChildRelNodeNo (* Ignore *) 
            , (* VAR *) LChildLeafElem (* Ignore *) 
            ) 
        END (* IF *) 
      ; Display ( Session ) 
      END (* IF *) 
    END ToAbsNodeNo 

; PROCEDURE ToAbsNode ( Session : SessionTyp ; RemainingLine : TEXT ) 
  RAISES { AssertionFailure } 

  = VAR LNodeNo : LbeStd . EstNodeNoTyp 

  ; BEGIN (* ToNode *) 
      LNodeNo := GetInt ( RemainingLine ) 
    ; ToAbsNodeNo ( Session , LNodeNo ) 
    END ToAbsNode 

; PROCEDURE ToRelNode ( Session : SessionTyp ; RemainingLine : TEXT ) 
  RAISES { AssertionFailure } 

  = VAR LRelNodeNo : LbeStd . EstNodeNoTyp 

  ; BEGIN (* ToRelNode  ToRelativeNode *) 
      LRelNodeNo := GetInt ( RemainingLine ) 
    ; ToAbsNodeNo ( Session , Session . CurrentRef . NodeNo + LRelNodeNo ) 
    END ToRelNode 

; PROCEDURE ToParent 
    ( Session : SessionTyp ; <* UNUSED *> RemainingLine : TEXT ) 
  RAISES { AssertionFailure } 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LParentRef : EstHs . EstRefTyp 
  ; VAR LRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildRelNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LChildLeafElem : EstHs . LeafElemTyp 

  ; BEGIN (* ToParent *) 
      EstUtil . GetParent 
        ( RootRef := Session . RootRef . NodeRef 
        , NodeNo := Session . CurrentRef . NodeNo 
        , ResultNodeNo := LNodeNo 
        , ResultNodeRef := LParentRef 
        ) 
    ; IF LNodeNo = LbeStd . EstNodeNoNull 
      THEN 
        Wr . PutText ( Session . WrT , NoParentMessage ) 
      ; Wr . PutText ( Session . WrT , Wr . EOL ) 
      ELSE 
        Session . CurrentRef . NodeRef := LParentRef 
      ; Session . CurrentRef . NodeNo := LNodeNo 
      ; Session . CurrentRef . ChildNo := LbeStd . EstChildNoNull 
      ; IF LNodeNo = 0 
        THEN 
          Session . ParentRef := AstView . AstRefNull 
        ELSE 
          EstUtil . GetParent 
            ( RootRef := Session . RootRef . NodeRef 
            , NodeNo := Session . CurrentRef . NodeNo 
            , ResultNodeNo := Session . ParentRef . NodeNo 
            , ResultNodeRef := LParentRef  
            ) 
        ; Session . ParentRef . NodeRef := LParentRef  
        ; LRelNodeNo := Session . CurrentRef . NodeNo - Session . ParentRef . NodeNo
        ; EstUtil . GetEstChildContainingRelNodeNo 
            ( Session . ParentRef . NodeRef 
            , LRelNodeNo 
            , (* VAR *) Session . ParentRef . ChildNo 
            , (* VAR *) LChildRelNodeNo (* Ignore *) 
            , (* VAR *) LChildLeafElem (* Ignore *) 
            ) 
        END (* IF *) 
      ; Display ( Session ) 
      END (* IF *) 
    END ToParent 

; PROCEDURE ToIthChild 
    ( Session : SessionTyp 
    ; Parent : AstView . AstRefTyp 
    ; ChildNo : LbeStd . EstChildNoTyp 
    ) 
  RAISES { AssertionFailure } 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LChildCt : LbeStd . EstChildNoTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LLeafElem : EstHs . LeafElemTyp 

  ; BEGIN (* ToIthChild *) 
      LChildCt := EstUtil . EstChildCt ( Parent . NodeRef ) 
    ; IF ChildNo < 0 OR ChildNo >= LChildCt 
      THEN 
        Wr . PutText ( Session . WrT , NoChildMessage ) 
      ; Wr . PutText ( Session . WrT , Wr . EOL ) 
      ELSE 
        EstUtil . GetIthChild 
          ( EstRef := Parent . NodeRef 
          , I := ChildNo 
          , (* VAR *) ResultChildRelNodeNo := LNodeNo 
          , (* VAR *) ResultLeafElem := LLeafElem 
          ) 
      ; Session . ParentRef := Parent  
      ; Session . CurrentRef . NodeRef := LLeafElem . LeChildRef 
      ; Session . CurrentRef . NodeNo 
          := Parent . NodeNo 
             + LNodeNo 
             + ORD ( EstHs . EstChildKindOptSingletonList 
                     IN LLeafElem . LeKindSet 
                   ) 
               (* Just don't show an optimized-away singleton list node.  
                  NodeRef already points to its one element.  Make NodeNo
                  agree.  User can tell by actual node number displayed. 
               *) 
(* TODO: Something nicer about this case.  Maybe have a way to actually 
         appear to stop on the absent list node. 
*) 
      ; Session . ParentRef . ChildNo := ChildNo 
      ; Session . CurrentRef . ChildNo := LbeStd . EstChildNoNull 
      ; Display ( Session ) 
      END (* IF *) 
    END ToIthChild 

; PROCEDURE ToEldestChild 
    ( Session : SessionTyp ; <* UNUSED *> RemainingLine : TEXT ) 
  RAISES { AssertionFailure } 

  = BEGIN (* ToEldestChild *) 
      ToIthChild ( Session , Session . CurrentRef , 0 ) 
    END ToEldestChild 

; PROCEDURE ToYoungestChild 
    ( Session : SessionTyp ; <* UNUSED *> RemainingLine : TEXT ) 
  RAISES { AssertionFailure } 

  = VAR LChildCt : LbeStd . EstChildNoTyp 

  ; BEGIN (* ToYoungestChild *) 
      LChildCt := EstUtil . EstChildCt ( Session . CurrentRef . NodeRef ) 
    ; ToIthChild ( Session , Session . CurrentRef , LChildCt - 1 ) 
    END ToYoungestChild 

; PROCEDURE ToChild 
    ( Session : SessionTyp ; RemainingLine : TEXT ) 
  RAISES { AssertionFailure } 

  = BEGIN (* ToChild *) 
      ToIthChild ( Session , Session . CurrentRef , GetInt ( RemainingLine ) ) 
    END ToChild 

; PROCEDURE ToElderSib 
    ( Session : SessionTyp ; <* UNUSED *> RemainingLine : TEXT ) 
  RAISES { AssertionFailure } 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* ToElderSib *) 
      IF Session . ParentRef . NodeRef = Session . RootRef . NodeRef 
      THEN 
        Wr . PutText ( Session . WrT , NoElderSibMessage ) 
      ; Wr . PutText ( Session . WrT , Wr . EOL ) 
      ELSIF Session . ParentRef . ChildNo # LbeStd . EstChildNoNull 
      THEN 
        IF Session . ParentRef . ChildNo <= 0 
        THEN 
          Wr . PutText ( Session . WrT , NoElderSibMessage ) 
        ; Wr . PutText ( Session . WrT , Wr . EOL ) 
        ELSE 
          ToIthChild 
            ( Session , Session . ParentRef , Session . ParentRef . ChildNo - 1 ) 
        END (* IF *) 
      END (* IF *) 
    END ToElderSib 

; PROCEDURE ToYoungerSib 
    ( Session : SessionTyp ; <* UNUSED *> RemainingLine : TEXT ) 
  RAISES { AssertionFailure } 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN (* ToYoungerSib *) 
      IF Session . ParentRef . NodeRef = Session . RootRef . NodeRef 
      THEN 
        Wr . PutText ( Session . WrT , NoYoungerSibMessage ) 
      ; Wr . PutText ( Session . WrT , Wr . EOL ) 
      ELSIF Session . ParentRef . ChildNo # LbeStd . EstChildNoNull 
      THEN 
        IF Session . ParentRef . ChildNo 
           >= EstUtil . EstChildCt ( Session . ParentRef . NodeRef ) - 1 
        THEN 
          Wr . PutText ( Session . WrT , NoYoungerSibMessage ) 
        ; Wr . PutText ( Session . WrT , Wr . EOL ) 
        ELSE 
          ToIthChild 
            ( Session , Session . ParentRef , Session . ParentRef . ChildNo + 1 ) 
        END (* IF *) 
      END (* IF *) 
    END ToYoungerSib 

(* VISIBLE: *) 
; PROCEDURE InitStdSession 
    ( Session : SessionTyp 
    ; RootEstRef : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : SessionTyp 

  = BEGIN (* InitStdSession *) 
      Session . WrT := Stdio . stdout 
    ; Session . RdT := Stdio . stdin 
    ; Session . Prompt := ">" 
    ; Session . RootRef . NodeRef := RootEstRef 
    ; Session . RootRef . NodeNo := 0 
    ; Session . RootRef . ChildNo := 0 
    ; Session . CurrentRef := Session . RootRef 
    ; Session . Lang := Lang 
    ; Session . IsReady := TRUE 
    ; RETURN Session 
    END InitStdSession 

(* VISIBLE: *) 
; PROCEDURE InitSessionRdWr 
    ( Session : SessionTyp 
    ; RootEstRef : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ; RdT : Rd . T 
    ; WrT : Wr . T 
    ) 
    : SessionTyp 

  = BEGIN (* InitSessionRdWr *) 
      Session . WrT := WrT 
    ; Session . RdT := RdT 
    ; Session . Prompt := ">" 
    ; Session . RootRef . NodeRef := RootEstRef 
    ; Session . RootRef . NodeNo := 0 
    ; Session . RootRef . ChildNo := 0 
    ; Session . CurrentRef := Session . RootRef 
    ; Session . Lang := Lang 
    ; Session . IsReady := TRUE 
    ; RETURN Session 
    END InitSessionRdWr  

(* VISIBLE: *) 
; PROCEDURE Browse 
    ( EstRoot : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ; TreeId : TEXT := "" 
    ) 
  RAISES { AssertionFailure } 
  (* A complete browse, using Stdio. *) 

(* TODO: Calling this from m3gdb with EstRoot = NIL once segfaulted m3gdb. *) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LSession 
      := NEW ( T ) . initStdSession ( EstRoot , Lang ) 

  ; BEGIN (* Browse *) 
      Wr . PutText ( Stdio . stderr , "Browsing Est:" & TreeId & Wr . EOL ) 
    ; Interp ( LSession ) 
    END Browse 

(* VISIBLE: *) 
; PROCEDURE Interp ( Session : SessionTyp ) 
  RAISES { AssertionFailure } 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LLine : TEXT 
  ; VAR LRemainingLine : TEXT 
  ; VAR LPrevRemainingLine : TEXT 
  ; VAR LCommand : CommandTyp := CommandTyp . Invalid  
  ; VAR LPrevCommand : CommandTyp := CommandTyp . Invalid  

  ; BEGIN (* Interp *) 
      LOOP 
        IF Session . IsReady 
        THEN 
          TRY 
            Wr . PutText ( Session . WrT , Session . Prompt ) 
       (* ; Wr . PutText ( Session . WrT , Wr . EOL ) *) 
          ; Wr . Flush ( Session . WrT ) 
          EXCEPT Thread . Alerted , Wr . Failure 
            => Session . IsReady := FALSE 
            ; EXIT 
          END (* EXCEPT *) 
        ; TRY 
            LLine := Rd . GetLine ( Session . RdT ) 
          EXCEPT Thread . Alerted , Rd . Failure , Rd . EndOfFile 
            => Session . IsReady := FALSE 
            ; EXIT 
          END (* EXCEPT *) 
        ; IF Text . Equal ( LLine , "" ) 
          THEN
            LCommand := LPrevCommand 
          ; LRemainingLine := LPrevRemainingLine 
          ELSE 
            ParseCommand ( LLine , LCommand , LRemainingLine ) 
          END (* IF *) 
        ; CASE LCommand 
          OF CommandTyp . Invalid 
          => Wr . PutText ( Session . WrT , InvalidCommandMessage ) 
          ; Wr . PutText ( Session . WrT , Wr . EOL ) 
          | CommandTyp . Help 
          => DisplayHelp ( Session ) 
          | CommandTyp . Quit 
          => EXIT 
          | CommandTyp . ToRoot 
          => ToRoot ( Session , LRemainingLine ) 
          | CommandTyp . ToNode 
          => ToAbsNode ( Session , LRemainingLine ) 
          | CommandTyp . ToRelNode 
          => ToRelNode ( Session , LRemainingLine ) 
          | CommandTyp . ToParent 
          => ToParent ( Session , LRemainingLine ) 
          | CommandTyp . ToEldestChild 
          => ToEldestChild ( Session , LRemainingLine ) 
          | CommandTyp . ToYoungestChild 
          => ToYoungestChild ( Session , LRemainingLine ) 
          | CommandTyp . ToChild 
          => ToChild ( Session , LRemainingLine ) 
          | CommandTyp . ToElderSib 
          => ToElderSib ( Session , LRemainingLine ) 
          | CommandTyp . ToYoungerSib 
          => ToYoungerSib ( Session , LRemainingLine ) 
          | CommandTyp . Redisplay  
          => Display ( Session ) 
          END (* CASE *) 
        ; IF LCommand # CommandTyp . Invalid 
          THEN LPrevCommand := LCommand 
          ; LPrevRemainingLine := LRemainingLine 
          END 
        END (* IF *) 
      END (* LOOP *) 
    END Interp 

; BEGIN (* TreeBrowse *) 
  END TreeBrowse 
. 
