
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LineNumbers 

(* Some routines for manipulating line numbers of displayed code. *) 

; IMPORT Wr 
; IMPORT Thread 

; IMPORT PaintHs  
; IMPORT EstHs  
; IMPORT LbeStd 
; IMPORT Strings 
; IMPORT WriteTrv 
; IMPORT Marks 
; IMPORT EstUtil 
; IMPORT TravUtil 
; IMPORT LineMarks
; IMPORT Assertions  

; FROM Failures IMPORT Backout 

; TYPE MarkKindTyp = Marks . MarkKindTyp 

; VAR LCLineCt : LbeStd . LineNoTyp 
(* FIX: LCLineCt should be inside LineCt, but a CG bug makes this
        fail.  *) 

(* VISIBLE: *) 
; PROCEDURE LineCtExport ( ImageRef : PaintHs . ImageTransientTyp ) 
  : LbeStd . LineNoTyp 
  RAISES { Backout , Thread . Alerted }
  (* Count of lines as would be exported to a text file, i.e., with
     proposed but not accepted syntactic corrections removed. *)  

  = PROCEDURE WriteProc 
      ( <* UNUSED *> ImageRef : PaintHs . ImageTransientTyp 
      ; <* UNUSED *> String : Strings . StringTyp 
      )  

    = BEGIN
        INC ( LCLineCt ) 
      END WriteProc 

  ; BEGIN (* LineCt *) 
      LCLineCt := 0 
    (* No need to TextEdit.FlushEdit.  It would not affect the line count. 
       And we don't even necessarily have that module. *) 
    ; WriteTrv . WriteText ( ImageRef , WriteProc , DoGenerateText := TRUE )
    ; RETURN LCLineCt 
    END LineCtExport  

(* VISIBLE: *) 
; PROCEDURE LineCtDisplay 
    ( ImageRef : PaintHs . ImageTransientTyp ) : LbeStd . LineNoTyp 
  RAISES { Backout , Thread . Alerted } 
  (* Count of lines as would appear in a window.  This can differ
     from LineCtExport in case of repairs shown in a window but not in a file. 
  *)  
  (* Compute line count by repeated invocation of LineMarks . GetNextLine *) 

  = VAR LLineCt : LbeStd . LineNoTyp 
  ; VAR LStartMark : Marks . TokMarkTyp 
  ; VAR LNewMark : Marks . TokMarkTyp 
  ; VAR LAtEndOfImage : BOOLEAN 
  ; VAR LDidHitExistingMark : BOOLEAN 
  ; VAR LLineText : Strings . StringTyp 
  ; VAR LBlankLineCt : LbeStd . LineNoTyp 
  ; VAR LTextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
  ; VAR LLineErrArrayRef : PaintHs . LineErrArrayRefTyp 

  ; BEGIN (* LineCtDisplay *) 
      LLineCt := 0 
    ; LineMarks . GetLMBegOfImage 
        ( ImageRef . ItPers . IpLang  
        , ImageRef . ItPers . IpEstRoot  
        , (* VAR *) LStartMark 
        ) 
    ; LOOP 
        LineMarks . GetNextLine 
          ( ImageRef . ItPers . IpLang  
          , ImageRef . ItPers . IpEstRoot  
          , StartMark := LStartMark 
          , ExistingMark := Marks . TokMarkNull 
          , (* VAR *) DidHitExistingMark:= LDidHitExistingMark (* Dead. *) 
          , (* VAR *) NewMark :=LNewMark 
          , (* VAR *) AtEndOfImage := LAtEndOfImage 
          , (* VAR *) LineText := LLineText 
          , (* VAR *) BlankLineCt := LBlankLineCt 
          , (* VAR *) TextAttrArrayRef := LTextAttrArrayRef 
          , (* VAR *) LineErrArrayRef := LLineErrArrayRef
          ) 
      ; IF LAtEndOfImage 
        THEN EXIT 
        ELSE
          INC ( LLineCt , MAX ( 1 , LBlankLineCt )  ) 
        ; LStartMark := LNewMark 
        END (* IF *) 
      END (* LOOP *) 
    ; RETURN LLineCt 
    END LineCtDisplay   

(* VISIBLE: *) 
; PROCEDURE WriteDisplay 
    ( ImageRef : PaintHs . ImageTransientTyp ; WrT : Wr . T ) 
  RAISES { Backout , Thread . Alerted } 
  (* Write to WrT, by repeated invocation of LineMarks . GetNextLine *) 

  = <* FATAL Wr . Failure *> 
    VAR LStartMark : Marks . TokMarkTyp 
  ; VAR LNewMark : Marks . TokMarkTyp 
  ; VAR LLineText : Strings . StringTyp 
  ; VAR LTextAttrArrayRef : PaintHs . TextAttrArrayRefTyp 
  ; VAR LLineErrArrayRef : PaintHs . LineErrArrayRefTyp 
  ; VAR LBlankLineCt : LbeStd . LineNoTyp 
  ; VAR LLineNo : LbeStd . LineNoTyp 
  ; VAR LAtEndOfImage : BOOLEAN 
  ; VAR LDidHitExistingMark : BOOLEAN 

  ; BEGIN (* WriteDisplay *) 
      LLineNo := 1 
    ; LineMarks . GetLMBegOfImage 
        ( ImageRef . ItPers . IpLang  
        , ImageRef . ItPers . IpEstRoot  
        , (* VAR *) LStartMark 
        ) 
    ; LOOP 
        LineMarks . GetNextLine 
          ( ImageRef . ItPers . IpLang  
          , ImageRef . ItPers . IpEstRoot  
          , StartMark := LStartMark 
          , ExistingMark := Marks . TokMarkNull 
          , (* VAR *) DidHitExistingMark:= LDidHitExistingMark (* Dead. *) 
          , (* VAR *) NewMark :=LNewMark 
          , (* VAR *) AtEndOfImage := LAtEndOfImage 
          , (* VAR *) LineText := LLineText 
          , (* VAR *) BlankLineCt := LBlankLineCt 
          , (* VAR *) TextAttrArrayRef := LTextAttrArrayRef 
          , (* VAR *) LineErrArrayRef := LLineErrArrayRef
          ) 
      ; IF LAtEndOfImage 
        THEN EXIT 
        ELSE
          FOR RI := 1 TO MAX ( 1 , LBlankLineCt ) 
          DO 
            Wr . PutText ( WrT , Strings . ToText ( LLineText ) ) 
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; INC ( LLineNo ) 
          END (* FOR *) 
        ; LStartMark := LNewMark 
        END (* IF *) 
      END (* LOOP *) 
    END WriteDisplay   

(* VISIBLE: *) 
; PROCEDURE EstimateOfLinesInSubtree 
    ( READONLY WidthInfo : EstHs . WidthInfoTyp 
    ; LineBreakCt : LbeStd . LineNoSignedTyp 
    ) 
  : LbeStd . LineNoSignedTyp  
  RAISES { Backout } 
  (* Actually, of the number of new lines at the top level. *) 

  = CONST AssumedStartPos = 12  

  ; VAR LResult : LbeStd . LineNoSignedTyp  

  ; BEGIN 
      IF EstUtil . CharPosPlusWidthInfo ( AssumedStartPos , WidthInfo ) 
         = LbeStd . LimitedCharNoInfinity 
      THEN
        LResult := LineBreakCt 
      ELSE 
        LResult := 0  
      END (* IF *) 
 (* ; INC ( LResult , ORD ( WidthInfo . WiHasNlBefore ) ) 
    ; INC ( LResult , ORD ( WidthInfo . WiHasNlAfter ) ) 
 *) 
    ; RETURN LResult 
    END EstimateOfLinesInSubtree 

(* VISIBLE: *) 
; PROCEDURE EstimateLineNo
    ( LineMark : PaintHs . LineMarkMeatTyp 
    ; ImageRef : PaintHs . ImageTransientTyp 
    ; VAR LineNo : LbeStd . LineNoTyp 
    ; VAR IsExact : BOOLEAN 
    ) 
  RAISES { Backout } 

  = VAR LNodeNo : LbeStd . EstNodeNoTyp 

  ; BEGIN 
      CASE LineMark . LmTokMark . TkmKind <* NOWARN *>
      OF MarkKindTyp . Null 
      => LNodeNo := 0 

      | MarkKindTyp . Plain 
      => LNodeNo 
           := LineMark . LmTokMark . TkmEstNodeNo 
              + 4 * ORD ( LineMark . LmTokMark . TkmStartAtEnd )  
                (* Estimate 4 nodes per line, average. *) 

      | MarkKindTyp . BlankLine 
      => LNodeNo 
           := LineMark . LmTokMark . TkmEstNodeNo 
              + LineMark . LmLineNo  
              + 4 * ORD ( LineMark . LmTokMark . TkmStartAtEnd )  
                (* Estimate 4 nodes per line, average. *) 

      | MarkKindTyp . LeftSibFmtNo 
      => LNodeNo := LineMark . LmTokMark . TkmEstNodeNo 

      | MarkKindTyp . RightSibFmtNo 
      => LNodeNo 
          := LineMark . LmTokMark . TkmEstNodeNo 
             + TravUtil . NodeCtOfDescendantWithNodeNo 
                 ( ImageRef . ItPers . IpEstRoot 
                 , LineMark . LmTokMark . TkmEstNodeNo 
                 ) 

      | MarkKindTyp . ChildFmtNo  
      => LNodeNo := LineMark . LmTokMark . TkmEstNodeNo 
      END (* CASE *) 
    ; LineNo 
        := ( ImageRef . ItPers . IpLineCtDisplay * LNodeNo ) 
           DIV EstUtil . EstNodeCt ( ImageRef . ItPers . IpEstRoot )   
    ; IsExact := LNodeNo = 0 
    END EstimateLineNo 

; BEGIN 
  END LineNumbers 
. 
