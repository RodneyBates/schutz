
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE GenConstEst 

(* From an in-memory Est, Generate Modula-3 code for a module that will build 
   a fresh copy of the Est. 
*) 

; IMPORT Fmt 
; IMPORT Wr 
; IMPORT Pathname 
; IMPORT Date 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Time 
; IMPORT Stdio

; IMPORT EstHs 
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT Ldl0Tok 
; IMPORT LdlSemantics 
; IMPORT MessageCodes 
; IMPORT Misc 
; IMPORT ModHs 
; IMPORT PortTypes 
; IMPORT SharedStrings 
; IMPORT TravUtil  
; IMPORT VersionedFiles 

; FROM Assertions IMPORT CantHappen 
; FROM Failures IMPORT Backout  

; TYPE AFT = MessageCodes . T 

; TYPE IndentTyp = PortTypes . Int32Typ 

; <* UNUSED *> PROCEDURE FmtKindImage ( K : LangUtil . FmtKindTyp ) : TEXT 

  = BEGIN 
      RETURN "LangUtil.FmtKindTyp . " & LangUtil.FmtKindImage ( K ) 
    END FmtKindImage 

(* EXPORTED: *) 
; PROCEDURE WriteStream 
    ( Lang : LbeStd . LangTyp 
    ; EstRoot : LbeStd . EstRootTyp 
    ; LangInfoRef : LdlSemantics . LangInfoRefTyp 
    ; UseLdlTok : BOOLEAN 
    ; ModuleName : TEXT := "Ldl0MakeEst" 
    ; WrT : Wr . T 
      (* Must be open.  Not closed. *) 
    )
  RAISES { Backout }

  = PROCEDURE W ( TextValue : TEXT ) 
    (* Easy to call: write to current output stream. *) 

    = <* FATAL Wr . Failure *> 
      <* FATAL Thread . Alerted *> 
      BEGIN 
        Wr . PutText ( WrT , TextValue ) 
      END W 

  ; PROCEDURE WL ( TextValue : TEXT ) 
    (* Easy to call: write to current output stream, with Nl after. *) 

    = <* FATAL Wr . Failure *> 
      <* FATAL Thread . Alerted *> 
      BEGIN 
        Wr . PutText ( WrT , TextValue ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
      END WL 

  ; PROCEDURE B ( Indent : IndentTyp ) : TEXT 
    (* Easy to call: Return new line + blank padding *) 

    = BEGIN 
        RETURN Wr . EOL & Misc . Blanks ( Indent )  
      END B 

  ; PROCEDURE TokImageM3Code 
      ( T : LbeStd . TokTyp 
      ; UseLdlTok : BOOLEAN 
      ; Qualified : BOOLEAN 
      ) 
    : TEXT 
    (* Token image in a format compilable as Modula-3 code. *) 

    = VAR LTextForTok : TEXT 

    ; BEGIN 
        IF FIRST ( LbeStd . StdTokTyp ) <= T  
           AND T <= LAST ( LbeStd . StdTokTyp ) 
        THEN
          IF Qualified 
          THEN  
            RETURN "LbeStd . " & LbeStd . StdTokImage ( T ) 
          ELSE 
            RETURN LbeStd . StdTokImage ( T ) 
          END (* IF *) 
        ELSIF UseLdlTok 
        THEN 
          IF Qualified 
          THEN 
            RETURN "LdlTok . " & Ldl0Tok . ToText ( T ) 
          ELSE
            RETURN Ldl0Tok . ToText ( T ) 
          END (* IF *) 
        ELSE 
          LTextForTok := LangUtil . TextForTok ( Lang , T ) 
        ; IF LTextForTok = NIL OR Text . Equal ( LTextForTok , "" ) 
          THEN
            RETURN LbeStd . NumTokImage ( T ) 
          ELSE 
            RETURN 
              LbeStd . NumTokImage ( T ) 
              & "(* " & LTextForTok & " *)"
          END 
        END (* IF *) 
      END TokImageM3Code 

  ; PROCEDURE SharedString 
      ( S : SharedStrings . T ; Indent : IndentTyp := 0 ) 
    : TEXT 

    = BEGIN 
        RETURN 
          "SharedStrings . FromText" 
          & B ( Indent + 2 ) 
          & "( \"" & Misc . EscapeText ( SharedStrings . ToText ( S ) ) & "\""
          & B ( Indent + 2 ) & ", Tok := " 
          & TokImageM3Code 
              ( SharedStrings . Tok ( S ) , UseLdlTok , Qualified := TRUE ) 
          & B ( Indent + 2 ) & ") " 
      END SharedString 

  ; PROCEDURE ModBlankLine 
      ( M : ModHs . ModBlankLineTyp ; Indent : IndentTyp := 0 ) 

    = BEGIN 
        W ( "NEW ( ModHs . ModBlankLineTyp " 
            & B ( Indent + 4 ) & ", ModBlankLineCt := "
            & LbeStd . LineNoImage ( M . ModBlankLineCt ) 
            & B ( Indent + 4 ) & ") " 
          )   
      END ModBlankLine 

  ; PROCEDURE ModLexErr ( M : ModHs . ModLexErrTyp ; Indent : IndentTyp := 0 ) 

    = BEGIN 
        W ( "NEW ( ModHs . ModLexErrTyp " 
            & B ( Indent + 4 ) & ", ModLexErrCode := "
            & LbeStd . ErrCodeImage ( M . ModLexErrCode ) 
            & B ( Indent + 4 ) & ", ModLexErrPos := " 
            & LbeStd . LimitedCharNoImage ( M . ModLexErrPos ) 
            & B ( Indent + 4 ) & ") " 
          )   
      END ModLexErr 

  ; PROCEDURE ModSyntErr ( M : ModHs . ModSyntErrTyp ; Indent : IndentTyp := 0 ) 

    = BEGIN 
        W ( "NEW ( ModHs . ModSyntErrTyp " 
            & B ( Indent + 4 ) & ", ModSyntErrCode := "
            & LbeStd . ErrCodeImage ( M . ModSyntErrCode ) 
            & B ( Indent + 4 ) & ") " 
          )   
      END ModSyntErr 

  ; PROCEDURE ModDel 
      ( M : ModHs . ModDelTyp ; <* UNUSED *> Indent : IndentTyp := 0 ) 

    = BEGIN 
        W ( "NEW ModHs.ModDelTyp ( ModDelThruFmtNo := "
            & EstHs . FmtNoImage ( M . ModDelThruFmtNo , Pad := 0 ) 
            & " , ModDelIsRepair := " & Fmt . Bool ( M . ModDelIsRepair ) 
            & " )" 
          )   
      END ModDel 

  ; PROCEDURE ModCmnt 
      ( TypeName : TEXT ; C : ModHs . ModCmntTyp ; Indent : IndentTyp := 0 ) 

    = BEGIN
        W ( "NEW ( " & TypeName  
            & B ( Indent + 4 ) & ", ModCmntBegScanState := " 
            & "LbeStd . " & LbeStd . ScanStateImage ( C . ModCmntBegScanState ) 
            & B ( Indent + 4 ) & ", ModCmntEndScanState := " 
            & "LbeStd . " & LbeStd . ScanStateImage ( C . ModCmntEndScanState ) 
            & B ( Indent + 4 ) & ", ModCmntFromPos := " 
            & LbeStd . LimitedCharNoImage ( C . ModCmntFromPos ) 
            & B ( Indent + 4 ) & ", ModCmntNlBefore := " 
            & Fmt . Bool ( C . ModCmntNlBefore )
            & B ( Indent + 4 ) & ", ModCmntNlAfter := " 
            & Fmt . Bool ( C . ModCmntNlAfter )
            & B ( Indent + 4 ) & ", ModCmntStringRef " 
            & B ( Indent + 8 ) & ":=  "
            & SharedString ( C . ModCmntStringRef , Indent + 9 )
            & B ( Indent + 4 ) & ") "
          ) 
      END ModCmnt 

  ; PROCEDURE ModText ( T : ModHs . ModTextTyp ; Indent : IndentTyp := 0 ) 

    = BEGIN
        W ( "NEW ( ModHs . ModTextTyp " 
            & ", ModTextLeftTokToPos := "
            & LbeStd . LimitedCharNoImage ( T . ModTextLeftTokToPos )  
            & B ( Indent + 4 ) & ", ModTextFromPos := "
            & LbeStd . LimitedCharNoImage ( T . ModTextFromPos )  
            & B ( Indent + 4 ) & ", ModTextToPos := "
            & LbeStd . LimitedCharNoImage ( T . ModTextToPos )  
            & B ( Indent + 4 ) & ", ModTextStringRef "
            & B ( Indent + 8 ) & ":= " 
            & SharedString ( T . ModTextStringRef , Indent + 9 )
            & B ( Indent + 4 ) & ")"
          ) 
      END ModText  

  ; PROCEDURE EstOtherChild 
      ( N : LbeStd . EstRootTyp ; Indent : IndentTyp := 0 ) 
    RAISES { Backout }

    = BEGIN 
        TYPECASE N 
        OF NULL => W ( "NIL" )

        | SharedStrings . T ( TSharedString ) 
        => W ( SharedString ( TSharedString , Indent ) )  

        | ModHs . ModBlankLineTyp ( TModBlankLine )  
        => ModBlankLine ( TModBlankLine , Indent ) 

        | ModHs . ModLexErrTyp ( TModLexErr )  
        => ModLexErr ( TModLexErr , Indent ) 

        | ModHs . ModSyntErrTyp ( TModSyntErr )  
        => ModSyntErr ( TModSyntErr , Indent ) 

        | ModHs . ModDelTyp ( TModDel )  
        => ModDel ( TModDel , Indent ) 

        | ModHs . ModCmntLeadingFixedTyp ( TCmnt )  
        => ModCmnt ( "ModHs . ModCmntLeadingFixedTyp" , TCmnt , Indent ) 

        | ModHs . ModCmntLeadingRelativeTyp ( TCmnt )  
        => ModCmnt ( "ModHs . ModCmntLeadingRelativeTyp" , TCmnt , Indent ) 

        | ModHs . ModCmntTrailingFixedTyp ( TCmnt )  
        => ModCmnt ( "ModHs . ModCmntTrailingFixedTyp" , TCmnt , Indent ) 

        | ModHs . ModCmntTrailingRelativeTyp ( TCmnt )  
        => ModCmnt ( "ModHs . ModCmntTrailingRelativeTyp" , TCmnt , Indent ) 

        | ModHs . ModTextTyp ( TModText )  
        => ModText ( TModText , Indent ) 

        ELSE CantHappen 
               ( AFT . A_GenConstEst_EstOtherChild_BadChild ) 

        END (* TYPECASE *) 
      END EstOtherChild 

  ; PROCEDURE NodeName 
      ( NodeRef : EstHs . EstRefTyp ; AbsNodeNo : LbeStd . EstNodeNoTyp )  
    : TEXT 
    
    = BEGIN
        RETURN 
          "BuildNode" & Fmt . Int ( AbsNodeNo )
          & "_" 
          & TokImageM3Code 
              ( NodeRef . EstTok , UseLdlTok := TRUE , Qualified := FALSE ) 
      END NodeName 

  ; PROCEDURE GenEstNodeProc 
      ( NodeRef : EstHs . EstRefTyp 
      ; AbsNodeNo : LbeStd . EstNodeNoTyp 
      ; Indent : IndentTyp := 0 
      )  
    RAISES { Backout }
    (* Generate a procedure that builds and returns the subtree rooted
       at NodeRef. *) 

    = VAR LNodeName : TEXT 
    ; VAR LEstTravInfo : TravUtil . EstTravInfoTyp 

    ; BEGIN 
        LNodeName := NodeName ( NodeRef , AbsNodeNo ) 
      (* Make a pass over Est children, generating build procedures for each
         that is an Est interior node.  Name the procedures from the absolute
         node numbers. 
      *) 
      (* Do not use TravUtil.InitEstTravInfoFwd.  Coded as here will 
         reconstruct the tree verbatim, neither expanding nor re-optimizing
         singleton lists.
      *) 
      ; TravUtil . InitEstTravInfo 
          ( LEstTravInfo , NodeRef , AbsNodeNo )
      ; TravUtil . GetLMEstChild ( LEstTravInfo )
      ; WHILE LEstTravInfo . EtiChildNo < LEstTravInfo . EtiChildCt 
        DO
          TYPECASE LEstTravInfo . EtiChildLeafElem . LeChildRef 
          OF NULL => 
          | EstHs . EstRefTyp ( TEstRef )  
          => GenEstNodeProc 
               ( TEstRef 
               , LEstTravInfo . EtiAbsNodeNo 
                 + LEstTravInfo . EtiChildRelNodeNo 
               , Indent 
               ) 
          ELSE 
          END (* TYPECASE *) 
        ; TravUtil . IncEstChild ( LEstTravInfo ) 
        END  

      (* Now generate the build procedure for this node. *) 
      ; WL ( "; PROCEDURE " & LNodeName & " ( ) : EstHs . EstRefTyp RAISES { Backout } " ) 
      ; W ( B ( Indent + 2 ) & "= VAR LResult : EstHs . EstRefTyp " ) 
      ; WL ( B ( Indent + 2 ) 
            & "; VAR LMergeState : EstBuild . MergeStateTyp " 
          ) 
      ; W ( B ( Indent + 2 ) & "; BEGIN (* " & LNodeName & " *) " ) 
      ; W ( B ( Indent + 6 ) & "LMergeState " ) 
      ; W ( B ( Indent + 8 ) & ":= EstBuild . NewMergeState " ) 
      ; W ( B ( Indent + 13 ) & "( Lang := Lang " ) 
      ; W ( B ( Indent + 13 ) & ", EstTok := " 
            & TokImageM3Code 
                ( NodeRef . EstTok , UseLdlTok , Qualified := TRUE ) 
          ) 
      ; W ( B ( Indent + 13 ) & ", EstRefToInheritFrom := NIL " ) 
      ; W ( B ( Indent + 13 ) & ") " ) 

      (* Make a second pass, this time right-to-left, generating merges 
         of children. *)
      ; TravUtil . GetRMEstChild ( LEstTravInfo )
      ; WHILE LEstTravInfo . EtiChildNo >= 0 
        DO
          W ( B ( Indent + 4 ) & "; EstBuild . MergeChild " ) 
        ; W ( B ( Indent + 8 ) & "( MergeState := LMergeState " )   

        ; TYPECASE LEstTravInfo . EtiChildLeafElem . LeChildRef 
          OF NULL 
          => W ( B ( Indent + 8 ) & ", EstRef := NIL " )   
          | EstHs . EstRefTyp ( TEstRef )  
          => W ( B ( Indent + 8 ) & ", EstRef := "    
                 & NodeName 
                     ( TEstRef 
                     , LEstTravInfo . EtiAbsNodeNo 
                       + LEstTravInfo . EtiChildRelNodeNo 
                     ) 
                 & " ( ) "
               )  
          ELSE 
            W ( B ( Indent + 8 ) & ", EstRef " )   
          ; W ( B ( Indent + 12 ) & ":= " )   
          ; EstOtherChild 
              ( LEstTravInfo . EtiChildLeafElem . LeChildRef , Indent + 15 ) 
          END (* TYPECASE *) 
        ; W ( B ( Indent + 8 ) & ", KindSet := EstHs . EstChildKindSetTyp " 
              & B ( Indent + 12 ) 
              & EstHs . EstChildKindSetImage
                  ( LEstTravInfo . EtiChildLeafElem . LeKindSet 
                    - EstHs . EstChildKindSetFirstOfGroup 
                  , Indent := Indent + 12
                  , ImageKind := EstHs . ImageKindTyp . Qualified 
                  ) 
            )   
        ; W ( B ( Indent + 8 ) & ", IsFirstOfGroup := " 
              & Fmt . Bool 
                  ( EstHs . EstChildKindFirstOfGroup 
                    IN LEstTravInfo . EtiChildLeafElem . LeKindSet 
                  ) 
            )   
        ; W ( B ( Indent + 8 ) & ", GroupFmtNo := " 
              & EstHs . FmtNoImage 
                  ( LEstTravInfo . EtiChildLeafElem . LeFmtNo , Pad := 0 ) 
            )   
        ; W ( B ( Indent + 8 ) & ") " )   
        ; TravUtil . DecEstChild ( LEstTravInfo ) 
        END (* WHILE *) 

      ; W ( B ( Indent + 4 ) & "; EstBuild . FinishMerge " ) 
      ; W ( B ( Indent + 8 ) & "( MergeState := LMergeState " ) 
      ; W ( B ( Indent + 8 ) 
            & ", ResultEstNodeKind := EstHs . EstNodeKindTyp . " 
            & EstHs . EstNodeKindImage ( NodeRef . EstNodeKind )
           )
      ; W ( B ( Indent + 8 ) & ", ResultTreeRef := LResult " ) 
      ; W ( B ( Indent + 8 ) & ") " ) 
      ; W ( B ( Indent + 4 ) & "; RETURN LResult " ) 
      ; WL ( B ( Indent + 4 ) & "END " & LNodeName & " " ) 
      ; W ( B ( Indent ) ) 
      END GenEstNodeProc 

  ; PROCEDURE GenRoot ( N : LbeStd . EstRootTyp ; Indent : IndentTyp := 0 ) 
    RAISES { Backout }
    (* Generate a parameterless procedure named "Root" that builds and returns
       a copy of the whole tree rooted at N. *)  

    = VAR LEstRef : EstHs . EstRefTyp 

    ; BEGIN 
        WL ( "(* EXPORTED: *) " ) 
      ; WL ( "; PROCEDURE Root ( Lang : LbeStd . LangTyp := LbeStd . LangNull ) " ) 
      ; WL ( "  : LbeStd . EstRootTyp " ) 
      ; WL ( "  RAISES { Backout } " ) 
      ; WL ( B ( Indent + 2 ) & "= <* UNUSED *> CONST Dummy = 0 (* For ease of generation. *) " ) 
      ; TYPECASE N 
        OF NULL => LEstRef := NIL 
        | EstHs . EstRefTyp ( TEstRef )  
        => W ( B ( Indent + 2 ) ) 
        ; GenEstNodeProc ( TEstRef , 0 , Indent + 2 ) 
        ;  LEstRef := TEstRef
        ELSE LEstRef := NIL 
        END (* TYPECASE *) 
      ; W ( B ( Indent + 2 ) & "; BEGIN (* Root *) " ) 
      ; IF LEstRef = NIL 
        THEN 
          W ( B ( Indent + 6 ) & "RETURN " ) 
        ; W ( B ( Indent + 8 ) ) 
        ; EstOtherChild ( N , Indent + 8 ) 
        ELSE
          W ( B ( Indent + 6 ) & "RETURN " & NodeName ( LEstRef , 0 ) 
              & " ( ) " 
            ) 
        END (* IF *) 
      ; WL ( B ( Indent + 4 ) & "END Root " ) 
      END GenRoot  

  ; BEGIN (* WriteStream *) 

      VAR LLangIdRef : LangUtil . LangIdRefTyp := NIL 

    ; BEGIN 
        WL ( "MODULE " & ModuleName ) 
      ; WL ( "" ) 
      ; WL ( "(* This module was mechanically generated by GenConstEst. " ) 
      ; WL ( "     Generated on:  " 
             & Misc . DateImage 
                 ( Date . FromTime ( Time . Now ( ) , z := Date . UTC ) ) 
           ) 
      ; IF LangInfoRef # NIL 
        THEN LLangIdRef := LangInfoRef . DefLangIdRef 
        END (* IF *) 
      ; IF LLangIdRef # NIL 
        THEN
          WL ( "     " & LangUtil . LangIdImage ( LLangIdRef ^ , Indent := 5 ) )  
        END 
      ; WL ( "*) " ) 
      ; WL ( "" ) 
      ; WL ( "; IMPORT LbeStd " ) 
      ; WL ( "; IMPORT EstHs " ) 
      ; WL ( "; IMPORT EstBuild " ) 
      ; WL ( "; IMPORT ModHs (* Might not be used. *)" )
        (* ^An UNUSED pragma on an IMPORT is illegal.  A NOWARN pragma on
           an IMPORT seems to be ignored.
        *)   
      ; WL ( "; IMPORT SharedStrings " ) 
      ; WL ( "; IMPORT Ldl0Tok AS LdlTok " ) 
      ; WL ( "; FROM Failures IMPORT Backout " ) 
      ; WL ( "" ) 
      ; GenRoot  ( EstRoot , Indent := 0 ) 
      ; WL ( "" ) 
      ; WL ( "; BEGIN (* " & ModuleName & " *) " ) 
      ; WL ( "  END " & ModuleName & " " ) 
      ; WL ( ". " ) 
      ; WL ( "" ) 
      END (* Block *) 
    END WriteStream 

(* EXPORTED: *) 
; PROCEDURE WriteName  
    ( Lang : LbeStd . LangTyp 
    ; EstRoot : LbeStd . EstRootTyp 
    ; LangInfoRef : LdlSemantics . LangInfoRefTyp 
    ; UseLdlTok : BOOLEAN 
    ; ModuleName : TEXT := "Ldl0MakeEst" 
    ; FilePath : Pathname . T := "" (* Means Pathname . Current *)  
    )

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    VAR LWrT : Wr . T 

  ; BEGIN (* WriteName *) 

      VAR LFileName : TEXT 

    ; BEGIN 
        IF Text . Equal ( FilePath , "" ) 
        THEN FilePath := Pathname . Current 
        END (* IF *) 
      ; LFileName := Pathname . Join ( FilePath , ModuleName , "m3" ) 
      ; TRY 
          LWrT := VersionedFiles . OpenWrite ( LFileName ) 
        ; WriteStream 
            ( Lang , EstRoot , LangInfoRef , UseLdlTok , ModuleName , LWrT ) 
        ; Wr . Close ( LWrT ) 
        ; Wr . PutText 
            ( Stdio . stderr 
            , "Wrote  \"" & LFileName & "\"" & Wr . EOL 
            ) 
        EXCEPT 
          VersionedFiles . Error ( EMessage ) 
          => Wr . PutText 
               ( Stdio . stderr 
               , EMessage & "while trying to open \"" 
                 & LFileName & "\"" & Wr . EOL 
               ) 
        ELSE 
          Wr . PutText 
            ( Stdio . stderr 
            , "Unable to write \"" & LFileName & "\"" & Wr . EOL 
            ) 
        END (* TRY EXCEPT *) 
      END (* Block *) 
    END WriteName  

; BEGIN 
  END GenConstEst 
. 

