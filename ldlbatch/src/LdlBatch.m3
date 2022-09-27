
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LdlBatch

EXPORTS Main 

(* Library: *)
; IMPORT Compiler 
; IMPORT FileRd 
; IMPORT Fingerprint 
; IMPORT Pathname 
; IMPORT Params 
(* Except for swapping the strings "Pickle" and "Pickle2 AS Pickle", make no
   changes to the following line, as it is recognized/edited by scripts.
   (see scripts/topickle.sh and scripts/topickle2.sh)
*) 
; IMPORT Pickle2 AS Pickle (* The Pickle2 mess. *)
; IMPORT Process 
; IMPORT Rd 
; IMPORT RefList 
; IMPORT Rsrc
(* ; IMPORT RT0 *)  
; IMPORT RTIO  
; IMPORT Stdio 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr 

(* Application: *) 
; IMPORT AstView 
; IMPORT Boot 
; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT Failures 
; FROM Failures IMPORT Backout
; IMPORT Files
; IMPORT GenConstEst 
; IMPORT GrammarGen 
; IMPORT Infos
; IMPORT LangMap   
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT Ldl0Bundle 
; IMPORT Ldl0Scanner   (* Not "used", but must register itself. *)
; IMPORT Ldl0Semantics (* Not "used", but types declared within 
                                       are needed by pickles. *)
; IMPORT Ldl1Bundle 
; IMPORT Ldl1Scanner   (* Not "used", but must register itself. *)
; IMPORT Ldl1Semantics (* Not "used", but types declared within 
                                       are needed by pickles. *)
; IMPORT LdlSemantics
; IMPORT LineNumbers 
; IMPORT LRTable 
; IMPORT LRUtils 
; IMPORT MessageCodes 
; IMPORT Messages 
; IMPORT Misc 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT ScannerIf 
; IMPORT SharedStrings 
; IMPORT Strings 
; IMPORT SuffixInfo 
; IMPORT TreeBrowse
; IMPORT UnsafeUtils 
; IMPORT Version 
; IMPORT VersionedFiles 
; IMPORT WriteTrv 

; CONST TokFileNameSuffix = "Tok.i3" 
; CONST ChildFileNameSuffix = "Child.i3" 
; CONST InitTokStringsFileNameSuffix = "InitTokStrings.m3" 
; CONST MakeEstFileNameSuffix = "MakeEst.m3" 
; CONST ParseInfoManFileNameSuffix = "ParseInfoMan" 
; CONST ParseInfoGenFileNameSuffix = "ParseInfoGen" 
; CONST LRTablesManFileNameSuffix = "LRTablesMan" 
; CONST LRTablesGenFileNameSuffix = "LRTablesGen" 
; CONST SemPklFileNameSuffix = "Sem.pkl" 
; CONST FsTreesFileNameSuffix = "FsTrees.dump"
; CONST DefaultDebugLevel = 3 

(* For reading preexisting LDLs: *) 
; VAR Ldl0LangInfoRef : LdlSemantics . LangInfoRefTyp := NIL 
; VAR Ldl1LangInfoRef : LdlSemantics . LangInfoRefTyp := NIL 
; CONST Ldl0SemFileName = "Ldl0Sem.pkl" 
; CONST Ldl1SemFileName = "Ldl1Sem.pkl" 

; VAR DoDisplayHelp := FALSE 
; VAR DoDisplayVersion := FALSE
; VAR DoPickles := FALSE  
; VAR DoWriteManPrecAndAssoc := FALSE 
; VAR DoWriteGenPrecAndAssoc := FALSE 
; VAR DoWriteManProductions := FALSE 
; VAR DoWriteGenProductions := FALSE 
; VAR DoWriteManLRTables := FALSE 
; VAR DoWriteGenLRTables := FALSE 
; VAR DoWriteManStates := FALSE 
; VAR DoWriteGenStates := FALSE 
; VAR DoWriteText := FALSE 
; VAR DoWriteLdlTok := FALSE 
; VAR DoWriteLdlChild := FALSE 
; VAR DoWriteInitTokStrings := FALSE 
; VAR DoWriteMakeEst := FALSE 
; VAR DoWriteFsTrees := FALSE 
; VAR DoAnalyze := FALSE   
; VAR DoGenGrammar := FALSE   
; VAR DoFactorGenGrammar := FALSE   
; VAR ForceAnalyze := FALSE   
; VAR DoBrowse := FALSE   
; VAR DoPrintSizes := FALSE   
; VAR DoPrintStats := FALSE   
; VAR DoInsertNilFixedChildren := FALSE 
; VAR UseGenGrammar := FALSE 

; VAR GFileName : TEXT := ""
; VAR GImageRef : PaintHs . ImageTransientTyp 
; VAR GDevelWritePath : TEXT := "" 
; VAR GLangInfoRef : LdlSemantics . LangInfoRefTyp 

; PROCEDURE WL ( Msg : TEXT ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN 
      Wr . PutText ( Stdio . stderr , Msg & Wr . EOL ) 
    END WL  

; CONST DL = Messages . StdErrLine 

; PROCEDURE GetParams ( ) 

  = VAR LParamCt : INTEGER 
  ; VAR LParamNo : INTEGER 
  ; VAR LParam : TEXT 
  ; VAR LParamLength : INTEGER 
  ; VAR LCharNo : INTEGER 
  ; VAR LChar : CHAR 

  ; BEGIN 
      Options . DebugLevel := DefaultDebugLevel 
    ; LParamNo := 1 
    ; LParamCt := Params . Count 
    ; WHILE LParamNo < LParamCt 
      DO
        LParam := Params . Get ( LParamNo ) 
      ; LParamLength := Text . Length ( LParam ) 
      ; IF Text . GetChar ( LParam , 0 ) = '-' 
        THEN
          LCharNo := 1 
        ; WHILE LCharNo < LParamLength   
          DO
            LChar := Text . GetChar ( LParam , LCharNo ) 
          ; CASE LChar 
            OF 'a' => DoWriteManLRTables := TRUE  
            | 'A' => DoWriteGenLRTables := TRUE  
            | 'b' => DoBrowse := TRUE  
            | 'c' => DoWriteLdlChild := TRUE 
            | 'd' => WL ( "-d option not yet implemented." )           
              (* Hint: draw from Lbe.GetArgs.GaNumericArg. *) 
            | 'D' 
              => INC ( LCharNo ) 
              ; IF LCharNo >= LParamLength  
                THEN
                  INC ( LParamNo ) 
                ; IF LParamNo >= LParamCt 
                  THEN 
                    DoDisplayHelp := TRUE 
                  ; EXIT 
                  END 
                ; LParam := Params . Get ( LParamNo ) 
                ; LParamLength := Text . Length ( LParam ) 
                ; LCharNo := 0 
                END (* IF *)    
              ; LChar := Text . GetChar ( LParam , LCharNo ) 
              ; Options . ResourceDir := Text . Sub ( LParam , LCharNo , LParamLength )
              ; LCharNo := LParamLength 
            | 'e' => DoWriteMakeEst := TRUE 
            | 'f' => DoWriteFsTrees := TRUE 
            | 'g' => DoFactorGenGrammar := TRUE 
            | 'G' => DoGenGrammar := TRUE 
            | 'h' => DoDisplayHelp := TRUE 
            | 'i' => DoWriteInitTokStrings := TRUE 
            | 'j' => DoPrintSizes := TRUE 
            | 'k' => DoPrintStats := TRUE 
            | 'l' => DoWriteText := TRUE 
            | 'n' => DoInsertNilFixedChildren := TRUE 
            | 'p' => DoPickles := TRUE 
            | 'q' => DoWriteManPrecAndAssoc := TRUE 
            | 'Q' => DoWriteGenPrecAndAssoc := TRUE 
            | 'r' => DoWriteManProductions := TRUE 
            | 'R' => DoWriteGenProductions := TRUE 
            | 's' => DoWriteManStates := TRUE 
            | 'S' => DoWriteGenStates := TRUE 
            | 't' => DoWriteLdlTok := TRUE 
            | 'u' => UseGenGrammar := TRUE 
            | 'v' => DoDisplayVersion := TRUE 
            | 'z' => DoAnalyze := TRUE 
            | 'Z' => ForceAnalyze := TRUE 
            ELSE 
              WL ( "Invalid option character: \'" 
                   & Text . FromChar ( LChar )  & "\'"  
                 ) 
            ; DoDisplayHelp := TRUE 
            END
          ; INC ( LCharNo )  
          END (* WHILE *) 
        ELSE
          GFileName := LParam 
        END (* IF *)  
      ; INC ( LParamNo )  
      END (* WHILE *)  
    ; DoGenGrammar 
        := DoGenGrammar 
           OR DoWriteGenProductions 
           OR DoWriteGenStates 
           OR DoWriteGenLRTables 
           OR DoFactorGenGrammar 
    ; DoAnalyze 
        := DoAnalyze 
           OR DoGenGrammar 
           OR DoWriteLdlChild OR DoWriteLdlTok OR DoWriteInitTokStrings 
           OR DoPickles OR DoWriteFsTrees 
           OR DoWriteManProductions OR DoWriteGenProductions 
           OR DoWriteManPrecAndAssoc OR DoWriteGenPrecAndAssoc  
           OR DoWriteManStates OR DoWriteGenStates 
           OR DoWriteManLRTables OR DoWriteGenLRTables 
    ; Options . PreferGeneratedGrammar := UseGenGrammar 
    ; Options . SetDerivedDebugOptions ( ) 
    ; IF NOT DoDisplayVersion AND NOT DoDisplayHelp AND NOT DoPrintSizes 
         AND Text . Equal ( GFileName , "" ) 
      THEN 
        DL ( "No file name specified." ) 
      ; DoDisplayHelp := TRUE
      END 
    END GetParams 

; PROCEDURE DisplayHelp ( ) 

  = BEGIN 
      WL ( "LdlBatch program: Schutz semantic editor, new languages." ) 
    ; WL ( "Usage: LdlBatch {-{option}} <File>" ) 
    ; WL ( "  Options are: " ) 
    ; WL ( "  -a Write manual LR tables to file <Lang>" 
                 & LRTablesManFileNameSuffix 
                 & " (implies -z)." 
         )
    ; WL ( "  -A Write generated LR tables to file <Lang>" 
                 & LRTablesGenFileNameSuffix 
                 & " (implies -z and -G)." 
         )
    ; WL ( "  -b Invoke interactive tree browser." ) 
    ; WL ( "  -c Write interface <Lang>" & ChildFileNameSuffix 
                 & " (implies -z)." 
         )
    ; WL ( "  -d<n> or -d <n> Set debug level to <n>. (Not yet implemented.)" ) 
    ; WL ( "  -D<dir> or -D <dir> Use <dir> to find resources." ) 
    ; WL ( "     (default: " & Options . ResourceDir ) 
    ; WL ( "  -e Write module <Lang>" & MakeEstFileNameSuffix ) 
    ; WL ( "  -f Write format syntax tree dump file <Lang>" 
                 & FsTreesFileNameSuffix 
                 & " (implies -z)." 
         ) 
    ; WL ( "  -g Factor generated grammar (implies -G and -z).") 
    ; WL ( "  -G Generate a concrete grammar from FS rules (implies -z).") 
    ; WL ( "  -h Display version and help text and exit." ) 
    ; WL ( "  -i Write module <Lang>" & InitTokStringsFileNameSuffix 
                 & " (implies -z)." 
         ) 
    ; WL ( "  -j Display sizes of various heap node types." ) 
    ; WL ( "  -k Display language statistics." ) 
    ; WL ( "  -l (Re)write text file of Ldl spec, from internal form." ) 
    ; WL ( "  -n Insert NIL fixed children." ) 
    ; WL ( "  -p Write pickle file." & " (implies -z)." ) 
    ; WL ( "  -q Write manual precedence and associativities to file <Lang>" 
                 & ParseInfoManFileNameSuffix 
                 & " (implies -z)." 
         ) 
    ; WL ( "  -Q Write generated precedence and associativities to file <Lang>" 
                 & ParseInfoGenFileNameSuffix 
                 & " (implies -z and -G)." 
         ) 
    ; WL ( "  -r Write manual productions to file <Lang>" 
                 & ParseInfoManFileNameSuffix 
                 & " (implies -z)." 
         ) 
    ; WL ( "  -R Write generated productions to file <Lang>" 
                 & ParseInfoGenFileNameSuffix 
                 & " (implies -z and -G)." 
         ) 
    ; WL ( "  -s Write manual LR states to file <Lang>" 
                 & ParseInfoManFileNameSuffix 
                 & " (implies -z)." 
         )
    ; WL ( "  -S Write generated LR states to file <Lang>" 
                 & ParseInfoGenFileNameSuffix 
                 & " (implies -z and -G)." 
         )
    ; WL ( "  -t Write interface <Lang>" & TokFileNameSuffix 
                 & " (implies -z)." 
         ) 
    ; WL ( "  -u Use generated grammar for parsing, if available." ) 
    ; WL ( "  -v Display version and exit." ) 
    ; WL ( "  -z Analyze (if not already done)." ) 
    ; WL ( "  -Z Force analyze." ) 
    END DisplayHelp 

; PROCEDURE DisplayVersion ( ) 

  = BEGIN 
      WL ( "Schutz Semantic Editor, LdlBatch program," ) 
    ; WL ( "version " 
           & Version . VersionString 
           & " " 
           & Version . DateString 
         ) 
    END DisplayVersion  

; PROCEDURE WritePkl 
    ( LangInfoRef : LdlSemantics . LangInfoRefTyp 
    ; FileName : TEXT 
    ; Label : TEXT 
    ; PickleIdInfoRef : LbeStd . PickleIdInfoRefTyp 
    ) 

  = VAR LWriter : Wr . T 

  ; BEGIN
      TRY
        LWriter := VersionedFiles . OpenWrite ( FileName ) 
      EXCEPT 
        VersionedFiles . Error ( EMessage ) 
        => WL ( EMessage & "while trying to open " & Label 
                &  " \"" & FileName & "\" for writing." 
              ) 
      ; Failures . ExitWFailure ( ) <* NORETURN *> 
      ELSE 
        WL ( "Unable to open " & Label 
             & " \"" & FileName & "\" for writing." 
           ) 
      ; Failures . ExitWFailure ( ) <* NORETURN *> 
      END (* TRY EXCEPT *) 
    ; TRY 
        Pickle . Write ( LWriter , PickleIdInfoRef ) 
      ; Pickle . Write ( LWriter , LangInfoRef ) 
      ; Wr . Close ( LWriter ) 
      ; WL ( "Wrote:" ) 
      ; WL ( LangUtil . LangIdImage ( LangInfoRef ^ . DefLangIdRef ^ , 2 ) ) 
      ; WL ( "to " & Label & " \"" & FileName & "\"" ) 
      EXCEPT 
      Pickle . Error ( EMessage )
      => WL ( "Exception raised while writing " & Label 
              & " \"" & FileName & "\":" 
            ) 
      ; WL ( "  ConvertPacking.Error(" & EMessage & ")" ) 
      ; Failures . ExitWFailure ( ) <* NORETURN *> 
      ELSE 
        WL ( "Unable to write " & Label & " \"" & FileName & "\"" ) 
      ; Failures . ExitWFailure ( ) <* NORETURN *> 
      END (* TRY EXCEPT *) 
    END WritePkl 

; PROCEDURE WriteFsTrees 
    ( FileName : TEXT ; Lang : LbeStd . LangTyp ) 

  = VAR LWriter : Wr . T 

  ; BEGIN
      IF DoWriteFsTrees 
      THEN 
        TRY
          LWriter := VersionedFiles . OpenWrite ( FileName ) 
        ; LdlSemantics . DumpAllFsTreesStream ( LWriter , Lang ) 
        ; Wr . Close ( LWriter ) 
        ; WL ( "Wrote FS trees dump file \"" & FileName & "\"" ) 
        EXCEPT 
          VersionedFiles . Error ( EMessage ) 
          => WL ( EMessage & "while trying to open \"" & FileName & "\"" 
                ) 
        ELSE 
          WL ( "Unable to write FS trees dump file \"" & FileName & "\"" 
             ) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END WriteFsTrees 

; PROCEDURE Browse 
    ( EstRoot : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull  
    ) 
  RAISES { Backout } 

  = VAR LSession : TreeBrowse . T 

  ; BEGIN
      IF DoBrowse 
      THEN 
        LSession 
          := NEW ( TreeBrowse . T ) . initStdSession ( EstRoot , Lang ) 
      ; WL ( "Browsing Est:" ) 
      ; TreeBrowse . Interp ( LSession ) 
      END (* IF *) 
    END Browse  

; VAR LdlTextWr : Wr . T 
(* TODO:  This is here because of a one-time CG bug.  It and WriteProc
          belong inside WriteText. 
*)  

(* TODO: WriteProc and WriteText more or less duplicate code in
         Ui for exporting.  Can they be combined?. *) 
; PROCEDURE WriteProc 
    ( <* UNUSED *> ImageRef : PaintHs . ImageTransientTyp 
    ; String : Strings . StringTyp 
    )  
  RAISES { Backout } 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN
      Wr . PutText ( LdlTextWr , Strings . ToTextNonNIL ( String ) ) 
    ; Wr . PutText ( LdlTextWr , Wr . EOL ) 
    END WriteProc 

; PROCEDURE WriteText 
    ( ImageRef : PaintHs . ImageTransientTyp ; FileName : TEXT ; Label : TEXT )
  (* Write a text file of an analyzed image. *) 

  = BEGIN
      IF DoWriteText 
      THEN 
        TRY 
          LdlTextWr := VersionedFiles . OpenWrite ( FileName ) 
        ; WriteTrv . WriteText 
            ( ImageRef , WriteProc , DoGenerateText := TRUE ) 
        ; Wr . Close ( LdlTextWr ) 
        ; WL ( "Wrote " & Label & " \"" & FileName & "\"" ) 
        EXCEPT 
          VersionedFiles . Error ( EMessage ) 
          => DL ( EMessage & "while trying to open " & Label 
                  & " \"" & FileName & "\"" 
                ) 
        ELSE 
          DL ( "Unable to write " & Label 
                & " \"" & FileName & "\"" 
             ) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END WriteText 

; PROCEDURE JoinGeneratedFileName 
    ( Path : TEXT ; Module : TEXT ; Suffix : TEXT ) 
  : TEXT 

  = BEGIN 
      RETURN 
        Pathname . Join 
          ( Path , Module & Suffix , ext := NIL ) 
    END JoinGeneratedFileName 

; PROCEDURE WriteParseInfo 
    ( FileNameSuffix : TEXT 
    ; Gram : LRTable . GrammarTyp 
    ; DoWritePrecAndAssoc : BOOLEAN 
    ; DoWriteProductions : BOOLEAN 
    ; DoWriteStates : BOOLEAN 
    ) 

  = VAR LFileName : TEXT 
  ; VAR LWrT : Wr . T 

  ; BEGIN 
      IF DoWriteProductions OR DoWriteStates 
      THEN
        LFileName 
          := JoinGeneratedFileName
               ( GDevelWritePath 
               , LdlSemantics . LdlModuleName ( GImageRef ) 
               , FileNameSuffix  
               ) 
      ; TRY 
          LWrT := VersionedFiles . OpenWrite ( LFileName )
        ; IF DoWritePrecAndAssoc  
          THEN 
            Infos . WritePrecAndAssocList ( Gram , LWrT ) 
          ; WL ( "Wrote precedences and associativites to file \"" 
                 & LFileName & "\"" 
               ) 
          END 
        ; IF DoWriteProductions 
          THEN 
            Infos . WriteProductions ( Gram , LWrT ) 
          ; WL ( "Wrote productions to file \"" & LFileName & "\"" ) 
          END 
        ; IF DoWriteStates 
          THEN 
            Infos . WriteStates ( Gram , LWrT ) 
          ; WL ( "Wrote states to file \"" & LFileName & "\"" ) 
          END 
        ; Wr . Close ( LWrT ) 
        EXCEPT 
        VersionedFiles . Error ( EMessage ) 
        => DL ( EMessage & Wr . EOL 
                & ",  while opening parse info file \"" & LFileName & "\"" 
              ) 
        ELSE
          DL ( " Unable to write productions/states to file \"" 
               & LFileName & "\"" 
             ) 
        END 
      END 
    END WriteParseInfo 

; PROCEDURE WriteLRTables  
    ( FileNameSuffix : TEXT 
    ; Gram : LRTable . GrammarTyp 
    ) 

  = VAR LFileName : TEXT 
  ; VAR LWrT : Wr . T 

  ; BEGIN 
      LFileName 
        := JoinGeneratedFileName
             ( GDevelWritePath 
             , LdlSemantics . LdlModuleName ( GImageRef ) 
             , FileNameSuffix  
             ) 
    ; TRY 
        LWrT := VersionedFiles . OpenWrite ( LFileName )
      ; Infos . WriteLRTables ( Gram , LWrT ) 
      ; WL ( "Wrote LR Tables to file \"" & LFileName & "\"" ) 
      ; Wr . Close ( LWrT ) 
      EXCEPT ELSE 
        DL ( " Unable to write LR tables to file \"" 
             & LFileName & "\"" 
           ) 
      END (* TRY EXCEPT *) 
    END WriteLRTables  

; PROCEDURE ReadFile 
    ( FileName : TEXT ) : PaintHs . ImageTransientTyp 
  RAISES { Backout } 

  = VAR LSimpleName : TEXT 
  ; VAR LImageName : TEXT 
  ; VAR LAbsFileName : TEXT 
  ; VAR LImageRef : PaintHs . ImageTransientTyp 

  ; BEGIN (* ReadFile *) 
      LSimpleName := Pathname . Last ( FileName ) 
    ; LImageName := Misc . TextName ( LSimpleName ) 
    ; TRY <* NOWARN *>
        LAbsFileName := Misc . AbsFileName ( FileName ) 
      EXCEPT ELSE 
        RETURN NIL 
      END (* EXCEPT *) 
    ; IF Misc . IsPickleName ( FileName ) 
      THEN 
        TRY 
          LImageRef := Files . ReadNamedImageFile ( FileName ) 
        EXCEPT 
        Backout ( E ) => RAISE Backout ( E )  
        | Thread . Alerted => 
        | Files . Error ( EMessage ) 
        => DL ( EMessage ) 
        ; LImageRef := NIL 
        ELSE LImageRef := NIL 
        END (* TRY EXCEPT *) 
      ; IF LImageRef # NIL 
        THEN 
          LImageRef . ItPers . IpImageName := LImageName 
(* TODO: Check/handle the case where previously existing name or path
       has changed. *) 
        ; LImageRef . ItPers . IpAbsTextFileName 
            := Misc . TextName ( LAbsFileName ) 
        ; LImageRef . ItPers . IpAbsPklFileName 
            := Misc . PickleName ( LAbsFileName )    
        END (* IF *) 
      ELSE (* It's a text file. *) 
        TRY 
          LImageRef := Files . OpenNamedTextFile ( FileName ) 
        EXCEPT 
        Backout ( E ) => RAISE Backout ( E )  
        | Thread . Alerted => 
        | Files . Error ( EMessage ) 
        => DL ( EMessage ) 
        ; LImageRef := NIL 
        ELSE LImageRef := NIL 
        END (* TRY EXCEPT *) 
   (* ; LImageRef := ParseFile ( FileName ) *) 
      ; IF LImageRef # NIL 
        THEN 
          LImageRef . ItPers . IpImageName := LImageName 
        END (* IF *) 
      END (* IF *) 
    ; RETURN LImageRef 
    END ReadFile 

; PROCEDURE Open ( FileName : TEXT ) : BOOLEAN (* Success *) 
  RAISES { Backout } 

  = VAR LResult : BOOLEAN := FALSE 

  ; BEGIN 
      IF FileName # NIL AND NOT Text . Equal ( FileName , "" )  
      THEN 
        GImageRef := ReadFile ( FileName ) 
      ; IF GImageRef = NIL 
        THEN 
          WL ( "Unable to open file \"" & FileName & "\"" ) 
        ELSE 
          TRY <* NOWARN *> 
            GDevelWritePath 
              := Pathname . Prefix ( Misc . AbsFileName ( FileName ) )  
          ; LResult := TRUE 
          EXCEPT ELSE 
            DL ( "Unable to get absolute path for \"" & FileName & "\"" ) 
          END 
        END 
      END 
    ; RETURN LResult 
    END Open

; PROCEDURE LangImage ( Lang : LbeStd . LangTyp ) : TEXT 

  = BEGIN 
      RETURN 
        LangUtil . LangIdRef ( Lang ) ^ . LangShortName 
        & " (code=" &  LbeStd . LangImage ( Lang ) & ")" 
    END LangImage 

; PROCEDURE HasErrors ( ) : BOOLEAN 

  = BEGIN 
      RETURN 
        Messages . MessageCount ( MessageCodes . KindTyp . MkError ) > 0 
        OR Messages . MessageCount ( MessageCodes . KindTyp . MkFatal ) > 0 
        OR Messages . MessageCount ( MessageCodes . KindTyp . MkAssert ) > 0 
    END HasErrors 

; PROCEDURE Analyze
    ( ImageRef : PaintHs . ImageTransientTyp ) : BOOLEAN (* Success *) 
  RAISES { Backout } 

  = <* FATAL Thread . Alerted *> 
    BEGIN 
      IF ImageRef = NIL 
      THEN RETURN FALSE 
      ELSIF ImageRef . ItPers . IpLang # LbeStd . LangLdl0 
            AND ImageRef . ItPers . IpLang # LbeStd . LangLdl1 
      THEN 
        WL ( "Language " & LangImage ( ImageRef . ItPers . IpLang ) 
              & " is not a known LDL" 
           ) 
      ; RETURN FALSE 
      ELSIF LangMap . LangInfo ( ImageRef . ItPers . IpLang ) = NIL 
      THEN 
        WL ( "Definition of language " 
              & LangImage ( ImageRef . ItPers . IpLang ) 
              & " is not loaded." 
           ) 
      ; RETURN FALSE 
      ELSE 
        DL ( "Analyzing -----------------------------------------------" ) 
      ; Messages . ZeroMessageCounts ( ) 
      ; ImageRef . ItPers . IpSemRoot 
          := LdlSemantics . Analyze 
               ( ImageRef . ItPers . IpEstRoot , ImageRef . ItPers . IpLang ) 
      ; IF HasErrors ( ) 
        THEN 
          DL ( "Analysis has errors. ------------------------------------" ) 
        ; RETURN FALSE 
        ELSE 
          DL ( "Analysis completed without errors. ----------------------" ) 
        ; RETURN TRUE 
        END (* IF *) 
      END (* IF *) 
    END Analyze 

; PROCEDURE Init ( ) 

  = BEGIN
   (* Get LDL languages *) 
      Options . ResourcePath 
        := RefList . AppendD 
             ( Rsrc . BuildPath 
                 ( Options . ResourceDir , "$LBERESOURCE" ) 
             , Rsrc . BuildPath 
                 ( Ldl0Bundle . Get ( ) , Ldl1Bundle . Get ( ) ) 
             ) 
(* No longer load languages eagerly.  
    ; EVAL LangUtil . LoadLanguage ( "ldl0" ) 
    ; EVAL LangUtil . LoadLanguage ( "ldl1" ) 
*) 
    ; Options . InsertNilFixedChildren := DoInsertNilFixedChildren 
    END Init 

; PROCEDURE Work ( ) 
  RAISES { Backout } 

  = VAR LStats1 : EstUtil . StatisticsTyp 
  ; VAR LFileName : TEXT 
  ; VAR LWrT : Wr . T 
  ; VAR LLangInfoRef : LdlSemantics . LangInfoRefTyp 
  ; LAnalyzedOK : BOOLEAN 

  ; BEGIN 

    (* Print node sizes. *) 
      IF DoPrintSizes 
      THEN Boot . PrintSizes ( Stdio . stderr ) 
      END (* IF *) 

   (* Get file *)
    ; IF Open ( GFileName )  
      THEN 
        WL ( "File \"" & GFileName & "\" successfully opened." ) 

      (* Print statistics. *) 
      ; EstUtil . Statistics ( GImageRef . ItPers . IpEstRoot , LStats1 ) 
      ; IF DoPrintStats 
        THEN 
          Boot . PrintStats ( Stdio . stderr , LStats1 ) 
        END (* IF *) 

      (* Write text file from parsed Ldl spec. *) 
      ; WriteText 
         ( GImageRef , GImageRef . ItPers . IpAbsTextFileName , "text file") 

      (* Analyze. *) 
      ; IF ForceAnalyze 
           OR ( DoAnalyze 
                AND GImageRef # NIL AND NOT GImageRef . ItPers . IpIsAnalyzed 
              ) 
        THEN 
          LAnalyzedOK := Analyze ( GImageRef )
(* TODO: We need to distinguish different stages of failure.  Some of them
         should not prevent some of the output emissions below.
*) 
        ; IF FALSE AND NOT LAnalyzedOK 
          THEN  
            Failures . ExitWFailure ( ) <* NORETURN *> 
          END (* IF *) 
        END (* IF *) 
      ; GLangInfoRef 
          := NARROW 
               ( GImageRef . ItPers . IpSemRoot 
               , LdlSemantics . LangInfoRefTyp 
               ) 

      (* Do LR generation. *) 
      ; IF LAnalyzedOK AND GLangInfoRef # NIL 
        THEN 
          LdlSemantics . ManLRGen ( GLangInfoRef ) 
        END (* IF *)  

      (* Generate a concrete grammar from FS rules. *) 
      ; IF LAnalyzedOK AND DoGenGrammar
        THEN
          DL ( "Generating concrete grammar -----------------------------" ) 
        ; IF DoFactorGenGrammar 
          THEN DL ( "Generated grammar will be factored." ) 
          ELSE DL ( "Generated grammar will not be factored." ) 
          END (* IF *) 
        ; GrammarGen . Generate 
            ( GLangInfoRef , Factored := DoFactorGenGrammar ) 
        ; DL ( "Generation complete -------------------------------------" ) 
        END (* IF *) 

      (* Browse the tree. *) 
      ; Browse 
          ( GImageRef . ItPers . IpEstRoot , GImageRef . ItPers . IpLang ) 

      (* Write interface <Lang>Tok. *) 
      ; IF DoWriteLdlTok AND LAnalyzedOK AND GLangInfoRef # NIL  
        THEN 
          LFileName 
            := JoinGeneratedFileName
                 ( GDevelWritePath 
                 , LdlSemantics . LdlModuleName ( GImageRef ) 
                 , TokFileNameSuffix  
                 ) 
        ; TRY 
            LWrT := VersionedFiles . OpenWrite ( LFileName )
          ; LdlSemantics . WriteLdlTokInterfaceToStream 
              ( LWrT , GLangInfoRef , Pathname . LastBase ( LFileName ) )  
          ; Wr . Close ( LWrT ) 
          ; DL ( "Wrote token file \"" & LFileName & "\"" ) 
          EXCEPT ELSE 
            DL ( " Unable to write token file \"" & LFileName & "\"" ) 
          END 
        END (* IF *) 

      (* Write interface <Lang>Child. *) 
      ; IF DoWriteLdlChild AND LAnalyzedOK AND GLangInfoRef # NIL 
        THEN 
          LFileName 
            := JoinGeneratedFileName
                 ( GDevelWritePath 
                 , LdlSemantics . LdlModuleName ( GImageRef ) 
                 , ChildFileNameSuffix  
                 ) 
        ; TRY 
            LWrT := VersionedFiles . OpenWrite ( LFileName )
          ; LdlSemantics . WriteLdlChildInterfaceToStream 
              ( LWrT , GLangInfoRef , Pathname . LastBase ( LFileName ) )  
          ; Wr . Close ( LWrT ) 
          ; DL ( "Wrote Est child file \"" & LFileName & "\"" ) 
          EXCEPT ELSE 
            DL ( " Unable to write Est child file \"" & LFileName & "\"" ) 
          END 
        END (* IF *) 

      (* Write module <Lang>InitTokStrings. *) 
      ; IF DoWriteInitTokStrings AND LAnalyzedOK AND GLangInfoRef # NIL 
        THEN 
          LFileName 
            := JoinGeneratedFileName
                 ( GDevelWritePath 
                 , LdlSemantics . LdlModuleName ( GImageRef ) 
                 , InitTokStringsFileNameSuffix  
                 ) 
        ; TRY 
            LWrT := VersionedFiles . OpenWrite ( LFileName )
          ; LdlSemantics . WriteInitTokStringsModuleToStream 
              ( LWrT 
              , GLangInfoRef 
              , LdlSemantics . LdlModuleName ( GImageRef ) 
                (* ^Oh, who cares if we reevaluate this. *) 
              , Pathname . LastBase ( LFileName ) 
              )  
          ; Wr . Close ( LWrT ) 
          ; DL ( "Wrote initialization file \"" & LFileName & "\"" ) 
          EXCEPT ELSE 
            DL ( " Unable to write initialization file \"" 
                 & LFileName & "\"" 
               ) 
          END 
        END (* IF *) 

      (* Generate EstBuilder. *) 
      ; IF DoWriteMakeEst 
        THEN
          LFileName 
            := JoinGeneratedFileName
                 ( GDevelWritePath 
                 , LdlSemantics . LdlModuleName ( GImageRef ) 
                 , MakeEstFileNameSuffix  
                 ) 
        ; TRY 
            LWrT := VersionedFiles . OpenWrite ( LFileName )
          ; GenConstEst . WriteStream 
              ( Lang := GImageRef . ItPers . IpLang 
              , EstRoot := GImageRef . ItPers . IpEstRoot 
              , LangInfoRef := GLangInfoRef  
              , UseLdlTok := TRUE 
              , ModuleName := Pathname . LastBase ( LFileName ) 
              , WrT := LWrT   
              ) 
          ; Wr . Close ( LWrT ) 
          ; DL ( "Wrote Est builder file \"" & LFileName & "\"" ) 
          EXCEPT ELSE 
            DL ( "Unable to write Est builder file \"" 
                 & LFileName & "\"" 
               ) 
          END 
        END (* IF *) 

      (* Write FsTrees dump file. *)  
      ; WriteFsTrees 
          ( JoinGeneratedFileName
              ( GDevelWritePath 
              , LdlSemantics . LdlModuleName ( GImageRef ) 
              , FsTreesFileNameSuffix  
              ) 
          , LbeStd . LangLdlAnalyzed 
          )

      (* Write parsing information. *) 
      ; IF LAnalyzedOK AND GLangInfoRef # NIL AND GLangInfoRef . Gram # NIL  
        THEN 
          WriteParseInfo 
            ( ParseInfoManFileNameSuffix 
            , GLangInfoRef . Gram 
            , DoWriteManPrecAndAssoc 
            , DoWriteManProductions 
            , DoWriteManStates 
            ) 
        END (* IF *) 
      ; IF DoGenGrammar 
           AND GLangInfoRef # NIL  
           AND GLangInfoRef . GenGram # NIL  
        THEN 
          WriteParseInfo 
            ( ParseInfoGenFileNameSuffix 
            , GLangInfoRef . GenGram 
            , DoWriteManPrecAndAssoc 
            , DoWriteGenProductions 
            , DoWriteGenStates 
            ) 
        END (* IF *)

      (* Write LR tables. *) 
      ; IF DoWriteManLRTables 
           AND LAnalyzedOK 
           AND GLangInfoRef # NIL 
           AND GLangInfoRef . Gram # NIL  
        THEN 
          WriteLRTables  
            ( LRTablesManFileNameSuffix , GLangInfoRef . Gram ) 
        END (* IF *) 
      ; IF DoGenGrammar 
           AND DoWriteGenLRTables 
           AND GLangInfoRef # NIL  
           AND GLangInfoRef . GenGram # NIL  
        THEN 
          WriteLRTables  
            ( LRTablesGenFileNameSuffix , GLangInfoRef . GenGram ) 
        END (* IF *) 

      (* Write pickle of Ldl in Ldl. *)  
      ; IF DoPickles AND NOT HasErrors ( )  
        THEN 
          LLangInfoRef := NEW ( LdlSemantics . LangInfoRefTyp )
        ; LLangInfoRef ^ := GLangInfoRef ^  
        ; LLangInfoRef ^ . Root := AstView . AstRefNull 
        ; LLangInfoRef ^ . SemMapRef := NIL 
        ; LLangInfoRef ^ . TokMapRef := NIL 
        ; LLangInfoRef ^ . SymbolTable := NIL 
        ; LLangInfoRef ^ . StartRule := AstView . AstRefNull 
        ; LLangInfoRef ^ . PrecRule := AstView . AstRefNull 
        ; LLangInfoRef ^ . ClassTable := NIL 
        ; LLangInfoRef ^ . TokStringTable := NIL 
        ; LRUtils . StripGrammar ( LLangInfoRef ^ . Gram ) 
        ; LRUtils . StripGrammar ( LLangInfoRef ^ . GenGram ) 
        ; WritePkl 
            ( GLangInfoRef 
            , JoinGeneratedFileName
                ( GDevelWritePath 
                , LdlSemantics . LdlModuleName ( GImageRef ) 
                , SemPklFileNameSuffix  
                ) 
            , "language pickle file" 
            , LbeStd . LangPickleIdInfoRef 
            )
        END (* IF *) 

      ; IF HasErrors ( ) OR NOT LAnalyzedOK 
        THEN
          DL ( "LDL translation failed" ) 
        ; Failures . ExitWFailure ( ) <* NORETURN *> 
        END (* IF *)  

      ELSE (* Open failed. *) 
        Failures . ExitWFailure ( ) <* NORETURN *> 
      END (* IF *) 
    END Work 

; TYPE FPArrayTyp = ARRAY [0..7] OF BITS 8 FOR [0..255]

; TYPE FPTyp = RECORD byte: FPArrayTyp END

; VAR FP := FPTyp { byte := FPArrayTyp { 109,125,137,63,246,33,196,22 } }

; PROCEDURE SetDefaults ( )

  = BEGIN
      Options . ResourceDir := Options . ResourceDirDefault
    END SetDefaults 

; BEGIN
    Misc . LoadYourself ( )
    (* Load libschutz right away, so m3gdb can set breakpoints therein. *)
  ; GetParams ( )
  ; SetDefaults ( ) 
  ; IF DoDisplayHelp 
    THEN 
      DisplayVersion ( ) 
    ; DisplayHelp ( )
    ELSIF DoDisplayVersion 
    THEN 
      DisplayVersion ( ) 
    ELSE  
      TRY 
        Init ( ) 
      ; Work ( ) 
      EXCEPT 
      ELSE (* Including Failures . Terminate. *)
        RTIO . PutText ( "LbeBatch, Unhandled exception: " ) 
      ; RTIO . PutText 
          ( Failures . ExcName
              ( UnsafeUtils . AdrToRT0_ActivationPtr 
                  ( Compiler . ThisException ( ) 
                    (* ^ NOTE: Misnamed Compiler.ThisException should
                        be named ThisActivation!! *)
                  )
                ^ 
              ) 
          )
      ; RTIO . PutText ( Wr . EOL )  
      ; RTIO . Flush ( ) 
      END (* TRY EXCEPT *) 
    ; Failures . ExitWFailure ( ) <* NORETURN *> 
    END (* IF *) 
  END LdlBatch 
.
 
