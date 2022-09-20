
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LdlBootMain 
EXPORTS Main 

(* Library: *) 
; IMPORT Compiler 
; IMPORT FileRd 
; IMPORT Fmt
; IMPORT OSError 
; IMPORT Params 
(* Except for swapping the strings "Pickle" and "Pickle2 AS Pickle", make no
   changes to the following line, as it is recognized/edited by scripts.
   (see scripts/topickle.sh and scripts/topickle2.sh)
*) 
; IMPORT Pickle2 AS Pickle (* The Pickle2 mess. *)
; IMPORT Process
; IMPORT Rd
; IMPORT RTIO 
; IMPORT Stdio 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr 

(* Ldlboot: *) 
; IMPORT Boot 
; IMPORT SchutzCoroutine 
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
; IMPORT Ldl0MakeEst

(* Sometime bootstrap problem:  
     LdlBootMain IMPORTs Ldl0MakeEst.m3, so it can (conditionally, -E option,
     UseGeneratedEst variable) use the Est that Ldl0MakeEst.m3 builds for the 
     language definition of Ldl0, instead of the one from ManualAsTrees.  
     Ldl0MakeEst.m3 is machine-generated code, produced in module GenConstEst, 
     during a previous analysis of Ldl0.  This is useful in doing cyclic checks 
     of the bootstrap.    

     But some changes to (handwritten) code that Ldl0MakeEst.m3 IMPORTs, can 
     make it non-compilable, until it has been regenerated, which is normally 
     invoked by LdlBootMain.  

     GenConstEst might also need to be modified, although that should not create
     bootstrap problems.  

     You could manually edit the current Ldl0MakeEst.m3 or temporarily take out
     LdlBootMain's use of it.  

     The best thing would be to put Ldl0MakeEst in a dynamically loaded library,
     and load it only when requested.  Then one could run LdlBootMain without
     -E, to regenerate Ldl0MakeEst, recompile that, then make another run of 
     LdlBootMain with -E.  Depending on the changes made, the old compiled 
     library Ldl0MakeEst could even run, even though it won't compile.   
*)  

; IMPORT Ldl0Scanner 
; IMPORT Ldl0Semantics
; IMPORT LdlSemantics
; IMPORT LRTable 
; IMPORT LRUtils 
; IMPORT ManualAsTrees
; IMPORT MessageCodes 
; IMPORT Messages
; IMPORT Misc 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT ScannerIf
; IMPORT SharedStrings 
; IMPORT Strings 
; IMPORT TreeBrowse 
; IMPORT UnsafeUtils 
; IMPORT Version 
; IMPORT VersionedFiles 
; IMPORT WriteTrv 

; VAR LdlRoot1 : LbeStd . EstRootTyp  
; VAR LdlRoot2 : LbeStd . EstRootTyp  
; CONST EstFileName = "Ldl0Est.pkl" 

; VAR LdlLangInfoRef1 : LdlSemantics . LangInfoRefTyp 
; VAR LdlLangInfoRef2 : LdlSemantics . LangInfoRefTyp 
; CONST SemFileName = "Ldl0Sem.pkl" 
; CONST ParseInfoManFileName = "Ldl0BootParseInfoMan" 
; CONST ParseInfoGenFileName = "Ldl0BootParseInfoGen" 
; CONST Ldl0MakeEstOrigName = "Ldl0MakeEstOrig" 
; CONST Ldl0MakeEstName = "Ldl0MakeEst" 
; CONST FsTreesFileName = "Ldl0BootFsTrees.dump"

; VAR DoDisplayHelp := FALSE 
; VAR DoDisplayVersion := FALSE
; VAR DoPickles := FALSE  
; VAR DoGenGrammar := FALSE   
; VAR DoFactorGenGrammar := FALSE   
; VAR DoWriteManPrecAndAssoc := FALSE 
; VAR DoWriteGenPrecAndAssoc := FALSE 
; VAR DoWriteManProductions := FALSE 
; VAR DoWriteGenProductions := FALSE 
; VAR DoWriteManStates := FALSE 
; VAR DoWriteGenStates := FALSE 
; VAR DoWriteLdlText := FALSE 
; VAR DoWriteLdlTok := FALSE 
; VAR DoWriteLdlChild := FALSE 
; VAR DoWriteInitTokStrings := FALSE 
; VAR DoWriteOrigConstEst := FALSE 
; VAR DoWriteReparsedConstEst := FALSE 
; VAR DoWriteFsTrees := FALSE 
; VAR DoReparse := FALSE   
; VAR UseGenGrammar := FALSE   
; VAR DoReanalyze := FALSE   
; VAR DoBrowse := FALSE   
; VAR DoPrintSizes := FALSE   
; VAR DoPrintStats := FALSE   
; VAR UseGeneratedEst := FALSE 
; VAR InsertNilFixedChildren := FALSE 

; PROCEDURE DL ( Msg : TEXT ) 

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN 
      Wr . PutText ( Stdio . stderr , Msg & Wr . EOL ) 
    END DL  

; PROCEDURE HasErrors ( ) : BOOLEAN 

  = BEGIN 
      RETURN 
        Messages . MessageCount ( MessageCodes . KindTyp . MkError ) > 0 
        OR Messages . MessageCount ( MessageCodes . KindTyp . MkFatal ) > 0 
        OR Messages . MessageCount ( MessageCodes . KindTyp . MkAssert ) > 0 
    END HasErrors 

; PROCEDURE GetParams ( ) 

  = VAR LParam : TEXT 
  ; VAR LChar : CHAR 

  ; BEGIN 
      FOR RParamNo := 1 TO Params . Count - 1 
      DO
        LParam := Params . Get ( RParamNo ) 
      ; IF Text . GetChar ( LParam , 0 ) = '-' 
        THEN
          FOR RCharNo := 1 TO Text . Length ( LParam ) - 1  
          DO
            LChar := Text . GetChar ( LParam , RCharNo ) 
          ; CASE LChar 
            OF 'a' 
            => DoReparse := TRUE  
            ;  InsertNilFixedChildren := TRUE 
            | 'A' => DoReparse := TRUE  
            | 'b' => DoBrowse := TRUE  
            | 'c' => DoWriteLdlChild := TRUE 
            | 'd' => DoWriteReparsedConstEst := TRUE 
            | 'e' => DoWriteOrigConstEst := TRUE 
            | 'E' => UseGeneratedEst := TRUE 
            | 'f' => DoWriteFsTrees := TRUE 
            | 'g' => DoFactorGenGrammar := TRUE 
            | 'G' => DoGenGrammar := TRUE 
            | 'h' => DoDisplayHelp := TRUE 
            | 'i' => DoWriteInitTokStrings := TRUE 
            | 'j' => DoPrintSizes := TRUE 
            | 'k' => DoPrintStats := TRUE 
            | 'l' => DoWriteLdlText := TRUE 
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
            | 'z' => DoReanalyze := TRUE 
            ELSE 
              DL ( "Invalid option character: \'" 
                   & Text . FromChar ( LChar )  & "\'"  
                 ) 
            ; DoDisplayHelp := TRUE 
            END 
          END (* FOR *) 
        ELSE
          DL ( "Invalid parameter: \"" & LParam & "\"" ) 
        ; DoDisplayHelp := TRUE 
        END (* IF *)  
      END (* FOR *)  
    ; DoGenGrammar 
        := DoGenGrammar 
           OR DoWriteGenPrecAndAssoc 
           OR DoWriteGenProductions 
           OR DoWriteGenStates 
           OR DoFactorGenGrammar 
    ; Options . PreferGeneratedGrammar := UseGenGrammar 
    ; Options . SetDerivedDebugOptions ( ) 
    END GetParams 

; PROCEDURE DisplayHelp ( ) 

  = BEGIN 
      DL ( "LdlBoot program: Schutz semantic editor bootstrap" ) 
    ; DL ( "Usage: LdlBoot {-{option}}" ) 
    ; DL ( "  Options are: " ) 
    ; DL ( "  -a Reparse text file with NIL fixed children." ) 
    ; DL ( "  -A Reparse text file without NIL fixed children." ) 
    ; DL ( "  -b Invoke interactive tree browser." ) 
    ; DL ( "  -c Write interface Ldl0Child." ) 
    ; DL ( "  -d Write module " & Ldl0MakeEstName 
                 & ".m3 from reparsed Est (Only if -a supplied) ." 
         ) 
    ; DL ( "  -e Write module " & Ldl0MakeEstOrigName & " from used Est." ) 
    ; DL ( "  -E Use generated Est from module " & Ldl0MakeEstName & ".m3." ) 
    ; DL ( "  -f Write format syntax trees dump file " & FsTreesFileName ) 
    ; DL ( "  -g Factor generated grammar (implies -G).") 
    ; DL ( "  -G Generate a concrete grammar." )
    ; DL ( "  -h Display version and help text and exit." ) 
    ; DL ( "  -i Write module Ldl0InitTokStrings." ) 
    ; DL ( "  -j Display sizes of various heap node types." ) 
    ; DL ( "  -k Display language statistics." ) 
    ; DL ( "  -l Write Ldl text files." ) 
    ; DL ( "  -p Write pickle files." ) 
    ; DL ( "  -q Write manual precedence and associativities to file " 
                 & ParseInfoManFileName & "." 
         ) 
    ; DL ( "  -Q Write generated precedence and associativities to file " 
                 & ParseInfoGenFileName & " (implies -G)." 
         ) 
    ; DL ( "  -r Write manual productions to file " & ParseInfoManFileName 
                 & "." 
         ) 
    ; DL ( "  -R Write generated productions to file " & ParseInfoGenFileName 
                 & " (implies -G)." 
         ) 
    ; DL ( "  -s Write manual LR states to file " & ParseInfoManFileName 
                 & "." 
         )
    ; DL ( "  -S Write generated LR states to file " & ParseInfoGenFileName 
                 & " (implies -G)." 
         )
    ; DL ( "  -t Write interface Ldl0Tok." ) 
    ; DL ( "  -u Use generated concrete grammar for reparsing, if available." )
    ; DL ( "  -v Display version and exit." ) 
    ; DL ( "  -z Reanalyze (only if -a or -A, analyzing as appropriate)." ) 
    END DisplayHelp 

; PROCEDURE DisplayVersion ( ) 

  = BEGIN 
      DL ( "Schutz Semantic Editor, Ldl bootstrap program," ) 
    ; DL ( "version " 
           & Version . VersionString 
           & " " 
           & Version . DateString 
         ) 
    END DisplayVersion  

; PROCEDURE WritePkl 
    ( Root : REFANY 
    ; FileName : TEXT 
    ; Label : TEXT 
    ; InfoRef : LbeStd . PickleIdInfoRefTyp 
    ) 

  = VAR Writer : Wr . T 

  ; BEGIN
      TRY
        Writer := VersionedFiles . OpenWrite ( FileName ) 
      EXCEPT 
        VersionedFiles . Error ( EMessage ) 
        => DL ( EMessage & "while trying to open " & Label 
                &  " \"" & FileName & "\" for writing." 
              ) 
      ; Failures . ExitAfterTerminate ( ) <* NORETURN *> 
      ELSE 
        DL ( "Unable to open " & Label 
             & " \"" & FileName & "\" for writing." 
           ) 
      ; Failures . ExitAfterTerminate ( ) <* NORETURN *> 
      END (* TRY EXCEPT *) 
    ; TRY 
        Pickle . Write ( Writer , InfoRef ) 
      ; Pickle . Write ( Writer , Root ) 
      ; Wr . Close ( Writer ) 
      ; DL ( "Wrote " & Label & " \"" & FileName & "\"" ) 
      EXCEPT 
      Pickle . Error ( EMessage )
      => DL ( "Exception raised while writing " & Label 
              & " \"" & FileName & "\":" 
            ) 
      ; DL ( "  ConvertPacking.Error(" & EMessage & ")" ) 
      ; Failures . ExitAfterTerminate ( ) <* NORETURN *> 
      ELSE 
        DL ( "Unable to write " & Label & " \"" & FileName & "\"" ) 
      ; Failures . ExitAfterTerminate ( ) <* NORETURN *> 
      END (* TRY EXCEPT *) 
    END WritePkl 

; PROCEDURE MaybeBrowse 
    ( EstRoot : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull  
    ; TreeId : TEXT := "" 
    ) 

  = <* FATAL Backout *> 
    VAR LSession : TreeBrowse . T 

  ; BEGIN
      IF DoBrowse 
      THEN 
        LSession 
          := NEW ( TreeBrowse . T ) . initStdSession ( EstRoot , Lang ) 
      ; DL ( "Browsing Est:" & TreeId ) 
      ; TreeBrowse . Interp ( LSession ) 
      END (* IF *) 
    END MaybeBrowse  

; CONST LangLdl0 = LbeStd . LangLdl0 

; VAR LdlTextWr : Wr . T 
; CONST LdlTextFileName1 = "Ldl0.ldl0"
; CONST LdlTextFileName2 = "Ldl0-2.ldl0"

(* TODO: WriteProc and MaybeWriteText more or less duplicate code in
         Ui for exporting.  Can they be combined?. *) 
; PROCEDURE WriteProc 
    ( <* UNUSED *> ImageRef : PaintHs . ImageTransientTyp 
    ; String : Strings . StringTyp 
    )  

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    <* FATAL Backout *> 
    BEGIN
      Wr . PutText ( LdlTextWr , Strings . ToTextNonNIL ( String ) ) 
    ; Wr . PutText ( LdlTextWr , Wr . EOL ) 
    ; Wr . Flush ( LdlTextWr ) 
    END WriteProc 

; PROCEDURE MaybeWriteText 
    ( EstRoot : LbeStd . EstRootTyp ; FileName : TEXT ; Label : TEXT ) 

  = VAR LImageRef : PaintHs . ImageTransientTyp 

  ; BEGIN
      IF DoWriteLdlText 
      THEN 
        LImageRef := NEW ( PaintHs . ImageTransientTyp ) . initDefaults ( ) 
      ; LImageRef . ItPers 
          := NEW ( PaintHs . ImagePersistentTyp ) . initDefaults ( ) 
      ; LImageRef . ItPers . IpEstRoot := EstRoot 
      ; LImageRef . ItPers . IpLang := LangLdl0  
      ; LImageRef . ItLangIdRef := LangUtil . LangIdRef ( LangLdl0 ) 
      ; TRY 
          LdlTextWr := VersionedFiles . OpenWrite ( FileName ) 
        ; WriteTrv . WriteText 
            ( LImageRef , WriteProc , DoGenerateText := TRUE ) 
        ; Wr . Close ( LdlTextWr ) 
        ; DL ( "Wrote " & Label & " \"" & FileName & "\"" ) 
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
    END MaybeWriteText 

; PROCEDURE WriteParseInfo 
    ( FileName : TEXT 
    ; Gram : LRTable . GrammarTyp 
    ; DoWritePrecAndAssoc : BOOLEAN 
    ; DoWriteProductions : BOOLEAN 
    ; DoWriteStates : BOOLEAN 
    ) 

  = VAR LWrT : Wr . T 

  ; BEGIN 
      IF DoWriteProductions OR DoWriteStates 
      THEN
        TRY 
          LWrT := VersionedFiles . OpenWrite ( FileName )
        ; IF DoWritePrecAndAssoc  
          THEN 
            Infos . WritePrecAndAssocList ( Gram , LWrT ) 
          ; DL ( "Wrote precedences and associativites to file \"" 
                 & FileName & "\"" 
               ) 
          END 
        ; IF DoWriteProductions 
          THEN 
            Infos . WriteProductions ( Gram , LWrT ) 
          ; DL ( "Wrote productions to file \"" & FileName & "\"" ) 
          END 
        ; IF DoWriteStates 
          THEN 
            Infos . WriteStates ( Gram , LWrT ) 
          ; DL ( "Wrote states to file \"" & FileName & "\"" ) 
          END 
        ; Wr . Close ( LWrT ) 
        EXCEPT  
        VersionedFiles . Error ( EMessage ) 
        => DL ( EMessage & Wr . EOL 
                & ",  while opening parse info file \"" & FileName & "\"" 
              ) 
        ELSE
          DL ( "Unable to write productions/states to file \"" 
               & FileName & "\"" 
             ) 
        END 
      END 
    END WriteParseInfo 

; PROCEDURE WriteFsTrees ( FileName : TEXT ) 

  = VAR LWrT : Wr . T 

  ; BEGIN 
      IF DoWriteFsTrees 
      THEN
        TRY 
          LWrT := VersionedFiles . OpenWrite ( FileName ) 
        ; LdlSemantics . DumpAllFsTreesStream ( LWrT , LangLdl0 ) 
        ; Wr . Close ( LWrT ) 
        ; DL ( "Format syntax trees written to file \"" & FileName & "\"" ) 
        EXCEPT 
          VersionedFiles . Error ( EMessage ) 
          => DL ( EMessage 
                  & "while trying to write formax syntax tree file \"" 
                  & FileName & "\"" 
                ) 
        ELSE 
          DL ( "Unable to write format syntax tree file \"" & FileName & "\"" ) 
        END (* TRY EXCEPT *) 
      END 
    END WriteFsTrees 

; PROCEDURE EstSourceMsg ( ) : TEXT 

  = BEGIN 
      IF UseGeneratedEst 
      THEN
        RETURN Ldl0MakeEstName & ".m3" 
      ELSE
        RETURN "ManualAsTrees.m3" 
      END (* IF *) 
    END EstSourceMsg

; PROCEDURE BuildEst ( ) : LbeStd . EstRootTyp 

  = <* FATAL Backout *> 
    BEGIN 
      IF UseGeneratedEst 
      THEN
        RETURN Ldl0MakeEst . Root ( ) 
      ELSE
        RETURN ManualAsTrees . LanguageDefinition ( ) 
      END (* IF *) 
    END BuildEst 

; VAR BogusSs : SharedStrings . T 

; PROCEDURE SetSuffixes ( LangInfo : LdlSemantics . LangInfoRefTyp ) 

  = BEGIN 
      LangInfo . Suffixes 
        := NEW ( LdlSemantics . SuffixListRefTyp , 1 ) 
    ; LangInfo . Suffixes ^ [ 0 ] 
        := LdlSemantics . SuffixPairTyp 
             { Tok := LangInfo . StartTok 
             , Suffix := "ldl0" 
             } 
    END SetSuffixes 

; PROCEDURE Work ( ) 
  RAISES { Backout }  

  = <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    <* FATAL OSError . E  *> 
    <* FATAL LangUtil . DuplicateSuffix *> 
    VAR LStats1 : EstUtil . StatisticsTyp 
  ; VAR LScanIf : ScannerIf . ScanIfTyp 
  ; VAR LdlTextRd : Rd . T 
  ; VAR LLang : LbeStd . LangTyp 
  ; VAR LHasSyntErrors : BOOLEAN 
  ; VAR LHasSemanticErrors : BOOLEAN 
  ; VAR LMsg : TEXT 

  ; BEGIN 

    (* Build Est for Ldl0 in Ldl0. *) 
      LdlRoot1 := BuildEst ( )

    (* Display info on built Est. *) 
    ; IF DoPrintSizes 
      THEN 
        Boot . PrintSizes ( Stdio . stderr ) 
      END (* IF *) 
    ; EstUtil . Statistics ( LdlRoot1 , LStats1 ) 
    ; IF DoPrintStats 
      THEN 
        Boot . PrintStats ( Stdio . stderr , LStats1 ) 
      END (* IF *) 
    ; DL ( "Ldl0 AST built from " & EstSourceMsg ( ) & ", " 
           & Fmt . Int ( EstUtil . EstNodeCt ( LdlRoot1 ) ) & " nodes." 
         ) 
    ; MaybeBrowse ( LdlRoot1 , TreeId := "AST for Ldl0: " ) 

    (* Semantics on Est for Ldl0 in Ldl0. *) 
    ; DL ( "Analyzing -----------------------------------------------" ) 
    ; LdlLangInfoRef1 := Ldl0Semantics . Analyze ( LdlRoot1 ) 
    ; LHasSemanticErrors := HasErrors ( ) 
    ; DL ( "Analysis completed --------------------------------------" ) 
    ; LangMap . AddOrChange ( LangLdl0 , LdlLangInfoRef1 ) 

    ; BogusSs 
        := SharedStrings . FromText 
             ( "A bogus shared string to test that pickling loses it" ) 
    ; DL ( "Ldl analyzed:" ) 
    ; DL ( LangUtil . LangIdImage ( LdlLangInfoRef1 ^ . DefLangIdRef ^ , 2 ) ) 
    ; DL ( "Unique shared strings: " 
           & Fmt . Int ( SharedStrings . UniqueStringCt ( ) ) 
         ) 

    ; MaybeBrowse ( LdlRoot1 , TreeId := "Analyzed Ldl0: " ) 

    ; LangUtil . MergeSuffixes ( LangLdl0 ) 
 (* ; SetSuffixes ( LdlLangInfoRef1 ) *) 

    (* Write text of Ldl0 in Ldl0. *)  
    ; MaybeWriteText ( LdlRoot1 , LdlTextFileName1 , "Ldl0 text file" ) 

    (* Write interface Ldl0Tok. *) 
    ; IF DoWriteLdlTok 
      THEN 
        LdlSemantics . WriteLdlTokInterfaceForName 
          ( LdlLangInfoRef1 , "Ldl0" )  
      END (* IF *) 

    (* Write interface LdlChild. *) 
    ; IF DoWriteLdlChild 
      THEN
        LdlSemantics . WriteLdlChildInterfaceForName 
          ( LdlLangInfoRef1 , "Ldl0" )  
      END (* IF *) 

    (* Write Module Ldl0InitTokStrings. *) 
    ; IF DoWriteInitTokStrings 
      THEN 
        LdlSemantics . WriteInitTokStringsModuleForName 
          ( LdlLangInfoRef1 , "Ldl0" )  
      END (* IF *) 

    ; WriteFsTrees ( FsTreesFileName ) 

    (* Generate Ldl0MakeEstOrig from whichever compiled-in tree was used *) 
    ; IF DoWriteOrigConstEst 
      THEN
        GenConstEst . WriteName
          ( Lang := LangLdl0
          , EstRoot := LdlRoot1 
          , LangInfoRef := LdlLangInfoRef1  
          , UseLdlTok := TRUE 
          , ModuleName := Ldl0MakeEstOrigName  
          ) 
      END (* IF *) 

(* Once, I vainly hoped doing this twice would get the Width info
   properly computed, but, alas, only parsing puts in the insertion
   tokens.   
    ; Messages . ZeroMessageCounts ( ) 
    ; LdlRoot2 := BuildEst ( ) 
    ; EstUtil . Statistics ( LdlRoot2 , LStats2 ) 
    ; IF DoPrintStats 
      THEN 
        Boot . PrintStats ( Stdio . stderr , LStats2 ) 
      END (*IF *) 
    ; DL ( "Ldl AST built, 2nd time." 
           & Fmt . Int ( EstUtil . EstNodeCt ( LdlRoot2 ) ) & " nodes." 
         ) 
    ; IF DoPickles 
      THEN 
        WritePkl 
          ( LdlRoot2 
          , EstFileName 
          , "second Est pickle file" 
          , LbeStd . PickleKindTyp . Est 
          ) 
      END (* IF *) 
    ; DL ( "Analyzing -----------------------------------------------" ) 
    ; LdlLangInfoRef2 := Ldl0Semantics . Analyze1 ( LdlRoot2 ) 
    ; DL ( "Analysis completed --------------------------------------" ) 
    ; IF DoGenGrammar
      THEN
        DL ( "Generating concrete grammar -----------------------------" ) 
      ; IF DoFactorGenGrammar 
        THEN DL ( "Generated grammar will be factored." ) 
        ELSE DL ( "Generated grammar will not be factored." ) 
        END (* IF *) 
      ; GrammarGen . Generate 
          ( Ldl1LangInfoRef2 , Factored := DoFactorGenGrammar ) 
      ; DL ( "Grammar Generation complete -----------------------------" ) 
      END (* IF *) 
    ; DL ( "Ldl analyzed, 2nd time." ) 

    ; MaybeBrowse ( LdlRoot2 , LangLdl0 , TreeId := "Second analyzed: " ) 

    ; MaybeWriteText ( LdlRoot2 , LdlTextFileName2 , "Second Ldl text file" ) 
    ; IF HasErrors ( ) 
      THEN 
        DL ( "Second analysis failed" ) 
      ; Failures . ExitAfterTerminate ( ) <* NORETURN *> 
      END (* IF *) 
    ; SetSuffixes ( LdlLangInfoRef ) 
    ; LangMap . AddOrChange ( LangLdl0 , LdlLangInfoRef2 ) 
*) 

    (* Do LR Generation for handwritten grammar. *) 
    ; DL ( "Doing LR generation for handwritten grammar -----------" ) 
    ; LdlSemantics . ManLRGen ( LdlLangInfoRef1 ) 
    ; DL ( "LR generation for handwritten grammar complete --------" ) 

    (* Generate concrete grammar. *) 
    ; IF DoGenGrammar
      THEN
        DL ( "Generating concrete grammar -----------------------------" ) 
      ; IF DoFactorGenGrammar 
        THEN DL ( "Generated grammar will be factored." ) 
        ELSE DL ( "Generated grammar will not be factored." ) 
        END (* IF *) 
      ; GrammarGen . Generate 
          ( LdlLangInfoRef1 , Factored := DoFactorGenGrammar ) 
      ; DL ( "Grammar Generation complete -----------------------------" ) 
      END (* IF *) 

    (* Write parsing information. *) 
    ; WriteParseInfo 
        ( ParseInfoManFileName 
        , LdlLangInfoRef1 ^ . Gram
        , DoWriteManPrecAndAssoc 
        , DoWriteManProductions
        , DoWriteManStates  
        ) 
    ; WriteParseInfo 
        ( ParseInfoGenFileName 
        , LdlLangInfoRef1 ^ . GenGram
        , DoWriteGenPrecAndAssoc 
        , DoWriteGenProductions
        , DoWriteGenStates  
        ) 

    (* Write pickles of Ldl in Ldl. *)  
    ; IF DoPickles 
      THEN 
        WritePkl 
          ( LdlRoot1 
          , EstFileName 
          , "Est pickle file" 
          , LbeStd . EstPickleIdInfoRef 
          ) 
      ; LRUtils . StripGrammar ( LdlLangInfoRef1 . Gram ) 
      ; LRUtils . StripGrammar ( LdlLangInfoRef1 . GenGram ) 
      ; WritePkl 
          ( LdlLangInfoRef1 
          , SemFileName 
          , "language pickle file" 
          , LbeStd . LangPickleIdInfoRef 
          )
      ; DL ( "For:" ) 
      ; DL 
          ( LangUtil . LangIdImage ( LdlLangInfoRef1 ^ . DefLangIdRef ^ , 2 ) )
      END (* IF *) 

    (* Reparse the text file of Ldl0 in Ldl0 that was written earlier. *) 
    ; IF DoReparse 
      THEN 
        Wr . PutText   
          ( Stdio . stderr , "Reparsing text file " & LdlTextFileName1 ) 
      ; LScanIf 
          := SchutzCoroutine . Init 
               ( NEW ( ScannerIf . ScanIfTyp )
               , Ldl0Scanner . Scan 
               ) 
      ; LdlTextRd := FileRd . Open ( LdlTextFileName1 ) 
      ; Files . ParseTextRdT 
          ( LangLdl0 
          , LScanIf 
          , File := LdlTextRd 
          , NewTreeRef := LdlRoot2 
          , InsertNilFixedChildren := InsertNilFixedChildren  
          )   
      ; LHasSyntErrors := EstUtil . HasSyntErrors ( LdlRoot2 ) 
      ; Wr . PutText   
          ( Stdio . stderr 
          , "Text file " & LdlTextFileName1 & " reparsed "  
          ) 
      ; IF LHasSyntErrors 
        THEN 
          DL ( "with errors." ) 
        ELSE 
          DL ( "without errors." ) 
        END (* IF *) 
      ; MaybeBrowse ( LdlRoot2 , LangLdl0 , TreeId := "Reparsed tree: " ) 
      ; IF LHasSyntErrors 
        THEN 
          Failures . ExitAfterTerminate ( ) <* NORETURN *> 
        END (* IF *) 

      (* Write text file from reparsed Ldl in Ldl. *)  
      ; MaybeWriteText 
          ( LdlRoot2 , LdlTextFileName2 , "Ldl0 text file after reparse" )

      (* Generate Ldl0MakeEst from reparsed tree. *) 
      ; IF DoWriteReparsedConstEst 
        THEN
          GenConstEst . WriteName
            ( Lang := LangLdl0
            , EstRoot := LdlRoot2 
            , LangInfoRef := LdlLangInfoRef1  
            , UseLdlTok := TRUE 
            , ModuleName := Ldl0MakeEstName  
            ) 
        END (* IF *) 

      (* Renanalize. *) 
      ; IF DoReanalyze 
        THEN 
          IF InsertNilFixedChildren 
          THEN LLang := LbeStd . LangNull 
          ; LMsg := "absent" 
          ELSE LLang := LangLdl0 
          ; LMsg := "present" 
          END 
        ; DL ( "Analyzing, 2nd time -------------------------------------" ) 
        ; LdlLangInfoRef2 := Ldl0Semantics . Analyze ( LdlRoot1 , LLang ) 
        ; DL ( "Analysis completed, 2nd time ----------------------------" ) 

        (* Do LR Generation for handwritten grammar. *) 
        ; DL ( "Doing LR generation for handwritten grammar, 2nd time -" ) 
        ; LdlSemantics . ManLRGen ( LdlLangInfoRef1 ) 
        ; DL ( "LR generation for handwritten grammar complete, 2nd time" ) 

        (* Regenerate concrete grammar. *) 
        ; IF DoGenGrammar
          THEN
            DL ( "Generating concrete grammar, 2nd time -------------------" ) 
          ; IF DoFactorGenGrammar 
            THEN DL ( "Generated grammar will be factored." ) 
            ELSE DL ( "Generated grammar will not be factored." ) 
            END (* IF *) 
          ; GrammarGen . Generate 
              ( LdlLangInfoRef2 , Factored := DoFactorGenGrammar ) 
          ; DL ( "Grammar Generation complete, 2nd time -------------------" ) 
          END (* IF *) 

        ; DL ( "Ldl reanalyzed, assuming NIL fixed children are" & LMsg ) 
        ; DL ( LangUtil . LangIdImage ( LdlLangInfoRef2 ^ . DefLangIdRef ^ , 2 )
             ) 
        ; IF HasErrors ( ) 
          THEN 
            DL ( "Reanalysis failed" ) 
          ; Failures . ExitAfterTerminate ( ) <* NORETURN *> 
          END (* IF *) 
        ; SetSuffixes ( LdlLangInfoRef1 ) 
        END (* IF *) 
      END (* IF *) 
    END Work 

; BEGIN
    Misc . LoadYourself ( )
    (* ^Load libschutz right away, so m3gdb can set breakpoints therein. *)
  ; GetParams ( ) 
  ; IF DoDisplayHelp 
    THEN
      DisplayVersion ( ) 
    ; DisplayHelp ( )
    ELSIF DoDisplayVersion 
    THEN DisplayVersion ( ) 
    ELSE 
      TRY  
        Work ( )
      EXCEPT 
      ELSE (* Including Failures . Terminate. *)  
        RTIO . PutText ( "LbeBoot, Unhandled exception: " ) 
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
    ; Failures . ExitAfterTerminate ( ) 
    END (* IF *) 
  END LdlBootMain
.
 
