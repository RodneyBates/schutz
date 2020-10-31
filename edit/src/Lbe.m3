
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE Lbe 

EXPORTS Main 

; IMPORT Env 
; IMPORT FloatMode 
; IMPORT Fmt 
; IMPORT Lex 
; IMPORT Params 
; IMPORT Rd 
; IMPORT RefList 
; IMPORT Rsrc 
; IMPORT RTProcess 
; IMPORT Stdio 
; IMPORT Text 
; IMPORT TextRd 
; IMPORT Thread  
; IMPORT Wr 

; IMPORT AssertDevel  
; IMPORT Assertions 
; FROM Assertions IMPORT AssertionFailure 
; IMPORT Files
; IMPORT GrammarGen    (* Not "used", but types declared within 
                                       are needed by pickles. *)
; IMPORT LangMap 
; IMPORT LangUtil 
; IMPORT LbeBundle 
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
; IMPORT Messages  
; IMPORT M3Bundle 
; IMPORT M3Scanner     (* Not "used", but must register itself. *)
; IMPORT Options 
; IMPORT PortTypes 
; IMPORT RecPlayBundle
; IMPORT SharedStrings 
; IMPORT TreeBrowse 
; IMPORT Ui 
; IMPORT UiDevel  
; IMPORT Version 
; IMPORT Worker 

; CONST Ldl0SemFileName = "Ldl0Sem.pkl" 
; CONST Ldl1SemFileName = "Ldl1Sem.pkl" 
; CONST M3SemFileName = "M3Sem.pkl" 
; CONST DefaultDebugLevel = 3 

; VAR ResourceDirDefault : TEXT := "/home/rodney/proj/lbe/git/resources"  
; VAR ResourceDir : TEXT 
; VAR PlaybackFileName : TEXT 
; VAR RecordFileName : TEXT 
; VAR DelayTime : INTEGER := FIRST ( INTEGER ) 
; VAR DoRunPlayback : BOOLEAN 
; VAR RespectStops : BOOLEAN 
; VAR Ldl0LangInfoRef : LdlSemantics . LangInfoRefTyp 
; VAR Ldl1LangInfoRef : LdlSemantics . LangInfoRefTyp 
; VAR M3LangInfoRef : LdlSemantics . LangInfoRefTyp 

; CONST DL = Messages . StdErrLine 

; PROCEDURE Browse 
    ( EstRoot : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ; TreeId : TEXT := "" 
    ) 

  = VAR LSession 
      := NEW ( TreeBrowse . T ) . initStdSession ( EstRoot , Lang ) 

  ; BEGIN (* Browse *) 
      TRY 
        Wr . PutText ( Stdio . stderr , "Browsing Est:" & TreeId & Wr . EOL ) 
      ; TreeBrowse . Interp ( LSession ) 
      EXCEPT
        AssertionFailure 
      => DL ( "Tree browse suffered an assertion failure." ) 
      | Thread . Alerted 
      , Wr . Failure 
      => DL ( "Tree browse unable to write." ) 
      END (* TRY EXCEPT *) 
    END Browse 

; PROCEDURE DisplayVersion ( ) 

  = BEGIN (* DisplayVersion *) 
      DL 
        ( LbeStd . AppName 
          & " Semantic Editor, version " 
          & Version . VersionString 
          & " " 
          & Version . DateString 
        ) 
    END DisplayVersion 

; PROCEDURE DisplayHelp ( ) 

  = BEGIN (* DisplayHelp *) 
      DL ( "Usage: " & LbeStd . CommandName & " [options] [file] " ) 
    ; DL ( "" ) 
    ; DL ( "  options: " ) 
    ; DL ( "    -v                      " ) 
    ; DL ( "    --version                :Display version and exit." ) 
    ; DL ( "    -h                      " ) 
    ; DL ( "    --help                   :Display help text and exit." ) 
    ; DL ( "    --crash                  :Make runtime errors crash normally." ) 
    ; DL ( "    -d <display>            " ) 
    ; DL ( "    --display <display>      :Display on <display>." ) 
    ; DL ( "    -g <geometry>     " ) 
    ; DL ( "    --geometry <geometry>    :Use geometry <geometry>." ) 
    ; DL ( "    -r <resdir> " ) 
    ; DL ( "    --resource <resdir>      :Get resources from directory <resdir>." 
         ) 
    ; DL ( "                              (default: " & ResourceDirDefault ) 
    ; DL ( "    -b <integer>" ) 
    ; DL ( "    --debug <integer>        :Set debug level ("
                    & Fmt . Int ( FIRST ( Options . DebugLevelTyp ) ) 
                    & ".." 
                    & Fmt . Int ( LAST ( Options . DebugLevelTyp ) ) 
                    & ") (default "
                    & Fmt . Int ( DefaultDebugLevel ) 
                    & ")" 
         ) 
    ; DL ( "    -o                      " ) 
    ; DL ( "    --optsingletonlists      :Optimize singleton lists, if possible." ) 
    ; DL ( "    -p <pbfile>             " ) 
    ; DL ( "    --playback <pbfile>      :Open playback file <pbfile>." ) 
    ; DL ( "    -P <pbfile>             " ) 
    ; DL ( "    --Playback <pbfile>      :Open and run playback file <pbfile>." ) 
    ; DL ( "    --record <rcfile>        :Open and write new playback file <rcfile>." ) 
    ; DL ( "                                May be the same file name as <pbfile>." ) 
    ; DL ( "    -t <integer>            " ) 
    ; DL ( "    --time <integer>         :Set playback delay to <integer>." ) 
    ; DL ( "    -u                      " ) 
    ; DL ( "    --use-gen-grammar        :Prefer to parse with generated grammar." ) 
    ; DL ( "    -i                      " ) 
    ; DL ( "    --ignorestops            :Ignore playback STOP commands," ) 
    ; DL ( "                              and don't stop on RT/Assert failures." ) 
    END DisplayHelp 

; PROCEDURE SetDefaults ( ) 

  = BEGIN (* SetDefaults *) 
      Options . Display := Env . Get ( "TRUE_DISPLAY" ) 
    ; IF Options . Display = NIL 
      THEN 
        Options . Display := Env . Get ( "DISPLAY" ) 
      ; IF Options . Display = NIL 
        THEN 
          Options . Display := ":0.0" 
        END (* IF *) 
      ; 
      END (* IF *) 
    ; ResourceDir := ResourceDirDefault
    ; PlaybackFileName := "" 
    ; DoRunPlayback := FALSE 
    ; RespectStops := TRUE 
    ; RecordFileName := "" 
    ; Options . EditFileName := "" 
    ; Options . DebugLevel := DefaultDebugLevel 
    ; Options . SetDerivedDebugOptions ( )    
    ; Options . EnablePickleWrite := TRUE  
    ; Options . DoOptimizeSingletonLists := FALSE
    ; Options . Crash := FALSE 
    ; Assertions . Callback := Worker . Failure   
 (* ; Assertions . Callback := AssertDevel . AssertDialogCommandLine *) 
 (* ; Assertions . Callback := Assertions . NeverRaise *) 
    ; AssertDevel . DoStop := TRUE 
    END SetDefaults 

; PROCEDURE GetArgs ( ) : BOOLEAN (* True iff should continue. *) 

  = VAR GaArgNo : INTEGER 
  ; VAR GaBadArgs : BOOLEAN := FALSE 
  ; VAR GaHelp : BOOLEAN := FALSE 
  ; VAR GaVersion : BOOLEAN := FALSE 

  ; PROCEDURE GaNumericArg ( VAR Result : INTEGER ) 

    = VAR LNumString : TEXT 
    ; VAR LRdT : Rd . T 

    ; BEGIN  
        TRY 
          Result := 0 
        ; IF GaArgNo >= Params . Count - 1 
          THEN 
            GaBadArgs := TRUE 
          ELSE 
            LNumString := Params . Get ( GaArgNo + 1 ) 
          ; LRdT := NEW ( TextRd . T ) . init ( LNumString ) 
          ; Result := Lex . Int ( LRdT ) 
          ; IF NOT Rd . EOF ( LRdT ) 
            THEN 
              GaBadArgs := TRUE 
            END (* IF *)  
          ; INC ( GaArgNo , 2 ) 
          END (* IF *)
        EXCEPT Rd . Failure 
        , Thread . Alerted  
        , FloatMode . Trap  
        , Lex . Error 
        => DL ( "Unable to read numeric command line argument." )       
        ; Result := FIRST ( PortTypes . Int32Typ ) 
        END (* TRY EXCEPT *) 
      END GaNumericArg 

  ; BEGIN (* GetArgs *) 
      VAR LInt : INTEGER 

    ; BEGIN (* Block GetArgs *) 
        GaArgNo := 1 
      ; LOOP 
          IF GaArgNo >= Params . Count  
          THEN 
            EXIT 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-v" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--version" ) 
          THEN 
            GaVersion := TRUE 
          ; INC ( GaArgNo ) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-h" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--help" ) 
          THEN 
            GaHelp := TRUE 
          ; INC ( GaArgNo ) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-b" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--debug" ) 
          THEN 
            GaNumericArg ( LInt ) 
          ; Options . DebugLevel 
              := MAX ( FIRST ( Options . DebugLevelTyp ) 
                     , MIN ( LAST ( Options . DebugLevelTyp ) 
                           , LInt 
                           ) 
                     ) 
          ; Options . SetDerivedDebugOptions ( ) 
          ; IF GaBadArgs THEN EXIT END 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "--crash" ) 
          THEN 
            Options . Crash := TRUE 
          ; INC ( GaArgNo ) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-d" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--display" ) 
          THEN 
            IF GaArgNo >= Params . Count - 1 
            THEN 
              GaBadArgs := TRUE 
            ; EXIT 
            ELSE 
              Options . Display := Params . Get ( GaArgNo + 1 ) 
            ; INC ( GaArgNo , 2 ) 
            END (* IF *) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-g" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--geometry" ) 
          THEN 
            IF GaArgNo >= Params . Count - 1 
            THEN 
              GaBadArgs := TRUE 
            ; EXIT 
            ELSE 
              Options . Geometry := Params . Get ( GaArgNo + 1 ) 
            ; INC ( GaArgNo , 2 ) 
            END (* IF *) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-o" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--optsingletonlists" ) 
          THEN 
            Options . DoOptimizeSingletonLists := TRUE 
          ; INC ( GaArgNo ) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-r" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--resource" ) 
          THEN 
            IF GaArgNo >= Params . Count - 1 
            THEN 
              GaBadArgs := TRUE 
            ; EXIT 
            ELSE 
              ResourceDir := Params . Get ( GaArgNo + 1 ) 
            ; INC ( GaArgNo , 2 ) 
            END (* IF *) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-p" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--playback" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "-P" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--Playback" ) 
          THEN 
            IF GaArgNo >= Params . Count - 1 
            THEN 
              GaBadArgs := TRUE 
            ; EXIT 
            ELSE 
              DoRunPlayback 
                := Text . Equal ( Params . Get ( GaArgNo ) , "-P" ) 
                   OR Text . Equal ( Params . Get ( GaArgNo ) , "--Playback" ) 
            ; PlaybackFileName := Params . Get ( GaArgNo + 1 ) 
            ; INC ( GaArgNo , 2 ) 
            END (* IF *) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "--record" ) 
          THEN 
            IF GaArgNo >= Params . Count - 1 
            THEN 
              GaBadArgs := TRUE 
            ; EXIT 
            ELSE 
              RecordFileName := Params . Get ( GaArgNo + 1 ) 
            ; INC ( GaArgNo , 2 ) 
            END (* IF *) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-u" ) 
                OR Text . Equal 
                  ( Params . Get ( GaArgNo ) , "--use-gen-grammar" ) 
          THEN 
            Options . PreferGeneratedGrammar := TRUE 
          ; INC ( GaArgNo ) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-i" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--ignorestops" ) 
          THEN 
            RespectStops := FALSE 
          ; AssertDevel . DoStop := FALSE 
          ; INC ( GaArgNo ) 
          ELSIF Text . Equal ( Params . Get ( GaArgNo ) , "-t" ) 
                OR Text . Equal ( Params . Get ( GaArgNo ) , "--time" ) 
          THEN 
            GaNumericArg ( (* VAR *) DelayTime ) 
          ; IF GaBadArgs THEN EXIT END 
          ELSIF Text . Length ( Params . Get ( GaArgNo ) ) > 0
                AND Text . GetChar ( Params . Get ( GaArgNo ) , 0 ) = '-'  
          THEN 
            GaBadArgs := TRUE 
          ; EXIT 
          ELSE 
            Options . EditFileName := Params . Get ( GaArgNo ) 
          ; INC ( GaArgNo ) 
          END (* IF *) 
        END (* LOOP *) 
      ; IF NOT Options . Crash
        THEN RTProcess . RegisterExitor ( AssertDevel . RuntimeFailureDialog )
        END (* IF *) 
      ; IF GaHelp OR GaBadArgs 
        THEN 
          DisplayVersion ( ) 
        ; DisplayHelp ( ) 
        ; RETURN FALSE 
        ELSIF GaVersion 
        THEN 
          DisplayVersion ( ) 
        ; RETURN FALSE 
        ELSE 
          Options . ResourcePath 
            := RefList . AppendD 
                 ( Rsrc . BuildPath 
                     ( ResourceDir , "$LBERESOURCE" , LbeBundle . Get ( ) ) 
                 , Rsrc . BuildPath 
                     ( Ldl0Bundle . Get ( ) 
                     , Ldl1Bundle . Get ( ) 
                     , M3Bundle . Get ( ) 
                     , RecPlayBundle . Get ( ) 
                     ) 
                 ) 
        ; RETURN TRUE  
        END (* IF *) 
      END (* Block *)
    END GetArgs 

(* TODO: Make manual language loading and tree browsing devel gui functions. *) 
; VAR WantedThreadStackSize := 64000 (* Word.T's *) 
      (* = 256 K Bytes on 32-bit . *)  

; PROCEDURE StackSize ( Wanted : INTEGER)  

  = VAR LOldDefault : INTEGER 
  ; VAR LNewDefault : INTEGER 

  ; BEGIN 
      LOldDefault := Thread . GetDefaultStackSize ( ) 
    ; Thread . MinDefaultStackSize ( Wanted ) 
    ; LNewDefault := Thread . GetDefaultStackSize ( ) 
    ; EVAL LNewDefault (* Place to put a breakpoint. *) 
    END StackSize 

; PROCEDURE Work ( ) 

  = <* FATAL Assertions . AssertionFailure *> 
    BEGIN (* Work *) 
      SetDefaults ( ) 
    ; IF GetArgs ( ) 
      THEN 
(* No longer load languages eagerly. 
        EVAL LangUtil . LoadLanguage ( "ldl0" ) 
      ; EVAL LangUtil . LoadLanguage ( "ldl1" ) 
      ; EVAL LangUtil . LoadLanguage ( "m3" ) 
      ; 
*)
        IF NOT Ui . Install 
                 ( Options . EditFileName 
                 , PlaybackFileName 
                 , DoRunPlayback 
                 , RespectStops 
                 , RecordFileName 
                 , DelayTime 
                 )
        THEN Assertions . TerminatingNormally := TRUE 
        ELSE UiDevel . ShowDebugOptions ( )
        END (* IF *)
      ELSE Assertions . TerminatingNormally := TRUE 
      END (* IF *)
    END Work

; <* UNUSED *> PROCEDURE CauseRuntimeError ( ) 
  RAISES { AssertionFailure } <* NOWARN *>
  (* This is here for use in debugging. *) 

  = BEGIN 
      <* NOWARN *> EVAL VAL ( - 1 , CARDINAL ) 
    END CauseRuntimeError 

; BEGIN (* Lbe *) 
    StackSize ( WantedThreadStackSize ) 
  ; Worker . Init ( ) 
  ; Work ( ) 
  END Lbe 
. 
