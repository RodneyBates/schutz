
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Files 

(* Routines for accessing Schutz-specific files. *) 

; IMPORT Atom
; IMPORT Compiler 
; IMPORT FileRd 
; IMPORT FileWr 
; IMPORT Fmt
; IMPORT FS 
; IMPORT OSError 
(* Except for swapping the strings "Pickle" and "Pickle2 AS Pickle", make no
   changes to the following line, as it is recognized/edited by scripts.
   (see scripts/topickle.sh and scripts/topickle2.sh)
*) 
; IMPORT Pickle2 AS Pickle (* The Pickle2 mess. *)
; IMPORT Rd 
; IMPORT RegularFile  
; IMPORT Rsrc 
; IMPORT TextRd 
; IMPORT TextWr 
; IMPORT Thread
; IMPORT UnsafeUtils 
; IMPORT Wr 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , AssertionFailure  
; IMPORT EstHs 
; IMPORT EstUtil  
; IMPORT LangMap
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT LineNumbers 
; IMPORT MessageCodes 
; IMPORT Messages 
; IMPORT Misc 
; IMPORT Options 
; IMPORT PaintHs 
; IMPORT ParseHs 
; IMPORT Parser 
; IMPORT ParseTrv 
; IMPORT PickleThread 
; IMPORT ScannerIf 
; IMPORT SharedStrings 
; IMPORT Strings 
; IMPORT SuffixInfo 
; IMPORT Version 
; IMPORT VersionedFiles 
; IMPORT WriteTrv 

; TYPE AFT = MessageCodes . T 

; CONST DL = Messages . StdErrLine 

; VAR (* CONST *) FileKindNonexistent := Atom . FromText ("Nonexistant" )  

(* VISIBLE: *) 
; PROCEDURE RegularFileExists 
     ( FileName : TEXT ; CreateIfNot : BOOLEAN := FALSE ) 
  : BOOLEAN (* Existence before any entailed creation. *) 
  RAISES { Thread . Alerted } 

  = <* FATAL Wr . Failure *> 
    <* FATAL OSError . E *> 
    VAR LFileKind : Atom . T 
  ; VAR LWrT : Wr . T 

  ; BEGIN 
      TRY 
        LFileKind := FS . Status ( FileName ) . type  
      EXCEPT 
      OSError . E (* ( EArg ) *)  
      => LFileKind := FileKindNonexistent 
      | Thread . Alerted => RAISE Thread . Alerted 
      ELSE 
        LFileKind := FileKindNonexistent 
      END (* TRY EXCEPT *) 
    ; IF LFileKind = FileKindNonexistent 
      THEN 
        IF CreateIfNot 
        THEN (* Create an empty file. *) 
          LWrT := FileWr . Open ( FileName ) 
        ; Wr . Close ( LWrT ) 
        END (* IF *)
      ; RETURN FALSE  
      ELSIF LFileKind = RegularFile . FileType 
      THEN RETURN TRUE 
      ELSE RETURN FALSE  
      END (* IF *)  
    END RegularFileExists 

; PROCEDURE ReadIdAndPickle 
    ( RdT : Rd . T 
    ; FileName : TEXT (* Just for message texts. *) 
    ; VAR Info : LbeStd . PickleIdInfoRefTyp 
    ; VAR Result : REFANY 
    ) 
  RAISES { Error , Thread . Alerted } 

  = VAR LRefany : REFANY 

  ; BEGIN (* ReadIdAndPickle *) 
      Info := NIL 
    ; Result := NIL 
    ; TRY 
        LRefany := Pickle . Read ( RdT ) 
      EXCEPT 
        Rd . EndOfFile 
      => RAISE 
           Error 
             ( "Premature end of file reading identification of " 
               & LbeStd . AppName & " language file \"" & FileName & "\"" 
             ) 
      | Rd . Failure 
      => RAISE 
           Error 
             ( "Read failure reading identification of " 
               & LbeStd . AppName & " file \"" & FileName & "\"" 
             ) 
      | Pickle . Error ( EMsg ) 
      => RAISE 
           Error 
             ( "Deserializing error ("
               & EMsg 
               & ") while reading identification of " 
               & LbeStd . AppName & " file \"" & FileName & "\"" 
             ) 
      | Thread . Alerted => RAISE Thread . Alerted 
      ELSE
        RAISE  
          Error 
             ( "Error while reading identification of " 
               & LbeStd . AppName & " file \"" & FileName & "\"" 
             ) 
      END (* EXCEPT *) 
    ; TYPECASE LRefany 
      OF NULL 
      => RAISE 
           Error 
             ( "NIL " & LbeStd . AppName & " identification in file \"" 
               & FileName & "\"" 
             ) 
      | LbeStd . PickleIdInfoRefTyp ( TPickleIdInfoRef )  
      => Info := TPickleIdInfoRef  
      ELSE 
        RAISE 
          Error 
            ( "Not " & LbeStd . AppName & " identification in file \"" 
              & FileName & "\"" 
            ) 
      END (* TYPECASE *) 
    ; IF Info ^ . Magic # LbeStd . SchutzMagic 
      THEN
        RAISE 
          Error 
            ( "Wrong " & LbeStd . AppName & " magic number in file \"" 
              & FileName & "\"" 
            )  
      ELSIF Info ^ . DSVersion . Incompatible 
            # Version . DataStructureVersion . Incompatible 
      THEN 
        RAISE 
          Error 
            ( "Incompatible " & LbeStd . AppName & " data version in file \"" 
            & FileName & "\"" 
            )  
      ELSIF Info ^ . DSVersion . UpwardCompatible 
            > Version . DataStructureVersion . UpwardCompatible 
      THEN 
        RAISE 
          Error 
            ( "Too new " & LbeStd . AppName & " data version in file \"" 
            & FileName & "\"" 
            )  
      END (* IF *) 
    ; TRY 
        Result := Pickle . Read ( RdT ) 
      EXCEPT 
        Rd . EndOfFile 
      => RAISE 
           Error 
             ( "Premature end of file reading contents of " 
               & LbeStd . AppName & " language file \"" & FileName & "\"" 
             ) 
      | Rd . Failure 
      => RAISE 
           Error 
             ( "Read failure reading contents of " 
               & LbeStd . AppName & " file \"" & FileName & "\"" 
             ) 
      | Pickle . Error ( TMsg ) 
      => RAISE 
           Error 
             ( "Deserializing error ("
               & TMsg 
               & ") while reading contents of " 
               & LbeStd . AppName & " file \"" & FileName & "\"" 
             ) 
      | Thread . Alerted => RAISE Thread . Alerted 
      ELSE
        RAISE  
          Error 
             ( "Error while reading contents of " 
               & LbeStd . AppName & " file \"" & FileName & "\"" 
             ) 
      END (* EXCEPT *) 
    END ReadIdAndPickle 

(* VISIBLE: *) 
; PROCEDURE ReadLangPickle ( FileName : TEXT ; ResourcePath : Rsrc . Path ) 
  : REFANY RAISES { Error , Thread . Alerted }  

  = <* FATAL Rd . Failure *> 
    VAR Reader : Rd . T 
  ; VAR LPickleIdInfoRef : LbeStd . PickleIdInfoRefTyp 
  ; VAR LResult : REFANY 

  ; BEGIN (* ReadLangPickle *) 
      TRY 
        Reader := Rsrc . Open ( FileName , ResourcePath )
(* TODO: Find a way to get the description of where on ResourcePath
         the Reader was opened, and display it. *)
      EXCEPT Thread . Alerted 
      => RAISE Thread . Alerted 
      ELSE
        RAISE 
          Error  
           ( "Unable to open " & LbeStd . AppName 
              & " language file \"" & FileName & "\"" 
           ) 
      END (* EXCEPT *) 
    ; TRY 
        ReadIdAndPickle 
          ( Reader , FileName , (* VAR *) LPickleIdInfoRef , (* VAR *) LResult )
      FINALLY 
        Rd . Close ( Reader ) 
      END (* FINALLY *) 
    ; IF LPickleIdInfoRef ^ . Kind # LbeStd . PickleKindTyp . Lang 
      THEN 
        RAISE 
          Error
            ( "Not a " & LbeStd . AppName 
              & " language file: \"" & FileName & "\"" 
            ) 
      ELSE 
        RETURN LResult 
      END (* IF *) 
    END ReadLangPickle 

(* VISIBLE: *) 
; PROCEDURE ReadNamedImageFile ( FileName : TEXT ) 
  : PaintHs . ImageTransientTyp 
  RAISES { AssertionFailure , Error , Thread . Alerted } 
  (* Reads an est, image, or checkpoint file.  Builds an ImageRef for an est 
     file.  Does not set IpImageName, ItAbsTextFileName, or ItAbsPickleFileName.
  *) 

  = <* FATAL Rd . Failure *> 
    VAR LRd : Rd . T 
  ; VAR LRefany : REFANY 
  ; VAR LPickleIdInfoRef : LbeStd . PickleIdInfoRefTyp 
  ; VAR LResult : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LSuffixInfo : SuffixInfo . T 
  ; VAR LSuffix : TEXT 
  ; VAR LMsg : TEXT 

  ; BEGIN (* ReadNamedImageFile *) 
      LSuffix := LangUtil . LangSuffixOfFileName ( FileName )  
    ; TRY 
        LangUtil . LoadLanguage ( LSuffix ) 
      EXCEPT LangUtil . LangError ( EMsg ) 
      => LMsg := EMsg & ", needed by image file \"" & FileName & "\"" 
      ; RAISE Error ( LMsg ) 
      END (* TRY EXCEPT *) 
    ; LSuffixInfo := LangUtil . RetSuffixInfo ( LSuffix ) 
    ; TRY 
        LRd := FileRd . Open ( FileName ) 
      EXCEPT Thread . Alerted => RAISE Thread . Alerted 
      ELSE 
        LMsg := "Can't open " & LbeStd . AppName & " file \"" & FileName & "\""
      ; RAISE Error ( LMsg ) 
      END (* EXCEPT *) 
    ; TRY 
        ReadIdAndPickle ( LRd , FileName , LPickleIdInfoRef , LRefany ) 
      FINALLY 
        Rd . Close ( LRd ) 
      END (* TRY FINALLY *)
    ; IF NOT  LPickleIdInfoRef ^ . Kind IN LbeStd . PickleKindSetImageYielding 
      THEN 
        LMsg := "Not a " & LbeStd . AppName & " image file: \"" 
                 & FileName & "\""
      ; RAISE Error ( LMsg ) 
      ELSE  
        TYPECASE LRefany 
        OF PaintHs . ImageTransientTyp ( TImageTransRef ) 
        => RETURN TImageTransRef  

        | PaintHs . ImagePersistentTyp ( TImagePersRef ) 
        => LResult := NEW ( PaintHs . ImageTransientTyp ) . initDefaults ( ) 
        ; LResult . ItPers := TImagePersRef 
        ; Options . OpeningImageRef := LResult   
        ; LResult . ItLangIdRef 
            := LangUtil . LangIdRef ( TImagePersRef . IpLang ) 
        ; LResult . ItHistoryWrT := TextWr . New ( ) 
        ; RETURN LResult  

        | EstHs . KTreeRefTyp ( TEstRef ) 
        => LResult := NEW ( PaintHs . ImageTransientTyp ) . initDefaults ( ) 
        ; LImagePers 
            := NEW ( PaintHs . ImagePersistentTyp ) . initDefaults ( ) 
        ; LResult . ItPers := LImagePers 
        ; Options . OpeningImageRef := LResult 
        ; LImagePers . IpLang := LSuffixInfo . Lang 
        ; LResult . ItLangIdRef := LangUtil . LangIdRef ( LSuffixInfo . Lang ) 
        ; LImagePers . IpLineCtDisplay
            := LineNumbers . LineCtDisplay ( LResult ) 
        ; LImagePers . IpLineCtIsExact := TRUE 
        ; LImagePers . IpIsParsed 
            := NOT EstHs . EstChildKindContainsSyntMod 
                   IN EstUtil . EstChildKindSet ( TEstRef )    
        ; LImagePers . IpIsAnalyzed := FALSE   
        ; LResult . ItIsSaved := TRUE    
        ; LImagePers . IpEstRoot := TEstRef 
        ; LResult . ItHistoryWrT := TextWr . New ( ) 
        ; LImagePers . IpHistoryText := NIL 
        ; RETURN LResult 

        ELSE 
          LMsg := "Unknown serialized type in " 
                  & LbeStd . AppName & " file \"" 
                  & FileName & "\"" 
        ; RAISE Error ( LMsg ) 
        END (* TYPECASE *) 
      END (* IF *) 
    END ReadNamedImageFile 

(* VISIBLE: *) 
; PROCEDURE ParseTextRdT 
    ( Lang : LbeStd . LangTyp 
    ; ScanIf : ScannerIf . ScanIfTyp 
    ; File : Rd . T 
    ; VAR NewTreeRef : LbeStd . EstRootTyp 
    ; InsertNilFixedChildren := FALSE 
    )
  RAISES { AssertionFailure , Thread . Alerted }  

  (* Parse from a file. *) 

  = VAR LParseInfo : ParseHs . ParseInfoTyp 

  ; BEGIN (* ParseTextRdT *) 
      LParseInfo . PiLang := Lang 
    ; LParseInfo . PiScanIf := ScanIf 
    ; LParseInfo . PiGram := LangUtil . Gram ( Lang ) 
    ; LParseInfo . PiInsertNilFixedChildren 
        := InsertNilFixedChildren
    ; LParseInfo . PiOrigTempMarkListRef := NIL 
    ; Messages . TextOnly 
        ( "Parsing -------------------------------------------------" 
        , Kind := MessageCodes . KindTyp . MkInformation 
        ) 
    ; Parser . Parse 
        ( LParseInfo 
        , ParseTrv . InitParseFile ( LParseInfo , File ) 
        , NewTreeRef 
        ) 
    ; IF NewTreeRef = NIL 
         OR EstUtil . HasSyntErrors ( NewTreeRef ) 
      THEN 
        Messages . TextOnly 
          ( "Parse complete, with errors -----------------------------" 
          , Kind := MessageCodes . KindTyp . MkInformation 
          ) 
      ELSE 
        Messages . TextOnly 
          ( "Parse complete, no errors -------------------------------" 
          , Kind := MessageCodes . KindTyp . MkInformation 
          ) 
      END  
    END ParseTextRdT 

(* VISIBLE: *) 
; PROCEDURE OpenEmptyFile ( ImageName : TEXT ) 
  : PaintHs . ImageTransientTyp 
  RAISES { AssertionFailure , Error , Thread . Alerted }  
  (* Does not set IpImageName, ItAbsTextFileName, or ItAbsPickleFileName. 
     Does not write a pickle file. 
  *) 

  = VAR LResult : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LRdT : Rd . T 
  ; VAR LSuffixInfo : SuffixInfo . T 
  ; VAR LScannerIf : ScannerIf . ScanIfTyp 
  ; VAR LSuffix : TEXT 
  ; VAR LMsg : TEXT 

  ; BEGIN
      LSuffix := LangUtil . LangSuffixOfFileName ( ImageName )  
    ; TRY 
        LangUtil . LoadLanguage ( LSuffix ) 
      EXCEPT LangUtil . LangError ( EMsg ) 
      => LMsg := EMsg & ", needed for new file \"" & ImageName & "\"" 
      ; RAISE Error ( LMsg ) 
      END (* TRY EXCEPT *) 
    ; LSuffixInfo := LangUtil . RetSuffixInfo ( LSuffix ) 
    ; IF LSuffixInfo . Lang = LbeStd . LangNull 
      THEN
        LMsg := "Unrecognized file suffix: \"" & LSuffix & "\"" 
      ; RAISE Error ( LMsg ) 
      ELSE 
        LResult := NEW ( PaintHs . ImageTransientTyp ) . initDefaults ( ) 
      ; LImagePers := NEW ( PaintHs . ImagePersistentTyp ) . initDefaults ( )
      ; LResult . ItPers := LImagePers 
      ; Options . OpeningImageRef := LResult 
      ; LImagePers . IpLang := LSuffixInfo . Lang 
      ; LResult . ItLangIdRef := LangUtil . LangIdRef ( LSuffixInfo . Lang ) 
      ; LScannerIf 
          := LangUtil . ScannerIfForLang ( LImagePers . IpLang ) 
   (* ; LResult . ItScannerIf := LScannerIf These things choke Pickle. *)  
      ; LRdT := TextRd . New ( "" ) 
      ; ParseTextRdT 
          ( LSuffixInfo . Lang 
          , LScannerIf  
          , LRdT 
          , (* VAR *) LImagePers . IpEstRoot 
          , InsertNilFixedChildren := Options . InsertNilFixedChildren
          )  
      ; LImagePers . IpLineCtDisplay := LineNumbers . LineCtDisplay ( LResult )
      ; LImagePers . IpLineCtIsExact := TRUE 
      ; LImagePers . IpIsParsed := TRUE  
      ; LImagePers . IpIsAnalyzed := FALSE   
      ; LResult . ItIsSaved := FALSE   
      ; LResult . ItHistoryWrT := TextWr . New ( ) 
      ; LImagePers . IpHistoryText := NIL 
      ; RETURN LResult 
      END (* IF *) 
    END OpenEmptyFile 

; PROCEDURE OpenTextRdT 
     ( RdT : Rd . T ; Lang : LbeStd . LangTyp ; FileName : TEXT ) 
  : PaintHs . ImageTransientTyp 
  RAISES { AssertionFailure , Thread . Alerted } 
  (* Does not set IpImageName. 
     Does not write a pickle file. 
  *) 

  = <* FATAL Wr . Failure *> 
    <* FATAL OSError . E *> 
    VAR LResult : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LLineCtDisplay : LbeStd . LineNoTyp 
  ; VAR LLineCtExport : LbeStd . LineNoTyp 
  ; VAR LScannerIf : ScannerIf . ScanIfTyp 
  ; VAR LWrT : Wr . T 

  ; BEGIN 
      LResult := NEW ( PaintHs . ImageTransientTyp ) . initDefaults ( ) 
    ; LImagePers := NEW ( PaintHs . ImagePersistentTyp ) . initDefaults ( ) 
    ; LResult . ItPers := LImagePers 
    ; LImagePers . IpAbsTextFileName := Misc . AbsFileName ( FileName ) 
    ; LImagePers . IpAbsPklFileName 
        := Misc . PickleName ( LImagePers . IpAbsTextFileName ) 
    ; Options . OpeningImageRef := LResult 
    ; LImagePers . IpLang := Lang 
    ; LResult . ItLangIdRef := LangUtil . LangIdRef ( Lang ) 
    ; LScannerIf 
      := LangUtil . ScannerIfForLang ( LImagePers . IpLang ) 
 (* ; LResult . ItScannerIf := LScannerIf These things choke pickles. *) 
    ; ParseTextRdT 
        ( Lang 
        , LScannerIf 
        , RdT
        , (* VAR *) LImagePers . IpEstRoot 
        , InsertNilFixedChildren := Options . InsertNilFixedChildren
        ) 
    ; LLineCtDisplay := LineNumbers . LineCtDisplay ( LResult ) 
    ; LImagePers . IpLineCtDisplay := LLineCtDisplay 

(* Extra evidence to aid in debugging: *) 
; LWrT := FileWr . Open ( "WriteDisplay" ) 
; LineNumbers . WriteDisplay ( LResult , LWrT ) 
; Wr . Close ( LWrT ) 

    ; Assertions . MessageText 
        ( Fmt . Int ( LLineCtDisplay ) & " Lines displayed in window." ) 
    ; LLineCtExport := LineNumbers . LineCtExport ( LResult ) 
    ; Assertions . MessageText 
        ( Fmt . Int ( LLineCtExport ) & " Lines without syntax corrections." ) 
    ; IF LLineCtDisplay # LLineCtExport 
      THEN 
        Assertions . MessageText
          ( "Proposed syntax error corrections will change line counts." ) 
      END (* IF *) 
    ; LImagePers . IpLineCtIsExact := TRUE 
    ; LImagePers . IpIsParsed := TRUE  
    ; LImagePers . IpIsAnalyzed := FALSE   
    ; LResult . ItIsSaved := FALSE   
    ; LResult . ItHistoryWrT := TextWr . New ( ) 
    ; LImagePers . IpHistoryText := NIL 
    ; RETURN LResult 
    END OpenTextRdT 

(* VISIBLE: *) 
; PROCEDURE OpenNamedTextFile ( FileName : TEXT ) 
  : PaintHs . ImageTransientTyp 
  RAISES { AssertionFailure , Error , Thread . Alerted } 
  (* Does not set IpImageName, ItAbsTextFileName, or ItAbsPickleFileName. 
     Does not write a pickle file. 
  *) 

  = VAR LResult : PaintHs . ImageTransientTyp 
  ; VAR LRdT : Rd . T 
  ; VAR LSuffixInfo : SuffixInfo . T 
  ; VAR LSuffix : TEXT 
  ; VAR LMsg : TEXT
  ; VAR LException : ADDRESS 

  ; BEGIN (* OpenNamedTextFile *) 
      LSuffix := LangUtil . LangSuffixOfFileName ( FileName )  
    ; TRY 
        LangUtil . LoadLanguage ( LSuffix ) 
      EXCEPT LangUtil . LangError ( EMsg ) 
      => LMsg := EMsg & ", needed by text file \"" & FileName & "\"" 
      ; RAISE Error ( LMsg ) 
      END (* TRY EXCEPT *) 
    ; LSuffixInfo := LangUtil . RetSuffixInfo ( LSuffix ) 
    ; IF LSuffixInfo . Lang = LbeStd . LangNull 
      THEN
        LMsg := "Unrecognized file name suffix: \"" & LSuffix & "\"" 
      ; RAISE Error ( LMsg ) 
      ELSE 
        TRY 
          LRdT := FileRd . Open ( FileName ) 
        EXCEPT Thread . Alerted => RAISE Thread . Alerted 
        ELSE 
          LMsg := "Can't open input file " & FileName 
        ; LException := Compiler . ThisException ( ) 
        ; RAISE Error ( LMsg ) 
        END (* EXCEPT *) 
      ; TRY 
          LResult := OpenTextRdT ( LRdT , LSuffixInfo . Lang , FileName ) 
        EXCEPT 
        AssertionFailure ( E ) => RAISE AssertionFailure ( E )  

        | Thread . Alerted => RAISE Thread . Alerted 

        ELSE
          LMsg := "Can't parse input text file " & FileName
        ; LException := Compiler . ThisException ( )
        ; UnsafeUtils . DisplayException ( "OpenNamedTextFile" , LException ) 
        ; RAISE Error ( LMsg ) 
        END (* EXCEPT *) 
(* 
       ; TRY 
          Rd . Close ( LRdT ) 
        EXCEPT Thread . Alerted => RAISE Thread . Alerted 
        ELSE
          LMsg := "Can\'t close input text file " & FileName 
        ; RAISE Error ( )  
        END (* EXCEPT *) 
*) 
      END (* IF *) 
    ; RETURN LResult 
    END OpenNamedTextFile 

(* VISIBLE: *) 
; PROCEDURE WriteImagePickle 
    ( Ref : REFANY 
    ; AbsFileName : TEXT  
    ; DoCreateVersion : BOOLEAN 
    ) 
  RAISES { Error , Thread . Alerted } 

  = VAR LWr : Wr . T 

  ; BEGIN (* WriteImagePickle *) 
      IF DoCreateVersion 
      THEN
        TRY 
          LWr := VersionedFiles . OpenWrite ( AbsFileName ) 
        EXCEPT VersionedFiles . Error ( EMessage ) 
        => RAISE Error ( EMessage ) 
(* TODO: Combine VersionedFiles.Error and Files.Error somewhere, so we 
         don't have to do this ridiculous stuff. *) 
        END (* TRY EXCEPT *) 
      ELSE 
        TRY 
          LWr := FileWr . Open ( AbsFileName ) 
        EXCEPT Thread . Alerted => RAISE Thread . Alerted 
        ELSE 
          RAISE 
            Error 
              ( "Unable to open for write: " & AbsFileName )
        END (* TRY EXCEPT *) 
      END (* IF *) 
    ; TRY 
        IF Options . EnablePickleWrite 
        THEN 
          PickleThread . Write ( LWr , Ref ) 
        END 
      ; Wr . Close ( LWr ) 
      EXCEPT Thread . Alerted => RAISE Thread . Alerted 
      ELSE 
        RAISE 
          Error 
            ( "Unable to serialize to file " 
              & AbsFileName 
              & ". WARNING: You may not be able to save later edits." 
            )
      END (* TRY EXCEPT *) 
    END WriteImagePickle 

(*
; PROCEDURE ReadFile ( FileName : TEXT ) 
  : PaintHs . ImageTransientTyp 
  RAISES { Error , Thread . Alerted } 
(* CHECK: This is uncalled, 2004-08-04 *) 

  = VAR LSimpleName : TEXT 
  ; VAR LImageName : TEXT 
  ; VAR LAbsFileName : TEXT 
  ; VAR LRef : REFANY 
  ; VAR LImageRef : PaintHs . ImageTransientTyp 
  ; VAR LImagePers : PaintHs . ImagePersistentTyp 
  ; VAR LFileKind : Atom . T 
  ; VAR LWrT : Wr . T 
  ; VAR LIsPickleOrCheckpointName : BOOLEAN 

  ; BEGIN (* ReadFile *) 
      LSimpleName := Pathname . Last ( FileName ) 
    ; LImageName := Misc . TextName ( LSimpleName ) 
    ; TRY 
        LAbsFileName 
          := Misc . AbsFileName ( Misc . WoVersionSuffix ( FileName ) ) 
      EXCEPT Thread . Alerted => RAISE Thread . Alerted 
      ELSE 
        LAbsFileName := NIL 
      END (* EXCEPT *) 
    ; IF ImageTable . get ( LImageName , LRef ) 
      THEN (* It's already loaded. *) 
        LImageRef := LRef 
      ; LImagePers := LImageRef . ItPers  
      ; Assert 
          ( Text . Equal ( LImagePers . IpImageName , LImageName ) 
          , AFT . A_Files_ReadFile_ImageNameMismatch 
          ) 
      ; IF LAbsFileName = NIL 
           OR NOT Text . Equal 
                    ( LImagePers . IpAbsPklFileName 
                    , Misc . PickleName ( LAbsFileName ) 
                    ) 
(* Check: ^Is this really the right criterion, when it is a text file? *)  
        THEN (* But the loaded version has a different file path. *) 
          RAISE Error 
            ( "Already open as " & LImagePers . IpAbsPklFileName ) 
        ELSE 
          RETURN LImageRef 
        END (* IF *) 
      ELSE (* Must read or create. *) 
        TRY 
          LFileKind := FS . Status ( FileName ) . type  
        EXCEPT Thread . Alerted => RAISE Thread . Alerted 
        ELSE 
          LFileKind := FileKindNonexistent 
        END (* TRY EXCEPT *) 
      ; IF LFileKind = FileKindNonexistent 
        THEN (* Create an empty file, then read it later. *) 
          LWrT := FileWr . Open ( FileName ) 
        ; Wr . Close ( LWrT ) 
        ELSIF LFileKind = RegularFile . FileType 
        THEN 
        ELSE 
          RETURN NIL 
        END (* IF *)  
      ; LIsPickleOrCheckpointName 
          := Misc . IsPickleOrCheckpointName ( FileName ) 
      ; IF LIsPickleOrCheckpointName 
        THEN 
          LImageRef := ReadNamedImageFile ( FileName ) 
        ; IF LImageRef # NIL 
          THEN 
            LImagePers := LImageRef . ItPers  
          ; LImagePers . IpImageName := LImageName 
(* TODO: Check/handle the case where previously existing name or path
         has changed. *) 
          ; LImageRef . ItWindowList := NIL 
          ; LImageRef . ItVisibleIn := PaintHs . WindowNoSetEmpty 
          ; LImageRef . ItScannerIf := NIL 
          ; LImageRef . ItIsSaved := TRUE  
          ; LResult . ItHistoryWrT := TextWr . New ( ) 
          ; LResult . ItPers . IpHistoryText := NIL 
          ; InsertNewImageRef ( LImageRef ) 
          END (* IF *) 
        ELSE LImageRef := NIL 
        END (* IF *) 
      ; IF LImageRef = NIL AND NOT LIsPickleOrCheckpointName  
        THEN (* It's a text file. *) 
          Messages . TextOnly 
            ( "Parsing -------------------------------------------------" 
            , Kind := MessageCodes . KindTyp . MkInformation 
            ) 
        ; LImageRef := OpenNamedTextFile ( FileName ) 
        ; IF LImageRef = NIL 
             OR EstUtil . HasSyntErrors ( LImageRef . ItPers . IpEstRoot ) 
          THEN 
            Messages . TextOnly 
              ( "Parse complete, with errors -----------------------------" 
              , Kind := MessageCodes . KindTyp . MkInformation 
              ) 
          ELSE 
            Messages . TextOnly 
              ( "Parse complete, no errors -------------------------------" 
              , Kind := MessageCodes . KindTyp . MkInformation 
              ) 
          END  
        ; IF LImageRef # NIL 
          THEN 
            LImagePers := LImageRef . ItPers  
          ; LImagePers . IpImageName := LImageName 
          ; LImagePers . IpIsParsed := TRUE  
          ; LImagePers . IpIsAnalyzed := FALSE  
          ; WriteImagePickle 
              ( LImageRef 
              , Images . PersistentImageToSave ( LImageRef , ForSave := TRUE ) 
              , DoCreateVersion := TRUE 
              )  
          ; LImageRef . ItIsSaved := TRUE 
          ; InsertNewImageRef ( LImageRef ) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    ; RETURN LImageRef 
    END ReadFile 
*)

(* VISIBLE: *) 
; PROCEDURE WriteText 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; FileName : TEXT 
    ) 
  RAISES { AssertionFailure , Error , Thread . Alerted } 
(* TODO: This is unused as of 2004-05-20.  See if it should go. *) 

  = VAR WrT : Wr . T 

  ; PROCEDURE WriteProc 
      ( <* UNUSED *> ImageRef : PaintHs . ImageTransientTyp 
      ; String : Strings . StringTyp 
      ) 
   RAISES { AssertionFailure , Thread . Alerted } 

    = <* FATAL Wr . Failure *> 
      BEGIN (* WriteProc *) 
        Wr . PutText ( WrT , Strings . ToTextNonNIL ( String ) ) 
      ; Wr . PutText ( WrT , Wr . EOL ) 
   (* ; Wr . Flush ( WrT ) for debug. *) 
      END WriteProc 

  ; BEGIN (* WriteText *) 
(* TODO: Check that the file suffix is right for the lang/tok. *) 
      TRY 
        WrT := FileWr . Open ( FileName ) 
(* TextEdit.FlushEdit? *) 
      ; WriteTrv . WriteText ( ImageRef , WriteProc , DoGenerateText := TRUE ) 
      ; Wr . Close ( WrT ) 
      EXCEPT Thread . Alerted => RAISE Thread . Alerted 
      | AssertionFailure ( EArg ) => RAISE AssertionFailure ( EArg ) 
      ELSE 
(* FIX:  eliminate this case. *) 
        RAISE Error ( "Can't open output text file " & FileName ) 
      END (* EXCEPT *) 
    END WriteText 

; BEGIN (* Files *) 
  END Files 
. 
