
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE VersionedFiles

(* Support for multiple versions of a file, using ordinary files whose names 
   end with a version suffix, like emacs does.
*) 

; IMPORT Pathname 
; IMPORT FS
; IMPORT Lex
; IMPORT TextRd
; IMPORT Fmt
; IMPORT Text
; IMPORT OSError 
; IMPORT File
; IMPORT RegularFile
; IMPORT Wr 
; IMPORT FileWr 
; IMPORT Thread 

; PROCEDURE FileVersionNo ( S : TEXT ) : INTEGER
  RAISES { Thread . Alerted } 
  (* Negative, if not a recognizable version suffix. *) 

  = VAR LLength : INTEGER 
  ; VAR LResult : INTEGER 

  ; BEGIN 
      LLength := Text . Length ( S )
    ; IF LLength < 3 
      THEN 
        LResult := - 1  
      ELSIF Text . GetChar ( S , 0 ) = '~' 
            AND Text . GetChar ( S , LLength - 1 ) = '~' 
      THEN
        TRY 
         LResult 
            := Lex . Int 
                 ( TextRd . New ( Text . Sub ( S , 1 , LLength - 2 ) ) )
        EXCEPT Thread . Alerted => RAISE Thread . Alerted 
        ELSE
          LResult := - 1 
        END (* TRY EXCEPT *) 
      ELSE 
        LResult := - 1 
      END (* IF *) 
    ; RETURN LResult 
    END FileVersionNo 

; PROCEDURE VersionSuffix ( No : INTEGER ) : TEXT 

  = BEGIN
      RETURN 
        "~" 
        & Fmt . Int ( No ) 
        & "~" 
    END VersionSuffix 

(* VISIBLE: *) 
; PROCEDURE RenameVersion ( FileName : TEXT ) : TEXT 
  RAISES { Error , Thread . Alerted } 
  (* If a regular file named "FileName" exists, rename it as
     "FileName.~<n>~", where n is one greater than the greatest
     number of any file in the same directory with this same
     form of name.  "FileName" may include an absolute or relative
     path.  The result is the simple name of the renamed file,
     or "" if none existed to rename.  The argument of Error
     is an explanation of what went wrong. *)  

  = VAR LStatus : File . Status 
  ; VAR LPrefix : TEXT 
  ; VAR LLast : TEXT 
  ; VAR LIterator : FS . Iterator  
  ; VAR LCandidate : TEXT 
  ; VAR LCandidateBase : TEXT 
  ; VAR LCandidateExt : TEXT 
  ; VAR LNewFileName : TEXT 
  ; VAR LNewFullName : TEXT 
  ; VAR LVersionSuffix : TEXT 
  ; VAR LVersionNo : INTEGER  
  ; VAR LLatestVersionNo : INTEGER  

  ; <* FATAL OSError . E *>
    BEGIN 
      TRY 
        LStatus := FS . Status ( FileName ) 
      ; IF LStatus . type # RegularFile . FileType 
        THEN 
          RAISE Error ( "Exists but not a regular file" ) 
        END (* IF *) 
      EXCEPT 
        OSError . E 
        => RETURN ""
      END (* TRY EXCEPT *) 
    ; LPrefix := Pathname . Prefix ( FileName ) 
    ; LLast := Pathname . Last ( FileName )
    ; LIterator := FS . Iterate ( LPrefix ) 
    ; LLatestVersionNo := 0 
    ; TRY
        WHILE LIterator . next ( LCandidate ) 
        DO IF NOT Text . Equal ( LCandidate , LLast ) 
          THEN
            LCandidateBase := Pathname . LastBase ( LCandidate ) 
          ; IF Text . Equal ( LCandidateBase , LLast ) 
            THEN 
              LCandidateExt := Pathname . LastExt ( LCandidate )
            ; LVersionNo := FileVersionNo ( LCandidateExt )  
            ; IF LVersionNo >= 0   
              THEN
                LLatestVersionNo := MAX ( LLatestVersionNo , LVersionNo ) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* WHILE *) 
      FINALLY
        LIterator . close ( ) 
      END (* TRY FINALLY *) 
    ; LVersionSuffix := VersionSuffix ( LLatestVersionNo + 1 ) 
    ; LNewFileName := LLast & LVersionSuffix 
    ; LNewFullName := Pathname . Join ( LPrefix , LLast , LVersionSuffix )
    ; TRY 
        FS . Rename ( FileName , LNewFullName ) 
      EXCEPT Thread . Alerted => RAISE Thread . Alerted
      ELSE
        RAISE 
          Error  ( "Can't rename to\"" & LNewFullName & "\"" ) 
      END (* TRY EXCEPT *) 
    ; RETURN LNewFileName
    END RenameVersion 

(* VISIBLE: *) 
; PROCEDURE OpenWrite ( FileName : TEXT ) : Wr . T 
  RAISES { Error , Thread . Alerted } 
  (* Open file for writing, with old version renaming. *) 

  = VAR LOldVersion : TEXT 
  ; VAR LResult : Wr . T 

  ; BEGIN 
      LOldVersion := RenameVersion ( FileName ) 
    ; TRY 
        LResult := FileWr . Open ( FileName ) 
      EXCEPT 
      OSError . E => 
        RAISE Error ( "OSError.E" ) 
      | Thread . Alerted => RAISE Thread . Alerted
      ELSE 
        RAISE Error ( "" ) 
      END (* TRY EXCEPT *) 
    ; RETURN LResult 
    END OpenWrite 

; BEGIN 
  END VersionedFiles
. 
