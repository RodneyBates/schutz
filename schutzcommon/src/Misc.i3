
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Misc 

(* Some miscellaneous utilities, not easily otherwise categorized. *) 

; IMPORT Date 
; IMPORT Fingerprint  
; IMPORT Rd 

; IMPORT PortTypes 

; PROCEDURE Blanks ( Length : PortTypes . Int32Typ ) : TEXT 
  (* A TEXT of length Length, all blanks. *) 

; PROCEDURE IntegerImage ( Value : INTEGER ) : TEXT 

; PROCEDURE BooleanImageShort ( Value : BOOLEAN ) : TEXT 
  (* "F" or "T" *) 

; PROCEDURE DateImage ( READONLY Value : Date . T ) : TEXT 

; PROCEDURE FingerprintImage ( READONLY Value : Fingerprint . T ) : TEXT 

; PROCEDURE RefanyImage ( Ref : REFANY ) : TEXT 

; PROCEDURE EscapeText ( String : TEXT ) : TEXT 
  (* Add Modula-3 TEXT literal escapes. Do not add enclosing double quotes. *) 

; PROCEDURE QuoteText ( String : TEXT ) : TEXT 
  (* Add escape sequences and string quotes. *) 

; PROCEDURE QuoteChar ( Ch : CHAR ) : TEXT 
  (* Add escape sequences and character quotes. *) 

; PROCEDURE ReadAndUnquoteText ( RdT : Rd . T ) : TEXT 
  (* Read a Modula-3 string from RdT.  Remove leading and trailing blanks 
     and bounding string quotes.  Also convert escape sequences inside to 
     single characters.  Consume the string from RdT and return its 
     unquoted version. 
  *) 

; PROCEDURE UnescapeText ( String : TEXT ) : TEXT 
  (* Remove Modula-3 TEXT literal escapes. *)  

; PROCEDURE UnquoteText ( String : TEXT ) : TEXT 
  (* Remove any leading and trailing blanks and bounding string quotes.
     If there are bounding quotes at each end, convert escape sequences 
     inside to single characters. 
  *) 

; PROCEDURE UnquoteChar ( String : TEXT ) : CHAR  
  (* Remove leading and trailing blanks and bounding character quotes.
     Convert any escape sequence inside to a single character.
     Convert to a CHAR.  
     Return LbeStd . CharNull, if invalid. 
  *) 

; PROCEDURE CharIsEqual 
    ( Ch1 , Ch2 : CHAR ; CaseSensitive : BOOLEAN := TRUE ) 
  : BOOLEAN 

; PROCEDURE CharCompare  
    ( Ch1 , Ch2 : CHAR ; CaseSensitive : BOOLEAN := TRUE ) 
  : [ - 1 .. 1 ] 

; PROCEDURE FilePathSeparator ( ) : TEXT 
  (* OS-independent. *) 

; PROCEDURE WoVersionSuffix ( Name : TEXT ) : TEXT 
  (* Remove any suffix that is either dot tilde digit* tilde, or just tilde. *)

; PROCEDURE AbsFileName ( Name : TEXT ) : TEXT 
  (* Name need not exist. OS-independent. *) 

; PROCEDURE RelFileName 
    ( Name : TEXT  
    ; RelativeTo : TEXT := NIL 
      (* ^NIL, "", and Pathname.Current all mean working directory. *) 
    ) 
  : TEXT 
  (* If anything goes wrong, returns Name unchanged. *)

; PROCEDURE JoinPath ( Path : TEXT ; File : TEXT ) : TEXT 
  (* OS-independent. *) 

; PROCEDURE PickleName ( Name : TEXT ) : TEXT 
  (* Convert text file suffix or checkpoint suffix to pickle suffix. 
     First remove version suffix. 
     If this is already a pickle suffix, return it. 
     Works on anything from a full path name down to 
     just a suffix without even a dot. *) 

; PROCEDURE CheckpointName ( Name : TEXT ) : TEXT 
  (* Convert text file suffix or pickle to checkpoint suffix. 
     First remove version suffix. 
     If this is already a checkpoint suffix, return it. 
     Works on anything from a full path name down to 
     just a suffix without even a dot. *) 

; PROCEDURE TextName ( Name : TEXT ) : TEXT 
  (* Convert a pickle suffix or checkpoint suffix to a text file suffix. 
     First remove version suffix. 
     If this is already a text file suffix, return it. 
     Works on anything from a full path name down to 
     just a suffix without even a dot. *) 

; PROCEDURE IsPickleName ( Name : TEXT ) : BOOLEAN 
  (* Ignores a version suffix.  Works on anything from a full path name 
     down to just a suffix without even a dot. 
  *) 

; PROCEDURE IsPickleOrCheckpointName ( Name : TEXT ) : BOOLEAN 
  (* Ignores a version suffix.  Works on anything from a full path name 
     down to just a suffix without even a dot. 
  *) 

; END Misc 
. 

