
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE VersionedFiles

(* Support for multiple versions of a file, using ordinary files whose names 
   end with a version suffix, like emacs does.
*) 

; IMPORT Wr 
; IMPORT Thread  

; EXCEPTION Error ( TEXT ) 

; PROCEDURE RenameVersion ( File : TEXT ) : TEXT 
  RAISES { Error , Thread . Alerted } 
  (* If a regular file named "File" exists, rename it as
     "File.~<n>~", where n is one greater than the greatest
     number of any file in the same directory with this same
     form of name.  "File" may include an absolute or relative
     path.  The result is the simple name of the renamed file,
     or "" if none existed to rename.  The argument of Error
     is an explanation of what went wrong. *)  

; PROCEDURE OpenWrite ( FileName : TEXT ) : Wr . T 
  RAISES { Error , Thread . Alerted } 
  (* Open file for writing, with old version renaming. *) 

; END VersionedFiles
. 
