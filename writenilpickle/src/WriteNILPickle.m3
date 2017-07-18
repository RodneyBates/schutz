
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Rodney M. Bates.  Write a NIL pickle. *) 

MODULE WriteNILPickle EXPORTS Main 

(* Except for swapping the strings "Pickle" and "Pickle2 AS Pickle", make no
   changes to the following line, as it is recognized/edited by scripts.
   (see scripts/topickle.sh and scripts/topickle2.sh)
*) 
; IMPORT Pickle2 AS Pickle (* The Pickle2 mess. *)
; IMPORT IO 
; IMPORT Wr 

; CONST FileName = "NIL.pkl"

; PROCEDURE WritePkl ( Val : REFANY ) 

  = VAR Writer : Wr . T := IO . OpenWrite ( FileName ) 

  ; BEGIN (* WritePkl *) 
      Pickle . Write ( Writer , Val ) 
    ; Wr . Close ( Writer ) 
    END WritePkl 

; BEGIN 
    WritePkl ( NIL ) 
  END WriteNILPickle 
. 

