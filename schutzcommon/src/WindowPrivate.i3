
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE WindowPrivate 

(* Reveal the Private prefix of PaintHs . WindowPublic. *) 

; IMPORT PaintHs 
; IMPORT VBT 

; REVEAL PaintHs . WindowPrivateTyp 
    = VBT . Leaf BRANDED "WindowPrivateTyp" OBJECT END (* OBJECT *) 

; END WindowPrivate 
. 
