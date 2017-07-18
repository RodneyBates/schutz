
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Errors 

(* Errors communicated to the editor user. *) 

; TYPE ErrorTyp 
    = { EJoinLinesLineTooLong 
      , EJoinLinesAtBOL 
      , EInsertCharLineTooLong 
      , EBadChar 
      , EControlC 
      , ETransposeCharsAtBOL
      , EWorkRefused 
      } 

; END Errors 
. 
