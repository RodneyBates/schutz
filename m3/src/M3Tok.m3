
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE M3Tok 

; IMPORT LbeStd 

(* VISIBLE *) 
; PROCEDURE ToText ( Tok : LbeStd . TokTyp ) : TEXT 

  = BEGIN
      CASE Tok 
      OF 
      ELSE 
        RETURN 
          "M3Tok" & LbeStd . NumTokImage ( Tok ) 
      END (* CASE *) 
    END ToText  

; BEGIN 
  END M3Tok 
. 

