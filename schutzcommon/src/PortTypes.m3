
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 2001..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* "File PortTypes.m3". Rodney M. Bates.  July, 2001 *) 

MODULE PortTypes 

(* Type declarations that are 
   intended to be the same size regardless of a 
   particular compiler's representation choices. 
   It is intended to provide portability.  Change 
   the declarations here and programs that use 
   them will continue to work as before. 
*) 

; IMPORT Fmt 
; IMPORT Text 

(* EXPORTED: *) 
; PROCEDURE IntImage ( Value : INTEGER ) : TEXT 
  (* Result never has blanks. *) 

  = VAR LImage : TEXT := Fmt . Int ( Value ) 
  ; VAR LLength := Text . Length ( LImage ) 

  ; BEGIN (* IntImage *) 
      IF LLength > 0 AND Text . GetChar ( LImage , 0 ) = ' ' 
      THEN 
        RETURN Text . Sub ( LImage , 1 ) 
      ELSE 
        RETURN LImage 
      END (* IF *) 
    END IntImage 

; BEGIN (* PortTypes *) 
  END PortTypes 
. 
