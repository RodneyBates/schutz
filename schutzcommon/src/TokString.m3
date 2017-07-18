
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE TokString

(* Open arrays of tokens.  Used in CFG productions and to instantiate Table. *) 

; IMPORT Word 

(* VISIBLE: *) 
; PROCEDURE Equal ( Left , Right : T ) : BOOLEAN 

  = VAR LLeftNumber , LRightNumber : CARDINAL 

  ; BEGIN 
      IF Left = Right 
      THEN RETURN TRUE 
      ELSE
        IF Left = NIL 
        THEN LLeftNumber := 0 
        ELSE LLeftNumber := NUMBER ( Left ^ ) 
        END (* IF *) 
      ; IF Right = NIL 
        THEN LRightNumber := 0 
        ELSE LRightNumber := NUMBER ( Right ^ ) 
        END (* IF *) 
      ; IF LLeftNumber # LRightNumber 
        THEN RETURN FALSE 
        ELSIF LLeftNumber = 0 
        THEN RETURN TRUE 
        ELSE RETURN Left ^ = Right ^ 
        END (* IF *) 
      END (* IF *) 
    END Equal 

(* VISIBLE: *) 
; PROCEDURE Hash ( Value : T ) : Word . T 

  = VAR LResult : Word . T 
  ; VAR LNumber : CARDINAL 

  ; BEGIN 
      IF Value = NIL 
      THEN RETURN 0 
      ELSE 
        LNumber := NUMBER ( Value ^ ) 
      ; IF LNumber = 0 
        THEN RETURN 0 
        ELSE 
          LResult := 0 
        ; FOR RI := 0 TO LNumber - 1 
          DO 
            LResult 
              := Word . Plus 
                   ( LResult 
                   , Word . Rotate ( ORD ( Value ^ [ RI ] ) , RI MOD 8 ) 
                   ) 
          END (* FOR *) 
        ; RETURN LResult 
        END (* IF *) 
      END (* IF *) 
    END Hash 

; BEGIN 
  END TokString
. 

