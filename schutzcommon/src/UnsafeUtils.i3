  
(* -----------------------------------------------------------------------1- *)
(* File UnsafeUtils.i3  Modula-3 source code.                                *)
(* Copyright 2010 .. 2020, Rodney M. Bates.                                  *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *) 
(* -----------------------------------------------------------------------2- *)

INTERFACE UnsafeUtils

(* From m3core: *)
; IMPORT RT0 
; IMPORT Word 

; PROCEDURE IntOfRefany ( Ref : REFANY ) : INTEGER 

; PROCEDURE RefanyOfInt ( I : INTEGER ) : REFANY 

; PROCEDURE NULLOfInt ( I : INTEGER ) : NULL

; PROCEDURE PtrTo8CharArray ( VAR W : Word . T ) 
  : UNTRACED REF ARRAY [ 0 .. 7 ] OF CHAR

; PROCEDURE AdrToRT0_ActivationPtr ( Address : ADDRESS ) : RT0 . ActivationPtr 

; PROCEDURE DisplayException ( Tag : TEXT ; Addr : ADDRESS )
  (* Display, on command line, an interpretation of a value gotten from
     Compiler.ThisException. *)

; TYPE TypeCodeTyp = CARDINAL 

; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : INTEGER 

; END UnsafeUtils 
. 
