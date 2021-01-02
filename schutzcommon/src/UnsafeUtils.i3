  
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

; TYPE TypeCodeTyp = CARDINAL 

; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : INTEGER 

; END UnsafeUtils 
. 
