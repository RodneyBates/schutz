  
(* -----------------------------------------------------------------------1- *)
(* File UnsafeUtils.i3  Modula-3 source code.                                *)
(* Copyright 2010 .. 2022, Rodney M. Bates.                                  *)
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

; PROCEDURE DisplayActivation 
    ( Tag : TEXT ; Addr : ADDRESS (* RT0.ActivationPtr *) )
  (* Display, on command line, an interpretation of a value gotten from
     'Compiler.ThisException', which, contrary to the name 'ThisException',
     is a RT0.ActivationPtr, NOT an ExceptionPtr!! *)

; TYPE TypeCodeTyp = CARDINAL 

; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : INTEGER 

; END UnsafeUtils 
. 
