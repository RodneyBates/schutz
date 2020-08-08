
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1997..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* "File PortTypes.i3". Rodney M. Bates.  Sep. 1997 *) 

INTERFACE PortTypes 

(* Type declarations that are 
   intended to be the same size regardless of a 
   particular compiler's representation choices. 
   It is intended to provide portability.  Change 
   the declarations here and programs that use 
   them will continue to work as before. 
*) 

; TYPE Card8Typ = [ 0 .. 255 ] 
  (* ^8 bit cardinal *) 

; TYPE Int8Typ = [ - 128 .. 127 ] 
  (* ^8 bit integer *) 

; TYPE Card16Typ = [ 0 .. 65535 ] 
  (* ^16 bit cardinal *) 

; TYPE Int16Typ = [ - 32768 .. 32767 ] 
  (* ^16 bit integer *) 

; TYPE Card32Typ = [ 0 .. 16_7FFFFFFF ] 
  (* ^32 bit cardinal *) 

; TYPE Int32Typ = [ - 16_7FFFFFFF - 1 .. 16_7FFFFFFF ] 
  (* ^32 bit integer *) 

; PROCEDURE IntImage ( Value : INTEGER ) : TEXT 
  (* Result never has blanks. *) 
  (* Works for Int* and Card* types with subset value ranges. *) 

; CONST Int32Image = IntImage 
; CONST Card32Image = IntImage 
; CONST Int16Image = IntImage 
; CONST Card16Image = IntImage 
; CONST Int8Image = IntImage 
; CONST Card8Image = IntImage 

; CONST INTEGERImage = IntImage 

; END PortTypes 
. 
