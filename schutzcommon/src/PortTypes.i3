
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
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

(* TODO: Needs updating for the modern world. *) 
(* This version is for PM3. *) 

; TYPE Card8Typ = [ 0 .. 255 ] 
  (* ^8 bit cardinal *) 

; TYPE Int8Typ = [ - 128 .. 127 ] 
  (* ^8 bit integer *) 

; TYPE Card16Typ = [ 0 .. 65535 ] 
  (* ^16 bit cardinal *) 

; TYPE Int16Typ = [ - 32768 .. 32767 ] 
  (* ^16 bit integer *) 

; TYPE Card32Typ = [ 0 .. LAST ( INTEGER ) ] 
  (* ^32 bit cardinal *) 
  (* Make it compatible with INTEGER *) 

; TYPE Int32Typ = INTEGER 
  (* ^32 bit integer *) 

; PROCEDURE Int32Image ( Value : Int32Typ ) : TEXT 
  (* Result never has blanks. *) 
  (* Works for other Int* and Card* types with subset value ranges. *) 

; CONST Card32Image = Int32Image 
; CONST Int16Image = Int32Image 
; CONST Card16Image = Int32Image 
; CONST Int8Image = Int32Image 
; CONST Card8Image = Int32Image 

; CONST INTEGERImage = Int32Image 

; END PortTypes 
. 
