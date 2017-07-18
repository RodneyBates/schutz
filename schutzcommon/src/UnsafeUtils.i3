
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE UnsafeUtils 

; IMPORT PortTypes 

; PROCEDURE IntOfRefany ( Ref : REFANY ) : INTEGER 

; PROCEDURE RefanyOfInt ( I : INTEGER ) : REFANY 

; PROCEDURE NULLOfInt ( I : INTEGER ) : NULL

; TYPE TypeCodeTyp = CARDINAL 

; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : PortTypes . Int32Typ 

; END UnsafeUtils 
. 
