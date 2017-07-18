
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE UncertainBool

(* Boolean logic extended to a three-valued domain by an "unknown". *) 

(* These operators respect standard Boolean arithmetic, while giving maximum 
   possible knownness consistent therewith.

   Changing an operand of any operator herein from Unknown to False or True 
   can change the result only from Unknown to False or True.  This is the
   monotonic property needed to ensure that the iterate-until-nothing-changes
   strategy for solving a system of equations will converge.   
   
*)  

; CONST Brand = "UncertainBool1.0" 

; TYPE T = { Unknown , False , True }

; PROCEDURE FromBool ( Value : BOOLEAN ) : T  

; EXCEPTION UnknownBool 

; PROCEDURE ToBool ( Value : T ) : BOOLEAN RAISES { UnknownBool }

; PROCEDURE And ( Left , Right : T ) : T 

; PROCEDURE Or ( Left , Right : T ) : T 

; CONST Xor = Unequal  

; PROCEDURE Not ( Value : T ) : T 

; PROCEDURE Nand ( Left , Right : T ) : T 

; PROCEDURE Nor ( Left , Right : T ) : T 

; CONST Implies = LE 

; PROCEDURE Equal ( Left , Right : T ) : T 
  (* Not the same as Left = Right! *)

; PROCEDURE Unequal ( Left , Right : T ) : T
  (* Not the same as Left # Right! *)

; PROCEDURE LT ( Left , Right : T ) : T
  (* Not the same as Left < Right! *)

; PROCEDURE GT ( Left , Right : T ) : T
  (* Not the same as Left > Right! *)

; PROCEDURE LE ( Left , Right : T ) : T
  (* Not the same as Left <= Right! *)

; PROCEDURE GE ( Left , Right : T ) : T
  (* Not the same as Left >= Right! *)

; PROCEDURE Image ( Value : T ) : TEXT 

; END UncertainBool 
.
 
