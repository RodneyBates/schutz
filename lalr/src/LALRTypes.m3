
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* This module has low level operations on types declared in its exported
   interface, which are used only by the LALR processing code in subdirectory 
   lalr.
*) 

MODULE LALRTypes 

(* VISIBLE: *) 
; PROCEDURE IgnoredCounterpart ( Val : ReprKindTyp ) : ReprKindTyp 
  (* Converts any of TermRep, NontermRep, AstNontermRep, or RedRep to its 
     ignored counterpart, otherwise identity.
  *) 

  = BEGIN 
      CASE Val 
      OF ReprKindTyp . TermRep => RETURN ReprKindTyp . TermRepIgn  
      | ReprKindTyp . NontermRep => RETURN ReprKindTyp . NontermRepIgn 
      | ReprKindTyp . AstNontermRep => RETURN ReprKindTyp . AstNontermRepIgn 
      | ReprKindTyp . RedRep => RETURN ReprKindTyp . RedRepIgn 
      ELSE RETURN Val
      END (* IF *) 
    END IgnoredCounterpart 

; BEGIN 
  END LALRTypes
. 

