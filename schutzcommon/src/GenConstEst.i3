
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE GenConstEst 

(* From an in-memory Est, Generate Modula-3 code for a module that will build 
   a fresh copy of the Est. 
*) 

; IMPORT Pathname 
; IMPORT OSError 
; IMPORT Wr 

; IMPORT LbeStd 
; IMPORT LdlSemantics 

; FROM Failures IMPORT Backout 

; PROCEDURE WriteStream 
    ( Lang : LbeStd . LangTyp 
    ; EstRoot : LbeStd . EstRootTyp 
    ; LangInfoRef : LdlSemantics . LangInfoRefTyp 
    ; UseLdlTok : BOOLEAN 
      (* ^The generated module use token names from interface LdlTok, 
         rather than numeric token codes. *) 
    ; ModuleName : TEXT := "Ldl0MakeEst" 
    ; WrT : Wr . T 
      (* Must be open.  Not closed. *) 
    )
  RAISES { Backout }
  (* Write the generated module to WrT. *) 

; PROCEDURE WriteName  
    ( Lang : LbeStd . LangTyp 
    ; EstRoot : LbeStd . EstRootTyp 
    ; LangInfoRef : LdlSemantics . LangInfoRefTyp 
    ; UseLdlTok : BOOLEAN 
      (* ^The generated module use token names from interface LdlTok, 
         rather than numeric token codes. *) 
    ; ModuleName : TEXT := "Ldl0MakeEst" 
    ; FilePath : Pathname . T := "" (* Means Pathname . Current *)  
    )
  RAISES { OSError . E }  
  (* Open a file and write the generated module to it. *) 

; END GenConstEst 
. 

