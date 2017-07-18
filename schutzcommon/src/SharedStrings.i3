
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE SharedStrings 

(* Strings of CHAR, taken from several representations, turned into atoms. *) 

; IMPORT Word 

; IMPORT PortTypes 
; IMPORT LbeStd 
; IMPORT Strings 
; IMPORT Assertions 

; FROM Assertions IMPORT AssertionFailure 

; TYPE StringNoTyp = BITS 15 FOR [ - 16_4000 .. 16_3FFF ] 
; CONST StringNoNull = 0 
; CONST FirstRealStringNo = 1 

; CONST StringNoImage = PortTypes . Int32Image 

; TYPE LengthTyp = PortTypes . Card32Typ 

; CONST Brand = "SharedStrings.T.Brand" 

; TYPE T <: LbeStd . EstRootTyp 

(* All visible procedures here accept NIL as arguments of type
   T, TEXT, or Strings.T and treat as "".
*) 

; PROCEDURE Equal ( k1 , k2 : T ) : BOOLEAN 

; PROCEDURE Hash ( k : T ) : Word . T 

; PROCEDURE FromString 
    ( String : Strings . T 
    ; Tok : LbeStd . TokTyp := LbeStd . Tok__Null 
    ) 
  : T 
  RAISES { AssertionFailure } 

; PROCEDURE FromArrayOfChar 
    ( READONLY String : ARRAY OF CHAR 
    ; Tok : LbeStd . TokTyp := LbeStd . Tok__Null 
    ) 
  : T 
  RAISES { AssertionFailure } 

; PROCEDURE FromText 
    ( String : TEXT 
    ; Tok : LbeStd . TokTyp := LbeStd . Tok__Null 
    ) 
  : T 
  RAISES { AssertionFailure } 

; VAR (* CONST *) Null : T (* := FromText ( "" ) *)  

; PROCEDURE SetTok ( String : T ; Tok : LbeStd . TokTyp ) 

; PROCEDURE Length ( String : T ) : LengthTyp 

; PROCEDURE StringNo ( String : T ) : StringNoTyp 
  (* StringNos are equal IFF the strings are equal. *) 

; PROCEDURE Tok ( String : T ) : LbeStd . TokTyp 

; PROCEDURE ToString ( String : T ) : Strings . T 

; PROCEDURE ToText ( String : T ) : TEXT 
  (* Will not return NIL *) 

; PROCEDURE Image ( String : T ; Indent := LbeStd . StdIndent ) : TEXT 
  (* Will not return NIL *) 

; EXCEPTION SsOutOfBounds 

; PROCEDURE IthChar 
    ( String : T ; I : LengthTyp ) : CHAR RAISES { SsOutOfBounds } 

; PROCEDURE UniqueStringCt ( ) : StringNoTyp 

; END SharedStrings 
. 
