
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Ldl0ChildTok 

; IMPORT LbeStd 

(* VISIBLE *) 
; PROCEDURE ToText ( Tok : LbeStd . TokTyp ) : TEXT 

  = BEGIN
      CASE Tok 
      OF MinTok => RETURN "MinTok"
      | Alternatives => RETURN "Alternatives"
      | Child => RETURN "Child"
      | ChildClass => RETURN "ChildClass"
      | ChildName => RETURN "ChildName"
      | Children => RETURN "Children"
      | Class => RETURN "Class"
      | ClassMembers => RETURN "ClassMembers"
      | ClassName => RETURN "ClassName"
      | ClosingName => RETURN "ClosingName"
      | Elements => RETURN "Elements"
      | Formatters => RETURN "Formatters"
      | IndentCode => RETURN "IndentCode"
      | LanguageName => RETURN "LanguageName"
      | Levels => RETURN "Levels"
      | Lhs => RETURN "Lhs"
      | ListChild => RETURN "ListChild"
      | Name => RETURN "Name"
      | Operators => RETURN "Operators"
      | Parents => RETURN "Parents"
      | Rhs => RETURN "Rhs"
      | Rules => RETURN "Rules"
      | Separators => RETURN "Separators"
      | StartName => RETURN "StartName"
      | MaxTok => RETURN "MaxTok"
      | Ldl0 => RETURN "Ldl0"
      ELSE 
        RETURN "UnknownLdlChildToken"
      END (* CASE *) 
    END ToText  

; BEGIN 
  END Ldl0ChildTok 
. 

