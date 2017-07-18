
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Ldl0ChildTok 
; IMPORT LbeStd 

(* Used by ManualAsTrees. *) 

; CONST MinTok = 1200 

(* Names of children: 1200 *) 

; CONST Alternatives = 1201
; CONST Child = 1202 
; CONST ChildClass = 1203 
; CONST ChildName = 1204 
; CONST Children = 1205 
; CONST Class = 1206 
; CONST ClassMembers = 1207 
; CONST ClassName = 1208  
; CONST ClosingName = 1209 
; CONST Elements = 1210
; CONST Formatters = 1211 
; CONST IndentCode = 1212 
; CONST LanguageName = 1213 
; CONST Levels = 1214 
; CONST Lhs = 1215 
; CONST ListChild = 1216 
; CONST Name = 1217 
; CONST Operators = 1218 
; CONST Parents = 1219 
; CONST Rhs = 1220 
; CONST Rules = 1221 
; CONST Separators = 1222 
; CONST StartName = 1223 

(* Other: *) 

; CONST Ldl0 = 1224

; CONST MaxTok = 1225

; PROCEDURE ToText ( Tok : LbeStd . TokTyp ) : TEXT 

; END Ldl0ChildTok 
. 

