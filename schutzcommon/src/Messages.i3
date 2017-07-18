
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Messages 

(* Error messaages to the terminal.  Mostly for batch processing, 
   especially processing an Ldl specification. 
*) 

; IMPORT Wr 

; IMPORT PortTypes 
; IMPORT LbeStd 
; IMPORT MessageCodes 

; PROCEDURE ZeroMessageCounts ( ) 

; PROCEDURE MessageCount 
     ( Kind : MessageCodes . KindTyp := MessageCodes . KindTyp . MkError ) 
  : PortTypes . Card32Typ 

; PROCEDURE SemError0 ( Code : MessageCodes . T ) 

; PROCEDURE SemError1 
    ( Node : LbeStd . EstNodeNoTyp ; Code : MessageCodes . T ) 

; CONST SemError = SemError1 

; PROCEDURE SemError2 
    ( Node1 : LbeStd . EstNodeNoTyp 
    ; Node2 : LbeStd . EstNodeNoTyp 
    ; Code : MessageCodes . T 
    ) 

; PROCEDURE SemErrorText ( Info : TEXT ; Code : MessageCodes . T ) 
  (* Message is prefixed by a label derived from Code. *) 

; PROCEDURE TextOnly 
    ( Info : TEXT ; Kind := MessageCodes . KindTyp . MkError ) 
  (* Kind is used only to increment a message count. *) 

; PROCEDURE Flush ( ) 

; PROCEDURE MessageWr ( ) : Wr . T 
  (* The stream used for messages. *) 

; PROCEDURE StdErrLine ( Line : TEXT ) 
  (* Write Line to standard error and flush. *) 

; END Messages 
. 

