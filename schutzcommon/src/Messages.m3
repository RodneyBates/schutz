
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Messages 

(* Error messaages to the terminal.  Mostly for batch processing, 
   especially processing an Ldl specification. 
*) 

; IMPORT Wr 
; IMPORT Fmt 
; IMPORT Stdio 
; IMPORT Thread 

; IMPORT PortTypes 
; IMPORT LbeStd 
; IMPORT MessageCodes 

; VAR GMessageCounts 
        := ARRAY MessageCodes . KindTyp OF PortTypes . Card32Typ { 0 , .. } 

; VAR WrT := Stdio . stderr 

(* VISIBLE: *) 
; PROCEDURE ZeroMessageCounts ( ) 

  = BEGIN 
      GMessageCounts := ARRAY MessageCodes . KindTyp OF PortTypes . Card32Typ 
        { 0 , .. }
    END ZeroMessageCounts 

; PROCEDURE SE ( ) 

  = VAR L : INTEGER 

  ; BEGIN (* SE *) 
    (* This is just a place to set a breakpoint. *) 
      L := 0 
    END SE

; PROCEDURE CountMessage ( Code : MessageCodes . T ) 

  = BEGIN
      INC ( GMessageCounts [ MessageCodes . Kind ( Code ) ] ) 
    END CountMessage 

(* VISIBLE: *) 
; PROCEDURE MessageCount 
     ( Kind : MessageCodes . KindTyp := MessageCodes . KindTyp . MkError ) 
  : PortTypes . Card32Typ 

  = BEGIN
      RETURN GMessageCounts [ Kind ] 
    END MessageCount 

(* VISIBLE: *) 
; PROCEDURE SemError0 ( Code : MessageCodes . T ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* SemError0 *) 
      Wr . PutText ( WrT , MessageCodes . KindImage 
        ( MessageCodes . Kind ( Code ) )  ) 
    ; Wr . PutText ( WrT , ": " ) 
    ; Wr . PutText ( WrT , MessageCodes . Image ( Code ) ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT ) 
    ; CountMessage ( Code )  
    ; SE ( ) 
    END SemError0 

(* VISIBLE: *) 
; PROCEDURE SemError1 
    ( Node : LbeStd . EstNodeNoTyp ; Code : MessageCodes . T ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* SemError1 *) 
      Wr . PutText ( WrT , MessageCodes . KindImage 
        ( MessageCodes . Kind ( Code ) )  ) 
    ; Wr . PutText ( WrT , ": " ) 
    ; Wr . PutText ( WrT , MessageCodes . Image ( Code ) ) 
    ; Wr . PutText ( WrT , ", node: " ) 
    ; Wr . PutText ( WrT , Fmt . Int ( Node ) ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT ) 
    ; CountMessage ( Code )  
    ; SE ( ) 
    END SemError1 

(* VISIBLE: *) 
; PROCEDURE SemError2 
    ( Node1 : LbeStd . EstNodeNoTyp 
    ; Node2 : LbeStd . EstNodeNoTyp 
    ; Code : MessageCodes . T 
    ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* SemError2 *) 
      Wr . PutText 
        ( WrT , MessageCodes . KindImage ( MessageCodes . Kind ( Code ) )  ) 
    ; Wr . PutText ( WrT , ": " ) 
    ; Wr . PutText ( WrT , MessageCodes . Image ( Code ) ) 
    ; Wr . PutText ( WrT , ", node:" ) 
    ; Wr . PutText ( WrT , Fmt . Int ( Node1 ) ) 
    ; Wr . PutText ( WrT , "," ) 
    ; Wr . PutText ( WrT , Fmt . Int ( Node2 ) ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT ) 
    ; CountMessage ( Code )  
    ; SE ( ) 
    END SemError2 

(* VISIBLE: *) 
; PROCEDURE SemErrorText ( Info : TEXT ; Code : MessageCodes . T ) 
  (* Message is prefixed by a label derived from Code. *) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* SemErrorText *) 
      Wr . PutText 
        ( WrT , MessageCodes . KindImage ( MessageCodes . Kind ( Code ) )  ) 
    ; Wr . PutText ( WrT , ": " ) 
    ; Wr . PutText ( WrT , MessageCodes . Image ( Code ) ) 
    ; Wr . PutText ( WrT , Info ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT ) 
    ; CountMessage ( Code )  
    ; SE ( ) 
    END SemErrorText 

(* VISIBLE: *) 
; PROCEDURE TextOnly 
    ( Info : TEXT ; Kind := MessageCodes . KindTyp . MkError ) 
  (* Kind is used only to increment a message count. *) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN 
      Wr . PutText ( WrT , Info ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT ) 
    ; INC ( GMessageCounts [ Kind ] ) 
    ; SE ( ) 
    END TextOnly 

(* VISIBLE: *) 
; PROCEDURE Flush ( ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN 
      Wr . Flush ( WrT ) 
    END Flush 

(* VISIBLE: *) 
; PROCEDURE MessageWr ( ) : Wr . T 
  (* The stream used for messages. *) 

  = BEGIN 
      RETURN WrT 
    END MessageWr 

(* VISIBLE: *) 
; PROCEDURE StdErrLine ( Line : TEXT ) 
  (* Write Line to standard error and flush. *) 

  = BEGIN 
      TRY 
        Wr . PutText ( Stdio . stderr , Line & Wr . EOL ) 
      ; Wr . Flush ( Stdio . stderr ) 
      EXCEPT Wr . Failure , Thread . Alerted 
      => (* Oh, just forget it then. *)  
      END (* TRY EXCEPT *) 
    END StdErrLine 

; BEGIN (* Messages *) 
  END Messages 
. 

