
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Assertions 

(* Support for richer runtime assertions than <*ASSERT*>.  Ability to 
   catch failures, print explanations, etc. 
*) 

; IMPORT Wr 
; IMPORT Fmt
; IMPORT RTIO 
; IMPORT Stdio 
; IMPORT Thread 

; IMPORT Failures 
; IMPORT MessageCodes 

; PROCEDURE AF ( ) 

  = VAR L : INTEGER 

  ; BEGIN (* AF *) 
    (* This is just a place to set a breakpoint for an assertion failure. *) 
      L := 0 
    END AF 

; PROCEDURE FailText ( Message : TEXT ) RAISES { AssertionFailure }   

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* FailText *)
(* Let Backstop, etc. produce this message:  
      Wr . PutText ( Stdio . stderr , "Assertion Failure: " ) 
    ; Wr . PutText ( Stdio . stderr , Message ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
    ; Wr . Flush ( Stdio . stderr ) 
*)
      AF ( ) 
    ; TRY
        RAISE AssertionFailure ( Message )
      EXCEPT
      | Failures . Ignore
      =>  RTIO . PutText ( "Ignoring assertion failure." )
        ; RTIO . PutText ( Wr . EOL )
        ; RTIO . Flush ( ) 
      END (* EXCEPT *) 
    END FailText 

(* EXPORTED: *) 
; PROCEDURE CantHappenText ( Message : TEXT ) RAISES { AssertionFailure } 

  = BEGIN (* CantHappenText *) 
      IF Checking 
      THEN FailText ( Message ) 
      END (*( IF *) 
    END CantHappenText 

(* EXPORTED: *) 
; PROCEDURE AssertText 
    ( Condition : BOOLEAN ; Message : TEXT ) RAISES { AssertionFailure } 

  = BEGIN (* AssertText *) 
      IF Checking AND NOT Condition 
      THEN FailText ( Message ) 
      END (* IF*) 
    END AssertText 

; PROCEDURE Fail ( Code : AFT ) RAISES { AssertionFailure } 
  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LMessage : TEXT 

  ; BEGIN (* Fail *) 
      LMessage := MessageCodes . Image ( Code ) 
(* Let Backstop, etc. produce this message:  
    ; Wr . PutText 
        ( Stdio . stderr 
        , "Assertion Failure (" & Fmt . Int ( ORD ( Code ) ) & "): " 
        ) 
    ; Wr . PutText ( Stdio . stderr , LMessage ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
    ; Wr . Flush ( Stdio . stderr ) 
*) 
    ; AF ( )
    ; TRY
        RAISE AssertionFailure ( LMessage ) 
      EXCEPT 
      | Failures . Ignore
      =>  RTIO . PutText ( "Ignoring assertion failure." )
        ; RTIO . PutText ( Wr . EOL )
        ; RTIO . Flush ( ) 
      END (* EXCEPT *) 
    END Fail 

(* EXPORTED: *) 
; PROCEDURE CantHappen ( Code : AFT ) RAISES { AssertionFailure } 

  = BEGIN (* CantHappen *) 
      IF Checking 
      THEN Fail ( Code ) 
      END (* IF *) 
    END CantHappen 

(* EXPORTED: *) 
; PROCEDURE Message ( Code : AFT ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    VAR LMessage : TEXT 

  ; BEGIN (* Message *)  
      LMessage := MessageCodes . Image ( Code ) 
    ; Wr . PutText 
        ( Stdio . stderr 
        , "Message with code (" & Fmt . Int ( ORD ( Code ) ) & "): " 
        ) 
    ; Wr . PutText ( Stdio . stderr , LMessage ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
    ; Wr . Flush ( Stdio . stderr ) 
    END Message 

(* EXPORTED: *) 
; PROCEDURE Assert 
    ( Condition : BOOLEAN ; Code : AFT ) RAISES { AssertionFailure } 

  = BEGIN (* Assert *) 
      IF Checking AND NOT Condition 
      THEN 
        Fail ( Code ) 
      END (* IF *) 
    END Assert 

; PROCEDURE NYI ( ) 

  = VAR L : INTEGER 

  ; BEGIN (* NYI *) 
      L := 0 
    END NYI 

(* EXPORTED: *) 
; PROCEDURE NotImplementedText ( Message : TEXT ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* NotImplementedText *) 
      Wr . PutText ( Stdio . stderr , "NotImplemented: " ) 
    ; Wr . PutText ( Stdio . stderr , Message ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
    ; Wr . Flush ( Stdio . stderr ) 
    ; NYI ( ) 
    END NotImplementedText 

(* EXPORTED: *) 
; PROCEDURE MessageText ( Message : TEXT ) 

  = <* FATAL Thread . Alerted *> 
    <* FATAL Wr . Failure *> 
    BEGIN (* MessageText *)  
      Wr . PutText ( Stdio . stderr , Message ) 
    ; Wr . PutText ( Stdio . stderr , Wr . EOL ) 
    ; Wr . Flush ( Stdio . stderr ) 
    END MessageText 

(* EXPORTED: *) 
; PROCEDURE AlwaysRaise 
    ( <* UNUSED *> String1 : TEXT 
    ; <* UNUSED *>  String2 : TEXT 
    ; <* UNUSED *> Code : MessageCodes . T 
    ; <* UNUSED *> DoWriteCheckpoint : BOOLEAN 
    ) 
  : BOOLEAN
  RAISES { AssertionFailure } 
  (* Ignores RaiseOnFailure and always raises AssertionFailure. *) 

  = BEGIN 
      RETURN TRUE 
    END AlwaysRaise  

(* EXPORTED: *) 
; PROCEDURE NeverRaise 
    ( <* UNUSED *> String1 : TEXT 
    ; <* UNUSED *> String2 : TEXT 
    ; <* UNUSED *> Code : MessageCodes . T 
    ; <* UNUSED *> DoWriteCheckpoint : BOOLEAN 
    ) 
  : BOOLEAN 
  (* Ignores RaiseOnFailure and Never raises AssertionFailure. *) 

  = BEGIN 
      RETURN FALSE 
    END NeverRaise  

; PROCEDURE DefaultRaise 
    ( <* UNUSED *> String1 : TEXT 
    ; <* UNUSED *> String2 : TEXT 
    ; <* UNUSED *> Code : MessageCodes . T 
    ; <* UNUSED *> DoWriteCheckpoint : BOOLEAN 
    ) 
  : BOOLEAN 
  RAISES { AssertionFailure } 
  (* The default QueryProc. Returns RaiseOnFailure. *) 

  = BEGIN 
      RETURN RaiseOnFailure  
    END DefaultRaise  

; PROCEDURE DoNothing ( ) 
  (* Call this for a do-nothing statement that will not be optimized away. *) 

  = VAR L : INTEGER 

  ; BEGIN 
    (* This is a good place to set a breakpoint. *) 
      L := 0 
    END DoNothing 

(* EXPORTED: *) 
; PROCEDURE CauseRuntimeError ( <* UNUSED *> Msg : TEXT ) 
  RAISES { AssertionFailure } <* NOWARN *>
  (* This is useful for debugging the catching of RT failures. *)

  = BEGIN 
      <* NOWARN *> EVAL VAL ( - 1 , CARDINAL ) 
    END CauseRuntimeError 

; BEGIN (* Assertions *) 
  END Assertions 
. 
