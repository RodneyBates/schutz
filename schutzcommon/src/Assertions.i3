
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Assertions 

(* Support for richer runtime assertions than <*ASSERT*>.  Ability to 
   catch failures, print explanations, etc. 
*) 

; IMPORT Thread

; IMPORT MessageCodes 

; TYPE QueryProcTyp 
    = PROCEDURE 
        ( String1 , String2 : TEXT 
        ; Code : MessageCodes . T 
        ; DoWriteCheckpoint : BOOLEAN 
        ) 
      : BOOLEAN 
  (* When there is a registered QueryProc callback, an assertion failure
     or runtime error calls it to query the user as to what to do. 
     It returns TRUE to ask that AssertionFailure or Backout be raised. *)

; VAR DefaultQueryProc : QueryProcTyp := AlwaysRaise
  (* This query procedure will be used when an assertion failure or runtime
     error occurs on a thread that is not of type AssertThreadT.
     It can be changed by client code. *)

; PROCEDURE InvokeQuery 
    ( String1 , String2 : TEXT
    ; Code : MessageCodes . T 
    ; DoWriteCheckpoint : BOOLEAN 
    ) 
  : BOOLEAN 

; TYPE ActionTyp
         = { None
           , Backout (* Query user and maybe raise Backout. *) 
           , AssertionFailure (* Query user and maybe raise AssertionFailure. *) 
           }

(* Use this Thread.T subtype to create a thread with options about how
   to handle a failure.  Generally, query the user. *) 

; TYPE AssertThreadT
    = Thread . T OBJECT
        QueryProc : QueryProcTyp := AlwaysRaise  
      ; AssertAction : ActionTyp 
      END 

; VAR Checking : BOOLEAN := TRUE 
      (* Causes assertions to be checked. Clients can change it. *) 
; VAR RaiseOnFailure : BOOLEAN := FALSE 
      (* For convenience.  Clients can set this and 
         QueryProcs can use it to decide whether the 
         exception should be raised.
      *)
      
; VAR DoWriteCheckpoint : BOOLEAN := TRUE   

; TYPE AFT = MessageCodes . T 

; EXCEPTION AssertionFailure ( TEXT ) 

; PROCEDURE Assert 
    ( Condition : BOOLEAN ; Code : AFT ) RAISES { AssertionFailure } 

; PROCEDURE CantHappen ( Code : AFT ) RAISES { AssertionFailure } 

; PROCEDURE Message ( Code : AFT ) 

; PROCEDURE AssertText 
    ( Condition : BOOLEAN ; Message : TEXT ) RAISES { AssertionFailure } 

; PROCEDURE CantHappenText ( Message : TEXT ) RAISES { AssertionFailure } 

; PROCEDURE NotImplementedText ( Message : TEXT ) 

; PROCEDURE MessageText ( Message : TEXT ) 

(* Some convenient QueryProcs you might want to use: *) 

; PROCEDURE AlwaysRaise 
    ( String1 , String2 : TEXT 
    ; Code : MessageCodes . T 
    ; DoWriteCheckpoint : BOOLEAN 
    ) 
  : BOOLEAN 
  (* Ignores RaiseOnFailure and always raises AssertionFailure. *) 

; PROCEDURE NeverRaise 
    ( String1 , String2 : TEXT 
    ; Code : MessageCodes . T 
    ; DoWriteCheckpoint : BOOLEAN 
    ) 
  : BOOLEAN 
  (* Ignores RaiseOnFailure and Never raises AssertionFailure. *) 

; PROCEDURE DefaultRaise 
    ( String1 , String2 : TEXT 
    ; Code : MessageCodes . T 
    ; DoWriteCheckpoint : BOOLEAN 
    ) 
  : BOOLEAN 
  (* The default QueryProc. Returns RaiseOnFailure. *) 

; PROCEDURE DoNothing ( ) 
  (* Call this for a do-nothing statement that will not be optimized away. *) 

; PROCEDURE CauseRuntimeError ( Msg : TEXT ) 
  RAISES { AssertionFailure } 

; VAR DoTerminate : BOOLEAN := FALSE 
  (* Set this during processing of an assertion failure, to cause termination
     later, after the exception has propagated through all levels that need
     do undo actions.
  *) 

; VAR TerminatingNormally : BOOLEAN := FALSE 
  (* Set this true before terminating in any way, to notify any registered
     Exitor routine that it should not do anything.  This kludge is the only
     way I can think of, without changing the Modula-3 RT code, to allow an 
     exitor to distinguish whether it is called because of a RT error or not. *)

; END Assertions 
. 
