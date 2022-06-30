 
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2022, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE Failures 

(* UNSAFE stuff needed for snagging runtime errors from the RTS and
   querying the user about what to do with them. *)

; IMPORT Compiler 
; IMPORT Fmt 
; IMPORT M3toC 
; IMPORT RT0
; IMPORT RTException
; IMPORT RTIO 
; IMPORT RTProcedureSRC  
; IMPORT RTProcess 
; IMPORT RuntimeError
; IMPORT TextWr 
; IMPORT Thread
; IMPORT Wr 

; IMPORT Assertions

; VAR GOldBackstop : RTException . Backstop

; TYPE ThreadInfoTyp = RECORD
      Link : ThreadInfoRefTyp := NIL
    ; Thread : Thread . T 
    ; QueryProc : QueryProcTyp := NIL 
    END
    
; REVEAL ThreadInfoRefTyp = BRANDED REF ThreadInfoTyp

; VAR GThreadInfoList : ThreadInfoRefTyp := NIL 

; VAR GThreadInfoDefault := ThreadInfoTyp { NIL , NIL , NIL }

(* EXPORTED *) 
; PROCEDURE RegisterQueryProc ( QueryProc : QueryProcTyp )
  (* Register a query procedure for Thread.Self. *) 

  = VAR LInfoRef , LTrailingInfoRef , LNewInfoRef : ThreadInfoRefTyp 
  ; VAR LThreadSelf : Thread . T
  
  ; BEGIN
      LThreadSelf := Thread . Self ( )
    ; IF LThreadSelf = NIL THEN RETURN END (* IF *) 
    ; LNewInfoRef 
        := NEW ( ThreadInfoRefTyp 
               , Link := GThreadInfoList
               , Thread := LThreadSelf
               , QueryProc := QueryProc
               )
    ; IF GThreadInfoList = NIL
         OR GThreadInfoList ^ . Thread = LThreadSelf 
      THEN (* Make the new node the only node. *)
        LNewInfoRef . Link := GThreadInfoList
      ; GThreadInfoList := LNewInfoRef
      ELSE 
        LTrailingInfoRef := NIL 
      ; LInfoRef := GThreadInfoList 
      ; LOOP
          IF LInfoRef = NIL
          THEN
            LNewInfoRef ^ . Link := NIL
          ; LTrailingInfoRef ^ . Link  := LNewInfoRef 
          ; EXIT 
          ELSIF LInfoRef ^ . Thread = LThreadSelf
          THEN (* Replace old node with the new one. *)
            LNewInfoRef ^ . Link := LTrailingInfoRef ^ . Link 
          ; LTrailingInfoRef ^ . Link := LNewInfoRef
          ; EXIT
          ELSE
            LTrailingInfoRef := LInfoRef 
          ; LInfoRef := LInfoRef ^ . Link 
          END (* IF *)
        END (* LOOP *)
      END (* IF *) 
    END RegisterQueryProc 

 ; PROCEDURE QueryDefault
     ( READONLY Act : RT0 . RaiseActivation
     ; String1 , String2 : TEXT 
     ; AllowedActions : FailureActionSetTyp
     )
   : FailureActionTyp

   = BEGIN
(* IMPLEMENT ME. *)
       RETURN FailureActionTyp . FaCrash 
(*
      NewExc = NIL 
              THEN RTException . InvokeBackstop ( Act , raises )
              ELSE (* An interesting exception. *)
                (* Query the user about it. *) 
                IF Act . module # NIL
                THEN LFileString := Act . module . file
                END (* IF *)
              ; LLocation
                  := M3toC . StoT ( LFileString )
                     & ":" & Fmt . Int ( Act . line )
              ; LMessage
                  := "Runtime error "
                     & RuntimeError . Tag ( VAL ( LIntArg , RuntimeError . T ) ) 
              ; LDoTerminate
                  := TThread . QueryProc  
                       ( LLocation  
                       , LMessage 
                       , AFT . A_RuntimeError
                       , DoWriteCheckpoint := TRUE
                       )
*)
     END QueryDefault 

; PROCEDURE ThreadInfoRef ( ) : ThreadInfoRefTyp
  (* The thread info for the executing thread. *) 

  = VAR LThreadInfoRef : ThreadInfoRefTyp 
  ; VAR LThreadSelf : Thread . T
  
  ; BEGIN
      LThreadSelf := Thread . Self ( )
    ; IF LThreadSelf = NIL (* Can happen? *) 
      THEN RETURN
             NEW ( ThreadInfoRefTyp 
                 , Link := NIL
                 , Thread := LThreadSelf
                 , QueryProc := QueryDefault
                 ) 
      ELSE
        LThreadInfoRef := GThreadInfoList 
      ; LOOP
          IF LThreadInfoRef = NIL
          THEN
            RETURN
              NEW ( ThreadInfoRefTyp
                  , Link := NIL
                  , Thread := LThreadSelf
                  , QueryProc := QueryDefault
                  ) 
          END (* IF *)
        ; IF LThreadInfoRef . Thread = LThreadSelf
          THEN RETURN LThreadInfoRef
          END (* IF *)
        ; LThreadInfoRef := LThreadInfoRef . Link 
        END (* LOOP *) 
      END (* IF *)  
    END ThreadInfoRef

; CONST SecondaryRtes
    = SET OF RuntimeError . T
              { RuntimeError . T . UnhandledException
              , RuntimeError . T . BlockedException
              }

(* EXPORTED *) 
; PROCEDURE ExcName ( READONLY Act : RT0 . RaiseActivation ) : TEXT

  = VAR LExc : RT0 . ExceptionPtr
  ; VAR LArg : RT0 . ExceptionArg 
  ; VAR LIntArg : INTEGER
  
  ; BEGIN
      IF Act . un_except = NIL
      THEN (* Not a secondary failure. *) 
        LExc := Act . exception
      ; LArg := Act . arg 
      ELSE (* Secondary.  Report the primary exception. *) 
        LExc := Act . un_except
      ; LArg := Act . un_arg 
      END (* IF *) 
    ; LIntArg := LOOPHOLE ( LArg , INTEGER ) 
    ; IF LIntArg < ORD ( FIRST ( RuntimeError . T ) ) 
         OR LIntArg > ORD ( LAST ( RuntimeError . T ) )
      THEN LIntArg := ORD ( RuntimeError . T . Unknown )
      END (* IF *)
    ; RETURN ExcNameInternal ( Act , LExc , LIntArg ) 
    END ExcName 

; PROCEDURE ExcNameInternal
    ( READONLY Act : RT0 . RaiseActivation
    ; Exc : RT0 . ExceptionPtr
    ; IntArg : INTEGER
    )
  : TEXT
  (* Exc.name^ is in compiler-initialized constant data, so
     no risk of its disappearing and undermining the result. *) 

  = VAR LResult : TEXT

  ; BEGIN
      IF Exc = NIL OR Exc . name = NIL 
      THEN RETURN "<unknown>"
      ELSE
        LResult := M3toC . StoT ( LOOPHOLE ( Exc . name , ADDRESS ) )
      ; IF Exc = RuntimeError . Self ( )
        THEN
          LResult
            := LResult & "("
               & RuntimeError . Tag ( VAL ( IntArg , RuntimeError . T ) )
        ; IF VAL ( IntArg , RuntimeError . T )
             = RuntimeError . T . AssertFailed
             AND Act . info0 # NIL
          THEN
            LResult
              := LResult & "(" & LOOPHOLE ( Act . info0 , TEXT ) & ")"  
          END (* IF *)
        ; LResult := LResult & ")"
        END (* IF *) 
      ; RETURN LResult 
      END (* IF *) 
    END ExcNameInternal

; PROCEDURE PutHex ( WrT : Wr . T ; Value : INTEGER )

  = BEGIN 
      Wr.PutText
        ( WrT
        , Fmt . Pad
            ( Fmt . Unsigned ( Value , base := 16 )
            , length := 2 * BYTESIZE ( INTEGER )
            , padChar := '0'
            , align := Fmt . Align . Right 
            )
        )
    END PutHex 

(* EXPORTED *) 
; PROCEDURE ActivationImage ( READONLY Act : RT0 . RaiseActivation ) : TEXT 

  = VAR LExc : RT0 . ExceptionPtr
  ; VAR LArg : RT0 . ExceptionArg 
  ; VAR LIntArg : INTEGER 
  ; VAR LWrT : TextWr . T
  ; VAR LOffset : INTEGER 
  ; VAR LProcAddr : ADDRESS  
  ; VAR LFileNameS , LProcNameS : ADDRESS 

  ; BEGIN
      IF Act . un_except = NIL
      THEN (* Not a secondary failure. *) 
        LExc := Act . exception
      ; LArg := Act . arg 
      ELSE (* Secondary.  Report the primary exception. *) 
        LExc := Act . un_except
      ; LArg := Act . un_arg 
      END (* IF *) 
    ; LIntArg := LOOPHOLE ( LArg , INTEGER ) 
    ; IF LIntArg < ORD ( FIRST ( RuntimeError . T ) ) 
         OR LIntArg > ORD ( LAST ( RuntimeError . T ) )
      THEN LIntArg := ORD ( RuntimeError . T . Unknown )
      END (* IF *)
    ; IF Act . module # NIL
      THEN LFileNameS := Act . module . file
      ELSE LFileNameS := NIL 
      END (* IF *)
    ; LWrT := TextWr . New ( ) 
    ; IF LFileNameS = NIL AND Act . pc # NIL
      THEN (* Use the pc to get the raise location. *)
        Wr . PutText ( LWrT , "***    pc = 16_" )
      ; PutHex ( LWrT , LOOPHOLE ( Act . pc , INTEGER ) ) 
      ; RTProcedureSRC . FromPC
          ( Act . pc
          , (*VAR*) LProcAddr , (*VAR*) LFileNameS , (*VAR*) LProcNameS
          )
      ; LOffset
          := LOOPHOLE ( Act . pc , INTEGER ) - LOOPHOLE  ( LProcAddr , INTEGER )
      ; IF 0 <= LOffset AND LOffset < 65536 (* Why this limit? *) 
        THEN
          IF LProcNameS # NIL THEN
            Wr . PutText ( LWrT , " = " )
          ; Wr . PutText ( LWrT , M3toC . StoT ( LProcNameS ) )
          ; IF LOffset # 0
            THEN
              Wr . PutText ( LWrT , " + ")
            ; PutHex ( LWrT , LOffset)
            END (* IF *)
          END (* IF *)
        ; IF LFileNameS # NIL THEN
            Wr . PutText ( LWrT , " in file " )
          ; Wr . PutText ( LWrT , M3toC . StoT ( LFileNameS ) )
          END (* IF *)
        END (* IF *)
      ELSIF LFileNameS # NIL
      THEN
        Wr . PutText ( LWrT , "***    file \"") 
      ; Wr . PutText ( LWrT , M3toC . StoT ( LFileNameS ) )
      ; Wr . PutText ( LWrT , "\"")
      ; IF Act . line # 0
        THEN
          Wr . PutText ( LWrT , ", line ")
        ; Wr . PutText ( LWrT , Fmt . Int ( Act . line ) )
        END (* IF *)
      ELSE Wr . PutText ( LWrT , "<unknown location>" )
      END (* IF *) 
    ; RETURN TextWr . ToText ( LWrT ) 
    END ActivationImage 

; PROCEDURE Crash
    ( READONLY Act : RT0 . RaiseActivation
    ; IntArg : INTEGER
    ; KindMsg , ExcName : TEXT
    )

  = BEGIN
      RTIO . PutText ( Wr . EOL )
    ; RTIO . PutText ( "##### " )
    ; RTIO . PutText ( KindMsg )
    ; RTIO . PutText ( ": " )
    ; RTIO . PutText ( ExcName )
    ; RTIO . PutText ( " #####" )
    ; RTIO . PutText ( Wr . EOL )
    ; RTIO . PutText ( ActivationImage ( Act ) ) 
    ; RTIO . PutText ( Wr . EOL )
    ; RTIO . Flush ( ) 
    ; RTProcess.Crash (NIL) 
    END Crash

; CONST SecondaryMap 
   = ARRAY BOOLEAN (* wasBlocked *)
     OF RuntimeError . T
          { RuntimeError . T . UnhandledException
          , RuntimeError . T . BlockedException
          }

; PROCEDURE BackstopPrimary
    ( VAR Act : RT0 . RaiseActivation ; wasBlocked : BOOLEAN )
  (* Turn the fault into a secondary RT error and reraise. *)

  = BEGIN
      Act . un_except := Act . exception
    ; Act . un_arg    := Act . arg
    ; Act . exception := RuntimeError.Self ( )
    ; Act . arg := LOOPHOLE ( ORD ( SecondaryMap [ wasBlocked ] ) , ADDRESS ) 
    ; RTException . Raise ( Act ) 
    END BackstopPrimary 

; PROCEDURE Backstop ( VAR Act : RT0 . RaiseActivation ; wasBlocked : BOOLEAN )
    RAISES ANY
  (* Come here at raise time of an exception that is either blocked
     or unhandled. *) 

  = VAR LExc : RT0 . ExceptionPtr
  ; VAR LThreadInfoRef : ThreadInfoRefTyp
  ; VAR LIntArg : INTEGER
  ; VAR LIntUnArg : INTEGER
  ; VAR LAction : FailureActionTyp
  ; VAR LAllowedActions : FailureActionSetTyp
  ; VAR LKindMsg : TEXT 
  
  ; BEGIN
       LExc := Act . exception 
    ; IF LExc # RuntimeError . Self ( ) (* Programmer-declared exception. *) 
      THEN BackstopPrimary ( Act , wasBlocked )
      ELSE (* RTExcepton.E => runtime error. *) 
        LIntArg := LOOPHOLE ( Act . arg , INTEGER ) 
      ; IF LIntArg < ORD ( FIRST ( RuntimeError . T ) ) 
           OR LIntArg > ORD ( LAST ( RuntimeError . T ) )
        THEN LIntArg := ORD ( RuntimeError . T . Unknown )
        END (* IF *)
      ; IF NOT VAL ( LIntArg , RuntimeError . T ) IN SecondaryRtes  
        THEN (* Primary RT error. *)
          BackstopPrimary ( Act , wasBlocked )
        ELSE (* Secondary RT error (Unhandled or blocked), which
                is now also Unhandled or blocked. *)
          LIntUnArg := LOOPHOLE ( Act . un_arg , INTEGER ) 
        ; IF LIntUnArg < ORD ( FIRST ( RuntimeError . T ) ) 
             OR LIntUnArg > ORD ( LAST ( RuntimeError . T ) )
          THEN LIntUnArg := ORD ( RuntimeError . T . Unknown )
          END (* IF *)
        ; CASE <* NOWARN *> VAL ( LIntArg , RuntimeError . T )
          OF RuntimeError . T . UnhandledException
          => LKindMsg := "Unhandled: " 
          | RuntimeError . T . BlockedException
          => LKindMsg := "Not in RAISES clause: "
          END (* CASE *) 
        ; IF Act . un_except = BackoutRef ( )
             (* Primary was Failures.Backout. *) 
          THEN 
            Crash ( Act , LIntArg , LKindMsg , "Failures.Backout" ) 
          ELSIF Act . exception = IgnoreRef ( ) 
             (* Primary was Failures.Ignore.  This should not happen
                in the absence of bugs in Assertions, which should
                always catch Ignore. *) 
          THEN 
            Crash ( Act , LIntArg , LKindMsg , "Failures.Ignore" )
          ELSE (* Client code has passed on two chances to catch: the original
                  exception and a secondary exception (RT error: unhandled or
                  blocked), which also is now unhandled or blocked.  Now give
                  the human user some options. *)
            IF Act . un_except = AssertionFailureRef ( )
            THEN (* Only for AssertionFailure, is it clear that just raising
                    and catching an exception (Ignore) will resume execution
                    at the right place to ignore. *)  
              LAllowedActions := Assertions . AllowedActions 
            ELSE 
              LAllowedActions
                := FailureActionSetTyp
                     { FailureActionTyp . FaCrash
                     , FailureActionTyp . FaBackout
                     }
            END (* IF *)
          ; LThreadInfoRef := ThreadInfoRef ( ) 
          ; LAction
              := LThreadInfoRef . QueryProc
                   ( Act
                   , LKindMsg & ExcNameInternal ( Act , LExc , LIntArg )
                   , ActivationImage ( Act ) 
                   , LAllowedActions
                   ) 
          ; CASE LAction 
            OF FailureActionTyp . FaBackout 
              => (* Change to Failures.Backout, which client code
                    can catch to recover from the original exception. *)
                Act . exception := BackoutRef ( ) 
              ; RTException . Raise ( Act ) 
            | FailureActionTyp . FaIgnore 
              => (* This will work only when the original exception was
                    Assertions.AssertionFailure, and thus something
                    in Assertions is in the call chain, where it can catch
                    Failures.Ignore and return without action action. *)
                Act . exception := IgnoreRef ( ) 
              ; RTException . Raise ( Act ) 
            | FailureActionTyp . FaCrash 
              => Crash
                   ( Act
                   , LIntArg
                   , LKindMsg
                   , ExcNameInternal ( Act , LExc , LIntArg )
                   ) 
            END (* CASE *)
          END (* IF *)
        END (* IF *) 
      END (* IF *)
    END Backstop 

(* Getting RT0.ExceptionPtr values for certain declared exceptions: *)

; VAR GBackoutRef : RT0 . ExceptionPtr := NIL

; PROCEDURE BackoutRef ( ) : RT0 . ExceptionPtr
  (* The RT0.ExceptionPtr for exception Backout. *)

  = BEGIN
      IF GBackoutRef = NIL
      THEN 
        TRY
          RAISE Backout ( "" )    
        EXCEPT
        | Backout 
        => GBackoutRef
             := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ExceptionPtr )
        ELSE RETURN NIL 
        END (* EXCEPT *)
      END (* IF *)
    ; RETURN GBackoutRef 
    END BackoutRef

; VAR GIgnoreRef : RT0 . ExceptionPtr := NIL  

; PROCEDURE IgnoreRef ( ) : RT0 . ExceptionPtr
  (* The RT0.ExceptionPtr for exception Ignore. *)

  = BEGIN
      IF GIgnoreRef = NIL
      THEN 
        TRY
          RAISE Ignore   
        EXCEPT
        | Ignore 
        => GIgnoreRef
           := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ExceptionPtr ) 
        ELSE RETURN NIL 
        END (* EXCEPT *)
      END (* IF *)
    ; RETURN GIgnoreRef 
    END IgnoreRef

; VAR GAssertionFailureRef : RT0 . ExceptionPtr := NIL  

; PROCEDURE AssertionFailureRef ( ) : RT0 . ExceptionPtr
  (* The RT0.ExceptionPtr for exception Assertions.AssertionFailure. *)

  = BEGIN
      IF GAssertionFailureRef = NIL
      THEN 
        TRY
          RAISE Assertions . AssertionFailure ( "" )   
        EXCEPT
        | Assertions . AssertionFailure 
        => GAssertionFailureRef
           := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ExceptionPtr ) 
        ELSE RETURN NIL 
        END (* EXCEPT *)
      END (* IF *)
    ; RETURN GAssertionFailureRef 
    END AssertionFailureRef

; BEGIN (* Failures *)
    GOldBackstop := RTException . SetBackstop ( Backstop )
  END Failures 
. 
