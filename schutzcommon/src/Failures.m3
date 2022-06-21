
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
; IMPORT RuntimeError
; IMPORT Thread

; IMPORT Assertions
; IMPORT MessageCodes

; TYPE AFT = MessageCodes . T 

; VAR GOldBackstop : RTException . Backstop

; TYPE BkstpInfoTyp = RECORD
      Link : BkstpInfoRefTyp := NIL
    ; Thread : Thread . T 
    ; BackstopProc : RTException . Backstop := NIL
    ; QueryProc : QueryProcTyp := NIL 
    END
    
; REVEAL BkstpInfoRefTyp = REF BkstpInfoTyp

; VAR GBkstpInfoList : BkstpInfoRefTyp := NIL 

; VAR GBkstpInfoRefDefault := BkstpInfoRefTyp { NIL , NIL , NIL }

(* EXPORTED *) 
; PROCEDURE RegisterFailureActions 
    ( BackstopProc : RTException . Backstop ; QueryProc : QueryProcTyp )
  (* Register these procedures for Thread.Self. *) 

  = VAR LInfoRef , LTrailingInfoRef , LNewInfoRef : BkstpInfoRefTyp 
  ; VAR LThreadSelf : Thread . T
  
  ; BEGIN
      LThreadSelf := Thread . Self ( )
    ; IF LTreadSelf = NIL THE RETURN END (* IF *) 
    ; LNewInfoRef 
        := NEW ( BkstpInfoRefTyp 
               , Link := GBkstpInfoList
               , Thread := LThreadSelf
               , BackstopProc := BackstopProc
               , QueryProc := QueryProc
               )
    ; TYPECASE LThreadSelf 
      OF Assertions . AssertThreadT ( TThread )
      => TThread . BkstpInfoRef := LNewInfoRef 
      ELSE
        IF GBkstpInfoList = NIL
           OR GBkstpInfoList . Thread = LThreadSelf 
        THEN (* Possibly reregister with the new info. *)
          LNewInfoRef . Link := GBkstpInfoList
        ; GBkstpInfoList := LNewInfoRef
        ELSE 
          LTrailingInfoRef := NIL 
        ; LInfoRef := GBkstpInfoList 
        ; LOOP
            IF LInfoRef = NIL
            THEN
              LTrailingInfoRef . Link  := LNewInfoRef 
            ; EXIT 
            ELSIF LInfoRef . Tread = LThreadSelf
            THEN (* Possibly reregister with the new info. *)
              LNewInfoRef . Link := LTrailingInfoRef ^ . Link 
            ; LTrailingInfoRef ^ . Link := LNewInfoRef
            ; EXIT
            ELSE
              LTrailingInfoRef := LInfoRef 
            ; LInfoRef := LInfoRef . Link 
            END (* IF *)
          END (* LOOP *)
        END (* IF *) 
      END TYPECASE 
    END RegisterFailureActions 

; PROCEDURE ThreadBkstpInfoRef ( ) : BkstpInfoRefTyp 

  = VAR LBkstpInfoRef : BkstpInfoRefTyp 
  ; VAR LThreadSelf : Thread . T
  
  ; BEGIN
      LThreadSelf := Thread . Self ( )
    ; TYPECASE LThreadSelf 
      OF NULL
      => RETURN
           GBkstpInfoRefTyp
             { NIL , LThreadSelf , BkstpProcDefault , QueryDefault }  
      | Assertions . AssertThreadT ( TThread )
      => RETURN TThread . BkstpInfoRef
      ELSE
        LBkstpInfoRef := GBkstpInfoList 
      ; LOOP
          IF LBkstpInfoRef = NIL
          THEN
            RETURN
              GBkstpInfoRefTyp
                { NIL , LThreadSelf , BkstpProcDefault , QueryDefault } 
          END (* IF *)
        ; IF LBkstpInfoRef . Tread = LThreadSelf
          THEN RETURN LBkstpInfoRef
          END (* IF *)
        ; LBkstpInfoRef := LBkstpInfoRef . Link 
        END (* LOOP *) 
      END TYPECASE 
    END ThreadBkstpInfoRef

; CONST SecondaryRtes
    = SET OF RuntimeError . T
              { RuntimeError . T . UnhandledException
              , RuntimeError . T . BlockedException
              }

; PROCEDURE QueryDefault ( RT0 . ActivationPtr ) : FailureActionTyp

  = BEGIN
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
    END QueryDefault     

; PROCEDURE Backstop ( VAR Act : RT0 . RaiseActivation ; raises : BOOLEAN )
    RAISES ANY
  (* Come here at raise time of an exception that is either blocked
     or unhandled. *) 

  = VAR LBkstpInfoRef : BkstpInfoRefTyp
  ; VAR LIntArg : INTEGER
  ; VAR LAction : FailureActionTyp
  ; VAR LMsg : TEXT 
  
  = VAR LExc , LNewExc : RT0 . ExceptionPtr
  ; VAR LFileString : ADDRESS := NIL 
  ; VAR LLocation , LMessage : TEXT := ""
  ; VAR LDoTerminate : BOOLEAN 
  
  ; BEGIN
      TRY (* FINALLY *) 
        EVAL RTException . SetBackstop ( GOldBackstop )
        (* ^Temporarily revert to the RT system's default backstop
            so we can delegate to it. *)
      ; LBkstpInfoRef := ThreadBkstpInfoRef ( ) 
      ; IF LBkstpInfoRef = NIL (* No special backstop for current thread. *)
           (* How did we get here? *) 
        THEN (* Let the default backstop handle it. *)  
          RTException . InvokeBackstop ( Act , raises )
        ELSE 
          LExc := Act . exception 
        ; IF LExc # RuntimeError . Self ( ) (* Programmer-declared exception. *) 
          THEN RTException . InvokeBackstop ( Act , raises )
               (* ^Will turn it into a secondary RT error. *)
          ELSE (* RTExcepton.E => runtime error. *) 
            LIntArg := LOOPHOLE ( Act . arg , INTEGER ) 
          ; IF LIntArg < ORD ( FIRST ( RuntimeError . T ) ) 
               OR LIntArg > ORD ( LAST ( RuntimeError . T ) )
            THEN LIntArg := ORD ( RuntimeError . T . Unknown )
            END (* IF *)
          ; IF NOT VAL ( LIntArg , RuntimeError . T ) IN SecondaryRtes  
            THEN (* Primary RT error. *)
              RTException . InvokeBackstop ( Act , raises )
              (* ^Will turn it into a secondary RT error. *)
            ELSE (* Secondary RT error. unhandled or blocked. *)
              LIntUnArg := LOOPHOLE ( Act . un_arg , INTEGER ) 
            ; IF LIntUnArg < ORD ( FIRST ( RuntimeError . T ) ) 
                 OR LIntUnArg > ORD ( LAST ( RuntimeError . T ) )
              THEN LIntUnArg := ORD ( RuntimeError . T . Unknown )
              END (* IF *)
            ; IF Act . un_except = BackoutRef ( )
                 (* Primary was Failures.Backout. *) 
              THEN 
                Crash ( Act , Act . un_except , Act . un_arg
                      , "Uncaught Failures.Backout "
                      ) 
              ELSIF Act . exception = IgnoreRef ( ) 
                 (* Primary was Failures.Ignore.  This should not happen
                    in the absence of bugs in Assertions. *) 
              THEN (* 
                Crash ( Act , Act . un_except , Act . un_arg
                      , "Uncaught Failures.Ignore "
                      )
              ELSE (* Client code has passed on two chances to catch: the original
                      exception and a secondary exception.  Now give the human user
                      some options. *)
                LAction := LBkstpInfoRef . QueryProc ( Act ) 
              ; CASE LAction OF
                OF FailureActionTyp . FaBackout  
                  => (* Change to Failures.Backout, which client code
                        can catch to recover from the original exception. *)
                     (* If un_except = NIL, Backout was raised by client code. *)
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
                  => CASE VAL ( LIntArg , RuntimeErr . T )
                     OF RuntimeError . T . UnhandledException
                     => LMsg := "Unhandled exception: " 
                     | RuntimeError . T . BlockedException
                     => LMsg := "Not in RAISES clause: "
                     END (* CASE *) 
                  ; Crash ( Act , Act . un_except , Act . un_arg , LMsg )                          ) 
                END (* CASE *)
              END (* IF *)
            END (* IF *) 
          END (* IF *)
        END (* TYPECASE *) 
      FINALLY (* Rehook ourself as backstop, for next time. *) 
        GOldBackstop :=  RTException . SetBackstop ( Backstop )
      END (* FINALLY *) 
    END Backstop 

; VAR GBackoutRef : RT0 . ExceptionPtr := NIL

; PROCEDURE BackoutRef ( ) : RT0 . ExceptionPtr
  (* The RT0.ExceptionPtr for exception Assertions.Backout. *)

  = BEGIN
      IF GBackoutRef = NIL
      THEN 
        TRY
          RAISE Assertions . Backout   
        EXCEPT
        | Assertions . Backout 
        => GBackoutRef
           := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ActivationPtr )
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
           := LOOPHOLE ( Compiler . ThisException ( ) , RT0 . ActivationPtr ) 
        ELSE RETURN NIL 
        END (* EXCEPT *)
      END (* IF *)
    ; RETURN GIgnoreRef 
    END IgnoreRef

; BEGIN (* Failures *)
    GOldBackstop := RTException . SetBackstop ( Backstop )
  END Failures 
. 
