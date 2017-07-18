
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE TreeBrowse 

(* An interactive, command-line browser for an Est. *) 

; IMPORT Rd 
; IMPORT Wr 

; IMPORT Assertions 
; IMPORT LbeStd 

; TYPE T <: Public 
; TYPE SessionTyp = T 

; CONST Brand = "TreeBrowse.T" 

; TYPE Public 
    = OBJECT 
      METHODS 
        initStdSession 
          ( RootEstRef : LbeStd . EstRootTyp 
          ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
          ) 
          : SessionTyp
        (* uses standard in and out. *)  
      ; initSessionRdWr
          ( RootEstRef : LbeStd . EstRootTyp 
          ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
          ; RdT : Rd . T 
          ; WrT : Wr . T 
          ) 
          : SessionTyp
      END (* OBJECT *) 

; PROCEDURE InitStdSession 
    ( Session : SessionTyp 
    ; RootEstRef : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ) 
    : SessionTyp 
  (* uses standard in and out. *)  

; PROCEDURE InitSessionRdWr 
    ( Session : SessionTyp 
    ; RootEstRef : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ; RdT : Rd . T 
    ; WrT : Wr . T 
    ) 
    : SessionTyp 

; PROCEDURE Browse 
    ( EstRoot : LbeStd . EstRootTyp 
    ; Lang : LbeStd . LangTyp := LbeStd . LangNull 
    ; TreeId : TEXT := "" 
    ) 
  RAISES { Assertions . AssertionFailure } 
  (* A complete browse, using Stdio. *) 
(* TODO: Several places duplicate this proc.  Remove them. *) 

; PROCEDURE Interp ( Session : SessionTyp ) 
  RAISES { Assertions . AssertionFailure } 

; END TreeBrowse 
. 

