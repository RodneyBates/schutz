
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE UnsafeUtils 

(* From m3core: *) 
; IMPORT Fingerprint 
; IMPORT RT0 
; IMPORT RTHeapRep 
; IMPORT RTType 
; IMPORT RTTypeFP
; IMPORT Text 
 
; IMPORT Assertions 
; IMPORT PortTypes 

(* VISIBLE: *) 
; PROCEDURE IntOfRefany ( Ref : REFANY ) : INTEGER 

  = BEGIN (* IntOfRefany *) 
      RETURN LOOPHOLE ( Ref , INTEGER ) 
    END IntOfRefany  

(* VISIBLE: *) 
; PROCEDURE RefanyOfInt ( I : INTEGER ) : REFANY 

  = BEGIN (* RefanyOfInt *) 
      RETURN LOOPHOLE ( I , REFANY ) 
    END RefanyOfInt   

(* VISIBLE: *) 
; PROCEDURE NULLOfInt ( I : INTEGER ) : NULL

  = BEGIN (* RefanyOfInt *) 
      RETURN LOOPHOLE ( I , NULL ) 
    END NULLOfInt   

(* VISIBLE: *) 
; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : PortTypes . Int32Typ 

  = BEGIN 
      RETURN
        RTType . Get ( TC ) . dataSize (* DIV BitsPerAddrUnit *) 
        + ADRSIZE ( RTHeapRep . Header ) 
    END ObjectSize 

(* The following have to be coded with care, because they use RT0 and RTType, 
   which differ between PM3 and CM3.
*) 

; PROCEDURE TextEqRT0String ( Left : TEXT ; Right : RT0 . String ) : BOOLEAN 

  = VAR LLeftLen : CARDINAL  
  ; VAR LLeftCharNo : CARDINAL 
  ; VAR LLeftChar : CHAR 
  ; VAR LRightPtr : RT0 . String 
  ; VAR LRightChar : CHAR 

  ; BEGIN 
      IF Left = NIL 
      THEN 
        RETURN Right = NIL 
      ELSE 
        IF Right = NIL 
        THEN 
          RETURN FALSE 
        ELSE 
          LLeftLen := Text . Length ( Left ) 
        ; LLeftCharNo := 0 
        ; LRightPtr := Right 
        ; LRightChar := LRightPtr ^ 
        ; LOOP 
            IF LLeftCharNo = LLeftLen 
            THEN 
              RETURN LRightChar = '\000'
            ELSE 
              LLeftChar := Text . GetChar ( Left , LLeftCharNo ) 
            ; IF LLeftChar # LRightChar
              THEN 
                RETURN FALSE 
              ELSE 
                INC ( LLeftCharNo ) 
              ; LRightPtr 
                  := LOOPHOLE 
                       ( LOOPHOLE ( LRightPtr , ADDRESS ) + 1 , RT0 . String ) 
              ; LRightChar := LRightPtr ^ 
              END (* IF *) 
            END (* IF *) 
          END (* LOOP *) 
        END (* IF *) 
      END (* IF *) 
    END TextEqRT0String 

(* For now, no need to make this visible, as it is intended primarily
   to be called by an m3gdb command.
*) 
; PROCEDURE TypecodeOfTypeName ( Name : TEXT ) : TypeCodeTyp 

  = VAR LTypecode : TypeCodeTyp 
  ; VAR LMaxTypecode : TypeCodeTyp 
  ; VAR LTypeDefn : RT0 . TypeDefn 

  ; BEGIN 
      LTypecode := 0 
    ; LMaxTypecode := RTType . MaxTypecode ( )
    ; LOOP 
        IF LTypecode > LMaxTypecode 
        THEN 
          RETURN RTType . NoSuchType 
        ELSE
          LTypeDefn := RTType . Get ( LTypecode ) 
        ; IF LTypeDefn # NIL 
          THEN 
            IF TextEqRT0String ( Name , LTypeDefn ^ . name ) 
            THEN 
              RETURN LTypecode 
            END (* IF *) 
          END (* IF *)  
        ; INC ( LTypecode ) 
        END (* IF *) 
      END (* LOOP *) 
    END TypecodeOfTypeName  

; VAR GrammarSubName : TEXT := "Ldl0Semantics.GrammarSubTyp" 

(* This duplicates the function of RTTypeFP.FromFingerprint, but it uses
   a brute-force linear search, for more confidence than the hash algorithm
   used in FromFingerprint.
*) 
(* For now, no need to make this visible, as it is intended primarily
   to be called by an m3gdb command.
*) 
; PROCEDURE TypecodeOfFingerprint ( FP : Fingerprint . T ) : TypeCodeTyp 

  = VAR LTypecode : TypeCodeTyp 
  ; VAR LMaxTypecode : TypeCodeTyp 
  ; VAR LTypeDefn : RT0 . TypeDefn 

  ; BEGIN 
      LTypecode := 0 
    ; LMaxTypecode := RTType . MaxTypecode ( )
    ; LOOP 
        IF LTypecode > LMaxTypecode 
        THEN 
          RETURN RTType . NoSuchType 
        ELSE
          LTypeDefn := RTType . Get ( LTypecode ) 
        ; IF LTypeDefn # NIL 
          THEN 
            IF FP = LOOPHOLE ( LTypeDefn ^ . fp , Fingerprint . T )   
            THEN 
              RETURN LTypecode 
            END (* IF *) 
          ; IF LOOPHOLE ( FP . byte , RT0 . Fingerprint ) = LTypeDefn ^ . fp 
            THEN 
              RETURN LTypecode 
            END (* IF *) 
          END (* IF *)  
        ; INC ( LTypecode ) 
        END (* IF *) 
      END (* LOOP *) 
    END TypecodeOfFingerprint  


; BEGIN (* UnsafeUtils *) 
    EVAL TypecodeOfTypeName ( "Text.T" ) 
  END UnsafeUtils 
. 
