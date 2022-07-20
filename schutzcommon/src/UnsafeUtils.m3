  
(* -----------------------------------------------------------------------1- *)
(* File UnsafeUtils.m3  Modula-3 source code.                                *)
(* Copyright 2010 .. 2022, Rodney M. Bates.                                  *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *) 
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE UnsafeUtils 

(* From m3core: *)
; IMPORT RT0 
(* ; IMPORT RTException *)
; IMPORT RTHeapRep
; IMPORT RTIO 
; IMPORT RTType 
; IMPORT Word 
 
(* EXPORTED *) 
; PROCEDURE IntOfRefany ( Ref : REFANY ) : INTEGER 

  = BEGIN (* IntOfRefany *) 
      RETURN LOOPHOLE ( Ref , INTEGER ) 
    END IntOfRefany  

(* EXPORTED *) 
; PROCEDURE RefanyOfInt ( I : INTEGER ) : REFANY 

  = BEGIN (* RefanyOfInt *) 
      RETURN LOOPHOLE ( I , REFANY ) 
    END RefanyOfInt   

(* EXPORTED *) 
; PROCEDURE NULLOfInt ( I : INTEGER ) : <* NOWARN *> NULL

  = BEGIN (* NULLOfInt *) 
      RETURN LOOPHOLE ( I , NULL ) 
    END NULLOfInt   

(* EXPORTED *) 
; PROCEDURE PtrTo8CharArray ( VAR W : Word . T ) 
  : UNTRACED REF ARRAY [ 0 .. 7 ] OF CHAR 

  = BEGIN 
      RETURN ( LOOPHOLE ( ADR ( W ) , UNTRACED REF ARRAY [ 0 .. 7 ] OF CHAR ) )
    END PtrTo8CharArray 

(* EXPORTED *) 
; PROCEDURE AdrToRT0_ActivationPtr ( Address : ADDRESS ) : RT0 . ActivationPtr

  = BEGIN
      RETURN LOOPHOLE ( Address , RT0 . ActivationPtr ) 
    END AdrToRT0_ActivationPtr

(* PutExcept is copied from RTExFrame.  A (possibly) different) version
   is in RtExStack.  Its signature needs to go into RTException.i3, to
   do this properly.  That is a runtime system change, not to be undertaken
   lightly.  This is a temporary expedient to keep moving forward on
   schutz. *)
(* WARNING: it is misnamed; should be PutActivation!! *) 

(* Later note: The version in RTExStack looks to be identical. *)

(* OR, maybe keep here and write to a TEXT or Wr.T, for more flexibility? *)

; PROCEDURE PutExcept (tag: TEXT;  READONLY a: RT0.RaiseActivation) =
  BEGIN
    RTIO.PutText ("---> ");         RTIO.PutText (tag);
    RTIO.PutText (":  en=");        RTIO.PutAddr (a.exception);
    RTIO.PutText (" uid=");         RTIO.PutHex (a.exception.uid);
    RTIO.Flush ();
    RTIO.PutText (" ");             RTIO.PutString (a.exception.name);
    RTIO.PutText ("  arg=");        RTIO.PutAddr (a.arg);
    RTIO.PutText ("\n  module: ");  RTIO.PutAddr (a.module);
    IF (a.module # NIL) AND (a.module.file # NIL) THEN
      RTIO.PutText ("  ");          RTIO.PutString (a.module.file);
    END;
    RTIO.PutText ("\n  line: ");    RTIO.PutInt (a.line);
    RTIO.PutText ("   pc: ");       RTIO.PutAddr (a.pc);
    RTIO.PutText ("   info0: ");    RTIO.PutAddr (a.info0);
    RTIO.PutText ("   info1: ");    RTIO.PutAddr (a.info1);
    IF (a.un_except # NIL) THEN
      RTIO.PutText ("\n  unhandled: ");
      RTIO.PutText (" ");             RTIO.PutString (a.un_except.name);
      RTIO.PutText ("  arg=");        RTIO.PutAddr (a.un_arg);
    END;
    RTIO.PutText ("\n");
    RTIO.Flush ();
  END PutExcept 

(* EXPORTED: *) 
; PROCEDURE DisplayActivation
    ( Tag : TEXT ; Addr : ADDRESS (* RT0.ActivationPtr *) )
  (* Display, on command line, an interpretation of a value gotten from
     'Compiler.ThisException', which, contrary to the name 'ThisException',
     is a RT0.ActivationPtr, NOT an ExceptionPtr!! *)

  = BEGIN
      (* RTException . *) PutExcept
        ( Tag , AdrToRT0_ActivationPtr ( Addr ) ^ ) 
    END DisplayActivation 

(* EXPORTED *) 
; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : INTEGER 

  = BEGIN 
      RETURN
        RTType . Get ( TC ) . dataSize (* DIV BitsPerAddrUnit *) 
        + ADRSIZE ( RTHeapRep . Header ) 
    END ObjectSize 

; BEGIN (* UnsafeUtils *) 
  END UnsafeUtils 
. 
