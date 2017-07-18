
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE RefSet 

(* Sets of reference values.  Implemented as linear search, with obvious
   performance implications.  But it's immune to damamge caused by the
   GC's moving heap objects.
*) 

; CONST InitialCt = 8 

; TYPE ArrayTyp = ARRAY OF REFANY 

; TYPE ArrayRefTyp = REF ArrayTyp 

; REVEAL T = BRANDED "RefSet.T" OBJECT 
    Values : ArrayRefTyp (* INVARIANT: Never NIL *) 
  ; Ct : CARDINAL 
  END 

(* VISIBLE: *) 
; PROCEDURE Empty ( ) : T 
  (* Return a new, empty, set of references *)

  = BEGIN
      RETURN NIL 
    END Empty  

; PROCEDURE MakeInitial ( ) : T 

  = BEGIN 
      RETURN 
        NEW ( T , Values := NEW ( ArrayRefTyp , InitialCt ) , Ct := 0   ) 
    END MakeInitial 

; PROCEDURE Expand ( VAR Set : T ) 

  = VAR LOldCt : INTEGER 
  ; VAR LNewValues : ArrayRefTyp 

  ; BEGIN
      LOldCt := NUMBER ( Set . Values ^ ) 
    ; LNewValues := NEW ( ArrayRefTyp , LOldCt * 2 ) 
    ; SUBARRAY ( LNewValues ^ , 0 , LOldCt ) := Set . Values ^ 
    ; Set . Values := LNewValues   
    END Expand 

(* VISIBLE: *) 
; PROCEDURE Add ( Value : REFANY ; VAR Set : T ) 
  (* Add Value to Set. *) 

  = BEGIN
      IF Set = NIL 
      THEN
        Set := MakeInitial ( ) 
      END 
    ; IF NOT IsElement ( Value , Set ) 
      THEN
        IF Set . Ct >= NUMBER ( Set . Values ^ ) 
        THEN
          Expand ( Set ) 
        END 
      ; Set . Values ^ [ Set . Ct ] := Value 
      ; INC ( Set . Ct ) 
      END 
    END Add    

(* VISIBLE: *) 
; PROCEDURE IsElement ( Value : REFANY ; Set : T ) : BOOLEAN  

  = VAR LSs : INTEGER 

  ; BEGIN
      IF Set = NIL OR Set . Ct = 0 
      THEN RETURN FALSE
      ELSE
        LSs := 0  
      ; LOOP
          IF Set . Values ^ [ LSs ] = Value 
          THEN RETURN TRUE 
          ELSE
            INC ( LSs ) 
          ; IF LSs >= Set . Ct  
            THEN  RETURN FALSE 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      END (* IF *) 
    END IsElement 

; BEGIN 
  END RefSet 
. 
