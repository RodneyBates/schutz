
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LangMap 

(* Alter the map from (numeric) language codes of type LbeStd.LangTyp to 
   references to a record full of language-specific information.
*) 

; IMPORT LbeStd 
; IMPORT LdlSemantics 

; TYPE MapArrayTyp = ARRAY OF LdlSemantics . LangInfoRefTyp 
; TYPE MapArrayRefTyp = REF MapArrayTyp 

; VAR MapRef : MapArrayRefTyp := NIL 

(* TODO: Make this a self-expanding map. *) 

(* VISIBLE: *) 
; PROCEDURE AddOrChange 
    ( Lang : LbeStd . LangTyp 
    ; LangInfoRef : LdlSemantics . LangInfoRefTyp 
    ) 

  = BEGIN (* AddOrChange *) 
      IF 0 <= Lang AND Lang < LbeStd . LangNull 
      THEN 
        MapRef ^ [ Lang ] := LangInfoRef 
      END (* IF *) 
    END AddOrChange 

(* VISIBLE: *) 
; PROCEDURE Remove ( Lang : LbeStd . LangTyp ) 

  = BEGIN (* Remove *) 
      IF 0 <= Lang AND Lang < LbeStd . LangNull 
      THEN 
(* TODO: Remove suffixes from SuffixMap *) 
        MapRef ^ [ Lang ] := NIL 
      END (* IF *) 
    END Remove 

(* VISIBLE: *) 
; PROCEDURE LangInfo 
    ( Lang : LbeStd . LangTyp ) : LdlSemantics . LangInfoRefTyp 

  = BEGIN (* LangInfo *) 
      IF 0 <= Lang AND Lang < LbeStd . LangNull 
      THEN 
        RETURN MapRef ^ [ Lang ] 
      ELSE 
        RETURN NIL 
      END (* IF *) 
    END LangInfo 

; BEGIN (* LangMap *) 
    MapRef := NEW ( MapArrayRefTyp , LbeStd . LangNull ) 
  ; FOR I := 0 TO LAST ( MapRef ^ ) - 1 
    DO MapRef ^ [ I ] := NIL 
    END (* FOR *) 
  END LangMap 
. 
