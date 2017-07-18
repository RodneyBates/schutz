
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE TokRelation 

(* A relation on INTEGER X INTEGER.  Used for tokens. *) 

; IMPORT IntSets 
; IMPORT LbeStd 

; TYPE LeftArrayTyp = ARRAY OF IntSets . T 
; TYPE LeftArrayRefTyp = REF LeftArrayTyp 

; REVEAL T 
    = BRANDED 
        "TokRelation.T" 
        OBJECT 
          LeftMin : ElementTyp := 0 
        ; LeftMax : ElementTyp := 0 
        ; LeftArrayRef : LeftArrayRefTyp := NIL 
        END (* OBJECT *) 

(* VISIBLE: *) 
; PROCEDURE New ( LeftMin : ElementTyp := 0 ; LeftMax : ElementTyp ) : T 

  = VAR LLeftArrayRef : LeftArrayRefTyp 

  ; BEGIN (* New *) 
      LLeftArrayRef := NEW ( LeftArrayRefTyp , LeftMax - LeftMin + 1 ) 
    ; FOR FI := 0 TO NUMBER ( LLeftArrayRef ^ ) - 1 
      DO LLeftArrayRef ^ [ FI ] := NIL 
      END (* FOR *) 
    ; RETURN 
        NEW 
          ( T 
          , LeftArrayRef := LLeftArrayRef 
          , LeftMin := LeftMin 
          , LeftMax := LeftMax 
          ) 
    END New 

(* VISIBLE: *) 
; PROCEDURE AddRule 
    ( VAR Relation : T ; Left : ElementTyp ; Right : ElementTyp ) 

  = VAR LLeftElem : ElementTyp 

  ; BEGIN (* AddRule *) 
      LLeftElem := Left - Relation . LeftMin 
    ; WITH WLeft = Relation . LeftArrayRef ^ [ LLeftElem ] 
      DO WLeft := IntSets . Include ( WLeft , Right ) 
      END (* WITH *) 
    END AddRule 

(* VISIBLE: *) 
; PROCEDURE SetRightRelatives 
    ( Relation : T ; Left : ElementTyp ; Relatives : IntSets . T ) 

  = VAR LLeftElem : ElementTyp 

  ; BEGIN (* SetRightRelatives *) 
      LLeftElem := Left - Relation . LeftMin 
    ; Relation . LeftArrayRef ^ [ LLeftElem ] := Relatives  
    END SetRightRelatives 

(* VISIBLE: *) 
; PROCEDURE Close ( VAR Relation : T ) 

(* TODO: Use a better algorithm for this. *) 

  = VAR Close_DidConverge : BOOLEAN := TRUE 

  ; BEGIN (* Close *) 
      LOOP (* Until closure converges. *) 
        Close_DidConverge := TRUE 
      ; FOR FBiasedLeft := 0 TO Relation . LeftMax - Relation . LeftMin 
        DO WITH WSet = Relation . LeftArrayRef ^ [ FBiasedLeft ] 
           DO PROCEDURE Rule ( Right : ElementTyp ) 

              = VAR LBiasedTransLeft : ElementTyp 

              ; BEGIN (* Rule *) 
                  IF Relation . LeftMin <= Right 
                     AND Right <= Relation . LeftMax 
                  THEN (* Right is an unbiased left for transitive rules. *) 
                    LBiasedTransLeft := Right - Relation . LeftMin 
                  ; WITH 
                      WTransSet 
                      = Relation . LeftArrayRef ^ [ LBiasedTransLeft ] 
                    DO PROCEDURE TransRule ( TransRight : ElementTyp ) 

                       = BEGIN (* TransRule *) 
                           IF NOT IntSets . IsElement ( TransRight , WSet ) 
                           THEN 
                             WSet 
                               := IntSets . Include ( WSet , TransRight ) 
                           ; Close_DidConverge := FALSE 
                           END (* IF *) 
                         END TransRule 

                    ; <* FATAL ANY *> (* Can't happen, TransRule *) 
                      BEGIN (* Block 2 *) 
                        IntSets . ForAllDo ( WTransSet , TransRule ) 
                      END (* Block 2 *) 
                    END (* WITH *) 
                  END (* IF *) 
                END Rule 

           ; <* FATAL ANY *> (* Can't happen, Rule *) 
             BEGIN (* Block 1 *) 
               IntSets . ForAllDo ( WSet , Rule ) 
             END (* Block 1 *) 
           END (* WITH *) 
        END (* FOR *) 
      ; IF Close_DidConverge THEN EXIT END (* IF *) 
      END (* LOOP *) 
    END Close 

(* VISIBLE: *) 
; PROCEDURE IsElement ( Relation : T ; Left , Right : ElementTyp ) : BOOLEAN 

  = VAR LLeft : ElementTyp 

  ; BEGIN (* IsElement *) 
      IF Relation . LeftMin <= Left 
         AND Left <= Relation . LeftMax 
      THEN 
        LLeft := Left - Relation . LeftMin 
      ; RETURN 
          IntSets . IsElement ( Right , Relation . LeftArrayRef ^ [ LLeft ] ) 
      ELSE 
        RETURN FALSE 
      END (* IF *) 
    END IsElement 

(* VISIBLE: *) 
; PROCEDURE RightRelatives ( Relation : T ; Left : ElementTyp ) : IntSets . T 

  = VAR LLeft : ElementTyp 

  ; BEGIN (* RightRelatives *) 
      IF Relation . LeftMin <= Left AND Left <= Relation . LeftMax 
      THEN 
        LLeft := Left - Relation . LeftMin 
      ; RETURN Relation . LeftArrayRef ^ [ LLeft ] 
      ELSE 
        RETURN IntSets . Empty ( ) 
      END (* IF *) 
    END RightRelatives 

(* VISIBLE: *) 
; PROCEDURE GroundRightRelatives ( Relation : T ; Left : ElementTyp ) 
  : IntSets . T 

  = VAR LLeft : ElementTyp 

  ; BEGIN (* GroundRightRelatives *) 
      IF Relation . LeftMin <= Left AND Left <= Relation . LeftMax 
      THEN 
        LLeft := Left - Relation . LeftMin 
      ; RETURN 
          IntSets . Difference 
            ( Relation . LeftArrayRef ^ [ LLeft ] 
            , IntSets . Range ( Relation . LeftMin , Relation . LeftMax ) 
            )  
      ELSE 
        RETURN IntSets . Empty ( ) 
      END (* IF *) 
    END GroundRightRelatives 

(* VISIBLE: *) 
; PROCEDURE ForAllGroundMembers 
    ( Relation : T 
    ; ClassTok : LbeStd . TokTyp 
    ; VisitMember : ProcOfTokTyp 
    ) 
  RAISES ANY 

  = PROCEDURE VisitRight ( Elem : ElementTyp ) 
    RAISES ANY 

    = BEGIN (* VisitRight *) 
        IF Relation . LeftMin <= Elem AND Elem <= Relation . LeftMax 
        THEN (* This is another class. Ignore it. *) 
        ELSE (* It's a ground member. *) 
          VisitMember ( Elem ) 
        END (* IF *) 
      END VisitRight 

  ; VAR LLeft : ElementTyp 
  ; VAR LSet : IntSets . T 

  ; BEGIN (* ForAllGroundMembers *) 
      IF Relation . LeftMin <= ClassTok AND ClassTok <= Relation . LeftMax 
      THEN 
        LLeft := ClassTok - Relation . LeftMin 
      ; LSet := Relation . LeftArrayRef ^ [ LLeft ] 
      ; IntSets . ForAllDo ( LSet , VisitRight ) 
      END (* IF *) 
    END ForAllGroundMembers 

; BEGIN (* TokRelation *) 
  END TokRelation 
. 
