
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE BuildLexMachine 

(* Build a table for LexTable to use.  Entire table must be built
   before it can be used.  Calls must follow the protocol given by\
   the regular expression:
     MakeEmpty AddPair* Build
*) 

; IMPORT Text 

; IMPORT LexTable 
; IMPORT LexTableRep  
; IMPORT PortTypes 

; TYPE MapSsTyp = CHAR  

; TYPE MapTyp = ARRAY MapSsTyp OF TempStateTyp 
; TYPE MapRefTyp = REF MapTyp 

; TYPE TempStateTyp = (* ABSTRACT *) OBJECT END 

; TYPE SingletonTempStateTyp 
       = TempStateTyp OBJECT 
           Value : LexTable . ValueTyp 
         ; String : TEXT 
         ; ReverseMap : BOOLEAN 
         END 

; TYPE MultTempStateTyp 
       = TempStateTyp OBJECT 
           MapRef : MapRefTyp := NIL 
         ; MinOccupied : MapSsTyp := LAST ( MapSsTyp ) 
         ; MaxOccupied : MapSsTyp := FIRST ( MapSsTyp ) 
         ; StateNo : LexTableRep . StateNoTyp := 0 
         END 

; VAR GMachine : TempStateTyp := NIL 
; VAR GNextStateNo : LexTableRep . StateNoTyp := 0 
; VAR GRuleCt : PortTypes . Int32Typ := 0 
; VAR GMinValue : LexTable . ValueTyp := LAST ( LexTable . ValueTyp ) 
; VAR GMaxValue : LexTable . ValueTyp := FIRST ( LexTable . ValueTyp ) 

(* VISIBLE *) 
; PROCEDURE MakeEmpty ( ) 

  = BEGIN 
      GMachine := NIL 
    ; GNextStateNo := 0 
    ; GRuleCt := 0 
    ; GMinValue := LAST ( LexTable . ValueTyp ) 
    ; GMaxValue := FIRST ( LexTable . ValueTyp ) 
    END MakeEmpty 

; PROCEDURE NextStateNo ( VAR Next : LexTableRep . StateNoTyp ) 
  : LexTableRep . StateNoTyp  
  (* Has SIDE EFFECTS! *) 

  = VAR LResult : LexTableRep . StateNoTyp 

  ; BEGIN 
      LResult := Next
    ; INC ( Next ) 
    ; RETURN LResult 
    END NextStateNo 

; PROCEDURE EmptyMapRef ( ) : MapRefTyp 

  = VAR LResult : MapRefTyp 

  ; BEGIN 
      LResult := NEW ( MapRefTyp ) 
    ; FOR RI := FIRST ( MapSsTyp ) TO LAST ( MapSsTyp ) 
      DO LResult ^ [ RI ] := NIL 
      END (* FOR *) 
    ; RETURN LResult 
    END EmptyMapRef 

; PROCEDURE EmptyMultState ( ) : MultTempStateTyp 

  = VAR LResult : MultTempStateTyp 

  ; BEGIN 
      LResult := NEW ( MultTempStateTyp ) 
    ; LResult . MapRef := EmptyMapRef ( ) 
    ; LResult . MinOccupied := LAST ( MapSsTyp ) 
    ; LResult . MaxOccupied := FIRST ( MapSsTyp ) 
    ; LResult . StateNo := NextStateNo ( GNextStateNo ) 
    ; RETURN LResult 
    END EmptyMultState 

(* VISIBLE *) 
; PROCEDURE AddPair 
    ( AddString : TEXT 
    ; Value : LexTable . ValueTyp 
    ; ReverseMap : BOOLEAN := TRUE  
    ) 

  = PROCEDURE ApMapElem 
      ( MultState : MultTempStateTyp 
      ; String : TEXT 
      ; CharSs : INTEGER 
      ; Value : LexTable . ValueTyp 
      ; ReverseMap : BOOLEAN 
      ) 
    (* First char of Tail selects within this MultState. *) 

    = VAR LLength : INTEGER 
    ; VAR LChar : CHAR 
    ; VAR LCharSs : INTEGER 

    ; BEGIN 
        LLength := Text . Length ( String ) 
      ; IF CharSs = LLength  
        THEN 
          LChar := LexTableRep . NullChar 
        ; LCharSs := CharSs 
        ELSE 
          LChar := Text . GetChar ( String , CharSs ) 
        ; LCharSs := CharSs + 1  
        END (* IF *) 
      ; ApRecurse 
          ( (* VAR *) MultState . MapRef ^ [ LChar ] 
          , String 
          , LCharSs 
          , Value 
          , ReverseMap 
          ) 
      ; MultState . MinOccupied := MIN ( MultState . MinOccupied , LChar ) 
      ; MultState . MaxOccupied := MAX ( MultState . MaxOccupied , LChar ) 
      END ApMapElem 

  ; PROCEDURE ApRecurse 
      ( VAR State : TempStateTyp 
      ; String : TEXT 
      ; CharSs : INTEGER 
      ; Value : LexTable . ValueTyp 
      ; ReverseMap : BOOLEAN 
      ) 

    = VAR LNewSingletonState : SingletonTempStateTyp 
    ; VAR LNewMultState : MultTempStateTyp 

    ; BEGIN 
        TYPECASE State <* NOWARN *> 
        OF NULL 
        => IF CharSs = Text . Length ( String )  
           THEN 
             LNewSingletonState := NEW ( SingletonTempStateTyp ) 
           ; LNewSingletonState . Value := Value 
           ; LNewSingletonState . String := String 
           ; LNewSingletonState . ReverseMap := ReverseMap  
           ; State := LNewSingletonState 
           ELSE
             LNewMultState := EmptyMultState ( ) 
           ; ApMapElem 
               ( LNewMultState , String , CharSs , Value ,  ReverseMap ) 
           ; State := LNewMultState 
           END (* IF *) 

        | MultTempStateTyp ( TMultState )  
        => ApMapElem ( TMultState , String , CharSs , Value , ReverseMap )  

        | SingletonTempStateTyp ( TSingle ) 
        => IF Text . Equal ( String , TSingle . String )  
           THEN (* Reinserting this string. *)  
             IF TSingle . Value # Value 
             THEN (* This string has > 1 value. *) 
               TSingle . Value := Value
             END (* IF *) 
           ELSE 
             LNewMultState := EmptyMultState ( ) 
           ; ApMapElem 
               ( LNewMultState 
               , TSingle . String 
               , CharSs 
               , TSingle . Value 
               , TSingle . ReverseMap  
               ) 
           ; ApMapElem 
               ( LNewMultState , String , CharSs , Value , ReverseMap ) 
           ; State := LNewMultState 
           END (* IF *) 
        END (* TYPECASE *) 
      END ApRecurse 

  ; BEGIN (* AddPair *)  
      ApRecurse 
        ( (* VAR *) GMachine 
        , AddString 
        , CharSs := 0 
        , Value := Value 
        , ReverseMap := ReverseMap 
        ) 
    ; INC ( GRuleCt ) 
    ; GMinValue := MIN ( GMinValue , Value ) 
    ; GMaxValue := MAX ( GMaxValue , Value ) 
    END AddPair 

; PROCEDURE BuildPass1 
    ( VAR States : LexTableRep . StatesTyp 
    ; VAR TotalTransitionCt : LexTableRep . TransitionTyp 
    )  
  (* Renumber states in preorder.
     Fill in fields of States. 
     Count Total Transitions
  *) 

  = VAR P1NextStateNo : LexTableRep . StateNoTyp := 0

  ; PROCEDURE P1Recurse ( TempState : TempStateTyp ) 

    = BEGIN 
        TYPECASE TempState <* NOWARN *> 
        OF NULL => 

        | MultTempStateTyp ( TMult ) 
        => TMult . StateNo := NextStateNo ( P1NextStateNo ) 
        ; WITH WState = States [ TMult . StateNo ] 
          DO
            WState . Min := TMult . MinOccupied  
          ; WState . Max:= TMult . MaxOccupied 
          ; WState . SpaceBias 
              := TotalTransitionCt - ORD ( TMult . MinOccupied )    
          ; INC ( TotalTransitionCt 
                , ORD ( TMult . MaxOccupied ) 
                  - ORD ( TMult . MinOccupied ) + 1 
                ) 
          ; FOR RI := TMult . MinOccupied TO TMult . MaxOccupied   
            DO P1Recurse ( TMult . MapRef ^ [ RI ] ) 
            END (* FOR *) 
          END (* WITH *)  

        | SingletonTempStateTyp =>
        END (* TYPECASE *) 
      END P1Recurse 

  ; BEGIN (* BuildPass1 *) 
      TotalTransitionCt := 0 
    ; P1NextStateNo := 0 
    ; P1Recurse ( GMachine ) 
    END BuildPass1 

; PROCEDURE BuildPass2 ( Table : LexTable . T )  
  (* Fill in Space and Names. *) 

  = PROCEDURE P2Recurse 
      ( TempState : TempStateTyp 
      ; VAR ParentTransition : LexTableRep . TransitionTyp 
      ) 

    = BEGIN 
        TYPECASE TempState <* NOWARN *>
        OF NULL 
        => ParentTransition := LexTableRep . NoTransition 

        | MultTempStateTyp ( TMult ) 
        => ParentTransition := TMult . StateNo 
        ; WITH WState = Table . StatesRef ^ [ TMult . StateNo ] 
          DO 
            FOR RI := TMult . MinOccupied TO TMult . MaxOccupied   
            DO P2Recurse 
                 ( TMult . MapRef ^ [ RI ] 
                 , (* VAR *) Table . SpaceRef  
                               ^ [ WState . SpaceBias + ORD ( RI ) ] 
                 ) 
            END (* FOR *) 
          END (* WITH *)  

        | SingletonTempStateTyp ( TSingle ) 
        => ParentTransition := TSingle . Value + LexTableRep . FirstRealValue 
        ; IF TSingle . ReverseMap 
          THEN 
            WITH WRef 
                 = Table . NamesRef ^  [ TSingle . Value - Table . MinValue ] 
            DO IF WRef = NIL 
               THEN WRef := TSingle . String 
               ELSE (* This value has > 1 reverse map string. *)  
               END (* IF *) 
            END (* WITH *) 
          END (* IF *) 
        END (* TYPECASE *) 
      END P2Recurse 

  ; VAR LDontCareTransition : LexTableRep . TransitionTyp 

  ; BEGIN (* BuildPass2 *) 
      P2Recurse ( GMachine , (* VAR *) LDontCareTransition ) 
    END BuildPass2 

(* VISIBLE *) 
; PROCEDURE Build ( ) : LexTable . T 

  = VAR LResult : LexTable . T
  ; VAR LTotalTransitionCt : LexTableRep . TransitionTyp 

  ; BEGIN 
      LResult := NEW ( LexTable . T ) 
    ; LResult . StatesRef := NEW ( LexTableRep . StatesRefTyp , GNextStateNo ) 
    ; IF GNextStateNo > 0 
      THEN 
        BuildPass1 ( LResult . StatesRef ^ , (* VAR *) LTotalTransitionCt ) 
      ; LResult . SpaceRef 
         := NEW ( LexTableRep . SpaceRefTyp , LTotalTransitionCt ) 
      ; LResult . MinValue := GMinValue 
      ; LResult . MaxValue := GMaxValue 
      ; LResult . NamesRef 
          := NEW ( LexTableRep . NamesRefTyp 
                 , LResult . MaxValue - LResult . MinValue + 1 
                 ) 
      ; BuildPass2 ( LResult ) 
      END (* IF*) 
    ; RETURN LResult 
    END Build 

; BEGIN 
  END BuildLexMachine 
. 
