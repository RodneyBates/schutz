MODULE TokPartition 

; FROM LbeStd 
  IMPORT TokTyp , Tok__Null , TokClassTyp , TokClassSetTyp , TokClassPred  

; TYPE PartitionTyp = ARRAY TokClassTyp OF TokTyp 

TODO: Move this to LbeStd. *) 
(* VISIBLE: *) 
; PROCEDURE TokClassPred ( Class : TokClassTyp ) : TokClassTyp 
  (* Predecessor function on TokClassTyp.  Will crash on FIRST(TokClassTyp) *) 

  = BEGIN 
      RETURN VAL ( ORD ( Class ) + 1 , TokClassTyp ) 
    END TokClassPred 

(* VISIBLE: *) 
; PROCEDURE MakeEmpty ( VAR Part : PartitionTyp )
  (* Make all partition classes empty except the last one.  Make it
     contain the entire range of TokTyp. 
  *) 

  = BEGIN 
      FOR RI := FIRST ( TokClassTyp ) TO LAST ( TokClassTyp ) - 1 
      DO Part [ RI ] := FIRST ( TokClassTyp ) 
      END (* FOR *) 
    ; Part [ LAST ( TokClassTyp ) ] := LAST ( TokTyp ) 
    END MakeEmpty 

; EXCEPTION InsufficientSpace 

(* VISIBLE: *) 
; PROCEDURE ExpandClass 
    ( VAR Part : PartitionTyp , Class : TokClassTyp ; N : INTEGER ) 
  RAISES InsufficientSpace 
  (* Expand class Class by N members in partition Part, adjusting
     the bounds of higher-numbered classes as needed.  N can be negative,
     shrinking the class, but it will not shrink to negative size.  
     Raise InsufficientSpace if the expansion would overfill TokTyp.  
  *) 

  = BEGIN
      IF LAST ( TokTyp ) - Part [ LAST ( TokClassTyp ) - 1 ] < N 
      THEN RAISE InsufficientSpace 
      ELSE 
        N := MAX ( N , - ClassCard ( Part , Class ) ) 
      ; FOR RI := Class TO LAST ( TokClassTyp ) - 1  
        DO INC ( Part [ RI ] , N ) 
        END (* FOR *) 
      END (* IF *) 
    END ExpandClass 

(* VISIBLE: *) 
; PROCEDURE From 
    ( READONLY Part : PartitionTyp ; Class : TokClassTyp ) : TokTyp 
  (* Minimum value in class Class. *) 

  = BEGIN 
      IF Class = FIRST ( TokClassTyp ) 
      THEN RETURN FIRST ( TokClassTyp ) 
      ELSE 
        RETURN Part [ TokClassPred ( Class ) ] 
      END (* IF *) 
    END From  

(* VISIBLE: *) 
; PROCEDURE Thru 
    ( READONLY Part : PartitionTyp ; Class : TokClassTyp ) : TokTyp 
  (* Maximum value in class Class. *) 

  = BEGIN 
      RETURN Part [ Class ] - 1 
    END Thru 

(* VISIBLE: *) 
; PROCEDURE To ( READONLY Part : PartitionTyp ; Class : TokClassTyp ) : TokTyp 
  (* Maximum value in class Class, plus one. *)  

  = BEGIN 
      RETURN Part [ Class ] 
    END To

(* VISIBLE: *) 
; PROCEDURE PartitionCard ( READONLY Part : PartitionTyp ) : CARDINAL 
  (* Number of tokens in all classs of partition Part. *) 

  = BEGIN 
      RETURN Part [ LAST ( TokClassTyp ) - 1 ] 
    END PartitioniCard 

(* VISIBLE: *) 
; PROCEDURE ClassCard 
    ( READONLY Part : PartitionTyp ; Class : TokClassTyp ) : CARDINAL 
  (* Number of tokens in class Class of partition Part. *) 

  = VAR LLow : TokTyp  

  ; BEGIN 
      IF Class = FIRST ( TokClassTyp ) 
      THEN LLow := FIRST ( TokClassTyp ) 
      ELSE LLow := Part [ TokClassPred ( Class ) ]  
      END (* IF *) 
    ; RETURN Part [ Class ] - LLow  
    END PartitioniCard 

(* VISIBLE: *) 
; PROCEDURE ContainingClass 
    ( READONLY Part : PartitionTyp ; Tok : TokTypClass ) : TokClassTyp 
  (* The class of partition Part that contains token Tok. *) 

  = VAR LLo , LHi , LProbe , LPred : TokClassTyp 

  ; BEGIN 
      LLo := FIRST ( TokClassTyp ) 
    ; LHi := LAST ( TokClassTyp ) 
    ; LOOP 
        IF LLo > LHi 
        THEN CantHappen ( ) 
        ELSE 
          LProbe := VAL ( ORD ( LLo ) + ORD ( LHi ) DIV 2 , TokClassTyp ) 
        ; IF Tok >= Part [ TokClassPred ( LProbe ) ] 
          THEN (* Tok is in a higher class. *) 
            LLo := LProbe 
          ELSIF LProbe = FIRST ( TokClassTyp )
          THEN (* This is the lowest class, Tok has to be in it. *) 
            RETURN LProbe 
          ELSE
            LPred := TokClassPred ( LProbe ) 
          ; IF Tok < Part [ LPred ] 
            THEN (* Tok is in a lower class. *) 
              LHi := LProbe 
            ELSE (* Tok is in this class. *)  
              RETURN LProbe 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    END ContainingClass 

(* VISIBLE: *) 
; PROCEDURE TokIsInClass 
    ( READONLY Part : PartitionTyp ; Tok : TokTypClass ; Class : TokClassTyp )
  : BOOLEAN 
  (* Token Tok is in class Class of partition Part *) 

  = BEGIN 
      RETURN Class = ContainingClass ( Part , Tok ) 
    END TokIsInClass 

(* VISIBLE: *) 
; PROCEDURE TokIsInClassRange 
    ( READONLY Part : PartitionTyp 
    ; Tok : TokTypClass 
    ; LoClass , HiClass : TokClassTyp 
    )
  : BOOLEAN 
  (* Token Tok is in a class in the range [LoClass,HiClass] 
     of partition Part 
  *) 

  = VAR LClass : TokClassTyp 

  ; BEGIN 
      LClass = ContainingClass ( Part , Tok ) 
    ; RETURN LoClass <= LClass AND LClass <= HiClass 
    END TokIsInClassRange 

(* VISIBLE: *) 
; PROCEDURE TokIsInClassSet 
    ( READONLY Part : PartitionTyp 
    ; Tok : TokTypClass 
    ; ClassSet : TokClassSetTyp 
    )
  : BOOLEAN 
  (* Token Tok is in a class in the set ClassSet of partition Part *) 

  = VAR LClass : TokClassTyp 

  ; BEGIN 
      LClass = ContainingClass ( Part , Tok ) 
    ; RETURN LClass IN ClassSet 
    END TokIsInClassSet 

; BEGIN (* TokPartition *) 
  END 
. 


