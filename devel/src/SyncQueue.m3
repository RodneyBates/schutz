
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE SyncQueue 

; IMPORT Thread 

; TYPE SsTyp = INTEGER 

; TYPE BufferTyp = ARRAY (* SsTyp *) OF ElemTyp 
; TYPE BufferRefTyp = REF BufferTyp 

; REVEAL T = MUTEX BRANDED Brand 
    OBJECT 
      NextIn : SsTyp := 0 
    ; NextOut : SsTyp := 0 
    ; Count : SsTyp := 0 
    ; MaxCount : SsTyp := 0 
    ; BufferRef : BufferRefTyp := NIL
    ; WaitingMessages : Thread . Condition  
    ; WaitingSpace : Thread . Condition  
    END   

; PROCEDURE Cleanout ( Queue : T ; WaitForEmpty : BOOLEAN ) 
  RAISES { Thread . Alerted }  
  (* PRE Queue is non-NIL and locked. *) 

  = BEGIN 
      Queue . MaxCount := 0 
    ; IF Queue . WaitingSpace # NIL 
      THEN 
        Thread . Broadcast ( Queue . WaitingSpace ) 
      END (* IF *) 
    ; IF Queue . WaitingMessages # NIL 
      THEN 
        Thread . Broadcast ( Queue . WaitingMessages ) 
      END (* IF *) 
    ; IF WaitForEmpty 
      THEN 
        WHILE Queue . Count > 0 
        DO Thread . AlertWait ( Queue , Queue . WaitingSpace ) 
        END (* WHILE *) 
      END (* IF *) 
    END Cleanout  

(* VISIBLE: *) 
; PROCEDURE Init ( Queue : T ; MaxContents : INTEGER ) : T 
  RAISES { Thread . Alerted }  

  = BEGIN 
      IF Queue # NIL 
      THEN 
        LOCK Queue 
        DO 
          Cleanout ( Queue , WaitForEmpty := FALSE ) 
        ; Queue . BufferRef := NEW ( BufferRefTyp , MaxContents ) 
        ; Queue . MaxCount := MaxContents 
        ; Queue . NextIn := 0 
        ; Queue . NextOut := 0 
        ; Queue . Count := 0 
        ; Queue . WaitingMessages := NEW ( Thread . Condition ) 
        ; Queue . WaitingSpace := NEW ( Thread . Condition ) 
        END (* LOCK *) 
      END (* IF *) 
    ; RETURN Queue 
    END Init 

(* VISIBLE: *) 
; PROCEDURE Put ( Queue : T ; Value : ElemTyp ) 
  RAISES { Thread . Alerted }  
  (* If Queue is not initialized/working, a NOOP *) 

  = BEGIN 
      IF Queue # NIL 
      THEN 
        LOCK Queue 
        DO 
          WHILE Queue . MaxCount > 0 
                AND Queue . Count >= Queue . MaxCount 
          DO Thread . AlertWait ( Queue , Queue . WaitingSpace ) 
          END (* WHILE *) 
        ; IF Queue . MaxCount > 0 
          THEN 
            Queue . BufferRef ^ [ Queue . NextIn ] := Value 
          ; Queue . NextIn 
              := ( Queue . NextIn + 1 ) MOD NUMBER ( Queue . BufferRef ^ )     
          ; INC ( Queue . Count ) 
          ; Thread . Signal ( Queue . WaitingMessages ) 
          END (* IF *) 
        END (* LOCK *) 
      END (* IF *) 
    END Put

(* VISIBLE: *) 
; PROCEDURE Get ( Queue : T ) : ElemTyp 
  RAISES 
    { NotInitialized 
      (* Also happens if Queue is closed and empty. *) 
    , Thread . Alerted 
    }  

  = VAR LResult : ElemTyp 

  ; BEGIN 
      IF Queue = NIL 
      THEN RAISE NotInitialized 
      ELSE 
        LOCK Queue 
        DO 
          WHILE Queue . MaxCount > 0 AND Queue . Count <= 0 
          DO Thread . AlertWait ( Queue , Queue . WaitingMessages ) 
          END (* WHILE *) 
        ; IF Queue . Count <= 0 
          THEN RAISE NotInitialized 
          ELSE 
            LResult := Queue . BufferRef ^ [ Queue . NextOut ] 
          ; Queue . NextOut 
              := ( Queue . NextOut + 1 ) MOD NUMBER ( Queue . BufferRef ^ )    
          ; DEC ( Queue . Count ) 
          ; Thread . Signal ( Queue . WaitingSpace ) 
          END (* IF *) 
        END (* LOCK *) 
      ; RETURN LResult 
      END (* IF *) 
    END Get  

(* VISIBLE: *) 
; PROCEDURE Close ( Queue : T ; WaitForEmpty : BOOLEAN ) 
  RAISES { Thread . Alerted }  
  (* NOOP if already closed. *) 

  = BEGIN 
      IF Queue # NIL 
      THEN 
        LOCK Queue 
        DO Cleanout ( Queue , WaitForEmpty ) 
        END (* LOCK *) 
      END (* IF *) 
    END Close 

; BEGIN (* SyncQueue *) 
  END SyncQueue 
. 
