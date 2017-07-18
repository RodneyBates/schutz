
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Recording and playback of edit events. *) 

MODULE Record

; IMPORT Wr 

; REVEAL Private = MUTEX  

; REVEAL T = Public BRANDED "Record.T" OBJECT 
      RecordEnabled : BOOLEAN := FALSE 
    ; WrT : Wr . T 
    END (* T *) 

; PROCEDURE SetWriter 
    ( Stream : T ; WrT : Wr . T ; Enabled : BOOLEAN := FALSE ) 
  (* WrT must already be open. *)  

  = BEGIN 
      LOCK Stream  
      DO 
        Stream . WrT := WrT 
      ; Stream . RecordEnabled := Enabled 
      END (* LOCK *) 
    END SetWriter  

; PROCEDURE Flush ( Stream : T ) 

  = BEGIN 
      LOCK Stream  
      DO 
        Wr . Flush ( Stream . WrT )  
      END (* LOCK *) 
    END Flush   

; PROCEDURE Close ( Stream : T ) 

  = BEGIN 
      LOCK Stream  
      DO 
        Wr . Close ( Stream . WrT )  
      ; Stream . RecordEnabled := FALSE  
      END (* LOCK *) 
    END Close    

; PROCEDURE Enable ( Stream : T ) 

  = BEGIN 
      LOCK Stream  
      DO 
        Stream . RecordEnabled := TRUE 
      END (* LOCK *) 
    END Enable 

; PROCEDURE Disable ( Stream : T ) 

  = BEGIN 
      LOCK Stream  
      DO 
        Stream . RecordEnabled := FALSE  
      END (* LOCK *) 
    END Disable 

; PROCEDURE Record ( Stream : T : Event : TEXT )  

  = BEGIN 
      LOCK Stream  
      DO
        Wr . PutText ( Stream . WrT , Event )  
      ; Wr . PutText ( Stream . WrT , Wr . EOL )  
      END (* LOCK *) 
    END Record 

; BEGIN (* Record *) 
  END Record 
. 

