
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE SyncQueue 

; IMPORT Thread 

; TYPE T <: ROOT  

; CONST Brand = "SyncQueue.T"  

; TYPE ElemTyp = REFANY 

; EXCEPTION NotInitialized 

; PROCEDURE Init ( Queue : T ; MaxContents : INTEGER ) : T 
  RAISES { Thread . Alerted }  

; PROCEDURE Put ( Queue : T ; Value : ElemTyp ) 
  RAISES { Thread . Alerted }  
  (* If Queue is not initialized/working, a NOOP *) 

; PROCEDURE Get ( Queue : T ) : ElemTyp 
  RAISES 
    { NotInitialized 
      (* Also happens if Queue is closed and empty. *) 
    , Thread . Alerted 
    }  

; PROCEDURE Close ( Queue : T ; WaitForEmpty : BOOLEAN ) 
  RAISES { Thread . Alerted }  
  (* NOOP if already closed. *) 

; END SyncQueue 
. 
