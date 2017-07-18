
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE SetPred 

(* An algebra that captures a predicate applied with quantifiers to some set. *)

; CONST ImageTable 
    = ARRAY T OF TEXT 
        { "SpUnknown" , "SpNever" , "SpSometimes" , "SpAlways" } 

; TYPE AT = ARRAY T OF T 

; CONST SpUnknown = T . SpUnknown 
; CONST SpNever = T . SpNever
; CONST SpSometimes = T . SpSometimes
; CONST SpAlways = T . SpAlways

; CONST AndTable 
    = ARRAY T OF AT
                      (* SpUnknown     SpNever       SpSometimes   SpAlways   *)

(* SpUnknown *)   { AT { SpUnknown   , SpNever     , SpUnknown   , SpUnknown   }
(* SpNever *)     , AT { SpNever     , SpNever     , SpNever     , SpNever     }
(* SpSometimes *) , AT { SpUnknown   , SpNever     , SpSometimes , SpSometimes }
(* SpAlways *)    , AT { SpUnknown   , SpNever     , SpSometimes , SpAlways    }
                  }         

; CONST OrTable 
    = ARRAY T OF AT
                      (* SpUnknown     SpNever       SpSometimes   SpAlways   *)

(* SpUnknown *)   { AT { SpUnknown   , SpUnknown   , SpUnknown   , SpAlways    }
(* SpNever *)     , AT { SpUnknown   , SpNever     , SpSometimes , SpAlways    }
(* SpSometimes *) , AT { SpUnknown   , SpSometimes , SpSometimes , SpAlways    }
(* SpAlways *)    , AT { SpAlways    , SpAlways    , SpAlways    , SpAlways    }
                  }         


(* VISIBLE: *) 
; PROCEDURE Image ( Value : T ) : TEXT  

  = BEGIN 
      RETURN ImageTable [ Value ] 
    END Image  
  
(* VISIBLE: *) 
; PROCEDURE And ( Left , Right : T ) : T 

  = BEGIN 
      RETURN AndTable [ Left , Right ] 
    END And

(* VISIBLE: *) 
; PROCEDURE Or ( Left , Right : T ) : T 

  = BEGIN 
      RETURN OrTable [ Left , Right ] 
    END Or

; BEGIN 
  END SetPred 
. 

