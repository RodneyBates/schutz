
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE UncertainBool

(* Boolean logic extended to a three-valued domain by an "unknown". *) 

; CONST TU = T . Unknown 
; CONST TF = T . False 
; CONST TT = T . True  

(* VISIBLE: *) 
; PROCEDURE FromBool ( Value : BOOLEAN ) : T  

  = BEGIN
      IF Value 
      THEN RETURN T . True 
      ELSE RETURN T . False
      END (* IF *)  
    END FromBool

(* VISIBLE: *) 
; PROCEDURE ToBool ( Value : T ) : BOOLEAN RAISES { UnknownBool }

  = BEGIN 
      CASE Value 
      OF T . Unknown => RAISE UnknownBool 
      | T . False => RETURN FALSE 
      | T . True => RETURN TRUE 
      END (* CASE *) 
    END ToBool

; TYPE RowTyp = ARRAY [ 0 .. 3 ] OF T 
  (* We really only need three elements here.  But hopefully, this will get 
     compiled with shifts to do multiplication by 4 and be faster. *)  
; TYPE TblTyp = ARRAY T OF RowTyp 
  (* ^No need for 4-valued subscripts here. *) 

; CONST AndTable 
               (*            Right:   TU   TF   TT      Can't happen *) 
               (* Left  +------------------------------------------- *) 
    = TblTyp { (* TU    | *) RowTyp { TU , TF , TU , (* Unused: *) TU } 
             , (* TF    | *) RowTyp { TF , TF , TF , (* Unused: *) TU } 
             , (* TT    | *) RowTyp { TU , TF , TT , (* Unused: *) TU } 
             } 

(* VISIBLE: *) 
; PROCEDURE And ( Left , Right : T ) : T 

  = BEGIN 
      RETURN AndTable [ Left , ORD ( Right ) ] 
    END And 

; CONST OrTable 
               (*            Right:   TU   TF   TT      Can't happen *) 
               (* Left  +------------------------------------------- *) 
    = TblTyp { (* TU    | *) RowTyp { TU , TU , TT , (* Unused: *) TU } 
             , (* TF    | *) RowTyp { TU , TF , TT , (* Unused: *) TU } 
             , (* TT    | *) RowTyp { TT , TT , TT , (* Unused: *) TU } 
             } 

(* VISIBLE: *) 
; PROCEDURE Or ( Left , Right : T ) : T 

  = BEGIN 
      RETURN OrTable [ Left , ORD ( Right ) ] 
    END Or 

; CONST NotTable 
    = ARRAY T OF T { TU , TT , TF } 

(* VISIBLE: *) 
; PROCEDURE Not ( Value : T ) : T 

  = BEGIN 
      RETURN NotTable [ Value ]  
    END Not

; CONST NandTable 
               (*            Right:   TU   TF   TT      Can't happen *) 
               (* Left  +------------------------------------------- *) 
    = TblTyp { (* TU    | *) RowTyp { TU , TT , TU , (* Unused: *) TU } 
             , (* TF    | *) RowTyp { TT , TT , TT , (* Unused: *) TU } 
             , (* TT    | *) RowTyp { TU , TT , TF , (* Unused: *) TU } 
             } 

(* VISIBLE: *) 
; PROCEDURE Nand ( Left , Right : T ) : T 

  = BEGIN 
      RETURN NandTable [ Left , ORD ( Right ) ] 
    END Nand

; CONST NorTable 
               (*            Right:   TU   TF   TT      Can't happen *) 
               (* Left  +------------------------------------------- *) 
    = TblTyp { (* TU    | *) RowTyp { TU , TU , TF , (* Unused: *) TU } 
             , (* TF    | *) RowTyp { TU , TT , TF , (* Unused: *) TU } 
             , (* TT    | *) RowTyp { TF , TF , TF , (* Unused: *) TU } 
             } 

(* VISIBLE: *) 
; PROCEDURE Nor ( Left , Right : T ) : T 

  = BEGIN 
      RETURN NorTable [ Left , ORD ( Right ) ] 
    END Nor

; CONST EqualTable 
               (*            Right:   TU   TF   TT      Can't happen *) 
               (* Left  +------------------------------------------- *) 
    = TblTyp { (* TU    | *) RowTyp { TU , TU , TU , (* Unused: *) TU } 
             , (* TF    | *) RowTyp { TU , TT , TF , (* Unused: *) TU } 
             , (* TT    | *) RowTyp { TU , TF , TT , (* Unused: *) TU } 
             } 

(* VISIBLE: *) 
; PROCEDURE Equal ( Left , Right : T ) : T

  = BEGIN 
      RETURN EqualTable [ Left , ORD ( Right ) ] 
    END Equal

; CONST UnequalTable 
               (*            Right:   TU   TF   TT      Can't happen *) 
               (* Left  +------------------------------------------- *) 
    = TblTyp { (* TU    | *) RowTyp { TU , TU , TU , (* Unused: *) TU } 
             , (* TF    | *) RowTyp { TU , TF , TT , (* Unused: *) TU } 
             , (* TT    | *) RowTyp { TU , TT , TF , (* Unused: *) TU } 
             } 

(* VISIBLE: *) 
; PROCEDURE Unequal ( Left , Right : T ) : T

  = BEGIN 
      RETURN UnequalTable [ Left , ORD ( Right ) ] 
    END Unequal 

; CONST LTTable 
               (*            Right:   TU   TF   TT      Can't happen *) 
               (* Left  +------------------------------------------- *) 
    = TblTyp { (* TU    | *) RowTyp { TU , TF , TU , (* Unused: *) TU } 
             , (* TF    | *) RowTyp { TU , TF , TT , (* Unused: *) TU } 
             , (* TT    | *) RowTyp { TF , TF , TF , (* Unused: *) TU } 
             } 

(* VISIBLE: *) 
; PROCEDURE LT ( Left , Right : T ) : T

  = BEGIN 
      RETURN LTTable [ Left , ORD ( Right ) ] 
    END LT

(* VISIBLE: *) 
; PROCEDURE GT ( Left , Right : T ) : T

  = BEGIN (* Just transpose LTTable. *) 
      RETURN LTTable [ Right , ORD( Left ) ] 
    END GT

; CONST LETable 
               (*            Right:   TU   TF   TT      Can't happen *) 
               (* Left  +------------------------------------------- *) 
    = TblTyp { (* TU    | *) RowTyp { TU , TU , TT , (* Unused: *) TU } 
             , (* TF    | *) RowTyp { TT , TT , TT , (* Unused: *) TU } 
             , (* TT    | *) RowTyp { TU , TF , TT , (* Unused: *) TU } 
             } 

(* VISIBLE: *) 
; PROCEDURE LE ( Left , Right : T ) : T

  = BEGIN 
      RETURN LETable [ Left , ORD ( Right ) ] 
    END LE

(* VISIBLE: *) 
; PROCEDURE GE ( Left , Right : T ) : T

  = BEGIN (* Just transpose LETable. *)
      RETURN LETable [ Right , ORD ( Left ) ] 
    END GE

(* VISIBLE: *) 
; PROCEDURE Image ( Value : T ) : TEXT 

  = BEGIN 
      CASE Value 
      OF T . Unknown => RETURN "Unknown" 
      | T . False => RETURN "False" 
      | T . True => RETURN "True" 
      END (* CASE *) 
    END Image 

; BEGIN 
  END UncertainBool 
.
 
