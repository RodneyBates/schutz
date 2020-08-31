(* Testing various forms of comments. *)
(* Codes are : 
   L Leading 
   T Trailing 
   F Fixed 
   R Relative 
   Y NlBefore    Relevant only for L
   N NotNlBefore Relevant only for L 
*)
(* Code to handle an LFN is in place, but not to generate one. *)

(*LFY1x*) MODULE test12_17 (*TRx*)

(*LFY2x*) (*LRN1x*) (*LRN2x*) ; VAR V
    : INTEGER (*TRx*)                                     (*TF*)
; (*LRN3x*) VAR (*LRN4x*) W (*LRN5x*) : (*LRN6x*) BOOLEAN

; BEGIN
      (*LRYx*)(*LRN7x*) X := 0
  END test12_17
.
