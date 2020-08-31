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

(*LFY1x*) MODULE texst12_17 (*TR*)

(*LFY2*) (*LRN1*) (*LRN2*) ; VAR V
    : INTEGER (*TR*)                                     (*TF*)
; (*LRN3*) VAR (*LRN4*) W (*LRN5*) : (*LRN6*) BOOLEAN

; BEGIN
      (*LRY*)(*LRN7*) X := 0
  END test12_17
.
