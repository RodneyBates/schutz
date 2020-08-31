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

(*LFY2x*) (*LRN1x*) (*LRN2x*) ; VAR V : INTEGER (*TR*)      (*TF*)
; (*LRN3*) VAR (*LxRN4*) W (*LRN5*)
    : (*LRN6*) BOOLEAN x x x
    x
; BEGIN
      (*LRYx*)(*LRN7x*) X := 0
  END test12_17
.
