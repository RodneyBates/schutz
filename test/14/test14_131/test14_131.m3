MODULE test14_131 

(* VISIBLE: *) 
; PROCEDURE FetchChars 
    ( VAR Chars : ARRAY OF CHAR ; String : T ) 
    RAISES { Assertions . AssertionFailure } 
  (* Truncate or leave suffix of Chars unchanged if lengths are # *) 

  = VAR LLength := MIN ( String . FromSs - String . ToSs , NUMBER ( Chars ) ) 

  ; BEGIN (* FetchChars *) 
      IF String . Space # NIL 
      THEN 
        Assert ( String . FromSs = 0 , AFT . A_FetchChars_BadFromSs ) 
      ; SUBARRAY ( Chars , 0 , LLength ) 
          := SUBARRAY ( String . Space ^ , 0 , LLength ) 
      ELSIF String . Text # NIL 
      THEN 
        SUBARRAY ( Chars , 0 , LLength ) 
          := SUBARRAY ( String . Text ^ , String . FromSs , LLength ) 
   (* ELSE do nothing, as this String is empty. *) 
      END (* IF *) 
    END FetchChars 

; BEGIN 
  END test14_131
. 


