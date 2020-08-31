MODULE Test15_181

; PROCEDURE P ( ) 

  = BEGIN 
      IF X 
      THEN
        A ( ) 
      ; B ( ) 
      ; C ( ) 
      ; 
      ELSE
        A ( ) ;  
      END (* IF *)
    ; WHILE X 
      DO 
        A ( ) 
      END (* WHILE *)  
    ; WITH X = Y  
      DO 
        A ( ) ; 
      END (* WITH *) 
    ; CASE C 
      OF 0 => 
        A ( ) 
      ; B( ) 
      | 1 => 
        A ( ) 
      ; B ( ) 
      ; 
      | 2 => 
        A ( ) 
      ; B ( ) 
      ; C ( ) 
      | 3 => 
        A ( ) 
      ; B ( ) 
      ; C ( ) 
      ; 
      | 4 => 
        A ( ) 
      ; B ( ) 
      ; C ( ) 
      ; D ( ) 
      ELSE 
        A ( ) 
      ; B ( ) 
      ; C ( ) 
      ; D ( ) 
     ; 
      END (* CASE *)         
    END P 

; BEGIN 
  END Test15_181
. 
