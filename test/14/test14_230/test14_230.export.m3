MODULE Queue

; PROCEDURE Expand ( )

  = BEGIN
      LOldBufferRef := Queue . BufferRef
    ; Queue . BufferRef := LNewBufferRef
    ; IF Queue . Count = 0
      THEN
      ELSIF Queue . NextOut < Queue . NextIn
      THEN
      ELSE
      END (* IF *)
    END Expand

; BEGIN
  END Queue
.
