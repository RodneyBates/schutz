MODULE test14_142 EXPORTS Main

; PROCEDURE R ( F : INTEGER ) : INTEGER

  = VAR L : INTEGER
  ; VAR Res : INTEGER

  ; BEGIN
      L := F + 1000
    ; IF F = MaxDepth
      THEN
        Res := MaxDepth + 2000
      ELSE
        Res := R ( F + 1 ) - 1
      END (* IF *)
    ; EVAL Res
    ; RETURN Res
    END R

; BEGIN
    R ( 0 )
  END test14_142 
.
