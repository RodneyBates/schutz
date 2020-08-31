MODULE test14_142 EXPORTS Main

; PROCEDURE R ( F : INTEGER ) : INTEGER

  = VAR L : INTEGER
  ; VAR Res : INTEGER


  ; PROCEDURE Inner ( )
    = VAR IV : INTEGER := 17

    ; BEGIN
        IF F = MaxDepth
        THEN
          Res := MaxDepth + 2000
        ELSE
          Res := R ( F + 1 ) - 1
        END (* IF *)
      END Inner
  ; BEGIN
      L := F + 1000
    ; Inner ( )
    ; EVAL Res
    ; RETURN Res
    END R

; BEGIN
    R ( 0 )
  END test14_142
.
