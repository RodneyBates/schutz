MODULE test14_135

; IMPORT Stdio
; IMPORT Wr

; IMPORT Format

; PROCEDURE OneCase ( IntVal : CARDINAL ; ExpectedTextVal : TEXT )

  = BEGIN
      LResult := Format . IntToText ( IntVal )
    ; INC ( TestCt )
    ; Wr . PutText
        ( Stdio . stdout
        , Fmt . Pad ( Fmt . Int ( IntVal ) , 11 ) & "\"" & LResult & "\""
          & Wr . EOL
        )

    ; IF Text . Equal ( LResult , ExpectedVal )
      THEN
      ELSE
      ; INC ( FailureCt )
      END (* IF *)
    END OneCase

; BEGIN
  END test14_135
.
