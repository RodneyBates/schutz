MODULE Queue

; TYPE BufferTyp = ARRAY OF INTEGER
; TYPE BufferRefTyp = REF BufferTyp

; REVEAL T
    = Public BRANDED OBJECT
        NextIn : INTEGER
      ; NextOut : INTEGER
      ; Count : INTEGER
      ; Buffer : BufferRefTyp
      OVERRIDES
        init := Init ; put := Put ; get := Get ; count := Count
      END

; CONST MinSize = 5

; PROCEDURE Init ( Queue : T ; SizeHint : INTEGER ) : T

  = BEGIN
      Queue . NextIn := 0
    ; Queue . NextOut := 0
    ; Queue . Count := 0
    ; SizeHint := MAX ( SizeHint , MinSize )
    ; Queue . Buffer := NEW ( BufferRefTyp , SizeHint )
    ; RETURN Queue
    END Init

; PROCEDURE Expand ( Queue : T ; NewSize : INTEGER )

  = VAR LOldSize : INTEGER
  ; LOldBufferRef : BufferRefTyp
  ; LNewBufferRef : BufferRefTyp

  ; BEGIN
      LOldBufferRef := Queue . BufferRef
    ; LOldSize := NUMBER ( LOldBufferRef ^ )
    ; NewSize := MAX ( NewSize , LOldSize + MinSize )
    ; LNewBufferRef := NEW ( BufferRefTyp , NewSize )
    ; Queue . BufferRef := LNewBufferRef
    END Expand

; PROCEDURE Put ( Queue : T ; Elem : ElemTyp )

  = VAR LSize : INTEGER

  ; BEGIN
      LSize := NUMBER ( Queue . Buffer ^ )
    ; IF Queue . Count = LSize
      THEN
        Expand ( Queue , LSize * 2 )
      END (* IF *)
    ; Queue . BufferRef ^ [ Queue . NextIn ] := Elem
    ; Queue . NextIn := ( Queue . NextIn + 1 ) MOD LSize
    ; INC ( Queue . Count )
    END Put

; PROCEDURE Get ( Queue : T ) : ElemTyp

  = VAR LResult : ElemTyp

  ; BEGIN
      IF Queue . Count <= 0
      THEN
        LResult := EmptyVal
      ELSE
        LResult := Queue . BufferRef ^ [ Queue . NextOut ]
      ; Queue . NextOut
          := ( Queue . NextOut - 1 ) MOD NUMBER ( Queue . BufferRef ^ )
      ; DEC ( Queue . Count )
      END (* IF *)
    ; RETURN LResult
    END Get

; PROCEDURE Count ( Queue : T ) : INTEGER

  = BEGIN
      RETURN Queueu . Count
    END Count

; BEGIN
  END Queue
.
