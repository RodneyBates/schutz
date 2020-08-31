(* Name: Mounir Cheaib
   Course: CS810, FALL semester 2004
   Assignment: 10 *)


   
MODULE     set_m   EXPORTS  set
;IMPORT Word
;IMPORT  Wr


; compare : CompareTyp

; TYPE Arr_ref = ARRAY of  ElemTyp
;VAR WrT : Wr.T
REVEAL  T = Public OBJECT

  ; count :INTEGER
OVERRIDES

; Init := init_m
; Card := card_m
; isIn := isin_m
; InsertElem := inserel_m
; removeElem := re_elm
; compare := comp_m

END

; PROCEDURE init_m (self: T, SizeHint := 25)  : T
= BEGIN

;SizeHint : INTEGER := 25
; Space : Arr_ref :=NIL
END

; PROCEDURE card_m :INTEGER
  BEGIN
;  card_val := 0
; INCR (card_val)
RETURN card_val
END

;PROCEDURE Hash(m : T) : Word.T


; PROCEDURE inserel_m (self : T , Elem : ElemTyp)
= BEGIN

   ;Arr_ref :=  Hash(Elem : Elemtyp)
   ; Wr.Puttext(WrT," Element is inserted "  & Wr.EOL)
END

; PROCEDURE isin_m (self : T , Elem : ElemTyp)
= BEGIN
; VAR mx : INTEGER
; VAR low : INTEGER
;VAR Isequal : BOOLEAN
;VAR high : INTEGER
; VAR Lcompare : CompareTyp
;VAR Probe :  INTEGER
; IF  Elem.Space:= NIL
THEN
mx:=0
;Isequal := FALSE
ELSE
;low := 0
;high :=  card_val -1
; LOOP
      IF low > high
         THEN
                 ;mx:=low
                 ; Isequal := FALSE
                  ;EXIT
         ELSE
                   ;Probe := (low + high) DRV 2
                   ; Lcompare := compare(Elem.Space ^[Probe])
CASE  Lcompare
OF compare.less
=> high ;= Probe -1 | compareEqual
=> mx := Probe
; Isequal := TRUE
;EXIT       | compareGreater
=> low := Probe +1
                  END  (* CASE *)
               END (*IF *)
            END (*LOOP*)
         END (*IF*)
END (*isin_m*)

   END


