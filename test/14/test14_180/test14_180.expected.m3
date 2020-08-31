(* Rodney M. Bates.  CS 697v, spring 2006, examples of:
   1) Procedures passed as parameters.
   2) Nested procedures with ocal variable access.
*)

MODULE ProcParams EXPORTS Main

; IMPORT Fmt
; IMPORT Stdio
; IMPORT Text
; IMPORT Wr

; TYPE NodeTyp = RECORD LeftChild , RightChild : NodeRefTyp ; Val : TEXT END

; TYPE NodeRefTyp = REF NodeTyp

(* Note recursive type NodeTyp and NodeRefTyp *)
(* These types support a binary tree with strings as data fields.  *)

; TYPE ClosureTyp = OBJECT METHODS visit ( Node : NodeTyp ) END

; PROCEDURE Traverse ( Root : NodeRefTyp ; Closure : ClosureTyp )
        (* Traverse the tree rooted at Root in inorder, calling back Visit for each node. *)

  = PROCEDURE Recurse ( Node : NodeRefTyp )

    = BEGIN
        IF Node # NIL
        THEN
          Recurse ( Node ^ . LeftChild )
        ; Visit ( Node )
        ; Recurse ( Node ^ . RightChild )
        END (* IF *)
      END Recurse

    = BEGIN (* Traverse *)
      Recurse ( Root )
    END Traverse

; PROCEDURE Verify ( Root : NodeRefTyp )
  (* Verify that:
     1) All Val fields are non-NIL texts.
     2) They are in nondescending order.
  *)

  = VAR LastVal : TEXT

  ; PROCEDURE Visit ( Node : NodeRefTyp )

    = BEGIN
        IF Node # NIL
        THEN
          IF Node ^ . Val = NIL
          THEN
            Wr . PutText ( Stdio . stderr , "NIL node value." & Wr . EOL )
          ELSE
            IF LastVal # NIL AND Text . Compare ( LastVal , Node ^ . Val ) >= 0
            THEN
              Wr . PutText
                ( Stdio . stderr
                , "Value \"" & Node ^ . Val & "\" Out of order after \""
                  & LastVal
                  & "\"."
                  & Wr . EOL
                )
            END (* IF *)
          END (* IF *)
        ; LastVal := Node ^ . Val
        END (* IF *)
      END Visit

  ; BEGIN (* Verify *)
      LastVal := NIL
    ; Traverse ( Root , Visit )
    END Verify

; TYPE CharSetTyp = SET OF CHAR

; PROCEDURE LetterOccs ( Root : NodeRefTyp )
  (* Count and print the number of distinct letters in strings in the tree
     rooted at Root.
  *)

  = VAR Chars := CharSetTyp { }
  ; VAR Ct : INTEGER := 0
  (* Chars and Ct are local to LetterOccs and accessed as nonlocal variables
       from Visit.  This works even though LetterOccs only calls Visit indirectly,
       through Traverse.
    *)

  ; PROCEDURE Visit ( Node : NodeRefTyp )
          (* A different declaration of the same identifier Visit, in a different scope. *)
    = VAR LChar : CHAR

    ; BEGIN
        IF Node # NIL AND Node ^ . Val # NIL
        THEN
          FOR I := 0 TO Text . Length ( Node ^ . Val ) - 1
          DO
            LChar := Text . GetChar ( Node ^ . Val , I )
          ; IF NOT LChar IN Chars
            THEN
              INC ( Ct )
            ; Chars := Chars + CharSetTyp { LChar }
            END (* IF *)
          END (* FOR *)
        END (* IF *)
      END Visit

  ; BEGIN (* LetterOccs *)
      Traverse ( Root , Visit )
    ; Wr . PutText
        ( Stdio . stderr , Fmt . Int ( Ct ) & " Distinct characters." & Wr . EOL )
    END LetterOccs

; PROCEDURE Build ( ) : NodeRefTyp
        (* Build a sample tree, with an intentional sequence error. *)
  = BEGIN
      RETURN
        NEW
          ( NodeRefTyp
          , LeftChild := NEW ( NodeRefTyp , Val := "A" )
          , Val := "C"
          , RightChild
              := NEW
                   ( NodeRefTyp
                   , LeftChild := NEW ( NodeRefTyp , Val := "E" )
                   , Val := "G"
                   , RightChild := NEW ( NodeRefTyp , Val := "F" )
                   )
          )
    END Build

; VAR Tree : NodeRefTyp

; BEGIN
    Tree := Build ( )
  ; Verify ( Tree )
  ; LetterOccs ( Tree )
  END ProcParams
.
