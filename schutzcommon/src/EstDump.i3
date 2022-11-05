INTERFACE EstDump


; IMPORT Wr 

; IMPORT EstHs 
; IMPORT LbeStd 

; VAR GNodeIndent := LbeStd . StdIndent
      (* Amount to increase indentation for children. *)
; VAR GFieldsIndent := 6
      (* Amount to increase indentation for fields. *)

; PROCEDURE WriteTreeWr  
    ( WrT : Wr . T 
    ; RootRef : LbeStd . EstRootTyp
    ; Lang : LbeStd . LangTyp
    ; Label : TEXT := "" 
    )

; PROCEDURE WriteTreeToStdout  
    ( RootRef : LbeStd . EstRootTyp
    ; Lang : LbeStd . LangTyp
    )

; PROCEDURE WriteTreeToFile 
    ( FileName : TEXT
    ; RootRef : LbeStd . EstRootTyp
    ; Lang : LbeStd . LangTyp
    )

; PROCEDURE WriteNode
    ( NodeRef : LbeStd . EstRootTyp 
    ; Indent := LbeStd . StdIndent 
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; FmtNo : EstHs . FmtNoTyp 
    ; Lang : LbeStd . LangTyp 
    ; Mnemonic : BOOLEAN := FALSE 
    ) 

  ; END EstDump 
.
