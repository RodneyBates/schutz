MODULE EstDump

; IMPORT FileWr 
; IMPORT Fmt 
; IMPORT Stdio
; IMPORT Text 
; IMPORT Wr 

; IMPORT EstHs
; IMPORT EstUtil
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT Misc 
; IMPORT TravUtil
; IMPORT VersionedFiles 

; VAR GWrT : Wr . T 

; PROCEDURE WEOL ( )

  = BEGIN
      Wr . PutText ( GWrT , Wr . EOL ) 
    END WEOL

; PROCEDURE FL ( )

  = BEGIN
      Wr . Flush ( GWrT ) 
    END FL

; PROCEDURE WT ( Txt : TEXT )

  = BEGIN
      Wr . PutText ( GWrT , Txt ) 
    END WT 

; PROCEDURE WC ( Ch : CHAR )

  = BEGIN
      Wr . PutChar ( GWrT , Ch ) 
    END WC 

; PROCEDURE WI ( Int : INTEGER )

  = BEGIN
      Wr . PutText ( GWrT , Fmt . Int ( Int ) ) 
    END WI 

; PROCEDURE WX ( Addr : REFANY )

  = BEGIN
      Wr . PutText ( GWrT , Misc . RefanyImage ( Addr ) ) 
    END WX 

; PROCEDURE WriteTreeWr  
    ( WrT : Wr . T 
    ; RootRef : LbeStd . EstRootTyp
    ; Lang : LbeStd . LangTyp
    ; Label : TEXT := "" 
    )

  = BEGIN
      GWrT := WrT

    ; IF Label # NIL AND NOT Text . Equal ( Label , "" )
      THEN WT ( Label )
      ELSE WT ( "EstRoot" ) 
      END (* IF *) 
    ; WEOL ( )
    ; WriteNode
        ( RootRef , Indent := 0
        , NodeNo := 0
        , FmtNo := EstHs . FmtNoNull
        , Lang := Lang
        )
    ; FL ( ) 
    END WriteTreeWr 

; PROCEDURE WriteTreeToStdout  
    ( RootRef : LbeStd . EstRootTyp
    ; Lang : LbeStd . LangTyp
    )

  = BEGIN
      WriteTreeWr
        ( Stdio . stdout , RootRef , Lang , "Stdout:" ) 
    END WriteTreeToStdout 

; PROCEDURE WriteTreeToFile 
    ( FileName : TEXT
    ; RootRef : LbeStd . EstRootTyp
    ; Lang : LbeStd . LangTyp
    )

  = VAR LWrT : Wr . T

  ; BEGIN
      LWrT := VersionedFiles . OpenWrite ( FileName ) 
    ; WriteTreeWr
        ( LWrT , RootRef , Lang , "TreeDump file: \"" & FileName & "\"")
    ; Wr . Close ( LWrT ) 
    END WriteTreeToFile 

; PROCEDURE WriteNode
    ( NodeRef : LbeStd . EstRootTyp 
    ; Indent : INTEGER  
    ; NodeNo : LbeStd . EstNodeNoTyp 
    ; <*UNUSED*> FmtNo : EstHs . FmtNoTyp 
    ; Lang : LbeStd . LangTyp 
    ; Mnemonic : BOOLEAN := FALSE 
    ) 

  = VAR LEstTravInfo : TravUtil . EstTravInfoTyp

  ; BEGIN
      TYPECASE NodeRef
      OF NULL 
        => WT ( Misc . Blanks ( Indent ) )
        ; WT ( "NIL" )
        ; WEOL ( ) 

      | EstHs . EstRefTyp ( TEstRef ) 
        => TravUtil .  InitEstTravInfoFwd 
            ( (* OUT *) LEstTravInfo 
            , NodeRef 
            , KindSet := TEstRef . EstChildKindSet 
            , ParentAbsNodeNo := NodeNo
            ) 

        ; <* ASSERT NodeNo = LEstTravInfo . EtiAbsNodeNo *> 
           
          WT ( Misc . Blanks ( Indent ) )
        ; WT ( "NodeNo:" )
        ; WI ( LEstTravInfo . EtiAbsNodeNo )
        ; WT ( " FIELDS:" ) 
        ; WT ( " Addr:" ) 
        ; WX ( TEstRef ) 
        ; WT ( " NodeCt:" ) 
        ; WI ( EstUtil . EstNodeCt ( TEstRef ) )
        ; WT ( " Ht:" ) 
        ; WI ( TEstRef . EstHeight )
        ; WC ( ' ' )
        ; WT ( EstHs . EstNodeKindImage ( TEstRef . EstNodeKind ) ) 
        ; WEOL ( )
        
        ; WT ( Misc . Blanks ( Indent + GFieldsIndent ) )
        ; WT ( "LeftTok:" )
        ; WT ( LangUtil . TokImage ( TEstRef . EstLeftTok , Lang ) ) 
        ; WT ( " Tok:" )
        ; WT ( LangUtil . TokImage ( TEstRef . EstTok , Lang ) )  
        ; WT ( " RightTok:" )
        ; WT ( LangUtil . TokImage ( TEstRef . EstRightTok , Lang ) ) 
        ; WT ( " Sem:" ) 
        ; WX ( TEstRef . EstSemRef )
        ; WEOL ( )
        
        ; WT ( Misc . Blanks ( Indent + GFieldsIndent ) )
        ; WT ( EstHs . EstChildKindSetImage
                 ( TEstRef . EstChildKindSet
                 , EstHs .ImageKindTyp . Short
                 , Indent := Indent + GFieldsIndent 
                 ) 
             )
        ; WEOL ( )

        ; WHILE LEstTravInfo . EtiChildNo < LEstTravInfo . EtiChildCt
          DO

          (* Child info found within parent node. *) 
            WT ( Misc . Blanks ( Indent ) )
          ; WT ( "NodeNo:" )
          ; WI ( LEstTravInfo . EtiAbsNodeNo )
          ; WT ( " ChildNo:" ) 
          ; WI ( LEstTravInfo . EtiChildNo )
          ; WT ( " FmtNo: " ) 
          ; WI ( LEstTravInfo . EtiChildFmtNo )
          ; WT ( " ChildRelNodeNo:" )
          ; WI ( LEstTravInfo . EtiChildRelNodeNo )
          ; WT ( " ChildAddr:" ) 
          ; WX ( LEstTravInfo . EtiChildLeafElem . LeChildRef ) 
          ; WEOL ( )

          ; WT ( Misc . Blanks ( Indent + GFieldsIndent ) )
          ; WT ( EstHs . EstChildKindSetImage
                   ( LEstTravInfo . EtiChildLeafElem . LeKindSet
                   , EstHs .ImageKindTyp . Short
                   , Indent := Indent + GFieldsIndent 
                   ) 
               )
          ; WEOL ( )

          (* The child node itself. *) 
          ; WriteNode
              ( LEstTravInfo . EtiChildLeafElem . LeChildRef  
              , Indent + GNodeIndent 
              , LEstTravInfo . EtiAbsNodeNo + LEstTravInfo . EtiChildRelNodeNo
              , LEstTravInfo . EtiChildFmtNo
              , Lang
              , Mnemonic
              ) 

          ; TravUtil . IncEstChild ( (* IN OUT *) LEstTravInfo )
          END (* WHILE *)

        ; WT ( Misc . Blanks ( Indent ) )
        ; WT ( "NodeNo:" )
        ; WI ( LEstTravInfo . EtiAbsNodeNo )
        ; WT ( " END" ) 
        ; WEOL ( )

      ELSE
        WT ( Misc . Blanks ( Indent ) )
      ; WT ( EstUtil . EstNodeImage ( NodeRef , Indent , NodeNo , Lang ) ) 
      ; WEOL ( )
      END (* TYPECASE *) 
    END WriteNode

  ; BEGIN 
    END EstDump 
.


