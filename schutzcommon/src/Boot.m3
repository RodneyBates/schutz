
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Boot 

(* Print some data generated during Ldl bootstrapping. *) 

; IMPORT Fmt 
; IMPORT Thread 
; IMPORT Wr 

; IMPORT EstHs 
; IMPORT EstUtil 
; IMPORT LangUtil 
; IMPORT ModHs
; IMPORT PortTypes 
; IMPORT SharedStrings 
; IMPORT UnsafeUtils 

(* VISIBLE *) 
; PROCEDURE PrintSizes ( WrT : Wr . T ) 

  = PROCEDURE PrintSize ( Value : PortTypes . Int32Typ ; Label : TEXT ) 

    = <* FATAL Wr . Failure *> 
      <* FATAL Thread . Alerted *> 
      BEGIN
	Wr . PutText
	  ( WrT 
	  , Fmt . Pad ( Fmt . Int ( Value ) , 10 ) 
	    & " " & Label & Wr . EOL 
	  ) 
      END PrintSize

  ; PROCEDURE PS ( TC : UnsafeUtils . TypeCodeTyp ; Label : TEXT ) 
    (* Print heap consumed size for TC *) 

    = BEGIN
	PrintSize ( UnsafeUtils . ObjectSize ( TC ) , Label ) 
      END PS 

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN
      Wr . PutText 
        ( WrT , "Node byte sizes, including header, are:" & Wr . EOL ) 
    ; PS ( TYPECODE ( EstHs . EstLeafRefTyp ) , "EstLeafRefTyp" ) 
    ; PS ( TYPECODE ( EstHs . EstNonleafRefTyp ) , "EstNonleafRefTyp" ) 
    ; PS ( TYPECODE ( EstHs . KTreeLeafRefTyp ) , "KTreeLeafRefTyp" ) 
    ; PS ( TYPECODE ( EstHs . KTreeNonleafRefTyp ) , "KTreeNonleafRefTyp" ) 
    ; PS ( TYPECODE ( ModHs . ModBlankLineTyp ) , "ModBlankLineTyp" ) 
    ; PS ( TYPECODE ( ModHs . ModLexErrTyp ) , "ModLexErrTyp" ) 
    ; PS ( TYPECODE ( ModHs . ModSyntErrTyp ) , "ModSyntErrTyp" ) 
    ; PS ( TYPECODE ( ModHs . ModDelTyp ) , "ModDelTyp" ) 
    ; PS ( TYPECODE ( ModHs . ModCmntTrailingFixedTyp ) , "ModCmntTrailingFixedTyp" ) 
    ; PS ( TYPECODE ( ModHs . ModCmntTrailingRelativeTyp ) , "ModCmntTrailingRelativeTyp" ) 
    ; PS ( TYPECODE ( ModHs . ModCmntLeadingFixedTyp ) , "ModCmntLeadingFixedTyp" ) 
    ; PS ( TYPECODE ( ModHs . ModCmntLeadingRelativeTyp ) , "ModCmntLeadingRelativeTyp" ) 
    ; PS ( TYPECODE ( ModHs . ModTextTyp ) , "ModTextTyp" ) 
    ; PS ( TYPECODE ( ModHs . EstDummyTokTyp ) , "EstDummyTokTyp" ) 
    ; PS ( TYPECODE ( ModHs . EstDummyTempMarkTyp ) , "EstDummyTempMarkTyp" ) 
    ; PS ( TYPECODE ( SharedStrings . T ) , "SharedStrings . T" ) 
    ; PS ( TYPECODE ( LangUtil . FsNodeRefTyp ) , "LangUtil . FsNodeRefTyp" ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    END PrintSizes 

(* VISIBLE *) 
; PROCEDURE PrintStats 
    ( WrT : Wr . T ; READONLY Stats : EstUtil . StatisticsTyp ) 

  = PROCEDURE PrintStat ( Value : PortTypes . Int32Typ ; Label : TEXT ) 

    = <* FATAL Wr . Failure *> 
      <* FATAL Thread . Alerted *> 
      BEGIN
	Wr . PutText
	  ( WrT 
	  , Fmt . Pad ( Fmt . Int ( Value ) , 10 ) 
	    & " " & Label & Wr . EOL 
	  ) 
      END PrintStat

  ; <* FATAL Wr . Failure *> 
    <* FATAL Thread . Alerted *> 
    BEGIN
      Wr . PutText 
        ( WrT , "Node counts, etc. are:" & Wr . EOL ) 
    ; PrintStat ( Stats . EstLeafCt , "EstLeafCt" )
    ; PrintStat ( Stats . EstNonleafCt , "EstNonleafCt" )
    ; PrintStat ( Stats . KTreeLeafCt , "KTreeLeafCt" )
    ; PrintStat ( Stats . KTreeNonleafCt , "KTreeNonleafCt" )
    ; PrintStat ( Stats . LeafArrayCt , "LeafArrayCt" )
    ; PrintStat ( Stats . LeafArrayElemCt , "LeafArrayElemCt" )
    ; PrintStat ( Stats . LeafArraySize , "LeafArraySize" )
    ; PrintStat ( Stats . NonleafArrayCt , "NonleafArrayCt" )
    ; PrintStat ( Stats . NonleafArrayElemCt , "NonleafArrayElemCt" )
    ; PrintStat ( Stats . NonleafArraySize , "NonleafArraySize" )
    ; PrintStat ( Stats . ModBlankLineCt , "ModBlankLineCt" )
    ; PrintStat ( Stats . ModLexErrCt , "ModLexErrCt" )
    ; PrintStat ( Stats . ModSyntErrCt , "ModSyntErrCt" )
    ; PrintStat ( Stats . ModDelCt , "ModDelCt" )
    ; PrintStat ( Stats . ModCmntTrailingFixedCt , "ModCmntTrailingFixedCt" )
    ; PrintStat ( Stats . ModCmntTrailingRelativeCt , "ModCmntTrailingRelativeCt" )
    ; PrintStat ( Stats . ModCmntLeadingFixedCt , "ModCmntLeadingFixedCt" )
    ; PrintStat ( Stats . ModCmntLeadingRelativeCt , "ModCmntLeadingRelativeCt" )
    ; PrintStat ( Stats . ModTextCt , "ModTextCt" )
    ; PrintStat ( Stats . AstCt , "AstCt" )
    ; PrintStat ( Stats . DummyTokCt , "DummyTokCt" )
    ; PrintStat ( Stats . DummyTempMarkCt , "DummyTempMarkCt" )
    ; PrintStat ( Stats . NilCt , "NilCt" )    
    ; PrintStat ( SharedStrings . UniqueStringCt ( ) , "Unique shared strings" ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    END PrintStats 

; BEGIN
  END Boot 

. 
