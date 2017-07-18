
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE Files 

(* Routines for accessing Schutz-specific files. *) 

; IMPORT Rd  
; IMPORT Rsrc 
; IMPORT Thread 

; IMPORT LbeStd 
; IMPORT PaintHs
; IMPORT ScannerIf  

; FROM Assertions IMPORT AssertionFailure 

; EXCEPTION Error ( TEXT ) 

; PROCEDURE RegularFileExists 
     ( FileName : TEXT ; CreateIfNot : BOOLEAN := FALSE ) 
  : BOOLEAN (* Existence before any entailed creation. *) 
  RAISES { Thread . Alerted } 

; PROCEDURE ReadLangPickle ( FileName : TEXT ; ResourcePath : Rsrc . Path ) 
  : REFANY 
  RAISES { Error , Thread . Alerted }  

; PROCEDURE ReadNamedImageFile ( FileName : TEXT ) 
  : PaintHs . ImageTransientTyp 
  RAISES { AssertionFailure , Error , Thread . Alerted } 
  (* Reads an est, image, or checkpoint file.  Builds an ImageRef for an est 
     file. Does not set IpImageName, ItAbsTextFileName, or 
     ItAbsPickleFileName. 
  *) 

(* Language definitions. *) 

; PROCEDURE ParseTextRdT 
    ( Lang : LbeStd . LangTyp 
    ; ScanIf : ScannerIf . ScanIfTyp 
    ; File : Rd . T 
    ; VAR NewTreeRef : LbeStd . EstRootTyp 
    ; InsertNilFixedChildren := FALSE 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 
  (* Parse from a file. *) 

; PROCEDURE OpenEmptyFile ( ImageName : TEXT ) 
  : PaintHs . ImageTransientTyp 
  RAISES { AssertionFailure , Error , Thread . Alerted }  
  (* Does not set IpImageName, ItAbsTextFileName, or ItAbsPickleFileName. 
     Does not write a pickle file. 
  *) 

; PROCEDURE OpenNamedTextFile ( FileName : TEXT ) 
  : PaintHs . ImageTransientTyp 
  RAISES { AssertionFailure , Error , Thread . Alerted } 
  (* Does not set IpImageName, ItAbsTextFileName, or ItAbsPickleFileName. 
     Does not write a pickle file. 
  *) 

; PROCEDURE WriteImagePickle 
    ( Ref : REFANY 
    ; AbsFileName : TEXT  
    ; DoCreateVersion : BOOLEAN 
    ) 
  RAISES { Error , Thread . Alerted } 

; PROCEDURE WriteText 
    ( ImageRef : PaintHs . ImageTransientTyp 
    ; FileName : TEXT 
    ) 
  RAISES { AssertionFailure , Error , Thread . Alerted } 
(* TODO: ^This is unused as of 2004-05-20.  See if it should go. *) 

; END Files 
. 
