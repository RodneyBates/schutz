
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* Some selectively-revealed data structure for LangUtil. *) 

INTERFACE LangUtilRep 

; IMPORT LangUtil 

; TYPE FsFmtNoMapTyp 
    = ARRAY (* EstHs . FmtNoTyp *) OF LangUtil . FsChildNoTyp 

; TYPE FsFmtNoMapRefTyp = REF FsFmtNoMapTyp 

; REVEAL LangUtil . FsNodeRefTyp 
    = LangUtil . FsNodeRefPublicTyp 
        BRANDED "LangUtil.FsNodeRefTyp" 
        OBJECT 
          FsFmtNoMapRef : FsFmtNoMapRefTyp := NIL 
        END (* OBJECT *) 
(* TODO: See if more fields need to be moved into here. *) 

; END LangUtilRep 
. 
