
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

(* "File Strings.i3". Rodney M. Bates.  Sep. 1997 *) 
(* Copyright Rodney M. Bates. 1997, 1999. 
   Licensed under Gnu Public License. *) 
(* A string package, tailored to certain usage patterns. *) 
(* A string package, tailored to certain usage patterns. *)
(* Version that works in PM3. *) 

INTERFACE Strings 
(* 
   A Strings.T is a mutable string.  It is designed to efficiently 
   support the expected usage pattern where lots of substrings of 
   Text.Ts will be passed around, maybe substringed again, and 
   many of them eventually will be concatenated, left to right into 
   a new string.  This might also be converted to a Text.T, after 
   it is finished. 
 
   Procedures in this interface that might need to allocate internal 
   space that could be later expanded by concatenation accept an 
   optional hint parameter for the length the string might grow to. 
   Clients may exceed the hint. 
*) 

; IMPORT Text 

; IMPORT PortTypes 
; IMPORT Assertions 

(* Treat TYPE SpaceType and StringTyp ( = T ) as opaque. 
   Use Copy instead of := and AreEqual instead of = *) 

; TYPE SpaceTyp = Text . T (* REF ARRAY OF CHAR *)  
; TYPE StringTyp = T 
; TYPE T 
    = RECORD 
        Text : Text . T := NIL 
      ; Space : SpaceTyp := NIL 
      ; FromSs : StringSsTyp := 0 
      ; ToSs : StringSsTyp := 0 
      ; EventualLengthHint : StringSsTyp := 0 
      END (* RECORD *) 
(* TODO: ^This is a leftover.  Make it opaque. *) 
; TYPE StringSsTyp = PortTypes . Card32Typ 
; TYPE StringSsSignedTyp = PortTypes . Int32Typ 

; EXCEPTION SsOutOfBounds 

(* TODO: Establish some consistency/sanity over mutable/functional
         character of these strings. *) 

; PROCEDURE FromText 
    ( Txt : TEXT ; EventualLengthHint : StringSsTyp := 0 ) : T 

; PROCEDURE ToText 
    ( String : T ; From : StringSsTyp := 0 ) : TEXT 
    RAISES { Assertions . AssertionFailure } 

; PROCEDURE ToTextNonNIL 
    ( String : T ; From : StringSsTyp := 0 ) : TEXT 
    RAISES { Assertions . AssertionFailure } 

; PROCEDURE FromChar 
    ( Ch : CHAR ; EventualLengthHint : StringSsTyp := 0 ) : T 

; PROCEDURE FromChars 
    ( READONLY Chars : ARRAY OF CHAR 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    : T 

; PROCEDURE FetchChars 
    ( VAR Chars : ARRAY OF CHAR ; String : T ) 
    RAISES { Assertions . AssertionFailure } 
   (* Truncate or leave suffix of Chars unchanged if lengths are # *) 

; PROCEDURE IthChar 
    ( String : T ; I : StringSsTyp ) : CHAR RAISES { SsOutOfBounds } 

; PROCEDURE StoreIthChar 
    ( VAR (* IN OUT *) String : T 
    ; I : StringSsTyp 
    ; Ch : CHAR 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    RAISES { SsOutOfBounds , Assertions . AssertionFailure } 

; PROCEDURE AppendInPlace 
    ( VAR (*IN OUT *) Left : T 
    ; Right : T 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    RAISES { Assertions . AssertionFailure } 

; PROCEDURE AppendTextInPlace 
    ( VAR (*IN OUT *) Left : T 
    ; Right : TEXT (* Can be NIL *) 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    RAISES { Assertions . AssertionFailure } 

; PROCEDURE AppendCharInPlace 
    ( VAR (*IN OUT *) Left : T 
    ; Ch : CHAR 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    RAISES { Assertions . AssertionFailure } 

; PROCEDURE MakeEmpty ( VAR String : T ) 

; PROCEDURE Empty ( EventualLengthHint : StringSsTyp := 0 ) : T 

; PROCEDURE Length ( String : T ) : StringSsTyp 

; PROCEDURE Substring 
    ( String : T 
    ; From : StringSsTyp := 0 
    ; For : StringSsTyp := LAST ( StringSsTyp ) 
    ) : T 
    RAISES { Assertions . AssertionFailure } 

; PROCEDURE TruncateInPlace 
    ( VAR (* IN OUT *) String : T ; ToLength : StringSsTyp ) 
    RAISES { Assertions . AssertionFailure } 
  (* Noop if ToLength > Length ( String ) *) 

; PROCEDURE LeftTruncateInPlace 
    ( VAR (* IN OUT *) String : T ; ToLength : StringSsTyp ) 
    RAISES { Assertions . AssertionFailure } 
  (* Remove characters from the left, shortening the string 
     to ToLength. *) 
  (* Noop if ToLength > Length ( String ) *) 

; PROCEDURE DeleteCharsInPlace 
    ( VAR (* IN OUT *) String : T 
    ; PrefixLength : StringSsTyp 
    ; DeleteCount : INTEGER 
    ) 
    RAISES { SsOutOfBounds , Assertions . AssertionFailure } 
  (* After the prefix, delete DeleteCount characters, 
     shifting the suffix left and shortening the string 
     by DeleteCount. Attempting to delete beyond the 
     end of the string does nothing. *) 

; PROCEDURE InsertBlanksInPlace 
    ( VAR (* IN OUT *) String : T 
    ; PrefixLength : StringSsTyp 
    ; BlankCount : INTEGER 
    ; EventualLengthHint : StringSsTyp := 0 
    ) 
    RAISES { SsOutOfBounds , Assertions . AssertionFailure } 
  (* After the prefix, insert BlankCount blanks, shifting 
     the suffix right and extending the string by BlankCount *) 

; TYPE ProcArrayOfChar = PROCEDURE ( READONLY Chars : ARRAY OF CHAR ) 

; PROCEDURE InvokeWithArrayOfChar 
    ( String : T ; Proc : ProcArrayOfChar ) 
    RAISES { Assertions . AssertionFailure } 

; PROCEDURE AreEqual ( Left , Right : T ) : BOOLEAN 

; PROCEDURE AreEqualButForTrailingBlanks ( Left , Right : T ) : BOOLEAN 

; PROCEDURE Copy ( String : T ) : T 

; PROCEDURE PosOf1stNonblank 
    ( READONLY String : T ) : StringSsTyp 
    RAISES { Assertions . AssertionFailure } 

; PROCEDURE PosOfLastNonblank 
    ( READONLY String : T ) : StringSsSignedTyp 
    RAISES { Assertions . AssertionFailure } 

; PROCEDURE AtLeastBlanks ( MinLength : StringSsTyp ) : T 
  RAISES { Assertions . AssertionFailure } 
  (* Return a string that is all blanks and of at least MinLength *) 
  (* Depends on clients' not mutating the result. *) 

; END Strings 
. 
