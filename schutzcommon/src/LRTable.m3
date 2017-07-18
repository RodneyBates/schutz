
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE LRTable 

(* Internal data used in generating and accessing LALR parsing tables. *) 

; IMPORT Fmt 
; IMPORT Text 

; IMPORT LbeStd 
; IMPORT MessageCodes 
; IMPORT Assertions 

; FROM Assertions IMPORT Assert , CantHappen , AssertionFailure 

; TYPE AFT = MessageCodes . T 

(* VISIBLE: *) 
; PROCEDURE ProdNoImage ( ProdNo : ProdNoTyp ) : TEXT 

  = BEGIN 
      IF ProdNo = ProdNoNull 
      THEN RETURN "Null"
      ELSIF ProdNo = ProdNoInfinity 
      THEN RETURN "Infinity"
      ELSE RETURN Fmt . Int ( ProdNo ) 
      END (* IF *) 
    END ProdNoImage 

(* VISIBLE: *) 
; PROCEDURE OptionIdSetImage ( Set : OptionIdSetTyp ) : TEXT 

  = CONST BufferSize = 1 + 5 * MaxOptionId 
  ; VAR LResult := ARRAY [ 0 .. BufferSize - 1 ] OF CHAR { ' ' , .. }  
  ; VAR LResultSs : CARDINAL  
  ; VAR LElemText : TEXT 
  ; VAR LElemChar : CHAR 

  ; BEGIN 
      LResult [ 0 ] := '{' 
    ; LResultSs := 1
    ; FOR RElem := 0  TO LAST ( OptionIdTyp ) - 1 
      DO 
        IF RElem IN Set 
        THEN
          IF LResultSs > 1 
          THEN  
            LResult [ LResultSs ] := ','
          ; INC ( LResultSs )  
          END (* IF *) 
        ; LElemText := Fmt . Int ( RElem ) 
        ; FOR RElemSs := 0 TO Text . Length ( LElemText ) - 1 
          DO  
            LElemChar := Text . GetChar ( LElemText , RElemSs )  
          ; IF LElemChar # ' ' 
            THEN 
              LResult [ LResultSs ] := LElemChar 
            ; INC ( LResultSs ) 
            END (* IF *) 
          END (* FOR *) 
        END (* IF *) 
      END (* FOR *) 
    ; LResult [ LResultSs ] := '}' 
    ; INC ( LResultSs ) 
    ; RETURN Text . FromChars ( SUBARRAY ( LResult , 0 , LResultSs ) ) 
    END OptionIdSetImage 

(* Associativities. *) 

; PROCEDURE AssocImage ( Val : AssocTyp ) : TEXT 

  = BEGIN 
      CASE Val 
      OF AssocTyp . right => RETURN "right" 
      | AssocTyp . left => RETURN "left" 
      | AssocTyp . none => RETURN "none" 
      | AssocTyp . nonassoc => RETURN "nonassoc" 
      END (* CASE *) 
    END AssocImage 

(* LR parsing states *) 

(* VISIBLE: *) 
; PROCEDURE TokImageDefault 
    ( <* UNUSED *> Self : Public ; Tok : LbeStd . TokTyp ) : TEXT 

  = BEGIN (* TokImageDefault *) 
      RETURN LbeStd . NumIdTokImage ( Tok ) 
    END TokImageDefault 

(* VISIBLE: *) 
; PROCEDURE EmptyString ( ) : TokArrayRefTyp 
  (* Could be shared.  Treat as immutable. *) 

  = BEGIN 
      RETURN NIL
    END EmptyString  

(* VISIBLE: *) 
; PROCEDURE SingletonString ( Tok : LbeStd . TokTyp ) 
  : TokArrayRefTyp
  (* Could be shared.  Treat as immutable. *) 

  = VAR LResult : TokArrayRefTyp

  ; BEGIN 
(* TODO: Atomize/share these results. *) 
      LResult := NEW ( TokArrayRefTyp , 1 ) 
    ; LResult ^ [ 0 ] := Tok 
    ; RETURN LResult 
    END SingletonString 

(* VISIBLE: *) 
; PROCEDURE IsEmptyString 
    ( String : TokArrayRefTyp ; Tok : LbeStd . TokTyp ) 
  : BOOLEAN 
  (* String is empty.  Did I really need to say that? *) 

  = BEGIN 
      RETURN String = NIL OR NUMBER ( String ^ ) = 0 
    END IsEmptyString 

(* VISIBLE: *) 
; PROCEDURE IsSingletonString 
    ( String : TokArrayRefTyp ; Tok : LbeStd . TokTyp ) 
  : BOOLEAN 
  (* String contains exactly one token. *) 

  = BEGIN 
      RETURN String # NIL AND NUMBER ( String ^ ) = 1 
    END IsSingletonString 

(* VISIBLE: *) 
; PROCEDURE IsSingletonStringOf 
    ( String : TokArrayRefTyp ; Tok : LbeStd . TokTyp ) 
  : BOOLEAN 
  (* String contains exactly one token, and it is Tok. *) 

  = BEGIN 
      RETURN String # NIL AND NUMBER ( String ^ ) = 1 AND String ^ [ 0 ] = Tok  
    END IsSingletonStringOf 

(* VISIBLE: *) 
; PROCEDURE Action 
    ( Gram : GrammarTyp 
    ; State : LbeStd . LRStateTyp 
    ; Tok : LbeStd . TokTyp 
    ) 
  : LbeStd . LRStateTyp 
  RAISES { AssertionFailure } 

  = VAR LState : LbeStd . LRStateTyp 
  ; VAR LNBase : CARDINAL 

  ; BEGIN (* Action *) 
      Assert 
        ( State < Gram . FirstReadRedAction 
        , AFT . A_Parse_Action_BadStartingState 
        ) 
    ; IF Tok < Gram . FirstTerminal 
      THEN 
        CantHappen ( AFT . A_Parse_Action_BadTokenLow ) 
      ; RETURN StateNoNull  
      ELSIF Tok <= Gram . LastAstNonterminal 
      THEN (* This a token that ParseTrv could return. *) 
        LState := State 
      ; LOOP 
          WITH 
            WAction 
            = Gram . ActionTableRef ^ 
                [ Gram . BaseRef ^ [ LState ] + Tok - Gram . FirstTerminal ] 
          DO IF WAction . Check = LState 
             THEN 
               RETURN WAction . Next 
             ELSE 
               LState := Gram . DefaultRef ^ [ LState ] 
             ; IF LState = StateNoNull THEN RETURN LState END (* IF *) 
             END (* IF *) 
          END (* WITH *) 
        END (* LOOP *) 
      ELSIF Tok <= Gram . LastNonterminal 
      THEN (* This is a pure nonterminal that cannot be supplied by
              ParseTrv, only pushed after reductions.  There can be no
              errors here. 
           *) 
        LNBase := Gram . NBaseRef ^ [ State ] 
      ; RETURN 
          Gram . NNextRef ^ [ LNBase + Tok - Gram . FirstNonterminal ] 
      ELSE 
        CantHappen ( AFT . A_Parse_Action_BadTokenHigh )
      ; RETURN StateNoNull 
      END (* IF *) 
    END Action 

(* VISIBLE: *) 
; PROCEDURE AcceptProdNo ( Gram : GrammarTyp ) : ProdNoTyp 

  = BEGIN (* AcceptProdNo *) 
      RETURN Gram . AcceptProdNo 
    END AcceptProdNo 

(* VISIBLE: *) 
; PROCEDURE Continuation 
    ( Gram : GrammarTyp ; State : LbeStd . LRStateTyp ) : LbeStd . TokTyp 
  RAISES { AssertionFailure } 

  = BEGIN (* Continuation *) 
      Assert 
        ( State < Gram . FirstReadRedAction 
        , AFT . A_Parse_Action_BadStartingState 
        ) 
    ; RETURN Gram . ContinuationRef ^ [ State ] 
    END Continuation 

(* VISIBLE: *) 
; PROCEDURE ShortParseCheckCost 
    ( <* UNUSED *> Gram : GrammarTyp 
    ; Expected : LbeStd . LimitedTokCtTyp 
    ; Actual : LbeStd . LimitedTokCtTyp 
    ; Accepted : BOOLEAN 
    ) 
    : LbeStd . RepairCostTyp 
  (* Return additional cost due to parse check being shorter than 
     desired.  Take into account the case where input has been accepted. *) 

  = BEGIN (* ShortParseCheckCost *) 
      IF Accepted 
      THEN 
        RETURN 0 
      ELSE 
        RETURN MAX ( 0 , Expected - Actual ) 
      END (* IF *) 
    END ShortParseCheckCost 

; BEGIN (* LRTable *) 
  END LRTable 
. 
