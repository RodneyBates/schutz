
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2020, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

MODULE Parser 

(* Parsing.  Includes batch parsing from a file and incremental
   (re)parsing of an Est with possibly text edits embedded within.
   Handles sytax error recovery/repair and (re)building of the Est.
*)  

; IMPORT Fmt 
; IMPORT Stdio 
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr 

; IMPORT EstBuild 
; IMPORT EstHs 
; IMPORT EstUtil
; IMPORT IntSets 
; FROM LangUtil IMPORT ChildOptTyp , FsKindTyp , PredicateKindTyp 
; IMPORT LangMap 
; IMPORT LangUtil 
; IMPORT LbeStd 
; IMPORT LRTable 
; IMPORT Marks 
; IMPORT MessageCodes 
; IMPORT Misc 
; IMPORT ModHs 
; IMPORT Options 
; IMPORT ParseHs 
; IMPORT ParseTrv 
; IMPORT PortTypes 
; IMPORT SharedStrings 
; IMPORT TokRelation 
; IMPORT VersionedFiles 

; IMPORT Assertions 
; FROM Assertions IMPORT Assert , AssertionFailure , CantHappen 

; TYPE AFT = MessageCodes . T 

; TYPE MarkKindTyp = Marks . MarkKindTyp 

; TYPE TrialStateTyp 
    = RECORD 
        TsParseStackTopRef : ParseHs . ParseStackElemRefTyp := NIL 
      ; TsBuildStackTopRef : ParseHs . BuildStackElemRefTyp := NIL 
      ; TsWaitingBuildStackElemRef : ParseHs . BuildStackElemRefTyp := NIL  
      (* ^List of a syntactic error and antideletion token insertions 
          waiting to be merged in front of the next token shifted: *) 
      ; TsParseTravStateRef : ParseHs . ParseTravStateRefTyp := NIL 
      ; TsTempMarkListRef : ParseHs . TempMarkArrayRefTyp := NIL  
      ; TsJustShifted : BOOLEAN := FALSE 
        (* TsJustShifted is true if the last parsing operation 
           leading to this state was a shift *) 
      END (* RECORD  TrialStateTyp *) 

; VAR GNullTrialState : TrialStateTyp
      (* ^Just use the field values of TrialStateTyp. *)

; CONST StateListStaticMax = 4 
; VAR StateListMax : PortTypes . Int32Typ := 1 (* StateListStaticMax *)  
(* ^Someday, make this runtime variable. *) 
; TYPE StateListElemNoTyp = [ 0 .. StateListStaticMax - 1 ] 
; TYPE StateListTyp 
    = RECORD 
        (* A circular buffer. *) 
        SlLatest : StateListElemNoTyp 
      ; SlOldest : StateListElemNoTyp 
      (* The circular buffer always has at least one element, so 
         when ( SlLatest + 1 ) MOD StateListMax = SlOldest, the 
         buffer is full, not empty. *) 
      ; SlStates : ARRAY StateListElemNoTyp OF TrialStateTyp 
      ; SlLastShiftedAstStates : ARRAY StateListElemNoTyp OF TrialStateTyp 
      (* ^Most recent shift of an Ast NT, that is followed only by reduces. 
          TsParseStackTopRef = NIL if no such exists.  1-to-1 correspondence
          of array elements to SlStates. *) 
      END (* RECORD  StateListElemTyp *) 

; PROCEDURE RepairId ( READONLY ParseInfo : ParseHs . ParseInfoTyp ) : TEXT 

  = BEGIN 
      RETURN 
        "(" 
        & Fmt . Int ( ParseInfo . PiAttemptedRepairCt )  
        & "," 
        & Fmt . Int ( ParseInfo . PiAttemptedRepairActionCt )  
        & ")" 
    END RepairId 

; CONST TraceFileName = LbeStd . AppName & "ParseTrace"

; PROCEDURE WriteTraceParseBegin ( ) RAISES { Thread . Alerted } 

  = <* FATAL Wr . Failure *>
    BEGIN 
      IF Options . ParseTracing  
      THEN 
        IF Options . TraceWrT # NIL AND NOT Wr . Closed ( Options . TraceWrT ) 
        THEN Wr . Close ( Options . TraceWrT ) 
        END (* IF *) 
      ; Options . TraceFileName := TraceFileName 
      ; TRY 
          Options . TraceWrT := VersionedFiles . OpenWrite ( TraceFileName ) 
        ; Wr . PutText 
            ( Options . TraceWrT 
            , "######################## Begin Parse Trace #####################"
              & Wr . EOL 
            ) 
        ; Wr . Flush ( Options . TraceWrT ) 
        EXCEPT VersionedFiles . Error  
        => (* Just give up writing trace. *) 
        END (* TRY EXCEPT *) 
      END (* IF *) 
    END WriteTraceParseBegin

; VAR WriteTrials : BOOLEAN := FALSE (* Maybe change this in a debugger. *)

; PROCEDURE WriteParseTraceText ( Msg : TEXT ) 
  RAISES { Thread . Alerted } 

  = <* FATAL Wr . Failure *> 
    BEGIN 
      IF Options . ParseTracing 
         AND Options . TraceWrT # NIL 
         AND NOT Wr . Closed ( Options . TraceWrT ) 
      THEN 
        Wr . PutText ( Options . TraceWrT , Msg ) 
      ; Wr . Flush ( Options . TraceWrT ) 
      END (* IF *) 
    END WriteParseTraceText 

; PROCEDURE WriteMoreTokInfo  ( <* UNUSED *> TokInfo : ParseHs . TokInfoTyp ) 
  RAISES { Thread . Alerted } 

  = <* FATAL Wr . Failure *> 
    BEGIN 
(* TODO: Fill this in. *) 
      IF Options . TraceWrT # NIL AND NOT Wr . Closed ( Options . TraceWrT ) 
      THEN 
(*      ; Wr . PutText ( Options . TraceWrT , ) 
      ; Wr . PutText ( Options . TraceWrT , ) 
      ; Wr . PutText ( Options . TraceWrT , ) 
      ; Wr . PutText ( Options . TraceWrT , ) 
      ; Wr . PutText ( Options . TraceWrT , ) 
      ; Wr . PutText ( Options . TraceWrT , ) 
*) 
      END (* IF *) 
    END WriteMoreTokInfo 

; PROCEDURE WriteAction 
    ( READONLY ParseInfo : ParseHs . ParseInfoTyp 
    ; Action : LbeStd . LRStateTyp 
    ) 
  RAISES { Thread . Alerted } 

  = <* FATAL Wr . Failure *> 
    BEGIN 
      Wr . PutText ( Options . TraceWrT , Fmt . Int ( Action ) ) 
    ; IF Action >= ParseInfo . PiGram . FirstReduceAction 
      THEN 
        Wr . PutText ( Options . TraceWrT , " (Reduce, Prod ") 
      ; Wr . PutText 
          ( Options . TraceWrT 
          , Fmt . Int ( Action - ParseInfo . PiGram . FirstReduceAction ) 
          ) 
      ; Wr . PutText ( Options . TraceWrT , ")" ) 
      ELSIF Action >= ParseInfo . PiGram . FirstReadRedAction 
      THEN 
        Wr . PutText ( Options . TraceWrT , " (Shift-Reduce, Prod ") 
      ; Wr . PutText 
          ( Options . TraceWrT 
          , Fmt . Int ( Action - ParseInfo . PiGram . FirstReadRedAction ) 
          ) 
      ; Wr . PutText ( Options . TraceWrT , ")" ) 
      END (* IF *) 
    END WriteAction 

; PROCEDURE NextParseTravState 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; FromStateRef : ParseHs . ParseTravStateRefTyp 
    ; SuccKind : ParseTrv . SuccKindTyp 
    ; LRState : LbeStd . LRStateTyp 
    ; Comment : TEXT := NIL  
    ) 
  : ParseHs . ParseTravStateRefTyp 
  RAISES { Thread . Alerted , AssertionFailure } 
  (* This is a wrapper around ParseTrv . NextParseTravState, that 
     conditionally writes trace output. 
  *) 

  = VAR LResult : ParseHs . ParseTravStateRefTyp 
  ; VAR LAction : LbeStd . LRStateTyp  

  ; <* FATAL Wr . Failure *> 
    BEGIN 
      LResult 
        := ParseTrv . NextParseTravState 
             ( ParseInfo , FromStateRef , SuccKind ) 
    ; IF Options . ParseTracing 
         AND Options . TraceWrT # NIL 
         AND NOT Wr . Closed ( Options . TraceWrT ) 
      THEN 
        CASE SuccKind 
        OF ParseTrv . SuccKindTyp . SuccKindAdvance 
        =>  Wr . PutText ( Options . TraceWrT , "Advance " ) 
        | ParseTrv . SuccKindTyp . SuccKindDescend 
        =>  Wr . PutText ( Options . TraceWrT , "Descend ") 
        END (* CASE *) 
      ; Wr . PutText ( Options . TraceWrT , "from " ) 
      ; Wr . PutText 
          ( Options . TraceWrT 
          , Fmt . Pad ( Misc . RefanyImage ( FromStateRef ) , RefanyPad ) 
          ) 
      ; Wr . PutText ( Options . TraceWrT , ", SeqNo " ) 
      ; Wr . PutText 
          ( Options . TraceWrT , Fmt . Int ( FromStateRef . PtsSeqNo ) ) 
      ; Wr . PutText ( Options . TraceWrT , ", " ) 
      ; Wr . PutText 
          ( Options . TraceWrT 
          , ParseHs . TokInfoImage 
              ( FromStateRef . PtsTokInfo , ParseInfo . PiLang )
          ) 
      ; Wr . PutText ( Options . TraceWrT , Wr . EOL ) 
      ; Wr . PutText ( Options . TraceWrT , "        to   " ) 
      ; Wr . PutText 
          ( Options . TraceWrT 
          , Fmt . Pad ( Misc . RefanyImage ( LResult ) , RefanyPad ) 
          ) 
      ; Wr . PutText ( Options . TraceWrT , ", SeqNo " ) 
      ; Wr . PutText ( Options . TraceWrT , Fmt . Int ( LResult . PtsSeqNo ) ) 
      ; Wr . PutText ( Options . TraceWrT , ", " ) 
      ; Wr . PutText 
          ( Options . TraceWrT 
          , ParseHs . TokInfoImage 
              ( LResult . PtsTokInfo , ParseInfo . PiLang )
          ) 
      ; Wr . PutText ( Options . TraceWrT , ", from LRState " ) 
      ; Wr . PutText ( Options . TraceWrT , Fmt . Int ( LRState ) ) 
      ; Wr . PutText ( Options . TraceWrT , ", Action " )
      ; LAction  
          := LRTable . Action 
               ( ParseInfo . PiGram , LRState , LResult . PtsTokInfo . TiTok ) 
      ; WriteAction ( ParseInfo , LAction ) 
      ; IF Comment # NIL AND NOT Text . Equal ( Comment , "" ) 
        THEN 
          Wr . PutText ( Options . TraceWrT , " (" ) 
        ; Wr . PutText ( Options . TraceWrT , Comment ) 
        ; Wr . PutText ( Options . TraceWrT , ")" ) 
        END 
      ; WriteMoreTokInfo ( LResult . PtsTokInfo )
      ; Wr . PutText ( Options . TraceWrT , Wr . EOL ) 
      ; Wr . Flush ( Options . TraceWrT ) 
      END (* IF *) 
    ; RETURN LResult 
    END NextParseTravState 

; PROCEDURE WriteTraceParseEnd ( ) RAISES { Thread . Alerted } 

  = <* FATAL Wr . Failure *> 
    BEGIN 
      IF Options . ParseTracing  
         AND Options . TraceWrT # NIL 
         AND NOT Wr . Closed ( Options . TraceWrT )
      THEN 
        Wr . PutText 
          ( Options . TraceWrT 
          , "######################## End Parse Trace ########################"
            & Wr . EOL 
          ) 
      ; Wr . Flush ( Options . TraceWrT ) 
      ; Wr . Close ( Options . TraceWrT ) 
      END (* IF *) 
    END WriteTraceParseEnd 

  ; CONST RefanyPad = 4 + 2 * BYTESIZE ( REFANY ) 
  ; CONST TokPad = 25

  ; PROCEDURE DumpParseTrvState 
      ( READONLY ParseInfo : ParseHs . ParseInfoTyp 
      ; State : ParseHs . ParseTravStateRefTyp 
      ) 

    = <* FATAL Thread . Alerted , Wr . Failure *>
      VAR LWrT : Wr . T := Stdio . stderr 

    ; BEGIN 
        Wr . PutText 
          ( LWrT 
          , Fmt . Pad 
              ( ParseHs . TokInfoImage 
                  ( State . PtsTokInfo , ParseInfo . PiLang ) 
              , TokPad  
              )  
          )
      ; Wr . PutText ( LWrT , " LineNo = " ) 
      ; Wr . PutText ( LWrT , Fmt . Int ( ParseInfo . PiLineNo )  ) 
(* TODO: Add more stuff: SeqNo, CharPos, text of VarTerm, etc. *) 
      ; Wr . PutText ( LWrT , Wr . EOL ) 
      END DumpParseTrvState 

  ; PROCEDURE DumpStacks 
      ( READONLY ParseInfo : ParseHs . ParseInfoTyp 
      ; PseTopRef : ParseHs . ParseStackElemRefTyp  
      ; PseOldRef : ParseHs . ParseStackElemRefTyp := NIL 
      ; PseDeepestRef : ParseHs . ParseStackElemRefTyp := NIL 
      ; BseTopRef : ParseHs . BuildStackElemRefTyp 
      ; BseOldRef : ParseHs . BuildStackElemRefTyp := NIL 
      ; BseDeepestRef : ParseHs . BuildStackElemRefTyp := NIL 
      ; DumpAll : BOOLEAN := FALSE 
      ) 

    = <* FATAL Thread . Alerted , Wr . Failure *>
      VAR LWrT : Wr . T := Stdio . stderr 
    ; VAR LPse : ParseHs . ParseStackElemRefTyp 
    ; VAR LBse : ParseHs . BuildStackElemRefTyp 
    ; VAR LPNo , LBNo : CARDINAL 

    ; CONST ItemNoPad = 4
    ; CONST StateNoPad = 5
    ; CONST BlankPsePad 
        = 1 + ItemNoPad + RefanyPad + 1 + TokPad + StateNoPad + 1

    ; BEGIN
        LPse := PseTopRef 
      ; LPNo := 0 
      ; LBse := BseTopRef
      ; LBNo := 0 
      ; LOOP 
          IF LPse = NIL 
             OR ( NOT DumpAll AND LPse = PseDeepestRef ) 
          THEN EXIT 
          ELSE 
            IF LPse = PseOldRef 
            THEN
              Wr . PutText ( LWrT , ">" )
            ELSIF LPse = PseDeepestRef  
            THEN
              Wr . PutText ( LWrT , "D" )
            ELSE 
              Wr . PutText ( LWrT , " " )
            END (* IF *) 
          ; Wr . PutText 
              ( LWrT , Fmt . Pad ( Fmt . Int ( LPNo ) , ItemNoPad ) )
          ; Wr . PutText 
              ( LWrT 
              , Fmt . Pad ( Misc . RefanyImage ( LPse ) , RefanyPad ) 
              ) 
          ; Wr . PutText ( LWrT , " " )
          ; Wr . PutText 
              ( LWrT 
              , Fmt . Pad 
                  ( LangUtil . TokImage
                      ( LPse ^ . PseTok , ParseInfo . PiLang ) 
                  , TokPad  
                  )  
              )
          ; Wr . PutText 
              ( LWrT 
              , Fmt . Pad 
                  ( LRTable . StateNoImage ( LPse ^ . PseLRState  ) 
                  , StateNoPad 
                  )  
              )
          ; IF LBse = LPse ^ . PseDeepestBuildStackElemRef 
            THEN (* No build stack element corresponds to LPse. *) 
              Wr . PutText ( LWrT , Wr . EOL ) 
            ELSIF LBse = NIL 
            THEN (* Build stack terminates prematurely . *) 
              Wr . PutText ( LWrT , Fmt . Pad ( "NIL" , 4 + ItemNoPad ) )
            ; Wr . PutText ( LWrT , Wr . EOL ) 
            ELSE 
              Wr . PutText ( LWrT , " " )
            ; LOOP 
                IF LBse = BseOldRef 
                THEN
                  Wr . PutText ( LWrT , ">" )
                ELSIF LBse = BseDeepestRef 
                THEN
                  Wr . PutText ( LWrT , "D" )
                ELSE 
                  Wr . PutText ( LWrT , " " )
                END (* IF *) 
              ; Wr . PutText 
                  ( LWrT , Fmt . Pad ( Fmt . Int ( LBNo ) , ItemNoPad ) )
              ; Wr . PutText 
                  ( LWrT 
                  , Fmt . Pad ( Misc . RefanyImage ( LBse ) , RefanyPad ) 
                  ) 
              ; Wr . PutText ( LWrT , " " )
              ; Wr . PutText 
                  ( LWrT 
                  , Fmt . Pad 
                      ( ParseHs . TokInfoImage 
                          ( LBse ^ . BseTokInfo , ParseInfo . PiLang )
                      , TokPad  
                      )  
                  )
              ; Wr . PutText ( LWrT , " " )
              ; Wr . PutText 
                  ( LWrT 
                  , Misc . BooleanImageShort 
                      ( LBse ^ . BseTokInfo . TiIsInterior)  
                  )
              ; Wr . PutText 
                  ( LWrT 
                  , Misc . BooleanImageShort ( LBse ^ . BseWasInsertedByParser )
                  )
              ; Wr . PutText 
                  ( LWrT 
                  , Misc . BooleanImageShort 
                      ( LBse ^ . BseWasDeletedByParser )
                  )
              ; Wr . PutText ( LWrT , Wr . EOL ) 
              ; LBse := LBse ^ . BseLink 
              ; INC ( LBNo ) 
              ; IF LBse = LPse ^ . PseDeepestBuildStackElemRef 
                THEN EXIT 
                ELSIF LBse = NIL 
                THEN (* Build stack terminates prematurely . *) 
                  Wr . PutText ( LWrT , Fmt . Pad ( "NIL" , 4 + ItemNoPad ) )
                ; Wr . PutText ( LWrT , Wr . EOL ) 
                ; EXIT 
                ELSE 
                  Wr . PutText ( LWrT , Misc . Blanks ( BlankPsePad ) ) 
                END (* IF *) 
              END (* LOOP *) 
            END (* IF *) 
          ; LPse := LPse ^ . PseLink 
          ; INC ( LPNo ) 
          END (* IF *) 
        END (* LOOP *)
      ; Wr . Flush ( LWrT )  
      END DumpStacks 

; <* UNUSED *> 
  PROCEDURE DumpTrialState 
    ( READONLY ParseInfo : ParseHs . ParseInfoTyp 
    ; READONLY State : TrialStateTyp 
    ) 
  (* This is here to be called within a debugger. *) 

  = BEGIN 
      DumpParseTrvState ( ParseInfo , State . TsParseTravStateRef ) 
    ; DumpStacks 
        ( ParseInfo 
        , PseTopRef := State . TsParseStackTopRef 
        , BseTopRef := State . TsBuildStackTopRef 
        , DumpAll := TRUE 
        ) 
    END DumpTrialState 

; PROCEDURE Shift 
    ( READONLY ParseInfo : ParseHs . ParseInfoTyp 
    ; TokInfo : ParseHs . TokInfoTyp 
    ; ShiftLRState : LbeStd . LRStateTyp 
    ; VAR (* IN OUT *) TrialState : TrialStateTyp 
    ; VAR (* IN OUT *) LastShifted : TrialStateTyp 
    ; Delete : BOOLEAN (* Arrange to delete this token *) 
    ) 
  RAISES { Thread . Alerted } 

  = PROCEDURE ShiftWaitingBuildStackElems 
      ( BuildStackElemRef : ParseHs . BuildStackElemRefTyp ) 
    (* ^Recursive procedure to copy the waiting build stack elements 
        onto the build stack.  One deletion repair can have many 
        following insertion repairs. Each deletion repair needs a separate 
        copy of its list of build stack elements. *) 

    = VAR LBuildStackElemRef : ParseHs . BuildStackElemRefTyp 

    ; BEGIN (* ShiftWaitingBuildStackElems *) 
        IF BuildStackElemRef # NIL 
        THEN 
          ShiftWaitingBuildStackElems ( BuildStackElemRef . BseLink ) 
        ; LBuildStackElemRef 
            := NEW 
                 ( ParseHs . BuildStackElemRefTyp 
                 , BseLink := TrialState . TsBuildStackTopRef 
                 , BseTokInfo := BuildStackElemRef . BseTokInfo 
                 , BseWasInsertedByParser 
                     := BuildStackElemRef . BseWasInsertedByParser 
                 , BseWasDeletedByParser 
                     := BuildStackElemRef . BseWasDeletedByParser 
                 ) 
        ; TrialState . TsBuildStackTopRef := LBuildStackElemRef 
        END (* IF *) 
      END ShiftWaitingBuildStackElems 

  ; PROCEDURE ShiftWriteTrace ( Repairing : BOOLEAN ) 
    RAISES { Thread . Alerted } 

    = <* FATAL Wr . Failure *>
      BEGIN 
        IF Options . ParseTracing  
           AND Options . TraceWrT # NIL 
           AND NOT Wr . Closed ( Options . TraceWrT ) 
        THEN 
          IF Repairing 
          THEN 
            Wr . PutText 
              ( Options . TraceWrT 
              , "(Repair) " 
              )
          ELSE
            Wr . PutText
              ( Options . TraceWrT
              , "(SeqNo="
                & Fmt . Int ( TrialState . TsParseTravStateRef . PtsSeqNo )
                & ")"
              ) 
          END (* IF *) 
        ; Wr . PutText 
            ( Options . TraceWrT 
            , "Shift "
              & ParseHs . TokInfoImage ( TokInfo , ParseInfo . PiLang ) 
              & ", to LRState = " 
              & Fmt . Int ( ShiftLRState )  
              & ", Delete = " 
              & Misc . BooleanImageShort ( Delete ) 
              & ", Full = " 
              & ParseHs . TempMarkRangeImage ( TokInfo . TiFullTempMarkRange ) 
              & ", Patch = " 
              & ParseHs . TempMarkRangeImage ( TokInfo . TiPatchTempMarkRange )
              & Wr . EOL 
            ) 
        ; Wr . Flush ( Options . TraceWrT ) 
        END (* IF *) 
      END ShiftWriteTrace 

  ; BEGIN (* Shift *) 
      VAR LParseStackElemRef : ParseHs . ParseStackElemRefTyp 

    ; BEGIN (* Block for Shift body *) 
        ShiftWriteTrace ( Repairing := Delete ) 
      ; IF TokInfo . TiIsInterior 
        THEN (* Save the pre-shift state, in case we need to back up and 
                descend from it after a series of reductions leads to a
                postponed parsing error. *) 
          LastShifted := TrialState  
        ELSE LastShifted := GNullTrialState  
        END (* IF *) 
      ; LParseStackElemRef := NEW ( ParseHs . ParseStackElemRefTyp ) 
      ; LParseStackElemRef . PseTok := TokInfo . TiTok 
      ; LParseStackElemRef . PseLRState := ShiftLRState 
      ; LParseStackElemRef . PseDeepestBuildStackElemRef 
          := TrialState . TsBuildStackTopRef 
      ; ShiftWaitingBuildStackElems 
          ( TrialState . TsWaitingBuildStackElemRef ) 
      ; TrialState . TsWaitingBuildStackElemRef := NIL 
      ; TrialState . TsBuildStackTopRef 
          := NEW 
               ( ParseHs . BuildStackElemRefTyp 
               , BseTokInfo := TokInfo 
               , BseWasInsertedByParser := Delete 
               , BseWasDeletedByParser := FALSE 
               , BseLink := TrialState . TsBuildStackTopRef 
               ) 
      ; LParseStackElemRef . PseLink := TrialState . TsParseStackTopRef 
      ; TrialState . TsParseStackTopRef := LParseStackElemRef 
      ; TrialState . TsJustShifted := TRUE 
      END (* Block  Shift body block *) 
    END Shift 

; PROCEDURE TempMarkNeedsCopying
    ( ParseMark , OrigMark: ParseHs . TempMarkTyp ) 
  : BOOLEAN 

  = BEGIN 
      RETURN
        ParseMark . TokMark . Kind # MarkKindTyp . Null
        AND ( ParseMark . TokMark . Kind 
              # OrigMark. TokMark . Kind 
              OR ParseMark . TokMark . FmtNo  
                 # OrigMark. TokMark . FmtNo  
              OR ParseMark . EstRef 
                 # OrigMark . EstRef 
            ) 
    END TempMarkNeedsCopying

(* For building during parsing, we need a number of additional fields
   associated with the merge state.  We put them in subtype MergeInfoTyp.  
*) 
; TYPE MergeInfoTyp 
    = EstBuild . MergeStateTyp 
      OBJECT 
        MiDeferredTempMarkSet : IntSets . T
        (* ^Subscripts of temp marks whose Kind and EstRef fields need later
           to be patched, when the field values become known. *) 
      ; MiLMNewChildRef : LbeStd . EstRootTyp := NIL 
      ; MiRMChildRef : LbeStd . EstRootTyp 
      ; MiRMSliceListElemRef : ParseHs . SliceListElemRefTyp 
      ; MiTempMarkListRef : ParseHs . TempMarkArrayRefTyp
        (* ^Just a convenient copy of TsTempMarkListRef, during merge. *) 
      ; MiTempMarkRangeTo : LbeStd . MarkNoTyp 
      END (* MergeInfoTyp *) 

(* We have to patch the FmtNo of temp marks while visiting the
   Fs node for an insertion token, when we know what the FmtNo is.
   But, when we visit the Fs node for what will eventually be the 
   target of a RightSibFmtNo or ChildFmtNo mark, we don't yet know
   which kind, nor do we have the EstRef.  So there are two patch
   procedures, one to patch FmtNos and the other to patch Kind and
   EstRef.

   PatchTempMarksFmtNos is called only from within MergeSliceList, 
   so could be nested inside it, but putting both here makes the
   similarities easier to see, at the cost of passing more parameters. 

   For LeftSibFmtNo marks, we know the kind and all the fields at
   the time of visiting the Fs node, but calling both procedures 
   is correct and easy.  Maybe a specialized one that combines the
   functions slightly more efficiently would be a nice optimization,
   when all the desired features are done and no known bugs exist. 
   And cars run on water, world hunger is eliminated, and pigs fly. 
*)    

; PROCEDURE DeferTempMarkRange 
    ( MergeInfo : MergeInfoTyp (* Fields can change. *)  
    ; TempMarkRange : ParseHs . TempMarkRangeTyp
    ) 
  RAISES { AssertionFailure }
  (* Put the numbers in TempMarkRange into the deferred set. *)

  = BEGIN
      IF NOT ParseHs . RangeIsEmpty ( TempMarkRange ) 
      THEN
        MergeInfo . MiDeferredTempMarkSet
          := IntSets . Union
               ( MergeInfo . MiDeferredTempMarkSet
               , IntSets . Range
                   ( TempMarkRange . From , TempMarkRange . To - 1 )
               )
      END (* IF *) 
    END DeferTempMarkRange 

  ; PROCEDURE CopyFullTempMarks 
    ( READONLY ParseInfo : ParseHs . ParseInfoTyp  
    ; READONLY MergeInfo : MergeInfoTyp (* Fields may change. *) 
    ; READONLY TokInfo : ParseHs . TokInfoTyp 
    ) 
  RAISES { AssertionFailure }
  (* ParseTrv will be making changes to its temp mark list, as it traverses
     the tree.  By the time of a reduce, when this is called, (but hot much
     before, ParseTrv will have finished with all temp marks that are in the
     reduced-from tokens and subtrees.  Here, we copy any such changes to
     the evolving trial state's list. 
  *)  

  = BEGIN
      IF ParseHs . RangeIsEmpty ( TokInfo . TiFullTempMarkRange )
      THEN
        Assert
          ( ParseHs . RangeIsEmpty ( TokInfo . TiPatchTempMarkRange )
          , AFT . A_CopyFullTempMarks_patch_range_wo_full_range
          )
      ELSE
        IF LangUtil . TokClass ( ParseInfo . PiLang , TokInfo . TiTok )
           IN LbeStd . TokClassSetTerm
        THEN
          FOR RTempMarkSs := TokInfo . TiFullTempMarkRange . From 
              TO TokInfo . TiFullTempMarkRange . To - 1
          DO
            MergeInfo . MiTempMarkListRef ^ [ RTempMarkSs ]
              := ParseInfo . PiTravTempMarkListRef ^ [ RTempMarkSs ]
          END (* FOR *) 
        ELSE (* ParseTrv should neither have made nor requested (of this
                Parser), changes to marks in more comples subtrees. *) 
          Assert
            ( ParseHs . RangeIsEmpty ( TokInfo . TiPatchTempMarkRange )
            , AFT . A_CopyFullTempMarks_patch_range_wo_full_range
            )
        ; FOR RTempMarkSs := TokInfo . TiFullTempMarkRange . From 
              TO TokInfo . TiFullTempMarkRange . To - 1
          DO
            Assert
              ( MergeInfo . MiTempMarkListRef ^ [ RTempMarkSs ]
                = ParseInfo . PiTravTempMarkListRef ^ [ RTempMarkSs ]
              , AFT . A_CopyFullTempMarks_changed_temp_mark_inside_Est 
              ) 
          END (* FOR *) 
        END (* IF *) 
      END (* IF *) 
    END CopyFullTempMarks 

; PROCEDURE PatchTempMarkRangeFmtNos
    ( TempMarkListRef : ParseHs . TempMarkArrayRefTyp  
    ; READONLY TempMarkRange : ParseHs . TempMarkRangeTyp 
    ; FmtNo : EstHs . FmtNoTyp
    ) 
  (* Patch FmtNo fields of TempMarks in TempMarkRange.  *) 
  (* PRE: CopyFullTempMarks has been done. *)

  = BEGIN 
      FOR RTempMarkSs := TempMarkRange . From TO TempMarkRange . To - 1 
      DO
        TempMarkListRef ^ [ RTempMarkSs ] . TokMark . FmtNo := FmtNo
      END (* FOR *) 
    END PatchTempMarkRangeFmtNos

; PROCEDURE PatchTempMarkRangeKindsAndEstRefs
    ( TempMarkListRef : ParseHs . TempMarkArrayRefTyp  
    ; READONLY TempMarkRange : ParseHs . TempMarkRangeTyp 
    ; MarkKind : MarkKindTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ) 
  (* Patch Kind and EstRef fields of TempMarks in TempMarkRange. *) 
  (* PRE: CopyFullTempMarks has been done. *)

  = BEGIN 
      FOR RTempMarkSs := TempMarkRange . From TO TempMarkRange . To - 1 
      DO WITH WTempMarkRec = TempMarkListRef ^ [ RTempMarkSs ] 
         DO
           WTempMarkRec . TokMark . Kind := MarkKind    
         ; WTempMarkRec . EstRef := EstRef
         END (* WITH *) 
      END (* FOR *)
    END PatchTempMarkRangeKindsAndEstRefs

; PROCEDURE PatchTempMarkSetKindsAndEstRefs
    ( TempMarkListRef : ParseHs . TempMarkArrayRefTyp  
    ; TempMarkSet : IntSets . T 
    ; MarkKind : MarkKindTyp 
    ; EstRef : LbeStd . EstRootTyp 
    ) 
  (* Patch Kind and EstRef fields of TempMarks in TempMarkSet. *) 
  (* PRE: CopyFullTempMarks has been done. *)

  = PROCEDURE PtmVisit ( TempMarkSs : IntSets . ValidElemT )
    = BEGIN
        WITH WTempMarkRec = TempMarkListRef ^ [ TempMarkSs ] 
        DO
          WTempMarkRec . TokMark . Kind := MarkKind    
        ; WTempMarkRec . EstRef := EstRef
        END (* WITH *) 
      END PtmVisit 

  ; BEGIN (* PatchTempMarkSetKindsAndEstRefs *) 
      IntSets . ForAllDo ( TempMarkSet , PtmVisit )
    END PatchTempMarkSetKindsAndEstRefs 

; PROCEDURE TempMarksForTok 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; MergeInfo : MergeInfoTyp (* Fields can change. *)  
    ; READONLY TokInfo : ParseHs . TokInfoTyp 
    ; FmtNo : EstHs . FmtNoTyp
    ; VAR (* IN OUT *) TempMarksExist : BOOLEAN 
    )
  (* PRE: CopyFullTempMarks has been done. *)

  = BEGIN
      IF LangUtil . TokClass ( ParseInfo . PiLang , TokInfo . TiTok )
         # LbeStd . TokClassTyp . TokClassConstTerm
      THEN (* Not an insertion token. *) 
        Assert
          ( ParseHs . RangeIsEmpty ( TokInfo . TiPatchTempMarkRange ) 
          , AFT . A_TempMarksForTok_Patch_marks_on_noninsertion_tok
          ) 
      ELSIF TempMarksExist
      THEN 
        PatchTempMarkRangeFmtNos
          ( MergeInfo . MiTempMarkListRef
          , TokInfo . TiPatchTempMarkRange
          , FmtNo
          ) 
      ; IF NOT LangUtil . TokClass ( ParseInfo . PiLang , MergeInfo . MsEstTok )
               IN LbeStd . TokClassSetAsList
               (* ^ Merging an Est fixed node.  Will later patch as ChildFmtNo. *)
           OR MergeInfo . MiRMChildRef = NIL 
              (* ^ No child yet.  Will later patch as RightSibFmtNo. *)
        THEN 
          DeferTempMarkRange
            ( MergeInfo , TokInfo . TiPatchTempMarkRange )
        ELSE (* Patch as LeftSib.  We have enough info to do so right now. *)
          PatchTempMarkRangeKindsAndEstRefs 
            ( MergeInfo . MiTempMarkListRef 
            , TokInfo . TiPatchTempMarkRange
            , MarkKindTyp . LeftSibFmtNo 
            , MergeInfo . MiLMNewChildRef
            )
        ; EstBuild . AlterLMChild
            ( MergeInfo , EstHs . EstChildKindSetContainsTempMark) 
(* CHECK: ^That this will be copied up to the root of the Est. *)
        END (* IF *)
      ; TempMarksExist := FALSE 
      END (* IF *)
    END TempMarksForTok  

; PROCEDURE NoteChildExists
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; MergeInfo : MergeInfoTyp (* Fields can change. *)
    ; ChildRef : LbeStd . EstRootTyp
    )
  = BEGIN
(* FIXME: Add containstempmark bit to ChildRef, and, maybe, its parent. *)
      IF MergeInfo . MiRMChildRef = NIL 
      THEN (* First, thus RM, child to be noted. *)
        IF LangUtil . TokClass ( ParseInfo . PiLang , MergeInfo . MsEstTok )
           IN LbeStd . TokClassSetAsList
        THEN (* This is the RM child of a list.  Patch any RightSib tempmarks. *)
          Assert
            ( ChildRef # NIL
            , AFT . A_NoteChildExists_temp_mark_on_nil_child_of_list
            )
        ; PatchTempMarkSetKindsAndEstRefs
            ( MergeInfo . MiTempMarkListRef 
            , MergeInfo . MiDeferredTempMarkSet 
            , MarkKindTyp . RightSibFmtNo
            , ChildRef
            ) 
        ; EstBuild . InclNextRightmostChildKindSet 
            ( MergeInfo , EstHs . EstChildKindSetContainsTempMark ) 
        END (* IF *) 
(* CHECK: ^That this will be copied up to the root of the Est. *)
      ; MergeInfo . MiRMChildRef := ChildRef 
      END (* IF *) 
    END NoteChildExists 

; PROCEDURE InitMergeInfo 
    ( READONLY TrialState : TrialStateTyp 
    ; VAR MergeInfo : MergeInfoTyp 
    ) 

  =  BEGIN 
      MergeInfo . MiTempMarkRangeTo 
        := TrialState . TsParseTravStateRef . PtsTokInfo 
           . TiFullTempMarkRange . To 
    ; MergeInfo . MiDeferredTempMarkSet := IntSets . Empty ( ) 
    ; MergeInfo . MiLMNewChildRef := NIL  
    ; MergeInfo . MiRMChildRef := NIL  
    ; MergeInfo . MiRMSliceListElemRef := NIL
    ; MergeInfo . MiTempMarkListRef := TrialState . TsTempMarkListRef 
    END InitMergeInfo 

; PROCEDURE MergeSliceList 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; MergeInfo : MergeInfoTyp (* Fields can change. *)  
    ; TokInfo : ParseHs . TokInfoTyp 
    ; FmtNo : EstHs . FmtNoTyp 
    ; IsFirstOfGroup : BOOLEAN 
    ) 
  RAISES { AssertionFailure }
  (* Merging right to left.  The slices are linked R to L too. *) 

  = BEGIN (* MergeSliceList *)  

      VAR LSliceListElemRef : ParseHs . SliceListElemRefTyp 
    ; VAR LFromNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LThruNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR LTrailingNodeNo : LbeStd . EstNodeNoTyp
    ; VAR LTrailingChildNo : LbeStd . EstChildNoTyp  
    ; VAR LChildKindSet : EstHs . EstChildKindSetTyp 
    ; VAR LFromLeafElem : EstHs . LeafElemTyp 
    ; VAR LThruLeafElem : EstHs . LeafElemTyp 
    ; VAR LChildLeafElem : EstHs . LeafElemTyp 
    ; VAR LTrailingLeafElem : EstHs . LeafElemTyp 
    ; VAR LLMKindSet : EstHs . EstChildKindSetTyp  
    ; VAR LTokInfoIsUncounted : BOOLEAN 
    ; VAR LTempMarksToPatchExist : BOOLEAN 
    ; VAR LIsFirstOfGroup : BOOLEAN 

    ; BEGIN (* MergeSliceList block *)
        CopyFullTempMarks ( ParseInfo , MergeInfo , TokInfo ) 
      ; LTokInfoIsUncounted 
          := LangUtil . TokClass ( ParseInfo . PiLang , TokInfo . TiTok ) 
             = LbeStd . TokClassTyp . TokClassConstTerm 
      ; LTempMarksToPatchExist 
          := NOT ParseHs . RangeIsEmpty ( TokInfo . TiPatchTempMarkRange )
      ; LSliceListElemRef
          := NARROW ( TokInfo . TiInfo , ParseHs . SliceListElemRefTyp ) 
      ; MergeInfo . MiRMSliceListElemRef := LSliceListElemRef 
      ; WHILE LSliceListElemRef # NIL 
        DO Assert 
             ( LSliceListElemRef ^ . SleNodeRef # NIL 
             , AFT . A_MergeSliceList_NilSleNodeRef 
             ) 
        ; IF LSliceListElemRef ^ . SleIsSlice 
          THEN
            IF LSliceListElemRef ^ . SleFrom < LSliceListElemRef ^ . SleTo 
            THEN (* Slice is nonempty *)
              WITH WParentEstRef 
                = NARROW 
                    ( LSliceListElemRef ^ . SleNodeRef , EstHs . EstRefTyp ) 
              DO 
                EstUtil . GetIthChild 
                  ( WParentEstRef 
                  , LSliceListElemRef ^ . SleTo - 1 
                  , (* VAR *) ResultChildRelNodeNo := LThruNodeNo (* Dead. *)
                  , (* VAR *) ResultLeafElem := LThruLeafElem 
                  ) 
              ; IF LTokInfoIsUncounted OR LTempMarksToPatchExist 
                THEN
                  Assert 
                    ( LangUtil . TokClass 
                        ( ParseInfo . PiLang , TokInfo . TiTok ) 
                      # LbeStd . TokClassTyp . TokClassSublist 
                    , AFT . A_MergeSliceList_SublistHasTokInfoOrTempMarks 
                    )   
                ; EstUtil . GetIthChild 
                    ( WParentEstRef 
                    , LSliceListElemRef ^ . SleFrom 
                    , (* VAR *) ResultChildRelNodeNo := LFromNodeNo (* Dead. *)
                    , (* VAR *) ResultLeafElem := LFromLeafElem 
                    )
                ; IF ISTYPE 
                       ( LFromLeafElem . LeChildRef
                       , ModHs . ModCmntTrailingTyp 
                       ) 
                  THEN
                  
                  (* All of this slice and slices to its right are trailing mods.
                     It's too soon/rightward for TokInfo or TempMarks. *)
                    EstBuild . MergeSlice 
                      ( MergeInfo 
                      , WParentEstRef 
                      , LSliceListElemRef ^ . SleFrom 
                      , LSliceListElemRef ^ . SleTo 
                      , SetFirstOfGroupAndFmtNo := TRUE 
                      , IsFirstOfGroup 
                          := IsFirstOfGroup 
                             AND LSliceListElemRef ^ . SlePredLink = NIL 
                      , GroupFmtNo := FmtNo 
                      , (* VAR *) LeftmostNewChildRef 
                                    := MergeInfo . MiLMNewChildRef 
                      , (* VAR *) LeftmostNewKindSet := LLMKindSet (* Dead. *) 
                      ) 
                  ; NoteChildExists
                      ( ParseInfo , MergeInfo , LThruLeafElem . LeChildRef )
                  ELSE
                  
                  (* Slice begins with a leading mod.  The syntactic token is
                     an insertion token, thus not an explicit child. *) 
                    IF ISTYPE 
                         ( LThruLeafElem . LeChildRef 
                         , ModHs . ModCmntTrailingTyp 
                         ) 
                    THEN (* Begins with leading item and ends with trailing. *)
                    (* We have to split the slice, merge the trailing items,
                       take care of TempMarks (to get the mark bit in the 
                       right place) and TokInfo too (because we split the
                       slice), then merge the leading marks.
                    *) 
(* TODO: Figure out a way to get EstBuild to put the mark bit 
         (Only a mutable bit is needed) into the middle of a range, so we
         don't have to do this otherwise unnecessary tree rebuilding.
*) 
                      EstUtil . NextInKindSet 
                        ( WParentEstRef 
                        , LSliceListElemRef ^ . SleFrom + 1  
                          (* ^Optimization: LM one is known not trailing. *) 
                        , EstHs . EstChildKindSetTrailingMod 
                        , (* VAR *) LTrailingChildNo 
                        , (* VAR *) LTrailingNodeNo (* Dead. *) 
                        , (* VAR *) LTrailingLeafElem (* Dead. *) 
                        )
                    ; Assert 
                        ( LTrailingChildNo < LSliceListElemRef ^ . SleTo   
                        , AFT . A_MergeSliceList_MissingTrailingMod 
                        ) 
                      (* Merge the trailing mods. *) 
                    ; EstBuild . MergeSlice 
                        ( MergeInfo 
                        , WParentEstRef  
                        , LTrailingChildNo  
                        , LSliceListElemRef ^ . SleTo 
                        , SetFirstOfGroupAndFmtNo := FALSE  
                        , IsFirstOfGroup := FALSE (* Dead. *) 
                        , GroupFmtNo := FmtNo (* Dead. *) 
                        , (* VAR *) LeftmostNewChildRef 
                                      := MergeInfo . MiLMNewChildRef 
                        , (* VAR *) LeftmostNewKindSet := LLMKindSet (* Dead. *)
                        )
                    ; NoteChildExists
                        ( ParseInfo , MergeInfo , LThruLeafElem . LeChildRef )
                    ; TempMarksForTok
                        ( ParseInfo
                        , MergeInfo
                        , TokInfo
                        , FmtNo
                        , (* IN OUT *) LTempMarksToPatchExist
                        ) 
                    ; IF LTokInfoIsUncounted 
                      THEN (* Only trailing items can be to the right. *) 
                        EstBuild . PrependTokInfo 
                          ( MergeInfo , TokInfo . TiTok ) 
                      ; LTokInfoIsUncounted := FALSE 
                      END (* IF *)
                     (* Now merge the leading mods. *)
                   ; EstBuild . MergeSlice 
                        ( MergeInfo 
                        , WParentEstRef 
                        , LSliceListElemRef ^ . SleFrom 
                        , LTrailingChildNo   
                        , SetFirstOfGroupAndFmtNo := TRUE 
                        , IsFirstOfGroup 
                            := IsFirstOfGroup 
                               AND LSliceListElemRef ^ . SlePredLink = NIL 
                        , GroupFmtNo := FmtNo 
                        , (* VAR *) LeftmostNewChildRef 
                            := MergeInfo . MiLMNewChildRef 
                        , (* VAR *) LeftmostNewKindSet := LLMKindSet (* Dead. *)
                        ) 
                    ELSE (* Begins and ends with leading items. *) 
                      TempMarksForTok
                        ( ParseInfo
                        , MergeInfo
                        , TokInfo
                        , FmtNo
                        , (* IN OUT *) LTempMarksToPatchExist
                        ) 
                    ; IF LTokInfoIsUncounted 
                      THEN (* Only trailing items can be to the right. *) 
                        EstBuild . PrependTokInfo 
                          ( MergeInfo , TokInfo . TiTok ) 
                      ; LTokInfoIsUncounted := FALSE 
                      END (* IF *) 
                    ; EstBuild . MergeSlice 
                        ( MergeInfo 
                        , WParentEstRef 
                        , LSliceListElemRef ^ . SleFrom 
                        , LSliceListElemRef ^ . SleTo 
                        , SetFirstOfGroupAndFmtNo := TRUE 
                        , IsFirstOfGroup 
                            := IsFirstOfGroup 
                               AND LSliceListElemRef ^ . SlePredLink = NIL 
                        , GroupFmtNo := FmtNo 
                        , (* VAR *) LeftmostNewChildRef 
                            := MergeInfo . MiLMNewChildRef 
                        , (* VAR *) LeftmostNewKindSet := LLMKindSet (* Dead. *)
                        )
                    ; NoteChildExists
                        ( ParseInfo , MergeInfo , LThruLeafElem . LeChildRef )
                    END (* IF *) 
                  END (* IF *) 
                ELSE (* Neither TokInfo nor TempMarks to deal with. *) 
                  EstBuild . MergeSlice 
                    ( MergeInfo 
                    , WParentEstRef 
                    , LSliceListElemRef ^ . SleFrom 
                    , LSliceListElemRef ^ . SleTo 
                    , SetFirstOfGroupAndFmtNo 
                        := LangUtil . TokClass 
                             ( ParseInfo . PiLang , TokInfo . TiTok ) 
                           # LbeStd . TokClassTyp . TokClassSublist 
                           (* A slice may begin with some other FmtNo
                              than the EstListChild FmtNo, by starting with a 
                              mod attached to an insertion token.  In this 
                              case, We don't want to mess with FirstOfGroup 
                              or FmtNo *) 
                    , IsFirstOfGroup 
                        := IsFirstOfGroup 
                           AND LSliceListElemRef ^ . SlePredLink = NIL 
                    , GroupFmtNo := FmtNo 
                    , (* VAR *) LeftmostNewChildRef 
                        := MergeInfo . MiLMNewChildRef 
                    , (* VAR *) LeftmostNewKindSet := LLMKindSet (* Dead. *)
                    ) 
                ; NoteChildExists
                    ( ParseInfo , MergeInfo , LThruLeafElem . LeChildRef )
                END (* IF *) 
              END (* WITH WParentEstRef *) 
            END (* IF Slice is nonempty *) 
          ELSE (* NOT SleIsSlice.  It's a single child. *) 
            IF NOT ISTYPE 
                     ( LSliceListElemRef ^ . SleNodeRef 
                     , ModHs . ModCmntTrailingTyp 
                     ) 
            THEN 
              TempMarksForTok
                ( ParseInfo
                , MergeInfo
                , TokInfo
                , FmtNo
                , (* IN OUT *) LTempMarksToPatchExist
                ) 
            ; IF LTokInfoIsUncounted  
              THEN (* The child is a leading mod. *) 
                EstBuild . PrependTokInfo ( MergeInfo , TokInfo . TiTok ) 
              ; LTokInfoIsUncounted := FALSE 
              END (* IF *) 
            END (* IF*) 
          ; LIsFirstOfGroup 
              := IsFirstOfGroup AND LSliceListElemRef ^ . SlePredLink = NIL 
          ; LChildKindSet := LSliceListElemRef ^ . SleKindSet 
          ; IF NOT LIsFirstOfGroup 
            THEN 
              LChildKindSet 
                := LChildKindSet - EstHs . EstChildKindSetFirstOfGroup
              (* The original KindSet from ParseTrv could have FirstOfGroup,
                 but this is no longer correct in the Est node we are building. 
                 We must turn it off.
              *) 
            END (* IF *) 
          ; EstBuild . MergeChild 
              ( MergeInfo 
              , LSliceListElemRef ^ . SleNodeRef 
              , LChildKindSet 
              , LIsFirstOfGroup 
              , GroupFmtNo := FmtNo 
              ) 
          ; MergeInfo . MiLMNewChildRef := LSliceListElemRef ^ . SleNodeRef
          ; NoteChildExists
              ( ParseInfo , MergeInfo , LSliceListElemRef ^ . SleNodeRef )
          END (* IF SleIsSlice *) 
        ; LSliceListElemRef := LSliceListElemRef ^ . SlePredLink 
        END (* WHILE over elements of slice list. *) 
      ; TempMarksForTok
          ( ParseInfo
          , MergeInfo
          , TokInfo
          , FmtNo
          , (* IN OUT *) LTempMarksToPatchExist
          ) 
      ; IF LTokInfoIsUncounted 
        THEN
          EstBuild . PrependTokInfo ( MergeInfo , TokInfo . TiTok ) 
        ; LTokInfoIsUncounted := FALSE (* Dead, but robust & consistent. *) 
        END 
      END (* Block *) 
    END MergeSliceList 

; PROCEDURE FinishMergeInfo 
    ( READONLY ParseInfo : ParseHs . ParseInfoTyp  
    ; MergeInfo : MergeInfoTyp (* Fields can change. *) 
    ; EstRef : EstHs . EstRefTyp 
    ) 
  RAISES { AssertionFailure } 

  = BEGIN 
      IF NOT IntSets . IsEmpty ( MergeInfo . MiDeferredTempMarkSet ) 
      THEN
        IF NOT LangUtil . TokClass ( ParseInfo . PiLang , MergeInfo . MsEstTok )
               IN LbeStd . TokClassSetAsList
           AND NOT IntSets . IsEmpty ( MergeInfo . MiDeferredTempMarkSet ) 
        THEN (* Finished building a fixed Est node.  Patch deferred temp marks
                as ChildFmtNo marks pointing to it. *) 
          PatchTempMarkSetKindsAndEstRefs 
            ( MergeInfo . MiTempMarkListRef 
            , TempMarkSet := MergeInfo . MiDeferredTempMarkSet
            , MarkKind := MarkKindTyp . ChildFmtNo  
            , EstRef := EstRef 
            ) 
        ; EstRef . EstChildKindSet
            := EstRef . EstChildKindSet + EstHs . EstChildKindSetContainsTempMark
        END (* IF *) 
      END (* IF *)
    END FinishMergeInfo 

; PROCEDURE FindEstChildInSliceList 
    ( SliceListRoot : ParseHs . SliceListElemRefTyp 
    ; VAR EstChild : LbeStd . EstRootTyp 
      (* ^NIL if none found. *) 
    ; VAR EstChildKindSet : EstHs . EstChildKindSetTyp 
    ) 

  RAISES { AssertionFailure } 
  (* There will be at most one Est child in a slice list.  *) 

  = VAR LSliceListElemRef : ParseHs . SliceListElemRefTyp 
  ; VAR LChildNo : LbeStd . EstChildNoTyp 
  ; VAR LNodeNo : LbeStd . EstNodeNoTyp 
  ; VAR LLeafElem : EstHs . LeafElemTyp 

  ; BEGIN 
      LSliceListElemRef := SliceListRoot 
    ; LOOP 
        IF LSliceListElemRef = NIL 
        THEN (* Node found. *) 
          EstChild := NIL 
        ; EstChildKindSet := EstHs . EstChildKindSetEmpty 
        ; EXIT 
        ELSE 
          IF LSliceListElemRef ^ . SleIsSlice 
          THEN 
            EstUtil . PrevInKindSet 
              ( NARROW ( LSliceListElemRef ^ . SleNodeRef , EstHs . EstRefTyp ) 
              , LSliceListElemRef ^ . SleTo - 1 
              , EstHs . EstChildKindSetEstChild  
              , (* VAR *) LChildNo 
              , (* VAR *) LNodeNo 
              , (* VAR *) LLeafElem 
              ) 
          ; IF LChildNo >= LSliceListElemRef ^ . SleFrom  
            THEN 
              EstChild := LLeafElem . LeChildRef 
            ; EstChildKindSet := LLeafElem . LeKindSet 
            ; EXIT 
            END (* IF *) 
          ELSE 
            IF EstHs . EstChildKindEstChild 
               IN LSliceListElemRef ^ . SleKindSet 
            THEN 
              EstChild := LSliceListElemRef ^ . SleNodeRef 
            ; EstChildKindSet := LSliceListElemRef ^ . SleKindSet 
            ; EXIT 
            END (* IF *) 
          END (* IF *) 
        ; LSliceListElemRef := LSliceListElemRef ^ . SlePredLink 
        END (* IF *) 
      END (* LOOP *) 
    END FindEstChildInSliceList 

; EXCEPTION RedNDone 

; PROCEDURE Reduce 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; ProdNo : LRTable . ProdNoTyp 
    ; VAR StateList : StateListTyp 
    ; Repairing : BOOLEAN 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

  = VAR RedInsertionRepairIsWithin : BOOLEAN 
  ; VAR RedOldParseStackElemRef : ParseHs . ParseStackElemRefTyp 
  ; VAR RedOldBuildStackElemRef : ParseHs . BuildStackElemRefTyp 
  ; VAR RedDeepestParseStackElemRef : ParseHs . ParseStackElemRefTyp 
  ; VAR RedDeepestBuildStackElemRef : ParseHs . BuildStackElemRefTyp 
  ; VAR RedEstChildBuildStackElemRef : ParseHs . BuildStackElemRefTyp 
        (* ^Set non-NIL if and when we hit the Est child of a conditional
            construct, during building.
        *) 
  ; VAR RedModDelRef : ModHs . ModDelTyp 
  ; VAR RedNewEstNodeKind : EstHs . EstNodeKindTyp 
  ; VAR RedDoMarkNewTree : BOOLEAN 
  ; VAR RedEstChildForPredicateIsPresent : BOOLEAN 
  ; VAR RedEstChildRefForPredicate : LbeStd . EstRootTyp 
  ; VAR RedEstChildKindSetForPredicate : EstHs . EstChildKindSetTyp 
  ; VAR RedTempMarkToSs : LbeStd . MarkNoTyp 
  ; VAR RedLineBreakCt : LbeStd . LineNoSignedTyp := 0 
  ; VAR RedCondFmtKnownPresent : BOOLEAN := FALSE 
  ; VAR RedCondFmtKnownAbsent : BOOLEAN := FALSE
  ; VAR RedCondFmtMethod2 : BOOLEAN := FALSE 
  ; VAR RedOptionIdSsToUse : BOOLEAN 
  ; VAR RedMergeInfo : MergeInfoTyp 

  ; PROCEDURE RedDumpStacks ( DumpAll : BOOLEAN := FALSE ) 
    (* This is here to be called within a debugger. *) 

    = BEGIN
        WITH WTrialState = StateList . SlStates [ StateList . SlLatest ] 
        DO 
          DumpStacks 
            ( ParseInfo 
            , WTrialState . TsParseStackTopRef 
            , RedOldParseStackElemRef 
            , RedDeepestParseStackElemRef 
            , WTrialState . TsBuildStackTopRef
            , RedOldBuildStackElemRef 
            , RedDeepestBuildStackElemRef 
            , DumpAll
            ) 
        END (* WITH *) 
      END RedDumpStacks

  ; PROCEDURE RedNextBseMatches ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
    : BOOLEAN 

    = PROCEDURE RedNVisit ( Tok : LbeStd . TokTyp ) 
      RAISES { RedNDone } 

      = VAR LChildFsNodeRef : LangUtil . FsNodeRefTyp 

      ; BEGIN 
          IF LangUtil . TokClass ( ParseInfo . PiLang , Tok ) 
             IN LbeStd . TokClassSetAsList  
          THEN 
            LChildFsNodeRef 
              := LangUtil . FsRuleForTok ( ParseInfo . PiLang , Tok ) 
          ; IF LChildFsNodeRef # NIL 
               AND ( RedOldBuildStackElemRef . BseTokInfo . TiTok 
                     = LChildFsNodeRef . FsEmptyListTok  
                     OR RedOldBuildStackElemRef . BseTokInfo . TiTok 
                        = LChildFsNodeRef . FsSingletonListTok  
                     OR RedOldBuildStackElemRef . BseTokInfo . TiTok 
                        = LChildFsNodeRef . FsPluralListTok  
                   ) 
            THEN 
              RAISE RedNDone 
            END (* IF *)  
          END (* IF *)  
        END RedNVisit  

    ; BEGIN (* RedNextBseMatches *)
        IF RedOldBuildStackElemRef = RedDeepestBuildStackElemRef 
        THEN (* No build stack elements *) 
          RETURN FALSE 
        ELSIF LangUtil . IsInClass 
                ( ParseInfo . PiLang 
                , Tok := RedOldBuildStackElemRef . BseTokInfo . TiTok 
                , Class := FsNodeRef . FsTok 
                )
        THEN (* Matching token *) 
          RETURN TRUE 
        ELSIF FsNodeRef . FsIsInsideList
              AND ( RedOldBuildStackElemRef . BseTokInfo . TiTok 
                    = FsNodeRef . FsSublistTok 
                    OR RedOldBuildStackElemRef . BseTokInfo . TiTok 
                       = FsNodeRef . FsPartialTok
                  ) 
        THEN 
          RETURN TRUE  
        ELSE 
          TRY 
            RedNVisit ( FsNodeRef . FsTok )  
          ; IF LangUtil . TokClass ( ParseInfo . PiLang , FsNodeRef . FsTok ) 
               = LbeStd . TokClassTyp .  TokClassAsClass 
            THEN 
              <* FATAL ANY *> BEGIN 
                TokRelation . ForAllGroundMembers 
                  ( LangMap . LangInfo ( ParseInfo . PiLang ) ^ 
                    . ClassRelation 
                 , FsNodeRef . FsTok 
                 , RedNVisit 
                 ) 
              END (* Block *) 
            END (* IF *) 
          EXCEPT 
          RedNDone => RETURN TRUE 
          END (* TRY EXCEPT *) 
        ; RETURN FALSE 
        END (* IF *) 
      END RedNextBseMatches 
 
  ; PROCEDURE RedIsOmittableEmptyListChild 
      ( FsNodeRef : LangUtil . FsNodeRefTyp ) : BOOLEAN 
    RAISES { AssertionFailure } 
    (* The subtree to be merged is an explicit list node with no children,
       and it can be omitted entirely from the node being built. *) 

    = VAR LSliceListElemRef : ParseHs . SliceListElemRefTyp  
    ; VAR LEstNodeRef : LbeStd . EstRootTyp 

    ; BEGIN 
        IF FsNodeRef . FsKind 
           # FsKindTyp . FsKindEstChildOfFixed 
           (* Child of list must have an explicit NIL. *)
           OR FsNodeRef . FsEstChildIsOptional 
           (* If it's an optional child, absent and empty list are not the 
              same thing.  This distinction is needed, e.g. to distinguish 
              IF ... THEN ... END from IF ... THEN ... ELSE END *) 
           OR RedOldBuildStackElemRef . BseWasInsertedByParser 
        THEN RETURN FALSE 
        ELSE 
          LSliceListElemRef := RedOldBuildStackElemRef . BseTokInfo . TiInfo 
          (* ^Implied NARROW, always OK. *)  
        ; IF LSliceListElemRef = NIL 
          THEN (* No subtree at all. *)  
            RETURN FALSE 
          ELSIF LSliceListElemRef ^ . SlePredLink = NIL 
          THEN (* Singleton slice list. *) 
            IF LSliceListElemRef ^ . SleIsSlice  
            THEN (* An empty list can only happen if built by an earlier
                    reduction, and these are not slices. *) 
              RETURN FALSE 
            ELSE 
              LEstNodeRef := LSliceListElemRef ^ . SleNodeRef 
(* REVIEW the following cases: *) 
            ; IF LEstNodeRef = NIL
              THEN (* Can this happen? *) 
                RETURN TRUE 
              ELSE 
                RETURN 
                  EstUtil . EstChildCt ( LEstNodeRef ) = 0 
                  AND LangUtil . TokClass 
                        ( ParseInfo . PiLang 
                        , EstUtil . EstTok ( LEstNodeRef ) 
                        ) 
                      IN LbeStd . TokClassSetAsList  
              END (* IF *) 
            END (* IF *) 
          ELSE (* Plural slice list. The subtree can't be empty. *) 
            RETURN FALSE 
          END (* IF *) 
        END (* IF *) 
      END RedIsOmittableEmptyListChild 

  ; PROCEDURE RedDoSingletonListOpt 
      ( FsEstChildNodeRef : LangUtil . FsNodeRefTyp 
      ; VAR (* IN OUT *) TokInfo : ParseHs . TokInfoTyp 
      ) 
    RAISES { AssertionFailure } 
    (* Possibly do a singleton list optimization on TokInfo. *) 

    = VAR LSliceListElemRef : ParseHs . SliceListElemRefTyp  
    ; VAR LNewSle : ParseHs . SliceListElemRefTyp  
    ; VAR LListEstRef : LbeStd . EstRootTyp 
    ; VAR LListChildKindSet : EstHs . EstChildKindSetTyp 
    ; VAR LListTok : LbeStd . TokTyp 
    ; VAR LLeafElem : EstHs . LeafElemTyp 
    ; VAR LNodeNo : LbeStd . EstNodeNoTyp
    ; VAR LListMiscInfo , LElemMiscInfo : EstHs . EstMiscInfoTyp  

    ; BEGIN 
        IF LangUtil . TokClass ( ParseInfo . PiLang , TokInfo . TiTok ) 
           IN LbeStd . TokClassSetAsList  
        THEN (* It's an Est list. *) 
          LSliceListElemRef := TokInfo . TiInfo
          (* ^Implied NARROW, always OK. *)  
        ; FindEstChildInSliceList 
            ( LSliceListElemRef  
            , (* VAR *) LListEstRef
            , (* VAR *) LListChildKindSet 
            ) 
        ; IF EstUtil . EstChildCt ( LListEstRef ) = 1 
          THEN (* It's a singleton list. *)  
            IF Options . DoOptimizeSingletonLists
            THEN  
              EstUtil . GetIthChild 
                ( LListEstRef 
                , 0 
                , (* VAR *) ResultChildRelNodeNo := LNodeNo (* Dead. *)
                , (* VAR *) ResultLeafElem := LLeafElem 
                )
            ; LListTok 
               := LangUtil . OptSingletonListAsTok 
                    ( FsEstChildNodeRef 
                    , EstUtil . EstTok ( LLeafElem . LeChildRef )  
                    ) 
            ; IF LListTok # LbeStd . Tok__Null 
              THEN (* By syntactic rules, it's optimizable. *)  
                LListMiscInfo 
                  := EstUtil . EstMiscInfo ( ParseInfo . PiLang , LListEstRef ) 
              ; LElemMiscInfo 
                  := EstUtil . EstMiscInfo 
                       ( ParseInfo . PiLang , LLeafElem . LeChildRef )  
              ; IF LListMiscInfo . EmiWidthInfo = LElemMiscInfo . EmiWidthInfo 
                THEN (* It's optimizable. *) 
                  LNewSle := NEW ( ParseHs . SliceListElemRefTyp ) 
                ; LNewSle . SlePredLink := NIL 
                ; LNewSle . SleIsSlice := FALSE 
                ; LNewSle . SleNodeRef := LLeafElem . LeChildRef 
                ; LNewSle . SleKindSet 
                    := LListChildKindSet 
                         + EstHs . EstChildKindSetOptSingletonList  
                ; TokInfo . TiInfo := LNewSle 
                ; TokInfo . TiSliceListRMRoot := LNewSle 
                  (* ^Just to simplify debugging. *) 
                ; INC ( GSingletonListOptCt ) 
                ELSE (* Not optimizable, only because of unequal WidthInfo. *) 
                  Assertions . MessageText  
                    ( "Singleton list not optimized in Parse because of unequal WidthInfo" ) 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          ; INC ( GSingletonListCt ) 
          END (* IF *) 
        END (* IF *) 
      END RedDoSingletonListOpt 

  ; PROCEDURE RedMergeForFmtNo 
      ( FmtNo : EstHs . FmtNoTyp 
      ; IsInsTok : BOOLEAN 
      ; DoOmitChild : BOOLEAN := FALSE 
      ) 
    RAISES { AssertionFailure } 
    (* PRE: A matching Bse is next, in RedOldBuildStackElemRef. *) 
    (* POST: RedOldBuildStackElemRef has been popped for the token
             and any insertions to its left.
    *) 

    = VAR LInsertionIsToLeft : BOOLEAN 
    ; VAR LSecondBuildStackElemRef : ParseHs . BuildStackElemRefTyp 

    ; BEGIN (* RedMergeForFmtNo *) 
        Assert 
          ( NOT RedOldBuildStackElemRef . BseWasDeletedByParser 
          , AFT . A_RedMergeForFmtNo_FmtGroupStartsWithInsertedBse 
          ) 
      ; LSecondBuildStackElemRef := RedOldBuildStackElemRef . BseLink
      ; LInsertionIsToLeft 
          := LSecondBuildStackElemRef 
             # RedDeepestBuildStackElemRef 
(* FIXME: ^This isn't right.  We need to compare to 
        RedOldParseStackElemRef . PseDeepestBuildStackElemRef, 
        but RedOldParseStackElemRef is not being maintained.
*) 
             AND LSecondBuildStackElemRef . BseWasDeletedByParser 
      ; IF RedOldBuildStackElemRef . BseWasInsertedByParser 
        THEN (* Was inserted by the parser.  Will have no TempMarks. *) 
          IF IsInsTok 
          THEN (* Construct deletion mod and ascertain whether it should be 
                  merged to left of the real token. *) 
            Assert
              ( RedOldBuildStackElemRef . BseTokInfo . TiInfo = NIL 
              , AFT . A_RedMergeForFmtNoFmt_DeletedFormatterHasChildren 
              ) 
          ; EstBuild . PrependTokInfo 
              ( RedMergeInfo , RedOldBuildStackElemRef . BseTokInfo . TiTok ) 
            (* The token will be displayed on the screen as a proposed 
               insertion, so count it and its width. *) 
          ; IF RedModDelRef = NIL 
            THEN 
              RedModDelRef := NEW ( ModHs . ModDelTyp ) 
            ; RedModDelRef . ModDelThruFmtNo := FmtNo 
            ; RedModDelRef . ModDelIsRepair := TRUE 
            END (* IF *) 
          ; IF LSecondBuildStackElemRef = RedDeepestBuildStackElemRef 
               OR NOT LSecondBuildStackElemRef . BseWasInsertedByParser 
               OR LangUtil . TokClass 
                    ( ParseInfo . PiLang 
                    , LSecondBuildStackElemRef . BseTokInfo . TiTok 
                    ) 
                  IN LbeStd . TokClassSetEstSubtree 
               OR TRUE 
(* FIX: This was failing to close the ModDel when the next thing back
      was merging a NIL.  So I made it close it always.  This could result
      in multiple ModDels for the same FmtNo, in succession, making for 
      slightly larger Ests than absolutely necessary.  
      I think all the traversers can deal with this.
      Get this working. Maybe just close DelMods lazily? 
*) 
            THEN (* Close the ModDel and merge it. *) 
              EstBuild . MergeChild 
                ( RedMergeInfo 
                , RedModDelRef 
                , EstHs . EstChildKindSetInsertionRepairModDel 
                , IsFirstOfGroup := NOT LInsertionIsToLeft 
                , GroupFmtNo := FmtNo 
                ) 
            ; RedMergeInfo . MiLMNewChildRef := RedModDelRef  
            ; NoteChildExists ( ParseInfo , RedMergeInfo , RedModDelRef )
            ; RedModDelRef := NIL 
            ; RedInsertionRepairIsWithin := TRUE 
            END (* IF *) 
          ELSE (* The parser-inserted token is an Est child *) 
            MergeSliceList 
              ( ParseInfo 
              , RedMergeInfo 
              , RedOldBuildStackElemRef . BseTokInfo 
              , FmtNo 
              , IsFirstOfGroup := NOT LInsertionIsToLeft 
              ) 
          ; RedInsertionRepairIsWithin := TRUE 
          END (* IF IsInsTok *) 
        ELSIF NOT DoOmitChild 
        THEN (* A real child. *) 
          MergeSliceList 
            ( ParseInfo 
            , RedMergeInfo 
            , RedOldBuildStackElemRef . BseTokInfo 
            , FmtNo 
            , IsFirstOfGroup := NOT LInsertionIsToLeft 
            ) 
        END (* IF BseWasInsertedByParser *) 
      (* Merge any insertions that are separate build stack elements. *) 
      ; WHILE LInsertionIsToLeft 
        DO RedOldBuildStackElemRef := LSecondBuildStackElemRef 
        ; LSecondBuildStackElemRef := RedOldBuildStackElemRef . BseLink
        ; LInsertionIsToLeft 
            := LSecondBuildStackElemRef 
               # RedDeepestBuildStackElemRef 
(* FIXME: This isn't right.  We need to compare to 
        RedOldParseStackElemRef . PseDeepestBuildStackElemRef, 
        but RedOldParseStackElemRef is not being maintained.
*) 
               AND LSecondBuildStackElemRef . BseWasDeletedByParser 
        ; MergeSliceList 
            ( ParseInfo 
            , RedMergeInfo 
            , RedOldBuildStackElemRef . BseTokInfo 
            , FmtNo 
            , IsFirstOfGroup := NOT LInsertionIsToLeft 
            ) 
        END (* WHILE *) 
      ; RedOldBuildStackElemRef := LSecondBuildStackElemRef 
      END RedMergeForFmtNo 

  ; PROCEDURE RedFindEstChildRefForPredicate 
      ( FsNodeRef : LangUtil . FsNodeRefTyp  
      ; READONLY ReduceInfo : LRTable . ReduceInfoTyp 
      ) 
    RAISES { AssertionFailure } 
    (* Set RedEstChildForPredicateIsPresent.  
       If TRUE, starting from the current build stack element and searching 
       leftward, find the RM Est child in the range covered by the current 
       Pse whose  token is in the class of FsNodeRef. 
       Set RedEstChildRefForPredicate to this child, and
       RedEstChildKindSetForPredicate to its KindSet from above.   
       Set RedEstChildRefForPredicate NIL if none exists.  
    *) 
    (* Used only for handwritten concrete grammar. *) 

    = VAR LEstChildRef : LangUtil . FsNodeRefTyp  
    ; VAR LBuildStackElemRef : ParseHs . BuildStackElemRefTyp 

    ; BEGIN (* RedFindEstChildRefForPredicate *) 
        LEstChildRef := LangUtil . FsEstChildRef ( FsNodeRef ) 
      ; IF LEstChildRef . FsEstChildOpt = ChildOptTyp . OptRequired 
           (* Always TRUE for child of Est list. *) 
        THEN RedEstChildForPredicateIsPresent := TRUE 
        ELSE 
          RedEstChildForPredicateIsPresent 
            := FsNodeRef . FsOptionIds [ RedOptionIdSsToUse ] 
               IN ReduceInfo . OptionIdSet 
        END (* IF *) 
      ; IF RedEstChildForPredicateIsPresent 
        THEN 
          LBuildStackElemRef := RedOldBuildStackElemRef 
        ; LOOP 
            IF LBuildStackElemRef = RedDeepestBuildStackElemRef 
            THEN 
              Assert 
                ( FALSE 
                , AFT . A_Parser_RedFindEstChildRefForPredicate_Present_Est_Child_not_Found 
                )   
            ; RedEstChildRefForPredicate := NIL 
            ; RedEstChildKindSetForPredicate := EstHs . EstChildKindSetEmpty 
            ; EXIT 
            ELSIF LangUtil . TokClass 
                    ( ParseInfo . PiLang 
                    , LBuildStackElemRef ^ . BseTokInfo . TiTok 
                    ) 
                  IN LbeStd . TokClassSetEstSentential 
            THEN (* This should be the Bse we want. *) 
              FindEstChildInSliceList 
                ( LBuildStackElemRef . BseTokInfo . TiInfo
                  (* ^Implied NARROW, always OK. *)  
(* CHECK:          ^Is this really true? *) 
                , (* VAR *) RedEstChildRefForPredicate  
                , (* VAR *) RedEstChildKindSetForPredicate 
                )  
            ; EXIT 
            ELSE 
              LBuildStackElemRef := LBuildStackElemRef . BseLink 
            (* And loop. *) 
            END (* IF *)
          END (* LOOP *) 
        ELSE 
          RedEstChildRefForPredicate := NIL   
        ; RedEstChildKindSetForPredicate := EstHs . EstChildKindSetEmpty  
        END (* IF *) 
      END RedFindEstChildRefForPredicate  

  ; PROCEDURE RedInsTok ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { AssertionFailure } 
    (* The Fs node is an insertion token. *) 

    = BEGIN
        RedMergeForFmtNo ( FsNodeRef . FsFmtNo , IsInsTok := TRUE ) 
      ; IF ParseInfo . PiGram . IsGenerated 
        THEN 
          IF RedOldParseStackElemRef . PseDeepestBuildStackElemRef 
             = RedOldBuildStackElemRef 
          THEN 
            RedOldParseStackElemRef := RedOldParseStackElemRef . PseLink 
          END (* IF *) 
        ELSIF NOT FsNodeRef . FsIsInsideList
        THEN 
          RedOldParseStackElemRef := RedOldParseStackElemRef . PseLink 
        END (* IF *) 
      END RedInsTok 

  ; PROCEDURE RedAstStringOrEstChild 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; ChildIsPresent : BOOLEAN 
      ) 
    RAISES { AssertionFailure } 
    (* The Fs node is a string or Est child. *) 

    = VAR LChild : LbeStd . EstRootTyp 

    ; BEGIN (* RedAstStringOrEstChild *) 
(* TODO:  This is silly.  The ChildIsPresent and NOT ChildIsPresent cases
          are entirely disjoint.  Although we do have 2 calls that use a 
          variable for this parameter, it is computed in a way that would
          be more transparent with separate procedures.  Make 2 procedures. *) 
        IF ChildIsPresent 
        THEN 
(* Helps with debugging: *) 
  IF NOT RedNextBseMatches ( FsNodeRef ) 
  THEN 
    EVAL RedNextBseMatches ( FsNodeRef )
  ; RedDumpStacks ( DumpAll := TRUE ) 
  END 
; 

          Assert 
            ( RedNextBseMatches ( FsNodeRef ) 
            , AFT . A_RedAstStringOrEstChild_Mismatched_est_child 
            ) 
        ; Assert 
            ( ParseHs . RangeIsEmpty
                ( RedOldBuildStackElemRef . BseTokInfo . TiPatchTempMarkRange ) 
            , AFT . A_RedAstStringOrEstChild_UnpatchedTempMark
            ) 
        ; RedEstChildBuildStackElemRef := RedOldBuildStackElemRef 
        ; RedDoSingletonListOpt 
            ( FsNodeRef , (* VAR *) RedOldBuildStackElemRef . BseTokInfo )  
        ; RedMergeForFmtNo 
               ( FsNodeRef . FsFmtNo 
               , IsInsTok := FALSE 
               , DoOmitChild := RedIsOmittableEmptyListChild ( FsNodeRef ) 
               ) 
        ; IF ParseInfo . PiGram . IsGenerated 
          THEN (* Consume parse stack element for the child. *) 
            IF RedOldParseStackElemRef . PseDeepestBuildStackElemRef 
               = RedOldBuildStackElemRef 
            THEN 
              RedOldParseStackElemRef := RedOldParseStackElemRef . PseLink 
            END (* IF *) 
          ELSIF NOT FsNodeRef . FsIsInsideList 
          THEN  
            RedOldParseStackElemRef := RedOldParseStackElemRef . PseLink 
          END (* IF *) 
        ELSE (* Child is absent. *) 
          Assert 
            ( NOT FsNodeRef . FsIsInsideList 
            , AFT . A_RedAstStringOrEstChild_Child_of_list_is_absent 
            ) 
        ; IF NOT IntSets . IsEmpty ( RedMergeInfo . MiDeferredTempMarkSet ) 
          THEN (* Merge an EstDummyTempMarkTyp node. *)   
            LChild := NEW ( ModHs . EstDummyTempMarkTyp ) 
          ; Assertions . MessageText 
              ( "Parser inserted DummyTempMark, for RightSibFmtNo TempMark." )
          (* Dummy will have no temp marks. *) 
          ; EstBuild . MergeChild 
              ( RedMergeInfo 
              , EstRef := LChild 
              , KindSet := EstHs . EstChildKindSetDummy  
              , IsFirstOfGroup := TRUE   
              , GroupFmtNo := FsNodeRef . FsFmtNo 
              ) 
          ; RedMergeInfo . MiLMNewChildRef := LChild 
          ; NoteChildExists ( ParseInfo , RedMergeInfo , LChild )
          ELSIF ParseInfo . PiInsertNilFixedChildren 
          THEN (* Merge a NIL for the absent child. *) 
          (* NIL will have no temp marks. *) 
            EstBuild . MergeChild 
              ( RedMergeInfo 
              , EstRef := NIL  
              , KindSet := EstHs . EstChildKindSetEstChild 
              , IsFirstOfGroup := TRUE   
              , GroupFmtNo := FsNodeRef . FsFmtNo 
              ) 
          ; RedMergeInfo . MiLMNewChildRef := NIL
          ; NoteChildExists ( ParseInfo , RedMergeInfo , ChildRef := NIL )
          END (* IF *) 
        END (* IF Child is present. *) 
      END RedAstStringOrEstChild 

  ; PROCEDURE RedEvalPredicate 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; READONLY ReduceInfo : LRTable . ReduceInfoTyp 
      ) 
    : BOOLEAN 
    RAISES { AssertionFailure }
    (* PRE: FsNodeRef.FsKind = FsKindCondFmt. *)  

    = BEGIN (* RedEvalPredicate *)
        CASE FsNodeRef . FsCondPredicate . PredicateKind 

        OF PredicateKindTyp . PredicateKindNull 
        => (* This is actually an unconditional Est child node. *) 
           RETURN TRUE  

        | PredicateKindTyp . PredicateKindFalse 
        => (* These really should have been removed, but we'll be careful. *) 
           RETURN FALSE 

        | PredicateKindTyp . PredicateKindTrue
        => RETURN TRUE 

        | PredicateKindTyp . PredicateKindAbsent 
        => RETURN NOT RedEstChildForPredicateIsPresent  

        | PredicateKindTyp . PredicateKindPresent 
        => RETURN RedEstChildForPredicateIsPresent  

        | PredicateKindTyp . PredicateKindEmptyList 
        , PredicateKindTyp . PredicateKindNonpluralList 
        => IF NOT RedEstChildForPredicateIsPresent 
          THEN RETURN TRUE 
(* REVIEW: ^We are considering an absent list to also be empty here.  Odd. *) 
          ELSE (* A build stack element will be present, even if it's for an
                  empty, non-optional list that will end up omitted from the
                  about-to-be-built physical data structure. 
               *) 
            RETURN EstUtil . EvalPredicate 
                     ( ParseInfo . PiLang 
                     , FsNodeRef 
                     , EstRef := RedEstChildRefForPredicate 
                     , KindSet := RedEstChildKindSetForPredicate   
                     ) 
          END (* IF *)  

        | PredicateKindTyp . PredicateKindNonemptyList 
        , PredicateKindTyp . PredicateKindPluralList 
        , PredicateKindTyp . PredicateKindInClass 
        => IF NOT RedEstChildForPredicateIsPresent 
          THEN RETURN FALSE  
(* REVIEW: This will return FALSE for any InClass predicate applied
         to an absent child.  EstUtil.EvalPredicate can sometimes
         return TRUE for an InClass predicate on a NIL EstRef, if
         the class is LbeStd.Tok__Null or LbeStd.Tok__UniversalClass,
         for example.  What do we really want here?  Consistify this.  
*) 
          ELSE (* A build stack element will be present. *) 
            RETURN EstUtil . EvalPredicate 
                     ( ParseInfo . PiLang 
                     , FsNodeRef 
                     , EstRef := RedEstChildRefForPredicate  
                     , KindSet := RedEstChildKindSetForPredicate   
                     ) 
          END (* IF *)  

        END (* CASE *) 
      END RedEvalPredicate 

  ; PROCEDURE RedPostCheckPredicate ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
    RAISES { AssertionFailure } 
    (* Used only for generated concrete grammar. *) 

    = VAR LSliceListElemRef : ParseHs . SliceListElemRefTyp  
    ; VAR LEstChild : LbeStd . EstRootTyp 
    ; VAR LEstChildKindSet : EstHs . EstChildKindSetTyp 

    ; BEGIN 
        IF RedEstChildBuildStackElemRef = NIL 
        THEN (* The Est child was omitted altogether. *) 
          CASE FsNodeRef . FsCondPredicate . PredicateKind 

          OF PredicateKindTyp . PredicateKindNull 
          , PredicateKindTyp . PredicateKindAbsent 
          => (* OK *)  

          | PredicateKindTyp . PredicateKindEmptyList 
          , PredicateKindTyp . PredicateKindNonpluralList
          => (* OK *)  
(* FIXME: Actually, it would be better to verify here that it's omittable, 
          but that would require getting the FsNode for the Principal child.
*) 

          ELSE (* No other predicate is consistent with absent child. *) 
            CantHappen 
              ( AFT . A_PostCheckPredicate_Non_absent_predicate )  

          END (* CASE *) 
        ELSE 
          LSliceListElemRef 
            := RedEstChildBuildStackElemRef . BseTokInfo . TiInfo
          (* ^Implied NARROW, always OK. *) 
        ; FindEstChildInSliceList 
            ( LSliceListElemRef 
            , (* VAR *) LEstChild 
            , (* VAR *) LEstChildKindSet 
            ) 
        ; Assert 
            ( EstUtil . EvalPredicate 
                ( ParseInfo . PiLang 
                , FsNodeRef 
                , EstRef := LEstChild 
                , KindSet := LEstChildKindSet   
                ) 
            , AFT . A_PostCheckPredicate_Est_predicate_mismatch  
            ) 
        END (* IF *) 
      END RedPostCheckPredicate 

  ; PROCEDURE RedTraverseCondFmtNode 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; READONLY ReduceInfo : LRTable . ReduceInfoTyp 
      ) 
    RAISES { AssertionFailure } 
    (* PRE: FsNodeRef.FsKind = FsKindCondFmt. *) 
    (* PRE: FsNodeRef is the first alternative. *) 

    = PROCEDURE RedTcfRecurse ( FsNodeRef : LangUtil . FsNodeRefTyp ) 
      RAISES { AssertionFailure } 
      (* Called only within the chosen alternative. *) 

      = VAR LChildIsPresent : BOOLEAN 

      ; BEGIN 
          IF FsNodeRef # NIL 
          THEN 
            CASE FsNodeRef . FsKind 
            OF FsKindTyp . FsKindInsTok 
            => RedInsTok ( FsNodeRef ) 

            | FsKindTyp . FsKindSubtreeVert 
            , FsKindTyp . FsKindSubtreeHoriz 
            , FsKindTyp . FsKindSubtreeFill 
            , FsKindTyp . FsKindCondFmt 
              (* ^This happens at the top of recursion. *) 
            => IF FsNodeRef . FsChildren # NIL 
              THEN 
                FOR RFsChildNo := NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
                    TO 0 BY - 1 
                DO RedTcfRecurse  
                     ( FsNodeRef . FsChildren ^ [ RFsChildNo ] ) 
                END (* FOR *) 
              END (* IF *) 

            | FsKindTyp . FsKindAstString 
            , FsKindTyp . FsKindEstChildOfFixed 
            , FsKindTyp . FsKindEstChildOfList 
            => (* An Est child that is inside a condition. *) 
               IF FsNodeRef . FsEstChildIsOptional 
               THEN 
                 LChildIsPresent 
                   := FsNodeRef . FsOptionIds [ RedOptionIdSsToUse ] 
                      IN ReduceInfo . OptionIdSet  
               ELSE (* Child is required.  Always, for Ast child of list. *) 
                 LChildIsPresent := TRUE 
               END (* IF Optional *) 

            ; RedAstStringOrEstChild ( FsNodeRef , LChildIsPresent ) 
         (* ; RedOldParseStackElemRef := RedOldParseStackElemRef . PseLink *) 

            | FsKindTyp . FsKindLineBreakOpt 
            => EstBuild . PrependNl ( RedMergeInfo , IsConditional := TRUE )
            ; INC ( RedLineBreakCt ) 

            | FsKindTyp . FsKindLineBreakReqd 
            => EstBuild . PrependNl ( RedMergeInfo , IsConditional := FALSE )  
            ; INC ( RedLineBreakCt ) 

            ELSE 
              CantHappen 
                ( AFT . A_Parser_Reduce_RedTcfRecurse_Bad_Fs_Kind  
                ) 
            END (* CASE *) 
          END (* IF *) 
        END RedTcfRecurse 

    ; BEGIN (* RedTraverseCondFmtNode *) 
        VAR LFsNodeRef : LangUtil . FsNodeRefTyp 
      ; BEGIN (* Block in RedTraverseCondFmtNode *) 
          RedEstChildBuildStackElemRef := NIL 
        ; LFsNodeRef := FsNodeRef 
        ; LOOP (* Thru alternatives. *) 
            IF LFsNodeRef . FsKind # FsKindTyp . FsKindCondFmt 
            THEN (* Not a predicate.  Could still be optional. *)  
              RedTcfRecurse ( LFsNodeRef ) 
            ; EXIT 
            ELSIF RedEvalPredicate ( LFsNodeRef , ReduceInfo ) 
            THEN 
              RedTcfRecurse ( LFsNodeRef ) 
            ; IF ParseInfo . PiGram . IsGenerated 
              THEN 
                RedPostCheckPredicate ( LFsNodeRef )  
              END (* IF *) 
            ; EXIT 
            ELSE (* Failed condition, try another alternative. *) 
              LFsNodeRef := LFsNodeRef . FsCondAltRef 
            (* And loop. *) 
            END (* IF *) 
          END (* LOOP *) 
        END (* Block in RedTraverseCondFmtNode *) 
      END RedTraverseCondFmtNode 

  ; PROCEDURE RedTraverseFsTree 
      ( FsNodeRef : LangUtil . FsNodeRefTyp 
      ; READONLY ReduceInfo : LRTable . ReduceInfoTyp 
      ) 
    RAISES { AssertionFailure } 

    = VAR LFsChildNo : LangUtil . FsChildNoSignedTyp 
    ; VAR LChildIsPresent : BOOLEAN 

    ; BEGIN (* RedTraverseFsTree *) 
        CASE FsNodeRef . FsKind 
        OF FsKindTyp . FsKindEstFixedVert 
        , FsKindTyp . FsKindEstFixedHoriz 
        , FsKindTyp . FsKindEstFixedFill 
        , FsKindTyp . FsKindSubtreeVert 
        , FsKindTyp . FsKindSubtreeHoriz 
        , FsKindTyp . FsKindSubtreeFill 
        => (* Go thru Fs children, right to left. *) 
          IF FsNodeRef . FsChildren # NIL 
          THEN 
            FOR RFsChildNo := NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
                TO 0 BY - 1 
            DO RedTraverseFsTree 
                 ( FsNodeRef . FsChildren ^ [ RFsChildNo ] , ReduceInfo ) 
            END (* FOR *) 
          END (* IF *) 

        | FsKindTyp . FsKindEstListVert 
        , FsKindTyp . FsKindEstListHoriz 
        , FsKindTyp . FsKindEstListFill 
        , FsKindTyp . FsKindEstListTrailVert 
        , FsKindTyp . FsKindEstListTrailHoriz 
        , FsKindTyp . FsKindEstListTrailFill 
        => IF RedOldBuildStackElemRef = RedDeepestBuildStackElemRef 
           THEN (* Empty list *) 
           ELSE 
             IF RedNewEstNodeKind = EstHs . EstNodeKindTyp . EstNodeKindTrail
             THEN (* Start with trailing separators. *) 
               LFsChildNo := NUMBER ( FsNodeRef . FsChildren ^ ) - 1 
             ELSE (* Start with Ast child. *) 
               LFsChildNo := 0 
             END (* IF *) 
           ; LOOP (* Thru' the elements/sublists/partials. *) 
               Assert 
                 ( RedOldBuildStackElemRef # RedDeepestBuildStackElemRef 
                 , AFT . A_RedTraverseFsTree__EstList_Build_Stack_Mismatch 
                 ) 
             ; IF LFsChildNo = 0 
                  AND RedOldBuildStackElemRef . BseTokInfo . TiTok  
                      = FsNodeRef . FsSublistTok 
               THEN (* We have a sublist. *) 
                 IF ParseInfo . PiGram . IsGenerated 
                 THEN (* Consume parse stack element for the sublist. *)  
                   Assert 
                     ( RedOldParseStackElemRef # RedDeepestParseStackElemRef 
                       AND RedOldParseStackElemRef . PseTok 
                           = FsNodeRef . FsSublistTok 
                     , AFT . A_RedTraverseFsTree_Sublist_token_mismatch  
                     ) 
                 END (* IF *) 
               ; RedAstStringOrEstChild 
                   ( FsNodeRef . FsEstDescendantRef , ChildIsPresent := TRUE ) 
               ELSIF LFsChildNo = 0 
                     AND RedOldBuildStackElemRef . BseTokInfo . TiTok  
                         = FsNodeRef . FsPartialTok 
               THEN (* Partial token. Just skip everything. *) 
                 Assert
                   ( RedOldParseStackElemRef . PseTok 
                     = FsNodeRef . FsPartialTok 
                   , AFT . A_RedTraverseFsTree_Partial_token_mismatch  
                   ) 
               ; RedOldBuildStackElemRef 
                  := RedOldBuildStackElemRef . BseLink
               ; RedOldParseStackElemRef 
                  := RedOldParseStackElemRef . PseLink
               ELSE (* Principal child or separator. *) 
                 RedTraverseFsTree 
                   ( FsNodeRef . FsChildren ^ [ LFsChildNo ] , ReduceInfo ) 
               END (* IF *) 
             ; IF ParseInfo . PiGram . IsGenerated 
                  AND RedOldParseStackElemRef = RedDeepestParseStackElemRef 
               THEN 
                 Assert 
                   ( LFsChildNo = 0 
                     AND RedOldBuildStackElemRef = RedDeepestBuildStackElemRef 
                   , AFT . A_RedTraverseFsTree_Inconsistent_list_termination
                   ) 
               ; EXIT 
               ELSIF LFsChildNo = 0 
                     AND RedOldBuildStackElemRef = RedDeepestBuildStackElemRef 
               THEN 
                 EXIT 
               ELSE (* Keep looping. *)  
                 LFsChildNo 
                   := ( LFsChildNo - 1 ) 
                      MOD NUMBER ( FsNodeRef . FsChildren ^ ) 
               END (* IF *) 
             END (* LOOP *) 
           END (* IF *) 
     (* ; Assert
            ( LangUtil . TokClass 
                ( ParseInfo . PiLang , RedOldParseStackElemRef . PseTok ) 
              = LbeStd . TokClassTyp . TokClassSublist
            , AFT . A_RedTraverseFsTree_NotSublist   
            ) 
        ; RedOldParseStackElemRef 
            := RedOldParseStackElemRef . PseLink 
          (* ^Need to do this exactly once for FsList. *) 
     *) 

        | FsKindTyp . FsKindInsTok 
        => IF 
TRUE OR 
              ParseInfo . PiGram . IsGenerated 
          THEN 
            RedInsTok ( FsNodeRef ) 
          ELSIF RedOldBuildStackElemRef = RedDeepestBuildStackElemRef 
                OR RedOldBuildStackElemRef . BseTokInfo . TiTok 
                   # FsNodeRef . FsTok 
          THEN (* No matching token on the build stack. *) 
            Assert
              ( RedCondFmtMethod2 
              , AFT . A_RedTraverseFsTree_NoBuildStackTokenAndNotMethod2
              ) 
          ; RedCondFmtKnownAbsent := TRUE 
          ELSE 
            RedCondFmtKnownPresent := TRUE 
          ; RedMergeForFmtNo ( FsNodeRef . FsFmtNo , IsInsTok := TRUE ) 
          END (* IF *) 

        | FsKindTyp . FsKindAstString 
        , FsKindTyp . FsKindEstChildOfList 
        , FsKindTyp . FsKindEstChildOfFixed 
        => (* We get here only if not inside a predicate. *)  
          IF FsNodeRef . FsEstChildIsOptional 
          THEN 
            LChildIsPresent 
              := FsNodeRef . FsOptionIds [ RedOptionIdSsToUse ] 
                 IN ReduceInfo . OptionIdSet 
          ELSE 
            LChildIsPresent := TRUE 
          END (* IF *) 
        ; RedAstStringOrEstChild ( FsNodeRef , LChildIsPresent ) 

     (* ; IF NOT FsNodeRef . FsIsInsideList 
          THEN 
            RedOldParseStackElemRef := RedOldParseStackElemRef . PseLink 
          END (* IF *) 
     *) 

        | FsKindTyp . FsKindLineBreakOpt 
        => EstBuild . PrependNl ( RedMergeInfo , IsConditional := TRUE )  
        ; INC ( RedLineBreakCt ) 

        | FsKindTyp . FsKindLineBreakReqd 
        => EstBuild . PrependNl ( RedMergeInfo , IsConditional := FALSE )  
        ; INC ( RedLineBreakCt ) 

        | FsKindTyp . FsKindBegOfImage 
        => (* Just ignore it.  Nothing on either stack. *) 

        | FsKindTyp . FsKindEndOfImage 
        => RedInsTok ( FsNodeRef )  

        | FsKindTyp . FsKindCondFmt 
        => RedFindEstChildRefForPredicate ( FsNodeRef , ReduceInfo ) 

(* TODO: Could postpone computing RedEstChildRefForPredicate, etc.  until we
         know we need it.  There are a lot of cases where we don't.
*) 
        ; RedTraverseCondFmtNode ( FsNodeRef , ReduceInfo ) 

        ELSE 
          CantHappen ( AFT . A_RedTraverseFsTree_BadFsKind ) 
        END (* CASE *) 
      END RedTraverseFsTree 

  ; PROCEDURE RedWriteTraceBefore 
      ( READONLY TrialState : TrialStateTyp 
      ; READONLY ReduceInfo : LRTable . ReduceInfoTyp 
      ; Repairing : BOOLEAN 
      ) 
    RAISES { Thread . Alerted } 

    = VAR LP : ParseHs . ParseStackElemRefTyp 
    ; VAR LB : ParseHs . BuildStackElemRefTyp 

    ; <* FATAL Wr . Failure *>
      BEGIN 
        IF Options . ParseTracing  
           AND Options . TraceWrT # NIL 
           AND NOT Wr . Closed ( Options . TraceWrT ) 
        THEN 
          IF Repairing 
          THEN 
            Wr . PutText 
              ( Options . TraceWrT 
              , "(Repair) " 
              )           
          END (* IF *) 
        ; Wr . PutText 
            ( Options . TraceWrT 
            , "Reduce , production "
              & Fmt . Int ( ProdNo ) 
              & ", " 
              & LangUtil . TokImage 
                  ( ReduceInfo . LhsTok , ParseInfo . PiLang ) 
            ) 
        ; IF ReduceInfo . BuildTok # LbeStd . Tok__Null 
          THEN 
            Wr . PutText 
              ( Options . TraceWrT 
              , " BUILD " 
                & LangUtil . TokImage 
                    ( ReduceInfo . BuildTok , ParseInfo . PiLang ) 
              ) 
           END (* IF *) 
        ; Wr . PutText 
            ( Options . TraceWrT 
            , " ::= (RhsLen = "
              & Fmt . Int ( ReduceInfo . RhsLen ) 
              & "), OptionIdSet = "
              & LRTable . OptionIdSetImage ( ReduceInfo . OptionIdSet ) 
              & Wr . EOL 
            ) 

        ; Wr . PutText 
            ( Options . TraceWrT , "  ParseStack:" & Wr . EOL ) 
        ; LP := TrialState . TsParseStackTopRef 
        ; WHILE LP # NIL AND LP # RedDeepestParseStackElemRef 
          DO
            Wr . PutText ( Options . TraceWrT , "   " ) 
          ; Wr . PutText 
              ( Options . TraceWrT 
              , Fmt . Pad ( Misc . RefanyImage ( LP ) , RefanyPad ) 
              ) 
          ; Wr . PutText 
              ( Options . TraceWrT 
              , ", Tok = " 
                & LangUtil . TokImage ( LP . PseTok , ParseInfo . PiLang ) 
                & ", LRState = " 
                & Fmt . Int ( LP . PseLRState )  
                & Wr . EOL 
              ) 
          ; LP := LP ^ . PseLink 
          END (* WHILE *) 
        ; IF LP # NIL 
          THEN
            Wr . PutText ( Options . TraceWrT , "    Deeper LR state = " ) 
          ; Wr . PutText ( Options . TraceWrT , Fmt . Int ( LP . PseLRState ) )
          ; Wr . PutText ( Options . TraceWrT , Wr . EOL ) 
          END (* IF *) 

        ; Wr . PutText 
            ( Options . TraceWrT , "  BuildStack:" & Wr . EOL ) 
        ; LB := TrialState . TsBuildStackTopRef 
        ; WHILE LB # NIL AND LB # RedDeepestBuildStackElemRef 
          DO
            Wr . PutText ( Options . TraceWrT , "   " ) 
          ; Wr . PutText 
              ( Options . TraceWrT 
              , Fmt . Pad ( Misc . RefanyImage ( LB ) , RefanyPad ) 
              ) 
          ; Wr . PutText 
              ( Options . TraceWrT 
              , ", Tok = " 
                & ParseHs . TokInfoImage 
                    ( LB . BseTokInfo , ParseInfo . PiLang ) 
                & ", " 
                & Misc . BooleanImageShort ( LB . BseTokInfo . TiIsInterior )  
                & ", " 
                & Misc . BooleanImageShort ( LB . BseWasInsertedByParser )  
                & ", " 
                & Misc . BooleanImageShort ( LB . BseWasDeletedByParser )  
                & Wr . EOL 
              ) 
          ; LB := LB ^ . BseLink 
          END (* WHILE *) 

        ; Wr . Flush ( Options . TraceWrT ) 
        END (* IF *) 
      END RedWriteTraceBefore 

  ; PROCEDURE RedWriteTraceAfter 
      ( BuildStackTopRef : ParseHs . BuildStackElemRefTyp 
      ; ParseStackTopRef : ParseHs . ParseStackElemRefTyp 
      ; LookaheadTok : LbeStd . TokTyp 
      ) 
    RAISES { Thread . Alerted , AssertionFailure } 

    = VAR LAction : LbeStd . LRStateTyp  

    ; <* FATAL Wr . Failure *> 
      BEGIN 
        IF Options . ParseTracing  
           AND Options . TraceWrT # NIL 
           AND NOT Wr . Closed ( Options . TraceWrT ) 
        THEN 
          Wr . PutText ( Options . TraceWrT , "  After reduce: " )
        ; IF ParseStackTopRef # NIL 
          THEN 
            Wr . PutText ( Options . TraceWrT , "Parse top" )
          ; Wr . PutText 
              ( Options . TraceWrT 
              , Fmt . Pad 
                  ( Misc . RefanyImage ( ParseStackTopRef ) , RefanyPad ) 
              ) 
          ; Wr . PutText ( Options . TraceWrT , ", Tok " )
          ; Wr . PutText 
              ( Options . TraceWrT 
              , LangUtil . TokImage 
                  ( ParseStackTopRef . PseTok , ParseInfo . PiLang ) 
              )  
          ; Wr . PutText ( Options . TraceWrT , ", LRState " ) 
          ; Wr . PutText 
              ( Options . TraceWrT 
              , Fmt . Int ( ParseStackTopRef . PseLRState ) 
              )  
          ; Wr . PutText ( Options . TraceWrT , ", InputTok " )
          ; Wr . PutText 
              ( Options . TraceWrT 
              , LangUtil . TokImage ( LookaheadTok , ParseInfo . PiLang ) 
              )  
          ; Wr . PutText ( Options . TraceWrT , ", Action " ) 
          ; LAction  
              := LRTable . Action 
                   ( ParseInfo . PiGram 
                   , ParseStackTopRef . PseLRState 
                   , LookaheadTok 
                   ) 
          ; WriteAction ( ParseInfo , LAction ) 
          ; Wr . PutText ( Options . TraceWrT , Wr . EOL ) 
          END (* IF *) 
        ; IF BuildStackTopRef # NIL 
          THEN 
            Wr . PutText ( Options . TraceWrT , "  Build top" )
          ; Wr . PutText 
              ( Options . TraceWrT 
              , Fmt . Pad 
                  ( Misc . RefanyImage ( BuildStackTopRef ) , RefanyPad ) 
              ) 
          ; Wr . PutText ( Options . TraceWrT , ", Tok " )
          ; Wr . PutText 
              ( Options . TraceWrT 
                  , ParseHs . TokInfoImage 
                      ( BuildStackTopRef . BseTokInfo , ParseInfo . PiLang )
             ) 
          ; Wr . PutText ( Options . TraceWrT , ", " )
          ELSE 
            Wr . PutText ( Options . TraceWrT , "  Build stack is empty." )
          END (* IF *) 
        ; Wr . PutText ( Options . TraceWrT , Wr . EOL ) 
        ; Wr . Flush ( Options . TraceWrT ) 
        END (* IF *) 
      END RedWriteTraceAfter 

  ; BEGIN (* Reduce *) 
      VAR LFsKind : FsKindTyp 
    ; VAR LOldLatest : StateListElemNoTyp 
    ; VAR LFsNodeRef : LangUtil . FsNodeRefTyp 
    ; VAR LNewEstRef : EstHs . EstRefTyp 
    ; VAR LNewParseStackElemRef : ParseHs . ParseStackElemRefTyp 
    ; VAR LParseStackElemRef : ParseHs . ParseStackElemRefTyp 
    ; VAR LBuildStackElemRef : ParseHs . BuildStackElemRefTyp 
    ; VAR LNewBuildStackElemRef : ParseHs . BuildStackElemRefTyp 
    ; VAR LNewSliceListElemRef : ParseHs . SliceListElemRefTyp 
    ; VAR LReduceToPartial : BOOLEAN

    ; BEGIN (* Block Reduce body *)

      (* If this is the first reduce after a shift, advance the parse 
         state list. *) 
        IF StateList . SlStates [ StateList . SlLatest ] . TsJustShifted 
        THEN 
          LOldLatest := StateList . SlLatest 
        ; StateList . SlLatest 
            := ( StateList . SlLatest + 1 ) MOD StateListMax 
        ; IF StateList . SlOldest = StateList . SlLatest 
          THEN 
            StateList . SlOldest 
              := ( StateList . SlOldest + 1 ) MOD StateListMax 
          END (* IF *)
        ; WITH WOldTrialState = StateList . SlStates [ LOldLatest ] 
               , WNewTrialState = StateList . SlStates [ StateList . SlLatest ]
          DO WNewTrialState := WOldTrialState
          ; WNewTrialState . TsTempMarkListRef
              := ParseHs . CopyOfTempMarkList
                   ( WOldTrialState . TsTempMarkListRef )
          ; WNewTrialState . TsJustShifted := FALSE 
          END (* WITH *)
        END (* IF *) 

      (* Now do the real reduce. *) 
      ; WITH 
          WTrialState = StateList . SlStates [ StateList . SlLatest ] 
        , WReduceInfo = ParseInfo . PiGram . ReduceInfoRef ^ [ ProdNo ] 
        DO 
          LNewParseStackElemRef := NEW ( ParseHs . ParseStackElemRefTyp ) 
        ; LFsNodeRef 
            := LangUtil . FsRuleForTok 
                 ( ParseInfo . PiLang , WReduceInfo . BuildTok ) 
        ; IF LFsNodeRef = NIL 
          THEN (* No build. *) 
            IF LangUtil . TokClass 
                 ( ParseInfo . PiLang , WReduceInfo . LhsTok ) 
               = LbeStd . TokClassTyp . TokClassPartial 
            THEN (* Reducing to a list partial token. *) 
              LFsNodeRef 
                := LangUtil . FsRuleForTok 
                     ( ParseInfo . PiLang , WReduceInfo . LhsTok ) 
              (* ^The corresponding AS list token.  Will traverse this FS 
                 tree to merge the partial contributions, even though we
                 don't finish the build on this reduction. *) 
            ; Assert
                ( LFsNodeRef # NIL 
                , AFT . A_Parser_Reduce_No_Fs_tree_for_list_partial_token
                ) 
            ; LReduceToPartial := TRUE 
            ELSE (* Not a list partial token. *) 
              LReduceToPartial := FALSE 
            END (* IF *)  
          ELSE (* Will build an Est node. *) 
            LReduceToPartial := FALSE 
          END (* IF *) 

        ; IF LFsNodeRef = NIL 
          THEN (* Nothing to build.  Fiddle with parse stack only. *) 
            IF WReduceInfo . RhsLen = 0 
            THEN (* Reduce empty to some nonterminal. *) 
              LNewParseStackElemRef . PseLink 
                := WTrialState . TsParseStackTopRef 
            ; LNewParseStackElemRef . PseDeepestBuildStackElemRef 
                := WTrialState . TsBuildStackTopRef 
            (* Needed only for tracing: *) 
            ; RedDeepestParseStackElemRef 
                := WTrialState . TsParseStackTopRef 
            ; RedDeepestBuildStackElemRef 
                := WTrialState . TsBuildStackTopRef 
            ELSE (* Reduce nonempty right side. *) 
              LParseStackElemRef := WTrialState . TsParseStackTopRef 
            ; FOR I := 2 TO WReduceInfo . RhsLen 
              DO LParseStackElemRef := LParseStackElemRef . PseLink 
              END (* FOR *) 
            (* LParseStackElemRef now points to the deepest 
               stack element to be popped *) 
            ; LNewParseStackElemRef . PseLink := LParseStackElemRef . PseLink 
            ; LNewParseStackElemRef . PseDeepestBuildStackElemRef 
                := LParseStackElemRef . PseDeepestBuildStackElemRef 
            ; RedDeepestBuildStackElemRef 
                := LParseStackElemRef . PseDeepestBuildStackElemRef 
            ; RedDeepestParseStackElemRef := LParseStackElemRef . PseLink  
            END (* IF *) 
          (* Here PseLink and PseDeepestBuildStackElemRef 
             are set for the new Parse stack element, and 
             build stack is completely done. *) 
          ; RedWriteTraceBefore ( WTrialState , WReduceInfo , Repairing ) 
          ; LNewBuildStackElemRef := NIL 
            (* ^Just for call to RedWriteTraceAfter *) 

          ELSIF WReduceInfo . BuildTok = LbeStd . Tok__Empty 
          THEN (* We are reducing to an empty list child that precedes a
                  trailing list "separator".  Use the single Est node kept
                  around for this purpose. *) 
(* TODO: Eliminate this case. *) 
            LNewBuildStackElemRef := NEW ( ParseHs . BuildStackElemRefTyp ) 
          ; LNewBuildStackElemRef . BseLink := WTrialState . TsBuildStackTopRef
          ; WTrialState . TsBuildStackTopRef := LNewBuildStackElemRef 
          ; LNewSliceListElemRef := NEW ( ParseHs . SliceListElemRefTyp ) 
          ; LNewSliceListElemRef . SlePredLink := NIL 
          ; LNewSliceListElemRef . SleNodeRef 
              := EstHs . UniqueEstNodeTrailingSep  
          ; LNewSliceListElemRef . SleIsSlice := FALSE 
          ; LNewSliceListElemRef . SleKindSet 
              := ( EstHs . UniqueEstNodeTrailingSep . EstChildKindSet 
                   * EstHs . EstChildKindSetCopyUp 
                 ) 
                 + EstHs . EstChildKindSetTrailingSep
          ; WITH WTokInfo = LNewBuildStackElemRef . BseTokInfo 
            DO 
              WTokInfo . TiIsInterior := TRUE  
            ; WTokInfo . TiTok := WReduceInfo . BuildTok 
            ; WTokInfo . TiInfo := LNewSliceListElemRef 
            ; WTokInfo . TiSliceListRMRoot := LNewSliceListElemRef 
            ; WTokInfo . TiPatchTempMarkRange := ParseHs . TempMarkRangeNull 
            END (* WITH WTokInfo *) 
          ; LNewParseStackElemRef . PseLink 
              := WTrialState . TsParseStackTopRef 
          ; LNewParseStackElemRef . PseDeepestBuildStackElemRef 
              := LNewBuildStackElemRef 
          (* Needed only for tracing: *) 
          ; RedDeepestParseStackElemRef 
              := WTrialState . TsParseStackTopRef 
          ; RedDeepestBuildStackElemRef 
              := WTrialState . TsBuildStackTopRef 

          ELSE (* Must traverse Fs tree, and maybe build an Est node. *) 
            LFsKind := LFsNodeRef . FsKind 
          ; Assert 
              ( LFsKind IN LangUtil . FsKindSetEstInterior 
              , AFT . A_Parse_Reduce_Bad_Fs_kind 
              ) 
          ; RedInsertionRepairIsWithin := FALSE 
          ; RedTempMarkToSs := 0 
          ; RedLineBreakCt := 0 
          ; RedDoMarkNewTree := FALSE 
          ; IF LFsKind IN LangUtil . FsKindSetEstListTrail
               AND LRTable . OptionIdForListTrail  IN WReduceInfo . OptionIdSet 
            THEN RedNewEstNodeKind := EstHs . EstNodeKindTyp . EstNodeKindTrail
            ELSE RedNewEstNodeKind := EstHs . EstNodeKindTyp . EstNodeKindPlain
            END (* IF *) 

          ; IF WReduceInfo . RhsLen = 0 
            THEN (* Build for empty right side. *)
              RedDeepestParseStackElemRef 
                := WTrialState . TsParseStackTopRef  
            ; RedDeepestBuildStackElemRef 
                := WTrialState . TsBuildStackTopRef  
            ELSE 
              LParseStackElemRef := WTrialState . TsParseStackTopRef 
            ; FOR I := 2 TO WReduceInfo . RhsLen 
              DO LParseStackElemRef := LParseStackElemRef . PseLink 
              END (* FOR *) 
            (* LParseStackElemRef now points to the deepest 
               stack element to be popped *) 
            ; RedDeepestParseStackElemRef := LParseStackElemRef . PseLink 
            ; RedDeepestBuildStackElemRef 
                := LParseStackElemRef . PseDeepestBuildStackElemRef 
            END (* IF *) 
          ; RedWriteTraceBefore ( WTrialState , WReduceInfo , Repairing ) 
          ; LNewParseStackElemRef . PseLink := RedDeepestParseStackElemRef 
          ; LNewParseStackElemRef . PseDeepestBuildStackElemRef 
              := RedDeepestBuildStackElemRef 

          (* Even for an empty list node, go ahead and build it and push 
             onto the build stack.  Later, when it is reduced into its 
             parent, we might eliminate it altogether, but not necessarily.  
             Meantime, it will allow parent conditional format predicates
             to be evaluated on it. 
          *)
          
          ; IF LangUtil . TokClass 
                 ( ParseInfo . PiLang 
                 , WTrialState . TsParseStackTopRef . PseTok 
                 ) 
               = LbeStd . TokClassTyp . TokClassPartial 
            THEN (* RM token of RHS is a list partial token.  We already 
                    started a merge in a previous reduction to it.  Fetch 
                    its MergeInfo for use in the current reduction. *)  
              Assert 
                ( WTrialState . TsBuildStackTopRef . BseTokInfo . TiTok 
                  = WTrialState . TsParseStackTopRef . PseTok 
                , AFT . A_Parse_Reduce_Build_stack_partial_token_mismatch 
                ) 
            ; RedMergeInfo 
                := WTrialState . TsBuildStackTopRef . BseTokInfo . TiInfo 
              (* ^Implied NARROW, always OK. *) 
            ELSE (* Start a new merge. *) 
              RedMergeInfo := NEW ( MergeInfoTyp ) 
            ; InitMergeInfo ( WTrialState , RedMergeInfo ) 
            ; EVAL EstBuild . InitMergeState 
                     ( RedMergeInfo  
                     , ParseInfo . PiLang 
                     , LFsNodeRef . FsTok 
                     , EstRefToInheritFrom := NIL 
                     ) 
            END (* IF *) 

          ; LNewBuildStackElemRef := NEW ( ParseHs . BuildStackElemRefTyp ) 
          ; RedModDelRef := NIL 
          ; LNewBuildStackElemRef . BseLink := RedDeepestBuildStackElemRef 
          ; RedOldParseStackElemRef := WTrialState . TsParseStackTopRef 
          ; RedOldBuildStackElemRef := WTrialState . TsBuildStackTopRef 
          ; RedEstChildBuildStackElemRef := NIL 
          ; IF ParseInfo . PiGram . IsGenerated 
            THEN
              (* RedOptionIdSsToUse := TRUE *) 
              RedOptionIdSsToUse := FALSE
(* This is still experimental.  For a generated grammar, we are now using the 
   OptionId mechanism only to ascertain absence/presence.  The OptionIds 
   set for handwritten grammars are assigned this way, so we use them even
   for a generated grammar. *) 
            ELSE RedOptionIdSsToUse := FALSE 
            END (* IF *) 
          ; RedTraverseFsTree ( LFsNodeRef , WReduceInfo ) 
          ; INC ( ParseInfo . PiLineCtIncr , RedLineBreakCt ) 
            (* ^ParseTrv counts every line break it traverses thru', 
                regardless of whether it is actually taken, so we count
                them the same way here. *) 
          ; IF ParseInfo . PiGram . IsGenerated 
            THEN 
              Assert 
                ( RedOldParseStackElemRef = RedDeepestParseStackElemRef 
                , AFT . A_Reduce_ParseStackMismatch 
                ) 
            END (* IF *) 

          ; Assert 
              ( RedOldBuildStackElemRef = RedDeepestBuildStackElemRef 
              , AFT . A_Reduce_BuildStackSegmentMismatch 
              ) 
          ; WITH WTokInfo = LNewBuildStackElemRef . BseTokInfo 
            DO 
              IF LReduceToPartial
              THEN (* We have started a merge, but must wait to a later
                      reduction to finish it.  Attach the MergeInfo to the
                      new Bse, where we can get it at that time. *) 
                WTokInfo . TiIsInterior := FALSE  
(* REVIEW: ^This means repair actions can't dismantle partially merged
           sublists.  This could be bad for error recovery. OTOH, we will
           only reduce to list partial tokens after a list has been shifted 
           and found complete and error-free.  Then a series of list
           reductions will follow, building the entire list.  Perhaps there
           is no chance of syntax error once this process begins. 
*) 
              ; WTokInfo . TiTok := WReduceInfo . LhsTok 
              ; WTokInfo . TiInfo := RedMergeInfo  
              ; WTokInfo . TiSliceListRMRoot := NIL  
              ; WTokInfo . TiPatchTempMarkRange := ParseHs . TempMarkRangeNull 
(* REVIEW: ^Is this right? *) 
              ELSE (* Done with this merge. *) 
                EstBuild . FinishMerge 
                  ( RedMergeInfo 
                  , ResultEstNodeKind := RedNewEstNodeKind  
                  , (* VAR *) ResultTreeRef := LNewEstRef 
                  ) 
              ; FinishMergeInfo 
                  ( (* READONLY *) ParseInfo , RedMergeInfo , LNewEstRef ) 
              ; LNewSliceListElemRef := NEW ( ParseHs . SliceListElemRefTyp ) 
              ; LNewSliceListElemRef . SlePredLink := NIL 
              ; LNewSliceListElemRef . SleNodeRef := LNewEstRef 
              ; LNewSliceListElemRef . SleIsSlice := FALSE 
              ; LNewSliceListElemRef . SleKindSet 
                  := ( LNewEstRef . EstChildKindSet
                       * EstHs . EstChildKindSetCopyUp 
                     ) 
                     + EstHs . EstChildKindSetEstChildNonNIL 
              ; IF RedInsertionRepairIsWithin 
                THEN 
                  LNewSliceListElemRef . SleKindSet 
                    := LNewSliceListElemRef . SleKindSet 
                       + EstHs . EstChildKindSetContainsInsertionRepair  
                END (* IF *) 
              ; WTokInfo . TiIsInterior := TRUE  
              ; WTokInfo . TiTok := WReduceInfo . BuildTok 
              ; WTokInfo . TiInfo := LNewSliceListElemRef 
              ; WTokInfo . TiSliceListRMRoot := LNewSliceListElemRef 
              ; WTokInfo . TiPatchTempMarkRange := ParseHs . TempMarkRangeNull 
              END (* IF *)
            ; WTokInfo . TiIsInsertionRepair := FALSE 
            END (* WITH WTokInfo *) 
          ; LNewBuildStackElemRef . BseWasInsertedByParser := FALSE 
          ; LNewBuildStackElemRef . BseWasDeletedByParser := FALSE 
          ; WTrialState . TsBuildStackTopRef := LNewBuildStackElemRef 
          END (* IF traversing Fs tree. *) 
        ; LNewParseStackElemRef . PseTok := WReduceInfo . LhsTok 
        ; IF LNewParseStackElemRef . PseLink # NIL 
          THEN 
            LNewParseStackElemRef . PseLRState 
              := LRTable . Action 
                   ( ParseInfo . PiGram 
                   , LNewParseStackElemRef . PseLink . PseLRState 
                   , LNewParseStackElemRef . PseTok 
                   ) 
          ELSE 
            LNewParseStackElemRef . PseLRState 
              := ParseInfo . PiGram . StartStateNo 
            (* The right value here is uncertain, but it is dead. *) 
          END (* IF *) 
        ; WTrialState . TsParseStackTopRef := LNewParseStackElemRef 
        ; RedWriteTraceAfter 
            ( LNewBuildStackElemRef 
            , LNewParseStackElemRef 
            , WTrialState . TsParseTravStateRef . PtsTokInfo . TiTok 
            ) 
        END (* WITH WTrialState *) 
      END (* Reduce body block *) 
    END Reduce 

; <* UNUSED *> VAR GBuildTok : LbeStd . TokTyp := LbeStd . Tok__Null 
      (* ^Set this with a debugger. *)  

; PROCEDURE LRMachine 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; VAR StateList : StateListTyp 
    ; MaxParseCheck : LbeStd . LimitedTokCtTyp 
      (* ^Caller wants no more tokens shifted than MaxParseCheck. 
          Can be LbeStd . ParseCheckInfinity, which means unlimited. *) 
    ; VAR ActualParseCheck : LbeStd . LimitedTokCtTyp 
      (* ^Upon return, ActualParseCheck = MaxParseCheck implies the 
          last operation was a shift, which means neither an error 
          was detected, nor was input accepted. *) 
    ; VAR ErrCode : LbeStd . ErrCodeTyp 
      (* ^LbeStd . ErrCodeNull, if no error detected. *) 
    ; VAR Accepted : BOOLEAN 
      (* ^Upon return, an error was detected iff 
          ActualParseCheck < MaxParseCheck AND NOT Accepted. *) 
    ; Repairing : BOOLEAN 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

  = VAR LParseCheck : LbeStd . LimitedTokCtTyp 
  ; VAR LLRState : LbeStd . LRStateTyp 
  ; VAR LTokInfo : ParseHs . TokInfoTyp 
  ; VAR LTokInfo2 : ParseHs . TokInfoTyp 
  ; VAR LAction : LbeStd . LRStateTyp 
  ; VAR LAction2 : LbeStd . LRStateTyp 
  ; VAR LParseTravState2 : ParseHs . ParseTravStateRefTyp 

  ; BEGIN (* LRMachine *) 
      LParseCheck := 0 
    ; ErrCode := LbeStd . ErrCodeNull 
    ; Accepted := FALSE 
    ; LTokInfo 
        := StateList . SlStates [ StateList . SlLatest ] . TsParseTravStateRef
           . PtsTokInfo 
    ; LLRState 
        := StateList . SlStates 
             [ StateList . SlLatest ] 
           . TsParseStackTopRef . PseLRState 
    ; LAction 
        := LRTable . Action 
             ( ParseInfo . PiGram , LLRState , LTokInfo . TiTok ) 
    ; LOOP 
        IF Thread . TestAlert ( ) THEN RAISE Thread . Alerted END 
      ; IF LAction = ParseInfo . PiGram . AcceptAction 
        THEN 
          Accepted := TRUE 
        ; EXIT 
        ELSIF LAction = LRTable . StateNoNull 
        THEN (* No legal action. *)  
          WITH 
            WTrialState 
            = StateList . SlStates [ StateList . SlLatest ] 
          DO IF LTokInfo . TiIsInterior
             THEN (* An Ast NT is next.  Try a lower subtree *)
               WTrialState . TsParseTravStateRef 
                 := NextParseTravState 
                      ( ParseInfo 
                      , WTrialState . TsParseTravStateRef 
                      , ParseTrv . SuccKindTyp . SuccKindDescend 
                      , LLRState  
                      , Comment := "Ast gave error" 
                      ) 
             ; StateList . SlLastShiftedAstStates [ StateList . SlLatest ] 
                 := GNullTrialState 
             ; LTokInfo := WTrialState . TsParseTravStateRef . PtsTokInfo 
             ; LAction 
                 := LRTable . Action 
                      ( ParseInfo . PiGram , LLRState , LTokInfo . TiTok ) 
             ELSE 
               WITH WLastShift 
                    = StateList . SlLastShiftedAstStates 
                        [ StateList . SlLatest ] 
               DO IF WLastShift . TsParseStackTopRef # NIL 
                 THEN (* This was just a deferred error after shifting an Ast
                         NT.  Restore to before that shift and descend. *) 
                   WTrialState := WLastShift 
                 ; WTrialState . TsParseTravStateRef 
                     := NextParseTravState 
                          ( ParseInfo 
                          , WTrialState . TsParseTravStateRef 
                          , ParseTrv . SuccKindTyp . SuccKindDescend 
                          , LLRState 
                          , Comment := "Ast would give error"
                          ) 
                 ; WLastShift := GNullTrialState 
                 ; LLRState 
                     := WTrialState . TsParseStackTopRef . PseLRState 
                 ; LTokInfo 
                     := WTrialState . TsParseTravStateRef . PtsTokInfo 
                 ; LAction 
                     := LRTable . Action 
                          ( ParseInfo . PiGram , LLRState , LTokInfo . TiTok )
                 ELSE (* real parsing error *) 
                   ErrCode := LLRState 
(* TODO: Do something about meaningful error codes. *) 
                 ; EXIT 
                 END (* IF *) 
               END (* WITH *) 
             END (* IF *) 
          END (* WITH WTrialState *) 
        ELSIF LAction >= ParseInfo . PiGram . FirstReduceAction 
        THEN (* Reduce only. *) 
          Reduce 
            ( ParseInfo 
            , LAction - ParseInfo . PiGram . FirstReduceAction 
            , StateList 
            , Repairing  
            ) 
        ; LLRState 
            := StateList . SlStates 
                 [ StateList . SlLatest ] 
               . TsParseStackTopRef . PseLRState 
        ; LAction 
            := LRTable . Action 
                 ( ParseInfo . PiGram , LLRState , LTokInfo . TiTok ) 
        ELSE (* Shift or shift-reduce. *)  
          WITH WTrialState 
               = StateList . SlStates [ StateList . SlLatest ] 
          DO 
            IF LTokInfo . TiIsInterior 
            THEN (* Ast NT. See if we can shift it without error in the 
                    next step. *) 
              Assert   
                ( LAction < ParseInfo . PiGram . FirstReadRedAction 
                , AFT . A_LRMachine_ShiftReduceOfNonterminal
                ) 
; IF LAction = 401 
  THEN 
    Assertions . DoNothing ( ) 
  END 
            ; LParseTravState2 (* Trial state after the shift. *)  
                := NextParseTravState 
                     ( ParseInfo 
                     , WTrialState . TsParseTravStateRef 
                     , ParseTrv . SuccKindTyp . SuccKindAdvance 
                     , LRState := LAction 
                     , Comment := "Checking Ast" 
                     ) 
            ; LTokInfo2 := LParseTravState2 . PtsTokInfo 
            ; LAction2 (* Action after shifting the Ast NT. *) 
                := LRTable . Action 
                     ( ParseInfo . PiGram , LAction , LTokInfo2 . TiTok ) 
            ; IF LAction2 = LRTable . StateNoNull 
              THEN (* Shifting the NT would lead to an error.
                      Instead, descend. *) 
                WTrialState . TsParseTravStateRef 
                  := NextParseTravState 
                       ( ParseInfo 
                       , WTrialState . TsParseTravStateRef 
                       , ParseTrv . SuccKindTyp . SuccKindDescend 
                       , LLRState 
                       , Comment := "Ast would give error"
                       ) 
              ; StateList . SlLastShiftedAstStates [ StateList . SlLatest ] 
                  := GNullTrialState 
              ; LTokInfo 
                  := WTrialState . TsParseTravStateRef . PtsTokInfo 
              ; LAction 
                  := LRTable . Action 
                       ( ParseInfo . PiGram , LLRState , LTokInfo . TiTok )
              ELSE (* Go ahead and really shift the Ast NT. *)  
                Shift 
                  ( ParseInfo 
                  , LTokInfo 
                  , ShiftLRState := LAction 
                    (* See comment on Shift call below. *) 
                  , TrialState := WTrialState 
                  , LastShifted 
                      := StateList . SlLastShiftedAstStates 
                           [ StateList . SlLatest ]
                  , Delete := FALSE 
                  ) 
              (* Now consume the shifted nonterminal input token. *) 
              ; WTrialState . TsParseTravStateRef := LParseTravState2
              ; LbeStd . IncLimitedTokCt 
                  ( LParseCheck , LTokInfo . TiSyntTokCt ) 
              ; IF MaxParseCheck < LbeStd . ParseCheckInfinity 
                   AND LParseCheck >= MaxParseCheck 
                THEN 
                  EXIT 
                END (* IF *) 
              ; LTokInfo := LTokInfo2 
              ; LLRState := LAction
              ; LAction := LAction2  
              END (* IF *) 
            ELSE (* Terminal.  Unconditionally shift. *) 
              Shift 
                ( ParseInfo 
                , LTokInfo 
                , ShiftLRState := LAction 
                  (* If this is a shift-reduce action, this value will not 
                     be the right state to enter after the shift, but it 
                     will be ignored when the reduce is done later in this 
                     procedure, without using the parse tables in between. *) 
                , TrialState := WTrialState 
                , LastShifted 
                    := StateList . SlLastShiftedAstStates 
                         [ StateList . SlLatest ]
                , Delete := FALSE 
                ) 
            (* Now consume the input token. *) 
            ; WTrialState . TsParseTravStateRef 
                := NextParseTravState 
                     ( ParseInfo 
                     , WTrialState . TsParseTravStateRef 
                     , ParseTrv . SuccKindTyp . SuccKindAdvance 
                     , LRState := LAction 
                     , Comment := "Terminal shifted" 
                     )
            ; LbeStd . IncLimitedTokCt 
                ( LParseCheck , LTokInfo . TiSyntTokCt ) 
            ; LTokInfo 
                := WTrialState . TsParseTravStateRef . PtsTokInfo 
            ; IF LAction >= ParseInfo . PiGram . FirstReadRedAction 
              THEN (* Shift-reduce, and we already did the shift. *) 
                Reduce 
                  ( ParseInfo 
                  , LAction - ParseInfo . PiGram . FirstReadRedAction 
                  , StateList 
                  , Repairing  
                  ) 
              END (* IF *) 
            (* WTrialState is now invalid. *) 
            ; IF MaxParseCheck < LbeStd . ParseCheckInfinity 
                 AND LParseCheck >= MaxParseCheck 
              THEN 
                EXIT
              ELSE  
                LLRState 
                  := StateList . SlStates [ StateList . SlLatest ] 
                     . TsParseStackTopRef . PseLRState 
              ; LAction 
                  := LRTable . Action 
                       ( ParseInfo . PiGram , LLRState , LTokInfo . TiTok ) 
              END (* IF *) 
            END (* IF *) 
          END (* WITH WTrialState *) 
        END (* IF *) 
      END (* LOOP *) 
    ; ActualParseCheck := LParseCheck 
    END LRMachine 

; PROCEDURE RepairCostSum 
    ( Left , Right : LbeStd . RepairCostTyp ) : LbeStd . RepairCostTyp 

  = VAR LSum : PortTypes . Card32Typ 

  ; BEGIN (* RepairCostSum *) 
      IF Left = LbeStd . RepairCostInfinity 
         OR Right = LbeStd . RepairCostInfinity 
      THEN 
        RETURN LbeStd . RepairCostInfinity 
      ELSE 
        LSum := Left + Right 
      ; IF LSum >= LbeStd . RepairCostInfinity 
        THEN 
          RETURN LbeStd . RepairCostInfinity 
        ELSE 
          RETURN LSum 
        END (* IF *) 
      END (* IF *) 
    END RepairCostSum 

; PROCEDURE Repair 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; READONLY ErrorStateList : StateListTyp 
    ; VAR StateList : StateListTyp 
    ; ErrCode : LbeStd . ErrCodeTyp 
    ; VAR Accepted : BOOLEAN 
    ) 
  RAISES { AssertionFailure , Thread . Alerted }  

  = VAR RepBestCost : LbeStd . RepairCostTyp 
  ; VAR RepBestAccepted : BOOLEAN 
  ; VAR RepBestStateList : StateListTyp 

  ; PROCEDURE RepBuildTokInsertionRepairSlice 
      ( Tok : LbeStd . TokTyp 
      ; VAR TokInfo : ParseHs . TokInfoTyp 
      ) 

    = VAR LSliceListElemRef : ParseHs . SliceListElemRefTyp 
    ; VAR LStringRef : SharedStrings . T 

    ; BEGIN (* RepBuildTokInsertionRepairSlice *) 
        IF LangUtil . TokClass ( ParseInfo . PiLang , Tok ) 
           IN LbeStd . TokClassSetEstSentential 
        THEN 
          LStringRef 
            := LangUtil . DisplayStringForTok ( ParseInfo . PiLang , Tok )
        ; LSliceListElemRef := NEW ( ParseHs . SliceListElemRefTyp ) 
        ; LSliceListElemRef ^ . SlePredLink := NIL 
        ; LSliceListElemRef ^ . SleNodeRef := LStringRef 
        ; LSliceListElemRef ^ . SleIsSlice := FALSE 
        ; LSliceListElemRef ^ . SleKindSet 
            := EstHs . EstChildKindSetRepairPlaceholder  
        ; TokInfo . TiInfo := LSliceListElemRef 
        ; TokInfo . TiSliceListRMRoot := LSliceListElemRef 
          (* ^Just to simplify debugging. *)  
        ELSE 
          TokInfo . TiInfo := NIL 
        ; TokInfo . TiSliceListRMRoot := NIL 
        END (* IF *) 
      ; TokInfo . TiTok := Tok 
      ; TokInfo . TiIsInterior := FALSE 
      ; TokInfo . TiIsInsertionRepair := TRUE  
      ; TokInfo . TiSyntTokCt := 1 
      ; TokInfo . TiFullTempMarkRange := ParseHs . TempMarkRangeEmpty 
      ; TokInfo . TiPatchTempMarkRange := ParseHs . TempMarkRangeNull 
      END RepBuildTokInsertionRepairSlice 

  ; PROCEDURE RepContinuationCost ( ContTok : LbeStd . TokTyp ) 
    : LbeStd . RepairCostTyp 

    = VAR LCost : LbeStd . RepairCostTyp 

    ; BEGIN 
        IF ContTok < ParseInfo . PiGram . FirstTerminal  
           OR ParseInfo . PiGram . InsertionCostRef = NIL 
           OR ContTok - ParseInfo . PiGram . FirstTerminal 
              > LAST ( ParseInfo . PiGram . InsertionCostRef ^ ) 
        THEN 
          LCost := 1  
        ELSE 
          LCost 
            := ParseInfo . PiGram . InsertionCostRef ^ 
                 [ ContTok - ParseInfo . PiGram . FirstTerminal ] 
        END (* IF *) 
      ; RETURN LCost 
      END RepContinuationCost 

  ; PROCEDURE RepBuildContinuation 
      ( VAR (* IN OUT *) StateList : StateListTyp 
      ; VAR (* IN OUT *) Cost : LbeStd . RepairCostTyp 
      ; VAR Tok : LbeStd . TokTyp 
      ; VAR Success : BOOLEAN 
            (* NOT Success if either can't extend continuation or if 
                   doing so would give >= best cost. 
               Success => one token has been shifted in StateList *) 
      ) 
    RAISES { AssertionFailure , Thread . Alerted } 

    (* Add a continuation token by doing continuation reductions 
       and one continuation shift, Adjusting StateList. 
       Increment Cost by the insertion cost of the shifted token. 
       When Cost exceeds RepBestCost, quit and return NOT Success. *) 

    = VAR LLRState : LbeStd . LRStateTyp 
    ; VAR LAction : LbeStd . LRStateTyp 
    ; VAR LContTok : LbeStd . TokTyp 
    ; VAR LContTokInfo : ParseHs . TokInfoTyp 
    ; VAR LCost : LbeStd . RepairCostTyp  

    ; BEGIN (* RepBuildContinuation *) 
        LLRState 
          := StateList . SlStates 
               [ StateList . SlLatest ] 
             . TsParseStackTopRef . PseLRState 
      ; LContTok := LRTable . Continuation ( ParseInfo . PiGram , LLRState )
      ; IF LContTok < ParseInfo . PiGram . FirstTerminal  
           OR ParseInfo . PiGram . InsertionCostRef = NIL 
           OR LContTok - ParseInfo . PiGram . FirstTerminal 
              > LAST ( ParseInfo . PiGram . InsertionCostRef ^ ) 
(* TODO: Investigate how we can get an LRState of zero? *) 
        THEN 
          Success := FALSE 
        ELSE 
          LCost 
            := ParseInfo . PiGram . InsertionCostRef ^ 
                 [ LContTok - ParseInfo . PiGram . FirstTerminal ]
        ; IF WriteTrials
          THEN 
            WriteParseTraceText 
              ( RepairId ( ParseInfo ) 
                & "Try continuation Tok "
                & LangUtil . TokImage ( LContTok , ParseInfo . PiLang ) 
                & " at insertion cost " 
                & LbeStd . RepairCostImage ( LCost ) 
                & Wr . EOL  
              )
          END (* IF *)
        ; LOOP (* Parsing actions through accept or one shift. *) 
            LAction 
              := LRTable . Action ( ParseInfo . PiGram , LLRState , LContTok ) 
          ; IF LAction = ParseInfo . PiGram . AcceptAction 
            THEN (* We never allow a continuation-inserted token to accept. 
                    That would require the inserted token to be EndOfImage. 
                    But we want exactly one of those, and it is already in 
                    the input stream. *) 
              Success := FALSE 
            ; EXIT 
            ELSIF LAction < ParseInfo . PiGram . FirstReadRedAction 
            THEN (* This will be our one shift action. *)  
              Cost := RepairCostSum ( Cost , LCost ) 
            ; Success := Cost < RepBestCost 
            ; IF Success 
              THEN 
                RepBuildTokInsertionRepairSlice ( LContTok , LContTokInfo ) 
              ; Assert 
                  ( NOT LContTokInfo . TiIsInterior 
                  , AFT . A_RepBuildContinuation_Inserting_interior_Ast_node 
                  ) 
              ; Shift 
                  ( ParseInfo 
                  , LContTokInfo 
                  , LAction 
                  , StateList . SlStates [ StateList . SlLatest ] 
                  , StateList . SlLastShiftedAstStates [ StateList . SlLatest ]
                  , Delete := TRUE 
                  ) 
              (* There can be no TempMarks in an inserted continuation token. *)
              ; WriteParseTraceText 
                  ( RepairId ( ParseInfo ) 
                    & "Continuation Tok "
                    & LangUtil . TokImage ( LContTok , ParseInfo . PiLang ) 
                    & " succeeded at total cost " 
                    & LbeStd . RepairCostImage ( Cost ) 
                    & " < best cost " 
                    & LbeStd . RepairCostImage ( RepBestCost ) 
                    & Wr . EOL  
                  )  
              END (* IF *) 
            ; EXIT 
            ELSIF LAction >= ParseInfo . PiGram . FirstReduceAction 
            THEN 
              Reduce 
                ( ParseInfo 
                , LAction - ParseInfo . PiGram . FirstReduceAction 
                , StateList 
                , Repairing := TRUE  
                ) 
            ; LLRState 
                := StateList . SlStates 
                     [ StateList . SlLatest ] 
                   . TsParseStackTopRef . PseLRState 
            ; LContTok 
                := LRTable . Continuation ( ParseInfo . PiGram , LLRState )
            ; LCost 
                := ParseInfo . PiGram . InsertionCostRef ^ 
                     [ LContTok - ParseInfo . PiGram . FirstTerminal ] 
            (* And loop for more parsing actions. *) 
            ELSE 
              CantHappen ( AFT . A_RepBuildContinuationBadActionKind ) 
            END (* IF *) 
          END (* LOOP *) 
        ; Tok := LContTok 
        END (* IF *) 
      END RepBuildContinuation 

  ; PROCEDURE RepConstructAntideletion 
      ( VAR (* IN OUT *) TokInfo : ParseHs . TokInfoTyp 
      ; VAR (* IN OUT *) TrialState : TrialStateTyp 
      ) 
    RAISES { AssertionFailure } 
    (* Construct antideletion insertion mod *) 

    = VAR LMergeInfo : MergeInfoTyp 
    ; VAR LNewEstRef : EstHs . EstRefTyp 
    ; VAR LSliceListElemRef : ParseHs . SliceListElemRefTyp 
    ; VAR LNewBuildStackElemRef : ParseHs . BuildStackElemRefTyp
    ; VAR LTempMarkRange : ParseHs . TempMarkRangeTyp 
    ; VAR LDoRepatch : BOOLEAN 

    ; BEGIN (* RepConstructAntideletion *) 
        IF TokInfo . TiIsInsertionRepair  
           (* Go ahead and delete without creating a ModTok (i.e. a proposed 
              deletion), a repair that was inserted in a previous parse. 
              NOTE: At the time of coding, ParseTrv doesn't give these to
                    Parser anyway, but this is defensive. 
              NOTE: If this could happen, we would have to do something
                    about temp marks in the deleted token. 
           *) 
        THEN 
        ELSE 
          LMergeInfo := NEW ( MergeInfoTyp ) 
        ; InitMergeInfo ( TrialState , (* VAR *) LMergeInfo ) 
        ; EVAL EstBuild . InitMergeState 
                 ( LMergeInfo  
                 , ParseInfo . PiLang 
                 , LangUtil . VarTermModTok 
                     ( ParseInfo . PiLang , TokInfo . TiTok ) 
                 , EstRefToInheritFrom := NIL 
                 ) 
        
        (* It would be cleaner to add EstChildKindContainsDeletionRepair to
           the Est child of the ModTok node (this could only be a string),
           but it could involve taking apart a slice and differentiating
           some K-tree nodes.  Instead, we make traversers propagate the 
           property down from the ModTok node. 
        *)
        ; LDoRepatch
            := NOT ParseHs . RangeIsEmpty ( TokInfo . TiPatchTempMarkRange )
        ; IF LDoRepatch
          THEN 
            Assert
              ( LangUtil . TokClass ( ParseInfo . PiLang , TokInfo . TiTok )
                = LbeStd . TokClassTyp . TokClassConstTerm 
              , AFT . A_RepConstructAntideletion_patch_not_ins_tok 
              )
          ; LTempMarkRange := TokInfo . TiPatchTempMarkRange
          ; TokInfo . TiPatchTempMarkRange := ParseHs . TempMarkRangeEmpty
            (* ^Don't let MergeSliceList do its usual FmtNo patching. *)
          END (* IF *) 
        ; MergeSliceList 
            ( ParseInfo 
            , LMergeInfo 
            , TokInfo 
            , FmtNo := EstHs . FmtNoModTok 
            , IsFirstOfGroup := TRUE 
            ) 
        ; EstBuild . FinishMerge 
            ( LMergeInfo 
            , ResultEstNodeKind := EstHs . EstNodeKindTyp . EstNodeKindModTok 
            , (* VAR *) ResultTreeRef := LNewEstRef 
            )

        ; IF LDoRepatch 
          THEN
            Assert
              ( IntSets . IsEmpty ( LMergeInfo . MiDeferredTempMarkSet )
              , AFT . A_RepConstructAntideletion_deferred_temp_marks
              ) 
            (* Change FmtNo marks to Plain, denoting the explicit ModTok. *)
          ; PatchTempMarkRangeFmtNos
              ( TrialState . TsTempMarkListRef
              , LTempMarkRange 
              , EstHs . FmtNoModTok
              ) 
          ; PatchTempMarkRangeKindsAndEstRefs
              ( TrialState . TsTempMarkListRef
              , LTempMarkRange 
              , MarkKindTyp . Plain
              , LNewEstRef
              ) 
        ; LNewEstRef . EstChildKindSet
            := LNewEstRef . EstChildKindSet
               + EstHs . EstChildKindSetContainsTempMark
          END (* IF *) 

        ; FinishMergeInfo 
            ( (* READONLY *) ParseInfo , LMergeInfo , LNewEstRef )  
        ; LSliceListElemRef := NEW ( ParseHs . SliceListElemRefTyp ) 
        ; LSliceListElemRef ^ . SleNodeRef := LNewEstRef 
        ; LSliceListElemRef ^ . SlePredLink := NIL 
        ; LSliceListElemRef ^ . SleIsSlice := FALSE 
        ; LSliceListElemRef ^ . SleKindSet 
            := ( LNewEstRef . EstChildKindSet * EstHs . EstChildKindSetCopyUp )
               + EstHs . EstChildKindSetModTok 
        ; LNewBuildStackElemRef := NEW ( ParseHs . BuildStackElemRefTyp ) 
        ; LNewBuildStackElemRef . BseLink 
            := TrialState . TsWaitingBuildStackElemRef 
        ; LNewBuildStackElemRef . BseTokInfo := TokInfo 
        ; LNewBuildStackElemRef . BseTokInfo . TiInfo := LSliceListElemRef 
        ; LNewBuildStackElemRef . BseTokInfo . TiSliceListRMRoot 
            := LSliceListElemRef 
          (* ^Just to simplify debugging. *) 
        ; LNewBuildStackElemRef . BseWasDeletedByParser := TRUE 
        ; LNewBuildStackElemRef . BseWasInsertedByParser := FALSE 
        ; TrialState . TsWaitingBuildStackElemRef 
            := LNewBuildStackElemRef 
        END (* IF TiIsInsertionRepair ELSE *) 
      END RepConstructAntideletion 

  ; PROCEDURE RepRepairsAtPoint 
      ( READONLY StateList : StateListTyp 
      ; ExtraParseCheck : LbeStd . LimitedTokCtTyp 
      ) 
    RAISES { AssertionFailure , Thread . Alerted } 

    = VAR LInsertionNo : PortTypes . Int32Typ 
    ; VAR LInsertionCt : PortTypes . Int32Typ 
    ; VAR LCostOfDeletions : LbeStd . RepairCostTyp 
    ; VAR LCost : LbeStd . RepairCostTyp 
    ; VAR LExpectedParseCheck : LbeStd . LimitedTokCtTyp 
    ; VAR LActualParseCheck : LbeStd . LimitedTokCtTyp 
    ; VAR LErrCode : LbeStd . ErrCodeTyp 
    ; VAR LAccepted : BOOLEAN 
    ; VAR LSuccess : BOOLEAN 
    ; VAR LAction : LbeStd . LRStateTyp 
    ; VAR LTokInfo : ParseHs . TokInfoTyp 
    ; VAR LStateList : StateListTyp 
    ; VAR LStateListAfterDeletions : StateListTyp 
    ; VAR LStateListAfterContinuation : StateListTyp 
    ; VAR LContTok : LbeStd . TokTyp 
    ; VAR LContLength : INTEGER 

    ; BEGIN (* RepRepairsAtPoint *) 
        LCostOfDeletions := 0 
      ; LStateListAfterDeletions := StateList 
        (* ^Start with no deletions. *) 
      ; LExpectedParseCheck 
          := LbeStd . LimitedTokCtSum 
               ( Options . ParseCheckMax , ExtraParseCheck ) 
      ; LOOP (* Thru deletions, starting with none. *) 
        (* Try single token insertions *) 
          LInsertionNo := 0  
        ; LInsertionCt 
            := NUMBER ( ParseInfo . PiGram . AscendingInsertionCostRef ^ ) 
        ; LOOP (* Thru single tokens to insert *) 
            LAccepted := FALSE 
          ; IF (* TRUE OR *) LInsertionNo >= LInsertionCt 
(* CHECK: This loop was once commented out for some reason. Is it now fixed? *) 
            THEN 
              EXIT (* Single tokens. *) 
            END (* IF *) 
          ; LTokInfo . TiTok 
              := ParseInfo . PiGram . AscendingInsertionCostRef ^ 
                   [ LInsertionNo ] 
          ; LCost 
              := ParseInfo . PiGram . InsertionCostRef ^ 
                   [ LTokInfo . TiTok - ParseInfo . PiGram . FirstTerminal ] 
          ; LCost := RepairCostSum ( LCost , LCostOfDeletions ) 
          ; IF LCost >= RepBestCost 
            THEN (* We already have as good a repair as this would be. *)  
              EXIT (* Single tokens. *) 
            END (* IF *) 
          ; INC ( ParseInfo . PiAttemptedRepairActionCt ) 
          ; IF WriteTrials
            THEN
              WriteParseTraceText 
                ( RepairId ( ParseInfo ) 
                  & "Try inserting single Tok "
                  & ParseHs . TokInfoImage
                      ( LTokInfo , ParseInfo . PiLang ) 
                  & " at cost " 
                  & LbeStd . RepairCostImage ( LCost ) 
                  & Wr . EOL  
                )
            END (* IF *) 
          ; LStateList := LStateListAfterDeletions 
          ; LOOP (* Thru parsing actions that are possible when the proposed
                    to-be-inserted token is treated as the lookahead. 
                 *) 
              LAction 
                := LRTable . Action 
                     ( ParseInfo . PiGram 
                     , LStateList . SlStates [ LStateList . SlLatest ] 
                       . TsParseStackTopRef . PseLRState 
                     , LTokInfo . TiTok 
                     ) 
            ; IF LAction = ParseInfo . PiGram . AcceptAction 
              THEN (* This can't happen, because we never insert EOI. *) 
                CantHappen 
                  ( AFT . A_RepRepairsAtPointAcceptOnRepairInsertion ) 
              ; LAccepted := FALSE 
              ; LActualParseCheck := 0 
              ; EXIT (* parsing actions loop *) 
              ELSIF LAction = LRTable . StateNoNull 
              THEN (* Error *) 
                LAccepted := FALSE 
              ; LActualParseCheck := 0 
              ; EXIT (* parsing actions loop *) 
              ELSIF LAction >= ParseInfo . PiGram . FirstReduceAction 
              THEN 
                Reduce 
                  ( ParseInfo 
                  , LAction - ParseInfo . PiGram . FirstReduceAction 
                  , LStateList 
                  , Repairing := TRUE  
                  ) 
              ; LAction 
                  := LRTable . Action 
                       ( ParseInfo . PiGram 
                       , LStateList . SlStates [ LStateList . SlLatest ] 
                         . TsParseStackTopRef . PseLRState 
                       , LTokInfo . TiTok 
                       ) 
              (* And go around to try this new parse action. *) 
              ELSE (* Shift or shift-and-reduce *) 
                RepBuildTokInsertionRepairSlice ( LTokInfo . TiTok , LTokInfo )
              ; Assert 
                  ( NOT LTokInfo . TiIsInterior 
                  , AFT . A_RepRepairsAtPoint_Inserting_interior_Ast_node 
                  ) 
              ; Shift 
                  ( ParseInfo 
                  , LTokInfo 
                  , LAction 
                  , LStateList . SlStates [ LStateList . SlLatest ] 
                  , LStateList . SlLastShiftedAstStates [ StateList . SlLatest ]
                  , Delete := TRUE 
                  ) 
              (* There can be no TempMarks in an inserted repair token. *)
              ; IF LAction >= ParseInfo . PiGram . FirstReadRedAction 
                THEN (* Shift-and-reduce action. *) 
                  Reduce 
                    ( ParseInfo 
                    , LAction - ParseInfo . PiGram . FirstReadRedAction 
                    , LStateList 
                    , Repairing := TRUE  
                    ) 
                END (* IF *) 

              (* We have now shifted the inserted single token, so go 
                 back to parsing from the input stream. *) 
              ; LRMachine 
                  ( ParseInfo 
                  , LStateList 
                  , LExpectedParseCheck 
                  , LActualParseCheck 
                  , LErrCode 
                  , LAccepted
                  , Repairing := TRUE  
                  ) 
              ; IF LAccepted OR LActualParseCheck > 0 
                THEN (* This repair parsed a real input token. *) 
                  LCost 
                    := RepairCostSum 
                         ( LCost 
                         , LRTable . ShortParseCheckCost 
                             ( ParseInfo . PiGram 
                             , LExpectedParseCheck 
                             , LActualParseCheck 
                             , LAccepted 
                             ) 
                         ) 
                ; IF LAccepted > RepBestAccepted 
                     OR ( LAccepted = RepBestAccepted AND LCost < RepBestCost )
                  THEN (* This a cheaper repair than previously found. *) 
                    WriteParseTraceText 
                      ( RepairId ( ParseInfo ) 
                        & "Single Tok "
                        & ParseHs . TokInfoImage 
                            ( LTokInfo , ParseInfo . PiLang ) 
                        & " succeeded at total cost " 
                        & LbeStd . RepairCostImage ( LCost ) 
                        & " < best cost " 
                        & LbeStd . RepairCostImage ( RepBestCost ) 
                        & " with parse check " 
                        & LbeStd . LimitedTokCtImage ( LActualParseCheck ) 
                        & ", Accept = " 
                        & Misc . BooleanImageShort ( LAccepted ) 
                        & Wr . EOL  
                      )  
                  ; RepBestCost := LCost 
                  ; RepBestStateList := LStateList 
                  ; RepBestAccepted := LAccepted 
                  ELSE 
                  END (* IF *) 
                END (* IF *) 
              ; EXIT (* parsing actions loop *) 
              END (* IF *) 
            END (* LOOP parsing actions *) 
          ; IF LAccepted 
            THEN (* There can never be a cheaper accept.  For more trips 
                    around this loop, the deletion cost will be the same, 
                    the insertion cost will be at least as high, and the 
                    parse check will be no longer, since this accept used 
                    all of the remaining input, thus the short parse cost 
                    will be the same or higher. *) 
              EXIT (* Single token insertions *) 
            ELSIF LActualParseCheck >= LExpectedParseCheck 
            THEN (* Similarly, we can never get a cheaper repair in this 
                    case by choosing a different insertion token. *) 
              EXIT (* Single token insertions *) 
            ELSE 
              INC ( LInsertionNo ) 
            (* And loop to try a different single token insertion. *) 
            END (* IF *) 
          END (* LOOP single tokens to insert *) 

        (* Next, try continuation prefixes *) 
        ; LContLength := 0 
        ; IF NOT LAccepted 
          THEN (* No continuation extension can be better than an 
                  existing accept. *) 
            LStateListAfterContinuation := LStateListAfterDeletions 
          ; LCost := LCostOfDeletions 
          ; INC ( ParseInfo . PiAttemptedRepairActionCt ) 
          ; INC ( LContLength ) 
          ; RepBuildContinuation 
              ( (* IN OUT *) LStateListAfterContinuation 
              , (* IN OUT *) LCost 
              , (* VAR*) LContTok 
              , (* VAR*) LSuccess 
              ) 
            (* Since the first token of the continuation will have 
               been  tried or eliminated as one of the single token 
               insertions, must build continuation to length of 2 
               to get a new repair candidate. *) 
          ; IF LSuccess 
            THEN 
              LOOP (* Thru increasing length continuation prefixes *) 
                INC ( ParseInfo . PiAttemptedRepairActionCt ) 
              ; INC ( LContLength ) 
              ; IF LContLength > 1000
                THEN 
                  Assertions . MessageText 
                    ( "Terminating too-long continuation. " )  
                ; EXIT
                END (* IF *) 
(* FIXME: Get continuation repair working so we don't need this exit. *) 
              ; RepBuildContinuation 
                  ( (* IN OUT *) LStateListAfterContinuation 
                  , (* IN OUT *) LCost 
                  , (* VAR*) LContTok 
                  , (* VAR*) LSuccess 
                  ) 
              ; IF LSuccess 
                THEN 
                  INC ( ParseInfo . PiAttemptedRepairActionCt ) 
                ; LStateList := LStateListAfterContinuation 
                ; LRMachine 
                    ( ParseInfo 
                    , LStateList 
                    , LExpectedParseCheck 
                    , LActualParseCheck 
                    , LErrCode 
                    , LAccepted 
                    , Repairing := TRUE  
                    ) 
                ; IF LAccepted OR LActualParseCheck > 0 
                  THEN (* This is an acceptable repair *) 
                    LCost 
                      := RepairCostSum 
                           ( LCost 
                           , LRTable . ShortParseCheckCost 
                               ( ParseInfo . PiGram 
                               , LExpectedParseCheck 
                               , LActualParseCheck 
                               , LAccepted 
                               ) 
                           ) 
                  ; IF LAccepted > RepBestAccepted 
                       OR ( LAccepted = RepBestAccepted 
                            AND LCost < RepBestCost 
                          ) 
                    THEN (* This a cheaper repair. *) 
                      WriteParseTraceText 
                        ( RepairId ( ParseInfo ) 
                          & "Continuation Tok "
                          & LangUtil . TokImage
                              ( LContTok , ParseInfo . PiLang ) 
                          & " succeeded at total cost " 
                         & LbeStd . RepairCostImage ( LCost ) 
                          & " < best cost " 
                          & LbeStd . RepairCostImage ( RepBestCost ) 
                          & " with parse check " 
                          & LbeStd . LimitedTokCtImage ( LActualParseCheck ) 
                          & ", Accept = " 
                          & Misc . BooleanImageShort ( LAccepted ) 
                          & Wr . EOL  
                        )  
                    ; RepBestCost := LCost 
                    ; RepBestStateList := LStateList 
                    ; RepBestAccepted := LAccepted 
                    END (* IF *) 
                  END (* IF *) 
                ; IF LAccepted 
                  THEN (* There can never be a cheaper accept by lengthening 
                          the continuation.  The deletion cost will be the 
                          same, the insertion cost will be at least as high, 
                          and the parse check will be no longer, since this 
                          accept used all of the remaining input, thus the 
                          short parse cost will be the same or higher. 
                       *) 
                    EXIT (* Continuation insertions *) 
                  ELSIF LActualParseCheck >= LExpectedParseCheck 
                  THEN (* Similarly, in this case, we can never get a cheaper 
                          repair by extending the continuation. *) 
                    EXIT (* Continuation prefixes. *) 
                  END (* IF *) 
                ELSE (* Could not extend continuation. *) 
                  EXIT (* Continuation prefixes. *) 
                END (* IF *) 
              END (* LOOP *) 
            END (* IF *) 
          END (* IF *) 

        (* Now go on to the next deletion *) 
        ; WITH WStateAfterDeletions 
               = LStateListAfterDeletions . SlStates 
                   [ LStateListAfterDeletions . SlLatest ] 
          DO 
          (* Don't delete a whole Est subtree. Descend to a single token. *)  
            LOOP 
              LTokInfo 
                := WStateAfterDeletions . TsParseTravStateRef . PtsTokInfo 
            ; IF LTokInfo . TiIsInterior 
              THEN (* Interior node, try a lower subtree *) 
                WStateAfterDeletions . TsParseTravStateRef 
                  := NextParseTravState 
                       ( ParseInfo 
                       , WStateAfterDeletions . TsParseTravStateRef 
                       , ParseTrv . SuccKindTyp . SuccKindDescend 
                       , WStateAfterDeletions . TsParseStackTopRef 
                         . PseLRState 
                       , Comment := "Deletion repair, descend" 
                       ) 
              ; LStateListAfterDeletions . SlLastShiftedAstStates 
                  [ LStateListAfterDeletions . SlLatest ] 
                  := GNullTrialState 
              ELSE EXIT
              END (* IF *) 
            END (* LOOP *) 
          ; IF LTokInfo . TiTok = LbeStd . Tok__EndOfImage 
            THEN
              WriteParseTraceText ( "Never delete EOI" & Wr . EOL )  
            ; EXIT 
            ELSE 
              LCostOfDeletions 
                := RepairCostSum 
                     ( LCostOfDeletions 
                     , ParseInfo . PiGram . DeletionCostRef ^ 
                         [ LTokInfo . TiTok 
                           - ParseInfo . PiGram . FirstTerminal 
                         ] 
                     ) 
            ; IF RepBestCost <= LCostOfDeletions 
              THEN
                WriteParseTraceText 
                  ( "Deleting Tok "
                    & ParseHs . TokInfoImage 
                        ( LTokInfo , ParseInfo . PiLang ) 
                    & " would cost " 
                    & LbeStd . RepairCostImage ( LCostOfDeletions ) 
                    & ", no better than best cost of " 
                    & LbeStd . RepairCostImage ( RepBestCost ) 
                    & Wr . EOL  
                  )  
              ; EXIT (* From loop thru deletions *) 
              ELSE 
                INC ( ParseInfo . PiAttemptedRepairActionCt )
              ; IF WriteTrials
                THEN 
                  WriteParseTraceText 
                    ( RepairId ( ParseInfo ) 
                      & "Try deleting Tok " 
                      & ParseHs . TokInfoImage 
                          ( LTokInfo , ParseInfo . PiLang ) 
                      & " at deletions-only cost " 
                      & LbeStd . RepairCostImage ( LCostOfDeletions ) 
                      & Wr . EOL  
                    )
                END (* IF *) 
              ; RepConstructAntideletion 
                  ( (* IN OUT *) LTokInfo , (* IN OUT *) WStateAfterDeletions )
              ; WStateAfterDeletions . TsParseTravStateRef 
                  := NextParseTravState 
                       ( ParseInfo 
                       , WStateAfterDeletions . TsParseTravStateRef 
                       , ParseTrv . SuccKindTyp . SuccKindAdvance 
                       , WStateAfterDeletions . TsParseStackTopRef 
                         . PseLRState 
                       , Comment := "Successor to deleted token" 
                       ) (* Consume. *) 
              (* Try this deletion without any insertions *) 
              ; LStateList := LStateListAfterDeletions 
              ; LCost := LCostOfDeletions 
              ; LRMachine 
                  ( ParseInfo 
                  , LStateList 
                  , LExpectedParseCheck 
                  , LActualParseCheck 
                  , LErrCode 
                  , LAccepted 
                  , Repairing := TRUE  
                  ) 
              ; IF LAccepted OR LActualParseCheck > 0 
                THEN (* This is an acceptable repair *) 
                  LCost 
                    := RepairCostSum 
                         ( LCost 
                         , LRTable . ShortParseCheckCost 
                             ( ParseInfo . PiGram 
                             , LExpectedParseCheck 
                             , LActualParseCheck 
                             , LAccepted 
                             ) 
                         ) 
                ; IF LAccepted > RepBestAccepted 
                     OR ( LAccepted = RepBestAccepted AND LCost < RepBestCost )
                  THEN (* This a cheaper repair. *) 
                    WriteParseTraceText 
                      ( RepairId ( ParseInfo ) 
                        & "Deleting Tok " 
                        & ParseHs . TokInfoImage 
                            ( LTokInfo , ParseInfo . PiLang ) 
                        & " succeeded at total parse cost " 
                        & LbeStd . RepairCostImage ( LCost ) 
                        & " better than previous best cost of " 
                        & LbeStd . RepairCostImage ( RepBestCost ) 
                        & ", Accept = " 
                        & Misc . BooleanImageShort ( LAccepted ) 
                        & Wr . EOL  
                      ) 
                  ; RepBestCost := LCost 
                  ; RepBestStateList := LStateList 
                  ; RepBestAccepted := LAccepted 
                  END (* IF *) 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END (* WITH WStateAfterDeletions *) 
        END (* LOOP thru deletions *) 
      END RepRepairsAtPoint 

  ; BEGIN (* Repair *) 
      VAR LStateListSs : StateListElemNoTyp 
    ; VAR LRepairPointCt : PortTypes . Card16Typ 
    ; VAR LExtraParseCheck : LbeStd . LimitedTokCtTyp 
    ; VAR LParseStackTopRef : ParseHs . ParseStackElemRefTyp 
    ; VAR LBuildStackElemRef : ParseHs . BuildStackElemRefTyp 
    ; VAR LDeepestBuildStackElemRef : ParseHs . BuildStackElemRefTyp 
    ; VAR LParseTravStateRef : ParseHs . ParseTravStateRefTyp 
    ; VAR LStartStateList : StateListTyp 
    ; VAR LNewSliceListElemRef : ParseHs . SliceListElemRefTyp 
    ; VAR LNewBuildStackElemRef : ParseHs . BuildStackElemRefTyp 

    ; BEGIN (* Block for Repair  *) 
        INC ( ParseInfo . PiAttemptedRepairCt ) 
      ; RepBestCost := LbeStd . RepairCostInfinity 
      ; RepBestAccepted := FALSE 
      ; LRepairPointCt := 0 
      ; LStateListSs := ErrorStateList . SlLatest 
        (* Build a syntactic error. *) 
      ; LNewSliceListElemRef := NEW ( ParseHs . SliceListElemRefTyp ) 
      ; LNewBuildStackElemRef := NEW ( ParseHs . BuildStackElemRefTyp ) 
      ; LNewBuildStackElemRef . BseLink 
          := ErrorStateList . SlStates [ LStateListSs ] 
             . TsWaitingBuildStackElemRef   
      ; LNewBuildStackElemRef . BseWasDeletedByParser := TRUE 
      ; LNewBuildStackElemRef . BseWasInsertedByParser := FALSE 
      ; WITH WTokInfo = LNewBuildStackElemRef . BseTokInfo 
        DO WTokInfo . TiTok := LbeStd . Tok__Null 
        ; WTokInfo . TiPatchTempMarkRange := ParseHs . TempMarkRangeNull 
        ; WTokInfo . TiIsInsertionRepair := FALSE 
        ; WTokInfo . TiInfo := LNewSliceListElemRef 
        ; WTokInfo . TiSliceListRMRoot := LNewSliceListElemRef 
          (* ^Just to simplify debugging. *) 
        END (* WITH WTokInfo *) 
      ; LNewSliceListElemRef . SlePredLink := NIL 
      ; LNewSliceListElemRef . SleIsSlice := FALSE 
      ; LNewSliceListElemRef . SleKindSet 
          := EstHs . EstChildKindSetModSyntErr 
      ; LNewSliceListElemRef . SleNodeRef 
          := NEW ( ModHs . ModSyntErrTyp , ModSyntErrCode := ErrCode ) 
      ; LOOP (* Thru repairs at points in state list *) 
          LStartStateList . SlLatest := 0 
        ; LStartStateList . SlOldest := 0 
        ; LStartStateList . SlStates [ 0 ] 
            := ErrorStateList . SlStates [ LStateListSs ] 
        ; LStartStateList . SlStates [ 0 ] . TsWaitingBuildStackElemRef
            := LNewBuildStackElemRef 
        ; RepRepairsAtPoint 
            ( (* READONLY *) LStartStateList , LRepairPointCt ) 
        ; INC ( LRepairPointCt ) 
        ; IF LStateListSs = ErrorStateList . SlOldest 
          THEN 
            EXIT 
          ELSE 
            LStateListSs := ( LStateListSs - 1 ) MOD StateListMax 
          END (* IF *) 
        END (* LOOP *) 
      (* Now back down the (reduced) stack *) 
      ; WITH WTrialState = ErrorStateList . SlStates [ LStateListSs ] 
        DO LParseStackTopRef := WTrialState . TsParseStackTopRef 
        ; LBuildStackElemRef := WTrialState . TsBuildStackTopRef 
        ; LParseTravStateRef := WTrialState . TsParseTravStateRef 
        END (* WITH WTrialState *) 
      ; LExtraParseCheck := LRepairPointCt 
      ; WHILE LParseStackTopRef # NIL 
              AND LRepairPointCt < Options . SimpleRepairPointCt 
        DO IF LBuildStackElemRef # NIL 
           THEN 
             LDeepestBuildStackElemRef 
               := LParseStackTopRef . PseDeepestBuildStackElemRef 
           ; LOOP (* Prepend all the build stack tokens back on 
                     the front of the input stream. *) 
               LParseTravStateRef 
                 := ParseTrv . PrependToken 
                      ( ParseInfo 
                      , LParseTravStateRef 
                      , LBuildStackElemRef . BseTokInfo 
                      ) 
             ; LBuildStackElemRef := LBuildStackElemRef . BseLink 
             ; IF LBuildStackElemRef = LDeepestBuildStackElemRef 
               THEN 
                 EXIT 
               END (* IF *) 
             ; LbeStd . IncLimitedTokCt ( LExtraParseCheck ) 
             END (* LOOP *) 
           END (* IF *) 
        ; LParseStackTopRef := LParseStackTopRef . PseLink 
        ; LStartStateList . SlLatest := 0 
        ; LStartStateList . SlOldest := 0 
        ; WITH WTrialState = LStartStateList . SlStates [ 0 ] 
          DO WTrialState . TsParseStackTopRef := LParseStackTopRef 
          ; WTrialState . TsBuildStackTopRef := LBuildStackElemRef 
          ; WTrialState . TsParseTravStateRef := LParseTravStateRef 
          END (* WITH WTrialState *) 
        ; RepRepairsAtPoint ( LStartStateList , LExtraParseCheck ) 
        ; INC ( LRepairPointCt ) 
        END (* WHILE *) 
      ; Assert 
          ( RepBestCost < LbeStd . RepairCostInfinity 
          , AFT . A_RepairNoRepairFound 
          ) 
      ; StateList := RepBestStateList 
      ; Accepted := RepBestAccepted 
      END (* Block  Repair body block *) 
    END Repair 

; VAR GSingletonListCt : CARDINAL := 0 
; VAR GSingletonListOptCt : CARDINAL := 0 

(* VISIBLE: *) 
; PROCEDURE Parse 
    ( VAR ParseInfo : ParseHs . ParseInfoTyp 
    ; ParseTravStateRef : ParseHs . ParseTravStateRefTyp
    ; VAR NewTreeRef : LbeStd . EstRootTyp 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

  = VAR LActualParseCheck : LbeStd . LimitedTokCtTyp 
  ; VAR LErrCode : LbeStd . ErrCodeTyp 
  ; VAR LParseStackElemRef : ParseHs . ParseStackElemRefTyp 
  ; VAR LStateList : StateListTyp 
  ; VAR LAccepted : BOOLEAN 

  ; BEGIN (* Parse *) 
      WriteTraceParseBegin ( ) 
    ; GSingletonListCt := 0 
    ; GSingletonListOptCt := 0 
    ; ParseInfo . PiAttemptedRepairActionCt := 0 
    ; ParseInfo . PiAttemptedRepairCt := 0 
    ; LStateList . SlOldest := 0 
    ; LStateList . SlLatest := 0 
    ; WITH WTrialState = LStateList . SlStates [ LStateList . SlLatest ] 
      DO 
        LParseStackElemRef := NEW ( ParseHs . ParseStackElemRefTyp ) 
      ; WTrialState . TsParseStackTopRef := LParseStackElemRef 
      ; LParseStackElemRef . PseLink := NIL 
      ; LParseStackElemRef . PseTok := LbeStd . Tok__Null (* Irrelevant. *) 
      ; LParseStackElemRef . PseLRState := ParseInfo . PiGram . StartStateNo 
      ; LParseStackElemRef . PseDeepestBuildStackElemRef := NIL 
      ; WTrialState . TsBuildStackTopRef := NIL 
      ; WTrialState . TsWaitingBuildStackElemRef := NIL 
      ; WTrialState . TsParseTravStateRef (* For 1st token. *) 
          := NextParseTravState 
               ( ParseInfo 
               , ParseTravStateRef (* For BOI. *) 
               , ParseTrv . SuccKindTyp . SuccKindAdvance 
               , LParseStackElemRef . PseLRState 
               , Comment := "Initial"
               ) 
      ; WTrialState . TsJustShifted := TRUE 
      ; WTrialState . TsTempMarkListRef := ParseInfo . PiTravTempMarkListRef 
      END (* WITH WTrialState *) 
    ; LStateList . SlLastShiftedAstStates [ LStateList . SlLatest ] 
        := GNullTrialState 
    ; LOOP 
        LRMachine 
          ( ParseInfo 
          , LStateList 
          , LbeStd . ParseCheckInfinity 
          , LActualParseCheck 
          , LErrCode 
          , LAccepted 
          , Repairing := FALSE   
          ) 
      ; IF LAccepted 
        THEN 
          EXIT 
        ELSE (* error *) 
          Repair 
            ( (* VAR *) ParseInfo 
            , (* READONLY *) LStateList 
(* TODO:      ^v Same actual, these two modes, is either dangerous or, 
                at best, silly. *) 
            , (* VAR *) LStateList 
            , LErrCode 
            , (* VAR *) LAccepted 
            ) 
        ; IF LAccepted
          THEN
            EXIT
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    ; WITH WTrialState = LStateList . SlStates [ LStateList . SlLatest ] 
      DO 
        Shift 
          ( ParseInfo 
          , WTrialState . TsParseTravStateRef . PtsTokInfo 
          , ShiftLRState := ParseInfo . PiGram . AcceptAction 
          , TrialState := WTrialState 
          , LastShifted 
              := LStateList . SlLastShiftedAstStates [ LStateList . SlLatest ]
          , Delete := FALSE 
          ) 
      END (* WITH *)
    ; Reduce 
        ( ParseInfo 
        , LRTable . AcceptProdNo ( ParseInfo . PiGram ) 
        , LStateList 
        , Repairing := FALSE   
        ) 
    ; WITH WTrialState = LStateList . SlStates [ LStateList . SlLatest ] 
      DO Assert 
           ( WTrialState . TsParseStackTopRef 
             . PseDeepestBuildStackElemRef 
             = NIL 
           , AFT . A_Parse_ExtraBuildStackSegmentUponAccept 
           ) 
      ; Assert 
          ( WTrialState . TsParseStackTopRef . PseTok 
            = LbeStd . Tok__Augment 
          , AFT . A_Parse_Not_augment_upon_accept 
          ) 
      ; Assert 
          ( WTrialState . TsParseStackTopRef ^ . PseLink ^ . PseLink 
            = NIL 
          , AFT . A_Parse_Extra_parse_stack_elements_upon_accept 
          ) 
      ; WITH 
          WSliceListElemRef 
          = NARROW 
              ( WTrialState . TsBuildStackTopRef . BseTokInfo . TiInfo  
              , ParseHs . SliceListElemRefTyp 
              ) 
        DO
          Assert  
            ( WSliceListElemRef . SlePredLink = NIL 
            , AFT . A_Parse_Extra_slice_list_element 
            ) 
        ; Assert  
            ( NOT WSliceListElemRef . SleIsSlice 
            , AFT . A_Parse_Slice 
            ) 
        ; NewTreeRef := WSliceListElemRef . SleNodeRef 
        ; ParseInfo . PiFinalTempMarkListRef := WTrialState . TsTempMarkListRef
        END (* WITH WSliceListElemRef *) 
      END (* WITH WTrialState *) 
    ; WriteTraceParseEnd ( ) 
    ; Assertions . MessageText
        ( Fmt . Int ( GSingletonListOptCt ) 
          & "/" 
          & Fmt . Int ( GSingletonListCt ) 
          & " Singleton lists optimized."
        )  
    END Parse 

; BEGIN (* Parser *) 
  END Parser 
. 
