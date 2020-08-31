(*  Name         :Priya Palanivelu
 *  Assignment   :11
 *  Instructor   :DR.Rodney Bates
 *  Course       :CS 810
 *  Login        :pxpalani

 *)

MODULE Set
;

  IMPORT IO
;

  REVEAL T
    = Public BRANDED OBJECT

        Elements : REF ARRAY OF ElemTyp
      ;

      Used : INTEGER
      ;

      Size : INTEGER
      ;

      END
;

  PROCEDURE InIt ( SetPassed : T ; SizeHint : INTEGER ) : T

  = BEGIN

      SetPassed . Used := 0
    ;

    SetPassed . Elements := NEW ( REF ARRAY OF ElemTyp , SizeHint )
    ;

    SetPassed . Size := SizeHint
    ;

    RETURN SetPassed
    ;

    END InIt
;


  PROCEDURE InsertElem ( SetPassed : T ; Value : ElemTyp ) : BOOLEAN

  = VAR flag := FALSE
  ;

    VAR i : INTEGER := 0
  ;

    VAR j : INTEGER := 0
  ;

    VAR k : INTEGER := 0
  ;

    VAR oldsize : INTEGER := 0
  ;

    newarray : REF ARRAY OF ElemTyp
  ;

    BEGIN

      IF ( NOT ( IsIn ( SetPassed , Value ) ) )
      THEN

        IF ( SetPassed . Used + 1 <= SetPassed . Size )

        THEN

          IF SetPassed . Used # 0
          THEN

            WHILE ( i < SetPassed . Used )
            DO

              IF Value . compare ( SetPassed . Elements [ i ] , Value )
                 = CompareTyp . Greater

              THEN

                EXIT
              ;

              ELSE

                i := i + 1
              ;

              END

            END
          ;

          FOR j
            := SetPassed . Used
            TO i
            BY - 1
            DO

              SetPassed . Elements [ j ] := SetPassed . Elements [ j - 1 ]
            ;

            END
          ;

            SetPassed . Elements [ i ] := Value
          ;

          SetPassed . Used := SetPassed . Used + 1
          ;

          flag := TRUE
          ;

          ELSE

            SetPassed . Elements [ 0 ] := Value
          ;

          SetPassed . Used := SetPassed . Used + 1
          ;

          flag := TRUE
          ;

          END

        ELSE

          oldsize := SetPassed . Size
        ;

        SetPassed . Size := oldsize * 2
        ;

        newarray := NEW ( REF ARRAY OF ElemTyp , SetPassed . Size )
        ;

        WHILE (   k < SetPassed . Used )
          DO

            newarray [ k ] := SetPassed . Elements [ k ]
          ;

          k   := k + 1
          ;

          END
        ;

        SetPassed . Elements := newarray
        ;

        WHILE (   i < SetPassed . Used )
          DO

            IF Value . compare ( SetPassed . Elements [ i ] , Value )
               = CompareTyp . Greater
            THEN

              EXIT

            ELSE

              i := i + 1
            ;

            END

          END
        ;

        j   := SetPassed . Used
        ;

        WHILE (   j >= i )
          DO

            SetPassed . Elements [ j ] := SetPassed . Elements [ j - 1 ]
          ;

          j   := j - 1
          ;

          END
        ;

          SetPassed . Elements [ i ] := Value
        ;

        SetPassed . Used := SetPassed . Used + 1
        ;

        flag := TRUE
        ;

        END

      ELSE

        flag := FALSE
      ;

      END
    ;

    RETURN flag
    ;

    END InsertElem
;

  PROCEDURE IsIn ( SetPassed : T ; Value : ElemTyp ) : BOOLEAN

  = VAR high : INTEGER := SetPassed . Used - 1
  ;

    VAR low : INTEGER := 0
  ;

    VAR mid : INTEGER := 0
  ;

    BEGIN

      IF SetPassed . Used # 0
      THEN

        WHILE high >= low
        DO

          mid := ( high + low ) DIV 2
        ;

        IF   Value . compare ( SetPassed . Elements [ mid ] , Value )
             = CompareTyp . Equal

          THEN
            RETURN TRUE
          ;

          ELSE

            IF Value . compare ( SetPassed . Elements [ mid ] , Value )
               = CompareTyp . Less

            THEN

              low := mid + 1
            ;

            ELSE

              high := mid - 1
            ;

            END

          END

        END

      END
    ;

    RETURN FALSE
    ;

    END IsIn
;


  PROCEDURE RemoveElem ( SetPassed : T ; Value : ElemTyp ) : BOOLEAN

  = VAR high : INTEGER := SetPassed . Used - 1
  ;


    VAR low : INTEGER := 0
  ;

    VAR mid : INTEGER := 0
  ;

    VAR i : INTEGER := 0
  ;

    VAR flag : BOOLEAN := FALSE
  ;



    BEGIN

      WHILE high >= low
      DO


        mid := ( high + low ) DIV 2
      ;

      IF   Value . compare ( SetPassed . Elements [ mid ] , Value )
           = CompareTyp . Equal
        THEN

          flag := TRUE
        ;

        EXIT
        ;

        ELSE

          IF Value . compare ( SetPassed . Elements [ mid ] , Value )
             = CompareTyp . Less
          THEN

            low := mid + 1
          ;

          ELSE

            high := mid - 1
          ;

          END

        END

      END
    ;

    IF   flag = TRUE
      THEN

        i := mid
      ;

      WHILE (   i < SetPassed . Used )
        DO

          SetPassed . Elements [ i ] := SetPassed . Elements [ i + 1 ]
        ;

        i   := i + 1
        ;

        END
      ;

      SetPassed . Used := SetPassed . Used - 1
      ;

      END
    ;

    RETURN flag
    ;

    END RemoveElem
;


  PROCEDURE Cardinality ( SetPassed : T ) : INTEGER

  = BEGIN

      RETURN SetPassed . Used
    ;

    END Cardinality
;


  BEGIN

  END Set
.



