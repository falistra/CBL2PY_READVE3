      $set repository(update ON)
       FUNCTION-ID. tgxid.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  Integer1                       PIC S9(4) COMP.
       01  ResultInteger                  PIC S9(4) COMP.
      *01  ResultInteger                  binary-long.
      *01  Integer1                       binary-long VALUES 0.
      *procedure division using BY VALUE Integer1 , 
       procedure division using Integer1 ,
                    returning ResultInteger.

      *    DISPLAY "Integer1:" Integer1.
           IF Integer1 = 10 
              MOVE 0 TO ResultInteger
           ELSE
              MOVE Integer1 TO ResultInteger
           END-IF.
      *    DISPLAY "Result:" ResultInteger.
           goback.

       end function tgxid.
