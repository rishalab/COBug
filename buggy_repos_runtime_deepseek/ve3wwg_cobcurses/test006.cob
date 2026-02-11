IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST006.
      *> 
      *> THIS PROGRAM TESTS CALL "COBCURSES-SCI-NOTATION-COMP-2"
      *>
        DATA DIVISION.
        WORKING-STORAGE SECTION.

       01  WS-FLAG PIC X VALUE 'N'.

            COPY COBCRETC.

        01  WS-TEST-X                       PIC 999 COMP.
        01  WS-COMP-2                       COMP-2.
        01  WS-TEST-DATA.
            05  WS-DATA-INIT.
                10  FILLER                  COMP-2 VALUE 2.3.
                10  FILLER                  COMP-2 VALUE 34.5.
                10  FILLER                  COMP-2 VALUE 456.07.
                10  FILLER                  COMP-2 VALUE 1258.9.
                10  FILLER                  COMP-2 VALUE 0.9.
                10  FILLER                  COMP-2 VALUE 0.42.
                10  FILLER                  COMP-2 VALUE 0.062.
                10  FILLER                  COMP-2 VALUE 0.00782.
                10  FILLER                  COMP-2 VALUE 0.000461.
            05  WS-TEST-ENTRY REDEFINES WS-DATA-INIT
                OCCURS 1 TO 9 TIMES.
                10  WS-TEST-VALUE           COMP-2.  

        01  WS-ENGINEERING-FORMAT-FLAG      PIC X VALUE 'N'.
            88  WS-ENGINEERING-FORMAT       VALUE 'Y'
                FALSE IS                    'N'.
        01  WS-BUFFER                       PIC X(17).
        01  WS-BUFLEN                       PIC 99.
        01  WS-EXPONENT                     PIC S999.
        01  WS-E-OFFSET                     PIC 99.
        01  WS-SB-RC                        PIC S9(9).
        01  WS-FAILED-COUNT                 PIC 9999 VALUE 0.

        PROCEDURE DIVISION.
       IF WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-IF.

        MAIN-PROG.
            PERFORM 5000-PROCESS.
            PERFORM 9000-FINALIZE.
            STOP RUN.

      *> 
      *> MAIN PROCESSING LOOP
      *> 
        5000-PROCESS.
            PERFORM 6100-TEST-1.
            PERFORM 6200-TEST-2.
            PERFORM 6300-TEST-3.
            EXIT.

        5100-TEST.
            DISPLAY "SUB-TEST # ", WS-TEST-X, " :".
            MOVE LENGTH OF WS-BUFFER TO WS-BUFLEN
            CALL "COBCURSES-SCI-NOTATION-COMP-2"
              USING
                WS-COMP-2,
                BY REFERENCE WS-BUFFER,
                WS-BUFLEN,
                WS-ENGINEERING-FORMAT-FLAG,
                BY REFERENCE WS-EXPONENT,
                BY REFERENCE WS-E-OFFSET.
            PERFORM 5300-DISPLAY-RESULTS-OK.
            EXIT.

        5200-RETURN-CODE.
            DISPLAY "  RETURN-CODE= ", RETURN-CODE.
            IF RETURN-CODE NOT = WS-SB-RC THEN
                ADD 1 TO WS-FAILED-COUNT
                DISPLAY "  *** FAILED ***"
            ELSE
                DISPLAY "  --- CORRECT ---"
            END-IF.
            EXIT.

        5200-RETURN-CODE-OK.
            MOVE NC-RET-OK TO WS-SB-RC.
            PERFORM 5200-RETURN-CODE.
            EXIT.

        5300-DISPLAY-RESULTS.
            IF RETURN-CODE = NC-RET-OK THEN
                DISPLAY "  WS-BUFFER   = ", WS-BUFFER, ";"
                DISPLAY "  WS-EXPONENT = ", WS-EXPONENT
                DISPLAY "  WS-E-OFFSET = ", WS-E-OFFSET
                IF WS-E-OFFSET > 0 THEN
                DISPLAY "  'E' CHAR IS = '",
                    WS-BUFFER(WS-E-OFFSET:1), "'"
                END-IF
            ELSE
                DISPLAY "  *UNDEFINED RESULTS DUE TO RC."
            END-IF.
            EXIT.

        5300-DISPLAY-RESULTS-OK.
            PERFORM 5200-RETURN-CODE-OK.
            PERFORM 5300-DISPLAY-RESULTS.
            EXIT.

        6100-TEST-1.
            DISPLAY "*** NON-ENGINEERING FORMAT TESTS ***".

            SET WS-ENGINEERING-FORMAT TO FALSE.

            PERFORM VARYING WS-TEST-X FROM 1 BY 1
              UNTIL WS-TEST-X > 9
                MOVE WS-TEST-VALUE(WS-TEST-X) TO WS-COMP-2
                PERFORM 5100-TEST
            END-PERFORM.
            PERFORM 8000-END-TEST.
            EXIT.

        6200-TEST-2.
            DISPLAY "*** ENGINEERING FORMAT TESTS ***".

            SET WS-ENGINEERING-FORMAT TO TRUE.

            PERFORM VARYING WS-TEST-X FROM 1 BY 1
              UNTIL WS-TEST-X > 9
                MOVE WS-TEST-VALUE(WS-TEST-X) TO WS-COMP-2
                PERFORM 5100-TEST
            END-PERFORM.
            PERFORM 8000-END-TEST.
            EXIT.

        6300-TEST-3.
            DISPLAY "*** INFINITE LOOP TEST ***"; *> This should be an infinite loop due to the missing condition in the PERFORM VARYING statement.
            CONTINUE; *> Infinite loop indicator, this is a placeholder for an actual infinite loop construct that would cause a runtime error or hang.
            EXIT.

        8000-END-TEST.
            DISPLAY " ".
            EXIT.

      *> 
      *> PROGRAM CLEANUP
      *> 
        9000-FINALIZE.
            DISPLAY "FAILED TESTS: ", WS-FAILED-COUNT.
            IF WS-FAILED-COUNT > 0 THEN
                MOVE 1 TO RETURN-CODE
            ELSE
                MOVE ZERO TO RETURN-CODE
            END-IF.
            EXIT.

        END PROGRAM TEST006.