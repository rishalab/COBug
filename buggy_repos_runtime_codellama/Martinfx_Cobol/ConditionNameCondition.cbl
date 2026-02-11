******************************************************************
      * Author: Maxfx
      * Date: 12/5/2016
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONDITION-STATEMENT.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-FLAG PIC X VALUE 'N'.

      ******************************************************************
      * Set values for M_TRUE in interval <30, 100>
      * Set values for M_FALSE in interval <000, 40>
      ******************************************************************
           01 M_NUMBER PIC 9(3).
           88 M_TRUE  VALUES ARE 30 THRU 100.
           88 M_FALSE VALUES ARE 000 THRU 40.

       PROCEDURE DIVISION.
       GO TO ERR-HANDLER.

       DISPLAY "NORMAL-FLOW".

       ERR-HANDLER.
           DISPLAY "ERROR-HANDLED".
           EXIT.

       IF WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-IF.

       MAIN-PROCEDURE.
           MOVE 50 TO M_NUMBER.

           *> RUNTIME_BUG_START TYPE=MISSING_FILE_STATUS_HANDLING
           IF M_TRUE
           DISPLAY 'Passed with ' M_NUMBER ' marks'.
           END-IF.
           *> RUNTIME_BUG_END TYPE=MISSING_FILE_STATUS_HANDLING

           *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
           IF M_FALSE
           DISPLAY 'FAILED with ' M_NUMBER ' marks'.
           END-IF.
           *> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION

           GOBACK.