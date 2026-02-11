******************************************************************
      * Author: Lucas Magalh√£es
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       01  WS-FLAG PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
       PERFORM UNTIL WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-PERFORM.
       *> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION

       MAIN-PROCEDURE.
            DISPLAY "Hello Cobol"
            *> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
            ADD 1 TO WS-FLAG.
            *> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.