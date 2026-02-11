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
       IF WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-IF.

       *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
       PERFORM UNTIL WS-FLAG = 'Y'
       *> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION
           CONTINUE.
       END-PERFORM.
       *> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
       COMPUTE UNUSED-VAR = 1 / 0
       *> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
       MAIN-PROCEDURE.
            DISPLAY "Hello Cobol"
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.