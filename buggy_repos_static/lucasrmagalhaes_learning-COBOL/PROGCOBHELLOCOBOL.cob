      ******************************************************************
      * Author: Lucas Magalhães
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

       MAIN-PROCEDURE.
            DISPLAY "Hello Cobol"
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
