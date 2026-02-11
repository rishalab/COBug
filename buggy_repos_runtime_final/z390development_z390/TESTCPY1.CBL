       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTCPY1.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  12/01/08.
      ***************************************************************
      * REGRESSION TEST NESTED COPY SUPPORT
      ***************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FLAG PIC X VALUE 'N'.

       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       COPY TESTCPYA.
       PROCEDURE DIVISION.
       IF WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-IF.

       MAINLINE.
           DISPLAY 'TESTCPY1 STARTING'
           DISPLAY 'TESTCPYA FROM TESTCPYA.CPZ =' TESTCPYA
           DISPLAY 'TESTCPYB FROM TESTCPYB.CPZ =' TESTCPYB
           DISPLAY 'TESTCPY1 ENDED OK'
           STOP RUN.
