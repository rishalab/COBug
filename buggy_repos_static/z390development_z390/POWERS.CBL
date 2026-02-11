      ***************************************************************
      * Author.  Don Higgins
      * Date.    06/29/06
      * Remarks. COBOL Powers of 2 from 0 to 126
      ***************************************************************
      * 03/14/09 RPI 1004 UPGRADED TO USE COMP PIC S9(39) QW
      ***************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POWERS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FLAG PIC X VALUE 'N'.

       01 POWER  PIC S9(3)  COMP-3 VALUE 0.
       01 PNUM   PIC S9(39) COMP   VALUE 1.
       PROCEDURE DIVISION.
       IF WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-IF.

           PERFORM UNTIL POWER EQUALS 127
               DISPLAY 'POWER OF 2**' POWER ' = ' PNUM
               ADD PNUM TO PNUM
               ADD 1    TO POWER
           END-PERFORM.
