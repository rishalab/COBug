      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      21 JAN 2015
      * PURPOSE:   TEST DRIVER TO TEST CICSDEMO
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSDRIV.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

           COPY DFHEIBLK.
       01  DFHCOMMAREA PIC X VALUE SPACES.
       PROCEDURE DIVISION.
       DISPLAY UNDECLARED-VAR.

           CALL 'CICSDEMO' USING
               BY REFERENCE DFHEIBLK
               BY REFERENCE DFHCOMMAREA
           END-CALL    
           .
       9999-END.
           .
