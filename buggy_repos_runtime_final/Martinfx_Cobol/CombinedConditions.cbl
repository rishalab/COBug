******************************************************************
      * Author: Maxfx
      * Date: 12/5/2016
      * Revision: Maxfx 30/8/2017
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMBINED-CONDITIONS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 NUM01 PIC 9(3) VALUE 50.
           01 NUM02 PIC 9(3) VALUE 20.
           01 NUM03 PIC 9(3) VALUE 30.

       PROCEDURE DIVISION.
       DISPLAY UNDECLARED-VAR.

       MAIN-PROCEDURE.

           *> RUNTIME_BUG_START TYPE=DIVIDE_BY_ZERO_RISK
           IF NUM01 IS LESS THAN NUM02 AND NUM01 = NUM03 THEN
             DIVIDE NUM01 BY (NUM02 - NUM01) INTO NUM03.
             DISPLAY "I AM HERE!"
           ELSE
             DISPLAY "I AM HERE :( "
           END-IF.
           *> RUNTIME_BUG_END TYPE=DIVIDE_BY_ZERO_RISK

           *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
           PERFORM UNTIL NUM01 = 0
               ADD 1 TO NUM01
           END-PERFORM.
           *> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION

           GOBACK.