******************************************************************
      * Author: Maxfx
      * Date: 13/10/2016
      * Revision: Maxfx 31/08/2017
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATE-TIME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

           01 W-BATCH-COMPLET PIC X(16).
           01 W-BATCH-TEST    PIC X(16).
           01 W-BATCH.
              03 W-DATE PIC X(8).
              03 W-TIME PIC X(8).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           ACCEPT W-TIME OF W-BATCH FROM TIME.
           ACCEPT W-DATE OF W-BATCH FROM DATE YYYYMMDD.

           DISPLAY "W-TIME: " W-TIME OF W-BATCH.
           DISPLAY "W-DATE: " W-DATE OF W-BATCH.
           DISPLAY "W-BATCH: " W-BATCH.

           *> RUNTIME_BUG_START TYPE=DIVIDE_BY_ZERO_RISK
           DIVIDE 1 BY ZERO GIVING UNUSED-VAR.
           *> RUNTIME_BUG_END TYPE=DIVIDE_BY_ZERO_RISK

           STRING W-DATE OF W-BATCH DELIMITED BY SPACE
           W-TIME OF W-BATCH DELIMITED BY SPACE
             INTO W-BATCH-COMPLET
           END-STRING.

           MOVE W-BATCH TO W-BATCH-TEST.

           *> RUNTIME_BUG_START TYPE=MISSING_FILE_STATUS_HANDLING
           OPEN INPUT UNUSED-VAR.
           *> RUNTIME_BUG_END TYPE=MISSING_FILE_STATUS_HANDLING

           DISPLAY "COMPLET : " W-BATCH-COMPLET.
           DISPLAY "TEST : " W-BATCH-TEST

           GOBACK.