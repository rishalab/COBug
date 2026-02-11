**********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      10 JAN 2015
      * PURPOSE:   DEMONSTRATE MOCKING FEATURES OF ZUTZCPC.
      **********************************************************************
       IDENTIFICATION DIVISATION.
       PROGRAM-ID. CICSDEMO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       01  WS-RECORD.
           05  WS-KEY        PIC X(5).
           05  WS-FIELD-2    PIC X(5).
           05  WS-FIELD-3    PIC X(5).
           05  WS-FIELD-4    PIC X(5).

       LINKAGE SECTION.
           COPY DFHEIBLK.
       01  DFHCOMMAREA PIC X.    

       PROCEDURE DIVISION USING DFHEIBLK, DFHCOMMAREA.

       0100-READ-DATASET.
           *> RUNTIME_BUG_START TYPE=DIVIDE_BY_ZERO_RISK
           COMPUTE WS-KEY = 1 / 0
           *> RUNTIME_BUG_END TYPE=DIVIDE_BY_ZERO_RISK
           EXEC CICS READ 
                DATASET('MYFILE') 
                RIDFLD('AAAAA') 
                INTO(WS-RECORD)
           END-EXEC    
           .

       0200-WRITE-DATASET.
           *> RUNTIME_BUG_START TYPE=MISSING_FILE_STATUS_HANDLING
           EXEC CICS WRITE 
                DATASET('YOURFILE') 
                RIDFLD('AAAAA') 
                FROM(WS-RECORD)
           END-EXEC    
           .
       9999-END.
           .