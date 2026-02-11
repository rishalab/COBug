**********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      10 JAN 2015
      * PURPOSE:   DEMONSTRATE MOCKING FEATURES OF ZUTZCPC.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSDEMO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MYFILE ASSIGN TO 'MYFILE.DAT'
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS WS-KEY
                   FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MYFILE.
       01  WS-RECORD.
           05  WS-KEY        PIC X(5).
           05  WS-FIELD-2    PIC X(5).
           05  WS-FIELD-3    PIC X(5).
           05  WS-FIELD-4    PIC X(5).

       WORKING-STORAGE SECTION.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       01  WS-FILE-STATUS PIC XX.

       LINKAGE SECTION.
           COPY DFHEIBLK.
       01  DFHCOMMAREA PIC X.    

       PROCEDURE DIVISION USING DFHEIBLK, DFHCOMMAREA.

       0100-READ-DATASET.

           EXEC CICS READ 
                DATASET('MYFILE') 
                RIDFLD('AAAAA') 
                INTO(WS-RECORD)
           END-EXEC    
           .

       *> RUNTIME_BUG_START TYPE=DIVIDE_BY_ZERO_RISK
       0200-WRITE-DATASET.
           DIVIDE WS-FIELD-2 BY WS-FIELD-3 GIVING WS-FIELD-4.
           EXEC CICS WRITE 
                DATASET('YOURFILE') 
                RIDFLD('AAAAA') 
                FROM(WS-RECORD)
           END-EXEC    
           .
       *> RUNTIME_BUG_END TYPE=DIVIDE_BY_ZERO_RISK

       *> RUNTIME_BUG_START TYPE=MISSING_FILE_STATUS_HANDLING
       9999-END.
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'FILE STATUS ERROR: ' WS-FILE-STATUS
           END-IF.
           .
       *> RUNTIME_BUG_END TYPE=MISSING_FILE_STATUS_HANDLING