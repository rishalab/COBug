**********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      13 APR 2017
      * PURPOSE:   TEST DRIVER CARD2.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARD2D.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FLAG PIC X VALUE 'N'.

       01  WS-PARAMETERS.
           05  WS-ARGUMENTS.
               10  FILLER             PIC X(10).
               10  IN-ACCOUNT-NUMBER  PIC X(16).
               10  FILLER             PIC X(174).
           05  WS-RETURN-VALUES.
               10  OUT-ACCOUNT-NUMBER PIC X(16).
               10  OUT-MESSAGE        PIC X(184).    
       PROCEDURE DIVISION.

*> RUNTIME_BUG_START TYPE=UNINITIALIZED_DATA_ITEM_USE
       DISPLAY UNDECLARED-VAR.
*> RUNTIME_BUG_END TYPE=UNINITIALIZED_DATA_ITEM_USE

       IF WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-IF.

*> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
           CALL 'CARD2' USING
               BY REFERENCE WS-PARAMETERS
           END-CALL    
*> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC

           .
       9999-END.
           .