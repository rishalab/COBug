      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      13 APR 2017
      * PURPOSE:   TEST DRIVER CARD3.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARD3D.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       01  WS-PARAMETERS.
           05  WS-ARGUMENTS.
               10  FILLER             PIC X(10).
               10  IN-ACCOUNT-NUMBER  PIC X(16).
               10  FILLER             PIC X(174).
           05  WS-RETURN-VALUES.
               10  OUT-ACCOUNT-NUMBER PIC X(16).
               10  OUT-MESSAGE        PIC X(184).    
       PROCEDURE DIVISION.

       GO TO ERR-HANDLER.

       DISPLAY "NORMAL-FLOW".

       ERR-HANDLER.
           DISPLAY "ERROR-HANDLED".
           EXIT.

           CALL 'CARD3' USING
               BY REFERENCE WS-PARAMETERS
           END-CALL    
           .
       9999-END.
           .
