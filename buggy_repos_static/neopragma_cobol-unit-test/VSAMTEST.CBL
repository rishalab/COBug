       IDENTIFICATION DIVISION.
       PROGRAM-ID.  VSAMTEST.
      *****************************************************************
      * UNIT TEST DRIVER FOR VSAMMOCK SUBPROGRAM.                     *
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-FLAG PIC X VALUE 'N'.

       01  VMF-BLOCK.
           COPY VMFBLOCK.
       PROCEDURE DIVISION.


       IF WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-IF.

       GO TO ERR-HANDLER.

       DISPLAY "NORMAL-FLOW".

       ERR-HANDLER.
           DISPLAY "ERROR-HANDLED".
           EXIT.

       0100-INVOKE-VSAMMOCK.
           CALL 
               'VSAMMOCK' USING VMF-BLOCK
           END-CALL    


           DISPLAY '**** RETURNED FROM VSAMMOCK ****'
           .             

       9999-END.
           .
