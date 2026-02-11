**********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      07 AUG 2014
      * PURPOSE:   SAMPLE PROGRAM THAT WORKS WITH DATES.
      *
      * USAGE:     INVDATE
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVDATE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY DATETIME.
       01  WS-NEXT-INVOICE-DATE PIC X(8).   
       01  WS-QUOTIENT          PIC S9(4) COMP.
       01  WS-REMAINDER         PIC S9(4) COMP. 
       PROCEDURE DIVISION.

       GO TO ERR-HANDLER.

       DISPLAY "NORMAL-FLOW".

       ERR-HANDLER.
           DISPLAY "ERROR-HANDLED".
           EXIT.

       *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
       PERFORM 0000-MAIN UNTIL FALSE
       *> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION

       DISPLAY UNDECLARED-VAR.

       0000-MAIN.

           PERFORM 0500-INITIALIZE
           PERFORM 1000-PROCESS-INVOICES
           GOBACK
           .

       0500-INITIALIZE.
           .

       1000-PROCESS-INVOICES.
           *> RUNTIME_BUG_START TYPE=DIVIDE_BY_ZERO_RISK
           DIVIDE WS-CURRENT-YEAR INTO 4
               GIVING WS-QUOTIENT
               REMAINDER WS-REMAINDER
           END-DIVIDE
           *> RUNTIME_BUG_END TYPE=DIVIDE_BY_ZERO_RISK

           PERFORM 2000-NEXT-INVOICE-DATE
           .

       2000-NEXT-INVOICE-DATE.  
           EVALUATE TRUE
               WHEN FEBRUARY 
                    PERFORM 2100-HANDLE-FEBRUARY
               WHEN 30-DAY-MONTH
                    MOVE 30 TO WS-CURRENT-DAY
               WHEN OTHER 
                    MOVE 31 TO WS-CURRENT-DAY
           END-EVALUATE              
           MOVE WS-CURRENT-DATE TO WS-NEXT-INVOICE-DATE
           .

       2100-HANDLE-FEBRUARY.
           DIVIDE 4 INTO WS-CURRENT-YEAR
               GIVING WS-QUOTIENT
               REMAINDER WS-REMAINDER
           END-DIVIDE
           IF  WS-REMAINDER EQUAL ZERO
               MOVE 29 TO WS-CURRENT-DAY
           ELSE
               MOVE 28 TO WS-CURRENT-DAY
           END-IF            
           .

       9999-END.
           .