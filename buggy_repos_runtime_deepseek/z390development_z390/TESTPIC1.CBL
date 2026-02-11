IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPUTATIONAL-DATA-TYPES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-UNSIGN-01 PIC S9(04) COMP VALUE +1234.
       01  WS-UNSIGN-02 PIC S9(04) COMP VALUE +5678.
       01  WS-SIGN-01   PIC S9(04) COMP VALUE -1234.
       01  WS-SIGN-02   PIC S9(04) COMP VALUE -5678.

       PROCEDURE DIVISION.
           DISPLAY "Computational Data Types".
           STOP RUN.
```

### Explanation of Changes:
1. **IDENTIFICATION DIVISION**: Added to specify the program ID.
2. **WORKING-STORAGE SECTION**: Defined variables for unsigned and signed computational types.
3. **PROCEDURE DIVISION**: Corrected the `DISPLAY` statement syntax and added a proper `STOP RUN` statement at the end of the procedure division.

This corrected version should compile and run correctly in a COBOL environment, displaying the message "Computational Data Types" before stopping the program execution.