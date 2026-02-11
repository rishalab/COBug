IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTDFP1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GOOD PIC 9 VALUE 0.
       01 HFP-SRT PIC 9(6) VALUE 0.
       01 HFP-LNG PIC 9(18) VALUE 0.
       01 BFP-SRT PIC 9(6) VALUE 0.
       01 BFP-LNG PIC 9(18) VALUE 0.
       01 BFP-EXT PIC 9(18) VALUE 0.
       01 DFP-SRT PIC 9(6) VALUE 0.
       01 DFP-LNG PIC 9(18) VALUE 0.
       01 DFP-EXT PIC 9(18) VALUE 0.
       01 FLT-SRT PIC 9(18) VALUE 0.
       01 FLT-LNG PIC 9(18) VALUE 0.
       01 FLT-EXT PIC 9(18) VALUE 0.

       PROCEDURE DIVISION.
           MOVE 10 TO HFP-SRT
           MOVE 1234560 TO FLT-SRT
           DIVIDE HFP-SRT INTO FLT-SRT
           IF FLT-SRT = 123456
               ADD 1 TO GOOD
           ELSE
               MOVE 16 TO RETURN-CODE
               STOP RUN.

           DISPLAY 'TESTDFP1 GOOD TESTS =' GOOD
           STOP RUN.

       BAD.
           MOVE 16 TO RETURN-CODE 
           STOP RUN.
```

This COBOL program is designed to test floating-point arithmetic operations and ensure that the results are as expected. If any of the tests fail, it will terminate with an error code indicating failure.