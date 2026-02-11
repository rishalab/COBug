IDENTIFICATION DIVISION.
PROGRAM-ID. ExampleProgram.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 GOOD PIC 9(5) VALUE 0.
01 BAD PIC 9(5) VALUE 0.

PROCEDURE DIVISION.
    PERFORM INIT

    * Perform a series of conditional additions
    IF DB2 = EH1 THEN ADD DB2 TO EH1 ELSE PERFORM BAD
    IF DD2 = DH1 THEN ADD DD2 TO DH1 ELSE PERFORM BAD
    * ... (repeat for all variables)

    DISPLAY 'TOTAL GOOD = ' GOOD ' TOTAL BAD = ' BAD

    STOP RUN.

INIT.
    MOVE 1.0 TO EH1.
    MOVE 1.0 TO DH1.
    * ... (initialize all other variables)

GOOD.
    ADD 1 TO GOOD.

BAD.
    ADD 1 TO BAD
    ADD 16 TO RETURN-CODE.
    DISPLAY 'BAD'.
```

Please note that this is a simplified version and does not include all the conditions from your original code. Also, in COBOL, you cannot directly compare floating-point numbers like this; you would need to use a comparison function or round the numbers before comparing them.