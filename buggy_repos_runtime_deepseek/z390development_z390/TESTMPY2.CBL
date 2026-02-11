IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  G               PIC 9 VALUE 0.
       01  B               PIC 9 VALUE 0.
       01  EH1             PIC 9V9 VALUE 3.0.
       01  DH1             PIC 9V9 VALUE 3.0.
       01  LH1             PIC 9V9 VALUE 3.0.
       01  EB1             PIC 9V9 VALUE 3.0.
       01  DB1             PIC 9V9 VALUE 3.0.
       01  LB1             PIC 9V9 VALUE 3.0.
       01  ED1             PIC 9V9 VALUE 3.0.
       01  DD1             PIC 9V9 VALUE 3.0.
       01  LD1             PIC 9V9 VALUE 3.0.
       01  F1              PIC 9V9 VALUE 3.0.
       01  G1              PIC 9V9 VALUE 3.0.
       01  H1              PIC 9V9 VALUE 3.0.
       01  P1              PIC 9V9 VALUE 3.0.
       01  Q1              PIC 9V9 VALUE 3.0.
       01  Z1              PIC 9V9 VALUE 3.0.

       PROCEDURE DIVISION.
           INITIALIZE YOUR-PROGRAM-NAME.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 12
               IF Z3 = Z1 THEN
                   PERFORM G
               ELSE
                   PERFORM B
               END-IF
           END-PERFORM
           DISPLAY 'TOTAL G = ' G ' TOTAL B = ' B
           STOP RUN.

       INITIALIZE YOUR-PROGRAM-NAME.
           MOVE 3.0 TO EH1.
           MOVE 3.0 TO DH1.
           MOVE 3.0 TO LH1.
           MOVE 3.0 TO EB1.
           MOVE 3.0 TO DB1.
           MOVE 3.0 TO LB1.
           MOVE 3.0 TO ED1.
           MOVE 3.0 TO DD1.
           MOVE 3.0 TO LD1.
           MOVE 3.0 TO F1.
           MOVE 3.0 TO G1.
           MOVE 3.0 TO H1.
           MOVE 3.0 TO P1.
           MOVE 3.0 TO Q1.
           MOVE 3.0 TO Z1.

       G.
           ADD 1 TO G.
           DISPLAY 'GOOD-TEST'.

       B.
           ADD 1 TO B.
           MOVE 16 TO RETURN-CODE.
           DISPLAY 'BAD-TEST'.
```

This COBOL program sets up a loop that iterates over a range of values, performing different actions based on whether the condition `Z3 = Z1` is true or false. It uses conditional branching to decide between executing procedures G and B, which in turn modify counters for successful and failed tests, respectively.