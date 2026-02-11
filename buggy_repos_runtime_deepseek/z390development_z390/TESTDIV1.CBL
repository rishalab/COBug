IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 EH1  PIC 9V9 VALUE 6.0.
       01 DH1  PIC 9V9 VALUE 6.0.
       01 LH1  PIC 9V9 VALUE 6.0.
       01 EB1  PIC 9V9 VALUE 6.0.
       01 DB1  PIC 9V9 VALUE 6.0.
       01 LB1  PIC 9V9 VALUE 6.0.
       01 ED1  PIC 9V9 VALUE 6.0.
       01 DD1  PIC 9V9 VALUE 6.0.
       01 LD1  PIC 9V9 VALUE 6.0.
       01 F1   PIC 9V9 VALUE 6.0.
       01 G1   PIC 9V9 VALUE 6.0.
       01 H1   PIC 9V9 VALUE 6.0.
       01 P1   PIC 9V9 VALUE 6.0.
       01 Q1   PIC 9V9 VALUE 6.0.
       01 Z1   PIC 9V9 VALUE 6.0.

       PROCEDURE DIVISION.
           INITIALIZE.
               DISPLAY 'INIT'.
               MOVE 6.0 TO EH1.
               MOVE 6.0 TO DH1.
               MOVE 6.0 TO LH1.
               MOVE 6.0 TO EB1.
               MOVE 6.0 TO DB1.
               MOVE 6.0 TO LB1.
               MOVE 6.0 TO ED1.
               MOVE 6.0 TO DD1.
               MOVE 6.0 TO LD1.
               MOVE 6.0 TO F1.
               MOVE 6.0 TO G1.
               MOVE 6.0 TO H1.
               MOVE 6.0 TO P1.
               MOVE 6.0 TO Q1.
               MOVE 6.0 TO Z1.

           IF EH3 = EH1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF DH3 = DH1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF LH3 = LH1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF EB3 = EB1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF DB3 = DB1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF LB3 = LB1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF ED3 = ED1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF DD3 = DD1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF LD3 = LD1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF F3 = F1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF G3 = G1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF H3 = H1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF P3 = P1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF Q3 = Q1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.
           IF Z3 = Z1 THEN ADD 1 TO G DISPLAY 'GOOD-TEST'.

           IF EH3 NOT = EH1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF DH3 NOT = DH1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF LH3 NOT = LH1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF EB3 NOT = EB1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF DB3 NOT = DB1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF LB3 NOT = LB1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF ED3 NOT = ED1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF DD3 NOT = DD1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF LD3 NOT = LD1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF F3 NOT = F1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF G3 NOT = G1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF H3 NOT = H1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF P3 NOT = P1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF Q3 NOT = Q1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.
           IF Z3 NOT = Z1 THEN ADD 1 TO B DISPLAY 'BAD-TEST'.

           DISPLAY 'TOTAL G = ' G ' TOTAL B = ' B.
           STOP RUN.
```

This COBOL program is designed to test conditions and count the number of times a condition is met (incrementing `G`) or not met (incrementing `B`). The final step displays the total counts of `G` and `B`.