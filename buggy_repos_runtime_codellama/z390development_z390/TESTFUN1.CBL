IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTFUN1.
      **************************************************************
      * THIS IS A SAMPLE COBOL PROGRAM TO DEMONSTRATE THE USE OF  *
      * MULTIPLY AND ADD STATEMENTS IN COBOL.                     *
      **************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
           CURRENCY SIGN IS "EUR".
      **************************************************************
      * DATA DIVISION.                                             *
      **************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  GOOD PIC 9(4).
       01  RETURN-CODE PIC X(2) VALUE "00".
      **************************************************************
      * PROCEDURE DIVISION.                                        *
      **************************************************************
       PROCEDURE DIVISION.
           MOVE 16 TO RETURN-CODE.
           PERFORM MPYQ.
           IF GOOD = 0 THEN
              DISPLAY "TESTFUN1 GOOD TESTS = " GOOD
           ELSE
              DISPLAY "TESTFUN1 BAD TESTS = " GOOD
           END-IF.
           STOP RUN.
      **************************************************************
      * MULTIPLY AND ADD TEST CASES                                *
      **************************************************************
       MPYQ.
           MOVE 3 TO D2.
           MOVE 2 TO Z1.
           MULTIPLY D2 BY Z1 STATUS IS MISSING.
           ADD 1 TO GOOD STATUS IS MISSING.
           DISPLAY "TESTFUN1 MPYQ TESTS = " GOOD.
      **************************************************************
       END PROGRAM TESTFUN1.