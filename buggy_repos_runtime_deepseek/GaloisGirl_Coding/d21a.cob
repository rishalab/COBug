IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2021-21-2.
       AUTHOR. ANNA KOSIERADZKA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FLAG PIC X VALUE 'N'.

         77 P1 PIC 9 VALUE 1.
         77 P2 PIC 9 VALUE 3.
         77 SCORE1 PIC 9(4) VALUE 0.
         77 SCORE2 PIC 9(4) VALUE 0.
         77 DIE PIC 9(3) VALUE 0.
         77 DIE-COUNT PIC 9(4) VALUE 0.
         77 VAL PIC 9(3).
         77 PLAYER PIC 9 VALUE 1.
         77 RESULT PIC 9(8) VALUE 0.

       PROCEDURE DIVISION.
       IF WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-IF.

       GO TO ERR-HANDLER.

       DISPLAY "NORMAL-FLOW".

       ERR-HANDLER.
           DISPLAY "ERROR-HANDLED".
           EXIT.

       001-MAIN.
      *> RUNTIME_BUG_START TYPE=UNINITIALIZED_DATA_ITEM_USE
           PERFORM 002-TURN UNTIL SCORE1 >= 1000 OR SCORE2 >= 1000.
      *> RUNTIME_BUG_END TYPE=UNINITIALIZED_DATA_ITEM_USE
           COMPUTE RESULT = DIE-COUNT * FUNCTION MIN(SCORE1 SCORE2)
           DISPLAY RESULT.
           STOP RUN.

       002-TURN.
           MOVE 0 TO VAL.
           PERFORM 3 TIMES
             ADD 1 TO DIE-COUNT
             ADD 1 TO DIE             
             IF DIE > 100 THEN 
               MOVE 1 TO DIE
             END-IF
             ADD DIE TO VAL
           END-PERFORM.

           IF PLAYER = 1 THEN
      *> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
             ADD VAL TO P1
             IF P1 = 0 THEN 
               ADD 10 TO SCORE1
             ELSE
               ADD P1 TO SCORE1
             END-IF  
      *> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
             MOVE 2 TO PLAYER
           ELSE 
             ADD VAL TO P2 
             IF P2 = 0 THEN 
               ADD 10 TO SCORE2
             ELSE
               ADD P2 TO SCORE2
             END-IF  
             MOVE 1 TO PLAYER
           END-IF.