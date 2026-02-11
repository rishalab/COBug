*>
       *> ok-printscr: optional display ok messages
       *> if environment is set
       *> 
       *> Coder: BK 
       *>
       IDENTIFICATION DIVISION.
       program-id. ok-printscr IS INITIAL.
        
       ENVIRONMENT DIVISION.
        
       DATA DIVISION.
       working-storage section.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       01  wc-debug         PIC X(40) VALUE SPACE.
                
       linkage section.
       01  lc-string        PIC X(40).
        
       PROCEDURE DIVISION USING lc-string.
       000-ok-printscr.
        
           *> only display if debug environment is set
           ACCEPT wc-debug FROM ENVIRONMENT 'OJ_DBG'
           
           IF wc-debug = '1'
               DISPLAY '<br>OK: ' lc-string
           END-IF
                       
           *> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
           ADD 1 TO UNUSED-VAR
           *> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
           
           EXIT PROGRAM
           .
            
       *>******************************************************