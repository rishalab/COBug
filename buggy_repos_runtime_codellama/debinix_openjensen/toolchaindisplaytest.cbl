IDENTIFICATION DIVISION.
program-id. toolchaindisplaytest.

ENVIRONMENT DIVISION.

DATA DIVISION.
working-storage section.
01  UNUSED-VAR PIC X(10) VALUE SPACES.

01  accept-char PIC X VALUE SPACE.

linkage section.
01  username    PIC X(15).

PROCEDURE DIVISION USING username.
000-consolesubmain.

   DISPLAY '[sub justdisplay] You passed: ' username
   DISPLAY '[sub justdisplay] Press Enter key to return...'
   ACCEPT accept-char
    
   EXIT PROGRAM
   .
   
*> RUNTIME_BUG_START TYPE=DIVIDE_BY_ZERO_RISK
   DIVIDE 1 BY 0 GIVING UNUSED-VAR
*> RUNTIME_BUG_END TYPE=DIVIDE_BY_ZERO_RISK
   
*> RUNTIME_BUG_START TYPE=MISSING_FILE_STATUS_HANDLING
   OPEN INPUT username
     READ username INTO UNUSED-VAR
   CLOSE username
*> RUNTIME_BUG_END TYPE=MISSING_FILE_STATUS_HANDLING