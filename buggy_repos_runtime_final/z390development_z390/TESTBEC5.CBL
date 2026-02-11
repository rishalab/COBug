*****************************************************************
      * z390 - Mainframe assembler program
      * This program demonstrates the use of CICS commands to handle AID
      * blocks, conditions, and memory management.
      *****************************************************************

      IDENTIFICATION DIVISION.
      PROGRAM-ID. TESTCICS.

      ENVIRONMENT DIVISION.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 DFHEIBLK.
         05 EIBAID PIC X(4).
      01 DFHCOMMAREA.
         05 COMMAREA-LENGTH PIC S9(8) COMP.
         05 COMMAREA-DATA PIC X(256).

      PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.

      * DISPLAY INITIAL MESSAGE
          EXEC CICS SEND FROM('HELLO, THIS IS A TEST OF CICS COMMANDS.')
                LENGTH(43)
                END-EXEC.

      * HANDLE AID BLOCK FOR PF1 AND CLEAR KEY
          EXEC CICS HANDLE AID
               ENTER(PF1)
               ANYKEY(CLEAR)
               END-EXEC.

      * RECEIVE INPUT FROM USER
      KEY-PRESS.
          EXEC CICS RECEIVE INTO(COMMAREA-DATA) LENGTH(COMMAREA-LENGTH)
                END-EXEC.

      * CHECK FOR CLEAR KEY PRESSED
          IF EIBAID = '03'
             EXEC CICS RETURN END-EXEC
          ELSE
             GO TO KEY-PRESS
          END-IF.

      * HANDLE CONDITION BLOCK FOR ERROR
          EXEC CICS HANDLE CONDITION ERROR(GOTERROR)
                END-EXEC.

      * ATTEMPT TO LINK TO A NON-EXISTENT PROGRAM TO RAISE AN ERROR
          EXEC CICS LINK PROGRAM('NONEXISTENT')
                END-EXEC.

      * DISPLAY MESSAGE IF NO ERROR OCCURRED
          EXEC CICS SEND FROM('NO ERROR OCCURRED.')
                LENGTH(19)
                END-EXEC.

      * RETURN CONTROL TO THE CICS REGION
          EXEC CICS RETURN END-EXEC.

      * HANDLE ERROR CONDITION
      GOTERROR.
          EXEC CICS SEND FROM('AN ERROR OCCURRED.')
                LENGTH(17)
                END-EXEC.
          GO TO KEY-PRESS.

      END PROGRAM TESTCICS.