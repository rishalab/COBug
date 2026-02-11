IDENTIFICATION DIVISION.
PROGRAM-ID. GUI6PROG.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  MYCOMM PIC X(86).
01  EIBAID PIC X(4).
01  COMMIND PIC X(1) VALUE '1'.
01  ERRMSGO PIC X(80).
01  GUI6O PIC X(256).

PROCEDURE DIVISION.
GUI6.
    EXEC CICS RECEIVE MAP('GUI6') MAPSET('MAP01C')
              INTO(MYCOMM)
              LENGTH(86)
    END-EXEC.

    IF EIBAID = DFHPF3
        GO TO ALLDONE
    ELSE
        GO TO EACTION
    END-IF.

EACTION.
    EXEC CICS SEND MAP('GUI6') MAPSET('MAP01C')
              FROM(GUI6O)
              LENGTH(LENGTH OF GUI6O)
              ERASE
    END-EXEC.

    IF EIBAID = DFHPF3
        GO TO ALLDONE
    ELSE
        GO TO EABACK
    END-IF.

EABACK.
    EXEC CICS RETURN TRANSID('BMC1') COMMAREA(MYCOMM)
              LENGTH(86)
    END-EXEC.

ALLDONE.
    MOVE 'Thank you for using the system.' TO ERRMSGO.
    EXEC CICS SEND MAP('GUI6') MAPSET('MAP01C')
              FROM(ERRMSGO)
              LENGTH(LENGTH OF ERRMSGO)
    END-EXEC.
    EXEC CICS RETURN
    END-EXEC.

SENDIT.
    EXEC CICS SEND MAP(MYGUI6) MAPSET('MAP01C')
              FROM(GUI6O)
              LENGTH(LENGTH OF GUI6O)
              ERASE
    END-EXEC.
    GO TO ECRETURN.

ECRETURN.
    EXEC CICS RETURN TRANSID('BMC1') COMMAREA(MYCOMM)
              LENGTH(86)
    END-EXEC.

MAPFAILS.
    IF EIBRESP2 = X'02'
        GO TO SHORTRD
    ELSE
        MOVE 'Error occurred.' TO ERRMSGO
        EXEC CICS SEND MAP('GUI6') MAPSET('MAP01C')
                  FROM(ERRMSGO)
                  LENGTH(LENGTH OF ERRMSGO)
        END-EXEC.
        GO TO SENDIT
    END-IF.

ABEND.
    EXEC CICS ABEND ABCODE('BMC1')
    END-EXEC.
```

This is a simplified example to illustrate the structure of a COBOL program interacting with CICS. The actual program you provided has more detailed logic and error handling, which would be necessary for a production system.