IDENTIFICATION DIVISION.
       PROGRAM-ID. WGRVGBAL IS RECURSIVE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CUST-REC-KEY.
           05  NAMEI      PIC X(24).
           05  NAMEL      PIC S9(4) COMP.
           05  NAMEO      PIC X(24).
           05  SSN        PIC X(9).
       01  ACCT-REC-KEY.
           05  NUM        PIC X(7).
           05  NUML       PIC S9(4) COMP.
           05  NUMO       PIC X(7).
       01  CUSTOMER-RECORD.
           05  NAME       PIC X(24).
           05  SSN        PIC X(9).
           05  ADDRESS    PIC X(30).
           05  PHONE      PIC X(10).
       01  ACCOUNT-RECORD.
           05  ACCTNUM    PIC X(7).
           05  BALANCE    PIC S9(7)V9(2).
           05  TYPE       PIC X(1).
       01  RESP-CODE      PIC S9(4) COMP.
       01  RET-CODE       PIC S9(4) COMP.
       01  GBMSG1O        PIC X(30).
       01  GBMSG2O        PIC X(30).
       01  ACCTBALO       PIC S9(7)V9(2).
       01  WBCUSTDB-DD    PIC X(8) VALUE 'WBCUSTDB'.
       01  WBACCTDB-DD    PIC X(8) VALUE 'WBACCTDB'.
       01  LOG-MSG.
           05  TASK-NUMBER  PIC S9(7) COMP-5.
           05  HW-LENGTH    PIC S9(4) COMP.
           05  LENGTH       PIC S9(4) COMP.
           05  EIBTASKN      PIC S9(7) COMP-5.
       01  EDIT-NUM       PIC S9(7) COMP-5.
       01  WGRVMGBO.
           05  GBTRANO     PIC X(4).
           05  NAMEO       PIC X(24).
           05  ACCTNUMO    PIC X(7).
           05  ACCTBALO    PIC S9(7)V9(2).
           05  GBMSG1O     PIC X(30).
           05  GBMSG2O     PIC X(30).
       01  WGRVMGB-MAP.
           05  GBTRANI     PIC X(4).
           05  NAMEI       PIC X(24).
           05  ACCTNUMI    PIC X(7).
           05  ACCTBALI    PIC S9(7)V9(2).
           05  GBMSG1I     PIC X(30).
           05  GBMSG2I     PIC X(30).
       01  WGRVMGB-MAP-LENGTH      PIC S9(4) COMP VALUE 68.
       LINKAGE SECTION.
       01  LK-WGRVMGB-MAP.
           05  LK-GBTRANI     PIC X(4).
           05  LK-NAMEI       PIC X(24).
           05  LK-ACCTNUMI    PIC X(7).
           05  LK-ACCTBALI    PIC S9(7)V9(2).
           05  LK-GBMSG1I     PIC X(30).
           05  LK-GBMSG2I     PIC X(30).
       PROCEDURE DIVISION.
       000-MAIN-PROCEDURE.
           MOVE WGRVMGBO TO WGRVMGB-MAP.
           PERFORM SET-MAP-DEFAULTS.
           IF NAMEL = 0 OR NAMEI = SPACES
               MOVE 'Name is invalid' TO GBMSG1O
               MOVE 1 TO RET-CODE
               GO TO FORMAT-ERROR-MSG
           END-IF.
           MOVE NAMEI TO NAME OF CUST-REC-KEY.
           IF ACCTNUML = 0 OR ACCTNUMI = SPACES
               MOVE 'Account number is invalid' TO GBMSG1O
               MOVE 1 TO RET-CODE
               GO TO FORMAT-ERROR-MSG
           END-IF.
           MOVE ACCTNUMI TO NUM OF ACCT-REC-KEY.
           PERFORM GET-CUST-SSN.
           IF RET-CODE = 0
               PERFORM GET-ACCT-BAL
               IF RET-CODE = 0
                   PERFORM FORMAT-GOOD-MSG
               ELSE
                   GO TO FORMAT-ERROR-MSG
               END-IF
           ELSE
               GO TO FORMAT-ERROR-MSG
           END-IF.
           EXEC CICS SEND MAP('WGRVMGB') MAPSET('WGRVMAP')
                          FROM(WGRVMGBO) ERASE END-EXEC.
           EXIT PROGRAM.
       010-FORMAT-GOOD-MSG.
           EVALUATE ACCOUNT-TYPE-CODE
              WHEN 'C'
                 MOVE ACCOUNT-CHK-BAL TO ACCTBALO
              WHEN 'S'
                 MOVE ACCOUNT-SAV-BAL TO ACCTBALO
              WHEN OTHER
                 MOVE ZERO TO ACCTBALO
           END-EVALUATE.
           EXEC CICS SEND MAP('WGRVMGB') MAPSET('WGRVMAP')
                          FROM(WGRVMGBO) ERASE END-EXEC.
           EXIT.
       020-FORMAT-ERROR-MSG.
           EXEC CICS SEND MAP('WGRVMGB') MAPSET('WGRVMAP')
                          FROM(WGRVMGBO) ERASE END-EXEC.
           EXIT.
       030-VALIDATE-INPUT.
           IF NAMEL = 0 OR NAMEI = SPACES
               MOVE 'Name is invalid' TO GBMSG1O
               MOVE 1 TO RET-CODE
               GO TO VALIDATE-INPUT-EXIT
           END-IF.
           MOVE NAMEI TO NAME OF CUST-REC-KEY.
           IF ACCTNUML = 0 OR ACCTNUMI = SPACES
               MOVE 'Account number is invalid' TO GBMSG1O
               MOVE 1 TO RET-CODE
               GO TO VALIDATE-INPUT-EXIT
           END-IF.
           MOVE ACCTNUMI TO NUM OF ACCT-REC-KEY.
       VALIDATE-INPUT-EXIT.
           EXIT.
       040-GET-CUST-SSN.
           EXEC CICS READ
                     DATASET(WBCUSTDB-DD)
                     INTO(CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CUSTOMER-RECORD)
                     KEYLENGTH(LENGTH OF CUST-REC-KEY)
                     RIDFLD(CUST-REC-KEY)
                     RESP(RESP-CODE)
           END-EXEC.
           EVALUATE RESP-CODE
              WHEN 0
                 MOVE CUSTOMER-SSN TO SSN OF ACCT-REC-KEY
                 MOVE 0 TO RET-CODE
                 MOVE SPACES TO GBMSG1O
                 GO TO GET-CUST-SSN-EXIT
              WHEN DFHRESP(NOTOPEN)
                 MOVE 'Customer file not open' TO GBMSG1O
                 MOVE 1 TO RET-CODE
                 GO TO GET-CUST-SSN-EXIT
              WHEN DFHRESP(ENDFILE)
                 GO TO GET-CUST-SSN-NOTFND
              WHEN DFHRESP(NOTFND)
                 GO TO GET-CUST-SSN-NOTFND
              WHEN OTHER
                 MOVE 'I/O error on Customer file' TO GBMSG1O
                 MOVE RESP-CODE TO EDIT-NUM
                 STRING 'Response code=' DELIMITED SIZE
                        EDIT-NUM DELIMITED SIZE
                        INTO GBMSG2O
                 END-STRING
                 MOVE 3 TO RET-CODE
                 GO TO GET-CUST-SSN-EXIT
           END-EVALUATE.
           GO TO GET-CUST-SSN-EXIT.
       GET-CUST-SSN-NOTFND.
           MOVE 'Customer name not found' TO GBMSG1O.
           MOVE 2 TO RET-CODE.
           GO TO GET-CUST-SSN-EXIT.
       GET-CUST-SSN-EXIT.
           EXIT.
       050-GET-ACCT-BAL.
           EXEC CICS READ
                     DATASET(WBACCTDB-DD)
                     INTO(ACCOUNT-RECORD)
                     LENGTH(LENGTH OF ACCOUNT-RECORD)
                     KEYLENGTH(LENGTH OF ACCT-REC-KEY)
                     RIDFLD(NUM)
                     RESP(RESP-CODE)
           END-EXEC.
           IF RET-CODE = 0
               MOVE BALANCE TO ACCTBALO
               EXIT PROGRAM
           ELSE
               MOVE 'Account not found' TO GBMSG1O
               MOVE 3 TO RET-CODE
               GO TO FORMAT-ERROR-MSG
           END-IF.
       GET-ACCT-BAL-EXIT.
           EXIT.
       060-SET-MAP-DEFAULTS.
           INITIALIZE WGRVMGBO.
           MOVE SPACES TO WGRVMGBO.
           MOVE 'WGRV' TO GBTRANO.
       SET-MAP-DEFAULTS-EXIT.
           EXIT.