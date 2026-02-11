IDENTIFICATION DIVISION.
       PROGRAM-ID. WTIADDC.
       ENVIRONMENT DIVISION.

       DATA DIVISION.

      *****************************************************************
      ** VARIABLES FOR INTERACTING WITH THE TERMINAL SESSION          *
      *****************************************************************
       WORKING-STORAGE SECTION.

       01 CUST-REC-KEY.
           05 NAME                        PIC X(30)  VALUE SPACES.

       01 CUSTOMER-RECORD.
           05 CUSTOMER-NAME             PIC X(30).
           05 CUSTOMER-SSN              PIC X(11).

       01 USER-DATA.
           05 CUSTOMER-NAME             PIC X(30).
           05 CUSTOMER-SSN              PIC X(11).

       01 CUST-REC-KEY.
           05 NAME                        PIC X(30)  VALUE SPACES.

       01 RESP-CODE                     PIC S9(8) COMP.

       01 DFHRESP-NOTOPEN               PIC S9(8) COMP VALUE -2.
       01 DFHRESP-DISABLED              PIC S9(8) COMP VALUE -3.
       01 DFHRESP-ENDFILE               PIC S9(8) COMP VALUE -4.
       01 DFHRESP-NOTFND                PIC S9(8) COMP VALUE -5.
       01 DFHRESP-DUPREC                PIC S9(8) COMP VALUE -6.
       01 DFHRESP-DUPKEY                PIC S9(8) COMP VALUE -7.

       01 BSTRHELPSTRING                PIC X(255).

       01 EDIT-NUM                      PIC S9(8) COMP.

       01 LOG-MSG.
           05 LOGGING-IS-ENABLED        PIC X(1).
           05 TASK-NUMBER               PIC S9(8) COMP.
           05 LOG-MSG-BUFFER            PIC X(255).

       PROCEDURE DIVISION.

      **************************************************
      *    CHECK TO SEE IF THE CUSTOMER NAME EXISTS
      **************************************************
       CHECK-CUST-NAME.
           MOVE CUSTOMER-NAME OF USER-DATA TO NAME OF CUST-REC-KEY.
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
                 GO TO CHECK-CUST-NAME-FOUND
              WHEN DFHRESP-NOTOPEN
                 GO TO CHECK-CUST-NAME-NOTOPEN
              WHEN DFHRESP-DISABLED
                 GO TO CHECK-CUST-NAME-NOTOPEN
              WHEN DFHRESP-ENDFILE
                 GO TO CHECK-CUST-NAME-NOTFND
              WHEN DFHRESP-NOTFND
                 GO TO CHECK-CUST-NAME-NOTFND
              WHEN OTHER
                 GO TO CHECK-CUST-NAME-ERROR
           END-EVALUATE.

       CHECK-CUST-NAME-NOTOPEN.
           MOVE 'Customer file not open' TO BSTRHELPSTRING.
           MOVE 5001 TO SERRORCODE RET-CODE.
           GO TO CHECK-CUST-NAME-EXIT.

       CHECK-CUST-NAME-FOUND.
           MOVE 'Customer name already exists' TO BSTRHELPSTRING.
           MOVE 5002 TO SERRORCODE RET-CODE.
           GO TO CHECK-CUST-NAME-EXIT.

       CHECK-CUST-NAME-NOTFND.
           MOVE 0 TO RET-CODE.
           GO TO CHECK-CUST-NAME-EXIT.

       CHECK-CUST-NAME-ERROR.
           MOVE SPACES TO BSTRHELPSTRING.
           MOVE RESP-CODE TO EDIT-NUM.
           STRING 'I/O Error one Customer file, response code='
                            DELIMITED SIZE
                  EDIT-NUM  DELIMITED SIZE
                  INTO BSTRHELPSTRING
           END-STRING.
           MOVE 5003 TO  SERRORCODE RET-CODE.
           GO TO CHECK-CUST-NAME-EXIT.

       CHECK-CUST-NAME-EXIT.
           EXIT.

      **************************************************************
      ** VALIDATE THE INFORMATION IN THE MAP                      **
      **************************************************************
       CHECK-CUST-SSN.
           MOVE LOW-VALUES TO CUST-REC-KEY.
           EXEC CICS STARTBR DATASET(WBCUSTDB-DD)
                     RIDFLD(CUST-REC-KEY)
                     KEYLENGTH(LENGTH OF CUST-REC-KEY)
                     GTEQ
                     RESP(RESP-CODE)
           END-EXEC.

           EVALUATE RESP-CODE
              WHEN 0
                 CONTINUE
              WHEN OTHER
                 GO TO CHECK-CUST-SSN-ERROR-SB
           END-EVALUATE.

       CHECK-CUST-SSN-NEXT.
           EXEC CICS READNEXT
                     DATASET(WBCUSTDB-DD)
                     INTO(CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CUSTOMER-RECORD)
                     RIDFLD(CUST-REC-KEY)
                     KEYLENGTH(LENGTH OF CUST-REC-KEY)
                     RESP(RESP-CODE)
           END-EXEC.

           EVALUATE RESP-CODE
              WHEN 0
                 IF CUSTOMER-SSN OF CUSTOMER-RECORD NOT =
                    CUSTOMER-SSN OF USER-DATA THEN
                    GO TO CHECK-CUST-SSN-NEXT
                 ELSE
                    EXEC CICS ENDBR DATASET(WBCUSTDB-DD) END-EXEC
                    MOVE 'Duplicate SSN found' TO BSTRHELPSTRING
                    MOVE 5003 TO  SERRORCODE RET-CODE
                    GO TO CHECK-CUST-SSN-EXIT
                 END-IF
              WHEN DFHRESP-NOTOPEN
                 MOVE 'Customer file not open' TO BSTRHELPSTRING
                 MOVE 5004 TO  SERRORCODE RET-CODE
                 GO TO CHECK-CUST-SSN-EXIT
              WHEN DFHRESP-ENDFILE
                 EXEC CICS ENDBR DATASET(WBCUSTDB-DD) END-EXEC
                 MOVE 0 TO RET-CODE
                 GO TO CHECK-CUST-SSN-EXIT
              WHEN OTHER
                 GO TO CHECK-CUST-SSN-ERROR
           END-EVALUATE.
           GO TO CHECK-CUST-SSN-EXIT.

       CHECK-CUST-SSN-ERROR.
           EXEC CICS ENDBR DATASET(WBCUSTDB-DD) END-EXEC.
           MOVE SPACES TO BSTRHELPSTRING.
           MOVE RESP-CODE TO EDIT-NUM.
           STRING 'I/O Error on Customer file: Repsonse Code='
                  DELIMITED SIZE
                  EDIT-NUM DELIMITED SIZE
                  INTO BSTRHELPSTRING
           END-STRING.
           MOVE BSTRHELPSTRING TO LOG-MSG-BUFFER.
           PERFORM WRITE-LOG-MSG THRU WRITE-LOG-MSG-EXIT.
           MOVE 5005 TO  SERRORCODE RET-CODE
           GO TO CHECK-CUST-SSN-EXIT.

       CHECK-CUST-SSN-ERROR-SB.
           MOVE SPACES TO BSTRHELPSTRING.
           MOVE RESP-CODE TO EDIT-NUM.
           STRING 'I/O Error on startbr Customer file: Repsonse Code='
                  DELIMITED SIZE
                  EDIT-NUM DELIMITED SIZE
                  INTO BSTRHELPSTRING
           END-STRING.
           MOVE BSTRHELPSTRING TO LOG-MSG-BUFFER.
           PERFORM WRITE-LOG-MSG THRU WRITE-LOG-MSG-EXIT.
           MOVE 5006 TO SERRORCODE RET-CODE.
           GO TO CHECK-CUST-SSN-EXIT.

       CHECK-CUST-SSN-EXIT.
           EXIT.

       ADD-CUST.
      **************************************************
      *    ADD THE CUSTOMER RECORD TO THE VSAM DATA SET
      **************************************************
           MOVE CORRESPONDING USER-DATA TO CUSTOMER-RECORD.
           MOVE CUSTOMER-NAME OF CUSTOMER-RECORD TO
                NAME OF CUST-REC-KEY.
           EXEC CICS WRITE
                     DATASET(WBCUSTDB-DD)
                     FROM(CUSTOMER-RECORD)
                     LENGTH(LENGTH OF CUSTOMER-RECORD)
                     KEYLENGTH(LENGTH OF CUST-REC-KEY)
                     RIDFLD(CUST-REC-KEY)
                     RESP(RESP-CODE)
           END-EXEC.

           EVALUATE RESP-CODE
              WHEN 0
                 MOVE 0 TO RET-CODE
                 GO TO ADD-CUST-EXIT
              WHEN DFHRESP-NOTOPEN
                 MOVE 'Customer file not open' TO BSTRHELPSTRING
                 MOVE 5008 TO SERRORCODE RET-CODE
                 GO TO ADD-CUST-EXIT
              WHEN DFHRESP-DUPREC
                 GO TO ADD-CUST-DUPLICATE
              WHEN DFHRESP-DUPKEY
                 GO TO ADD-CUST-DUPLICATE
              WHEN OTHER
                 GO TO ADD-CUST-ERROR
           END-EVALUATE.

           GO TO ADD-CUST-EXIT.

       ADD-CUST-DUPLICATE.
           MOVE 'Customer name already defined' TO BSTRHELPSTRING.
           MOVE 5007 TO SERRORCODE RET-CODE.
           GO TO ADD-CUST-EXIT.

       ADD-CUST-ERROR.
           MOVE 'Error occurred writing the Customer VSAM file'
                 TO BSTRHELPSTRING.
           MOVE RESP-CODE TO EDIT-NUM.
           STRING 'Error occurred writing the Customer VSAM file, '
                                   DELIMITED SIZE
                  'Response code=' DELIMITED SIZE
                  EDIT-NUM         DELIMITED SIZE
                  INTO BSTRHELPSTRING
           END-STRING.
           MOVE 5008 TO SERRORCODE RET-CODE.
           GO TO ADD-CUST-EXIT.

       ADD-CUST-EXIT.
           EXIT.

      **************************************************
      *    WRITE LOG MESSAGE
      **************************************************
       WRITE-LOG-MSG.
           IF LOGGING-IS-ENABLED = 'Y'
               EXEC CICS SEND TEXT FROM(LOG-MSG-BUFFER)
                         LENGTH(LENGTH OF LOG-MSG-BUFFER)
               END-EXEC
           END-IF.
           EXIT.

       MAIN-PROCEDURE.
           PERFORM CHECK-CUST-NAME.
           PERFORM CHECK-CUST-SSN.
           IF RET-CODE = 0
               PERFORM ADD-CUST
           END-IF.
           EXEC CICS RETURN
           END-EXEC.

       END PROGRAM CUSTOMER-MANAGEMENT.