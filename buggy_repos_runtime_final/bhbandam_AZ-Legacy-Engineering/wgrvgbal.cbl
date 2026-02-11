*****************************************************************
     ** Copyright (c) Microsoft Corporation.                         * 	
     ** Licensed under the MIT license.                              * 
     **                                                              *
     ** THIS PROGRAM IS A SAMPLE CICS CLIENT FOR DEMONSTRATING A 3270*
     ** APPLICATION THAT READS AND WRITE TO A VSAM DATA SET FOR      * 
     ** BANKING TYPE OF INFORMATION.                                 *
     **                                                              *
     ** THE INPUT TO THIS CICS PROGRAM IS PROVIDED THROUGH A BMS MAP *
     ** THAT IS NAMED WGRVMAP.                                       *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WGRVGBAL.
       ENVIRONMENT DIVISION.

       DATA DIVISION.

      *****************************************************************
      ** VARIABLES FOR INTERACTING WITH THE TERMINAL SESSION          *
      *****************************************************************
       WORKING-STORAGE SECTION.

       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       01 CUST-REC-KEY.
           05 NAME                        PIC X(30)  VALUE SPACES.

       01  CUSTOMER-RECORD.
           05 CUSTOMER-NAME               PIC X(30).
           05 CUSTOMER-SSN                PIC X(9).
           05 CUSTOMER-ADDRESS.
              10 CUSTOMER-STREET          PIC X(20).
              10 CUSTOMER-CITY            PIC X(10).
              10 CUSTOMER-STATE           PIC X(4).
              10 CUSTOMER-ZIP             PIC 9(5).
           05 CUSTOMER-PHONE              PIC X(13).
           05 CUSTOMER-ACCESS-PIN         PIC X(4).

       01 ACCT-REC-KEY.
           05 SSN                         PIC X(9).
           05 NUM                         PIC X(10).

       01  ACCOUNT-RECORD.
           05 ACCOUNT-NUMBER              PIC X(10).
           05 ACCOUNT-BALANCE             PIC S9(18)V99 COMP-3.

       01 GBTRANO                       PIC X(4).
       01 NAMEO                         PIC X(30).
       01 ACCTNUMO                      PIC X(10).
       01 ACCTBALO                      PIC S9(18)V99.
       01 GBMSG1O                       PIC X(50).
       01 GBMSG2O                       PIC X(50).

       01 NAMEI                         PIC X(30).
       01 ACCTNUML                      PIC 9(4).
       01 ACCTNUMI                      PIC X(10).

       01 RESP-CODE                     PIC S9(8) COMP.
       01 EDIT-NUM                      PIC S9(8) COMP.

       01 LOG-MSG.
           05 FILLER                      PIC X(4).
           05 TASK-NUMBER                 PIC 9(8).
           05 MESSAGE                     PIC X(50).

       PROCEDURE DIVISION.

       MAIN-PARA.
           PERFORM SET-MAP-DEFAULTS
           PERFORM UNTIL DONE
               EXEC CICS RECEIVE MAP('WGRVMGMI')
                               INTO(NAMEI ACCTNUML ACCTNUMI)
                               LENGTH(LENGTH OF NAMEI ACCTNUML ACCTNUMI)
                               END-EXEC
               IF NAMEL = 0 OR NAMEI = SPACES
                   MOVE 'Name is invalid' TO GBMSG1O
                   PERFORM WRITE-LOG-MSG
                   GO TO MAIN-PARA
               END-IF
               MOVE NAMEI TO NAME OF CUST-REC-KEY
               EXEC CICS READ DATASET('WBCUSTDB-DD')
                               INTO(CUSTOMER-RECORD)
                               LENGTH(LENGTH OF CUSTOMER-RECORD)
                               KEYLENGTH(LENGTH OF CUST-REC-KEY)
                               RIDFLD(CUST-REC-KEY)
                               RESP(RESP-CODE)
               END-EXEC
               EVALUATE RESP-CODE
                   WHEN 0
                       MOVE CUSTOMER-SSN TO SSN OF ACCT-REC-KEY
                       EXEC CICS READ DATASET('WBACCTDB-DD')
                                   INTO(ACCOUNT-RECORD)
                                   LENGTH(LENGTH OF ACCOUNT-RECORD)
                                   KEYLENGTH(LENGTH OF ACCT-REC-KEY)
                                   RIDFLD(ACCT-REC-KEY)
                                   RESP(RESP-CODE)
                       END-EXEC
                       EVALUATE RESP-CODE
                           WHEN 0
                               MOVE FUNCTION NUMVAL(ACCOUNT-BALANCE) TO ACCTBALO
                               PERFORM WRITE-LOG-MSG
                               EXEC CICS SEND MAP('WGRVMGBO')
                                           FROM(GBTRANO NAMEO ACCTNUMO ACCTBALO GBMSG1O GBMSG2O)
                                           LENGTH(LENGTH OF GBTRANO NAMEO ACCTNUMO ACCTBALO GBMSG1O GBMSG2O)
                                           END-EXEC
                           WHEN OTHER
                               MOVE 'I/O error reading the Account VSAM file' TO GBMSG1O
                               PERFORM WRITE-LOG-MSG
                               EXEC CICS SEND MAP('WGRVMGBO')
                                           FROM(GBTRANO NAMEO ACCTNUMO ACCTBALO GBMSG1O GBMSG2O)
                                           LENGTH(LENGTH OF GBTRANO NAMEO ACCTNUMO ACCTBALO GBMSG1O GBMSG2O)
                                           END-EXEC
                       END-EVALUATE
                   WHEN OTHER
                       MOVE 'I/O error reading the Customer VSAM file' TO GBMSG1O
                       PERFORM WRITE-LOG-MSG
                       EXEC CICS SEND MAP('WGRVMGBO')
                                   FROM(GBTRANO NAMEO ACCTNUMO ACCTBALO GBMSG1O GBMSG2O)
                                   LENGTH(LENGTH OF GBTRANO NAMEO ACCTNUMO ACCTBALO GBMSG1O GBMSG2O)
                                   END-EXEC
               END-EVALUATE
           END-PERFORM.

       SET-MAP-DEFAULTS.
           MOVE 'WBGB' TO GBTRANO.
           MOVE SPACES TO NAMEO.
           MOVE SPACES TO ACCTNUMO.
           MOVE 0      TO ACCTBALO.
           MOVE SPACES TO GBMSG1O.
           MOVE SPACES TO GBMSG2O.

       WRITE-LOG-MSG.
           IF LOGGING-IS-ENABLED THEN
              MOVE LENGTH OF LOG-MSG TO HW-LENGTH
              MOVE EIBTASKN          TO TASK-NUMBER
              EXEC CICS WRITEQ TD QUEUE('CSMT')
                                  FROM(LOG-MSG)
                                  LENGTH(HW-LENGTH)
                                  NOHANDLE
                                  END-EXEC
           END-IF.

       END-WGRVGBAL.