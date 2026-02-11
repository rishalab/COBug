******************************************************************
      * Copyright 2014 IBM Corp. All Rights Reserved.
      *
      * Licensed under the Apache License, Version 2.0 (the "License");
      * you may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      * http://www.apache.org/licenses/LICENSE-2.0
      *
      * Unless required by applicable law or agreed to in writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied.
      * See the License for the specific language governing permissions
      * and limitations under the License.
      ******************************************************************
      *                                                                *
      *                    Inquire Threshold                           *
      *                                                                *
      *   Business Logic to obtain patient threshold                   *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HCT1BI01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  WS-FLAG PIC X VALUE 'N'.
      *> RUNTIME_BUG_START TYPE=UNINITIALIZED_DATA_ITEM_USE
       01  UNDECLARED-VAR PIC X.
      *> RUNTIME_BUG_END TYPE=UNINITIALIZED_DATA_ITEM_USE

       01 HCITDB01                  PIC x(8) Value 'HCITDB01'.
      *----------------------------------------------------------------*
      * Fields to be used to calculate if commarea is large enough
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADERTRAILER-LEN  PIC S9(4) COMP VALUE +18.
           03 WS-REQUIRED-CA-LEN       PIC S9(4)      VALUE +0.

       01  WS-Resp                     PIC S9(8) Comp.
      *----------------------------------------------------------------*
       COPY HCERRSWS.
      ******************************************************************
      *    L I N K A G E     S E C T I O N
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
             COPY HCCMAREA.

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

       IF WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-IF.

      *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
       PERFORM UNTIL WS-FLAG = 'Y'
           CONTINUE
       END-PERFORM.
      *> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION

       DISPLAY UNDECLARED-VAR.

      *----------------------------------------------------------------*
       MAINLINE SECTION.
      *
           INITIALIZE WS-HEADER.
      *
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
      *----------------------------------------------------------------*
      * Check commarea and obtain required details                     *
      *----------------------------------------------------------------*
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('HCCA') NODUMP END-EXEC
           END-IF

           MOVE '00' TO CA-RETURN-CODE
           MOVE EIBCALEN TO WS-CALEN.
           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA.

      *----------------------------------------------------------------*
      * Process incoming commarea                                      *
      *----------------------------------------------------------------*
      * check commarea length
      *    MOVE WS-CUSTOMER-LEN        TO WS-REQUIRED-CA-LEN
           ADD WS-CA-HEADERTRAILER-LEN TO WS-REQUIRED-CA-LEN
           IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
             MOVE '98' TO CA-RETURN-CODE
             EXEC CICS RETURN END-EXEC
           END-IF

           MOVE CA-PATIENT-ID TO EM-PATNUM

           PERFORM GET-THRESHOLD-INFO.

      *----------------------------------------------------------------*
      * END PROGRAM and return to caller                               *
      *----------------------------------------------------------------*
       MAINLINE-END.
           EXEC CICS RETURN END-EXEC.

       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       GET-THRESHOLD-INFO.
      *----------------------------------------------------------------*
      *
             EXEC CICS LINK Program(HCITDB01)
                 Commarea(DFHCOMMAREA)
                 LENGTH(32500)
             END-EXEC.
           EXIT.
      *----------------------------------------------------------------*
       COPY HCERRSPD.