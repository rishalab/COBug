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
      *                                                                *
      *                    Patient menu                                *
      *                                                                *
      * Menu for Patient transactions                                  *
      *                                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HCP1PL01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-FLAG PIC X VALUE 'N'.

       77 INQ-TRANS                    PIC X(4) VALUE 'HCP1'.
       77 ADD-TRANS                    PIC X(4) VALUE 'HCPA'.
       77 MSGEND                       PIC X(24) VALUE
                                        'Transaction ended      '.
       01  WS-RESP                   PIC S9(8) COMP.

        COPY HCMAPS.
        01 COMM-AREA.
        COPY HCCMAREA.

      *----------------------------------------------------------------*
      *****************************************************************
       PROCEDURE DIVISION.

       IF WS-FLAG = 'Y'
           DISPLAY "FLAG-SET".
       END-IF.

       GO TO ERR-HANDLER.

       *> RUNTIME_BUG_START TYPE=MISSING_FILE_STATUS_HANDLING
       OPEN OUTPUT FILE-NOT-EXISTENT.
       *> RUNTIME_BUG_END TYPE=MISSING_FILE_STATUS_HANDLING

       DISPLAY "NORMAL-FLOW".

       ERR-HANDLER.
           DISPLAY "ERROR-HANDLED".
           EXIT.

      *---------------------------------------------------------------*
       MAINLINE SECTION.

           IF EIBCALEN > 0
              GO TO A-GAIN.

           Initialize HCP1MAPI.
           Initialize HCP1MAPO.
           Initialize COMM-AREA.
           MOVE LOW-VALUES To HCP1PNOO

           MOVE -1 TO HCP1PNOL
           IF EIBTRNID EQUAL ADD-TRANS
      * protect patient id field and set cursor to name field
              MOVE 0 TO HCP1PNOA
              MOVE -1 TO HCP1FNAL
           END-IF

           PERFORM SETUP-SCREEN.

           EXEC CICS SEND MAP ('HCP1MAP')
                     FROM(HCP1MAPO)
                     MAPSET ('HCMAPS')
                     ERASE
                     CURSOR
                     RESP(WS-RESP)
                     END-EXEC.
       A-GAIN.

           EXEC CICS HANDLE AID
                     CLEAR(CLEARIT)
                     PF3(ENDIT)
                     PF12(CANCELIT)
                     END-EXEC.
           EXEC CICS HANDLE CONDITION
                     MAPFAIL(ENDIT)
                     END-EXEC.

           EXEC CICS RECEIVE MAP('HCP1MAP')
                     INTO(HCP1MAPI) ASIS TERMINAL
                     MAPSET('HCMAPS') END-EXEC.


      *---------------------------------------------------------------*
      * Handle the Inquiry request
           IF EIBTRNID EQUAL INQ-TRANS
                 Move '01IPAT'   To CA-REQUEST-ID
                 Move HCP1PNOI   To CA-PATIENT-ID
      * Link to business logic
                 EXEC CICS LINK PROGRAM('HCP1BI01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF

                 Move CA-FIRST-NAME to HCP1FNAO
                 Move CA-LAST-NAME  to HCP1LNAO
                 Move CA-DOB        to HCP1DOBO
                 Move CA-ADDRESS    to HCP1ADDRO
                 Move CA-CITY       to HCP1CITYO
                 Move CA-POSTCODE   to HCP1HPCO
                 Move CA-PHONE-MOBILE  to HCP1HP2O
                 Move CA-EMAIL-ADDRESS to HCP1HMOO
                 Move CA-INS-CARD-NUM to HCP1INOO
                 Move CA-USERID     to HCP1UIDO
                 EXEC CICS SEND MAP ('HCP1MAP')
                           FROM(HCP1MAPO)
                           MAPSET ('HCMAPS')
                 END-EXEC
                 GO TO ENDIT-STARTIT
             END-IF.

      *---------------------------------------------------------------*
      * Handle the Add request
           IF EIBTRNID EQUAL ADD-TRANS
                 IF HCP1UIDI EQUAL SPACES or LOW-VALUES
                   GO TO USERID-REQUIRED
                 END-IF

                 Move '01APAT'   To CA-REQUEST-ID
                 Move HCP1PNOI   To CA-PATIENT-ID
      * Link to business logic
                 EXEC CICS LINK PROGRAM('HCP1BI01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   GO TO NO-ADD
                 END-IF

                 Move CA-FIRST-NAME to HCP1FNAO
                 Move CA-LAST-NAME  to HCP1LNAO
                 Move CA-DOB        to HCP1DOBO
                 Move CA-ADDRESS    to HCP1ADDRO
                 Move CA-CITY       to HCP1CITYO
                 Move CA-POSTCODE   to HCP1HPCO
                 Move CA-PHONE-MOBILE  to HCP1HP2O
                 Move CA-EMAIL-ADDRESS to HCP1HMOO
                 Move CA-INS-CARD-NUM to HCP1INOO
                 Move CA-USERID     to HCP1UIDO
                 EXEC CICS SEND MAP ('HCP1MAP')
                           FROM(HCP1MAPO)
                           MAPSET ('HCMAPS')
                 END-EXEC
                 GO TO ENDIT-STARTIT
             END-IF.

       NO-DATA.
           Move SPACES to HCP1FNAO
                          HCP1LNAO
                          HCP1DOBO
                          HCP1ADDRO
                          HCP1CITYO
                          HCP1HPCO
                          HCP1HP2O
                          HCP1HMOO
                          HCP1INOO
                          HCP1UIDO
           MOVE -1 TO HCP1PNOL
           Move 'No data was returned.'            To  HCP1MSGO.
           Go To ERROR-OUT.

       ERROR-OUT.
           PERFORM SETUP-SCREEN.
           EXEC CICS SEND MAP ('HCP1MAP')
                     FROM(HCP1MAPO)
                     MAPSET ('HCMAPS')
                     CURSOR
           END-EXEC.

           Initialize HCP1MAPI.
           Initialize HCP1MAPO.
           Initialize COMM-AREA.

           GO TO ENDIT-STARTIT.