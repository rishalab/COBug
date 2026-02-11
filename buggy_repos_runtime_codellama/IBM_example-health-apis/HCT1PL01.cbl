IDENTIFICATION DIVISION.
       PROGRAM-ID. HCT1PL01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       77 INQ-TRANS                    PIC X(4) VALUE 'HCT1'.
       77 ADD-TRANS                    PIC X(4) VALUE 'HCTA'.
       77 MSGEND                       PIC X(24) VALUE
                                        'Transaction ended      '.
       01  WS-RESP                   PIC S9(8) COMP.

        COPY HCMAPS.
        01 COMM-AREA.
        COPY HCCMAREA.

      *----------------------------------------------------------------*
      *****************************************************************
       PROCEDURE DIVISION.

      *---------------------------------------------------------------*
       MAINLINE SECTION.

           IF EIBCALEN > 0
              GO TO A-GAIN.

           Initialize HCT1MAPI.
           Initialize HCT1MAPO.
           Initialize COMM-AREA.
           MOVE LOW-VALUES To HCT1PNOO

           MOVE -1 TO HCT1PNOL

           PERFORM SETUP-SCREEN.

           EXEC CICS SEND MAP ('HCT1MAP')
                     FROM(HCT1MAPO)
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

           EXEC CICS RECEIVE MAP('HCT1MAP')
                     INTO(HCT1MAPI) ASIS TERMINAL
                     MAPSET('HCMAPS') END-EXEC.

           PERFORM GET-PATIENT

      *---------------------------------------------------------------*
      * Handle the Inquiry request
           IF EIBTRNID EQUAL INQ-TRANS
                 Move '01ITHR'   To CA-REQUEST-ID
                 Move HCT1PNOI   To CA-PATIENT-ID
      * Link to business logic
                 EXEC CICS LINK PROGRAM('HCT1BI01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF

                 Move CA-HR-THRESHOLD  To HCT1HRTHO
                 Move CA-BP-THRESHOLD  To HCT1BPTHO

                 EXEC CICS SEND MAP ('HCT1MAP')
                           FROM(HCT1MAPO)
                           MAPSET ('HCMAPS')
                 END-EXEC
                 GO TO ENDIT-STARTIT
             END-IF.

      *---------------------------------------------------------------*
      * Handle the Add request
           IF EIBTRNID EQUAL ADD-TRANS
              Move '01IADD'   To CA-REQUEST-ID
              Move HCT1PNOI   To CA-PATIENT-ID
              Move HCT1HRTHO  To CA-HR-THRESHOLD
              Move HCT1BPTHO  To CA-BP-THRESHOLD
      * Link to business logic
                 EXEC CICS LINK PROGRAM('HCT1AI01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   GO TO NO-UPD
                 END-IF

                 Move 'Threshold Added' To HCT1MSGO
                 Go To ERROR-OUT.
             END-IF.

       CLEARIT.

           Initialize HCT1MAPI.
           EXEC CICS SEND MAP ('HCT1MAP')
                     MAPSET ('HCMAPS')
                     MAPONLY
           END-EXEC

           EXEC CICS RETURN
                TRANSID(EIBTRNID)
                COMMAREA(COMM-AREA)
                LENGTH(LENGTH OF COMM-AREA)
                END-EXEC.

       CANCELIT.

           EXEC CICS RETURN
                TRANSID('HCAZ')
                IMMEDIATE
                END-EXEC.

       SETUP-SCREEN.
      * Determine appropriate heading
           IF EIBTRNID EQUAL INQ-TRANS
              MOVE INQ-TRANS TO HCT1TRNO
              MOVE 'Inquire Threshold Information' to HCT1TITO
           ELSE
           IF EIBTRNID EQUAL ADD-TRANS
              MOVE ADD-TRANS TO HCT1TRNO
              MOVE 'Add Threshold Information' to HCT1TITO
           END-IF.

       GET-PATIENT.
      * Get patient name
           Move '01IPAT'   To CA-REQUEST-ID
           Move HCT1PNOI   To CA-PATIENT-ID
           EXEC CICS LINK PROGRAM('HCP1BI01')
                     COMMAREA(COMM-AREA)
                     LENGTH(32500)
           END-EXEC

           IF CA-RETURN-CODE > 0
              GO TO NO-PATIENT-DATA
           END-IF

           Move CA-FIRST-NAME to HCT1FNAI
           Move CA-LAST-NAME  to HCT1LNAI.


       NO-UPD.
           MOVE -1 TO HCT1HRTHL
           Move 'Error Updating Threshold'          To  HCT1MSGO.
           Go To ERROR-OUT.

       NO-ADD.
           MOVE -1 TO HCT1HRTHL
           Move 'Error Adding Threshold'            To  HCT1MSGO.
           Go To ERROR-OUT.

       NO-DATA.
           MOVE -1 TO HCT1PNOL
           Move 'No Threshold data was returned.'   To  HCT1MSGO.
           Go To ERROR-OUT.

       NO-PATIENT-DATA.
           Move 'No patient data was returned.'  To  HCT1MSGO
           Go To ERROR-OUT.

       ERROR-OUT.
           PERFORM SETUP-SCREEN.
           EXEC CICS SEND MAP ('HCT1MAP')
                     FROM(HCT1MAPO)
                     MAPSET ('HCMAPS')
                     CURSOR
           END-EXEC.

           Initialize HCT1MAPI.
           Initialize HCT1MAPO.
           Initialize COMM-AREA.

           GO TO ENDIT-STARTIT.