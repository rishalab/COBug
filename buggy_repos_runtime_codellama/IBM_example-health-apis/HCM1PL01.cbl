IDENTIFICATION DIVISION.
       PROGRAM-ID. HCM1PL01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       77 MSGEND                       PIC X(24) VALUE
                                        'Transaction ended      '.

       COPY HCMAPS.

       01 COMM-AREA.
       COPY HCCMARE2.
       01 PATIENT-COMM-AREA.
           03 PCA-REQUEST-ID            PIC X(6).
           03 PCA-RETURN-CODE           PIC 9(2).
           03 PCA-PATIENT-ID            PIC 9(10).
           03 PCA-REQUEST-SPECIFIC      PIC X(32482).
      *    Fields used in INQ All and ADD patient
           03 CA-PATIENT-REQUEST REDEFINES PCA-REQUEST-SPECIFIC.
              05 CA-INS-CARD-NUM       PIC X(10).
              05 CA-FIRST-NAME         PIC X(10).
              05 CA-LAST-NAME          PIC X(20).
              05 CA-DOB                PIC X(10).
              05 CA-ADDRESS            PIC X(20).
              05 CA-CITY               PIC X(20).
              05 CA-POSTCODE           PIC X(10).
              05 CA-PHONE-MOBILE       PIC X(20).
              05 CA-EMAIL-ADDRESS      PIC X(50).
              05 CA-USERID             PIC X(10).
              05 CA-ADDITIONAL-DATA    PIC X(32302).

      *----------------------------------------------------------------*
      *****************************************************************
       PROCEDURE DIVISION.

       DISPLAY UNDECLARED-VAR.

      *---------------------------------------------------------------*
       MAINLINE SECTION.

           IF EIBCALEN > 0
              GO TO A-GAIN.

           Initialize HCM1MAPI.
           Initialize HCM1MAPO.
           Initialize PATIENT-COMM-AREA.
      *    Initialize COMM-AREA.
           Move LOW-VALUES To HCM1CNOI.
           Move SPACES to CA-REQUEST-ID.
           Move zeroes to CA-RETURN-CODE.
           Move zeroes to CA-PATIENT-ID.
           Move zeroes to CA-NUM-MEDICATIONS.

           EXEC CICS RETURN
                TRANSID('HCM1')
                COMMAREA(COMM-AREA)
                END-EXEC.

       A-GAIN.
           Move '01IPAT'   To PCA-REQUEST-ID
           Move HCM1CNOI   To PCA-PATIENT-ID
           EXEC CICS LINK PROGRAM('HCP1BI01')
                     COMMAREA(PATIENT-COMM-AREA)
                     LENGTH(32500)
           END-EXEC

      *    Introducing a runtime bug by using an undefined variable
           Move UNDEFINED-VAR To HCM1FNAI
           Move CA-LAST-NAME  to HCM1LNAI.

       CLEARIT.

           Initialize HCM1MAPI.
           EXEC CICS SEND MAP ('HCM1MAP')
                     MAPSET ('HCMAPS')
                     MAPONLY
           END-EXEC

           Move SPACES to CA-REQUEST-ID.
           Move zeroes to CA-RETURN-CODE.
           Move zeroes to CA-PATIENT-ID.
           Move zeroes to CA-NUM-MEDICATIONS.

           EXEC CICS RETURN
                TRANSID('HCM1')
                COMMAREA(COMM-AREA)
                END-EXEC.

       CANCELIT.

           EXEC CICS RETURN
                TRANSID('HCAZ')
                IMMEDIATE
                END-EXEC.

       NOT-IMPLEMENTED.
           Move 'That feature has not been implemented yet.'
                To  HCM1MSGO
           Go To ERROR-OUT.

       NO-PATIENT-DATA.
           Move 'No patient data was returned.'  To  HCM1MSGO
           Go To ERROR-OUT.

       NO-MED-DATA.
           Move 'No medication data was returned.' To  HCM1MSGO
           Go To ERROR-OUT.

       ERROR-OUT.
           EXEC CICS SEND MAP ('HCM1MAP')
                     FROM(HCM1MAPO)
                     MAPSET ('HCMAPS')
           END-EXEC.

           Initialize HCM1MAPI.
           Initialize HCM1MAPO.
           Initialize PATIENT-COMM-AREA.

           GO TO ENDIT-STARTIT.