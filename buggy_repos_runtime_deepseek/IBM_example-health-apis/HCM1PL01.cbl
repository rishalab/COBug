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
      *                    Patient Prescription Menu                   *
      *                                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
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

       DISPLAY UNDECLARED-VAR. *> RUNTIME_BUG_START TYPE=MISSING_FILE_STATUS_HANDLING