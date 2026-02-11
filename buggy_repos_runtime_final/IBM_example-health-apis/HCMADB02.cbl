******************************************************************
      * Copyright 2017 IBM Corp. All Rights Reserved.
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
      * ADD Prescription Details   - changed feb 27 2017
      *  Look for %regi for fixes on abends due missing date
      *
      *  To add patient's name, address and date of birth to the       *
      *  DB2 patient table creating a new patient entry.               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HCMADB02.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'HCMADB02------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-DFHCOMMAREA      USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.

      *
       01  WS-RESP                   PIC S9(8) COMP.
      *----------------------------------------------------------------*
       COPY HCERRSWS.
      *----------------------------------------------------------------*
      * Definitions required for data manipulation                     *
      *----------------------------------------------------------------*
      * Fields to be used to check that commarea is correct length
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADER-LEN         PIC S9(4) COMP VALUE +18.
           03 WS-REQUIRED-CA-LEN       PIC S9(4)      VALUE +0.

      *----------------------------------------------------------------*
       01  WS-NUM-DATE-FIELDS.
             05  WS-WORKING-DATE          PIC  9(8).
             05  WS-START-NUM-DATE.
                 10  WS-START-NUM-YEAR    PIC  9(4).
                 10  WS-START-NUM-MONTH   PIC  9(2).
                 10  WS-START-NUM-DAY     PIC  9(2).
             05  WS-START-NUM-TIME.
                 10  WS-START-NUM-HOUR    PIC  9(2).
                 10  WS-START-NUM-MINUTE  PIC  9(2).
                 10  WS-START-NUM-SECOND  PIC  9(2).
                 10  WS-START-NUM-MS      PIC  9(2).
             05  WS-END-NUM-DATE.
                 10  WS-END-NUM-YEAR    PIC  9(4).
                 10  WS-END-NUM-MONTH   PIC  9(2).
                 10  WS-END-NUM-DAY     PIC  9(2).
             05  WS-END-NUM-TIME.
                 10  WS-END-NUM-HOUR    PIC  9(2).
                 10  WS-END-NUM-MINUTE  PIC  9(2).
                 10  WS-END-NUM-SECOND  PIC  9(2).
                 10  WS-END-NUM-MS      PIC  9(2).
             05  WS-INTEGER-START-DATE  PIC S9(9) COMP.
             05  WS-INTEGER-END-DATE    PIC S9(9) COMP.
      *----------------------------------------------------------------*
       COPY HCERRSPD.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           DISPLAY 'Starting HCMADB02'.

           PERFORM MAINLINE
               UNTIL FALSE.

           STOP RUN.

      *----------------------------------------------------------------*
       MAINLINE.
           *> Initialize variables
           MOVE 0 TO WS-RESP.

           *> Process the input data
           *> ...

           *> Perform calculations
           *> ...

           *> Return to caller
           EXEC CICS RETURN END-EXEC.

      *----------------------------------------------------------------*
       COPY HCERRSPD.