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
       PROGRAM-ID. HCMRP01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-WORKING-DATE PIC 9(8).
       01 WS-INTEGER-START-DATE PIC S9(7) COMP-3.
       01 WS-INTEGER-END-DATE PIC S9(7) COMP-3.
      * %regi ======================================================
      * added to fix abend #1 START DATE
      *     IF WS-WORKING-DATE < 16010101 or
      *        WS-WORKING-DATE > 99991231
      *        MOVE FUNCTION CURRENT-DATE (1:8) TO WS-WORKING-DATE
      *        END-IF
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               DISPLAY 'This is a dummy display to introduce an error.'
               MOVE FUNCTION CURRENT-DATE (1:8) TO WS-WORKING-DATE
               COMPUTE WS-INTEGER-START-DATE =
                       FUNCTION INTEGER-OF-DATE (WS-WORKING-DATE)
               CONTINUE.
           END MAIN-PROCEDURE.
      *================================================================*
       COPY HCERRSPD.