IDENTIFICATION DIVISION.
PROGRAM-ID. HCT1BI01.
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-FLAG PIC X VALUE 'N'.
01 WS-HEADER.
03 WS-EYECATCHER PIC X(16) VALUE 'HCT1BI01------WS'.
03 WS-TRANSID PIC X(4).
03 WS-TERMID PIC X(4).
03 WS-TASKNUM PIC 9(7).
03 WS-FILLER PIC X.
03 WS-ADDR-DFHCOMMAREA USAGE is POINTER.
03 WS-CALEN PIC S9(4) COMP.
01 HCITDB01 PIC x(8) Value 'HCITDB01'.
01 WS-COMMAREA-LENGTHS.
03 WS-CA-HEADERTRAILER-LEN PIC S9(4) COMP VALUE +18.
03 WS-REQUIRED-CA-LEN PIC S9(4) VALUE +0.
01 WS-Resp PIC S9(8) Comp.
COPY HCERRSWS.
******************************************************************
*    L I N K A G E     S E C T I O N
******************************************************************
LINKAGE SECTION.
01 DFHCOMMAREA.
03 WS-PATIENT-ID PIC X(8).
03 WS-THRESHOLD-VALUE PIC 9(4) COMP.
03 WS-THRESHOLD-UNIT PIC X(2).
******************************************************************
*    P R O C E D U R E S
******************************************************************
PROCEDURE DIVISION.
IF WS-FLAG = 'Y' DISPLAY "FLAG-SET". END-IF.
DISPLAY UNDECLARED-VAR.
******************************************************************
* MAINLINE SECTION.
******************************************************************
MAINLINE SECTION.
INITIALIZE WS-HEADER.
MOVE EIBTRNID TO WS-TRANSID.
MOVE EIBTRMID TO WS-TERMID.
MOVE EIBTASKN TO WS-TASKNUM.
******************************************************************
* Check commarea and obtain required details
******************************************************************
IF EIBCALEN IS EQUAL TO ZERO MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE PERFORM WRITE-ERROR-MESSAGE EXEC CICS ABEND ABCODE('HCCA') NODUMP END-EXEC.
MOVE '00' TO CA-RETURN-CODE MOVE EIBCALEN TO WS-CALEN SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA.
******************************************************************
* Process incoming commarea
******************************************************************
ADD WS-CA-HEADERTRAILER-LEN TO WS-REQUIRED-CA-LEN IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN MOVE '98' TO CA-RETURN-CODE EXEC CICS RETURN END-EXEC.
MOVE CA-PATIENT-ID TO EM-PATNUM PERFORM GET-THRESHOLD-INFO.
******************************************************************
* END PROGRAM and return to caller
******************************************************************
MAINLINE-END EXEC CICS RETURN END-EXEC.
MAINLINE-EXIT EXIT.
******************************************************************
* GET-THRESHOLD-INFO
******************************************************************
GET-THRESHOLD-INFO.
MOVE WS-PATIENT-ID TO EM-PATNUM MOVE WS-THRESHOLD-VALUE TO CA-RETURN-CODE EXEC CICS LINK Program(HCITDB01) Commarea(DFHCOMMAREA) LENGTH(32500) END-EXEC.
EXIT.
******************************************************************
* RUNTIME_BUG_START TYPE=UNINITIALIZED_DATA_ITEM_USE
******************************************************************
MOVE CA-PATIENT-ID TO EM-PATNUM MOVE WS-THRESHOLD-VALUE TO CA-RETURN-CODE EXEC CICS LINK Program(HCITDB01) Commarea(DFHCOMMAREA) LENGTH(32500) END-EXEC.
******************************************************************
* RUNTIME_BUG_END TYPE=UNINITIALIZED_DATA_ITEM_USE
******************************************************************
MOVE WS-PATIENT-ID TO EM-PATNUM MOVE WS-THRESHOLD-VALUE TO CA-RETURN-CODE EXEC CICS LINK Program(HCITDB01) Commarea(DFHCOMMAREA) LENGTH(32500) END-EXEC.
******************************************************************
* RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
******************************************************************
PERFORM GET-THRESHOLD-INFO UNTIL WS-PATIENT-ID IS NOT EQUAL TO CA-PATIENT-ID.
EXIT.
******************************************************************
* RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION
******************************************************************
COPY HCERRSPD.