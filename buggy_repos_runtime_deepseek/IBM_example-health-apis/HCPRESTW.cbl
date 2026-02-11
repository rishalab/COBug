IDENTIFICATION DIVISION.
       PROGRAM-ID. YOURPROGRAMID.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESID               PIC X(256).
       01  WS-FIELD1             PIC X(32).
       01  WS-FIELD2             PIC X(32).
       01  WS-FIELD3             PIC X(32).
       01  WS-FIELD4             PIC X(32).
       01  WS-FIELD5             PIC X(32).
       PROCEDURE DIVISION.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GENERIC-ABEND.
           EXEC CICS ABEND
                     ABCODE(UNEXPECTED-RESP-ABCODE)
           END-EXEC.
      ******************************************************************
       WRITE-TSQ.
           EXEC CICS WRITEQ TS QUEUE(WS-TSQ-NAME)
                     FROM(WS-TSQ-DATA)
                     RESP(RESP)
                     NOSUSPEND
                     LENGTH(WS-TSQ-LEN)
           END-EXEC.
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-REQUEST-ROOT-DATA.
           EXEC CICS GET CONTAINER('DFHWS-DATA')
                         INTO(JSON-REST-DATA)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC.
           IF RESP NOT = DFHRESP(NORMAL) THEN
              PERFORM GENERIC-ABEND
           END-IF.
      ******************************************************************
       PUT-RESPONSE-ROOT-DATA.
           EXEC CICS PUT
                     CONTAINER('DFHWS-DATA')
                     FROM (JSON-REST-DATA)
                     RESP(RESP)
                     RESP2(RESP2)
           END-EXEC.
           IF RESP NOT = DFHRESP(NORMAL) THEN
              PERFORM GENERIC-ABEND
           END-IF.
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-RESID.
           MOVE ' ' TO WS-RESID
           EXEC CICS GET CONTAINER('DFHWS-URI-QUERY')
                         INTO(WS-RESID)
                         RESP(RESP)
                         RESP2(RESP2)
           END-EXEC
           IF RESP NOT = DFHRESP(NORMAL) THEN
              DISPLAY 'Cannot get QUERY container.'
           ELSE
              DISPLAY 'QUERY in WS-RESID is:' WS-RESID
           * UNEXPECTED OPERATION: The original code has an unimplemented operation here.
           END-IF
      ******************************************************************
       GET-