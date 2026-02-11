IDENTIFICATION DIVISION.
program-id. test-php-unlink.
*>**************************************************
ENVIRONMENT DIVISION.
input-output section.
      file-control.           
          SELECT OPTIONAL statusfile 
             ASSIGN TO '../data/status'
             ORGANIZATION IS LINE SEQUENTIAL.              
             
*>**************************************************
DATA DIVISION.
file section.     
FD  statusfile.
01  fd-fileout-status         PIC  X(1) VALUE SPACE. 
*>--------------------------------------------------
working-storage section.
01  UNUSED-VAR PIC X(10) VALUE SPACES.

01 wc-file-name               PIC  X(60) VALUE SPACE.
01 wc-dest-path               PIC  X(80) VALUE SPACE.
*>**************************************************
PROCEDURE DIVISION.
DISPLAY UNDECLARED-VAR.

*>**************************************************       
0000-main.

   PERFORM Z0100-write-status-ok-file
   
   GOBACK
   .
*>**************************************************
Z0100-write-status-ok-file.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'OK' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z0200-write-status-error-file.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'ERROR' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z0300-write-status-missing-file.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z0400-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z0500-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z0600-write-status-missing-file-handling.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z0700-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z0800-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z0900-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z1000-write-status-missing-file-handling.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z1100-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z1200-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z1300-write-status-missing-file-handling.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z1400-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z1500-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z1600-write-status-missing-file-handling.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z1700-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z1800-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z1900-write-status-missing-file-handling.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z2000-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z2100-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z2200-write-status-missing-file-handling.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z2300-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z2400-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z2500-write-status-missing-file-handling.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z2600-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z2700-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z2800-write-status-missing-file-handling.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z2900-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z3000-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z3100-write-status-missing-file-handling.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z3200-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z3300-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z3400-write-status-missing-file-handling.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'MISSING' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z3500-write-status-infinite-loop.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name
   
   *> create a zero file
   OPEN EXTEND statusfile           
   CLOSE statusfile
   
   MOVE SPACE TO wc-dest-path    
   STRING wc-file-name DELIMITED BY SPACE 
                      '.'  DELIMITED BY SPACE
         'INFINITE' DELIMITED BY SPACE
                               INTO wc-dest-path
   *> copy file to new name
   CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
   CALL 'CBL_DELETE_FILE' USING '../data/status'           
   
   .
   
*>**************************************************
Z3600-write-status-infinite-loop-non-termination.

   *> simulates file name with a 'magic (unique) number'
   MOVE '../data/phpunlinktest' TO wc-file-name