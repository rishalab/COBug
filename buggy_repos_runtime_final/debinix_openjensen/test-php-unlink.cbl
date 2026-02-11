*>
       *> creates a file (0-byte) for delete tests
       *> from php with the php unlink() function
       *>
       *> Coder: Bk
       *>
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
           
           *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
           PERFORM Z0100-write-status-ok-file
           *> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION
           
           MOVE SPACE TO wc-dest-path    
           STRING wc-file-name DELIMITED BY SPACE 
                          '.'  DELIMITED BY SPACE
              'OK' DELIMITED BY SPACE
                               INTO wc-dest-path
           *> copy file to new name
           CALL 'CBL_COPY_FILE' USING '../data/status', wc-dest-path
           
           *> RUNTIME_BUG_START TYPE=MISSING_FILE_STATUS_HANDLING
           OPEN INPUT statusfile
           *> RUNTIME_BUG_END TYPE=MISSING_FILE_STATUS_HANDLING
           
           CALL 'CBL_DELETE_FILE' USING '../data/status'           
       
           .
           

       *>**************************************************