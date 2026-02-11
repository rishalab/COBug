IDENTIFICATION DIVISION.
       PROGRAM-ID. cgi-list-users.
       *>**************************************************
       DATA DIVISION.
       *>**************************************************
       WORKING-STORAGE SECTION.
       01  WS-FLAG PIC X VALUE 'N'.

       *>**************************************************
       01 Switches.
           05  is-valid-post-switch            PIC X   VALUE 'N'.
               88  is-valid-post                       VALUE 'Y'.
           05  is-db-connected-switch          PIC X   VALUE 'N'.
               88  is-db-connected                     VALUE 'Y'.
           05  is-valid-init-switch            PIC X   VALUE 'N'.
               88 is-valid-init                        VALUE 'Y'.

       *>**************************************************
       *> SQL Copybooks

       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  wc-database                 PIC  X(30) VALUE SPACE.
       01  wc-passwd                   PIC  X(10) VALUE SPACE.
       01  wc-username                 PIC  X(30) VALUE SPACE.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  t-user-id                  PIC 9(5).
       01  t-user-firstname           PIC X(50).
       01  t-user-lastname            PIC X(50).
       01  t-user-email               PIC X(100).
       01  t-user-phonenumber         PIC X(20).
       01  t-user-usertype-id         PIC 9(3).
       01  t-user-program-id          PIC 9(3).
       01  t-user-lastlogin           PIC X(50).
       EXEC SQL END DECLARE SECTION END-EXEC.

       *>**************************************************
       *> Utility variables
       01  idx-program                PIC S9(4) COMP VALUE 0.
       01  idx-user-type              PIC S9(4) COMP VALUE 0.
       01  wn-user-id-edit            PIC 9(5).
       *>**************************************************

       *>**************************************************
       *> Lookup tables
       01  tbl-program-name           OCCURS 10 TIMES INDEXED BY idx-program.
           03  program-name             PIC X(50).

       01  tbl-user-type-name         OCCURS 10 TIMES INDEXED BY idx-user-type.
           03  user-type-name           PIC X(50).
       *>**************************************************

       *> HTML constants
       01  html-table-row-start       PIC X(20) VALUE '<tr>'.
       01  html-table-cell-start      PIC X(20) VALUE '<td>'.
       01  html-table-cell-end        PIC X(5) VALUE '</td>'.
       01  html-table-row-end         PIC X(5) VALUE '</tr>'.
       *>**************************************************

       *>**************************************************
       *> Procedures
       *>**************************************************
       PROCEDURE DIVISION.
           DISPLAY 'Starting program'.

           PERFORM A0000-Initialize.

           PERFORM B0000-Main.

           PERFORM C0000-Cleanup.

           STOP RUN.
       *>**************************************************

       *>**************************************************
       *> Initialize procedure
       *>**************************************************
       A0000-Initialize.
           DISPLAY 'Initializing program'.
           *> Additional initialization code can be added here
           .
       *>**************************************************

       *>**************************************************
       *> Main procedure
       *>**************************************************
       B0000-Main.
           DISPLAY 'Main procedure'.

           PERFORM B0100-Connect.

           IF is-db-connected
               PERFORM B0200-Get-Lookup-Data.
               PERFORM B0300-List-Users.
               PERFORM Z0200-Disconnect.
           END-IF.
       *>**************************************************

       *>**************************************************
       *> Connect to database procedure
       *>**************************************************
       B0100-Connect.
           DISPLAY 'Connecting to database'.

           MOVE  "openjensen"    TO   wc-database.
           MOVE  "jensen"        TO   wc-username.
           MOVE  SPACE           TO   wc-passwd.

           EXEC SQL
               CONNECT :wc-username IDENTIFIED BY :wc-passwd
                                    USING :wc-database
           END-EXEC

           IF SQLSTATE NOT = '00000'
               PERFORM Z0100-Error-Routine.
           ELSE
               SET is-db-connected TO TRUE.
           END-IF.
       *>**************************************************

       *>**************************************************
       *> Get lookup data procedure
       *>**************************************************
       B0200-Get-Lookup-Data.
           DISPLAY 'Getting lookup data'.

           PERFORM B0210-Get-Program-Names.

           PERFORM B0220-Get-User-Type-Names.
       *>**************************************************

       *>**************************************************
       *> Get program names procedure
       *>**************************************************
       B0210-Get-Program-Names.
           DISPLAY 'Getting program names'.

           EXEC SQL
               DECLARE cur4 CURSOR FOR
                  SELECT  program_id, program_name
                  FROM tbl_program
                  ORDER BY program_id
           END-EXEC

           EXEC SQL
                OPEN cur4
           END-EXEC

           EXEC SQL
               FETCH cur4 INTO
                   :t-program-id,
                   :t-program-name
           END-EXEC

           SET idx-program TO 1.

           PERFORM UNTIL SQLSTATE NOT = '00000'
                MOVE t-program-name TO tbl-program-name(idx-program).
                SET idx-program UP BY 1.

                EXEC SQL
                    FETCH cur4 INTO
                        :t-program-id,
                        :t-program-name
                END-EXEC
           END-PERFORM

           EXEC SQL
                CLOSE cur4
           END-EXEC.
       *>**************************************************

       *>**************************************************
       *> Get user type names procedure
       *>**************************************************
       B0220-Get-User-Type-Names.
           DISPLAY 'Getting user type names'.

           EXEC SQL
                DECLARE cur5 CURSOR FOR
                   SELECT usertype_id, usertype_name
                   FROM tbl_usertype
                   ORDER BY usertype_id
           END-EXEC

           EXEC SQL
                OPEN cur5
           END-EXEC

           EXEC SQL
                FETCH cur5 INTO
                    :t-usertype-id,
                    :t-usertype-name
           END-EXEC

           SET idx-user-type TO 1.

           PERFORM UNTIL SQLSTATE NOT = '00000'
                MOVE t-usertype-name TO tbl-user-type-name(idx-user-type).

                SET idx-user-type UP BY 1.

                EXEC SQL
                    FETCH cur5 INTO
                        :t-usertype-id,
                        :t-usertype-name
                END-EXEC
           END-PERFORM

           EXEC SQL
                CLOSE cur5
           END-EXEC.
       *>**************************************************

       *>**************************************************
       *> List users procedure
       *>**************************************************
       B0300-List-Users.
           DISPLAY 'Listing users'.

           EXEC SQL
                DECLARE curpupil CURSOR FOR
                   SELECT  user_id,
                           user_firstname,
                           user_lastname,
                           user_email,
                           user_phonenumber,
                           usertype_id,
                           user_program,
                           user_lastlogin
                   FROM tbl_user
                   WHERE usertype_id = 1
                   ORDER BY user_lastname, user_firstname
           END-EXEC

           EXEC SQL
                DECLARE curall CURSOR FOR
                    SELECT  user_id,
                            user_firstname,
                            user_lastname,
                            user_email,
                            user_phonenumber,
                            usertype_id,
                            user_program,
                            user_lastlogin
                    FROM tbl_user
                    ORDER BY user_lastname, user_firstname
           END-EXEC

           *> Fetch the first record           
           EVALUATE wn-user-type-number
               WHEN 1
                   EXEC SQL
                        OPEN curpupil
                   END-EXEC
                   PERFORM B0310-Get-Pupil-Data.
               WHEN OTHER
                   EXEC SQL
                        OPEN curall
                   END-EXEC
                   PERFORM B0320-Get-All-User-Data.
           END-EVALUATE

           *> Fetch the remaining records
           PERFORM UNTIL sqlstate NOT = '00000'              
               DISPLAY
                   html-table-row-start
                   html-table-cell-start
                      tbl-user-type-name(t-user-usertype-id)
                   html-table-cell-end
                   html-table-cell-start
                      t-user-firstname
                   html-table-cell-end
                   html-table-cell-start
                      t-user-lastname
                   html-table-cell-end
                   html-table-cell-start
                      tbl-program-name(t-user-program-id)
                   html-table-cell-end
                   html-table-cell-start
                      t-user-email
                   html-table-cell-end
                   html-table-cell-start
                      t-user-phonenumber
                   html-table-cell-end
                   html-table-cell-start
                      t-user-lastlogin
                   html-table-cell-end
               END-DISPLAY

               PERFORM B0400-Check-if-Admin.

               *> fetch next
               EVALUATE wn-user-type-number
                   WHEN 1
                       PERFORM B0310-Get-Pupil-Data.
                   WHEN OTHER
                       PERFORM B0320-Get-All-User-Data.
               END-EVALUATE

           END-PERFORM.

           *> Close cursors
           EVALUATE wn-user-type-number
              WHEN 1
                 EXEC SQL
                        CLOSE curpupil
                 END-EXEC
              WHEN OTHER
                  EXEC SQL
                        CLOSE curall
                  END-EXEC
           END-EVALUATE.
       *>**************************************************

       *>**************************************************
       *> Get pupil data procedure
       *>**************************************************
       B0310-Get-Pupil-Data.
           DISPLAY 'Getting pupil data'.

           EXEC SQL
               FETCH curpupil INTO
                    :t-user-id,
                    :t-user-firstname,
                    :t-user-lastname,
                    :t-user-email,
                    :t-user-phonenumber,
                    :t-user-usertype-id,
                    :t-user-program-id,
                    :t-user-lastlogin
           END-EXEC.
       *>**************************************************

       *>**************************************************
       *> Get all user data procedure
       *>**************************************************
       B0320-Get-All-User-Data.
           DISPLAY 'Getting all user data'.

           EXEC SQL
               FETCH curall INTO
                    :t-user-id,
                    :t-user-firstname,
                    :t-user-lastname,
                    :t-user-email,
                    :t-user-phonenumber,
                    :t-user-usertype-id,
                    :t-user-program-id,
                    :t-user-lastlogin
           END-EXEC.
       *>**************************************************

       *>**************************************************
       *> Check if admin procedure
       *>**************************************************
       B0400-Check-if-Admin.
           DISPLAY 'Checking if user is admin'.

           IF wn-user-type-edit = 1
               DISPLAY 'User is an admin'.
           ELSE
               DISPLAY 'User is not an admin'.
           END-IF.
       *>**************************************************

       *>**************************************************
       *> Cleanup procedure
       *>**************************************************
       C0000-Cleanup.
           DISPLAY 'Cleaning up'.

           *> Additional cleanup code can be added here
           .
       *>**************************************************

       *>**************************************************
       *> Error routine
       *>**************************************************
       Z0100-Error-Routine.
           DISPLAY 'An error occurred'.
           *> Additional error handling code can be added here
           STOP RUN.
       *>**************************************************

       *>**************************************************
       *> Disconnect from database procedure
       *>**************************************************
       Z0200-Disconnect.
           DISPLAY 'Disconnecting from database'.

           EXEC SQL
               DISCONNECT CURRENT
           END-EXEC.

           SET is-db-connected TO FALSE.
       *>**************************************************

       *>**************************************************
       *> End of program
       *>**************************************************
       .