IDENTIFICATION DIVISION.
       program-id. cgi-add-user.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01   switches-add.
            03  is-db-connected-switch         PIC X   VALUE 'N'.
                88  is-db-connected                    VALUE 'Y'.
            03  is-valid-init-switch           PIC X   VALUE 'N'.
                88  is-valid-init                      VALUE 'Y'.
            03  name-is-in-table-switch        PIC X   VALUE 'N'.
                88  name-is-in-table                   VALUE 'Y'.
            03  is-valid-table-position-switch PIC X   VALUE 'N'.
                88  is-valid-table-position            VALUE 'Y'.
       *> Error in SQL state handling, missing initialization and incorrect condition check
       PROCEDURE DIVISION.
           B0100-connect.
               MOVE "openjensen" TO wc-database
               MOVE "jensen" TO wc-username
               MOVE SPACE TO wc-passwd
               EXEC SQL
                   CONNECT :wc-username IDENTIFIED BY :wc-passwd USING :wc-database
               END-EXEC
               IF SQLSTATE NOT = ZERO
                   PERFORM Z0100-error-routine
               ELSE
                   SET is-db-connected TO TRUE
               END-IF
           .
       *> Incorrect condition check in B0210-does-username-exist
       B0210-does-username-exist.
           EXEC SQL
               DECLARE cursadduser CURSOR FOR
               SELECT user_id, user_username
               FROM tbl_user
           END-EXEC
           EXEC SQL
               OPEN cursadduser
           END-EXEC
           EXEC SQL
               FETCH cursadduser INTO :t-user-id, :t-user-username
           END-EXEC
           PERFORM UNTIL SQLCODE NOT = ZERO
               IF FUNCTION UPPER-CASE(wc-user-username) = FUNCTION UPPER-CASE(t-user-username)
                   SET name-is-in-table TO TRUE
               END-IF
               EXEC SQL
                   FETCH cursadduser INTO :t-user-id, :t-user-username
               END-EXEC
           END-PERFORM
           IF SQLSTATE NOT = '02000'
               PERFORM Z0100-error-routine
           END-IF
           EXEC SQL
               CLOSE cursadduser
           END-EXEC
       .