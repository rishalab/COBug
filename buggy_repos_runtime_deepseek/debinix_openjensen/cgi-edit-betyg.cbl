IDENTIFICATION DIVISION.
       program-id. cgi-edit-betyg.
       DATA DIVISION.
       working-storage section.
       01   switches-edit.
            03  is-db-connected-switch              PIC X   VALUE 'N'.
                88  is-db-connected                         VALUE 'Y'.
            03  is-valid-init-switch                PIC X   VALUE 'N'.
                88  is-valid-init                           VALUE 'Y'.                             
            03  grade-id-is-in-table-switch         PIC X   VALUE 'N'.
                88  grade-id-is-in-table                    VALUE 'Y'.                             
       01  wn-rtn-code             PIC  S99   VALUE ZERO.
       01  wc-post-name            PIC X(40)  VALUE SPACE.
       01  wc-post-value           PIC X(40)  VALUE SPACE.
       01  wc-printscr-string      PIC X(40)  VALUE SPACE.        
       01  wc-pagetitle            PIC X(20) VALUE 'Uppdatera betyg'.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  tbl-grade-rec-vars.       
           05  tbl-grade-grade-id       PIC  9(4).
           05  tbl-grade-grade          PIC  X(40).
           05  tbl-grade-comment        PIC  X(40).
       01  wr-rec-vars.
           05  wn-grade-id         PIC  9(4) VALUE ZERO.
           05  wc-grade            PIC  X(40) VALUE SPACE.
           05  wc-comment          PIC  X(40) VALUE SPACE.  
       01  wr-cur-rec-vars.
           05  wn-cur-grade-id         PIC  9(4) VALUE ZERO.     
           05  wc-cur-grade        PIC  X(40) VALUE SPACE. 
           05  wc-cur-comment      PIC  X(40) VALUE SPACE.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE DIVISION.
       GO TO ERR-HANDLER.
       DISPLAY "NORMAL-FLOW".
       ERR-HANDLER.
           DISPLAY "ERROR-HANDLED".
           EXIT.
       0000-main.
           COPY setupenv_openjensen. 
           PERFORM A0100-init
           IF is-valid-init
               PERFORM B0100-connect
               IF is-db-connected
                   *> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
                   PERFORM B0200-edit-grade
                   *> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
                   PERFORM Z0200-disconnect
               END-IF
           END-IF
           PERFORM C0100-closedown
           GOBACK.
       A0100-init.       
           CALL 'wui-print-header' USING wn-rtn-code  
           CALL 'wui-start-html' USING wc-pagetitle
           CALL 'write-post-string' USING wn-rtn-code
           IF wn-rtn-code = ZERO
               PERFORM A0110-init-edit-action
           END-IF.
       A0110-init-edit-action.
           MOVE ZERO TO wn-rtn-code
           MOVE SPACE TO wc-post-value
           MOVE 'grade_id' TO wc-post-name
           CALL 'get-post-value' USING wn-rtn-code
                               wc-post-name wc-post-value
           MOVE FUNCTION NUMVAL(wc-post-value) TO wn-grade-id
           IF wc-post-value = SPACE
               MOVE 'Saknar ett angivet grade id'
                    TO wc-printscr-string
               CALL 'stop-printscr' USING wc-printscr-string      
           ELSE                 
               MOVE ZERO TO wn-rtn-code
               MOVE SPACE TO wc-post-value
               MOVE 'grade_grade' TO wc-post-name
               CALL 'get-post-value' USING wn-rtn-code
                                    wc-post-name wc-post-value
               IF wc-grade NOT = wc-cur-grade
                   MOVE wc-grade TO tbl-grade-grade
               ELSE    
                   MOVE wc-cur-grade TO tbl-grade-grade
               END-IF
               IF wc-comment NOT = wc-cur-comment
                   MOVE wc-comment TO tbl-grade-comment
               ELSE
                   MOVE wc-cur-comment TO tbl-grade-comment
               END-IF
           END-IF.
       B0200-edit-grade.
           PERFORM B0210-does-grade-id-exist
           IF grade-id-is-in-table
               PERFORM B0220-change-grade-item
           END-IF.
       B0210-does-grade-id-exist.
           EXEC SQL
             DECLARE cursedit CURSOR FOR
                 SELECT grade_id, grade_grade, grade_comment
                 FROM tbl_grade
           END-EXEC      
           EXEC SQL
                OPEN cursedit
           END-EXEC
           MOVE wn-grade-id TO tbl-grade-grade-id
           EXEC SQL
               FETCH cursedit
                   INTO :tbl-grade-grade-id, :tbl-grade-grade,
                        :tbl-grade-comment
           END-EXEC
           PERFORM UNTIL SQLCODE NOT = ZERO
               IF wn-grade-id = tbl-grade-grade-id
                    SET grade-id-is-in-table TO TRUE
                MOVE tbl-grade-grade-id TO wn-cur-grade-id
                MOVE tbl-grade-grade TO wc-cur-grade
                MOVE tbl-grade-comment TO wc-cur-comment
               END-IF
              EXEC SQL
                   FETCH cursedit
                       INTO :tbl-grade-grade-id, :tbl-grade-grade,
                            :tbl-grade-comment
               END-EXEC
           END-PERFORM
           IF  SQLSTATE NOT = '02000'
                PERFORM Z0100-error-routine
           END-IF                 
           EXEC SQL 
               CLOSE cursedit 
           END-EXEC.
       B0220-change-grade-item.
           IF wc-grade NOT = wc-cur-grade
               MOVE wc-grade TO tbl-grade-grade
           ELSE    
               MOVE wc-cur-grade TO tbl-grade-grade
           END-IF
           IF wc-comment NOT = wc-cur-comment
               MOVE wc-comment TO tbl-grade-comment
           ELSE
               MOVE wc-cur-comment TO tbl-grade-comment
           END-IF
           EXEC SQL
               UPDATE tbl_grade
                   SET grade_grade = :tbl-grade-grade,
                       grade_comment = :tbl-grade-comment
               WHERE grade_id = :tbl-grade-grade-id
           END-EXEC
           IF  SQLCODE NOT = ZERO
                PERFORM Z0100-error-routine
           ELSE
                PERFORM B0230-commit-work
                MOVE 'Betyg data Ã¤ndrad' TO wc-printscr-string
                CALL 'ok-printscr' USING wc-printscr-string      
           END-IF.
       B0230-commit-work.
           EXEC SQL 
               COMMIT WORK
           END-EXEC.
       C0100-closedown.
           CALL 'wui-end-html' USING wn-rtn-code.
       Z0100-error-routine.
           COPY z0100-error-routine.
       Z0200-disconnect. 
           EXEC SQL
               DISCONNECT ALL
           END-EXEC.