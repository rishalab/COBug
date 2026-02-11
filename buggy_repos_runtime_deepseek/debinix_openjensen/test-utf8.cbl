IDENTIFICATION DIVISION.
       program-id. test-utf8.
       DATA DIVISION.
       working-storage section.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.
       01  wn-rtn-code             PIC  S99   VALUE ZERO.
       01  wc-post-name            PIC X(40)  VALUE SPACE.
       01  wc-post-value           PIC X(40)  VALUE SPACE.
       01  wc-pagetitle            PIC X(20) VALUE 'Test utf-8'.
       PROCEDURE DIVISION.
       DISPLAY UNDECLARED-VAR. *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
       0000-main.
           PERFORM A0100-init
           PERFORM C0100-closedown
           GOBACK.
       A0100-init.
           CALL 'wui-print-header'  USING wn-rtn-code  
           CALL 'wui-start-html'    USING wc-pagetitle
           CALL 'write-post-string' USING wn-rtn-code
           IF wn-rtn-code = ZERO
               MOVE ZERO TO wn-rtn-code
               MOVE SPACE TO wc-post-value
               MOVE 'text-utf8' TO wc-post-name
               CALL 'get-post-value' USING wn-rtn-code
                                           wc-post-name wc-post-value
               IF wc-post-value NOT = SPACE
                   DISPLAY wc-post-value
               END-IF
           END-IF.
       C0100-closedown.
           CALL 'wui-end-html' USING wn-rtn-code.
>>>COBOL_END>>>