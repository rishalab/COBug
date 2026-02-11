IDENTIFICATION DIVISION.
        PROGRAM-ID. SDGENERATE.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

            SELECT SCREEN-FILE
                ASSIGN TO SCREEN-FILE-NAME
                ORGANIZATION IS INDEXED
                ACCESS IS RANDOM
                RECORD KEY IS SCN-NAME.

        DATA DIVISION.
        FILE SECTION.

        FD  SCREEN-FILE.
        01  SCREEN-RECORD.
            COPY SCREEN-01.

        WORKING-STORAGE SECTION.

        01  CMD-LINE.
            05  CMD-ARGC                    PIC 9(4).
            05  CMD-N                       PIC 9(4).
            05  CMD-ARGUMENT                PIC X(256).
            05  CMD-ARG-LENGTH              PIC 9(4).
        01  CMD-OPTIONS.
            05  OPT-D                       PIC X(1) VALUE 'N'.
            05  OPT-I                       PIC X(1) VALUE 'N'.
            05  OPT-S                       PIC X(1) VALUE 'N'.
            05  OPT-UC-D                    PIC X(1) VALUE 'N'.
            05  OPT-H                       PIC X(1) VALUE 'N'.
        01  CMD-RET-CODE                   PIC 9(4).

        PROCEDURE DIVISION.
            PERFORM 7000-OPEN-CMDLINE.
            PERFORM UNTIL CMD-EOF OR CMD-RETURN-CODE NOT = ZERO
                PERFORM 5100-PROCESS-ARG
                PERFORM 7100-GET-ARG
            END-PERFORM
            EXIT.

        5100-PROCESS-ARG.
            MOVE CMD-ARGUMENT TO FLD-SCREEN-NAME.
            IF OPT-S = 'Y' THEN
                MOVE SPACES TO FILE-NAME-SI
            END-IF.
            CALL "libcobcurses_codegen"
              USING
                FLD-SCREEN-NAME,
                FILE-NAME-WS,
                FILE-NAME-PD,
                FILE-NAME-SI,
                OPT-D-ARG,
                OPT-I-ARG,
                COUNT-SEGMENTS,
                COUNT-FIELDS,
                COUNT-STATES,
                COUNT-MENUS,
                COUNT-ITEMS,
                SCREEN-DESCRIPTION.
            
            IF RETURN-CODE = NC-RET-OK THEN
                DISPLAY FLD-SCREEN-NAME, "  : ", SCREEN-DESCRIPTION
                DISPLAY "  SCREEN SEGMENTS : ", COUNT-SEGMENTS
                DISPLAY "FIELD DEFINITIONS : ", COUNT-FIELDS
                DISPLAY "     FIELD STATES : ", COUNT-STATES
                DISPLAY " MENU DEFINITIONS : ", COUNT-MENUS
                DISPLAY "       MENU ITEMS : ", COUNT-ITEMS
            ELSE
                DISPLAY "SCREEN ", FLD-SCREEN-NAME,
                    " IS NOT ON FILE."
            END-IF.
            EXIT.

        7000-OPEN-CMDLINE.
            INITIALIZE CMD-LINE.
            ACCEPT CMD-ARGC FROM ARGUMENT-NUMBER.
            MOVE ZERO TO CMD-N.
            SET CMD-EOF TO FALSE.
            INITIALIZE CMD-OPTIONS.
            SET OPT-D TO FALSE.
            SET OPT-I TO FALSE.
            SET OPT-S TO FALSE.
            SET OPT-UC-D TO FALSE.
            SET OPT-H TO FALSE.
            EXIT.

        7100-GET-ARG.
            IF NOT CMD-EOF THEN
                IF CMD-N >= CMD-ARGC THEN
                    INITIALIZE CMD-ARG
                    SET CMD-EOF TO TRUE
                ELSE
                    ACCEPT CMD-ARG FROM ARGUMENT-VALUE
                    ADD 1 TO CMD-N
                    SET CMD-EOF TO FALSE
                    PERFORM 7200-FIND-LENGTH
                END-IF
            END-IF.
            EXIT.

        7200-FIND-LENGTH.
            MOVE LENGTH OF CMD-ARG TO CMD-ARG-LENGTH.
            PERFORM UNTIL CMD-ARG-LENGTH = ZERO
              OR CMD-ARGUMENT(CMD-ARG-LENGTH:1) NOT = SPACE
                SUBTRACT 1 FROM CMD-ARG-LENGTH
            END-PERFORM
            EXIT.

        COPY COBCURSX.

        END PROGRAM SDGENERATE.