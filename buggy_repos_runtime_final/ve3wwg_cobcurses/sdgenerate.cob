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
            05  CMD-EOF-FLAG                PIC X.
                88  CMD-EOF                 VALUE 'Y'
                    FALSE IS                'N'.
            05  CMD-ARG.
                10  CMD-OPTION-FLAG         PIC X.
                    88  CMD-OPTION          VALUE '-'.
                10  CMD-OPTION-NAME         PIC X(32).
                    88  CMD-OPT-D           VALUE 'd'.
                    88  CMD-OPT-I           VALUE 'i'.
                    88  CMD-OPT-UC-D        VALUE 'D'.
                    88  CMD-OPT-S           VALUE 's'.
                    88  CMD-OPT-H           VALUE 'h'.
                    88  CMD-OPT-HELP        VALUE '-help'.
                    88  CMD-OPT-EOF         VALUE '-'.
                10  FILLER                  PIC X(479).            
            05  CMD-ARG-2 REDEFINES CMD-ARG.
                10  CMD-ARGUMENT            PIC X(512).
            05  CMD-ARG-LENGTH              PIC 9(4).
            05  CMD-ARGLEN-5                PIC 9(4) COMP-5.

        01  CMD-RETURN-CODE                 PIC S9(9) VALUE 0.

        01  CMD-OPTIONS.
            05  OPT-VALID-FLAG              PIC X.
                88  OPT-VALID               VALUE 'Y'
                    FALSE IS                'N'.
                88  OPT-ARG-INVALID         VALUE 'X'.
            05  OPT-CURRENT                 PIC X(32).
            05  OPT-CURRENT-LENGTH          PIC 9(4).
            05  OPT-D-FLAG                  PIC X.
                88  OPT-D                   VALUE 'Y'
                    FALSE IS                'N'.
            05  OPT-D-ARG TO SPACES         PIC X(256).
            05  OPT-I-FLAG                  PIC X.
                88  OPT-I                   VALUE 'Y'
                    FALSE IS                'N'.
            05  OPT-I-ARG TO SPACES         PIC X(256).
            05  OPT-S-FLAG                  PIC X.
                88  OPT-S                   VALUE 'Y'
                    FALSE IS                'N'.
            05  OPT-UC-D-FLAG               PIC X.
                88  OPT-UC-D                VALUE 'Y'
                    FALSE IS                'N'.
            05  OPT-UC-D-ARG TO SPACES      PIC X(256).
            05  OPT-H-FLAG                  PIC X.
                88  OPT-H                   VALUE 'Y'
                    FALSE IS                'N'.

        01  SCREEN-DESCRIPTION              PIC X(256).

        PROCEDURE DIVISION.

        MAIN-PARA.
            DISPLAY "Starting SDGENERATE...".
            PERFORM 7000-OPEN-CMDLINE.
            PERFORM UNTIL CMD-EOF
                PERFORM 7100-GET-ARG
                IF CMD-OPTION
                    PERFORM 1200-SET-OPTIONS
                ELSE
                    PERFORM 5000-PROCESS
                END-IF
            END-PERFORM.
            DISPLAY "SDGENERATE completed.".
            STOP RUN.

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

        1200-SET-OPTIONS.
            IF OPT-D THEN
                MOVE CMD-ARGUMENT TO OPT-D-ARG
            ELSEIF OPT-I THEN
                MOVE CMD-ARGUMENT TO OPT-I-ARG
            ELSEIF OPT-UC-D THEN
                MOVE CMD-ARGUMENT TO OPT-UC-D-ARG
            END-IF.
            EXIT.

        5000-PROCESS.
            PERFORM UNTIL CMD-EOF OR CMD-RETURN-CODE NOT = ZERO
                PERFORM 5100-PROCESS-ARG
                PERFORM 7100-GET-ARG
            END-PERFORM
            EXIT.

        5100-PROCESS-ARG.
            MOVE CMD-ARGUMENT TO FLD-SCREEN-NAME.
            PERFORM 6000-LOOKUP-SCREEN.
            IF OPT-S THEN
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

        6000-LOOKUP-SCREEN.
            OPEN INPUT SCREEN-FILE.
            MOVE FLD-SCREEN-NAME TO SCN-NAME
            READ SCREEN-FILE
                INVALID KEY
                    DISPLAY "ERROR: SCREEN NOT ON FILE: ",
                        SCN-NAME
                    MOVE SPACES TO FILE-NAME-WS, FILE-NAME-PD,
                        FILE-NAME-SI
                    MOVE 1 TO CMD-RETURN-CODE
                NOT INVALID KEY
                    SET SCREEN-FOUND TO TRUE
                    MOVE SCN-WS-SECTION TO FILE-NAME-WS
                    MOVE SCN-PROCEDURE-DIVISION TO FILE-NAME-PD
                    MOVE SCN-NAME TO FILE-NAME-SI
            END-READ
            CLOSE SCREEN-FILE.
            EXIT.

        8900-DISPLAY-USAGE.
            DISPLAY "Usage:  sdgenerate [-d destdir] [-i imagedir] "
                "[-D datadir] [-s] screens..".
            DISPLAY "where".
            DISPLAY "        -d subdir       Specifies the destination"
                " for the generated".
            DISPLAY "                        copy books. Defaults "
                "to current directory.".
            DISPLAY "        -i imagedir     Specifies the destin"
                "ation for the generated".
            DISPLAY "                        screen image text file. "
                "Default is current".
            DISPLAY "                        directory.".
            DISPLAY "        -D datadir      Use this as the data "
                "directory for the ".
            DISPLAY "                        screens database".
            DISPLAY "        -s              Suppress the screen "
                "image file.".
            DISPLAY "        -h              Requests this info.".
            DISPLAY "                        other options and "
                "arguments.".
            DISPLAY "        screens         One or more screen "
                "names (as they exist".
            DISPLAY "                        on file).".
            EXIT.

        9000-FINALIZE.
            MOVE CMD-RETURN-CODE TO RETURN-CODE.        
            EXIT.
            
        COPY COBCURSX.

        END PROGRAM SDGENERATE.