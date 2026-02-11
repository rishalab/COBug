*****************************************************************

      * z390 - Mainframe assembler emulator and run-time engine
      * Copyright (C) 2021 z390 Assembler LLC
      *
      * This file is part of z390.
      *
      * z390 is free software; you can redistribute it and/or modify
      * it under the terms of the GNU General Public License as 
      * published by the Free Software Foundation; either version 2 of 
      * the License, or (at your option) any later version.
      * z390 is distributed in the hope that it will be useful,
      * but WITHOUT ANY WARRANTY; without even the implied warranty of
      * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      * GNU General Public License for more details.
      *
      * You should have received a copy of the GNU General Public 
      * License along with this program; if not, see 
      * https://www.gnu.org/licenses.
      *****************************************************************
      * Author - Don Higgins                                          *
      *****************************************************************
        IDENTIFICATION DIVISION.
      *
      * TEST HANDLE ABEND
      *
      * TEST 1: HANDLE ASRA ABEND BY LABEL
      *         PRETEND NOTHING HAPPENED
      *
      * TEST 2: RAISE PGMIDERR
      *         HANDLE AEI0 ABEND BY LABEL
      *         PRETEND NOTHING HAPPENED
      *
      * TEST 3: HANDLE ASRA ABEND BY PROGRAM TESTBEC7
      *         PRETEND NOTHING HAPPENED
      *
      * THE FOLLOWING TESTS ARE DRIVEN BY TRANSID BEC5
      *
      * TEST 4: SET HANDLE ABEND LABEL HERE
      *         LINK TO TESTBEC7 AND ABEND ASRA
      *         RETURN IS AFTER OUR LINK
      *
      * TEST 5: LINK TO TESTBEC7 AND HANDLE ABEND AEI0 THERE BY PROGRAM
      *         TESTBEC8
      *         RETURN IS AFTER OUR LINK
      *
      * TEST 6: EXEC CICS ABEND WITH DUMP HANDLED HERE BY LABEL
      *         PRETEND NOTHING HAPPENED
      *
        PROGRAM-ID. TESTBEC6.
        ENVIRONMENT DIVISION.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

        77  RECAREA  PIC X(80).
        77  RECLEN   PIC 9(4) COMP.
        01  TEST1.
            02  FILLER  PIC X(4)  VALUE X'C311C1D1'.
            02  FILLER  PIC X(59)
                VALUE 'TESTBEC6 TEST 1:PRESS AID KEY TO HANDLE ASRA ABEN
      -               'D BY LABEL'.
        77  TEST1L   PIC 9(4) COMP.
        01  TEST1R.
            02  FILLER  PIC X(4)  VALUE X'C311C261'.
            02  FILLER  PIC X(33)
                        VALUE 'TESTBEC6 ASRA HANDLED OK BY LABEL'.
            02  FILLER  PIC X(3)  VALUE X'11C3F1'.
            02  FILLER  PIC X(59)
                VALUE 'TESTBEC6 TEST 2:PRESS AID KEY TO HANDLE AEI0 ABEN
      -               'D BY LABEL'.
        77  TEST1RL  PIC 9(4) COMP.
        01  TEST2R.
            02  FILLER  PIC X(4)  VALUE X'C311C5C1'.
            02  FILLER  PIC X(33)
                        VALUE 'TESTBEC6 AEI0 HANDLED OK BY LABEL'.
            02  FILLER  PIC X(3)  VALUE X'11C6D1'.
            02  FILLER  PIC X(61)
                VALUE 'TESTBEC6 TEST 3:PRESS AID KEY TO HANDLE ASRA ABEN
      -               'D BY PROGRAM'.
        77  TEST2RL  PIC 9(4) COMP.
        01  TEST4.
            02  FILLER  PIC X(4)  VALUE X'C311C1D1'.
            02  FILLER  PIC X(77)
                VALUE 'TESTBEC6 TEST 4:PRESS AID KEY TO HANDLE ASRA ABEN
      -               'D IN LINKED PROGRAM BY LABEL'.
        77  TEST4L   PIC 9(4) COMP.
        01  TEST4R.
            02  FILLER  PIC X(4)  VALUE X'C311C3F1'.
            02  FILLER  PIC X(45)
                VALUE 'TESTBEC6 ASRA IN TESTBEC7 HANDLED OK BY LABEL'.
        77  TEST4RL  PIC 9(4) COMP.
        01  TEST4K.
            02  FILLER  PIC X(4)  VALUE X'C311C5C1'.
            02  FILLER  PIC X(33)
                        VALUE 'TESTBEC6 AEI0 HANDLED OK BY LABEL'.
            02  FILLER  PIC X(3)  VALUE X'11C6D1'.
            02  FILLER  PIC X(61)
                VALUE 'TESTBEC6 TEST 3:PRESS AID KEY TO HANDLE ASRA ABEN
      -               'D BY PROGRAM'.
        77  TEST4KL  PIC 9(4) COMP.
        01  TEST5K.
            02  FILLER  PIC X(4)  VALUE X'C311C5C1'.
            02  FILLER  PIC X(33)
                        VALUE 'TESTBEC6 AEI0 HANDLED OK BY LABEL'.
            02  FILLER  PIC X(3)  VALUE X'11C6D1'.
            02  FILLER  PIC X(61)
                VALUE 'TESTBEC6 TEST 3:PRESS AID KEY TO HANDLE ASRA ABEN
      -               'D BY PROGRAM'.
        77  TEST5KL  PIC 9(4) COMP.
        01  TEST6R.
            02  FILLER  PIC X(4)  VALUE X'C311C5C1'.
            02  FILLER  PIC X(33)
                        VALUE 'TESTBEC6 AEI0 HANDLED OK BY LABEL'.
            02  FILLER  PIC X(3)  VALUE X'11C6D1'.
            02  FILLER  PIC X(61)
                VALUE 'TESTBEC6 TEST 3:PRESS AID KEY TO HANDLE ASRA ABEN
      -               'D BY PROGRAM'.
        77  TEST6RL  PIC 9(4) COMP.
        77  BADFLD2  PIC 9(4) VALUE 0.

      * *> INFINITE LOOP INTRODUCED HERE <*
        PROCEDURE DIVISION.
            PERFORM FOREVER
                DISPLAY 'INFINITE LOOP'.
            END-PERFORM.
            STOP RUN.
