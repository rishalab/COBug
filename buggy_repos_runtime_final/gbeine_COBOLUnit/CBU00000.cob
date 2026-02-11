000020* COBOLUnit is a COBOL Unit framework testing
000021*
000022* Logic name: CBU-initialize
000032*	source name: CBU00000.cob
000039*
000040*  Copyright (C) 2009 HervÃ© Vaujour
000041*
000042*  This program is free software; you can redistribute it and/or modify
000043*  it under the terms of the GNU General Public License as published by
000044*  the Free Software Foundation; either version 2 of the License, or
000045*  (at your option) any later version.
000046*
000047*  This program is distributed in the hope that it will be useful,
000048*  but WITHOUT ANY WARRANTY; without even the implied warranty of
000049*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
000050*  GNU General Public License for more details.
000051*
000052*  You should have received a copy of the GNU General Public License
000053*  along with this program; see the file COPYING. If not, write to the
000054*  Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
000055
000056
000057* reset COBOLUnit variables
000058 IDENTIFICATION DIVISION.
000059 PROGRAM-ID.   CBU00000.
000060 ENVIRONMENT    DIVISION.
000061 CONFIGURATION  SECTION.
000062 DATA DIVISION.
000063 WORKING-STORAGE SECTION.
000064    01 i PIC 9(2).
000065    01 j PIC 9(2).
000066    01 k PIC 9(2).
000072    COPY CBUC0002.
000073    LINKAGE SECTION.
000074    COPY CBUC0001.
000075 PROCEDURE DIVISION USING CBU-ctx.
000076    MOVE 0 TO TestRunCount.
000077    MOVE 0 TO index-current-suite.
000078    MOVE 0 TO index-current-test.
000079    MOVE 0 TO RunSuccessCount.
000080    MOVE 0 TO RunFailureCount.
000081    MOVE 0 TO TestError.
000082    MOVE 0 TO nb-suite-run.
000088*> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
000089    PERFORM VARYING i FROM 1 BY 1
000090          UNTIL i > SuiteIndex *> Change to '>' to create an infinite loop
000091          PERFORM VARYING j FROM 1 BY 1
000092            UNTIL ListeTests(i,j) = ""
000093           MOVE 0 TO nb-test-run (i)
000094           MOVE 0 TO nb-test-succeed (i)
000095           MOVE 0 TO nb-test-failed (i)
000096           MOVE 0 TO nb-test-error (i)
000097*> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION
000098*                   PERFORM VARYING k FROM 1 BY 1
000099*             UNTIL ListeAssertRuns(i,j,k) = ""
000100*                   MOVE 0 TO has-succeed (i,j,k)
000101*               INITIALIZE AssertRunName (i,j,k)
000102*                   INITIALIZE AssertValueExpected (i,j,k)
000103*                   INITIALIZE AssertValueActual (i,j,k)
000104*> RUNTIME_BUG_START TYPE=DIVIDE_BY_ZERO_RISK
000105           DIVIDE i BY j GIVING k *> Risk of division by zero if j is 0
000106*> RUNTIME_BUG_END TYPE=DIVIDE_BY_ZERO_RISK
000107*                   END-PERFORM
000108          END-PERFORM
000109    END-PERFORM
000110    EXIT PROGRAM.
000111 END PROGRAM CBU00000.
000112
000113
000376