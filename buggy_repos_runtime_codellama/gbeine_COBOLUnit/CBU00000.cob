IDENTIFICATION DIVISION.
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
000088    PERFORM VARYING i FROM 1 BY 1
000098          UNTIL i >= SuiteIndex
000099          PERFORM VARYING j FROM 1 BY 1
000100            UNTIL ListeTests(i,j) = ""
000102           MOVE 0 TO nb-test-run (i)
000105           MOVE 0 TO nb-test-succeed (i)
000106           MOVE 0 TO nb-test-failed (i)
000107           MOVE 0 TO nb-test-error (i)
000110*                   PERFORM VARYING k FROM 1 BY 1
000111*             UNTIL ListeAssertRuns(i,j,k) = ""
000112*                   MOVE 0 TO has-succeed (i,j,k)
000113*               INITIALIZE AssertRunName (i,j,k)
000115*                   INITIALIZE AssertValueExpected (i,j,k)
000116*                   INITIALIZE AssertValueActual (i,j,k)
000122*                   END-PERFORM
000123          END-PERFORM
000124    END-PERFORM
000125    EXIT PROGRAM.
000126 END PROGRAM CBU00000.
000127
000130
000376