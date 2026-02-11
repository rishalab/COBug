000047* arg1: AssertName - Assertion naome
000048* arg2: ResExpected - Value expected
000049* arg3: ResActual - value returned
000050 IDENTIFICATION DIVISION.
000060 PROGRAM-ID.   CBU00101.
000083 DATA DIVISION.
000084  WORKING-STORAGE SECTION.
000085  77 str1 PIC X(32000).
000086  77 str2 PIC X(32000).
000089  77 WS-CNT1 PIC 99999.
000090  77 WS-CNT2 PIC 99999.
000091
000094  COPY CBUC0002.
000096  LINKAGE SECTION.
000098  77 ResExpected PIC X(32000).
000099  77 ResActual PIC X(32000).
000100  77 AssertName PIC X(20).
000101  COPY CBUC0001.
000102 PROCEDURE DIVISION
000103   USING CBU-ctx AssertName ResExpected ResActual.
000104  CALL CBU-add-assert-run USING CBU-ctx AssertName.
000105
000106
000107  MOVE 0 to WS-CNT1.
000109  MOVE 0 to WS-CNT2.
000110
000111*  MOVE FUNCTION Reverse(ResExpected) to str1.
000112*          DISPLAY "track 3.1".
000113*  MOVE FUNCTION Reverse(ResActual) to str2.
000114*  DISPLAY "track 4".
000115
000116*  Inspect str1   Tallying WS-CNT1 For Leading space
000117*  IF WS-CNT1 IS EQUAL TO 0 THEN
000118*   Inspect str1   Tallying WS-CNT1 For Leading X"00"
000119*  END-IF
000120
000121*  Inspect str2   Tallying WS-CNT2 For Leading space
000122*  IF WS-CNT2 IS EQUAL TO 0 THEN
000123*   Inspect str2   Tallying WS-CNT2 For Leading X"00"
000124*  END-IF
000125*  Compute WS-CNT1 = length of str1 - WS-CNT1.
000126*  Compute WS-CNT2 = length of str2 - WS-CNT2.
000131
000132  CALL CBU-get-last-index USING CBU-ctx ResExpected WS-CNT1
000134  CALL CBU-get-last-index USING CBU-ctx ResActual WS-CNT2
000135
000136  IF ResExpected(1:WS-CNT1)<>ResActual(1:WS-CNT2)
000137   THEN
000138    CALL CBU-log-assert-failed
000139          USING   CBU-ctx
000140                          AssertName
000141                          ResExpected(1:WS-CNT1)
000142                          ResActual(1:WS-CNT2)
000143    CALL CBU-add-assert-failed
000144          USING   CBU-ctx
000145                          AssertName
000146                          ResExpected(1:WS-CNT1)
000147                          ResActual(1:WS-CNT2)
000148   ELSE
000150    CALL CBU-add-assert-succeed
000151      USING CBU-ctx AssertName
000152    CALL CBU-log-assert-succeed
000153          USING CBU-ctx AssertName
000154  END-IF.
000155  EXIT PROGRAM.
000160 END PROGRAM CBU00101.