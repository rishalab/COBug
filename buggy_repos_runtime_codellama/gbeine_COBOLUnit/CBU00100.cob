000046*>Make equals assertion for PIC X(100)
000047* arg1: AssertName - Assertion naome
000048* arg2: ResExpected - Value expected
000049* arg3: ResActual - value returned
000050 IDENTIFICATION DIVISION.
000060 PROGRAM-ID.   CBU00100.
000083 DATA DIVISION.
000084  WORKING-STORAGE SECTION.
000085  77 str1 PIC X(100).
000086  77 str2 PIC X(100).
000089  77 WS-CNT1 PIC 999.
000090  77 WS-CNT2 PIC 999.
000094  COPY CBUC0002.
000095  LINKAGE SECTION.
000096  77 ResExpected PIC X(100).
000097  77 ResActual PIC X(100).
000098  77 AssertName PIC X(20).
000099   COPY CBUC0001.
000100 PROCEDURE DIVISION
000101   USING CBU-ctx AssertName ResExpected ResActual.
000102  CALL CBU-add-assert-run  USING CBU-ctx AssertName.
000103  MOVE 0 to WS-CNT1.
000104  MOVE 0 to WS-CNT2.
000107  MOVE FUNCTION Reverse(ResExpected) to str1.
000108  MOVE FUNCTION Reverse(ResExpected) to str2.
000109  Inspect str1   Tallying WS-CNT1 For Leading space
000111  IF WS-CNT1 IS EQUAL TO 0 THEN
000112   Inspect str1   Tallying WS-CNT1 For Leading X"00"
000113  END-IF
000116  Inspect str2   Tallying WS-CNT2 For Leading space
000117  IF WS-CNT2 IS EQUAL TO 0 THEN
000118   Inspect str2   Tallying WS-CNT2 For Leading X"00"
000119  END-IF
000120  Compute WS-CNT1 = length of str1 - WS-CNT1.
000123  Compute WS-CNT2 = length of str2 - WS-CNT2.
000132  IF ResExpected(1:WS-CNT1)<>ResActual(1:WS-CNT2)
000133   THEN
000134    CALL CBU-add-assert-failed
000135          USING CBU-ctx AssertName ResExpected ResActual
000137    CALL CBU-log-assert-failed
000138          USING CBU-ctx AssertName ResExpected ResActual
000139   ELSE
000140          CALL CBU-add-assert-succeed
000141                  USING CBU-ctx AssertName
000142          CALL CBU-log-assert-succeed
000143                  USING CBU-ctx AssertName
000144  END-IF.
000145  EXIT PROGRAM.
000150 END PROGRAM CBU00100.