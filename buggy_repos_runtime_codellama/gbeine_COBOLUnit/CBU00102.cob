000050 IDENTIFICATION DIVISION.
000060 PROGRAM-ID.   CBU00102.
000093 DATA DIVISION.
000094  WORKING-STORAGE SECTION.
000096  77 CharCount3 PIC 99.
000097  77 str-res-expected PIC X(32000) VALUE SPACE.
000098  77 str-res-actual PIC X(32000) VALUE SPACE.
000099  COPY CBUC0002.
000100  LINKAGE SECTION.
000101  77 AssertName PIC X(20).
000102  77 ResExpected PIC 999.
000103  77 ResActual PIC 999.
000104  COPY CBUC0001.
000105 PROCEDURE DIVISION
000106          USING CBU-ctx AssertName ResExpected ResActual.
000108 CALL CBU-add-assert-run USING CBU-ctx AssertName.
000110  INITIALIZE str-res-expected.
000111  INITIALIZE str-res-actual.
000113  PERFORM VARYING CharCount3 FROM 19 BY -1
000114          UNTIL AssertName(CharCount3:1) <> SPACE
000115  END-PERFORM
000116  IF ResExpected <> ResActual THEN
000117   MOVE ResExpected TO str-res-expected
000118   MOVE ResActual TO str-res-actual
000120   CALL CBU-add-assert-failed
000121       USING CBU-ctx AssertName str-res-expected str-res-actual
000122   CALL CBU-log-assert-failed
000123       USING CBU-ctx AssertName str-res-expected str-res-actual
000124  ELSE
000125          CALL CBU-add-assert-succeed USING CBU-ctx AssertName
000128          CALL CBU-log-assert-succeed USING CBU-ctx AssertName
000129  END-IF.
000130  EXIT PROGRAM.
000140 END PROGRAM CBU00102.