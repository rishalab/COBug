000010*>
000020 IDENTIFICATION DIVISION.
000030 PROGRAM-ID.   CTU0S1T2.
000040 ENVIRONMENT    DIVISION.
000050 CONFIGURATION  SECTION.
000060 DATA DIVISION.
000070  WORKING-STORAGE SECTION.
000080   01 str-expected PIC X(32000).
000090   01 str-actual PIC X(32000).
000091   01 assert-name PIC X(20).
000092   01 nb-expected PIC 999.
000093   01 nb-actual PIC 999.
000094   COPY CBUC0002.
000095   LINKAGE SECTION.
000096   COPY CBUC0001.
000097 PROCEDURE DIVISION USING CBU-ctx.
000099  INITIALIZE str-expected.
000100  *> RUNTIME_BUG_START TYPE=UNINITIALIZED_DATA_ITEM_USE
000102  MOVE "TOTO" TO assert-name.
000103  CALL CBU-assert-str-equals USING
000104          CBU-ctx
000105          assert-name
000106          str-expected
000107          str-actual.
000108  *> RUNTIME_BUG_END TYPE=UNINITIALIZED_DATA_ITEM_USE
000109
000110  MOVE "assert2" TO assert-name.
000111  CALL CBU-assert-str-equals USING
000112          CBU-ctx
000113          assert-name
000114          str-expected
000115          str-actual.
000116
000117  MOVE "nb-assert3" TO assert-name.
000118  *> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
000122  CALL CBU-assert-nb3-equals USING
000123          CBU-ctx
000124          assert-name
000125          nb-expected
000126          nb-assert-run(1,2).
000127  *> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
000128
000129  MOVE "assert-succes1" TO assert-name.
000130  MOVE 3 TO nb-expected.
000131  CALL CBU-assert-nb3-equals USING
000132          CBU-ctx
000133          assert-name
000134          nb-expected
000135          nb-assert-succeed(1,2).
000136
000137  MOVE "assert-succes2" TO assert-name.
000138  MOVE 4 TO nb-expected.
000139  CALL CBU-assert-nb3-equals USING
000140          CBU-ctx
000141          assert-name
000142          nb-expected
000143          nb-assert-succeed(1,2).
000144
000145  MOVE "nb-assert5" TO assert-name.
000146  MOVE 6 TO nb-expected.
000147  CALL CBU-assert-nb3-equals USING
000148          CBU-ctx
000149          assert-name
000150          nb-expected
000151          nb-assert-run(1,2).
000160 END PROGRAM CTU0S1T2.