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
000100  INITIALIZE str-actual.
000101  INITIALIZE nb-expected.
000102  INITIALIZE nb-actual.
000103  MOVE "TOTO" TO str-expected.
000104  MOVE "TOTO" TO str-actual.
000105
000106  *> RUNTIME_BUG_START TYPE=UNINITIALIZED_DATA_ITEM_USE
000107  CALL CBU-assert-str-equals USING
000108          CBU-ctx
000109          assert-name
000110          str-expected
000111          str-actual.
000112  *> RUNTIME_BUG_END TYPE=UNINITIALIZED_DATA_ITEM_USE
000113
000114  MOVE "assert2" TO assert-name.
000115  CALL CBU-assert-str-equals USING
000116          CBU-ctx
000117          assert-name
000118          str-expected
000119          str-actual.
000120
000121  MOVE "nb-assert3" TO assert-name.
000122  *> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
000123  CALL CBU-assert-nb3-equals USING
000124          CBU-ctx
000125          assert-name
000126          nb-expected
000127          nb-assert-run(1,2).
000128  *> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
000129
000130  MOVE "assert-succes1" TO assert-name.
000131  MOVE 3 TO nb-expected.
000132  CALL CBU-assert-nb3-equals USING
000133          CBU-ctx
000134          assert-name
000135          nb-expected
000136          nb-assert-succeed(1,2).
000137
000138  MOVE "assert-succes2" TO assert-name.
000139  MOVE 4 TO nb-expected.
000140  CALL CBU-assert-nb3-equals USING
000141          CBU-ctx
000142          assert-name
000143          nb-expected
000144          nb-assert-succeed(1,2).
000145
000146  MOVE "nb-assert5" TO assert-name.
000147  MOVE 6 TO nb-expected.
000148  CALL CBU-assert-nb3-equals USING
000149          CBU-ctx
000150          assert-name
000151          nb-expected
000152          nb-assert-run(1,2).
000160 END PROGRAM CTU0S1T2.