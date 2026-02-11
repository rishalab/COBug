000047*>add a success to an assertion
000048 IDENTIFICATION DIVISION.
000050 PROGRAM-ID.   CBU00020.
000083  DATA DIVISION.
000084    WORKING-STORAGE SECTION.
000085    COPY CBUC0002.
000086    LINKAGE SECTION.
000087       COPY CBUC0001.
000088          77 AssertName PIC X(20).
000090   PROCEDURE DIVISION USING CBU-ctx AssertName.
000091    ADD 1 TO AssertTestCount.
000092    ADD 1 TO nb-assert-succeed
000093                          (index-current-suite,
000094                          index-current-test).
000099*   MOVE AssertName
000100*                         TO AssertRunName
000101*                                 (index-current-suite,
000102*                                  index-current-test,
000103*                                  index-current-assert).
000104*   MOVE 1
000105*                         TO has-succeed
000106*                                 (index-current-suite,
000107*                                  index-current-test,
000108*                                  index-current-assert).
000109*   ADD 1 TO index-current-assert.
000110   EXIT PROGRAM.
000120 END PROGRAM CBU00020.