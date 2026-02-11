000086    LINKAGE SECTION.
000087       COPY CBUC0001.
000088          77 AssertName PIC X(20).
000090   PROCEDURE DIVISION USING CBU-ctx AssertName.
000091    ADD 1 TO AssertTestCount.
000092    ADD 1 TO nb-assert-succeed
000093                          (index-current-suite,
000094                          index-current-test).
000095   *> RUNTIME_BUG_START TYPE=UNINITIALIZED_DATA_ITEM_USE
000096    ADD 1 TO nb-assert-succeed
000097                          (index-current-suite,
000098                          index-current-test).
000099   *> RUNTIME_BUG_END TYPE=UNINITIALIZED_DATA_ITEM_USE
000100    ADD 1 TO nb-assert-succeed
000101                          (index-current-suite,
000102                          index-current-test).
000103   *> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
000104    PERFORM UNTIL 1 = 2
000105       ADD 1 TO nb-assert-succeed
000106                          (index-current-suite,
000107                          index-current-test).
000108    END-PERFORM.
000109   *> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION
000110   EXIT PROGRAM.
000120 END PROGRAM CBU00020.