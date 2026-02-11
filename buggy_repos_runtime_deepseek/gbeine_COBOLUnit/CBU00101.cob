000010* COBOLUnit is a COBOL Unit framework testing
000011*
000012* Logic name: CBU-assert-str-equals
000013* source name: CBU00101.cob
000023*
000028*  Copyright (C) 2009 Hervé Vaujour
000029*
000030*  This program is free software; you can redistribute it and/or modify
000031*  it under the terms of the GNU General Public License as published by
000032*  the Free Software Foundation; either version 2 of the License, or
000033*  (at your option) any later version.
000034*
000035*  This program is distributed in the hope that it will be useful,
000036*  but WITHOUT ANY WARRANTY; without even the implied warranty of
000037*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
000038*  GNU General Public License for more details.
000039*
000040*  You should have received a copy of the GNU General Public License
000041*  along with this program; see the file COPYING. If not, write to the
000042*  Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
000043
000044
000045
000046*>Make equals assertion for PIC X(32000)
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
000111* RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
000112  COMPUTE WS-CNT1 = LENGTH OF ResExpected - WS-CNT1.
000113  COMPUTE WS-CNT2 = LENGTH OF ResActual - WS-CNT2.
000114* RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
000115
000116  CALL CBU-get-last-index USING CBU-ctx ResExpected WS-CNT1.
000117  CALL CBU-get-last-index USING CBU-ctx ResActual WS-CNT2.
000118
000119* RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
000120  IF ResExpected(1:WS-CNT1) <> ResActual(1:WS-CNT2) THEN
000121   CALL CBU-log-assert-failed.
000122          USING   CBU-ctx AssertName ResExpected(1:WS-CNT1) ResActual(1:WS-CNT2).
000123   CALL CBU-add-assert-failed USING   CBU-ctx AssertName ResExpected(1:WS-CNT1) ResActual(1:WS-CNT2).
000124  ELSE
000125   CALL CBU-add-assert-succeed USING CBU-ctx AssertName.
000126   CALL CBU-log-assert-succeed USING CBU-ctx AssertName.
000127  END-IF.
000128* RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
000131
000132  EXIT PROGRAM.
000160 END PROGRAM CBU00101.