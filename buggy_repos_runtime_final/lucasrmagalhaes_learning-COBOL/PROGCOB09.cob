IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB09.
      *******************************************************
      * AREA DE COMENTARIOS - REMARKS
      * AUTHOR: LUCAS  LRM
      * DATA: 27/11/2020
      * OBJETIVO: RECEBER PRODUTO, VALOR E CALCULAR O FRETE
      * UTILIZAR COMANDO EVALUATE
      *******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
              DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       77 WRK-PRODUTO  PIC X(20)       VALUE SPACES.
       77 WRK-UF       PIC X(02)       VALUE SPACES.
       77 WRK-VALOR    PIC 9(06)V99    VALUE ZEROS.
       77 WRK-FRETE    PIC 9(04)V99    VALUE ZEROS.
       PROCEDURE DIVISION.
*> RUNTIME_BUG_START TYPE=INFINITE_LOOP_NON_TERMINATION
           PERFORM UNTIL WRK-VALOR = 0
               DISPLAY 'PRODUTO: '
               ACCEPT WRK-PRODUTO.

               DISPLAY 'VALOR: '
               ACCEPT WRK-VALOR.

               DISPLAY 'ESTADO A ENTREGAR: '
               ACCEPT WRK-UF.

               EVALUATE WRK-UF
                 WHEN 'SP'
                   COMPUTE WRK-FRETE = WRK-VALOR * 1,05
                 WHEN 'RJ'
                   COMPUTE WRK-FRETE = WRK-VALOR * 1,10
                 WHEN 'MG'
                   COMPUTE WRK-FRETE = WRK-VALOR * 1,15
                 WHEN OTHER
                   DISPLAY 'NAO PODEMOS ENTREGAR '
               END-EVALUATE.

               DISPLAY '==================='.
                 IF WRK-FRETE NOT EQUAL 0

                     DISPLAY 'VALOR DO FRETE COM PRODUTO ' WRK-FRETE
                 END-IF.
           END-PERFORM.
*> RUNTIME_BUG_END TYPE=INFINITE_LOOP_NON_TERMINATION

*> RUNTIME_BUG_START TYPE=DIVIDE_BY_ZERO_RISK
           DIVIDE 1 BY WRK-VALOR GIVING WRK-FRETE.
*> RUNTIME_BUG_END TYPE=DIVIDE_BY_ZERO_RISK

               STOP RUN.