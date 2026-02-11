IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB06.
      *********************************************
      * AREA DE COMENTARIOS - REMARKS
      * AUTHOR: LUCAS LRM
      * DATA: 18/11/2020
      * OBJETIVO: USO DO SINAL - e +
      *********************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       77 WRK-NUM1  	PIC 9(02)  VALUE ZEROS.
       77 WRK-NUM2  	PIC 9(02)  VALUE ZEROS.
      *************** SINAL = S
       77 WRK-RESUL 	PIC S9(03) VALUE ZEROS.
      *************** FORMATACAO
       77 WRK-RESUL-ED  PIC -ZZ9   VALUE ZEROS.
       PROCEDURE DIVISION.
       GO TO ERR-HANDLER.

       DISPLAY "NORMAL-FLOW".

       ERR-HANDLER.
           DISPLAY "ERROR-HANDLED".
           EXIT.

          ACCEPT WRK-NUM1 FROM CONSOLE.
          ACCEPT WRK-NUM2 FROM CONSOLE.
          DISPLAY '=============================='
          DISPLAY 'NUMERO 1: ' WRK-NUM1.
          DISPLAY '=============================='
          DISPLAY 'NUMERO 2: ' WRK-NUM2.
          DISPLAY '=============================='
      *************** SUBTRACAO - PARA O SINAL FUNCIONAR É NECESSÁRIO TROCAR A ORDEM DAS VARIAVEIS - NUM 2 - NUM 1
          SUBTRACT WRK-NUM2 FROM WRK-NUM1 GIVING WRK-RESUL.
           MOVE WRK-RESUL TO WRK-RESUL-ED.
           DISPLAY 'SUBTRACAO: ' WRK-RESUL.
           DISPLAY 'SUBTRACAO: ' WRK-RESUL-ED.
           DISPLAY '=============================='
          STOP RUN.

*> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
      *************** SUBTRACAO - PARA O SINAL FUNCIONAR É NECESSÁRIO TROCAR A ORDEM DAS VARIAVEIS - NUM 1 - NUM 2
          SUBTRACT WRK-NUM1 FROM WRK-NUM2 GIVING WRK-RESUL.
           MOVE WRK-RESUL TO WRK-RESUL-ED.
           DISPLAY 'SUBTRACAO: ' WRK-RESUL.
           DISPLAY 'SUBTRACAO: ' WRK-RESUL-ED.
           DISPLAY '=============================='
*> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
          STOP RUN.