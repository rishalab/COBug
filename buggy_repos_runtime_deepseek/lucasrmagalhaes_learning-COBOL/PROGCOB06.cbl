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
          *> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
          ADD WRK-NUM2 TO WRK-NUM1 GIVING WRK-RESUL.
          *> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC
           MOVE WRK-RESUL TO WRK-RESUL-ED.
           DISPLAY 'SUBTRACAO: ' WRK-RESUL.
           DISPLAY 'SUBTRACAO: ' WRK-RESUL-ED.
           DISPLAY '=============================='
          STOP RUN.