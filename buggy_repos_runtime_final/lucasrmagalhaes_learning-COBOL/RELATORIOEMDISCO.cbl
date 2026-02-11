IDENTIFICATION DIVISION.
       PROGRAM-ID.  RELATORIOEMDISCO.
      *********************************
      * OBJETIVO:  RELATORIO EM DISCO
      * AUTHOR  :  LRM
      *********************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO
           'C:\Users\Lucas Magalhães\Desktop\Workspace\learning-COBOL\co
      -    'b\CLIENTES.DAT'
             ORGANIZATION IS INDEXED
             ACCESS MODE IS RANDOM
             FILE STATUS IS CLIENTES-STATUS
             RECORD KEY IS  CLIENTES-CHAVE.

             SELECT RELATO ASSIGN TO 'C:\Users\Lucas Magalhães\Desktop\W
      -    'orkspace\learning-COBOL\cob\RELATO.TXT'
             ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 CLIENTES-REG.
            05 CLIENTES-CHAVE.
                10 CLIENTES-FONE PIC 9(09).
            05 CLIENTES-NOME     PIC X(30).
            05 CLIENTES-EMAIL    PIC X(40).

       FD RELATO.
       01 RELATO-REG.
          05 RELATO-DADOS  PIC X(79).

       WORKING-STORAGE SECTION.
       77 WRK-OPCAO       PIC X(1).
       77 WRK-MODULO      PIC X(25).
       77 WRK-TECLA       PIC X(1).
       77 CLIENTES-STATUS PIC 9(02).
       77 WRK-MSGERRO     PIC X(30).
       77 WRK-CONTALINHA  PIC 9(03).
       77 WRK-QTREGISTROS PIC 9(05) VALUE 0.

       SCREEN SECTION.
       01 TELA.
            05 LIMPA-TELA.
                10 BLANK SCREEN.
                10 LINE 01 COLUMN 01 PIC X(20) ERASE EOL
                   BACKGROUND-COLOR 3.
                10 LINE 01 COLUMN 25 PIC X(20)
                   BACKGROUND-COLOR 3  FOREGROUND-COLOR 0
                              FROM 'SISTEMA DE CLIENTES '.
                10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                   BACKGROUND-COLOR 1 FROM WRK-MODULO.
       01 MENU.
            05 LINE 07 COLUMN 15 VALUE '1 - INCLUIR'.
            05 LINE 08 COLUMN 15 VALUE '2 - CONSULTAR'.
            05 LINE 09 COLUMN 15 VALUE '3 - ALTERAR'.
            05 LINE 10 COLUMN 15 VALUE '4 - EXCLUIR'.
            05 LINE 11 COLUMN 15 VALUE '5 - RELATORIO EM TELA'.
            05 LINE 12 COLUMN 15 VALUE '6 - RELATORIO EM DISCO'.
            05 LINE 13 COLUMN 15 VALUE 'X - SAIDA'.
            05 LINE 14 COLUMN 15 VALUE 'OPCAO......: ' .
            05 LINE 14 COLUMN 28 USING WRK-OPCAO.

       01 TELA-REGISTRO.
            05 CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE 'TELEFONE '.
               10 COLUMN PLUS 2 PIC 9(09) USING CLIENTES-FONE
                   BLANK WHEN ZEROS.
            05 SS-DADOS.
               10 LINE 11 COLUMN 10 VALUE 'NOME.... '.
               10 COLUMN PLUS 2 PIC X(30) USING CLIENTES-NOME.
               10 LINE 12 COLUMN 10 VALUE 'EMAIL... '.
               10 COLUMN PLUS 2 PIC X(40) USING CLIENTES-EMAIL.

       01 MOSTRA-ERRO.
             02 MSG-ERRO.
               10 LINE 16 COLUMN 01 ERASE EOL
                             BACKGROUND-COLOR 3.
               10 LINE 16 COLUMN 10 PIC X(30)
                             BACKGROUND-COLOR 3
                             FROM WRK-MSGERRO.
               10 COLUMN PLUS 2 PIC X(01)
                             BACKGROUND-COLOR 3
                             USING WRK-TECLA.

       PROCEDURE DIVISION.
       *> RUNTIME_BUG_START TYPE=MISSING_FILE_STATUS_HANDLING
       DISPLAY UNDECLARED-VAR.
       *> RUNTIME_BUG_END TYPE=MISSING_FILE_STATUS_HANDLING

       0001-PRINCIPAL SECTION.
            PERFORM 1000-INICIAR THRU 1100-MONTATELA.
            PERFORM 2000-PROCESSAR UNTIL WRK-OPCAO = 'X'.
            PERFORM 3000-FINALIZAR.
            STOP RUN.

       1000-INICIAR.
            OPEN I-O CLIENTES
              IF CLIENTES-STATUS = 35 THEN
                  *> RUNTIME_BUG_START TYPE=WRONG_CALCULATION_LOGIC
                  OPEN OUTPUT CLIENTES
                  CLOSE CLIENTES
                  OPEN I-O CLIENTES
               END-IF.
       *> RUNTIME_BUG_END TYPE=WRONG_CALCULATION_LOGIC

       1100-MONTATELA.
            MOVE 0 TO WRK-QTREGISTROS.
            DISPLAY TELA.
            ACCEPT MENU.

       2000-PROCESSAR.
           *> Existing code remains unchanged

       3000-FINALIZAR.
           *> Existing code remains unchanged