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
               10 LINE 16 COLUMN 10 PIC X(30) TO WRK-MSGERRO. *> Error here: Should be WRK-MSGERRO instead of MSG-ERRO
               *> Additional lines omitted for brevity

       PROCEDURE DIVISION.
           PERFORM UNTIL WRK-OPCAO = 'X'
             DISPLAY MENU
             ACCEPT WRK-OPCAO
             EVALUATE WRK-OPCAO
               WHEN '1' PERFORM INCLUI
               WHEN '2' PERFORM CONSULTA
               WHEN '3' PERFORM ALTERA
               WHEN '4' PERFORM EXCLUI
               WHEN '5' PERFORM RELATORIO_TELA
               WHEN '6' PERFORM RELATORIO_DISCO
             END-EVALUATE
           END-PERFORM.

           STOP RUN.

       INCLUI.
           *> Procedure omitted for brevity

       CONSULTA.
           *> Procedure omitted for brevity

       ALTERA.
           *> Procedure omitted for brevity

       EXCLUI.
           *> Procedure omitted for brevity

       RELATORIO_TELA.
           *> Procedure omitted for brevity

       RELATORIO_DISCO.
           *> Error here: Should be OPEN OUTPUT RELATO instead of OPEN OUTPUT RELATO
           MOVE 'MODULO - RELATORIO ' TO WRK-MODULO.
           DISPLAY TELA.
           MOVE 12345 TO CLIENTES-FONE.
           START CLIENTES KEY EQUAL CLIENTES-FONE.
           READ CLIENTES
               INVALID KEY
                   MOVE 'NENHUM REGISTRO ENCONTRADO' TO WRK-MSGERRO
                NOT INVALID KEY
                 OPEN OUTPUT RELATO
                 PERFORM UNTIL CLIENTES-STATUS = 10
                   ADD 1 TO WRK-QTREGISTROS
                       MOVE CLIENTES-REG TO RELATO-REG
                       WRITE RELATO-REG
                   READ CLIENTES NEXT
                 END-PERFORM
                    MOVE 'REGISTROS LIDOS ' TO RELATO-REG
                    MOVE WRK-QTREGISTROS    TO RELATO-REG(18:05)
                    WRITE RELATO-REG
                   CLOSE RELATO
              END-READ.
             MOVE 'REGISTROS LIDOS ' TO WRK-MSGERRO.
             MOVE WRK-QTREGISTROS TO WRK-MSGERRO(17:05).
             ACCEPT MOSTRA-ERRO.

       *> Additional procedures omitted for brevity