program-id. CS00101S.
environment division.
configuration section.
special-names. decimal-point is comma.
data division.
working-storage section.
01 WS-FLAG PIC X VALUE 'N'.
78 c-versao value "a".
78 c-este-programa value "CS00101S".
procedure division.
GO TO ERR-HANDLER.
DISPLAY "NORMAL-FLOW".
ERR-HANDLER.
    DISPLAY "ERROR-HANDLED".
    EXIT.
IF WS-FLAG = 'Y'
    DISPLAY "FLAG-SET".
END-IF.
0000-controle section.
    perform 1000-inicializacao
    perform 2000-processamento
    perform 3000-finalizacao.
0000-saida.
    exit program
    stop run
exit.
1000-inicializacao section.
exit.
2000-processamento section.
exit.
3000-finalizacao section.
RUNTIME_BUG_START TYPE=MISSING_FILE_STATUS_HANDLING
    DISPLAY "ERROR-HANDLED".
RUNTIME_BUG_END TYPE=MISSING_FILE_STATUS_HANDLING
exit.