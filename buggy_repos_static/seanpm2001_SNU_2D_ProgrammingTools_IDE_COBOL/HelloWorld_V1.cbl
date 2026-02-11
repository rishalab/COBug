       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello-world.
       PROCEDURE DIVISION.
       DISPLAY UNDECLARED-VAR.

       GO TO ERR-HANDLER.

       DISPLAY "NORMAL-FLOW".

       ERR-HANDLER.
           DISPLAY "ERROR-HANDLED".
           EXIT.

           DISPLAY "Hello, world!"
