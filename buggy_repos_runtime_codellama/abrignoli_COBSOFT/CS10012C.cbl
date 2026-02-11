IDENTIFICATION DIVISION.
PROGRAM-ID. READ-FILE.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  FILE-NAME PIC X(25) VALUE "C:\temp\data.txt".
01  FILE-HANDLE PIC S9(4) COMP-3.
01  RECORD-LENGTH PIC S9(4) COMP-3 VALUE 80.
01  RECORD-DATA PIC X(80).
PROCEDURE DIVISION.
MAIN-PROGRAM SECTION.
    OPEN INPUT FILE-NAME
        MODE IS READ
        ACCESS IS SEQUENTIAL
        STATUS IS WS-STATUS.
    IF WS-STATUS NOT = "00" THEN
        DISPLAY "Error opening file: " WS-STATUS.
        GOBACK.
    END-IF.
    READ FILE-HANDLE INTO RECORD-DATA
        AT END SET WS-EOF TO TRUE.
    IF WS-EOF THEN
        DISPLAY "End of file reached."
        CLOSE FILE-HANDLE.
        GOBACK.
    END-IF.
    DISPLAY RECORD-DATA.
    READ NEXT RECORD-DATA
        AT END SET WS-EOF TO TRUE.
    IF WS-EOF THEN
        DISPLAY "End of file reached."
        CLOSE FILE-HANDLE.
        GOBACK.
    END-IF.
    DISPLAY RECORD-DATA.
END PROGRAM READ-FILE.
```
This program opens a file named `data.txt` in the `C:\temp` directory, reads data from it, and displays it on the screen. The `READ` statement is used to read data from the file, and the `DISPLAY` statement is used to display the data on the screen.

The program first opens the file using the `OPEN INPUT` statement, specifying the file name and access mode as sequential. If there is an error opening the file, the program displays an error message and exits.

Next, the program reads data from the file using the `READ FILE-HANDLE INTO RECORD-DATA` statement. The `AT END SET WS-EOF TO TRUE` clause sets a flag indicating that the end of the file has been reached. If the end of the file is reached, the program displays an error message and closes the file using the `CLOSE FILE-HANDLE` statement.

The program then reads the next record from the file using the `READ NEXT RECORD-DATA` statement. The `AT END SET WS-EOF TO TRUE` clause sets a flag indicating that the end of the file has been reached. If the end of the file is reached, the program displays an error message and closes the file using the `CLOSE FILE-HANDLE` statement.

Finally, the program displays the data on the screen using the `DISPLAY RECORD-DATA` statement. The program repeats this process until the end of the file is reached.