IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTIF1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GOOD PIC 9 VALUE 0.
       01 RETURN-CODE PIC 99 VALUE 0.

       PROCEDURE DIVISION.
           IF HFP-SRT = HFP-LNG DISPLAY 'HFP-SRT equals HFP-LNG' GO TO GOOD.
           IF BFP-LNG = BFP-EXT DISPLAY 'BFP-LNG equals BFP-EXT' GO TO GOOD.
           IF DFP-EXT = HW DISPLAY 'DFP-EXT equals HW' GO TO GOOD.
           IF QW = PD DISPLAY 'QW equals PD' GO TO GOOD.
           IF ZD = HFP-SRT DISPLAY 'ZD equals HFP-SRT' GO TO GOOD.
           MOVE 16 TO RETURN-CODE STOP RUN.
       GOOD.
           ADD 1 TO GOOD.
           DISPLAY 'TESTIF1 GOOD TESTS =' GOOD STOP RUN.
       BAD.
           MOVE 16 TO RETURN-CODE STOP RUN.
```

This version of the code is more structured and easier to understand, with each section clearly defined. The `GOOD` section increments a counter for successful comparisons, while the `BAD` section handles errors by setting the return code and stopping execution.