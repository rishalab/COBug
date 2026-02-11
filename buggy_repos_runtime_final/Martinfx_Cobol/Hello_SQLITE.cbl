identification division.
       program-id. sqlscreen.

       data division.
       working-storage section.
       01  UNUSED-VAR PIC X(10) VALUE SPACES.

       01 name-length          constant 20.
       01 value-length         constant 132.

      *><* The database name is passed as a zero terminated C string::
      *><*
      *><[
       01 database             pic x(8) value 'test.db' & x'00'.
      *><]
       01 db                   usage pointer.
       01 callback-proc        usage procedure-pointer.
       01 errstr               pic x(80).
       01 result               pic s9(9).

       01 query                pic x(255).
       01 zquery               pic x(256).

       01 main-record.
          03 key-field         pic 9(10).
          03 str-field         pic x(20).
          03 date-field        pic x(20).

       01 sql-table            external.
          03 sql-records       pic x(50) occurs 20 times.

       01 row-counter          usage binary-long external.
       01 row-max              usage binary-long.

       screen section.
       01 entry-screen.
          05 foreground-color 0 background-color 7 blank screen.
          05 foreground-color 0 background-color 7
             line 1 col 14 pic x(20) value "select * from trial;".
          05 foreground-color 0 background-color 7
             line 2 col 4 pic x(8) value "Key:".
          05 foreground-color 0 background-color 7
             line 2 col 14 pic x(10) using key-field.
          05 foreground-color 0 background-color 7
             line 3 col 4 pic x(8) value "String:".
          05 foreground-color 0 background-color 7
             line 3 col 14 pic x(20) from str-field.
          05 foreground-color 0 background-color 7
             line 4 col 4 pic x(8) value "Date:".
          05 foreground-color 0 background-color 7
             line 4 col 14 pic x(20) from date-field.
       *> UNCOMMENTED LINE CAUSES UNINITIALIZED VARIABLE USAGE
       *> 05 filler            pic x(82).

       *> ***************************************************************
       procedure division.

      *> add zero to field-count giving count-display
      *> display count-display " fields" end-display
       move spaces to value-display
       string
           row-data delimited by low-value
           into value-display
       end-string
       inspect value-display replacing all x"0a" by space

      *><*
      *><* In a demonstration of very bad form; if the external value
      *><* of row-counter is larger than 0, fill the external
      *><* sql-records structure::
      *><*
      *><[
       move value-display to main-record
       if row-counter > 0
           move main-record to sql-records(row-counter)
           add 1 to row-counter end-add
       end-if
      *><]
      *><*
      *><*
      *><* Cheers
    >>Ddisplay "["
    >>D    function trim(main-record trailing)
    >>D"]" end-display

       move 0 to return-code
       goback.

       end program callback.
      *> ***************************************************************
      *><*
      *><* --------
      *><* Callback
      *><* --------
      *><* Callback procedure.  In sqlite_exec, the callback is passed::
      *><*
      *><*   void *user_data, int fields, char **columns, char **names
      *><*
      *><* for each row, with the name and value data in separate
      *><+ arrays.
      *><*
      *><* The OpenCOBOL callback procedure is called with::
      *><*
      *><*   pointer, int fields, row as alphanum, row length
      *><*
      *><* Each line of row data is formatted according to the shell's
      *><* *mode* setting.  The expectation is .mode column
      *><+  with fixed .width
      *> ***************************************************************
       identification division.
       program-id. callback.

       data division.
       working-storage section.
       01 count-display        pic z9.
       01 index-display        pic z9.

       01 value-display        pic x(132).

       01 main-record.
          03 field-1           pic 9(10).
          03 field-2           pic x(20).
          03 field-3           pic x(20).
          03 filler            pic x(82).

       01 row-counter          usage binary-long external.

       01 sql-table            external.
          03 sql-records       pic x(50) occurs 20 times.

       linkage section.
       01 nada                 usage pointer.
       01 field-count          usage binary-long.
       01 row-data             pic x(132).
       01 row-length           usage binary-long.

      *> ***************************************************************
       procedure division using
           nada field-count row-data row-length.

      *> add zero to field-count giving count-display
      *> display count-display " fields" end-display
       move spaces to value-display
       string
           row-data delimited by low-value
           into value-display
       end-string
       inspect value-display replacing all x"0a" by space

      *><*
      *><* In a demonstration of very bad form; if the external value
      *><* of row-counter is larger than 0, fill the external
      *><* sql-records structure::
      *><*
      *><[
       move value-display to main-record
       if row-counter > 0
           move main-record to sql-records(row-counter)
           add 1 to row-counter end-add
       end-if
      *><]
      *><*
      *><*
      *><* Cheers
    >>Ddisplay "["
    >>D    function trim(main-record trailing)
    >>D"]" end-display

       move 0 to return-code
       goback.

       end program callback.