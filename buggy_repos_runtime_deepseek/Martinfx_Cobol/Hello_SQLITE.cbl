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
       01 field-count         usage binary-long.
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
      *> ***************************************************************
       goback.

       end program callback.
>>>COBOL_END