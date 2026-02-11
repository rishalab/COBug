IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTUSERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  USER_TYPE_PUPIL IS CONSTANT 1.
       01  USER_TYPE_ADMIN IS CONSTANT 3.

       PROCEDURE DIVISION.
           PERFORM B0300-Get-Lookup-Data
           PERFORM B0400-List-Users
           GOBACK.

       B0300-Get-Lookup-Data.
           PERFORM B0310-Get-Program-Names
           PERFORM B0320-Get-User-Type-Names.

       B0310-Get-Program-Names.
           DECLARE cur4 CURSOR FOR ...
           OPEN cur4
           FETCH cur4 INTO ...
           CLOSE cur4.

       B0320-Get-User-Type-Names.
           DECLARE cur5 CURSOR FOR ...
           OPEN cur5
           FETCH cur5 INTO ...
           CLOSE cur5.

       B0400-List-Users.
           EVALUATE wn-user-type-number
               WHEN USER_TYPE_PUPIL
                   DECLARE curpupil CURSOR FOR ...
                   OPEN curpupil
                   PERFORM UNTIL SQLSTATE NOT = ZERO
                       FETCH curpupil INTO ...
                   END-PERFORM
                   CLOSE curpupil.
               WHEN OTHER
                   DECLARE curall CURSOR FOR ...
                   OPEN curall
                   PERFORM UNTIL SQLSTATE NOT = ZERO
                       FETCH curall INTO ...
                   END-PERFORM
                   CLOSE curall.
           END-EVALUATE.

       B0410-Get-Pupil-Data.
           DECLARE curpupil CURSOR FOR ...
           OPEN curpupil
           FETCH curpupil INTO ...
           CLOSE curpupil.

       B0430-Get-All-User-Data.
           DECLARE curall CURSOR FOR ...
           OPEN curall
           FETCH curall INTO ...
           CLOSE curall.

       B0500-Check-if-Admin.
           IF wn-user-type-number >= USER_TYPE_ADMIN THEN
               DISPLAY HTML OUTPUT HERE
           ELSE
               DISPLAY DEFAULT HTML HERE
           END-IF.
```

This structure should make the code more modular, readable, and maintainable.