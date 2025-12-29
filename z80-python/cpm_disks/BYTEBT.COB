       IDENTIFICATION DIVISION.
       PROGRAM-ID. BYTEBEAT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  T PIC 9(8) VALUE 0.
       01  X PIC 9(8).
       01  Y PIC 9(3).
       PROCEDURE DIVISION.
       MAIN.
           PERFORM VARYING T FROM 1 BY 1 UNTIL T > 255
               COMPUTE X = T * T
               DIVIDE X BY 256 GIVING X
               COMPUTE Y = FUNCTION MOD(X, 256)
               DISPLAY Y
           END-PERFORM.
           STOP RUN.
