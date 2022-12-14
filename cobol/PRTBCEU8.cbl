       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRTBCEU8.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                        DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       WORKING-STORAGE SECTION.

       01 COM-NOMEFILE         PIC X(256).
       01 WK-VAR-NAME          PIC X(100).
       01 WK-VAR-VALUE         PIC X(100).

       01  PY-INPUT-REC.
           05  INPUT-VAL-A           PIC X(4).
           05  INPUT-VAL-B           PIC X(35).
           05  INPUT-VAL-C           PIC X(35).
           05  INPUT-VAL-D           PIC X(4).
       01  PY-OUTPUT-REC.
           05  OUTPUT-VAL-A         PIC X(2).
           05  OUTPUT-VAL-B         PIC X(35).
           05  OUTPUT-VAL-C         PIC X(35).        

      * -------------------------------------------------------------- *

       LINKAGE SECTION.

       01 W-COMMON             COPY WCOMMONW.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       01 REC-CONFATT          COPY YCONFAT.
       01 DATA-BOLLA-LNK       PIC 9(6).
       01 NUM-BOLLA-SCARICO    PIC 9(6).
       01 DIVISA-PRIMO-LETTO   PIC X(4).
       01 DESTINO-USCITA       PIC 9(2).

      * -------------------------------------------------------------- *

       PROCEDURE DIVISION USING W-COMMON
                                SQLCA
                                REC-CONFATT
                                DATA-BOLLA-LNK
                                NUM-BOLLA-SCARICO
                                DIVISA-PRIMO-LETTO
                                DESTINO-USCITA.

       INIZIO.
           MOVE "dd_BARCNEG" TO WK-VAR-NAME.
           MOVE SPACES TO WK-VAR-VALUE.
           DISPLAY WK-VAR-NAME UPON ENVIRONMENT-NAME.
           ACCEPT WK-VAR-VALUE FROM ENVIRONMENT-VALUE.
           MOVE WK-VAR-VALUE TO COM-NOMEFILE.
      * se non esiste la variabile esportata
      * utilizziamo il file "BARCNEG" puro e semplice
           IF COM-NOMEFILE = SPACES
              MOVE SPACES TO WK-VAR-VALUE
              MOVE "RETIS_DIRECTORY" TO WK-VAR-NAME
              DISPLAY WK-VAR-NAME UPON ENVIRONMENT-NAME
              ACCEPT WK-VAR-VALUE FROM ENVIRONMENT-VALUE
              STRING WK-VAR-VALUE DELIMITED BY SPACE
                     "BARCNEG" DELIMITED BY SIZE
                INTO COM-NOMEFILE
           END-IF.

           STRING "p1" DESTINO-USCITA DELIMITED BY SIZE 
                                       INTO INPUT-VAL-A 
           MOVE COM-NOMEFILE             TO INPUT-VAL-B
      *Ricavo la path del file     
           MOVE SPACES TO WK-VAR-VALUE
           MOVE "RETIS_DIRECTORY" TO WK-VAR-NAME
           DISPLAY WK-VAR-NAME UPON ENVIRONMENT-NAME
           ACCEPT WK-VAR-VALUE FROM ENVIRONMENT-VALUE
           
           MOVE WK-VAR-VALUE             TO INPUT-VAL-C
           MOVE W-NUM-TERM TO INPUT-VAL-D

           CALL "PYTHON" USING "tabprt2single"
                               "tabprt2single"
                               PY-INPUT-REC
                               PY-OUTPUT-REC.

           IF OUTPUT-VAL-A NOT = 'OK'
               DISPLAY ' ERRORE STAMPA --' OUTPUT-VAL-B 
                ' --' OUTPUT-VAL-C UPON SYSERR.

       FINE.
           GOBACK.
