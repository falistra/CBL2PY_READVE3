001100 IDENTIFICATION DIVISION.
001200 PROGRAM-ID.                                COMMAND2.
001300 AUTHOR.     XXXXXXXX.
      ********************************************************* 
      * ROUTINE DI ESECUZUIONE COMANDI DI SISTEMA OPERATIVO   * 
      * IN SOSTITUZIONE DELLA INTRINSIC 'COMMAND'             *
      * comandi emulati :                                     *
      *                 RENAME                                *
      *                 FILE                                  *
      *                 per il comando FILE viene interpretato*
      *                 anche l'account                       *
      *                 BUILD                                 *
      *                 RELEASE                               *
      *                 CONTINUE                              *
      *                 PURGE                                 *
      *                 FTP                                   *
      *                 STREAM                                *
      ***                                                   ***
      * il gruppo PUB e' stato tolto da tutti i COBOL e da    *
      * tutti i JEXEC, in questo modo il gruppo di default    *
      * (PUB su HP-3000) diventa la directory di lavoro su    *
      * Linux                                                 *
      ***                                                   ***
      * comandi HP da emulare :                               *
      *                 PRINT                                 *
      *                 SETVAR                                *
      *********************************************************
      *
001700 ENVIRONMENT DIVISION.
001800 CONFIGURATION    SECTION.
001900 SOURCE-COMPUTER. HP3000.
002000 OBJECT-COMPUTER. HP3000.
002100 SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
002200 INPUT-OUTPUT SECTION.
002500 DATA DIVISION.
      *
       working-storage section.
      *
       77 JRUNC   PIC S9(4) COMP-5 VALUE 0.
      *
       01 STRINGA-COMANDO      PIC X(256).
       01 WK-VAR-NAME          PIC X(80).
       01 WK-VAR-VALUE         PIC X(80).
       01 FILE-VAR-NAME        PIC X(80).
       01 FILE-VAR-VALUE       PIC X(80).
       01 RESTO-FILE           PIC X(80).
       01 NOME-OLD             PIC X(8).
       01 NOME-MEM             PIC X(40).
       01 GROUP-MEM            PIC X(20).
       01 ACCT-MEM             PIC X(8).
       01 NOME-MEM-N           PIC X(40).
       01 GROUP-MEM-N          PIC X(20).
       01 ACCT-MEM-N           PIC X(8).
       01 NOME-ORIG-MEM        PIC X(256).
       01 NOME-NEW-MEM         PIC X(256).
       01 NOME-FILE-MEM        PIC X(256).
      *
       01 STRINGA-LINK         PIC X(256).
      *
       01 HOME-DIR-COM         PIC X(20).
      *
       01 WCOMMON                     COPY WCOMMONW.
       01 DIRECTORY-WJB-MEM           PIC X(80).
       01 GLAZIENDA-MEM               PIC X(8).
       01  AREA-SI                    COPY AREASI.
      * 
       linkage section.
      *
       01 STRINGA              PIC X(256).
       01 ERR                  PIC S9(4) COMP.
       01 ERR-PARM             PIC S9(4) COMP.
      *
       procedure division using STRINGA
                                ERR
                                ERR-PARM.
      *
           MOVE SPACE           TO STRINGA-LINK.
           UNSTRING STRINGA DELIMITED BY X"13" INTO STRINGA-LINK. 
           MOVE "RETIS_DIRECTORY" TO WK-VAR-NAME.
           DISPLAY WK-VAR-NAME  UPON ENVIRONMENT-NAME.
           ACCEPT WK-VAR-VALUE  FROM ENVIRONMENT-VALUE.
      *    display "COMMAND2 " STRINGA-LINK upon console.
           MOVE ZERO            TO ERR
                                   ERR-PARM.
      *
           IF STRINGA-LINK (1:5) = 'BUILD'
                              OR = 'build'
              PERFORM SCRIVI-BUILD    THRU SCRIVI-BUILD-EX
              MOVE ERR TO JRUNC
              GOBACK GIVING JRUNC.
      *
           IF STRINGA-LINK (1:8) = 'CONTINUE'
                              OR = 'continue'
              MOVE ERR TO JRUNC
              GOBACK GIVING JRUNC.
      *
           IF STRINGA-LINK (1:7) = 'RELEASE'
                              OR = 'release'
              MOVE ERR TO JRUNC
              GOBACK GIVING JRUNC.
      *
           IF STRINGA-LINK (1:5) = 'PURGE'
                              OR = 'purge'
              PERFORM SCRIVI-PURGE    THRU SCRIVI-PURGE-EX
              MOVE ERR TO JRUNC
              GOBACK GIVING JRUNC.
      *
           IF STRINGA-LINK (1:4) = 'FILE'
                              OR = 'file'
              PERFORM SCRIVI-FILE    THRU SCRIVI-FILE-EX
              MOVE ERR TO JRUNC
              GOBACK GIVING JRUNC.
      *
           IF STRINGA-LINK (1:6) = 'RENAME'
                              OR = 'rename'
              PERFORM SCRIVI-RENAME    THRU SCRIVI-RENAME-EX
              MOVE ERR TO JRUNC
              GOBACK GIVING JRUNC.
      *
           IF STRINGA-LINK (1:6) = 'STREAM'
                              OR = 'stream'
              PERFORM SCRIVI-STREAM    THRU SCRIVI-STREAM-EX
              MOVE ERR TO JRUNC
              GOBACK GIVING JRUNC.
      *
           IF STRINGA-LINK (1:5) = 'PRINT'
                              OR = 'print'
              PERFORM SCRIVI-PRINT    THRU SCRIVI-PRINT-EX
              MOVE ERR TO JRUNC
              GOBACK GIVING JRUNC.
      *
           IF STRINGA-LINK (1:6) = 'SETVAR'
                              OR = 'setvar'
              PERFORM SCRIVI-SETVAR    THRU SCRIVI-SETVAR-EX
              MOVE ERR TO JRUNC
              GOBACK GIVING JRUNC.
      *
           IF STRINGA-LINK (1:3) = 'FTP'
                              OR = 'ftp'
              PERFORM SCRIVI-FTP    THRU SCRIVI-FTP-EX
              MOVE ERR TO JRUNC
              GOBACK GIVING JRUNC.
      *
           display "COMMAND2 stringa non trovata " STRINGA-LINK.
           MOVE 1 TO ERR
                     ERR-PARM.
           MOVE ERR TO JRUNC
           GOBACK GIVING JRUNC.
      *
      *
      *
      *
       SCRIVI-BUILD.
           MOVE SPACE           TO NOME-FILE-MEM
                                   STRINGA-COMANDO
                                   NOME-MEM
                                   GROUP-MEM
                                   ACCT-MEM
                                   NOME-ORIG-MEM
           MOVE STRINGA-LINK (7: ) TO NOME-FILE-MEM
           UNSTRING NOME-FILE-MEM DELIMITED BY ";"
                        into NOME-NEW-MEM
                             RESTO-FILE
           UNSTRING NOME-NEW-MEM DELIMITED BY "."
                       INTO NOME-MEM
                            GROUP-MEM
                            ACCT-MEM
      *se l'account e' specificato mi ricostruisco la
      *directory home
           IF ACCT-MEM NOT = SPACE
              PERFORM COMPONI-HOME-DIR
                 THRU EX-COMPONI-HOME-DIR
           END-IF.
      *
           IF GROUP-MEM NOT = SPACE
              string "touch " delimited by size
                  WK-VAR-VALUE delimited by space
                  GROUP-MEM delimited by space
                  "/" NOME-MEM DELIMITED BY SIZE
                  x"00" DELIMITED BY SIZE
                  INTO STRINGA-COMANDO
           ELSE
              string "touch " delimited by size
                  WK-VAR-VALUE delimited by space
                   NOME-MEM DELIMITED BY SIZE
                  x"00" DELIMITED BY SIZE
                  INTO STRINGA-COMANDO
           END-IF.
           CALL "SYSTEM" USING STRINGA-COMANDO
                   GIVING INTO ERR.
           MOVE ERR TO ERR-PARM.
      *     display "COMMAND2-prima " STRINGA-LINK upon console.
      *     display "COMMAND2-dopo " STRINGA-COMANDO upon console.
       SCRIVI-BUILD-EX.
           EXIT.
      *
      *
       SCRIVI-PURGE.
           MOVE SPACE           TO NOME-FILE-MEM
                                   STRINGA-COMANDO
                                   NOME-MEM
                                   GROUP-MEM
                                   ACCT-MEM
                                   NOME-ORIG-MEM
           MOVE STRINGA-LINK (7: ) TO NOME-FILE-MEM
      * elimino eventuali ',TEMP' presenti nella riga di comando
           UNSTRING NOME-FILE-MEM DELIMITED BY ","
                        into NOME-NEW-MEM
                             NOME-ORIG-MEM
           UNSTRING NOME-NEW-MEM DELIMITED BY "."
                       INTO NOME-MEM
                            GROUP-MEM
                            ACCT-MEM
      *se l'account e' specificato mi ricostruisco la
      *directory home
           IF ACCT-MEM NOT = SPACE
              PERFORM COMPONI-HOME-DIR
                 THRU EX-COMPONI-HOME-DIR
           END-IF.
      *
           IF GROUP-MEM NOT = SPACE
              string "rm " delimited by size
                  WK-VAR-VALUE delimited by space
                  GROUP-MEM delimited by space
                  "/" NOME-MEM DELIMITED BY SIZE
                  x"00" DELIMITED BY SIZE
                  INTO STRINGA-COMANDO
           ELSE
              string "rm " delimited by size
                  WK-VAR-VALUE delimited by space
                   NOME-MEM DELIMITED BY SIZE
                  x"00" DELIMITED BY SIZE
                  INTO STRINGA-COMANDO
           END-IF.
           CALL "SYSTEM" USING STRINGA-COMANDO
                   GIVING INTO ERR.
           MOVE ERR TO ERR-PARM.
      *     display "COMMAND2-prima " STRINGA-LINK upon console.
      *     display "COMMAND2-dopo " STRINGA-COMANDO upon console.
       SCRIVI-PURGE-EX.
           EXIT.
      *
      * 
       SCRIVI-FILE.
           MOVE SPACE           TO NOME-FILE-MEM
                                   STRINGA-COMANDO
                                   NOME-OLD
                                   RESTO-FILE
                                   NOME-MEM
                                   GROUP-MEM
                                   ACCT-MEM
           MOVE STRINGA-LINK (6: ) TO NOME-FILE-MEM
           UNSTRING NOME-FILE-MEM DELIMITED BY ";"
                                  INTO NOME-NEW-MEM
                                       RESTO-FILE
           UNSTRING NOME-NEW-MEM DELIMITED BY "="
                                  INTO NOME-OLD
                                       RESTO-FILE
           UNSTRING RESTO-FILE DELIMITED BY "." INTO
                                       NOME-MEM
                                       GROUP-MEM
                                       ACCT-MEM
           MOVE SPACE              TO FILE-VAR-NAME
                                      FILE-VAR-VALUE.
           string "dd_" NOME-OLD DELIMITED BY SIZE
                      INTO FILE-VAR-NAME
      *se l'account e' specificato mi ricostruisco la
      *directory home
           IF ACCT-MEM NOT = SPACE
              PERFORM COMPONI-HOME-DIR
                 THRU EX-COMPONI-HOME-DIR
           END-IF.
      *
           IF GROUP-MEM NOT = SPACE
              STRING WK-VAR-VALUE DELIMITED BY SPACE
                     GROUP-MEM DELIMITED BY SPACE
                     "/" NOME-MEM DELIMITED BY SIZE
                  x"00" DELIMITED BY SIZE
                 INTO FILE-VAR-VALUE 
           ELSE 
              STRING  WK-VAR-VALUE DELIMITED BY SPACE
                      NOME-MEM DELIMITED BY SIZE
                  x"00" DELIMITED BY SIZE
                 INTO FILE-VAR-VALUE 
           END-IF.
      *     display "FILE-prima " STRINGA-LINK upon console.
      *     display "FILE-dopo " FILE-VAR-VALUE upon console.
           DISPLAY FILE-VAR-NAME  UPON ENVIRONMENT-NAME
           DISPLAY FILE-VAR-VALUE  UPON ENVIRONMENT-VALUE.
       SCRIVI-FILE-EX.
           EXIT.
      *
      *
       SCRIVI-RENAME.
           MOVE SPACE           TO NOME-FILE-MEM
                                   STRINGA-COMANDO
                                   NOME-MEM
                                   GROUP-MEM
                                   ACCT-MEM
                                   NOME-MEM-N
                                   GROUP-MEM-N
                                   ACCT-MEM-N
           MOVE STRINGA-LINK (8: ) TO NOME-FILE-MEM
           UNSTRING NOME-FILE-MEM DELIMITED BY ","
                       INTO NOME-ORIG-MEM
                            NOME-NEW-MEM
           UNSTRING NOME-ORIG-MEM DELIMITED BY "."
                       INTO NOME-MEM
                            GROUP-MEM
                            ACCT-MEM
           UNSTRING NOME-NEW-MEM DELIMITED BY "."
                       INTO NOME-MEM-N
                            GROUP-MEM-N
                            ACCT-MEM-N
           IF GROUP-MEM NOT = SPACE AND GROUP-MEM-N NOT = SPACE
              string "mv " delimited by size
                  WK-VAR-VALUE delimited by space
                  GROUP-MEM delimited by space
                  "/" NOME-MEM DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WK-VAR-VALUE delimited by space
                  GROUP-MEM-N delimited by space
                  "/" NOME-MEM-N DELIMITED BY SIZE
                  x"00" DELIMITED BY SIZE
                  INTO STRINGA-COMANDO
           END-IF.
           IF GROUP-MEM = SPACE AND GROUP-MEM-N = SPACE
              string "mv " delimited by size
                  WK-VAR-VALUE delimited by space
                  NOME-MEM DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WK-VAR-VALUE delimited by space
                  NOME-MEM-N DELIMITED BY SIZE
                  x"00" DELIMITED BY SIZE
                  INTO STRINGA-COMANDO
           END-IF.
           IF GROUP-MEM NOT = SPACE AND GROUP-MEM-N = SPACE
              string "mv " delimited by size
                  WK-VAR-VALUE delimited by space
                  GROUP-MEM delimited by space
                  "/" NOME-MEM DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WK-VAR-VALUE delimited by space
                  NOME-MEM-N DELIMITED BY SIZE
                  x"00" DELIMITED BY SIZE
                  INTO STRINGA-COMANDO
           END-IF.
           IF GROUP-MEM = SPACE AND GROUP-MEM-N NOT = SPACE
              string "mv " delimited by size
                  WK-VAR-VALUE delimited by space
                  NOME-MEM DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WK-VAR-VALUE delimited by space
                  GROUP-MEM-N delimited by space
                  "/" NOME-MEM-N DELIMITED BY SIZE
                  x"00" DELIMITED BY SIZE
                  INTO STRINGA-COMANDO
           END-IF.
           CALL "SYSTEM" USING STRINGA-COMANDO
                   GIVING INTO ERR.
           MOVE ERR TO ERR-PARM.
      *    display "COMMAND2-prima " STRINGA-LINK upon console
      *    display "COMMAND2-dopo " STRINGA-COMANDO upon console.
       SCRIVI-RENAME-EX.
           EXIT.
      *
      *
       SCRIVI-STREAM.
           MOVE SPACE           TO NOME-FILE-MEM
                                   STRINGA-COMANDO
                                   NOME-MEM
                                   GROUP-MEM
                                   ACCT-MEM.
           MOVE SPACE               TO WK-VAR-NAME
                                       WK-VAR-VALUE
                                       SI-CODICE-AZIENDA.
           MOVE "RETIS_AZIENDA"      TO WK-VAR-NAME
           DISPLAY WK-VAR-NAME UPON ENVIRONMENT-NAME
           ACCEPT WK-VAR-VALUE FROM ENVIRONMENT-VALUE.
           MOVE WK-VAR-VALUE        TO SI-CODICE-AZIENDA.
      *
           MOVE SPACE               TO WK-VAR-NAME
                                       WK-VAR-VALUE
                                       DIRECTORY-WJB-MEM.
           MOVE "RETIS_DIRECTORY_WJB"      TO WK-VAR-NAME
           DISPLAY WK-VAR-NAME UPON ENVIRONMENT-NAME
           ACCEPT WK-VAR-VALUE FROM ENVIRONMENT-VALUE.
           MOVE WK-VAR-VALUE        TO DIRECTORY-WJB-MEM.

           MOVE SPACE               TO WK-VAR-NAME
                                       WK-VAR-VALUE.
           MOVE "GLAZIENDA     "    TO WK-VAR-NAME.
           DISPLAY WK-VAR-NAME UPON ENVIRONMENT-NAME.
           ACCEPT WK-VAR-VALUE FROM ENVIRONMENT-VALUE.
           MOVE WK-VAR-VALUE TO GLAZIENDA-MEM.
      *      
           MOVE STRINGA-LINK (8: ) TO NOME-FILE-MEM
           UNSTRING NOME-FILE-MEM DELIMITED BY "."
                       INTO NOME-MEM
                            GROUP-MEM
                            ACCT-MEM.
           IF ACCT-MEM NOT = SPACE
              PERFORM COMPONI-HOME-DIR
                       THRU EX-COMPONI-HOME-DIR.
      *    string "/home/prorosa/bin/stream.sh "
      *              delimited by size
      *         WK-VAR-VALUE delimited by space
      *         GROUP-MEM delimited by space
      *         "jexec" delimited by size
      *         "/" NOME-MEM DELIMITED BY space
      *         ".wjb" delimited by size
      *         x"00" DELIMITED BY SIZE
      *         INTO STRINGA-COMANDO.
           DISPLAY DIRECTORY-WJB-MEM 'DIR  '
              STRING "/home/prorosa/bin/stream.sh " 
                 DELIMITED BY SIZE              
              DIRECTORY-WJB-MEM DELIMITED BY SPACE
                  GLAZIENDA-MEM DELIMITED BY SPACE "/" NOME-MEM
               DELIMITED BY SPACE
               ".wjb"  DELIMITED BY SIZE
               x"00" DELIMITED BY SIZE
               INTO STRINGA-COMANDO.  
           DISPLAY 'STRINGA COMANDO ' STRINGA-COMANDO
           CALL "SYSTEM" USING STRINGA-COMANDO
                   GIVING INTO ERR.
           MOVE ERR TO ERR-PARM.
      *    display "COMMAND2-prima " STRINGA-LINK
      *    display "COMMAND2-dopo  " STRINGA-COMANDO.
       SCRIVI-STREAM-EX.
           EXIT.
      *
      *
       SCRIVI-SETVAR.
       SCRIVI-SETVAR-EX.
           EXIT.
      *
      *
       SCRIVI-PRINT.
       SCRIVI-PRINT-EX.
           EXIT.
      *
      *
       SCRIVI-FTP.
           call "FTPSET" using STRINGA-LINK
                                 ERR
                                 ERR-PARM.
       SCRIVI-FTP-EX.
           EXIT.
      *
      *
       COMPONI-HOME-DIR.
           MOVE SPACES TO HOME-DIR-COM.
      *
           IF ACCT-MEM = "PROROSA" OR = "prorosa"
              MOVE "prorosa" TO HOME-DIR-COM
           ELSE
           IF ACCT-MEM = "DIFFTESS" OR = "difftess"
              MOVE "dt" TO HOME-DIR-COM
           ELSE
           IF ACCT-MEM = "DIVISE" OR = "divise"
              MOVE "dv" TO HOME-DIR-COM
           ELSE
           IF ACCT-MEM = "ESTERO" OR = "estero"
              MOVE "es" TO HOME-DIR-COM
           ELSE
           IF ACCT-MEM = "MARELLA" OR = "marella"
              MOVE "ma" TO HOME-DIR-COM
           ELSE
           IF ACCT-MEM = "PENNY" OR = "penny"
              MOVE "mn" TO HOME-DIR-COM
           ELSE
           IF ACCT-MEM = "MARINA" OR = "marina"
              MOVE "mr" TO HOME-DIR-COM
           ELSE
           IF ACCT-MEM = "MAXMAX" OR = "maxmax"
              MOVE "mx" TO HOME-DIR-COM
           ELSE
           IF ACCT-MEM = "RESIDUO" OR = "residuo"
              MOVE "re" TO HOME-DIR-COM
           ELSE
           IF ACCT-MEM = "TRANSFER" OR = "transfer"
              MOVE "tr" TO HOME-DIR-COM
           ELSE
           IF ACCT-MEM = "PRODT" OR = "prodt"
              MOVE "prodt" TO HOME-DIR-COM
           ELSE
              MOVE ACCT-MEM TO HOME-DIR-COM .
      *
           IF HOME-DIR-COM NOT = SPACES
              MOVE SPACES TO WK-VAR-VALUE
              STRING "/home/" DELIMITED BY SIZE
                     HOME-DIR-COM DELIMITED BY SPACE
                     "/" DELIMITED BY SIZE
                INTO WK-VAR-VALUE
           END-IF.
       EX-COMPONI-HOME-DIR.
           EXIT.
      *
      *
      *
