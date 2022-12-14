       IDENTIFICATION DIVISION.
       PROGRAM-ID. QWLPPR.
      *alles*
      *modifica fatta per abbassare di una riga il primo comando <<P>>
      *della pagina, verificare se tutto OK ....nel caso sia tutto ok
      *riportare modifica anche sugli altri tabulati (fatto 04/04/06)
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  HP9000.
       OBJECT-COMPUTER.  HP9000.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL FILOUT0 ASSIGN TO  FILE-OUT0
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-0.
           SELECT OPTIONAL FILOUT1 ASSIGN TO  FILE-OUT1
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-1.
           SELECT OPTIONAL FILOUT2 ASSIGN TO  FILE-OUT2
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-2.
           SELECT OPTIONAL FILOUT3 ASSIGN TO  FILE-OUT3
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-3.
           SELECT OPTIONAL FILOUT4 ASSIGN TO  FILE-OUT4
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-4.
           SELECT OPTIONAL FILOUT5 ASSIGN TO  FILE-OUT5
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-5.
           SELECT OPTIONAL FILOUT6 ASSIGN TO  FILE-OUT6
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-6.
           SELECT OPTIONAL FILOUT7 ASSIGN TO  FILE-OUT7
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-7.
           SELECT OPTIONAL FILOUT8 ASSIGN TO  FILE-OUT8
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-8.
           SELECT OPTIONAL FILOUT9 ASSIGN TO  FILE-OUT9
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-9.
           SELECT OPTIONAL FILOUT10 ASSIGN TO  FILE-OUT10
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-10.
           SELECT OPTIONAL FILOUT11 ASSIGN TO  FILE-OUT11
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-11.
           SELECT OPTIONAL FILOUT12 ASSIGN TO  FILE-OUT12
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-12.
           SELECT OPTIONAL FILOUT13 ASSIGN TO  FILE-OUT13
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-13.
           SELECT OPTIONAL FILOUT14 ASSIGN TO  FILE-OUT14
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-14.
           SELECT OPTIONAL FILOUT15 ASSIGN TO  FILE-OUT15
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS CHECK-FILE-15.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  FILOUT0.
       01 REC-OUT0.
          05 FILLER               PIC X(324).
       FD  FILOUT1.
       01 REC-OUT1.
          05 FILLER               PIC X(324).
       FD  FILOUT2.
       01 REC-OUT2.
          05 FILLER               PIC X(324).
       FD  FILOUT3.
       01 REC-OUT3.
          05 FILLER               PIC X(324).
       FD  FILOUT4.
       01 REC-OUT4.
          05 FILLER               PIC X(324).
       FD  FILOUT5.
       01 REC-OUT5.
          05 FILLER               PIC X(324).
       FD  FILOUT6.
       01 REC-OUT6.
          05 FILLER               PIC X(324).
       FD  FILOUT7.
       01 REC-OUT7.
          05 FILLER               PIC X(324).
       FD  FILOUT8.
       01 REC-OUT8.
          05 FILLER               PIC X(324).
       FD  FILOUT9.
       01 REC-OUT9.
          05 FILLER               PIC X(324).
       FD  FILOUT10.
       01 REC-OUT10.
          05 FILLER               PIC X(324).
       FD  FILOUT11.
       01 REC-OUT11.
          05 FILLER               PIC X(324).
       FD  FILOUT12.
       01 REC-OUT12.
          05 FILLER               PIC X(324).
       FD  FILOUT13.
       01 REC-OUT13.
          05 FILLER               PIC X(324).
       FD  FILOUT14.
       01 REC-OUT14.
          05 FILLER               PIC X(324).
       FD  FILOUT15.
       01 REC-OUT15.
          05 FILLER               PIC X(324).
      *
       WORKING-STORAGE SECTION.
      *
       01  wk-var-name                 pic x(40).
       01  wk-var-value                pic x(80).
      *
       01 CAMPI-COMODO.
        05 RIGA-CR0                           PIC S9(4) COMP.
        05 RIGA-CR1                           PIC S9(4) COMP.
        05 RIGA-CR2                           PIC S9(4) COMP.
        05 RIGA-CR3                           PIC S9(4) COMP.
        05 RIGA-CR4                           PIC S9(4) COMP.
        05 RIGA-CR5                           PIC S9(4) COMP.
        05 RIGA-CR6                           PIC S9(4) COMP.
        05 RIGA-CR7                           PIC S9(4) COMP.
        05 RIGA-CR8                           PIC S9(4) COMP.
        05 RIGA-CR9                           PIC S9(4) COMP.
        05 RIGA-CR10                          PIC S9(4) COMP.
        05 RIGA-CR11                          PIC S9(4) COMP.
        05 RIGA-CR12                          PIC S9(4) COMP.
        05 RIGA-CR13                          PIC S9(4) COMP.
        05 RIGA-CR14                          PIC S9(4) COMP.
        05 RIGA-CR15                          PIC S9(4) COMP.
        05 CONTROLLO                          PIC S9(4) COMP.
        05 WS-IND                             PIC S9(4) COMP.
        05 WS-IND-TABULATO                    PIC 9.
        05 LL-RIGA-MEM                        PIC 9(4).
      * 05 TABULATO                           PIC X.
        05 SI-USER                            PIC X(8).
        05 SI-DIRECTORY                       PIC X(80).
        05 FILE-OUT0                          PIC X(80).
        05  CHECK-FILE-0.
         10 STAT-KEY-2-10                     PIC X.
         10 STAT-KEY-2-20                     PIC X.
         10 STAT-KEY-2-20-BINARY REDEFINES STAT-KEY-2-20 PIC 99 COMP-X.
        05 CHECK-FILE-0N REDEFINES CHECK-FILE-0 PIC 99.
        05 FILE-OUT1                          PIC X(80).
        05  CHECK-FILE-1.
         10 STAT-KEY-2-11                     PIC X.
         10 STAT-KEY-2-21                     PIC X.
         10 STAT-KEY-2-21-BINARY REDEFINES STAT-KEY-2-21 PIC 99 COMP-X.
        05 CHECK-FILE-1N REDEFINES CHECK-FILE-1 PIC 99.
        05 FILE-OUT2                          PIC X(80).
        05  CHECK-FILE-2.
         10 STAT-KEY-2-12                     PIC X.
         10 STAT-KEY-2-22                     PIC X.
         10 STAT-KEY-2-22-BINARY REDEFINES STAT-KEY-2-22 PIC 99 COMP-X.
        05 CHECK-FILE-2N REDEFINES CHECK-FILE-2 PIC 99.
        05 FILE-OUT3                          PIC X(80).
        05  CHECK-FILE-3.
         10 STAT-KEY-2-13                     PIC X.
         10 STAT-KEY-2-23                     PIC X.
         10 STAT-KEY-2-23-BINARY REDEFINES STAT-KEY-2-23 PIC 99 COMP-X.
        05 CHECK-FILE-3N REDEFINES CHECK-FILE-3 PIC 99.
        05 FILE-OUT4                          PIC X(80).
        05  CHECK-FILE-4.
         10 STAT-KEY-2-14                     PIC X.
         10 STAT-KEY-2-24                     PIC X.
         10 STAT-KEY-2-24-BINARY REDEFINES STAT-KEY-2-24 PIC 99 COMP-X.
        05 CHECK-FILE-4N REDEFINES CHECK-FILE-4 PIC 99.
        05 FILE-OUT5                          PIC X(80).
        05  CHECK-FILE-5.
         10 STAT-KEY-2-15                     PIC X.
         10 STAT-KEY-2-25                     PIC X.
         10 STAT-KEY-2-25-BINARY REDEFINES STAT-KEY-2-25 PIC 99 COMP-X.
        05 CHECK-FILE-5N REDEFINES CHECK-FILE-5 PIC 99.
        05 FILE-OUT6                          PIC X(80).
        05  CHECK-FILE-6.
         10 STAT-KEY-2-16                     PIC X.
         10 STAT-KEY-2-26                     PIC X.
         10 STAT-KEY-2-26-BINARY REDEFINES STAT-KEY-2-26 PIC 99 COMP-X.
        05 CHECK-FILE-6N REDEFINES CHECK-FILE-6 PIC 99.
        05 FILE-OUT7                          PIC X(80).
        05  CHECK-FILE-7.
         10 STAT-KEY-2-17                     PIC X.
         10 STAT-KEY-2-27                     PIC X.
         10 STAT-KEY-2-27-BINARY REDEFINES STAT-KEY-2-27 PIC 99 COMP-X.
        05 CHECK-FILE-7N REDEFINES CHECK-FILE-7 PIC 99.
        05 FILE-OUT8                          PIC X(80).
        05  CHECK-FILE-8.
         10 STAT-KEY-2-18                     PIC X.
         10 STAT-KEY-2-28                     PIC X.
         10 STAT-KEY-2-28-BINARY REDEFINES STAT-KEY-2-28 PIC 99 COMP-X.
        05 CHECK-FILE-8N REDEFINES CHECK-FILE-8 PIC 99.
        05 FILE-OUT9                          PIC X(80).
        05  CHECK-FILE-9.
         10 STAT-KEY-2-19                     PIC X.
         10 STAT-KEY-2-29                     PIC X.
         10 STAT-KEY-2-29-BINARY REDEFINES STAT-KEY-2-29 PIC 99 COMP-X.
        05 CHECK-FILE-9N REDEFINES CHECK-FILE-9 PIC 99.
        05 FILE-OUT10                         PIC X(80).
        05  CHECK-FILE-10.
         10 STAT-KEY-2-110                    PIC X.
         10 STAT-KEY-2-210                    PIC X.
         10 STAT-KEY-2-210-BINARY
            REDEFINES STAT-KEY-2-210 PIC 99 COMP-X.
        05 CHECK-FILE-10N REDEFINES CHECK-FILE-10 PIC 99.
        05 FILE-OUT11                         PIC X(80).
        05  CHECK-FILE-11.
         10 STAT-KEY-2-111                    PIC X.
         10 STAT-KEY-2-211                    PIC X.
         10 STAT-KEY-2-211-BINARY
            REDEFINES STAT-KEY-2-211 PIC 99 COMP-X.
        05 CHECK-FILE-11N REDEFINES CHECK-FILE-11 PIC 99.
        05 FILE-OUT12                         PIC X(80).
        05  CHECK-FILE-12.
         10 STAT-KEY-2-112                    PIC X.
         10 STAT-KEY-2-212                    PIC X.
         10 STAT-KEY-2-212-BINARY
            REDEFINES STAT-KEY-2-212 PIC 99 COMP-X.
        05 CHECK-FILE-12N REDEFINES CHECK-FILE-12 PIC 99.
        05 FILE-OUT13                         PIC X(80).
        05  CHECK-FILE-13.
         10 STAT-KEY-2-113                    PIC X.
         10 STAT-KEY-2-213                    PIC X.
         10 STAT-KEY-2-213-BINARY
            REDEFINES STAT-KEY-2-213 PIC 99 COMP-X.
        05 CHECK-FILE-13N REDEFINES CHECK-FILE-13 PIC 99.
        05 FILE-OUT14                         PIC X(80).
        05  CHECK-FILE-14.
         10 STAT-KEY-2-114                    PIC X.
         10 STAT-KEY-2-214                    PIC X.
         10 STAT-KEY-2-214-BINARY
            REDEFINES STAT-KEY-2-214 PIC 99 COMP-X.
        05 CHECK-FILE-14N REDEFINES CHECK-FILE-14 PIC 99.
        05 FILE-OUT15                         PIC X(80).
        05  CHECK-FILE-15.
         10 STAT-KEY-2-115                    PIC X.
         10 STAT-KEY-2-215                    PIC X.
         10 STAT-KEY-2-215-BINARY
            REDEFINES STAT-KEY-2-215 PIC 99 COMP-X.
        05 CHECK-FILE-15N REDEFINES CHECK-FILE-15 PIC 99.
      *
      *
       01 N-MAX-FIL      PIC S9(4) COMP VALUE 15.
       01 IND-FIL        PIC S9(4) COMP.
      *
       01 TABELLA-FIL.
         05 FILLER                  OCCURS 15.
           10 NOMEFILE-FIL   PIC X(12) VALUE SPACES.
           10 TABULATO-FIL   PIC 9     VALUE ZEROS.
      *
      *
      *
       LINKAGE SECTION.
      *
       01 PAR-PRINT.
        05 STATO                 PIC S9(4) COMP.
        05 LL-RIGA               PIC S9(4) COMP.
          88 LL-RIGA-OK VALUE 34 , 84 , 136 , 162 , 228 , 324.
        05 N-MAX-RIGHE           PIC S9(4) COMP.
        05 FLAGS-ROUTINE         PIC S9(4) COMP.
        05 NUM-FILE-IDEN         PIC S9(4) COMP.
        05 NOME-FILE             PIC X(12).
      *
       01 RIGA.
         10 STAMPANTE            PIC X.
         10 NR-STAMPANTE REDEFINES STAMPANTE PIC 9.
         10 COMANDO              PIC X.
            88 COMANDO-OK        VALUE  "S", "P", "G".
         10 NR-RIGA              PIC S9(4) COMP.
         10 DATI-RIGA            PIC X(252).
      *
       01 BUFFER-ST.
        05 N-BUFF                PIC S9(4) COMP.
        05 LL-ATTUALE            PIC S9(4) COMP.
        05 RESTO-BUFF            PIC X(5120).
      *
       PROCEDURE DIVISION USING PAR-PRINT RIGA BUFFER-ST.
      *
       VIA.
           MOVE NR-STAMPANTE    TO WS-IND-TABULATO.
      *
      *comando di chiusura file di stampa (uno o piu' tabulati)
           IF COMANDO OF RIGA = "C"
              PERFORM CHIUDI-TUTTI-I-TABULATI
                 THRU EX-CHIUDI-TUTTI-I-TABULATI
              GOBACK
           END-IF.
      *
           PERFORM OTTIENI-WS-IND THRU EX-OTTIENI-WS-IND.
           IF WS-IND < 0
              DISPLAY "** QWLPPR: Errore tabella file!!"
                      " nome file: " NOME-FILE OF PAR-PRINT
                      ", tabulato: " STAMPANTE OF RIGA
              STOP RUN
           END-IF.
      *
           IF COMANDO = 'M'
              PERFORM R-APRI      THRU R-APRI-EX
      *       display "QWLPPR  -dopo apri"
              PERFORM R-SCRIVI-M    THRU R-SCRIVI-M-EX
           ELSE
              PERFORM R-SCRIVI    THRU R-SCRIVI-EX.
      *
      ******************************************************************
      *
       laudebug.
           display "COMANDO OF RIGA="COMANDO OF RIGA.
           PERFORM VARYING IND-FIL FROM 1 BY 1
           UNTIL IND-FIL > N-MAX-FIL
              DISPLAY IND-FIL ") "  NOMEFILE-FIL(IND-FIL) " - "
                      TABULATO-FIL(IND-FIL)
           END-PERFORM.
       ex-laudebug.
          EXIT.
      *
      *
      *chiudo tutti i tabulati relativi al file di stampa corrente
       CHIUDI-TUTTI-I-TABULATI.
           PERFORM VARYING IND-FIL FROM 1 BY 1
           UNTIL IND-FIL > N-MAX-FIL
              IF NOME-FILE OF PAR-PRINT = NOMEFILE-FIL(IND-FIL)
                 MOVE SPACES TO NOMEFILE-FIL(IND-FIL)
                 MOVE ZEROS  TO TABULATO-FIL(IND-FIL)
                 COMPUTE WS-IND = IND-FIL - 1
                 PERFORM CHIUDI-FILE THRU EX-CHIUDI-FILE
              END-IF
           END-PERFORM.
       EX-CHIUDI-TUTTI-I-TABULATI.
           EXIT.
      *
      *
       OTTIENI-WS-IND.
           MOVE -1 TO WS-IND.
      *
      *cerco se il file e' gia' in tabella (quindi aperto)
           PERFORM VARYING IND-FIL FROM 1 BY 1
           UNTIL IND-FIL > N-MAX-FIL OR WS-IND <> -1
              IF NOME-FILE OF PAR-PRINT = NOMEFILE-FIL(IND-FIL) AND
                 STAMPANTE OF RIGA      = TABULATO-FIL(IND-FIL)
                     COMPUTE WS-IND = IND-FIL - 1
              END-IF
           END-PERFORM.
      *
      *inserisco in tabella il nuovo file
           IF COMANDO OF RIGA = "M"
              IF WS-IND = -1
                 PERFORM VARYING IND-FIL FROM 1 BY 1
                 UNTIL IND-FIL > N-MAX-FIL OR WS-IND <> -1
                    IF NOMEFILE-FIL(IND-FIL) = SPACES AND
                    TABULATO-FIL(IND-FIL) = ZEROS
                         MOVE NOME-FILE OF PAR-PRINT
                           TO NOMEFILE-FIL(IND-FIL)
                         MOVE STAMPANTE OF RIGA
                           TO TABULATO-FIL(IND-FIL)
                         COMPUTE WS-IND = IND-FIL - 1
                    END-IF
                 END-PERFORM
              ELSE
                GOBACK
              END-IF
           END-IF.
       EX-OTTIENI-WS-IND.
           EXIT.
      *
      *
       CHIUDI-FILE.
           EVALUATE TRUE
             WHEN WS-IND = 0
                 CLOSE FILOUT0
                 IF CHECK-FILE-0 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 1
                 CLOSE FILOUT1
                 IF CHECK-FILE-1 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 2
                 CLOSE FILOUT2
                 IF CHECK-FILE-2 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 3
                 CLOSE FILOUT3
                 IF CHECK-FILE-3 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 4
                 CLOSE FILOUT4
                 IF CHECK-FILE-4 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 5
                 CLOSE FILOUT5
                 IF CHECK-FILE-5 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 6
                 CLOSE FILOUT6
                 IF CHECK-FILE-6 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 7
                 CLOSE FILOUT7
                 IF CHECK-FILE-7 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 8
                 CLOSE FILOUT8
                 IF CHECK-FILE-8 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 9
                 CLOSE FILOUT9
                 IF CHECK-FILE-9 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 10
                 CLOSE FILOUT10
                 IF CHECK-FILE-10 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 11
                 CLOSE FILOUT11
                 IF CHECK-FILE-11 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 12
                 CLOSE FILOUT12
                 IF CHECK-FILE-12 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 13
                 CLOSE FILOUT13
                 IF CHECK-FILE-13 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 14
                 CLOSE FILOUT14
                 IF CHECK-FILE-14 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
             WHEN WS-IND = 15
                 CLOSE FILOUT15
                 IF CHECK-FILE-15 <> '00'
                    MOVE -2 TO STATO OF PAR-PRINT
                 END-IF
           END-EVALUATE.
       EX-CHIUDI-FILE.
           EXIT.
      *
      *
       R-APRI.
           MOVE SPACE             TO WK-VAR-VALUE
           MOVE "RETIS_DIRECTORY" TO WK-VAR-NAME
           DISPLAY WK-VAR-NAME UPON ENVIRONMENT-NAME
           ACCEPT WK-VAR-VALUE FROM ENVIRONMENT-VALUE
           MOVE WK-VAR-VALUE      TO SI-DIRECTORY
           MOVE SPACE             TO WK-VAR-VALUE
           MOVE "RETIS_UTENTE   " TO WK-VAR-NAME
           DISPLAY WK-VAR-NAME UPON ENVIRONMENT-NAME
           ACCEPT WK-VAR-VALUE FROM ENVIRONMENT-VALUE
           MOVE WK-VAR-VALUE        TO SI-USER
           EVALUATE  TRUE
             WHEN WS-IND = 0
                MOVE SPACE  TO FILE-OUT0
                STRING SI-DIRECTORY DELIMITED BY " "
                       "stampe/" SI-USER DELIMITED BY " " "_"
                          NOME-FILE "_" WS-IND-TABULATO
                          DELIMITED BY SPACE INTO FILE-OUT0
                OPEN EXTEND  FILOUT0
                IF CHECK-FILE-0 <> '00'
                   IF CHECK-FILE-0 = '05'
                      MOVE '00' TO CHECK-FILE-0
                      CONTINUE
                   ELSE
                      MOVE -1 TO STATO
                   END-IF
                END-IF
      *guiduz          MOVE ZERO     TO RIGA-CR0
             WHEN WS-IND = 1
                MOVE SPACE  TO FILE-OUT1
                STRING SI-DIRECTORY DELIMITED BY " "
                      "stampe/" SI-USER DELIMITED BY " " "_"
                       NOME-FILE "_" WS-IND-TABULATO
                          DELIMITED BY SPACE INTO FILE-OUT1
                OPEN EXTEND  FILOUT1
                IF CHECK-FILE-1 <> '00'
                   IF CHECK-FILE-1 = '05'
                      MOVE '00' TO CHECK-FILE-1
                      CONTINUE
                   ELSE
                      MOVE -1 TO STATO
                   END-IF
                END-IF
      *guiduz          MOVE ZERO     TO RIGA-CR1
             WHEN WS-IND = 2
                 MOVE SPACE  TO FILE-OUT2
                 STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT2
                 OPEN EXTEND  FILOUT2
                 IF CHECK-FILE-2 <> '00'
                    IF CHECK-FILE-2 = '05'
                       MOVE '00' TO CHECK-FILE-2
                       CONTINUE
                    ELSE
                       MOVE -1 TO STATO
                    END-IF
                 END-IF
      *guiduz          MOVE ZERO     TO RIGA-CR2
              WHEN WS-IND = 3
                 MOVE SPACE  TO FILE-OUT3
                 STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT3
                 OPEN EXTEND  FILOUT3
                 IF CHECK-FILE-3 <> '00'
                    IF CHECK-FILE-3 = '05'
                       MOVE '00' TO CHECK-FILE-3
                       CONTINUE
                    ELSE
                       MOVE -1 TO STATO
                    END-IF
                 END-IF
      *guiduz          MOVE ZERO     TO RIGA-CR3
              WHEN WS-IND = 4
                 MOVE SPACE  TO FILE-OUT4
                 STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                         NOME-FILE "_" WS-IND-TABULATO
                         DELIMITED BY SPACE INTO FILE-OUT4
                  OPEN EXTEND  FILOUT4
                  IF CHECK-FILE-4 <> '00'
                     IF CHECK-FILE-4 = '05'
                        MOVE '00' TO CHECK-FILE-4
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
      *guiduz          MOVE ZERO     TO RIGA-CR4
               WHEN WS-IND = 5
                  MOVE SPACE  TO FILE-OUT5
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                         NOME-FILE "_" WS-IND-TABULATO
                         DELIMITED BY SPACE INTO FILE-OUT5
                  OPEN EXTEND  FILOUT5
                  IF CHECK-FILE-5 <> '00'
                     IF CHECK-FILE-5 = '05'
                        MOVE '00' TO CHECK-FILE-5
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
      *guiduz            MOVE ZERO     TO RIGA-CR5
               WHEN WS-IND = 6
                  MOVE SPACE  TO FILE-OUT6
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT6
                  OPEN EXTEND  FILOUT6
                  IF CHECK-FILE-6 <> '00'
                     IF CHECK-FILE-6 = '05'
                        MOVE '00' TO CHECK-FILE-6
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
      *guiduz            MOVE ZERO     TO RIGA-CR6
               WHEN WS-IND = 7
                  MOVE SPACE  TO FILE-OUT7
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT7
                  OPEN EXTEND  FILOUT7
                  IF CHECK-FILE-7 <> '00'
                     IF CHECK-FILE-7 = '05'
                        MOVE '00' TO CHECK-FILE-7
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
      *guiduz            MOVE ZERO     TO RIGA-CR7
               WHEN WS-IND = 8
                  MOVE SPACE  TO FILE-OUT8
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT8
                  OPEN EXTEND  FILOUT8
                  IF CHECK-FILE-8 <> '00'
                     IF CHECK-FILE-8 = '05'
                        MOVE '00' TO CHECK-FILE-8
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
      *guiduz            MOVE ZERO     TO RIGA-CR8
               WHEN WS-IND = 9
                  MOVE SPACE  TO FILE-OUT9
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT9
                  OPEN EXTEND  FILOUT9
                  IF CHECK-FILE-9 <> '00'
                     IF CHECK-FILE-9 = '05'
                        MOVE '00' TO CHECK-FILE-9
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
      *guiduz            MOVE ZERO     TO RIGA-CR9
               WHEN WS-IND = 10
                  MOVE SPACE  TO FILE-OUT10
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT10
                  OPEN EXTEND  FILOUT10
                  IF CHECK-FILE-10 <> '00'
                     IF CHECK-FILE-10 = '05'
                        MOVE '00' TO CHECK-FILE-10
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
               WHEN WS-IND = 11
                  MOVE SPACE  TO FILE-OUT11
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT11
                  OPEN EXTEND  FILOUT11
                  IF CHECK-FILE-11 <> '00'
                     IF CHECK-FILE-11 = '05'
                        MOVE '00' TO CHECK-FILE-11
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
               WHEN WS-IND = 12
                  MOVE SPACE  TO FILE-OUT12
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT12
                  OPEN EXTEND  FILOUT12
                  IF CHECK-FILE-12 <> '00'
                     IF CHECK-FILE-12 = '05'
                        MOVE '00' TO CHECK-FILE-12
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
               WHEN WS-IND = 13
                  MOVE SPACE  TO FILE-OUT13
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT13
                  OPEN EXTEND  FILOUT13
                  IF CHECK-FILE-13 <> '00'
                     IF CHECK-FILE-13 = '05'
                        MOVE '00' TO CHECK-FILE-13
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
               WHEN WS-IND = 14
                  MOVE SPACE  TO FILE-OUT14
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT14
                  OPEN EXTEND  FILOUT14
                  IF CHECK-FILE-14 <> '00'
                     IF CHECK-FILE-14 = '05'
                        MOVE '00' TO CHECK-FILE-14
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
               WHEN WS-IND = 15
                  MOVE SPACE  TO FILE-OUT15
                  STRING SI-DIRECTORY DELIMITED BY " "
                        "stampe/" SI-USER DELIMITED BY " " "_"
                        NOME-FILE "_" WS-IND-TABULATO
                        DELIMITED BY SPACE INTO FILE-OUT15
                  OPEN EXTEND  FILOUT15
                  IF CHECK-FILE-15 <> '00'
                     IF CHECK-FILE-15 = '05'
                        MOVE '00' TO CHECK-FILE-15
                        CONTINUE
                     ELSE
                        MOVE -1 TO STATO
                     END-IF
                  END-IF
            END-EVALUATE.
       R-APRI-EX.
            EXIT. 
      *
      ******************************************************
      *
       R-SCRIVI-M.
      *    display "QWPRINT " TABULATO
           MOVE NR-RIGA            TO N-MAX-RIGHE.
           EVALUATE TRUE
             WHEN WS-IND = 0
              MOVE '<<M>>'           TO REC-OUT0
              MOVE DATI-RIGA (1:80)  TO REC-OUT0 (6:)
             WHEN WS-IND = 1
              MOVE '<<M>>'           TO REC-OUT1
              MOVE DATI-RIGA (1:80)  TO REC-OUT1 (6:)
             WHEN WS-IND = 2
              MOVE '<<M>>'           TO REC-OUT2
              MOVE DATI-RIGA (1:80)  TO REC-OUT2 (6:)
             WHEN WS-IND = 3
              MOVE '<<M>>'           TO REC-OUT3
              MOVE DATI-RIGA (1:80)  TO REC-OUT3 (6:)
             WHEN WS-IND = 4
              MOVE '<<M>>'           TO REC-OUT4
              MOVE DATI-RIGA (1:80)  TO REC-OUT4 (6:)
             WHEN WS-IND = 5
              MOVE '<<M>>'           TO REC-OUT5
              MOVE DATI-RIGA (1:80)  TO REC-OUT5 (6:)
             WHEN WS-IND = 6
              MOVE '<<M>>'           TO REC-OUT6
              MOVE DATI-RIGA (1:80)  TO REC-OUT6 (6:)
             WHEN WS-IND = 7
              MOVE '<<M>>'           TO REC-OUT7
              MOVE DATI-RIGA (1:80)  TO REC-OUT7 (6:)
             WHEN WS-IND = 8
              MOVE '<<M>>'           TO REC-OUT8
              MOVE DATI-RIGA (1:80)  TO REC-OUT8 (6:)
             WHEN WS-IND = 9
              MOVE '<<M>>'           TO REC-OUT9
              MOVE DATI-RIGA (1:80)  TO REC-OUT9 (6:)
             WHEN WS-IND = 10
              MOVE '<<M>>'           TO REC-OUT10
              MOVE DATI-RIGA (1:80)  TO REC-OUT10 (6:)
             WHEN WS-IND = 11
              MOVE '<<M>>'           TO REC-OUT11
              MOVE DATI-RIGA (1:80)  TO REC-OUT11 (6:)
             WHEN WS-IND = 12
              MOVE '<<M>>'           TO REC-OUT12
              MOVE DATI-RIGA (1:80)  TO REC-OUT12 (6:)
             WHEN WS-IND = 13
              MOVE '<<M>>'           TO REC-OUT13
              MOVE DATI-RIGA (1:80)  TO REC-OUT13 (6:)
             WHEN WS-IND = 14
              MOVE '<<M>>'           TO REC-OUT14
              MOVE DATI-RIGA (1:80)  TO REC-OUT14 (6:)
             WHEN WS-IND = 15
              MOVE '<<M>>'           TO REC-OUT15
              MOVE DATI-RIGA (1:80)  TO REC-OUT15 (6:)
           END-EVALUATE.
           PERFORM WRITE-RIGA     THRU WRITE-RIGA-EX.
       R-SCRIVI-M-EX.
            GOBACK.
      *
       R-SCRIVI.
           IF NR-RIGA > N-MAX-RIGHE
              MOVE -7 TO STATO
              GOBACK.
           MOVE 0 TO STATO.
           IF COMANDO = "S"
              PERFORM POS-S-LP THRU EX-POS-S-LP
             ELSE
              IF COMANDO = "P"
                 PERFORM POS-P-LP THRU EX-POS-P-LP
                ELSE
                 GOBACK.
           PERFORM SCRIVI THRU EX-SCRIVI.
       R-SCRIVI-EX.
            GOBACK.
      *
       POS-S-LP.
           EVALUATE TRUE
             WHEN WS-IND = 0
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT0
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR0 = RIGA-CR0 + NR-RIGA + 1
              IF RIGA-CR0 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR0
              END-IF
             WHEN WS-IND = 1
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT1
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR1 = RIGA-CR1 + NR-RIGA + 1
              IF RIGA-CR1 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR1
              END-IF
             WHEN WS-IND = 2
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT2
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR2 = RIGA-CR2 + NR-RIGA + 1
              IF RIGA-CR2 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR2
              END-IF
             WHEN WS-IND = 3
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT3
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR3 = RIGA-CR3 + NR-RIGA + 1
              IF RIGA-CR3 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR3
              END-IF
             WHEN WS-IND = 4
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT4
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR4 = RIGA-CR4 + NR-RIGA + 1
              IF RIGA-CR4 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR4
              END-IF
             WHEN WS-IND = 5
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT5
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR5 = RIGA-CR5 + NR-RIGA + 1
              IF RIGA-CR5 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR5
              END-IF
             WHEN WS-IND = 6
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT6
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR6 = RIGA-CR6 + NR-RIGA + 1
              IF RIGA-CR6 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR6
              END-IF
             WHEN WS-IND = 7
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT7
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR7 = RIGA-CR7 + NR-RIGA + 1
              IF RIGA-CR7 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR7
              END-IF
             WHEN WS-IND = 8
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT8
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR8 = RIGA-CR8 + NR-RIGA + 1
              IF RIGA-CR8 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR8
              END-IF
             WHEN WS-IND = 9
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT9
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR9 = RIGA-CR9 + NR-RIGA + 1
              IF RIGA-CR9 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR9
              END-IF
             WHEN WS-IND = 10
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT10
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR10 = RIGA-CR10 + NR-RIGA + 1
              IF RIGA-CR10 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR10
              END-IF
             WHEN WS-IND = 11
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT11
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR11 = RIGA-CR11 + NR-RIGA + 1
              IF RIGA-CR11 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR11
              END-IF
             WHEN WS-IND = 12
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT12
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR12 = RIGA-CR12 + NR-RIGA + 1
              IF RIGA-CR12 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR12
              END-IF
             WHEN WS-IND = 13
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT13
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR13 = RIGA-CR13 + NR-RIGA + 1
              IF RIGA-CR13 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR13
              END-IF
             WHEN WS-IND = 14
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT14
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR14 = RIGA-CR14 + NR-RIGA + 1
              IF RIGA-CR14 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR14
              END-IF
             WHEN WS-IND = 15
              IF NR-RIGA NOT = ZERO
                 MOVE SPACES            TO REC-OUT15
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    NR-RIGA TIMES
              END-IF
              COMPUTE RIGA-CR15 = RIGA-CR15 + NR-RIGA + 1
              IF RIGA-CR15 > N-MAX-RIGHE
                 SUBTRACT N-MAX-RIGHE FROM RIGA-CR15
              END-IF
           END-EVALUATE.
       EX-POS-S-LP.
           EXIT.
      *
       POS-P-LP.
           EVALUATE TRUE
             WHEN WS-IND = 0
              IF NR-RIGA = RIGA-CR0
                 OR RIGA-CR0 = ZERO
                 GO TO EX-POS-P-LP
              END-IF
              IF NR-RIGA > RIGA-CR0 AND
                 RIGA-CR0 NOT = ZERO
      *alles*
      *          COMPUTE CONTROLLO = NR-RIGA - RIGA-CR0
                 COMPUTE CONTROLLO = NR-RIGA - RIGA-CR0 - 1
      *alles*
                 MOVE SPACES           TO REC-OUT0
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
                ELSE
      *alles*   
      *          COMPUTE CONTROLLO =  NR-RIGA - 1
                 COMPUTE CONTROLLO = NR-RIGA
      *alles*
                 MOVE '<<P>>'          TO REC-OUT0
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                 MOVE SPACES          TO REC-OUT0
                 PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
              END-IF
              MOVE NR-RIGA        TO RIGA-CR0
             WHEN WS-IND = 1
              IF NR-RIGA = RIGA-CR1
                 OR RIGA-CR1 = ZERO
                 GO TO EX-POS-P-LP
              END-IF
              IF NR-RIGA > RIGA-CR1 AND
                 RIGA-CR1 NOT = ZERO
      *alles*
      *          COMPUTE CONTROLLO = NR-RIGA - RIGA-CR1
                 COMPUTE CONTROLLO = NR-RIGA - RIGA-CR1 - 1
      *alles*                 
                 MOVE SPACES           TO REC-OUT1
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
                ELSE
      *alles*   
      *          COMPUTE CONTROLLO =  NR-RIGA - 1
                 COMPUTE CONTROLLO = NR-RIGA
      *alles*                
                 MOVE '<<P>>'          TO REC-OUT1
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                 MOVE SPACES           TO REC-OUT1
                 PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
              END-IF
              MOVE NR-RIGA             TO RIGA-CR1
             WHEN WS-IND = 2
              IF NR-RIGA = RIGA-CR2
                 OR RIGA-CR2 = ZERO
                 GO TO EX-POS-P-LP
              END-IF
              IF NR-RIGA > RIGA-CR2 AND
                 RIGA-CR2 NOT = ZERO
      *alles*
      *          COMPUTE CONTROLLO = NR-RIGA - RIGA-CR2
                 COMPUTE CONTROLLO = NR-RIGA - RIGA-CR2 - 1
      *alles*                 
                 MOVE SPACES           TO REC-OUT2
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
                ELSE
      *alles*   
      *          COMPUTE CONTROLLO =  NR-RIGA - 1
                 COMPUTE CONTROLLO = NR-RIGA
      *alles*        
                 MOVE '<<P>>'          TO REC-OUT2
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                 MOVE SPACES           TO REC-OUT2
                 PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
              END-IF
              MOVE NR-RIGA        TO RIGA-CR2
             WHEN WS-IND = 3
              IF NR-RIGA = RIGA-CR3
                 OR RIGA-CR3 = ZERO
                 GO TO EX-POS-P-LP
              END-IF
              IF NR-RIGA > RIGA-CR3 AND
                 RIGA-CR3 NOT = ZERO
      *alles*
      *          COMPUTE CONTROLLO = NR-RIGA - RIGA-CR3
                 COMPUTE CONTROLLO = NR-RIGA - RIGA-CR3 - 1
      *alles*                 
                 MOVE SPACES           TO REC-OUT3
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
                ELSE
      *alles*   
      *          COMPUTE CONTROLLO =  NR-RIGA - 1
                 COMPUTE CONTROLLO = NR-RIGA
      *alles*  
                 MOVE '<<P>>'          TO REC-OUT3
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                 MOVE SPACES           TO REC-OUT3
                 PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
              END-IF
              MOVE NR-RIGA        TO RIGA-CR3
             WHEN WS-IND = 4
              IF NR-RIGA = RIGA-CR4
                 OR RIGA-CR4 = ZERO
                 GO TO EX-POS-P-LP
              END-IF
              IF NR-RIGA > RIGA-CR4 AND
                 RIGA-CR4 NOT = ZERO
      *alles*
      *          COMPUTE CONTROLLO = NR-RIGA - RIGA-CR4
                 COMPUTE CONTROLLO = NR-RIGA - RIGA-CR4 - 1
      *alles*                 
                 MOVE SPACES           TO REC-OUT4
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
                ELSE
      *alles*   
      *          COMPUTE CONTROLLO =  NR-RIGA - 1
                 COMPUTE CONTROLLO = NR-RIGA
      *alles*       
                 MOVE '<<P>>'          TO REC-OUT4
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                 MOVE SPACES           TO REC-OUT4
                 PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
              END-IF
              MOVE NR-RIGA        TO RIGA-CR4
             WHEN WS-IND = 5
              IF NR-RIGA = RIGA-CR5
                 OR RIGA-CR5 = ZERO
                 GO TO EX-POS-P-LP
              END-IF
              IF NR-RIGA > RIGA-CR5 AND
                 RIGA-CR5 NOT = ZERO
      *alles*
      *          COMPUTE CONTROLLO = NR-RIGA - RIGA-CR5
                 COMPUTE CONTROLLO = NR-RIGA - RIGA-CR5 - 1
      *alles*
                 MOVE SPACES           TO REC-OUT5
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
                ELSE
      *alles*   
      *          COMPUTE CONTROLLO =  NR-RIGA - 1
                 COMPUTE CONTROLLO = NR-RIGA
      *alles*       
                 MOVE '<<P>>'          TO REC-OUT5
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                 MOVE SPACES           TO REC-OUT5
                 PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
              END-IF
              MOVE NR-RIGA         TO RIGA-CR5
             WHEN WS-IND = 6
              IF NR-RIGA = RIGA-CR6
                 OR RIGA-CR6 = ZERO
                 GO TO EX-POS-P-LP
              END-IF
              IF NR-RIGA > RIGA-CR6 AND
                 RIGA-CR6 NOT = ZERO
      *alles*
      *          COMPUTE CONTROLLO = NR-RIGA - RIGA-CR6
                 COMPUTE CONTROLLO = NR-RIGA - RIGA-CR6 - 1
      *alles*
                 MOVE SPACES           TO REC-OUT6
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
                ELSE
      *alles*   
      *          COMPUTE CONTROLLO =  NR-RIGA - 1
                 COMPUTE CONTROLLO = NR-RIGA
      *alles*       
                 MOVE '<<P>>'          TO REC-OUT6
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                 MOVE SPACES           TO REC-OUT6
                 PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
              END-IF
              MOVE NR-RIGA        TO RIGA-CR6
             WHEN WS-IND = 7
              IF NR-RIGA = RIGA-CR7
                 OR RIGA-CR7 = ZERO
                 GO TO EX-POS-P-LP
              END-IF
              IF NR-RIGA > RIGA-CR7 AND
                 RIGA-CR7 NOT = ZERO

      *alles*
      *          COMPUTE CONTROLLO = NR-RIGA - RIGA-CR7
                 COMPUTE CONTROLLO = NR-RIGA - RIGA-CR7 - 1
      *alles*              
                 MOVE SPACES           TO REC-OUT7
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
                ELSE
      *alles*   
      *          COMPUTE CONTROLLO =  NR-RIGA - 1
                 COMPUTE CONTROLLO = NR-RIGA
      *alles*       
                 MOVE '<<P>>'          TO REC-OUT7
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                 MOVE SPACES           TO REC-OUT7
                 PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
              END-IF
              MOVE NR-RIGA        TO RIGA-CR7
             WHEN WS-IND = 8
              IF NR-RIGA = RIGA-CR8
                 OR RIGA-CR8 = ZERO
                 GO TO EX-POS-P-LP
              END-IF
              IF NR-RIGA > RIGA-CR8 AND
                 RIGA-CR8 NOT = ZERO
      *alles*
      *          COMPUTE CONTROLLO = NR-RIGA - RIGA-CR8
                 COMPUTE CONTROLLO = NR-RIGA - RIGA-CR8 - 1
      *alles*                 
                 MOVE SPACES           TO REC-OUT8
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
                ELSE
      *alles*   
      *          COMPUTE CONTROLLO =  NR-RIGA - 1
                 COMPUTE CONTROLLO = NR-RIGA
      *alles*       
                 MOVE '<<P>>'          TO REC-OUT8
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                 MOVE SPACES           TO REC-OUT8
                 PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
              END-IF
              MOVE  NR-RIGA        TO RIGA-CR8
             WHEN WS-IND = 9
              IF NR-RIGA = RIGA-CR9
                 OR RIGA-CR9 = ZERO
                 GO TO EX-POS-P-LP
              END-IF
              IF NR-RIGA > RIGA-CR9 AND
                 RIGA-CR9 NOT = ZERO
      *alles*
      *          COMPUTE CONTROLLO = NR-RIGA - RIGA-CR9
                 COMPUTE CONTROLLO = NR-RIGA - RIGA-CR9 - 1
      *alles*
                 MOVE SPACES           TO REC-OUT9
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
                ELSE
      *alles*   
      *          COMPUTE CONTROLLO =  NR-RIGA - 1
                 COMPUTE CONTROLLO = NR-RIGA
      *alles*       
                 MOVE '<<P>>'          TO REC-OUT9
                 PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                 MOVE SPACES           TO REC-OUT9
                 PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                    CONTROLLO TIMES
              END-IF
              MOVE NR-RIGA        TO RIGA-CR9
      *
             WHEN WS-IND = 10
                IF NR-RIGA = RIGA-CR10 OR RIGA-CR10 = ZERO
                   GO TO EX-POS-P-LP
                END-IF
                IF NR-RIGA > RIGA-CR10 AND RIGA-CR10 NOT = ZERO
                   COMPUTE CONTROLLO = NR-RIGA - RIGA-CR10 - 1
                   MOVE SPACES           TO REC-OUT10
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                ELSE
                   COMPUTE CONTROLLO = NR-RIGA
                   MOVE '<<P>>'          TO REC-OUT10
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                   MOVE SPACES           TO REC-OUT10
                   PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                END-IF
                MOVE NR-RIGA        TO RIGA-CR10
             WHEN WS-IND = 11
                IF NR-RIGA = RIGA-CR11 OR RIGA-CR11 = ZERO
                   GO TO EX-POS-P-LP
                END-IF
                IF NR-RIGA > RIGA-CR11 AND RIGA-CR11 NOT = ZERO
                   COMPUTE CONTROLLO = NR-RIGA - RIGA-CR11 - 1
                   MOVE SPACES           TO REC-OUT11
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                ELSE
                   COMPUTE CONTROLLO = NR-RIGA
                   MOVE '<<P>>'          TO REC-OUT11
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                   MOVE SPACES           TO REC-OUT11
                   PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                END-IF
                MOVE NR-RIGA        TO RIGA-CR11
             WHEN WS-IND = 12
                IF NR-RIGA = RIGA-CR12 OR RIGA-CR12 = ZERO
                   GO TO EX-POS-P-LP
                END-IF
                IF NR-RIGA > RIGA-CR12 AND RIGA-CR12 NOT = ZERO
                   COMPUTE CONTROLLO = NR-RIGA - RIGA-CR12 - 1
                   MOVE SPACES           TO REC-OUT12
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                ELSE
                   COMPUTE CONTROLLO = NR-RIGA
                   MOVE '<<P>>'          TO REC-OUT12
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                   MOVE SPACES           TO REC-OUT12
                   PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                END-IF
                MOVE NR-RIGA        TO RIGA-CR12
             WHEN WS-IND = 13
                IF NR-RIGA = RIGA-CR13 OR RIGA-CR13 = ZERO
                   GO TO EX-POS-P-LP
                END-IF
                IF NR-RIGA > RIGA-CR13 AND RIGA-CR13 NOT = ZERO
                   COMPUTE CONTROLLO = NR-RIGA - RIGA-CR13 - 1
                   MOVE SPACES           TO REC-OUT13
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                ELSE
                   COMPUTE CONTROLLO = NR-RIGA
                   MOVE '<<P>>'          TO REC-OUT13
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                   MOVE SPACES           TO REC-OUT13
                   PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                END-IF
                MOVE NR-RIGA        TO RIGA-CR13
             WHEN WS-IND = 14
                IF NR-RIGA = RIGA-CR14 OR RIGA-CR14 = ZERO
                   GO TO EX-POS-P-LP
                END-IF
                IF NR-RIGA > RIGA-CR14 AND RIGA-CR14 NOT = ZERO
                   COMPUTE CONTROLLO = NR-RIGA - RIGA-CR14 - 1
                   MOVE SPACES           TO REC-OUT14
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                ELSE
                   COMPUTE CONTROLLO = NR-RIGA
                   MOVE '<<P>>'          TO REC-OUT14
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                   MOVE SPACES           TO REC-OUT14
                   PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                END-IF
                MOVE NR-RIGA        TO RIGA-CR14
             WHEN WS-IND = 15
                IF NR-RIGA = RIGA-CR15 OR RIGA-CR15 = ZERO
                   GO TO EX-POS-P-LP
                END-IF
                IF NR-RIGA > RIGA-CR15 AND RIGA-CR15 NOT = ZERO
                   COMPUTE CONTROLLO = NR-RIGA - RIGA-CR15 - 1
                   MOVE SPACES           TO REC-OUT15
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                ELSE
                   COMPUTE CONTROLLO = NR-RIGA
                   MOVE '<<P>>'          TO REC-OUT15
                   PERFORM WRITE-RIGA    THRU WRITE-RIGA-EX
                   MOVE SPACES           TO REC-OUT15
                   PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
                      CONTROLLO TIMES
                END-IF
                MOVE NR-RIGA        TO RIGA-CR15
           END-EVALUATE.
       EX-POS-P-LP.
           EXIT.
      *
       SCRIVI.
           COMPUTE LL-RIGA-MEM  = LL-RIGA - 4
           EVALUATE TRUE
             WHEN WS-IND = 0
              MOVE SPACES                TO REC-OUT0
              MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT0
              PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 1
              MOVE SPACES                TO REC-OUT1
              MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT1
              PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 2
              MOVE SPACES                TO REC-OUT2
              MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT2
              PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 3
              MOVE SPACES                TO REC-OUT3
              MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT3
              PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 4
              MOVE SPACES                TO REC-OUT4
              MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT4
              PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 5
              MOVE SPACES                TO REC-OUT5
              MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT5
              PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 6
              MOVE SPACES                TO REC-OUT6
              MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT6
              PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 7
              MOVE SPACES                TO REC-OUT7
              MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT7
              PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 8
              MOVE SPACES                TO REC-OUT8
              MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT8
              PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 9
              MOVE SPACES                TO REC-OUT9
              MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT9
              PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 10
                MOVE SPACES                TO REC-OUT10
                MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT10
                PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 11
                MOVE SPACES                TO REC-OUT11
                MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT11
                PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 13
                MOVE SPACES                TO REC-OUT13
                MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT13
                PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 14
                MOVE SPACES                TO REC-OUT14
                MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT14
                PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
             WHEN WS-IND = 15
                MOVE SPACES                TO REC-OUT15
                MOVE DATI-RIGA (1:LL-RIGA-MEM) TO REC-OUT15
                PERFORM WRITE-RIGA   THRU WRITE-RIGA-EX
           END-EVALUATE.
       EX-SCRIVI.
           EXIT.
      *
       WRITE-RIGA.
           EVALUATE TRUE
             WHEN WS-IND = 0
              WRITE REC-OUT0
              IF CHECK-FILE-0 <> '00'
                 MOVE -2 TO STATO
              END-IF
             WHEN WS-IND = 1
              WRITE REC-OUT1
              IF CHECK-FILE-1 <> '00'
                 MOVE -2 TO STATO
              END-IF
             WHEN WS-IND = 2
              WRITE REC-OUT2
              IF CHECK-FILE-2 <> '00'
                 MOVE -2 TO STATO
              END-IF
             WHEN WS-IND = 3
              WRITE REC-OUT3
              IF CHECK-FILE-3 <> '00'
                 MOVE -2 TO STATO
              END-IF
             WHEN WS-IND = 4
              WRITE REC-OUT4
              IF CHECK-FILE-4 <> '00'
                 MOVE -2 TO STATO
              END-IF
             WHEN WS-IND = 5
              WRITE REC-OUT5
              IF CHECK-FILE-5 <> '00'
                 MOVE -2 TO STATO
              END-IF
             WHEN WS-IND = 6
              WRITE REC-OUT6
              IF CHECK-FILE-6 <> '00'
                 MOVE -2 TO STATO
              END-IF
             WHEN WS-IND = 7
              WRITE REC-OUT7
              IF CHECK-FILE-7 <> '00'
                 MOVE -2 TO STATO
              END-IF
             WHEN WS-IND = 8
              WRITE REC-OUT8
              IF CHECK-FILE-8 <> '00'
                 MOVE -2 TO STATO
              END-IF
             WHEN WS-IND = 9
              WRITE REC-OUT9
              IF CHECK-FILE-9 <> '00'
                 MOVE -2 TO STATO
              END-IF
             WHEN WS-IND = 10
                WRITE REC-OUT10
                IF CHECK-FILE-10 <> '00'
                   MOVE -2 TO STATO
                END-IF
             WHEN WS-IND = 11
                WRITE REC-OUT11
                IF CHECK-FILE-11 <> '00'
                   MOVE -2 TO STATO
                END-IF
             WHEN WS-IND = 12
                WRITE REC-OUT12
                IF CHECK-FILE-12 <> '00'
                   MOVE -2 TO STATO
                END-IF
             WHEN WS-IND = 13
                WRITE REC-OUT13
                IF CHECK-FILE-13 <> '00'
                   MOVE -2 TO STATO
                END-IF
             WHEN WS-IND = 14
                WRITE REC-OUT14
                IF CHECK-FILE-14 <> '00'
                   MOVE -2 TO STATO
                END-IF
             WHEN WS-IND = 15
                WRITE REC-OUT15
                IF CHECK-FILE-15 <> '00'
                   MOVE -2 TO STATO
                END-IF
           END-EVALUATE.
       WRITE-RIGA-EX.
           EXIT.
      *
