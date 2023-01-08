       IDENTIFICATION DIVISION.
       PROGRAM-ID. QOLPPR.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       WORKING-STORAGE SECTION.
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
      *
       PROCEDURE DIVISION USING PAR-PRINT RIGA BUFFER-ST.
      *
       VIA.
           MOVE 0 TO STATO OF PAR-PRINT.
       FINE.
           GOBACK.
      *
      *
