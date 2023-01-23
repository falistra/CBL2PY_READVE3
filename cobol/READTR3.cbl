001000*CONTROL DYNAMIC,BOUNDS       
001100 IDENTIFICATION DIVISION.                                                 
001200 PROGRAM-ID. READTR3.                                                     
001300*                                                                         
001400*   svuotamento mag 7 in piattaforma 3                                 
001500*                                                                         
      *        
      * W-INDICE-4 = XY
      *      X = Magazzino
      *      Y => 0 = trasferimento, 1 = rientro
      *
      * se W-INDICE-4 = 30 trasferimento al mag3
      * se W-INDICE-4 = 31 rientro dal mag3
      * se W-INDICE-4 = 40 trasferimento al mag4
      * se W-INDICE-4 = 41 rientro dal mag4
      *
      *MAG3+4*      04/12/08
      *      mag. 3 e 4 trattati con causali TRAS/TRA1
      * 
003500*TRASETTE*       21/10/14 
      *      trasferim a mag 7 (in blocco)
      *      con W-INDICE-4 = 70, con esclusione mag 2
      *
      *mag8*      27/11/14 
      *      creare giacenza impegnata per il mag. 8
      * 
      *ESTETA*     04/02/2019
      *      estensione taglie
      *
      *************************************
      *MAGCDEP* Valeria Maggio 2020
      *    poter gestire piu' magazzini di conto deposito
      *    reperire lista da parametro db
      *************************************
      *   
      *************************************
      *NO-DATGE     gennaio 2021 VALERIA 
      *     dismissione DATGE >> sostituita PRINTDD6 con PRINTDDF
      *
      ***************************************
      *MAGSEDE          MAGGIO 2022 VALERIA
      *     INSERITA FUNCTION-ID. magsede
      * per riconoscere i magazzini sede 
      *     dai parametrisu DB
      *    
002610 ENVIRONMENT DIVISION.                                                    
002620 CONFIGURATION SECTION.                                                   
002630 SOURCE-COMPUTER.  HP-3000.                                               
002640 OBJECT-COMPUTER.  HP-3000.                                               
002650 SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.       
      *MAGSEDE                                                          inizio
       REPOSITORY.
            FUNCTION magsede.
      *MAGSEDE                                                          FINE
      *
005500 INPUT-OUTPUT SECTION.                                                    
005600 FILE-CONTROL.                                                            
005700     SELECT PETSEL  ASSIGN TO SORTWORK. 
002700*                                                                         
002800 DATA DIVISION.   
006500 FILE SECTION.                                                            
006600 SD PETSEL  DATA RECORD REC-SEL.                                          
006700 01 REC-SEL.   
         05 CLASSE            PIC 99.
         05 SOCIETA-MOD       PIC 9.
         05 C-MAT             PIC S9(15) COMP-3.
         05 QTA-GIAC.                                              
             20  QTA-GIAC-PF        PIC S9(8) COMP COPY NTGOCCURS.        
002900*                                                                         
003000*                                                                         
003100 WORKING-STORAGE SECTION.                                                 
003200*                                                                         
003300 77 ERR-DISP               PIC -(6).                                      
003400 77 DISP-4  PIC ZZZ9-.                                                    
003500 77 DISP-8  PIC ZZZZZZZZ-.                                                
003600 77 ERR-D  PIC ZZZZ-.                                                     
003700 77 IR   PIC S9(4) COMP.   
       77 IT   PIC S9(4) COMP.  
       77 D3      PIC B9B.
       77 D2      PIC 99.
      *
      *01 X1               PIC XX VALUE "SI".                            attiva display
       01 X1               PIC XX VALUE "NO".                            DISattiva display
      *
012700*
       COPY NTG.
      *                                                                         
      *MAGSEDE                                                          inizio
        01 MAG-999 PIC 999.
        01 TIPO-MAG-INPUT PIC X(50).
        01 SINO-MAG-SEDE PIC 9.
            88 NO-MAG-SEDE VALUE 0.
            88 SI-MAG-SEDE VALUE 1.
        01 STRINGA-MAG.
            05 E OCCURS 16.
                10 M PIC Z9.
                10 V PIC X.
        01 T PIC 999.
        01 I PIC 99.
        01 MESSAGGIO-MAG PIC X(80).
      *MAGSEDE                                                          fine
      *
       01 FILLER.
         05 Z9   PIC Z(9) COPY NTGOCCURS.
       01 Z10    PIC Z(12).
003900*

       01 PARDEED   COPY  QPARDEED.
       01 DEED-X3       PIC X(3).
       01 DEED-93       PIC 9(3).

004000*******************************************   
      *
018100 01 FLAG-FILE PIC X.                                                      
018200  88 FINE-LETTURA VALUE "1". 
      *
013500 01 CAMPI-UTILI.                                                          
013600  05 USCITA-PROGRAMMA      PIC S9(4) COMP.
016900  05 OK-INP          PIC S9(4) COMP.                                      
017000     88 INP-OK VALUE 1. 
        05 NUMERO-RIGA-COM   PIC S9(4) COMP.
        05 MAGAZZINO-P       PIC S9(4) COMP VALUE 7.
        05 MAGAZZINO-D       PIC S9(4) COMP VALUE 3.
        05 CAUSALE-P         PIC X(4) VALUE "TRAS".
        05 CAUSALE-D         PIC X(4) VALUE "TRA1".
      *
004100*                                                                         
004200 01 CAMPI-ANAGRAFICI.                                                     
004300  05  INDIRIZZO-STD         PIC X(66).                                    
004400  05  INDIRIZZO-COM         PIC X(60) VALUE SPACES.                       
004500  05  LOCALITA-COM          PIC X(60) VALUE SPACES.                       
004600  05  CAP-COM               PIC S9(5) COMP-3 VALUE 0.                     
004700  05  PROV-COM              PIC XX VALUE SPACES.                          
004800  05  STATO-COM             PIC XXX VALUE SPACES.                         
004900*                                                                         
005000  05  INDIRIZZO-C-COM         PIC X(60) VALUE SPACES.                     
005100  05  LOCALITA-C-COM          PIC X(60) VALUE SPACES.                     
005200  05  CAP-C-COM               PIC S9(5) COMP-3 VALUE 0.                   
005300  05  PROV-C-COM              PIC XX VALUE SPACES.                        
005400*                                                                         
005500  05 D-CONTO-MEM     PIC X(24).                                           
005600  05 D-CONTO-P-MEM     PIC X(24).                                         
005700  05 D-CONTO-AGG-MEM PIC X(24).                                           
005800  05 D-CONTO-VET     PIC X(24)  VALUE SPACES.                             
005900*                                                                         
006000  05  INDIRIZZO-C-VET         PIC X(60) VALUE SPACES.                     
006100  05  LOCALITA-C-VET          PIC X(60) VALUE SPACES.                     
006200  05  CAP-C-VET               PIC S9(5) COMP-3 VALUE 0.                   
006300  05  PROV-C-VET              PIC XX VALUE SPACES.                        
006400*   
028700 01 LOCALITA-PART-STR      PIC X(52) VALUE                                
028800   "Magazzino Via Santi 8, Cavriago (R.E.) ".
      *
004100 01  PAR-INDIRIZZO.                                                       
004200  05  STATO-IND            PIC S9(4) COMP.                                
004300  05  FUNZIONE-IND         PIC S9(4) COMP.                                
004400  05  LL-STRINGA-IND       PIC S9(4) COMP.                                
004500  05  LL-SUBSTRINGA-IND    PIC S9(4) COMP.                                
004600  05  FILLER               PIC X(6).                                      
004700  05  TIPO-SEP             PIC XX VALUE " ;".
      *
      *
       01 CAMPI-COMODO.
006600  05 RIF-BOLLA-DDT         PIC 9(12).                                     
006700  05 FILLER REDEFINES RIF-BOLLA-DDT.                                      
006800   10 AA-MM-GG-DDT       PIC 9(6).                                        
006900   10 NUMERO-DDT         PIC 9(6).
007000  05 CLIENTE-DDT           PIC S9(9) COMP.                                
007100  05 MAGAZZINO-DDT         PIC S9(4) COMP.                                
007200  05 CAUSALE-DDT           PIC X(4).                                      
007300  05 TIPO-DOC-DDT          PIC S9(4) COMP.                                
007400     88 DOC-DDT      VALUE 1.                                             
007500     88 DOC-NOT-DDT  VALUE 2.                                             
007600  05 TIPO-MOVIMENTO-DDT    PIC S9(4) COMP.                                
007700     88 VENDITA       VALUE 1.                                            
007800     88 TRASFERIMENTO VALUE 2.                                            
007900     88 C-VISIONE     VALUE 3.                                            
008000     88 C-LAVAGGIO    VALUE 4.                                            
008100     88 C-LAVORAZIONE VALUE 5.                                            
008200  05 TIPO-STAMPA-DDT       PIC S9(4) COMP.                                
008300     88 PRODOTTI-FINITI   VALUE 1.                                        
008400     88 MATERIE-PRIME     VALUE 9.                                        
008500  05 LOC-PART-DDT          PIC X(56).                                     
008600  05 NOTE-DDT              PIC X(44) OCCURS 2.                            
008700  05 TIPO-DATA-SET-DDT     PIC X.                                         
008800     88 MOVMAG-DDT    VALUE "0" , " ".                                    
008900     88 MOVTRANS-DDT  VALUE "1".                                          
009000*                                                                         
009100 01 IMPORTO-X-PL           PIC S9(11) COMP-3.                             
009200*                                                                         
009300 01 FILE-FAT-DDT PIC X.                                                   
009400  88 SI-FILE-FAT VALUE "S".                                               
009500*                                                                           
009520 01 RIGA-1-DDT   PIC X(65).                                               
009530 01 RIGA-2-DDT   PIC X(65).                                               
      *
      *
       01 MAG-IN.
         05 MAG-IN-R       PIC 999.
       01 AS-IN.
          02 A-IN-X.
             05 A-IN       PIC 9.
          02 S-IN-X.
             05 S-IN       PIC 9.
       01 FORN-IN.
         05 FORN-IN-R      PIC 9.
       01 CLASSI-IN.
      *   05 FILLER OCCURS 10.   
         05 FILLER OCCURS 99.
           10 EL-CLASSE-IN  PIC 99.
           10 FILLER        PIC X.
       01 CLASSE-IN.
         05 CLASSE-IN-R PIC 99.
       01 I-IN       PIC S9(4) COMP.
       01 Z-IN       PIC ZZ.
       01 AGGIORNA-IN     PIC XX.
        88 SI-AGGIORNAMENTO VALUE "SI".
       01 CONFERMA-IN     PIC XX.
       01 DISIMPEGNA PIC XX.
      *
      *******************************************
004100*      
       01 REC-SITPF COPY YSITPF.
       01 REC-MOVMAG COPY YMOVMAG.
      *
013000 01 REC-PARAMETRI         COPY YPARAMDT.                                  
013100 01 REC-PARAM-RID REDEFINES REC-PARAMETRI.                                
013200  03 FILLER               PIC X(4).                                       
013300  03 PARAM-MAG         COPY WPARAM03.  
      *
020600 01 PARQDATA  COPY QPARDATS.                                              
020700 01 PARGEN    COPY QPARGEN.                                               
020800 01 PARAGGPF  COPY PARAGGPF. 
      *
       01 AREA-REC-SET     PIC X(512).
       01 FILLER REDEFINES AREA-REC-SET.
         05 REC-ANAMAT  COPY YANAMAT.
       01 FILLER REDEFINES AREA-REC-SET.
         05 REC-ANACON  COPY YANACON.  
        01 FILLER REDEFINES AREA-REC-SET.
         05 REC-INDIRIZZI COPY YINDIRIZ.    
      *
030200 01 TAB-LOCK.                                                                   
030300   05 FILLER PIC S9(4) COMP VALUE 2.                                      
030700   05 FILLER PIC S9(4) COMP VALUE 17.                                     
030800   05 FILLER PIC X(16) VALUE "SITPF;".                                    
030900   05 FILLER PIC X(16) VALUE "@".                                         
031000   05 FILLER PIC S9(4) COMP VALUE 17.                                     
031100   05 FILLER PIC X(16) VALUE "MOVMAG;".                                   
031200   05 FILLER PIC X(16) VALUE "@".               
      *
      *
       01 C-MAT-COM   COPY DANCODMT.
      *
022100 01 CODICE-CONTO PIC 9(8).                                                
022200 01 CODICE-CONTO-R REDEFINES CODICE-CONTO.                                
022300   05 CAPO-CONTO PIC 9(3).                                                
022400   05 SOTTO-CONTO PIC 9(5).                  
      *
      *
       01 CLASSE-MEM          PIC S9(4) COMP.
       01 QTA-CLASSE. 
         05 QTA-GIAC-CLASSE         PIC S9(8) COMP COPY NTGOCCURS.
       01 SOCIETA-MOD-MEM     PIC S9(4) COMP.
       01 QTA-SOCIETA. 
         05 QTA-GIAC-SOCIETA        PIC S9(8) COMP COPY NTGOCCURS.  
       01 QTA-TOTALE. 
         05 QTA-GIAC-TOTALE         PIC S9(8) COMP COPY NTGOCCURS.       
       01 TOT-GIAC            PIC S9(15) COMP-3.
      *
      *
          01 T1.
         05 FILLER        PIC X(8) VALUE
           "+--+---+".
         05 FILLER        PIC X(20) VALUE
           "---------+---------+".
         05 FILLER        PIC X(20) VALUE
           "---------+---------+".
         05 FILLER        PIC X(20) VALUE
           "---------+---------+".
         05 FILLER        PIC X(20) VALUE
           "---------+---------+".   
         05 FILLER        PIC X(20) VALUE
           "---------+---------+".   
         05 FILLER        PIC X(13) VALUE
           "------------+".
       01 T2.
         05 FILLER        PIC X(8) VALUE
           "|cl|for|".
         05 FILLER        PIC X(20) VALUE
           "--- t1 --|--- t2 --|".
         05 FILLER        PIC X(20) VALUE
           "--- t3 --|--- t4 --|".
         05 FILLER        PIC X(20) VALUE
           "--- t5 --|--- t6 --|".
         05 FILLER        PIC X(20) VALUE
           "--- t7 --|--- t8 --|". 
         05 FILLER        PIC X(20) VALUE
           "--- t9 --|--- t10--|". 
         05 FILLER        PIC X(13) VALUE
           "      tot   |".
       01 T3.
         05 FILLER        PIC X(8) VALUE
           "|  |   |".
         05 FILLER        PIC X(20) VALUE
           "---------|---------|".
         05 FILLER        PIC X(20) VALUE
           "---------|---------|".
         05 FILLER        PIC X(20) VALUE
           "---------|---------|".
         05 FILLER        PIC X(20) VALUE
           "---------|---------|".
         05 FILLER        PIC X(20) VALUE
           "---------|---------|".
         05 FILLER        PIC X(13) VALUE
           "------------|".           
      *  
      *
       01 MAG-CNTR    PIC 999.
       01 NUMERO-CNTR PIC 9(6).
       01 AAMMGG-CNTR PIC 9(6).
       01 RIF-CNTR  PIC 9(12).
       01 FILLER REDEFINES RIF-CNTR.
           10 AAMMGG-CNTR-R  PIC 9(6).
           10 NUMERO-CNTR-R  PIC 9(6).
      *
      *                               
      *
      *************************************
      *MAGCDEP*                                                         inizio
       01 PY-INPUT-REC-CDEP.
          05 ID-SEZIONE-CDEP   PIC X(50).
          05 CHIAVE-CDEP       PIC X(50).
          05 LEN-ELEMENT-CDEP  PIC 99.
          05 LEN-ELEMENT-CDEP-X REDEFINES LEN-ELEMENT-CDEP PIC XX.
      *
       01 PY-OUTPUT-REC-CDEP.
          05 OUTPUT-RET-CDEP     PIC XX.
          05 NUM-VALUES-CDEP     PIC 99.
          05 STREAM-VALUES-CDEP PIC X(120).
          05 STREAM-VALUES-CDEP-RID REDEFINES STREAM-VALUES-CDEP.
            10 MAG-CDEP PIC 999 OCCURS 40.  
      *
       01 I-CDEP        PIC S9(4) COMP.
       01 SW-CDEP       PIC 9.
       01 SW-MAG-CDEP   PIC 9.
       01 APPO          PIC 999.

       01 MAG-DEST-CDEP       PIC 9.

      *MAGCDEP*                                                         fine
 
      *MAGCDEP*                                                         inizio
        01 PY-INPUT-REC-EXCEL-PREBOLLA.
            05 DATA-EXCEL-PREBOLLA  PIC 9(6).
            05 NUM-EXCEL-PREBOLLA   PIC 9(6).
            05 SEP1                 PIC X.
            05 C-CONTO-EXCEL-PREBOLLA PIC 9(5).
            05 S-CONTO-EXCEL-PREBOLLA PIC 9(3).
            05 SEP2                 PIC X.
            05 MAG-EXCEL-PREBOLLA   PIC 999.
        
        01 PY-OUTPUT-REC-EXCEL-PREBOLLA.
          05 OUTPUT-RET-EXCEL-PREBOLLA     PIC XX.
        
       01 PY-INPUT-REC-DISIMPEGNA.
          05 LISTA-AS               OCCURS 20.
            10 AS-DISIMPEGNA.
              15 ANNO-DISIMPEGNA        PIC X.
              15 STAG-DISIMPEGNA        PIC X.
          05 MAG-DISIMPEGNA         PIC XXX.
          05 FORN-DISIMPEGNA        PIC X.
          05 LISTA-CLASSE-DISIMPEGNA.                                                           
            10 CLASSE-DISIMPEGNA       PIC XX OCCURS 99.

       01 PY-OUTPUT-DISIMPEGNO      PIC XX.
          88 PY-OUTPUT-DISIMPEGNO-OK VALUE "OK".
      *MAGCDEP*                                                         inizio

 
      *
003500*                                                  
031900*                                                                         
032000 LINKAGE SECTION.                                                         
032100*                                                                         
032200 01 W-COMMON COPY WCOMMONW.  
032300*                                                                         
032400 EXEC SQL INCLUDE SQLCA END-EXEC.                                         
032700*                                                                         

       01 FLAG-PAR                    PIC X(4).
       01 FILLER REDEFINES FLAG-PAR.
        05 MAG-PAR                    PIC X(3).
        05 AZIONE-PAR                 PIC X.
         88 USCITA-PAR                VALUE "0".
         88 RIENTRO-PAR               VALUE "1".                                          
032800*PAGE                                                                     
032900*                                                                         
033000 PROCEDURE DIVISION USING W-COMMON SQLCA FLAG-PAR.                           
033100 INIZIO.                                                                  
033200*          
      *     MOVE W-INDICE-4 TO FLAG-PAR.
033300     MOVE 0 TO USCITA-PROGRAMMA .                                         
033500     PERFORM TRATTA THRU EX-TRATTA                               
033600               UNTIL USCITA-PROGRAMMA = 1.
                                
033700 FINE.                                                                    
033800     EXIT PROGRAM.                                                        
033900*                                                                         
034000*                                                                         
034100*PAGE                                                                     
034200*                                                                         
034300*                                                                         
034400*PAGE                                                                     
034500*                                                                         
034600*                                                                         
034700 TRATTA.   
      *MAGCDEP*                                                         fine
           PERFORM CHIAMA-PARAMETRI-CDEP THRU EX-CHIAMA-PARAMETRI-CDEP.                                                               
      *MAGCDEP*                                                         fine
034900     MOVE 0 TO OK-INP.                                            
035000     PERFORM VERIF-INP THRU EX-VERIF-INP                                  
035100           UNTIL INP-OK.                                                  
035200     IF USCITA-PROGRAMMA = 1                                              
035300        GO TO EX-TRATTA.                                                                                               
035800*  
           IF AGGIORNA-IN = "SI"
              IF DISIMPEGNA = "SI"                
                MOVE A-IN-X TO ANNO-DISIMPEGNA(1)
                MOVE S-IN-X TO STAG-DISIMPEGNA(1)
                MOVE MAG-IN TO MAG-DISIMPEGNA
                MOVE FORN-IN TO FORN-DISIMPEGNA
                MOVE SPACES TO LISTA-CLASSE-DISIMPEGNA
                PERFORM VARYING IR FROM 1 BY 1 UNTIL IR > 99
                  MOVE EL-CLASSE-IN(IR) TO CLASSE-DISIMPEGNA(IR)
                END-PERFORM 
                PERFORM CALL-DISIMPEGNA-MAG THRU EX-CALL-DISIMPEGNA-MAG
                IF NOT PY-OUTPUT-DISIMPEGNO-OK
                  DISPLAY "ERRORE DISIMPEGNO!!!"
                  STOP RUN
                END-IF
              END-IF
           END-IF.           
 
034200     SORT PETSEL ON ASCENDING KEY CLASSE OF REC-SEL
                                        SOCIETA-MOD OF REC-SEL
                                        C-MAT OF REC-SEL
034300       INPUT PROCEDURE INPUTSEC                                           
034400       OUTPUT PROCEDURE OUTSEC.
036300 EX-TRATTA.                                                           
036400     EXIT.                                                                
036500*                                                                         
036600*                                                                   
039100*                                                                         
039200*                                                                         
039300 VERIF-INP.                                                               
           DISPLAY "----  INIZIO  -------------------------"
      *MAGCDEP*                                                         INIZIO
039400*    MOVE SPACES TO AS-IN.                                               
039500*    DISPLAY "AnnoStag (2 car) (vuoto=fine) "
039700*                NO ADVANCING.                                            
039800*    ACCEPT  AS-IN.
039900*    IF AS-IN = SPACE                                      
040000*      MOVE 1 TO OK-INP USCITA-PROGRAMMA
      *      GO TO EX-VERIF-INP
040100*    ELSE    
      *         IF AS-IN NOT NUMERIC
      *           DISPLAY "AS non numerici"
      *           GO TO EX-VERIF-INP
      *         ELSE
      *           IF S-IN NOT = 2 AND NOT = 4
      *             DISPLAY "S errata"
      *             GO TO EX-VERIF-INP
      *           END-IF
      *         END-IF
      *    END-IF
        MOVE SPACES TO DISIMPEGNA.
        DISPLAY "Si vuole eliminare impegnato ?(SI/NO) (//=fine)"
        ACCEPT DISIMPEGNA.
        IF DISIMPEGNA = "//"                                      
            MOVE 1 TO OK-INP USCITA-PROGRAMMA
            GO TO EX-VERIF-INP
        END-IF
        IF DISIMPEGNA = "si" 
             MOVE "SI" TO DISIMPEGNA
        END-IF
        IF DISIMPEGNA NOT = "SI"
             MOVE "NO" TO DISIMPEGNA
        END-IF
        MOVE SPACES TO AS-IN.                                               
        DISPLAY "Anno (1 car) (//=fine) "
                    NO ADVANCING.                                            
        ACCEPT AS-IN.
        IF AS-IN = "//"                                      
            MOVE 1 TO OK-INP USCITA-PROGRAMMA
            GO TO EX-VERIF-INP
        ELSE    
            IF A-IN-X NOT NUMERIC AND A-IN-X NOT = SPACES
                DISPLAY "Anno non numerico"
                GO TO EX-VERIF-INP.

        DISPLAY "Stagione (1 car)"
        ACCEPT S-IN-X.
        IF S-IN-X NOT NUMERIC AND S-IN-X NOT = SPACES
            DISPLAY "Stag non numerica"
            GO TO EX-VERIF-INP
        ELSE
            IF S-IN NOT = 2 AND NOT = 4
                DISPLAY "Stag errata"
                GO TO EX-VERIF-INP.
      *MAGCDEP*                                                         fine

      *MAGCDEP*                                                         INIZIO
      *     IF MAG-PAR = "851" 
            IF MAG-PAR = "CDP" 
                MOVE SPACES TO MAG-IN
                IF USCITA-PAR
                    DISPLAY "Magazzino destinazione c/dep " NO ADVANCING
                ELSE
                    DISPLAY "Magazzino provenienza c/dep " NO ADVANCING
                END-IF
                
                ACCEPT MAG-IN
                IF MAG-IN NOT NUMERIC
                    DISPLAY "Magazzino non numerico"
                    GO TO EX-VERIF-INP
                END-IF
                
                PERFORM VERIFICA-CDEP-MAG THRU EX-VERIFICA-CDEP-MAG
                IF SW-CDEP NOT = 1
                    DISPLAY "Magazzino NON C/DEP errato "
                    GO TO EX-VERIF-INP
                ELSE
                    MOVE MAG-IN TO MAG-PAR.
                    MOVE 1 TO MAG-DEST-CDEP.
      *MAGCDEP*                                                         fine
      *
           IF USCITA-PAR
      *MAGSEDE                                                          inizio
      *         DISPLAY "Magazzino provenienza (1,2,4,6,7,8,Cdep) "
      *           NO ADVANCING
               PERFORM POPOLA-MESSAGGIO-MAG THRU EX-POPOLA-MESSAGGIO-MAG
               DISPLAY "MESSAGGIO-MAG = " MESSAGGIO-MAG
      *MAGSEDE                                                          FINE
               ACCEPT MAG-IN
               
               MOVE 3 TO QD-LL-A QD-LL-B
               MOVE 0 TO QD-NR-DEC
               MOVE MAG-IN TO DEED-X3
               CANCEL "QDEEDIT"
               CALL "QDEEDIT" USING PARDEED DEED-X3 DEED-93
               IF QD-STATO OF PARDEED NOT = 0
                  DISPLAY "Magazzino non numerico"
                  GO TO EX-VERIF-INP
               ELSE
                  MOVE DEED-93 TO MAG-IN-R
               END-IF

      *MAGSEDE                                                          inizio
                PERFORM VERIFICA-CDEP-MAG THRU EX-VERIFICA-CDEP-MAG
      *         IF (MAG-IN-R NOT = 6 AND NOT = 7
      *              AND NOT = 4
      *              and not = 1
      *              and not = 8
      *              and not = 2)
      *              and (SW-CDEP NOT = 1)
      *            DISPLAY "Magazzino errato " MAG-IN
      *            GO TO EX-VERIF-INP

                MOVE "magazzini_sede_pf" TO TIPO-MAG-INPUT
                MOVE FUNCTION magsede(MAG-IN-R, TIPO-MAG-INPUT)
                  TO SINO-MAG-SEDE
                DISPLAY  "MAG-IN=" MAG-IN 
                        " sino-mag-sede= " SINO-MAG-SEDE        
               IF (NO-MAG-SEDE) and (SW-CDEP NOT = 1)
                    DISPLAY "Magazzino errato " MAG-IN
                    GO TO EX-VERIF-INP
      *MAGSEDE                                                          FINE
               ELSE
003500*TRASETTE*                                                        inizio
                IF MAG-PAR = "007"
                   IF MAG-IN-R = 2
                     display "Magazzino errato"
                     go to ex-verif-inp
                   END-IF
                ELSE
003500*TRASETTE*                                                        fine
                 if (mag-in-r = 4 or mag-in-r = 1) and
                    (mag-par not = "006" and not = "008"
                       and not = "003" and MAG-DEST-CDEP = 0)
                   display "Magazzino errato"
                   go to ex-verif-inp
                 end-if
003500*TRASETTE*                                                        inizio
                END-IF
003500*TRASETTE*                                                        fine
       
                 if MAG-IN = mag-par
                   display "Magazzino Part/Dest = !!"
                   go to ex-verif-inp
                 end-if                 
      *
                 MOVE MAG-IN-R TO MAGAZZINO-P
               END-IF
               MOVE MAG-PAR TO MAGAZZINO-D                              FISSO DESTINAZIONE
           END-IF.
      *
           IF RIENTRO-PAR
               MOVE SPACES TO MAG-IN
               DISPLAY "Magazzino destinazione  (6,7) " NO ADVANCING
               ACCEPT MAG-IN
               
               MOVE 3 TO QD-LL-A QD-LL-B
               MOVE 0 TO QD-NR-DEC
               MOVE MAG-IN TO DEED-X3
               CANCEL "QDEEDIT"
               CALL "QDEEDIT" USING PARDEED DEED-X3 DEED-93
               IF QD-STATO OF PARDEED NOT = 0
                  DISPLAY "Magazzino non numerico"
                  GO TO EX-VERIF-INP
               ELSE
                  MOVE DEED-93 TO MAG-IN-R
               END-IF
               
               IF MAG-IN-R NOT = 6 AND NOT = 7
                  DISPLAY "Magazzino errato"
                  GO TO EX-VERIF-INP
               ELSE
                  MOVE MAG-IN-R TO MAGAZZINO-D
               END-IF
               MOVE MAG-PAR TO MAGAZZINO-P                              FISSO PROVENIENZA
           END-IF.
      *
           display "MAG provenienza   : " MAGAZZINO-P.
           display "MAG destinazione  : " MAGAZZINO-D.
      *
           MOVE SPACE TO FORN-IN.
           DISPLAY "Forn (1 cifra)   (vuoto=tutti) " NO ADVANCING.
           ACCEPT FORN-IN
           IF FORN-IN NOT = SPACE
             IF FORN-IN NOT NUMERIC
               DISPLAY "Forn non numerico"
               GO TO EX-VERIF-INP
             END-IF
             IF FORN-IN = "0" 
               DISPLAY "Forn errato"
               GO TO EX-VERIF-INP
             END-IF
           END-IF
      *
           MOVE SPACE TO CLASSI-IN
           MOVE 0 TO I-IN.
           MOVE "XX" TO CLASSE-IN
           PERFORM WITH TEST AFTER
      *         UNTIL I-IN = 10 OR     
               UNTIL I-IN = 99 OR
                     CLASSE-IN = SPACE
             MOVE SPACE TO CLASSE-IN
             COMPUTE Z-IN = I-IN + 1
             DISPLAY Z-IN "a " 
               "Classe (2 cifre)(vuoto=fine) " NO ADVANCING
             ACCEPT CLASSE-IN
             IF CLASSE-IN NOT = SPACE
               IF CLASSE-IN NOT NUMERIC
                 DISPLAY "Classe non numerica"
               ELSE
                 ADD 1 TO I-IN 
                 MOVE CLASSE-IN-R TO EL-CLASSE-IN(I-IN)
               END-IF
             END-IF
           END-PERFORM.
      *
           MOVE SPACE TO AGGIORNA-IN.
           DISPLAY "Aggiorni? (SI/NO) " NO ADVANCING.
           ACCEPT AGGIORNA-IN
           IF AGGIORNA-IN = "si" 
             MOVE "SI" TO AGGIORNA-IN
           END-IF
           IF AGGIORNA-IN NOT = "SI"
               MOVE "NO" TO AGGIORNA-IN
           END-IF
      *
           DISPLAY SPACE
           DISPLAY "AnnoStag: " AS-IN
           IF FORN-IN = SPACE
             DISPLAY "tutti Forn"
           ELSE
             DISPLAY "Forn:     " FORN-IN
           END-IF
           IF CLASSI-IN = SPACE
             DISPLAY "tutte Classi"
           ELSE
             DISPLAY "Classi:   " CLASSI-IN
           END-IF.
           IF AGGIORNA-IN = "SI"
             DISPLAY "con aggiornamento"
           ELSE
             DISPLAY "senza aggiornamento" 
           END-IF.           
      *
           DISPLAY "Confermi? (SI/NO) " NO ADVANCING.
           MOVE SPACE TO CONFERMA-IN.
           ACCEPT CONFERMA-IN.
           IF CONFERMA-IN = "SI" OR = "si"
             MOVE 1 TO OK-INP.
       EX-VERIF-INP.
           EXIT.  
      *    
      *
      
      *MAGSEDE                                                          INIZIO
       POPOLA-MESSAGGIO-MAG.
            MOVE "magazzini_sede_pf" TO TIPO-MAG-INPUT.
            MOVE SPACES TO MESSAGGIO-MAG.
            MOVE 1 TO I.
            PERFORM VARYING T FROM 1 BY 1
                  UNTIL T > 16
                    MOVE FUNCTION magsede(T, TIPO-MAG-INPUT) 
                      TO SINO-MAG-SEDE
                      
                    IF SI-MAG-SEDE
                        MOVE T TO M(I)
                        MOVE "," TO V(I)
                        ADD 1 TO I
                    END-IF
            END-PERFORM
            STRING "Magazzino provenienza ("
                    STRINGA-MAG  
                    "Cdep)" 
                    DELIMITED BY SIZE INTO MESSAGGIO-MAG.
       EX-POPOLA-MESSAGGIO-MAG. EXIT.
      *MAGSEDE                                                          fine
      
      
      
      *MAGCDEP*                                                         inizio
       VERIFICA-CDEP-MAG.
            MOVE MAG-IN TO APPO.
            PERFORM LOOP-CDEP THRU EX-LOOP-CDEP.
            MOVE SW-CDEP TO SW-MAG-CDEP.
       EX-VERIFICA-CDEP-MAG. EXIT.
      *
       LOOP-CDEP.
            MOVE 0 TO SW-CDEP.
            PERFORM WITH TEST AFTER 
                    VARYING I-CDEP FROM 1 BY 1 
                    UNTIL I-CDEP > NUM-VALUES-CDEP
                    OR MAG-CDEP(I-CDEP) NOT NUMERIC
                    OR APPO = MAG-CDEP(I-CDEP)
      *                  DISPLAY MAG-CDEP(IND)
            END-PERFORM.
            IF I-CDEP NOT > NUM-VALUES-CDEP
            AND MAG-CDEP(I-CDEP) NUMERIC
            AND APPO = MAG-CDEP(I-CDEP)
                MOVE 1 TO SW-CDEP.
       EX-LOOP-CDEP. EXIT.
      *MAGCDEP*                                                         FINE
      
      *MAGCDEP*                                                         inizio
       CHIAMA-PARAMETRI-CDEP.
          MOVE 'MAG_CONTO_DEP' TO ID-SEZIONE-CDEP.
          MOVE 'deposito_pf' TO CHIAVE-CDEP.
          MOVE 3 TO LEN-ELEMENT-CDEP.
                       
          CALL "PYTHON" USING "get_param" "get_param_multi"
                               PY-INPUT-REC-CDEP
                               PY-OUTPUT-REC-CDEP.
          DISPLAY "PY-INPUT-REC-CDEP  >" PY-INPUT-REC-CDEP "<"
                        UPON SYSERR.
          DISPLAY "PY-OUTPUT-REC-CDEP >" PY-OUTPUT-REC-CDEP "<"
                        UPON SYSERR.
          DISPLAY "Ret. code     : " OUTPUT-RET-CDEP UPON SYSERR.
          DISPLAY "Num. values   : " NUM-VALUES-CDEP UPON SYSERR.
          DISPLAY "Stream values : " STREAM-VALUES-CDEP UPON SYSERR.
       EX-CHIAMA-PARAMETRI-CDEP. EXIT.
      *MAGCDEP*                                                         fine      
      *
035230*                                                                         
035240*                                                                         
035250 INPUTSEC SECTION 2.                                                      
035260* 
      *     MOVE 3 TO W-MODO.                                                    
018030     MOVE "SITPF;" TO W-NOME-DATA-SET.                                   
018040*     PERFORM TTDBCLOSE THRU EX-TTDBCLOSE.    
      *
           MOVE MAGAZZINO-P TO W-VALORE-CAMPO-HW
           
      *MAGCDEP* --------------------------------------------------------INIZIO
      *     DISPLAY MAGAZZINO-P.
      *     DISPLAY W-VALORE-CAMPO-HW.
      *MAGCDEP* --------------------------------------------------------fine
           
           MOVE "MAG" TO W-NOME-CAMPO.
           PERFORM TTDBFIND THRU EX-TTDBFIND.
           IF W-OK-IMAGE
             PERFORM LEGGI-SITPF THRU EX-LEGGI-SITPF.
           PERFORM TRATTA-SITPF THRU EX-TRATTA-SITPF
                 UNTIL NOT W-OK-IMAGE.
039600     GO TO FINE-INPUTSEC.                 
035300* 

       LEGGI-SITPF.
           MOVE "SITPF" TO W-NOME-DATA-SET.
           MOVE 5 TO W-MODO
           PERFORM TTDBGET-S THRU EX-TTDBGET-S.                        
       EX-LEGGI-SITPF.
           EXIT.
      *
       TRATTA-SITPF.
           MOVE C-MAT OF REC-SITPF TO C-MAT-TRANS-RID
                        OF C-MAT-COM.
                        
      *MAGCDEP*                                                         INIZIO
          IF X1 = "SI"
          IF C-MAT OF REC-SITPF = 841401900005001
               DISPLAY "C-MAT OF REC-SITPF "  C-MAT OF REC-SITPF          
               DISPLAY "MAGAZZINO OF REC-SITPF "  
                        MAGAZZINO OF REC-SITPF          
               DISPLAY "SOCIETA-MOD OF C-MAT-COMF "
                        SOCIETA-MOD OF C-MAT-COM          
               DISPLAY "CLASSE OF C-MAT-COM  "  
                        CLASSE OF C-MAT-COM.          
      *MAGCDEP*                                                         fine
                        
                        
      *
           IF MAGAZZINO OF REC-SITPF NOT = MAGAZZINO-P
             PERFORM LEGGI-SITPF THRU EX-LEGGI-SITPF
             GO TO EX-TRATTA-SITPF.             
      *
     
           IF FORN-IN NOT = SPACE AND
              SOCIETA-MOD OF C-MAT-COM NOT = FORN-IN-R
             PERFORM LEGGI-SITPF THRU EX-LEGGI-SITPF
             GO TO EX-TRATTA-SITPF.
      *
           IF CLASSI-IN NOT = SPACE
             PERFORM VARYING IR FROM 1 BY 1
                 UNTIL IR > I-IN OR 
                       EL-CLASSE-IN(IR) = CLASSE OF C-MAT-COM
               CONTINUE
             END-PERFORM
             IF IR > I-IN
               PERFORM LEGGI-SITPF THRU EX-LEGGI-SITPF
               GO TO EX-TRATTA-SITPF.
      *
      
           MOVE "ANAMAT" TO W-NOME-DATA-SET.
           MOVE 7 TO W-MODO.
           MOVE C-MAT OF REC-SITPF TO W-VALORE-CAMPO.
           PERFORM TTDBGET THRU EX-TTDBGET.
      *MAGCDEP*                                                         inizio
      *    IF NOT W-OK-IMAGE OR
      *       (W-OK-IMAGE AND (ANNO OF REC-ANAMAT NOT = A-IN OR
      *                 STAGIONE OF REC-ANAMAT NOT = S-IN))
      *      PERFORM LEGGI-SITPF THRU EX-LEGGI-SITPF
      *      GO TO EX-TRATTA-SITPF.  

           IF NOT W-OK-IMAGE
              PERFORM LEGGI-SITPF THRU EX-LEGGI-SITPF
              GO TO EX-TRATTA-SITPF.  

           IF (ANNO OF REC-ANAMAT NOT = A-IN AND A-IN-X NOT = SPACE)
               OR
              (STAGIONE OF REC-ANAMAT NOT = S-IN AND S-IN-X NOT = SPACE)
                    PERFORM LEGGI-SITPF THRU EX-LEGGI-SITPF
                    GO TO EX-TRATTA-SITPF.  
      *MAGCDEP*                                                         fine
      *      
           IF QTA-GIAC OF REC-SITPF = LOW-VALUE
               PERFORM LEGGI-SITPF THRU EX-LEGGI-SITPF
               GO TO EX-TRATTA-SITPF.           
      *
           PERFORM RILASCIA THRU EX-RILASCIA.
      *
           PERFORM LEGGI-SITPF THRU EX-LEGGI-SITPF.
       EX-TRATTA-SITPF.
           EXIT.
      *
      *
       CALL-DISIMPEGNA-MAG.
           CALL "PYTHON" USING "disimpegna_capi"
                              "elimina_impegnati"
                               PY-INPUT-REC-DISIMPEGNA
                               PY-OUTPUT-DISIMPEGNO.
       EX-CALL-DISIMPEGNA-MAG. EXIT.

       RILASCIA.
           MOVE CLASSE OF C-MAT-COM
               TO CLASSE OF REC-SEL
           MOVE SOCIETA-MOD OF C-MAT-COM
               TO SOCIETA-MOD OF REC-SEL
           MOVE C-MAT OF REC-SITPF
               TO C-MAT OF REC-SEL
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT > NTG-NTG
             IF DISIMPEGNA = "SI"
                 MOVE QTA-GIAC-PF OF REC-SITPF(IT)
                 TO QTA-GIAC-PF OF REC-SEL(IT)
             ELSE
               IF QTA-GIAC-PF OF REC-SITPF(IT) 
                   + QTA-IMP OF REC-SITPF(IT) > 0
                   COMPUTE QTA-GIAC-PF OF REC-SEL(IT) 
                   = QTA-GIAC-PF OF REC-SITPF(IT) 
                   + QTA-IMP OF REC-SITPF(IT)
               ELSE
                 MOVE 0 TO QTA-GIAC-PF OF REC-SEL(IT)
               END-IF
             END-IF
           END-PERFORM
           RELEASE REC-SEL.
       EX-RILASCIA.
           EXIT.
      *
      *
       TTDBGET-S.   COPY PDBGET REPLACING
                  AREA-REC-SET BY REC-SITPF
                  EX-TTDBGET BY EX-TTDBGET-S.
      *
       TTDBGET.   COPY PDBGET.
      * 
063200 TTDBCLOSE.                                                               
063300     COPY PDBCLOSE.      
      *
      *
125800 FINE-INPUTSEC.                                                           
125900*                                                                         
126100*                                                                         
126200*                                                                         
126300 OUTSEC SECTION 3.                                                        
126400* 
           MOVE LOW-VALUE TO QTA-TOTALE.
           MOVE 0 TO FLAG-FILE.
           PERFORM RITORNA THRU EX-RITORNA.
           IF NOT FINE-LETTURA AND
              SI-AGGIORNAMENTO
             PERFORM RICAVA-RIFERIMENTO 
                        THRU EX-RICAVA-RIFERIMENTO
             MOVE 0 TO NUMERO-RIGA-COM
             PERFORM RICAVA-ANAGRAFICA
                 THRU EX-RICAVA-ANAGRAFICA
                 DISPLAY SPACE
             DISPLAY "BOLLA n.  "
077700          NUMERO-DDT.
      *
           PERFORM INTESTA THRU EX-INTESTA.
      *
128500     PERFORM TRATTA-CLASSE                                           
128600       THRU EX-TRATTA-CLASSE                                         
128700       UNTIL FINE-LETTURA.
      *   
           IF SI-AGGIORNAMENTO AND
              NUMERO-RIGA-COM > 0    
             PERFORM CHIAMA-STRAPCAR THRU EX-CHIAMA-STRAPCAR
             PERFORM CHIAMA-PRINTDDT THRU EX-CHIAMA-PRINTDDT
      *MAGCDEP*                                                         inizio
             PERFORM GENERA-EXCEL-PREBOLLE THRU 
                  EX-GENERA-EXCEL-PREBOLLE
      *MAGCDEP*                                                         fine
           END-IF.
           MOVE 0 TO TOT-GIAC
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT > NTG-NTG
                MOVE QTA-GIAC-TOTALE(IT) TO Z9(IT)
                ADD QTA-GIAC-TOTALE(IT) TO TOT-GIAC
           END-PERFORM
           MOVE TOT-GIAC TO Z10.
           DISPLAY "|totale|" 
              Z9(1) "|" Z9(2)
               "|" Z9(3) "|" Z9(4)
               "|" Z9(5) "|" Z9(6)
               "|" Z9(7) "|" Z9(8)
               "|" Z9(9) "|" Z9(10)
               "|" Z10 "|".      
           DISPLAY T1.
      *
129000     GO TO FINE-OUTSEC.   
      *
       INTESTA.
           DISPLAY SPACE.
           
           IF USCITA-PAR
              DISPLAY "                         "
              "T R A S F E R I M E N T I    A L    M A G.    " MAG-PAR
           END-IF.
           IF RIENTRO-PAR
              DISPLAY "                         "
              "R I E N T R O    D A    M A G.    " MAG-PAR
           END-IF.
      *
           DISPLAY T1.
           DISPLAY T2.
           DISPLAY T1.
       EX-INTESTA.
           EXIT.
129100*            
      *
       TRATTA-CLASSE.
           MOVE CLASSE OF REC-SEL TO CLASSE-MEM
           MOVE LOW-VALUE TO QTA-CLASSE.
           PERFORM TRATTA-SOCIETA THRU EX-TRATTA-SOCIETA
               UNTIL FINE-LETTURA OR
                     CLASSE OF REC-SEL <> CLASSE-MEM.
           MOVE CLASSE-MEM TO D2.
           MOVE 0 TO TOT-GIAC
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT > NTG-NTG
                MOVE QTA-GIAC-CLASSE(IT) TO Z9(IT)
                ADD QTA-GIAC-CLASSE(IT) TO TOT-GIAC
           END-PERFORM
           MOVE TOT-GIAC TO Z10
           DISPLAY "|" D2 "|" "   " "|" 
              Z9(1) "|" Z9(2)
               "|" Z9(3) "|" Z9(4)
               "|" Z9(5) "|" Z9(6)
               "|" Z9(7) "|" Z9(8)
               "|" Z9(9) "|" Z9(10)
               "|" Z10 "|".      
             DISPLAY T3.
      *
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT > NTG-NTG
             ADD QTA-GIAC-CLASSE(IT)
                TO QTA-GIAC-TOTALE(IT)
           END-PERFORM.
       EX-TRATTA-CLASSE.
           EXIT.
      *
      *
       TRATTA-SOCIETA.
           MOVE SOCIETA-MOD OF REC-SEL TO SOCIETA-MOD-MEM
           MOVE LOW-VALUE TO QTA-SOCIETA
      *
           IF SI-AGGIORNAMENTO     
077000       MOVE 5 TO W-MODO                                                   
077100       PERFORM TTLOCK-T THRU EX-TTLOCK-T
           END-IF
           PERFORM TRATTA-REC-SEL THRU EX-TRATTA-REC-SEL
               UNTIL FINE-LETTURA OR 
               CLASSE OF REC-SEL <> CLASSE-MEM OR
               SOCIETA-MOD OF REC-SEL <> SOCIETA-MOD-MEM
           IF SI-AGGIORNAMENTO
             PERFORM TTUNLOCK THRU EX-TTUNLOCK
           END-IF
      *
           MOVE SOCIETA-MOD-MEM TO D3
           MOVE 0 TO TOT-GIAC
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT > NTG-NTG
                MOVE QTA-GIAC-SOCIETA(IT) TO Z9(IT)
                ADD QTA-GIAC-SOCIETA(IT) TO TOT-GIAC
           END-PERFORM     
           MOVE TOT-GIAC TO Z10    
           DISPLAY "|" "  " "|" D3 "|"      
             Z9(1) "|" Z9(2)
               "|" Z9(3) "|" Z9(4)
               "|" Z9(5) "|" Z9(6)
               "|" Z9(7) "|" Z9(8)
               "|" Z9(9) "|" Z9(10)
               "|" Z10 "|".               
      *
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT > NTG-NTG
             ADD QTA-GIAC-SOCIETA(IT)
                TO QTA-GIAC-CLASSE(IT)
           END-PERFORM.
       EX-TRATTA-SOCIETA.
           EXIT.
      *
      *
       TRATTA-REC-SEL.
           IF SI-AGGIORNAMENTO
088560           PERFORM PREPARA-MOVMAG THRU EX-PREPARA-MOVMAG                  
088600           PERFORM CREA-MOVMAG-P                                        
088700                 THRU EX-CREA-MOVMAG-P                                  
088800           PERFORM CREA-MOVMAG-D                                    
088900                 THRU EX-CREA-MOVMAG-D                                  
089000           PERFORM AGGIORNA-SITPF                                       
089100                 THRU EX-AGGIORNA-SITPF 
           END-IF.
      *
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT > NTG-NTG
             ADD QTA-GIAC-PF OF REC-SEL(IT)
                TO QTA-GIAC-SOCIETA(IT)
           END-PERFORM.
           IF X1 = "SI"
             DISPLAY "C-MAT: " C-MAT OF REC-SEL
             DISPLAY QTA-GIAC-PF OF REC-SEL(1) SPACE 
                   QTA-GIAC-PF OF REC-SEL(2)
               SPACE QTA-GIAC-PF OF REC-SEL(3) 
               SPACE QTA-GIAC-PF OF REC-SEL(4)
               SPACE QTA-GIAC-PF OF REC-SEL(5) 
               SPACE QTA-GIAC-PF OF REC-SEL(6)
               SPACE QTA-GIAC-PF OF REC-SEL(7) 
               SPACE QTA-GIAC-PF OF REC-SEL(8)
               SPACE QTA-GIAC-PF OF REC-SEL(9)
               SPACE QTA-GIAC-PF OF REC-SEL(10).
      *
           PERFORM RITORNA THRU EX-RITORNA.
       EX-TRATTA-REC-SEL.
           EXIT.
      *
      *
       RITORNA.
128400     RETURN PETSEL AT END MOVE "1" TO FLAG-FILE.
       EX-RITORNA.
           EXIT.
      *
      *
       RICAVA-RIFERIMENTO.
           MOVE W-FORMATO-INTERNO TO AA-MM-GG-DDT.
      *
           MOVE "DPARAM" TO W-NOME-DATA-SET.
           MOVE 3 TO W-MODO.
           PERFORM TTLOCK THRU EX-TTLOCK.
           PERFORM LEGGI-DPARAM THRU EX-LEGGI-DPARAM
           PERFORM AGG-DPARAM THRU EX-AGG-DPARAM.
           PERFORM TTUNLOCK THRU EX-TTUNLOCK.
       EX-RICAVA-RIFERIMENTO.
           EXIT.
      *
       RICAVA-ANAGRAFICA.
072000     MOVE SPACES TO D-CONTO-MEM                                           
072100                    INDIRIZZO-COM INDIRIZZO-C-COM                         
072200                    LOCALITA-COM LOCALITA-C-COM                           
072300                    PROV-COM PROV-C-COM.                                  
072400     MOVE 0 TO CAP-COM.                                                   
072500     MOVE "ANACON;" TO W-NOME-DATA-SET.                                                
072620     COMPUTE W-VALORE-CAMPO-W = 10000000 + MAGAZZINO-D                           
072800     MOVE 7 TO W-MODO.                                                    
072900     PERFORM TTDBGET THRU EX-TTDBGET.                                     
073000     MOVE D-CONTO OF REC-ANACON TO D-CONTO-MEM.                           
073200     PERFORM LEGGI-INDIRIZZI THRU EX-LEGGI-INDIRIZZI.                     
073300     PERFORM MUOVI-INDIRIZZO THRU EX-MUOVI-INDIRIZZO.                     
073400     PERFORM MUOVI-CAP THRU EX-MUOVI-CAP.         
       EX-RICAVA-ANAGRAFICA.
           EXIT.
      *
074000 LEGGI-INDIRIZZI.                                                         
074100     MOVE "CONTO;"  TO W-NOME-CAMPO.                                      
074200     MOVE "INDIRIZ;" TO W-NOME-DATA-SET.                                  
074300     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
074400     MOVE 5 TO W-MODO.                                                    
074500     PERFORM TTDBGET THRU EX-TTDBGET.                                     
074700     IF NOT W-OK-IMAGE                                                    
074900        PERFORM AZZERA-CAMPI-INDIRIZ THRU EX-AZZERA-CAMPI-INDIRIZ.        
075000 EX-LEGGI-INDIRIZZI.                                                      
075100     EXIT.                                                                
075200*                                                                         
075300*                                                                         
075400 AZZERA-CAMPI-INDIRIZ.                                                    
075500     MOVE SPACE TO D-AGG                                                  
075600                   SIGLA-PROV OF REC-INDIRIZZI (1)                        
075700                   SIGLA-PROV OF REC-INDIRIZZI (2)                        
075800                   INDIRIZZO OF REC-INDIRIZZI (1)                         
075900                   INDIRIZZO OF REC-INDIRIZZI (2)                         
076000                   STATO OF REC-INDIRIZZI.                                
076100     MOVE 0 TO CAP OF REC-INDIRIZZI (1)                                   
076200               CAP OF REC-INDIRIZZI (2)                                   
076300               TELEFONO OF REC-INDIRIZZI.                                 
076400 EX-AZZERA-CAMPI-INDIRIZ.                                                 
076500     EXIT.                                                                
076600*                                                                         
076700*                                                                         
076800 MUOVI-INDIRIZZO.                                                         
076900     MOVE INDIRIZZO OF REC-INDIRIZZI (1) TO INDIRIZZO-STD.                
077000     MOVE 66 TO LL-STRINGA-IND.                                           
077100     MOVE 60 TO LL-SUBSTRINGA-IND.                                        
077200     MOVE 1 TO FUNZIONE-IND.                                              
           CANCEL "QSTRINGV"
077300     CALL "QSTRINGV" USING PAR-INDIRIZZO                                  
077400                           INDIRIZZO-STD                                  
077500                           INDIRIZZO-COM.                                 
077600     MOVE 2 TO FUNZIONE-IND.                                              
           CANCEL "QSTRINGV"
077700     CALL "QSTRINGV" USING PAR-INDIRIZZO                                  
077800                           INDIRIZZO-STD                                  
077900                           LOCALITA-COM.                                  
078000     IF INDIRIZZO OF REC-INDIRIZZI (2) NOT = SPACE                        
078100        MOVE INDIRIZZO OF REC-INDIRIZZI (2) TO INDIRIZZO-STD              
078200        MOVE 1 TO FUNZIONE-IND                                            
           CANCEL "QSTRINGV"
078300        CALL "QSTRINGV" USING PAR-INDIRIZZO                               
078400                              INDIRIZZO-STD                               
078500                              INDIRIZZO-C-COM                             
078600        MOVE 2 TO FUNZIONE-IND                                            
           CANCEL "QSTRINGV"
078700        CALL "QSTRINGV" USING PAR-INDIRIZZO                               
078800                              INDIRIZZO-STD                               
078900                              LOCALITA-C-COM.                             
079000 EX-MUOVI-INDIRIZZO.                                                      
079100     EXIT.                                                                
079200*                                                                         
079300*                                                                         
079400 MUOVI-CAP.                                                               
079500     MOVE D-AGG OF REC-INDIRIZZI TO D-CONTO-AGG-MEM.                      
079600     MOVE STATO OF REC-INDIRIZZI TO STATO-COM.                            
079700     MOVE CAP OF REC-INDIRIZZI (1) TO CAP-COM.                            
079800     MOVE SIGLA-PROV OF REC-INDIRIZZI (1) TO PROV-COM.                    
079900     IF INDIRIZZO OF REC-INDIRIZZI (2) NOT = SPACE                        
080000        MOVE SIGLA-PROV OF REC-INDIRIZZI (2) TO PROV-C-COM                
080100        MOVE CAP OF REC-INDIRIZZI (2) TO CAP-C-COM.                       
080200 EX-MUOVI-CAP.                                                            
080300     EXIT.                                                
      *
      *
093600 LEGGI-DPARAM.                                                           
093700     MOVE "DPARAM;" TO W-NOME-DATA-SET.                                   
093800     MOVE "C-AZIENDA;" TO W-NOME-CAMPO.                                   
093900     MOVE 0 TO W-VALORE-CAMPO-HW.                                         
094000     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
094100     IF NOT W-OK-IMAGE                                                    
           CANCEL "QDBERROR"
094200        CALL "QDBERROR" USING W-COMMON.                                   
094300     MOVE 5 TO W-MODO.
           PERFORM DBGET-PARAMDT THRU EX-DBGET-PARAMDT
094600     PERFORM DBGET-PARAMDT THRU EX-DBGET-PARAMDT                          
094700      UNTIL                                                               
094800           W-FINE-CATENA  OR                                              
094900           P-MAGAZZINO.                                                   
095000     IF W-FINE-CATENA                                                     
           CANCEL "QDBERROR"
095100        CALL "QDBERROR" USING W-COMMON                                    
095200     ELSE                                                                 
095400        MOVE NUM-BOLLA-SCARICO-PER-TRASF TO NUMERO-DDT.                   
095500 EX-LEGGI-DPARAM.                                                        
095600     EXIT.                                             
      *
095900 DBGET-PARAMDT.                                                           
096000     PERFORM TTDBGET THRU EX-TTDBGET.                                     
096100     MOVE AREA-REC-SET TO REC-PARAM-RID.                                  
096200 EX-DBGET-PARAMDT.                                                        
096300     EXIT.
      *
091600 AGG-DPARAM.                                                              
091700     MOVE "DPARAM;" TO W-NOME-DATA-SET.                                   
091800     ADD 1 TO NUM-BOLLA-SCARICO-PER-TRASF.                                
091900     MOVE REC-PARAM-RID TO AREA-REC-SET                                   
092000     PERFORM TTUPDATE THRU EX-TTUPDATE.                                   
092100 EX-AGG-DPARAM.                                                           
092200     EXIT.      
      *
106100 PREPARA-MOVMAG.                                                          
106200     MOVE LOW-VALUE TO REC-MOVMAG.                                                                   
106500     MOVE W-FORMATO-INTERNO TO Q-DATA-I.                                  
106600     MOVE 2 TO Q-FUNZIONE OF PARGEN.                                      
           CANCEL "QDATAS"
106700     CALL "QDATAS" USING PARGEN                                           
106800                         Q-DATA-E Q-DATA-I                                
106900                         Q-SETTIMANA.                                     
107000     MOVE Q-SETTIMANA                                                     
107100              TO SETTIMANA OF REC-MOVMAG.                                     
107200     MOVE RIF-BOLLA-DDT TO RIF-INTERNO OF REC-MOVMAG .                         
107400     MOVE 0 TO MOD-IMPUTAZ OF REC-MOVMAG                                      
107500               RIF-ORDINE OF REC-MOVMAG                                       
107600               QUANTITA OF REC-MOVMAG
                     RIF-BOLLA-FORN OF REC-MOVMAG.                                        
107700     MOVE 0 TO PREZZO OF REC-MOVMAG.                                                                 
108300     MOVE "EUR" TO DIVISA OF REC-MOVMAG                                                                
108500*                                                                         
108600     MOVE SPACES TO VAL-REC OF REC-MOVMAG.                                    
108700     MOVE "NR" TO UN-MIS-FATT OF REC-MOVMAG.                                                                            
108911     MOVE C-MAT OF REC-SEL TO C-MAT OF REC-MOVMAG.   
109000 EX-PREPARA-MOVMAG.                                                       
109100     EXIT.
      *
102200 CREA-MOVMAG-P.   
106300     ADD 1 TO NUMERO-RIGA-COM.                                            
106400     MOVE NUMERO-RIGA-COM TO NUMERO-RIGA OF REC-MOVMAG.
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT > NTG-NTG
             COMPUTE QTA-TAGLIA OF REC-MOVMAG(IT) =
                  QTA-GIAC-PF OF REC-SEL(IT) * -1
           END-PERFORM                        
102500     MOVE MAGAZZINO-D TO SOTTO-CONTO.                                        
102600     MOVE W-CLIENTI-ITA TO CAPO-CONTO.                                    
102700     MOVE MAGAZZINO-P TO MAGAZZINO OF REC-MOVMAG.    
102370     MOVE CAUSALE-P TO C-OPE OF REC-MOVMAG.
102800     MOVE CODICE-CONTO TO CONTO OF REC-MOVMAG.                                
102900     MOVE REC-MOVMAG TO AREA-REC-SET.                                         
103000     MOVE "MOVMAG" TO W-NOME-DATA-SET.                                    
103100     PERFORM TTDBPUT THRU EX-TTDBPUT.                                     
103200     IF NOT W-OK-IMAGE                                                    
103400       DISPLAY "ERR PUT MOVMAG-P- " W-STATUS-WORD-IMAGE                               
103500       DISPLAY "PER C-MAT " C-MAT OF REC-MOVMAG                                
           CANCEL "QDBERROR"
103600       CALL "QDBERROR" USING W-COMMON.                                    
103700 EX-CREA-MOVMAG-P.    
           EXIT.
      *
100000 CREA-MOVMAG-D. 
106300     ADD 1 TO NUMERO-RIGA-COM.                                            
106400     MOVE NUMERO-RIGA-COM TO NUMERO-RIGA OF REC-MOVMAG.
100500     MOVE MAGAZZINO-D TO MAGAZZINO OF REC-MOVMAG.                                
100600     MOVE W-CLIENTI-ITA TO CAPO-CONTO.                                    
100700     MOVE MAGAZZINO-P TO SOTTO-CONTO                                       
100800     MOVE CODICE-CONTO TO CONTO OF REC-MOVMAG.  
102370     MOVE CAUSALE-D TO C-OPE OF REC-MOVMAG.
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT > NTG-NTG
             MOVE QTA-GIAC-PF OF REC-SEL(IT) 
                 TO QTA-TAGLIA OF REC-MOVMAG(IT)
           END-PERFORM                          
101000     MOVE REC-MOVMAG TO AREA-REC-SET.                                         
101100     MOVE "MOVMAG" TO W-NOME-DATA-SET.                                    
101200     PERFORM TTDBPUT THRU EX-TTDBPUT.                                     
101300     IF NOT W-OK-IMAGE                                                    
101400       MOVE W-STATUS-WORD-IMAGE TO ERR-D                                  
101500       DISPLAY "ERR PUT MOVMAG-D- " ERR-D                                
101600       DISPLAY "PER C-MAT " C-MAT OF REC-MOVMAG                                
           CANCEL "QDBERROR"
101700       CALL "QDBERROR" USING W-COMMON.                                    
101800 EX-CREA-MOVMAG-D.                                                      
101900     EXIT.       
      *
104100 AGGIORNA-SITPF.                                                        
104200     MOVE LOW-VALUE TO PARAGGPF.                                          
104300     MOVE C-MAT OF REC-SEL TO C-MAT OF PARAGGPF.                           
104400     MOVE MAGAZZINO-D TO MAGAZZINO OF PARAGGPF.                            
104500     MOVE -1 TO VALORE OF PARAGGPF.     
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT  > NTG-NTG
              MOVE QTA-GIAC-PF OF REC-SEL(IT)
104700            TO QTA OF PARAGGPF(IT)
           END-PERFORM
104800     MOVE 1 TO F-GIAC OF PARAGGPF.
           CANCEL "AGSITPFW"
104900     CALL "AGSITPFW" USING W-COMMON PARAGGPF.                                                                                           
100210*                                                         
105200     MOVE MAGAZZINO-P TO MAGAZZINO OF PARAGGPF.                              
105300     MOVE -1 TO F-GIAC OF PARAGGPF.                                       
           CANCEL "AGSITPFW"
105400     CALL "AGSITPFW" USING W-COMMON PARAGGPF.                             
105500 EX-AGGIORNA-SITPF.                                                     
105600     EXIT.    
      *
      *
080600 CHIAMA-STRAPCAR. 
           MOVE RIF-BOLLA-DDT TO RIF-CNTR.
      *
080610     MOVE NUMERO-CNTR-R TO NUMERO-CNTR.
           MOVE AAMMGG-CNTR-R TO AAMMGG-CNTR
080620     MOVE MAGAZZINO-D TO MAG-CNTR.           
      *
      
      *MAGCDEP*                                                         inizio
           IF X1 = "SI"
            display "x strapcar  mag-cntr=" MAG-CNTR                              
                               " bolla=" NUMERO-CNTR
                               " data-bolla=" AAMMGG-CNTR .
      *MAGCDEP*                                                         inizio
                              
                               
           CANCEL "STRAPCAR"
081700     CALL "STRAPCAR" USING W-COMMON                                        
082030                           MAG-CNTR                              
082040                           NUMERO-CNTR AAMMGG-CNTR .                     
082080*                                                                         
082100 EX-CHIAMA-STRAPCAR.                                                      
082200     EXIT.        
      *
      *
097100 CHIAMA-PRINTDDT.                                                         
097200     MOVE MAGAZZINO-D TO MAGAZZINO-DDT.                                                                  
097260     MOVE 2 TO TIPO-DOC-DDT.                                            
097270*                                                                         
097400     MOVE 25 TO TIPO-MOVIMENTO-DDT.                                       
097500     MOVE CAUSALE-D TO CAUSALE-DDT.                                                                          
097600     MOVE 1 TO TIPO-STAMPA-DDT.  
100600     MOVE W-CLIENTI-ITA TO CAPO-CONTO.                                    
100700     MOVE MAGAZZINO-P TO SOTTO-CONTO                                       
100800     MOVE CODICE-CONTO TO  CLIENTE-DDT.                                   
097800     MOVE LOCALITA-PART-STR TO LOC-PART-DDT.                              
097900     MOVE SPACES TO TIPO-DATA-SET-DDT                                     
098000                    D-CONTO-VET                                           
098100                    NOTE-DDT (1)                                          
098200                    NOTE-DDT (2).                                         
098300     MOVE 0 TO IMPORTO-X-PL.                                              
098400     MOVE "N" TO FILE-FAT-DDT.                                                                                                          
098421     MOVE SPACE TO RIGA-1-DDT RIGA-2-DDT.                                             
      * NO-DATGE                                                        inizio 
      *     CANCEL "PRINTDD6"
098510*     CALL "PRINTDD6" USING W-COMMON                                       
           CANCEL "PRINTDDF"
098510     CALL "PRINTDDF" USING W-COMMON                                       
      * NO-DATGE                                                        FINE 
098600                           SQLCA                                          
098700                           CAMPI-ANAGRAFICI                               
098800                           CAMPI-COMODO                                   
098900                           TIPO-DATA-SET-DDT                              
099000                           IMPORTO-X-PL                                   
099100                           FILE-FAT-DDT                                                                          
099120                           RIGA-1-DDT                                     
099130                           RIGA-2-DDT.                                    
099140*                                                                         
099200 EX-CHIAMA-PRINTDDT.                                                      
099300     EXIT.                                                   
      *
      *MAGCDEP*                                                         inizio
       GENERA-EXCEL-PREBOLLE.
            MOVE ";" TO SEP1 SEP2.
            MOVE RIF-BOLLA-DDT   TO RIF-CNTR.
            MOVE AAMMGG-CNTR-R   TO DATA-EXCEL-PREBOLLA.
            MOVE NUMERO-CNTR-R   TO NUM-EXCEL-PREBOLLA.
            MOVE 10000           TO C-CONTO-EXCEL-PREBOLLA.                                               
            MOVE CODICE-CONTO    TO S-CONTO-EXCEL-PREBOLLA.
            MOVE MAGAZZINO-D     TO MAG-EXCEL-PREBOLLA.

            CALL "PYTHON" USING "cobolpass_excel_prebolle" 
                                "genera_excel_prebolla"
                        PY-INPUT-REC-EXCEL-PREBOLLA
                        PY-OUTPUT-REC-EXCEL-PREBOLLA
            DISPLAY "PY-INPUT-REC-EXCEL-PREBOLLA  >" 
                     PY-INPUT-REC-EXCEL-PREBOLLA "<"       
            DISPLAY "PY-OUTPUT-REC-EXCEL-PREBOLLA  >" 
                     PY-OUTPUT-REC-EXCEL-PREBOLLA "<"       
      *               UPON SYSERR
            .
       EX-GENERA-EXCEL-PREBOLLE. EXIT.
      *MAGCDEP*                                                         FINE

      *
       TTLOCK.  COPY PDBLOCK.
090500 TTUNLOCK.                                                                
090600     COPY PDBUNLOC.  
090900 TTLOCK-T.  COPY PDBLOCK REPLACING                                        
091000                W-NOME-DATA-SET BY TAB-LOCK                               
091100                EX-TTLOCK BY EX-TTLOCK-T.
      *
092700 TTDBPUT.                                                                 
092800     COPY PDBPUT.                                                         
092900*                                                                                                                            
093100 TTUPDATE.                                                                
093200     COPY PDBUPDAT.    
      *
069000 TTDBFIND.                                                                
069100     COPY PDBFIND.
131800*                                                                         
131900*                                                                         
132000 FINE-OUTSEC.                                                             
132100*                                                                         
132200*
