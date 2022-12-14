001000*CONTROL DYNAMIC,BOUNDS                                                   
001100 IDENTIFICATION DIVISION.                                                 
001200 PROGRAM-ID. WRITERES.                                                    
001300*                                                                         
001400*CODI*   16/07/98                                                         
001500*      trasferisce COSTO-EFF-VALUTA per neg 51 in RESIDUO e               
001600*      controlla accesso esclusivo a file WRITE                           
001700*                                                                         
001800*2000*        05/08/99                                                    
001900*     tratta date a 6 cifre                                               
002000*                                                                         
002100*EURO*            14/04/2000                EURO/LIRE                     
002200*     trattamento importi in EURO                                         
002300*                                                                         
002310*MAXIMA*      12/09/2003                                                  
002320*      tratta anche il passaggio dati se il destinatario                  
002330*      e' la MAXIMA (la provenienza e' indifferente, puo'                 
002340*      essere DT come pure MA, MR, PB, CA)                                
002350*   
      *ACQUO*       10/08/11
      *      ACQUisizione Outlet esteri:
      *         per  W-ACCOUNT = "PROROSA" scrive in DANEG/Enegxxxx (cioe' /home/prorosa/DANEG/Enegxxxx)
      *         per  altri                        in /home/tr/DANEG/Enegxxxx
      *
      *         scrive rec 21 per tutti neg ESTERO;
      *         non scrive rec 15 per tutti neg ESTERO
      *
      *REC01*       24/02/2016  -  LAUROS
      *     Da ora le conferme di carico (record 01) arrivano direttamente
      *     dal negozio
      *
      *ESTETA*     20/11/18
      *      estensione taglie
002400 ENVIRONMENT DIVISION.                                                    
002500 CONFIGURATION SECTION.                                                   
002600 SOURCE-COMPUTER.  HP-3000.                                               
002700 OBJECT-COMPUTER.  HP-3000.                                               
002800 SPECIAL-NAMES.                                      
002900                DECIMAL-POINT IS COMMA.                                   
003000 INPUT-OUTPUT SECTION.                                                    
003100 FILE-CONTROL.                                                            
003200*CODI*                                                                    
      *conv
003300*    SELECT FILE-WRITE ASSIGN TO "WRITE" FILE STATUS IS CHECKER         
      *
           SELECT OPTIONAL FILE-WRITE ASSIGN TO "WRITE"
                  FILE STATUS IS CHECKER
                  ORGANIZATION IS LINE SEQUENTIAL.
      *
      *conv-end
      *ACQUO*                                                           inizio
          SELECT OPTIONAL FILE-MERCFR ASSIGN TO MERCFR-NOMEFILE
              ORGANIZATION IS LINE SEQUENTIAL.
002300    SELECT OPTIONAL FILE-DANEG 
              ASSIGN TO DANEG-NOMEFILE FILE STATUS IS M-F                 
              ORGANIZATION IS LINE SEQUENTIAL.              
      *ACQUO*                                                           fine
003400*                                                                         
003500 DATA DIVISION.                                                           
003600 FILE SECTION.                                                            
003700*                                                                         
003800 FD FILE-WRITE DATA RECORD REC-WRITE.                                     
003900 01 REC-WRITE   COPY DRECTRAS.    
      *
      *ACQUO*                                                           inizio
002717 FD FILE-MERCFR                                                           
002718                  DATA RECORD IS REC-MERCFR.  
       01 REC-MERCFR       PIC X(512). 
003600 FD FILE-DANEG                                  
003700           DATA RECORD IS REC-MOV01.                                      
003800 01 REC-MOV01.                                                            
003900   03 RESTO-REC    COPY DASPXASD.               
      *ACQUO*                                                           fine      
004000*                                                                         
004100 WORKING-STORAGE SECTION.                                                 
      *ESTETA* 
       COPY NTG.                                                                       
004300 77 ERR-DISP               PIC -(6).                                      
004400*CODI*                                                                    
004500 77 CHECKER        PIC XX.                                                
      *conv
         88 CHECKER-APERTO VALUES ARE "05", "00".
      *conv-end
004600*                                                                         
004700*SQL                                                                      
004800*                                                                         
004900 EXEC SQL BEGIN DECLARE SECTION END-EXEC.                                 
005000*
      *ACQUO*                                                           inizio
005100* EXEC SQL INCLUDE MODELLI.IF END-EXEC.
       EXEC SQL INCLUDE CONTESTE.IF END-EXEC.
013300 EXEC SQL INCLUDE INDNEG.IF END-EXEC.       
      *ACQUO*                                                           fine
005200*                                                                         
005300 EXEC SQL END DECLARE SECTION END-EXEC.                                   
005400*                                                                         
005500 01 SQL-CONST             COPY SQLCONST .                       
005600 01 PAR-ERR               COPY PARERR .                         
005700 01 AREA-HL               COPY AREAHL .                         
005800 01 AREA-SI               COPY AREASI .                         
005900*                                                                         
006000*SQL                                                                      
006100*                                                                         
006200 01 AREA-REC-SET         PIC X(512).                                      
006300*                                                                         
006400 01 FILLER REDEFINES AREA-REC-SET.                                        
006500   05 REC-COMPOS   COPY YCOMPOS.                                          
006600*                                                                         
006700 01 FILLER REDEFINES AREA-REC-SET.                                        
006800   05 REC-PREZZI    COPY YPREZZI.                                         
006900*                                                                         
007000 01 MOVMAG  COPY YMOVMAG.                                                 
007100*                                                                         
007200 01 REC-ANAMAT COPY YANAMAT.                                              
007300*                                                                         
007400*CODI*                                                                    
007500 01 COMANDO-FILE-EST.                                                     
007600*CODI*                                                                    
007700  05 FILLER        PIC X(40) VALUE                                        
007800     "FILE WRITE=XESTERO.ESTERO;ACC=APPEND;SHR".                          
007900  05 C-R           PIC X VALUE X"13".                                       
008000*                                                                         
008100*MAXIMA*                                                                  
008200 01 COMANDO-FILE-MAX.                                                     
008300  05 FILLER        PIC X(40) VALUE                                        
008400     "FILE WRITE=XMAXIMA.MAXIMA;ACC=APPEND;SHR".                          
008500  05 C-R           PIC X VALUE X"13".                                       
008600*                                                                         
008700 01 ERR         PIC S9(4) COMP.                                           
008800 01 ERR-PARM    PIC S9(4) COMP.                                           
008900*                                                                         
009000 01 COMANDO-FILE-RES.                                                     
009100*CODI*                                                                    
009200  05 FILLER        PIC X(42) VALUE                                        
009300     "FILE WRITE=XRESIDUO.RESIDUO;ACC=APPEND;SHR".                        
009400  05 C-R           PIC X VALUE X"13".                                       
009500*                                                                         
009510 01 SOCIETA-COMODO       PIC 99.                                          
009600*                                                                         
009700 01 DEST-WR-COM     PIC S9(4) COMP.                                       
009800    88 XESTERO     VALUE 3.                                               
009900    88 XRESIDUO    VALUE 2.                                               
009901*MAXIMA*                                                                  
009910    88 XMAXIMA     VALUE 1.                                               
010000*                                                                         
010100 01 PIATTAFORMA       PIC S9(9) COMP VALUE 10099553.                      
010200 01 DIFFTESSILE       PIC S9(9) COMP VALUE 10099053.                      
010300*CODI*                                                                    
010400 01 BUDAPEST          PIC S9(9) COMP VALUE 10099051.                      
010401*MAXIMA*                                                                  
010410 01 MAXIMA            PIC S9(9) COMP VALUE 10007035.                      
010500*                                                                         
010600 01 C-MAT-COM    COPY DANCODMT.                                           
010700*                                                                         
010800*CODI*                                                                    
010900 01 PREZZO-VENDITA-PREZZIA  PIC S9(9) COMP.                               
011000*                                                                         
011100*2000*                                                                    
011200 01 P-2000 COPY QPAR2000.                                                 
011300*                                                                         
011301*EURO*                                                                    
011310 01 PAR-EURO        COPY QPAREURO.                                        
011320 01 PAR-INEU   COPY QPARINEU.                                             
      *conv
      *
       01 COMANDO-SLEEP-LINUX.
         05 FILLER       PIC X(6) VALUE "sleep ".
         05 N-SEC-SLEEP  PIC 99.
         05 FILLER       PIC X VALUE X"00".
      *
       01 WK-VAR-NAME             PIC X(80).
       01 WK-VAR-VALUE            PIC X(80).
      *
      *conv-end
      *
      *ACQUO*                                                           inizio
004760 01 REC-MERCFR-COM.                                                       
004860   05 MERCATO-COM     PIC XXX.                                            
004960   05 MERCATO-COM-D REDEFINES MERCATO-COM                                 
005060                      PIC 999.                                            
005160   05 LINGUA-COM      PIC X(4).                                           
005260   05 SIGLA-NAZIONE-COM  PIC X(4).                                        
005360   05 DIVISA-COM         PIC X(4).                                        
005460   05 NRO-DEC-COM        PIC XX.                                          
005560   05 NRO-DEC-COM-D REDEFINES NRO-DEC-COM                                 
005660                         PIC 99.                                          
005860   05 CAMBIO-DVS-COM     PIC X(14).                                       
005960   05 CAMBIO-DVS-COM-D REDEFINES CAMBIO-DVS-COM  PIC 9(14).               
006060                                                                        
006170 01 DEP-DEED-8         PIC X(16).                                         
006270 01 DEP-DEED-8-9       PIC 9(16).     

004590 01 T-TAB PIC X VALUE X"9".                                                
004600                                                                        
004610 01 FINITO  PIC S9(4) COMP.                                               
004620  88 FINE-FILE VALUE 1.
        88 TROVATO-RECORD VALUE 2.
004610 01 TRATTATO  PIC S9(4) COMP.                                               
004620  88 GIA-TRATTATO VALUE 1.   
        88 NON-TRATTATO VALUE -1.
                                                                                                                                                 
006570 01 PARDEED   COPY QPARDEED.           
      *
       01 REC-ANACON  COPY YANACON.
       
       01 COSTO-COM     PIC S9(18) COMP.
       01 I             PIC S9(4) COMP.
       01 DIFF-DEC      PIC S9(4) COMP.
        
019200 01 NEG-DANEG          PIC 999.                                          
019300 01 PROGR-DANEG        PIC 9(4).       
  
018300 01 M-F            PIC XX.                                                
018400  88 M-F-APERTO VALUES ARE "05", "00".

       01 IT             PIC S9(4) COMP.
       01 NT             PIC S9(4) COMP.
       
       01 DATA-COM       PIC 9(6).
       01 FILLER REDEFINES DATA-COM.
         05 AA-COM       PIC 9(2).
         05 MM-COM       PIC 99.
         05 GG-COM       PIC 99.
      *ACQUO                                                            fine
011400*                                                                         
011500 LINKAGE SECTION.                                                         
011600*                                                                         
011700 01 W-COMMON COPY WCOMMONW.                                               
011800*                                                                         
011900 EXEC SQL INCLUDE SQLCA END-EXEC.                                         
012000*                                                                         
012100 01 CAMPI-UTILI.                                                          
012200  05 RIF-INTR-WR        PIC 9(12).                                        
012300  05 FILLER REDEFINES RIF-INTR-WR.                                        
012400   10 DATA-WR        PIC 9(6).                                            
012500   10 NUMERO-WR      PIC 9(6).                                            
012600  05 CONTO-CLI-WR       PIC S9(9) COMP.                                   
012700  05 CONTO-DEST-WR      PIC S9(9) COMP.                                   
012800  05 MAGAZZINO-WR       PIC S9(4) COMP.                                   
012900  05 DEST-WR            PIC S9(4) COMP.                                   
012910*EURO*                                                                    
012920* non piu' utilizzato                                                     
013000  05 DIVISA-WR          PIC XXXX.                                         
013010*                                                                         
013100  05 LISTINO-WR         PIC 9(4).                                         
013200  05 CAUSALE-WR         PIC X(4).                                         
013300*                                                                         
013400 01 DATA-CARICO-WR      PIC 9(6).                                         
013500*                                                                         
013600*                                                                         
013700*PAGE                                                                     
013800*                                                                         
013900*                                                                         
014000 PROCEDURE DIVISION USING W-COMMON                                        
014100                          SQLCA                                           
014200                          CAMPI-UTILI DATA-CARICO-WR.                     
014300 INIZIO.                                                                  
014400*                                                                         
014500*
014600     MOVE DEST-WR TO DEST-WR-COM.      
      *
014700     IF XESTERO                                                           
           CANCEL "COMMAND2"
014800        CALL "COMMAND2" USING COMANDO-FILE-EST,                  
014900                                     ERR, ERR-PARM                        
015000        IF ERR NOT = 0                                                    
015100           MOVE ERR TO ERR-DISP                                           
015200           DISPLAY "Errore COMMAND file XESTERO "                         
015300                SPACE SPACE ERR-DISP      
                    UPON SYSERR                               
015400           STOP RUN.                                                      
015401*MAXIMA*                                                                  
015410     IF XMAXIMA                                                           
           CANCEL "COMMAND2"
015420        CALL "COMMAND2" USING COMANDO-FILE-MAX,                  
015430                                     ERR, ERR-PARM                        
015440        IF ERR NOT = 0                                                    
015450           MOVE ERR TO ERR-DISP                                           
015460           DISPLAY "Errore COMMAND file XMAXIMA "                         
015470                SPACE SPACE ERR-DISP     
                    UPON SYSERR                                 
015480           STOP RUN.                                                      
015600     IF XRESIDUO                                                          
           CANCEL "COMMAND2"
015700        CALL "COMMAND2" USING COMANDO-FILE-RES,                  
015800                                     ERR, ERR-PARM                        
015900        IF ERR NOT = 0                                                    
016000           MOVE ERR TO ERR-DISP                                           
016100           DISPLAY "Errore COMMAND file XRESIDUO "                        
016200                SPACE SPACE ERR-DISP      
                    UPON SYSERR                                
016300           STOP RUN.                                                      
      *conv
016400*    OPEN OUTPUT FILE-WRITE.                                              
016500*CODI*                                                                    
016600*    EXCLUSIVE FILE-WRITE CONDITIONALLY.                                  
016700*    IF CHECKER IS NOT EQUAL TO "00"                                      
016800*      DISPLAY "WRITERES: in attesa OPEN esclusiva di WRITE "             
016900*         "" UPON SYSERR                                   
017000*      CLOSE FILE-WRITE                                                   
017100*      OPEN OUTPUT FILE-WRITE                                             
017200*      EXCLUSIVE FILE-WRITE.                                              
      *
           OPEN EXTEND FILE-WRITE WITH LOCK.
           IF NOT CHECKER-APERTO
              DISPLAY "**WRITERES: in attesa OPEN esclusiva di WRITE"
                      " - file CHECKER=" CHECKER
                    UPON SYSERR
           END-IF.
           PERFORM UNTIL CHECKER-APERTO
              MOVE 5 TO N-SEC-SLEEP
              CALL "SYSTEM" USING COMANDO-SLEEP-LINUX
              OPEN EXTEND FILE-WRITE WITH LOCK
           END-PERFORM.
      *
      *conv-end
017300*                                                                         
017310     MOVE 5 TO SOCIETA-COMODO.                                            
017311     IF W-SIGLA-UTENTE = "MARINA"                                         
017312        MOVE 4 TO SOCIETA-COMODO.                                         
017313     IF W-SIGLA-UTENTE = "MARELLA"                                        
017314        MOVE 2 TO SOCIETA-COMODO.                                         
017315     IF W-SIGLA-UTENTE = "PENNY"                                          
017316        MOVE 3 TO SOCIETA-COMODO.                                         
017317     IF W-SIGLA-UTENTE = "COMMABBI"                                       
017318        MOVE 11 TO SOCIETA-COMODO.                                        
017319*MAXIMA*                                                                  
017320     IF W-SIGLA-UTENTE = "RESIDUO"                                        
017321       AND XMAXIMA                                                        
017325        MOVE 13 TO SOCIETA-COMODO. 
017400     PERFORM TRATTA-NEG THRU EX-TRATTA-NEG.    
017500     CLOSE FILE-WRITE.  
      *
017600 FINE.                                                                    
017700     EXIT PROGRAM.                                                        
017800*                                                                         
017900*                                                                         
018000*                                                                         
018100*                                                                         
018200 TRATTA-NEG.        
      *ACQUO*                                                           inizio
           MOVE 0 TO TRATTATO
      *ACQUO*                                                           fine
      *
018300     MOVE "MOVMAG;" TO W-NOME-DATA-SET.                                   
018400     MOVE "RIF-INTR;" TO W-NOME-CAMPO.                                    
018500     MOVE RIF-INTR-WR TO W-VALORE-CAMPO.                                  
018600     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
018700     IF W-OK-IMAGE                                                        
018800        MOVE 5 TO W-MODO
018900        PERFORM TTDBGET THRU EX-TTDBGET
019000        PERFORM SCRIVI-REC THRU EX-SCRIVI-REC                             
019100                  UNTIL W-FINE-CATENA   
      *ACQUO*                                                           inizio
                              OR NON-TRATTATO
      *ACQUO*                                                           fine
019110       ELSE                                       
019120          DISPLAY "WRITERES "
                       RIF-INTR-WR "   " MAGAZZINO-WR
                ":  manca MOVMAG "
                    UPON SYSERR.                         
019200 EX-TRATTA-NEG.                                                           
019300     EXIT.                                                                
019400*                                                                         
019500*                                                                         
019600 SCRIVI-REC.
019700     MOVE AREA-REC-SET TO MOVMAG.                                         
019800     IF CONTO-CLI-WR = CONTO OF MOVMAG                                    
019900             AND MAGAZZINO-WR = MAGAZZINO OF MOVMAG                       
020000               AND C-OPE OF MOVMAG = CAUSALE-WR      
      *ACQUO*                                                           inizio
      *BUDI
      *        IF XESTERO AND CONTO-CLI-WR NOT = BUDAPEST
               IF XESTERO
      *
                IF NOT GIA-TRATTATO
                  MOVE 1 TO TRATTATO
                  PERFORM CARICA-MERCFR THRU EX-CARICA-MERCFR
                  IF NOT NON-TRATTATO
                    PERFORM VEDI-OUT-EST THRU EX-VEDI-OUT-EST
                  END-IF
                  IF NOT NON-TRATTATO
                    PERFORM VEDI-INDICI-NEG
                          THRU EX-VEDI-INDICI-NEG
      *REC01*                                                           inizio
      *             IF NOT NON-TRATTATO
      *               PERFORM APRI-DANEG
      *                   THRU EX-APRI-DANEG    
      *               PERFORM SCRIVI-DANEG THRU EX-SCRIVI-DANEG   
      *               CLOSE FILE-DANEG
      *             END-IF       
      *REC01*                                                           fine
                  END-IF                  
                END-IF
              END-IF 
              
              IF NOT NON-TRATTATO
      *ACQUO*                                                           fine
020100        MOVE C-MAT OF MOVMAG TO C-MAT-TRANS-RID 
020200        PERFORM TRATTA-IMAGE THRU EX-TRATTA-IMAGE.                        
020300     MOVE "MOVMAG;" TO W-NOME-DATA-SET.                                   
020400     MOVE 5 TO W-MODO.                                                    
020500     PERFORM TTDBGET THRU EX-TTDBGET.                                     
020600 EX-SCRIVI-REC.                                                           
020700     EXIT.                                                                
020800*                                                                         
020900*                                                                         
021000*                                                                         
021100*                                                                         
021200 TRATTA-IMAGE.                                                            
021300     MOVE "ANAMAT;" TO W-NOME-DATA-SET.                                   
021400     MOVE "C-MAT;" TO W-NOME-CAMPO.                                       
021500     MOVE C-MAT OF MOVMAG TO W-VALORE-CAMPO.                              
021600     MOVE 7 TO W-MODO.                                                    
021700     PERFORM TTDBGET THRU EX-TTDBGET.                                     
021710     COPY PEURO.                                                          
021800     MOVE AREA-REC-SET TO REC-ANAMAT.                                     
021900     PERFORM SCRIVI-ANAMAT THRU EX-SCRIVI-ANAMAT.                         
022000     PERFORM SCRIVI-COMPOS THRU EX-SCRIVI-COMPOS.                         
022100     PERFORM CERCA-PREZZI THRU EX-CERCA-PREZZI.                           
022200*CODI*                                                                    
022300     MOVE 0 TO PREZZO-VENDITA-PREZZIA.                                    
022400     IF (XRESIDUO                                                         
022500            AND CAPO-PROD-DIFF-TESS OF REC-ANAMAT)                        
022600        PERFORM CERCA-PREZZIA THRU EX-CERCA-PREZZIA.                      
022700     MOVE SPACES TO RECORD-DA-SOCIETA.                                    
022800     MOVE DEST-WR TO DEST-REC OF RECORD-21-22-23-24.                      
022900     MOVE SOCIETA-COMODO TO SOCIETA OF RECORD-21-22-23-24.                
023600     MOVE DIVISA OF MOVMAG TO VALUTA-ACQ OF RECORD-21-22-23-24            
023700                    VALUTA-VEND OF RECORD-21-22-23-24.                    
023710*CODI*                                                                    
023720     IF XESTERO AND CONTO-CLI-WR = BUDAPEST                               
023730       MOVE "HUF " TO VALUTA-ACQ OF RECORD-21-22-23-24                    
023740                      VALUTA-VEND OF RECORD-21-22-23-24.                  
023800     MOVE LISTINO-WR TO                                                   
023900              LISTINO-ACQ OF RECORD-21-22-23-24                           
024000              LISTINO-VEND OF RECORD-21-22-23-24.                         
024100     MOVE 0 TO COD-ANNULLO OF RECORD-21-22-23-24                          
024200              DATA-FATTURA OF RECORD-21-22-23-24                          
024300              NUM-FATTURA OF RECORD-21-22-23-24                           
024400              COSTO-EFF-VALUTA OF RECORD-21-22-23-24.                     
024500     MOVE "N" TO COD-RIASSORTIMENTO OF RECORD-21-22-23-24.                
024600     IF (MAGAZZINO-WR NOT < 11 AND (W-SIGLA-UTENTE = "MAXMAX"             
024700              OR W-SIGLA-UTENTE = "MARINA" OR                             
024800              W-SIGLA-UTENTE = "MARELLA" OR                               
024900              W-SIGLA-UTENTE = "PENNY" OR                                 
024901*MAXIMA*                                                                  
024910              W-SIGLA-UTENTE = "COMMABBI")) OR XMAXIMA                    
025000       MOVE "MX" TO MAGAZZINO OF RECORD-21-22-23-24                       
025100       COMPUTE MAG-PARTENZA-MX OF RECORD-21-22-23-24 =                    
025200             10000000 + MAGAZZINO-WR                                      
025300      ELSE                                                                
025400        MOVE "DN" TO MAGAZZINO OF RECORD-21-22-23-24                      
025500        MOVE 0 TO MAG-PARTENZA-MX OF RECORD-21-22-23-24.                  
025600     MOVE "F" TO TIPO-MOV OF RECORD-21-22-23-24.                          
025700     MOVE CONTO-DEST-WR TO CODICE-MAXIMA                                  
025800                            OF RECORD-21-22-23-24.         

025900     IF CONTO-DEST-WR = PIATTAFORMA                                       
026000        MOVE 10000008 TO CODICE-MAXIMA OF RECORD-21-22-23-24.             
026100     IF CONTO-DEST-WR = DIFFTESSILE                                       
026200        MOVE 10000007 TO CODICE-MAXIMA OF RECORD-21-22-23-24.             
026201*MAXIMA*                                                                  
026210     IF CONTO-DEST-WR = MAXIMA                                            
026220        MOVE 10000044 TO CODICE-MAXIMA OF RECORD-21-22-23-24.    
      *ACQUO*                                                           inizio
           IF XESTERO AND CONTO-CLI-WR NOT = BUDAPEST
             MOVE INDNEG-NEGOZIO 
                 TO CODICE-MAXIMA OF RECORD-21-22-23-24.
      *ACQUO*                                                           fine
026300     MOVE C-MAT-TRANS-RID TO                                              
026400                  C-MAT OF RECORD-21-22-23-24.                            
026500     MOVE NUMERO-WR TO NUM-ORD-CAR OF RECORD-21-22-23-24.                 
026600     MOVE DATA-WR TO DATA-ORD-CAR OF                                      
026700                        RECORD-21-22-23-24.                               
026800     IF DATA-CARICO-WR NOT = 0                                            
026900       MOVE DATA-CARICO-WR TO DATA-ORD-CAR OF                             
027000                         RECORD-21-22-23-24.                              
027100     PERFORM METTI-QTA-TAGLIA THRU EX-METTI-QTA-TAGLIA                    
027200        VARYING W-INDICE-7 FROM 1 BY 1                                    
027300        UNTIL W-INDICE-7 > NTG-NTG.                                             
027400     COMPUTE COSTO-EFF-LIRE OF RECORD-21-22-23-24 =                       
027500           COSTO-STD OF MOVMAG.    
      *ACQUO*                                                           inizio
027510*     IF XESTERO AND CONTO-CLI-WR = BUDAPEST
027510     IF XESTERO 
      *ACQUO*                                                           fine
027520        MOVE 21 TO TIPO-REC OF RECORD-DA-SOCIETA      
027530        WRITE REC-WRITE.                                               
027600     MOVE 23 TO TIPO-REC OF RECORD-DA-SOCIETA.                            
027700*CODI*                                                                    
027800     IF XESTERO AND CONTO-CLI-WR = BUDAPEST                               
027810       MOVE "HUF " TO VALUTA-ACQ OF RECORD-21-22-23-24                    
027820                      VALUTA-VEND OF RECORD-21-22-23-24                   
027900       IF MOD-IMPUTAZ OF MOVMAG = 0                                       
028000          MOVE COSTO-STD OF MOVMAG TO                                     
028100                 COSTO-EFF-VALUTA OF RECORD-21-22-23-24                   
028200         ELSE                                             
      *           DISPLAY ' CAMBIO-DVS-COM-D' CAMBIO-DVS-COM-D                 
028300            COMPUTE COSTO-EFF-VALUTA OF RECORD-21-22-23-24                
028400                ROUNDED = (COSTO-STD OF MOVMAG *                          
      *BUDI
028500*                  MOD-IMPUTAZ OF MOVMAG) / 10000.                         
028500                  CAMBIO-DVS-COM-D) / 10000.                         
028600*EURO*                                                                    
028700     MOVE 2 TO NUM-DEC-PRZ OF RECORD-21-22-23-24.                         
028800     MOVE 0 TO NUM-DEC-CNTR-PRZ OF RECORD-21-22-23-24.    
      *ACQUO*                                                           inizio
      *BUDI
      *     IF XESTERO AND CONTO-CLI-WR NOT = BUDAPEST
           IF XESTERO 
027810       MOVE DIVISA-COM TO VALUTA-ACQ OF RECORD-21-22-23-24                    
027820                      VALUTA-VEND OF RECORD-21-22-23-24                   
027900       COMPUTE COSTO-COM               
028400                    = (COSTO-STD OF MOVMAG *                          
028500                  CAMBIO-DVS-COM-D) 
             PERFORM AGGIUSTA-NRO-DEC THRU EX-AGGIUSTA-NRO-DEC
027900       MOVE COSTO-COM TO COSTO-EFF-VALUTA OF RECORD-21-22-23-24       
028800       MOVE NRO-DEC-COM-D
                    TO NUM-DEC-CNTR-PRZ OF RECORD-21-22-23-24
           END-IF.
      *ACQUO*                                                           fine
028900     WRITE REC-WRITE.                                                     
028901     IF XMAXIMA                                                           
028902        MOVE SPACES TO RECORD-DA-SOCIETA                                  
028903        MOVE DEST-WR TO DEST-REC OF RECORD-15-16                          
028904        MOVE 15 TO TIPO-REC OF                                            
028905             RECORD-DA-SOCIETA                                            
028906        COMPUTE C-MAT OF RECORD-15-16 =                                   
028907                 C-MAT-TRANS-RID / 1000 * 1000                            
028908        MOVE SOCIETA-COMODO TO SOCIETA OF RECORD-15-16                    
028909        MOVE 1 TO LISTINO OF RECORD-15-16                                 
028910*EURO*                                                                    
028911        MOVE EU-DIVISA-CORR TO VALUTA OF RECORD-15-16                     
028912        COMPUTE PREZZO-LIRE OF RECORD-15-16 =                             
028913           COSTO-STD OF MOVMAG                                            
028914        MOVE 0 TO PREZZO-VALUTA OF RECORD-15-16                           
028915*EURO*                                                                    
028916                  NUM-DEC-CNTR-PRZ OF RECORD-15-16                        
028917        MOVE 2 TO NUM-DEC-PRZ OF RECORD-15-16                             
028918        WRITE REC-WRITE.                                                  
029000*CODI*                                                                    
029100*    IF XESTERO OR (XRESIDUO                                              
029200*           AND CAPO-PROD-DIFF-TESS OF REC-ANAMAT)                        
029300*       PERFORM CERCA-PREZZIA THRU EX-CERCA-PREZZIA.       
029400 EX-TRATTA-IMAGE.                                                         
029500     EXIT.                                                                
029600*                                                                         
029700*                                                                         
029800 SCRIVI-COMPOS.                                                           
029900     MOVE SPACES TO RECORD-DA-SOCIETA.                                    
030000     MOVE DEST-WR TO DEST-REC OF RECORD-13.                               
030100     MOVE 13 TO TIPO-REC OF RECORD-DA-SOCIETA                             
030200     MOVE SOCIETA-COMODO TO SOCIETA OF RECORD-13.                         
030300     COMPUTE C-MAT OF RECORD-13 =                                         
030400                C-MAT-TRANS-RID / 1000 * 1000.                            
030500     MOVE "COMPOS;" TO W-NOME-DATA-SET                                    
030600     MOVE "C-MAT;" TO W-NOME-CAMPO                                        
030700     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.              
030800     PERFORM TTDBFIND THRU EX-TTDBFIND                                    
030900     IF W-OK-IMAGE                                                        
031000        MOVE 5 TO W-MODO                                                  
031100        PERFORM TTDBGET THRU EX-TTDBGET.                                  
031200     IF W-OK-IMAGE                                                        
031300        PERFORM METTI-COMP-T THRU EX-METTI-COMP-T                         
031400           VARYING W-INDICE-2 FROM 1 BY 1                                 
031500               UNTIL W-INDICE-2 > 6                                       
031600        MOVE "T" TO TIPO-TESSUTO OF RECORD-13                             
031700        MOVE 1 TO NUM-PEZZO OF RECORD-13                                  
031800                  PROGR OF RECORD-13                                      
031900        WRITE REC-WRITE                                                   
032000        PERFORM METTI-COMP-F THRU EX-METTI-COMP-F                         
032100           VARYING W-INDICE-2 FROM 1 BY 1                                 
032200               UNTIL W-INDICE-2 > 2                                       
032300        MOVE "F" TO TIPO-TESSUTO OF RECORD-13                             
032400        MOVE 1 TO NUM-PEZZO OF RECORD-13                                  
032500                  PROGR OF RECORD-13                                      
032600        WRITE REC-WRITE.                                                  
032700 EX-SCRIVI-COMPOS.                                                        
032800     EXIT.                                                                
032900*                                                                         
033000*                                                                         
033100 METTI-QTA-TAGLIA.                                                        
033200     COMPUTE DIECI-TG OF RECORD-21-22-23-24 (W-INDICE-7) =                
033300          QTA-TAGLIA OF MOVMAG (W-INDICE-7) * -1.                         
033400 EX-METTI-QTA-TAGLIA.                                                     
033500     EXIT.                                                                
033600*                                                                         
033700*                                                                         
033800*                                                                         
033900 METTI-COMP-T.                                                            
034000     MOVE PERC-COMPOS OF COMPOS-TESSUTO (W-INDICE-2)                      
034100              TO VAL-T (W-INDICE-2).                                      
034200     MOVE SIGLA-FIBRA OF COMPOS-TESSUTO (W-INDICE-2)                      
034300              TO SIG-T (W-INDICE-2).                                      
034400 EX-METTI-COMP-T.                                                         
034500     EXIT.                                                                
034600*                                                                         
034700*                                                                         
034800 METTI-COMP-F.                                                            
034900     MOVE PERC-COMPOS OF COMPOS-FODERA (W-INDICE-2)                       
035000               TO VAL-T (W-INDICE-2).                                     
035100     MOVE SIGLA-FIBRA OF COMPOS-FODERA (W-INDICE-2)                       
035200              TO SIG-T (W-INDICE-2).                                      
035300 EX-METTI-COMP-F.                                                         
035400     EXIT.                                                                
035500*                                                                         
035600*                                                                         
035700 CERCA-PREZZIA.                                                           
035800*                                                                         
035900     MOVE "PREZZIA;" TO W-NOME-DATA-SET.                                  
036000     MOVE "C-MAT;" TO W-NOME-CAMPO.                                       
036100     COMPUTE W-VALORE-CAMPO =                                             
036200                    C-MAT-TRANS-RID / 1000 * 1000.                        
036300     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
036400     IF W-OK-IMAGE                                                        
036500        MOVE 5 TO W-MODO                                                  
036600        PERFORM TTDBGET THRU EX-TTDBGET                                   
036700        PERFORM TTDBGET THRU EX-TTDBGET                                   
036800               UNTIL W-FINE-CATENA OR                                     
036900                  MERCATO OF REC-PREZZI = LISTINO-WR                      
037000        IF W-OK-IMAGE                                                     
037100           MOVE SPACES TO RECORD-DA-SOCIETA                               
037200           MOVE DEST-WR TO DEST-REC OF RECORD-15-16                       
037300           MOVE 15 TO TIPO-REC OF                                         
037400                RECORD-DA-SOCIETA                                         
037500           COMPUTE C-MAT OF RECORD-15-16 =                                
037600                    C-MAT-TRANS-RID / 1000 * 1000                         
037700           MOVE SOCIETA-COMODO TO SOCIETA OF RECORD-15-16                 
037800           MOVE LISTINO-WR TO LISTINO OF RECORD-15-16                     
037900           MOVE DIVISA OF REC-PREZZI TO VALUTA OF RECORD-15-16            
038000           MOVE 0 TO PREZZO-LIRE OF RECORD-15-16                          
038100                     PREZZO-VALUTA OF RECORD-15-16                        
039000           IF XESTERO                                                     
039100              COMPUTE PREZZO-VALUTA OF RECORD-15-16 =                     
039200                PREZZO-VENDITA OF REC-PREZZI (1) / 100                    
039300*CODI*                                                                    
039400              MOVE PREZZO-VENDITA OF REC-PREZZI(1)                        
039500                  TO PREZZO-VENDITA-PREZZIA                               
039600           END-IF                                                         
039700*EURO*                                                                    
039800           MOVE 0 TO NUM-DEC-PRZ OF RECORD-15-16                          
039900                     NUM-DEC-CNTR-PRZ OF RECORD-15-16                     
040000           WRITE REC-WRITE.                                               
040100 EX-CERCA-PREZZIA.                                                        
040200     EXIT.                                                                
040300*                                                                         
040400*                                                                         
040500 SCRIVI-ANAMAT.                                                           
040600     MOVE SPACES TO RECORD-DA-SOCIETA.                                    
040700     MOVE DEST-WR TO DEST-REC OF RECORD-11.                               
040800     MOVE 11 TO TIPO-REC OF RECORD-DA-SOCIETA.                            
040900     MOVE 0 TO COD-COMPOS OF RECORD-11                                    
041000               COD-DISEGNO OF RECORD-11.                                  
041100     MOVE CL-GR OF REC-ANAMAT TO CLASSE-R OF RECORD-11.                   
041200     MOVE COLLEZIONE OF REC-ANAMAT TO                                     
041300                    COLLEZIONE OF RECORD-11.                              
041400     MOVE 1 TO NUM-PEZZI OF RECORD-11.                                    
041500     COMPUTE STAGIONE OF RECORD-11 = STAGIONE OF REC-ANAMAT / 2.          
041600*2000*                                                                    
041700*    COMPUTE ANNO OF RECORD-11 = 1990 + ANNO OF REC-ANAMAT.               
041800     MOVE W-FORMATO-INTERNO TO AAMMGG-2000 OF P-2000.                     
041900     COPY P2000 REPLACING PARDAT-2000 BY P-2000.                          
042000     MOVE ANNO OF REC-ANAMAT TO NUM-2000 OF P-2000.                       
042100     COPY P2000U REPLACING PARDAT-2000 BY P-2000.                         
042200     MOVE AAAA-2000 OF P-2000 TO ANNO OF RECORD-11.                       
042300*                                                                         
042400     MOVE SOCIETA-COMODO TO SOCIETA OF RECORD-11.                         
042500     COMPUTE C-MAT OF RECORD-11 = C-MAT-TRANS-RID / 1000 * 1000.          
042600     MOVE D-MAT OF REC-ANAMAT TO NOME OF RECORD-11.                       
042700     MOVE PRIMA-TG OF REC-ANAMAT TO                                       
042800           PRIMA-TAGLIA OF RECORD-11.                                     
042900     MOVE ULTIMA-TG OF REC-ANAMAT TO                                      
043000           ULTIMA-TAGLIA OF RECORD-11.                                    
043100     MOVE 2 TO INCREMENTO-TAGLIA OF RECORD-11.                            
043200     COMPUTE ALIQ-IVA OF RECORD-11 =                                      
043300          ALIQ-IVA OF REC-ANAMAT  * 100.                                  
043310*MAXIMA*                                                                  
043400     IF CAPO-PROD-DIFF-TESS OF REC-ANAMAT                                 
043410                AND NOT XMAXIMA                                           
043500        MOVE "5" TO VALIDITA OF RECORD-11.                                
043600     WRITE REC-WRITE.                                                     
043700     IF XESTERO OR XMAXIMA                                                
043800        MOVE SPACES TO RECORD-DA-SOCIETA                                  
043900        MOVE DEST-WR TO DEST-REC OF RECORD-12                             
044000        MOVE SOCIETA-COMODO TO SOCIETA OF RECORD-12                       
044100        MOVE 12 TO TIPO-REC OF RECORD-12                                  
044200        COMPUTE C-MAT OF RECORD-12 =                                      
044300                C-MAT-TRANS-RID / 1000 * 1000                             
044400        MOVE COLORE OF C-MAT-TRANSITO TO                                  
044500                  VARIANTE OF RECORD-12                                   
044600                  COD-COLORE OF RECORD-12                                 
044700        WRITE REC-WRITE.                                                  
044800     IF XRESIDUO                                                          
044900        MOVE SPACES TO RECORD-DA-SOCIETA                                  
045000        MOVE DEST-WR TO DEST-REC OF RECORD-15-16                          
045100        MOVE 16 TO TIPO-REC OF                                            
045200             RECORD-DA-SOCIETA                                            
045300        COMPUTE C-MAT OF RECORD-15-16 =                                   
045400                 C-MAT-TRANS-RID / 1000 * 1000                            
045500        MOVE SOCIETA-COMODO TO SOCIETA OF RECORD-15-16                    
045510*EURO*                                                                    
045600        MOVE 2 TO LISTINO OF RECORD-15-16                                 
045700        MOVE EU-DIVISA-CORR TO VALUTA OF RECORD-15-16                     
045800        COMPUTE PREZZO-LIRE OF RECORD-15-16 =                             
045900           COSTO OF REC-ANAMAT                                            
046100*EURO*                                                                    
046200        MOVE 2 TO NUM-DEC-PRZ OF RECORD-15-16                             
046310        IF W-FORMATO-INTERNO < 020000                                     
046320           MOVE COSTO OF REC-ANAMAT TO IE-IMPORTO-IN                      
046330           PERFORM CHIAMA-PDAEU THRU EX-CHIAMA-PDAEU                      
046340           COMPUTE PREZZO-LIRE OF RECORD-15-16 =                          
046350                 IE-IMPORTO-OU / 100                                      
046351           MOVE IE-DIVISA-OU TO VALUTA OF RECORD-15-16                    
046360           MOVE 0 TO NUM-DEC-PRZ OF RECORD-15-16                          
046370        END-IF                                                            
046380        MOVE 0 TO PREZZO-VALUTA OF RECORD-15-16                           
046390        MOVE 0 TO NUM-DEC-CNTR-PRZ OF RECORD-15-16                        
046400        WRITE REC-WRITE.   
      *ACQUO*                                                           inizio
046500*    IF XESTERO                                                            
046600*        MOVE SPACES TO RECORD-DA-SOCIETA                                  
046700*        MOVE DEST-WR TO DEST-REC OF RECORD-15-16                          
046800*        MOVE 15 TO TIPO-REC OF                                            
046900*             RECORD-DA-SOCIETA                                            
047000*        COMPUTE C-MAT OF RECORD-15-16 =                                   
047100*                 C-MAT-TRANS-RID / 1000 * 1000                            
047200*        MOVE SOCIETA-COMODO TO SOCIETA OF RECORD-15-16                    
047300*        MOVE 1 TO LISTINO OF RECORD-15-16                                 
047310**EURO*                                                                    
047400*        MOVE EU-DIVISA-CORR TO VALUTA OF RECORD-15-16                     
047500*        COMPUTE PREZZO-LIRE OF RECORD-15-16 =                             
047600*           COSTO OF REC-ANAMAT                                            
047700*        MOVE 0 TO PREZZO-VALUTA OF RECORD-15-16                           
047800**EURO*                                                                    
048000*                  NUM-DEC-CNTR-PRZ OF RECORD-15-16                        
048010*        MOVE 2 TO NUM-DEC-PRZ OF RECORD-15-16                             
048100*        WRITE REC-WRITE.                                                
      *ACQUO*                                                           fine
048200 EX-SCRIVI-ANAMAT.                                                        
048300     EXIT.                                                                
048400*                                                                         
048500*                                                                         
048600 CERCA-PREZZI.                                                            
048700     MOVE "PREZZI;" TO W-NOME-DATA-SET.                                   
048800     MOVE "C-MAT;" TO W-NOME-CAMPO.                                       
048900     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.              
049000     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
049100     IF W-OK-IMAGE                                                        
049200        MOVE 5 TO W-MODO                                                  
049300        PERFORM TTDBGET THRU EX-TTDBGET                                   
049400        PERFORM TTDBGET THRU EX-TTDBGET                                   
049500               UNTIL W-FINE-CATENA OR                                     
049600                  MERCATO OF REC-PREZZI =                                 
049700                  LISTINO-WR                                              
049800        IF W-OK-IMAGE                                                     
049900           MOVE SPACES TO RECORD-DA-SOCIETA                               
050000           MOVE DEST-WR TO DEST-REC OF RECORD-15-16                       
050100           MOVE 16 TO TIPO-REC OF                                         
050200                RECORD-DA-SOCIETA                                         
050300           COMPUTE C-MAT OF RECORD-15-16 =                                
050400                    C-MAT-TRANS-RID / 1000 * 1000                         
050500           MOVE SOCIETA-COMODO TO SOCIETA OF RECORD-15-16                 
050600           MOVE LISTINO-WR TO                                             
050700                   LISTINO OF RECORD-15-16                                
050701*MAXIMA*                                                                  
050710           IF XMAXIMA                                                     
050720              MOVE 44 TO LISTINO OF RECORD-15-16                          
050730           END-IF                                                         
050800           MOVE DIVISA OF REC-PREZZI TO VALUTA OF RECORD-15-16            
050810           MOVE 0 TO NUM-DEC-CNTR-PRZ OF RECORD-15-16                     
050820                     NUM-DEC-PRZ OF RECORD-15-16                          
050900           IF XESTERO                                                     
051000              COMPUTE PREZZO-VALUTA OF RECORD-15-16 =                     
051100                  PREZZO-VENDITA OF REC-PREZZI (1) / 100                  
051200              MOVE 0 TO PREZZO-LIRE OF RECORD-15-16                       
051300             ELSE                                                         
051310                IF W-FORMATO-INTERNO < 020000                             
051400                   COMPUTE PREZZO-LIRE OF RECORD-15-16 =                  
051500                     PREZZO-VENDITA OF REC-PREZZI (1) / 100               
051600                   MOVE 0 TO PREZZO-VALUTA OF RECORD-15-16                
051620                  ELSE                                                    
051630                      MOVE PREZZO-VENDITA OF REC-PREZZI (1)               
051640                        TO PREZZO-LIRE OF RECORD-15-16                    
051650                      MOVE 2 TO NUM-DEC-PRZ OF RECORD-15-16               
051660                END-IF                                                    
051700           END-IF         
      *ACQUO*                                                           inizio
                 IF NOT (XESTERO AND CONTO-CLI-WR NOT = BUDAPEST)
      *ACQUO*                                                           fine
052100           WRITE REC-WRITE.                                               
052200 EX-CERCA-PREZZI.                                                         
052300     EXIT.                                                                
052400*                                                                         
052500*                                                                         
052600*                                                                         
052700 TTDBGET.                                                                 
052800     COPY PDBGET.                                                         
052900*                                                                         
053000*                                                                         
053100 TTDBFIND.                                                                
053200     COPY PDBFIND.                                                        
053300*                                                                         
053400*                                                                         
053500 CHIAMA-PDAEU.                                                            
053600     COPY PDAEU.                                                          
053700 EX-CHIAMA-PDAEU.                                                         
053800     EXIT.  
      *
      *
      *ACQUO*                                                           inizio
       VEDI-OUT-EST.  
013400       PERFORM S-CONNECT     THRU S-CONNECT-EX                       
013500       IF SQLCODE NOT = OK                                               
013600           DISPLAY "WRITERES: Errore CONNECT SQL SINIR "   
                    UPON SYSERR
013700           STOP RUN                                                     
013800       END-IF.     
      *
            MOVE MOD-IMPUTAZ OF MOVMAG TO CONTE-CONTO-CONTABILE                                        
            PERFORM WITH TEST AFTER UNTIL NOT DEAD-NOMEM                       
                PERFORM BEGIN-RC                                                
                       THRU BEGIN-RC-EX                                         
                IF SQLCODE = OK                                                 
                  PERFORM SELECT-CONTI-ESTERO
                        THRU SELECT-CONTI-ESTERO-EX
                  IF SQLCODE NOT = OK
019120              DISPLAY "WRITERES "
                       RIF-INTR-WR "   " MAGAZZINO-WR
                        ":  manca CONTI_ESTERO x "
                        CONTE-CONTO-CONTABILE
                        UPON SYSERR                  
                    MOVE -1 TO TRATTATO
                  END-IF
                END-IF                                                          
            END-PERFORM                                                        
            PERFORM S-COMMIT THRU S-COMMIT-EX     
      *     
026520      EXEC SQL RELEASE END-EXEC .      
       EX-VEDI-OUT-EST.
           EXIT.
           
124000 SELECT-CONTI-ESTERO.   
124100     EXEC SQL                                                             
124200        SELECT CONTO_RIPARTIZ                                        
124400             INTO   :CONTE-CONTO-RIPARTIZ                                      
128600               FROM CONTI_ESTERO               
128700               WHERE CONTO_CONTABILE = :CONTE-CONTO-CONTABILE
128900     END-EXEC                                                             
129000     MOVE "SELECT CONTI-ESTERO" TO ER-DESCRIZIONE                         
129100     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
129200 SELECT-CONTI-ESTERO-EX.                                                          
129300     EXIT.                  
           
031100 S-CONNECT.                                                               
031200     EXEC SQL                                                             
031300        CONNECT TO 'SINIR.PUB'                                        
031400     END-EXEC                                                             
031500     MOVE "Connect DBE SINIR" TO ER-DESCRIZIONE                           
031600     PERFORM TEST-ERR         THRU TEST-ERR-EX.                           
031700 S-CONNECT-EX.                                                            
031800     EXIT.   

032300 S-COMMIT.                                                                  
032400     EXEC SQL                                                             
032500        COMMIT WORK                                                       
032600     END-EXEC.                                                            
032700     MOVE "COMMIT WORK" TO ER-DESCRIZIONE                                 
032800     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
032900 S-COMMIT-EX.                                                               
033000     EXIT.                                                                
033100                                                                       
033300 BEGIN-RC.                                                                
033400     EXEC SQL                                                             
033500        BEGIN WORK RC                                                     
033600     END-EXEC                                                             
033700     MOVE "BEGIN WORK RC" TO ER-DESCRIZIONE                               
033800     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
033900 BEGIN-RC-EX.                                                             
034000     EXIT.        

045800 TEST-ERR.                                                                
045900     MOVE SQLCODE TO SQL-STATUS.                                          
046000     IF SQLCODE = OK OR NO-MEMORY OR DEADLOCK OR NOT-FOUND                
046100        CONTINUE                                                          
046200     ELSE                                                                 
           CANCEL "CALLSQLE"
046300        CALL "CALLSQLE" USING SQLCA PAR-ERR AREA-HL AREA-SI.              
046400 TEST-ERR-EX.                                                             
046500     EXIT.      
      *ACQUO*                                                           fine      
      
      *ACQUO*                                                           inizio
       VEDI-INDICI-NEG.  
013400       PERFORM S-CONNECT-D    THRU S-CONNECT-D-EX                       
013500       IF SQLCODE NOT = OK                                               
013600           DISPLAY "WRITERES: Errore CONNECT SQL DATGE "   
                    UPON SYSERR
013700           STOP RUN                                                     
013800       END-IF.     
      *
030900      MOVE CONTE-CONTO-RIPARTIZ TO INDNEG-NEGOZIO.                                            
            PERFORM WITH TEST AFTER UNTIL NOT DEAD-NOMEM                       
                PERFORM BEGIN-RC                                                
                       THRU BEGIN-RC-EX                                         
                IF SQLCODE = OK                                                 
                  PERFORM SE-SELECT-INDNEG                                       
031600                  THRU SE-SELECT-INDNEG-EX      
                  IF SQLCODE NOT = OK
019120              DISPLAY "WRITERES "
                       RIF-INTR-WR "   " MAGAZZINO-WR
                        ":  negozio inesistente "
                        INDNEG-NEGOZIO
                      UPON SYSERR                  
                    MOVE -1 TO TRATTATO
                  END-IF
                END-IF                                                          
            END-PERFORM                                                        
            PERFORM S-COMMIT THRU S-COMMIT-EX     
      *     
026520      EXEC SQL RELEASE END-EXEC .    
       EX-VEDI-INDICI-NEG.
           EXIT.
           
124000 SE-SELECT-INDNEG.   
124100     EXEC SQL                                                             
124200        SELECT PROX_FILE_E                                        
124400             INTO   :INDNEG-PROX-FILE-E                                    
128600               FROM SERV.INDICI_NEG              
128700               WHERE NEGOZIO       = :INDNEG-NEGOZIO 
128900     END-EXEC                                                             
129000     MOVE "SELECT SERV.INDICI_NEG" TO ER-DESCRIZIONE                         
129100     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
129200 SE-SELECT-INDNEG-EX.                                                          
129300     EXIT.   

031100 S-CONNECT-D.                                                               
031200     EXEC SQL                                                             
031300        CONNECT TO 'DATGE.DBE.TRANSFER'                                        
031400     END-EXEC                                                             
031500     MOVE "Connect DBE DATGE" TO ER-DESCRIZIONE                           
031600     PERFORM TEST-ERR         THRU TEST-ERR-EX.                           
031700 S-CONNECT-D-EX.                                                            
031800     EXIT.   
           
      *ACQUO*                                                           fine            

      *ACQUO*                                                           inizio
       APRI-DANEG.  
055400     MOVE INDNEG-NEGOZIO TO NEG-DANEG.                                    
055500     MOVE INDNEG-PROX-FILE-E TO PROGR-DANEG.        
           IF W-ACCOUNT = "PROROSA"   
             MOVE SPACES TO DANEG-NOMEFILE                                  
             STRING                                
                  "DANEG/E" NEG-DANEG PROGR-DANEG 
             DELIMITED BY SIZE                                   
             INTO DANEG-NOMEFILE.   
           IF W-ACCOUNT NOT = "PROROSA" 
             MOVE SPACES TO DANEG-NOMEFILE                                  
             STRING                                
                  "/home/tr/DANEG/E" NEG-DANEG PROGR-DANEG 
             DELIMITED BY SIZE                                   
             INTO DANEG-NOMEFILE.                         
      *   
           OPEN EXTEND FILE-DANEG WITH LOCK.
           IF NOT M-F-APERTO
              DISPLAY "**WRITERES: in attesa OPEN esclusiva di DANEG"
                      " - file M-F =" M-F
                    UPON SYSERR
           END-IF.
           PERFORM UNTIL M-F-APERTO
              MOVE 5 TO N-SEC-SLEEP
              CALL "SYSTEM" USING COMANDO-SLEEP-LINUX
              OPEN EXTEND FILE-DANEG WITH LOCK
           END-PERFORM.      
      *     
      *    DISPLAY SPACE  UPON SYSERR
      *    DISPLAY "aperto " DANEG-NOMEFILE UPON SYSERR
      *    DISPLAY SPACE  UPON SYSERR .
       EX-APRI-DANEG.
           EXIT.
      *ACQUO*                                                           fine            
      
      *ACQUO*                                                           inizio
       CARICA-MERCFR.                                               
           MOVE "ANACON" TO W-NOME-DATA-SET.
           MOVE 7 TO W-MODO
           MOVE MOD-IMPUTAZ OF MOVMAG TO W-VALORE-CAMPO-W
           PERFORM TTDBGET-A THRU EX-TTDBGET-A.
           IF NOT W-OK-IMAGE
019120       DISPLAY "WRITERES "
                       RIF-INTR-WR "   " MAGAZZINO-WR
                ":  manca ANACON " W-VALORE-CAMPO-W
                        UPON SYSERR                             
             MOVE -1 TO TRATTATO
             GO TO EX-CARICA-MERCFR.
073800*                                                                         
073900     MOVE 16 TO QD-LL-A OF PARDEED QD-LL-B OF PARDEED.                    
074000     MOVE 0 TO QD-NR-DEC OF PARDEED.                                      
074010*   
      * ottengo la directory di lavoro dalla variabile                          
      * d'ambiente RETIS_DIRECTORY                                              
           MOVE "RETIS_DIRECTORY" TO WK-VAR-NAME.                               
           DISPLAY WK-VAR-NAME  UPON ENVIRONMENT-NAME.                          
           ACCEPT WK-VAR-VALUE  FROM ENVIRONMENT-VALUE.                         
      *                                                                         
           MOVE SPACES TO MERCFR-NOMEFILE.                                     
           STRING WK-VAR-VALUE DELIMITED BY SPACE                               
                  "TABELLE/MERCFR" DELIMITED BY SIZE                                   
             INTO MERCFR-NOMEFILE.   
      *
074800     OPEN INPUT FILE-MERCFR.                                              
074900     MOVE 0 TO FINITO.                                                    
075000     PERFORM LEGGI-FILE-MERCFR THRU EX-LEGGI-FILE-MERCFR.                 
075100     PERFORM TRATTA-FILE-MERCFR THRU EX-TRATTA-FILE-MERCFR                
075200             UNTIL FINE-FILE OR TROVATO-RECORD .                                             
075300     CLOSE FILE-MERCFR.    
      *
           IF FINE-FILE      
019120       DISPLAY "WRITERES "
                       RIF-INTR-WR "   " MAGAZZINO-WR
                ": mercato non trovato su file MERCFR " 
                FIDO OF REC-ANACON
                        UPON SYSERR                             
             MOVE -1 TO TRATTATO
           END-IF.
075310*                                                
076500 EX-CARICA-MERCFR.                                                        
076600     EXIT.                                                                
                                                                       
076900 LEGGI-FILE-MERCFR.                                                       
077000     READ FILE-MERCFR                                                     
077100         AT END MOVE 1 TO FINITO.                                         
077200 EX-LEGGI-FILE-MERCFR.                                                    
077300     EXIT.                                                                
                                                                         
077600 TRATTA-FILE-MERCFR.                                                      
078800     MOVE SPACE TO REC-MERCFR-COM.                                        
078900     UNSTRING REC-MERCFR DELIMITED BY T-TAB                               
079000                        INTO MERCATO-COM OF REC-MERCFR-COM                
079100                             LINGUA-COM OF REC-MERCFR-COM                 
079200                             SIGLA-NAZIONE-COM OF REC-MERCFR-COM          
079300                             DIVISA-COM OF REC-MERCFR-COM                 
079400                             NRO-DEC-COM OF REC-MERCFR-COM                
079600                             CAMBIO-DVS-COM OF REC-MERCFR-COM.            
079700*                                                                         
079710     MOVE MERCATO-COM TO DEP-DEED-8.                                      
           CANCEL "QDEEDIT"
079720     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
079730     IF QD-STATO OF PARDEED NOT = 0                                       
079740       DISPLAY "WRITERES: merc. non numerico su file MERCFR "   
                 MERCATO-COM 
                    UPON SYSERR           
079750       PERFORM LEGGI-FILE-MERCFR THRU EX-LEGGI-FILE-MERCFR
             GO TO EX-TRATTA-FILE-MERCFR
           ELSE
079751       MOVE DEP-DEED-8-9 TO MERCATO-COM-D.                                  
079760*      
080210     IF MERCATO-COM-D NOT = FIDO OF REC-ANACON
079750       PERFORM LEGGI-FILE-MERCFR THRU EX-LEGGI-FILE-MERCFR
             GO TO EX-TRATTA-FILE-MERCFR
           ELSE
             MOVE 2 TO FINITO.
      *
079770     MOVE NRO-DEC-COM TO DEP-DEED-8.                                      
           CANCEL "QDEEDIT"
079780     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
079790     IF QD-STATO OF PARDEED NOT = 0
019120       DISPLAY "WRITERES "
                       RIF-INTR-WR "   " MAGAZZINO-WR
                ": nro dec. non numerico su file MERCFR "
                     NRO-DEC-COM
                        UPON SYSERR                             
             MOVE -1 TO TRATTATO
           ELSE
079820       MOVE DEP-DEED-8-9 TO NRO-DEC-COM-D.                                  
079830*                                                                         
079840     MOVE CAMBIO-DVS-COM TO DEP-DEED-8.                                   
           CANCEL "QDEEDIT"
079850     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
079860     IF QD-STATO OF PARDEED NOT = 0    
019120       DISPLAY "WRITERES "
                       RIF-INTR-WR "   " MAGAZZINO-WR
                ": cambio non numerico su file MERCFR "
                     CAMBIO-DVS-COM
                        UPON SYSERR                             
             MOVE -1 TO TRATTATO
           ELSE
079890       MOVE DEP-DEED-8-9 TO CAMBIO-DVS-COM-D.                               
079900*   
082300 EX-TRATTA-FILE-MERCFR.                                                   
082400     EXIT.      

       TTDBGET-A.   COPY PDBGET REPLACING
                  AREA-REC-SET BY REC-ANACON
                  EX-TTDBGET BY EX-TTDBGET-A.
      *ACQUO*                                                           fine     
      
      *ACQUO*                                                           inizio      
090900 AGGIUSTA-NRO-DEC.              
091000     COMPUTE DIFF-DEC = NRO-DEC-COM-D - 8.        
      *        6 decim nel cambio + 2 decim in COSTO-STD di MOVMAG
091100     IF DIFF-DEC > 0                                                      
091200       PERFORM VARYING I FROM 1 BY 1                                      
091300               UNTIL I > DIFF-DEC                                         
091400         COMPUTE COSTO-COM =                              
091410              COSTO-COM * 10                                
091500       END-PERFORM                                                        
091600     ELSE                                                                 
091700       PERFORM VARYING I FROM -1 BY -1                                    
091800               UNTIL I < DIFF-DEC                                         
091900         COMPUTE COSTO-COM ROUNDED =                      
092000               COSTO-COM / 10                              
092100       END-PERFORM                                                        
092200     END-IF.                                                              
092300 EX-AGGIUSTA-NRO-DEC.                                                     
092400     EXIT.    
      *ACQUO*                                                           fine  
      
      *ACQUO*                                                           inizio
       SCRIVI-DANEG.
           MOVE ALL "*" TO REC-PET-A-SEDE
           MOVE INDNEG-NEGOZIO TO COD-NEG OF REC-PET-A-SEDE
           MOVE 01 TO TIPO-REC OF REC-SITCAR-PET-A-SEDE
           MOVE 0 TO FLAG-P OF REC-SITCAR-PET-A-SEDE
           MOVE 0 TO C-MAT OF REC-SITCAR-PET-A-SEDE
063600     MOVE 0                  
063700           TO NUM-TG     OF REC-SITCAR-PET-A-SEDE           
           MOVE 0 TO DIFF-CAR-REALE-TEOR
           MOVE 0 TO FLAG-CAR
           MOVE NUMERO-WR TO NUM-BOLLA OF REC-SITCAR-PET-A-SEDE
           MOVE DATA-WR TO DATA-BOLLA OF REC-SITCAR-PET-A-SEDE
           MOVE 0 TO FLAG-X-AGGIORNAMENTO OF REC-SITCAR-PET-A-SEDE
           MOVE 05 TO SOCIETA-CARICO OF REC-SITCAR-PET-A-SEDE   
           IF DATA-CARICO-WR = 0
              MOVE DATA-WR TO DATA-COM
           ELSE
              MOVE DATA-CARICO-WR TO DATA-COM
           END-IF.
           MOVE GG-COM TO GIORNO OF DATA-ARRIVO-BOLLA-R 
                           OF REC-SITCAR-PET-A-SEDE
           MOVE MM-COM TO MESE OF DATA-ARRIVO-BOLLA-R 
                          OF REC-SITCAR-PET-A-SEDE
           MOVE AA-COM TO ANNO OF DATA-ARRIVO-BOLLA-R 
                          OF REC-SITCAR-PET-A-SEDE
      *
           WRITE REC-MOV01.
063800*                                                                      
       EX-SCRIVI-DANEG.
           EXIT.
      *ACQUO*                                                           fine         
053900*                                                                         
054000*                                                                         
054100**********************************************************                
