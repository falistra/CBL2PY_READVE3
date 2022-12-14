001000*CONTROL USLINIT, SUBPROGRAM                                              
001100*SET X5=OFF                                                               
001200 IDENTIFICATION DIVISION.                                                 
001300 PROGRAM-ID. DTVALSTK.                                                    
001400*                                                                         
001500*   CALCOLO VALORIZZAZIONE VENDITE STOCK DA RESIDUO                       
001600*                                                                         
001610*2000*        05/08/99                                                    
001620*     tratta date a 6 cifre                                               
001630*                                                                         
001640*EURO*        15/01/01                                                    
001650*     trattamento importi in EURO                                         
001660*     Da rivedere entro il 2001 in quanto le tabelle sono                 
001670*     valorizzate in Lit.                                                 
001700*                                                                         
001710*EURO1*       03/12/01                                                    
001720*      trattamento prezzi di vendita in Euro                              
001730*                                                                         
001740*10099051*    13/06/02                                                    
001750*      Se non trova lo sconto in tabella passa il cambio                  
001760*      x BUDA = a quello che trova nella prima riga di tabella            
001770*      e non a Zero come prima                                            
001780*                                                                         
001800 ENVIRONMENT DIVISION.                                                    
001900 CONFIGURATION SECTION.                                                   
002000 SOURCE-COMPUTER. HP-3000.                                                
002100 OBJECT-COMPUTER. HP-3000.                                                
002200 SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.            
002300*                                                                         
002400 INPUT-OUTPUT SECTION.                                                    
002500 FILE-CONTROL.                                                            
002600     SELECT OPTIONAL FILE-TAB ASSIGN TO "TABSTK"                          
            ORGANIZATION IS LINE SEQUENTIAL.
002700*                                                                         
002800 DATA DIVISION.                                                           
002900*                                                                         
003000*                                                                         
003100*                                                                         
003200 FILE SECTION.                                                            
003300*                                                                         
003400*                                                                         
003500*                                                                         
003600 FD FILE-TAB    DATA RECORD REC-TAB.                                      
003700 01 REC-TAB    PIC X(80).                                                 
004500*                                                                         
004600*                                                                         
004700*PAGE                                                                     
004800*                                                                         
004900 WORKING-STORAGE SECTION.                                                 
005000 77 ERR-DISP             PIC -(6).                                        
005100 77 STATO-DISP           PIC Z(15).                                       
005200 77 STATO-DISPLAY        PIC ZZZZ-.                                       
005300 77 CONT                 PIC 9(6).                                        
005400 77 CONT-MODELLO         PIC 9(4) COMP.                                   
005500 77 IND                    PIC S9(4) COMP.                                
005600*                                                                         
005700*                                                                         
005800 01 FINITO    PIC 9(4) COMP.                                              
005900  88 FINE-FILE   VALUE 1.                                                 
006000*                                                                         
006100*PAGE                                                                     
006200*                                                                         
006300*                                                                         
006310 01 REC-TAB-COM.                                                          
006320   10 ANNO-TB            PIC X.                                           
006330   10 STAGIONE-TB        PIC X.                                           
006340   10 FORN-TB            PIC X.                                           
006350   10 COLL-TB            PIC XX.                                          
006360   10 CLASSE-TB          PIC XX.                                          
006370   10 PREZZO-TB          PIC X(9).                                        
006410   10 P-SCONTO-TB      PIC X(5).                                          
006420   10 C-MAT-TB           PIC X(15).                                       
006430   10 CAMBIO-TB          PIC X(6).                                        
006700*                                                                         
006710*                                                                         
006720 01 T-TAB    PIC X VALUE X"9".                                             
006730*                                                                         
006800*                                                                         
006900*                                                                         
007000 01 QTA-TG-COM    PIC S9(9) COMP.                                         
007100 01 VAL-COM       PIC S9(18) COMP.                                        
007200 01 QTA-COM       PIC S9(18) COMP.                                        
007300 01 PRZ-SCO-COM         PIC S9(18) COMP.                                  
007400*                                                                         
007500*                                                                         
007600 01 PERCENTO    PIC S9999V9999 COMP.                                      
007700*                                                                         
007800*                                                                         
007900 01 C-MAT-TRANS COPY DANCODMT.                                            
008000*                                                                         
008100*                                                                         
008200*                                                                         
008300*                                                                         
008400*                                                                         
008500*                                                                         
008600*                                                                         
008700*                                                                         
008800*                                                                         
008900 01 COMANDO-FILE-T.                                                       
009000   05 FILLER PIC X(45).                                                   
009100 01 CARRIAGE-RETURN PIC X VALUE X"13".                                      
009200*                                                                         
009300 01 ERR   PIC S9999  COMP VALUE 0.                                        
009400 01 ERR-PARM  PIC S9999 COMP VALUE 0.                                     
009500*                                                                         
009600*                                                                         
009700*                                                                         
009800*                                                                         
009900*                                                                         
010000*                                                                         
010100*                                                                         
010200 01 DEP-DEED-8         PIC X(16).                                         
010300 01 DEP-DEED-8-9       PIC 9(16).                                         
010400*                                                                         
010500*                                                                         
010600 01 PARDEED   COPY QPARDEED.                                              
010700*                                                                         
010800*                                                                         
010900 01 TAB-SCO.                                                              
011000   05 EL-SCO    OCCURS 99999.                                              
011100     10 EL-ANNO           PIC 9.                                          
011200     10 EL-STAGIONE       PIC 9.                                          
011210     10 EL-FORN           PIC 9.                                          
011300     10 EL-COLL           PIC 99.                                         
011400     10 EL-CLASSE         PIC 99.                                         
011500     10 EL-C-MAT          PIC 9(15) COMP-3.                               
011600     10 EL-PREZZO         PIC 9(9) COMP.                                  
011700     10 EL-P-SCONTO       PIC 9(5) COMP.                                  
011710     10 EL-CAMBIO         PIC 9(9) COMP.                                  
011800*                                                                         
011900 01 COM-SCO.                                                              
012000     10 EL-ANNO           PIC 9.                                          
012100     10 EL-STAGIONE       PIC 9.                                          
012110     10 EL-FORN           PIC 9.                                          
012200     10 EL-COLL           PIC 99.                                         
012300     10 EL-CLASSE         PIC 99.                                         
012400     10 EL-C-MAT          PIC 9(15) COMP-3.                               
012500     10 EL-PREZZO         PIC 9(9) COMP.                                  
012600     10 EL-P-SCONTO       PIC 9(5) COMP.                                  
012610     10 EL-CAMBIO         PIC 9(9) COMP.                                  
012700*                                                                         
012800 01 NUM-ELEM-MAX-SC   PIC S9(9) COMP VALUE 99999.                          
012900*                                                                         
013000*                                                                         
013100 01 PARTAB-SC    COPY QPARTABX.                                            
013200*                                                                         
013300 01 PRZ-MAT-MEM       PIC S9(9) COMP.                                     
013400*                                                                         
013410*EURO*                                                                    
013420 01 PAR-INEU     COPY QPARINEU.                                           
013430* 
       01  PY-INPUT-REC.
           05  INPUT-VAL-CMAT            PIC X(15).
           05  INPUT-VAL-CONTO           PIC X(8).
           05  INPUT-VAL-TIPO-PREZZO     PIC X(1).
       01  PY-OUTPUT-REC.
           05  OUTPUT-VAL-RET            PIC X(2).
           05  OUTPUT-VAL-PREZZO         PIC X(9).
           05  OUTPUT-VAL-CAMBIO         PIC X(9).
           05  OUTPUT-VAL-MESS           PIC X(35).
           05  OUTPUT-VAL-ERR            PIC X(35).
013500*                                                                         
013600 LINKAGE SECTION.                                                         
013700*                                                                         
013800*                                                                         
013900*                                                                         
014000 01 VC-NOME       PIC X(30).                                              
014100 01 VC-C-MAT      PIC 9(15) COMP-3.                                       
014200 01 VC-SCO        PIC 9(5) COMP.                                          
014400 01 VC-STAGIONE     PIC 9.                                                
014800 01 VC-COLL    PIC 99.                                                    
014900 01 VC-PRZ-SCO    PIC 9(9) COMP.                                          
015000 01 VC-PRZ-LORDO  PIC 9(9) COMP.                                          
015100 01 VC-MSG.                                                               
015200   05 VC-MSG-1          PIC X(30).                                        
015300   05 VC-MSG-2          PIC 9(15).                                        
015400 01 VC-PRIMA-VOLTA   PIC S9(4) COMP.                                      
015500  88 PRIMA-VOLTA VALUE 0.                                                 
015510 01 VC-CAMBIO        PIC 9(9) COMP.                                       
015600*                                                                         
015610*EURO1*                                                                   
015620 01 W-COMMON COPY WCOMMONW.                                               
015630*                                                                         
015700*                                                                         
015800*PAGE                                                                     
015900*                                                                         
016000*                                                                         
016100 PROCEDURE DIVISION USING VC-NOME                                         
016200                          VC-C-MAT                                        
016300                          VC-STAGIONE                                     
016500                          VC-COLL                                         
016600                          VC-SCO                                          
016800                          VC-PRZ-SCO                                      
016900                          VC-PRZ-LORDO VC-MSG                             
016910                          VC-PRIMA-VOLTA                                  
016920                          VC-CAMBIO                                       
016930*EURO1*                                                                   
016940                          W-COMMON.                                       
016950*                                                                         
017000*                                                                         
017100 MAINSEC SECTION 01.                                                      
017200*                                                                         
017300 INIZIO.                                                                  
      *CALLPY
017400*   IF PRIMA-VOLTA                                                       
017500*     PERFORM CARICA-SCONTI THRU EX-CARICA-SCONTI                        
017600*     GO TO FINE.                                                        
017700*                                                                         
017800*                                                                         
017900*   PERFORM CALCOLA-SCONTO THRU EX-CALCOLA-SCONTO.                    
018000*
           PERFORM CHIAMA-GET-PREZZO THRU EX-CHIAMA-GET-PREZZO.                                                                         
018100*CALLPY                                                                         
018200 FINE.                                                                    
018300     EXIT PROGRAM.                                                        
018400*CALLPY
       CHIAMA-GET-PREZZO.
      *
           MOVE VC-NOME(3:1)  TO INPUT-VAL-TIPO-PREZZO.
           STRING '100',
                 VC-NOME(4:5)  INTO INPUT-VAL-CONTO. 
           MOVE VC-C-MAT TO INPUT-VAL-CMAT.
           DISPLAY PY-INPUT-REC
           CANCEL "PYTHON"
           CALL "PYTHON" USING "get_prezzo_stock" "prezzo_stock" 
                               PY-INPUT-REC
                               PY-OUTPUT-REC.

           MOVE OUTPUT-VAL-CAMBIO TO VC-CAMBIO.
           MOVE OUTPUT-VAL-PREZZO TO VC-PRZ-SCO.
      *
       EX-CHIAMA-GET-PREZZO.
          EXIT.                                                              
018500*CALLPY                                                                         
018600 CARICA-SCONTI.                                                           
018700     MOVE SPACES TO COMANDO-FILE-T.                                       
018800     STRING "FILE TABSTK=" DELIMITED BY SIZE                              
018900            VC-NOME DELIMITED BY SPACE                                    
019000            CARRIAGE-RETURN DELIMITED BY SIZE                             
019100         INTO COMANDO-FILE-T.                                             
           CANCEL "COMMAND2"
019200     CALL "COMMAND2" USING COMANDO-FILE-T,                       
019300                                    ERR, ERR-PARM.                        
019400     IF ERR NOT = 0                                                       
019500        MOVE ERR TO ERR-DISP                                              
019600        DISPLAY "DTVALSTK - Err File Eq. TABSTK = "                       
019700               VC-NOME SPACE ERR-DISP                                     
019800        STOP RUN.                                                         
019900*                                                                         
020000     MOVE 16 TO QD-LL-A QD-LL-B.                                          
020100     MOVE 0 TO QD-NR-DEC                                                  
020200     MOVE 0 TO QT-NUM-ELEM-EFF OF PARTAB-SC                               
020300               QT-INDEX-ELEM OF PARTAB-SC.                                
020400     MOVE NUM-ELEM-MAX-SC TO QT-NUM-ELEM-MAX OF PARTAB-SC.                
020500     MOVE 27 TO QT-LL-ELEM OF PARTAB-SC.                                  
020600     MOVE 1 TO QT-ADDR-KEY OF PARTAB-SC.                                  
020700     MOVE 15 TO QT-LL-KEY OF PARTAB-SC.                                   
020800     OPEN INPUT FILE-TAB.                                                 
020900     MOVE 0 TO FINITO.                                                    
021000     PERFORM LEGGI-FILE-TAB THRU EX-LEGGI-FILE-TAB.                       
021100     PERFORM TRATTA-FILE-TAB THRU EX-TRATTA-FILE-TAB                      
021200             UNTIL FINE-FILE.                                             
021300     CLOSE FILE-TAB.                                                      
021400*    CALL "QSORTAB" USING PARTAB-SC  TAB-SCO.                             
021500     MOVE "K2" TO QT-FUNZIONE OF PARTAB-SC.                               
021600*IF X5=ON                                                                 
021700*    DISPLAY VC-NOME ": " QT-NUM-ELEM-EFF OF PARTAB-SC.                   
021800*    PERFORM DISPLAY-EL-SCO THRU EX-DISPLAY-EL-SCO                        
021900*       VARYING IND FROM 1 BY 1                                           
022000*       UNTIL IND > QT-NUM-ELEM-EFF OF PARTAB-SC.                         
022100*IF                                                                       
022200     IF QT-NUM-ELEM-EFF OF PARTAB-SC = 0                                  
022300*      MOVE "DTVALSTK: tab.sconti vuota o inesistente"                    
022400*             TO VC-MSG                                                   
022410       STRING "DTVALSTK: tab. " DELIMITED BY SIZE                         
022420              VC-NOME DELIMITED BY SPACE                                  
022430              " vuota o inesi" DELIMITED BY SIZE                          
022440           INTO VC-MSG                                                    
022500     ELSE                                                                 
022600       MOVE 1 TO VC-PRIMA-VOLTA                                           
022700       MOVE SPACE TO VC-MSG.                                              
022800 EX-CARICA-SCONTI.                                                        
022900     EXIT.                                                                
023000*                                                                         
023100*                                                                         
023200 LEGGI-FILE-TAB.                                                          
023300     READ FILE-TAB                                                        
023400         AT END MOVE 1 TO FINITO.                                         
023410     IF NOT FINE-FILE                                                     
023420       MOVE SPACES TO REC-TAB-COM                                         
023430       UNSTRING REC-TAB DELIMITED BY T-TAB                                
023440                            INTO ANNO-TB OF REC-TAB-COM                   
023450                                 STAGIONE-TB OF REC-TAB-COM               
023460                                 FORN-TB OF REC-TAB-COM                   
023470                                 COLL-TB OF REC-TAB-COM                   
023480                                 CLASSE-TB OF REC-TAB-COM                 
023490                                 PREZZO-TB OF REC-TAB-COM                 
023491                                 P-SCONTO-TB OF REC-TAB-COM               
023492                                 C-MAT-TB OF REC-TAB-COM                  
023493                                 CAMBIO-TB OF REC-TAB-COM.                
023500 EX-LEGGI-FILE-TAB.                                                       
023600     EXIT.                                                                
023700*                                                                         
023800*                                                                         
023900 TRATTA-FILE-TAB.                                                         
024000     IF QT-NUM-ELEM-EFF OF PARTAB-SC =                                    
024100           QT-NUM-ELEM-MAX OF PARTAB-SC                                   
024200       DISPLAY SPACE UPON CONSOLE                                         
024300       DISPLAY                                                            
024400            "DTVALSTK:  TAB-SCO tabella sconti da allargare"              
024500                    UPON CONSOLE                                          
024600       DISPLAY SPACE UPON CONSOLE                                         
024700       DISPLAY                                                            
024800            "DTVALSTK:  TAB-SCO tabella sconti da allargare"              
024900       STOP RUN.                                                          
025000*    ADD 1 TO QT-NUM-ELEM-EFF OF PARTAB-SC.                               
025100     MOVE C-MAT-TB TO DEP-DEED-8.                                         
           CANCEL "QDEEDIT"
025200     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
025300     IF QD-STATO OF PARDEED NOT = 0                                       
025400       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
025500       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
025600                    UPON CONSOLE                                          
025700       STOP RUN.                                                          
025800     MOVE DEP-DEED-8-9 TO EL-C-MAT OF COM-SCO.                            
025900     MOVE ANNO-TB TO DEP-DEED-8.                                          
           CANCEL "QDEEDIT"
026000     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
026100     IF QD-STATO OF PARDEED NOT = 0                                       
026200       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
026300       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
026400                    UPON CONSOLE                                          
026500       STOP RUN.                                                          
026600     MOVE DEP-DEED-8-9 TO EL-ANNO OF COM-SCO.                             
026610     MOVE FORN-TB TO DEP-DEED-8.                                          
           CANCEL "QDEEDIT"
026620     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
026630     IF QD-STATO OF PARDEED NOT = 0                                       
026640       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
026650       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
026660                    UPON CONSOLE                                          
026670       STOP RUN.                                                          
026680     MOVE DEP-DEED-8-9 TO EL-FORN OF COM-SCO.                             
026700     MOVE STAGIONE-TB TO DEP-DEED-8.                                      
           CANCEL "QDEEDIT"
026800     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
026900     IF QD-STATO OF PARDEED NOT = 0                                       
027000       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
027100       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
027200                    UPON CONSOLE                                          
027300       STOP RUN.                                                          
027400     MOVE DEP-DEED-8-9 TO EL-STAGIONE OF COM-SCO.                         
027410     MOVE COLL-TB TO DEP-DEED-8.                                          
           CANCEL "QDEEDIT"
027420     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
027430     IF QD-STATO OF PARDEED NOT = 0                                       
027440       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
027450       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
027460                    UPON CONSOLE                                          
027470       STOP RUN.                                                          
027480     MOVE DEP-DEED-8-9 TO EL-COLL OF COM-SCO.                             
027600     MOVE CLASSE-TB TO DEP-DEED-8.                                        
           CANCEL "QDEEDIT"
027700     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
027800     IF QD-STATO OF PARDEED NOT = 0                                       
027900       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
028000       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
028100                    UPON CONSOLE                                          
028200       STOP RUN.                                                          
028300     MOVE DEP-DEED-8-9 TO EL-CLASSE OF COM-SCO.                           
028400     MOVE P-SCONTO-TB TO DEP-DEED-8.                                      
           CANCEL "QDEEDIT"
028500     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
028600     IF QD-STATO OF PARDEED NOT = 0                                       
028700       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
028800       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
028900                    UPON CONSOLE                                          
029000       STOP RUN.                                                          
029100     MOVE DEP-DEED-8-9 TO EL-P-SCONTO OF COM-SCO.                         
029200     MOVE PREZZO-TB TO DEP-DEED-8.                                        
           CANCEL "QDEEDIT"
029300     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
029400     IF QD-STATO OF PARDEED NOT = 0                                       
029500       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
029600       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
029700                    UPON CONSOLE                                          
029800       STOP RUN.                                                          
029810*EURO*                                                                    
029820*EURO1*                                                                   
029830     IF W-FORMATO-INTERNO NOT > 011231                                    
029900       MOVE DEP-DEED-8-9 TO IE-IMPORTO-IN                                 
029901       PERFORM PRZ-INEU THRU EX-PRZ-INEU                                  
029902       MOVE IE-IMPORTO-OU TO EL-PREZZO OF COM-SCO                         
029903     ELSE                                                                 
029904       MOVE DEP-DEED-8-9 TO EL-PREZZO OF COM-SCO.                         
029905*                                                                         
029911     MOVE CAMBIO-TB TO DEP-DEED-8.                                        
           CANCEL "QDEEDIT"
029912     CALL "QDEEDIT" USING PARDEED DEP-DEED-8 DEP-DEED-8-9.                
029913     IF QD-STATO OF PARDEED NOT = 0                                       
029914       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
029915       DISPLAY "DTVALSTK: dati non numerici su file TABSTK"               
029916                    UPON CONSOLE                                          
029917       STOP RUN.                                                          
029918     MOVE DEP-DEED-8-9 TO EL-CAMBIO OF COM-SCO.                           
029919*                                                                         
030000*    MOVE COM-SCO TO EL-SCO(QT-NUM-ELEM-EFF OF PARTAB-SC).                
030010     MOVE "K1" TO QT-FUNZIONE OF PARTAB-SC.                               
           CANCEL "QTABELXL"
030020     CALL "QTABELXL" USING PARTAB-SC TAB-SCO COM-SCO.                       
030090*                                                                         
030100     PERFORM LEGGI-FILE-TAB THRU EX-LEGGI-FILE-TAB.                       
030200 EX-TRATTA-FILE-TAB.                                                      
030300     EXIT.                                                                
030400*                                                                         
030410*EURO1*                                                                   
030420 PRZ-INEU.                                                                
030430     COPY PINEU.                                                          
030440 EX-PRZ-INEU.                                                             
030450     EXIT.                                                                
030460*                                                                         
030500*                                                                         
030600 DISPLAY-EL-SCO.                                                          
030700     MOVE EL-SCO(IND) TO COM-SCO.                                         
030800     DISPLAY EL-ANNO OF COM-SCO SPACE                                     
030900             EL-STAGIONE OF COM-SCO SPACE                                 
030910             EL-FORN OF COM-SCO SPACE                                     
031000             EL-COLL OF COM-SCO SPACE                                     
031100             EL-CLASSE OF COM-SCO SPACE                                   
031210             EL-PREZZO OF COM-SCO SPACE                                   
031300             EL-P-SCONTO OF COM-SCO SPACE                                 
031310             EL-C-MAT OF COM-SCO SPACE                                    
031320             EL-CAMBIO OF COM-SCO.                                        
031400 EX-DISPLAY-EL-SCO.                                                       
031500     EXIT.                                                                
031600*                                                                         
031700*                                                                         
031800 CALCOLA-SCONTO.                                                          
031900     MOVE VC-C-MAT TO C-MAT-TRANS-RID                                     
032000     COMPUTE EL-C-MAT OF COM-SCO = (VC-C-MAT / 1000) * 1000.              
032100     MOVE ANNO OF C-MAT-TRANSITO                                          
032200        TO EL-ANNO OF COM-SCO.                                            
032300     MOVE VC-STAGIONE TO EL-STAGIONE OF COM-SCO.                          
032310     MOVE SOCIETA-MOD TO EL-FORN OF COM-SCO.                              
032400     MOVE VC-COLL                                                         
032500        TO EL-COLL  OF COM-SCO.                                           
032600     MOVE CLASSE OF C-MAT-TRANSITO                                        
032700        TO EL-CLASSE OF COM-SCO.                
           CANCEL "QTABELXL"
032800     CALL "QTABELXL" USING PARTAB-SC TAB-SCO COM-SCO.                       
032900     IF QT-STATO OF PARTAB-SC = 0                                         
032910       IF EL-PREZZO                                                       
032920           OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC) = 0                      
033000         MOVE EL-P-SCONTO                                                 
033100                     OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC)                
033200           TO VC-SCO                                                      
033300*EURO*                                                                    
033700         COMPUTE PERCENTO = 100 - VC-SCO / 100                            
034100         COMPUTE PRZ-SCO-COM ROUNDED =                                    
034200               (VC-PRZ-LORDO * PERCENTO) / 100                            
034210*                                                                         
034300         MOVE PRZ-SCO-COM  TO VC-PRZ-SCO                                  
034310       ELSE                                                               
034320         MOVE 0 TO VC-SCO                                                 
034340         MOVE EL-PREZZO                                                   
034350                     OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC)                
034360           TO VC-PRZ-SCO                                                  
034390       END-IF                                                             
034391       MOVE EL-CAMBIO OF EL-SCO (QT-INDEX-ELEM OF                         
034392                    PARTAB-SC) TO VC-CAMBIO                               
034400       MOVE SPACE TO VC-MSG                                               
034500       GO TO EX-CALCOLA-SCONTO.                                           
034510**                                                                         
034600     MOVE 0 TO EL-C-MAT OF COM-SCO.                                       
           CANCEL "QTABELXL"
034700     CALL "QTABELXL" USING PARTAB-SC TAB-SCO COM-SCO.                       
034800     IF QT-STATO OF PARTAB-SC = 0                                         
034810       IF EL-PREZZO                                                       
034820           OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC) = 0                      
034900         MOVE EL-P-SCONTO                                                 
035000                     OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC)                
035100           TO VC-SCO                                                      
035110*EURO*                                                                    
035200         COMPUTE PERCENTO = 100 - VC-SCO / 100                            
035300         COMPUTE PRZ-SCO-COM ROUNDED =                                    
035400               (VC-PRZ-LORDO * PERCENTO) / 100                            
035410*                                                                         
035500         MOVE PRZ-SCO-COM  TO VC-PRZ-SCO                                  
035600       ELSE                                                               
035700         MOVE 0 TO VC-SCO                                                 
035800         MOVE EL-PREZZO                                                   
035900                     OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC)                
036000           TO VC-PRZ-SCO                                                  
036100       END-IF                                                             
036110       MOVE EL-CAMBIO OF EL-SCO (QT-INDEX-ELEM OF                         
036120                    PARTAB-SC) TO VC-CAMBIO                               
036200       MOVE SPACE TO VC-MSG                                               
036300       GO TO EX-CALCOLA-SCONTO.                                           
      **
036303     MOVE 0                                                               
036304        TO EL-COLL  OF COM-SCO.                                           
           CANCEL "QTABELXL"
036305     CALL "QTABELXL" USING PARTAB-SC TAB-SCO COM-SCO.                       
036306     IF QT-STATO OF PARTAB-SC = 0                                         
036307       IF EL-PREZZO                                                       
036308           OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC) = 0                      
036309         MOVE EL-P-SCONTO                                                 
036310                     OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC)                
036311           TO VC-SCO                                                      
036312*EURO*                                                                    
036313         COMPUTE PERCENTO = 100 - VC-SCO / 100                            
036314         COMPUTE PRZ-SCO-COM ROUNDED =                                    
036315               (VC-PRZ-LORDO * PERCENTO) / 100                            
036316*                                                                         
036317         MOVE PRZ-SCO-COM  TO VC-PRZ-SCO                                  
036318       ELSE                                                               
036319         MOVE 0 TO VC-SCO                                                 
036320         MOVE EL-PREZZO                                                   
036321                     OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC)                
036322           TO VC-PRZ-SCO                                                  
036323       END-IF                                                             
036324       MOVE EL-CAMBIO OF EL-SCO (QT-INDEX-ELEM OF                         
036325                    PARTAB-SC) TO VC-CAMBIO                               
036326       MOVE SPACE TO VC-MSG                                               
036327       GO TO EX-CALCOLA-SCONTO.                                           
036400**                                                                         
036500     MOVE 0                                                               
036600        TO EL-FORN OF COM-SCO.                                            
037000*    DISPLAY "EL-COM  " COM-SCO.                                          
           CANCEL "QTABELXL"
037400     CALL "QTABELXL" USING PARTAB-SC TAB-SCO COM-SCO.                       
037410*    DISPLAY "STATO PARTAB  " QT-STATO OF PARTAB-SC.                      
037500     IF QT-STATO OF PARTAB-SC = 0                                         
037510       IF EL-PREZZO                                                       
037520           OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC) = 0                      
037530         MOVE EL-P-SCONTO                                                 
037540                     OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC)                
037550           TO VC-SCO                                                      
037551*EURO*                                                                    
037560         COMPUTE PERCENTO = 100 - VC-SCO / 100                            
037570         COMPUTE PRZ-SCO-COM ROUNDED =                                    
037580               (VC-PRZ-LORDO * PERCENTO) / 100                            
037581*                                                                         
037590         MOVE PRZ-SCO-COM  TO VC-PRZ-SCO                                  
037600       ELSE                                                               
037610         MOVE 0 TO VC-SCO                                                 
037620         MOVE EL-PREZZO                                                   
037630                     OF EL-SCO(QT-INDEX-ELEM OF PARTAB-SC)                
037640           TO VC-PRZ-SCO                                                  
037650       END-IF                                                             
037660       MOVE EL-CAMBIO OF EL-SCO (QT-INDEX-ELEM OF                         
037670                    PARTAB-SC) TO VC-CAMBIO                               
039000*      MOVE "DTVALSTK: assunta tab. base" TO VC-MSG-1                     
039100*      MOVE VC-C-MAT TO VC-MSG-2                                          
039200     ELSE                                                                 
039300       MOVE VC-PRZ-LORDO TO VC-PRZ-SCO                                    
039310*10099051*                                                                
039400*      MOVE 0 TO VC-SCO VC-CAMBIO.                                        
039410       MOVE 0 TO VC-SCO                                                   
039420       MOVE EL-CAMBIO OF EL-SCO (1) TO VC-CAMBIO.                         
039500*      MOVE "DTVALSTK: non esiste sconto" TO VC-MSG-1                     
039600*      MOVE VC-C-MAT TO VC-MSG-2.                                         
039610*    DISPLAY EL-ANNO OF COM-SCO UPON CONSOLE.                             
039620*    DISPLAY EL-STAGIONE OF COM-SCO UPON CONSOLE.                         
039630*    DISPLAY EL-FORN OF COM-SCO UPON CONSOLE.                             
039640*    DISPLAY EL-COLL     OF COM-SCO UPON CONSOLE.                         
039650*    DISPLAY EL-CLASSE OF COM-SCO UPON CONSOLE.                           
039660*    DISPLAY EL-C-MAT    OF COM-SCO UPON CONSOLE.                         
039700 EX-CALCOLA-SCONTO.                                                       
039800     EXIT.                                                                
039900*                                                                         
040000*                                                                         
