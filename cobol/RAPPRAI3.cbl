001000*CONTROL SUBPROGRAM                                                       
001100 IDENTIFICATION DIVISION.                                                 
001200 PROGRAM-ID. RAPPRAI3.                                                    
001300*                                                                         
001400*2000*        06/08/99                                                    
001500*     tratta date a 6 cifre                                               
001600*                                                                         
001610*MAG6/7*                                                                  
001620*   20/03/00   aggiunto mag di provenienza in input                       
001630*                                                                         
001640*EURO*        27/12/00                          EURO/LIRE                 
001650*     trattamento importi in EURO                                         
001660*                                                                         
      *NO-DATGE*        novembre 2020 VALERIA 
      *     dismissione DATGE >> sostiuita PF.CLASSI con anagrafica_classi_dbg
      *     e' rimasta la connect a datge per serv_dati_stampa
      *
001700*                                                                         
001800 ENVIRONMENT DIVISION.                                                    
001900 CONFIGURATION   SECTION.                                                 
002000 SOURCE-COMPUTER.         HP3000.                                         
002100 OBJECT-COMPUTER.         HP3000.                                         
002200 SPECIAL-NAMES.                                                           
002300      DECIMAL-POINT IS COMMA.               
002400*                                                                         
002500 DATA DIVISION.                                                           
002600*                                                                         
002700*                                                                         
002800*                                                                         
002900 WORKING-STORAGE SECTION.                                                 
003000*  
      *ESTETA* 
       COPY NTG.                                                                        
003100*                                                                         
003200 77 ERRORE       PIC -(4).                                                
003300*                                                                         
003400*                                                                         
003500*                                                                         
003600*DEFINIZIONE DELLA RIGA DI STAMPA.                                        
003700*                                                                         
003800 01  RIGA.                                                                
003900  05 PAR-RIGA-COMANDO.                                                    
004000   10 N-STAMPANTE                   PIC 9.                                
004100   10 COMANDO                       PIC X.                                
004200   10 NUM-RIGA                      PIC 9(4) COMP.                        
004300  05 DATI-RIGA-STAMPA               PIC X(132).                           
004400  05 FILLER REDEFINES DATI-RIGA-STAMPA.                                   
004500    10 CLASSE-R                PIC 99.                                    
004600    10 CLASSE-T REDEFINES CLASSE-R                                        
004700                               PIC XX.                                    
004800    10 FILLER                  PIC X.                                     
004900    10 D-CLASSE-R              PIC X(22).                                 
005000    10 FILLER                  PIC X.                                     
005100    10 QTA-TOT-R               PIC Z(9).                                  
005200    10 QTA-TOT-T  REDEFINES QTA-TOT-R                                     
005300                               PIC X(9).                                  
005400    10 FILLER                  PIC X.                                     
005500    10 PREZZO-R                PIC Z(7),ZZ.                               
005600    10 PREZZO-T REDEFINES PREZZO-R                                        
005700                               PIC X(10).                                 
005800    10 FILLER                  PIC XX.                                    
005900    10 QTA-TOT-R-NO-G          PIC Z(9).                                  
006000    10 QTA-TOT-T-NO-G  REDEFINES QTA-TOT-R-NO-G                           
006100                               PIC X(9).                                  
006200    10 FILLER                  PIC X.                                     
006300    10 PREZZO-R-NO-G           PIC Z(7),ZZ.                               
006400    10 PREZZO-T-NO-G REDEFINES PREZZO-R-NO-G                              
006500                               PIC X(10).                                 
006600    10 FILLER                  PIC XX.                                    
006700    10 QTA-TOT-R-GEN           PIC Z(9).                                  
006800    10 QTA-TOT-T-GEN  REDEFINES QTA-TOT-R-GEN                             
006900                               PIC X(9).                                  
007000    10 FILLER                  PIC X.                                     
007100    10 PREZZO-R-GEN            PIC Z(7),ZZ.                               
007200    10 PREZZO-T-GEN REDEFINES PREZZO-R-GEN                                
007300                               PIC X(10).                                 
007310*EURO*                                                                    
007320    10 FILLER        PIC X.                                               
007330    10 PREUR-R       PIC ZZZ.ZZZ.ZZZ.                                     
007340    10 PREUR-T REDEFINES PREUR-R PIC X(11).                               
007350    10 FILLER        PIC X.                                               
007360    10 PREUR-R-NO-G   PIC ZZZ.ZZZ.ZZZ.                                    
007370    10 PREUR-T-NO-G REDEFINES PREUR-R-NO-G PIC X(11).                     
007380    10 FILLER        PIC X.                                               
007381    10 PREUR-R-GEN   PIC ZZZ.ZZZ.ZZZ.                                     
007382    10 PREUR-T-GEN REDEFINES PREUR-R-GEN PIC X(11).                       
007400*                                                                         
007500  05 FILLER REDEFINES DATI-RIGA-STAMPA.                                   
007600    10 FILLER        PIC X.                                               
007700    10 CONTO-T       PIC ZZ9/99999.                                       
007710*MAG6/7*                                                                  
007720    10 FILLER REDEFINES CONTO-T.                                          
007730      15 D-MAG-T     PIC X(4).                                            
007740      15 MAG-T       PIC Z(3).                                            
007750      15 FILLER      PIC XX.                                              
007800    10 FILLER        PIC X.                                               
007900    10 D-CONTO-T     PIC X(24).                                           
008000    10 FILLER        PIC XXX.                                             
008100    10 DATA-T        PIC X(12).                                           
008200    10 FILLER        PIC X(5).                                            
008300    10 D-PAG-T       PIC X(5).                                            
008400    10 FILLER        PIC X.                                               
008500    10 NUM-PAG-T     PIC ZZ9.                                             
008600*                                                                         
008700*                                                                         
008800*                                                                         
008900*CAMPI-OPERATIVI!!                                                        
009000*                                                                         
009100 01        CAMPI-OPERATIVI.                                               
009200  05       NUM-PAGINA-STAMPA        PIC 9(6) COMP.                        
009300  05       NUM-RIGHE-PAGINA         PIC 9(4) COMP.                        
009400  05       FLAG-CONTROLLO           PIC 9(4) COMP.                        
009500        88 FLAG-OK       VALUE 0.                                         
009600  05       CONTA-PAGINE             PIC 9(6) COMP.                        
009700  05       CONTA-RIGHE              PIC 9(4) COMP.                        
009800  05       FLAG-USCITA              PIC 9(4) COMP.                        
009900  05       I-S-RIGA                 PIC S9(4) COMP.                       
010000  05       RIGHE-DA-SALT            PIC S9(4) COMP.                       
010100*                                                                         
010200*                                                                         
010300 01 CONTA-ELEM                      PIC S9(4) COMP.                       
010400*                                                                         
010500 01 NUM-RIGHE-EFF   PIC S9(4) COMP.                                       
010600*                                                                         
010700*                                                                         
010800 01 ESCEk2S.                                                              
010900   05 FILLER   PIC X(1) VALUE "".                                        
011000   05 FILLER   PIC X(1) VALUE "&".                                        
011100   05 FILLER   PIC X(1) VALUE "k".                                        
011200   05 FILLER   PIC X(1) VALUE "2".                                        
011300   05 FILLER   PIC X(1) VALUE "S".                                        
011400*                                                                         
011500 01 LL-COM       PIC S9(4) COMP.                                          
011600*                                                                         
011700 01       CHIUDI-PROGRAMMA             PIC X(4).                          
011800        88 USCITA           VALUE "STOP", "stop".                         
011900 01       TABULATO                     PIC 9.                             
012000*                                                                         
012100* PARAMETRI PER GESTIONE DIVERSE LUNGHEZZE DI RIGA.                       
012200*                                                                         
012300 01 TIPO-STAMPA          PIC S9(4) COMP.                                  
012400  88 STAMPA-30             VALUE 0.                                       
012500  88 STAMPA-80             VALUE 1.                                       
012600  88 STAMPA-132            VALUE 2.                                       
012700  88 STAMPA-158            VALUE 3.                                       
012800  88 STAMPA-224            VALUE 4.                                       
012900  88 STAMPA-320            VALUE 5.                                       
013000*                                                                         
013100 01 PAR-INFO.                                                             
013200  05 F-NUM-INFO         PIC S9(4) COMP.                                   
013300  05 OPTION-INFO        PIC S9(4) COMP.                                   
013400  05 REC-INFO           PIC S9(4) COMP.                                   
013500  05 CODE-INFO          PIC S9(4) COMP.                                   
013600  05 BLOC-INFO          PIC S9(4) COMP.                                   
013700*                                                                         
013800 01 V1       PIC S9(4) COMP.                                              
013900 01 V2                                  PIC S9(4) COMP.                   
014000 01 V3                                  PIC S9(4) COMP.                   
014100*                                                                         
014200*                                                                         
014300*                                                                         
014400*DEFINIZIONE PARAMETRI PER LA QPRINTX!!                                   
014500*                                                                         
      *conv
      *
014600*01        PAR-PRINTX.                                                    
014700* 05       STATO                    PIC S9(4) COMP.                       
014800* 05       LL-RIGA                  PIC 9(4) COMP.                        
014900* 05       N-MAX-RIGHE              PIC 9(4) COMP.                        
015000* 05       NUM-FILE-ID              PIC 9(4) COMP.                        
015100* 05       N-RIGHE-PAGINA           PIC 9(4) COMP.                        
015200* 05       RIGA-CORRENTE            PIC 9(4) COMP.                        
015300* 05       FLAGS-ROUTINE            PIC 9(4) COMP.                        
015400* 05       FLAG-VIDEO               PIC 9 VALUE 0.                        
015500* 05       FILLER                   PIC XXX.                              
015600* 05       DIMENSIONE-BUFFER        PIC 9(4) COMP.                        
015700* 05       LL-OCCUPATA-BUFFER       PIC 9(4) COMP.                        
      *
       01 PAR-PRINTX.
        05 STATO                 PIC S9(4) COMP.
        05 LL-RIGA               PIC S9(4) COMP.
          88 LL-RIGA-OK VALUE 34 , 84 , 136 , 162 , 228 , 324.
        05 N-MAX-RIGHE           PIC S9(4) COMP.
        05 FLAGS-ROUTINE         PIC S9(4) COMP.
        05 NUM-FILE-ID           PIC S9(4) COMP.
        05 NOME-FILE             PIC X(12).
      *
       01 BUFFER-ST.
        05 DIMENSIONE-BUFFER     PIC S9(4) COMP.
        05 LL-OCCUPATA-BUFFER    PIC S9(4) COMP.
        05 RESTO-BUFF            PIC X(5120).
      *
      *conv-end
015800*                                                                         
015900*                                                                         
016000*                                                                         
016100 01 DISPOSITIVO-COM    PIC X(4) VALUE "0174".                             
016200 01 DISPOSITIVO-COM-PR PIC X(4) VALUE "0178".                             
016300*                                                                         
016400*                                                                         
016500*DEFINIZIONE TERMINALE SCRIVENTE!!                                        
016600*                                                                         
016700 01        W-SIGLA-OUT-ASSOCIATO.                                         
016800  05       DISPOSITIVO              PIC X(4).                             
016900  88 STAMPANTE-SISTEMA VALUE "LP  " , "LP2 " , "LP3 " , "LP4 "            
017000                           , "LP5 " , "LP6 " , "LP7 " , "LP8 "            
017100                           , "LP1 ".                                      
017200  05       TERMINALE-SR REDEFINES DISPOSITIVO.                            
017300   10      TERM-SR                  PIC X(3).                             
017400   10      TIPO-SR                  PIC 9.                                
017500  05       TERMINALE-RO REDEFINES DISPOSITIVO.                            
017600   10      TERM-RO                  PIC 9.                                
017700   10      TIPO-RO                  PIC 9.                                
017800   10      LINEA                    PIC 99.                               
017900*                                                                         
018000*                                                                         
018100*                                                                         
018200 01 NOME-FILE-OPEN-COM.                                                   
018300  05 PRIMI-7-NOME-COM.                                                    
018400   10 EL-PRIMI-7-COM                    PIC X OCCURS 7.                   
018500  05 NUM-TAB-COM                        PIC 9.                            
018600  05 ULT-CAR-COM                        PIC X.                            
018700*                                                                         
018800 01 SECONDO-PAR-OPEN.                                                     
018900   05 STAMPANTE-ASSOC                   PIC X(4).                         
019000   05 PAR-SEC-FILE-OPEN.                                                  
019100    10      NOME-FILE-OPEN              PIC X(7).                         
019200    10      NUM-TAB-OPEN                PIC 9.                            
019300    10      ULT-CAR-OPEN                PIC X.                            
019400*                                                                         
019500*                                                                         
019600 01 TENTATIVI        PIC S9(4) COMP.                                      
019700*                                                                         
019800 01  RIGA-MEM.                                                            
019900  05 PAR-RIGA-COMANDO-MEM.                                                
020000   10 N-STAMPANTE-MEM               PIC 9.                                
020100   10 COMANDO-MEM                   PIC X.                                
020200   10 NUM-RIGA-MEM                  PIC 9(4) COMP.                        
020300  05 DATI-RIGA-STAMPA-MEM           PIC X(320).                           
020400*                                                                         
020500*                                                                         
020600 01 FLAG-GRAF                           PIC 9.                            
020700  88 STAMPA-NORMALE                           VALUE 0.                    
020800  88 STAMPA-GRAF                              VALUE 1.                    
020900*                                                                         
021000*                                                                         
021100 01 RIGA-SEQESC.                                                          
021200  05 DETT-RIGA-SEQ   PIC X(40) OCCURS 3.                                  
021300*                                                                         
021400 01 FILLER REDEFINES RIGA-SEQESC.                                         
021500  05 RIGA-SEQ-ESC-INT   PIC X(120).                                       
021600*                                                                         
021700*                                                                         
021800*SQL                                                                      
021900 EXEC SQL BEGIN DECLARE SECTION END-EXEC.                                 
022000*                                                                         
022100 EXEC SQL INCLUDE SERVESC.IF END-EXEC. 

      *NO-DATGE*                                                        inizio
046510*EXEC SQL INCLUDE CLASSI.IF END-EXEC. 
       01 CC-SOCIETA PIC XX.
       01 CC-CLASSE PIC XXX.
       01 CC-DESCRIZIONE PIC X(50).
      *NO-DATGE*                                                        fine
      
022300*                                                                         
022400 01 TABELLA-CLASSI.                                                       
022500   05 CLASSI-TAB  OCCURS 200.                                             
022600     10 CLASSE-CL     PIC XXX.                                            
022700     10 DESCRIZIONE-CL PIC X(50).                                         
022800*                                                                         
022900 EXEC SQL END DECLARE SECTION END-EXEC.                                   
023000*                                                                         
023100 01 SQL-CONST             COPY SQLCONST .                       
023200 01 PAR-ERR               COPY PARERR .                         
023300 01 AREA-HL               COPY AREAHL .                         
023400 01 AREA-SI               COPY AREASI .                         
023500*                                                                         
023600*SQL                                                                      
023700 01 C-MAT-COM COPY DANCODBC.                                              
023800*                                                                         
023900*                                                                         
024000*                                                                         
024100 01 ART-ELEM-LETTI.                                                       
024200   05 ELEM-ART        PIC S9(15) COMP-3.                                  
024300   05 D-MAT-ELEM     PIC X(7).                                            
024400   05 PRIMA-TG-ELEM    PIC S9(4) COMP.                                    
024500   05 PREZZO-ELEM      PIC S9(9) COMP.                                    
024600   05 CAMBIO-ELEM      PIC S9(9) COMP.                                    
024700   05 TIPO-ANA-ELEM    PIC XX.                                            
024800   05 QTA-GIAC-ELEM.                                                      
024900     10 QTA-GIAC-PF-ELEM  PIC S9(8) COMP COPY NTGOCCURS.                       
025000   05 QTA-TAGLIE-ELEM.                                                    
025100     10 QTA-TAGLIA-ELEM PIC S9(4) COMP COPY NTGOCCURS.   
      *VACO*                                                            
         05 COSTO-TAB       PIC S9(9) COMP.                       
025200*                                                                         
025300 01 QTA-TOT-COM    PIC S9(9) COMP.                                        
025400 01 QTA-NO-GIAC-COM    PIC S9(9) COMP.                                    
025500 01 QTA-GEN-COM    PIC S9(9) COMP.                                        
025600*                                                                         
025700 01 IND-T      PIC S9(4) COMP.                                            
025800*                                                                         
025900 01 PREZZO-TOT-COM     PIC S9(15) COMP-3.                                 
026000 01 PREZZO-NO-GIAC-COM     PIC S9(15) COMP-3.                             
026100 01 PREZZO-GEN-COM     PIC S9(15) COMP-3.                                 
026200*                                                                         
026300*                                                                         
026400 01 TABELLA-CLASSI-LETTE.                                                 
026500   05 CLASSI-TAB-LETTE   OCCURS 100.                                      
026600     10 CLASSE-TAB     PIC S9(4) COMP.                                    
026700     10 QTA-TOT-TAB    PIC S9(9) COMP.                                    
026800     10 PREZZO-TOT-TAB PIC S9(15) COMP-3.                                 
026900     10 QTA-NO-GIAC-TAB  PIC S9(9) COMP.                                  
027000     10 PREZZO-NO-GIAC-TAB  PIC S9(15) COMP-3.                            
027100*                                                                         
027200 01 PARTAB-CLASSI  COPY QPARTAB.                                          
027300*                                                                         
027400 01 NUM-ELEM-MAX-CLASSI  PIC S9(4) COMP VALUE 100.                        
027500*                                                                         
027600 01 IND-C    PIC S9(4) COMP.                                              
027700*                                                                         
027800*                                                                         
027900 01 CLASSI-ELEM-LETTE.                                                    
028000     05 CLASSE-ELEM    PIC S9(4) COMP.                                    
028100     05 QTA-TOT-ELEM   PIC S9(9) COMP.                                    
028200     05 PREZZO-TOT-ELEM PIC S9(15) COMP-3.                                
028300     05 QTA-NO-GIAC-ELEM  PIC S9(9) COMP.                                 
028400     05 PREZZO-NO-GIAC-ELEM PIC S9(15) COMP-3.                            
028500*                                                                         
028600*                                                                         
028700 01 N-MAX-CL   PIC S9(4) COMP VALUE 100.                                  
028800*                                                                         
028900 01 FLAG-CLASSI  PIC 9(4) COMP VALUE 0.                                   
029000  88 CLASSI-DA-LEGGERE VALUE 0.                                           
029100*                                                                         
029200*                                                                         
029300 01 CLASSE-COM.                                                           
029400   05 CLASSE-2-COM    PIC 99.                                             
029500   05 FILLER          PIC X VALUE SPACES.                                 
029600*                                                                         
029610*EURO*                                                                    
029620 01 PAR-INEU      COPY QPARINEU.                                          
029630*                                                                         
029700*                                                                         
029800 LINKAGE SECTION.                                                         
029900*                                                                         
030000 EXEC SQL INCLUDE SQLCA END-EXEC.                                         
030100*                                                                         
030200 01 W-COMMON   COPY WCOMMONW.                                             
030300*                                                                         
030400 01 TABELLA-ARTICOLI-LETTI.                                               
030500  03 ART-TAB-LETTI   OCCURS 5000.
030600   05 TAB-ART        PIC S9(15) COMP-3.                                   
030700   05 D-MAT-TAB      PIC X(7).                                            
030800   05 PRIMA-TG-TAB     PIC S9(4) COMP.                                    
030900   05 PREZZO-TAB       PIC S9(9) COMP.                                    
031000   05 CAMBIO-TAB       PIC S9(9) COMP.                                    
031100   05 TIPO-ANA-TAB  PIC XX.                                               
031200   05 QTA-GIAC-TAB.                                                       
031300     10 QTA-GIAC-PF-TAB  PIC S9(8) COMP COPY NTGOCCURS.                         
031400   05 QTA-TAGLIE-TAB.                                                     
031500     10 QTA-TAGLIA-TAB PIC S9(4) COMP COPY NTGOCCURS.   
      *VACO*                                                            
         05 COSTO-TAB       PIC S9(9) COMP.                        
031600*                                                                         
031700 01 PARTAB-ART   COPY QPARTAB.                                            
031800*                                                                         
031900 01 CONTO-IN-R   PIC 9(8).                                                
032000*                                                                         
032100 01 D-CONTO-MEM  PIC X(24).                                               
032200*                                                                         
032300 01 TABELLA-NO-GIAC.                                                      
032400  05 ELEM-NO-GIAC       OCCURS 1000.                                      
032500   10 C-MAT-NO-GIAC     PIC S9(15) COMP-3.                                
032600   10 PREZZO-NO-GIAC     PIC S9(9) COMP.                                  
032610   10 D-MAT-NO-GIAC       PIC X(7).                                       
032700*                                                                         
032800 01 IND-CAPI-NO-GIAC      PIC S9(4) COMP.                                 
032900*                                                                         
032910*MAG6/7*                                                                  
032920 01 MAG-INPUT-R   PIC 999.                                                
032930*                                                                         
033000*PAGE                                                                     
033100 PROCEDURE DIVISION USING W-COMMON SQLCA                                  
033200                          TABELLA-ARTICOLI-LETTI PARTAB-ART               
033300                          CONTO-IN-R D-CONTO-MEM                          
033400                          TABELLA-NO-GIAC IND-CAPI-NO-GIAC                
033410*MAG6/7*                                                                  
033420                          MAG-INPUT-R.                                    
033500 VIA.                                                                     
033520                                                                          
033600*                                                                         
033610*    PERFORM DISP-TABELLA THRU EX-DISP-TABELLA                            
033620*             VARYING IND-C FROM 1 BY 1                                   
033630*        UNTIL IND-C > IND-CAPI-NO-GIAC.                                  
033700*                                                                         
033800     IF CLASSI-DA-LEGGERE                                                 
033900       MOVE 1 TO FLAG-CLASSI       
      *NO-DATGE*                                                        inizio
             PERFORM S-SET-1 THRU S-SET-1-EX                                                
      *NO-DATGE*                                                        fine                  
034000       PERFORM CARICA-TABELLA-CLASSI                                      
034100           THRU EX-CARICA-TABELLA-CLASSI                                  
      *NO-DATGE*                                                        inizio
             PERFORM S-SET-2 THRU S-SET-2-EX                                                
      *NO-DATGE*                                                        fine
034200       IF USCITA                                                          
034300          GO TO FINE.                                                     
034400*                                                                         
034500*                                                                         
034600*                                                                         
034700     PERFORM IN-PAR-CLASSE THRU EX-IN-PAR-CLASSE.                         
034800     PERFORM CARICA-QTA-CLASSE THRU EX-CARICA-QTA-CLASSE                  
034900       VARYING IND-C FROM 1 BY 1                                          
035000       UNTIL IND-C > QT-NUM-ELEM-EFF OF PARTAB-ART OR                     
035100             USCITA.                                                      
035200     IF USCITA                                                            
035300        GO TO FINE.                                                       
035400*                                                                         
035500     PERFORM CARICA-QTA-NO-GIAC THRU EX-CARICA-QTA-NO-GIAC                
035600          VARYING IND-C FROM 1 BY 1                                       
035700            UNTIL IND-C > IND-CAPI-NO-GIAC.                               
035800*                                                                         
036200     PERFORM IN-PAR-PRINTX THRU EX-IN-PAR-PRINTX.                         
036300     MOVE SPACE TO CHIUDI-PROGRAMMA.                                      
036400     MOVE SPACE TO DATI-RIGA-STAMPA.                                      
036500     PERFORM APRI-SCRIVENTE THRU EX-APRI-SCRIVENTE.                       
036600     IF USCITA                                                            
036700        GO TO FINE.                                                       
036800*                                                                         
036900*                                                                         
037000*                                                                         
037100     MOVE 0 TO CONTA-PAGINE.                                              
037200     MOVE 0 TO CONTA-RIGHE.                                               
037300*                                                                         
037400************* gestione x stampanti LASER                                  
037500*                                                                         
037600     IF ( TIPO-RO = 1 OR = 2 ) AND                                        
037610       (LINEA = 72 OR = 73 OR = 74 OR = 75 OR = 76 OR = 77                
037700        OR = 78 OR = 79 OR = 80 OR = 81 OR = 82 OR = 83                   
037800        OR = 84 OR = 85 OR = 86 OR = 87 OR = 88 OR = 89)                  
037900        IF DATI-RIGA-STAMPA (1:5) = ESCEk2S                               
038000           MOVE SPACES TO DATI-RIGA-STAMPA                                
038100        END-IF                                                            
038200        MOVE RIGA TO RIGA-MEM                                             
038300        MOVE 0 TO CONTA-RIGHE                                             
038400        MOVE 1 TO TENTATIVI                                               
038500        PERFORM CERCA-SEQ-ESC THRU EX-CERCA-SEQ-ESC                       
038600               UNTIL TENTATIVI > 4                                        
038700        PERFORM METTI-RIGA-X-LASER                                        
038800                THRU EX-METTI-RIGA-X-LASER                                
038900        MOVE RIGA-MEM TO RIGA.                                            
039000*                                                                         
039100*                                                                         
039200     IF NOT USCITA                                                        
039300       PERFORM STAMPA-TABULATO THRU EX-STAMPA-TABULATO                    
039400*                                                                         
039500**   COMPUTE RIGHE-DA-SALT = NUM-RIGHE-PAGINA - CONTA-RIGHE               
039600**                           - 1.                                         
039700**   IF RIGHE-DA-SALT > 0                                                 
039800**       MOVE 1 TO I-S-RIGA                                               
039900**       PERFORM SALTA-RIGA THRU EX-SALTA-RIGA                            
040000**       UNTIL I-S-RIGA > RIGHE-DA-SALT.                                  
040100*                                                                         
040200*                                                                         
040300*                                                                         
      *conv
040400*    CALL "QCPRINTX" USING PAR-PRINTX RIGA.                               
           CALL "QCLPPR" USING PAR-PRINTX
                               RIGA BUFFER-ST.
      *conv-end
040500 FINE.                                                                    
040600     EXIT PROGRAM.                                                        
040700*PAGE                                                                     
040800*                                                                         
040900 DISP-TABELLA.                                                            
041000     DISPLAY C-MAT-NO-GIAC (IND-C).                                       
041100     DISPLAY PREZZO-NO-GIAC (IND-C).                                      
041200*    DISPLAY PREZZO-TOT-TAB (IND-C).                                      
041300*    DISPLAY QTA-NO-GIAC-TAB (IND-C).                                     
041400*    DISPLAY PREZZO-NO-GIAC-TAB (IND-C).                                  
041500 EX-DISP-TABELLA.                                                         
041600     EXIT.                                                                
041700*                                                                         
041800*                                                                         
041900 CARICA-QTA-NO-GIAC.                                                      
042000     MOVE C-MAT-NO-GIAC (IND-C) TO C-MAT-A-BARRE-RID.                     
042200     MOVE CLASSE OF C-MAT-A-BARRE TO CLASSE-ELEM.                         
042300     MOVE "K2" TO QT-FUNZIONE OF PARTAB-CLASSI                            
           CANCEL "QTABEL"
042310     CALL "QTABEL" USING PARTAB-CLASSI TABELLA-CLASSI-LETTE               
042500                                  CLASSI-ELEM-LETTE.                      
042600    IF QT-STATO OF PARTAB-CLASSI NOT = 0                                  
042700       MOVE 0 TO QTA-TOT-ELEM QTA-NO-GIAC-ELEM                            
042800                PREZZO-TOT-ELEM PREZZO-NO-GIAC-ELEM                       
042900       MOVE "K1" TO QT-FUNZIONE OF PARTAB-CLASSI                          
           CANCEL "QTABEL"
043000       CALL "QTABEL" USING PARTAB-CLASSI TABELLA-CLASSI-LETTE             
043100                           CLASSI-ELEM-LETTE                              
043200       IF QT-STATO OF PARTAB-CLASSI NOT = 0                               
043300         DISPLAY "ERRORE ins TABELLA-CLASSI-LETTE"                        
043400         DISPLAY "CARICO C-MAT NO GIAC "                                  
043500         GO TO EX-CARICA-QTA-NO-GIAC.                                     
043600*                                                                         
043700     MOVE CLASSI-TAB-LETTE(QT-INDEX-ELEM OF PARTAB-CLASSI)                
043800         TO CLASSI-ELEM-LETTE.                                            
043900     ADD 1 TO QTA-NO-GIAC-ELEM.                                           
044000     ADD PREZZO-NO-GIAC(IND-C) TO PREZZO-NO-GIAC-ELEM.                    
044100     MOVE CLASSI-ELEM-LETTE TO CLASSI-TAB-LETTE(QT-INDEX-ELEM             
044200                                  OF PARTAB-CLASSI).                      
044300 EX-CARICA-QTA-NO-GIAC.                                                   
044400     EXIT.                                                                
044500*                                                                         
044600*                                                                         
044700 TR-COMANDO.                                                              
044800     IF COMANDO = "M"  GO TO EX-TR-COMANDO.                               
044900     IF COMANDO = "S" OR = "G"                                            
045000        COMPUTE CONTA-RIGHE = CONTA-RIGHE + NUM-RIGA + 1                  
045100        GO TO EX-TR-COMANDO.                                              
045200     IF COMANDO = "P"                                                     
045300        IF NUM-RIGA < CONTA-RIGHE                                         
045400           COMPUTE CONTA-RIGHE = NUM-RIGA + NUM-RIGHE-PAGINA              
045500        ELSE                                                              
045600           COMPUTE CONTA-RIGHE = NUM-RIGA                                 
045700     ELSE                                                                 
045800        DISPLAY "COMANDO-ERRATO IN RECORD  FASE SKIP"                     
045900        DISPLAY "TR-COMANDO"                                              
046000        STOP RUN.                                                         
046100 EX-TR-COMANDO.                                                           
046200     EXIT.                                                                
046300*                                                                         
046400*                                                                         
046500 IN-PAR-PRINTX.                                                           
046600     MOVE 136 TO LL-COM.                                                  
046700     MOVE 2 TO TIPO-STAMPA.                                               
046800*                                                                         
046900     MOVE "RAPPRAD" TO NOME-FILE-OPEN-COM.                                
047000     MOVE 0 TO TABULATO.                                                  
047100     IF W-SIGLA-UTENTE = "RESIDUO"                                        
047200       MOVE DISPOSITIVO-COM  TO  DISPOSITIVO                              
047300     ELSE                                                                 
047400       MOVE DISPOSITIVO-COM-PR TO  DISPOSITIVO .                          
047500*                                                                         
047600     MOVE 66 TO NUM-RIGHE-PAGINA.                                         
047700*                                                                         
047800     MOVE 60 TO NUM-RIGHE-EFF.                                            
047900*                                                                         
048000     MOVE 0 TO FLAG-GRAF.                                                 
048100*                                                                         
048200     MOVE LL-COM TO LL-RIGA.                                              
048300     MOVE 1000 TO N-MAX-RIGHE.                                            
048400     MOVE 500 TO DIMENSIONE-BUFFER.                                       
048500     MOVE 0 TO LL-OCCUPATA-BUFFER.                                        
      *conv
048600*    MOVE NUM-RIGHE-PAGINA TO N-RIGHE-PAGINA.                             
      *conv-end
048700 EX-IN-PAR-PRINTX.                                                        
048800     EXIT.                                                                
048900*                                                                         
049000*                                                                         
049100 APRI-SCRIVENTE.                                                          
049200*                                                                         
049300     MOVE DISPOSITIVO TO STAMPANTE-ASSOC.                                 
049400     MOVE TABULATO TO NUM-TAB-OPEN.                                       
049500     MOVE PRIMI-7-NOME-COM TO    NOME-FILE-OPEN.                          
049600     MOVE ";" TO ULT-CAR-OPEN.                                            
049700*                                                                         
      *conv
049800*    CALL "QOPRINTX" USING PAR-PRINTX SECONDO-PAR-OPEN.                   
           MOVE PRIMI-7-NOME-COM TO NOME-FILE OF PAR-PRINTX.
           CALL "QOLPPR" USING PAR-PRINTX
                               RIGA BUFFER-ST.
           MOVE "M" TO COMANDO OF RIGA.
           MOVE 0 TO N-STAMPANTE.
           MOVE 1000 TO NUM-RIGA.
           MOVE "Rapportino Vendita" TO DATI-RIGA-STAMPA.
           CALL "QWLPPR" USING PAR-PRINTX
                               RIGA
                               BUFFER-ST.
      *conv-end
049900     IF STATO OF PAR-PRINTX NOT = 0                                       
050000           MOVE STATO OF PAR-PRINTX TO ERRORE                             
050100           DISPLAY "ERRORE Open Stampante  ** " ERRORE " **"              
050200           MOVE "STOP" TO CHIUDI-PROGRAMMA.                               
050300 EX-APRI-SCRIVENTE.                                                       
050400     EXIT.                                                                
050500*                                                                         
050600*                                                                         
050700*                                                                         
050800*                                                                         
050900 STAMPA-TABULATO.                                                         
051000*                                                                         
051100     MOVE 1 TO CONTA-ELEM.                                                
051200     MOVE 0 TO PREZZO-TOT-COM QTA-TOT-COM                                 
051300               PREZZO-GEN-COM QTA-GEN-COM                                 
051400               PREZZO-NO-GIAC-COM QTA-NO-GIAC-COM.                        
051500     PERFORM STAMPA-PAGINA THRU EX-STAMPA-PAGINA UNTIL                    
051600       USCITA  OR                                                         
051700       CONTA-ELEM > QT-NUM-ELEM-EFF OF PARTAB-CLASSI.                     
051800*                                                                         
051900     MOVE SPACE TO DATI-RIGA-STAMPA.                                      
052000     MOVE ALL "-" TO    CLASSE-T D-CLASSE-R                               
052100                   QTA-TOT-T QTA-TOT-T-NO-G QTA-TOT-T-GEN                 
052200                   PREZZO-T PREZZO-T-NO-G PREZZO-T-GEN.                   
052300     PERFORM DEF-TIPO-SCRIVENTE THRU                                      
052400              EX-DEF-TIPO-SCRIVENTE.                                      
052500*                                                                         
052600     MOVE SPACE TO DATI-RIGA-STAMPA.                                      
052700     MOVE "Totali" TO D-CLASSE-R.                                         
052800     MOVE  QTA-TOT-COM TO QTA-TOT-R                                       
052900     COMPUTE PREZZO-R = PREZZO-TOT-COM / 100.                             
052910*EURO*                                                                    
052920     MOVE PREZZO-TOT-COM TO IE-IMPORTO-IN.                                
052930     COPY PDAEU.                                                          
052940     COMPUTE PREUR-R = IE-IMPORTO-OU / 100.                               
052950*                                                                         
053000     MOVE  QTA-NO-GIAC-COM TO QTA-TOT-R-NO-G.                             
053100     COMPUTE PREZZO-R-NO-G = PREZZO-NO-GIAC-COM / 100.                    
053110*EURO*                                                                    
053120     MOVE PREZZO-NO-GIAC-COM TO IE-IMPORTO-IN.                            
053130     COPY PDAEU.                                                          
053140     COMPUTE PREUR-R-NO-G = IE-IMPORTO-OU / 100.                          
053150*                                                                         
053200     MOVE  QTA-GEN-COM TO QTA-TOT-R-GEN.                                  
053300     COMPUTE PREZZO-R-GEN = PREZZO-GEN-COM / 100.                         
053310*EURO*                                                                    
053320     MOVE PREZZO-GEN-COM TO IE-IMPORTO-IN.                                
053330     COPY PDAEU.                                                          
053340     COMPUTE PREUR-R-GEN = IE-IMPORTO-OU / 100.                           
053350*                                                                         
053400     PERFORM DEF-TIPO-SCRIVENTE THRU                                      
053500              EX-DEF-TIPO-SCRIVENTE.                                      
053600 EX-STAMPA-TABULATO.                                                      
053700     EXIT.                                                                
053800*                                                                         
053900*                                                                         
054000 STAMPA-PAGINA.                                                           
054100     PERFORM INTESTA-PAGINA THRU EX-INTESTA-PAGINA.                       
054200*                                                                         
054300     MOVE SPACES TO DATI-RIGA-STAMPA.                                     
054400     MOVE "S" TO COMANDO.                                                 
054500     MOVE 0 TO NUM-RIGA.                                                  
054600     MOVE 0 TO N-STAMPANTE.                                               
054700*                                                                         
054800     PERFORM STAMPA-RIGA THRU EX-STAMPA-RIGA                              
054900         UNTIL USCITA OR                                                  
055000               CONTA-ELEM > QT-NUM-ELEM-EFF                               
055100                             OF PARTAB-CLASSI  OR                         
055200               CONTA-RIGHE > NUM-RIGHE-EFF.                               
055300 EX-STAMPA-PAGINA.                                                        
055400     EXIT.                                                                
055500*                                                                         
055600*                                                                         
055700 STAMPA-RIGA.                                                             
055800     MOVE CLASSI-TAB-LETTE(CONTA-ELEM) TO CLASSI-ELEM-LETTE.              
055900     MOVE CLASSE-ELEM TO CLASSE-R.                                        
056000     PERFORM IMPOSTA-DESCRIZIONE                                          
056100                THRU EX-IMPOSTA-DESCRIZIONE.                              
056200     MOVE QTA-TOT-ELEM TO QTA-TOT-R.                                      
056300     COMPUTE PREZZO-R = PREZZO-TOT-ELEM / 100.                            
056310*EURO*                                                                    
056320     MOVE PREZZO-TOT-ELEM TO IE-IMPORTO-IN.                               
056330     COPY PDAEU.                                                          
056340     COMPUTE PREUR-R = IE-IMPORTO-OU / 100.                               
056350*                                                                         
056400     MOVE QTA-NO-GIAC-ELEM TO QTA-TOT-R-NO-G.                             
056500     COMPUTE PREZZO-R-NO-G = PREZZO-NO-GIAC-ELEM / 100.                   
056510*EURO*                                                                    
056520     MOVE PREZZO-NO-GIAC-ELEM TO IE-IMPORTO-IN.                           
056530     COPY PDAEU.                                                          
056540     COMPUTE PREUR-R-NO-G = IE-IMPORTO-OU / 100.                          
056550*                                                                         
056600     COMPUTE QTA-TOT-R-GEN = QTA-TOT-ELEM + QTA-NO-GIAC-ELEM.             
056700     COMPUTE PREZZO-R-GEN = (PREZZO-TOT-ELEM / 100 +                      
056710                   PREZZO-NO-GIAC-ELEM / 100).                            
056720*EURO*                                                                    
056730     ADD PREZZO-TOT-ELEM TO PREZZO-NO-GIAC-ELEM                           
056740       GIVING IE-IMPORTO-IN.                                              
056750     COPY PDAEU.                                                          
056760     COMPUTE PREUR-R-GEN = IE-IMPORTO-OU / 100.                           
056900*                                                                         
057000     PERFORM DEF-TIPO-SCRIVENTE THRU                                      
057100              EX-DEF-TIPO-SCRIVENTE.                                      
057200*                                                                         
057300     ADD QTA-TOT-ELEM TO QTA-TOT-COM QTA-GEN-COM.                         
057400     ADD PREZZO-TOT-ELEM TO PREZZO-TOT-COM PREZZO-GEN-COM.                
057500     ADD QTA-NO-GIAC-ELEM TO QTA-NO-GIAC-COM QTA-GEN-COM.                 
057600     ADD PREZZO-NO-GIAC-ELEM TO PREZZO-NO-GIAC-COM                        
057700                         PREZZO-GEN-COM.                                  
057800*                                                                         
057900     ADD 1 TO CONTA-ELEM.                                                 
058000 EX-STAMPA-RIGA.                                                          
058100     EXIT.                                                                
058200*                                                                         
058300*                                                                         
058400 DEF-TIPO-SCRIVENTE.                                                      
058500                      PERFORM TT-STAMPANTE-OLIVETTI THRU                  
058600                              EX-TT-STAMPANTE-OLIVETTI.                   
058700 EX-DEF-TIPO-SCRIVENTE.                                                   
058800     EXIT.                                                                
058900*                                                                         
059000*                                                                         
059100*                                                                         
059200*                                                                         
059300 TT-STAMPANTE-OLIVETTI.                                                   
059310    IF (LINEA = 72 OR = 73 OR = 74 OR = 75 OR = 76 OR = 77                
059320        OR = 78 OR = 79 OR = 80 OR = 81 OR = 82 OR = 83                   
059330        OR = 84 OR = 85 OR = 86 OR = 87 OR = 88 OR = 89)                  
059600          AND DATI-RIGA-STAMPA (1:5) = ESCEk2S                            
059700        MOVE SPACES TO DATI-RIGA-STAMPA.                                  
059800     MOVE 1 TO FLAG-CONTROLLO.                                            
059900     IF COMANDO NOT = "P" AND NOT = "S" AND NOT = "G"                     
060000        GO TO EX-TT-STAMPANTE-OLIVETTI.                                   
060100*                                                                         
060200     IF COMANDO = "P"                                                     
060300        PERFORM OO-COMANDO-P THRU EX-OO-COMANDO-P                         
060400        GO TO CONT-OLIVETTI.                                              
060500*                                                                         
060600     IF COMANDO = "S" OR = "G"                                            
060700        PERFORM OO-COMANDO-S THRU EX-OO-COMANDO-S.                        
060800*                                                                         
060900 CONT-OLIVETTI.                                                           
061000        IF NOT USCITA                                                     
      *conv
061100*         CALL "QWPRINTX" USING PAR-PRINTX RIGA                           
                CALL "QWLPPR" USING PAR-PRINTX
                                    RIGA
                                    BUFFER-ST
      *conv-end
061200*         DISPLAY "LUNG " N-MAX-RIGHE                                     
061300          IF STATO OF PAR-PRINTX NOT = 0                                  
061400            DISPLAY "ERRORE PRINTX" STATO OF PAR-PRINTX                   
061500            IF STATO OF PAR-PRINTX = -112                                 
061600               MOVE "STOP" TO CHIUDI-PROGRAMMA                            
061700             ELSE                                                         
061800               MOVE STATO OF PAR-PRINTX TO ERRORE                         
061900               DISPLAY "ERRORE Stampante  ** " ERRORE " **"               
062000               MOVE "STOP" TO CHIUDI-PROGRAMMA  .                         
062100 EX-TT-STAMPANTE-OLIVETTI.                                                
062200     EXIT.                                                                
062300*                                                                         
062400*                                                                         
062500 OO-COMANDO-P.                                                            
062600 EX-OO-COMANDO-P.                                                         
062700     EXIT.                                                                
062800*                                                                         
062900*                                                                         
063000 OO-COMANDO-S.                                                            
063100      COMPUTE CONTA-RIGHE = CONTA-RIGHE + NUM-RIGA + 1.                   
063200 EX-OO-COMANDO-S.                                                         
063300     EXIT.                                                                
063400*                                                                         
063500*                                                                         
063600*                                                                         
063700*                                                                         
063800 CERCA-SEQ-ESC.                                                           
063900     MOVE TIPO-STAMPA TO SERV-TIPO-STAMPA.                                
064000     MOVE W-SIGLA-UTENTE TO SERV-NOME-ACCT.                               
064100     MOVE PRIMI-7-NOME-COM TO SERV-NOME-FILE.                             
064200     IF TENTATIVI NOT < 2                                                 
064300        MOVE SPACES TO SERV-NOME-ACCT.                                    
064400     IF TENTATIVI = 3                                                     
064500        MOVE SPACES TO SERV-NOME-FILE.                                    
064600     IF TENTATIVI = 4                                                     
064700        MOVE W-SIGLA-UTENTE TO SERV-NOME-ACCT.                            
064800     PERFORM WITH TEST AFTER                                              
064900             UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                   
065000             PERFORM BEGIN-RC THRU                                        
065100                     BEGIN-RC-EX                                          
065200             IF SQLCODE = OK    

      *NO-DATGE*                                                        INIZIO 
                          PERFORM S-SET-2 THRU S-SET-2-EX                                          
      *NO-DATGE*                                                        FINE 

065300                     PERFORM SELECT-SEQ-ESC                               
065400                        THRU SELECT-SEQ-ESC-EX                            

      *NO-DATGE*                                                        INIZIO 
                           PERFORM S-SET-1 THRU S-SET-1-EX                                          
      *NO-DATGE*                                                        FINE 

065500             END-IF                                                       
065600     END-PERFORM.                                                         
065700     IF SQLCODE = NOT-FOUND                                               
065800        ADD 1 TO TENTATIVI                                                
065900       ELSE                                                               
066000          ADD 9 TO TENTATIVI.                                             
066100     PERFORM S-COMMIT THRU S-COMMIT-EX.                                       
066200 EX-CERCA-SEQ-ESC.                                                        
066300     EXIT.                                                                
066400*                                                                         
066500*                                                                         
066600 SELECT-SEQ-ESC.                                                          
066700     EXEC SQL                                                             
066800        SELECT DESCR,                                                     
066900               SEQ_ESC_1,                                                 
067000               SEQ_ESC_2,                                                 
067100               SEQ_ESC_3                                                  
067200               INTO :SERV-DESCRIZIONE,                                    
067300                    :SERV-SEQ-ESC-1,                                      
067400                    :SERV-SEQ-ESC-2,                                      
067500                    :SERV-SEQ-ESC-3                                       
067600               FROM SERV.DATI_STAMPA                                      
067700               WHERE TIPO_STAMPA   =:SERV-TIPO-STAMPA AND                 
067800                     NOME_ACCT     =:SERV-NOME-ACCT AND                   
067900                     NOME_FILE     =:SERV-NOME-FILE                       
068000     END-EXEC                                                             
068100     MOVE "SELECT SERV-DATI_STAMPA" TO ER-DESCRIZIONE                     
068200     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
068300 SELECT-SEQ-ESC-EX.                                                       
068400     EXIT.                                                                
068500*                                                                         
068600*                                                                         
068700 BEGIN-RC.                                                                
068800     EXEC SQL                                                             
068900        BEGIN WORK RC                                                     
069000     END-EXEC                                                             
069100     MOVE "BEGIN WORK RC" TO ER-DESCRIZIONE                               
069200     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
069300 BEGIN-RC-EX.                                                             
069400     EXIT.                                                                
069500*                                                                         
069600*                                                                         
069700*                                                                         
069800 TEST-ERR.                                                                
069900     MOVE SQLCODE TO SQL-STATUS.                                          
070000     IF SQLCODE = OK OR NO-MEMORY OR DEADLOCK OR NOT-FOUND                
070100        CONTINUE                                                          
070200     ELSE                                                                 
           CANCEL "CALLSQLE"
070300        CALL "CALLSQLE" USING SQLCA PAR-ERR AREA-HL AREA-SI.              
070400 TEST-ERR-EX.                                                             
070500     EXIT.                                                                
070600*                                                                         
070700*                                                                         
070800 S-COMMIT.                                                                  
070900     EXEC SQL                                                             
071000        COMMIT WORK                                                       
071100     END-EXEC.                                                            
071200     MOVE "COMMIT WORK" TO ER-DESCRIZIONE                                 
071300     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
071400 S-COMMIT-EX.                                                               
071500     EXIT.                                                                
071600*                                                                         
071700*                                                                         
071800*                                                                         
071900 METTI-RIGA-X-LASER.                                                      
072000     MOVE "P" TO COMANDO.                                                 
072100     MOVE TABULATO TO N-STAMPANTE.                                        
072200     MOVE 1 TO NUM-RIGA.                                                  
072300     MOVE SERV-SEQ-ESC-1 TO DETT-RIGA-SEQ (1).                            
072400     MOVE SERV-SEQ-ESC-2 TO DETT-RIGA-SEQ (2).                            
072500     MOVE SERV-SEQ-ESC-3 TO DETT-RIGA-SEQ (3).                            
072600     MOVE RIGA-SEQ-ESC-INT TO DATI-RIGA-STAMPA.                           
072700     PERFORM DEF-TIPO-SCRIVENTE                                           
072800             THRU EX-DEF-TIPO-SCRIVENTE.                                  
      *conv
072900*    MOVE 0 TO RIGA-CORRENTE.                                             
      *conv-end
073000 EX-METTI-RIGA-X-LASER.                                                   
073100     EXIT.                                                                
073200*                                                                         
073300*                                                                         
073400 IN-PAR-CLASSE.                                                           
073500     MOVE 0 TO QT-STATO OF PARTAB-CLASSI                                  
073600               QT-NUM-ELEM-EFF OF PARTAB-CLASSI                           
073700               QT-INDEX-ELEM OF PARTAB-CLASSI.                            
073800     MOVE N-MAX-CL TO QT-NUM-ELEM-MAX OF PARTAB-CLASSI.                   
073900     MOVE 26 TO QT-LL-ELEM OF PARTAB-CLASSI.                              
074000     MOVE 1 TO QT-ADDR-KEY OF PARTAB-CLASSI.                              
074100     MOVE 2 TO QT-LL-KEY OF PARTAB-CLASSI.                                
074200 EX-IN-PAR-CLASSE.                                                        
074300     EXIT.                                                                
074400*                                                                         
074500*                                                                         
074600 CARICA-QTA-CLASSE.                                                       
074700     MOVE ART-TAB-LETTI(IND-C) TO ART-ELEM-LETTI. 
           DISPLAY ELEM-ART 'ELEM-ART'                         
074800     COMPUTE C-MAT-A-BARRE-RID = ELEM-ART * 10.                           
074900     MOVE CLASSE OF C-MAT-A-BARRE TO CLASSE-ELEM.                         
075000     MOVE "K2" TO QT-FUNZIONE OF PARTAB-CLASSI.                           
           CANCEL "QTABEL"
075100     CALL "QTABEL" USING PARTAB-CLASSI TABELLA-CLASSI-LETTE               
075200                         CLASSI-ELEM-LETTE.                               
075300     IF QT-STATO OF PARTAB-CLASSI NOT = 0                                 
075400       MOVE 0 TO QTA-TOT-ELEM QTA-NO-GIAC-ELEM                            
075500                PREZZO-TOT-ELEM PREZZO-NO-GIAC-ELEM                       
075600       MOVE "K1" TO QT-FUNZIONE OF PARTAB-CLASSI                          
           CANCEL "QTABEL"
075700       CALL "QTABEL" USING PARTAB-CLASSI TABELLA-CLASSI-LETTE             
075800                           CLASSI-ELEM-LETTE                              
075900       IF QT-STATO OF PARTAB-CLASSI NOT = 0                               
076000         DISPLAY "ERRORE ins TABELLA-CLASSI-LETTE"                        
076100         MOVE "STOP" TO CHIUDI-PROGRAMMA                                  
076200         GO TO EX-CARICA-QTA-CLASSE.                                      
076300*                                                                         
076400     MOVE CLASSI-TAB-LETTE(QT-INDEX-ELEM OF PARTAB-CLASSI)                
076500         TO CLASSI-ELEM-LETTE.                                            
076600     PERFORM VARYING IND-T FROM 1 BY 1                                    
076700            UNTIL IND-T > NTG-NTG        
             DISPLAY ' QTA-TAGLIA-ELEM(IND-T)' 
                QTA-TAGLIA-ELEM(IND-T)
                ' CMAT '   ELEM-ART                                     
076800       ADD QTA-TAGLIA-ELEM(IND-T) TO QTA-TOT-ELEM                         
076900       COMPUTE PREZZO-TOT-ELEM = PREZZO-TOT-ELEM +                        
077000            (PREZZO-ELEM * QTA-TAGLIA-ELEM(IND-T) )                       
077100     END-PERFORM.                                                         
077200     MOVE CLASSI-ELEM-LETTE TO CLASSI-TAB-LETTE(QT-INDEX-ELEM             
077300                                  OF PARTAB-CLASSI).                      
077400 EX-CARICA-QTA-CLASSE.                                                    
077500     EXIT.                                                                
077600*                                                                         
077700*                                                                         
077800 INTESTA-PAGINA.                                                          
077900     ADD 1 TO CONTA-PAGINE.                                               
078000*                                                                         
078100     MOVE SPACE TO DATI-RIGA-STAMPA.                                      
078110*MAG6/7*                                                                  
078120     MOVE "mag " TO D-MAG-T.                                              
078130     MOVE MAG-INPUT-R TO MAG-T.                                           
078200     MOVE W-FORMATO-GG-MMM-AAAA TO DATA-T.                                
078300     MOVE W-NUM-TERM TO NUM-PAG-T.                                        
078400     MOVE "term " TO D-PAG-T.                                             
078500*                                                                         
078600     MOVE "P" TO COMANDO.                                                 
078700     MOVE 2 TO NUM-RIGA.                                                  
078800     MOVE 0 TO N-STAMPANTE.                                               
078900     PERFORM DEF-TIPO-SCRIVENTE THRU                                      
079000              EX-DEF-TIPO-SCRIVENTE.                                      
079100*                                                                         
079200     MOVE SPACE TO DATI-RIGA-STAMPA.                                      
079300     MOVE CONTO-IN-R TO CONTO-T.                                          
079400     MOVE D-CONTO-MEM TO D-CONTO-T.                                       
079500     MOVE "pag. " TO D-PAG-T.                                             
079600     MOVE CONTA-PAGINE TO NUM-PAG-T.                                      
079700*                                                                         
079800     MOVE "P" TO COMANDO.                                                 
079900     MOVE 3 TO NUM-RIGA.                                                  
080000     MOVE 0 TO N-STAMPANTE.                                               
080100     PERFORM DEF-TIPO-SCRIVENTE THRU                                      
080200              EX-DEF-TIPO-SCRIVENTE.                                      
080300*                                                                         
080400     MOVE SPACE TO DATI-RIGA-STAMPA.                                      
080500     MOVE 5 TO NUM-RIGA.                                                  
080600     MOVE "  Venduti" TO QTA-TOT-T.                                       
080700     MOVE "   Manca " TO QTA-TOT-T-NO-G.                                  
080710     MOVE "Giacenza" TO PREZZO-T-NO-G.                                    
080800     MOVE "   Totale" TO QTA-TOT-T-GEN.                                   
080900     PERFORM DEF-TIPO-SCRIVENTE THRU                                      
081000              EX-DEF-TIPO-SCRIVENTE.                                      
081100*                                                                         
081200     MOVE SPACE TO DATI-RIGA-STAMPA.                                      
081300     MOVE 6 TO NUM-RIGA.                                                  
081400     MOVE "Classe" TO D-CLASSE-R.                                         
081500     MOVE "     Capi" TO QTA-TOT-T                                        
081600                                                                          
081700                  QTA-TOT-T-NO-G                                          
081800                  QTA-TOT-T-GEN.                                          
081900     MOVE "       Euro" TO PREZZO-T                                       
082000                  PREZZO-T-NO-G                                           
082100                  PREZZO-T-GEN.                                           
082110*EURO*                                                                    
082120     MOVE " Lit. Ven." TO PREUR-T.                                        
082130     MOVE "Lit. no G." TO PREUR-T-NO-G.                                   
082140     MOVE " Lit. Tot." TO PREUR-T-GEN.                                    
082150*                                                                         
082200     PERFORM DEF-TIPO-SCRIVENTE THRU                                      
082300              EX-DEF-TIPO-SCRIVENTE.                                      
082400*                                                                         
082500     MOVE SPACE TO DATI-RIGA-STAMPA.                                      
082600     MOVE 7 TO NUM-RIGA.                                                  
082700     MOVE ALL "-" TO    CLASSE-T D-CLASSE-R                               
082800                   QTA-TOT-T QTA-TOT-T-NO-G QTA-TOT-T-GEN                 
082900                   PREZZO-T PREZZO-T-NO-G PREZZO-T-GEN                    
082910*EURO*                                                                    
082920                   PREUR-T PREUR-T-NO-G PREUR-T-GEN.                      
082930*                                                                         
083000     PERFORM DEF-TIPO-SCRIVENTE THRU                                      
083100              EX-DEF-TIPO-SCRIVENTE.                                      
083200*                                                                         
083300     MOVE 7 TO CONTA-RIGHE.                                               
083400 EX-INTESTA-PAGINA.                                                       
083500     EXIT.                                                                
083600*                                                                         
083700*                                                                         
083800 CARICA-TABELLA-CLASSI.                                                   
      *NO-DATGE*                                                        inizio
075330*     MOVE "ITAL" TO CLASSI-LINGUA. 
           MOVE "MM" TO CC-SOCIETA.
      *NO-DATGE*                                                        fine
084000*                                                                         
084100     PERFORM WITH TEST AFTER                                              
084200             UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                   
084300        PERFORM BEGIN-RC THRU BEGIN-RC-EX                                 
084400        IF SQLCODE = OK                                                   
084500           PERFORM SE-SELECT-CLASSI                                       
084600                  THRU SE-SELECT-CLASSI-EX                                
084700        END-IF                                                            
084800     END-PERFORM.                                                         
084900     MOVE SQLERRD (3) TO NUM-ELEM-MAX-CLASSI.                             
085000     PERFORM S-COMMIT THRU S-COMMIT-EX.                                       
085100 EX-CARICA-TABELLA-CLASSI.                                                
085200     EXIT.                                                                
085300*                                                                         
085400* 
                                                                        
      *NO-DATGE*                                                        inizio
085500* SE-SELECT-CLASSI.                                                        
085600*     EXEC SQL                                                             
085700*     BULK SELECT      CLASSE,                                             
085800*                      DESCR                                               
085900*          INTO        :TABELLA-CLASSI                                     
086000*          FROM PF.CLASSI                                                  
086100*          WHERE LINGUA = :CLASSI-LINGUA                                   
086200*          ORDER BY CLASSE                                                 
086300*     END-EXEC                                                             
086400*     MOVE "SELECT CLASSI    " TO ER-DESCRIZIONE                           
086500*     IF SQLCODE NOT = MULTIPLE-ROWS                                       
086600*        PERFORM TEST-ERR THRU TEST-ERR-EX.                                
086700* SE-SELECT-CLASSI-EX.                                                     
086800*     EXIT.                                                                
086900*                                                                         
085500 SE-SELECT-CLASSI.                                                        
085600     EXEC SQL                                                             
085700     BULK SELECT      classe,                                             
085800                      desc_classe                                               
085900          INTO        :TABELLA-CLASSI                                     
086000          FROM anagrafica_classi_dbg                                                  
086100          WHERE societa = :CC-SOCIETA                                   
086200          ORDER BY classe                                                 
086300     END-EXEC                                                             
086400     MOVE "SELECT anagrafica_classi_dbg    " TO ER-DESCRIZIONE                           
086500     IF SQLCODE NOT = MULTIPLE-ROWS                                       
086600        PERFORM TEST-ERR THRU TEST-ERR-EX.                                
086700 SE-SELECT-CLASSI-EX.                                                     
086800     EXIT.                                                                
086900*                                                                         
      *NO-DATGE*                                                        fine

      *NO-DATGE*                                                        inizio
      ***********connessione a DATGE*******************      
020300 S-SET-2.                                                                 
020400        EXEC SQL                                                          
020500           SET CONNECTION 'DB2'                                           
020600        END-EXEC.                                                         
020700 S-SET-2-EX.                                                              
020800     EXIT.
      *
      ***********connessione a MAGAUTO*****************       
020300 S-SET-1.                                                                 
020400        EXEC SQL                                                          
020500           SET CONNECTION 'DB1'                                           
020600        END-EXEC.                                                         
020700 S-SET-1-EX.                                                              
020800     EXIT.                             
      *          
      *NO-DATGE*                                                        fine


087000*                                                                         
087100*                                                                         
087200 IMPOSTA-DESCRIZIONE.                                                     
087300     MOVE CLASSE-ELEM TO CLASSE-2-COM.                                    
087400     PERFORM NIENTE THRU EX-NIENTE                                        
087500           VARYING W-INDICE-5 FROM 1 BY 1                                 
087600                UNTIL W-INDICE-5 >                                        
087700                  NUM-ELEM-MAX-CLASSI OR                                  
087800                    CLASSE-CL (W-INDICE-5) =                              
087900                        CLASSE-COM.                                       
088000     IF W-INDICE-5 > NUM-ELEM-MAX-CLASSI                                  
088100       MOVE SPACES TO D-CLASSE-R                                          
088200     ELSE                                                                 
088300       MOVE DESCRIZIONE-CL(W-INDICE-5)                                    
088400           TO D-CLASSE-R.                                                 
088500 EX-IMPOSTA-DESCRIZIONE.                                                  
088600     EXIT.                                                                
088700*                                                                         
088800*                                                                         
088900 NIENTE.                                                                  
089000 EX-NIENTE.                                                               
089100     EXIT.                                                                
089200**                                                                        
089300*                                                                         
089400*************************************************                         
