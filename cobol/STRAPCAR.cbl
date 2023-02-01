001000 IDENTIFICATION DIVISION.                                                 
001100 PROGRAM-ID.         STRAPCAR.                                            
001200*                                                                         
001300*MP909*  05/09/97                                                         
         
002200*   
      *DMAT*          20/06/06
      *   stampa descriz. modello da ANAMAT: 
      *     1) e' capo conto fornit. e non esiste MATFOR;
      *     2) e' capo conto non fornit.
      *
      *MAG3*          26/09/08
      *   per trasf. a mag 3 tratta solo i carichi
      *   (carichi e scarichi hanno stesso rif. int.)
      *   e non fa display (rovinano rapportino di READTR3)
      *
      *NUCO*        29/04/09
      *     aggiunte nuove colonna a SITPEZ; 
      *     modificato schema di Zanardo per farle gestire da
      *     vecchie intefacce IMAGE
      *ESTETA*     20/11/18
      *      estensione taglie
      *                                                                         
002300 ENVIRONMENT DIVISION.                                                    
002400 CONFIGURATION SECTION.                                                   
002500 SOURCE-COMPUTER.    HP-3000.                                             
002600 OBJECT-COMPUTER.    HP-3000.                                             
002700 SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                              
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.       
      *ESTETA* 
       COPY NTG.                                                                                                                                                                           
002900 77 JRUNC   PIC S9(4) COMP-5 VALUE 0.
       77 I PIC S9(4) COMP.
003000*                                                                         
003200*                                                                         

      *
003400 01  AREA-MOVMAG.                                                         
003500  05 REC-MOVMAG  COPY  YMOVMAG.                                           
003600*                                                                         
003700*     
      *NUCO*
003800* 01  AREA-REC-SET         PIC X(120). 
003800 01  AREA-REC-SET         PIC X(256).
003900*                                                                        
004000 01  AREA-SITPEZ  REDEFINES  AREA-REC-SET.                                
004100  05 REC-SITPEZ  COPY  YSITPEZ.                                           
004200 01  AREA-MATFOR  REDEFINES  AREA-REC-SET.                                
004300  05 REC-MATFOR  COPY  YMATFOR.                                           
004400  05 FILLER               PIC XX.                                         
004500*                                                                         
004600*                                                                         
004700 01  AREA-ANACON  REDEFINES  AREA-REC-SET.                                
004800  05 REC-ANACON  COPY  YANACON.                                           
004900  05 FILLER               PIC X(20).                                      
005000*                                                                         
005100*                                                                         
005200 01  AREA-ANAMAT  REDEFINES  AREA-REC-SET.                                
005300  05 REC-ANAMAT  COPY  YANAMAT.                                           
005400  05 FILLER               PIC X(26).                                      
005500*                                                                         
005600 01 AREA-COMPOS         REDEFINES AREA-REC-SET.                           
005700  05 REC-COMPOS  COPY YCOMPOS.                                            
005800*
      *
044800 01  NOME-FILE          PIC X(12).                                        
044900 01 FILLER REDEFINES NOME-FILE.                                                                                                               
045100    10 NOME-FILE-1        PIC X(4).                                       
045200    10 NUM-TERM-FILE      PIC 999.                                        
045300    10 RESTO-FILE         PIC X(5).            
      *
005900 01  PAR-PRINT.                                                           
006000  05 STATO-PP             PIC S9(4)  COMP.                                
006100  05 LL-RIGA-PP           PIC S9(4)  COMP.                                
006200  05 N-MAX-RIGHE-PP       PIC S9(4)  COMP.                                
006300  05 FLAGS-ROUTINE-PP     PIC S9(4)  COMP.                                
006400  05 NUM-FILE-ID-PP       PIC S9(4)  COMP.                                
006500  05 NOME-FILE-PP         PIC X(12).                                      
006600*                                                                         
006700*                                                                         
006800 01  RIGA-PP.                                                             
006900  05 N-STAMPANTE-PP       PIC 9.                                          
007000  05 COMANDO-PP           PIC X.                                          
007100  05 N-RIGA-STAMPA-PP     PIC S9(4)  COMP.                                
007200  05 DATI-RIGA-PP         PIC X(132).                                     
007300*                                                                         
007400*                                                                         
007500 01 BUFFER.                                                               
007600  05 N-BUF          PIC S9(4) COMP VALUE 37.                              
007700  05 FILLER         PIC XX.                                               
007800  05 FILLER         PIC X(5120).                                          
007900*                                                                         
008000*                                                                         
008100*                                                                         
008200 01  RIF-COM.                                                             
008400  05 DATA-RIF.                                                            
008500   10 AA-RIF              PIC 99.                                         
008600   10 MM-RIF              PIC 99.                                         
008700   10 GG-RIF              PIC 99.                                         
008800  05 DATA-RIF-RID  REDEFINES  DATA-RIF  PIC 9(6).                         
008900  05 NUM-RIF              PIC 9(6).                                       
009000*                                                                         
009100 01  RIF-COM-RID  REDEFINES  RIF-COM  PIC 9(12).                          
009200* 
009300*                                                                         
009400 01  RIF-COM-FOR.                                                         
009500  05 FSL-RIF-FOR          PIC 9(3).                                       
009600  05 DATA-RIF-FOR.                                                        
009700   10 AA-RIF-FOR          PIC 99.                                         
009800   10 MM-RIF-FOR          PIC 99.                                         
009900   10 GG-RIF-FOR          PIC 99.                                         
010000  05 NUM-RIF-FOR          PIC 9(6).                                       
010100*                                                                         
010200 01  RIF-COM-FOR-RID  REDEFINES  RIF-COM-FOR  PIC 9(15).                  
010300*                                                                         

010100*                                                                         
010300*                                                                         
010400*                                                                         
010500 01  CONTO-TRANSITO.                                                      
010600  05 C-CONTO-TRANSITO     PIC 9(3).                                       
010700  05 S-CONTO-TRANSITO     PIC 9(5).                                       
010800*                                                                         
010900 01  CONTO-TRANS-RID  REDEFINES  CONTO-TRANSITO  PIC 9(8).                
011000*                                                                         
011100*                                                                         

       01 PARGEN  COPY QPARGEN.
       01 PARDATA COPY QPARDATS.
      *
012000*                                                                         
012100 01  CAMPI-COMODO-RIGA.                                                   
012200  05 RIGA-FINCATA-1       PIC X(66)  VALUE                                
012300     "|  |                 |                                              
012400-    " |    ".                                                            
012500  05 RIGA-FINCATA-2       PIC X(66)  VALUE                                
012600     "  |      |             |               |   |           |            
012700-    "     |".                                                            
012800  05 ZONA-ST-TAGLIE       PIC X(39)  VALUE                                
012900     "T-1|T-2|T-3|T-4|T-5|T-6|T-7|T-8|T-9|T10".                                   
013000  05 RIGA-FINCATA-A1.                                                     
013100   10 FILLER           PIC X(65) VALUE                                    
013200     "|N.|                 |                                              
013300-    " |   ".                                                             
013400   10 FILLER            PIC X(67) VALUE                                   
013500          "        QUANTITA PER TAGLIE           |  COSTO  | QTA |
      -    "  TOTALE   |".         
013600  05 RIGA-FINCATA-A2.                                                     
013700   10 FILLER               PIC X(65) VALUE                                
013800     "|RI|MODELLO ARTIC COL|     C O D I C I   F O R N I T O R E          
013900-    " |---".                                                             
014000   10 FILLER                  PIC X(67) VALUE                             
014100       "--------------------------------------|         |     |   
      -    "        |".         
014200  05 RIGA-FINCATA-A3.                                                     
014300   10 FILLER                PIC X(65) VALUE                               
014400     "|GA|                 |                                              
014500-    " |T-1".                                                             
014600   10 FILLER               PIC X(67) VALUE                                
014700       "|T-2|T-3|T-4|T-5|T-6|T-7|T-8|T-9|T-10 |UNITARIO |CAPI |  I
      -    "MPORTO  |".         
014800  05 RIGA-TOTALI-GEN.                                                     
014900   10 FILLER                  PIC X(65) VALUE                             
015000     "|                                                                   
015100-    " |   ".                                                             
015200   10 FILLER                 PIC X(67) VALUE                              
015300     "   T O T A L I     G E N E R A L I              |     |     
      -    "      |".         
015400  05 RIGA-DETTAGLIO-TAGLIE.                                               
015500   10 FILLER                PIC X(65) VALUE                               
015600     "|  |                 |                                              
015700-    " |   ".                                                             
015800   10 FILLER                 PIC X(67) VALUE                              
015900       "|   |   |   |   |   |   |   |   |     |         |     |    
      -     "        |".         
016000*                                                                         
016100  05 RIGA-FINCATA-A21.                                                    
016200   10 FILLER               PIC X(65) VALUE                                
016300     "|   Composizione                                                    
016400-    " | Al".                                                             
016500   10 FILLER                  PIC X(67) VALUE                             
016600       "iquota I.V.A.                                             
      -     "        |".         
016700*                                                                         
016800 01  RIGA-TESTATA.                                                        
016900  05 CONTO-ST             PIC X(8).                                       
017000  05 C-CONTO-ST           PIC Z(3).                                       
017100  05 FILLER               PIC X.                                          
017200  05 S-CONTO-ST           PIC 9(5).                                       
017300  05 RAGIONE-SOCIALE-ST   PIC X(20).                                      
017400  05 RA-SO-CONTO-ST       PIC X(34).                                      
017500  05 NUMERO-BOLLA-ST      PIC X(17).                                      
017600  05 NR-BOLLA-ST          PIC Z(6).                                       
017700  05 NR-BOLLA-RID-ST  REDEFINES  NR-BOLLA-ST  PIC X(6).                   
017800  05 DATA-BOLLA-ST        PIC X(15).                                      
017900  05 D-BOLLA-ST.                                                          
018000   10 GG-BOLLA-ST         PIC 99.                                         
018100   10 BARRA1-ST           PIC X.                                          
018200   10 MM-BOLLA-ST         PIC 99.                                         
018300   10 BARRA2-ST           PIC X.                                          
018400   10 AA-BOLLA-ST         PIC 99.                                         
018500  05 D-BOLLA-RID-ST  REDEFINES  D-BOLLA-ST  PIC X(8).                     
018600  05 FOGLIO-ST            PIC X(12).                                      
018700  05 CONTA-FOGLI-ST       PIC Z(2).                                       
018800  05 FILLER               PIC X.                                          
018900*                                                                         
019000*                                                                         
019100 01  RIGA-TESTATA-RID  REDEFINES  RIGA-TESTATA.                           
019200  05 NOME-ACCT-ST         PIC X(8).                                       
019300  05 FILLER               PIC X(15).                                      
019400  05 BOLLA-E-MAGAZZ-ST    PIC X(37).                                      
019500*MP909*                                                                   
019600  05 MAGAZZINO-ST         PIC Z(3).                                       
019700  05 FILLER               PIC X(92).                                      
019800*                                                                         
019900 01 RIGA-COMPOS-IVA REDEFINES RIGA-TESTATA.                               
020000  05 FILLER             PIC X(22).                                        
020100  05 DETT-COMPOS  OCCURS 6.                                               
020200   10 PERC-COMPOS-ST     PIC Z(4).                                        
020300   10 COMP-COMPOS-ST     PIC XX.                                          
020400  05 FILLER             PIC X(22).                                        
020500  05 ALIQ-IVA-ST        PIC 99.                                           
020600*                                                                         
020700 01  RIGA-DETTAGLIO  REDEFINES  RIGA-TESTATA.                             
020800  05 FILLER               PIC X.                                          
020900  05 NR-RIGA-DETT         PIC Z(2).                                       
021000  05 NR-RIGA-RID-DETT  REDEFINES  NR-RIGA-DETT  PIC X(2).                 
021100  05 FILLER               PIC X.                                          
021200  05 C-MAT-DETT.                                                          
021300   10 MODELLO-DETT        PIC Z(7).                                       
021400   10 ARTICOLO-DETT       PIC Z(6).                                       
021500   10 COLORE-DETT         PIC Z(4).                                       
021600  05 C-MAT-RID-DETT  REDEFINES  C-MAT-DETT  PIC X(17).                    
021700  05 FILLER               PIC X.                                          
021800  05 CODICI-FORNITORE-DETT PIC X(39).                                     
021900  05 CODICI-FORNITORE-RID-DETT REDEFINES CODICI-FORNITORE-DETT.           
022000   10 DESCRIZIONE-DETT    PIC X(18).                                      
022100   10 FILLER              PIC X(16).                                      
022200   10 COL-FOR-DETT        PIC X(5).                                       
022300  05 CODICI-FOR-COMP-DETT  REDEFINES  CODICI-FORNITORE-DETT.              
022400   10 DESCR-COMP-DETT     PIC X(24).                                      
022500   10 FILLER              PIC X(15).                                      
022600  05 FILLER               PIC X. 
      *NRPEZZE*
022700  05 NR-PEZZA-DETT        PIC Z(7).                                       
022800  05 NR-PEZZA-RID-DETT  REDEFINES  NR-PEZZA-DETT  PIC X(7).               
022900  05 FILLER               PIC X.                                          
023000  05 NR-PEZZA-FOR-DETT    PIC Z(7).                                       
023100  05 NR-P-F-RID-DETT  REDEFINES  NR-PEZZA-FOR-DETT  PIC X(7).                                               
022700*  05 NR-PEZZA-DETT        PIC Z(6).                                       
022800*  05 NR-PEZZA-RID-DETT  REDEFINES  NR-PEZZA-DETT  PIC X(6).               
022900*  05 FILLER               PIC X.                                          
023000*  05 NR-PEZZA-FOR-DETT    PIC Z(6).                                       
023100*  05 NR-P-F-RID-DETT  REDEFINES  NR-PEZZA-FOR-DETT  PIC X(6). 
      *NRPEZZE*             
023200  05 FILLER               PIC X.                                          
023300  05 UN-MIS-DETT          PIC X(4).                                       
023400  05 QTA-DETT             PIC Z(6),99  BLANK WHEN ZERO.                   
023500  05 QTA-RID-DETT  REDEFINES  QTA-DETT  PIC X(9).                         
023600  05 FILLER               PIC X.                                          
023610  05 FILLER                   PIC X(4).                                   
023620  05 CAUSALE-DETT             PIC X(7).                                   
023630  05 FILLER                   PIC X(5).                                   
023640  05 SEGNO-DETT               PIC XXX.                                    
023650  05 FILLER                   PIC X.                                      
023700* 05 FILLER                   PIC X(20).                                  
023800  05 COSTO-UNIT               PIC Z(3)9,9(6) BLANK WHEN ZERO.             
023900  05 COSTO-UNIT-RID REDEFINES COSTO-UNIT     PIC X(11).                   
024000  05 FILLER                   PIC X.                                      
024100  05 PREZZO-DETT          PIC Z(5)9,99  BLANK WHEN ZERO.                  
024200  05 IMPORTO-TOT-DETT  REDEFINES  PREZZO-DETT  PIC Z(9).                  
024300  05 PREZZO-RID-DETT  REDEFINES  PREZZO-DETT  PIC X(9).                   
024400  05 FILLER               PIC X.                                          
024500*                                                                         
024600*                                                                         
024700 01  RIGA-DETT-2 REDEFINES RIGA-TESTATA.                                  
024800  05 FILLER                    PIC X(62).                                 
024900  05 QTA-TAGLIE-DETT.                                                     
025000   10 QTA-TGL-DETT  COPY NTGOCCURS.                                            
025100    15 QTA-T-DETT         PIC -(3).                                       
025200    15 FILLER             PIC X.                                          
025300  05 QTA-TAGLIE-RID-DETT  REDEFINES  QTA-TAGLIE-DETT.                     
025400   10 ZONA-UTILE-DETT     PIC X(31).                                      
025500   10 FILLER              PIC X.                                          
025600*EURO*                                                                    
025700  05 DETTAGLIO-PF.                                                        
025800*                                                                         
025900* 05 FILLER                   PIC XX.                                     
026000   10 PREZZO-DETT-I            PIC Z(5)9,99 BLANK WHEN ZERO.              
026100   10 FILLER                   PIC XX.                                    
026200   10 TOT-RIGA                 PIC ZZZZ.                                  
026300   10 FILLER                   PIC XX.                                    
026400   10 TOT-IMP-RIGA             PIC -(7),99 BLANK WHEN ZERO.               
026500   10 FILLER                   PIC XX.                                    
026600   10 C-OP-DETT                PIC X(4).                                  
026700*EURO*                                                                    
026800  05 DETTAGLIO-MP REDEFINES DETTAGLIO-PF.                                 
026900   10 FILLER    PIC X(15).                                                
027000   10 FINC-MP  PIC X.                                                     
027100   10 FILLER     PIC X(8).                                                
027200   10 PRZ-TOT-MP  PIC Z(6),Z(6).                                          
027300   10 FILLER    PIC X.                                                    
027400*  
       01 RIGA-DETT-CNTR REDEFINES RIGA-TESTATA.
         05 FILLER PIC X(100).
         05 DETT-CNTR  PIC X(20).
027500*                                                                         
027600 01  RIGA-TOTALI REDEFINES RIGA-TESTATA.                                  
027700  05  FILLER                PIC X(115).                                   
027800  05  TOT-N-CAPI            PIC Z(4).                                     
027900  05  FILLER                PIC X.                                        
028000  05  TOT-IMP-GEN           PIC Z(6)9,99 BLANK WHEN ZERO.                 
028100*                                                                         
028200*                                                                         
028300 01  RIGA-SPEZZATA  REDEFINES  RIGA-TESTATA.                              
028400  05 PRIMO-PEZZO          PIC X(66).                                      
028500  05 PRIMO-PEZZO-RID  REDEFINES  PRIMO-PEZZO.                             
028600   10 FINC1               PIC X.                                          
028700   10 FILLER              PIC X(65).                                      
028800  05 SECONDO-PEZZO        PIC X(66).                                      
028900  05 SECONDO-PEZZO-RID  REDEFINES  SECONDO-PEZZO.                         
029000   10 FILLER              PIC X(23).                                      
029100   10 FINC2               PIC X.                                          
029200   10 FILLER              PIC X(31).                                      
029300   10 FINC3               PIC X.                                          
029400   10 FILLER              PIC X(9).                                       
029500   10 FINC4               PIC X.                                          
029600*                                                                         
029700 01  RIGA-SPEZZATA-2 REDEFINES RIGA-SPEZZATA.                             
029800  05 R-SPEZZATA                  PIC X(132).                              
029900*                                                                         
030000 01  AREA-TRANSITO  COPY  DANCODMT.                                       
030100*                                                                         
030200*                                                                         
030300 01  CAMPI-UTILITY.                                                       
030400  05 CONTA-FOGLI          PIC S9(4)  COMP.                                
030500  05 CONTARIGA            PIC S9(4)  COMP.                                                              
030700  05 I-TAGLIE             PIC S9(4)  COMP.                                
030800  05 STATO-DISPLAY        PIC ----.                                       
030900  05 MAX-RIGHE-TABULATO   PIC S9(4)  COMP  VALUE  54. 
        05 MAX-RIGHE-X-CNTR     PIC S9(4) COMP VALUE 42.
031000  05 D-CONTO-MEM          PIC X(24).                                      
031100*EURO*                                                                    
031200* 05 PREZZO-COM           PIC S9(9)V99  COMP-3.                           
031300  05 PREZZO-COM           PIC S9(18) COMP.                                
031400  05 TOT-PREZZO           PIC S9(18)  COMP.                               
031500  05 COMODO-QTA           PIC S9(6)V99 COMP-3.                            
031600  05 FLAG1-MEM            PIC X.                                          
031700  05 TOT-CAPI                PIC S9(4) COMP.                              
031800  05 PREZZO-NEW                    PIC S9(9)V99 COMP-3.                   
031900  05 IVA-MEM                       PIC S9(4) COMP.                        
032000*                                                                         
032100*
042400*                                                                         
042500 01 RIGHE-STR-CNTR.                                                       
042600  05 FILLER        PIC X(20) VALUE " __________________ ".                
042700  05 FILLER        PIC X(20) VALUE "! CONTROLLO  LISTE !".                
042800  05 FILLER        PIC X(20) VALUE "!     DI CARICO    !".                
042900  05 FILLER        PIC X(20) VALUE "!------------------!".                
043000  05 FILLER        PIC X(20) VALUE "! Fattura num.     !".                
043100  05 FILLER        PIC X(20) VALUE "!                  !".                
043200  05 FILLER        PIC X(20) VALUE "!__________________!".                
043300  05 FILLER        PIC X(20) VALUE "! del              !".                
043400  05 FILLER        PIC X(20) VALUE "!                  !".                
043500  05 FILLER        PIC X(20) VALUE "!__________________!".                
043600  05 FILLER        PIC X(20) VALUE "! Protocollo  IVA  !".                
043700  05 FILLER        PIC X(20) VALUE "!                  !".                
043800  05 FILLER        PIC X(20) VALUE "!__________________!".                
043900*                                                                         
044000 01 FILLER REDEFINES RIGHE-STR-CNTR.                                      
044100  05 RIGA-CNTR   PIC X(20) OCCURS 13.                                     
044200*  
       01 I-RIGHE-X-CNTR PIC S9(4) COMP VALUE 13.
032200*
      *
       01 D-MAT-MEM    PIC X(24).
      *
032300*                                                                         
013600 LINKAGE SECTION.                                                         
013700*                                                                         
013800 01 W-COMMON  COPY WCOMMONW.                                              
013900* 
       01 L-MAGAZZINO    PIC 999.
      *
       01 L-BOLLA       PIC 9(6).                             
       01 L-DATA-BOLLA  PIC 9(6).             
014000*   
      *
014100 PROCEDURE DIVISION USING W-COMMON L-MAGAZZINO
                                L-BOLLA  L-DATA-BOLLA.                                       
014200*                                                                                                      
021900*                                                                         
033400     PERFORM APRI-STAMPA THRU EX-APRI-STAMPA.                             
033501*                                                                                                                  
033600     PERFORM ELABORA-STAMPA THRU EX-ELABORA-STAMPA   .
           PERFORM CHIUDI-TUTTO THRU EX-CHIUDI-TUTTO.                                                                              
029700     EXIT PROGRAM.                                                                
      *
034700 APRI-STAMPA.                                                             
034800     MOVE 136 TO LL-RIGA-PP.                                              
034900     MOVE 3000 TO N-MAX-RIGHE-PP.  
056200     MOVE "RAPC" TO NOME-FILE-1.                                          
056300     MOVE W-NUM-TERM TO NUM-TERM-FILE.                                    
056400     MOVE ".ST" TO RESTO-FILE.                                            
056500     MOVE NOME-FILE TO NOME-FILE-PP.                                      
035100     CALL "QOLPPR"  USING  PAR-PRINT                                      
035200                            RIGA-PP BUFFER.                               
035300     MOVE 0 TO N-STAMPANTE-PP.                                            
035400     MOVE "M" TO COMANDO-PP.                                              
035500     MOVE "*** STAMPA RAPPORTO DI CARICO ***" TO DATI-RIGA-PP.               
035600     MOVE 66 TO N-RIGA-STAMPA-PP.                                         
035700     CALL "QWLPPR"  USING  PAR-PRINT                                      
035800                            RIGA-PP BUFFER.                               
035900     IF STATO-PP NOT = 0                                                  
036000             MOVE STATO-PP TO STATO-DISPLAY                               
036100             DISPLAY "ERRORE APERTURA " NOME-FILE-PP " "
                         STATO-DISPLAY               
036200*                   UPON CONSOLE                                          
036300             MOVE -9 TO W-INDICE-8                                        
                   CANCEL "QDBERROR"
036400             CALL "QDBERROR"  USING  W-COMMON.                            
036500 EX-APRI-STAMPA.                                                          
036600     EXIT.                                                                
036700*                                                                         
036800*                                                                         
036900*                                                                         
037000*                                                                         
037100 CHIUDI-TUTTO.                                                            
037200     CALL "QCLPPR"  USING  PAR-PRINT                                      
037300                            RIGA-PP BUFFER.                               
037400     IF STATO-PP NOT = 0                                                  
037500             MOVE STATO-PP TO STATO-DISPLAY                               
037600             DISPLAY "ERRORE CHIUSURA " NOME-FILE-PP " "
                       STATO-DISPLAY               
037700*                   UPON CONSOLE                                          
037800             MOVE -9 TO W-INDICE-8                                        
                   CANCEL "QDBERROR"
037900             CALL "QDBERROR"  USING  W-COMMON.                            
038200 EX-CHIUDI-TUTTO.                                                         
038300     EXIT.                                                                
038400*                                                                         
038500*                                                                         
038600 ELABORA-STAMPA.                                                                 
038700     MOVE L-BOLLA TO NUM-RIF.                             
038800     MOVE L-DATA-BOLLA TO DATA-RIF-RID.                                                                         
039000     MOVE RIF-COM-RID TO W-VALORE-CAMPO.                                  
039100     MOVE "RIF-INTR;" TO W-NOME-CAMPO.                                    
039200     MOVE "MOVMAG" TO W-NOME-DATA-SET.                                    
039300     PERFORM TTDBFIND THRU EX-TTDBFIND.                                                                  
039400     IF W-OK-IMAGE                                                                                                                  
039500         PERFORM ESEGUI-LAVORO THRU EX-ESEGUI-LAVORO.
           IF NOT W-OK-IMAGE
              DISPLAY ' ERRORE ELABORA STAMPA'.                   
039900 EX-ELABORA-STAMPA.                                                              
040000     EXIT.                                                                
040100*                                                                         
040200*                                                                         
040300 ESEGUI-LAVORO.  
040400     MOVE 1 TO CONTA-FOGLI.                                               
040500     PERFORM LEGGI-MOVMAG THRU EX-LEGGI-MOVMAG.    
040600     IF NOT W-OK-IMAGE
                   DISPLAY 'GO TO EX-ESEGUI-LAVORO'                                             
040800             GO TO EX-ESEGUI-LAVORO.  
      *
040900     PERFORM MEM-TESTATA THRU EX-MEM-TESTATA.                             
041000     MOVE 999 TO CONTARIGA.                                               
041100     MOVE 0 TO TOT-PREZZO   TOT-CAPI.                                     
041200     PERFORM TRATTA-STAMPA THRU EX-TRATTA-STAMPA UNTIL                    
041300             NOT W-OK-IMAGE.                                                                     
041501        PERFORM CHIUDI-PAGINA THRU EX-CHIUDI-PAGINA                       
041510        PERFORM TEST-CONTARIGA THRU EX-TEST-CONTARIGA                     
041530*                                                                         
041600        PERFORM STAMPA-TOT-PREZZO THRU EX-STAMPA-TOT-PREZZO.   
      *
           PERFORM TEST-CONTARIGA-CNTR THRU EX-TEST-CONTARIGA-CNTR.
080400     PERFORM STAMPA-CNTR-BOLLA THRU EX-STAMPA-CNTR-BOLLA                  
080500              VARYING I FROM 1 BY 1                     
080600                        UNTIL I > I-RIGHE-X-CNTR.                            
041700 EX-ESEGUI-LAVORO.                                                        
041800     EXIT.                                                                
041900*                                                                         
042000*                                                                         
042100 LEGGI-MOVMAG.              
           DISPLAY ' LEGGI-MOVMAG ' UPON SYSERR.                                                                     
042200     MOVE "MOVMAG" TO W-NOME-DATA-SET.                                    
042300     MOVE 5 TO W-MODO.                                                    
042400     PERFORM TTDBGET-MOVMAG THRU EX-TTDBGET-MOVMAG.
           DISPLAY ' EX-LEGGI-MOVMAG ' UPON SYSERR.                       
042500 EX-LEGGI-MOVMAG.                                                         
042600     EXIT.                                                                
042700*                                                                         
042800*                                                                         
042900 MEM-TESTATA.                                                             
043000     MOVE CONTO OF REC-MOVMAG TO W-VALORE-CAMPO-W.                        
043100     MOVE CONTO OF REC-MOVMAG TO CONTO-TRANS-RID.                         
043200     MOVE "ANACON" TO W-NOME-DATA-SET.                                    
043300     MOVE 7 TO W-MODO.                                                    
043400     PERFORM TTDBGET THRU EX-TTDBGET.                                     
043500     IF NOT W-OK-IMAGE                                                    
043600          DISPLAY "CONTO:" CONTO-TRANSITO " NON EXIST IN ANACON"          
043700          MOVE ALL "*" TO D-CONTO OF REC-ANACON.                          
043800     MOVE D-CONTO OF REC-ANACON TO D-CONTO-MEM.                           
043900     MOVE RIF-BOLLA-FORN OF REC-MOVMAG TO RIF-COM-FOR-RID.                
044000 EX-MEM-TESTATA.                                                          
044100     EXIT.                                                                
044200*                                                                         
044300*                                                                         
044400 TRATTA-STAMPA.     
      *MAG3*
           IF L-MAGAZZINO = 3 AND
              MAGAZZINO OF REC-MOVMAG NOT = L-MAGAZZINO
             PERFORM LEGGI-MOVMAG THRU EX-LEGGI-MOVMAG
             GO TO EX-TRATTA-STAMPA.
      *
044700     IF (MAGAZZINO OF REC-MOVMAG NOT > 10 OR                              
044800          (MAGAZZINO OF REC-MOVMAG >=901 AND <= 909))                     
044900            AND C-OPE OF REC-MOVMAG NOT = "SPED"                          
045000        PERFORM CERCA-GESTIONE THRU EX-CERCA-GESTIONE                     
045100        IF GESTIONE-A-PEZZE                                               
045200           PERFORM TRATTA-PEZZE THRU EX-TRATTA-PEZZE                      
045300        END-IF                                                            
045400        PERFORM TRATTA-RIGA THRU EX-TRATTA-RIGA.                          
045500     PERFORM LEGGI-MOVMAG THRU EX-LEGGI-MOVMAG.                           
045600 EX-TRATTA-STAMPA.                                                        
045700     EXIT.                                                                
045800*                                                                         
045900*                                                                         
046000 CERCA-GESTIONE.
           DISPLAY ' CERCA GESTIONE' UPON SYSERR                                                          
046100     MOVE C-MAT OF REC-MOVMAG TO W-VALORE-CAMPO.                          
046200     MOVE C-MAT OF REC-MOVMAG TO C-MAT-TRANS-RID.                         
046300     MOVE "ANAMAT" TO W-NOME-DATA-SET.                                    
046400     MOVE 7 TO W-MODO.                                                    
046500     PERFORM TTDBGET THRU EX-TTDBGET.                                     
046600     IF NOT W-OK-IMAGE                                                    
046700          DISPLAY "C-MAT:" C-MAT-TRANSITO
                 " NON EXIST IN ANAMAT"  UPON SYSERR        
046800          MOVE 4 TO W-INDICE-8                                            
                CANCEL "QDBERROR"
046900          CALL "QDBERROR"  USING  W-COMMON.                            
047000     MOVE FLAG1 OF REC-ANAMAT TO FLAG1-MEM.                               
047100     MOVE ALIQ-IVA OF REC-ANAMAT TO IVA-MEM.  
           MOVE D-MAT OF REC-ANAMAT TO D-MAT-MEM.
047200 EX-CERCA-GESTIONE.                                                       
047300     EXIT.                                                                
047400*                                                                         
047500*                                                                         
047600 TRATTA-PEZZE.    
           DISPLAY ' CERCA SITPEZ' UPON SYSERR                                                                                                                  
047700     MOVE "C-MAT" TO W-NOME-CAMPO.                                        
047800     MOVE "SITPEZ" TO W-NOME-DATA-SET.                                    
047900     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
048000     IF NOT W-OK-IMAGE                                                    
048100          DISPLAY "C-MAT:" C-MAT-TRANSITO " NON EXIST IN SITPEZ"          
048200          MOVE 3 TO W-INDICE-8                                            
                CANCEL "QDBERROR"
048300          CALL "QDBERROR"  USING  W-COMMON.                               
048400     MOVE 5 TO W-MODO.                                                    
048500     PERFORM TTDBGET THRU EX-TTDBGET.                                     
048600     PERFORM ESEC-TRATTA-PEZZE THRU EX-ESEC-TRATTA-PEZZE UNTIL            
048700             W-FINE-CATENA.                                               
048800 EX-TRATTA-PEZZE.                                                         
048900     EXIT.                                                                
049000*                                                                         
049100*                                                                         
049200 ESEC-TRATTA-PEZZE.                                                       
049300     IF RIF-INTERNO OF REC-SITPEZ =                                       
049400                 RIF-INTERNO OF REC-MOVMAG AND                            
049500                NUMERO-RIGA OF REC-SITPEZ =                               
049600                           NUMERO-RIGA OF REC-MOVMAG                      
049700        PERFORM STAMPA-PEZZA THRU EX-STAMPA-PEZZA.                        
049800     PERFORM TTDBGET THRU EX-TTDBGET.                                     
049900 EX-ESEC-TRATTA-PEZZE.                                                    
050000     EXIT.                                                                
050100*                                                                         
050200*                                                                         
050300 STAMPA-PEZZA.                                                            
050400     PERFORM TEST-CONTARIGA THRU EX-TEST-CONTARIGA.                       
050500     MOVE RIGA-FINCATA-1 TO PRIMO-PEZZO.                                  
050600     MOVE RIGA-FINCATA-2 TO SECONDO-PEZZO.                                
050700     MOVE MODELLO OF C-MAT-TRANSITO TO MODELLO-DETT.                      
050800     MOVE ARTICOLO OF C-MAT-TRANSITO TO ARTICOLO-DETT.                    
050900     MOVE COLORE OF C-MAT-TRANSITO TO COLORE-DETT.                        
051000     MOVE NR-PEZZA OF REC-SITPEZ TO NR-PEZZA-DETT.                        
051100     MOVE NR-PEZZA-F OF REC-SITPEZ TO NR-PEZZA-FOR-DETT.                  
051200     MOVE UN-MIS-FATT OF REC-MOVMAG TO UN-MIS-DETT.                       
051300     COMPUTE COMODO-QTA = LUNGH-DICHIARATA OF REC-SITPEZ / 100.           
051400     MOVE COMODO-QTA TO QTA-DETT.                                         
051500     PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                           
051600     IF CONTARIGA = MAX-RIGHE-TABULATO                                    
051700             PERFORM CHIUDI-PAGINA THRU EX-CHIUDI-PAGINA.                 
051800 EX-STAMPA-PEZZA.                                                         
051900     EXIT.                                                                
052000*                                                                         
052100*                                                                         
052200 TEST-CONTARIGA.                                                          
052300     IF CONTARIGA > MAX-RIGHE-TABULATO                                    
052400             PERFORM STAMPA-TESTATA THRU EX-STAMPA-TESTATA                
052500             ADD 1 TO CONTA-FOGLI.                                        
052600 EX-TEST-CONTARIGA.                                                       
052700     EXIT. 
052100*                                                                         
052200 TEST-CONTARIGA-CNTR.                                                          
052300     IF CONTARIGA > MAX-RIGHE-X-CNTR                                    
052400             PERFORM STAMPA-TESTATA THRU EX-STAMPA-TESTATA                
052500             ADD 1 TO CONTA-FOGLI.                                        
052600 EX-TEST-CONTARIGA-CNTR.                                                       
052700     EXIT.                
052800*  
096300*                                                                         
096400 STAMPA-CNTR-BOLLA.                                                       
096500     MOVE RIGA-CNTR (I) TO DETT-CNTR.                             
096600     PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                             
096700 EX-STAMPA-CNTR-BOLLA.                                                    
096800     EXIT.                                                                
096900*                                                  
052900*                                                                         
053000 CHIUDI-PAGINA.                                                           
053100     IF FLAG1-MEM = "1"                                                   
053200        MOVE ALL "-" TO R-SPEZZATA                                        
053300       ELSE                                                                
053400          MOVE ALL "-" TO RIGA-SPEZZATA                                    
053500     PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                           
053600 EX-CHIUDI-PAGINA.                                                        
053700     EXIT.                                                                
053800*                                                                         
053900*                                                                         
054000 STAMPA-TESTATA.                                                          
054100     MOVE 0 TO CONTARIGA.                                                 
054200     MOVE SPACES TO RIGA-TESTATA.                                         
054300     MOVE W-SIGLA-UTENTE TO NOME-ACCT-ST.                                 
054400     MOVE " ** BOLLA di CARICO **    Magazzino: " TO                      
054500                    BOLLA-E-MAGAZZ-ST.                                    
054600     MOVE MAGAZZINO OF REC-MOVMAG TO MAGAZZINO-ST.                        
054700     MOVE "P" TO COMANDO-PP.                                              
054800     MOVE 1 TO N-RIGA-STAMPA-PP.                                          
054900     PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                           
055000     MOVE SPACES TO RIGA-TESTATA.                                         
055100     MOVE "Conto : " TO CONTO-ST.                                         
055200     MOVE "   Ragione Sociale: " TO RAGIONE-SOCIALE-ST.                   
055300     MOVE "   Numero Bolla: " TO NUMERO-BOLLA-ST.                         
055400     MOVE "   Data Bolla: " TO DATA-BOLLA-ST.                             
055500     MOVE "/" TO BARRA1-ST  BARRA2-ST.                                    
055600     MOVE C-CONTO-TRANSITO TO C-CONTO-ST.                                 
055700     MOVE S-CONTO-TRANSITO TO S-CONTO-ST.                                 
055800     MOVE D-CONTO-MEM TO RA-SO-CONTO-ST.                                  
055900     MOVE NUM-RIF TO NR-BOLLA-ST.                                         
056000     MOVE AA-RIF TO AA-BOLLA-ST.                                          
056100     MOVE MM-RIF TO MM-BOLLA-ST.                                          
056200     MOVE GG-RIF TO GG-BOLLA-ST.                                          
056300     MOVE "    Foglio: " TO FOGLIO-ST.                                    
056400     MOVE CONTA-FOGLI TO CONTA-FOGLI-ST.                                  
056500     MOVE "S" TO COMANDO-PP.                                              
056600     MOVE 0 TO N-RIGA-STAMPA-PP.                                          
056700     PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                           
056800     MOVE SPACES TO RIGA-TESTATA.                                         
056900     MOVE "Riferimento FOR: " TO NUMERO-BOLLA-ST.                         
057000     IF RIF-COM-FOR-RID = 0                                               
057100             MOVE ALL "*" TO NR-BOLLA-RID-ST                              
057200                             D-BOLLA-RID-ST                               
057300         ELSE                                                             
057400             MOVE NUM-RIF-FOR TO NR-BOLLA-ST                              
057500             MOVE AA-RIF-FOR TO AA-BOLLA-ST                               
057600             MOVE MM-RIF-FOR TO MM-BOLLA-ST                               
057700             MOVE GG-RIF-FOR TO GG-BOLLA-ST                               
057800             MOVE "/" TO BARRA1-ST  BARRA2-ST.                            
057900     PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                           
058000     IF FLAG1-MEM = "1"                                                   
058100        MOVE ALL "-" TO R-SPEZZATA                                        
058200        PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                         
058300        MOVE RIGA-FINCATA-A1 TO R-SPEZZATA                                
058400        PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                         
058500        MOVE RIGA-FINCATA-A2 TO R-SPEZZATA                                
058600        PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                         
058700        MOVE RIGA-FINCATA-A3 TO R-SPEZZATA                                
058800        PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                         
058900        MOVE ALL "-" TO R-SPEZZATA                                        
059000        PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                         
059100       ELSE                                                                
059200          MOVE ALL "-" TO RIGA-SPEZZATA                                   
059300          PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                       
059400          MOVE RIGA-FINCATA-1 TO PRIMO-PEZZO                              
059500          MOVE RIGA-FINCATA-2 TO SECONDO-PEZZO                            
059600          MOVE "N." TO NR-RIGA-RID-DETT                                   
059700          MOVE "NUMERO" TO NR-PEZZA-RID-DETT  NR-P-F-RID-DETT             
059800          MOVE "UNIT" TO UN-MIS-DETT                                      
059900          MOVE "|        " TO QTA-RID-DETT                                
060000          MOVE "  COSTO  " TO COSTO-UNIT-RID                              
060100          PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                       
060200          MOVE RIGA-FINCATA-1 TO PRIMO-PEZZO                              
060300          MOVE RIGA-FINCATA-2 TO SECONDO-PEZZO                            
060400          MOVE "RI" TO NR-RIGA-RID-DETT                                   
060500          MOVE "MODELLO ARTIC COL" TO C-MAT-RID-DETT                      
060600          MOVE "    C O D I C I    F O R N I T O R E" TO                  
060700                 CODICI-FORNITORE-DETT                                    
060800          MOVE "PEZZA " TO NR-PEZZA-RID-DETT  NR-P-F-RID-DETT             
060900          MOVE "MIS." TO UN-MIS-DETT                                      
061000          MOVE "|QUANTITA" TO QTA-RID-DETT                                
061010          MOVE "CAUSALE" TO CAUSALE-DETT                                  
061020          MOVE "+/-" TO SEGNO-DETT                                        
061100          MOVE " PREZZO  " TO PREZZO-RID-DETT                             
061200          PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                       
061300          MOVE RIGA-FINCATA-1 TO PRIMO-PEZZO                              
061400          MOVE RIGA-FINCATA-2 TO SECONDO-PEZZO                            
061500          MOVE "GA" TO NR-RIGA-RID-DETT                                   
061600          MOVE "FORNIT" TO NR-P-F-RID-DETT                                
061700          MOVE "|        " TO QTA-RID-DETT                                
061800          MOVE "UNITARIO " TO COSTO-UNIT-RID                              
061900          PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                       
062000          MOVE ALL "-" TO RIGA-SPEZZATA                                   
062100          PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                      
062200 EX-STAMPA-TESTATA.                                                       
062300     EXIT.                                                                
062400*                                                                         
062500*                                                                         
062600 CALL-QWPRINT.                                                            
062700     MOVE RIGA-TESTATA TO DATI-RIGA-PP.                                   
062800     CALL "QWLPPR"  USING  PAR-PRINT                                      
062900                            RIGA-PP BUFFER.                               
063000     IF STATO-PP NOT = 0                                                  
063100             MOVE STATO-PP TO STATO-DISPLAY  
037600             DISPLAY "ERRORE CHIUSURA " NOME-FILE-PP " "
                       STATO-DISPLAY                             
063300*                   UPON CONSOLE                                          
063400*            MOVE -9 TO W-INDICE-8                                        
063500*            CALL "QDBERROR"  USING  W-COMMON.                            
063510             STOP RUN RETURNING JRUNC.                                                    
063600     MOVE SPACES TO RIGA-TESTATA.                                         
063700     ADD 1 TO CONTARIGA.                                                  
063800 EX-CALL-QWPRINT.                                                         
063900     EXIT.                                                                
064000*                                                                         
064100*                                                                         
064200 TRATTA-RIGA.                                                             
064300     PERFORM TEST-CONTARIGA THRU EX-TEST-CONTARIGA.                       
064400     IF C-CONTO-TRANSITO = W-FORNITORI                                    
064500         PERFORM RICERCA-MATFOR THRU EX-RICERCA-MATFOR                
064600        ELSE 
      *DMAT*
      *            MOVE SPACES TO D-MAT-F OF REC-MATFOR                         
064800*                            COL-F   OF REC-MATFOR
                 MOVE ALL "*" TO COL-F   OF REC-MATFOR
                 MOVE D-MAT-MEM TO D-MAT-F OF REC-MATFOR
      *
            END-IF.
064900     IF FLAG1-MEM = "1"                                                   
065000        MOVE RIGA-DETTAGLIO-TAGLIE TO R-SPEZZATA                          
065100       ELSE                                                                
065200          MOVE RIGA-FINCATA-1 TO PRIMO-PEZZO                              
065300          MOVE RIGA-FINCATA-2 TO SECONDO-PEZZO.                           
065400     MOVE NUMERO-RIGA OF REC-MOVMAG TO NR-RIGA-DETT.                      
065500     MOVE C-OPE OF REC-MOVMAG TO C-OP-DETT.                               
065600     MOVE MODELLO OF C-MAT-TRANSITO TO MODELLO-DETT.                      
065700     MOVE ARTICOLO OF C-MAT-TRANSITO TO ARTICOLO-DETT.                    
065800     MOVE COLORE OF C-MAT-TRANSITO TO COLORE-DETT.                        
065900     IF MODELLO OF C-MAT-TRANSITO = 0                                     
066000             MOVE D-MAT-F OF REC-MATFOR TO DESCRIZIONE-DETT               
066100             MOVE COL-F OF REC-MATFOR TO COL-FOR-DETT                     
066200             MOVE UN-MIS-FATT OF REC-MOVMAG TO UN-MIS-DETT                
066300         ELSE                                                             
066400             MOVE D-MAT-F OF REC-MATFOR TO DESCR-COMP-DETT.               
066500     IF FLAG1-MEM = "1"                                                   
066600             MOVE 0 TO COMODO-QTA                                         
066700             MOVE 1 TO I-TAGLIE                                           
066800             PERFORM SCARICA-TAGLIE THRU EX-SCARICA-TAGLIE                
066900                   UNTIL I-TAGLIE > NTG-NTG                                     
067000             COMPUTE PREZZO-COM =                                         
067100*EURO*                                                                    
067200*              (PREZZO OF REC-MOVMAG * COMODO-QTA / 100)                  
067300               (PREZZO OF REC-MOVMAG * COMODO-QTA)                        
067400*                                                                         
067500             MOVE COMODO-QTA TO TOT-RIGA                                  
067600*EURO*                                                                    
067700             MOVE PREZZO OF REC-MOVMAG TO PREZZO-NEW                      
067800             COMPUTE PREZZO-DETT-I = PREZZO-NEW / 100                     
067900             COMPUTE TOT-IMP-RIGA = PREZZO-COM / 100                      
068000*            COMPUTE PREZZO-NEW = PREZZO OF REC-MOVMAG / 100              
068100*            MOVE PREZZO-NEW TO PREZZO-DETT-I                             
068200*            MOVE PREZZO-COM TO TOT-IMP-RIGA                              
068300*                                                                         
068400             ADD COMODO-QTA TO TOT-CAPI                                   
068500         ELSE                                                             
068600             COMPUTE COMODO-QTA = QUANTITA OF REC-MOVMAG / 100            
068610             IF QUANTITA OF REC-MOVMAG < 0                                
068620                MOVE " - " TO SEGNO-DETT                                  
068630               ELSE                                                       
068640                  MOVE " + " TO SEGNO-DETT                                
068650             END-IF                                                       
068660             MOVE C-OPE OF REC-MOVMAG TO CAUSALE-DETT                     
068700             COMPUTE PREZZO-COM =                                         
068800*EURO*                                                                    
068900*        (PREZZO OF REC-MOVMAG * QUANTITA OF REC-MOVMAG / 10000)          
069000         (PREZZO OF REC-MOVMAG * QUANTITA OF REC-MOVMAG)                  
069100*                                                                         
069200             MOVE COMODO-QTA TO QTA-DETT                                  
069300*EURO*                                                                    
069400             MOVE PREZZO OF REC-MOVMAG TO PREZZO-NEW                      
069500             COMPUTE COSTO-UNIT = PREZZO-NEW / 1000000                    
069600             MOVE 0 TO IMPORTO-TOT-DETT.                                  
069700*            COMPUTE IMPORTO-TOT-DETT = PREZZO-COM / 1000000.             
069800*            COMPUTE PREZZO-NEW = PREZZO OF REC-MOVMAG / 100              
069900*            MOVE PREZZO-NEW TO COSTO-UNIT                                
070000*            MOVE PREZZO-COM TO IMPORTO-TOT-DETT.                         
070100*                                                                         
070200     ADD PREZZO-COM TO TOT-PREZZO.                                        
070300     PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                           
070400     MOVE SPACES TO C-OP-DETT.                                            
070500     IF FLAG1-MEM = "1"                                                   
070600        PERFORM METTI-COMPOS-IVA THRU EX-METTI-COMPOS-IVA                 
070700        PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                        
070800     IF CONTARIGA = MAX-RIGHE-TABULATO                                    
070900             PERFORM CHIUDI-PAGINA THRU EX-CHIUDI-PAGINA.                 
071000 EX-TRATTA-RIGA.                                                          
071100     EXIT.                                                                
071200*                                                                         
071300*                                                                         
071400 RICERCA-MATFOR.                                                          
071500     MOVE "C-MAT" TO W-NOME-CAMPO.                                        
071600     MOVE "MATFOR" TO W-NOME-DATA-SET.                                    
071700     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
071800     MOVE 6 TO W-MODO.                                                    
071900     PERFORM TTDBGET THRU EX-TTDBGET.                                     
072000     PERFORM TTDBGET THRU EX-TTDBGET UNTIL                                
072100             W-INIZIO-CATENA OR                                           
072200             CONTO OF REC-MATFOR = CONTO OF REC-MOVMAG.   
      *DMAT*
072300*     IF W-INIZIO-CATENA                                                   
072400*             MOVE ALL "*" TO D-MAT-F OF REC-MATFOR                        
072500*                             COL-F   OF REC-MATFOR.                       
            IF NOT W-OK-IMAGE
                 MOVE ALL "*" TO COL-F   OF REC-MATFOR
                 MOVE D-MAT-MEM TO D-MAT-F OF REC-MATFOR
            END-IF.
072600 EX-RICERCA-MATFOR.                                                       
072700     EXIT.                                                                
072800*                                                                         
072900*                                                                         
073000 SCARICA-TAGLIE.                                                          
073100     MOVE QTA-TAGLIA OF REC-MOVMAG (I-TAGLIE) TO QTA-T-DETT               
073200                     (I-TAGLIE).                                          
073300     ADD QTA-TAGLIA OF REC-MOVMAG (I-TAGLIE) TO COMODO-QTA.               
073400     ADD 1 TO I-TAGLIE.                                                   
073500 EX-SCARICA-TAGLIE.                                                       
073600     EXIT.                                                                
073700*                                                                         
073800*                                                                         
073900 STAMPA-TOT-PREZZO.                                                       
074000     IF FLAG1-MEM = "1"                                                   
074100          MOVE SPACES TO R-SPEZZATA                                       
074200          MOVE ALL "-" TO R-SPEZZATA                                      
074300          PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                       
074400          MOVE RIGA-TOTALI-GEN TO R-SPEZZATA                              
074500*EURO*                                                                    
074600          COMPUTE TOT-IMP-GEN = TOT-PREZZO / 100                          
074700*                                                                         
074800*         MOVE TOT-PREZZO TO TOT-IMP-GEN                                  
074900          MOVE TOT-CAPI TO TOT-N-CAPI                                     
074910*SALTATO*  
      *MAG3*
           IF L-MAGAZZINO NOT = 3
074920          DISPLAY SPACE                                                   
074930          DISPLAY "TOT. CAPI: " TOT-N-CAPI                                
074940            "  //  TOT. PREZZO: " TOT-IMP-GEN                             
074941          DISPLAY SPACE 
           END-IF
074950*                                                                         
075000          PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                       
075100          MOVE ALL "-" TO R-SPEZZATA                                      
075200          PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                       
075300        ELSE                                                              
075400            MOVE SPACES TO RIGA-SPEZZATA                                  
075500*EURO*                                                                    
075600            MOVE SPACE TO RIGA-DETT-2                                     
075700            MOVE "|" TO FINC1 FINC4                                       
075800            MOVE "- I M P O R T O   T O T A L E -"                        
075900                 TO ZONA-UTILE-DETT                                       
076000            COMPUTE PRZ-TOT-MP = TOT-PREZZO / 100000000                   
076100*           MOVE TOT-PREZZO TO IMPORTO-TOT-DETT                           
076200            MOVE "|" TO FINC-MP                                           
076210*SALTATO*  
      *MAG3*
           IF L-MAGAZZINO NOT = 3
076220            DISPLAY SPACE                                                 
076230            DISPLAY                                                       
076240              "TOT. PREZZO: " PRZ-TOT-MP                                  
076241            DISPLAY SPACE  
           END-IF
076250*                                                                         
076300*                                                                         
076400            PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT                     
076500            MOVE ALL "-" TO RIGA-SPEZZATA                                 
076600            PERFORM CALL-QWPRINT THRU EX-CALL-QWPRINT.                    
076700 EX-STAMPA-TOT-PREZZO.                                                    
076800     EXIT.                                                                
076900*                                                                         
077000*                                                                         
077100 METTI-COMPOS-IVA.                                                        
077200     MOVE RIGA-FINCATA-A21 TO R-SPEZZATA.                                 
077300     MOVE IVA-MEM TO ALIQ-IVA-ST.                                         
077400     MOVE "COMPOS;" TO W-NOME-DATA-SET.                                   
077500     MOVE "C-MAT;" TO W-NOME-CAMPO.                                       
077600     MOVE 0 TO COLORE OF C-MAT-TRANSITO.                                  
077700     MOVE C-MAT-TRANS-RID TO W-VALORE-CAMPO.                              
077800     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
077900     MOVE 5 TO W-MODO.                                                    
078000     PERFORM TTDBGET THRU EX-TTDBGET.                                     
078100     IF W-OK-IMAGE                                                        
078200        PERFORM METTI-COMPOS THRU EX-METTI-COMPOS                         
078300             VARYING W-INDICE-1 FROM 1 BY 1                               
078400                 UNTIL W-INDICE-1 > 6.                                    
078500 EX-METTI-COMPOS-IVA.                                                     
078600     EXIT.                                                                
078700*                                                                         
078800*                                                                         
078900 METTI-COMPOS.                                                            
079000     MOVE PERC-COMPOS OF COMPOS-TESSUTO (W-INDICE-1) TO                   
079100            PERC-COMPOS-ST (W-INDICE-1).                                  
079200     MOVE SIGLA-FIBRA OF COMPOS-TESSUTO (W-INDICE-1) TO                   
079300            COMP-COMPOS-ST (W-INDICE-1).                                  
079400 EX-METTI-COMPOS.                                                         
079500     EXIT.                                                                
079600*                                                                         
079700*                                                                         
079800 TTDBFIND.  COPY  PDBFIND.                                                
079700*                                                                         
079900 TTDBGET-MOVMAG.  COPY  PDBGET  REPLACING                                 
080000                                AREA-REC-SET  BY  AREA-MOVMAG             
080100                                EX-TTDBGET  BY  EX-TTDBGET-MOVMAG.        
079700*                                                                         
080200 TTDBGET.  COPY  PDBGET.                                                  
