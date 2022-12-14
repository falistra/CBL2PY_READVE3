001000*CONTROL DYNAMIC,BOUNDS                                                   
001200 IDENTIFICATION DIVISION.                                                 
001300 PROGRAM-ID. READVE3.       
      *
      *   ricavato da READRST2 per gestire vendite da mag 3 (come fallati)
      *
      *
      
001400*                                                                         
001500*BOLL*                                                                    
001600*   22/02/97   aggiunto mag in input e controllo su BOLLE                 
001700*                                                                         
001800*BUDA*                                                                    
001900*   17/06/97  vendite a BUDAPEST: scrive file XESTERO                     
002000*                                 e stampa cartellini                     
002100*                                                                         
002200*  ricavato da READRSTK- con riduzione dei DISPLAY a 25 colonne           
002300*                                                                         
002400*2000*        05/08/99                                                    
002500*     tratta date a 6 cifre                                               
002600*                                                                         
002700*MAG6/7*                                                                  
002800*   20/03/00   aggiunto mag di provenienza in input                       
002900*                                                                         
003000*NOPRZ*                                                                   
003100*    11/04/00   tratta mancanza prezzo al cliente                         
003200*               come mancanza giacenza                                    
003300*                                                                         
003400*NODMAT*     06/07/00                                                     
003500*         con il messaggio "CONFERMI MANCA PREZZO ?" visualizza           
003600*         descrizione articolo precedente:  corretto errore               
003700*                                                                         
003800*EURO*        27/12/00                          EURO/LIRE                 
003900*     trattamento importi in EURO                                         
004000*                                                                         
004100*NOPRZ1*      01/03/01                                                    
004200*      scrittura di movmag con COSTO-STD = 0                              
004300*                                                                         
004400*                                                                         
004500*EURO1*       03/12/01                                                    
004600*      trattamento prezzi di vendita in Euro                              
004700*                                                                         
004800*T5000*       05/11/02                                                    
004900*       Introdotta scelta del dispositivo di uscita                       
005000*                                                                         
005100*PRZBU*       04/12/02                                                    
005200*      Non scrive il movmag se il COSTO-STD e' 0 oppure                   
005300*      senza sconto ma lo tratta come un manca giacenza                   
005400*                                                                         
005500*FIFRA*      08/01/03                                                     
005600*     passaggio file a franchising                                        
005700*                                                                         
005800*PRODI*      12/06/03                                                     
005900*     produzione divise: abilitate vendite da mag 2                       
006000*     con prezzo da PREZZIA senza sconto                                  
006100*                                                                         
006200*PRZANABU*   11/05/04                                                     
006300*     se prezzo scontato = costo-anamat e se Vendita per                  
006400*     Budapest ne permette il trattamento                                 
006500*                                                                         
006600*TRAVMAG*   01/07/04                                                      
006700*  Travaso da mag 7 via F.lli Cervi a mag 7 via Santi                     
006800*                                                                         
006900*PRZ-PUBBL*     13/LUG/2005 - LAUROS                                      
007000*     Aggiunto il prezzo di listino al pubblico nella stampa              
007100*     dei cartellini solo per il conto DEMA = 10010101                    
007200*     (richiesta di Edoardo Testi)                                        
007300*                                                                         
007400*PRINTDD6*      20/LUG/2005 - LAUROS                                      
007500*     PRINTDD6 al posto di PRINTDD3                                       
007600*     PRINTDD6 va su ANAMAT.MODELLI per ogni MODELLO                      
007700*     PRINTDD3 carica tutta ANAMAT.MODELLI (+ lento)                      
007800*                                                                         
007900*TASTO-PER-CONTINUARE*  21/LUG/2005 - LAUROS                              
008000*     Aggiunto "premi un tasto per continuare..."                         
008100*                                                                         
      *etich-vuota*  17/FEB/2006  -  LAUROS
      *     aggiunta un'etichetta vuota alla fine, per evitare che
      *     l'ultima etichetta di fine lotto venga stampata dopo
      *     un salto pagina... (con il passaggio a linux si comporta
      *     in questo modo...!)
      *
      *volante*      04/04/07
      *     modifica fatta per scaricare EUROSTOCK con data
      *     22/12/06  e vecchio parametro 2006 + 1
      *
      *sempre0*      07/11/07
      *     in caso di manca prezzo metto il prezzo sempre a 0
      *     in modo da evitare fatturazioni a prezzo pieno !!
      *
      *FSTOCK*       10/04/08    (annullato)
      *     nuova dicitura per fatture stock
      *
      *Mag3_V/F*     27/01/09    -  CAIO
      *     per il magazzino 3, chiede all'utente se la roba e` roba
      *     buona o fallata
      *
      *MAXCA*        06/11/09
      *     aggiunto param. in input (MAX-CAPI) per limitare il numero 
      *     di capi trattati
      *
      *VIBLO*        10/12/09
      *     chiede in input mag (4 o 6) e non usa pi� fisso mag 3;
      *     non chiede valido o fallato      
      *     non chiede valido o fallato      
      *
      *ASZERO*       06/10/10    -  CAIO
      *     correzione bug: non distingue l'anno "0" dal dato "tutti
      *     gli anni"
      *
      *VACO*         25/10/10
      *           valorizza costo industriale (PREZZO) su MOVMAG con
      *           COSTO di ANAMAT variante (per Elisa)
      *                    
      *ACQUO*     28/11/2011  -  LAUROS + LANDUX
      *         acquisizione outlet esteri
      *         effettua test esistenza PREZZI solo se deve stampare le etichette
      *
      *MOVSKU   24/06/15
      *       Scrive MOVSKU usando il baruni restituitogli dal py e 
      *       aggiungere baruni nella stampa cartellino
      *  
      *ASOLOB2C*   12/04/2018
      *     valorizza costo acquisto su MOVMAG vendita da ANAMAT_CST distintamente 
      *     per neg B2C (no DT 575) e altri neg
      *
      *ESTETA*     20/11/18
      *      estensione taglie
      *
      *UNICODDT*
      *     VALERIA NOVEMBRE 2020
      *     poter inserire piu' as e per ogni as piu' classi e per ogni classe piu' max-capi
      *     per formare un unico ddt
      *
      *************************************
      *NO-DATGE 03 DICEMBRE 2020 VALERIA 
      *     dismissione DATGE >> sostituita PF.SOCIETA con COPY MAPPASOCIETA
      *         sostituite anamat_modelli e anamat_pezzi 
      *         con prezzi_modelli_dbg e anagrafica_modelli_dbg 
      *
008200 ENVIRONMENT DIVISION.                                                    
008300 CONFIGURATION SECTION.                                                   
008400 SOURCE-COMPUTER.  HP-3000.                                               
008500 OBJECT-COMPUTER.  HP-3000.                                               
008600 SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.          
      *ESTETA* 
      *REPOSITORY.
      *      FUNCTION idxtg
      *      FUNCTION tgxid.                   
008700*BUDA*                                                                    
008800 INPUT-OUTPUT SECTION.                                                    
008900     FILE-CONTROL.                                                        
009000     SELECT FILE-BC ASSIGN TO "BARCNEG"                                 
              ORGANIZATION IS LINE SEQUENTIAL.
009100     SELECT FILE-PEND ASSIGN TO "PEND"                                  
              ORGANIZATION IS LINE SEQUENTIAL.
009200*                                                                         
009300 DATA DIVISION.                                                           
009400*BUDA*                                                                    
009500*                                                                         
009600 FILE SECTION.                                                                                                                                    
009700* movsku                                                                        
009800 FD FILE-BC DATA RECORD REC-BC.                                           
009900 01 REC-BC.                                                               
010000  05 C-MAT-S             PIC 9(13).   
        05 T-1                 PIC X.                   
010100  05 TAGLIA-S            PIC 9.                
        05 T-2                 PIC X.          
010200  05 MAG-S               PIC 999.  
        05 T-3                 PIC X.                    
010300  05 SETTORE-S           PIC X(4).  
        05 T-4                 PIC X.                     
010400  05 NOME-S              PIC X(7).  
        05 T-5                 PIC X.                     
010500  05 PREZZO-S            PIC 9(8).  
        05 T-6                 PIC X.                     
010600  05 NOME-F-S            PIC X(15).  
        05 T-7                 PIC X.                    
010700  05 COL-F-S             PIC X(5).   
        05 T-8                 PIC X.                    
010800  05 TG-OUT-S            PIC XX.   
        05 T-9                 PIC X.
        05 BARUNI-S            PIC X(13).                                          
010900*                                                                         
011000 FD FILE-PEND DATA RECORD REC-PEND.                                       
011100 01 REC-PEND             PIC X(132).                                      
011200*                                                                         
011300 WORKING-STORAGE SECTION.                                                 
011400*      
      *ESTETA* 
       COPY NTG. 
       77 NTG-MEM PIC S9(4) COMP.                                                                                                     
011500 77 ERR-DISP               PIC -(6).                                      
011600 77 DISP-4  PIC ZZZ9-.                                                    
011700 77 DISP-8  PIC ZZZZZZZZ-.                                                
011800 77 STATO-DISPLAY  PIC ZZZZ-.                                             
011900*BUDA*                                                                    
012000 77 FLAG-DT-ESTERO          PIC S9(4) COMP.                               
012100  88 SI-DT-ESTERO    VALUE 1.                                             
012200 77 PREZZO-MEM              PIC S9(9) COMP.                               
012300 77 PREZZO-TOT              PIC 9(11) COMP-3.                             
012400 77 IR   PIC S9(4) COMP.                                                  
       77 JRUNC   PIC S9(4) COMP-5 VALUE 0.
       77 IT      PIC S9(4) COMP.
       77 IC      PIC S9(4) COMP.
012500*                                                                         
012600*******************************************                               
012700*                                                                         
012800 01 CAMPI-ANAGRAFICI.                                                     
012900  05  INDIRIZZO-STD         PIC X(66).                                    
013000  05  INDIRIZZO-COM         PIC X(60) VALUE SPACES.                       
013100  05  LOCALITA-COM          PIC X(60) VALUE SPACES.                       
013200  05  CAP-COM               PIC S9(5) COMP-3 VALUE 0.                     
013300  05  PROV-COM              PIC XX VALUE SPACES.                          
013400  05  STATO-COM             PIC XXX VALUE SPACES.                         
013500*                                                                         
013600  05  INDIRIZZO-C-COM         PIC X(60) VALUE SPACES.                     
013700  05  LOCALITA-C-COM          PIC X(60) VALUE SPACES.                     
013800  05  CAP-C-COM               PIC S9(5) COMP-3 VALUE 0.                   
013900  05  PROV-C-COM              PIC XX VALUE SPACES.                        
014000*                                                                         
014100  05 D-CONTO-MEM     PIC X(24).                                           
014200  05 D-CONTO-AGG-MEM  PIC X(24).                                          
014300  05 D-CONTO-VET     PIC X(24).                                           
014400*                                                                         
014500  05  INDIRIZZO-C-VET         PIC X(60) VALUE SPACES.                     
014600  05  LOCALITA-C-VET          PIC X(60) VALUE SPACES.                     
014700  05  CAP-C-VET               PIC S9(5) COMP-3 VALUE 0.                   
014800  05  PROV-C-VET              PIC XX VALUE SPACES.                        
014900*                                                                         
015000 01 CAMPI-COMODO.                                                         
015100  05 RIF-BOLLA-DDT         PIC 9(12).                                     
015200  05 FILLER REDEFINES RIF-BOLLA-DDT.                                      
015300   10 AA-MM-GG-DDT       PIC 9(6).                                        
015400   10 NUMERO-DDT         PIC 9(6).                                        
015500  05 CLIENTE-DDT           PIC S9(9) COMP.                                
015600  05 MAGAZZINO-DDT         PIC S9(4) COMP.                                
015700  05 CAUSALE-DDT           PIC X(4).                                      
015800  05 TIPO-DOC-DDT          PIC S9(4) COMP.                                
015900     88 DOC-DDT      VALUE 1.                                             
016000     88 DOC-NOT-DDT  VALUE 2.                                             
016100  05 TIPO-MOVIMENTO-DDT    PIC S9(4) COMP.                                
016200     88 VENDITA       VALUE 1.                                            
016300     88 TRASFERIMENTO VALUE 2.                                            
016400     88 C-VISIONE     VALUE 3.                                            
016500     88 C-LAVAGGIO    VALUE 4.                                            
016600     88 C-LAVORAZIONE VALUE 5.                                            
016700  05 TIPO-STAMPA-DDT       PIC S9(4) COMP.                                
016800     88 PRODOTTI-FINITI   VALUE 1.                                        
016900     88 MATERIE-PRIME     VALUE 9.                                        
017000  05 LOC-PART-DDT          PIC X(56).                                     
017100  05 NOTE-DDT              PIC X(44) OCCURS 2.                            
017200  05 TIPO-DATA-SET-DDT     PIC X.                                         
017300     88 MOVMAG-DDT    VALUE "0" , " ".                                    
017400     88 MOVTRANS-DDT  VALUE "1".                                          
017500*                                                                         
017600 01 IMPORTO-X-PL           PIC S9(11) COMP-3.                             
017700*                                                                         
017800*BUDA*                                                                    
017900 01 FILE-FAT-DDT PIC X.                                                   
018000  88 SI-FILE-FAT VALUE "S".                                               
018100*                                                                         
018200*TRAVMAG*                                                                 
018300 01 RIGA-1-DDT   PIC X(65).                                               
018400 01 RIGA-2-DDT   PIC X(65).                                               
018500*                                                                         
018600*******************************************                               
018700*                                                                         
018800 01  PAR-INDIRIZZO.                                                       
018900  05  STATO-IND            PIC S9(4) COMP.                                
019000  05  FUNZIONE-IND         PIC S9(4) COMP.                                
019100  05  LL-STRINGA-IND       PIC S9(4) COMP.                                
019200  05  LL-SUBSTRINGA-IND    PIC S9(4) COMP.                                
019300  05  FILLER               PIC X(6).                                      
019400  05  TIPO-SEP             PIC XX VALUE " ;".                             
019500*                                                                         
019600 01 AREA-REC-SET         PIC X(512).                                      
019700*                                                                         
019800 01 FILLER REDEFINES AREA-REC-SET.                                        
019900  05 REC-ANACON  COPY YANACON.                                            
020000*                                                                         
020100 01 FILLER REDEFINES AREA-REC-SET.                                        
020200  05 REC-BOLLE     COPY YBOLLE.                                           
020300*                                                                         
020400 01 FILLER REDEFINES AREA-REC-SET.                                        
020500  05 REC-ANAMAT COPY YANAMAT.                                             
020600*                                                                         
020700*BUDA*                                                                    
020800 01 REC-CONFATT   COPY YCONFAT.                                           
020900*                                                                         
021000*BUDA*                                                                    
021100 01 REC-PREZZI   COPY YPREZZI.                                            
021200*                                                                         
021300*                                                                         
021400*PRZ-PUBBL*                                                               
021500 01 REC-PREZZI-PUB COPY YPREZZI.                                          
021600*                                                                         
021700*                                                                         
021800 01 REC-INDIRIZZI  COPY YINDIRIZ.                                         
021900*                                                                         
022000 01 REC-SITPF COPY YSITPF.    
022100*                                                                         
022200 01 MOVMAG  COPY YMOVMAG.                                                  
022300*                                                                         
022400 01 REC-PARAMETRI         COPY YPARAMDT.                                  
022500 01 REC-PARAM-RID REDEFINES REC-PARAMETRI.                                
022600  03 FILLER               PIC X(4).                                       
022700  03 PARAM-MAG         COPY WPARAM13.                                     
022800*
      *volante*
022400 01 REC-PARAM-FITTIZI      COPY YPARAMDT.                                  
022500 01 REC-PARAM-FITTIZ-R REDEFINES REC-PARAM-FITTIZI.                                
022600  03 FILLER               PIC X(4).                                       
022700  03 PARAM-RIPRADI         COPY WPARAM55.                                     
022800*      
022900 01 RIGA-PENDENTI.                                                        
023000  03 ANTE-PRIMA-RIGA.                                                     
023200    10 FILLER        PIC X(9).                                            
023300    10 NUMERO-P      PIC 9(6).                                            
023400    10 FILLER        PIC X(18).                                           
023500  03 PRIMA-RIGA.                                                          
023700    10 FILLER        PIC X(9).                                            
023800    10 CODICE-P      PIC 9(14).                                           
023900    10 FILLER        PIC X(10).                                           
024000  03 SECONDA-RIGA.                                                        
024200    10 FILLER        PIC X(9).                                            
024300    10 NOME-P        PIC X(14).                                           
024400    10 FILLER        PIC X(10).                                           
024500*EURO*                                                                    
024600*                                                                         
024700  03 TERZA-RIGA.                                                          
024900    10 DIV-LIT       PIC X(9).                                            
025000    10 PREZZO-P      PIC ZZ.ZZZ.ZZZ.ZZZ.                                  
025100    10 FILLER        PIC X(10).                                           
025200*                                                                         
025300  03 QUARTA-RIGA.                                                         
025500    10 DIV-EUR       PIC X(11).                                           
025600    10 PREZZO-P-E    PIC Z(7),ZZ.                                         
025700    10 FILLER        PIC X(12).                                           
025800*                                                                         
025900*BUDA*                                                                    
026000 01 CAMPI-X-WRITE.                                                        
026100  05 RIF-INTR-WR        PIC 9(12).                                        
026200  05 CONTO-CLI-WR       PIC S9(9) COMP.                                   
026300  05 CONTO-DEST-WR      PIC S9(9) COMP.                                   
026400  05 MAGAZZINO-WR       PIC S9(4) COMP.                                   
026500  05 DEST-WR            PIC S9(4) COMP.                                   
026600     88 X-ESTERO     VALUE 3.                                             
026700     88 X-RESIDUO    VALUE 2.                                             
026800  05 DIVISA-WR          PIC XXXX.                                         
026900  05 LISTINO-WR         PIC 9(4).                                         
027000  05 CAUSALE-WR         PIC X(4).                                         
027100*                                                                         
027200 01 DATA-CARICO-WR      PIC 9(6).                                         
027300*                                                                         
027400*                                                                         
027500*BUDA*                                                                    
027600*                                                                         
027700 01 LISTINO-MEM      PIC 9999.                                            
027800 01 DIVISA-MEM    PIC X(4).                                               
027900*                                                                         
028000*PRODI*                                                                   
028100 01 CAMBIO-MEM   PIC S9(9) COMP.                                          
028200*                                                                         
028300 01 CAMPI-UTILI.                                                          
028400  05 OK-GIAC           PIC S9(4) COMP.                                    
028500     88 GIAC-OK        VALUE 1.                                           
028600*NOPRZ*                                                                   
028700  05 OK-PREZZO         PIC S9(4) COMP.                                    
028800     88 PREZZO-OK      VALUE 1.                                           
028900*                                                                         
029000  05 CONTA-PAGINE          PIC S9(4) COMP.                                
029100  05 TOT-CAPI-NO-GIAC      PIC 9(4).                                      
029200*                                                                         
029300  05 CONTA-RIGHE           PIC S9(4) COMP.                                
029400  05 USCITA-PROGRAMMA      PIC S9(4) COMP.                                
029600  05 RISP-NO-GIAC        PIC X.                                           
029700*NOPRZ*                                                                   
029800  05 RISP-NO-PREZZO      PIC X.                                           
029900*BUDA*                                                                    
030000  05 FLAG-ANACON       PIC X.                                             
030100  05 SOC-COM              PIC 99.                                         
030200  05 FILLER REDEFINES SOC-COM.                                            
030300   10 PRE-SOC       PIC 9.                                                
030400   10 SOC-REALE     PIC 9.                                                
030500  05 TROVATO-GIAC    PIC S9(4) COMP.                                      
030600     88 GIAC-TROVATA   VALUE 1.                                           
030700  05 NUM-BOLLE             PIC S9(8) COMP.                                
030800  05 IND-CAPI-LETTI        PIC S9(4) COMP.                                
030900  05 TOT-BOLLA-C              PIC S9(4) COMP.                             
031000  05 TOT-CAPI-RIGA-C              PIC S9(4) COMP.                         
031100  05 IND-PAG        PIC S9(4) COMP.                                       
031200  05 ITB-T          PIC S9(4) COMP.                                       
031300  05 ITB-T-5        PIC S9(4) COMP.                                       
031400  05 IND-CL          PIC S9(4) COMP.                                      
031500  05 VARIAZIONE        PIC S9(4) COMP.                                    
031600    88 FINE-VARIAZIONI   VALUE 1.                                         
031700  05 INDIRIZZO-DPARAM    PIC S9(9) COMP.                                  
031800  05 D-MAT-MEM       PIC X(24).                                           
031900  05 VAL-REC-MEM  PIC XX.                                                 
032000  05 IND-RIGA        PIC S9(4) COMP.                                      
032100  05 CNTR-ANAMAT      PIC S9(4) COMP.                                     
032200     88 C-MAT-OK   VALUE 1.                                               
032300  05 DISP-15         PIC 9(15).                                           
032400  05 DISP-3          PIC 999.                                             
032500*BUDA*                                                                    
032600  05 TG-CAL          PIC 99.                                              
032700  05 PTG-MEM         PIC S9(4) COMP.                                      
032800  05 TAGLIO-MEM      PIC S9(4) COMP.                                      
032900  05 IND-FILE           PIC 9(6).                                         
033000  05 COD-REC      PIC S9(4) COMP.                                         
033100  05 ESIST-VECCHIO   PIC S9(4) COMP.                                      
033200     88 VECCHIO-ESIST VALUE 1.                                            
033300  05 OK-O-N          PIC S9(4) COMP.                                      
033400     88 O-N-OK VALUE 1.                                                   
033500  05 OK-NEG          PIC S9(4) COMP.                                      
033600     88 NEG-OK VALUE 1.                                                   
033700  05 CONT                 PIC S9(4) COMP.                                 
033800  05 CONT-D               PIC ZZZZ.                                       
033900  05 PREZZO-D             PIC Z(6)9,99.                                   
034000  05 PREZZO-TOT-D         PIC ZZZ.ZZZ.ZZ9,99.                             
034100  05 FINITO             PIC S9(4) COMP.                                   
034200     88 FINE-FILE VALUE 1.                                                
034300  05 IND-4              PIC S9(4) COMP.                                   
034400  05 CONFERMA-STORNO PIC XX.                                              
034500  05 C-MAT-MEM  PIC 9(15).                                                
034600*T5000*                                                                   
034700  05 USCITA-DEVICE  PIC S9(4) COMP.                                       
034800     88 DEV-OK VALUE 1.                                                   
034900*                                                                         
035000*                                                                         
035100 01 COL-COM.                                                              
035200  05 PRE-COL         PIC 9.                                               
035300  05 COL-VAR         PIC 99.                                              
035400 01 COL-COM-R REDEFINES COL-COM  PIC 999.                                 
035500*                                                                         
035600*2000*                                                                    
035700*01 DATA-BOLLA-COM        PIC 9(6).                                       
035800*01 FILLER REDEFINES DATA-BOLLA-COM.                                      
035900* 05 AA-COM           PIC 99.                                             
036000* 05 MM-COM           PIC 99.                                             
036100* 05 GG-COM           PIC 99.                                             
036200*                                                                         
036300*                                                                         
036400*2000*                                                                    
036500*01 RIF-INTR.                                                             
036600*  05 AA-MM-GG PIC 9(6).                                                  
036700*  05 NUMERO PIC 9(6).                                                    
036800*01 RIF-INTR-RID REDEFINES RIF-INTR    PIC 9(12).                         
036900*                                                                         
037000*2000*                                                                    
037100*01 DATA-BOLLA-COM-R          PIC 9(6).                                   
037200*01 FILLER REDEFINES DATA-BOLLA-COM-R.                                    
037300* 05 GG-COM-R         PIC 99.                                             
037400* 05 MM-COM-R         PIC 99.                                             
037500* 05 AA-COM-R         PIC 99.                                             
037600*                                                                         
037700*                                                                         
037800 01 PARQDATA  COPY QPARDATS.                                              
037900 01 PARGEN    COPY QPARGEN.                                               
038000 01 PARAGGPF  COPY PARAGGPF.                                              
038100*                                                                         
038200 01 CODICE-CONTO PIC 9(8).                                                
038300 01 CODICE-CONTO-R REDEFINES CODICE-CONTO.                                
038400   05 CAPO-CONTO PIC 9(3).                                                
038500   05 SOTTO-CONTO PIC 9(5).                                               
038600   05 SOTTO-CONTO-R REDEFINES SOTTO-CONTO.                                
038700     10 SOTTO-CONTO-3 PIC 999.                                            
038800     10 SOTTO-CONTO-2 PIC 99.                                             
038900*                                                                         
039000*                                                                         
039100 01 TABELLA-SINGOLI.                                                      
      *MOVSKU                             
039200*  05 ELEM-TAB-SING     PIC X(23) OCCURS 9999.                             
        05 ELEM-TAB-SING     PIC X(36) OCCURS 9999.
039300*                                                                         
039400 01 ELEMENTO-SINGOLI.                                                     
039500*  05 CONT-SING         PIC S9(4) COMP.                                    
039600  05 C-MAT-SING        PIC S9(15) COMP-3.
039500  05 CONT-SING         PIC S9(4) COMP.  
039700  05 D-MAT-SING        PIC X(7).                                          
039800  05 PREZZO-SING       PIC S9(9) COMP.                                    
039900  05 PRIMA-TG-SING     PIC S9(4) COMP. 
      *MOVSKU
        05 SKU-SING          PIC X(13).                                      
040000*                                                                         
040100 01 PARTAB-SING       COPY QPARTAB.                                       
040200*                                                                         
040300 01  DATI-INPUT.                                                          
040400  05 CONTO-IN         PIC X(8).                                           
040500  05 CONTO-IN-R REDEFINES CONTO-IN   PIC 9(8).                            
040600*PRZ-PUBBL*                                                               
040700    88 CONTO-DEMA     VALUES ARE 10010101 10010362.                                     
040800*                                                                         
040900  05 CONTO-FINE REDEFINES CONTO-IN   PIC XXX.                             
041000*BUDA*                                                                    
041100  05 FILLER REDEFINES CONTO-IN.                                           
041200    10 FILLER         PIC XXX.                                            
041300    10 FILLER         PIC XX.                                             
041400    10 NEG-IN         PIC 999.                                            
041500*                                                                         
041600  05 COD-IN   COPY DANCODBC.                                              
041700  05 COD-IN-RID REDEFINES COD-IN   PIC X.                                 
041800     88 LETT-FINE      VALUE ".".                                         
041900     88 LETT-ANN-ULT   VALUE "%".                                         
042000     88 LETT-ANN-TUTTO VALUE "@".                                         
042100     88 LETT-STAMPA    VALUES "S", "s".
042200*
      *Mag3_V/F*
       01 F-V-INPUT   PIC X.
      *
042300 01 MEM-COD-IN  PIC X(14).                                                
042400*                                                                         
042500*T5000*                                                                   
042600 01 DEV-IN           PIC X(2).
           01 DISIMPEGNA       PIC XX.
           01 DA-TRASFERIRE    PIC S9(8) COMP.
042700*                                                                         
042800 01 C-MAT-COM COPY DANCODMT.                                              
042900*                                                                         
043000 01 COM-QTA-DISP     PIC S9(4) COMP.                                      
043100*                                                                         
043200 01 TABELLA-ARTICOLI-LETTI.                                               
043300  03 ART-TAB-LETTI   OCCURS 5000.                                          
043400   05 TAB-ART        PIC S9(15) COMP-3.                                   
043500   05 D-MAT-TAB      PIC X(7).                                            
043600*BUDA*                                                                    
043700   05 PRIMA-TG-TAB     PIC S9(4) COMP.                                    
043800   05 PREZZO-TAB       PIC S9(9) COMP.                                    
043900   05 CAMBIO-TAB       PIC S9(9) COMP.                                    
044000   05 TIPO-ANA-TAB  PIC XX.                                               
044100   05 QTA-GIAC-TAB.                                                       
044200     10 QTA-GIAC-PF-TAB  PIC S9(8) COMP   COPY NTGOCCURS. 
044300   05 QTA-TAGLIE-TAB.                                                     
044400     10 QTA-TAGLIA-TAB PIC S9(4) COMP    COPY NTGOCCURS. 
      *VACO*                                                            
         05 COSTO-TAB       PIC S9(9) COMP.
044500*                                                                         
044600 01 ART-ELEM-LETTI.                                                       
044700   05 ELEM-ART        PIC S9(15) COMP-3.                                  
044800   05 D-MAT-ELEM     PIC X(7).                                            
044900*BUDA*                                                                    
045000   05 PRIMA-TG-ELEM    PIC S9(4) COMP.                                    
045100   05 PREZZO-ELEM      PIC S9(9) COMP.                                    
045200   05 CAMBIO-ELEM      PIC S9(9) COMP.                                    
045300   05 TIPO-ANA-ELEM    PIC XX.                                            
045400   05 QTA-GIAC-ELEM.                                                      
045500     10 QTA-GIAC-PF-ELEM  PIC S9(8) COMP   COPY NTGOCCURS.                        
045600   05 QTA-TAGLIE-ELEM.                                                    
045700     10 QTA-TAGLIA-ELEM PIC S9(4) COMP  COPY NTGOCCURS.    
      *VACO*                                                            
         05 COSTO-ELEM       PIC S9(9) COMP.
045800*                                                                         
045900 01 QTA-TAGLIE-NEG.                                                       
046000     10 QTA-TAGLIA-NEG PIC S9(4) COMP COPY NTGOCCURS.                           
046100*                                                                         
046200 01 NUM-ELEM-MAX-ART  PIC S9(4) COMP VALUE 5000.                           
046300*                                                                         
046400 01 PARTAB-ART   COPY QPARTAB.                                            
046500*                                                                         
046600 01 TABELLA-NO-GIAC.                                                      
046700  05 ELEM-NO-GIAC    OCCURS 1000.                                         
046800   10 C-MAT-NO-GIAC       PIC S9(15) COMP-3.                              
046900   10 PREZZO-NO-GIAC       PIC S9(9) COMP.                                
047000   10 D-MAT-NO-GIAC        PIC X(7).                                      
047100*PRZBU*                                                                   
047200   10 CAUSALE-NO-GIAC      PIC X(10).                                     
047300   10 CAUSALE-NO-PRZ       PIC X(10).                                     
047400*                                                                         
047500 01 IND-CAPI-NO-GIAC      PIC S9(4) COMP.                                 
047600*                                                                         
047700 01 COMANDO-BUILD.                                                        
047800   05 FILLER PIC X(7) VALUE "BUILD P".                                    
047900   05 BUILD-N-DDT      PIC 9(6).                                          
048000   05 FILLER           PIC X(30) VALUE                                    
048100            ";REC=-132,3,F,ASCII;DISC=20000".                             
048200   05 CARRIAGE-RETURN PIC X VALUE X"13".                                    
048300*                                                                         
048400 01 COMANDO-FILE.                                                         
048500   05 FILLER PIC X(11) VALUE "FILE PEND=P".                               
048600   05 FILE-N-DDT       PIC 9(6).                                          
048700   05 CARRIAGE-RETURN PIC X VALUE X"13".                                    
048800*                                                                         
048900 01 COMANDO-FILE-2.                                                       
049000   05 FILLER PIC X(6) VALUE "FILE P".                                     
049100   05 FILE-N-DDT-2     PIC 9(6).                                          
049200   05 FILLER PIC X(9) VALUE ";DEV=91,5".                                  
049300   05 CARRIAGE-RETURN PIC X VALUE X"13".                                    
049400*                                                                         
049500 01 COMANDO-PURGE.                                                        
049600   05 FILLER PIC X(7) VALUE "PURGE P".                                    
049700   05 PURGE-N-DDT      PIC 9(6).                                          
049800   05 CARRIAGE-RETURN PIC X VALUE X"13".                                    
049900*                                                                         
050000 01 COMANDO-PRINT.                                                        
050100   05 FILLER PIC X(7) VALUE "PRINT P".                                    
050200   05 PRINT-N-DDT         PIC 9(6).                                       
050300   05 FILLER              PIC X(7) VALUE ";OUT=*P".                       
050400   05 PRINT-N-DDT-2       PIC 9(6).                                       
050500   05 CARRIAGE-RETURN PIC X VALUE X"13".                                    
050600*                                                                         
      *conv
      *
       01 COMANDO-LPR-LINUX.
         05 FILLER                PIC X(9) VALUE "lpr -P p1".
         05 LPR-NUM-STAMPANTE     PIC 99.
         05 FILLER                PIC X VALUE " ".
         05 LPR-NOME-FILE         PIC X(80).
         05 FILLER                PIC X VALUE X"00".
      *
       01 WK-VAR-NAME             PIC X(80).
       01 WK-VAR-VALUE            PIC X(80).
      *
       01 DIR-VAR-NAME            PIC X(80).
       01 DIR-VAR-VALUE           PIC X(80).
      *
       01 USER-VAR-NAME           PIC X(80).
       01 USER-VAR-VALUE          PIC X(80).
      *
       01 FILE-VAR-NAME           PIC X(80).
       01 FILE-VAR-VALUE          PIC X(80).
      *
      *conv-end
050700 01 ERR   PIC S9999  COMP VALUE 0.                                        
050800 01 ERR-PARM  PIC S9999 COMP VALUE 0.                                     
050900*                                                                         
051000 01 LOCALITA-PART-STR      PIC X(52).
051100*  "Magazzino Via Santi 8, Cavriago (R.E.) ".                             
      *
      * Tabella di mappatura magazzino -> localita'
      *
       01 IND-LOC                PIC S9(4) COMP.
      * allineare con il numero di occorrenze della tabella espansa!
       01 MAX-LOC                PIC S9(4) COMP VALUE 4.
      *
       01 TAB-LOC-EXP.
         05 FILLER               PIC 9(3)  VALUE   2.
         05 FILLER               PIC X(52) VALUE
            "Magazzino Via Santi 8, Cavriago (R.E.) ".
         05 FILLER               PIC 9(3)  VALUE   3.
         05 FILLER               PIC X(52) VALUE
      * "Via Dell'Artigianato 2/A Qre SPIP(PR) c/o LA GIOVANE".       
            "Magazzino Via Santi 5, Cavriago (R.E.) ".
         05 FILLER               PIC 9(3)  VALUE   6.
         05 FILLER               PIC X(52) VALUE
            "Magazzino Via Santi 8, Cavriago (R.E.) ".
         05 FILLER               PIC X(52) VALUE
            "Magazzino Via Santi 8, Cavriago (R.E.) ".            
         05 FILLER               PIC 9(3)  VALUE   7.
         05 FILLER               PIC X(52) VALUE
            "Magazzino Via Santi 8, Cavriago (R.E.) ".
      * allineare le occorrenze con quelle della tabella espansa!
       01 TAB-LOC REDEFINES TAB-LOC-EXP.
         05 FILLER OCCURS    4.
           10 COD-LOC            PIC 9(3).
           10 DESC-LOC           PIC X(52).
      *
051200*                                                                         
051300 01 RIGA-DISP.                                                            
051400  03 DATI-DISP-4   OCCURS 3.                                              
051500   05 FILLER        PIC XXX.                                              
051600   05 DISP-ART      PIC Z(14).                                            
051700  05 FILLER      PIC X.                                                   
051800   05 PARE1  PIC X.                                                       
051900   05 QTA-DISP  PIC ZZZ.                                                  
052000   05 PARE2  PIC X.                                                       
052100*                                                                         
052200 01 TOT-CAPI-LETTI-1     PIC 9(4).                                        
052300*                                                                         
052400 01 TAB-LOCK.                                                             
052500   05 FILLER PIC S9(4) COMP VALUE 4.                                      
052600   05 FILLER PIC S9(4) COMP VALUE 17.                                     
052700   05 FILLER PIC X(16) VALUE "DPARAM;".                                   
052800   05 FILLER PIC X(16) VALUE "@".                                         
052900   05 FILLER PIC S9(4) COMP VALUE 17.                                     
053000   05 FILLER PIC X(16) VALUE "SITPF;".                                    
053100   05 FILLER PIC X(16) VALUE "@".                                         
053200   05 FILLER PIC S9(4) COMP VALUE 17.                                     
053300   05 FILLER PIC X(16) VALUE "MOVMAG;".                                   
053400   05 FILLER PIC X(16) VALUE "@".                                         
053500   05 FILLER PIC S9(4) COMP VALUE 17.                                     
053600   05 FILLER PIC X(16) VALUE "BOLLE;".                                    
053700   05 FILLER PIC X(16) VALUE "@".                                         
053800*                                                                         
053900*                                                                         
054000 01 STK-NOME       PIC X(30).                                             
054100 01 STK-C-MAT      PIC 9(15) COMP-3.                                      
054200 01 STK-STAGIONE   PIC 9.                                                 
054300 01 STK-SCO        PIC 9(5) COMP.                                         
054400 01 STK-COLL    PIC 99.                                                   
054500 01 STK-PRZ-SCO    PIC 9(9) COMP.                                         
054600 01 STK-PRZ-LORDO  PIC 9(9) COMP.                                         
054700 01 STK-MSG.                                                              
054800   05 STK-MSG-1          PIC X(30).                                       
054900   05 STK-MSG-2          PIC 9(15).                                       
055000 01 STK-PRIMA-VOLTA      PIC S9(4) COMP.                                  
055100    88 PRIMA-VOLTA  VALUE 0.                                              
055200 01 STK-CAMBIO           PIC 9(9) COMP.                                   
055300*                                                                         
055400 01 NOME-IN   PIC X(30) VALUE "TABSTK.TABELLE".                           
055500 01 NOME-IN-B PIC X(30) VALUE "TABSTB.TABELLE".                           
055600*                                                                         
055700 01  PAR-PRINT.                                                           
055800  05  STATO         PIC S9(4) COMP.                                       
055900  05  LL-RIGA       PIC  9(4) COMP.                                       
056000  05  N-MAX-RIGHE   PIC  9(4) COMP.                                       
056100  05  FLAG-ROUTINE  PIC  9(4) COMP.                                       
056200  05  NUM-FILE-ID   PIC  9(4) COMP.                                       
056300  05  NOME-FILE.                                                          
056400   10 PRE-NOME-FILE     PIC X.                                            
056500   10 TERM-N-FILE       PIC 9(6).                                         
056600   10 FILLER            PIC XXX VALUE ".ST".                              
056700*                                                                         
056800 01  RIGA.                                                                
056900  05  N-STAMPANTE   PIC 9.                                                
057000  05  COMANDO       PIC X.                                                
057100  05  N-RIGA-STAMPA PIC 9(4) COMP.                                        
057200  05  DATI-RIGA     PIC X(132).                                           
057300*                                                                         
057400  05 RIGA-INTESTA REDEFINES DATI-RIGA.                                    
057500    10 FILLER        PIC X.                                               
057600    10 CONTO-T       PIC ZZ9/99999.                                       
057700    10 FILLER        PIC X.                                               
057800    10 D-CONTO-T     PIC X(24).                                           
057900    10 FILLER        PIC XXX.                                             
058000    10 DATA-T        PIC X(12).                                           
058100    10 FILLER        PIC X(5).                                            
058200    10 D-PAG-T       PIC X(5).                                            
058300    10 FILLER        PIC X.                                               
058400    10 NUM-PAG-T     PIC ZZ9.                                             
058500*                                                                         
058600  05 RIGA-DETTAGLIO REDEFINES DATI-RIGA.                                  
058700    10 FILLER        PIC X(7).                                            
058800    10 C-MAT-ST      PIC 9(15).                                           
058900    10 FILLER        PIC XX.                                              
059000    10 TAGLIA-ST     PIC 9.                                               
059100    10 FILLER        PIC X(6).                                            
059200    10 NOME-MOD-ST   PIC X(7).                                            
059300    10 FILLER        PIC X(6).                                            
059400    10 PREZZO-ST     PIC Z(8).                                            
059500*EURO1*                                                                   
059600    10 PREZZO-ST-EU REDEFINES PREZZO-ST  PIC Z(5),ZZ.                     
059700*PRZBU*                                                                   
059800    10 FILLER        PIC X(6).                                            
059900    10 NOGIAC-ST     PIC X(10).                                           
060000    10 FILLER        PIC X(3).                                            
060100    10 NOPRZ-ST      PIC X(10).                                           
060200*                                                                         
060300*                                                                         
060400 01 BUFFER.                                                               
060500  05 N-BUF               PIC S9(4) COMP VALUE 37.                         
060600  05 FILLER              PIC XX.                                          
060700  05 FILLER              PIC X(5120).                                     
060800*                                                                         
060900*MAG6/7*                                                                  
061000 01 MAG-INPUT   PIC X(3).                                                 
061100 01 MAG-INPUT-R REDEFINES MAG-INPUT PIC 9(3).
      *VIBLO*
      *   88 MAG-OK             VALUES  2, 3, 6, 7.
      *MAG1
      *   88 MAG-OK             VALUES  4, 6, 7.
         88 MAG-OK             VALUES  1, 4, 6, 7, 852, 853.
      *
         88 MAG-VALIDO         VALUES  4, 7, 852, 853.
         88 MAG-FALLATO        VALUES  1, 6.
         88 MAG-STOCK          VALUES  1, 4, 6, 7, 852, 853.
       01 SUGG-MAG-DISP        PIC X(50)
      *VIBLO*
      *    VALUE "accettato MAG 2 o 3 o 6 o 7".
          VALUE "accettato MAG 1, 4, 6, 7, 852 o 853".
      *
       01 CLASSE-INPUT.
         05 CLASSE-INPUT-R     PIC 99.
         
         
       01 SOCIETA-INPUT.
         05 SOCIETA-INPUT-R    PIC 9.
       01 AS-INPUT  .
         05 A-INPUT-R         PIC 9.
         05 S-INPUT-R         PIC 9.
      *ASZERO*
       01 FILLER REDEFINES AS-INPUT PIC XX.
         88 TUTTI-AS          VALUES "tt", "TT".
      *
       01 AS-INPUT-R REDEFINES AS-INPUT
                              PIC 99.
      
      *UNICODDT*                                                        INIZIO
       01 STRINGA-TUTTI-AS.
          05 PIC X(30) VALUE "02.04.12.14.22.24.32.34.42.44.".
          05 PIC X(30) VALUE "52.54.62.64.72.74.82.84.92.94.".
      
       01 ELENCO-AS.
          88 TUTTI-AS-ELENCO VALUE "TT", "tt".
          88 FINE-AS          VALUES "chiudi", "CHIUDI".
          05 OCCURS 20.
             10 AS-IN.
                15 AS-IN-R PIC 99.
             10            PIC X.
      
       01 ELENCO-CL.
          88 TUTTI-CL-ELENCO VALUE "TT", "tt".
          05 OCCURS  99.
             10 CL-IN.
                15 CL-IN-R PIC 99.
             10            PIC X.
       01 I-APP PIC S9(4) COMP.
       01 I-AS PIC S9(4) COMP.
       01 I-CL PIC S9(4) COMP.
       01 PRO PIC X.
       01 SINO PIC XX.
          88 SINO-SI VALUE "SI", "si".
          88 SINO-NO VALUE "NO", "no".
       
       01 APP-AS PIC 99.
       01 REDEFINES APP-AS.
           05 APP-A PIC 9.
           05 APP-S PIC 9.
           
       01 SW-AS-ERR PIC 9.
          88 AS-ERR VALUE 1.
       01 SW-CL-ERR PIC 9.
          88 CL-ERR VALUE 1.
       01 SW-MAX-CAPI-RAGGIUNTO PIC 9.
          88 MAX-CAPI-RAGGIUNTO VALUE 1.
       01 SW-ERR-AS-CL pic 9.
          88 err-as-cl value 1.
       01 SW-FINE-CARICA-TAB PIC 9.
          88 FINE-CARICA-TAB VALUE 1.
      * 01 XD PIC X VALUE "S".                                            SI-DISPLAY
       01 XD PIC X VALUE "N".                                            NO-DISPLAY
      *----------------------------------------------------------------*
       01 PAR-TAB-UNICO-DDT COPY QPARTABX.
      *
       01 TAB-UNICO-DDT.
          05 ELE-TAB-UNICO-DDT      PIC X(16) OCCURS   1980.
      *
       01 DEP-TAB-UNICO-DDT.
          05 KEY-TAB-UNICO-DDT. 
             10 TAB-AS PIC 99.
             10 TAB-CL PIC 99.
          05 DATI-TAB-UNICO-DDT. 
             10 TAB-MAX-CAPI PIC 9(6).
             10 TAB-CAPI-LETTI PIC 9(6).
      *----------------------------------------------------------------*
      *UNICODDT*                                                        FINE
061200*                                                                         
061300 01 NOME-IN-35.                                                           
061400   05 NOME-IN-3   PIC XXX.                                                
061500   05 NOME-IN-5   PIC 9(5).                                               
061600   05 FILLER      PIC X(8) VALUE ".TABELLE".                              
061700*                                                                         
061800*                                                                         
061900*                                                                         
062000*NOPRZ*                                                                   
062100 01 PREZZO-ANAMAT  PIC S9(9) COMP.                                        
062200*                                                                         
062300*                                                                         
062400 01 PARDEED  COPY QPARDEED.                                               
062500*                                                                         
062600*EURO*                                                                    
062700 01 PAR-INEU COPY QPARINEU.                                               
062800*                                                                         
062900*EURO1*                                                                   
063000 01 DIVISA-PRIMO-LETTO  PIC X(4).                                         
063100*                                                                         
063200*T5000*                                                                   
063300 01 DESTINO-USCITA  PIC 9(2).                                             
063400   88 DESTINO-VALIDO VALUES ARE 73 90 94 27 28 29 34 22.
063500*                                                                         
063600*                                                                         
063700*FIFRA*                                                                   
063800 01 CONTO-FATTURA-MEM   PIC S9(8) COMP.                                   
063900*                                                                         
      * NO-DATGE                                                        inizio 
        01 TAB-RIGHE-SOCIETA COPY MAPPASOCIETA.                                                    
      * NO-DATGE                                                        fine 
064000*                                                                         
064100*PRZ-PUBBL*                                                               
064200 01 PREZZO-PUBBL        PIC S9(9) COMP.                                   
064300*                                                                         
064400*PRZ-PUBBL*                                                               
064500*EXEC SQL BEGIN DECLARE SECTION END-EXEC.                                 
064600*                                                                         
      * NO-DATGE                                                        inizio 
064700* EXEC SQL INCLUDE MODELLI.IF END-EXEC. 
064800* EXEC SQL INCLUDE PREZZI1.IF END-EXEC. 
      * EXEC SQL INCLUDE SOCIETA.IF END-EXEC.
      
        01 CC-C-MAT         PIC 9(15) .
        01 CC-SOCIETA       PIC XX.
      *  01 CC-ANNO          PIC 9(4).
      *  01 CC-STAGIONE      PIC 9.
        01 CC-LISTINO       PIC 99.
        01 CC-TIPO-PREZZO   PIC X.
        01 CC-PREZZO-DBG    PIC  S9(13)V99 COMP-3.           
            
        
      * NO-DATGE                                                        fine 
065000*movsku   
       EXEC SQL INCLUDE YMOVSKU END-EXEC.     
       
      *ASOLOB2C*                                                        inizio
       EXEC SQL INCLUDE ANACST.IF END-EXEC.
       01 TAB-B2C-NO-DT.
          05 EL-B2C-NO-DT  OCCURS 100.
            10 MAG-B2C-NO-DT  PIC S9(4) COMP.
      *ASOLOB2C*                                                        fine 
       
065100*EXEC SQL END DECLARE SECTION END-EXEC.                                   
065200*                                                                         
065300 01 SQL-CONST             COPY SQLCONST .                       
065400 01 PAR-ERR               COPY PARERR .                         
065500 01 AREA-HL               COPY AREAHL .                         
065600 01 AREA-SI               COPY AREASI .                         
065700*                                                                         
065800 77 FLAG-CURSORE       PIC S9(4) COMP.                                    
065900   88 STOP-CURSORE VALUE 1.                                               
066000*                                                                         
066100 01 SQLCODE-MEM        PIC S9(9) COMP.                                    
066200*                                                                         
066300*                                                                         
066400*TASTO-PER-CONTINUARE*                                                    
066500 01 TASTO-INP          PIC X.                                             
066600*                                                                         
066700*                                                                         
      *
      * Comandi IGP
       01 COM-IGP-STANDARD.
        05 COMIGP-PTXSETUP   PIC X(10) VALUE "!PTX_SETUP".
        05 COMIGP-PTXCFG2    PIC X(13) VALUE "CONFIG-LOAD;2".
        05 COMIGP-PTXEND     PIC X(7)  VALUE "PTX_END".
        05 COMIGP-CREATE     PIC X(15) VALUE "^CREATE;LAB;144".
        05 COMIGP-EXECUTE    PIC X(14) VALUE "^EXECUTE;LAB;1".
        05 COMIGP-SCALEDOT   PIC X(17) VALUE "SCALE;DOT;300;300".
        05 COMIGP-ALPHA      PIC X(5)  VALUE "ALPHA".
        05 COMIGP-STOP       PIC X(4)  VALUE "STOP".
        05 COMIGP-END        PIC X(3)  VALUE "END".
        05 COMIGP-LISTEN     PIC X(7)  VALUE "^LISTEN".
        05 COMIGP-QUIET      PIC X(6)  VALUE "^QUIET".
      *
      *
       01 ROW-IGP            PIC 9(4).
       01 ROW-X-IGP          PIC X(4).
       01 COL-IGP            PIC 9(4).
       01 COL-X-IGP          PIC X(4).
       01 VERT-EXP-IGP       PIC 9.
       01 ORIZ-EXP-IGP       PIC 9.
      *
       01 TEMP-X-IN.
         05 TEMP-EL-X OCCURS 4 PIC X.
       01 TEMP-X-OUT           PIC X(4).
       01 PRIMO-NONZERO        PIC S9(4) COMP.
       01 K                    PIC S9(4) COMP.
      *
       01 RIGA-PEND-COM      PIC X(33).
      *
       01 IND-PEND           PIC S9(4) COMP.
      *S
      *MAXCA*
        01 MAX-CAPI-INPUT.
         05 MAX-CAPI-INPUT-R         PIC 9(6).
      *
      *VACO*                                                            inizio
       01 COSTO-MEM                  PIC S9(9) COMP.
      *VACO*                                                            fine      
      *
      *MOVSKU
       01  PY-INPUT-REC.
           05  INPUT-VAL           PIC 9(14).
          05 FILLER              PIC X VALUE ";".
          05 INPUT-VAL-B        PIC 9(12).
          05 FILLER              PIC X VALUE ";".
          05 INPUT-VAL-C        PIC X(10).
       01 PY-OUTPUT-REC.
          05  OUTPUT-VAL-A        PIC X.
          05  OUTPUT-VAL-B        PIC X(200).
          05  OUTPUT-VAL-B-OK REDEFINES OUTPUT-VAL-B PIC X(13).  
       01 T-TAB                      PIC X VALUE X"9". 
       01 IND-BARUNI                 PIC S9(4) COMP.     
      *MOVSKU       

       01  PY-INPUT-TRASF.
           05  INPUT-RIF-TRASF    PIC X(12).
           05  INPUT-CONTO-TRASF  PIC X(8).
       01  PY-OUTPUT-TRASF.
           05  OUTPUT-STATO-TRASF PIC XX.
           05  OUTPUT-ERR-TRASF   PIC X(70).

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

      *ASOLOB2C*                                                        inizio 
       01 FLAG-B2C-NO-DT     PIC X.          
        88 B2C-NO-DT   VALUES 'S'.   
        
       01 NUM-B2C-NO-DT       PIC S9(4) COMP.
       01 IND-B2C-NO-DT       PIC S9(4) COMP.
       01 ANACST-MAG-COM      PIC S9(4) COMP.
       01 ANACST-C-MAT-COM    PIC S9(15) COMP-3.
       01 ANACST-CST-COM    PIC S9(9) COMP.
       

       01 FLAG-ANACST        PIC X. 
        88 ESISTE-ANACST VALUE "E".
        88 NON-ESISTE-ANACST VALUE " ".     
        
       01  PY-INPUT-REC-B.
           05  INPUT-RIF-INTR     PIC X(12).
           05  INPUT-FLAG         PIC X(1).
           05  INPUT-CONTO        PIC X(8).
           05  INPUT-MAG          PIC X(4).
       01  PY-OUTPUT-REC-B.
           05  OUTPUT-VAL         PIC X(2).  



      *ASOLOB2C*                                                        fine

      *
066800 LINKAGE SECTION.                                                         
066900*                                                                         
067000 01 W-COMMON COPY WCOMMONW.                                               
067100*                                                                         
067200*EXEC SQL INCLUDE SQLCA END-EXEC.                                         
067300*                                                                         
067400 01 SALTO PIC S9(4) COMP.                                                 
067500*                                                                         
067600*PAGE                                                                     
067700*                                                                         
067800 PROCEDURE DIVISION USING W-COMMON SQLCA SALTO.                           
067900 INIZIO.                                                                  
068000     MOVE 0 TO USCITA-PROGRAMMA CONT                                      
068100*T5000*                                                                   
068200               USCITA-DEVICE.                                             
068300*                                                                         
068400     MOVE W-FORMATO-INTERNO TO AA-MM-GG-DDT.                              
068500*T5000*                                                                   
068600     PERFORM TRATTA-DEV THRU EX-TRATTA-DEV                                
068700            UNTIL DEV-OK.                                                 
068800*
      *ASOLOB2C*                                                        inizio
           PERFORM S-SET-1 THRU S-SET-1-EX 
           PERFORM CARICA-B2C-NO-DT THRU EX-CARICA-B2C-NO-DT.
      * NO-DATGE                                                        inizio
      *     PERFORM S-SET-2 THRU S-SET-2-EX 
      * NO-DATGE                                                        fine

      *ASOLOB2C*                                                        fine  
      *
                PERFORM TRATTA-IMPEGNATO THRU EX-TRATTA-IMPEGNATO.

068900     PERFORM TRATTA-NEG THRU EX-TRATTA-NEG                                
069000               UNTIL USCITA-PROGRAMMA = 1.                                
069100 FINE.                                                                    
069200     EXIT PROGRAM.                                                        
069300*                                                                         
069400*                                                                         
069500*                                                                         
069600*T5000*                                                                   
069700 TRATTA-DEV.                                                              
069800     MOVE SPACES TO DEV-IN.                                               
069900     DISPLAY "Disp. USCITA >> " NO ADVANCING.                             
070000     ACCEPT DEV-IN.                                                       
070100     MOVE 2 TO QD-LL-A QD-LL-B.                                           
070200     MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC.                             
           CANCEL "QDEEDIT"
070300     CALL "QDEEDIT" USING PARDEED DEV-IN                                  
070400                          DESTINO-USCITA.                                 
070500     IF QD-STATO OF PARDEED NOT = 0 OR                                    
070600      (QD-STATO OF PARDEED = 0 AND NOT DESTINO-VALIDO)                    
070700       DISPLAY "Dest. USCITA Err. " DEV-IN                             
             PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
070800     ELSE                                                                 
070900       MOVE 1 TO USCITA-DEVICE.                                           
071000 EX-TRATTA-DEV.                                                           
071100     EXIT.                                                                
071200*                                                                         
071300*                                                                         
            TRATTA-IMPEGNATO.
                MOVE SPACES TO DISIMPEGNA.
                DISPLAY "Si vuole eliminare impegnato ?(SI/NO)"
                ACCEPT DISIMPEGNA.
                IF DISIMPEGNA = "si" 
                  MOVE "SI" TO DISIMPEGNA
                END-IF
                IF DISIMPEGNA = "no"
                  MOVE "NO" TO DISIMPEGNA
                END-IF
                IF DISIMPEGNA NOT = "SI" AND DISIMPEGNA NOT = "NO"
                  DISPLAY "Input non valido."
                  PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
                END-IF.
            EX-TRATTA-IMPEGNATO. EXIT. 

071400 TRATTA-NEG.                                   
071500     MOVE 100 TO IND-RIGA.                                                
071600     MOVE 0 TO OK-NEG IND-PAG.                                            
071700     MOVE 0 TO PREZZO-TOT.                                                
071800     PERFORM VERIF-NEG THRU EX-VERIF-NEG                                  
071900                UNTIL NEG-OK.                                             
072000     IF USCITA-PROGRAMMA = 1                                              
072100        GO TO EX-TRATTA-NEG.                                              
072200                                                                          
072300*MAG6/7*                                                                  
072400     MOVE SPACE TO MAG-INPUT.                                             
072500     PERFORM VERIF-MAG THRU EX-VERIF-MAG                                  
072600         UNTIL MAG-INPUT NUMERIC.   
      *
      *Mag3_V/F*
           MOVE SPACE TO F-V-INPUT.                                             
           PERFORM VERIF-F-V THRU EX-VERIF-F-V                                  
               UNTIL NOT F-V-INPUT = SPACE.   
      *
      
      *UNICODDT*                                                        INIZIO
      *     MOVE 0 TO OK-NEG
      *     PERFORM VERIF-AS-SOC-CL  
      *         THRU EX-VERIF-AS-SOC-CL
      *     IF NOT NEG-OK
      *       GO TO EX-TRATTA-NEG.
      **MAXCA*
      *     MOVE SPACE TO MAX-CAPI-INPUT.
      *     PERFORM VERIF-MAX-CAPI  
      *         THRU EX-VERIF-MAX-CAPI   
      *        UNTIL MAX-CAPI-INPUT NUMERIC.
      *
      
           MOVE 0 TO OK-NEG.
           PERFORM VERIFICA-SOC THRU EX-VERIFICA-SOC
           IF NOT NEG-OK
              GO TO EX-TRATTA-NEG.
            
           PERFORM CARICA-TAB-UNICO-DDT THRU EX-CARICA-TAB-UNICO-DDT.
           IF QT-NUM-ELEM-EFF OF PAR-TAB-UNICO-DDT = 0
               MOVE 0 TO OK-NEG.
           
           IF NOT NEG-OK
              GO TO EX-TRATTA-NEG.
      *UNICODDT*                                                        FINE
      *
072700*                                                                         
072800*PRODI*       
072900*     IF MAG-STOCK
073100*        MOVE 0 TO STK-PRIMA-VOLTA
073200*        PERFORM CARICA-TABELLA THRU EX-CARICA-TABELLA
      *     END-IF.
           IF DISIMPEGNA = "SI"
             PERFORM CICLO-DISIMPEGNO THRU EX-CICLO-DISIMPEGNO
           END-IF

073300     PERFORM INIZIA-TAB-ART THRU EX-INIZIA-TAB-ART.                       
073400     PERFORM INIZIA-TAB-SING THRU EX-INIZIA-TAB-SING.                     
073500     PERFORM TRATTA-OLD-NEW THRU EX-TRATTA-OLD-NEW.                       
073600     PERFORM TRATTA-LETTI THRU EX-TRATTA-LETTI.                           
073700 EX-TRATTA-NEG.                                                           
073800     EXIT. 
      *

      *UNICODDT*                                                        INIZIO
       VERIFICA-SOC.
           MOVE SPACE TO SOCIETA-INPUT.
069900     DISPLAY "Soc >> (vuoto = tutti) " NO ADVANCING.                             
070000     ACCEPT SOCIETA-INPUT.                                                       
070100     MOVE 1 TO QD-LL-A QD-LL-B.                                           
070200     MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC.                             
           CANCEL "QDEEDIT"
070300     CALL "QDEEDIT" USING PARDEED SOCIETA-INPUT                                  
070400                            SOCIETA-INPUT-R.                                 
070500     IF QD-STATO OF PARDEED NOT = 0                    
070700       DISPLAY "Soc Err. " SOCIETA-INPUT                  
             GO TO EX-VERIFICA-SOC.      
      *
           MOVE 1 TO OK-NEG.
       EX-VERIFICA-SOC. EXIT. 
      *UNICODDT*                                                        FINE
      *
      *
      *UNICODDT*                                                        INIZIO
       CARICA-TAB-UNICO-DDT.
            PERFORM INIT-PAR-TAB-UNICO-DDT THRU 
                 EX-INIT-PAR-TAB-UNICO-DDT.
                 
            MOVE 0 TO SW-FINE-CARICA-TAB.
            PERFORM  UNTIL FINE-CARICA-TAB
                PERFORM VERIFICA-AS THRU EX-VERIFICA-AS
                IF NOT FINE-AS
                    PERFORM VERIFICA-CL THRU  EX-VERIFICA-CL
                    PERFORM VERIFICA-MAX-CAPI THRU
                         EX-VERIFICA-MAX-CAPI

      *              DISPLAY "INSERIMENTO-TAB-UNICO-DDT"
                    PERFORM VARYING I-AS FROM 1 BY 1 
                        UNTIL I-AS > 20
                        IF AS-IN-R(I-AS) > 0 
                            MOVE AS-IN-R(I-AS) 
                              TO TAB-AS OF DEP-TAB-UNICO-DDT
                            PERFORM VARYING I-CL FROM 1 BY 1 
                                UNTIL I-CL > 99
                                IF CL-IN-R(I-CL) > 0
                                     MOVE CL-IN-R(I-CL) 
                                       TO TAB-CL OF DEP-TAB-UNICO-DDT   
                                     MOVE MAX-CAPI-INPUT-R 
                                       TO TAB-MAX-CAPI  
                                     MOVE 0 TO TAB-CAPI-LETTI
      *                               DISPLAY "INS " DEP-TAB-UNICO-DDT   
                                     PERFORM INS-TAB-UNICO-DDT THRU 
                                          EX-INS-TAB-UNICO-DDT 
                                END-IF
                            END-PERFORM
                        END-IF
                    END-PERFORM 
                END-IF
            END-PERFORM.
            
            display "---------------------------".
            DISPLAY "AS CL MAX-CAPI INSERITI".
            PERFORM MOSTRA-TAB-UNICO-DDT THRU 
                 EX-MOSTRA-TAB-UNICO-DDT.
            DISPLAY "CORRETTI? 'SI' per proseguire "
                    "'NO' per rifare elenco AS CL da capo"
            MOVE "NO" TO SINO.
            ACCEPT SINO.
            IF SINO-NO
                GO TO CARICA-TAB-UNICO-DDT.
            
            IF XD = "S"
                DISPLAY "sono dopo mostra-tab-unico"
      *          display "return per proseguire"                                    
      *          accept pro
                .
       EX-CARICA-TAB-UNICO-DDT. EXIT.
      *
      *
       VERIFICA-AS.
      *     DISPLAY "VERIFICA-AS".
           DISPLAY "AS >> (tt=tutti) (elenco separato da ,) (CHIUDI)".  
      *
           MOVE SPACES TO ELENCO-AS.
           ACCEPT ELENCO-AS
           IF FINE-AS
               MOVE 1 TO SW-FINE-CARICA-TAB
               GO TO EX-VERIFICA-AS.
      *ASZERO*
           IF TUTTI-AS-ELENCO
               DISPLAY "Tratto tutti gli AS"
               MOVE STRINGA-TUTTI-AS TO ELENCO-AS
           ELSE
               MOVE 0 TO SW-AS-ERR
               PERFORM VARYING I-AS FROM 1 BY 1 
                 UNTIL I-AS > 10
                 OR AS-ERR
                    MOVE 2 TO QD-LL-A QD-LL-B                                           
                    MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC                             
                    CANCEL "QDEEDIT"
                    CALL "QDEEDIT" USING PARDEED AS-IN(I-AS)                                  
                                                AS-IN-R(I-AS)                                 
                    IF (QD-STATO OF PARDEED NOT = 0)
      *             OR (AS-IN-R(I-AS) = 0)
                       DISPLAY "AS Err. " AS-IN(I-AS)
                       MOVE 1 TO SW-AS-ERR
                       GO TO VERIFICA-AS
                    END-IF
                END-PERFORM.                
      *     DISPLAY "EX-VERIFICA-AS " ELENCO-AS.
       EX-VERIFICA-AS. EXIT.
      *
      *
       VERIFICA-CL.
      *      DISPLAY "VERIFICA-CL".
            MOVE SPACES TO ELENCO-CL.
            DISPLAY "CL >> (elenco separato da ,)".
            ACCEPT ELENCO-CL.  
           
           IF TUTTI-CL-ELENCO
               DISPLAY "Tratto tutti le CL"
               MOVE SPACES TO ELENCO-CL
               PERFORM VARYING I-APP FROM 1 BY 1 UNTIL I-APP > 99
                    MOVE I-APP TO CL-IN-R(I-APP)
               END-PERFORM
           ELSE
               MOVE 0 TO SW-CL-ERR
               PERFORM VARYING I-CL FROM 1 BY 1
                 UNTIL I-CL > 10 OR CL-ERR     
                  MOVE 2 TO QD-LL-A QD-LL-B                                           
                  MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC                             
                  CANCEL "QDEEDIT"
                  CALL "QDEEDIT" USING PARDEED CL-IN(I-CL)                                  
                                             CL-IN-R(I-CL)                                 
                  IF QD-STATO OF PARDEED NOT = 0  
      *            OR (QD-STATO OF PARDEED = 0 AND CL-IN(I-CL)  = 0)                   
                      DISPLAY "Cl Err. " CL-IN(I-CL)                  
                      MOVE 1 TO SW-CL-ERR
                  END-IF
                END-PERFORM
                IF CL-ERR
                    GO TO VERIFICA-CL.
                
      *      DISPLAY "EX-VERIFICA-CL " ELENCO-CL.
       EX-VERIFICA-CL. EXIT.

      **MAXCA*
        VERIFICA-MAX-CAPI.  
      *      DISPLAY "VERIFICA-MAX-CAPI".
069900      DISPLAY "MaxCapi >> (vuoto = tutti) " NO ADVANCING.                             
070000      ACCEPT MAX-CAPI-INPUT.                     
070100      MOVE 6 TO QD-LL-A QD-LL-B.                                           
070200      MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC.                             
            CANCEL "QDEEDIT"
070300      CALL "QDEEDIT" USING PARDEED MAX-CAPI-INPUT                                  
070400                             MAX-CAPI-INPUT-R. 
070500      IF QD-STATO OF PARDEED NOT = 0 
070700        DISPLAY "MaxCapi Err. " 
              GO TO VERIFICA-MAX-CAPI.
              
      *       MOVE SPACE TO MAX-CAPI-INPUT.
      *      DISPLAY "EX-VERIFICA-MAX-CAPI " MAX-CAPI-INPUT-R.
        EX-VERIFICA-MAX-CAPI.
            EXIT.

      *UNICODDT*                                                        FINE


      *UNICODDT*   TOLTO PER CARICARE TAB-UNICO-DDT                     INIZIO
      * VERIF-AS-SOC-CL.
      *     MOVE SPACE TO AS-INPUT.
      **ASZERO*
069900**     DISPLAY "AS >> (vuoto = tutti) " NO ADVANCING.                             
      *     DISPLAY "AS >> (tt = tutti) " NO ADVANCING.                             
      **
070000*     ACCEPT AS-INPUT.
      **ASZERO*
      *     IF TUTTI-AS
      *        DISPLAY "Tratto tutti gli AS"
      *     ELSE
      **
070100*        MOVE 2 TO QD-LL-A QD-LL-B                                           
070200*        MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC                             
      *        CANCEL "QDEEDIT"
070300*        CALL "QDEEDIT" USING PARDEED AS-INPUT                                  
070400*                               AS-INPUT-R                                 
070500*        IF (QD-STATO OF PARDEED NOT = 0)
      **ASZERO*
      *           OR (AS-INPUT-R = 0)
      **
      **     OR                                    
070600**      (QD-STATO OF PARDEED = 0 AND S-INPUT-R NOT = 2 AND
      **             S-INPUT-R NOT = 4)                    
070700*          DISPLAY "AS Err. " AS-INPUT                  
      *          GO TO EX-VERIF-AS-SOC-CL
      *     END-IF.
      **
      *     MOVE SPACE TO SOCIETA-INPUT.
069900*     DISPLAY "Soc >> (vuoto = tutti) " NO ADVANCING.                             
070000*     ACCEPT SOCIETA-INPUT.                                                       
070100*     MOVE 1 TO QD-LL-A QD-LL-B.                                           
070200*     MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC.                             
      *     CANCEL "QDEEDIT"
070300*     CALL "QDEEDIT" USING PARDEED SOCIETA-INPUT                                  
070400*                            SOCIETA-INPUT-R.                                 
070500*     IF QD-STATO OF PARDEED NOT = 0                    
070700*       DISPLAY "Soc Err. " SOCIETA-INPUT                  
      *       GO TO EX-VERIF-AS-SOC-CL.      
073900**                   
      *     MOVE SPACE TO CLASSE-INPUT.
069900*     DISPLAY "Cl >> " NO ADVANCING.                             
070000*     ACCEPT CLASSE-INPUT.                                                       
070100*     MOVE 2 TO QD-LL-A QD-LL-B.                                           
070200*     MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC.                             
      *     CANCEL "QDEEDIT"
070300*     CALL "QDEEDIT" USING PARDEED CLASSE-INPUT                                  
070400*                            CLASSE-INPUT-R.                                 
070500*     IF QD-STATO OF PARDEED NOT = 0  OR                                    
070600*      (QD-STATO OF PARDEED = 0 AND CLASSE-INPUT-R  = 0)                   
070700*       DISPLAY "Cl Err. " CLASSE-INPUT                  
      *       GO TO EX-VERIF-AS-SOC-CL.
      **
      *     MOVE 1 TO OK-NEG.
      * EX-VERIF-AS-SOC-CL.
      *     EXIT.
      *UNICODDT*                                                        FINE

         CALL-DISIMPEGNA-MAG.
             CALL "PYTHON" USING "disimpegna_capi"
                                "elimina_impegnati"
                                 PY-INPUT-REC-DISIMPEGNA
                                 PY-OUTPUT-DISIMPEGNO.
         EX-CALL-DISIMPEGNA-MAG. EXIT.

      *
074000*                                                                         
074100 INIZIA-TAB-ART.                                                          
074200     MOVE 0 TO QT-STATO OF PARTAB-ART                                     
074300               QT-NUM-ELEM-EFF OF PARTAB-ART                              
074400               QT-INDEX-ELEM OF PARTAB-ART.                               
074500     MOVE NUM-ELEM-MAX-ART TO QT-NUM-ELEM-MAX OF PARTAB-ART.              
074600     MOVE "K1" TO QT-FUNZIONE OF PARTAB-ART.   
      *VACO*                                                            inizio
074700**BUDA*                                                                    
074800*     MOVE 75 TO QT-LL-ELEM OF PARTAB-ART.
092000*     MOVE 79 TO QT-LL-ELEM OF PARTAB-ART.
            COMPUTE QT-LL-ELEM OF PARTAB-ART = 
                     FUNCTION LENGTH(ART-TAB-LETTI (1)). 
      *VACO*                                                            fine
074900     MOVE 1 TO QT-ADDR-KEY OF PARTAB-ART.                                 
075000     MOVE 8 TO QT-LL-KEY OF PARTAB-ART.                                   
075100 EX-INIZIA-TAB-ART.                                                       
075200     EXIT.                                                                
075300*                                                                         
075400*                                                                         
075500 INIZIA-TAB-SING.                                                         
075600     MOVE 0 TO QT-STATO OF PARTAB-SING                                    
075700               QT-NUM-ELEM-EFF OF PARTAB-SING                             
075800               QT-INDEX-ELEM OF PARTAB-SING.                              
075900     MOVE 9999 TO QT-NUM-ELEM-MAX OF PARTAB-SING.
076000     MOVE "K1" TO QT-FUNZIONE OF PARTAB-SING.                             
      *MOVSKU                            
076100*     MOVE 23 TO QT-LL-ELEM OF PARTAB-SING.       
           MOVE 36 TO QT-LL-ELEM OF PARTAB-SING.                                  
076200     MOVE 1 TO QT-ADDR-KEY OF PARTAB-SING.                                
076300     MOVE 10 TO QT-LL-KEY OF PARTAB-SING.                                 
076400 EX-INIZIA-TAB-SING.                                                      
076500     EXIT.                                                                
076600*                                                                         
076700 LEGGI-PARAMDT.                                                           
076800     MOVE "DPARAM;" TO W-NOME-DATA-SET.                                   
076900     MOVE "C-AZIENDA;" TO W-NOME-CAMPO.                                   
077000     MOVE 0 TO W-VALORE-CAMPO-HW.                                         
077100     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
077200     IF NOT W-OK-IMAGE                                                    
           CANCEL "QDBERROR"
077300        CALL "QDBERROR" USING W-COMMON.                                   
077400     MOVE 5 TO W-MODO.                                                    
077500     PERFORM TTDBGET THRU EX-TTDBGET.                                     
077600     MOVE AREA-REC-SET TO REC-PARAM-RID.                                  
077700     PERFORM DBGET-PARAMDT THRU EX-DBGET-PARAMDT                          
077800      UNTIL                                                               
077900           W-FINE-CATENA  OR                                              
078000           P-EXTRAMAG OF REC-PARAMETRI.                                                    
078100     IF W-FINE-CATENA                                                     
           CANCEL "QDBERROR"
078200        CALL "QDBERROR" USING W-COMMON                                    
078300       ELSE                                                               
078400          MOVE W-WORD-ATT TO INDIRIZZO-DPARAM                             
078500          MOVE NUM-BOLLA-TAGLIO-FODERE TO NUMERO-DDT.                     
078600 EX-LEGGI-PARAMDT.                                                        
078700     EXIT.                                                                
078800*                                                                         
078900*                                                                         
079000 DBGET-PARAMDT.                                                           
079100     PERFORM TTDBGET THRU EX-TTDBGET.                                     
079200     MOVE AREA-REC-SET TO REC-PARAM-RID.                                  
079300 EX-DBGET-PARAMDT.                                                        
079400     EXIT.                                                                
079500*
      *volante*
076600*                                                                         
076700 LEGGI-PARAMDT-FITTIZI.                                                           
076800     MOVE "DPARAM;" TO W-NOME-DATA-SET.                                   
076900     MOVE "C-AZIENDA;" TO W-NOME-CAMPO.                                   
077000     MOVE 0 TO W-VALORE-CAMPO-HW.                                         
077100     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
077200     IF NOT W-OK-IMAGE                                                    
           CANCEL "QDBERROR"
077300        CALL "QDBERROR" USING W-COMMON.                                   
077400     MOVE 5 TO W-MODO.                                                    
077500     PERFORM TTDBGET THRU EX-TTDBGET.                                     
077600     MOVE AREA-REC-SET TO REC-PARAM-FITTIZ-R.                                                                           
077700     PERFORM DBGET-PARAMDT-FTZ THRU EX-DBGET-PARAMDT-FTZ                          
077800      UNTIL                                                               
077900           W-FINE-CATENA  OR                                              
078000           P-RIPRADI OF REC-PARAM-FITTIZI.                                                    
078100     IF W-FINE-CATENA                                                     
           CANCEL "QDBERROR"
078200        CALL "QDBERROR" USING W-COMMON                                    
078300       ELSE                                                               
078400          MOVE W-WORD-ATT TO INDIRIZZO-DPARAM                             
078500          MOVE PAR-FITTIZIO-1 TO NUMERO-DDT.                     
078600 EX-LEGGI-PARAMDT-FITTIZI.                                                        
078700     EXIT.                                                                
078800*
078900*                                                                         
079000 DBGET-PARAMDT-FTZ.                                                           
079100     PERFORM TTDBGET THRU EX-TTDBGET.                                     
079200     MOVE AREA-REC-SET TO REC-PARAM-FITTIZ-R.                                  
079300 EX-DBGET-PARAMDT-FTZ.                                                        
079400     EXIT.                                                                
079500*
079500*
      *
079600*PAGE                                                                     
079700*                                                                         
079800*                                                                         
079900 VERIF-NEG.                                                               
080000     MOVE SPACES TO D-CONTO-MEM                                           
080100                    INDIRIZZO-COM INDIRIZZO-C-COM                         
080200                    LOCALITA-COM LOCALITA-C-COM                           
080300                    CONTO-IN                                              
080400                    PROV-COM PROV-C-COM.                                  
080500     MOVE 0 TO CAP-COM CAP-C-COM.                                         
080600*    DISPLAY " ".                                                         
080700*                  NO ADVANCING.                                          
080800*    DISPLAY "m".                                                        
080900*    DISPLAY "hJ".                                                      
081000*       PERFORM VARYING IR FROM 1 BY 1                                    
081100*            UNTIL IR > SALTO                                             
081200        DISPLAY SPACE                                                     
081300*       END-PERFORM                                                       
081400     DISPLAY "CONTO cliente (8 cifre)".                                   
081500     DISPLAY "  (END/end=fine)"                                               
081600     ACCEPT CONTO-IN.                                                     
081700     IF CONTO-FINE = "END" or = "end"
081800        MOVE 1 TO OK-NEG                                                  
081900                  USCITA-PROGRAMMA                                        
082000       ELSE                                                               
082100          IF CONTO-IN NOT NUMERIC                                         
082200             DISPLAY "CODICE non numerico"                                
                   PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
082300            ELSE                                                          
082400               MOVE "ANACON;" TO W-NOME-DATA-SET                          
082500               MOVE "CONTO;" TO W-NOME-CAMPO                              
082600               MOVE CONTO-IN-R TO W-VALORE-CAMPO-W                        
082700               MOVE 7 TO W-MODO                                           
082800               PERFORM TTDBGET THRU EX-TTDBGET                            
082900               IF NOT W-OK-IMAGE                                          
083000                  DISPLAY "Manca CLIENTE " CONTO-IN-R                     
                        PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
083100                 ELSE                                                     
083200                    MOVE D-CONTO OF REC-ANACON TO D-CONTO-MEM             
083300*BUDA*                                                                    
083400                    MOVE FLAG-ANA-8 TO FLAG-ANACON                        
083500                    IF FILIALE-DT-ESTERO                                  
083600                      MOVE 1 TO FLAG-DT-ESTERO                            
083700                    ELSE                                                  
083800                      MOVE 0 TO FLAG-DT-ESTERO                            
083900                    END-IF                                                
084000                    PERFORM LEGGI-IND THRU EX-LEGGI-IND                   
084100                    PERFORM MUOVI-IND THRU EX-MUOVI-IND                   
084200                    PERFORM MUOVI-CAP THRU EX-MUOVI-CAP                   
084300*FIFRA*                                                                   
084400                    PERFORM SCEGLI-CONTO-FATTURA                          
084500                         THRU EX-SCEGLI-CONTO-FATTURA                     
084600*BUDA*                                                                    
084700                    PERFORM CERCA-LISTINO                                 
084800                        THRU EX-CERCA-LISTINO                             
084900                    MOVE 1 TO OK-NEG.                                     
085000 EX-VERIF-NEG.                                                            
085100     EXIT.                                                                
085200*                                                                         
085300*                                                                         
085400*PAGE                                                                     
085500*                                                                         
085600*                                                                         
085700 TRATTA-OLD-NEW.                                                          
085800*    DISPLAY "m".                                                        
085900*    DISPLAY "hJ".                                                      
086000*    PERFORM VARYING IR FROM 1 BY 1                                       
086100*            UNTIL IR > SALTO                                             
086200        DISPLAY SPACE                                                     
086300*    END-PERFORM.                                                         
086400*    DISPLAY "hJ".                                                      
086500*    DISPLAY "&dB       Destinatario      &d@"                          
086600*               "&dB Vendita STOCK >>  &d@ "  D-CONTO-MEM.              
086700     DISPLAY D-CONTO-MEM.                                   
086800*MAG6/7*                                                                  
086900*    DISPLAY " ".                                                         
087000     DISPLAY "dal mag " MAG-INPUT.                           
087100     MOVE 0 TO IND-CL TOT-BOLLA-C IND-CAPI-LETTI                          
087200                       IND-CAPI-NO-GIAC.                                  
087300*                                                                         
087400     MOVE LOW-VALUE TO TABELLA-ARTICOLI-LETTI.                            
087500     INITIALIZE TABELLA-NO-GIAC.                                          
087600     MOVE SPACES TO COD-IN MEM-COD-IN.                                    
087700*     DISPLAY " . fine lettura".                                           
087800*     DISPLAY " % annulla ultimo letto".                                   
087900*     DISPLAY " S stampa rapportino".                                      
088000**    DISPLAY "l".                                                        
088100*     PERFORM TRATTA-LEGGI THRU EX-TRATTA-LEGGI                            
088200*               UNTIL LETT-FINE OR                                         
088300*               QT-NUM-ELEM-EFF OF PARTAB-ART = NUM-ELEM-MAX-ART
           PERFORM TRATTA-SITPF-3 THRU EX-TRATTA-SITPF-3.
      *     
088400**    DISPLAY "m".      
087900     DISPLAY " S stampa rapportino". 
099400     MOVE SPACES TO COD-IN.                                               
099500     ACCEPT COD-IN. 
100200     IF LETT-STAMPA                                                       
100300       PERFORM STAMPA-RAPPORTINO THRU EX-STAMPA-RAPPORTINO                
100400       DISPLAY "   rapportino stampato".
088500 EX-TRATTA-OLD-NEW.                                                       
088600     EXIT.                                                                
088700*                                                                         
088800*                                                                         
088900*PAGE                                                                     
089000*                                                                         
089100*                                                                         
089200 LEGGI-IND.                                                               
089300     MOVE "CONTO;"  TO W-NOME-CAMPO.                                      
089400     MOVE "INDIRIZ;" TO W-NOME-DATA-SET.                                  
089500     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
089600     MOVE 5 TO W-MODO.                                                    
089700     PERFORM TTDBGET THRU EX-TTDBGET.                                     
089800     MOVE AREA-REC-SET TO REC-INDIRIZZI.                                  
089900     IF NOT W-OK-IMAGE                                                    
090000        DISPLAY "ERR INDIRIZZI" CONTO-IN-R                                
              PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
090100        PERFORM AZZERA-CAMPI-INDIRIZ THRU EX-AZZERA-CAMPI-INDIRIZ.        
090200 EX-LEGGI-IND.                                                            
090300     EXIT.                                                                
090400*                                                                         
090500*                                                                         
090600 AZZERA-CAMPI-INDIRIZ.                                                    
090700     MOVE SPACE TO D-AGG STATO OF REC-INDIRIZZI                           
090800                   SIGLA-PROV OF REC-INDIRIZZI (1)                        
090900                   SIGLA-PROV OF REC-INDIRIZZI (2)                        
091000                   INDIRIZZO OF REC-INDIRIZZI (1)                         
091100                   INDIRIZZO OF REC-INDIRIZZI (2)                         
091200                   STATO OF REC-INDIRIZZI.                                
091300     MOVE 0 TO CAP OF REC-INDIRIZZI (1)                                   
091400               CAP OF REC-INDIRIZZI (2)                                   
091500*BUDA*                                                                    
091600               PRIORITA OF REC-INDIRIZZI                                  
091700               TELEFONO OF REC-INDIRIZZI.                                 
091800*FIFRA*                                                                   
091900     MOVE 0 TO TELEX OF REC-INDIRIZZI.                                    
092000*                                                                         
092100 EX-AZZERA-CAMPI-INDIRIZ.                                                 
092200     EXIT.                                                                
092300*                                                                         
092400*                                                                         
092500 MUOVI-IND.                                                               
092600     MOVE INDIRIZZO OF REC-INDIRIZZI (1) TO INDIRIZZO-STD.                
092700     MOVE 66 TO LL-STRINGA-IND.                                           
092800     MOVE 60 TO LL-SUBSTRINGA-IND.                                        
092900     MOVE 1 TO FUNZIONE-IND.                                              
           CANCEL "QSTRINGV"
093000     CALL "QSTRINGV" USING PAR-INDIRIZZO                                  
093100                           INDIRIZZO-STD                                  
093200                           INDIRIZZO-COM.                                 
093300     MOVE 2 TO FUNZIONE-IND.                                              
           CANCEL "QSTRINGV"
093400     CALL "QSTRINGV" USING PAR-INDIRIZZO                                  
093500                           INDIRIZZO-STD                                  
093600                           LOCALITA-COM.                                  
093700     IF INDIRIZZO OF REC-INDIRIZZI (2) NOT = SPACE                        
093800        MOVE INDIRIZZO OF REC-INDIRIZZI (2) TO INDIRIZZO-STD              
093900        MOVE 1 TO FUNZIONE-IND                                            
           CANCEL "QSTRINGV"
094000        CALL "QSTRINGV" USING PAR-INDIRIZZO                               
094100                              INDIRIZZO-STD                               
094200                              INDIRIZZO-C-COM                             
094300        MOVE 2 TO FUNZIONE-IND                                            
           CANCEL "QSTRINGV"
094400        CALL "QSTRINGV" USING PAR-INDIRIZZO                               
094500                              INDIRIZZO-STD                               
094600                              LOCALITA-C-COM.                             
094700 EX-MUOVI-IND.                                                            
094800     EXIT.                                                                
094900*                                                                         
095000*                                                                         
095100 MUOVI-CAP.                                                               
095200     MOVE D-AGG OF REC-INDIRIZZI TO D-CONTO-AGG-MEM.                      
095300     MOVE STATO OF REC-INDIRIZZI TO STATO-COM.                            
095400     MOVE CAP OF REC-INDIRIZZI (1) TO CAP-COM.                            
095500     MOVE SIGLA-PROV OF REC-INDIRIZZI (1) TO PROV-COM.                    
095600     IF INDIRIZZO OF REC-INDIRIZZI (2) NOT = SPACE                        
095700        MOVE SIGLA-PROV OF REC-INDIRIZZI (2) TO PROV-C-COM                
095800        MOVE CAP OF REC-INDIRIZZI (2) TO CAP-C-COM.                       
095900 EX-MUOVI-CAP.                                                            
096000     EXIT.                                                                
096100*                                                                         
096200*                                                                         
096300*BUDA*                                                                    
096400*                                                                         
096500 CERCA-LISTINO.                                                           
096600     MOVE "CONTO;"  TO W-NOME-CAMPO.                                      
096700     MOVE "CONFATT;" TO W-NOME-DATA-SET.                                  
096800     MOVE CONTO-IN-R TO W-VALORE-CAMPO-W.                                 
096900     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
097000     MOVE 5 TO W-MODO.                                                    
097100     PERFORM TTDBGET THRU EX-TTDBGET.                                     
097200     MOVE AREA-REC-SET TO REC-CONFATT.                                    
097300     MOVE LISTINO OF REC-CONFATT TO LISTINO-MEM.                          
097400     MOVE DIVISA OF REC-CONFATT TO DIVISA-MEM.                            
097500 EX-CERCA-LISTINO.                                                        
097600     EXIT.                                                                
097700*                                                                         
097800*                                                                         
097900*FIFRA*                                                                   
098000 SCEGLI-CONTO-FATTURA.                                                    
098100     IF TELEFONO OF REC-INDIRIZZI = 0                                     
098200       MOVE 0 TO CONTO-FATTURA-MEM                                        
098300     ELSE                                                                 
098400       MOVE TELEX OF REC-INDIRIZZI TO CONTO-FATTURA-MEM.                  
098500 EX-SCEGLI-CONTO-FATTURA.                                                 
098600     EXIT.                                                                
098700*                                                                         
098800*                                                                         
098900*                                                                         
099000*PAGE  
      *
      *
       TRATTA-SITPF-3.
           MOVE "SITPF" TO W-NOME-DATA-SET.
           MOVE "MAG" TO W-NOME-CAMPO
           MOVE MAG-INPUT-R TO W-VALORE-CAMPO-HW
           PERFORM TTDBFIND THRU EX-TTDBFIND.
           IF W-OK-IMAGE
             MOVE 5 TO W-MODO
             PERFORM TTDBGET-S THRU EX-TTDBGET-S.
           PERFORM SELEZIONA-SITPF-3 
               THRU EX-SELEZIONA-SITPF-3
088200            UNTIL NOT W-OK-IMAGE OR  
      *MAXCA*
      
      
      *UNICODDT*                                                        INIZIO
      *              (MAX-CAPI-INPUT-R <> 0 AND
      *                   IND-CAPI-LETTI >= MAX-CAPI-INPUT-R) OR

      *            FLAG QUANDO TUTTI I TAB-MAX-CAPI SONO STATI RAGGIUNTI
                   SW-MAX-CAPI-RAGGIUNTO = 1 OR 
      *UNICODDT*                                                        FINE
      *
088300              QT-NUM-ELEM-EFF OF PARTAB-ART = NUM-ELEM-MAX-ART
                    OR  QT-NUM-ELEM-EFF OF PARTAB-SING = 
                         QT-NUM-ELEM-MAX OF PARTAB-SING.      
           IF QT-NUM-ELEM-EFF OF PARTAB-ART = NUM-ELEM-MAX-ART
             DISPLAY SPACE
             DISPLAY "TABELLA-ARTICOLI-LETTI "
             DISPLAY " da allargare"
           END-IF.
           IF QT-NUM-ELEM-EFF OF PARTAB-SING = 
                         QT-NUM-ELEM-MAX OF PARTAB-SING
             DISPLAY SPACE
             DISPLAY "TABELLA-SINGOLI "
             DISPLAY " da allargare"       
           END-IF                         
           MOVE  "." TO COD-IN-RID.
       EX-TRATTA-SITPF-3.
           EXIT.
      *
       CALCOLA-AS-CL.
             MOVE 0 TO SW-ERR-AS-CL.
103800       MOVE C-MAT-TRANS-RID TO W-VALORE-CAMPO                             
103900       MOVE "ANAMAT;" TO W-NOME-DATA-SET                                  
104000       MOVE "C-MAT;" TO W-NOME-CAMPO                                      
104100       MOVE 7 TO W-MODO                                                   
104200       PERFORM TTDBGET THRU EX-TTDBGET                                    
104300       IF NOT W-OK-IMAGE                                                  
104400          DISPLAY "Inesist. " C-MAT-A-BARRE-RID                           
                PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
                MOVE 1 TO SW-ERR-AS-CL
            ELSE
                MOVE ANNO OF REC-ANAMAT TO APP-A
                MOVE STAGIONE OF REC-ANAMAT TO APP-S
                MOVE APP-AS TO TAB-AS
                MOVE CL-GR OF REC-ANAMAT TO TAB-CL
                PERFORM LEGGI-TAB-UNICO-DDT THRU 
                     EX-LEGGI-TAB-UNICO-DDT
                IF QT-STATO OF PAR-TAB-UNICO-DDT NOT = 0
                    MOVE 1 TO SW-ERR-AS-CL.
                     
           MOVE "SITPF" TO W-NOME-DATA-SET
           MOVE 5 TO W-MODO.
       EX-CALCOLA-AS-CL. EXIT.
       
      
      
      
       SELEZIONA-SITPF-3.
        
      *      DISPLAY "sono seleziona-sitpf-3"
      *      display "return per proseguire".                                    valeria 
      *      accept pro.
       
           IF MAGAZZINO OF REC-SITPF NOT = MAG-INPUT-R
             PERFORM TTDBGET-S THRU EX-TTDBGET-S
             GO TO EX-SELEZIONA-SITPF-3.
      *
           MOVE C-MAT OF REC-SITPF
               TO C-MAT-TRANS-RID .
               
      *UNICODDT*                                                        INIZIO
           PERFORM CALCOLA-AS-CL THRU EX-CALCOLA-AS-CL
           IF ERR-AS-CL
                PERFORM TTDBGET-S THRU EX-TTDBGET-S
                GO TO EX-SELEZIONA-SITPF-3.
           
           IF XD = "S"
                DISPLAY "sono dopo calcola-as-cl " C-MAT-TRANS-RID
                display dep-tab-unico-ddt
      *          display "return per proseguire"                                    
      *          accept pro
                .
          
           IF QT-STATO OF PAR-TAB-UNICO-DDT = 0
           AND TAB-MAX-CAPI <> 0  
           AND TAB-CAPI-LETTI >= TAB-MAX-CAPI
      *         VALERIA - HO SUPERATO IL LIMITE DI AS CL
      *         VERIFICO SE TUTTI I LIMITI SONO STATI SUPERATI     
                MOVE 1 TO SW-MAX-CAPI-RAGGIUNTO
                PERFORM VARYING I-APP FROM 1 BY 1 
                    UNTIL I-APP > QT-NUM-ELEM-EFF OF PAR-TAB-UNICO-DDT
                       OR SW-MAX-CAPI-RAGGIUNTO = 0
                          MOVE ELE-TAB-UNICO-DDT (I-APP)
                            TO DEP-TAB-UNICO-DDT
      *                    display dep-tab-unico-ddt
                          IF TAB-MAX-CAPI > TAB-CAPI-LETTI
                          OR TAB-MAX-CAPI = 0
                              MOVE 0 TO SW-MAX-CAPI-RAGGIUNTO
                          END-IF
                END-PERFORM  
                
                IF XD = "S"    
                    DISPLAY "sono dopo calcolo max capi raggiunto = "
                     SW-MAX-CAPI-RAGGIUNTO
      *              display "return per proseguire"                                   
      *              accept pro
                END-IF
           
                PERFORM TTDBGET-S THRU EX-TTDBGET-S
                GO TO EX-SELEZIONA-SITPF-3.

           MOVE APP-AS TO TAB-AS.
           MOVE CL-GR OF REC-ANAMAT TO TAB-CL.
           PERFORM LEGGI-TAB-UNICO-DDT THRU 
                EX-LEGGI-TAB-UNICO-DDT.         
      *UNICODDT*                                                        FINE
               
      *UNICODDT*                                                        INIZIO
      *     IF CLASSE OF C-MAT-TRANSITO NOT = CLASSE-INPUT-R
      *       PERFORM TTDBGET-S THRU EX-TTDBGET-S
      *       GO TO EX-SELEZIONA-SITPF-3.
      *UNICODDT*                                                        FINE
      *
           IF SOCIETA-INPUT-R NOT = 0 AND
              SOCIETA-MOD OF C-MAT-TRANSITO 
                     NOT = SOCIETA-INPUT-R
             PERFORM TTDBGET-S THRU EX-TTDBGET-S
             GO TO EX-SELEZIONA-SITPF-3.
      *                                                                         
           PERFORM VARYING IT FROM 1 BY 1
                   UNTIL IT > NTG-NTG
                   OR  QT-NUM-ELEM-EFF OF PARTAB-SING = 
                         QT-NUM-ELEM-MAX OF PARTAB-SING
                         
      *MAXCA*
      
      *UNICODDT*                                                        INIZIO
      *            OR (MAX-CAPI-INPUT-R <> 0 AND
      *                  IND-CAPI-LETTI >= MAX-CAPI-INPUT-R) 
                   OR (TAB-MAX-CAPI <> 0 AND
                       TAB-CAPI-LETTI >= TAB-MAX-CAPI)
      *UNICODDT*                                                        FINE
      *                         
              IF DISIMPEGNA = "SI"
                 MOVE QTA-GIAC-PF OF REC-SITPF(IT)
                   TO DA-TRASFERIRE
              ELSE
                 COMPUTE DA-TRASFERIRE =
                         QTA-GIAC-PF OF REC-SITPF(IT)
                         + QTA-IMP OF REC-SITPF(IT)
              END-IF
              PERFORM VARYING IC FROM 1 BY 1 
                       UNTIL IC > DA-TRASFERIRE
                       OR  QT-NUM-ELEM-EFF OF PARTAB-SING = 
                         QT-NUM-ELEM-MAX OF PARTAB-SING   
      *MAXCA*
      *UNICODDT*                                                        INIZIO
      *             OR (MAX-CAPI-INPUT-R <> 0 AND
      *                   IND-CAPI-LETTI >= MAX-CAPI-INPUT-R) 
                   OR (TAB-MAX-CAPI <> 0 AND
                       TAB-CAPI-LETTI >= TAB-MAX-CAPI)
      *UNICODDT*                                                        FINE
      *                         
                MOVE C-MAT OF REC-SITPF
                   TO C-MAT-TRANS-RID 
089605          MOVE MODELLO OF C-MAT-TRANSITO                                       
089606              TO MODELLO OF C-MAT-A-BARRE                                 
089607          MOVE VEST-A      OF C-MAT-TRANSITO                                   
089608             TO VESTIBILITA OF C-MAT-A-BARRE                                  
089609          MOVE PEZZO-A OF C-MAT-TRANSITO                                       
089610             TO PEZZO OF C-MAT-A-BARRE                                        
089611          MOVE PROGR-ART OF C-MAT-TRANSITO                                     
089612             TO SOCIETA OF C-MAT-A-BARRE                                      
089613          MOVE COLORE  OF C-MAT-TRANSITO                                       
089614             TO VARIANTE-COL OF C-MAT-A-BARRE
                MOVE FUNCTION tgxid(IT) 
                         TO TAGLIA OF C-MAT-A-BARRE
                MOVE IT TO NTG-MEM
      *          MOVE IT TO TAGLIA OF C-MAT-A-BARRE
                PERFORM TRATTA-LEGGI THRU EX-TRATTA-LEGGI
              END-PERFORM
           END-PERFORM.
      *
           MOVE "SITPF" TO W-NOME-DATA-SET
           MOVE 5 TO W-MODO
           PERFORM TTDBGET-S THRU EX-TTDBGET-S.
       EX-SELEZIONA-SITPF-3.
           EXIT.
099100*                                                                         
099200*                                                                         
099300 TRATTA-LEGGI. 
            IF XD = "S"    
                DISPLAY "sono in tratta-leggi "
                display dep-tab-unico-ddt
      *          display "return per proseguire"                                   
      *          accept pro
                .
           
                                                       
099400*     MOVE SPACES TO COD-IN.                                               
099500*     ACCEPT COD-IN.                                                       
099600*     IF LETT-FINE                                                         
099700*        GO TO EX-TRATTA-LEGGI.                                            
099800*     IF LETT-ANN-ULT                                                      
099900*       PERFORM ANNULLA-PRECEDENTE THRU EX-ANNULLA-PRECEDENTE              
100000*       MOVE SPACES TO MEM-COD-IN                                          
100100*       GO TO EX-TRATTA-LEGGI.                                             
100200*     IF LETT-STAMPA                                                       
100300*       PERFORM STAMPA-RAPPORTINO THRU EX-STAMPA-RAPPORTINO                
100400*       DISPLAY "   rapportino stampato"                                   
100500*       GO TO EX-TRATTA-LEGGI.                                             
100600**                                                                         
100700*     IF C-MAT-A-BARRE-RID NOT NUMERIC                                     
100800*        DISPLAY "COD non num >> RILEGGERE"                                
      *        PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
100900*        GO TO EX-TRATTA-LEGGI.                                            
101000*     IF VARIANTE-COL OF C-MAT-A-BARRE = 0                                 
101100*        DISPLAY "VARIANTE 0 >> RILEGGERE"                                 
      *        PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
101200*        GO TO EX-TRATTA-LEGGI.

101300     MOVE SPACES TO MEM-COD-IN.                                           
101400     MOVE SOCIETA OF C-MAT-A-BARRE TO SOC-COM.                            
101500     MOVE 0 TO PRE-SOC.                                                   
101600     MOVE SOC-COM TO SOCIETA OF C-MAT-A-BARRE.                            
101700     COMPUTE ELEM-ART = C-MAT-A-BARRE-RID / 10
101800     MOVE "K2" TO QT-FUNZIONE OF PARTAB-ART.                              
           CANCEL "QTABEL"
101900     CALL "QTABEL" USING PARTAB-ART TABELLA-ARTICOLI-LETTI                
102000                         ELEM-ART.                                        
102100*     IF TAGLIA OF C-MAT-A-BARRE = 0                                       
102200*           OR = 9                                                         
102300*      DISPLAY "TAGLIE da  1  a  8"                                
      *        PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
102400*        GO TO EX-TRATTA-LEGGI.                                            
102500*    IF QT-STATO OF PARTAB-ART NOT = 0                                    
102600       MOVE 0 TO C-MAT-TRANS-RID                                          
102700       MOVE MODELLO OF C-MAT-A-BARRE TO MODELLO OF                        
102800                         C-MAT-TRANSITO                                   
102900       MOVE VESTIBILITA OF C-MAT-A-BARRE TO                               
103000              VEST-A OF C-MAT-TRANSITO                                    
103100       MOVE SOCIETA OF C-MAT-A-BARRE TO                                   
103200                      PROGR-ART OF C-MAT-TRANSITO                         
103300       MOVE PEZZO OF C-MAT-A-BARRE TO PEZZO-A OF                          
103400                         C-MAT-TRANSITO                                   
103500       MOVE VARIANTE-COL OF C-MAT-A-BARRE TO COLORE OF                    
103600                         C-MAT-TRANSITO.                                  
103700     IF QT-STATO OF PARTAB-ART NOT = 0                                    
103800       MOVE C-MAT-TRANS-RID TO W-VALORE-CAMPO                             
103900       MOVE "ANAMAT;" TO W-NOME-DATA-SET                                  
104000       MOVE "C-MAT;" TO W-NOME-CAMPO                                      
104100       MOVE 7 TO W-MODO                                                   
104200       PERFORM TTDBGET THRU EX-TTDBGET                                    
104300       IF NOT W-OK-IMAGE                                                  
104400          DISPLAY "Inesist. " C-MAT-A-BARRE-RID                           
                PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
104500          GO TO EX-TRATTA-LEGGI.
      *ASZERO*
      *     IF (A-INPUT-R NOT = 0 AND 
      *         A-INPUT-R NOT = ANNO OF REC-ANAMAT) OR
      *        (S-INPUT-R NOT = 0 AND
      *         S-INPUT-R NOT = STAGIONE OF REC-ANAMAT)

      *UNICODDT*                                                        INIZIO
      *  QUESTO TEST NON SERVE PIU' PERCHE' GIA' FILTRATO 
      *     IF NOT TUTTI-AS AND (
      *        (A-INPUT-R NOT = ANNO OF REC-ANAMAT) OR
      *        (S-INPUT-R NOT = STAGIONE OF REC-ANAMAT) )
      *       GO TO EX-TRATTA-LEGGI.
      *UNICODDT*                                                        FINE

      *BARBARELLA 260516    
      *NO FORN = 4 PER AI 05*
      *     IF QT-STATO OF PARTAB-ART NOT = 0
      *        IF SOCIETA OF C-MAT-A-BARRE = 4
      *        AND ANNO OF REC-ANAMAT = 5 AND
      *        STAGIONE OF REC-ANAMAT = 4
      *           DISPLAY "FORNITORE 4 NON AMMESSO PER AI 05"
      *           PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
      *           GO TO EX-TRATTA-LEGGI.

104600     IF QT-STATO OF PARTAB-ART NOT = 0                                    
104700       MOVE D-MAT OF REC-ANAMAT TO D-MAT-MEM                              
104800*BUDA*                                                                    
104900       MOVE PRIMA-TG OF REC-ANAMAT TO PTG-MEM                             
105000       MOVE VALID-REC OF REC-ANAMAT TO VAL-REC-MEM   
      *VACO*                                                            inizio
             MOVE COSTO OF REC-ANAMAT  TO COSTO-MEM
      *VACO*                                                            fine
      *ASOLOB2C*                                                        inizio
             MOVE C-MAT OF REC-ANAMAT TO ANACST-C-MAT-COM
             MOVE MAG-INPUT-R TO ANACST-MAG-COM
      * NO-DATGE                                                        inizio
      *       PERFORM S-SET-1 THRU S-SET-1-EX 
      * NO-DATGE                                                        fine
             PERFORM RIVALUTA-COSTO-ANAMAT
                 THRU EX-RIVALUTA-COSTO-ANAMAT
             IF ANACST-CST-COM NOT = 0
               MOVE ANACST-CST-COM TO COSTO-MEM
             END-IF 
      * NO-DATGE                                                        inizio
      *       PERFORM S-SET-2 THRU S-SET-2-EX 
      * NO-DATGE                                                        fine
      
      *ASOLOB2C*                                                        fine
105100*       MOVE "SITPF;" TO W-NOME-DATA-SET                                   
105200*       MOVE "C-MAT;" TO W-NOME-CAMPO                                      
105300*       MOVE C-MAT-TRANS-RID TO W-VALORE-CAMPO                             
105400*       PERFORM TTDBFIND THRU EX-TTDBFIND                                  
105500*       IF W-OK-IMAGE                                                      
105600*          MOVE 5 TO W-MODO                                                
105700*          PERFORM TTDBGET-S THRU EX-TTDBGET-S                             
105800*          PERFORM TTDBGET-S THRU EX-TTDBGET-S                             
105900*                   UNTIL NOT W-OK-IMAGE OR                                
106000**MAG6/7*                                                                  
106100**                     MAGAZZINO OF REC-SITPF = 7                          
106200*                      MAGAZZINO OF REC-SITPF = MAG-INPUT-R                
106300*       END-IF                                                             
106400*       IF NOT W-OK-IMAGE                                                  
106500*          DISPLAY "Manca Sit  "                                           
106600*                       C-MAT-A-BARRE-RID                                  
      *          PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
106700*          GO TO EX-TRATTA-LEGGI.                                          
106800*NOPRZ*                                                                   
106900     MOVE SPACE TO RISP-NO-GIAC RISP-NO-PREZZO.                           
107000*                                                                         
107100*BUDA*                                                                    
107200     MOVE 0 TO PREZZO-MEM.                                                
107300     IF QT-STATO OF PARTAB-ART NOT = 0                                    
      *ACQUO*
107400*           AND (SI-DT-ESTERO OR                                          
107500*          (FLAG-ANACON NOT = '0' AND NOT = ' ') OR                       
107600*          PRIORITA OF REC-INDIRIZZI = 4 )                                
                 AND PRIORITA OF REC-INDIRIZZI = 4
      *
107700       PERFORM CERCA-PREZZO-V THRU EX-CERCA-PREZZO-V                      
107800       IF NOT W-OK-IMAGE OR PREZZO-MEM = 0                                
107900          DISPLAY "Manca prezzo al pubblico "                             
108000          DISPLAY "    " C-MAT-A-BARRE-RID                                
                PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
108100          GO TO EX-TRATTA-LEGGI.                                          
108200*                                                                         
108300     IF QT-STATO OF PARTAB-ART NOT = 0                                    
108400*NOPRZ*                                                                   
108500       MOVE 0 TO PREZZO-MEM PREZZO-ANAMAT                                 
108600*PRODI*                                                                   
108700       MOVE 0 TO CAMBIO-MEM                                               
108800*                                                                         
108900       PERFORM CERCA-PREZZO THRU EX-CERCA-PREZZO                          
109000*NOPRZ*                                                                   
109100*PRZANABU*                                                                
109200*      IF PREZZO-MEM = 0 OR PREZZO-MEM = PREZZO-ANAMAT                    
109300       IF PREZZO-MEM = 0                                                  
109400         OR ( PREZZO-MEM = PREZZO-ANAMAT AND                              
109500                   NOT SI-DT-ESTERO )                                     
      *
109600*          DISPLAY "Prz vend a CLI 0 o senza sconto"                       
109700*          DISPLAY "   " C-MAT-A-BARRE-RID                                 
109800*NOPRZ*                                                                   
109900          MOVE 0 TO OK-PREZZO                                             
110000          PERFORM TRATTA-NO-PREZZO THRU EX-TRATTA-NO-PREZZO               
110100               UNTIL PREZZO-OK                                            
110200          IF RISP-NO-PREZZO NOT = "S"                                     
110300            GO TO EX-TRATTA-LEGGI                                         
110400          END-IF
      *sempre0*
                if prezzo-mem not = 0
                  move 0 to prezzo-mem
                end-if
      *
110500       END-IF                                                             
110600     ELSE                                                                 
110700*PRODI*                                                                   
110800       MOVE CAMBIO-TAB(QT-INDEX-ELEM OF PARTAB-ART)                       
110900           TO CAMBIO-MEM                                                  
111000*                                                                         
111100       MOVE D-MAT-TAB(QT-INDEX-ELEM OF PARTAB-ART)                        
111200           TO D-MAT-MEM                                                   
111300       MOVE PREZZO-TAB(QT-INDEX-ELEM OF PARTAB-ART)                       
111400           TO PREZZO-MEM.                                                 
111500*    IF PREZZO-MEM = 0                                                    
111600*         DISPLAY "Manca prz vend a CLI "                                 
111700*         DISPLAY "   " C-MAT-A-BARRE-RID                                 
111800*NOPRZ*                                                                   
111900*         MOVE 0 TO OK-PREZZO                                             
112000*         PERFORM TRATTA-NO-PREZZO THRU EX-TRATTA-NO-PREZZO               
112100*              UNTIL PREZZO-OK                                            
112200*         IF RISP-NO-PREZZO NOT = "S"                                     
112300*                                                                         
112400*           GO TO EX-TRATTA-LEGGI.                                        
112500*                                                                         
112600     IF QT-STATO OF PARTAB-ART NOT = 0                                    
112700       COMPUTE ELEM-ART = C-MAT-A-BARRE-RID / 10                          
112800       MOVE D-MAT-MEM TO D-MAT-ELEM                                       
112900*BUDA*                                                                    
113000       MOVE PTG-MEM TO PRIMA-TG-ELEM                                      
113100       MOVE PREZZO-MEM TO PREZZO-ELEM    
      *VACO*                                                            inizio
             MOVE COSTO-MEM TO COSTO-ELEM
      *VACO*                                                            fine
113200*PRODI*                                                                   
113300*      MOVE STK-CAMBIO TO CAMBIO-ELEM                                     
113400       MOVE CAMBIO-MEM TO CAMBIO-ELEM                                     
113500       MOVE VAL-REC-MEM TO TIPO-ANA-ELEM                                  
113600       MOVE LOW-VALUE TO QTA-TAGLIE-ELEM                                  
113700       MOVE QTA-GIAC OF REC-SITPF TO QTA-GIAC-ELEM                        
113800     ELSE                                                                 
113900       MOVE ART-TAB-LETTI(QT-INDEX-ELEM OF PARTAB-ART)                    
114000                   TO ART-ELEM-LETTI                                      
114100       MOVE D-MAT-ELEM TO D-MAT-MEM                                       
114200       MOVE PRIMA-TG-ELEM TO PTG-MEM.                                     
114300     COMPUTE PREZZO-D = PREZZO-MEM / 100.                                 
114400     ADD 1 TO QTA-TAGLIA-ELEM(NTG-MEM).                   
114500     IF QTA-GIAC-PF-ELEM(NTG-MEM) <                       
114600             QTA-TAGLIA-ELEM(NTG-MEM)                     
114700           DISPLAY "Manca giac "                                          
114800             C-MAT-A-BARRE-RID                                            
114900        MOVE 0 TO OK-GIAC                                                 
115000        PERFORM TRATTA-NO-GIAC THRU EX-TRATTA-NO-GIAC                     
115100               UNTIL GIAC-OK                                              
115200*NOPRZ*                                                                   
115300        IF RISP-NO-GIAC NOT = "S"                                         
115400*                                                                         
115500           GO TO EX-TRATTA-LEGGI.                                         
115600*NOPRZ1*                                                                  
115700*    IF RISP-NO-GIAC = "S" OR RISP-NO-PREZZO = "S"                        
115800     IF RISP-NO-GIAC = "S"                                                
115900*PRZBU*                                                                   
116000      OR ( RISP-NO-PREZZO = "S" AND SI-DT-ESTERO )                        
116100*                                                                         
116200       PERFORM INSERISCI-NO-GIAC-PREZZO                                   
116300           THRU EX-INSERISCI-NO-GIAC-PREZZO                               
116400       GO TO EX-TRATTA-LEGGI.                                             
116500*                                                                         
116600*                                                                         
116700     ADD 1 TO IND-CAPI-LETTI.                                          
116800     ADD PREZZO-MEM TO PREZZO-TOT.                                        
116900     MOVE IND-CAPI-LETTI TO CONT-D.                                       
           DISPLAY CONT-D.

      *UNICODDT*                                                        INIZIO
           ADD 1 TO TAB-CAPI-LETTI.
           IF XD = "S"
                DISPLAY "sono in add 1 to ind-capi-letti "
                display dep-tab-unico-ddt
      *          display "return per proseguire"                                   
      *          accept pro
                .
      *UNICODDT*                                                        FINE

117000*EURO*                                                                    
117100*EURO1*                                                                   
117200     IF W-FORMATO-INTERNO NOT > 011231                                    
117300       MOVE PREZZO-MEM TO IE-IMPORTO-IN                                   
117400       PERFORM PRZ-INLIT THRU EX-PRZ-INLIT                                
117500       COMPUTE PREZZO-D = IE-IMPORTO-OU / 100                             
117600     ELSE                                                                 
117700       COMPUTE PREZZO-D = PREZZO-MEM / 100.                               
117800*                                                                         
117900*                                                                         
      *     CALL "FAIBEEP".
118000       DISPLAY CONT-D " "
118100                  D-MAT-ELEM SPACE PREZZO-D. 
 
 
      *UNICODDT*                                                        INIZIO
           PERFORM RIMETTI-DEP-TAB-UNICO-DDT THRU
                EX-RIMETTI-DEP-TAB-UNICO-DDT.

           IF XD = "S"
               DISPLAY "sono in tratta-leggi DOPO RIMETTI-DEP-TAB"
               DISPLAY "AS=" TAB-AS " CL=" TAB-CL  
                   " MAX-CAPI=" TAB-MAX-CAPI 
                   " CAPI-LETTI=" TAB-CAPI-LETTI
      *          DISPLAY ELE-TAB-UNICO-DDT(1)
      *          DISPLAY ELE-TAB-UNICO-DDT(2)
      *          DISPLAY ELE-TAB-UNICO-DDT(3)
      *          DISPLAY ELE-TAB-UNICO-DDT(4)
      *          DISPLAY ELE-TAB-UNICO-DDT(5)
      *          display "return per proseguire"                                   
      *          accept pro
                .
      *UNICODDT*                                                        FINE


                           
118200*EURO*                                                                    
118300*EURO1*                                                                   
118400     IF W-FORMATO-INTERNO NOT > 011231                                    
118500       MOVE PREZZO-TOT TO IE-IMPORTO-IN                                   
118600       PERFORM PRZ-INLIT THRU EX-PRZ-INLIT                                
118700       COMPUTE PREZZO-TOT-D = IE-IMPORTO-OU / 100                         
118800*       DISPLAY "  Tot. L." PREZZO-TOT-D                                   
118900     ELSE                                                                 
119000       COMPUTE PREZZO-TOT-D = PREZZO-TOT / 100                            
119100*       DISPLAY " Tot. Eur." PREZZO-TOT-D
           END-IF
119200*                                                                         
119300     PERFORM INSERT-ELEM-SING THRU EX-INSERT-ELEM-SING.                   
119400     IF QT-STATO OF PARTAB-ART = 0                                        
119500       MOVE ART-ELEM-LETTI                                                
119600          TO ART-TAB-LETTI(QT-INDEX-ELEM OF PARTAB-ART)                   
119700     ELSE                                                                 
119800       MOVE "K1" TO QT-FUNZIONE OF PARTAB-ART                             
           CANCEL "QTABEL"
119900       CALL "QTABEL" USING PARTAB-ART TABELLA-ARTICOLI-LETTI              
120000                           ART-ELEM-LETTI                                 
120100       IF QT-STATO OF PARTAB-ART NOT = 0                                  
120200         MOVE QT-STATO OF PARTAB-ART TO ERR-DISP                          
120300         DISPLAY "ERR INSERIM QTABEL " ERR-DISP                           
120400               " - TRATTA-LEGGI"                                          
           CANCEL "QDBERROR"
120500         CALL "QDBERROR" USING W-COMMON.                                  
120600     MOVE COD-IN TO MEM-COD-IN.                                           
120700 EX-TRATTA-LEGGI.                                                         
120800     EXIT.                                                                
120900*                                                                         
121000*                                                                         
121100*NOPRZ*                                                                   
121200 INSERISCI-NO-GIAC-PREZZO.                                                
121300     ADD 1 TO IND-CAPI-NO-GIAC.                                           
121400     IF RISP-NO-GIAC = "S"                                                
121500       DISPLAY "INSERITO Manca GIAC."                                     
121600*PRZBU*                                                                   
121700       MOVE "MancaGIAC" TO CAUSALE-NO-GIAC (IND-CAPI-NO-GIAC).            
121800*                                                                         
121900*NOPRZ1*                                                                  
122000*PRZBU*                                                                   
122100     IF ( RISP-NO-PREZZO = "S" AND SI-DT-ESTERO )                         
122200       DISPLAY "INS. Manca PREZZO x ESTERO"                               
122300       MOVE "MancaPRZ" TO CAUSALE-NO-PRZ (IND-CAPI-NO-GIAC).              
122400*                                                                         
122500     MOVE C-MAT-A-BARRE-RID TO                                            
122600              C-MAT-NO-GIAC (IND-CAPI-NO-GIAC).                           
122700     MOVE D-MAT-MEM TO D-MAT-NO-GIAC (IND-CAPI-NO-GIAC).                  
122800     MOVE PREZZO-MEM TO PREZZO-NO-GIAC (IND-CAPI-NO-GIAC).                
122900 EX-INSERISCI-NO-GIAC-PREZZO.                                             
123000     EXIT.                                                                
123100*                                                                         
123200*                                                                         
123300 TTDBGET-S.                                                               
123400     COPY PDBGET REPLACING AREA-REC-SET BY REC-SITPF                      
123500                  EX-TTDBGET BY EX-TTDBGET-S.                             
123600*                                                                         
123700*                                                                         
123800 TTDBGET.                                                                 
123900     COPY PDBGET.                                                         
124000*                                                                         
124100*                                                                         
124200 TRATTA-NO-GIAC.                                                          
           PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM.
124300     MOVE SPACE TO RISP-NO-GIAC.                                          
124400     DISPLAY "     " D-MAT-ELEM SPACE PREZZO-D.                           
124500     DISPLAY "CONFERMI MANCA GIAC ? (S/N)".                               
124600*     ACCEPT RISP-NO-GIAC.
           MOVE "N" TO RISP-NO-GIAC
           DISPLAY RISP-NO-GIAC
      *
124700     IF RISP-NO-GIAC NOT = "S" AND NOT = "N"                              
124800        GO TO EX-TRATTA-NO-GIAC.                                          
124900     MOVE 1 TO OK-GIAC.                                                   
125000*NOPRZ*                                                                   
125100*    IF RISP-NO-GIAC NOT = "S"                                            
125200*       NEXT SENTENCE                                                     
125300*      ELSE                                                               
125400*         DISPLAY "INSERITO Manca GIAC."
125500*         ADD 1 TO IND-CAPI-NO-GIAC                                       
125600*         MOVE C-MAT-A-BARRE-RID TO                                       
125700*                  C-MAT-NO-GIAC (IND-CAPI-NO-GIAC)                       
125800*         MOVE D-MAT-MEM TO D-MAT-NO-GIAC (IND-CAPI-NO-GIAC)              
125900*         MOVE PREZZO-MEM TO PREZZO-NO-GIAC (IND-CAPI-NO-GIAC).           
126000 EX-TRATTA-NO-GIAC.                                                       
126100     EXIT.                                                                
126200*                                                                         
126300*NOPRZ*                                                                   
126400 TRATTA-NO-PREZZO.                                                        
      *     PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM.
126500     MOVE SPACE TO RISP-NO-PREZZO.                                        
126600*NODMAT*                                                                  
126700**    DISPLAY "     " D-MAT-ELEM.                                          
126800*     DISPLAY "     " D-MAT-MEM.                                           
126900*     DISPLAY "CONFERMI MANCA PREZZO ? (S/N)".                             
127000*     ACCEPT RISP-NO-PREZZO.
           MOVE "S" TO RISP-NO-PREZZO.
      *     DISPLAY RISP-NO-PREZZO
      *
127100     IF RISP-NO-PREZZO NOT = "S" AND NOT = "N"                            
127200        GO TO EX-TRATTA-NO-PREZZO.                                        
127300     MOVE 1 TO OK-PREZZO.                                                 
127400 EX-TRATTA-NO-PREZZO.                                                     
127500     EXIT.                                                                
127600*                                                                         
127700*                                                                         
127800 TTDBFIND.                                                                
127900     COPY PDBFIND.                                                        
128000*                                                                         
128100 INSERT-ELEM-SING.                                                        
128200     MOVE "K1" TO QT-FUNZIONE OF PARTAB-SING.                             
      *UNICODDT*                                                        inizio
128300     MOVE IND-CAPI-LETTI TO CONT-SING.    
128300*     MOVE TAB-CAPI-LETTI TO CONT-SING.                               ?? VALERIA??
      *UNICODDT*                                                        FINE
128400     MOVE C-MAT-A-BARRE-RID TO C-MAT-SING.                                
128500     MOVE D-MAT-MEM TO D-MAT-SING.                                        
128600     MOVE PTG-MEM TO PRIMA-TG-SING.                                       
128700     MOVE PREZZO-MEM TO PREZZO-SING.  
      *MOVSKU
           MOVE SPACES TO SKU-SING.                                     
           CANCEL "QTABEL"
128800     CALL "QTABEL" USING PARTAB-SING                                      
128900                           TABELLA-SINGOLI                                
129000                              ELEMENTO-SINGOLI.                           
129100*     IF QT-STATO OF PARTAB-SING NOT = 0                                   
129200*        MOVE QT-STATO OF PARTAB-SING TO ERR-DISP                          
129300*        DISPLAY "ERR QTABEL SING  " ERR-DISP                              
129400*        DISPLAY "INSERIMENTO "                                            
129500*        DISPLAY "PER ELEMENTO     " C-MAT-A-BARRE-RID                     
      *     CANCEL "QDBERROR"
129600*        CALL "QDBERROR" USING W-COMMON.                                   
129700 EX-INSERT-ELEM-SING.                                                     
129800     EXIT.                                                                
129900*                                                                         
130000*                                                                         
130100* ANNULLA-PRECEDENTE.                                                      
130200*     IF MEM-COD-IN = SPACES                                               
130300*       DISPLAY "nulla da annullare"                                       
130400*       GO TO EX-ANNULLA-PRECEDENTE.                                       
130500*     MOVE MEM-COD-IN TO COD-IN.                                           
130600*     COMPUTE ELEM-ART = C-MAT-A-BARRE-RID / 10                            
130700*     MOVE "K2" TO QT-FUNZIONE OF PARTAB-ART.                              
      *     CANCEL "QTABEL"
130800*     CALL "QTABEL" USING PARTAB-ART TABELLA-ARTICOLI-LETTI                
130900*                         ELEM-ART.                                        
131000*     IF QT-STATO OF PARTAB-ART NOT = 0                                    
131100*       MOVE QT-STATO OF PARTAB-ART TO ERR-DISP                            
131200*         DISPLAY "ERR RILETTURA QTABEL " ERR-DISP                         
131300*               " - ANNULLA-PRECEDENTE"                                    
      *     CANCEL "QDBERROR"
131400*         CALL "QDBERROR" USING W-COMMON.                                  
131500*     SUBTRACT PREZZO-TAB(QT-INDEX-ELEM OF PARTAB-ART)                     
131600*              FROM PREZZO-TOT.                                            
131700*     PERFORM DELETE-ELEM-SING THRU EX-DELETE-ELEM-SING.                   
131800*     SUBTRACT 1 FROM QTA-TAGLIA-TAB(QT-INDEX-ELEM OF PARTAB-ART,          
131900*                 TAGLIA OF C-MAT-A-BARRE) IND-CAPI-LETTI.                 
132000*     MOVE QTA-TAGLIA-TAB(QT-INDEX-ELEM OF PARTAB-ART,                     
132100*                 TAGLIA OF C-MAT-A-BARRE) TO DISP-4.                      
132200*     DISPLAY "annullata 1 lettura "                                       
132300*     DISPLAY "ancora " DISP-4                                             
132400*                  C-MAT-A-BARRE-RID.                                      
132500**EURO1*                                                                   
132600*     IF W-FORMATO-INTERNO NOT > 011231                                    
132700*       MOVE PREZZO-TOT TO IE-IMPORTO-IN                                   
132800*       PERFORM PRZ-INLIT THRU EX-PRZ-INLIT                                
132900*       COMPUTE PREZZO-TOT-D = IE-IMPORTO-OU / 100                         
133000*       DISPLAY "  Tot. L." PREZZO-TOT-D                                   
133100*     ELSE                                                                 
133200*       COMPUTE PREZZO-TOT-D = PREZZO-TOT / 100                            
133300*       DISPLAY " Tot. Eur." PREZZO-TOT-D.                                 
133400**                                                                         
133500* EX-ANNULLA-PRECEDENTE.                                                   
133600*     EXIT.                                                                
133700*                                                                         
133800*EURO1*                                                                   
133900 PRZ-INLIT.                                                               
134000     COPY PDAEU.                                                          
134100 EX-PRZ-INLIT.                                                            
134200     EXIT.                                                                
134300*                                                                         
134400 DELETE-ELEM-SING.                                                        
134500     MOVE "K3" TO QT-FUNZIONE OF PARTAB-SING.                             
134600     MOVE IND-CAPI-LETTI TO CONT-SING.                                    
134700     MOVE C-MAT-A-BARRE-RID TO C-MAT-SING.                                
           CANCEL "QTABEL"
134800     CALL "QTABEL" USING PARTAB-SING                                      
134900                           TABELLA-SINGOLI                                
135000                              ELEMENTO-SINGOLI.                           
135100     IF QT-STATO OF PARTAB-SING NOT = 0                                   
135200        MOVE QT-STATO OF PARTAB-SING TO ERR-DISP                          
135300        DISPLAY "ERR QTABEL SING  " ERR-DISP                              
135400        DISPLAY "ANNULLO PREC"                                            
135500        DISPLAY "PER ELEMENTO     " C-MAT-A-BARRE-RID                     
           CANCEL "QDBERROR"
135600        CALL "QDBERROR" USING W-COMMON.                                   
135700 EX-DELETE-ELEM-SING.                                                     
135800     EXIT.                                                                
135900*                                                                         
136000*                                                                         
136100*PAGE                                                                     
136200*                                                                         
136300 TRATTA-LETTI.                                                            
136400*    DISPLAY "hJ".                                                      
136500*       PERFORM VARYING IR FROM 1 BY 1                                    
136600*            UNTIL IR > SALTO                                             
136700        DISPLAY SPACE                                                     
136800*       END-PERFORM     

      *                                                                 VALERIA  SOMMARE TAB-CAPI-LETTI DI TAB-UNIOCO-DDT???
                                                  
136900     MOVE IND-CAPI-LETTI TO TOT-CAPI-LETTI-1.                             
137000     DISPLAY "- Tot CAPI - " TOT-CAPI-LETTI-1.                            
137100     MOVE IND-CAPI-NO-GIAC TO TOT-CAPI-NO-GIAC.                           
137200*NOPRZ*                                                                   
137300     DISPLAY "- No GIAC./PREZZO - " TOT-CAPI-NO-GIAC.                     
137400*                                                                         
137500     DISPLAY " ".                                                         
137600     DISPLAY "Vuoi STORNARE "                                     
137700         "(SI-NO)".                                               
137800     MOVE SPACES TO CONFERMA-STORNO.                                      
137900     ACCEPT CONFERMA-STORNO.                                              
138000     IF (CONFERMA-STORNO = "SI" OR = "si") AND                            
138100               TOT-CAPI-LETTI-1 NOT = 0                                   
138200*       DISPLAY "hJ"                                                    
138300*       PERFORM VARYING IR FROM 1 BY 1                                    
138400*            UNTIL IR > SALTO                                             
138500        DISPLAY SPACE                                                     
138600*       END-PERFORM                                                       
138700        DISPLAY    "Dammi il CODICE"                              
138800        MOVE SPACES TO COD-IN MEM-COD-IN                                  
138900        DISPLAY " . fine lettura"                                         
139000        DISPLAY " @ storno totale"                                        
139100*       DISPLAY "l"                                                      
139200*        MOVE 3 TO QT-ADDR-KEY OF PARTAB-SING
              MOVE 1 TO QT-ADDR-KEY OF PARTAB-SING
      *
139300        MOVE 8 TO QT-LL-KEY OF PARTAB-SING                                
           CANCEL "QSORTAB"
139400        CALL "QSORTAB" USING PARTAB-SING TABELLA-SINGOLI                  
139500        PERFORM TRATTA-STORNO THRU EX-TRATTA-STORNO                       
139600               UNTIL LETT-FINE.                                           
139700*       DISPLAY "m".                                                     
139800     IF IND-CAPI-LETTI NOT < 1                                            
139900        MOVE 5 TO W-MODO                                                  
140000        PERFORM TTLOCK-T THRU EX-TTLOCK-T 

      *volante*
      *       if CONTO-IN-R = 10010261
      *         perform leggi-paramdt-fittizi
      *                 thru ex-leggi-paramdt-fittizi
      *         perform agg-dparam-fittizi
      *                 thru ex-agg-dparam-fittizi
      *         move 61222 to AA-MM-GG-DDT 
      *       else
140100          PERFORM LEGGI-PARAMDT THRU EX-LEGGI-PARAMDT                       
140200          PERFORM AGG-DPARAM THRU EX-AGG-DPARAM
                MOVE W-FORMATO-INTERNO TO AA-MM-GG-DDT
      *       end-if
      *
140300*       DISPLAY "hJ"                                                    
140400*       DISPLAY "BBBBBBB"                                          
140500        DISPLAY "Aggiorno  "                                      
140600        DISPLAY "BOLLA n.  " NUMERO-DDT   
      *
              IF QT-NUM-ELEM-EFF OF PARTAB-SING > 0 
      * NO-DATGE                                                        inizio
      *          PERFORM S-SET-1 THRU S-SET-1-EX
      * NO-DATGE                                                        fine
                PERFORM INSERISCI-MOVSKU THRU INSERISCI-MOVSKU-EX 
                         VARYING IND-BARUNI FROM 1 BY 1 
                               UNTIL IND-BARUNI > 
                                       QT-NUM-ELEM-EFF OF PARTAB-SING    
                PERFORM S-S-COMMIT THRU S-S-COMMIT-EX
      * NO-DATGE                                                        inizio
      *          PERFORM S-SET-2 THRU S-SET-2-EX                                                
      * NO-DATGE                                                        fine

              END-IF                              
140700*BUDA*                                                                    
140800        IF PRIORITA OF REC-INDIRIZZI = 4                                  
      *conv*
                MOVE SPACE               TO DIR-VAR-VALUE
                MOVE "RETIS_DIRECTORY"   TO DIR-VAR-NAME
                DISPLAY DIR-VAR-NAME UPON ENVIRONMENT-NAME
                ACCEPT DIR-VAR-VALUE FROM ENVIRONMENT-VALUE
      *
                MOVE SPACE               TO USER-VAR-VALUE
                MOVE "RETIS_UTENTE"      TO USER-VAR-NAME
                DISPLAY USER-VAR-NAME UPON ENVIRONMENT-NAME
                ACCEPT USER-VAR-VALUE FROM ENVIRONMENT-VALUE
      *
                MOVE "dd_BARCNEG"        TO FILE-VAR-NAME
                MOVE SPACES              TO FILE-VAR-VALUE
      *movsku
      *          STRING DIR-VAR-VALUE DELIMITED BY SPACE
                STRING 
                     USER-VAR-VALUE DELIMITED BY SPACE
      *              "_BARCNEG" DELIMITED BY SIZE
                     "_BC" DELIMITED BY SIZE
                     "_" AA-MM-GG-DDT NUMERO-DDT
                     "_" MAG-INPUT-R "_" NEG-IN 
                     "_B"
                                DELIMITED BY SIZE
      *movsku*                                                          fine                       
                        INTO FILE-VAR-VALUE
                DISPLAY FILE-VAR-NAME  UPON ENVIRONMENT-NAME
                DISPLAY FILE-VAR-VALUE UPON ENVIRONMENT-VALUE
      *conv-end*
140900          OPEN OUTPUT FILE-BC                                             
141000        END-IF                                                            
141100        PERFORM SCRIVI-RECORD THRU EX-SCRIVI-RECORD                       
141200            VARYING W-INDICE-3 FROM 1 BY 1                                
141300               UNTIL W-INDICE-3 > QT-NUM-ELEM-EFF OF PARTAB-ART           
141400*BUDA*                                                                    
141500        MOVE 1 TO QT-ADDR-KEY OF PARTAB-SING                              
141600        MOVE 10 TO QT-LL-KEY OF PARTAB-SING                               
           CANCEL "QSORTAB"
141700        CALL "QSORTAB" USING PARTAB-SING                                  
141800                                TABELLA-SINGOLI                           
141900        IF PRIORITA OF REC-INDIRIZZI = 4   
      *MOVSKU
                 PERFORM INTESTA-FILE-BC THRU EX-INTESTA-FILE-BC      
      *MOVSKU                               
142000           PERFORM SCORRI-TAB-SING THRU EX-SCORRI-TAB-SING                
142100               VARYING W-INDICE-3 FROM 1 BY 1                             
142200                 UNTIL W-INDICE-3 > QT-NUM-ELEM-EFF                       
142300                      OF PARTAB-SING                                      
142400           CLOSE FILE-BC                                                  
142500          ELSE                                                            
142600             PERFORM CALL-COMMAND THRU EX-CALL-COMMAND                    
142800             PERFORM SCORRI-TB THRU EX-SCORRI-TB
143100             PERFORM CALL-COMMAND-2 THRU EX-CALL-COMMAND-2                
143200        END-IF                                                            
143300        PERFORM SCRIVI-BOLLE THRU EX-SCRIVI-BOLLE                         
143400        PERFORM CHIAMA-PRINTDDT THRU EX-CHIAMA-PRINTDDT                   
143500*BUDA*                                                                    
143600*       IF FLAG-ANACON NOT = '0' AND NOT = ' '                            
143700*         PERFORM CHIAMA-WRITERES THRU EX-CHIAMA-WRITERES                 
143800*       END-IF                                                            
143900        IF PRIORITA OF REC-INDIRIZZI = 4                                  
144000*EURO1*                                                                   
144100           MOVE DIVISA OF REC-CONFATT TO                                  
144200                          DIVISA-PRIMO-LETTO                              
      *          CANCEL "PRTBCEU6"
      *          CALL "PRTBCEU6" USING W-COMMON
      *MOVSKU
      *           CANCEL "PRTBCEU7"
      *           CALL "PRTBCEU7" USING W-COMMON
                 CANCEL "PRTBCEU8"
                 CALL "PRTBCEU8" USING W-COMMON 
                                       SQLCA
                                       REC-CONFATT
                                       AA-MM-GG-DDT
                                       NUMERO-DDT
                                       DIVISA-PRIMO-LETTO
                                       DESTINO-USCITA       
144900        END-IF                                                            
              IF IND-CAPI-NO-GIAC > 0
145000           PERFORM STAMPA-NO-GIAC THRU EX-STAMPA-NO-GIAC
              END-IF
145100        PERFORM TTUNLOCK THRU EX-TTUNLOCK                                 
              IF FLAG-ANACON NOT = '0' AND NOT = ' '
                 PERFORM ALLINEA-BOLLA-ESTERO
                    THRU EX-ALLINEA-BOLLA-ESTERO
              END-IF
              PERFORM AVANZA-DDT THRU EX-AVANZA-DDT
145200*TASTO-PER-CONTINUARE*                                                    
145300        DISPLAY "premi un tasto per continuare..."                        
145400        ACCEPT TASTO-INP                                                  
145500*                                                                         
145600     END-IF.                                                              
145700 EX-TRATTA-LETTI.                                                         
145800     EXIT.                                                                
145900*                                                                         
      *
       ALLINEA-BOLLA-ESTERO.
           MOVE RIF-BOLLA-DDT TO INPUT-RIF-TRASF.
           IF CONTO-FATTURA-MEM NOT = 0
             MOVE CONTO-FATTURA-MEM TO INPUT-CONTO-TRASF
           ELSE
             MOVE CONTO-IN-R TO INPUT-CONTO-TRASF
           END-IF.
           CALL  "PYTHON"
           USING "allinea_bolle_in_estero"
                 "allinea_estero"
                 PY-INPUT-TRASF
                 PY-OUTPUT-TRASF.
           IF OUTPUT-ERR-TRASF NOT = SPACES
              DISPLAY 'ERRORE!'
              DISPLAY OUTPUT-ERR-TRASF
           END-IF.
       EX-ALLINEA-BOLLA-ESTERO.
           EXIT.
           
       AVANZA-DDT.
      *  
      *questo rif_intr ha anno a due cifre
         MOVE RIF-BOLLA-DDT TO INPUT-RIF-INTR.
         IF CONTO-FATTURA-MEM NOT = 0
           MOVE CONTO-FATTURA-MEM TO INPUT-CONTO
         ELSE
           MOVE CONTO-IN-R TO INPUT-CONTO
         END-IF.       
         MOVE MAG-INPUT-R TO INPUT-MAG.
      *il flag E indica ddt stock o estero alla libreria dell'avanzamento   
         MOVE 'E' TO INPUT-FLAG.                  
         CALL "PYTHON" USING "avanzamento" 
                             "genera_avanzamento" 
                               PY-INPUT-REC-B
                               PY-OUTPUT-REC-B.
         IF OUTPUT-VAL = 'KO'
             DISPLAY 'AVANZAMENTO NON RIUSCITO'.
       EX-AVANZA-DDT.
           EXIT. 
      *
146000*                                                                         
146100 CALL-COMMAND.                                                            
146200     MOVE NUMERO-DDT TO BUILD-N-DDT                                       
146300                        PURGE-N-DDT                                       
146400                        FILE-N-DDT                                        
146500                        FILE-N-DDT-2                                      
146600                        PRINT-N-DDT                                       
146700                        PRINT-N-DDT-2.                                    
           CANCEL "COMMAND2"
146800     CALL "COMMAND2" USING COMANDO-BUILD                         
146900                                     ERR, ERR-PARM.                       
147000     IF ERR NOT = 0                                                       
147100        MOVE ERR TO ERR-DISP                                              
147200*ERR-279*                                                                 
147300        DISPLAY "Errore COMMAND BUILD - P" BUILD-N-DDT                    
147400                SPACE "-  " ERR-DISP.                                     
147500*       STOP RUN.                                                         
147600*                                                                         
           CANCEL "COMMAND2"
147700     CALL "COMMAND2" USING COMANDO-FILE                          
147800                                     ERR, ERR-PARM.                       
147900     IF ERR NOT = 0                                                       
148000        MOVE ERR TO ERR-DISP                                              
148100        DISPLAY "Errore COMMAND FILE ECQ - P" BUILD-N-DDT                 
148200                SPACE "-  " ERR-DISP
              MOVE 1 TO JRUNC
148300*        STOP RUN JRUNC.    
                                            
148400     OPEN OUTPUT FILE-PEND.                                               
148500 EX-CALL-COMMAND.                                                         
148600     EXIT.                                                                
148700*                                                                         
148800*                                                                         
148900 STAMPA-NO-GIAC.                                                          
149000     MOVE 100 TO CONTA-RIGHE.                                             
149100     MOVE 1 TO CONTA-PAGINE.                                              
149200     PERFORM APRI-STAMPA THRU EX-APRI-STAMPA.                             
149300     PERFORM STAMPA-DETT-N-G THRU EX-STAMPA-DETT-N-G                      
149400                VARYING W-INDICE-3 FROM 1 BY 1                            
149500                    UNTIL W-INDICE-3 >                                    
149600                        IND-CAPI-NO-GIAC.                                 
149700     PERFORM CHIUDI-STAMPA THRU EX-CHIUDI-STAMPA.                         
149800 EX-STAMPA-NO-GIAC.                                                       
149900     EXIT.                                                                
150000*                                                                         
150100*                                                                         
150200 APRI-STAMPA.                                                             
150300     MOVE  136  TO LL-RIGA.                                               
150400     MOVE 9999 TO N-MAX-RIGHE.                                            
150500     MOVE "N" TO PRE-NOME-FILE.                                           
150600     MOVE NUMERO-DDT TO TERM-N-FILE.                                      
150700     CALL "QOLPPR" USING PAR-PRINT                                        
150800                          RIGA BUFFER.                                    
150900     MOVE 0   TO N-STAMPANTE.                                             
151000     MOVE "M" TO COMANDO.                                                 
151100     MOVE 66  TO N-RIGA-STAMPA.                                           
151200*NOPRZ*                                                                   
151300*    MOVE " MANCA GIACENZA SU VENDITA " TO DATI-RIGA.                     
151400     MOVE " MANCA GIAC./PREZZO SU VENDITA " TO DATI-RIGA.                 
151500     CALL "QWLPPR" USING PAR-PRINT                                        
151600                          RIGA BUFFER.                                    
151700     MOVE SPACES TO DATI-RIGA.                                            
151800 EX-APRI-STAMPA.                                                          
151900     EXIT.                                                                
152000*                                                                         
152100*                                                                         
152200 SCRIVI.                                                                  
152300     CALL "QWLPPR" USING PAR-PRINT                                        
152400                         RIGA BUFFER.                                     
152500     IF STATO OF PAR-PRINT NOT = 0                                        
152600        MOVE STATO OF PAR-PRINT TO STATO-DISPLAY                          
152700        DISPLAY "ERRORE QPRINT CON STATO : " STATO-DISPLAY                
           CANCEL "QDBERROR"
152800        CALL "QDBERROR" USING W-COMMON.                                   
152900     MOVE SPACES TO DATI-RIGA.                                            
153000     MOVE "S" TO COMANDO.                                                 
153100     MOVE 0 TO N-RIGA-STAMPA.                                             
153200 EX-SCRIVI.                                                               
153300     EXIT.                                                                
153400*                                                                         
153500*                                                                         
153600 INTESTA-PAGINA.                                                          
153700     MOVE SPACE TO DATI-RIGA.                                             
153800     MOVE W-FORMATO-GG-MMM-AAAA TO DATA-T.                                
153900     MOVE W-NUM-TERM TO NUM-PAG-T.                                        
154000     MOVE "term " TO D-PAG-T.                                             
154100     MOVE "P" TO COMANDO.                                                 
154200     MOVE 2 TO N-RIGA-STAMPA.                                             
154300     MOVE 0 TO N-STAMPANTE.                                               
154400     PERFORM SCRIVI THRU                                                  
154500              EX-SCRIVI.                                                  
154600*                                                                         
154700     MOVE CONTO-IN-R TO CONTO-T.                                          
154800     MOVE D-CONTO-MEM TO D-CONTO-T.                                       
154900     MOVE "pag. " TO D-PAG-T.                                             
155000     MOVE CONTA-PAGINE TO NUM-PAG-T.                                      
155100     MOVE "P" TO COMANDO.                                                 
155200     MOVE 3 TO N-RIGA-STAMPA.                                             
155300     MOVE 0 TO N-STAMPANTE.                                               
155400     PERFORM SCRIVI THRU                                                  
155500              EX-SCRIVI.                                                  
155600*                                                                         
155700*NOPRZ*                                                                   
155800     MOVE "     ELENCO MODELLI SENZA GIAC./PREZZO "                       
155900                    TO DATI-RIGA.                                         
156000     MOVE 2 TO N-RIGA-STAMPA.                                             
156100     PERFORM SCRIVI THRU                                                  
156200              EX-SCRIVI.                                                  
156300*                                                                         
156400     MOVE 1 TO N-RIGA-STAMPA.                                             
156500     MOVE 5 TO CONTA-RIGHE.                                               
156600 EX-INTESTA-PAGINA.                                                       
156700     EXIT.                                                                
156800*                                                                         
156900*                                                                         
157000 STAMPA-DETT-N-G.                                                         
157100     IF CONTA-RIGHE  > 50                                                 
157200        PERFORM INTESTA-PAGINA THRU EX-INTESTA-PAGINA                     
157300        ADD 1 TO CONTA-PAGINE.                                            
157400     MOVE C-MAT-NO-GIAC (W-INDICE-3) TO C-MAT-A-BARRE-RID.                
157500     MOVE 0 TO C-MAT-TRANS-RID.                                           
157600     MOVE MODELLO OF C-MAT-A-BARRE TO MODELLO OF                          
157700                         C-MAT-TRANSITO.                                  
157800     MOVE VESTIBILITA OF C-MAT-A-BARRE TO                                 
157900              VEST-A OF C-MAT-TRANSITO.                                   
158000     MOVE SOCIETA OF C-MAT-A-BARRE TO                                     
158100               PROGR-ART OF C-MAT-TRANSITO.                               
158200     MOVE PEZZO OF C-MAT-A-BARRE TO PEZZO-A OF                            
158300                         C-MAT-TRANSITO.                                  
158400     MOVE VARIANTE-COL OF C-MAT-A-BARRE TO COLORE OF                      
158500                         C-MAT-TRANSITO.                                  
158600     MOVE C-MAT-TRANS-RID TO C-MAT-ST.                                    
158700     MOVE D-MAT-NO-GIAC (W-INDICE-3) TO NOME-MOD-ST.
           MOVE TAGLIA OF C-MAT-A-BARRE TO NTG-IN                       
158800     MOVE FUNCTION idxtg(NTG-IN) TO TAGLIA-ST.                           
158900     COMPUTE PREZZO-ST-EU = PREZZO-NO-GIAC (W-INDICE-3) / 100.            
159000*PRZBU*                                                                   
159100     MOVE CAUSALE-NO-GIAC (W-INDICE-3) TO                                 
159200                         NOGIAC-ST.                                       
159300     MOVE CAUSALE-NO-PRZ (W-INDICE-3) TO                                  
159400                         NOPRZ-ST.                                        
159500*                                                                         
159600     PERFORM SCRIVI THRU EX-SCRIVI.                                       
159700 EX-STAMPA-DETT-N-G.                                                      
159800     EXIT.                                                                
159900*                                                                         
160000*                                                                         
160100*                                                                         
160200 CHIUDI-STAMPA.                                                           
160300     CALL "QCLPPR" USING PAR-PRINT                                        
160400                          RIGA BUFFER.                                    
160500 EX-CHIUDI-STAMPA.                                                        
160600     EXIT.                                                                
160700*                                                                         
160800*                                                                         
160900 CALL-COMMAND-2.                                                          
161000     CLOSE FILE-PEND.                                                     
      *conv
           MOVE "dd_PEND" TO WK-VAR-NAME.
           MOVE SPACES    TO WK-VAR-VALUE.
           DISPLAY WK-VAR-NAME UPON ENVIRONMENT-NAME.
           ACCEPT WK-VAR-VALUE FROM ENVIRONMENT-VALUE.
           MOVE WK-VAR-VALUE TO LPR-NOME-FILE.
      *
           MOVE DESTINO-USCITA TO LPR-NUM-STAMPANTE.
      *non stampa i pendenti, verranno stampati manualmente
      *se necessario
      *    CALL "SYSTEM" USING COMANDO-LPR-LINUX
      *            GIVING INTO ERR.
      *    IF ERR NOT = 0
      *       MOVE ERR TO ERR-DISP
      *       DISPLAY "**READVE3 - Errore COMANDO: "
      *               COMANDO-LPR-LINUX
      *               " - errno: " ERR-DISP
      *    END-IF.
      *
161100*    CALL INTRINSIC "COMMAND" USING COMANDO-FILE-2                        
161200*                                    ERR, ERR-PARM.                       
161300*    IF C-C NOT = 0                                                       
161400*       MOVE ERR TO ERR-DISP                                              
161500*       DISPLAY "Errore COMMAND FILE ECQ-2 - P" BUILD-N-DDT               
161600*               SPACE "-  " ERR-DISP                                      
161700*       STOP RUN.                                                         
161800*    CALL INTRINSIC "COMMAND" USING COMANDO-PRINT                         
161900*                                    ERR, ERR-PARM.                       
162000*    IF C-C NOT = 0                                                       
162100*       MOVE ERR TO ERR-DISP                                              
162200*       DISPLAY "Errore COMMAND PRINT ECQ - P" BUILD-N-DDT                
162300*               SPACE "-  " ERR-DISP                                      
162400*       STOP RUN.                                                         
      *conv-end
162500*    CALL INTRINSIC "COMMAND" USING COMANDO-PURGE                         
162600*                                    ERR, ERR-PARM.                       
162700*    IF C-C NOT = 0                                                       
162800*       MOVE ERR TO ERR-DISP                                              
162900*       DISPLAY "Errore COMMAND PURGE - P" BUILD-N-DDT                    
163000*               SPACE "-  " ERR-DISP                                      
163100*       STOP RUN.                                                         
163200 EX-CALL-COMMAND-2.                                                       
163300     EXIT.                                                                
163400*                                                                         
163500*                                                                         
163600*BUDA*                                                                    
163700 CHIAMA-WRITERES.                                                         
163800     MOVE 0 TO CAPO-CONTO.                                                
163900     MOVE NEG-IN TO SOTTO-CONTO.                                          
164000*                                                                         
164100     MOVE RIF-BOLLA-DDT TO RIF-INTR-WR.                                   
164200*MAG6/7*                                                                  
164300*    MOVE 7 TO MAGAZZINO-WR.                                              
164400     MOVE MAG-INPUT-R TO MAGAZZINO-WR.                                    
164500     MOVE FLAG-ANACON TO DEST-WR.                                         
164600*FIFRA*                                                                   
164700*    MOVE CONTO-IN-R TO CONTO-CLI-WR.                                     
164800     IF CONTO-FATTURA-MEM NOT = 0                                         
164900       MOVE CONTO-FATTURA-MEM TO CONTO-CLI-WR                             
165000     ELSE                                                                 
165100       MOVE CONTO-IN-R TO CONTO-CLI-WR.                                   
165200*                                                                         
165300     MOVE CODICE-CONTO TO CONTO-DEST-WR.                                  
165400     MOVE DIVISA-MEM TO DIVISA-WR.                                        
165500     MOVE LISTINO-MEM TO LISTINO-WR.                                      
165600     MOVE "VESD" TO CAUSALE-WR.                                           
165700     MOVE 0 TO DATA-CARICO-WR.                                            
           CANCEL "WRITERES"
165800     CALL "WRITERES" USING W-COMMON                                       
165900                           SQLCA                                          
166000                           CAMPI-X-WRITE DATA-CARICO-WR.                  
166100 EX-CHIAMA-WRITERES.                                                      
166200     EXIT.                                                                
166300*                                                                         
166400*                                                                         
166500 SCRIVI-BOLLE.                                                            
166600     MOVE "BOLLE;" TO W-NOME-DATA-SET.                                    
166700*FIFRA*                                                                   
166800*    MOVE CONTO-IN-R TO CONTO OF REC-BOLLE.                               
166900     IF CONTO-FATTURA-MEM NOT = 0                                         
167000       MOVE CONTO-FATTURA-MEM TO CONTO OF REC-BOLLE                       
167100     ELSE                                                                 
167200       MOVE CONTO-IN-R TO CONTO OF REC-BOLLE.                             
167300*                                                                         
167400     MOVE RIF-BOLLA-DDT TO RIF-INTERNO OF REC-BOLLE.                      
167500*BOLL*                                                                    
167600*MAG6/7*                                                                  
167700*    MOVE 7 TO MAGAZZINO OF REC-BOLLE                                     
167800     MOVE MAG-INPUT-R TO MAGAZZINO OF REC-BOLLE                           
167900     MOVE 0 TO DATA-NASCITA OF REC-BOLLE                                  
168000               NUM-PRE-FATT OF REC-BOLLE.                                 
168100     MOVE 1 TO NUMERO       OF REC-BOLLE.                                 
168200     MOVE SPACES TO VAL-REC OF REC-BOLLE.                                 
168300     PERFORM TTDBPUT THRU EX-TTDBPUT.                                     
168400     IF NOT W-OK-IMAGE                                                    
168500        MOVE W-STATUS-WORD-IMAGE TO ERR-DISP                              
168600        DISPLAY "Err. PUT BOLLE  " ERR-DISP                               
168700        DISPLAY "Per CONTO  " CONTO-IN-R                                  
168800           "   RIF-INTERNO  " RIF-BOLLA-DDT                               
           CANCEL "QDBERROR"
168900        CALL "QDBERROR" USING W-COMMON.                                   
169000 EX-SCRIVI-BOLLE.                                                         
169100     EXIT.                                                                
169200*                                                                         
169300*                                                                         
169400 TRATTA-STORNO.                                                           
169500     MOVE SPACES TO COD-IN.                                               
169600     ACCEPT COD-IN.                                                       
169700     IF LETT-FINE                                                         
169800        GO TO EX-TRATTA-STORNO.                                           
169900     IF LETT-ANN-TUTTO                                                    
170000        MOVE 0 TO IND-CAPI-LETTI                                          
170100        MOVE "." TO COD-IN-RID                                            
170200        GO TO EX-TRATTA-STORNO.                                           
170300     IF C-MAT-A-BARRE-RID NOT NUMERIC                                     
170400        DISPLAY "COD non num >> RILEGGERE"                                
              PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
170500        GO TO EX-TRATTA-STORNO.              
           MOVE TAGLIA OF C-MAT-A-BARRE TO NTG-IN 
           MOVE FUNCTION idxtg(NTG-IN) 
                    TO NTG-OUT                            
170600     MOVE SOCIETA OF C-MAT-A-BARRE TO SOC-COM.                            
170700     MOVE 0 TO PRE-SOC.                                                   
170800     MOVE SOC-COM TO SOCIETA OF C-MAT-A-BARRE.                            
170900     COMPUTE ELEM-ART = C-MAT-A-BARRE-RID / 10                            
171000     MOVE "K2" TO QT-FUNZIONE OF PARTAB-ART.                              
           CANCEL "QTABEL"
171100     CALL "QTABEL" USING PARTAB-ART TABELLA-ARTICOLI-LETTI                
171200                         ELEM-ART.                                        
171300     IF QT-STATO OF PARTAB-ART NOT = 0                                    
             PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
171400       DISPLAY "Manca lettura "                                           
171500       GO TO EX-TRATTA-STORNO.                                            
171600     MOVE ART-TAB-LETTI(QT-INDEX-ELEM OF PARTAB-ART)                      
171700           TO ART-ELEM-LETTI.                                             
171800     SUBTRACT 1 FROM QTA-TAGLIA-ELEM(NTG-OUT).                                         
172000     IF QTA-TAGLIA-ELEM(NTG-OUT) < 0                      
                 PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
172100           DISPLAY "Taglia non stornabile"                                
172200           GO TO EX-TRATTA-STORNO.                                        
172300     PERFORM DELETE-ELEM-SING THRU EX-DELETE-ELEM-SING.                   
172400     SUBTRACT 1 FROM IND-CAPI-LETTI.                                      
172500     SUBTRACT PREZZO-ELEM FROM PREZZO-TOT.                                
172600     MOVE  QTA-TAGLIA-ELEM(NTG-OUT)                       
172700       TO DISP-4.                                                         
172800     DISPLAY "ancora " DISP-4                                             
172900                  C-MAT-A-BARRE-RID.                                      
173000*EURO1*                                                                   
173100     IF W-FORMATO-INTERNO NOT > 011231                                    
173200       MOVE PREZZO-TOT TO IE-IMPORTO-IN                                   
173300       PERFORM PRZ-INLIT THRU EX-PRZ-INLIT                                
173400       COMPUTE PREZZO-TOT-D = IE-IMPORTO-OU / 100                         
173500*       DISPLAY "  Tot. L." PREZZO-TOT-D                                   
173600     ELSE                                                                 
173700       COMPUTE PREZZO-TOT-D = PREZZO-TOT / 100                            
173800*       DISPLAY " Tot. Eur." PREZZO-TOT-D
           END-IF
173900*                                                                         
174000     MOVE ART-ELEM-LETTI                                                  
174100        TO ART-TAB-LETTI(QT-INDEX-ELEM OF PARTAB-ART).                    
174200 EX-TRATTA-STORNO.                                                        
174300     EXIT.                                                                
174400*                                                                         
174500*                                                                         
174600 SCORRI-TB.
           PERFORM COMANDI-IGP-TESTA THRU EX-COMANDI-IGP-TESTA.
      *
           MOVE 0 TO VERT-EXP-IGP
                     ORIZ-EXP-IGP.
      *
           MOVE 80 TO ROW-IGP.
           MOVE 100 TO COL-IGP.
           PERFORM COMANDI-IGP-TESTA-1 THRU EX-COMANDI-IGP-TESTA-1.
           MOVE SPACES TO RIGA-PEND-COM.
           MOVE QT-NUM-ELEM-EFF OF PARTAB-SING
             TO DISP-4.
           STRING "INIZIO "   DELIMITED BY SIZE
                  NUMERO-DDT  DELIMITED BY SIZE
                  " - TOT "   DELIMITED BY SIZE
                  DISP-4      DELIMITED BY SIZE
             INTO RIGA-PEND-COM.
           PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
           PERFORM COMANDI-IGP-FINE-1 THRU EX-COMANDI-IGP-FINE-1.
      ***
           MOVE 0 TO IND-PEND.
           PERFORM VARYING W-INDICE-3 FROM 1 BY 1
           UNTIL W-INDICE-3 > QT-NUM-ELEM-EFF OF PARTAB-SING
              PERFORM SCORRI-TB-SING
                 THRU EX-SCORRI-TB-SING
           END-PERFORM.
      *
      *etich-vuota*
           PERFORM SCRIVI-ETICH-VUOTA
              THRU EX-SCRIVI-ETICH-VUOTA.
177700 EX-SCORRI-TB.
177800     EXIT.                                                                
177900*                                                                         
      *
       SCORRI-TB-SING.
           IF IND-PEND = 0
              PERFORM COMANDI-IGP-TESTA-1
                 THRU EX-COMANDI-IGP-TESTA-1
           END-IF.
      *
           MOVE SPACES TO RIGA-PENDENTI.
           PERFORM METTI-VALORI-PEND THRU EX-METTI-VALORI-PEND.
      *
           MOVE 80 TO ROW-IGP.
           COMPUTE COL-IGP = 80 + (IND-PEND * 1000).
           MOVE ANTE-PRIMA-RIGA TO RIGA-PEND-COM.
           PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
      *
           ADD 56 TO ROW-IGP.
           MOVE PRIMA-RIGA TO RIGA-PEND-COM.
           PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
      *
           ADD 56 TO ROW-IGP.
           MOVE SECONDA-RIGA TO RIGA-PEND-COM.
           PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
      *
           ADD 56 TO ROW-IGP.
           MOVE TERZA-RIGA TO RIGA-PEND-COM.
           PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
      *
           ADD 56 TO ROW-IGP.
           MOVE QUARTA-RIGA TO RIGA-PEND-COM.
           PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
      *
           IF IND-PEND = 1 OR
           W-INDICE-3 = QT-NUM-ELEM-EFF OF PARTAB-SING
              PERFORM COMANDI-IGP-FINE-1
                 THRU EX-COMANDI-IGP-FINE-1
           END-IF.
      *
           IF IND-PEND = 0
              MOVE 1 TO IND-PEND
           ELSE
              MOVE 0 TO IND-PEND
           END-IF.
       EX-SCORRI-TB-SING.
           EXIT.
      *
      *etich-vuota*
       SCRIVI-ETICH-VUOTA.
           MOVE 80 TO ROW-IGP.
           MOVE 100 TO COL-IGP.
           PERFORM COMANDI-IGP-TESTA-1 THRU EX-COMANDI-IGP-TESTA-1.
           MOVE SPACES TO RIGA-PEND-COM.
           PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
           PERFORM COMANDI-IGP-FINE-1 THRU EX-COMANDI-IGP-FINE-1.
       EX-SCRIVI-ETICH-VUOTA.
           EXIT.
      *
      *
178000*                                                                         
178100 METTI-VALORI-PEND.
178300     MOVE NUMERO-DDT TO NUMERO-P.
178400     MOVE ELEM-TAB-SING (W-INDICE-3) TO ELEMENTO-SINGOLI.                 
178500     MOVE C-MAT-SING TO CODICE-P.
178600     MOVE D-MAT-SING TO NOME-P.
178700*EURO*                                                                    
178800     COPY PINEU.                                                          
178900     MOVE IE-DIVISA-OU TO DIV-EUR.
179000*                                                                         
179100*PRZ-PUBBL*                                                               
179200*    COMPUTE PREZZO-P-E = PREZZO-SING / 100.
179300     MOVE 0 TO PREZZO-P-E.
      *annullato*   16/09/2011 - richiesta di Benassi
179400*    IF CONTO-DEMA                                                        
179500*      PERFORM CERCA-PREZZO-PUBBL THRU EX-CERCA-PREZZO-PUBBL              
179600*      COMPUTE PREZZO-P-E = PREZZO-PUBBL / 100
179700*    END-IF.                                                              
      *
179800*                                                                         
179900     MOVE PREZZO-SING TO IE-IMPORTO-IN.                                   
180000     COPY PDAEU.                                                          
180100     MOVE IE-DIVISA-OU TO DIV-LIT.
180200*    COMPUTE PREZZO-P = IE-IMPORTO-OU / 100.
180300     MOVE 0 TO PREZZO-P.
180600 EX-METTI-VALORI-PEND.
180700     EXIT.                                                                
180800*                                                                         
180900*                                                                         
181000*PRZ-PUBBL*                                                               
181100 CERCA-PREZZO-PUBBL.                                                      
181200     MOVE 0 TO PREZZO-PUBBL.                                              
181300*                                                                         
181400     MOVE C-MAT-SING TO C-MAT-A-BARRE-RID.                                
181500     MOVE 0 TO C-MAT-TRANS-RID OF C-MAT-COM.                              
181600     MOVE MODELLO OF C-MAT-A-BARRE                                        
181700       TO MODELLO OF C-MAT-TRANSITO.                                      
181800     MOVE VESTIBILITA OF C-MAT-A-BARRE                                    
181900       TO VEST-A OF C-MAT-TRANSITO.                                       
182000     MOVE SOCIETA OF C-MAT-A-BARRE                                        
182100       TO PROGR-ART OF C-MAT-TRANSITO.                                    
182200     MOVE PEZZO OF C-MAT-A-BARRE                                          
182300       TO PEZZO-A OF C-MAT-TRANSITO.                                      
182400     MOVE 0                                                               
182500       TO COLORE OF C-MAT-TRANSITO.                                       
182600*                                                                         
      * NO-DATGE                                                        inizio 
182700*     MOVE SOCIETA-MOD OF C-MAT-TRANSITO TO SOCIETA-CODICE-SOC.            
182800*     PERFORM SELEZIONA-PF-SOCIETA THRU EX-SELEZIONA-PF-SOCIETA.           
182900*     IF SQLCODE-MEM = OK                                                  
183000**trovata societa                                                          
183100*       MOVE C-MAT-TRANS-RID OF C-MAT-COM                                  
183200*         TO MODELLI-MODELLO-MAXIMA                                        
183300* *      MOVE SOCIETA-SOCIETA TO MODELLI-SOCIETA                            
183400*       PERFORM SELEZIONA-MODELLO-NEW                                      
183500*          THRU EX-SELEZIONA-MODELLO-NEW                                   
183600*       IF SQLCODE-MEM = OK                                                
183700**trovato modello new                                                      
183800*         MOVE MODELLI-MODELLO-NEW TO PREZZI-MODELLO-NEW                   
183900*         MOVE MODELLI-ANNO        TO PREZZI-ANNO                          
184000*         MOVE MODELLI-STAGIONE    TO PREZZI-STAGIONE                      
184100*         MOVE MODELLI-SOCIETA     TO PREZZI-SOCIETA                       
184200*         PERFORM SELEZIONA-PREZZO THRU EX-SELEZIONA-PREZZO                
184300*         IF SQLCODE-MEM =OK                                               
184400**trovato prezzo                                                           
184500*           MOVE PREZZI-PREZZO TO PREZZO-PUBBL                             
184600*         END-IF                                                           
184700*       END-IF                                                             
184800*     END-IF.                                                              
184900**                                                                         
      * NO-DATGE                                                        fine 

      * NO-DATGE                                                        inizio 
            PERFORM TRATTA-MODELLI-DBG THRU 
                 EX-TRATTA-MODELLI-DBG.
      * NO-DATGE                                                        FINE 
                                                          
185000     IF PREZZO-PUBBL = 0 AND                                              
185100     SOCIETA-MOD OF C-MAT-TRANSITO = 5                                    
185200       PERFORM CERCA-PREZZO-PUBBL-2                                       
185300          THRU EX-CERCA-PREZZO-PUBBL-2                                    
185400     END-IF.                                                              
185500 EX-CERCA-PREZZO-PUBBL.                                                   
185600     EXIT.                                                                
185700*                                                                         
        
      * NO-DATGE                                                        inizio 
       TRATTA-MODELLI-DBG.
            MOVE SOCIETA-SIGLA(SOCIETA-MOD OF C-MAT-TRANSITO)
              TO CC-SOCIETA.
              
            MOVE C-MAT-TRANS-RID OF C-MAT-COM TO CC-C-MAT.
            MOVE 1 TO CC-LISTINo.
            MOVE "V" TO CC-TIPO-PREZZO.

            PERFORM SELEZIONA-PREZZO-DBG THRU
                 EX-SELEZIONA-PREZZO-DBG.

            DISPLAY "SQLCODE " SQLCODE.
            PERFORM TEST-ERR THRU TEST-ERR-EX. 
      *      IF SQLCODE = OK        DISPLAY "SQLCODE = OK" END-IF
      *      IF SQLCODE = NO-MEMORY DISPLAY "SQLCODE = NO_MEMORY" END-IF
      *      IF SQLCODE = DEADLOCK  DISPLAY "SQLCODE = DEADLOCK" END-IF
      *      IF SQLCODE = NOT-FOUND DISPLAY "SQLCODE = NOT-FOUND" END-IF

            IF SQLCODE = OK
               MOVE CC-PREZZO-DBG TO PREZZO-PUBBL.
      *         DISPLAY "CC-PREZZO-DBG=" CC-PREZZO-DBG 
      *                 "  PREZZO-PUBBL=" PREZZO-PUBBL.  
       EX-TRATTA-MODELLI-DBG. EXIT.
      *
      *
       SELEZIONA-PREZZO-DBG.
           PERFORM WITH TEST AFTER                                              
           UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                           
             PERFORM BEGIN-RC THRU BEGIN-RC-EX                                  
             IF SQLCODE = OK                                                    
               EXEC SQL                                                         

               SELECT 
                   P.prezzo
                INTO :CC-PREZZO-DBG
                FROM prezzi_modelli_dbg P
                JOIN anagrafica_modelli_dbg M
                    ON  (M.SOCIETA = P.SOCIETA)
                    AND (P.MODELLO = M.MODELLO)
                JOIN anagrafica_modelli_barcode_negozio_dbg B
                    ON  (B.SOCIETA = P.SOCIETA) 
                    AND (P.MODELLO = B.MODELLO)
            
                WHERE
                    P.modello = :CC-C-MAT 
                    AND M.societa = :CC-SOCIETA                             *'MM'
                    AND P.f_listino_rif = :CC-LISTINO                        *1
                    AND P.tipo_prezzo = :CC-TIPO-PREZZO                     *"V"

               END-EXEC 
             END-IF                                                        
             MOVE SQLCODE TO SQLCODE-MEM                                        
             PERFORM S-S-COMMIT THRU S-S-COMMIT-EX                                  
           END-PERFORM.                                                         
       EX-SELEZIONA-PREZZO-DBG. EXIT.
      * NO-DATGE                                                        FINE 

185800*                                                                         
185900*PRZ-PUBBL*                                                               
      * NO-DATGE                                                        inizio 
186000*SELEZIONA-PF-SOCIETA.                                                    
186100*    MOVE SPACES TO SOCIETA-SOCIETA.                                      
186200*                                                                         
186300*    PERFORM WITH TEST AFTER                                              
186400*    UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                           
186500*      PERFORM BEGIN-RC THRU BEGIN-RC-EX                                  
186600*      IF SQLCODE = OK                                                    
186700*        EXEC SQL                                                         
186800*          SELECT SOCIETA                                                 
186900*            INTO :SOCIETA-SOCIETA                                        
187000*            FROM PF.SOCIETA                                              
187100*           WHERE COD_X_BARCODE = :SOCIETA-CODICE-SOC                     
187200*        END-EXEC                                                         
187300*      END-IF                                                             
187400*      MOVE SQLCODE TO SQLCODE-MEM                                        
187500*      PERFORM S-S-COMMIT THRU S-S-COMMIT-EX                                  
187600*    END-PERFORM.                                                         
187700*EX-SELEZIONA-PF-SOCIETA.                                                 
187800*    EXIT.                                                                
      * NO-DATGE                                                        fine 
187900*                                                                         
188000*                                                                         
188100*PRZ-PUBBL*                                                               
      * NO-DATGE                                                        inizio 
188200* SELEZIONA-MODELLO-NEW.                                                   
188300*     MOVE 0 TO FLAG-CURSORE.                                              
188400**                                                                         
188500*     MOVE SPACES TO MODELLI-MODELLO-NEW.                                  
188600*     MOVE '000'  TO MODELLI-ESTENSIONE.                                   
188700**                                                                         
188800*     PERFORM WITH TEST AFTER                                              
188900*     UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                           
189000*       PERFORM BEGIN-RC THRU BEGIN-RC-EX                                  
189100*       PERFORM DECLARE-CURS-MOD THRU DECLARE-CURS-MOD-EX                  
189200*       PERFORM OPEN-CURS-MOD THRU OPEN-CURS-MOD-EX                        
189300*       PERFORM FETCH-SINGOLA-CURS-MOD                                     
189400*          THRU FETCH-SINGOLA-CURS-MOD-EX                                  
189500*       MOVE SQLCODE TO SQLCODE-MEM                                        
189600**      IF NOT STOP-CURSORE                                                
189700**trovato modello new                                                      
189800**        CONTINUE                                                         
189900**      END-IF                                                             
190000*       PERFORM CLOSE-CURS-MOD THRU CLOSE-CURS-MOD-EX                      
190100*       PERFORM S-S-COMMIT THRU S-S-COMMIT-EX                                  
190200*     END-PERFORM.                                                         
190300* EX-SELEZIONA-MODELLO-NEW.                                                
190400*     EXIT.                                                                
190500**                                                                         
190600**                                                                         
190700**PRZ-PUBBL*                                                               
190800* SELEZIONA-PREZZO.                                                        
190900*     MOVE 0    TO PREZZI-PREZZO.                                          
191000*     MOVE '01' TO PREZZI-LISTINO.                                         
191100*     MOVE 'V'  TO PREZZI-TIPO-PREZZO.                                     
191200**                                                                         
191300*     PERFORM WITH TEST AFTER                                              
191400*     UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                           
191500*       PERFORM BEGIN-RC THRU BEGIN-RC-EX                                  
191600*       IF SQLCODE = OK                                                    
191700*         EXEC SQL                                                         
191800*           SELECT PREZZO                                                  
191900*             INTO :PREZZI-PREZZO                                          
192000*             FROM ANAMAT.PREZZI                                           
192100*            WHERE LISTINO     = :PREZZI-LISTINO AND                       
192200*                  TIPO_PREZZO = :PREZZI-TIPO-PREZZO AND                   
192300*                  MODELLO_NEW = :PREZZI-MODELLO-NEW AND                   
192400*                  ANNO        = :PREZZI-ANNO AND                          
192500*                  STAG        = :PREZZI-STAGIONE AND                      
192600*                  SOCIETA     = :PREZZI-SOCIETA                           
192700*         END-EXEC                                                         
192800*       END-IF                                                             
192900*       MOVE SQLCODE TO SQLCODE-MEM                                        
193000*       PERFORM S-S-COMMIT THRU S-S-COMMIT-EX                                  
193100*     END-PERFORM.                                                         
193200* EX-SELEZIONA-PREZZO.                                                     
193300*     EXIT.                                                                
      * NO-DATGE                                                        fine 
193400*                                                                         
193500*                                                                         
193600*PRZ-PUBBL*                                                               
193700 CERCA-PREZZO-PUBBL-2.                                                    
193800     MOVE 0 TO PREZZO-PUBBL.                                              
193900*                                                                         
194000     MOVE "PREZZI;" TO W-NOME-DATA-SET.                                   
194100     MOVE "C-MAT;"  TO W-NOME-CAMPO.                                      
194200     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.              
194300*                                                                         
194400     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
194500     IF W-OK-IMAGE                                                        
194600        MOVE 5 TO W-MODO                                                  
194700        PERFORM TTDBGET-P-PUB THRU EX-TTDBGET-P-PUB                       
194800        PERFORM TTDBGET-P-PUB THRU EX-TTDBGET-P-PUB                       
194900          UNTIL NOT W-OK-IMAGE OR                                         
195000                (MERCATO OF REC-PREZZI-PUB = 2)                           
195100     END-IF.                                                              
195200*                                                                         
195300     IF W-OK-IMAGE                                                        
195400        MOVE PREZZO-VENDITA OF REC-PREZZI-PUB(1)                          
195500          TO PREZZO-PUBBL                                                 
195600     END-IF.                                                              
195700 EX-CERCA-PREZZO-PUBBL-2.                                                 
195800     EXIT.                                                                
195900*                                                                         
196000*                                                                         
196100*                                                                         
196200*                                                                         
196300 SCORRI-TAB-SING.                                                         
196400     MOVE ELEM-TAB-SING (W-INDICE-3) TO ELEMENTO-SINGOLI.                 
196500*    DISPLAY CONT-SING SPACE C-MAT-SING SPACE D-MAT-SING.                 
      
      *UNICODDT*                                                        DISPLAY
           IF XD = "S"
               DISPLAY "SCORRI-TAB-SING"
               DISPLAY C-MAT-SING SPACE CONT-SING SPACE D-MAT-SING.
      *UNICODDT*                                                        FINE
           
196600     MOVE C-MAT-SING TO C-MAT-A-BARRE-RID.                                
196700     MOVE 0 TO C-MAT-TRANS-RID.                                           
196800     MOVE MODELLO OF C-MAT-A-BARRE TO MODELLO OF                          
196900                       C-MAT-TRANSITO                                     
197000     MOVE VESTIBILITA OF C-MAT-A-BARRE TO                                 
197100            VEST-A OF C-MAT-TRANSITO                                      
197200     MOVE SOCIETA OF C-MAT-A-BARRE TO                                     
197300                    PROGR-ART OF C-MAT-TRANSITO                           
197400     MOVE PEZZO OF C-MAT-A-BARRE TO PEZZO-A OF                            
197500                       C-MAT-TRANSITO                                     
197600     COMPUTE C-MAT-S = C-MAT-SING / 10.                                   
197700     MOVE NEG-IN TO MAG-S                                                 
197800     MOVE D-MAT-SING TO NOME-S                                            
197900     MOVE SPACES TO NOME-F-S                                              
198000                    COL-F-S                                               
198100                    SETTORE-S                                             
198200     MOVE 0 TO PREZZO-S.  
      *MOVSKU*
           MOVE SKU-SING TO BARUNI-S.
           MOVE T-TAB TO T-1 T-2 T-3 T-4 T-5 T-6 T-7 T-8 T-9
      *MOVSKU*                                                   
198300     PERFORM CERCA-PREZZO-V THRU EX-CERCA-PREZZO-V.                       
198400     COMPUTE PREZZO-S = PREZZO-MEM / 100.                                 
198500     MOVE TAGLIA OF C-MAT-A-BARRE TO TAGLIA-S   
           MOVE TAGLIA OF C-MAT-A-BARRE TO NTG-IN
           MOVE FUNCTION idxtg(NTG-IN)
                              TO W-INDICE-4               
198600     PERFORM ESPLODI-TG THRU EX-ESPLODI-TG.                               
198700 EX-SCORRI-TAB-SING.                                                      
198800     EXIT.                                                                
198900*    
200900*MOVSKU 
       INTESTA-FILE-BC.
           MOVE SPACE TO REC-BC
           STRING "C-MAT" T-TAB "TAGLIA" T-TAB "MAG" T-TAB 
               "SETTORE" T-TAB "NOME" T-TAB "PREZZO" T-TAB 
               "NOME-F" T-TAB "COL-F" T-TAB "TG-OUT" T-TAB
               "BARUNI"
              DELIMITED BY SIZE 
              INTO REC-BC.
           WRITE REC-BC.   
       EX-INTESTA-FILE-BC.
           EXIT.                                                                        
201000*MOVSKU                                                                        
199000*                                                                         
199100 CERCA-PREZZO-V.                                                          
199200     MOVE "PREZZI;" TO W-NOME-DATA-SET.                                   
199300     MOVE "C-MAT;" TO W-NOME-CAMPO.                                       
199400     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.              
199500     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
199600     IF W-OK-IMAGE                                                        
199700        MOVE 5 TO W-MODO                                                  
199800        PERFORM TTDBGET-P THRU EX-TTDBGET-P                               
199900        PERFORM TTDBGET-P THRU EX-TTDBGET-P                               
200000              UNTIL NOT W-OK-IMAGE OR                                     
200100                (MERCATO OF REC-PREZZI =                                  
200200                     LISTINO-MEM).                                        
200300     IF W-OK-IMAGE                                                        
200400*BUDA*                                                                    
200500        MOVE PREZZO-VENDITA OF REC-PREZZI(1)                              
200600            TO PREZZO-MEM.                                                
200700 EX-CERCA-PREZZO-V.                                                       
200800     EXIT.                                                                
200900*                                                                         
201000*                                                                         
201100 SCRIVI-RECORD.                                                           
201200     IF TAB-ART (W-INDICE-3) = 0                                          
201300            OR QTA-TAGLIE-TAB (W-INDICE-3) = LOW-VALUE                    
201400        GO TO EX-SCRIVI-RECORD.     
      *
201500     COMPUTE C-MAT-A-BARRE-RID = TAB-ART (W-INDICE-3) * 10.               
201600     MOVE ART-TAB-LETTI(W-INDICE-3)                                       
201700         TO ART-ELEM-LETTI.                                               
201800     MOVE 0 TO C-MAT-TRANS-RID.                                           
201900     MOVE MODELLO OF C-MAT-A-BARRE TO MODELLO OF                          
202000                         C-MAT-TRANSITO.                                  
202100     MOVE VESTIBILITA OF C-MAT-A-BARRE TO                                 
202200                  VEST-A OF C-MAT-TRANSITO.                               
202300     MOVE SOCIETA OF C-MAT-A-BARRE TO                                     
202400                      PROGR-ART OF C-MAT-TRANSITO.                        
202500     MOVE PEZZO OF C-MAT-A-BARRE TO PEZZO-A OF                            
202600                         C-MAT-TRANSITO.                                  
202700     MOVE VARIANTE-COL OF C-MAT-A-BARRE TO COLORE OF                      
202800                         C-MAT-TRANSITO.                                  
202900*BUDA*                                                                    
203000*          IF PRIORITA OF REC-INDIRIZZI = 4                               
203100*             COMPUTE C-MAT-S = C-MAT-A-BARRE-RID / 10                    
203200*             MOVE NEG-IN TO MAG-S                                        
203300*             MOVE D-MAT-ELEM TO NOME-S                                   
203400*             MOVE SPACES TO NOME-F-S                                     
203500*                            COL-F-S                                      
203600*                            SETTORE-S                                    
203700*BUDA*                                                                    
203800*             MOVE 0 TO PREZZO-S                                          
203900*             PERFORM CERCA-PREZZO THRU EX-CERCA-PREZZO                   
204000*             COMPUTE PREZZO-S = PREZZO-ELEM / 100                        
204100*             PERFORM ESPLODI-8-TG THRU EX-ESPLODI-8-TG                   
204200*                     VARYING W-INDICE-4 FROM 1 BY 1                      
204300*                        UNTIL W-INDICE-4 > NTG-NTG                             
204400*          END-IF                                                         
204500      PERFORM INVERTI-QTA THRU EX-INVERTI-QTA                             
204600            VARYING W-INDICE-5 FROM 1 BY 1                                
204700            UNTIL W-INDICE-5 > NTG-NTG.                                         
204800     PERFORM PREPARA-MOVMAG THRU EX-PREPARA-MOVMAG.                       
204900     PERFORM CREA-MOVMAG-P-3                                              
205000           THRU EX-CREA-MOVMAG-P-3.                                       
205100     PERFORM AGGIORNA-SITPF-P-3                                           
205200           THRU EX-AGGIORNA-SITPF-P-3.                                    
205300 EX-SCRIVI-RECORD.                                                        
205400     EXIT.                                                                
205500*                                                                         
205600*                                                                         
205700 INVERTI-QTA.                                                             
205800     COMPUTE QTA-TAGLIA-NEG(W-INDICE-5) =                                 
205900         QTA-TAGLIA-ELEM(W-INDICE-5) * -1.                                
206000 EX-INVERTI-QTA.                                                          
206100     EXIT.                                                                
206200*                                                                         
206300*                                                                         
206400 CERCA-PREZZO.                                                            
206500     MOVE "ANAMAT;" TO W-NOME-DATA-SET.                                   
206600     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.              
206700     MOVE 7 TO W-MODO                                                     
206800     PERFORM TTDBGET THRU EX-TTDBGET                                      
206900     IF NOT W-OK-IMAGE                                                    
207000          DISPLAY "Inesist. col 0  " C-MAT-A-BARRE-RID                    
207100       GO TO EX-CERCA-PREZZO.                                             
207200*                                                                         
207300*PRODI*                                                                   
207400     MOVE COSTO OF REC-ANAMAT TO PREZZO-ANAMAT.                           
207500*                                                                         
207600     IF MAG-STOCK
207700       PERFORM CHIAMA-DTVALSTK THRU EX-CHIAMA-DTVALSTK                    
207800     ELSE                                                                 
207900       PERFORM CERCA-PREZZIA THRU EX-CERCA-PREZZIA.                       
208000*                                                                         
208100 EX-CERCA-PREZZO.                                                         
208200     EXIT.                                                                
208300*                                                                         
208400*                                                                         
208500*PRODI*                                                                   
208600 CHIAMA-DTVALSTK.                                                         
208700     MOVE C-MAT-TRANS-RID TO STK-C-MAT.                                   
208800     MOVE COLLEZIONE OF REC-ANAMAT TO STK-COLL.                           
208900     MOVE STAGIONE OF REC-ANAMAT TO STK-STAGIONE.                         
209000     MOVE COSTO OF REC-ANAMAT TO STK-PRZ-LORDO.                           
209100*MAG6/7*                                                                  
209200*    IF SI-DT-ESTERO                                                      
209300*       MOVE NOME-IN-B TO STK-NOME                                        
209400*      ELSE                                                               
209500*         MOVE NOME-IN TO STK-NOME.                                       
209600     IF MAG-FALLATO
209700       MOVE "STF" TO NOME-IN-3                                            
209800     ELSE
      *Mag3_V/F*
             IF F-V-INPUT = "F"
                MOVE "STF" TO NOME-IN-3
             ELSE
      *
209900       MOVE "STV" TO NOME-IN-3.                                           
210000     MOVE CONTO-IN-R TO NOME-IN-5.                                        
210100     MOVE NOME-IN-35 TO STK-NOME.                                         
210200*                                                                         
210300     CALL "DTVALSTK" USING STK-NOME                                       
210400                          STK-C-MAT                                       
210500                          STK-STAGIONE                                    
210600                          STK-COLL                                        
210700                          STK-SCO                                         
210800                          STK-PRZ-SCO                                     
210900                          STK-PRZ-LORDO STK-MSG                           
211000                          STK-PRIMA-VOLTA                                 
211100                          STK-CAMBIO                                      
211200*EURO1*                                                                   
211300                          W-COMMON.                                       
211400*                                                                         
211500*IF X5=ON                                                                 
211600*    DISPLAY "STK-NOME        " STK-NOME  .                               
211700*    DISPLAY "STK-C-MAT       " STK-C-MAT .                               
211800*    DISPLAY "STK-STAGIONE    " STK-STAGIONE .                            
211900*    DISPLAY "STK-COLL        " STK-COLL .                                
212000*    DISPLAY "STK-SCO         " STK-SCO                                   
212100*    DISPLAY "STK-PRZ-SCO     " STK-PRZ-SCO                               
212200*    DISPLAY "STK-PRZ-LORDO   " STK-PRZ-LORDO .                           
212300*    DISPLAY "STK-PRIMA-VOLTA " STK-PRIMA-VOLTA .                         
212400*    DISPLAY "STK-CAMBIO      " STK-CAMBIO.                               
212500*     IF STK-MSG NOT = SPACE                                              
212600*       DISPLAY STK-MSG.                                                  
212700*IF                                                                       
212800*     toglie i decimali (Farini 12/05/99)                                 
212900*NOPRZ*                                                                   
213000*EURO*                                                                    
213100*    COMPUTE PREZZO-ANAMAT = (COSTO OF REC-ANAMAT / 100) * 100            
213200*    COMPUTE STK-PRZ-SCO = (STK-PRZ-SCO / 100) * 100.                     
213300*PRODI*                                                                   
213400*    MOVE COSTO OF REC-ANAMAT TO PREZZO-ANAMAT.                           
213500     MOVE STK-PRZ-SCO TO PREZZO-MEM.                                      
213600     MOVE STK-CAMBIO TO CAMBIO-MEM.                                       
213700 EX-CHIAMA-DTVALSTK.                                                      
213800     EXIT.                                                                
213900*                                                                         
214000*                                                                         
214100 CERCA-PREZZIA.                                                           
214200     MOVE "PREZZIA;" TO W-NOME-DATA-SET.                                  
214300     MOVE "C-MAT;" TO W-NOME-CAMPO.                                       
214400     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.              
214500     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
214600     IF W-OK-IMAGE                                                        
214700        MOVE 5 TO W-MODO                                                  
214800        PERFORM TTDBGET-P THRU EX-TTDBGET-P                               
214900        PERFORM TTDBGET-P THRU EX-TTDBGET-P                               
215000              UNTIL NOT W-OK-IMAGE OR                                     
215100                (MERCATO OF REC-PREZZI =                                  
215200                     LISTINO-MEM).                                        
215300     IF W-OK-IMAGE                                                        
215400*BUDA*                                                                    
215500        MOVE PREZZO-VENDITA OF REC-PREZZI(1)                              
215600            TO PREZZO-MEM.                                                
215700     MOVE 0 TO CAMBIO-MEM.                                                
215800 EX-CERCA-PREZZIA.                                                        
215900     EXIT.                                                                
216000*                                                                         
216100*                                                                         
216200 ESPLODI-8-TG.                                                            
216300     IF QTA-TAGLIA-ELEM (W-INDICE-4) NOT = 0                              
216400        PERFORM ESPLODI-TG THRU EX-ESPLODI-TG                             
216500              VARYING W-INDICE-5 FROM 1 BY 1                              
216600                  UNTIL W-INDICE-5 >                                      
216700                      QTA-TAGLIA-ELEM (W-INDICE-4).                       
216800 EX-ESPLODI-8-TG.                                                         
216900     EXIT.                                                                
217000*                                                                         
217100*                                                                         
217200 ESPLODI-TG.                                                              
217300     MOVE W-INDICE-4 TO TAGLIA-S.                                         
217400     COMPUTE TG-CAL = PRIMA-TG-SING + (W-INDICE-4 * 2 - 2).               
217500     PERFORM T-TG THRU EX-T-TG.                                           
217600     WRITE REC-BC.                                                        
217700     IF CLASSE OF C-MAT-TRANSITO = 52                                     
217800        WRITE REC-BC.                                                     
217900 EX-ESPLODI-TG.                                                           
218000     EXIT.                                                                
218100*                                                                         
218200*                                                                         
218300 T-TG.                                                                    
218400     IF TG-CAL < 79                                                       
218500        MOVE TG-CAL TO TG-OUT-S.                                          
218600     IF TG-CAL = 80                                                       
218700        MOVE "XS" TO TG-OUT-S.                                            
218800     IF TG-CAL = 82                                                       
218900        MOVE " S" TO TG-OUT-S.                                            
219000     IF TG-CAL = 84                                                       
219100        MOVE " M" TO TG-OUT-S.                                            
219200     IF TG-CAL = 86                                                       
219300        MOVE " L" TO TG-OUT-S.                                            
219400     IF TG-CAL = 88                                                       
219500        MOVE "XL" TO TG-OUT-S.                                            
219600 EX-T-TG.                                                                 
219700     EXIT.                                                                
219800*                                                                         
219900*                                                                         
220000*PAGE                                                                     
220100*                                                                         
220200 TTUNLOCK.                                                                
220300     COPY PDBUNLOC.                                                       
220400*                                                                         
220500*                                                                         
220600 TTLOCK-T.  COPY PDBLOCK REPLACING                                        
220700                W-NOME-DATA-SET BY TAB-LOCK                               
220800                EX-TTLOCK BY EX-TTLOCK-T.                                 
220900*                                                                         
221000 TTLOCK.                                                                  
221100     COPY PDBLOCK.                                                        
221200*                                                                         
221300*                                                                         
221400 TTDBPUT.                                                                 
221500     COPY PDBPUT.                                                         
221600*                                                                         
221700*                                                                         
221800 TTUPDATE.                                                                
221900     COPY PDBUPDAT.                                                       
222000*                                                                         
222100*                                                                         
222200 TTDBGET-P.   COPY PDBGET REPLACING                                       
222300           AREA-REC-SET BY REC-PREZZI                                     
222400           EX-TTDBGET BY EX-TTDBGET-P.                                    
222500*                                                                         
222600*                                                                         
222700*PRZ-PUBBL*                                                               
222800 TTDBGET-P-PUB.                                                           
222900     COPY PDBGET REPLACING AREA-REC-SET                                   
223000                        BY REC-PREZZI-PUB                                 
223100                           EX-TTDBGET                                     
223200                        BY EX-TTDBGET-P-PUB.                              
223300*                                                                         
223400*                                                                         
223500*PAGE                                                                     
223600*                                                                         
223700*                                                                         
223800 DISP-C-MAT.                                                              
223900     MOVE SPACES TO RIGA-DISP.                                            
224000     MOVE 1 TO IND-4.                                                     
224100     PERFORM METTI-4 THRU EX-METTI-4                                      
224200            UNTIL IND-4 > 3 OR                                            
224300              W-INDICE-3 > QT-NUM-ELEM-EFF OF PARTAB-ART.                 
224400     DISPLAY RIGA-DISP.                                                   
224500 EX-DISP-C-MAT.                                                           
224600     EXIT.                                                                
224700*                                                                         
224800*                                                                         
224900 METTI-4.                                                                 
225000     IF TAB-ART (W-INDICE-3) = 0                                          
225100            OR QTA-TAGLIE-TAB (W-INDICE-3) = LOW-VALUE                    
225200        ADD 1 TO W-INDICE-3                                               
225300        GO TO EX-METTI-4.                                                 
225400     MOVE TAB-ART (W-INDICE-3) TO DISP-ART (IND-4).                       
225500     MOVE "(" TO PARE1(IND-4).                                            
225600     MOVE ")" TO PARE2(IND-4).                                            
225700     MOVE ART-TAB-LETTI(W-INDICE-3) TO ART-ELEM-LETTI.                    
225800     MOVE 0 TO COM-QTA-DISP.                                              
225900     PERFORM ACCUMULA-QTA THRU EX-ACCUMULA-QTA                            
226000         VARYING W-INDICE-7 FROM 1 BY 1                                   
226100         UNTIL W-INDICE-7 > NTG-NTG.                                            
226200     MOVE COM-QTA-DISP TO QTA-DISP(IND-4).                                
226300     ADD COM-QTA-DISP TO TOT-CAPI-LETTI-1.                                
226400     ADD 1 TO W-INDICE-3 IND-4.                                           
226500 EX-METTI-4.                                                              
226600     EXIT.                                                                
226700*                                                                         
226800*                                                                         
226900 ACCUMULA-QTA.                                                            
227000     ADD QTA-TAGLIA-ELEM(W-INDICE-7) TO COM-QTA-DISP.                     
227100 EX-ACCUMULA-QTA.                                                         
227200     EXIT.                                                                
227300*                                                                         
227400*                                                                         
227500*                                                                         
227600*                                                                         
227700 AGG-DPARAM.                                                              
227800     MOVE "DPARAM;" TO W-NOME-DATA-SET.                                   
227900      ADD 1 TO NUM-BOLLA-TAGLIO-FODERE.                                   
228000      MOVE REC-PARAM-RID TO AREA-REC-SET.                                 
228100      PERFORM TTUPDATE THRU EX-TTUPDATE.                                  
228200 EX-AGG-DPARAM.                                                           
228300      EXIT.                                                               
228400*
227600*                                                                         
227700 AGG-DPARAM-FITTIZI.                                                              
227800     MOVE "DPARAM;" TO W-NOME-DATA-SET.                                   
227900      ADD 1 TO PAR-FITTIZIO-1.                                   
228000      MOVE REC-PARAM-FITTIZ-R TO AREA-REC-SET.                                 
228100      PERFORM TTUPDATE THRU EX-TTUPDATE.                                  
228200 EX-AGG-DPARAM-FITTIZI.                                                           
228300      EXIT.                                                               
228400*                                        
228500*                                                                         
      *
       GET-LOCALITA.
           MOVE SPACES TO LOCALITA-PART-STR.
      *
           PERFORM VARYING IND-LOC FROM 1 BY 1
           UNTIL (IND-LOC > MAX-LOC) OR (LOCALITA-PART-STR NOT = SPACES)
              IF COD-LOC(IND-LOC) = MAG-INPUT-R
                 MOVE DESC-LOC(IND-LOC) TO LOCALITA-PART-STR
              END-IF
           END-PERFORM.
      *    DISPLAY '>'LOCALITA-PART-STR'<'.
       EX-GET-LOCALITA.
           EXIT.
      *
228600*                                                                         
228700 CHIAMA-PRINTDDT.                                                         
228800*MAG6/7*                                                                  
228900*    MOVE 7 TO MAGAZZINO-DDT.                                             
229000     MOVE MAG-INPUT-R TO MAGAZZINO-DDT.                                   
229100     MOVE 1 TO TIPO-DOC-DDT.                                              
229200     MOVE 11 TO TIPO-MOVIMENTO-DDT.                                       
229300     MOVE "VESD" TO CAUSALE-DDT.                                          
229400     MOVE 1 TO TIPO-STAMPA-DDT.                                           
229500*FIFRA*                                                                   
229600*    MOVE CONTO-IN-R TO CLIENTE-DDT.                                      
229700     IF CONTO-FATTURA-MEM NOT = 0                                         
229800       MOVE CONTO-FATTURA-MEM TO CLIENTE-DDT                              
229900     ELSE                                                                 
230000       MOVE CONTO-IN-R TO CLIENTE-DDT.                                    
230100*                                                                         
           PERFORM GET-LOCALITA THRU EX-GET-LOCALITA.
230200     MOVE LOCALITA-PART-STR TO LOC-PART-DDT.                              
230300     MOVE SPACES TO TIPO-DATA-SET-DDT                                     
230400                    D-CONTO-VET                                           
230500                    NOTE-DDT (1)                                          
230600                    NOTE-DDT (2).    
      **FSTOCK*
      *       IF W-SIGLA-UTENTE = "RESIDUO" OR = "PROROSA"
      *         MOVE "Merce distrib/prod da Diffusione Tessile srl"  
      *             TO NOTE-DDT(1)
      *         MOVE 
      *              " unipersonale; via Santi,8 42025Cavriago(RE)"                 
      *             TO NOTE-DDT(2)
      *        END-IF.
      **
230700     MOVE 0 TO IMPORTO-X-PL.                                              
230800*BUDA*                                                                    
230900     MOVE "N"  TO FILE-FAT-DDT.                                           
231000*TRAVMAG*                                                                 
231100     IF W-SIGLA-UTENTE = "MAXMAX"                                         
231200       MOVE                                                               
231300 "MITTENTE DEPOSITARIO:MAXIMA SPA C/O DEPOSITO DIFFUSIONE TESSILE"                                                   
231500              TO RIGA-1-DDT                                               
231600       MOVE                                                               
231700 "SRL - VIA SANTI,8 42025 CAVRIAGO (RE) (SCARICO CONTO DEPOSITO)"                                                     
231900              TO RIGA-2-DDT                                               
232000     ELSE
      **FSTOCK*      alternativa
      *       IF W-SIGLA-UTENTE = "RESIDUO" OR = "PROROSA"
      *         MOVE "Merce distrib e/o prodotta da Diffusione Tessile sr      
      *-          "l unipersonale"
      *             TO RIGA-1-DDT
      *         MOVE "via Santi, 8 42025 Cavriago (RE)"                 
      *             TO RIGA-2-DDT
      *        
      *        ELSE
      **
232100       MOVE SPACE TO RIGA-1-DDT RIGA-2-DDT.                               
232200*                                                                         
232300*PRINTDD6*                                                                
232400*    CALL "PRINTDD3" USING W-COMMON                                       
           CANCEL "PRINTDDF"
232500     CALL "PRINTDDF" USING W-COMMON                                       
232600                           SQLCA                                          
232700                           CAMPI-ANAGRAFICI                               
232800                           CAMPI-COMODO                                   
232900                           TIPO-DATA-SET-DDT                              
233000                           IMPORTO-X-PL                                   
233100*BUDA*                                                                    
233200                           FILE-FAT-DDT                                   
233300*TRAVMAG*                                                                 
233400                           RIGA-1-DDT                                     
233500                           RIGA-2-DDT.                                    
233600*                                                                         
233700 EX-CHIAMA-PRINTDDT.                                                      
233800     EXIT.                                                                
233900*                                                                         
234000*                                                                         
234100 PREPARA-MOVMAG.                                                          
234200     MOVE LOW-VALUE TO MOVMAG.                                            
234300     MOVE SPACES TO VAL-REC OF MOVMAG.                                    
234400     MOVE 1 TO NUMERO-RIGA OF MOVMAG.                                     
234500     MOVE W-FORMATO-INTERNO TO Q-DATA-I.                                  
234600     MOVE 2 TO Q-FUNZIONE OF PARGEN.                                      
           CANCEL "QDATAS"
234700      CALL "QDATAS" USING PARGEN                                          
234800                         Q-DATA-E Q-DATA-I                                
234900                         Q-SETTIMANA.                                     
235000     MOVE Q-SETTIMANA                                                     
235100       TO SETTIMANA OF MOVMAG.                                            
235200     MOVE RIF-BOLLA-DDT TO RIF-INTERNO OF MOVMAG.                         
235300     MOVE 0 TO RIF-BOLLA-FORN OF MOVMAG                                   
235400               RIF-ORDINE OF MOVMAG                                       
235500               MOD-IMPUTAZ OF MOVMAG                                      
235600               QUANTITA OF MOVMAG.                                        
      *VACO*                                                            inizio
235700*     MOVE 0 TO PREZZO OF MOVMAG.       
      *VACO*                                                            fine                                           
235800     MOVE SPACES TO DIVISA OF MOVMAG                                      
235900                    VAL-REC OF MOVMAG.                                    
236000     MOVE "NR" TO UN-MIS-FATT OF MOVMAG.                                  
236100*EURO*                                                                    
236200     MOVE "EUR " TO DIVISA OF MOVMAG.                                     
236300*    MOVE "LIT " TO DIVISA OF MOVMAG.                                     
236400     MOVE C-MAT-TRANS-RID TO C-MAT OF MOVMAG.                             
236500 EX-PREPARA-MOVMAG.                                                       
236600     EXIT.                                                                
236700*                                                                         
236800*                                                                         
236900*                                                                         
237000 CREA-MOVMAG-P-3. 
      *VACO*                                                            inizio      
            MOVE COSTO-ELEM TO PREZZO OF MOVMAG.
      *VACO*                                                            fine 
237100     MOVE PREZZO-ELEM TO COSTO-STD OF MOVMAG.                             
237200*BUDA*                                                                    
237300     MOVE CAMBIO-ELEM TO MOD-IMPUTAZ OF MOVMAG.                           
237400*BUDA*                                                                    
237500     MOVE "VESD" TO C-OPE OF MOVMAG.                                      
237600     MOVE QTA-TAGLIE-NEG TO QTA-TAGLIE OF MOVMAG.                         
237700*MAG6/7*                                                                  
237800*    MOVE 7 TO MAGAZZINO OF MOVMAG.                                       
237900     MOVE MAG-INPUT-R TO MAGAZZINO OF MOVMAG.                             
238000*FIFRA*                                                                   
238100*    MOVE CONTO-IN-R TO CONTO OF MOVMAG.                                  
238200     IF CONTO-FATTURA-MEM NOT = 0                                         
238300       MOVE CONTO-FATTURA-MEM TO CONTO OF MOVMAG                          
238400       MOVE CONTO-IN-R TO MOD-IMPUTAZ OF MOVMAG                           
238500     ELSE                                                                 
238600       MOVE CONTO-IN-R TO CONTO OF MOVMAG.                                
238700     MOVE MOVMAG TO AREA-REC-SET.                                         
238800*                                                                         
238900     MOVE "MOVMAG" TO W-NOME-DATA-SET.                                    
239000     PERFORM TTDBPUT THRU EX-TTDBPUT.                                     
239100     IF NOT W-OK-IMAGE                                                    
239200       MOVE W-STATUS-WORD-IMAGE TO STATO-DISPLAY                          
239300       DISPLAY "ERR PUT MOVMAG-P3- " STATO-DISPLAY                        
239400       DISPLAY "PER C-MAT " C-MAT-TRANSITO                                
           CANCEL "QDBERROR"
239500       CALL "QDBERROR" USING W-COMMON.                                    
239600 EX-CREA-MOVMAG-P-3.                                                      
239700     EXIT.                                                                
239800*                                                                         
239900*                                                                         
240000*                                                                         
240100*                                                                         
240200 AGGIORNA-SITPF-P-3.                                                      
240300     MOVE LOW-VALUE TO PARAGGPF.                                          
240400     MOVE C-MAT-TRANS-RID TO C-MAT OF PARAGGPF.                           
240500*MAG6/7*                                                                  
240600*    MOVE 7 TO MAGAZZINO OF PARAGGPF.                                     
240700     MOVE MAG-INPUT-R TO MAGAZZINO OF PARAGGPF.                           
240800     MOVE -1 TO VALORE OF PARAGGPF.                                       
240900     MOVE QTA-TAGLIE-NEG                                                  
241000       TO QTA-8 OF PARAGGPF.                                              
241100     MOVE 1 TO F-GIAC OF PARAGGPF.                                        
           CANCEL "AGSITPFW"
241200     CALL "AGSITPFW" USING W-COMMON PARAGGPF.                             
241300 EX-AGGIORNA-SITPF-P-3.                                                   
241400     EXIT.                                                                
241500*                                                                         
241600*                                                                         
241700*                                                                         
241800*                                                                         
241900*                                                                         
242000*                                                                         
242100*                                                                         
242200*                                                                         
242300 CARICA-TABELLA.                                                          
242400     MOVE 0 TO STK-C-MAT.                                                 
242500     MOVE 0 TO STK-STAGIONE.                                              
242600     MOVE 0 TO STK-COLL.                                                  
242700     MOVE 0 TO STK-PRZ-LORDO STK-PRIMA-VOLTA.                             
242800*MAG6/7*                                                                  
242900*    IF SI-DT-ESTERO                                                      
243000*       MOVE NOME-IN-B TO STK-NOME                                        
243100*      ELSE                                                               
243200*         MOVE NOME-IN TO STK-NOME.  
243300     IF MAG-FALLATO
243400       MOVE "STF" TO NOME-IN-3                                            
243500     ELSE
      *Mag3_V/F*
             IF F-V-INPUT = "F"
                MOVE "STF" TO NOME-IN-3
             ELSE
      *
243600       MOVE "STV" TO NOME-IN-3.                                           
243700     MOVE CONTO-IN-R TO NOME-IN-5.                                        
243800     MOVE NOME-IN-35 TO STK-NOME.   
243900*                                                                         
244000     CALL "DTVALSTK" USING STK-NOME                                       
244100                          STK-C-MAT                                       
244200                          STK-STAGIONE                                    
244300                          STK-COLL                                        
244400                          STK-SCO                                         
244500                          STK-PRZ-SCO                                     
244600                          STK-PRZ-LORDO STK-MSG                           
244700                          STK-PRIMA-VOLTA                                 
244800                          STK-CAMBIO                                      
244900*EURO1*                                                                   
245000                          W-COMMON.                                       
245100*                                                                         
245200*      IF STK-MSG NOT = SPACE                                              
245300*        DISPLAY STK-MSG
      *        MOVE 1 TO JRUNC
245400*        STOP RUN JRUNC.                                                         
245500 EX-CARICA-TABELLA.                                                       
245600     EXIT.                                                                
245700*                                                                         
245800*                                                                         
245900*                                                                         
246000 STAMPA-RAPPORTINO. 
           PERFORM VARYING W-INDICE-3 FROM 1 BY 1 UNTIL 
            W-INDICE-3 > QT-NUM-ELEM-EFF OF PARTAB-ART
              DISPLAY 'M' TAB-ART OF ART-TAB-LETTI(W-INDICE-3)
           END-PERFORM                                                       
246100     CALL "RAPPRAI3" USING W-COMMON SQLCA                                 
246200                           TABELLA-ARTICOLI-LETTI PARTAB-ART              
246300                           CONTO-IN-R D-CONTO-MEM                         
246400                           TABELLA-NO-GIAC IND-CAPI-NO-GIAC               
246500*MAG6/7*                                                                  
246600                           MAG-INPUT-R.                                   
246700 EX-STAMPA-RAPPORTINO.                                                    
246800     EXIT.                                                                
246900*                                                                         
247000*   
247100*MAG6/7*                                                                  
247200 VERIF-MAG.    
      *VIBLO*
247300     DISPLAY "MAG provenienza (3 cifre)".                                 
247400     ACCEPT MAG-INPUT.                                                    
247500     IF MAG-INPUT NOT NUMERIC                                             
             PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
247600       DISPLAY "MAG non numerico"                                         
247700     ELSE                                                                 
              IF NOT MAG-OK
               PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
248100         DISPLAY SUGG-MAG-DISP
248200*                                                                         
248300         MOVE SPACE TO MAG-INPUT.   
      *     MOVE "003" TO MAG-INPUT.
      *
248400 EX-VERIF-MAG.                                                            
248500     EXIT.                                                                
248600*
     
      *Mag3_V/F*
      * Copiaincollato senza pieta` dalla procedurina precedente
       VERIF-F-V.   
      *VIBLO*
      *     DISPLAY "Stock Fallato o Valido? (F/V)".                                 
      *     ACCEPT F-V-INPUT.                                                    
      *     IF NOT (F-V-INPUT = "F" OR = "V" OR = "f" OR = "v")                                             
      *       PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
      *       DISPLAY "Digitare o 'F' o 'V'"                                                                     
      *       MOVE SPACE TO F-V-INPUT
      *     ELSE
      *       IF F-V-INPUT = "v" OR = "V"
      *         MOVE "V" TO F-V-INPUT
      *       ELSE
      *         MOVE "F" TO F-V-INPUT.
           MOVE "V" TO F-V-INPUT.
      *
       EX-VERIF-F-V.                                                            
           EXIT.

248700*PRZ-PUBBL*                                                               
248800***************** ROUTINES SQL *******************                        
248900*                                                                         
249000 TEST-ERR.                                                                
249100     MOVE SQLCODE TO SQL-STATUS.                                          
249200     IF SQLCODE = OK OR NO-MEMORY OR DEADLOCK OR NOT-FOUND                
249300        CONTINUE                                                          
249400     ELSE                                                                 
           CANCEL "CALLSQLE"
249500        CALL "CALLSQLE" USING SQLCA PAR-ERR AREA-HL AREA-SI.              
249600 TEST-ERR-EX.                                                             
249700     EXIT.                                                                
249800*                                                                         
249900*                                                                         
250000 BEGIN-RC.                                                                
250100     EXEC SQL                                                             
250200        BEGIN WORK RC                                                     
250300     END-EXEC                                                             
250400     MOVE "BEGIN WORK RC" TO ER-DESCRIZIONE                               
250500     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
250600 BEGIN-RC-EX.                                                             
250700     EXIT.                                                                
250800*                                                                         
250900*                                                                         
251000 S-S-COMMIT.                                                                
251100     EXEC SQL                                                             
251200        COMMIT WORK                                                       
251300     END-EXEC.                                                            
251400     MOVE "COMMIT WORK" TO ER-DESCRIZIONE                                 
251500     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
251600 S-S-COMMIT-EX.                                                             
251700     EXIT.                                                                
251800*                                                                         
251900*                                                                         
      * NO-DATGE                                                        inizio 
252000* DECLARE-CURS-MOD.                                                        
252100*     EXEC SQL                                                             
252200*       DECLARE CURSMOD CURSOR FOR                                         
252300*           SELECT MODELLO_NEW,                                            
252400*                  ANNO,                                                   
252500*                  STAG                                                    
252600*             FROM ANAMAT.MODELLI                                          
252700*            WHERE                                                         
252800*           SOCIETA         = :MODELLI-SOCIETA AND                         
252900*           COD_ESTENSIONE  = :MODELLI-ESTENSIONE AND                      
253000*           MODELLO_MAXIMA  = :MODELLI-MODELLO-MAXIMA                      
253100*     END-EXEC.                                                            
253200* DECLARE-CURS-MOD-EX.                                                     
253300*     EXIT.                                                                
253400**                                                                         
253500**                                                                         
253600* OPEN-CURS-MOD.                                                           
253700*     EXEC SQL                                                             
253800*          OPEN CURSMOD KEEP CURSOR                                        
253900*     END-EXEC.                                                            
254000*     MOVE 'OPEN-CURS-MOD' TO ER-DESCRIZIONE.                              
254100*     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
254200* OPEN-CURS-MOD-EX.                                                        
254300*     EXIT.                                                                
254400**                                                                         
254500**                                                                         
254600* FETCH-SINGOLA-CURS-MOD.                                                  
254700*     EXEC SQL                                                             
254800*       FETCH CURSMOD                                                      
254900*       INTO :MODELLI-MODELLO-NEW,                                         
255000*            :MODELLI-ANNO,                                                
255100*            :MODELLI-STAGIONE                                             
255200*     END-EXEC.                                                            
255300**                                                                         
255400*     IF SQLCODE NOT = OK                                                  
255500*       MOVE 1 TO FLAG-CURSORE                                             
255600*     END-IF.                                                              
255700* FETCH-SINGOLA-CURS-MOD-EX.                                               
255800*     EXIT.                                                                
255900**                                                                         
256000**                                                                         
256100* CLOSE-CURS-MOD.                                                          
256200*     EXEC SQL                                                             
256300*          CLOSE CURSMOD                                                   
256400*     END-EXEC.                                                            
256500*     MOVE 'CLOSE-CURS-MOD' TO ER-DESCRIZIONE.                             
256600*     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
256700* CLOSE-CURS-MOD-EX.                                                       
256800*     EXIT.                                                                
      * NO-DATGE                                                        fine 
256900*                                                                         
257000*                                                                         
      *
      *
       COMANDI-IGP-TESTA.
           MOVE COMIGP-PTXSETUP TO REC-PEND.
           WRITE REC-PEND.
           MOVE COMIGP-PTXCFG2 TO REC-PEND.
           WRITE REC-PEND.
           MOVE COMIGP-PTXEND TO REC-PEND.
           WRITE REC-PEND.
       EX-COMANDI-IGP-TESTA.
           EXIT.
      *
      *
       COMANDI-IGP-TESTA-1.
           MOVE COMIGP-LISTEN TO REC-PEND.
           WRITE REC-PEND.
           MOVE COMIGP-CREATE TO REC-PEND.
           WRITE REC-PEND.
           MOVE COMIGP-SCALEDOT TO REC-PEND.
           WRITE REC-PEND.
           MOVE COMIGP-ALPHA TO REC-PEND.
           WRITE REC-PEND.
       EX-COMANDI-IGP-TESTA-1.
           EXIT.
      *
      *
       COMANDI-IGP-FINE-1.
           MOVE COMIGP-STOP TO REC-PEND.
           WRITE REC-PEND.
           MOVE COMIGP-END TO REC-PEND.
           WRITE REC-PEND.
           MOVE COMIGP-EXECUTE TO REC-PEND.
           WRITE REC-PEND.
           MOVE COMIGP-QUIET TO REC-PEND.
           WRITE REC-PEND.
       EX-COMANDI-IGP-FINE-1.
           EXIT.
      *
      *
       SCRIVI-RECORD-IGP.
           MOVE SPACES TO REC-PEND.
      *
           MOVE ROW-IGP TO TEMP-X-IN.
           PERFORM TOGLI-ZERI THRU EX-TOGLI-ZERI.
           MOVE TEMP-X-OUT TO ROW-X-IGP.
      *
           MOVE COL-IGP TO TEMP-X-IN.
           PERFORM TOGLI-ZERI THRU EX-TOGLI-ZERI.
           MOVE TEMP-X-OUT TO COL-X-IGP.
      *
           STRING "C12;"        DELIMITED BY SIZE
                  ROW-X-IGP     DELIMITED BY SPACE
                  ";"           DELIMITED BY SIZE
                  COL-X-IGP     DELIMITED BY SPACE
                  ";"           DELIMITED BY SIZE
                  VERT-EXP-IGP  DELIMITED BY SIZE
                  ";"           DELIMITED BY SIZE
                  ORIZ-EXP-IGP  DELIMITED BY SIZE
                  ';"'          DELIMITED BY SIZE
                  RIGA-PEND-COM DELIMITED BY SIZE
                  '"'           DELIMITED BY SIZE
             INTO REC-PEND.
      *
           WRITE REC-PEND.
       EX-SCRIVI-RECORD-IGP.
           EXIT.
      *
      *
       TOGLI-ZERI.
           MOVE 0 TO PRIMO-NONZERO.
           PERFORM VARYING K FROM 1 BY 1
             UNTIL K > 4 OR PRIMO-NONZERO <> 0
               IF TEMP-EL-X OF TEMP-X-IN(K) <> "0"
                   MOVE K TO PRIMO-NONZERO
               END-IF
           END-PERFORM.
      *
           IF PRIMO-NONZERO <> 0
             UNSTRING TEMP-X-IN INTO TEMP-X-OUT
                      WITH POINTER PRIMO-NONZERO
           ELSE
             MOVE TEMP-X-IN TO TEMP-X-OUT
           END-IF.
       EX-TOGLI-ZERI.
           EXIT.
      *
      *
      *MOVSKU 
       INSERISCI-MOVSKU.
      *
       MOVE ELEM-TAB-SING(IND-BARUNI) TO ELEMENTO-SINGOLI.
      *
       MOVE '1' TO OUTPUT-VAL-A    
       MOVE SPACES TO OUTPUT-VAL-B  
       PERFORM CHIAMA-GETBARUNI THRU CHIAMA-GETBARUNI-EX.
      *  
       PERFORM CONVERTI-BARCODE THRU EX-CONVERTI-BARCODE.
      * 
       MOVE C-MAT-TRANS-RID TO MOVSKU-CMAT.
       MOVE TAGLIA OF C-MAT-A-BARRE TO NTG-IN
       MOVE FUNCTION idxtg(NTG-IN)
                                TO MOVSKU-TG.
      * 
       IF OUTPUT-VAL-A = '0'
          MOVE OUTPUT-VAL-B-OK TO MOVSKU-BARUNI
       ELSE
          STOP RUN
       END-IF. 
      * 
       PERFORM INSERT-SKU-E-SING THRU EX-INSERT-SKU-E-SING.
      * 
       MOVE MOVSKU-BARUNI(1:8)          TO MOVSKU-SKU
       MOVE RIF-BOLLA-DDT TO  MOVSKU-RIF-INTERNO.
       MOVE CONTO-IN-R TO MOVSKU-CONTO. 
       MOVE MAG-INPUT-R TO MOVSKU-MAG.     
       MOVE 0 TO MOVSKU-IS-BARUNI-READ.
       MOVE 0 TO MOVSKU-IS-BARUNI-CERTIFIED.
       MOVE MOVSKU-BARUNI(1:8) TO MOVSKU-SKU-FATTURAZIONE.
      * 
      * NO-DATGE                                                        inizio
      *PERFORM S-SET-1 THRU S-SET-1-EX. 
      * NO-DATGE                                                        fine

           EXEC SQL                                                             
               INSERT INTO MOV_SKU                                          
               VALUES(
                       NULL,
                      :MOVSKU-RIF-INTERNO,
                      :MOVSKU-CMAT,
                      :MOVSKU-TG,
                      :MOVSKU-BARUNI,
                      :MOVSKU-CONTO,                                                                     
                      :MOVSKU-MAG,
                      :MOVSKU-SKU,
                      :MOVSKU-IS-BARUNI-READ,
                      :MOVSKU-IS-BARUNI-CERTIFIED,
                      :MOVSKU-SKU-FATTURAZIONE
                      )                                     
           END-EXEC                                                             
           MOVE "Insert MOV-SKU " TO ER-DESCRIZIONE                        
           PERFORM TEST-ERR THRU TEST-ERR-EX.      
      * 
       INSERISCI-MOVSKU-EX.
           EXIT.
      * 
014600 CONVERTI-BARCODE.                                                      
014700     MOVE ZEROS                                                           
014800       TO C-MAT-COM.                                  
014900*                                                                         
015000     MOVE C-MAT-SING                                                
015100       TO C-MAT-A-BARRE-RID.                                    
015200*   
015300     MOVE MODELLO OF  C-MAT-A-BARRE                                       
015400       TO MODELLO      OF      C-MAT-COM.                                     
015500     MOVE VESTIBILITA OF  C-MAT-A-BARRE                                    
015600       TO VEST-A       OF      C-MAT-COM.                                      
015700     MOVE PEZZO   OF  C-MAT-A-BARRE                                       
015800       TO PEZZO-A      OF      C-MAT-COM.                                       
015900     MOVE PREFBC-V-F OF  C-MAT-A-BARRE                                    
016000       TO PREFISSO-V-F OF      C-MAT-COM.                                       
016100     MOVE SOC-BC-MOD OF  C-MAT-A-BARRE                                       
016200       TO SOCIETA-MOD  OF      C-MAT-COM.                                     
016300     MOVE VARIANTE-COL OF  C-MAT-A-BARRE                                       
016400       TO COLORE       OF      C-MAT-COM.                                     
016500 EX-CONVERTI-BARCODE.                                                   
016600     EXIT.          
      *
       CHIAMA-GETBARUNI.
      *        
           MOVE C-MAT-SING TO INPUT-VAL
           MOVE RIF-BOLLA-DDT TO INPUT-VAL-B
           MOVE "READVE3" TO INPUT-VAL-C
      *     
           CALL "PYTHON" USING "ret_sku" "mod_sku_from_cobol" 
                               PY-INPUT-REC
                               PY-OUTPUT-REC.
           IF OUTPUT-VAL-A = '1'                    
             DISPLAY OUTPUT-VAL-B.
      *
       CHIAMA-GETBARUNI-EX.
           EXIT.
      *
       INSERT-SKU-E-SING.  
      *                
           MOVE "K2" TO QT-FUNZIONE OF PARTAB-SING                              
               CANCEL "QTABEL"
           CALL "QTABEL" USING PARTAB-SING 
                                TABELLA-SINGOLI               
                                  ELEMENTO-SINGOLI                      
      *
           IF QT-STATO OF PARTAB-SING = 0
              MOVE OUTPUT-VAL-B-OK TO SKU-SING
              MOVE SKU-SING TO 
                      ELEM-TAB-SING(QT-INDEX-ELEM OF PARTAB-SING)(24:13)  
           ELSE
              MOVE QT-STATO OF PARTAB-SING TO ERR-DISP
              DISPLAY "ERR UPDATE QTABEL " ERR-DISP                           
                        " - INSERISCI-SKU"                                          
              CANCEL "QDBERROR"
              CALL "QDBERROR" USING W-COMMON                                  
           END-IF.       
      *       
       EX-INSERT-SKU-E-SING.
           EXIT.
      * 
      * NO-DATGE                                                        inizio
      ***********connessione a DATGE*******************      
020300* S-SET-2.                                                                 
020400*        EXEC SQL                                                          
020500*           SET CONNECTION 'DB2'                                           
020600*        END-EXEC.                                                         
020700* S-SET-2-EX.                                                              
020800*     EXIT.
      ** NO-DATGE                                                        fine
      *
      ***********connessione a MAGAUTO*****************       
020300*S-SET-1.                                                                 
020400*        EXEC SQL                                                          
020500*           SET CONNECTION 'DB1'                                           
020600*        END-EXEC.                                                         
020700* S-SET-1-EX.                                                              
020800*     EXIT.                             
      *          
      *MOVSKU         
      
      *
      *ASOLOB2C*                                                        inizio
       CARICA-B2C-NO-DT.         
      *
075340     PERFORM WITH TEST AFTER                                              
075350             UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                   
075360             PERFORM BEGIN-RC THRU                                        
075370                     BEGIN-RC-EX                                          
075380             IF SQLCODE = OK                                              
075390                     PERFORM SE-SELECT-NEGOZIO-CATEG                           
075400                        THRU SE-SELECT-NEGOZIO-CATEG-EX                          
075410             END-IF                                                       
075420     END-PERFORM.                                                         
075430     IF SQLCODE = NOT-FOUND                                               
075440        DISPLAY "NESSUNA NEGOZIO CATEGORIA TROVATA"
                                      UPON SYSERR
              MOVE 0 TO NUM-B2C-NO-DT
075450      ELSE                                                               
075460        MOVE SQLERRD (3) TO NUM-B2C-NO-DT
            END-IF.
      *
075470     PERFORM S-S-COMMIT THRU S-S-COMMIT-EX.   
        
      *     DISPLAY SPACE.
      *     DISPLAY "TAB-B2C-NO-DT:  " NUM-B2C-NO-DT
      *     PERFORM VARYING IND-B2C-NO-DT FROM 1 BY 1 
      *             UNTIL IND-B2C-NO-DT > NUM-B2C-NO-DT
      *       DISPLAY MAG-B2C-NO-DT(IND-B2C-NO-DT)
      *     END-PERFORM. 
  
       EX-CARICA-B2C-NO-DT.
           EXIT.                                                                        
075510*                                                                         
075520*                                                                         
075530 SE-SELECT-NEGOZIO-CATEG.                                                        
075540     EXEC SQL  
075550          BULK SELECT NEGOZIO                                               
075570          INTO :TAB-B2C-NO-DT                                             
075580          FROM NEGOZIO_ANAG_CATEGORIA 
                   JOIN NEGOZIO_CATEGORIA USING (ID_CATEGORIA)
                   where DESC_CATEGORIA = 'NEGOZI_ITALIA_B2C_SOC' 
                   order by NEGOZIO                                               
075610     END-EXEC                                                             
075620     MOVE "SELECT NEGOZIO-CATEG  " TO ER-DESCRIZIONE                          
075630     IF SQLCODE NOT = MULTIPLE-ROWS                                       
075640        PERFORM TEST-ERR THRU TEST-ERR-EX.                                
075650 SE-SELECT-NEGOZIO-CATEG-EX.                                                     
075660     EXIT.   

      *ASOLOB2C*                                                        fine            
      
      
      
      *ASOLOB2C*                                                        inizio
      
       RIVALUTA-COSTO-ANAMAT.
            PERFORM CERCA-B2C-NO-DT THRU EX-CERCA-B2C-NO-DT.
      *
           PERFORM RICERCA-COSTO-ANAMAT THRU EX-RICERCA-COSTO-ANAMAT.   

       EX-RIVALUTA-COSTO-ANAMAT.
                                        
       CERCA-B2C-NO-DT.
           PERFORM VARYING IND-B2C-NO-DT FROM 1 BY 1
                UNTIL IND-B2C-NO-DT > NUM-B2C-NO-DT OR
                    ANACST-MAG-COM = MAG-B2C-NO-DT(IND-B2C-NO-DT)
              CONTINUE
           END-PERFORM.
           IF IND-B2C-NO-DT > NUM-B2C-NO-DT 
             MOVE 'N' TO FLAG-B2C-NO-DT
           ELSE
             MOVE 'S' TO FLAG-B2C-NO-DT
           END-IF.  
       EX-CERCA-B2C-NO-DT.
           EXIT.    
           
169200 RICERCA-COSTO-ANAMAT.      
      *
           MOVE ANACST-C-MAT-COM TO ANACST-C-MAT.
           MOVE 0 TO ANACST-CST-COM
075340     PERFORM WITH TEST AFTER                                              
075350             UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                   
075360             PERFORM BEGIN-RC THRU                                        
075370                     BEGIN-RC-EX                                          
075380             IF SQLCODE = OK                                              
075390                     PERFORM SE-SELECT-ANAMAT-CST                             
075400                        THRU SE-SELECT-ANAMAT-CST-EX                          
075410             END-IF                                                       
075420     END-PERFORM.                                                         
075430     IF SQLCODE = OK   

             IF B2C-NO-DT
               MOVE ANACST-CST-STD-2 TO ANACST-CST-COM
             ELSE
               MOVE ANACST-CST-STD TO ANACST-CST-COM
             END-IF
             
           END-IF.
      *
075470     PERFORM S-S-COMMIT THRU S-S-COMMIT-EX. 
       EX-RICERCA-COSTO-ANAMAT.
           EXIT.

       SE-SELECT-ANAMAT-CST.
075540     EXEC SQL                                                             
075550          SELECT CST_STD, CST_STD_2                                                      
075570          INTO :ANACST-CST-STD, :ANACST-CST-STD-2                                          
075580          FROM ANAMAT_CST                                               
075590          WHERE C_MAT   = :ANACST-C-MAT                                
075610     END-EXEC                                           
      *
075620     MOVE "SELECT ANAMAT_CST   " TO ER-DESCRIZIONE                                                
075640     PERFORM TEST-ERR THRU TEST-ERR-EX.  
       SE-SELECT-ANAMAT-CST-EX.
           EXIT.
           

      *ASOLOB2C*                                                        fine
      *

      *UNICODDT*                                                        INIZIO
      *================================================================*
       INIT-PAR-TAB-UNICO-DDT.
      *================================================================*
          MOVE   16 TO QT-LL-ELEM       OF PAR-TAB-UNICO-DDT.
          MOVE    4 TO QT-LL-KEY        OF PAR-TAB-UNICO-DDT.
          MOVE    1 TO QT-ADDR-KEY      OF PAR-TAB-UNICO-DDT.
          MOVE 1980 TO QT-NUM-ELEM-MAX  OF PAR-TAB-UNICO-DDT.
          MOVE    0 TO QT-NUM-ELEM-EFF  OF PAR-TAB-UNICO-DDT.
          INITIALIZE TAB-UNICO-DDT.
       EX-INIT-PAR-TAB-UNICO-DDT. EXIT.
      *----------------------------------------------------------------*
       LEGGI-TAB-UNICO-DDT.
          MOVE "K2" TO QT-FUNZIONE OF PAR-TAB-UNICO-DDT.
          CANCEL "QTABELXL"
          CALL "QTABELXL" USING PAR-TAB-UNICO-DDT 
                              TAB-UNICO-DDT 
                              DEP-TAB-UNICO-DDT.
          IF QT-STATO OF PAR-TAB-UNICO-DDT = 0
              PERFORM PRENDI-DEP-TAB-UNICO-DDT THRU 
                   EX-PRENDI-DEP-TAB-UNICO-DDT
          ELSE
              INITIALIZE DATI-TAB-UNICO-DDT.
       EX-LEGGI-TAB-UNICO-DDT. EXIT.
      *----------------------------------------------------------------*
       PRENDI-DEP-TAB-UNICO-DDT.
          MOVE ELE-TAB-UNICO-DDT(QT-INDEX-ELEM OF PAR-TAB-UNICO-DDT) 
            TO DEP-TAB-UNICO-DDT.
       EX-PRENDI-DEP-TAB-UNICO-DDT. EXIT.
      *----------------------------------------------------------------*
       AGG-TAB-UNICO-DDT.
          IF QT-STATO OF PAR-TAB-UNICO-DDT = 0
              PERFORM RIMETTI-DEP-TAB-UNICO-DDT 
                 THRU EX-RIMETTI-DEP-TAB-UNICO-DDT
          ELSE
              PERFORM INS-TAB-UNICO-DDT THRU EX-INS-TAB-UNICO-DDT.
       EX-AGG-TAB-UNICO-DDT. EXIT.
      *----------------------------------------------------------------*
       RIMETTI-DEP-TAB-UNICO-DDT.
          MOVE DEP-TAB-UNICO-DDT 
            TO ELE-TAB-UNICO-DDT(QT-INDEX-ELEM OF PAR-TAB-UNICO-DDT).
       EX-RIMETTI-DEP-TAB-UNICO-DDT. EXIT.
      *----------------------------------------------------------------*
       INS-TAB-UNICO-DDT.
          MOVE "K1"   TO QT-FUNZIONE OF PAR-TAB-UNICO-DDT.
          CANCEL "QTABELXL"
          CALL "QTABELXL" USING PAR-TAB-UNICO-DDT 
                              TAB-UNICO-DDT 
                              DEP-TAB-UNICO-DDT.
                              
      *    DISPLAY QT-STATO OF PAR-TAB-UNICO-DDT.                        
          IF QT-STATO OF PAR-TAB-UNICO-DDT NOT = 0
          
              IF QT-STATO OF PAR-TAB-UNICO-DDT = -2
                  DISPLAY "AS=" TAB-AS " CL=" TAB-CL 
                  " GIA' INSERITI CON CAPI " TAB-MAX-CAPI
              ELSE
                  IF QT-STATO OF PAR-TAB-UNICO-DDT = -1
                      DISPLAY "TAB-UNICO-DDT PIENA >> ALLARGARE"
                  ELSE
                     MOVE QT-STATO OF PAR-TAB-UNICO-DDT 
                       TO ERR-DISP
                     DISPLAY "ERR k1 QTABELXL " ERR-DISP                           
                               " TAB-TAB-UNICO-DDT"                                          
                     CANCEL "QDBERROR"
                     CALL "QDBERROR" USING W-COMMON .                                 
       EX-INS-TAB-UNICO-DDT. EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       MOSTRA-TAB-UNICO-DDT.
      *    DISPLAY "TAB-UNICO-DDT".
          PERFORM M-DEP-TAB-UNICO-DDT THRU 
               EX-M-DEP-TAB-UNICO-DDT
               VARYING QT-INDEX-ELEM   OF PAR-TAB-UNICO-DDT
               FROM 1 BY 1
               UNTIL   QT-INDEX-ELEM   OF PAR-TAB-UNICO-DDT >
                       QT-NUM-ELEM-EFF OF PAR-TAB-UNICO-DDT.
          DISPLAY " ".
       EX-MOSTRA-TAB-UNICO-DDT. EXIT.
      *----------------------------------------------------------------*
       M-DEP-TAB-UNICO-DDT.
          PERFORM PRENDI-DEP-TAB-UNICO-DDT THRU 
               EX-PRENDI-DEP-TAB-UNICO-DDT.
          IF XD = "S" 
              DISPLAY DEP-TAB-UNICO-DDT
          ELSE     
              DISPLAY 
      *           "KEY: "
                  "AS = "  TAB-AS           OF DEP-TAB-UNICO-DDT "  "
                  "CL = "  TAB-CL           OF DEP-TAB-UNICO-DDT "  "
      *           "DATI: "
                  "MAX-CAPI = " TAB-MAX-CAPI OF DEP-TAB-UNICO-DDT "  "
      *            "CAPI-LETTI = " 
      *                   TAB-CAPI-LETTI OF DEP-TAB-UNICO-DDT " "
                  .
       EX-M-DEP-TAB-UNICO-DDT. EXIT.

       CICLO-DISIMPEGNO.
          PERFORM
               VARYING QT-INDEX-ELEM   OF PAR-TAB-UNICO-DDT
               FROM 1 BY 1
               UNTIL   QT-INDEX-ELEM   OF PAR-TAB-UNICO-DDT >
                       QT-NUM-ELEM-EFF OF PAR-TAB-UNICO-DDT
             PERFORM PRENDI-DEP-TAB-UNICO-DDT
                THRU EX-PRENDI-DEP-TAB-UNICO-DDT
             DISPLAY DEP-TAB-UNICO-DDT
             MOVE MAG-INPUT TO MAG-DISIMPEGNA
             MOVE SOCIETA-INPUT TO FORN-DISIMPEGNA
             MOVE TAB-AS TO AS-DISIMPEGNA(1)
             MOVE TAB-CL TO CLASSE-DISIMPEGNA(1)
             PERFORM CALL-DISIMPEGNA-MAG THRU EX-CALL-DISIMPEGNA-MAG
             IF NOT PY-OUTPUT-DISIMPEGNO-OK
                DISPLAY "ERRORE DISIMPEGNO!!!"
                STOP RUN
             END-IF
          END-PERFORM.
       EX-CICLO-DISIMPEGNO. EXIT.
      *----------------------------------------------------------------*
      *UNICODDT*                                                        FINE
