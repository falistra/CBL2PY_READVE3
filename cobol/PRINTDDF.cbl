001000*CONTROL USLINIT, SUBPROGRAM                                              
001100*SET X9 = ON                                                              
001200 IDENTIFICATION DIVISION.                                                 
001300 PROGRAM-ID. PRINTDDF.                                                    
001400*                                                                         
001500*  ricavato da PRINTDD3 con aggiunta di 2 righe di note                   
001600*  da stampare all'inizio del dettaglio della prima pag                   
001700*                                                                         
001800*                                                                         
001900*TERM*   17/11/98                                                         
002000*      W-NUM-TERM portato a 3 cifre                                       
002100*                                                                         
002200*CAMME*       24/05/99                                                    
002300*       gestisce cambio merce anche su mag 8                              
002400*                                                                         
002500*2000*                                                                    
002600*     tratta date a 6 cifre                                               
002700*                                                                         
002800*EURO*        19/12/00                          EURO/LIRE                 
002900*     trattamento importi in EURO                                         
003000*                                                                         
003100*VELOX*       02/05/01                                                    
003200*      Carica in memoria la prima volta che viene                         
003300*      eseguito tramite la gestione del F-PRIMA-VOLTA                     
003400*      la tavola ANAMAT.MODELLI                                           
003500*                                                                         
003600*EURO1*       29/11/01                                                    
003700*      trattamento prezzi di vendita in Euro                              
003800*                                                                         
003900*DTALLO*     02/10/02                                                     
004000*       stampa riga con allocazione                                       
004100*                                                                         
004200*  ATT.NE  : per RESIDUO occorre fare LOCK di SETTORE                     
004300*  ------    prima di chiamare PRINTDD3 con                               
004400*  ------           TIPO-MOVIMENTO-DDT = 25                               
004500*                                                                         
004600*                                                                         
004700*RECO*       04/03/04                                                     
004800*       inserita causale RECO                                             
004900*                                                                         
005000*NODE*    14/06/04                                                        
005100*      aggiunti 2 nuovi parametri da stampare nel dettaglio               
005200*      della prima riga                                                   
005300*                                                                         
005400*SELECT-MIRATA*    19/LUG/2005 - LAUROS                                   
005500*      Sostituito il caricamento di tutta la tabella                      
005600*      ANAMAT.MODELLI con SELECT mirate per ogni modello                  
005700*      (velocizza il programma)                                           
005800*                                                                         
005900*                                                                         
005901*CAUPSK-*  24/08/05                                                       
005902*        Se magazzino 2 e RESIDUO azzera pref V/F                         
005903*                                                                         
      *vettore-guida*   07/08/2006  -  LAUROS
      *       dicitura "il vettore e' tenuto a seguire le prescrizioni
      *       del codice della strada"....
      *
      *etich*            13/10/2006
      *       aggiunta Etichettatura se dovuta prima di riga
      *       capi di abbigliamento esterno .....
      *
      *ALLOC2*     16/12/2014
      *       sostituito tav. SETTORE con tav. ALLOCAZIONI
      *
      * 
      *ESTETA*     27/12/18
      *      estensione taglie
      *
      *************************************
      *NO-DATGE*        dicembre 2020 VALERIA 
      *     dismissione DATGE >> 
      *  (ANAMAT.COMPOS, ANAMAT.MOD_PEZZI, ANAMAT.MODELLI)
      *  sostituiti con ( composizioni_modello_dbg, 
      *                   anagrafica_modelli_dbg ,
      *                   anagrafica_modelli_barcode_negozio_dbg)
      *
      *  PF.CLASSI  SOSTITUITA CON anagrafica_classi_dbg
      *  PF.SOCIETA SOSTITUITA CON COPY MAPPASOCIETA
      *    
006000 ENVIRONMENT DIVISION.                                                    
006100 CONFIGURATION SECTION.                                                   
006200 SOURCE-COMPUTER.  HP-3000.                                               
006300 OBJECT-COMPUTER.  HP-3000.                                               
006400 SPECIAL-NAMES.                                      
006500                DECIMAL-POINT IS COMMA.                                   

      *ESTETA* 
       REPOSITORY.
            FUNCTION idxtg
            FUNCTION tgxid.                     

006600 INPUT-OUTPUT SECTION.                                                    
006700     FILE-CONTROL.                                                        
006800     SELECT FILE-PRINTDDT   ASSIGN TO "PRINTDDT"                        
              ORGANIZATION IS LINE SEQUENTIAL.
006900     SELECT FILE-DAFRPFAT   ASSIGN TO "DAFRPFAT".                         
007000*                                                                         
007100 DATA DIVISION.                                                           
007200*                                                                         
007300 FILE SECTION.                                                            
007400 FD FILE-PRINTDDT DATA RECORD REC-PRINTDDT.                               
007500 01 REC-PRINTDDT.                                                         
007600  05 TIPO-MOV-PR      PIC 99.                                             
007700  05 NOME-FILE-PR     PIC X(7).                                           
007800  05 NUM-TAB-PR       PIC 9.                                              
007900  05 DESCR-TAB-PR     PIC X(30).                                          
008000*                                                                         
008100 FD FILE-DAFRPFAT DATA RECORD REC-DAFRPFAT.                               
008200 01 REC-DAFRPFAT.                                                         
008300   05 C-MAT-F          PIC S9(15) COMP-3.                                 
008400   05 TIPO-REC-F        PIC 99.                                           
008500    88 REC-QTA   VALUE 0.                                                 
008600   05 RESTO2-F.                                                           
008700     10 D-CLASSE-PEZZO-F    PIC X(21).                                    
008800     10 D-TIPO-COMP-F    PIC X(8).                                        
008900     10 D-COMPOS-COMP-F  PIC X(30).                                       
009000   05 RESTO-F REDEFINES RESTO2-F.                                         
009100     10 NOME-MODELLO-F   PIC X(7).                                        
009200     10 DESC-CL-C-MAT-F  PIC X(21).                                       
009300     10 QTA-OTTO-F.                                                       
009400       15 QTA-TG-F       PIC S9(4) COMP COPY NTGOCCURS.                         
009500     10 PREZZO-F         PIC 9(9) COMP.                                   
009600     10 IVA-F            PIC 9(9) COMP.                                   
009700*                                                                         
009800 WORKING-STORAGE SECTION.                                                 
009900*                                                                         
010000 77 ERR-DISP               PIC -(6).                                      
010100*                                                                         
010200*                                                                         
010300*SELECT-MIRATA*                                                           
010400**VELOX*                                                                  
010500*77 I1                   PIC S9(8) COMP.                                  
010600*                                                                         
010700*                                                                         
010800*SELECT-MIRATA*                                                           
010900*01 F-PRIMA-VOLTA  PIC S9(4) COMP VALUE 0.                                
011000*  88 PRIMA-VOLTA VALUE 0.                                                
011100*                                                                         
011200*                                                                         
011300 01 COM-TIPO-REC-F         PIC 99.                                        
011400 01 COM-IVA-F            PIC 9(9) COMP.                                   
012700*
       COPY NTG.
      *                                                                         
011500*                                                                         
011600 01 AREA-REC-SET         PIC X(512).                                      
011700*                                                                         
011800 01 FILLER REDEFINES AREA-REC-SET.                                        
011900  05 REC-ANAMAT COPY YANAMAT.                                             
012000*                                                                         
012100 01 FILLER REDEFINES AREA-REC-SET.                                        
012200   05 REC-COMPOS  COPY YCOMPOS.                                           
012300*                                                                         
012400 01 FILLER REDEFINES AREA-REC-SET.                                        
012500   05 REC-ANAMATCL  COPY YANAMCL.                                         
012600*                                                                         
012700 01 FILLER REDEFINES AREA-REC-SET.                                        
012800   05 REC-ANAMAT2   COPY YANAMAT2.                                        
012900*                                                                         
013000 01 MOVMAG      COPY YMOVMAG.                                             
013100*                                                                         
013200*DTALLO*                                                                  
013300 01 SETTORE     COPY YSETTORE.                                            
013400*                                                                         
013500 01 CAMPI-UTILI.                                                          
013600  05 TOT-BOLLA-C              PIC S9(11) COMP-3.                          
013700  05 TOT-CAPI-RIGA-C              PIC S9(11) COMP-3.                      
013800  05 IND-PAG        PIC S9(4) COMP.                                       
013900  05 IND-RIGA        PIC S9(4) COMP.                                      
014000  05 C-MAT-MEM  PIC 9(15).                                                
014100  05 FINITO              PIC S9(4) COMP.                                  
014200     88 FINE-FILE     VALUE 1.                                            
014300*                                                                         
      *NO-DATGE*                                                        INIZIO
025500* 01 NUM-MAX-ELEM-C       PIC S9(9) COMP.                                  
025600* 01 NUM-MAX-ELEM-P       PIC S9(9) COMP.                                  
025700* 01 NUM-MAX-ELEM-M      PIC S9(9) COMP.                                   
      *NO-DATGE*                                                        fine
014700*                                                                         

      *NO-DATGE*                                                        INIZIO
       01 I PIC S9(4) COMP.
       01 J PIC S9(4) COMP.
       01 SW-PIU-PEZZI PIC 9.
          88 PIU-PEZZI VALUE 1.
          
       01 SW-IMAGE-SQL PIC X(5).
          88 VAI-CON-IMAGE VALUE "IMAGE".
          88 VAI-CON-SQL  VALUE "SQL".
      *NO-DATGE*                                                        fine



014800*SELECT-MIRATA*                                                           
014900*VELOX*                                                                   
015000*01 PARTAB-MODELLI COPY QPARTABX.                                         
015100*                                                                         
015200*01 EL-TAB-MODELLI.                                                       
015300* 05 EL-MODELLO-NEW       PIC X(15).                                      
015400* 05 EL-NOME              PIC X(8).                                       
015500* 05 EL-SOCIETA           PIC XX.                                         
015600* 05 EL-CLASSE            PIC XXX.                                        
015700* 05 EL-MODELLO-MAXIMA    PIC S9(15) COMP-3.                              
015800* 05 EL-NUM-PEZZI         PIC S9(9) COMP.                                 
015900* 05 EL-COD-IVA           PIC S9(9) COMP.                                 
016000*                                                                         
016100*                                                                         
016200*SQL                                                                      
016300*                                                                         
016400*SELECT-MIRATA*                                                           
      *NO-DATGE*                                                        INIZIO
016500* 01 SQLCODE-MEM           PIC S9(9) COMP.                                 
      *NO-DATGE*                                                        FINE
016600 77 FLAG-CURSORE          PIC S9(4) COMP.                                 
016700   88 STOP-CURSORE VALUE 1.                                               
      *vettore-guida*
       01 VETTORE-STRADA-STR.
         05 FILLER PIC X(52)
         VALUE "  IL VETTORE E' TENUTO A SEGUIRE LE PRESCRIZIONI DEL".
         05 FILLER PIC X(20)
         VALUE " CODICE DELLA STRADA".
      *
016800*                                                                         
016900*                                                                         
017000 EXEC SQL BEGIN DECLARE SECTION END-EXEC.                                 
017100*                                                                         
      *NO-DATGE*                                                        INIZIO
026300* EXEC SQL INCLUDE MODELLI.IF END-EXEC. 
026400* EXEC SQL INCLUDE MODPEZZI.IF END-EXEC. 
026500* EXEC SQL INCLUDE COMPOS.IF END-EXEC. 
026600* EXEC SQL INCLUDE CLASSI.IF END-EXEC. 
      *NO-DATGE*                                                        fine

017600*SELECT-MIRATA*                                                           
      *NO-DATGE*                                                        INIZIO
026600* EXEC SQL INCLUDE SOCIETA.IF END-EXEC.
      *NO-DATGE*                                                        FINE

      *NO-DATGE*                                                        INIZIO
        01 CC-CLASSE PIC XXX.
        01 CC-DESCRIZIONE PIC X(40).
        
        01 TAB-RIGHE-SOCIETA COPY MAPPASOCIETA.                                   
        01 CC-SOC           PIC XX.                 
        01 CC-SOCIETA       PIC XX.                 
        01 CC-C-MAT         PIC 9(15).
        01 CC-MEM-PROGR     PIC 99.
        01 CC-MODELLO-DT    PIC X(15).
        01 CC-NOME          PIC X(15).
        01 CC-IVA           PIC S9(4) COMP.
        
                
        01 CC-RIGHE-COMPOS.
           05 CC-RR OCCURS 20.
             10 CC-PROGR-PEZZO          PIC 99.
             10 CC-PROGR-COMPOSIZIONE   PIC 99.
             10 CC-CODICE-COMPOSIZIONE  PIC X(70).
             10 CC-CODICE-PEZZO         PIC 99.
             10 CC-TIPO-MATERIALE       PIC X(4).
             10 CC-DESC-CLASSE          PIC X(70).
             10 CC-TESTO-COMPOSIZIONE   PIC X(700).
      *NO-DATGE*                                                        FINE


017800*                                                                         
017900*SELECT-MIRATA*                                                           
018000*01 TAB-MODELLI.                                                          
018100* 05 REC-RIGHE-MODELLI OCCURS 300000.                                     
018200*  10 TAB-M-MODELLO-NEW               PIC X(15).                          
018300*  10 TAB-M-NOME                      PIC X(8).                           
018400*  10 TAB-M-SOCIETA                   PIC XX.                             
018500*  10 TAB-M-CLASSE                    PIC XXX.                            
018600*  10 TAB-M-MODELLO-MAXIMA            PIC S9(15) COMP-3.                  
018700*  10 TAB-M-NUM-PEZZI                 PIC S9(9) COMP.                     
018800**VELOX*                                                                  
018900**  10 TAB-M-ESTENSIONE                PIC XXX.                           
019000*  10 TAB-M-COD-IVA                   PIC S9(9) COMP.                     
019100*                                                                         
019200*                                                                         
      *NO-DATGE*                                                        INIZIO
019300* 01 TAB-ELEM-PEZZI.                                                       
019400*  05 REC-RIGHE-PEZZI   OCCURS 20.                                         
019500*   10 TAB-MOD-PEZZO      PIC S9(9) COMP.                                  
019600*   10 TAB-COD-PEZZO      PIC X(8).                                        
019700**                                                                         
019800* 01 TAB-ELEM-COMPOS.                                                      
019900*  05 REC-RIGHE-COMPOS  OCCURS 50.                                         
020000*   10 TAB-PEZZO          PIC S9(9) COMP.                                  
020100*   10 TAB-PROGR          PIC S9(9) COMP.                                  
020200*   10 TAB-TIPO-MAT       PIC X.                                           
020300*   10 TAB-COMPOSIZIONE   PIC X(30).                                       
020400* 
      *NO-DATGE*                                                        FINE
      *alloc2* 
      * 
       01 ALLO-ENTITA  PIC X(20).
       01 ALLO-MAG     PIC 9(3). 
      *       
       01 TAB-REG-ALLO.
         05 REC-RIGHE-ALLO   OCCURS 1. 
           10 REC-ALLO-ENTITA   PIC X(20).
           10 REC-ALLO-CMATSET  PIC X(12).
           10 REC-ALLO-MAG      PIC 999.
           10 REC-ALLO-TIPO     PIC X.
           10 REC-ALLO-ENTITA-R PIC S9(15) COMP-3.
           10 REC-ALLO-TSNAS    PIC X(19).
           10 REC-ALLO-TSVAR    PIC X(19).
      *alloc2                                                                         
020500 EXEC SQL END DECLARE SECTION END-EXEC.     
      *
      *alloc2* 
       01 SQLCODE-MEM-2 PIC S9(4).
      *alloc2*                              
020600*                                                                         
020700 01 SQL-CONST             COPY SQLCONST .                       
020800 01 PAR-ERR               COPY PARERR .                         
020900 01 AREA-HL               COPY AREAHL .                         
021000 01 AREA-SI               COPY AREASI .                         
021100*                                                                         
021200*SQL                                                                      
021300*                                                                         
021400 01 COMANDO-FILE.                                                         
021500   05 FILLER PIC X(39) VALUE                                              
021600      "FILE PRINTDDT=PRINTDDT.TABELLE.TRANSFER".                          
021700     05 CARRIAGE-RETURN PIC X VALUE X"13".                                  
021800 01 ERR   PIC S9999  COMP VALUE 0.                                        
021900 01 ERR-PARM  PIC S9999 COMP VALUE 0.                                     
022000*                                                                         
022100 01 DATA-BOLLA-COM        PIC 9(6).                                       
022200 01 DATA-B-COM REDEFINES DATA-BOLLA-COM.                                  
022300  05 AA-COM           PIC 99.                                             
022400  05 MM-COM           PIC 99.                                             
022500  05 GG-COM           PIC 99.                                             
022600*                                                                         
022700 01 DATA-BOLLA-COM-R          PIC 9(6).                                   
022800 01 DATA-B-COM-R REDEFINES DATA-BOLLA-COM-R.                              
022900  05 GG-COM           PIC 99.                                             
023000  05 MM-COM           PIC 99.                                             
023100  05 AA-COM           PIC 99.                                             
023200*                                                                         
023300 01 RIF-INTR.                                                             
023400   05 AA-MM-GG PIC 9(6).                                                  
023500   05 NUMERO PIC 9(6).                                                    
023600 01 RIF-INTR-RID REDEFINES RIF-INTR    PIC 9(12).                         
023700*                                                                         
023800*                                                                         
023900 01 CONTO-COM.                                                            
024000  05 CAP-B          PIC 999.                                              
024100  05 SOT-B          PIC 9(5).                                             
024200 01 CONTO-COM-RID REDEFINES CONTO-COM     PIC 9(8).                       
024300*                                                                         
024400 01 PROGR-ART-COM   PIC 999.                                              
024500 01 FILLER REDEFINES PROGR-ART-COM.                                       
024600   05 PRE-PROGR-ART-COM    PIC 99.                                        
024700   05 FORN-PROGR-ART-COM   PIC 9.                                         
024800*                                                                         
024900 01 RIGA-IMPORTO.                                                         
025000   05 FILLER    PIC X(9) VALUE SPACES.                                    
025100   05 FILLER    PIC XXX VALUE "L. ".                                      
025200   05 IMPORTO-GEN-R                                                       
025300                PIC ZZZ.ZZZ.ZZZ.                                          
025400   05 FILLER    PIC X(15) VALUE                                           
025500     " (IVA compresa)".                                                   
025600*                                                                         
025700*EURO1*                                                                   
025800*                                                                         
025900 01 RIGA-IMPORTO-EUR.                                                     
026000   05 FILLER    PIC X(9) VALUE SPACES.                                    
026100   05 FILLER    PIC X(4) VALUE "Eur.".                                    
026200   05 IMPORTO-GEN-R                                                       
026300                PIC ZZZ.ZZZ,ZZ.                                           
026400   05 FILLER    PIC X(15) VALUE                                           
026500     " (IVA compresa)".                                                   
026600*                                                                         
026700 01 C-MAT-COM COPY DANCODMT.                                              
026800*                                                                         
026900 01 ZONA-COMPOSIZIONI        PIC X(24).                                   
027000 01 FILLER REDEFINES ZONA-COMPOSIZIONI.                                   
027100  05 DATI-COMP-TESSUTO.                                                   
027200   10 DATI-COMP-T  OCCURS 6.                                              
027300    15 PERC-T    PIC ZZ.                                                  
027400    15 SIGLA-T   PIC XX.                                                  
027500  05 FILLER REDEFINES DATI-COMP-TESSUTO.                                  
027600   10 PERC-T-R   PIC ZZZ.                                                 
027700   10 SIGLA-T-R  PIC XX.                                                  
027800*                                                                         
027900 01 ZONA-COMPOSIZIONI-F          PIC X(8).                                
028000 01 FILLER REDEFINES ZONA-COMPOSIZIONI-F.                                 
028100  05 DATI-COMP-FODERA.                                                    
028200   10 DATI-COMP-F  OCCURS 2.                                              
028300    15 PERC-F    PIC ZZ.                                                  
028400    15 SIGLA-F   PIC XX.                                                  
028500  05 FILLER REDEFINES DATI-COMP-FODERA.                                   
028600   10 PERC-F-R   PIC ZZZ.                                                 
028700   10 SIGLA-F-R  PIC XX.                                                  
028800*                                                                         
028900*                                                                         
029000 01  TRACCIATI-TABULATI.                                                  
029100  03  PAR-PRINT.                                                          
029200   05  STATO              PIC S9(4) COMP.                                 
029300   05  LL-RIGA            PIC S9(4) COMP.                                 
029400   05  N-MAX-RIGHE        PIC S9(4) COMP.                                 
029500   05  FILLER             PIC X(4).                                       
029600   05  NOME-FILE          PIC X(12).                                      
029700   05 FILLER REDEFINES NOME-FILE.                                         
029800*TERM*                                                                    
029900*CAMME*                                                                   
030000*   10 NOME-FILE-1        PIC X(4).                                       
030100    10 NOME-FILE-1.                                                       
030200      15 FILLER  PIC XX.                                                  
030300      15 NOME-FILE-2 PIC 9.                                               
030400      15 FILLER  PIC X.                                                   
030500*                                                                         
030600    10 NUM-TERM-FILE      PIC 999.                                        
030700    10 RESTO-FILE         PIC X(5).                                       
030800*                                                                         
030900  03  RIGA-STAMPA.                                                        
031000   05  N-STAMPANTE         PIC 9.                                         
031100   05  COMANDO             PIC X.                                         
031200   05  N-RIGA-STAMPA       PIC S9(4) COMP.                                
031300   05  DATI-RIGA           PIC X(132).                                    
031400   05  TABULATO-2 REDEFINES DATI-RIGA.                                    
031500    10 CAMPI-TRACCIATI      COPY DATIDDT.                                 
031600*                                                                         
031700  03 BUFFER.                                                              
031800   05 N-BUF               PIC S9(4) COMP VALUE 37.                        
031900   05 FILLER              PIC XX.                                         
032000   05 FILLER              PIC X(5120).                                    
032100*                                                                         
032200 01 RIGA-STAMPA-MEM         PIC X(132).                                   
032300*                                                                         
032400 01 TIPO-DOC-DDT-M        PIC S9(4) COMP.                                 
032500     88 DOC-DDT      VALUE 1.                                             
032600     88 DOC-NOT-DDT  VALUE 2.                                             
032700 01 TIPO-MOVIMENTO-DDT-M  PIC S9(4) COMP.                                 
032800     88 VENDITA       VALUE 1.                                            
032900     88 TRASFERIMENTO VALUE 2.                                            
033000     88 C-VISIONE     VALUE 3.                                            
033100     88 C-LAVAGGIO    VALUE 4.                                            
033200     88 C-LAVORAZIONE VALUE 5.                                            
033300     88 PREBOLLA      VALUE 6.                                            
033400     88 RESO-F        VALUE 7.                                            
033500     88 C-RIPARAZIONE VALUE 8.                                            
033600     88 VENDITA-DA-TERZI VALUE 9.                                         
033700     88 VENDITA-MAX-E-CO VALUE 10.                                        
033800     88 VENDITA-CONFEZIONI VALUE 11.                                      
033900     88 TRASFERIMENTO-CONFEZIONI VALUE 22.                                
034000     88 RESO-MAX-E-CO  VALUE 77.                                          
034100     88 VENDITA-CONTR-EST   VALUE 12.                                     
034200*DTALLO*                                                                  
034300     88 TRASFERIMENTO-DT  VALUE 25.                                       
034400 01 TIPO-STAMPA-DDT-M     PIC S9(4) COMP.                                 
034500     88 PRODOTTI-FINITI   VALUE 1.                                        
034600     88 MATERIE-PRIME     VALUE 9.                                        
034700*                                                                         
034800*                                                                         
034900*EURO*                                                                    
035000 01 PAR-EURO COPY QPAREURO.                                               
035100*                                                                         
035200 LINKAGE SECTION.                                                         
035300*                                                                         
035400 01 W-COMMON COPY WCOMMONW.                                               
035500*                                                                         
035600 EXEC SQL INCLUDE SQLCA END-EXEC.                                         
035700*                                                                         
035800 01 CAMPI-ANAGRAFICI.                                                     
035900  05  INDIRIZZO-STD         PIC X(66).                                    
036000  05  INDIRIZZO-COM         PIC X(60).                                    
036100  05  LOCALITA-COM          PIC X(60).                                    
036200  05  CAP-COM               PIC S9(5) COMP-3.                             
036300  05  PROV-COM              PIC XX.                                       
036400  05  STATO-COM             PIC XXX.                                      
036500*                                                                         
036600  05  INDIRIZZO-C-COM         PIC X(60).                                  
036700  05  LOCALITA-C-COM          PIC X(60).                                  
036800  05  CAP-C-COM               PIC S9(5) COMP-3.                           
036900  05  PROV-C-COM              PIC XX.                                     
037000*                                                                         
037100  05 D-CONTO-MEM     PIC X(24).                                           
037200  05 D-CONTO-AGG-MEM  PIC X(24).                                          
037300  05 D-CONTO-VET     PIC X(24).                                           
037400*                                                                         
037500  05  INDIRIZZO-C-VET         PIC X(60).                                  
037600  05  LOCALITA-C-VET          PIC X(60).                                  
037700  05  CAP-C-VET               PIC S9(5) COMP-3.                           
037800  05  PROV-C-VET              PIC XX.                                     
037900*                                                                         
038000 01 CAMPI-COMODO.                                                         
038100  05 RIF-BOLLA-DDT         PIC 9(12).                                     
038200  05 FILLER REDEFINES RIF-BOLLA-DDT.                                      
038300   10 AA-MM-GG-DDT       PIC 9(6).                                        
038400   10 NUMERO-DDT         PIC 9(6).                                        
038500  05 CLIENTE-DDT           PIC S9(9) COMP.                                
038600  05 MAGAZZINO-DDT         PIC S9(4) COMP.                                
038700  05 CAUSALE-DDT           PIC X(4).                                      
038800  05 TIPO-DOC-DDT          PIC S9(4) COMP.                                
038900  05 TIPO-MOVIMENTO-DDT    PIC S9(4) COMP.                                
039000  05 TIPO-STAMPA-DDT       PIC S9(4) COMP.                                
039100  05 LOC-PART-DDT          PIC X(56).                                     
039200  05 NOTE-DDT              PIC X(44) OCCURS 2.                            
039300*EURO*                                                                    
039400  05 DIVISA-EUR            PIC X(4).                                      
039500*                                                                         
039600*                                                                         
039700 01 TIPO-DATA-SET-DDT   PIC X.                                            
039800  88 MOVMAG-DDT    VALUE "0", " ".                                        
039900  88 MOVTRANS-DDT  VALUE "1".                                             
040000*                                                                         
040100 01 IMPORTO-X-PL         PIC S9(11) COMP-3.                               
040200*                                                                         
040300 01 FILE-FAT-DDT PIC X.                                                   
040400  88 SI-FILE-FAT VALUE "S".                                               
040500*                                                                         
040600*NODE*                                                                    
040700 01 RIGA-1-DDT   PIC X(65).                                               
040800 01 RIGA-2-DDT   PIC X(65).                                               
040900*                                                                         
041000 PROCEDURE DIVISION USING W-COMMON SQLCA                                  
041100             CAMPI-ANAGRAFICI CAMPI-COMODO                                
041200             TIPO-DATA-SET-DDT IMPORTO-X-PL FILE-FAT-DDT                  
041300*NODE*                                                                    
041400             RIGA-1-DDT  RIGA-2-DDT.                                      
041500*                                                                         
041600 INIZIO.                                                                  
041700*                                                      
      *NO-DATGE*                                                        inizio
           PERFORM S-SET-1 THRU S-SET-1-EX.  
      *NO-DATGE*                                                        FINE
041800     MOVE 0 TO FINITO W-INDICE-1 TOT-BOLLA-C.                             
041900     MOVE TIPO-DOC-DDT TO TIPO-DOC-DDT-M.                                 
042000     MOVE TIPO-MOVIMENTO-DDT TO TIPO-MOVIMENTO-DDT-M.                     
042100     MOVE TIPO-STAMPA-DDT TO TIPO-STAMPA-DDT-M.                           
           CANCEL "COMMAND2"
042200     CALL "COMMAND2" USING COMANDO-FILE,                         
042300                                  ERR, ERR-PARM.                          
042400     IF ERR NOT = 0                                                       
042500        MOVE ERR TO ERR-DISP                                              
042600        DISPLAY "Errore COMMAND file PRINTDDT.TABELLE "                   
042700             SPACE SPACE ERR-DISP                                         
042800        STOP RUN.                                                         
042900     OPEN INPUT FILE-PRINTDDT.                                            
043000     PERFORM LEGGI-FILE THRU EX-LEGGI-FILE.                               
043100     PERFORM LEGGI-FILE THRU EX-LEGGI-FILE                                
043200             UNTIL FINE-FILE OR                                           
043300                TIPO-MOVIMENTO-DDT-M =                                    
043400                     TIPO-MOV-PR.                                         
043500     CLOSE FILE-PRINTDDT.                                                 
043600     IF FINE-FILE                                                         
043700        DISPLAY "----- Err. RICERCA in PRINTDDT.TABELLE"                  
043800        DISPLAY "----- per TIPO-MOV  " TIPO-MOVIMENTO-DDT-M               
043900        STOP RUN.                                                         
044000*                                                                         
044100*SELECT-MIRATA*                                                           
044200**VELOX*                                                                  
044300*    IF PRIMA-VOLTA                                                       
044400*      PERFORM CARICA-MODELLI THRU EX-CARICA-MODELLI.                     
044500*                                                                         
044600*                                                                         
044700     IF SI-FILE-FAT                                                       
044800       OPEN OUTPUT FILE-DAFRPFAT.                                         
044900*                                                                         
045000     PERFORM APRI-STAMPA THRU EX-APRI-STAMPA.                       
045100     PERFORM TRATTA-NEG THRU EX-TRATTA-NEG.                               
045200     CALL "QCLPPR" USING PAR-PRINT RIGA-STAMPA BUFFER.                    
045300*                                                                         
045400     IF SI-FILE-FAT                                                       
045500       CLOSE FILE-DAFRPFAT.                                               
045600*                                                                         
045700 FINE.                                                                    
045800     EXIT PROGRAM.                                                        
045900*                                                                         
046000*                                                                         
046100*                                                                         
046200 LEGGI-FILE.                                                              
046300     READ FILE-PRINTDDT AT END MOVE 1 TO FINITO.                          
046400 EX-LEGGI-FILE.                                                           
046500     EXIT.                                                                
046600*                                                                         
046700*SELECT-MIRATA*                                                           
046800**VELOX*                                                                  
046900*CARICA-MODELLI.                                                          
047000*    MOVE "000" TO MODELLI-ESTENSIONE.                                    
047100*    PERFORM WITH TEST AFTER                                              
047200*            UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                   
047300*       PERFORM BEGIN-RC THRU BEGIN-RC-EX                                 
047400*       IF SQLCODE = OK                                                   
047500*          PERFORM SE-SELECT-MODELLI                                      
047600*                 THRU SE-SELECT-MODELLI-EX                               
047700*       END-IF                                                            
047800*    END-PERFORM.                                                         
047900*    MOVE SQLERRD(3) TO QT-NUM-ELEM-EFF OF PARTAB-MODELLI.                
048000**DTALLO*                                                                 
048100**    DISPLAY "Num. Elem   " QT-NUM-ELEM-EFF                              
048200**      OF PARTAB-MODELLI.                                                
048300*    PERFORM COMMIT THRU COMMIT-EX.                                       
048400*    MOVE 300000 TO QT-NUM-ELEM-MAX OF PARTAB-MODELLI.                    
048500*    MOVE 44 TO QT-LL-ELEM OF PARTAB-MODELLI.                             
048600*    MOVE 29 TO QT-ADDR-KEY OF PARTAB-MODELLI.                            
048700*    MOVE 8 TO QT-LL-KEY OF PARTAB-MODELLI.                               
048800*    MOVE 1 TO F-PRIMA-VOLTA.                                             
048900*EX-CARICA-MODELLI.                                                       
049000*    EXIT.                                                                
049100*                                                                         
049200*                                                                         
049300 APRI-STAMPA.                                                             
049400     MOVE 136 TO LL-RIGA OF PAR-PRINT.                                    
049500     MOVE 5000 TO N-MAX-RIGHE OF PAR-PRINT.                               
049600     MOVE NOME-FILE-PR TO NOME-FILE-1.                                    
049700*CAMME*                                                                   
049800     IF MAGAZZINO-DDT = 7                                                 
049900       MOVE 7 TO NOME-FILE-2                                              
050000     ELSE                                                                 
050100       IF MAGAZZINO-DDT = 8
      *anche3*
050200*        MOVE 8 TO NOME-FILE-2.
               MOVE 8 TO NOME-FILE-2
             ELSE
               IF MAGAZZINO-DDT = 4
                 MOVE 4 TO NOME-FILE-2
               END-IF
             END-IF
           END-IF
      *
050300*                                                                         
050400     MOVE W-NUM-TERM TO NUM-TERM-FILE.                                    
050500     IF MAGAZZINO-DDT = 2                                                 
050600        MOVE 2 TO NUM-TERM-FILE.                                          
050700     IF CLIENTE-DDT = 10000007                                            
050800        MOVE SPACES TO NOTE-DDT (1) NOTE-DDT (2)                          
050900        IF NOT TRASFERIMENTO-DT                                           
051000          MOVE 7 TO NUM-TERM-FILE.                                        
051100     MOVE ".ST" TO RESTO-FILE.                                            
051200     CALL "QOLPPR" USING PAR-PRINT                                        
051300                          RIGA-STAMPA BUFFER.                             
051400     IF STATO OF PAR-PRINT NOT = 0                                        
051500       DISPLAY "APRI-STAMPA " NOME-FILE                                   
051600                           " err " STATO OF PAR-PRINT                     
051700       GOBACK.                                                          
051800*                                                                         
051900     MOVE NUM-TAB-PR TO N-STAMPANTE OF RIGA-STAMPA                        
052000     MOVE "M" TO COMANDO OF RIGA-STAMPA                                   
052100     MOVE DESCR-TAB-PR TO DATI-RIGA.                                      
052200     MOVE 66 TO N-RIGA-STAMPA OF RIGA-STAMPA                              
052300     CALL "QWLPPR" USING PAR-PRINT                                        
052400                          RIGA-STAMPA BUFFER                              
052500     MOVE SPACES TO DATI-RIGA.                                            
052600 EX-APRI-STAMPA.                                                          
052700     EXIT.                                                                
052800*                                                                         
052900*                                                                         
053000 CALL-QWPRINT-1.                                                          
053100     CALL "QWLPPR" USING PAR-PRINT                                        
053200                          RIGA-STAMPA BUFFER.                             
053300     IF STATO OF PAR-PRINT NOT = 0                                        
053400        MOVE STATO OF PAR-PRINT TO ERR-DISP                               
053500        DISPLAY "FROM=> PRINTDDT  "                                       
053600        DISPLAY "ERR TABULATO 0" ERR-DISP                                 
053700        DISPLAY "COMANDO => " COMANDO                                     
           CANCEL "QDBERROR"
053800        CALL "QDBERROR" USING W-COMMON.                                   
053900     MOVE SPACES TO DATI-RIGA.                                            
054000     MOVE "S" TO COMANDO.                                                 
054100     MOVE 0 TO N-RIGA-STAMPA.                                             
054200 EX-CALL-QWPRINT-1.                                                       
054300     EXIT.                                                                
054400*                                                                         
054500*                                                                         
054600 CALL-QWPRINT-2.                                                          
054700     IF IND-RIGA > 34                                                     
054800        MOVE DATI-RIGA TO RIGA-STAMPA-MEM                                 
054900        ADD 1 TO IND-PAG                                                  
055000        PERFORM STAMPA-TESTATA THRU EX-STAMPA-TESTATA                     
055100        MOVE RIGA-STAMPA-MEM TO DATI-RIGA.                                
055200     CALL "QWLPPR" USING PAR-PRINT                                        
055300                          RIGA-STAMPA BUFFER.                             
055400     IF STATO OF PAR-PRINT NOT = 0                                        
055500        MOVE STATO OF PAR-PRINT TO ERR-DISP                               
055600        DISPLAY "FROM=> PRINTDDT  "                                       
055700        DISPLAY "ERR TABULATO 0" ERR-DISP                                 
055800        DISPLAY "COMANDO => " COMANDO                                     
           CANCEL "QDBERROR"
055900        CALL "QDBERROR" USING W-COMMON.                                   
056000     MOVE SPACES TO DATI-RIGA.                                            
056100     MOVE "S" TO COMANDO.                                                 
056200     MOVE 0 TO N-RIGA-STAMPA.                                             
056300 EX-CALL-QWPRINT-2.                                                       
056400     EXIT.                                                                
056500*                                                                         
056600*                                                                         
056700*                                                                         
056800*                                                                         
056900 METTI-TOT.                                                               
057000     MOVE 53 TO N-RIGA-STAMPA.                                            
057100     MOVE "P" TO COMANDO.                                                 
057200     IF PRODOTTI-FINITI                                                   
057300        MOVE TOT-BOLLA-C TO TOT-CAPI-RIGA                                 
057400       ELSE                                                               
057500          COMPUTE TOT-METRI-RIGA =                                        
057600                  TOT-BOLLA-C / 100.                                      
057700     PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                       
057800     MOVE LOC-PART-DDT TO LOC-PART.                                       
057900     PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                       
058000     MOVE 56 TO N-RIGA-STAMPA.                                            
058100     MOVE "P" TO COMANDO.                                                 
058200     IF D-CONTO-VET NOT = SPACES                                          
058300        MOVE D-CONTO-VET TO RAG-SOC-VETTORE                               
058400        MOVE INDIRIZZO-C-VET TO INDIRIZZO-VETTORE                         
058500        PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1                     
058600        STRING LOCALITA-C-VET (1:30) "  " PROV-C-VET                      
058700               DELIMITED BY SIZE INTO INDIRIZZO-VETTORE                   
058800        PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                    
058900     MOVE 60 TO N-RIGA-STAMPA.                                            
059000     MOVE "P" TO COMANDO.                                                 
059100     IF NOTE-DDT (1) NOT = SPACES AND                                     
059200           NOTE-DDT (2) NOT = SPACES                                      
059300        MOVE NOTE-DDT (1) TO ANNOTAZ-RIGA                                 
059400        PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1                     
059500        MOVE NOTE-DDT (2) TO ANNOTAZ-RIGA                                 
059600        PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                    
059700 EX-METTI-TOT.                                                            
059800     EXIT.                                                                
059900*                                                                         
060000*                                                                         
060100 TRATTA-NEG.                                                              
060200     MOVE 100 TO IND-RIGA.                                                
060300     MOVE 0 TO IND-PAG.                                                   
060400     IF MOVTRANS-DDT                                                      
060500       MOVE "MOVTRANS" TO W-NOME-DATA-SET                                 
060600     ELSE                                                                 
060700       MOVE "MOVMAG;" TO W-NOME-DATA-SET.                                 
060800     MOVE "RIF-INTR;" TO W-NOME-CAMPO.                                    
060900     MOVE RIF-BOLLA-DDT TO W-VALORE-CAMPO                                 
061000                           RIF-INTR-RID.                                  
061100     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
061200     IF W-OK-IMAGE                                                        
061300        MOVE 5 TO W-MODO                                                  
061400        PERFORM TTDBGET THRU EX-TTDBGET                                   
061500        PERFORM STAMPA-DETT THRU EX-STAMPA-DETT                           
061600                  UNTIL W-FINE-CATENA                                     
061700        IF (W-SIGLA-UTENTE = "RESIDUO" OR = "PROROSA")                    
061800               AND MAGAZZINO-DDT = 8 AND                                  
061900                  IMPORTO-X-PL NOT = 0                                    
062000          IF W-FORMATO-INTERNO NOT > 011231                               
062100            COMPUTE IMPORTO-GEN-R OF RIGA-IMPORTO =                       
062200                   IMPORTO-X-PL / 100                                     
062300            MOVE RIGA-IMPORTO TO DATI-RIGA                                
062400          ELSE                                                            
062500            COMPUTE IMPORTO-GEN-R OF RIGA-IMPORTO-EUR =                   
062600                   IMPORTO-X-PL / 100                                     
062700            MOVE RIGA-IMPORTO-EUR TO DATI-RIGA                            
062800          END-IF                                                          
062900*                                                                         
063000          PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2                   
063100          ADD 1 TO IND-RIGA                                               
063200        END-IF                                                            
      *vettore-guida*
              MOVE SPACES TO DATI-RIGA
              PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2
              ADD 1 TO IND-RIGA
              MOVE VETTORE-STRADA-STR TO DATI-RIGA
              PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2
              ADD 1 TO IND-RIGA
063300        PERFORM METTI-TOT THRU EX-METTI-TOT.                              
063400 EX-TRATTA-NEG.                                                           
063500     EXIT.                                                                
063600*                                                                         
063700*                                                                         
063800 STAMPA-DETT.                                                             
063900     MOVE AREA-REC-SET TO MOVMAG.                                         
064000*EURO*                                                                    
064100     COPY PEURO REPLACING AREA-REC-SET BY MOVMAG.                         
064200     MOVE EU-DIVISA-CORR TO DIVISA-EUR.                                   
064300*                                                                         
064400     IF CLIENTE-DDT = CONTO OF MOVMAG                                     
064500             AND MAGAZZINO-DDT = MAGAZZINO OF MOVMAG                      
064600                AND CAUSALE-DDT = C-OPE OF MOVMAG                         
064700        MOVE C-MAT OF MOVMAG TO C-MAT-TRANS-RID                           
064701*CAUPSK-*                                                                 
064800*       IF W-SIGLA-UTENTE = "RESIDUO" OR = "PROROSA"                      
064900*                   AND MAGAZZINO-DDT = 8                                 
064901        IF (W-SIGLA-UTENTE = "RESIDUO" OR = "PROROSA")                    
064902               AND (MAGAZZINO-DDT = 8 OR = 2)                             
064903*                                                                         
065000           MOVE PROGR-ART OF C-MAT-TRANSITO TO PROGR-ART-COM              
065100           MOVE 0 TO PRE-PROGR-ART-COM                                    
065200           MOVE PROGR-ART-COM TO PROGR-ART OF C-MAT-TRANSITO              
065300        END-IF                                                            

      *NO-DATGE*                                                        INIZIO
065400*        COMPUTE MODELLI-MODELLO-MAXIMA =                                  
065500*                      C-MAT-TRANS-RID / 1000 * 1000                       
      *NO-DATGE*                                                        fine

065600        MOVE C-MAT OF MOVMAG TO C-MAT-TRANS-RID   
                      
065700*VELOX*                                                                   
065800        PERFORM CERCA-MODELLO THRU EX-CERCA-MODELLO                       

065900*SELECT-MIRATA*                                                           
066000*       IF QT-STATO OF PARTAB-MODELLI NOT = 0                             

      *NO-DATGE*                                                        INIZIO
066100*        IF SQLCODE-MEM NOT = OK                                           
              IF VAI-CON-IMAGE         
      *NO-DATGE*                                                        FINE
      *          display "entro in tratta-image "  C-MAT-TRANS-RID                                                                          
066300           PERFORM TRATTA-IMAGE THRU EX-TRATTA-IMAGE                      
066400          ELSE                                                            
066500*            DISPLAY "Entro in Tratta-sql " C-MAT-TRANS-RID        ALLE   
066600*            MOVE QT-INDEX-ELEM OF PARTAB-MODELLI                         
066700*              TO I1                                                      
066800             PERFORM TRATTA-SQL THRU EX-TRATTA-SQL                        
066900*DTALLO*                                                                  
067000        END-IF                                                            
067100*                                                                       
067200        IF (W-SIGLA-UTENTE = "RESIDUO" OR = "PROROSA")                    
067300                       AND TRASFERIMENTO-DT                               
067400           PERFORM STAMPA-ALLOC THRU EX-STAMPA-ALLOC                      
067500        ELSE                                                              
067600*                                                                         
067700        IF RESO-F OR RESO-MAX-E-CO                                        
067800          PERFORM STAMPA-RIF-RESO THRU EX-STAMPA-RIF-RESO.                
067900*                                                                         
068000     IF MOVTRANS-DDT                                                      
068100       MOVE "MOVTRANS" TO W-NOME-DATA-SET                                 
068200     ELSE                                                                 
068300       MOVE "MOVMAG;" TO W-NOME-DATA-SET.                                 
068400     MOVE 5 TO W-MODO.                                                    
068500     PERFORM TTDBGET THRU EX-TTDBGET.                                     
068600 EX-STAMPA-DETT.                                                          
068700     EXIT.                                                                


      *NO-DATGE*                                                        inizio
       LEGGI-COMPOSIZIONI-DBG.
      *      DISPLAY "LEGGI-COMPOSIZIONI".
           MOVE SPACES TO CC-RIGHE-COMPOS.
           IF MODELLO OF C-MAT-TRANSITO NOT = 0                                 
              COMPUTE CC-C-MAT =                                  
                            C-MAT-TRANS-RID / 1000 * 1000                       
               PERFORM VUOTO THRU EX-VUOTO
                    VARYING I FROM 1 BY 1
                        UNTIL I > NUMERO-MAX-SOCIETA OR
                            SOCIETA-MOD OF C-MAT-TRANSITO =
                              SOCIETA-CODICE (I)
                MOVE SOCIETA-SIGLA (I) TO CC-SOC                
      *          DISPLAY "CC-C-MAT = " CC-C-MAT
      *          DISPLAY "CC-SOC = " CC-SOC
                MOVE SPACES TO CC-RIGHE-COMPOS
                
                PERFORM SELECT-COMPOSIZIONI THRU 
                     EX-SELECT-COMPOSIZIONI
                     
      *          DISPLAY "SQLCODE " SQLCODE
                PERFORM TEST-ERR THRU TEST-ERR-EX
      *          IF SQLCODE = OK        DISPLAY "OK" END-IF
      *          IF SQLCODE = NO-MEMORY DISPLAY "NO_MEMORY" END-IF
      *          IF SQLCODE = DEADLOCK  DISPLAY "DEADLOCK" END-IF
      *          IF SQLCODE = NOT-FOUND DISPLAY "NOT-FOUND" END-IF
    
      *          IF SQLCODE = OK
      *             PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
      *                 DISPLAY 
      *                 CC-PROGR-PEZZO(J)  "/"
      *                 CC-PROGR-COMPOSIZIONE(J) "/"
      *                 CC-CODICE-COMPOSIZIONE(J)(1:20) "/"
      *                 CC-CODICE-PEZZO(J)  "/"
      *                 CC-TIPO-MATERIALE(J)   "/"
      *                 CC-DESC-CLASSE(J)  (1:20)"/"
      *                 CC-TESTO-COMPOSIZIONE(J) (1:20) "/"
      *              END-PERFORM
                   .
      *    DISPLAY "EX-LEGGI-COMPOSIZIONI".
       EX-LEGGI-COMPOSIZIONI-DBG. EXIT.
      *NO-DATGE*                                                        FINE

      *NO-DATGE*                                                        INIZIO
       SELECT-COMPOSIZIONI.
      *     DISPLAY "SELECT-COMPOSIZIONI".
        EXEC SQL                                                                    
           BULK SELECT
               C.progr_pezzo,          C.progr_composizione, 
               C.codice_composizione,  C.codice_pezzo, 
               tipo_materiale,         L.desc_classe,
               testo_composizione
               INTO :CC-RIGHE-COMPOS
           FROM composizioni_modello_dbg C
               JOIN anagrafica_modelli_dbg A USING (societa, modello)
               JOIN anagrafica_modelli_barcode_negozio_dbg B 
                 ON (A.societa = B.societa)
                AND(A.modello = B.modello)
               JOIN CLASSI L ON 
                (L.classe = C.codice_pezzo) 
           WHERE
               C.societa = :CC-SOC
               AND B.modello_dt = :CC-C-MAT 
               AND f_anagrafica_modello_base = 1
            GROUP BY C.progr_pezzo, C.progr_composizione
            ORDER BY C.progr_pezzo, C.progr_composizione
        END-EXEC.
      *  DISPLAY "EX-SELECT-COMPOSIZIONI".
       EX-SELECT-COMPOSIZIONI. EXIT.
      *NO-DATGE*                                                        FINE      


068800*                                                                         
068900*VELOX*                                                                   
      *NO-DATGE*                                                        INIZIO      
069000* CERCA-MODELLO.                                                           
069100**SELECT-MIRATA*                                                           
069200**    MOVE MODELLI-MODELLO-MAXIMA TO EL-MODELLO-MAXIMA.                    
069300**    MOVE "K2" TO QT-FUNZIONE OF PARTAB-MODELLI.                          
069400**    CALL "QTABELXL" USING PARTAB-MODELLI TAB-MODELLI                     
069500**                        EL-TAB-MODELLI.                                  
069600**                                                                         
069700*     PERFORM SELEZIONA-PF-SOCIETA                                         
069800*        THRU EX-SELEZIONA-PF-SOCIETA.                                     
069900**                                                                         
070000*     IF SQLCODE-MEM = OK                                                  
070100*       PERFORM SELEZIONA-MODELLO-NEW                                      
070200*          THRU EX-SELEZIONA-MODELLO-NEW                                   
070300*     END-IF.                                                              
070400* EX-CERCA-MODELLO.                                                        
070500*     EXIT.                                                                

       CERCA-MODELLO.
           MOVE SOCIETA-SIGLA(SOCIETA-MOD OF C-MAT-COM) TO CC-SOC.
      *      DISPLAY "SOCIETA-MOD OF C-MAT-COM="
      *               SOCIETA-MOD OF C-MAT-COM
      *               "   CC-SOC=" CC-SOC.
           IF MODELLO OF C-MAT-TRANSITO NOT = 0                                 
              COMPUTE CC-C-MAT =                                  
                            C-MAT-TRANS-RID / 1000 * 1000   .                    
070100       PERFORM SELEZIONA-MODELLO-NEW                                      
070200          THRU EX-SELEZIONA-MODELLO-NEW.                                    
       EX-CERCA-MODELLO. EXIT.
      *NO-DATGE*                                                        FINE      


070600**                                                                         
070700**SELECT-MIRATA*                                                           
070800* SELEZIONA-PF-SOCIETA.                                                    
070900*     MOVE SOCIETA-MOD OF C-MAT-COM                                        
071000*       TO SOCIETA-CODICE-SOC.                                             
071100**                                                                         
071200*     PERFORM WITH TEST AFTER                                              
071300*     UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                           
071400*       PERFORM BEGIN-RC THRU BEGIN-RC-EX                                  
071500*       IF SQLCODE = OK                                                    
071600*         EXEC SQL                                                         
071700*           SELECT SOCIETA                                                 
071800*             INTO :SOCIETA-SOCIETA                                        
071900*             FROM PF.SOCIETA                                              
072000*            WHERE COD_X_BARCODE = :SOCIETA-CODICE-SOC                     
072100*         END-EXEC                                                         
072200*       END-IF                                                             
072300*       MOVE SQLCODE TO SQLCODE-MEM                                        
072400*       PERFORM S-COMMIT THRU S-COMMIT-EX                                      
072500*     END-PERFORM.                                                         
072600* EX-SELEZIONA-PF-SOCIETA.                                                 
072700*     EXIT.                                                                
072800**                                                                         
072900**SELECT-MIRATA*                                                           
073000 SELEZIONA-MODELLO-NEW.                                                   
073100     MOVE 0 TO FLAG-CURSORE.                                              
073200*                                                                         

      *NO-DATGE*                                                        inizio
073300*     MOVE SPACES TO MODELLI-MODELLO-NEW.                                  
073300     MOVE SPACES TO CC-MODELLO-DT.                                  
073400*     MOVE SOCIETA-SOCIETA TO MODELLI-SOCIETA.                             
073500*    MOVE '000' TO MODELLI-ESTENSIONE.                                    
      *NO-DATGE*                                                        inizio
073600*                                                                         
073700     PERFORM WITH TEST AFTER                                              
073800     UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                           
073900       PERFORM BEGIN-RC THRU BEGIN-RC-EX                                  
074000       PERFORM DECLARE-CURS-MOD THRU DECLARE-CURS-MOD-EX                  
074100       PERFORM OPEN-CURS-MOD THRU OPEN-CURS-MOD-EX                        
074200       PERFORM FETCH-SINGOLA-CURS-MOD                                     
074300          THRU FETCH-SINGOLA-CURS-MOD-EX                                  
      *NO-DATGE*                                                        INIZIO
074400*       MOVE SQLCODE TO SQLCODE-MEM                                        
      *NO-DATGE*                                                        FINE

074500*      IF NOT STOP-CURSORE                                                
074600*trovato modello new                                                      
074700*        CONTINUE                                                         
074800*      END-IF                                                             
074900       PERFORM CLOSE-CURS-MOD THRU CLOSE-CURS-MOD-EX                      
075000       PERFORM S-COMMIT THRU S-COMMIT-EX                                      
075100     END-PERFORM.                                                         
075200 EX-SELEZIONA-MODELLO-NEW.                                                
075300     EXIT.                                                                
075400*                                                                         
075500*                                                                         
075600 STAMPA-RIF-RESO.                                                         
075700     MOVE "rif. DOCUMENTO fornitore" TO D-BOLLA-F-ST.                     
           IF RIF-BOLLA-FORN = 0
075800        MOVE RIF-ORDINE OF MOVMAG TO RIF-INTR-RID
             ELSE                       
075800         MOVE RIF-BOLLA-FORN OF MOVMAG TO RIF-INTR-RID.                       
075900     MOVE AA-MM-GG OF RIF-INTR TO DATA-BOLLA-COM.                         
076000     MOVE CORR DATA-B-COM TO DATA-B-COM-R.                                
076100     MOVE DATA-BOLLA-COM-R TO N-DATA-F-ST.                                
076200     MOVE NUMERO OF RIF-INTR TO N-BOLLA-F-ST.                             
076300     MOVE " del " TO D-DEL-F-ST.                                          
076400     PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2.                       
076500     ADD 1 TO IND-RIGA.                                                   
076600 EX-STAMPA-RIF-RESO.                                                      
076700     EXIT.                                                                
076800*                                                                         
076900*                                                                         
063420*alloc2                                                                         
063430*DTALLO*                                                                 
077100 STAMPA-ALLOC.                                                            
077200*     MOVE "SETTORE" TO W-NOME-DATA-SET.                                   
077300*     MOVE "C-MAT" TO W-NOME-CAMPO.                                       
077400     MOVE C-MAT OF MOVMAG TO C-MAT-TRANS-RID                              
077500     MOVE PROGR-ART OF C-MAT-TRANSITO TO PROGR-ART-COM                    
077600     MOVE 0 TO PRE-PROGR-ART-COM                                          
077700     MOVE PROGR-ART-COM TO PROGR-ART OF C-MAT-TRANSITO                    
077800*     MOVE C-MAT-TRANS-RID TO W-VALORE-CAMPO.                              
077900*     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
078000*     IF W-OK-IMAGE                                                        
078100*       MOVE 5 TO W-MODO                                                   
078200*       PERFORM TTDBGET-S THRU EX-TTDBGET-S.                               
078300*     IF NOT W-OK-IMAGE                                                    
078400*       COMPUTE W-VALORE-CAMPO =                                           
078500*          (W-VALORE-CAMPO / 1000) * 1000                                  
078600*       PERFORM TTDBFIND THRU EX-TTDBFIND                                  
078700*       IF W-OK-IMAGE                                                      
078800*         MOVE 5 TO W-MODO                                                 
078900*         PERFORM TTDBGET-S THRU EX-TTDBGET-S.                             
079000*     IF NOT W-OK-IMAGE                                                    
079100*       INITIALIZE SETTORE.                                                
079200*
      *                                               
           MOVE C-MAT-TRANS-RID TO ALLO-ENTITA.   
           MOVE MAGAZZINO-DDT  TO ALLO-MAG.
           PERFORM SELECT-ALLOCAZIONI THRU EX-SELECT-ALLOCAZIONI.                                                                         
079300*     MOVE CMAT-SETTORE OF SETTORE TO NOME-MODELLO  
           IF SQLCODE-MEM-2 = 0
               MOVE REC-ALLO-CMATSET(1) TO NOME-MODELLO.                        
079400*     IF W-OK-IMAGE AND                                                    
079500*          DT-STAMPA OF SETTORE = 0                                        
079600*         MOVE "  ***  Alloc"  TO C-MAT-DETT                               
079700*     ELSE                                                                 
079800         MOVE "  Alloc" TO C-MAT-DETT.                                    
079900     PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2.                       
080000     ADD 1 TO IND-RIGA.                                                   
080100*                                                                         
080200*     IF W-OK-IMAGE AND                                                    
080300*           DT-STAMPA OF SETTORE = 0                                       
080400*         PERFORM DATA-ALLOCAZIONE THRU EX-DATA-ALLOCAZIONE.               
080500 EX-STAMPA-ALLOC.                                                         
080600     EXIT.                                                                
080700*
       SELECT-ALLOCAZIONI.
      *
      *NO-DATGE*                                                        inizio
      *    PERFORM S-SET-1 THRU S-SET-1-EX  
      *NO-DATGE*                                                        FINE
      *                      
           EXEC SQL                                                             
           BULK   SELECT ENTITA,CMATSET,MAG,TIPO,ENTITA_REALE,TS_NAS,
                         TS_UV                                               
                  INTO                                                             
                        :TAB-REG-ALLO                                                
                  FROM   ALLOCAZIONI                                        
                  WHERE  ENTITA = :ALLO-ENTITA
                    AND  MAG    = :ALLO-MAG                    
           END-EXEC. 
      * 
           MOVE SQLCODE TO SQLCODE-MEM-2.
      *     
           PERFORM S-COMMIT THRU S-COMMIT-EX.        
      *            
      *NO-DATGE*                                                        inizio
      *     PERFORM S-SET-2 THRU S-SET-2-EX.     
      *NO-DATGE*                                                        FINE
       EX-SELECT-ALLOCAZIONI.  
           EXIT.  
      *  
      *NO-DATGE*                                                        inizio
020300* S-SET-2.                                                                 
020400*        EXEC SQL                                                          
020500*           SET CONNECTION 'DB2'                                           
020600*        END-EXEC.                                                         
020700* S-SET-2-EX.                                                              
020800*     EXIT.
      *NO-DATGE*                                                        FINE
      *
020300 S-SET-1.                                                                 
020400        EXEC SQL                                                          
020500           SET CONNECTION 'DB1'                                           
020600        END-EXEC.                                                         
020700 S-SET-1-EX.                                                              
020800     EXIT.                                                                                  
080800*                                                                         
080900* DATA-ALLOCAZIONE.                                                        
081000*     MOVE W-FORMATO-INTERNO TO DT-STAMPA OF SETTORE.                      
081100*     PERFORM TTUPDATE-S THRU EX-TTUPDATE-S.                               
081200*     IF NOT W-OK-IMAGE                                                    
081300*       DISPLAY "DATA-ALLOCAZIONE : err upd SETTORE "                      
081400*          W-STATUS-WORD-IMAGE                                             
      *     CANCEL "QDBERROR"
081500*       CALL "QDBERROR" USING W-COMMON.                                    
081600*                                                                         
081700* EX-DATA-ALLOCAZIONE.                                                     
081800*     EXIT.                                                                
081900*alloc2*                                                                         
082000*                                                                         
082100*SELECT-MIRATA*                                                           
082200*TRATTA-SQL.                                                              
082300*    IF PEZZO-A OF C-MAT-TRANSITO NOT = 0                                 
082400*       MOVE PEZZO-A OF C-MAT-TRANSITO TO TAB-MOD-PEZZO(1)                
082500*       MOVE TAB-M-CLASSE (I1) TO TAB-COD-PEZZO(1)                        
082600*       MOVE 1 TO NUM-MAX-ELEM-P                                          
082700*      ELSE                                                               
082800*         IF TAB-M-NUM-PEZZI (I1) > 1                                     
082900*            MOVE TAB-M-SOCIETA (I1) TO MODELLI-SOCIETA                   
083000*            MOVE TAB-M-MODELLO-NEW (I1) TO                               
083100*                MODELLI-MODELLO-NEW                                      
083200*            PERFORM WITH TEST AFTER                                      
083300*                 UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK              
083400*               PERFORM BEGIN-RC THRU BEGIN-RC-EX                         
083500*               IF SQLCODE = OK                                           
083600*                  PERFORM SE-SELECT-MOD-PEZZI                            
083700*                      THRU SE-SELECT-MOD-PEZZI-EX                        
083800*               END-IF                                                    
083900*            END-PERFORM                                                  
084000*            MOVE SQLERRD(3) TO NUM-MAX-ELEM-P                            
084100*            PERFORM COMMIT THRU COMMIT-EX                                
084200*           ELSE                                                          
084300*              MOVE 1 TO TAB-MOD-PEZZO(1)                                 
084400*              MOVE TAB-M-CLASSE (I1) TO TAB-COD-PEZZO(1)                 
084500*              MOVE 1 TO NUM-MAX-ELEM-P                                   
084600*         END-IF                                                          
084700*    END-IF.                                                              
084800*    MOVE C-MAT-TRANS-RID TO C-MAT-DETT.                                  
084900*    MOVE 0 TO TOT-CAPI-RIGA-C.                                           
085000*    PERFORM METTI-QTA-TAGLIA THRU EX-METTI-QTA-TAGLIA                    
085100*       VARYING W-INDICE-7 FROM 1 BY 1                                    
085200*       UNTIL W-INDICE-7 > 8.                                             
085300*    MOVE TOT-CAPI-RIGA-C TO TOT-CAPI-RIGA.                               
085400*    MOVE TAB-M-NOME (I1) TO NOME-MODELLO.                                
085500*    MOVE CLASSE OF C-MAT-TRANSITO TO CLASSI-CLASSE.                      
085600*    PERFORM CERCA-CL THRU EX-CERCA-CL.                                   
085700*                                                                         
085800*    MOVE TAB-M-COD-IVA(I1) TO COM-IVA-F. 
085900*    PERFORM SCRIVI-RESTO THRU EX-SCRIVI-RESTO.                           
086000*                                                                         
086100*    PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2.                       
086200*    ADD 1 TO IND-RIGA.                                                   
086300*    IF DOC-DDT                                                           
086400*       DISPLAY "Entro in STAMPA-RIGHE-COMPOS"  C-MAT-DETT         ALLE   
086500*       PERFORM STAMPA-RIGHE-COMPOS                                       
086600*              THRU EX-STAMPA-RIGHE-COMPOS                                
086700*                VARYING W-INDICE-7 FROM 1 BY 1                           
086800*                  UNTIL W-INDICE-7 > NUM-MAX-ELEM-P.                     
086900*    ADD TOT-CAPI-RIGA-C TO TOT-BOLLA-C.                                  
087000*EX-TRATTA-SQL.                                                           
087100*    EXIT.                                                                
087200*                                                                         
087300*                                                                         
      *NO-DATGE*                                                        inizio
087400* TRATTA-SQL.                              
087500*     IF PEZZO-A OF C-MAT-TRANSITO NOT = 0                                 
087600*       MOVE PEZZO-A OF C-MAT-TRANSITO TO TAB-MOD-PEZZO(1)                 
087700*       MOVE MODELLI-CLASSE TO TAB-COD-PEZZO(1)                            
087800*       MOVE 1 TO NUM-MAX-ELEM-P                                           
087900*     ELSE                                                                 
088000*       IF MODELLI-NUM-PEZZI > 1                                           
088100*         PERFORM WITH TEST AFTER                                          
088200*         UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                       
088300*            PERFORM BEGIN-RC THRU BEGIN-RC-EX                             
088400*            IF SQLCODE = OK                                               
088500*               PERFORM SE-SELECT-MOD-PEZZI                                
088600*                  THRU SE-SELECT-MOD-PEZZI-EX                             
088700*            END-IF                                                        
088800*         END-PERFORM                                                      
088900*         MOVE SQLERRD(3) TO NUM-MAX-ELEM-P                                
089000*         PERFORM S-COMMIT THRU S-COMMIT-EX                                    
089100*       ELSE                                                               
089200*         MOVE 1 TO TAB-MOD-PEZZO(1)                                       
089300*         MOVE MODELLI-CLASSE TO TAB-COD-PEZZO(1)                          
089400*         MOVE 1 TO NUM-MAX-ELEM-P                                         
089500*       END-IF                                                             
089600*     END-IF.                                                              
089700**                                                                         
089800*     MOVE C-MAT-TRANS-RID TO C-MAT-DETT.                                  
089900*     MOVE 0 TO TOT-CAPI-RIGA-C.                                           
090000*     PERFORM METTI-QTA-TAGLIA THRU EX-METTI-QTA-TAGLIA                    
090100*        VARYING W-INDICE-7 FROM 1 BY 1                                    
090200*        UNTIL W-INDICE-7 > NTG-NTG.                                             
090300*     MOVE TOT-CAPI-RIGA-C TO TOT-CAPI-RIGA.                               
090400*     MOVE MODELLI-NOME TO NOME-MODELLO.                                   
090500*     MOVE CLASSE OF C-MAT-TRANSITO TO CLASSI-CLASSE.                      
090600*     PERFORM CERCA-CL THRU EX-CERCA-CL.                                   
090700**                                                                         
090800*     MOVE MODELLI-COD-IVA TO COM-IVA-F.                                   
090900*     PERFORM SCRIVI-RESTO THRU EX-SCRIVI-RESTO.                           
091000**                                                                         
091100*     PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2.                       
091200*     ADD 1 TO IND-RIGA.                                                   
091300*     IF DOC-DDT                                                           
091400*        PERFORM STAMPA-RIGHE-COMPOS                                       
091500*               THRU EX-STAMPA-RIGHE-COMPOS                                
091600*                 VARYING W-INDICE-7 FROM 1 BY 1                           
091700*                   UNTIL W-INDICE-7 > NUM-MAX-ELEM-P.                     
091800*     ADD TOT-CAPI-RIGA-C TO TOT-BOLLA-C.                                  
091900* EX-TRATTA-SQL.                                                           
092000*     EXIT.                                                                
      *NO-DATGE*                                                        FINE


      *NO-DATGE*                                                        INIZIO
       TRATTA-SQL.
089800     MOVE C-MAT-TRANS-RID TO C-MAT-DETT.                                  
089900     MOVE 0 TO TOT-CAPI-RIGA-C.                                           
090000     PERFORM METTI-QTA-TAGLIA THRU EX-METTI-QTA-TAGLIA                    
090100        VARYING W-INDICE-7 FROM 1 BY 1                                    
090200        UNTIL W-INDICE-7 > NTG-NTG.                                             
090300     MOVE TOT-CAPI-RIGA-C TO TOT-CAPI-RIGA.                               
090400     MOVE CC-NOME TO NOME-MODELLO.        
      *    display "tratta-sql cc-nome=" cc-nome                           
090500     MOVE CLASSE OF C-MAT-TRANSITO TO CC-CLASSE.                      
090600     PERFORM CERCA-CL THRU EX-CERCA-CL.                                   
090700*                                                                         
090800*     MOVE MODELLI-COD-IVA TO COM-IVA-F.                           
           MOVE CC-IVA  TO COM-IVA-F.
           
090900     PERFORM SCRIVI-RESTO THRU EX-SCRIVI-RESTO.                           
091000*                                                                         
091100     PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2.                       
091200     ADD 1 TO IND-RIGA.                   

      *      display " in tratta-sql tipo-doc-ddt-m = "  tipo-doc-ddt                                                     
                                           
091300     IF DOC-DDT     
091400*       PERFORM STAMPA-RIGHE-COMPOS                                       
091500*              THRU EX-STAMPA-RIGHE-COMPOS                                
091600*                VARYING W-INDICE-7 FROM 1 BY 1                           
091700*                  UNTIL W-INDICE-7 > NUM-MAX-ELEM-P.                     

      *NO-DATGE*                                                        inizio      
                PERFORM LEGGI-COMPOSIZIONI-DBG THRU
                     EX-LEGGI-COMPOSIZIONI-DBG
      *          DISPLAY "SQLCODE " SQLCODE
                PERFORM TEST-ERR THRU TEST-ERR-EX
      *          IF SQLCODE = OK        DISPLAY "OK" END-IF
      *          IF SQLCODE = NO-MEMORY DISPLAY "NO_MEMORY" END-IF
      *          IF SQLCODE = DEADLOCK  DISPLAY "DEADLOCK" END-IF
      *          IF SQLCODE = NOT-FOUND DISPLAY "NOT-FOUND" END-IF

                IF SQLCODE = OK
                AND CC-RIGHE-COMPOS <> SPACES
                
                    MOVE 0 TO SW-PIU-PEZZI
                    PERFORM VARYING I FROM 1 BY 1 
                      UNTIL I > 20  
      *                DISPLAY CC-RR(I)
      *                DISPLAY "CC-PROGR-PEZZO(" I ") = [" 
      *                        CC-PROGR-PEZZO(I) "]"
                      IF CC-RR(I) <> SPACES
                      AND CC-PROGR-PEZZO(I) > 1
                            MOVE 20 TO I
                            MOVE 1 TO SW-PIU-PEZZI
                    END-PERFORM
      *              display  "SW-PIU-PEZZI=" SW-PIU-PEZZI
      
                                        
                    MOVE 9 TO CC-MEM-PROGR
                    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
                       OR CC-RR(I) = SPACES
                        IF I = 1
                            MOVE "composizione : " TO D-DESC-COMP
                            MOVE SPACES TO D-CLASSE-PEZZO
                            IF PIU-PEZZI
                                MOVE CC-DESC-CLASSE(I) 
                                  TO D-CLASSE-PEZZO
                            END-IF                            
                            MOVE CC-TESTO-COMPOSIZIONE(I) TO D-TIPO-COMP
                            MOVE CC-CODICE-COMPOSIZIONE(I) 
                              TO D-COMPOS-COMP
                            PERFORM CALL-QWPRINT-2 THRU 
                                 EX-CALL-QWPRINT-2
                        ELSE
                            MOVE SPACES TO D-CLASSE-PEZZO
                            MOVE CC-TESTO-COMPOSIZIONE(I) TO D-TIPO-COMP
                            IF CC-PROGR-PEZZO(I) <> CC-MEM-PROGR 
                                MOVE CC-DESC-CLASSE(I) 
                                  TO D-CLASSE-PEZZO
                            END-IF
                            MOVE CC-CODICE-COMPOSIZIONE(I) 
                              TO D-COMPOS-COMP
                            PERFORM CALL-QWPRINT-2 THRU 
                                 EX-CALL-QWPRINT-2
                        END-IF
                        MOVE CC-PROGR-PEZZO(I) TO CC-MEM-PROGR
                    END-PERFORM.
                    
091800     ADD TOT-CAPI-RIGA-C TO TOT-BOLLA-C.                                  
       EX-TRATTA-SQL. EXIT.
      *NO-DATGE*                                                        FINE      


092100*                                                                         
092200*                                                                         
092300*                                                                         
092400 SCRIVI-RESTO.
092500     IF NOT SI-FILE-FAT     
      *          display "scrivi-resto si-file-fat nome-moodello-f="             
      *                  nome-modello-f                                                                                                          
092600       GO TO EX-SCRIVI-RESTO.                                             
092700*                                                                         
092800     MOVE SPACES TO REC-DAFRPFAT.                                         
092900     MOVE C-MAT-TRANS-RID TO C-MAT-F.                                     
093000     MOVE 0 TO TIPO-REC-F.                                                
093100     MOVE NOME-MODELLO TO NOME-MODELLO-F.                                 
093200     MOVE PREZZO OF MOVMAG TO PREZZO-F.                                   
093300     MOVE COM-IVA-F TO IVA-F.                                            
093400     MOVE DESC-CL-C-MAT TO DESC-CL-C-MAT-F.                               
093500     MOVE QTA-TAGLIE OF MOVMAG TO QTA-OTTO-F.                             
093600     WRITE REC-DAFRPFAT.    
      *      display "ex-scrivi-resto nome-moodello-f=" nome-modello-f.                                                
093700 EX-SCRIVI-RESTO.                                                         
093800     EXIT.                                                                
093900*                                                                         
094000 SCRIVI-RESTO2.                                                           
094100     IF NOT SI-FILE-FAT                                                   
      *          display "scrivi-resto2 si-file-fat nome-moodello="              
      *                  nome-modello                                       
094200       GO TO EX-SCRIVI-RESTO2.                                            
094300*                                                                         
094400     MOVE SPACES TO REC-DAFRPFAT.                                         
094500     MOVE C-MAT-TRANS-RID TO C-MAT-F.                                     
094600     MOVE COM-TIPO-REC-F TO TIPO-REC-F.                                   
094700     MOVE D-CLASSE-PEZZO TO D-CLASSE-PEZZO-F.                             
094800     MOVE D-TIPO-COMP TO D-TIPO-COMP-F.                                   
094900     MOVE D-COMPOS-COMP TO D-COMPOS-COMP-F.                               
095000     WRITE REC-DAFRPFAT.                                                  
      *      display "ex-scrivi-resto2 nome-moodello-f=" nome-modello-f.                                                
095100 EX-SCRIVI-RESTO2.                                                        
095200     EXIT.                                                                
095300*                                                                         
095400*                                                                         
      *NO-DATGE*                                                        inizio
095500* STAMPA-RIGHE-COMPOS.                                                     
095600*     MOVE TAB-MOD-PEZZO(W-INDICE-7) TO MODPEZZI-PEZZO.                    
095700**SELECT-MIRATA*                                                           
095800**    MOVE TAB-M-SOCIETA (I1) TO MODELLI-SOCIETA.                          
095900**    MOVE TAB-M-MODELLO-NEW (I1) TO MODELLI-MODELLO-NEW.                  
096000**                                                                         
096100*     PERFORM WITH TEST AFTER                                              
096200*             UNTIL SQLCODE <> NO-MEMORY                                   
096300*                       AND <> DEADLOCK                                    
096400*        PERFORM BEGIN-RC THRU BEGIN-RC-EX                                 
096500*        IF SQLCODE = OK                                                   
096600*           PERFORM SE-SELECT-COMPOS                                       
096700*                  THRU SE-SELECT-COMPOS-EX                                
096800*        END-IF                                                            
096900*     END-PERFORM.                                                         
097000*     IF SQLCODE = NOT-FOUND                                               
097100*        MOVE SPACES TO TAB-ELEM-COMPOS                                    
097200*        MOVE 0 TO NUM-MAX-ELEM-C                                          
097300*       ELSE                                                               
097400*          MOVE SQLERRD (3) TO NUM-MAX-ELEM-C.                             
097500*     PERFORM S-COMMIT THRU S-COMMIT-EX.                                       
097600*     MOVE TAB-COD-PEZZO (W-INDICE-7) TO CLASSI-CLASSE.                    
097700*     IF NUM-MAX-ELEM-P > 1                                                
097800*        PERFORM CERCA-CL THRU EX-CERCA-CL.                                
097900*     MOVE SPACES TO ZONA-COMPOSIZIONI                                     
098000*                       ZONA-COMPOSIZIONI-F.                               
098100*     PERFORM METTI-COMP-ST THRU EX-METTI-COMP-ST                          
098200*             VARYING W-INDICE-6 FROM 1 BY 1                               
098300*                UNTIL W-INDICE-6 > NUM-MAX-ELEM-C.                        
098400*     IF W-INDICE-7 = 1                                                    
098500*        MOVE "composizione :" TO C-MAT-DETT.                              
098600*     MOVE "tessuto " TO D-TIPO-COMP.                                      
098700*     MOVE ZONA-COMPOSIZIONI TO D-COMPOS-COMP.                             
098800**                                                                         
098900*     COMPUTE COM-TIPO-REC-F = (W-INDICE-7 - 1) * 2 + 1.                   
099000*     PERFORM SCRIVI-RESTO2 THRU EX-SCRIVI-RESTO2.                         
099100**                                                                         
099200*     PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2.                       
099300*     ADD 1 TO IND-RIGA.                                                   
099400*     IF ZONA-COMPOSIZIONI-F NOT = SPACES                                  
099500*        MOVE "fodera " TO D-TIPO-COMP                                     
099600*        MOVE ZONA-COMPOSIZIONI-F TO D-COMPOS-COMP                         
099700**                                                                         
099800*        COMPUTE COM-TIPO-REC-F =                                          
099900*                   (W-INDICE-7 - 1) * 2 + 2                               
100000*        PERFORM SCRIVI-RESTO2 THRU EX-SCRIVI-RESTO2                       
100100**                                                                         
100200*        PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2                     
100300*        ADD 1 TO IND-RIGA.                                                
100400* EX-STAMPA-RIGHE-COMPOS.                                                  
100500*     EXIT.                                                                
      *NO-DATGE*                                                        FINE
100600*                                                                         
100700*                                                                         
100800 TRATTA-IMAGE.                                                            
100900     MOVE "ANAMAT;" TO W-NOME-DATA-SET.                                   
101000     MOVE "C-MAT;" TO W-NOME-CAMPO.                                       
101100     MOVE C-MAT-TRANS-RID TO W-VALORE-CAMPO.                              
101200     MOVE 7 TO W-MODO.                                                    
101300     PERFORM TTDBGET THRU EX-TTDBGET.                                     
101400     IF PRODOTTI-FINITI                                                   
101500        MOVE D-MAT OF REC-ANAMAT TO NOME-MODELLO                          
101600       ELSE                                                               
101700          MOVE D-MAT OF REC-ANAMAT TO DESC-CL-C-MAT.                      
101800     COMPUTE COM-IVA-F = ALIQ-IVA OF REC-ANAMAT * 100.                    
101900     MOVE C-MAT-TRANS-RID TO C-MAT-DETT                                   
102000     MOVE 0 TO TOT-CAPI-RIGA-C.                                           
102100     PERFORM METTI-QTA-TAGLIA THRU EX-METTI-QTA-TAGLIA                    
102200        VARYING W-INDICE-7 FROM 1 BY 1                                    
102300        UNTIL W-INDICE-7 > NTG-NTG.                                             
102400     MOVE TOT-CAPI-RIGA-C TO TOT-CAPI-RIGA.                               
102500     IF PRODOTTI-FINITI                                                   

      *NO-DATGE*                                                        INIZIO
102600*        MOVE CLASSE OF C-MAT-TRANSITO TO CLASSI-CLASSE                    
102600        MOVE CLASSE OF C-MAT-TRANSITO TO CC-CLASSE                    
      *NO-DATGE*                                                        fine

102700        PERFORM CERCA-CL THRU EX-CERCA-CL.                                
102800*                                                                         
102900     PERFORM SCRIVI-RESTO THRU EX-SCRIVI-RESTO.                           
103000*                                                                         
103100     PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2.                       
103200     ADD 1 TO IND-RIGA.                                                   
103300     IF DOC-DDT                                                           
103400        MOVE SPACES TO ZONA-COMPOSIZIONI                                  
103500                       ZONA-COMPOSIZIONI-F                                
103600        MOVE "COMPOS;" TO W-NOME-DATA-SET                                 
103700        MOVE "C-MAT;" TO W-NOME-CAMPO                                     
103800        COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000            
103900        PERFORM TTDBFIND THRU EX-TTDBFIND                                 
104000        IF W-OK-IMAGE                                                     
104100           MOVE 5 TO W-MODO                                               
104200           PERFORM TTDBGET THRU EX-TTDBGET                                
104300        END-IF                                                            
104400        IF W-OK-IMAGE                                                     
104500           PERFORM METTI-COMP-T THRU EX-METTI-COMP-T                      
104600              VARYING W-INDICE-2 FROM 1 BY 1                              
104700                  UNTIL W-INDICE-2 > 6                                    
104800           PERFORM METTI-COMP-F THRU EX-METTI-COMP-F                      
104900              VARYING W-INDICE-2 FROM 1 BY 1                              
105000                  UNTIL W-INDICE-2 > 2                                    
105100           MOVE "composizione :" TO C-MAT-DETT                            
105200           MOVE "tessuto " TO D-TIPO-COMP                                 
105300           MOVE ZONA-COMPOSIZIONI TO D-COMPOS-COMP                        
105400*                                                                         
105500           COMPUTE COM-TIPO-REC-F =                                       
105600                                             1                            
105700           PERFORM SCRIVI-RESTO2 THRU EX-SCRIVI-RESTO2                    
105800*                                                                         
105900           PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2                  
106000           ADD 1 TO IND-RIGA                                              
106100           IF ZONA-COMPOSIZIONI-F NOT = SPACES                            
106200              MOVE "fodera " TO D-TIPO-COMP                               
106300              MOVE ZONA-COMPOSIZIONI-F TO D-COMPOS-COMP                   
106400*                                                                         
106500              COMPUTE COM-TIPO-REC-F =                                    
106600                                                2                         
106700              PERFORM SCRIVI-RESTO2 THRU EX-SCRIVI-RESTO2                 
106800*                                                                         
106900              PERFORM CALL-QWPRINT-2 THRU EX-CALL-QWPRINT-2               
107000              ADD 1 TO IND-RIGA.                                          
107100     ADD TOT-CAPI-RIGA-C TO TOT-BOLLA-C.                                  
107200 EX-TRATTA-IMAGE.                                                         
107300     EXIT.                                                                
107400*                                                                         
107500*                                                                         

      *NO-DATGE*                                                        INIZIO
107600* METTI-COMP-ST.                                                           
107700*     IF TAB-TIPO-MAT (W-INDICE-6) = "F"                                   
107800*        MOVE TAB-COMPOSIZIONE(W-INDICE-6)                                 
107900*                   TO ZONA-COMPOSIZIONI-F                                 
108000*       ELSE                                                               
108100*          MOVE TAB-COMPOSIZIONE(W-INDICE-6)                               
108200*                  TO ZONA-COMPOSIZIONI.                                   
108300* EX-METTI-COMP-ST.                                                        
108400*     EXIT.                                                                
      *NO-DATGE*                                                        FINE

108500*                                                                         
108600*                                                                         
108700 CERCA-CL.                                                                
108800     MOVE SPACES TO DESC-CL-C-MAT.                                        
108900     IF PEZZO-A OF C-MAT-TRANSITO NOT = 0                                 
109000        PERFORM CERCA-ANAMAT-CL THRU EX-CERCA-ANAMAT-CL.                  
109100     PERFORM CERCA-ANAMAT2 THRU EX-CERCA-ANAMAT2.                         
109200     IF NOT W-OK-IMAGE OR                                                 
109300            (W-OK-IMAGE AND DESCR-CL-AGG                                  
109400                       OF REC-ANAMAT2 = SPACE)                            

      *NO-DATGE*                                                        inizio
109500*        MOVE "ITAL" TO CLASSI-LINGUA                                      
              MOVE "MM" TO CC-SOCIETA
      *NO-DATGE*                                                        fine
                                       
109600        PERFORM WITH TEST AFTER                                           
109700                UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK                
109800           PERFORM BEGIN-RC THRU                                          
109900                   BEGIN-RC-EX                                            
110000           IF SQLCODE = OK                                                
110100              PERFORM SE-SELECT-CLASSI                                    
110200                     THRU SE-SELECT-CLASSI-EX                             
110300           END-IF                                                         
110400        END-PERFORM                                                       
110500        PERFORM S-COMMIT THRU S-COMMIT-EX      
                               
      *NO-DATGE*                                                        inizio
110600*        MOVE CLASSI-DESCRIZIONE TO DESC-CL-C-MAT                          
110600        MOVE CC-DESCRIZIONE TO DESC-CL-C-MAT                          
      *NO-DATGE*                                                        fine


110700       ELSE                                                               
110800          MOVE DESCR-CL-AGG TO DESC-CL-C-MAT.                             
110900 EX-CERCA-CL.                                                             
111000     EXIT.                                                                
111100*                                                                         
111200*                                                                         
      *NO-DATGE*                                                        INIZIO
111300* SE-SELECT-CLASSI.                                                        
111400*     EXEC SQL                                                             
111500*          SELECT      CLASSE,                                             
111600*                      DESCR                                               
111700*          INTO :CLASSI-CLASSE,                                            
111800*               :CLASSI-DESCRIZIONE                                        
111900*          FROM PF.CLASSI                                                  
112000*          WHERE CLASSE        = :CLASSI-CLASSE AND                        
112100*                LINGUA        = :CLASSI-LINGUA                            
112200*     END-EXEC                                                             
112300*     MOVE "SELECT CLASSI     " TO ER-DESCRIZIONE                          
112400*     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
112500* SE-SELECT-CLASSI-EX.                                                     
112600*     EXIT.                                                                
112700*                                                                         
112800*                                                                         
111300 SE-SELECT-CLASSI.                                                        
111400     EXEC SQL                                                             
111500          SELECT      classe, 
                            desc_classe                                               
111700          INTO :CC-CLASSE,                                            
111800               :CC-DESCRIZIONE                                        
111900          FROM  anagrafica_classi_dbg                                                  
112000          WHERE classe        = :CC-CLASSE AND                        
112100                societa       = :CC-SOCIETA                            
112200     END-EXEC                                                             
112300     MOVE "SELECT CLASSI     " TO ER-DESCRIZIONE                          
112400     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
112500 SE-SELECT-CLASSI-EX.                                                     
112600     EXIT.                                                                
      *NO-DATGE*                                                        fine

112900 CERCA-ANAMAT2.                                                           
113000     MOVE "ANAMAT2;" TO W-NOME-DATA-SET.                                  
113100     MOVE "C-MAT;" TO W-NOME-CAMPO.                                       
113200     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.              
113300     MOVE 7 TO W-MODO.                                                    
113400     PERFORM TTDBGET THRU EX-TTDBGET.                                     
113500 EX-CERCA-ANAMAT2.                                                        
113600     EXIT.                                                                
113700*                                                                         
113800*                                                                         
113900 CERCA-ANAMAT-CL.                                                         
114000     MOVE "ANAMATCL;" TO W-NOME-DATA-SET.                                 
114100     MOVE "C-MAT;" TO W-NOME-CAMPO                                        
114200     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.              
114300     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
114400     IF W-OK-IMAGE                                                        
114500        MOVE 5 TO W-MODO                                                  
114600        PERFORM TTDBGET THRU EX-TTDBGET                                   
114700        IF W-OK-IMAGE                                                     

      *NO-DATGE*                                                        INIZIO
114800*           MOVE CLASSE-ABB OF REC-ANAMATCL TO CLASSI-CLASSE.              
114800           MOVE CLASSE-ABB OF REC-ANAMATCL TO CC-CLASSE.              
      *NO-DATGE*                                                        fine

114900 EX-CERCA-ANAMAT-CL.                                                      
115000     EXIT.                                                                
115100*                                                                         
115200*                                                                         
115300*                                                                         
115400 METTI-QTA-TAGLIA.                                                        
115500     COMPUTE QTA-TG-SPED (W-INDICE-7) =                                   
115600            QTA-TAGLIA OF MOVMAG (W-INDICE-7) * -1.                       
115700     COMPUTE TOT-CAPI-RIGA-C = TOT-CAPI-RIGA-C +                          
115800             QTA-TAGLIA OF MOVMAG (W-INDICE-7) * -1.                      
115900 EX-METTI-QTA-TAGLIA.                                                     
116000     EXIT.                                                                
116100*                                                                         
116200*                                                                         
116300*                                                                         
116400 METTI-COMP-T.                                                            
116500     IF PERC-COMPOS OF COMPOS-TESSUTO (W-INDICE-2) = 100                  
116600        MOVE PERC-COMPOS OF COMPOS-TESSUTO (W-INDICE-2)                   
116700               TO PERC-T-R                                                
116800        MOVE SIGLA-FIBRA OF COMPOS-TESSUTO (W-INDICE-2)                   
116900               TO SIGLA-T-R                                               
117000        MOVE 10 TO W-INDICE-2                                             
117100      ELSE                                                                
117200         MOVE PERC-COMPOS OF COMPOS-TESSUTO (W-INDICE-2)                  
117300                      TO PERC-T (W-INDICE-2)                              
117400         MOVE SIGLA-FIBRA OF COMPOS-TESSUTO (W-INDICE-2)                  
117500                      TO SIGLA-T (W-INDICE-2).                            
117600 EX-METTI-COMP-T.                                                         
117700     EXIT.                                                                
117800*                                                                         
117900*                                                                         
118000*                                                                         
118100*                                                                         
      *NO-DATGE*                                                        INIZIO
118200* SE-SELECT-COMPOS.                                                        
118300*     EXEC SQL                                                             
118400*     BULK SELECT       PEZZO,                                             
118500*                       PROGR,                                             
118600*                       TIPO_MAT,                                          
118700*                       COMPOSIZIONE                                       
118800*          INTO         :TAB-ELEM-COMPOS                                   
118900*          FROM ANAMAT.COMPOS                                              
119000*          WHERE SOCIETA       = :MODELLI-SOCIETA AND                      
119100*                MODELLO_NEW   = :MODELLI-MODELLO-NEW AND                  
119200*                PEZZO         = :MODPEZZI-PEZZO                           
119300*     END-EXEC                                                             
119400*     MOVE "SELECT COMPOS       " TO ER-DESCRIZIONE                        
119500*     IF SQLCODE NOT = MULTIPLE-ROWS                                       
119600*        PERFORM TEST-ERR THRU TEST-ERR-EX.                                
119700* SE-SELECT-COMPOS-EX.                                                     
119800*     EXIT.                                                                
      *NO-DATGE*                                                        fine
119900*                                                                         
120000*                                                                         
120100*SELECT-MIRATA*                                                           
120200*SE-SELECT-MODELLI.                                                       
120300*    EXEC SQL                                                             
120400*    BULK SELECT      MODELLO_NEW,                                        
120500*                     NOME,                                               
120600*                     SOCIETA,                                            
120700*                     CLASSE_MMFG,                                        
120800*                     MODELLO_MAXIMA,                                     
120900*                     NUM_PEZZI,                                          
121000**VELOX*                                                                  
121100**                     COD_ESTENSIONE,                                    
121200*                     IVA                                                 
121300*         INTO        :TAB-MODELLI                                        
121400*         FROM ANAMAT.MODELLI                                             
121500**VELOX*                                                                  
121600**         WHERE MODELLO_MAXIMA = :MODELLI-MODELLO-MAXIMA AND             
121700*         WHERE COD_ESTENSIONE = :MODELLI-ESTENSIONE                      
121800*VELOX*                                                                   
121900*         ORDER BY MODELLO_MAXIMA                                         
122000*                                                                         
122100*    END-EXEC                                                             
122200*    MOVE "SELECT MODELLI    " TO ER-DESCRIZIONE                          
122300*    IF SQLCODE NOT = MULTIPLE-ROWS                                       
122400*       PERFORM TEST-ERR THRU TEST-ERR-EX.                                
122500*SE-SELECT-MODELLI-EX.                                                    
122600*    EXIT.                                                                
122700*                                                                         
122800*                                                                         
      *NO-DATGE*                                                        INIZIO
122900* SE-SELECT-MOD-PEZZI.                                                     
123000*     EXEC SQL                                                             
123100*     BULK SELECT      PEZZO,                                              
123200*                      COD_PEZZO                                           
123300*          INTO        :TAB-ELEM-PEZZI                                     
123400*          FROM ANAMAT.MOD_PEZZI                                           
123500*          WHERE SOCIETA       = :MODELLI-SOCIETA AND                      
123600*                MODELLO_NEW   = :MODELLI-MODELLO-NEW                      
123700*     END-EXEC                                                             
123800*     MOVE "SELECT MOD-PEZZI    " TO ER-DESCRIZIONE                        
123900*     IF SQLCODE NOT = MULTIPLE-ROWS                                       
124000*        PERFORM TEST-ERR THRU TEST-ERR-EX.                                
124100* SE-SELECT-MOD-PEZZI-EX.                                                  
124200*     EXIT.                                                                
      *NO-DATGE*                                                        fine
124300*                                                                         
124400*                                                                         
124500 METTI-COMP-F.                                                            
124600     IF PERC-COMPOS OF COMPOS-FODERA (W-INDICE-2) = 100                   
124700        MOVE PERC-COMPOS OF COMPOS-FODERA (W-INDICE-2)                    
124800               TO PERC-F-R                                                
124900        MOVE SIGLA-FIBRA OF COMPOS-FODERA (W-INDICE-2)                    
125000               TO SIGLA-F-R                                               
125100        MOVE 10 TO W-INDICE-2                                             
125200      ELSE                                                                
125300         MOVE PERC-COMPOS OF COMPOS-FODERA (W-INDICE-2)                   
125400                      TO PERC-F (W-INDICE-2)                              
125500         MOVE SIGLA-FIBRA OF COMPOS-FODERA (W-INDICE-2)                   
125600                      TO SIGLA-F (W-INDICE-2).                            
125700 EX-METTI-COMP-F.                                                         
125800     EXIT.                                                                
125900*                                                                         
126000*                                                                         
126100*                                                                         
126200*                                                                         
126300 STAMPA-TESTATA.                                                          
126400     MOVE SPACES TO DATI-RIGA.                                            
126500     MOVE "P" TO COMANDO.                                                 
126600     MOVE 1 TO N-RIGA-STAMPA.                                             
126700     MOVE D-CONTO-MEM TO RAG-SOC.                                         
126800     MOVE CLIENTE-DDT TO COD-RAG-SOC.   
126900     PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                       
127000     MOVE D-CONTO-AGG-MEM TO RAG-SOC.       
127100     PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                       
127200     MOVE 1 TO N-RIGA-STAMPA.                                             
127300     MOVE INDIRIZZO-COM TO RAG-SOC.        
127400     PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                       
127500     MOVE LOCALITA-COM TO LOCALITA-TEST.                                  
127600     MOVE CAP-COM TO CAP-TEST.                                            
127700     MOVE PROV-COM TO PROV-TEST.                                          
127800     MOVE STATO-COM TO STATO-TEST.      
127900     PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                       
128000     IF INDIRIZZO-C-COM NOT = SPACES                                      
128100        MOVE 2 TO N-RIGA-STAMPA                                           
128200        MOVE D-CONTO-MEM TO RAG-SOC 
128300        PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1                     
128400        MOVE D-CONTO-AGG-MEM TO RAG-SOC     
128500        PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1                     
128600       ELSE                                                               
128700          MOVE 4 TO N-RIGA-STAMPA.                                        
128800     IF IND-PAG NOT < 2                                                   
128900        MOVE "Seguito" TO DESC-BOLLA.                                     
129000     PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                       
129100     IF INDIRIZZO-C-COM NOT = SPACES                                      
129200        MOVE INDIRIZZO-C-COM TO RAG-SOC                                   
129300        PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1                     
129400        MOVE LOCALITA-C-COM TO LOCALITA-TEST                              
129500        MOVE CAP-C-COM TO CAP-TEST                                        
129600        MOVE PROV-C-COM TO PROV-TEST                                      
129700        MOVE STATO-COM TO STATO-TEST                                      
129800       ELSE                                                               
129900          MOVE 1 TO N-RIGA-STAMPA.                                        
130000     IF NOT VENDITA-DA-TERZI                                              
130100       MOVE AA-MM-GG-DDT TO DATA-BOLLA-COM                                
130200       MOVE CORR DATA-B-COM TO DATA-B-COM-R                               
130300       MOVE DATA-BOLLA-COM-R TO DATA-BOLLA                                
130400       MOVE NUMERO-DDT TO NUMERO-BOLLA.                                   
130500     IF VENDITA-MAX-E-CO OR RESO-MAX-E-CO                                 
130600        MOVE "/P" TO BARRA-C.                                             
130700     IF VENDITA-CONFEZIONI OR TRASFERIMENTO-CONFEZIONI                    
130800             OR VENDITA-CONTR-EST                                         
130900        MOVE "/C" TO BARRA-C.                                             
131000     MOVE IND-PAG TO NUM-PAG.                                             
131100     PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                       
131200     MOVE 1 TO N-RIGA-STAMPA.                                             
131300     IF VENDITA-MAX-E-CO                                                  
131400          OR VENDITA OR VENDITA-DA-TERZI                                  
131500                OR VENDITA-CONFEZIONI                                     
131600        MOVE "VENDITA" TO CAUSALE-T.                                      
131700     IF VENDITA-CONTR-EST                                                 
131800        MOVE "CONSEGNA IN CONTR. ESTIMATORIO" TO CAUSALE-T.               
131900     IF C-VISIONE                                                         
132000        MOVE "C/VISIONE" TO CAUSALE-T.                                    
132100     IF C-LAVAGGIO                                                        
132200        MOVE "C/LAVAGGIO" TO CAUSALE-T.                                   
132300     IF TRASFERIMENTO OR TRASFERIMENTO-CONFEZIONI                         
132400        MOVE "TRASFERIMENTO" TO CAUSALE-T.                                
132500     IF PREBOLLA                                                          
132600        MOVE "PREBOLLA" TO CAUSALE-T.                                     
132700     IF RESO-F OR RESO-MAX-E-CO                                           
132800        MOVE "RESO A FORNITORE" TO CAUSALE-T.                             
132900     IF C-RIPARAZIONE                                                     
133000        MOVE "C/RIPARZIONE" TO CAUSALE-T.                                 
133100*DTALLO*                                                                  
133200     IF TRASFERIMENTO-DT                                                  
133300       MOVE "TRASFERIMENTO" TO CAUSALE-T.                                 
           IF CLIENTE-DDT = 10000852 OR = 10000853
               MOVE "TRASFERIMENTO CONTO DEPOSITO" TO CAUSALE-T.
133400*                                                                         
133500     IF DOC-DDT                                                           
133600        MOVE "D.d.T. D.P.R. 472/96" TO TIPO-DOC-DDT-T.                    
133700     IF DOC-NOT-DDT                                                       
133800        MOVE "DOCUMENTO INTERNO" TO TIPO-DOC-DDT-T.                       
133900     PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.                       
134000     MOVE 2 TO N-RIGA-STAMPA.                                             
134100*NODE*                                                                    
134200     IF IND-PAG < 2 AND (RIGA-1-DDT NOT = SPACE OR                        
134300        RIGA-2-DDT NOT = SPACE)                                           
134400       PERFORM STAMPA-PRIME-2-RIGHE                                       
134500            THRU EX-STAMPA-PRIME-2-RIGHE                                  
134600     END-IF.                                                              
134700*
      *etich*
           if ind-pag < 2
             MOVE "    Etichettatura se dovuta, ex D.Lgs. 194/99"                         
                     TO DATI-RIGA                      
             PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1
             move 2 to ind-riga
           else
             move 1 to ind-riga.             
      *
134800     MOVE "    CAPI DI ABBIGL. ESTERNO FEMMINILE"                         
134900               TO DATI-RIGA.                                              
135000     MOVE "  T1 T2 T3 T4 T5 T6 T7 T8 T9T10" TO RIGA-TEST-TG.                        
135100     PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1.
      *etich*
135200*    MOVE 1 TO IND-RIGA.                                                  
135300 EX-STAMPA-TESTATA.                                                       
135400     EXIT.                                                                
135500*                                                                         
135600*                                                                         
135700*                                                                         
135800*NODE*                                                                    
135900 STAMPA-PRIME-2-RIGHE.                                                    
136000     IF RIGA-1-DDT NOT = SPACE                                            
136100       STRING "    "  RIGA-1-DDT                                          
136200          DELIMITED BY SIZE                                               
136300          INTO DATI-RIGA                                                  
136400       PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1                      
136500       ADD 1 TO IND-RIGA                                                  
136600     END-IF.                                                              
136700     IF RIGA-2-DDT NOT = SPACE                                            
136800       STRING "    "  RIGA-2-DDT                                          
136900          DELIMITED BY SIZE                                               
137000          INTO DATI-RIGA                                                  
137100       PERFORM CALL-QWPRINT-1 THRU EX-CALL-QWPRINT-1                      
137200       ADD 1 TO IND-RIGA                                                  
137300     END-IF.                                                              
137400 EX-STAMPA-PRIME-2-RIGHE.                                                 
137500     EXIT.                                                                
137600*                                                                         
137700*                                                                         
137800*                                                                         
137900 BEGIN-RC.                                                                
138000     EXEC SQL                                                             
138100        BEGIN WORK RC                                                     
138200     END-EXEC                                                             
138300     MOVE "BEGIN WORK RC" TO ER-DESCRIZIONE                               
138400     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
138500 BEGIN-RC-EX.                                                             
138600     EXIT.                                                                
138700*                                                                         
138800*                                                                         
138900 S-COMMIT.                                                                  
139000     EXEC SQL                                                             
139100        COMMIT WORK                                                       
139200     END-EXEC.                                                            
139300     MOVE "COMMIT WORK" TO ER-DESCRIZIONE                                 
139400     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
139500 S-COMMIT-EX.                                                               
139600     EXIT.                                                                
139700*                                                                         
139800 TEST-ERR.                                                                
139900     MOVE SQLCODE TO SQL-STATUS.                                          
140000     IF SQLCODE = OK OR NO-MEMORY OR DEADLOCK OR NOT-FOUND                
140100        CONTINUE                                                          
140200     ELSE                                                                 
           CANCEL "CALLSQLE"
140300        CALL "CALLSQLE" USING SQLCA PAR-ERR AREA-HL AREA-SI.              
140400 TEST-ERR-EX.                                                             
140500     EXIT.                                                                
140600*                                                                         
140700*                                                                         
140800 TTDBGET.                                                                 
140900     COPY PDBGET.                                                         
141000*                                                                         
141100*                                                                         
141200 TTDBFIND.                                                                
141300     COPY PDBFIND.                                                        
141400*                                                                         
141500*                                                                         
141600*DTALLO*                                                                  
141700 TTDBGET-S. COPY PDBGET REPLACING                                         
141800       AREA-REC-SET BY SETTORE                                            
141900       EX-TTDBGET BY EX-TTDBGET-S.                                        
142000 TTUPDATE-S. COPY PDBUPDAT REPLACING                                      
142100       AREA-REC-SET BY SETTORE                                            
142200       EX-TTUPDATE BY EX-TTUPDATE-S.                                      
142300*                                                                         
142400*                                                                         
142500*                                                                         
142600*SELECT-MIRATA*                                                           
142700 DECLARE-CURS-MOD.                                                        
142800     EXEC SQL                                                             
142900       DECLARE CURSMOD CURSOR FOR                                         

      *NO-DATGE*                                                        inizio
143000*           SELECT MODELLO_NEW,                                            
143100*                  NOME,                                                   
143200*                  SOCIETA,                                                
143300*                  CLASSE_MMFG,                                            
143400*                  MODELLO_MAXIMA,                                         
143500*                  NUM_PEZZI,                                              
143600*                  COD_ESTENSIONE,                                         
143700*                  IVA                                                     
143800*             FROM ANAMAT.MODELLI                                          
143900*            WHERE                                                         
144000*           SOCIETA         = :MODELLI-SOCIETA AND                         
144100*           COD_ESTENSIONE  = :MODELLI-ESTENSIONE AND                      
144200*           MODELLO_MAXIMA  = :MODELLI-MODELLO-MAXIMA   
       
            SELECT B.modello_dt,  
                    A.nome, 
                    A.classe, 
                    L.desc_classe,
                    N.CS_IVA
            
            FROM  anagrafica_modelli_dbg A 
               JOIN anagrafica_modelli_barcode_negozio_dbg B 
                 ON (A.societa = B.societa)
                AND(A.modello = B.modello)
               JOIN CLASSI L USING (classe)
               JOIN ANAMAT N ON (B.modello_dt = C_MAT)

           WHERE
               B.societa = :CC-SOC 
               AND B.modello_dt = :CC-C-MAT  
               AND f_anagrafica_modello_base = 1

      *NO-DATGE*                                                        FINE
      
144300     END-EXEC.                                                            
144400 DECLARE-CURS-MOD-EX.                                                     
144500     EXIT.                                                                
144600*                                                                         
144700*                                                                         
144800 OPEN-CURS-MOD.                                                           
144900     EXEC SQL                                                             
145000          OPEN CURSMOD KEEP CURSOR                                        
145100     END-EXEC.                                                            
145200     MOVE 'OPEN-CURS-MOD' TO ER-DESCRIZIONE.                              
145300     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
145400 OPEN-CURS-MOD-EX.                                                        
145500     EXIT.                                                                
145600*                                                                         
145700*                                                                         
145800 FETCH-SINGOLA-CURS-MOD.                                                  
145900     EXEC SQL                                                             
146000       FETCH CURSMOD                                                      
      *NO-DATGE*                                                        inizio
146100*       INTO :MODELLI-MODELLO-NEW,                                         
146200*            :MODELLI-NOME,                                                
146300*            :MODELLI-SOCIETA,                                             
146400*            :MODELLI-CLASSE,                                              
146500*            :MODELLI-MODELLO-MAXIMA,                                      
146600*            :MODELLI-NUM-PEZZI,                                           
146700*            :MODELLI-ESTENSIONE,                                          
146800*            :MODELLI-COD-IVA                                              

              INTO  :CC-MODELLO-DT,
                    :CC-NOME,
                    :CC-CLASSE,
                    :CC-DESCRIZIONE,
                    :CC-IVA
146900     END-EXEC.                                                            
147000*               
      *   display "fetch-singola "
      *         " cc-soc=" cc-soc
      *         " cc-c-mat" cc-c-mat
      *         " / "
      *   display "    cc-modello-dt=" cc-modello-dt
      *   display "    cc-nome=" cc-nome
      *   display "    cc-classe=" cc-classe
      *   display "    cc-descrizione=" cc-descrizione
      *   display "    cc-iva=" cc-iva
      *                           
           IF SQLCODE     = OK  SET VAI-CON-SQL   TO TRUE.                                                 
           IF SQLCODE NOT = OK  SET VAI-CON-IMAGE TO TRUE.                                                
      *NO-DATGE*                                                        FINE
                                                          
147100     IF SQLCODE NOT = OK                                                  
147200       MOVE 1 TO FLAG-CURSORE                                             
147300     END-IF.                                                              
147400 FETCH-SINGOLA-CURS-MOD-EX.                                               
147500     EXIT.                                                                
147600*                                                                         
147700*                                                                         
147800 CLOSE-CURS-MOD.                                                          
147900     EXEC SQL                                                             
148000          CLOSE CURSMOD                                                   
148100     END-EXEC.                                                            
148200     MOVE 'CLOSE-CURS-MOD' TO ER-DESCRIZIONE.                             
148300     PERFORM TEST-ERR THRU TEST-ERR-EX.                                   
148400 CLOSE-CURS-MOD-EX.                                                       
148500     EXIT.                                                                
148600*                                                                         
148700                                                                         
      
      
      *NO-DATGE*                                                        inizio
       VUOTO.
       EX-VUOTO. EXIT.
      *NO-DATGE*                                                        fine

      
      
      
