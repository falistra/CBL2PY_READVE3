* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page   1
* READVE3.cob
* Options: int(READVE3.int) anim csi verbose nolist endp list(READVE3.lst)
     1*CONTROL DYNAMIC,BOUNDS
     2 IDENTIFICATION DIVISION.
     3 PROGRAM-ID. READVE3.
     4*
     5*   ricavato da READRST2 per gestire vendite da mag 3 (come fallat
     6*
     7*
     8*
     9*BOLL*
    10*   22/02/97   aggiunto mag in input e controllo su BOLLE
    11*
    12*BUDA*
    13*   17/06/97  vendite a BUDAPEST: scrive file XESTERO
    14*                                 e stampa cartellini
    15*
    16*  ricavato da READRSTK- con riduzione dei DISPLAY a 25 colonne
    17*
    18*2000*        05/08/99
    19*     tratta date a 6 cifre
    20*
    21*MAG6/7*
    22*   20/03/00   aggiunto mag di provenienza in input
    23*
    24*NOPRZ*
    25*    11/04/00   tratta mancanza prezzo al cliente
    26*               come mancanza giacenza
    27*
    28*NODMAT*     06/07/00
    29*         con il messaggio "CONFERMI MANCA PREZZO ?" visualizza
    30*         descrizione articolo precedente:  corretto errore
    31*
    32*EURO*        27/12/00                          EURO/LIRE
    33*     trattamento importi in EURO
    34*
    35*NOPRZ1*      01/03/01
    36*      scrittura di movmag con COSTO-STD = 0
    37*
    38*
    39*EURO1*       03/12/01
    40*      trattamento prezzi di vendita in Euro
    41*
    42*T5000*       05/11/02
    43*       Introdotta scelta del dispositivo di uscita
    44*
    45*PRZBU*       04/12/02
    46*      Non scrive il movmag se il COSTO-STD e' 0 oppure
    47*      senza sconto ma lo tratta come un manca giacenza
    48*
    49*FIFRA*      08/01/03
    50*     passaggio file a franchising
    51*
    52*PRODI*      12/06/03
    53*     produzione divise: abilitate vendite da mag 2
    54*     con prezzo da PREZZIA senza sconto
    55*
    56*PRZANABU*   11/05/04
    57*     se prezzo scontato = costo-anamat e se Vendita per
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page   2
* READVE3.cob
    58*     Budapest ne permette il trattamento
    59*
    60*TRAVMAG*   01/07/04
    61*  Travaso da mag 7 via F.lli Cervi a mag 7 via Santi
    62*
    63*PRZ-PUBBL*     13/LUG/2005 - LAUROS
    64*     Aggiunto il prezzo di listino al pubblico nella stampa
    65*     dei cartellini solo per il conto DEMA = 10010101
    66*     (richiesta di Edoardo Testi)
    67*
    68*PRINTDD6*      20/LUG/2005 - LAUROS
    69*     PRINTDD6 al posto di PRINTDD3
    70*     PRINTDD6 va su ANAMAT.MODELLI per ogni MODELLO
    71*     PRINTDD3 carica tutta ANAMAT.MODELLI (+ lento)
    72*
    73*TASTO-PER-CONTINUARE*  21/LUG/2005 - LAUROS
    74*     Aggiunto "premi un tasto per continuare..."
    75*
    76*etich-vuota*  17/FEB/2006  -  LAUROS
    77*     aggiunta un'etichetta vuota alla fine, per evitare che
    78*     l'ultima etichetta di fine lotto venga stampata dopo
    79*     un salto pagina... (con il passaggio a linux si comporta
    80*     in questo modo...!)
    81*
    82*volante*      04/04/07
    83*     modifica fatta per scaricare EUROSTOCK con data
    84*     22/12/06  e vecchio parametro 2006 + 1
    85*
    86*sempre0*      07/11/07
    87*     in caso di manca prezzo metto il prezzo sempre a 0
    88*     in modo da evitare fatturazioni a prezzo pieno !!
    89*
    90*FSTOCK*       10/04/08    (annullato)
    91*     nuova dicitura per fatture stock
    92*
    93*Mag3_V/F*     27/01/09    -  CAIO
    94*     per il magazzino 3, chiede all'utente se la roba e` roba
    95*     buona o fallata
    96*
    97*MAXCA*        06/11/09
    98*     aggiunto param. in input (MAX-CAPI) per limitare il numero
    99*     di capi trattati
   100*
   101*VIBLO*        10/12/09
   102*     chiede in input mag (4 o 6) e non usa pi� fisso mag 3;
   103*     non chiede valido o fallato
   104*     non chiede valido o fallato
   105*
   106*ASZERO*       06/10/10    -  CAIO
   107*     correzione bug: non distingue l'anno "0" dal dato "tutti
   108*     gli anni"
   109*
   110*VACO*         25/10/10
   111*           valorizza costo industriale (PREZZO) su MOVMAG con
   112*           COSTO di ANAMAT variante (per Elisa)
   113*
   114*ACQUO*     28/11/2011  -  LAUROS + LANDUX
   115*         acquisizione outlet esteri
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page   3
* READVE3.cob
   116*         effettua test esistenza PREZZI solo se deve stampare le
   117*
   118*MOVSKU   24/06/15
   119*       Scrive MOVSKU usando il baruni restituitogli dal py e
   120*       aggiungere baruni nella stampa cartellino
   121*
   122*ASOLOB2C*   12/04/2018
   123*     valorizza costo acquisto su MOVMAG vendita da ANAMAT_CST dis
   124*     per neg B2C (no DT 575) e altri neg
   125*
   126*ESTETA*     20/11/18
   127*      estensione taglie
   128*
   129*UNICODDT*
   130*     VALERIA NOVEMBRE 2020
   131*     poter inserire piu' as e per ogni as piu' classi e per ogni
   132*     per formare un unico ddt
   133*
   134*************************************
   135*NO-DATGE 03 DICEMBRE 2020 VALERIA
   136*     dismissione DATGE >> sostituita PF.SOCIETA con COPY MAPPASOC
   137*         sostituite anamat_modelli e anamat_pezzi
   138*         con prezzi_modelli_dbg e anagrafica_modelli_dbg
   139*
   140 ENVIRONMENT DIVISION.
   141 CONFIGURATION SECTION.
   142 SOURCE-COMPUTER.  HP-3000.
   143 OBJECT-COMPUTER.  HP-3000.
   144 SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.
   145*ESTETA*
   146 REPOSITORY.
   147      FUNCTION idxtg
   148      FUNCTION tgxid.
   149*BUDA*
   150 INPUT-OUTPUT SECTION.
   151     FILE-CONTROL.
   152     SELECT FILE-BC ASSIGN TO "BARCNEG"
   153        ORGANIZATION IS LINE SEQUENTIAL.
   154     SELECT FILE-PEND ASSIGN TO "PEND"
   155        ORGANIZATION IS LINE SEQUENTIAL.
   156*
   157 DATA DIVISION.
   158*BUDA*
   159*
   160 FILE SECTION.
   161* movsku
   162 FD FILE-BC DATA RECORD REC-BC.
   163 01 REC-BC.
   164  05 C-MAT-S             PIC 9(13).
   165  05 T-1                 PIC X.
   166  05 TAGLIA-S            PIC 9.
   167  05 T-2                 PIC X.
   168  05 MAG-S               PIC 999.
   169  05 T-3                 PIC X.
   170  05 SETTORE-S           PIC X(4).
   171  05 T-4                 PIC X.
   172  05 NOME-S              PIC X(7).
   173  05 T-5                 PIC X.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page   4
* READVE3.cob
   174  05 PREZZO-S            PIC 9(8).
   175  05 T-6                 PIC X.
   176  05 NOME-F-S            PIC X(15).
   177  05 T-7                 PIC X.
   178  05 COL-F-S             PIC X(5).
   179  05 T-8                 PIC X.
   180  05 TG-OUT-S            PIC XX.
   181  05 T-9                 PIC X.
   182  05 BARUNI-S            PIC X(13).
   183*
   184 FD FILE-PEND DATA RECORD REC-PEND.
   185 01 REC-PEND             PIC X(132).
   186*
   187 WORKING-STORAGE SECTION.
   188**** Start Inserted Statements ****
   189 01 SQL-SEL-001-X.
   190    03 FILLER PIC X(58) VALUE
   191     "SELECT P.PREZZO FROM prezzi_modelli_dbg P JOIN anagrafica_".
   192    03 FILLER PIC X(58) VALUE
   193     "modelli_dbg M ON ( M.SOCIETA = P.SOCIETA ) AND ( P.MODELLO".
   194    03 FILLER PIC X(58) VALUE
   195     " = M.MODELLO ) JOIN anagrafica_modelli_barcode_negozio_dbg".
   196    03 FILLER PIC X(58) VALUE
   197     " B ON ( B.SOCIETA = P.SOCIETA ) AND ( P.MODELLO = B.MODELL".
   198    03 FILLER PIC X(58) VALUE
   199     "O ) WHERE P.MODELLO = ? AND M.SOCIETA = ? AND P.F_LISTINO_".
   200    03 FILLER PIC X(29) VALUE
   201     "RIF = ? AND P.TIPO_PREZZO = ?".
   202 01 SQL-PARAM-001-X.
   203    03 SQL-NUMSQL-001 PIC S9(4) COMP VALUE 0.
   204    03 SQL-STEP-001 PIC S9(4) COMP VALUE 1.
   205    03 SQL-SEL-001-L PIC S9(4) COMP VALUE 319.
   206    03 SQLI-LTIPO-001 PIC S9(4) COMP VALUE 12.
   207    03 SQLI-LREC-001 PIC S9(4) COMP VALUE 20.
   208    03 SQLI-NCOL-001  PIC S9(4) COMP VALUE 4.
   209    03 SQLO-LTIPO-001 PIC S9(4) COMP VALUE 5.
   210    03 SQLO-LREC-001 PIC S9(4) COMP VALUE 8.
   211    03 SQLO-NCOL-001  PIC S9(4) COMP VALUE 1.
   212 01 SQL-SEL-002-X.
   213    03 FILLER PIC X(58) VALUE
   214     "INSERT INTO MOV_SKU VALUES( NULL, ?, ?, ?, ?, ?, ?, ?, ?, ".
   215    03 FILLER PIC X(6) VALUE
   216     "?, ? )".
   217 01 SQL-PARAM-002-X.
   218    03 SQL-NUMSQL-002 PIC S9(4) COMP VALUE 1.
   219    03 SQL-STEP-002 PIC S9(4) COMP VALUE 1.
   220    03 SQL-SEL-002-L PIC S9(4) COMP VALUE 64.
   221    03 SQLI-LTIPO-002 PIC S9(4) COMP VALUE 28.
   222    03 SQLI-LREC-002 PIC S9(4) COMP VALUE 55.
   223    03 SQLI-NCOL-002  PIC S9(4) COMP VALUE 10.
   224 01 SQL-SEL-003-X.
   225    03 FILLER PIC X(58) VALUE
   226     "SELECT NEGOZIO FROM NEGOZIO_ANAG_CATEGORIA JOIN NEGOZIO_CA".
   227    03 FILLER PIC X(58) VALUE
   228     "TEGORIA USING (ID_CATEGORIA) where DESC_CATEGORIA = 'NEGOZ".
   229    03 FILLER PIC X(34) VALUE
   230     "I_ITALIA_B2C_SOC' order by NEGOZIO".
   231 01 SQL-PARAM-003-X.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page   5
* READVE3.cob
   232    03 SQL-NUMSQL-003 PIC S9(4) COMP VALUE 2.
   233    03 SQL-STEP-003 PIC S9(4) COMP VALUE 0.
   234    03 SQL-SEL-003-L PIC S9(4) COMP VALUE 150.
   235    03 SQLI-LTIPO-003 PIC S9(4) COMP VALUE 1.
   236    03 SQLI-LREC-003 PIC S9(4) COMP VALUE 1.
   237    03 SQLI-NCOL-003  PIC S9(4) COMP VALUE 0.
   238    03 SQLO-LTIPO-003 PIC S9(4) COMP VALUE 1.
   239    03 SQLO-LREC-003 PIC S9(4) COMP VALUE 2.
   240    03 SQLO-NCOL-003  PIC S9(4) COMP VALUE 1.
   241    03 SQL-START-003 PIC S9(9) COMP.
   242    03 SQL-TIMES-003 PIC S9(9) COMP.
   243 01 SQL-SEL-004-X.
   244    03 FILLER PIC X(57) VALUE
   245     "SELECT CST_STD, CST_STD_2 FROM ANAMAT_CST WHERE C_MAT = ?".
   246 01 SQL-PARAM-004-X.
   247    03 SQL-NUMSQL-004 PIC S9(4) COMP VALUE 3.
   248    03 SQL-STEP-004 PIC S9(4) COMP VALUE 1.
   249    03 SQL-SEL-004-L PIC S9(4) COMP VALUE 57.
   250    03 SQLI-LTIPO-004 PIC S9(4) COMP VALUE 5.
   251    03 SQLI-LREC-004 PIC S9(4) COMP VALUE 8.
   252    03 SQLI-NCOL-004  PIC S9(4) COMP VALUE 1.
   253    03 SQLO-LTIPO-004 PIC S9(4) COMP VALUE 2.
   254    03 SQLO-LREC-004 PIC S9(4) COMP VALUE 8.
   255    03 SQLO-NCOL-004  PIC S9(4) COMP VALUE 2.
   256
   257 01  SQLX-PROG.
   258     05  SQL-NPROG  PIC S9(4) COMP VALUE -1.
   259     05  SQL-NUMSQL PIC S9(4) COMP VALUE 4.
   260     05  SQL-NUMCUR PIC S9(4) COMP VALUE 0.
   261     05  SQL-PROG   PIC X(8) VALUE "READVE3".
   262 01  SQLX-ISOLATION-LEVEL     PIC S9(4) COMP.
   263 01  SQLX-LOCK-TABLE          PIC X(64).
   264 01  SQLX-LOCK-MODE           PIC S9(4) COMP.
   265 01  SQLX-SETTA-KEY           PIC X.
   266 01  SQLX-SETTA-MODE          PIC S9(9) COMP.
   267 01  SQLX-LIKE-LEN            PIC S9(9) COMP.
   268 01  SQL-CONN-RECORD.
   269     05  SQL-CONN-DB          PIC X(32).
   270     05  SQL-CONN-ALIAS       PIC X(32).
   271 01 SQLI-TIPO-001-X.
   272    03 SQLI-TIPO-001-000 PIC X VALUE "3".
   273    03 SQLI-CLEN-001-000 PIC S9(4) COMP VALUE 15.
   274    03 SQLI-TIPO-001-001 PIC X VALUE "3".
   275    03 SQLI-CLEN-001-001 PIC S9(4) COMP VALUE 2.
   276    03 SQLI-TIPO-001-002 PIC X VALUE "3".
   277    03 SQLI-CLEN-001-002 PIC S9(4) COMP VALUE 2.
   278    03 SQLI-TIPO-001-003 PIC X VALUE "3".
   279    03 SQLI-CLEN-001-003 PIC S9(4) COMP VALUE 1.
   280 01 SQLO-TIPO-001-X.
   281    03 SQLO-TIPO-001-000 PIC X VALUE "2".
   282    03 SQLO-CLEN-001-000 PIC S9(4) COMP VALUE 15.
   283    03 SQLO-DEC-001-000 PIC S9(4) COMP VALUE 2.
   284 01 SQLI-TIPO-002-X.
   285    03 SQLI-TIPO-002-000 PIC X VALUE "2".
   286    03 SQLI-CLEN-002-000 PIC S9(4) COMP VALUE 15.
   287    03 SQLI-DEC-002-000 PIC S9(4) COMP VALUE 0.
   288    03 SQLI-TIPO-002-001 PIC X VALUE "2".
   289    03 SQLI-CLEN-002-001 PIC S9(4) COMP VALUE 15.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page   6
* READVE3.cob
   290    03 SQLI-DEC-002-001 PIC S9(4) COMP VALUE 0.
   291    03 SQLI-TIPO-002-002 PIC X VALUE "0".
   292    03 SQLI-TIPO-002-003 PIC X VALUE "3".
   293    03 SQLI-CLEN-002-003 PIC S9(4) COMP VALUE 13.
   294    03 SQLI-TIPO-002-004 PIC X VALUE "1".
   295    03 SQLI-TIPO-002-005 PIC X VALUE "0".
   296    03 SQLI-TIPO-002-006 PIC X VALUE "3".
   297    03 SQLI-CLEN-002-006 PIC S9(4) COMP VALUE 8.
   298    03 SQLI-TIPO-002-007 PIC X VALUE "3".
   299    03 SQLI-CLEN-002-007 PIC S9(4) COMP VALUE 1.
   300    03 SQLI-TIPO-002-008 PIC X VALUE "3".
   301    03 SQLI-CLEN-002-008 PIC S9(4) COMP VALUE 1.
   302    03 SQLI-TIPO-002-009 PIC X VALUE "3".
   303    03 SQLI-CLEN-002-009 PIC S9(4) COMP VALUE 8.
   304 01 SQLI-TIPO-003-X.
   305    03 SQLI-TIPO-003-000 PIC X VALUE "0".
   306 01 SQLO-TIPO-003-X.
   307    03 SQLO-TIPO-003-000 PIC X VALUE "0".
   308 01 SQLI-TIPO-004-X.
   309    03 SQLI-TIPO-004-000 PIC X VALUE "2".
   310    03 SQLI-CLEN-004-000 PIC S9(4) COMP VALUE 15.
   311    03 SQLI-DEC-004-000 PIC S9(4) COMP VALUE 0.
   312 01 SQLO-TIPO-004-X.
   313    03 SQLO-TIPO-004-000 PIC X VALUE "1".
   314    03 SQLO-TIPO-004-001 PIC X VALUE "1".
   315 01 SQLO-TMP-X PIC X(8).
   316 01 SQLO-REC-001 REDEFINES SQLO-TMP-X.
   317    03 SQLO-001-000 PIC S9(13)V99 COMP-3.
   318*   03 SQLO-001-FIL PIC X(YYYY).
   319 01 SQLO-REC-003 REDEFINES SQLO-TMP-X.
   320    03 SQLO-003-000 PIC S9(4) COMP.
   321    03 SQLO-003-FIL PIC X(6).
   322 01 SQLO-REC-004 REDEFINES SQLO-TMP-X.
   323    03 SQLO-004-000 PIC S9(9) COMP.
   324    03 SQLO-004-001 PIC S9(9) COMP.
   325*   03 SQLO-004-FIL PIC X(YYYY).
   326 01 SQLI-TMP-X PIC X(55).
   327 01 SQLI-REC-001 REDEFINES SQLI-TMP-X.
   328    03 SQLI-001-000 PIC 9(15).
   329    03 SQLI-001-001 PIC XX.
   330    03 SQLI-001-002 PIC 99.
   331    03 SQLI-001-003 PIC X.
   332    03 SQLI-001-FIL PIC X(35).
   333 01 SQLI-REC-002 REDEFINES SQLI-TMP-X.
   334    03 SQLI-002-000 PIC S9(15) COMP-3.
   335    03 SQLI-002-001 PIC S9(15) COMP-3.
   336    03 SQLI-002-002 PIC S9(4) COMP.
   337    03 SQLI-002-003 PIC X(13).
   338    03 SQLI-002-004 PIC S9(8) COMP.
   339    03 SQLI-002-005 PIC S9(4) COMP.
   340    03 SQLI-002-006 PIC X(8).
   341    03 SQLI-002-007 PIC 9(1).
   342    03 SQLI-002-008 PIC 9(1).
   343    03 SQLI-002-009 PIC X(8).
   344*   03 SQLI-002-FIL PIC X(YYYY).
   345 01 SQLI-REC-003 REDEFINES SQLI-TMP-X.
   346    03 SQLI-003-000 PIC X.
   347    03 SQLI-003-FIL PIC X(54).
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page   7
* READVE3.cob
   348 01 SQLI-REC-004 REDEFINES SQLI-TMP-X.
   349    03 SQLI-004-000 PIC S9(15) COMP-3.
   350    03 SQLI-004-FIL PIC X(47).
   351**** End SQL Processor   ****
   352*
   353*ESTETA*
*  354 COPY NTG.
   355 01 NTG-NTG PIC S9(4) COMP VALUE 10.
   356 01 NTG-IN PIC S9(4) COMP.
   357 01 NTG-OUT PIC S9(4) COMP.
   358 77 NTG-MEM PIC S9(4) COMP.
   359 77 ERR-DISP               PIC -(6).
   360 77 DISP-4  PIC ZZZ9-.
   361 77 DISP-8  PIC ZZZZZZZZ-.
   362 77 STATO-DISPLAY  PIC ZZZZ-.
   363*BUDA*
   364 77 FLAG-DT-ESTERO          PIC S9(4) COMP.
   365  88 SI-DT-ESTERO    VALUE 1.
   366 77 PREZZO-MEM              PIC S9(9) COMP.
   367 77 PREZZO-TOT              PIC 9(11) COMP-3.
   368 77 IR   PIC S9(4) COMP.
   369 77 JRUNC   PIC S9(4) COMP-5 VALUE 0.
   370 77 IT      PIC S9(4) COMP.
   371 77 IC      PIC S9(4) COMP.
   372*
   373*******************************************
   374*
   375 01 CAMPI-ANAGRAFICI.
   376  05  INDIRIZZO-STD         PIC X(66).
   377  05  INDIRIZZO-COM         PIC X(60) VALUE SPACES.
   378  05  LOCALITA-COM          PIC X(60) VALUE SPACES.
   379  05  CAP-COM               PIC S9(5) COMP-3 VALUE 0.
   380  05  PROV-COM              PIC XX VALUE SPACES.
   381  05  STATO-COM             PIC XXX VALUE SPACES.
   382*
   383  05  INDIRIZZO-C-COM         PIC X(60) VALUE SPACES.
   384  05  LOCALITA-C-COM          PIC X(60) VALUE SPACES.
   385  05  CAP-C-COM               PIC S9(5) COMP-3 VALUE 0.
   386  05  PROV-C-COM              PIC XX VALUE SPACES.
   387*
   388  05 D-CONTO-MEM     PIC X(24).
   389  05 D-CONTO-AGG-MEM  PIC X(24).
   390  05 D-CONTO-VET     PIC X(24).
   391*
   392  05  INDIRIZZO-C-VET         PIC X(60) VALUE SPACES.
   393  05  LOCALITA-C-VET          PIC X(60) VALUE SPACES.
   394  05  CAP-C-VET               PIC S9(5) COMP-3 VALUE 0.
   395  05  PROV-C-VET              PIC XX VALUE SPACES.
   396*
   397 01 CAMPI-COMODO.
   398  05 RIF-BOLLA-DDT         PIC 9(12).
   399  05 FILLER REDEFINES RIF-BOLLA-DDT.
   400   10 AA-MM-GG-DDT       PIC 9(6).
   401   10 NUMERO-DDT         PIC 9(6).
   402  05 CLIENTE-DDT           PIC S9(9) COMP.
   403  05 MAGAZZINO-DDT         PIC S9(4) COMP.
   404  05 CAUSALE-DDT           PIC X(4).
   405  05 TIPO-DOC-DDT          PIC S9(4) COMP.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page   8
* READVE3.cob
   406     88 DOC-DDT      VALUE 1.
   407     88 DOC-NOT-DDT  VALUE 2.
   408  05 TIPO-MOVIMENTO-DDT    PIC S9(4) COMP.
   409     88 VENDITA       VALUE 1.
   410     88 TRASFERIMENTO VALUE 2.
   411     88 C-VISIONE     VALUE 3.
   412     88 C-LAVAGGIO    VALUE 4.
   413     88 C-LAVORAZIONE VALUE 5.
   414  05 TIPO-STAMPA-DDT       PIC S9(4) COMP.
   415     88 PRODOTTI-FINITI   VALUE 1.
   416     88 MATERIE-PRIME     VALUE 9.
   417  05 LOC-PART-DDT          PIC X(56).
   418  05 NOTE-DDT              PIC X(44) OCCURS 2.
   419  05 TIPO-DATA-SET-DDT     PIC X.
   420     88 MOVMAG-DDT    VALUE "0" , " ".
   421     88 MOVTRANS-DDT  VALUE "1".
   422*
   423 01 IMPORTO-X-PL           PIC S9(11) COMP-3.
   424*
   425*BUDA*
   426 01 FILE-FAT-DDT PIC X.
   427  88 SI-FILE-FAT VALUE "S".
   428*
   429*TRAVMAG*
   430 01 RIGA-1-DDT   PIC X(65).
   431 01 RIGA-2-DDT   PIC X(65).
   432*
   433*******************************************
   434*
   435 01  PAR-INDIRIZZO.
   436  05  STATO-IND            PIC S9(4) COMP.
   437  05  FUNZIONE-IND         PIC S9(4) COMP.
   438  05  LL-STRINGA-IND       PIC S9(4) COMP.
   439  05  LL-SUBSTRINGA-IND    PIC S9(4) COMP.
   440  05  FILLER               PIC X(6).
   441  05  TIPO-SEP             PIC XX VALUE " ;".
   442*
   443 01 AREA-REC-SET         PIC X(512).
   444*
   445 01 FILLER REDEFINES AREA-REC-SET.
   446  05 REC-ANACON
*                      COPY YANACON.
   447     .                                                            YANACON
   448*IF X7=OFF                                                        YANACON
   449*CONTROL NOLIST                                                   YANACON
   450*IF                                                               YANACON
   451*                                           ********************* YANACON
   452*          ***********************************                    YANACON
   453*          *  REC-ANACON             LL=100  *                    YANACON
   454*          ***********************************                    YANACON
   455   15 CONTO                         PIC S9(8) COMP.               YANACON
   456   15 D-CONTO                       PIC X(24).                    YANACON
   457   15 FLAGS.                                                      YANACON
   458    20 FLAG-ANA-1                   PIC X.                        YANACON
   459     88 CONTO-A-PARTITE             VALUE "1".                    YANACON
   460    20 FLAG-ANA-2                   PIC X.                        YANACON
   461     88 CONTO-A-SCADENZA            VALUE "1".                    YANACON
   462    20 FLAG-ANA-3                   PIC X.                        YANACON
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page   9
* READVE3.cob (/home/prorosa/cobol/cpy/YANACON)
   463     88 RICHIEDE-CDC                VALUE "1".                    YANACON
   464     88 RICHIEDE-FIN                VALUE "2".                    YANACON
   465     88 RICHIEDE-DIP                VALUE "3".                    YANACON
   466    20 FLAG-ANA-4                   PIC X.                        YANACON
   467     88 SPLIT-PAYMENT VALUE "A".                                  YANACON
   468    20 FLAG-ANA-5                   PIC X.                        YANACON
   469     88 FATTURAZ-ELETTRONICA VALUE "A".                           YANACON
   470    20 FLAG-ANA-6                   PIC X.                        YANACON
   471        88 SCONTO-F    VALUE "0" THRU "9".                        YANACON
   472    20 FLAG-ANA-7                   PIC X.                        YANACON
   473     88 MOSTRA-PREZZO-SELLOUT VALUE "S".
   474    20 FLAG-ANA-8                   PIC X.                        YANACON
   475     88 SCRIVI-REC-ESTERO VALUE  "3".                             YANACON
   476     88 SCRIVI-REC-RESIDUO VALUE "2".                             YANACON
   477    20 FLAG-ANA-9                   PIC X.                        YANACON
   478     88 FILIALE-DT-ESTERO VALUE  "B".                             YANACON
   479     88 BUDAPEST-KFT      VALUE  "B".                             YANACON
   480     88 SL-BRATISLAVA     VALUE  "S".                             YANACON
   481    20 FLAG-ANA-10                  PIC 9.                        YANACON
   482     88 CLI-CLE                     VALUE 1.                      YANACON
   483     88 FOI-FOE                     VALUE 2.                      YANACON
   484     88 AGI-AGE                     VALUE 3.                      YANACON
   485     88 PORTAF-I-E                  VALUE 4.                      YANACON
   486     88 PORTAF-CO-BANCHE            VALUE 5.                      YANACON
   487     88 BANCHE                      VALUE 6.                      YANACON
   488     88 CRED-DEB-DIV                VALUE 7.                      YANACON
   489     88 ALTRI-CONTI                 VALUE 8.                      YANACON
   490   15 FILLER REDEFINES FLAGS.                                     YANACON
   491    20 FLAG                         PIC X OCCURS 10.              YANACON
   492   15 P-DARE                        PIC S9(15) COMP-3.            YANACON
   493   15 P-AVERE                       PIC S9(15) COMP-3.            YANACON
   494   15 P-DARE-C                      PIC S9(15) COMP-3.            YANACON
   495   15 P-AVERE-C                     PIC S9(15) COMP-3.            YANACON
   496   15 C-RAGG                        PIC S9(8) COMP OCCURS 2.      YANACON
   497   15 FIDO                          PIC S9(4) COMP.               YANACON
   498*   in RESIDUO contiene il codice del magazzino di DIFFTESS       YANACON
   499*   corrispondente al CONTO                                       YANACON
   500   15 DATA-FIDO                     PIC S9(8) COMP.               YANACON
   501   15 DATA-NA                       PIC S9(8) COMP.               YANACON
   502   15 DATA-UM                       PIC S9(8) COMP.               YANACON
   503   15 DATA-UV                       PIC S9(8) COMP.               YANACON
   504   15 TIPO-CONTO                    PIC XX.                       YANACON
   505      88 CONT-GENERALE              VALUE "CG".                   YANACON
   506      88 CONT-INDUSTRIALE           VALUE "CI".                   YANACON
   507      88 CONT-FINANZIARIA           VALUE "CF".                   YANACON
   508   15 VALIDITA-REC                  PIC XX.                       YANACON
   509    88 ANNULLATO                    VALUE "**".                   YANACON
   510*                                                                 YANACON
   511*CONTROL LIST                                                     YANACON
   512*                                                                 YANACON
   513*                                                                 YANACON
   514*
   515 01 FILLER REDEFINES AREA-REC-SET.
   516  05 REC-BOLLE
*                        COPY YBOLLE.
   517   .                                                              YBOLLE
   518*IF X7=OFF                                                        YBOLLE
   519*CONTROL NOLIST                                                   YBOLLE
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  10
* READVE3.cob (/home/prorosa/cobol/cpy/YBOLLE)
   520*IF                                                               YBOLLE
   521*                                                                 YBOLLE
   522*            ************************************                 YBOLLE
   523*            * REC-BOLLE               LL.  22  *                 YBOLLE
   524*            ************************************                 YBOLLE
   525*                                                                 YBOLLE
   526        10 CONTO               PIC S9(9) COMP.                    YBOLLE
   527        10 RIF-INTERNO         PIC S9(15) COMP-3.                 YBOLLE
   528*                              riferimento di MOVMAG              YBOLLE
   529*                              (data scarico per vend. da neg)    YBOLLE
   530*                              (data bolla per vend. da sede )    YBOLLE
   531        10 DATA-NASCITA        PIC S9(9) COMP.                    YBOLLE
   532*                              (data bolla   per vend. da neg)    YBOLLE
   533*                              (data scarico per vend. da sede)   YBOLLE
   534        10 NUM-PRE-FATT        PIC S9(4) COMP.                    YBOLLE
   535        10 NUMERO              PIC S9(4) COMP.                    YBOLLE
   536        10 VAL-REC             PIC XX.                            YBOLLE
   537           88 REC-AGGIORNATO VALUE "**".                          YBOLLE
   538        10 MAGAZZINO           PIC S9(4) COMP.                    YBOLLE
   539*                                                                 YBOLLE
   540*CONTROL LIST                                                     YBOLLE
   541*                                                                 YBOLLE
   542*
   543 01 FILLER REDEFINES AREA-REC-SET.
   544  05 REC-ANAMAT
*                     COPY YANAMAT.
   545*                                           ********************  YANAMAT
   546*            *********************************                    YANAMAT
   547*            * REC-ANAMAT            LL:94   *                    YANAMAT
   548*            *********************************                    YANAMAT
   549     .                                                            YANAMAT
   550*IF X7=OFF                                                        YANAMAT
   551*CONTROL NOLIST                                                   YANAMAT
   552*IF                                                               YANAMAT
   553*                                                                 YANAMAT
   554     20  C-MAT                   PIC S9(15)   COMP-3.             YANAMAT
   555     20  D-MAT                   PIC X(24).                       YANAMAT
   556     20  DT-NA                  PIC S9(8)     COMP.               YANAMAT
   557     20  DT-UV                  PIC S9(8)     COMP.               YANAMAT
   558     20  P-STOCK                 PIC X(2).                        YANAMAT
   559************** P-STOCK  contiene il campo COMPOS. CLASSE          YANAMAT
   560     20  CTO-RICAVI             PIC S9(8)     COMP.               YANAMAT
   561     20  CTO-COSTI              PIC S9(9)     COMP.               YANAMAT
   562************** CTO-COSTI contiene il COSTO - lo sconto            YANAMAT
   563     20  FLAGS-A.                                                 YANAMAT
   564      25 FLAG                    PIC X  OCCURS 8.                 YANAMAT
   565     20  FLAGS-RID REDEFINES FLAGS-A.                             YANAMAT
   566      25  FLAG1                  PIC X.                           YANAMAT
   567       88  GESTIONE-1-QTA        VALUE " ", "0".                  YANAMAT
   568       88  GESTIONE-PER-TAGLIE   VALUE "1".                       YANAMAT
   569      25  FLAG2                  PIC X.                           YANAMAT
   570       88  GESTIONE-A-PEZZE      VALUE "1".                       YANAMAT
   571       88  NO-GESTIONE-A-PEZZE   VALUE " ", "0".                  YANAMAT
   572      25  FLAG3                  PIC X.                           YANAMAT
   573       88  CAPO-APPESO           VALUE "1".                       YANAMAT
   574       88  CAPO-IN-SCATOLA       VALUE "0" , " ".                 YANAMAT
   575      25  FLAG4                  PIC X.                           YANAMAT
   576************** FLAG4 contiene il numero di pezzi                  YANAMAT
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  11
* READVE3.cob (/home/prorosa/cobol/cpy/YANAMAT)
   577      25  FLAG5                  PIC X.                           YANAMAT
   578        88  CAPO-MODA VALUE "1".                                  YANAMAT
   579        88 NO-CAPO-MODA  VALUE "0" , " ".                         YANAMAT
   580        88 MAT-CONSUMO-MP VALUE "1".                              YANAMAT
   581        88 MAT-NO-CONSUMO-MP VALUE "0", " ".                      YANAMAT
   582        88 CAPO-DIVISA VALUE "D".                                 YANAMAT
   583        88 PROTOTIPO  VALUE "P".                                  YANAMAT
   584        88 PROMOZIONALE  VALUE "Z".                               YANAMAT
   585      25  FLAG6                  PIC X.                           YANAMAT
   586        88 NORMALE VALUE "0", " ".                                YANAMAT
   587        88 COMPON-TAILLEUR VALUE "1".                             YANAMAT
   588        88 TAILLEUR VALUE "2".                                    YANAMAT
   589      25 FLAG7  PIC X.                                            YANAMAT
   590        88 CAPO-ACQUISTO VALUE "1".                               YANAMAT
   591        88 CAPO-PROD     VALUE "2".                               YANAMAT
   592        88 ACCESS-ACQ    VALUE "3".                               YANAMAT
   593        88 CAPO-PROD-DIFF-TESS VALUE "5".                         YANAMAT
   594        88 CAPO-PROD-TERZI VALUE "6".                             YANAMAT
   595        88 CAPO-X-MAXIMA VALUE "1", "2", "3", "4".                YANAMAT
   596        88 ACCESS-FACON  VALUE "4".                               YANAMAT
   597        88 CAPO-ESTERNI-DIFF-TESS VALUE "E".                      YANAMAT
   598        88 CAPO-SCONTO-STOCK VALUE "E","2","5","6","4".           YANAMAT
   599      25 FLAG8  PIC X.                                            YANAMAT
   600        88 NO-CONFORM    VALUE "0" , " ".                         YANAMAT
   601        88 CONFORMATO    VALUE "1".                               YANAMAT
   602        88 MAXECO        VALUE "2".                               YANAMAT
   603**** aggiunto INTREND = 3 modelli con collezione = 4,5,6,8        YANAMAT
   604**** e societa = 5 ( non valido per riass. Maxima/MM/B2B          YANAMAT
   605**** collez 7,9 )
   606        88 INTREND       VALUE "3".                               YANAMAT
   607        88 P-BLACK       VALUE "4".                               YANAMAT
   608        88 MARELLA       VALUE "5".                               YANAMAT
   609        88 SPORTMAX      VALUE "6".                               YANAMAT
   610        88 PERSONA       VALUE "7".                               YANAMAT
   611        88 WEEK-END      VALUE "8".                               YANAMAT
   612     20  PERC-MAGG              PIC S9(4) COMP.                   YANAMAT
   613************** PERC-MAGG contiene il campo DISEGNO                YANAMAT
   614     20  ALIQ-IVA               PIC S9(4)     COMP.               YANAMAT
   615     20  CL-GR                   PIC S9(4)    COMP.               YANAMAT
   616     20  COLLEZIONE              PIC S9(4)    COMP.               YANAMAT
   617     20  ANNO                    PIC S9(4)    COMP.               YANAMAT
   618     20  STAGIONE                PIC S9(4)    COMP.               YANAMAT
   619     20  COSTO                   PIC S9(9)    COMP.               YANAMAT
   620     20  UN-MIS                  PIC X(4).                        YANAMAT
   621     20  CAT-TAGLIO              PIC XX.                          YANAMAT
   622************** CAT-TAGLIO contiene il campo COLORE MAXIMA         YANAMAT
   623     20  MATER-MAX               PIC S9(4)    COMP.               YANAMAT
   624     20  CATEG-GHELDA REDEFINES MATER-MAX PIC S9(4) COMP.         YANAMAT
   625     20  TG-BASE                 PIC S9(4)    COMP.               YANAMAT
   626     20  PRIMA-TG                PIC S9(4)    COMP.               YANAMAT
   627     20  ULTIMA-TG               PIC S9(4)    COMP.               YANAMAT
   628     20  SCORTA-IND.                                              YANAMAT
   629      25  SCORTA                 PIC X.                           YANAMAT
   630      25  IND                    PIC X.                           YANAMAT
   631     20  TIPO-MAT                PIC XX.                          YANAMAT
   632     20  VALID-REC               PIC XX.                          YANAMAT
   633*                                                                 YANAMAT
   634*CONTROL LIST                                                     YANAMAT
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  12
* READVE3.cob
   635*                                                                 YANAMAT
   636*                                                                 YANAMAT
   637*
   638*BUDA*
   639 01 REC-CONFATT
*                       COPY YCONFAT.
   640     .                                                            YCONFAT
   641*IF X7=OFF                                                        YCONFAT
   642*CONTROL NOLIST                                                   YCONFAT
   643*IF                                                               YCONFAT
   644*                                           ********************* YCONFAT
   645*          ***********************************                    YCONFAT
   646*          *  REC-CONFAT             LL=86   *                    YCONFAT
   647*          ***********************************                    YCONFAT
   648   15 CONTO                         PIC S9(8) COMP.               YCONFAT
   649   15 COND-PAG                       PIC S9(15) COMP-3.           YCONFAT
   650   15 COND-PAGAMENTO REDEFINES COND-PAG.                          YCONFAT
   651    20 CPAG                          PIC S9(5) COMP-3.            YCONFAT
   652    20 SCPAG                         PIC S9(4) COMP-3.            YCONFAT
   653    20 GGPAG                         PIC S99 COMP-3.              YCONFAT
   654   15 SC1                           PIC S9(4) COMP.               YCONFAT
   655   15 SC2                           PIC S9(4) COMP.               YCONFAT
   656   15 COD-FISCALE                    PIC X(16).                   YCONFAT
   657   15 COD-IVA                       PIC S9(9) COMP.               YCONFAT
   658   15 BANCA-APP                      PIC X(36).                   YCONFAT
   659   15 CAUSALE-IVA                   PIC S9(4) COMP.               YCONFAT
   660   15 DIVISA                         PIC X(4).                    YCONFAT
   661   15 MERCATO                       PIC S9(4) COMP.               YCONFAT
   662   15 NR-COPIE-FATT                 PIC S9(4) COMP.               YCONFAT
   663   15 LISTINO                       PIC S9(4) COMP.               YCONFAT
   664   15 VALIDITA-REC                   PIC XX.                      YCONFAT
   665*                                                                 YCONFAT
   666*CONTROL LIST                                                     YCONFAT
   667*                                                                 YCONFAT
   668*                                                                 YCONFAT
   669*
   670*BUDA*
   671 01 REC-PREZZI
*                      COPY YPREZZI.
   672     .                                                            YPREZZI
   673*IF X7=OFF                                                        YPREZZI
   674*CONTROL NOLIST                                                   YPREZZI
   675*IF                                                               YPREZZI
   676*                                          *********************  YPREZZI
   677*            *************************************                YPREZZI
   678*            * REC-PREZZI           LL:38        *                YPREZZI
   679*            *************************************                YPREZZI
   680*                                                                 YPREZZI
   681     20 C-MAT                     PIC S9(15)    COMP-3.           YPREZZI
   682     20 MERCATO                   PIC S9(4)     COMP.             YPREZZI
   683     20 DIVISA                    PIC X(4).                       YPREZZI
   684     20 MAGAZZINO                 PIC S9(4) COMP.                 YPREZZI
   685     20 PREZZO-VENDITA-SUP.                                       YPREZZI
   686      25 PREZZO-VENDITA           PIC S9(9)     COMP OCCURS 4.    YPREZZI
   687     20 DATA-ULT-AGG              PIC S9(9) COMP.                 YPREZZI
   688     20 VALID-REC                 PIC X(2).                       YPREZZI
   689*                                                                 YPREZZI
   690*CONTROL LIST                                                     YPREZZI
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  13
* READVE3.cob
   691*                                                                 YPREZZI
   692*
   693*
   694*PRZ-PUBBL*
   695 01 REC-PREZZI-PUB
*                        COPY YPREZZI.
   696     .                                                            YPREZZI
   697*IF X7=OFF                                                        YPREZZI
   698*CONTROL NOLIST                                                   YPREZZI
   699*IF                                                               YPREZZI
   700*                                          *********************  YPREZZI
   701*            *************************************                YPREZZI
   702*            * REC-PREZZI           LL:38        *                YPREZZI
   703*            *************************************                YPREZZI
   704*                                                                 YPREZZI
   705     20 C-MAT                     PIC S9(15)    COMP-3.           YPREZZI
   706     20 MERCATO                   PIC S9(4)     COMP.             YPREZZI
   707     20 DIVISA                    PIC X(4).                       YPREZZI
   708     20 MAGAZZINO                 PIC S9(4) COMP.                 YPREZZI
   709     20 PREZZO-VENDITA-SUP.                                       YPREZZI
   710      25 PREZZO-VENDITA           PIC S9(9)     COMP OCCURS 4.    YPREZZI
   711     20 DATA-ULT-AGG              PIC S9(9) COMP.                 YPREZZI
   712     20 VALID-REC                 PIC X(2).                       YPREZZI
   713*                                                                 YPREZZI
   714*CONTROL LIST                                                     YPREZZI
   715*                                                                 YPREZZI
   716*
   717*
   718 01 REC-INDIRIZZI
*                        COPY YINDIRIZ.
   719     .                                                            YINDIRIZ
   720*IF X7=OFF                                                        YINDIRIZ
   721*CONTROL NOLIST                                                   YINDIRIZ
   722*IF                                                               YINDIRIZ
   723*                                           ********************* YINDIRIZ
   724*          ***********************************                    YINDIRIZ
   725*          *  REC-INDIRIZ            LL=236  *                    YINDIRIZ
   726*          ***********************************                    YINDIRIZ
   727   15 CONTO                        PIC S9(8) COMP.                YINDIRIZ
   728   15 D-AGG                         PIC X(24).                    YINDIRIZ
   729   15 D-BANCA REDEFINES D-AGG.                                    YINDIRIZ
   730    20 D-AGG-BANCA                  PIC X(12).                    YINDIRIZ
   731    20 CC-BANCA                     PIC X(12).                    YINDIRIZ
   732   15 INDIRIZZO                     PIC X(66)   OCCURS 2.         YINDIRIZ
   733   15 TIPO-INDIRIZZO                PIC XX.                       YINDIRIZ
   734    88  IND-FATT                    VALUE " F".                   YINDIRIZ
   735    88  IND-SPED                    VALUE "S ".                   YINDIRIZ
   736    88  IND-SPED-FATT               VALUE "SF".                   YINDIRIZ
   737   15 CAP                          PIC S9(8) COMP   OCCURS 2.     YINDIRIZ
   738   15 SIGLA-PROV                    PIC XX     OCCURS 2.          YINDIRIZ
   739   15 STATO                         PIC XXXX.                     YINDIRIZ
   740   15 TELEX                         PIC S9(8) COMP.               YINDIRIZ
   741* per i clienti (da 10000001 a 10000899) assume i valori :        YINDIRIZ
   742*     0          invio anagrafica e dati a PC NEGOZIO             YINDIRIZ
   743*     99999      nessun invio                                     YINDIRIZ
   744   15 TELEFONO                       PIC S9(15) COMP-3.           YINDIRIZ
   745   15 CONTO-FATTURA                 PIC S9(8) COMP.               YINDIRIZ
   746   15 INF-COMM-INDIRIZ               PIC X(40).                   YINDIRIZ
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  14
* READVE3.cob (/home/prorosa/cobol/cpy/YINDIRIZ)
   747   15 INF-COMM-ABB  REDEFINES  INF-COMM-INDIRIZ.                  YINDIRIZ
   748    20 PRIORITA                     PIC S9(4) COMP.               YINDIRIZ
   749* nell'account RESIDUO se il valore e' 4 significa                YINDIRIZ
   750* ristampa del CARTELLINO PREZZO                                  YINDIRIZ
   751    20 PROVINCIA                    PIC S9(4) COMP.               YINDIRIZ
   752    20 CAMPO-AGE  OCCURS 3.                                       YINDIRIZ
   753     25 CONTO-PROVV                 PIC S9(8) COMP.               YINDIRIZ
   754     25 PERC-PROVV                  PIC S9(4) COMP.               YINDIRIZ
   755     25 COLLEZIONI.                                               YINDIRIZ
   756      30 COLL-VENDITA                PIC S9 COMP-3 OCCURS 6.      YINDIRIZ
   757   15 VALIDITA-REC                   PIC XX.                      YINDIRIZ
   758    88 ANNULLATO                    VALUE "**".                   YINDIRIZ
   759*                                                                 YINDIRIZ
   760*CONTROL LIST                                                     YINDIRIZ
   761*                                                                 YINDIRIZ
   762*                                                                 YINDIRIZ
   763*
   764 01 REC-SITPF
*                   COPY YSITPF.
   765*                                                                 YSITPF
   766     .                                                            YSITPF
   767*IF X7=OFF                                                        YSITPF
   768*CONTROL NOLIST                                                   YSITPF
   769*IF                                                               YSITPF
   770*                                            ******************** YSITPF
   771*            *************************************                YSITPF
   772*            * REC SITPF         LL: 224         *                YSITPF
   773*            *************************************                YSITPF
   774      15 C-MAT               PIC S9(15) COMP-3.                   YSITPF
   775      15 MAGAZZINO           PIC S9(4)  COMP.                     YSITPF
   776      15 QTA-GIAC.                                                YSITPF
   777       20  QTA-GIAC-PF        PIC S9(8) COMP
*                                                  COPY NTGOCCURS.      YSITPF
   778        OCCURS 10.
   779      15 VAL-GIAC            PIC S9(11) COMP-3.                   YSITPF
   780      15 QTA-INV.                                                 YSITPF
   781       20 QTA-INV-PF       PIC S9(8) COMP
*                                               COPY NTGOCCURS.         YSITPF
   782        OCCURS 10.
   783      15 VAL-INV             PIC S9(11) COMP-3.                   YSITPF
   784      15 DT-UM               PIC S9(8)  COMP.                     YSITPF
   785      15 DT-INV              PIC S9(8)  COMP.                     YSITPF
   786      15 QTA-ORDINATA.                                            YSITPF
   787       20 QTA-ORD           PIC S9(8) COMP
*                                                COPY NTGOCCURS.        YSITPF
   788        OCCURS 10.
   789      15 QTA-ORDINATA-C.                                          YSITPF
   790       20  QTA-ORD-C        PIC S9(8) COMP
*                                                COPY NTGOCCURS.        YSITPF
   791        OCCURS 10.
   792      15 QTA-IMPEGNATA.                                           YSITPF
   793       20  QTA-IMP          PIC S9(8) COMP
*                                                COPY NTGOCCURS.        YSITPF
   794        OCCURS 10.
   795      15 QTA-IMPEGNATA-C.                                         YSITPF
   796       20  QTA-IMP-C        PIC S9(8) COMP
*                                                COPY NTGOCCURS.        YSITPF
   797        OCCURS 10.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  15
* READVE3.cob (/home/prorosa/cobol/cpy/YSITPF)
   798      15 VAL-REC             PIC XX.                              YSITPF
   799       88 BOX-SOSPESO   VALUE "S ".                               YSITPF
   800*                                                                 YSITPF
   801*CONTROL LIST                                                     YSITPF
   802*                                                                 YSITPF
   803*                                                                 YSITPF
   804*
   805 01 MOVMAG
*                 COPY YMOVMAG.
   806     .                                                            YMOVMAG
   807*IF X7=OFF                                                        YMOVMAG
   808*CONTROL NOLIST                                                   YMOVMAG
   809*IF                                                               YMOVMAG
   810*                                           ********************* YMOVMAG
   811*            **********************************                   YMOVMAG
   812*            * REC MOVIMENTI MAGAZZINO LL: 96 *                   YMOVMAG
   813*            **********************************                   YMOVMAG
   814  14   D-MOVMAG.                                                  YMOVMAG
   815  15 RIF-INTERNO                    PIC S9(15) COMP-3.            YMOVMAG
   816  15 NUMERO-RIGA                   PIC S9(4) COMP.                YMOVMAG
   817  15  RIF-ORDINE                    PIC S9(15) COMP-3.            YMOVMAG
   818  15  RIF-BOLLA-FORN                PIC S9(15) COMP-3.            YMOVMAG
   819  15  CLI-FINALE  REDEFINES RIF-BOLLA-FORN                        YMOVMAG
   820                   PIC S9(15) COMP-3.                             YMOVMAG
   821  15  C-MAT                         PIC S9(15) COMP-3.            YMOVMAG
   822  15  CONTO                        PIC S9(8)  COMP.               YMOVMAG
   823  15  C-OPE                         PIC X(4).                     YMOVMAG
   824  15  PREZZO                          PIC S9(9) COMP.             YMOVMAG
   825  15  COSTO-STD                    PIC S9(9) COMP.                YMOVMAG
   826  15  SETTIMANA                    PIC S9(4) COMP.                YMOVMAG
   827  15  FILLER-MOVMAG                PIC S9(4) COMP.                YMOVMAG
   828  15  CATEG-GHELDA REDEFINES FILLER-MOVMAG PIC S9(4) COMP.        YMOVMAG
   829  15  MOD-IMPUTAZ                   PIC S9(15) COMP-3.            YMOVMAG
   830  15  DATA-AGG-TRASF-MOV REDEFINES                                YMOVMAG
   831                 MOD-IMPUTAZ       PIC S9(15) COMP-3.             YMOVMAG
   832  15  MAGAZZINO                        PIC S9(4) COMP.            YMOVMAG
   833  15  DIVISA                           PIC X(4).                  YMOVMAG
   834  15  UN-MIS-FATT                      PIC X(4).                  YMOVMAG
   835  15 QTA-TAGLIE.                                                  YMOVMAG
   836   20 QTA-TAGLIA               PIC S9(4) COMP
*                                                   COPY NTGOCCURS.     YMOVMAG
   837        OCCURS 10.
   838  15 QUANTITA                        PIC S9(11) COMP-3.           YMOVMAG
   839 15  VAL-REC                           PIC XX.                    YMOVMAG
   840* Questi campi di MOVTRANS hanno un significato particolare
   841* per comodita' di trattamento :   (scritto da ANASOCM)
   842*
   843*       NUMERO-RIGA    1     se CARICO
   844*                     -1     se STORNO
   845*
   846*       VAL-REC        BLANK se ORDINE NORMALE
   847*                      I     se ORDINE INSERITO
   848*                      R     se ORDINE RIASSORTIMENTO
   849*                      XX    se SPEDIZIONE di MAXIMA
   850*RESSDT*
   851*                      FR    se reso Franchising (SSDT)
   852*                      MX    se reso Maxima      (SSDT)
   853*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  16
* READVE3.cob
   854*RETOPO*
   855*                      MN    se reso ManNord
   856*
   857*       MOD-IMPUTAZ    cod societa destinazione
   858*
   859* Questi significati vengono persi nel MOVMAG
   860*                                                                 YMOVMAG
   861*CONTROL LIST                                                     YMOVMAG
   862*                                                                 YMOVMAG
   863*
   864 01 REC-PARAMETRI
*                               COPY YPARAMDT.
   865     .                                                            YPARAMDT
   866*IF X7=OFF                                                        YPARAMDT
   867*CONTROL NOLIST                                                   YPARAMDT
   868*IF                                                               YPARAMDT
   869*                                           ********************* YPARAMDT
   870*          ***********************************                    YPARAMDT
   871*          *  REC-PARAMDT            LL=150  *                    YPARAMDT
   872*          ***********************************                    YPARAMDT
   873   10   ENTRY-PARAM-DETAIL.                                       YPARAMDT
   874    15  C-AZIENDA                   PIC 9(4)  COMP.               YPARAMDT
   875    15  TIPO-PARAMETRI              PIC 9(4) COMP.                YPARAMDT
   876      88  P-ANAGRAFICI              VALUE 1.                      YPARAMDT
   877      88  P-ORDINI                  VALUE 2.                      YPARAMDT
   878      88  P-MAGAZZINO               VALUE 3.                      YPARAMDT
   879      88  P-CONTAB-GEN              VALUE 4.                      YPARAMDT
   880      88  P-CONTAB-IND              VALUE 5.                      YPARAMDT
   881      88  P-CONTAB-FIN              VALUE 6.                      YPARAMDT
   882      88  P-DISTINTA-BASE           VALUE 7.                      YPARAMDT
   883      88  P-FATTURAZIONE            VALUE 8.                      YPARAMDT
   884      88  P-PAGHE                   VALUE 9.                      YPARAMDT
   885      88  P-EXTRAMAG                VALUE 13.                     YPARAMDT
   886      88  P-PROCEDURE-CED           VALUE 15.                     YPARAMDT
   887      88  P-SUPERMAG                VALUE 23.                     YPARAMDT
   888      88  P-RIPRADI                 VALUE 55.                     YPARAMDT
   889      88  P-SERVIZI                 VALUE 99.                     YPARAMDT
   890    15  STRINGA-PARAMETRI           PIC X(144).                   YPARAMDT
   891    15  VALIDITA-REC                PIC XX.                       YPARAMDT
   892    88 ANNULLATO                    VALUE "**".                   YPARAMDT
   893*                                                                 YPARAMDT
   894*CONTROL LIST                                                     YPARAMDT
   895*                                                                 YPARAMDT
   896*                                                                 YPARAMDT
   897 01 REC-PARAM-RID REDEFINES REC-PARAMETRI.
   898  03 FILLER               PIC X(4).
   899  03 PARAM-MAG
*                            COPY WPARAM13.
   900*********  ^ Non deve essere Azzerato   ***************           WPARAM13
   901     .                                                            WPARAM13
   902*IF X9=OFF                                                        WPARAM13
   903*CONTROL NOLIST                                                   WPARAM13
   904*IF                                                               WPARAM13
   905*                                           ********************* WPARAM13
   906*            ************************************                 WPARAM13
   907*            * REC PARAMETRI MAGAZZINO  LL:144  *                 WPARAM13
   908*            ************************************                 WPARAM13
   909*                                                                 WPARAM13
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  17
* READVE3.cob (/home/prorosa/cobol/cpy/WPARAM13)
   910     30     CAMPI-FLAG-13.                                        WPARAM13
   911      35    FLAG-1                    PIC X.                      WPARAM13
   912      35    FLAG-2                    PIC X.                      WPARAM13
   913      35    FLAG-3                    PIC X.                      WPARAM13
   914      35    FLAG-4                    PIC X.                      WPARAM13
   915      35    FLAG-5                    PIC X.                      WPARAM13
   916      35    FLAG-6                    PIC X.                      WPARAM13
   917      35    FLAG-7                    PIC X.                      WPARAM13
   918      35    FLAG-8                    PIC X.                      WPARAM13
   919      35    FILLER-FLAG               PIC X(24).                  WPARAM13
   920     30     CAMPI-FLAG-RID REDEFINES CAMPI-FLAG-13.               WPARAM13
   921      35    CAMPO-FLAG                PIC X OCCURS 32             WPARAM13
   922                                      INDEXED BY I-FLAG-13.       WPARAM13
   923     30     CAMPI-DATE-13.                                        WPARAM13
   924      35    DATA-BOLLA-TAGLIO-TESSUTO       PIC S9(8) COMP.       WPARAM13
   925      35    DATA-BOLLA-RESI-PF REDEFINES                          WPARAM13
   926                                     DATA-BOLLA-TAGLIO-TESSUTO    WPARAM13
   927                                                PIC S9(8) COMP.   WPARAM13
   928      35    DATA-BOLLA-TAGLIO-INTERNI       PIC S9(8) COMP.       WPARAM13
   929      35    DATA-BOLLA-TAGLIO-FODERE        PIC S9(8) COMP.       WPARAM13
   930      35    DATA-BOLLA-MATERASSO            PIC S9(8) COMP.       WPARAM13
   931      35    DATA-TRAVASO REDEFINES DATA-BOLLA-MATERASSO           WPARAM13
   932                                                PIC S9(8) COMP.   WPARAM13
   933      35    DATA-RIEP-MAG                   PIC S9(8) COMP.       WPARAM13
   934      35    DATA-6                          PIC S9(8) COMP.       WPARAM13
   935      35    DATA-7                          PIC S9(8) COMP.       WPARAM13
   936      35    DATA-8                          PIC S9(8) COMP.       WPARAM13
   937     30     CAMPI-DATE-RID REDEFINES CAMPI-DATE-13.               WPARAM13
   938      35    CAMPO-DATA                PIC S9(8) COMP OCCURS 8     WPARAM13
   939                                      INDEXED BY I-CAMPO-DATA-13. WPARAM13
   940     30     CAMPI-NUMERI-13.                                      WPARAM13
   941      35    NUM-BOLLA-RESI-PF                   PIC S9(8) COMP.   WPARAM13
   942*********  ^ Numero di partenza  350001 ***************           WPARAM13
   943      35    NUM-BOLLA-TAGLIO-INTERNI            PIC S9(8) COMP.   WPARAM13
   944      35    NUM-BOLLA-TAGLIO-FODERE             PIC S9(8) COMP.   WPARAM13
   945      35    NUM-BOLLA-MATERASSO                 PIC S9(8) COMP.   WPARAM13
   946      35    NUM-TRAVASO REDEFINES NUM-BOLLA-MATERASSO             WPARAM13
   947                                                PIC S9(8) COMP.   WPARAM13
   948      35    NUM-BUONO-PRELIEVO                  PIC S9(8) COMP.   WPARAM13
   949      35    NUM-CONSEGNA-NO-BOLLA               PIC S9(8) COMP.   WPARAM13
   950      35    NUM-CONSEGNA-BOLLA                  PIC S9(8) COMP.   WPARAM13
   951*********  ^ Numero di partenza  090001 ***************           WPARAM13
   952      35    NUMERO-PEZZA                        PIC S9(8) COMP.   WPARAM13
   953*********  ^ Non deve essere Azzerato   ***************           WPARAM13
   954     30     CAMPI-NUMERI-RID REDEFINES CAMPI-NUMERI-13.           WPARAM13
   955      35    CAMPO-NUMERO              PIC 9(8)  COMP OCCURS 8     WPARAM13
   956                                      INDEXED BY I-CAMPO-NUM-13.  WPARAM13
   957     30     CAMPI-CHIAVE-13.                                      WPARAM13
   958      35    KEY-1                     PIC X(4).                   WPARAM13
   959      35    KEY-2                     PIC X(4).                   WPARAM13
   960      35    KEY-3                     PIC X(4).                   WPARAM13
   961      35    KEY-4                     PIC X(4).                   WPARAM13
   962     30     CAMPI-CHIAVE-RID REDEFINES CAMPI-CHIAVE-13.           WPARAM13
   963      35    CAMPO-CHIAVE              PIC X(4) OCCURS 4           WPARAM13
   964                            INDEXED BY I-CAMPO-KEY-13.            WPARAM13
   965     30    PARAMETRI-VARI             PIC X(32).                  WPARAM13
   966*                                                                 WPARAM13
   967*CONTROL LIST                                                     WPARAM13
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  18
* READVE3.cob
   968*                                                                 WPARAM13
   969*                                                                 WPARAM13
   970*
   971*volante*
   972 01 REC-PARAM-FITTIZI
*                                COPY YPARAMDT.
   973     .                                                            YPARAMDT
   974*IF X7=OFF                                                        YPARAMDT
   975*CONTROL NOLIST                                                   YPARAMDT
   976*IF                                                               YPARAMDT
   977*                                           ********************* YPARAMDT
   978*          ***********************************                    YPARAMDT
   979*          *  REC-PARAMDT            LL=150  *                    YPARAMDT
   980*          ***********************************                    YPARAMDT
   981   10   ENTRY-PARAM-DETAIL.                                       YPARAMDT
   982    15  C-AZIENDA                   PIC 9(4)  COMP.               YPARAMDT
   983    15  TIPO-PARAMETRI              PIC 9(4) COMP.                YPARAMDT
   984      88  P-ANAGRAFICI              VALUE 1.                      YPARAMDT
   985      88  P-ORDINI                  VALUE 2.                      YPARAMDT
   986      88  P-MAGAZZINO               VALUE 3.                      YPARAMDT
   987      88  P-CONTAB-GEN              VALUE 4.                      YPARAMDT
   988      88  P-CONTAB-IND              VALUE 5.                      YPARAMDT
   989      88  P-CONTAB-FIN              VALUE 6.                      YPARAMDT
   990      88  P-DISTINTA-BASE           VALUE 7.                      YPARAMDT
   991      88  P-FATTURAZIONE            VALUE 8.                      YPARAMDT
   992      88  P-PAGHE                   VALUE 9.                      YPARAMDT
   993      88  P-EXTRAMAG                VALUE 13.                     YPARAMDT
   994      88  P-PROCEDURE-CED           VALUE 15.                     YPARAMDT
   995      88  P-SUPERMAG                VALUE 23.                     YPARAMDT
   996      88  P-RIPRADI                 VALUE 55.                     YPARAMDT
   997      88  P-SERVIZI                 VALUE 99.                     YPARAMDT
   998    15  STRINGA-PARAMETRI           PIC X(144).                   YPARAMDT
   999    15  VALIDITA-REC                PIC XX.                       YPARAMDT
  1000    88 ANNULLATO                    VALUE "**".                   YPARAMDT
  1001*                                                                 YPARAMDT
  1002*CONTROL LIST                                                     YPARAMDT
  1003*                                                                 YPARAMDT
  1004*                                                                 YPARAMDT
  1005 01 REC-PARAM-FITTIZ-R REDEFINES REC-PARAM-FITTIZI.
  1006  03 FILLER               PIC X(4).
  1007  03 PARAM-RIPRADI
*                                COPY WPARAM55.
  1008     .                                                            WPARAM55
  1009*IF X9=OFF                                                        WPARAM55
  1010*CONTROL NOLIST                                                   WPARAM55
  1011*IF                                                               WPARAM55
  1012*                                           ********************* WPARAM55
  1013*            ************************************                 WPARAM55
  1014*            * REC PARAMETRI MAGAZZINO  LL:144  *                 WPARAM55
  1015*            ************************************                 WPARAM55
  1016*                                                                 WPARAM55
  1017     30     CAMPI-FLAG-55.                                        WPARAM55
  1018      35    FLAG-1                    PIC X.                      WPARAM55
  1019      35    FLAG-2                    PIC X.                      WPARAM55
  1020      35    FLAG-3                    PIC X.                      WPARAM55
  1021      35    FLAG-4                    PIC X.                      WPARAM55
  1022      35    FLAG-5                    PIC X.                      WPARAM55
  1023      35    FLAG-6                    PIC X.                      WPARAM55
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  19
* READVE3.cob (/home/prorosa/cobol/cpy/WPARAM55)
  1024      35    FLAG-7                    PIC X.                      WPARAM55
  1025      35    FLAG-8                    PIC X.                      WPARAM55
  1026      35    FILLER-FLAG               PIC X(24).                  WPARAM55
  1027     30     CAMPI-FLAG-RID REDEFINES CAMPI-FLAG-55.               WPARAM55
  1028      35    CAMPO-FLAG                PIC X OCCURS 32             WPARAM55
  1029                                      INDEXED BY I-FLAG-55.       WPARAM55
  1030     30     CAMPI-DATE-55.                                        WPARAM55
  1031      35    FILLER                          PIC S9(8) COMP.       WPARAM55
  1032      35    FILLER                          PIC S9(8) COMP.       WPARAM55
  1033      35    FILLER                          PIC S9(8) COMP.       WPARAM55
  1034      35    FILLER                          PIC S9(8) COMP.       WPARAM55
  1035      35    FILLER                          PIC S9(8) COMP.       WPARAM55
  1036      35    DATA-6                          PIC S9(8) COMP.       WPARAM55
  1037      35    DATA-7                          PIC S9(8) COMP.       WPARAM55
  1038      35    DATA-8                          PIC S9(8) COMP.       WPARAM55
  1039     30     CAMPI-DATE-RID REDEFINES CAMPI-DATE-55.               WPARAM55
  1040      35    CAMPO-DATA                PIC S9(8) COMP OCCURS 8     WPARAM55
  1041                                      INDEXED BY I-CAMPO-DATA-55. WPARAM55
  1042     30     CAMPI-NUMERI-55.                                      WPARAM55
  1043      35    PAR-PROGR-SESS                      PIC S9(8) COMP.   WPARAM55
  1044*            ****SESSIONE DI CONTROLLO***********                 WPARAM55
  1045      35    PAR-PROGR-SORTER                    PIC S9(8) COMP.   WPARAM55
  1046*            ***SETTORE LOGICO VITE ESTERNA******                 WPARAM55
  1047      35    PAR-PROGR-PRESPED                   PIC S9(8) COMP.   WPARAM55
  1048*            ***NUMERO DI PRE SPEDIZIONE*********                 WPARAM55
  1049      35    PAR-FITTIZIO-1                      PIC S9(8) COMP.   WPARAM55
  1050*            ***utilizzo di comodo      *********                 WPARAM55
  1051      35    NUM-TRASF-DA-MAG-4                  PIC S9(8) COMP.   WPARAM55
  1052      35    FILLER                              PIC S9(8) COMP.   WPARAM55
  1053      35    PAR-RESI-SS                         PIC S9(8) COMP.   WPARAM55
  1054*            ***numero rif. interno resi stock service            WPARAM55
  1055      35    FILLER                              PIC S9(8) COMP.   WPARAM55
  1056     30     CAMPI-NUMERI-RID REDEFINES CAMPI-NUMERI-55.           WPARAM55
  1057      35    CAMPO-NUMERO              PIC 9(8)  COMP OCCURS 8     WPARAM55
  1058                                      INDEXED BY I-CAMPO-NUM-55.  WPARAM55
  1059     30     CAMPI-CHIAVE-55.                                      WPARAM55
  1060      35    KEY-1                     PIC X(4).                   WPARAM55
  1061      35    KEY-2                     PIC X(4).                   WPARAM55
  1062      35    KEY-3                     PIC X(4).                   WPARAM55
  1063      35    KEY-4                     PIC X(4).                   WPARAM55
  1064     30     CAMPI-CHIAVE-RID REDEFINES CAMPI-CHIAVE-55.           WPARAM55
  1065      35    CAMPO-CHIAVE              PIC X(4) OCCURS 4           WPARAM55
  1066                            INDEXED BY I-CAMPO-KEY-55.            WPARAM55
  1067     30    PARAMETRI-VARI             PIC X(32).                  WPARAM55
  1068*                                                                 WPARAM55
  1069*CONTROL LIST                                                     WPARAM55
  1070*                                                                 WPARAM55
  1071*                                                                 WPARAM55
  1072*
  1073 01 RIGA-PENDENTI.
  1074  03 ANTE-PRIMA-RIGA.
  1075    10 FILLER        PIC X(9).
  1076    10 NUMERO-P      PIC 9(6).
  1077    10 FILLER        PIC X(18).
  1078  03 PRIMA-RIGA.
  1079    10 FILLER        PIC X(9).
  1080    10 CODICE-P      PIC 9(14).
  1081    10 FILLER        PIC X(10).
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  20
* READVE3.cob
  1082  03 SECONDA-RIGA.
  1083    10 FILLER        PIC X(9).
  1084    10 NOME-P        PIC X(14).
  1085    10 FILLER        PIC X(10).
  1086*EURO*
  1087*
  1088  03 TERZA-RIGA.
  1089    10 DIV-LIT       PIC X(9).
  1090    10 PREZZO-P      PIC ZZ.ZZZ.ZZZ.ZZZ.
  1091    10 FILLER        PIC X(10).
  1092*
  1093  03 QUARTA-RIGA.
  1094    10 DIV-EUR       PIC X(11).
  1095    10 PREZZO-P-E    PIC Z(7),ZZ.
  1096    10 FILLER        PIC X(12).
  1097*
  1098*BUDA*
  1099 01 CAMPI-X-WRITE.
  1100  05 RIF-INTR-WR        PIC 9(12).
  1101  05 CONTO-CLI-WR       PIC S9(9) COMP.
  1102  05 CONTO-DEST-WR      PIC S9(9) COMP.
  1103  05 MAGAZZINO-WR       PIC S9(4) COMP.
  1104  05 DEST-WR            PIC S9(4) COMP.
  1105     88 X-ESTERO     VALUE 3.
  1106     88 X-RESIDUO    VALUE 2.
  1107  05 DIVISA-WR          PIC XXXX.
  1108  05 LISTINO-WR         PIC 9(4).
  1109  05 CAUSALE-WR         PIC X(4).
  1110*
  1111 01 DATA-CARICO-WR      PIC 9(6).
  1112*
  1113*
  1114*BUDA*
  1115*
  1116 01 LISTINO-MEM      PIC 9999.
  1117 01 DIVISA-MEM    PIC X(4).
  1118*
  1119*PRODI*
  1120 01 CAMBIO-MEM   PIC S9(9) COMP.
  1121*
  1122 01 CAMPI-UTILI.
  1123  05 OK-GIAC           PIC S9(4) COMP.
  1124     88 GIAC-OK        VALUE 1.
  1125*NOPRZ*
  1126  05 OK-PREZZO         PIC S9(4) COMP.
  1127     88 PREZZO-OK      VALUE 1.
  1128*
  1129  05 CONTA-PAGINE          PIC S9(4) COMP.
  1130  05 TOT-CAPI-NO-GIAC      PIC 9(4).
  1131*
  1132  05 CONTA-RIGHE           PIC S9(4) COMP.
  1133  05 USCITA-PROGRAMMA      PIC S9(4) COMP.
  1134  05 RISP-NO-GIAC        PIC X.
  1135*NOPRZ*
  1136  05 RISP-NO-PREZZO      PIC X.
  1137*BUDA*
  1138  05 FLAG-ANACON       PIC X.
  1139  05 SOC-COM              PIC 99.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  21
* READVE3.cob
  1140  05 FILLER REDEFINES SOC-COM.
  1141   10 PRE-SOC       PIC 9.
  1142   10 SOC-REALE     PIC 9.
  1143  05 TROVATO-GIAC    PIC S9(4) COMP.
  1144     88 GIAC-TROVATA   VALUE 1.
  1145  05 NUM-BOLLE             PIC S9(8) COMP.
  1146  05 IND-CAPI-LETTI        PIC S9(4) COMP.
  1147  05 TOT-BOLLA-C              PIC S9(4) COMP.
  1148  05 TOT-CAPI-RIGA-C              PIC S9(4) COMP.
  1149  05 IND-PAG        PIC S9(4) COMP.
  1150  05 ITB-T          PIC S9(4) COMP.
  1151  05 ITB-T-5        PIC S9(4) COMP.
  1152  05 IND-CL          PIC S9(4) COMP.
  1153  05 VARIAZIONE        PIC S9(4) COMP.
  1154    88 FINE-VARIAZIONI   VALUE 1.
  1155  05 INDIRIZZO-DPARAM    PIC S9(9) COMP.
  1156  05 D-MAT-MEM       PIC X(24).
  1157  05 VAL-REC-MEM  PIC XX.
  1158  05 IND-RIGA        PIC S9(4) COMP.
  1159  05 CNTR-ANAMAT      PIC S9(4) COMP.
  1160     88 C-MAT-OK   VALUE 1.
  1161  05 DISP-15         PIC 9(15).
  1162  05 DISP-3          PIC 999.
  1163*BUDA*
  1164  05 TG-CAL          PIC 99.
  1165  05 PTG-MEM         PIC S9(4) COMP.
  1166  05 TAGLIO-MEM      PIC S9(4) COMP.
  1167  05 IND-FILE           PIC 9(6).
  1168  05 COD-REC      PIC S9(4) COMP.
  1169  05 ESIST-VECCHIO   PIC S9(4) COMP.
  1170     88 VECCHIO-ESIST VALUE 1.
  1171  05 OK-O-N          PIC S9(4) COMP.
  1172     88 O-N-OK VALUE 1.
  1173  05 OK-NEG          PIC S9(4) COMP.
  1174     88 NEG-OK VALUE 1.
  1175  05 CONT                 PIC S9(4) COMP.
  1176  05 CONT-D               PIC ZZZZ.
  1177  05 PREZZO-D             PIC Z(6)9,99.
  1178  05 PREZZO-TOT-D         PIC ZZZ.ZZZ.ZZ9,99.
  1179  05 FINITO             PIC S9(4) COMP.
  1180     88 FINE-FILE VALUE 1.
  1181  05 IND-4              PIC S9(4) COMP.
  1182  05 CONFERMA-STORNO PIC XX.
  1183  05 C-MAT-MEM  PIC 9(15).
  1184*T5000*
  1185  05 USCITA-DEVICE  PIC S9(4) COMP.
  1186     88 DEV-OK VALUE 1.
  1187*
  1188*
  1189 01 COL-COM.
  1190  05 PRE-COL         PIC 9.
  1191  05 COL-VAR         PIC 99.
  1192 01 COL-COM-R REDEFINES COL-COM  PIC 999.
  1193*
  1194*2000*
  1195*01 DATA-BOLLA-COM        PIC 9(6).
  1196*01 FILLER REDEFINES DATA-BOLLA-COM.
  1197* 05 AA-COM           PIC 99.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  22
* READVE3.cob
  1198* 05 MM-COM           PIC 99.
  1199* 05 GG-COM           PIC 99.
  1200*
  1201*
  1202*2000*
  1203*01 RIF-INTR.
  1204*  05 AA-MM-GG PIC 9(6).
  1205*  05 NUMERO PIC 9(6).
  1206*01 RIF-INTR-RID REDEFINES RIF-INTR    PIC 9(12).
  1207*
  1208*2000*
  1209*01 DATA-BOLLA-COM-R          PIC 9(6).
  1210*01 FILLER REDEFINES DATA-BOLLA-COM-R.
  1211* 05 GG-COM-R         PIC 99.
  1212* 05 MM-COM-R         PIC 99.
  1213* 05 AA-COM-R         PIC 99.
  1214*
  1215*
  1216 01 PARQDATA
*                   COPY QPARDATS.
  1217*                                           ********************* QPARDATS
  1218     .                                                            QPARDATS
  1219*IF X9=OFF                                                        QPARDATS
  1220*CONTROL NOLIST                                                   QPARDATS
  1221*IF                                                               QPARDATS
  1222   05  Q-PARAMETRI-DATA.                                          QPARDATS
  1223    10  Q-DATA-E.                                                 QPARDATS
  1224     15  Q-DATA.                                                  QPARDATS
  1225      20 Q-GG                  PIC 99.                            QPARDATS
  1226      20 Q-MM                  PIC 99.                            QPARDATS
  1227      20 Q-AA                  PIC 99.                            QPARDATS
  1228     15 Q-DATA-9 REDEFINES Q-DATA   PIC 9(6).                     QPARDATS
  1229     15 FILLER                 PIC X(6).                          QPARDATS
  1230    10  FILLER REDEFINES Q-DATA-E.                                QPARDATS
  1231     15 Q-DATA-EE.                                                QPARDATS
  1232      20 Q-GG-S                PIC XXX.                           QPARDATS
  1233      20 Q-MM-S                PIC X(4).                          QPARDATS
  1234      20 Q-AA-S                PIC X(4).                          QPARDATS
  1235     15 FILLER                 PIC X.                             QPARDATS
  1236*                                                                 QPARDATS
  1237   05  Q-DATA-I                PIC S9(8)   COMP.                  QPARDATS
  1238*                                                                 QPARDATS
  1239   05  Q-SETTIMANA             PIC S9(4)   COMP.                  QPARDATS
  1240*                                                                 QPARDATS
  1241*                                                                 QPARDATS
  1242*     FUNZ. = 1  Q-DATA-E  IN  Q-DATA-I                           QPARDATS
  1243*     FUNZ. = 2  Q-DATA-I  IN  Q-DATA-E                           QPARDATS
  1244*     FUNZ. = 3  Q-DATA-I  IN  Q-DATA-EE                          QPARDATS
  1245*     STATO = 0  TUTTO OK                                         QPARDATS
  1246*     STATO = -1 ERR MESE                                         QPARDATS
  1247*     STATO = -2 ERR GIORNO                                       QPARDATS
  1248*     STATO = -3 ERR MESE PER FUNZIONE = 3                        QPARDATS
  1249*     STATO = -4 ERR CAMPI NON NUMERICI                           QPARDATS
  1250*                                                                 QPARDATS
  1251******************************                                    QPARDATS
  1252* La funzione 2 serve solo per capovolgere una data dal           QPARDATS
  1253* formato AAMMGG al formato GGMMAA non esegue controlli           QPARDATS
  1254* per fare un controllo bisogna prima metterla in formato         QPARDATS
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  23
* READVE3.cob
  1255* GGMMAA poi eseguire il controllo con la funzione 1 poi          QPARDATS
  1256* riportarla nel formato AAMMGG                                   QPARDATS
  1257*                                                                 QPARDATS
  1258*CONTROL LIST                                                     QPARDATS
  1259 01 PARGEN
*                   COPY QPARGEN.
  1260*                                           ********************* QPARGEN
  1261     .                                                            QPARGEN
  1262*IF X9=OFF                                                        QPARGEN
  1263*CONTROL NOLIST                                                   QPARGEN
  1264*IF                                                               QPARGEN
  1265   05  Q-PARAMETRI-GEN.                                           QPARGEN
  1266    10  Q-STATO              PIC S9(4)    COMP.                   QPARGEN
  1267    10  Q-FUNZIONE           PIC  9(4)    COMP.                   QPARGEN
  1268    10  Q-PARAMETRO-2        PIC  9(4)    COMP.                   QPARGEN
  1269    10  Q-PARAMETRO-3        PIC  9(4)    COMP.                   QPARGEN
  1270    10  Q-PARAMETRO-4        PIC  9(4)    COMP.                   QPARGEN
  1271    10  Q-PARAMETRO-5        PIC  9(4)    COMP.                   QPARGEN
  1272    10  Q-PARAMETRO-6        PIC  9(4)    COMP.                   QPARGEN
  1273    10  Q-PARAMETRO-7        PIC  XX.                             QPARGEN
  1274    10  FILLER               PIC  XX.                             QPARGEN
  1275*                                                                 QPARGEN
  1276*CONTROL LIST                                                     QPARGEN
  1277*                                                                 QPARGEN
  1278 01 PARAGGPF
*                   COPY PARAGGPF.
  1279*                                                                 PARAGGPF
  1280     .                                                            PARAGGPF
  1281*IF X9=OFF                                                        PARAGGPF
  1282*CONTROL NOLIST                                                   PARAGGPF
  1283*IF                                                               PARAGGPF
  1284     20  C-MAT                   PIC S9(15) COMP-3.               PARAGGPF
  1285     20  MAGAZZINO               PIC S9(4) COMP.                  PARAGGPF
  1286     20  VALORE                  PIC S9(9) COMP.                  PARAGGPF
  1287     20  QTA-8.                                                   PARAGGPF
  1288      25  QTA         PIC S9(4) COMP
*                                          COPY NTGOCCURS.              PARAGGPF
  1289        OCCURS 10.
  1290     20  FLAGS-AGGIORNAMENTO.                                     PARAGGPF
  1291      25  F-GIAC                 PIC S9(4) COMP.                  PARAGGPF
  1292             88  NO-GIAC         VALUE 0.                         PARAGGPF
  1293      25  F-QTA-ORD              PIC S9(4) COMP.                  PARAGGPF
  1294             88  NO-QTA-ORD      VALUE 0.                         PARAGGPF
  1295      25  F-QTA-ORD-C            PIC S9(4) COMP.                  PARAGGPF
  1296             88  NO-QTA-ORD-C    VALUE 0.                         PARAGGPF
  1297      25  F-QTA-IMP              PIC S9(4) COMP.                  PARAGGPF
  1298             88  NO-QTA-IMP      VALUE 0.                         PARAGGPF
  1299      25  F-QTA-IMP-C            PIC S9(4) COMP.                  PARAGGPF
  1300             88  NO-QTA-IMP-C    VALUE 0.                         PARAGGPF
  1301*                                                                 PARAGGPF
  1302*CONTROL LIST                                                     PARAGGPF
  1303*                                                                 PARAGGPF
  1304*                                                                 PARAGGPF
  1305*
  1306 01 CODICE-CONTO PIC 9(8).
  1307 01 CODICE-CONTO-R REDEFINES CODICE-CONTO.
  1308   05 CAPO-CONTO PIC 9(3).
  1309   05 SOTTO-CONTO PIC 9(5).
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  24
* READVE3.cob
  1310   05 SOTTO-CONTO-R REDEFINES SOTTO-CONTO.
  1311     10 SOTTO-CONTO-3 PIC 999.
  1312     10 SOTTO-CONTO-2 PIC 99.
  1313*
  1314*
  1315 01 TABELLA-SINGOLI.
  1316*MOVSKU
  1317*  05 ELEM-TAB-SING     PIC X(23) OCCURS 9999.
  1318  05 ELEM-TAB-SING     PIC X(36) OCCURS 9999.
  1319*
  1320 01 ELEMENTO-SINGOLI.
  1321*  05 CONT-SING         PIC S9(4) COMP.
  1322  05 C-MAT-SING        PIC S9(15) COMP-3.
  1323  05 CONT-SING         PIC S9(4) COMP.
  1324  05 D-MAT-SING        PIC X(7).
  1325  05 PREZZO-SING       PIC S9(9) COMP.
  1326  05 PRIMA-TG-SING     PIC S9(4) COMP.
  1327*MOVSKU
  1328  05 SKU-SING          PIC X(13).
  1329*
  1330 01 PARTAB-SING
*                           COPY QPARTAB.
  1331*                                ******************************   QPARTAB
  1332     .                                                            QPARTAB
  1333*IF X9=OFF                                                        QPARTAB
  1334*CONTROL NOLIST                                                   QPARTAB
  1335*IF                                                               QPARTAB
  1336   05 QT-PARAMETRI.                                               QPARTAB
  1337    10 QT-STATO               PIC S9(4) COMP.                     QPARTAB
  1338    10 QT-NUM-ELEM-EFF        PIC  9(4) COMP.                     QPARTAB
  1339    10 QT-NUM-ELEM-MAX        PIC  9(4) COMP.                     QPARTAB
  1340    10 QT-LL-ELEM             PIC  9(4) COMP.                     QPARTAB
  1341    10 QT-ADDR-KEY            PIC  9(4) COMP.                     QPARTAB
  1342    10 QT-LL-KEY              PIC  9(4) COMP.                     QPARTAB
  1343    10 QT-INDEX-ELEM          PIC  9(4) COMP.                     QPARTAB
  1344    10 QT-FUNZIONE            PIC  XX.                            QPARTAB
  1345*                                                                 QPARTAB
  1346*CONTROL LIST                                                     QPARTAB
  1347*                                                                 QPARTAB
  1348*
  1349 01  DATI-INPUT.
  1350  05 CONTO-IN         PIC X(8).
  1351  05 CONTO-IN-R REDEFINES CONTO-IN   PIC 9(8).
  1352*PRZ-PUBBL*
  1353    88 CONTO-DEMA     VALUES ARE 10010101 10010362.
  1354*
  1355  05 CONTO-FINE REDEFINES CONTO-IN   PIC XXX.
  1356*BUDA*
  1357  05 FILLER REDEFINES CONTO-IN.
  1358    10 FILLER         PIC XXX.
  1359    10 FILLER         PIC XX.
  1360    10 NEG-IN         PIC 999.
  1361*
  1362  05 COD-IN
*                   COPY DANCODBC.
  1363*                                                                 DANCODBC
  1364     .                                                            DANCODBC
  1365*IF X9=OFF                                                        DANCODBC
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  25
* READVE3.cob (/home/prorosa/cobol/cpy/DANCODBC)
  1366*CONTROL NOLIST                                                   DANCODBC
  1367*IF                                                               DANCODBC
  1368*                                                                 DANCODBC
  1369*         *****************************************               DANCODBC
  1370*         *  DESCRIZIONE CODICE ARTICOLO A BARRE  *               DANCODBC
  1371*         *****************************************               DANCODBC
  1372*                                                                 DANCODBC
  1373     15 C-MAT-A-BARRE.                                            DANCODBC
  1374      20 MODELLO               PIC 9(7).                          DANCODBC
  1375      20 MOD-RID REDEFINES MODELLO.                               DANCODBC
  1376       25 MARCHIO              PIC 9.                             DANCODBC
  1377       25 CLASSE               PIC 99.                            DANCODBC
  1378       25 STAGIONE             PIC 9.                             DANCODBC
  1379       25 PROGR-ART            PIC 99.                            DANCODBC
  1380       25 ANNO                 PIC 9.                             DANCODBC
  1381      20 VESTIBILITA           PIC 9.                             DANCODBC
  1382      20 SOCIETA               PIC 99.                            DANCODBC
  1383      20 FILLER REDEFINES SOCIETA.                                DANCODBC
  1384       25 PREFBC-V-F           PIC 9.                             DANCODBC
  1385       25 SOC-BC-MOD           PIC 9.                             DANCODBC
  1386      20 PEZZO                 PIC 9.                             DANCODBC
  1387      20 VARIANTE-COL          PIC 99.                            DANCODBC
  1388      20 TAGLIA                PIC 9.                             DANCODBC
  1389     15 C-MAT-A-BARRE-RID REDEFINES C-MAT-A-BARRE  PIC 9(14).     DANCODBC
  1390*                                                                 DANCODBC
  1391*CONTROL LIST                                                     DANCODBC
  1392*                                                                 DANCODBC
  1393  05 COD-IN-RID REDEFINES COD-IN   PIC X.
  1394     88 LETT-FINE      VALUE ".".
  1395     88 LETT-ANN-ULT   VALUE "%".
  1396     88 LETT-ANN-TUTTO VALUE "@".
  1397     88 LETT-STAMPA    VALUES "S", "s".
  1398*
  1399*Mag3_V/F*
  1400 01 F-V-INPUT   PIC X.
  1401*
  1402 01 MEM-COD-IN  PIC X(14).
  1403*
  1404*T5000*
  1405 01 DEV-IN           PIC X(2).
  1406     01 DISIMPEGNA       PIC XX.
  1407     01 DA-TRASFERIRE    PIC S9(8) COMP.
  1408*
  1409 01 C-MAT-COM
*                   COPY DANCODMT.
  1410     .                                                            DANCODMT
  1411*IF X9=OFF                                                        DANCODMT
  1412*CONTROL NOLIST                                                   DANCODMT
  1413*IF                                                               DANCODMT
  1414     15 C-MAT-TRANSITO.                                           DANCODMT
  1415       20  MODELLO                  PIC 9(7).                     DANCODMT
  1416       20 MOD-RID  REDEFINES MODELLO.                             DANCODMT
  1417        25  COLLEZIONE              PIC 9.                        DANCODMT
  1418        25  CLASSE                  PIC 99.                       DANCODMT
  1419        25  STAGIONE                PIC 9.                        DANCODMT
  1420        25  PROGR-MOD               PIC 99.                       DANCODMT
  1421        25  ANNO                    PIC 9.                        DANCODMT
  1422       20  ARTICOLO                 PIC 9(5).                     DANCODMT
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  26
* READVE3.cob (/home/prorosa/cobol/cpy/DANCODMT)
  1423       20 ART-RID  REDEFINES ARTICOLO.                            DANCODMT
  1424        25 GR-MERC                  PIC 99.                       DANCODMT
  1425        25 FILLER REDEFINES GR-MERC.                              DANCODMT
  1426          30 VEST-A                 PIC 9.                        DANCODMT
  1427          30 PEZZO-A                PIC 9.                        DANCODMT
  1428        25 PROGR-ART                PIC 999.                      DANCODMT
  1429        25 FILLER REDEFINES PROGR-ART.                            DANCODMT
  1430         30 FILLER                  PIC 9.                        DANCODMT
  1431         30 PREFISSO-V-F            PIC 9.                        DANCODMT
  1432         30 SOCIETA-MOD             PIC 9.                        DANCODMT
  1433       20  COLORE                   PIC 999.                      DANCODMT
  1434*                                                                 DANCODMT
  1435     15 C-MAT-TRANS-RID REDEFINES C-MAT-TRANSITO PIC 9(15).       DANCODMT
  1436*CONTROL LIST                                                     DANCODMT
  1437*
  1438 01 COM-QTA-DISP     PIC S9(4) COMP.
  1439*
  1440 01 TABELLA-ARTICOLI-LETTI.
  1441  03 ART-TAB-LETTI   OCCURS 5000.
  1442   05 TAB-ART        PIC S9(15) COMP-3.
  1443   05 D-MAT-TAB      PIC X(7).
  1444*BUDA*
  1445   05 PRIMA-TG-TAB     PIC S9(4) COMP.
  1446   05 PREZZO-TAB       PIC S9(9) COMP.
  1447   05 CAMBIO-TAB       PIC S9(9) COMP.
  1448   05 TIPO-ANA-TAB  PIC XX.
  1449   05 QTA-GIAC-TAB.
  1450     10 QTA-GIAC-PF-TAB  PIC S9(8) COMP
*                                               COPY NTGOCCURS.
  1451        OCCURS 10.
  1452   05 QTA-TAGLIE-TAB.
  1453     10 QTA-TAGLIA-TAB PIC S9(4) COMP
*                                              COPY NTGOCCURS.
  1454        OCCURS 10.
  1455*VACO*
  1456   05 COSTO-TAB       PIC S9(9) COMP.
  1457*
  1458 01 ART-ELEM-LETTI.
  1459   05 ELEM-ART        PIC S9(15) COMP-3.
  1460   05 D-MAT-ELEM     PIC X(7).
  1461*BUDA*
  1462   05 PRIMA-TG-ELEM    PIC S9(4) COMP.
  1463   05 PREZZO-ELEM      PIC S9(9) COMP.
  1464   05 CAMBIO-ELEM      PIC S9(9) COMP.
  1465   05 TIPO-ANA-ELEM    PIC XX.
  1466   05 QTA-GIAC-ELEM.
  1467     10 QTA-GIAC-PF-ELEM  PIC S9(8) COMP
*                                                COPY NTGOCCURS.
  1468        OCCURS 10.
  1469   05 QTA-TAGLIE-ELEM.
  1470     10 QTA-TAGLIA-ELEM PIC S9(4) COMP
*                                             COPY NTGOCCURS.
  1471        OCCURS 10.
  1472*VACO*
  1473   05 COSTO-ELEM       PIC S9(9) COMP.
  1474*
  1475 01 QTA-TAGLIE-NEG.
  1476     10 QTA-TAGLIA-NEG PIC S9(4) COMP
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  27
* READVE3.cob
*                                           COPY NTGOCCURS.
  1477        OCCURS 10.
  1478*
  1479 01 NUM-ELEM-MAX-ART  PIC S9(4) COMP VALUE 5000.
  1480*
  1481 01 PARTAB-ART
*                      COPY QPARTAB.
  1482*                                ******************************   QPARTAB
  1483     .                                                            QPARTAB
  1484*IF X9=OFF                                                        QPARTAB
  1485*CONTROL NOLIST                                                   QPARTAB
  1486*IF                                                               QPARTAB
  1487   05 QT-PARAMETRI.                                               QPARTAB
  1488    10 QT-STATO               PIC S9(4) COMP.                     QPARTAB
  1489    10 QT-NUM-ELEM-EFF        PIC  9(4) COMP.                     QPARTAB
  1490    10 QT-NUM-ELEM-MAX        PIC  9(4) COMP.                     QPARTAB
  1491    10 QT-LL-ELEM             PIC  9(4) COMP.                     QPARTAB
  1492    10 QT-ADDR-KEY            PIC  9(4) COMP.                     QPARTAB
  1493    10 QT-LL-KEY              PIC  9(4) COMP.                     QPARTAB
  1494    10 QT-INDEX-ELEM          PIC  9(4) COMP.                     QPARTAB
  1495    10 QT-FUNZIONE            PIC  XX.                            QPARTAB
  1496*                                                                 QPARTAB
  1497*CONTROL LIST                                                     QPARTAB
  1498*                                                                 QPARTAB
  1499*
  1500 01 TABELLA-NO-GIAC.
  1501  05 ELEM-NO-GIAC    OCCURS 1000.
  1502   10 C-MAT-NO-GIAC       PIC S9(15) COMP-3.
  1503   10 PREZZO-NO-GIAC       PIC S9(9) COMP.
  1504   10 D-MAT-NO-GIAC        PIC X(7).
  1505*PRZBU*
  1506   10 CAUSALE-NO-GIAC      PIC X(10).
  1507   10 CAUSALE-NO-PRZ       PIC X(10).
  1508*
  1509 01 IND-CAPI-NO-GIAC      PIC S9(4) COMP.
  1510*
  1511 01 COMANDO-BUILD.
  1512   05 FILLER PIC X(7) VALUE "BUILD P".
  1513   05 BUILD-N-DDT      PIC 9(6).
  1514   05 FILLER           PIC X(30) VALUE
  1515            ";REC=-132,3,F,ASCII;DISC=20000".
  1516   05 CARRIAGE-RETURN PIC X VALUE X"13".
  1517*
  1518 01 COMANDO-FILE.
  1519   05 FILLER PIC X(11) VALUE "FILE PEND=P".
  1520   05 FILE-N-DDT       PIC 9(6).
  1521   05 CARRIAGE-RETURN PIC X VALUE X"13".
  1522*
  1523 01 COMANDO-FILE-2.
  1524   05 FILLER PIC X(6) VALUE "FILE P".
  1525   05 FILE-N-DDT-2     PIC 9(6).
  1526   05 FILLER PIC X(9) VALUE ";DEV=91,5".
  1527   05 CARRIAGE-RETURN PIC X VALUE X"13".
  1528*
  1529 01 COMANDO-PURGE.
  1530   05 FILLER PIC X(7) VALUE "PURGE P".
  1531   05 PURGE-N-DDT      PIC 9(6).
  1532   05 CARRIAGE-RETURN PIC X VALUE X"13".
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  28
* READVE3.cob
  1533*
  1534 01 COMANDO-PRINT.
  1535   05 FILLER PIC X(7) VALUE "PRINT P".
  1536   05 PRINT-N-DDT         PIC 9(6).
  1537   05 FILLER              PIC X(7) VALUE ";OUT=*P".
  1538   05 PRINT-N-DDT-2       PIC 9(6).
  1539   05 CARRIAGE-RETURN PIC X VALUE X"13".
  1540*
  1541*conv
  1542*
  1543 01 COMANDO-LPR-LINUX.
  1544   05 FILLER                PIC X(9) VALUE "lpr -P p1".
  1545   05 LPR-NUM-STAMPANTE     PIC 99.
  1546   05 FILLER                PIC X VALUE " ".
  1547   05 LPR-NOME-FILE         PIC X(80).
  1548   05 FILLER                PIC X VALUE X"00".
  1549*
  1550 01 WK-VAR-NAME             PIC X(80).
  1551 01 WK-VAR-VALUE            PIC X(80).
  1552*
  1553 01 DIR-VAR-NAME            PIC X(80).
  1554 01 DIR-VAR-VALUE           PIC X(80).
  1555*
  1556 01 USER-VAR-NAME           PIC X(80).
  1557 01 USER-VAR-VALUE          PIC X(80).
  1558*
  1559 01 FILE-VAR-NAME           PIC X(80).
  1560 01 FILE-VAR-VALUE          PIC X(80).
  1561*
  1562*conv-end
  1563 01 ERR   PIC S9999  COMP VALUE 0.
  1564 01 ERR-PARM  PIC S9999 COMP VALUE 0.
  1565*
  1566 01 LOCALITA-PART-STR      PIC X(52).
  1567*  "Magazzino Via Santi 8, Cavriago (R.E.) ".
  1568*
  1569* Tabella di mappatura magazzino -> localita'
  1570*
  1571 01 IND-LOC                PIC S9(4) COMP.
  1572* allineare con il numero di occorrenze della tabella espansa!
  1573 01 MAX-LOC                PIC S9(4) COMP VALUE 4.
  1574*
  1575 01 TAB-LOC-EXP.
  1576   05 FILLER               PIC 9(3)  VALUE   2.
  1577   05 FILLER               PIC X(52) VALUE
  1578      "Magazzino Via Santi 8, Cavriago (R.E.) ".
  1579   05 FILLER               PIC 9(3)  VALUE   3.
  1580   05 FILLER               PIC X(52) VALUE
  1581* "Via Dell'Artigianato 2/A Qre SPIP(PR) c/o LA GIOVANE".
  1582      "Magazzino Via Santi 5, Cavriago (R.E.) ".
  1583   05 FILLER               PIC 9(3)  VALUE   6.
  1584   05 FILLER               PIC X(52) VALUE
  1585      "Magazzino Via Santi 8, Cavriago (R.E.) ".
  1586   05 FILLER               PIC X(52) VALUE
  1587      "Magazzino Via Santi 8, Cavriago (R.E.) ".
  1588   05 FILLER               PIC 9(3)  VALUE   7.
  1589   05 FILLER               PIC X(52) VALUE
  1590      "Magazzino Via Santi 8, Cavriago (R.E.) ".
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  29
* READVE3.cob
  1591* allineare le occorrenze con quelle della tabella espansa!
  1592 01 TAB-LOC REDEFINES TAB-LOC-EXP.
  1593   05 FILLER OCCURS    4.
  1594     10 COD-LOC            PIC 9(3).
  1595     10 DESC-LOC           PIC X(52).
  1596*
  1597*
  1598 01 RIGA-DISP.
  1599  03 DATI-DISP-4   OCCURS 3.
  1600   05 FILLER        PIC XXX.
  1601   05 DISP-ART      PIC Z(14).
  1602  05 FILLER      PIC X.
  1603   05 PARE1  PIC X.
  1604   05 QTA-DISP  PIC ZZZ.
  1605   05 PARE2  PIC X.
  1606*
  1607 01 TOT-CAPI-LETTI-1     PIC 9(4).
  1608*
  1609 01 TAB-LOCK.
  1610   05 FILLER PIC S9(4) COMP VALUE 4.
  1611   05 FILLER PIC S9(4) COMP VALUE 17.
  1612   05 FILLER PIC X(16) VALUE "DPARAM;".
  1613   05 FILLER PIC X(16) VALUE "@".
  1614   05 FILLER PIC S9(4) COMP VALUE 17.
  1615   05 FILLER PIC X(16) VALUE "SITPF;".
  1616   05 FILLER PIC X(16) VALUE "@".
  1617   05 FILLER PIC S9(4) COMP VALUE 17.
  1618   05 FILLER PIC X(16) VALUE "MOVMAG;".
  1619   05 FILLER PIC X(16) VALUE "@".
  1620   05 FILLER PIC S9(4) COMP VALUE 17.
  1621   05 FILLER PIC X(16) VALUE "BOLLE;".
  1622   05 FILLER PIC X(16) VALUE "@".
  1623*
  1624*
  1625 01 STK-NOME       PIC X(30).
  1626 01 STK-C-MAT      PIC 9(15) COMP-3.
  1627 01 STK-STAGIONE   PIC 9.
  1628 01 STK-SCO        PIC 9(5) COMP.
  1629 01 STK-COLL    PIC 99.
  1630 01 STK-PRZ-SCO    PIC 9(9) COMP.
  1631 01 STK-PRZ-LORDO  PIC 9(9) COMP.
  1632 01 STK-MSG.
  1633   05 STK-MSG-1          PIC X(30).
  1634   05 STK-MSG-2          PIC 9(15).
  1635 01 STK-PRIMA-VOLTA      PIC S9(4) COMP.
  1636    88 PRIMA-VOLTA  VALUE 0.
  1637 01 STK-CAMBIO           PIC 9(9) COMP.
  1638*
  1639 01 NOME-IN   PIC X(30) VALUE "TABSTK.TABELLE".
  1640 01 NOME-IN-B PIC X(30) VALUE "TABSTB.TABELLE".
  1641*
  1642 01  PAR-PRINT.
  1643  05  STATO         PIC S9(4) COMP.
  1644  05  LL-RIGA       PIC  9(4) COMP.
  1645  05  N-MAX-RIGHE   PIC  9(4) COMP.
  1646  05  FLAG-ROUTINE  PIC  9(4) COMP.
  1647  05  NUM-FILE-ID   PIC  9(4) COMP.
  1648  05  NOME-FILE.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  30
* READVE3.cob
  1649   10 PRE-NOME-FILE     PIC X.
  1650   10 TERM-N-FILE       PIC 9(6).
  1651   10 FILLER            PIC XXX VALUE ".ST".
  1652*
  1653 01  RIGA.
  1654  05  N-STAMPANTE   PIC 9.
  1655  05  COMANDO       PIC X.
  1656  05  N-RIGA-STAMPA PIC 9(4) COMP.
  1657  05  DATI-RIGA     PIC X(132).
  1658*
  1659  05 RIGA-INTESTA REDEFINES DATI-RIGA.
  1660    10 FILLER        PIC X.
  1661    10 CONTO-T       PIC ZZ9/99999.
  1662    10 FILLER        PIC X.
  1663    10 D-CONTO-T     PIC X(24).
  1664    10 FILLER        PIC XXX.
  1665    10 DATA-T        PIC X(12).
  1666    10 FILLER        PIC X(5).
  1667    10 D-PAG-T       PIC X(5).
  1668    10 FILLER        PIC X.
  1669    10 NUM-PAG-T     PIC ZZ9.
  1670*
  1671  05 RIGA-DETTAGLIO REDEFINES DATI-RIGA.
  1672    10 FILLER        PIC X(7).
  1673    10 C-MAT-ST      PIC 9(15).
  1674    10 FILLER        PIC XX.
  1675    10 TAGLIA-ST     PIC 9.
  1676    10 FILLER        PIC X(6).
  1677    10 NOME-MOD-ST   PIC X(7).
  1678    10 FILLER        PIC X(6).
  1679    10 PREZZO-ST     PIC Z(8).
  1680*EURO1*
  1681    10 PREZZO-ST-EU REDEFINES PREZZO-ST  PIC Z(5),ZZ.
  1682*PRZBU*
  1683    10 FILLER        PIC X(6).
  1684    10 NOGIAC-ST     PIC X(10).
  1685    10 FILLER        PIC X(3).
  1686    10 NOPRZ-ST      PIC X(10).
  1687*
  1688*
  1689 01 BUFFER.
  1690  05 N-BUF               PIC S9(4) COMP VALUE 37.
  1691  05 FILLER              PIC XX.
  1692  05 FILLER              PIC X(5120).
  1693*
  1694*MAG6/7*
  1695 01 MAG-INPUT   PIC X(3).
  1696 01 MAG-INPUT-R REDEFINES MAG-INPUT PIC 9(3).
  1697*VIBLO*
  1698*   88 MAG-OK             VALUES  2, 3, 6, 7.
  1699*MAG1
  1700*   88 MAG-OK             VALUES  4, 6, 7.
  1701   88 MAG-OK             VALUES  1, 4, 6, 7, 852, 853.
  1702*
  1703   88 MAG-VALIDO         VALUES  4, 7, 852, 853.
  1704   88 MAG-FALLATO        VALUES  1, 6.
  1705   88 MAG-STOCK          VALUES  1, 4, 6, 7, 852, 853.
  1706 01 SUGG-MAG-DISP        PIC X(50)
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  31
* READVE3.cob
  1707*VIBLO*
  1708*    VALUE "accettato MAG 2 o 3 o 6 o 7".
  1709    VALUE "accettato MAG 1, 4, 6, 7, 852 o 853".
  1710*
  1711 01 CLASSE-INPUT.
  1712   05 CLASSE-INPUT-R     PIC 99.
  1713 01 SOCIETA-INPUT.
  1714   05 SOCIETA-INPUT-R    PIC 9.
  1715 01 AS-INPUT  .
  1716   05 A-INPUT-R         PIC 9.
  1717   05 S-INPUT-R         PIC 9.
  1718*ASZERO*
  1719 01 FILLER REDEFINES AS-INPUT PIC XX.
  1720   88 TUTTI-AS          VALUES "tt", "TT".
  1721*
  1722 01 AS-INPUT-R REDEFINES AS-INPUT
  1723                        PIC 99.
  1724*UNICODDT*
  1725 01 STRINGA-TUTTI-AS.
  1726    05 PIC X(30) VALUE "02.04.12.14.22.24.32.34.42.44.".
  1727    05 PIC X(30) VALUE "52.54.62.64.72.74.82.84.92.94.".
  1728 01 ELENCO-AS.
  1729    88 TUTTI-AS-ELENCO VALUE "TT", "tt".
  1730    88 FINE-AS          VALUES "chiudi", "CHIUDI".
  1731    05 OCCURS 20.
  1732       10 AS-IN.
  1733          15 AS-IN-R PIC 99.
  1734       10            PIC X.
  1735 01 ELENCO-CL.
  1736    88 TUTTI-CL-ELENCO VALUE "TT", "tt".
  1737    05 OCCURS  99.
  1738       10 CL-IN.
  1739          15 CL-IN-R PIC 99.
  1740       10            PIC X.
  1741 01 I-APP PIC S9(4) COMP.
  1742 01 I-AS PIC S9(4) COMP.
  1743 01 I-CL PIC S9(4) COMP.
  1744 01 PRO PIC X.
  1745 01 SINO PIC XX.
  1746    88 SINO-SI VALUE "SI", "si".
  1747    88 SINO-NO VALUE "NO", "no".
  1748 01 APP-AS PIC 99.
  1749 01 REDEFINES APP-AS.
  1750     05 APP-A PIC 9.
  1751     05 APP-S PIC 9.
  1752 01 SW-AS-ERR PIC 9.
  1753    88 AS-ERR VALUE 1.
  1754 01 SW-CL-ERR PIC 9.
  1755    88 CL-ERR VALUE 1.
  1756 01 SW-MAX-CAPI-RAGGIUNTO PIC 9.
  1757    88 MAX-CAPI-RAGGIUNTO VALUE 1.
  1758 01 SW-ERR-AS-CL pic 9.
  1759    88 err-as-cl value 1.
  1760 01 SW-FINE-CARICA-TAB PIC 9.
  1761    88 FINE-CARICA-TAB VALUE 1.
  1762* 01 XD PIC X VALUE "S".
  1763 01 XD PIC X VALUE "N".
  1764*----------------------------------------------------------------*
  1765 01 PAR-TAB-UNICO-DDT
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  32
* READVE3.cob
*                           COPY QPARTABX.
  1766*                                ******************************   QPARTABX
  1767     .                                                            QPARTABX
  1768*IF X9=OFF                                                        QPARTABX
  1769*CONTROL NOLIST                                                   QPARTABX
  1770*IF                                                               QPARTABX
  1771   05 QT-PARAMETRI.                                               QPARTABX
  1772    10 QT-STATO               PIC S9(9) COMP.                     QPARTABX
  1773    10 QT-NUM-ELEM-EFF        PIC  9(9) COMP.                     QPARTABX
  1774    10 QT-NUM-ELEM-MAX        PIC  9(9) COMP.                     QPARTABX
  1775    10 QT-LL-ELEM             PIC  9(9) COMP.                     QPARTABX
  1776    10 QT-ADDR-KEY            PIC  9(9) COMP.                     QPARTABX
  1777    10 QT-LL-KEY              PIC  9(9) COMP.                     QPARTABX
  1778    10 QT-INDEX-ELEM          PIC  9(9) COMP.                     QPARTABX
  1779    10 QT-FUNZIONE            PIC  XX.                            QPARTABX
  1780*                                                                 QPARTABX
  1781*CONTROL LIST                                                     QPARTABX
  1782*                                                                 QPARTABX
  1783*
  1784 01 TAB-UNICO-DDT.
  1785    05 ELE-TAB-UNICO-DDT      PIC X(16) OCCURS   1980.
  1786*
  1787 01 DEP-TAB-UNICO-DDT.
  1788    05 KEY-TAB-UNICO-DDT.
  1789       10 TAB-AS PIC 99.
  1790       10 TAB-CL PIC 99.
  1791    05 DATI-TAB-UNICO-DDT.
  1792       10 TAB-MAX-CAPI PIC 9(6).
  1793       10 TAB-CAPI-LETTI PIC 9(6).
  1794*----------------------------------------------------------------*
  1795*UNICODDT*
  1796*
  1797 01 NOME-IN-35.
  1798   05 NOME-IN-3   PIC XXX.
  1799   05 NOME-IN-5   PIC 9(5).
  1800   05 FILLER      PIC X(8) VALUE ".TABELLE".
  1801*
  1802*
  1803*
  1804*NOPRZ*
  1805 01 PREZZO-ANAMAT  PIC S9(9) COMP.
  1806*
  1807*
  1808 01 PARDEED
*                  COPY QPARDEED.
  1809*                                ******************************   QPARDEED
  1810     .                                                            QPARDEED
  1811*IF X9=OFF                                                        QPARDEED
  1812*CONTROL NOLIST                                                   QPARDEED
  1813*IF                                                               QPARDEED
  1814   05 QD-PARAMETRI.                                               QPARDEED
  1815    10 QD-STATO               PIC S9(4) COMP.                     QPARDEED
  1816    10 QD-NR-DEC              PIC  9(4) COMP.                     QPARDEED
  1817    10 QD-LL-A                PIC  9(4) COMP.                     QPARDEED
  1818    10 QD-LL-B                PIC  9(4) COMP.                     QPARDEED
  1819    10 FILLER                 PIC  X(8).                          QPARDEED
  1820*                                                                 QPARDEED
  1821*CONTROL LIST                                                     QPARDEED
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  33
* READVE3.cob
  1822*                                                                 QPARDEED
  1823*
  1824*EURO*
  1825 01 PAR-INEU
*                  COPY QPARINEU.
  1826*                                ******************************   QPARINEU
  1827     .                                                            QPARINEU
  1828*IF X9=OFF                                                        QPARINEU
  1829*CONTROL NOLIST                                                   QPARINEU
  1830*IF                                                               QPARINEU
  1831   05 IE-PARAMETRI.                                               QPARINEU
  1832    10 IE-STATO               PIC S9(4) COMP.                     QPARINEU
  1833     88 IE-ERRORE  VALUE -1.                                      QPARINEU
  1834    10 IE-IMPORTO-IN          PIC  S9(18) COMP.                   QPARINEU
  1835    10 IE-IMPORTO-IN-V REDEFINES IE-IMPORTO-IN                    QPARINEU
  1836                              PIC  S9(16)V99 COMP.                QPARINEU
  1837    10 IE-IMPORTO-OU          PIC  S9(18) COMP.                   QPARINEU
  1838    10 IE-IMPORTO-OU-V REDEFINES IE-IMPORTO-OU                    QPARINEU
  1839                              PIC  S9(16)V99 COMP.                QPARINEU
  1840    10 IE-DIVISA-IN           PIC  X(004).                        QPARINEU
  1841    10 IE-DIVISA-OU           PIC  X(004).                        QPARINEU
  1842     88  IE-LIRE   VALUE "LIT".                                   QPARINEU
  1843     88  IE-EURO VALUE "EUR".                                     QPARINEU
  1844    10 IE-MSG                 PIC  X(40).                         QPARINEU
  1845*                                                                 QPARINEU
  1846*CONTROL LIST                                                     QPARINEU
  1847*                                                                 QPARINEU
  1848*
  1849*EURO1*
  1850 01 DIVISA-PRIMO-LETTO  PIC X(4).
  1851*
  1852*T5000*
  1853 01 DESTINO-USCITA  PIC 9(2).
  1854   88 DESTINO-VALIDO VALUES ARE 73 90 94 27 28 29 34 22.
  1855*
  1856*
  1857*FIFRA*
  1858 01 CONTO-FATTURA-MEM   PIC S9(8) COMP.
  1859*
  1860* NO-DATGE
  1861  01 TAB-RIGHE-SOCIETA
*                            COPY MAPPASOCIETA.
  1862*                                                                 MAPPASOC
  1863     .                                                            MAPPASOC
  1864*IF X9=OFF                                                        MAPPASOC
  1865*CONTROL NOLIST                                                   MAPPASOC
  1866*IF                                                               MAPPASOC
  1867      20 TAB-MAPPA-SOCIETA.                                       MAPPASOC
  1868          25 FILLER   PIC S9(9) COMP VALUE 9.                     MAPPASOC
  1869          25 SOC-1 PIC XX            VALUE "CA".                  MAPPASOC
  1870          25 COD-1 PIC S9(9) COMP    VALUE 1.                     MAPPASOC
  1871          25 SOC-2 PIC XX            VALUE "MA".                  MAPPASOC
  1872          25 COD-2 PIC S9(9) COMP    VALUE 2.                     MAPPASOC
  1873          25 SOC-3 PIC XX            VALUE "MN".                  MAPPASOC
  1874          25 COD-3 PIC S9(9) COMP    VALUE 3.                     MAPPASOC
  1875          25 SOC-4 PIC XX            VALUE "MR".                  MAPPASOC
  1876          25 COD-4 PIC S9(9) COMP    VALUE 4.                     MAPPASOC
  1877          25 SOC-5 PIC XX            VALUE "DT".                  MAPPASOC
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  34
* READVE3.cob (/home/prorosa/cobol/cpy/MAPPASOCIETA)
  1878          25 COD-5 PIC S9(9) COMP    VALUE 5.                     MAPPASOC
  1879          25 SOC-6 PIC XX            VALUE "MM".                  MAPPASOC
  1880          25 COD-6 PIC S9(9) COMP    VALUE 6.                     MAPPASOC
  1881          25 SOC-7 PIC XX            VALUE "YE".                  MAPPASOC
  1882          25 COD-7 PIC S9(9) COMP    VALUE 7.                     MAPPASOC
  1883          25 SOC-8 PIC XX            VALUE "MH".                  MAPPASOC
  1884          25 COD-8 PIC S9(9) COMP    VALUE 8.                     MAPPASOC
  1885          25 SOC-9 PIC XX            VALUE "IM".                  MAPPASOC
  1886          25 COD-9 PIC S9(9) COMP    VALUE 9.                     MAPPASOC
  1887*                                                                 MAPPASOC
  1888                                                                  MAPPASOC
  1889       20 TAB-MAPPA-SOCIETA-RID REDEFINES TAB-MAPPA-SOCIETA.      MAPPASOC
  1890          25 NUMERO-MAX-SOCIETA PIC S9(9) COMP.                   MAPPASOC
  1891          25 EL-TAB-SOC OCCURS 9.                                 MAPPASOC
  1892             30 SOCIETA-SIGLA      PIC XX.                        MAPPASOC
  1893             30 SOCIETA-CODICE     PIC S9(9) COMP.                MAPPASOC
  1894                                                                  MAPPASOC
  1895*  {1: 'CA',                                                      MAPPASOC
  1896*  2: 'MA',                                                       MAPPASOC
  1897*  3: 'MN',                                                       MAPPASOC
  1898*  4: 'MR',                                                       MAPPASOC
  1899*  5: 'DT',                                                       MAPPASOC
  1900*  6: 'MM',                                                       MAPPASOC
  1901*  7: 'YE',                                                       MAPPASOC
  1902*  8: 'MH',                                                       MAPPASOC
  1903*  9: 'IM'}                                                       MAPPASOC
  1904*                                                                 MAPPASOC
  1905*CONTROL LIST                                                     MAPPASOC
  1906*                                                                 MAPPASOC
  1907*                                                                 MAPPASOC
  1908* NO-DATGE
  1909*
  1910*PRZ-PUBBL*
  1911 01 PREZZO-PUBBL        PIC S9(9) COMP.
  1912*
  1913*PRZ-PUBBL*
  1914**** Start SQL Preprocessor ****
  1915*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
  1916**** Start Inserted Statements ****
  1917**** End SQL Processor   ****
  1918*
  1919* NO-DATGE
  1920* EXEC SQL INCLUDE MODELLI.IF END-EXEC.
  1921* EXEC SQL INCLUDE PREZZI1.IF END-EXEC.
  1922* EXEC SQL INCLUDE SOCIETA.IF END-EXEC.
  1923  01 CC-C-MAT         PIC 9(15) .
  1924  01 CC-SOCIETA       PIC XX.
  1925*  01 CC-ANNO          PIC 9(4).
  1926*  01 CC-STAGIONE      PIC 9.
  1927  01 CC-LISTINO       PIC 99.
  1928  01 CC-TIPO-PREZZO   PIC X.
  1929  01 CC-PREZZO-DBG    PIC  S9(13)V99 COMP-3.
  1930* NO-DATGE
  1931*movsku
  1932*EXEC SQL INCLUDE YMOVSKU END-EXEC.
  1933*
  1934*           *************************************
  1935*           *     TAV. MOV_SKU                  *
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  35
* READVE3.cob
  1936*           *************************************
  1937*
  1938  01  MOVSKU-RIF-INTERNO                    PIC S9(15) COMP-3.
  1939*  01  MOVSKU-BARCODE                        PIC S9(15) COMP-3.
  1940  01  MOVSKU-CMAT                           PIC S9(15) COMP-3.
  1941  01  MOVSKU-TG                             PIC S9(4) COMP.
  1942  01  MOVSKU-BARUNI                         PIC X(13).
  1943  01  MOVSKU-CONTO                          PIC S9(8)  COMP.
  1944  01  MOVSKU-MAG                            PIC S9(4)  COMP.
  1945  01  MOVSKU-SKU                            PIC X(8).
  1946  01  MOVSKU-IS-BARUNI-READ                 PIC 9(1).
  1947  01  MOVSKU-IS-BARUNI-CERTIFIED            PIC 9(1).
  1948  01  MOVSKU-SKU-FATTURAZIONE               PIC X(8).
  1949*ASOLOB2C*
  1950*EXEC SQL INCLUDE ANACST.IF END-EXEC.
  1951*
  1952*                tav. ANAMAT_CST  (ANACST.IF)
  1953 01 ANACST-C-MAT                    PIC S9(15) COMP-3.
  1954 01 ANACST-CST-STD                  PIC S9(9) COMP.
  1955 01 ANACST-CST-STD-2                PIC S9(9) COMP.
  1956 01 ANACST-TS-CST                   PIC S9(15) COMP-3.
  1957*
  1958 01 TAB-B2C-NO-DT.
  1959    05 EL-B2C-NO-DT  OCCURS 100.
  1960      10 MAG-B2C-NO-DT  PIC S9(4) COMP.
  1961*ASOLOB2C*
  1962**** Start SQL Preprocessor ****
  1963*EXEC SQL END DECLARE SECTION END-EXEC.
  1964**** Start Inserted Statements ****
  1965**** End SQL Processor   ****
  1966*
  1967 01 SQL-CONST
*                               COPY SQLCONST .
  1968*
  1969*    Costanti usate per gestione Hp-Sql
  1970   .
  1971   05  COSTANTI-SQL.
  1972     10  DEADLOCK           PIC S9(9) COMP VALUE -14024.
  1973     10  NO-MEMORY          PIC S9(9) COMP VALUE  -4008.
  1974     10  MULTIPLE-ROWS      PIC S9(9) COMP VALUE  -2112.
  1975     10  NOT-FOUND          PIC S9(9) COMP VALUE   100.
  1976     10  MODULE-NOT-FOUND   PIC S9(9) COMP VALUE  -2216.
  1977     10  CONSTR-VIOLATED-DF PIC S9(9) COMP VALUE  -2091.
  1978     10  CONSTR-VIOLATED    PIC S9(9) COMP VALUE  -2292.
  1979     10  OK                 PIC S9(9) COMP VALUE     +0.
  1980*
  1981   05  SQL-STATUS           PIC S9(9) COMP.
  1982     88 DEAD-NOMEM          VALUE -14024, -4008.
  1983 01 PAR-ERR
*                               COPY PARERR .
  1984     .
  1985*************************************************************
  1986*  area dati routine generalizzata ERRORI
  1987*************************************************************
  1988   03 ER-NUM                       PIC S9(4) COMP.
  1989      88 SQL-FINE                  VALUE 0.
  1990      88 SQL-CONTINUA              VALUE 1.
  1991   03 ER-IND                       PIC S9(4) COMP.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  36
* READVE3.cob (/home/prorosa/cobol/cpy/PARERR)
  1992   03 ER-DISP                      PIC X.
  1993      88 ER-DISPLAY                VALUE "D".
  1994      88 ER-WINDOW                 VALUE "W".
  1995      88 ER-DISPLAY-CONTINUE       VALUE "C".
  1996   03 ER-DESCRIZIONE               PIC X(80).
  1997*
  1998 01 AREA-HL
*                               COPY AREAHL .
  1999             .
  2000*                                               ******************
  2001*                                               *  Area   HI-LI  *
  2002*                                               ******************
  2003  03 HL-COMMON-AREA.
  2004   05 HL-SEND-PAR.
  2005    15 HL-ENABLE-INPUT              PIC S9(8) COMP.
  2006    15 HL-WINDOW-ENH                PIC X(8).
  2007    15 HL-BYPASS-FEATURE            PIC S9(8) COMP.
  2008*
  2009   05 HL-GLOBAL-PAR.
  2010    15 HL-EXPECTED-VUF              PIC X(8)  SYNC.
  2011    15 HL-CALL-PROTOCOL             PIC S9(8) COMP.
  2012    15 HL-COMAREA-LEN               PIC S9(8) COMP.
  2013    15 HL-COMAREA                   PIC X(300).
  2014*
  2015   05 HL-RETURN-PAR.
  2016    15 HL-STATUS                    PIC S9(8) COMP.
  2017       88 HILI-OK                   VALUE 0.
  2018       88 HILI-READ-TIMEOUT         VALUE 24.
  2019       88 HILI-ERR-EDIT             VALUE 33.
  2020    15 HL-RETURN-SUBSTATUS          PIC S9(8) COMP.
  2021    15 HL-RETURN-MSGLENGTH          PIC S9(8) COMP.
  2022    15 HL-RETURN-MSG                PIC X(256).
  2023    15 HL-LAST-ITEMTYPE             PIC S9(8) COMP.
  2024    15 HL-LAST-ITEMNUM              PIC S9(8) COMP.
  2025       88 HL-ENTER                  VALUE 0.
  2026       88 HL-F1                     VALUE 1.
  2027       88 HL-F2                     VALUE 2.
  2028       88 HL-F3                     VALUE 3.
  2029       88 HL-F4                     VALUE 4.
  2030       88 HL-F5                     VALUE 5.
  2031       88 HL-F6                     VALUE 6.
  2032       88 HL-F7                     VALUE 7.
  2033       88 HL-F8                     VALUE 8.
  2034    15 HL-LAST-ITEMNAME             PIC X(32).
  2035    15 HL-NUM-DATAERRS              PIC S9(8) COMP.
  2036    15 HL-NUM-CHANGEFIELDS          PIC S9(8) COMP.
  2037*
  2038   05 HL-TERM-PAR.
  2039    15 HL-TERM-FILE                 PIC X(88).
  2040    15 HL-BYPASS-FEATURE-2          PIC S9(8) COMP.
  2041*
  2042   05 HL-FORM-PAR.
  2043    15 HL-FORM-NAME                 PIC X(32).
  2044    15 HL-FORM-POSITION             PIC S9(8) COMP.
  2045    15 HL-CHANGE-LISTTYPE           PIC S9(8) COMP.
  2046    15 HL-LIST-COUNT                PIC S9(8) COMP.
  2047    15 HL-CHANGE-ENTRY              OCCURS 64.
  2048     20 HL-FIELD-ID                 PIC X(32).
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  37
* READVE3.cob (/home/prorosa/cobol/cpy/AREAHL)
  2049     20 HL-CHANGE-TYPE              PIC S9(8) COMP.
  2050     20 HL-CHANGE-SPEC              PIC X(8).
  2051*
  2052   05 HL-PROMPT-PAR.
  2053    15 HL-PROMPT-REPAINT            PIC S9(8) COMP.
  2054    15 HL-PROMPT-WINDENH            PIC X(8).
  2055    15 HL-PROMPT-RESET              PIC S9(8) COMP.
  2056*
  2057   05 HL-READ-PAR.
  2058    15 HL-READ-TIME                 PIC S9(8) COMP.
  2059    15 HL-ENABLE-FOR                PIC S9(8) COMP.
  2060    15 HL-DOREREAD                  PIC S9(8) COMP.
  2061    15 HL-FILLER                    PIC S9(8) COMP.
  2062*
  2063   05 HL-READ-ITEMS.
  2064    15 HL-READITEM-COUNT            PIC S9(8) COMP.
  2065    15 HL-READITEM-ENTRY            OCCURS 9.
  2066     20 HL-READITEM-TYPE            PIC S9(8) COMP.
  2067     20 HL-READITEM-ID              PIC S9(8) COMP.
  2068     20 HL-READITEM-OPTION          PIC S9(8) COMP.
  2069*
  2070   05 HL-CURSOR-POSITION.
  2071    15 HL-CURSOR-POS-NUM            PIC S9(8) COMP.
  2072    15 HL-CURSOR-POS-NAME           PIC X(32).
  2073*
  2074   05 HL-PROMPT-CURSOR-POSITION.
  2075    15 HL-FIL-PROMPT                PIC XX.
  2076    15 HL-ERR-CURSOR-POS            PIC X(34).
  2077*
  2078   05 HL-DATA-DESC.
  2079    15 HL-DESC-TYPE                 PIC S9(8) COMP.
  2080    15 HL-BUFF-LENGTH               PIC S9(8) COMP.
  2081    15 HL-RET-BUFLEN                PIC S9(8) COMP.
  2082*
  2083   05 HL-LABEL-DESC.
  2084    15 HL-LABEL-NUM                 PIC S9(8) COMP.
  2085    15 HL-LABEL-ELEM                OCCURS 8.
  2086     20 HL-LABEL-ID                 PIC S9(8) COMP.
  2087     20 HL-LABEL-ENH                PIC X(8).
  2088*
  2089   05 HL-LABEL-BUFF.
  2090    15 HL-LABEL-ENTRY               OCCURS 8.
  2091     20 HL-LABEL-TEXT               PIC X(16).
  2092*
  2093   05 HL-MSG-WINDOW.
  2094    15 HL-MSG-LENGTH                PIC S9(8) COMP.
  2095    15 HL-WINDOW                    PIC X(256).
  2096*
  2097   05 HL-FORMCNTRL-PAR.
  2098    15 HL-FC-NAME                   PIC X(32).
  2099    15 HL-FC-UNDLCNTR               PIC S9(8) COMP.
  2100    15 HL-FC-PAGECNTR               PIC S9(8) COMP.
  2101    15 HL-FC-ENREF                  PIC S9(8) COMP.
  2102*
  2103   05 HL-FILLCNTRL-PAR.
  2104    15 HL-FC-DESC                   PIC S9(8) COMP.
  2105    15 HL-FC-ENTRYCNT               PIC S9(8) COMP.
  2106    15 HL-FC-FIELDID                PIC S9(8) COMP.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  38
* READVE3.cob (/home/prorosa/cobol/cpy/AREAHL)
  2107*
  2108   05 HL-PRINT-DEV                  PIC S9(8) COMP.
  2109*
  2110   05 HL-FORMS-FILE                 PIC X(88).
  2111*
  2112   05 HL-UNUSED-PAR                 PIC S9(8) COMP.
  2113*
  2114   05 HL-ERROR-PAR.
  2115    15 HL-ERR-LIST-TYPE             PIC S9(9) COMP.
  2116    15 HL-ERR-LIST-ALLOC            PIC S9(9) COMP.
  2117    15 HL-ERR-LIST-ACTUAL           PIC S9(9) COMP.
  2118    15 HL-ERR-FIELD                 PIC X(32).
  2119*
  2120   05 HL-CALL                       PIC S9(8) COMP.
  2121*
  2122******************************************************************
  2123*
  2124   05 TP-PARAM.
  2125    15 TP-ERRORI                    PIC S9(8) COMP.
  2126       88 TP-SENZA-ERRORI           VALUE 0.
  2127    15 TP-FLAG.
  2128     20 TP-FLAG-SEND                PIC S9(8) COMP.
  2129        88 TP-SEND                  VALUE 0.
  2130     20 TP-FLAG-READ                PIC S9(8) COMP.
  2131        88 TP-READ                  VALUE 0.
  2132     20 TP-FLAG-AGG-FASE            PIC S9(8) COMP.
  2133        88 TP-AGGIORNA-PER-FASE     VALUE 1.
  2134*
  2135    15 TP-NUMERO-FASI               PIC S9(8) COMP.
  2136    15 TP-FASE-ATTUALE              PIC S9(8) COMP.
  2137       88 TP-PRIMA-FASE             VALUE 1.
  2138    15 TP-SET-KEYF                  PIC S9(8) COMP.
  2139       88 ALTRE-CHIAVI              VALUE 1.
  2140*
  2141    15 TP-RIGHE.
  2142     20 TP-RIGHE-FASE               PIC S9(8) COMP.
  2143        88 TP-SENZA-RIGHE           VALUE 0.
  2144     20 TP-PASSO-ROLL               PIC S9(8) COMP.
  2145     20 TP-RIGA-END                 PIC S9(8) COMP.
  2146     20 TP-RIGA-START               PIC S9(8) COMP.
  2147     20 TP-RIGHE-EFF                PIC S9(8) COMP.
  2148     20 TP-IND-RIGA                 PIC S9(8) COMP.
  2149     20 TP-IND-TERM                 PIC S9(8) COMP.
  2150*
  2151    15 TP-COMANDO.
  2152     20 TP-FIL1                     PIC XXX.
  2153        88 TP-COMANDO-ROLL          VALUE ".SI",
  2154                                          ".SA".
  2155        88 TP-SCORRI-AVANTI         VALUE ".SA".
  2156        88 TP-SCORRI-INDIETRO       VALUE ".SI".
  2157        88 TP-CAMBIA-FASE           VALUE ".FS",
  2158                                          ".FP".
  2159        88 TP-FASE-SEGUENTE         VALUE ".FS".
  2160        88 TP-FASE-PRECEDENTE       VALUE ".FP".
  2161        88 TP-CHIUDI-PROGRAMMA      VALUE ".CP".
  2162        88 TP-ANNULLA-FUNZIONE      VALUE ".AO".
  2163        88 TP-ANNULLA-OPERAZIONE    VALUE ".AO".
  2164        88 TP-ALTRE-CHIAVI          VALUE ".AC".
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  39
* READVE3.cob (/home/prorosa/cobol/cpy/AREAHL)
  2165        88 TP-RICREA-MASCHERA       VALUE ".RM".
  2166        88 TP-CANCELLA-OPERAZIONE   VALUE ".DO".
  2167        88 TP-CHIUDI-OPERAZIONE     VALUE ".CO".
  2168        88 TP-BREAK                 VALUE ".BR".
  2169        88 TP-STAMPA                VALUE ".ST".
  2170        88 TP-HELP                  VALUE ".HE".
  2171        88 TP-COMANDO-VALIDO        VALUE ".FS", ".FP",
  2172                                          ".SI", ".SA",
  2173                                          ".CP", ".CO",
  2174                                          ".DO",
  2175                                          ".AO", ".AC",
  2176                                          ".BR", ".HE",
  2177                                          ".ST", ".RM".
  2178     20 TP-RESTO-COMANDO.
  2179      25 TP-RIGA1                     PIC X.
  2180      25 TP-START1 REDEFINES TP-RIGA1 PIC 9.
  2181      25 FILLER                       PIC X(2).
  2182     20 FILLER REDEFINES TP-RESTO-COMANDO.
  2183      25 TP-RIGA2                     PIC XX.
  2184      25 TP-START2 REDEFINES TP-RIGA2 PIC 99.
  2185      25 FILLER                       PIC X.
  2186     20 FILLER REDEFINES TP-RESTO-COMANDO.
  2187      25 TP-RIGA3                     PIC X(3).
  2188      25 TP-START3 REDEFINES TP-RIGA3 PIC 9(3).
  2189    15 FILLER REDEFINES TP-COMANDO.
  2190     20 FILLER                        PIC X.
  2191        88 TP-COMANDO-DIGITATO       VALUE ".".
  2192     20 FILLER                        PIC X(5).
  2193    15 FILLER REDEFINES TP-COMANDO.
  2194     20 FILLER-COM                    PIC XX.
  2195        88 TP-SCORRI                 VALUE ".S".
  2196     20 TP-RIGA4                      PIC X(4).
  2197     20 TP-START4 REDEFINES TP-RIGA4  PIC 9(4).
  2198     20 TP-RIGA41 REDEFINES TP-RIGA4  PIC X.
  2199     20 TP-START41 REDEFINES TP-RIGA4  PIC 9.
  2200     20 TP-RIGA42 REDEFINES TP-RIGA4  PIC X(2).
  2201     20 TP-START42 REDEFINES TP-RIGA4 PIC 9(2).
  2202     20 TP-RIGA43 REDEFINES TP-RIGA4  PIC X(3).
  2203     20 TP-START43 REDEFINES TP-RIGA4 PIC 9(3).
  2204    15 TP-INFO.
  2205     20 TP-DISAB-FKEY.
  2206      25 TP-NUM-DISAB                 PIC S9(9) COMP.
  2207      25 TP-TABELLA-DISAB.
  2208       30 TP-COM-DISAB OCCURS 16      PIC X(3).
  2209     20 TP-INFO-2                     PIC X(72).
  2210*
  2211  05 H-COMAREA.
  2212     10 H-COM-STATUS       PIC S9(4) COMP-5 .
  2213     10 H-COM-LANGUAGE     PIC S9(4) COMP-5.
  2214     10 H-COM-COMAREALEN   PIC S9(4) COMP-5.
  2215     10   FILLER         PIC S9(4) COMP-5.
  2216     10 H-COM-MODE         PIC S9(4) COMP-5.
  2217     10 H-COM-LASTKEY      PIC S9(4) COMP-5.
  2218     10 H-COM-NUMERRS      PIC S9(4) COMP-5.
  2219     10   FILLER         PIC S9(4) COMP-5.
  2220     10   FILLER         PIC S9(4) COMP-5.
  2221     10 H-COM-KEYLABOPT    PIC S9(4) COMP-5.
  2222     10 H-COM-CFNAME       PIC X(15) .
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  40
* READVE3.cob (/home/prorosa/cobol/cpy/AREAHL)
  2223     10   FILLER         PIC X(1) .
  2224     10 H-COM-NFNAME       PIC X(15).
  2225     10   FILLER         PIC X(1) .
  2226     10 H-COM-REPEATOPT    PIC S9(4) COMP-5 .
  2227     10 H-COM-NFOPT        PIC S9(4) COMP-5 .
  2228     10   FILLER         PIC S9(4) COMP-5 .
  2229     10 H-COM-DBUFLEN      PIC S9(4) COMP-5 .
  2230     10   FILLER         PIC S9(4) COMP-5 .
  2231     10   FILLER         PIC S9(4) COMP-5 .
  2232     10 H-COM-DELETEFLAG   PIC S9(4) COMP-5.
  2233     10 H-COM-SHOWCONTROL  PIC S9(4) COMP-5 .
  2234     10   FILLER         PIC S9(4) COMP-5 .
  2235     10   FILLER         PIC S9(4) COMP-5 .
  2236     10   FILLER         PIC S9(4) COMP-5 .
  2237     10   FILLER         PIC S9(4) COMP-5 .
  2238     10 H-COM-FRMSTORSIZE  PIC S9(4) COMP-5 .
  2239     10   FILLER         PIC S9(4) COMP-5 .
  2240     10   FILLER         PIC S9(4) COMP-5 .
  2241     10   FILLER         PIC S9(4) COMP-5 .
  2242     10 H-COM-NUMRECS      PIC S9(9) COMP-5 .
  2243     10 H-COM-RECNUM       PIC S9(9) COMP-5 .
  2244     10   FILLER         PIC S9(4) COMP-5 .
  2245     10   FILLER         PIC S9(4) COMP-5 .
  2246     10 H-COM-TERMFILENUM  PIC S9(4) COMP-5 .
  2247     10   FILLER         PIC S9(4) COMP-5 .
  2248     10   FILLER         PIC S9(4) COMP-5 .
  2249     10   FILLER         PIC S9(4) COMP-5 .
  2250     10   FILLER         PIC S9(4) COMP-5 .
  2251     10   FILLER         PIC S9(4) COMP-5 .
  2252     10   FILLER         PIC S9(4) COMP-5 .
  2253     10 H-COM-TERMOPTIONS  PIC S9(4) COMP-5 .
  2254     10   FILLER         PIC S9(4) COMP-5 .
  2255     10   FILLER         PIC S9(4) COMP-5 .
  2256     10   FILLER         PIC S9(4) COMP-5 .
  2257     10   FILLER         PIC S9(4) COMP-5 .
  2258
  2259 01 AREA-SI
*                               COPY AREASI .
  2260       .
  2261*                                            *********************
  2262*                                            * Area dati SISTEMA *
  2263*                                            *********************
  2264*
  2265  05 SI-AREA-SISTEMA.
  2266   07 SI-DATI-UTENTE.
  2267    10 SI-SOTTOSISTEMA                     PIC XX.
  2268    10 SI-TIMEOUT                          PIC S9(9) COMP.
  2269    10 SI-STAMPANTE                        PIC X(8).
  2270    10 SI-STFORM                           PIC X(8).
  2271    10 SI-CODICE-AZIENDA                   PIC XX.
  2272    10 SI-DATA-DEL-GIORNO.
  2273     15 SI-FORMATO-INTERNO                 PIC 9(8).
  2274     15 FILLER                             PIC X(2).
  2275     15 SI-FORMATO-GG-MM-AA                PIC X(8).
  2276     15 SI-FORMATO-GG-MMM-AAAA             PIC X(12).
  2277     15 SI-FORMATO-GGMMAAAA                PIC X(8).
  2278     15 FILLER REDEFINES SI-FORMATO-GGMMAAAA.
  2279        20 FILLER                          PIC X(4).
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  41
* READVE3.cob (/home/prorosa/cobol/cpy/AREASI)
  2280        20 SI-AAAA                         PIC X(4).
  2281     15 SI-FORMATO-GGMMAA.
  2282      20 SI-GG                             PIC 99.
  2283      20 SI-MM                             PIC 99.
  2284      20 SI-AA                             PIC 99.
  2285     15 SI-FORMATO-GMA-9 REDEFINES SI-FORMATO-GGMMAA
  2286                                           PIC 9(6).
  2287    10 SI-PARAMETRI-UTE.
  2288     15 SI-TIPO                            PIC S9(4) COMP.
  2289        88 SI-PROGRAMMA-BATCH              VALUE 8.
  2290    10 SI-DATI-PROCESSO.
  2291     15 SI-DATI-PROG.
  2292      20 SI-NOME-PROG.
  2293       25 SI-NOME-PROC                     PIC XX.
  2294       25 FILLER                           PIC X(6).
  2295      20 SI-NOME-GROUP-PROG                PIC X(8).
  2296      20 SI-NOME-ACCT-PROG                 PIC X(8).
  2297     15 SI-GROUP                           PIC X(8).
  2298     15 SI-USER                            PIC X(8).
  2299     15 SI-ACCOUNT                         PIC X(8).
  2300     15 SI-SESSION-NAME                    PIC X(8).
  2301     15 SI-TERM                            PIC S9(4) COMP.
  2302     15 SI-NOME-FUNZIONE                   PIC X(8).
  2303     15 SI-CHIAVE-FUNZIONE                 PIC X(8).
  2304     15 SI-NOME-CHIAMANTE                  PIC X(28).
  2305        88 SI-CHIAMATO-DA-CI VALUES
  2306           "CI.PUB.SYS                  ",
  2307           "TOOLSET.PUB.SYS             ",
  2308           "QEDIT.PUB.ROBELLE           ".
  2309     15 FILLER REDEFINES SI-NOME-CHIAMANTE.
  2310      20 SI-PRIMI4-CHIAMANTE               PIC X(4).
  2311         88 SI-CHIAMATO-DA-MENU            VALUE "MENU".
  2312      20 FILLER                            PIC X(24).
  2313    10 SI-PAR-SISTEMA.
  2314      20 SI-AREA-UTENTE                    PIC X(20).
  2315      20 SI-FLAG-1                         PIC X.
  2316         88 LIRA-PESANTE                   VALUE "1" "2" "3" .
  2317      20 SI-FLAG-2                         PIC X.
  2318         88 SI-GIAC-NEGATIVE               VALUE "S".
  2319      20 SI-DEV-OUTCLASS                   PIC X(4).
  2320      20 SI-PRI-OUTCLASS                   PIC X.
  2321      20 SI-PRIORITA-OUT                   PIC X.
  2322      20 SI-PRIORITA-JOB                   PIC X.
  2323      20 SI-DATA-VALUTA                     PIC X.
  2324         88 SI-OBBLIGO-VALUTA          VALUE "X".
  2325      20 SI-DATA-RIFERIMENTO                PIC X.
  2326         88 SI-OBBLIGO-RIFERIMENTO     VALUE "X".
  2327      20  SI-FLAG-COMMESSA                  PIC X.
  2328         88 SI-OBBLIGO-COMMESSA        VALUE "X".
  2329      20 FLAG-NUM-EFFETTI                  PIC X.
  2330         88 NUM-EFFETTI-UNICA          VALUE "X".
  2331      20 SI-DIVISA-DEFAULT                 PIC X(4).
  2332      20 SI-PREVIS-OLTRE                   PIC X.
  2333         88 NO-PREVIS-DATA-OLTRE           VALUE "S".
  2334      20 FILLER                            PIC X(02).
  2335      20 SI-PAR-IVA                        PIC X(11).
  2336      20 SI-CODA-DEFAULT REDEFINES SI-PAR-IVA PIC X(11).
  2337      20 SI-DIVISA-EURO                    PIC X(4).
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  42
* READVE3.cob (/home/prorosa/cobol/cpy/AREASI)
  2338      20 SI-DATA-INIZIO-EURO               PIC X(8).
  2339      20 SI-DATA-INIZIO-EURO-NUM
  2340         REDEFINES SI-DATA-INIZIO-EURO     PIC 9(8).
  2341      20 SI-DATA-CONTAB-EURO               PIC X(8).
  2342      20 SI-DATA-CONTAB-EURO-NUM
  2343         REDEFINES SI-DATA-CONTAB-EURO     PIC 9(8).
  2344      20 SI-DIVISA-ITL                     PIC X(4).
  2345      20 FILLER                            PIC X(25).
  2346*
  2347   07 SI-AREE.
  2348    10 SI-DESCR-FUNZIONE                    PIC X(12).
  2349    10 SI-NR-FUNZIONE                       PIC S9(4) COMP.
  2350    10 SI-FLAG-DISABLETERM                  PIC X.
  2351       88 SI-DISABLETERM                    VALUES " ", "D".
  2352       88 SI-NOT-DISABLETERM                VALUE  "N".
  2353    10 SI-AREA-MSG                          PIC X(80).
  2354    10 SI-FLAG-BATCH                        PIC X(2).
  2355       88 SI-BATCH                          VALUE "BT".
  2356       88 SI-TP                             VALUE "TP".
  2357    10 SI-INFO-AREA-LIBERA                  PIC X(126).
  2358    10 FILLER REDEFINES SI-INFO-AREA-LIBERA.
  2359     15 SI-LINGUA-USER                      PIC X(4).
  2360     15 SI-CAMBIO-EURO-LIRA                 PIC 9(5)V9(6).
  2361     15 SI-DB-CONNESSO                      PIC X(30).
  2362     15 SI-PASSWORD                         PIC X(8).
  2363     15 SI-DIRECTORY                        PIC X(30).
  2364*
  2365   07 SI-AREE-PARAMETRI.
  2366    10 SI-PAR-SOTTOSISTEMA                  PIC X(80).
  2367    10 SI-PAR-FUNZIONE                      PIC X(80).
  2368    10 FILLER REDEFINES SI-PAR-FUNZIONE.
  2369     15 SI-FLAG-UPD                         PIC X.
  2370       88 SI-UPDATE                         VALUE SPACE.
  2371     15 SI-FLAG-DEL                         PIC X.
  2372       88 SI-DELETE                         VALUE SPACE.
  2373     15 SI-RESTO-PAR-FUN                    PIC X(78).
  2374*
  2375*
  2376 77 FLAG-CURSORE       PIC S9(4) COMP.
  2377   88 STOP-CURSORE VALUE 1.
  2378*
  2379 01 SQLCODE-MEM        PIC S9(9) COMP.
  2380*
  2381*
  2382*TASTO-PER-CONTINUARE*
  2383 01 TASTO-INP          PIC X.
  2384*
  2385*
  2386*
  2387* Comandi IGP
  2388 01 COM-IGP-STANDARD.
  2389  05 COMIGP-PTXSETUP   PIC X(10) VALUE "!PTX_SETUP".
  2390  05 COMIGP-PTXCFG2    PIC X(13) VALUE "CONFIG-LOAD;2".
  2391  05 COMIGP-PTXEND     PIC X(7)  VALUE "PTX_END".
  2392  05 COMIGP-CREATE     PIC X(15) VALUE "^CREATE;LAB;144".
  2393  05 COMIGP-EXECUTE    PIC X(14) VALUE "^EXECUTE;LAB;1".
  2394  05 COMIGP-SCALEDOT   PIC X(17) VALUE "SCALE;DOT;300;300".
  2395  05 COMIGP-ALPHA      PIC X(5)  VALUE "ALPHA".
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  43
* READVE3.cob
  2396  05 COMIGP-STOP       PIC X(4)  VALUE "STOP".
  2397  05 COMIGP-END        PIC X(3)  VALUE "END".
  2398  05 COMIGP-LISTEN     PIC X(7)  VALUE "^LISTEN".
  2399  05 COMIGP-QUIET      PIC X(6)  VALUE "^QUIET".
  2400*
  2401*
  2402 01 ROW-IGP            PIC 9(4).
  2403 01 ROW-X-IGP          PIC X(4).
  2404 01 COL-IGP            PIC 9(4).
  2405 01 COL-X-IGP          PIC X(4).
  2406 01 VERT-EXP-IGP       PIC 9.
  2407 01 ORIZ-EXP-IGP       PIC 9.
  2408*
  2409 01 TEMP-X-IN.
  2410   05 TEMP-EL-X OCCURS 4 PIC X.
  2411 01 TEMP-X-OUT           PIC X(4).
  2412 01 PRIMO-NONZERO        PIC S9(4) COMP.
  2413 01 K                    PIC S9(4) COMP.
  2414*
  2415 01 RIGA-PEND-COM      PIC X(33).
  2416*
  2417 01 IND-PEND           PIC S9(4) COMP.
  2418*S
  2419*MAXCA*
  2420  01 MAX-CAPI-INPUT.
  2421   05 MAX-CAPI-INPUT-R         PIC 9(6).
  2422*
  2423*VACO*
  2424 01 COSTO-MEM                  PIC S9(9) COMP.
  2425*VACO*
  2426*
  2427*MOVSKU
  2428 01  PY-INPUT-REC.
  2429     05  INPUT-VAL           PIC 9(14).
  2430    05 FILLER              PIC X VALUE ";".
  2431    05 INPUT-VAL-B        PIC 9(12).
  2432    05 FILLER              PIC X VALUE ";".
  2433    05 INPUT-VAL-C        PIC X(10).
  2434 01 PY-OUTPUT-REC.
  2435    05  OUTPUT-VAL-A        PIC X.
  2436    05  OUTPUT-VAL-B        PIC X(200).
  2437    05  OUTPUT-VAL-B-OK REDEFINES OUTPUT-VAL-B PIC X(13).
  2438 01 T-TAB                      PIC X VALUE X"9".
  2439 01 IND-BARUNI                 PIC S9(4) COMP.
  2440*MOVSKU
  2441 01  PY-INPUT-TRASF.
  2442     05  INPUT-RIF-TRASF    PIC X(12).
  2443     05  INPUT-CONTO-TRASF  PIC X(8).
  2444 01  PY-OUTPUT-TRASF.
  2445     05  OUTPUT-STATO-TRASF PIC XX.
  2446     05  OUTPUT-ERR-TRASF   PIC X(70).
  2447 01 PY-INPUT-REC-DISIMPEGNA.
  2448    05 LISTA-AS               OCCURS 20.
  2449      10 AS-DISIMPEGNA.
  2450        15 ANNO-DISIMPEGNA        PIC X.
  2451        15 STAG-DISIMPEGNA        PIC X.
  2452    05 MAG-DISIMPEGNA         PIC XXX.
  2453    05 FORN-DISIMPEGNA        PIC X.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  44
* READVE3.cob
  2454    05 LISTA-CLASSE-DISIMPEGNA.
  2455      10 CLASSE-DISIMPEGNA       PIC XX OCCURS 99.
  2456 01 PY-OUTPUT-DISIMPEGNO      PIC XX.
  2457    88 PY-OUTPUT-DISIMPEGNO-OK VALUE "OK".
  2458*ASOLOB2C*
  2459 01 FLAG-B2C-NO-DT     PIC X.
  2460  88 B2C-NO-DT   VALUES 'S'.
  2461 01 NUM-B2C-NO-DT       PIC S9(4) COMP.
  2462 01 IND-B2C-NO-DT       PIC S9(4) COMP.
  2463 01 ANACST-MAG-COM      PIC S9(4) COMP.
  2464 01 ANACST-C-MAT-COM    PIC S9(15) COMP-3.
  2465 01 ANACST-CST-COM    PIC S9(9) COMP.
  2466 01 FLAG-ANACST        PIC X.
  2467  88 ESISTE-ANACST VALUE "E".
  2468  88 NON-ESISTE-ANACST VALUE " ".
  2469 01  PY-INPUT-REC-B.
  2470     05  INPUT-RIF-INTR     PIC X(12).
  2471     05  INPUT-FLAG         PIC X(1).
  2472     05  INPUT-CONTO        PIC X(8).
  2473     05  INPUT-MAG          PIC X(4).
  2474 01  PY-OUTPUT-REC-B.
  2475     05  OUTPUT-VAL         PIC X(2).
  2476*ASOLOB2C*
  2477*
  2478 LINKAGE SECTION.
  2479*
  2480 01 W-COMMON
*                  COPY WCOMMONW.
  2481     .                                                            WCOMMONW
  2482*IF  X9=OFF                                                       WCOMMONW
  2483*CONTROL NOLIST                                                   WCOMMONW
  2484*IF                                                               WCOMMONW
  2485   15   W-SOTTOSISTEMA              PIC 99.                       WCOMMONW
  2486   15   W-DATI-W-IMAGE.                                           WCOMMONW
  2487    20   W-NOME-DATA-BASE-1         PIC X(16).                    WCOMMONW
  2488    20   W-TAB-DB.                                                WCOMMONW
  2489     25   W-NOME-DB                 PIC X(16) OCCURS 8.           WCOMMONW
  2490    20   FILLER REDEFINES W-TAB-DB.                               WCOMMONW
  2491     25   W-NOME-DB-1               PIC X(16).                    WCOMMONW
  2492     25   W-NOME-DB-2               PIC X(16).                    WCOMMONW
  2493     25   W-NOME-DB-3               PIC X(16).                    WCOMMONW
  2494     25   W-NOME-DB-4               PIC X(16).                    WCOMMONW
  2495     25   W-NOME-DB-5               PIC X(16).                    WCOMMONW
  2496     25   W-NOME-DB-6               PIC X(16).                    WCOMMONW
  2497     25   W-NOME-DB-7               PIC X(16).                    WCOMMONW
  2498     25   W-NOME-DB-8               PIC X(16).                    WCOMMONW
  2499    20   W-MODO                     PIC S9(4) COMP.               WCOMMONW
  2500    20   W-NOME-CAMPO               PIC X(16).                    WCOMMONW
  2501    20   W-VALORE-CAMPO             PIC S9(15) COMP-3.            WCOMMONW
  2502    20   W-DB-KEY-P16 REDEFINES W-VALORE-CAMPO                    WCOMMONW
  2503                                    PIC S9(15) COMP-3.            WCOMMONW
  2504    20   W-VAL-1 REDEFINES W-VALORE-CAMPO.                        WCOMMONW
  2505     25   W-VALORE-CAMPO-W          PIC S9(9) COMP.               WCOMMONW
  2506     25   W-DB-KEY-W REDEFINES W-VALORE-CAMPO-W                   WCOMMONW
  2507                                    PIC S9(9) COMP.               WCOMMONW
  2508     25   FILLER                    PIC X(4).                     WCOMMONW
  2509    20   W-VAL-2 REDEFINES W-VALORE-CAMPO.                        WCOMMONW
  2510     25   W-VALORE-CAMPO-HW         PIC S9(4) COMP.               WCOMMONW
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  45
* READVE3.cob (/home/prorosa/cobol/cpy/WCOMMONW)
  2511     25   W-DB-KEY-HW REDEFINES W-VALORE-CAMPO-HW                 WCOMMONW
  2512                                    PIC S9(4) COMP.               WCOMMONW
  2513     25   FILLER                    PIC X(6).                     WCOMMONW
  2514    20   W-VAL-3 REDEFINES W-VALORE-CAMPO.                        WCOMMONW
  2515     25   W-VALORE-CAMPO-X4         PIC X(4).                     WCOMMONW
  2516     25   W-DB-KEY-X4 REDEFINES W-VALORE-CAMPO-X4                 WCOMMONW
  2517                                    PIC X(4).                     WCOMMONW
  2518     25   FILLER                    PIC X(4).                     WCOMMONW
  2519    20   W-VAL-4 REDEFINES W-VALORE-CAMPO.                        WCOMMONW
  2520     25   W-VALORE-CAMPO-12         PIC S9(11) COMP-3.            WCOMMONW
  2521     25   FILLER                    PIC XX.                       WCOMMONW
  2522    20   W-VAL-5 REDEFINES W-VALORE-CAMPO.                        WCOMMONW
  2523     25   W-VALORE-CAMPO-15         PIC S9(15) COMP.              WCOMMONW
  2524    20   W-NOME-DATA-SET            PIC X(8).                     WCOMMONW
  2525    20   W-PAROLA-CHIAVE-1          PIC X(8).                     WCOMMONW
  2526    20   W-TUTTO-RECORD             PIC XX.                       WCOMMONW
  2527    20   W-CHIUSURA-TOTALE          PIC XX.                       WCOMMONW
  2528*                                                                 WCOMMONW
  2529    20   W-CA-IMAGE.                                              WCOMMONW
  2530     25   W-STATUS-WORD-IMAGE       PIC S9(4) COMP.               WCOMMONW
  2531        88  W-OK-IMAGE              VALUE  0.                     WCOMMONW
  2532        88  W-ERRORI-TRAGICI        VALUE  -9999 THRU -1.         WCOMMONW
  2533        88  W-INIZIO-FILE           VALUE  10.                    WCOMMONW
  2534        88  W-FINE-FILE             VALUE  11.                    WCOMMONW
  2535        88  W-FUORI-FILE            VALUE  12 , 13.               WCOMMONW
  2536        88  W-INIZIO-CATENA         VALUE  14.                    WCOMMONW
  2537        88  W-FINE-CATENA           VALUE  15.                    WCOMMONW
  2538        88  W-DATA-SET-PIENO        VALUE  16.                    WCOMMONW
  2539        88  W-REC-NON-TROVATO       VALUE  17.                    WCOMMONW
  2540        88  W-ACCESSO-PROTETTO      VALUE  20.                    WCOMMONW
  2541        88  W-CHIAVE-DOPPIA         VALUE  43.                    WCOMMONW
  2542        88  W-CATENA-NON-ESAURITA   VALUE  44.                    WCOMMONW
  2543        88  W-CATENA-PIENA          VALUE 200 THRU 299.           WCOMMONW
  2544        88  W-MASTER-PIENO          VALUE 300 THRU 399.           WCOMMONW
  2545     25   W-WORD-L-BUFF             PIC S9(4) COMP.               WCOMMONW
  2546     25   W-WORD-ATT                PIC S9(9) COMP.               WCOMMONW
  2547     25   W-WORD-SIN                PIC S9(9) COMP.               WCOMMONW
  2548          88 W-CATENA-VUOTA         VALUE 0.                      WCOMMONW
  2549     25   W-WORD-PREC               PIC S9(9) COMP.               WCOMMONW
  2550     25   W-WORD-SEG                PIC S9(9) COMP.               WCOMMONW
  2551*                                                                 WCOMMONW
  2552   15   W-DATI-W-DEL.                                             WCOMMONW
  2553    20   W-CA-DEL.                                                WCOMMONW
  2554     25   W-STATUS-WORD-DEL         PIC S9(4) COMP.               WCOMMONW
  2555        88  W-OK-DEL                VALUE  0.                     WCOMMONW
  2556        88  W-ERRORE-DEL            VALUE  -1.                    WCOMMONW
  2557        88  W-ERRORE-FILE           VALUE  0 THRU 999.            WCOMMONW
  2558        88  W-ERRORE-SHOW           VALUE  1000 THRU 9999.        WCOMMONW
  2559        88  W-FINE-MESSAGGIO        VALUE  -2.                    WCOMMONW
  2560        88  W-ERRORE-EDIT           VALUE  -1.                    WCOMMONW
  2561        88  W-ERRORE-EDIT-SPECIALE  VALUE  -3.                    WCOMMONW
  2562        88  W-FLAG-EDIT             VALUE  -3 THRU 0.             WCOMMONW
  2563        88  W-ERRORI-TRAGICI-DEL    VALUE  -2007 THRU -1000.      WCOMMONW
  2564        88  W-ERRORE-DEL-TRASMIS    VALUE -2001.                  WCOMMONW
  2565         25        W-SW.                                          WCOMMONW
  2566    30  W-SW-1                          PIC S9(4) COMP.           WCOMMONW
  2567    30  W-SW-2                      PIC  S9(4) COMP.              WCOMMONW
  2568    30  W-SW-3                      PIC S9(4) COMP.               WCOMMONW
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  46
* READVE3.cob (/home/prorosa/cobol/cpy/WCOMMONW)
  2569    30  W-SW-4                      PIC S9(4) COMP.               WCOMMONW
  2570    30  W-SW-5                      PIC S9(4) COMP.               WCOMMONW
  2571    30  W-SW-6                      PIC S9(4) COMP.               WCOMMONW
  2572    30  W-SW-7                      PIC S9(4) COMP.               WCOMMONW
  2573    30  W-SW-8                      PIC S9(4) COMP.               WCOMMONW
  2574    30  W-SW-9                      PIC S9(4) COMP.               WCOMMONW
  2575    30  W-SW-10                     PIC S9(4) COMP.               WCOMMONW
  2576     25   FILLER                    PIC X(234).                   WCOMMONW
  2577    20   W-NOME-MODULO              PIC X(16).                    WCOMMONW
  2578    20   W-LL-MODULO                PIC S9(4) COMP.               WCOMMONW
  2579    20   W-NOME-PROX-MODULO         PIC X(16).                    WCOMMONW
  2580    20   W-CAMPO-ERRATO.                                          WCOMMONW
  2581     25   W-RIGA                    PIC  999.                     WCOMMONW
  2582     25   W-COLONNA                 PIC  999.                     WCOMMONW
  2583     25   FILLER                    PIC X(66).                    WCOMMONW
  2584     25   W-SYSMODULI               PIC X(36).                    WCOMMONW
  2585     25   W-PROX-MODULO             PIC X(16).                    WCOMMONW
  2586     25   W-TERMINALE               PIC X(8).                     WCOMMONW
  2587*                                                                 WCOMMONW
  2588   15   W-DATI-VIEW REDEFINES W-DATI-W-DEL.                       WCOMMONW
  2589    20   W-CA-VIEW.                                               WCOMMONW
  2590     25   W-CSTATUS                 PIC S9(4) COMP-5.             WCOMMONW
  2591        88 W-OK-VIEW                VALUE 0.                      WCOMMONW
  2592     25   W-LINGUAGGIO              PIC S9(4) COMP-5.             WCOMMONW
  2593        88  W-COBOL                 VALUE  0.                     WCOMMONW
  2594     25   W-LL-CA-VIEW              PIC S9(4) COMP-5.             WCOMMONW
  2595     25   W-EXT-BASIC               PIC S9(4) COMP-5.             WCOMMONW
  2596     25   W-COLLECT-BROWSE          PIC S9(4) COMP-5.             WCOMMONW
  2597        88  W-COLLECT               VALUE 0.                      WCOMMONW
  2598        88  W-BROWSE                VALUE 1.                      WCOMMONW
  2599     25   W-ULT-TASTO-FUNZ          PIC S9(4) COMP-5.             WCOMMONW
  2600        88  W-F0                    VALUE 0.                      WCOMMONW
  2601        88  W-F1                    VALUE 1.                      WCOMMONW
  2602        88  W-F2                    VALUE 2.                      WCOMMONW
  2603        88  W-F3                    VALUE 3.                      WCOMMONW
  2604        88  W-F4                    VALUE 4.                      WCOMMONW
  2605        88  W-F5                    VALUE 5.                      WCOMMONW
  2606        88  W-F6                    VALUE 6.                      WCOMMONW
  2607        88  W-F7                    VALUE 7.                      WCOMMONW
  2608        88  W-F8                    VALUE 8.                      WCOMMONW
  2609     25   W-NUMERO-ERRORI           PIC S9(4) COMP-5.             WCOMMONW
  2610        88  W-NO-ERR                VALUE 0.                      WCOMMONW
  2611        88  W-ERRORI-FORMALI        VALUE 9999.                   WCOMMONW
  2612     25   W-WIND-EN                 PIC S9(4) COMP-5.             WCOMMONW
  2613     25   FILLER REDEFINES W-WIND-EN.                             WCOMMONW
  2614      30   W-FILLER                 PIC X.                        WCOMMONW
  2615      30   W-WINDOW-ENH             PIC X.                        WCOMMONW
  2616     25   W-MULTIUSAGE              PIC S9(4) COMP-5.             WCOMMONW
  2617     25   W-LABEL-OPTION            PIC S9(4) COMP-5.             WCOMMONW
  2618     25   W-NOME-CFORM              PIC X(15).                    WCOMMONW
  2619     25   W-FILLER                  PIC X.                        WCOMMONW
  2620     25   W-NOME-NFORM              PIC X(15).                    WCOMMONW
  2621     25   W-FILLER                  PIC X.                        WCOMMONW
  2622     25   W-FLAG-REPEAT             PIC S9(4) COMP-5.             WCOMMONW
  2623        88  W-NORMAL                VALUE 0.                      WCOMMONW
  2624        88  W-REPEAT                VALUE 1.                      WCOMMONW
  2625        88  W-V-REPEAT-APP          VALUE 2.                      WCOMMONW
  2626     25   W-FLAG-FREEZE             PIC S9(4) COMP-5.             WCOMMONW
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  47
* READVE3.cob (/home/prorosa/cobol/cpy/WCOMMONW)
  2627        88  W-CLEAR                 VALUE 0.                      WCOMMONW
  2628        88  W-APP-NEXT              VALUE 1.                      WCOMMONW
  2629        88  W-FREEZE-APP            VALUE 2.                      WCOMMONW
  2630     25   W-NR-RIGHE-CFORM          PIC S9(4) COMP-5.             WCOMMONW
  2631     25   W-LL-BUFFER               PIC S9(4) COMP-5.             WCOMMONW
  2632     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2633     25   W-LOOK-AHEAD              PIC S9(4) COMP-5.             WCOMMONW
  2634     25   W-DELETE-FLAG             PIC S9(4) COMP-5.             WCOMMONW
  2635     25   W-SHOWCONTROL             PIC S9(4) COMP-5.             WCOMMONW
  2636        88  W-DFORM-BIT15           VALUE 1, 129.                 WCOMMONW
  2637        88  W-DDATI-BIT14           VALUE 2, 130.                 WCOMMONW
  2638        88  W-DWIND-BIT13           VALUE 4, 132.                 WCOMMONW
  2639     25   W-FILLER                  PIC S9(4) COMP-5.             WCOMMONW
  2640     25   W-PRINTFILE-NUM           PIC S9(4) COMP-5.             WCOMMONW
  2641     25   W-FILERRNUM               PIC S9(4) COMP-5.             WCOMMONW
  2642     25   W-ERRFILNUM               PIC S9(4) COMP-5.             WCOMMONW
  2643     25   W-FORM-STORE-SIZE         PIC S9(4) COMP-5.             WCOMMONW
  2644     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2645     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2646     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2647     25   W-NUM-RECS                PIC S9(9) COMP-5.             WCOMMONW
  2648     25   W-RECNUM                  PIC S9(9) COMP-5.             WCOMMONW
  2649     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2650     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2651     25   W-TERMFILENUM             PIC S9(4) COMP-5.             WCOMMONW
  2652     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2653     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2654     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2655     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2656     25   FILLER                    PIC S9(4) COMP-5.             WCOMMONW
  2657     25   W-RETRIES                 PIC S9(4) COMP-5.             WCOMMONW
  2658     25   W-OPTIONS                 PIC S9(4) COMP-5.             WCOMMONW
  2659     25   W-ENVIRON                 PIC S9(4) COMP-5.             WCOMMONW
  2660     25   W-USER-TIME               PIC S9(4) COMP-5.             WCOMMONW
  2661     25   W-TERM-IDENTIFIER         PIC S9(4) COMP-5.             WCOMMONW
  2662          88 W-TERM-2624            VALUE 9, 13.                  WCOMMONW
  2663     25   W-LAB-INFO                PIC S9(4) COMP-5.             WCOMMONW
  2664*                                                                 WCOMMONW
  2665    20   W-NOME-FORMSFILE           PIC X(20).                    WCOMMONW
  2666    20   W-NOME-TERMINALE           PIC X(10).                    WCOMMONW
  2667    20   W-NOME-FORM-TEST           PIC X(14).                    WCOMMONW
  2668    20   W-NOME-FORM-DETT           PIC X(14).                    WCOMMONW
  2669    20   W-LL-FIELD                 PIC S9(4) COMP-5.             WCOMMONW
  2670    20   W-FLAGS-STD.                                             WCOMMONW
  2671     25   W-FLAG-ERRORI-STD         PIC S9(4) COMP.               WCOMMONW
  2672        88 W-NO-ERRORI              VALUE 0.                      WCOMMONW
  2673        88 W-ERR-TESTATA            VALUE 1.                      WCOMMONW
  2674        88 W-ERR-DETTAGLIO          VALUE 2.                      WCOMMONW
  2675     25  W-FLAG-COMANDO             PIC S9(4) COMP-5.             WCOMMONW
  2676        88 W-NO-COMANDO             VALUE 0.                      WCOMMONW
  2677        88 W-COMANDO-OK             VALUE 1.                      WCOMMONW
  2678        88 W-COMANDO-NO-OK          VALUE -1.                     WCOMMONW
  2679     25  W-FLAG-RICERCA             PIC S9(4) COMP-5.             WCOMMONW
  2680    20   W-NR-FIELD                 PIC S9(4) COMP-5.             WCOMMONW
  2681    20   W-LL-FIELD-MOSSO           PIC S9(4) COMP-5.             WCOMMONW
  2682    20   W-NR-PROX-FIELD            PIC S9(4) COMP-5.             WCOMMONW
  2683    20   W-PRINT-C                  PIC S9(4) COMP-5.             WCOMMONW
  2684    20   W-PAGE-C                   PIC S9(4) COMP-5.             WCOMMONW
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  48
* READVE3.cob (/home/prorosa/cobol/cpy/WCOMMONW)
  2685*                                                                 WCOMMONW
  2686    20   W-DATI-WINDOW.                                           WCOMMONW
  2687     25   W-LL-MAX-WINDOW           PIC S9(4) COMP-5.             WCOMMONW
  2688     25   W-LL-WINDOW               PIC S9(4) COMP-5.             WCOMMONW
  2689     25   W-MESS-X-WINDOW.                                        WCOMMONW
  2690      30   W-FIL-WINDOW             PIC X(54).                    WCOMMONW
  2691      30   W-RESTO-WINDOW           PIC X(24).                    WCOMMONW
  2692*                                                                 WCOMMONW
  2693    20   W-CAMPO-ERR.                                             WCOMMONW
  2694     25   W-FIL-ERR                 PIC X.                        WCOMMONW
  2695     25   W-NUM-ERR                 PIC Z(15)9.                   WCOMMONW
  2696     25   W-NUM-ERR-X REDEFINES W-NUM-ERR PIC X(16).              WCOMMONW
  2697     25   W-FILLER                  PIC X.                        WCOMMONW
  2698    20   W-LL-CAMPO-ERR             PIC S9(4) COMP.               WCOMMONW
  2699    20   W-LL-CAMPO-ERR-2           PIC S9(4) COMP.               WCOMMONW
  2700*                                                                 WCOMMONW
  2701    20   W-CAMPI-FLAG-STD.                                        WCOMMONW
  2702     25   W-FLAG-FINISH-TEST        PIC S9(4) COMP.               WCOMMONW
  2703     25   W-FLAG-FINISH-DETT        PIC S9(4) COMP.               WCOMMONW
  2704          88 W-FINISH-DETT          VALUE 1, 3.                   WCOMMONW
  2705          88 W-DETT-CIECO           VALUE 2, 3.                   WCOMMONW
  2706     25   W-NUMERO-FORMS            PIC S9(4) COMP.               WCOMMONW
  2707     25   W-FLAG-LOOP-TEST          PIC S9(4) COMP.               WCOMMONW
  2708     25   W-FLAG-TIPO-AGG           PIC S9(4) COMP.               WCOMMONW
  2709        88  W-AGG-FINALE            VALUE 1.                      WCOMMONW
  2710        88  W-AGG-PER-PAG           VALUE 2.                      WCOMMONW
  2711*                                                                 WCOMMONW
  2712    20   W-FLAG-CHIUDI-TEST             PIC S9(4)  COMP.          WCOMMONW
  2713     88   W-FINE-TESTATA              VALUE 1.                    WCOMMONW
  2714   20   W-FLAG-CHIUDI-DETT          PIC  S9(4)  COMP.             WCOMMONW
  2715     88   W-FINE-DETTAGLIO          VALUE 1.                      WCOMMONW
  2716   20  W-LL-DISPLAY               PIC S9(4) COMP.                 WCOMMONW
  2717   20  W-CTL-DISPLAY              PIC S9(4) COMP.                 WCOMMONW
  2718     20   W-CAMPO-SIGLA.                                          WCOMMONW
  2719      30   W-PAR-TESTP.                                           WCOMMONW
  2720      35   W-LL-MAX-SIGLA           PIC S9(4) COMP.               WCOMMONW
  2721      35   W-LL-SIGLA               PIC S9(4) COMP.               WCOMMONW
  2722      30   W-SIGLA-OUT.                                           WCOMMONW
  2723       35   W-POS-RC                PIC X(9).                     WCOMMONW
  2724       35   FILLER                  PIC XX.                       WCOMMONW
  2725       35   W-SIGLA-AZ              PIC X(8).                     WCOMMONW
  2726       35   FILLER                  PIC XX.                       WCOMMONW
  2727      30  FILLER REDEFINES W-SIGLA-OUT.                           WCOMMONW
  2728*      35  FILLER                   PIC X(20).                    WCOMMONW
  2729    35   W-CAMPO-COMANDO.                                         WCOMMONW
  2730     45   W-CAMPO-PUNTO             PIC X.                        WCOMMONW
  2731     45   W-FILLER                  PIC X.                        WCOMMONW
  2732          88 W-PAGINAZ              VALUE "P".                    WCOMMONW
  2733     45   W-FIL-COM-SPACE.                                        WCOMMONW
  2734      47  W-COMAND-SPACE-1          PIC X.                        WCOMMONW
  2735          88 W-P-1-9                VALUE "1" THRU "9".           WCOMMONW
  2736      47  W-COMAND-SPACE-2          PIC X.                        WCOMMONW
  2737          88 W-P-10-99              VALUE "0" THRU "9".           WCOMMONW
  2738     20   FILLER                    PIC X.                        WCOMMONW
  2739     20   W-PAR-SOPTV.                                            WCOMMONW
  2740      25   W-FUNZ-SOPTV             PIC S9(4) COMP.               WCOMMONW
  2741      25   W-STATO-SOPTV            PIC S9(4) COMP.               WCOMMONW
  2742      25   W-TIPO-SOPTV             PIC S9(4) COMP.               WCOMMONW
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  49
* READVE3.cob (/home/prorosa/cobol/cpy/WCOMMONW)
  2743      25   FILLER                   PIC X.                        WCOMMONW
  2744      25   W-RIGA-V                 PIC 99.                       WCOMMONW
  2745      25   FILLER                   PIC X.                        WCOMMONW
  2746      25   W-COLONNA-V              PIC 99.                       WCOMMONW
  2747*                                                                 WCOMMONW
  2748   15   W-MESSAGGI-COMANDO.                                       WCOMMONW
  2749    20   W-ULT-MESS-COMANDO         PIC X(4).                     WCOMMONW
  2750        88  W-CHIUDI-SOTTOSISTEMA   VALUE  ".CS ".                WCOMMONW
  2751        88  W-CHIUDI-FUNZIONE       VALUE  ".CF ".                WCOMMONW
  2752        88  W-CHIUDI-OPERAZIONE     VALUE  ".CO ".                WCOMMONW
  2753        88  W-ANNULLA-1               VALUE  ".A  ".              WCOMMONW
  2754        88  W-PROSEGUI-1            VALUE  ".P  ".                WCOMMONW
  2755        88  W-VARIA                 VALUE  ".V  ".                WCOMMONW
  2756        88  W-PAG-PRECEDENTE        VALUE  ".PP ".                WCOMMONW
  2757        88  W-PAG-SEGUENTE          VALUE  ".PS ".                WCOMMONW
  2758         88 W-INSERISCI             VALUE  ".I  ".                WCOMMONW
  2759         88 W-ANNULLA-RIGA          VALUE  ".AR ".                WCOMMONW
  2760         88 W-STAMPA                VALUE ".S  ".                 WCOMMONW
  2761    20 FILLER REDEFINES W-ULT-MESS-COMANDO.                       WCOMMONW
  2762     25 W-ULT-MESS-1                PIC XX.                       WCOMMONW
  2763      88 W-CHIUDI                   VALUE ".C".                   WCOMMONW
  2764      88 W-ANNULLA                  VALUE ".A".                   WCOMMONW
  2765      88 W-PROSEGUI                 VALUE ".P".                   WCOMMONW
  2766     25 W-NR-PAGINA-10-99           PIC 99.                       WCOMMONW
  2767     25 FILLER REDEFINES W-NR-PAGINA-10-99.                       WCOMMONW
  2768      30 W-NR-PAGINA-1-9            PIC 9.                        WCOMMONW
  2769      30 FILLER                     PIC X.                        WCOMMONW
  2770    20 FILLER REDEFINES W-ULT-MESS-COMANDO.                       WCOMMONW
  2771     25 W-ULT-MESS-2                PIC X.                        WCOMMONW
  2772      88 W-COMANDO                  VALUE ".".                    WCOMMONW
  2773     25 FILLER                      PIC X.                        WCOMMONW
  2774     25 FILLER                      PIC X.                        WCOMMONW
  2775       88 W-PAGINA-1-9              VALUE "1" THRU "9".           WCOMMONW
  2776     25 FILLER                      PIC X.                        WCOMMONW
  2777      88 W-PAGINA-10-99             VALUE "0" THRU "9".           WCOMMONW
  2778    20   W-ZONA-MESSAGGI-FISSI.                                   WCOMMONW
  2779      25 W-ZONA-MESSAGGI-PUNTO.                                   WCOMMONW
  2780       30 W-NR-MESS-PUNTO           PIC S9(4)  COMP.              WCOMMONW
  2781      30  W-PUNTO-CS.                                             WCOMMONW
  2782       35  W-PUNTO                  PIC X.                        WCOMMONW
  2783       35  FILLER                   PIC XXX.                      WCOMMONW
  2784      30  W-PUNTO-CF                PIC X(4).                     WCOMMONW
  2785      30  W-PUNTO-CO                PIC X(4).                     WCOMMONW
  2786      30  W-PUNTO-A                 PIC X(4).                     WCOMMONW
  2787      30  W-PUNTO-P                 PIC X(4).                     WCOMMONW
  2788      30  W-PUNTO-V                 PIC X(4).                     WCOMMONW
  2789      30  W-PUNTO-PP                PIC X(4).                     WCOMMONW
  2790      30  W-PUNTO-PS                PIC X(4).                     WCOMMONW
  2791      30  W-PUNTO-I                 PIC X(4).                     WCOMMONW
  2792      30  W-PUNTO-AR                PIC X(4).                     WCOMMONW
  2793    25 FILLER REDEFINES W-ZONA-MESSAGGI-PUNTO.                    WCOMMONW
  2794     30 FILLER                      PIC XX.                       WCOMMONW
  2795     30   W-MESS-FISSO    PIC X(4) OCCURS 10                      WCOMMONW
  2796                            INDEXED BY  I-W-MESS-FISSO.           WCOMMONW
  2797*                                                                 WCOMMONW
  2798    15  W-FLAG-LAVORO               PIC S9(4)  COMP.              WCOMMONW
  2799     88  W-FINE-LAVORO              VALUE 1.                      WCOMMONW
  2800     88  W-LAVORO-CONTINUO          VALUE 0.                      WCOMMONW
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  50
* READVE3.cob (/home/prorosa/cobol/cpy/WCOMMONW)
  2801   15   W-DATI-GENERALI.                                          WCOMMONW
  2802    20   W-SIGLA-TERMINALE          PIC X(4).                     WCOMMONW
  2803    20   W-SIGLA-OUT-ASSOCIATO.                                   WCOMMONW
  2804      25    W-TIPO-TER       PIC XX.                              WCOMMONW
  2805      25    W-NR-OUT-ASSOCIATO    PIC XX.                         WCOMMONW
  2806    20   W-TIPO-ASSOCIAZIONE        PIC XX.                       WCOMMONW
  2807    20   W-INDICI-W-COMMON.                                       WCOMMONW
  2808     25   W-INDICE-1                PIC S9(4) COMP.               WCOMMONW
  2809     25   W-INDICE-2                PIC S9(4) COMP.               WCOMMONW
  2810     25   W-INDICE-3                PIC S9(4) COMP.               WCOMMONW
  2811     25   W-INDICE-4                PIC S9(4) COMP.               WCOMMONW
  2812     25   W-INDICE-5                PIC S9(4) COMP.               WCOMMONW
  2813     25   W-INDICE-6                PIC S9(4) COMP.               WCOMMONW
  2814     25   W-INDICE-7                PIC S9(4) COMP.               WCOMMONW
  2815     25   W-INDICE-8                PIC S9(4) COMP.               WCOMMONW
  2816*           USATO COME FLAG PER ROUTINES DEL E IMAGE              WCOMMONW
  2817    20   W-INDICI-W-COMMON-RID REDEFINES W-INDICI-W-COMMON.       WCOMMONW
  2818     25   W-INDICE                  PIC S9(4) COMP OCCURS 8       WCOMMONW
  2819                                    INDEXED BY I-W-INDICE.        WCOMMONW
  2820    20   W-SIGLA-UTENTE             PIC X(8).                     WCOMMONW
  2821    20   W-DATA-DEL-GIORNO.                                       WCOMMONW
  2822     25   W-FORMATO-INTERNO         PIC 9(8) COMP.                WCOMMONW
  2823     25   W-FORMATO-ESTERNO-1       PIC X(8).                     WCOMMONW
  2824     25   W-FORMATO-GG-MM-AA REDEFINES W-FORMATO-ESTERNO-1        WCOMMONW
  2825                                    PIC X(8).                     WCOMMONW
  2826     25   W-FORMATO-ESTERNO-2       PIC X(12).                    WCOMMONW
  2827     25 W-FORMATO-GG-MMM-AAAA REDEFINES W-FORMATO-ESTERNO-2       WCOMMONW
  2828                                    PIC X(12).                    WCOMMONW
  2829     25 W-FORMATO-GGMMAA.                                         WCOMMONW
  2830       30  W-GG           PIC 99.                                 WCOMMONW
  2831       30 W-MM            PIC 99.                                 WCOMMONW
  2832       30 W-AA            PIC 99.                                 WCOMMONW
  2833    20   W-PARAMETRI-UTE.                                         WCOMMONW
  2834     25   W-CODICE-FISCALE          PIC X(16).                    WCOMMONW
  2835     25   W-CONTI-BASE.                                           WCOMMONW
  2836      30   W-CLIENTI-ITA            PIC 9(4) COMP.                WCOMMONW
  2837      30   W-CLIENTI-EXP            PIC 9(4) COMP.                WCOMMONW
  2838      30   W-FORNITORI              PIC 9(4) COMP.                WCOMMONW
  2839      30   W-AGENTI-ITA             PIC 9(4) COMP.                WCOMMONW
  2840      30   W-AGENTI-EXP             PIC 9(4) COMP.                WCOMMONW
  2841      30   W-PORTAF-ITA             PIC 9(4) COMP.                WCOMMONW
  2842      30   W-PORTAF-EXP             PIC 9(4) COMP.                WCOMMONW
  2843      30   W-I-V-A                  PIC 9(4) COMP.                WCOMMONW
  2844      30   W-RICAVI-ITA-INV         PIC 9(4) COMP.                WCOMMONW
  2845      30   W-RICAVI-ITA-PE          PIC S9(4) COMP.               WCOMMONW
  2846      30   W-RICAVI-EXP-INV         PIC S9(4) COMP.               WCOMMONW
  2847      30   W-RICAVI-EXP-PE          PIC S9(4) COMP.               WCOMMONW
  2848      30   W-RICAVI-MP              PIC 9(4) COMP.                WCOMMONW
  2849      30   W-TRASPORTI              PIC 9(4) COMP.                WCOMMONW
  2850      30   W-IMBALLI                PIC 9(4) COMP.                WCOMMONW
  2851      30   W-SCONTI                 PIC 9(4) COMP.                WCOMMONW
  2852      30   W-BANCHE                 PIC 9(4) COMP.                WCOMMONW
  2853      30   W-DEBIT-DIV              PIC 9(4) COMP.                WCOMMONW
  2854      30   W-CREDIT-DIV             PIC 9(4) COMP.                WCOMMONW
  2855      30   W-EFFETTI-SCONTO         PIC 9(4) COMP.                WCOMMONW
  2856      30   W-EFFETTI-SBF            PIC 9(4) COMP.                WCOMMONW
  2857      30   W-EFFETTI-DOPO-INCASSO   PIC 9(4) COMP.                WCOMMONW
  2858      30   W-SPESE-VARIE-BOLLI      PIC 9(4) COMP.                WCOMMONW
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  51
* READVE3.cob (/home/prorosa/cobol/cpy/WCOMMONW)
  2859      30   W-RICAVI-CAUZ            PIC 9(4) COMP.                WCOMMONW
  2860      30   W-COSTI                  PIC 9(4) COMP.                WCOMMONW
  2861      30   W-FORN-ANTICIPI          PIC 9(4) COMP.                WCOMMONW
  2862      30   W-CLIENTI-ANTICIPI       PIC 9(4) COMP.                WCOMMONW
  2863      30   W-CONTO-RAG-1            PIC 9(4) COMP.                WCOMMONW
  2864      30   W-CONTO-RAG-2            PIC 9(4) COMP.                WCOMMONW
  2865      30   W-PERDITE-PROFITTI       PIC 9(4) COMP.                WCOMMONW
  2866      30   W-BILANCIO-CHIUSURA      PIC 9(4) COMP.                WCOMMONW
  2867      30   W-BILANCIO-APERTURA      PIC 9(4) COMP.                WCOMMONW
  2868     25   W-CONTI-BASE-RID REDEFINES W-CONTI-BASE.                WCOMMONW
  2869      30   W-CONTO-BASE             PIC 9(4) COMP OCCURS 32       WCOMMONW
  2870                                    INDEXED BY I-W-CONTO-BASE.    WCOMMONW
  2871     25   W-TIPO-LANCIO             PIC S9(4) COMP.               WCOMMONW
  2872     25   W-NUM-TERM                PIC  9(4) COMP.               WCOMMONW
  2873     25   W-UTENTE                  PIC X(8).                     WCOMMONW
  2874     25   W-GRUPPO                  PIC X(8).                     WCOMMONW
  2875     25   W-ACCOUNT                 PIC X(8).                     WCOMMONW
  2876     25   W-IND-REC-PARAM           PIC S9(9) COMP.               WCOMMONW
  2877*conv                                                             WCOMMONW
  2878* directory di lavoro                                             WCOMMONW
  2879     25 AREE-VARIE-MENUTREE.                                      WCOMMONW
  2880       30 W-DIRECTORY               PIC X(60).                    WCOMMONW
  2881*conv-end                                                         WCOMMONW
  2882     25   W-PARAMETRI-FUNZIONE      PIC X(144).                   WCOMMONW
  2883     25   W-PAR-FUNZ-RID  REDEFINES W-PARAMETRI-FUNZIONE.         WCOMMONW
  2884      30   W-FLAGS.                                               WCOMMONW
  2885       35   W-FLAG-1                PIC X.                        WCOMMONW
  2886       35   W-FLAG-2                PIC X.                        WCOMMONW
  2887       35   W-FLAG-3                PIC X.                        WCOMMONW
  2888       35   W-FLAG-4                PIC X.                        WCOMMONW
  2889       35   W-FLAG-5                PIC X.                        WCOMMONW
  2890       35   W-FLAG-6                PIC X.                        WCOMMONW
  2891       35   W-FLAG-7                PIC X.                        WCOMMONW
  2892       35   W-FLAG-8                PIC X.                        WCOMMONW
  2893*                                                                 WCOMMONW
  2894      30   FILLER                   PIC X(136).                   WCOMMONW
  2895*                                                                 WCOMMONW
  2896*                                                                 WCOMMONW
  2897*                                                                 WCOMMONW
  2898*CONTROL LIST                                                     WCOMMONW
  2899*
  2900**** Start SQL Preprocessor ****
  2901*EXEC SQL INCLUDE SQLCA END-EXEC.
  2902**** Start Inserted Statements ****
  2903
  2904***********************************
  2905 01  SQLCA.
  2906     05  SQLCAID              PIC X(8).
  2907     05  SQLCABC              PIC S9(9) COMP SYNC.
  2908     05  SQLCODE              PIC S9(9) COMP SYNC.
  2909     05  SQLERRM.
  2910         49  SQLERRML         PIC S9(9) COMP SYNC.
  2911         49  SQLERRMC         PIC X(256).
  2912     05  SQLERRP              PIC X(8).
  2913     05  SQLERRD OCCURS 6     PIC S9(9) COMP SYNC.
  2914     05  SQLWARN.
  2915         10  SQLWARN0         PIC X(1).
  2916         10  SQLWARN1         PIC X(1).
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  52
* READVE3.cob
  2917         10  SQLWARN2         PIC X(1).
  2918         10  SQLWARN3         PIC X(1).
  2919         10  SQLWARN4         PIC X(1).
  2920         10  SQLWARN5         PIC X(1).
  2921         10  SQLWARN6         PIC X(1).
  2922         10  SQLWARN7         PIC X(1).
  2923     05  SQLEXT1              PIC X(4).
  2924     05  SQLEXT2              PIC X(4).
  2925***********************************
  2926**** End SQL Processor   ****
  2927*
  2928 01 SALTO PIC S9(4) COMP.
  2929*
  2930*PAGE
  2931*
  2932 PROCEDURE DIVISION USING W-COMMON SQLCA SALTO.
  2933 INIZIO.
  2934     MOVE 0 TO USCITA-PROGRAMMA CONT
  2935*T5000*
  2936               USCITA-DEVICE.
  2937*
  2938     MOVE W-FORMATO-INTERNO TO AA-MM-GG-DDT.
  2939*T5000*
  2940     PERFORM TRATTA-DEV THRU EX-TRATTA-DEV
  2941            UNTIL DEV-OK.
  2942*
  2943*ASOLOB2C*
  2944     PERFORM S-SET-1 THRU S-SET-1-EX
  2945     PERFORM CARICA-B2C-NO-DT THRU EX-CARICA-B2C-NO-DT.
  2946* NO-DATGE
  2947*     PERFORM S-SET-2 THRU S-SET-2-EX
  2948* NO-DATGE
  2949*ASOLOB2C*
  2950*
  2951          PERFORM TRATTA-IMPEGNATO THRU EX-TRATTA-IMPEGNATO.
  2952     PERFORM TRATTA-NEG THRU EX-TRATTA-NEG
  2953               UNTIL USCITA-PROGRAMMA = 1.
  2954 FINE.
  2955     EXIT PROGRAM.
  2956*
  2957*
  2958*
  2959*T5000*
  2960 TRATTA-DEV.
  2961     MOVE SPACES TO DEV-IN.
  2962     DISPLAY "Disp. USCITA >> " NO ADVANCING.
  2963     ACCEPT DEV-IN.
  2964     MOVE 2 TO QD-LL-A QD-LL-B.
  2965     MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC.
  2966     CANCEL "QDEEDIT"
  2967     CALL "QDEEDIT" USING PARDEED DEV-IN
  2968                          DESTINO-USCITA.
  2969     IF QD-STATO OF PARDEED NOT = 0 OR
  2970      (QD-STATO OF PARDEED = 0 AND NOT DESTINO-VALIDO)
  2971       DISPLAY "Dest. USCITA Err. " DEV-IN
  2972       PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  2973     ELSE
  2974       MOVE 1 TO USCITA-DEVICE.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  53
* READVE3.cob
  2975 EX-TRATTA-DEV.
  2976     EXIT.
  2977*
  2978*
  2979      TRATTA-IMPEGNATO.
  2980          MOVE SPACES TO DISIMPEGNA.
  2981          DISPLAY "Si vuole eliminare impegnato ?(SI/NO)"
  2982          ACCEPT DISIMPEGNA.
  2983          IF DISIMPEGNA = "si"
  2984            MOVE "SI" TO DISIMPEGNA
  2985          END-IF
  2986          IF DISIMPEGNA = "no"
  2987            MOVE "NO" TO DISIMPEGNA
  2988          END-IF
  2989          IF DISIMPEGNA NOT = "SI" AND DISIMPEGNA NOT = "NO"
  2990            DISPLAY "Input non valido."
  2991            PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  2992          END-IF.
  2993      EX-TRATTA-IMPEGNATO. EXIT.
  2994 TRATTA-NEG.
  2995     MOVE 100 TO IND-RIGA.
  2996     MOVE 0 TO OK-NEG IND-PAG.
  2997     MOVE 0 TO PREZZO-TOT.
  2998     PERFORM VERIF-NEG THRU EX-VERIF-NEG
  2999                UNTIL NEG-OK.
  3000     IF USCITA-PROGRAMMA = 1
  3001        GO TO EX-TRATTA-NEG.
  3002*MAG6/7*
  3003     MOVE SPACE TO MAG-INPUT.
  3004     PERFORM VERIF-MAG THRU EX-VERIF-MAG
  3005         UNTIL MAG-INPUT NUMERIC.
  3006*
  3007*Mag3_V/F*
  3008     MOVE SPACE TO F-V-INPUT.
  3009     PERFORM VERIF-F-V THRU EX-VERIF-F-V
  3010         UNTIL NOT F-V-INPUT = SPACE.
  3011*
  3012*UNICODDT*
  3013*     MOVE 0 TO OK-NEG
  3014*     PERFORM VERIF-AS-SOC-CL
  3015*         THRU EX-VERIF-AS-SOC-CL
  3016*     IF NOT NEG-OK
  3017*       GO TO EX-TRATTA-NEG.
  3018**MAXCA*
  3019*     MOVE SPACE TO MAX-CAPI-INPUT.
  3020*     PERFORM VERIF-MAX-CAPI
  3021*         THRU EX-VERIF-MAX-CAPI
  3022*        UNTIL MAX-CAPI-INPUT NUMERIC.
  3023*
  3024     MOVE 0 TO OK-NEG.
  3025     PERFORM VERIFICA-SOC THRU EX-VERIFICA-SOC
  3026     IF NOT NEG-OK
  3027        GO TO EX-TRATTA-NEG.
  3028     PERFORM CARICA-TAB-UNICO-DDT THRU EX-CARICA-TAB-UNICO-DDT.
  3029     IF QT-NUM-ELEM-EFF OF PAR-TAB-UNICO-DDT = 0
  3030         MOVE 0 TO OK-NEG.
  3031     IF NOT NEG-OK
  3032        GO TO EX-TRATTA-NEG.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  54
* READVE3.cob
  3033*UNICODDT*
  3034*
  3035*
  3036*PRODI*
  3037*     IF MAG-STOCK
  3038*        MOVE 0 TO STK-PRIMA-VOLTA
  3039*        PERFORM CARICA-TABELLA THRU EX-CARICA-TABELLA
  3040*     END-IF.
  3041     IF DISIMPEGNA = "SI"
  3042       PERFORM CICLO-DISIMPEGNO THRU EX-CICLO-DISIMPEGNO
  3043     END-IF
  3044     PERFORM INIZIA-TAB-ART THRU EX-INIZIA-TAB-ART.
  3045     PERFORM INIZIA-TAB-SING THRU EX-INIZIA-TAB-SING.
  3046     PERFORM TRATTA-OLD-NEW THRU EX-TRATTA-OLD-NEW.
  3047     PERFORM TRATTA-LETTI THRU EX-TRATTA-LETTI.
  3048 EX-TRATTA-NEG.
  3049     EXIT.
  3050*
  3051*UNICODDT*
  3052 VERIFICA-SOC.
  3053     MOVE SPACE TO SOCIETA-INPUT.
  3054     DISPLAY "Soc >> (vuoto = tutti) " NO ADVANCING.
  3055     ACCEPT SOCIETA-INPUT.
  3056     MOVE 1 TO QD-LL-A QD-LL-B.
  3057     MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC.
  3058     CANCEL "QDEEDIT"
  3059     CALL "QDEEDIT" USING PARDEED SOCIETA-INPUT
  3060                            SOCIETA-INPUT-R.
  3061     IF QD-STATO OF PARDEED NOT = 0
  3062       DISPLAY "Soc Err. " SOCIETA-INPUT
  3063       GO TO EX-VERIFICA-SOC.
  3064*
  3065     MOVE 1 TO OK-NEG.
  3066 EX-VERIFICA-SOC. EXIT.
  3067*UNICODDT*
  3068*
  3069*
  3070*UNICODDT*
  3071 CARICA-TAB-UNICO-DDT.
  3072      PERFORM INIT-PAR-TAB-UNICO-DDT THRU
  3073           EX-INIT-PAR-TAB-UNICO-DDT.
  3074      MOVE 0 TO SW-FINE-CARICA-TAB.
  3075      PERFORM  UNTIL FINE-CARICA-TAB
  3076          PERFORM VERIFICA-AS THRU EX-VERIFICA-AS
  3077          IF NOT FINE-AS
  3078              PERFORM VERIFICA-CL THRU  EX-VERIFICA-CL
  3079              PERFORM VERIFICA-MAX-CAPI THRU
  3080                   EX-VERIFICA-MAX-CAPI
  3081*              DISPLAY "INSERIMENTO-TAB-UNICO-DDT"
  3082              PERFORM VARYING I-AS FROM 1 BY 1
  3083                  UNTIL I-AS > 20
  3084                  IF AS-IN-R(I-AS) > 0
  3085                      MOVE AS-IN-R(I-AS)
  3086                        TO TAB-AS OF DEP-TAB-UNICO-DDT
  3087                      PERFORM VARYING I-CL FROM 1 BY 1
  3088                          UNTIL I-CL > 99
  3089                          IF CL-IN-R(I-CL) > 0
  3090                               MOVE CL-IN-R(I-CL)
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  55
* READVE3.cob
  3091                                 TO TAB-CL OF DEP-TAB-UNICO-DDT
  3092                               MOVE MAX-CAPI-INPUT-R
  3093                                 TO TAB-MAX-CAPI
  3094                               MOVE 0 TO TAB-CAPI-LETTI
  3095*                               DISPLAY "INS " DEP-TAB-UNICO-DDT
  3096                               PERFORM INS-TAB-UNICO-DDT THRU
  3097                                    EX-INS-TAB-UNICO-DDT
  3098                          END-IF
  3099                      END-PERFORM
  3100                  END-IF
  3101              END-PERFORM
  3102          END-IF
  3103      END-PERFORM.
  3104      display "---------------------------".
  3105      DISPLAY "AS CL MAX-CAPI INSERITI".
  3106      PERFORM MOSTRA-TAB-UNICO-DDT THRU
  3107           EX-MOSTRA-TAB-UNICO-DDT.
  3108      DISPLAY "CORRETTI? 'SI' per proseguire "
  3109              "'NO' per rifare elenco AS CL da capo"
  3110      MOVE "NO" TO SINO.
  3111      ACCEPT SINO.
  3112      IF SINO-NO
  3113          GO TO CARICA-TAB-UNICO-DDT.
  3114      IF XD = "S"
  3115          DISPLAY "sono dopo mostra-tab-unico"
  3116*          display "return per proseguire"
  3117*          accept pro
  3118          .
  3119 EX-CARICA-TAB-UNICO-DDT. EXIT.
  3120*
  3121*
  3122 VERIFICA-AS.
  3123*     DISPLAY "VERIFICA-AS".
  3124     DISPLAY "AS >> (tt=tutti) (elenco separato da ,) (CHIUDI)".
  3125*
  3126     MOVE SPACES TO ELENCO-AS.
  3127     ACCEPT ELENCO-AS
  3128     IF FINE-AS
  3129         MOVE 1 TO SW-FINE-CARICA-TAB
  3130         GO TO EX-VERIFICA-AS.
  3131*ASZERO*
  3132     IF TUTTI-AS-ELENCO
  3133         DISPLAY "Tratto tutti gli AS"
  3134         MOVE STRINGA-TUTTI-AS TO ELENCO-AS
  3135     ELSE
  3136         MOVE 0 TO SW-AS-ERR
  3137         PERFORM VARYING I-AS FROM 1 BY 1
  3138           UNTIL I-AS > 10
  3139           OR AS-ERR
  3140              MOVE 2 TO QD-LL-A QD-LL-B
  3141              MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC
  3142              CANCEL "QDEEDIT"
  3143              CALL "QDEEDIT" USING PARDEED AS-IN(I-AS)
  3144                                          AS-IN-R(I-AS)
  3145              IF (QD-STATO OF PARDEED NOT = 0)
  3146*             OR (AS-IN-R(I-AS) = 0)
  3147                 DISPLAY "AS Err. " AS-IN(I-AS)
  3148                 MOVE 1 TO SW-AS-ERR
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  56
* READVE3.cob
  3149                 GO TO VERIFICA-AS
  3150              END-IF
  3151          END-PERFORM.
  3152*     DISPLAY "EX-VERIFICA-AS " ELENCO-AS.
  3153 EX-VERIFICA-AS. EXIT.
  3154*
  3155*
  3156 VERIFICA-CL.
  3157*      DISPLAY "VERIFICA-CL".
  3158      MOVE SPACES TO ELENCO-CL.
  3159      DISPLAY "CL >> (elenco separato da ,)".
  3160      ACCEPT ELENCO-CL.
  3161     IF TUTTI-CL-ELENCO
  3162         DISPLAY "Tratto tutti le CL"
  3163         MOVE SPACES TO ELENCO-CL
  3164         PERFORM VARYING I-APP FROM 1 BY 1 UNTIL I-APP > 99
  3165              MOVE I-APP TO CL-IN-R(I-APP)
  3166         END-PERFORM
  3167     ELSE
  3168         MOVE 0 TO SW-CL-ERR
  3169         PERFORM VARYING I-CL FROM 1 BY 1
  3170           UNTIL I-CL > 10 OR CL-ERR
  3171            MOVE 2 TO QD-LL-A QD-LL-B
  3172            MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC
  3173            CANCEL "QDEEDIT"
  3174            CALL "QDEEDIT" USING PARDEED CL-IN(I-CL)
  3175                                       CL-IN-R(I-CL)
  3176            IF QD-STATO OF PARDEED NOT = 0
  3177*            OR (QD-STATO OF PARDEED = 0 AND CL-IN(I-CL)  = 0)
  3178                DISPLAY "Cl Err. " CL-IN(I-CL)
  3179                MOVE 1 TO SW-CL-ERR
  3180            END-IF
  3181          END-PERFORM
  3182          IF CL-ERR
  3183              GO TO VERIFICA-CL.
  3184*      DISPLAY "EX-VERIFICA-CL " ELENCO-CL.
  3185 EX-VERIFICA-CL. EXIT.
  3186**MAXCA*
  3187  VERIFICA-MAX-CAPI.
  3188*      DISPLAY "VERIFICA-MAX-CAPI".
  3189      DISPLAY "MaxCapi >> (vuoto = tutti) " NO ADVANCING.
  3190      ACCEPT MAX-CAPI-INPUT.
  3191      MOVE 6 TO QD-LL-A QD-LL-B.
  3192      MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC.
  3193      CANCEL "QDEEDIT"
  3194      CALL "QDEEDIT" USING PARDEED MAX-CAPI-INPUT
  3195                             MAX-CAPI-INPUT-R.
  3196      IF QD-STATO OF PARDEED NOT = 0
  3197        DISPLAY "MaxCapi Err. "
  3198        GO TO VERIFICA-MAX-CAPI.
  3199*       MOVE SPACE TO MAX-CAPI-INPUT.
  3200*      DISPLAY "EX-VERIFICA-MAX-CAPI " MAX-CAPI-INPUT-R.
  3201  EX-VERIFICA-MAX-CAPI.
  3202      EXIT.
  3203*UNICODDT*
  3204*UNICODDT*   TOLTO PER CARICARE TAB-UNICO-DDT
  3205* VERIF-AS-SOC-CL.
  3206*     MOVE SPACE TO AS-INPUT.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  57
* READVE3.cob
  3207**ASZERO*
  3208**     DISPLAY "AS >> (vuoto = tutti) " NO ADVANCING.
  3209*     DISPLAY "AS >> (tt = tutti) " NO ADVANCING.
  3210**
  3211*     ACCEPT AS-INPUT.
  3212**ASZERO*
  3213*     IF TUTTI-AS
  3214*        DISPLAY "Tratto tutti gli AS"
  3215*     ELSE
  3216**
  3217*        MOVE 2 TO QD-LL-A QD-LL-B
  3218*        MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC
  3219*        CANCEL "QDEEDIT"
  3220*        CALL "QDEEDIT" USING PARDEED AS-INPUT
  3221*                               AS-INPUT-R
  3222*        IF (QD-STATO OF PARDEED NOT = 0)
  3223**ASZERO*
  3224*           OR (AS-INPUT-R = 0)
  3225**
  3226**     OR
  3227**      (QD-STATO OF PARDEED = 0 AND S-INPUT-R NOT = 2 AND
  3228**             S-INPUT-R NOT = 4)
  3229*          DISPLAY "AS Err. " AS-INPUT
  3230*          GO TO EX-VERIF-AS-SOC-CL
  3231*     END-IF.
  3232**
  3233*     MOVE SPACE TO SOCIETA-INPUT.
  3234*     DISPLAY "Soc >> (vuoto = tutti) " NO ADVANCING.
  3235*     ACCEPT SOCIETA-INPUT.
  3236*     MOVE 1 TO QD-LL-A QD-LL-B.
  3237*     MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC.
  3238*     CANCEL "QDEEDIT"
  3239*     CALL "QDEEDIT" USING PARDEED SOCIETA-INPUT
  3240*                            SOCIETA-INPUT-R.
  3241*     IF QD-STATO OF PARDEED NOT = 0
  3242*       DISPLAY "Soc Err. " SOCIETA-INPUT
  3243*       GO TO EX-VERIF-AS-SOC-CL.
  3244**
  3245*     MOVE SPACE TO CLASSE-INPUT.
  3246*     DISPLAY "Cl >> " NO ADVANCING.
  3247*     ACCEPT CLASSE-INPUT.
  3248*     MOVE 2 TO QD-LL-A QD-LL-B.
  3249*     MOVE 0 TO QD-STATO OF PARDEED QD-NR-DEC.
  3250*     CANCEL "QDEEDIT"
  3251*     CALL "QDEEDIT" USING PARDEED CLASSE-INPUT
  3252*                            CLASSE-INPUT-R.
  3253*     IF QD-STATO OF PARDEED NOT = 0  OR
  3254*      (QD-STATO OF PARDEED = 0 AND CLASSE-INPUT-R  = 0)
  3255*       DISPLAY "Cl Err. " CLASSE-INPUT
  3256*       GO TO EX-VERIF-AS-SOC-CL.
  3257**
  3258*     MOVE 1 TO OK-NEG.
  3259* EX-VERIF-AS-SOC-CL.
  3260*     EXIT.
  3261*UNICODDT*
  3262   CALL-DISIMPEGNA-MAG.
  3263       CALL "PYTHON" USING "disimpegna_capi"
  3264                          "elimina_impegnati"
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  58
* READVE3.cob
  3265                           PY-INPUT-REC-DISIMPEGNA
  3266                           PY-OUTPUT-DISIMPEGNO.
  3267   EX-CALL-DISIMPEGNA-MAG. EXIT.
  3268*
  3269*
  3270 INIZIA-TAB-ART.
  3271     MOVE 0 TO QT-STATO OF PARTAB-ART
  3272               QT-NUM-ELEM-EFF OF PARTAB-ART
  3273               QT-INDEX-ELEM OF PARTAB-ART.
  3274     MOVE NUM-ELEM-MAX-ART TO QT-NUM-ELEM-MAX OF PARTAB-ART.
  3275     MOVE "K1" TO QT-FUNZIONE OF PARTAB-ART.
  3276*VACO*
  3277**BUDA*
  3278*     MOVE 75 TO QT-LL-ELEM OF PARTAB-ART.
  3279*     MOVE 79 TO QT-LL-ELEM OF PARTAB-ART.
  3280      COMPUTE QT-LL-ELEM OF PARTAB-ART =
  3281               FUNCTION LENGTH(ART-TAB-LETTI (1)).
  3282*VACO*
  3283     MOVE 1 TO QT-ADDR-KEY OF PARTAB-ART.
  3284     MOVE 8 TO QT-LL-KEY OF PARTAB-ART.
  3285 EX-INIZIA-TAB-ART.
  3286     EXIT.
  3287*
  3288*
  3289 INIZIA-TAB-SING.
  3290     MOVE 0 TO QT-STATO OF PARTAB-SING
  3291               QT-NUM-ELEM-EFF OF PARTAB-SING
  3292               QT-INDEX-ELEM OF PARTAB-SING.
  3293     MOVE 9999 TO QT-NUM-ELEM-MAX OF PARTAB-SING.
  3294     MOVE "K1" TO QT-FUNZIONE OF PARTAB-SING.
  3295*MOVSKU
  3296*     MOVE 23 TO QT-LL-ELEM OF PARTAB-SING.
  3297     MOVE 36 TO QT-LL-ELEM OF PARTAB-SING.
  3298     MOVE 1 TO QT-ADDR-KEY OF PARTAB-SING.
  3299     MOVE 10 TO QT-LL-KEY OF PARTAB-SING.
  3300 EX-INIZIA-TAB-SING.
  3301     EXIT.
  3302*
  3303 LEGGI-PARAMDT.
  3304     MOVE "DPARAM;" TO W-NOME-DATA-SET.
  3305     MOVE "C-AZIENDA;" TO W-NOME-CAMPO.
  3306     MOVE 0 TO W-VALORE-CAMPO-HW.
  3307     PERFORM TTDBFIND THRU EX-TTDBFIND.
  3308     IF NOT W-OK-IMAGE
  3309     CANCEL "QDBERROR"
  3310        CALL "QDBERROR" USING W-COMMON.
  3311     MOVE 5 TO W-MODO.
  3312     PERFORM TTDBGET THRU EX-TTDBGET.
  3313     MOVE AREA-REC-SET TO REC-PARAM-RID.
  3314     PERFORM DBGET-PARAMDT THRU EX-DBGET-PARAMDT
  3315      UNTIL
  3316           W-FINE-CATENA  OR
  3317           P-EXTRAMAG OF REC-PARAMETRI.
  3318     IF W-FINE-CATENA
  3319     CANCEL "QDBERROR"
  3320        CALL "QDBERROR" USING W-COMMON
  3321       ELSE
  3322          MOVE W-WORD-ATT TO INDIRIZZO-DPARAM
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  59
* READVE3.cob
  3323          MOVE NUM-BOLLA-TAGLIO-FODERE TO NUMERO-DDT.
  3324 EX-LEGGI-PARAMDT.
  3325     EXIT.
  3326*
  3327*
  3328 DBGET-PARAMDT.
  3329     PERFORM TTDBGET THRU EX-TTDBGET.
  3330     MOVE AREA-REC-SET TO REC-PARAM-RID.
  3331 EX-DBGET-PARAMDT.
  3332     EXIT.
  3333*
  3334*volante*
  3335*
  3336 LEGGI-PARAMDT-FITTIZI.
  3337     MOVE "DPARAM;" TO W-NOME-DATA-SET.
  3338     MOVE "C-AZIENDA;" TO W-NOME-CAMPO.
  3339     MOVE 0 TO W-VALORE-CAMPO-HW.
  3340     PERFORM TTDBFIND THRU EX-TTDBFIND.
  3341     IF NOT W-OK-IMAGE
  3342     CANCEL "QDBERROR"
  3343        CALL "QDBERROR" USING W-COMMON.
  3344     MOVE 5 TO W-MODO.
  3345     PERFORM TTDBGET THRU EX-TTDBGET.
  3346     MOVE AREA-REC-SET TO REC-PARAM-FITTIZ-R.
  3347     PERFORM DBGET-PARAMDT-FTZ THRU EX-DBGET-PARAMDT-FTZ
  3348      UNTIL
  3349           W-FINE-CATENA  OR
  3350           P-RIPRADI OF REC-PARAM-FITTIZI.
  3351     IF W-FINE-CATENA
  3352     CANCEL "QDBERROR"
  3353        CALL "QDBERROR" USING W-COMMON
  3354       ELSE
  3355          MOVE W-WORD-ATT TO INDIRIZZO-DPARAM
  3356          MOVE PAR-FITTIZIO-1 TO NUMERO-DDT.
  3357 EX-LEGGI-PARAMDT-FITTIZI.
  3358     EXIT.
  3359*
  3360*
  3361 DBGET-PARAMDT-FTZ.
  3362     PERFORM TTDBGET THRU EX-TTDBGET.
  3363     MOVE AREA-REC-SET TO REC-PARAM-FITTIZ-R.
  3364 EX-DBGET-PARAMDT-FTZ.
  3365     EXIT.
  3366*
  3367*
  3368*
  3369*PAGE
  3370*
  3371*
  3372 VERIF-NEG.
  3373     MOVE SPACES TO D-CONTO-MEM
  3374                    INDIRIZZO-COM INDIRIZZO-C-COM
  3375                    LOCALITA-COM LOCALITA-C-COM
  3376                    CONTO-IN
  3377                    PROV-COM PROV-C-COM.
  3378     MOVE 0 TO CAP-COM CAP-C-COM.
  3379*    DISPLAY " ".
  3380*                  NO ADVANCING.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  60
* READVE3.cob
  3381*    DISPLAY "m".
  3382*    DISPLAY "hJ".
  3383*       PERFORM VARYING IR FROM 1 BY 1
  3384*            UNTIL IR > SALTO
  3385        DISPLAY SPACE
  3386*       END-PERFORM
  3387     DISPLAY "CONTO cliente (8 cifre)".
  3388     DISPLAY "  (END/end=fine)"
  3389     ACCEPT CONTO-IN.
  3390     IF CONTO-FINE = "END" or = "end"
  3391        MOVE 1 TO OK-NEG
  3392                  USCITA-PROGRAMMA
  3393       ELSE
  3394          IF CONTO-IN NOT NUMERIC
  3395             DISPLAY "CODICE non numerico"
  3396             PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3397            ELSE
  3398               MOVE "ANACON;" TO W-NOME-DATA-SET
  3399               MOVE "CONTO;" TO W-NOME-CAMPO
  3400               MOVE CONTO-IN-R TO W-VALORE-CAMPO-W
  3401               MOVE 7 TO W-MODO
  3402               PERFORM TTDBGET THRU EX-TTDBGET
  3403               IF NOT W-OK-IMAGE
  3404                  DISPLAY "Manca CLIENTE " CONTO-IN-R
  3405                  PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3406                 ELSE
  3407                    MOVE D-CONTO OF REC-ANACON TO D-CONTO-MEM
  3408*BUDA*
  3409                    MOVE FLAG-ANA-8 TO FLAG-ANACON
  3410                    IF FILIALE-DT-ESTERO
  3411                      MOVE 1 TO FLAG-DT-ESTERO
  3412                    ELSE
  3413                      MOVE 0 TO FLAG-DT-ESTERO
  3414                    END-IF
  3415                    PERFORM LEGGI-IND THRU EX-LEGGI-IND
  3416                    PERFORM MUOVI-IND THRU EX-MUOVI-IND
  3417                    PERFORM MUOVI-CAP THRU EX-MUOVI-CAP
  3418*FIFRA*
  3419                    PERFORM SCEGLI-CONTO-FATTURA
  3420                         THRU EX-SCEGLI-CONTO-FATTURA
  3421*BUDA*
  3422                    PERFORM CERCA-LISTINO
  3423                        THRU EX-CERCA-LISTINO
  3424                    MOVE 1 TO OK-NEG.
  3425 EX-VERIF-NEG.
  3426     EXIT.
  3427*
  3428*
  3429*PAGE
  3430*
  3431*
  3432 TRATTA-OLD-NEW.
  3433*    DISPLAY "m".
  3434*    DISPLAY "hJ".
  3435*    PERFORM VARYING IR FROM 1 BY 1
  3436*            UNTIL IR > SALTO
  3437        DISPLAY SPACE
  3438*    END-PERFORM.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  61
* READVE3.cob
  3439*    DISPLAY "hJ".
  3440*    DISPLAY "&dB       Destinatario      &d@"
  3441*               "&dB Vendita STOCK >>  &d@ "  D-CONTO-MEM.
  3442     DISPLAY D-CONTO-MEM.
  3443*MAG6/7*
  3444*    DISPLAY " ".
  3445     DISPLAY "dal mag " MAG-INPUT.
  3446     MOVE 0 TO IND-CL TOT-BOLLA-C IND-CAPI-LETTI
  3447                       IND-CAPI-NO-GIAC.
  3448*
  3449     MOVE LOW-VALUE TO TABELLA-ARTICOLI-LETTI.
  3450     INITIALIZE TABELLA-NO-GIAC.
  3451     MOVE SPACES TO COD-IN MEM-COD-IN.
  3452*     DISPLAY " . fine lettura".
  3453*     DISPLAY " % annulla ultimo letto".
  3454*     DISPLAY " S stampa rapportino".
  3455**    DISPLAY "l".
  3456*     PERFORM TRATTA-LEGGI THRU EX-TRATTA-LEGGI
  3457*               UNTIL LETT-FINE OR
  3458*               QT-NUM-ELEM-EFF OF PARTAB-ART = NUM-ELEM-MAX-ART
  3459     PERFORM TRATTA-SITPF-3 THRU EX-TRATTA-SITPF-3.
  3460*
  3461**    DISPLAY "m".
  3462     DISPLAY " S stampa rapportino".
  3463     MOVE SPACES TO COD-IN.
  3464     ACCEPT COD-IN.
  3465     IF LETT-STAMPA
  3466       PERFORM STAMPA-RAPPORTINO THRU EX-STAMPA-RAPPORTINO
  3467       DISPLAY "   rapportino stampato".
  3468 EX-TRATTA-OLD-NEW.
  3469     EXIT.
  3470*
  3471*
  3472*PAGE
  3473*
  3474*
  3475 LEGGI-IND.
  3476     MOVE "CONTO;"  TO W-NOME-CAMPO.
  3477     MOVE "INDIRIZ;" TO W-NOME-DATA-SET.
  3478     PERFORM TTDBFIND THRU EX-TTDBFIND.
  3479     MOVE 5 TO W-MODO.
  3480     PERFORM TTDBGET THRU EX-TTDBGET.
  3481     MOVE AREA-REC-SET TO REC-INDIRIZZI.
  3482     IF NOT W-OK-IMAGE
  3483        DISPLAY "ERR INDIRIZZI" CONTO-IN-R
  3484        PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3485        PERFORM AZZERA-CAMPI-INDIRIZ THRU EX-AZZERA-CAMPI-INDIRIZ.
  3486 EX-LEGGI-IND.
  3487     EXIT.
  3488*
  3489*
  3490 AZZERA-CAMPI-INDIRIZ.
  3491     MOVE SPACE TO D-AGG STATO OF REC-INDIRIZZI
  3492                   SIGLA-PROV OF REC-INDIRIZZI (1)
  3493                   SIGLA-PROV OF REC-INDIRIZZI (2)
  3494                   INDIRIZZO OF REC-INDIRIZZI (1)
  3495                   INDIRIZZO OF REC-INDIRIZZI (2)
  3496                   STATO OF REC-INDIRIZZI.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  62
* READVE3.cob
  3497     MOVE 0 TO CAP OF REC-INDIRIZZI (1)
  3498               CAP OF REC-INDIRIZZI (2)
  3499*BUDA*
  3500               PRIORITA OF REC-INDIRIZZI
  3501               TELEFONO OF REC-INDIRIZZI.
  3502*FIFRA*
  3503     MOVE 0 TO TELEX OF REC-INDIRIZZI.
  3504*
  3505 EX-AZZERA-CAMPI-INDIRIZ.
  3506     EXIT.
  3507*
  3508*
  3509 MUOVI-IND.
  3510     MOVE INDIRIZZO OF REC-INDIRIZZI (1) TO INDIRIZZO-STD.
  3511     MOVE 66 TO LL-STRINGA-IND.
  3512     MOVE 60 TO LL-SUBSTRINGA-IND.
  3513     MOVE 1 TO FUNZIONE-IND.
  3514     CANCEL "QSTRINGV"
  3515     CALL "QSTRINGV" USING PAR-INDIRIZZO
  3516                           INDIRIZZO-STD
  3517                           INDIRIZZO-COM.
  3518     MOVE 2 TO FUNZIONE-IND.
  3519     CANCEL "QSTRINGV"
  3520     CALL "QSTRINGV" USING PAR-INDIRIZZO
  3521                           INDIRIZZO-STD
  3522                           LOCALITA-COM.
  3523     IF INDIRIZZO OF REC-INDIRIZZI (2) NOT = SPACE
  3524        MOVE INDIRIZZO OF REC-INDIRIZZI (2) TO INDIRIZZO-STD
  3525        MOVE 1 TO FUNZIONE-IND
  3526     CANCEL "QSTRINGV"
  3527        CALL "QSTRINGV" USING PAR-INDIRIZZO
  3528                              INDIRIZZO-STD
  3529                              INDIRIZZO-C-COM
  3530        MOVE 2 TO FUNZIONE-IND
  3531     CANCEL "QSTRINGV"
  3532        CALL "QSTRINGV" USING PAR-INDIRIZZO
  3533                              INDIRIZZO-STD
  3534                              LOCALITA-C-COM.
  3535 EX-MUOVI-IND.
  3536     EXIT.
  3537*
  3538*
  3539 MUOVI-CAP.
  3540     MOVE D-AGG OF REC-INDIRIZZI TO D-CONTO-AGG-MEM.
  3541     MOVE STATO OF REC-INDIRIZZI TO STATO-COM.
  3542     MOVE CAP OF REC-INDIRIZZI (1) TO CAP-COM.
  3543     MOVE SIGLA-PROV OF REC-INDIRIZZI (1) TO PROV-COM.
  3544     IF INDIRIZZO OF REC-INDIRIZZI (2) NOT = SPACE
  3545        MOVE SIGLA-PROV OF REC-INDIRIZZI (2) TO PROV-C-COM
  3546        MOVE CAP OF REC-INDIRIZZI (2) TO CAP-C-COM.
  3547 EX-MUOVI-CAP.
  3548     EXIT.
  3549*
  3550*
  3551*BUDA*
  3552*
  3553 CERCA-LISTINO.
  3554     MOVE "CONTO;"  TO W-NOME-CAMPO.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  63
* READVE3.cob
  3555     MOVE "CONFATT;" TO W-NOME-DATA-SET.
  3556     MOVE CONTO-IN-R TO W-VALORE-CAMPO-W.
  3557     PERFORM TTDBFIND THRU EX-TTDBFIND.
  3558     MOVE 5 TO W-MODO.
  3559     PERFORM TTDBGET THRU EX-TTDBGET.
  3560     MOVE AREA-REC-SET TO REC-CONFATT.
  3561     MOVE LISTINO OF REC-CONFATT TO LISTINO-MEM.
  3562     MOVE DIVISA OF REC-CONFATT TO DIVISA-MEM.
  3563 EX-CERCA-LISTINO.
  3564     EXIT.
  3565*
  3566*
  3567*FIFRA*
  3568 SCEGLI-CONTO-FATTURA.
  3569     IF TELEFONO OF REC-INDIRIZZI = 0
  3570       MOVE 0 TO CONTO-FATTURA-MEM
  3571     ELSE
  3572       MOVE TELEX OF REC-INDIRIZZI TO CONTO-FATTURA-MEM.
  3573 EX-SCEGLI-CONTO-FATTURA.
  3574     EXIT.
  3575*
  3576*
  3577*
  3578*PAGE
  3579*
  3580*
  3581 TRATTA-SITPF-3.
  3582     MOVE "SITPF" TO W-NOME-DATA-SET.
  3583     MOVE "MAG" TO W-NOME-CAMPO
  3584     MOVE MAG-INPUT-R TO W-VALORE-CAMPO-HW
  3585     PERFORM TTDBFIND THRU EX-TTDBFIND.
  3586     IF W-OK-IMAGE
  3587       MOVE 5 TO W-MODO
  3588       PERFORM TTDBGET-S THRU EX-TTDBGET-S.
  3589     PERFORM SELEZIONA-SITPF-3
  3590         THRU EX-SELEZIONA-SITPF-3
  3591            UNTIL NOT W-OK-IMAGE OR
  3592*MAXCA*
  3593*UNICODDT*
  3594*              (MAX-CAPI-INPUT-R <> 0 AND
  3595*                   IND-CAPI-LETTI >= MAX-CAPI-INPUT-R) OR
  3596*            FLAG QUANDO TUTTI I TAB-MAX-CAPI SONO STATI RAGGIUNTI
  3597             SW-MAX-CAPI-RAGGIUNTO = 1 OR
  3598*UNICODDT*
  3599*
  3600              QT-NUM-ELEM-EFF OF PARTAB-ART = NUM-ELEM-MAX-ART
  3601              OR  QT-NUM-ELEM-EFF OF PARTAB-SING =
  3602                   QT-NUM-ELEM-MAX OF PARTAB-SING.
  3603     IF QT-NUM-ELEM-EFF OF PARTAB-ART = NUM-ELEM-MAX-ART
  3604       DISPLAY SPACE
  3605       DISPLAY "TABELLA-ARTICOLI-LETTI "
  3606       DISPLAY " da allargare"
  3607     END-IF.
  3608     IF QT-NUM-ELEM-EFF OF PARTAB-SING =
  3609                   QT-NUM-ELEM-MAX OF PARTAB-SING
  3610       DISPLAY SPACE
  3611       DISPLAY "TABELLA-SINGOLI "
  3612       DISPLAY " da allargare"
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  64
* READVE3.cob
  3613     END-IF
  3614     MOVE  "." TO COD-IN-RID.
  3615 EX-TRATTA-SITPF-3.
  3616     EXIT.
  3617*
  3618 CALCOLA-AS-CL.
  3619       MOVE 0 TO SW-ERR-AS-CL.
  3620       MOVE C-MAT-TRANS-RID TO W-VALORE-CAMPO
  3621       MOVE "ANAMAT;" TO W-NOME-DATA-SET
  3622       MOVE "C-MAT;" TO W-NOME-CAMPO
  3623       MOVE 7 TO W-MODO
  3624       PERFORM TTDBGET THRU EX-TTDBGET
  3625       IF NOT W-OK-IMAGE
  3626          DISPLAY "Inesist. " C-MAT-A-BARRE-RID
  3627          PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3628          MOVE 1 TO SW-ERR-AS-CL
  3629      ELSE
  3630          MOVE ANNO OF REC-ANAMAT TO APP-A
  3631          MOVE STAGIONE OF REC-ANAMAT TO APP-S
  3632          MOVE APP-AS TO TAB-AS
  3633          MOVE CL-GR OF REC-ANAMAT TO TAB-CL
  3634          PERFORM LEGGI-TAB-UNICO-DDT THRU
  3635               EX-LEGGI-TAB-UNICO-DDT
  3636          IF QT-STATO OF PAR-TAB-UNICO-DDT NOT = 0
  3637              MOVE 1 TO SW-ERR-AS-CL.
  3638     MOVE "SITPF" TO W-NOME-DATA-SET
  3639     MOVE 5 TO W-MODO.
  3640 EX-CALCOLA-AS-CL. EXIT.
  3641 SELEZIONA-SITPF-3.
  3642*      DISPLAY "sono seleziona-sitpf-3"
  3643*      display "return per proseguire".
  3644*      accept pro.
  3645     IF MAGAZZINO OF REC-SITPF NOT = MAG-INPUT-R
  3646       PERFORM TTDBGET-S THRU EX-TTDBGET-S
  3647       GO TO EX-SELEZIONA-SITPF-3.
  3648*
  3649     MOVE C-MAT OF REC-SITPF
  3650         TO C-MAT-TRANS-RID .
  3651*UNICODDT*
  3652     PERFORM CALCOLA-AS-CL THRU EX-CALCOLA-AS-CL
  3653     IF ERR-AS-CL
  3654          PERFORM TTDBGET-S THRU EX-TTDBGET-S
  3655          GO TO EX-SELEZIONA-SITPF-3.
  3656     IF XD = "S"
  3657          DISPLAY "sono dopo calcola-as-cl " C-MAT-TRANS-RID
  3658          display dep-tab-unico-ddt
  3659*          display "return per proseguire"
  3660*          accept pro
  3661          .
  3662     IF QT-STATO OF PAR-TAB-UNICO-DDT = 0
  3663     AND TAB-MAX-CAPI <> 0
  3664     AND TAB-CAPI-LETTI >= TAB-MAX-CAPI
  3665*         VALERIA - HO SUPERATO IL LIMITE DI AS CL
  3666*         VERIFICO SE TUTTI I LIMITI SONO STATI SUPERATI
  3667          MOVE 1 TO SW-MAX-CAPI-RAGGIUNTO
  3668          PERFORM VARYING I-APP FROM 1 BY 1
  3669              UNTIL I-APP > QT-NUM-ELEM-EFF OF PAR-TAB-UNICO-DDT
  3670                 OR SW-MAX-CAPI-RAGGIUNTO = 0
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  65
* READVE3.cob
  3671                    MOVE ELE-TAB-UNICO-DDT (I-APP)
  3672                      TO DEP-TAB-UNICO-DDT
  3673*                    display dep-tab-unico-ddt
  3674                    IF TAB-MAX-CAPI > TAB-CAPI-LETTI
  3675                    OR TAB-MAX-CAPI = 0
  3676                        MOVE 0 TO SW-MAX-CAPI-RAGGIUNTO
  3677                    END-IF
  3678          END-PERFORM
  3679          IF XD = "S"
  3680              DISPLAY "sono dopo calcolo max capi raggiunto = "
  3681               SW-MAX-CAPI-RAGGIUNTO
  3682*              display "return per proseguire"
  3683*              accept pro
  3684          END-IF
  3685          PERFORM TTDBGET-S THRU EX-TTDBGET-S
  3686          GO TO EX-SELEZIONA-SITPF-3.
  3687     MOVE APP-AS TO TAB-AS.
  3688     MOVE CL-GR OF REC-ANAMAT TO TAB-CL.
  3689     PERFORM LEGGI-TAB-UNICO-DDT THRU
  3690          EX-LEGGI-TAB-UNICO-DDT.
  3691*UNICODDT*
  3692*UNICODDT*
  3693*     IF CLASSE OF C-MAT-TRANSITO NOT = CLASSE-INPUT-R
  3694*       PERFORM TTDBGET-S THRU EX-TTDBGET-S
  3695*       GO TO EX-SELEZIONA-SITPF-3.
  3696*UNICODDT*
  3697*
  3698     IF SOCIETA-INPUT-R NOT = 0 AND
  3699        SOCIETA-MOD OF C-MAT-TRANSITO
  3700               NOT = SOCIETA-INPUT-R
  3701       PERFORM TTDBGET-S THRU EX-TTDBGET-S
  3702       GO TO EX-SELEZIONA-SITPF-3.
  3703*
  3704     PERFORM VARYING IT FROM 1 BY 1
  3705             UNTIL IT > NTG-NTG
  3706             OR  QT-NUM-ELEM-EFF OF PARTAB-SING =
  3707                   QT-NUM-ELEM-MAX OF PARTAB-SING
  3708*MAXCA*
  3709*UNICODDT*
  3710*            OR (MAX-CAPI-INPUT-R <> 0 AND
  3711*                  IND-CAPI-LETTI >= MAX-CAPI-INPUT-R)
  3712             OR (TAB-MAX-CAPI <> 0 AND
  3713                 TAB-CAPI-LETTI >= TAB-MAX-CAPI)
  3714*UNICODDT*
  3715*
  3716        IF DISIMPEGNA = "SI"
  3717           MOVE QTA-GIAC-PF OF REC-SITPF(IT)
  3718             TO DA-TRASFERIRE
  3719        ELSE
  3720           COMPUTE DA-TRASFERIRE =
  3721                   QTA-GIAC-PF OF REC-SITPF(IT)
  3722                   + QTA-IMP OF REC-SITPF(IT)
  3723        END-IF
  3724        PERFORM VARYING IC FROM 1 BY 1
  3725                 UNTIL IC > DA-TRASFERIRE
  3726                 OR  QT-NUM-ELEM-EFF OF PARTAB-SING =
  3727                   QT-NUM-ELEM-MAX OF PARTAB-SING
  3728*MAXCA*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  66
* READVE3.cob
  3729*UNICODDT*
  3730*             OR (MAX-CAPI-INPUT-R <> 0 AND
  3731*                   IND-CAPI-LETTI >= MAX-CAPI-INPUT-R)
  3732             OR (TAB-MAX-CAPI <> 0 AND
  3733                 TAB-CAPI-LETTI >= TAB-MAX-CAPI)
  3734*UNICODDT*
  3735*
  3736          MOVE C-MAT OF REC-SITPF
  3737             TO C-MAT-TRANS-RID
  3738          MOVE MODELLO OF C-MAT-TRANSITO
  3739              TO MODELLO OF C-MAT-A-BARRE
  3740          MOVE VEST-A      OF C-MAT-TRANSITO
  3741             TO VESTIBILITA OF C-MAT-A-BARRE
  3742          MOVE PEZZO-A OF C-MAT-TRANSITO
  3743             TO PEZZO OF C-MAT-A-BARRE
  3744          MOVE PROGR-ART OF C-MAT-TRANSITO
  3745             TO SOCIETA OF C-MAT-A-BARRE
  3746          MOVE COLORE  OF C-MAT-TRANSITO
  3747             TO VARIANTE-COL OF C-MAT-A-BARRE
  3748          MOVE FUNCTION tgxid(IT)
  3749                   TO TAGLIA OF C-MAT-A-BARRE
  3750          MOVE IT TO NTG-MEM
  3751*          MOVE IT TO TAGLIA OF C-MAT-A-BARRE
  3752          PERFORM TRATTA-LEGGI THRU EX-TRATTA-LEGGI
  3753        END-PERFORM
  3754     END-PERFORM.
  3755*
  3756     MOVE "SITPF" TO W-NOME-DATA-SET
  3757     MOVE 5 TO W-MODO
  3758     PERFORM TTDBGET-S THRU EX-TTDBGET-S.
  3759 EX-SELEZIONA-SITPF-3.
  3760     EXIT.
  3761*
  3762*
  3763 TRATTA-LEGGI.
  3764      IF XD = "S"
  3765          DISPLAY "sono in tratta-leggi "
  3766          display dep-tab-unico-ddt
  3767*          display "return per proseguire"
  3768*          accept pro
  3769          .
  3770*     MOVE SPACES TO COD-IN.
  3771*     ACCEPT COD-IN.
  3772*     IF LETT-FINE
  3773*        GO TO EX-TRATTA-LEGGI.
  3774*     IF LETT-ANN-ULT
  3775*       PERFORM ANNULLA-PRECEDENTE THRU EX-ANNULLA-PRECEDENTE
  3776*       MOVE SPACES TO MEM-COD-IN
  3777*       GO TO EX-TRATTA-LEGGI.
  3778*     IF LETT-STAMPA
  3779*       PERFORM STAMPA-RAPPORTINO THRU EX-STAMPA-RAPPORTINO
  3780*       DISPLAY "   rapportino stampato"
  3781*       GO TO EX-TRATTA-LEGGI.
  3782**
  3783*     IF C-MAT-A-BARRE-RID NOT NUMERIC
  3784*        DISPLAY "COD non num >> RILEGGERE"
  3785*        PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3786*        GO TO EX-TRATTA-LEGGI.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  67
* READVE3.cob
  3787*     IF VARIANTE-COL OF C-MAT-A-BARRE = 0
  3788*        DISPLAY "VARIANTE 0 >> RILEGGERE"
  3789*        PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3790*        GO TO EX-TRATTA-LEGGI.
  3791     MOVE SPACES TO MEM-COD-IN.
  3792     MOVE SOCIETA OF C-MAT-A-BARRE TO SOC-COM.
  3793     MOVE 0 TO PRE-SOC.
  3794     MOVE SOC-COM TO SOCIETA OF C-MAT-A-BARRE.
  3795     COMPUTE ELEM-ART = C-MAT-A-BARRE-RID / 10
  3796     MOVE "K2" TO QT-FUNZIONE OF PARTAB-ART.
  3797     CANCEL "QTABEL"
  3798     CALL "QTABEL" USING PARTAB-ART TABELLA-ARTICOLI-LETTI
  3799                         ELEM-ART.
  3800*     IF TAGLIA OF C-MAT-A-BARRE = 0
  3801*           OR = 9
  3802*      DISPLAY "TAGLIE da  1  a  8"
  3803*        PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3804*        GO TO EX-TRATTA-LEGGI.
  3805*    IF QT-STATO OF PARTAB-ART NOT = 0
  3806       MOVE 0 TO C-MAT-TRANS-RID
  3807       MOVE MODELLO OF C-MAT-A-BARRE TO MODELLO OF
  3808                         C-MAT-TRANSITO
  3809       MOVE VESTIBILITA OF C-MAT-A-BARRE TO
  3810              VEST-A OF C-MAT-TRANSITO
  3811       MOVE SOCIETA OF C-MAT-A-BARRE TO
  3812                      PROGR-ART OF C-MAT-TRANSITO
  3813       MOVE PEZZO OF C-MAT-A-BARRE TO PEZZO-A OF
  3814                         C-MAT-TRANSITO
  3815       MOVE VARIANTE-COL OF C-MAT-A-BARRE TO COLORE OF
  3816                         C-MAT-TRANSITO.
  3817     IF QT-STATO OF PARTAB-ART NOT = 0
  3818       MOVE C-MAT-TRANS-RID TO W-VALORE-CAMPO
  3819       MOVE "ANAMAT;" TO W-NOME-DATA-SET
  3820       MOVE "C-MAT;" TO W-NOME-CAMPO
  3821       MOVE 7 TO W-MODO
  3822       PERFORM TTDBGET THRU EX-TTDBGET
  3823       IF NOT W-OK-IMAGE
  3824          DISPLAY "Inesist. " C-MAT-A-BARRE-RID
  3825          PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3826          GO TO EX-TRATTA-LEGGI.
  3827*ASZERO*
  3828*     IF (A-INPUT-R NOT = 0 AND
  3829*         A-INPUT-R NOT = ANNO OF REC-ANAMAT) OR
  3830*        (S-INPUT-R NOT = 0 AND
  3831*         S-INPUT-R NOT = STAGIONE OF REC-ANAMAT)
  3832*UNICODDT*
  3833*  QUESTO TEST NON SERVE PIU' PERCHE' GIA' FILTRATO
  3834*     IF NOT TUTTI-AS AND (
  3835*        (A-INPUT-R NOT = ANNO OF REC-ANAMAT) OR
  3836*        (S-INPUT-R NOT = STAGIONE OF REC-ANAMAT) )
  3837*       GO TO EX-TRATTA-LEGGI.
  3838*UNICODDT*
  3839*BARBARELLA 260516
  3840*NO FORN = 4 PER AI 05*
  3841*     IF QT-STATO OF PARTAB-ART NOT = 0
  3842*        IF SOCIETA OF C-MAT-A-BARRE = 4
  3843*        AND ANNO OF REC-ANAMAT = 5 AND
  3844*        STAGIONE OF REC-ANAMAT = 4
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  68
* READVE3.cob
  3845*           DISPLAY "FORNITORE 4 NON AMMESSO PER AI 05"
  3846*           PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3847*           GO TO EX-TRATTA-LEGGI.
  3848     IF QT-STATO OF PARTAB-ART NOT = 0
  3849       MOVE D-MAT OF REC-ANAMAT TO D-MAT-MEM
  3850*BUDA*
  3851       MOVE PRIMA-TG OF REC-ANAMAT TO PTG-MEM
  3852       MOVE VALID-REC OF REC-ANAMAT TO VAL-REC-MEM
  3853*VACO*
  3854       MOVE COSTO OF REC-ANAMAT  TO COSTO-MEM
  3855*VACO*
  3856*ASOLOB2C*
  3857       MOVE C-MAT OF REC-ANAMAT TO ANACST-C-MAT-COM
  3858       MOVE MAG-INPUT-R TO ANACST-MAG-COM
  3859* NO-DATGE
  3860*       PERFORM S-SET-1 THRU S-SET-1-EX
  3861* NO-DATGE
  3862       PERFORM RIVALUTA-COSTO-ANAMAT
  3863           THRU EX-RIVALUTA-COSTO-ANAMAT
  3864       IF ANACST-CST-COM NOT = 0
  3865         MOVE ANACST-CST-COM TO COSTO-MEM
  3866       END-IF
  3867* NO-DATGE
  3868*       PERFORM S-SET-2 THRU S-SET-2-EX
  3869* NO-DATGE
  3870*ASOLOB2C*
  3871*       MOVE "SITPF;" TO W-NOME-DATA-SET
  3872*       MOVE "C-MAT;" TO W-NOME-CAMPO
  3873*       MOVE C-MAT-TRANS-RID TO W-VALORE-CAMPO
  3874*       PERFORM TTDBFIND THRU EX-TTDBFIND
  3875*       IF W-OK-IMAGE
  3876*          MOVE 5 TO W-MODO
  3877*          PERFORM TTDBGET-S THRU EX-TTDBGET-S
  3878*          PERFORM TTDBGET-S THRU EX-TTDBGET-S
  3879*                   UNTIL NOT W-OK-IMAGE OR
  3880**MAG6/7*
  3881**                     MAGAZZINO OF REC-SITPF = 7
  3882*                      MAGAZZINO OF REC-SITPF = MAG-INPUT-R
  3883*       END-IF
  3884*       IF NOT W-OK-IMAGE
  3885*          DISPLAY "Manca Sit  "
  3886*                       C-MAT-A-BARRE-RID
  3887*          PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3888*          GO TO EX-TRATTA-LEGGI.
  3889*NOPRZ*
  3890     MOVE SPACE TO RISP-NO-GIAC RISP-NO-PREZZO.
  3891*
  3892*BUDA*
  3893     MOVE 0 TO PREZZO-MEM.
  3894     IF QT-STATO OF PARTAB-ART NOT = 0
  3895*ACQUO*
  3896*           AND (SI-DT-ESTERO OR
  3897*          (FLAG-ANACON NOT = '0' AND NOT = ' ') OR
  3898*          PRIORITA OF REC-INDIRIZZI = 4 )
  3899           AND PRIORITA OF REC-INDIRIZZI = 4
  3900*
  3901       PERFORM CERCA-PREZZO-V THRU EX-CERCA-PREZZO-V
  3902       IF NOT W-OK-IMAGE OR PREZZO-MEM = 0
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  69
* READVE3.cob
  3903          DISPLAY "Manca prezzo al pubblico "
  3904          DISPLAY "    " C-MAT-A-BARRE-RID
  3905          PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  3906          GO TO EX-TRATTA-LEGGI.
  3907*
  3908     IF QT-STATO OF PARTAB-ART NOT = 0
  3909*NOPRZ*
  3910       MOVE 0 TO PREZZO-MEM PREZZO-ANAMAT
  3911*PRODI*
  3912       MOVE 0 TO CAMBIO-MEM
  3913*
  3914       PERFORM CERCA-PREZZO THRU EX-CERCA-PREZZO
  3915*NOPRZ*
  3916*PRZANABU*
  3917*      IF PREZZO-MEM = 0 OR PREZZO-MEM = PREZZO-ANAMAT
  3918       IF PREZZO-MEM = 0
  3919         OR ( PREZZO-MEM = PREZZO-ANAMAT AND
  3920                   NOT SI-DT-ESTERO )
  3921*
  3922*          DISPLAY "Prz vend a CLI 0 o senza sconto"
  3923*          DISPLAY "   " C-MAT-A-BARRE-RID
  3924*NOPRZ*
  3925          MOVE 0 TO OK-PREZZO
  3926          PERFORM TRATTA-NO-PREZZO THRU EX-TRATTA-NO-PREZZO
  3927               UNTIL PREZZO-OK
  3928          IF RISP-NO-PREZZO NOT = "S"
  3929            GO TO EX-TRATTA-LEGGI
  3930          END-IF
  3931*sempre0*
  3932          if prezzo-mem not = 0
  3933            move 0 to prezzo-mem
  3934          end-if
  3935*
  3936       END-IF
  3937     ELSE
  3938*PRODI*
  3939       MOVE CAMBIO-TAB(QT-INDEX-ELEM OF PARTAB-ART)
  3940           TO CAMBIO-MEM
  3941*
  3942       MOVE D-MAT-TAB(QT-INDEX-ELEM OF PARTAB-ART)
  3943           TO D-MAT-MEM
  3944       MOVE PREZZO-TAB(QT-INDEX-ELEM OF PARTAB-ART)
  3945           TO PREZZO-MEM.
  3946*    IF PREZZO-MEM = 0
  3947*         DISPLAY "Manca prz vend a CLI "
  3948*         DISPLAY "   " C-MAT-A-BARRE-RID
  3949*NOPRZ*
  3950*         MOVE 0 TO OK-PREZZO
  3951*         PERFORM TRATTA-NO-PREZZO THRU EX-TRATTA-NO-PREZZO
  3952*              UNTIL PREZZO-OK
  3953*         IF RISP-NO-PREZZO NOT = "S"
  3954*
  3955*           GO TO EX-TRATTA-LEGGI.
  3956*
  3957     IF QT-STATO OF PARTAB-ART NOT = 0
  3958       COMPUTE ELEM-ART = C-MAT-A-BARRE-RID / 10
  3959       MOVE D-MAT-MEM TO D-MAT-ELEM
  3960*BUDA*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  70
* READVE3.cob
  3961       MOVE PTG-MEM TO PRIMA-TG-ELEM
  3962       MOVE PREZZO-MEM TO PREZZO-ELEM
  3963*VACO*
  3964       MOVE COSTO-MEM TO COSTO-ELEM
  3965*VACO*
  3966*PRODI*
  3967*      MOVE STK-CAMBIO TO CAMBIO-ELEM
  3968       MOVE CAMBIO-MEM TO CAMBIO-ELEM
  3969       MOVE VAL-REC-MEM TO TIPO-ANA-ELEM
  3970       MOVE LOW-VALUE TO QTA-TAGLIE-ELEM
  3971       MOVE QTA-GIAC OF REC-SITPF TO QTA-GIAC-ELEM
  3972     ELSE
  3973       MOVE ART-TAB-LETTI(QT-INDEX-ELEM OF PARTAB-ART)
  3974                   TO ART-ELEM-LETTI
  3975       MOVE D-MAT-ELEM TO D-MAT-MEM
  3976       MOVE PRIMA-TG-ELEM TO PTG-MEM.
  3977     COMPUTE PREZZO-D = PREZZO-MEM / 100.
  3978     ADD 1 TO QTA-TAGLIA-ELEM(NTG-MEM).
  3979     IF QTA-GIAC-PF-ELEM(NTG-MEM) <
  3980             QTA-TAGLIA-ELEM(NTG-MEM)
  3981           DISPLAY "Manca giac "
  3982             C-MAT-A-BARRE-RID
  3983        MOVE 0 TO OK-GIAC
  3984        PERFORM TRATTA-NO-GIAC THRU EX-TRATTA-NO-GIAC
  3985               UNTIL GIAC-OK
  3986*NOPRZ*
  3987        IF RISP-NO-GIAC NOT = "S"
  3988*
  3989           GO TO EX-TRATTA-LEGGI.
  3990*NOPRZ1*
  3991*    IF RISP-NO-GIAC = "S" OR RISP-NO-PREZZO = "S"
  3992     IF RISP-NO-GIAC = "S"
  3993*PRZBU*
  3994      OR ( RISP-NO-PREZZO = "S" AND SI-DT-ESTERO )
  3995*
  3996       PERFORM INSERISCI-NO-GIAC-PREZZO
  3997           THRU EX-INSERISCI-NO-GIAC-PREZZO
  3998       GO TO EX-TRATTA-LEGGI.
  3999*
  4000*
  4001     ADD 1 TO IND-CAPI-LETTI.
  4002     ADD PREZZO-MEM TO PREZZO-TOT.
  4003     MOVE IND-CAPI-LETTI TO CONT-D.
  4004     DISPLAY CONT-D.
  4005*UNICODDT*
  4006     ADD 1 TO TAB-CAPI-LETTI.
  4007     IF XD = "S"
  4008          DISPLAY "sono in add 1 to ind-capi-letti "
  4009          display dep-tab-unico-ddt
  4010*          display "return per proseguire"
  4011*          accept pro
  4012          .
  4013*UNICODDT*
  4014*EURO*
  4015*EURO1*
  4016     IF W-FORMATO-INTERNO NOT > 011231
  4017       MOVE PREZZO-MEM TO IE-IMPORTO-IN
  4018       PERFORM PRZ-INLIT THRU EX-PRZ-INLIT
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  71
* READVE3.cob
  4019       COMPUTE PREZZO-D = IE-IMPORTO-OU / 100
  4020     ELSE
  4021       COMPUTE PREZZO-D = PREZZO-MEM / 100.
  4022*
  4023*
  4024*     CALL "FAIBEEP".
  4025       DISPLAY CONT-D " "
  4026                  D-MAT-ELEM SPACE PREZZO-D.
  4027*UNICODDT*
  4028     PERFORM RIMETTI-DEP-TAB-UNICO-DDT THRU
  4029          EX-RIMETTI-DEP-TAB-UNICO-DDT.
  4030     IF XD = "S"
  4031         DISPLAY "sono in tratta-leggi DOPO RIMETTI-DEP-TAB"
  4032         DISPLAY "AS=" TAB-AS " CL=" TAB-CL
  4033             " MAX-CAPI=" TAB-MAX-CAPI
  4034             " CAPI-LETTI=" TAB-CAPI-LETTI
  4035*          DISPLAY ELE-TAB-UNICO-DDT(1)
  4036*          DISPLAY ELE-TAB-UNICO-DDT(2)
  4037*          DISPLAY ELE-TAB-UNICO-DDT(3)
  4038*          DISPLAY ELE-TAB-UNICO-DDT(4)
  4039*          DISPLAY ELE-TAB-UNICO-DDT(5)
  4040*          display "return per proseguire"
  4041*          accept pro
  4042          .
  4043*UNICODDT*
  4044*EURO*
  4045*EURO1*
  4046     IF W-FORMATO-INTERNO NOT > 011231
  4047       MOVE PREZZO-TOT TO IE-IMPORTO-IN
  4048       PERFORM PRZ-INLIT THRU EX-PRZ-INLIT
  4049       COMPUTE PREZZO-TOT-D = IE-IMPORTO-OU / 100
  4050*       DISPLAY "  Tot. L." PREZZO-TOT-D
  4051     ELSE
  4052       COMPUTE PREZZO-TOT-D = PREZZO-TOT / 100
  4053*       DISPLAY " Tot. Eur." PREZZO-TOT-D
  4054     END-IF
  4055*
  4056     PERFORM INSERT-ELEM-SING THRU EX-INSERT-ELEM-SING.
  4057     IF QT-STATO OF PARTAB-ART = 0
  4058       MOVE ART-ELEM-LETTI
  4059          TO ART-TAB-LETTI(QT-INDEX-ELEM OF PARTAB-ART)
  4060     ELSE
  4061       MOVE "K1" TO QT-FUNZIONE OF PARTAB-ART
  4062     CANCEL "QTABEL"
  4063       CALL "QTABEL" USING PARTAB-ART TABELLA-ARTICOLI-LETTI
  4064                           ART-ELEM-LETTI
  4065       IF QT-STATO OF PARTAB-ART NOT = 0
  4066         MOVE QT-STATO OF PARTAB-ART TO ERR-DISP
  4067         DISPLAY "ERR INSERIM QTABEL " ERR-DISP
  4068               " - TRATTA-LEGGI"
  4069     CANCEL "QDBERROR"
  4070         CALL "QDBERROR" USING W-COMMON.
  4071     MOVE COD-IN TO MEM-COD-IN.
  4072 EX-TRATTA-LEGGI.
  4073     EXIT.
  4074*
  4075*
  4076*NOPRZ*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  72
* READVE3.cob
  4077 INSERISCI-NO-GIAC-PREZZO.
  4078     ADD 1 TO IND-CAPI-NO-GIAC.
  4079     IF RISP-NO-GIAC = "S"
  4080       DISPLAY "INSERITO Manca GIAC."
  4081*PRZBU*
  4082       MOVE "MancaGIAC" TO CAUSALE-NO-GIAC (IND-CAPI-NO-GIAC).
  4083*
  4084*NOPRZ1*
  4085*PRZBU*
  4086     IF ( RISP-NO-PREZZO = "S" AND SI-DT-ESTERO )
  4087       DISPLAY "INS. Manca PREZZO x ESTERO"
  4088       MOVE "MancaPRZ" TO CAUSALE-NO-PRZ (IND-CAPI-NO-GIAC).
  4089*
  4090     MOVE C-MAT-A-BARRE-RID TO
  4091              C-MAT-NO-GIAC (IND-CAPI-NO-GIAC).
  4092     MOVE D-MAT-MEM TO D-MAT-NO-GIAC (IND-CAPI-NO-GIAC).
  4093     MOVE PREZZO-MEM TO PREZZO-NO-GIAC (IND-CAPI-NO-GIAC).
  4094 EX-INSERISCI-NO-GIAC-PREZZO.
  4095     EXIT.
  4096*
  4097*
  4098 TTDBGET-S.
* 4099     COPY PDBGET REPLACING AREA-REC-SET BY REC-SITPF
* 4100                  EX-TTDBGET BY EX-TTDBGET-S.
  4101*                                           *********************
  4102*IF X8=OFF
  4103*CONTROL NOLIST
  4104*IF
  4105     CALL "DBGET" USING W-NOME-DATA-BASE-1
  4106                        W-NOME-DATA-SET
  4107                        W-MODO
  4108                        W-CA-IMAGE
  4109                        W-TUTTO-RECORD
  4110                        REC-SITPF
  4111                        W-VALORE-CAMPO.
  4112     IF W-ERRORI-TRAGICI
  4113        MOVE 4 TO W-INDICE-8
  4114        CALL "QDBERROR" USING W-COMMON.
  4115 EX-TTDBGET-S.
  4116     EXIT.
  4117*
  4118*
  4119*CONTROL LIST
  4120*
  4121*
  4122*
  4123 TTDBGET.
* 4124     COPY PDBGET.
  4125*                                           ********************* PDBGET
  4126*IF X8=OFF                                                        PDBGET
  4127*CONTROL NOLIST                                                   PDBGET
  4128*IF                                                               PDBGET
  4129     CALL "DBGET" USING W-NOME-DATA-BASE-1                        PDBGET
  4130                        W-NOME-DATA-SET                           PDBGET
  4131                        W-MODO                                    PDBGET
  4132                        W-CA-IMAGE                                PDBGET
  4133                        W-TUTTO-RECORD                            PDBGET
  4134                        AREA-REC-SET                              PDBGET
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  73
* READVE3.cob (/home/prorosa/cobol/cpy/PDBGET)
  4135                        W-VALORE-CAMPO.                           PDBGET
  4136     IF W-ERRORI-TRAGICI                                          PDBGET
  4137        MOVE 4 TO W-INDICE-8                                      PDBGET
  4138        CALL "QDBERROR" USING W-COMMON.                           PDBGET
  4139 EX-TTDBGET.                                                      PDBGET
  4140     EXIT.                                                        PDBGET
  4141*                                                                 PDBGET
  4142*                                                                 PDBGET
  4143*CONTROL LIST                                                     PDBGET
  4144*                                                                 PDBGET
  4145*
  4146*
  4147 TRATTA-NO-GIAC.
  4148     PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM.
  4149     MOVE SPACE TO RISP-NO-GIAC.
  4150     DISPLAY "     " D-MAT-ELEM SPACE PREZZO-D.
  4151     DISPLAY "CONFERMI MANCA GIAC ? (S/N)".
  4152*     ACCEPT RISP-NO-GIAC.
  4153     MOVE "N" TO RISP-NO-GIAC
  4154     DISPLAY RISP-NO-GIAC
  4155*
  4156     IF RISP-NO-GIAC NOT = "S" AND NOT = "N"
  4157        GO TO EX-TRATTA-NO-GIAC.
  4158     MOVE 1 TO OK-GIAC.
  4159*NOPRZ*
  4160*    IF RISP-NO-GIAC NOT = "S"
  4161*       NEXT SENTENCE
  4162*      ELSE
  4163*         DISPLAY "INSERITO Manca GIAC."
  4164*         ADD 1 TO IND-CAPI-NO-GIAC
  4165*         MOVE C-MAT-A-BARRE-RID TO
  4166*                  C-MAT-NO-GIAC (IND-CAPI-NO-GIAC)
  4167*         MOVE D-MAT-MEM TO D-MAT-NO-GIAC (IND-CAPI-NO-GIAC)
  4168*         MOVE PREZZO-MEM TO PREZZO-NO-GIAC (IND-CAPI-NO-GIAC).
  4169 EX-TRATTA-NO-GIAC.
  4170     EXIT.
  4171*
  4172*NOPRZ*
  4173 TRATTA-NO-PREZZO.
  4174*     PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM.
  4175     MOVE SPACE TO RISP-NO-PREZZO.
  4176*NODMAT*
  4177**    DISPLAY "     " D-MAT-ELEM.
  4178*     DISPLAY "     " D-MAT-MEM.
  4179*     DISPLAY "CONFERMI MANCA PREZZO ? (S/N)".
  4180*     ACCEPT RISP-NO-PREZZO.
  4181     MOVE "S" TO RISP-NO-PREZZO.
  4182*     DISPLAY RISP-NO-PREZZO
  4183*
  4184     IF RISP-NO-PREZZO NOT = "S" AND NOT = "N"
  4185        GO TO EX-TRATTA-NO-PREZZO.
  4186     MOVE 1 TO OK-PREZZO.
  4187 EX-TRATTA-NO-PREZZO.
  4188     EXIT.
  4189*
  4190*
  4191 TTDBFIND.
* 4192     COPY PDBFIND.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  74
* READVE3.cob (/home/prorosa/cobol/cpy/PDBFIND)
  4193*                                           ********************* PDBFIND
  4194*IF X8=OFF                                                        PDBFIND
  4195*CONTROL NOLIST                                                   PDBFIND
  4196*IF                                                               PDBFIND
  4197     MOVE 1 TO W-MODO.                                            PDBFIND
  4198     CALL "DBFIND" USING W-NOME-DATA-BASE-1                       PDBFIND
  4199                         W-NOME-DATA-SET                          PDBFIND
  4200                         W-MODO                                   PDBFIND
  4201                         W-CA-IMAGE                               PDBFIND
  4202                         W-NOME-CAMPO                             PDBFIND
  4203                         W-VALORE-CAMPO.                          PDBFIND
  4204     IF W-ERRORI-TRAGICI                                          PDBFIND
  4205        MOVE 3 TO W-INDICE-8                                      PDBFIND
  4206        CALL "QDBERROR" USING W-COMMON.                           PDBFIND
  4207 EX-TTDBFIND.                                                     PDBFIND
  4208     EXIT.                                                        PDBFIND
  4209*                                                                 PDBFIND
  4210*CONTROL LIST                                                     PDBFIND
  4211*                                                                 PDBFIND
  4212*                                                                 PDBFIND
  4213*
  4214 INSERT-ELEM-SING.
  4215     MOVE "K1" TO QT-FUNZIONE OF PARTAB-SING.
  4216*UNICODDT*
  4217     MOVE IND-CAPI-LETTI TO CONT-SING.
  4218*     MOVE TAB-CAPI-LETTI TO CONT-SING.
  4219*UNICODDT*
  4220     MOVE C-MAT-A-BARRE-RID TO C-MAT-SING.
  4221     MOVE D-MAT-MEM TO D-MAT-SING.
  4222     MOVE PTG-MEM TO PRIMA-TG-SING.
  4223     MOVE PREZZO-MEM TO PREZZO-SING.
  4224*MOVSKU
  4225     MOVE SPACES TO SKU-SING.
  4226     CANCEL "QTABEL"
  4227     CALL "QTABEL" USING PARTAB-SING
  4228                           TABELLA-SINGOLI
  4229                              ELEMENTO-SINGOLI.
  4230*     IF QT-STATO OF PARTAB-SING NOT = 0
  4231*        MOVE QT-STATO OF PARTAB-SING TO ERR-DISP
  4232*        DISPLAY "ERR QTABEL SING  " ERR-DISP
  4233*        DISPLAY "INSERIMENTO "
  4234*        DISPLAY "PER ELEMENTO     " C-MAT-A-BARRE-RID
  4235*     CANCEL "QDBERROR"
  4236*        CALL "QDBERROR" USING W-COMMON.
  4237 EX-INSERT-ELEM-SING.
  4238     EXIT.
  4239*
  4240*
  4241* ANNULLA-PRECEDENTE.
  4242*     IF MEM-COD-IN = SPACES
  4243*       DISPLAY "nulla da annullare"
  4244*       GO TO EX-ANNULLA-PRECEDENTE.
  4245*     MOVE MEM-COD-IN TO COD-IN.
  4246*     COMPUTE ELEM-ART = C-MAT-A-BARRE-RID / 10
  4247*     MOVE "K2" TO QT-FUNZIONE OF PARTAB-ART.
  4248*     CANCEL "QTABEL"
  4249*     CALL "QTABEL" USING PARTAB-ART TABELLA-ARTICOLI-LETTI
  4250*                         ELEM-ART.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  75
* READVE3.cob
  4251*     IF QT-STATO OF PARTAB-ART NOT = 0
  4252*       MOVE QT-STATO OF PARTAB-ART TO ERR-DISP
  4253*         DISPLAY "ERR RILETTURA QTABEL " ERR-DISP
  4254*               " - ANNULLA-PRECEDENTE"
  4255*     CANCEL "QDBERROR"
  4256*         CALL "QDBERROR" USING W-COMMON.
  4257*     SUBTRACT PREZZO-TAB(QT-INDEX-ELEM OF PARTAB-ART)
  4258*              FROM PREZZO-TOT.
  4259*     PERFORM DELETE-ELEM-SING THRU EX-DELETE-ELEM-SING.
  4260*     SUBTRACT 1 FROM QTA-TAGLIA-TAB(QT-INDEX-ELEM OF PARTAB-ART,
  4261*                 TAGLIA OF C-MAT-A-BARRE) IND-CAPI-LETTI.
  4262*     MOVE QTA-TAGLIA-TAB(QT-INDEX-ELEM OF PARTAB-ART,
  4263*                 TAGLIA OF C-MAT-A-BARRE) TO DISP-4.
  4264*     DISPLAY "annullata 1 lettura "
  4265*     DISPLAY "ancora " DISP-4
  4266*                  C-MAT-A-BARRE-RID.
  4267**EURO1*
  4268*     IF W-FORMATO-INTERNO NOT > 011231
  4269*       MOVE PREZZO-TOT TO IE-IMPORTO-IN
  4270*       PERFORM PRZ-INLIT THRU EX-PRZ-INLIT
  4271*       COMPUTE PREZZO-TOT-D = IE-IMPORTO-OU / 100
  4272*       DISPLAY "  Tot. L." PREZZO-TOT-D
  4273*     ELSE
  4274*       COMPUTE PREZZO-TOT-D = PREZZO-TOT / 100
  4275*       DISPLAY " Tot. Eur." PREZZO-TOT-D.
  4276**
  4277* EX-ANNULLA-PRECEDENTE.
  4278*     EXIT.
  4279*
  4280*EURO1*
  4281 PRZ-INLIT.
* 4282     COPY PDAEU.
  4283*                                           ********************* PDAEU
  4284*IF X8=OFF                                                        PDAEU
  4285*CONTROL NOLIST                                                   PDAEU
  4286*IF                                                               PDAEU
  4287       CALL "QDAEURO" USING        PAR-INEU                       PDAEU
  4288       IF IE-ERRORE                                               PDAEU
  4289         DISPLAY IE-MSG UPON CONSOLE                              PDAEU
  4290       END-IF.                                                    PDAEU
  4291*                                           ********************* PDAEU
  4292*CONTROL LIST                                                     PDAEU
  4293*                                                                 PDAEU
  4294 EX-PRZ-INLIT.
  4295     EXIT.
  4296*
  4297 DELETE-ELEM-SING.
  4298     MOVE "K3" TO QT-FUNZIONE OF PARTAB-SING.
  4299     MOVE IND-CAPI-LETTI TO CONT-SING.
  4300     MOVE C-MAT-A-BARRE-RID TO C-MAT-SING.
  4301     CANCEL "QTABEL"
  4302     CALL "QTABEL" USING PARTAB-SING
  4303                           TABELLA-SINGOLI
  4304                              ELEMENTO-SINGOLI.
  4305     IF QT-STATO OF PARTAB-SING NOT = 0
  4306        MOVE QT-STATO OF PARTAB-SING TO ERR-DISP
  4307        DISPLAY "ERR QTABEL SING  " ERR-DISP
  4308        DISPLAY "ANNULLO PREC"
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  76
* READVE3.cob
  4309        DISPLAY "PER ELEMENTO     " C-MAT-A-BARRE-RID
  4310     CANCEL "QDBERROR"
  4311        CALL "QDBERROR" USING W-COMMON.
  4312 EX-DELETE-ELEM-SING.
  4313     EXIT.
  4314*
  4315*
  4316*PAGE
  4317*
  4318 TRATTA-LETTI.
  4319*    DISPLAY "hJ".
  4320*       PERFORM VARYING IR FROM 1 BY 1
  4321*            UNTIL IR > SALTO
  4322        DISPLAY SPACE
  4323*       END-PERFORM
  4324*
  4325     MOVE IND-CAPI-LETTI TO TOT-CAPI-LETTI-1.
  4326     DISPLAY "- Tot CAPI - " TOT-CAPI-LETTI-1.
  4327     MOVE IND-CAPI-NO-GIAC TO TOT-CAPI-NO-GIAC.
  4328*NOPRZ*
  4329     DISPLAY "- No GIAC./PREZZO - " TOT-CAPI-NO-GIAC.
  4330*
  4331     DISPLAY " ".
  4332     DISPLAY "Vuoi STORNARE "
  4333         "(SI-NO)".
  4334     MOVE SPACES TO CONFERMA-STORNO.
  4335     ACCEPT CONFERMA-STORNO.
  4336     IF (CONFERMA-STORNO = "SI" OR = "si") AND
  4337               TOT-CAPI-LETTI-1 NOT = 0
  4338*       DISPLAY "hJ"
  4339*       PERFORM VARYING IR FROM 1 BY 1
  4340*            UNTIL IR > SALTO
  4341        DISPLAY SPACE
  4342*       END-PERFORM
  4343        DISPLAY    "Dammi il CODICE"
  4344        MOVE SPACES TO COD-IN MEM-COD-IN
  4345        DISPLAY " . fine lettura"
  4346        DISPLAY " @ storno totale"
  4347*       DISPLAY "l"
  4348*        MOVE 3 TO QT-ADDR-KEY OF PARTAB-SING
  4349        MOVE 1 TO QT-ADDR-KEY OF PARTAB-SING
  4350*
  4351        MOVE 8 TO QT-LL-KEY OF PARTAB-SING
  4352     CANCEL "QSORTAB"
  4353        CALL "QSORTAB" USING PARTAB-SING TABELLA-SINGOLI
  4354        PERFORM TRATTA-STORNO THRU EX-TRATTA-STORNO
  4355               UNTIL LETT-FINE.
  4356*       DISPLAY "m".
  4357     IF IND-CAPI-LETTI NOT < 1
  4358        MOVE 5 TO W-MODO
  4359        PERFORM TTLOCK-T THRU EX-TTLOCK-T
  4360*volante*
  4361*       if CONTO-IN-R = 10010261
  4362*         perform leggi-paramdt-fittizi
  4363*                 thru ex-leggi-paramdt-fittizi
  4364*         perform agg-dparam-fittizi
  4365*                 thru ex-agg-dparam-fittizi
  4366*         move 61222 to AA-MM-GG-DDT
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  77
* READVE3.cob
  4367*       else
  4368          PERFORM LEGGI-PARAMDT THRU EX-LEGGI-PARAMDT
  4369          PERFORM AGG-DPARAM THRU EX-AGG-DPARAM
  4370          MOVE W-FORMATO-INTERNO TO AA-MM-GG-DDT
  4371*       end-if
  4372*
  4373*       DISPLAY "hJ"
  4374*       DISPLAY "BBBBBBB"
  4375        DISPLAY "Aggiorno  "
  4376        DISPLAY "BOLLA n.  " NUMERO-DDT
  4377*
  4378        IF QT-NUM-ELEM-EFF OF PARTAB-SING > 0
  4379* NO-DATGE
  4380*          PERFORM S-SET-1 THRU S-SET-1-EX
  4381* NO-DATGE
  4382          PERFORM INSERISCI-MOVSKU THRU INSERISCI-MOVSKU-EX
  4383                   VARYING IND-BARUNI FROM 1 BY 1
  4384                         UNTIL IND-BARUNI >
  4385                                 QT-NUM-ELEM-EFF OF PARTAB-SING
  4386          PERFORM S-S-COMMIT THRU S-S-COMMIT-EX
  4387* NO-DATGE
  4388*          PERFORM S-SET-2 THRU S-SET-2-EX
  4389* NO-DATGE
  4390        END-IF
  4391*BUDA*
  4392        IF PRIORITA OF REC-INDIRIZZI = 4
  4393*conv*
  4394          MOVE SPACE               TO DIR-VAR-VALUE
  4395          MOVE "RETIS_DIRECTORY"   TO DIR-VAR-NAME
  4396          DISPLAY DIR-VAR-NAME UPON ENVIRONMENT-NAME
  4397          ACCEPT DIR-VAR-VALUE FROM ENVIRONMENT-VALUE
  4398*
  4399          MOVE SPACE               TO USER-VAR-VALUE
  4400          MOVE "RETIS_UTENTE"      TO USER-VAR-NAME
  4401          DISPLAY USER-VAR-NAME UPON ENVIRONMENT-NAME
  4402          ACCEPT USER-VAR-VALUE FROM ENVIRONMENT-VALUE
  4403*
  4404          MOVE "dd_BARCNEG"        TO FILE-VAR-NAME
  4405          MOVE SPACES              TO FILE-VAR-VALUE
  4406*movsku
  4407*          STRING DIR-VAR-VALUE DELIMITED BY SPACE
  4408          STRING
  4409               USER-VAR-VALUE DELIMITED BY SPACE
  4410*              "_BARCNEG" DELIMITED BY SIZE
  4411               "_BC" DELIMITED BY SIZE
  4412               "_" AA-MM-GG-DDT NUMERO-DDT
  4413               "_" MAG-INPUT-R "_" NEG-IN
  4414               "_B"
  4415                          DELIMITED BY SIZE
  4416*movsku*
  4417                  INTO FILE-VAR-VALUE
  4418          DISPLAY FILE-VAR-NAME  UPON ENVIRONMENT-NAME
  4419          DISPLAY FILE-VAR-VALUE UPON ENVIRONMENT-VALUE
  4420*conv-end*
  4421          OPEN OUTPUT FILE-BC
  4422        END-IF
  4423        PERFORM SCRIVI-RECORD THRU EX-SCRIVI-RECORD
  4424            VARYING W-INDICE-3 FROM 1 BY 1
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  78
* READVE3.cob
  4425               UNTIL W-INDICE-3 > QT-NUM-ELEM-EFF OF PARTAB-ART
  4426*BUDA*
  4427        MOVE 1 TO QT-ADDR-KEY OF PARTAB-SING
  4428        MOVE 10 TO QT-LL-KEY OF PARTAB-SING
  4429     CANCEL "QSORTAB"
  4430        CALL "QSORTAB" USING PARTAB-SING
  4431                                TABELLA-SINGOLI
  4432        IF PRIORITA OF REC-INDIRIZZI = 4
  4433*MOVSKU
  4434           PERFORM INTESTA-FILE-BC THRU EX-INTESTA-FILE-BC
  4435*MOVSKU
  4436           PERFORM SCORRI-TAB-SING THRU EX-SCORRI-TAB-SING
  4437               VARYING W-INDICE-3 FROM 1 BY 1
  4438                 UNTIL W-INDICE-3 > QT-NUM-ELEM-EFF
  4439                      OF PARTAB-SING
  4440           CLOSE FILE-BC
  4441          ELSE
  4442             PERFORM CALL-COMMAND THRU EX-CALL-COMMAND
  4443             PERFORM SCORRI-TB THRU EX-SCORRI-TB
  4444             PERFORM CALL-COMMAND-2 THRU EX-CALL-COMMAND-2
  4445        END-IF
  4446        PERFORM SCRIVI-BOLLE THRU EX-SCRIVI-BOLLE
  4447        PERFORM CHIAMA-PRINTDDT THRU EX-CHIAMA-PRINTDDT
  4448*BUDA*
  4449*       IF FLAG-ANACON NOT = '0' AND NOT = ' '
  4450*         PERFORM CHIAMA-WRITERES THRU EX-CHIAMA-WRITERES
  4451*       END-IF
  4452        IF PRIORITA OF REC-INDIRIZZI = 4
  4453*EURO1*
  4454           MOVE DIVISA OF REC-CONFATT TO
  4455                          DIVISA-PRIMO-LETTO
  4456*          CANCEL "PRTBCEU6"
  4457*          CALL "PRTBCEU6" USING W-COMMON
  4458*MOVSKU
  4459*           CANCEL "PRTBCEU7"
  4460*           CALL "PRTBCEU7" USING W-COMMON
  4461           CANCEL "PRTBCEU8"
  4462           CALL "PRTBCEU8" USING W-COMMON
  4463                                 SQLCA
  4464                                 REC-CONFATT
  4465                                 AA-MM-GG-DDT
  4466                                 NUMERO-DDT
  4467                                 DIVISA-PRIMO-LETTO
  4468                                 DESTINO-USCITA
  4469        END-IF
  4470        IF IND-CAPI-NO-GIAC > 0
  4471           PERFORM STAMPA-NO-GIAC THRU EX-STAMPA-NO-GIAC
  4472        END-IF
  4473        PERFORM TTUNLOCK THRU EX-TTUNLOCK
  4474        IF FLAG-ANACON NOT = '0' AND NOT = ' '
  4475           PERFORM ALLINEA-BOLLA-ESTERO
  4476              THRU EX-ALLINEA-BOLLA-ESTERO
  4477        END-IF
  4478        PERFORM AVANZA-DDT THRU EX-AVANZA-DDT
  4479*TASTO-PER-CONTINUARE*
  4480        DISPLAY "premi un tasto per continuare..."
  4481        ACCEPT TASTO-INP
  4482*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  79
* READVE3.cob
  4483     END-IF.
  4484 EX-TRATTA-LETTI.
  4485     EXIT.
  4486*
  4487*
  4488 ALLINEA-BOLLA-ESTERO.
  4489     MOVE RIF-BOLLA-DDT TO INPUT-RIF-TRASF.
  4490     IF CONTO-FATTURA-MEM NOT = 0
  4491       MOVE CONTO-FATTURA-MEM TO INPUT-CONTO-TRASF
  4492     ELSE
  4493       MOVE CONTO-IN-R TO INPUT-CONTO-TRASF
  4494     END-IF.
  4495     CALL  "PYTHON"
  4496     USING "allinea_bolle_in_estero"
  4497           "allinea_estero"
  4498           PY-INPUT-TRASF
  4499           PY-OUTPUT-TRASF.
  4500     IF OUTPUT-ERR-TRASF NOT = SPACES
  4501        DISPLAY 'ERRORE!'
  4502        DISPLAY OUTPUT-ERR-TRASF
  4503     END-IF.
  4504 EX-ALLINEA-BOLLA-ESTERO.
  4505     EXIT.
  4506 AVANZA-DDT.
  4507*
  4508*questo rif_intr ha anno a due cifre
  4509   MOVE RIF-BOLLA-DDT TO INPUT-RIF-INTR.
  4510   IF CONTO-FATTURA-MEM NOT = 0
  4511     MOVE CONTO-FATTURA-MEM TO INPUT-CONTO
  4512   ELSE
  4513     MOVE CONTO-IN-R TO INPUT-CONTO
  4514   END-IF.
  4515   MOVE MAG-INPUT-R TO INPUT-MAG.
  4516*il flag E indica ddt stock o estero alla libreria dell'avanzament
  4517   MOVE 'E' TO INPUT-FLAG.
  4518   CALL "PYTHON" USING "avanzamento"
  4519                       "genera_avanzamento"
  4520                         PY-INPUT-REC-B
  4521                         PY-OUTPUT-REC-B.
  4522   IF OUTPUT-VAL = 'KO'
  4523       DISPLAY 'AVANZAMENTO NON RIUSCITO'.
  4524 EX-AVANZA-DDT.
  4525     EXIT.
  4526*
  4527*
  4528 CALL-COMMAND.
  4529     MOVE NUMERO-DDT TO BUILD-N-DDT
  4530                        PURGE-N-DDT
  4531                        FILE-N-DDT
  4532                        FILE-N-DDT-2
  4533                        PRINT-N-DDT
  4534                        PRINT-N-DDT-2.
  4535     CANCEL "COMMAND2"
  4536     CALL "COMMAND2" USING COMANDO-BUILD
  4537                                     ERR, ERR-PARM.
  4538     IF ERR NOT = 0
  4539        MOVE ERR TO ERR-DISP
  4540*ERR-279*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  80
* READVE3.cob
  4541        DISPLAY "Errore COMMAND BUILD - P" BUILD-N-DDT
  4542                SPACE "-  " ERR-DISP.
  4543*       STOP RUN.
  4544*
  4545     CANCEL "COMMAND2"
  4546     CALL "COMMAND2" USING COMANDO-FILE
  4547                                     ERR, ERR-PARM.
  4548     IF ERR NOT = 0
  4549        MOVE ERR TO ERR-DISP
  4550        DISPLAY "Errore COMMAND FILE ECQ - P" BUILD-N-DDT
  4551                SPACE "-  " ERR-DISP
  4552        MOVE 1 TO JRUNC
  4553        STOP RUN JRUNC.
  4554     OPEN OUTPUT FILE-PEND.
  4555 EX-CALL-COMMAND.
  4556     EXIT.
  4557*
  4558*
  4559 STAMPA-NO-GIAC.
  4560     MOVE 100 TO CONTA-RIGHE.
  4561     MOVE 1 TO CONTA-PAGINE.
  4562     PERFORM APRI-STAMPA THRU EX-APRI-STAMPA.
  4563     PERFORM STAMPA-DETT-N-G THRU EX-STAMPA-DETT-N-G
  4564                VARYING W-INDICE-3 FROM 1 BY 1
  4565                    UNTIL W-INDICE-3 >
  4566                        IND-CAPI-NO-GIAC.
  4567     PERFORM CHIUDI-STAMPA THRU EX-CHIUDI-STAMPA.
  4568 EX-STAMPA-NO-GIAC.
  4569     EXIT.
  4570*
  4571*
  4572 APRI-STAMPA.
  4573     MOVE  136  TO LL-RIGA.
  4574     MOVE 9999 TO N-MAX-RIGHE.
  4575     MOVE "N" TO PRE-NOME-FILE.
  4576     MOVE NUMERO-DDT TO TERM-N-FILE.
  4577     CALL "QOLPPR" USING PAR-PRINT
  4578                          RIGA BUFFER.
  4579     MOVE 0   TO N-STAMPANTE.
  4580     MOVE "M" TO COMANDO.
  4581     MOVE 66  TO N-RIGA-STAMPA.
  4582*NOPRZ*
  4583*    MOVE " MANCA GIACENZA SU VENDITA " TO DATI-RIGA.
  4584     MOVE " MANCA GIAC./PREZZO SU VENDITA " TO DATI-RIGA.
  4585     CALL "QWLPPR" USING PAR-PRINT
  4586                          RIGA BUFFER.
  4587     MOVE SPACES TO DATI-RIGA.
  4588 EX-APRI-STAMPA.
  4589     EXIT.
  4590*
  4591*
  4592 SCRIVI.
  4593     CALL "QWLPPR" USING PAR-PRINT
  4594                         RIGA BUFFER.
  4595     IF STATO OF PAR-PRINT NOT = 0
  4596        MOVE STATO OF PAR-PRINT TO STATO-DISPLAY
  4597        DISPLAY "ERRORE QPRINT CON STATO : " STATO-DISPLAY
  4598     CANCEL "QDBERROR"
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  81
* READVE3.cob
  4599        CALL "QDBERROR" USING W-COMMON.
  4600     MOVE SPACES TO DATI-RIGA.
  4601     MOVE "S" TO COMANDO.
  4602     MOVE 0 TO N-RIGA-STAMPA.
  4603 EX-SCRIVI.
  4604     EXIT.
  4605*
  4606*
  4607 INTESTA-PAGINA.
  4608     MOVE SPACE TO DATI-RIGA.
  4609     MOVE W-FORMATO-GG-MMM-AAAA TO DATA-T.
  4610     MOVE W-NUM-TERM TO NUM-PAG-T.
  4611     MOVE "term " TO D-PAG-T.
  4612     MOVE "P" TO COMANDO.
  4613     MOVE 2 TO N-RIGA-STAMPA.
  4614     MOVE 0 TO N-STAMPANTE.
  4615     PERFORM SCRIVI THRU
  4616              EX-SCRIVI.
  4617*
  4618     MOVE CONTO-IN-R TO CONTO-T.
  4619     MOVE D-CONTO-MEM TO D-CONTO-T.
  4620     MOVE "pag. " TO D-PAG-T.
  4621     MOVE CONTA-PAGINE TO NUM-PAG-T.
  4622     MOVE "P" TO COMANDO.
  4623     MOVE 3 TO N-RIGA-STAMPA.
  4624     MOVE 0 TO N-STAMPANTE.
  4625     PERFORM SCRIVI THRU
  4626              EX-SCRIVI.
  4627*
  4628*NOPRZ*
  4629     MOVE "     ELENCO MODELLI SENZA GIAC./PREZZO "
  4630                    TO DATI-RIGA.
  4631     MOVE 2 TO N-RIGA-STAMPA.
  4632     PERFORM SCRIVI THRU
  4633              EX-SCRIVI.
  4634*
  4635     MOVE 1 TO N-RIGA-STAMPA.
  4636     MOVE 5 TO CONTA-RIGHE.
  4637 EX-INTESTA-PAGINA.
  4638     EXIT.
  4639*
  4640*
  4641 STAMPA-DETT-N-G.
  4642     IF CONTA-RIGHE  > 50
  4643        PERFORM INTESTA-PAGINA THRU EX-INTESTA-PAGINA
  4644        ADD 1 TO CONTA-PAGINE.
  4645     MOVE C-MAT-NO-GIAC (W-INDICE-3) TO C-MAT-A-BARRE-RID.
  4646     MOVE 0 TO C-MAT-TRANS-RID.
  4647     MOVE MODELLO OF C-MAT-A-BARRE TO MODELLO OF
  4648                         C-MAT-TRANSITO.
  4649     MOVE VESTIBILITA OF C-MAT-A-BARRE TO
  4650              VEST-A OF C-MAT-TRANSITO.
  4651     MOVE SOCIETA OF C-MAT-A-BARRE TO
  4652               PROGR-ART OF C-MAT-TRANSITO.
  4653     MOVE PEZZO OF C-MAT-A-BARRE TO PEZZO-A OF
  4654                         C-MAT-TRANSITO.
  4655     MOVE VARIANTE-COL OF C-MAT-A-BARRE TO COLORE OF
  4656                         C-MAT-TRANSITO.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  82
* READVE3.cob
  4657     MOVE C-MAT-TRANS-RID TO C-MAT-ST.
  4658     MOVE D-MAT-NO-GIAC (W-INDICE-3) TO NOME-MOD-ST.
  4659     MOVE TAGLIA OF C-MAT-A-BARRE TO NTG-IN
  4660     MOVE FUNCTION idxtg(NTG-IN) TO TAGLIA-ST.
  4661     COMPUTE PREZZO-ST-EU = PREZZO-NO-GIAC (W-INDICE-3) / 100.
  4662*PRZBU*
  4663     MOVE CAUSALE-NO-GIAC (W-INDICE-3) TO
  4664                         NOGIAC-ST.
  4665     MOVE CAUSALE-NO-PRZ (W-INDICE-3) TO
  4666                         NOPRZ-ST.
  4667*
  4668     PERFORM SCRIVI THRU EX-SCRIVI.
  4669 EX-STAMPA-DETT-N-G.
  4670     EXIT.
  4671*
  4672*
  4673*
  4674 CHIUDI-STAMPA.
  4675     CALL "QCLPPR" USING PAR-PRINT
  4676                          RIGA BUFFER.
  4677 EX-CHIUDI-STAMPA.
  4678     EXIT.
  4679*
  4680*
  4681 CALL-COMMAND-2.
  4682     CLOSE FILE-PEND.
  4683*conv
  4684     MOVE "dd_PEND" TO WK-VAR-NAME.
  4685     MOVE SPACES    TO WK-VAR-VALUE.
  4686     DISPLAY WK-VAR-NAME UPON ENVIRONMENT-NAME.
  4687     ACCEPT WK-VAR-VALUE FROM ENVIRONMENT-VALUE.
  4688     MOVE WK-VAR-VALUE TO LPR-NOME-FILE.
  4689*
  4690     MOVE DESTINO-USCITA TO LPR-NUM-STAMPANTE.
  4691*non stampa i pendenti, verranno stampati manualmente
  4692*se necessario
  4693*    CALL "SYSTEM" USING COMANDO-LPR-LINUX
  4694*            GIVING INTO ERR.
  4695*    IF ERR NOT = 0
  4696*       MOVE ERR TO ERR-DISP
  4697*       DISPLAY "**READVE3 - Errore COMANDO: "
  4698*               COMANDO-LPR-LINUX
  4699*               " - errno: " ERR-DISP
  4700*    END-IF.
  4701*
  4702*    CALL INTRINSIC "COMMAND" USING COMANDO-FILE-2
  4703*                                    ERR, ERR-PARM.
  4704*    IF C-C NOT = 0
  4705*       MOVE ERR TO ERR-DISP
  4706*       DISPLAY "Errore COMMAND FILE ECQ-2 - P" BUILD-N-DDT
  4707*               SPACE "-  " ERR-DISP
  4708*       STOP RUN.
  4709*    CALL INTRINSIC "COMMAND" USING COMANDO-PRINT
  4710*                                    ERR, ERR-PARM.
  4711*    IF C-C NOT = 0
  4712*       MOVE ERR TO ERR-DISP
  4713*       DISPLAY "Errore COMMAND PRINT ECQ - P" BUILD-N-DDT
  4714*               SPACE "-  " ERR-DISP
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  83
* READVE3.cob
  4715*       STOP RUN.
  4716*conv-end
  4717*    CALL INTRINSIC "COMMAND" USING COMANDO-PURGE
  4718*                                    ERR, ERR-PARM.
  4719*    IF C-C NOT = 0
  4720*       MOVE ERR TO ERR-DISP
  4721*       DISPLAY "Errore COMMAND PURGE - P" BUILD-N-DDT
  4722*               SPACE "-  " ERR-DISP
  4723*       STOP RUN.
  4724 EX-CALL-COMMAND-2.
  4725     EXIT.
  4726*
  4727*
  4728*BUDA*
  4729 CHIAMA-WRITERES.
  4730     MOVE 0 TO CAPO-CONTO.
  4731     MOVE NEG-IN TO SOTTO-CONTO.
  4732*
  4733     MOVE RIF-BOLLA-DDT TO RIF-INTR-WR.
  4734*MAG6/7*
  4735*    MOVE 7 TO MAGAZZINO-WR.
  4736     MOVE MAG-INPUT-R TO MAGAZZINO-WR.
  4737     MOVE FLAG-ANACON TO DEST-WR.
  4738*FIFRA*
  4739*    MOVE CONTO-IN-R TO CONTO-CLI-WR.
  4740     IF CONTO-FATTURA-MEM NOT = 0
  4741       MOVE CONTO-FATTURA-MEM TO CONTO-CLI-WR
  4742     ELSE
  4743       MOVE CONTO-IN-R TO CONTO-CLI-WR.
  4744*
  4745     MOVE CODICE-CONTO TO CONTO-DEST-WR.
  4746     MOVE DIVISA-MEM TO DIVISA-WR.
  4747     MOVE LISTINO-MEM TO LISTINO-WR.
  4748     MOVE "VESD" TO CAUSALE-WR.
  4749     MOVE 0 TO DATA-CARICO-WR.
  4750     CANCEL "WRITERES"
  4751     CALL "WRITERES" USING W-COMMON
  4752                           SQLCA
  4753                           CAMPI-X-WRITE DATA-CARICO-WR.
  4754 EX-CHIAMA-WRITERES.
  4755     EXIT.
  4756*
  4757*
  4758 SCRIVI-BOLLE.
  4759     MOVE "BOLLE;" TO W-NOME-DATA-SET.
  4760*FIFRA*
  4761*    MOVE CONTO-IN-R TO CONTO OF REC-BOLLE.
  4762     IF CONTO-FATTURA-MEM NOT = 0
  4763       MOVE CONTO-FATTURA-MEM TO CONTO OF REC-BOLLE
  4764     ELSE
  4765       MOVE CONTO-IN-R TO CONTO OF REC-BOLLE.
  4766*
  4767     MOVE RIF-BOLLA-DDT TO RIF-INTERNO OF REC-BOLLE.
  4768*BOLL*
  4769*MAG6/7*
  4770*    MOVE 7 TO MAGAZZINO OF REC-BOLLE
  4771     MOVE MAG-INPUT-R TO MAGAZZINO OF REC-BOLLE
  4772     MOVE 0 TO DATA-NASCITA OF REC-BOLLE
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  84
* READVE3.cob
  4773               NUM-PRE-FATT OF REC-BOLLE.
  4774     MOVE 1 TO NUMERO       OF REC-BOLLE.
  4775     MOVE SPACES TO VAL-REC OF REC-BOLLE.
  4776     PERFORM TTDBPUT THRU EX-TTDBPUT.
  4777     IF NOT W-OK-IMAGE
  4778        MOVE W-STATUS-WORD-IMAGE TO ERR-DISP
  4779        DISPLAY "Err. PUT BOLLE  " ERR-DISP
  4780        DISPLAY "Per CONTO  " CONTO-IN-R
  4781           "   RIF-INTERNO  " RIF-BOLLA-DDT
  4782     CANCEL "QDBERROR"
  4783        CALL "QDBERROR" USING W-COMMON.
  4784 EX-SCRIVI-BOLLE.
  4785     EXIT.
  4786*
  4787*
  4788 TRATTA-STORNO.
  4789     MOVE SPACES TO COD-IN.
  4790     ACCEPT COD-IN.
  4791     IF LETT-FINE
  4792        GO TO EX-TRATTA-STORNO.
  4793     IF LETT-ANN-TUTTO
  4794        MOVE 0 TO IND-CAPI-LETTI
  4795        MOVE "." TO COD-IN-RID
  4796        GO TO EX-TRATTA-STORNO.
  4797     IF C-MAT-A-BARRE-RID NOT NUMERIC
  4798        DISPLAY "COD non num >> RILEGGERE"
  4799        PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  4800        GO TO EX-TRATTA-STORNO.
  4801     MOVE TAGLIA OF C-MAT-A-BARRE TO NTG-IN
  4802     MOVE FUNCTION idxtg(NTG-IN)
  4803              TO NTG-OUT
  4804     MOVE SOCIETA OF C-MAT-A-BARRE TO SOC-COM.
  4805     MOVE 0 TO PRE-SOC.
  4806     MOVE SOC-COM TO SOCIETA OF C-MAT-A-BARRE.
  4807     COMPUTE ELEM-ART = C-MAT-A-BARRE-RID / 10
  4808     MOVE "K2" TO QT-FUNZIONE OF PARTAB-ART.
  4809     CANCEL "QTABEL"
  4810     CALL "QTABEL" USING PARTAB-ART TABELLA-ARTICOLI-LETTI
  4811                         ELEM-ART.
  4812     IF QT-STATO OF PARTAB-ART NOT = 0
  4813       PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  4814       DISPLAY "Manca lettura "
  4815       GO TO EX-TRATTA-STORNO.
  4816     MOVE ART-TAB-LETTI(QT-INDEX-ELEM OF PARTAB-ART)
  4817           TO ART-ELEM-LETTI.
  4818     SUBTRACT 1 FROM QTA-TAGLIA-ELEM(NTG-OUT).
  4819     IF QTA-TAGLIA-ELEM(NTG-OUT) < 0
  4820           PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  4821           DISPLAY "Taglia non stornabile"
  4822           GO TO EX-TRATTA-STORNO.
  4823     PERFORM DELETE-ELEM-SING THRU EX-DELETE-ELEM-SING.
  4824     SUBTRACT 1 FROM IND-CAPI-LETTI.
  4825     SUBTRACT PREZZO-ELEM FROM PREZZO-TOT.
  4826     MOVE  QTA-TAGLIA-ELEM(NTG-OUT)
  4827       TO DISP-4.
  4828     DISPLAY "ancora " DISP-4
  4829                  C-MAT-A-BARRE-RID.
  4830*EURO1*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  85
* READVE3.cob
  4831     IF W-FORMATO-INTERNO NOT > 011231
  4832       MOVE PREZZO-TOT TO IE-IMPORTO-IN
  4833       PERFORM PRZ-INLIT THRU EX-PRZ-INLIT
  4834       COMPUTE PREZZO-TOT-D = IE-IMPORTO-OU / 100
  4835*       DISPLAY "  Tot. L." PREZZO-TOT-D
  4836     ELSE
  4837       COMPUTE PREZZO-TOT-D = PREZZO-TOT / 100
  4838*       DISPLAY " Tot. Eur." PREZZO-TOT-D
  4839     END-IF
  4840*
  4841     MOVE ART-ELEM-LETTI
  4842        TO ART-TAB-LETTI(QT-INDEX-ELEM OF PARTAB-ART).
  4843 EX-TRATTA-STORNO.
  4844     EXIT.
  4845*
  4846*
  4847 SCORRI-TB.
  4848     PERFORM COMANDI-IGP-TESTA THRU EX-COMANDI-IGP-TESTA.
  4849*
  4850     MOVE 0 TO VERT-EXP-IGP
  4851               ORIZ-EXP-IGP.
  4852*
  4853     MOVE 80 TO ROW-IGP.
  4854     MOVE 100 TO COL-IGP.
  4855     PERFORM COMANDI-IGP-TESTA-1 THRU EX-COMANDI-IGP-TESTA-1.
  4856     MOVE SPACES TO RIGA-PEND-COM.
  4857     MOVE QT-NUM-ELEM-EFF OF PARTAB-SING
  4858       TO DISP-4.
  4859     STRING "INIZIO "   DELIMITED BY SIZE
  4860            NUMERO-DDT  DELIMITED BY SIZE
  4861            " - TOT "   DELIMITED BY SIZE
  4862            DISP-4      DELIMITED BY SIZE
  4863       INTO RIGA-PEND-COM.
  4864     PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
  4865     PERFORM COMANDI-IGP-FINE-1 THRU EX-COMANDI-IGP-FINE-1.
  4866***
  4867     MOVE 0 TO IND-PEND.
  4868     PERFORM VARYING W-INDICE-3 FROM 1 BY 1
  4869     UNTIL W-INDICE-3 > QT-NUM-ELEM-EFF OF PARTAB-SING
  4870        PERFORM SCORRI-TB-SING
  4871           THRU EX-SCORRI-TB-SING
  4872     END-PERFORM.
  4873*
  4874*etich-vuota*
  4875     PERFORM SCRIVI-ETICH-VUOTA
  4876        THRU EX-SCRIVI-ETICH-VUOTA.
  4877 EX-SCORRI-TB.
  4878     EXIT.
  4879*
  4880*
  4881 SCORRI-TB-SING.
  4882     IF IND-PEND = 0
  4883        PERFORM COMANDI-IGP-TESTA-1
  4884           THRU EX-COMANDI-IGP-TESTA-1
  4885     END-IF.
  4886*
  4887     MOVE SPACES TO RIGA-PENDENTI.
  4888     PERFORM METTI-VALORI-PEND THRU EX-METTI-VALORI-PEND.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  86
* READVE3.cob
  4889*
  4890     MOVE 80 TO ROW-IGP.
  4891     COMPUTE COL-IGP = 80 + (IND-PEND * 1000).
  4892     MOVE ANTE-PRIMA-RIGA TO RIGA-PEND-COM.
  4893     PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
  4894*
  4895     ADD 56 TO ROW-IGP.
  4896     MOVE PRIMA-RIGA TO RIGA-PEND-COM.
  4897     PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
  4898*
  4899     ADD 56 TO ROW-IGP.
  4900     MOVE SECONDA-RIGA TO RIGA-PEND-COM.
  4901     PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
  4902*
  4903     ADD 56 TO ROW-IGP.
  4904     MOVE TERZA-RIGA TO RIGA-PEND-COM.
  4905     PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
  4906*
  4907     ADD 56 TO ROW-IGP.
  4908     MOVE QUARTA-RIGA TO RIGA-PEND-COM.
  4909     PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
  4910*
  4911     IF IND-PEND = 1 OR
  4912     W-INDICE-3 = QT-NUM-ELEM-EFF OF PARTAB-SING
  4913        PERFORM COMANDI-IGP-FINE-1
  4914           THRU EX-COMANDI-IGP-FINE-1
  4915     END-IF.
  4916*
  4917     IF IND-PEND = 0
  4918        MOVE 1 TO IND-PEND
  4919     ELSE
  4920        MOVE 0 TO IND-PEND
  4921     END-IF.
  4922 EX-SCORRI-TB-SING.
  4923     EXIT.
  4924*
  4925*etich-vuota*
  4926 SCRIVI-ETICH-VUOTA.
  4927     MOVE 80 TO ROW-IGP.
  4928     MOVE 100 TO COL-IGP.
  4929     PERFORM COMANDI-IGP-TESTA-1 THRU EX-COMANDI-IGP-TESTA-1.
  4930     MOVE SPACES TO RIGA-PEND-COM.
  4931     PERFORM SCRIVI-RECORD-IGP THRU EX-SCRIVI-RECORD-IGP.
  4932     PERFORM COMANDI-IGP-FINE-1 THRU EX-COMANDI-IGP-FINE-1.
  4933 EX-SCRIVI-ETICH-VUOTA.
  4934     EXIT.
  4935*
  4936*
  4937*
  4938 METTI-VALORI-PEND.
  4939     MOVE NUMERO-DDT TO NUMERO-P.
  4940     MOVE ELEM-TAB-SING (W-INDICE-3) TO ELEMENTO-SINGOLI.
  4941     MOVE C-MAT-SING TO CODICE-P.
  4942     MOVE D-MAT-SING TO NOME-P.
  4943*EURO*
* 4944     COPY PINEU.
  4945*                                           ********************* PINEU
  4946*IF X8=OFF                                                        PINEU
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  87
* READVE3.cob (/home/prorosa/cobol/cpy/PINEU)
  4947*CONTROL NOLIST                                                   PINEU
  4948*IF                                                               PINEU
  4949       CALL "QINEURO" USING        PAR-INEU                       PINEU
  4950       IF IE-ERRORE                                               PINEU
  4951         DISPLAY IE-MSG UPON CONSOLE                              PINEU
  4952       END-IF.                                                    PINEU
  4953*                                           ********************* PINEU
  4954*CONTROL LIST                                                     PINEU
  4955*                                                                 PINEU
  4956     MOVE IE-DIVISA-OU TO DIV-EUR.
  4957*
  4958*PRZ-PUBBL*
  4959*    COMPUTE PREZZO-P-E = PREZZO-SING / 100.
  4960     MOVE 0 TO PREZZO-P-E.
  4961*annullato*   16/09/2011 - richiesta di Benassi
  4962*    IF CONTO-DEMA
  4963*      PERFORM CERCA-PREZZO-PUBBL THRU EX-CERCA-PREZZO-PUBBL
  4964*      COMPUTE PREZZO-P-E = PREZZO-PUBBL / 100
  4965*    END-IF.
  4966*
  4967*
  4968     MOVE PREZZO-SING TO IE-IMPORTO-IN.
* 4969     COPY PDAEU.
  4970*                                           ********************* PDAEU
  4971*IF X8=OFF                                                        PDAEU
  4972*CONTROL NOLIST                                                   PDAEU
  4973*IF                                                               PDAEU
  4974       CALL "QDAEURO" USING        PAR-INEU                       PDAEU
  4975       IF IE-ERRORE                                               PDAEU
  4976         DISPLAY IE-MSG UPON CONSOLE                              PDAEU
  4977       END-IF.                                                    PDAEU
  4978*                                           ********************* PDAEU
  4979*CONTROL LIST                                                     PDAEU
  4980*                                                                 PDAEU
  4981     MOVE IE-DIVISA-OU TO DIV-LIT.
  4982*    COMPUTE PREZZO-P = IE-IMPORTO-OU / 100.
  4983     MOVE 0 TO PREZZO-P.
  4984 EX-METTI-VALORI-PEND.
  4985     EXIT.
  4986*
  4987*
  4988*PRZ-PUBBL*
  4989 CERCA-PREZZO-PUBBL.
  4990     MOVE 0 TO PREZZO-PUBBL.
  4991*
  4992     MOVE C-MAT-SING TO C-MAT-A-BARRE-RID.
  4993     MOVE 0 TO C-MAT-TRANS-RID OF C-MAT-COM.
  4994     MOVE MODELLO OF C-MAT-A-BARRE
  4995       TO MODELLO OF C-MAT-TRANSITO.
  4996     MOVE VESTIBILITA OF C-MAT-A-BARRE
  4997       TO VEST-A OF C-MAT-TRANSITO.
  4998     MOVE SOCIETA OF C-MAT-A-BARRE
  4999       TO PROGR-ART OF C-MAT-TRANSITO.
  5000     MOVE PEZZO OF C-MAT-A-BARRE
  5001       TO PEZZO-A OF C-MAT-TRANSITO.
  5002     MOVE 0
  5003       TO COLORE OF C-MAT-TRANSITO.
  5004*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  88
* READVE3.cob
  5005* NO-DATGE
  5006*     MOVE SOCIETA-MOD OF C-MAT-TRANSITO TO SOCIETA-CODICE-SOC.
  5007*     PERFORM SELEZIONA-PF-SOCIETA THRU EX-SELEZIONA-PF-SOCIETA.
  5008*     IF SQLCODE-MEM = OK
  5009**trovata societa
  5010*       MOVE C-MAT-TRANS-RID OF C-MAT-COM
  5011*         TO MODELLI-MODELLO-MAXIMA
  5012* *      MOVE SOCIETA-SOCIETA TO MODELLI-SOCIETA
  5013*       PERFORM SELEZIONA-MODELLO-NEW
  5014*          THRU EX-SELEZIONA-MODELLO-NEW
  5015*       IF SQLCODE-MEM = OK
  5016**trovato modello new
  5017*         MOVE MODELLI-MODELLO-NEW TO PREZZI-MODELLO-NEW
  5018*         MOVE MODELLI-ANNO        TO PREZZI-ANNO
  5019*         MOVE MODELLI-STAGIONE    TO PREZZI-STAGIONE
  5020*         MOVE MODELLI-SOCIETA     TO PREZZI-SOCIETA
  5021*         PERFORM SELEZIONA-PREZZO THRU EX-SELEZIONA-PREZZO
  5022*         IF SQLCODE-MEM =OK
  5023**trovato prezzo
  5024*           MOVE PREZZI-PREZZO TO PREZZO-PUBBL
  5025*         END-IF
  5026*       END-IF
  5027*     END-IF.
  5028**
  5029* NO-DATGE
  5030* NO-DATGE
  5031      PERFORM TRATTA-MODELLI-DBG THRU
  5032           EX-TRATTA-MODELLI-DBG.
  5033* NO-DATGE
  5034     IF PREZZO-PUBBL = 0 AND
  5035     SOCIETA-MOD OF C-MAT-TRANSITO = 5
  5036       PERFORM CERCA-PREZZO-PUBBL-2
  5037          THRU EX-CERCA-PREZZO-PUBBL-2
  5038     END-IF.
  5039 EX-CERCA-PREZZO-PUBBL.
  5040     EXIT.
  5041*
  5042* NO-DATGE
  5043 TRATTA-MODELLI-DBG.
  5044      MOVE SOCIETA-SIGLA(SOCIETA-MOD OF C-MAT-TRANSITO)
  5045        TO CC-SOCIETA.
  5046      MOVE C-MAT-TRANS-RID OF C-MAT-COM TO CC-C-MAT.
  5047      MOVE 1 TO CC-LISTINo.
  5048      MOVE "V" TO CC-TIPO-PREZZO.
  5049      PERFORM SELEZIONA-PREZZO-DBG THRU
  5050           EX-SELEZIONA-PREZZO-DBG.
  5051      DISPLAY "SQLCODE " SQLCODE.
  5052      PERFORM TEST-ERR THRU TEST-ERR-EX.
  5053*      IF SQLCODE = OK        DISPLAY "SQLCODE = OK" END-IF
  5054*      IF SQLCODE = NO-MEMORY DISPLAY "SQLCODE = NO_MEMORY" END-IF
  5055*      IF SQLCODE = DEADLOCK  DISPLAY "SQLCODE = DEADLOCK" END-IF
  5056*      IF SQLCODE = NOT-FOUND DISPLAY "SQLCODE = NOT-FOUND" END-IF
  5057      IF SQLCODE = OK
  5058         MOVE CC-PREZZO-DBG TO PREZZO-PUBBL.
  5059*         DISPLAY "CC-PREZZO-DBG=" CC-PREZZO-DBG
  5060*                 "  PREZZO-PUBBL=" PREZZO-PUBBL.
  5061 EX-TRATTA-MODELLI-DBG. EXIT.
  5062*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  89
* READVE3.cob
  5063*
  5064 SELEZIONA-PREZZO-DBG.
  5065     PERFORM WITH TEST AFTER
  5066     UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK
  5067       PERFORM BEGIN-RC THRU BEGIN-RC-EX
  5068       IF SQLCODE = OK
  5069**** Start SQL Preprocessor ****
  5070*        EXEC SQL
  5071*        SELECT
  5072*            P.prezzo
  5073*         INTO :CC-PREZZO-DBG
  5074*         FROM prezzi_modelli_dbg P
  5075*         JOIN anagrafica_modelli_dbg M
  5076*             ON  (M.SOCIETA = P.SOCIETA)
  5077*             AND (P.MODELLO = M.MODELLO)
  5078*         JOIN anagrafica_modelli_barcode_negozio_dbg B
  5079*             ON  (B.SOCIETA = P.SOCIETA)
  5080*             AND (P.MODELLO = B.MODELLO)
  5081*         WHERE
  5082*             P.modello = :CC-C-MAT
  5083*             AND M.societa = :CC-SOCIETA
  5084*             AND P.f_listino_rif = :CC-LISTINO
  5085*             AND P.tipo_prezzo = :CC-TIPO-PREZZO
  5086*        END-EXEC
  5087**** Start Inserted Statements ****
  5088     MOVE CC-C-MAT TO SQLI-001-000
  5089     MOVE CC-SOCIETA TO SQLI-001-001
  5090     MOVE CC-LISTINO TO SQLI-001-002
  5091     MOVE CC-TIPO-PREZZO TO SQLI-001-003
  5092     CALL "sqlx_select" USING
  5093          SQLX-PROG,
  5094          SQL-PARAM-001-X,
  5095          SQL-SEL-001-X,
  5096          SQLI-TIPO-001-X,
  5097          SQLO-TIPO-001-X,
  5098          SQLI-REC-001,
  5099          SQLO-REC-001,
  5100          SQLCA
  5101     IF SQLCODE = 0
  5102       MOVE SQLO-001-000 TO CC-PREZZO-DBG
  5103     END-IF
  5104**** End SQL Processor   ****
  5105       END-IF
  5106       MOVE SQLCODE TO SQLCODE-MEM
  5107       PERFORM S-S-COMMIT THRU S-S-COMMIT-EX
  5108     END-PERFORM.
  5109 EX-SELEZIONA-PREZZO-DBG. EXIT.
  5110* NO-DATGE
  5111*
  5112*PRZ-PUBBL*
  5113* NO-DATGE
  5114*SELEZIONA-PF-SOCIETA.
  5115*    MOVE SPACES TO SOCIETA-SOCIETA.
  5116*
  5117*    PERFORM WITH TEST AFTER
  5118*    UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK
  5119*      PERFORM BEGIN-RC THRU BEGIN-RC-EX
  5120*      IF SQLCODE = OK
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  90
* READVE3.cob
  5121*        EXEC SQL
  5122*          SELECT SOCIETA
  5123*            INTO :SOCIETA-SOCIETA
  5124*            FROM PF.SOCIETA
  5125*           WHERE COD_X_BARCODE = :SOCIETA-CODICE-SOC
  5126*        END-EXEC
  5127*      END-IF
  5128*      MOVE SQLCODE TO SQLCODE-MEM
  5129*      PERFORM S-S-COMMIT THRU S-S-COMMIT-EX
  5130*    END-PERFORM.
  5131*EX-SELEZIONA-PF-SOCIETA.
  5132*    EXIT.
  5133* NO-DATGE
  5134*
  5135*
  5136*PRZ-PUBBL*
  5137* NO-DATGE
  5138* SELEZIONA-MODELLO-NEW.
  5139*     MOVE 0 TO FLAG-CURSORE.
  5140**
  5141*     MOVE SPACES TO MODELLI-MODELLO-NEW.
  5142*     MOVE '000'  TO MODELLI-ESTENSIONE.
  5143**
  5144*     PERFORM WITH TEST AFTER
  5145*     UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK
  5146*       PERFORM BEGIN-RC THRU BEGIN-RC-EX
  5147*       PERFORM DECLARE-CURS-MOD THRU DECLARE-CURS-MOD-EX
  5148*       PERFORM OPEN-CURS-MOD THRU OPEN-CURS-MOD-EX
  5149*       PERFORM FETCH-SINGOLA-CURS-MOD
  5150*          THRU FETCH-SINGOLA-CURS-MOD-EX
  5151*       MOVE SQLCODE TO SQLCODE-MEM
  5152**      IF NOT STOP-CURSORE
  5153**trovato modello new
  5154**        CONTINUE
  5155**      END-IF
  5156*       PERFORM CLOSE-CURS-MOD THRU CLOSE-CURS-MOD-EX
  5157*       PERFORM S-S-COMMIT THRU S-S-COMMIT-EX
  5158*     END-PERFORM.
  5159* EX-SELEZIONA-MODELLO-NEW.
  5160*     EXIT.
  5161**
  5162**
  5163**PRZ-PUBBL*
  5164* SELEZIONA-PREZZO.
  5165*     MOVE 0    TO PREZZI-PREZZO.
  5166*     MOVE '01' TO PREZZI-LISTINO.
  5167*     MOVE 'V'  TO PREZZI-TIPO-PREZZO.
  5168**
  5169*     PERFORM WITH TEST AFTER
  5170*     UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK
  5171*       PERFORM BEGIN-RC THRU BEGIN-RC-EX
  5172*       IF SQLCODE = OK
  5173*         EXEC SQL
  5174*           SELECT PREZZO
  5175*             INTO :PREZZI-PREZZO
  5176*             FROM ANAMAT.PREZZI
  5177*            WHERE LISTINO     = :PREZZI-LISTINO AND
  5178*                  TIPO_PREZZO = :PREZZI-TIPO-PREZZO AND
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  91
* READVE3.cob
  5179*                  MODELLO_NEW = :PREZZI-MODELLO-NEW AND
  5180*                  ANNO        = :PREZZI-ANNO AND
  5181*                  STAG        = :PREZZI-STAGIONE AND
  5182*                  SOCIETA     = :PREZZI-SOCIETA
  5183*         END-EXEC
  5184*       END-IF
  5185*       MOVE SQLCODE TO SQLCODE-MEM
  5186*       PERFORM S-S-COMMIT THRU S-S-COMMIT-EX
  5187*     END-PERFORM.
  5188* EX-SELEZIONA-PREZZO.
  5189*     EXIT.
  5190* NO-DATGE
  5191*
  5192*
  5193*PRZ-PUBBL*
  5194 CERCA-PREZZO-PUBBL-2.
  5195     MOVE 0 TO PREZZO-PUBBL.
  5196*
  5197     MOVE "PREZZI;" TO W-NOME-DATA-SET.
  5198     MOVE "C-MAT;"  TO W-NOME-CAMPO.
  5199     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.
  5200*
  5201     PERFORM TTDBFIND THRU EX-TTDBFIND.
  5202     IF W-OK-IMAGE
  5203        MOVE 5 TO W-MODO
  5204        PERFORM TTDBGET-P-PUB THRU EX-TTDBGET-P-PUB
  5205        PERFORM TTDBGET-P-PUB THRU EX-TTDBGET-P-PUB
  5206          UNTIL NOT W-OK-IMAGE OR
  5207                (MERCATO OF REC-PREZZI-PUB = 2)
  5208     END-IF.
  5209*
  5210     IF W-OK-IMAGE
  5211        MOVE PREZZO-VENDITA OF REC-PREZZI-PUB(1)
  5212          TO PREZZO-PUBBL
  5213     END-IF.
  5214 EX-CERCA-PREZZO-PUBBL-2.
  5215     EXIT.
  5216*
  5217*
  5218*
  5219*
  5220 SCORRI-TAB-SING.
  5221     MOVE ELEM-TAB-SING (W-INDICE-3) TO ELEMENTO-SINGOLI.
  5222*    DISPLAY CONT-SING SPACE C-MAT-SING SPACE D-MAT-SING.
  5223*UNICODDT*
  5224     IF XD = "S"
  5225         DISPLAY "SCORRI-TAB-SING"
  5226         DISPLAY C-MAT-SING SPACE CONT-SING SPACE D-MAT-SING.
  5227*UNICODDT*
  5228     MOVE C-MAT-SING TO C-MAT-A-BARRE-RID.
  5229     MOVE 0 TO C-MAT-TRANS-RID.
  5230     MOVE MODELLO OF C-MAT-A-BARRE TO MODELLO OF
  5231                       C-MAT-TRANSITO
  5232     MOVE VESTIBILITA OF C-MAT-A-BARRE TO
  5233            VEST-A OF C-MAT-TRANSITO
  5234     MOVE SOCIETA OF C-MAT-A-BARRE TO
  5235                    PROGR-ART OF C-MAT-TRANSITO
  5236     MOVE PEZZO OF C-MAT-A-BARRE TO PEZZO-A OF
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  92
* READVE3.cob
  5237                       C-MAT-TRANSITO
  5238     COMPUTE C-MAT-S = C-MAT-SING / 10.
  5239     MOVE NEG-IN TO MAG-S
  5240     MOVE D-MAT-SING TO NOME-S
  5241     MOVE SPACES TO NOME-F-S
  5242                    COL-F-S
  5243                    SETTORE-S
  5244     MOVE 0 TO PREZZO-S.
  5245*MOVSKU*
  5246     MOVE SKU-SING TO BARUNI-S.
  5247     MOVE T-TAB TO T-1 T-2 T-3 T-4 T-5 T-6 T-7 T-8 T-9
  5248*MOVSKU*
  5249     PERFORM CERCA-PREZZO-V THRU EX-CERCA-PREZZO-V.
  5250     COMPUTE PREZZO-S = PREZZO-MEM / 100.
  5251     MOVE TAGLIA OF C-MAT-A-BARRE TO TAGLIA-S
  5252     MOVE TAGLIA OF C-MAT-A-BARRE TO NTG-IN
  5253     MOVE FUNCTION idxtg(NTG-IN)
  5254                        TO W-INDICE-4
  5255     PERFORM ESPLODI-TG THRU EX-ESPLODI-TG.
  5256 EX-SCORRI-TAB-SING.
  5257     EXIT.
  5258*
  5259*MOVSKU
  5260 INTESTA-FILE-BC.
  5261     MOVE SPACE TO REC-BC
  5262     STRING "C-MAT" T-TAB "TAGLIA" T-TAB "MAG" T-TAB
  5263         "SETTORE" T-TAB "NOME" T-TAB "PREZZO" T-TAB
  5264         "NOME-F" T-TAB "COL-F" T-TAB "TG-OUT" T-TAB
  5265         "BARUNI"
  5266        DELIMITED BY SIZE
  5267        INTO REC-BC.
  5268     WRITE REC-BC.
  5269 EX-INTESTA-FILE-BC.
  5270     EXIT.
  5271*MOVSKU
  5272*
  5273 CERCA-PREZZO-V.
  5274     MOVE "PREZZI;" TO W-NOME-DATA-SET.
  5275     MOVE "C-MAT;" TO W-NOME-CAMPO.
  5276     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.
  5277     PERFORM TTDBFIND THRU EX-TTDBFIND.
  5278     IF W-OK-IMAGE
  5279        MOVE 5 TO W-MODO
  5280        PERFORM TTDBGET-P THRU EX-TTDBGET-P
  5281        PERFORM TTDBGET-P THRU EX-TTDBGET-P
  5282              UNTIL NOT W-OK-IMAGE OR
  5283                (MERCATO OF REC-PREZZI =
  5284                     LISTINO-MEM).
  5285     IF W-OK-IMAGE
  5286*BUDA*
  5287        MOVE PREZZO-VENDITA OF REC-PREZZI(1)
  5288            TO PREZZO-MEM.
  5289 EX-CERCA-PREZZO-V.
  5290     EXIT.
  5291*
  5292*
  5293 SCRIVI-RECORD.
  5294     IF TAB-ART (W-INDICE-3) = 0
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  93
* READVE3.cob
  5295            OR QTA-TAGLIE-TAB (W-INDICE-3) = LOW-VALUE
  5296        GO TO EX-SCRIVI-RECORD.
  5297*
  5298     COMPUTE C-MAT-A-BARRE-RID = TAB-ART (W-INDICE-3) * 10.
  5299     MOVE ART-TAB-LETTI(W-INDICE-3)
  5300         TO ART-ELEM-LETTI.
  5301     MOVE 0 TO C-MAT-TRANS-RID.
  5302     MOVE MODELLO OF C-MAT-A-BARRE TO MODELLO OF
  5303                         C-MAT-TRANSITO.
  5304     MOVE VESTIBILITA OF C-MAT-A-BARRE TO
  5305                  VEST-A OF C-MAT-TRANSITO.
  5306     MOVE SOCIETA OF C-MAT-A-BARRE TO
  5307                      PROGR-ART OF C-MAT-TRANSITO.
  5308     MOVE PEZZO OF C-MAT-A-BARRE TO PEZZO-A OF
  5309                         C-MAT-TRANSITO.
  5310     MOVE VARIANTE-COL OF C-MAT-A-BARRE TO COLORE OF
  5311                         C-MAT-TRANSITO.
  5312*BUDA*
  5313*          IF PRIORITA OF REC-INDIRIZZI = 4
  5314*             COMPUTE C-MAT-S = C-MAT-A-BARRE-RID / 10
  5315*             MOVE NEG-IN TO MAG-S
  5316*             MOVE D-MAT-ELEM TO NOME-S
  5317*             MOVE SPACES TO NOME-F-S
  5318*                            COL-F-S
  5319*                            SETTORE-S
  5320*BUDA*
  5321*             MOVE 0 TO PREZZO-S
  5322*             PERFORM CERCA-PREZZO THRU EX-CERCA-PREZZO
  5323*             COMPUTE PREZZO-S = PREZZO-ELEM / 100
  5324*             PERFORM ESPLODI-8-TG THRU EX-ESPLODI-8-TG
  5325*                     VARYING W-INDICE-4 FROM 1 BY 1
  5326*                        UNTIL W-INDICE-4 > NTG-NTG
  5327*          END-IF
  5328      PERFORM INVERTI-QTA THRU EX-INVERTI-QTA
  5329            VARYING W-INDICE-5 FROM 1 BY 1
  5330            UNTIL W-INDICE-5 > NTG-NTG.
  5331     PERFORM PREPARA-MOVMAG THRU EX-PREPARA-MOVMAG.
  5332     PERFORM CREA-MOVMAG-P-3
  5333           THRU EX-CREA-MOVMAG-P-3.
  5334     PERFORM AGGIORNA-SITPF-P-3
  5335           THRU EX-AGGIORNA-SITPF-P-3.
  5336 EX-SCRIVI-RECORD.
  5337     EXIT.
  5338*
  5339*
  5340 INVERTI-QTA.
  5341     COMPUTE QTA-TAGLIA-NEG(W-INDICE-5) =
  5342         QTA-TAGLIA-ELEM(W-INDICE-5) * -1.
  5343 EX-INVERTI-QTA.
  5344     EXIT.
  5345*
  5346*
  5347 CERCA-PREZZO.
  5348     MOVE "ANAMAT;" TO W-NOME-DATA-SET.
  5349     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.
  5350     MOVE 7 TO W-MODO
  5351     PERFORM TTDBGET THRU EX-TTDBGET
  5352     IF NOT W-OK-IMAGE
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  94
* READVE3.cob
  5353          DISPLAY "Inesist. col 0  " C-MAT-A-BARRE-RID
  5354       GO TO EX-CERCA-PREZZO.
  5355*
  5356*PRODI*
  5357     MOVE COSTO OF REC-ANAMAT TO PREZZO-ANAMAT.
  5358*
  5359     IF MAG-STOCK
  5360       PERFORM CHIAMA-DTVALSTK THRU EX-CHIAMA-DTVALSTK
  5361     ELSE
  5362       PERFORM CERCA-PREZZIA THRU EX-CERCA-PREZZIA.
  5363*
  5364 EX-CERCA-PREZZO.
  5365     EXIT.
  5366*
  5367*
  5368*PRODI*
  5369 CHIAMA-DTVALSTK.
  5370     MOVE C-MAT-TRANS-RID TO STK-C-MAT.
  5371     MOVE COLLEZIONE OF REC-ANAMAT TO STK-COLL.
  5372     MOVE STAGIONE OF REC-ANAMAT TO STK-STAGIONE.
  5373     MOVE COSTO OF REC-ANAMAT TO STK-PRZ-LORDO.
  5374*MAG6/7*
  5375*    IF SI-DT-ESTERO
  5376*       MOVE NOME-IN-B TO STK-NOME
  5377*      ELSE
  5378*         MOVE NOME-IN TO STK-NOME.
  5379     IF MAG-FALLATO
  5380       MOVE "STF" TO NOME-IN-3
  5381     ELSE
  5382*Mag3_V/F*
  5383       IF F-V-INPUT = "F"
  5384          MOVE "STF" TO NOME-IN-3
  5385       ELSE
  5386*
  5387       MOVE "STV" TO NOME-IN-3.
  5388     MOVE CONTO-IN-R TO NOME-IN-5.
  5389     MOVE NOME-IN-35 TO STK-NOME.
  5390*
  5391     CALL "DTVALSTK" USING STK-NOME
  5392                          STK-C-MAT
  5393                          STK-STAGIONE
  5394                          STK-COLL
  5395                          STK-SCO
  5396                          STK-PRZ-SCO
  5397                          STK-PRZ-LORDO STK-MSG
  5398                          STK-PRIMA-VOLTA
  5399                          STK-CAMBIO
  5400*EURO1*
  5401                          W-COMMON.
  5402*
  5403*IF X5=ON
  5404*    DISPLAY "STK-NOME        " STK-NOME  .
  5405*    DISPLAY "STK-C-MAT       " STK-C-MAT .
  5406*    DISPLAY "STK-STAGIONE    " STK-STAGIONE .
  5407*    DISPLAY "STK-COLL        " STK-COLL .
  5408*    DISPLAY "STK-SCO         " STK-SCO
  5409*    DISPLAY "STK-PRZ-SCO     " STK-PRZ-SCO
  5410*    DISPLAY "STK-PRZ-LORDO   " STK-PRZ-LORDO .
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  95
* READVE3.cob
  5411*    DISPLAY "STK-PRIMA-VOLTA " STK-PRIMA-VOLTA .
  5412*    DISPLAY "STK-CAMBIO      " STK-CAMBIO.
  5413*     IF STK-MSG NOT = SPACE
  5414*       DISPLAY STK-MSG.
  5415*IF
  5416*     toglie i decimali (Farini 12/05/99)
  5417*NOPRZ*
  5418*EURO*
  5419*    COMPUTE PREZZO-ANAMAT = (COSTO OF REC-ANAMAT / 100) * 100
  5420*    COMPUTE STK-PRZ-SCO = (STK-PRZ-SCO / 100) * 100.
  5421*PRODI*
  5422*    MOVE COSTO OF REC-ANAMAT TO PREZZO-ANAMAT.
  5423     MOVE STK-PRZ-SCO TO PREZZO-MEM.
  5424     MOVE STK-CAMBIO TO CAMBIO-MEM.
  5425 EX-CHIAMA-DTVALSTK.
  5426     EXIT.
  5427*
  5428*
  5429 CERCA-PREZZIA.
  5430     MOVE "PREZZIA;" TO W-NOME-DATA-SET.
  5431     MOVE "C-MAT;" TO W-NOME-CAMPO.
  5432     COMPUTE W-VALORE-CAMPO = C-MAT-TRANS-RID / 1000 * 1000.
  5433     PERFORM TTDBFIND THRU EX-TTDBFIND.
  5434     IF W-OK-IMAGE
  5435        MOVE 5 TO W-MODO
  5436        PERFORM TTDBGET-P THRU EX-TTDBGET-P
  5437        PERFORM TTDBGET-P THRU EX-TTDBGET-P
  5438              UNTIL NOT W-OK-IMAGE OR
  5439                (MERCATO OF REC-PREZZI =
  5440                     LISTINO-MEM).
  5441     IF W-OK-IMAGE
  5442*BUDA*
  5443        MOVE PREZZO-VENDITA OF REC-PREZZI(1)
  5444            TO PREZZO-MEM.
  5445     MOVE 0 TO CAMBIO-MEM.
  5446 EX-CERCA-PREZZIA.
  5447     EXIT.
  5448*
  5449*
  5450 ESPLODI-8-TG.
  5451     IF QTA-TAGLIA-ELEM (W-INDICE-4) NOT = 0
  5452        PERFORM ESPLODI-TG THRU EX-ESPLODI-TG
  5453              VARYING W-INDICE-5 FROM 1 BY 1
  5454                  UNTIL W-INDICE-5 >
  5455                      QTA-TAGLIA-ELEM (W-INDICE-4).
  5456 EX-ESPLODI-8-TG.
  5457     EXIT.
  5458*
  5459*
  5460 ESPLODI-TG.
  5461     MOVE W-INDICE-4 TO TAGLIA-S.
  5462     COMPUTE TG-CAL = PRIMA-TG-SING + (W-INDICE-4 * 2 - 2).
  5463     PERFORM T-TG THRU EX-T-TG.
  5464     WRITE REC-BC.
  5465     IF CLASSE OF C-MAT-TRANSITO = 52
  5466        WRITE REC-BC.
  5467 EX-ESPLODI-TG.
  5468     EXIT.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  96
* READVE3.cob
  5469*
  5470*
  5471 T-TG.
  5472     IF TG-CAL < 79
  5473        MOVE TG-CAL TO TG-OUT-S.
  5474     IF TG-CAL = 80
  5475        MOVE "XS" TO TG-OUT-S.
  5476     IF TG-CAL = 82
  5477        MOVE " S" TO TG-OUT-S.
  5478     IF TG-CAL = 84
  5479        MOVE " M" TO TG-OUT-S.
  5480     IF TG-CAL = 86
  5481        MOVE " L" TO TG-OUT-S.
  5482     IF TG-CAL = 88
  5483        MOVE "XL" TO TG-OUT-S.
  5484 EX-T-TG.
  5485     EXIT.
  5486*
  5487*
  5488*PAGE
  5489*
  5490 TTUNLOCK.
* 5491     COPY PDBUNLOC.
  5492*                                           ********************* PDBUNLOC
  5493*IF X8=OFF                                                        PDBUNLOC
  5494*CONTROL NOLIST                                                   PDBUNLOC
  5495*IF                                                               PDBUNLOC
  5496     MOVE 1 TO W-MODO.                                            PDBUNLOC
  5497     CALL "DBUNLOCK" USING W-NOME-DATA-BASE-1                     PDBUNLOC
  5498                           W-NOME-DATA-SET                        PDBUNLOC
  5499                           W-MODO                                 PDBUNLOC
  5500                           W-CA-IMAGE.                            PDBUNLOC
  5501     IF W-ERRORI-TRAGICI                                          PDBUNLOC
  5502        MOVE 8 TO W-INDICE-8                                      PDBUNLOC
  5503        CALL "QDBERROR" USING W-COMMON.                           PDBUNLOC
  5504 EX-TTUNLOCK.                                                     PDBUNLOC
  5505     EXIT.                                                        PDBUNLOC
  5506*                                                                 PDBUNLOC
  5507*CONTROL LIST                                                     PDBUNLOC
  5508*                                                                 PDBUNLOC
  5509*                                                                 PDBUNLOC
  5510*
  5511*
  5512 TTLOCK-T.
*                 COPY PDBLOCK REPLACING
* 5513                W-NOME-DATA-SET BY TAB-LOCK
* 5514                EX-TTLOCK BY EX-TTLOCK-T.
  5515*                                           *********************
  5516*IF X8=OFF
  5517*CONTROL NOLIST
  5518*IF
  5519     CALL "DBLOCK" USING W-NOME-DATA-BASE-1
  5520                         TAB-LOCK
  5521                         W-MODO
  5522                         W-CA-IMAGE.
  5523     IF W-ERRORI-TRAGICI
  5524        MOVE 5 TO W-INDICE-8
  5525        CALL "QDBERROR" USING W-COMMON.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  97
* READVE3.cob (/home/prorosa/cobol/cpy/PDBLOCK)
  5526 EX-TTLOCK-T.
  5527     EXIT.
  5528*
  5529*CONTROL LIST
  5530*
  5531*
  5532*
  5533 TTLOCK.
* 5534     COPY PDBLOCK.
  5535*                                           ********************* PDBLOCK
  5536*IF X8=OFF                                                        PDBLOCK
  5537*CONTROL NOLIST                                                   PDBLOCK
  5538*IF                                                               PDBLOCK
  5539     CALL "DBLOCK" USING W-NOME-DATA-BASE-1                       PDBLOCK
  5540                         W-NOME-DATA-SET                          PDBLOCK
  5541                         W-MODO                                   PDBLOCK
  5542                         W-CA-IMAGE.                              PDBLOCK
  5543     IF W-ERRORI-TRAGICI                                          PDBLOCK
  5544        MOVE 5 TO W-INDICE-8                                      PDBLOCK
  5545        CALL "QDBERROR" USING W-COMMON.                           PDBLOCK
  5546 EX-TTLOCK.                                                       PDBLOCK
  5547     EXIT.                                                        PDBLOCK
  5548*                                                                 PDBLOCK
  5549*CONTROL LIST                                                     PDBLOCK
  5550*                                                                 PDBLOCK
  5551*                                                                 PDBLOCK
  5552*
  5553*
  5554 TTDBPUT.
* 5555     COPY PDBPUT.
  5556*                                           ********************* PDBPUT
  5557*IF X8=OFF                                                        PDBPUT
  5558*CONTROL NOLIST                                                   PDBPUT
  5559*IF                                                               PDBPUT
  5560     MOVE 1 TO W-MODO.                                            PDBPUT
  5561     CALL "DBPUT" USING W-NOME-DATA-BASE-1                        PDBPUT
  5562                        W-NOME-DATA-SET                           PDBPUT
  5563                        W-MODO                                    PDBPUT
  5564                        W-CA-IMAGE                                PDBPUT
  5565                        W-TUTTO-RECORD                            PDBPUT
  5566                        AREA-REC-SET.                             PDBPUT
  5567     IF W-ERRORI-TRAGICI OR W-DATA-SET-PIENO OR                   PDBPUT
  5568        W-CATENA-PIENA OR W-MASTER-PIENO                          PDBPUT
  5569        MOVE 7 TO W-INDICE-8                                      PDBPUT
  5570        CALL "QDBERROR" USING W-COMMON.                           PDBPUT
  5571 EX-TTDBPUT.                                                      PDBPUT
  5572     EXIT.                                                        PDBPUT
  5573*                                                                 PDBPUT
  5574*CONTROL LIST                                                     PDBPUT
  5575*                                                                 PDBPUT
  5576*                                                                 PDBPUT
  5577*
  5578*
  5579 TTUPDATE.
* 5580     COPY PDBUPDAT.
  5581*                                           ********************* PDBUPDAT
  5582*IF X8=OFF                                                        PDBUPDAT
  5583*CONTROL NOLIST                                                   PDBUPDAT
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  98
* READVE3.cob (/home/prorosa/cobol/cpy/PDBUPDAT)
  5584*IF                                                               PDBUPDAT
  5585     MOVE 1 TO W-MODO.                                            PDBUPDAT
  5586     CALL "DBUPDATE" USING W-NOME-DATA-BASE-1                     PDBUPDAT
  5587                           W-NOME-DATA-SET                        PDBUPDAT
  5588                           W-MODO                                 PDBUPDAT
  5589                           W-CA-IMAGE                             PDBUPDAT
  5590                           W-TUTTO-RECORD                         PDBUPDAT
  5591                           AREA-REC-SET.                          PDBUPDAT
  5592     IF W-ERRORI-TRAGICI                                          PDBUPDAT
  5593        MOVE 9 TO W-INDICE-8                                      PDBUPDAT
  5594        CALL "QDBERROR" USING W-COMMON.                           PDBUPDAT
  5595 EX-TTUPDATE.                                                     PDBUPDAT
  5596     EXIT.                                                        PDBUPDAT
  5597*                                                                 PDBUPDAT
  5598*CONTROL LIST                                                     PDBUPDAT
  5599*                                                                 PDBUPDAT
  5600*                                                                 PDBUPDAT
  5601*
  5602*
  5603 TTDBGET-P.
*                   COPY PDBGET REPLACING
* 5604           AREA-REC-SET BY REC-PREZZI
* 5605           EX-TTDBGET BY EX-TTDBGET-P.
  5606*                                           *********************
  5607*IF X8=OFF
  5608*CONTROL NOLIST
  5609*IF
  5610     CALL "DBGET" USING W-NOME-DATA-BASE-1
  5611                        W-NOME-DATA-SET
  5612                        W-MODO
  5613                        W-CA-IMAGE
  5614                        W-TUTTO-RECORD
  5615                        REC-PREZZI
  5616                        W-VALORE-CAMPO.
  5617     IF W-ERRORI-TRAGICI
  5618        MOVE 4 TO W-INDICE-8
  5619        CALL "QDBERROR" USING W-COMMON.
  5620 EX-TTDBGET-P.
  5621     EXIT.
  5622*
  5623*
  5624*CONTROL LIST
  5625*
  5626*
  5627*
  5628*PRZ-PUBBL*
  5629 TTDBGET-P-PUB.
* 5630     COPY PDBGET REPLACING AREA-REC-SET
* 5631                        BY REC-PREZZI-PUB
* 5632                           EX-TTDBGET
* 5633                        BY EX-TTDBGET-P-PUB.
  5634*                                           *********************
  5635*IF X8=OFF
  5636*CONTROL NOLIST
  5637*IF
  5638     CALL "DBGET" USING W-NOME-DATA-BASE-1
  5639                        W-NOME-DATA-SET
  5640                        W-MODO
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page  99
* READVE3.cob (/home/prorosa/cobol/cpy/PDBGET)
  5641                        W-CA-IMAGE
  5642                        W-TUTTO-RECORD
  5643                        REC-PREZZI-PUB
  5644                        W-VALORE-CAMPO.
  5645     IF W-ERRORI-TRAGICI
  5646        MOVE 4 TO W-INDICE-8
  5647        CALL "QDBERROR" USING W-COMMON.
  5648 EX-TTDBGET-P-PUB.
  5649     EXIT.
  5650*
  5651*
  5652*CONTROL LIST
  5653*
  5654*
  5655*
  5656*PAGE
  5657*
  5658*
  5659 DISP-C-MAT.
  5660     MOVE SPACES TO RIGA-DISP.
  5661     MOVE 1 TO IND-4.
  5662     PERFORM METTI-4 THRU EX-METTI-4
  5663            UNTIL IND-4 > 3 OR
  5664              W-INDICE-3 > QT-NUM-ELEM-EFF OF PARTAB-ART.
  5665     DISPLAY RIGA-DISP.
  5666 EX-DISP-C-MAT.
  5667     EXIT.
  5668*
  5669*
  5670 METTI-4.
  5671     IF TAB-ART (W-INDICE-3) = 0
  5672            OR QTA-TAGLIE-TAB (W-INDICE-3) = LOW-VALUE
  5673        ADD 1 TO W-INDICE-3
  5674        GO TO EX-METTI-4.
  5675     MOVE TAB-ART (W-INDICE-3) TO DISP-ART (IND-4).
  5676     MOVE "(" TO PARE1(IND-4).
  5677     MOVE ")" TO PARE2(IND-4).
  5678     MOVE ART-TAB-LETTI(W-INDICE-3) TO ART-ELEM-LETTI.
  5679     MOVE 0 TO COM-QTA-DISP.
  5680     PERFORM ACCUMULA-QTA THRU EX-ACCUMULA-QTA
  5681         VARYING W-INDICE-7 FROM 1 BY 1
  5682         UNTIL W-INDICE-7 > NTG-NTG.
  5683     MOVE COM-QTA-DISP TO QTA-DISP(IND-4).
  5684     ADD COM-QTA-DISP TO TOT-CAPI-LETTI-1.
  5685     ADD 1 TO W-INDICE-3 IND-4.
  5686 EX-METTI-4.
  5687     EXIT.
  5688*
  5689*
  5690 ACCUMULA-QTA.
  5691     ADD QTA-TAGLIA-ELEM(W-INDICE-7) TO COM-QTA-DISP.
  5692 EX-ACCUMULA-QTA.
  5693     EXIT.
  5694*
  5695*
  5696*
  5697*
  5698 AGG-DPARAM.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 100
* READVE3.cob
  5699     MOVE "DPARAM;" TO W-NOME-DATA-SET.
  5700      ADD 1 TO NUM-BOLLA-TAGLIO-FODERE.
  5701      MOVE REC-PARAM-RID TO AREA-REC-SET.
  5702      PERFORM TTUPDATE THRU EX-TTUPDATE.
  5703 EX-AGG-DPARAM.
  5704      EXIT.
  5705*
  5706*
  5707 AGG-DPARAM-FITTIZI.
  5708     MOVE "DPARAM;" TO W-NOME-DATA-SET.
  5709      ADD 1 TO PAR-FITTIZIO-1.
  5710      MOVE REC-PARAM-FITTIZ-R TO AREA-REC-SET.
  5711      PERFORM TTUPDATE THRU EX-TTUPDATE.
  5712 EX-AGG-DPARAM-FITTIZI.
  5713      EXIT.
  5714*
  5715*
  5716*
  5717 GET-LOCALITA.
  5718     MOVE SPACES TO LOCALITA-PART-STR.
  5719*
  5720     PERFORM VARYING IND-LOC FROM 1 BY 1
  5721     UNTIL (IND-LOC > MAX-LOC) OR (LOCALITA-PART-STR NOT = SPACES)
  5722        IF COD-LOC(IND-LOC) = MAG-INPUT-R
  5723           MOVE DESC-LOC(IND-LOC) TO LOCALITA-PART-STR
  5724        END-IF
  5725     END-PERFORM.
  5726*    DISPLAY '>'LOCALITA-PART-STR'<'.
  5727 EX-GET-LOCALITA.
  5728     EXIT.
  5729*
  5730*
  5731 CHIAMA-PRINTDDT.
  5732*MAG6/7*
  5733*    MOVE 7 TO MAGAZZINO-DDT.
  5734     MOVE MAG-INPUT-R TO MAGAZZINO-DDT.
  5735     MOVE 1 TO TIPO-DOC-DDT.
  5736     MOVE 11 TO TIPO-MOVIMENTO-DDT.
  5737     MOVE "VESD" TO CAUSALE-DDT.
  5738     MOVE 1 TO TIPO-STAMPA-DDT.
  5739*FIFRA*
  5740*    MOVE CONTO-IN-R TO CLIENTE-DDT.
  5741     IF CONTO-FATTURA-MEM NOT = 0
  5742       MOVE CONTO-FATTURA-MEM TO CLIENTE-DDT
  5743     ELSE
  5744       MOVE CONTO-IN-R TO CLIENTE-DDT.
  5745*
  5746     PERFORM GET-LOCALITA THRU EX-GET-LOCALITA.
  5747     MOVE LOCALITA-PART-STR TO LOC-PART-DDT.
  5748     MOVE SPACES TO TIPO-DATA-SET-DDT
  5749                    D-CONTO-VET
  5750                    NOTE-DDT (1)
  5751                    NOTE-DDT (2).
  5752**FSTOCK*
  5753*       IF W-SIGLA-UTENTE = "RESIDUO" OR = "PROROSA"
  5754*         MOVE "Merce distrib/prod da Diffusione Tessile srl"
  5755*             TO NOTE-DDT(1)
  5756*         MOVE
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 101
* READVE3.cob
  5757*              " unipersonale; via Santi,8 42025Cavriago(RE)"
  5758*             TO NOTE-DDT(2)
  5759*        END-IF.
  5760**
  5761     MOVE 0 TO IMPORTO-X-PL.
  5762*BUDA*
  5763     MOVE "N"  TO FILE-FAT-DDT.
  5764*TRAVMAG*
  5765     IF W-SIGLA-UTENTE = "MAXMAX"
  5766       MOVE
  5767              "MITTENTE DEPOSITARIO: MAXIMA SPA C/O DEPOSITO DIFFU
  5768-       "SIONE TESSILE"
  5769              TO RIGA-1-DDT
  5770       MOVE
  5771              "SRL - VIA SANTI,8 42025 CAVRIAGO (RE) (SCARICO CONT
  5772-       "O DEPOSITO)"
  5773              TO RIGA-2-DDT
  5774     ELSE
  5775**FSTOCK*      alternativa
  5776*       IF W-SIGLA-UTENTE = "RESIDUO" OR = "PROROSA"
  5777*         MOVE "Merce distrib e/o prodotta da Diffusione Tessile s
  5778*-          "l unipersonale"
  5779*             TO RIGA-1-DDT
  5780*         MOVE "via Santi, 8 42025 Cavriago (RE)"
  5781*             TO RIGA-2-DDT
  5782*
  5783*        ELSE
  5784**
  5785       MOVE SPACE TO RIGA-1-DDT RIGA-2-DDT.
  5786*
  5787*PRINTDD6*
  5788*    CALL "PRINTDD3" USING W-COMMON
  5789     CANCEL "PRINTDDF"
  5790     CALL "PRINTDDF" USING W-COMMON
  5791                           SQLCA
  5792                           CAMPI-ANAGRAFICI
  5793                           CAMPI-COMODO
  5794                           TIPO-DATA-SET-DDT
  5795                           IMPORTO-X-PL
  5796*BUDA*
  5797                           FILE-FAT-DDT
  5798*TRAVMAG*
  5799                           RIGA-1-DDT
  5800                           RIGA-2-DDT.
  5801*
  5802 EX-CHIAMA-PRINTDDT.
  5803     EXIT.
  5804*
  5805*
  5806 PREPARA-MOVMAG.
  5807     MOVE LOW-VALUE TO MOVMAG.
  5808     MOVE SPACES TO VAL-REC OF MOVMAG.
  5809     MOVE 1 TO NUMERO-RIGA OF MOVMAG.
  5810     MOVE W-FORMATO-INTERNO TO Q-DATA-I.
  5811     MOVE 2 TO Q-FUNZIONE OF PARGEN.
  5812     CANCEL "QDATAS"
  5813      CALL "QDATAS" USING PARGEN
  5814                         Q-DATA-E Q-DATA-I
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 102
* READVE3.cob
  5815                         Q-SETTIMANA.
  5816     MOVE Q-SETTIMANA
  5817       TO SETTIMANA OF MOVMAG.
  5818     MOVE RIF-BOLLA-DDT TO RIF-INTERNO OF MOVMAG.
  5819     MOVE 0 TO RIF-BOLLA-FORN OF MOVMAG
  5820               RIF-ORDINE OF MOVMAG
  5821               MOD-IMPUTAZ OF MOVMAG
  5822               QUANTITA OF MOVMAG.
  5823*VACO*
  5824*     MOVE 0 TO PREZZO OF MOVMAG.
  5825*VACO*
  5826     MOVE SPACES TO DIVISA OF MOVMAG
  5827                    VAL-REC OF MOVMAG.
  5828     MOVE "NR" TO UN-MIS-FATT OF MOVMAG.
  5829*EURO*
  5830     MOVE "EUR " TO DIVISA OF MOVMAG.
  5831*    MOVE "LIT " TO DIVISA OF MOVMAG.
  5832     MOVE C-MAT-TRANS-RID TO C-MAT OF MOVMAG.
  5833 EX-PREPARA-MOVMAG.
  5834     EXIT.
  5835*
  5836*
  5837*
  5838 CREA-MOVMAG-P-3.
  5839*VACO*
  5840      MOVE COSTO-ELEM TO PREZZO OF MOVMAG.
  5841*VACO*
  5842     MOVE PREZZO-ELEM TO COSTO-STD OF MOVMAG.
  5843*BUDA*
  5844     MOVE CAMBIO-ELEM TO MOD-IMPUTAZ OF MOVMAG.
  5845*BUDA*
  5846     MOVE "VESD" TO C-OPE OF MOVMAG.
  5847     MOVE QTA-TAGLIE-NEG TO QTA-TAGLIE OF MOVMAG.
  5848*MAG6/7*
  5849*    MOVE 7 TO MAGAZZINO OF MOVMAG.
  5850     MOVE MAG-INPUT-R TO MAGAZZINO OF MOVMAG.
  5851*FIFRA*
  5852*    MOVE CONTO-IN-R TO CONTO OF MOVMAG.
  5853     IF CONTO-FATTURA-MEM NOT = 0
  5854       MOVE CONTO-FATTURA-MEM TO CONTO OF MOVMAG
  5855       MOVE CONTO-IN-R TO MOD-IMPUTAZ OF MOVMAG
  5856     ELSE
  5857       MOVE CONTO-IN-R TO CONTO OF MOVMAG.
  5858     MOVE MOVMAG TO AREA-REC-SET.
  5859*
  5860     MOVE "MOVMAG" TO W-NOME-DATA-SET.
  5861     PERFORM TTDBPUT THRU EX-TTDBPUT.
  5862     IF NOT W-OK-IMAGE
  5863       MOVE W-STATUS-WORD-IMAGE TO STATO-DISPLAY
  5864       DISPLAY "ERR PUT MOVMAG-P3- " STATO-DISPLAY
  5865       DISPLAY "PER C-MAT " C-MAT-TRANSITO
  5866     CANCEL "QDBERROR"
  5867       CALL "QDBERROR" USING W-COMMON.
  5868 EX-CREA-MOVMAG-P-3.
  5869     EXIT.
  5870*
  5871*
  5872*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 103
* READVE3.cob
  5873*
  5874 AGGIORNA-SITPF-P-3.
  5875     MOVE LOW-VALUE TO PARAGGPF.
  5876     MOVE C-MAT-TRANS-RID TO C-MAT OF PARAGGPF.
  5877*MAG6/7*
  5878*    MOVE 7 TO MAGAZZINO OF PARAGGPF.
  5879     MOVE MAG-INPUT-R TO MAGAZZINO OF PARAGGPF.
  5880     MOVE -1 TO VALORE OF PARAGGPF.
  5881     MOVE QTA-TAGLIE-NEG
  5882       TO QTA-8 OF PARAGGPF.
  5883     MOVE 1 TO F-GIAC OF PARAGGPF.
  5884     CANCEL "AGSITPFW"
  5885     CALL "AGSITPFW" USING W-COMMON PARAGGPF.
  5886 EX-AGGIORNA-SITPF-P-3.
  5887     EXIT.
  5888*
  5889*
  5890*
  5891*
  5892*
  5893*
  5894*
  5895*
  5896 CARICA-TABELLA.
  5897     MOVE 0 TO STK-C-MAT.
  5898     MOVE 0 TO STK-STAGIONE.
  5899     MOVE 0 TO STK-COLL.
  5900     MOVE 0 TO STK-PRZ-LORDO STK-PRIMA-VOLTA.
  5901*MAG6/7*
  5902*    IF SI-DT-ESTERO
  5903*       MOVE NOME-IN-B TO STK-NOME
  5904*      ELSE
  5905*         MOVE NOME-IN TO STK-NOME.
  5906     IF MAG-FALLATO
  5907       MOVE "STF" TO NOME-IN-3
  5908     ELSE
  5909*Mag3_V/F*
  5910       IF F-V-INPUT = "F"
  5911          MOVE "STF" TO NOME-IN-3
  5912       ELSE
  5913*
  5914       MOVE "STV" TO NOME-IN-3.
  5915     MOVE CONTO-IN-R TO NOME-IN-5.
  5916     MOVE NOME-IN-35 TO STK-NOME.
  5917*
  5918     CALL "DTVALSTK" USING STK-NOME
  5919                          STK-C-MAT
  5920                          STK-STAGIONE
  5921                          STK-COLL
  5922                          STK-SCO
  5923                          STK-PRZ-SCO
  5924                          STK-PRZ-LORDO STK-MSG
  5925                          STK-PRIMA-VOLTA
  5926                          STK-CAMBIO
  5927*EURO1*
  5928                          W-COMMON.
  5929*
  5930      IF STK-MSG NOT = SPACE
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 104
* READVE3.cob
  5931        DISPLAY STK-MSG
  5932        MOVE 1 TO JRUNC
  5933        STOP RUN JRUNC.
  5934 EX-CARICA-TABELLA.
  5935     EXIT.
  5936*
  5937*
  5938*
  5939 STAMPA-RAPPORTINO.
  5940     PERFORM VARYING W-INDICE-3 FROM 1 BY 1 UNTIL
  5941      W-INDICE-3 > QT-NUM-ELEM-EFF OF PARTAB-ART
  5942        DISPLAY 'M' TAB-ART OF ART-TAB-LETTI(W-INDICE-3)
  5943     END-PERFORM
  5944     CALL "RAPPRAI3" USING W-COMMON SQLCA
  5945                           TABELLA-ARTICOLI-LETTI PARTAB-ART
  5946                           CONTO-IN-R D-CONTO-MEM
  5947                           TABELLA-NO-GIAC IND-CAPI-NO-GIAC
  5948*MAG6/7*
  5949                           MAG-INPUT-R.
  5950 EX-STAMPA-RAPPORTINO.
  5951     EXIT.
  5952*
  5953*
  5954*MAG6/7*
  5955 VERIF-MAG.
  5956*VIBLO*
  5957     DISPLAY "MAG provenienza (3 cifre)".
  5958     ACCEPT MAG-INPUT.
  5959     IF MAG-INPUT NOT NUMERIC
  5960       PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  5961       DISPLAY "MAG non numerico"
  5962     ELSE
  5963        IF NOT MAG-OK
  5964         PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  5965         DISPLAY SUGG-MAG-DISP
  5966*
  5967         MOVE SPACE TO MAG-INPUT.
  5968*     MOVE "003" TO MAG-INPUT.
  5969*
  5970 EX-VERIF-MAG.
  5971     EXIT.
  5972*
  5973*Mag3_V/F*
  5974* Copiaincollato senza pieta` dalla procedurina precedente
  5975 VERIF-F-V.
  5976*VIBLO*
  5977*     DISPLAY "Stock Fallato o Valido? (F/V)".
  5978*     ACCEPT F-V-INPUT.
  5979*     IF NOT (F-V-INPUT = "F" OR = "V" OR = "f" OR = "v")
  5980*       PERFORM 2 TIMES CALL "FAIBEEP" END-PERFORM
  5981*       DISPLAY "Digitare o 'F' o 'V'"
  5982*       MOVE SPACE TO F-V-INPUT
  5983*     ELSE
  5984*       IF F-V-INPUT = "v" OR = "V"
  5985*         MOVE "V" TO F-V-INPUT
  5986*       ELSE
  5987*         MOVE "F" TO F-V-INPUT.
  5988     MOVE "V" TO F-V-INPUT.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 105
* READVE3.cob
  5989*
  5990 EX-VERIF-F-V.
  5991     EXIT.
  5992*PRZ-PUBBL*
  5993***************** ROUTINES SQL *******************
  5994*
  5995 TEST-ERR.
  5996     MOVE SQLCODE TO SQL-STATUS.
  5997     IF SQLCODE = OK OR NO-MEMORY OR DEADLOCK OR NOT-FOUND
  5998        CONTINUE
  5999     ELSE
  6000     CANCEL "CALLSQLE"
  6001        CALL "CALLSQLE" USING SQLCA PAR-ERR AREA-HL AREA-SI.
  6002 TEST-ERR-EX.
  6003     EXIT.
  6004*
  6005*
  6006 BEGIN-RC.
  6007**** Start SQL Preprocessor ****
  6008*    EXEC SQL
  6009*       BEGIN WORK RC
  6010*    END-EXEC
  6011**** Start Inserted Statements ****
  6012     MOVE 2 TO SQLX-ISOLATION-LEVEL
  6013     CALL "sqlx_bw" USING
  6014          SQLX-ISOLATION-LEVEL,
  6015          SQLCA
  6016**** End SQL Processor   ****
  6017     MOVE "BEGIN WORK RC" TO ER-DESCRIZIONE
  6018     PERFORM TEST-ERR THRU TEST-ERR-EX.
  6019 BEGIN-RC-EX.
  6020     EXIT.
  6021*
  6022*
  6023 S-S-COMMIT.
  6024**** Start SQL Preprocessor ****
  6025*    EXEC SQL
  6026*       COMMIT WORK
  6027*    END-EXEC.
  6028**** Start Inserted Statements ****
  6029     CALL "sqlx_cw" USING
  6030          SQLCA
  6031         CONTINUE.
  6032**** End SQL Processor   ****
  6033     MOVE "COMMIT WORK" TO ER-DESCRIZIONE
  6034     PERFORM TEST-ERR THRU TEST-ERR-EX.
  6035 S-S-COMMIT-EX.
  6036     EXIT.
  6037*
  6038*
  6039* NO-DATGE
  6040* DECLARE-CURS-MOD.
  6041*     EXEC SQL
  6042*       DECLARE CURSMOD CURSOR FOR
  6043*           SELECT MODELLO_NEW,
  6044*                  ANNO,
  6045*                  STAG
  6046*             FROM ANAMAT.MODELLI
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 106
* READVE3.cob
  6047*            WHERE
  6048*           SOCIETA         = :MODELLI-SOCIETA AND
  6049*           COD_ESTENSIONE  = :MODELLI-ESTENSIONE AND
  6050*           MODELLO_MAXIMA  = :MODELLI-MODELLO-MAXIMA
  6051*     END-EXEC.
  6052* DECLARE-CURS-MOD-EX.
  6053*     EXIT.
  6054**
  6055**
  6056* OPEN-CURS-MOD.
  6057*     EXEC SQL
  6058*          OPEN CURSMOD KEEP CURSOR
  6059*     END-EXEC.
  6060*     MOVE 'OPEN-CURS-MOD' TO ER-DESCRIZIONE.
  6061*     PERFORM TEST-ERR THRU TEST-ERR-EX.
  6062* OPEN-CURS-MOD-EX.
  6063*     EXIT.
  6064**
  6065**
  6066* FETCH-SINGOLA-CURS-MOD.
  6067*     EXEC SQL
  6068*       FETCH CURSMOD
  6069*       INTO :MODELLI-MODELLO-NEW,
  6070*            :MODELLI-ANNO,
  6071*            :MODELLI-STAGIONE
  6072*     END-EXEC.
  6073**
  6074*     IF SQLCODE NOT = OK
  6075*       MOVE 1 TO FLAG-CURSORE
  6076*     END-IF.
  6077* FETCH-SINGOLA-CURS-MOD-EX.
  6078*     EXIT.
  6079**
  6080**
  6081* CLOSE-CURS-MOD.
  6082*     EXEC SQL
  6083*          CLOSE CURSMOD
  6084*     END-EXEC.
  6085*     MOVE 'CLOSE-CURS-MOD' TO ER-DESCRIZIONE.
  6086*     PERFORM TEST-ERR THRU TEST-ERR-EX.
  6087* CLOSE-CURS-MOD-EX.
  6088*     EXIT.
  6089* NO-DATGE
  6090*
  6091*
  6092*
  6093*
  6094 COMANDI-IGP-TESTA.
  6095     MOVE COMIGP-PTXSETUP TO REC-PEND.
  6096     WRITE REC-PEND.
  6097     MOVE COMIGP-PTXCFG2 TO REC-PEND.
  6098     WRITE REC-PEND.
  6099     MOVE COMIGP-PTXEND TO REC-PEND.
  6100     WRITE REC-PEND.
  6101 EX-COMANDI-IGP-TESTA.
  6102     EXIT.
  6103*
  6104*
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 107
* READVE3.cob
  6105 COMANDI-IGP-TESTA-1.
  6106     MOVE COMIGP-LISTEN TO REC-PEND.
  6107     WRITE REC-PEND.
  6108     MOVE COMIGP-CREATE TO REC-PEND.
  6109     WRITE REC-PEND.
  6110     MOVE COMIGP-SCALEDOT TO REC-PEND.
  6111     WRITE REC-PEND.
  6112     MOVE COMIGP-ALPHA TO REC-PEND.
  6113     WRITE REC-PEND.
  6114 EX-COMANDI-IGP-TESTA-1.
  6115     EXIT.
  6116*
  6117*
  6118 COMANDI-IGP-FINE-1.
  6119     MOVE COMIGP-STOP TO REC-PEND.
  6120     WRITE REC-PEND.
  6121     MOVE COMIGP-END TO REC-PEND.
  6122     WRITE REC-PEND.
  6123     MOVE COMIGP-EXECUTE TO REC-PEND.
  6124     WRITE REC-PEND.
  6125     MOVE COMIGP-QUIET TO REC-PEND.
  6126     WRITE REC-PEND.
  6127 EX-COMANDI-IGP-FINE-1.
  6128     EXIT.
  6129*
  6130*
  6131 SCRIVI-RECORD-IGP.
  6132     MOVE SPACES TO REC-PEND.
  6133*
  6134     MOVE ROW-IGP TO TEMP-X-IN.
  6135     PERFORM TOGLI-ZERI THRU EX-TOGLI-ZERI.
  6136     MOVE TEMP-X-OUT TO ROW-X-IGP.
  6137*
  6138     MOVE COL-IGP TO TEMP-X-IN.
  6139     PERFORM TOGLI-ZERI THRU EX-TOGLI-ZERI.
  6140     MOVE TEMP-X-OUT TO COL-X-IGP.
  6141*
  6142     STRING "C12;"        DELIMITED BY SIZE
  6143            ROW-X-IGP     DELIMITED BY SPACE
  6144            ";"           DELIMITED BY SIZE
  6145            COL-X-IGP     DELIMITED BY SPACE
  6146            ";"           DELIMITED BY SIZE
  6147            VERT-EXP-IGP  DELIMITED BY SIZE
  6148            ";"           DELIMITED BY SIZE
  6149            ORIZ-EXP-IGP  DELIMITED BY SIZE
  6150            ';"'          DELIMITED BY SIZE
  6151            RIGA-PEND-COM DELIMITED BY SIZE
  6152            '"'           DELIMITED BY SIZE
  6153       INTO REC-PEND.
  6154*
  6155     WRITE REC-PEND.
  6156 EX-SCRIVI-RECORD-IGP.
  6157     EXIT.
  6158*
  6159*
  6160 TOGLI-ZERI.
  6161     MOVE 0 TO PRIMO-NONZERO.
  6162     PERFORM VARYING K FROM 1 BY 1
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 108
* READVE3.cob
  6163       UNTIL K > 4 OR PRIMO-NONZERO <> 0
  6164         IF TEMP-EL-X OF TEMP-X-IN(K) <> "0"
  6165             MOVE K TO PRIMO-NONZERO
  6166         END-IF
  6167     END-PERFORM.
  6168*
  6169     IF PRIMO-NONZERO <> 0
  6170       UNSTRING TEMP-X-IN INTO TEMP-X-OUT
  6171                WITH POINTER PRIMO-NONZERO
  6172     ELSE
  6173       MOVE TEMP-X-IN TO TEMP-X-OUT
  6174     END-IF.
  6175 EX-TOGLI-ZERI.
  6176     EXIT.
  6177*
  6178*
  6179*MOVSKU
  6180 INSERISCI-MOVSKU.
  6181*
  6182 MOVE ELEM-TAB-SING(IND-BARUNI) TO ELEMENTO-SINGOLI.
  6183*
  6184 MOVE '1' TO OUTPUT-VAL-A
  6185 MOVE SPACES TO OUTPUT-VAL-B
  6186 PERFORM CHIAMA-GETBARUNI THRU CHIAMA-GETBARUNI-EX.
  6187*
  6188 PERFORM CONVERTI-BARCODE THRU EX-CONVERTI-BARCODE.
  6189*
  6190 MOVE C-MAT-TRANS-RID TO MOVSKU-CMAT.
  6191 MOVE TAGLIA OF C-MAT-A-BARRE TO NTG-IN
  6192 MOVE FUNCTION idxtg(NTG-IN)
  6193                          TO MOVSKU-TG.
  6194*
  6195 IF OUTPUT-VAL-A = '0'
  6196    MOVE OUTPUT-VAL-B-OK TO MOVSKU-BARUNI
  6197 ELSE
  6198    STOP RUN
  6199 END-IF.
  6200*
  6201 PERFORM INSERT-SKU-E-SING THRU EX-INSERT-SKU-E-SING.
  6202*
  6203 MOVE MOVSKU-BARUNI(1:8)          TO MOVSKU-SKU
  6204 MOVE RIF-BOLLA-DDT TO  MOVSKU-RIF-INTERNO.
  6205 MOVE CONTO-IN-R TO MOVSKU-CONTO.
  6206 MOVE MAG-INPUT-R TO MOVSKU-MAG.
  6207 MOVE 0 TO MOVSKU-IS-BARUNI-READ.
  6208 MOVE 0 TO MOVSKU-IS-BARUNI-CERTIFIED.
  6209 MOVE MOVSKU-BARUNI(1:8) TO MOVSKU-SKU-FATTURAZIONE.
  6210*
  6211* NO-DATGE
  6212 PERFORM S-SET-1 THRU S-SET-1-EX.
  6213* NO-DATGE
  6214**** Start SQL Preprocessor ****
  6215*    EXEC SQL
  6216*        INSERT INTO MOV_SKU
  6217*        VALUES(
  6218*                NULL,
  6219*               :MOVSKU-RIF-INTERNO,
  6220*               :MOVSKU-CMAT,
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 109
* READVE3.cob
  6221*               :MOVSKU-TG,
  6222*               :MOVSKU-BARUNI,
  6223*               :MOVSKU-CONTO,
  6224*               :MOVSKU-MAG,
  6225*               :MOVSKU-SKU,
  6226*               :MOVSKU-IS-BARUNI-READ,
  6227*               :MOVSKU-IS-BARUNI-CERTIFIED,
  6228*               :MOVSKU-SKU-FATTURAZIONE
  6229*               )
  6230*    END-EXEC
  6231**** Start Inserted Statements ****
  6232     MOVE MOVSKU-RIF-INTERNO TO SQLI-002-000
  6233     MOVE MOVSKU-CMAT TO SQLI-002-001
  6234     MOVE MOVSKU-TG TO SQLI-002-002
  6235     MOVE MOVSKU-BARUNI TO SQLI-002-003
  6236     MOVE MOVSKU-CONTO TO SQLI-002-004
  6237     MOVE MOVSKU-MAG TO SQLI-002-005
  6238     MOVE MOVSKU-SKU TO SQLI-002-006
  6239     MOVE MOVSKU-IS-BARUNI-READ TO SQLI-002-007
  6240     MOVE MOVSKU-IS-BARUNI-CERTIFIED TO SQLI-002-008
  6241     MOVE MOVSKU-SKU-FATTURAZIONE TO SQLI-002-009
  6242     CALL "sqlx_insert" USING
  6243          SQLX-PROG,
  6244          SQL-PARAM-002-X,
  6245          SQL-SEL-002-X,
  6246          SQLI-TIPO-002-X,
  6247          SQLI-REC-002,
  6248          SQLCA
  6249**** End SQL Processor   ****
  6250     MOVE "Insert MOV-SKU " TO ER-DESCRIZIONE
  6251     PERFORM TEST-ERR THRU TEST-ERR-EX.
  6252*
  6253 INSERISCI-MOVSKU-EX.
  6254     EXIT.
  6255*
  6256 CONVERTI-BARCODE.
  6257     MOVE ZEROS
  6258       TO C-MAT-COM.
  6259*
  6260     MOVE C-MAT-SING
  6261       TO C-MAT-A-BARRE-RID.
  6262*
  6263     MOVE MODELLO OF  C-MAT-A-BARRE
  6264       TO MODELLO      OF      C-MAT-COM.
  6265     MOVE VESTIBILITA OF  C-MAT-A-BARRE
  6266       TO VEST-A       OF      C-MAT-COM.
  6267     MOVE PEZZO   OF  C-MAT-A-BARRE
  6268       TO PEZZO-A      OF      C-MAT-COM.
  6269     MOVE PREFBC-V-F OF  C-MAT-A-BARRE
  6270       TO PREFISSO-V-F OF      C-MAT-COM.
  6271     MOVE SOC-BC-MOD OF  C-MAT-A-BARRE
  6272       TO SOCIETA-MOD  OF      C-MAT-COM.
  6273     MOVE VARIANTE-COL OF  C-MAT-A-BARRE
  6274       TO COLORE       OF      C-MAT-COM.
  6275 EX-CONVERTI-BARCODE.
  6276     EXIT.
  6277*
  6278 CHIAMA-GETBARUNI.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 110
* READVE3.cob
  6279*
  6280     MOVE C-MAT-SING TO INPUT-VAL
  6281     MOVE RIF-BOLLA-DDT TO INPUT-VAL-B
  6282     MOVE "READVE3" TO INPUT-VAL-C
  6283*
  6284     CALL "PYTHON" USING "ret_sku" "mod_sku_from_cobol"
  6285                         PY-INPUT-REC
  6286                         PY-OUTPUT-REC.
  6287     IF OUTPUT-VAL-A = '1'
  6288       DISPLAY OUTPUT-VAL-B.
  6289*
  6290 CHIAMA-GETBARUNI-EX.
  6291     EXIT.
  6292*
  6293 INSERT-SKU-E-SING.
  6294*
  6295     MOVE "K2" TO QT-FUNZIONE OF PARTAB-SING
  6296         CANCEL "QTABEL"
  6297     CALL "QTABEL" USING PARTAB-SING
  6298                          TABELLA-SINGOLI
  6299                            ELEMENTO-SINGOLI
  6300*
  6301     IF QT-STATO OF PARTAB-SING = 0
  6302        MOVE OUTPUT-VAL-B-OK TO SKU-SING
  6303        MOVE SKU-SING TO
  6304                ELEM-TAB-SING(QT-INDEX-ELEM OF PARTAB-SING)(24:13)
  6305     ELSE
  6306        MOVE QT-STATO OF PARTAB-SING TO ERR-DISP
  6307        DISPLAY "ERR UPDATE QTABEL " ERR-DISP
  6308                  " - INSERISCI-SKU"
  6309        CANCEL "QDBERROR"
  6310        CALL "QDBERROR" USING W-COMMON
  6311     END-IF.
  6312*
  6313 EX-INSERT-SKU-E-SING.
  6314     EXIT.
  6315*
  6316* NO-DATGE
  6317***********connessione a DATGE*******************
  6318* S-SET-2.
  6319*        EXEC SQL
  6320*           SET CONNECTION 'DB2'
  6321*        END-EXEC.
  6322* S-SET-2-EX.
  6323*     EXIT.
  6324** NO-DATGE
  6325*
  6326***********connessione a MAGAUTO*****************
  6327 S-SET-1.
  6328**** Start SQL Preprocessor ****
  6329*       EXEC SQL
  6330*          SET CONNECTION 'DB1'
  6331*       END-EXEC.
  6332**** Start Inserted Statements ****
  6333  MOVE "DB1" TO SQL-CONN-ALIAS
  6334    CALL "sqlx_setconn" USING
  6335             SQL-CONN-ALIAS,SQLCA
  6336         CONTINUE.
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 111
* READVE3.cob
  6337**** End SQL Processor   ****
  6338 S-SET-1-EX.
  6339     EXIT.
  6340*
  6341*MOVSKU
  6342*
  6343*ASOLOB2C*
  6344 CARICA-B2C-NO-DT.
  6345*
  6346     PERFORM WITH TEST AFTER
  6347             UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK
  6348             PERFORM BEGIN-RC THRU
  6349                     BEGIN-RC-EX
  6350             IF SQLCODE = OK
  6351                     PERFORM SE-SELECT-NEGOZIO-CATEG
  6352                        THRU SE-SELECT-NEGOZIO-CATEG-EX
  6353             END-IF
  6354     END-PERFORM.
  6355     IF SQLCODE = NOT-FOUND
  6356        DISPLAY "NESSUNA NEGOZIO CATEGORIA TROVATA"
  6357                                UPON SYSERR
  6358        MOVE 0 TO NUM-B2C-NO-DT
  6359      ELSE
  6360        MOVE SQLERRD (3) TO NUM-B2C-NO-DT
  6361      END-IF.
  6362*
  6363     PERFORM S-S-COMMIT THRU S-S-COMMIT-EX.
  6364*     DISPLAY SPACE.
  6365*     DISPLAY "TAB-B2C-NO-DT:  " NUM-B2C-NO-DT
  6366*     PERFORM VARYING IND-B2C-NO-DT FROM 1 BY 1
  6367*             UNTIL IND-B2C-NO-DT > NUM-B2C-NO-DT
  6368*       DISPLAY MAG-B2C-NO-DT(IND-B2C-NO-DT)
  6369*     END-PERFORM.
  6370 EX-CARICA-B2C-NO-DT.
  6371     EXIT.
  6372*
  6373*
  6374 SE-SELECT-NEGOZIO-CATEG.
  6375**** Start SQL Preprocessor ****
  6376*    EXEC SQL
  6377*         BULK SELECT NEGOZIO
  6378*         INTO :TAB-B2C-NO-DT
  6379*         FROM NEGOZIO_ANAG_CATEGORIA
  6380*            JOIN NEGOZIO_CATEGORIA USING (ID_CATEGORIA)
  6381*            where DESC_CATEGORIA = 'NEGOZI_ITALIA_B2C_SOC'
  6382*            order by NEGOZIO
  6383*    END-EXEC
  6384**** Start Inserted Statements ****
  6385     MOVE 100 TO SQL-TIMES-003
  6386     MOVE 1 TO SQL-START-003
  6387     CALL "sqlx_bulksel" USING
  6388          SQLX-PROG,
  6389          SQL-PARAM-003-X,
  6390          SQL-SEL-003-X,
  6391          SQLI-TIPO-003-X,
  6392          SQLO-TIPO-003-X,
  6393          SQLI-REC-003,
  6394          TAB-B2C-NO-DT,
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 112
* READVE3.cob
  6395          SQL-START-003,
  6396          SQL-TIMES-003,
  6397          SQLCA
  6398**** End SQL Processor   ****
  6399     MOVE "SELECT NEGOZIO-CATEG  " TO ER-DESCRIZIONE
  6400     IF SQLCODE NOT = MULTIPLE-ROWS
  6401        PERFORM TEST-ERR THRU TEST-ERR-EX.
  6402 SE-SELECT-NEGOZIO-CATEG-EX.
  6403     EXIT.
  6404*ASOLOB2C*
  6405*ASOLOB2C*
  6406 RIVALUTA-COSTO-ANAMAT.
  6407      PERFORM CERCA-B2C-NO-DT THRU EX-CERCA-B2C-NO-DT.
  6408*
  6409     PERFORM RICERCA-COSTO-ANAMAT THRU EX-RICERCA-COSTO-ANAMAT.
  6410 EX-RIVALUTA-COSTO-ANAMAT.
  6411 CERCA-B2C-NO-DT.
  6412     PERFORM VARYING IND-B2C-NO-DT FROM 1 BY 1
  6413          UNTIL IND-B2C-NO-DT > NUM-B2C-NO-DT OR
  6414              ANACST-MAG-COM = MAG-B2C-NO-DT(IND-B2C-NO-DT)
  6415        CONTINUE
  6416     END-PERFORM.
  6417     IF IND-B2C-NO-DT > NUM-B2C-NO-DT
  6418       MOVE 'N' TO FLAG-B2C-NO-DT
  6419     ELSE
  6420       MOVE 'S' TO FLAG-B2C-NO-DT
  6421     END-IF.
  6422 EX-CERCA-B2C-NO-DT.
  6423     EXIT.
  6424 RICERCA-COSTO-ANAMAT.
  6425*
  6426     MOVE ANACST-C-MAT-COM TO ANACST-C-MAT.
  6427     MOVE 0 TO ANACST-CST-COM
  6428     PERFORM WITH TEST AFTER
  6429             UNTIL SQLCODE <> NO-MEMORY AND <> DEADLOCK
  6430             PERFORM BEGIN-RC THRU
  6431                     BEGIN-RC-EX
  6432             IF SQLCODE = OK
  6433                     PERFORM SE-SELECT-ANAMAT-CST
  6434                        THRU SE-SELECT-ANAMAT-CST-EX
  6435             END-IF
  6436     END-PERFORM.
  6437     IF SQLCODE = OK
  6438       IF B2C-NO-DT
  6439         MOVE ANACST-CST-STD-2 TO ANACST-CST-COM
  6440       ELSE
  6441         MOVE ANACST-CST-STD TO ANACST-CST-COM
  6442       END-IF
  6443     END-IF.
  6444*
  6445     PERFORM S-S-COMMIT THRU S-S-COMMIT-EX.
  6446 EX-RICERCA-COSTO-ANAMAT.
  6447     EXIT.
  6448 SE-SELECT-ANAMAT-CST.
  6449**** Start SQL Preprocessor ****
  6450*    EXEC SQL
  6451*         SELECT CST_STD, CST_STD_2
  6452*         INTO :ANACST-CST-STD, :ANACST-CST-STD-2
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 113
* READVE3.cob
  6453*         FROM ANAMAT_CST
  6454*         WHERE C_MAT   = :ANACST-C-MAT
  6455*    END-EXEC
  6456**** Start Inserted Statements ****
  6457     MOVE ANACST-C-MAT TO SQLI-004-000
  6458     CALL "sqlx_select" USING
  6459          SQLX-PROG,
  6460          SQL-PARAM-004-X,
  6461          SQL-SEL-004-X,
  6462          SQLI-TIPO-004-X,
  6463          SQLO-TIPO-004-X,
  6464          SQLI-REC-004,
  6465          SQLO-REC-004,
  6466          SQLCA
  6467     IF SQLCODE = 0
  6468       MOVE SQLO-004-000 TO ANACST-CST-STD
  6469       MOVE SQLO-004-001 TO ANACST-CST-STD-2
  6470     END-IF
  6471**** End SQL Processor   ****
  6472*
  6473     MOVE "SELECT ANAMAT_CST   " TO ER-DESCRIZIONE
  6474     PERFORM TEST-ERR THRU TEST-ERR-EX.
  6475 SE-SELECT-ANAMAT-CST-EX.
  6476     EXIT.
  6477*ASOLOB2C*
  6478*
  6479*UNICODDT*
  6480*================================================================*
  6481 INIT-PAR-TAB-UNICO-DDT.
  6482*================================================================*
  6483    MOVE   16 TO QT-LL-ELEM       OF PAR-TAB-UNICO-DDT.
  6484    MOVE    4 TO QT-LL-KEY        OF PAR-TAB-UNICO-DDT.
  6485    MOVE    1 TO QT-ADDR-KEY      OF PAR-TAB-UNICO-DDT.
  6486    MOVE 1980 TO QT-NUM-ELEM-MAX  OF PAR-TAB-UNICO-DDT.
  6487    MOVE    0 TO QT-NUM-ELEM-EFF  OF PAR-TAB-UNICO-DDT.
  6488    INITIALIZE TAB-UNICO-DDT.
  6489 EX-INIT-PAR-TAB-UNICO-DDT. EXIT.
  6490*----------------------------------------------------------------*
  6491 LEGGI-TAB-UNICO-DDT.
  6492    MOVE "K2" TO QT-FUNZIONE OF PAR-TAB-UNICO-DDT.
  6493    CANCEL "QTABELXL"
  6494    CALL "QTABELXL" USING PAR-TAB-UNICO-DDT
  6495                        TAB-UNICO-DDT
  6496                        DEP-TAB-UNICO-DDT.
  6497    IF QT-STATO OF PAR-TAB-UNICO-DDT = 0
  6498        PERFORM PRENDI-DEP-TAB-UNICO-DDT THRU
  6499             EX-PRENDI-DEP-TAB-UNICO-DDT
  6500    ELSE
  6501        INITIALIZE DATI-TAB-UNICO-DDT.
  6502 EX-LEGGI-TAB-UNICO-DDT. EXIT.
  6503*----------------------------------------------------------------*
  6504 PRENDI-DEP-TAB-UNICO-DDT.
  6505    MOVE ELE-TAB-UNICO-DDT(QT-INDEX-ELEM OF PAR-TAB-UNICO-DDT)
  6506      TO DEP-TAB-UNICO-DDT.
  6507 EX-PRENDI-DEP-TAB-UNICO-DDT. EXIT.
  6508*----------------------------------------------------------------*
  6509 AGG-TAB-UNICO-DDT.
  6510    IF QT-STATO OF PAR-TAB-UNICO-DDT = 0
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 114
* READVE3.cob
  6511        PERFORM RIMETTI-DEP-TAB-UNICO-DDT
  6512           THRU EX-RIMETTI-DEP-TAB-UNICO-DDT
  6513    ELSE
  6514        PERFORM INS-TAB-UNICO-DDT THRU EX-INS-TAB-UNICO-DDT.
  6515 EX-AGG-TAB-UNICO-DDT. EXIT.
  6516*----------------------------------------------------------------*
  6517 RIMETTI-DEP-TAB-UNICO-DDT.
  6518    MOVE DEP-TAB-UNICO-DDT
  6519      TO ELE-TAB-UNICO-DDT(QT-INDEX-ELEM OF PAR-TAB-UNICO-DDT).
  6520 EX-RIMETTI-DEP-TAB-UNICO-DDT. EXIT.
  6521*----------------------------------------------------------------*
  6522 INS-TAB-UNICO-DDT.
  6523    MOVE "K1"   TO QT-FUNZIONE OF PAR-TAB-UNICO-DDT.
  6524    CANCEL "QTABELXL"
  6525    CALL "QTABELXL" USING PAR-TAB-UNICO-DDT
  6526                        TAB-UNICO-DDT
  6527                        DEP-TAB-UNICO-DDT.
  6528*    DISPLAY QT-STATO OF PAR-TAB-UNICO-DDT.
  6529    IF QT-STATO OF PAR-TAB-UNICO-DDT NOT = 0
  6530        IF QT-STATO OF PAR-TAB-UNICO-DDT = -2
  6531            DISPLAY "AS=" TAB-AS " CL=" TAB-CL
  6532            " GIA' INSERITI CON CAPI " TAB-MAX-CAPI
  6533        ELSE
  6534            IF QT-STATO OF PAR-TAB-UNICO-DDT = -1
  6535                DISPLAY "TAB-UNICO-DDT PIENA >> ALLARGARE"
  6536            ELSE
  6537               MOVE QT-STATO OF PAR-TAB-UNICO-DDT
  6538                 TO ERR-DISP
  6539               DISPLAY "ERR k1 QTABELXL " ERR-DISP
  6540                         " TAB-TAB-UNICO-DDT"
  6541               CANCEL "QDBERROR"
  6542               CALL "QDBERROR" USING W-COMMON .
  6543 EX-INS-TAB-UNICO-DDT. EXIT.
  6544*----------------------------------------------------------------*
  6545*----------------------------------------------------------------*
  6546 MOSTRA-TAB-UNICO-DDT.
  6547*    DISPLAY "TAB-UNICO-DDT".
  6548    PERFORM M-DEP-TAB-UNICO-DDT THRU
  6549         EX-M-DEP-TAB-UNICO-DDT
  6550         VARYING QT-INDEX-ELEM   OF PAR-TAB-UNICO-DDT
  6551         FROM 1 BY 1
  6552         UNTIL   QT-INDEX-ELEM   OF PAR-TAB-UNICO-DDT >
  6553                 QT-NUM-ELEM-EFF OF PAR-TAB-UNICO-DDT.
  6554    DISPLAY " ".
  6555 EX-MOSTRA-TAB-UNICO-DDT. EXIT.
  6556*----------------------------------------------------------------*
  6557 M-DEP-TAB-UNICO-DDT.
  6558    PERFORM PRENDI-DEP-TAB-UNICO-DDT THRU
  6559         EX-PRENDI-DEP-TAB-UNICO-DDT.
  6560    IF XD = "S"
  6561        DISPLAY DEP-TAB-UNICO-DDT
  6562    ELSE
  6563        DISPLAY
  6564*           "KEY: "
  6565            "AS = "  TAB-AS           OF DEP-TAB-UNICO-DDT "  "
  6566            "CL = "  TAB-CL           OF DEP-TAB-UNICO-DDT "  "
  6567*           "DATI: "
  6568            "MAX-CAPI = " TAB-MAX-CAPI OF DEP-TAB-UNICO-DDT "  "
* Micro Focus Server Express         V4.0 revision 000 01-Dec-22 15:02 Page 115
* READVE3.cob
  6569*            "CAPI-LETTI = "
  6570*                   TAB-CAPI-LETTI OF DEP-TAB-UNICO-DDT " "
  6571            .
  6572 EX-M-DEP-TAB-UNICO-DDT. EXIT.
  6573 CICLO-DISIMPEGNO.
  6574    PERFORM
  6575         VARYING QT-INDEX-ELEM   OF PAR-TAB-UNICO-DDT
  6576         FROM 1 BY 1
  6577         UNTIL   QT-INDEX-ELEM   OF PAR-TAB-UNICO-DDT >
  6578                 QT-NUM-ELEM-EFF OF PAR-TAB-UNICO-DDT
  6579       PERFORM PRENDI-DEP-TAB-UNICO-DDT
  6580          THRU EX-PRENDI-DEP-TAB-UNICO-DDT
  6581       DISPLAY DEP-TAB-UNICO-DDT
  6582       MOVE MAG-INPUT TO MAG-DISIMPEGNA
  6583       MOVE SOCIETA-INPUT TO FORN-DISIMPEGNA
  6584       MOVE TAB-AS TO AS-DISIMPEGNA(1)
  6585       MOVE TAB-CL TO CLASSE-DISIMPEGNA(1)
  6586       PERFORM CALL-DISIMPEGNA-MAG THRU EX-CALL-DISIMPEGNA-MAG
  6587       IF NOT PY-OUTPUT-DISIMPEGNO-OK
  6588          DISPLAY "ERRORE DISIMPEGNO!!!"
  6589          STOP RUN
  6590       END-IF
  6591    END-PERFORM.
  6592 EX-CICLO-DISIMPEGNO. EXIT.
  6593*----------------------------------------------------------------*
  6594*UNICODDT*
* Micro Focus Server Express         V4.0 revision 000 Compiler
* Copyright (C) 1984-2004 Micro Focus International Ltd. URN RXCTO/AA0/00000A
*                                                        REF GNR-154062001AC
* Total Messages:     0
* Data:      972736     Code:       22728
