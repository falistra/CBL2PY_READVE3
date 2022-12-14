001000*CONTROL DYNAMIC                                                          
001100 IDENTIFICATION DIVISION.                                                 
001200 PROGRAM-ID. AGSITPFW.                                                    
001210*2000*        07/07/99                                                    
001220*     tratta date a 6 cifre                                               
001230*                                                                         
001240*EURO*        18/10/00                          EURO/LIRE                 
001250*     trattamento importi in EURO                                         
001251*  
      *ESTETA*     26/11/18
      *      estensione taglie
      *
001260*  
001300 ENVIRONMENT DIVISION.                                                    
001400 DATA DIVISION.                                                           
001500*                                                                         
001600*                                                                         
001700 WORKING-STORAGE SECTION.                                                 
001800*                        
      *ESTETA* 
       COPY NTG.     
      * 
001900 01  AREA-REC-SET.                                                        
002000  05  REC-SITPF       COPY YSITPF.                                        
002100*                                                                         
002200 01  QTA-8-COM.                                                           
002300  05  QTA-COM           PIC S9(8) COMP COPY NTGOCCURS.                          
002400*                                                                         
002500 01  FLAG-COMODO        PIC S9(4) COMP.                                   
002600  88  SOMMA-QTA           VALUE 1.                                        
002700  88  SOTTRAI-QTA         VALUE -1.                                       
002800*                                                                         
002900 01  I-QTA              PIC S9(4) COMP.                                   
003000*                                                                         
003100 01  SOMMA-QTA-8-COMODO      PIC S9(8) COMP.                              
003200*                                                                         
003300 01  VALORE-COMODO           PIC S9(11) COMP-3.                           
003400 01  VALORE-COMODO-1         PIC S9(9)V9(4) COMP-3.                       
003500*                                                                         
003600*   
003200*  
003700 LINKAGE SECTION.                                                         
003800*                                                                         
003900 01  W-COMMON       COPY WCOMMONW.                                        
004000*                                                                         
004100 01  PAR-SITPF      COPY PARAGGPF.                                        
004200*                                                                         
004300*PAGE                                                                     
004400 PROCEDURE DIVISION  USING W-COMMON                                       
004500                           PAR-SITPF.                                     
004600 INIZIO.                                                                  
004700     PERFORM DBFIND-SITPF THRU EX-DBFIND-SITPF.                           
004800     IF W-OK-IMAGE                                                        
004900         PERFORM TRATTA-SITPF THRU EX-TRATTA-SITPF                        
005000     ELSE                                                                 
005100         MOVE 15 TO W-STATUS-WORD-IMAGE.                                  
005200     IF W-FINE-CATENA                                                     
005300         PERFORM TRATTA-NEW-REC THRU EX-TRATTA-NEW-REC                    
005400     ELSE                                                                 
005500         PERFORM AGGIORNA-REC THRU EX-AGGIORNA-REC.                       
005510*                                                                         
005600 FINE.                                                                    
005700     EXIT  PROGRAM.                                                       
005800*                                                                         
005900*                                                                         
006000 DBFIND-SITPF.                                                            
006100     MOVE "C-MAT;"      TO W-NOME-CAMPO.                                  
006200     MOVE C-MAT OF PAR-SITPF    TO W-VALORE-CAMPO.                        
006300     MOVE "SITPF;"      TO W-NOME-DATA-SET.                               
006400     PERFORM TTDBFIND THRU EX-TTDBFIND.                                   
006500 EX-DBFIND-SITPF.                                                         
006600     EXIT.                                                                
006700*                                                                         
006800*                                                                         
006900 TTDBFIND.                                                                
007000              COPY PDBFIND.                                               
007100*                                                                         
007200*                                                                         
007300 TRATTA-SITPF.                                                            
007400     PERFORM DBGET-SITPF THRU EX-DBGET-SITPF.                             
007500     PERFORM DBGET-SITPF THRU EX-DBGET-SITPF                              
007600         UNTIL W-FINE-CATENA OR                                           
007700               MAGAZZINO OF REC-SITPF = MAGAZZINO OF                      
007800               PAR-SITPF.                                                 
007900 EX-TRATTA-SITPF.                                                         
008000     EXIT.                                                                
008100*                                                                         
008200*                                                                         
008300 DBGET-SITPF.                                                             
008400     MOVE 5 TO W-MODO.                                                    
008500     PERFORM TTDBGET THRU EX-TTDBGET.                                     
008600 EX-DBGET-SITPF.                                                          
008700     EXIT.                                                                
008800*                                                                         
008900*                                                                         
009000 TTDBGET.                                                                 
009100              COPY PDBGET.                                                
009200*                                                                         
009300*                                                                         
009400 TRATTA-NEW-REC.                                                          
009500     PERFORM INIZIALIZZA-REC THRU EX-INIZIALIZZA-REC.                     
009600     PERFORM MUOVI-QUANTITA THRU EX-MUOVI-QUANTITA.                       
009700     MOVE "SITPF;"  TO W-NOME-DATA-SET.                                   
009800     PERFORM TTDBPUT  THRU EX-TTDBPUT.                                    
009900 EX-TRATTA-NEW-REC.                                                       
010000     EXIT.                                                                
010100*                                                                         
010200*                                                                         
010300 INIZIALIZZA-REC.                                                         
010400     MOVE LOW-VALUE TO REC-SITPF.                                         
010500     MOVE MAGAZZINO OF PAR-SITPF  TO MAGAZZINO OF REC-SITPF.              
010600     MOVE C-MAT OF PAR-SITPF  TO C-MAT OF REC-SITPF.                      
010700     MOVE 0 TO VAL-GIAC OF REC-SITPF.                                     
010800     MOVE 0 TO VAL-INV OF REC-SITPF.                                      
010900     MOVE SPACES TO VAL-REC OF REC-SITPF.                                 
011000 EX-INIZIALIZZA-REC.                                                      
011100     EXIT.                                                                
011200*                                                                         
011300*                                                                         
011400*                                                                         
011500*                                                                         
011600 AGGIORNA-REC.                                                            
011700     PERFORM MUOVI-QUANTITA THRU EX-MUOVI-QUANTITA.                       
011800     MOVE "SITPF;"  TO W-NOME-DATA-SET.                                   
011900     PERFORM TTUPDATE THRU EX-TTUPDATE.                                   
012000 EX-AGGIORNA-REC.                                                         
012100     EXIT.                                                                
012200*                                                                         
012300*                                                                         
012400 MUOVI-QUANTITA.                                                          
012500*                                                                         
012600     MOVE 0 TO SOMMA-QTA-8-COMODO.                                        
012700*                                                                         
012800     IF VALORE OF PAR-SITPF  < 0                                          
012900         PERFORM CALCOLA-VALORE-MEDIO THRU                                
013000                EX-CALCOLA-VALORE-MEDIO.                                  
013100*                                                                         
013200     MOVE 0 TO SOMMA-QTA-8-COMODO.                                        
013300*                                                                         
013400     IF NOT NO-GIAC                                                       
013500         MOVE W-FORMATO-INTERNO TO DT-UM OF REC-SITPF                     
013600         MOVE QTA-GIAC OF REC-SITPF TO QTA-8-COM                          
013700         MOVE F-GIAC TO FLAG-COMODO                                       
013800*                                                                         
013900         PERFORM TRATTA-VALORE THRU EX-TRATTA-VALORE                      
014000*                                                                         
014100         PERFORM VARIA-QTA THRU EX-VARIA-QTA                              
014200         MOVE QTA-8-COM TO QTA-GIAC OF REC-SITPF                          
014300         PERFORM VRF-GIAC THRU EX-VRF-GIAC                                
014400                 VARYING I-QTA FROM 1 BY 1 UNTIL                          
014500                         I-QTA >  NTG-NTG.                                       
014600     IF NOT NO-QTA-ORD                                                    
014700         MOVE QTA-ORDINATA OF REC-SITPF TO QTA-8-COM                      
014800         MOVE F-QTA-ORD TO FLAG-COMODO                                    
014900         PERFORM VARIA-QTA THRU EX-VARIA-QTA                              
015000         MOVE QTA-8-COM TO QTA-ORDINATA OF REC-SITPF                      
015100         PERFORM VRF-ORD THRU EX-VRF-ORD                                  
015200                 VARYING I-QTA FROM 1 BY 1 UNTIL                          
015300                         I-QTA >  NTG-NTG.                                       
015400     IF NOT NO-QTA-ORD-C                                                  
015500         MOVE QTA-ORDINATA-C OF REC-SITPF TO QTA-8-COM                    
015600         MOVE F-QTA-ORD-C TO FLAG-COMODO                                  
015700         PERFORM VARIA-QTA THRU EX-VARIA-QTA                              
015800         MOVE QTA-8-COM TO QTA-ORDINATA-C OF REC-SITPF                    
015900         PERFORM VRF-ORD-C THRU EX-VRF-ORD-C                              
016000                 VARYING I-QTA FROM 1 BY 1 UNTIL                          
016100                         I-QTA >  NTG-NTG.                                       
016200     IF NOT NO-QTA-IMP                                                    
016300         MOVE QTA-IMPEGNATA OF REC-SITPF TO QTA-8-COM                     
016400         MOVE F-QTA-IMP TO FLAG-COMODO                                    
016500         PERFORM VARIA-QTA THRU EX-VARIA-QTA                              
016600         MOVE QTA-8-COM TO QTA-IMPEGNATA OF REC-SITPF                     
016700         PERFORM VRF-IMP THRU EX-VRF-IMP                                  
016800                 VARYING I-QTA FROM 1 BY 1 UNTIL                          
016900                         I-QTA >  NTG-NTG.                                       
017000     IF NOT NO-QTA-IMP-C                                                  
017100         MOVE QTA-IMPEGNATA-C OF REC-SITPF TO QTA-8-COM                   
017200         MOVE F-QTA-IMP-C TO FLAG-COMODO                                  
017300         PERFORM VARIA-QTA THRU EX-VARIA-QTA                              
017400         MOVE QTA-8-COM TO QTA-IMPEGNATA-C OF REC-SITPF                   
017500         PERFORM VRF-IMP-C THRU EX-VRF-IMP-C                              
017600                 VARYING I-QTA FROM 1 BY 1 UNTIL                          
017700                         I-QTA >  NTG-NTG.                                       
017800 EX-MUOVI-QUANTITA.                                                       
017900     EXIT.                                                                
018000*                                                                         
018100*                                                                         
018200 CALCOLA-VALORE-MEDIO.                                                    
018300     PERFORM SOMMA-QTA-RECSITPF THRU                                      
018400                     EX-SOMMA-QTA-RECSITPF                                
018500             VARYING W-INDICE-1 FROM 1 BY 1                               
018600             UNTIL W-INDICE-1 >  NTG-NTG.                                        
018700     IF SOMMA-QTA-8-COMODO = 0                                            
018800         MOVE 0 TO VALORE OF PAR-SITPF                                    
018900     ELSE                                                                 
019000         COMPUTE VALORE-COMODO-1     =                                    
019100           VAL-GIAC OF REC-SITPF / SOMMA-QTA-8-COMODO                     
019200         MOVE VALORE-COMODO-1 TO VALORE OF PAR-SITPF.                     
019300 EX-CALCOLA-VALORE-MEDIO.                                                 
019400     EXIT.                                                                
019500*                                                                         
019600*                                                                         
019700 TTUPDATE.                                                                
019800          COPY PDBUPDAT.                                                  
019900*                                                                         
020000*                                                                         
020100 SOMMA-QTA-RECSITPF.                                                      
020200     ADD QTA-GIAC-PF OF REC-SITPF (W-INDICE-1) TO                         
020300              SOMMA-QTA-8-COMODO.                                         
020400 EX-SOMMA-QTA-RECSITPF.                                                   
020500     EXIT.                                                                
020600*                                                                         
020700*                                                                         
020800 TRATTA-VALORE.                                                           
020900     PERFORM SOMMA-QTA-PARPF THRU                                         
021000                  EX-SOMMA-QTA-PARPF                                      
021100         VARYING W-INDICE-1 FROM 1 BY 1                                   
021200         UNTIL W-INDICE-1 >  NTG-NTG.                                            
021300     COMPUTE VALORE-COMODO =                                              
021400         SOMMA-QTA-8-COMODO * VALORE OF PAR-SITPF.                        
021500*                                                                         
021600     IF SOMMA-QTA                                                         
021700         ADD VALORE-COMODO TO VAL-GIAC OF REC-SITPF                       
021800     ELSE                                                                 
021900         SUBTRACT VALORE-COMODO FROM VAL-GIAC OF REC-SITPF.               
022000 EX-TRATTA-VALORE.                                                        
022100     EXIT.                                                                
022200*                                                                         
022300*                                                                         
022400 SOMMA-QTA-PARPF.                                                         
022500     ADD QTA OF PAR-SITPF (W-INDICE-1) TO                                 
022600                SOMMA-QTA-8-COMODO.                                       
022700 EX-SOMMA-QTA-PARPF.                                                      
022800     EXIT.                                                                
022900*                                                                         
023000*                                                                         
023100 VARIA-QTA.                                                               
023200     PERFORM SOMMA-QUANTITA THRU EX-SOMMA-QUANTITA                        
023300         VARYING I-QTA FROM 1 BY 1                                        
023400         UNTIL I-QTA >  NTG-NTG.                                                 
023500 EX-VARIA-QTA.                                                            
023600     EXIT.                                                                
023700*                                                                         
023800*                                                                         
023900 SOMMA-QUANTITA.                                                          
024000     IF SOMMA-QTA                                                         
024100         ADD QTA OF PAR-SITPF (I-QTA) TO QTA-COM (I-QTA)                  
024200     ELSE                                                                 
024300         SUBTRACT QTA OF PAR-SITPF (I-QTA) FROM                           
024400                                   QTA-COM (I-QTA).                       
024500 EX-SOMMA-QUANTITA.                                                       
024600     EXIT.                                                                
024700*                                                                         
024800*                                                                         
024900 VRF-GIAC.                                                                
025000      IF QTA-GIAC-PF (I-QTA) < 0                                          
025010         DISPLAY "AGSITPFW   *******************"                         
025020         DISPLAY "forzo 0 in taglia " I-QTA                               
025030         DISPLAY "per C-MAT         " C-MAT OF PAR-SITPF         
               DISPLAY "per mag           " MAGAZZINO OF PAR-SITPF
025040         DISPLAY "QTA reale         " QTA-GIAC-PF (I-QTA)                 
025050         DISPLAY "           *******************"                         
025100         MOVE 0 TO QTA-GIAC-PF (I-QTA).                                   
025200 EX-VRF-GIAC.                                                             
025300      EXIT.                                                               
025400*                                                                         
025500*                                                                         
025600 VRF-ORD.                                                                 
025700      IF QTA-ORD (I-QTA) < 0                                              
025800         MOVE 0 TO QTA-ORD (I-QTA).                                       
025900 EX-VRF-ORD.                                                              
026000      EXIT.                                                               
026100*                                                                         
026200*                                                                         
026300 VRF-ORD-C.                                                               
026400      IF QTA-ORD-C (I-QTA) < 0                                            
026500         MOVE 0 TO QTA-ORD-C (I-QTA).                                     
026600 EX-VRF-ORD-C.                                                            
026700      EXIT.                                                               
026800*                                                                         
026900*                                                                         
027000 VRF-IMP.                                                                 
027100      IF QTA-IMP (I-QTA) > 0                                              
027200         MOVE 0 TO QTA-IMP (I-QTA).                                       
027300 EX-VRF-IMP.                                                              
027400      EXIT.                                                               
027500*                                                                         
027600*                                                                         
027700 VRF-IMP-C.                                                               
027800      IF QTA-IMP-C (I-QTA) > 0                                            
027900         MOVE 0 TO QTA-IMP-C (I-QTA).                                     
028000 EX-VRF-IMP-C.                                                            
028100      EXIT.                                                               
028200*                                                                         
028300*                                                                         
028400*                                                                         
028500*                                                                         
028600 TTDBPUT.                                                                 
028700           COPY PDBPUT.                                                   
028800*                                                                         
028900*      FINE PROGRAMMA    **** /K AGSITPFW.COB  *****                      
