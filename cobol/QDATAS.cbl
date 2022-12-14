001000*CONTROL USLINIT,DYNAMIC                                                  
001100 IDENTIFICATION DIVISION.                                                 
001200 PROGRAM-ID. QDATAS.                                                      
001300                                                                          
001310*2000*                                                                    
001311*     tratta date a 6 cifre                                               
001320*                                                                         
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 DATA DIVISION.                                                           
001700 WORKING-STORAGE SECTION.                                                 
001800                                                                          
001900 01  TAB-M.                                                               
002000     05      FILLER PIC X(5) VALUE "GEN31".                               
002100     05      FILLER PIC X(5) VALUE "FEB28".                               
002200     05      FILLER PIC X(5) VALUE "MAR31".                               
002300     05      FILLER PIC X(5) VALUE "APR30".                               
002400     05      FILLER PIC X(5) VALUE "MAG31".                               
002500     05      FILLER PIC X(5) VALUE "GIU30".                               
002600     05      FILLER PIC X(5) VALUE "LUG31".                               
002700     05      FILLER PIC X(5) VALUE "AGO31".                               
002800     05      FILLER PIC X(5) VALUE "SET30".                               
002900     05      FILLER PIC X(5) VALUE "OTT31".                               
003000     05      FILLER PIC X(5) VALUE "NOV30".                               
003100     05      FILLER PIC X(5) VALUE "DIC31".                               
003200                                                                          
003300 01  FILLER REDEFINES TAB-M.                                              
003400     05      FILLER OCCURS 12.                                            
003500             10      EL-MESE         PIC X(3).                            
003600             10      EL-GG-MESE      PIC   99.                            
003700                                                                          
003800 01  DATA-2          PIC 9(6).                                            
003900                                                                          
004000 01  DATA-2X REDEFINES DATA-2.                                            
004100     05      Q-AA    PIC 99.                                              
004200     05      Q-MM    PIC 99.                                              
004300     05      Q-GG    PIC 99.                                              
004400                                                                          
004500 01  IND     PIC S9(4) COMP.                                              
004600                                                                          
004700 01  QUO2    PIC S9(4) COMP.                                              
004800                                                                          
004900 01  RESTO   PIC S9(4) COMP.                                              
005000                                                                          
005010 01 GG-ANNO-0          PIC S9(11) COMP.                                   
005020                                                                          
005030 01 DOMENICA           PIC S9(4) COMP.                                    
005040                                                                          
005050 01 GIORNI             PIC S9(9) COMP.                                    
005060                                                                          
005100 LINKAGE SECTION.                                                         
005200 
      *conv
005300 01  PAR-DATA  COPY QPARGEN.  
      *conv                                     
005400 01  Q-DATA-E.                                                            
005500     05      Q-DATA.                                                      
005600             10      Q-GG    PIC 99.                                      
005700             10      Q-MM    PIC 99.                                      
005800             10      Q-AA    PIC 99.                                      
005900                                                                          
006000     05      Q-DATA-9 REDEFINES Q-DATA PIC 9(6).                          
006100     05      FILLER      PIC X(6).                                        
006200                                                                          
006300 01  FILLER REDEFINES Q-DATA-E.                                           
006400     05      Q-DATA-EE.                                                   
006500             10      Q-GG-S          PIC XX.                              
006600             10      Q-BAR-1         PIC X.                               
006700             10      Q-MM-S          PIC X(3).                            
006800             10      Q-BAR-2         PIC X.                               
006900             10      Q-AA-FISSO      PIC XX.                              
007000             10      Q-AA-VAR        PIC XX.                              
007100     05      FILLER  PIC X.                                               
007200                                                                          
007300 01  Q-DATA-I        PIC S9(8) COMP.                                      
007400                                                                          
007410 01 Q-SETTIMANA      PIC S9(4) COMP.                                      
007420                                                                          
007500 PROCEDURE DIVISION USING  PAR-DATA  Q-DATA-E  Q-DATA-I                   
007510                              Q-SETTIMANA.                                
007600                                                                          
007700 INIZIO.                                                                  
007800     MOVE 0 TO Q-STATO.                                                   
007900     EVALUATE Q-FUNZIONE                                                  
008000              WHEN 1 PERFORM TRT-1 THRU EX-TRT-1                          
008100              WHEN 2 PERFORM TRT-2 THRU EX-TRT-2                          
008200              WHEN 3 PERFORM TRT-3 THRU EX-TRT-3                          
008300              WHEN 4 PERFORM TRT-4 THRU EX-TRT-4                          
008400              WHEN OTHER                                                  
008500              MOVE -9 TO Q-STATO                                          
008600     END-EVALUATE.                                                        
008601     IF Q-FUNZIONE = 1 OR = 2                                             
008605        IF Q-STATO = 0                                                    
008610          PERFORM CALC-SETTIMANA THRU EX-CALC-SETTIMANA.                  
008700     EXIT PROGRAM.                                                        
008800                                                                          
008900 TRT-1.                                                                   
009000     IF Q-DATA NOT NUMERIC                                                
009100             MOVE -4 TO Q-STATO                                           
009200             GO TO EX-TRT-1.                                              
009300     IF Q-MM OF Q-DATA < 1 OR Q-MM OF Q-DATA > 12                         
009400             MOVE -1 TO Q-STATO                                           
009500             GO TO EX-TRT-1.                                              
009600     DIVIDE Q-AA OF Q-DATA BY 4 GIVING QUO2 REMAINDER RESTO.              
009700     MOVE Q-MM OF Q-DATA TO IND.                                          
009800     IF RESTO = 0  MOVE 29 TO EL-GG-MESE (2).                             
009900     IF Q-GG OF Q-DATA < 1 OR Q-GG OF Q-DATA > EL-GG-MESE (IND)           
010000             MOVE -2 TO Q-STATO                                           
010100             GO TO EX-TRT-1.                                              
010200     MOVE CORRESPONDING Q-DATA TO DATA-2X.                                
010300     MOVE DATA-2 TO Q-DATA-I.                                             
010400 EX-TRT-1.                                                                
010500     EXIT.                                                                
010600                                                                          
010700 TRT-2.                                                                   
010800     MOVE Q-DATA-I TO DATA-2 .  
      * Paolo 21/03/06 inserito il controllo sul mese perchè
      *                negli archivi ci sono delle date errate
009300     IF Q-MM OF DATA-2X < 1 OR Q-MM OF DATA-2X > 12                         
009400             MOVE -1 TO Q-STATO                                           
009500             GO TO EX-TRT-2.                                              

010801*2000*                                                                    
010810     MOVE SPACE TO Q-DATA-E.                                              
010900     MOVE CORRESPONDING DATA-2X TO Q-DATA.                                
010910     DIVIDE Q-AA OF Q-DATA BY 4 GIVING QUO2 REMAINDER RESTO.              
010920     IF RESTO = 0  MOVE 29 TO EL-GG-MESE (2).                             
011000 EX-TRT-2.                                                                
011100     EXIT.                                                                
011200                                                                          
011300 TRT-3.                                                                   
011400     MOVE Q-DATA-I TO DATA-2.                                             
011500     IF Q-MM OF DATA-2X < 1 OR                                            
011600        Q-MM OF DATA-2X > 12                                              
011700             MOVE -3 TO Q-STATO                                           
011800             GO TO EX-TRT-3.                                              
011900     MOVE Q-GG OF DATA-2X TO Q-GG-S.                                      
012000     MOVE "/"   TO Q-BAR-1                                                
012100                   Q-BAR-2.                                               
012200     MOVE Q-MM OF DATA-2X TO IND.                                         
012300     MOVE EL-MESE (IND) TO Q-MM-S.                                        
012310*2000*                                                                    
012320     IF Q-AA OF DATA-2X <= 50                                             
012330       MOVE "20" TO Q-AA-FISSO                                            
012340     ELSE                                                                 
012400     MOVE "19" TO Q-AA-FISSO.                                             
012500     MOVE Q-AA OF DATA-2X TO Q-AA-VAR.                                    
012600                                                                          
012700 EX-TRT-3.                                                                
012800     EXIT.                                                                
012900                                                                          
013000 TRT-4.                                                                   
013100     MOVE Q-DATA-I TO DATA-2.                                             
013200     IF Q-GG OF DATA-2X = 0                                               
013300             MOVE -4 TO Q-STATO                                           
013400             GO TO EX-TRT-4.                                              
013500     DIVIDE Q-AA OF DATA-2X BY 4 GIVING QUO2 REMAINDER RESTO.             
013600     IF RESTO = 0                                                         
013700             MOVE 29 TO EL-GG-MESE (2).                                   
013800     MOVE Q-MM OF DATA-2X TO IND.                                         
013900     PERFORM CALC THRU EX-CALC                                            
014000             UNTIL Q-GG OF DATA-2X NOT > EL-GG-MESE (IND).                
014100     MOVE IND TO Q-MM OF DATA-2X.                                         
014200     MOVE DATA-2 TO Q-DATA-I.                                             
014300 EX-TRT-4.                                                                
014400     EXIT.                                                                
014500                                                                          
014600 CALC.                                                                    
014700     SUBTRACT EL-GG-MESE (IND) FROM Q-GG OF DATA-2X.                      
014800     ADD 1 TO IND.                                                        
014900     IF IND > 12 PERFORM RICALC THRU EX-RICALC.                           
015000                                                                          
015100 EX-CALC.                                                                 
015200     EXIT.                                                                
015300                                                                          
015400 RICALC.                                                                  
015500     MOVE 1 TO IND.                                                       
015600     ADD 1 TO Q-AA OF DATA-2X.                                            
015700     DIVIDE Q-AA OF DATA-2X BY 4 GIVING QUO2 REMAINDER RESTO.             
015800     IF RESTO = 0                                                         
015900             MOVE 29 TO EL-GG-MESE (2)                                    
015910*2000*                                                                    
015920     ELSE                                                                 
015930             MOVE 28 TO EL-GG-MESE(2).                                    
016000 EX-RICALC.                                                               
016100     EXIT.                                                                
016200                                                                          
016300                                                                          
016400 CALC-SETTIMANA.                                                          
016410*2000*                                                                    
016420     IF Q-AA OF DATA-2X <= 50                                             
016430       COMPUTE GG-ANNO-0 = (1999 + Q-AA OF Q-DATA) * 365                  
016440                + (1999 + Q-AA OF Q-DATA) / 4                             
016450     ELSE                                                                 
016500     COMPUTE GG-ANNO-0 = (1899 + Q-AA OF Q-DATA) * 365                    
016600              + (1899 + Q-AA OF Q-DATA) / 4.                              
016700     MOVE 0 TO GIORNI.                                                    
016800     PERFORM SCORRI-MESI THRU EX-SCORRI-MESI                              
016900            VARYING IND FROM 1 BY 1                                       
017000              UNTIL IND = Q-MM OF Q-DATA.                                 
017100     ADD Q-GG OF Q-DATA TO GIORNI.                                        
017200     COMPUTE DOMENICA = GG-ANNO-0 - (GG-ANNO-0 / 7) * 7.                  
017300     IF DOMENICA = 0                                                      
017400        MOVE 1 TO DOMENICA                                                
017500       ELSE                                                               
017600          COMPUTE DOMENICA = 8 - DOMENICA.                                
017700     IF GIORNI > DOMENICA                                                 
017800        COMPUTE Q-SETTIMANA = (GIORNI - DOMENICA - 1) / 7 + 1             
017810*2000*                                                                    
017820        IF DOMENICA NOT = 1                                               
017900*       IF DOMENICA > 3                                                   
018000           ADD 1 TO Q-SETTIMANA                                           
018100           END-IF                                                         
018200       ELSE                                                               
018300          MOVE 1 TO Q-SETTIMANA.                                          
018400 EX-CALC-SETTIMANA.                                                       
018500     EXIT.                                                                
018600                                                                          
018700                                                                          
018800 SCORRI-MESI.                                                             
018900     ADD EL-GG-MESE (IND) TO GIORNI.                                      
019000 EX-SCORRI-MESI.                                                          
019100     EXIT.                                                                
019200                                                                          
019300                                                                          
