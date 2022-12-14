001100 IDENTIFICATION DIVISION.                                                 
001200 PROGRAM-ID. QTABEL.                                                      
001230 ENVIRONMENT DIVISION.                                                    
001260 CONFIGURATION SECTION.                                                   
001290 SOURCE-COMPUTER.  HP3000.                                                
001320 OBJECT-COMPUTER.  HP3000.                                                
001350 DATA DIVISION.                                                           
001380 WORKING-STORAGE SECTION.                                                 
001410*                                                                         
001440 01 SW1          PIC S9(4) COMP.                                          
001470 01 IT           PIC S9(4) COMP.                                          
001500 01 IT1          PIC S9(4) COMP.                                          
001530 01 J            PIC S9(4) COMP.                                          
001560 01 IMIN         PIC S9(4) COMP.                                          
001590 01 IMAX         PIC S9(4) COMP.                                          
001620 01 POS          PIC S9(9) COMP.                                          
001650 01 POS1         PIC S9(9) COMP.                                          
001680 01 PMIN         PIC S9(9) COMP.                                          
001710 01 PMAX         PIC S9(9) COMP.                                          
001740 01 LL-TAB       PIC S9(9) COMP.                                          
001770*                                                                         
001800 LINKAGE SECTION.                                                         
002000*                                                                         
002100 01 PAR-TAB.                                                              
002200  05 STATO       PIC S9(4) COMP.                                          
002300  05 N-EL-EFF    PIC 9(4) COMP.                                           
002400  05 N-EL-MAX    PIC 9(4) COMP.                                           
002500  05 LL-EL       PIC 9(4) COMP.                                           
002600  05 ADDR-K      PIC 9(4) COMP.                                           
002700  05 LL-K        PIC 9(4) COMP.                                           
002710  05 IND-EL      PIC 9(4) COMP.                                           
002720  05 FUNZ.                                                                
002730   10 FUNZ1      PIC X.                                                   
002740   10 FUNZ2      PIC X.                                                   
002800*                                                                         
002900 01 TAB          PIC X(100).                                              
003000*                                                                         
003100 01 ELEM         PIC X(10).                                               
003200*                                                                         
003300 PROCEDURE DIVISION USING PAR-TAB                                         
003400                          TAB                                             
003500                          ELEM.                                           
003600*                                                                         
003700 VIA.                                                                     
003800*              
004140     MOVE 0 TO STATO.                                                     
004141     IF FUNZ NOT = "K1" AND                                               
004142             NOT = "K2" AND                                               
004143             NOT = "K3" AND                                               
004144             NOT = "P1"                                                   
004145        MOVE -4 TO STATO                                                  
004146        GOBACK.                                                           
004156*??                                                                       
004160     IF FUNZ1 = "K"                                                       
004170        IF FUNZ2 = "1" AND                                                
004180           N-EL-EFF NOT < N-EL-MAX                                        
004400           MOVE -1 TO STATO                                               
004500           GOBACK.                                                        
004600*                                                                         
004700     IF FUNZ1 = "K"                                                       
004800        PERFORM TRATTA-K THRU EX-TRATTA-K                                 
004900       ELSE                                                               
005000        PERFORM TRATTA-P THRU EX-TRATTA-P.                                
005100*                                                                         
005200 FINE.                                                                    
005300     GOBACK.                                                              
005400*                                                                         
005500 TRATTA-K.                                                                
005600*                                                                         
005610     MOVE 0 TO SW1.                                                       
005700     MOVE 1 TO IMIN.                                                      
005800     MOVE N-EL-EFF TO IMAX.                                               
005900     IF IMAX = 0 AND                                                      
006000        FUNZ2 NOT = "1"                                                   
006100          MOVE -3 TO STATO                                                
006200          GOBACK.                                                         
006300     IF IMAX > 0                                                          
006400        PERFORM RICERCA THRU EX-RICERCA.                                  
006500*                                                                         
006510     IF FUNZ2 = "1"                                                       
006520        PERFORM INSERISCI THRU EX-INSERISCI                               
006530       ELSE                                                               
006540        IF SW1 NOT = 0                                                    
006550           MOVE -3 TO STATO                                               
006560           GOBACK                                                         
006570          ELSE                                                            
006600           IF FUNZ2 = "3"                                                 
006700              PERFORM CANCELLA THRU EX-CANCELLA                           
006710             ELSE                                                         
006720              MOVE IT TO IND-EL.                                          
006800*                                                                         
006900 EX-TRATTA-K.                                                             
007000     EXIT.                                                                
007100*                                                                         
007200 RICERCA.                                                                 
007300*                                                                         
007400     COMPUTE PMIN = ( IMIN - 1 ) * LL-EL + ADDR-K .                       
007410     COMPUTE PMAX = ( IMAX - 1 ) * LL-EL + ADDR-K .                       
007500     IF ELEM (ADDR-K:LL-K) = TAB (PMIN:LL-K)                              
007600        MOVE IMIN TO IT                                                   
007700        GO TO EX-RICERCA.                                                 
007710     IF ELEM (ADDR-K:LL-K) = TAB (PMAX:LL-K)                              
007720        MOVE IMAX TO IT                                                   
007730        GO TO EX-RICERCA.                                                 
007731*                                                                         
007740     IF ELEM (ADDR-K:LL-K) < TAB (PMIN:LL-K)                              
007750        MOVE IMIN TO IT                                                   
007751        MOVE 2 TO SW1                                                     
007760        GO TO EX-RICERCA.                                                 
007770     IF ELEM (ADDR-K:LL-K) > TAB (PMAX:LL-K)                              
007780        MOVE IMAX TO IT                                                   
007781        MOVE 3 TO SW1                                                     
007790        GO TO EX-RICERCA.                                                 
007800*                                                                         
007900     COMPUTE IT = ( IMIN + IMAX ) / 2 .                                   
008000     MOVE 0 TO IT1 .                                                      
008100     COMPUTE POS = ( IT - 1 ) * LL-EL + ADDR-K .                          
008200     PERFORM CERCA THRU EX-CERCA                                          
008300        UNTIL ELEM (ADDR-K:LL-K) = TAB (POS:LL-K) OR                      
008400              IT = IT1 .                                                  
008410*                                                                         
008420     IF ELEM (ADDR-K:LL-K) NOT = TAB (POS:LL-K)                           
008430        MOVE 1 TO SW1 .                                                   
008600*                                                                         
008700 EX-RICERCA.                                                              
008800     EXIT.                                                                
008900*                                                                         
009000 CERCA.                                                                   
009100     IF ELEM (ADDR-K:LL-K) > TAB (POS:LL-K)                               
009200        MOVE IT TO IMIN                                                   
009300       ELSE                                                               
009400        MOVE IT TO IMAX.                                                  
009500     MOVE IT TO IT1.                                                      
009510     COMPUTE IT = ( IMIN + IMAX ) / 2 .                                   
009530     COMPUTE POS = ( IT - 1 ) * LL-EL + ADDR-K .                          
009600 EX-CERCA.                                                                
009700     EXIT.                                                                
009800*                                                                         
009900 INSERISCI.                                                               
010000*                                                                         
010040     IF SW1 = 0  AND                                                      
010050        N-EL-EFF NOT = 0                                                  
010080        MOVE -2 TO STATO                                                  
010120        GOBACK.                                                           
010160*                                                                         
010200     IF N-EL-EFF = 0                                                      
010240        MOVE ELEM (1:LL-EL) TO TAB (1:LL-EL)                              
010280        MOVE 1 TO IND-EL                                                  
010320       ELSE                                                               
010360     IF SW1 = 2                                                           
010400*-----                                                                    
010440*       MULTIPLY N-EL-EFF BY LL-EL GIVING LL-TAB                          
010480*       COMPUTE POS = LL-EL + 1                                           
010520*       MOVE TAB (1:LL-TAB) TO TAB (POS:LL-TAB)                           
010560*-----                                                                    
010600        PERFORM VARYING J FROM N-EL-EFF BY -1                             
010640                UNTIL J < 1                                               
010680            COMPUTE POS  = ( J - 1 ) * LL-EL + 1                          
010720            COMPUTE POS1 = J * LL-EL + 1                                  
010760            MOVE TAB (POS:LL-EL) TO TAB (POS1:LL-EL)                      
010800        END-PERFORM                                                       
010840        MOVE ELEM (1:LL-EL) TO TAB (1:LL-EL)                              
011020        MOVE 1 TO IND-EL                                                  
011100       ELSE                                                               
011200     IF SW1 = 3                                                           
011300        COMPUTE POS = N-EL-EFF * LL-EL + 1                                
011400        MOVE ELEM (1:LL-EL) TO TAB (POS:LL-EL)                            
011500        ADD 1 , N-EL-EFF GIVING IND-EL                                    
011600       ELSE                                                               
011610*----                                                                     
011700*       COMPUTE LL-TAB = LL-EL * ( N-EL-EFF - IT )                        
011800*       COMPUTE POS  = IT * LL-EL + 1                                     
011900*       COMPUTE POS1 = ( IT + 1 ) * LL-EL + 1                             
012000*       MOVE TAB (POS:LL-TAB) TO TAB (POS1:LL-TAB)                        
012010*----                                                                     
012020        PERFORM VARYING J FROM N-EL-EFF BY -1                             
012030                UNTIL J = IT                                              
012040            COMPUTE POS  = ( J - 1 ) * LL-EL + 1                          
012050            COMPUTE POS1 = J * LL-EL + 1                                  
012060            MOVE TAB (POS:LL-EL) TO TAB (POS1:LL-EL)                      
012070        END-PERFORM                                                       
012100        MOVE ELEM (1:LL-EL) TO TAB (POS:LL-EL)                            
012110*       MOVE IT TO IND-EL.                                                
012120        COMPUTE IND-EL = IT + 1.                                          
012200*                                                                         
012300     ADD 1 TO N-EL-EFF.                                                   
012400*                                                                         
012500 EX-INSERISCI.                                                            
012600     EXIT.                                                                
012700*                                                                         
012800 CANCELLA.                                                                
012900*                                                                         
013000     IF IT NOT = N-EL-EFF                                                 
013010        COMPUTE LL-TAB = LL-EL * ( N-EL-EFF - IT )                        
013020        COMPUTE POS  = IT * LL-EL + 1                                     
013030        COMPUTE POS1 = ( IT - 1 ) * LL-EL + 1                             
013040        MOVE TAB (POS:LL-TAB) TO TAB (POS1:LL-TAB).                       
013100*-- ?? blanck in fondo --                                                 
013200     SUBTRACT 1 FROM N-EL-EFF.                                            
013300*                                                                         
013400 EX-CANCELLA.                                                             
013500     EXIT.                                                                
013600*                                                                         
013700 TRATTA-P.                                                                
013800*                                                                         
013900     IF IND-EL < 1 OR                                                     
014000        IND-EL > N-EL-EFF                                                 
014100        MOVE -1 TO STATO                                                  
014200        GOBACK.                                                           
014300*                                                                         
014400     MOVE IND-EL TO IT.                                                   
014500     PERFORM CANCELLA THRU EX-CANCELLA.                                   
014600*                                                                         
014700 EX-TRATTA-P.                                                             
014800     EXIT.                                                                
014900*                                                                         
