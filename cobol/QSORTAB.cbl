001000*CONTROL USLINIT,DYNAMIC                                                  
001100 IDENTIFICATION DIVISION.                                                 
001200 PROGRAM-ID. QSORTAB.                                                     
001300 ENVIRONMENT DIVISION.                                                    
001400 CONFIGURATION SECTION.                                                   
001500 SOURCE-COMPUTER.  HP3000.                                                
001600 OBJECT-COMPUTER.  HP3000.                                                
001700 DATA DIVISION.                                                           
001800 WORKING-STORAGE SECTION.                                                 
001900*                                                                         
002000 01 COMO        PIC X(1000).                                               
002100*                                                                         
002200 01 I           PIC S9(4) COMP.                                           
002300 01 J           PIC S9(4) COMP.                                           
002400 01 POS         PIC S9(9) COMP.                                           
002500 01 POS1        PIC S9(9) COMP.                                           
002600 01 POSK        PIC S9(9) COMP.                                           
002700 01 POSK1       PIC S9(9) COMP.                                           
002800*                                                                         
002900 LINKAGE SECTION.                                                         
003000*                                                                         
003100 01 PAR-SORT.                                                             
003200  05 STATO       PIC S9(4) COMP.                                          
003300  05 N-EL-EFF    PIC 9(4) COMP.                                           
003400  05 N-EL-MAX    PIC 9(4) COMP.                                           
003500  05 LL-EL       PIC 9(4) COMP.                                           
003600  05 ADDR-K      PIC 9(4) COMP.                                           
003700  05 LL-K        PIC 9(4) COMP.                                           
003800  05 IND-EL      PIC 9(4) COMP.                                           
003900  05 FILLER      PIC XX.                                                  
004000*                                                                         
004100 01 TAB          PIC X(50000).                                             
004200*                                                                         
004300 PROCEDURE DIVISION USING PAR-SORT                                        
004400                          TAB.                                            
004500*                                                                         
004600 VIA.                                                                     
004700*                                                                         
004800     MOVE 0 TO STATO.                                                     
004900     PERFORM LOOP1 THRU EX-LOOP1                                          
005000         VARYING I FROM 2 BY 1 UNTIL I > N-EL-EFF.                        
005100*                                                                         
005200 FINE.                                                                    
005300     GOBACK.                                                              
005400*                                                                         
005500 LOOP1.                                                                   
005600*                                                                         
005700     PERFORM LOOP2 THRU EX-LOOP2                                          
005800         VARYING J FROM I BY -1 UNTIL J NOT > 1 .                         
005900*                                                                         
006000 EX-LOOP1.                                                                
006100     EXIT.                                                                
006200*                                                                         
006300 LOOP2.                                                                   
006400*                                                                         
006500     COMPUTE POS = ( J - 1 ) * LL-EL .                                    
006600     ADD ADDR-K , POS GIVING POSK.                                        
006700     SUBTRACT LL-EL FROM POSK GIVING POSK1 .                              
006800     IF TAB (POSK1:LL-K) = TAB (POSK:LL-K)                                
006900        MOVE -1 TO STATO                                                  
007000        GO TO EX-LOOP2.                                                   
007100     IF TAB (POSK1:LL-K) < TAB (POSK:LL-K)                                
007200        MOVE 1 TO J                                                       
007300       ELSE                                                               
007400        ADD 1 TO POS                                                      
007500        SUBTRACT LL-EL FROM POS GIVING POS1                               
007600        MOVE TAB (POS1:LL-EL) TO COMO (1:LL-EL)                           
007700        MOVE TAB (POS:LL-EL)  TO TAB (POS1:LL-EL)                         
007800        MOVE COMO (1:LL-EL)   TO TAB (POS:LL-EL).                         
007900*                                                                         
008000 EX-LOOP2.                                                                
008100     EXIT.                                                                
008200*                                                                         
