001020 IDENTIFICATION DIVISION.                                                 
001040 PROGRAM-ID. QSTRINGV.                                                    
001041*                                                                         
001042*2000*        07/07/99                                                    
001043*     tratta date a 6 cifre                                               
001044*                                                                         
001045*EURO*        18/10/00                          EURO/LIRE                 
001046*     trattamento importi in EURO                                         
001047*                                                                         
001060 ENVIRONMENT DIVISION.                                                    
001080 CONFIGURATION SECTION.                                                   
001100 SOURCE-COMPUTER.  HP3000.                                                
001120 OBJECT-COMPUTER.  HP3000.                                                
001140 DATA DIVISION.                                                           
001160 WORKING-STORAGE SECTION.                                                 
001180*                                                                         
001200 01 LL-COMO         PIC S9(4) COMP.                                       
001210 01 PUNT            PIC S9(4) COMP.                                       
001300 01 PUNT-1          PIC S9(4) COMP.                                       
001301 01 PUNT-2          PIC S9(4) COMP.                                       
001302 01 PUNT-N          PIC S9(4) COMP.                                       
001320 01 IND             PIC S9(4) COMP.                                       
001440*                                                                         
001650 LINKAGE SECTION.                                                         
001700*                                                                         
001750 01 PARAM.                                                                
001800  05 STATO          PIC S9(4) COMP.                                       
001850  05 FUNZ           PIC S9(4) COMP.                                       
001900  05 LL-STR         PIC S9(4) COMP.                                       
001950  05 LL-SUB         PIC S9(4) COMP.                                       
002000  05 P-4            PIC S9(4) COMP.                                       
002050  05 P-5            PIC S9(4) COMP.                                       
002060  05 P-6            PIC S9(4) COMP.                                       
002100  05 TIPO-OP-SEP.                                                         
002150   10 TIPO-OP       PIC X.                                                
002160   10 SEP           PIC X.                                                
002200*                                                                         
002250 01 STRINGA         PIC X(10).                                            
002300 01 SUB-STRINGA     PIC X(10).                                            
002350*                                                                         
002400 PROCEDURE DIVISION USING PARAM                                           
002450                          STRINGA                                         
002500                          SUB-STRINGA.                                    
002550*                                                                         
002600 VIA.                                                                     
002700*                                                                         
002750     MOVE 0 TO STATO.                                                     
002800*                                                                         
002850     IF FUNZ = 0                                                          
002900        PERFORM TR-FUNZ-0 THRU EX-TR-FUNZ-0                               
002950       ELSE                                                               
003000        PERFORM TR-FUNZ-N THRU EX-TR-FUNZ-N.                              
003050*                                                                         
003100*                                                                         
003150     GOBACK.                                                              
003200*                                                                         
003250 TR-FUNZ-0.                                                               
003300*                                                                         
003400     PERFORM VARYING IND FROM LL-STR  BY -1                               
003450        UNTIL   IND = 0 OR                                                
003500                STRINGA (IND:1) = SEP                                     
003510        CONTINUE                                                          
003520     END-PERFORM.                                                         
003550     ADD 1 , IND GIVING PUNT .                                            
003570     PERFORM VARYING IND FROM LL-SUB  BY -1                               
003580        UNTIL   IND = 0 OR                                                
003590                SUB-STRINGA (IND:1) NOT = SPACE                           
003591        CONTINUE                                                          
003592     END-PERFORM.                                                         
003600     MOVE IND TO LL-COMO.                                                 
003650     IF ( LL-STR - PUNT + 1 ) NOT > LL-COMO                               
003700        MOVE -1 TO STATO                                                  
003750        GOBACK.                                                           
003760     IF LL-COMO > 0                                                       
003800        MOVE SUB-STRINGA (1:LL-COMO) TO                                   
003850                 STRINGA (PUNT:LL-COMO)                                   
003950        ADD LL-COMO TO PUNT                                               
004050        MOVE SEP TO STRINGA (PUNT:1).                                     
004930*                                                                         
005100 EX-TR-FUNZ-0.                                                            
005800     EXIT.                                                                
005900*                                                                         
005950 TR-FUNZ-N.                                                               
006000*                                                                         
006250     MOVE SEP TO STRINGA (LL-STR:1).                                      
006300     MOVE 1 TO IND.                                                       
006310     MOVE 1 TO PUNT-N.                                                    
006350     PERFORM VARYING PUNT FROM 1 BY 1                                     
006450             UNTIL PUNT > LL-STR  OR                                      
006500                   IND  > FUNZ                                            
006600        IF STRINGA (PUNT:1) = SEP                                         
006610           MOVE PUNT-N TO PUNT-1                                          
006611           SUBTRACT 1 FROM PUNT GIVING PUNT-2                             
006620           ADD 1 , PUNT GIVING PUNT-N                                     
006650           ADD 1 TO IND                                                   
006700        END-IF                                                            
006800     END-PERFORM.                                                         
006820     MOVE SPACE TO STRINGA (LL-STR:1).                                    
006840     IF IND NOT > FUNZ                                                    
006860        MOVE -2 TO STATO                                                  
006880        GOBACK.                                                           
006900     PERFORM VARYING PUNT FROM PUNT-2 BY -1                               
006920        UNTIL   PUNT NOT > PUNT-1 OR                                      
006940                STRINGA (PUNT:1) NOT = SPACE                              
006941        CONTINUE                                                          
006942     END-PERFORM.                                                         
006960     MOVE PUNT TO PUNT-2.                                                 
006980     PERFORM VARYING PUNT FROM PUNT-1 BY 1                                
007000        UNTIL   PUNT NOT < PUNT-2 OR                                      
007020                STRINGA (PUNT:1) NOT = SPACE                              
007021        CONTINUE                                                          
007022     END-PERFORM.                                                         
007030     MOVE PUNT TO PUNT-1.                                                 
007040     COMPUTE LL-COMO = PUNT-2 - PUNT-1 + 1 .                              
007080     IF LL-COMO > LL-SUB                                                  
007100        MOVE -2 TO STATO                                                  
007120        GOBACK.                                                           
007240*                                                                         
007260     IF LL-COMO > 0                                                       
007280        MOVE STRINGA (PUNT-1:LL-COMO) TO SUB-STRINGA (1:LL-SUB)           
007310       ELSE                                                               
007320        MOVE SPACE TO SUB-STRINGA (1:LL-SUB).                             
007400*                                                                         
007500 EX-TR-FUNZ-N.                                                            
007600     EXIT.                                                                
007700*                                                                         
008100*                                                                         
