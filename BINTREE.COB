000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. BINTREE.                                                     
000300*                                                                         
000400*                                                                         
000500*     LOAD A TABLE WHILE BUILDING A BINARY TREE STRUCTURE.                
000600*                                                                         
000700*     THE TABLE CAN BE SEARCHED OR TRAVERSED IN AN ASCENDING ORDER.       
000800*                                                                         
000900*     THE SEARCH IS MORE EFFICIENT THAN THE SEARCH VERB IF THE TABLE HAS  
001000*     MORE THAN 32 ENTRIES.                                               
001100*                                                                         
001200*                                                                         
001300*                                                                         
001400*                                                                         
001500*                                                                         
001600*                                                                         
001700 ENVIRONMENT DIVISION.                                                    
001800 CONFIGURATION SECTION.                                                   
001900*                                                                         
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200     SELECT PRNTFILE         ASSIGN TO PRINT.                             
002300*                                                                         
002400*                                                                         
002500*                                                                         
002600*                                                                         
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900*                                                                         
003000 FD  PRNTFILE.                                                            
003100*                                                                         
003200 01  PRT-REC                 PIC  X(133).                                 
003300*                                                                         
003400*                                                                         
003500 WORKING-STORAGE SECTION.                                                 
003600*                                                                         
003700 01  WORK-AREA.                                                           
003800     03  SS                  PIC S9(04)      COMP                         
003900                                             VALUE ZERO.                  
004000     03  SS2                 PIC S9(04)      COMP                         
004100                                             VALUE ZERO.                  
004200     03  AVAIL               PIC S9(04)      COMP                         
004300                                             VALUE ZERO.                  
004400     03  WS-CTR              PIC S9(04)      COMP                         
004500                                             VALUE ZERO.                  
004600     03  WS-ENTRY            PIC X(10)       VALUE SPACE.                 
004700     03  WS-FIND             PIC X(10)       VALUE SPACE.                 
004800*                                                                         
004900*                                                                         
005000 01  INITIALIZE-TABLE.                                                    
005100     03  INIT-TABLE.                                                      
005200         05  FILLER          PIC S9(03)      COMP-3   VALUE ZERO.         
005300         05  FILLER          PIC S9(03)      COMP-3   VALUE ZERO.         
005400         05  FILLER          PIC S9(03)      COMP-3   VALUE ZERO.         
005500         05  FILLER          PIC  X(10)               VALUE SPACE.        
005600     03  TABLE-AREA.                                                      
005700         05  TABLE-ENTRY     OCCURS 10 TIMES.                             
005800             07  TBL-LL      PIC S9(03)      COMP-3.                      
005900             07  TBL-RL      PIC S9(03)      COMP-3.                      
006000             07  TBL-LINK    PIC S9(03)      COMP-3.                      
006100             07  TBL-ITEM    PIC  X(10).                                  
006200*                                                                         
006300*                                                                         
006400 PROCEDURE DIVISION.                                                      
006500*                                                                         
006600 0000-START.                                                              
006700     OPEN OUTPUT PRNTFILE.                                                
006800     MOVE INITIALIZE-TABLE   TO TABLE-AREA.                               
006900*                                                                         
007000*    LOAD TABLE. AFTER 10 ENTRIES GO TO THE SEARCH ROUTINE.               
007100*    TO USE THE TABLE SORT FUNCTION, CHANGE 'GO TO FIND-ACCEPT'           
007200*    TO 'GO TO TEST-LEFT'.                                                
007300*                                                                         
007400 ACCEPT-INPUT.                                                            
007500     IF  AVAIL > 10                                                       
007600         MOVE 1              TO SS, SS2                                   
007700         GO TO FIND-ACCEPT.                                               
007800*                                                                         
007900     ACCEPT WS-ENTRY.
008000*                                                                         
008100     IF  SS = ZERO                                                        
008200         MOVE WS-ENTRY       TO TBL-ITEM (1)                              
008300         MOVE ZERO           TO TBL-LL   (1)                              
008400         MOVE ZERO           TO TBL-RL   (1)                              
008500         ADD 2               TO AVAIL.                                    
008600*                                                                         
008700     MOVE 1                  TO SS.
008800*                                                                         
008900 BUILD-TABLE.                                                             
009000      IF  WS-ENTRY < TBL-ITEM (SS)
009100          IF  TBL-LL (SS) = ZERO
009200              MOVE WS-ENTRY  TO TBL-ITEM (AVAIL)
009300              MOVE AVAIL     TO TBL-LL   (SS)
009400              MOVE ZERO      TO TBL-LL   (AVAIL), TBL-RL (AVAIL)
009500              ADD 1          TO AVAIL
009600              GO TO ACCEPT-INPUT
009700          ELSE
009800              MOVE TBL-LL (SS)
009900                             TO SS                                        
010000              GO TO BUILD-TABLE.
010100*
010200      IF  WS-ENTRY > TBL-ITEM (SS)
010300          IF  TBL-RL (SS) = ZERO                                          
010400              MOVE WS-ENTRY  TO TBL-ITEM (AVAIL)                          
010500              MOVE AVAIL     TO TBL-RL   (SS)                             
010600              MOVE ZERO      TO TBL-LL   (AVAIL), TBL-RL (AVAIL)          
010700              ADD 1          TO AVAIL                                     
010800              GO TO ACCEPT-INPUT                                          
010900          ELSE                                                            
011000              MOVE TBL-RL (SS)                                            
011100                             TO SS                                        
011200              GO TO BUILD-TABLE.                                          
011300*
011400     GO TO ACCEPT-INPUT.
011500*
011600*    UNLOAD TABLE IN ASCENDING ORDER
011700*
011800 TEST-LEFT.
011900     IF  TBL-LL (SS) NOT = ZERO
012000         MOVE SS             TO TBL-LINK (SS2)
012100         MOVE TBL-LL (SS)    TO SS
012200         ADD 1               TO SS2
012300         GO TO TEST-LEFT.
012400*
012500 PRINT-HERE.                                                              
012600     MOVE TBL-ITEM (SS)      TO PRT-REC.                                  
012700     WRITE PRT-REC AFTER ADVANCING 2 LINES.                               
012800     ADD 1                   TO WS-CTR.                                   
012900     IF  WS-CTR > 9                                                       
013000         GO TO END-JOB.                                                   
013100*                                                                         
013200 TEST-RIGHT.                                                              
013300     IF  TBL-RL (SS) NOT = ZERO                                           
013400         MOVE TBL-RL (SS)    TO SS                                        
013500         GO TO TEST-LEFT.                                                 
013600*                                                                         
013700     SUBTRACT 1 FROM SS2.                                                 
013800     MOVE TBL-LINK (SS2)     TO SS.                                       
013900     GO TO PRINT-HERE.                                                    
014000*                                                                         
014100*    SEARCH THE TABLE                                                     
014200*                                                                         
014300 FIND-ACCEPT.
014400     ACCEPT WS-FIND.                                                      
014500     IF  WS-FIND = 'END'                                                  
014600         GO TO END-JOB.                                                   
014700*                                                                         
014800     MOVE 1                  TO SS.                                       
014900*                                                                         
015000 FIND-RTN.                                                                
015100     IF  WS-FIND < TBL-ITEM (SS)                                          
015200         IF  TBL-LL (SS) = ZERO                                           
015300             DISPLAY 'NOT IN TABLE'                                       
015400             GO TO FIND-ACCEPT                                            
015500         ELSE                                                             
015600             MOVE TBL-LL (SS)                                             
015700                             TO SS                                        
015800             GO TO FIND-RTN.                                              
015900*                                                                         
016000     IF  WS-FIND > TBL-ITEM (SS)                                          
016100         IF  TBL-RL (SS) = ZERO                                           
016200             DISPLAY 'NOT IN TABLE'                                       
016300             GO TO FIND-ACCEPT                                            
016400         ELSE                                                             
016500             MOVE TBL-RL (SS)                                             
016600                             TO SS                                        
016700             GO TO FIND-RTN.                                              
016800*                                                                         
016900     DISPLAY 'FOUND ' TBL-ITEM (SS).                                      
017000*                                                                         
017100 END-JOB.
017200     CLOSE PRNTFILE.                                                      
017300     STOP RUN.                                                            
017400*                                                                         
017500*                                                                         
017600*    END OF BINTREE.                                                      
