       IDENTIFICATION DIVISION.
       PROGRAM-ID. Lab5.
      * DO_1: Complete the following information.  
       AUTHOR.  
       DATE-WRITTEN. 
      ******************************************************************
      * Purpose:	  
      *     Learn how to use ADD, SUBTRACT, MULTIPLY, DIVIDE,
      *                      STRING, UNSTRING, INSPECT and SORT.
      *          
      * Input:
      *     LAB5.dat
      * Output:
      *     LAB5_SORTED.DAT
      ****************************************************************** 	   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL.
      ******************************************************************	   
      * DO_2: Define the files for SORT. 
      ****************************************************************** 
           SELECT  
           SELECT  
                
           SELECT  
                
       DATA DIVISION.
      ******************************************************************	   
      * DO_3: Define SD entry.     
      ******************************************************************  
       SD  


	   
       FD  IN-FILE.
       01  IN-REC           PIC X(80).
       FD  OUT-FILE.
       01  OUT-REC          PIC X(80).
       WORKING-STORAGE SECTION.
       01  WORKING-ITEMS.
           05  FIRST-NAME  PIC X(30) VALUE 'TOM'.
           05  MIDDLE-NAME PIC X(30) VALUE 'MICHAEL'.
           05  LAST-NAME   PIC X(30) VALUE 'CRUISE'.
           05  FULL-NAME   PIC X(40).
           05  FULL-ADDR   PIC X(60) VALUE 
                           '65 WEST ST/MIDDLETOWN/NY/10098/'.	
   	       05  STREET      PIC X(20).
	       05  CITY        PIC X(10). 
	       05  STATE       PIC X(2).
	       05  ZIP         PIC X(5).
           05  NUMBER1     PIC 9(5)  VALUE 10050.
           05  NUMBER2     PIC 9(5)  VALUE 00157.
           05  TEL-NO      PIC X(12) VALUE '608/342/5515'.
           05  COUNT1      PIC 9.
           05  COUNT2      PIC 9.
           05  A           PIC 99    VALUE 2.
           05  B           PIC 99    VALUE 4.
           05  C           PIC 99    VALUE 8.
           05  D           PIC 99    VALUE 3.
           05  F           PIC 99    VALUE 10.
           05  RESULT      PIC S99V99.
           05  TEMP        PIC S99V99.
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-STRING THRU 500-SORT.
           STOP RUN.
      ******************************************************************	   
      * DO_4: Question 1.
      ******************************************************************	   
       100-STRING.
           DISPLAY LAST-NAME FIRST-NAME MIDDLE-NAME
           STRING 
                
				
				
           END-STRING
           DISPLAY FULL-NAME. 
      ******************************************************************	   
      * DO_5: Question 2.
      ******************************************************************	   
       200-UNSTRING.
           DISPLAY FULL-ADDR
           UNSTRING  
                
				
				
           END-UNSTRING
           DISPLAY STREET CITY STATE ZIP.   
      ******************************************************************	   
      * DO_6: Question 3.
      ******************************************************************		   
       300-INSPECT.
           DISPLAY NUMBER1
           INSPECT  
           DISPLAY COUNT1
           INSPECT   
           DISPLAY NUMBER1
           INSPECT  
           DISPLAY COUNT2
           INSPECT  
           DISPLAY TEL-NO.
      ******************************************************************	   
      * DO_7: Question 4.
      ******************************************************************	   
       400-ARITHMETIC-VERBS.
           DISPLAY RESULT A B C D F 
            
			
			
           DISPLAY RESULT A B C D F.   
      ******************************************************************	   
      * DO_8: Question 5.
      ******************************************************************		   
       500-SORT.
           
               
               
              DISPLAY SPACE
              DISPLAY "DONE!".
          