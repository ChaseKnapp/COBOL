       IDENTIFICATION DIVISION.
       PROGRAM-ID. Lab7.
      * DO_1: Complete the following information.   
       AUTHOR.  
       DATE-WRITTEN. 
      ******************************************************************
      * Purpose:	  
      *     Learn  
      *       1. How to use OCCURS to define arrays and tables
      *       2. How to use PERFORM VARYING with subscript/index
      *       3. How to use SEARCH to look up a table
      * Input:
      *     1. lab7.dat
      *     2. Prompt the user to enter a 2-character department code
      * Output:
      *     1. Display tax rate table (compile-time table)
      *     2. Display department code table (runtime table)
      *     3. Display the department name after the user entered
      *        a department code.
      ******************************************************************  
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEPT-TABLE ASSIGN TO "lab7.dat"
               ORGANIZATION IS LINE SEQUENTIAL. 
       DATA DIVISION.
       FD  DEPT-TABLE.
       01  DEPT-REC.
           05  D-CODE   PIC X(2).
           05  D-NAME   PIC X(10).
       WORKING-STORAGE SECTION.
      ******************************************************************	   
      * DO_2: Define the compile-time table and a SUBSCRIPT for
      *       accessing the table	  
      ******************************************************************
      01  TAX-TABLE. 
	   

      ******************************************************************	   
      * DO_3: Define the runtime table with an INDEX for accesing 
      *       the table.
      ******************************************************************			   
       01  WS-DEPT-TABLE.
            
			
			
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-COMPILE-TIME-TABLE.
           OPEN INPUT DEPT-TABLE
           PERFORM 200-RUN-TIME-TABLE.
           CLOSE DEPT-TABLE
           STOP RUN.
      ******************************************************************	   
      * DO_4: Use PERFORM VARYING with the SUBSCRIPT to DISPLAY the
      *       compile-time table.
      ******************************************************************		   
       100-COMPILE-TIME-TABLE.    
      
       
       200-RUN-TIME-TABLE.
           PERFORM 210-LOAD-DEPT-TABLE
           PERFORM 220-DISPLAY-DEPT-TABLE
           DISPLAY 'ENTER A DEPARTMENT CODE'
           ACCEPT WS-CODE
           PERFORM 230-LOOKUP-DEPT.
      ******************************************************************	   
      * DO_5: Use PERFORM VARYING with the INDEX to LOAD the runtime
      *       table.	  
      ******************************************************************	 	   
       210-LOAD-DEPT-TABLE.
            
			
			
      ******************************************************************	   
      * DO_6: Use PERFORM VARYING with the INDEX to DISPLAY the table 
      ******************************************************************	 		
       220-DISPLAY-DEPT-TABLE.
           PERFORM VARYING    
               DISPLAY  
           END-PERFORM.
      ******************************************************************	   
      * DO_7: Use a SEARCH statement with the INDEX to look up the
      *       department table and find the appropriate department name
      ******************************************************************	 	   
       230-LOOKUP-DEPT.
            
           SEARCH WS-DEPT-REC 
               AT END
                    
               WHEN  
                    
           END-SEARCH.