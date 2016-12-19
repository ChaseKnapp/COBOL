       IDENTIFICATION DIVISION.
       PROGRAM-ID. Lab7.
      * DO_1: Complete the following information.   
       AUTHOR.  Chase Knapp
       DATE-WRITTEN. 4/7/2015
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
       01  TAX-TABLE VALUE '1000060200006530000704000080'.
	       05  TAX-REC OCCURS 4 TIMES.
               10  T-WAGES       PIC 9(4).
               10  T-RATE        PIC V999.

      ******************************************************************	   
      * DO_3: Define the runtime table with an INDEX for accesing 
      *       the table.
      ******************************************************************			   
       01  WS-DEPT-TABLE.
           05  WS-DEPT-REC OCCURS 25 TIMES INDEXED BY INDX.
               10  WS-DEPT-CODE  PIC X(2).
               10  WS-DEPT-NAME  PIC X(10).
       01  SUB                   PIC 9(2).
       01  NO-DEPT               PIC 9(2)    VALUE 25.
       01  WS-CODE               PIC X(2).
       01  E-WAGES               PIC Z,999.99.
       01  E-RATE                PIC Z9.99.
       01  PERCENTAGE            PIC 9(3)    VALUE 100.
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
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               MOVE T-WAGES(SUB) TO E-WAGES
               COMPUTE E-RATE = T-RATE(SUB) * PERCENTAGE
               DISPLAY E-WAGES ' ' E-RATE '%'
           END-PERFORM. 
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
           PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > 25
               READ DEPT-TABLE
			       AT END
                       CLOSE DEPT-TABLE
                   NOT AT END
                       MOVE D-CODE TO WS-DEPT-CODE(INDX)
                       MOVE D-NAME TO WS-DEPT-NAME(INDX)
               END-READ
           END-PERFORM.
      ******************************************************************	   
      * DO_6: Use PERFORM VARYING with the INDEX to DISPLAY the table 
      ******************************************************************	 		
       220-DISPLAY-DEPT-TABLE.
           PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > NO-DEPT
               DISPLAY  WS-DEPT-CODE(INDX) ' ' WS-DEPT-NAME(INDX)
           END-PERFORM.
      ******************************************************************	   
      * DO_7: Use a SEARCH statement with the INDEX to look up the
      *       department table and find the appropriate department name
      ******************************************************************	 	   
       230-LOOKUP-DEPT.
           SET INDX TO 1
           SEARCH WS-DEPT-REC 
               AT END
                   DISPLAY 'NOT FOUND!'
               WHEN  WS-DEPT-CODE(INDX) = WS-CODE
                   DISPLAY WS-DEPT-NAME(INDX)
           END-SEARCH.