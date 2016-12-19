       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXAM2.
      ******************************************************************
      * Type your full name below. -1 point if it is not done.
      ****************************************************************** 
       AUTHOR. CHASE KNAPP
       DATE-WRITTEN.  3/10/2015
      ******************************************************************
      * Purpose:	  
      *     This program generates a report file.
      *     
      * Input:  exam2P2.dat
      *      
      * Output: exam2P2.rpt     
      ****************************************************************** 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.	  	 
	  ******************************************************************
      * Question 1: Complete the SELECT statement for Input/Output files
      * (1 points)
      ****************************************************************** 
       FILE-CONTROL.
           SELECT  INPUT-FILE
               ASSIGN TO "exam2P2.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  OUTPUT-FILE 
               ASSIGN TO "exam2P2.rpt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
      ******************************************************************
      * Question 2: Define the record layout for input/output files.
      * (3 points)
      ******************************************************************	 
       FILE SECTION.
       FD INPUT-FILE.
       01  INPUT-FILES.
           05  IN-INITIAL1   PIC X.
           05  IN-INITIAL2   PIC X.
	       05  IN-LNAME      PIC X(10).
           05  IN-MONTH      PIC XX.
	       05  IN-YEAR       PIC X(4).
	       05  IN-AMOUNT     PIC 9(6).
       FD OUTPUT-FILE.
	   01  OUT-REC.
           05  FILE-OUT-REC  PIC X(80).
       WORKING-STORAGE SECTION.
       77  EOF               PIC X VALUE 'N'.
       01  WS-DATE.
           05  WS-YEAR       PIC 9(4).
           05  WS-MM         PIC 99.
           05  WS-DD         PIC 99.      
       01  TOTAL             PIC 9(7)  VALUE 0.
       01  HEADING1.
           05  FILLER        PIC X(30) VALUE SPACE.
           05  FILLER        PIC X(18) VALUE "TRANSACTION REPORT".
           05  FILLER        PIC X(32) VALUE SPACE.
       01  HEADING2.
           05  FILLER         PIC X(31) VALUE SPACE.
           05  FILLER        PIC X(6)  VALUE 'DATE: '.          
           05  H-DATE.
               07  H-MONTH    PIC 99.
               07  FILLER     PIC X     VALUE "/".
               07  H-DAY      PIC 99.
               07  FILLER     PIC X     VALUE "/".
               07  H-YEAR     PIC 9(4).
           05  FILLER         PIC X(33) VALUE SPACE.
      ******************************************************************
      * Question 3: Define HEADING3, which prints your name.
      * (1 points)
      ****************************************************************** 
       01  HEADING3.
	       05  FILLER         PIC X(32) VALUE SPACE.
           05  FILLER         PIC XXX   VALUE 'BY:'.
           05  FILLER         PIC X(11) VALUE 'CHASE KNAPP'.
           05  FILLER         PIC X(34) VALUE SPACE.
       01  HEADING4.
           05  FILLER         PIC X(6)  VALUE SPACE.
           05  FILLER         PIC X(4)  VALUE 'NAME'.
           05  FILLER         PIC X(8)  VALUE SPACE.
           05  FILLER         PIC X(19) VALUE 'DATE OF TRANSACTION'.
           05  FILLER         PIC X(5)  VALUE SPACE.
           05  FILLER         PIC X(21) VALUE 'AMOUNT OF TRANSACTION'.
           05  FILLER         PIC X(17) VALUE SPACE.     
      ******************************************************************
      * Question 4: Define the print record details.
      * (3 points)
      ****************************************************************** 
       01  RECORD-DETAILS.
           05  FILLER         PIC X     VALUE SPACE.
           05  LNAME          PIC X(10).
           05  FILLER         PIC X     VALUE SPACE.
           05  INITIAL1       PIC X.
           05  FILLER         PIC X     VALUE SPACE.
           05  INITIAL2       PIC X.
           05  FILLER         PIC X(8)  VALUE SPACE. 
           05  YEAR           PIC 9(4).
           05  FILLER         PIC X     VALUE '/'.
           05  MONTH          PIC 99.
           05  FILLER         PIC X(19) VALUE SPACE.
           05  TRANS-AMOUNT   PIC ZZZ,ZZ9.
           
      ******************************************************************
      * Question 5: Define the total line on the report.
      * (1 points)
      ****************************************************************** 
       01  TOTAL-LINE.
           05 FILLER          PIC X(40) VALUE SPACE.
           05 FINAL-TOTAL     PIC $$,$$$,$$9.
           05 FILLER          PIC X(30) VALUE SPACE. 
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           PERFORM 050-PRINT-HEADER   
           PERFORM 100-PROCESS-INFILE UNTIL EOF = 'Y'
           CLOSE INPUT-FILE OUTPUT-FILE
           DISPLAY "DONE"
           STOP RUN.    
       050-PRINT-HEADER.
      ******************************************************************
      * Question 6: Deal with the date, and write the headers.
      * (2 points)
      ****************************************************************** 
           MOVE WS-MM TO H-MONTH
           MOVE WS-YEAR TO H-YEAR
           MOVE WS-DD TO H-DAY        
           WRITE OUT-REC FROM HEADING1.
           WRITE OUT-REC FROM HEADING2.
           WRITE OUT-REC FROM HEADING3.   
           WRITE OUT-REC FROM HEADING4
               AFTER ADVANCING 2 LINES.
             
      ******************************************************************
      * Question 7:  Add transaction amount to the total OR print out  
      *              the total amount.  
      * (2 points)
      ******************************************************************  		
       100-PROCESS-INFILE.
           READ INPUT-FILE
               AT END
                   MOVE 'Y' TO EOF
                   MOVE TOTAL TO FINAL-TOTAL
                   WRITE OUT-REC FROM TOTAL-LINE
               NOT AT END
                   ADD IN-AMOUNT TO TOTAL GIVING TOTAL
                   PERFORM 200-WRITE-REPORT
           END-READ.
      ******************************************************************
      * Question 8: Write COBOL codes to print out a record on the 
      *             report.  
      * (2 points)
      ******************************************************************  		   
       200-WRITE-REPORT.
           MOVE IN-LNAME TO LNAME
           MOVE IN-INITIAL1 TO INITIAL1
           MOVE IN-INITIAL2 TO INITIAL2
           MOVE IN-YEAR TO YEAR
           MOVE IN-MONTH TO MONTH
           MOVE IN-AMOUNT TO TRANS-AMOUNT
           WRITE OUT-REC FROM RECORD-DETAILS 
               AFTER ADVANCING 1 LINES. 