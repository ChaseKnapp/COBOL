       IDENTIFICATION DIVISION.
      * DO_1: Complete the following information. 
       PROGRAM-ID.  Lab3
       AUTHOR.  Chase Knapp
       DATE-WRITTEN.  2/19/2015
      ******************************************************************
      * Purpose:	  
      *     This program creates a customer purchase report.
      *          
      * Input:
      *     customer.dat
      * Output:
      *     customer.rpt
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	  ******************************************************************	   
      * DO_2: Complete the SELECT statements.
      ****************************************************************** 
           SELECT  IN-CUSTOMER-FILE
               ASSIGN TO "CUSTOMER.DAT"
               ORGANIZATION IS  LINE SEQUENTIAL.
           SELECT  OUT-CUSTOMER-FILE
               ASSIGN TO "CUSTOMER.RPT"
               ORGANIZATION IS  LINE SEQUENTIAL.
       DATA DIVISION.
	  ******************************************************************	   
      * DO_3: Describe your input/output files.
      ******************************************************************
       FILE SECTION.
       FD  IN-CUSTOMER-FILE.
       01  IN-CUST-REC.
           05 IN-CUST-NO          PIC X(5).
           05 IN-CUST-NAME        PIC X(20).
           05 IN-CUST-PURCHASE    PIC 9(5)V99.
       FD  OUT-CUSTOMER-FILE.
       01  OUT-CUST-REC           PIC X(80).
       WORKING-STORAGE SECTION.
       01  WORKING-ITEMS.
           05  EOF                PIC X     VALUE "N".
           05  WS-DATE.
               10  WS-YEAR        PIC 9(4).
               10  WS-MONTH       PIC 99.
               10  WS-DAY         PIC 99.
       01  HEADING-MAIN-TITLE.
           05  FILLER             PIC X(33) VALUE SPACES.
           05  FILLER             PIC X(16) VALUE "PURCHASE  REPORT".
           05  FILLER             PIC X(11) VALUE SPACES.
      ******************************************************************	   
      * DO_4: Print out your full name on the report using the VALUE 
	  *       clause.
      ******************************************************************	  
           05  FILLER             PIC X(20) VALUE "Chase Knapp".
       01  HEADING-SUBTITLE.
           05  FILLER             PIC X(33) VALUE SPACES.
           05  FILLER             PIC X(6)  VALUE "DATE: ".
           05  HD-DATE.
               10  HD-MM          PIC 99.
               10  FILLER         PIC X     VALUE "/".
               10  HD-DD          PIC 99.
               10  FILLER         PIC X     VALUE "/".
               10  HD-YEAR        PIC 9(4).
           05  FILLER             PIC X(31) VALUE SPACES.
	  ******************************************************************	   
      * DO_5: Define the field names to be printed on the report. 
	  *        
      ****************************************************************** 	   
       01  HEADING-FIELD-NAMES.
           05  FILLER             PIC X(14) VALUE "   CUSTOMER NO". 
           05  FILLER             PIC X(17) VALUE "    CUSTOMER NAME". 
           05  FILLER             PIC X(9)  VALUE SPACES. 
           05  FILLER             PIC X(16) VALUE "AMOUNT PURCHASED".
           05  FILLER             PIC X(22) VALUE SPACES.  
       01  RECORD-DETAILS. 
           05  FILLER             PIC X(6)  VALUE SPACES.
           05  WS-CUST-NO         PIC X(5).
           05  FILLER             PIC X(7)  VALUE SPACES.
           05  WS-CUST-NAME       PIC X(20).
           05  FILLER             PIC X(6)  VALUE SPACES.
           05  WS-CUST-PURCHASE   PIC ZZ,ZZ9.99.
		   05  FILLER             PIC X(27) VALUE SPACES. 
       PROCEDURE DIVISION.
       000-MAIN. 
      ******************************************************************	   
      * DO_6: OPEN and CLOSE the input/output file.
      *       (a) open the output file.
      *       (b) close the input/output files before program stop.
      ******************************************************************
           OPEN INPUT IN-CUSTOMER-FILE
           OPEN OUTPUT OUT-CUSTOMER-FILE
           PERFORM 050-PRINT-TITLES.
           WRITE OUT-CUST-REC FROM HEADING-FIELD-NAMES
                              AFTER ADVANCING 1 LINES
           PERFORM 100-READ-CUSTOMER-FILE UNTIL EOF = 'Y'
           DISPLAY "REPORT GENERATED!"
           CLOSE IN-CUSTOMER-FILE
           CLOSE OUT-CUSTOMER-FILE
           STOP RUN.   
       050-PRINT-TITLES.
      ******************************************************************	   
      * DO_7: MOVE the date to output items.
      ******************************************************************
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-YEAR  TO HD-YEAR
           MOVE WS-MONTH TO HD-MM 
           MOVE WS-DAY   TO HD-DD 
      ******************************************************************	   
      * DO_8: WRITE the main title and subtitle to the output file.
      ******************************************************************	   
            
			
           WRITE OUT-CUST-REC FROM HEADING-MAIN-TITLE
                              AFTER ADVANCING 3 LINES 
           WRITE OUT-CUST-REC FROM HEADING-SUBTITLE.
       100-READ-CUSTOMER-FILE.
      ******************************************************************	   
      * DO_9: (a) READ a customer record from the input file
      *       (b) AT end of file, move 'Y' to EOF 
      ****************************************************************** 
           READ  IN-CUSTOMER-FILE
               AT END 
                  MOVE 'Y' TO EOF
               NOT AT END           
                  PERFORM 200-WRITE-CUSTOMER-REPORT
           END-READ.
       200-WRITE-CUSTOMER-REPORT.
           MOVE IN-CUST-NO TO WS-CUST-NO
           MOVE IN-CUST-NAME TO WS-CUST-NAME
           MOVE IN-CUST-PURCHASE TO WS-CUST-PURCHASE
      ******************************************************************	   
      * DO_10: WRITE a customer record to the output file.
      *        Skip a line before writing the record. 
      ******************************************************************	   
           WRITE OUT-CUST-REC FROM RECORD-DETAILS
                              AFTER ADVANCING 2 LINES
                 