       IDENTIFICATION DIVISION.
      * DO_1: Complete the following information. 
       PROGRAM-ID. Lab4.
       AUTHOR. Chase Knapp
       DATE-WRITTEN. 2/26/2015  
      ******************************************************************
      * Purpose:	  
      *     This program creates a car rental report.
      *          
      * Input:
      *     rental.dat
      * Output:
      *     rental.rpt
      ****************************************************************** 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      ******************************************************************	   
      * DO_2: Complete the SELECT statement that assgin input/output
      *       files.
      ******************************************************************	
       FILE-CONTROL.
           SELECT  IN-RENTAL-FILE
              ASSIGN TO "RENTAL.DAT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  OUT-RENTAL-FILE
              ASSIGN TO "RENTAL.RPT"
              ORGANIZATION IS LINE SEQUENTIAL. 
                
       DATA DIVISION.
      ******************************************************************	   
      * DO_3: Define the input record layout. 
      *       You MUST define condition names for car makes.
      ******************************************************************
       FILE SECTION.
       FD  IN-RENTAL-FILE.         
       01  IN-RENTAL-REC.           
           05 IN-LAST-NAME         PIC X(20).
           05 IN-FIRST-INITIAL     PIC X.
           05 IN-CAR-MAKE          PIC X.
              88  TOYOTA          VALUE "1".
              88  CHEVROLET       VALUE "2".
              88  FORD            VALUE "3".
           05 IN-MILES             PIC 9(5).
           05 IN-DAYS-RENTED       PIC 999.
      ******************************************************************	   
      * DO_4: Define the output record.  
      ******************************************************************	   
       FD  OUT-RENTAL-FILE.
       01  CUSTOMER-OUTREC         PIC X(80).

       WORKING-STORAGE SECTION.
       01  CONSTANT-RATES.
           05  TOYOTA-RATE         PIC 99     VALUE 26.
           05  TOYOTA-PER-MILE     PIC V99    VALUE .18.
           05  CHEVY-RATE          PIC 99     VALUE 32.
           05  CHEVY-PER-MILE      PIC V99    VALUE .22.
           05  FORD-RATE           PIC 99     VALUE 43.
           05  FORD-PER-MILE       PIC V99    VALUE .28.
           05  BASE-MILE           PIC 999    VALUE 100.
       01  WORK-ITEMS.
           05  EOF-SWITCH          PIC X      VALUE 'N'.
           05  WS-RENTAL-FEE       PIC 9(4)V99.
	       05  WS-DATE.
		       10  WS-YEAR         PIC 9(4).
			   10  WS-MM           PIC 99.
			   10  WS-DD           PIC 99.
      ******************************************************************	   
      * DO_5: Write your name on the report.  
      ******************************************************************	   
       01  REPORT-TITLE.
           05  FILLER              PIC  X(11) VALUE "Chase Knapp".
           05  FILLER              PIC  X(16) VALUE SPACES.
           05  FILLER              PIC  X(17) VALUE "CAR RENTAL REPORT".
           05  FILLER              PIC  X(10) VALUE SPACES.
           05  FILLER              PIC  X(6)  VALUE "DATE: ".
		   05  H-MM				   PIC  99.
		   05  FILLER              PIC  X     VALUE '/'.
		   05  H-DD                PIC  99.
		   05  FILLER              PIC  X     VALUE '/'.
		   05  H-YEAR              PIC  9(4).
           05  FILLER              PIC  X(10) VALUE SPACES.
       01  COLUMN-TITLE.
           05  FILLER              PIC  X(13) VALUE "CUSTOMER NAME".
           05  FILLER              PIC  X(9)  VALUE SPACES.
           05  FILLER              PIC  X(9)  VALUE "INITIAL  ".
           05  FILLER              PIC  X(8)  VALUE "CAR MAKE".
           05  FILLER              PIC  X(5)  VALUE SPACES.
           05  FILLER              PIC  X(5)  VALUE "MILES".
           05  FILLER              PIC  X(4)  VALUE SPACES.
           05  FILLER              PIC  X(7)  VALUE "DAYS   ".
           05  FILLER              PIC  X(10) VALUE "AMOUNT DUE".
           05  FILLER              PIC  X(10) VALUE SPACES.	    
       01  EDITED-CUSTOMER-OUTREC.
           05  ED-LASTNAME         PIC  X(20).
           05  FILLER              PIC  X(2)  VALUE SPACES.
           05  ED-INITIAL          PIC  X.
           05  FILLER              PIC  X(8)  VALUE SPACES.
           05  ED-CARMAKE          PIC  X(10).
           05  FILLER              PIC  X(3)  VALUE SPACES.
           05  ED-MILES            PIC  ZZZZ9.
           05  FILLER              PIC  X(4)  VALUE SPACES.
           05  ED-DAYS             PIC  ZZ9.
           05  FILLER              PIC  X(5)  VALUE SPACES.
           05  ED-AMOUNT           PIC  $$,$$9.99.
           05  FILLER              PIC  X(10) VALUE SPACES.
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-OPEN.
           PERFORM 200-WRITE-HEADING.
           PERFORM 300-READ UNTIL EOF-SWITCH = 'Y'
           DISPLAY "DONE!"
           STOP RUN.
      ******************************************************************	   
      * DO_6: Open input/output files
      ******************************************************************	   
       100-OPEN.
           OPEN INPUT IN-RENTAL-FILE
           OPEN OUTPUT OUT-RENTAL-FILE.
		   
      ******************************************************************	   
      * DO_7: Get the current date and move them to the header fields.
      *       So the date will be written on the report.
      ******************************************************************	   
       200-WRITE-HEADING.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-YEAR  TO H-YEAR
           MOVE WS-MM TO H-MM 
           MOVE WS-DD   TO H-DD 
           WRITE CUSTOMER-OUTREC FROM REPORT-TITLE
               AFTER  ADVANCING 2 LINES
           WRITE CUSTOMER-OUTREC FROM COLUMN-TITLE
               AFTER  ADVANCING 2 LINES.
      ******************************************************************	   
      * DO_8: Read rental file if EOF is reached, move 'Y' to the switch     
      ******************************************************************		   
       300-READ.
           READ   IN-RENTAL-FILE
                AT END
                   MOVE 'Y' TO EOF-SWITCH
                NOT AT END PERFORM 400-RENTAL-FEE
           END-READ.
       400-RENTAL-FEE.
      ******************************************************************	   
      * DO_9: Use the condition names defined to compute the rental fee.
      *       Please refer to Worksheet #3
      ****************************************************************** 
           EVALUATE TRUE
               WHEN TOYOTA 
                    COMPUTE WS-RENTAL-FEE = IN-DAYS-RENTED * TOYOTA-RATE
                               + (IN-MILES - BASE-MILE) * 
                               TOYOTA-PER-MILE                          
               WHEN CHEVROLET
			        COMPUTE WS-RENTAL-FEE = IN-DAYS-RENTED * CHEVY-RATE
                               + (IN-MILES - BASE-MILE) * CHEVY-PER-MILE
               WHEN FORD
                    COMPUTE WS-RENTAL-FEE = IN-DAYS-RENTED * FORD-RATE
                               + (IN-MILES - BASE-MILE) * FORD-PER-MILE
               WHEN OTHER
                    COMPUTE WS-RENTAL-FEE = 0
                    COMPUTE IN-MILES = 0
                    COMPUTE IN-DAYS-RENTED = 0
           END-EVALUATE
           PERFORM 500-WRITE-CUSTOMER-REC.
      ******************************************************************	   
      * DO_10: Move the data items from the input record to the working
      *        storage items to be written to the report.
      ******************************************************************	   
       500-WRITE-CUSTOMER-REC.
           MOVE IN-LAST-NAME TO ED-LASTNAME
           MOVE IN-FIRST-INITIAL TO ED-INITIAL
           MOVE IN-MILES TO ED-MILES
           MOVE IN-DAYS-RENTED TO ED-DAYS.
      ******************************************************************	   
      * DO_11: Complete the EVALUATE statement that displays the names
      *        of car makes. 
      ******************************************************************	   
           EVALUATE TRUE
               WHEN TOYOTA MOVE "TOYOTA"    TO ED-CARMAKE
               WHEN CHEVROLET MOVE "CHEVROLET" TO ED-CARMAKE  
               WHEN FORD MOVE "FORD"      TO ED-CARMAKE
               WHEN OTHER MOVE "ERROR!!"   TO ED-CARMAKE 
           END-EVALUATE.
           MOVE WS-RENTAL-FEE TO ED-AMOUNT.
           WRITE CUSTOMER-OUTREC FROM EDITED-CUSTOMER-OUTREC 
               AFTER ADVANCING 2 LINES.        
      ******************************************************************	   
      * DO_12: Close input/output files.
      ******************************************************************	   			   
       500-CLOSE.           
           CLOSE IN-RENTAL-FILE
           CLOSE OUT-RENTAL-FILE
           STOP RUN. 
