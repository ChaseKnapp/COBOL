       IDENTIFICATION DIVISION.
       PROGRAM-ID. Lab6.
      * DO_1: Complete the following information.   
       AUTHOR.  Chase Knapp
       DATE-WRITTEN. 4/4/2015
      ******************************************************************
      * Purpose:	  
      *     Learn how to create an Exception Report with 
      *     contition names.
      * Input:
      *     lab6.dat
      * Output:
      *     errsales.rpt
      ******************************************************************  
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-INFILE ASSIGN TO "lab6.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SALES-OUTFILE ASSIGN TO "ERRSALES.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FD  CUSTOMER-INFILE.
       01  CUSTOMER-REC.
           05  CS-NO         PIC 9(4).
           05  CS-NAME       PIC X(10).
               88  NONAME    VALUE SPACES.
           05  CS-STORENO    PIC 9.
           05  CS-SALESNO    PIC 9(3).
           05  CS-SALESAMT   PIC 9(3)V99.
           05  CS-TRANDATE.
               10  CS-MONTH  PIC 99.
               10  CS-DAY    PIC 99.
               10  CS-YEAR   PIC 9(4).
       FD  SALES-OUTFILE.
       01  SALES-REC   PIC X(80).
       WORKING-STORAGE SECTION.
       01  WORKING-ITEMS.
           05  ERRMSG-NUMBER PIC 9             VALUE 5.
           05  EOF-SWITCH    PIC X             VALUE 'N'.
               88  EOF                         VALUE 'Y'.
           05  CUSNO-FLAG    PIC X.
               88  CUSNO-FLAGGED               VALUE 'Y'.
           05  STORENO-FLAG  PIC X.
               88  STORENO-FLAGGED             VALUE 'Y'.
           05  NAME-FLAG     PIC X.
               88  NAME-FLAGGED                VALUE 'Y'.
           05  AMT-FLAG      PIC X.
               88  AMT-FLAGGED                 VALUE 'Y'.
           05  SALES-FLAG    PIC X.
               88  SALES-FLAGGED               VALUE 'Y'.
           05  SALESAMT      PIC 999V99.
               88  VALID-AMT                   VALUE 1 THRU 200.
       01  WS-REPORT-HEADING.
           05  FILLER        PIC X(35)         VALUE
                             "SALES TRANSACTION VALIDATION REPORT".
           05  FILLER        PIC X(29)         VALUE
                             " CREATED  BY: CHASE KNAPP".
      ******************************************************************	   
      * DO_2: Define your name printed on the report. 
      ****************************************************************** 	   
       01  WS-ASTERISK-LINE  PIC X(56)         VALUE ALL '*'.
       01  WS-FOOTER.
           05  ERR-COUNT     PIC 99            VALUE ZEROS.
           05  FILLER        PIC X(33)         VALUE
                             " ERROR RECORDS IDENTIFIED OUT OF ".
           05  TOTAL-COUNT   PIC 99            VALUE ZEROS.
           05  FILLER        PIC X(19)         VALUE
                             " RECORDS PROCESSED.".
       01  WS-CUSTOMER-REC.
           05  WS-NO         PIC 9(4).
               88  VALID-CUSNO                 VALUE 101 THRU 9621.
           05  FILLER        PIC XX.
           05  WS-NAME       PIC X(10)         VALUE SPACES.
           05  FILLER        PIC XX.
      ******************************************************************	   
      * DO_3: Define condition names for valid store numbers. 
      ******************************************************************  
           05  WS-STORENO    PIC 9.
               88  NY                          VALUE 1.
               88  LA                          VALUE 2.
               88  MI                          VALUE 3.
               88  CI                          VALUE 4.
               88  VALID-STORENO               VALUE 1 THRU 4.   
           05  FILLER        PIC XX.
      ******************************************************************	   
      * DO_4: Define condition names for valid sales numbers.
      ******************************************************************  
           05  WS-SALESNO    PIC 9(3).
               88  VALID-SALES-NY              VALUE 001 THRU 087.
               88  VALID-SALES-LA              VALUE 088 THRU 192.
			   88  VALID-SALES-MI              VALUE 193 THRU 254.
			   88  VALID-SALES-CI              VALUE 255 THRU 400.
           05  FILLER        PIC XX.
           05  WS-SALESAMT   PIC $$$9.99.
           05  FILLER        PIC XX.
           05  WS-TRANDATE.
               07  WS-MONTH  PIC 99.
               07  FILLER    PIC X             VALUE '/'.
               07  WS-DAY    PIC 99.
               07  FILLER    PIC X             VALUE '/'.
               07  WS-YEAR   PIC 9(4).
       PROCEDURE DIVISION.
       000-MAIN.
           OPEN INPUT CUSTOMER-INFILE
           OPEN OUTPUT SALES-OUTFILE
           WRITE SALES-REC FROM WS-REPORT-HEADING.
           PERFORM 100-READ-SALES UNTIL EOF
           WRITE SALES-REC FROM WS-ASTERISK-LINE
           WRITE SALES-REC FROM WS-FOOTER
           CLOSE CUSTOMER-INFILE SALES-OUTFILE
           DISPLAY 'DONE!!'
           STOP RUN.
       100-READ-SALES.
           READ CUSTOMER-INFILE
               AT END
                  MOVE 'Y' TO EOF-SWITCH
               NOT AT END
                  ADD 1 TO TOTAL-COUNT
                  PERFORM 200-WRITE-SALES.
       200-WRITE-SALES.
           PERFORM 220-MOVE-A-RECORD
      ******************************************************************	   
      * DO_5: Write conditional statements with the condition names 
      *       to PERFORM 300-ERR-REPORT if an error is detected
      ****************************************************************** 	   
           EVALUATE TRUE
               WHEN NOT VALID-CUSNO
                   PERFORM 300-ERR-REPORT
               WHEN WS-NAME EQUALS SPACES
                   PERFORM 300-ERR-REPORT
               WHEN NOT VALID-STORENO  
                   PERFORM 300-ERR-REPORT
               WHEN NOT VALID-AMT
                   PERFORM 300-ERR-REPORT
               WHEN VALID-STORENO
                   EVALUATE TRUE ALSO TRUE
		               WHEN NY ALSO NOT VALID-SALES-NY
                           PERFORM 300-ERR-REPORT
                       WHEN LA ALSO NOT VALID-SALES-LA
                           PERFORM 300-ERR-REPORT
                       WHEN MI ALSO NOT VALID-SALES-MI
                           PERFORM 300-ERR-REPORT
                       WHEN CI ALSO NOT VALID-SALES-CI
                           PERFORM 300-ERR-REPORT
                   END-EVALUATE
		   END-EVALUATE.
		   
       220-MOVE-A-RECORD.
           MOVE CS-NO TO WS-NO
           MOVE CS-NAME TO WS-NAME
           MOVE CS-STORENO TO WS-STORENO
           MOVE CS-SALESNO TO WS-SALESNO
           MOVE CS-SALESAMT TO WS-SALESAMT SALESAMT
           MOVE CS-MONTH TO WS-MONTH
           MOVE CS-DAY TO WS-DAY
           MOVE CS-YEAR TO WS-YEAR.
       300-ERR-REPORT.
           ADD 1 TO ERR-COUNT
           WRITE SALES-REC FROM WS-CUSTOMER-REC AFTER ADVANCING 1 LINES
           MOVE 'N' TO CUSNO-FLAG STORENO-FLAG NAME-FLAG
                       AMT-FLAG SALES-FLAG
           PERFORM 310-ERR-MSG ERRMSG-NUMBER TIMES.
      ******************************************************************	   
      * DO_6: Complete the EVALUATE statement that prints error messages 
      *       based on the error type. There are 5 possible error types.
      *       This paragraph will be performed for 5 times.  
      *       You MUST use condition names defined in the 
      *       WORKING-STORAGE SECTION.
      ******************************************************************	   
       310-ERR-MSG.
           EVALUATE TRUE ALSO TRUE
               WHEN NOT VALID-CUSNO ALSO NOT CUSNO-FLAGGED
                   WRITE SALES-REC FROM "INVALID CUSTOMER NO!!"
                   MOVE 'Y' TO CUSNO-FLAG
      ******************************************************************			   
      * CUSNO-FLAG is used to identified the error message has been 
      * printed, and will not be check again. Do the same thing for 
      * other error messages.
      ******************************************************************	  
               WHEN WS-NAME EQUALS SPACES ALSO NOT NAME-FLAGGED
                   WRITE SALES-REC FROM "CUSTOMER NAME MISSING!!"
                   MOVE 'Y' TO NAME-FLAG
               WHEN NOT VALID-STORENO ALSO NOT STORENO-FLAGGED
                   WRITE SALES-REC FROM "INVALID STORE NO!!"
                   MOVE "Y" TO STORENO-FLAG
               WHEN NOT VALID-AMT ALSO NOT AMT-FLAGGED
                   WRITE SALES-REC FROM "EXCEEDS MAXIMUM SALES AMOUNT!!"
                   MOVE "Y" TO AMT-FLAG
               WHEN VALID-STORENO ALSO NOT SALES-FLAGGED
                   EVALUATE TRUE ALSO TRUE
                       WHEN NY ALSO NOT VALID-SALES-NY
                           WRITE SALES-REC FROM
                              "STORE NO AND SALES NO ARE INCONSISTENT!!"
                           MOVE "Y" TO SALES-FLAG
                       WHEN LA ALSO NOT VALID-SALES-LA 
                           WRITE SALES-REC FROM
                              "STORE NO AND SALES NO ARE INCONSISTENT!!"
                           MOVE "Y" TO SALES-FLAG
                       WHEN MI ALSO NOT VALID-SALES-MI
                           WRITE SALES-REC FROM
                              "STORE NO AND SALES NO ARE INCONSISTENT!!"
                           MOVE "Y" TO SALES-FLAG
                      WHEN CI ALSO NOT VALID-SALES-CI
                           WRITE SALES-REC FROM
                              "STORE NO AND SALES NO ARE INCONSISTENT!!"
                           MOVE "Y" TO SALES-FLAG
                   END-EVALUATE
           END-EVALUATE.
