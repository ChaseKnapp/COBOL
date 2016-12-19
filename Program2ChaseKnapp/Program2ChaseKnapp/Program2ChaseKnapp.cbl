       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program2ChaseKnapp.
       AUTHOR. CHASE KNAPP
       DATE-WRITTEN. 3/11/2015
       DATE-COMPILED. 3/12/2015
      ******************************************************************
      * Purpose:
      *     Adjust employees salary with a percentage. 
      * Input:
      *     1.  Employee NO (alphanumeric)
      *     2.  Emplyee Name (alphanumeric)
      *     3.  Territory no (alphanueric)
      *     4.  Office no (alphanumeric)
      *     5.  Annual Salary (numeric)
      *     6.  SSN (alphanumeric)
      *     7.  No of Dependents (alphanumeric)
      *     8.  Job Classification Code (alphanumeric)
      * Output:
      *     outputs a file with input information and updated salary. 
      *      
      ******************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
           SELECT  INPUT-FILE
               ASSIGN TO "employee.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  OUTPUT-FILE
               ASSIGN TO "employee.rpt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-FILES. 
           05  IN-EMPLOYEE-NO        PIC X(5).
           05  IN-EMPLOYEE-NAME      PIC X(20).
           05  IN-LOCATION-CODE.
               07  TERRITORY-NO      PIC XX.
               07  OFFICE-NO         PIC XX.
                   88  FIRST-CODE                 VALUE "01".
                   88  SECOND-CODE                VALUE "02".
                   88  THIRD-CODE                 VALUE "03".
                   88  FOURTH-CODE                VALUE "04".
                   88  FIFTH-CODE                 VALUE "05".
                   88  SIXTH-CODE                 VALUE "06".
                   88  SEVENTH-CODE               VALUE "07".
           05  IN-SALARY             PIC 9(6).
           05  IN-SSN                PIC X(9).
           05  IN-NO-DEPENDENTS      PIC XX.
           05  IN-JOB-CODE           PIC XX.
                   88  FIRST-NO                   VALUE "01".
                   88  SECOND-NO                  VALUE "02".
                   88  THIRD-NO                   VALUE "03".
           05  UNUSED                PIC X(32).
       FD  OUTPUT-FILE. 
       01  EMPLOYEE-OUTREC           PIC X(80).
           
       WORKING-STORAGE SECTION.
       01  CONSTANT-RATES.
           05  MULT                  PIC 99       VALUE 10.
           05  PERCENTAGE-246        PIC V99      VALUE .25.
           05  PERCENTAGE-1          PIC V99      VALUE .30.
           05  PERCENTAGE-3          PIC V99      VALUE .35.
           05  PERCENTAGE-7          PIC V99      VALUE .40.
       01  WORK-ITEMS.
           05  EOF-SWITCH            PIC X        VALUE 'N'.
           05  WS-PERCENT-INCREASE   PIC 9V9.
           05  WS-AMOUNT-INCREASE    PIC 9(5)V99.
           05  WS-NEW-SALARY         PIC 9(7)9V99.
       	   05  WS-DATE.
		       10  WS-YEAR           PIC 9(4).
			   10  WS-MM             PIC 99.
			   10  WS-DD             PIC 99.
               10  WS-HOUR           PIC 99.
               10  WS-MINUTE         PIC 99.
           05  WS-TOTAL-SALARY       PIC 9(9).
           05  WS-TOTAL-FINAL        PIC 9(8)V99.
           05  WS-TOTAL-INCREASE     PIC 9(9)V99.
       01  REPORT-TITLE.
           05  FILLER                PIC X(6)     VALUE "DATE: ".
           05  H-MM                  PIC 99.
           05  FILLER                PIC X        VALUE '/'.
           05  H-DD                  PIC 99.
           05  FILLER                PIC X        VALUE '/'.
           05  H-YEAR                PIC 9(4).
           05  FILLER                PIC X(7)     VALUE SPACES.
           05  FILLER                PIC X(13)    VALUE "INTERNATIONAL".
           05  FILLER                PIC X        VALUE SPACES.
           05  FILLER                PIC X(7)     VALUE "CHERRY ".
           05  FILLER                PIC X(8)     VALUE "MACHINE ".
           05  FILLER                PIC X(7)     VALUE "COMPANY".
           05  FILLER                PIC X(13)    VALUE SPACES.
           05  FILLER                PIC X(8)     VALUE "PAGE   1".
       01  REPORT-TITLE2
           05  FILLER                PIC X(6)     VALUE "TIME: ".
           05  H-HOUR                PIC 99.
           05  FILLER                PIC X        VALUE ":".
           05  H-MINUTE              PIC 99.
           05  FILLER                PIC X(16)    VALUE SPACES. 
           05  FILLER                PIC X(7)     VALUE "SALARY ".
           05  FILLER                PIC X(9)     VALUE "INCREASE ".
           05  FILLER                PIC X(7)     VALUE "DETAIL ".
           05  FILLER                PIC X(6)     VALUE "REPORT".
           05  FILLER                PIC X(13)    VALUE SPACES.
           05  FILLER                PIC X(11)    VALUE "CHASE KNAPP".
       01  REPORT-HEADING. 
           05  FILLER                PIC X(9)     VALUE "EMPLOYEE ".
           05  FILLER                PIC X(8)     VALUE "EMPLOYEE".
           05  FILLER                PIC X(12)    VALUE SPACES.
           05  FILLER                PIC X(7)     VALUE "OFFICE ".
           05  FILLER                PIC X(6)     VALUE "JOB   ".
           05  FILLER                PIC X(8)     VALUE "ANNUAL  ".
           05  FILLER                PIC X(9)     VALUE "PERC.    ".
           05  FILLER                PIC X(3)     VALUE "AMT".
           05  FILLER                PIC X(13)    VALUE SPACES.
           05  FILLER                PIC X(5)     VALUE "NEW  ".
       01  REPORT-HEADING2.
           05  FILLER                PIC X(5)     VALUE "  NUM".
           05  FILLER                PIC X(8)     VALUE "    NAME".
           05  FILLER                PIC X(18)    VALUE SPACES.
           05  FILLER                PIC X(5)     VALUE "NO   ".
           05  FILLER                PIC X(6)     VALUE "CODE  ".
           05  FILLER                PIC X(8)     VALUE "SALARY  ".
           05  FILLER                PIC X(7)     VALUE "INCR.  ".
           05  FILLER                PIC X(8)     VALUE "INCREASE".
           05  FILLER                PIC X(9)     VALUE SPACES.
           05  FILLER                PIC X(6)     VALUE "SALARY".
       01  EMPLOYEE-RECORD.
           05  FILLER                PIC X        VALUE SPACES.
           05  OUT-NUM               PIC X(5).
           05  FILLER                PIC XXX      VALUE SPACES.
           05  OUT-NAME              PIC X(20).
           05  FILLER                PIC XX      VALUE SPACES.
           05  OUT-NO                PIC XX.
           05  FILLER                PIC XXX      VALUE SPACES.
           05  OUT-CODE              PIC XX. 
           05  FILLER                PIC XXX      VALUE SPACES.
           05  ANNUAL-SALARY         PIC $ZZZ,ZZ9.
           05  FILLER                PIC X        VALUE SPACES.
           05  OUT-PERC              PIC 9.9.
           05  FILLER                PIC XX       VALUE "% ".
           05  OUT-INCREASE          PIC $$$,$$9.99.
           05  FILLER                PIC XX       VALUE SPACES.
           05  OUT-NEW-SALARY        PIC $$,$$$,$$9.99.
       01  TOTAL-LINE.
           05  FILLER                PIC X(24)    VALUE SPACES.
           05  FILLER                PIC X(6)     VALUE "TOTALS".
           05  FILLER                PIC X(8)     VALUE SPACES.
           05  OUT-ANNUAL            PIC $$$,$$$,$$9.
           05  FILLER                PIC XXX      VALUE SPACES.
           05  OUT-INC               PIC $$,$$$,$$9.99.
           05  FILLER                PIC X        VALUE SPACES.
           05  OUT-FINAL             PIC $$$,$$$,$$9.99.
       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-OPEN.
           PERFORM 200-WRITE-HEADING.
           PERFORM 300-READ UNTIL EOF-SWITCH = 'Y'.
           PERFORM 500-TOTAL-LINE.
           PERFORM 600-FILE-CLOSE.
           STOP RUN.
      ******************************************************************
      * Opens the input file and the output file for the program.
      ******************************************************************                                                             
       100-OPEN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE.
      ******************************************************************
      *  Writes the first 4 header lines to the report file 
      ******************************************************************     
       200-WRITE-HEADING.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-YEAR TO H-YEAR
           MOVE WS-MM TO H-MM
           MOVE WS-DD TO H-DD
           MOVE WS-HOUR TO H-HOUR
           MOVE WS-MINUTE TO H-MINUTE
           WRITE EMPLOYEE-OUTREC FROM REPORT-TITLE
           WRITE EMPLOYEE-OUTREC FROM REPORT-TITLE2
           WRITE EMPLOYEE-OUTREC FROM REPORT-HEADING 
               AFTER ADVANCING 1 LINES
           WRITE EMPLOYEE-OUTREC FROM REPORT-HEADING2
               AFTER ADVANCING 0 LINES.
      ******************************************************************  
      * Reads the lines in the input file until the end.                                                                   
      ******************************************************************                                                               
       300-READ.
           READ   INPUT-FILE
                AT END
                   MOVE 'Y' TO EOF-SWITCH
                NOT AT END PERFORM 400-SALARY-INCREASE
           END-READ.           
      ******************************************************************                                                             
      * Checks the location code and the job code and performs correct
      * paragraph depending on codes.
      ******************************************************************                                                               
       400-SALARY-INCREASE.
           EVALUATE TRUE
               WHEN FIRST-CODE
                   IF FIRST-NO OR SECOND-NO
                       PERFORM 320-MID-LOW-PERCENTAGE
                   ELSE 
                       PERFORM 350-NO-PERCENTAGE
               WHEN THIRD-CODE
                   IF THIRD-NO
                       PERFORM 320-MID-LOW-PERCENTAGE
                   ELSE 
                       PERFORM 330-MID-HIGH-PERCENTAGE
               WHEN FIFTH-CODE
                   IF FIRST-NO OR THIRD-NO
                       PERFORM 350-NO-PERCENTAGE
                   ELSE
                       PERFORM 310-LOW-PERCENTAGE
               WHEN SEVENTH-CODE
                   IF FIRST-NO
                       PERFORM 320-MID-LOW-PERCENTAGE
                   ELSE
                       PERFORM 340-HIGH-PERCENTAGE
               WHEN SECOND-CODE OR FOURTH-CODE OR SIXTH-CODE
                   PERFORM 310-LOW-PERCENTAGE
               WHEN OTHER
                   PERFORM 350-NO-PERCENTAGE
           END-EVALUATE                                 
           PERFORM 400-PRINT-LINE.
      ******************************************************************  
      *  Performs equationf for 2.5% increase                                                                  
      ******************************************************************                                                                      
           310-LOW-PERCENTAGE.
               COMPUTE WS-PERCENT-INCREASE = PERCENTAGE-246 * MULT
               COMPUTE WS-AMOUNT-INCREASE = PERCENTAGE-246 * IN-SALARY
               COMPUTE WS-AMOUNT-INCREASE = WS-AMOUNT-INCREASE / MULT
               COMPUTE WS-NEW-SALARY = IN-SALARY + WS-AMOUNT-INCREASE
               COMPUTE WS-TOTAL-SALARY = WS-TOTAL-SALARY + IN-SALARY                                     
               COMPUTE WS-TOTAL-FINAL = WS-TOTAL-FINAL + WS-NEW-SALARY
               COMPUTE WS-TOTAL-INCREASE = WS-TOTAL-INCREASE + 
                       WS-AMOUNT-INCREASE .  
      ******************************************************************  
      *  Performs equationf for 3.0% increase                                                                  
      ****************************************************************** 
           320-MID-LOW-PERCENTAGE.
               COMPUTE WS-PERCENT-INCREASE = PERCENTAGE-1 * MULT
               COMPUTE WS-AMOUNT-INCREASE = PERCENTAGE-1 * IN-SALARY
               COMPUTE WS-AMOUNT-INCREASE = WS-AMOUNT-INCREASE / MULT
               COMPUTE WS-NEW-SALARY = IN-SALARY + WS-AMOUNT-INCREASE
               COMPUTE WS-TOTAL-SALARY = WS-TOTAL-SALARY + IN-SALARY
               COMPUTE WS-TOTAL-FINAL = WS-TOTAL-FINAL + WS-NEW-SALARY
               COMPUTE WS-TOTAL-INCREASE = WS-TOTAL-INCREASE + 
                       WS-AMOUNT-INCREASE.
      ******************************************************************  
      *  Performs equationf for 3.5% increase                                                                  
      ****************************************************************** 
           330-MID-HIGH-PERCENTAGE.
               COMPUTE WS-PERCENT-INCREASE = PERCENTAGE-3 * MULT
               COMPUTE WS-AMOUNT-INCREASE = PERCENTAGE-3 * IN-SALARY
               COMPUTE WS-AMOUNT-INCREASE = WS-AMOUNT-INCREASE / MULT
               COMPUTE WS-NEW-SALARY = IN-SALARY + WS-AMOUNT-INCREASE
               COMPUTE WS-TOTAL-SALARY = WS-TOTAL-SALARY + IN-SALARY
               COMPUTE WS-TOTAL-FINAL = WS-TOTAL-FINAL + WS-NEW-SALARY
               COMPUTE WS-TOTAL-INCREASE = WS-TOTAL-INCREASE + 
                       WS-AMOUNT-INCREASE.   
      ******************************************************************  
      *  Performs equationf for 4.0% increase                                                                  
      ******************************************************************       
           340-HIGH-PERCENTAGE.
               COMPUTE WS-PERCENT-INCREASE = PERCENTAGE-7 * MULT
               COMPUTE WS-AMOUNT-INCREASE = PERCENTAGE-7 * IN-SALARY
               COMPUTE WS-AMOUNT-INCREASE = WS-AMOUNT-INCREASE / MULT
               COMPUTE WS-NEW-SALARY = IN-SALARY + WS-AMOUNT-INCREASE
               COMPUTE WS-TOTAL-SALARY = WS-TOTAL-SALARY + IN-SALARY
               COMPUTE WS-TOTAL-FINAL = WS-TOTAL-FINAL + WS-NEW-SALARY
               COMPUTE WS-TOTAL-INCREASE = WS-TOTAL-INCREASE + 
                       WS-AMOUNT-INCREASE.   
      ******************************************************************  
      *  Performs equationf for no increase                                                                  
      ******************************************************************       
           350-NO-PERCENTAGE.
               COMPUTE WS-PERCENT-INCREASE = PERCENTAGE-246 * ZERO
               COMPUTE WS-AMOUNT-INCREASE = PERCENTAGE-246 * ZERO
               COMPUTE WS-NEW-SALARY = IN-SALARY + ZERO
               COMPUTE WS-TOTAL-SALARY = WS-TOTAL-SALARY + IN-SALARY                                     
               COMPUTE WS-TOTAL-FINAL = WS-TOTAL-FINAL + WS-NEW-SALARY
               COMPUTE WS-TOTAL-INCREASE = WS-TOTAL-INCREASE + 
                       WS-AMOUNT-INCREASE.
      ****************************************************************** 
      *  Prints the line with employee information. Includes salary and 
      *  new salary.                                                                
      ******************************************************************                                                                         
       400-PRINT-LINE.
           MOVE IN-EMPLOYEE-NAME TO OUT-NAME
           MOVE IN-EMPLOYEE-NO TO OUT-NUM
           MOVE OFFICE-NO TO OUT-NO
           MOVE IN-JOB-CODE TO OUT-CODE
           MOVE IN-SALARY TO ANNUAL-SALARY 
           MOVE WS-PERCENT-INCREASE TO OUT-PERC
           MOVE WS-AMOUNT-INCREASE TO OUT-INCREASE
           MOVE WS-NEW-SALARY TO OUT-NEW-SALARY
           WRITE EMPLOYEE-OUTREC FROM EMPLOYEE-RECORD 
               AFTER ADVANCING 2 LINES.
      ****************************************************************** 
      *  Prints out the final line to outrec with all totals.                                                                
      ******************************************************************                                                                         
       500-TOTAL-LINE.
           MOVE WS-TOTAL-SALARY TO OUT-ANNUAL
           MOVE WS-TOTAL-INCREASE TO OUT-INC
           MOVE WS-TOTAL-FINAL TO OUT-FINAL
           WRITE EMPLOYEE-OUTREC FROM TOTAL-LINE
                 AFTER ADVANCING 2 LINES.
      ******************************************************************
      *  Closes the input and output file.                                                                   
      ******************************************************************                                                                         
       600-FILE-CLOSE.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
       END PROGRAM Program2ChaseKnapp.