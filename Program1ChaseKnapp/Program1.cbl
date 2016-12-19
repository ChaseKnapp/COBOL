       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program1.
       AUTHOR. CHASE KNAPP
       DATE-WRITTEN. 2/14/2015
      ******************************************************************
      * Purpose:
      *     Calculate shipping charges for sent packages. 
      * Input:
      *     1.  Customer Name (Alphanumeric)
      *     2.  Package Weight (Numeric)
      *     3.  Is Hazardous (Alphanumeric)
      * Output:
      *     Program does the computation and outputs total cost of
      *     package. 
      *      
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
       
       77  LOW-SURCHARGE               PIC 99     VALUE 17.
       77  MID-SURCHARGE               PIC 99     VALUE 22.
       77  HIGH-SURCHARGE              PIC 99     VALUE 27.
       77  LOW-POUND-FEE               PIC 9      VALUE 5.
       77  MID-POUND-FEE               PIC 9      VALUE 6.
       77  HIGH-POUND-FEE              PIC 9      VALUE 7.
       77  MID-BASE-CHARGE             PIC 99     VALUE 20.
       77  HIGH-BASE-CHARGE            PIC 99     VALUE 41.
       77  NO-SURCHARGE                PIC 9      VALUE 0.
       77  LOW-WEIGHT                  PIC 9      VALUE 5.
       77  HIGH-WEIGHT                 PIC 99     VALUE 10.
       
       01  INPUT-ITEMS.
           05  IN-CUST-NAME            PIC X(15).
           05  IN-PACKAGE-WEIGHT       PIC 999v99.
           05  IN-HAZARDOUS            PIC X.
       01  COMPUTATION-ITEMS.
           05  WS-SHIPPING-FEE         PIC 99V99.
           05  WS-SURCHARGE            PIC 99.
           05  WS-TOTAL-CHARGE         PIC 999V99.
           05  WS-BASE-CHARGE          PIC 99V99.
           05  WS-POUND-FEE            PIC 99V99.
           05  WS-TEMP-WEIGHT          PIC 99V9.
       01  WS-DATE
           05  CD-YEAR                 PIC 9(4).
           05  CD-MONTH                PIC 9(2).
           05  CD-DAY                  PIC 9(2).
       01  OUT-ITEMS.
           05  OUT-BASE-CHARGE         PIC $ZZ9.
           05  OUT-SURCHARGE           PIC $ZZ9.
           05  OUT-SHIPPING-FEE        PIC $ZZ9.99.
           05  OUT-TOTAL-CHARGE        PIC $ZZZ9.99.
           05  OUT-POUND-CHARGE        PIC Z9.
           05  OUT-PACKAGE-WEIGHT      PIC Z9.99.

      *
       PROCEDURE DIVISION.
       000-SHIPING-FEE-CALCULATOR.
           MOVE NO-SURCHARGE TO OUT-TOTAL-CHARGE
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           PERFORM 100-DISPLAY-TITLE.
       
       100-DISPLAY-TITLE.
           DISPLAY 'ACME SHIPPING COMPANY    ' CD-MONTH '/' CD-DAY '/' 
           CD-YEAR                                                      
           DISPLAY '-----------------------------------'
           DISPLAY 'ENTER CUSTOMER NAME'
           ACCEPT IN-CUST-NAME
           PERFORM 200-INPUT-PACKAGE-INFO.
       
       200-INPUT-PACKAGE-INFO.
           DISPLAY ' '
           DISPLAY 'ENTER PACKAGE WEIGHT, ENTER 0 TO END THE PROGRAM'
           ACCEPT IN-PACKAGE-WEIGHT
           IF IN-PACKAGE-WEIGHT = NO-SURCHARGE
               PERFORM 500-DISPLAY-TOTAL
           ELSE 
               DISPLAY 'PACKAGE CONTAINS HAZARDOUS MATERIAL? (YES/NO)'
               ACCEPT IN-HAZARDOUS
               PERFORM 300-COMPUTE-SHIPING-FEE
           END-IF.
       300-COMPUTE-SHIPING-FEE.
           IF IN-PACKAGE-WEIGHT > LOW-WEIGHT
               IF IN-PACKAGE-WEIGHT > HIGH-WEIGHT
                   PERFORM 360-LEVEL3-PACKAGE
               ELSE
                   PERFORM 340-LEVEL2-PACKAGE
               END-IF
           ELSE
               PERFORM 320-LEVEL1-PACKAGE
           END-IF.
           320-LEVEL1-PACKAGE.
               IF IN-HAZARDOUS = 'Y'
                   MOVE LOW-SURCHARGE TO WS-SURCHARGE
               ELSE
                   MOVE NO-SURCHARGE TO WS-SURCHARGE
               END-IF.
               COMPUTE WS-SHIPPING-FEE = WS-SURCHARGE
               COMPUTE WS-POUND-FEE = LOW-POUND-FEE * IN-PACKAGE-WEIGHT
               COMPUTE WS-SHIPPING-FEE = WS-SHIPPING-FEE + WS-POUND-FEE
               MOVE WS-SHIPPING-FEE TO OUT-SHIPPING-FEE
               MOVE NO-SURCHARGE TO WS-BASE-CHARGE
               MOVE WS-BASE-CHARGE TO OUT-BASE-CHARGE
               MOVE WS-SURCHARGE TO OUT-SURCHARGE
               MOVE LOW-POUND-FEE TO OUT-POUND-CHARGE
               MOVE IN-PACKAGE-WEIGHT TO OUT-PACKAGE-WEIGHT
               COMPUTE WS-TOTAL-CHARGE = WS-TOTAL-CHARGE +
               WS-SHIPPING-FEE
               MOVE WS-TOTAL-CHARGE TO OUT-TOTAL-CHARGE
               PERFORM 400-DISPLAY-SUMMARY.                             
           
           340-LEVEL2-PACKAGE.
               IF IN-HAZARDOUS = 'Y'
                   MOVE MID-SURCHARGE TO WS-SURCHARGE
               ELSE
                   MOVE NO-SURCHARGE TO WS-SURCHARGE
               END-IF.
               MOVE MID-BASE-CHARGE TO WS-BASE-CHARGE
               COMPUTE WS-TEMP-WEIGHT = IN-PACKAGE-WEIGHT - LOW-WEIGHT
               COMPUTE WS-SHIPPING-FEE = WS-SURCHARGE + WS-BASE-CHARGE
               COMPUTE WS-POUND-FEE = MID-POUND-FEE * WS-TEMP-WEIGHT
               COMPUTE WS-SHIPPING-FEE = WS-SHIPPING-FEE + WS-POUND-FEE
               MOVE WS-SHIPPING-FEE TO OUT-SHIPPING-FEE
               MOVE WS-BASE-CHARGE TO OUT-BASE-CHARGE
               MOVE WS-SURCHARGE TO OUT-SURCHARGE
               MOVE MID-POUND-FEE TO OUT-POUND-CHARGE 
               MOVE IN-PACKAGE-WEIGHT TO OUT-PACKAGE-WEIGHT
               COMPUTE WS-TOTAL-CHARGE = WS-TOTAL-CHARGE + 
               WS-SHIPPING-FEE                                          
               MOVE WS-TOTAL-CHARGE TO OUT-TOTAL-CHARGE
               PERFORM 400-DISPLAY-SUMMARY.                             
           
           360-LEVEL3-PACKAGE.
               IF IN-HAZARDOUS = 'Y'
                   MOVE HIGH-SURCHARGE TO WS-SURCHARGE
               ELSE
                   MOVE NO-SURCHARGE TO WS-SURCHARGE
               END-IF.
               MOVE HIGH-BASE-CHARGE TO WS-BASE-CHARGE
               COMPUTE WS-TEMP-WEIGHT = IN-PACKAGE-WEIGHT - HIGH-WEIGHT
               COMPUTE WS-SHIPPING-FEE = WS-SURCHARGE + WS-BASE-CHARGE
               COMPUTE WS-POUND-FEE = HIGH-POUND-FEE * WS-TEMP-WEIGHT
               COMPUTE WS-SHIPPING-FEE = WS-SHIPPING-FEE + WS-POUND-FEE
               MOVE WS-SHIPPING-FEE TO OUT-SHIPPING-FEE
               MOVE WS-BASE-CHARGE TO OUT-BASE-CHARGE
               MOVE WS-SURCHARGE TO OUT-SURCHARGE
               MOVE HIGH-POUND-FEE TO OUT-POUND-CHARGE    
               MOVE IN-PACKAGE-WEIGHT TO OUT-PACKAGE-WEIGHT
               COMPUTE WS-TOTAL-CHARGE = WS-TOTAL-CHARGE + 
               WS-SHIPPING-FEE                                          
               MOVE WS-TOTAL-CHARGE TO OUT-TOTAL-CHARGE
               PERFORM 400-DISPLAY-SUMMARY.                             
           
       400-DISPLAY-SUMMARY.
           DISPLAY ' '
           DISPLAY '-----------------------------------'
           DISPLAY 'PACKAGE WEIGHT: ' OUT-PACKAGE-WEIGHT ' POUNDS'
           DISPLAY 'CHARGE PER POUND: $ ' OUT-POUND-CHARGE
           DISPLAY 'BASE CHARGE:      ' OUT-BASE-CHARGE
           DISPLAY 'SURCHARGE:        ' OUT-SURCHARGE
           DISPLAY 'SHIPPING FEE:     ' OUT-SHIPPING-FEE
           DISPLAY '-----------------------------------'
           DISPLAY ' '
           PERFORM 200-INPUT-PACKAGE-INFO.
       
       500-DISPLAY-TOTAL.
           DISPLAY ' '
           DISPLAY IN-CUST-NAME '    YOUR TOTAL CHARGE DUE: '
           OUT-TOTAL-CHARGE
           DISPLAY ' '
           DISPLAY '--END OF SESSION--'
           STOP RUN.