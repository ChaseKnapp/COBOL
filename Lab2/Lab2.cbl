       IDENTIFICATION DIVISION.
      * DO_1: Complete the following items and the comment block. 
       PROGRAM-ID.  Lab2 
       AUTHOR.  Chase Knapp
       DATE-WRITTEN.  2/3/2015
      ******************************************************************
      * Purpose:
      *     Do calculations to determine total price for customers
      *     order. 
      * Input:
      *     1.  Pizza Flavor (what type bought)
      *     2.  Pizza Quantity (how many bought)
      *     3.  Pizza Price Per Unit (price for each pizza)
      * Output:
      *     Program does the computation and outputs total cost of
      *     pizza with tax. 
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
      * DO_2: Define a 77 level constant data item for the sales tax
      *       rate 5.5%. Use WS- as the prefix of the data name.
       77  WS-TAX-RATE           PIC 9V999    VALUE .055.
	  
      * DO_3: Define the input data items under the 01 group item.
      *       Use IN- as the prefix of all data names. 
       01  INPUT-ITEMS.
           05  IN-FLAVOR         PIC X(10).
           05  IN-UNIT-PRICE     PIC 99V99.
           05  IN-QUANTITY       PIC 9.
      * DO_4: Define the output data items under the 01 group item.
      *       Use OUT- as the prefix of all data names
	  *       All output are numeric edited items.   
       01  OUTPUT-ITEMS.    
           05  OUT-UNIT-PRICE    PIC $Z9.99. 
		   05  OUT-AMOUNT        PIC $ZZ9.99.
           05  OUT-SALES-TAX     PIC $Z9.99.
           05  OUT-SALES-TOTAL   PIC $ZZZ9.99.
      * DO_5: Define other data items here if necessary.                 
       01  COMPUTATION-ITEMS.
           05  WS-AMOUNT         PIC 999V999.
           05  WS-SALES-TOTAL    PIC 999V99.     
           05  WS-SALES-TAX      PIC 9(4)V99.
      *        
       PROCEDURE DIVISION.
       000-PIZZA-CASHIER. 
           PERFORM 100-INPUT-ORDER
           PERFORM 200-COMPUTE-SALES-AMOUNT
      * DO_6: PERFORM the 300 paragraph.     
           PERFORM 300-DISPLAY-TRANSACTION 
           STOP RUN.
      *     
       100-INPUT-ORDER.  
           DISPLAY 'ENTER PIZZA FLAVOR'  
           ACCEPT IN-FLAVOR
      * DO_7: Prompt and enter quantity and unit price.     
           DISPLAY 'ENTER QUANTITY'    
           ACCEPT IN-QUANTITY
           DISPLAY 'ENTER UNIT PRICE'
           ACCEPT IN-UNIT-PRICE.
      *    
       200-COMPUTE-SALES-AMOUNT.
           MOVE IN-UNIT-PRICE TO OUT-UNIT-PRICE          
           COMPUTE WS-AMOUNT = IN-QUANTITY * IN-UNIT-PRICE
           MOVE WS-AMOUNT TO OUT-AMOUNT 
      * DO_8: COMPUTE sales tax and transaction amount and
      *       MOVE them to the output data items.	  
           COMPUTE WS-SALES-TAX = WS-AMOUNT * WS-TAX-RATE
           MOVE WS-SALES-TAX TO OUT-SALES-TAX
           COMPUTE WS-SALES-TOTAL = WS-AMOUNT + WS-SALES-TAX   
           MOVE WS-SALES-TOTAL TO OUT-SALES-TOTAL.     
      *
       300-DISPLAY-TRANSACTION.   
           DISPLAY SPACE 
           DISPLAY '-----------------------'
           DISPLAY '  PIZZA SALES SUMMARY'
           DISPLAY '-----------------------'
      * DO_9: DISPLAY the rest of the data items.     
           DISPLAY 'FLAVOR CHOSEN:   ' IN-FLAVOR
           DISPLAY 'UNIT PRICE:      ' OUT-UNIT-PRICE
           DISPLAY 'SALES AMOUNT:    ' OUT-AMOUNT
           DISPLAY 'SALES TAX:       ' OUT-SALES-TAX
           DISPLAY '-----------------------'
           DISPLAY 'SALES TOTAL:    ' OUT-SALES-TOTAL.