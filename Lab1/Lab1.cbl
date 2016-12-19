       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAB1.
       AUTHOR. CHASE KNAPP
       DATE-WRITTEN. 1/22/2015
      ****************************************************************
      * Puropse: 
      *    This program shows a general structure of a Cobol program. 
      *    You can use this program as a template for your labs or 
      *    programs. Use ALL UPPERCASE letters for your COBOL codes.
      *    Use lowercase letters for your comments. 
      * Input:
      *    None.
      * Output:
      *    Display data items on the console window
      *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. 
      *
       INPUT-OUTPUT sECTION.
       FILE-CONTROL.
      *
       DATA DIVISION. 
       FILE SECTION.
      *
       WORKING-STORAGE SECTION. 
      *
       77  CONSTANT-1        PIC 9      VALUE 5.
       77  CONSTANT-2        PIC 9      VALUE 7.
       01  TELEPHONE-NO.
           05  AREA-CODE     PIC X(3)   VALUE '608'.
           05  PHONE-1       PIC X(3)   VALUE '342'.
           05  PHONE-2       PIC X(4)   VALUE '1557'.
       LINKAGE SECTION. 
      *
       PROCEDURE DIVISION. 
       000-MAIN. 
           DISPLAY 'This is my first COBOL program'
           PERFORM 100-DISPLAY-CONSTANTS.
           STOP RUN.
       100-DISPLAY-CONSTANTS.
           DISPLAY 'Constant 1: ' constant-1
           DISPLAY 'Constant 2; ' constant-2
           PERFORM 200-DISPLAY-TELNO.
       200-DISPLAY-TELNO.
           DISPLAY 'Telphone Number: (' AREA-CODE ')'
                    PHONE-1 '-' PHONE-2.
          