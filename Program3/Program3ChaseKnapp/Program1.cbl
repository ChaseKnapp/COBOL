       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program1.
       AUTHOR. CHASE KNAPP
       DATE-WRITTEN. 4/7/2015 
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SORT-FILE ASSIGN TO "TEMP.RPT".
           SELECT TRANSACTION-INFILE ASSIGN TO "PROGRAM3.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-OUTFILE ASSIGN TO "PROGRAM3.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       SD  SORT-FILE. 
       01  SORT-REC.
           05  
       FD  TRANSACTION-INFILE.
       01  IN-REC              PIC X(80).
       FD  TRANSACTION-OUTFILE.
       01  OUT-REC             PIC X(80).
       WORKING-STORAGE SECTION.

       procedure division.

           goback.

       end program Program1.