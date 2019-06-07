       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLNB06.
       AUTHOR.     NICO BUSATTO.
       DATE-WRITTEN.       11/02/19.
       DATE-COMPILED.      12/02/19.

      *-----------------------------------------------------------------

      **************************************************************
      *THIS PROGRAM WILL PRINT DOUBLE SPACED RECORDS FOR EACH      *
      *CUSTOMER THAT MADE A PURCHASE. IT WILL PRINT ALSO AND ERROR *
      *REPORT FOR EACH INVALID RECORD. AT THE END, IT WILL PRINT   *
      *A GRAND TOTAL FOR EACH POP TYPE, AND A GRAND TOTAL FOR EACH *
      *TEAM. THIS PROJECT USES ARRAYS.                             *
      **************************************************************
     
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT POP-MASTER
               ASSIGN TO "C:\COBOL\CBLNB06\CBLPOPSL.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

            SELECT PRTOUT
               ASSIGN TO "C:\COBOL\CBLNB06\CBLPOPSL.PRT"
               ORGANIZATION IS RECORD SEQUENTIAL.

            SELECT ERR-PRTOUT
               ASSIGN TO "C:\COBOL\CBLNB06\CBLPOPER.PRT"
               ORGANIZATION IS RECORD SEQUENTIAL.

      *-----------------------------------------------------------------

       DATA DIVISION.
       FILE SECTION.

       FD  POP-MASTER
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 71 CHARACTERS
           DATA RECORD IS I-REC.

      *Pop sale Rec Declaration   

       01  I-REC.
           05  I-LNAME            PIC X(15).
           05  I-FNAME            PIC X(15).
           05  I-ADDRESS          PIC X(15).
           05  I-CITY             PIC X(10).
           05  I-STATE            PIC XX.
               88  VAL-STATE      VALUE "IA" "IL" "MI" "MO" "NE" "WI".
           05  I-ZIP.              
               10  I-ZIP-1        PIC 9(5).
               10  I-ZIP-2        PIC 9(4).
           05  I-POP-TYPE         PIC 99.
               88  VAL-POP-TYPE   VALUE 01 THRU 06.
           05  I-NUM-CASES        PIC 99.
           05  I-TEAM             PIC X.
               88  VAL-TEAM       VALUE "A" "B" "C" "D" "E".

       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 55 WITH FOOTING 35
           DATA RECORD IS PRTLINE.

       01  PRTLINE                PIC X(132).

       FD  ERR-PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 55 WITH FOOTING 35
           DATA RECORD IS PRTLINE2.

       01  PRTLINE2               PIC X(132).
      
       WORKING-STORAGE SECTION.

       01  WORK-AREA.
           05  X                  PIC 9.
           05  Y                  PIC 9.
           05  Z                  PIC 9.
           05  MORE-RECS          PIC XXX           VALUE "YES".
           05  ERR-SW             PIC XXX           VALUE "YES".
           05  C-PCTR             PIC 9             VALUE ZERO.
           05  C-ERR-PCTR         PIC 9             VALUE ZERO.
           05  C-DEPOSIT-AMT      PIC 9999V99       VALUE ZERO.
           05  C-CASE-COST        PIC 99V99         VALUE 18.71.
           05  C-TOT-CASES        PIC 9(4)V99       VALUE ZERO.
           05  C-TOT-SALES        PIC 9(6)V99       VALUE ZERO.
           05  C-ERR-CTR          PIC 9(4)V99       VALUE ZERO.

       01  ERROR-AREA.
           05  FILLER             PIC X(35)         VALUE "LAST NAME IS 
      -     "REQUIRED".
           05  FILLER             PIC X(35)         VALUE "FIRST NAME IS
      -     "REQUIRED".
           05  FILLER             PIC X(35)         VALUE "ADDRESS IS RE
      -     "QUIRED".
           05  FILLER             PIC X(35)         VALUE "CITY IS REQUI
      -     "RED".
           05  FILLER             PIC X(35)         VALUE "ONLY IA, IL,
      -     "MI, MO, NE, WI ALLOWED".
           05  FILLER             PIC X(35)         VALUE "ZIP CODE MUST
      -     " BE NUMERIC".
           05  FILLER             PIC X(35)         VALUE "POP TYPE MUST
      -     "BE A NUMBER".
           05  FILLER             PIC X(35)         VALUE "TYPE MUST BE
      -     "A NUMBER FROM 01 TO 06".
           05  FILLER             PIC X(35)         VALUE "NUMBER OF CAS
      -     "ES MUST BE NUMERIC".
           05  FILLER             PIC X(35)         VALUE "NUMBER OF CAS
      -     "ES MUST BE AT LEAST 1".
           05  FILLER             PIC X(35)         VALUE "TEAM CAN ONLY
      -     "BE A, B, C, D OR E".

       01  ERROR-TABLE REDEFINES ERROR-AREA.
           05  ERRORS OCCURS 11.
               10  T-ERR          PIC X(35).

       01  TEAM-NAMES.
           05  FILLER             PIC X             VALUE "A".
           05  FILLER             PIC X             VALUE "B".
           05  FILLER             PIC X             VALUE "C".
           05  FILLER             PIC X             VALUE "D".
           05  FILLER             PIC X             VALUE "E".

       01  TEAM-TABLE REDEFINES TEAM-NAMES.
           05  TEAMS OCCURS 5.
               10  TEAM-NAME      PIC X.

       01  TEAM-TOTALS.
           05  C-GT-RAISED        PIC 9(10)V99      OCCURS 5.

       01  POP-DEPOSIT.
           05  FILLER             PIC X(5)          VALUE "005IA".
           05  FILLER             PIC X(5)          VALUE "000IL".
           05  FILLER             PIC X(5)          VALUE "010MI".
           05  FILLER             PIC X(5)          VALUE "000MO".
           05  FILLER             PIC X(5)          VALUE "005NE".
           05  FILLER             PIC X(5)          VALUE "005WI".

       01  POP-DEPOSIT-TABLE REDEFINES POP-DEPOSIT.
           05  STATE-DEPOSIT OCCURS 6.
               10  DEPOSIT        PIC 9V99.
               10  STATE          PIC XX.
               
       01  POP-TOT-QTY.
           05  C-POP-TOT          PIC 9(6)          OCCURS 6.

       01  POP-NAME.
           05  FILLER             PIC X(16)         VALUE "COKE".
           05  FILLER             PIC X(16)         VALUE "DIET COKE".
           05  FILLER             PIC X(16)         VALUE "MELLO YELLO".
           05  FILLER             PIC X(16)         VALUE "CHERRY COKE".
           05  FILLER             PIC X(16)         VALUE "DIET CHERRY C
      -                                             "OKE".
           05  FILLER             PIC X(16)         VALUE "SPRITE".

       01  POP-NAMES-TABLE REDEFINES POP-NAME.
           05  POP-TABLE OCCURS 6.
               10  POP-LIT        PIC X(16).

       01  I-DATE.
           05  I-YEAR             PIC 9(4).
           05  I-MONTH            PIC 99.
           05  I-DAY              PIC 99.

       01  COMPANY-TITLE.
           05  FILLER             PIC X(6)          VALUE "DATE: ".
           05  O-MONTH            PIC 99.
           05  FILLER             PIC X             VALUE "/".
           05  O-DAY              PIC 99.
           05  FILLER             PIC X             VALUE "/".
           05  O-YEAR             PIC 9(4).
           05  FILLER             PIC X(36)         VALUE SPACES.
           05  FILLER             PIC X(6)          VALUE "ALBIA ".
           05  FILLER             PIC X(7)          VALUE "SOCCER ".
           05  FILLER             PIC X(5)          VALUE "CLUB ".
           05  FILLER             PIC X(10)         VALUE "FUNDRAISER".
           05  FILLER             PIC X(44)         VALUE SPACES.
           05  FILLER             PIC X(6)          VALUE "PAGE: ".
           05  O-PCTR             PIC Z9.
           05  O-ERR-PCTR         PIC Z9.

       01  DIVISION-TITLE.
           05  FILLER             PIC X(8)          VALUE "CBLNB06".
           05  FILLER             PIC X(48)         VALUE SPACES.
           05  FILLER             PIC X(9)          VALUE "BUSATTO'S".
           05  FILLER             PIC X(2)          VALUE SPACES.
           05  FILLER             PIC X(8)          VALUE "DIVISION".
           05  FILLER             PIC X(56)         VALUE SPACES.

       01  REPORT-TITLE.
           05  FILLER             PIC X(60)         VALUE SPACES.
           05  FILLER             PIC X(6)          VALUE "SALES ".
           05  FILLER             PIC X(6)          VALUE "REPORT".
           05  FILLER             PIC X(59)         VALUE SPACES.

       01  COLUMN-HDG.
           05  FILLER             PIC X(3)          VALUE SPACES.
           05  FILLER             PIC X(9)          VALUE "LAST NAME".
           05  FILLER             PIC X(8)          VALUE SPACES.
           05  FILLER             PIC X(10)         VALUE "FIRST NAME".
           05  FILLER             PIC X(7)          VALUE SPACES.
           05  FILLER             PIC X(4)          VALUE "CITY".
           05  FILLER             PIC X(8)          VALUE SPACES.
           05  FILLER             PIC X(6)          VALUE "STATE ".
           05  FILLER             PIC X(8)          VALUE "ZIP CODE".
           05  FILLER             PIC X(4)          VALUE SPACES.
           05  FILLER             PIC X(8)          VALUE "POP TYPE".
           05  FILLER             PIC X(13)         VALUE SPACES.
           05  FILLER             PIC X(8)          VALUE "QUANTITY".
           05  FILLER             PIC X(6)          VALUE SPACES.
           05  FILLER             PIC X(11)         VALUE "DEPOSIT AMT".
           05  FILLER             PIC X(6)          VALUE SPACES.
           05  FILLER             PIC X(11)         VALUE "TOTAL SALES".
           05  FILLER             PIC XX            VALUE SPACES.

       01  DETAIL-LINE.
           05  FILLER             PIC X(3)          VALUE SPACES.
           05  O-LNAME            PIC X(15).
           05  FILLER             PIC XX            VALUE SPACES.
           05  O-FNAME            PIC X(15).
           05  FILLER             PIC XX            VALUE SPACES.
           05  O-CITY             PIC X(10).
           05  FILLER             PIC X(3)          VALUE SPACES.
           05  O-STATE            PIC XX.
           05  FILLER             PIC X(3)          VALUE SPACES.
           05  O-ZIP-1            PIC X(5).
           05  FILLER             PIC X             VALUE "-".
           05  O-ZIP-2            PIC X(4).
           05  FILLER             PIC XX            VALUE SPACES.
           05  O-POP-LIT          PIC X(16).
           05  FILLER             PIC X(8)          VALUE SPACES.
           05  O-QTY              PIC Z9.
           05  FILLER             PIC X(11)         VALUE SPACES.
           05  O-DEPOSIT-AMT      PIC $$$$.99.
           05  FILLER             PIC X(9)          VALUE SPACES.
           05  O-TOT-SALES        PIC $$,$$$.99.
           05  FILLER             PIC X(3)          VALUE SPACES.

       01  GT-HDG.
           05  FILLER             PIC X(6)          VALUE "GRAND ".
           05  FILLER             PIC X(8)          VALUE "TOTALS: ".
           05  FILLER             PIC X(118)        VALUE SPACES.

       01  POP-TOTALS.
           05  FILLER             PIC X(3)          VALUE SPACES.
           05  FILLER             OCCURS 3.
               10  O-POP          PIC X(16).
               10  FILLER         PIC X             VALUE SPACES.
               10  O-POP-AMT      PIC ZZZ,ZZ9.
               10  FILLER         PIC X(6)          VALUE SPACES.
           05  FILLER             PIC X(40)         VALUE SPACES.

       01  TEAM-TOT-HDG.
           05  FILLER             PIC X(5)          VALUE "TEAM ".
           05  FILLER             PIC X(7)          VALUE "TOTALS:".
           05  FILLER             PIC X(119)        VALUE SPACES.

       01  TEAM-TOT-DETAILS.
           05  FILLER             PIC X(3)          VALUE SPACES.
           05  O-TEAM             PIC X.
           05  FILLER             PIC X             VALUE SPACES.
           05  O-GT-RAISED        PIC $$$$,$$$,$$$.99.
           05  FILLER             PIC X(111)        VALUE SPACES.

       01  ERR-REPORT-TITLE.
           05  FILLER             PIC X(60)         VALUE SPACES.
           05  FILLER             PIC X(6)          VALUE "ERROR ".
           05  FILLER             PIC X(6)          VALUE "REPORT".
           05  FILLER             PIC X(59)         VALUE SPACES.

       01  ERR-REPORT-HDG.
           05  FILLER             PIC X(6)          VALUE "ERROR ".
           05  FILLER             PIC X(6)          VALUE "RECORD".
           05  FILLER             PIC X(60)         VALUE SPACES.
           05  FILLER             PIC X(6)          VALUE "ERROR ".
           05  FILLER             PIC X(11)         VALUE "DESCRIPTION".
           05  FILLER             PIC X(42)         VALUE SPACES.

       01  ERR-REPORT-DETAILS.
           05  O-ERR-REC          PIC X(71).
           05  FILLER             PIC X(1)          VALUE SPACES.
           05  O-ERR-MSG          PIC X(59).

       01  ERR-REPORT-TOT.
           05  FILLER             PIC X(6)          VALUE "TOTAL ".
           05  FILLER             PIC X(7)          VALUE "ERRORS ".
           05  O-TOT-ERR          PIC Z,ZZ9.

      *-----------------------------------------------------------------

       PROCEDURE DIVISION.

       0000-CBLNB06.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = "NO".
           PERFORM 3000-CLOSING.
           STOP RUN.

      *----------------------
      
       1000-INIT.

           MOVE FUNCTION CURRENT-DATE TO I-DATE.
           MOVE I-YEAR TO O-YEAR.
           MOVE I-MONTH TO O-MONTH.
           MOVE I-DAY TO O-DAY.

           OPEN INPUT POP-MASTER.
           OPEN OUTPUT PRTOUT.
           OPEN OUTPUT ERR-PRTOUT.

           PERFORM 9200-ERR-HDG.
           PERFORM 9100-HDG.
           PERFORM 9000-READ.

           PERFORM VARYING X FROM 1 BY 1
             UNTIL X > 5
               MOVE 0 TO C-GT-RAISED(X).

           PERFORM VARYING Y FROM 1 BY 1
             UNTIL Y > 6
               MOVE 0 TO C-POP-TOT(Y).

      *----------------------
      
       2000-MAINLINE.

           PERFORM 2100-VALIDATION THRU 2100-EXIT.

           IF ERR-SW = "YES"
               PERFORM 2200-ERROR-RPT
               PERFORM 9000-READ
           ELSE
               PERFORM 2300-CALCS
               PERFORM 2400-OUTPUT
               PERFORM 9000-READ
           END-IF.

      *----------------------
      *Perform validation checking for errors in the records
       2100-VALIDATION.

           MOVE "YES" TO ERR-SW.

           IF I-LNAME = SPACES
               MOVE T-ERR(1) TO O-ERR-MSG
               GO TO 2100-EXIT.

           IF I-FNAME = SPACES
               MOVE T-ERR(2) TO O-ERR-MSG
               GO TO 2100-EXIT.

           IF I-ADDRESS = SPACES
               MOVE T-ERR(3) TO O-ERR-MSG
               GO TO 2100-EXIT.

           IF I-CITY = SPACES
               MOVE T-ERR(4) TO O-ERR-MSG
               GO TO 2100-EXIT.

           IF NOT VAL-STATE
               MOVE T-ERR(5) TO O-ERR-MSG
               GO TO 2100-EXIT.

           IF I-ZIP NOT NUMERIC
               MOVE T-ERR(6) TO O-ERR-MSG
               GO TO 2100-EXIT.

           IF I-POP-TYPE NOT NUMERIC
               MOVE T-ERR(7) TO O-ERR-MSG
               GO TO 2100-EXIT.

           IF NOT VAL-POP-TYPE
               MOVE T-ERR(8) TO O-ERR-MSG
               GO TO 2100-EXIT.

           IF I-NUM-CASES NOT NUMERIC
               MOVE T-ERR(9) TO O-ERR-MSG
               GO TO 2100-EXIT.

           IF I-NUM-CASES < 1
               MOVE T-ERR(10) TO O-ERR-MSG
               GO TO 2100-EXIT.

           IF NOT VAL-TEAM
               MOVE T-ERR(11) TO O-ERR-MSG
               GO TO 2100-EXIT.

           MOVE "NO" TO ERR-SW. 

      *----------------------

       2100-EXIT.

           EXIT.

      *----------------------
      *Print error records in error file
       2200-ERROR-RPT.

           MOVE I-REC TO O-ERR-REC.
           WRITE PRTLINE2 FROM ERR-REPORT-DETAILS
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9200-ERR-HDG.

           ADD 1 TO C-ERR-CTR.

      *----------------------
     
       2300-CALCS.

           PERFORM VARYING Y FROM 1 BY 1
                UNTIL STATE(Y) = I-STATE.
                   COMPUTE C-DEPOSIT-AMT = (DEPOSIT(Y) * 24) * 
                       I-NUM-CASES.

           COMPUTE C-TOT-CASES ROUNDED = I-NUM-CASES * C-CASE-COST.
           COMPUTE C-TOT-SALES ROUNDED = C-TOT-CASES + C-DEPOSIT-AMT.

           ADD I-NUM-CASES TO C-POP-TOT(I-POP-TYPE).

           PERFORM VARYING Z FROM 1 BY 1
               UNTIL TEAM-NAME(Z) = I-TEAM.
                   ADD C-TOT-SALES TO C-GT-RAISED(Z).

      *----------------------
      *Format and output detail lines
       2400-OUTPUT.

           MOVE I-LNAME TO O-LNAME.
           MOVE I-FNAME TO O-FNAME.
           MOVE I-CITY TO O-CITY.
           MOVE I-STATE TO O-STATE.
           MOVE I-ZIP-1 TO O-ZIP-1.
           MOVE I-ZIP-2 TO O-ZIP-2.
           
           MOVE POP-LIT(I-POP-TYPE) TO O-POP-LIT.

           MOVE I-NUM-CASES TO O-QTY.
           MOVE C-DEPOSIT-AMT TO O-DEPOSIT-AMT.
           MOVE C-TOT-SALES TO O-TOT-SALES.

      * Print detail line
           WRITE PRTLINE
               FROM DETAIL-LINE
                 AFTER ADVANCING 2 LINES
                    AT EOP 
                      PERFORM 9100-HDG.
       
      *----------------------

       3000-CLOSING.

           PERFORM 9100-HDG.
           PERFORM 3100-GRANDTOTALS.
           PERFORM 3200-TEAMTOT.

           CLOSE POP-MASTER.
           CLOSE PRTOUT.
           CLOSE ERR-PRTOUT.
               
      *----------------------
      
       3100-GRANDTOTALS.

           WRITE PRTLINE
               FROM GT-HDG
                   AFTER ADVANCING 3 LINES.
      *Format and Print first and second line of pop GT

           MOVE 0 TO X.
        
           PERFORM VARYING X FROM 1 BY 1
             UNTIL X > 3
               MOVE POP-LIT(X) TO O-POP(X)
               MOVE C-POP-TOT(X) TO O-POP-AMT(X).

               WRITE PRTLINE
                       FROM POP-TOTALS
                           AFTER ADVANCING 2 LINES.

           PERFORM VARYING X FROM 4 BY 1
             UNTIL X > 6
               MOVE POP-LIT(X) TO O-POP(X - 3)
               MOVE C-POP-TOT(X) TO O-POP-AMT(X - 3).

               WRITE PRTLINE
                       FROM POP-TOTALS
                           AFTER ADVANCING 2 LINES.

      *Format and print error total       

           MOVE C-ERR-CTR TO O-TOT-ERR.

           WRITE PRTLINE2
               FROM ERR-REPORT-TOT
                   AFTER ADVANCING 3 LINES.

      *----------------------
      *Format and output the team grandtotals
       3200-TEAMTOT.

           WRITE PRTLINE
               FROM TEAM-TOT-HDG
                   AFTER ADVANCING 3 LINES.

           MOVE 0 TO X.

           PERFORM VARYING X FROM 1 BY 1
             UNTIL X > 5
               MOVE TEAM-NAME(X) TO O-TEAM
               MOVE C-GT-RAISED(X) TO O-GT-RAISED
               WRITE PRTLINE
                   FROM TEAM-TOT-DETAILS
                       AFTER ADVANCING 2 LINES.

      *----------------------

       9000-READ.

      * Read .dat file
           READ POP-MASTER
               AT END
                   MOVE "NO" TO MORE-RECS.

      *----------------------
      *Detail headings
       9100-HDG.

           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.

           WRITE PRTLINE
               FROM COMPANY-TITLE
                   AFTER ADVANCING PAGE.

           WRITE PRTLINE
               FROM DIVISION-TITLE
                   AFTER ADVANCING 1 LINE.

           WRITE PRTLINE
               FROM REPORT-TITLE
                   AFTER ADVANCING 1 LINE.

           WRITE PRTLINE
               FROM COLUMN-HDG
                   AFTER ADVANCING 2 LINES.

      *----------------------

       9200-ERR-HDG.

           ADD 1 TO C-ERR-PCTR.
               MOVE C-ERR-PCTR TO O-ERR-PCTR.

           WRITE PRTLINE2
               FROM COMPANY-TITLE.

           WRITE PRTLINE2
               FROM DIVISION-TITLE
                   AFTER ADVANCING 1 LINE.

           WRITE PRTLINE2
               FROM ERR-REPORT-TITLE
                   AFTER ADVANCING 1 LINE.

           WRITE PRTLINE2
               FROM ERR-REPORT-HDG
                   AFTER ADVANCING 2 LINES.
           
       END PROGRAM CBLNB06.
