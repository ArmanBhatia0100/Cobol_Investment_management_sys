       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVESTMENT-REPRT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STOCKS-FILE ASSIGN TO '..\STOCKS.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PORTFOLIO-FILE ASSIGN TO '..\PORTFOLIO.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REPORT-FILE ASSIGN TO '..\REPORT-OUT.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STOCKS-FILE.
       01 STOCK-RECORD.
           05 STOCK-SYMBOL-IN      PIC X(7).
           05 STOCK-NAME-IN        PIC X(25).
           05 CLOSING-PRICE-IN     PIC 9(4)V99.

       FD PORTFOLIO-FILE.
       01 INVESTMENT-RECORD.
           05 INVESTMENT-SYMBOL    PIC X(7).
           05 INVESTMENT-SHARES    PIC 9(5).
           05 AVERAGE-COST         PIC 9(4)V99.

       FD REPORT-FILE.
       01 REPORT-RECORD            PIC X(132).

       WORKING-STORAGE SECTION.
       01 STOCK-TABLE.
           05 STOCK-ENTRY OCCURS 20 TIMES.
               10 STOCK-SYMBOL     PIC X(7).
               10 STOCK-NAME       PIC X(25).
               10 CLOSING-PRICE    PIC 9(4)V99.
       01 STOCK-COUNT              PIC 99 VALUE 0.

       01 END-OF-FILE              PIC X VALUE 'N'.
       01 RECORD-READ-COUNT        PIC 9(3) VALUE 0.
       01 RECORD-WRITE-COUNT       PIC 9(3) VALUE 0.
       01 I                        PIC 99.
       01 FOUND-INDEX              PIC 99.


       01 CALC-COST-BASE           PIC 9(9)V99.
       01 CALC-MARKET-VALUE        PIC 9(9)V99.
       01 CALC-GAIN-LOSS           PIC S9(9)V99.

       01 EDITED-SHARES            PIC ZZ,ZZ9.
       01 EDITED-UNIT-COST         PIC $Z,ZZZ,ZZ9.99.
       01 EDITED-CLOSING-PRICE     PIC $Z,ZZZ,ZZ9.99.
       01 EDITED-COST-BASE         PIC $ZZ,ZZZ,ZZ9.99.
       01 EDITED-MARKET-VALUE      PIC $ZZ,ZZZ,ZZ9.99.
       01 EDITED-GAIN-LOSS         PIC $ZZ,ZZZ,ZZ9.99-.

       01 WS-REPORT-LINE.
           05 WS-STOCK-NAME        PIC X(25).
           05 FILLER               PIC X(3) VALUE SPACES.

           05 WS-SHARES            PIC ZZ,ZZ9.
           05 FILLER               PIC X(3) VALUE SPACES.

           05 WS-UNIT-COST         PIC $$,$$9.99.
           05 FILLER               PIC X(1) VALUE SPACES.

           05 WS-CLOSING-PRICE     PIC $$$$,$$9.99.
           05 FILLER               PIC X(3) VALUE SPACES.

           05 WS-COST-BASE         PIC $$$,$$9.99.
           05 FILLER               PIC X(3) VALUE SPACES.

           05 WS-MARKET-VALUE      PIC $$,$$,$$9.99.
           05 FILLER               PIC X(3) VALUE SPACES.

           05 WS-GAIN-LOSS         PIC $$$,$$9.99-.

        01 WS-HEADER-LINE.
           05 HEADER-STOCK-NAME        PIC X(25).
           05 FILLER               PIC X(3) VALUE SPACES.

           05 HEADER-SHARES            PIC X(6).
           05 FILLER               PIC X(3) VALUE SPACES.

           05 HEADER-UNIT-COST         PIC X(9).
           05 FILLER               PIC X(3) VALUE SPACES.

           05 HEADER-CLOSING-PRICE     PIC X(9).
           05 FILLER               PIC X(4) VALUE SPACES.

           05 HEADER-COST-BASE         PIC X(10).
           05 FILLER               PIC X(4) VALUE SPACES.

           05 HEADER-MARKET-VALUE      PIC X(10).
           05 FILLER               PIC X(5) VALUE SPACES.

           05 HEADER-GAIN-LOSS         PIC X(10).

       01 SEPARATOR-LINE           PIC X(132) VALUE ALL '='.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZATION
           PERFORM PROCESS-RECORDS
           PERFORM TERMINATION
           STOP RUN.

       INITIALIZATION.
           OPEN INPUT STOCKS-FILE
           OPEN INPUT PORTFOLIO-FILE
           OPEN OUTPUT REPORT-FILE
           PERFORM LOAD-STOCK-TABLE
           PERFORM WRITE-HEADER
           PERFORM WRITE-SEPARATOR.

       LOAD-STOCK-TABLE.
           MOVE 1 TO I
           MOVE 'N' TO END-OF-FILE
           PERFORM UNTIL END-OF-FILE = 'Y' OR I > 20
               READ STOCKS-FILE INTO STOCK-RECORD
                   AT END
                       MOVE 'Y' TO END-OF-FILE
                   NOT AT END
                       MOVE STOCK-SYMBOL-IN TO STOCK-SYMBOL(I)
                       MOVE STOCK-NAME-IN TO STOCK-NAME(I)
                       MOVE CLOSING-PRICE-IN TO CLOSING-PRICE(I)
                       ADD 1 TO I
               END-READ
           END-PERFORM
           SUBTRACT 1 FROM I
           MOVE I TO STOCK-COUNT.

       WRITE-HEADER.
           MOVE 'STOK NAME ' TO HEADER-STOCK-NAME
           MOVE '#SHARES' TO HEADER-SHARES
           MOVE 'UNIT COST' TO HEADER-UNIT-COST
           MOVE 'AT CLOSING' TO HEADER-CLOSING-PRICE
           MOVE 'C0ST BASE' TO HEADER-COST-BASE
           MOVE 'MARKET VALUE' TO HEADER-MARKET-VALUE
           MOVE 'GAIN/LOES' TO HEADER-GAIN-LOSS
           MOVE WS-HEADER-LINE TO REPORT-RECORD
           WRITE REPORT-RECORD.


       WRITE-SEPARATOR.
           MOVE SEPARATOR-LINE TO REPORT-RECORD
           WRITE REPORT-RECORD.

       PROCESS-RECORDS.
           MOVE 'N' TO END-OF-FILE
           PERFORM UNTIL END-OF-FILE = 'Y'
               READ PORTFOLIO-FILE INTO INVESTMENT-RECORD
                   AT END
                       MOVE 'Y' TO END-OF-FILE
                   NOT AT END
                       ADD 1 TO RECORD-READ-COUNT
                       PERFORM PROCESS-INVESTMENT-RECORD
               END-READ
           END-PERFORM.

       PROCESS-INVESTMENT-RECORD.
           MOVE 0 TO FOUND-INDEX
           PERFORM VARYING I FROM 1 BY 1 UNTIL
           I > STOCK-COUNT OR FOUND-INDEX > 0
               IF STOCK-SYMBOL(I) = INVESTMENT-SYMBOL
                   MOVE I TO FOUND-INDEX
               END-IF
           END-PERFORM
           IF FOUND-INDEX > 0
               COMPUTE CALC-COST-BASE = INVESTMENT-SHARES * AVERAGE-COST
               COMPUTE CALC-MARKET-VALUE =
               INVESTMENT-SHARES * CLOSING-PRICE(FOUND-INDEX)
               COMPUTE CALC-GAIN-LOSS =
               CALC-MARKET-VALUE - CALC-COST-BASE
               MOVE STOCK-NAME(FOUND-INDEX) TO WS-STOCK-NAME
               MOVE INVESTMENT-SHARES TO EDITED-SHARES
               MOVE EDITED-SHARES TO WS-SHARES
               MOVE AVERAGE-COST TO EDITED-UNIT-COST
               MOVE EDITED-UNIT-COST TO WS-UNIT-COST
               MOVE CLOSING-PRICE(FOUND-INDEX) TO EDITED-CLOSING-PRICE
               MOVE EDITED-CLOSING-PRICE TO WS-CLOSING-PRICE
               MOVE CALC-COST-BASE TO EDITED-COST-BASE
               MOVE EDITED-COST-BASE TO WS-COST-BASE
               MOVE CALC-MARKET-VALUE TO EDITED-MARKET-VALUE
               MOVE EDITED-MARKET-VALUE TO WS-MARKET-VALUE
               MOVE CALC-GAIN-LOSS TO EDITED-GAIN-LOSS
               MOVE EDITED-GAIN-LOSS TO WS-GAIN-LOSS
               MOVE WS-REPORT-LINE TO REPORT-RECORD
               WRITE REPORT-RECORD
               ADD 1 TO RECORD-WRITE-COUNT
           END-IF.

       TERMINATION.
           PERFORM WRITE-SEPARATOR
           MOVE SPACES TO REPORT-RECORD
           STRING 'Records read:    ' DELIMITED BY SIZE
               RECORD-READ-COUNT DELIMITED BY SIZE
               '   Records written:   ' DELIMITED BY SIZE
               RECORD-WRITE-COUNT DELIMITED BY SIZE
               INTO REPORT-RECORD
           WRITE REPORT-RECORD
           CLOSE STOCKS-FILE
           CLOSE PORTFOLIO-FILE
           CLOSE REPORT-FILE.
