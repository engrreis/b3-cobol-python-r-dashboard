       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRICEREPORT-ENGINE-WEEK.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-IN ASSIGN TO "data/pricereport_week.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQ-OUT ASSIGN TO "data/pricereport_week_cobol.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-IN.
       01 LINHA-IN                 PIC X(300).

       FD ARQ-OUT.
       01 LINHA-OUT                PIC X(300).

       WORKING-STORAGE SECTION.
       01 WS-EOF                   PIC X VALUE "N".
       01 CABECALHO-LIDO           PIC X VALUE "N".

       * Campos texto lidos do CSV
       01 WS-TRADEDATE-TXT         PIC X(10).
       01 WS-TICKER-TXT            PIC X(12).
       01 WS-NTLFINVOL-TXT         PIC X(20).
       01 WS-INTLFINVOL-TXT        PIC X(20).
       01 WS-QTY-TXT               PIC X(20).
       01 WS-FRSTPRIC-TXT          PIC X(20).
       01 WS-MINPRIC-TXT           PIC X(20).
       01 WS-MAXPRIC-TXT           PIC X(20).
       01 WS-LASTPRIC-TXT          PIC X(20).

       * Campos numéricos
       01 WS-NTLFINVOL-NUM         PIC 9(13)V9(4) VALUE 0.
       01 WS-QTY-NUM               PIC 9(13)      VALUE 0.
       01 WS-FRSTPRIC-NUM          PIC 9(5)V9(4)  VALUE 0.
       01 WS-MINPRIC-NUM           PIC 9(5)V9(4)  VALUE 0.
       01 WS-MAXPRIC-NUM           PIC 9(5)V9(4)  VALUE 0.
       01 WS-LASTPRIC-NUM          PIC 9(5)V9(4)  VALUE 0.

       * Métricas derivadas (por ticker/dia)
       01 WS-VOL-MILLIONS          PIC 9(9)V9(4)    VALUE 0.
       01 WS-RET-INTRADAY          PIC S9(3)V9(6)   VALUE 0.
       01 WS-VOL-INTRADAY          PIC S9(3)V9(6)   VALUE 0.

       * Acumuladores simples (para futuros usos)
       01 WS-TOTAL-REGS            PIC 9(9)        VALUE 0.
       01 WS-TOTAL-VOLUME-MIL      PIC 9(13)V9(4)  VALUE 0.

       * Controle de tempo (hhmmsscc -> centésimos)
       01 WS-START-TIME            PIC 9(8).
       01 WS-END-TIME              PIC 9(8).

       01 WS-START-HH              PIC 99.
       01 WS-START-MM              PIC 99.
       01 WS-START-SS              PIC 99.
       01 WS-START-CS              PIC 99.

       01 WS-END-HH                PIC 99.
       01 WS-END-MM                PIC 99.
       01 WS-END-SS                PIC 99.
       01 WS-END-CS                PIC 99.

       01 WS-START-TOTAL-CS        PIC 9(9)        VALUE 0.
       01 WS-END-TOTAL-CS          PIC 9(9)        VALUE 0.
       01 WS-ELAPSED-CS            PIC 9(9)        VALUE 0.
       01 WS-ELAPSED-SEC           PIC 9(7)V9(2)   VALUE 0.

       PROCEDURE DIVISION.
       MAIN-SECTION.
           DISPLAY "=== PRICEREPORT ENGINE WEEK (COBOL) ===".

      * Marca o tempo de início
           ACCEPT WS-START-TIME FROM TIME
           PERFORM CONVERTE-START-TIME.

           OPEN INPUT ARQ-IN
                OUTPUT ARQ-OUT.

      * Cabeçalho do CSV tratado
           MOVE "TradeDate,TickerSymbol,NtlFinVol,FinInstrmQty,FrstPric," &
                "MinPric,MaxPric,LastPric,NtlFinVol_Millions,RetIntraday,VolIntraday"
             TO LINHA-OUT
           WRITE LINHA-OUT.

      * Loop de leitura
           PERFORM UNTIL WS-EOF = "S"
              READ ARQ-IN INTO LINHA-IN
                 AT END
                    MOVE "S" TO WS-EOF
                 NOT AT END
                    PERFORM PROCESSA-LINHA
              END-READ
           END-PERFORM.

           CLOSE ARQ-IN ARQ-OUT.

      * Marca o tempo de fim
           ACCEPT WS-END-TIME FROM TIME
           PERFORM CONVERTE-END-TIME
           COMPUTE WS-ELAPSED-CS =
                WS-END-TOTAL-CS - WS-START-TOTAL-CS
           COMPUTE WS-ELAPSED-SEC =
                WS-ELAPSED-CS / 100.

           DISPLAY "TOTAL REGISTROS PROCESSADOS: " WS-TOTAL-REGS
           DISPLAY "VOLUME TOTAL (R$ milhoes):   " WS-TOTAL-VOLUME-MIL
           DISPLAY "TEMPO COBOL (segundos):      " WS-ELAPSED-SEC.

           DISPLAY "=== FIM PRICEREPORT ENGINE WEEK ===".
           STOP RUN.
              WS-MAXPRIC-TXT
              WS-LASTPRIC-TXT.

      * Quebra por vírgula
                  WS-TICKER-TXT
                  WS-NTLFINVOL-TXT
                  WS-INTLFINVOL-TXT
                  WS-QTY-TXT
                  WS-FRSTPRIC-TXT
                  WS-MINPRIC-TXT
                  WS-MAXPRIC-TXT
                  WS-LASTPRIC-TXT
           END-UNSTRING.

      * Converte textos em números (NUMVAL aceita . ou ,)
           MOVE FUNCTION NUMVAL(WS-NTLFINVOL-TXT) TO WS-NTLFINVOL-NUM
           MOVE FUNCTION NUMVAL(WS-QTY-TXT)       TO WS-QTY-NUM
           MOVE FUNCTION NUMVAL(WS-FRSTPRIC-TXT)  TO WS-FRSTPRIC-NUM
           MOVE FUNCTION NUMVAL(WS-MINPRIC-TXT)   TO WS-MINPRIC-NUM
           MOVE FUNCTION NUMVAL(WS-MAXPRIC-TXT)   TO WS-MAXPRIC-NUM
           MOVE FUNCTION NUMVAL(WS-LASTPRIC-TXT)  TO WS-LASTPRIC-NUM

      * Volume em milhões
           IF WS-NTLFINVOL-NUM > 0
              COMPUTE WS-VOL-MILLIONS =
                  WS-NTLFINVOL-NUM / 1000000
           ELSE
              MOVE 0 TO WS-VOL-MILLIONS
           END-IF

      * Retorno intraday: (Last / First - 1)
           IF WS-FRSTPRIC-NUM > 0
              COMPUTE WS-RET-INTRADAY =
                  (WS-LASTPRIC-NUM / WS-FRSTPRIC-NUM) - 1
           ELSE
              MOVE 0 TO WS-RET-INTRADAY
           END-IF

      * Volatilidade intraday simplificada: (Max - Min) / First
           IF WS-FRSTPRIC-NUM > 0
              COMPUTE WS-VOL-INTRADAY =
                  (WS-MAXPRIC-NUM - WS-MINPRIC-NUM)
                    / WS-FRSTPRIC-NUM
           ELSE
              MOVE 0 TO WS-VOL-INTRADAY
           END-IF

      * Acumuladores
           ADD 1              TO WS-TOTAL-REGS
           ADD WS-VOL-MILLIONS TO WS-TOTAL-VOLUME-MIL

      * Monta linha de saída
           STRING
              WS-TRADEDATE-TXT      DELIMITED BY SPACE
              ","                   DELIMITED BY SIZE
              WS-TICKER-TXT         DELIMITED BY SPACE
              ","                   DELIMITED BY SIZE
              WS-NTLFINVOL-TXT      DELIMITED BY SPACE
              ","                   DELIMITED BY SIZE
              WS-QTY-TXT            DELIMITED BY SPACE
              ","                   DELIMITED BY SIZE
              WS-FRSTPRIC-TXT       DELIMITED BY SPACE
              ","                   DELIMITED BY SIZE
              WS-MINPRIC-TXT        DELIMITED BY SPACE
              ","                   DELIMITED BY SIZE
              WS-MAXPRIC-TXT        DELIMITED BY SPACE
              ","                   DELIMITED BY SIZE
              WS-LASTPRIC-TXT       DELIMITED BY SPACE
              ","                   DELIMITED BY SIZE
              WS-VOL-MILLIONS       DELIMITED BY SIZE
              ","                   DELIMITED BY SIZE
              WS-RET-INTRADAY       DELIMITED BY SIZE
              ","                   DELIMITED BY SIZE
              WS-VOL-INTRADAY       DELIMITED BY SIZE
             INTO LINHA-OUT
           END-STRING

           WRITE LINHA-OUT.

       CONVERTE-START-TIME.
      * WS-START-TIME = hhmmsscc
           MOVE WS-START-TIME(1:2) TO WS-START-HH
           MOVE WS-START-TIME(3:2) TO WS-START-MM
           MOVE WS-START-TIME(5:2) TO WS-START-SS
           MOVE WS-START-TIME(7:2) TO WS-START-CS

           COMPUTE WS-START-TOTAL-CS =
               (((WS-START-HH * 60) + WS-START-MM) * 60 + WS-START-SS) * 100
                 + WS-START-CS
           .

       CONVERTE-END-TIME.
           MOVE WS-END-TIME(1:2) TO WS-END-HH
           MOVE WS-END-TIME(3:2) TO WS-END-MM
           MOVE WS-END-TIME(5:2) TO WS-END-SS
           MOVE WS-END-TIME(7:2) TO WS-END-CS

           COMPUTE WS-END-TOTAL-CS =
               (((WS-END-HH * 60) + WS-END-MM) * 60 + WS-END-SS) * 100
                 + WS-END-CS
           .
           UNSTRING LINHA-IN
             DELIMITED BY ","
             INTO WS-TRADEDATE-TXT
              WS-QTY-TXT
              WS-FRSTPRIC-TXT
              WS-MINPRIC-TXT

              WS-NTLFINVOL-TXT
              WS-INTLFINVOL-TXT
           END-IF
              WS-TRADEDATE-TXT
              WS-TICKER-TXT

      * Limpa variáveis texto
           MOVE SPACES TO
       PROCESSA-LINHA.
      * pula o cabeçalho original do CSV de entrada
              EXIT PARAGRAPH

