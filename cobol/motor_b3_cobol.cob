>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MOTOR-B3-COBOL.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT PRICEREPORT-IN
        ASSIGN TO "data/pricereport_acoes.csv"
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT DAILY-RET-FILE
        ASSIGN TO "processados/daily_returns_cobol.csv"
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT ROLLING-OUT
        ASSIGN TO "processados/rolling_metrics_by_ticker_cobol.csv"
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT ROLLING-IN
        ASSIGN TO "processados/rolling_metrics_by_ticker_cobol.csv"
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT MARKET-OUT
        ASSIGN TO "processados/rolling_market_metrics_cobol.csv"
        ORGANIZATION IS LINE SEQUENTIAL.

    SELECT STATS-OUT
        ASSIGN TO "processados/motor_b3_cobol_stats.csv"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.

FD  PRICEREPORT-IN.
01  PRICEREPORT-RECORD   PIC X(512).

FD  DAILY-RET-FILE.
01  DAILY-RET-REC        PIC X(256).

FD  ROLLING-OUT.
01  ROLLING-REC          PIC X(512).

FD  ROLLING-IN.
01  ROLLING-IN-REC       PIC X(512).

FD  MARKET-OUT.
01  MARKET-REC           PIC X(256).

FD  STATS-OUT.
01  STATS-REC            PIC X(256).

WORKING-STORAGE SECTION.

*> ---------- CONTROLE GERAL ----------
01  WS-EOF              PIC X     VALUE "N".
01  WS-HEADER-SKIPPED   PIC X     VALUE "N".
01  WS-TOTAL-LINES      PIC 9(9)  VALUE 0.
01  WS-VALID-LINES      PIC 9(9)  VALUE 0.
01  WS-TOTAL-RET        PIC 9(9)  VALUE 0.
01  WS-TOTAL-ROLLING    PIC 9(9)  VALUE 0.

*> ---------- FASE 1: CAMPOS DO CSV ORIGINAL ----------
01  WS-CSV-FIELDS.
    05  F-TRADEDATE   PIC X(20).
    05  F-TICKER      PIC X(20).
    05  F-INSTRTYPE   PIC X(20).
    05  F-MKTSTRM     PIC X(20).
    05  F-FINQTY      PIC X(20).
    05  F-NTLFINVOL   PIC X(20).
    05  F-INTLFINVOL  PIC X(20).
    05  F-RGLRTXSQTY  PIC X(20).
    05  F-FRSTPRIC    PIC X(20).
    05  F-MINPRIC     PIC X(20).
    05  F-MAXPRIC     PIC X(20).
    05  F-TRADAVRGP   PIC X(20).
    05  F-LASTPRIC    PIC X(20).

01  WS-ROW-DATA.
    05  WS-TRADE-DATE-TXT   PIC X(10).
    05  WS-TICKER           PIC X(12).
    05  WS-NTL-FIN-VOL      PIC 9(15)V9(4) VALUE 0.
    05  WS-RGLR-TXS-QTY     PIC 9(9)      VALUE 0.
    05  WS-LAST-PRICE       PIC 9(7)V9(4) VALUE 0.

*> Tabela de último preço por ticker (para retornos diários)
01  WS-TICKER-TABLE.
    05  WS-TICKER-ENTRY OCCURS 5000 TIMES.
        10  WS-TBL-TICKER      PIC X(12).
        10  WS-TBL-LAST-DATE   PIC X(10).
        10  WS-TBL-LAST-PRICE  PIC 9(7)V9(4).

01  WS-TBL-COUNT  PIC 9(4) VALUE 0.
01  WS-IDX        PIC 9(4) VALUE 0.
01  WS-FOUND      PIC X    VALUE "N".

*> Log-retorno e texto para saída da Fase 1
01  WS-RET-LOG        PIC S9(3)V9(8) VALUE 0.
01  WS-RET-LOG-TXT    PIC -9.9(8).
01  WS-LAST-PRICE-TXT PIC -9(5).9(4).
01  WS-NTLFINVOL-TXT  PIC 9(15).9(4).
01  WS-RGLRTXSQTY-TXT PIC 9(9).

*> ---------- FASE 2: LEITURA DE DAILY RETURNS ----------
01  DR-EOF            PIC X     VALUE "N".
01  DR-HEADER-SKIPPED PIC X     VALUE "N".

01  DR-CSV-FIELDS.
    05  DR-F-TRADEDATE   PIC X(20).
    05  DR-F-TICKER      PIC X(20).
    05  DR-F-RETLOG      PIC X(30).
    05  DR-F-LASTPRICE   PIC X(30).
    05  DR-F-NTLFINVOL   PIC X(30).
    05  DR-F-RGLRTXSQTY  PIC X(30).

01  DR-ROW-DATA.
    05  DR-TRADE-DATE-TXT   PIC X(10).
    05  DR-TICKER-TXT       PIC X(12).
    05  DR-RET-LOG-N        PIC S9(3)V9(8) VALUE 0.
    05  DR-LAST-PRICE-N     PIC 9(7)V9(4)  VALUE 0.
    05  DR-NTL-FIN-VOL-N    PIC 9(15)V9(4) VALUE 0.
    05  DR-RGLR-TXS-QTY-N   PIC 9(9)       VALUE 0.

*> ---------- FASE 2: TABELA DE JANELA ROLANTE POR TICKER ----------
01  WS-ROLL-TBL.
    05  RT-ENTRY OCCURS 5000 TIMES.
        10  RT-TICKER        PIC X(12).
        10  RT-N             PIC 9      VALUE 0.
        10  RT-LAST-IDX      PIC 9      VALUE 0.
        10  RT-R-RET     OCCURS 7 TIMES PIC S9(3)V9(8).
        10  RT-R-VOL     OCCURS 7 TIMES PIC 9(15)V9(4).
        10  RT-R-NEG     OCCURS 7 TIMES PIC 9(9).
        10  RT-SUM-R         PIC S9(5)V9(8) VALUE 0.
        10  RT-SUM-R2        PIC S9(7)V9(8) VALUE 0.
        10  RT-SUM-VOL       PIC 9(17)V9(4)  VALUE 0.
        10  RT-SUM-NEG       PIC 9(9)        VALUE 0.

01  WS-ROLL-COUNT      PIC 9(4) VALUE 0.
01  WS-ROLL-IDX        PIC 9(4) VALUE 0.
01  WS-ROLL-FOUND      PIC X    VALUE "N".
01  WS-NEWPOS          PIC 9    VALUE 0.

*> Variáveis para cálculo da janela
01  WS-NOBS            PIC 9     VALUE 0.
01  WS-SUM-R           PIC S9(5)V9(8) VALUE 0.
01  WS-SUM-R2          PIC S9(7)V9(8) VALUE 0.
01  WS-TMP-R2          PIC S9(7)V9(8) VALUE 0.
01  WS-SUM-VOL         PIC 9(17)V9(4)  VALUE 0.
01  WS-SUM-NEG         PIC 9(9)        VALUE 0.
01  WS-MEAN-R          PIC S9(3)V9(8)  VALUE 0.
01  WS-VAR-R           PIC S9(5)V9(8)  VALUE 0.
01  WS-VOL-DIARIA      PIC S9(3)V9(8)  VALUE 0.
01  WS-VOL-ANUAL       PIC S9(3)V9(8)  VALUE 0.
01  WS-RET-JANELA      PIC S9(3)V9(8)  VALUE 0.
01  WS-IND-LIQ         PIC 9(17)V9(4)  VALUE 0.

*> Constante sqrt(252) aproximada
01  WS-SQRT252         PIC 99V9(4) VALUE 15.8745.

*> Texto para saída da Fase 2
01  WS-NOBS-TXT        PIC 9(2).
01  WS-RET-JAN-TXT     PIC -9.9(8).
01  WS-VOL-DIA-TXT     PIC -9.9(8).
01  WS-VOL-ANU-TXT     PIC -9.9(8).
01  WS-VOLJAN-TXT      PIC 9(17).9(4).
01  WS-NEGJAN-TXT      PIC 9(9).
01  WS-INDLIQ-TXT      PIC 9(17).9(4).

*> ---------- FASE 3: MÉTRICAS DE MERCADO / HHI ----------
01  MK-EOF             PIC X     VALUE "N".
01  MK-HEADER-SKIPPED  PIC X     VALUE "N".

01  MK-CSV-FIELDS.
    05  MK-F-TRADEDATE  PIC X(20).
    05  MK-F-TICKER     PIC X(20).
    05  MK-F-NOBS       PIC X(10).
    05  MK-F-RETJAN     PIC X(30).
    05  MK-F-VOLDIA     PIC X(30).
    05  MK-F-VOLANU     PIC X(30).
    05  MK-F-VOLJAN     PIC X(30).
    05  MK-F-NEGJAN     PIC X(30).
    05  MK-F-INDLIQ     PIC X(30).

01  MK-ROW-DATA.
    05  MK-TRADE-DATE-TXT       PIC X(10).
    05  MK-VOLUME-JANELA-N      PIC 9(17)V9(4) VALUE 0.
    05  MK-NEG-JANELA-N         PIC 9(9)       VALUE 0.

01  MK-CURR-DATE         PIC X(10)       VALUE SPACES.
01  MK-SUM-VOL-N         PIC 9(17)V9(4)  VALUE 0.
01  MK-SUM-NEG           PIC 9(9)        VALUE 0.
01  MK-NTICKERS          PIC 9(5)        VALUE 0.

01  MK-VOLUME-JANELA-F   USAGE COMP-2    VALUE 0.
01  MK-SUM-VOL-F         USAGE COMP-2    VALUE 0.
01  MK-SUM-VOL-SQ-F      USAGE COMP-2    VALUE 0.
01  MK-HHI-F             USAGE COMP-2    VALUE 0.

01  MK-HHI-TXT           PIC 9V9(8).
01  MK-VOLTOT-TXT        PIC 9(17).9(4).
01  MK-NEGTOT-TXT        PIC 9(9).
01  MK-NTICKERS-TXT      PIC 9(5).

*> ---------- MEDIÇÃO DE TEMPO DE EXECUÇÃO ----------
01  WS-START-TS-R        PIC X(21)      VALUE SPACES.
01  WS-END-TS-R          PIC X(21)      VALUE SPACES.

01  WS-START-HH          PIC 9(2)       VALUE 0.
01  WS-START-MM          PIC 9(2)       VALUE 0.
01  WS-START-SS          PIC 9(2)       VALUE 0.

01  WS-END-HH            PIC 9(2)       VALUE 0.
01  WS-END-MM            PIC 9(2)       VALUE 0.
01  WS-END-SS            PIC 9(2)       VALUE 0.

01  WS-START-TOTSEC      PIC 9(7)       VALUE 0.
01  WS-END-TOTSEC        PIC 9(7)       VALUE 0.
01  WS-ELAPSED-SEC       PIC 9(7)       VALUE 0.

01  WS-START-ISO         PIC X(19)      VALUE SPACES.
01  WS-END-ISO           PIC X(19)      VALUE SPACES.
01  WS-ELAPSED-TXT       PIC 9(7)       VALUE 0.

PROCEDURE DIVISION.
MAIN-PARA.
    *> Marca horário de início
    PERFORM PH-GET-START-TIME

    PERFORM PHASE-1-GERA-RETORNOS
    PERFORM PHASE-2-GERA-ROLLING
    PERFORM PHASE-3-GERA-MARKET

    *> Marca horário de fim e grava stats
    PERFORM PH-GET-END-TIME
    PERFORM PH-WRITE-STATS

    DISPLAY "--------------------------------------------"
    DISPLAY "FIM MOTOR COBOL"
    DISPLAY "Linhas entrada (PriceReport).......: " WS-TOTAL-LINES
    DISPLAY "Linhas validas (preco/volume)......: " WS-VALID-LINES
    DISPLAY "Retornos diarios gerados............: " WS-TOTAL-RET
    DISPLAY "Linhas rolling por ticker geradas...: " WS-TOTAL-ROLLING
    DISPLAY "--------------------------------------------"

    GOBACK.

*> ==========================================================
*> FASE 1 - LEITURA DO CSV ORIGINAL E GERAÇÃO DE DAILY_RETURNS
*> ==========================================================
PHASE-1-GERA-RETORNOS.
    MOVE "N" TO WS-EOF
    MOVE "N" TO WS-HEADER-SKIPPED
    MOVE 0   TO WS-TOTAL-LINES
    MOVE 0   TO WS-VALID-LINES
    MOVE 0   TO WS-TOTAL-RET
    MOVE 0   TO WS-TBL-COUNT

    OPEN INPUT  PRICEREPORT-IN
         OUTPUT DAILY-RET-FILE

    *> Header do CSV de retornos (inclui volume e nº negócios)
    MOVE "TradeDate,TickerSymbol,ReturnLog,LastPrice,NtlFinVol,RglrTxsQty"
        TO DAILY-RET-REC
    WRITE DAILY-RET-REC

    PERFORM UNTIL WS-EOF = "Y"
        READ PRICEREPORT-IN
            AT END
                MOVE "Y" TO WS-EOF
            NOT AT END
                PERFORM PH1-PROCESS-RECORD
        END-READ
    END-PERFORM

    CLOSE PRICEREPORT-IN
          DAILY-RET-FILE
    .

PH1-PROCESS-RECORD.
    ADD 1 TO WS-TOTAL-LINES

    *> Pula o header do CSV original
    IF WS-HEADER-SKIPPED = "N"
        MOVE "Y" TO WS-HEADER-SKIPPED
        EXIT PARAGRAPH
    END-IF

    MOVE SPACES TO WS-CSV-FIELDS

    UNSTRING PRICEREPORT-RECORD
      DELIMITED BY ALL ","
      INTO
        F-TRADEDATE
        F-TICKER
        F-INSTRTYPE
        F-MKTSTRM
        F-FINQTY
        F-NTLFINVOL
        F-INTLFINVOL
        F-RGLRTXSQTY
        F-FRSTPRIC
        F-MINPRIC
        F-MAXPRIC
        F-TRADAVRGP
        F-LASTPRIC
    END-UNSTRING

    MOVE F-TRADEDATE(1:10) TO WS-TRADE-DATE-TXT
    MOVE F-TICKER          TO WS-TICKER

    *> Converte NtlFinVol
    IF F-NTLFINVOL NOT = SPACES
        COMPUTE WS-NTL-FIN-VOL =
            FUNCTION NUMVAL(F-NTLFINVOL)
    ELSE
        MOVE 0 TO WS-NTL-FIN-VOL
    END-IF

    *> Converte RglrTxsQty
    IF F-RGLRTXSQTY NOT = SPACES
        COMPUTE WS-RGLR-TXS-QTY =
            FUNCTION NUMVAL(F-RGLRTXSQTY)
    ELSE
        MOVE 0 TO WS-RGLR-TXS-QTY
    END-IF

    *> Converte LastPric
    IF F-LASTPRIC NOT = SPACES
        COMPUTE WS-LAST-PRICE =
            FUNCTION NUMVAL(F-LASTPRIC)
    ELSE
        MOVE 0 TO WS-LAST-PRICE
    END-IF

    IF WS-LAST-PRICE > 0 AND WS-NTL-FIN-VOL > 0
        ADD 1 TO WS-VALID-LINES
    END-IF

    *> Se não há preço, não calcula retorno nem grava daily_returns
    IF WS-LAST-PRICE = 0
        EXIT PARAGRAPH
    END-IF

    *> Procura ticker na tabela de último preço
    MOVE "N" TO WS-FOUND
    MOVE 0   TO WS-IDX

    PERFORM VARYING WS-IDX FROM 1 BY 1
        UNTIL WS-IDX > WS-TBL-COUNT OR WS-FOUND = "Y"
        IF WS-TBL-TICKER(WS-IDX) = WS-TICKER
            MOVE "Y" TO WS-FOUND
        END-IF
    END-PERFORM

    IF WS-FOUND = "N"
        *> Novo ticker: apenas cadastra, sem retorno
        IF WS-TBL-COUNT < 5000
            ADD 1 TO WS-TBL-COUNT
            MOVE WS-TICKER         TO WS-TBL-TICKER(WS-TBL-COUNT)
            MOVE WS-TRADE-DATE-TXT TO WS-TBL-LAST-DATE(WS-TBL-COUNT)
            MOVE WS-LAST-PRICE     TO WS-TBL-LAST-PRICE(WS-TBL-COUNT)
        END-IF
        EXIT PARAGRAPH
    END-IF

    *> Já existia ticker: calcula retorno
    IF WS-TBL-LAST-PRICE(WS-IDX) > 0
        COMPUTE WS-RET-LOG =
            FUNCTION LOG(WS-LAST-PRICE / WS-TBL-LAST-PRICE(WS-IDX))

        MOVE WS-RET-LOG       TO WS-RET-LOG-TXT
        MOVE WS-LAST-PRICE    TO WS-LAST-PRICE-TXT
        MOVE WS-NTL-FIN-VOL   TO WS-NTLFINVOL-TXT
        MOVE WS-RGLR-TXS-QTY  TO WS-RGLRTXSQTY-TXT

        MOVE SPACES TO DAILY-RET-REC
        STRING
            WS-TRADE-DATE-TXT      DELIMITED BY SIZE
            ","                    DELIMITED BY SIZE
            WS-TICKER              DELIMITED BY SIZE
            ","                    DELIMITED BY SIZE
            WS-RET-LOG-TXT         DELIMITED BY SIZE
            ","                    DELIMITED BY SIZE
            WS-LAST-PRICE-TXT      DELIMITED BY SIZE
            ","                    DELIMITED BY SIZE
            WS-NTLFINVOL-TXT       DELIMITED BY SIZE
            ","                    DELIMITED BY SIZE
            WS-RGLRTXSQTY-TXT      DELIMITED BY SIZE
            INTO DAILY-RET-REC
        END-STRING

        WRITE DAILY-RET-REC
        ADD 1 TO WS-TOTAL-RET
    END-IF

    *> Atualiza último preço do ticker
    MOVE WS-TRADE-DATE-TXT TO WS-TBL-LAST-DATE(WS-IDX)
    MOVE WS-LAST-PRICE     TO WS-TBL-LAST-PRICE(WS-IDX)

    .

*> ==========================================================
*> FASE 2 - LER DAILY_RETURNS E GERAR MÉTRICAS ROLANTES
*> ==========================================================
PHASE-2-GERA-ROLLING.
    MOVE "N" TO DR-EOF
    MOVE "N" TO DR-HEADER-SKIPPED
    MOVE 0   TO WS-ROLL-COUNT
    MOVE 0   TO WS-TOTAL-ROLLING

    OPEN INPUT  DAILY-RET-FILE
         OUTPUT ROLLING-OUT

    *> Header do CSV de rolling por ticker
    MOVE "TradeDate,TickerSymbol,NObsJanela,RetJanela,VolDiariaJanela,VolAnualJanela,VolumeJanela,NumNegociosJanela,IndiceLiquidez"
        TO ROLLING-REC
    WRITE ROLLING-REC

    PERFORM UNTIL DR-EOF = "Y"
        READ DAILY-RET-FILE
            AT END
                MOVE "Y" TO DR-EOF
            NOT AT END
                PERFORM PH2-PROCESS-RET
        END-READ
    END-PERFORM

    CLOSE DAILY-RET-FILE
          ROLLING-OUT
    .

PH2-PROCESS-RET.
    *> Pula header do daily_returns
    IF DR-HEADER-SKIPPED = "N"
        MOVE "Y" TO DR-HEADER-SKIPPED
        EXIT PARAGRAPH
    END-IF

    MOVE SPACES TO DR-CSV-FIELDS

    UNSTRING DAILY-RET-REC
      DELIMITED BY ALL ","
      INTO
        DR-F-TRADEDATE
        DR-F-TICKER
        DR-F-RETLOG
        DR-F-LASTPRICE
        DR-F-NTLFINVOL
        DR-F-RGLRTXSQTY
    END-UNSTRING

    MOVE DR-F-TRADEDATE(1:10) TO DR-TRADE-DATE-TXT
    MOVE DR-F-TICKER          TO DR-TICKER-TXT

    IF DR-F-RETLOG NOT = SPACES
        COMPUTE DR-RET-LOG-N =
            FUNCTION NUMVAL(DR-F-RETLOG)
    ELSE
        MOVE 0 TO DR-RET-LOG-N
    END-IF

    IF DR-F-LASTPRICE NOT = SPACES
        COMPUTE DR-LAST-PRICE-N =
            FUNCTION NUMVAL(DR-F-LASTPRICE)
    ELSE
        MOVE 0 TO DR-LAST-PRICE-N
    END-IF

    IF DR-F-NTLFINVOL NOT = SPACES
        COMPUTE DR-NTL-FIN-VOL-N =
            FUNCTION NUMVAL(DR-F-NTLFINVOL)
    ELSE
        MOVE 0 TO DR-NTL-FIN-VOL-N
    END-IF

    IF DR-F-RGLRTXSQTY NOT = SPACES
        COMPUTE DR-RGLR-TXS-QTY-N =
            FUNCTION NUMVAL(DR-F-RGLRTXSQTY)
    ELSE
        MOVE 0 TO DR-RGLR-TXS-QTY-N
    END-IF

    *> Se não tem retorno, não faz sentido janela
    IF DR-RET-LOG-N = 0
        EXIT PARAGRAPH
    END-IF

    *> Procura ticker na tabela de rolling
    MOVE "N" TO WS-ROLL-FOUND
    MOVE 0   TO WS-ROLL-IDX

    PERFORM VARYING WS-ROLL-IDX FROM 1 BY 1
        UNTIL WS-ROLL-IDX > WS-ROLL-COUNT OR WS-ROLL-FOUND = "Y"
        IF RT-TICKER(WS-ROLL-IDX) = DR-TICKER-TXT
            MOVE "Y" TO WS-ROLL-FOUND
        END-IF
    END-PERFORM

    IF WS-ROLL-FOUND = "N"
        IF WS-ROLL-COUNT < 5000
            ADD 1 TO WS-ROLL-COUNT
            MOVE DR-TICKER-TXT TO RT-TICKER(WS-ROLL-COUNT)
            MOVE 0 TO RT-N(WS-ROLL-COUNT)
            MOVE 0 TO RT-LAST-IDX(WS-ROLL-COUNT)
            MOVE 0 TO RT-SUM-R(WS-ROLL-COUNT)
            MOVE 0 TO RT-SUM-R2(WS-ROLL-COUNT)
            MOVE 0 TO RT-SUM-VOL(WS-ROLL-COUNT)
            MOVE 0 TO RT-SUM-NEG(WS-ROLL-COUNT)
            MOVE WS-ROLL-COUNT TO WS-ROLL-IDX
            MOVE "Y" TO WS-ROLL-FOUND
        ELSE
            EXIT PARAGRAPH
        END-IF
    END-IF

    *> Atualiza janela rolante (circular buffer de 7 posições)
    MOVE RT-N(WS-ROLL-IDX) TO WS-NOBS

    IF WS-NOBS < 7
        ADD 1 TO WS-NOBS
        MOVE WS-NOBS TO RT-N(WS-ROLL-IDX)
        MOVE WS-NOBS TO WS-NEWPOS
    ELSE
        *> janela cheia: sobrescreve o mais antigo
        IF RT-LAST-IDX(WS-ROLL-IDX) = 7
            MOVE 1 TO WS-NEWPOS
        ELSE
            ADD 1 TO RT-LAST-IDX(WS-ROLL-IDX)
            MOVE RT-LAST-IDX(WS-ROLL-IDX) TO WS-NEWPOS
        END-IF
    END-IF

    *> Se já havia 7, remover contribuições antigas da posição WS-NEWPOS
    IF RT-N(WS-ROLL-IDX) = 7 AND RT-SUM-R2(WS-ROLL-IDX) NOT = 0
        *> Remove contribuição antiga de r da soma
        SUBTRACT RT-R-RET(WS-ROLL-IDX, WS-NEWPOS)
            FROM RT-SUM-R(WS-ROLL-IDX)

        *> Calcula r^2 antigo em uma variável temporária
        COMPUTE WS-TMP-R2 =
            RT-R-RET(WS-ROLL-IDX, WS-NEWPOS)
            * RT-R-RET(WS-ROLL-IDX, WS-NEWPOS)

        *> Remove contribuição antiga de r^2
        SUBTRACT WS-TMP-R2
            FROM RT-SUM-R2(WS-ROLL-IDX)

        *> Remove volume e nº de negócios antigos da janela
        SUBTRACT RT-R-VOL(WS-ROLL-IDX, WS-NEWPOS)
            FROM RT-SUM-VOL(WS-ROLL-IDX)
        SUBTRACT RT-R-NEG(WS-ROLL-IDX, WS-NEWPOS)
            FROM RT-SUM-NEG(WS-ROLL-IDX)
    END-IF

    *> Escreve o novo valor na posição WS-NEWPOS
    MOVE DR-RET-LOG-N      TO RT-R-RET(WS-ROLL-IDX, WS-NEWPOS)
    MOVE DR-NTL-FIN-VOL-N  TO RT-R-VOL(WS-ROLL-IDX, WS-NEWPOS)
    MOVE DR-RGLR-TXS-QTY-N TO RT-R-NEG(WS-ROLL-IDX, WS-NEWPOS)

    *> Atualiza somatórios
    ADD DR-RET-LOG-N TO RT-SUM-R(WS-ROLL-IDX)
    COMPUTE RT-SUM-R2(WS-ROLL-IDX) =
        RT-SUM-R2(WS-ROLL-IDX)
        + (DR-RET-LOG-N * DR-RET-LOG-N)

    ADD DR-NTL-FIN-VOL-N    TO RT-SUM-VOL(WS-ROLL-IDX)
    ADD DR-RGLR-TXS-QTY-N   TO RT-SUM-NEG(WS-ROLL-IDX)

    MOVE WS-NEWPOS TO RT-LAST-IDX(WS-ROLL-IDX)

    *> Cálculo da janela (se pelo menos 2 pontos)
    MOVE RT-N(WS-ROLL-IDX)       TO WS-NOBS
    MOVE RT-SUM-R(WS-ROLL-IDX)   TO WS-SUM-R
    MOVE RT-SUM-R2(WS-ROLL-IDX)  TO WS-SUM-R2
    MOVE RT-SUM-VOL(WS-ROLL-IDX) TO WS-SUM-VOL
    MOVE RT-SUM-NEG(WS-ROLL-IDX) TO WS-SUM-NEG

    IF WS-NOBS < 2
        EXIT PARAGRAPH
    END-IF

    COMPUTE WS-MEAN-R = WS-SUM-R / WS-NOBS

    COMPUTE WS-VAR-R =
        (WS-SUM-R2 - (WS-SUM-R * WS-SUM-R / WS-NOBS))
        / (WS-NOBS - 1)

    IF WS-VAR-R < 0
        MOVE 0 TO WS-VAR-R
    END-IF

    IF WS-VAR-R > 0
        COMPUTE WS-VOL-DIARIA = FUNCTION SQRT(WS-VAR-R)
    ELSE
        MOVE 0 TO WS-VOL-DIARIA
    END-IF

    COMPUTE WS-VOL-ANUAL = WS-VOL-DIARIA * WS-SQRT252

    *> Retorno composto da janela: exp(sum r) - 1
    COMPUTE WS-RET-JANELA =
        FUNCTION EXP(WS-SUM-R) - 1

    IF WS-SUM-NEG > 0
        COMPUTE WS-IND-LIQ = WS-SUM-VOL / WS-SUM-NEG
    ELSE
        MOVE 0 TO WS-IND-LIQ
    END-IF

    *> Monta campos texto
    MOVE WS-NOBS        TO WS-NOBS-TXT
    MOVE WS-RET-JANELA  TO WS-RET-JAN-TXT
    MOVE WS-VOL-DIARIA  TO WS-VOL-DIA-TXT
    MOVE WS-VOL-ANUAL   TO WS-VOL-ANU-TXT
    MOVE WS-SUM-VOL     TO WS-VOLJAN-TXT
    MOVE WS-SUM-NEG     TO WS-NEGJAN-TXT
    MOVE WS-IND-LIQ     TO WS-INDLIQ-TXT

    MOVE SPACES TO ROLLING-REC
    STRING
        DR-TRADE-DATE-TXT   DELIMITED BY SIZE
        ","                 DELIMITED BY SIZE
        DR-TICKER-TXT       DELIMITED BY SIZE
        ","                 DELIMITED BY SIZE
        WS-NOBS-TXT         DELIMITED BY SIZE
        ","                 DELIMITED BY SIZE
        WS-RET-JAN-TXT      DELIMITED BY SIZE
        ","                 DELIMITED BY SIZE
        WS-VOL-DIA-TXT      DELIMITED BY SIZE
        ","                 DELIMITED BY SIZE
        WS-VOL-ANU-TXT      DELIMITED BY SIZE
        ","                 DELIMITED BY SIZE
        WS-VOLJAN-TXT       DELIMITED BY SIZE
        ","                 DELIMITED BY SIZE
        WS-NEGJAN-TXT       DELIMITED BY SIZE
        ","                 DELIMITED BY SIZE
        WS-INDLIQ-TXT       DELIMITED BY SIZE
        INTO ROLLING-REC
    END-STRING

    WRITE ROLLING-REC
    ADD 1 TO WS-TOTAL-ROLLING

    .

*> ==========================================================
*> FASE 3 - LER ROLLING E GERAR MÉTRICAS DE MERCADO (HHI)
*> ==========================================================
PHASE-3-GERA-MARKET.
    MOVE "N"      TO MK-EOF
    MOVE "N"      TO MK-HEADER-SKIPPED
    MOVE SPACES   TO MK-CURR-DATE
    MOVE 0        TO MK-SUM-VOL-N
    MOVE 0        TO MK-SUM-NEG
    MOVE 0        TO MK-NTICKERS
    MOVE 0        TO MK-SUM-VOL-F
    MOVE 0        TO MK-SUM-VOL-SQ-F

    OPEN INPUT  ROLLING-IN
         OUTPUT MARKET-OUT

    *> Header do CSV de métricas de mercado
    MOVE "TradeDate,NTickersWindow,VolumeTotalJanela,NumNegociosTotalJanela,HHI"
        TO MARKET-REC
    WRITE MARKET-REC

    PERFORM UNTIL MK-EOF = "Y"
        READ ROLLING-IN
            AT END
                MOVE "Y" TO MK-EOF
            NOT AT END
                PERFORM PH3-PROCESS-ROLL
        END-READ
    END-PERFORM

    *> Flush da última data
    IF MK-NTICKERS > 0
        PERFORM PH3-WRITE-MARKET-ROW
    END-IF

    CLOSE ROLLING-IN
          MARKET-OUT
    .

PH3-PROCESS-ROLL.
    *> Pula header de rolling_metrics_by_ticker
    IF MK-HEADER-SKIPPED = "N"
        MOVE "Y" TO MK-HEADER-SKIPPED
        EXIT PARAGRAPH
    END-IF

    MOVE SPACES TO MK-CSV-FIELDS

    UNSTRING ROLLING-IN-REC
      DELIMITED BY ALL ","
      INTO
        MK-F-TRADEDATE
        MK-F-TICKER
        MK-F-NOBS
        MK-F-RETJAN
        MK-F-VOLDIA
        MK-F-VOLANU
        MK-F-VOLJAN
        MK-F-NEGJAN
        MK-F-INDLIQ
    END-UNSTRING

    MOVE MK-F-TRADEDATE(1:10) TO MK-TRADE-DATE-TXT

    *> Se é a primeira linha de dados, inicializa data corrente
    IF MK-CURR-DATE = SPACES
        MOVE MK-TRADE-DATE-TXT TO MK-CURR-DATE
    END-IF

    *> Se mudou a data, fecha agregação anterior
    IF MK-TRADE-DATE-TXT NOT = MK-CURR-DATE
        IF MK-NTICKERS > 0
            PERFORM PH3-WRITE-MARKET-ROW
        END-IF

        MOVE MK-TRADE-DATE-TXT TO MK-CURR-DATE
        MOVE 0 TO MK-SUM-VOL-N
        MOVE 0 TO MK-SUM-NEG
        MOVE 0 TO MK-NTICKERS
        MOVE 0 TO MK-SUM-VOL-F
        MOVE 0 TO MK-SUM-VOL-SQ-F
    END-IF

    *> Converte volume da janela e nº de negócios
    IF MK-F-VOLJAN NOT = SPACES
        COMPUTE MK-VOLUME-JANELA-N =
            FUNCTION NUMVAL(MK-F-VOLJAN)
    ELSE
        MOVE 0 TO MK-VOLUME-JANELA-N
    END-IF

    IF MK-F-NEGJAN NOT = SPACES
        COMPUTE MK-NEG-JANELA-N =
            FUNCTION NUMVAL(MK-F-NEGJAN)
    ELSE
        MOVE 0 TO MK-NEG-JANELA-N
    END-IF

    *> Atualiza somatórios
    ADD MK-VOLUME-JANELA-N TO MK-SUM-VOL-N
    ADD MK-NEG-JANELA-N    TO MK-SUM-NEG
    ADD 1                  TO MK-NTICKERS

    COMPUTE MK-VOLUME-JANELA-F = MK-VOLUME-JANELA-N

    COMPUTE MK-SUM-VOL-F =
        MK-SUM-VOL-F + MK-VOLUME-JANELA-F

    COMPUTE MK-SUM-VOL-SQ-F =
        MK-SUM-VOL-SQ-F
        + (MK-VOLUME-JANELA-F * MK-VOLUME-JANELA-F)

    .

PH3-WRITE-MARKET-ROW.
    IF MK-SUM-VOL-F > 0
        COMPUTE MK-HHI-F =
            MK-SUM-VOL-SQ-F
            / (MK-SUM-VOL-F * MK-SUM-VOL-F)
    ELSE
        MOVE 0 TO MK-HHI-F
    END-IF

    COMPUTE MK-HHI-TXT = MK-HHI-F
    MOVE MK-SUM-VOL-N TO MK-VOLTOT-TXT
    MOVE MK-SUM-NEG   TO MK-NEGTOT-TXT
    MOVE MK-NTICKERS  TO MK-NTICKERS-TXT

    MOVE SPACES TO MARKET-REC
    STRING
        MK-CURR-DATE       DELIMITED BY SIZE
        ","                DELIMITED BY SIZE
        MK-NTICKERS-TXT    DELIMITED BY SIZE
        ","                DELIMITED BY SIZE
        MK-VOLTOT-TXT      DELIMITED BY SIZE
        ","                DELIMITED BY SIZE
        MK-NEGTOT-TXT      DELIMITED BY SIZE
        ","                DELIMITED BY SIZE
        MK-HHI-TXT         DELIMITED BY SIZE
        INTO MARKET-REC
    END-STRING

    WRITE MARKET-REC

    .

*> ==========================================================
*> MEDIÇÃO DE TEMPO
*> ==========================================================
PH-GET-START-TIME.
    MOVE FUNCTION CURRENT-DATE TO WS-START-TS-R

    *> WS-START-TS-R = YYYYMMDDHHMMSSCC+HHMM (21 chars)
    MOVE WS-START-TS-R(9:2)  TO WS-START-HH
    MOVE WS-START-TS-R(11:2) TO WS-START-MM
    MOVE WS-START-TS-R(13:2) TO WS-START-SS

    COMPUTE WS-START-TOTSEC =
        (WS-START-HH * 3600)
        + (WS-START-MM * 60)
        + WS-START-SS

    *> Monta ISO "YYYY-MM-DDThh:mm:ss"
    MOVE SPACES TO WS-START-ISO
    STRING
        WS-START-TS-R(1:4)  DELIMITED BY SIZE  *> YYYY
        "-"                 DELIMITED BY SIZE
        WS-START-TS-R(5:2)  DELIMITED BY SIZE  *> MM
        "-"                 DELIMITED BY SIZE
        WS-START-TS-R(7:2)  DELIMITED BY SIZE  *> DD
        "T"                 DELIMITED BY SIZE
        WS-START-TS-R(9:2)  DELIMITED BY SIZE  *> HH
        ":"                 DELIMITED BY SIZE
        WS-START-TS-R(11:2) DELIMITED BY SIZE  *> MM
        ":"                 DELIMITED BY SIZE
        WS-START-TS-R(13:2) DELIMITED BY SIZE  *> SS
        INTO WS-START-ISO
    END-STRING
    .

PH-GET-END-TIME.
    MOVE FUNCTION CURRENT-DATE TO WS-END-TS-R

    MOVE WS-END-TS-R(9:2)  TO WS-END-HH
    MOVE WS-END-TS-R(11:2) TO WS-END-MM
    MOVE WS-END-TS-R(13:2) TO WS-END-SS

    COMPUTE WS-END-TOTSEC =
        (WS-END-HH * 3600)
        + (WS-END-MM * 60)
        + WS-END-SS

    *> Elapsed em segundos (assumindo que não vira o dia; se virar, ajusta)
    COMPUTE WS-ELAPSED-SEC = WS-END-TOTSEC - WS-START-TOTSEC
    IF WS-ELAPSED-SEC < 0
        ADD 86400 TO WS-ELAPSED-SEC
    END-IF

    *> Monta ISO do fim
    MOVE SPACES TO WS-END-ISO
    STRING
        WS-END-TS-R(1:4)  DELIMITED BY SIZE
        "-"               DELIMITED BY SIZE
        WS-END-TS-R(5:2)  DELIMITED BY SIZE
        "-"               DELIMITED BY SIZE
        WS-END-TS-R(7:2)  DELIMITED BY SIZE
        "T"               DELIMITED BY SIZE
        WS-END-TS-R(9:2)  DELIMITED BY SIZE
        ":"               DELIMITED BY SIZE
        WS-END-TS-R(11:2) DELIMITED BY SIZE
        ":"               DELIMITED BY SIZE
        WS-END-TS-R(13:2) DELIMITED BY SIZE
        INTO WS-END-ISO
    END-STRING

    MOVE WS-ELAPSED-SEC TO WS-ELAPSED-TXT
    .

PH-WRITE-STATS.
    OPEN OUTPUT STATS-OUT

    *> Header do CSV de stats
    MOVE "engine,started_at,finished_at,elapsed_seconds"
        TO STATS-REC
    WRITE STATS-REC

    *> Linha de stats do COBOL
    MOVE SPACES TO STATS-REC
    STRING
        "cobol"           DELIMITED BY SIZE
        ","               DELIMITED BY SIZE
        WS-START-ISO      DELIMITED BY SIZE
        ","               DELIMITED BY SIZE
        WS-END-ISO        DELIMITED BY SIZE
        ","               DELIMITED BY SIZE
        WS-ELAPSED-TXT    DELIMITED BY SIZE
        INTO STATS-REC
    END-STRING

    WRITE STATS-REC

    CLOSE STATS-OUT
    .
