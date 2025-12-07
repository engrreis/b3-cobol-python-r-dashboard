
import os
import time
from datetime import datetime

import numpy as np
import pandas as pd


# --------------------------------------------------------
# Configuração básica de caminhos
# --------------------------------------------------------

DATA_DIR = "data"
OUT_DIR = "processados"

INPUT_CSV = os.path.join(DATA_DIR, "pricereport_acoes.csv")

DAILY_RET_CSV = os.path.join(OUT_DIR, "daily_returns_python.csv")
ROLLING_TICKER_CSV = os.path.join(OUT_DIR, "rolling_metrics_by_ticker_python.csv")
ROLLING_MARKET_CSV = os.path.join(OUT_DIR, "rolling_market_metrics_python.csv")
STATS_CSV = os.path.join(OUT_DIR, "motor_b3_python_stats.csv")


def ensure_output_dir():
    os.makedirs(OUT_DIR, exist_ok=True)


# --------------------------------------------------------
# FASE 1 - Gera daily_returns_python.csv
# --------------------------------------------------------

def phase1_generate_daily_returns():
    """Lê pricereport_acoes.csv e gera daily_returns_python.csv
    com:
        TradeDate, TickerSymbol, ReturnLog, LastPrice, NtlFinVol, RglrTxsQty
    """
    print("[Python] Fase 1 - Lendo PriceReport e calculando retornos diários...")

    usecols = [
        "TradeDate",
        "TickerSymbol",
        "NtlFinVol",
        "RglrTxsQty",
        "LastPric",
    ]

    df = pd.read_csv(INPUT_CSV, usecols=usecols)

    # Normaliza nomes para bater com o layout de saída
    df = df.rename(
        columns={
            "LastPric": "LastPrice",
        }
    )

    # Converte tipos numéricos
    df["NtlFinVol"] = pd.to_numeric(df["NtlFinVol"], errors="coerce")
    df["RglrTxsQty"] = pd.to_numeric(df["RglrTxsQty"], errors="coerce")
    df["LastPrice"] = pd.to_numeric(df["LastPrice"], errors="coerce")

    # Filtra linhas com preço e volume válidos
    df = df.dropna(subset=["LastPrice", "NtlFinVol"])
    df = df[(df["LastPrice"] > 0) & (df["NtlFinVol"] > 0)]

    # Ordena para garantir a ordem por ativo / data
    df["TradeDate"] = pd.to_datetime(df["TradeDate"])
    df = df.sort_values(["TickerSymbol", "TradeDate"])

    # Calcula log-preço e log-retorno por ativo
    df["log_price"] = np.log(df["LastPrice"])
    df["ReturnLog"] = df.groupby("TickerSymbol")["log_price"].diff()

    # Descarta a primeira observação de cada ticker (ReturnLog = NaN)
    df = df.dropna(subset=["ReturnLog"])

    # Ajusta formato de data de volta para YYYY-MM-DD
    df["TradeDate"] = df["TradeDate"].dt.strftime("%Y-%m-%d")

    out_cols = [
        "TradeDate",
        "TickerSymbol",
        "ReturnLog",
        "LastPrice",
        "NtlFinVol",
        "RglrTxsQty",
    ]

    df[out_cols].to_csv(DAILY_RET_CSV, index=False)
    print(f"[Python] Fase 1 OK - {len(df):6d} linhas escritas em {DAILY_RET_CSV}")


# --------------------------------------------------------
# FASE 2 - Gera rolling_metrics_by_ticker_python.csv
# --------------------------------------------------------

def phase2_generate_rolling_by_ticker(window_size: int = 7):
    """Lê daily_returns_python.csv e gera métricas rolantes por ticker:

    TradeDate,TickerSymbol,NObsJanela,RetJanela,VolDiariaJanela,VolAnualJanela,
    VolumeJanela,NumNegociosJanela,IndiceLiquidez
    """
    print("[Python] Fase 2 - Calculando métricas rolantes por ticker...")

    df = pd.read_csv(DAILY_RET_CSV)

    # Converte tipos e ordena
    df["TradeDate"] = pd.to_datetime(df["TradeDate"])
    df["ReturnLog"] = pd.to_numeric(df["ReturnLog"], errors="coerce")
    df["LastPrice"] = pd.to_numeric(df["LastPrice"], errors="coerce")
    df["NtlFinVol"] = pd.to_numeric(df["NtlFinVol"], errors="coerce")
    df["RglrTxsQty"] = pd.to_numeric(df["RglrTxsQty"], errors="coerce")

    df = df.dropna(subset=["ReturnLog"])
    df = df.sort_values(["TickerSymbol", "TradeDate"])

    # Agrupa por ativo
    g = df.groupby("TickerSymbol", group_keys=False)

    # Número de observações na janela
    df["NObsJanela"] = g["ReturnLog"].rolling(window_size, min_periods=1).count().reset_index(level=0, drop=True)

    # Soma de retornos e variância / desvio padrão
    rolling_sum_r = g["ReturnLog"].rolling(window_size, min_periods=1).sum().reset_index(level=0, drop=True)

    # Volatilidade diária da janela (desvio padrão amostral, ddof=1)
    vol_diaria = g["ReturnLog"].rolling(window_size, min_periods=2).std(ddof=1).reset_index(level=0, drop=True)

    # Vol anualizada
    vol_anual = vol_diaria * np.sqrt(252.0)

    # Retorno composto na janela: exp(soma r) - 1
    ret_janela = np.exp(rolling_sum_r) - 1.0

    # Volume e número de negócios na janela
    vol_janela = g["NtlFinVol"].rolling(window_size, min_periods=1).sum().reset_index(level=0, drop=True)
    neg_janela = g["RglrTxsQty"].rolling(window_size, min_periods=1).sum().reset_index(level=0, drop=True)

    # Índice de liquidez
    indice_liq = np.where(neg_janela > 0, vol_janela / neg_janela, np.nan)

    df["RetJanela"] = ret_janela
    df["VolDiariaJanela"] = vol_diaria
    df["VolAnualJanela"] = vol_anual
    df["VolumeJanela"] = vol_janela
    df["NumNegociosJanela"] = neg_janela
    df["IndiceLiquidez"] = indice_liq

    # Mantém apenas casos com pelo menos 2 observações na janela, como no COBOL
    df_out = df[df["NObsJanela"] >= 2].copy()

    # Formata TradeDate novamente
    df_out["TradeDate"] = df_out["TradeDate"].dt.strftime("%Y-%m-%d")

    out_cols = [
        "TradeDate",
        "TickerSymbol",
        "NObsJanela",
        "RetJanela",
        "VolDiariaJanela",
        "VolAnualJanela",
        "VolumeJanela",
        "NumNegociosJanela",
        "IndiceLiquidez",
    ]

    df_out[out_cols].to_csv(ROLLING_TICKER_CSV, index=False)
    print(f"[Python] Fase 2 OK - {len(df_out):6d} linhas escritas em {ROLLING_TICKER_CSV}")


# --------------------------------------------------------
# FASE 3 - Gera rolling_market_metrics_python.csv (HHI)
# --------------------------------------------------------

def phase3_generate_market_metrics():
    """Lê rolling_metrics_by_ticker_python.csv e agrega por TradeDate:

    TradeDate,NTickersWindow,VolumeTotalJanela,NumNegociosTotalJanela,HHI
    """
    print("[Python] Fase 3 - Calculando métricas de mercado (HHI)...")

    df = pd.read_csv(ROLLING_TICKER_CSV)

    df["TradeDate"] = pd.to_datetime(df["TradeDate"])
    df["VolumeJanela"] = pd.to_numeric(df["VolumeJanela"], errors="coerce")
    df["NumNegociosJanela"] = pd.to_numeric(df["NumNegociosJanela"], errors="coerce")

    # Remove linhas sem volume
    df = df.dropna(subset=["VolumeJanela"])
    df = df[df["VolumeJanela"] > 0]

    g = df.groupby("TradeDate")

    vol_total = g["VolumeJanela"].sum()
    neg_total = g["NumNegociosJanela"].sum()
    ntickers = g.size()

    # HHI = sum_i (vol_i^2) / (sum_i vol_i)^2
    sum_vol_sq = g.apply(lambda x: (x["VolumeJanela"] ** 2).sum())
    hhi = sum_vol_sq / (vol_total ** 2)

    market = pd.DataFrame(
        {
            "TradeDate": vol_total.index,
            "NTickersWindow": ntickers.values,
            "VolumeTotalJanela": vol_total.values,
            "NumNegociosTotalJanela": neg_total.values,
            "HHI": hhi.values,
        }
    )

    market["TradeDate"] = market["TradeDate"].dt.strftime("%Y-%m-%d")

    market.to_csv(ROLLING_MARKET_CSV, index=False)
    print(f"[Python] Fase 3 OK - {len(market):3d} linhas escritas em {ROLLING_MARKET_CSV}")


# --------------------------------------------------------
# Medição de tempo de execução - motor_b3_python_stats.csv
# --------------------------------------------------------

def write_stats(start_ts: datetime, end_ts: datetime, elapsed_seconds: float):
    ensure_output_dir()

    header = not os.path.exists(STATS_CSV)

    with open(STATS_CSV, "a", encoding="utf-8") as f:
        if header:
            f.write("engine,started_at,finished_at,elapsed_seconds\n")
        line = (
            f"python,{start_ts.isoformat(timespec='seconds')},"
            f"{end_ts.isoformat(timespec='seconds')},"
            f"{elapsed_seconds:.6f}\n"
        )
        f.write(line)


def main():
    ensure_output_dir()

    start_ts = datetime.now()
    t0 = time.perf_counter()

    phase1_generate_daily_returns()
    phase2_generate_rolling_by_ticker(window_size=7)
    phase3_generate_market_metrics()

    t1 = time.perf_counter()
    end_ts = datetime.now()
    elapsed = t1 - t0

    print("--------------------------------------------")
    print("FIM MOTOR PYTHON")
    print(f"Tempo total (s).....................: {elapsed:.6f}")
    print("--------------------------------------------")

    write_stats(start_ts, end_ts, elapsed)


if __name__ == "__main__":
    main()
