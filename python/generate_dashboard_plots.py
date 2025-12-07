import json
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd


def main():
    # Base: este script fica em python/, então subimos um nível
    base_dir = Path(__file__).resolve().parents[1]
    proc_dir = base_dir / "processados"
    dash_dir = base_dir / "dashboard"
    img_dir = dash_dir / "img"
    data_out_dir = dash_dir / "data"

    img_dir.mkdir(parents=True, exist_ok=True)
    data_out_dir.mkdir(parents=True, exist_ok=True)

    # ---------------------------------------------------------
    # 1) Engine stats (COBOL x R x Python)
    # ---------------------------------------------------------
    def read_last_elapsed(path: Path) -> float:
        if not path.exists():
            return None
        df = pd.read_csv(path)
        if "elapsed_seconds" not in df.columns or df.empty:
            return None
        return float(df["elapsed_seconds"].iloc[-1])

    engines = []

    cobol_stats = proc_dir / "motor_b3_cobol_stats.csv"
    py_stats = proc_dir / "motor_b3_python_stats.csv"
    r_stats = proc_dir / "motor_b3_r_stats.csv"

    engines.append(
        {
            "engine": "cobol",
            "elapsed_seconds": read_last_elapsed(cobol_stats),
        }
    )
    engines.append(
        {
            "engine": "r",
            "elapsed_seconds": read_last_elapsed(r_stats),
        }
    )
    engines.append(
        {
            "engine": "python",
            "elapsed_seconds": read_last_elapsed(py_stats),
        }
    )

    # salva engine_stats.json
    engine_stats_path = data_out_dir / "engine_stats.json"
    with engine_stats_path.open("w", encoding="utf-8") as f:
        json.dump(engines, f, ensure_ascii=False, indent=2)

    # também gera um bar chart simples (opcional, caso queira usar como PNG)
    valid_engines = [e for e in engines if e["elapsed_seconds"] is not None]
    if valid_engines:
        names = [e["engine"].upper() for e in valid_engines]
        times = [e["elapsed_seconds"] for e in valid_engines]

        plt.figure()
        plt.bar(names, times)
        plt.xlabel("Engine")
        plt.ylabel("Tempo (s)")
        plt.title("Tempo de execução por engine")
        plt.tight_layout()
        plt.savefig(img_dir / "engine_times.png", dpi=100)
        plt.close()

    # ---------------------------------------------------------
    # 2) Rolling Market Metrics (HHI, volume, negócios)
    # ---------------------------------------------------------
    rolling_market_src = proc_dir / "rolling_market_metrics_python.csv"
    if rolling_market_src.exists():
        market_df = pd.read_csv(rolling_market_src)

        if "TradeDate" in market_df.columns:
            # converte para datetime e ordena
            market_df["TradeDate"] = pd.to_datetime(market_df["TradeDate"])
            market_df = market_df.sort_values("TradeDate")

            # --- HACK SIMPLES: remove a última data da série ---
            # (útil quando sabemos que só o último dia está inconsistente)
            max_date = market_df["TradeDate"].max()
            print("Última data original em rolling_market:", max_date)

            # se quiser algo bem genérico, pode simplesmente tirar a última linha:
            # market_df = market_df.iloc[:-1, :]

            # aqui vou fazer: manter apenas datas <= 2025-12-05
            limite = pd.Timestamp("2025-12-05")
            market_df = market_df[market_df["TradeDate"] <= limite]

            # volta a data para string ISO
            market_df["TradeDate"] = market_df["TradeDate"].dt.strftime("%Y-%m-%d")

        market_df.to_csv(
            data_out_dir / "rolling_market_metrics_python.csv", index=False
        )


    # ---------------------------------------------------------
    # 3) Daily Returns (retorno médio diário)
    # ---------------------------------------------------------
    daily_ret_src = proc_dir / "daily_returns_python.csv"
    if daily_ret_src.exists():
        daily_df = pd.read_csv(daily_ret_src)
        # TradeDate, TickerSymbol, ReturnLog, LastPrice, NtlFinVol, RglrTxsQty
        if "TradeDate" in daily_df.columns and "ReturnLog" in daily_df.columns:
            daily_df["TradeDate"] = pd.to_datetime(daily_df["TradeDate"])
            agg = (
                daily_df.groupby("TradeDate", as_index=False)
                .agg(
                    ReturnMean=("ReturnLog", "mean"),
                    ReturnMedian=("ReturnLog", "median"),
                )
                .sort_values("TradeDate")
            )
            agg["TradeDate"] = agg["TradeDate"].dt.strftime("%Y-%m-%d")
            agg.to_csv(data_out_dir / "daily_returns_market_python.csv", index=False)

    # ---------------------------------------------------------
    # 4) Rolling by Ticker (scatter + rankings)
    # ---------------------------------------------------------
    rolling_ticker_src = proc_dir / "rolling_metrics_by_ticker_python.csv"
    if rolling_ticker_src.exists():
        df = pd.read_csv(rolling_ticker_src)

        # Espera colunas:
        # TradeDate, TickerSymbol, NObsJanela, RetJanela, VolDiariaJanela,
        # VolAnualJanela, VolumeJanela, NumNegociosJanela, IndiceLiquidez
        needed_cols = [
            "TradeDate",
            "TickerSymbol",
            "RetJanela",
            "VolAnualJanela",
            "VolumeJanela",
            "NumNegociosJanela",
            "IndiceLiquidez",
        ]
        if all(c in df.columns for c in needed_cols):
            # pega a última data disponível
            last_date = df["TradeDate"].max()
            sub = df[df["TradeDate"] == last_date].copy()

            # filtra lixo (volumes nulos, NaN)
            sub = sub[
                (sub["VolumeJanela"].notna())
                & (sub["VolumeJanela"] > 0)
                & sub["RetJanela"].notna()
                & sub["VolAnualJanela"].notna()
            ]

            # ---------------- Scatter Retorno x Vol ----------------
            if not sub.empty:
                plt.figure()
                plt.scatter(sub["VolAnualJanela"], sub["RetJanela"])
                plt.xlabel("Volatilidade anualizada (janela)")
                plt.ylabel("Retorno acumulado (janela)")
                plt.title(f"Retorno x Volatilidade - {last_date}")
                plt.tight_layout()
                plt.savefig(img_dir / "scatter_ret_vol_python.png", dpi=100)
                plt.close()

                # ---------------- Ranking por liquidez --------------
                top_liq = (
                    sub.dropna(subset=["IndiceLiquidez"])
                    .sort_values("IndiceLiquidez", ascending=False)
                    .head(20)
                )
                if not top_liq.empty:
                    plt.figure(figsize=(8, 6))
                    plt.barh(
                        top_liq["TickerSymbol"],
                        top_liq["IndiceLiquidez"],
                    )
                    plt.xlabel("Índice de liquidez")
                    plt.title(
                        f"Top 20 por liquidez (janela) - {last_date}"
                    )
                    plt.gca().invert_yaxis()
                    plt.tight_layout()
                    plt.savefig(
                        img_dir / "ranking_liquidez_python.png", dpi=100
                    )
                    plt.close()

                # ---------------- Ranking por volatilidade ----------
                top_vol = (
                    sub.dropna(subset=["VolAnualJanela"])
                    .sort_values("VolAnualJanela", ascending=False)
                    .head(20)
                )
                if not top_vol.empty:
                    plt.figure(figsize=(8, 6))
                    plt.barh(
                        top_vol["TickerSymbol"],
                        top_vol["VolAnualJanela"],
                    )
                    plt.xlabel("Volatilidade anualizada (janela)")
                    plt.title(
                        f"Top 20 por volatilidade (janela) - {last_date}"
                    )
                    plt.gca().invert_yaxis()
                    plt.tight_layout()
                    plt.savefig(
                        img_dir / "ranking_vol_python.png", dpi=100
                    )
                    plt.close()

                # ---------------- Ranking por retorno ---------------
                top_ret = (
                    sub.dropna(subset=["RetJanela"])
                    .sort_values("RetJanela", ascending=False)
                    .head(20)
                )
                if not top_ret.empty:
                    plt.figure(figsize=(8, 6))
                    plt.barh(
                        top_ret["TickerSymbol"],
                        top_ret["RetJanela"],
                    )
                    plt.xlabel("Retorno acumulado (janela)")
                    plt.title(
                        f"Top 20 por retorno (janela) - {last_date}"
                    )
                    plt.gca().invert_yaxis()
                    plt.tight_layout()
                    plt.savefig(
                        img_dir / "ranking_retorno_python.png", dpi=100
                    )
                    plt.close()


if __name__ == "__main__":
    main()
