# motor_b3_dt_ultra.R
suppressPackageStartupMessages({
  library(data.table)
})

# Caminhos
DATA_DIR  <- "data"
OUT_DIR   <- "processados"
INPUT_CSV <- file.path(DATA_DIR, "pricereport_acoes.csv")
DAILY_RET_CSV      <- file.path(OUT_DIR, "daily_returns_dt.csv")
ROLLING_TICKER_CSV <- file.path(OUT_DIR, "rolling_metrics_by_ticker_dt.csv")
ROLLING_MARKET_CSV <- file.path(OUT_DIR, "rolling_market_metrics_dt.csv")
STATS_CSV          <- file.path(OUT_DIR, "motor_b3_dt_stats.csv")

# Função auxiliar para desvio-padrão rolante vetorizado
roll_sd_fast <- function(x, w, min_n = 2L) {
  valid <- !is.na(x)
  n     <- frollsum(valid, w)
  sx    <- frollsum(fifelse(valid, x, 0), w)
  sx2   <- frollsum(fifelse(valid, x*x, 0), w)
  var_bessel <- (sx2 - (sx * sx) / pmax(n, 1)) / pmax(n - 1, 1)
  sqrt(fifelse(n >= min_n, pmax(var_bessel, 0), NA_real_))
}

main <- function(window_size = 7L, min_obs = 2L) {
  start_ts <- Sys.time(); t0 <- proc.time()[["elapsed"]]

  # Fase 1: retornos
  cat("[R-dt] Fase 1 - Retornos...\n")
  df <- fread(INPUT_CSV,
              colClasses = list(character = "TradeDate",
                                character = "TickerSymbol",
                                numeric   = c("NtlFinVol","RglrTxsQty","LastPric")))
  setnames(df, "LastPric", "LastPrice")
  df[, TradeDate := as.IDate(TradeDate)]
  df <- df[LastPrice > 0 & NtlFinVol > 0]
  setorder(df, TickerSymbol, TradeDate)
  df[, ReturnLog := c(NA_real_, diff(log(LastPrice))), by = TickerSymbol]
  df <- df[!is.na(ReturnLog)]
  fwrite(df, DAILY_RET_CSV)

  # Fase 2: métricas rolantes
  cat("[R-dt] Fase 2 - Rolantes...\n")
  df[, `:=`(
    NObsJanela        = frollsum(!is.na(ReturnLog), window_size),
    sum_r             = frollsum(fifelse(is.na(ReturnLog), 0, ReturnLog), window_size),
    VolDiariaJanela   = roll_sd_fast(ReturnLog, window_size, min_n = min_obs),
    VolumeJanela      = frollsum(NtlFinVol, window_size),
    NumNegociosJanela = frollsum(RglrTxsQty, window_size)
  ), by = TickerSymbol]

  df[, `:=`(
    RetJanela       = exp(sum_r) - 1,
    VolAnualJanela  = VolDiariaJanela * sqrt(252),
    IndiceLiquidez  = fifelse(NumNegociosJanela > 0, VolumeJanela / NumNegociosJanela, NA_real_)
  )]

  df_out <- df[NObsJanela >= min_obs]
  fwrite(df_out, ROLLING_TICKER_CSV)

  # Fase 3: HHI
  cat("[R-dt] Fase 3 - HHI...\n")
  market <- df_out[, .(
    NTickersWindow         = .N,
    VolumeTotalJanela      = sum(VolumeJanela, na.rm = TRUE),
    NumNegociosTotalJanela = sum(NumNegociosJanela, na.rm = TRUE),
    HHI = {
      vt <- sum(VolumeJanela, na.rm = TRUE)
      if (vt > 0) sum((VolumeJanela / vt)^2, na.rm = TRUE) else NA_real_
    }
  ), by = TradeDate]
  fwrite(market, ROLLING_MARKET_CSV)

  # Tempo
  elapsed <- proc.time()[["elapsed"]] - t0; end_ts <- Sys.time()
  cat("--------------------------------------------\nFIM MOTOR R-DT ULTRA\n")
  cat(sprintf("Tempo total (s): %.6f\n", elapsed))
  cat("--------------------------------------------\n")

  # Stats
  header_needed <- !file.exists(STATS_CSV)
  con <- file(STATS_CSV, open = "a", encoding = "UTF-8"); on.exit(close(con), add = TRUE)
  if (header_needed) writeLines("engine,started_at,finished_at,elapsed_seconds", con)
  line <- sprintf("r_dt_ultra,%s,%s,%.6f",
                  format(start_ts, "%Y-%m-%dT%H:%M:%S"),
                  format(end_ts,   "%Y-%m-%dT%H:%M:%S"),
                  elapsed)
  writeLines(line, con)
}

if (sys.nframe() == 0L) main()