// ---------- Engine (Chart.js) ----------

async function initEngineChart() {
  try {
    const resp = await fetch("data/engine_stats.json");
    const engines = await resp.json();

    const labels = engines.map((e) => e.engine.toUpperCase());
    const data = engines.map((e) => e.elapsed_seconds);

    const ctx = document
      .getElementById("enginePerformanceChart")
      .getContext("2d");

    new Chart(ctx, {
      type: "bar",
      data: {
        labels,
        datasets: [
          {
            label: "Tempo (s)",
            data,
          },
        ],
      },
      options: {
        responsive: true,
        plugins: {
          legend: { display: false },
          tooltip: {
            callbacks: {
              label: (ctx) => `${ctx.raw.toFixed(3)} s`,
            },
          },
        },
        scales: {
          y: {
            beginAtZero: true,
          },
        },
      },
    });
  } catch (err) {
    console.error("Erro ao carregar engine_stats.json:", err);
  }
}

// ---------- Market (Dygraphs) ----------

function initMarketDygraphs() {
  // rolling_market_metrics_python.csv:
  // TradeDate,NTickersWindow,VolumeTotalJanela,NumNegociosTotalJanela,HHI

  const baseOptions = {
    legend: "always",
    showRoller: false,
    rollPeriod: 1,
    drawGrid: true,
    labelsSeparateLines: true,
    dateWindow: null, // auto
    ylabel: "",
    xlabel: "Data",
  };

  // HHI
  const hhiEl = document.getElementById("hhiDygraph");
  if (hhiEl) {
    new Dygraph(hhiEl, "data/rolling_market_metrics_python.csv", {
      ...baseOptions,
      ylabel: "HHI",
      labels: [
        "TradeDate",
        "NTickersWindow",
        "VolumeTotalJanela",
        "NumNegociosTotalJanela",
        "HHI",
      ],
      visibility: [false, false, false, true], // só HHI
    });
  }

  // Volume total
  const volEl = document.getElementById("volumeDygraph");
  if (volEl) {
    new Dygraph(volEl, "data/rolling_market_metrics_python.csv", {
      ...baseOptions,
      ylabel: "Volume total da janela",
      labels: [
        "TradeDate",
        "NTickersWindow",
        "VolumeTotalJanela",
        "NumNegociosTotalJanela",
        "HHI",
      ],
      visibility: [false, true, false, false], // só VolumeTotalJanela
    });
  }

  // Número de negócios
  const tradesEl = document.getElementById("tradesDygraph");
  if (tradesEl) {
    new Dygraph(tradesEl, "data/rolling_market_metrics_python.csv", {
      ...baseOptions,
      ylabel: "Nº de negócios na janela",
      labels: [
        "TradeDate",
        "NTickersWindow",
        "VolumeTotalJanela",
        "NumNegociosTotalJanela",
        "HHI",
      ],
      visibility: [false, false, true, false], // só NumNegociosTotalJanela
    });
  }

  // Retorno médio diário (daily_returns_market_python.csv):
  // TradeDate,ReturnMean,ReturnMedian
  const dailyEl = document.getElementById("dailyReturnDygraph");
  if (dailyEl) {
    new Dygraph(dailyEl, "data/daily_returns_market_python.csv", {
      ...baseOptions,
      ylabel: "Retorno médio (log)",
      labels: ["TradeDate", "ReturnMean", "ReturnMedian"],
      visibility: [true, false], // só média
    });
  }
}

// ---------- Bootstrap ----------

document.addEventListener("DOMContentLoaded", () => {
  initEngineChart();
  initMarketDygraphs();
});
