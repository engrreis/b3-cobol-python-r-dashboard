// js/dashboard.js

document.addEventListener("DOMContentLoaded", () => {
  initDashboard();
});

async function initDashboard() {
  try {
    // Carrega tempos (independente)
    await loadEngineTimes();

    // Carrega retornos diários e obtém última data válida
    const { dates: retDates, returns, lastDate } = await loadDailyReturns();

    drawReturnsChart(retDates, returns);

    // Carrega métricas de mercado, filtrando até a última data dos retornos
    await loadMarketMetrics(lastDate);
  } catch (err) {
    console.error("Erro na inicialização do dashboard:", err);
  }
}

/* ============================================================
 * 1. TEMPOS DOS MOTORES (engine_stats.json)
 *    Formato esperado: [{ "engine": "cobol", "elapsed_seconds": 26 }, ...]
 * ========================================================== */
async function loadEngineTimes() {
  const res = await fetch("data/engine_stats.json");
  const data = await res.json();

  const labels = data.map(d => d.engine.toUpperCase());
  const valores = data.map(d => d.elapsed_seconds);

  const ctx = document.getElementById("chartEngineTimes");
  if (!ctx) return;

  new Chart(ctx, {
    type: "bar",
    data: {
      labels,
      datasets: [
        {
          label: "Tempo total (s)",
          data: valores,
          borderWidth: 1
        }
      ]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      scales: {
        y: {
          beginAtZero: true,
          title: {
            display: true,
            text: "Segundos"
          }
        }
      },
      plugins: {
        legend: {
          display: true
        },
        tooltip: {
          callbacks: {
            label: ctx => `${ctx.parsed.y.toFixed(3)} s`
          }
        }
      }
    }
  });
}

/* ============================================================
 * 2. RETORNOS DIÁRIOS (daily_returns_market_python.csv)
 *    Formato esperado: TradeDate,MeanReturn
 * ========================================================== */
async function loadDailyReturns() {
  const res = await fetch("data/daily_returns_market_python.csv");
  const text = await res.text();

  const linhas = text.trim().split("\n");
  if (linhas.length <= 1) {
    return { dates: [], returns: [], lastDate: null };
  }

  const datas = [];
  const retornos = [];

  for (let i = 1; i < linhas.length; i++) {
    const cols = linhas[i].split(",");
    if (cols.length < 2) continue;

    const dt = cols[0].trim();
    const r = parseFloat(cols[1]);

    if (!dt || isNaN(r)) continue;

    datas.push(dt);
    retornos.push(r);
  }

  // Última data válida (ISO yyyy-mm-dd -> ordenação lexicográfica funciona)
  const lastDate = datas.length ? datas[datas.length - 1] : null;

  return { dates: datas, returns: retornos, lastDate };
}

function drawReturnsChart(dates, returns) {
  const ctx = document.getElementById("chartReturns");
  if (!ctx || dates.length === 0) return;

  new Chart(ctx, {
    type: "line",
    data: {
      labels: dates,
      datasets: [
        {
          label: "Retorno médio diário (log)",
          data: returns,
          borderWidth: 2,
          tension: 0.25,
          pointRadius: 0
        }
      ]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      scales: {
        y: {
          title: {
            display: true,
            text: "Retorno (log)"
          },
          ticks: {
            callback: value => value.toFixed(4)
          }
        }
      },
      plugins: {
        legend: { display: true },
        tooltip: {
          callbacks: {
            label: ctx => `r̄: ${ctx.parsed.y.toFixed(6)}`
          }
        }
      }
    }
  });
}

/* ============================================================
 * 3. MÉTRICAS DE MERCADO (rolling_market_metrics_python.csv)
 *    Formato esperado:
 *    TradeDate,NTickersWindow,VolumeTotalJanela,NumNegociosTotalJanela,HHI
 * ========================================================== */
async function loadMarketMetrics(lastValidDate) {
  const res = await fetch("data/rolling_market_metrics_python.csv");
  const text = await res.text();

  const linhas = text.trim().split("\n");
  if (linhas.length <= 1) return;

  const datas = [];
  const hhi = [];
  const volumeMilhoes = [];
  const negocios = [];

  for (let i = 1; i < linhas.length; i++) {
    const cols = linhas[i].split(",");
    if (cols.length < 5) continue;

    const dt = cols[0].trim();
    const vol = parseFloat(cols[2]);
    const neg = parseFloat(cols[3]);
    const h = parseFloat(cols[4]);

    // Se tivermos lastValidDate (da série de retornos), ignora datas posteriores
    if (lastValidDate && dt > lastValidDate) {
      continue;
    }

    datas.push(dt);
    volumeMilhoes.push(isNaN(vol) ? null : vol / 1_000_000);
    negocios.push(isNaN(neg) ? null : neg);
    hhi.push(isNaN(h) ? null : h);
  }

  drawHHIChart(datas, hhi);
  drawVolumeChart(datas, volumeMilhoes);
  drawNegociosChart(datas, negocios);
}

function drawHHIChart(labels, hhi) {
  const ctx = document.getElementById("chartHHI");
  if (!ctx || labels.length === 0) return;

  new Chart(ctx, {
    type: "line",
    data: {
      labels,
      datasets: [
        {
          label: "HHI de liquidez",
          data: hhi,
          borderWidth: 2,
          tension: 0.25,
          pointRadius: 2
        }
      ]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      scales: {
        y: {
          min: 0,
          max: 1,
          title: {
            display: true,
            text: "HHI (0 a 1)"
          },
          ticks: {
            callback: value => value.toFixed(2)
          }
        }
      },
      plugins: {
        legend: { display: true },
        tooltip: {
          callbacks: {
            label: ctx => `HHI: ${ctx.parsed.y.toFixed(3)}`
          }
        }
      }
    }
  });
}
function drawVolumeChart(labels, volumeMilhoes) {
  const ctx = document.getElementById("chartVolume");
  if (!ctx || labels.length === 0) return;

  const valid = volumeMilhoes.filter(v => v != null && v > 0);
  if (!valid.length) return;

  const minVal = Math.min(...valid);
  const maxVal = Math.max(...valid);

  new Chart(ctx, {
    type: "line",
    data: {
      labels,
      datasets: [
        {
          label: "Volume financeiro (R$ milhões)",
          data: volumeMilhoes,
          borderWidth: 2,
          tension: 0.25,
          pointRadius: 2
        }
      ]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      scales: {
        y: {
          // em vez de começar em 0, aproximamos da menor observação
          suggestedMin: minVal * 0.9,
          suggestedMax: maxVal * 1.05,
          title: {
            display: true,
            text: "R$ milhões"
          },
          ticks: {
            callback: value =>
              value.toLocaleString("pt-BR", { maximumFractionDigits: 0 }) + " M"
          }
        }
      },
      plugins: {
        legend: { display: true },
        tooltip: {
          callbacks: {
            label: ctx =>
              `Volume: ${ctx.parsed.y.toLocaleString("pt-BR", {
                maximumFractionDigits: 2
              })} M`
          }
        }
      }
    }
  });
}

function drawNegociosChart(labels, negocios) {
  const ctx = document.getElementById("chartNegocios");
  if (!ctx || labels.length === 0) return;

  const valid = negocios.filter(v => v != null && v > 0);
  if (!valid.length) return;

  const minVal = Math.min(...valid);
  const maxVal = Math.max(...valid);

  new Chart(ctx, {
    type: "line",
    data: {
      labels,
      datasets: [
        {
          label: "Negócios regulares",
          data: negocios,
          borderWidth: 2,
          tension: 0.25,
          pointRadius: 2
        }
      ]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      scales: {
        y: {
          suggestedMin: minVal * 0.9,
          suggestedMax: maxVal * 1.05,
          title: {
            display: true,
            text: "Número de negócios"
          },
          ticks: {
            callback: value =>
              value.toLocaleString("pt-BR", { maximumFractionDigits: 0 })
          }
        }
      },
      plugins: {
        legend: { display: true },
        tooltip: {
          callbacks: {
            label: ctx =>
              `Negócios: ${ctx.parsed.y.toLocaleString("pt-BR")}`
          }
        }
      }
    }
  });
}
