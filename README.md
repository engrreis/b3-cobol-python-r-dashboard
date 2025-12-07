Dashboard B3 — Benchmark entre COBOL, Python e R

Comparação de desempenho entre motores computacionais aplicados ao processamento de dados da B3, com cálculo de log-retornos, volatilidade, liquidez e índice de concentração (HHI).
Resultados visualizados em um dashboard interativo publicado via GitHub Pages.

📌 Objetivo do Projeto

Este projeto compara três abordagens distintas para processar dados de mercado da B3:

🟦 COBOL — linguagem legada amplamente usada no sistema financeiro

🟨 Python — motor moderno otimizado com pandas

🟩 R — motor estatístico com foco em análise quantitativa

O mesmo conjunto de dados e regras de cálculo é executado nos três motores para avaliar:

Tempo total de execução

Eficiência na manipulação de séries financeiras

Consistência dos resultados

Capacidade de cálculo de indicadores de mercado

📊 Indicadores Calculados

O pipeline extrai e processa os dados do PriceReport da B3 (XML/CSV) para gerar:

✔ Log-retornos diários por ticker
✔ Volatilidade realizada (janela móvel)
✔ Liquidez por volume e número de negócios
✔ HHI — Herfindahl-Hirschman Index (concentração de mercado)
✔ Rankings por retorno, volatilidade e liquidez
✔ Gráficos estáticos (Python) e dinâmicos (Chart.js)
🚀 Resumo dos Resultados
Engine	Tempo Total
COBOL	~26 s
R	~8 s
Python	~0.85 s

Python foi o motor mais eficiente, principalmente devido ao uso otimizado do pandas e operações vetorizadas.

📂 Estrutura do Repositório
/
├── cobol/
│   ├── motor_b3_cobol.cob
│   └── motor_b3_cobol.exe
│
├── python/
│   ├── motor_b3_python.py
│   ├── generate_dashboard_plots.py
│   └── xml_to_csv_*.py
│
├── R/
│   └── motor_b3_r.R
│
├── data/
│   ├── pricereport_acoes.csv
│   ├── pricereport_opcoes.csv
│   ├── XMLs da B3
│
├── processados/
│   ├── daily_returns_*.csv
│   ├── rolling_metrics_by_ticker_*.csv
│   ├── rolling_market_metrics_*.csv
│   └── engine_stats.json
│
├── dashboard/   ← (publicado no GitHub Pages)
│   ├── index.html
│   ├── css/
│   ├── js/
│   ├── img/
│   └── data/
│
└── README.md

🔧 Tecnologias Utilizadas
COBOL

Processamento sequencial de arquivos CSV

Cálculo de log-retornos

Implementação manual de janela móvel

Registro de tempo via CURRENT-DATE

Python (pandas + matplotlib)

Leitura e agregação de dados

Cálculo vetorizado de retornos e métricas

Geração de gráficos estáticos (PNG)

Exportação de CSVs finais

Arquitetura mais rápida do projeto

R

Manipulação com dplyr

Cálculo de métricas financeiras

Exportação dos mesmos arquivos processados

Dashboard

HTML/CSS

Chart.js para gráficos dinâmicos

Imagens PNG geradas em Python

Totalmente estático, ideal para GitHub Pages

▶ Como Executar
Python
py python/motor_b3_python.py
py python/generate_dashboard_plots.py

COBOL
cobc -x -free -o cobol/motor_b3_cobol.exe cobol/motor_b3_cobol.cob
./cobol/motor_b3_cobol.exe

R
Rscript R/motor_b3_r.R


Todos geram arquivos dentro de /processados.

🧠 Pontos de Estudo

Este projeto serve como referência para:

Modernização de sistemas legados

Comparação entre motores de processamento

Análise quantitativa de dados do mercado financeiro

Construção de pipelines reprodutíveis

Publicação de dashboards em ambientes estáticos

📝 Licença

MIT — fique à vontade para usar, aprender e adaptar.

Sugestões e melhorias são bem-vindas!