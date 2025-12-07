import xml.etree.ElementTree as ET
from pathlib import Path
import csv
import re

# Pasta base: .../_cobol/
BASE_DIR = Path(__file__).resolve().parents[1]
DATA_DIR = BASE_DIR / "data"

# Padrão dos arquivos XML do PriceReport
XML_PATTERN = "BVBG.086.01_*.xml"

# Arquivos de saída
ACOES_CSV = DATA_DIR / "pricereport_acoes.csv"
OPCOES_CSV = DATA_DIR / "pricereport_opcoes.csv"


def classify_instrument(ticker: str, mkt_stream_id: str | None, has_days_to_sttlm: bool) -> str:
    """
    Classifica o instrumento em:
      - ACAO
      - OPCAO
      - TERMO
      - FRACIONARIO
      - OUTRO
    Usando:
      - ticker (TckrSymb)
      - MktDataStrmId
      - presença de DaysToSttlm
    """

    if ticker is None:
        return "OUTRO"

    ticker = ticker.strip().upper()
    mkt_stream_id = (mkt_stream_id or "").strip().upper()

    # 1) TERMO: stream T ou tem DaysToSttlm
    if mkt_stream_id == "T" or has_days_to_sttlm:
        return "TERMO"

    # 2) Fracionário: convenção B3 - F no fim (ex: PETR4F)
    if ticker.endswith("F"):
        return "FRACIONARIO"

    # 3) OPÇÃO: padrão raiz (4 letras) + letra de série + 2 ou 3 dígitos
    #   Ex: PETRA155, VALEX20, ITUBC32
    if re.fullmatch(r"[A-Z]{4}[A-Z][0-9]{2,3}", ticker):
        return "OPCAO"

    # 4) AÇÃO / UNIT / BDR: 4 letras + 1 ou 2 dígitos (ex: PETR4, VALE3, B3SA3, IVVB11)
    if re.fullmatch(r"[A-Z]{4}[0-9]{1,2}", ticker):
        return "ACAO"

    # 5) Caso contrário, joga em OUTRO (ETF diferente, índices, etc.)
    return "OUTRO"


def parse_pricereport_file(xml_path: Path):
    """
    Lê um XML de PriceReport (BVBG.086.01 / bvmf.217.01)
    e retorna uma lista de dicionários prontos para CSV.
    """

    tree = ET.parse(xml_path)
    root = tree.getroot()

    # Descobre namespace padrão (ex: {urn:bvmf.217.01.xsd}Document)
    ns_uri = root.tag.split('}')[0].strip('{')
    ns = {"d": ns_uri}

    registros = []

    for pric in root.findall(".//d:PricRpt", ns):
        # Data
        trade_date = pric.findtext("d:TradDt/d:Dt", default="", namespaces=ns)

        # Ticker
        ticker = pric.findtext("d:SctyId/d:TckrSymb", default="", namespaces=ns)

        # DaysToSttlm (para detectar TERMO)
        days_to_sttlm_el = pric.find("d:TradDtls/d:DaysToSttlm", ns)
        has_days_to_sttlm = days_to_sttlm_el is not None

        # Quantidade negociada "bruta" do registro (nem sempre vamos usar)
        trad_qty = pric.findtext("d:TradDtls/d:TradQty", default="", namespaces=ns)

        # Atributos financeiros
        fin = pric.find("d:FinInstrmAttrbts", ns)
        if fin is None:
            continue

        mkt_stream_id = fin.findtext("d:MktDataStrmId", default="", namespaces=ns)

        ntl_fin_vol = fin.findtext("d:NtlFinVol", default="", namespaces=ns)
        intl_fin_vol = fin.findtext("d:IntlFinVol", default="", namespaces=ns)
        fin_instrm_qty = fin.findtext("d:FinInstrmQty", default="", namespaces=ns)

        frst_pric = fin.findtext("d:FrstPric", default="", namespaces=ns)
        min_pric = fin.findtext("d:MinPric", default="", namespaces=ns)
        max_pric = fin.findtext("d:MaxPric", default="", namespaces=ns)
        trad_avrg_pric = fin.findtext("d:TradAvrgPric", default="", namespaces=ns)
        last_pric = fin.findtext("d:LastPric", default="", namespaces=ns)

        rglr_txs_qty = fin.findtext("d:RglrTxsQty", default="", namespaces=ns)

        # Classificação do instrumento
        instr_type = classify_instrument(ticker, mkt_stream_id, has_days_to_sttlm)

        reg = {
            "TradeDate": trade_date,
            "TickerSymbol": ticker,
            "InstrType": instr_type,
            "MktDataStrmId": mkt_stream_id,
            "TradQty": trad_qty,
            "NtlFinVol": ntl_fin_vol,
            "IntlFinVol": intl_fin_vol,
            "FinInstrmQty": fin_instrm_qty,
            "RglrTxsQty": rglr_txs_qty,
            "FrstPric": frst_pric,
            "MinPric": min_pric,
            "MaxPric": max_pric,
            "TradAvrgPric": trad_avrg_pric,
            "LastPric": last_pric,
        }

        registros.append(reg)

    return registros


def main():
    xml_files = sorted(DATA_DIR.glob(XML_PATTERN))

    if not xml_files:
        print(f"Nenhum arquivo encontrado com padrão {XML_PATTERN} em {DATA_DIR}")
        return

    print(f"Encontrados {len(xml_files)} arquivos XML para processar.")

    # Acumuladores
    acoes_rows = []
    opcoes_rows = []

    for xml_path in xml_files:
        print(f"Processando: {xml_path.name}")
        registros = parse_pricereport_file(xml_path)

        for reg in registros:
            if reg["InstrType"] == "ACAO":
                acoes_rows.append(reg)
            elif reg["InstrType"] == "OPCAO":
                opcoes_rows.append(reg)
            # TERMO, FRACIONARIO, OUTRO são ignorados por enquanto

    # Cabeçalho comum para os dois CSVs
    fieldnames = [
        "TradeDate",
        "TickerSymbol",
        "InstrType",
        "MktDataStrmId",
        "TradQty",
        "NtlFinVol",
        "IntlFinVol",
        "FinInstrmQty",
        "RglrTxsQty",
        "FrstPric",
        "MinPric",
        "MaxPric",
        "TradAvrgPric",
        "LastPric",
    ]

    # Grava ações
    print(f"Gravando {len(acoes_rows)} linhas de ACOES em {ACOES_CSV}")
    with ACOES_CSV.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(acoes_rows)

    # Grava opções
    print(f"Gravando {len(opcoes_rows)} linhas de OPCOES em {OPCOES_CSV}")
    with OPCOES_CSV.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(opcoes_rows)

    print("Concluído.")


if __name__ == "__main__":
    main()
