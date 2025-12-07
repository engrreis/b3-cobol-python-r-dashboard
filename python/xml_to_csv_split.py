from pathlib import Path
import xml.etree.ElementTree as ET
import csv
import re

BASE_DIR = Path(__file__).resolve().parents[1]
DATA_DIR = BASE_DIR / "data"

ACOES_CSV = DATA_DIR / "pricereport_acoes.csv"
OPCOES_CSV = DATA_DIR / "pricereport_opcoes.csv"

xml_files = sorted(DATA_DIR.glob("BVBG.086.01_*.xml"))
if not xml_files:
    xml_files = sorted(DATA_DIR.glob("*.xml"))
if not xml_files:
    raise SystemExit(f"Nenhum XML encontrado em {DATA_DIR}")

def local_name(tag: str) -> str:
    if "}" in tag:
        return tag.split("}", 1)[1]
    return tag

def find_child(elem, name):
    for ch in elem:
        if local_name(ch.tag) == name:
            return ch
    return None

def find_text_path(elem, path_list):
    cur = elem
    for name in path_list:
        if cur is None:
            return ""
        cur = find_child(cur, name)
    if cur is None or cur.text is None:
        return ""
    return cur.text.strip()

def classify_instrument(ticker: str, mkt_stream_id: str, has_days_to_sttlm: bool):
    if not ticker:
        return "OUTRO"

    t = ticker.strip().upper()
    m = (mkt_stream_id or "").strip().upper()

    if m == "T" or has_days_to_sttlm:
        return "TERMO"

    if t.endswith("F"):
        return "FRACIONARIO"

    if re.fullmatch(r"[A-Z]{4}[A-Z][0-9]{2,3}", t):
        return "OPCAO"

    if re.fullmatch(r"[A-Z]{4}[0-9]{1,2}", t):
        return "ACAO"

    return "OUTRO"

def process_file(xml_path, writer_acoes, writer_opcoes):
    print(f"Processando {xml_path.name} ...")
    tree = ET.parse(xml_path)
    root = tree.getroot()

    for pric in root.iter():
        if local_name(pric.tag) != "PricRpt":
            continue

        trade_date = find_text_path(pric, ["TradDt", "Dt"])
        ticker     = find_text_path(pric, ["SctyId", "TckrSymb"])

        days_to_sttlm = find_text_path(pric, ["TradDtls", "DaysToSttlm"])
        has_days = days_to_sttlm != ""

        fin = find_child(pric, "FinInstrmAttrbts")
        if fin is None:
            continue

        mkt_stream_id = find_text_path(fin, ["MktDataStrmId"])

        ntlfinvol  = find_text_path(fin, ["NtlFinVol"])
        intlfinvol = find_text_path(fin, ["IntlFinVol"])
        qty        = find_text_path(fin, ["FinInstrmQty"])
        rglr_txs   = find_text_path(fin, ["RglrTxsQty"])

        frst     = find_text_path(fin, ["FrstPric"])
        minp     = find_text_path(fin, ["MinPric"])
        maxp     = find_text_path(fin, ["MaxPric"])
        trad_avg = find_text_path(fin, ["TradAvrgPric"])
        last     = find_text_path(fin, ["LastPric"])

        instrtype = classify_instrument(ticker, mkt_stream_id, has_days)

        row = [
            trade_date,
            ticker,
            instrtype,
            mkt_stream_id,
            qty,
            ntlfinvol,
            intlfinvol,
            rglr_txs,
            frst,
            minp,
            maxp,
            trad_avg,
            last,
        ]

        if instrtype == "ACAO":
            writer_acoes.writerow(row)
        elif instrtype == "OPCAO":
            writer_opcoes.writerow(row)

def main():
    DATA_DIR.mkdir(parents=True, exist_ok=True)

    header = [
        "TradeDate",
        "TickerSymbol",
        "InstrType",
        "MktDataStrmId",
        "FinInstrmQty",
        "NtlFinVol",
        "IntlFinVol",
        "RglrTxsQty",
        "FrstPric",
        "MinPric",
        "MaxPric",
        "TradAvrgPric",
        "LastPric",
    ]

    with ACOES_CSV.open("w", newline="", encoding="utf-8") as fa,\
         OPCOES_CSV.open("w", newline="", encoding="utf-8") as fo:

        writer_acoes = csv.writer(fa)
        writer_opcoes = csv.writer(fo)

        writer_acoes.writerow(header)
        writer_opcoes.writerow(header)

        for xml_path in xml_files:
            process_file(xml_path, writer_acoes, writer_opcoes)

    print("\nArquivos gerados:")
    print(" -", ACOES_CSV)
    print(" -", OPCOES_CSV)

if __name__ == "__main__":
    main()
