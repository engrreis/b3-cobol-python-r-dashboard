from pathlib import Path
import xml.etree.ElementTree as ET
import csv

BASE_DIR = Path(__file__).resolve().parents[1]
DATA_DIR = BASE_DIR / "data"
OUTPUT = DATA_DIR / "pricereport_week.csv"

# pega todos os PriceReport da semana
xml_files = sorted(DATA_DIR.glob("BVBG.086.01_*.xml"))
if not xml_files:
    xml_files = sorted(DATA_DIR.glob("*.xml"))

if not xml_files:
    raise SystemExit(f"Nenhum XML encontrado em {DATA_DIR}")

def local_name(tag: str) -> str:
    """Remove o namespace e retorna só o nome da tag."""
    if "}" in tag:
        return tag.split("}", 1)[1]
    return tag

def find_child(elem, name):
    """Procura filho direto com nome local 'name'."""
    for ch in elem:
        if local_name(ch.tag) == name:
            return ch
    return None

def find_text_path(elem, path_list):
    """Navega por uma sequência de nomes locais e retorna o texto."""
    cur = elem
    for name in path_list:
        if cur is None:
            return ""
        cur = find_child(cur, name)
    if cur is None or cur.text is None:
        return ""
    return cur.text.strip()

def process_file(xml_path, writer):
    print(f"Processando {xml_path.name} ...")
    tree = ET.parse(xml_path)
    root = tree.getroot()

    # percorre TODOS os elementos e pega os que são PricRpt (ignorando namespace)
    for pric in root.iter():
        if local_name(pric.tag) != "PricRpt":
            continue

        # TradDt/Dt
        trade_date = find_text_path(pric, ["TradDt", "Dt"])

        # SctyId/TckrSymb
        ticker = find_text_path(pric, ["SctyId", "TckrSymb"])

        # FinInstrmAttrbts
        fin_attrs = find_child(pric, "FinInstrmAttrbts")
        if fin_attrs is None:
            continue

        ntlfinvol    = find_text_path(fin_attrs, ["NtlFinVol"])
        intlfinvol   = find_text_path(fin_attrs, ["IntlFinVol"])
        qty          = find_text_path(fin_attrs, ["FinInstrmQty"])
        frst         = find_text_path(fin_attrs, ["FrstPric"])
        minp         = find_text_path(fin_attrs, ["MinPric"])
        maxp         = find_text_path(fin_attrs, ["MaxPric"])
        trad_avg     = find_text_path(fin_attrs, ["TradAvrgPric"])
        last         = find_text_path(fin_attrs, ["LastPric"])

        writer.writerow([
            trade_date,
            ticker,
            ntlfinvol,
            intlfinvol,
            qty,
            frst,
            minp,
            maxp,
            trad_avg,
            last,
        ])

def main():
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    total_registros = 0

    with OUTPUT.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow([
            "TradeDate",
            "TickerSymbol",
            "NtlFinVol",
            "IntlFinVol",
            "FinInstrmQty",
            "FrstPric",
            "MinPric",
            "MaxPric",
            "TradAvrgPric",
            "LastPric",
        ])

        for xml_path in xml_files:
            antes = total_registros
            process_file(xml_path, writer)
            # para contar, abrimos de novo rapidinho
            # (simples: recontar linhas depois, se quiser algo mais preciso)
        # contagem simples (só para debug rápido)
    # reconta as linhas (menos o cabeçalho)
    with OUTPUT.open("r", encoding="utf-8") as f:
        linhas = sum(1 for _ in f) - 1

    print(f"CSV gerado em: {OUTPUT}")
    print(f"Total de registros (linhas de dados): {linhas}")

if __name__ == "__main__":
    main()
