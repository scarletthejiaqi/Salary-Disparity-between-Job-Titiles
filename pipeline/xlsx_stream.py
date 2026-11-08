"""Stream selected columns from the first worksheet of an .xlsx file to CSV.

The DOL disclosure spreadsheets are up to ~300 MB compressed (several GB of
XML). Libraries that build the whole sheet in memory need 10-20+ GB for the
largest files; this reader walks the XML row by row with lxml.iterparse and
keeps memory flat (~100-300 MB, mostly the shared-strings table).

Usage:
    python3 -I xlsx_stream.py INPUT.xlsx OUTPUT.csv COL1,COL2,...

Columns are matched against the header row (first non-empty row). Requested
columns that are absent are written as empty strings. Cell values are written
as stored: numbers (including Excel date serials) as plain numbers, text as text.
"""

import csv
import sys
import zipfile

from lxml import etree

NS = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
REL_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
PKG_REL_NS = "http://schemas.openxmlformats.org/package/2006/relationships"
T = "{%s}t" % NS
C = "{%s}c" % NS
V = "{%s}v" % NS
IS = "{%s}is" % NS
ROW = "{%s}row" % NS
SI = "{%s}si" % NS


def first_sheet_path(zf):
    wb = etree.fromstring(zf.read("xl/workbook.xml"))
    sheet = wb.find(".//{%s}sheets/{%s}sheet" % (NS, NS))
    rid = sheet.get("{%s}id" % REL_NS)
    rels = etree.fromstring(zf.read("xl/_rels/workbook.xml.rels"))
    for rel in rels.iter("{%s}Relationship" % PKG_REL_NS):
        if rel.get("Id") == rid:
            target = rel.get("Target").lstrip("/")
            return target if target.startswith("xl/") else "xl/" + target
    return "xl/worksheets/sheet1.xml"


def shared_strings(zf):
    if "xl/sharedStrings.xml" not in zf.namelist():
        return []
    out = []
    with zf.open("xl/sharedStrings.xml") as fh:
        for _, si in etree.iterparse(fh, events=("end",), tag=SI):
            out.append("".join(t.text or "" for t in si.iter(T)))
            si.clear()
            while si.getprevious() is not None:
                del si.getparent()[0]
    return out


def cell_value(c, sst):
    t = c.get("t")
    if t == "inlineStr":
        is_el = c.find(IS)
        return "".join(x.text or "" for x in is_el.iter(T)) if is_el is not None else ""
    v = c.find(V)
    if v is None or v.text is None:
        return ""
    if t == "s":
        return sst[int(v.text)]
    return v.text


def col_letters(index):
    letters = ""
    index += 1
    while index:
        index, rem = divmod(index - 1, 26)
        letters = chr(65 + rem) + letters
    return letters


def main(src, dst, wanted):
    digits = "0123456789"
    with zipfile.ZipFile(src) as zf:
        sst = shared_strings(zf)
        sheet = first_sheet_path(zf)
        want_pos = None  # column letters -> output position
        header = None
        n = 0
        width = len(wanted)
        with zf.open(sheet) as fh, open(dst, "w", newline="", encoding="utf-8") as out:
            writer = csv.writer(out)
            writer.writerow(wanted)
            for _, row in etree.iterparse(fh, events=("end",), tag=ROW):
                if want_pos is None:
                    names = {}
                    for pos, c in enumerate(row):
                        ref = c.get("r")
                        letters = ref.rstrip(digits) if ref else col_letters(pos)
                        names[letters] = cell_value(c, sst).strip()
                    if any(names.values()):
                        header = {v: k for k, v in names.items() if v}
                        want_pos = {header[w]: i for i, w in enumerate(wanted) if w in header}
                else:
                    vals = [""] * width
                    hit = False
                    for pos, c in enumerate(row):
                        ref = c.get("r")
                        j = want_pos.get(ref.rstrip(digits) if ref else col_letters(pos))
                        if j is not None:
                            v = cell_value(c, sst)
                            if v:
                                vals[j] = v
                                hit = True
                    if hit:  # skip formatting-only rows at the end of the sheet
                        writer.writerow(vals)
                        n += 1
                row.clear()
                while row.getprevious() is not None:
                    del row.getparent()[0]
    missing = [w for w in wanted if header is None or w not in header]
    sys.stderr.write("%s: %d rows, missing columns: %s\n" % (src, n, ",".join(missing) or "none"))


if __name__ == "__main__":
    if len(sys.argv) != 4:
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2], sys.argv[3].split(","))
