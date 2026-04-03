import json
import sys
from pathlib import Path

from openpyxl import Workbook


def main() -> int:
    if len(sys.argv) != 3:
        print("Usage: write_workbook_from_json.py <payload.json> <output.xlsx>", file=sys.stderr)
        return 2

    payload_path = Path(sys.argv[1])
    output_path = Path(sys.argv[2])

    with payload_path.open("r", encoding="utf-8") as handle:
        payload = json.load(handle)

    workbook = Workbook()
    default_sheet = workbook.active
    workbook.remove(default_sheet)

    for sheet_name, sheet_payload in payload.items():
        worksheet = workbook.create_sheet(title=sheet_name)
        columns = sheet_payload.get("columns", [])
        rows = sheet_payload.get("rows", [])

        if isinstance(columns, str):
            columns = [columns]
        elif isinstance(columns, dict):
            columns = list(columns.values())

        if isinstance(rows, dict):
            rows = [rows]

        if columns:
            worksheet.append(columns)

        for row in rows:
            worksheet.append([row.get(column) for column in columns])

    output_path.parent.mkdir(parents=True, exist_ok=True)
    workbook.save(output_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
