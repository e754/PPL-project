"""
Reads BAWE_csv/ (one CSV per essay, filename = <student_id><letter>.csv)
and writes BAWE_by_student/ (one file per student, one essay per line).

Essay text is reconstructed by joining the 'word' column in
sentence_number / word_number order.
"""

import csv
import os
import re
from collections import defaultdict
from pathlib import Path

SRC = Path(__file__).parent / "BAWE_csv"
DST = Path(__file__).parent / "BAWE_by_student"
DST.mkdir(exist_ok=True)

# Group filenames by 4-digit student ID
student_files: dict[str, list[Path]] = defaultdict(list)
for f in sorted(SRC.iterdir()):
    if f.suffix == ".csv":
        m = re.match(r"^(\d{4})", f.name)
        if m:
            student_files[m.group(1)].append(f)

def essay_text(path: Path) -> str:
    rows = []
    with open(path, newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            rows.append((int(row["sentence_number"]), int(row["word_number"]), row["word"]))
    rows.sort()
    return " ".join(word for _, _, word in rows)

for student_id, paths in student_files.items():
    out_path = DST / f"{student_id}.txt"
    with open(out_path, "w", encoding="utf-8") as out:
        for path in sorted(paths):
            line = essay_text(path)
            out.write(line + "\n")

print(f"Done: {len(student_files)} student files written to {DST}")
