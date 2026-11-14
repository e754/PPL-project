"""
Builds per-student word frequency tables from BAWE_csv/.
- Uses the 'lemma' column (normalises inflected forms)
- Skips -LRB- / -RRB- bracket artifacts and punctuation-only tokens

Outputs three folders:
  total_word_frequency/   — all essays
  first_essay_frequency/  — first essay only
  rest_essay_frequency/   — all essays except the first
"""

import csv
import re
from collections import Counter, defaultdict
from pathlib import Path

SRC = Path(__file__).parent / "BAWE_csv"

TOTAL_DST = Path(__file__).parent / "total_word_frequency"
FIRST_DST = Path(__file__).parent / "first_essay_frequency"
REST_DST  = Path(__file__).parent / "rest_essay_frequency"

for d in (TOTAL_DST, FIRST_DST, REST_DST):
    d.mkdir(exist_ok=True)

ARTIFACTS = {"-lrb-", "-rrb-", "lrb", "rrb"}

def is_valid(lemma: str) -> bool:
    lemma = lemma.lower()
    if lemma in ARTIFACTS:
        return False
    if not re.search(r"[a-z]", lemma):
        return False
    return True

def count_file(path: Path) -> Counter:
    counter: Counter = Counter()
    with open(path, newline="", encoding="utf-8") as fh:
        for row in csv.DictReader(fh):
            lemma = row["lemma"].lower()
            if is_valid(lemma):
                counter[lemma] += 1
    return counter

def write_freq(counter: Counter, path: Path) -> None:
    with open(path, "w", newline="", encoding="utf-8") as out:
        writer = csv.writer(out)
        writer.writerow(["word", "frequency"])
        writer.writerows(counter.most_common())

# Group CSV files by student ID (sorted = alphabetical essay order)
student_files: dict[str, list[Path]] = defaultdict(list)
for f in sorted(SRC.glob("*.csv")):
    m = re.match(r"^(\d{4})", f.name)
    if m:
        student_files[m.group(1)].append(f)

for student_id, paths in student_files.items():
    first  = paths[0]
    rest   = paths[1:]

    total_counter = Counter()
    for p in paths:
        total_counter += count_file(p)

    first_counter = count_file(first)

    rest_counter = Counter()
    for p in rest:
        rest_counter += count_file(p)

    write_freq(total_counter, TOTAL_DST / f"{student_id}.csv")
    write_freq(first_counter, FIRST_DST / f"{student_id}.csv")
    if rest_counter:
        write_freq(rest_counter, REST_DST / f"{student_id}.csv")

print(f"Done: {len(student_files)} students processed.")
print(f"  total_word_frequency/ : {len(list(TOTAL_DST.glob('*.csv')))} files")
print(f"  first_essay_frequency/: {len(list(FIRST_DST.glob('*.csv')))} files")
print(f"  rest_essay_frequency/ : {len(list(REST_DST.glob('*.csv')))} files")
