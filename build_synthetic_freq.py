"""
Builds per-essay word frequency tables for all synthetic essays.
Applies the same cleaning as BAWE:
  - lemmatise using spaCy (matches the lemmatizer used to produce BAWE CSVs)
  - skip punctuation, spaces, and tokens with no letters
Output: synthetic_data_frequency/<Model>_<NN>.csv  (word, frequency)
"""

import csv
import re
from collections import Counter
from pathlib import Path

import spacy

nlp = spacy.load("en_core_web_sm")

SRC = Path(__file__).parent / "synthetic_data"
DST = Path(__file__).parent / "synthetic_data_frequency"
DST.mkdir(exist_ok=True)

ARTIFACTS = {"-lrb-", "-rrb-", "lrb", "rrb"}

# Matches LaTeX commands, math-mode delimiters, and inline math fragments
LATEX_RE = re.compile(r'\\[a-zA-Z]+|\$.*?\$|\^|_|\{|\}')
# Em-dash and en-dash used without spaces (fuse adjacent words into one token)
EMDASH_RE = re.compile(r'[—–]')

def strip_latex(text: str) -> str:
    text = LATEX_RE.sub(' ', text)
    text = EMDASH_RE.sub(' ', text)
    return text

def is_valid(t: str) -> bool:
    t = t.lower()
    if t in ARTIFACTS:
        return False
    if not re.search(r'[a-z]', t):
        return False
    if re.search(r'[$\\^_{}]', t):  # residual LaTeX fragments
        return False
    return True

def essay_freq(text: str) -> Counter:
    doc = nlp(strip_latex(text))
    counter: Counter = Counter()
    for token in doc:
        if token.is_punct or token.is_space:
            continue
        lemma = token.lemma_.lower()
        if is_valid(lemma):
            counter[lemma] += 1
    return counter

MODEL_LABELS = {
    'chatgpt_combined.csv': 'chatgpt',
    'gemini_combined.csv':  'gemini',
    'deepseek_combined.csv': 'deepseek',
    'claude_combined.csv':  'claude',
}

total = 0
for fname, model_label in MODEL_LABELS.items():
    path = SRC / fname
    with open(path, newline='', encoding='utf-8') as fh:
        rows = list(csv.DictReader(fh))
    for row in rows:
        essay_id = int(row['essay_id'])
        counter = essay_freq(row['essay'])
        out_name = f"{model_label}_{essay_id:02d}.csv"
        with open(DST / out_name, 'w', newline='', encoding='utf-8') as out:
            writer = csv.writer(out)
            writer.writerow(['word', 'frequency'])
            writer.writerows(counter.most_common())
        total += 1

print(f"Done: {total} frequency files written to {DST}")
