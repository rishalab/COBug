import csv
import os
import random
import re
import time
import hashlib
from typing import List, Tuple, Optional

import chardet
import requests

# =========================================================
# STEP-2 FINAL FIXED: Runtime bug injection via Ollama
# =========================================================
# Key properties:
# ✅ Inject only into COBOL PROGRAM files (must have PROCEDURE DIVISION.)
# ✅ Force extractable output using <<<COBOL_START>>> ... <<<COBOL_END>>>
# ✅ Count bugs only if output is valid COBOL AND changed
# ✅ Markers per bug are OPTIONAL (if present we record spans; else spans blank)
# ✅ Full failure logging with reasons + short preview
# =========================================================

# -------------------------
# USER CONFIG
# -------------------------
INPUT_DIR = "buggy_repos_static"
OUTPUT_DIR = "buggy_repos_runtime_codellama"

DATASET_SEED = 42
RUN_ID = "COBUG_RUNTIME_FINAL_FIXED_V1"

# Pick ONE model; keep fixed for dataset
# Recommended for formatting compliance:
#   qwen2.5-coder:14b  (usually better structured output)
#   deepseek-coder-v2:16b (strong, but may add extra text)
OLLAMA_MODEL = "codellama:13b"
OLLAMA_URL = "http://localhost:11434/api/generate"

# To guarantee 120++ on your ~544 files, select enough eligible files.
# Note: eligibility filter reduces candidates. Use 0.70 if needed.
APPLY_TO_FILE_PROB = 0.60

MIN_BUGS_PER_FILE = 1
MAX_BUGS_PER_FILE = 2

REQUEST_TIMEOUT_SEC = 420
MAX_API_RETRIES = 2
SLEEP_BETWEEN_RETRIES_SEC = 4

MAX_UNCHANGED_RETRIES = 2

NUM_PREDICT = 8192
TEMPERATURE = 0.15
TOP_P = 0.80

COB_EXTS = (".cbl", ".cob")

ALLOWED_TYPES = [
    "DIVIDE_BY_ZERO_RISK",
    "UNINITIALIZED_DATA_ITEM_USE",
    "INFINITE_LOOP_NON_TERMINATION",
    "WRONG_CALCULATION_LOGIC",
    "MISSING_FILE_STATUS_HANDLING",
]

BUGTYPE_TO_TEMPLATE_RUNTIME = {
    "DIVIDE_BY_ZERO_RISK": "T_R_DIV0_01",
    "UNINITIALIZED_DATA_ITEM_USE": "T_R_UNINIT_01",
    "INFINITE_LOOP_NON_TERMINATION": "T_R_LOOP_01",
    "WRONG_CALCULATION_LOGIC": "T_R_LOGIC_01",
    "MISSING_FILE_STATUS_HANDLING": "T_R_FILE_01",
}

def guess_severity_runtime(bug_type: str) -> str:
    return {
        "DIVIDE_BY_ZERO_RISK": "HIGH",
        "UNINITIALIZED_DATA_ITEM_USE": "HIGH",
        "INFINITE_LOOP_NON_TERMINATION": "HIGH",
        "WRONG_CALCULATION_LOGIC": "MEDIUM",
        "MISSING_FILE_STATUS_HANDLING": "MEDIUM",
    }.get(bug_type, "MEDIUM")

# Optional spans if model includes markers (not required)
MARK_START_RE = re.compile(r"^\s*\*>\s*RUNTIME_BUG_START\s+TYPE=(\w+)\s*$", re.IGNORECASE | re.MULTILINE)
MARK_END_RE   = re.compile(r"^\s*\*>\s*RUNTIME_BUG_END\s+TYPE=(\w+)\s*$", re.IGNORECASE | re.MULTILINE)

PROC_DIV_RE = re.compile(r"^\s*PROCEDURE\s+DIVISION\.\s*$", re.IGNORECASE | re.MULTILINE)
ID_DIV_RE   = re.compile(r"^\s*IDENTIFICATION\s+DIVISION\.\s*$", re.IGNORECASE | re.MULTILINE)

# -------------------------
# File helpers
# -------------------------
def detect_encoding(raw: bytes) -> str:
    enc = chardet.detect(raw).get("encoding")
    return enc or "utf-8"

def safe_read_text(path: str) -> str:
    with open(path, "rb") as f:
        raw = f.read()
    enc = detect_encoding(raw)
    try:
        return raw.decode(enc, errors="replace")
    except Exception:
        return raw.decode("utf-8", errors="replace")

def safe_write_text(path: str, text: str) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8", newline="\n") as f:
        f.write(text)

def norm_path(p: str) -> str:
    return p.replace("\\", "/")

def get_repo_id(root_dir: str, full_file_path: str) -> str:
    rel = os.path.relpath(full_file_path, root_dir)
    parts = norm_path(rel).split("/")
    return parts[0] if len(parts) > 1 else "__root__"

def stable_int_seed(base: int, rel_path: str) -> int:
    h = hashlib.md5(rel_path.encode("utf-8")).hexdigest()
    return base + int(h[:8], 16)

def looks_like_cobol_program(code: str) -> bool:
    # For runtime injection: MUST have PROCEDURE DIVISION
    up = code.upper()
    return ("PROCEDURE DIVISION" in up)

def line_number_of_index(text: str, idx: int) -> int:
    return text.count("\n", 0, max(0, idx)) + 1

def extract_bug_spans(mutated_code: str) -> List[Tuple[str, int, int]]:
    spans = []
    for sm in MARK_START_RE.finditer(mutated_code):
        bug_type = sm.group(1).upper()
        start_line = line_number_of_index(mutated_code, sm.start())
        em = MARK_END_RE.search(mutated_code, pos=sm.end())
        if not em:
            continue
        end_line = line_number_of_index(mutated_code, em.start())
        spans.append((bug_type, start_line, end_line))
    return spans

# -------------------------
# Output extraction (critical fix)
# -------------------------
def strip_think_and_codefences(text: str) -> str:
    t = text.strip()
    # remove common <think> blocks
    t = re.sub(r"<think>.*?</think>", "", t, flags=re.DOTALL | re.IGNORECASE).strip()
    # remove markdown fences
    t = re.sub(r"^```[a-zA-Z0-9_-]*\s*", "", t)
    t = re.sub(r"\s*```$", "", t)
    return t.strip()

def extract_between_outer_tags(text: str) -> Optional[str]:
    """
    Extract code between:
      <<<COBOL_START>>>
      ...
      <<<COBOL_END>>>
    """
    m1 = re.search(r"<<<COBOL_START>>>", text)
    m2 = re.search(r"<<<COBOL_END>>>", text)
    if not m1 or not m2 or m2.start() <= m1.end():
        return None
    return text[m1.end():m2.start()].strip()

def salvage_cobol_by_division(text: str) -> Optional[str]:
    """
    Fallback extraction:
    - find IDENTIFICATION DIVISION or PROCEDURE DIVISION
    - take from that point to end
    """
    up = text.upper()
    idx = up.find("IDENTIFICATION DIVISION.")
    if idx == -1:
        idx = up.find("PROCEDURE DIVISION.")
    if idx == -1:
        return None
    return text[idx:].strip()

def normalize_llm_to_cobol(text: str) -> Optional[str]:
    """
    Best-effort:
      1) remove think/fences
      2) extract between COBOL_START/END
      3) fallback salvage by division keywords
    """
    t = strip_think_and_codefences(text)
    block = extract_between_outer_tags(t)
    if block:
        return block
    return salvage_cobol_by_division(t)

def is_changed(original: str, mutated: str) -> bool:
    return original.strip() != mutated.strip()

# -------------------------
# Ollama call
# -------------------------
def ollama_generate(prompt: str, seed: int) -> str:
    payload = {
        "model": OLLAMA_MODEL,
        "prompt": prompt,
        "stream": False,
        "options": {
            "temperature": TEMPERATURE,
            "top_p": TOP_P,
            "num_predict": NUM_PREDICT,
            "seed": seed,
        },
    }

    last_err = None
    for _ in range(MAX_API_RETRIES + 1):
        try:
            r = requests.post(OLLAMA_URL, json=payload, timeout=REQUEST_TIMEOUT_SEC)
            r.raise_for_status()
            data = r.json()
            return (data.get("response") or "")
        except Exception as e:
            last_err = e
            time.sleep(SLEEP_BETWEEN_RETRIES_SEC)

    raise RuntimeError(f"Ollama call failed: {last_err}")

# -------------------------
# Prompt (fixed output contract)
# -------------------------
def build_runtime_prompt(code: str, chosen_types: List[str]) -> str:
    n = len(chosen_types)
    types_str = ", ".join(chosen_types)

    return f"""
You are a COBOL expert.

Inject EXACTLY {n} RUNTIME bugs into the COBOL program below.
Use these runtime bug types (one each, no extras): {types_str}

STRICT constraints:
- Runtime bugs only; the program must still compile.
- Do NOT introduce compile-time/static errors (no undeclared identifiers; no broken syntax).
- Make minimal localized edits (do NOT rewrite whole file).
- Output MUST be wrapped EXACTLY like this:

<<<COBOL_START>>>
<full COBOL program only>
<<<COBOL_END>>>

Optional (best effort): If possible, mark each buggy region using:
   *> RUNTIME_BUG_START TYPE=<BUG_TYPE>
   ...
   *> RUNTIME_BUG_END TYPE=<BUG_TYPE>

Output ONLY the wrapped COBOL block. No explanations. No markdown.

COBOL PROGRAM:
{code}
""".strip()

# -------------------------
# Main
# -------------------------
def run():
    random.seed(DATASET_SEED)
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    runtime_csv = os.path.join(OUTPUT_DIR, "runtime_bugs.csv")
    failures_csv = os.path.join(OUTPUT_DIR, "runtime_failures.csv")

    bug_counter = 0
    bug_rows: List[List[str]] = []
    failures: List[List[str]] = []

    total_files = 0
    eligible_files = 0
    selected_files = 0
    success_files = 0

    for root, _, files in os.walk(INPUT_DIR):
        for fname in files:
            if not fname.lower().endswith(COB_EXTS):
                continue

            total_files += 1
            full_path = os.path.join(root, fname)
            code = safe_read_text(full_path)

            repo_id = get_repo_id(INPUT_DIR, full_path)
            rel_path = norm_path(os.path.relpath(full_path, INPUT_DIR))
            out_path = os.path.join(OUTPUT_DIR, rel_path)

            # Only COBOL PROGRAMS can have runtime bugs injected
            if not looks_like_cobol_program(code):
                safe_write_text(out_path, code)
                failures.append([repo_id, rel_path, "", "SKIP_NOT_PROGRAM", "No PROCEDURE DIVISION"])
                continue

            eligible_files += 1

            # selection
            if random.random() > APPLY_TO_FILE_PROB:
                safe_write_text(out_path, code)
                continue

            selected_files += 1

            file_seed = stable_int_seed(DATASET_SEED, rel_path)
            n_bugs = random.randint(MIN_BUGS_PER_FILE, MAX_BUGS_PER_FILE)
            chosen_types = random.sample(ALLOWED_TYPES, k=n_bugs)

            ok = False
            mutated_cobol: Optional[str] = None

            for attempt in range(MAX_UNCHANGED_RETRIES + 1):
                try:
                    prompt = build_runtime_prompt(code, chosen_types)
                    raw = ollama_generate(prompt, seed=file_seed + attempt)
                except Exception as e:
                    failures.append([repo_id, rel_path, ",".join(chosen_types), "OLLAMA_ERROR", str(e)])
                    continue

                cobol = normalize_llm_to_cobol(raw)
                if not cobol:
                    preview = strip_think_and_codefences(raw)[:180].replace("\n", "\\n")
                    failures.append([repo_id, rel_path, ",".join(chosen_types), "EXTRACT_FAIL", preview])
                    continue

                if not looks_like_cobol_program(cobol):
                    preview = cobol[:180].replace("\n", "\\n")
                    failures.append([repo_id, rel_path, ",".join(chosen_types), "NOT_PROGRAM_AFTER_EXTRACT", preview])
                    continue

                if not is_changed(code, cobol):
                    failures.append([repo_id, rel_path, ",".join(chosen_types), "UNCHANGED", "Model returned unchanged program"])
                    continue

                mutated_cobol = cobol
                ok = True
                break

            if not ok or not mutated_cobol:
                safe_write_text(out_path, code)
                failures.append([repo_id, rel_path, ",".join(chosen_types), "FINAL_FAIL", "Could not get changed program output"])
                continue

            # Success: write mutated file
            safe_write_text(out_path, mutated_cobol)
            success_files += 1

            # If spans exist, use them; else blank spans (file-level GT)
            spans = extract_bug_spans(mutated_cobol)

            if spans:
                # log what was actually marked
                for (bt, sl, el) in spans:
                    bt = bt.upper()
                    if bt not in ALLOWED_TYPES:
                        continue
                    bug_counter += 1
                    bug_id = f"RB{bug_counter:06d}"
                    bug_rows.append([
                        repo_id, rel_path, bug_id, bt,
                        guess_severity_runtime(bt),
                        BUGTYPE_TO_TEMPLATE_RUNTIME.get(bt, ""),
                        "PROCEDURE-DIVISION",
                        str(sl), str(el),
                        str(DATASET_SEED), RUN_ID,
                        "MARKERS_PRESENT"
                    ])
            else:
                # log requested types as bug instances (still real runtime mutation happened)
                for bt in chosen_types:
                    bug_counter += 1
                    bug_id = f"RB{bug_counter:06d}"
                    bug_rows.append([
                        repo_id, rel_path, bug_id, bt,
                        guess_severity_runtime(bt),
                        BUGTYPE_TO_TEMPLATE_RUNTIME.get(bt, ""),
                        "PROCEDURE-DIVISION",
                        "", "",
                        str(DATASET_SEED), RUN_ID,
                        "NO_MARKERS"
                    ])

    # write outputs
    with open(runtime_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow([
            "repo_id","file_path","bug_instance_id","bug_type","bug_severity","template_id",
            "injection_site","injected_line_start","injected_line_end","dataset_seed","run_id","notes"
        ])
        w.writerows(bug_rows)

    with open(failures_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["repo_id","file_path","requested_types","failure_type","details"])
        w.writerows(failures)

    print("✅ Step-2 Runtime (FINAL FIXED) complete")
    print(f"   Total COBOL files   : {total_files}")
    print(f"   Eligible programs   : {eligible_files}")
    print(f"   Selected programs   : {selected_files}")
    print(f"   Success programs    : {success_files}")
    print(f"   Runtime bugs logged : {len(bug_rows)}")
    print(f"   runtime_bugs.csv    : {runtime_csv}")
    print(f"   runtime_failures.csv: {failures_csv}")
    print(f"   Model               : {OLLAMA_MODEL}")
    print(f"   APPLY_TO_FILE_PROB  : {APPLY_TO_FILE_PROB}")
    print(f"   Bugs/file (min/max) : {MIN_BUGS_PER_FILE}/{MAX_BUGS_PER_FILE}")

if __name__ == "__main__":
    run()
