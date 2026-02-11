# ============================
# STEP-1: STATIC BUG INJECTION (CSV-clean, spec-compliant)
# Dataset is generated ONCE (not per seed).
# ============================

import csv
import os
import random
import re
from dataclasses import dataclass
from typing import Callable, List, Optional, Tuple

import chardet

# ---------------------------------------------------------
# ✅ USER CONFIG (EDIT HERE) — FIXED DATASET GENERATION
# ---------------------------------------------------------
X_COBOL_DIR = "X-COBOL_files"           # Clean X-COBOL root
OUTPUT_DIR = "buggy_repos_static"      # Output: static-buggy repos + static_bugs.csv

DATASET_SEED = 42                      # Fixed seed for dataset creation (keep constant)
RUN_ID = "COBUG_STATIC_V1"             # Dataset version tag

# Inject 1–2 STATIC bugs per COBOL file
MIN_BUGS_PER_FILE = 1
MAX_BUGS_PER_FILE = 2
# ---------------------------------------------------------

COB_EXTS = (".cbl", ".cob")

# -----------------------------
# Helpers
# -----------------------------
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

def get_repo_id(x_cobol_root: str, full_file_path: str) -> str:
    rel = os.path.relpath(full_file_path, x_cobol_root)
    parts = norm_path(rel).split("/")
    return parts[0] if len(parts) > 1 else "__root__"

def guess_severity(bug_type: str) -> str:
    return {
        "UNDECLARED_IDENTIFIER_REFERENCE": "HIGH",
        "UNUSED_DATA_ITEM": "LOW",
        "DEAD_CODE_AFTER_STOP_RUN": "MEDIUM",
        "REDUNDANT_GOTO": "MEDIUM",
        "MISSING_ELSE_FOR_CRITICAL_CHECK": "LOW",
    }.get(bug_type, "LOW")

def line_of_offset(text: str, offset: int) -> int:
    return text.count("\n", 0, max(0, offset)) + 1

# -----------------------------
# ✅ Template IDs (for Step-3: Bug reports without leakage)
# -----------------------------
BUGTYPE_TO_TEMPLATE = {
    "UNDECLARED_IDENTIFIER_REFERENCE": "T_UNDECL_01",
    "UNUSED_DATA_ITEM": "T_UNUSED_01",
    "DEAD_CODE_AFTER_STOP_RUN": "T_DEAD_01",
    "REDUNDANT_GOTO": "T_GOTO_01",
    "MISSING_ELSE_FOR_CRITICAL_CHECK": "T_MELSE_01",
}

@dataclass
class InjectResult:
    code: str
    applied: bool
    injection_site: Optional[str]
    injected_line_start: Optional[int]
    injected_line_end: Optional[int]
    template_id: Optional[str]

Injector = Callable[[str], InjectResult]

# -----------------------------
# Static bug injectors (COBOL-valid ONLY)
# -----------------------------
WS_SECTION_RE = re.compile(r"^\s*WORKING-STORAGE\s+SECTION\.\s*$", re.IGNORECASE | re.MULTILINE)
PROC_DIV_RE   = re.compile(r"^\s*PROCEDURE\s+DIVISION\.\s*$", re.IGNORECASE | re.MULTILINE)
STOP_RUN_RE   = re.compile(r"\bSTOP\s+RUN\.\s*$", re.IGNORECASE | re.MULTILINE)

def inject_unused_data_item(code: str) -> InjectResult:
    m = WS_SECTION_RE.search(code)
    if not m:
        return InjectResult(code, False, None, None, None, None)

    insert_pos = m.end()
    snippet = "\n       01  UNUSED-VAR PIC X(10) VALUE SPACES.\n"
    new_code = code[:insert_pos] + snippet + code[insert_pos:]
    line = line_of_offset(new_code, insert_pos)

    return InjectResult(
        code=new_code,
        applied=True,
        injection_site="WORKING-STORAGE",
        injected_line_start=line,
        injected_line_end=line,
        template_id=BUGTYPE_TO_TEMPLATE["UNUSED_DATA_ITEM"],
    )

def inject_undeclared_identifier_reference(code: str) -> InjectResult:
    m = PROC_DIV_RE.search(code)
    if not m:
        return InjectResult(code, False, None, None, None, None)

    insert_pos = m.end()
    snippet = "\n       DISPLAY UNDECLARED-VAR.\n"
    new_code = code[:insert_pos] + snippet + code[insert_pos:]
    line = line_of_offset(new_code, insert_pos)

    return InjectResult(
        code=new_code,
        applied=True,
        injection_site="PROCEDURE-DIVISION",
        injected_line_start=line,
        injected_line_end=line,
        template_id=BUGTYPE_TO_TEMPLATE["UNDECLARED_IDENTIFIER_REFERENCE"],
    )

def inject_dead_code_after_stop_run(code: str) -> InjectResult:
    m = STOP_RUN_RE.search(code)
    if not m:
        return InjectResult(code, False, None, None, None, None)

    insert_pos = m.end()
    snippet = "\n       DISPLAY \"UNREACHABLE-STATEMENT\".\n"
    new_code = code[:insert_pos] + snippet + code[insert_pos:]
    line = line_of_offset(new_code, insert_pos)

    return InjectResult(
        code=new_code,
        applied=True,
        injection_site="AFTER-STOP-RUN",
        injected_line_start=line,
        injected_line_end=line,
        template_id=BUGTYPE_TO_TEMPLATE["DEAD_CODE_AFTER_STOP_RUN"],
    )

def inject_redundant_goto(code: str) -> InjectResult:
    m = PROC_DIV_RE.search(code)
    if not m:
        return InjectResult(code, False, None, None, None, None)

    insert_pos = m.end()
    snippet = (
        "\n       GO TO ERR-HANDLER.\n"
        "\n       DISPLAY \"NORMAL-FLOW\".\n"
        "\n       ERR-HANDLER.\n"
        "           DISPLAY \"ERROR-HANDLED\".\n"
        "           EXIT.\n"
    )
    new_code = code[:insert_pos] + snippet + code[insert_pos:]
    start_line = line_of_offset(new_code, insert_pos)
    end_line = start_line + max(0, snippet.count("\n") - 1)

    return InjectResult(
        code=new_code,
        applied=True,
        injection_site="PROCEDURE-DIVISION",
        injected_line_start=start_line,
        injected_line_end=end_line,
        template_id=BUGTYPE_TO_TEMPLATE["REDUNDANT_GOTO"],
    )

def inject_missing_else_for_critical_check(code: str) -> InjectResult:
    ws_m = WS_SECTION_RE.search(code)
    proc_m = PROC_DIV_RE.search(code)
    if not ws_m or not proc_m:
        return InjectResult(code, False, None, None, None, None)

    ws_insert_pos = ws_m.end()
    ws_snippet = "\n       01  WS-FLAG PIC X VALUE 'N'.\n"
    tmp_code = code[:ws_insert_pos] + ws_snippet + code[ws_insert_pos:]

    proc_m2 = PROC_DIV_RE.search(tmp_code)
    proc_insert_pos = proc_m2.end()
    proc_snippet = "\n       IF WS-FLAG = 'Y'\n           DISPLAY \"FLAG-SET\".\n       END-IF.\n"
    new_code = tmp_code[:proc_insert_pos] + proc_snippet + tmp_code[proc_insert_pos:]

    start_line = line_of_offset(new_code, proc_insert_pos)
    end_line = start_line + max(0, proc_snippet.count("\n") - 1)

    return InjectResult(
        code=new_code,
        applied=True,
        injection_site="PROCEDURE-DIVISION",
        injected_line_start=start_line,
        injected_line_end=end_line,
        template_id=BUGTYPE_TO_TEMPLATE["MISSING_ELSE_FOR_CRITICAL_CHECK"],
    )

STATIC_INJECTORS: List[Tuple[str, Injector]] = [
    ("UNDECLARED_IDENTIFIER_REFERENCE", inject_undeclared_identifier_reference),
    ("UNUSED_DATA_ITEM", inject_unused_data_item),
    ("DEAD_CODE_AFTER_STOP_RUN", inject_dead_code_after_stop_run),
    ("REDUNDANT_GOTO", inject_redundant_goto),
    ("MISSING_ELSE_FOR_CRITICAL_CHECK", inject_missing_else_for_critical_check),
]

# -----------------------------
# Main injection pipeline
# -----------------------------
def inject_static_bugs(
    x_cobol_dir: str,
    out_dir: str,
    dataset_seed: int,
    run_id: str,
    min_bugs_per_file: int,
    max_bugs_per_file: int,
) -> str:
    random.seed(dataset_seed)
    os.makedirs(out_dir, exist_ok=True)

    csv_path = os.path.join(out_dir, "static_bugs.csv")
    bug_counter = 0
    rows: List[List[str]] = []

    for root, _, files in os.walk(x_cobol_dir):
        for fname in files:
            if not fname.lower().endswith(COB_EXTS):
                continue

            full_path = os.path.join(root, fname)
            code = safe_read_text(full_path)
            original_code = code

            repo_id = get_repo_id(x_cobol_dir, full_path)
            rel_path = norm_path(os.path.relpath(full_path, x_cobol_dir))

            # Choose 1–2 injectors per file (only those applicable)
            k = random.randint(min_bugs_per_file, max_bugs_per_file)
            pool = STATIC_INJECTORS.copy()
            random.shuffle(pool)

            applied_any = False
            applied_count = 0

            for bug_type, injector in pool:
                if applied_count >= k:
                    break

                res = injector(code)
                if not res.applied:
                    continue

                code = res.code
                applied_any = True
                applied_count += 1

                bug_counter += 1
                bug_instance_id = f"SB{bug_counter:06d}"
                severity = guess_severity(bug_type)

                rows.append([
                    repo_id,
                    rel_path,
                    bug_instance_id,
                    bug_type,
                    severity,
                    res.template_id or "",
                    res.injection_site or "",
                    str(res.injected_line_start) if res.injected_line_start is not None else "",
                    str(res.injected_line_end) if res.injected_line_end is not None else "",
                    str(dataset_seed),
                    run_id,
                ])

            # Mirror to output folder (even if no injection applied)
            out_path = os.path.join(out_dir, rel_path)
            safe_write_text(out_path, code if applied_any else original_code)

    # Write CSV (one row per bug instance)
    with open(csv_path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow([
            "repo_id",
            "file_path",
            "bug_instance_id",
            "bug_type",
            "bug_severity",
            "template_id",
            "injection_site",
            "injected_line_start",
            "injected_line_end",
            "dataset_seed",
            "run_id",
        ])
        w.writerows(rows)

    return (
        f"✅ Step-1 (Static) complete\n"
        f"   Output Dir : {out_dir}\n"
        f"   CSV        : {csv_path}\n"
        f"   Bugs Logged: {len(rows)}\n"
        f"   DatasetSeed: {dataset_seed}\n"
        f"   RunID      : {run_id}"
    )

# -----------------------------
# Run once (fixed dataset)
# -----------------------------
if __name__ == "__main__":
    print(
        inject_static_bugs(
            x_cobol_dir=X_COBOL_DIR,
            out_dir=OUTPUT_DIR,
            dataset_seed=DATASET_SEED,
            run_id=RUN_ID,
            min_bugs_per_file=MIN_BUGS_PER_FILE,
            max_bugs_per_file=MAX_BUGS_PER_FILE,
        )
    )
