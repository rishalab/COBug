# ============================
# STEP-3: BENCHMARK CSV BUILDER (NO LEAKAGE)
# Generates:
#   - bug_reports.csv (templates only, no code leakage)
#   - gt_labels.csv (bug_id -> gt_file_path)
#   - bug_file_pairs.csv (bug_id paired with every file in its repo)
#   - dataset_stats.csv (repo/type counts)
#
# ✅ NEW ADDITIONS:
#   - repo_wise_pairs/<repo_id>.csv  (pairings stored per repo)
#   - repo_bugtype_matrix.csv        (repos as rows, bug types as columns)
#
# IMPORTANT:
# - bug_id is derived from bug_instance_id (SBxxxxxx / RBxxxxxx) => unique even if text same
# - bug report text NEVER uses file names / identifiers / code tokens
# - GT comes ONLY from injection logs
# ============================

import csv
import os
import re
import random
from collections import defaultdict, Counter
from typing import Dict, List, Tuple

# ---------------------------------------------------------
# ✅ USER CONFIG
# ---------------------------------------------------------
STATIC_DIR = "buggy_repos_static"        # contains static_bugs.csv + static-buggy files
RUNTIME_DIR = "buggy_repos_runtime_codellama"      # contains runtime_bugs.csv + final buggy repos
OUT_DIR = "benchmark_csv_codellama"               # output benchmark artifacts

STATIC_CSV = os.path.join(STATIC_DIR, "static_bugs.csv")
RUNTIME_CSV = os.path.join(RUNTIME_DIR, "runtime_bugs.csv")

# Use final buggy repos for pairing candidates
CORPUS_ROOT = RUNTIME_DIR

# Bug report generation options
PARAPHRASE_PER_BUG = 1   # keep 1 for strict reproducibility; can set 2-3 later
REPORT_SEED = 123        # controls template paraphrase choice only

COB_EXTS = (".cbl", ".cob")

# ✅ NEW: repo-wise pairs folder
REPO_WISE_PAIRS_DIR = os.path.join(OUT_DIR, "repo_wise_pairs")

# ---------------------------------------------------------
# Leakage guard: forbid obvious leaks in reports
# ---------------------------------------------------------
PATH_LIKE_RE = re.compile(r"\b(?:path|file|dir|folder|directory)\b", re.IGNORECASE)
# ---------------------------------------------------------
FORBIDDEN_PATTERNS = [
    r"\.cbl\b", r"\.cob\b",             # file names
    r"[A-Z0-9-]+\b",                    # aggressive: would block normal words if all-caps; so use carefully below
]
COBOL_IDENT_RE = re.compile(r"\b[A-Z][A-Z0-9-]{2,}\b")
# ---------------------------------------------------------
# Templates (NO leakage)
# ---------------------------------------------------------
STATIC_TEMPLATES: Dict[str, List[Tuple[str, str]]] = {
    "UNDECLARED_IDENTIFIER_REFERENCE": [
        (
            "Compilation fails due to missing data item declaration",
            "A referenced data item is not declared in the DATA DIVISION, causing an unknown identifier or compile-time failure during build."
        ),
        (
            "Build error caused by undeclared identifier usage",
            "The program references a data item that has no corresponding declaration, resulting in a compilation error."
        ),
    ],
    "UNUSED_DATA_ITEM": [
        (
            "Unused data item increases maintenance risk",
            "A data item is declared but never referenced, indicating dead state or leftover code that can confuse maintainers and reviewers."
        ),
        (
            "Redundant declaration not used in processing",
            "A declared data item is never used by any statement; this suggests unnecessary data structure and possible incomplete refactoring."
        ),
    ],
    "DEAD_CODE_AFTER_STOP_RUN": [
        (
            "Unreachable statements present after termination",
            "Statements appear after program termination and can never execute, reducing clarity and possibly hiding intended behavior."
        ),
        (
            "Dead code detected in termination path",
            "Code exists in a path that is not executable due to program termination earlier in the flow."
        ),
    ],
    "REDUNDANT_GOTO": [
        (
            "Unnecessary control transfer complicates program flow",
            "A redundant control transfer changes execution flow unnecessarily, making logic harder to follow and increasing maintenance risk."
        ),
        (
            "Unsafe jump-based flow reduces readability",
            "Jump-style control flow is used where structured flow would suffice, complicating reasoning about execution paths."
        ),
    ],
    "MISSING_ELSE_FOR_CRITICAL_CHECK": [
        (
            "Missing else-handling for an important condition",
            "A conditional check has no corresponding else-path handling, which can lead to silent skipping of required actions when the condition is not met."
        ),
        (
            "Condition lacks alternative-path handling",
            "A decision point does not define behavior for the opposite condition, leading to incomplete handling of expected scenarios."
        ),
    ],
}

RUNTIME_TEMPLATES: Dict[str, List[Tuple[str, str]]] = {
    "DIVIDE_BY_ZERO_RISK": [
        (
            "Arithmetic failure during calculation under specific inputs",
            "A division operation may execute when the denominator becomes zero due to missing validation, causing abnormal termination or invalid numeric results."
        ),
        (
            "Potential divide-by-zero in processing path",
            "The program performs division without ensuring a non-zero denominator, which can fail for certain records or execution paths."
        ),
    ],
    "UNINITIALIZED_DATA_ITEM_USE": [
        (
            "Inconsistent output due to uninitialized state usage",
            "A data item may be used before being assigned a valid value, causing behavior that depends on execution path and leading to inconsistent results."
        ),
        (
            "Uninitialized value affects conditional behavior",
            "A decision uses a value that may not have been initialized, so output can vary unpredictably across runs or inputs."
        ),
    ],
    "INFINITE_LOOP_NON_TERMINATION": [
        (
            "Program does not complete due to non-terminating loop",
            "A loop termination condition is never satisfied on some paths, causing the program to hang and the job to not finish."
        ),
        (
            "Non-termination observed during processing",
            "A repeated processing block may never reach its termination condition, preventing completion for certain scenarios."
        ),
    ],
    "WRONG_CALCULATION_LOGIC": [
        (
            "Incorrect numeric results produced for valid inputs",
            "The program completes but produces incorrect computed values due to a logic error in calculation or conditional selection."
        ),
        (
            "Business rule produces wrong output",
            "A rule or computation is applied incorrectly, leading to wrong results while still running successfully."
        ),
    ],
    "MISSING_FILE_STATUS_HANDLING": [
        (
            "File operation fails without proper error handling",
            "A file read/write path lacks robust status checks, which can cause failures or incorrect behavior when file conditions are unexpected."
        ),
        (
            "I/O error not handled in file processing path",
            "File operations do not handle error/status outcomes adequately, leading to runtime failures under certain file conditions."
        ),
    ],
}

DEFAULT_SEVERITY_STATIC = "MEDIUM"
DEFAULT_SEVERITY_RUNTIME = "HIGH"


def norm_path(p: str) -> str:
    return p.replace("\\", "/")


def list_repo_files(corpus_root: str) -> Dict[str, List[str]]:
    repo_to_files: Dict[str, List[str]] = defaultdict(list)
    for root, _, files in os.walk(corpus_root):
        for fname in files:
            if not fname.lower().endswith(COB_EXTS):
                continue
            full = os.path.join(root, fname)
            rel = norm_path(os.path.relpath(full, corpus_root))
            parts = rel.split("/")
            repo_id = parts[0] if len(parts) > 1 else "__root__"
            repo_to_files[repo_id].append(rel)
    for r in repo_to_files:
        repo_to_files[r] = sorted(repo_to_files[r])
    return repo_to_files


def read_injection_csv(path: str) -> List[Dict[str, str]]:
    rows = []
    if not os.path.exists(path):
        return rows
    with open(path, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for r in reader:
            r["repo_id"] = r.get("repo_id", "").strip()
            r["file_path"] = norm_path(r.get("file_path", "").strip())
            r["bug_instance_id"] = r.get("bug_instance_id", "").strip()
            r["bug_type"] = r.get("bug_type", "").strip().upper()
            r["bug_severity"] = r.get("bug_severity", "").strip().upper()
            r["template_id"] = r.get("template_id", "").strip()
            rows.append(r)
    return rows


def choose_template(bug_type: str, is_runtime: bool, rng: random.Random) -> Tuple[str, str]:
    choices = (RUNTIME_TEMPLATES if is_runtime else STATIC_TEMPLATES).get(bug_type, [])
    if not choices:
        return (
            "Program exhibits a defect during processing",
            "A defect is observed under certain conditions; the behavior differs from expected outcomes and requires investigation."
        )
    return rng.choice(choices)


def leak_check_text(text: str) -> None:
    # if PATH_LIKE_RE.search(text):
    #     raise ValueError(f"Leakage detected: path-like token in report text: {text}")
    if re.search(r"\b[A-Z]{2,}-[A-Z0-9-]{2,}\b", text):
        raise ValueError(f"Leakage detected: COBOL identifier-like token in report text: {text}")
    if re.search(r"\bSB\d{6}\b|\bRB\d{6}\b", text):
        raise ValueError(f"Leakage detected: internal bug id leaked into report: {text}")


def build_benchmark():
    os.makedirs(OUT_DIR, exist_ok=True)
    os.makedirs(REPO_WISE_PAIRS_DIR, exist_ok=True)  # ✅ NEW

    static_rows = read_injection_csv(STATIC_CSV)
    runtime_rows = read_injection_csv(RUNTIME_CSV)

    repo_to_files = list_repo_files(CORPUS_ROOT)

    bug_instances: List[Dict[str, str]] = []

    for r in static_rows:
        bug_instances.append({
            "bug_id": r["bug_instance_id"],
            "repo_id": r["repo_id"],
            "gt_file_path": r["file_path"],
            "bug_type": r["bug_type"],
            "severity": r["bug_severity"] or DEFAULT_SEVERITY_STATIC,
            "is_runtime": "0",
            "template_id": r.get("template_id", ""),
        })

    for r in runtime_rows:
        bug_instances.append({
            "bug_id": r["bug_instance_id"],
            "repo_id": r["repo_id"],
            "gt_file_path": r["file_path"],
            "bug_type": r["bug_type"],
            "severity": r["bug_severity"] or DEFAULT_SEVERITY_RUNTIME,
            "is_runtime": "1",
            "template_id": r.get("template_id", ""),
        })

    bug_instances = [b for b in bug_instances if b["bug_id"]]

    # 4) bug_reports.csv
    rng = random.Random(REPORT_SEED)
    bug_reports_path = os.path.join(OUT_DIR, "bug_reports.csv")

    with open(bug_reports_path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["bug_id", "repo_id", "bug_type", "summary", "description", "severity", "template_id", "seed"])
        for b in bug_instances:
            is_runtime = b["is_runtime"] == "1"
            for _ in range(PARAPHRASE_PER_BUG):
                summary, desc = choose_template(b["bug_type"], is_runtime, rng)
                leak_check_text(summary)
                leak_check_text(desc)
                w.writerow([
                    b["bug_id"], b["repo_id"], b["bug_type"],
                    summary, desc, b["severity"], b["template_id"], str(REPORT_SEED)
                ])

    # 5) gt_labels.csv
    gt_path = os.path.join(OUT_DIR, "gt_labels.csv")
    with open(gt_path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["bug_id", "repo_id", "gt_file_path"])
        for b in bug_instances:
            w.writerow([b["bug_id"], b["repo_id"], b["gt_file_path"]])

    # 6) bug_file_pairs.csv + ✅ NEW repo-wise pairs
    pairs_path = os.path.join(OUT_DIR, "bug_file_pairs.csv")
    missing_repos = set()
    total_pairs = 0

    # ✅ NEW: open repo-wise writers lazily
    repo_writers = {}
    repo_files = {}
    repo_handles = {}

    def get_repo_writer(repo_id: str):
        if repo_id in repo_writers:
            return repo_writers[repo_id]
        repo_path = os.path.join(REPO_WISE_PAIRS_DIR, f"{repo_id}.csv")
        fh = open(repo_path, "w", newline="", encoding="utf-8")
        w = csv.writer(fh)
        w.writerow(["bug_id", "repo_id", "candidate_file_path", "label"])
        repo_handles[repo_id] = fh
        repo_writers[repo_id] = w
        return w

    with open(pairs_path, "w", newline="", encoding="utf-8") as f:
        w_all = csv.writer(f)
        w_all.writerow(["bug_id", "repo_id", "candidate_file_path", "label"])

        for b in bug_instances:
            repo_id = b["repo_id"]
            gt = b["gt_file_path"]

            candidates = repo_to_files.get(repo_id)
            if not candidates:
                missing_repos.add(repo_id)
                continue

            w_repo = get_repo_writer(repo_id)  # ✅ NEW

            for c in candidates:
                label = 1 if c == gt else 0
                row = [b["bug_id"], repo_id, c, label]
                w_all.writerow(row)
                w_repo.writerow(row)  # ✅ NEW
                total_pairs += 1

    # ✅ NEW: close all repo-wise files
    for fh in repo_handles.values():
        fh.close()

    # 7) dataset_stats.csv
    stats_path = os.path.join(OUT_DIR, "dataset_stats.csv")

    by_repo = Counter([b["repo_id"] for b in bug_instances])
    by_type = Counter([b["bug_type"] for b in bug_instances])
    by_repo_type = Counter([(b["repo_id"], b["bug_type"]) for b in bug_instances])

    with open(stats_path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["kind", "key", "count"])
        for repo_id, cnt in sorted(by_repo.items()):
            w.writerow(["bugs_per_repo", repo_id, cnt])
        for bug_type, cnt in sorted(by_type.items()):
            w.writerow(["bugs_per_type", bug_type, cnt])
        for (repo_id, bug_type), cnt in sorted(by_repo_type.items()):
            w.writerow(["bugs_per_repo_type", f"{repo_id}::{bug_type}", cnt])

        w.writerow(["info", "total_bug_instances", len(bug_instances)])
        w.writerow(["info", "total_pairs", total_pairs])
        if missing_repos:
            w.writerow(["warning", "repos_missing_in_corpus_listing", ";".join(sorted(missing_repos))])

    # ✅ NEW: repo × bug_type matrix for visual understanding
    matrix_path = os.path.join(OUT_DIR, "repo_bugtype_matrix.csv")
    all_repos = sorted(set(b["repo_id"] for b in bug_instances))
    all_types = sorted(set(b["bug_type"] for b in bug_instances))

    # build matrix values
    counts = defaultdict(int)
    for b in bug_instances:
        counts[(b["repo_id"], b["bug_type"])] += 1

    with open(matrix_path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["repo_id"] + all_types)
        for repo_id in all_repos:
            w.writerow([repo_id] + [counts[(repo_id, t)] for t in all_types])

    print("✅ Step-3 complete (with repo-wise pairs + matrix)")
    print(f"   Output folder          : {OUT_DIR}")
    print(f"   bug_reports.csv        : {bug_reports_path}")
    print(f"   gt_labels.csv          : {gt_path}")
    print(f"   bug_file_pairs.csv     : {pairs_path}")
    print(f"   repo_wise_pairs/       : {REPO_WISE_PAIRS_DIR}")
    print(f"   repo_bugtype_matrix.csv: {matrix_path}")
    print(f"   dataset_stats.csv      : {stats_path}")
    print(f"   bug_instances          : {len(bug_instances)}")
    print(f"   total_pairs            : {total_pairs}")
    if missing_repos:
        print(f"⚠️ Missing repos in corpus listing: {sorted(missing_repos)}")


if __name__ == "__main__":
    build_benchmark()
