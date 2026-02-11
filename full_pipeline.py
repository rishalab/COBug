import os
import csv
import random
from dataclasses import dataclass
from collections import defaultdict
from typing import Dict, List, Tuple

import numpy as np

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import normalize

from scipy.stats import wilcoxon
import warnings

warnings.filterwarnings(
    "ignore",
    message="`sklearn.utils.parallel.delayed` should be used",
    category=UserWarning,
)


# ==========================================================
# USER CONFIG
# ==========================================================
BENCH_DIR = "benchmark_csv_codellama"
CORPUS_ROOT = "buggy_repos_runtime_codellama"

BUG_REPORTS_CSV = os.path.join(BENCH_DIR, "bug_reports.csv")
GT_LABELS_CSV = os.path.join(BENCH_DIR, "gt_labels.csv")
BUG_FILE_PAIRS_CSV = os.path.join(BENCH_DIR, "bug_file_pairs.csv")

OUT_DIR = "results_codellama"

SEEDS = list(range(1))       # 10 seeds
INNER_KFOLDS = 5             # inner CV folds

COB_EXTS = (".cbl", ".cob")

SUMMARY_BOOST = 2.0          # summary field weighting factor (rVSM-like)

MANUAL_AUDIT_SAMPLES = 50
MANUAL_AUDIT_SEED = 2026


# ==========================================================
# DATA STRUCTURES
# ==========================================================
@dataclass
class BugReport:
    bug_id: str
    repo_id: str
    bug_type: str
    summary: str
    description: str
    severity: str


@dataclass
class PairRow:
    bug_id: str
    repo_id: str
    candidate_file_path: str
    label: int


# ==========================================================
# BASIC UTILS
# ==========================================================
def ensure_dir(p: str) -> None:
    os.makedirs(p, exist_ok=True)


def norm_path(p: str) -> str:
    return p.replace("\\", "/")


def safe_read_text(path: str) -> str:
    with open(path, "rb") as f:
        raw = f.read()
    try:
        return raw.decode("utf-8", errors="replace")
    except Exception:
        return raw.decode("latin-1", errors="replace")


# ==========================================================
# LOADERS (STEP-3 CSVs)
# ==========================================================
def load_bug_reports(path: str) -> Dict[str, BugReport]:
    out: Dict[str, BugReport] = {}
    with open(path, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            bid = row["bug_id"].strip()
            if bid in out:
                continue
            out[bid] = BugReport(
                bug_id=bid,
                repo_id=row["repo_id"].strip(),
                bug_type=row["bug_type"].strip(),
                summary=row["summary"].strip(),
                description=row["description"].strip(),
                severity=row.get("severity", "").strip(),
            )
    return out


def load_gt_labels(path: str) -> Dict[str, str]:
    gt: Dict[str, str] = {}
    with open(path, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            gt[row["bug_id"].strip()] = norm_path(row["gt_file_path"].strip())
    return gt


def load_pairs(path: str) -> List[PairRow]:
    pairs: List[PairRow] = []
    with open(path, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            pairs.append(
                PairRow(
                    bug_id=row["bug_id"].strip(),
                    repo_id=row["repo_id"].strip(),
                    candidate_file_path=norm_path(row["candidate_file_path"].strip()),
                    label=int(row["label"]),
                )
            )
    return pairs


# ==========================================================
# CORPUS HELPERS
# ==========================================================
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


def build_doc_cache(corpus_root: str, repo_to_files: Dict[str, List[str]]) -> Dict[str, str]:
    cache: Dict[str, str] = {}
    for _, files in repo_to_files.items():
        for rel in files:
            full = os.path.join(corpus_root, rel)
            cache[rel] = safe_read_text(full)
    return cache


# ==========================================================
# METRICS
# ==========================================================
def average_precision(labels_ranked: List[int]) -> float:
    num_rel = 0
    ap_sum = 0.0
    for i, y in enumerate(labels_ranked, start=1):
        if y == 1:
            num_rel += 1
            ap_sum += (num_rel / i)
    if num_rel == 0:
        return 0.0
    return ap_sum / num_rel


def reciprocal_rank(labels_ranked: List[int]) -> float:
    for i, y in enumerate(labels_ranked, start=1):
        if y == 1:
            return 1.0 / i
    return 0.0


def hit_at_k(labels_ranked: List[int], k: int) -> int:
    return 1 if any(labels_ranked[:k]) else 0


# ==========================================================
# TF-IDF (NO GLOBAL VOCAB LEAKAGE)
# ==========================================================
def build_vectorizer(train_doc_texts: List[str]) -> TfidfVectorizer:
    vec = TfidfVectorizer(
        lowercase=True,
        token_pattern=r"(?u)\b\w+\b",
        min_df=1,
        max_df=1.0,
        sublinear_tf=True,
        norm="l2",  # doc vectors become L2-normalized; queries we normalize too
    )
    vec.fit(train_doc_texts)
    return vec


def sparse_cosine(q_row, d_row) -> float:
    # Both are TF-IDF L2-normalized row vectors.
    # Cosine = dot product for normalized vectors.
    return float(q_row.multiply(d_row).sum())


# ==========================================================
# INNER CV SPLIT (BUG_ID-LEVEL)
# ==========================================================
def inner_kfold_split(bug_ids: List[str], k: int, seed: int) -> List[Tuple[List[str], List[str]]]:
    rng = random.Random(seed)
    items = list(bug_ids)
    rng.shuffle(items)

    k = max(2, min(k, len(items)))  # prevent k > n, ensure at least 2 folds
    folds = [items[i::k] for i in range(k)]

    splits = []
    for i in range(k):
        val_ids = folds[i]
        train_ids = [x for j, fold in enumerate(folds) if j != i for x in fold]
        splits.append((train_ids, val_ids))
    return splits


# ==========================================================
# MODEL TRAINERS (WITH INNER CV)
# ==========================================================
def train_lr_with_cv(X: np.ndarray, y: np.ndarray, groups: List[str], bug_ids: List[str], seed: int) -> LogisticRegression:
    Cs = [0.1, 1.0, 3.0, 10.0]

    idx_by_bug = defaultdict(list)
    for i, bid in enumerate(groups):
        idx_by_bug[bid].append(i)

    splits = inner_kfold_split(bug_ids, INNER_KFOLDS, seed=seed)

    best_C = Cs[0]
    best_score = -1.0

    for C in Cs:
        fold_scores = []
        for train_bugs, val_bugs in splits:
            train_idx = [ii for b in train_bugs for ii in idx_by_bug[b]]
            val_idx = [ii for b in val_bugs for ii in idx_by_bug[b]]

            if not train_idx or not val_idx:
                continue
            if len(set(y[train_idx].tolist())) < 2:
                continue

            model = LogisticRegression(
                C=C,
                max_iter=2000,
                random_state=seed,
                solver="liblinear",
            )
            model.fit(X[train_idx], y[train_idx])

            ap_vals = []
            for b in val_bugs:
                rows = idx_by_bug[b]
                if not rows:
                    continue
                probs = model.predict_proba(X[rows])[:, 1]
                order = np.argsort(-probs)
                labels_ranked = [int(y[rows[i]]) for i in order]
                ap_vals.append(average_precision(labels_ranked))

            if ap_vals:
                fold_scores.append(float(np.mean(ap_vals)))

        if fold_scores:
            score = float(np.mean(fold_scores))
            if score > best_score:
                best_score = score
                best_C = C

    final = LogisticRegression(
        C=best_C,
        max_iter=2000,
        random_state=seed,
        solver="liblinear",
    )
    final.fit(X, y)
    return final


def train_rf_with_cv(X: np.ndarray, y: np.ndarray, groups: List[str], bug_ids: List[str], seed: int) -> RandomForestClassifier:
    grids = [
        {"n_estimators": 200, "max_depth": None},
        {"n_estimators": 300, "max_depth": None},
        {"n_estimators": 300, "max_depth": 20},
    ]

    idx_by_bug = defaultdict(list)
    for i, bid in enumerate(groups):
        idx_by_bug[bid].append(i)

    splits = inner_kfold_split(bug_ids, INNER_KFOLDS, seed=seed)

    best_cfg = grids[0]
    best_score = -1.0

    for cfg in grids:
        fold_scores = []
        for train_bugs, val_bugs in splits:
            train_idx = [ii for b in train_bugs for ii in idx_by_bug[b]]
            if not train_idx:
                continue
            if len(set(y[train_idx].tolist())) < 2:
                continue

            model = RandomForestClassifier(
                n_estimators=cfg["n_estimators"],
                max_depth=cfg["max_depth"],
                random_state=seed,
                n_jobs=1,
                class_weight="balanced_subsample",
            )
            model.fit(X[train_idx], y[train_idx])

            ap_vals = []
            for b in val_bugs:
                rows = idx_by_bug[b]
                if not rows:
                    continue
                probs = model.predict_proba(X[rows])[:, 1]
                order = np.argsort(-probs)
                labels_ranked = [int(y[rows[i]]) for i in order]
                ap_vals.append(average_precision(labels_ranked))

            if ap_vals:
                fold_scores.append(float(np.mean(ap_vals)))

        if fold_scores:
            score = float(np.mean(fold_scores))
            if score > best_score:
                best_score = score
                best_cfg = cfg

    final = RandomForestClassifier(
        n_estimators=best_cfg["n_estimators"],
        max_depth=best_cfg["max_depth"],
        random_state=seed,
        n_jobs=-1,
        class_weight="balanced_subsample",
    )
    final.fit(X, y)
    return final


# ==========================================================
# MANUAL AUDIT SUPPORT (OpenCBS-style motivation)
# ==========================================================
def write_manual_validation_samples(
    bugs: Dict[str, BugReport],
    gt: Dict[str, str],
    out_path: str,
    n: int,
    seed: int,
) -> None:
    rng = random.Random(seed)
    bug_ids = sorted(list(bugs.keys()))
    sample = bug_ids if len(bug_ids) <= n else rng.sample(bug_ids, n)

    with open(out_path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["bug_id", "repo_id", "bug_type", "summary", "description", "gt_file_path"])
        for bid in sample:
            b = bugs[bid]
            w.writerow([bid, b.repo_id, b.bug_type, b.summary, b.description, gt.get(bid, "")])


def write_manual_audit_template(samples_csv: str, out_template_csv: str) -> None:
    with open(samples_csv, "r", encoding="utf-8") as f_in, open(out_template_csv, "w", newline="", encoding="utf-8") as f_out:
        r = csv.DictReader(f_in)
        w = csv.writer(f_out)
        w.writerow([
            "bug_id", "repo_id", "bug_type", "summary", "description", "gt_file_path",
            "leakage_free_yes_no", "gt_correct_yes_no", "bug_type_matches_yes_no", "notes"
        ])
        for row in r:
            w.writerow([
                row["bug_id"], row["repo_id"], row["bug_type"],
                row["summary"], row["description"], row["gt_file_path"],
                "", "", "", ""
            ])


def compute_manual_audit_stats(audit_csv: str, out_stats_csv: str) -> None:
    def norm(v: str) -> str:
        return (v or "").strip().lower()

    total = 0
    valid = 0
    leakage_ok = 0
    gt_ok = 0
    type_ok = 0

    with open(audit_csv, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            total += 1
            lf = norm(row.get("leakage_free_yes_no", ""))
            gt = norm(row.get("gt_correct_yes_no", ""))
            bt = norm(row.get("bug_type_matches_yes_no", ""))

            if lf not in ("yes", "no") or gt not in ("yes", "no") or bt not in ("yes", "no"):
                continue

            valid += 1
            if lf == "yes":
                leakage_ok += 1
            if gt == "yes":
                gt_ok += 1
            if bt == "yes":
                type_ok += 1

    def pct(x: int, denom: int) -> float:
        return 0.0 if denom <= 0 else (100.0 * x / denom)

    with open(out_stats_csv, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["metric", "count", "denominator", "percentage"])
        w.writerow(["total_rows", total, total, 100.0])
        w.writerow(["valid_rows", valid, total, pct(valid, total)])
        w.writerow(["leakage_free_yes", leakage_ok, valid, pct(leakage_ok, valid)])
        w.writerow(["gt_correct_yes", gt_ok, valid, pct(gt_ok, valid)])
        w.writerow(["bug_type_matches_yes", type_ok, valid, pct(type_ok, valid)])


# ==========================================================
# CORE: EVALUATE ONE OUTER FOLD (TEST ONE REPO)
# ==========================================================
def evaluate_one_fold(
    seed: int,
    test_repo: str,
    bugs: Dict[str, BugReport],
    pairs: List[PairRow],
    repo_to_files: Dict[str, List[str]],
    doc_cache: Dict[str, str],
) -> Tuple[Dict[str, Dict[str, float]], Dict[str, Dict[str, Dict[str, float]]]]:

    train_repos = [r for r in repo_to_files.keys() if r != test_repo]

    # ---- TF-IDF fit ONLY on training repos ----
    train_doc_texts: List[str] = []
    for r in train_repos:
        for fp in repo_to_files[r]:
            train_doc_texts.append(doc_cache[fp])

    vec = build_vectorizer(train_doc_texts)

    # Split pairs by repo
    train_pairs = [p for p in pairs if p.repo_id != test_repo]
    test_pairs = [p for p in pairs if p.repo_id == test_repo]

    # Precompute doc vectors for needed docs
    needed_doc_paths = set([p.candidate_file_path for p in (train_pairs + test_pairs)])
    doc_vecs = {fp: vec.transform([doc_cache[fp]]) for fp in needed_doc_paths}

    # Precompute query vectors (field-weighted)
    needed_bug_ids = set([p.bug_id for p in (train_pairs + test_pairs)])
    q_vec_cos: Dict[str, any] = {}
    q_vec_rvsm: Dict[str, any] = {}

    alpha_summary = float(SUMMARY_BOOST)

    for bid in needed_bug_ids:
        if bid not in bugs:
            continue

        br = bugs[bid]
        q_sum = vec.transform([br.summary])
        q_desc = vec.transform([br.description])

        # Normal query: summary + description
        q_vec_cos[bid] = normalize(q_sum + q_desc, norm="l2", axis=1)

        # rVSM-like: summary weighted more than description
        q_vec_rvsm[bid] = normalize(q_sum.multiply(alpha_summary) + q_desc, norm="l2", axis=1)

    def pair_features(bid: str, fp: str) -> Tuple[float, float]:
        # If bid is missing from bug_reports.csv, stay safe (don’t crash)
        if bid not in q_vec_cos or bid not in q_vec_rvsm:
            return 0.0, 0.0
        cos = sparse_cosine(q_vec_cos[bid], doc_vecs[fp])
        rv = sparse_cosine(q_vec_rvsm[bid], doc_vecs[fp])
        return cos, rv

    # ---- training features ----
    X_train: List[List[float]] = []
    y_train: List[int] = []
    group_train: List[str] = []
    train_bug_ids = set()

    for pr in train_pairs:
        if pr.bug_id not in bugs:
            continue
        cos, rv = pair_features(pr.bug_id, pr.candidate_file_path)
        X_train.append([cos, rv])
        y_train.append(pr.label)
        group_train.append(pr.bug_id)
        train_bug_ids.add(pr.bug_id)

    X_train = np.array(X_train, dtype=np.float32)
    y_train = np.array(y_train, dtype=np.int32)
    train_bug_ids = sorted(list(train_bug_ids))

    # Guard for small data
    can_train_ml = (len(set(y_train.tolist())) >= 2 and len(train_bug_ids) >= 2)

    lr_model = None
    rf_model = None
    if can_train_ml:
        lr_model = train_lr_with_cv(X_train, y_train, group_train, train_bug_ids, seed=seed)
        rf_model = train_rf_with_cv(X_train, y_train, group_train, train_bug_ids, seed=seed)

    # ---- test grouped by bug ----
    test_by_bug = defaultdict(list)
    for pr in test_pairs:
        test_by_bug[pr.bug_id].append(pr)

    models = ["IR", "IR+LR", "IR+RF"]
    per_bug_metrics = {m: [] for m in models}
    per_bugtype_metrics = {m: defaultdict(list) for m in models}

    for bid, rows in test_by_bug.items():
        if bid not in bugs:
            continue

        bug_type = bugs[bid].bug_type

        feats = []
        for r in rows:
            cos, rv = pair_features(bid, r.candidate_file_path)
            feats.append((cos, rv, r.label))

        X_test = np.array([[f[0], f[1]] for f in feats], dtype=np.float32)
        labels_true = np.array([int(f[2]) for f in feats], dtype=np.int32)

        # ===== IR ONLY (rank by rvsm score) =====
        rv_scores = np.array([f[1] for f in feats], dtype=np.float32)
        order_ir = np.argsort(-rv_scores)
        labels_ir = [int(labels_true[i]) for i in order_ir]

        ap_ir = average_precision(labels_ir)
        mrr_ir = reciprocal_rank(labels_ir)
        t1_ir = hit_at_k(labels_ir, 1)
        t5_ir = hit_at_k(labels_ir, 5)
        t10_ir = hit_at_k(labels_ir, 10)

        per_bug_metrics["IR"].append((t1_ir, t5_ir, t10_ir, mrr_ir, ap_ir))
        per_bugtype_metrics["IR"][bug_type].append((t1_ir, t5_ir, t10_ir, mrr_ir, ap_ir))

        # ===== IR + LR =====
        if lr_model is not None:
            probs = lr_model.predict_proba(X_test)[:, 1]
            order = np.argsort(-probs)
            labels = [int(labels_true[i]) for i in order]
        else:
            labels = labels_ir

        ap = average_precision(labels)
        mrr = reciprocal_rank(labels)
        t1 = hit_at_k(labels, 1)
        t5 = hit_at_k(labels, 5)
        t10 = hit_at_k(labels, 10)
        per_bug_metrics["IR+LR"].append((t1, t5, t10, mrr, ap))
        per_bugtype_metrics["IR+LR"][bug_type].append((t1, t5, t10, mrr, ap))

        # ===== IR + RF =====
        if rf_model is not None:
            probs = rf_model.predict_proba(X_test)[:, 1]
            order = np.argsort(-probs)
            labels = [int(labels_true[i]) for i in order]
        else:
            labels = labels_ir

        ap = average_precision(labels)
        mrr = reciprocal_rank(labels)
        t1 = hit_at_k(labels, 1)
        t5 = hit_at_k(labels, 5)
        t10 = hit_at_k(labels, 10)
        per_bug_metrics["IR+RF"].append((t1, t5, t10, mrr, ap))
        per_bugtype_metrics["IR+RF"][bug_type].append((t1, t5, t10, mrr, ap))

    def agg(vals: List[Tuple[int, int, int, float, float]]) -> Dict[str, float]:
        if not vals:
            return {"top1": 0.0, "top5": 0.0, "top10": 0.0, "mrr": 0.0, "map": 0.0}
        arr = np.array(vals, dtype=np.float32)
        return {
            "top1": float(np.mean(arr[:, 0])),
            "top5": float(np.mean(arr[:, 1])),
            "top10": float(np.mean(arr[:, 2])),
            "mrr": float(np.mean(arr[:, 3])),
            "map": float(np.mean(arr[:, 4])),
        }

    repo_metrics = {m: agg(per_bug_metrics[m]) for m in models}
    repo_bugtype_metrics: Dict[str, Dict[str, Dict[str, float]]] = {}
    for m in models:
        repo_bugtype_metrics[m] = {bt: agg(v) for bt, v in per_bugtype_metrics[m].items()}

    return repo_metrics, repo_bugtype_metrics


# ==========================================================
# MAIN
# ==========================================================
def main():
    ensure_dir(OUT_DIR)

    # ---- Existence checks ----
    for p in [BUG_REPORTS_CSV, GT_LABELS_CSV, BUG_FILE_PAIRS_CSV]:
        if not os.path.exists(p):
            raise FileNotFoundError(f"Missing required input: {p}")
    if not os.path.exists(CORPUS_ROOT):
        raise FileNotFoundError(f"Missing corpus folder: {CORPUS_ROOT}")

    # ---- Load benchmark ----
    bugs = load_bug_reports(BUG_REPORTS_CSV)
    gt = load_gt_labels(GT_LABELS_CSV)
    pairs = load_pairs(BUG_FILE_PAIRS_CSV)

    # ---- Load corpus ----
    repo_to_files = list_repo_files(CORPUS_ROOT)
    repos = sorted(list(repo_to_files.keys()))
    doc_cache = build_doc_cache(CORPUS_ROOT, repo_to_files)

    # ---- Fail-fast GT correctness (very important) ----
    missing_gt = 0
    for bid, gt_path in gt.items():
        if gt_path not in doc_cache:
            missing_gt += 1
    if missing_gt > 0:
        raise RuntimeError(
            f"GT paths missing in corpus: {missing_gt}. "
            f"Check CORPUS_ROOT='{CORPUS_ROOT}' and Step-3 pairing corpus root."
        )

    # ---- Manual validation sample + template ----
    samples_csv = os.path.join(OUT_DIR, "manual_validation_samples.csv")
    write_manual_validation_samples(bugs, gt, samples_csv, MANUAL_AUDIT_SAMPLES, MANUAL_AUDIT_SEED)

    audit_csv = os.path.join(OUT_DIR, "manual_audit_results.csv")
    write_manual_audit_template(samples_csv, audit_csv)

    # ---- Evaluation outputs ----
    per_repo_rows = []
    overall_rows = []
    wilcoxon_rows = []

    models = ["IR", "IR+LR", "IR+RF"]

    per_seed_repo_bugtype_map = defaultdict(lambda: defaultdict(dict))
    per_seed_repo_bugtype_top1 = defaultdict(lambda: defaultdict(dict))

    # ---- Outer: seeds x leave-one-repo-out ----
    for seed in SEEDS:
        print(f"\n====================\nSEED = {seed}\n====================")

        seed_repo_metrics = {m: [] for m in models}
        seed_repo_map = {m: {} for m in models}

        for test_repo in repos:
            repo_metrics, repo_bugtype_metrics = evaluate_one_fold(
                seed=seed,
                test_repo=test_repo,
                bugs=bugs,
                pairs=pairs,
                repo_to_files=repo_to_files,
                doc_cache=doc_cache,
            )

            for m in models:
                met = repo_metrics[m]
                per_repo_rows.append([seed, m, test_repo, met["top1"], met["top5"], met["top10"], met["mrr"], met["map"]])
                seed_repo_metrics[m].append(met)
                seed_repo_map[m][test_repo] = met["map"]

            for m in models:
                for bt, met in repo_bugtype_metrics[m].items():
                    per_seed_repo_bugtype_map[seed][m][(test_repo, bt)] = met["map"]
                    per_seed_repo_bugtype_top1[seed][m][(test_repo, bt)] = met["top1"]

        # ---- Overall per seed: mean+std across repos ----
        for m in models:
            top1s = [x["top1"] for x in seed_repo_metrics[m]]
            top5s = [x["top5"] for x in seed_repo_metrics[m]]
            top10s = [x["top10"] for x in seed_repo_metrics[m]]
            mrrs = [x["mrr"] for x in seed_repo_metrics[m]]
            maps = [x["map"] for x in seed_repo_metrics[m]]

            overall_rows.append([
                seed, m,
                float(np.mean(top1s)), float(np.std(top1s, ddof=1)) if len(top1s) > 1 else 0.0,
                float(np.mean(top5s)), float(np.std(top5s, ddof=1)) if len(top5s) > 1 else 0.0,
                float(np.mean(top10s)), float(np.std(top10s, ddof=1)) if len(top10s) > 1 else 0.0,
                float(np.mean(mrrs)), float(np.std(mrrs, ddof=1)) if len(mrrs) > 1 else 0.0,
                float(np.mean(maps)), float(np.std(maps, ddof=1)) if len(maps) > 1 else 0.0,
            ])

        # ---- Wilcoxon signed-rank (paired per repo) on MAP ----
        comparisons = [("IR", "IR+LR"), ("IR", "IR+RF"), ("IR+LR", "IR+RF")]
        for a, b in comparisons:
            a_vals, b_vals = [], []
            for r in repos:
                if r in seed_repo_map[a] and r in seed_repo_map[b]:
                    a_vals.append(seed_repo_map[a][r])
                    b_vals.append(seed_repo_map[b][r])

            if len(a_vals) >= 5:
                try:
                    if np.allclose(np.array(a_vals) - np.array(b_vals), 0.0):
                        wilcoxon_rows.append([seed, "MAP", a, b, len(a_vals), 0.0, 1.0, 0])
                    else:
                        stat, p = wilcoxon(a_vals, b_vals, zero_method="wilcox", alternative="two-sided")
                        wilcoxon_rows.append([seed, "MAP", a, b, len(a_vals), float(stat), float(p), 1 if p < 0.05 else 0])
                except Exception:
                    wilcoxon_rows.append([seed, "MAP", a, b, len(a_vals), "", "", ""])
            else:
                wilcoxon_rows.append([seed, "MAP", a, b, len(a_vals), "", "", ""])

    # ---- Write outputs ----
    out_per_repo = os.path.join(OUT_DIR, "results_per_repo.csv")
    with open(out_per_repo, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["seed", "model", "test_repo", "top1", "top5", "top10", "mrr", "map"])
        w.writerows(per_repo_rows)

    out_overall = os.path.join(OUT_DIR, "results_overall.csv")
    with open(out_overall, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow([
            "seed", "model",
            "mean_top1", "std_top1",
            "mean_top5", "std_top5",
            "mean_top10", "std_top10",
            "mean_mrr", "std_mrr",
            "mean_map", "std_map",
        ])
        w.writerows(overall_rows)

    out_wilcoxon = os.path.join(OUT_DIR, "wilcoxon.csv")
    with open(out_wilcoxon, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["seed", "metric", "model_a", "model_b", "n_pairs", "statistic", "p_value", "significant_0.05"])
        w.writerows(wilcoxon_rows)

    # ---- results_across_seeds.csv ----
    out_across = os.path.join(OUT_DIR, "results_across_seeds.csv")
    by_model = defaultdict(lambda: defaultdict(list))
    for row in overall_rows:
        model = row[1]
        by_model[model]["mean_top1"].append(float(row[2]))
        by_model[model]["mean_top5"].append(float(row[4]))
        by_model[model]["mean_top10"].append(float(row[6]))
        by_model[model]["mean_mrr"].append(float(row[8]))
        by_model[model]["mean_map"].append(float(row[10]))

    best_seed_by_model = {}
    for model in by_model.keys():
        best_seed, best_map = None, -1.0
        for row in overall_rows:
            if row[1] != model:
                continue
            seed = int(row[0])
            mean_map = float(row[10])
            if mean_map > best_map:
                best_map, best_seed = mean_map, seed
        best_seed_by_model[model] = best_seed

    with open(out_across, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["model", "metric", "mean_over_seeds", "std_over_seeds", "best_seed_by_map"])
        for model in sorted(by_model.keys()):
            for metric in ["mean_top1", "mean_top5", "mean_top10", "mean_mrr", "mean_map"]:
                vals = by_model[model][metric]
                w.writerow([
                    model, metric,
                    float(np.mean(vals)),
                    float(np.std(vals, ddof=1)) if len(vals) > 1 else 0.0,
                    best_seed_by_model[model],
                ])

    # ---- Repo x BugType matrices (mean across seeds) ----
    all_bug_types = sorted({b.bug_type for b in bugs.values()})
    repos_sorted = sorted(list(repo_to_files.keys()))

    def write_repo_bugtype_matrix(model: str, per_seed_dict, out_file: str):
        accum = defaultdict(list)
        for seed in SEEDS:
            d = per_seed_dict[seed][model]
            for key, val in d.items():
                accum[key].append(val)

        with open(out_file, "w", newline="", encoding="utf-8") as f:
            w = csv.writer(f)
            w.writerow(["repo_id"] + all_bug_types)
            for repo in repos_sorted:
                row = [repo]
                for bt in all_bug_types:
                    vals = accum.get((repo, bt), [])
                    row.append(float(np.mean(vals)) if vals else "")
                w.writerow(row)

    for m in models:
        write_repo_bugtype_matrix(
            model=m,
            per_seed_dict=per_seed_repo_bugtype_map,
            out_file=os.path.join(OUT_DIR, f"results_repo_bugtype_MAP_{m.replace('+','_')}.csv"),
        )
        write_repo_bugtype_matrix(
            model=m,
            per_seed_dict=per_seed_repo_bugtype_top1,
            out_file=os.path.join(OUT_DIR, f"results_repo_bugtype_TOP1_{m.replace('+','_')}.csv"),
        )

    print("\n✅ STEP-4 COMPLETE")
    print(f"Outputs: {OUT_DIR}")
    print("Manual audit:")
    print(" - manual_validation_samples.csv")
    print(" - manual_audit_results.csv (fill by humans)")
    print("After filling audit, compute stats by running:")
    print("  run this cmd - python3 run_manual_audit_stats.py")


if __name__ == "__main__":
    main()
