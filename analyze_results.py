# ==========================================================
# STEP-5: STATISTICAL ANALYSIS & OBSERVATIONS
# ----------------------------------------------------------
# Inputs (from Step-4):
#   - results_step4/results_overall.csv
#   - results_step4/results_per_repo.csv
#   - results_step4/wilcoxon.csv
#
# Outputs:
#   - analysis/best_model_summary.csv
#   - analysis/repo_win_counts.csv
#   - analysis/model_rankings.csv
#   - analysis/final_observations.txt
# ==========================================================

import os
import csv
from collections import defaultdict
import numpy as np

RESULTS_DIR = "results_codellama"
OUT_DIR = "analysis_codellama"

OVERALL_CSV = os.path.join(RESULTS_DIR, "results_overall.csv")
PER_REPO_CSV = os.path.join(RESULTS_DIR, "results_per_repo.csv")
WILCOXON_CSV = os.path.join(RESULTS_DIR, "wilcoxon.csv")

PRIMARY_METRIC = "mean_map"
ALPHA = 0.05


def ensure_dir(p):
    os.makedirs(p, exist_ok=True)


# ----------------------------------------------------------
# Loaders
# ----------------------------------------------------------
def load_overall():
    rows = []
    with open(OVERALL_CSV, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            rows.append(row)
    return rows


def load_per_repo():
    rows = []
    with open(PER_REPO_CSV, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            rows.append(row)
    return rows


def load_wilcoxon():
    rows = []
    with open(WILCOXON_CSV, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            rows.append(row)
    return rows


# ----------------------------------------------------------
# Core analysis
# ----------------------------------------------------------
def main():
    ensure_dir(OUT_DIR)

    overall = load_overall()
    per_repo = load_per_repo()
    wilcoxon = load_wilcoxon()

    models = sorted({row["model"] for row in overall})
    seeds = sorted({row["seed"] for row in overall})

    # ------------------------------------------------------
    # 1) Mean MAP per model across seeds
    # ------------------------------------------------------
    map_by_model = defaultdict(list)
    for row in overall:
        map_by_model[row["model"]].append(float(row["mean_map"]))

    model_stats = []
    for m, vals in map_by_model.items():
        model_stats.append({
            "model": m,
            "mean_map": np.mean(vals),
            "std_map": np.std(vals, ddof=1) if len(vals) > 1 else 0.0
        })

    model_stats = sorted(model_stats, key=lambda x: -x["mean_map"])

    # ------------------------------------------------------
    # 2) Repo-wise wins (who ranks GT at top more often)
    # ------------------------------------------------------
    repo_group = defaultdict(list)
    for row in per_repo:
        key = (row["seed"], row["test_repo"])
        repo_group[key].append(row)

    win_counts = defaultdict(int)
    for _, rows in repo_group.items():
        best = max(rows, key=lambda r: float(r["map"]))
        win_counts[best["model"]] += 1

    # ------------------------------------------------------
    # 3) Wilcoxon decisions
    # ------------------------------------------------------
    wilcoxon_decisions = []
    for row in wilcoxon:
        if row["metric"] != "MAP":
            continue
        if not row["p_value"]:
            continue

        p = float(row["p_value"])
        wilcoxon_decisions.append({
            "seed": row["seed"],
            "model_a": row["model_a"],
            "model_b": row["model_b"],
            "p_value": p,
            "significant": p < ALPHA
        })

    # ------------------------------------------------------
    # Write CSVs
    # ------------------------------------------------------
    with open(os.path.join(OUT_DIR, "model_rankings.csv"), "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["model", "mean_map", "std_map"])
        for r in model_stats:
            w.writerow([r["model"], r["mean_map"], r["std_map"]])

    with open(os.path.join(OUT_DIR, "repo_win_counts.csv"), "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["model", "repo_wins"])
        for m in models:
            w.writerow([m, win_counts.get(m, 0)])

    with open(os.path.join(OUT_DIR, "best_model_summary.csv"), "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["criterion", "value"])
        w.writerow(["best_model_by_mean_map", model_stats[0]["model"]])
        w.writerow(["best_mean_map", model_stats[0]["mean_map"]])
        w.writerow(["std_map", model_stats[0]["std_map"]])

    # ------------------------------------------------------
    # Final textual observations (paper-ready)
    # ------------------------------------------------------
    best_model = model_stats[0]["model"]

    with open(os.path.join(OUT_DIR, "final_observations.txt"), "w", encoding="utf-8") as f:
        f.write("FINAL OBSERVATIONS (STEP-5)\n")
        f.write("=====================================\n\n")

        f.write(f"Best performing model: {best_model}\n\n")

        f.write("Mean MAP across seeds:\n")
        for r in model_stats:
            f.write(f"  {r['model']}: {r['mean_map']:.4f} ± {r['std_map']:.4f}\n")

        f.write("\nRepo-wise win counts:\n")
        for m in models:
            f.write(f"  {m}: {win_counts.get(m, 0)} wins\n")

        f.write("\nWilcoxon signed-rank test (MAP):\n")
        for wlx in wilcoxon_decisions:
            f.write(
                f"  Seed {wlx['seed']}: {wlx['model_a']} vs {wlx['model_b']} "
                f"| p={wlx['p_value']:.4f} "
                f"| significant={wlx['significant']}\n"
            )

    print("✅ STEP-5 ANALYSIS COMPLETE")
    print(f"Outputs written to: {OUT_DIR}")


if __name__ == "__main__":
    main()
