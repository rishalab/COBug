from full_pipeline import compute_manual_audit_stats

compute_manual_audit_stats(
    "results_step4/manual_audit_results.csv",
    "results_step4/manual_audit_stats.csv",
)

print("✅ Manual audit stats written to results_step4/manual_audit_stats.csv")
