import os
import re
import csv
import glob
import shutil
import string
import joblib
import google.generativeai as genai
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

# Config and folder paths
TOOL_DIR = 'tool'
REPORTS_DIR = os.path.join(TOOL_DIR, 'tool_reports')
FIRST_CSVS_DIR = os.path.join(TOOL_DIR, 'First csvs')
SECOND_CSVS_DIR = os.path.join(TOOL_DIR, 'second csvs')
TESTING_DATA_DIR = os.path.join(TOOL_DIR, 'testing_data')
OUTPUT_DIR = os.path.join(TOOL_DIR, 'outputs')
ML_RESULTS_DIR = os.path.join(TOOL_DIR, 'ML_RESULTS')
BUG_COUNTER_FILE = os.path.join(TOOL_DIR, 'bug_counter.txt')
MAX_CALLS_PER_KEY = 200
MODEL_NAME = 'models/gemini-2.0-flash'


def reset_tool_folders():
    if os.path.exists(TOOL_DIR):
        shutil.rmtree(TOOL_DIR)
    for path in [REPORTS_DIR, FIRST_CSVS_DIR, SECOND_CSVS_DIR, TESTING_DATA_DIR, OUTPUT_DIR, ML_RESULTS_DIR]:
        os.makedirs(path, exist_ok=True)
    with open(BUG_COUNTER_FILE, "w") as f:
        f.write("1")


def is_cobol_file(fname):
    return fname.lower().endswith(('.cob', '.cbl', '.cpy','.cobol'))


def preprocess(text):
    if not isinstance(text, str): return ''
    text = text.lower()
    text = re.sub(r"[^\x00-\x7F]+", " ", text)
    text = re.sub(r"\s+", " ", text)
    text = re.sub(f"[{re.escape(string.punctuation)}]", " ", text)
    return text.strip()


def compute_cosine(text1, text2, vectorizer):
    v1 = vectorizer.transform([text1])
    v2 = vectorizer.transform([text2])
    return cosine_similarity(v1, v2)[0][0]


def compute_rvsm(Summary, desc, code, vectorizer):
    return 0.6 * compute_cosine(Summary, code, vectorizer) + 0.4 * compute_cosine(desc, code, vectorizer)


def read_bug_counter():
    if not os.path.exists(BUG_COUNTER_FILE):
        with open(BUG_COUNTER_FILE, 'w') as f:
            f.write("1")
        return 1
    with open(BUG_COUNTER_FILE, 'r') as f:
        return int(f.read().strip())


def write_bug_counter(val):
    with open(BUG_COUNTER_FILE, 'w') as f:
        f.write(str(val))


def generate_bug_report(code, api_key):
    try:
        genai.configure(api_key=api_key)
        model = genai.GenerativeModel(model_name=MODEL_NAME)
        chat = model.start_chat()
        chat.send_message(
            "Analyze the following COBOL code and generate a detailed bug report. "
            "The report must follow this plain-text format (no markdown, no bold, no file names):\n"
            "Bug ID:\n"
            "Summary:(Bug Summary - concise explanation in 1 line)\n"
            "Description: (Bug-description-full explanation of the bug)\n\n"
            "Do not quote or mention the COBOL file name. Do not use file-based prefixes like DEC-001. "
            "Avoid quoting or pasting code directly. Describe bugs clearly, concisely, and in plain English."
        )
        response = chat.send_message(f"COBOL CODE:\n\n{code}")
        return response.text.strip()
    except Exception as e:
        if "quota" in str(e).lower() or "429" in str(e):
            raise RuntimeError("API_KEY_EXHAUSTED")
        raise e


def extract_bug_blocks(text):
    blocks = re.split(r"Bug ID:\s*", text)[1:]
    out = []
    for b in blocks:
        Summary_match = re.search(r"Summary:\s*(.*)", b)
        desc_match = re.search(r"Description:\s*(.*)", b, re.DOTALL)
        if Summary_match and desc_match:
            out.append({"Summary": Summary_match.group(1).strip(), "Description": desc_match.group(1).strip()})
    return out


def create_first_csv(rows, repo_name):
    path = os.path.join(FIRST_CSVS_DIR, f"{repo_name}.csv")
    with open(path, "w", newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=rows[0].keys())
        writer.writeheader()
        writer.writerows(rows)
    return path


def create_second_csv():
    for csv_file in glob.glob(os.path.join(FIRST_CSVS_DIR, "*.csv")):
        rows = list(csv.DictReader(open(csv_file)))
        all_cbls = set(r['Ground Truth'] for r in rows)
        paired = []
        for r in rows:
            for cbl in all_cbls:
                paired.append({**r, "CBL Path": cbl, "Label": 1 if r["Ground Truth"] == cbl else 0})
        out_path = os.path.join(SECOND_CSVS_DIR, os.path.basename(csv_file))
        with open(out_path, "w", newline='') as f:
            writer = csv.DictWriter(f, fieldnames=paired[0].keys())
            writer.writeheader()
            writer.writerows(paired)


def train_model_and_rank():
    file = os.path.join(SECOND_CSVS_DIR, os.listdir(SECOND_CSVS_DIR)[0])
    df = pd.read_csv(file)
    df['Summary'] = df['Summary'].apply(preprocess)
    df['Description'] = df['Description'].apply(preprocess)

    bug_ids = df['BUG_ID'].unique()
    test_ids = np.random.choice(bug_ids, size=max(1, int(0.1 * len(bug_ids))), replace=False)

    train_df = df[~df['BUG_ID'].isin(test_ids)]
    test_df = df[df['BUG_ID'].isin(test_ids)]
    test_df.to_csv(os.path.join(TESTING_DATA_DIR, "test_data.csv"), index=False)

    vectorizer = TfidfVectorizer()
    vectorizer.fit(train_df['Summary'] + ' ' + train_df['Description'])

    def create_features(data):
        feats = []
        for _, r in data.iterrows():
            try:
                with open(r['CBL Path'], 'r', encoding='utf-8', errors='ignore') as cfile:
                    code = cfile.read()
            except:
                code = ""
            combo = f"{r['Summary']} {r['Description']}"
            cos = compute_cosine(combo, code, vectorizer)
            rvsm = compute_rvsm(r['Summary'], r['Description'], code, vectorizer)
            tfidf = vectorizer.transform([combo]).toarray()[0]
            feats.append(np.concatenate((tfidf, [cos, rvsm])))
        return np.array(feats)

    X_train = create_features(train_df)
    y_train = train_df['Label'].values
    X_test = create_features(test_df)
    y_test = test_df['Label'].values

    model = RandomForestClassifier(n_estimators=100, random_state=42)
    model.fit(X_train, y_train)

    joblib.dump(model, os.path.join(ML_RESULTS_DIR, "rf_model.pkl"))
    joblib.dump(vectorizer, os.path.join(ML_RESULTS_DIR, "tfidf_vectorizer.pkl"))

    # Ranking evaluation
    test_df = test_df.copy()
    test_df['Score'] = model.predict_proba(X_test)[:, 1]

    top1 = top5 = top10 = mrr_sum = count = 0

    for _, group in test_df.groupby('BUG_ID'):
        labels = group['Label'].values
        scores = group['Score'].values
        if 1 not in labels:
            continue
        correct_idx = np.where(labels == 1)[0][0]
        sorted_idx = np.argsort(scores)[::-1]
        rank = np.where(sorted_idx == correct_idx)[0][0] + 1
        mrr_sum += 1 / rank
        if rank == 1: top1 += 1
        if rank <= 5: top5 += 1
        if rank <= 10: top10 += 1
        count += 1

    # Save metrics
    with open(os.path.join(ML_RESULTS_DIR, "model_metrics.txt"), "w") as f:
        f.write(f"Top-1: {top1/count:.4f}\n")
        f.write(f"Top-5: {top5/count:.4f}\n")
        f.write(f"Top-10: {top10/count:.4f}\n")
        f.write(f"MRR: {mrr_sum/count:.4f}\n")

    print(f"Ranking evaluation done. MRR: {mrr_sum/count:.4f}")


def predict_user_input(cbl_files):
    print("=== Input bug details for prediction ===")
    Summary = input("Bug Summary: ").strip()
    desc = input("Bug Description: ").strip()
    ground_truth = input("Ground truth CBL path (optional): ").strip()

    vectorizer = joblib.load(os.path.join(ML_RESULTS_DIR, "tfidf_vectorizer.pkl"))
    model = joblib.load(os.path.join(ML_RESULTS_DIR, "rf_model.pkl"))
    combined = preprocess(f"{Summary} {desc}")

    ranked_preds = []
    for cbf in cbl_files:
        try:
            with open(cbf, 'r', encoding='utf-8', errors='ignore') as f:
                code = f.read()
        except:
            code = ""
        code = preprocess(code)
        cos = compute_cosine(combined, code, vectorizer)
        rvsm = compute_rvsm(preprocess(Summary), preprocess(desc), code, vectorizer)
        vec = vectorizer.transform([combined]).toarray()[0]
        features = np.concatenate((vec, [cos, rvsm]))
        score = model.predict_proba([features])[0][1]
        ranked_preds.append((cbf, score))

    ranked_preds.sort(key=lambda x: x[1], reverse=True)

    output_csv_path = os.path.join(OUTPUT_DIR, "predicted_ranking.csv")
    found_gt = False
    gt_rank = None

    with open(output_csv_path, "w", newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(["Rank", "CBL File", "Relevance Score", "Ground Truth Match", "Top-K", "MRR"])
        for i, (path, score) in enumerate(ranked_preds, 1):
            match = "Yes" if os.path.normpath(path) == os.path.normpath(ground_truth) else "No"
            if match == "Yes" and not found_gt:
                found_gt, gt_rank = True, i
            topk = "Top-1" if i == 1 else "Top-5" if i <= 5 else "Top-10" if i <= 10 else "No"
            mrr_val = f"{1/i:.4f}" if match == "Yes" else ""
            writer.writerow([i, path, f"{score:.4f}", match, topk, mrr_val])

    # Print some info
    print("\nTop 5 predictions:")
    for i, (path, score) in enumerate(ranked_preds[:5], 1):
        print(f"{i}. {path} — Score: {score:.4f}")

    if found_gt:
        print(f"\n✅ Ground truth found at rank {gt_rank}, MRR: {1/gt_rank:.4f}")

    print(f"\nSaved full ranking to '{output_csv_path}'.")


def main():
    reset_tool_folders()
    repo = input("Enter path to COBOL repo: ").strip()
    cbl_files = [f for f in glob.glob(os.path.join(repo, "*")) if is_cobol_file(f)]
    if not cbl_files:
        print("No COBOL files found, aborting.")
        return

    print(f"Found {len(cbl_files)} COBOL files.")

    print(f"API keys needed for processing")
    api_keys = input("Enter API keys (comma separated): ").split(',')
    api_usage = [0] * len(api_keys)
    api_idx = 0

    repo_name = os.path.basename(repo.rstrip('/\\'))
    bug_counter = read_bug_counter()
    rows = []
    processed_basename = set(os.path.basename(f) for f in glob.glob(os.path.join(REPORTS_DIR, "bug_report_*.txt")))

    for path in cbl_files:
        base = os.path.basename(path)
        if base in processed_basename:
            print(f"Skipping already processed file: {base}")
            continue
        while True:
            if api_idx >= len(api_keys):
                print("Out of API keys. Please add more or exit.")
                new_keys = input("Add new API keys (comma-separated), or press Enter to stop: ").strip()
                if not new_keys:
                    print("No new keys provided. Exiting.")
                    return
                new_keys_list = [k.strip() for k in new_keys.split(",") if k.strip()]
                api_keys.extend(new_keys_list)
                api_usage.extend([0]*len(new_keys_list))
                print(f"Added {len(new_keys_list)} new API keys. Continuing...")
                continue
            try:
                with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                    code = f.read()
                print(f"Generating bug report for {base} using key #{api_idx+1}...")
                report = generate_bug_report(code, api_keys[api_idx])
                rf_path = os.path.join(REPORTS_DIR, f"bug_report_{bug_counter}.txt")
                with open(rf_path, "w", encoding='utf-8') as rf:
                    rf.write(report)
                bugs = extract_bug_blocks(report)
                for bug in bugs:
                    rows.append({
                        "BUG_ID": bug_counter,
                        "Summary": bug["Summary"],
                        "Description": bug["Description"],
                        "Report Path": rf_path,
                        "Ground Truth": path
                    })
                    bug_counter += 1
                api_usage[api_idx] += 1
                if api_usage[api_idx] >= MAX_CALLS_PER_KEY:
                    api_idx += 1
                write_bug_counter(bug_counter)
                break
            except RuntimeError as e:
                if str(e) == "API_KEY_EXHAUSTED":
                    print(f"API key #{api_idx+1} exhausted.")
                    api_idx += 1
                    continue
                else:
                    print(f"Error processing {base}: {e}")
                    break

    if not rows:
        print("No bug reports generated.")
        return

    create_first_csv(rows, repo_name)
    create_second_csv()
    train_model_and_rank()
    predict_user_input(cbl_files)


if __name__ == "__main__":
    main()
