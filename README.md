# COBug - A Bug Localization Pipeline for COBOL Software Systems

# COBOL Bug Localization Tool 🐞📍
This tool introduces a **novel bug localization pipeline** for **COBOL legacy systems**, which historically lacks such tooling. By leveraging **Gemini 2.0 Flash** (an LLM model) and **machine learning-based similarity ranking**, we aim to identify and rank the most likely buggy COBOL files in a repository based on bug descriptions.

---

## 🔬 Problem Statement

COBOL continues to power critical systems in financial, governmental, and enterprise applications. However, **bug localization** for COBOL systems has not been addressed effectively in research. This tool is the first of its kind to automatically identify buggy COBOL files given a **natural language bug report**, offering a significant improvement to the maintenance of COBOL systems.

---

# Important Links
1. [Demonstration Video](https://youtu.be/R4W6-GoUHYE)
2. [Tool Website](https://rishalab.github.io/COBug/)

## 📂 Dataset

- **X-COBOL Dataset**: [X-COBOL on HuggingFace](https://huggingface.co/datasets/Trivandrum/x-cobol)
  - A collection of real-world COBOL repositories designed for bug localization research.

---

## Pipeline 

Pipeline of COBug Tool 

![Pipeline](./pipeline.png)

---

## 🛠️ Tool Workflow

1. **User Input**
    - A COBOL repository containing `.cob`, `.cbl`, `.cobol`, or `.cpy` files.
    - A **bug description** and **bug summary** for predicting the buggy file.
    - **Gemini 2.0 Flash API keys** (explained below).

2. **Bug Report Generation**
    - **Gemini 2.0 Flash LLM** is used to generate bug reports for each COBOL file, based on the bug description and summary. This requires a valid **API key** from Google.
    - The generated reports include:
        - **Bug ID**
        - **Bug Summary**
        - **Bug Description**
        - **Bug Report Path** (for reference)
        - **Ground Truth** (the file path of the buggy file)

    These reports are saved in the directory `tool/tool_reports/` and summarized in CSV files under `tool/First csvs/`.

3. **Bug-File Pairing**
    - For each bug report, every COBOL file in the repository is paired with the report.
    - Each pairing is labeled as `1` if the file is the true buggy file, and `0` otherwise.
    - All pairings are saved in CSV format under `tool/second csvs/`.

4. **Feature Extraction**
    - **TF-IDF vectorization** is performed to convert both the bug reports and COBOL files into numerical representations.
    - A **global vocabulary** is created from all the bug reports, ensuring that vectorization is consistent across different repositories.
    - **Cosine similarity** and **revised VSM (rVSM)** are calculated between the bug descriptions and each paired COBOL file.
    
    The feature vectors and similarity scores are stored for training.

5. **Model Training & Evaluation**
    - The data is split into **training** (90%) and **testing** (10%) sets, based on **unique Bug IDs**.
    - A **Random Forest** classifier is trained to predict if a file is buggy based on the similarity features (Cosine similarity, rVSM).
    - Evaluation is done using:
        - **Top-1 / Top-5 / Top-10 Accuracy**
        - **Mean Reciprocal Rank (MRR)**
    - Evaluation results are saved in `tool/ML_RESULTS/model_metrics.txt`.

6. **User Testing**
    - Once the model is trained, users can input any **bug description** and **bug summary** for testing.
    - The model will rank all COBOL files in the repository, predicting which are most likely to be buggy.
    - The ranked predictions, along with associated scores (Top-K, MRR), are saved in `tool/outputs/predicted_ranking.csv`.

---

## 🤖 How to Get Gemini 2.0 Flash API Key

Follow these steps to obtain the **API key** for the **Gemini 2.0 Flash** model:

1. Go to [Google AI Studio - API Key Management](https://aistudio.google.com/app/apikey).
2. Sign in with your **Google Account**.
3. Click **Create API Key** and select **Gemini 2.0** as the model.
4. Copy the generated **API key** and keep it safe.

To verify that you have API access, use this Python snippet:

```python
import google.generativeai as genai
genai.configure(api_key="YOUR_API_KEY")
model = genai.GenerativeModel("models/gemini-2.0-flash")
````

---

## 🔄 Flowchart

```plaintext
   [User Input] - Repo Path, API Keys
         ↓
  [Bug Report Generation]
         ↓
  [Bug-File Pairing]
         ↓
  [Feature Extraction] - Cosine Similarity, RVSM
         ↓
  [Model Training] - 90%
         ↓
  [Model Evaluation] - 10%
         ↓
  [User Testing & Predictions]
         ↓
  [Predicted Ranking in CSV] - Top-K, MRR
         ↓
  [Tool/outputs/]

```

---

## 🧪 Usage Example

To run the tool and generate predictions:

```bash
python main.py \
  --repo_path path/to/cobol_repo \
  --bug_description "System crash when ATM withdraw fails" \
  --bug_summary "ATM withdraw bug" \
  --api_key "YOUR_GEMINI_API_KEY"
```

### Output

* **Bug reports** and metadata will be saved in `tool/tool_reports/`.
* **Prediction results** (Top-K ranking) will be saved in `tool/outputs/predicted_ranking.csv`.

---

## 📈 Output Artifacts

* **`tool/tool_reports/`**: LLM-generated bug reports (plain text)
* **`tool/First csvs/`**: CSV files of bug reports with ground truth
* **`tool/second csvs/`**: CSV files of bug-file pairs for ML training
* **`tool/testing_data/`**: Test data split
* **`tool/ML_RESULTS/`**: Trained models and evaluation metrics
* **`tool/outputs/`**: Predicted rankings of COBOL files (for user input)

---

## 📦 Installation

Clone the repository and install the dependencies:

```bash
git clone https://github.com/rishalab/COBug.git
```
run the tool 
python main.py

### Prerequisites

* Python 3.9, 3.10, or 3.11
* pip ≥ 21.0

### Setup Instructions

1. **Install Python (Recommended: 3.11)**
   Download and install Python from the [official site](https://www.python.org/downloads/release/python-3110/).
   ✅ Ensure you check **"Add Python to PATH"** during installation.

2. **Create and activate a virtual environment**

   ```bash
   py -3.11 -m venv buglocal-env
   buglocal-env\Scripts\activate  # On Windows
   ```

3. **Upgrade pip**

   ```bash
   pip install --upgrade pip
   ```

4. **Install dependencies**

   ```bash
   pip install -r requirements.txt
   ⚠️ Note: Some packages like google-generativeai require Python ≥ 3.9 and may not support Python 3.13 yet.
   ```

---

## ✅ `requirements.txt`

```txt
# Requires Python >= 3.9
google-generativeai>=0.2.0
scikit-learn>=1.1
pandas>=1.3
numpy>=1.21
tqdm
joblib
matplotlib
python-dotenv
```

---

## 📁 Tool Directory Structure

```
tool/
├── tool_reports/      # LLM-generated bug reports (per COBOL file)
├── First csvs/        # CSV files: bug reports + ground truth
├── second csvs/       # CSV files of all bug-file pairs
├── testing_data/      # Evaluation data split
├── ML_RESULTS/        # Trained models and metrics
├── outputs/           # Final predictions (Top-K ranking)
├── bug_counter.txt    # Tracks global bug IDs
```

---

## 🧠 Citation

If you use this tool in your academic research, please cite:

```bibtex
@article{yourpaper2025,
  title={COBOL Bug Localization using Generative AI and Similarity Models},
  author={Your Name et al.},
  journal={Under Submission},
  year={2025}
}
```

---

## 🙏 Acknowledgements

* **X-COBOL Dataset** - [HuggingFace X-COBOL](https://huggingface.co/datasets/Trivandrum/x-cobol)
* **Gemini 2.0 Flash** - [Google AI Studio](https://aistudio.google.com/app/apikey)
* **Scikit-learn, TfidfVectorizer, Joblib** - Libraries for ML and vectorization

---

## 🔒 Notes & Limitations

* **API key limitations**: One API key can process up to 200 files at a time. Larger repos may require multiple keys.
* **Quality of bug reports**: The accuracy of bug localization depends on the **Gemini 2.0 Flash** LLM’s output quality. Always inspect generated bug reports.
* **Precision**: Bug localization precision is limited by the available comments and context in COBOL code, and LLM model performance.

---

## 💡 Further Reading

* [Gemini 2.0 Flash Model Card](https://cloud.google.com/vertex-ai/generative-ai/docs/models/gemini/2-0-flash)
* [XMainframe Technical Report (COBOL LLM)](https://arxiv.org/pdf/2408.04660.pdf)

---

