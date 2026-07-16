# Time-lag Bias in Posting Results and Adverse Events for RCTs: A Cross-Sectional Analysis
This repository contains the data and code for a cross-sectional analysis examining the timeliness of results and adverse event reporting on ClinicalTrials.gov for a sample of published randomized controlled trials (RCTs), stratified by Applicable Clinical Trial (ACT) status. The analysis evaluates posting relative to journal publication date, primary completion date, and study start date, and examines factors associated with delayed or incomplete reporting (e.g., certification/extension status, NIH funding).
## Repository structure
This repository reflects three successive rounds of data collection and analysis, organized chronologically:
### `01_initial_upload/`
The original code package for this project, uploaded at project start. Provided for archival/transparency purposes.
### `02_updated_API_pull/`
An intermediate round of analysis using an updated ClinicalTrials.gov API pull, conducted to capture additional posting activity after the original data collection. Includes updated R scripts, source data, a variable codebook, and accompanying documentation.
### `03_FINAL_updated_API_pull/` — **This is the final version of the analysis.**
The final ClinicalTrials.gov API pull and complete analysis pipeline, conducted after sufficient time had elapsed for the full 1-year-post-publication reporting window to have occurred for all included studies. All tables, figures, and reported statistics in the associated manuscript are generated from this version. Organized into:
- **`data/`** — Final analytic datasets (original and updated
