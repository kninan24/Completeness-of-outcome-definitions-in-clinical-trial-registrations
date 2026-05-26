######################################################################################################
# Title: Registration and Reporting Timeliness
# Purpose: Generates a table of the number and proportion of RCTs with information posted on
#          ClinicalTrials.gov prior to journal publication and within 1 year of publication
# Date last updated: 05/21/2026
#
# Data sources:
#   Posting_CTG_trial_level_data.csv        — original ClinicalTrials.gov API pull, conducted close to
#                                             the time of journal publication; used for Section 1
#                                             (posted prior to journal publication)
#   Posting_Update_CTG_trial_level_data.csv — updated ClinicalTrials.gov API pull, conducted approximately
#                                             10 months after the last journal publication in the sample;
#                                             used for Section 2 (posted within 1 year of publication)
######################################################################################################

library(tidyverse)
library(lubridate)
library(flextable)
library(officer)

###############################
# Setup - Define paths and filenames
###############################
# UPDATE THESE PATHS to the folder where you have saved the data files:
#   - Posting_CTG_trial_level_data.csv
#   - Posting_Update_CTG_trial_level_data.csv
# Output file will also be saved to this folder.
filepath     <- "C:/your/filepath/here"
compare_file <- file.path(filepath, "Posting_Update_CTG_trial_level_data.csv")
trial_file   <- file.path(filepath, "Posting_CTG_trial_level_data.csv")
output_file  <- file.path(filepath, "Number_and_proportion_posted_CTG.docx")

###############################
# Load data
###############################
df_compare <- read_csv(compare_file, show_col_types = FALSE)
df_trial   <- read_csv(trial_file,   show_col_types = FALSE)

###############################
# Helper functions
###############################

# Parse date columns, replacing placeholder date 3011-01-01 with NA
placeholder <- as.Date("3011-01-01")

parse_date_col <- function(x) {
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  d <- if_else(is.na(d), suppressWarnings(as.Date(x, format = "%m/%d/%Y")), d)
  if_else(!is.na(d) & d == placeholder, as.Date(NA), d)
}

# Format n (%) for table cells
fmt_pct <- function(n, denom) sprintf("%d (%.1f%%)", n, n / denom * 100)

###############################
# Section 1: Posted prior to journal publication
# Source: Posting_CTG_trial_level_data.csv (original API pull, done close to journal publication)
###############################
df_trial <- df_trial %>%
  mutate(
    pub_date     = parse_date_col(Publication_Date_Prio_Elec),
    pcd_orig     = parse_date_col(primary_completion_date_clean_01),
    proto_date   = parse_date_col(Earliest_Document_Date_Protocol),
    sap_date     = parse_date_col(Earliest_Document_Date_SAP),
    results_post = parse_date_col(results_first_post_date),
    
    has_results  = str_to_upper(as.character(has_results_section)) == "TRUE",
    is_act_flag  = str_to_upper(as.character(is_act)) == "YES",
    
    # Actual study start date posted
    actual_start = start_date_type == "ACTUAL",
    
    # Actual primary completion date posted (before or after publication)
    actual_pcd = case_when(
      primary_completion_date_type == "ACTUAL" & pcd_orig <  pub_date ~ TRUE,
      primary_completion_date_type == "ACTUAL" & pcd_orig >= pub_date ~ TRUE,
      TRUE                                                             ~ FALSE
    ),
    
    # Protocol and SAP uploaded prior to publication
    proto_prior_pub = !is.na(proto_date) & proto_date < pub_date,
    sap_prior_pub   = !is.na(sap_date)   & sap_date   < pub_date,
    
    # Results posted prior to publication
    results_prior_pub = has_results & !is.na(results_post) &
      results_post < pub_date
  )

###############################
# Section 2: Posted within 1 year of journal publication
# Source: Posting_Update_CTG_trial_level_data.csv (updated API pull)
###############################
df_compare <- df_compare %>%
  mutate(
    pub_date          = parse_date_col(Publication_Date_Prio_Elec),
    
    proto_date_update = parse_date_col(earliest_protocol_date_update),
    sap_date_update   = parse_date_col(earliest_sap_date_update),
    
    results_post_orig   = parse_date_col(results_first_post_date),
    results_post_update = parse_date_col(results_first_post_date_update),
    # Use updated post date if available, otherwise fall back to original
    results_post_best   = if_else(!is.na(results_post_update),
                                  results_post_update, results_post_orig),
    
    has_results_update = str_to_upper(as.character(has_results_section_update)) == "TRUE",
    
    # Actual study start date from updated API pull
    actual_start_update = start_date_type_update == "ACTUAL",
    
    # Actual primary completion date from updated API pull
    actual_pcd_update = primary_completion_date_type_update == "ACTUAL",
    
    # Protocol and SAP uploaded within 1 year of publication
    proto_1yr_pub = !is.na(proto_date_update) & !is.na(pub_date) &
      proto_date_update <= pub_date + days(365),
    sap_1yr_pub   = !is.na(sap_date_update)   & !is.na(pub_date) &
      sap_date_update   <= pub_date + days(365),
    
    # Results posted within 1 year of publication
    results_1yr_pub = has_results_update & !is.na(results_post_best) &
      !is.na(pub_date) &
      results_post_best <= pub_date + days(365)
  )

###############################
# Join section 2 variables onto section 1
###############################
df <- df_trial %>%
  left_join(
    df_compare %>% select(nct_id, actual_start_update, actual_pcd_update,
                          proto_1yr_pub, sap_1yr_pub, results_1yr_pub),
    by = "nct_id"
  )

###############################
# Subsets by ACT status
###############################
df_yes <- df %>% filter(is_act_flag)
df_no  <- df %>% filter(!is_act_flag)
N      <- nrow(df)
n_yes  <- nrow(df_yes)
n_no   <- nrow(df_no)

###############################
# Table construction
###############################
count_row <- function(var, label) {
  tibble(
    Variable = label,
    All      = fmt_pct(sum(df[[var]],     na.rm = TRUE), N),
    ACT_Yes  = fmt_pct(sum(df_yes[[var]], na.rm = TRUE), n_yes),
    ACT_No   = fmt_pct(sum(df_no[[var]],  na.rm = TRUE), n_no)
  )
}

make_header <- function(h) tibble(Variable = h, All = "", ACT_Yes = "", ACT_No = "")

table_out <- bind_rows(
  make_header("Posted prior to journal publication"),
  count_row("actual_start",        "Actual Study Start Date"),
  count_row("actual_pcd",          "Actual Primary Completion Date"),
  count_row("proto_prior_pub",     "Protocol Uploaded"),
  count_row("sap_prior_pub",       "SAP Uploaded"),
  count_row("results_prior_pub",   "Results and Adverse Events Posted"),
  
  make_header("Posted within 1 year of journal publication"),
  count_row("actual_start_update", "Actual Study Start Date"),
  count_row("actual_pcd_update",   "Actual Primary Completion Date"),
  count_row("results_1yr_pub",     "Results and Adverse Events Posted"),
  count_row("proto_1yr_pub",       "Protocol Uploaded"),
  count_row("sap_1yr_pub",         "SAP Uploaded")
)

cat("\n=== TABLE: REGISTRATION AND REPORTING TIMELINESS ===\n")
print(table_out, n = Inf)

###############################
# Format and save to Word
###############################
border_outer <- fp_border(color = "black", width = 1)
border_inner <- fp_border(color = "grey80", width = 0.5)

make_ft <- function(data, title) {
  flextable(data) %>%
    set_header_labels(
      Variable = "Variable",
      All      = sprintf("All RCTs\n(N=%d)", N),
      ACT_Yes  = sprintf("ACT Yes\n(N=%d)", n_yes),
      ACT_No   = sprintf("ACT No\n(N=%d)",  n_no)
    ) %>%
    add_header_lines(title) %>%
    bold(part = "header") %>%
    bold(i = ~ All == "", part = "body") %>%
    fontsize(size = 10, part = "all") %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    autofit() %>%
    border_outer(part = "all", border = border_outer) %>%
    border_inner_h(part = "body", border = border_inner) %>%
    align(j = c("All", "ACT_Yes", "ACT_No"), align = "center", part = "all") %>%
    bg(part = "header", bg = "#D9E1F2") %>%
    bg(i = ~ All == "", bg = "#F2F2F2", part = "body")
}

ft <- make_ft(table_out,
              "Number and proportion of RCTs with information posted on ClinicalTrials.gov prior to journal publication and within 1 year of publication")

doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = output_file)
cat("Saved:", output_file, "\n")

###############################
# Primary completion date reached by time of publication (actual or estimated)
###############################
n_pcd   <- sum(!is.na(df_trial$pcd_orig) & df_trial$pcd_orig <= df_trial$pub_date, na.rm = TRUE)
n_total <- nrow(df_trial)
cat(sprintf("\nBy the time of publication, %d of %d (%.1f%%) RCTs had reached their primary completion date, whether actual or estimated.\n",
            n_pcd, n_total, n_pcd / n_total * 100))

did_not_reach_pcd <- df_trial %>%
  filter(is.na(pcd_orig) | pcd_orig > pub_date) %>%
  select(nct_id, PMID, journal_book, is_act, pub_date, pcd_orig)

cat(sprintf("\nTrials that had not reached primary completion date by publication: %d\n",
            nrow(did_not_reach_pcd)))

write_csv(did_not_reach_pcd,
          file.path(filepath, "Did_not_reach_PCD_by_publication.csv"))
cat("Saved: Did_not_reach_PCD_by_publication.csv\n")