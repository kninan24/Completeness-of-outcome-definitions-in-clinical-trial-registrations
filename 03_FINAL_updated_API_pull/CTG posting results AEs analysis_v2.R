######################################################################################################
# Title: Registration and Reporting Timeliness
# Purpose: Generates a table of the number and proportion of RCTs with information posted on
#          ClinicalTrials.gov prior to journal publication and within 1 year of publication
#
# This version uses the updated ClinicalTrials.gov API pull, conducted after enough time had
# elapsed for the full 1-year-since-publication window to have occurred for all included studies.
# This ensures "within 1 year of publication" estimates are not artificially truncated by trials
# that simply hadn't reached their 1-year mark yet at the time of the original pull.
#
# Date last updated: 07/16/2026
#
# Data sources:
#   Posting_CTG_trial_level_data_v2.csv        — original ClinicalTrials.gov API pull (201 NCTs),
#                                                 used for Section 1 (posted prior to publication)
#                                                 and for sponsor/funder information
#   Posting_Update_CTG_trial_level_data_v2.csv — updated ClinicalTrials.gov API pull (201 NCTs),
#                                                 used for Section 2 (posted within 1 year) and
#                                                 for extension/certification information
######################################################################################################

library(tidyverse)
library(lubridate)
library(flextable)
library(officer)

###############################
# Setup - Define paths and filenames
###############################
filepath     <- "ENTER_FILE_PATH"
trial_file   <- file.path(filepath, "Posting_CTG_trial_level_data_v2.csv")
compare_file <- file.path(filepath, "Posting_Update_CTG_trial_level_data_v2.csv")
output_file  <- file.path(filepath, "Number_and_proportion_posted_CTG_v2.docx")

# Column holding the actual STUDY START DATE value
study_start_date_col <- "trial_start_date_clean_01"

###############################
# Load data
###############################
df_trial   <- read_csv(trial_file,   show_col_types = FALSE)
df_compare <- read_csv(compare_file, show_col_types = FALSE)

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
# Source: Posting_CTG_trial_level_data_v2.csv (original pull)
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
# Source: Posting_Update_CTG_trial_level_data_v2.csv (updated pull)
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
    is_act_flag         = str_to_upper(as.character(is_act)) == "YES",
    
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
      results_post_best <= pub_date + days(365),
    
    # Extension/certification requested
    has_extension = !is.na(disp_first_submit_date)
  )

###############################
# Section 3: Protocol/SAP posted by study start date
# (used only for footnote calculation, not shown as table rows)
###############################
df_compare <- df_compare %>%
  mutate(
    start_date_actual = parse_date_col(.data[[study_start_date_col]]),
    
    proto_by_start = !is.na(proto_date_update) & !is.na(start_date_actual) &
      proto_date_update <= start_date_actual,
    sap_by_start   = !is.na(sap_date_update) & !is.na(start_date_actual) &
      sap_date_update <= start_date_actual
  )

###############################
# Join Section 2/3 variables + funder_sponsor onto Section 1
###############################
df <- df_trial %>%
  left_join(
    df_compare %>% select(nct_id, actual_pcd_update,
                          proto_1yr_pub, sap_1yr_pub, results_1yr_pub,
                          proto_by_start, sap_by_start, has_extension),
    by = "nct_id"
  ) %>%
  mutate(
    is_nih = str_to_upper(as.character(funder_sponsor)) == "NIH"
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
  count_row("actual_pcd",          "Actual Primary Completion Date"),
  count_row("results_prior_pub",   "Adverse Events"),
  count_row("proto_prior_pub",     "Protocol"),
  count_row("sap_prior_pub",       "SAP"),
  
  make_header("Posted within 1 year of journal publication"),
  count_row("actual_pcd_update",   "Actual Primary Completion Date"),
  count_row("results_1yr_pub",     "Adverse Events"),
  count_row("proto_1yr_pub",       "Protocol"),
  count_row("sap_1yr_pub",         "SAP")
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

######################################################################################################
# FOOTNOTE CALCULATIONS
######################################################################################################

cat("\n--- ACTs without AE posted within 1 year, by extension status ---\n")

act_no_results <- df_yes %>%
  filter(!results_1yr_pub)

cat(sprintf("ACTs that did not post adverse events within 1 year of publication: %d\n",
            nrow(act_no_results)))
cat(sprintf("  Of these, with extension/cert:    %d\n",
            sum(act_no_results$has_extension, na.rm = TRUE)))
cat(sprintf("  Of these, without extension/cert: %d\n",
            sum(!act_no_results$has_extension, na.rm = TRUE)))

cat("\n--- NIH funding by ACT status ---\n")

nih_check <- df %>%
  group_by(is_act_flag) %>%
  summarise(
    n_total = n(),
    n_nih   = sum(is_nih, na.rm = TRUE),
    pct_nih = round(100 * n_nih / n_total, 1),
    .groups = "drop"
  )
print(nih_check)

cat(sprintf("\nACT NIH-funded:     %d (%.1f%%)\n",
            nih_check$n_nih[nih_check$is_act_flag == TRUE],
            nih_check$pct_nih[nih_check$is_act_flag == TRUE]))
cat(sprintf("Non-ACT NIH-funded: %d (%.1f%%)\n",
            nih_check$n_nih[nih_check$is_act_flag == FALSE],
            nih_check$pct_nih[nih_check$is_act_flag == FALSE]))

cat("\n--- Actual primary completion date confirmed ---\n")

n_actual_pcd <- sum(df$actual_pcd, na.rm = TRUE)
cat(sprintf("RCTs with confirmed Actual Primary Completion Date: %d / %d (%.1f%%)\n",
            n_actual_pcd, N, n_actual_pcd / N * 100))

cat("\n--- Protocol/SAP posted by study start date ---\n")

n_proto_by_start <- sum(df$proto_by_start, na.rm = TRUE)
n_sap_by_start   <- sum(df$sap_by_start,   na.rm = TRUE)

cat(sprintf("RCTs with protocol posted by study start date: %d\n", n_proto_by_start))
cat(sprintf("RCTs with SAP posted by study start date:      %d\n", n_sap_by_start))

cat("\n--- AE posted vs submitted-but-not-posted within 1 year (ACTs) ---\n")

n_results_1yr <- sum(df$results_1yr_pub, na.rm = TRUE)
cat(sprintf("RCTs with adverse events posted within 1 year of publication: %d\n", n_results_1yr))

if ("results_first_submit_date_update" %in% names(df_compare)) {
  
  df_compare <- df_compare %>%
    mutate(
      results_submit_date = parse_date_col(results_first_submit_date_update),
      submitted_1yr        = !is.na(results_submit_date) &
        !is.na(pub_date) &
        results_submit_date <= pub_date + days(365)
    )
  
  submitted_not_posted <- df_compare %>%
    filter(is_act_flag & submitted_1yr & !results_1yr_pub)
  
  cat(sprintf("ACTs that submitted AE within 1 year but were NOT posted within 1 year: %d\n",
              nrow(submitted_not_posted)))
  print(submitted_not_posted %>%
          select(nct_id, pub_date, results_submit_date, results_post_best, results_1yr_pub),
        n = Inf)
  
} else {
  cat("Column 'results_first_submit_date_update' not found in the updated file —\n")
  cat("cannot compute submitted-but-not-posted count. Confirm the correct column name.\n")
}