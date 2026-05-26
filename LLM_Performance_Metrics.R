######################################################################################################
#Title: Completeness of outcome definitions in clinical trial registrations: LLM Performance Metrics
#Purpose: This file includes the code used to calculate diagnostic performance metrics for LLM classification
#Date last updated: 12/15/2025
######################################################################################################

###############################
# Setup - Define paths and filenames
###############################
# UPDATE THIS PATH to the folder where you have saved the data files:
#   - Completeness_outcome_level_data.csv
# Output CSV will also be saved to this folder.
# Place all data files in the same folder before running.
filepath      <- "C:/your/filepath/here"
data_outcomes <- "Completeness_outcome_level_data.csv"

###############################
# Load packages
###############################
library(readr)
library(dplyr)
library(binom)
library(tibble)
library(stringr)
library(purrr)

###############################
# Read data
###############################
df <- read_csv(file.path(filepath, data_outcomes), show_col_types = FALSE)


###############################
# Read data
###############################
df <- read_csv(file.path(filepath, data_outcomes), show_col_types = FALSE)

# Filter out rows where GPT values are "NA" (string)
df <- df %>%
  filter(
    !(meas_rate_gpt == "NA" | 
      metric_def_gpt == "NA" | 
      method_def_gpt == "NA" | 
      cut_rate_gpt == "NA")
  )

###############################
# Define CI method and helper functions
###############################
CI_METHOD <- "exact"

binom_ci <- function(x, n, method = CI_METHOD) {
  if (is.na(n) || n == 0) return(c(NA_real_, NA_real_))
  bc <- binom.confint(x = x, n = n, method = method)
  c(bc$lower, bc$upper)
}

compute_metrics_from_table <- function(tab2x2) {
  a <- as.numeric(tab2x2[1,1])
  b <- as.numeric(tab2x2[1,2])
  c <- as.numeric(tab2x2[2,1])
  d <- as.numeric(tab2x2[2,2])
  
  sens <- ifelse((a + c) > 0, a / (a + c), NA_real_)
  spec <- ifelse((b + d) > 0, d / (b + d), NA_real_)
  ppv  <- ifelse((a + b) > 0, a / (a + b), NA_real_)
  npv  <- ifelse((c + d) > 0, d / (c + d), NA_real_)
  f1   <- ifelse((2*a + b + c) > 0, (2 * a) / (2 * a + b + c), NA_real_)
  
  sens_ci <- binom_ci(a, a + c)
  spec_ci <- binom_ci(d, b + d)
  ppv_ci  <- binom_ci(a, a + b)
  npv_ci  <- binom_ci(d, c + d)
  
  tibble(
    TP = a, FP = b, FN = c, TN = d,
    Sensitivity = sens, Sensitivity_LCL = sens_ci[1], Sensitivity_UCL = sens_ci[2],
    Specificity = spec, Specificity_LCL = spec_ci[1], Specificity_UCL = spec_ci[2],
    PPV = ppv, PPV_LCL = ppv_ci[1], PPV_UCL = ppv_ci[2],
    NPV = npv, NPV_LCL = npv_ci[1], NPV_UCL = npv_ci[2],
    F1 = f1
  )
}

compute_element <- function(data, truth_col, test_col,
                            pos_truth_vals, neg_truth_vals,
                            pos_test_vals = pos_truth_vals,
                            neg_test_vals = neg_truth_vals,
                            element_name) {
  
  truth_raw <- data[[truth_col]]
  test_raw  <- data[[test_col]]
  
  truth_bin <- ifelse(truth_raw %in% pos_truth_vals, "pos",
                      ifelse(truth_raw %in% neg_truth_vals, "neg", NA_character_))
  test_bin  <- ifelse(test_raw %in% pos_test_vals, "pos",
                      ifelse(test_raw %in% neg_test_vals, "neg", NA_character_))
  
  keep <- !is.na(truth_bin) & !is.na(test_bin)
  dropped <- sum(is.na(truth_bin) | is.na(test_bin))
  
  if (dropped > 0) {
    message(sprintf("[%s] Dropped %d row(s) not matching the specified code sets.",
                    element_name, dropped))
  }
  
  tab <- table(factor(test_bin[keep],  levels = c("pos","neg")),
               factor(truth_bin[keep], levels = c("pos","neg")))
  
  metrics <- compute_metrics_from_table(tab)
  
  metrics %>%
    mutate(Element = element_name) %>%
    select(
      Element, TP, FP, FN, TN,
      Sensitivity, Sensitivity_LCL, Sensitivity_UCL,
      Specificity, Specificity_LCL, Specificity_UCL,
      PPV, PPV_LCL, PPV_UCL,
      NPV, NPV_LCL, NPV_UCL,
      F1
    )
}

###############################
# Calculate element-level metrics
###############################

res_meas_rate <- compute_element(
  data = df,
  truth_col = "meas_rate",
  test_col  = "meas_rate_gpt",
  pos_truth_vals = c(1),
  neg_truth_vals = c(2),
  pos_test_vals  = c(1),
  neg_test_vals  = c(2),
  element_name   = "Specific measurement"
)

res_metric_def <- compute_element(
  data = df,
  truth_col = "metric_def",
  test_col  = "metric_def_gpt",
  pos_truth_vals = c(1),
  neg_truth_vals = c(2),
  pos_test_vals  = c(1),
  neg_test_vals  = c(2),
  element_name   = "Specific metric"
)

res_method_def <- compute_element(
  data = df,
  truth_col = "method_def",
  test_col  = "method_def_gpt",
  pos_truth_vals = c(1),
  neg_truth_vals = c(2),
  pos_test_vals  = c(1),
  neg_test_vals  = c(2),
  element_name   = "Variable type"
)

res_cut_rate <- compute_element(
  data = df,
  truth_col = "cut_rate",
  test_col  = "cut_rate_gpt",
  pos_truth_vals = c(1,3),
  neg_truth_vals = c(2),
  pos_test_vals  = c(1,3),
  neg_test_vals  = c(2),
  element_name   = "Cutoff"
)

summary_tbl <- bind_rows(
  res_meas_rate,
  res_metric_def,
  res_method_def,
  res_cut_rate
)

###############################
# Calculate percent agreement for each element
###############################

summary_tbl <- summary_tbl %>%
  mutate(
    Total = TP + FP + FN + TN,
    Agreements = TP + TN,
    Agreement = Agreements / Total,
    Agreement_ci = map2(Agreements, Total, ~binom_ci(.x, .y))
  ) %>%
  mutate(
    Agreement_LCL = map_dbl(Agreement_ci, ~.x[1]),
    Agreement_UCL = map_dbl(Agreement_ci, ~.x[2])
  ) %>%
  select(-Agreement_ci)

formatted_tbl <- summary_tbl %>%
  mutate(
    `% Agreement (95% CI)` = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                                     Agreement * 100, 
                                     Agreement_LCL * 100, 
                                     Agreement_UCL * 100),
    `Sensitivity (95% CI)` = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                                     Sensitivity * 100, 
                                     Sensitivity_LCL * 100, 
                                     Sensitivity_UCL * 100),
    `Specificity (95% CI)` = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                                     Specificity * 100, 
                                     Specificity_LCL * 100, 
                                     Specificity_UCL * 100),
    `Positive Predictive Value (95% CI)` = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                                                   PPV * 100, 
                                                   PPV_LCL * 100, 
                                                   PPV_UCL * 100),
    `Negative Predictive Value (95% CI)` = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                                                   NPV * 100, 
                                                   NPV_LCL * 100, 
                                                   NPV_UCL * 100),
    `F1 Score` = sprintf("%.1f%%", F1 * 100)
  ) %>%
  select(
    `Element` = Element,
    `% Agreement (95% CI)`,
    `Sensitivity (95% CI)`,
    `Specificity (95% CI)`,
    `Positive Predictive Value (95% CI)`,
    `Negative Predictive Value (95% CI)`,
    `F1 Score`
  )

###############################
# Outcome-level agreement calculation
###############################

outcome_agreement <- df %>%
  mutate(
    meas_match = case_when(
      is.na(meas_rate) | is.na(meas_rate_gpt) ~ NA,
      meas_rate == meas_rate_gpt ~ TRUE,
      TRUE ~ FALSE
    ),
    metric_match = case_when(
      is.na(metric_def) | is.na(metric_def_gpt) ~ NA,
      metric_def == metric_def_gpt ~ TRUE,
      TRUE ~ FALSE
    ),
    method_match = case_when(
      is.na(method_def) | is.na(method_def_gpt) ~ NA,
      method_def == method_def_gpt ~ TRUE,
      TRUE ~ FALSE
    ),
    cut_match = case_when(
      is.na(cut_rate) | is.na(cut_rate_gpt) ~ NA,
      cut_rate == cut_rate_gpt ~ TRUE,
      TRUE ~ FALSE
    ),
    perfect_agreement = meas_match & metric_match & method_match & cut_match
  )

complete_outcomes <- outcome_agreement %>%
  filter(!is.na(perfect_agreement))

n_perfect <- sum(complete_outcomes$perfect_agreement, na.rm = TRUE)
n_total <- nrow(complete_outcomes)

agreement_ci <- binom_ci(n_perfect, n_total)

outcome_level_metrics <- tibble(
  Element = "Outcome-level (all elements)",
  `% Agreement (95% CI)` = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                                   (n_perfect/n_total) * 100,
                                   agreement_ci[1] * 100,
                                   agreement_ci[2] * 100),
  `Sensitivity (95% CI)` = "—",
  `Specificity (95% CI)` = "—",
  `Positive Predictive Value (95% CI)` = "—",
  `Negative Predictive Value (95% CI)` = "—",
  `F1 Score` = "—"
)

###############################
# Combine and print results
###############################

formatted_tbl_with_outcome <- bind_rows(
  formatted_tbl,
  outcome_level_metrics
)

print(formatted_tbl_with_outcome, n = nrow(formatted_tbl_with_outcome))

###############################
# Save output
###############################

write_csv(formatted_tbl_with_outcome, 
          file.path(filepath, "diagnostic_metrics_summary_with_agreement.csv"))