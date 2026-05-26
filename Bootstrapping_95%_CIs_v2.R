######################################################################################################
#Title: Outcome definitions in clinical trial registrations: Bootstrap Confidence Intervals
#Purpose: Proportions with 95% Confidence Intervals of outcome-level findings for the 210 RCTs registered on ClinicalTrials.gov. This code takes 10-15 minutes to execute.
#Date last updated: 04/27/2026
######################################################################################################

###############################
# Setup - Define paths and filenames
###############################
# UPDATE THIS PATH to the folder where you have saved the data files:
#   - Completeness_outcome_level_data.csv
# Output CSVs will also be saved to this folder.
# Place all data files in the same folder before running.
filepath      <- "C:/your/filepath/here"
data_outcomes <- "Completeness_outcome_level_data.csv"

###############################
# Load packages
###############################
library(tidyverse)
library(boot)

###############################
# Read data
###############################
completeness_data <- read_csv(file.path(filepath, data_outcomes), show_col_types = FALSE)

###############################
# Prepare data with all variables
###############################
df_analysis <- completeness_data %>%
  mutate(
    # Outcome complete
    outcome_complete = if_else(
      meas_rate == 1 & 
        metric_def == 1 & 
        method_def == 1 & 
        cut_rate %in% c(1, 3),
      1, 0
    ),
    
    # Create binary versions: 1=Yes (coded as 1), 0=No (coded as 2)
    meas_rate_yes = if_else(meas_rate == 1, 1, 0),
    meas_rate_no = if_else(meas_rate == 2, 1, 0),
    metric_def_yes = if_else(metric_def == 1, 1, 0),
    metric_def_no = if_else(metric_def == 2, 1, 0),
    method_def_yes = if_else(method_def == 1, 1, 0),
    method_def_no = if_else(method_def == 2, 1, 0),
    
    # Three separate binary variables for cut_rate (all outcomes as denominator)
    cut_rate_complete = if_else(cut_rate == 1, 1, 0),
    cut_rate_not_applicable = if_else(cut_rate == 3, 1, 0),
    cut_rate_incomplete = if_else(cut_rate == 2, 1, 0),
    
    # Cut-off complete EXCLUDING Not Applicable (NA=3):
    # Among outcomes where cut_rate is 1 or 2 only, is it complete?
    # Set to NA for outcomes where cut_rate == 3 so they are excluded from bootstrap mean
    cut_rate_complete_excl_na = case_when(
      cut_rate == 1 ~ 1,
      cut_rate == 2 ~ 0,
      cut_rate == 3 ~ NA_real_   # excluded from denominator
    ),
    
    # CREATE time_desc_10 and time_desc_11
    time_desc_10 = if_else(
      time_desc_1 == 1 & 
        rowSums(select(., time_desc_2, time_desc_3, time_desc_4, time_desc_5, 
                       time_desc_6, time_desc_7, time_desc_8, time_desc_9), na.rm = TRUE) == 0,
      1, 0
    ),
    
    time_desc_11 = if_else(
      time_desc_1 == 1 & 
        rowSums(select(., time_desc_2, time_desc_3, time_desc_4, time_desc_5, 
                       time_desc_6, time_desc_7, time_desc_8, time_desc_9), na.rm = TRUE) > 0,
      1, 0
    ),
    
    # Cluster ID
    cluster_id = record_id,
    
    # Add labels for out_num_cat
    outcome_type = case_when(
      out_num_cat == 1 ~ "Primary",
      out_num_cat == 2 ~ "Secondary",
      out_num_cat == 3 ~ "Other",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(!is.na(cluster_id))

# Check is_act distribution
cat("\n=== IS_ACT DISTRIBUTION ===\n")
df_analysis %>%
  count(is_act) %>%
  print()

# Check cut_rate distribution
cat("\n=== CUT_RATE DISTRIBUTION ===\n")
df_analysis %>%
  count(cut_rate) %>%
  print()

cat(sprintf("\nCut-off excl NA: %d complete, %d incomplete, %d excluded (NA)\n",
            sum(df_analysis$cut_rate_complete_excl_na == 1, na.rm = TRUE),
            sum(df_analysis$cut_rate_complete_excl_na == 0, na.rm = TRUE),
            sum(is.na(df_analysis$cut_rate_complete_excl_na))))

###############################
# Bootstrap functions
###############################

# Cluster bootstrap function (percentile method)
# NOTE: na.rm=TRUE in mean() means NA values (cut_rate==3) are excluded from denominator
cluster_bootstrap_proportion <- function(data, cluster_var, outcome_var, n_boot = 5000) {
  
  clusters <- unique(data[[cluster_var]])
  n_clusters <- length(clusters)
  
  boot_proportions <- numeric(n_boot)
  
  for (i in 1:n_boot) {
    sampled_clusters <- sample(clusters, size = n_clusters, replace = TRUE)
    boot_data <- data %>%
      filter(.data[[cluster_var]] %in% sampled_clusters)
    boot_proportions[i] <- mean(boot_data[[outcome_var]], na.rm = TRUE)
  }
  
  point_est <- mean(data[[outcome_var]], na.rm = TRUE)
  ci_lower <- quantile(boot_proportions, 0.025, names = FALSE)
  ci_upper <- quantile(boot_proportions, 0.975, names = FALSE)
  
  return(list(
    point_estimate = point_est,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_outcomes = sum(!is.na(data[[outcome_var]])),
    n_clusters = n_clusters
  ))
}

# Function to bootstrap multiple outcomes
bootstrap_multiple_outcomes <- function(data, cluster_var, outcome_vars, n_boot = 5000) {
  
  results_list <- list()
  
  for (var in outcome_vars) {
    cat("Bootstrapping:", var, "\n")
    results_list[[var]] <- cluster_bootstrap_proportion(
      data = data,
      cluster_var = cluster_var,
      outcome_var = var,
      n_boot = n_boot
    )
  }
  
  results_df <- map_df(names(results_list), function(var) {
    res <- results_list[[var]]
    tibble(
      Variable = var,
      N_Outcomes = res$n_outcomes,
      N_Trials = res$n_clusters,
      Proportion = res$point_estimate,
      CI_Lower = res$ci_lower,
      CI_Upper = res$ci_upper
    )
  })
  
  return(results_df)
}

# Function to bootstrap group proportions
bootstrap_group_proportion <- function(data, group_var, cluster_var, group_value, n_boot = 5000) {
  
  clusters <- unique(data[[cluster_var]])
  n_clusters <- length(clusters)
  
  boot_proportions <- numeric(n_boot)
  
  for (i in 1:n_boot) {
    sampled_clusters <- sample(clusters, size = n_clusters, replace = TRUE)
    boot_data <- data %>% filter(.data[[cluster_var]] %in% sampled_clusters)
    
    n_in_group <- sum(boot_data[[group_var]] == group_value, na.rm = TRUE)
    n_total <- nrow(boot_data)
    boot_proportions[i] <- n_in_group / n_total
  }
  
  n_in_group <- sum(data[[group_var]] == group_value, na.rm = TRUE)
  n_total <- nrow(data)
  point_est <- n_in_group / n_total
  
  ci_lower <- quantile(boot_proportions, 0.025, names = FALSE)
  ci_upper <- quantile(boot_proportions, 0.975, names = FALSE)
  
  return(list(
    group_value = group_value,
    n_in_group = n_in_group,
    n_total = n_total,
    point_estimate = point_est,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

###############################
# Define outcome variables
# NOTE: cut_rate_complete_excl_na replaces cut_rate_complete for the figure
###############################

outcome_vars <- c("outcome_complete", 
                  "meas_rate_yes", "meas_rate_no",
                  "metric_def_yes", "metric_def_no",
                  "method_def_yes", "method_def_no",
                  "cut_rate_complete_excl_na",  # <-- excludes NA from denominator
                  "cut_rate_not_applicable", 
                  "cut_rate_incomplete")

###############################
# SECTION 1: COMPLETENESS COMPONENTS
###############################
cat("\n=== BOOTSTRAPPING COMPLETENESS COMPONENTS ===\n")
set.seed(123)

# Overall
overall_results <- bootstrap_multiple_outcomes(
  data = df_analysis,
  cluster_var = "cluster_id",
  outcome_vars = outcome_vars,
  n_boot = 5000
)

overall_comp <- overall_results %>%
  mutate(
    Variable = case_when(
      Variable == "outcome_complete"          ~ "Outcome Complete",
      Variable == "meas_rate_yes"             ~ "Measurement Rate - Yes",
      Variable == "meas_rate_no"              ~ "Measurement Rate - No",
      Variable == "metric_def_yes"            ~ "Metric Definition - Yes",
      Variable == "metric_def_no"             ~ "Metric Definition - No",
      Variable == "method_def_yes"            ~ "Method Definition - Yes",
      Variable == "method_def_no"             ~ "Method Definition - No",
      Variable == "cut_rate_complete_excl_na" ~ "Cut Rate - Complete (excl. Not Applicable)",
      Variable == "cut_rate_not_applicable"   ~ "Cut Rate - Not Applicable",
      Variable == "cut_rate_incomplete"       ~ "Cut Rate - Incomplete"
    ),
    Overall = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                      Proportion * 100, 
                      CI_Lower * 100, 
                      CI_Upper * 100)
  ) %>%
  select(Variable, Overall)

# Stratified by out_num_cat
df_stratified <- df_analysis %>% filter(!is.na(out_num_cat))

stratified_comp <- map_df(c(1, 2, 3), function(out_type) {
  cat("Processing out_num_cat =", out_type, "\n")
  
  type_data <- df_stratified %>% filter(out_num_cat == out_type)
  
  type_results <- bootstrap_multiple_outcomes(
    data = type_data,
    cluster_var = "cluster_id",
    outcome_vars = outcome_vars,
    n_boot = 5000
  )
  
  type_results %>%
    mutate(out_num_cat = out_type)
}) %>%
  mutate(
    Variable = case_when(
      Variable == "outcome_complete"          ~ "Outcome Complete",
      Variable == "meas_rate_yes"             ~ "Measurement Rate - Yes",
      Variable == "meas_rate_no"             ~ "Measurement Rate - No",
      Variable == "metric_def_yes"            ~ "Metric Definition - Yes",
      Variable == "metric_def_no"            ~ "Metric Definition - No",
      Variable == "method_def_yes"            ~ "Method Definition - Yes",
      Variable == "method_def_no"            ~ "Method Definition - No",
      Variable == "cut_rate_complete_excl_na" ~ "Cut Rate - Complete (excl. Not Applicable)",
      Variable == "cut_rate_not_applicable"   ~ "Cut Rate - Not Applicable",
      Variable == "cut_rate_incomplete"       ~ "Cut Rate - Incomplete"
    ),
    outcome_type_label = case_when(
      out_num_cat == 1 ~ "Primary",
      out_num_cat == 2 ~ "Secondary",
      out_num_cat == 3 ~ "Other"
    ),
    result = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                     Proportion * 100, 
                     CI_Lower * 100, 
                     CI_Upper * 100)
  ) %>%
  select(Variable, outcome_type_label, result) %>%
  pivot_wider(names_from = outcome_type_label, values_from = result)

# Stratified by is_act
df_act <- df_analysis %>% filter(!is.na(is_act))

stratified_comp_act <- map_df(c("Yes", "No"), function(act_status) {
  cat("Processing is_act =", act_status, "\n")
  
  type_data <- df_act %>% filter(is_act == act_status)
  
  type_results <- bootstrap_multiple_outcomes(
    data = type_data,
    cluster_var = "cluster_id",
    outcome_vars = outcome_vars,
    n_boot = 5000
  )
  
  type_results %>%
    mutate(is_act = act_status)
}) %>%
  mutate(
    Variable = case_when(
      Variable == "outcome_complete"          ~ "Outcome Complete",
      Variable == "meas_rate_yes"             ~ "Measurement Rate - Yes",
      Variable == "meas_rate_no"             ~ "Measurement Rate - No",
      Variable == "metric_def_yes"            ~ "Metric Definition - Yes",
      Variable == "metric_def_no"            ~ "Metric Definition - No",
      Variable == "method_def_yes"            ~ "Method Definition - Yes",
      Variable == "method_def_no"            ~ "Method Definition - No",
      Variable == "cut_rate_complete_excl_na" ~ "Cut Rate - Complete (excl. Not Applicable)",
      Variable == "cut_rate_not_applicable"   ~ "Cut Rate - Not Applicable",
      Variable == "cut_rate_incomplete"       ~ "Cut Rate - Incomplete"
    ),
    act_label = case_when(
      is_act == "Yes" ~ "ACT",
      is_act == "No" ~ "Non-ACT"
    ),
    result = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                     Proportion * 100, 
                     CI_Lower * 100, 
                     CI_Upper * 100)
  ) %>%
  select(Variable, act_label, result) %>%
  pivot_wider(names_from = act_label, values_from = result)

# Combine all
section1_table <- overall_comp %>%
  left_join(stratified_comp, by = "Variable") %>%
  left_join(stratified_comp_act, by = "Variable")

###############################
# SECTION 2: TIME_PRIO DISTRIBUTION
###############################
cat("\n\n=== BOOTSTRAPPING TIME_PRIO DISTRIBUTION ===\n")
set.seed(123)

# Overall
time_prio_2_overall <- bootstrap_group_proportion(
  data = df_analysis,
  group_var = "time_prio",
  cluster_var = "cluster_id",
  group_value = 2,
  n_boot = 5000
)

time_prio_3_overall <- bootstrap_group_proportion(
  data = df_analysis,
  group_var = "time_prio",
  cluster_var = "cluster_id",
  group_value = 3,
  n_boot = 5000
)

overall_time <- tibble(
  Variable = c("Prospective Registration", "Retrospective Registration"),
  Overall = c(
    sprintf("%.1f%% (%.1f%%-%.1f%%)", 
            time_prio_2_overall$point_estimate * 100,
            time_prio_2_overall$ci_lower * 100,
            time_prio_2_overall$ci_upper * 100),
    sprintf("%.1f%% (%.1f%%-%.1f%%)", 
            time_prio_3_overall$point_estimate * 100,
            time_prio_3_overall$ci_lower * 100,
            time_prio_3_overall$ci_upper * 100)
  )
)

# Stratified by out_num_cat
stratified_time <- map_df(c(1, 2, 3), function(out_type) {
  cat("Processing out_num_cat =", out_type, "\n")
  
  type_data <- df_stratified %>% filter(out_num_cat == out_type)
  
  time_prio_2 <- bootstrap_group_proportion(
    data = type_data,
    group_var = "time_prio",
    cluster_var = "cluster_id",
    group_value = 2,
    n_boot = 5000
  )
  
  time_prio_3 <- bootstrap_group_proportion(
    data = type_data,
    group_var = "time_prio",
    cluster_var = "cluster_id",
    group_value = 3,
    n_boot = 5000
  )
  
  tibble(
    out_num_cat = out_type,
    Variable = c("Prospective Registration", "Retrospective Registration"),
    Proportion = c(time_prio_2$point_estimate, time_prio_3$point_estimate),
    CI_Lower = c(time_prio_2$ci_lower, time_prio_3$ci_lower),
    CI_Upper = c(time_prio_2$ci_upper, time_prio_3$ci_upper)
  )
}) %>%
  mutate(
    outcome_type_label = case_when(
      out_num_cat == 1 ~ "Primary",
      out_num_cat == 2 ~ "Secondary",
      out_num_cat == 3 ~ "Other"
    ),
    result = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                     Proportion * 100,
                     CI_Lower * 100,
                     CI_Upper * 100)
  ) %>%
  select(Variable, outcome_type_label, result) %>%
  pivot_wider(names_from = outcome_type_label, values_from = result)

# Stratified by is_act
stratified_time_act <- map_df(c("Yes", "No"), function(act_status) {
  cat("Processing is_act =", act_status, "\n")
  
  type_data <- df_act %>% filter(is_act == act_status)
  
  time_prio_2 <- bootstrap_group_proportion(
    data = type_data,
    group_var = "time_prio",
    cluster_var = "cluster_id",
    group_value = 2,
    n_boot = 5000
  )
  
  time_prio_3 <- bootstrap_group_proportion(
    data = type_data,
    group_var = "time_prio",
    cluster_var = "cluster_id",
    group_value = 3,
    n_boot = 5000
  )
  
  tibble(
    is_act = act_status,
    Variable = c("Prospective Registration", "Retrospective Registration"),
    Proportion = c(time_prio_2$point_estimate, time_prio_3$point_estimate),
    CI_Lower = c(time_prio_2$ci_lower, time_prio_3$ci_lower),
    CI_Upper = c(time_prio_2$ci_upper, time_prio_3$ci_upper)
  )
}) %>%
  mutate(
    act_label = case_when(
      is_act == "Yes" ~ "ACT",
      is_act == "No" ~ "Non-ACT"
    ),
    result = sprintf("%.1f%% (%.1f%%-%.1f%%)", 
                     Proportion * 100,
                     CI_Lower * 100,
                     CI_Upper * 100)
  ) %>%
  select(Variable, act_label, result) %>%
  pivot_wider(names_from = act_label, values_from = result)

# Combine
section2_table <- overall_time %>%
  left_join(stratified_time, by = "Variable") %>%
  left_join(stratified_time_act, by = "Variable")

###############################
# SECTION 3: TIMEPOINT DESCRIPTIONS
###############################
cat("\n\n=== BOOTSTRAPPING TIMEPOINT DESCRIPTIONS ===\n")

time_desc_vars <- c(paste0("time_desc_", 2:9), "time_desc_10", "time_desc_11")

# Overall
n_total_outcomes <- nrow(df_analysis)
time_desc_results_list <- list()

cat("\nBootstrapping timepoint descriptions (overall)...\n")
set.seed(123)

for (td_var in time_desc_vars) {
  cat("  ", td_var, "\n")
  
  n_count <- sum(df_analysis[[td_var]], na.rm = TRUE)
  pct_of_outcomes <- (n_count / n_total_outcomes) * 100
  
  n_boot <- 5000
  boot_pcts <- numeric(n_boot)
  
  clusters <- unique(df_analysis$cluster_id)
  n_clusters <- length(clusters)
  
  for (i in 1:n_boot) {
    sampled_clusters <- sample(clusters, size = n_clusters, replace = TRUE)
    boot_data <- df_analysis %>% filter(cluster_id %in% sampled_clusters)
    
    boot_count <- sum(boot_data[[td_var]], na.rm = TRUE)
    boot_total <- nrow(boot_data)
    boot_pcts[i] <- (boot_count / boot_total) * 100
  }
  
  ci_lower <- quantile(boot_pcts, 0.025, names = FALSE)
  ci_upper <- quantile(boot_pcts, 0.975, names = FALSE)
  
  time_desc_results_list[[td_var]] <- list(
    timepoint = td_var,
    count = n_count,
    percent = pct_of_outcomes,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

overall_timepoint <- map_df(time_desc_results_list, function(res) {
  tibble(
    Variable = res$timepoint,
    Count = res$count,
    Percent = res$percent,
    CI_Lower = res$ci_lower,
    CI_Upper = res$ci_upper
  )
}) %>%
  mutate(
    Variable = case_when(
      Variable == "time_desc_2"  ~ "Single discrete numeric time point excluding baseline",
      Variable == "time_desc_3"  ~ "Multiple discrete numeric time points excluding baseline",
      Variable == "time_desc_4"  ~ "Interval/frequency",
      Variable == "time_desc_5"  ~ "Event",
      Variable == "time_desc_6"  ~ "Estimated time",
      Variable == "time_desc_7"  ~ "Unclear",
      Variable == "time_desc_8"  ~ "Non-Follow-Up information",
      Variable == "time_desc_9"  ~ "Not reported",
      Variable == "time_desc_10" ~ "Baseline only",
      Variable == "time_desc_11" ~ "Baseline and other timepoints",
      TRUE ~ Variable
    ),
    Overall = sprintf("%.1f%% (%.1f%%-%.1f%%)", Percent, CI_Lower, CI_Upper)
  ) %>%
  select(Variable, Overall)

# Stratified by out_num_cat
cat("\nBootstrapping timepoint descriptions (stratified by out_num_cat)...\n")
set.seed(123)

stratified_timepoint <- map_df(c(1, 2, 3), function(out_type) {
  cat("Processing out_num_cat =", out_type, "\n")
  
  type_data <- df_stratified %>% filter(out_num_cat == out_type)
  n_total_type <- nrow(type_data)
  
  time_desc_results <- map_df(time_desc_vars, function(td_var) {
    cat("  ", td_var, "\n")
    
    n_count <- sum(type_data[[td_var]], na.rm = TRUE)
    pct_of_outcomes <- (n_count / n_total_type) * 100
    
    n_boot <- 5000
    boot_pcts <- numeric(n_boot)
    
    clusters <- unique(type_data$cluster_id)
    n_clusters <- length(clusters)
    
    for (i in 1:n_boot) {
      sampled_clusters <- sample(clusters, size = n_clusters, replace = TRUE)
      boot_data <- type_data %>% filter(cluster_id %in% sampled_clusters)
      
      boot_count <- sum(boot_data[[td_var]], na.rm = TRUE)
      boot_total <- nrow(boot_data)
      boot_pcts[i] <- (boot_count / boot_total) * 100
    }
    
    ci_lower <- quantile(boot_pcts, 0.025, names = FALSE)
    ci_upper <- quantile(boot_pcts, 0.975, names = FALSE)
    
    tibble(Timepoint = td_var, Percent = pct_of_outcomes,
           CI_Lower = ci_lower, CI_Upper = ci_upper)
  })
  
  time_desc_results %>% mutate(out_num_cat = out_type)
}) %>%
  mutate(
    Variable = case_when(
      Timepoint == "time_desc_2"  ~ "Single discrete numeric time point excluding baseline",
      Timepoint == "time_desc_3"  ~ "Multiple discrete numeric time points excluding baseline",
      Timepoint == "time_desc_4"  ~ "Interval/frequency",
      Timepoint == "time_desc_5"  ~ "Event",
      Timepoint == "time_desc_6"  ~ "Estimated time",
      Timepoint == "time_desc_7"  ~ "Unclear",
      Timepoint == "time_desc_8"  ~ "Non-Follow-Up information",
      Timepoint == "time_desc_9"  ~ "Not reported",
      Timepoint == "time_desc_10" ~ "Baseline only",
      Timepoint == "time_desc_11" ~ "Baseline and other timepoints"
    ),
    outcome_type_label = case_when(
      out_num_cat == 1 ~ "Primary",
      out_num_cat == 2 ~ "Secondary",
      out_num_cat == 3 ~ "Other"
    ),
    result = sprintf("%.1f%% (%.1f%%-%.1f%%)", Percent, CI_Lower, CI_Upper)
  ) %>%
  select(Variable, outcome_type_label, result) %>%
  pivot_wider(names_from = outcome_type_label, values_from = result)

# Stratified by is_act
cat("\nBootstrapping timepoint descriptions (stratified by is_act)...\n")
set.seed(123)

stratified_timepoint_act <- map_df(c("Yes", "No"), function(act_status) {
  cat("Processing is_act =", act_status, "\n")
  
  type_data <- df_act %>% filter(is_act == act_status)
  n_total_type <- nrow(type_data)
  
  time_desc_results <- map_df(time_desc_vars, function(td_var) {
    cat("  ", td_var, "\n")
    
    n_count <- sum(type_data[[td_var]], na.rm = TRUE)
    pct_of_outcomes <- (n_count / n_total_type) * 100
    
    n_boot <- 5000
    boot_pcts <- numeric(n_boot)
    
    clusters <- unique(type_data$cluster_id)
    n_clusters <- length(clusters)
    
    for (i in 1:n_boot) {
      sampled_clusters <- sample(clusters, size = n_clusters, replace = TRUE)
      boot_data <- type_data %>% filter(cluster_id %in% sampled_clusters)
      
      boot_count <- sum(boot_data[[td_var]], na.rm = TRUE)
      boot_total <- nrow(boot_data)
      boot_pcts[i] <- (boot_count / boot_total) * 100
    }
    
    ci_lower <- quantile(boot_pcts, 0.025, names = FALSE)
    ci_upper <- quantile(boot_pcts, 0.975, names = FALSE)
    
    tibble(Timepoint = td_var, Percent = pct_of_outcomes,
           CI_Lower = ci_lower, CI_Upper = ci_upper)
  })
  
  time_desc_results %>% mutate(is_act = act_status)
}) %>%
  mutate(
    Variable = case_when(
      Timepoint == "time_desc_2"  ~ "Single discrete numeric time point excluding baseline",
      Timepoint == "time_desc_3"  ~ "Multiple discrete numeric time points excluding baseline",
      Timepoint == "time_desc_4"  ~ "Interval/frequency",
      Timepoint == "time_desc_5"  ~ "Event",
      Timepoint == "time_desc_6"  ~ "Estimated time",
      Timepoint == "time_desc_7"  ~ "Unclear",
      Timepoint == "time_desc_8"  ~ "Non-Follow-Up information",
      Timepoint == "time_desc_9"  ~ "Not reported",
      Timepoint == "time_desc_10" ~ "Baseline only",
      Timepoint == "time_desc_11" ~ "Baseline and other timepoints"
    ),
    act_label = case_when(
      is_act == "Yes" ~ "ACT",
      is_act == "No" ~ "Non-ACT"
    ),
    result = sprintf("%.1f%% (%.1f%%-%.1f%%)", Percent, CI_Lower, CI_Upper)
  ) %>%
  select(Variable, act_label, result) %>%
  pivot_wider(names_from = act_label, values_from = result)

# Combine
section3_table <- overall_timepoint %>%
  left_join(stratified_timepoint, by = "Variable") %>%
  left_join(stratified_timepoint_act, by = "Variable")

###############################
# SAVE OUTPUT 1: Overall only
###############################
cat("\n\n=== SAVING OVERALL RESULTS ===\n")

section1_overall <- overall_comp %>%
  mutate(`Proportion (95% CI)` = Overall) %>%
  select(Variable, `Proportion (95% CI)`)

section2_overall <- overall_time %>%
  mutate(`Proportion (95% CI)` = Overall) %>%
  select(Variable, `Proportion (95% CI)`)

section3_overall <- overall_timepoint %>%
  mutate(`Proportion (95% CI)` = Overall) %>%
  select(Variable, `Proportion (95% CI)`)

header1 <- tibble(Variable = "COMPLETENESS COMPONENTS", `Proportion (95% CI)` = "")
header2 <- tibble(Variable = "TIME PRIORITY",           `Proportion (95% CI)` = "")
header3 <- tibble(Variable = "TIMEPOINT DESCRIPTIONS",  `Proportion (95% CI)` = "")
blank   <- tibble(Variable = "",                         `Proportion (95% CI)` = "")

combined_table_overall <- bind_rows(
  header1, section1_overall, blank,
  header2, section2_overall, blank,
  header3, section3_overall
)

write_csv(combined_table_overall, file.path(filepath, "bootstrap_results_overall_v2.csv"))
cat("Overall table saved: bootstrap_results_overall_v2.csv\n")

###############################
# SAVE OUTPUT 2: Complete stratification
###############################
cat("\n\n=== SAVING COMPLETE STRATIFICATION RESULTS ===\n")

header1_strat <- tibble(Variable = "COMPLETENESS COMPONENTS",
                        Overall = "", Primary = "", Secondary = "", Other = "", ACT = "", `Non-ACT` = "")
header2_strat <- tibble(Variable = "TIME PRIORITY",
                        Overall = "", Primary = "", Secondary = "", Other = "", ACT = "", `Non-ACT` = "")
header3_strat <- tibble(Variable = "TIMEPOINT DESCRIPTIONS",
                        Overall = "", Primary = "", Secondary = "", Other = "", ACT = "", `Non-ACT` = "")
blank_strat   <- tibble(Variable = "",
                        Overall = "", Primary = "", Secondary = "", Other = "", ACT = "", `Non-ACT` = "")

combined_table_stratified <- bind_rows(
  header1_strat, section1_table, blank_strat,
  header2_strat, section2_table, blank_strat,
  header3_strat, section3_table
)

write_csv(combined_table_stratified,
          file.path(filepath, "bootstrap_results_complete_stratification_v2.csv"))

cat("Stratified table saved: bootstrap_results_complete_stratification_v2.csv\n")
cat("\nColumns in the stratified output:\n")
cat("  1. Variable\n")
cat("  2. Overall\n")
cat("  3. Primary (out_num_cat=1)\n")
cat("  4. Secondary (out_num_cat=2)\n")
cat("  5. Other (out_num_cat=3)\n")
cat("  6. ACT (is_act=Yes)\n")
cat("  7. Non-ACT (is_act=No)\n")