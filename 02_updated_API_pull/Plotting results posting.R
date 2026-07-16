######################################################################################################
# Title: Cumulative Results Posting after Journal Publication Date
# Purpose: Generates a Kaplan-Meier style cumulative incidence figure showing the proportion
#          of RCTs with results posted on ClinicalTrials.gov after journal publication,
#          stratified by ACT status
# Date last updated: 05/21/2026
#
# Data source:
#   Posting_Update_CTG_trial_level_data.csv — updated ClinicalTrials.gov API pull conducted on 2026-04-27
######################################################################################################

library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(patchwork)

###############################
# Setup - Define paths and filenames
###############################
# UPDATE THIS PATH to the folder where you have saved the data files:
#   - Posting_Update_CTG_trial_level_data.csv
# Output figure will also be saved to this folder.
filepath     <- "C:/your/filepath/here"
compare_file <- file.path(filepath, "Posting_Update_CTG_trial_level_data.csv")

###############################
# Load data
###############################
df <- read_csv(compare_file, show_col_types = FALSE)

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

# Date of updated API pull — used as censoring date for trials without results
today <- as.Date("2026-04-27")

###############################
# Prepare data
###############################
df <- df %>%
  mutate(
    pub_date            = parse_date_col(Publication_Date_Prio_Elec),
    results_post_orig   = parse_date_col(results_first_post_date),
    results_post_update = parse_date_col(results_first_post_date_update),
    # Use updated post date if available, otherwise fall back to original
    results_post_best   = if_else(!is.na(results_post_update),
                                  results_post_update, results_post_orig),
    has_results_update  = str_to_upper(as.character(has_results_section_update)) == "TRUE",
    is_act_flag         = str_to_upper(as.character(is_act)) == "YES",
    act_label           = factor(
      if_else(is_act_flag, "ACT", "Non-ACT"),
      levels = c("ACT", "Non-ACT")
    )
  )

###############################
# Build survival dataset
# Time is days from publication to results posting
# Censored at 365 days or date of API pull if no results posted
###############################
build_surv_df <- function(data, date_var) {
  data %>%
    filter(!is.na(pub_date)) %>%
    mutate(
      results_date = .data[[date_var]],
      time = case_when(
        has_results_update & !is.na(results_date) ~
          as.numeric(results_date - pub_date),
        TRUE ~
          as.numeric(today - pub_date)
      ),
      event = if_else(has_results_update & !is.na(results_date), 1, 0),
      # Administrative censoring at 365 days post-publication
      event = if_else(time > 365, 0, event),
      time  = if_else(time > 365, 365, time)
    )
}

df_post <- build_surv_df(df, "results_post_best")

###############################
# Generate figure
###############################
plot_surv <- function(surv_df, title, output_name) {
  
  x_min_orig  <- -720
  x_max_orig  <- 365
  time_offset <- x_min_orig
  
  surv_df <- surv_df %>%
    mutate(
      time_shifted = time - time_offset,
      time_shifted = if_else(time_shifted < 0, 0.5, time_shifted)
    )
  
  fit <- survfit(Surv(time_shifted, event) ~ act_label, data = surv_df)
  
  # ── Medians ───────────────────────────────────────────────────────────────
  tbl        <- summary(fit)$table
  act_row    <- tbl[grep("Non-ACT", rownames(tbl), invert = TRUE), , drop = FALSE]
  nonact_row <- tbl[grep("Non-ACT", rownames(tbl)), , drop = FALSE]
  
  med_act_shifted    <- act_row[1, "median"]
  med_nonact_shifted <- nonact_row[1, "median"]
  
  med_act_orig    <- if (!is.na(med_act_shifted))    med_act_shifted    + time_offset else NA
  med_nonact_orig <- if (!is.na(med_nonact_shifted)) med_nonact_shifted + time_offset else NA
  
  act_label_txt    <- if (!is.na(med_act_orig))
    sprintf("ACT: median = %d days",     as.integer(med_act_orig))    else "ACT: median not reached"
  nonact_label_txt <- if (!is.na(med_nonact_orig))
    sprintf("Non-ACT: median = %d days", as.integer(med_nonact_orig)) else "Non-ACT: median not reached"
  
  x_breaks_orig    <- c(seq(x_min_orig, 180, by = 180), 365)
  x_breaks_shifted <- x_breaks_orig - time_offset
  xlim_left        <- 0
  xlim_right       <- x_max_orig - time_offset
  pub_shifted      <- 0 - time_offset
  
  # ── Main survival plot ────────────────────────────────────────────────────
  p <- ggsurvplot(
    fit,
    data             = surv_df,
    fun              = "event",
    conf.int         = TRUE,
    conf.int.alpha   = 0.1,
    palette          = c("black", "#2166ac"),
    legend.title     = "",
    legend.labs      = c("ACT", "Non-ACT"),
    xlab             = "Days from journal publication",
    ylab             = "Cumulative proportion with results posted (%)",
    title            = title,
    ylim             = c(0, 1),
    xlim             = c(xlim_left, xlim_right),
    break.x.by      = 180,
    axes.offset      = FALSE,
    risk.table       = FALSE,
    surv.median.line = "none",
    ggtheme          = theme_classic(base_size = 12) +
      theme(
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
        plot.title         = element_text(face = "bold", hjust = 0.5, size = 13),
        legend.position    = "top",
        legend.key.width   = unit(1.2, "cm"),
        axis.text          = element_text(size = 10),
        axis.title         = element_text(size = 11)
      )
  )
  
  p$plot <- p$plot +
    scale_x_continuous(
      breaks = x_breaks_shifted,
      labels = x_breaks_orig,
      expand = expansion(add = c(0, 0))
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      labels = paste0(seq(0, 100, 25), "%")
    ) +
    coord_cartesian(xlim = c(xlim_left, xlim_right), clip = "off") +
    geom_vline(xintercept = pub_shifted, linetype = "solid",
               color = "grey60", linewidth = 0.5) +
    annotate("text", x = pub_shifted + 5, y = 0.03,
             label = "Publication\ndate", hjust = 0,
             size = 2.8, color = "grey50")
  
  if (!is.na(med_act_orig)) {
    med_act_s <- med_act_orig - time_offset
    p$plot <- p$plot +
      annotate("segment", x = xlim_left, xend = med_act_s, y = 0.5, yend = 0.5,
               linetype = "dashed", color = "black", linewidth = 0.5) +
      annotate("segment", x = med_act_s, xend = med_act_s, y = 0.5, yend = 0,
               linetype = "dashed", color = "black", linewidth = 0.5)
  }
  
  if (!is.na(med_nonact_orig)) {
    med_nonact_s <- med_nonact_orig - time_offset
    p$plot <- p$plot +
      annotate("segment", x = xlim_left, xend = med_nonact_s, y = 0.5, yend = 0.5,
               linetype = "dashed", color = "#2166ac", linewidth = 0.5) +
      annotate("segment", x = med_nonact_s, xend = med_nonact_s, y = 0.5, yend = 0,
               linetype = "dashed", color = "#2166ac", linewidth = 0.5)
  }
  
  p$plot <- p$plot +
    annotate("label",
             x     = xlim_left + 250,
             y     = 0.42,
             label = paste(act_label_txt, nonact_label_txt, sep = "\n"),
             hjust = 0, size = 3.5,
             fill  = "white", color = "black")
  
  # ── Manual risk table ─────────────────────────────────────────────────────
  query_times    <- x_breaks_shifted
  query_times[1] <- 1
  
  sf <- summary(fit, times = query_times, extend = TRUE)
  
  risk_data <- tibble(
    time_shifted = rep(x_breaks_shifted, times = 2),
    strata       = as.character(sf$strata),
    n_risk       = sf$n.risk
  ) %>%
    mutate(
      group = if_else(str_detect(strata, "Non-ACT"), "Non-ACT", "ACT"),
      group = factor(group, levels = c("Non-ACT", "ACT"))
    )
  
  risk_table <- ggplot(risk_data,
                       aes(x = time_shifted, y = group,
                           label = n_risk, color = group)) +
    geom_text(size = 3.8) +
    annotate("text",
             x     = -80,
             y     = c(1, 2),
             label = c("Non-ACT", "ACT"),
             color = c("#2166ac", "black"),
             hjust = 1,
             size  = 3.8) +
    scale_color_manual(values = c("ACT" = "black", "Non-ACT" = "#2166ac")) +
    scale_x_continuous(
      breaks = x_breaks_shifted,
      labels = x_breaks_orig,
      expand = expansion(add = c(0, 0)),
      limits = c(xlim_left, xlim_right)
    ) +
    scale_y_discrete(labels = NULL) +
    coord_cartesian(xlim = c(xlim_left, xlim_right), clip = "off") +
    labs(x = "Days from journal publication", y = NULL,
         title = "Number at risk") +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x     = element_text(size = 9),
      axis.text.y     = element_blank(),
      axis.ticks.y    = element_blank(),
      axis.line.y     = element_blank(),
      legend.position = "none",
      plot.title      = element_text(face = "bold", size = 10, hjust = 0)
    )
  
  # ── Combine plot and risk table ───────────────────────────────────────────
  combined <- (p$plot    + theme(plot.margin = margin(5,  10, 0, 60))) /
    (risk_table + theme(plot.margin = margin(0, 10, 5, 60))) +
    plot_layout(heights = c(3, 1))
  
  ggsave(file.path(filepath, output_name),
         combined, width = 10, height = 7, dpi = 300)
  
  cat("Saved:", output_name, "\n")
  cat("ACT:    ", act_label_txt, "\n")
  cat("Non-ACT:", nonact_label_txt, "\n\n")
}

###############################
# Generate and save figure
###############################
plot_surv(df_post,
          "Cumulative Results Posting after Journal Publication Date",
          "Survival_results_post_date.png")
