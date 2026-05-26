######################################################################################################
# Title: Completeness Forest Plots — 3-Panel Publication Figure
# Purpose: This file outputs the outcome completeness figure stratified by subgroups
# Date: 04/27/2026
######################################################################################################
library(tidyverse)
library(patchwork)

###############################
# Setup - Define paths and filenames
###############################
# UPDATE THIS PATH to the folder where you have saved the data files:
#   - bootstrap_results_complete_stratification_v2.csv
#   - bootstrap_results_sponsor_v2.csv
# Ensure you run the bootstrap code first to generate these data files.
# Output figures will also be saved to this folder.
filepath    <- "C:/your/filepath/here"
csv_strat   <- file.path(filepath, "bootstrap_results_complete_stratification_v2.csv")
csv_sponsor <- file.path(filepath, "bootstrap_results_sponsor_v2.csv")
boot_strat   <- read_csv(csv_strat,   show_col_types = FALSE)
boot_sponsor <- read_csv(csv_sponsor, show_col_types = FALSE)

# ── Okabe-Ito colorblind-safe palette ─────────────────────────────────────────
oi_black  <- "#000000"
oi_blue   <- "#0072B2"
oi_red    <- "#D55E00"
oi_green  <- "#009E73"

# ── Parse helper ──────────────────────────────────────────────────────────────
parse_ci_str <- function(x) {
  if (is.na(x) || x == "" || x == "N/A") return(c(NA_real_, NA_real_, NA_real_))
  nums <- as.numeric(str_extract_all(x, "[0-9]+\\.?[0-9]*")[[1]])
  if (length(nums) < 3) return(c(nums[1], NA_real_, NA_real_))
  c(nums[1], nums[2], nums[3])
}

extract_panel <- function(df, var_labels, col_names, col_display = col_names) {
  vars_ordered <- c(
    "Outcome Complete",
    "Measurement Rate - Yes",
    "Metric Definition - Yes",
    "Method Definition - Yes",
    "Cut Rate - Complete (excl. Not Applicable)"
  )
  map_df(var_labels, function(vl) {
    row <- df %>% filter(Variable == vl) %>% slice(1)
    map_df(seq_along(col_names), function(i) {
      cn  <- col_names[i]
      val <- if (cn %in% names(row)) pull(row, cn) else NA_character_
      p   <- parse_ci_str(val)
      tibble(element = vl, subgroup = col_display[i],
             pct = p[1], lo = p[2], hi = p[3])
    })
  }) %>%
    mutate(element = factor(element, levels = rev(vars_ordered)))
}

var_labels <- c(
  "Outcome Complete",
  "Measurement Rate - Yes",
  "Metric Definition - Yes",
  "Method Definition - Yes",
  "Cut Rate - Complete (excl. Not Applicable)"
)

element_display <- c(
  "Outcome Complete"                           = "All elements",
  "Measurement Rate - Yes"                     = "Specific\nMeasurement",
  "Metric Definition - Yes"                    = "Specific\nMetric",
  "Method Definition - Yes"                    = "Variable\nType",
  "Cut Rate - Complete (excl. Not Applicable)" = "Cutoff"
)
el_levels <- rev(unname(element_display))

add_element_lab <- function(df) {
  df %>% mutate(element_lab = factor(
    element_display[as.character(element)], levels = el_levels
  ))
}

x_scale <- scale_x_continuous(
  limits = c(0, 130), breaks = seq(0, 100, 25),
  labels = function(x) ifelse(x <= 100, paste0(x, "%"), ""),
  expand = expansion(mult = c(0, 0))
)

base_theme <- theme_classic(base_size = 11) +
  theme(
    panel.grid.major.x = element_line(color = "gray88", linewidth = 0.4),
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 10),
    plot.title         = element_text(face = "bold", size = 11, hjust = 0.5),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.5, "cm"),
    axis.title.x       = element_text(size = 10),
    plot.margin        = margin(t = 8, r = 5, b = 8, l = 5)
  )

dodge_w   <- 0.6
text_size <- 3.0

# ── Panel A: Outcome Priority ─────────────────────────────────────────────────
pri_display <- c("Primary (n=312)", "Secondary (n=1,925)", "Other (n=306)")
pri_colors  <- c("Primary (n=312)"     = oi_black,
                 "Secondary (n=1,925)" = oi_blue,
                 "Other (n=306)"       = oi_red)
pri_shapes  <- c("Primary (n=312)"     = 16,
                 "Secondary (n=1,925)" = 15,
                 "Other (n=306)"       = 17)

df_priority <- extract_panel(boot_strat, var_labels,
                             c("Primary", "Secondary", "Other"),
                             pri_display) %>%
  add_element_lab() %>%
  mutate(subgroup = factor(subgroup, levels = rev(pri_display)))

pA <- ggplot(df_priority, aes(x = pct, y = element_lab,
                              color = subgroup, shape = subgroup, group = subgroup)) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.7,
                 position = position_dodge(width = dodge_w),
                 show.legend = FALSE) +
  geom_point(size = 3, position = position_dodge(width = dodge_w)) +
  geom_text(aes(x = 101, label = sprintf("%.1f%% [%.1f–%.1f]", pct, lo, hi)),
            hjust = 0, size = text_size,
            position = position_dodge(width = dodge_w),
            show.legend = FALSE) +
  coord_cartesian(clip = "off") +
  x_scale +
  scale_color_manual(values = pri_colors, name = NULL) +
  scale_shape_manual(values = pri_shapes, name = NULL) +
  labs(x = NULL, y = NULL, title = "A. Outcome Priority") +
  base_theme +
  guides(color = guide_legend(reverse = TRUE),
         shape = guide_legend(reverse = TRUE))

# ── Panel B: ACT Status ───────────────────────────────────────────────────────
act_display <- c("ACT (n=1,031)", "Non-ACT (n=1,512)")
act_colors  <- c("ACT (n=1,031)"     = oi_black,
                 "Non-ACT (n=1,512)" = oi_blue)
act_shapes  <- c("ACT (n=1,031)"     = 16,
                 "Non-ACT (n=1,512)" = 15)

df_act <- extract_panel(boot_strat, var_labels,
                        c("ACT", "Non-ACT"), act_display) %>%
  add_element_lab() %>%
  mutate(subgroup = factor(subgroup, levels = rev(act_display)))

pB <- ggplot(df_act, aes(x = pct, y = element_lab,
                         color = subgroup, shape = subgroup, group = subgroup)) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.7,
                 position = position_dodge(width = dodge_w),
                 show.legend = FALSE) +
  geom_point(size = 3, position = position_dodge(width = dodge_w)) +
  geom_text(aes(x = 101, label = sprintf("%.1f%% [%.1f–%.1f]", pct, lo, hi)),
            hjust = 0, size = text_size,
            position = position_dodge(width = dodge_w),
            show.legend = FALSE) +
  coord_cartesian(clip = "off") +
  x_scale +
  scale_color_manual(values = act_colors, name = NULL) +
  scale_shape_manual(values = act_shapes, name = NULL) +
  labs(x = NULL, y = NULL, title = "B. Applicable Clinical Trial Status") +
  base_theme +
  guides(color = guide_legend(reverse = TRUE),
         shape = guide_legend(reverse = TRUE))

# ── Panel C: Lead Sponsor ─────────────────────────────────────────────────────
sp_cols    <- c("INDUSTRY",          "NIH",         "OTHER",           "OTHER_GOV")
sp_display <- c("Industry (n=969)",  "NIH (n=342)", "Other (n=1,216)", "Other Gov (n=16)")
sp_colors  <- c("Industry (n=969)"  = oi_black,
                "NIH (n=342)"       = oi_blue,
                "Other (n=1,216)"   = oi_red,
                "Other Gov (n=16)"  = oi_green)
sp_shapes  <- c("Industry (n=969)"  = 16,
                "NIH (n=342)"       = 15,
                "Other (n=1,216)"   = 17,
                "Other Gov (n=16)"  = 18)

df_sp <- extract_panel(boot_sponsor, var_labels, sp_cols, sp_display) %>%
  add_element_lab() %>%
  mutate(subgroup = factor(subgroup, levels = rev(sp_display)))

pC <- ggplot(df_sp, aes(x = pct, y = element_lab,
                        color = subgroup, shape = subgroup, group = subgroup)) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.7,
                 position = position_dodge(width = 0.7),
                 na.rm = TRUE, show.legend = FALSE) +
  geom_point(size = 3, position = position_dodge(width = 0.7),
             na.rm = TRUE) +
  geom_text(aes(x = 101, label = ifelse(is.na(pct), "",
                                        sprintf("%.1f%% [%.1f–%.1f]", pct, lo, hi))),
            hjust = 0, size = text_size,
            position = position_dodge(width = 0.7),
            na.rm = TRUE, show.legend = FALSE) +
  coord_cartesian(clip = "off") +
  x_scale +
  scale_color_manual(values = sp_colors, name = NULL) +
  scale_shape_manual(values = sp_shapes, name = NULL) +
  labs(x = "% defined completely", y = NULL, title = "C. Lead Sponsor") +
  base_theme +
  guides(color = guide_legend(reverse = TRUE),
         shape = guide_legend(reverse = TRUE))

# ── Combine with patchwork ────────────────────────────────────────────────────
combined <- (pA / pB / pC) +
  plot_layout(heights = c(1.6, 1.4, 1.8)) &
  theme(plot.margin = margin(t = 6, r = 140, b = 6, l = 5))

ggsave(file.path(filepath, "Figure1_combined.tiff"),
       combined,
       width       = 8.5,
       height      = 11,
       dpi         = 300,
       compression = "lzw")

ggsave(file.path(filepath, "Figure1_combined.png"),
       combined,
       width  = 8.5,
       height = 11,
       dpi    = 300)

cat("Saved: Figure1_combined.tiff and .png\n")