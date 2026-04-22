# ==============================================================
# Fairness Attitudes — Proportionality & Reciprocity
# Four-region Canadian survey: Alberta, Ontario, Quebec, Atlantic Canada
#
# DVs are coded 0, 0.33, 0.66, 1  (four ordered response levels)
#   → Primary model:    ordered logit (polr), AME on P(highest category = 1)
#   → Robustness check: OLS on raw scale, HC1 robust SEs
#
# Sections:
#   0.  Setup: packages, paths, output folders
#   1.  Variable construction
#   2.  DV definitions (by type) and IV list
#   3.  Helper functions
#   4.  Descriptive plots — distribution by region
#   5.  Ordered logit — fit models & AMEs
#   6.  OLS — robustness check
#   7.  Coefficient plots
#   8.  Regression tables
#   9.  Diagnostics
# ==============================================================


# ==============================================================
# 0.  SETUP
# ==============================================================

# ── 0.1  Packages ─────────────────────────────────────────────
library(tidyverse)
library(ggplot2)
library(marginaleffects)
library(modelsummary)
library(MASS)          # polr()
library(sandwich)
library(lmtest)
library(RColorBrewer)

# Resolve namespace conflicts introduced by MASS
select <- dplyr::select
filter <- dplyr::filter

# ── 0.2  Global parameters ────────────────────────────────────
params <- list(
  dpi         = 300,
  plot_width  = 14,
  plot_height = 10,
  out_desc    = "code/source_redistribution/graphs/descriptives_fairness",
  out_reg     = "code/source_redistribution/graphs/regressions_fairness",
  data_path   = "data/clean_df_valid.csv"
)

# ── 0.3  Output folders ───────────────────────────────────────
dir.create(params$out_desc, recursive = TRUE, showWarnings = FALSE)
dir.create(params$out_reg,  recursive = TRUE, showWarnings = FALSE)

# ── 0.4  Load data ────────────────────────────────────────────
df <- read.csv(params$data_path)


# ==============================================================
# 1.  VARIABLE CONSTRUCTION
# ==============================================================

# ── 1.1  Derived binary variables ─────────────────────────────
df$univ_educ_bin <- as.integer(df$educ_group == "educUniv")

if (!"incomeHigh_bin" %in% names(df)) {
  df$incomeHigh_bin <- as.integer(df$ses_income3Cat == "High")
}

# ── 1.2  Factor ordering ──────────────────────────────────────
df$educ_group     <- factor(df$educ_group,
                            levels = c("educBHS", "educHS", "educUniv"))
df$ses_region_cat <- factor(
  dplyr::recode(df$ses_region_cat, "East Coast" = "Atlantic Canada"),
  levels = c("Ontario", "Quebec", "Alberta", "Atlantic Canada")
)
df$ses_income3Cat <- factor(df$ses_income3Cat,
                            levels = c("Low", "Mid", "High"))


# ==============================================================
# 2.  DV DEFINITIONS AND IV LIST
# ==============================================================

# ── 2.1  Proportionality DVs ──────────────────────────────────
#   Coded 0 / 0.33 / 0.66 / 1; highest value (1) = endorsing
#   the "fair" outcome.
prop_vars <- c(
  "redis_opportunity_num",
  "redis_intelligence_num",
  "redis_effort_num",
  "redis_reasons_poor_num",
  "redis_reasons_rich_num"
)

prop_labels <- c(
  redis_opportunity_num   = "Equal opportunity",
  redis_intelligence_num  = "Rewarded for effort & skill",
  redis_effort_num        = "Fairness of income distribution",
  redis_reasons_poor_num  = "Violating outcomes (Poor)",
  redis_reasons_rich_num  = "Violating outcomes (Rich)"
)

# ── 2.2  Reciprocity DVs ──────────────────────────────────────
recip_vars <- c(
  "redis_social_benefits_num",
  "redis_welfare_num",
  "redis_no_cheat_system_num"
)

recip_labels <- c(
  redis_social_benefits_num = "Social benefits not a choice",
  redis_welfare_num         = "Welfare goes to undeserving",
  redis_no_cheat_system_num = "Trust not to cheat system"
)

# ── 2.3  Combined vectors for loops ───────────────────────────
all_dv_vars   <- c(prop_vars, recip_vars)
all_dv_labels <- c(prop_labels, recip_labels)

# Canonical display order for all faceted plots (proportionality first)
dv_order <- unname(all_dv_labels)

# ── 2.4  Ordered-factor versions of DVs (required by polr) ────
#   Levels are sorted numerically so 0 < 0.33 < 0.66 < 1 is
#   preserved as the ordinal structure.
for (v in all_dv_vars) {
  observed_levels <- sort(unique(na.omit(df[[v]])))
  df[[paste0(v, "_ord")]] <- factor(
    as.character(df[[v]]),
    levels  = as.character(observed_levels),
    ordered = TRUE
  )
}

dv_ord_vars <- paste0(all_dv_vars, "_ord")

# ── 2.5  IV list ──────────────────────────────────────────────
#   Ontario is the omitted region reference category.
ivs <- list(
  # ── Socio-economic ────────────────────────────────────────────
  list(type = "numeric", var = "incomeHigh_bin",             low = 0,  high = 1,  label = "Income (High vs low/mid)"),
  list(type = "binary",  var = "ses_male_bin",               low = 0, high = 1, label = "Gender (Female=0 vs Male=1)"),
  list(type = "numeric", var = "ses_age",                    low = 30, high = 60, label = "Age (30 vs 60)"),
  list(type = "numeric", var = "univ_educ_bin",              low = 0,  high = 1,  label = "Education (University vs below)"),
  list(type = "binary",  var = "employ_fulltime_bin",        low = 0, high = 1, label = "Employed Full-Time (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ses_citizenYes_bin",         low = 0, high = 1, label = "Citizen (No=0 vs Yes=1)"),
  # ── Ideology & political interest ─────────────────────────────
  list(type = "numeric", var = "ideo_right_num",             low = 0,  high = 1,  label = "Ideology: Left (0) vs Right (1)"),
  list(type = "numeric", var = "ideo_interest_politics_num", low = 0,  high = 1,  label = "Political Interest: Low (0) vs High (1)"),
  # ── Region & identity ─────────────────────────────────────────
  list(type = "binary",  var = "ideo_define_QC_first_bin",   low = 0, high = 1, label = "Quebecker First (No=0 vs Yes=1)"),
  list(type = "binary",  var = "quebec_bin",                 low = 0, high = 1, label = "Quebec (No=0 vs Yes=1)"),
  list(type = "binary",  var = "alberta_bin",                low = 0, high = 1, label = "Alberta (No=0 vs Yes=1)"),
  list(type = "binary",  var = "region_eastcoast_bin",       low = 0, high = 1, label = "Atlantic Canada (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ses_french_bin",             low = 0, high = 1, label = "French-speaking (No=0 vs Yes=1)"),
  # ── Institutional / social trust ──────────────────────────────
  list(type = "binary",  var = "trust_social_bin",           low = 0, high = 1, label = "Social Trust (Low=0 vs High=1)")
)

# ── 2.6  RHS formula string ───────────────────────────────────
#   Ontario is the omitted region; no vote variables in this project.
rhs <- "incomeHigh_bin + ses_male_bin + ses_age + univ_educ_bin +
        employ_fulltime_bin + ses_citizenYes_bin +
        ideo_right_num + ideo_interest_politics_num +
        ideo_define_QC_first_bin + quebec_bin + alberta_bin + region_eastcoast_bin +
        ses_french_bin +
        trust_social_bin"

# ── 2.6b  RHS without regional variables (robustness check) ──
#   Used to verify that individual-level results are stable when
#   regional variables are excluded. Presented as a secondary
#   specification given null regional effects in the main models.
rhs_no_region <- "incomeHigh_bin + ses_male_bin + ses_age + univ_educ_bin +
                  employ_fulltime_bin + ses_citizenYes_bin +
                  ideo_right_num + ideo_interest_politics_num +
                  trust_social_bin"

# ── 2.7  Human-readable term labels ───────────────────────────
term_labels <- c(
  # Socio-economic
  "incomeHigh_bin"             = "Income: High vs Low/Mid",
  "ses_male_bin"               = "Male",
  "ses_age"                    = "Age",
  "univ_educ_bin"              = "Education: Univ vs Below",
  "employ_fulltime_bin"        = "Employed Full-Time",
  "ses_citizenYes_bin"         = "Citizen",
  # Ideology & political interest
  "ideo_right_num"             = "Ideology (Right)",
  "ideo_interest_politics_num" = "Political Interest",
  # Region & identity
  "ideo_define_QC_first_bin"   = "Quebecker First",
  "quebec_bin"                 = "Quebec",
  "alberta_bin"                = "Alberta",
  "region_eastcoast_bin"       = "Atlantic Canada",
  "ses_french_bin"             = "French-speaking",
  # Institutional / social trust
  "trust_social_bin"           = "Social Trust"
)


# Consistent region colour palette across all plots
region_colours <- c(
  "Ontario"    = "#d6604d",
  "Quebec"     = "#2166ac",
  "Alberta"    = "#4dac26",
  "Atlantic Canada" = "#984ea3"
)

# Human-readable labels for the four response levels
level_labels <- c(
  "0"    = "0 (Unfair)",
  "0.33" = "0.33",
  "0.66" = "0.66",
  "1"    = "1 (Fair)"
)


# ==============================================================
# 3.  HELPER FUNCTIONS
# ==============================================================

# ── 3.1  HC1 robust variance-covariance ───────────────────────
robust_vcov <- function(model) vcovHC(model, type = "HC1")


# ── 3.2  Ordered logit AME extractor ─────────────────────────
#   Re-fits polr() on complete cases for numerical stability,
#   computes HC1 avg_slopes(), and returns a list:
#     $top  — AME on the highest response category (= 1) only
#     $full — AME across all four response categories
tidy_polr_slopes <- function(model, dv_label, data = df) {
  fml        <- formula(model)
  model_vars <- all.vars(fml)
  model_data <- data |> dplyr::select(all_of(model_vars)) |> drop_na()
  model_fit  <- MASS::polr(fml, data = model_data, Hess = TRUE)
  
  slopes_all <- avg_slopes(model_fit, vcov = "HC1", newdata = model_data) |>
    as_tibble() |>
    mutate(dv = dv_label)
  
  # Identify the top factor level by sorting character representations
  top_level <- slopes_all |>
    pull(group) |> as.character() |> unique() |> sort() |> tail(1)
  
  slopes_top <- slopes_all |>
    dplyr::filter(as.character(group) == top_level) |>
    transmute(
      dv,
      term,
      outcome_level = top_level,
      estimate  = round(estimate,  3),
      conf.low  = round(conf.low,  3),
      conf.high = round(conf.high, 3),
      p.value   = round(p.value,   3),
      sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".", TRUE ~ ""
      ),
      direction = case_when(
        conf.low  > 0 ~ "Positive",
        conf.high < 0 ~ "Negative",
        TRUE          ~ "No clear effect"
      )
    )
  
  list(top = slopes_top, full = slopes_all)
}


# ── 3.3  OLS AME extractor (robustness check) ─────────────────
#   Fits OLS on the raw 0/0.33/0.66/1 scale; HC1 robust SEs.
extract_ame_ols <- function(dv, dv_label, rhs, data = df) {
  model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
  model_data <- data |> dplyr::select(all_of(model_vars)) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", rhs)), data = model_data)
  vm         <- robust_vcov(model)
  
  avg_slopes(model, vcov = vm, newdata = model_data) |>
    as_tibble() |>
    transmute(
      dv        = dv_label,
      term,
      estimate  = round(estimate,  3),
      conf.low  = round(conf.low,  3),
      conf.high = round(conf.high, 3),
      p.value   = round(p.value,   3),
      sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".", TRUE ~ ""
      ),
      direction = case_when(
        conf.low  > 0 ~ "Positive",
        conf.high < 0 ~ "Negative",
        TRUE          ~ "No clear effect"
      )
    )
}


# ── 3.4  Coefficient plot helper ──────────────────────────────
plot_coefs <- function(coef_df, title_str, caption_str, file_path,
                       ncol_facet = 2,
                       width  = params$plot_width,
                       height = params$plot_height) {
  coef_df |>
    mutate(
      term = recode(term, !!!term_labels),
      dv   = factor(dv, levels = intersect(dv_order, unique(dv)))
    ) |>
    ggplot(aes(x = estimate, y = reorder(term, estimate), color = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    scale_color_manual(values = c(
      "Positive"        = "#2166ac",
      "Negative"        = "#d6604d",
      "No clear effect" = "grey60"
    )) +
    facet_wrap(~ dv, ncol = ncol_facet, scales = "free_x") +
    labs(
      x       = "Estimated effect (HC1 robust SEs, 95% CI)",
      y       = NULL,
      color   = NULL,
      title   = title_str,
      caption = caption_str
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold"))
  
  ggsave(file_path, width = width, height = height, dpi = params$dpi)
  invisible()
}


# ── 3.5  Robustness comparison plot: ordered logit AME vs OLS ─
plot_robustness <- function(coef_polr, coef_ols, title_str, file_path,
                            ncol_facet = 2,
                            width  = params$plot_width,
                            height = params$plot_height) {
  bind_rows(
    coef_polr |> mutate(model = "Ordered logit AME"),
    coef_ols  |> mutate(model = "OLS")
  ) |>
    mutate(
      term = recode(term, !!!term_labels),
      dv   = factor(dv, levels = intersect(dv_order, unique(dv)))
    ) |>
    ggplot(aes(x = estimate, y = reorder(term, estimate),
               colour = model, shape = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    scale_colour_manual(
      values = c("Ordered logit AME" = "#d6604d", "OLS" = "#2166ac")
    ) +
    facet_wrap(~ dv, ncol = ncol_facet, scales = "free_x") +
    labs(
      x       = "Estimated effect",
      y       = NULL,
      colour  = NULL,
      shape   = NULL,
      title   = title_str,
      caption = "HC1 robust SEs. 95% CI. Ordered logit AME on P(response = 1)."
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold"))
  
  ggsave(file_path, width = width, height = height, dpi = params$dpi)
  invisible()
}


# ==============================================================
# 4.  DESCRIPTIVE PLOTS — DISTRIBUTION BY REGION
# ==============================================================

# ── 4.1  Compute full response distributions per DV × region ──
desc_by_region <- map_dfr(all_dv_vars, function(v) {
  df |>
    dplyr::filter(!is.na(.data[[v]]), !is.na(ses_region_cat)) |>
    group_by(ses_region_cat, value = as.character(.data[[v]])) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(ses_region_cat) |>
    mutate(pct = n / sum(n) * 100) |>
    ungroup() |>
    mutate(
      dv      = all_dv_labels[[v]],
      dv_type = ifelse(v %in% prop_vars, "Proportionality", "Reciprocity"),
      value   = factor(value,
                       levels = c("0", "0.33", "0.66", "1"),
                       labels = unname(level_labels))
    )
})

# ── 4.2  Proportionality — response distribution by region ────
desc_by_region |>
  dplyr::filter(dv_type == "Proportionality") |>
  mutate(dv = factor(dv, levels = unname(prop_labels))) |>
  ggplot(aes(x = value, y = pct, fill = ses_region_cat)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.85),
            vjust = -0.35, size = 2.6) +
  scale_fill_manual(values = region_colours) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.14))
  ) +
  facet_wrap(~ dv, ncol = 2) +
  labs(
    x       = "Response level  (0 = Unfair,  1 = Fair)",
    y       = "% of respondents (within region)",
    fill    = "Region",
    title   = "Proportionality attitudes — response distribution by region",
    caption = "DVs coded 0 / 0.33 / 0.66 / 1."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 20, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom"
  )

ggsave(
  file.path(params$out_desc, "desc_prop_dist_by_region.png"),
  width = params$plot_width, height = params$plot_height, dpi = params$dpi
)

# ── 4.3  Reciprocity — response distribution by region ────────
desc_by_region |>
  dplyr::filter(dv_type == "Reciprocity") |>
  mutate(dv = factor(dv, levels = unname(recip_labels))) |>
  ggplot(aes(x = value, y = pct, fill = ses_region_cat)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.85),
            vjust = -0.35, size = 2.6) +
  scale_fill_manual(values = region_colours) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.14))
  ) +
  facet_wrap(~ dv, ncol = 2) +
  labs(
    x       = "Response level  (0 = Unfair,  1 = Fair)",
    y       = "% of respondents (within region)",
    fill    = "Region",
    title   = "Reciprocity attitudes — response distribution by region",
    caption = "DVs coded 0 / 0.33 / 0.66 / 1."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 20, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom"
  )

ggsave(
  file.path(params$out_desc, "desc_recip_dist_by_region.png"),
  width = params$plot_width, height = params$plot_height / 1.5,
  dpi = params$dpi
)

# ── 4.4  Summary: mean response — dot-and-CI plot ─────────────
#
#   We summarise each DV as a mean on the 0 / 0.33 / 0.66 / 1 scale
#   with t-based 95% CIs, rather than showing the full response
#   distribution or the proportion at the top category only.
#
#   Rationale: the goal here is not to test regional differences
#   (no significant differences are expected or found across regions),
#   but to give readers an intuitive sense of where Canadians stand
#   on each fairness dimension. The mean and its CI communicate both
#   the central tendency and the uncertainty around it in a single,
#   readable number anchored to the substantive scale: values near
#   0.66–1 indicate attitudes closer to "fair", while values near
#   0–0.33 indicate attitudes closer to "unfair".
#
#   For example, equal opportunity clusters near or above 0.66 across
#   all regions, suggesting broad agreement that outcomes should
#   reflect equal opportunity. By contrast, violating outcomes (rich
#   and poor) sit closer to 0.33, reflecting a more widespread sense
#   that actual outcomes are unfair. This pattern is obscured in a
#   histogram but immediately legible in a dot-and-CI plot.
#
#   The t-based CI is appropriate here because the DV is treated as
#   a quasi-continuous scale; Wilson CIs, designed for binary
#   proportions, would be a poorer fit for this four-level ordinal
#   variable.
mean_ci <- map_dfr(all_dv_vars, function(v) {
  # ── By region ───────────────────────────────────────────────
  by_region <- df |>
    dplyr::filter(!is.na(.data[[v]]), !is.na(ses_region_cat)) |>
    group_by(ses_region_cat) |>
    summarise(
      n      = n(),
      mean   = mean(.data[[v]], na.rm = TRUE),
      se     = sd(.data[[v]], na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) |>
    rename(region = ses_region_cat)
  
  # ── All Canada ──────────────────────────────────────────────
  all_canada <- df |>
    dplyr::filter(!is.na(.data[[v]])) |>
    summarise(
      n    = n(),
      mean = mean(.data[[v]], na.rm = TRUE),
      se   = sd(.data[[v]], na.rm = TRUE) / sqrt(n())
    ) |>
    mutate(region = "All Canada")
  
  bind_rows(by_region, all_canada) |>
    mutate(
      dv      = all_dv_labels[[v]],
      dv_type = ifelse(v %in% prop_vars, "Proportionality", "Reciprocity"),
      lo      = mean - qt(0.975, df = n - 1) * se,
      hi      = mean + qt(0.975, df = n - 1) * se,
      region  = factor(region,
                       levels = c("Ontario", "Quebec", "Alberta",
                                  "Atlantic Canada", "All Canada"))
    ) |>
    dplyr::select(region, dv, dv_type, n, mean, se, lo, hi)
})

mean_ci |>
  mutate(dv = factor(dv, levels = dv_order)) |>
  ggplot(aes(x = mean, y = region, colour = region)) +
  geom_point(size = 3.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.3) +
  scale_colour_manual(values = c(region_colours, "All Canada" = "#636363"), guide = "none") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.33, 0.66, 1)) +
  facet_wrap(~ dv, ncol = 2) +
  labs(
    x       = "Mean response (0–1 scale)  —  t-based 95% CI",
    y       = NULL,
    title   = "Proportionality & Reciprocity — mean response by region",
    caption = "DV scale: 0 = Unfair, 1 = Fair. Error bars = t-based 95% CI."
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

ggsave(
  file.path(params$out_desc, "desc_all_mean_dotCI_by_region.png"),
  width = params$plot_width, height = params$plot_height + 2,
  dpi = params$dpi
)

# Save summary table
write.csv(
  mean_ci |> mutate(across(where(is.numeric), ~ round(.x, 3))),
  file.path(params$out_desc, "mean_response_by_region.csv"),
  row.names = FALSE
)

# ── 4.5  LaTeX descriptive table — mean [lo, hi] by region ────
#
#   Produces a publication-ready LaTeX table for the descriptive
#   statistics section. DVs are rows; regions are columns. Each
#   cell shows the mean and 95% CI as "mean [lo, hi]". Proportionality
#   and reciprocity DVs are separated by a group header row.
#   Requires kableExtra.

library(kableExtra)

desc_table <- mean_ci |>
  mutate(
    cell = sprintf("%.2f [%.2f, %.2f]", mean, lo, hi)
  ) |>
  dplyr::select(region, dv, dv_type, cell) |>
  pivot_wider(names_from = region, values_from = cell) |>
  # Enforce region column order
  dplyr::select(dv, dv_type,
                any_of(c("Ontario", "Quebec", "Alberta", "Atlantic Canada", "All Canada"))) |>
  # Enforce DV row order: proportionality first, then reciprocity
  mutate(dv = factor(dv, levels = c(unname(prop_labels), unname(recip_labels)))) |>
  arrange(dv) |>
  mutate(dv = as.character(dv))

# Row indices for each group (for kableExtra group headers)
n_prop  <- length(prop_vars)
n_recip <- length(recip_vars)

desc_table |>
  dplyr::select(-dv_type) |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    col.names = c("", "ON", "QC", "AB", "AC", "All Canada"),
    caption   = "Mean Fairness Attitudes by Region",
    label     = "tab:desc_fairness"
  ) |>
  kable_styling(latex_options = "hold_position") |>
  pack_rows("Proportionality", 1, n_prop) |>
  pack_rows("Reciprocity",     n_prop + 1, n_prop + n_recip) |>
  footnote(
    general           = "Mean response on 0/0.33/0.66/1 scale with t-based 95\\\\% CI in brackets. DV scale: 0 = Unfair, 1 = Fair. Ontario is the reference region. ON = Ontario, QC = Quebec, AB = Alberta, AC = Atlantic Canada.\\\\\\\\\\\\\\\\Source: Original dataset.",
    general_title     = "Note: ",
    footnote_as_chunk = TRUE,
    escape            = FALSE
  ) |>
  save_kable(file.path(params$out_desc, "table_desc_fairness.tex"))


# ==============================================================
# 5.  ORDERED LOGIT — FIT MODELS & AMEs
# ==============================================================

# ── 5.1  Fit one polr model per DV ────────────────────────────
polr_models <- dv_ord_vars |>
  set_names(all_dv_labels[all_dv_vars]) |>
  map(function(dv_ord) {
    model_vars <- c(dv_ord, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    MASS::polr(as.formula(paste(dv_ord, "~", rhs)),
               data = model_data, Hess = TRUE)
  })

# ── 5.2  Extract AMEs ─────────────────────────────────────────
ame_results <- map2(
  polr_models,
  names(polr_models),
  function(model, dv_label) tidy_polr_slopes(model, dv_label, data = df)
)

# Primary: AME on P(highest category = 1)
coef_polr_top <- map_dfr(ame_results, ~ .x$top)

# Full AME across all four levels — saved for reference
coef_polr_full <- map_dfr(ame_results, function(res) {
  res$full |>
    transmute(
      dv,
      term,
      outcome_level = as.character(group),
      estimate  = round(estimate,  3),
      conf.low  = round(conf.low,  3),
      conf.high = round(conf.high, 3),
      p.value   = round(p.value,   3),
      sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".", TRUE ~ ""
      )
    )
})

write.csv(coef_polr_full,
          file.path(params$out_reg, "AME_all_levels_fairness.csv"),
          row.names = FALSE)

# ── 5.3  Model fit: McFadden pseudo-R2 ────────────────────────
fit_polr <- map2_dfr(polr_models, names(polr_models), function(model, dv_label) {
  dv_ord     <- as.character(formula(model)[[2]])
  model_data <- model$model
  null_mod   <- MASS::polr(
    as.formula(paste(dv_ord, "~ 1")),
    data = model_data, Hess = TRUE
  )
  tibble(
    dv        = dv_label,
    pseudo_r2 = round(as.numeric(1 - logLik(model) / logLik(null_mod)), 3),
    n         = nrow(model_data)
  )
})

write.csv(fit_polr,
          file.path(params$out_reg, "fit_polr_fairness.csv"),
          row.names = FALSE)


# ==============================================================
# 6.  OLS — ROBUSTNESS CHECK
# ==============================================================
#   OLS on the raw 0/0.33/0.66/1 scale. Coefficients represent
#   unit changes in the mean response. Used to verify sign and
#   significance agreement with the ordered logit AMEs.

# ── 6.1  Fit OLS models ───────────────────────────────────────
ols_models <- all_dv_vars |>
  set_names(all_dv_labels[all_dv_vars]) |>
  map(function(dv) {
    model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    lm(as.formula(paste(dv, "~", rhs)), data = model_data)
  })

# ── 6.2  Extract HC1 marginal effects ─────────────────────────
coef_ols <- map2_dfr(all_dv_vars, all_dv_labels[all_dv_vars], function(dv, lbl) {
  extract_ame_ols(dv = dv, dv_label = lbl, rhs = rhs, data = df)
})

# ── 6.3  OLS model fit ────────────────────────────────────────
fit_ols <- map2_dfr(ols_models, names(ols_models), function(model, dv_label) {
  s <- summary(model)
  tibble(
    dv        = dv_label,
    r_squared = round(s$r.squared,     3),
    adj_r_sq  = round(s$adj.r.squared, 3),
    n         = length(s$residuals)
  )
})

write.csv(fit_ols,
          file.path(params$out_reg, "fit_ols_fairness.csv"),
          row.names = FALSE)


# ==============================================================
# 7.  COEFFICIENT PLOTS
# ==============================================================

# ── 7.1  Ordered logit AME — proportionality DVs ─────────────
plot_coefs(
  coef_df     = coef_polr_top |> dplyr::filter(dv %in% unname(prop_labels)),
  title_str   = "Proportionality attitudes — Ordered logit AME on P(response = 1)",
  caption_str = "AME on P(highest response level = 1). HC1 robust SEs. Error bars = 95% CI.",
  file_path   = file.path(params$out_reg, "coef_prop_polr_top.png"),
  ncol_facet  = 2,
  height      = params$plot_height + 2
)

# ── 7.2  Ordered logit AME — reciprocity DVs ─────────────────
plot_coefs(
  coef_df     = coef_polr_top |> dplyr::filter(dv %in% unname(recip_labels)),
  title_str   = "Reciprocity attitudes — Ordered logit AME on P(response = 1)",
  caption_str = "AME on P(highest response level = 1). HC1 robust SEs. Error bars = 95% CI.",
  file_path   = file.path(params$out_reg, "coef_recip_polr_top.png"),
  ncol_facet  = 2,
  height      = params$plot_height
)

# ── 7.3  OLS robustness — all DVs ─────────────────────────────
plot_coefs(
  coef_df     = coef_ols,
  title_str   = "Proportionality & Reciprocity — OLS coefficients (robustness check)",
  caption_str = "OLS on 0/0.33/0.66/1 scale. HC1 robust SEs. Error bars = 95% CI.",
  file_path   = file.path(params$out_reg, "coef_all_ols.png"),
  ncol_facet  = 3,
  width       = params$plot_width + 4,
  height      = params$plot_height + 4
)

# ── 7.4  Robustness comparison — proportionality DVs ──────────
plot_robustness(
  coef_polr  = coef_polr_top |> dplyr::filter(dv %in% unname(prop_labels)),
  coef_ols   = coef_ols      |> dplyr::filter(dv %in% unname(prop_labels)),
  title_str  = "Proportionality attitudes — Ordered logit AME vs OLS",
  file_path  = file.path(params$out_reg, "robustness_prop_polr_vs_ols.png"),
  ncol_facet = 2,
  height     = params$plot_height + 2
)

# ── 7.5  Robustness comparison — reciprocity DVs ──────────────
plot_robustness(
  coef_polr  = coef_polr_top |> dplyr::filter(dv %in% unname(recip_labels)),
  coef_ols   = coef_ols      |> dplyr::filter(dv %in% unname(recip_labels)),
  title_str  = "Reciprocity attitudes — Ordered logit AME vs OLS",
  file_path  = file.path(params$out_reg, "robustness_recip_polr_vs_ols.png"),
  ncol_facet = 2,
  height     = params$plot_height
)


# ==============================================================
# 8.  REGRESSION TABLES
# ==============================================================

# ── 8.1  Ordered logit — log-odds, proportionality DVs ────────
modelsummary(
  polr_models[unname(prop_labels)],
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = file.path(params$out_reg, "regtable_prop_polr_logodds.txt"),
  notes     = paste(
    "Ordered logit (polr). Log-odds coefficients.",
    "DVs coded 0 / 0.33 / 0.66 / 1; highest level (= 1) = 'fair'.",
    "Ontario is the reference region.",
    "* p<0.05, ** p<0.01, *** p<0.001"
  )
)

# ── 8.2  Ordered logit — log-odds, reciprocity DVs ────────────
modelsummary(
  polr_models[unname(recip_labels)],
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = file.path(params$out_reg, "regtable_recip_polr_logodds.txt"),
  notes     = paste(
    "Ordered logit (polr). Log-odds coefficients.",
    "DVs coded 0 / 0.33 / 0.66 / 1; highest level (= 1) = 'fair'.",
    "Ontario is the reference region.",
    "* p<0.05, ** p<0.01, *** p<0.001"
  )
)

# ── 8.3  OLS robustness — proportionality DVs ─────────────────
modelsummary(
  ols_models[unname(prop_labels)],
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov      = map(ols_models[unname(prop_labels)], robust_vcov),
  coef_map  = term_labels,
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  output    = file.path(params$out_reg, "regtable_prop_ols.txt"),
  notes     = paste(
    "OLS robustness check. DV scale: 0 / 0.33 / 0.66 / 1.",
    "HC1 robust SEs in parentheses.",
    "Ontario is the reference region.",
    "* p<0.05, ** p<0.01, *** p<0.001"
  )
)

# ── 8.4  OLS robustness — reciprocity DVs ─────────────────────
modelsummary(
  ols_models[unname(recip_labels)],
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov      = map(ols_models[unname(recip_labels)], robust_vcov),
  coef_map  = term_labels,
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  output    = file.path(params$out_reg, "regtable_recip_ols.txt"),
  notes     = paste(
    "OLS robustness check. DV scale: 0 / 0.33 / 0.66 / 1.",
    "HC1 robust SEs in parentheses.",
    "Ontario is the reference region.",
    "* p<0.05, ** p<0.01, *** p<0.001"
  )
)


# ==============================================================
# 8b.  NO-REGION MODELS — ROBUSTNESS CHECK
# ==============================================================
#   Re-runs ordered logit and OLS excluding all regional variables
#   (quebec_bin, alberta_bin, region_eastcoast_bin,
#   ideo_define_QC_first_bin, ses_french_bin).
#   Presented as a secondary specification to verify that
#   individual-level results are stable when regional variables
#   are excluded given null regional effects in the main models.

# ── 8b.1  Ordered logit — no-region models ────────────────────
polr_models_nr <- dv_ord_vars |>
  set_names(all_dv_labels[all_dv_vars]) |>
  map(function(dv_ord) {
    model_vars <- c(dv_ord, all.vars(as.formula(paste("~", rhs_no_region))))
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    MASS::polr(as.formula(paste(dv_ord, "~", rhs_no_region)),
               data = model_data, Hess = TRUE)
  })

ame_results_nr <- map2(
  polr_models_nr,
  names(polr_models_nr),
  function(model, dv_label) tidy_polr_slopes(model, dv_label, data = df)
)

coef_polr_nr_top <- map_dfr(ame_results_nr, ~ .x$top)

# ── 8b.2  OLS — no-region models ──────────────────────────────
ols_models_nr <- all_dv_vars |>
  set_names(all_dv_labels[all_dv_vars]) |>
  map(function(dv) {
    model_vars <- c(dv, all.vars(as.formula(paste("~", rhs_no_region))))
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    lm(as.formula(paste(dv, "~", rhs_no_region)), data = model_data)
  })

coef_ols_nr <- map2_dfr(all_dv_vars, all_dv_labels[all_dv_vars], function(dv, lbl) {
  extract_ame_ols(dv = dv, dv_label = lbl, rhs = rhs_no_region, data = df)
})

# ── 8b.3  Regression tables — no-region ordered logit ─────────
modelsummary(
  polr_models_nr[unname(prop_labels)],
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = file.path(params$out_reg, "regtable_prop_polr_noregion.txt"),
  notes     = paste(
    "Ordered logit (polr). Log-odds coefficients.",
    "DVs coded 0 / 0.33 / 0.66 / 1; highest level (= 1) = 'fair'.",
    "Regional variables excluded (robustness check).",
    "* p<0.05, ** p<0.01, *** p<0.001"
  )
)

modelsummary(
  polr_models_nr[unname(recip_labels)],
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = file.path(params$out_reg, "regtable_recip_polr_noregion.txt"),
  notes     = paste(
    "Ordered logit (polr). Log-odds coefficients.",
    "DVs coded 0 / 0.33 / 0.66 / 1; highest level (= 1) = 'fair'.",
    "Regional variables excluded (robustness check).",
    "* p<0.05, ** p<0.01, *** p<0.001"
  )
)

# ── 8b.4  Regression tables — no-region OLS ───────────────────
modelsummary(
  ols_models_nr[unname(prop_labels)],
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov      = map(ols_models_nr[unname(prop_labels)], robust_vcov),
  coef_map  = term_labels,
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  output    = file.path(params$out_reg, "regtable_prop_ols_noregion.txt"),
  notes     = paste(
    "OLS robustness check. DV scale: 0 / 0.33 / 0.66 / 1.",
    "HC1 robust SEs in parentheses.",
    "Regional variables excluded (robustness check).",
    "* p<0.05, ** p<0.01, *** p<0.001"
  )
)

modelsummary(
  ols_models_nr[unname(recip_labels)],
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov      = map(ols_models_nr[unname(recip_labels)], robust_vcov),
  coef_map  = term_labels,
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  output    = file.path(params$out_reg, "regtable_recip_ols_noregion.txt"),
  notes     = paste(
    "OLS robustness check. DV scale: 0 / 0.33 / 0.66 / 1.",
    "HC1 robust SEs in parentheses.",
    "Regional variables excluded (robustness check).",
    "* p<0.05, ** p<0.01, *** p<0.001"
  )
)


# ==============================================================
# 9.  DIAGNOSTICS
# ==============================================================

# ── 9.1  Brant test — proportional odds assumption ────────────
#
#   The Brant test evaluates whether the proportional odds
#   assumption holds for each predictor in the ordered logit.
#   A significant result (p < 0.05) for a given term indicates
#   that the log-odds ratio is not constant across response
#   thresholds, violating the assumption. The omnibus test
#   assesses the assumption globally across all predictors.
#
#   Note: brant() requires the model to be fitted with polr()
#   and does not support HC1 robust SEs — this is a diagnostic
#   only. If violations are found, consider a partial proportional
#   odds model (vglm with cumulative family) or report the OLS
#   robustness check as the primary specification.
#
#   Requires the brant package.

library(brant)

brant_results <- map2_dfr(polr_models, names(polr_models), function(model, dv_label) {
  bt <- tryCatch(
    brant(model),
    error = function(e) NULL
  )
  if (is.null(bt)) {
    tibble(dv = dv_label, term = "ERROR", chi_sq = NA, df = NA, p_value = NA)
  } else {
    as.data.frame(bt) |>
      rownames_to_column("term") |>
      as_tibble() |>
      rename(chi_sq = X2, df = df, p_value = p) |>
      mutate(
        dv  = dv_label,
        sig = case_when(
          p_value < 0.001 ~ "***", p_value < 0.01 ~ "**",
          p_value < 0.05  ~ "*",   p_value < 0.10 ~ ".", TRUE ~ ""
        )
      ) |>
      dplyr::select(dv, term, chi_sq, df, p_value, sig)
  }
})

cat("\n========== BRANT TEST — PROPORTIONAL ODDS ASSUMPTION ==========\n")
cat("Significant results (p < 0.05) indicate a violation for that term/DV.\n\n")
brant_results |>
  dplyr::filter(!is.na(p_value)) |>
  mutate(across(c(chi_sq, p_value), ~ round(.x, 3))) |>
  print(n = Inf)

write.csv(
  brant_results |> mutate(across(c(chi_sq, p_value), ~ round(.x, 3))),
  file.path(params$out_reg, "brant_test_fairness.csv"),
  row.names = FALSE
)

cat("\n========== MODEL FIT — ORDERED LOGIT (McFadden pseudo-R2) ==========\n")
print(fit_polr)

cat("\n========== MODEL FIT — OLS (adjusted R2) ==========\n")
print(fit_ols)

cat("\n========== MOST CONSISTENTLY SIGNIFICANT PREDICTORS (polr top-AME) ==========\n")
coef_polr_top |>
  dplyr::filter(sig %in% c("*", "**", "***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

cat("\n========== SIGN AGREEMENT: ordered logit AME vs OLS ==========\n")
check_signs <- coef_polr_top |>
  dplyr::select(dv, term, est_polr = estimate, sig_polr = sig) |>
  left_join(
    coef_ols |> dplyr::select(dv, term, est_ols = estimate, sig_ols = sig),
    by = c("dv", "term")
  ) |>
  mutate(
    sign_agree = sign(est_polr) == sign(est_ols),
    both_sig   = sig_polr %in% c("*", "**", "***") &
      sig_ols   %in% c("*", "**", "***")
  )

cat("Sign disagreements among terms significant in both models:\n")
check_signs |>
  dplyr::filter(both_sig, !sign_agree) |>
  print()

cat("\nOverall sign agreement rate (both-significant terms):",
    round(mean(check_signs$sign_agree[check_signs$both_sig], na.rm = TRUE), 3), "\n")

cat("\n========== OUTPUT FILES ==========\n")
cat("Descriptive CSV:                  ", file.path(params$out_desc, "mean_response_by_region.csv"), "\n")
cat("Descriptive LaTeX table:          ", file.path(params$out_desc, "table_desc_fairness.tex"), "\n")
cat("AME all levels CSV:               ", file.path(params$out_reg,  "AME_all_levels_fairness.csv"), "\n")
cat("Brant test CSV:                   ", file.path(params$out_reg,  "brant_test_fairness.csv"), "\n")
cat("Model fit — polr:                 ", file.path(params$out_reg,  "fit_polr_fairness.csv"), "\n")
cat("Model fit — OLS:                  ", file.path(params$out_reg,  "fit_ols_fairness.csv"), "\n")
cat("Reg table prop polr:              ", file.path(params$out_reg,  "regtable_prop_polr_logodds.txt"), "\n")
cat("Reg table recip polr:             ", file.path(params$out_reg,  "regtable_recip_polr_logodds.txt"), "\n")
cat("Reg table prop OLS:               ", file.path(params$out_reg,  "regtable_prop_ols.txt"), "\n")
cat("Reg table recip OLS:              ", file.path(params$out_reg,  "regtable_recip_ols.txt"), "\n")
cat("Reg table prop polr (no region):  ", file.path(params$out_reg,  "regtable_prop_polr_noregion.txt"), "\n")
cat("Reg table recip polr (no region): ", file.path(params$out_reg,  "regtable_recip_polr_noregion.txt"), "\n")
cat("Reg table prop OLS (no region):   ", file.path(params$out_reg,  "regtable_prop_ols_noregion.txt"), "\n")
cat("Reg table recip OLS (no region):  ", file.path(params$out_reg,  "regtable_recip_ols_noregion.txt"), "\n")
cat("\n========== PIPELINE COMPLETE ==========\n")
