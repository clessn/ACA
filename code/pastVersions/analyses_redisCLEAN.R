# ==============================================================
# Redistribution Attitudes — Master Script
# Four-region Canadian survey: Alberta, Ontario, Quebec, Atlantic Canada
#
# Sections:
#   0.  Setup: packages, paths, output folders
#   1.  Variable construction
#   2.  IV list, RHS formula, term labels
#   3.  Helper functions
#   4.  Descriptive statistics (bar charts, region comparisons)
#   5.  Bivariate marginal means & contrast tables
#   6.  Ordered logit — fit models
#   7.  Ordered logit — average marginal effects (AME)
#   8.  OLS models (robustness check)
#   9.  Regression tables
#  10.  Coefficient plots
#  11.  Interaction plots: Ideology × Quebec
#  12.  Model fit summaries
#  13.  Diagnostics
# ==============================================================


# ==============================================================
# 0.  SETUP
# ==============================================================

# ── 0.1  Packages ─────────────────────────────────────────────
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(marginaleffects)
library(modelsummary)
library(MASS)          # polr()
library(sandwich)
library(lmtest)
library(ggthemes)
library(RColorBrewer)

# Resolve namespace conflicts introduced by MASS
select <- dplyr::select
filter <- dplyr::filter

# ── 0.2  Global parameters ────────────────────────────────────
params <- list(
  dpi         = 300,
  plot_width  = 14,
  plot_height = 10,
  out_graphs  = "graphs",
  out_desc    = "graphs/descriptives_redis",
  out_reg     = "graphs/regressions_redis",
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

# ── 1.1  Derived binary variables ────────────────────────────
df$univ_educ_bin <- as.integer(df$educ_group == "educUniv")

if (!"incomeHigh_bin" %in% names(df)) {
  df$incomeHigh_bin <- as.integer(df$ses_income3Cat == "High")
}

# ── 1.2  Factor ordering ──────────────────────────────────────
df$educ_group     <- factor(df$educ_group,
                            levels = c("educBHS", "educHS", "educUniv"))
df$ses_region_cat <- factor(df$ses_region_cat,
                            levels = c("Ontario", "Quebec", "Alberta", "East Coast"))
df$ses_income3Cat <- factor(df$ses_income3Cat,
                            levels = c("Low", "Mid", "High"))

# ── 1.3  Quebec vs Other regions grouping ────────────────────
df <- df |>
  mutate(
    region_group = ifelse(ses_region_cat == "Quebec", "Quebec", "Other regions"),
    region_group = factor(region_group, levels = c("Other regions", "Quebec"))
  )

# ── 1.4  DV vectors and labels ───────────────────────────────
# 1 = fair, 0 = unfair
dv_vars <- c(
  "redis_effort_num",
  "redis_no_cheat_system_num",
  "redis_reasons_poor_num",
  "redis_reasons_rich_num",
  "redis_social_benefits_num",
  "redis_welfare_num",
  "redis_opportunity_num",
  "redis_intelligence_num"
)

dv_labels <- c(
  redis_effort_num          = "Fairness of income distribution",
  redis_no_cheat_system_num = "Trust not to cheat system",
  redis_reasons_poor_num    = "Violating outcomes (Poor)",
  redis_reasons_rich_num    = "Violating outcomes (Rich)",
  redis_social_benefits_num = "Social benefits not a choice",
  redis_welfare_num         = "Welfare goes to undeserving",
  redis_opportunity_num     = "People have equal opportunity",
  redis_intelligence_num    = "Rewarded for effort and skill"
)

# ── 1.5  Ordered-factor versions of DVs (for polr) ───────────
for (v in dv_vars) {
  observed_levels <- sort(unique(na.omit(df[[v]])))
  df[[paste0(v, "_ord")]] <- factor(
    as.character(df[[v]]),
    levels  = as.character(observed_levels),
    ordered = TRUE
  )
}

dv_ord_vars <- paste0(dv_vars, "_ord")

# ── 1.6  Canonical DV order for plots ────────────────────────
dv_order <- unname(dv_labels)   # preserves declaration order


# ==============================================================
# 2.  IV LIST, RHS FORMULA, TERM LABELS
# ==============================================================

# ── 2.1  IV list ──────────────────────────────────────────────
#   Each entry: type ("binary" | "numeric" | "factor"),
#               var  (column name), low/high (contrast values),
#               label (axis label for plots)

ivs <- list(
  list(type = "binary",  var = "quebec_bin",                 low = 0, high = 1, label = "Quebec (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ideo_define_QC_first_bin",   low = 0, high = 1, label = "Quebecker First (No=0 vs Yes=1)"),
  list(type = "binary",  var = "alberta_bin",                low = 0, high = 1, label = "Alberta (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ontario_bin",                low = 0, high = 1, label = "Ontario (No=0 vs Yes=1)"),
  list(type = "binary",  var = "region_eastcoast_bin",       low = 0, high = 1, label = "Atlantic Canada (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ses_male_bin",               low = 0, high = 1, label = "Gender (Female=0 vs Male=1)"),
  list(type = "numeric", var = "ses_age",                    low = 30, high = 60, label = "Age (30 vs 60)"),
  list(type = "numeric", var = "univ_educ_bin",              low = 0, high = 1, label = "Education (University vs below)"),
  list(type = "numeric", var = "incomeHigh_bin",             low = 0, high = 1, label = "Income (High vs low/mid)"),
  list(type = "binary",  var = "ses_citizenYes_bin",         low = 0, high = 1, label = "Citizen (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ses_french_bin",             low = 0, high = 1, label = "French-speaking (No=0 vs Yes=1)"),
  list(type = "binary",  var = "trust_social_bin",           low = 0, high = 1, label = "Social Trust (Low=0 vs High=1)"),
  list(type = "binary",  var = "employ_fulltime_bin",        low = 0, high = 1, label = "Employed Full-Time (No=0 vs Yes=1)"),
  list(type = "numeric", var = "ideo_right_num",             low = 0, high = 1, label = "Ideology: Left (0) vs Right (1)"),
  list(type = "numeric", var = "ideo_interest_politics_num", low = 0, high = 1, label = "Political Interest: Low (0) vs High (1)")
)

# ── 2.2  RHS formula string ───────────────────────────────────
#   Ontario is the omitted region reference category
rhs <- "quebec_bin + ideo_define_QC_first_bin + alberta_bin + region_eastcoast_bin +
        ses_male_bin + ses_age + incomeHigh_bin + univ_educ_bin +
        ses_citizenYes_bin + ses_french_bin + trust_social_bin +
        employ_fulltime_bin + ideo_right_num + ideo_interest_politics_num +
        vote_PLC_bin + vote_PCC_bin"

# ── 2.3  Term labels (used by all regression tables and plots) ─
term_labels <- c(
  "quebec_bin"                 = "Quebec",
  "ideo_define_QC_first_bin"   = "Quebecker First",
  "alberta_bin"                = "Alberta",
  "region_eastcoast_bin"       = "Atlantic Canada",
  "ses_male_bin"               = "Male",
  "ses_age"                    = "Age",
  "univ_educ_bin"              = "Education: Below HS/HS vs. Univ",
  "incomeHigh_bin"             = "Income: Low/Mid vs High",
  "ses_citizenYes_bin"         = "Citizen",
  "children_bin"               = "Has Children",
  "ses_french_bin"             = "French-speaking",
  "trust_social_bin"           = "Social Trust",
  "trust_inst_prov_bin"        = "Provincial Trust",
  "trust_inst_fed_bin"         = "Federal Trust",
  "employ_fulltime_bin"        = "Employed Full-Time",
  "ideo_right_num"             = "Ideology (Right)",
  "ideo_interest_politics_num" = "Political Interest"
)


# ==============================================================
# 3.  HELPER FUNCTIONS
# ==============================================================

# ── 3.1  HC1 robust variance-covariance ───────────────────────
robust_vcov <- function(model) vcovHC(model, type = "HC1")


# ── 3.2  Bivariate marginal means plot ───────────────────────
#   Fits a simple bivariate OLS, predicts at low/high contrast
#   values, and saves a dot-and-CI plot.
plot_marginal <- function(dv, dv_label, iv, data) {
  model_data <- data |> dplyr::select(all_of(c(dv, iv$var))) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", iv$var)), data = model_data)
  vm         <- robust_vcov(model)
  
  plot_data <- if (iv$type == "numeric") {
    nd <- data.frame(x = c(iv$low, iv$high)); names(nd) <- iv$var
    predictions(model, newdata = nd, vcov = vm) |>
      mutate(x_label = factor(.data[[iv$var]],
                              levels = c(iv$low, iv$high),
                              labels = c(as.character(iv$low), as.character(iv$high))))
  } else if (iv$type == "binary") {
    nd <- data.frame(x = c(0, 1)); names(nd) <- iv$var
    predictions(model, newdata = nd, vcov = vm) |>
      mutate(x_label = factor(.data[[iv$var]],
                              levels = c(0, 1),
                              labels = c(as.character(iv$low), as.character(iv$high))))
  } else {
    predictions(model, by = iv$var, vcov = vm) |>
      mutate(x_label = .data[[iv$var]])
  }
  
  p <- ggplot(plot_data, aes(x = x_label, y = estimate)) +
    geom_point(size = 3, color = "#2166ac") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12, color = "#2166ac") +
    scale_y_continuous(limits = c(0, 1), labels = scales::label_number(accuracy = 0.1)) +
    labs(x = iv$label, y = "Predicted value (0–1 scale)", title = dv_label,
         caption = "OLS prediction. HC1 robust SEs. Error bars = 95% CI.") +
    theme_minimal(base_size = 13)
  
  ggsave(file.path(params$out_desc, paste0("marginal_", iv$var, "_", dv, ".png")),
         plot = p, width = 6, height = 4, dpi = params$dpi)
  invisible(p)
}


# ── 3.3  Bivariate contrast table ────────────────────────────
#   Returns a one-row tibble with the HC1-robust contrast between
#   low and high values of an IV for a single DV.
interpret_contrast <- function(dv, dv_label, iv, data) {
  iv_var  <- iv$var
  iv_low  <- as.character(iv$low)
  iv_high <- as.character(iv$high)
  
  model_data    <- data |> dplyr::select(all_of(c(dv, iv_var))) |> drop_na()
  model         <- lm(as.formula(paste(dv, "~", iv_var)), data = model_data)
  vm            <- robust_vcov(model)
  contrast_spec <- list(c(iv$low, iv$high))
  names(contrast_spec) <- iv_var
  
  tryCatch({
    avg_comparisons(model, variables = contrast_spec, vcov = vm, newdata = model_data) |>
      as_tibble() |>
      transmute(
        iv        = iv$label,
        dv        = dv_label,
        contrast  = paste(iv_low, "vs", iv_high),
        estimate  = round(estimate,  3),
        conf.low  = round(conf.low,  3),
        conf.high = round(conf.high, 3),
        p.value   = round(p.value,   3),
        sig = case_when(
          p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
          p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".", TRUE ~ ""
        ),
        direction = case_when(
          conf.low  > 0 ~ paste(iv_high, ">", iv_low),
          conf.high < 0 ~ paste(iv_low,  ">", iv_high),
          TRUE          ~ "No clear difference"
        )
      )
  }, error = function(e) {
    cat("FAILED:", iv_var, "~", dv, "\n  Error:", conditionMessage(e), "\n")
    tibble(iv = iv$label, dv = dv_label, contrast = paste(iv_low, "vs", iv_high),
           estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
           p.value  = NA_real_, sig = NA_character_, direction = "ERROR")
  })
}


# ── 3.4  Ordered logit AME extractor ─────────────────────────
#   Re-fits polr() on complete cases for numerical stability,
#   computes HC1 avg_slopes(), and returns a list with:
#     $top  — AME on the highest response category only
#     $full — AME across all response categories
tidy_polr_slopes <- function(model, dv_label, question = NULL, data = df) {
  fml         <- formula(model)
  model_vars  <- all.vars(fml)
  model_data  <- data |> dplyr::select(all_of(model_vars)) |> drop_na()
  model_clean <- MASS::polr(fml, data = model_data, Hess = TRUE)
  
  slopes_all <- avg_slopes(model_clean, vcov = "HC1", newdata = model_data) |>
    as_tibble() |>
    mutate(
      dv         = dv_label,
      question   = question,
      term_clean = case_when(
        term == "educ_group"     & contrast == "educHS - educBHS"   ~ "Education: HS vs Below HS",
        term == "educ_group"     & contrast == "educUniv - educBHS" ~ "Education: Univ vs Below HS",
        term == "ses_income3Cat" & contrast == "Mid - Low"          ~ "Income: Mid vs Low",
        term == "ses_income3Cat" & contrast == "High - Low"         ~ "Income: High vs Low",
        TRUE ~ term
      )
    )
  
  top_level  <- slopes_all |> pull(group) |> as.character() |> unique() |> sort() |> tail(1)
  
  slopes_top <- slopes_all |>
    dplyr::filter(as.character(group) == top_level) |>
    transmute(
      question, dv, term = term_clean,
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


# ── 3.5  OLS AME extractor ────────────────────────────────────
#   Fits OLS on complete cases and extracts HC1 avg_slopes().
#   Note: DVs are 1–4 ordinal scales; OLS is used as a robustness
#   check alongside the primary ordered logit, not as an LPM.
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


# ── 3.6  Coefficient plot helper ─────────────────────────────
plot_coefs <- function(coef_df, title_str, file_path, ncol_facet = 2,
                       width = params$plot_width, height = params$plot_height) {
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
    labs(x = "Average marginal effect (HC1 robust SEs)", y = NULL,
         color = NULL, title = title_str,
         caption = "AME on P(highest response category). Ordered logit.") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file_path, width = width, height = height, dpi = params$dpi)
}


# ── 3.7  Robustness plot: polr AME vs OLS ────────────────────
plot_robustness <- function(coef_polr, coef_lpm, title_str, file_path,
                            ncol = 2,
                            width = params$plot_width,
                            height = params$plot_height) {
  bind_rows(
    coef_polr |> mutate(model = "Ordered logit AME"),
    coef_lpm  |> mutate(model = "OLS")
  ) |>
    mutate(
      term = recode(term, !!!term_labels),
      dv   = factor(dv, levels = intersect(dv_order, unique(dv)))
    ) |>
    ggplot(aes(x = estimate, y = reorder(term, estimate),
               color = model, shape = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    scale_color_manual(values = c("Ordered logit AME" = "#d6604d", "OLS" = "#2166ac")) +
    facet_wrap(~ dv, ncol = ncol) +
    labs(x = "Estimated effect", y = NULL, color = NULL, shape = NULL,
         title = title_str, caption = "HC1 robust SEs. 95% CI.") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file_path, width = width, height = height, dpi = params$dpi)
}


# ── 3.8  R-squared / pseudo-R2 bar chart ─────────────────────
plot_r2 <- function(fit_df, r2_col = "pseudo_r2", title_str, file_path,
                    width = 8, height = 5) {
  fit_df |>
    mutate(
      fit_level = case_when(
        .data[[r2_col]] >= 0.07 ~ "High (>=0.07)",
        .data[[r2_col]] >= 0.03 ~ "Moderate (0.03-0.07)",
        TRUE                    ~ "Low (<0.03)"
      ),
      fit_level = factor(fit_level,
                         levels = c("High (>=0.07)", "Moderate (0.03-0.07)", "Low (<0.03)")),
      dv = fct_reorder(dv, .data[[r2_col]])
    ) |>
    ggplot(aes(x = .data[[r2_col]], y = dv, fill = fit_level)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.3f", .data[[r2_col]])), hjust = -0.15, size = 3.2) +
    scale_fill_manual(values = c(
      "High (>=0.07)"        = "#2166ac",
      "Moderate (0.03-0.07)" = "#92c5de",
      "Low (<0.03)"          = "#d6604d"
    )) +
    scale_x_continuous(limits = c(0, 0.20),
                       labels = scales::label_number(accuracy = 0.01)) +
    labs(
      x     = ifelse(r2_col == "pseudo_r2", "McFadden pseudo-R2", "Adjusted R2"),
      y     = NULL, fill = "Model fit", title = title_str
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank())
  ggsave(file_path, width = width, height = height, dpi = params$dpi)
}


# ── 3.9  Direction agreement diagnostic ──────────────────────
check_direction_agreement <- function(coef_polr, coef_lpm, label = "") {
  cat("\n========== Ordered logit vs OLS direction agreement", label, "==========\n")
  bind_rows(
    coef_polr |> mutate(model = "Polr"),
    coef_lpm  |> mutate(model = "OLS")
  ) |>
    dplyr::filter(sig %in% c("*", "**", "***")) |>
    dplyr::select(dv, term, model, direction) |>
    pivot_wider(names_from = model, values_from = direction) |>
    dplyr::filter(OLS != Polr) |>
    print()
}


# ==============================================================
# 4.  DESCRIPTIVE STATISTICS
# ==============================================================

# ── 4.1  Overall distributions (with NAs) ────────────────────
desc_redis_NA <- map_dfr(dv_vars, function(v) {
  df |>
    count(.data[[v]]) |>
    mutate(dv = dv_labels[[v]], value = as.character(.data[[v]]), pct = n / sum(n) * 100) |>
    rename(raw_value = 1)
})

desc_redis_NA |>
  mutate(raw_value = factor(raw_value, levels = sort(unique(as.numeric(raw_value))))) |>
  ggplot(aes(x = raw_value, y = pct, fill = raw_value)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.4, size = 3.2) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  facet_wrap(~ dv, ncol = 2) +
  labs(x = "Response category (1 = low, 4 = high)", y = "% of respondents",
       title = "Distribution of redistribution attitude items (incl. NAs)") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.major.x = element_blank())

ggsave(file.path(params$out_desc, "desc_redis_distributions_NAs.png"),
       width = 12, height = 10, dpi = params$dpi)


# ── 4.2  Overall distributions (without NAs) ─────────────────
desc_redis <- map_dfr(dv_vars, function(v) {
  df |>
    dplyr::filter(!is.na(.data[[v]])) |>
    count(raw_value = .data[[v]]) |>
    mutate(dv = dv_labels[[v]], pct = n / sum(n) * 100)
})

desc_redis |>
  mutate(raw_value = factor(raw_value, levels = sort(unique(raw_value)))) |>
  ggplot(aes(x = raw_value, y = pct, fill = raw_value)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.4, size = 3.2) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  facet_wrap(~ dv, ncol = 2) +
  labs(x = "Response category (1 = low, 4 = high)", y = "% of respondents",
       title = "Distribution of redistribution attitude items") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.major.x = element_blank())

ggsave(file.path(params$out_desc, "desc_redis_distributions.png"),
       width = 12, height = 10, dpi = params$dpi)


# ── 4.3  By four regions ──────────────────────────────────────
desc_redis_region <- map_dfr(dv_vars, function(v) {
  df |>
    dplyr::filter(!is.na(.data[[v]])) |>
    group_by(ses_region_cat, value = .data[[v]]) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(ses_region_cat) |>
    mutate(pct = n / sum(n) * 100) |>
    ungroup() |>
    mutate(dv = dv_labels[[v]])
}) |>
  mutate(value = factor(value, levels = sort(unique(as.numeric(value)))))

ggplot(desc_redis_region, aes(x = value, y = pct, fill = ses_region_cat)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  facet_wrap(~ dv, ncol = 2) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Response category (1 = low, 4 = high)", y = "% of respondents",
       fill = "Region", title = "Redistribution attitude items by region") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.major.x = element_blank(),
        legend.position = "bottom")

ggsave(file.path(params$out_desc, "desc_redis_by_region_dodged.png"),
       width = 12, height = 10, dpi = params$dpi)


# ── 4.4  Quebec vs Other regions — bar chart ─────────────────
desc_redis_region2 <- map_dfr(dv_vars, function(v) {
  df |>
    dplyr::filter(!is.na(.data[[v]])) |>
    group_by(region_group, value = .data[[v]]) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(region_group) |>
    mutate(pct = n / sum(n) * 100) |>
    ungroup() |>
    mutate(dv = dv_labels[[v]])
}) |>
  mutate(value = factor(value, levels = sort(unique(as.numeric(value)))))

ggplot(desc_redis_region2, aes(x = value, y = pct, fill = region_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
  facet_wrap(~ dv, ncol = 2) +
  scale_fill_manual(values = c("Other regions" = "#2166ac", "Quebec" = "#d6604d")) +
  scale_y_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Response category (1 = low, 4 = high)", y = "% of respondents (within region)",
       fill = "Region", title = "Redistribution attitudes: Quebec vs Other regions") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.major.x = element_blank(),
        legend.position = "bottom")

ggsave(file.path(params$out_desc, "desc_redis_Quebec_vs_Others_bars.png"),
       width = 12, height = 10, dpi = params$dpi)


# ── 4.5  Quebec vs Other regions — line chart ────────────────
ggplot(desc_redis_region2,
       aes(x = value, y = pct, group = region_group, color = region_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ dv, ncol = 2) +
  scale_color_manual(values = c("Other regions" = "#2166ac", "Quebec" = "#d6604d")) +
  scale_y_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Response category (1 = low, 4 = high)", y = "% of respondents (within region)",
       color = "Region", title = "Redistribution attitudes: Quebec vs Other regions") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.major.x = element_blank(),
        legend.position = "bottom")

ggsave(file.path(params$out_desc, "desc_redis_Quebec_vs_Others_lines.png"),
       width = 12, height = 10, dpi = params$dpi)


# ==============================================================
# 5.  BIVARIATE MARGINAL MEANS & CONTRAST TABLES
# ==============================================================

# ── 5.1  Marginal means plots (one PNG per IV × DV combination) ─
walk(ivs, function(iv) {
  walk2(dv_vars, dv_labels, plot_marginal, iv = iv, data = df)
})

# ── 5.2  Contrast tables ──────────────────────────────────────
bivar_contrasts <- map_dfr(ivs, function(iv) {
  map2_dfr(dv_vars, dv_labels, interpret_contrast, iv = iv, data = df)
})

write.csv(bivar_contrasts,
          file.path(params$out_reg, "bivar_contrasts_redis.csv"), row.names = FALSE)


# ==============================================================
# 6.  ORDERED LOGIT — FIT MODELS
# ==============================================================

polr_models <- dv_ord_vars |>
  set_names(dv_labels[dv_vars]) |>
  map(function(dv_ord) {
    model_vars <- c(dv_ord, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    MASS::polr(as.formula(paste(dv_ord, "~", rhs)), data = model_data, Hess = TRUE)
  })


# ==============================================================
# 7.  ORDERED LOGIT — AVERAGE MARGINAL EFFECTS
# ==============================================================

ame_results <- map2(
  polr_models,
  names(polr_models),
  function(model, dv_label) tidy_polr_slopes(model, dv_label, data = df)
)

# Top-category AME (used for plots and diagnostics)
coef_redis_top <- map_dfr(ame_results, ~ .x$top)

# Full AME across all response levels (saved to CSV)
coef_redis_full <- map_dfr(ame_results, function(res) {
  res$full |>
    transmute(
      dv,
      term          = term_clean,
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

write.csv(coef_redis_full,
          file.path(params$out_reg, "AME_all_levels_redis.csv"), row.names = FALSE)


# ==============================================================
# 8.  OLS MODELS (robustness check)
# ==============================================================

ols_models <- dv_vars |>
  set_names(dv_labels[dv_vars]) |>
  map(function(dv) {
    model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    lm(as.formula(paste(dv, "~", rhs)), data = model_data)
  })

coef_redis_ols <- map2_dfr(ols_models, names(ols_models), function(model, dv_label) {
  extract_ame_ols(
    dv       = all.vars(formula(model))[1],
    dv_label = dv_label,
    rhs      = rhs,
    data     = df
  )
})

fit_redis_ols <- map2_dfr(ols_models, names(ols_models), function(model, dv_label) {
  s <- summary(model)
  tibble(
    dv        = dv_label,
    r_squared = round(s$r.squared,     3),
    adj_r_sq  = round(s$adj.r.squared, 3),
    n         = length(s$residuals)
  )
})


# ==============================================================
# 9.  REGRESSION TABLES
# ==============================================================

# ── 9.1  Ordered logit — log-odds ────────────────────────────
modelsummary(
  polr_models,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = file.path(params$out_reg, "regtable_redis_polr_logodds.txt"),
  notes     = paste("Ordered logit (polr). Log-odds coefficients.",
                    "Interpret via AME table (Section 7).",
                    "* p<0.05, ** p<0.01, *** p<0.001")
)

# ── 9.2  Ordered logit — AME on highest category ─────────────
modelsummary(
  map(polr_models, function(m) {
    model_vars  <- all.vars(formula(m))
    model_data  <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    model_clean <- MASS::polr(formula(m), data = model_data, Hess = TRUE)
    avg_slopes(model_clean, vcov = "HC1", newdata = model_data,
               variables = names(term_labels)[names(term_labels) %in% all.vars(formula(m))])
  }),
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = term_labels,
  gof_map   = c("nobs"),
  output    = file.path(params$out_reg, "regtable_redis_polr_AME.txt"),
  notes     = paste("Average marginal effects on P(highest category) from ordered logit.",
                    "HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001")
)

# ── 9.3  OLS coefficients (robustness check) ─────────────────
modelsummary(
  ols_models,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov      = map(ols_models, robust_vcov),
  coef_map  = term_labels,
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  output    = file.path(params$out_reg, "regtable_redis_OLS.txt"),
  notes     = paste("OLS (robustness check). DVs are 1–4 ordinal scales.",
                    "HC1 robust SEs in parentheses.",
                    "* p<0.05, ** p<0.01, *** p<0.001")
)


# ==============================================================
# 10.  COEFFICIENT PLOTS
# ==============================================================

# ── 10.1  Ordered logit AME — top category ───────────────────
plot_coefs(
  coef_redis_top,
  title_str = "Redistribution attitudes — Ordered logit AME (highest category)",
  file_path = file.path(params$out_reg, "coef_redis_polr_top.png")
)

# ── 10.2  OLS coefficients (robustness check) ────────────────
plot_coefs(
  coef_redis_ols,
  title_str = "Redistribution attitudes — OLS coefficients (robustness check)",
  file_path = file.path(params$out_reg, "coef_redis_OLS.png")
)

# ── 10.3  Robustness: ordered logit AME vs OLS ───────────────
plot_robustness(
  coef_polr = coef_redis_top,
  coef_lpm  = coef_redis_ols,
  title_str = "Redistribution attitudes — Ordered logit AME vs OLS (robustness check)",
  file_path = file.path(params$out_reg, "coef_redis_polr_vs_OLS.png")
)


# ==============================================================
# 11.  INTERACTION PLOTS: IDEOLOGY × QUEBEC
# ==============================================================

walk2(dv_vars, dv_labels, function(dv, dv_label) {
  model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
  model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", rhs)), data = model_data)
  
  me <- ggpredict(model, terms = c("ideo_right_num [all]", "quebec_bin [0,1]"))
  
  ggplot(me, aes(x = x, y = predicted,
                 colour = as.factor(group), fill = as.factor(group))) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
    scale_colour_manual(values = c("0" = "#d6604d", "1" = "#2166ac"),
                        labels = c("0" = "Rest of Canada", "1" = "Quebec")) +
    scale_fill_manual(  values = c("0" = "#d6604d", "1" = "#2166ac"),
                        labels = c("0" = "Rest of Canada", "1" = "Quebec")) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Ideology (Left to Right)", y = "Predicted value (0–1)",
         colour = NULL, fill = NULL,
         title   = paste("Ideology x Quebec —", dv_label),
         caption = "OLS predictions for visual clarity. Shaded area = 95% CI.") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  ggsave(file.path(params$out_reg, paste0("me_ideo_", dv, ".png")),
         width = 8, height = 5, dpi = params$dpi)
})


# ==============================================================
# 12.  MODEL FIT SUMMARIES
# ==============================================================

# ── 12.1  Ordered logit — McFadden pseudo-R2 ─────────────────
fit_redis_polr <- map2_dfr(polr_models, names(polr_models), function(model, dv_label) {
  s <- summary(model)
  tibble(
    dv        = dv_label,
    pseudo_r2 = round(1 - (s$deviance / model$null.deviance), 3),
    n         = length(s$residuals)
  )
})

plot_r2(fit_redis_polr,
        r2_col    = "pseudo_r2",
        title_str = "Model fit — Redistribution attitudes (ordered logit, McFadden pseudo-R2)",
        file_path = file.path(params$out_reg, "r2_redis_polr.png"))

# ── 12.2  OLS — adjusted R2 ──────────────────────────────────
plot_r2(fit_redis_ols,
        r2_col    = "adj_r_sq",
        title_str = "Model fit — Redistribution attitudes (OLS, adjusted R2)",
        file_path = file.path(params$out_reg, "r2_redis_OLS.png"))


# ==============================================================
# 13.  DIAGNOSTICS
# ==============================================================

cat("\n========== ORDERED LOGIT: model fit ==========\n")
print(fit_redis_polr)

cat("\n========== OLS: model fit ==========\n")
print(fit_redis_ols)

cat("\n========== Most consistently significant predictors (ordered logit top-category AME) ==========\n")
coef_redis_top |>
  dplyr::filter(sig %in% c("*", "**", "***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

check_direction_agreement(coef_redis_top, coef_redis_ols,
                          "— Redistribution attitudes")

cat("\n========== OUTPUT FILES ==========\n")
cat("Bivariate contrasts:    ", file.path(params$out_reg, "bivar_contrasts_redis.csv"), "\n")
cat("AME all levels:         ", file.path(params$out_reg, "AME_all_levels_redis.csv"), "\n")
cat("Reg table polr log-odds:", file.path(params$out_reg, "regtable_redis_polr_logodds.txt"), "\n")
cat("Reg table polr AME:     ", file.path(params$out_reg, "regtable_redis_polr_AME.txt"), "\n")
cat("Reg table OLS:          ", file.path(params$out_reg, "regtable_redis_OLS.txt"), "\n")
cat("\n========== PIPELINE COMPLETE ==========\n")
