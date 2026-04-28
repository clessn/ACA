# ==============================================================
# fairness_config.R
# Single source of truth for the fairness beliefs pipeline.
#
# SOURCE THIS FIRST in every fairness script:
#   source("fairness_config.R")
#
# Defines:
#   - Output paths
#   - DV vectors and labels
#   - RHS formula (full model)
#   - Term labels
#   - vars_main (predictors shown in main figures)
#   - ggplot theme (theme_cpp)
#   - Shared plot dimensions
# ==============================================================


# ==============================================================
# 0.  PACKAGES
# ==============================================================

library(tidyverse)
library(MASS)
library(marginaleffects)
library(modelsummary)
library(sandwich)
library(lmtest)
library(ggplot2)

select <- dplyr::select
filter <- dplyr::filter


# ==============================================================
# 1.  DATA
# ==============================================================

data_path <- "data/clean_df_valid.csv"

if (!exists("df")) {
  df <- read.csv(data_path)
}

# ── Variable construction ─────────────────────────────────────
df$univ_educ_bin  <- as.integer(df$educ_group == "educUniv")
df$incomeHigh_bin <- as.integer(df$ses_income3Cat == "High")
df$quebec_x_income <- df$quebec_bin * df$incomeHigh_bin

# ── Ordered factors for polr ──────────────────────────────────
dv_raw_all <- c(
  "redis_opportunity_num", "redis_intelligence_num",
  "redis_effort_num", "redis_reasons_poor_num",
  "redis_reasons_rich_num", "redis_social_benefits_num",
  "redis_welfare_num", "redis_no_cheat_system_num"
)
for (v in dv_raw_all) {
  lvls <- sort(unique(na.omit(df[[v]])))
  df[[paste0(v, "_ord")]] <- factor(
    as.character(df[[v]]),
    levels  = as.character(lvls),
    ordered = TRUE
  )
}


# ==============================================================
# 2.  OUTPUT PATHS
# ==============================================================

base_out <- "/Users/shannondinan/Library/CloudStorage/Dropbox/_RESEARCH/_COLLABORATIONS/_GitHub/ACA/code/source_redistribution/graphs"

paths <- list(
  desc        = file.path(base_out, "descriptives_fairness"),
  reg         = file.path(base_out, "regressions_fairness"),
  nested      = file.path(base_out, "regressions_fairness", "nested_models"),
  nested_int  = file.path(base_out, "regressions_fairness", "nested_models", "interaction_plots"),
  pub_main    = file.path(base_out, "publication_fairness", "main"),
  pub_app     = file.path(base_out, "publication_fairness", "appendix"),
  rds         = file.path(base_out, "rds")   # saved model objects
)

for (p in paths) dir.create(p, recursive = TRUE, showWarnings = FALSE)

# Plot dimensions
plot_width  <- 14
plot_height <- 10
plot_dpi    <- 300


# ==============================================================
# 3.  DV DEFINITIONS
# ==============================================================

prop_vars <- c(
  "redis_opportunity_num",
  "redis_intelligence_num",
  "redis_effort_num",
  "redis_reasons_poor_num",
  "redis_reasons_rich_num"
)
prop_labels <- c(
  redis_opportunity_num  = "Equal opportunity",
  redis_intelligence_num = "Rewarded for effort & skill",
  redis_effort_num       = "Fairness of income distribution",
  redis_reasons_poor_num = "Violating outcomes (Poor)",
  redis_reasons_rich_num = "Violating outcomes (Rich)"
)

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

all_dv_vars   <- c(prop_vars, recip_vars)
all_dv_labels <- c(prop_labels, recip_labels)
dv_ord_vars   <- paste0(all_dv_vars, "_ord")

# Canonical panel order for plots
dv_order <- c(unname(prop_labels), unname(recip_labels))

# Region colours
region_colours <- c(
  "Ontario"      = "#d6604d",
  "Quebec"       = "#2166ac",
  "Alberta"      = "#4dac26",
  "Atlantic Canada" = "#984ea3"
)


# ==============================================================
# 4.  FULL MODEL RHS FORMULA
# ==============================================================
#
# Ontario = reference region.
# ses_french_bin excluded: high multicollinearity with quebec_bin (VIF >> 5).
# ses_citizenYes_bin included as control; excluded from main figures.
# ses_age used as continuous; age bins used in nested models only.

rhs <- "quebec_bin + alberta_bin + region_eastcoast_bin +
        incomeHigh_bin + ses_male_bin + ses_age + univ_educ_bin +
        employ_fulltime_bin + ses_citizenYes_bin +
        ideo_right_num + ideo_interest_politics_num +
        ideo_vote_fed_left + ideo_vote_prov_left +
        ideo_define_QC_first_bin +
        trust_social_bin + trust_inst_fed_bin + trust_inst_prov_bin"


# ==============================================================
# 5.  TERM LABELS
# ==============================================================

term_labels <- c(
  # Socio-economic
  "incomeHigh_bin"           = "Income: High",
  "ses_male_bin"             = "Male",
  "ses_age"                  = "Age",
  "univ_educ_bin"            = "Education: University+",
  "employ_fulltime_bin"      = "Employed Full-Time",
  "ses_citizenYes_bin"       = "Citizen",
  # Ideology & partisanship
  "ideo_right_num"           = "Ideology (Right)",
  "ideo_interest_politics_num" = "Political Interest",
  "ideo_vote_fed_left"       = "Fed. vote (Left scale)",
  "ideo_vote_prov_left"      = "Prov. vote (Left scale)",
  # Region & identity
  "quebec_bin"               = "Quebec",
  "alberta_bin"              = "Alberta",
  "region_eastcoast_bin"     = "Atlantic Canada",
  "ideo_define_QC_first_bin" = "Québécois identity first",
  # Trust
  "trust_social_bin"         = "Social trust",
  "trust_inst_fed_bin"       = "Trust: Federal institutions",
  "trust_inst_prov_bin"      = "Trust: Provincial institutions"
)

# Term labels for nested models (adds interaction + age bins)
term_labels_nested <- c(
  "quebec_bin"               = "Quebec",
  "region_eastcoast_bin"     = "Atlantic Canada",
  "quebec_x_income"          = "Quebec × Income (High)",
  "incomeHigh_bin"           = "Income: High",
  "ses_male_bin"             = "Male",
  "age18_34_bin"             = "Age 18–34",
  "age55plus_bin"            = "Age 55+",
  "univ_educ_bin"            = "Education: University+",
  "employ_fulltime_bin"      = "Employed Full-Time",
  "ses_citizenYes_bin"       = "Citizen",
  "ses_french_bin"           = "French-speaking",
  "ideo_define_QC_first_bin" = "Québécois identity first",
  "ideo_right_num"           = "Ideology (Right scale)",
  "ideo_vote_fed_left"       = "Fed. vote (Left scale)",
  "ideo_vote_prov_left"      = "Prov. vote (Left scale)",
  "trust_social_bin"         = "Social trust",
  "trust_inst_fed_bin"       = "Trust: Federal institutions",
  "trust_inst_prov_bin"      = "Trust: Provincial institutions"
)

# Canonical IV order for coefficient plots (bottom to top on y-axis)
iv_order <- c(
  "Income: High", "Male", "Age", "Age 18–34", "Age 55+",
  "Education: University+", "Employed Full-Time", "Citizen",
  "Ideology (Right)", "Political Interest",
  "Fed. vote (Left scale)", "Prov. vote (Left scale)",
  "Québécois identity first", "French-speaking",
  "Quebec", "Alberta", "Atlantic Canada",
  "Social trust", "Trust: Federal institutions",
  "Trust: Provincial institutions"
)

# Predictors shown in MAIN figures (controls appear in appendix only)
vars_main <- c(
  "incomeHigh_bin", "ses_male_bin", "ses_age", "univ_educ_bin",
  "employ_fulltime_bin",
  "ideo_right_num", "ideo_interest_politics_num",
  "ideo_vote_fed_left", "ideo_vote_prov_left",
  "quebec_bin", "alberta_bin", "region_eastcoast_bin",
  "ideo_define_QC_first_bin",
  "trust_social_bin", "trust_inst_fed_bin", "trust_inst_prov_bin"
)


# ==============================================================
# 6.  NESTED MODEL RHS FORMULAS
# ==============================================================
#
# Uses age bins (age18_34_bin, age55plus_bin) instead of ses_age.
# Sample held constant across M1–M5 via listwise deletion on the
# union of all RHS variables before any model is fitted.

core_demo <- "quebec_bin + region_eastcoast_bin + incomeHigh_bin +
              ses_male_bin + age18_34_bin + age55plus_bin +
              univ_educ_bin + employ_fulltime_bin + ses_citizenYes_bin"

rhs_M1 <- core_demo
rhs_M2 <- paste(core_demo, "+ ses_french_bin + ideo_define_QC_first_bin")
rhs_M3 <- paste(rhs_M2,   "+ ideo_right_num + ideo_vote_fed_left + ideo_vote_prov_left")
rhs_M4 <- paste(rhs_M3,   "+ trust_social_bin + trust_inst_fed_bin + trust_inst_prov_bin")
rhs_M5 <- paste(rhs_M4,   "+ quebec_x_income")

rhs_nested <- list(M1 = rhs_M1, M2 = rhs_M2, M3 = rhs_M3,
                   M4 = rhs_M4, M5 = rhs_M5)

nested_footnote <- paste(
  "Ordered logit (polr). Log-odds coefficients.",
  "HC1 robust SEs in parentheses.",
  "Sample held constant across M1-M5 (listwise deletion on union of all RHS vars).",
  "DV coded 0/0.33/0.66/1; highest level (= 1) = 'fair'. Ontario = reference region.",
  "M1 = Quebec + Atlantic Canada + demographics.",
  "M2 = M1 + French language + Quebecois-first identity.",
  "M3 = M2 + ideology (right scale) + federal vote + provincial vote (left scales).",
  "M4 = M3 + social trust + federal inst. trust + provincial inst. trust.",
  "M5 = M4 + Quebec x Income (High) interaction.",
  "* p<0.05  ** p<0.01  *** p<0.001"
)


# ==============================================================
# 7.  GGPLOT THEME
# ==============================================================

theme_cpp <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      text               = element_text(family = "serif"),
      axis.line          = element_line(colour = "black", linewidth = 0.4),
      axis.ticks         = element_line(colour = "black", linewidth = 0.3),
      panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      strip.background   = element_blank(),
      strip.text         = element_text(face = "bold", size = rel(0.95)),
      legend.position    = "bottom",
      legend.key.size    = unit(0.4, "cm"),
      plot.title         = element_text(face = "bold", size = rel(1.05)),
      plot.caption       = element_text(size = rel(0.8), colour = "grey40",
                                        hjust = 0, margin = margin(t = 6))
    )
}


# ==============================================================
# 8.  SHARED HELPER FUNCTIONS
# ==============================================================

# ── HC1 robust vcov ───────────────────────────────────────────
robust_vcov <- function(model) sandwich::vcovHC(model, type = "HC1")

# ── Safe file label from DV label string ──────────────────────
safe_filename <- function(x) {
  x <- gsub("[^a-zA-Z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  gsub("_$", "", x)
}

# ── Paired vcov list for modelsummary (polr safe) ─────────────
make_vcov_list <- function(models_ok) {
  vcov_list <- vector("list", length(models_ok))
  names(vcov_list) <- names(models_ok)
  for (nm in names(models_ok)) {
    vcov_list[[nm]] <- tryCatch(
      sandwich::vcovHC(models_ok[[nm]], type = "HC1"),
      error = function(e) tryCatch(vcov(models_ok[[nm]]),
                                   error = function(e2) NULL)
    )
  }
  ok <- !sapply(vcov_list, is.null)
  list(models = models_ok[ok], vcovs = vcov_list[ok])
}

# ── Coefficient plot ───────────────────────────────────────────
plot_coefs <- function(coef_df, caption_str, file_path,
                       keep_vars  = NULL,
                       ncol_facet = 2,
                       width      = plot_width,
                       height     = plot_height) {
  if (!is.null(keep_vars))
    coef_df <- coef_df |> dplyr::filter(term %in% keep_vars)

  coef_df |>
    mutate(
      term = dplyr::recode(term, !!!term_labels),
      dv   = factor(dv, levels = intersect(dv_order, unique(dv)))
    ) |>
    ggplot(aes(x = estimate,
               y = factor(term, levels = rev(iv_order)),
               color = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    scale_color_manual(values = c(
      "Positive"        = "#2166ac",
      "Negative"        = "#d6604d",
      "No clear effect" = "grey60"
    )) +
    facet_wrap(~ dv, ncol = ncol_facet, scales = "free_x") +
    labs(x = "Estimated effect (HC1 robust SEs, 95% CI)",
         y = NULL, color = NULL, caption = caption_str) +
    theme_cpp() +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold"))

  ggsave(file_path, width = width, height = height, dpi = plot_dpi)
  invisible()
}

# ── Robustness plot: polr AME vs OLS ──────────────────────────
plot_robustness <- function(coef_polr, coef_ols, caption_str, file_path,
                            keep_vars  = NULL,
                            ncol_facet = 2,
                            width      = plot_width,
                            height     = plot_height) {
  if (!is.null(keep_vars)) {
    coef_polr <- coef_polr |> dplyr::filter(term %in% keep_vars)
    coef_ols  <- coef_ols  |> dplyr::filter(term %in% keep_vars)
  }
  bind_rows(
    coef_polr |> mutate(model = "Ordered logit AME"),
    coef_ols  |> mutate(model = "OLS")
  ) |>
    mutate(
      term = dplyr::recode(term, !!!term_labels),
      dv   = factor(dv, levels = intersect(dv_order, unique(dv)))
    ) |>
    ggplot(aes(x = estimate,
               y = factor(term, levels = rev(iv_order)),
               colour = model, shape = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    scale_colour_manual(
      values = c("Ordered logit AME" = "#d6604d", "OLS" = "#2166ac")
    ) +
    facet_wrap(~ dv, ncol = ncol_facet, scales = "free_x") +
    labs(x = "Estimated effect (HC1 robust SEs, 95% CI)",
         y = NULL, colour = NULL, shape = NULL, caption = caption_str) +
    theme_cpp() +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold"))

  ggsave(file_path, width = width, height = height, dpi = plot_dpi)
  invisible()
}

cat("fairness_config.R loaded.\n")
