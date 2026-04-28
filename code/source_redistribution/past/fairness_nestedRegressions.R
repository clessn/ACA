# ==============================================================
# 13_regressions_fairness_nested.R
# Nested-model comparison for fairness belief batteries
#
# Pipeline assumptions:
#   • 00_packages.R, 01_data_prep.R, 02_helpers.R, 03_variable_definitions.R
#     have been sourced (df, params, term_labels, etc. in scope).
#   • OR: run standalone after sourcing analyses_fairness.R up to
#     section 1 (variable construction).
#
# DVs: 8 ordered categorical variables (0 / 0.33 / 0.66 / 1)
#   Proportionality (5): redis_opportunity_num, redis_intelligence_num,
#     redis_effort_num, redis_reasons_poor_num, redis_reasons_rich_num
#   Reciprocity (3): redis_social_benefits_num, redis_welfare_num,
#     redis_no_cheat_system_num
#
# Estimator: ordered logit (polr), log-odds coefficients, HC1 robust SEs
#
# FIVE-MODEL NESTED PROGRESSION (same sample across all models,
# listwise deletion on union of all RHS variables):
#
#   M1 — Quebec + Atlantic Canada effect (clean):
#          quebec_bin + region_eastcoast_bin + demographics only
#          Answers: Is there a distinct Quebec or Atlantic Canada effect
#          net of demographics? (H3)
#
#   M2 — Identity decomposition:
#          M1 + ses_french_bin + ideo_define_QC_first_bin
#          Answers: How much of the Quebec effect runs through language
#          and provincial identity? Does the quebec_bin coefficient shrink?
#
#   M3 — Add ideology & partisanship:
#          M2 + ideo_right_num + ideo_vote_fed_left
#          Answers: How much of the remaining Quebec effect is partisan/
#          ideological in character?
#
#   M4 — Add trust:
#          M3 + trust_social_bin + trust_inst_fed_bin + trust_inst_prov_bin
#          Answers: How much does institutional and social trust explain
#          on top of ideology? (Key for reciprocity hypotheses H6.)
#
#   M5 — Quebec × Income interaction:
#          M4 + quebec_bin × incomeHigh_bin
#          Answers: Is the Quebec effect heterogeneous by income?
#          (Tests whether H1 operates differently inside Quebec.)
#
# Output: one .txt regression table per DV (8 tables total)
#         one predicted-values plot per DV for the Quebec × income
#         interaction from M5 (8 plots total)
#
# Hypotheses being tested:
#   H1:  High income → more likely to evaluate proportionality as fair
#   H2:  Income has weaker / null effect on reciprocity beliefs
#   H3:  Quebec / Atlantic Canada → more likely to evaluate reciprocity as fair
#   H4a: Québécois-first identity → more positive on reciprocity (in Quebec)
#   H5a: Left ideology → less likely to evaluate proportionality as fair
#   H5b: Left ideology → more likely to evaluate reciprocity as fair
#   H6:  Higher trust → more likely to evaluate reciprocity as fair
# ==============================================================


# ==============================================================
# 0.  SETUP
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

# ── 0.1  Paths ────────────────────────────────────────────────
# Inherits params from pipeline, or define locally if standalone
if (!exists("params")) {
  params <- list(
    data_path   = "data/clean_df_valid.csv",
    out_reg     = "code/source_redistribution/graphs/regressions_fairness",
    dpi         = 300,
    plot_width  = 10,
    plot_height = 6
  )
  df <- read.csv(params$data_path)
  
  # Reproduce necessary variable construction if standalone
  df$univ_educ_bin  <- as.integer(df$educ_group == "educUniv")
  df$educBHS_bin    <- as.integer(df$educ_group == "educBHS")
  df$incomeHigh_bin <- as.integer(df$ses_income3Cat == "High")
  
  for (v in c("redis_opportunity_num", "redis_intelligence_num",
              "redis_effort_num", "redis_reasons_poor_num",
              "redis_reasons_rich_num", "redis_social_benefits_num",
              "redis_welfare_num", "redis_no_cheat_system_num")) {
    observed_levels <- sort(unique(na.omit(df[[v]])))
    df[[paste0(v, "_ord")]] <- factor(
      as.character(df[[v]]),
      levels  = as.character(observed_levels),
      ordered = TRUE
    )
  }
}

nested_out_fair  <- "/Users/shannondinan/Library/CloudStorage/Dropbox/_RESEARCH/_COLLABORATIONS/_GitHub/ACA/code/source_redistribution/graphs/regressions_fairness/nested_models"
nested_plots_out <- file.path(nested_out_fair, "interaction_plots")
dir.create(nested_out_fair,  recursive = TRUE, showWarnings = FALSE)
dir.create(nested_plots_out, recursive = TRUE, showWarnings = FALSE)


# ==============================================================
# 1.  DV DEFINITIONS
# ==============================================================

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


# ==============================================================
# 2.  INTERACTION TERM
# ==============================================================

df$quebec_x_income <- df$quebec_bin * df$incomeHigh_bin


# ==============================================================
# 3.  NESTED RHS FORMULAS
# ==============================================================

# ── Shared demographic core (in every model) ──────────────────
#   incomeHigh_bin:      material self-interest (H1, H2)
#   ses_male_bin:        gender control
#   age18_34_bin:        age group (reference: 35–54)
#   age55plus_bin:       age group (reference: 35–54)
#   educBHS_bin:         below high school (reference: high school)
#   univ_educ_bin:       university+ (reference: high school)
#   employ_fulltime_bin: employment control
#   ses_citizenYes_bin:  citizenship control
#   quebec_bin:          Quebec vs. all other regions (focal variable)

core_demo <- "quebec_bin + region_eastcoast_bin + incomeHigh_bin +
              ses_male_bin + age18_34_bin + age55plus_bin +
              univ_educ_bin +
              employ_fulltime_bin + ses_citizenYes_bin"

# M1: Quebec effect, demographics only
rhs_M1 <- core_demo

# M2: + identity decomposition
#   ses_french_bin:            French-speaking (language mechanism)
#   ideo_define_QC_first_bin:  Québécois-first identity (H4a)
rhs_M2 <- paste(core_demo,
                "+ ses_french_bin + ideo_define_QC_first_bin")

# M3: + ideology & partisanship
#   ideo_right_num:      left–right self-placement, 0=left, 1=right (H5a, H5b)
#   ideo_vote_fed_left:  federal vote left scale, 0=right/none, 1=NDP (H5a, H5b)
rhs_M3 <- paste(rhs_M2,
                "+ ideo_right_num + ideo_vote_fed_left + ideo_vote_prov_left")

# M4: + trust (H6)
#   trust_social_bin:    generalised social trust
#   trust_inst_fed_bin:  trust in federal institutions
#   trust_inst_prov_bin: trust in provincial institutions
rhs_M4 <- paste(rhs_M3,
                "+ trust_social_bin + trust_inst_fed_bin + trust_inst_prov_bin")

# M5: + Quebec × income interaction
rhs_M5 <- paste(rhs_M4, "+ quebec_x_income")

rhs_list_fair <- list(M1 = rhs_M1, M2 = rhs_M2, M3 = rhs_M3,
                      M4 = rhs_M4, M5 = rhs_M5)


# ==============================================================
# 4.  TERM LABELS
# ==============================================================

term_labels_fair <- c(
  # Focal variables
  "quebec_bin"               = "Quebec",
  "region_eastcoast_bin"     = "Atlantic Canada",
  # Interaction
  "quebec_x_income"          = "Quebec × Income (High)",
  # Material self-interest
  "incomeHigh_bin"           = "Income: High",
  # Demographics
  "ses_male_bin"             = "Male",
  "age18_34_bin"             = "Age 18–34",
  "age55plus_bin"            = "Age 55+",
  "univ_educ_bin"            = "Education: University+",
  "employ_fulltime_bin"      = "Employed Full-Time",
  "ses_citizenYes_bin"       = "Citizen",
  # Identity decomposition
  "ses_french_bin"           = "French-speaking",
  "ideo_define_QC_first_bin" = "Québécois identity first",
  # Ideology & partisanship
  "ideo_right_num"           = "Ideology (Right scale)",
  "ideo_vote_fed_left"       = "Fed. vote (Left scale)",
  "ideo_vote_prov_left"      = "Prov. vote (Left scale)",
  # Trust
  "trust_social_bin"         = "Social trust",
  "trust_inst_fed_bin"       = "Trust: Federal institutions",
  "trust_inst_prov_bin"      = "Trust: Provincial institutions"
)


# ==============================================================
# 5.  HELPER: ROBUST VCOV (HC1)
# ==============================================================

robust_vcov <- function(model) {
  sandwich::vcovHC(model, type = "HC1")
}


# ==============================================================
# 6.  HELPER: CPP JOURNAL THEME
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
# 7.  HELPER: FIT FIVE POLR MODELS FOR ONE DV
#     Sample is held constant across M1–M5 via listwise deletion
#     on the union of ALL RHS variables across all five models.
# ==============================================================

fit_five_polr <- function(dv_ord_var) {
  
  # Union of all variables across all five models
  all_rhs_vars <- unique(unlist(lapply(rhs_list_fair, function(r) {
    all.vars(as.formula(paste("~", r)))
  })))
  
  model_vars <- unique(c(dv_ord_var, all_rhs_vars,
                         "quebec_bin", "incomeHigh_bin", "quebec_x_income"))
  
  # Single listwise-deleted dataset — same for M1 through M5
  model_data <- df |>
    dplyr::select(all_of(intersect(model_vars, names(df)))) |>
    drop_na()
  
  cat("    N (complete cases):", nrow(model_data), "\n")
  
  lapply(rhs_list_fair, function(rhs) {
    tryCatch(
      MASS::polr(
        as.formula(paste(dv_ord_var, "~", rhs)),
        data  = model_data,
        Hess  = TRUE
      ),
      error = function(e) {
        message("    Model failed (", dv_ord_var, "): ", conditionMessage(e))
        NULL
      }
    )
  })
}


# ==============================================================
# 8.  HELPER: SAVE NESTED REGRESSION TABLE
#     Log-odds coefficients + HC1 SEs + stars.
#     Log-odds preferred over AMEs for nested comparison because
#     AMEs conflate coefficient changes with scale changes across
#     models as control sets expand.
# ==============================================================

save_nested_regtable_polr <- function(models, dv_label, file_path) {
  
  # ── Remove failed models, then build a PAIRED vcov list ─────
  # polr objects do not expose a standard $residuals slot, so
  # sandwich::vcovHC() fails on them directly.  We instead use
  # the sandwich approach via MASS::polr + vcov(model) which
  # gives the inverse-Hessian SE, and pass vcov = "classical"
  # to modelsummary (letting it use polr's own vcov), OR we
  # supply a named list of vcov matrices computed via the
  # lmtest/sandwich coeftest pathway.
  #
  # Safest route for polr: use modelsummary's built-in
  # "HC3"-style via vcov = "HC3" string, which modelsummary
  # handles internally for polr via sandwich.  If that fails,
  # fall back to the model's own Hessian-based vcov.
  
  models_ok <- Filter(Negate(is.null), models)
  if (length(models_ok) == 0) {
    message("  All models failed for ", dv_label, " — skipping table.")
    return(invisible(NULL))
  }
  
  # Build vcov list strictly paired to models_ok.
  # For polr, sandwich vcovHC requires the model to have been
  # fitted with Hess = TRUE (it is), and we call it via
  # sandwich::vcovHC with type = "HC1".
  vcov_list <- vector("list", length(models_ok))
  names(vcov_list) <- names(models_ok)
  for (nm in names(models_ok)) {
    vcov_list[[nm]] <- tryCatch(
      sandwich::vcovHC(models_ok[[nm]], type = "HC1"),
      error = function(e) {
        # Fallback: use the model's own Hessian-based vcov
        tryCatch(vcov(models_ok[[nm]]),
                 error = function(e2) NULL)
      }
    )
  }
  
  # Drop any model whose vcov is NULL and keep lists in sync
  ok_idx    <- !sapply(vcov_list, is.null)
  models_ok <- models_ok[ok_idx]
  vcov_list <- vcov_list[ok_idx]
  
  if (length(models_ok) == 0) {
    message("  All vcov computations failed for ", dv_label, " — skipping.")
    return(invisible(NULL))
  }
  
  tryCatch({
    modelsummary(
      models_ok,
      estimate  = "{estimate}{stars}",
      statistic = "({std.error})",
      vcov      = vcov_list,
      coef_map  = term_labels_fair,
      gof_map   = c("nobs", "logLik", "AIC"),
      output    = file_path,
      title     = paste("Ordered logit —", dv_label),
      notes     = paste(
        "Ordered logit (polr). Log-odds coefficients.",
        "HC1 robust SEs in parentheses.",
        "Sample held constant across M1–M5 (listwise deletion on union of all RHS vars).",
        "DV coded 0 / 0.33 / 0.66 / 1; highest level (= 1) = 'fair'.",
        "Ontario = reference region.",
        "M1 = Quebec + demographics.",
        "M2 = M1 + French language + Québécois-first identity.",
        "M3 = M2 + ideology (right scale) + federal vote (left scale).",
        "M4 = M3 + social trust + federal inst. trust + provincial inst. trust.",
        "M5 = M4 + Quebec × Income (High) interaction.",
        "* p<0.05  ** p<0.01  *** p<0.001"
      )
    )
    cat("  Table saved:", file_path, "\n")
  }, error = function(e) {
    cat("  TABLE ERROR (", dv_label, "):", conditionMessage(e), "\n")
  })
}


# ==============================================================
# 9.  HELPER: INTERACTION PLOT — Quebec × Income (from M5)
#     X-axis: income level (Low/Mid vs. High)
#     Lines:  Quebec vs. Rest of Canada
#     Y-axis: predicted P(response = 1), the "fair" top category
#     CIs:    HC1 delta-method via avg_predictions()
# ==============================================================

plot_interaction_polr <- function(model_M5, dv_label, file_path) {
  
  if (is.null(model_M5)) {
    message("  Skipping interaction plot for ", dv_label, " — M5 is NULL.")
    return(invisible(NULL))
  }
  
  model_data <- model_M5$model
  top_level  <- nlevels(model_data[[1]])   # polr orders ascending → last = 1
  
  # Identify other covariates (not the focal interaction vars or DV)
  other_vars <- setdiff(
    names(model_data),
    c(names(model_data)[1],
      "quebec_bin", "incomeHigh_bin", "quebec_x_income")
  )
  
  # Hold other covariates at mean (numeric) or mode (binary / factor)
  modal_val <- function(x) {
    tab <- table(x, useNA = "no")
    as.numeric(names(tab)[which.max(tab)])
  }
  baseline <- lapply(model_data[other_vars], function(col) {
    if (is.numeric(col)) mean(col, na.rm = TRUE) else modal_val(col)
  })
  
  # Prediction grid: 2 × 2 cells
  grid <- expand.grid(quebec_bin = c(0, 1), incomeHigh_bin = c(0, 1))
  grid$quebec_x_income <- grid$quebec_bin * grid$incomeHigh_bin
  for (vn in names(baseline)) grid[[vn]] <- baseline[[vn]]
  
  # Point predictions
  preds    <- predict(model_M5, newdata = grid, type = "probs")
  pred_top <- if (is.matrix(preds)) preds[, top_level] else preds
  
  plot_df <- grid |>
    mutate(
      pred_top = pred_top,
      Quebec   = factor(quebec_bin,     labels = c("Rest of Canada", "Quebec")),
      Income   = factor(incomeHigh_bin, labels = c("Low / Mid", "High"))
    )
  
  # Delta-method CIs via avg_predictions
  pred_ci <- tryCatch({
    top_label <- levels(model_data[[1]])[top_level]
    avg_predictions(
      model_M5,
      variables = list(quebec_bin = c(0, 1), incomeHigh_bin = c(0, 1)),
      vcov      = \(x) sandwich::vcovHC(x, type = "HC1")
    ) |>
      as_tibble() |>
      dplyr::filter(as.character(group) == top_label) |>
      mutate(
        Quebec = factor(quebec_bin,     labels = c("Rest of Canada", "Quebec")),
        Income = factor(incomeHigh_bin, labels = c("Low / Mid", "High"))
      )
  }, error = function(e) {
    message("  CI computation failed for ", dv_label, ": ", conditionMessage(e))
    NULL
  })
  
  p <- ggplot(plot_df,
              aes(x = Income, y = pred_top, colour = Quebec, group = Quebec)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 3.5) +
    {
      if (!is.null(pred_ci)) {
        geom_errorbar(
          data = pred_ci,
          aes(x = Income, y = estimate,
              ymin = conf.low, ymax = conf.high, colour = Quebec),
          width = 0.08, linewidth = 0.6
        )
      }
    } +
    scale_colour_manual(
      values = c("Rest of Canada" = "#d6604d", "Quebec" = "#2166ac")
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    labs(
      x       = "Income level",
      y       = "Predicted P(response = 1 ['fair'])",
      colour  = NULL,
      title   = dv_label,
      caption = paste(
        "Predicted probability of the top ('fair') response from M5 (ordered logit).",
        "All other covariates held at mean / mode.",
        "Error bars = HC1 delta-method 95% CI."
      )
    ) +
    theme_cpp()
  
  ggsave(file_path, plot = p,
         width  = params$plot_width,
         height = params$plot_height,
         dpi    = params$dpi)
  
  invisible(p)
}


# ==============================================================
# 10.  MAIN LOOP
# ==============================================================

cat("\n========== FAIRNESS BELIEFS — NESTED MODEL COMPARISON ==========\n")
cat("DVs:", length(all_dv_vars), "| Models per DV: 5\n\n")

for (i in seq_along(all_dv_vars)) {
  
  dv_raw <- all_dv_vars[i]
  dv_ord <- dv_ord_vars[i]
  dv_lbl <- all_dv_labels[[dv_raw]]
  
  # Determine type for console output
  dv_type <- if (dv_raw %in% prop_vars) "Proportionality" else "Reciprocity"
  
  cat("─────────────────────────────────────────────────────\n")
  cat("[", dv_type, "]  DV:", dv_lbl, "\n")
  
  # ── 10.1  Fit five models ──────────────────────────────────
  models <- fit_five_polr(dv_ord_var = dv_ord)
  names(models) <- paste0("M", 1:5)
  
  # ── 10.2  Regression table ─────────────────────────────────
  safe_label <- gsub("[^a-zA-Z0-9_]", "_", dv_lbl)
  safe_label <- gsub("_+", "_", safe_label)
  safe_label <- gsub("_$", "",  safe_label)
  
  table_path <- file.path(
    nested_out_fair,
    paste0("nested_polr_", tolower(dv_type), "_", safe_label, ".txt")
  )
  
  save_nested_regtable_polr(
    models    = models,
    dv_label  = dv_lbl,
    file_path = table_path
  )
  
  # ── 10.3  Interaction plot (M5) ────────────────────────────
  plot_path <- file.path(
    nested_plots_out,
    paste0("interaction_quebec_income_", safe_label, ".png")
  )
  
  tryCatch(
    plot_interaction_polr(
      model_M5  = models[["M5"]],
      dv_label  = dv_lbl,
      file_path = plot_path
    ),
    error = function(e) cat("  PLOT ERROR:", conditionMessage(e), "\n")
  )
}

cat("\n========== PIPELINE COMPLETE ==========\n")
cat("Tables: ", nested_out_fair, "\n")
cat("Plots:  ", nested_plots_out, "\n")
