# ==============================================================
# Fairness Attitudes — Quebec-Focused Model Comparison
# Four-region Canadian survey: Alberta, Ontario, Quebec, Atlantic Canada
#
# Research questions:
#   1. Quebec vs. other regions (isolating geographic effect, with
#      Alberta and Atlantic Canada included as additional region
#      dummies; Ontario = omitted reference)
#   2. Interaction between Quebec and income
#   3. Effect of partisanship (ideo_vote_fed_left)
#   4. Effects of social and institutional trust
#
# Six models per DV (ordered logit, polr):
#   M1 — Core: Region dummies + demographics + French identity
#   M2 — M1 + trust (social + federal + provincial)
#   M3 — M1 + ideology / partisanship
#   M4 — Full: M1 + trust + ideology
#   M5 — Full + Quebec × income interaction
#   M6 — Full + Quebec × income, but WITHOUT the identity-overlap
#        controls (ses_french_bin, ideo_define_QC_first_bin), so
#        the Quebec coefficient captures the total Quebec effect
#        (including its identity-mediated component) rather than
#        the residual after partialling those out.
#
# Output (per DV):
#   - Regression table (.txt) with stars
#   - Predicted values plot for Quebec × income interaction
# ==============================================================


# ==============================================================
# 0.  SETUP
# ==============================================================

library(tidyverse)
library(MASS)           # polr()
library(marginaleffects)
library(modelsummary)
library(sandwich)
library(lmtest)
library(ggplot2)
library(patchwork)

# Resolve MASS namespace conflicts
select <- dplyr::select
filter <- dplyr::filter

# ── 0.1  Paths ────────────────────────────────────────────────
params <- list(
  data_path  = "data/clean_df_valid.csv",
  out_tables = "code/source_redistribution/graphs/quebec_models/tables",
  out_plots  = "code/source_redistribution/graphs/quebec_models/plots",
  dpi        = 300,
  plot_width  = 10,
  plot_height = 6
)

dir.create(params$out_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(params$out_plots,  recursive = TRUE, showWarnings = FALSE)

# ── 0.2  Load data ────────────────────────────────────────────
df <- read.csv(params$data_path)


# ==============================================================
# 1.  VARIABLE CONSTRUCTION
# ==============================================================

# ── 1.1  Education binaries ───────────────────────────────────
df$univ_educ_bin <- as.integer(df$educ_group == "educUniv")
df$educBHS_bin   <- as.integer(df$educ_group == "educBHS")
# Reference category: educHS (high school), omitted

# ── 1.2  Income binary ────────────────────────────────────────
df$incomeHigh_bin <- as.integer(df$ses_income3Cat == "High")

# ── 1.3  Region factor (for display) ──────────────────────────
df$ses_region_cat <- factor(
  dplyr::recode(df$ses_region_cat, "East Coast" = "Atlantic Canada"),
  levels = c("Ontario", "Quebec", "Alberta", "Atlantic Canada")
)

# ── 1.4  Ideology / partisanship variables ────────────────────
#   Already constructed in the cleaning script as 0–1 numeric scales:
#
#   ideo_vote_fed_left  — federal vote, "left" direction
#     NDP = 1, Green = 0.8, Liberal = 0.6, BQ = 0.4, CPC = 0.2,
#     Other/Did not vote = 0
#
#   ideo_vote_prov_left — provincial vote, "left" direction
#     (parallel coding to federal)
#
#   ideo_right_num      — left-right self-placement, "right" direction
#     ideo_left_right_1: 0 → 0, 1 → 0.1, 2 → 0.2, …, 10 → 1
#     Higher values = more right-wing (opposite direction from the two
#     vote-based scales above — keep this in mind when interpreting
#     coefficients).
#
#   Variables expected as: df$ideo_vote_fed_left, df$ideo_vote_prov_left,
#     df$ideo_right_num. (No recoding here — verified from cleaning code.)

# ── 1.5  Quebec × income interaction term (for M5) ────────────
#   Created explicitly so modelsummary labels it cleanly.
#   Only Quebec × income is included — Quebec is the focal region of
#   the study; replicating the interaction for Alberta or East Coast
#   has no theoretical motivation.
df$quebec_x_income <- df$quebec_bin * df$incomeHigh_bin


# ==============================================================
# 2.  DV DEFINITIONS
# ==============================================================

# ── 2.1  Proportionality DVs ──────────────────────────────────
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

all_dv_vars   <- c(prop_vars, recip_vars)
all_dv_labels <- c(prop_labels, recip_labels)

# ── 2.3  Ordered-factor versions (required by polr) ───────────
for (v in all_dv_vars) {
  observed_levels <- sort(unique(na.omit(df[[v]])))
  df[[paste0(v, "_ord")]] <- factor(
    as.character(df[[v]]),
    levels  = as.character(observed_levels),
    ordered = TRUE
  )
}
dv_ord_vars <- paste0(all_dv_vars, "_ord")


# ==============================================================
# 3.  MODEL SPECIFICATIONS
# ==============================================================

# ── 3.1  RHS formula strings ──────────────────────────────────
#
#   Core demographics shared by all models:
#     - quebec_bin:              Quebec (reference: Ontario)
#     - alberta_bin:             Alberta (reference: Ontario)
#     - region_eastcoast_bin:    Atlantic Canada (reference: Ontario)
#     - incomeHigh_bin:          Income (High vs. Low/Mid)
#     - age18_34_bin:            Age 18–34 (reference: 35–54)
#     - age55plus_bin:           Age 55+ (reference: 35–54)
#     - educBHS_bin:             Below high school (reference: high school)
#     - univ_educ_bin:           University+ (reference: high school)
#     - ses_citizenYes_bin:      Canadian citizen
#     - ses_male_bin:            Male (reference: Female / other)
#     - employ_fulltime_bin:     Employed full-time (reference: not full-time)
#     - ses_french_bin:          French-speaking
#     - ideo_define_QC_first_bin: Identifies as Québécois first
#
#   M1: Core only
rhs_M1 <- "quebec_bin + alberta_bin + region_eastcoast_bin +
            incomeHigh_bin +
            age18_34_bin + age55plus_bin +
            educBHS_bin + univ_educ_bin +
            ses_citizenYes_bin + ses_male_bin + employ_fulltime_bin +
            ses_french_bin +
            ideo_define_QC_first_bin"

#   M2: Core + trust
rhs_M2 <- paste(rhs_M1,
                "+ trust_social_bin + trust_inst_fed_bin + trust_inst_prov_bin")

#   M3: Core + ideology / partisanship
rhs_M3 <- paste(rhs_M1,
                "+ ideo_vote_fed_left + ideo_vote_prov_left + ideo_right_num")

#   M4: Full (core + trust + ideology / partisanship)
rhs_M4 <- paste(rhs_M1,
                "+ trust_social_bin + trust_inst_fed_bin + trust_inst_prov_bin",
                "+ ideo_vote_fed_left + ideo_vote_prov_left + ideo_right_num")

#   M5: Full + Quebec × income interaction
#   Note: main effects (quebec_bin, incomeHigh_bin) already in M4;
#   we add the explicit interaction term.
rhs_M5 <- paste(rhs_M4, "+ quebec_x_income")

#   M6: Same RHS as M5 but with the identity-overlap controls removed
#       (ses_french_bin, ideo_define_QC_first_bin). Use this to read
#       the *total* Quebec effect — i.e. including the share that runs
#       through French language and Québécois-first identity, rather
#       than only the residual after partialling those out.
rhs_M6_core <- "quebec_bin + alberta_bin + region_eastcoast_bin +
                 incomeHigh_bin +
                 age18_34_bin + age55plus_bin +
                 educBHS_bin + univ_educ_bin +
                 ses_citizenYes_bin + ses_male_bin + employ_fulltime_bin"

rhs_M6 <- paste(rhs_M6_core,
                "+ trust_social_bin + trust_inst_fed_bin + trust_inst_prov_bin",
                "+ ideo_vote_fed_left + ideo_vote_prov_left + ideo_right_num",
                "+ quebec_x_income")

rhs_list  <- list(M1 = rhs_M1, M2 = rhs_M2, M3 = rhs_M3,
                  M4 = rhs_M4, M5 = rhs_M5, M6 = rhs_M6)
model_names <- names(rhs_list)

# ── 3.2  Human-readable coefficient labels ────────────────────
term_labels <- c(
  "quebec_bin"               = "Quebec",
  "alberta_bin"              = "Alberta",
  "region_eastcoast_bin"     = "Atlantic Canada",
  "incomeHigh_bin"           = "Income: High",
  "age18_34_bin"             = "Age 18–34",
  "age55plus_bin"            = "Age 55+",
  "educBHS_bin"              = "Education: Below HS",
  "univ_educ_bin"            = "Education: University+",
  "ses_citizenYes_bin"       = "Citizen",
  "ses_male_bin"             = "Male",
  "employ_fulltime_bin"      = "Employed full-time",
  "ses_french_bin"           = "French-speaking",
  "ideo_define_QC_first_bin" = "Québécois identity first",
  "trust_social_bin"         = "Social trust",
  "trust_inst_fed_bin"       = "Trust: Federal institutions",
  "trust_inst_prov_bin"      = "Trust: Provincial institutions",
  "ideo_vote_fed_left"       = "Federal vote (Left scale)",
  "ideo_vote_prov_left"      = "Provincial vote (Left scale)",
  "ideo_right_num"           = "Self-placement (Right scale)",
  "quebec_x_income"          = "Quebec × Income (High)"
)


# ==============================================================
# 4.  HELPER FUNCTIONS
# ==============================================================

# ── 4.1  CPP journal theme ────────────────────────────────────
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

# ── 4.2  Fit five polr models for one DV ──────────────────────
fit_five_models <- function(dv_ord_var, dv_raw_var) {
  lapply(rhs_list, function(rhs) {
    model_vars <- c(dv_ord_var,
                    all.vars(as.formula(paste("~", rhs))),
                    # ensure interaction components always present
                    "quebec_bin", "incomeHigh_bin", "quebec_x_income")
    model_vars <- unique(model_vars)
    model_data <- df |>
      dplyr::select(all_of(model_vars)) |>
      drop_na()
    tryCatch(
      MASS::polr(as.formula(paste(dv_ord_var, "~", rhs)),
                 data = model_data, Hess = TRUE),
      error = function(e) {
        message("Model failed for ", dv_ord_var, ": ", conditionMessage(e))
        NULL
      }
    )
  })
}

# ── 4.3  Predicted probability plot — Quebec × income (M5) ────
#   X-axis: income level (Low/Mid vs. High)
#   Two lines: Quebec vs. Rest of Canada
#   Y-axis: predicted P(response = 1), the "fair" top category
plot_interaction <- function(model_M5, dv_label, file_path) {
  
  if (is.null(model_M5)) {
    message("Skipping interaction plot for ", dv_label, " — model is NULL")
    return(invisible(NULL))
  }
  
  # Build a prediction grid:
  #   - quebec_bin:    0 (Rest of Canada) vs. 1 (Quebec)
  #   - incomeHigh_bin: 0 (Low/Mid) vs. 1 (High)
  #   - quebec_x_income = quebec_bin * incomeHigh_bin
  #   - all other covariates held at their means/modes
  model_data <- model_M5$model
  
  # Helper: mode for binary vars
  modal_value <- function(x) {
    tab <- table(x, useNA = "no")
    as.numeric(names(tab)[which.max(tab)])
  }
  
  # Covariate means / modes (excluding the interaction variables)
  other_vars <- setdiff(
    names(model_data),
    c(names(model_data)[1],          # DV (first col in polr model frame)
      "quebec_bin", "incomeHigh_bin", "quebec_x_income")
  )
  
  baseline <- lapply(model_data[other_vars], function(col) {
    if (is.numeric(col)) mean(col, na.rm = TRUE)
    else modal_value(as.numeric(col))
  })
  
  grid <- expand.grid(
    quebec_bin     = c(0, 1),
    incomeHigh_bin = c(0, 1)
  )
  grid$quebec_x_income <- grid$quebec_bin * grid$incomeHigh_bin
  
  # Attach baseline covariates
  for (vname in names(baseline)) {
    grid[[vname]] <- baseline[[vname]]
  }
  
  # Predicted probabilities across all response levels
  preds <- predict(model_M5, newdata = grid, type = "probs")
  
  # Identify top level (= 1 = "fair")
  top_level_col <- ncol(preds)   # polr orders levels ascending
  
  plot_df <- grid |>
    mutate(
      pred_top   = preds[, top_level_col],
      Quebec     = factor(quebec_bin,     labels = c("Rest of Canada", "Quebec")),
      Income     = factor(incomeHigh_bin, labels = c("Low / Mid income", "High income"))
    )
  
  # Approximate SE via delta method using marginaleffects
  # We compute avg predictions at each cell for CIs
  pred_ci <- tryCatch({
    avg_predictions(
      model_M5,
      variables  = list(quebec_bin = c(0, 1), incomeHigh_bin = c(0, 1)),
      vcov       = \(x) sandwich::vcovHC(x, type = "HC1")
    ) |>
      as_tibble() |>
      dplyr::filter(as.character(group) == levels(model_M5$model[[1]])[top_level_col]) |>
      mutate(
        Quebec = factor(quebec_bin,     labels = c("Rest of Canada", "Quebec")),
        Income = factor(incomeHigh_bin, labels = c("Low / Mid income", "High income"))
      )
  }, error = function(e) NULL)
  
  # ── Plot ──────────────────────────────────────────────────
  p <- ggplot(plot_df,
              aes(x = Income, y = pred_top,
                  colour = Quebec, group = Quebec)) +
    geom_line(linewidth = 0.9, linetype = "solid") +
    geom_point(size = 3.5) +
    {
      if (!is.null(pred_ci)) {
        geom_errorbar(
          data = pred_ci,
          aes(x     = Income,
              y     = estimate,
              ymin  = conf.low,
              ymax  = conf.high,
              colour = Quebec),
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
      y       = "Predicted P(response = 1)",
      colour  = NULL,
      title   = dv_label,
      caption = paste(
        "Predicted probability of the 'fair' (= 1) response from M5 (ordered logit).",
        "All other covariates held at their mean/mode.",
        "Error bars = HC1 delta-method 95% CI."
      )
    ) +
    theme_cpp() +
    theme(legend.position = "bottom")
  
  ggsave(file_path,
         plot  = p,
         width = params$plot_width,
         height = params$plot_height,
         dpi   = params$dpi)
  
  invisible(p)
}


# ==============================================================
# 5.  MAIN LOOP — fit models, write tables, draw plots
# ==============================================================

cat("\n========== QUEBEC-FOCUSED MODEL COMPARISON ==========\n")
cat("DVs:", length(all_dv_vars), "| Models per DV: 5\n\n")

for (i in seq_along(all_dv_vars)) {
  
  dv_raw  <- all_dv_vars[i]
  dv_ord  <- dv_ord_vars[i]
  dv_lbl  <- all_dv_labels[[dv_raw]]
  
  cat("─────────────────────────────────────────────────────\n")
  cat("DV:", dv_lbl, "\n")
  
  # ── 5.1  Fit five models ───────────────────────────────────
  models <- fit_five_models(dv_ord_var = dv_ord, dv_raw_var = dv_raw)
  names(models) <- paste0("M", seq_along(models))
  
  # Drop any models that failed
  models_ok <- Filter(Negate(is.null), models)
  
  if (length(models_ok) == 0) {
    cat("  All models failed — skipping.\n")
    next
  }
  
  # ── 5.2  Regression table (.txt) ──────────────────────────
  #   Log-odds coefficients with stars; SEs in parentheses.
  safe_label <- gsub("[^a-zA-Z0-9_]", "_", dv_lbl)   # file-safe name
  
  table_path <- file.path(
    params$out_tables,
    paste0("regtable_", safe_label, ".txt")
  )
  
  tryCatch({
    modelsummary(
      models_ok,
      estimate  = "{estimate}{stars}",
      statistic = "({std.error})",
      coef_map  = term_labels,
      gof_map   = c("nobs", "logLik", "AIC"),
      output    = table_path,
      title     = paste("Ordered logit —", dv_lbl),
      notes     = paste(
        "Ordered logit (polr). Log-odds coefficients. SEs in parentheses.",
        "DVs coded 0 / 0.33 / 0.66 / 1; highest level (= 1) = 'fair'.",
        "Region dummies: Ontario = omitted reference.",
        "Controls include sex (Male) and full-time employment.",
        "M1 = Core; M2 = M1 + Trust; M3 = M1 + Ideology;",
        "M4 = Full (M1 + Trust + Ideology); M5 = M4 + Quebec × Income;",
        "M6 = M5 without ses_french_bin and ideo_define_QC_first_bin",
        "(isolates total Quebec effect, incl. identity-mediated component).",
        "* p<0.05  ** p<0.01  *** p<0.001"
      )
    )
    cat("  Table saved:", table_path, "\n")
  }, error = function(e) {
    cat("  TABLE ERROR:", conditionMessage(e), "\n")
  })
  
  # ── 5.3  Interaction plot (M5 only) ───────────────────────
  if (!is.null(models[["M5"]])) {
    plot_path <- file.path(
      params$out_plots,
      paste0("interaction_quebec_income_", safe_label, ".png")
    )
    tryCatch({
      plot_interaction(
        model_M5  = models[["M5"]],
        dv_label  = dv_lbl,
        file_path = plot_path
      )
      cat("  Interaction plot saved:", plot_path, "\n")
    }, error = function(e) {
      cat("  PLOT ERROR:", conditionMessage(e), "\n")
    })
  } else {
    cat("  M5 failed — interaction plot skipped.\n")
  }
}

cat("\n========== PIPELINE COMPLETE ==========\n")
cat("Tables: ", params$out_tables, "\n")
cat("Plots:  ", params$out_plots,  "\n")
