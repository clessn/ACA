# ==============================================================
# 12_regressions_tradeoff_nested.R  [REVISED]
# Nested-model comparison for budget tradeoff batteries
#
# Pipeline assumptions:
#   • 00_packages.R, 01_data_prep.R, 02_helpers.R, 03_variable_definitions.R
#     have been sourced (df, params, batteries, term_labels, etc. in scope).
#
# DVs: binary logit (first-choice preference and preference intensity)
#   organised by battery (general priorities, taxation, green economy,
#   child care, home care, target-group tradeoffs).
#
# Estimator: binary logit, log-odds coefficients, HC1 robust SEs
#
# SIX-MODEL NESTED PROGRESSION (same sample across all models,
# listwise deletion on union of all RHS variables):
#
#   M1 — Quebec effect (clean):
#          quebec_bin + other regions + demographics only
#          Answers: Is there a distinct Quebec effect net of demographics?
#          All four regions enter additively; Ontario = reference.
#
#   M2 — Identity decomposition:
#          M1 + ideo_define_QC_first_bin
#          Answers: How much of the Quebec effect runs through provincial
#          identity? Does the quebec_bin coefficient shrink?
#          Note: ses_french_bin excluded from this project (collinearity
#          with quebec_bin already documented — see params$out_reg note).
#
#   M3 — Add ideology & partisanship:
#          M2 + ideo_right_num + vote_PLC_bin + vote_PCC_bin
#             + ideo_vote_fed_left + ideo_vote_prov_left
#          Answers: How much of the remaining Quebec effect is partisan
#          or ideological in character?
#
#   M4 — Add trust:
#          M3 + trust_inst_fed_bin + trust_inst_prov_bin
#          Answers: How much does institutional trust explain on top of
#          ideology? (H8: trust → support for spending under constraint.)
#
#   M5 — Quebec × Income interaction:
#          M4 + quebec_bin × incomeHigh_bin
#          Answers: Is the Quebec effect on tradeoff preferences
#          heterogeneous by income?
#
#   M6 — Total Quebec effect (without identity):
#          Same as M5 but WITHOUT ideo_define_QC_first_bin.
#          The quebec_bin coefficient now captures the TOTAL Quebec
#          effect including the identity-mediated component, rather
#          than only the residual after partialling it out.
#          Useful for assessing whether the null Quebec result in the
#          main models is a suppression artefact.
#
# Theory note on the interaction:
#   Quebec × income is the only region × income interaction included.
#   Quebec is the focal region of the study with theoretical motivation
#   for income heterogeneity. Replicating for Alberta or Atlantic Canada
#   has no theoretical justification; those regions enter additively.
#
# Output: one .txt regression table per DV, one per battery/type
# ==============================================================


# ==============================================================
# 0.  OUTPUT DIRECTORY
# ==============================================================

nested_out <- file.path(params$out_reg, "nested_models")
dir.create(nested_out, recursive = TRUE, showWarnings = FALSE)


# ==============================================================
# 1.  INTERACTION TERM
# ==============================================================

df$quebec_x_income <- df$quebec_bin * df$incomeHigh_bin


# ==============================================================
# 2.  NESTED RHS FORMULAS
# ==============================================================

# ── Shared demographic core (in every model) ──────────────────
#   quebec_bin:           focal region variable (Quebec vs. all others)
#   alberta_bin:          Alberta dummy (Ontario = reference)
#   region_eastcoast_bin: Atlantic Canada dummy (Ontario = reference)
#   incomeHigh_bin:       material self-interest (H3)
#   ses_male_bin:         gender control
#   age18_34_bin:         age group (reference: 35–54)
#   age55plus_bin:        age group (reference: 35–54)
#   univ_educ_bin:        university+ (reference: high school)
#   employ_fulltime_bin:  employment control
#   children_bin:         parental status (relevant for child care DVs)

core_demo <- "quebec_bin + alberta_bin + region_eastcoast_bin +
              incomeHigh_bin + ses_male_bin +
              age18_34_bin + age55plus_bin +
              univ_educ_bin + employ_fulltime_bin + children_bin"

# M1: Quebec effect, demographics + all regions
rhs_M1 <- core_demo

# M2: + Québécois-first identity
#   ideo_define_QC_first_bin: provincial-first identity (H5)
rhs_M2 <- paste(core_demo, "+ ideo_define_QC_first_bin")

# M3: + ideology & partisanship (H3, H4b)
#   ideo_right_num:       left–right self-placement, 0=left, 1=right
#   vote_PLC_bin:         voted Liberal (centrist) federally
#   vote_PCC_bin:         voted Conservative federally
#   ideo_vote_fed_left:   federal vote left scale (0=CPC/none → 1=NDP)
#   ideo_vote_prov_left:  provincial vote left scale
rhs_M3 <- paste(rhs_M2,
                "+ ideo_right_num + vote_PLC_bin + vote_PCC_bin",
                "+ ideo_vote_fed_left + ideo_vote_prov_left")

# M4: + institutional trust (H8)
#   trust_inst_fed_bin:   trust in federal institutions
#   trust_inst_prov_bin:  trust in provincial institutions
rhs_M4 <- paste(rhs_M3,
                "+ trust_inst_fed_bin + trust_inst_prov_bin")

# M5: + Quebec × income interaction
rhs_M5 <- paste(rhs_M4, "+ quebec_x_income")

# M6: Total Quebec effect — M5 without Québécois-first identity
#   Removes ideo_define_QC_first_bin so that the quebec_bin coefficient
#   absorbs the identity-mediated component of the Quebec effect.
rhs_M6_base <- paste(core_demo,
                     "+ ideo_right_num + vote_PLC_bin + vote_PCC_bin",
                     "+ ideo_vote_fed_left + ideo_vote_prov_left",
                     "+ trust_inst_fed_bin + trust_inst_prov_bin",
                     "+ quebec_x_income")
rhs_M6 <- rhs_M6_base   # ideo_define_QC_first_bin intentionally absent

rhs_list_nested <- list(
  M1 = rhs_M1, M2 = rhs_M2, M3 = rhs_M3,
  M4 = rhs_M4, M5 = rhs_M5, M6 = rhs_M6
)


# ==============================================================
# 3.  TERM LABELS
# ==============================================================

term_labels_nested <- c(
  # Focal variable
  "quebec_bin"               = "Quebec",
  # Interaction
  "quebec_x_income"          = "Quebec × Income (High)",
  # Other regions (Ontario = reference)
  "alberta_bin"              = "Alberta",
  "region_eastcoast_bin"     = "Atlantic Canada",
  # Material self-interest
  "incomeHigh_bin"           = "Income: High",
  # Demographics
  "ses_male_bin"             = "Male",
  "age18_34_bin"             = "Age 18–34",
  "age55plus_bin"            = "Age 55+",
  "univ_educ_bin"            = "Education: University+",
  "employ_fulltime_bin"      = "Employed Full-Time",
  "children_bin"             = "Has children",
  # Identity
  "ideo_define_QC_first_bin" = "Québécois identity first",
  # Ideology & partisanship
  "ideo_right_num"           = "Ideology (Right scale)",
  "vote_PLC_bin"             = "Fed. vote: Liberal",
  "vote_PCC_bin"             = "Fed. vote: Conservative",
  "ideo_vote_fed_left"       = "Fed. vote (Left scale)",
  "ideo_vote_prov_left"      = "Prov. vote (Left scale)",
  # Trust
  "trust_inst_fed_bin"       = "Trust: Federal institutions",
  "trust_inst_prov_bin"      = "Trust: Provincial institutions"
)


# ==============================================================
# 4.  HELPER: ROBUST VCOV (HC1)
# ==============================================================

if (!exists("robust_vcov")) {
  robust_vcov <- function(model) sandwich::vcovHC(model, type = "HC1")
}


# ==============================================================
# 5.  HELPER: FIT SIX LOGIT MODELS FOR ONE DV
#     Sample held constant across M1–M6 via listwise deletion
#     on the union of ALL RHS variables across all six models.
# ==============================================================

fit_six_logits <- function(dv, data = df) {

  # Union of all variables across all six models
  all_rhs_vars <- unique(unlist(lapply(rhs_list_nested, function(r) {
    all.vars(as.formula(paste("~", r)))
  })))

  model_vars <- unique(c(dv, all_rhs_vars,
                         "quebec_bin", "incomeHigh_bin", "quebec_x_income"))

  # Single listwise-deleted dataset — same for M1 through M6
  model_data <- data |>
    dplyr::select(all_of(intersect(model_vars, names(data)))) |>
    drop_na()

  cat("    N (complete cases):", nrow(model_data), "\n")

  lapply(rhs_list_nested, function(rhs) {
    tryCatch(
      glm(as.formula(paste(dv, "~", rhs)),
          data   = model_data,
          family = binomial(link = "logit")),
      error = function(e) {
        message("    Model failed (", dv, "): ", conditionMessage(e))
        NULL
      }
    )
  })
}


# ==============================================================
# 6.  HELPER: SAVE NESTED REGRESSION TABLE
#     Log-odds coefficients + HC1 SEs + stars.
#     Log-odds preferred over AMEs for nested comparison because
#     AMEs conflate coefficient changes with scale changes as
#     control sets expand.
# ==============================================================

save_nested_regtable <- function(models, dv_label, file_path,
                                 battery_title) {
  models_ok <- Filter(Negate(is.null), models)
  if (length(models_ok) == 0) {
    message("  All models failed for ", dv_label, " — skipping.")
    return(invisible(NULL))
  }

  vcov_list <- lapply(models_ok, function(m) {
    tryCatch(robust_vcov(m), error = function(e) NULL)
  })
  vcov_list <- Filter(Negate(is.null), vcov_list)

  tryCatch({
    modelsummary(
      models_ok,
      estimate  = "{estimate}{stars}",
      statistic = "({std.error})",
      vcov      = vcov_list,
      coef_map  = term_labels_nested,
      gof_map   = c("nobs", "logLik", "AIC", "BIC"),
      output    = file_path,
      title     = paste0(battery_title, " — ", dv_label),
      notes     = paste(
        "Binary logit. Log-odds coefficients.",
        "HC1 robust SEs in parentheses.",
        "Sample held constant across M1–M6 (listwise deletion on union of all RHS vars).",
        "Ontario = reference region.",
        "M1 = Quebec + other regions + demographics.",
        "M2 = M1 + Québécois-first identity.",
        "M3 = M2 + ideology (right scale) + partisanship (fed. & prov. vote).",
        "M4 = M3 + federal inst. trust + provincial inst. trust.",
        "M5 = M4 + Quebec × Income (High) interaction.",
        "M6 = M5 without Québécois-first identity",
        "(captures TOTAL Quebec effect including identity-mediated component).",
        "* p<0.05  ** p<0.01  *** p<0.001"
      )
    )
    cat("  Table saved:", file_path, "\n")
  }, error = function(e) {
    cat("  TABLE ERROR (", dv_label, "):", conditionMessage(e), "\n")
  })
}


# ==============================================================
# 7.  MAIN LOOP — fit + write tables for every DV in every battery
# ==============================================================

cat("\n========== TRADEOFF NESTED-MODEL COMPARISON [REVISED] ==========\n")
cat("Batteries:", length(batteries),
    "| DV types per battery: 2 (pref, intense)",
    "| Models per DV:", length(rhs_list_nested), "\n\n")

for (bat_name in names(batteries)) {

  bat <- batteries[[bat_name]]

  cat("\n#####################################################\n")
  cat("##  BATTERY:", bat_name, "—", bat$title, "\n")
  cat("#####################################################\n")

  for (dv_type in c("pref", "intense")) {

    dv_vec  <- bat[[dv_type]]
    lbl_vec <- bat$labels[dv_vec]

    cat("─────────────────────────────────────────────────────\n")
    cat("DV type:", dv_type, "\n")

    for (i in seq_along(dv_vec)) {

      dv     <- dv_vec[i]
      dv_lbl <- lbl_vec[[i]]

      cat("  DV:", dv, "—", dv_lbl, "\n")

      if (!dv %in% names(df)) {
        cat("    Column not found in df — skipping.\n")
        next
      }

      models <- fit_six_logits(dv)
      names(models) <- paste0("M", seq_along(models))

      safe_label <- gsub("[^a-zA-Z0-9_]", "_", dv_lbl)
      safe_label <- gsub("_+", "_", safe_label)
      safe_label <- gsub("_$",  "",  safe_label)

      file_path <- file.path(
        nested_out,
        paste0("nested_logit_", bat_name, "_", dv_type, "_", dv, ".txt")
      )

      save_nested_regtable(
        models        = models,
        dv_label      = paste0(dv_lbl, " (", dv_type, ")"),
        file_path     = file_path,
        battery_title = bat$title
      )
    }
  }
}

cat("\n========== NESTED-MODEL PIPELINE COMPLETE ==========\n")
cat("Tables written to:", nested_out, "\n")
