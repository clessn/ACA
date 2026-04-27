# ==============================================================
# 12_regressions_tradeoff_nested.R
# Nested-model comparison for budget tradeoff batteries
# Five-model logit progression per DV, with Quebec × income in M5
#
# Pipeline assumptions:
#   • 00_packages.R, 01_data_prep.R, 02_helpers.R, 03_variable_definitions.R
#     have been sourced (df, params, batteries, term_labels, etc. in scope).
#
# Models (per DV, binary logit, HC1 robust SEs):
#   M1 — Core: regions + demographics + Quebecker-First identity
#   M2 — M1 + institutional trust (federal + provincial)
#   M3 — M1 + ideology / vote (right scale, fed & prov vote, party vote)
#   M4 — Full: M1 + trust + ideology
#   M5 — Full + Quebec × Income interaction
#   M6 — Full + Quebec × Income, but WITHOUT ideo_define_QC_first_bin
#        (Quebecker-First identity), so the Quebec coefficient captures
#        the total Quebec effect rather than the residual after
#        partialling out Québécois-first identity.
#        (ses_french_bin is already excluded from this project's RHS —
#        see params$out_reg = "graphs/regressions_noFrench".)
#
# Output: regression tables (.txt) only — one per DV, with M1–M5 as
# columns. No plots.
#
# Theory note on the interaction:
#   Quebec × income is the only region × income interaction included.
#   Quebec is the focal region of the study, with theoretical motivation
#   for income heterogeneity. Replicating the interaction for Alberta or
#   Atlantic Canada has no theoretical justification, so those regions
#   enter as additive dummies only. (Ontario = omitted reference.)
# ==============================================================


# ── 1. OUTPUT DIRECTORY ───────────────────────────────────────

nested_out <- file.path(params$out_reg, "nested_models")
dir.create(nested_out, recursive = TRUE, showWarnings = FALSE)


# ── 2. INTERACTION TERM ───────────────────────────────────────
# Build explicitly so modelsummary labels it cleanly.
df$quebec_x_income <- df$quebec_bin * df$incomeHigh_bin


# ── 3. NESTED RHS FORMULAS ────────────────────────────────────
#
#   Variable groupings (each block layered on top of M1):
#
#   Core (in every model):
#     ideo_define_QC_first_bin
#     quebec_bin, alberta_bin, region_eastcoast_bin   (Ontario = ref)
#     incomeHigh_bin, ses_male_bin,
#     age18_34_bin, age55plus_bin,                    (35–54 = ref)
#     univ_educ_bin, employ_fulltime_bin, children_bin
#
#   Trust block:
#     trust_inst_fed_bin, trust_inst_prov_bin
#
#   Ideology / vote block:
#     ideo_right_num                (0 = left, 1 = right — opposite
#                                    direction from the two left scales)
#     vote_PLC_bin, vote_PCC_bin
#     ideo_vote_fed_left, ideo_vote_prov_left

rhs_M1 <- "ideo_define_QC_first_bin +
            quebec_bin + alberta_bin + region_eastcoast_bin +
            incomeHigh_bin + ses_male_bin +
            age18_34_bin + age55plus_bin +
            univ_educ_bin + employ_fulltime_bin + children_bin"

rhs_M2 <- paste(rhs_M1,
                "+ trust_inst_fed_bin + trust_inst_prov_bin")

rhs_M3 <- paste(rhs_M1,
                "+ ideo_right_num + vote_PLC_bin + vote_PCC_bin",
                "+ ideo_vote_fed_left + ideo_vote_prov_left")

rhs_M4 <- paste(rhs_M1,
                "+ trust_inst_fed_bin + trust_inst_prov_bin",
                "+ ideo_right_num + vote_PLC_bin + vote_PCC_bin",
                "+ ideo_vote_fed_left + ideo_vote_prov_left")

rhs_M5 <- paste(rhs_M4, "+ quebec_x_income")

# M6: Same RHS as M5 but with ideo_define_QC_first_bin removed, so the
#     Quebec coefficient captures the *total* Quebec effect — i.e.
#     including the share that runs through Québécois-first identity
#     — rather than only the residual after partialling that out.
rhs_M6_core <- "quebec_bin + alberta_bin + region_eastcoast_bin +
                 incomeHigh_bin + ses_male_bin +
                 age18_34_bin + age55plus_bin +
                 univ_educ_bin + employ_fulltime_bin + children_bin"

rhs_M6 <- paste(rhs_M6_core,
                "+ trust_inst_fed_bin + trust_inst_prov_bin",
                "+ ideo_right_num + vote_PLC_bin + vote_PCC_bin",
                "+ ideo_vote_fed_left + ideo_vote_prov_left",
                "+ quebec_x_income")

rhs_list_nested <- list(
  M1 = rhs_M1, M2 = rhs_M2, M3 = rhs_M3,
  M4 = rhs_M4, M5 = rhs_M5, M6 = rhs_M6
)


# ── 4. TERM LABELS ────────────────────────────────────────────
# Reuse the project's term_labels and add the interaction term.
term_labels_nested <- c(
  term_labels,
  "quebec_x_income" = "Quebec × Income (High)"
)


# ── 5. HELPER: FIT FIVE LOGIT MODELS FOR ONE DV ──────────────

fit_five_logits <- function(dv, data = df, rhs_list = rhs_list_nested) {
  lapply(rhs_list, function(rhs) {
    # Always drop NA on the union of variables across all 5 RHS — keeps
    # the sample constant across M1–M5 so coefficients are comparable.
    all_rhs_vars <- unique(unlist(lapply(rhs_list, function(r) {
      all.vars(as.formula(paste("~", r)))
    })))
    model_vars <- unique(c(dv, all_rhs_vars,
                           "quebec_bin", "incomeHigh_bin", "quebec_x_income"))
    model_data <- data |>
      dplyr::select(all_of(model_vars)) |>
      drop_na()

    tryCatch(
      glm(as.formula(paste(dv, "~", rhs)),
          data   = model_data,
          family = binomial(link = "logit")),
      error = function(e) {
        message("Model failed for ", dv, " (", rhs, "): ",
                conditionMessage(e))
        NULL
      }
    )
  })
}


# ── 6. HELPER: SAVE NESTED REGRESSION TABLE ───────────────────
#
# Log-odds coefficients with stars and HC1 robust SEs.
# Log-odds (rather than AMEs) are used here because the goal is to
# trace how each coefficient shifts as control blocks are added —
# AMEs change implicit scale across nested models, which complicates
# block-by-block comparison.

save_nested_regtable <- function(models, dv_label, file_path,
                                 battery_title) {
  models_ok <- Filter(Negate(is.null), models)
  if (length(models_ok) == 0) {
    message("All models failed for ", dv_label, " — skipping.")
    return(invisible(NULL))
  }

  vcov_list <- lapply(models_ok, robust_vcov)

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
        "Binary logit. Log-odds coefficients with HC1 robust SEs in parentheses.",
        "Sample held constant across M1–M5 (NA-listwise on union of RHS vars).",
        "M1 = Core (regions + demographics + Quebecker-First).",
        "M2 = M1 + institutional trust.",
        "M3 = M1 + ideology / vote.",
        "M4 = Full (M1 + trust + ideology).",
        "M5 = Full + Quebec × Income (High).",
        "M6 = M5 without ideo_define_QC_first_bin",
        "(isolates total Quebec effect, incl. identity-mediated component).",
        "Region reference: Ontario.",
        "* p<0.05  ** p<0.01  *** p<0.001"
      )
    )
    cat("  Table saved:", file_path, "\n")
  }, error = function(e) {
    cat("  TABLE ERROR (", dv_label, "):", conditionMessage(e), "\n")
  })
}


# ── 7. MAIN LOOP — fit + write tables for every DV ────────────

cat("\n========== TRADEOFF NESTED-MODEL COMPARISON ==========\n")
cat("Batteries:", length(batteries),
    "| DV types per battery: 2 (pref, intense)",
    "| Models per DV:", length(rhs_list_nested), "\n\n")

for (bat_name in names(batteries)) {

  bat <- batteries[[bat_name]]

  cat("\n#####################################################\n")
  cat("##  BATTERY:", bat_name, "—", bat$title, "\n")
  cat("#####################################################\n")

  # Loop over both binary DV types: first choice (pref) + intense
  for (dv_type in c("pref", "intense")) {

    dv_vec   <- bat[[dv_type]]
    lbl_vec  <- bat$labels[dv_vec]

    cat("─────────────────────────────────────────────────────\n")
    cat("DV type:", dv_type, "\n")

    for (i in seq_along(dv_vec)) {

      dv      <- dv_vec[i]
      dv_lbl  <- lbl_vec[[i]]

      cat("  DV:", dv, "—", dv_lbl, "\n")

      # Skip if DV column missing
      if (!dv %in% names(df)) {
        cat("    Column not found in df — skipping.\n")
        next
      }

      models <- fit_five_logits(dv)
      names(models) <- paste0("M", seq_along(models))

      safe_label <- gsub("[^a-zA-Z0-9_]", "_", dv_lbl)
      safe_label <- gsub("_+", "_", safe_label)
      safe_label <- gsub("_$", "", safe_label)

      file_path <- file.path(
        nested_out,
        paste0("nested_logit_", bat_name, "_", dv_type, "_",
               dv, ".txt")
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
