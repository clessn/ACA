# ==============================================================
# Budgetary Preferences - Master Script
# Four-region Canadian survey: Alberta, Ontario, Quebec, Atlantic Canada
#
# Changes prior to submission:
### 01_data_prep.R — Recoded raw "East Coast" → "Atlantic Canada" in ses_region_cat before the factor() call. This fixed the empty Atlantic Canada panels in all descriptive plots.
### 02_helpers.R — Three fixes: plot_r2() now uses drop_na() and a safe max(..., na.rm = TRUE) to eliminate the dropped-row warnings; extract_fit_logit() now correctly computes n via df.residual + length(coefficients) instead of length(residuals); extract_ame() now adds n as a column in all AME output.
### 03_variable_definitions.R — "Eastern Canada" → "Atlantic Canada" in ivs, term_labels, iv_order, and region_levels.
### 06_regressions_prio.R — Added hyp_vars filter to coef_prio_pref_logit_AME.png, the typed version, and the robustness plot. Full model retained for tables.
### 07_regressions_tradeoff.R — Removed duplicate copy of the script. Added hyp_vars filter to all T6 coefficient and robustness plots, and inside plot_cross_battery_response().
### 08_regressions_intense.R — Added hyp_vars filter to all three coefficient plots and fixed the no-spend plot to use iv_order instead of reorder(term, estimate).
### 11_appendix_tables.R — New script generating appendix_tables.docx with five full AME regression tables (A1–A5) corresponding to Figures 6–9 in the paper.
### master.R — Updated comment, corrected source paths to code/source/, added 11_appendix_tables.R.
# Run this file to execute the full analysis pipeline.
# All modules are in code/source/. Outputs go to graphs/.
# ==============================================================

# ── Global parameters (change here, propagates everywhere) ────
params <- list(
  dpi         = 300,
  plot_width  = 14,
  plot_height = 10,
  out_graphs  = "graphs",
  out_desc    = "graphs/descriptives_noFrench",
  out_reg     = "graphs/regressions_noFrench",
  data_path   = "data/clean_df_valid.csv"
)

# ── Create output folders if needed ───────────────────────────
dir.create(params$out_desc, recursive = TRUE, showWarnings = FALSE)
dir.create(params$out_reg,  recursive = TRUE, showWarnings = FALSE)

# ── Source modules in order ────────────────────────────────────
source("code/source_tradeoffs/00_packages.R")
source("code/source_tradeoffs/01_data_prep.R")
source("code/source_tradeoffs/02_helpers.R")
source("code/source_tradeoffs/03_variable_definitions.R")
source("code/source_tradeoffs/04_descriptives.R")
source("code/source_tradeoffs/05_regressions_imp.R")
source("code/source_tradeoffs/06_regressions_prio.R")
source("code/source_tradeoffs/07_regressions_tradeoff.R")
source("code/source_tradeoffs/08_regressions_intense.R")
source("code/source_tradeoffs/09_regressions_uc.R")
source("code/source_tradeoffs/11_appendix_tables.R")
source("code/source_tradeoffs/12_regressions_tradeoff_nested.R")

cat("\n========== PIPELINE COMPLETE ==========\n")
cat("All outputs saved to:", params$out_graphs, "\n")
