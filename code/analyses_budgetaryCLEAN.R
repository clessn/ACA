# ==============================================================
# Budgetary Preferences - Master Script
# Four-region Canadian survey: Alberta, Ontario, Quebec, Eastern Canada
#
# Run this file to execute the full analysis pipeline.
# All modules are in R/. Outputs go to graphs/.
# ==============================================================

# ── Global parameters (change here, propagates everywhere) ────
params <- list(
  dpi         = 300,
  plot_width  = 14,
  plot_height = 10,
  out_graphs  = "graphs",
  out_desc    = "graphs/descriptives",
  out_reg     = "graphs/regressions",
  data_path   = "data/clean_df_valid.csv"
)

# ── Create output folders if needed ───────────────────────────
dir.create(params$out_desc, recursive = TRUE, showWarnings = FALSE)
dir.create(params$out_reg,  recursive = TRUE, showWarnings = FALSE)

# ── Source modules in order ────────────────────────────────────
source("code/source/00_packages.R")
source("code/source/01_data_prep.R")
source("code/source/02_helpers.R")
source("code/source/03_variable_definitions.R")
source("code/source/04_descriptives.R")
source("code/source/05_regressions_imp.R")
source("code/source/06_regressions_prio.R")
source("code/source/07_regressions_tradeoff.R")
source("code/source/08_regressions_intense.R")
source("code/source/09_regressions_uc.R")

cat("\n========== PIPELINE COMPLETE ==========\n")
cat("All outputs saved to:", params$out_graphs, "\n")