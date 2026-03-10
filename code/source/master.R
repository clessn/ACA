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
source("R/00_packages.R")
source("R/01_data_prep.R")
source("R/02_helpers.R")
source("R/03_variable_definitions.R")
source("R/04_descriptives.R")
source("R/05_regressions_imp.R")
source("R/06_regressions_prio.R")
source("R/07_regressions_tradeoff.R")
source("R/08_regressions_intense.R")
source("R/09_regressions_uc.R")

cat("\n========== PIPELINE COMPLETE ==========\n")
cat("All outputs saved to:", params$out_graphs, "\n")
