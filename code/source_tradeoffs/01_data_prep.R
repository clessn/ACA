# ==============================================================
# 01_data_prep.R
# Load data, construct all variables, run sanity checks
# ==============================================================

# ── Load data ─────────────────────────────────────────────────
df <- read.csv(params$data_path)

# ── 1. DEMOGRAPHIC VARIABLES ──────────────────────────────────

df$univ_educ_bin <- as.integer(df$educ_group == "educUniv")

if (!"incomeHigh_bin" %in% names(df)) {
  df$incomeHigh_bin <- as.integer(df$ses_income3Cat == "High")
}

df$educ_group     <- factor(df$educ_group,     levels = c("educBHS", "educHS", "educUniv"))
df$ses_income3Cat <- factor(df$ses_income3Cat, levels = c("Low", "Mid", "High"))

# Recode raw data value to match the Atlantic Canada label used throughout
df$ses_region_cat[df$ses_region_cat == "East Coast"] <- "Atlantic Canada"
df$ses_region_cat <- factor(df$ses_region_cat, levels = c("Ontario", "Quebec", "Alberta", "Atlantic Canada"))


# ── 2. BUDGET IMPORTANCE COMPOSITES ───────────────────────────

df$expansion_num <- rowMeans(df[, c("budget_imp_health_num", "budget_imp_edu_num",
                                    "budget_imp_pensions_num")], na.rm = TRUE)

df$expansion_bin <- as.integer(
  rowMeans(df[, c("budget_imp_health_bin", "budget_imp_edu_bin",
                  "budget_imp_pensions_bin")], na.rm = TRUE) >= 0.5
)

df$reduc_num <- rowMeans(df[, c("budget_imp_taxes_num", "budget_imp_debt_num")], na.rm = TRUE)

df$reduc_bin <- as.integer(
  rowMeans(df[, c("budget_imp_taxes_bin", "budget_imp_debt_bin")], na.rm = TRUE) >= 0.5
)


# ── 3. BUDGET PRIORITY COMPOSITES ─────────────────────────────

df$soc_pol_num <- rowMeans(df[, c("budget_prio_health", "budget_prio_seniors",
                                  "budget_prio_cc")], na.rm = TRUE)

df$other_num <- rowMeans(df[, c("budget_prio_ecn", "budget_prio_clim")], na.rm = TRUE)

df$soc_pol_pref <- as.integer(
  df$budget_prio_health_pref  == 1 |
  df$budget_prio_seniors_pref == 1 |
  df$budget_prio_cc_pref      == 1
)

df$other_pref <- as.integer(
  df$budget_prio_ecn_pref  == 1 |
  df$budget_prio_clim_pref == 1
)

df$soc_pol_intense <- as.integer(
  df$budget_prio_health_intense  == 1 |
  df$budget_prio_seniors_intense == 1 |
  df$budget_prio_cc_intense      == 1
)

df$other_intense <- as.integer(
  df$budget_prio_ecn_intense  == 1 |
  df$budget_prio_clim_intense == 1
)


# ── 4. TRADEOFF HC COMPOSITES ─────────────────────────────────

df$hc_univ_bin     <- df$tradeoff_hc_all_pref
df$hc_univ_intense <- df$tradeoff_hc_all_intense

df$hc_target_bin <- as.integer(
  df$tradeoff_hc_spend_pref    == 1 |
  df$tradeoff_hc_pensions_pref == 1
)

df$hc_target_intense <- as.integer(
  df$tradeoff_hc_spend_intense    == 1 |
  df$tradeoff_hc_pensions_intense == 1
)


# ── 5. TRADEOFF CC2 COMPOSITES ────────────────────────────────

df$cc_univ_bin <- as.integer(
  df$tradeoff_cc2_all_pref      == 1 |
  df$tradeoff_cc2_educ_all_pref == 1
)

df$cc_univ_intense <- as.integer(
  df$tradeoff_cc2_all_intense      == 1 |
  df$tradeoff_cc2_educ_all_intense == 1
)

df$cc_target_bin <- as.integer(
  df$tradeoff_cc2_low_inc_pref      == 1 |
  df$tradeoff_cc2_educ_low_inc_pref == 1
)

df$cc_target_intense <- as.integer(
  df$tradeoff_cc2_low_inc_intense      == 1 |
  df$tradeoff_cc2_educ_low_inc_intense == 1
)


# ── 6. SANITY CHECKS ──────────────────────────────────────────

sanity_vars <- c("expansion_bin", "reduc_bin", "soc_pol_pref", "soc_pol_intense",
                 "other_pref", "hc_univ_bin", "hc_target_bin", "cc_univ_bin", "cc_target_bin")

walk(sanity_vars, function(v) {
  cat("---", v, "---\n")
  print(table(df[[v]], useNA = "always"))
})

cat("\n--- Intense variable n check ---\n")
intense_vars <- df |> dplyr::select(matches("_intense$")) |> names()
map_dfr(intense_vars, ~tibble(
  variable = .x,
  n        = sum(df[[.x]] == 1, na.rm = TRUE),
  pct      = round(mean(df[[.x]] == 1, na.rm = TRUE) * 100, 1),
  n_na     = sum(is.na(df[[.x]]))
)) |> arrange(pct) |> print(n = Inf)
