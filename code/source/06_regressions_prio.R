# ==============================================================
# 06_regressions_prio.R
# Budget priority: OLS on raw scores (0-100 allocation)
# ==============================================================


# ── 8. PRIORITY RAW SCORE OLS ─────────────────────────────────

lm_prio <- prio_num_df$var |>
  set_names(prio_num_df$label) |>
  map(~lm(as.formula(paste(.x, "~", rhs)), data = df))

coef_prio <- extract_ame_list(lm_prio, "prio_ols")

fit_prio <- extract_fit_lm(lm_prio, "OLS")

# Regression table
save_regtable(
  lm_prio,
  file.path(params$out_reg, "regtable_prio_OLS.txt"),
  notes        = "OLS. HC1 robust SEs. DV = budget points allocated (0-100). * p<0.05, ** p<0.01, *** p<0.001",
  is_logit_ame = FALSE,
  gof          = c("nobs", "r.squared", "adj.r.squared")
)

# Coefficient plot
plot_coefs(
  coef_prio,
  "Budget priority allocation (0-100 pts) -- OLS avg marginal effects",
  file.path(params$out_reg, "coef_prio_OLS.png")
)

# R2 plot
plot_r2(
  fit_prio,
  r2_col    = "adj_r_sq",
  title_str = "Model fit -- Priority allocation (OLS)",
  file_path = file.path(params$out_reg, "r2_prio_OLS.png")
)

# Diagnostics
cat("\n========== PRIORITY: OLS fit ==========\n"); print(fit_prio)

cat("\n========== Most significant predictors -- prio OLS ==========\n")
coef_prio |>
  filter(sig %in% c("*", "**", "***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

# ── PRIORITY FIRST CHOICE: Logit AME ─────────────────────────

prio_pref_models <- fit_binary_models(
  dv_vec    = prio_pref_vars,
  label_vec = recode(str_extract(prio_pref_vars, "(?<=budget_prio_).*(?=_pref)"),
                     !!!prio_policy_labels, .default = prio_pref_vars),
  rhs       = rhs
)

coef_logit_prio_pref <- extract_ame_list(prio_pref_models$logit, "prio_pref_logit")
coef_lpm_prio_pref   <- extract_ame_list(prio_pref_models$lpm,   "prio_pref_lpm")

fit_logit_prio_pref <- extract_fit_logit(prio_pref_models$logit, "Logit_pref")

# Regression table
save_regtable(
  prio_pref_models$logit,
  file.path(params$out_reg, "regtable_prio_pref_logit_AME.txt"),
  notes        = "Logit AME. Budget priority first choice. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001",
  is_logit_ame = TRUE
)

# Coefficient plot — hypothesis variables only (hyp_vars defined in 03_variable_definitions.R)
plot_coefs(
  coef_logit_prio_pref |> dplyr::filter(term %in% hyp_vars),
  "Budget priority first choice -- Logit average marginal effects",
  file.path(params$out_reg, "coef_prio_pref_logit_AME.png")
)

# Typed version: all priority DVs are spending investments
prio_type_map <- c(
  "Healthcare access"                      = "Investment",
  "Home care for seniors"                  = "Investment",
  "Subsidized child care"                  = "Investment",
  "Support businesses and economic growth" = "Investment",
  "Fight against climate change"           = "Investment"
)
plot_coefs_typed(
  coef_logit_prio_pref |> dplyr::filter(term %in% hyp_vars),
  "Budget priority first choice — Logit AME (investment vs fiscal consolidation)",
  file.path(params$out_reg, "coef_prio_pref_logit_AME_typed.png"),
  type_map = prio_type_map
)

# Robustness
plot_robustness(
  coef_logit = coef_logit_prio_pref |> dplyr::filter(term %in% hyp_vars),
  coef_lpm   = coef_lpm_prio_pref   |> dplyr::filter(term %in% hyp_vars),
  title_str  = "Budget priority first choice -- Logit AME vs. LPM (robustness)",
  file_path  = file.path(params$out_reg, "coef_prio_pref_logit_vs_lpm.png")
)

# R2
plot_r2(
  fit_logit_prio_pref,
  r2_col    = "pseudo_r2",
  title_str = "Model fit -- Budget priority first choice (Logit pseudo-R2)",
  file_path = file.path(params$out_reg, "r2_prio_pref_logit.png")
)

# Diagnostics
check_direction_agreement(coef_logit_prio_pref, coef_lpm_prio_pref,
                          "-- Priority first choice")

# Save AMEs
bind_rows(
  coef_logit_prio_pref |> mutate(model = "Logit AME"),
  coef_lpm_prio_pref   |> mutate(model = "LPM")
) |>
  write.csv(file.path(params$out_reg, "AME_prio_pref_all.csv"), row.names = FALSE)