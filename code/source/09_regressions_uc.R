# ==============================================================
# 09_regressions_uc.R
# HC & CC2 universal vs targeted composites
# Logit AME (primary) + LPM (robustness), _bin and _intense
# ==============================================================


# ── FIT MODELS ───────────────────────────────────────────────

uc_bin_models     <- fit_binary_models(names(uc_dvs$bin),     uc_dvs$bin,     rhs)
uc_intense_models <- fit_binary_models(names(uc_dvs$intense), uc_dvs$intense, rhs)


# ── AVERAGE MARGINAL EFFECTS ──────────────────────────────────

coef_logit_uc_bin     <- extract_ame_list(uc_bin_models$logit,     "uc_bin_logit")
coef_lpm_uc_bin       <- extract_ame_list(uc_bin_models$lpm,       "uc_bin_lpm")
coef_logit_uc_intense <- extract_ame_list(uc_intense_models$logit, "uc_intense_logit")
coef_lpm_uc_intense   <- extract_ame_list(uc_intense_models$lpm,   "uc_intense_lpm")


# ── MODEL FIT ─────────────────────────────────────────────────

fit_logit_uc_bin     <- extract_fit_logit(uc_bin_models$logit,     "Logit_bin")
fit_lpm_uc_bin       <- extract_fit_lm(uc_bin_models$lpm,         "LPM_bin")
fit_logit_uc_intense <- extract_fit_logit(uc_intense_models$logit, "Logit_intense")
fit_lpm_uc_intense   <- extract_fit_lm(uc_intense_models$lpm,     "LPM_intense")


# ── REGRESSION TABLES ─────────────────────────────────────────

save_regtable(
  uc_bin_models$logit,
  file.path(params$out_reg, "regtable_uc_bin_logit_AME.txt"),
  notes        = "HC & CC2 universal/targeted — Logit AME (first choice). HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001",
  is_logit_ame = TRUE
)

save_regtable(
  uc_intense_models$logit,
  file.path(params$out_reg, "regtable_uc_intense_logit_AME.txt"),
  notes        = "HC & CC2 universal/targeted — Logit AME (intense preference). HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001",
  is_logit_ame = TRUE
)

save_regtable(
  uc_bin_models$lpm,
  file.path(params$out_reg, "regtable_uc_bin_lpm.txt"),
  notes        = "HC & CC2 universal/targeted — LPM robustness (first choice). HC1 robust SEs.",
  is_logit_ame = FALSE,
  gof          = c("nobs", "r.squared")
)

save_regtable(
  uc_intense_models$lpm,
  file.path(params$out_reg, "regtable_uc_intense_lpm.txt"),
  notes        = "HC & CC2 universal/targeted — LPM robustness (intense preference). HC1 robust SEs.",
  is_logit_ame = FALSE,
  gof          = c("nobs", "r.squared")
)


# ── COEFFICIENT PLOTS ─────────────────────────────────────────

plot_coefs(coef_logit_uc_bin,
           "HC & CC2 universal/targeted — Logit AME (first choice)",
           file.path(params$out_reg, "coef_uc_bin_logit_AME.png"))

plot_coefs(coef_logit_uc_intense,
           "HC & CC2 universal/targeted — Logit AME (intense preference)",
           file.path(params$out_reg, "coef_uc_intense_logit_AME.png"))

plot_robustness(
  coef_logit_uc_bin, coef_lpm_uc_bin,
  "HC & CC2 universal/targeted — Logit AME vs. LPM (first choice, robustness)",
  file.path(params$out_reg, "coef_uc_bin_logit_vs_lpm.png")
)

plot_robustness(
  coef_logit_uc_intense, coef_lpm_uc_intense,
  "HC & CC2 universal/targeted — Logit AME vs. LPM (intense preference, robustness)",
  file.path(params$out_reg, "coef_uc_intense_logit_vs_lpm.png")
)


# ── R-SQUARED PLOTS ───────────────────────────────────────────

plot_r2(fit_logit_uc_bin,
        r2_col    = "pseudo_r2",
        title_str = "Model fit — UC universal/targeted, Logit pseudo-R2 (first choice)",
        file_path = file.path(params$out_reg, "r2_uc_bin_logit.png"))

plot_r2(fit_logit_uc_intense,
        r2_col    = "pseudo_r2",
        title_str = "Model fit — UC universal/targeted, Logit pseudo-R2 (intense preference)",
        file_path = file.path(params$out_reg, "r2_uc_intense_logit.png"))


# ── DIAGNOSTICS ───────────────────────────────────────────────

cat("\n========== UC bin: Logit pseudo-R2 ==========\n");     print(fit_logit_uc_bin)
cat("\n========== UC intense: Logit pseudo-R2 ==========\n"); print(fit_logit_uc_intense)

check_direction_agreement(coef_logit_uc_bin,     coef_lpm_uc_bin,     "-- UC bin")
check_direction_agreement(coef_logit_uc_intense, coef_lpm_uc_intense, "-- UC intense")


# ── SAVE AMEs ─────────────────────────────────────────────────

bind_rows(
  coef_logit_uc_bin     |> mutate(model = "Logit AME"),
  coef_logit_uc_intense |> mutate(model = "Logit AME"),
  coef_lpm_uc_bin       |> mutate(model = "LPM"),
  coef_lpm_uc_intense   |> mutate(model = "LPM")
) |>
  write.csv(file.path(params$out_reg, "AME_uc_all.csv"), row.names = FALSE)
