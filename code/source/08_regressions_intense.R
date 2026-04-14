# ==============================================================
# 08_regressions_intense.R
# Intense preference DVs: Logit AME (primary) + LPM (robustness)
# 13 DVs across tradeoff batteries and composite variables
# ==============================================================


# ── FIT MODELS ───────────────────────────────────────────────

intense_models <- fit_binary_models(
  dv_vec    = intense_dvs,
  label_vec = intense_labels[intense_dvs],
  rhs       = rhs
)


# ── AVERAGE MARGINAL EFFECTS ──────────────────────────────────

coef_logit_intense <- extract_ame_list(intense_models$logit, "intense_logit")
coef_lpm_intense   <- extract_ame_list(intense_models$lpm,   "intense_lpm")


# ── MODEL FIT ─────────────────────────────────────────────────

fit_logit_intense <- extract_fit_logit(intense_models$logit, "Logit")
fit_lpm_intense   <- extract_fit_lm(intense_models$lpm,      "LPM")


# ── REGRESSION TABLES ─────────────────────────────────────────

save_regtable(
  intense_models$logit,
  file.path(params$out_reg, "regtable_intense_logit_AME.txt"),
  notes        = "Average marginal effects from logit. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001",
  is_logit_ame = TRUE
)

save_regtable(
  intense_models$logit,
  file.path(params$out_reg, "regtable_intense_logit_coefs.txt"),
  notes        = "Logit coefficients (log-odds). HC1 robust SEs. Interpret via AME table.",
  is_logit_ame = FALSE,
  gof          = c("nobs", "logLik", "AIC")
)

save_regtable(
  intense_models$lpm,
  file.path(params$out_reg, "regtable_intense_LPM.txt"),
  notes        = "LPM (OLS) robustness check. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001",
  is_logit_ame = FALSE,
  gof          = c("nobs", "r.squared", "adj.r.squared")
)


# ── COEFFICIENT PLOTS ─────────────────────────────────────────
#
# Figures show only the key hypothesis variables listed below.
# All variables remain in the fitted models and regression tables.
hyp_vars <- c(
  "incomeHigh_bin",
  "univ_educ_bin",
  "employ_fulltime_bin",
  "children_bin",
  "ideo_right_num",
  "vote_PLC_bin",
  "vote_PCC_bin",
  "ideo_define_QC_first_bin",
  "quebec_bin",
  "alberta_bin",
  "region_eastcoast_bin",
  "trust_inst_fed_bin",
  "trust_inst_prov_bin"
)

plot_coefs(
  coef_logit_intense |> dplyr::filter(term %in% hyp_vars),
  "Intense preference -- Logit average marginal effects",
  file.path(params$out_reg, "coef_intense_logit_AME.png")
)

plot_coefs(
  coef_lpm_intense |> dplyr::filter(term %in% hyp_vars),
  "Intense preference -- LPM coefficients (robustness)",
  file.path(params$out_reg, "coef_intense_LPM.png")
)

plot_robustness(
  coef_logit = coef_logit_intense |> dplyr::filter(term %in% hyp_vars),
  coef_lpm   = coef_lpm_intense   |> dplyr::filter(term %in% hyp_vars),
  title_str  = "Intense preference -- Logit AME vs. LPM (robustness check)",
  file_path  = file.path(params$out_reg, "coef_intense_LPM_vs_logit.png"),
  ncol = 3, width = 16, height = 14
)


# ── R-SQUARED PLOTS ───────────────────────────────────────────

plot_r2(fit_logit_intense, r2_col = "pseudo_r2",
        title_str = "Model fit -- Intense preference (Logit pseudo-R2)",
        file_path = file.path(params$out_reg, "r2_intense_logit.png"))

plot_r2(fit_lpm_intense,   r2_col = "adj_r_sq",
        title_str = "Model fit -- Intense preference (LPM, robustness)",
        file_path = file.path(params$out_reg, "r2_intense_LPM.png"))


# ── DIAGNOSTICS ───────────────────────────────────────────────

cat("\n========== INTENSE: Logit pseudo-R2 ==========\n"); print(fit_logit_intense)
cat("\n========== INTENSE: LPM adjusted R2 ==========\n");  print(fit_lpm_intense)

cat("\n========== Most significant predictors -- Intense Logit AME ==========\n")
coef_logit_intense |>
  filter(sig %in% c("*", "**", "***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

check_direction_agreement(coef_logit_intense, coef_lpm_intense, "-- Intense preference")


# ── SAVE AMEs ─────────────────────────────────────────────────

bind_rows(
  coef_logit_intense |> mutate(model = "Logit AME"),
  coef_lpm_intense   |> mutate(model = "LPM")
) |>
  write.csv(file.path(params$out_reg, "AME_intense_all.csv"), row.names = FALSE)

# ── No-spend/no-tax intense: Green economy, Childcare & Tax -- Logit AME ─

no_spend_dvs <- c(
  "tradeoff_ge_no_spend_intense"       = "Don't spend more on green economy (intense)",
  "tradeoff_cc1_no_spend_intense"      = "Don't spend more on childcare (intense)",
  "tradeoff_tax_less_services_intense" = "No tax increase, even if fewer services (intense)"
)

no_spend_models <- fit_binary_models(
  dv_vec    = names(no_spend_dvs),
  label_vec = unname(no_spend_dvs),
  rhs       = rhs
)

coef_logit_no_spend <- extract_ame_list(no_spend_models$logit, "no_spend_logit")

coef_logit_no_spend |>
  dplyr::filter(term %in% hyp_vars) |>
  mutate(term = recode(term, !!!term_labels)) |>
  ggplot(aes(x = estimate, y = factor(term, levels = rev(iv_order)), color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  scale_color_manual(
    values = c("Positive" = "#2166ac", "Negative" = "#d6604d",
               "No clear effect" = "grey60")
  ) +
  facet_wrap(~dv, ncol = 3, scales = "free_x") +
  labs(
    x       = "Average marginal effect (HC1 robust SEs)",
    y       = NULL,
    color   = NULL,
    caption = "Logit AME. HC1 robust SEs. 95% CI.\nOutcome: intense preference (>50 pts) for no spending increase or tax increase."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

ggsave(
  file.path(params$out_reg, "coef_no_spend_no_tax_intense_logit_AME.png"),
  width = 18, height = 8, dpi = params$dpi
)
