# ==============================================================
# 05_regressions_imp.R
# Importance battery: LPM + Logit AME, bivariate contrasts
# ==============================================================


# ── 6. BIVARIATE MARGINAL MEANS ───────────────────────────────

all_reg_df <- bind_rows(
  imp_bin_df  |> mutate(battery = "imp_bin"),
  prio_num_df |> mutate(battery = "prio_num")
)

marginal_results <- map_dfr(ivs, function(iv) {
  map2_dfr(all_reg_df$var, all_reg_df$label, interpret_contrast, iv = iv, data = df)
})

write.csv(marginal_results,
          file.path(params$out_reg, "marginal_means_all.csv"), row.names = FALSE)


# ── 7. IMPORTANCE BINARY REGRESSIONS ─────────────────────────

imp_models <- fit_binary_models(imp_bin_df$var, imp_bin_df$label, rhs)

coef_logit_imp <- extract_ame_list(imp_models$logit, "imp_logit")
coef_lpm_imp   <- extract_ame_list(imp_models$lpm,   "imp_lpm")

fit_logit_imp <- extract_fit_logit(imp_models$logit, "Logit")
fit_lpm_imp   <- extract_fit_lm(imp_models$lpm,      "LPM")

# Regression tables
save_regtable(
  imp_models$lpm,
  file.path(params$out_reg, "regtable_imp_LPM.txt"),
  notes        = "LPM (OLS). HC1 robust SEs in parentheses. * p<0.05, ** p<0.01, *** p<0.001",
  is_logit_ame = FALSE,
  gof          = c("nobs", "r.squared", "adj.r.squared")
)

save_regtable(
  imp_models$logit,
  file.path(params$out_reg, "regtable_imp_logit_coefs.txt"),
  notes        = "Logit coefficients (log-odds). HC1 robust SEs. Interpret via AME table.",
  is_logit_ame = FALSE,
  gof          = c("nobs", "logLik", "AIC")
)

save_regtable(
  imp_models$logit,
  file.path(params$out_reg, "regtable_imp_logit_AME.txt"),
  notes        = "Average marginal effects from logit. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001",
  is_logit_ame = TRUE
)

# Coefficient plots
plot_coefs(coef_logit_imp, "Importance (binary) -- Logit average marginal effects",
           file.path(params$out_reg, "coef_imp_logit_AME.png"))

plot_coefs(coef_lpm_imp, "Importance (binary) -- LPM coefficients",
           file.path(params$out_reg, "coef_imp_LPM.png"))

plot_robustness(
  coef_logit = coef_logit_imp,
  coef_lpm   = coef_lpm_imp,
  title_str  = "Importance battery -- Logit AME vs. LPM (robustness check)",
  file_path  = file.path(params$out_reg, "coef_imp_LPM_vs_logit.png"),
  ncol       = 3, width = 14, height = 9
)

# R2 plots
plot_r2(fit_lpm_imp,   r2_col = "adj_r_sq",
        title_str = "Model fit -- Importance battery (LPM)",
        file_path = file.path(params$out_reg, "r2_imp_LPM.png"))

plot_r2(fit_logit_imp, r2_col = "pseudo_r2",
        title_str = "Model fit -- Importance battery (Logit pseudo-R2)",
        file_path = file.path(params$out_reg, "r2_imp_logit.png"))

# Diagnostics
cat("\n========== IMPORTANCE: LPM fit ==========\n");         print(fit_lpm_imp)
cat("\n========== IMPORTANCE: Logit pseudo-R2 ==========\n"); print(fit_logit_imp)

cat("\n========== Most significant predictors -- Importance Logit AME ==========\n")
coef_logit_imp |>
  filter(sig %in% c("*", "**", "***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

check_direction_agreement(coef_logit_imp, coef_lpm_imp, "-- Importance battery")


# ── 9. MARGINAL EFFECTS PLOTS -- IDEOLOGY x QUEBEC ───────────

walk2(
  c(imp_bin_df$var, prio_num_df$var),
  c(imp_bin_df$label, prio_num_df$label),
  function(dv, dv_label) {
    model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> select(all_of(model_vars)) |> drop_na()
    model      <- lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    me         <- ggpredict(model, terms = c("ideo_right_num [all]", "quebec_bin [0,1]"))
    ggplot(me, aes(x = x, y = predicted,
                   colour = as.factor(group), fill = as.factor(group))) +
      geom_line(linewidth = 1) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
      scale_colour_manual(values = c("0" = "#d6604d", "1" = "#2166ac"),
                          labels = c("0" = "Rest of Canada", "1" = "Quebec")) +
      scale_fill_manual(values   = c("0" = "#d6604d", "1" = "#2166ac"),
                        labels   = c("0" = "Rest of Canada", "1" = "Quebec")) +
      labs(x = "Ideology (Left to Right)", y = "Predicted value",
           colour = NULL, fill = NULL,
           title   = paste("Ideology x Quebec --", dv_label),
           caption = "OLS predictions. HC1 robust SEs. Shaded area = 95% CI.") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    ggsave(file.path(params$out_reg, paste0("me_ideo_", dv, ".png")),
           width = 8, height = 5, dpi = params$dpi)
  }
)
