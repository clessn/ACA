# ==============================================================
# 07_regressions_tradeoff.R
# Tradeoff batteries: Logit AME (primary) + LPM (robustness)
# Batteries: cc1, ge, tax, hc, cc2
# ==============================================================


# ── T2. FIT ALL BATTERY MODELS ───────────────────────────────

tradeoff_models <- map(batteries, function(bat) {
  list(
    lm_raw        = set_names(bat$raw,     bat$labels[bat$raw]) |>
      map(~lm(as.formula(paste(.x, "~", rhs)),
              data = df |> dplyr::select(all_of(c(.x, all.vars(as.formula(paste("~", rhs)))))) |> drop_na())),
    logit_pref    = fit_binary_models(bat$pref,    bat$labels[bat$pref],    rhs)$logit,
    lpm_pref      = fit_binary_models(bat$pref,    bat$labels[bat$pref],    rhs)$lpm,
    logit_intense = fit_binary_models(bat$intense, bat$labels[bat$intense], rhs)$logit,
    lpm_intense   = fit_binary_models(bat$intense, bat$labels[bat$intense], rhs)$lpm
  )
})


# ── T3. AVERAGE MARGINAL EFFECTS ─────────────────────────────

coef_tradeoff <- map_dfr(names(batteries), function(bat_name) {
  models <- tradeoff_models[[bat_name]]
  bind_rows(
    extract_ame_list(models$logit_pref,    paste0(bat_name, "_pref_logit")),
    extract_ame_list(models$logit_intense, paste0(bat_name, "_intense_logit")),
    extract_ame_list(models$lpm_pref,      paste0(bat_name, "_pref_lpm")),
    extract_ame_list(models$lpm_intense,   paste0(bat_name, "_intense_lpm")),
    extract_ame_list(models$lm_raw,        paste0(bat_name, "_raw"))
  ) |> mutate(battery = bat_name, battery_title = batteries[[bat_name]]$title)
})

write.csv(coef_tradeoff,
          file.path(params$out_reg, "AME_tradeoff_all.csv"), row.names = FALSE)


# ── T4. MODEL FIT ─────────────────────────────────────────────

fit_tradeoff <- map_dfr(names(batteries), function(bat_name) {
  models <- tradeoff_models[[bat_name]]
  bind_rows(
    extract_fit_lm(models$lm_raw,        "OLS_raw")       |> mutate(battery = bat_name),
    extract_fit_logit(models$logit_pref,    "Logit_pref")  |> mutate(battery = bat_name),
    extract_fit_logit(models$logit_intense, "Logit_intense")|> mutate(battery = bat_name),
    extract_fit_lm(models$lpm_pref,      "LPM_pref")      |> mutate(battery = bat_name),
    extract_fit_lm(models$lpm_intense,   "LPM_intense")   |> mutate(battery = bat_name)
  )
})

print(fit_tradeoff, n = 60)
write.csv(fit_tradeoff,
          file.path(params$out_reg, "fit_tradeoff_all.csv"), row.names = FALSE)


# ── T5. REGRESSION TABLES ─────────────────────────────────────

walk(names(batteries), function(bat_name) {
  models <- tradeoff_models[[bat_name]]
  bat    <- batteries[[bat_name]]

  save_regtable(models$logit_pref,    file.path(params$out_reg, paste0("regtable_tradeoff_", bat_name, "_pref_logit_AME.txt")),
                paste0(bat$title, " — Logit AME (first choice). HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001"),
                is_logit_ame = TRUE)

  save_regtable(models$logit_intense, file.path(params$out_reg, paste0("regtable_tradeoff_", bat_name, "_intense_logit_AME.txt")),
                paste0(bat$title, " — Logit AME (intense preference). HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001"),
                is_logit_ame = TRUE)

  save_regtable(models$lpm_pref,      file.path(params$out_reg, paste0("regtable_tradeoff_", bat_name, "_pref_lpm.txt")),
                paste0(bat$title, " — LPM robustness check. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001"),
                is_logit_ame = FALSE, gof = c("nobs", "r.squared"))

  save_regtable(models$lpm_intense,   file.path(params$out_reg, paste0("regtable_tradeoff_", bat_name, "_intense_lpm.txt")),
                paste0(bat$title, " — LPM robustness check (intense). HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001"),
                is_logit_ame = FALSE, gof = c("nobs", "r.squared"))

  save_regtable(models$lm_raw,        file.path(params$out_reg, paste0("regtable_tradeoff_", bat_name, "_raw_ols.txt")),
                paste0(bat$title, " — OLS (0-100 allocation). HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001"),
                is_logit_ame = FALSE, gof = c("nobs", "r.squared"))
})


# ── T6. COEFFICIENT PLOTS ─────────────────────────────────────

walk(names(batteries), function(bat_name) {
  bat <- batteries[[bat_name]]

  # Primary: Logit AME
  coef_lp <- coef_tradeoff |> dplyr::filter(battery == bat_name, question == paste0(bat_name, "_pref_logit"))
  coef_li <- coef_tradeoff |> dplyr::filter(battery == bat_name, question == paste0(bat_name, "_intense_logit"))
  coef_pp <- coef_tradeoff |> dplyr::filter(battery == bat_name, question == paste0(bat_name, "_pref_lpm"))
  coef_pi <- coef_tradeoff |> dplyr::filter(battery == bat_name, question == paste0(bat_name, "_intense_lpm"))

  if (nrow(coef_lp) > 0)
    plot_coefs(coef_lp, paste0(bat$title, " — Logit AME (first choice)"),
               file.path(params$out_reg, paste0("coef_tradeoff_", bat_name, "_pref_logit_AME.png")))

  if (nrow(coef_li) > 0)
    plot_coefs(coef_li, paste0(bat$title, " — Logit AME (intense preference)"),
               file.path(params$out_reg, paste0("coef_tradeoff_", bat_name, "_intense_logit_AME.png")))

  # Robustness: LPM vs Logit AME
  if (nrow(coef_lp) > 0 && nrow(coef_pp) > 0)
    plot_robustness(coef_lp, coef_pp,
                    paste0(bat$title, " — Logit AME vs. LPM (first choice, robustness)"),
                    file.path(params$out_reg, paste0("coef_tradeoff_", bat_name, "_pref_logit_vs_lpm.png")))

  if (nrow(coef_li) > 0 && nrow(coef_pi) > 0)
    plot_robustness(coef_li, coef_pi,
                    paste0(bat$title, " — Logit AME vs. LPM (intense preference, robustness)"),
                    file.path(params$out_reg, paste0("coef_tradeoff_", bat_name, "_intense_logit_vs_lpm.png")))
})


# ── T7. R-SQUARED PLOTS ───────────────────────────────────────

walk(names(batteries), function(bat_name) {
  bat <- batteries[[bat_name]]

  fit_tradeoff |> dplyr::filter(battery == bat_name, model_type == "Logit_pref") |>
    plot_r2("pseudo_r2", paste0(bat$title, " — Logit pseudo-R2 (first choice)"),
            file.path(params$out_reg, paste0("r2_tradeoff_", bat_name, "_pref_logit.png")))

  fit_tradeoff |> dplyr::filter(battery == bat_name, model_type == "Logit_intense") |>
    plot_r2("pseudo_r2", paste0(bat$title, " — Logit pseudo-R2 (intense preference)"),
            file.path(params$out_reg, paste0("r2_tradeoff_", bat_name, "_intense_logit.png")))

  fit_tradeoff |> dplyr::filter(battery == bat_name, model_type == "LPM_pref") |>
    plot_r2("adj_r_sq", paste0(bat$title, " — LPM adjusted R2 (first choice, robustness)"),
            file.path(params$out_reg, paste0("r2_tradeoff_", bat_name, "_pref_lpm.png")))

  fit_tradeoff |> dplyr::filter(battery == bat_name, model_type == "LPM_intense") |>
    plot_r2("adj_r_sq", paste0(bat$title, " — LPM adjusted R2 (intense preference, robustness)"),
            file.path(params$out_reg, paste0("r2_tradeoff_", bat_name, "_intense_lpm.png")))
})


# ── T8. DIAGNOSTICS ───────────────────────────────────────────

cat("\n========== Most consistently significant predictors (Logit AME _pref) ==========\n")
coef_tradeoff |>
  dplyr::filter(grepl("_pref_logit$", question), sig %in% c("*", "**", "***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

cat("\n========== Most consistently significant predictors (Logit AME _intense) ==========\n")
coef_tradeoff |>
  dplyr::filter(grepl("_intense_logit$", question), sig %in% c("*", "**", "***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

walk(names(batteries), function(bat_name) {
  check_direction_agreement(
    coef_tradeoff |> dplyr::filter(battery == bat_name, grepl("_pref_logit$",    question)),
    coef_tradeoff |> dplyr::filter(battery == bat_name, grepl("_pref_lpm$",      question)),
    paste("--", bat_name, "_pref")
  )
  check_direction_agreement(
    coef_tradeoff |> dplyr::filter(battery == bat_name, grepl("_intense_logit$", question)),
    coef_tradeoff |> dplyr::filter(battery == bat_name, grepl("_intense_lpm$",   question)),
    paste("--", bat_name, "_intense")
  )
})

cat("\nAMEs saved to:", file.path(params$out_reg, "AME_tradeoff_all.csv"), "\n")
cat("Model fit saved to:", file.path(params$out_reg, "fit_tradeoff_all.csv"), "\n")
