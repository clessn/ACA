# ==============================================================
# fairness_models.R
# Model fitting — Fairness Beliefs paper
#
# Run independently. Sources fairness_config.R for all metadata.
#
# Fits:
#   - Full ordered logit (polr) model per DV → AMEs saved
#   - Full OLS model per DV → AMEs saved (robustness)
#   - Five nested polr models per DV (M1–M5)
#
# Saves to paths$rds:
#   polr_models.rds      — fitted polr models (larger sample, full rhs)
#   coef_polr_top.rds    — AMEs from full models (reference only)
#   coef_M4_top.rds      — AMEs from M4 nested models (USED FOR PUBLICATION)
#   coef_M4_models.rds   — fitted M4 models (USED FOR APPENDIX TABLES)
#   coef_ols.rds         — OLS AMEs (robustness, matched to M4 sample)
#   fit_polr.rds         — McFadden pseudo-R2
#   fit_ols.rds          — OLS R2
#
# Also writes:
#   - Nested regression tables (.txt) to paths$nested
#   - Quebec × income interaction plots (.png) to paths$nested_int
# ==============================================================

source("code/source_redistribution/fairness_config.R")


# ==============================================================
# 1.  HELPER: AME EXTRACTOR
# ==============================================================

tidy_polr_slopes <- function(model, dv_label, data = df) {
  fml        <- formula(model)
  model_vars <- all.vars(fml)
  model_data <- data |> dplyr::select(all_of(model_vars)) |> drop_na()
  model_fit  <- MASS::polr(fml, data = model_data, Hess = TRUE)
  
  slopes_all <- avg_slopes(
    model_fit,
    vcov    = \(x) sandwich::vcovHC(x, type = "HC1"),
    newdata = model_data
  ) |>
    as_tibble() |>
    mutate(dv = dv_label)
  
  top_level <- slopes_all |>
    pull(group) |> as.character() |> unique() |> sort() |> tail(1)
  
  slopes_top <- slopes_all |>
    dplyr::filter(as.character(group) == top_level) |>
    transmute(
      dv, term,
      outcome_level = top_level,
      estimate  = round(estimate,  3),
      conf.low  = round(conf.low,  3),
      conf.high = round(conf.high, 3),
      p.value   = round(p.value,   3),
      sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".",
        TRUE ~ ""
      ),
      direction = case_when(
        conf.low  > 0 ~ "Positive",
        conf.high < 0 ~ "Negative",
        TRUE          ~ "No clear effect"
      )
    )
  
  list(top = slopes_top, full = slopes_all)
}

extract_ame_ols <- function(dv, dv_label, data = df) {
  model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
  model_data <- data |> dplyr::select(all_of(model_vars)) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", rhs)), data = model_data)
  vm         <- robust_vcov(model)
  avg_slopes(model, vcov = vm, newdata = model_data) |>
    as_tibble() |>
    transmute(
      dv = dv_label, term,
      estimate  = round(estimate,  3),
      conf.low  = round(conf.low,  3),
      conf.high = round(conf.high, 3),
      p.value   = round(p.value,   3),
      sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".",
        TRUE ~ ""
      ),
      direction = case_when(
        conf.low  > 0 ~ "Positive",
        conf.high < 0 ~ "Negative",
        TRUE          ~ "No clear effect"
      )
    )
}


# ==============================================================
# 2.  FULL POLR MODELS + AMEs
# ==============================================================

cat("\n── Fitting full polr models ──\n")

polr_models <- dv_ord_vars |>
  set_names(all_dv_labels[all_dv_vars]) |>
  map(function(dv_ord) {
    model_vars <- c(dv_ord, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    MASS::polr(as.formula(paste(dv_ord, "~", rhs)),
               data = model_data, Hess = TRUE)
  })

ame_results <- map2(polr_models, names(polr_models),
                    function(m, lbl) tidy_polr_slopes(m, lbl, data = df))

coef_polr_top <- map_dfr(ame_results, ~ .x$top)

# Full AME across all levels — saved for reference
coef_polr_full <- map_dfr(ame_results, function(res) {
  res$full |>
    transmute(
      dv, term,
      outcome_level = as.character(group),
      estimate  = round(estimate,  3),
      conf.low  = round(conf.low,  3),
      conf.high = round(conf.high, 3),
      p.value   = round(p.value,   3),
      sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".",
        TRUE ~ ""
      )
    )
})

# McFadden pseudo-R2
fit_polr <- map2_dfr(polr_models, names(polr_models), function(model, lbl) {
  dv_ord     <- as.character(formula(model)[[2]])
  model_data <- model$model
  null_mod   <- MASS::polr(as.formula(paste(dv_ord, "~ 1")),
                           data = model_data, Hess = TRUE)
  tibble(
    dv        = lbl,
    pseudo_r2 = round(as.numeric(1 - logLik(model) / logLik(null_mod)), 3),
    n         = nrow(model_data)
  )
})

# Save
saveRDS(polr_models,   file.path(paths$rds, "polr_models.rds"))
saveRDS(coef_polr_top, file.path(paths$rds, "coef_polr_top.rds"))
saveRDS(coef_polr_full,file.path(paths$rds, "coef_polr_full.rds"))
saveRDS(fit_polr,      file.path(paths$rds, "fit_polr.rds"))

write.csv(coef_polr_full,
          file.path(paths$reg, "AME_all_levels_fairness.csv"),
          row.names = FALSE)
write.csv(fit_polr,
          file.path(paths$reg, "fit_polr_fairness.csv"),
          row.names = FALSE)

cat("Full polr models saved.\n")


# ==============================================================
# 3.  OLS ROBUSTNESS MODELS + AMEs
# ==============================================================

cat("\n── Fitting OLS robustness models ──\n")

coef_ols <- map2_dfr(all_dv_vars, all_dv_labels[all_dv_vars],
                     function(dv, lbl) extract_ame_ols(dv, lbl, data = df))

fit_ols <- all_dv_vars |>
  set_names(all_dv_labels[all_dv_vars]) |>
  map_dfr(function(dv) {
    model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    model      <- lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    s          <- summary(model)
    tibble(r_squared = round(s$r.squared, 3),
           adj_r_sq  = round(s$adj.r.squared, 3),
           n         = length(s$residuals))
  }, .id = "dv")

saveRDS(coef_ols, file.path(paths$rds, "coef_ols.rds"))
saveRDS(fit_ols,  file.path(paths$rds, "fit_ols.rds"))
write.csv(fit_ols, file.path(paths$reg, "fit_ols_fairness.csv"),
          row.names = FALSE)

cat("OLS models saved.\n")


# ==============================================================
# 4.  NESTED POLR MODELS (M1–M5)
# ==============================================================

cat("\n── Fitting nested polr models ──\n")

# ── 4.1  Fit five models for one DV (constant sample) ─────────
fit_five_polr <- function(dv_ord_var) {
  all_rhs_vars <- unique(unlist(lapply(rhs_nested, function(r) {
    all.vars(as.formula(paste("~", r)))
  })))
  model_vars <- unique(c(dv_ord_var, all_rhs_vars,
                         "quebec_bin", "incomeHigh_bin", "quebec_x_income"))
  model_data <- df |>
    dplyr::select(all_of(intersect(model_vars, names(df)))) |>
    drop_na()
  cat("    N (complete cases):", nrow(model_data), "\n")
  lapply(rhs_nested, function(rhs_i) {
    tryCatch(
      MASS::polr(as.formula(paste(dv_ord_var, "~", rhs_i)),
                 data = model_data, Hess = TRUE),
      error = function(e) {
        message("    Model failed (", dv_ord_var, "): ", conditionMessage(e))
        NULL
      }
    )
  })
}

# ── 4.2  Save nested table (.tex) ─────────────────────────────
save_nested_table <- function(models, dv_label, file_path) {
  models_ok <- Filter(Negate(is.null), models)
  if (length(models_ok) == 0) {
    message("  All models NULL for ", dv_label); return(invisible(NULL))
  }
  paired <- make_vcov_list(models_ok)
  if (length(paired$models) == 0) {
    message("  All vcovs failed for ", dv_label); return(invisible(NULL))
  }
  tryCatch({
    modelsummary(
      paired$models,
      estimate  = "{estimate}{stars}",
      statistic = "({std.error})",
      vcov      = paired$vcovs,
      coef_map  = term_labels_nested,
      gof_map   = c("nobs", "logLik", "AIC"),
      output    = file_path,
      title     = paste("Ordered logit (M1--M5) ---", dv_label),
      notes     = nested_footnote,
      longtable = TRUE,
      booktabs  = TRUE
    )
    # Strip float wrapper so table appears inline in LaTeX
    tex <- readLines(file_path)
    tex <- tex[!grepl("\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\centering", tex)]
    writeLines(tex, file_path)
    cat("  Table saved:", file_path, "\n")
  }, error = function(e) cat("  TABLE ERROR:", conditionMessage(e), "\n"))
}

# ── 4.3  Interaction plot: Quebec × income (M5) ───────────────
plot_interaction <- function(model_M5, dv_label, file_path) {
  if (is.null(model_M5)) {
    message("  Skipping interaction plot — M5 NULL."); return(invisible(NULL))
  }
  model_data <- model_M5$model
  top_level  <- nlevels(model_data[[1]])
  
  other_vars <- setdiff(
    names(model_data),
    c(names(model_data)[1], "quebec_bin", "incomeHigh_bin", "quebec_x_income")
  )
  modal_val <- function(x) {
    tab <- table(x, useNA = "no")
    as.numeric(names(tab)[which.max(tab)])
  }
  baseline <- lapply(model_data[other_vars], function(col) {
    if (is.numeric(col)) mean(col, na.rm = TRUE) else modal_val(col)
  })
  
  grid <- expand.grid(quebec_bin = c(0, 1), incomeHigh_bin = c(0, 1))
  grid$quebec_x_income <- grid$quebec_bin * grid$incomeHigh_bin
  for (vn in names(baseline)) grid[[vn]] <- baseline[[vn]]
  
  preds    <- predict(model_M5, newdata = grid, type = "probs")
  pred_top <- if (is.matrix(preds)) preds[, top_level] else preds
  
  plot_df <- grid |>
    mutate(pred_top = pred_top,
           Quebec   = factor(quebec_bin,     labels = c("Rest of Canada", "Quebec")),
           Income   = factor(incomeHigh_bin, labels = c("Low / Mid", "High")))
  
  pred_ci <- tryCatch({
    top_label <- levels(model_data[[1]])[top_level]
    avg_predictions(
      model_M5,
      variables = list(quebec_bin = c(0, 1), incomeHigh_bin = c(0, 1)),
      vcov      = \(x) sandwich::vcovHC(x, type = "HC1")
    ) |>
      as_tibble() |>
      dplyr::filter(as.character(group) == top_label) |>
      mutate(Quebec = factor(quebec_bin,     labels = c("Rest of Canada", "Quebec")),
             Income = factor(incomeHigh_bin, labels = c("Low / Mid", "High")))
  }, error = function(e) { message("  CI failed: ", conditionMessage(e)); NULL })
  
  p <- ggplot(plot_df,
              aes(x = Income, y = pred_top, colour = Quebec, group = Quebec)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 3.5) +
    { if (!is.null(pred_ci)) {
      geom_errorbar(data = pred_ci,
                    aes(x = Income, y = estimate,
                        ymin = conf.low, ymax = conf.high, colour = Quebec),
                    width = 0.08, linewidth = 0.6)
    }} +
    scale_colour_manual(values = c("Rest of Canada" = "#d6604d", "Quebec" = "#2166ac")) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = scales::label_number(accuracy = 0.01)) +
    labs(x       = "Income level",
         y       = "Predicted P(response = 1 ['fair'])",
         colour  = NULL,
         title   = dv_label,
         caption = paste("Predicted probability of the top ('fair') response from M5 (ordered logit).",
                         "All other covariates held at mean / mode.",
                         "Error bars = HC1 delta-method 95% CI.")) +
    theme_cpp()
  
  ggsave(file_path, plot = p,
         width = plot_width / 1.4, height = plot_height / 1.4, dpi = plot_dpi)
  invisible(p)
}

# ── 4.4  Main loop ────────────────────────────────────────────
cat("DVs:", length(all_dv_vars), "| Models per DV: 5\n\n")

# Store all nested model lists for M4 extraction after the loop
nested_models_all <- list()

for (i in seq_along(all_dv_vars)) {
  dv_raw  <- all_dv_vars[i]
  dv_ord  <- dv_ord_vars[i]
  dv_lbl  <- all_dv_labels[[dv_raw]]
  dv_type <- if (dv_raw %in% prop_vars) "proportionality" else "reciprocity"
  slug    <- safe_filename(dv_lbl)
  
  cat("─────────────────────────────────────────────────────\n")
  cat("[", dv_type, "]  DV:", dv_lbl, "\n")
  
  models <- fit_five_polr(dv_ord)
  names(models) <- paste0("M", 1:5)
  
  # Store for M4 extraction below
  nested_models_all[[dv_lbl]] <- models
  
  save_nested_table(
    models    = models,
    dv_label  = dv_lbl,
    file_path = file.path(paths$nested,
                          paste0("nested_polr_", dv_type, "_", slug, ".tex"))
  )
  
  tryCatch(
    plot_interaction(
      model_M5  = models[["M5"]],
      dv_label  = dv_lbl,
      file_path = file.path(paths$nested_int,
                            paste0("interaction_quebec_income_", slug, ".png"))
    ),
    error = function(e) cat("  PLOT ERROR:", conditionMessage(e), "\n")
  )
}


# ==============================================================
# 5.  M4 EXTRACTION — PUBLICATION FIGURES AND TABLES
#     M4 uses a common listwise-deleted sample across all models,
#     ensuring consistency between figures and regression tables.
#     This replaces the larger-sample full polr models for all
#     publication outputs.
# ==============================================================

cat("\n── Extracting M4 AMEs for publication ──\n")

# ── 5.1  AMEs from M4 (proportionality: P(response = 1)) ──────
coef_M4_top <- map2_dfr(
  nested_models_all,
  names(nested_models_all),
  function(models, lbl) {
    m4 <- models[["M4"]]
    if (is.null(m4)) {
      message("  M4 is NULL for: ", lbl)
      return(NULL)
    }
    cat("  AME:", lbl, "\n")
    tryCatch(
      tidy_polr_slopes(m4, dv_label = lbl)$top,
      error = function(e) {
        message("  AME failed (", lbl, "): ", conditionMessage(e))
        NULL
      }
    )
  }
)

saveRDS(coef_M4_top, file.path(paths$rds, "coef_M4_top.rds"))
cat("  coef_M4_top.rds saved.\n")

# ── 5.2  Save M4 fitted models (for appendix regression tables) ─
coef_M4_models <- lapply(nested_models_all, function(models) models[["M4"]])
coef_M4_models <- Filter(Negate(is.null), coef_M4_models)
saveRDS(coef_M4_models, file.path(paths$rds, "coef_M4_models.rds"))
cat("  coef_M4_models.rds saved.\n")

# ── 5.3  OLS robustness on the same M4 sample ─────────────────
#     Re-fit OLS using the M4 variable set and common sample
#     so the robustness comparison is on identical observations.
cat("\n── Fitting OLS on M4 sample for robustness ──\n")

coef_ols_M4 <- map2_dfr(
  names(nested_models_all),
  names(nested_models_all),
  function(dv_lbl, dv_lbl2) {
    m4 <- nested_models_all[[dv_lbl]][["M4"]]
    if (is.null(m4)) return(NULL)
    
    # Recover the M4 data and DV raw variable name
    dv_raw <- all_dv_vars[all_dv_labels[all_dv_vars] == dv_lbl]
    if (length(dv_raw) == 0) return(NULL)
    
    model_data <- m4$model
    # Replace ordered DV with numeric raw DV for OLS
    model_data[[1]] <- as.numeric(as.character(model_data[[1]]))
    
    ols_fml <- as.formula(paste(
      names(model_data)[1], "~",
      paste(names(model_data)[-1], collapse = " + ")
    ))
    ols_mod <- tryCatch(
      lm(ols_fml, data = model_data),
      error = function(e) NULL
    )
    if (is.null(ols_mod)) return(NULL)
    
    avg_slopes(ols_mod,
               vcov    = \(x) sandwich::vcovHC(x, type = "HC1"),
               newdata = model_data) |>
      as_tibble() |>
      transmute(
        dv = dv_lbl, term,
        estimate  = round(estimate,  3),
        conf.low  = round(conf.low,  3),
        conf.high = round(conf.high, 3),
        p.value   = round(p.value,   3),
        sig = case_when(
          p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
          p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".",
          TRUE ~ ""
        ),
        direction = case_when(
          conf.low  > 0 ~ "Positive",
          conf.high < 0 ~ "Negative",
          TRUE          ~ "No clear effect"
        )
      )
  }
)

saveRDS(coef_ols_M4, file.path(paths$rds, "coef_ols_M4.rds"))
cat("  coef_ols_M4.rds saved.\n")

cat("\n========== MODELS COMPLETE ==========\n")
cat("RDS objects:      ", paths$rds,        "\n")
cat("  coef_M4_top.rds    — M4 AMEs (use for publication figures)\n")
cat("  coef_M4_models.rds — M4 fitted models (use for appendix tables)\n")
cat("  coef_ols_M4.rds    — OLS AMEs on M4 sample (use for robustness)\n")
cat("  coef_polr_top.rds  — full model AMEs (reference only)\n")
cat("Nested tables:    ", paths$nested,     "\n")
cat("Interaction plots:", paths$nested_int, "\n")


# ==============================================================
# SIGN AGREEMENT RATE — polr AME (M4) vs OLS (M4 sample)
# Run after fairness_models.R has been sourced or executed.
# ==============================================================

agreement <- coef_M4_top |>
  dplyr::select(dv, term, estimate_polr = estimate) |>
  inner_join(
    coef_ols_M4 |> dplyr::select(dv, term, estimate_ols = estimate),
    by = c("dv", "term")
  ) |>
  mutate(agree = sign(estimate_polr) == sign(estimate_ols)) |>
  summarise(
    n_terms       = n(),
    n_agree       = sum(agree),
    agreement_rate = round(mean(agree), 3)
  )

cat("Sign agreement rate (polr AME vs OLS, M4 sample):\n")
cat("  Terms compared:", agreement$n_terms, "\n")
cat("  Terms agreeing:", agreement$n_agree, "\n")
cat("  Agreement rate:", agreement$agreement_rate, "\n")

# Identify the disagreeing terms
coef_M4_top |>
  dplyr::select(dv, term, estimate_polr = estimate) |>
  inner_join(
    coef_ols_M4 |> dplyr::select(dv, term, estimate_ols = estimate),
    by = c("dv", "term")
  ) |>
  mutate(agree = sign(estimate_polr) == sign(estimate_ols)) |>
  dplyr::filter(!agree) |>
  arrange(dv, term) |>
  print(n = Inf)