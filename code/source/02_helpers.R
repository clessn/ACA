# ==============================================================
# 02_helpers.R
# All reusable functions for modelling, AME extraction,
# regression tables, and plotting.
# ==============================================================


# ── 1. MODELLING HELPERS ──────────────────────────────────────

#' HC1 robust variance-covariance matrix
robust_vcov <- function(model) vcovHC(model, type = "HC1")


#' Fit logit + LPM pair for a vector of binary DVs
#'
#' @param dv_vec    Character vector of DV column names
#' @param label_vec Character vector of human-readable DV labels (same length)
#' @param rhs       RHS formula string
#' @param data      Data frame
#' @return List with elements `logit` and `lpm`, each a named list of models
fit_binary_models <- function(dv_vec, label_vec, rhs, data = df) {
  dvs <- set_names(dv_vec, label_vec)
  list(
    logit = map(dvs, function(dv) {
      model_data <- data |>
        dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |>
        drop_na()
      glm(as.formula(paste(dv, "~", rhs)), data = model_data,
          family = binomial(link = "logit"))
    }),
    lpm = map(dvs, function(dv) {
      model_data <- data |>
        dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |>
        drop_na()
      lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    })
  )
}


# ── 2. AME EXTRACTION ─────────────────────────────────────────

#' Extract average marginal effects with HC1 robust SEs
#'
#' Works for both lm() and glm() models. The single source of truth
#' for all AME extraction in the pipeline — replaces all ad-hoc
#' avg_slopes + transmute blocks previously repeated across sections.
#'
#' @param model       A fitted lm or glm model
#' @param dv_label    Human-readable label for the DV
#' @param question_tag String tag identifying the battery/question group
#' @param data        Data frame (used to reconstruct clean model data)
#' @return A tibble of AMEs with sig stars and direction labels
extract_ame <- function(model, dv_label, question_tag, data = df) {
  fml        <- formula(model)
  model_vars <- all.vars(fml)
  model_data <- data |> dplyr::select(all_of(model_vars)) |> drop_na()
  vm         <- robust_vcov(model)
  avg_slopes(model, vcov = vm, newdata = model_data) |>
    as_tibble() |>
    transmute(
      question  = question_tag,
      dv        = dv_label,
      term,
      estimate  = round(estimate, 3),
      conf.low  = round(conf.low,  3),
      conf.high = round(conf.high, 3),
      p.value   = round(p.value,   3),
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.10  ~ ".",
        TRUE            ~ ""
      ),
      direction = case_when(
        conf.low  > 0 ~ "Positive",
        conf.high < 0 ~ "Negative",
        TRUE          ~ "No clear effect"
      )
    )
}


#' Extract AMEs for a named list of models
#'
#' Convenience wrapper around extract_ame() for use with map2_dfr.
#'
#' @param model_list  Named list of fitted models (names = DV labels)
#' @param question_tag String tag for the battery/question group
#' @param data        Data frame
#' @return Row-bound tibble of AMEs across all models
extract_ame_list <- function(model_list, question_tag, data = df) {
  map2_dfr(model_list, names(model_list), extract_ame,
           question_tag = question_tag, data = data)
}


#' Bivariate contrast interpretation (used in section 6)
interpret_contrast <- function(dv, dv_label, iv, data) {
  iv_var <- iv$var
  iv_low  <- as.character(iv$low)
  iv_high <- as.character(iv$high)
  model_data    <- data |> dplyr::select(all_of(c(dv, iv_var))) |> drop_na()
  model         <- lm(as.formula(paste(dv, "~", iv_var)), data = model_data)
  contrast_spec <- list(c(iv$low, iv$high))
  names(contrast_spec) <- iv_var
  tryCatch({
    avg_comparisons(model, variables = contrast_spec, vcov = robust_vcov(model)) |>
      as_tibble() |>
      transmute(
        iv        = iv$label,
        dv        = dv_label,
        contrast  = paste(iv_low, "vs", iv_high),
        estimate  = round(estimate, 3),
        conf.low  = round(conf.low,  3),
        conf.high = round(conf.high, 3),
        p.value   = round(p.value,   3),
        sig = case_when(
          p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
          p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".",
          TRUE ~ ""
        ),
        direction = case_when(
          conf.low  > 0 ~ paste(iv_high, ">", iv_low),
          conf.high < 0 ~ paste(iv_low,  ">", iv_high),
          TRUE          ~ "No clear difference"
        )
      )
  }, error = function(e) {
    cat("FAILED:", iv_var, "~", dv, "\n  Error:", conditionMessage(e), "\n")
    tibble(iv = iv$label, dv = dv_label, contrast = paste(iv_low, "vs", iv_high),
           estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
           p.value = NA_real_, sig = NA_character_, direction = "ERROR")
  })
}


# ── 3. MODEL FIT EXTRACTION ───────────────────────────────────

#' Extract fit stats from a named list of lm models
extract_fit_lm <- function(model_list, model_type_label) {
  map2_dfr(model_list, names(model_list), function(model, dv_label) {
    s <- summary(model)
    tibble(
      dv         = dv_label,
      model_type = model_type_label,
      r_squared  = round(s$r.squared,     3),
      adj_r_sq   = round(s$adj.r.squared, 3),
      n          = length(s$residuals)
    )
  })
}

#' Extract fit stats from a named list of glm (logit) models
extract_fit_logit <- function(model_list, model_type_label) {
  map2_dfr(model_list, names(model_list), function(model, dv_label) {
    s <- summary.glm(model)
    tibble(
      dv         = dv_label,
      model_type = model_type_label,
      pseudo_r2  = round(1 - (s$deviance / s$null.deviance), 3),
      n          = length(s$residuals)
    )
  })
}


# ── 4. REGRESSION TABLE HELPER ────────────────────────────────

#' Save a modelsummary regression table to a text file
#'
#' @param models      Named list of fitted models
#' @param file_path   Output path
#' @param notes       Footnote string
#' @param is_logit_ame Logical; if TRUE, extracts AMEs from logit models first
#' @param gof         GOF statistics to include
save_regtable <- function(models, file_path, notes,
                          is_logit_ame = FALSE,
                          gof = c("nobs", "r.squared", "adj.r.squared")) {
  if (is_logit_ame) {
    models <- map(models, ~avg_slopes(.x, vcov = robust_vcov(.x)))
    gof    <- "nobs"
    vcov_arg <- NULL
  } else {
    vcov_arg <- map(models, robust_vcov)
  }
  modelsummary(
    models,
    estimate  = "{estimate}{stars}",
    statistic = "({std.error})",
    vcov      = vcov_arg,
    coef_map  = term_labels,
    gof_map   = gof,
    output    = file_path,
    notes     = notes
  )
}


# ── 5. PLOTTING HELPERS ───────────────────────────────────────

#' Coefficient plot: one panel per DV, coloured by direction
plot_coefs <- function(coef_df, title_str = NULL, file_path,
                       width = params$plot_width, height = params$plot_height) {
  coef_df |>
    mutate(
      term = recode(term, !!!term_labels),
      dv   = factor(dv, levels = intersect(dv_order, unique(dv)))
    ) |>
    ggplot(aes(x = estimate, y = factor(term, levels = rev(iv_order)), color = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    scale_color_manual(
      values = c("Positive" = "#2166ac", "Negative" = "#d6604d",
                 "No clear effect" = "grey60")
    ) +
    facet_wrap(~dv, scales = "free_x") +
    labs(x = "Average marginal effect (HC1 robust SEs)",
         y = NULL, color = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file_path, width = width, height = height, dpi = params$dpi)
}

#' Coefficient plot with policy-type strip colouring (investment vs fiscal consolidation)
#'
#' Identical to plot_coefs() but shades facet strip backgrounds to distinguish
#' spending/investment DVs from fiscal-consolidation DVs (tax/debt reduction).
#'
#' @param coef_df      AME data frame
#' @param title_str    Plot title
#' @param file_path    Output PNG path
#' @param type_map     Named character vector: DV label → policy type string.
#'                     E.g. c("Healthcare" = "Investment", "Tax reduction" = "Fiscal consolidation")
#' @param type_colours Named colour vector keyed on type_map values
plot_coefs_typed <- function(coef_df, title_str = NULL, file_path,
                             type_map,
                             type_colours = c(
                               "Investment"            = "#2166ac",
                               "Fiscal consolidation"  = "#b2182b"
                             ),
                             width  = params$plot_width,
                             height = params$plot_height) {
  plot_df <- coef_df |>
    mutate(
      term        = recode(term, !!!term_labels),
      dv          = factor(dv, levels = intersect(dv_order, unique(dv))),
      policy_type = recode(as.character(dv), !!!type_map, .default = "Other")
    )

  # Build a strip-colour lookup aligned to the factor levels actually present
  dv_levels   <- levels(plot_df$dv)
  strip_fills <- type_colours[recode(dv_levels, !!!type_map, .default = "Other")]
  strip_fills[is.na(strip_fills)] <- "grey80"

  p <- ggplot(plot_df,
              aes(x = estimate, y = factor(term, levels = rev(iv_order)), color = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    scale_color_manual(
      values = c("Positive" = "#2166ac", "Negative" = "#d6604d",
                 "No clear effect" = "grey60")
    ) +
    facet_wrap(~dv, scales = "free_x") +
    labs(
      x       = "Average marginal effect (HC1 robust SEs)",
      y       = NULL,
      color   = NULL,
      caption = paste0(
        "Strip colour: ",
        paste(names(type_colours), collapse = " = shaded; "),
        ". 95% CIs shown."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

  # Apply per-strip background colours using ggh4x or ggplot2 strip theming
  # We use a rect annotation approach via strip.background per facet level
  # by building a named theme override with ggh4x::facet_wrap2 if available,
  # otherwise fall back to a legend-based annotation in the caption.
  if (requireNamespace("ggh4x", quietly = TRUE)) {
    strip_themes <- lapply(strip_fills, function(col) {
      element_rect(fill = col, color = NA)
    })
    names(strip_themes) <- dv_levels
    p <- p + ggh4x::facetted_pos_scales(
      # ggh4x approach: use strip_themed
    )
    # Use strip_themed for coloured strips
    p <- p +
      ggh4x::facet_wrap2(
        ~dv, scales = "free_x",
        strip = ggh4x::strip_themed(
          background_x = lapply(strip_fills, element_rect)
        )
      )
  } else {
    # Fallback: add a type legend via colour scale on the facet label
    # by annotating with a policy_type fill guide
    p <- p +
      geom_rect(
        data = plot_df |> distinct(dv, policy_type),
        aes(fill = policy_type),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.08,
        inherit.aes = FALSE
      ) +
      scale_fill_manual(
        values = type_colours,
        name   = "Policy type"
      )
  }

  ggsave(file_path, plot = p, width = width, height = height, dpi = params$dpi)
}


#' Broad vs narrow scope plots: one plot per outcome type (pref / intense)
#'
#' Each plot has two facets (Broad / Narrow), with Home care and Childcare
#' dodged within each facet — exactly matching plot_cross_battery_response.
#'
#' @param coef_df      AME data frame for one outcome type
#' @param outcome_slug "pref" or "intense" — appended to filename
#' @param file_prefix  Base output path (outcome_slug is appended automatically)
#' @param dv_to_scope  Named vector: DV label → scope string
#' @param dv_to_domain Named vector: DV label → domain string
plot_uc_scope_by_scope <- function(coef_df, outcome_slug, file_prefix,
                                   dv_to_scope, dv_to_domain,
                                   width  = params$plot_width,
                                   height = params$plot_height) {
  scope_levels <- c("Broad beneficiaries", "Narrow beneficiaries")

  coef_plot <- coef_df |>
    mutate(
      scope  = recode(dv, !!!dv_to_scope),
      domain = recode(dv, !!!dv_to_domain),
      term   = recode(term, !!!term_labels),
      scope  = factor(scope, levels = scope_levels)
    )

  if (nrow(coef_plot) == 0) {
    warning("No data for UC scope plot: ", outcome_slug)
    return(invisible(NULL))
  }

  file_path <- paste0(file_prefix, "_", outcome_slug, ".png")

  ggplot(coef_plot,
         aes(x     = estimate,
             y     = factor(term, levels = rev(iv_order)),
             color = domain,
             shape = domain)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.5, position = position_dodge(width = 0.55)) +
    geom_errorbarh(
      aes(xmin = conf.low, xmax = conf.high),
      height = 0.25,
      position = position_dodge(width = 0.55)
    ) +
    scale_color_manual(
      values = c("Home care" = "#2166ac", "Childcare" = "#d6604d"),
      name   = "Policy domain"
    ) +
    scale_shape_manual(
      values = c("Home care" = 16, "Childcare" = 17),
      name   = "Policy domain"
    ) +
    facet_wrap(~scope, nrow = 1, scales = "free_x") +
    labs(
      x       = "Average marginal effect (Logit AME, HC1 robust SEs)",
      y       = NULL,
      caption = "95% CIs shown. Each facet = beneficiary scope, both policy domains overlaid."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      strip.text      = element_text(face = "bold"),
      panel.spacing   = unit(1, "lines")
    )

  ggsave(file_path, width = width, height = height, dpi = params$dpi)
  cat("Saved:", file_path, "\n")
}


#' Robustness plot: LPM vs Logit AME side by side, one panel per DV
plot_robustness <- function(coef_logit, coef_lpm, title_str = NULL, file_path,
                            ncol = 2,
                            width = params$plot_width,
                            height = params$plot_height) {
  bind_rows(
    coef_logit |> mutate(model = "Logit AME"),
    coef_lpm   |> mutate(model = "LPM")
  ) |>
    mutate(
      term = recode(term, !!!term_labels),
      dv   = factor(dv, levels = intersect(dv_order, unique(dv)))
    ) |>
    ggplot(aes(x = estimate, y = factor(term, levels = rev(iv_order)),
               color = model, shape = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2, position = position_dodge(width = 0.5)) +
    scale_color_manual(values = c("Logit AME" = "#d6604d", "LPM" = "#2166ac")) +
    facet_wrap(~dv, ncol = ncol) +
    labs(x = "Estimated effect", y = NULL, color = NULL, shape = NULL,
         caption = "HC1 robust SEs. 95% CI.") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file_path, width = width, height = height, dpi = params$dpi)
}

#' R-squared / pseudo-R2 bar chart
plot_r2 <- function(fit_df, r2_col = "adj_r_sq", title_str = NULL, file_path,
                    width = 8, height = 5) {
  fit_df |>
    mutate(
      fit_level = case_when(
        .data[[r2_col]] >= 0.07 ~ "High (>=0.07)",
        .data[[r2_col]] >= 0.03 ~ "Moderate (0.03-0.07)",
        TRUE                    ~ "Low (<0.03)"
      ),
      fit_level = factor(fit_level,
                         levels = c("High (>=0.07)", "Moderate (0.03-0.07)", "Low (<0.03)")),
      dv = fct_reorder(dv, .data[[r2_col]])
    ) |>
    ggplot(aes(x = .data[[r2_col]], y = dv, fill = fit_level)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.3f", .data[[r2_col]])), hjust = -0.15, size = 3.2) +
    scale_fill_manual(
      values = c("High (>=0.07)"        = "#2166ac",
                 "Moderate (0.03-0.07)" = "#92c5de",
                 "Low (<0.03)"          = "#d6604d")
    ) +
    scale_x_continuous(
      limits = function(x) c(0, max(x[2], 0.05) * 1.25),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    labs(
      x     = ifelse(r2_col == "pseudo_r2", "McFadden pseudo-R2", "Adjusted R2"),
      y     = NULL, fill = "Model fit"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank())
  ggsave(file_path, width = width, height = height, dpi = params$dpi)
}


#' Direction agreement diagnostic: flag significant terms where LPM and
#' Logit AME disagree on direction
check_direction_agreement <- function(coef_logit, coef_lpm, label = "") {
  cat("\n========== LPM vs Logit direction agreement", label, "==========\n")
  bind_rows(
    coef_logit |> mutate(model = "Logit"),
    coef_lpm   |> mutate(model = "LPM")
  ) |>
    dplyr::filter(sig %in% c("*", "**", "***")) |>
    dplyr::select(dv, term, model, direction) |>
    pivot_wider(names_from = model, values_from = direction) |>
    dplyr::filter(LPM != Logit) |>
    print()
}
