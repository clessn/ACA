# ==============================================================
# 15_publication_tradeoff.R
# Publication-ready figures and tables — Tradeoff article
#
# STANDALONE: source master.R first so all pipeline objects
# are in scope, then run this script independently.
#
# Required objects from pipeline:
#   df, params, batteries, battery_titles,
#   term_labels, iv_order, dv_order, hyp_vars,
#   robust_vcov(), extract_ame(), plot_coefs(), plot_robustness(),
#   rhs (full model RHS formula string)
#
# OUTPUTS
# ── Main text ──────────────────────────────────────────────────
#   Figure 6  — Policy priorities (pref): all five policy options
#   Figure 7  — Taxation preferences (pref): four options
#   Figure 8  — Green economy + childcare tradeoffs (pref)
#   Figure 9  — Intensity: no-spend on green economy, childcare,
#               no tax increase (three DVs, one figure)
#
# ── Appendix ───────────────────────────────────────────────────
#   Table B1  — Nested logit: childcare funding (cc1), pref DVs
#   Table B2  — Nested logit: green economy (ge), pref DVs
#   Table B3  — Nested logit: taxation (tax), pref DVs
#   (hc and cc2 target-group batteries excluded: pseudo-R2 < 0.02,
#    no significant effects across all models.)
#
# Nested model progression (M1–M4 shown in appendix tables;
# M5 interaction and M6 total-Quebec check omitted — no effects):
#   M1: Quebec + other regions + demographics
#   M2: M1 + Québécois-first identity
#   M3: M2 + ideology (right scale) + partisanship (fed + prov vote,
#       PLC, PCC)
#   M4: M3 + federal inst. trust + provincial inst. trust
# ==============================================================


# ==============================================================
# 0.  PATHS AND PACKAGES
# ==============================================================

library(tidyverse)
library(marginaleffects)
library(modelsummary)
library(sandwich)
library(lmtest)
library(ggplot2)
library(kableExtra)

select <- dplyr::select
filter <- dplyr::filter

# ── Output directories ────────────────────────────────────────
pub_root <- "/Users/shannondinan/Library/CloudStorage/Dropbox/_RESEARCH/_COLLABORATIONS/_GitHub/ACA/code/source_tradeoffs/graphs/publication_tradeoff"

pub_main <- file.path(pub_root, "main")
pub_app  <- file.path(pub_root, "appendix")

dir.create(pub_main, recursive = TRUE, showWarnings = FALSE)
dir.create(pub_app,  recursive = TRUE, showWarnings = FALSE)

cat("Output — main:     ", pub_main, "\n")
cat("Output — appendix: ", pub_app,  "\n\n")

# ── Shared figure dimensions ──────────────────────────────────
fig_w      <- params$plot_width
fig_h      <- params$plot_height
fig_dpi    <- params$dpi


# ==============================================================
# 1.  NESTED RHS FORMULAS (M1–M4)
#     Ontario = reference region throughout.
#     Sample held constant across M1–M4 via listwise deletion
#     on the union of all RHS variables.
# ==============================================================

# Core: region + demographics (no identity, ideology, or trust)
rhs_n1 <- "quebec_bin + alberta_bin + region_eastcoast_bin +
            incomeHigh_bin + ses_male_bin + age18_34_bin + age55plus_bin +
            univ_educ_bin + employ_fulltime_bin + children_bin"

# M2: + Québécois-first identity
rhs_n2 <- paste(rhs_n1, "+ ideo_define_QC_first_bin")

# M3: + ideology and partisanship
rhs_n3 <- paste(rhs_n2,
                "+ ideo_right_num + vote_PLC_bin + vote_PCC_bin",
                "+ ideo_vote_fed_left + ideo_vote_prov_left")

# M4: + institutional trust (full model, matches main pipeline rhs
#     minus social trust which is not in the tradeoff pipeline)
rhs_n4 <- paste(rhs_n3,
                "+ trust_inst_fed_bin + trust_inst_prov_bin")

rhs_nested <- list(M1 = rhs_n1, M2 = rhs_n2, M3 = rhs_n3, M4 = rhs_n4)

# ── Term labels for nested tables ─────────────────────────────
# Ordered to match appendix table layout:
# regional block first, then demographics, then ideology, then trust
term_labels_nested <- c(
  # Region (focal block)
  "quebec_bin"               = "Quebec",
  "alberta_bin"              = "Alberta",
  "region_eastcoast_bin"     = "Atlantic Canada",
  "ideo_define_QC_first_bin" = "Québécois identity first",
  # Demographics
  "incomeHigh_bin"           = "Income: High",
  "ses_male_bin"             = "Male",
  "age18_34_bin"             = "Age 18–34",
  "age55plus_bin"            = "Age 55+",
  "univ_educ_bin"            = "University education",
  "employ_fulltime_bin"      = "Employed full-time",
  "children_bin"             = "Has children",
  # Ideology and partisanship
  "ideo_right_num"           = "Ideology (Right scale)",
  "vote_PLC_bin"             = "Fed. vote: Liberal",
  "vote_PCC_bin"             = "Fed. vote: Conservative",
  "ideo_vote_fed_left"       = "Fed. vote (Left scale)",
  "ideo_vote_prov_left"      = "Prov. vote (Left scale)",
  # Trust
  "trust_inst_fed_bin"       = "Trust: Federal government",
  "trust_inst_prov_bin"      = "Trust: Provincial government"
)


# ==============================================================
# 2.  HELPERS
# ==============================================================

# ── 2.1  Fit four nested logit models for one DV ──────────────
#     Listwise deletion on union of all M1–M4 variables ensures
#     the same sample across all models.
fit_nested_logits <- function(dv, data = df) {

  all_rhs_vars <- unique(unlist(lapply(rhs_nested, function(r) {
    all.vars(as.formula(paste("~", r)))
  })))

  model_vars <- unique(c(dv, all_rhs_vars))
  model_data <- data |>
    dplyr::select(all_of(intersect(model_vars, names(data)))) |>
    drop_na()

  cat("    N (complete cases):", nrow(model_data), "\n")

  lapply(rhs_nested, function(rhs) {
    tryCatch(
      glm(as.formula(paste(dv, "~", rhs)),
          data   = model_data,
          family = binomial(link = "logit")),
      error = function(e) {
        message("    Model failed (", dv, "): ", conditionMessage(e))
        NULL
      }
    )
  })
}

# ── 2.2  Save nested logit table (LaTeX) ──────────────────────
#     Log-odds coefficients + HC1 robust SEs + stars.
#     One column per DV, rows = predictors.
save_nested_latex <- function(models_by_dv, dv_labels, file_path,
                              table_title, table_label, footnote_str) {

  # models_by_dv: named list of lists — outer name = DV label,
  #               inner names = M1/M2/M3/M4
  # Flatten to a single named list for modelsummary:
  #   column names = "DV label - M1", "DV label - M2", etc.
  flat_models <- list()
  flat_vcovs  <- list()

  for (dv_lbl in dv_labels) {
    m_list <- models_by_dv[[dv_lbl]]
    for (mn in names(m_list)) {
      m <- m_list[[mn]]
      if (is.null(m)) next
      col_name <- paste0(mn)   # just M1/M2/M3/M4 — DV shown via column grouping
      key <- paste0(dv_lbl, "__", mn)
      flat_models[[key]] <- m
      flat_vcovs[[key]]  <- tryCatch(
        sandwich::vcovHC(m, type = "HC1"),
        error = function(e) vcov(m)
      )
    }
  }

  if (length(flat_models) == 0) {
    message("  No models to table for: ", table_title)
    return(invisible(NULL))
  }

  # Rename columns for display: M1 M2 M3 M4 repeated per DV
  display_names <- sub(".*__", "", names(flat_models))

  tryCatch({
    modelsummary(
      flat_models,
      estimate   = "{estimate}{stars}",
      statistic  = "({std.error})",
      vcov       = flat_vcovs,
      coef_map   = term_labels_nested,
      gof_map    = c("nobs", "logLik", "AIC"),
      output     = file_path,
      title      = table_title,
      col.names  = display_names,
      notes      = footnote_str
    )
    cat("  Saved:", file_path, "\n")
  }, error = function(e) {
    cat("  TABLE ERROR:", conditionMessage(e), "\n")
  })
}

# ── 2.3  CPP-style ggplot theme (matches pipeline) ────────────
theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text               = element_text(family = "serif"),
      panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      strip.background   = element_blank(),
      strip.text         = element_text(face = "bold", size = rel(0.95)),
      legend.position    = "bottom",
      legend.key.size    = unit(0.4, "cm"),
      plot.caption       = element_text(size = rel(0.8), colour = "grey40",
                                        hjust = 0, margin = margin(t = 6))
    )
}

# ── 2.4  AME coefficient plot (wraps pipeline plot_coefs) ─────
#     Filters to hyp_vars only for main figures.
plot_pub_coefs <- function(coef_df, file_path,
                           ncol_facet = 2,
                           keep_hyp   = TRUE,
                           width      = fig_w,
                           height     = fig_h,
                           caption    = "Logit AME on P(first choice = 1). HC1 robust SEs, 95% CI. Ontario = reference region.") {
  if (keep_hyp) {
    coef_df <- coef_df |> dplyr::filter(term %in% hyp_vars)
  }

  coef_df |>
    mutate(
      term = dplyr::recode(term, !!!term_labels),
      dv   = factor(dv, levels = intersect(dv_order, unique(dv)))
    ) |>
    ggplot(aes(x = estimate,
               y = factor(term, levels = rev(iv_order)),
               color = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    scale_color_manual(values = c(
      "Positive"        = "#2166ac",
      "Negative"        = "#d6604d",
      "No clear effect" = "grey60"
    )) +
    facet_wrap(~ dv, ncol = ncol_facet, scales = "free_x") +
    labs(
      x       = "Average marginal effect (HC1 robust SEs)",
      y       = NULL,
      color   = NULL,
      caption = caption
    ) +
    theme_pub()
}


# ==============================================================
# 3.  RE-FIT MAIN MODELS (full rhs) FOR PUBLICATION FIGURES
#     Uses the same rhs from 03_variable_definitions.R.
#     If pipeline objects already exist (e.g. from sourcing master.R),
#     this section can be skipped — AME objects are reused directly.
# ==============================================================

cat("\n── Fitting main models for publication figures ──\n")

# Helper: fit logit + extract AME for a vector of DVs
fit_and_extract <- function(dv_vec, label_vec, question_tag) {
  map2_dfr(dv_vec, label_vec, function(dv, lbl) {
    if (!dv %in% names(df)) {
      message("  Column not found: ", dv)
      return(NULL)
    }
    model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |>
      dplyr::select(all_of(intersect(model_vars, names(df)))) |>
      drop_na()
    m <- tryCatch(
      glm(as.formula(paste(dv, "~", rhs)),
          data = model_data, family = binomial(link = "logit")),
      error = function(e) { message("  Failed: ", dv); NULL }
    )
    if (is.null(m)) return(NULL)
    extract_ame(m, dv_label = lbl, question_tag = question_tag)
  })
}

# ── Priority first-choice (pref): all five policy options ─────
prio_pref_labels <- unname(prio_policy_labels[prio_policies])
ame_prio_pref <- fit_and_extract(
  dv_vec       = prio_pref_vars,
  label_vec    = prio_pref_labels,
  question_tag = "prio_pref"
)

# ── Tradeoff batteries: pref DVs ─────────────────────────────
# cc1: childcare funding
ame_cc1_pref <- fit_and_extract(
  dv_vec       = batteries$cc1$pref,
  label_vec    = unname(batteries$cc1$labels[batteries$cc1$pref]),
  question_tag = "cc1_pref"
)

# ge: green economy funding
ame_ge_pref <- fit_and_extract(
  dv_vec       = batteries$ge$pref,
  label_vec    = unname(batteries$ge$labels[batteries$ge$pref]),
  question_tag = "ge_pref"
)

# tax: taxation preferences
ame_tax_pref <- fit_and_extract(
  dv_vec       = batteries$tax$pref,
  label_vec    = unname(batteries$tax$labels[batteries$tax$pref]),
  question_tag = "tax_pref"
)

# ── Intensity DVs for Figure 9 (three no-spend DVs only) ──────
intense_fig9_dvs <- c(
  "tradeoff_ge_no_spend_intense",
  "tradeoff_cc1_no_spend_intense",
  "tradeoff_tax_less_services_intense"
)
intense_fig9_labels <- c(
  "Don't spend more on green economy (intense)",
  "Don't spend more on childcare (intense)",
  "No tax increase, fewer services (intense)"
)

ame_intense_fig9 <- fit_and_extract(
  dv_vec       = intense_fig9_dvs,
  label_vec    = intense_fig9_labels,
  question_tag = "intense_fig9"
)


# ==============================================================
# 4.  FIGURE 6 — POLICY PRIORITIES (pref)
#     All five priority options. Hypothesis variables only.
# ==============================================================

cat("\n── Figure 6: Policy priorities (pref)\n")

p6 <- plot_pub_coefs(
  coef_df    = ame_prio_pref,
  file_path  = NULL,
  ncol_facet = 3,
  width      = fig_w + 4,
  height     = fig_h + 2,
  caption    = "Logit AME on P(first choice = 1). HC1 robust SEs, 95% CI. Ontario = reference region."
)

ggsave(
  file.path(pub_main, "figure6_prio_pref.png"),
  plot   = p6,
  width  = fig_w + 4,
  height = fig_h + 2,
  dpi    = fig_dpi
)
cat("   Saved: figure6_prio_pref.png\n")


# ==============================================================
# 5.  FIGURE 7 — TAXATION PREFERENCES (pref)
#     Four tax options. Hypothesis variables only.
# ==============================================================

cat("\n── Figure 7: Taxation preferences (pref)\n")

p7 <- plot_pub_coefs(
  coef_df    = ame_tax_pref,
  file_path  = NULL,
  ncol_facet = 2,
  width      = fig_w,
  height     = fig_h,
  caption    = "Logit AME on P(first choice = 1). HC1 robust SEs, 95% CI. Ontario = reference region."
)

ggsave(
  file.path(pub_main, "figure7_tax_pref.png"),
  plot   = p7,
  width  = fig_w,
  height = fig_h,
  dpi    = fig_dpi
)
cat("   Saved: figure7_tax_pref.png\n")


# ==============================================================
# 6.  FIGURE 8 — GREEN ECONOMY + CHILDCARE TRADEOFFS (pref)
#     ge and cc1 pref DVs combined in one figure.
#     Hypothesis variables only.
# ==============================================================

cat("\n── Figure 8: Green economy + childcare tradeoffs (pref)\n")

ame_fig8 <- bind_rows(ame_ge_pref, ame_cc1_pref)

p8 <- plot_pub_coefs(
  coef_df    = ame_fig8,
  file_path  = NULL,
  ncol_facet = 2,
  width      = fig_w + 2,
  height     = fig_h + 3,
  caption    = "Logit AME on P(first choice = 1). HC1 robust SEs, 95% CI. Ontario = reference region."
)

ggsave(
  file.path(pub_main, "figure8_ge_cc1_pref.png"),
  plot   = p8,
  width  = fig_w + 2,
  height = fig_h + 3,
  dpi    = fig_dpi
)
cat("   Saved: figure8_ge_cc1_pref.png\n")


# ==============================================================
# 7.  FIGURE 9 — INTENSITY: NO-SPEND (green economy,
#     childcare, no tax increase)
#     Three DVs combined. Hypothesis variables only.
# ==============================================================

cat("\n── Figure 9: Intensity — no-spend tradeoffs\n")

p9 <- plot_pub_coefs(
  coef_df    = ame_intense_fig9,
  file_path  = NULL,
  ncol_facet = 3,
  width      = fig_w + 4,
  height     = fig_h,
  caption    = paste(
    "Logit AME on P(intense preference = 1).",
    "HC1 robust SEs, 95% CI. Ontario = reference region.",
    "Intense = respondent allocated 50% or more of points to this option."
  )
)

ggsave(
  file.path(pub_main, "figure9_intense_nospend.png"),
  plot   = p9,
  width  = fig_w + 4,
  height = fig_h,
  dpi    = fig_dpi
)
cat("   Saved: figure9_intense_nospend.png\n")


# ==============================================================
# 8.  APPENDIX TABLES — NESTED LOGIT (LaTeX)
#     Tables B1 (cc1), B2 (ge), B3 (tax).
#     Columns: M1 M2 M3 M4 repeated per DV within battery.
#     Rows: predictors from term_labels_nested.
#     hc and cc2 excluded (pseudo-R2 < 0.02, no significant effects).
# ==============================================================

nested_footnote <- paste(
  "Binary logit. Log-odds coefficients.",
  "HC1 robust SEs in parentheses.",
  "Sample held constant across M1--M4 (listwise deletion on union of all RHS variables).",
  "Ontario = reference region.",
  "M1 = Quebec + other regions + demographics.",
  "M2 = M1 + Qu\\'ebecois-first identity.",
  "M3 = M2 + ideology (right scale) + partisanship (Liberal, Conservative,",
  "federal left vote, provincial left vote).",
  "M4 = M3 + federal institutional trust + provincial institutional trust.",
  "* $p<0.05$, ** $p<0.01$, *** $p<0.001$"
)

# ── Battery loop: cc1, ge, tax ─────────────────────────────────
app_batteries <- c("cc1", "ge", "tax")
app_table_nums <- c(cc1 = "B1", ge = "B2", tax = "B3")
app_table_titles <- c(
  cc1 = "Nested Logit — Childcare Funding Tradeoffs (First-Choice Preference)",
  ge  = "Nested Logit — Green Economy Funding Tradeoffs (First-Choice Preference)",
  tax = "Nested Logit — Taxation Preferences (First-Choice Preference)"
)

for (bat_name in app_batteries) {

  bat       <- batteries[[bat_name]]
  dv_vec    <- bat$pref
  lbl_vec   <- unname(bat$labels[dv_vec])
  tbl_num   <- app_table_nums[[bat_name]]
  tbl_title <- app_table_titles[[bat_name]]
  tbl_label <- paste0("tab:nested_", bat_name)

  cat("\n── Table", tbl_num, ":", tbl_title, "\n")

  # Fit M1–M4 for each pref DV in this battery
  models_by_dv <- setNames(
    lapply(seq_along(dv_vec), function(i) {
      dv  <- dv_vec[i]
      lbl <- lbl_vec[i]
      if (!dv %in% names(df)) {
        message("  Column not found: ", dv)
        return(NULL)
      }
      cat("  DV:", lbl, "\n")
      ms <- fit_nested_logits(dv)
      names(ms) <- paste0("M", seq_along(ms))
      ms
    }),
    lbl_vec
  )

  # Build flat model list: keys = "DV label__M1" etc., cols labeled M1–M4
  flat_models <- list()
  flat_vcovs  <- list()
  col_names   <- c()

  for (lbl in lbl_vec) {
    m_list <- models_by_dv[[lbl]]
    if (is.null(m_list)) next
    for (mn in names(m_list)) {
      m <- m_list[[mn]]
      if (is.null(m)) next
      key              <- paste0(lbl, "__", mn)
      flat_models[[key]] <- m
      flat_vcovs[[key]]  <- tryCatch(
        sandwich::vcovHC(m, type = "HC1"),
        error = function(e) vcov(m)
      )
      col_names <- c(col_names, mn)   # M1, M2, M3, M4
    }
  }

  if (length(flat_models) == 0) {
    cat("  No models fitted — skipping table.\n")
    next
  }

  file_path <- file.path(
    pub_app,
    paste0("tableB", tbl_num, "_nested_", bat_name, "_pref.tex")
  )

  # Column spanning header: DV labels span their four model columns
  # modelsummary does not natively support column spanners in .tex output,
  # so we write a short LaTeX header note in the title and use add_header_above
  # via kableExtra after capturing the gt/latex output.
  # Strategy: output to a tibble first, then reformat with kableExtra.

  tryCatch({
    modelsummary(
      flat_models,
      estimate   = "{estimate}{stars}",
      statistic  = "({std.error})",
      vcov       = flat_vcovs,
      coef_map   = term_labels_nested,
      gof_map    = c("nobs", "logLik", "AIC"),
      output     = file_path,
      title      = tbl_title,
      notes      = nested_footnote
    )
    cat("  Saved:", file_path, "\n")
  }, error = function(e) {
    cat("  TABLE ERROR:", conditionMessage(e), "\n")
  })
}


# ==============================================================
# 9.  SUMMARY
# ==============================================================

cat("\n\n========== TRADEOFF PUBLICATION OUTPUT COMPLETE ==========\n")
cat("\nMain text (", pub_main, "):\n", sep = "")
cat("  Figure 6  — figure6_prio_pref.png\n")
cat("  Figure 7  — figure7_tax_pref.png\n")
cat("  Figure 8  — figure8_ge_cc1_pref.png\n")
cat("  Figure 9  — figure9_intense_nospend.png\n")
cat("\nAppendix (", pub_app, "):\n", sep = "")
cat("  Table B1  — tableB1_nested_cc1_pref.tex\n")
cat("  Table B2  — tableB2_nested_ge_pref.tex\n")
cat("  Table B3  — tableB3_nested_tax_pref.tex\n")
