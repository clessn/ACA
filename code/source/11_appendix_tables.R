# ==============================================================
# 11_appendix_tables.R
# Full regression tables for appendix — Word (.docx) output
#
# Run AFTER all model scripts (05 through 08) have been sourced,
# so that all model objects are available in the environment.
#
# Output: graphs/regressions/appendix_tables.docx
# Paste this file's tables into BPPsubmission.docx after pandoc.
# ==============================================================


# ── Helper: build AME table from a named list of logit models ──
#
# Returns a modelsummary-ready list of avg_slopes objects with
# clean coef_map, HC1 robust SEs, and a formatted notes string.

make_ame_table <- function(models, notes_str) {
  ame_list <- map(models, ~avg_slopes(.x, vcov = robust_vcov(.x)))
  list(models = ame_list, notes = notes_str)
}


# ── Shared formatting arguments ────────────────────────────────

shared_args <- list(
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  coef_map   = term_labels,
  gof_map    = list(
    list(raw = "nobs", clean = "N", fmt = 0)
  ),
  stars      = c("*" = 0.05, "**" = 0.01, "***" = 0.001)
)


# ── Output path ────────────────────────────────────────────────

out_path <- file.path(params$out_reg, "appendix_tables.docx")


# ── TABLE 1: Budget Priority First Choice (Figure 6) ──────────
#
# Models: prio_pref_models$logit  (from 06_regressions_prio.R)

t1 <- make_ame_table(
  prio_pref_models$logit,
  "Logit average marginal effects. HC1 robust SEs in parentheses. * p<0.05, ** p<0.01, *** p<0.001. DV: first choice in budget priority allocation (binary)."
)

tbl1 <- do.call(modelsummary, c(
  list(models    = t1$models,
       output    = "flextable",
       title     = "Table A1. Budget Priority First Choice — Logit AME (Figure 6)",
       notes     = t1$notes),
  shared_args
))


# ── TABLE 2: Taxation Tradeoff First Choice (Figure 7) ────────
#
# Models: tradeoff_models[["tax"]]$logit_pref  (from 07)

t2 <- make_ame_table(
  tradeoff_models[["tax"]]$logit_pref,
  "Logit average marginal effects. HC1 robust SEs in parentheses. * p<0.05, ** p<0.01, *** p<0.001. DV: first choice for each taxation tradeoff option (binary)."
)

tbl2 <- do.call(modelsummary, c(
  list(models = t2$models,
       output = "flextable",
       title  = "Table A2. Taxation Tradeoff First Choice — Logit AME (Figure 7)",
       notes  = t2$notes),
  shared_args
))


# ── TABLE 3: Green Economy Tradeoff First Choice (Figure 8) ───
#
# Models: tradeoff_models[["ge"]]$logit_pref  (from 07)

t3 <- make_ame_table(
  tradeoff_models[["ge"]]$logit_pref,
  "Logit average marginal effects. HC1 robust SEs in parentheses. * p<0.05, ** p<0.01, *** p<0.001. DV: first choice for each green economy tradeoff option (binary)."
)

tbl3 <- do.call(modelsummary, c(
  list(models = t3$models,
       output = "flextable",
       title  = "Table A3. Green Economy Tradeoff First Choice — Logit AME (Figure 8)",
       notes  = t3$notes),
  shared_args
))


# ── TABLE 4: Child Care Tradeoff First Choice (Figure 8) ──────
#
# Models: tradeoff_models[["cc1"]]$logit_pref  (from 07)

t4 <- make_ame_table(
  tradeoff_models[["cc1"]]$logit_pref,
  "Logit average marginal effects. HC1 robust SEs in parentheses. * p<0.05, ** p<0.01, *** p<0.001. DV: first choice for each child care tradeoff option (binary)."
)

tbl4 <- do.call(modelsummary, c(
  list(models = t4$models,
       output = "flextable",
       title  = "Table A4. Child Care Tradeoff First Choice — Logit AME (Figure 8)",
       notes  = t4$notes),
  shared_args
))


# ── TABLE 5: Intense No-Spend / No-Tax Preferences (Figure 9) ─
#
# Models: no_spend_models$logit  (from 08_regressions_intense.R)

t5 <- make_ame_table(
  no_spend_models$logit,
  "Logit average marginal effects. HC1 robust SEs in parentheses. * p<0.05, ** p<0.01, *** p<0.001. DV: intense preference (>50 pts) for no spending increase or no tax increase."
)

tbl5 <- do.call(modelsummary, c(
  list(models = t5$models,
       output = "flextable",
       title  = "Table A5. Intense Preferences Against Spending/Tax Increases — Logit AME (Figure 9)",
       notes  = t5$notes),
  shared_args
))


# ── WRITE TO WORD ──────────────────────────────────────────────
#
# Uses officer to assemble all tables into a single .docx with
# page breaks between tables for clean copy-paste into submission.

library(officer)
library(flextable)

doc <- read_docx() |>
  body_add_par("Appendix: Full Regression Tables", style = "heading 1") |>
  body_add_par(
    paste(
      "All models use logit with HC1 robust standard errors.",
      "Coefficients are average marginal effects (AME).",
      "All covariates are retained in each model.",
      "Only key hypothesis variables are shown in the main-text figures;",
      "complete covariate results are shown here."
    ),
    style = "Normal"
  ) |>
  body_add_par("", style = "Normal") |>

  # Table A1
  body_add_flextable(tbl1) |>
  body_add_break() |>

  # Table A2
  body_add_flextable(tbl2) |>
  body_add_break() |>

  # Table A3
  body_add_flextable(tbl3) |>
  body_add_break() |>

  # Table A4
  body_add_flextable(tbl4) |>
  body_add_break() |>

  # Table A5
  body_add_flextable(tbl5)

print(doc, target = out_path)
cat("Appendix tables saved to:", out_path, "\n")
