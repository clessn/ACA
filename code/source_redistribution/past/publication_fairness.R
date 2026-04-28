# ==============================================================
# 14_publication_fairness.R
# Publication-ready figures and tables — Fairness Beliefs paper
#
# Produces:
#
#   MAIN TEXT
#     Figure 1 — Descriptive: mean response by region (dot-and-CI)
#     Figure 2 — AME coefficient plot: proportionality DVs
#     Figure 3 — AME coefficient plot: reciprocity DVs
#     Table 1  — Descriptive means table (LaTeX, kableExtra)
#
#   APPENDIX
#     Figure A1 — Full coefficient plot: proportionality DVs (all covariates)
#     Figure A2 — Full coefficient plot: reciprocity DVs (all covariates)
#     Figure A3 — Robustness comparison (polr AME vs OLS): proportionality
#     Figure A4 — Robustness comparison (polr AME vs OLS): reciprocity
#     Table A1  — Ordered logit regression table: proportionality (LaTeX)
#     Table A2  — Ordered logit regression table: reciprocity (LaTeX)
#
# Pipeline assumption: analyses_fairness.R has been sourced in full,
# so all objects below are in scope:
#   df, params, polr_models, ols_models, coef_polr_top, coef_ols,
#   mean_ci, prop_labels, recip_labels, all_dv_labels, dv_order,
#   vars_main, term_labels, region_colours, theme_cpp(),
#   plot_coefs(), plot_robustness(), robust_vcov()
#
# If running standalone, source analyses_fairness.R first.
# ==============================================================


# ==============================================================
# 0.  OUTPUT PATHS
# ==============================================================

pub_out <- "/Users/shannondinan/Library/CloudStorage/Dropbox/_RESEARCH/_COLLABORATIONS/_GitHub/ACA/code/source_redistribution/graphs/publication_fairness"

pub_main <- file.path(pub_out, "main")
pub_app  <- file.path(pub_out, "appendix")

dir.create(pub_main, recursive = TRUE, showWarnings = FALSE)
dir.create(pub_app,  recursive = TRUE, showWarnings = FALSE)

cat("Output — main text: ", pub_main, "\n")
cat("Output — appendix:  ", pub_app,  "\n\n")


# ==============================================================
# 1.  SHARED SETTINGS
# ==============================================================

# Publication dimensions (inches)
# Main figures: wider, moderate height — fits one-column or two-column layout
fig_width_main  <- 14
fig_height_main <- 10
fig_height_recip <- 7    # reciprocity has only 3 DVs — shorter
fig_dpi <- 300

# Caption strings — kept consistent with journal style
cap_ame  <- "AME on P(response = 1). HC1 robust SEs, 95\\% CI. Ontario = reference region."
cap_app  <- "Appendix. AME on P(response = 1). HC1 robust SEs, 95\\% CI. Ontario = reference region."
cap_rob  <- "HC1 robust SEs, 95\\% CI. Ordered logit AME on P(response = 1) vs. OLS on 0/0.33/0.66/1 scale. Ontario = reference region."

# Require kableExtra for LaTeX tables
if (!requireNamespace("kableExtra", quietly = TRUE)) {
  stop("kableExtra is required. Install with: install.packages('kableExtra')")
}
library(kableExtra)


# ==============================================================
# 2.  FIGURE 1 — DESCRIPTIVE: MEAN RESPONSE BY REGION
#     (Main text)
#     Dot-and-CI plot. One panel per DV. Proportionality and
#     reciprocity separated by a strip label, not a separate figure.
#     X-axis: mean on 0–1 scale. Y-axis: region.
#     Error bars: t-based 95% CI.
# ==============================================================

cat("── Figure 1: Descriptive mean response by region\n")

mean_ci |>
  mutate(dv = factor(dv, levels = dv_order)) |>
  ggplot(aes(x = mean, y = region, colour = region)) +
  geom_point(size = 3.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.3, linewidth = 0.5) +
  scale_colour_manual(
    values = c(region_colours, "All Canada" = "#636363"),
    guide  = "none"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.33, 0.66, 1)) +
  facet_wrap(~ dv, ncol = 2) +
  labs(
    x       = "Mean response (0\\u20131 scale) \u2014 t-based 95% CI",
    y       = NULL,
    caption = "Mean response on 0/0.33/0.66/1 scale. Error bars = t-based 95% CI. DV scale: 0 = Unfair, 1 = Fair."
  ) +
  theme_cpp() +
  theme(strip.text = element_text(face = "bold"))

ggsave(
  file.path(pub_main, "figure1_desc_mean_by_region.png"),
  width  = fig_width_main,
  height = fig_height_main + 2,
  dpi    = fig_dpi
)

cat("   Saved: figure1_desc_mean_by_region.png\n\n")


# ==============================================================
# 3.  TABLE 1 — DESCRIPTIVE MEANS (LaTeX)
#     (Main text)
#     Mean [lo, hi] by region and All Canada.
#     Proportionality and reciprocity separated by pack_rows().
# ==============================================================

cat("── Table 1: Descriptive means (LaTeX)\n")

desc_table_pub <- mean_ci |>
  mutate(cell = sprintf("%.2f [%.2f, %.2f]", mean, lo, hi)) |>
  dplyr::select(region, dv, dv_type, cell) |>
  pivot_wider(names_from = region, values_from = cell) |>
  dplyr::select(
    dv, dv_type,
    any_of(c("Ontario", "Quebec", "Alberta", "Atlantic Canada", "All Canada"))
  ) |>
  mutate(dv = factor(dv, levels = c(unname(prop_labels), unname(recip_labels)))) |>
  arrange(dv) |>
  mutate(dv = as.character(dv))

n_prop_pub  <- length(prop_vars)
n_recip_pub <- length(recip_vars)

desc_table_pub |>
  dplyr::select(-dv_type) |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    col.names = c("", "ON", "QC", "AB", "AC", "All Canada"),
    caption   = "Mean Fairness Attitudes by Region",
    label     = "tab:desc_fairness"
  ) |>
  kable_styling(
    latex_options = c("hold_position", "scale_down")
  ) |>
  pack_rows("Proportionality", 1, n_prop_pub) |>
  pack_rows("Reciprocity",     n_prop_pub + 1, n_prop_pub + n_recip_pub) |>
  footnote(
    general = paste(
      "Mean response on 0/0.33/0.66/1 scale with t-based 95\\\\% CI in brackets.",
      "DV scale: 0 = Unfair, 1 = Fair. Ontario is the reference region.",
      "ON = Ontario, QC = Quebec, AB = Alberta, AC = Atlantic Canada.",
      "Source: Original dataset."
    ),
    general_title     = "\\\\textit{Note}: ",
    footnote_as_chunk = TRUE,
    escape            = FALSE
  ) |>
  save_kable(file.path(pub_main, "table1_desc_means.tex"))

cat("   Saved: table1_desc_means.tex\n\n")


# ==============================================================
# 4.  FIGURE 2 — AME COEFFICIENT PLOT: PROPORTIONALITY DVs
#     (Main text)
#     Key predictors only (vars_main). Colour = direction.
# ==============================================================

cat("── Figure 2: AME coefficient plot — proportionality (main)\n")

plot_coefs(
  coef_df     = coef_polr_top |> dplyr::filter(dv %in% unname(prop_labels)),
  keep_vars   = vars_main,
  caption_str = cap_ame,
  file_path   = file.path(pub_main, "figure2_coef_prop_main.png"),
  ncol_facet  = 2,
  width       = fig_width_main,
  height      = fig_height_main
)

cat("   Saved: figure2_coef_prop_main.png\n\n")


# ==============================================================
# 5.  FIGURE 3 — AME COEFFICIENT PLOT: RECIPROCITY DVs
#     (Main text)
#     Key predictors only (vars_main). Colour = direction.
# ==============================================================

cat("── Figure 3: AME coefficient plot — reciprocity (main)\n")

plot_coefs(
  coef_df     = coef_polr_top |> dplyr::filter(dv %in% unname(recip_labels)),
  keep_vars   = vars_main,
  caption_str = cap_ame,
  file_path   = file.path(pub_main, "figure3_coef_recip_main.png"),
  ncol_facet  = 2,
  width       = fig_width_main,
  height      = fig_height_recip
)

cat("   Saved: figure3_coef_recip_main.png\n\n")


# ==============================================================
# 6.  FIGURE A1 — FULL COEFFICIENT PLOT: PROPORTIONALITY DVs
#     (Appendix)
#     All covariates including ses_citizenYes_bin.
# ==============================================================

cat("── Figure A1: Full coefficient plot — proportionality (appendix)\n")

plot_coefs(
  coef_df     = coef_polr_top |> dplyr::filter(dv %in% unname(prop_labels)),
  keep_vars   = NULL,
  caption_str = cap_app,
  file_path   = file.path(pub_app, "figureA1_coef_prop_appendix.png"),
  ncol_facet  = 2,
  width       = fig_width_main,
  height      = fig_height_main + 3
)

cat("   Saved: figureA1_coef_prop_appendix.png\n\n")


# ==============================================================
# 7.  FIGURE A2 — FULL COEFFICIENT PLOT: RECIPROCITY DVs
#     (Appendix)
#     All covariates including ses_citizenYes_bin.
# ==============================================================

cat("── Figure A2: Full coefficient plot — reciprocity (appendix)\n")

plot_coefs(
  coef_df     = coef_polr_top |> dplyr::filter(dv %in% unname(recip_labels)),
  keep_vars   = NULL,
  caption_str = cap_app,
  file_path   = file.path(pub_app, "figureA2_coef_recip_appendix.png"),
  ncol_facet  = 2,
  width       = fig_width_main,
  height      = fig_height_recip + 2
)

cat("   Saved: figureA2_coef_recip_appendix.png\n\n")


# ==============================================================
# 8.  FIGURE A3 — ROBUSTNESS: PROPORTIONALITY DVs
#     (Appendix)
#     Overlays polr AMEs and OLS coefficients, key predictors only.
# ==============================================================

cat("── Figure A3: Robustness comparison — proportionality (appendix)\n")

plot_robustness(
  coef_polr   = coef_polr_top |> dplyr::filter(dv %in% unname(prop_labels)),
  coef_ols    = coef_ols      |> dplyr::filter(dv %in% unname(prop_labels)),
  keep_vars   = vars_main,
  caption_str = cap_rob,
  file_path   = file.path(pub_app, "figureA3_robustness_prop.png"),
  ncol_facet  = 2,
  height      = fig_height_main
)

cat("   Saved: figureA3_robustness_prop.png\n\n")


# ==============================================================
# 9.  FIGURE A4 — ROBUSTNESS: RECIPROCITY DVs
#     (Appendix)
#     Overlays polr AMEs and OLS coefficients, key predictors only.
# ==============================================================

cat("── Figure A4: Robustness comparison — reciprocity (appendix)\n")

plot_robustness(
  coef_polr   = coef_polr_top |> dplyr::filter(dv %in% unname(recip_labels)),
  coef_ols    = coef_ols      |> dplyr::filter(dv %in% unname(recip_labels)),
  keep_vars   = vars_main,
  caption_str = cap_rob,
  file_path   = file.path(pub_app, "figureA4_robustness_recip.png"),
  ncol_facet  = 2,
  height      = fig_height_recip
)

cat("   Saved: figureA4_robustness_recip.png\n\n")


# ==============================================================
# 10.  TABLE A1 — ORDERED LOGIT: PROPORTIONALITY DVs (LaTeX)
#      (Appendix)
#      Log-odds coefficients, HC1 robust SEs, stars.
#      One column per DV; all covariates shown.
# ==============================================================

cat("── Table A1: Ordered logit — proportionality (LaTeX)\n")

# Build paired vcov list for modelsummary
polr_prop_models <- polr_models[unname(prop_labels)]

vcov_prop <- vector("list", length(polr_prop_models))
names(vcov_prop) <- names(polr_prop_models)
for (nm in names(polr_prop_models)) {
  vcov_prop[[nm]] <- tryCatch(
    sandwich::vcovHC(polr_prop_models[[nm]], type = "HC1"),
    error = function(e) vcov(polr_prop_models[[nm]])
  )
}

modelsummary(
  polr_prop_models,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov      = vcov_prop,
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = file.path(pub_app, "tableA1_polr_prop.tex"),
  title     = "Ordered Logit — Proportionality Beliefs",
  notes     = paste(
    "Ordered logit (polr). Log-odds coefficients.",
    "HC1 robust SEs in parentheses.",
    "DV coded 0/0.33/0.66/1; highest level (= 1) = ``fair''.",
    "Ontario = reference region.",
    "* $p<0.05$, ** $p<0.01$, *** $p<0.001$"
  )
)

cat("   Saved: tableA1_polr_prop.tex\n\n")


# ==============================================================
# 11.  TABLE A2 — ORDERED LOGIT: RECIPROCITY DVs (LaTeX)
#      (Appendix)
#      Log-odds coefficients, HC1 robust SEs, stars.
# ==============================================================

cat("── Table A2: Ordered logit — reciprocity (LaTeX)\n")

polr_recip_models <- polr_models[unname(recip_labels)]

vcov_recip <- vector("list", length(polr_recip_models))
names(vcov_recip) <- names(polr_recip_models)
for (nm in names(polr_recip_models)) {
  vcov_recip[[nm]] <- tryCatch(
    sandwich::vcovHC(polr_recip_models[[nm]], type = "HC1"),
    error = function(e) vcov(polr_recip_models[[nm]])
  )
}

modelsummary(
  polr_recip_models,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov      = vcov_recip,
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = file.path(pub_app, "tableA2_polr_recip.tex"),
  title     = "Ordered Logit — Reciprocity Beliefs",
  notes     = paste(
    "Ordered logit (polr). Log-odds coefficients.",
    "HC1 robust SEs in parentheses.",
    "DV coded 0/0.33/0.66/1; highest level (= 1) = ``fair''.",
    "Ontario = reference region.",
    "* $p<0.05$, ** $p<0.01$, *** $p<0.001$"
  )
)

cat("   Saved: tableA2_polr_recip.tex\n\n")


# ==============================================================
# 12.  SUMMARY
# ==============================================================

cat("\n========== PUBLICATION OUTPUT COMPLETE ==========\n")
cat("\nMain text:\n")
cat("  Figure 1 — figure1_desc_mean_by_region.png\n")
cat("  Figure 2 — figure2_coef_prop_main.png\n")
cat("  Figure 3 — figure3_coef_recip_main.png\n")
cat("  Table  1 — table1_desc_means.tex\n")
cat("\nAppendix:\n")
cat("  Figure A1 — figureA1_coef_prop_appendix.png\n")
cat("  Figure A2 — figureA2_coef_recip_appendix.png\n")
cat("  Figure A3 — figureA3_robustness_prop.png\n")
cat("  Figure A4 — figureA4_robustness_recip.png\n")
cat("  Table  A1 — tableA1_polr_prop.tex\n")
cat("  Table  A2 — tableA2_polr_recip.tex\n")
cat("\nAll files written to:", pub_out, "\n")
