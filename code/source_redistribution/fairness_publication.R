# ==============================================================
# fairness_publication.R
# Publication figures and tables — Fairness Beliefs paper
#
# Run independently after fairness_models.R and
# fairness_descriptives.R have been run at least once.
# Loads saved objects from paths$rds — no model refitting.
#
# MAIN TEXT
#   Figure 1 — Mean response by region (dot-and-CI)
#   Figure 2 — AME coefficient plot: proportionality DVs
#   Figure 3 — AME coefficient plot: reciprocity DVs
#   Table 1  — Descriptive means (LaTeX)
#
# APPENDIX
#   Figure A1 — Full coefficient plot: proportionality (all covariates)
#   Figure A2 — Full coefficient plot: reciprocity (all covariates)
#   Figure A3 — Robustness (polr AME vs OLS): proportionality
#   Figure A4 — Robustness (polr AME vs OLS): reciprocity
#   Table A1  — Ordered logit table: proportionality (LaTeX)
#   Table A2  — Ordered logit table: reciprocity (LaTeX)
# ==============================================================

source("code/source_redistribution/fairness_config.R")
library(kableExtra)

# ── Load saved objects ────────────────────────────────────────
cat("Loading model objects from RDS...\n")
polr_models   <- readRDS(file.path(paths$rds, "polr_models.rds"))
coef_polr_top <- readRDS(file.path(paths$rds, "coef_polr_top.rds"))
coef_ols      <- readRDS(file.path(paths$rds, "coef_ols.rds"))
mean_ci       <- readRDS(file.path(paths$rds, "mean_ci.rds"))
cat("Done.\n\n")

# ── Shared caption strings ────────────────────────────────────
cap_main <- "AME on P(response = 1). HC1 robust SEs, 95% CI. Ontario = reference region."
cap_app  <- "Appendix. AME on P(response = 1). HC1 robust SEs, 95% CI. Ontario = reference region."
cap_rob  <- "Ordered logit AME on P(response = 1) vs. OLS on 0/0.33/0.66/1 scale. HC1 robust SEs, 95% CI. Ontario = reference region."


# ==============================================================
# FIGURE 1 — DESCRIPTIVE: MEAN RESPONSE BY REGION
# ==============================================================

cat("── Figure 1\n")

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
  labs(x       = "Mean response (0\u20131 scale) \u2014 t-based 95% CI",
       y       = NULL,
       caption = "Mean response on 0/0.33/0.66/1 scale. Error bars = t-based 95% CI. DV scale: 0 = Unfair, 1 = Fair.") +
  theme_cpp() +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(paths$pub_main, "figure1_desc_mean_by_region.png"),
       width = plot_width, height = plot_height + 2, dpi = plot_dpi)
cat("   Saved: figure1_desc_mean_by_region.png\n\n")


# ==============================================================
# TABLE 1 — DESCRIPTIVE MEANS (LaTeX)
# ==============================================================

cat("── Table 1\n")

mean_ci |>
  mutate(cell = sprintf("%.2f [%.2f, %.2f]", mean, lo, hi)) |>
  dplyr::select(region, dv, dv_type, cell) |>
  pivot_wider(names_from = region, values_from = cell) |>
  dplyr::select(dv, dv_type,
                any_of(c("Ontario", "Quebec", "Alberta",
                         "Atlantic Canada", "All Canada"))) |>
  mutate(dv = factor(dv, levels = c(unname(prop_labels),
                                    unname(recip_labels)))) |>
  arrange(dv) |>
  mutate(dv = as.character(dv)) |>
  dplyr::select(-dv_type) |>
  kbl(format    = "latex", booktabs = TRUE,
      col.names = c("", "ON", "QC", "AB", "AC", "All Canada"),
      caption   = "Mean Fairness Attitudes by Region",
      label     = "tab:desc_fairness") |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  pack_rows("Proportionality", 1, length(prop_vars)) |>
  pack_rows("Reciprocity", length(prop_vars) + 1,
            length(prop_vars) + length(recip_vars)) |>
  add_footnote(
    label  = c(
      "Mean response on 0/0.33/0.66/1 scale with t-based 95\\% CI in brackets.",
      "DV scale: 0 = Unfair, 1 = Fair. Ontario is the reference region.",
      "ON = Ontario, QC = Quebec, AB = Alberta, AC = Atlantic Canada.",
      "Source: Original dataset."
    ),
    notation = "none"
  ) |>
  save_kable(file.path(paths$pub_main, "table1_desc_means.tex"))

cat("   Saved: table1_desc_means.tex\n\n")


# ==============================================================
# FIGURE 2 — AME: PROPORTIONALITY DVs (main)
# ==============================================================

cat("── Figure 2\n")
plot_coefs(
  coef_df     = coef_polr_top |> dplyr::filter(dv %in% unname(prop_labels)),
  keep_vars   = vars_main,
  caption_str = cap_main,
  file_path   = file.path(paths$pub_main, "figure2_coef_prop_main.png"),
  ncol_facet  = 2,
  width       = plot_width,
  height      = plot_height
)
cat("   Saved: figure2_coef_prop_main.png\n\n")


# ==============================================================
# FIGURE 3 — AME: RECIPROCITY DVs (main)
# ==============================================================

cat("── Figure 3\n")
plot_coefs(
  coef_df     = coef_polr_top |> dplyr::filter(dv %in% unname(recip_labels)),
  keep_vars   = vars_main,
  caption_str = cap_main,
  file_path   = file.path(paths$pub_main, "figure3_coef_recip_main.png"),
  ncol_facet  = 2,
  width       = plot_width,
  height      = plot_height - 3
)
cat("   Saved: figure3_coef_recip_main.png\n\n")


# ==============================================================
# FIGURE A1 — FULL COEFFICIENT PLOT: PROPORTIONALITY (appendix)
# ==============================================================

cat("── Figure A1\n")
plot_coefs(
  coef_df     = coef_polr_top |> dplyr::filter(dv %in% unname(prop_labels)),
  keep_vars   = NULL,
  caption_str = cap_app,
  file_path   = file.path(paths$pub_app, "figureA1_coef_prop_appendix.png"),
  ncol_facet  = 2,
  width       = plot_width,
  height      = plot_height + 3
)
cat("   Saved: figureA1_coef_prop_appendix.png\n\n")


# ==============================================================
# FIGURE A2 — FULL COEFFICIENT PLOT: RECIPROCITY (appendix)
# ==============================================================

cat("── Figure A2\n")
plot_coefs(
  coef_df     = coef_polr_top |> dplyr::filter(dv %in% unname(recip_labels)),
  keep_vars   = NULL,
  caption_str = cap_app,
  file_path   = file.path(paths$pub_app, "figureA2_coef_recip_appendix.png"),
  ncol_facet  = 2,
  width       = plot_width,
  height      = plot_height
)
cat("   Saved: figureA2_coef_recip_appendix.png\n\n")


# ==============================================================
# FIGURE A3 — ROBUSTNESS: PROPORTIONALITY (appendix)
# ==============================================================

cat("── Figure A3\n")
plot_robustness(
  coef_polr   = coef_polr_top |> dplyr::filter(dv %in% unname(prop_labels)),
  coef_ols    = coef_ols      |> dplyr::filter(dv %in% unname(prop_labels)),
  keep_vars   = vars_main,
  caption_str = cap_rob,
  file_path   = file.path(paths$pub_app, "figureA3_robustness_prop.png"),
  ncol_facet  = 2,
  width       = plot_width,
  height      = plot_height
)
cat("   Saved: figureA3_robustness_prop.png\n\n")


# ==============================================================
# FIGURE A4 — ROBUSTNESS: RECIPROCITY (appendix)
# ==============================================================

cat("── Figure A4\n")
plot_robustness(
  coef_polr   = coef_polr_top |> dplyr::filter(dv %in% unname(recip_labels)),
  coef_ols    = coef_ols      |> dplyr::filter(dv %in% unname(recip_labels)),
  keep_vars   = vars_main,
  caption_str = cap_rob,
  file_path   = file.path(paths$pub_app, "figureA4_robustness_recip.png"),
  ncol_facet  = 2,
  width       = plot_width,
  height      = plot_height - 3
)
cat("   Saved: figureA4_robustness_recip.png\n\n")


# ==============================================================
# TABLE A1 — ORDERED LOGIT: PROPORTIONALITY (appendix, LaTeX)
# ==============================================================

cat("── Table A1\n")

polr_prop <- polr_models[unname(prop_labels)]
paired_prop <- make_vcov_list(polr_prop)

modelsummary(
  paired_prop$models,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov      = paired_prop$vcovs,
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = file.path(paths$pub_app, "tableA1_polr_prop.tex"),
  title     = "Ordered Logit --- Proportionality Beliefs",
  notes     = paste(
    "Ordered logit (polr). Log-odds coefficients. HC1 robust SEs in parentheses.",
    "DV coded 0/0.33/0.66/1; highest level (= 1) = ``fair''. Ontario = reference region.",
    "* $p<0.05$, ** $p<0.01$, *** $p<0.001$"
  )
)
cat("   Saved: tableA1_polr_prop.tex\n\n")


# ==============================================================
# TABLE A2 — ORDERED LOGIT: RECIPROCITY (appendix, LaTeX)
# ==============================================================

cat("── Table A2\n")

polr_recip <- polr_models[unname(recip_labels)]
paired_recip <- make_vcov_list(polr_recip)

modelsummary(
  paired_recip$models,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  vcov      = paired_recip$vcovs,
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = file.path(paths$pub_app, "tableA2_polr_recip.tex"),
  title     = "Ordered Logit --- Reciprocity Beliefs",
  notes     = paste(
    "Ordered logit (polr). Log-odds coefficients. HC1 robust SEs in parentheses.",
    "DV coded 0/0.33/0.66/1; highest level (= 1) = ``fair''. Ontario = reference region.",
    "* $p<0.05$, ** $p<0.01$, *** $p<0.001$"
  )
)
cat("   Saved: tableA2_polr_recip.tex\n\n")


# ==============================================================
# SUMMARY
# ==============================================================

cat("========== PUBLICATION OUTPUT COMPLETE ==========\n\n")
cat("Main (", paths$pub_main, "):\n", sep = "")
cat("  Figure 1 — figure1_desc_mean_by_region.png\n")
cat("  Figure 2 — figure2_coef_prop_main.png\n")
cat("  Figure 3 — figure3_coef_recip_main.png\n")
cat("  Table  1 — table1_desc_means.tex\n\n")
cat("Appendix (", paths$pub_app, "):\n", sep = "")
cat("  Figure A1 — figureA1_coef_prop_appendix.png\n")
cat("  Figure A2 — figureA2_coef_recip_appendix.png\n")
cat("  Figure A3 — figureA3_robustness_prop.png\n")
cat("  Figure A4 — figureA4_robustness_recip.png\n")
cat("  Table  A1 — tableA1_polr_prop.tex\n")
cat("  Table  A2 — tableA2_polr_recip.tex\n")
