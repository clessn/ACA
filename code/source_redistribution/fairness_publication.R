# ==============================================================
# fairness_publication.R
# Publication figures and tables — Fairness Beliefs paper
#
# Run independently after fairness_models.R and
# fairness_descriptives.R have been run at least once.
# Loads saved objects from paths$rds — no model refitting.
#
# All figures and tables use M4 from the nested models,
# ensuring a consistent common sample across all outputs.
#
# MAIN TEXT
#   Figure 1 — Mean response by region (dot-and-CI)
#   Figure 2 — AME coefficient plot: proportionality DVs (M4)
#   Figure 3 — AME coefficient plot: reciprocity DVs (M4)
#   Table 1  — Descriptive means (LaTeX)
#
# APPENDIX
#   Figure A1 — Robustness (polr AME vs OLS, M4 sample): proportionality
#   Figure A2 — Robustness (polr AME vs OLS, M4 sample): reciprocity
#   Table A1  — Ordered logit M4 table: proportionality (LaTeX)
#   Table A2  — Ordered logit M4 table: reciprocity (LaTeX)
# ==============================================================

source("code/source_redistribution/fairness_config.R")
library(kableExtra)

# ── Load saved objects ────────────────────────────────────────
cat("Loading model objects from RDS...\n")
coef_M4_top    <- readRDS(file.path(paths$rds, "coef_M4_top.rds"))
coef_M4_models <- readRDS(file.path(paths$rds, "coef_M4_models.rds"))
coef_ols_M4    <- readRDS(file.path(paths$rds, "coef_ols_M4.rds"))
mean_ci        <- readRDS(file.path(paths$rds, "mean_ci.rds"))
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
# FIGURE 2 — AME: PROPORTIONALITY DVs (main, M4)
# ==============================================================

cat("── Figure 2\n")
plot_coefs(
  coef_df     = coef_M4_top |> dplyr::filter(dv %in% unname(prop_labels)),
  keep_vars   = vars_main,
  caption_str = "AME on P(response = 1), M4. HC1 robust SEs, 95% CI. Ontario = reference region.",
  file_path   = file.path(paths$pub_main, "figure2_coef_prop_main.png"),
  ncol_facet  = 2,
  width       = plot_width,
  height      = plot_height
)
cat("   Saved: figure2_coef_prop_main.png\n\n")


# ==============================================================
# FIGURE 3 — AME: RECIPROCITY DVs (main, M4)
# ==============================================================

cat("── Figure 3\n")
plot_coefs(
  coef_df     = coef_M4_top |> dplyr::filter(dv %in% unname(recip_labels)),
  keep_vars   = vars_main,
  caption_str = "AME on P(response = 1), M4. HC1 robust SEs, 95% CI. Ontario = reference region.",
  file_path   = file.path(paths$pub_main, "figure3_coef_recip_main.png"),
  ncol_facet  = 2,
  width       = plot_width,
  height      = plot_height - 3
)
cat("   Saved: figure3_coef_recip_main.png\n\n")


# ==============================================================
# FIGURE A1 — ROBUSTNESS: PROPORTIONALITY (appendix, M4 sample)
# ==============================================================

cat("── Figure A1\n")
plot_robustness(
  coef_polr   = coef_M4_top  |> dplyr::filter(dv %in% unname(prop_labels)),
  coef_ols    = coef_ols_M4  |> dplyr::filter(dv %in% unname(prop_labels)),
  keep_vars   = vars_main,
  caption_str = "Ordered logit AME on P(response = 1) vs. OLS. M4 common sample. HC1 robust SEs, 95% CI. Ontario = reference region.",
  file_path   = file.path(paths$pub_app, "figureA1_robustness_prop.png"),
  ncol_facet  = 2,
  width       = plot_width,
  height      = plot_height
)
cat("   Saved: figureA1_robustness_prop.png\n\n")


# ==============================================================
# FIGURE A2 — ROBUSTNESS: RECIPROCITY (appendix, M4 sample)
# ==============================================================

cat("── Figure A2\n")
plot_robustness(
  coef_polr   = coef_M4_top  |> dplyr::filter(dv %in% unname(recip_labels)),
  coef_ols    = coef_ols_M4  |> dplyr::filter(dv %in% unname(recip_labels)),
  keep_vars   = vars_main,
  caption_str = "Ordered logit AME on P(response = 1) vs. OLS. M4 common sample. HC1 robust SEs, 95% CI. Ontario = reference region.",
  file_path   = file.path(paths$pub_app, "figureA2_robustness_recip.png"),
  ncol_facet  = 2,
  width       = plot_width,
  height      = plot_height - 3
)
cat("   Saved: figureA2_robustness_recip.png\n\n")


# ==============================================================
# APPENDIX TABLES — NESTED MODELS (M1–M5), ONE PER DV
#
# Tables are written by fairness_models.R to paths$nested as
# .tex files. This section copies them to paths$pub_app so all
# publication outputs are in one place, and prints the LaTeX
# \input{} commands to use in the appendix.
#
# File naming convention (matches fairness_models.R loop):
#   nested_polr_proportionality_<slug>.tex
#   nested_polr_reciprocity_<slug>.tex
# ==============================================================

cat("── Appendix nested tables\n")

nested_table_files <- map_chr(
  seq_along(all_dv_vars),
  function(i) {
    dv_raw  <- all_dv_vars[i]
    dv_lbl  <- all_dv_labels[[dv_raw]]
    dv_type <- if (dv_raw %in% prop_vars) "proportionality" else "reciprocity"
    slug    <- safe_filename(dv_lbl)
    paste0("nested_polr_", dv_type, "_", slug, ".tex")
  }
)

# Copy from paths$nested to paths$pub_app
walk(nested_table_files, function(fname) {
  src  <- file.path(paths$nested, fname)
  dest <- file.path(paths$pub_app, fname)
  if (file.exists(src)) {
    file.copy(src, dest, overwrite = TRUE)
    cat("  Copied:", fname, "\n")
  } else {
    message("  NOT FOUND (run fairness_models.R first): ", fname)
  }
})

# Print LaTeX \input{} commands for easy copy-paste into appendix
cat("\n── LaTeX \\input{} commands for appendix:\n")
walk2(nested_table_files, all_dv_labels[all_dv_vars], function(fname, lbl) {
  cat(sprintf("\\subsection*{%s}\n\\input{appendix/%s}\n\n", lbl, fname))
})


# ==============================================================
# SUMMARY
# ==============================================================

cat("\n========== PUBLICATION OUTPUT COMPLETE ==========\n\n")
cat("Main (", paths$pub_main, "):\n", sep = "")
cat("  Figure 1 — figure1_desc_mean_by_region.png\n")
cat("  Figure 2 — figure2_coef_prop_main.png  (M4 AMEs)\n")
cat("  Figure 3 — figure3_coef_recip_main.png (M4 AMEs)\n")
cat("  Table  1 — table1_desc_means.tex\n\n")
cat("Appendix (", paths$pub_app, "):\n", sep = "")
cat("  Figure A1 — figureA1_robustness_prop.png  (M4 polr vs OLS)\n")
cat("  Figure A2 — figureA2_robustness_recip.png (M4 polr vs OLS)\n")
cat("  Tables    — one nested M1-M5 .tex file per DV (8 total)\n")
walk(nested_table_files, function(f) cat("    ", f, "\n"))
