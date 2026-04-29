# ==============================================================
# fairness_descriptives.R
# Descriptive statistics — Fairness Beliefs paper
#
# Run independently. Sources fairness_config.R for all metadata.
#
# Outputs (all to paths$desc):
#   desc_prop_dist_by_region.png     — response distribution, proportionality
#   desc_recip_dist_by_region.png    — response distribution, reciprocity
#   desc_mean_dotCI_by_region.png    — mean response dot-and-CI (main figure)
#   desc_median_dotCI_by_region.png  — median response dot-and-CI (robustness)
#   table_desc_fairness.tex          — LaTeX descriptive means table
# ==============================================================

source("code/source_redistribution/fairness_config.R")
library(kableExtra)


# ==============================================================
# 1.  RESPONSE DISTRIBUTION BY REGION
# ==============================================================

desc_by_region <- map_dfr(all_dv_vars, function(v) {
  df |>
    dplyr::filter(!is.na(.data[[v]]), !is.na(ses_region_cat)) |>
    group_by(ses_region_cat, value = .data[[v]]) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(ses_region_cat) |>
    mutate(pct = 100 * n / sum(n)) |>
    ungroup() |>
    mutate(
      dv      = all_dv_labels[[v]],
      dv_type = ifelse(v %in% prop_vars, "Proportionality", "Reciprocity"),
      value   = factor(as.character(value),
                       levels = c("0", "0.33", "0.66", "1"),
                       labels = c("0 (Unfair)", "0.33", "0.66", "1 (Fair)"))
    ) |>
    rename(region = ses_region_cat)
})

# ── Proportionality ───────────────────────────────────────────
desc_by_region |>
  dplyr::filter(dv_type == "Proportionality") |>
  mutate(dv = factor(dv, levels = unname(prop_labels))) |>
  ggplot(aes(x = value, y = pct, fill = region)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.85),
            vjust = -0.35, size = 2.5) +
  scale_fill_manual(values = region_colours) +
  scale_y_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.14))) +
  facet_wrap(~ dv, ncol = 2) +
  labs(x = "Response level  (0 = Unfair,  1 = Fair)",
       y = "% of respondents (within region)",
       fill    = "Region",
       title   = "Proportionality attitudes — response distribution by region",
       caption = "DVs coded 0 / 0.33 / 0.66 / 1.") +
  theme_cpp() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave(file.path(paths$desc, "desc_prop_dist_by_region.png"),
       width = plot_width, height = plot_height, dpi = plot_dpi)
cat("Saved: desc_prop_dist_by_region.png\n")

# ── Reciprocity ───────────────────────────────────────────────
desc_by_region |>
  dplyr::filter(dv_type == "Reciprocity") |>
  mutate(dv = factor(dv, levels = unname(recip_labels))) |>
  ggplot(aes(x = value, y = pct, fill = region)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.85),
            vjust = -0.35, size = 2.5) +
  scale_fill_manual(values = region_colours) +
  scale_y_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.14))) +
  facet_wrap(~ dv, ncol = 2) +
  labs(x = "Response level  (0 = Unfair,  1 = Fair)",
       y = "% of respondents (within region)",
       fill    = "Region",
       title   = "Reciprocity attitudes — response distribution by region",
       caption = "DVs coded 0 / 0.33 / 0.66 / 1.") +
  theme_cpp() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave(file.path(paths$desc, "desc_recip_dist_by_region.png"),
       width = plot_width, height = plot_height / 1.5, dpi = plot_dpi)
cat("Saved: desc_recip_dist_by_region.png\n")


# ==============================================================
# 2.  MEAN RESPONSE — DOT-AND-CI BY REGION
# ==============================================================

mean_ci <- map_dfr(all_dv_vars, function(v) {
  by_region <- df |>
    dplyr::filter(!is.na(.data[[v]]), !is.na(ses_region_cat)) |>
    group_by(ses_region_cat) |>
    summarise(n    = n(),
              mean = mean(.data[[v]], na.rm = TRUE),
              se   = sd(.data[[v]],   na.rm = TRUE) / sqrt(n()),
              .groups = "drop") |>
    rename(region = ses_region_cat)
  
  all_canada <- df |>
    dplyr::filter(!is.na(.data[[v]])) |>
    summarise(n    = n(),
              mean = mean(.data[[v]], na.rm = TRUE),
              se   = sd(.data[[v]],   na.rm = TRUE) / sqrt(n())) |>
    mutate(region = "All Canada")
  
  bind_rows(by_region, all_canada) |>
    mutate(
      dv      = all_dv_labels[[v]],
      dv_type = ifelse(v %in% prop_vars, "Proportionality", "Reciprocity"),
      lo      = mean - qt(0.975, df = n - 1) * se,
      hi      = mean + qt(0.975, df = n - 1) * se,
      region  = factor(region,
                       levels = c("Ontario", "Quebec", "Alberta",
                                  "Atlantic Canada", "All Canada"))
    ) |>
    dplyr::select(region, dv, dv_type, n, mean, se, lo, hi)
})

# Save for use in publication script
saveRDS(mean_ci, file.path(paths$rds, "mean_ci.rds"))

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

ggsave(file.path(paths$desc, "desc_mean_dotCI_by_region.png"),
       width = plot_width, height = plot_height + 2, dpi = plot_dpi)
cat("Saved: desc_mean_dotCI_by_region.png\n")


# ==============================================================
# 3.  MEDIAN RESPONSE — DOT-AND-CI BY REGION (robustness check)
#     Uses MAD-based approximate SE given skewed DV distributions.
# ==============================================================

median_ci_mad <- map_dfr(all_dv_vars, function(v) {
  by_region <- df |>
    dplyr::filter(!is.na(.data[[v]]), !is.na(ses_region_cat)) |>
    group_by(ses_region_cat) |>
    summarise(
      n      = n(),
      median = median(.data[[v]], na.rm = TRUE),
      mad    = mad(.data[[v]],    na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(region = ses_region_cat)
  
  all_canada <- df |>
    dplyr::filter(!is.na(.data[[v]])) |>
    summarise(
      n      = n(),
      median = median(.data[[v]], na.rm = TRUE),
      mad    = mad(.data[[v]],    na.rm = TRUE)
    ) |>
    mutate(region = "All Canada")
  
  bind_rows(by_region, all_canada) |>
    mutate(
      dv        = all_dv_labels[[v]],
      dv_type   = ifelse(v %in% prop_vars, "Proportionality", "Reciprocity"),
      se_approx = mad / sqrt(n),
      lo        = median - qt(0.975, df = n - 1) * se_approx,
      hi        = median + qt(0.975, df = n - 1) * se_approx,
      region    = factor(region,
                         levels = c("Ontario", "Quebec", "Alberta",
                                    "Atlantic Canada", "All Canada"))
    ) |>
    dplyr::select(region, dv, dv_type, n, median, mad, lo, hi)
})

median_ci_mad |>
  mutate(dv = factor(dv, levels = dv_order)) |>
  ggplot(aes(x = median, y = region, colour = region)) +
  geom_point(size = 3.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.3, linewidth = 0.5) +
  scale_colour_manual(
    values = c(region_colours, "All Canada" = "#636363"),
    guide  = "none"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.33, 0.66, 1)) +
  facet_wrap(~ dv, ncol = 2) +
  labs(x       = "Median response (0\u20131 scale) \u2014 MAD-based 95% CI",
       y       = NULL,
       caption = "Median response on 0/0.33/0.66/1 scale. Error bars = MAD-based robust CI. DV scale: 0 = Unfair, 1 = Fair.") +
  theme_cpp() +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(paths$desc, "desc_median_dotCI_by_region.png"),
       width = plot_width, height = plot_height + 2, dpi = plot_dpi)
cat("Saved: desc_median_dotCI_by_region.png\n")


# ==============================================================
# 4.  TABLE 1 — DESCRIPTIVE MEANS (LaTeX)
# ==============================================================

desc_table <- mean_ci |>
  mutate(cell = sprintf("%.2f [%.2f, %.2f]", mean, lo, hi)) |>
  dplyr::select(region, dv, dv_type, cell) |>
  pivot_wider(names_from = region, values_from = cell) |>
  dplyr::select(dv, dv_type,
                any_of(c("Ontario", "Quebec", "Alberta",
                         "Atlantic Canada", "All Canada"))) |>
  mutate(dv = factor(dv, levels = c(unname(prop_labels),
                                    unname(recip_labels)))) |>
  arrange(dv) |>
  mutate(dv = as.character(dv))

desc_table |>
  dplyr::select(-dv_type) |>
  kbl(format    = "latex",
      booktabs  = TRUE,
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
  save_kable(file.path(paths$desc, "table_desc_fairness.tex"))


cat("Saved: table_desc_fairness.tex\n")
cat("\n========== DESCRIPTIVES COMPLETE ==========\n")
cat("Output:", paths$desc, "\n")
