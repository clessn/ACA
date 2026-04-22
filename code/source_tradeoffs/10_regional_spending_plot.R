# ==============================================================
# 10_regional_spending_plot.R
# Regional descriptive plot: Importance (raking) + Priority
# (first choice & intense) by region
# Run after 03_variable_definitions.R is sourced
# ==============================================================


# ── COLOUR PALETTE ────────────────────────────────────────────
# Importance = blue family  |  Priority = red/purple family
col_imp    <- "#2166ac"   # solid blue   — importance (ranking #1)
col_pref   <- "#d6604d"   # red-orange   — priority first choice
col_int    <- "#762a83"   # purple       — priority intense

# ── 1. IMPORTANCE (RAKING) BY REGION ─────────────────────────
# budget_imp_*_bin == 1  →  respondent ranked this area #1

imp_regional_df <- map_dfr(region_levels, function(reg) {
  reg_data <- df |> dplyr::filter(.data[[region_var]] == reg)
  imp_bin_df |>
    mutate(
      pct    = map_dbl(var, ~ mean(reg_data[[.x]], na.rm = TRUE) * 100),
      region = reg,
      metric = "Importance: ranked #1 (raking)"
    )
}) |>
  mutate(
    region = factor(region, levels = region_levels),
    metric = factor(metric)
  )


# ── 2. PRIORITY (FIRST CHOICE + INTENSE) BY REGION ───────────
# budget_prio_*_pref    == 1  →  allocated most points
# budget_prio_*_intense == 1  →  allocated >50 points

prio_var_map <- tibble(
  var_pref    = prio_pref_vars,
  var_intense = prio_intense_vars,
  policy      = str_extract(prio_pref_vars, "(?<=budget_prio_).*(?=_pref)"),
  label       = recode(
    str_extract(prio_pref_vars, "(?<=budget_prio_).*(?=_pref)"),
    !!!prio_policy_labels, .default = policy
  )
)

prio_regional_df <- map_dfr(region_levels, function(reg) {
  reg_data <- df |> dplyr::filter(.data[[region_var]] == reg)
  bind_rows(
    prio_var_map |> transmute(
      region = reg, label, var = var_pref,
      metric = "Priority: first choice"
    ),
    prio_var_map |> transmute(
      region = reg, label, var = var_intense,
      metric = "Priority: intense preference (>50 pts)"
    )
  ) |>
    mutate(
      pct    = map_dbl(var, ~ mean(reg_data[[.x]], na.rm = TRUE) * 100),
      region = factor(region, levels = region_levels),
      metric = factor(metric, levels = c(
        "Priority: first choice",
        "Priority: intense preference (>50 pts)"
      ))
    )
})


# ── 3. COMBINED PLOT ──────────────────────────────────────────

# 3a. Sort order: importance labels ranked by national mean
imp_sort <- imp_regional_df |>
  group_by(label) |>
  summarise(m = mean(pct), .groups = "drop") |>
  arrange(m) |>
  pull(label)

prio_sort <- prio_regional_df |>
  dplyr::filter(metric == "Priority: first choice") |>
  group_by(label) |>
  summarise(m = mean(pct), .groups = "drop") |>
  arrange(m) |>
  pull(label)


# ── 3b. IMPORTANCE PANEL ──────────────────────────────────────

p_imp <- imp_regional_df |>
  mutate(label = factor(label, levels = imp_sort)) |>
  ggplot(aes(x = pct, y = label)) +
  geom_col(fill = col_imp, width = 0.65, color = "black") +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    hjust = -0.1, size = 2.8, color = "black"
  ) +
  scale_x_continuous(
    limits = c(0, 75),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  facet_wrap(~region, ncol = 2) +
  labs(
    title   = "A. Budget Importance (raking question)",
    subtitle = "% ranking this area as #1 spending priority",
    x = "% of respondents within region", y = NULL,
    caption = "Binary: 1 = respondent ranked this area first out of all options."
  ) +
  desc_theme +
  theme(
    strip.text   = element_text(face = "bold", color = "black"),
    plot.caption = element_text(color = "grey30"),
    plot.title   = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30")
  )


# ── 3c. PRIORITY PANEL (first choice + intense, dodged) ───────

p_prio <- prio_regional_df |>
  mutate(label = factor(label, levels = prio_sort)) |>
  ggplot(aes(x = pct, y = label,
             fill = metric, pattern = metric)) +
  geom_col_pattern(
    position        = position_dodge(width = 0.7),
    width           = 0.62, color = "black",
    pattern_fill    = "black", pattern_density = 0.05,
    pattern_spacing = 0.02
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    position = position_dodge(width = 0.7),
    hjust = -0.1, size = 2.5, color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Priority: first choice"                = col_pref,
      "Priority: intense preference (>50 pts)"= col_int
    )
  ) +
  scale_pattern_manual(
    values = c(
      "Priority: first choice"                = "none",
      "Priority: intense preference (>50 pts)"= "stripe"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 85),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  facet_wrap(~region, ncol = 2) +
  labs(
    title    = "B. Budget Priority (point-allocation question)",
    subtitle = "% choosing as first priority / allocating >50 of 100 points",
    x = "% of respondents within region", y = NULL,
    fill = NULL, pattern = NULL,
    caption = paste0(
      "First choice: respondent allocated the most points to this area.\n",
      "Intense preference: respondent allocated >50 of 100 points to this area.\n",
      "Percentages computed within each region."
    )
  ) +
  dodge_theme +
  theme(
    strip.text    = element_text(face = "bold", color = "black"),
    plot.caption  = element_text(color = "grey30"),
    plot.title    = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    legend.position = "bottom"
  )


# ── 3d. EXPANSION VS REDUCTION COMPOSITE BY REGION ───────────
# Expansion = health, education, or pensions ranked #1 (raking)
# Reduction  = tax or debt reduction ranked #1 (raking)

composite_regional_df <- map_dfr(region_levels, function(reg) {
  reg_data <- df |> dplyr::filter(.data[[region_var]] == reg)
  tibble(
    region = reg,
    label  = c(
      "Expansion\n(Health, Education, or Pensions ranked #1)",
      "Reduction\n(Tax or Debt reduction ranked #1)"
    ),
    pct = c(
      mean(reg_data$budget_imp_health_bin   == 1 |
             reg_data$budget_imp_edu_bin      == 1 |
             reg_data$budget_imp_pensions_bin == 1, na.rm = TRUE) * 100,
      mean(reg_data$budget_imp_taxes_bin == 1 |
             reg_data$budget_imp_debt_bin  == 1,    na.rm = TRUE) * 100
    )
  )
}) |>
  mutate(region = factor(region, levels = region_levels))

# Fix label order: Expansion on top, Reduction on bottom
composite_regional_df <- composite_regional_df |>
  mutate(label = factor(label, levels = c(
    "Reduction\n(Tax or Debt reduction ranked #1)",
    "Expansion\n(Health, Education, or Pensions ranked #1)"
  )))

p_composite <- composite_regional_df |>
  ggplot(aes(x = pct, y = label,
             fill = label)) +
  geom_col(width = 0.55, color = "black") +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    hjust = -0.1, size = 3.2, color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Expansion\n(Health, Education, or Pensions ranked #1)" = "#2166ac",
      "Reduction\n(Tax or Debt reduction ranked #1)"          = "#d6604d"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  facet_wrap(~region, ncol = 2) +
  labs(
    title    = "Investment vs. Fiscal Restraint Composite by Region",
    subtitle = "% ranking spending expansion or reduction as #1 budget priority (raking question)",
    x = "% of respondents within region", y = NULL,
    caption = paste0(
      "Expansion: ranked healthcare, education, or pensions as #1 budget priority.\n",
      "Reduction: ranked tax or debt reduction as #1 budget priority.\n",
      "Percentages computed within each region. Categories are not mutually exclusive."
    )
  ) +
  desc_theme +
  theme(
    strip.text    = element_text(face = "bold", color = "black"),
    plot.caption  = element_text(color = "grey30"),
    plot.title    = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30")
  )


# ── 4. SAVE SEPARATELY ────────────────────────────────────────

out_imp  <- file.path(params$out_desc, "desc_regional_imp.png")
out_prio <- file.path(params$out_desc, "desc_regional_prio.png")

ggsave(out_imp,  plot = p_imp,  width = 13, height = 7,  dpi = params$dpi)
cat("Saved:", out_imp, "\n")

ggsave(out_prio, plot = p_prio, width = 13, height = 8, dpi = params$dpi)
cat("Saved:", out_prio, "\n")

out_comp <- file.path(params$out_desc, "desc_regional_composite_exp_reduc.png")
ggsave(out_comp, plot = p_composite, width = 13, height = 6, dpi = params$dpi)
cat("Saved:", out_comp, "\n")
