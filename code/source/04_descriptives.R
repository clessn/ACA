# ==============================================================
# 04_descriptives.R
# All descriptive bar charts: importance, priority, tradeoffs,
# composites, regional comparisons
# ==============================================================


# ── SHARED PLOT THEME ─────────────────────────────────────────

desc_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    plot.title   = element_text(face = "bold", color = "black"),
    plot.caption = element_text(color = "black")
  )

dodge_theme <- desc_theme +
  theme(legend.position = "bottom")


# ── SHARED DODGED BAR HELPER ──────────────────────────────────

plot_pref_vs_intense <- function(plot_df, x_limit, file_path,
                                 width = 10, height = 6) {
  ggplot(plot_df, aes(x = pct, y = label, fill = metric, pattern = metric)) +
    geom_col_pattern(
      position        = position_dodge(width = 0.7),
      width           = 0.62, color = "black",
      pattern_fill    = "black", pattern_density = 0.05, pattern_spacing = 0.02
    ) +
    geom_text(
      aes(label = sprintf("%.1f%%", pct)),
      position = position_dodge(width = 0.7),
      hjust = -0.12, size = 3.3, color = "black"
    ) +
    scale_fill_manual(values    = c("First choice" = "grey30", "Intense preference" = "grey70")) +
    scale_pattern_manual(values = c("First choice" = "none",   "Intense preference" = "stripe")) +
    scale_x_continuous(limits = c(0, x_limit),
                       labels = scales::label_number(suffix = "%"),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = "% of respondents", y = NULL, fill = NULL, pattern = NULL,
         caption = paste0(
           "First choice: respondent allocated the most points to this area.\n",
           "Intense preference: respondent allocated >50 of 100 points to this area."
         )) +
    dodge_theme
  ggsave(file_path, width = width, height = height, dpi = params$dpi)
}


# ── 5.1  IMPORTANCE BINARY ────────────────────────────────────

imp_bin_df |>
  mutate(pct   = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
         label = fct_reorder(label, pct)) |>
  ggplot(aes(x = pct, y = label)) +
  geom_col(fill = "#2166ac", width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.12, size = 3.6) +
  scale_x_continuous(limits = c(0, 100),
                     labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "% of respondents", y = NULL,
       caption = "Binary: 1 = respondent ranked first out of all options") +
  desc_theme

ggsave(file.path(params$out_desc, "desc_imp_bin_grouped.png"),
       width = 9, height = 5.5, dpi = params$dpi)


# ── 5.2  PRIORITY FIRST CHOICE ────────────────────────────────

tibble(var = prio_pref_vars) |>
  mutate(policy = str_extract(var, "(?<=budget_prio_).*(?=_pref)"),
         label  = recode(policy, !!!prio_policy_labels, .default = policy),
         pct    = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
         label  = fct_reorder(label, pct)) |>
  ggplot(aes(x = pct, y = label)) +
  geom_col(fill = "#d6604d", width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.12, size = 3.6) +
  scale_x_continuous(limits = c(0, 80),
                     labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "% of respondents choosing this as their first priority", y = NULL,
       caption = "Binary: 1 = respondent allocated the most points to this policy") +
  desc_theme

ggsave(file.path(params$out_desc, "desc_prio_pref_grouped.png"),
       width = 9, height = 5.5, dpi = params$dpi)


# ── 5.3  PRIORITY INTENSE ────────────────────────────────────

tibble(var = prio_intense_vars) |>
  mutate(policy = str_extract(var, "(?<=budget_prio_).*(?=_intense)"),
         label  = recode(policy, !!!prio_policy_labels, .default = policy),
         pct    = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
         label  = fct_reorder(label, pct)) |>
  ggplot(aes(x = pct, y = label)) +
  geom_col(fill = "#762a83", width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.12, size = 3.6) +
  scale_x_continuous(limits = c(0, 20),
                     labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "% of respondents allocating >50 points (intense preference)", y = NULL,
       caption = "Binary: 1 = respondent allocated >50 of 100 points to this policy") +
  desc_theme

ggsave(file.path(params$out_desc, "desc_prio_intense_grouped.png"),
       width = 9, height = 5.5, dpi = params$dpi)


# ── 5.4  PRIORITY PREF vs INTENSE DODGED ─────────────────────

prio_pref_base <- tibble(var = prio_pref_vars) |>
  mutate(policy = str_extract(var, "(?<=budget_prio_).*(?=_pref)"),
         label  = recode(policy, !!!prio_policy_labels, .default = policy),
         pct    = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
         metric = "First choice")

prio_intense_base <- tibble(var = prio_intense_vars) |>
  mutate(policy = str_extract(var, "(?<=budget_prio_).*(?=_intense)"),
         label  = recode(policy, !!!prio_policy_labels, .default = policy),
         pct    = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
         metric = "Intense preference")

pref_order <- prio_pref_base |> arrange(pct) |> pull(label)

bind_rows(prio_pref_base, prio_intense_base) |>
  mutate(label  = factor(label,  levels = pref_order),
         metric = factor(metric, levels = c("First choice", "Intense preference"))) |>
  plot_pref_vs_intense(
    x_limit   = 80,
    file_path = file.path(params$out_desc, "desc_prio_pref_vs_intense.png")
  )


# ── 5.5  PRIORITY MEAN ALLOCATION ────────────────────────────

prio_num_df |>
  mutate(mean_alloc = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE)),
         sd_alloc   = map_dbl(var, ~ sd(df[[.x]],   na.rm = TRUE)),
         label      = fct_reorder(label, mean_alloc)) |>
  ggplot(aes(x = mean_alloc, y = label)) +
  geom_col(fill = "#4dac26", width = 0.65) +
  geom_errorbarh(aes(xmin = pmax(mean_alloc - sd_alloc, 0),
                     xmax = mean_alloc + sd_alloc),
                 height = 0.25, color = "grey30") +
  geom_text(aes(label = sprintf("%.1f", mean_alloc)), hjust = -0.2, size = 3.5) +
  scale_x_continuous(limits = c(0, 80),
                     labels = scales::label_number(suffix = " pts"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Mean budget points allocated (out of 100)", y = NULL,
       caption = "Error bars = ±1 SD. Points sum to 100 per respondent.") +
  desc_theme

ggsave(file.path(params$out_desc, "desc_prio_mean.png"),
       width = 9, height = 5.5, dpi = params$dpi)


# ── 5B. TRADEOFF BATTERY DESCRIPTIVES ────────────────────────

build_tradeoff_df <- function(battery, suffix = "") {
  var_map   <- tradeoff_labels[[battery]]
  base_vars <- names(var_map)
  vars      <- if (suffix != "") paste0(base_vars, suffix) else base_vars
  tibble(var = vars, label = unname(var_map), battery = battery) |>
    mutate(pct = map_dbl(var, ~ {
      col <- df[[.x]]
      if (is.null(col)) NA_real_
      else if (max(col, na.rm = TRUE) <= 1) mean(col, na.rm = TRUE) * 100
      else mean(col, na.rm = TRUE)
    }))
}

plot_tradeoff_bar <- function(plot_df, x_lab, fill_col, x_limit,
                              is_pct = TRUE, file_path,
                              width = 9, height = NULL) {
  fmt    <- if (is_pct) "%.1f%%" else "%.1f"
  suffix <- if (is_pct) "%" else " pts"
  h      <- height %||% (4.5 + 0.4 * nrow(plot_df))
  p <- plot_df |>
    mutate(label = fct_reorder(label, pct)) |>
    ggplot(aes(x = pct, y = label)) +
    geom_col(fill = fill_col, width = 0.65, color = "black") +
    geom_text(aes(label = sprintf(fmt, pct)), hjust = -0.12, size = 3.6, color = "black") +
    scale_x_continuous(limits = c(0, x_limit),
                       labels = scales::label_number(suffix = suffix),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = x_lab, y = NULL) +
    desc_theme
  ggsave(file_path, plot = p, width = width, height = h, dpi = params$dpi)
}

plot_tradeoff_comparison <- function(battery, file_path) {
  pref_df    <- build_tradeoff_df(battery, "_pref")    |> mutate(metric = "First choice")
  intense_df <- build_tradeoff_df(battery, "_intense") |> mutate(metric = "Intense preference")
  sort_order <- pref_df |> arrange(pct) |> pull(label)
  bind_rows(pref_df, intense_df) |>
    mutate(label  = factor(label,  levels = sort_order),
           metric = factor(metric, levels = c("First choice", "Intense preference"))) |>
    plot_pref_vs_intense(x_limit = 80, file_path = file_path,
                         width = 10, height = 5.5)
}

batteries_desc <- names(tradeoff_labels)

walk(batteries_desc, function(bat) {
  plot_tradeoff_bar(build_tradeoff_df(bat),
                    "Mean points allocated (out of 100)",
                    "#4dac26", 80, is_pct = FALSE,
                    file.path(params$out_desc, paste0("tradeoff_", bat, "_mean.png")))
  
  plot_tradeoff_bar(build_tradeoff_df(bat, "_pref"),
                    "% of respondents (first choice)",
                    "#d6604d", 80, is_pct = TRUE,
                    file.path(params$out_desc, paste0("tradeoff_", bat, "_pref.png")))
  
  plot_tradeoff_bar(build_tradeoff_df(bat, "_intense"),
                    "% of respondents (intense preference: >50 pts)",
                    "#762a83", 60, is_pct = TRUE,
                    file.path(params$out_desc, paste0("tradeoff_", bat, "_intense.png")))
  
  plot_tradeoff_comparison(bat,
                           file.path(params$out_desc, paste0("tradeoff_", bat, "_pref_vs_intense.png")))
  cat("Battery", bat, "-- all 4 plots saved.\n")
})

# ── 5C. TRADEOFF REGIONAL ─────────────────────────────────────

build_tradeoff_regional <- function(battery) {
  var_map    <- tradeoff_labels[[battery]]
  base_vars  <- names(var_map)
  opt_labels <- unname(var_map)
  map_dfr(region_levels, function(reg) {
    reg_data <- df |> filter(.data[[region_var]] == reg)
    bind_rows(
      tibble(region = reg, label = opt_labels,
             var = paste0(base_vars, "_pref"),    metric = "First choice"),
      tibble(region = reg, label = opt_labels,
             var = paste0(base_vars, "_intense"), metric = "Intense preference")
    ) |>
      mutate(
        pct    = map_dbl(var, ~ {
          col <- reg_data[[.x]]
          if (is.null(col) || all(is.na(col))) NA_real_
          else mean(col, na.rm = TRUE) * 100
        }),
        region = factor(region, levels = region_levels),
        metric = factor(metric, levels = c("First choice", "Intense preference"))
      )
  })
}

plot_tradeoff_regional <- function(battery, file_path) {
  plot_df    <- build_tradeoff_regional(battery)
  sort_order <- plot_df |>
    filter(metric == "First choice") |>
    group_by(label) |>
    summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop") |>
    arrange(mean_pct) |> pull(label)
  n_opts <- length(tradeoff_labels[[battery]])

  plot_df |>
    mutate(label = factor(label, levels = sort_order)) |>
    ggplot(aes(x = pct, y = label, fill = metric, pattern = metric)) +
    geom_col_pattern(position = position_dodge(width = 0.7), width = 0.62,
                     color = "black", pattern_fill = "black",
                     pattern_density = 0.05, pattern_spacing = 0.02) +
    geom_text(aes(label = sprintf("%.1f%%", pct)),
              position = position_dodge(width = 0.7),
              hjust = -0.1, size = 2.8, color = "black") +
    scale_fill_manual(values    = c("First choice" = "grey30", "Intense preference" = "grey70")) +
    scale_pattern_manual(values = c("First choice" = "none",   "Intense preference" = "stripe")) +
    scale_x_continuous(limits = c(0, 85),
                       labels = scales::label_number(suffix = "%"),
                       expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(~region, ncol = 2) +
    labs(x = "% of respondents within region", y = NULL, fill = NULL, pattern = NULL,
         caption = paste0(
           "First choice: respondent allocated the most points to this option.\n",
           "Intense preference: respondent allocated >50 of 100 points to this option.\n",
           "Percentages computed within each region."
         )) +
    dodge_theme +
    theme(strip.text = element_text(face = "bold", color = "black"),
          plot.caption = element_text(color = "grey30"))
  ggsave(file_path, width = 13, height = 5 + n_opts * 0.6, dpi = params$dpi)
}

walk(batteries_desc, function(bat) {
  plot_tradeoff_regional(bat,
    file.path(params$out_desc, paste0("tradeoff_", bat, "_regional.png")))
  cat("Battery", bat, "-- regional plot saved.\n")
})


# ── 5D. COMPOSITE DESCRIPTIVES ───────────────────────────────

# 5D.1  Expansion vs Reduction
tibble(
  label = c("Expansion priority\n(Health, education, or pensions ranked first)",
            "Reduction priority\n(Tax or debt reduction ranked first)"),
  pct   = c(
    mean(df$budget_imp_health_bin == 1 | df$budget_imp_edu_bin == 1 |
           df$budget_imp_pensions_bin == 1, na.rm = TRUE) * 100,
    mean(df$budget_imp_taxes_bin  == 1 | df$budget_imp_debt_bin  == 1,
         na.rm = TRUE) * 100
  )
) |>
  ggplot(aes(x = pct, y = reorder(label, pct))) +
  geom_col(fill = "#2166ac", width = 0.5, color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.12, size = 4, color = "black") +
  scale_x_continuous(limits = c(0, 100),
                     labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "% of respondents", y = NULL,
       caption = "Expansion: ranked healthcare, education, or pensions as budget priority.\nReduction: ranked tax or debt reduction as budget priority.") +
  desc_theme

ggsave(file.path(params$out_desc, "desc_composite_imp_expansion_reduc.png"),
       width = 9, height = 4, dpi = params$dpi)


# 5D.2  Social policy vs Other
build_composite_pref_intense <- function(pref_labels, intense_labels_vec,
                                         pref_fns, intense_fns) {
  pref_df <- tibble(label = pref_labels,
                    pct   = map_dbl(pref_fns, ~mean(.x, na.rm = TRUE) * 100),
                    metric = "First choice")
  int_df  <- tibble(label = intense_labels_vec,
                    pct   = map_dbl(intense_fns, ~mean(.x, na.rm = TRUE) * 100),
                    metric = "Intense preference")
  sort_order <- pref_df |> arrange(pct) |> pull(label)
  bind_rows(pref_df, int_df) |>
    mutate(label  = factor(label,  levels = sort_order),
           metric = factor(metric, levels = c("First choice", "Intense preference")))
}

soc_vs_other_df <- build_composite_pref_intense(
  pref_labels = c("Social policy (Healthcare, Home care, Subsidized child care)",
                  "Other (Economic growth, Climate change)"),
  intense_labels_vec = c("Social policy (Healthcare, Home care, Subsidized child care)",
                         "Other (Economic growth, Climate change)"),
  pref_fns = list(
    with(df, budget_prio_health_pref == 1 | budget_prio_seniors_pref == 1 | budget_prio_cc_pref == 1),
    with(df, budget_prio_ecn_pref == 1 | budget_prio_clim_pref == 1)
  ),
  intense_fns = list(
    with(df, budget_prio_health_intense == 1 | budget_prio_seniors_intense == 1 | budget_prio_cc_intense == 1),
    with(df, budget_prio_ecn_intense == 1 | budget_prio_clim_intense == 1)
  )
)

soc_vs_other_df |>
  plot_pref_vs_intense(x_limit = 100,
                       file_path = file.path(params$out_desc, "desc_composite_prio_soc_vs_other.png"),
                       width = 10, height = 5)


# 5D.3  HC universal vs targeted
build_composite_pref_intense(
  pref_labels        = c("Universal (all seniors)", "Targeted (low-income seniors)"),
  intense_labels_vec = c("Universal (all seniors)", "Targeted (low-income seniors)"),
  pref_fns = list(
    with(df, tradeoff_hc_all_pref == 1),
    with(df, tradeoff_hc_spend_pref == 1 | tradeoff_hc_pensions_pref == 1)
  ),
  intense_fns = list(
    with(df, tradeoff_hc_all_intense == 1),
    with(df, tradeoff_hc_spend_intense == 1 | tradeoff_hc_pensions_intense == 1)
  )
) |>
  plot_pref_vs_intense(x_limit = 100,
                       file_path = file.path(params$out_desc, "desc_composite_hc_univ_vs_target.png"),
                       width = 10, height = 5)


# 5D.4  CC2 universal vs targeted
build_composite_pref_intense(
  pref_labels        = c("Universal (all families)", "Targeted (low-income families)"),
  intense_labels_vec = c("Universal (all families)", "Targeted (low-income families)"),
  pref_fns = list(
    with(df, tradeoff_cc2_all_pref == 1 | tradeoff_cc2_educ_all_pref == 1),
    with(df, tradeoff_cc2_low_inc_pref == 1 | tradeoff_cc2_educ_low_inc_pref == 1)
  ),
  intense_fns = list(
    with(df, tradeoff_cc2_all_intense == 1 | tradeoff_cc2_educ_all_intense == 1),
    with(df, tradeoff_cc2_low_inc_intense == 1 | tradeoff_cc2_educ_low_inc_intense == 1)
  )
) |>
  plot_pref_vs_intense(x_limit = 100,
                       file_path = file.path(params$out_desc, "desc_composite_cc_univ_vs_target.png"),
                       width = 10, height = 5)


# ── 5.6  PRIORITY BY REGION ───────────────────────────────────

prio_var_map <- tibble(
  var_pref    = prio_pref_vars,
  var_intense = prio_intense_vars,
  policy      = str_extract(prio_pref_vars, "(?<=budget_prio_).*(?=_pref)"),
  label       = recode(str_extract(prio_pref_vars, "(?<=budget_prio_).*(?=_pref)"),
                       !!!prio_policy_labels, .default = policy)
)

prio_regional_df <- map_dfr(region_levels, function(reg) {
  reg_data <- df |> filter(.data[[region_var]] == reg)
  bind_rows(
    prio_var_map |> transmute(region = reg, label, var = var_pref,    metric = "First choice"),
    prio_var_map |> transmute(region = reg, label, var = var_intense, metric = "Intense preference")
  ) |>
    mutate(pct    = map_dbl(var, ~ mean(reg_data[[.x]], na.rm = TRUE) * 100),
           region = factor(region, levels = region_levels),
           metric = factor(metric, levels = c("First choice", "Intense preference")))
})

sort_order_prio <- prio_regional_df |>
  filter(metric == "First choice") |>
  group_by(label) |>
  summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop") |>
  arrange(mean_pct) |> pull(label)

prio_regional_df |>
  mutate(label = factor(label, levels = sort_order_prio)) |>
  ggplot(aes(x = pct, y = label, fill = metric, pattern = metric)) +
  geom_col_pattern(position = position_dodge(width = 0.7), width = 0.62,
                   color = "black", pattern_fill = "black",
                   pattern_density = 0.05, pattern_spacing = 0.02) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 2.8, color = "black") +
  scale_fill_manual(values    = c("First choice" = "grey30", "Intense preference" = "grey70")) +
  scale_pattern_manual(values = c("First choice" = "none",   "Intense preference" = "stripe")) +
  scale_x_continuous(limits = c(0, 85),
                     labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~region, ncol = 2) +
  labs(x = "% of respondents within region", y = NULL, fill = NULL, pattern = NULL,
       caption = paste0(
         "First choice: respondent allocated the most points to this area.\n",
         "Intense preference: respondent allocated >50 of 100 points to this area.\n",
         "Percentages computed within each region."
       )) +
  dodge_theme +
  theme(strip.text = element_text(face = "bold", color = "black"),
        plot.caption = element_text(color = "grey30"))

ggsave(file.path(params$out_desc, "desc_prio_pref_vs_intense_regional.png"),
       width = 13, height = 7, dpi = params$dpi)
