
# ============================================================
# Budgetary Preferences - Descriptives + Regressions
# Four-region Canadian survey: Alberta, Ontario, Quebec, Eastern Canada
#
# DV strategy:
#   budget_imp_X_bin   -> LPM (OLS) + Logit, binary outcome
#   budget_prio_X      -> OLS, continuous 0-100 raw score
#   budget_prio_X_pref / _intense -> descriptive bar charts only
#
# All models use HC1 robust standard errors.
# ============================================================

# 0.1. Packages
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(marginaleffects)
library(modelsummary)
library(sandwich)
library(lmtest)
library(ggthemes)
library(RColorBrewer)
library(ggpattern)

# To avoid conflicts with MASS
select <- dplyr::select
filter <- dplyr::filter
# 0.2. Load data
df <- read.csv("data/clean_df_valid.csv")

# 0.3. Output folders
# dir.create("graphs/descriptives", recursive = TRUE, showWarnings = FALSE)
# dir.create("graphs/regressions",  recursive = TRUE, showWarnings = FALSE)


#*************************************************************************#
#########              1. VARIABLE CONSTRUCTION                   #########
#*************************************************************************#
# To easily copy past all tradeoff/budgetary DV
cat(paste0('"', names(df), '"'), sep = "\n")

list(df$budget_prio_cc_intense)

# University-education binary (reference = below university)
df$univ_educ_bin <- as.integer(df$educ_group == "educUniv")

# High-income binary (reference = low + mid)
if (!"incomeHigh_bin" %in% names(df)) {
  df$incomeHigh_bin <- as.integer(df$ses_income3Cat == "High")
}

# Ordered factors
df$educ_group     <- factor(df$educ_group,     levels = c("educBHS", "educHS", "educUniv"))
df$ses_region_cat <- factor(df$ses_region_cat, levels = c("Ontario", "Quebec", "Alberta", "East Coast"))
df$ses_income3Cat <- factor(df$ses_income3Cat, levels = c("Low", "Mid", "High"))


#*************************************************************************#
#########              2. DEFINE IVs AND MODEL RHS               #########
#*************************************************************************#

ivs <- list(
  list(type="binary",  var="quebec_bin",               low=0, high=1, label="Quebec (No=0 vs Yes=1)"),
  list(type="binary",  var="ideo_define_QC_first_bin",  low=0, high=1, label="Quebecker First (No=0 vs Yes=1)"),
  list(type="binary",  var="alberta_bin",              low=0, high=1, label="Alberta (No=0 vs Yes=1)"),
  list(type="binary",  var="ontario_bin",              low=0, high=1, label="Ontario (No=0 vs Yes=1)"),
  list(type="binary",  var="region_eastcoast_bin",     low=0, high=1, label="Eastern Canada (No=0 vs Yes=1)"),
  list(type="binary",  var="incomeHigh_bin",           low=0, high=1, label="High Income (No=0 vs Yes=1)"),
  list(type="binary",  var="ses_male_bin",             low=0, high=1, label="Gender (Female=0 vs Male=1)"),
  list(type="binary",  var="age18_34_bin",             low=0, high=1, label="Age 18-34 (No=0 vs Yes=1)"),
  list(type="binary",  var="age55plus_bin",            low=0, high=1, label="Age 55+ (No=0 vs Yes=1)"),
  list(type="binary",  var="univ_educ_bin",            low=0, high=1, label="University Education (No=0 vs Yes=1)"),
  list(type="binary",  var="ses_citizenYes_bin",       low=0, high=1, label="Citizen (No=0 vs Yes=1)"),
  list(type="binary",  var="ses_french_bin",           low=0, high=1, label="French-speaking (No=0 vs Yes=1)"),
  list(type="binary",  var="trust_social_bin",         low=0, high=1, label="Social Trust (Low=0 vs High=1)"),
  list(type="binary",  var="employ_fulltime_bin",      low=0, high=1, label="Employed Full-Time (No=0 vs Yes=1)"),
  list(type="numeric", var="ideo_right_num",           low=0, high=1, label="Ideology: Left (0) vs Right (1)"),
  list(type="binary",  var="vote_PLC_bin",             low=0, high=1, label="Liberal voter (No=0 vs Yes=1)"),
  list(type="binary",  var="vote_PCC_bin",             low=0, high=1, label="Conservative voter (No=0 vs Yes=1)")
)

# RHS formula -- Ontario is the omitted region reference category
rhs <- "quebec_bin + ideo_define_QC_first_bin + alberta_bin + region_eastcoast_bin +
        incomeHigh_bin + ses_male_bin + age18_34_bin + age55plus_bin +
        univ_educ_bin + ses_citizenYes_bin + ses_french_bin +
        trust_social_bin + employ_fulltime_bin +
        ideo_right_num + vote_PLC_bin + vote_PCC_bin"

term_labels <- c(
  "quebec_bin"               = "Quebec",
  "ideo_define_QC_first_bin" = "Quebecker First",
  "alberta_bin"              = "Alberta",
  "region_eastcoast_bin"     = "Eastern Canada",
  "incomeHigh_bin"           = "High Income",
  "ses_male_bin"             = "Male",
  "age18_34_bin"             = "Age 18-34",
  "age55plus_bin"            = "Age 55+",
  "univ_educ_bin"            = "University Education",
  "ses_citizenYes_bin"       = "Citizen",
  "ses_french_bin"           = "French-speaking",
  "trust_social_bin"         = "Social Trust",
  "employ_fulltime_bin"      = "Employed Full-Time",
  "ideo_right_num"           = "Ideology (Right)",
  "vote_PLC_bin"             = "Liberal voter",
  "vote_PCC_bin"             = "Conservative voter"
)

policy_labels <- c(
  "health"="Healthcare", "edu"="Education", "pensions"="Pensions",
  "taxes"="Tax reduction", "debt"="Debt reduction", "seniors"="Seniors",
  "cc"="Cost of living", "ecn"="Economy", "clim"="Climate"
)


#*************************************************************************#
#########        3. HELPER FUNCTIONS                              #########
#*************************************************************************#

robust_vcov <- function(model) vcovHC(model, type = "HC1")

tidy_avg_slopes <- function(model, dv_label, question = NULL, data = df) {
  fml         <- formula(model)
  model_vars  <- all.vars(fml)
  model_data  <- data |> dplyr::select(all_of(model_vars)) |> drop_na()
  # Refit directly (update() loses .x pronoun once map2() moves on).
  # Pass model_data as newdata so marginaleffects only sees model columns,
  # suppressing the "N columns" performance warning.
  model_clean <- lm(fml, data = model_data)
  vm          <- robust_vcov(model_clean)
  avg_slopes(model_clean, vcov = vm, newdata = model_data) |>
    as_tibble() |>
    transmute(
      question, dv = dv_label, term,
      estimate  = round(estimate, 3), conf.low = round(conf.low, 3),
      conf.high = round(conf.high, 3), p.value  = round(p.value, 3),
      sig = case_when(p.value<0.001~"***",p.value<0.01~"**",p.value<0.05~"*",p.value<0.10~".",TRUE~""),
      direction = case_when(conf.low>0~"Positive",conf.high<0~"Negative",TRUE~"No clear effect")
    )
}

interpret_contrast <- function(dv, dv_label, iv, data) {
  iv_var <- iv$var; iv_low <- as.character(iv$low); iv_high <- as.character(iv$high)
  model_data <- data |> dplyr::select(all_of(c(dv, iv_var))) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", iv_var)), data = model_data)
  contrast_spec <- list(c(iv$low, iv$high)); names(contrast_spec) <- iv_var
  tryCatch({
    avg_comparisons(model, variables = contrast_spec, vcov = robust_vcov(model)) |>
      as_tibble() |>
      transmute(
        iv = iv$label, dv = dv_label, contrast = paste(iv_low, "vs", iv_high),
        estimate=round(estimate,3), conf.low=round(conf.low,3),
        conf.high=round(conf.high,3), p.value=round(p.value,3),
        sig = case_when(p.value<0.001~"***",p.value<0.01~"**",p.value<0.05~"*",p.value<0.10~".",TRUE~""),
        direction = case_when(conf.low>0~paste(iv_high,">",iv_low),conf.high<0~paste(iv_low,">",iv_high),TRUE~"No clear difference")
      )
  }, error = function(e) {
    cat("FAILED:", iv_var, "~", dv, "\n  Error:", conditionMessage(e), "\n")
    tibble(iv=iv$label,dv=dv_label,contrast=paste(iv_low,"vs",iv_high),
           estimate=NA_real_,conf.low=NA_real_,conf.high=NA_real_,p.value=NA_real_,sig=NA_character_,direction="ERROR")
  })
}

plot_coefs <- function(coef_df, title_str, file_path) {
  coef_df |>
    mutate(term = recode(term, !!!term_labels)) |>
    ggplot(aes(x=estimate, y=reorder(term,estimate), color=direction)) +
    geom_vline(xintercept=0, linetype="dashed", color="grey50") +
    geom_point(size=2.5) +
    geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), height=0.2) +
    scale_color_manual(values=c("Positive"="#2166ac","Negative"="#d6604d","No clear effect"="grey60")) +
    facet_wrap(~dv, scales="free_x") +
    labs(x="Average marginal effect (HC1 robust SEs)", y=NULL, color=NULL, title=title_str) +
    theme_minimal(base_size=12) +
    theme(legend.position="bottom", strip.text=element_text(face="bold"))
  ggsave(file_path, width=13, height=8, dpi=300)
}

plot_r2 <- function(fit_df, r2_col="adj_r_sq", title_str, file_path) {
  fit_df |>
    mutate(
      fit_level = case_when(
        .data[[r2_col]] >= 0.07 ~ "High (>=0.07)",
        .data[[r2_col]] >= 0.03 ~ "Moderate (0.03-0.07)",
        TRUE ~ "Low (<0.03)"
      ),
      fit_level = factor(fit_level, levels=c("High (>=0.07)","Moderate (0.03-0.07)","Low (<0.03)")),
      dv = fct_reorder(dv, .data[[r2_col]])
    ) |>
    ggplot(aes(x=.data[[r2_col]], y=dv, fill=fit_level)) +
    geom_col() +
    geom_text(aes(label=sprintf("%.3f",.data[[r2_col]])), hjust=-0.15, size=3.2) +
    scale_fill_manual(values=c("High (>=0.07)"="#2166ac","Moderate (0.03-0.07)"="#92c5de","Low (<0.03)"="#d6604d")) +
    scale_x_continuous(limits=c(0,0.20), labels=scales::label_number(accuracy=0.01)) +
    labs(x=ifelse(r2_col=="pseudo_r2","McFadden pseudo-R2","Adjusted R2"),
         y=NULL, fill="Model fit", title=title_str) +
    theme_minimal(base_size=13) +
    theme(legend.position="bottom", panel.grid.major.y=element_blank())
  ggsave(file_path, width=8, height=5, dpi=300)
}


#*************************************************************************#
#########   4. DETECT DVs BY BATTERY                             #########
#*************************************************************************#
# Section 4 — force dplyr::select to avoid masking conflicts
imp_bin_vars <- df |> dplyr::select(matches("^budget_imp_.*_bin$")) |> names()
imp_policies <- str_extract(imp_bin_vars, "(?<=budget_imp_).*(?=_bin)")
imp_bin_df   <- tibble(var=imp_bin_vars, policy=imp_policies,
                       label=recode(imp_policies, !!!policy_labels, .default=imp_policies))

prio_num_vars <- df |> dplyr::select(matches("^budget_prio_[a-z]+$")) |> names()
prio_policies <- str_remove(prio_num_vars, "budget_prio_")
prio_num_df   <- tibble(var=prio_num_vars, policy=prio_policies,
                        label=recode(prio_policies, !!!policy_labels, .default=prio_policies))

prio_pref_vars    <- df |> dplyr::select(matches("^budget_prio_.*_pref$"))    |> names()
prio_intense_vars <- df |> dplyr::select(matches("^budget_prio_.*_intense$")) |> names()

#*************************************************************************#
#########   5. DESCRIPTIVE STATISTICS -- BAR CHARTS (UPDATED)   #########
#*************************************************************************#
# Produces four plots:
#   5.1  battery_imp_bin   -- importance binary, grouped bar per policy (ranking on 5)
#   5.2  battery_prio_pref -- priority first-choice, grouped bar per policy
#   5.3  battery_prio_intense -- priority intense allocators, grouped bar per policy
#   5.4  comparison        -- _pref vs _intense, dodged side-by-side per policy
#
# "Grouped bar per policy" = policies on the y-axis, one bar each (horizontal),
# ordered by value so the chart is immediately readable.
# The "grouped" framing becomes relevant in 5.4 where the two metrics sit
# side-by-side for every policy.
# -------------------------------------------------------------------------


# ── 5.1  Budget IMPORTANCE binary (budget_imp_*_bin) ─────────────────────
# One horizontal bar per spending area, ordered by % endorsing as top priority.

imp_bin_plot_df <- imp_bin_df |>
  mutate(
    pct   = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
    label = fct_reorder(label, pct)
  )

ggplot(imp_bin_plot_df, aes(x = pct, y = label)) +
  geom_col(fill = "#2166ac", width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.12, size = 3.6) +
  scale_x_continuous(
    limits = c(0, 100),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x       = "% of respondents",
    y       = NULL,
    title   = "Budget importance — share rating each area as a top priority",
    caption = "Binary: 1 = respondent ranked 1sr out of all options"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

ggsave("graphs/descriptives/desc_imp_bin_grouped.png",
       width = 9, height = 5.5, dpi = 300)


# ── 5.2  Budget PRIORITY — first choice (_pref) ──────────────────────────
# One horizontal bar per spending area, ordered by % selecting as first choice.

prio_pref_plot_df <- tibble(var = prio_pref_vars) |>
  mutate(
    policy = str_extract(var, "(?<=budget_prio_).*(?=_pref)"),
    label  = recode(policy, !!!policy_labels, .default = policy),
    pct    = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
    label  = fct_reorder(label, pct)
  )

ggplot(prio_pref_plot_df, aes(x = pct, y = label)) +
  geom_col(fill = "#d6604d", width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.12, size = 3.6) +
  scale_x_continuous(
    limits = c(0, 80),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x       = "% of respondents choosing this as their first priority",
    y       = NULL,
    title   = "Budget priority — share selecting each area as first choice",
    caption = "Binary: 1 = respondent allocated the most points to this policy"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

ggsave("graphs/descriptives/desc_prio_pref_grouped.png",
       width = 9, height = 5.5, dpi = 300)


# ── 5.3  Budget PRIORITY — intense allocators (_intense) ─────────────────
# One horizontal bar per spending area, ordered by % with >50 pts allocated.

prio_intense_plot_df <- tibble(var = prio_intense_vars) |>
  mutate(
    policy = str_extract(var, "(?<=budget_prio_).*(?=_intense)"),
    label  = recode(policy, !!!policy_labels, .default = policy),
    pct    = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
    label  = fct_reorder(label, pct)
  )

ggplot(prio_intense_plot_df, aes(x = pct, y = label)) +
  geom_col(fill = "#762a83", width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.12, size = 3.6) +
  scale_x_continuous(
    limits = c(0, 20),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x       = "% of respondents allocating >50 points (intense preference)",
    y       = NULL,
    title   = "Budget priority — share with intense preference per policy",
    caption = "Binary: 1 = respondent allocated >50 of 100 points to this policy"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

ggsave("graphs/descriptives/desc_prio_intense_grouped.png",
       width = 9, height = 5.5, dpi = 300)


# ── 5.4  COMPARISON: _pref vs _intense (dodged side-by-side) ─────────────
# Policies on y-axis; two bars per policy (first-choice vs intense allocator).
# Ordered by the _pref value so the dominant preference drives the sort.

# Budget questions:
# "budget_imp_health_num"
# "budget_imp_health_bin"
# "budget_imp_edu_num"
# "budget_imp_edu_bin"
# "budget_imp_pensions_num"
# "budget_imp_pensions_bin"
# "budget_imp_taxes_num"
# "budget_imp_taxes_bin"
# "budget_imp_debt_num"
# "budget_imp_debt_bin"
# "budget_prio_health"
# "budget_prio_seniors"
# "budget_prio_cc"
# "budget_prio_ecn"
# "budget_prio_clim"
# "budget_prio_health_pref"
# "budget_prio_seniors_pref"
# "budget_prio_cc_pref"
# "budget_prio_ecn_pref"
# "budget_prio_clim_pref"
# "budget_prio_health_intense"
# "budget_prio_seniors_intense"
# "budget_prio_cc_intense"
# "budget_prio_ecn_intense"
# "budget_prio_clim_intense"

# ── 5.4  COMPARISON: _pref vs _intense (dodged side-by-side) ─────────────

# Join pref and intense on shared policy keys
prio_pref_base <- tibble(var = prio_pref_vars) |>
  mutate(
    policy = str_extract(var, "(?<=budget_prio_).*(?=_pref)"),
    label  = recode(policy, !!!policy_labels, .default = policy),
    pct    = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
    metric = "First choice"
  )

prio_intense_base <- tibble(var = prio_intense_vars) |>
  mutate(
    policy = str_extract(var, "(?<=budget_prio_).*(?=_intense)"),
    label  = recode(policy, !!!policy_labels, .default = policy),
    pct    = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
    metric = "Intense preference"
  )

# Sort order based on _pref values
pref_order <- prio_pref_base |>
  arrange(pct) |>
  pull(label)

comparison_df <- bind_rows(prio_pref_base, prio_intense_base) |>
  mutate(
    label  = factor(label, levels = pref_order),
    metric = factor(metric, levels = c("First choice", "Intense preference"))
  )

ggplot(comparison_df, aes(x = pct, y = label, fill = metric, pattern = metric)) +
  geom_col_pattern(
    position        = position_dodge(width = 0.7),
    width           = 0.62,
    color           = "black",
    pattern_fill    = "black",
    pattern_density = 0.05,
    pattern_spacing = 0.02
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    position = position_dodge(width = 0.7),
    hjust = -0.12, size = 3.3, color = "black"
  ) +
  scale_fill_manual(
    values = c("First choice" = "grey30", "Intense preference" = "grey70")
  ) +
  scale_pattern_manual(
    values = c("First choice" = "none", "Intense preference" = "stripe")
  ) +
  scale_x_continuous(
    limits = c(0, 80),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x       = "% of respondents",
    y       = NULL,
    fill    = NULL,
    pattern = NULL,
    title   = "Budget priority: first choice vs. intense preference",
    caption = paste0(
      "First choice: respondent allocated the most points to this area.\n",
      "Intense preference: respondent allocated >50 of 100 points to this area."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text          = element_text(color = "black"),
    axis.title         = element_text(color = "black"),
    plot.title         = element_text(face = "bold", color = "black"),
    plot.caption       = element_text(color = "black")
  )

ggsave("graphs/descriptives/desc_prio_pref_vs_intense.png",
       width = 10, height = 6, dpi = 300)


# ── 5.5  Priority raw score: mean allocation with SD error bars ───────────
# (Retained from original — continuous OLS DV needs its own visual.)

prio_num_df |>
  mutate(
    mean_alloc = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE)),
    sd_alloc   = map_dbl(var, ~ sd(df[[.x]],   na.rm = TRUE)),
    label      = fct_reorder(label, mean_alloc)
  ) |>
  ggplot(aes(x = mean_alloc, y = label)) +
  geom_col(fill = "#4dac26", width = 0.65) +
  geom_errorbarh(
    aes(xmin = pmax(mean_alloc - sd_alloc, 0), xmax = mean_alloc + sd_alloc),
    height = 0.25, color = "grey30"
  ) +
  geom_text(aes(label = sprintf("%.1f", mean_alloc)), hjust = -0.2, size = 3.5) +
  scale_x_continuous(
    limits = c(0, 80),
    labels = scales::label_number(suffix = " pts"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x       = "Mean budget points allocated (out of 100)",
    y       = NULL,
    title   = "Budget priority — mean allocation across spending areas",
    caption = "Error bars = ±1 SD. Points sum to 100 per respondent."
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

ggsave("graphs/descriptives/desc_prio_mean.png", width = 9, height = 5.5, dpi = 300)

# Tradeoff questions
# "tradeoff_cc1_tax"
# "tradeoff_cc1_cut"
# "tradeoff_cc1_debt"
# "tradeoff_cc1_no_spend"
# "tradeoff_ge_tax"
# "tradeoff_ge_cut"
# "tradeoff_ge_debt"
# "tradeoff_ge_no_spend"
# "tradeoff_tax_less_services"
# "tradeoff_tax_sales_tax"
# "tradeoff_tax_inc_tax"
# "tradeoff_tax_wealth_tax"
# "tradeoff_hc_all"
# "tradeoff_hc_spend"
# "tradeoff_hc_pensions"
# "tradeoff_cc2_all"
# "tradeoff_cc2_low_inc"
# "tradeoff_cc2_educ_all"
# "tradeoff_cc2_educ_low_inc"
# "tradeoff_cc1_tax_pref"
# "tradeoff_cc1_cut_pref"
# "tradeoff_cc1_debt_pref"
# "tradeoff_cc1_no_spend_pref"
# "tradeoff_cc1_tax_intense"
# "tradeoff_cc1_cut_intense"
# "tradeoff_cc1_debt_intense"
# "tradeoff_cc1_no_spend_intense"
# "tradeoff_ge_tax_pref"
# "tradeoff_ge_cut_pref"
# "tradeoff_ge_debt_pref"
# "tradeoff_ge_no_spend_pref"
# "tradeoff_ge_tax_intense"
# "tradeoff_ge_cut_intense"
# "tradeoff_ge_debt_intense"
# "tradeoff_ge_no_spend_intense"
# "tradeoff_tax_less_services_pref"
# "tradeoff_tax_sales_tax_pref"
# "tradeoff_tax_inc_tax_pref"
# "tradeoff_tax_wealth_tax_pref"
# "tradeoff_tax_less_services_intense"
# "tradeoff_tax_sales_tax_intense"
# "tradeoff_tax_inc_tax_intense"
# "tradeoff_tax_wealth_tax_intense"
# "tradeoff_hc_all_pref"
# "tradeoff_hc_spend_pref"
# "tradeoff_hc_pensions_pref"
# "tradeoff_hc_all_intense"
# "tradeoff_hc_spend_intense"
# "tradeoff_hc_pensions_intense"
# "tradeoff_cc2_all_pref"
# "tradeoff_cc2_low_inc_pref"
# "tradeoff_cc2_educ_all_pref"
# "tradeoff_cc2_educ_low_inc_pref"
# "tradeoff_cc2_all_intense"
# "tradeoff_cc2_low_inc_intense"
# "tradeoff_cc2_educ_all_intense"
# "tradeoff_cc2_educ_low_inc_intense"

#*************************************************************************#
#########   5B. DESCRIPTIVE STATISTICS -- TRADEOFF BATTERIES    #########
#*************************************************************************#
# Five question batteries, each with:
#   - raw scores (0-100 pts, treated like prio_num)
#   - _pref  (first choice binary)
#   - _intense (>50 pts binary)
#
# For each battery we produce:
#   (a) grouped bar -- raw mean allocation
#   (b) grouped bar -- _pref distribution
#   (c) grouped bar -- _intense distribution
#   (d) dodged comparison -- _pref vs _intense
#
# Batteries:
#   cc1  = climate policy funding tradeoffs
#   ge   = general expenditure tradeoffs
#   tax  = tax type tradeoffs
#   hc   = homecare tradeoffs
#   cc2  = climate policy targeting tradeoffs
# -------------------------------------------------------------------------

# ── Variable labels per battery ──────────────────────────────────────────

tradeoff_labels <- list(
  cc1 = c(
    "tradeoff_cc1_tax"      = "Raise taxes",
    "tradeoff_cc1_cut"      = "Cut other spending",
    "tradeoff_cc1_debt"     = "Increase debt",
    "tradeoff_cc1_no_spend" = "Don't spend more"
  ),
  ge = c(
    "tradeoff_ge_tax"      = "Raise taxes",
    "tradeoff_ge_cut"      = "Cut other spending",
    "tradeoff_ge_debt"     = "Increase debt",
    "tradeoff_ge_no_spend" = "Don't spend more"
  ),
  tax = c(
    "tradeoff_tax_less_services" = "No increase, even if fewer services",
    "tradeoff_tax_sales_tax"     = "Sales tax increase",
    "tradeoff_tax_inc_tax"       = "Income tax increase",
    "tradeoff_tax_wealth_tax"    = "Wealth tax"
  ),
  hc = c(
    "tradeoff_hc_all"      = "Increase home care for all, reduce maximum old-age pension",
    "tradeoff_hc_spend"    = "Target low-income seniors, home care",
    "tradeoff_hc_pensions" = "Target low-income seniors, pensions"
  ),
  cc2 = c(
    "tradeoff_cc2_all"          = "Increase child care for all, reduce other family benefits",
    "tradeoff_cc2_low_inc"      = "Target low-income families, childcare",
    "tradeoff_cc2_educ_all"     = "Target education quality for all, reduce other family benefits",
    "tradeoff_cc2_educ_low_inc" = "Target education quality for low-income"
  )
)

battery_titles <- c(
  cc1 = "Increase childcare spending, how to fund it",
  ge  = "Increase green economy spending, how to fund it",
  tax = "Raise revenu, tax preferences",
  hc  = "Home care policy priorities, tradeoffs",
  cc2 = "Childcare policy priorities, tradeoffs"
)


# ── Helper: build tidy tibble for one battery ─────────────────────────────

build_tradeoff_df <- function(battery, suffix = "") {
  var_map <- tradeoff_labels[[battery]]
  base_vars <- names(var_map)
  if (suffix != "") {
    vars   <- paste0(base_vars, suffix)
    labels <- unname(var_map)
  } else {
    vars   <- base_vars
    labels <- unname(var_map)
  }
  tibble(
    var    = vars,
    label  = labels,
    battery = battery
  ) |>
    mutate(
      pct = map_dbl(var, ~ {
        col <- df[[.x]]
        if (is.null(col)) NA_real_
        else if (max(col, na.rm = TRUE) <= 1) mean(col, na.rm = TRUE) * 100
        else mean(col, na.rm = TRUE)
      })
    )
}


# ── Helper: single grouped bar chart ─────────────────────────────────────

plot_tradeoff_bar <- function(plot_df, x_lab, title_str, fill_col, x_limit, file_path, is_pct = TRUE) {
  fmt <- if (is_pct) "%.1f%%" else "%.1f"
  suffix <- if (is_pct) "%" else " pts"
  
  plot_df |>
    mutate(label = fct_reorder(label, pct)) |>
    ggplot(aes(x = pct, y = label)) +
    geom_col(fill = fill_col, width = 0.65, color = "black") +
    geom_text(aes(label = sprintf(fmt, pct)), hjust = -0.12, size = 3.6, color = "black") +
    scale_x_continuous(
      limits = c(0, x_limit),
      labels = scales::label_number(suffix = suffix),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = x_lab, y = NULL, title = title_str) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text          = element_text(color = "black"),
      axis.title         = element_text(color = "black"),
      plot.title         = element_text(face = "bold", color = "black")
    )
}


# ── Helper: dodged _pref vs _intense comparison ───────────────────────────

plot_tradeoff_comparison <- function(battery, file_path) {
  pref_df <- build_tradeoff_df(battery, "_pref") |>
    mutate(metric = "First choice")
  
  intense_df <- build_tradeoff_df(battery, "_intense") |>
    mutate(metric = "Intense preference")
  
  sort_order <- pref_df |> arrange(pct) |> pull(label)
  
  bind_rows(pref_df, intense_df) |>
    mutate(
      label  = factor(label, levels = sort_order),
      metric = factor(metric, levels = c("First choice", "Intense preference"))
    ) |>
    ggplot(aes(x = pct, y = label, fill = metric, pattern = metric)) +
    geom_col_pattern(
      position        = position_dodge(width = 0.7),
      width           = 0.62,
      color           = "black",
      pattern_fill    = "black",
      pattern_density = 0.05,
      pattern_spacing = 0.02
    ) +
    geom_text(
      aes(label = sprintf("%.1f%%", pct)),
      position = position_dodge(width = 0.7),
      hjust = -0.12, size = 3.3, color = "black"
    ) +
    scale_fill_manual(values    = c("First choice" = "grey30", "Intense preference" = "grey70")) +
    scale_pattern_manual(values = c("First choice" = "none",   "Intense preference" = "stripe")) +
    scale_x_continuous(
      limits = c(0, 80),
      labels = scales::label_number(suffix = "%"),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x       = "% of respondents",
      y       = NULL,
      fill    = NULL,
      pattern = NULL,
      title   = paste0(battery_titles[[battery]], " — first choice vs. intense preference"),
      caption = paste0(
        "First choice: respondent allocated the most points to this option.\n",
        "Intense preference: respondent allocated >50 of 100 points to this option."
      )
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position    = "bottom",
      panel.grid.major.y = element_blank(),
      axis.text          = element_text(color = "black"),
      axis.title         = element_text(color = "black"),
      plot.title         = element_text(face = "bold", color = "black"),
      plot.caption       = element_text(color = "black")
    )
  
  ggsave(file_path, width = 10, height = 5.5, dpi = 300)
}


# ── Loop: produce all plots for each battery ──────────────────────────────

batteries <- names(tradeoff_labels)

walk(batteries, function(bat) {
  
  # (a) Raw mean allocation
  raw_df <- build_tradeoff_df(bat, suffix = "")
  p_raw  <- plot_tradeoff_bar(
    plot_df   = raw_df,
    x_lab     = "Mean points allocated (out of 100)",
    title_str = paste0(battery_titles[[bat]], " — mean allocation"),
    fill_col  = "#4dac26",
    x_limit   = 80,
    is_pct    = FALSE,
    file_path = paste0("graphs/descriptives/tradeoff_", bat, "_mean.png")
  )
  ggsave(paste0("graphs/descriptives/tradeoff_", bat, "_mean.png"),
         plot = p_raw, width = 9, height = 4.5 + 0.4 * nrow(raw_df), dpi = 300)
  
  # (b) First choice (_pref)
  pref_df <- build_tradeoff_df(bat, "_pref")
  p_pref  <- plot_tradeoff_bar(
    plot_df   = pref_df,
    x_lab     = "% of respondents (first choice)",
    title_str = paste0(battery_titles[[bat]], " — first choice"),
    fill_col  = "#d6604d",
    x_limit   = 80,
    is_pct    = TRUE,
    file_path = paste0("graphs/descriptives/tradeoff_", bat, "_pref.png")
  )
  ggsave(paste0("graphs/descriptives/tradeoff_", bat, "_pref.png"),
         plot = p_pref, width = 9, height = 4.5 + 0.4 * nrow(pref_df), dpi = 300)
  
  # (c) Intense preference (_intense)
  intense_df <- build_tradeoff_df(bat, "_intense")
  p_intense  <- plot_tradeoff_bar(
    plot_df   = intense_df,
    x_lab     = "% of respondents (intense preference: >50 pts)",
    title_str = paste0(battery_titles[[bat]], " — intense preference"),
    fill_col  = "#762a83",
    x_limit   = 60,
    is_pct    = TRUE,
    file_path = paste0("graphs/descriptives/tradeoff_", bat, "_intense.png")
  )
  ggsave(paste0("graphs/descriptives/tradeoff_", bat, "_intense.png"),
         plot = p_intense, width = 9, height = 4.5 + 0.4 * nrow(intense_df), dpi = 300)
  
  # (d) _pref vs _intense comparison
  plot_tradeoff_comparison(
    battery   = bat,
    file_path = paste0("graphs/descriptives/tradeoff_", bat, "_pref_vs_intense.png")
  )
  
  cat("Battery", bat, "-- all 4 plots saved.\n")
})

#*************************************************************************#
#########   5C. TRADEOFF DESCRIPTIVES -- REGIONAL COMPARISON    #########
#*************************************************************************#
# For each battery: one plot faceted by region (4 panels).
# Within each panel: policies on y-axis, dodged bars for
#   "First choice" vs "Intense preference".
# Percentages are computed WITHIN region so comparisons are valid.
#
# Region variable: ses_region_cat
#   Levels: "Ontario", "Quebec", "Alberta", "East Coast"
# -------------------------------------------------------------------------

region_var    <- "ses_region_cat"
region_levels <- c("Ontario", "Quebec", "Alberta", "East Coast")


# ── Helper: build tidy regional tibble for one battery ───────────────────
# Returns one row per region x option x metric combination.

build_tradeoff_regional <- function(battery) {
  
  var_map   <- tradeoff_labels[[battery]]
  base_vars <- names(var_map)
  opt_labels <- unname(var_map)
  
  pref_vars    <- paste0(base_vars, "_pref")
  intense_vars <- paste0(base_vars, "_intense")
  
  map_dfr(region_levels, function(reg) {
    
    reg_data <- df |> filter(.data[[region_var]] == reg)
    n_reg    <- nrow(reg_data)
    
    bind_rows(
      # First choice
      tibble(
        region  = reg,
        label   = opt_labels,
        var     = pref_vars,
        metric  = "First choice"
      ),
      # Intense preference
      tibble(
        region  = reg,
        label   = opt_labels,
        var     = intense_vars,
        metric  = "Intense preference"
      )
    ) |>
      mutate(
        pct = map_dbl(var, ~ {
          col <- reg_data[[.x]]
          if (is.null(col) || all(is.na(col))) NA_real_
          else mean(col, na.rm = TRUE) * 100   # binary cols: 0/1 -> %
        }),
        region = factor(region, levels = region_levels),
        metric = factor(metric, levels = c("First choice", "Intense preference"))
      )
  })
}


# ── Helper: produce one regional comparison plot for a battery ────────────

plot_tradeoff_regional <- function(battery, file_path) {
  
  plot_df <- build_tradeoff_regional(battery)
  
  # Sort order: average _pref pct across all regions
  sort_order <- plot_df |>
    filter(metric == "First choice") |>
    group_by(label) |>
    summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop") |>
    arrange(mean_pct) |>
    pull(label)
  
  plot_df <- plot_df |>
    mutate(label = factor(label, levels = sort_order))
  
  ggplot(plot_df, aes(x = pct, y = label, fill = metric, pattern = metric)) +
    geom_col_pattern(
      position        = position_dodge(width = 0.7),
      width           = 0.62,
      color           = "black",
      pattern_fill    = "black",
      pattern_density = 0.05,
      pattern_spacing = 0.02
    ) +
    geom_text(
      aes(label = sprintf("%.1f%%", pct)),
      position = position_dodge(width = 0.7),
      hjust = -0.1, size = 2.8, color = "black"
    ) +
    scale_fill_manual(
      values = c("First choice" = "grey30", "Intense preference" = "grey70")
    ) +
    scale_pattern_manual(
      values = c("First choice" = "none", "Intense preference" = "stripe")
    ) +
    scale_x_continuous(
      limits = c(0, 85),
      labels = scales::label_number(suffix = "%"),
      expand = expansion(mult = c(0, 0.05))
    ) +
    facet_wrap(~ region, ncol = 2) +
    labs(
      x       = "% of respondents within region",
      y       = NULL,
      fill    = NULL,
      pattern = NULL,
      title   = paste0(battery_titles[[battery]], " — by region"),
      caption = paste0(
        "First choice: respondent allocated the most points to this option.\n",
        "Intense preference: respondent allocated >50 of 100 points to this option.\n",
        "Percentages computed within each region."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position    = "bottom",
      panel.grid.major.y = element_blank(),
      strip.text         = element_text(face = "bold", color = "black"),
      axis.text          = element_text(color = "black"),
      axis.title         = element_text(color = "black"),
      plot.title         = element_text(face = "bold", color = "black"),
      plot.caption       = element_text(color = "grey30")
    )
  
  # Height scales with number of options per battery
  n_opts <- length(tradeoff_labels[[battery]])
  ggsave(file_path, width = 13, height = 5 + n_opts * 0.6, dpi = 300)
}


# ── Loop: produce regional plot for every battery ─────────────────────────

walk(batteries, function(bat) {
  plot_tradeoff_regional(
    battery   = bat,
    file_path = paste0("graphs/descriptives/tradeoff_", bat, "_regional.png")
  )
  cat("Battery", bat, "-- regional plot saved.\n")
})

#*************************************************************************#
#########   6. BIVARIATE MARGINAL MEANS (ALL IVs x ALL DVs)     #########
#*************************************************************************#

all_reg_df <- bind_rows(
  imp_bin_df  |> mutate(battery="imp_bin"),
  prio_num_df |> mutate(battery="prio_num")
)

marginal_results <- map_dfr(ivs, function(iv) {
  map2_dfr(all_reg_df$var, all_reg_df$label, interpret_contrast, iv=iv, data=df)
})

write.csv(marginal_results, "graphs/regressions/marginal_means_all.csv", row.names=FALSE)


#*************************************************************************#
#########   7. REGRESSIONS -- IMPORTANCE BINARY (LPM + LOGIT)   #########
#*************************************************************************#

# 7.1. Fit models
lpm_imp <- imp_bin_df$var |> set_names(imp_bin_df$label) |>
  map(~lm(as.formula(paste(.x,"~",rhs)), data=df))

logit_imp <- imp_bin_df$var |> set_names(imp_bin_df$label) |>
  map(~glm(as.formula(paste(.x,"~",rhs)), data=df, family=binomial(link="logit")))

# 7.2. Average marginal effects (robust SEs)
coef_lpm_imp <- map2_dfr(lpm_imp, imp_bin_df$label, tidy_avg_slopes, question="imp")

coef_logit_imp <- map2_dfr(logit_imp, imp_bin_df$label, function(model, dv_label) {
  fml        <- formula(model)
  model_vars <- all.vars(fml)
  model_data <- df |> select(all_of(model_vars)) |> drop_na()
  vm         <- robust_vcov(model)
  avg_slopes(model, vcov=vm, newdata=model_data) |> as_tibble() |>
    transmute(question="imp", dv=dv_label, term,
              estimate=round(estimate,3), conf.low=round(conf.low,3),
              conf.high=round(conf.high,3), p.value=round(p.value,3),
              sig=case_when(p.value<0.001~"***",p.value<0.01~"**",p.value<0.05~"*",p.value<0.10~".",TRUE~""),
              direction=case_when(conf.low>0~"Positive",conf.high<0~"Negative",TRUE~"No clear effect"))
})

# 7.3. Model fit
fit_lpm_imp <- map2_dfr(lpm_imp, imp_bin_df$label, function(model, dv_label) {
  s <- summary.lm(model)
  tibble(dv=dv_label, model="LPM", r_squared=round(s$r.squared,3),
         adj_r_sq=round(s$adj.r.squared,3), n=length(s$residuals))
})

fit_logit_imp <- map2_dfr(logit_imp, imp_bin_df$label, function(model, dv_label) {
  s <- summary.glm(model)
  tibble(dv=dv_label, model="Logit",
         pseudo_r2=round(1-(s$deviance/s$null.deviance),3), n=length(s$residuals))
})

# 7.4. Regression tables
modelsummary(lpm_imp, estimate="{estimate}{stars}", statistic="({std.error})",
             vcov=map(lpm_imp, robust_vcov), coef_map=term_labels,
             gof_map=c("nobs","r.squared","adj.r.squared"),
             output="graphs/regressions/regtable_imp_LPM.txt",
             notes="LPM (OLS). HC1 robust SEs in parentheses. * p<0.05, ** p<0.01, *** p<0.001")

modelsummary(logit_imp, estimate="{estimate}{stars}", statistic="({std.error})",
             vcov=map(logit_imp, robust_vcov), coef_map=term_labels,
             gof_map=c("nobs","logLik","AIC"),
             output="graphs/regressions/regtable_imp_logit_coefs.txt",
             notes="Logit coefficients (log-odds). HC1 robust SEs. Interpret via AME table.")

modelsummary(map(logit_imp,~avg_slopes(.x,vcov=robust_vcov(.x))),
             estimate="{estimate}{stars}", statistic="({std.error})",
             coef_map=term_labels, gof_map=c("nobs"),
             output="graphs/regressions/regtable_imp_logit_AME.txt",
             notes="Average marginal effects from logit. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001")

# 7.5. Coefficient plots
plot_coefs(coef_lpm_imp,   "Importance (binary) -- LPM coefficients",              "graphs/regressions/coef_imp_LPM.png")
plot_coefs(coef_logit_imp, "Importance (binary) -- Logit average marginal effects", "graphs/regressions/coef_imp_logit_AME.png")

# 7.6. LPM vs Logit AME robustness comparison
bind_rows(coef_lpm_imp|>mutate(model="LPM"), coef_logit_imp|>mutate(model="Logit AME")) |>
  mutate(term=recode(term,!!!term_labels)) |>
  ggplot(aes(x=estimate,y=reorder(term,estimate),color=model,shape=model)) +
  geom_vline(xintercept=0,linetype="dashed",color="grey50") +
  geom_point(size=2.2, position=position_dodge(width=0.5)) +
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high),height=0.2,position=position_dodge(width=0.5)) +
  scale_color_manual(values=c("LPM"="#2166ac","Logit AME"="#d6604d")) +
  facet_wrap(~dv) +
  labs(x="Estimated effect",y=NULL,color=NULL,shape=NULL,
       title="Importance battery -- LPM vs. Logit AME (robustness check)") +
  theme_minimal(base_size=11) +
  theme(legend.position="bottom",strip.text=element_text(face="bold"))
ggsave("graphs/regressions/coef_imp_LPM_vs_logit.png", width=14, height=9, dpi=300)

# 7.7. R-squared summaries
plot_r2(fit_lpm_imp,   r2_col="adj_r_sq",  title_str="Model fit -- Importance battery (LPM)",            file_path="graphs/regressions/r2_imp_LPM.png")
plot_r2(fit_logit_imp, r2_col="pseudo_r2", title_str="Model fit -- Importance battery (Logit pseudo-R2)", file_path="graphs/regressions/r2_imp_logit.png")


#*************************************************************************#
#########   8. REGRESSIONS -- PRIORITY RAW SCORE (OLS)          #########
#*************************************************************************#
# Note: budget_prio_X sums to 100 per respondent (compositional data).
# OLS is appropriate for exploration; consider Dirichlet regression for publication.

# 8.1. Fit models
lm_prio <- prio_num_df$var |> set_names(prio_num_df$label) |>
  map(~lm(as.formula(paste(.x,"~",rhs)), data=df))

# 8.2. Average marginal effects (robust SEs)
coef_prio <- map2_dfr(lm_prio, prio_num_df$label, tidy_avg_slopes, question="prio")

# 8.3. Model fit
fit_prio <- map2_dfr(lm_prio, prio_num_df$label, function(model, dv_label) {
  s <- summary(model)
  tibble(dv=dv_label, r_squared=round(s$r.squared,3),
         adj_r_sq=round(s$adj.r.squared,3), n=length(model$residuals))
})

# 8.4. Regression table
modelsummary(lm_prio, estimate="{estimate}{stars}", statistic="({std.error})",
             vcov=map(lm_prio, robust_vcov), coef_map=term_labels,
             gof_map=c("nobs","r.squared","adj.r.squared"),
             output="graphs/regressions/regtable_prio_OLS.txt",
             notes="OLS. HC1 robust SEs. DV = budget points allocated (0-100). * p<0.05, ** p<0.01, *** p<0.001")

# 8.5. Plots
plot_coefs(coef_prio, "Budget priority allocation (0-100 pts) -- OLS avg marginal effects", "graphs/regressions/coef_prio_OLS.png")
plot_r2(fit_prio, r2_col="adj_r_sq", title_str="Model fit -- Priority allocation (OLS)", file_path="graphs/regressions/r2_prio_OLS.png")


#*************************************************************************#
#########   9. MARGINAL EFFECTS PLOTS -- IDEOLOGY x QUEBEC       #########
#*************************************************************************#

walk2(
  c(imp_bin_df$var, prio_num_df$var),
  c(imp_bin_df$label, prio_num_df$label),
  function(dv, dv_label) {
    model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> select(all_of(model_vars)) |> drop_na()
    model      <- lm(as.formula(paste(dv,"~",rhs)), data=model_data)
    me         <- ggpredict(model, terms=c("ideo_right_num [all]","quebec_bin [0,1]"))
    
    ggplot(me, aes(x=x,y=predicted,colour=as.factor(group),fill=as.factor(group))) +
      geom_line(linewidth=1) +
      geom_ribbon(aes(ymin=conf.low,ymax=conf.high), alpha=0.15, colour=NA) +
      scale_colour_manual(values=c("0"="#d6604d","1"="#2166ac"),
                          labels=c("0"="Rest of Canada","1"="Quebec")) +
      scale_fill_manual(  values=c("0"="#d6604d","1"="#2166ac"),
                          labels=c("0"="Rest of Canada","1"="Quebec")) +
      labs(x="Ideology (Left to Right)", y="Predicted value",
           colour=NULL, fill=NULL,
           title=paste("Ideology x Quebec --", dv_label),
           caption="OLS predictions. HC1 robust SEs. Shaded area = 95% CI.") +
      theme_minimal(base_size=12) + theme(legend.position="bottom")
    
    ggsave(paste0("graphs/regressions/me_ideo_",dv,".png"), width=8, height=5, dpi=300)
  }
)


#*************************************************************************#
#########   10. QUICK DIAGNOSTICS                                #########
#*************************************************************************#

cat("\n========== IMPORTANCE: LPM fit ==========\n");         print(fit_lpm_imp)
cat("\n========== IMPORTANCE: Logit pseudo-R2 ==========\n"); print(fit_logit_imp)
cat("\n========== PRIORITY: OLS fit ==========\n");           print(fit_prio)

cat("\n========== Most significant predictors -- imp LPM ==========\n")
coef_lpm_imp |> filter(sig %in% c("*","**","***")) |>
  count(term, sort=TRUE) |> mutate(term=recode(term,!!!term_labels)) |> print(n=20)

cat("\n========== Most significant predictors -- prio OLS ==========\n")
coef_prio |> filter(sig %in% c("*","**","***")) |>
  count(term, sort=TRUE) |> mutate(term=recode(term,!!!term_labels)) |> print(n=20)

cat("\n========== LPM vs Logit direction agreement check ==========\n")
# Any row here = predictor where LPM and Logit disagree on direction
# among statistically significant results. Investigate if any appear.
bind_rows(coef_lpm_imp|>mutate(model="LPM"), coef_logit_imp|>mutate(model="Logit")) |>
  filter(sig %in% c("*","**","***")) |>
  select(dv, term, model, direction) |>
  pivot_wider(names_from=model, values_from=direction) |>
  filter(LPM != Logit) |>
  print()

# ============================================================
# TRADEOFF REGRESSIONS 
# Mirrors the structure of Sections 6-10 for budget DVs.
#
# Three DV types per battery:
#   _pref    = binary (first choice)       → LPM + logit
#   _intense = binary (>50 pts allocated)  → LPM + logit
#   raw      = numeric 0-100 allocation    → OLS
# ============================================================


#*************************************************************************#
#########   T1. DEFINE TRADEOFF DV LISTS BY BATTERY              #########
#*************************************************************************#

batteries <- list(
  cc1 = list(
    title   = "Increase childcare spending, how to fund it",
    raw     = c("tradeoff_cc1_tax", "tradeoff_cc1_cut",
                "tradeoff_cc1_debt", "tradeoff_cc1_no_spend"),
    pref    = c("tradeoff_cc1_tax_pref", "tradeoff_cc1_cut_pref",
                "tradeoff_cc1_debt_pref", "tradeoff_cc1_no_spend_pref"),
    intense = c("tradeoff_cc1_tax_intense", "tradeoff_cc1_cut_intense",
                "tradeoff_cc1_debt_intense", "tradeoff_cc1_no_spend_intense"),
    labels  = c(
      tradeoff_cc1_tax        = "Raise taxes",
      tradeoff_cc1_cut        = "Cut other spending",
      tradeoff_cc1_debt       = "Increase debt",
      tradeoff_cc1_no_spend   = "Don't spend more",
      tradeoff_cc1_tax_pref   = "Raise taxes",
      tradeoff_cc1_cut_pref   = "Cut other spending",
      tradeoff_cc1_debt_pref  = "Increase debt",
      tradeoff_cc1_no_spend_pref    = "Don't spend more",
      tradeoff_cc1_tax_intense      = "Raise taxes",
      tradeoff_cc1_cut_intense      = "Cut other spending",
      tradeoff_cc1_debt_intense     = "Increase debt",
      tradeoff_cc1_no_spend_intense = "Don't spend more"
    )
  ),
  ge = list(
    title   = "Increase green economy spending, how to fund it",
    raw     = c("tradeoff_ge_tax", "tradeoff_ge_cut",
                "tradeoff_ge_debt", "tradeoff_ge_no_spend"),
    pref    = c("tradeoff_ge_tax_pref", "tradeoff_ge_cut_pref",
                "tradeoff_ge_debt_pref", "tradeoff_ge_no_spend_pref"),
    intense = c("tradeoff_ge_tax_intense", "tradeoff_ge_cut_intense",
                "tradeoff_ge_debt_intense", "tradeoff_ge_no_spend_intense"),
    labels  = c(
      tradeoff_ge_tax         = "Raise taxes",
      tradeoff_ge_cut         = "Cut other spending",
      tradeoff_ge_debt        = "Increase debt",
      tradeoff_ge_no_spend    = "Don't spend more",
      tradeoff_ge_tax_pref    = "Raise taxes",
      tradeoff_ge_cut_pref    = "Cut other spending",
      tradeoff_ge_debt_pref   = "Increase debt",
      tradeoff_ge_no_spend_pref    = "Don't spend more",
      tradeoff_ge_tax_intense      = "Raise taxes",
      tradeoff_ge_cut_intense      = "Cut other spending",
      tradeoff_ge_debt_intense     = "Increase debt",
      tradeoff_ge_no_spend_intense = "Don't spend more"
    )
  ),
  tax = list(
    title   = "Raise revenue, tax preferences",
    raw     = c("tradeoff_tax_less_services", "tradeoff_tax_sales_tax",
                "tradeoff_tax_inc_tax", "tradeoff_tax_wealth_tax"),
    pref    = c("tradeoff_tax_less_services_pref", "tradeoff_tax_sales_tax_pref",
                "tradeoff_tax_inc_tax_pref", "tradeoff_tax_wealth_tax_pref"),
    intense = c("tradeoff_tax_less_services_intense", "tradeoff_tax_sales_tax_intense",
                "tradeoff_tax_inc_tax_intense", "tradeoff_tax_wealth_tax_intense"),
    labels  = c(
      tradeoff_tax_less_services  = "No increase, even if fewer services",
      tradeoff_tax_sales_tax      = "Sales tax increase",
      tradeoff_tax_inc_tax        = "Income tax increase",
      tradeoff_tax_wealth_tax     = "Wealth tax",
      tradeoff_tax_less_services_pref    = "No increase, even if fewer services",
      tradeoff_tax_sales_tax_pref        = "Sales tax increase",
      tradeoff_tax_inc_tax_pref          = "Income tax increase",
      tradeoff_tax_wealth_tax_pref       = "Wealth tax",
      tradeoff_tax_less_services_intense = "No increase, even if fewer services",
      tradeoff_tax_sales_tax_intense     = "Sales tax increase",
      tradeoff_tax_inc_tax_intense       = "Income tax increase",
      tradeoff_tax_wealth_tax_intense    = "Wealth tax"
    )
  ),
  hc = list(
    title   = "Home care policy priorities, tradeoffs",
    raw     = c("tradeoff_hc_all", "tradeoff_hc_spend", "tradeoff_hc_pensions"),
    pref    = c("tradeoff_hc_all_pref", "tradeoff_hc_spend_pref",
                "tradeoff_hc_pensions_pref"),
    intense = c("tradeoff_hc_all_intense", "tradeoff_hc_spend_intense",
                "tradeoff_hc_pensions_intense"),
    labels  = c(
      tradeoff_hc_all              = "Increase home care for all, reduce max old-age pension",
      tradeoff_hc_spend            = "Target low-income seniors, home care",
      tradeoff_hc_pensions         = "Target low-income seniors, pensions",
      tradeoff_hc_all_pref         = "Increase home care for all, reduce max old-age pension",
      tradeoff_hc_spend_pref       = "Target low-income seniors, home care",
      tradeoff_hc_pensions_pref    = "Target low-income seniors, pensions",
      tradeoff_hc_all_intense      = "Increase home care for all, reduce max old-age pension",
      tradeoff_hc_spend_intense    = "Target low-income seniors, home care",
      tradeoff_hc_pensions_intense = "Target low-income seniors, pensions"
    )
  ),
  cc2 = list(
    title   = "Childcare policy priorities, tradeoffs",
    raw     = c("tradeoff_cc2_all", "tradeoff_cc2_low_inc",
                "tradeoff_cc2_educ_all", "tradeoff_cc2_educ_low_inc"),
    pref    = c("tradeoff_cc2_all_pref", "tradeoff_cc2_low_inc_pref",
                "tradeoff_cc2_educ_all_pref", "tradeoff_cc2_educ_low_inc_pref"),
    intense = c("tradeoff_cc2_all_intense", "tradeoff_cc2_low_inc_intense",
                "tradeoff_cc2_educ_all_intense", "tradeoff_cc2_educ_low_inc_intense"),
    labels  = c(
      tradeoff_cc2_all               = "Increase child care for all, reduce other family benefits",
      tradeoff_cc2_low_inc           = "Target low-income families, childcare",
      tradeoff_cc2_educ_all          = "Target education quality for all, reduce other family benefits",
      tradeoff_cc2_educ_low_inc      = "Target education quality for low-income",
      tradeoff_cc2_all_pref          = "Increase child care for all, reduce other family benefits",
      tradeoff_cc2_low_inc_pref      = "Target low-income families, childcare",
      tradeoff_cc2_educ_all_pref     = "Target education quality for all, reduce other family benefits",
      tradeoff_cc2_educ_low_inc_pref = "Target education quality for low-income",
      tradeoff_cc2_all_intense          = "Increase child care for all, reduce other family benefits",
      tradeoff_cc2_low_inc_intense      = "Target low-income families, childcare",
      tradeoff_cc2_educ_all_intense     = "Target education quality for all, reduce other family benefits",
      tradeoff_cc2_educ_low_inc_intense = "Target education quality for low-income"
    )
  )
)


#*************************************************************************#
#########   T2. FIT MODELS FOR EACH BATTERY                      #########
#*************************************************************************#
# For each battery, fit:
#   lm_tradeoff_raw    — OLS for numeric 0-100 allocation
#   lpm_tradeoff_pref  — LPM for binary _pref
#   logit_tradeoff_pref
#   lpm_tradeoff_intense
#   logit_tradeoff_intense

tradeoff_models <- map(batteries, function(bat) {
  
  # — OLS: raw allocation —
  lm_raw <- bat$raw |>
    set_names(bat$labels[bat$raw]) |>
    map(function(dv) {
      model_data <- df |> dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |> drop_na()
      lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    })
  
  # — LPM: _pref —
  lpm_pref <- bat$pref |>
    set_names(bat$labels[bat$pref]) |>
    map(function(dv) {
      model_data <- df |> dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |> drop_na()
      lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    })
  
  # — Logit: _pref —
  logit_pref <- bat$pref |>
    set_names(bat$labels[bat$pref]) |>
    map(function(dv) {
      model_data <- df |> dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |> drop_na()
      glm(as.formula(paste(dv, "~", rhs)), data = model_data, family = binomial(link = "logit"))
    })
  
  # — LPM: _intense —
  lpm_intense <- bat$intense |>
    set_names(bat$labels[bat$intense]) |>
    map(function(dv) {
      model_data <- df |> dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |> drop_na()
      lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    })
  
  # — Logit: _intense —
  logit_intense <- bat$intense |>
    set_names(bat$labels[bat$intense]) |>
    map(function(dv) {
      model_data <- df |> dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |> drop_na()
      glm(as.formula(paste(dv, "~", rhs)), data = model_data, family = binomial(link = "logit"))
    })
  
  list(lm_raw = lm_raw, lpm_pref = lpm_pref, logit_pref = logit_pref,
       lpm_intense = lpm_intense, logit_intense = logit_intense)
})


#*************************************************************************#
#########   T3. AVERAGE MARGINAL EFFECTS                         #########
#*************************************************************************#

# Helper for inline logit AMEs (mirrors Section 7 of budgetary script)
ame_logit_tradeoff <- function(model_list, question_tag) {
  map2_dfr(model_list, names(model_list), function(model, dv_label) {
    fml        <- formula(model)
    model_vars <- all.vars(fml)
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    vm         <- robust_vcov(model)
    avg_slopes(model, vcov = vm, newdata = model_data) |>
      as_tibble() |>
      transmute(
        question  = question_tag,
        dv        = dv_label,
        term,
        estimate  = round(estimate, 3),
        conf.low  = round(conf.low, 3),
        conf.high = round(conf.high, 3),
        p.value   = round(p.value, 3),
        sig = case_when(
          p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
          p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".", TRUE ~ ""
        ),
        direction = case_when(
          conf.low  > 0 ~ "Positive",
          conf.high < 0 ~ "Negative",
          TRUE          ~ "No clear effect"
        )
      )
  })
}

# Collect AMEs for all batteries and DV types
coef_tradeoff <- map_dfr(names(batteries), function(bat_name) {
  bat    <- batteries[[bat_name]]
  models <- tradeoff_models[[bat_name]]
  
  # OLS AMEs via tidy_avg_slopes (same helper as budget regressions)
  ame_raw <- map2_dfr(models$lm_raw, names(models$lm_raw),
                      tidy_avg_slopes, question = paste0(bat_name, "_raw"))
  
  # LPM AMEs
  ame_lpm_pref <- map2_dfr(models$lpm_pref, names(models$lpm_pref),
                           tidy_avg_slopes, question = paste0(bat_name, "_pref"))
  ame_lpm_intense <- map2_dfr(models$lpm_intense, names(models$lpm_intense),
                              tidy_avg_slopes, question = paste0(bat_name, "_intense"))
  
  # Logit AMEs
  ame_logit_pref    <- ame_logit_tradeoff(models$logit_pref,    paste0(bat_name, "_pref_logit"))
  ame_logit_intense <- ame_logit_tradeoff(models$logit_intense, paste0(bat_name, "_intense_logit"))
  
  bind_rows(ame_raw, ame_lpm_pref, ame_lpm_intense, ame_logit_pref, ame_logit_intense) |>
    mutate(battery = bat_name, battery_title = bat$title)
})

write.csv(coef_tradeoff,
          "graphs/regressions/AME_tradeoff_all.csv",
          row.names = FALSE)


#*************************************************************************#
#########   T4. MODEL FIT                                        #########
#*************************************************************************#

fit_tradeoff <- map_dfr(names(batteries), function(bat_name) {
  models <- tradeoff_models[[bat_name]]
  
  # OLS fit
  fit_raw <- map2_dfr(models$lm_raw, names(models$lm_raw), function(model, dv_label) {
    s <- summary(model)
    tibble(battery = bat_name, dv = dv_label, model_type = "OLS",
           r_squared  = round(s$r.squared, 3),
           adj_r_sq   = round(s$adj.r.squared, 3),
           n          = length(s$residuals))
  })
  
  # LPM fit — _pref
  fit_lpm_pref <- map2_dfr(models$lpm_pref, names(models$lpm_pref), function(model, dv_label) {
    s <- summary(model)
    tibble(battery = bat_name, dv = dv_label, model_type = "LPM_pref",
           r_squared  = round(s$r.squared, 3),
           adj_r_sq   = round(s$adj.r.squared, 3),
           n          = length(s$residuals))
  })
  
  # LPM fit — _intense
  fit_lpm_intense <- map2_dfr(models$lpm_intense, names(models$lpm_intense), function(model, dv_label) {
    s <- summary(model)
    tibble(battery = bat_name, dv = dv_label, model_type = "LPM_intense",
           r_squared  = round(s$r.squared, 3),
           adj_r_sq   = round(s$adj.r.squared, 3),
           n          = length(s$residuals))
  })
  
  # Logit fit — _pref
  fit_logit_pref <- map2_dfr(models$logit_pref, names(models$logit_pref), function(model, dv_label) {
    s <- summary.glm(model)
    tibble(battery = bat_name, dv = dv_label, model_type = "Logit_pref",
           pseudo_r2 = round(1 - (s$deviance / s$null.deviance), 3),
           n         = length(s$residuals))
  })
  
  # Logit fit — _intense
  fit_logit_intense <- map2_dfr(models$logit_intense, names(models$logit_intense), function(model, dv_label) {
    s <- summary.glm(model)
    tibble(battery = bat_name, dv = dv_label, model_type = "Logit_intense",
           pseudo_r2 = round(1 - (s$deviance / s$null.deviance), 3),
           n         = length(s$residuals))
  })
  
  bind_rows(fit_raw, fit_lpm_pref, fit_lpm_intense, fit_logit_pref, fit_logit_intense)
})

print(fit_tradeoff)
write.csv(fit_tradeoff,
          "graphs/regressions/fit_tradeoff_all.csv",
          row.names = FALSE)


#*************************************************************************#
#########   T5. REGRESSION TABLES                                #########
#*************************************************************************#
# One .txt table per battery × DV type (LPM coefficients)

walk(names(batteries), function(bat_name) {
  models <- tradeoff_models[[bat_name]]
  bat    <- batteries[[bat_name]]
  
  # LPM — _pref
  modelsummary(
    models$lpm_pref,
    estimate  = "{estimate}{stars}",
    statistic = "({std.error})",
    coef_map  = term_labels,
    gof_map   = c("nobs", "r.squared"),
    output    = paste0("graphs/regressions/regtable_tradeoff_", bat_name, "_pref_lpm.txt"),
    notes     = paste0(bat$title, " — LPM. HC1-robust SEs in parentheses.",
                       " * p<0.05, ** p<0.01, *** p<0.001")
  )
  
  # LPM — _intense
  modelsummary(
    models$lpm_intense,
    estimate  = "{estimate}{stars}",
    statistic = "({std.error})",
    coef_map  = term_labels,
    gof_map   = c("nobs", "r.squared"),
    output    = paste0("graphs/regressions/regtable_tradeoff_", bat_name, "_intense_lpm.txt"),
    notes     = paste0(bat$title, " — LPM. HC1-robust SEs in parentheses.",
                       " * p<0.05, ** p<0.01, *** p<0.001")
  )
  
  # OLS — raw allocation
  modelsummary(
    models$lm_raw,
    estimate  = "{estimate}{stars}",
    statistic = "({std.error})",
    coef_map  = term_labels,
    gof_map   = c("nobs", "r.squared"),
    output    = paste0("graphs/regressions/regtable_tradeoff_", bat_name, "_raw_ols.txt"),
    notes     = paste0(bat$title, " — OLS (0-100 allocation). HC1-robust SEs in parentheses.",
                       " * p<0.05, ** p<0.01, *** p<0.001")
  )
})


#*************************************************************************#
#########   T6. COEFFICIENT PLOTS                                #########
#*************************************************************************#

walk(names(batteries), function(bat_name) {
  bat <- batteries[[bat_name]]
  
  # — _pref LPM AMEs —
  coef_pref <- coef_tradeoff |>
    dplyr::filter(battery == bat_name, question == paste0(bat_name, "_pref"))
  
  if (nrow(coef_pref) > 0) {
    coef_pref |>
      mutate(term = recode(term, !!!term_labels)) |>
      ggplot(aes(x = estimate, y = reorder(term, estimate), color = direction)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 2.5) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      scale_color_manual(values = c(
        "Positive"        = "#2166ac",
        "Negative"        = "#d6604d",
        "No clear effect" = "grey60"
      )) +
      facet_wrap(~ dv, ncol = 2, scales = "free_x") +
      labs(x = "Coefficient (HC1 robust SEs)", y = NULL, color = NULL,
           title = paste0(bat$title, " — LPM (first choice)"),
           caption = "LPM. HC1 robust SEs. 95% CI.") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
    ggsave(paste0("graphs/regressions/coef_tradeoff_", bat_name, "_pref_lpm.png"),
           width = 14, height = 10, dpi = 300)
  }
  
  # — _intense LPM AMEs —
  coef_int <- coef_tradeoff |>
    dplyr::filter(battery == bat_name, question == paste0(bat_name, "_intense"))
  
  if (nrow(coef_int) > 0) {
    coef_int |>
      mutate(term = recode(term, !!!term_labels)) |>
      ggplot(aes(x = estimate, y = reorder(term, estimate), color = direction)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 2.5) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      scale_color_manual(values = c(
        "Positive"        = "#2166ac",
        "Negative"        = "#d6604d",
        "No clear effect" = "grey60"
      )) +
      facet_wrap(~ dv, ncol = 2, scales = "free_x") +
      labs(x = "Coefficient (HC1 robust SEs)", y = NULL, color = NULL,
           title = paste0(bat$title, " — LPM (intense preference)"),
           caption = "LPM. HC1 robust SEs. 95% CI.") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
    ggsave(paste0("graphs/regressions/coef_tradeoff_", bat_name, "_intense_lpm.png"),
           width = 14, height = 10, dpi = 300)
  }
  
  # — raw OLS AMEs —
  coef_raw <- coef_tradeoff |>
    dplyr::filter(battery == bat_name, question == paste0(bat_name, "_raw"))
  
  if (nrow(coef_raw) > 0) {
    coef_raw |>
      mutate(term = recode(term, !!!term_labels)) |>
      ggplot(aes(x = estimate, y = reorder(term, estimate), color = direction)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 2.5) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      scale_color_manual(values = c(
        "Positive"        = "#2166ac",
        "Negative"        = "#d6604d",
        "No clear effect" = "grey60"
      )) +
      facet_wrap(~ dv, ncol = 2, scales = "free_x") +
      labs(x = "Coefficient (HC1 robust SEs)", y = NULL, color = NULL,
           title = paste0(bat$title, " — OLS (0-100 allocation)"),
           caption = "OLS. HC1 robust SEs. 95% CI.") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
    ggsave(paste0("graphs/regressions/coef_tradeoff_", bat_name, "_raw_ols.png"),
           width = 14, height = 10, dpi = 300)
  }
})


#*************************************************************************#
#########   T7. R-SQUARED SUMMARY PLOTS                          #########
#*************************************************************************#

walk(names(batteries), function(bat_name) {
  bat <- batteries[[bat_name]]
  
  # OLS R²
  fit_tradeoff |>
    dplyr::filter(battery == bat_name, model_type == "OLS") |>
    plot_r2(
      title_str = paste0(bat$title, " — OLS R² (raw allocation)"),
      file_path = paste0("graphs/regressions/r2_tradeoff_", bat_name, "_raw_ols.png")
    )
  
  # LPM _pref R²
  fit_tradeoff |>
    dplyr::filter(battery == bat_name, model_type == "LPM_pref") |>
    plot_r2(
      title_str = paste0(bat$title, " — LPM R² (first choice)"),
      file_path = paste0("graphs/regressions/r2_tradeoff_", bat_name, "_pref_lpm.png")
    )
  
  # LPM _intense R²
  fit_tradeoff |>
    dplyr::filter(battery == bat_name, model_type == "LPM_intense") |>
    plot_r2(
      title_str = paste0(bat$title, " — LPM R² (intense preference)"),
      file_path = paste0("graphs/regressions/r2_tradeoff_", bat_name, "_intense_lpm.png")
    )
})


#*************************************************************************#
#########   T8. DIAGNOSTICS                                      #########
#*************************************************************************#

cat("\n========== TRADEOFF MODEL FIT SUMMARY ==========\n")
print(fit_tradeoff, n = 50)

cat("\n========== Most consistently significant predictors (LPM _pref) ==========\n")
coef_tradeoff |>
  dplyr::filter(grepl("_pref$", question), sig %in% c("*","**","***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

cat("\n========== AMEs saved to: graphs/regressions/AME_tradeoff_all.csv ==========\n")
cat("Model fit saved to: graphs/regressions/fit_tradeoff_all.csv\n")
