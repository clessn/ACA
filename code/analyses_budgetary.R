# ==============
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
table(df$children_bin)

# 0.3. Output folders
# dir.create("graphs/descriptives", recursive = TRUE, showWarnings = FALSE)
# dir.create("graphs/regressions",  recursive = TRUE, showWarnings = FALSE)


#*************************************************************************#
#########              1. VARIABLE CONSTRUCTION                   #########
#*************************************************************************#

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
#########         1B. COMPOSITE VARIABLE CONSTRUCTION            #########
#*************************************************************************#

# ── Budget IMPORTANCE composites ─────────────────────────────────────────
df$expansion_num <- rowMeans(df[, c("budget_imp_health_num", "budget_imp_edu_num",
                                    "budget_imp_pensions_num")], na.rm = TRUE)

df$expansion_bin <- as.integer(rowMeans(df[, c("budget_imp_health_bin", "budget_imp_edu_bin",
                                               "budget_imp_pensions_bin")], na.rm = TRUE) >= 0.5)

df$reduc_num <- rowMeans(df[, c("budget_imp_taxes_num", "budget_imp_debt_num")], na.rm = TRUE)

df$reduc_bin <- as.integer(rowMeans(df[, c("budget_imp_taxes_bin", "budget_imp_debt_bin")],
                                    na.rm = TRUE) >= 0.5)

# ── Budget PRIORITY composites ────────────────────────────────────────────
df$soc_pol_num <- rowMeans(df[, c("budget_prio_health", "budget_prio_seniors",
                                  "budget_prio_cc")], na.rm = TRUE)

df$other_num <- rowMeans(df[, c("budget_prio_ecn", "budget_prio_clim")], na.rm = TRUE)

df$soc_pol_pref <- as.integer(
  df$budget_prio_health_pref == 1 |
    df$budget_prio_seniors_pref == 1 |
    df$budget_prio_cc_pref == 1
)

df$other_pref <- as.integer(
  df$budget_prio_ecn_pref == 1 |
    df$budget_prio_clim_pref == 1
)

df$soc_pol_intense <- as.integer(
  df$budget_prio_health_intense == 1 |
    df$budget_prio_seniors_intense == 1 |
    df$budget_prio_cc_intense == 1
)

df$other_intense <- as.integer(
  df$budget_prio_ecn_intense == 1 |
    df$budget_prio_clim_intense == 1
)

# ── Tradeoff HC composites ────────────────────────────────────────────────
df$hc_univ_bin     <- df$tradeoff_hc_all_pref
df$hc_univ_intense <- df$tradeoff_hc_all_intense

df$hc_target_bin <- as.integer(
  df$tradeoff_hc_spend_pref == 1 |
    df$tradeoff_hc_pensions_pref == 1
)

df$hc_target_intense <- as.integer(
  df$tradeoff_hc_spend_intense == 1 |
    df$tradeoff_hc_pensions_intense == 1
)

# ── Tradeoff CC2 composites ───────────────────────────────────────────────
df$cc_univ_bin <- as.integer(
  df$tradeoff_cc2_all_pref == 1 |
    df$tradeoff_cc2_educ_all_pref == 1
)

df$cc_univ_intense <- as.integer(
  df$tradeoff_cc2_all_intense == 1 |
    df$tradeoff_cc2_educ_all_intense == 1
)

df$cc_target_bin <- as.integer(
  df$tradeoff_cc2_low_inc_pref == 1 |
    df$tradeoff_cc2_educ_low_inc_pref == 1
)

df$cc_target_intense <- as.integer(
  df$tradeoff_cc2_low_inc_intense == 1 |
    df$tradeoff_cc2_educ_low_inc_intense == 1
)

# ── Sanity checks ─────────────────────────────────────────────────────────
cat("--- expansion_bin ---\n");    print(table(df$expansion_bin,    useNA = "always"))
cat("--- reduc_bin ---\n");        print(table(df$reduc_bin,        useNA = "always"))
cat("--- soc_pol_pref ---\n");     print(table(df$soc_pol_pref,     useNA = "always"))
cat("--- soc_pol_intense ---\n");  print(table(df$soc_pol_intense,  useNA = "always"))
cat("--- other_pref ---\n");       print(table(df$other_pref,       useNA = "always"))
cat("--- hc_univ_bin ---\n");      print(table(df$hc_univ_bin,      useNA = "always"))
cat("--- hc_target_bin ---\n");    print(table(df$hc_target_bin,    useNA = "always"))
cat("--- cc_univ_bin ---\n");      print(table(df$cc_univ_bin,      useNA = "always"))
cat("--- cc_target_bin ---\n");    print(table(df$cc_target_bin,    useNA = "always"))


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
  list(type="binary",  var="employ_fulltime_bin",      low=0, high=1, label="Employed Full-Time (No=0 vs Yes=1)"),
  list(type="numeric", var="ideo_right_num",           low=0, high=1, label="Ideology: Left (0) vs Right (1)"),
  list(type="binary",  var="vote_PLC_bin",             low=0, high=1, label="Liberal voter (No=0 vs Yes=1)"),
  list(type="binary",  var="vote_PCC_bin",             low=0, high=1, label="Conservative voter (No=0 vs Yes=1)"),
  list(type="binary", var= "children_bin",             low=0, high=1, label="Has children")
)

# RHS formula -- Ontario is the omitted region reference category
rhs <- "quebec_bin + ideo_define_QC_first_bin + alberta_bin + region_eastcoast_bin +
        incomeHigh_bin + ses_male_bin + age18_34_bin + age55plus_bin +
        univ_educ_bin + ses_citizenYes_bin + ses_french_bin +
        employ_fulltime_bin + children_bin +
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
  "children_bin"             = "Has Children",
  "employ_fulltime_bin"      = "Employed Full-Time",
  "ideo_right_num"           = "Ideology (Right)",
  "vote_PLC_bin"             = "Liberal voter",
  "vote_PCC_bin"             = "Conservative voter"
)

# ── Separate label vectors for each battery ───────────────────────────────
imp_policy_labels <- c(
  "health"   = "Healthcare",
  "edu"      = "Education",
  "pensions" = "Pensions",
  "taxes"    = "Tax reduction",
  "debt"     = "Debt reduction"
)

prio_policy_labels <- c(
  "health"  = "Healthcare access",
  "seniors" = "Home care for seniors",
  "cc"      = "Subsidized child care",
  "ecn"     = "Support businesses and economic growth",
  "clim"    = "Fight against climate change"
)

# Combined for any code that needs both
policy_labels <- c(imp_policy_labels, prio_policy_labels)


#*************************************************************************#
#########        3. HELPER FUNCTIONS                              #########
#*************************************************************************#

robust_vcov <- function(model) vcovHC(model, type = "HC1")

tidy_avg_slopes <- function(model, dv_label, question = NULL, data = df) {
  fml         <- formula(model)
  model_vars  <- all.vars(fml)
  model_data  <- data |> dplyr::select(all_of(model_vars)) |> drop_na()
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

imp_bin_vars <- df |> dplyr::select(matches("^budget_imp_.*_bin$")) |> names()
imp_policies <- str_extract(imp_bin_vars, "(?<=budget_imp_).*(?=_bin)")
imp_bin_df   <- tibble(var=imp_bin_vars, policy=imp_policies,
                       label=recode(imp_policies, !!!imp_policy_labels, .default=imp_policies))

prio_num_vars <- df |> dplyr::select(matches("^budget_prio_[a-z]+$")) |> names()
prio_policies <- str_remove(prio_num_vars, "budget_prio_")
prio_num_df   <- tibble(var=prio_num_vars, policy=prio_policies,
                        label=recode(prio_policies, !!!prio_policy_labels, .default=prio_policies))

prio_pref_vars    <- df |> dplyr::select(matches("^budget_prio_.*_pref$"))    |> names()
prio_intense_vars <- df |> dplyr::select(matches("^budget_prio_.*_intense$")) |> names()


#*************************************************************************#
#########   5. DESCRIPTIVE STATISTICS -- BAR CHARTS              #########
#*************************************************************************#

# ── 5.1  Budget IMPORTANCE binary ────────────────────────────────────────

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
    caption = "Binary: 1 = respondent ranked first out of all options"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

ggsave("graphs/descriptives/desc_imp_bin_grouped.png",
       width = 9, height = 5.5, dpi = 300)


# ── 5.2  Budget PRIORITY — first choice (_pref) ──────────────────────────

prio_pref_plot_df <- tibble(var = prio_pref_vars) |>
  mutate(
    policy = str_extract(var, "(?<=budget_prio_).*(?=_pref)"),
    label  = recode(policy, !!!prio_policy_labels, .default = policy),
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
   # title   = "Budget priority: share selecting each area as first choice",
    caption = "Binary: 1 = respondent allocated the most points to this policy"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

ggsave("graphs/descriptives/desc_prio_pref_grouped.png",
       width = 9, height = 5.5, dpi = 300)


# ── 5.3  Budget PRIORITY — intense allocators (_intense) ─────────────────

prio_intense_plot_df <- tibble(var = prio_intense_vars) |>
  mutate(
    policy = str_extract(var, "(?<=budget_prio_).*(?=_intense)"),
    label  = recode(policy, !!!prio_policy_labels, .default = policy),
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
    #title   = "Budget priority: share with intense preference per policy",
    caption = "Binary: 1 = respondent allocated >50 of 100 points to this policy"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

ggsave("graphs/descriptives/desc_prio_intense_grouped.png",
       width = 9, height = 5.5, dpi = 300)


# ── 5.4  COMPARISON: _pref vs _intense (dodged side-by-side) ─────────────

prio_pref_base <- tibble(var = prio_pref_vars) |>
  mutate(
    policy = str_extract(var, "(?<=budget_prio_).*(?=_pref)"),
    label  = recode(policy, !!!prio_policy_labels, .default = policy),
    pct    = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
    metric = "First choice"
  )

prio_intense_base <- tibble(var = prio_intense_vars) |>
  mutate(
    policy = str_extract(var, "(?<=budget_prio_).*(?=_intense)"),
    label  = recode(policy, !!!prio_policy_labels, .default = policy),
    pct    = map_dbl(var, ~ mean(df[[.x]], na.rm = TRUE) * 100),
    metric = "Intense preference"
  )

pref_order <- prio_pref_base |> arrange(pct) |> pull(label)

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
    #title   = "Budget priority: mean allocation across spending areas",
    caption = "Error bars = ±1 SD. Points sum to 100 per respondent."
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

ggsave("graphs/descriptives/desc_prio_mean.png", width = 9, height = 5.5, dpi = 300)


#*************************************************************************#
#########   5B. DESCRIPTIVE STATISTICS -- TRADEOFF BATTERIES    #########
#*************************************************************************#

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
  tax = "Raise revenue, tax preferences",
  hc  = "Home care policy priorities, tradeoffs",
  cc2 = "Childcare policy priorities, tradeoffs"
)

build_tradeoff_df <- function(battery, suffix = "") {
  var_map   <- tradeoff_labels[[battery]]
  base_vars <- names(var_map)
  vars      <- if (suffix != "") paste0(base_vars, suffix) else base_vars
  labels    <- unname(var_map)
  tibble(var = vars, label = labels, battery = battery) |>
    mutate(
      pct = map_dbl(var, ~ {
        col <- df[[.x]]
        if (is.null(col)) NA_real_
        else if (max(col, na.rm = TRUE) <= 1) mean(col, na.rm = TRUE) * 100
        else mean(col, na.rm = TRUE)
      })
    )
}

plot_tradeoff_bar <- function(plot_df, x_lab, title_str, fill_col, x_limit, file_path, is_pct = TRUE) {
  fmt    <- if (is_pct) "%.1f%%" else "%.1f"
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
    labs(x = x_lab, y = NULL) +#, title = title_str) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text  = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold", color = "black")
    )
}

plot_tradeoff_comparison <- function(battery, file_path) {
  pref_df    <- build_tradeoff_df(battery, "_pref")    |> mutate(metric = "First choice")
  intense_df <- build_tradeoff_df(battery, "_intense") |> mutate(metric = "Intense preference")
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
      x = "% of respondents", y = NULL, fill = NULL, pattern = NULL,
      caption = paste0(
        "First choice: respondent allocated the most points to this option.\n",
        "Intense preference: respondent allocated >50 of 100 points to this option."
      )
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position    = "bottom",
      panel.grid.major.y = element_blank(),
      axis.text  = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold", color = "black"),
      plot.caption = element_text(color = "black")
    )
  ggsave(file_path, width = 10, height = 5.5, dpi = 300)
}

batteries_desc <- names(tradeoff_labels)

walk(batteries_desc, function(bat) {
  raw_df <- build_tradeoff_df(bat, suffix = "")
  p_raw  <- plot_tradeoff_bar(
    plot_df = raw_df, x_lab = "Mean points allocated (out of 100)",
    title_str = paste0(battery_titles[[bat]], " — mean allocation"),
    fill_col = "#4dac26", x_limit = 80, is_pct = FALSE,
    file_path = paste0("graphs/descriptives/tradeoff_", bat, "_mean.png")
  )
  ggsave(paste0("graphs/descriptives/tradeoff_", bat, "_mean.png"),
         plot = p_raw, width = 9, height = 4.5 + 0.4 * nrow(raw_df), dpi = 300)
  
  pref_df <- build_tradeoff_df(bat, "_pref")
  p_pref  <- plot_tradeoff_bar(
    plot_df = pref_df, x_lab = "% of respondents (first choice)",
    title_str = paste0(battery_titles[[bat]], " — first choice"),
    fill_col = "#d6604d", x_limit = 80, is_pct = TRUE,
    file_path = paste0("graphs/descriptives/tradeoff_", bat, "_pref.png")
  )
  ggsave(paste0("graphs/descriptives/tradeoff_", bat, "_pref.png"),
         plot = p_pref, width = 9, height = 4.5 + 0.4 * nrow(pref_df), dpi = 300)
  
  intense_df <- build_tradeoff_df(bat, "_intense")
  p_intense  <- plot_tradeoff_bar(
    plot_df = intense_df, x_lab = "% of respondents (intense preference: >50 pts)",
    title_str = paste0(battery_titles[[bat]], " — intense preference"),
    fill_col = "#762a83", x_limit = 60, is_pct = TRUE,
    file_path = paste0("graphs/descriptives/tradeoff_", bat, "_intense.png")
  )
  ggsave(paste0("graphs/descriptives/tradeoff_", bat, "_intense.png"),
         plot = p_intense, width = 9, height = 4.5 + 0.4 * nrow(intense_df), dpi = 300)
  
  plot_tradeoff_comparison(
    battery   = bat,
    file_path = paste0("graphs/descriptives/tradeoff_", bat, "_pref_vs_intense.png")
  )
  cat("Battery", bat, "-- all 4 plots saved.\n")
})


#*************************************************************************#
#########   5C. TRADEOFF DESCRIPTIVES -- REGIONAL COMPARISON    #########
#*************************************************************************#

region_var    <- "ses_region_cat"
region_levels <- c("Ontario", "Quebec", "Alberta", "East Coast")

build_tradeoff_regional <- function(battery) {
  var_map    <- tradeoff_labels[[battery]]
  base_vars  <- names(var_map)
  opt_labels <- unname(var_map)
  pref_vars    <- paste0(base_vars, "_pref")
  intense_vars <- paste0(base_vars, "_intense")
  map_dfr(region_levels, function(reg) {
    reg_data <- df |> filter(.data[[region_var]] == reg)
    bind_rows(
      tibble(region = reg, label = opt_labels, var = pref_vars,    metric = "First choice"),
      tibble(region = reg, label = opt_labels, var = intense_vars, metric = "Intense preference")
    ) |>
      mutate(
        pct = map_dbl(var, ~ {
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
  plot_df <- build_tradeoff_regional(battery)
  sort_order <- plot_df |>
    filter(metric == "First choice") |>
    group_by(label) |>
    summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop") |>
    arrange(mean_pct) |>
    pull(label)
  plot_df <- plot_df |> mutate(label = factor(label, levels = sort_order))
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
    scale_fill_manual(values    = c("First choice" = "grey30", "Intense preference" = "grey70")) +
    scale_pattern_manual(values = c("First choice" = "none",   "Intense preference" = "stripe")) +
    scale_x_continuous(
      limits = c(0, 85),
      labels = scales::label_number(suffix = "%"),
      expand = expansion(mult = c(0, 0.05))
    ) +
    facet_wrap(~ region, ncol = 2) +
    labs(
      x = "% of respondents within region", y = NULL, fill = NULL, pattern = NULL,
      #title   = paste0(battery_titles[[battery]], " — by region"),
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
      strip.text = element_text(face = "bold", color = "black"),
      axis.text  = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold", color = "black"),
      plot.caption = element_text(color = "grey30")
    )
  n_opts <- length(tradeoff_labels[[battery]])
  ggsave(file_path, width = 13, height = 5 + n_opts * 0.6, dpi = 300)
}

walk(batteries_desc, function(bat) {
  plot_tradeoff_regional(
    battery   = bat,
    file_path = paste0("graphs/descriptives/tradeoff_", bat, "_regional.png")
  )
  cat("Battery", bat, "-- regional plot saved.\n")
})


#*************************************************************************#
#########   5D. DESCRIPTIVES -- COMPOSITE VARIABLES              #########
#*************************************************************************#

# ── 5D.1  Expansion vs Reduction -- simple bar chart ─────────────────────

composite_imp_df <- tibble(
  label = c("Expansion priority\n(Health, education, or pensions ranked first)",
            "Reduction priority\n(Tax or debt reduction ranked first)"),
  pct   = c(
    mean(df$budget_imp_health_bin == 1 | df$budget_imp_edu_bin == 1 |
           df$budget_imp_pensions_bin == 1, na.rm = TRUE) * 100,
    mean(df$budget_imp_taxes_bin == 1 | df$budget_imp_debt_bin == 1,
         na.rm = TRUE) * 100
  )
)

ggplot(composite_imp_df, aes(x = pct, y = reorder(label, pct))) +
  geom_col(fill = "#2166ac", width = 0.5, color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.12, size = 4, color = "black") +
  scale_x_continuous(
    limits = c(0, 100),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x       = "% of respondents",
    y       = NULL,
    caption = "Expansion: ranked healthcare, education, or pensions as budget priority.\nReduction: ranked tax or debt reduction as budget priority."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.caption = element_text(color = "black")
  )

ggsave("graphs/descriptives/desc_composite_imp_expansion_reduc.png",
       width = 9, height = 4, dpi = 300)


# ── 5D.2  Social policy vs Other -- _pref vs _intense (dodged) ───────────

composite_pref_base <- tibble(
  label  = c("Social policy (Healthcare, Home care, Subsidized child care)",
             "Other (Economic growth, Climate change)"),
  pct    = c(
    mean(df$budget_prio_health_pref == 1 | df$budget_prio_seniors_pref == 1 |
           df$budget_prio_cc_pref == 1, na.rm = TRUE) * 100,
    mean(df$budget_prio_ecn_pref == 1 | df$budget_prio_clim_pref == 1,
         na.rm = TRUE) * 100
  ),
  metric = "First choice"
)

composite_intense_base <- tibble(
  label  = c("Social policy (Healthcare, Home care, Subsidized child care)",
             "Other (Economic growth, Climate change)"),
  pct    = c(
    mean(df$budget_prio_health_intense == 1 | df$budget_prio_seniors_intense == 1 |
           df$budget_prio_cc_intense == 1, na.rm = TRUE) * 100,
    mean(df$budget_prio_ecn_intense == 1 | df$budget_prio_clim_intense == 1,
         na.rm = TRUE) * 100
  ),
  metric = "Intense preference"
)

pref_order_comp <- composite_pref_base |> arrange(pct) |> pull(label)

composite_prio_df <- bind_rows(composite_pref_base, composite_intense_base) |>
  mutate(
    label  = factor(label, levels = pref_order_comp),
    metric = factor(metric, levels = c("First choice", "Intense preference"))
  )

ggplot(composite_prio_df, aes(x = pct, y = label, fill = metric, pattern = metric)) +
  geom_col_pattern(
    position        = position_dodge(width = 0.7),
    width           = 0.55,
    color           = "black",
    pattern_fill    = "black",
    pattern_density = 0.05,
    pattern_spacing = 0.02
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    position = position_dodge(width = 0.7),
    hjust = -0.12, size = 3.5, color = "black"
  ) +
  scale_fill_manual(values    = c("First choice" = "grey30", "Intense preference" = "grey70")) +
  scale_pattern_manual(values = c("First choice" = "none",   "Intense preference" = "stripe")) +
  scale_x_continuous(
    limits = c(0, 100),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "% of respondents", y = NULL, fill = NULL, pattern = NULL,
    caption = paste0(
      "First choice: respondent allocated the most points to any area in the group.\n",
      "Intense preference: respondent allocated >50 of 100 points to any area in the group.\n",
      "Social policy = Healthcare access, Home care for seniors, Subsidized child care.\n",
      "Other = Support businesses and economic growth, Fight against climate change."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", color = "black"),
    plot.caption = element_text(color = "black")
  )

ggsave("graphs/descriptives/desc_composite_prio_soc_vs_other.png",
       width = 10, height = 5, dpi = 300)


# ── 5D.3  HC composites -- universal vs targeted (dodged) ────────────────

hc_pref_base <- tibble(
  label  = c("Universal (all seniors)", "Targeted (low-income seniors)"),
  pct    = c(
    mean(df$tradeoff_hc_all_pref == 1, na.rm = TRUE) * 100,
    mean(df$tradeoff_hc_spend_pref == 1 | df$tradeoff_hc_pensions_pref == 1, na.rm = TRUE) * 100
  ),
  metric = "First choice"
)

hc_intense_base <- tibble(
  label  = c("Universal (all seniors)", "Targeted (low-income seniors)"),
  pct    = c(
    mean(df$tradeoff_hc_all_intense == 1, na.rm = TRUE) * 100,
    mean(df$tradeoff_hc_spend_intense == 1 | df$tradeoff_hc_pensions_intense == 1, na.rm = TRUE) * 100
  ),
  metric = "Intense preference"
)

pref_order_hc <- hc_pref_base |> arrange(pct) |> pull(label)

hc_comp_df <- bind_rows(hc_pref_base, hc_intense_base) |>
  mutate(
    label  = factor(label, levels = pref_order_hc),
    metric = factor(metric, levels = c("First choice", "Intense preference"))
  )

ggplot(hc_comp_df, aes(x = pct, y = label, fill = metric, pattern = metric)) +
  geom_col_pattern(
    position        = position_dodge(width = 0.7),
    width           = 0.55,
    color           = "black",
    pattern_fill    = "black",
    pattern_density = 0.05,
    pattern_spacing = 0.02
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    position = position_dodge(width = 0.7),
    hjust = -0.12, size = 3.5, color = "black"
  ) +
  scale_fill_manual(values    = c("First choice" = "grey30", "Intense preference" = "grey70")) +
  scale_pattern_manual(values = c("First choice" = "none",   "Intense preference" = "stripe")) +
  scale_x_continuous(
    limits = c(0, 100),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "% of respondents", y = NULL, fill = NULL, pattern = NULL,
   # title   = "Home care tradeoffs: universal vs. targeted — first choice vs. intense preference",
    caption = paste0(
      "First choice: respondent allocated the most points to any option in the group.\n",
      "Intense preference: respondent allocated >50 of 100 points to any option in the group.\n",
      "Universal = home care for all seniors. Targeted = home care or pensions for low-income seniors."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", color = "black"),
    plot.caption = element_text(color = "black")
  )

ggsave("graphs/descriptives/desc_composite_hc_univ_vs_target.png",
       width = 10, height = 5, dpi = 300)


# ── 5D.4  CC2 composites -- universal vs targeted (dodged) ───────────────

cc_pref_base <- tibble(
  label  = c("Universal (all families)", "Targeted (low-income families)"),
  pct    = c(
    mean(df$tradeoff_cc2_all_pref == 1 | df$tradeoff_cc2_educ_all_pref == 1, na.rm = TRUE) * 100,
    mean(df$tradeoff_cc2_low_inc_pref == 1 | df$tradeoff_cc2_educ_low_inc_pref == 1, na.rm = TRUE) * 100
  ),
  metric = "First choice"
)

cc_intense_base <- tibble(
  label  = c("Universal (all families)", "Targeted (low-income families)"),
  pct    = c(
    mean(df$tradeoff_cc2_all_intense == 1 | df$tradeoff_cc2_educ_all_intense == 1, na.rm = TRUE) * 100,
    mean(df$tradeoff_cc2_low_inc_intense == 1 | df$tradeoff_cc2_educ_low_inc_intense == 1, na.rm = TRUE) * 100
  ),
  metric = "Intense preference"
)

pref_order_cc <- cc_pref_base |> arrange(pct) |> pull(label)

cc_comp_df <- bind_rows(cc_pref_base, cc_intense_base) |>
  mutate(
    label  = factor(label, levels = pref_order_cc),
    metric = factor(metric, levels = c("First choice", "Intense preference"))
  )

ggplot(cc_comp_df, aes(x = pct, y = label, fill = metric, pattern = metric)) +
  geom_col_pattern(
    position        = position_dodge(width = 0.7),
    width           = 0.55,
    color           = "black",
    pattern_fill    = "black",
    pattern_density = 0.05,
    pattern_spacing = 0.02
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    position = position_dodge(width = 0.7),
    hjust = -0.12, size = 3.5, color = "black"
  ) +
  scale_fill_manual(values    = c("First choice" = "grey30", "Intense preference" = "grey70")) +
  scale_pattern_manual(values = c("First choice" = "none",   "Intense preference" = "stripe")) +
  scale_x_continuous(
    limits = c(0, 100),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "% of respondents", y = NULL, fill = NULL, pattern = NULL,
    #title   = "Childcare tradeoffs: universal vs. targeted — first choice vs. intense preference",
    caption = paste0(
      "First choice: respondent allocated the most points to any option in the group.\n",
      "Intense preference: respondent allocated >50 of 100 points to any option in the group.\n",
      "Universal = childcare or education quality for all families. Targeted = same programs for low-income families."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", color = "black"),
    plot.caption = element_text(color = "black")
  )

ggsave("graphs/descriptives/desc_composite_cc_univ_vs_target.png",
       width = 10, height = 5, dpi = 300)


# ── 5.6  Budget priority: _pref vs _intense by region ────────────────────

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
    mutate(
      pct    = map_dbl(var, ~ mean(reg_data[[.x]], na.rm = TRUE) * 100),
      region = factor(region, levels = region_levels),
      metric = factor(metric, levels = c("First choice", "Intense preference"))
    )
})

sort_order_prio <- prio_regional_df |>
  filter(metric == "First choice") |>
  group_by(label) |>
  summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop") |>
  arrange(mean_pct) |>
  pull(label)

prio_regional_df <- prio_regional_df |>
  mutate(label = factor(label, levels = sort_order_prio))

ggplot(prio_regional_df, aes(x = pct, y = label, fill = metric, pattern = metric)) +
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
  scale_fill_manual(values    = c("First choice" = "grey30", "Intense preference" = "grey70")) +
  scale_pattern_manual(values = c("First choice" = "none",   "Intense preference" = "stripe")) +
  scale_x_continuous(
    limits = c(0, 85),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  facet_wrap(~ region, ncol = 2) +
  labs(
    x = "% of respondents within region", y = NULL, fill = NULL, pattern = NULL,
   # title   = "Budget priority: first choice vs. intense preference — by region",
    caption = paste0(
      "First choice: respondent allocated the most points to this area.\n",
      "Intense preference: respondent allocated >50 of 100 points to this area.\n",
      "Percentages computed within each region."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    strip.text   = element_text(face = "bold", color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(color = "black"),
    plot.title   = element_text(face = "bold", color = "black"),
    plot.caption = element_text(color = "grey30")
  )

ggsave("graphs/descriptives/desc_prio_pref_vs_intense_regional.png",
       width = 13, height = 7, dpi = 300)


#*************************************************************************#
#########   6. BIVARIATE MARGINAL MEANS (ALL IVs x ALL DVs)     #########
#*************************************************************************#

all_reg_df <- bind_rows(
  imp_bin_df  |> mutate(battery = "imp_bin"),
  prio_num_df |> mutate(battery = "prio_num")
)

marginal_results <- map_dfr(ivs, function(iv) {
  map2_dfr(all_reg_df$var, all_reg_df$label, interpret_contrast, iv = iv, data = df)
})

write.csv(marginal_results, "graphs/regressions/marginal_means_all.csv", row.names = FALSE)


#*************************************************************************#
#########   7. REGRESSIONS -- IMPORTANCE BINARY (LPM + LOGIT)   #########
#*************************************************************************#

lpm_imp <- imp_bin_df$var |> set_names(imp_bin_df$label) |>
  map(~lm(as.formula(paste(.x, "~", rhs)), data = df))

logit_imp <- imp_bin_df$var |> set_names(imp_bin_df$label) |>
  map(~glm(as.formula(paste(.x, "~", rhs)), data = df, family = binomial(link = "logit")))

coef_lpm_imp <- map2_dfr(lpm_imp, imp_bin_df$label, tidy_avg_slopes, question = "imp")

coef_logit_imp <- map2_dfr(logit_imp, imp_bin_df$label, function(model, dv_label) {
  fml        <- formula(model)
  model_vars <- all.vars(fml)
  model_data <- df |> select(all_of(model_vars)) |> drop_na()
  vm         <- robust_vcov(model)
  avg_slopes(model, vcov = vm, newdata = model_data) |> as_tibble() |>
    transmute(question = "imp", dv = dv_label, term,
              estimate  = round(estimate, 3), conf.low = round(conf.low, 3),
              conf.high = round(conf.high, 3), p.value  = round(p.value, 3),
              sig = case_when(p.value<0.001~"***",p.value<0.01~"**",p.value<0.05~"*",p.value<0.10~".",TRUE~""),
              direction = case_when(conf.low>0~"Positive",conf.high<0~"Negative",TRUE~"No clear effect"))
})

fit_lpm_imp <- map2_dfr(lpm_imp, imp_bin_df$label, function(model, dv_label) {
  s <- summary.lm(model)
  tibble(dv = dv_label, model = "LPM", r_squared = round(s$r.squared, 3),
         adj_r_sq = round(s$adj.r.squared, 3), n = length(s$residuals))
})

fit_logit_imp <- map2_dfr(logit_imp, imp_bin_df$label, function(model, dv_label) {
  s <- summary.glm(model)
  tibble(dv = dv_label, model = "Logit",
         pseudo_r2 = round(1 - (s$deviance / s$null.deviance), 3), n = length(s$residuals))
})

modelsummary(lpm_imp, estimate = "{estimate}{stars}", statistic = "({std.error})",
             vcov = map(lpm_imp, robust_vcov), coef_map = term_labels,
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "graphs/regressions/regtable_imp_LPM.txt",
             notes = "LPM (OLS). HC1 robust SEs in parentheses. * p<0.05, ** p<0.01, *** p<0.001")

modelsummary(logit_imp, estimate = "{estimate}{stars}", statistic = "({std.error})",
             vcov = map(logit_imp, robust_vcov), coef_map = term_labels,
             gof_map = c("nobs", "logLik", "AIC"),
             output = "graphs/regressions/regtable_imp_logit_coefs.txt",
             notes = "Logit coefficients (log-odds). HC1 robust SEs. Interpret via AME table.")

modelsummary(map(logit_imp, ~avg_slopes(.x, vcov = robust_vcov(.x))),
             estimate = "{estimate}{stars}", statistic = "({std.error})",
             coef_map = term_labels, gof_map = c("nobs"),
             output = "graphs/regressions/regtable_imp_logit_AME.txt",
             notes = "Average marginal effects from logit. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001")

plot_coefs(coef_lpm_imp,   "Importance (binary) -- LPM coefficients",               "graphs/regressions/coef_imp_LPM.png")
plot_coefs(coef_logit_imp, "Importance (binary) -- Logit average marginal effects",  "graphs/regressions/coef_imp_logit_AME.png")

bind_rows(coef_lpm_imp |> mutate(model = "LPM"), coef_logit_imp |> mutate(model = "Logit AME")) |>
  mutate(term = recode(term, !!!term_labels)) |>
  ggplot(aes(x = estimate, y = reorder(term, estimate), color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2,
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("LPM" = "#2166ac", "Logit AME" = "#d6604d")) +
  facet_wrap(~dv) +
  labs(x = "Estimated effect", y = NULL, color = NULL, shape = NULL,
       title = "Importance battery -- LPM vs. Logit AME (robustness check)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
ggsave("graphs/regressions/coef_imp_LPM_vs_logit.png", width = 14, height = 9, dpi = 300)

plot_r2(fit_lpm_imp,   r2_col = "adj_r_sq",  title_str = "Model fit -- Importance battery (LPM)",             file_path = "graphs/regressions/r2_imp_LPM.png")
plot_r2(fit_logit_imp, r2_col = "pseudo_r2", title_str = "Model fit -- Importance battery (Logit pseudo-R2)", file_path = "graphs/regressions/r2_imp_logit.png")


#*************************************************************************#
#########   8. REGRESSIONS -- PRIORITY RAW SCORE (OLS)          #########
#*************************************************************************#

lm_prio <- prio_num_df$var |> set_names(prio_num_df$label) |>
  map(~lm(as.formula(paste(.x, "~", rhs)), data = df))

coef_prio <- map2_dfr(lm_prio, prio_num_df$label, tidy_avg_slopes, question = "prio")

fit_prio <- map2_dfr(lm_prio, prio_num_df$label, function(model, dv_label) {
  s <- summary(model)
  tibble(dv = dv_label, r_squared = round(s$r.squared, 3),
         adj_r_sq = round(s$adj.r.squared, 3), n = length(model$residuals))
})

modelsummary(lm_prio, estimate = "{estimate}{stars}", statistic = "({std.error})",
             vcov = map(lm_prio, robust_vcov), coef_map = term_labels,
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "graphs/regressions/regtable_prio_OLS.txt",
             notes = "OLS. HC1 robust SEs. DV = budget points allocated (0-100). * p<0.05, ** p<0.01, *** p<0.001")

plot_coefs(coef_prio, "Budget priority allocation (0-100 pts) -- OLS avg marginal effects", "graphs/regressions/coef_prio_OLS.png")
plot_r2(fit_prio, r2_col = "adj_r_sq", title_str = "Model fit -- Priority allocation (OLS)", file_path = "graphs/regressions/r2_prio_OLS.png")


#*************************************************************************#
#########   9. MARGINAL EFFECTS PLOTS -- IDEOLOGY x QUEBEC       #########
#*************************************************************************#

walk2(
  c(imp_bin_df$var, prio_num_df$var),
  c(imp_bin_df$label, prio_num_df$label),
  function(dv, dv_label) {
    model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> select(all_of(model_vars)) |> drop_na()
    model      <- lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    me         <- ggpredict(model, terms = c("ideo_right_num [all]", "quebec_bin [0,1]"))
    ggplot(me, aes(x = x, y = predicted, colour = as.factor(group), fill = as.factor(group))) +
      geom_line(linewidth = 1) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
      scale_colour_manual(values = c("0" = "#d6604d", "1" = "#2166ac"),
                          labels = c("0" = "Rest of Canada", "1" = "Quebec")) +
      scale_fill_manual(values   = c("0" = "#d6604d", "1" = "#2166ac"),
                        labels   = c("0" = "Rest of Canada", "1" = "Quebec")) +
      labs(x = "Ideology (Left to Right)", y = "Predicted value",
           colour = NULL, fill = NULL,
           title   = paste("Ideology x Quebec --", dv_label),
           caption = "OLS predictions. HC1 robust SEs. Shaded area = 95% CI.") +
      theme_minimal(base_size = 12) + theme(legend.position = "bottom")
    ggsave(paste0("graphs/regressions/me_ideo_", dv, ".png"), width = 8, height = 5, dpi = 300)
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
  count(term, sort = TRUE) |> mutate(term = recode(term, !!!term_labels)) |> print(n = 20)

cat("\n========== Most significant predictors -- prio OLS ==========\n")
coef_prio |> filter(sig %in% c("*","**","***")) |>
  count(term, sort = TRUE) |> mutate(term = recode(term, !!!term_labels)) |> print(n = 20)

cat("\n========== LPM vs Logit direction agreement check ==========\n")
bind_rows(coef_lpm_imp |> mutate(model = "LPM"), coef_logit_imp |> mutate(model = "Logit")) |>
  filter(sig %in% c("*","**","***")) |>
  select(dv, term, model, direction) |>
  pivot_wider(names_from = model, values_from = direction) |>
  filter(LPM != Logit) |>
  print()


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
      tradeoff_cc1_tax              = "Raise taxes",
      tradeoff_cc1_cut              = "Cut other spending",
      tradeoff_cc1_debt             = "Increase debt",
      tradeoff_cc1_no_spend         = "Don't spend more",
      tradeoff_cc1_tax_pref         = "Raise taxes",
      tradeoff_cc1_cut_pref         = "Cut other spending",
      tradeoff_cc1_debt_pref        = "Increase debt",
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
      tradeoff_ge_tax              = "Raise taxes",
      tradeoff_ge_cut              = "Cut other spending",
      tradeoff_ge_debt             = "Increase debt",
      tradeoff_ge_no_spend         = "Don't spend more",
      tradeoff_ge_tax_pref         = "Raise taxes",
      tradeoff_ge_cut_pref         = "Cut other spending",
      tradeoff_ge_debt_pref        = "Increase debt",
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
      tradeoff_tax_less_services         = "No increase, even if fewer services",
      tradeoff_tax_sales_tax             = "Sales tax increase",
      tradeoff_tax_inc_tax               = "Income tax increase",
      tradeoff_tax_wealth_tax            = "Wealth tax",
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
    pref    = c("tradeoff_hc_all_pref", "tradeoff_hc_spend_pref", "tradeoff_hc_pensions_pref"),
    intense = c("tradeoff_hc_all_intense", "tradeoff_hc_spend_intense", "tradeoff_hc_pensions_intense"),
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
      tradeoff_cc2_all                  = "Increase child care for all, reduce other family benefits",
      tradeoff_cc2_low_inc              = "Target low-income families, childcare",
      tradeoff_cc2_educ_all             = "Target education quality for all, reduce other family benefits",
      tradeoff_cc2_educ_low_inc         = "Target education quality for low-income",
      tradeoff_cc2_all_pref             = "Increase child care for all, reduce other family benefits",
      tradeoff_cc2_low_inc_pref         = "Target low-income families, childcare",
      tradeoff_cc2_educ_all_pref        = "Target education quality for all, reduce other family benefits",
      tradeoff_cc2_educ_low_inc_pref    = "Target education quality for low-income",
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

tradeoff_models <- map(batteries, function(bat) {
  
  # OLS on raw scores (continuous 0-100 allocation)
  lm_raw <- bat$raw |> set_names(bat$labels[bat$raw]) |>
    map(function(dv) {
      model_data <- df |> dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |> drop_na()
      lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    })
  
  # LPM -- first choice (robustness check)
  lpm_pref <- bat$pref |> set_names(bat$labels[bat$pref]) |>
    map(function(dv) {
      model_data <- df |> dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |> drop_na()
      lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    })
  
  # Logit -- first choice (primary)
  logit_pref <- bat$pref |> set_names(bat$labels[bat$pref]) |>
    map(function(dv) {
      model_data <- df |> dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |> drop_na()
      glm(as.formula(paste(dv, "~", rhs)), data = model_data, family = binomial(link = "logit"))
    })
  
  # LPM -- intense preference (robustness check)
  lpm_intense <- bat$intense |> set_names(bat$labels[bat$intense]) |>
    map(function(dv) {
      model_data <- df |> dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |> drop_na()
      lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    })
  
  # Logit -- intense preference (primary)
  logit_intense <- bat$intense |> set_names(bat$labels[bat$intense]) |>
    map(function(dv) {
      model_data <- df |> dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |> drop_na()
      glm(as.formula(paste(dv, "~", rhs)), data = model_data, family = binomial(link = "logit"))
    })
  
  list(lm_raw       = lm_raw,
       lpm_pref     = lpm_pref,     logit_pref    = logit_pref,
       lpm_intense  = lpm_intense,  logit_intense = logit_intense)
})


#*************************************************************************#
#########   T3. AVERAGE MARGINAL EFFECTS                         #########
#*************************************************************************#

# Helper: AME from logit with HC1 robust SEs
ame_logit_battery <- function(model_list, question_tag) {
  map2_dfr(model_list, names(model_list), function(model, dv_label) {
    fml        <- formula(model)
    model_vars <- all.vars(fml)
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    vm         <- robust_vcov(model)
    avg_slopes(model, vcov = vm, newdata = model_data) |>
      as_tibble() |>
      transmute(
        question  = question_tag, dv = dv_label, term,
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
          conf.low  > 0 ~ "Positive",
          conf.high < 0 ~ "Negative",
          TRUE          ~ "No clear effect"
        )
      )
  })
}

coef_tradeoff <- map_dfr(names(batteries), function(bat_name) {
  bat    <- batteries[[bat_name]]
  models <- tradeoff_models[[bat_name]]
  
  # Primary: Logit AME
  ame_logit_pref    <- ame_logit_battery(models$logit_pref,    paste0(bat_name, "_pref_logit"))
  ame_logit_intense <- ame_logit_battery(models$logit_intense, paste0(bat_name, "_intense_logit"))
  
  # Robustness: LPM
  ame_lpm_pref    <- map2_dfr(models$lpm_pref,    names(models$lpm_pref),    tidy_avg_slopes, question = paste0(bat_name, "_pref_lpm"))
  ame_lpm_intense <- map2_dfr(models$lpm_intense, names(models$lpm_intense), tidy_avg_slopes, question = paste0(bat_name, "_intense_lpm"))
  
  # Raw OLS (kept for reference)
  ame_raw <- map2_dfr(models$lm_raw, names(models$lm_raw), tidy_avg_slopes, question = paste0(bat_name, "_raw"))
  
  bind_rows(ame_logit_pref, ame_logit_intense,
            ame_lpm_pref,   ame_lpm_intense,
            ame_raw) |>
    mutate(battery = bat_name, battery_title = bat$title)
})

write.csv(coef_tradeoff, "graphs/regressions/AME_tradeoff_all.csv", row.names = FALSE)


#*************************************************************************#
#########   T4. MODEL FIT                                        #########
#*************************************************************************#

fit_tradeoff <- map_dfr(names(batteries), function(bat_name) {
  models <- tradeoff_models[[bat_name]]
  
  # OLS raw
  fit_raw <- map2_dfr(models$lm_raw, names(models$lm_raw), function(model, dv_label) {
    s <- summary(model)
    tibble(battery    = bat_name, dv = dv_label, model_type = "OLS_raw",
           r_squared  = round(s$r.squared, 3),
           adj_r_sq   = round(s$adj.r.squared, 3),
           n          = length(s$residuals))
  })
  
  # Logit _pref (primary)
  fit_logit_pref <- map2_dfr(models$logit_pref, names(models$logit_pref), function(model, dv_label) {
    s <- summary.glm(model)
    tibble(battery   = bat_name, dv = dv_label, model_type = "Logit_pref",
           pseudo_r2 = round(1 - (s$deviance / s$null.deviance), 3),
           n         = length(s$residuals))
  })
  
  # Logit _intense (primary)
  fit_logit_intense <- map2_dfr(models$logit_intense, names(models$logit_intense), function(model, dv_label) {
    s <- summary.glm(model)
    tibble(battery   = bat_name, dv = dv_label, model_type = "Logit_intense",
           pseudo_r2 = round(1 - (s$deviance / s$null.deviance), 3),
           n         = length(s$residuals))
  })
  
  # LPM _pref (robustness)
  fit_lpm_pref <- map2_dfr(models$lpm_pref, names(models$lpm_pref), function(model, dv_label) {
    s <- summary(model)
    tibble(battery   = bat_name, dv = dv_label, model_type = "LPM_pref",
           r_squared = round(s$r.squared, 3),
           adj_r_sq  = round(s$adj.r.squared, 3),
           n         = length(s$residuals))
  })
  
  # LPM _intense (robustness)
  fit_lpm_intense <- map2_dfr(models$lpm_intense, names(models$lpm_intense), function(model, dv_label) {
    s <- summary(model)
    tibble(battery   = bat_name, dv = dv_label, model_type = "LPM_intense",
           r_squared = round(s$r.squared, 3),
           adj_r_sq  = round(s$adj.r.squared, 3),
           n         = length(s$residuals))
  })
  
  bind_rows(fit_raw, fit_logit_pref, fit_logit_intense,
            fit_lpm_pref, fit_lpm_intense)
})

print(fit_tradeoff)
write.csv(fit_tradeoff, "graphs/regressions/fit_tradeoff_all.csv", row.names = FALSE)


#*************************************************************************#
#########   T5. REGRESSION TABLES                                #########
#*************************************************************************#

walk(names(batteries), function(bat_name) {
  models <- tradeoff_models[[bat_name]]
  bat    <- batteries[[bat_name]]
  
  # Primary: Logit AME tables
  modelsummary(
    map(models$logit_pref, ~avg_slopes(.x, vcov = robust_vcov(.x))),
    estimate  = "{estimate}{stars}", statistic = "({std.error})",
    coef_map  = term_labels, gof_map = c("nobs"),
    output    = paste0("graphs/regressions/regtable_tradeoff_", bat_name, "_pref_logit_AME.txt"),
    notes     = paste0(bat$title, " — Logit AME. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001")
  )
  
  modelsummary(
    map(models$logit_intense, ~avg_slopes(.x, vcov = robust_vcov(.x))),
    estimate  = "{estimate}{stars}", statistic = "({std.error})",
    coef_map  = term_labels, gof_map = c("nobs"),
    output    = paste0("graphs/regressions/regtable_tradeoff_", bat_name, "_intense_logit_AME.txt"),
    notes     = paste0(bat$title, " — Logit AME. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001")
  )
  
  # Robustness: LPM tables
  modelsummary(
    models$lpm_pref,
    estimate  = "{estimate}{stars}", statistic = "({std.error})",
    vcov      = map(models$lpm_pref, robust_vcov),
    coef_map  = term_labels, gof_map = c("nobs", "r.squared"),
    output    = paste0("graphs/regressions/regtable_tradeoff_", bat_name, "_pref_lpm.txt"),
    notes     = paste0(bat$title, " — LPM robustness check. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001")
  )
  
  modelsummary(
    models$lpm_intense,
    estimate  = "{estimate}{stars}", statistic = "({std.error})",
    vcov      = map(models$lpm_intense, robust_vcov),
    coef_map  = term_labels, gof_map = c("nobs", "r.squared"),
    output    = paste0("graphs/regressions/regtable_tradeoff_", bat_name, "_intense_lpm.txt"),
    notes     = paste0(bat$title, " — LPM robustness check. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001")
  )
  
  # Raw OLS table (kept for reference)
  modelsummary(
    models$lm_raw,
    estimate  = "{estimate}{stars}", statistic = "({std.error})",
    vcov      = map(models$lm_raw, robust_vcov),
    coef_map  = term_labels, gof_map = c("nobs", "r.squared"),
    output    = paste0("graphs/regressions/regtable_tradeoff_", bat_name, "_raw_ols.txt"),
    notes     = paste0(bat$title, " — OLS (0-100 allocation). HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001")
  )
})


#*************************************************************************#
#########   T6. COEFFICIENT PLOTS                                #########
#*************************************************************************#

walk(names(batteries), function(bat_name) {
  bat <- batteries[[bat_name]]
  
  # ── Primary: Logit AME plots ────────────────────────────────────────────
  
  coef_logit_pref <- coef_tradeoff |>
    dplyr::filter(battery == bat_name, question == paste0(bat_name, "_pref_logit"))
  
  if (nrow(coef_logit_pref) > 0) {
    coef_logit_pref |>
      mutate(term = recode(term, !!!term_labels)) |>
      ggplot(aes(x = estimate, y = reorder(term, estimate), color = direction)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 2.5) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      scale_color_manual(values = c("Positive"="#2166ac","Negative"="#d6604d","No clear effect"="grey60")) +
      facet_wrap(~dv, ncol = 2, scales = "free_x") +
      labs(x = "Average marginal effect (HC1 robust SEs)", y = NULL, color = NULL,
           title   = paste0(bat$title, " — Logit AME (first choice)"),
           caption = "Logit AME. HC1 robust SEs. 95% CI.") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
    ggsave(paste0("graphs/regressions/coef_tradeoff_", bat_name, "_pref_logit_AME.png"),
           width = 14, height = 10, dpi = 300)
  }
  
  coef_logit_intense <- coef_tradeoff |>
    dplyr::filter(battery == bat_name, question == paste0(bat_name, "_intense_logit"))
  
  if (nrow(coef_logit_intense) > 0) {
    coef_logit_intense |>
      mutate(term = recode(term, !!!term_labels)) |>
      ggplot(aes(x = estimate, y = reorder(term, estimate), color = direction)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 2.5) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      scale_color_manual(values = c("Positive"="#2166ac","Negative"="#d6604d","No clear effect"="grey60")) +
      facet_wrap(~dv, ncol = 2, scales = "free_x") +
      labs(x = "Average marginal effect (HC1 robust SEs)", y = NULL, color = NULL,
           title   = paste0(bat$title, " — Logit AME (intense preference)"),
           caption = "Logit AME. HC1 robust SEs. 95% CI.") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
    ggsave(paste0("graphs/regressions/coef_tradeoff_", bat_name, "_intense_logit_AME.png"),
           width = 14, height = 10, dpi = 300)
  }
  
  # ── Robustness: LPM vs Logit AME side by side ───────────────────────────
  
  rob_pref <- bind_rows(
    coef_tradeoff |> dplyr::filter(battery == bat_name, question == paste0(bat_name, "_pref_logit")) |> mutate(model = "Logit AME"),
    coef_tradeoff |> dplyr::filter(battery == bat_name, question == paste0(bat_name, "_pref_lpm"))   |> mutate(model = "LPM")
  )
  
  if (nrow(rob_pref) > 0) {
    rob_pref |>
      mutate(term = recode(term, !!!term_labels)) |>
      ggplot(aes(x = estimate, y = reorder(term, estimate),
                 color = model, shape = model)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     height = 0.2, position = position_dodge(width = 0.5)) +
      scale_color_manual(values = c("Logit AME" = "#d6604d", "LPM" = "#2166ac")) +
      facet_wrap(~dv, ncol = 2) +
      labs(x = "Estimated effect", y = NULL, color = NULL, shape = NULL,
           title   = paste0(bat$title, " — Logit AME vs. LPM (first choice, robustness)"),
           caption = "HC1 robust SEs. 95% CI.") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
    ggsave(paste0("graphs/regressions/coef_tradeoff_", bat_name, "_pref_logit_vs_lpm.png"),
           width = 14, height = 10, dpi = 300)
  }
  
  rob_intense <- bind_rows(
    coef_tradeoff |> dplyr::filter(battery == bat_name, question == paste0(bat_name, "_intense_logit")) |> mutate(model = "Logit AME"),
    coef_tradeoff |> dplyr::filter(battery == bat_name, question == paste0(bat_name, "_intense_lpm"))   |> mutate(model = "LPM")
  )
  
  if (nrow(rob_intense) > 0) {
    rob_intense |>
      mutate(term = recode(term, !!!term_labels)) |>
      ggplot(aes(x = estimate, y = reorder(term, estimate),
                 color = model, shape = model)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     height = 0.2, position = position_dodge(width = 0.5)) +
      scale_color_manual(values = c("Logit AME" = "#d6604d", "LPM" = "#2166ac")) +
      facet_wrap(~dv, ncol = 2) +
      labs(x = "Estimated effect", y = NULL, color = NULL, shape = NULL,
           title   = paste0(bat$title, " — Logit AME vs. LPM (intense preference, robustness)"),
           caption = "HC1 robust SEs. 95% CI.") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
    ggsave(paste0("graphs/regressions/coef_tradeoff_", bat_name, "_intense_logit_vs_lpm.png"),
           width = 14, height = 10, dpi = 300)
  }
})


#*************************************************************************#
#########   T7. R-SQUARED SUMMARY PLOTS                          #########
#*************************************************************************#

walk(names(batteries), function(bat_name) {
  bat <- batteries[[bat_name]]
  
  # Primary: Logit pseudo-R2
  fit_tradeoff |>
    dplyr::filter(battery == bat_name, model_type == "Logit_pref") |>
    plot_r2(r2_col    = "pseudo_r2",
            title_str = paste0(bat$title, " — Logit pseudo-R2 (first choice)"),
            file_path = paste0("graphs/regressions/r2_tradeoff_", bat_name, "_pref_logit.png"))
  
  fit_tradeoff |>
    dplyr::filter(battery == bat_name, model_type == "Logit_intense") |>
    plot_r2(r2_col    = "pseudo_r2",
            title_str = paste0(bat$title, " — Logit pseudo-R2 (intense preference)"),
            file_path = paste0("graphs/regressions/r2_tradeoff_", bat_name, "_intense_logit.png"))
  
  # Robustness: LPM adjusted R2
  fit_tradeoff |>
    dplyr::filter(battery == bat_name, model_type == "LPM_pref") |>
    plot_r2(r2_col    = "adj_r_sq",
            title_str = paste0(bat$title, " — LPM adjusted R2 (first choice, robustness)"),
            file_path = paste0("graphs/regressions/r2_tradeoff_", bat_name, "_pref_lpm.png"))
  
  fit_tradeoff |>
    dplyr::filter(battery == bat_name, model_type == "LPM_intense") |>
    plot_r2(r2_col    = "adj_r_sq",
            title_str = paste0(bat$title, " — LPM adjusted R2 (intense preference, robustness)"),
            file_path = paste0("graphs/regressions/r2_tradeoff_", bat_name, "_intense_lpm.png"))
})


#*************************************************************************#
#########   T8. DIAGNOSTICS                                      #########
#*************************************************************************#

cat("\n========== TRADEOFF MODEL FIT SUMMARY ==========\n")
print(fit_tradeoff, n = 60)

cat("\n========== Most consistently significant predictors (Logit AME _pref) ==========\n")
coef_tradeoff |>
  dplyr::filter(grepl("_pref_logit$", question), sig %in% c("*", "**", "***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

cat("\n========== Most consistently significant predictors (Logit AME _intense) ==========\n")
coef_tradeoff |>
  dplyr::filter(grepl("_intense_logit$", question), sig %in% c("*", "**", "***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

cat("\n========== LPM vs Logit direction agreement -- _pref ==========\n")
bind_rows(
  coef_tradeoff |> dplyr::filter(grepl("_pref_logit$", question)) |> mutate(model = "Logit"),
  coef_tradeoff |> dplyr::filter(grepl("_pref_lpm$",   question)) |> mutate(model = "LPM")
) |>
  dplyr::filter(sig %in% c("*", "**", "***")) |>
  dplyr::select(battery, dv, term, model, direction) |>
  pivot_wider(names_from = model, values_from = direction) |>
  dplyr::filter(LPM != Logit) |>
  print()

cat("\n========== LPM vs Logit direction agreement -- _intense ==========\n")
bind_rows(
  coef_tradeoff |> dplyr::filter(grepl("_intense_logit$", question)) |> mutate(model = "Logit"),
  coef_tradeoff |> dplyr::filter(grepl("_intense_lpm$",   question)) |> mutate(model = "LPM")
) |>
  dplyr::filter(sig %in% c("*", "**", "***")) |>
  dplyr::select(battery, dv, term, model, direction) |>
  pivot_wider(names_from = model, values_from = direction) |>
  dplyr::filter(LPM != Logit) |>
  print()

cat("\n========== AMEs saved to: graphs/regressions/AME_tradeoff_all.csv ==========\n")
cat("Model fit saved to: graphs/regressions/fit_tradeoff_all.csv\n")
# Quick count and % for all _intense variables
intense_vars <- df |> dplyr::select(matches("_intense$")) |> names()

check <- map_dfr(intense_vars, ~tibble(
  variable = .x,
  n        = sum(df[[.x]] == 1, na.rm = TRUE),
  pct      = round(mean(df[[.x]] == 1, na.rm = TRUE) * 100, 1),
  n_na     = sum(is.na(df[[.x]]))
)) |>
  arrange(pct)

#*************************************************************************#
#########   INT. REGRESSIONS -- INTENSE PREFERENCE (LOGIT + LPM) #########
#*************************************************************************#

# ── Define the 13 intense DVs ─────────────────────────────────────────────

intense_dvs <- c(
  "tradeoff_tax_wealth_tax_intense",
  "tradeoff_hc_pensions_intense",
  "soc_pol_intense",
  "cc_target_intense",
  "tradeoff_hc_all_intense",
  "hc_univ_intense",
  "tradeoff_tax_inc_tax_intense",
  "tradeoff_tax_less_services_intense",
  "cc_univ_intense",
  "tradeoff_hc_spend_intense",
  "tradeoff_cc1_no_spend_intense",
  "hc_target_intense",
  "tradeoff_ge_no_spend_intense"
)

intense_labels <- c(
  "tradeoff_tax_wealth_tax_intense"    = "Wealth tax (intense)",
  "tradeoff_hc_pensions_intense"       = "Target low-income seniors, pensions (intense)",
  "soc_pol_intense"                    = "Social policy composite (intense)",
  "cc_target_intense"                  = "Childcare: targeted (intense)",
  "tradeoff_hc_all_intense"            = "Home care for all seniors (intense)",
  "hc_univ_intense"                    = "Home care: universal (intense)",
  "tradeoff_tax_inc_tax_intense"       = "Income tax increase (intense)",
  "tradeoff_tax_less_services_intense" = "No tax increase, fewer services (intense)",
  "cc_univ_intense"                    = "Childcare: universal (intense)",
  "tradeoff_hc_spend_intense"          = "Target low-income seniors, home care (intense)",
  "tradeoff_cc1_no_spend_intense"      = "Don't spend more on childcare (intense)",
  "hc_target_intense"                  = "Home care: targeted (intense)",
  "tradeoff_ge_no_spend_intense"       = "Don't spend more on green economy (intense)"
)

# ── Fit LPM and Logit for all 13 DVs ─────────────────────────────────────

lpm_intense <- intense_dvs |>
  set_names(intense_labels[intense_dvs]) |>
  map(~lm(as.formula(paste(.x, "~", rhs)), data = df))

logit_intense <- intense_dvs |>
  set_names(intense_labels[intense_dvs]) |>
  map(~glm(as.formula(paste(.x, "~", rhs)), data = df,
           family = binomial(link = "logit")))

# ── Average marginal effects ──────────────────────────────────────────────

coef_lpm_intense <- map2_dfr(
  lpm_intense, intense_labels[intense_dvs],
  tidy_avg_slopes, question = "intense"
)

coef_logit_intense <- map2_dfr(
  logit_intense, intense_labels[intense_dvs],
  function(model, dv_label) {
    fml        <- formula(model)
    model_vars <- all.vars(fml)
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    vm         <- robust_vcov(model)
    avg_slopes(model, vcov = vm, newdata = model_data) |>
      as_tibble() |>
      transmute(
        question  = "intense", dv = dv_label, term,
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
          conf.low  > 0 ~ "Positive",
          conf.high < 0 ~ "Negative",
          TRUE          ~ "No clear effect"
        )
      )
  }
)

# ── Model fit ─────────────────────────────────────────────────────────────

fit_lpm_intense <- map2_dfr(
  lpm_intense, intense_labels[intense_dvs],
  function(model, dv_label) {
    s <- summary(model)
    tibble(dv = dv_label, model = "LPM",
           r_squared = round(s$r.squared,     3),
           adj_r_sq  = round(s$adj.r.squared, 3),
           n         = length(s$residuals))
  }
)

fit_logit_intense <- map2_dfr(
  logit_intense, intense_labels[intense_dvs],
  function(model, dv_label) {
    s <- summary.glm(model)
    tibble(dv = dv_label, model = "Logit",
           pseudo_r2 = round(1 - (s$deviance / s$null.deviance), 3),
           n         = length(s$residuals))
  }
)

# ── Regression tables ─────────────────────────────────────────────────────

modelsummary(
  lpm_intense,
  estimate  = "{estimate}{stars}", statistic = "({std.error})",
  vcov      = map(lpm_intense, robust_vcov),
  coef_map  = term_labels,
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  output    = "graphs/regressions/regtable_intense_LPM.txt",
  notes     = "LPM (OLS). HC1 robust SEs in parentheses. * p<0.05, ** p<0.01, *** p<0.001"
)

modelsummary(
  logit_intense,
  estimate  = "{estimate}{stars}", statistic = "({std.error})",
  vcov      = map(logit_intense, robust_vcov),
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = "graphs/regressions/regtable_intense_logit_coefs.txt",
  notes     = "Logit coefficients (log-odds). HC1 robust SEs. Interpret via AME table."
)

modelsummary(
  map(logit_intense, ~avg_slopes(.x, vcov = robust_vcov(.x))),
  estimate  = "{estimate}{stars}", statistic = "({std.error})",
  coef_map  = term_labels,
  gof_map   = c("nobs"),
  output    = "graphs/regressions/regtable_intense_logit_AME.txt",
  notes     = "Average marginal effects from logit. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001"
)

# ── Coefficient plots ─────────────────────────────────────────────────────

plot_coefs(
  coef_lpm_intense,
  "Intense preference -- LPM coefficients",
  "graphs/regressions/coef_intense_LPM.png"
)

plot_coefs(
  coef_logit_intense,
  "Intense preference -- Logit average marginal effects",
  "graphs/regressions/coef_intense_logit_AME.png"
)

# ── Robustness check: LPM vs Logit AME side by side ──────────────────────

bind_rows(
  coef_lpm_intense    |> mutate(model = "LPM"),
  coef_logit_intense  |> mutate(model = "Logit AME")
) |>
  mutate(term = recode(term, !!!term_labels)) |>
  ggplot(aes(x = estimate, y = reorder(term, estimate),
             color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("LPM" = "#2166ac", "Logit AME" = "#d6604d")) +
  facet_wrap(~dv, ncol = 3) +
  labs(x = "Estimated effect", y = NULL, color = NULL, shape = NULL,
       title   = "Intense preference -- LPM vs. Logit AME (robustness check)",
       caption = "HC1 robust SEs. 95% CI.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave("graphs/regressions/coef_intense_LPM_vs_logit.png",
       width = 16, height = 14, dpi = 300)

# ── R-squared summary plots ───────────────────────────────────────────────

plot_r2(
  fit_lpm_intense,
  r2_col    = "adj_r_sq",
  title_str = "Model fit -- Intense preference (LPM)",
  file_path = "graphs/regressions/r2_intense_LPM.png"
)

plot_r2(
  fit_logit_intense,
  r2_col    = "pseudo_r2",
  title_str = "Model fit -- Intense preference (Logit pseudo-R2)",
  file_path = "graphs/regressions/r2_intense_logit.png"
)

# ── Direction agreement check ─────────────────────────────────────────────

cat("\n========== LPM vs Logit direction agreement -- intense preference ==========\n")
bind_rows(
  coef_lpm_intense   |> mutate(model = "LPM"),
  coef_logit_intense |> mutate(model = "Logit")
) |>
  filter(sig %in% c("*", "**", "***")) |>
  dplyr::select(dv, term, model, direction) |>
  pivot_wider(names_from = model, values_from = direction) |>
  filter(LPM != Logit) |>
  print()

# ── Save AMEs to CSV ──────────────────────────────────────────────────────

bind_rows(
  coef_lpm_intense   |> mutate(model = "LPM"),
  coef_logit_intense |> mutate(model = "Logit AME")
) |>
  write.csv("graphs/regressions/AME_intense_all.csv", row.names = FALSE)


#*************************************************************************#
#########   UC. LOGIT REGRESSIONS -- HC & CC2 UNIVERSAL/TARGETED #########
#*************************************************************************#

# ── Define DVs ────────────────────────────────────────────────────────────

uc_dvs <- list(
  bin = c(
    "hc_univ_bin"    = "Home care: universal (first choice)",
    "hc_target_bin"  = "Home care: targeted (first choice)",
    "cc_univ_bin"    = "Childcare: universal (first choice)",
    "cc_target_bin"  = "Childcare: targeted (first choice)"
  ),
  intense = c(
    "hc_univ_intense"    = "Home care: universal (intense)",
    "hc_target_intense"  = "Home care: targeted (intense)",
    "cc_univ_intense"    = "Childcare: universal (intense)",
    "cc_target_intense"  = "Childcare: targeted (intense)"
  )
)

# ── Fit models ────────────────────────────────────────────────────────────

# Logit -- primary
logit_uc_bin <- names(uc_dvs$bin) |>
  set_names(uc_dvs$bin) |>
  map(function(dv) {
    model_data <- df |>
      dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |>
      drop_na()
    glm(as.formula(paste(dv, "~", rhs)), data = model_data,
        family = binomial(link = "logit"))
  })

logit_uc_intense <- names(uc_dvs$intense) |>
  set_names(uc_dvs$intense) |>
  map(function(dv) {
    model_data <- df |>
      dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |>
      drop_na()
    glm(as.formula(paste(dv, "~", rhs)), data = model_data,
        family = binomial(link = "logit"))
  })

# LPM -- robustness
lpm_uc_bin <- names(uc_dvs$bin) |>
  set_names(uc_dvs$bin) |>
  map(function(dv) {
    model_data <- df |>
      dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |>
      drop_na()
    lm(as.formula(paste(dv, "~", rhs)), data = model_data)
  })

lpm_uc_intense <- names(uc_dvs$intense) |>
  set_names(uc_dvs$intense) |>
  map(function(dv) {
    model_data <- df |>
      dplyr::select(all_of(c(dv, all.vars(as.formula(paste("~", rhs)))))) |>
      drop_na()
    lm(as.formula(paste(dv, "~", rhs)), data = model_data)
  })

# ── Average marginal effects ──────────────────────────────────────────────

coef_logit_uc_bin <- map2_dfr(
  logit_uc_bin, uc_dvs$bin,
  function(model, dv_label) {
    fml        <- formula(model)
    model_vars <- all.vars(fml)
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    vm         <- robust_vcov(model)
    avg_slopes(model, vcov = vm, newdata = model_data) |>
      as_tibble() |>
      transmute(
        question  = "uc_bin", dv = dv_label, term,
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
          conf.low  > 0 ~ "Positive",
          conf.high < 0 ~ "Negative",
          TRUE          ~ "No clear effect"
        )
      )
  }
)

coef_logit_uc_intense <- map2_dfr(
  logit_uc_intense, uc_dvs$intense,
  function(model, dv_label) {
    fml        <- formula(model)
    model_vars <- all.vars(fml)
    model_data <- df |> dplyr::select(all_of(model_vars)) |> drop_na()
    vm         <- robust_vcov(model)
    avg_slopes(model, vcov = vm, newdata = model_data) |>
      as_tibble() |>
      transmute(
        question  = "uc_intense", dv = dv_label, term,
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
          conf.low  > 0 ~ "Positive",
          conf.high < 0 ~ "Negative",
          TRUE          ~ "No clear effect"
        )
      )
  }
)

coef_lpm_uc_bin <- map2_dfr(
  lpm_uc_bin, uc_dvs$bin,
  tidy_avg_slopes, question = "uc_bin_lpm"
)

coef_lpm_uc_intense <- map2_dfr(
  lpm_uc_intense, uc_dvs$intense,
  tidy_avg_slopes, question = "uc_intense_lpm"
)

# ── Model fit ─────────────────────────────────────────────────────────────

fit_logit_uc_bin <- map2_dfr(
  logit_uc_bin, uc_dvs$bin,
  function(model, dv_label) {
    s <- summary.glm(model)
    tibble(dv        = dv_label, model_type = "Logit_bin",
           pseudo_r2 = round(1 - (s$deviance / s$null.deviance), 3),
           n         = length(s$residuals))
  }
)

fit_logit_uc_intense <- map2_dfr(
  logit_uc_intense, uc_dvs$intense,
  function(model, dv_label) {
    s <- summary.glm(model)
    tibble(dv        = dv_label, model_type = "Logit_intense",
           pseudo_r2 = round(1 - (s$deviance / s$null.deviance), 3),
           n         = length(s$residuals))
  }
)

fit_lpm_uc_bin <- map2_dfr(
  lpm_uc_bin, uc_dvs$bin,
  function(model, dv_label) {
    s <- summary(model)
    tibble(dv        = dv_label, model_type = "LPM_bin",
           adj_r_sq  = round(s$adj.r.squared, 3),
           n         = length(s$residuals))
  }
)

fit_lpm_uc_intense <- map2_dfr(
  lpm_uc_intense, uc_dvs$intense,
  function(model, dv_label) {
    s <- summary(model)
    tibble(dv        = dv_label, model_type = "LPM_intense",
           adj_r_sq  = round(s$adj.r.squared, 3),
           n         = length(s$residuals))
  }
)

# ── Regression tables ─────────────────────────────────────────────────────

modelsummary(
  map(logit_uc_bin, ~avg_slopes(.x, vcov = robust_vcov(.x))),
  estimate  = "{estimate}{stars}", statistic = "({std.error})",
  coef_map  = term_labels, gof_map = c("nobs"),
  output    = "graphs/regressions/regtable_uc_bin_logit_AME.txt",
  notes     = "HC & CC2 universal/targeted — Logit AME (first choice). HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001"
)

modelsummary(
  map(logit_uc_intense, ~avg_slopes(.x, vcov = robust_vcov(.x))),
  estimate  = "{estimate}{stars}", statistic = "({std.error})",
  coef_map  = term_labels, gof_map = c("nobs"),
  output    = "graphs/regressions/regtable_uc_intense_logit_AME.txt",
  notes     = "HC & CC2 universal/targeted — Logit AME (intense preference). HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001"
)

modelsummary(
  lpm_uc_bin,
  estimate  = "{estimate}{stars}", statistic = "({std.error})",
  vcov      = map(lpm_uc_bin, robust_vcov),
  coef_map  = term_labels, gof_map = c("nobs", "r.squared"),
  output    = "graphs/regressions/regtable_uc_bin_lpm.txt",
  notes     = "HC & CC2 universal/targeted — LPM robustness check (first choice). HC1 robust SEs."
)

modelsummary(
  lpm_uc_intense,
  estimate  = "{estimate}{stars}", statistic = "({std.error})",
  vcov      = map(lpm_uc_intense, robust_vcov),
  coef_map  = term_labels, gof_map = c("nobs", "r.squared"),
  output    = "graphs/regressions/regtable_uc_intense_lpm.txt",
  notes     = "HC & CC2 universal/targeted — LPM robustness check (intense preference). HC1 robust SEs."
)

# ── Coefficient plots -- primary (Logit AME) ──────────────────────────────

plot_coefs(
  coef_logit_uc_bin,
  "HC & CC2 universal/targeted — Logit AME (first choice)",
  "graphs/regressions/coef_uc_bin_logit_AME.png"
)

plot_coefs(
  coef_logit_uc_intense,
  "HC & CC2 universal/targeted — Logit AME (intense preference)",
  "graphs/regressions/coef_uc_intense_logit_AME.png"
)

# ── Robustness check: LPM vs Logit AME ───────────────────────────────────

bind_rows(
  coef_logit_uc_bin |> mutate(model = "Logit AME"),
  coef_lpm_uc_bin   |> mutate(model = "LPM")
) |>
  mutate(term = recode(term, !!!term_labels)) |>
  ggplot(aes(x = estimate, y = reorder(term, estimate),
             color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Logit AME" = "#d6604d", "LPM" = "#2166ac")) +
  facet_wrap(~dv, ncol = 2) +
  labs(x = "Estimated effect", y = NULL, color = NULL, shape = NULL,
       title   = "HC & CC2 universal/targeted — Logit AME vs. LPM (first choice, robustness)",
       caption = "HC1 robust SEs. 95% CI.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

ggsave("graphs/regressions/coef_uc_bin_logit_vs_lpm.png",
       width = 14, height = 10, dpi = 300)

bind_rows(
  coef_logit_uc_intense |> mutate(model = "Logit AME"),
  coef_lpm_uc_intense   |> mutate(model = "LPM")
) |>
  mutate(term = recode(term, !!!term_labels)) |>
  ggplot(aes(x = estimate, y = reorder(term, estimate),
             color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Logit AME" = "#d6604d", "LPM" = "#2166ac")) +
  facet_wrap(~dv, ncol = 2) +
  labs(x = "Estimated effect", y = NULL, color = NULL, shape = NULL,
       title   = "HC & CC2 universal/targeted — Logit AME vs. LPM (intense preference, robustness)",
       caption = "HC1 robust SEs. 95% CI.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

ggsave("graphs/regressions/coef_uc_intense_logit_vs_lpm.png",
       width = 14, height = 10, dpi = 300)

# ── R-squared summary plots ───────────────────────────────────────────────

plot_r2(
  fit_logit_uc_bin,
  r2_col    = "pseudo_r2",
  title_str = "Model fit — HC & CC2 universal/targeted, Logit pseudo-R2 (first choice)",
  file_path = "graphs/regressions/r2_uc_bin_logit.png"
)

plot_r2(
  fit_logit_uc_intense,
  r2_col    = "pseudo_r2",
  title_str = "Model fit — HC & CC2 universal/targeted, Logit pseudo-R2 (intense preference)",
  file_path = "graphs/regressions/r2_uc_intense_logit.png"
)

# ── Direction agreement check ─────────────────────────────────────────────

cat("\n========== LPM vs Logit direction agreement -- UC bin ==========\n")
bind_rows(
  coef_logit_uc_bin |> mutate(model = "Logit"),
  coef_lpm_uc_bin   |> mutate(model = "LPM")
) |>
  dplyr::filter(sig %in% c("*", "**", "***")) |>
  dplyr::select(dv, term, model, direction) |>
  pivot_wider(names_from = model, values_from = direction) |>
  dplyr::filter(LPM != Logit) |>
  print()

cat("\n========== LPM vs Logit direction agreement -- UC intense ==========\n")
bind_rows(
  coef_logit_uc_intense |> mutate(model = "Logit"),
  coef_lpm_uc_intense   |> mutate(model = "LPM")
) |>
  dplyr::filter(sig %in% c("*", "**", "***")) |>
  dplyr::select(dv, term, model, direction) |>
  pivot_wider(names_from = model, values_from = direction) |>
  dplyr::filter(LPM != Logit) |>
  print()

# ── Save AMEs to CSV ──────────────────────────────────────────────────────

bind_rows(
  coef_logit_uc_bin     |> mutate(model = "Logit AME"),
  coef_logit_uc_intense |> mutate(model = "Logit AME"),
  coef_lpm_uc_bin       |> mutate(model = "LPM"),
  coef_lpm_uc_intense   |> mutate(model = "LPM")
) |>
  write.csv("graphs/regressions/AME_uc_all.csv", row.names = FALSE)
