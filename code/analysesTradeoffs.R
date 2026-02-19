# ============================================================
# Budgetary Preferences — Marginal Means + Regressions
# Four-region Canadian survey: Alberta, Ontario, Quebec, Eastern Canada
# ============================================================

# 0.1. Load packages
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(marginaleffects)
library(modelsummary)
library(psych)
library(sandwich)
library(lmtest)
library(ggthemes)

# 0.2. Load data
df <- read.csv("data/clean_df_valid.csv")

#*************************************************************************#
#########              1. VARIABLE CONSTRUCTION                   #########
#*************************************************************************#

# ── 1.1. Binary from educ_group ──────────────────────────────
# Create a university-education binary (reference = Below University)
df$univ_educ_bin <- as.integer(df$educ_group == "educUniv")

# ── 1.2. Factorise factor-type IVs ──────────────────────────
df$educ_group     <- factor(df$educ_group,     levels = c("educBHS", "educHS", "educUniv"))
df$ses_region_cat <- factor(df$ses_region_cat, levels = c("Ontario", "Quebec", "Alberta", "East Coast"))
df$ses_income3Cat <- factor(df$ses_income3Cat, levels = c("Low", "Mid", "High"))

# ── 1.3. Convenience binary for high income ──────────────────
# Create incomeHigh_bin if not already in data (High vs Low+Mid)
if (!"incomeHigh_bin" %in% names(df)) {
  df$incomeHigh_bin <- as.integer(df$ses_income3Cat == "High")
}


#*************************************************************************#
#########              2. DEFINE IVs AND MODEL RHS               #########
#*************************************************************************#

# ── 2.1. IV list for bivariate marginal means ────────────────
# Each entry defines the variable, its low/high contrast values, and a readable label.
ivs <- list(
  list(type = "binary",  var = "quebec_bin",          low = 0,           high = 1,
       label = "Quebec (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ideo_define_QC_first_bin",          low = 0,           high = 1,
       label = "Quebec ID (No=0 vs Yes=1)"), 
  list(type = "binary",  var = "alberta_bin",              low = 0,           high = 1,
       label = "Alberta (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ontario_bin",             low = 0,           high = 1,
       label = "Ontario (No=0 vs Yes=1)"),
  list(type = "binary",  var = "region_eastcoast_bin",              low = 0,           high = 1,
       label = "Eastern Canada (No=0 vs Yes=1)"),
  list(type = "binary",  var = "incomeHigh_bin",      low = 0,           high = 1,
       label = "High Income (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ses_male_bin",        low = 0,           high = 1,
       label = "Gender (Female=0 vs Male=1)"),
  list(type = "binary",  var = "age18_34_bin",        low = 0,           high = 1,
       label = "Age 18–34 (No=0 vs Yes=1)"),
  list(type = "binary",  var = "age55plus_bin",       low = 0,           high = 1,
       label = "Age 55+ (No=0 vs Yes=1)"),
  list(type = "binary",  var = "univ_educ_bin",       low = 0,           high = 1,
       label = "University Education (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ses_citizenYes_bin",  low = 0,           high = 1,
       label = "Citizen (No=0 vs Yes=1)"),
  list(type = "binary",  var = "ses_french_bin",      low = 0,           high = 1,
       label = "French-speaking (No=0 vs Yes=1)"),
  list(type = "binary",  var = "trust_social_bin",    low = 0,           high = 1,
       label = "Social Trust (Low=0 vs High=1)"),
  list(type = "binary",  var = "employ_fulltime_bin", low = 0,           high = 1,
       label = "Employed Full-Time (No=0 vs Yes=1)"),
  list(type = "numeric", var = "ideo_right_num",      low = 0,           high = 1,
       label = "Ideology: Left (0) vs Right (1)"),
  list(type = "binary",  var = "vote_PLC_bin",        low = 0,           high = 1,
       label = "Liberal voter (No=0 vs Yes=1)"),
  list(type = "binary",  var = "vote_PCC_bin",        low = 0,           high = 1,
       label = "Conservative voter (No=0 vs Yes=1)")
)

# ── 2.2. RHS formula for multivariate regressions ────────────
# Note: ses_region_cat subsumes the individual region binaries and quebec_bin —
# include either the categorical region OR the individual binaries, not both,
# to avoid perfect multicollinearity. The line below uses individual binaries
# so each region is interpretable against Ontario (the omitted reference).
# Swap in ses_region_cat if you prefer a single factor variable.

rhs <- "quebec_bin + ideo_define_QC_first_bin + alberta_bin + region_eastcoast_bin +
        incomeHigh_bin + ses_male_bin + age18_34_bin + age55plus_bin +
        univ_educ_bin + ses_citizenYes_bin + ses_french_bin +
        trust_social_bin + employ_fulltime_bin +
        ideo_right_num + vote_PLC_bin + vote_PCC_bin"

# ── 2.3. Term labels for coefficient plots ───────────────────
term_labels <- c(
  "quebec_bin"          = "Quebec",
  "ideo_define_QC_first_bin" = "Quebecker first",
  "alberta_bin"         = "Alberta",
  "region_eastcoast_bin"   = "Eastern Canada",
  "incomeHigh_bin"      = "High Income",
  "ses_male_bin"        = "Male",
  "age18_34_bin"        = "Age 18–34",
  "age55plus_bin"       = "Age 55+",
  "univ_educ_bin"       = "University Education",
  "ses_citizenYes_bin"  = "Citizen",
  "ses_french_bin"      = "French-speaking",
  "trust_social_bin"    = "Social Trust",
  "employ_fulltime_bin" = "Employed Full-Time",
  "ideo_right_num"      = "Ideology (Right)",
  "vote_PLC_bin"        = "Liberal voter",
  "vote_PCC_bin"        = "Conservative voter"
)


#*************************************************************************#
#########              3. DETECT DEPENDENT VARIABLES             #########
#*************************************************************************#

# Detect all budgetary DV columns — adjust the regex to match your naming convention
budget_vars <- df |>
  select(matches("^budget_")) |>
  names()

budget_df <- tibble(var = budget_vars) |>
  mutate(
    question = str_extract(var, "^budget_[^_]+") |> str_remove("budget_"),
    policy   = str_remove(var, "^budget_[^_]+_"),
    label    = paste0(question, ": ", policy)
  )

question_groups <- unique(budget_df$question)

dir.create("graphs", showWarnings = FALSE)


#*************************************************************************#
#########        4. HELPER FUNCTIONS (ROBUST STANDARD ERRORS)    #########
#*************************************************************************#

# ── 4.1. Robust variance-covariance (HC1, as in old project) ─
robust_vcov <- function(model) vcovHC(model, type = "HC1")

# ── 4.2. Bivariate marginal means with robust SEs ─────────────
interpret_contrast <- function(dv, dv_label, iv, data) {
  iv_label <- iv$label
  iv_var   <- iv$var
  iv_low   <- as.character(iv$low)
  iv_high  <- as.character(iv$high)
  
  model_data <- data |> select(all_of(c(dv, iv_var))) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", iv_var)), data = model_data)
  
  contrast_spec        <- list(c(iv$low, iv$high))
  names(contrast_spec) <- iv_var
  
  tryCatch({
    avg_comparisons(model,
                    variables  = contrast_spec,
                    vcov       = robust_vcov(model)) |>
      as_tibble() |>
      transmute(
        iv        = iv_label,
        dv        = dv_label,
        contrast  = paste(iv_low, "vs", iv_high),
        estimate  = round(estimate, 3),
        conf.low  = round(conf.low, 3),
        conf.high = round(conf.high, 3),
        p.value   = round(p.value, 3),
        sig       = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          p.value < 0.10  ~ ".",
          TRUE            ~ ""
        ),
        direction = case_when(
          conf.low > 0  ~ paste(iv_high, ">", iv_low),
          conf.high < 0 ~ paste(iv_low, ">", iv_high),
          TRUE          ~ "No clear difference"
        )
      )
  }, error = function(e) {
    cat("FAILED:", iv_var, "~", dv, "\n  Error:", conditionMessage(e), "\n")
    tibble(
      iv = iv_label, dv = dv_label, contrast = paste(iv_low, "vs", iv_high),
      estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
      p.value = NA_real_, sig = NA_character_, direction = "ERROR — see console"
    )
  })
}

# ── 4.3. Multivariate avg_slopes with robust SEs ─────────────
tidy_avg_slopes <- function(model, dv_label, question = NULL) {
  model_vars  <- all.vars(formula(model))
  model_data  <- df |> select(all_of(model_vars)) |> drop_na()
  model_clean <- lm(formula(model), data = model_data)
  vm          <- robust_vcov(model_clean)
  
  avg_slopes(model_clean, vcov = vm) |>
    as_tibble() |>
    transmute(
      question  = question,
      dv        = dv_label,
      term      = case_when(
        term == "educ_group"     & contrast == "educHS - educBHS"   ~ "Education: HS vs Below HS",
        term == "educ_group"     & contrast == "educUniv - educBHS" ~ "Education: Univ vs Below HS",
        term == "ses_income3Cat" & contrast == "Mid - Low"          ~ "Income: Mid vs Low",
        term == "ses_income3Cat" & contrast == "High - Low"         ~ "Income: High vs Low",
        TRUE ~ term
      ),
      estimate  = round(estimate, 3),
      conf.low  = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value   = round(p.value, 3),
      sig       = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.10  ~ ".",
        TRUE            ~ ""
      ),
      direction = case_when(
        conf.low > 0  ~ "Positive",
        conf.high < 0 ~ "Negative",
        TRUE          ~ "No clear effect"
      )
    )
}


#*************************************************************************#
#########   5. SECTION A — BIVARIATE MARGINAL MEANS               #########
#*************************************************************************#
# One bivariate model per IV × DV pair, with robust SEs.

budget_marginal <- map_dfr(ivs, function(iv) {
  map2_dfr(budget_df$var, budget_df$label, interpret_contrast, iv = iv, data = df)
})

budget_marginal

# Save to CSV for inspection
write.csv(budget_marginal, "graphs/marginal_means_budgetary.csv", row.names = FALSE)


#*************************************************************************#
#########   6. SECTION B — REGRESSIONS PER POLICY OPTION         #########
#*************************************************************************#
# One full multivariate OLS model per DV, with HC1 robust SEs.

models_per_policy <- budget_df$var |>
  set_names() |>
  map(~ lm(as.formula(paste(.x, "~", rhs)), data = df))

# ── 6.1. Coefficient table (avg marginal effects, robust SEs) ─
coef_per_policy <- map2_dfr(
  models_per_policy,
  budget_df$label,
  tidy_avg_slopes
)

# ── 6.2. Model fit summary ────────────────────────────────────
fit_per_policy <- map2_dfr(models_per_policy, budget_df$label, function(model, dv_label) {
  s <- summary(model)
  tibble(
    dv        = dv_label,
    question  = str_extract(dv_label, "^[^:]+"),
    r_squared = round(s$r.squared, 3),
    adj_r_sq  = round(s$adj.r.squared, 3),
    n         = nobs(model)
  )
})

fit_per_policy

# ── 6.3. Regression tables via modelsummary (robust SEs) ─────
# Group models by question for cleaner tables
walk(question_groups, function(q) {
  group_vars   <- budget_df |> filter(question == q) |> pull(var)
  group_labels <- budget_df |> filter(question == q) |> pull(label)
  group_models <- models_per_policy[group_vars]
  names(group_models) <- group_labels
  
  # Build robust SE list
  rob_se <- map(group_models, ~ sqrt(diag(robust_vcov(.x))))
  
  modelsummary(
    group_models,
    estimate   = "{estimate}{stars}",
    statistic  = "({std.error})",
    vcov       = map(group_models, robust_vcov),
    coef_map   = term_labels,
    gof_map    = c("nobs", "r.squared", "adj.r.squared"),
    output     = paste0("graphs/regtable_", q, ".txt"),
    notes      = paste("OLS with HC1 robust standard errors in parentheses.",
                       "* p<0.05, ** p<0.01, *** p<0.001")
  )
})


#*************************************************************************#
#########   7. SECTION C — REGRESSIONS PER QUESTION GROUP        #########
#*************************************************************************#

models_per_group <- map(question_groups, function(q) {
  group_vars <- budget_df |> filter(question == q) |> pull(var)
  group_labs <- budget_df |> filter(question == q) |> pull(label)
  
  map2(group_vars, group_labs, function(dv, dv_label) {
    model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> select(all_of(model_vars)) |> drop_na()
    list(
      dv_label = dv_label,
      question = q,
      model    = lm(as.formula(paste(dv, "~", rhs)), data = model_data)
    )
  })
}) |>
  set_names(question_groups) |>
  list_flatten()

# ── 7.1. Coefficient table by group ──────────────────────────
coef_per_group <- map_dfr(models_per_group, function(m) {
  tidy_avg_slopes(m$model, m$dv_label, m$question)
})

# ── 7.2. Model fit by group ───────────────────────────────────
fit_per_group <- map_dfr(models_per_group, function(m) {
  s <- summary(m$model)
  tibble(
    question  = m$question,
    dv        = m$dv_label,
    r_squared = round(s$r.squared, 3),
    adj_r_sq  = round(s$adj.r.squared, 3),
    n         = nobs(m$model)
  )
})

fit_per_group


#*************************************************************************#
#########   8. SECTION D — COEFFICIENT PLOTS PER QUESTION GROUP  #########
#*************************************************************************#

walk(question_groups, function(q) {
  coef_per_group |>
    filter(question == q) |>
    mutate(term = recode(term, !!!term_labels)) |>
    ggplot(aes(x = estimate, y = reorder(term, estimate), color = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    scale_color_manual(values = c(
      "Positive"       = "#2166ac",
      "Negative"       = "#d6604d",
      "No clear effect"= "grey60"
    )) +
    facet_wrap(~ dv) +
    labs(
      x     = "Estimated effect (OLS, HC1 robust SEs)",
      y     = NULL,
      color = NULL,
      title = paste("Budgetary preferences —", toupper(q))
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  ggsave(
    paste0("graphs/coef_plot_budget_", q, ".png"),
    width = 12, height = 8, dpi = 300
  )
})


#*************************************************************************#
#########   9. SECTION E — R² SUMMARY PLOT (ALL MODELS)          #########
#*************************************************************************#

fit_per_policy |>
  mutate(
    policy    = str_remove(dv, "^[^:]+: "),
    question  = toupper(question),
    fit_level = case_when(
      adj_r_sq >= 0.07 ~ "High (≥0.07)",
      adj_r_sq >= 0.03 ~ "Moderate (0.03–0.07)",
      TRUE             ~ "Low (<0.03)"
    ),
    fit_level = factor(fit_level, levels = c("High (≥0.07)", "Moderate (0.03–0.07)", "Low (<0.03)"))
  ) |>
  ggplot(aes(x = adj_r_sq, y = reorder(policy, adj_r_sq), fill = fit_level)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.3f", adj_r_sq)), hjust = -0.15, size = 3.2) +
  scale_fill_manual(values = c(
    "High (≥0.07)"         = "#2166ac",
    "Moderate (0.03–0.07)" = "#92c5de",
    "Low (<0.03)"          = "#d6604d"
  )) +
  scale_x_continuous(
    limits = c(0, 0.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  facet_wrap(~ question, scales = "free_y", ncol = 1) +
  labs(
    x        = "Adjusted R²",
    y        = NULL,
    fill     = "Model fit",
    title    = "Model fit by budgetary preference",
    subtitle = "Higher R² indicates stronger sociodemographic / ideological structuring of preference"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "bottom",
    strip.text         = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

ggsave("graphs/r2_summary_budget.png", width = 8, height = 12, dpi = 300)


#*************************************************************************#
#########   10. SECTION F — MARGINAL EFFECTS PLOTS (KEY IVs)     #########
#*************************************************************************#
# Visualize predicted values across the range of ideology, by Quebec vs. rest —
# mirrors the income × Quebec interaction approach from the old project.

walk(budget_df$var, function(dv) {
  model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
  model_data <- df |> select(all_of(model_vars)) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", rhs)), data = model_data)
  dv_label   <- budget_df |> filter(var == dv) |> pull(label)
  
  # Marginal effect of ideology across Quebec vs. non-Quebec
  me <- ggpredict(model, terms = c("ideo_right_num [all]", "quebec_bin [0,1]"))
  
  ggplot(me, aes(x = x, y = predicted, colour = as.factor(group),
                 fill = as.factor(group))) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
    scale_colour_manual(values = c("0" = "#d6604d", "1" = "#2166ac"),
                        labels = c("0" = "Rest of Canada", "1" = "Quebec")) +
    scale_fill_manual(  values = c("0" = "#d6604d", "1" = "#2166ac"),
                        labels = c("0" = "Rest of Canada", "1" = "Quebec")) +
    labs(
      x      = "Ideology (Left → Right)",
      y      = "Predicted support",
      colour = NULL, fill = NULL,
      title  = paste("Marginal effect of ideology —", dv_label),
      caption = "OLS with HC1 robust SEs. Shaded area = 95% CI."
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  ggsave(
    paste0("graphs/me_ideology_", dv, ".png"),
    width = 8, height = 5, dpi = 300
  )
})


#*************************************************************************#
#########   11. QUICK DIAGNOSTICS                                 #########
#*************************************************************************#

# Which predictors are most consistently significant across all models?
coef_per_policy |>
  filter(sig %in% c("*", "**", "***")) |>
  count(term, sort = TRUE) |>
  print(n = 20)

# Which DVs have the highest adjusted R²?
fit_per_policy |>
  arrange(desc(adj_r_sq)) |>
  print(n = 20)

# Strong predictors within high-R² models
coef_per_policy |>
  filter(dv %in% (fit_per_policy |> filter(adj_r_sq >= 0.07) |> pull(dv))) |>
  filter(sig %in% c("*", "**", "***")) |>
  arrange(dv, desc(abs(estimate))) |>
  print(n = 40)

