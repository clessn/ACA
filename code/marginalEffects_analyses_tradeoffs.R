# ============================================================
# Tradeoff Intense Variables — Marginal Means + Regressions
# ============================================================

library(tidyverse)
library(marginaleffects)

# ── Load data ─────────────────────────────────────────────────
df <- read.csv("data/clean_df_valid.csv")

# ── Factorise factor-type IVs ────────────────────────────────
df$educ_group     <- factor(df$educ_group,     levels = c("educBHS", "educHS", "educUniv"))
df$ses_region_cat <- factor(df$ses_region_cat, levels = c("Ontario", "Quebec", "Alberta", "East Coast"))
df$ses_income3Cat <- factor(df$ses_income3Cat, levels = c("Low", "Mid", "High"))

# ── IVs ──────────────────────────────────────────────────────
ivs <- list(
  list(type="binary",  var="ses_male_bin",               low=0,         high=1,         label="Gender (Female=0 vs Male=1)"),
  list(type="numeric", var="ses_age",                    low=30,        high=60,        label="Age (30 vs 60)"),
  list(type="binary",  var="quebec_bin",                 low=0,         high=1,         label="Quebec (No=0 vs Yes=1)"),
  list(type="binary",  var="region_eastcoast_bin",       low=0,         high=1,         label="East Coast (No=0 vs Yes=1)"),
  list(type="factor",  var="ses_region_cat",             low="Alberta", high="Quebec",
       levels=c("Ontario","Quebec","Alberta","East Coast"),              label="Region (Alberta vs Quebec)"),
  list(type="factor",  var="educ_group",                 low="educBHS", high="educUniv",
       levels=c("educBHS","educHS","educUniv"),                          label="Education (Below HS vs University)"),
  list(type="factor",  var="ses_income3Cat",             low="Low",     high="High",
       levels=c("Low","Mid","High"),                                     label="Income (Low vs High)"),
  list(type="binary",  var="ses_citizenYes_bin",         low=0,         high=1,         label="Citizen (No=0 vs Yes=1)"),
  list(type="binary",  var="employ_fulltime_bin",        low=0,         high=1,         label="Employed Full-Time (No=0 vs Yes=1)"),
  list(type="numeric", var="ideo_right_num",             low=0,         high=1,         label="Ideology: Left (0) vs Right (1)"),
  list(type="numeric", var="ideo_interest_politics_num", low=0,         high=1,         label="Political Interest: Low (0) vs High (1)"),
  list(type="binary",  var="ideo_define_QC_first_bin",   low=0,         high=1,         label="Quebec Identity First (No=0 vs Yes=1)")
)

# ── RHS formula ───────────────────────────────────────────────
# ses_income3Cat included — remove if you prefer to exclude it
rhs <- "ses_male_bin + ses_age + quebec_bin + region_eastcoast_bin +
        educ_group + ses_income3Cat + ses_citizenYes_bin + employ_fulltime_bin +
        ideo_right_num + ideo_interest_politics_num + ideo_define_QC_first_bin"

# ── DVs — detect all tradeoff intense variables ───────────────
tradeoff_vars <- df |>
  select(matches("^tradeoff_.*_intense$")) |>
  names()

tradeoff_df <- tibble(var = tradeoff_vars) |>
  mutate(
    question = str_extract(var, "^tradeoff_[^_]+") |> str_remove("tradeoff_"),
    policy   = str_remove(var, "^tradeoff_[^_]+_") |> str_remove("_intense"),
    label    = paste0(question, ": ", policy)
  )

question_groups <- unique(tradeoff_df$question)

# ── Marginal means function ───────────────────────────────────
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
    avg_comparisons(model, variables = contrast_spec) |>
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
          conf.high < 0 ~ paste(iv_low,  ">", iv_high),
          TRUE          ~ "No clear difference"
        )
      )
  }, error = function(e) {
    cat("FAILED:", iv_var, "~", dv, "\n  Error:", conditionMessage(e), "\n")
    tibble(
      iv=iv_label, dv=dv_label, contrast=paste(iv_low,"vs",iv_high),
      estimate=NA_real_, conf.low=NA_real_, conf.high=NA_real_,
      p.value=NA_real_, sig=NA_character_, direction="ERROR — see console"
    )
  })
}

# ── Regression coefficient helper ────────────────────────────
tidy_avg_slopes <- function(model, dv_label, question = NULL) {
  model_vars  <- all.vars(formula(model))
  model_data  <- df |> select(all_of(model_vars)) |> drop_na()
  model_clean <- lm(formula(model), data = model_data)
  
  avg_slopes(model_clean) |>
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

# ── Term labels for plots ─────────────────────────────────────
term_labels <- c(
  "ses_male_bin"               = "Male",
  "ses_age"                    = "Age",
  "quebec_bin"                 = "Quebec",
  "region_eastcoast_bin"       = "East Coast",
  "ses_citizenYes_bin"         = "Citizen",
  "employ_fulltime_bin"        = "Employed Full-Time",
  "ideo_right_num"             = "Ideology (Right)",
  "ideo_interest_politics_num" = "Political Interest",
  "ideo_define_QC_first_bin"   = "Quebec Identity First",
  "Education: HS vs Below HS"  = "Education: HS",
  "Education: Univ vs Below HS"= "Education: Univ",
  "Income: Mid vs Low"         = "Income: Mid",
  "Income: High vs Low"        = "Income: High"
)

dir.create("graphs", showWarnings = FALSE)

# ════════════════════════════════════════════════════════════
# A. MARGINAL MEANS — bivariate, all IVs × all tradeoff DVs
# ════════════════════════════════════════════════════════════
tradeoff_marginal <- map_dfr(ivs, function(iv) {
  map2_dfr(tradeoff_df$var, tradeoff_df$label, interpret_contrast, iv = iv, data = df)
})

tradeoff_marginal

# ════════════════════════════════════════════════════════════
# B. REGRESSIONS — one model per policy option (19 models)
# ════════════════════════════════════════════════════════════
models_per_policy <- tradeoff_df$var |>
  set_names() |>
  map(~ lm(as.formula(paste(.x, "~", rhs)), data = df))

# Coefficient table
coef_per_policy <- map2_dfr(
  models_per_policy,
  tradeoff_df$label,
  tidy_avg_slopes
)

# Model fit
fit_per_policy <- map2_dfr(models_per_policy, tradeoff_df$label, function(model, dv_label) {
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

# ════════════════════════════════════════════════════════════
# C. REGRESSIONS — one per question group (cc1, ge, tax, hc, cc2)
# ════════════════════════════════════════════════════════════
models_per_group <- map(question_groups, function(q) {
  group_vars <- tradeoff_df |> filter(question == q) |> pull(var)
  group_labs <- tradeoff_df |> filter(question == q) |> pull(label)
  
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

# Coefficient table by group
coef_per_group <- map_dfr(models_per_group, function(m) {
  tidy_avg_slopes(m$model, m$dv_label, m$question)
})

# Model fit by group
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

# ════════════════════════════════════════════════════════════
# D. COEFFICIENT PLOTS — one PNG per question group
# ════════════════════════════════════════════════════════════
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
      x     = "Estimated effect (OLS coefficient)",
      y     = NULL,
      color = NULL,
      title = paste("Policy preferences —", toupper(q))
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  ggsave(
    paste0("graphs/coef_plot_tradeoff_", q, ".png"),
    width = 12, height = 8, dpi = 300
  )
})

# Interesting because low R2 except for less ervices and taxation -- just as elsewhere ideology
# appears to rule the day.

# Quick check
# Which predictors are driving the high R² models?
coef_per_policy |>
  filter(dv %in% c("cc1: no_spend", "ge: no_spend", "tax: less_services")) |>
  filter(sig %in% c("*", "**", "***")) |>
  arrange(dv, desc(abs(estimate)))

# ── R² Summary Plot — all 19 models ──────────────────────────
fit_per_policy |>
  mutate(
    # Clean up DV labels for the plot
    policy   = str_remove(dv, "^[^:]+: "),
    question = toupper(question),
    # Flag high vs low fit for colour
    fit_level = case_when(
      adj_r_sq >= 0.07 ~ "High (≥0.07)",
      adj_r_sq >= 0.03 ~ "Moderate (0.03–0.07)",
      TRUE             ~ "Low (<0.03)"
    ),
    fit_level = factor(fit_level, levels = c("High (≥0.07)", "Moderate (0.03–0.07)", "Low (<0.03)"))
  ) |>
  ggplot(aes(x = adj_r_sq, y = reorder(policy, adj_r_sq), fill = fit_level)) +
  geom_col() +
  geom_text(
    aes(label = sprintf("%.3f", adj_r_sq)),
    hjust = -0.15, size = 3.2
  ) +
  scale_fill_manual(values = c(
    "High (≥0.07)"         = "#2166ac",
    "Moderate (0.03–0.07)" = "#92c5de",
    "Low (<0.03)"          = "#d6604d"
  )) +
  scale_x_continuous(
    limits = c(0, 0.15),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  facet_wrap(~ question, scales = "free_y", ncol = 1) +
  labs(
    x     = "Adjusted R²",
    y     = NULL,
    fill  = "Model fit",
    title = "Model fit by policy preference",
    subtitle = "Higher R² indicates stronger ideological/sociodemographic structuring of preference"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

ggsave("graphs/r2_summary_tradeoff.png", width = 8, height = 12, dpi = 300)
