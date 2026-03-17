# ============================================================
# Marginal Means Analysis — Redistribution
# ============================================================

library(tidyverse)
library(marginaleffects)

df <- read.csv("data/clean_df_valid.csv")
df$ses_income3Cat

tab <- table(
  redis_intelligence = df$redis_intelligence_num,
  income_category   = df$ses_income3Cat
)
tab

tab <- table(
  redis_intelligence = df$redis_effort_num,
  income_category   = df$ses_income3Cat
)
tab

# ── 1. DVs ───────────────────────────────────────────────────
dvs <- c(
  "redis_effort_num",
  "redis_no_cheat_system_num",
  "redis_reasons_poor_num",
  "redis_reasons_rich_num",
  "redis_social_benefits_num",
  "redis_welfare_num"
)

dv_labels <- c(
  redis_effort_num          = "Fairness of income distribution",
  redis_no_cheat_system_num = "Trust not to cheat system",
  redis_reasons_poor_num    = "Non-violating outcomes (Poor)",
  redis_reasons_rich_num    = "Non-violating outcomes (Rich)",
  redis_social_benefits_num = "Social benefits not a choice",
  redis_welfare_num         = "Welfare does not go to undeserving"
)

# ── 2. IVs ───────────────────────────────────────────────────
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

# ── 3. Factorise factor-type IVs ─────────────────────────────
for (iv in ivs) {
  if (iv$type == "factor") {
    df[[iv$var]] <- factor(df[[iv$var]], levels = iv$levels)
  }
}

# ── 4. Plot function ──────────────────────────────────────────
plot_marginal <- function(dv, dv_label, iv, data) {
  
  model_data <- data |> select(all_of(c(dv, iv$var))) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", iv$var)), data = model_data)
  
  plot_data <- if (iv$type == "numeric") {
    nd <- data.frame(x = c(iv$low, iv$high))
    names(nd) <- iv$var
    predictions(model, newdata = nd) |>
      mutate(x_label = factor(
        .data[[iv$var]],
        levels = c(iv$low, iv$high),
        labels = c(as.character(iv$low), as.character(iv$high))
      ))
    
  } else if (iv$type == "binary") {
    nd <- data.frame(x = c(0, 1))
    names(nd) <- iv$var
    predictions(model, newdata = nd) |>
      mutate(x_label = factor(
        .data[[iv$var]],
        levels = c(0, 1),
        labels = c(as.character(iv$low), as.character(iv$high))
      ))
    
  } else {
    predictions(model, by = iv$var) |>
      mutate(x_label = .data[[iv$var]])
  }
  
  p <- ggplot(plot_data, aes(x = x_label, y = estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.12) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      x     = iv$label,
      y     = "Predicted support (0–1)",
      title = dv_label
    ) +
    theme_minimal(base_size = 13)
  
  dir.create("graphs", showWarnings = FALSE)
  ggsave(
    filename = paste0("graphs/marginal_", iv$var, "_", dv, ".png"),
    plot = p, width = 6, height = 4, dpi = 300
  )
  
  p
}

# ── 5. Contrast table function ────────────────────────────────
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

# ── 6. Run marginal means ─────────────────────────────────────
walk(ivs, function(iv) {
  walk2(dvs, dv_labels, plot_marginal, iv = iv, data = df)
})

results <- map_dfr(ivs, function(iv) {
  map2_dfr(dvs, dv_labels, interpret_contrast, iv = iv, data = df)
})

results

# ── 7. Regressions ───────────────────────────────────────────
rhs <- "ses_male_bin + ses_age + quebec_bin + region_eastcoast_bin +
        educ_group + ses_income3Cat + ses_citizenYes_bin + employ_fulltime_bin +
        ideo_right_num + ideo_interest_politics_num + ideo_define_QC_first_bin"

models <- dvs |>
  set_names() |>
  map(~ lm(as.formula(paste(.x, "~", rhs)), data = df))

# ── 8. Coefficient table — with fixed factor term labels ─────
coef_table <- map2_dfr(models, dv_labels, function(model, dv_label) {
  model_vars  <- all.vars(formula(model))
  model_data  <- df |> select(all_of(model_vars)) |> drop_na()
  model_clean <- lm(formula(model), data = model_data)
  
  avg_slopes(model_clean) |>
    as_tibble() |>
    transmute(
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
})

coef_table

# ── 9. Model fit ──────────────────────────────────────────────
fit_summary <- map2_dfr(models, dv_labels, function(model, dv_label) {
  s <- summary(model)
  tibble(
    dv        = dv_label,
    r_squared = round(s$r.squared, 3),
    adj_r_sq  = round(s$adj.r.squared, 3),
    n         = nobs(model)
  )
})

fit_summary

# ── 10. Coefficient plot ──────────────────────────────────────
coef_table |>
  mutate(term = recode(term,
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
  )) |>
  ggplot(aes(x = estimate, y = reorder(term, estimate), color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  scale_color_manual(values = c(
    "Positive"       = "#2166ac",
    "Negative"       = "#d6604d",
    "No clear effect"= "grey60"
  )) +
  facet_wrap(~ dv, ncol = 2) +
  labs(
    x     = "Estimated effect (OLS coefficient)",
    y     = NULL,
    color = NULL,
    title = "Predictors of redistribution attitudes"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("graphs/coef_plot_redis_all_dvs.png", width = 12, height = 10, dpi = 300)
