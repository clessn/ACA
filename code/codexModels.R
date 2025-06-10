# ===================================================
# Binary Logistic Regression Models and Plots (codexModels.R)
# ===================================================
# Version: June 10th, 2025

# -- 1. Load packages
library(tidyverse)
library(modelsummary)
library(broom)
library(ggplot2)

# -- 2. Load data
df <- read.csv("data/ACA_weighted.csv")

# -- 3. Define dependent variables (binary outcomes) and predictors
dvs <- c(
  "budget_health_priority_bin",
  "budget_education_priority_bin",
  "budget_pensions_priority_bin",
  "budget_taxes_priority_bin",
  "budget_debt_priority_bin"
)

ivs <- c(
  "ideo_right_num.",
  "ses_male_bin",
  "age",
  "education",
  "incomeMid_bin"
)

# -- 4. Fit logistic regression models with survey weights
models <- map(dvs, function(dv) {
  frm <- as.formula(paste(dv, "~", paste(ivs, collapse = " + ")))
  glm(frm, data = df, family = binomial(), weights = weightvec)
})
names(models) <- dvs

# -- 5. Create and save a summary table of coefficients (p-values and significance stars)
modelsummary(
  models,
  statistic = "p.value",
  stars = TRUE,
  coef_rename = c(ideo_right_num. = "Ideology"),
  output = "html",
  file = "modelsummary/codex_models.html"
)

# -- 6. Tidy model outputs and combine for plotting
coef_df <- map_df(models, ~ tidy(.x, conf.int = TRUE), .id = "outcome")

# -- 7. Plot coefficient estimates for all models
coef_plot <- coef_df %>%
  filter(term != "(Intercept)") %>%
  mutate(outcome = factor(outcome, levels = dvs)) %>%
  ggplot(aes(x = estimate, y = term, color = outcome)) +
  geom_point(position = position_dodge(width = 0.7)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0) +
  labs(
    x = "Coefficient Estimate",
    y = "Predictor",
    color = "Outcome",
    title = "Logistic Regression Coefficients for Selected Binary Outcomes"
  ) +
  theme_minimal()

# -- 8. Save the coefficient plot
ggsave(
  filename = "graphs/codex_models_coefficients.png",
  plot = coef_plot,
  width = 8,
  height = 6,
  dpi = 300
)
