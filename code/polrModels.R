# ===================================================
# Models for SASE Conference (July 2025)
# ===================================================
# Version: June 9th, 2025

# -----------------------
# 1. Load packages
# -----------------------
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(modelsummary)
library(stringr)
library(purrr)
library(rlang)
library(MASS) # use MASS:polr() to avoid errors with tidyverse
library(effects)
library(ggeffects)
library(sjPlot)
library(patchwork)

# -----------------------
# 2. Load data
# -----------------------
df <- read.csv("data/ACA_weighted.csv") 

# -----------------------
# 3. Models
# -----------------------
# Budgetary priority question
# What predicts assigning a higher priority to this specific policy (e.g., health)?

# --- Convert numeric priority to ordered factors for ordinal logistic regression  ---
# Define the priority levels: 1 = highest, 0 lowest
ordered_levels <- c(1, 0.75, 0.5, 0.25, 0)

# Convert to ordered factor (safer and clearer)
df$budget_health_rank    <- ordered(df$budget_health_priority_num, levels = ordered_levels)
df$budget_education_rank <- ordered(df$budget_education_priority_num, levels = ordered_levels)
df$budget_pensions_rank  <- ordered(df$budget_pensions_priority_num, levels = ordered_levels)
df$budget_debt_rank      <- ordered(df$budget_debt_priority_num, levels = ordered_levels)
df$budget_taxes_rank     <- ordered(df$budget_taxes_priority_num, levels = ordered_levels)

# --- Create new dataframe to avoid issues with weights
df2 <- df  # copy original data frame
df2$budget_rank <- NA  # initialize column (optional)
df2$w <- df2$weightvec  # Add the weights as a column with a standard name

# --- Define formula (to avoid repetition) ---
# One formula to compare ranking across health, education, pensions, debt and taxes
formula_ord <- as.formula(
  budget_rank ~ ideo_right_num. + ideo_interest_politics_num + gender + age + education + income)

# --- Helper function
run_polr_model <- function(data, dv_var, formula) {
  data$budget_rank <- data[[dv_var]]
  
  model <- MASS::polr(
    formula = update(formula, budget_rank ~ .),
    data = data,
    weights = w,  # must match the column name
    Hess = TRUE
  )
  
  return(model)
}

# --- Run ordinal logistic regressions using the function ---
mod_ord_health <- run_polr_model(df2, "budget_health_rank", formula_ord)
mod_ord_edu    <- run_polr_model(df2, "budget_education_rank", formula_ord)
mod_ord_pensions <- run_polr_model(df2, "budget_pensions_rank", formula_ord)
mod_ord_debt   <- run_polr_model(df2, "budget_debt_rank", formula_ord)
mod_ord_taxes  <- run_polr_model(df2, "budget_taxes_rank", formula_ord)

# --- Runs models for debt
debt_ord_list <- list(
  "Health"    = mod_ord_health,
  "Education" = mod_ord_edu,
  "Pensions"  = mod_ord_pensions,
  "Debt"      = mod_ord_debt,
  "Taxes"     = mod_ord_taxes
)

# -- p-values in polr
tidy_polr_with_p <- function(model) {
  coef_table <- coef(summary(model))
  p_values <- 2 * (1 - pnorm(abs(coef_table[, "t value"])))
  tidy_df <- as.data.frame(coef_table)
  tidy_df$p.value <- p_values
  tidy_df$term <- rownames(tidy_df)
  rownames(tidy_df) <- NULL
  tidy_df <- tidy_df[, c("term", "Estimate", "Std. Error", "p.value")]
  colnames(tidy_df) <- c("term", "estimate", "std.error", "p.value")
  return(tidy_df)
}

# --- Store models in a named list ---
priority_ord_list <- list(
  "Health"    = mod_ord_health,
  "Education" = mod_ord_edu,
  "Pensions"  = mod_ord_pensions,
  "Debt"      = mod_ord_debt,
  "Taxes"     = mod_ord_taxes
)

# --- Create coefficient summary table ---
model_tbl <- modelsummary(
  priority_ord_list,
  tidy = tidy_polr_with_p,
  coef_rename = c(
    ideo_right_num.             = "Right",
    ideo_interest_politics_num  = "Political interest"
  ),
  stars = TRUE,              # this works when p.value is present
  statistic = "p.value",     # this tells modelsummary to show p-values
  intercept = TRUE,
  output = "html",
  digits = 2,
  file = "test_mod.html"
)

# --- Comparing simple & multivariate models for debt and taxes individually
formula_ord_1 <- as.formula(
  budget_rank ~ ideo_right_num.)

formula_ord_2 <- as.formula(
  budget_rank ~ ideo_right_num. + gender + age + education + income)

formula_ord_3 <- as.formula(
  budget_rank ~ ideo_party_them + gender + age + education + income)

# --- Debt models
mod_debt_1   <- run_polr_model(df2, "budget_debt_rank", formula_ord_1)
mod_debt_2   <- run_polr_model(df2, "budget_debt_rank", formula_ord_2)
mod_debt_3   <- run_polr_model(df2, "budget_debt_rank", formula_ord_3)

# --- Store models in a named list ---
priority_debt_list <- list(
  "(1)"    = mod_debt_1,
  "(2)"    = mod_debt_2,
  "(3)"    = mod_debt_3
)

# --- Create coefficient summary table ---
model_debt <- modelsummary(
  priority_debt_list,
  tidy = tidy_polr_with_p,
  coef_rename = c(
    ideo_right_num.             = "Right",
    ideo_interest_politics_num  = "Political interest"
  ),
  stars = TRUE,              # this works when p.value is present
  statistic = "p.value",     # this tells modelsummary to show p-values
  intercept = TRUE,
  output = "html",
  digits = 2,
  file = "test_mod.html"
)

# --- Variable to plot ---
term_to_plot <- "ideo_right_num."  # exact predictor name

# --- Plot predicted values for each model ---
for (name in names(priority_ord_list)) {
  model <- priority_ord_list[[name]]
  
  pred <- ggpredict(model, terms = term_to_plot)
}

p <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(
    x = "Ideology (Right)",
    y = paste("Predicted Priority for", name),
    title = paste("Predicted Priority for", name, "by Ideology")
  ) +
  theme_minimal()

# Save plot as PNG in working directory
ggsave(filename = paste0("plot_", name, ".png"), plot = p, width = 6, height = 4)