# ===================================================
# Binary Logistic Regression Models and Plots
# ===================================================
# Version: June 10th, 2025
#

# -- 1. Load packages
library(tidyverse)
library(modelsummary)
library(broom)
library(ggplot2)

# -- 2. Load data & create binary IV 
df <- read.csv("data/ACA_weighted.csv")


df$education_bin <- ifelse(df$education == "University", 1, 0)
df$age_young_bin <- ifelse(df$age == "18–34", 1, 0)
df$income_high_bin <- ifelse(df$income == "High", 1, 0)
df <- df %>%
  mutate(ideo_right_bin = if_else(ideo_right_num. >= 0.5, 1, 0))


# -- 3. Define dependent variables (binary outcomes) and predictors
dvs <- c(
  # "budget_education_priority_bin",
  # "budget_pensions_priority_bin",
  # "budget_taxes_priority_bin",
  # "budget_debt_priority_bin",
  # "budget_spend_prio_health_bin",
  # "budget_spend_prio_seniors_bin",
  # "budget_spend_prio_childcare_bin",
  # "budget_spend_prio_costLiving_bin",
  # "budget_spend_prio_climateChange_bin",
  # "tradeoff_childcare_num_bin", 
  # "tradeoff_childcare_higher_taxes_bin",
  # "tradeoff_childcare_by_cutting_bin",
  # "tradeoff_childcare_debt_bin",
  #"tradeoff_no_taxes_bin", # Increase to Taxation (control)
  #"tradeoff_taxes_sales_bin", # Increase to taxation, sales tax
  #"tradeoff_taxes_high_income_bin", # Increase to taxation, high incomes
  #"tradeoff_taxes_wealthy_bin", # Increase to taxation, wealth tax
  #"tradeoff_childcare_lowincome_bin",
  #"tradeoff_childcare_benefits_bin",
   "tradeoff_senior_benefits_bin",
   "tradeoff_senior_income_bin"
  # "redis_opportunity_bin",
  # "redis_reasons_rich_bin",
  # "redis_reasons_poor_bin",
  # "redis_effort_bin",
  # "redis_social_benefits_bin",
  # "redis_welfare_bin",
  # "redis_no_cheat_system_bin"
)

ivs <- c(
  "ses_male_bin", # Male
  "age_young_bin", # Age 
  "income_high_bin", # Income (High)
  "education_bin", # Education
  "home_owned_bin", # Homeowner
  "children_bin", # Children
  "employ_fulltime_bin", # Employed full time
  "ideo_right_bin", # Right ideology
  "ideo_country_bin", # Identify as Canadian first
  "trust_social_bin", # Trust in society
  "trust_pol_parties_bin" # Trust in political parties
)

# -- 3a. Define labels for outcomes and predictors
dv_labels <- c(
  tradeoff_senior_benefits_bin = "Cut, but not for lower income",
  tradeoff_senior_income_bin = "Cut, but for all"
)

var_labels <- c(
  ses_male_bin          = "Male",
  age_young_bin         = "Age (18-34)",
  income_high_bin       = "Income (high)",
  education_bin         = "University Education",
  home_owned_bin        = "Homeowner",
  children_bin          = "Children",
  employ_fulltime_bin   = "Employed full time",
  ideo_right_bin        = "Right ideology",
  ideo_country_bin      = "Identify as Canadian first",
  trust_social_bin      = "Trust in society",
  trust_pol_parties_bin = "Trust in political parties"
)

# -- 4. Fit logistic regression models with survey weights
models <- map(dvs, function(dv) {
  frm <- as.formula(paste(dv, "~", paste(ivs, collapse = " + ")))
  glm(frm, data = df, family = binomial(), weights = weightvec)
})

names(models) <- dv_labels

# -- 5. Create and save a summary table of coefficients (p-values and stars)
modelsummary(
  models,
  statistic = "p.value",
  stars    = TRUE,
  coef_map = var_labels,
  output   = "html",
  file     = "modelsummary/tradeoffHC.html"
)

# -- 6. Tidy model outputs and combine for plotting
coef_df <- map_df(models, ~ tidy(.x, conf.int = TRUE), .id = "outcome")

# -- 7. Plot coefficient estimates for all models
coef_plot <- coef_df %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(term, !!!var_labels),  # Apply recoding
    term = factor(term, levels = unique(unname(var_labels))),  # Set levels to only the visible labels
    outcome = factor(outcome, levels = dv_labels)
  )%>%
  ggplot(aes(x = estimate, y = term, color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.7)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0) +
  labs(
    x     = "Coefficient Estimate",
    y     = "Predictor",
    color = "Outcome",
    title = "Logistic Regression Coefficients for Binary Outcomes"
  ) +
  theme_minimal()

coef_plot

# -- 8. Save the coefficient plot
ggsave(
  filename = "graphs/logOdds_tradeoffHC.png",
  plot     = coef_plot,
  width    = 10,
  height   = 8,
  dpi      = 300
)

### Predicted probabilities 

# --- Step 1: Define mode function for categorical predictors
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# --- Step 2: Create baseline profile with typical values
baseline <- df %>%
  select(all_of(ivs)) %>%
  summarise(across(everything(), function(x) {
    if (is.numeric(x)) mean(x, na.rm = TRUE) else get_mode(x)
  }))

# --- Step 3: Create newdata varying key predictors
predictors_to_vary <- c("trust_social_bin", "trust_pol_parties_bin", "ideo_country_bin", "ideo_right_bin")

newdata_list <- lapply(predictors_to_vary, function(var) {
  baseline_exp <- baseline[rep(1, ifelse(var == "ideo_right_num.", 5, 2)), ]
  
  baseline_exp[[var]] <- if (var == "ideo_right_num.") {
    seq(0, 1, length.out = 5)
  } else {
    c(0, 1)
  }
  
  baseline_exp$varied <- var
  baseline_exp$varied_value <- baseline_exp[[var]]
  baseline_exp
})

newdata_all <- bind_rows(newdata_list)

# --- Step 4: Predict probabilities from each model
pred_df <- map_df(names(models), function(name) {
  model <- models[[name]]
  
  preds <- predict(model, newdata = newdata_all, type = "response", se.fit = TRUE)
  
  tibble(
    outcome        = name,
    varied         = newdata_all$varied,
    varied_value   = newdata_all$varied_value,
    predicted_prob = preds$fit,
    se             = preds$se.fit
  ) %>%
    mutate(
      conf.low = predicted_prob - 1.96 * se,
      conf.high = predicted_prob + 1.96 * se
    )
})

# --- Step 5: Plot predicted probabilities
pred_df %>%
  mutate(
    outcome = factor(outcome, levels = names(dv_labels), labels = unname(dv_labels)),
    varied  = recode(varied,
                     trust_social_bin = "Trust in society",
                     trust_pol_parties_bin = "Trust in political parties",
                     ideo_country_bin = "Identify as Canadian first",
                     `ideo_right_num.` = "Right ideology"
    )
  ) %>%
  ggplot(aes(x = varied_value, y = predicted_prob, color = outcome)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = outcome),
              alpha = 0.2, linetype = 0, color = NA) +
  facet_wrap(~varied, scales = "free_x") +
  labs(
    x = "Predictor Value",
    y = "Predicted Probability",
    color = "Outcome",
    fill = "Outcome",
    title = "Predicted Probabilities by Key Predictors"
  ) +
  theme_minimal(base_size = 14)

