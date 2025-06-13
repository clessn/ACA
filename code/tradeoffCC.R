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


# -- 3. Define dependent variables (binary outcomes) and predictors
dvs <- c(
   "tradeoff_childcare_lowincome_bin",
   "tradeoff_childcare_benefits_bin"
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
  "trust_pol_parties_bin", # Trust in political parties
  "reciprocity_index",
  "redis_effort_bin"
  )

# -- 3a. Define labels for outcomes and predictors
dv_labels <- c(
  tradeoff_childcare_lowincome_bin = "Subsidize child care, \n but lower family benefits",
  tradeoff_childcare_benefits_bin = "Subsidize child care for low-income, \n increase cost for middle- and upper-class"
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
  trust_pol_parties_bin = "Trust in political parties",
  reciprocity_index = "Reciprocity Index",
  redis_effort_bin = "Proportionality"  
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
  file     = "modelsummary/tradeoffChildcare.html"
)

# -- 6. Tidy model outputs and combine for plotting
coef_df <- map_df(models, ~ tidy(.x, conf.int = TRUE), .id = "outcome")

# -- 7. Plot coefficient estimates for all models
coef_plot <- coef_df %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(term, !!!var_labels),
    term = factor(term, levels = unique(unname(var_labels))),
    outcome = factor(outcome, levels = rev(dv_labels))  # reverse order here
  ) %>%
  ggplot(aes(x = estimate, y = term, color = outcome, shape = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0) +
  scale_color_manual(
    values = c("black", "grey50")
  ) +
  scale_shape_manual(
    values = c(16, 17)  # 16 = solid circle, 17 = solid triangle
  ) +
  labs(
    x     = "Coefficient Estimate",
    y     = "Predictor",
    color = "Outcome",
    shape = "Outcome",
    title = "Logistic Regression Coefficients for Binary Outcomes"
  ) +
  theme_minimal()

coef_plot
# -- 8. Save the coefficient plot
ggsave(
  filename = "graphs/logOdds_tradeoffCC.png",
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
