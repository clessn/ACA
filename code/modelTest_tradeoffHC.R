# Testing models


library(modelsummary)
library(purrr)

# -- 2. Load data & create binary IV 
df <- read.csv("data/ACA_weighted.csv")

# Define dependent variables
dvs <- c(
  #"tradeoff_senior_benefits_bin"
  "tradeoff_senior_income_bin"
)

# Baseline covariates (excluding the variables to be swapped)
base_covariates <- c(
  "ses_male_bin",
  "age_young_bin",
  "income_high_bin",
  "education_bin",
  "employ_fulltime_bin"
)

var_labels <- c(
  ses_male_bin = "Male",
  age_young_bin = "Age (18-34)",
  income_high_bin = "Income (high)",
  education_bin = "University education",
  employ_fulltime_bin = "Employed full time",
  ideo_right_bin = "Right ideology",
  ideo_country_bin = "Identify as Canadian first",
  trust_pol_parties_bin = "Trust in political parties",
   budget_spend_prio_seniors_bin = "Priority Senior Spending",
  #budget_pensions_priority_bin = "Priorty Pensions Ranking",
  reciprocity_index = "Reciprocity Index",
  redis_effort_bin = "Proportionality"   
)
# Three key predictors to test separately
key_preds <- c("ideo_right_bin", "ideo_country_bin", "trust_pol_parties_bin", 
               #"budget_pensions_priority_bin",
               "budget_spend_prio_seniors_bin",
               "reciprocity_index", "redis_effort_bin")

# Prepare a named list to store models
all_models <- list()

model_name <- paste(dv, key_pred, sep = "_")
# Loop over dependent variables and key predictors
for (dv in dvs) {
  for (key_pred in key_preds) {
    # Combine predictors: baseline + current key predictor
    ivs_current <- c(base_covariates, key_pred)
    
    # Create formula
    formula_str <- paste(dv, "~", paste(ivs_current, collapse = " + "))
    formula <- as.formula(formula_str)
    
    # Fit logistic regression with weights (adjust if you use weights)
    mod <- glm(formula, data = df, family = binomial(), weights = weightvec)
    
    # Name model like "dv_keypredictor" for clarity
    model_name <- paste(dv, key_pred, sep = "_")
    all_models[[model_name]] <- mod
  }
}

# Optional: define variable label map with only used variables
var_labels_sub <- var_labels[names(var_labels) %in% c(base_covariates, key_preds)]
print(var_labels_sub)

# Optional: tidy up model names for presentation
model_labels <- c(
  tradeoff_senior_income_bin_ideo_right_bin      = "Right ideology",
  tradeoff_senior_income_bin_ideo_country_bin   = "Ideology Country",
  tradeoff_senior_income_bin_trust_pol_parties_bin = "Trust in political parties",
  #tradeoff_senior_income_bin_budget_pensions_priority_bin = "Priority Pensions Ranking",
 tradeoff_senior_income_bin_budget_spend_prio_seniors_bin = "Priority Senior Spending",
  tradeoff_senior_income_bin_reciprocity_index     = "Reciprocity Index",
  tradeoff_senior_income_bin_redis_effort_bin =  "Proportionality"
)

# Create modelsummary table comparing all models side-by-side
#!!!!! Changine file name 1 or 2
modelsummary(
  all_models,
  coef_map = var_labels_sub,
  model_names = model_labels,
  output = "modelsummary/senior_predictors_table_2.tex",  # <- save to file
  stars = TRUE,
  statistic = c("p.value"),
  fmt = 3,
  title = "Comparison of Key Predictors on Senior Care Attitudes",
)
