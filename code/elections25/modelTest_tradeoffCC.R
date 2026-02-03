# Testing models
library(modelsummary)
library(purrr)
library(ggeffects)

# -- 2. Load data & create binary IV 
df <- read.csv("data/ACA_weighted.csv")

# Define dependent variables
dvs <- c(
  "tradeoff_childcare_lowincome_bin"
  #"tradeoff_childcare_benefits_bin"
)

# Baseline covariates (excluding the variables to be swapped)
base_covariates <- c(
  "ses_male_bin",
  "age_young_bin",
  "income_high_bin",
  "education_bin",
  "employ_fulltime_bin",
  "children_bin"
)

var_labels <- c(
  ses_male_bin = "Male",
  age_young_bin = "Age (18-34)",
  income_high_bin = "Income (high)",
  education_bin = "University education",
  employ_fulltime_bin = "Employed full time",
  children_bin = "Children",
  ideo_right_bin = "Right ideology",
  terr_identity_bin = "Identify with province",
  trust_pol_parties_bin = "Trust in political parties",
  reciprocity_index = "Reciprocity Index",
  redis_effort_bin = "Proportionality",
  income_reciprocity    = "High income × Reciprocity",
  income_proportionality = "High income × Proportionality"
)

# Key predictors to test separately
## Priority child care spending not included because all 0
key_preds <- c("ideo_right_bin", 
               "terr_identity_bin", 
               "trust_pol_parties_bin", 
               "reciprocity_index", 
               "redis_effort_bin",
               "income_reciprocity",
               "income_proportionality")

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
  tradeoff_childcare_lowincome_bin_ideo_right_bin      = "Right ideology",
  tradeoff_childcare_lowincome_bin_terr_identity_bin   = "Ideology Country",
  tradeoff_childcare_lowincome_bin_trust_pol_parties_bin = "Trust in political parties",
  tradeoff_childcare_lowincome_bin_reciprocity_index     = "Reciprocity Index",
  tradeoff_childcare_lowincome_bin_redis_effort_bin =  "Proportionality"
)

# Create modelsummary table comparing all models side-by-side
#!!!!! Change file name 1 or 2
modelsummary(
  all_models,
  coef_map = var_labels_sub,
  model_names = model_labels,
  output = "modelsummary/childcare_predictors_table_1.tex",  # <- save to file
  stars = TRUE,
  statistic = c("p.value"),
  fmt = 3
)
 ### Predicted probabilities for interaction effects

# With this, for interaction terms: (can also change with tradeoff_childcare_lowincome_bin)
mod <- glm(
  tradeoff_childcare_benefits_bin ~ 
    ses_male_bin + age_young_bin + income_high_bin + education_bin + 
    employ_fulltime_bin + children_bin + 
    income_high_bin * reciprocity_index,
  data = df, 
  family = binomial(), 
  weights = weightvec
)

# Preds (can change with redis_effort_bin/reciprocity_index)
preds <- ggpredict(mod, terms = c("reciprocity_index", "income_high_bin"))

# Plot
pp <- plot(preds) + 
  labs(
    x = "Prop Beliefs",
    y = "Predicted Probability",
    color = "High Income",
    title = "Interaction: High Income × Prop"
  ) +
  theme_minimal()

#Save 
ggsave(
  filename = "graphs/predictedProp_tradeoffCC_lowIncome.png",
  plot     = pp,
  width    = 10,
  height   = 8,
  dpi      = 300
)
