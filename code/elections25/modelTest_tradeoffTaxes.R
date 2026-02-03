library(modelsummary)
library(purrr)

# Load data
df <- read.csv("data/ACA_weighted.csv")

# Define dependent variables
dvs <- c(
  "tradeoff_no_taxes_bin"           # Control
  # "tradeoff_taxes_sales_bin"       # Sales tax
  #"tradeoff_taxes_high_income_bin"  # High income tax
  #"tradeoff_taxes_wealthy_bin"       # Wealth tax
)

# Baseline covariates
base_covariates <- c(
  "ses_male_bin",
  "age_young_bin",
  "income_high_bin",
  "education_bin",
  "employ_fulltime_bin"
)

# Variable labels
var_labels <- c(
  ses_male_bin = "Male",
  age_young_bin = "Age (18–34)",
  income_high_bin = "Income (high)",
  education_bin = "University education",
  employ_fulltime_bin = "Employed full time",
  ideo_right_bin = "Right ideology",
  terr_identity_bin = "Identify with province",
  trust_pol_parties_bin = "Trust in political parties",
  budget_taxes_priority_bin = "Priority: reduce taxes",
  budget_debt_priority_bin = "Priority: reduce debt",
  reciprocity_index = "Reciprocity Index",
  redis_effort_bin = "Proportionality",
  income_reciprocity    = "High income × Reciprocity",
  income_proportionality = "High income × Proportionality"
)

# Key predictors to test separately (each will be swapped in)
key_preds <- c(
  "ideo_right_bin", 
  "terr_identity_bin", 
  "trust_pol_parties_bin", 
  "budget_taxes_priority_bin", 
  "budget_debt_priority_bin", 
  "reciprocity_index", 
  "redis_effort_bin",
  "income_reciprocity",
  "income_proportionality"
)

# Prepare a named list to store models
all_models <- list()

# Loop over DVs and key predictors
for (dv in dvs) {
  for (key_pred in key_preds) {
    ivs_current <- c(base_covariates, key_pred)
    formula_str <- paste(dv, "~", paste(ivs_current, collapse = " + "))
    formula <- as.formula(formula_str)
    
    mod <- glm(formula, data = df, family = binomial(), weights = weightvec)
    
    model_name <- paste(dv, key_pred, sep = "_")
    all_models[[model_name]] <- mod
  }
}

# Subset variable labels to used variables
var_labels_sub <- var_labels[names(var_labels) %in% c(base_covariates, key_preds)]

# Create model name labels (optional - revise to match your desired output)
model_labels <- setNames(
  nm = names(all_models),
  object = gsub(
    pattern = "tradeoff_(no_taxes|taxes_sales|taxes_high_income|taxes_wealthy)_bin_(.*)",
    replacement = "\\2: \\1",
    x = names(all_models)
  )
)

# Create modelsummary table
modelsummary(
  all_models,
  coef_map = var_labels_sub,
  model_names = model_labels,
  output = "modelsummary/taxation_predictors_table_1.tex",
  stars = TRUE,
  statistic = "p.value",
  fmt = 3
)

## Predicted probabilities


# With this, for interaction terms: (can also change   with "tradeoff_no_taxes_bin" (Control)   
# "tradeoff_taxes_sales_bin"       # Sales tax
#"tradeoff_taxes_high_income_bin"  # High income tax
# "tradeoff_taxes_wealthy_bin"  # welath tax
mod <- glm(
  tradeoff_taxes_wealthy_bin ~ 
    ses_male_bin + age_young_bin + income_high_bin + education_bin + 
    employ_fulltime_bin  + 
    income_high_bin * reciprocity_index,
  data = df, 
  family = binomial(), 
  weights = weightvec
)

# Preds (can change with redis_effort_bin)
preds <- ggpredict(mod, terms = c("reciprocity_index", "income_high_bin"))

# Plot (can change with redis_effort_bin)
pp <- plot(preds) + 
  labs(
    x = "Reciprocity Index",
    y = "Predicted Probability",
    color = "High Income",
    title = "Interaction: High Income × Reciprocity"
  ) +
  theme_minimal()

#Save 
ggsave(
  filename = "graphs/predictedProp_tradeofftaxes_control.png",
  plot     = pp,
  width    = 10,
  height   = 8,
  dpi      = 300
)
