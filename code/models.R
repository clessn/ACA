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
names(df)
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


# Spending distribution by issue
# What explains priorities for an issue? (Same as above)
budget_spend_prio_childcare_norm
budget_spend_prio_climateChange_norm
budget_spend_prio_costLiving_norm
budget_spend_prio_health_norm
budget_spend_prio_seniors_norm

# Step 1: Create ordered factors
ordered_levels <- c(1, 0.75, 0.5, 0.25, 0)
df$priorty_childcare_rank    <- factor(df$budget_spend_prio_childcare_norm, levels = ordered_levels, ordered = TRUE)
df$priorty_climateChange_rank <- factor(df$budget_spend_prio_climateChange_norm, levels = ordered_levels, ordered = TRUE)
df$priorty_costLiving_rank    <- factor(df$budget_spend_prio_costLiving_norm, levels = ordered_levels, ordered = TRUE)
df$priorty_health_rank        <- factor(df$budget_spend_prio_health_norm, levels = ordered_levels, ordered = TRUE)
df$priorty_seniors_rank       <- factor(df$budget_spend_prio_seniors_norm, levels = ordered_levels, ordered = TRUE)

# Step 2: Collapse to fewer categories
collapse_ranks <- function(x) {
  case_when(
    x %in% c(1, 0.75, 0.5) ~ "High",
    x == 0.25 ~ "Medium",
    x == 0 ~ "Low",
    TRUE ~ NA_character_
  )
}

df$rank_childcare_collapsed     <- factor(collapse_ranks(df$budget_spend_prio_childcare_norm), levels = c("Low", "Medium", "High"), ordered = TRUE)
df$rank_climateChange_collapsed <- factor(collapse_ranks(df$budget_spend_prio_climateChange_norm), levels = c("Low", "Medium", "High"), ordered = TRUE)
df$rank_costLiving_collapsed    <- factor(collapse_ranks(df$budget_spend_prio_costLiving_norm), levels = c("Low", "Medium", "High"), ordered = TRUE)
df$rank_health_collapsed        <- factor(collapse_ranks(df$budget_spend_prio_health_norm), levels = c("Low", "Medium", "High"), ordered = TRUE)
df$rank_seniors_collapsed       <- factor(collapse_ranks(df$budget_spend_prio_seniors_norm), levels = c("Low", "Medium", "High"), ordered = TRUE)

# Step 3: Model fitting
priority_vars <- c(
  "rank_childcare_collapsed",
  "rank_climateChange_collapsed",
  "rank_costLiving_collapsed",
  "rank_health_collapsed",
  "rank_seniors_collapsed"
)

predictors <- c("ideo_right_num.")
priority_models <- list()

for (var in priority_vars) {
  cat("Fitting model for:", var, "\n")
  
  vars_to_use <- c(var, predictors)
  df_model <- df[, vars_to_use]
  df_model <- na.omit(df_model)
  
  # Drop unused factor levels
  df_model[[var]] <- droplevels(df_model[[var]])
  
  if (nlevels(df_model[[var]]) > 1) {
    formula_i <- as.formula(paste(var, "~", paste(predictors, collapse = " + ")))
    tryCatch({
      model <- polr(formula_i, data = df_model, Hess = TRUE)
      priority_models[[var]] <- model
    }, error = function(e) {
      message("Error fitting model for ", var, ": ", e$message)
    })
  } else {
    message("Skipping ", var, ": not enough response levels after cleaning.")
  }
}

# Step 4: Plot predicted probabilities
library(ggeffects)
library(ggplot2)

for (name in names(priority_models)) {
  model <- priority_models[[name]]
  
  pred <- ggpredict(model, terms = "ideo_right_num.")
  
  p <- ggplot(pred, aes(x = x, y = predicted, color = response.level, fill = response.level)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
    labs(
      x = "Ideology (Right)",
      y = "Predicted Probability",
      title = paste("Predicted Priority for", gsub("rank_|_collapsed", "", name)),
      color = "Response Level",
      fill = "Response Level"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
# Step 5: Summary table
library(modelsummary)

model_tbl_2 <- modelsummary(
  priority_models,
  coef_rename = c(
    ideo_right_num.             = "Right",
    ses_french_bin              = "French language",
    ses_male_bin                = "Gender",
    age34                       = "Young",
    age35_54                    = "Middle age",
    educBHS                     = "BHS education",
    educHS                      = "HS education",
    incomeLow_bin               = "Low income",
    incomeMid_bin               = "Middle income",
    matStatus_married_bin       = "Married",
    home_owned_bin              = "Homeowner",
    children_bin                = "Children",
    employ_fulltime_bin         = "Employed",
    ideo_party_them             = "Cynic themselves",
    ideo_party_bin              = "Cynic party",
    ideo_country_bin            = "Not-cynical",
    family_first_union_bin      = "Traditional family",
    ideo_interest_politics_num  = "Political interest",
    dependentChildren           = "Dependent Children",
    youngChildren               = "Young Children",
    trust_social_bin            = "Trust Social",
    trust_media_bin             = "Trust Media"
  ),
  stars = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
  output = "html",
  digits = 2,
  file = "test_mod_spend.html"
)

# Policy: childcare spending (low n, because divided with green economy)
# What explains priorities for an issue? (Same as above)
sum(!is.na(df$tradeoff_childcare_num))                 # control
sum(!is.na(df$tradeoff_childcare_higher_taxes_num))    # higher taxes treatment
sum(!is.na(df$tradeoff_childcare_by_cutting_num))      # cuts treatment
sum(!is.na(df$tradeoff_childcare_debt_num))            # debt treatment

# List of outcome variables
outcomes <- c(
  "tradeoff_childcare_num",
  "tradeoff_childcare_higher_taxes_num",
  "tradeoff_childcare_by_cutting_num",
  "tradeoff_childcare_debt_num"
)

# Formula with a placeholder for the outcome
predictors <- "ideo_right_num. + budget_debt_priority_num + budget_taxes_priority_num + budget_spend_prio_childcare_norm"

# Fit models and store in a named list
models <- lapply(outcomes, function(outcome_var) {
  df[[outcome_var]] <- ordered(df[[outcome_var]])
  formula <- as.formula(paste(outcome_var, "~", predictors))
  polr(formula, data = df, Hess = TRUE)
})
names(models) <- outcomes

# Display all models side-by-side in one table
modelsummary(models,
             stars = TRUE,
             statistic = "p.value",
             title = "Comparison of Polr Models for Childcare Tradeoffs",
             coef_map = NULL)  # You can specify coef_map to rename predictors if desired


# Policy: Green economy (low n, because divided with child care)
sum(!is.na(df$tradeoff_invest_green_num))             # control
sum(!is.na(df$tradeoff_taxes_green_num))              # higher taxes treatment
sum(!is.na(df$tradeoff_cutting_for_green_num))        # cuts treatment
sum(!is.na(df$tradeoff_debt_green_num))               # debt treatment

# Policy: taxes
# What explains priorities for an issue? (Same as above)
tradeoff_no_taxes_num                 # control
tradeoff_taxes_sales_num              # sales tax treatment
tradeoff_taxes_high_income_num        # income tax treatment
tradeoff_taxes_wealthy_num            # capital gains treatment


# List of outcome variables
outcomes_taxes <- c(
  "tradeoff_no_taxes_num",
  "tradeoff_taxes_sales_num",
  "tradeoff_taxes_high_income_num",
  "tradeoff_taxes_wealthy_num"
)



# Formula with a placeholder for the outcome
predictors_taxes <- "ideo_right_num. + age34 + educHS + 
employ_fulltime_bin + budget_taxes_priority_bin + budget_debt_priority_bin + ideo_country_bin + ideo_interest_politics_num"

df$budget_debt_priority_bin
ideo_interest_politics_num
ideo_country_bin

# Fit models and store in a named list
models_taxes <- lapply(outcomes_taxes, function(outcome_var) {
  df[[outcome_var]] <- ordered(df[[outcome_var]])
  formula <- as.formula(paste(outcome_var, "~", predictors_taxes))
  polr(formula, data = df,   weights = df$weightvec, Hess = TRUE)
})

names(models_taxes) <- outcomes_taxes

# Display all models side-by-side in one table
modelsummary(models_taxes,
             stars = TRUE,
             statistic = "p.value",
             title = "Comparison of Polr Models for Taxation Tradeoffs",
             coef_map = NULL)  # You can specify coef_map to rename predictors if desired


names(df)
# Policy: childcare benefits (no treatment, choice)
tradeoff_childcare_benefits_num       # lower other benefits
tradeoff_childcare_lowincome_num      # increase price med/high income

# Policy: seniors (no treatment, choice)
tradeoff_senior_benefits_num         # lower pension benefits
tradeoff_senior_income_num           # increase price med/high income


# IV
ses_french_bin                      # language, FR = 1
ses_male_bin                        # gender, M = 1
age34                               # Age cat (55 plus becomes reference)
age35_54                            # Age cat (55 plus becomes reference)
educBHS                             # Educ (univ becomes reference)
educHS                              # Educ (univ becomes reference)
incomeLow                           # Income (high becomes reference)
incomeMid                           # Income (high becomes reference)
matStatus_married_bin               # Marital status, married = 1
home_owned_bin                      # Homeowner, homeowner = 1
children_bin                        # Parent, child (alone or married) = 1
employ_fulltime_bin                 # Employment, employed = 1 
ideo_right_num.                     # Ideology, right = 1
ideo_party_them                     # Cynicism, themselves = 1  
ideo_party_bin                      # Cynicism, party = 1 
ideo_country_bin                    # Cynicism, country = 1 (not cynical)
family_first_union_bin              # "Traditional" family = 1
ideo_interest_politics_num          # Continuous
dependentChildren
youngChildren

# IV complex
ses_french_bin                      # language, FR = 1
ses_male_bin                        # gender, M = 1
age1824_bin                         # Age cat
age2534_bin                         # Age cat
age3544_bin                         # Age cat
age4554_bin                         # Age cat
age5564_bin                         # Age cat
age65plus_bin                       # Age cat
quebec_bin                          # Prov cat
ses_educElementary_bin              # Edu cat
ses_educSecondaryCompleted_bin      # Edu cat
ses_educTechnicalCompleted_bin      # Edu cat
ses_educMaster_bin                  # Edu cat
ses_educDoctorate_bin               # Edu cat
ses_educHighSchoolSome_bin          # Edu cat
ses_educTechnicalSome_bin           # Edu cat
ses_educUniversitySome_bin          # Edu cat
ses_income30000_bin                 # Inc cat
ses_income60000_bin                 # Inc cat
ses_income90000_bin                 # Inc cat
ses_income110000_bin                # Inc cat
ses_income150000_bin                # Inc cat
ses_income200000_bin                # Inc cat
ses_incomeMore200000_bin            # Inc cat
matStatus_married_bin               # Marital cat
matStatus_commonlaw_bin             # Marital cat
matStatus_widow_bin                 # Marital cat
matStatus_divorced_bin              # Marital cat
home_owned_bin                      # Homeowner
alone_with_children_bin             # Children cat
alone_without_children_bin          # Children cat
couple_with_children_bin            # Children cat
couple_without_children_bin         # Children cat
employ_fulltime_bin                 # Employment bin
family_single_bin                   # Family structure
family_separate_bin                 # Family structure
family_first_union_bin              # Family structure
family_second_union_bin             # Family structure
family_other_bin                    # Family structure
ideo_right_num.                     # Ideology, right = 1
ideo_party_them                     # Cynicism, themselves = 1  
ideo_party_bin                      # Cynicism, party = 1 
ideo_country_bin                    # Cynicism, country = 1 (not cynical)


