# ===================================================
# Models for SASE Conference (July 2025)
# ===================================================
# Version: June 6th, 2025

# -----------------------
# 1. Load packages
# -----------------------
library(tidyverse)
library(tidyr)
library(dplyr)
library(anesrake)
library(ggplot2)
library(modelsummary)
library(stringr)
library(purrr)
library(rlang)
library(MASS)
library(effects)
library(ggeffects)
library(sjPlot)
library(patchwork)

# -----------------------
# 2. Load data
# -----------------------
df <- read.csv("data/clean_df_full.csv") 

# -----------------------
# 3. Descriptive statistics & weights
# -----------------------
# Age, gender, employment, income, region, education, political interest and ideology
# names(df)
# prop.table(table(df$ideo_interest_politics_num))
# hist(df$ideo_interest_politics_num)

# Highly biased, anesrake is applied for age, gender, employment, income, 
# education using 2021 census data (categories not always the same, proportions done by myself)

# Step 1: Clean and rename
df$gender <- NA_integer_
df$gender[df$ses_male_bin == 1] <- 2          # man
df$gender[df$ses_male_bin == 0] <- 1       # women & other
table(df$gender)

df$age <- NA_integer_
df$age[df$age34 == 1] <- 1          # 18-34
df$age[df$age35_54 == 1] <- 2       # 35-54
df$age[is.na(df$age) | (df$age34 == 0 & df$age35_54 == 0)] <- 3  # 55+ group
table(df$age)

df$education <- NA_integer_
df$education[df$educ_group %in% c("educBHS", "educHS")] <- 1
df$education[df$educ_group == "educUniv"] <- 2     
table(df$education)

df$income <- NA_integer_
df$income[df$ses_income3Cat == "Low"] <- 1
df$income[df$ses_income3Cat == "Mid"] <- 2
df$income[df$ses_income3Cat == "High"] <- 3
table(df$income)

# Census info
gender <- c(.49,.51)
age  <- c(0.24, 0.33,  0.43)
education <- c(0.71, 0.29)
income  <-  c(0.135,  0.7,  0.165)


# definitions of target list
targets <- list(gender, age, education, income)
# important: to use the same variable names of the dataset
names(targets) <- c("gender", "age", "education", "income")

# id variable
df$caseid <- 1:length(df$gender)

anesrakefinder(targets, df, choosemethod = "total")

outsave <- anesrake(targets, df, caseid = df$caseid,
                    verbose= FALSE, cap = 5, choosemethod = "total",
                    type = "pctlim", pctlim = .05 , nlim = 5,
                    iterate = TRUE , force1 = TRUE)

summary(outsave)

# add weights to the dataset
df$weightvec  <- unlist(outsave[1])

n  <- length(df$income)

# weighting loss
((sum(df$weightvec ^ 2) / (sum(df$weightvec)) ^ 2) * n) - 1
names(df)
unweighted <-  wpct(df$tradeoff_childcare_benefits_bin)
weighted  <-  wpct(df$tradeoff_childcare_benefits_bin, df$weightvec)
tab  <- data.frame(unweighted, weighted)

# -----------------------
# 3. DV & IV
# -----------------------

# Models
# Priority issue (continuous)
# What predicts assigning a higher priority to this specific policy (e.g., health)?

# OLS regression is we expect distance between scales to be equivalent
budget_health_priority_num
budget_education_priority_num
budget_pensions_priority_num
budget_debt_priority_num
budget_taxes_priority_num

# --- Convert numeric priority to ordered factors ---
ordered_levels <- c(1, 0.75, 0.5, 0.25, 0)

df$budget_health_rank <- factor(df$budget_health_priority_num, levels = ordered_levels, ordered = TRUE)
df$budget_education_rank <- factor(df$budget_education_priority_num, levels = ordered_levels, ordered = TRUE)
df$budget_pensions_rank <- factor(df$budget_pensions_priority_num, levels = ordered_levels, ordered = TRUE)
df$budget_debt_rank <- factor(df$budget_debt_priority_num, levels = ordered_levels, ordered = TRUE)
df$budget_taxes_rank <- factor(df$budget_taxes_priority_num, levels = ordered_levels, ordered = TRUE)

# --- Define formula (to avoid repetition) ---
formula_ord <- as.formula(
  budget_rank ~ ideo_right_num. + age34 + age35_54 + ses_male_bin + educBHS + educHS +
    incomeLow_bin + incomeMid_bin + children_bin + employ_fulltime_bin +
    ideo_interest_politics_num + trust_social_bin + trust_media_bin
)

# --- Run ordinal logistic regressions ---
mod_ord_health <- polr(
  update(formula_ord, budget_rank ~ .),
  data = transform(df, budget_rank = budget_health_rank),
  weights = weightvec,
  Hess = TRUE
)
str(df[, all.vars(update(formula_ord, budget_rank ~ .))])
mod_ord_edu <- polr(
  update(formula_ord, budget_rank ~ .),
  data = transform(df, budget_rank = budget_education_rank),
  weights = weightvec,
  Hess = TRUE
)

mod_ord_pensions <- polr(
  update(formula_ord, budget_rank ~ .),
  data = transform(df, budget_rank = budget_pensions_rank),
  weights = weightvec,
  Hess = TRUE
)

mod_ord_debt <- polr(
  update(formula_ord, budget_rank ~ .),
  data = transform(df, budget_rank = budget_debt_rank),
  weights = weightvec,
  Hess = TRUE
)

mod_ord_taxes <- polr(
  update(formula_ord, budget_rank ~ .),
  data = transform(df, budget_rank = budget_taxes_rank),
  weights = weightvec,
  Hess = TRUE
)

# --- Store models in a named list ---
priority_ord_list <- list(
  "Health"    = mod_ord_health,
  "Education" = mod_ord_edu,
  "Pensions"  = mod_ord_pensions,
  "Debt"      = mod_ord_debt,
  "Taxes"     = mod_ord_taxes
)

# --- Variable to plot ---
term_to_plot <- "ideo_right_num."  # exact predictor name

# --- Plot predicted values for each model ---
for (name in names(priority_ord_list)) {
  model <- priority_ord_list[[name]]
  
  pred <- ggpredict(model, terms = term_to_plot)
  
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
}

# --- Create coefficient summary table ---
model_tbl <- modelsummary(
  priority_ord_list,
  coef_rename = c(
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
    ideo_right_num.             = "Right",
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
  file = "test_mod.html"
)

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
  polr(formula, data = df, Hess = TRUE)
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


