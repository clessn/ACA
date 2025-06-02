# ===================================================
# Models for SASE Conference (July 2025)
# ===================================================
# Version: June 2nd, 2025

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
library(MASS)
library(effects)
library(patchwork)

# -----------------------
# 2. Load data
# -----------------------
df <- read.csv("data/clean_df.csv") 

str(df$ses_male_bin)
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

# Convert to ordered factor forGLM (if we expect distance to vary--which we do)
# Health
df$budget_health_rank <- factor(df$budget_health_priority_num, 
                                levels = c(1, 0.75, 0.5, 0.25, 0), 
                                ordered = TRUE)

# Education priority
df$budget_education_rank <- factor(df$budget_education_priority_num, 
                                   levels = c(1, 0.75, 0.5, 0.25, 0), 
                                   ordered = TRUE)

# Pensions priority
df$budget_pensions_rank <- factor(df$budget_pensions_priority_num, 
                                  levels = c(1, 0.75, 0.5, 0.25, 0), 
                                  ordered = TRUE)

# Debt priority
df$budget_debt_rank <- factor(df$budget_debt_priority_num, 
                              levels = c(1, 0.75, 0.5, 0.25, 0), 
                              ordered = TRUE)

# Taxes priority
df$budget_taxes_rank <- factor(df$budget_taxes_priority_num, 
                               levels = c(1, 0.75, 0.5, 0.25, 0), 
                               ordered = TRUE)

# Models: what do Canadians prioritize (do we care about debt & taxes or policy)
mod_ord_health <- polr(
  budget_health_rank ~ ideo_right_num + ses_french_bin + age34 + age35_54 + ses_male_bin + educBHS + educHS
  + incomeLow + incomeMed + dependentchildren + ideo_party_them + matStatus_married_bin 
  + ideo_interest_politics_num,
  data = df,
  Hess = TRUE
)

mod_ord_edu <- polr(
  budget_education_rank ~ ideo_right_num + ses_french_bin + age34 + age35_54 + ses_male_bin + educBHS + educHS
  + incomeLow + incomeMed + dependentchildren + ideo_party_them + matStatus_married_bin 
  + ideo_interest_politics_num,
  data = df,
  Hess = TRUE
)

mod_ord_pensions <- polr(
  budget_pensions_rank ~ ideo_right_num + ses_french_bin + age34 + age35_54 + ses_male_bin + educBHS + educHS
  + incomeLow + incomeMed + dependentchildren + ideo_party_them + matStatus_married_bin 
  + ideo_interest_politics_num,
  data = df,
  Hess = TRUE
)

mod_ord_debt <- polr(
  budget_debt_rank ~ ideo_right_num + ses_french_bin + age34 + age35_54 + ses_male_bin + educBHS + educHS
  + incomeLow + incomeMed + dependentchildren + ideo_party_them + matStatus_married_bin 
  + ideo_interest_politics_num,
  data = df,
  Hess = TRUE
)

mod_ord_taxes <- polr(
  budget_taxes_rank ~ ideo_right_num + ses_french_bin + age34 + age35_54 + ses_male_bin + educBHS + educHS
  + incomeLow + incomeMed + dependentchildren + ideo_party_them + matStatus_married_bin 
  + ideo_interest_politics_num,
  data = df,
  Hess = TRUE
)

df$ideo_right_num
# Define models with labels
priority_ord_list <- list(
  "Health"    = mod_ord_health,
  "Education" = mod_ord_edu,
  "Pensions"  = mod_ord_pensions,
  "Debt"      = mod_ord_debt,
  "Taxes"     = mod_ord_taxes
)

# Select key predictor to visualize: youngChildren, dependentChildren, employment status age, trust cynicsm, provIdentity
# Pick variable of interest (e.g., treatment)
term_to_plot <- df$ideo_right_num.

# Create plots for each policy model
priority_plots <- lapply(names(priority_ord_list), function(name) {
  model <- priority_ord_list[[name]]
  pred <- ggpredict(model, terms = term_to_plot)
  
  plot(pred) +
    ggtitle(paste("Priority for", name, "by", term_to_plot)) +
    theme_minimal()
})

pred_health <- ggpredict(mod_ord_health, terms = "treatment")  # Or another variable
plot(pred_health) +
  theme_minimal() +
  labs(title = "Predicted Probabilities of Ranking Health as High Priority",
       x = "Treatment Group", y = "Probability")

# Combine into one panel
wrap_plots(priority_plots, ncol = 1)

#gof_map_df <- data.frame(
#  raw = c("r.squared", "adj.r.squared", "nobs", "AIC", "BIC"),
#  clean = c("R²", "Adj. R²", "N", "AIC", "BIC"),
#  fmt = c("%.2f", "%.2f", "%.0f", "%.2f", "%.2f"),
#  stringsAsFactors = FALSE
#)

model_tbl <- modelsummary(
  priority_ord_list,
  coef_rename = c(
    ses_french_bin              = "French language",
    ses_male_bin                = "Gender",
    age34                       = "Young",
    age35_54                    = "Middle age",
    educBHS                     = "BHS education",
    educHS                      = "HS education",
    incomeLow                   = "Low income",
    incomeMid                   = "Middle income",
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
    dependentChildren = "",
    youngChildren = ""
  ),
  stars = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
  #title = "",
  #note = "Survey",
  output = "latex",
  digits = 2 #,
  #gof_map = gof_map_df
)

# Spending distribution by issue
# What explains priorities for an issue? (Same as above)
budget_spend_prio_childcare_norm
budget_spend_prio_climateChange_norm
budget_spend_prio_costLiving_norm
budget_spend_prio_health_norm
budget_spend_prio_seniors_norm

# Policy: childcare spending
# What explains priorities for an issue? (Same as above)
tradeoff_childcare_num                 # control
tradeoff_childcare_higher_taxes_num    # higher taxes treatment
tradeoff_childcare_by_cutting_num      # cuts treatment
tradeoff_childcare_debt_num            # debt treatment

# Models
mod_tradeoff_control  <- lm(tradeoff_childcare_num ~ age65plus_bin + ses_male_bin + ..., data = df)
mod_tradeoff_taxes    <- lm(tradeoff_childcare_higher_taxes_num ~ age65plus_bin + ses_male_bin + ..., data = df)
mod_tradeoff_cuts     <- lm(tradeoff_childcare_by_cutting_num ~ age65plus_bin + ses_male_bin + ..., data = df)
mod_tradeoff_debt     <- lm(tradeoff_childcare_debt_num ~ age65plus_bin + ses_male_bin + ..., data = df)

# Plot
plot(ggpredict(mod_tradeoff_taxes, terms = "ses_male_bin"))

# Policy: taxes
# What explains priorities for an issue? (Same as above)
tradeoff_no_taxes_num                 # control
tradeoff_taxes_sales_num              # sales tax treatment
tradeoff_taxes_high_income_num        # income tax treatment
tradeoff_taxes_wealthy_num            # capital gains treatment

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


