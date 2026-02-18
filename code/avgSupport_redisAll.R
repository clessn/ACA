# ===================================================
# Plots Redistribution questions
# ===================================================
# Version: Feb 17th 2026

# -- 1. Load packages
library(tidyverse)
library(modelsummary)
library(broom)
library(ggplot2)

library(tibble)
library(purrr)

# -- 2. Load data 
df <- read.csv("data/clean_df_valid.csv")
table(df$ses_income3Cat)
# === 1. Setup ===
# Dependent variables
dvs <- c(
  "redis_effort_num",
  "redis_intelligence_num",               
  "redis_no_cheat_system_num",               
  "redis_opportunity_num",               
  "redis_reasons_poor_num",                     
  "redis_reasons_rich_num",            
  "redis_social_benefits_num",                    
  "redis_welfare_num"   
)

# Labels for plotting
dv_labels <- c(
#  redis_intelligence_num = "Rewarded for \n intelligence and skill",
#  redis_opportunity_num = "Equal opportunity \n to get ahead",               
#  redis_reasons_rich_num = "Non-violating outcomes \n Rich",               
#  redis_reasons_poor_num = "Non-violating outcomes \n Poor",                 
  redis_effort_num = "Fairness of the \n income distribution" #,                  
#  redis_social_benefits_num = "Social benefits \n not a choice",            
#  redis_welfare_num = "Welfare doesn't go to \n the underserving",             
#  redis_no_cheat_system_num = "Trust not to cheat \n system"  
)

# Independent variables (covariates)
ivs <- c(
#  "ses_male_bin",           # Male
#  "age18_34_bin",           # Age (18–34)
#  "incomeHigh_bin",         # Income (High)
#  "educBHS",                # BHS
#  "children_bin",           # Children
#  "employ_fulltime_bin",    # Employed full time
#  "ideo_right_num",         # Right ideology
#  "trust_social_bin",       # Trust in society
  # "ses_region_cat",         # Regions 
  "ses_income3Cat" # Income
)
df$ses_income3Cat
# === 2. Fit models ===
# Models *with* covariates
ols_models_cov <- map(dvs, function(dv) {
  frm <- as.formula(paste(dv, "~", paste(ivs, collapse = " + ")))
  lm(frm, data = df)
})
names(ols_models_cov) <- dv_labels

# Models *without* covariates (intercept-only)
ols_models_nocov <- map(dvs, function(dv) {
  frm <- as.formula(paste(dv, "~ 1"))
  lm(frm, data = df)
})
names(ols_models_nocov) <- dv_labels

# === 3. Extract predicted means and CIs ===
get_predictions <- function(models, model_type) {
  map2_dfr(models, names(models), function(model, label) {
    preds <- predict(model, se.fit = TRUE)
    fit <- mean(preds$fit)
    se <- sqrt(mean(preds$se.fit^2))
    
    tibble(
      treatment = label,
      estimate = fit,
      conf.low = fit - 1.96 * se,
      conf.high = fit + 1.96 * se,
      model = model_type
    )
  })
}

pred_cov <- get_predictions(ols_models_cov, "With Covariates")
pred_nocov <- get_predictions(ols_models_nocov, "Without Covariates")

# Combine both
pred_all <- bind_rows(pred_cov, pred_nocov)
pred_all$treatment <- factor(pred_all$treatment, levels = rev(dv_labels))  # ordered

# Order
pred_all$treatment <- factor(pred_all$treatment, levels = dv_labels)
# === 4. Plot ===
plot <- ggplot(pred_all, aes(x = treatment, y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5),
    width = 0.15
  ) +
  scale_color_manual(
    values = c(
      "Without Covariates" = "black",
      "With Covariates" = "grey50"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Without Covariates" = 16,  # solid circle
      "With Covariates" = 17      # solid triangle
    )
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    # title = "Average Support for Redistribution",
    # subtitle = "OLS predicted means with and without covariates (95% CIs)",
    x = " ",
    y = "Predicted Support \n (0–1 scale)",
    
    color = "Model Type",
    shape = "Model Type" #,
    #caption = "Note: Covariate-adjusted models include: age, gender, education, employment, children, homeownership, ideology, and trust."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption.position = "plot",  # ensures caption aligns with the plot area
    plot.caption = element_text(hjust = 0)  # left-justify the caption
  )

plot

# -- 8. Save the coefficient plot
ggsave(
  filename = "graphs/avgSupport_redistribution.png",
  plot     = plot,
  width    = 10,
  height   = 8,
  dpi      = 300
)

###########################
# Marginal means
#########################
# Income

# Make sure a factor
df$ses_income3Cat <- factor(df$ses_income3Cat,
                            levels = c("Low", "Medium", "High"))
table(df$ses_income3Cat)
# Model
ols_models_cov <- map(dvs, function(dv) {
  frm <- as.formula(paste(dv, "~ ses_income3Cat"))
  lm(frm, data = df)
})

names(ols_models_cov) <- dv_labels

# Marginal effects
get_predictions_income <- function(models) {
  map2_dfr(models, names(models), function(model, label) {
    predictions(
      model,
      by = "ses_income3Cat"
    ) %>%
      transmute(
        treatment = label,
        income = ses_income3Cat,
        estimate = estimate,
        conf.low = conf.low,
        conf.high = conf.high
      )
  })
}

pred_income <- get_predictions_income(ols_models_cov)

ggplot(pred_income,
       aes(x = income, y = estimate, group = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1
  ) +
  facet_wrap(~ treatment) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Income category",
    y = "Predicted support (0–1)",
    title = "Predicted support for redistribution based on effort by income"
  ) +
  theme_minimal(base_size = 13)

# -- 8. Save the coefficient plot
ggsave(
  filename = "graphs/margEffects_.png",
  plot     = plot,
  width    = 10,
  height   = 8,
  dpi      = 300
)

