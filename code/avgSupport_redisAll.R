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
  redis_intelligence_num = "Rewarded for \n intelligence and skill",
  redis_opportunity_num = "Equal opportunity \n to get ahead",               
  redis_reasons_rich_num = "Non-violating outcomes \n Rich",               
  redis_reasons_poor_num = "Non-violating outcomes \n Poor",                 
  redis_effort_num = "Fairness of the \n income distribution",                  
  redis_social_benefits_num = "Social benefits \n not a choice",            
  redis_welfare_num = "Welfare doesn't go to \n the underserving",             
  redis_no_cheat_system_num = "Trust not to cheat \n system"  
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
  "ses_income3Cat" # Quebec region
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
###########################
#########################


# --- 1. Variables ---
dvs <- names(dv_labels)

ivs <- c("ses_income3Cat")  # 3-category income factor

# Ensure the income variable is a factor
# Ensure ses_income3Cat is a factor and has all levels you want
df$ses_income3Cat <- factor(df$ses_income3Cat,
                            levels = c("Low", "Medium", "High"))

# Remove rows where DV or IV is NA for the current DV
# AFTER: capture full levels first, then clean, then restore levels
get_income_predictions <- function(dv, data, income_var = "ses_income3Cat") {
  
  # Capture the full set of levels BEFORE dropping NAs
  full_levels <- levels(data[[income_var]])
  
  data_clean <- data %>%
    select(all_of(c(dv, income_var))) %>%
    drop_na() %>%
    mutate(!!income_var := factor(.data[[income_var]], levels = full_levels))  # restore all levels
  
  formula <- as.formula(paste(dv, "~", income_var))
  model <- lm(formula, data = data_clean)
  
  newdata <- setNames(
    data.frame(factor(full_levels, levels = full_levels)),
    income_var
  )
  
  preds <- predict(model, newdata = newdata, se.fit = TRUE)
  
  tibble(
    dv = dv_labels[dv],
    income = newdata[[income_var]],
    estimate = preds$fit,
    conf.low = preds$fit - 1.96 * preds$se.fit,
    conf.high = preds$fit + 1.96 * preds$se.fit
  )
}

pred_all <- map_dfr(dvs, ~ get_income_predictions(.x, df))

# Make dv a factor for faceting
pred_all$dv <- factor(pred_all$dv, levels = dv_labels)

# Plot
ggplot(pred_all, aes(x = income, y = estimate, color = income)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  facet_wrap(~ dv, scales = "free_y") +
  labs(
    x = "Income Category",
    y = "Predicted Support (0–1 scale)",
    title = "Predicted Redistribution Support by Income Category",
    caption = "OLS models with income as predictor; 95% confidence intervals shown"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12)
  )





# Check observation counts for each DV × income category combination
library(purrr)

walk(dvs, function(dv) {
  cat("\n---", dv, "---\n")
  df %>%
    select(all_of(c(dv, "ses_income3Cat"))) %>%
    drop_na() %>%
    count(ses_income3Cat, .drop = FALSE) %>%
    print()
})

table(df$ses_income3Cat)
