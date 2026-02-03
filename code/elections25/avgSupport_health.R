# ===================================================
# Plots
# ===================================================
# Version: June 11th, 2025
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

# === 1. Setup ===
# Dependent variables
dvs <- c(
  "budget_health_priority_num",
  "budget_spend_prio_health_norm"
)

# Labels for plotting
dv_labels <- c(
  budget_health_priority_num = "Health as Top Priority", 
  budget_spend_prio_health_norm = "Health Point Allocation"
)

# Independent variables (covariates)
ivs <- c(
  "ses_male_bin",           # Male
  "age_young_bin",          # Age (18–34)
  "income_high_bin",        # Income (High)
  "education_bin",          # University education
  "home_owned_bin",         # Homeowner
  "children_bin",           # Children
  "employ_fulltime_bin",    # Employed full time
  "ideo_right_bin",         # Right ideology
  "ideo_country_bin",       # Identify as Canadian first
  "trust_social_bin",       # Trust in society
  "trust_pol_parties_bin"   # Trust in political parties
)

# === 2. Fit models ===
# Models *with* covariates
ols_models_cov <- map(dvs, function(dv) {
  frm <- as.formula(paste(dv, "~", paste(ivs, collapse = " + ")))
  lm(frm, data = df, weights = weightvec)
})
names(ols_models_cov) <- dv_labels

# Models *without* covariates (intercept-only)
ols_models_nocov <- map(dvs, function(dv) {
  frm <- as.formula(paste(dv, "~ 1"))
  lm(frm, data = df, weights = weightvec)
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
pred_all$model <- factor(pred_all$model, levels = c("Without Covariates", "With Covariates"))


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
    # title = "Average Support for Spending Increases by Treatment",
    # subtitle = "OLS predicted means with and without covariates (95% CIs)",
    x = " ",
    y = "Predicted Priority/Support \n (0–1 scale)",
    color = "Model Type",
    shape = "Model Type",
    caption = "Covariate-adjusted models include: age, gender, education, employment, children, homeownership, ideology, and trust."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption.position = "plot",  # ensures caption aligns with the plot area
    plot.caption = element_text(hjust = 0)  # left-justify the caption
  )

plot 

# -- 8. Save the coefficient plot
ggsave(
  filename = "graphs/avgSupport_health.png",
  plot     = plot,
  width    = 10,
  height   = 8,
  dpi      = 300
)


####

df$health_ranked_top <- ifelse(df$budget_health_priority_num == 1, "Ranked Health #1", "Did Not Rank Health #1")

plot2 <- ggplot(df, aes(x = health_ranked_top, y = budget_spend_prio_health_norm)) +
  stat_summary(
    fun = mean,
    fun.min = function(x) mean(x) - qt(0.975, df=length(x)-1)*sd(x)/sqrt(length(x)),
    fun.max = function(x) mean(x) + qt(0.975, df=length(x)-1)*sd(x)/sqrt(length(x)),
    geom = "bar", fill = "grey80", color = "black"
  ) +
  stat_summary(
    fun.data = mean_cl_normal, geom = "errorbar", width = 0.15
  ) +
  labs(
   # title = "Point Allocations to Health Policy by Stated Priority Ranking",
   # subtitle = "Respondents who ranked health as their top priority allocated more points to health",
    x = "",
    y = "Mean Points Allocated to Health \n (0–1 scale)",
    caption = "Bars show 95% confidence intervals around the mean"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption.position = "plot",  # ensures caption aligns with the plot area
    plot.caption = element_text(hjust = 0)  # left-justify the caption
  )

# -- Save the coefficient plot
ggsave(
  filename = "graphs/avgSupport_healthPointsvsRank.png",
  plot     = plot2,
  width    = 10,
  height   = 8,
  dpi      = 300
)
