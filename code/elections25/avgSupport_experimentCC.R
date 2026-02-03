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

#df$education_bin <- ifelse(df$education == "University", 1, 0)
#df$age_young_bin <- ifelse(df$age == "18–34", 1, 0)
#df$income_high_bin <- ifelse(df$income == "High", 1, 0)
#df <- df %>%
#  mutate(ideo_right_bin = if_else(ideo_right_num. >= 0.5, 1, 0))

# === 1. Setup ===
# Dependent variables
dvs <- c(
  "tradeoff_childcare_num", # "Control"
  "tradeoff_childcare_higher_taxes_num", #"Higher taxes"
  "tradeoff_childcare_by_cutting_num", # "Cuts"
  "tradeoff_childcare_debt_num" # "More debt
)

# Labels for plotting
dv_labels <- c(
  tradeoff_childcare_num = "Control",
  tradeoff_childcare_higher_taxes_num = "Higher taxes",
  tradeoff_childcare_by_cutting_num = "Cuts",
  tradeoff_childcare_debt_num = "More debt"
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
  "trust_pol_parties_bin",  # Trust in political parties
  "budget_spend_prio_childcare_bin" # Priority for childcare spending
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


# === 4. Plot difference with control ===
control_vals <- pred_all %>% 
  filter(treatment == "Control") %>%
  select(model, estimate)


plot_data <- pred_all %>% 
  filter(treatment != "Control")

plot2 <- ggplot(plot_data, aes(x = treatment, y = estimate, color = model, shape = model)) +
  geom_hline(data = control_vals, aes(yintercept = estimate, color = model),
             linetype = "dashed", show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5),
    width = 0.15
  ) +
  scale_color_manual(
    values = c("Without Covariates" = "black", "With Covariates" = "grey50")
  ) +
  scale_shape_manual(
    values = c("Without Covariates" = 16, "With Covariates" = 17)
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    x = NULL,
    y = "Predicted Support \n(0–1 scale)",
    color = "Model Type",
    shape = "Model Type",
    caption = "Dashed line = predicted support in control condition. \n Covariate-adjusted models include: age, gender, education, employment, children, homeownership, ideology, and trust."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0),
    axis.text.x = element_text(margin = margin(t = 5))
  )


# === 5. Plot all ===
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
    #title = "Average Support for Policy by Tradeoff",
    #subtitle = "OLS predicted means with and without covariates (95% CIs)",
    x = " ",
    y = "Predicted Support \n (0–1 scale)",
    color = "Model Type",
    shape = "Model Type",
    caption = "Covariate-adjusted models include: age, gender, education, employment, children, \n homeownership, ideology,trust, and priority for child care spending."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption.position = "plot",  # ensures caption aligns with the plot area
    plot.caption = element_text(hjust = 0)  # left-justify the caption
  )

plot

# !!! To help me visualize
control_vals <- pred_all %>% 
  filter(treatment == "Control")

plot + 
  geom_rect(
    data = control_vals,
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = conf.low, ymax = conf.high,
      fill = model
    ),
    alpha = 0.1,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c(
    "Without Covariates" = "black",
    "With Covariates" = "grey70"
  ))
  )

# -- 8. Save the coefficient plot
ggsave(
  filename = "graphs/avgSupport_experimentCC_reference.png",
  plot     = plot2,
  width    = 10,
  height   = 8,
  dpi      = 300
)
