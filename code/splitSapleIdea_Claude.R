# ===================================================================
# COMPARING SPLIT-SAMPLE QUESTION WORDINGS
# ===================================================================
# This script compares responses between two different question wordings
# administered to randomly split samples.
#
# Assumes you have a variable indicating wording condition (e.g., "wording_condition")
# with values like "version_a" and "version_b"
# ===================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# -------------------------------------------------------------------
# 1. BALANCE CHECK
# -------------------------------------------------------------------
# Verify that the two wording groups are balanced on key demographics
# Important for establishing that differences are due to wording, not sample composition
# -------------------------------------------------------------------

balance_check <- df %>% 
  group_by(wording_condition) %>%
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    pct_female = mean(female, na.rm = TRUE),
    # Add other demographic variables as needed
    mean_education = mean(education, na.rm = TRUE),
    mean_income = mean(income, na.rm = TRUE)
  )

print("Balance Check Across Wording Conditions:")
print(balance_check)

# -------------------------------------------------------------------
# 2. SIMPLE COMPARISON: DESCRIPTIVE STATISTICS
# -------------------------------------------------------------------
# Compare mean intensity for each policy by wording condition
# -------------------------------------------------------------------

comparison_stats <- df %>%
  group_by(wording_condition) %>%
  summarise(
    # Policy A
    mean_policy_a = mean(policy_a_intensity, na.rm = TRUE),
    se_policy_a = sd(policy_a_intensity, na.rm = TRUE) / sqrt(n()),
    
    # Policy B
    mean_policy_b = mean(policy_b_intensity, na.rm = TRUE),
    se_policy_b = sd(policy_b_intensity, na.rm = TRUE) / sqrt(n()),
    
    # Policy C
    mean_policy_c = mean(policy_c_intensity, na.rm = TRUE),
    se_policy_c = sd(policy_c_intensity, na.rm = TRUE) / sqrt(n()),
    
    # Policy D
    mean_policy_d = mean(policy_d_intensity, na.rm = TRUE),
    se_policy_d = sd(policy_d_intensity, na.rm = TRUE) / sqrt(n()),
    
    n = n()
  )

print("Mean Intensity by Wording Condition:")
print(comparison_stats)

# -------------------------------------------------------------------
# 3. T-TESTS: Test if wording affects preference for each policy
# -------------------------------------------------------------------

print("T-Tests for Wording Effect on Each Policy:")
print("Policy A:")
print(t.test(policy_a_intensity ~ wording_condition, data = df))

print("Policy B:")
print(t.test(policy_b_intensity ~ wording_condition, data = df))

print("Policy C:")
print(t.test(policy_c_intensity ~ wording_condition, data = df))

print("Policy D:")
print(t.test(policy_d_intensity ~ wording_condition, data = df))

# -------------------------------------------------------------------
# 4. VISUALIZATION: Side-by-Side Bar Chart
# -------------------------------------------------------------------
# Shows average intensity for each policy, split by wording condition
# Error bars represent standard error
# -------------------------------------------------------------------

# Reshape data for plotting
df_compare <- df %>%
  pivot_longer(
    cols = matches("_intensity$"),
    names_to = "policy",
    values_to = "intensity"
  ) %>%
  mutate(policy = gsub("_intensity", "", policy))

# Create bar chart with error bars
ggplot(df_compare, aes(x = policy, y = intensity, fill = wording_condition)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = "mean_se",
                position = position_dodge(width = 0.9), width = 0.25) +
  labs(title = "Policy Preferences by Question Wording",
       subtitle = "Error bars show standard error of the mean",
       x = "Policy",
       y = "Mean Intensity (Deviation from Average Allocation)",
       fill = "Wording Condition") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme(legend.position = "bottom")

# -------------------------------------------------------------------
# 5. VISUALIZATION: Line Plot (Preference Patterns)
# -------------------------------------------------------------------
# Shows how preference patterns differ across wording conditions
# Useful for seeing if wording shifts relative preferences
# -------------------------------------------------------------------

# Calculate means and standard errors
df_diff <- df_compare %>%
  group_by(wording_condition, policy) %>%
  summarise(
    mean_intensity = mean(intensity, na.rm = TRUE),
    se = sd(intensity, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Create line plot
ggplot(df_diff, aes(x = policy, y = mean_intensity, 
                    color = wording_condition, group = wording_condition)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_intensity - se, ymax = mean_intensity + se),
                width = 0.2) +
  labs(title = "Wording Effect on Policy Preference Patterns",
       subtitle = "Lines show preference pattern for each wording condition",
       x = "Policy",
       y = "Mean Intensity (Deviation from Average Allocation)",
       color = "Wording Condition") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme(legend.position = "bottom")

# -------------------------------------------------------------------
# 6. STATISTICAL TEST: ANOVA with Interaction
# -------------------------------------------------------------------
# Tests whether wording affects preferences differently across policies
# Useful if you expect wording to have varying effects on different policies
# -------------------------------------------------------------------

# ANOVA model with interaction
model_anova <- lm(intensity ~ wording_condition * policy, data = df_compare)
print("ANOVA Results:")
print(anova(model_anova))
print(summary(model_anova))

# -------------------------------------------------------------------
# 7. REGRESSION WITH COVARIATES (Optional)
# -------------------------------------------------------------------
# Test wording effect while controlling for demographics
# Run separately for each policy of interest
# -------------------------------------------------------------------

# Example for Policy A
model_policy_a <- lm(policy_a_intensity ~ wording_condition + age + education + income, 
                     data = df)
print("Regression Model for Policy A (with covariates):")
print(summary(model_policy_a))

# Example for Policy B
model_policy_b <- lm(policy_b_intensity ~ wording_condition + age + education + income, 
                     data = df)
print("Regression Model for Policy B (with covariates):")
print(summary(model_policy_b))

# -------------------------------------------------------------------
# 8. EFFECT SIZE CALCULATION
# -------------------------------------------------------------------
# Cohen's d for effect size - helps assess practical significance
# beyond statistical significance
# -------------------------------------------------------------------

# Function to calculate Cohen's d
cohens_d <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  mean_diff <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
  pooled_sd <- sqrt(((n1-1)*var(x, na.rm = TRUE) + (n2-1)*var(y, na.rm = TRUE)) / (n1+n2-2))
  d <- mean_diff / pooled_sd
  return(d)
}

# Calculate effect sizes for each policy
version_a_data <- df %>% filter(wording_condition == "version_a")
version_b_data <- df %>% filter(wording_condition == "version_b")

effect_sizes <- data.frame(
  policy = c("policy_a", "policy_b", "policy_c", "policy_d"),
  cohens_d = c(
    cohens_d(version_a_data$policy_a_intensity, version_b_data$policy_a_intensity),
    cohens_d(version_a_data$policy_b_intensity, version_b_data$policy_b_intensity),
    cohens_d(version_a_data$policy_c_intensity, version_b_data$policy_c_intensity),
    cohens_d(version_a_data$policy_d_intensity, version_b_data$policy_d_intensity)
  )
)

print("Effect Sizes (Cohen's d):")
print(effect_sizes)
print("Interpretation: |d| < 0.2 = small, 0.2-0.5 = small-medium, 0.5-0.8 = medium, > 0.8 = large")

# -------------------------------------------------------------------
# NOTES ON INTERPRETATION
# -------------------------------------------------------------------
# Balance check: Groups should be similar on demographics. Large imbalances
#   suggest randomization issues and may require weighting or controls.
#
# T-tests: Show whether wording significantly affects each policy preference.
#   Look at both p-values and mean differences.
#
# Effect sizes: Statistical significance doesn't always mean practical importance.
#   Cohen's d helps assess magnitude: d=0.5 means wording shifts preference
#   by half a standard deviation.
#
# Interaction in ANOVA: Tests if wording affects policies differently.
#   Significant interaction = wording changes relative preference between policies.
#
# Focus your analysis: If wording targets one specific policy, focus on that
#   comparison rather than testing all policies (reduces multiple testing issues).
# ===================================================================