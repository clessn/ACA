# ===================================================================
# POLICY PREFERENCE SURVEY DATA WRANGLING
# ===================================================================
# This script processes survey data with 100-point allocation questions
# to create binary preference indicators and intensity measures.
#
# Binary variables: 1 if policy received the most points (sole maximum)
#                   0 if tied or not the maximum (ties = no clear priority)
# Intensity: Deviation from mean allocation (positive = above average support)
# ===================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# -------------------------------------------------------------------
# FUNCTION: Create Policy Variables
# -------------------------------------------------------------------
# Processes one set of policy allocation questions to create:
# - Binary variables indicating top priority (ties set to 0)
# - Intensity variables measuring deviation from mean allocation
#
# Arguments:
#   df: dataframe containing policy columns
#   policy_cols: character vector of column names (e.g., c("q1_policy_a", "q1_policy_b"))
#   prefix: optional string to add to output variable names (e.g., "q1_")
# -------------------------------------------------------------------

create_policy_vars <- function(df, policy_cols, prefix = NULL) {
  # Calculate max value, number at max, and mean allocation for each respondent
  df <- df %>%
    rowwise() %>%
    mutate(
      max_val = max(c_across(all_of(policy_cols))),
      n_at_max = sum(c_across(all_of(policy_cols)) == max_val),
      mean_allocation = mean(c_across(all_of(policy_cols)))
    ) %>%
    ungroup()
  
  # Create binary and intensity variables for each policy
  for (col in policy_cols) {
    # Extract policy name (e.g., "policy_a" from "q1_policy_a")
    policy_name <- sub(".*_", "", col)
    
    # Create variable names with optional prefix
    if (!is.null(prefix)) {
      bin_name <- paste0(prefix, policy_name, "_bin")
      int_name <- paste0(prefix, policy_name, "_intensity")
    } else {
      bin_name <- paste0(col, "_bin")
      int_name <- paste0(col, "_intensity")
    }
    
    # Binary: 1 only if sole maximum (ties become 0)
    # Intensity: deviation from respondent's mean allocation
    df <- df %>%
      mutate(
        !!bin_name := as.integer(!!sym(col) == max_val & n_at_max == 1),
        !!int_name := !!sym(col) - mean_allocation
      )
  }
  
  # Clean up helper columns
  df <- df %>% select(-max_val, -n_at_max, -mean_allocation)
  
  return(df)
}

# -------------------------------------------------------------------
# APPLY FUNCTION TO ALL QUESTIONS
# -------------------------------------------------------------------
# Assumes columns are named like: q1_policy_a, q1_policy_b, etc.
# Adjust question_prefixes and policy_names to match your data
# -------------------------------------------------------------------

question_prefixes <- c("q1", "q2", "q3", "q4", "q5", "q6")
policy_names <- c("policy_a", "policy_b", "policy_c", "policy_d")

for (q in question_prefixes) {
  cols <- paste0(q, "_", policy_names)
  df <- create_policy_vars(df, cols, prefix = paste0(q, "_"))
}

# -------------------------------------------------------------------
# VISUALIZATION: Average Intensity by Group
# -------------------------------------------------------------------
# Shows how different groups prioritize policies (deviation from mean)
# Replace "spending_preference" with your independent variable
# -------------------------------------------------------------------

# Reshape data to long format for plotting
df_long <- df %>%
  pivot_longer(
    cols = matches("_intensity$"),
    names_to = "policy",
    values_to = "intensity"
  ) %>%
  mutate(
    policy = gsub("_intensity", "", policy),
    # CUSTOMIZE: Add your independent variable here
    # Example: spending_preference = ifelse(ideology > 5, "Higher Spending", "Spending Cuts")
    spending_preference = ifelse(ideology > 5, "Higher Spending", "Spending Cuts")
  )

# Plot average intensity by group with error bars
ggplot(df_long, aes(x = policy, y = intensity, fill = spending_preference)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Average Policy Preference Intensity by Spending Preference",
       x = "Policy", 
       y = "Deviation from Mean Allocation",
       fill = "Group") +
  theme_minimal()

# -------------------------------------------------------------------
# VISUALIZATION: Proportion Selecting Each Policy as Top Priority
# -------------------------------------------------------------------
# Shows what % of each group ranked each policy as their #1 choice
# Uses binary variables
# -------------------------------------------------------------------

# Calculate proportions for each group
policy_props <- df %>%
  group_by(spending_preference) %>%
  summarise(
    policy_a = mean(policy_a_bin, na.rm = TRUE),
    policy_b = mean(policy_b_bin, na.rm = TRUE),
    policy_c = mean(policy_c_bin, na.rm = TRUE),
    policy_d = mean(policy_d_bin, na.rm = TRUE)
  ) %>%
  pivot_longer(-spending_preference, names_to = "policy", values_to = "proportion")

ggplot(policy_props, aes(x = policy, y = proportion, fill = spending_preference)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion Ranking Each Policy as Top Priority",
       x = "Policy",
       y = "% Selecting as Top Priority",
       fill = "Group") +
  theme_minimal()

# -------------------------------------------------------------------
# STATISTICAL TESTING
# -------------------------------------------------------------------

# Test if intensity differs by group for a specific policy
t.test(policy_a_intensity ~ spending_preference, data = df)

# Regression approach for multiple predictors
model <- lm(policy_a_intensity ~ spending_preference + age + education, data = df)
summary(model)

# -------------------------------------------------------------------
# NOTES ON INTERPRETATION
# -------------------------------------------------------------------
# Intensity (deviation from mean):
#   - Positive values = above-average support for that policy
#   - Negative values = below-average support
#   - Values sum to zero within each respondent
#   - Useful for capturing "how much more" someone prioritized a policy
#
# Binary variables:
#   - 1 = policy received the most points (sole maximum)
#   - 0 = policy tied for max or was not the maximum
#   - Ties set to 0 because they indicate no clear priority
#   - Useful for understanding who considers each policy their top priority
#
# Why deviation from mean for intensity?
#   - Controls for individual response styles (some spread points, others concentrate)
#   - Measures relative prioritization within each person's allocation
#   - Better than raw points when you care about comparative preference
# ===================================================================