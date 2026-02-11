# ===================================================================
# DESCRIPTIVE STATISTICS PRIOR TO MODELS
# ===================================================================

# -----------------------
# Load packages
# -----------------------
library(tidyverse)
library(tidyr)
library(dplyr)

# -----------------------
# Load cleaned data (from aca_wrangling_W26.csv)
# -----------------------

df <- read.csv("data/clean_df_full.csv")


# -------------------------------------------------------------------
# VISUALIZATION: Proportion Selecting Each Policy as Top Priority
# -------------------------------------------------------------------
# Shows what % of each group ranked each policy as their #1 choice
# Uses binary variables
# -------------------------------------------------------------------

# For cc1 question
cc1_data <- DataClean %>%
  select(starts_with("tradeoff_cc1_") & ends_with("_intensity")) %>%
  pivot_longer(everything(), names_to = "policy", values_to = "points") %>%
  mutate(policy = str_remove(policy, "tradeoff_cc1_") %>% str_remove("_intensity"))

ggplot(cc1_data, aes(x = policy, y = points)) +
  geom_boxplot() +
  labs(title = "Distribution of Points Allocated - CC1 Question",
       x = "Policy Option", y = "Points Allocated") +
  theme_minimal()

ggplot(cc1_data, aes(x = points, fill = policy)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Points - CC1 Question",
       x = "Points Allocated", y = "Density") +
  theme_minimal()

# Summary statistics for each policy within a question
DataClean %>%
  select(starts_with("tradeoff_cc1_") & ends_with("_intensity")) %>%
  pivot_longer(everything(), names_to = "policy", values_to = "points") %>%
  group_by(policy) %>%
  summarise(
    mean = mean(points, na.rm = TRUE),
    median = median(points, na.rm = TRUE),
    sd = sd(points, na.rm = TRUE),
    pct_zero = mean(points == 0, na.rm = TRUE) * 100,
    pct_strong = mean(points >= 50, na.rm = TRUE) * 100
  )

# Faceted histogram
ggplot(cc1_data, aes(x = points)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  facet_wrap(~policy, ncol = 2) +
  labs(title = "Distribution of Points - CC1 Question",
       x = "Points Allocated", y = "Count") +
  theme_minimal()

# Average allocation across all questions
summary_all <- DataClean %>%
  select(ends_with("_intensity")) %>%
  pivot_longer(everything(), names_to = "policy", values_to = "points") %>%
  mutate(
    question = str_extract(policy, "tradeoff_[^_]+"),
    policy = str_remove(policy, "tradeoff_[^_]+_") %>% str_remove("_intensity")
  ) %>%
  group_by(question, policy) %>%
  summarise(mean_points = mean(points, na.rm = TRUE), .groups = "drop")

ggplot(summary_all, aes(x = policy, y = question, fill = mean_points)) +
  geom_tile() +
  geom_text(aes(label = round(mean_points, 1)), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Average Points Allocated by Question and Policy",
       fill = "Mean Points") +
  theme_minimal()
