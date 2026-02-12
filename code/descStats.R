# ===================================================================
# DESCRIPTIVE STATISTICS PRIOR TO MODELS
# ===================================================================

# -----------------------
# Load packages
# -----------------------
library(tidyverse)
library(tidyr)
library(dplyr)
df
# -----------------------
# Load wrangled and clean data (from aca_wrangling_W26.csv, now data/clean_df_valid.csv)
# -----------------------

df <- read.csv("data/clean_df_valid.csv")

# -------------------------------------------------------------------
# VISUALIZATION: Proportion Selecting Each Policy as Top Priority (intensity)
# -------------------------------------------------------------------
# Uses binary variables
# -------------------------------------------------------------------

# Intense allocation across all tradeoff questions
summary_all <- df %>%
  # select all columns ending with "_intense"
  select(ends_with("_intense")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "policy",
    values_to = "points"
  ) %>%
  mutate(
    # extract the question type (cc1, cc2, ge, etc.)
    question = str_extract(policy, "^tradeoff_[^_]+"),
    # clean policy name
    policy = str_remove(policy, "^tradeoff_[^_]+_") %>% str_remove("_intense")
  ) %>%
  group_by(question, policy) %>%
  summarise(
    mean_points = mean(points, na.rm = TRUE),
    .groups = "drop"
  )

# Reshape the data
all_points <- df %>%
  select(ends_with("_intense")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "policy",
    values_to = "points"
  ) %>%
  mutate(
    question = str_extract(policy, "^tradeoff_[^_]+"),
    policy = str_remove(policy, "^tradeoff_[^_]+_") %>% str_remove("_intense")
  )

# Plot 
ggplot(all_points %>% filter(!is.na(points)), 
       aes(x = points, color = policy, fill = policy)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ question, scales = "free_y") +
  labs(
    title = "Distribution of Points Across Policies",
    x = "Points",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

# Count intense preferences for all tradeoff questions
all_counts <- df %>%
  select(matches("^tradeoff_.*_intense$")) %>%       # select all tradeoff intense columns
  pivot_longer(
    cols = everything(),
    names_to = "full_var",
    values_to = "intense"
  ) %>%
  mutate(
    question = str_extract(full_var, "^tradeoff_[^_]+"),       # extract question, e.g., tradeoff_cc1
    policy = str_remove(full_var, "^tradeoff_[^_]+_") %>%      # remove question prefix
      str_remove("_intense")                             # remove suffix
  ) %>%
  group_by(question, policy) %>%
  summarise(
    n_intense = sum(intense, na.rm = TRUE),
    n_total = sum(!is.na(intense)),
    pct_intense = n_intense / n_total * 100,
    .groups = "drop"
  )

all_counts

# Bar chart of intense preferences per policy per question
ggplot(all_counts, aes(x = policy, y = pct_intense, fill = policy)) +
  geom_col(show.legend = FALSE) +             # simple bars, no legend
  facet_wrap(~ question, scales = "free_x") + # one panel per question
  labs(
    title = "Percentage of Respondents with Intense Preferences",
    x = "Policy",
    y = "Percent (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # rotate x labels
  )
