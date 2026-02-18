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
# Load wrangled and clean data (from aca_wrangling_W26.csv, now data/clean_df_valid.csv)
# -----------------------

df <- read.csv("data/clean_df_valid.csv")

# -------------------------------------------------------------------
# VISUALIZATION: Proportion Selecting Each Policy as Top Priority (intensity)
# -------------------------------------------------------------------
# Uses binary tradeoff intensity variables
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


# -------------------------------------------------------------------
# Five budget priority variables

budget_vars <- c(
  "budget_prio_health",
  "budget_prio_seniors",
  "budget_prio_cc",
  "budget_prio_ecn",
  "budget_prio_clim"
)

df$budget

# Reshape to long format
budget_long <- df %>%
  select(all_of(budget_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "policy",
    values_to = "points"
  )

# Plot density
ggplot(budget_long %>% filter(!is.na(points)), aes(x = points, fill = policy, color = policy)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Distribution of Points Across Budget Priorities",
    x = "Points",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)

# -------------------------------------------------------------------
# VISUALIZATION: Redistribution
# -------------------------------------------------------------------
# Binary variables

df_bin_long <- df %>%
  select(
    redis_effort_bin,
    redis_intelligence_bin,
    redis_no_cheat_system_bin,
    redis_opportunity_bin,
    redis_reasons_poor_bin,
    redis_reasons_rich_bin,
    redis_social_benefits_bin,
    redis_welfare_bin
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response"
  )

ggplot(df_bin_long, aes(x = factor(response))) +
  geom_bar(aes(y = after_stat(prop), group = question)) +
  facet_wrap(~ question, ncol = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Response (0 / 1)",
    y = "Proportion",
    title = "Distribution of Binary Redistribution Attitudes"
  ) +
  theme_minimal()

# Numeric variables
df_num_long <- df %>%
  select(
    redis_effort_num,
    redis_intelligence_num,
    redis_no_cheat_system_num,
    redis_opportunity_num,
    redis_reasons_poor_num,
    redis_reasons_rich_num,
    redis_social_benefits_num,
    redis_welfare_num
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response"
  )

ggplot(df_num_long, aes(x = response)) +
  geom_density() +
  facet_wrap(~ question, scales = "free_x", ncol = 4) +
  labs(
    x = "Response value",
    y = "Density",
    title = "Distribution of Numeric Redistribution Attitudes"
  ) +
  theme_minimal()

# Correlation tests for similar questions
rich_poor <- df[, c("redis_reasons_poor_num", "redis_reasons_rich_num")]

free_ride <- df[, c(
  "redis_social_benefits_num",
  "redis_welfare_num",
  "redis_no_cheat_system_num"
)]

opportunity <- df[, c(
  "redis_effort_num",
  "redis_intelligence_num",
  "redis_opportunity_num"
)]

cor(
  rich_poor,
  use = "pairwise.complete.obs",
  method = "pearson"
)

cor.test(
  rich_poor$redis_reasons_poor_num,
  rich_poor$redis_reasons_rich_num
)

cor(
  free_ride,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# Robustness check
cor(
  free_ride,
  use = "pairwise.complete.obs",
  method = "spearman"
)

cor(
  opportunity,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# Robustness check
cor(
  opportunity,
  use = "pairwise.complete.obs",
  method = "spearman"
)

pairwise_n <- function(x) {
  outer(
    colnames(x),
    colnames(x),
    Vectorize(function(i, j)
      sum(complete.cases(x[, c(i, j)])))
  )
}

pairwise_n(rich_poor)
pairwise_n(free_ride)
pairwise_n(opportunity)

# All moderate correlations between 0.19 and 0.33 -- performed a quick factor analysis and not worth persuing (between grooups and all)


