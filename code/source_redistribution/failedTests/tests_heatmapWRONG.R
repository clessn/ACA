# ==============================================================
# Fairness Belief Diagnostics — Exploratory Structure Check
# Items: Proportionality + Reciprocity (ordinal 0–1 scale)
# ==============================================================

library(tidyverse)
library(corrplot)
library(gridExtra)

# ── Output folder ─────────────────────────────────────────────
out_path <- "code/source_redistribution/graphs/descriptives_fairness"
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

# ── Load data ─────────────────────────────────────────────────
df <- read.csv("data/clean_df_valid.csv")

# ── Variables ────────────────────────────────────────────────
prop_vars <- c(
  "redis_opportunity_num",
  "redis_intelligence_num",
  "redis_effort_num",
  "redis_reasons_poor_num",
  "redis_reasons_rich_num"
)

recip_vars <- c(
  "redis_social_benefits_num",
  "redis_welfare_num",
  "redis_no_cheat_system_num"
)

all_items <- c(prop_vars, recip_vars)

# ==============================================================
# 1. CORRELATION STRUCTURE
# ==============================================================

items_df <- df[, all_items]

cor_mat <- cor(items_df, use = "pairwise.complete.obs")

png(file.path(out_path, "01_item_correlation_heatmap.png"),
    width = 2000, height = 1600, res = 250)

corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.cex = 0.8,
  number.cex = 0.6,
  addCoef.col = "black"
)

dev.off()

# ==============================================================
# 2. ITEM DISTRIBUTIONS
# ==============================================================

df_long <- df |>
  select(all_of(all_items)) |>
  pivot_longer(cols = everything(),
               names_to = "item",
               values_to = "value")

p_dist <- ggplot(df_long, aes(x = value)) +
  geom_bar(fill = "grey60", colour = "white") +
  facet_wrap(~item, scales = "free_y") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Distribution of Fairness Items",
    x = NULL,
    y = "Count"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 9)
  )

ggsave(
  filename = file.path(out_path, "02_item_distributions.png"),
  plot = p_dist,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)

# ==============================================================
# 3. INDIVIDUAL RESPONSE PROFILES
# ==============================================================

df_long2 <- df |>
  mutate(id = row_number()) |>
  select(id, all_of(all_items)) |>
  pivot_longer(-id, names_to = "item", values_to = "value")

p_profiles <- ggplot(df_long2,
                     aes(x = item, y = value, group = id)) +
  geom_line(alpha = 0.04, colour = "black") +
  geom_point(alpha = 0.04, size = 0.5) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Individual Fairness Response Patterns",
    x = NULL,
    y = "Response (0–1 scale)"
  )

ggsave(
  filename = file.path(out_path, "03_individual_profiles.png"),
  plot = p_profiles,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)

# ==============================================================
# 4. OPTIONAL: PCA STRUCTURE CHECK
# ==============================================================

pca_data <- df[, all_items]

pca_data <- pca_data[complete.cases(pca_data), ]

pca <- prcomp(pca_data, scale. = TRUE)

# ==============================================================
# DONE
# ==============================================================

cat("\nAll diagnostic plots saved to:", out_path, "\n")
