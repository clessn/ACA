# ==============================================================
# Heatmap — Fairness Profiles: Canada + Regions (Greyscale, CPP Theme)
# ==============================================================

library(tidyverse)
library(ggplot2)
library(grid)  # for unit()

# ── Output folder
out_path <- "code/source_redistribution/graphs/descriptives_fairness"
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

# ── Read data
df <- read.csv("data/clean_df_valid.csv")
unique(df$redis_opportunity_num)
# ── Variables
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

# ── Region factor
df$ses_region_cat <- factor(
  dplyr::recode(df$ses_region_cat, "East Coast" = "Atlantic Canada"),
  levels = c("Alberta", "Ontario", "Quebec", "Atlantic Canada")
)

# ── Profile construction
# ── Profile construction (Option 3: median split on standardized indices)

df <- df |>
  mutate(
    prop_index  = rowMeans(across(all_of(prop_vars)), na.rm = TRUE),
    recip_index = rowMeans(across(all_of(recip_vars)), na.rm = TRUE)
  ) |>
  mutate(
    prop_z  = as.numeric(scale(prop_index)),
    recip_z = as.numeric(scale(recip_index))
  )

prop_cut  <- median(df$prop_z, na.rm = TRUE)
recip_cut <- median(df$recip_z, na.rm = TRUE)

df <- df |>
  mutate(
    prop_fair  = prop_z  >= prop_cut,
    recip_fair = recip_z >= recip_cut,
    
    fairness_profile = case_when(
      prop_fair & !recip_fair  ~ "Market fair\nRedistribution unfair",
      prop_fair & recip_fair   ~ "Market fair\nRedistribution fair",
      !prop_fair & recip_fair  ~ "Market unfair\nRedistribution fair",
      !prop_fair & !recip_fair ~ "Market unfair\nRedistribution unfair"
    )
  )
#prop_cut  <- 0.66
#recip_cut <- 0.66
#
#df <- df |>
#  mutate(
#    n_valid_prop  = rowSums(!is.na(across(all_of(prop_vars)))),
#    n_valid_recip = rowSums(!is.na(across(all_of(recip_vars)))),
#    
#    prop_mean = ifelse(n_valid_prop >= 0,
#                       rowMeans(across(all_of(prop_vars)), na.rm = TRUE),
#                       NA_real_),
#    
#    recip_mean = ifelse(n_valid_recip >= 0,
#                        rowMeans(across(all_of(recip_vars)), na.rm = TRUE),
#                        NA_real_),
#    
#    # binary split
#    prop_fair  = prop_mean  >= prop_cut,
#    recip_fair = recip_mean >= recip_cut,
#    
#    fairness_profile = case_when(
#      is.na(prop_mean) | is.na(recip_mean) ~ NA_character_,
#      prop_fair & !recip_fair  ~ "Market fair\nRedistribution unfair",
#      prop_fair & recip_fair   ~ "Market fair\nRedistribution fair",
#      !prop_fair & recip_fair  ~ "Market unfair\nRedistribution fair",
#      !prop_fair & !recip_fair ~ "Market unfair\nRedistribution unfair"
#    )
#  )

# ── CPP journal theme
theme_cpp <- function(base_size = 11) {
  theme_classic(base_size = base_size) +
    theme(
      text               = element_text(family = "serif"),
      axis.line          = element_line(colour = "black", linewidth = 0.4),
      axis.ticks         = element_line(colour = "black", linewidth = 0.3),
      panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      strip.background   = element_blank(),
      strip.text         = element_text(face = "bold", size = rel(0.95)),
      legend.position    = "bottom",
      legend.key.size    = unit(0.4, "cm"),
      plot.title         = element_text(face = "bold", size = rel(1.05)),
      plot.caption       = element_text(size = rel(0.8), colour = "grey40",
                                        hjust = 0, margin = margin(t = 6))
    )
}

# ── Prepare heatmap data
region_data <- df |>
  filter(!is.na(fairness_profile), !is.na(ses_region_cat)) |>
  count(ses_region_cat, fairness_profile) |>
  group_by(ses_region_cat) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup() |>
  mutate(group = as.character(ses_region_cat))

canada_data <- df |>
  filter(!is.na(fairness_profile)) |>
  count(fairness_profile) |>
  mutate(pct = n / sum(n) * 100,
         group = "Canada")

heatmap_data <- bind_rows(canada_data, region_data) |>
  mutate(
    pct = as.numeric(pct),   # force numeric for continuous greyscale
    group = factor(group,
                   levels = c("Canada",
                              "Alberta",
                              "Ontario",
                              "Quebec",
                              "Atlantic Canada"))
  )

# ── Plot heatmap
p <- ggplot(heatmap_data,
            aes(x = group,
                y = fairness_profile,
                fill = pct)) +
  
  geom_tile(colour = "white", linewidth = 1.2) +
  
  # Percentage labels
  geom_text(
    aes(label = sprintf("%.1f%%", pct),
        colour = pct > 30),
    size = 3.8,
    fontface = "bold"
  ) +
  
  # Sample size labels
  geom_text(
    aes(label = paste0("n = ", n),
        colour = pct > 30),
    size = 2.9,
    vjust = 2.5
  ) +
  
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20"),
                      guide = "none") +
  
  # Continuous greyscale fill
  scale_fill_gradient(
    low = "grey95",   # light
    high = "grey20",  # dark
    limits = c(0, 50),
    name   = "% of sample",
    guide  = guide_colourbar(
      barwidth  = 10,
      barheight = 0.6,
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +
  
  scale_x_discrete(position = "top") +
  labs(
    title    = NULL,
    subtitle = NULL,
    x        = NULL,
    y        = NULL,
    caption  = "Note: Profiles based on mean scores across proportionality (5 items) and reciprocity (3 items). Threshold > 0.66 indicates 'fair' classification. Percentages are calculated over respondents."
  ) +
  
  theme_cpp(base_size = 12)

# ── Save plot
out_file <- file.path(out_path, "heatmap_fairness_profiles_region_canada_greyscale.png")
ggsave(filename = out_file,
       plot = p,
       width = 10,
       height = 6,
       dpi = 300,
       bg = "white")

cat("\nHeatmap saved to:", out_file, "\n")


#To decide, what are the latent items?

library(psych)

prop_vars_clean <- setdiff(prop_vars,
                           c("redis_opportunity_num",
                             "redis_intelligence_num"))

prop_items <- df[, prop_vars_clean]

recip_items <- df[, recip_vars]

# polychoric correlation matrix
prop_poly <- psych::polychoric(prop_items)$rho
recip_poly <- psych::polychoric(recip_items)$rho

# factor analysis
fa_prop <- fa(prop_poly, nfactors = 1, fm = "ml")
fa_recip <- fa(recip_poly, nfactors = 1, fm = "ml")

print(fa_prop$loadings)
print(fa_recip$loadings)