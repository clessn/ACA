#' === PLOT 1: FOCUSED PLOT OF MAIN PREDICTORS ===
#' -------------------------------------------
#' This graph shows the most important predictors, prioritizing those
#' that are statistically significant.

# Separation of significant and non-significant results
significant_results <- results %>%
  filter(p.value < 0.05) %>%
  arrange(desc(abs(estimate))) 

nonsignificant_results <- results %>%
  filter(p.value >= 0.05) %>%
  arrange(desc(abs(estimate)))

# Combine by prioritizing significant results
plot_results <- bind_rows(
  significant_results %>% head(20),  # Top 20 significant predictors
  nonsignificant_results %>% head(10) # Top 10 non-significant predictors with largest effect
) %>%
  arrange(desc(abs(estimate)))

# Mark small sample variables (following political science best practices)
plot_results <- plot_results %>%
  mutate(
    is_small_sample = term %in% small_sample_vars,
    # Create combined label for plotting that includes sample size for small sample variables
    term_with_n = if_else(
      is_small_sample,
      paste0(term, " (n=", muslim_count, ")"),
      term
    )
  )

# Calculate limits for the graph
x_min <- min(plot_results$LowerCI99, na.rm = TRUE) - 0.05
x_max <- max(plot_results$UpperCI99, na.rm = TRUE) + 0.2

# Creation of focused plot

plot_regression <- ggplot(plot_results, aes(x = estimate, y = reorder(term_with_n, abs(estimate)))) +
  # Points - with different shapes for small sample variables
  geom_point(aes(color = sig_level, shape = is_small_sample), size = 3.5) +
  # Confidence intervals
  geom_linerange(aes(xmin = LowerCI999, xmax = UpperCI999, color = sig_level), linewidth = 0.5) +
  geom_linerange(aes(xmin = LowerCI99, xmax = UpperCI99, color = sig_level), linewidth = 1.2) +
  geom_linerange(aes(xmin = LowerCI95, xmax = UpperCI95, color = sig_level), linewidth = 2.5) +
  # Theme and labels
  clessnize::theme_clean_light() +
  labs(x = "\nRegression Coefficient\n",
       y = "\nVariable\n",
       title = "Main Predictors of Abortion Support",
       subtitle = "Linear regression results with multiple confidence levels",
       caption = paste0(
         "Line thickness indicates confidence level: thick (95%), medium (99%), thin (99.9%)\n",
         "Triangle shape indicates small sample size variable (n<5% of dataset)"
       )) +
  # Scale and reference line
  scale_x_continuous(limits = c(x_min, x_max)) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  # T value labels
  geom_text(aes(x = ifelse(estimate > 0, 
                           UpperCI999 + 0.05, 
                           LowerCI999 - 0.05), 
                label = paste("t =", round(t_value, 2), significance)),
            hjust = ifelse(plot_results$estimate > 0, 0, 1),
            size = 4) +
  # Color and shape scales
  scale_color_manual(values = c("p < 0.001" = "#08519C",
                                "p < 0.01" = "#3182BD",
                                "p < 0.05" = "#6BAED6",
                                "Not significant" = "grey60"),
                     name = "Statistical Significance") +
  scale_shape_manual(values = c("TRUE" = 17, "FALSE" = 16),
                     labels = c("TRUE" = "Small sample size", "FALSE" = "Adequate sample size"),
                     name = "Sample Size") +
  # Theme adjustments
  theme(
    text = element_text(family = "Times New Roman"),  # Set global font
    plot.caption.position = "plot",
    axis.title.x = element_text(hjust = 0.5, size = 20),
    axis.title.y = element_text(hjust = 0.5, size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    plot.caption = element_text(size = 14, hjust = 0, lineheight = 0.9), # Smaller caption
    legend.position = "bottom",
    legend.title = element_text(size = 14),  # Smaller legend titles
    legend.text = element_text(size = 12)     # Smaller legend text
  )

cat("Saving focused plot...\n")
ggsave("graph/regression_predictors_focused.png", plot_regression, width = 12, height = 8, dpi = 300)