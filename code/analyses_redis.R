
# ============================================================
# Redistribution Attitudes — Descriptives + Ordered Logit
# Four-region Canadian survey: Alberta, Ontario, Quebec, Eastern Canada
#
# DVs: six redistribution items, coded 0-1 across ordered categories
# Strategy: ordered logit (polr) as main model
#           HC1-equivalent robust SEs via marginaleffects vcov="HC1"
#           AMEs reported for the highest response category
# ============================================================

#You're welcome! You've got a solid, consistent analytical framework across both scripts now. A few things worth keeping in mind as you move forward:
# When you run the redis script, the one thing most likely to need adjustment is the DV conversion in Section 1. If your items don't all share the same category spacing (e.g. some are 3-point, others 4-point), the ordered factor conversion handles that automatically — but double-check the desc_redis_distributions.png output first to confirm the categories look right before running the regressions.
# For publication, the two main methodological points to be ready to defend are the LPM vs logit choice in the budgetary script (the side-by-side robustness plot in Section 7.6 is your answer to that) and the OLS-for-visualization note in the redis script's ideology × Quebec plots.
# If a reviewer raises the compositional data issue on the budget_prio_X raw scores, the comment in Section 8 of the budgetary script already flags Dirichlet regression as the rigorous alternative — worth keeping that in your back pocket.


# 0.1. Packages
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(marginaleffects)
library(modelsummary)
library(MASS)          # polr()
library(sandwich)
library(lmtest)
library(ggthemes)

# 0.2. Load data
df <- read.csv("data/clean_df_valid.csv")

# 0.3. Output folders
dir.create("graphs/descriptives", recursive = TRUE, showWarnings = FALSE)
dir.create("graphs/regressions",  recursive = TRUE, showWarnings = FALSE)


#*************************************************************************#
#########              1. VARIABLE CONSTRUCTION                   #########
#*************************************************************************#

# University-education binary (reference = below university)
df$univ_educ_bin <- as.integer(df$educ_group == "educUniv")

# High-income binary (reference = low + mid)
if (!"incomeHigh_bin" %in% names(df)) {
  df$incomeHigh_bin <- as.integer(df$ses_income3Cat == "High")
}

# Ordered factors — kept as factors for educ and income (hybrid approach)
df$educ_group     <- factor(df$educ_group,
                             levels = c("educBHS", "educHS", "educUniv"))
df$ses_region_cat <- factor(df$ses_region_cat,
                             levels = c("Ontario", "Quebec", "Alberta", "East Coast"))
df$ses_income3Cat <- factor(df$ses_income3Cat,
                             levels = c("Low", "Mid", "High"))

# DVs must be ordered factors for polr()
# Reconstruct from the 0-1 numeric coding.
# Adjust the levels vector below if your categories differ.
redis_levels <- c("0", "0.33", "0.67", "1")   # <-- edit if needed

dv_vars <- c(
  "redis_effort_num",
  "redis_no_cheat_system_num",
  "redis_reasons_poor_num",
  "redis_reasons_rich_num",
  "redis_social_benefits_num",
  "redis_welfare_num"
)

dv_labels <- c(
  redis_effort_num          = "Fairness of income distribution",
  redis_no_cheat_system_num = "Trust not to cheat system",
  redis_reasons_poor_num    = "Non-violating outcomes (Poor)",
  redis_reasons_rich_num    = "Non-violating outcomes (Rich)",
  redis_social_benefits_num = "Social benefits not a choice",
  redis_welfare_num         = "Welfare does not go to undeserving"
)

# Convert DVs to ordered factors for polr()
# Using as.character() first so factor levels match the observed values exactly.
for (v in dv_vars) {
  observed_levels <- sort(unique(na.omit(df[[v]])))
  df[[paste0(v, "_ord")]] <- factor(
    as.character(df[[v]]),
    levels = as.character(observed_levels),
    ordered = TRUE
  )
}

dv_ord_vars <- paste0(dv_vars, "_ord")


#*************************************************************************#
#########              2. DEFINE IVs AND MODEL RHS               #########
#*************************************************************************#

# ── 2.1. IV list — hybrid: factors for educ/income, numeric age,
#                           binaries for region (ref = Ontario) ──────────
ivs <- list(
  list(type="binary",  var="quebec_bin",               low=0, high=1,
       label="Quebec (No=0 vs Yes=1)"),
  list(type="binary",  var="ideo_define_QC_first_bin",  low=0, high=1,
       label="Quebecker First (No=0 vs Yes=1)"),
  list(type="binary",  var="alberta_bin",              low=0, high=1,
       label="Alberta (No=0 vs Yes=1)"),
  list(type="binary",  var="ontario_bin",              low=0, high=1,
       label="Ontario (No=0 vs Yes=1)"),
  list(type="binary",  var="region_eastcoast_bin",     low=0, high=1,
       label="Eastern Canada (No=0 vs Yes=1)"),
  list(type="binary",  var="ses_male_bin",             low=0, high=1,
       label="Gender (Female=0 vs Male=1)"),
  list(type="numeric", var="ses_age",                  low=30, high=60,
       label="Age (30 vs 60)"),
  list(type="factor",  var="educ_group",               low="educBHS", high="educUniv",
       levels=c("educBHS","educHS","educUniv"),
       label="Education (Below HS vs University)"),
  list(type="factor",  var="ses_income3Cat",           low="Low", high="High",
       levels=c("Low","Mid","High"),
       label="Income (Low vs High)"),
  list(type="binary",  var="ses_citizenYes_bin",       low=0, high=1,
       label="Citizen (No=0 vs Yes=1)"),
  list(type="binary",  var="ses_french_bin",           low=0, high=1,
       label="French-speaking (No=0 vs Yes=1)"),
  list(type="binary",  var="trust_social_bin",         low=0, high=1,
       label="Social Trust (Low=0 vs High=1)"),
  list(type="binary",  var="employ_fulltime_bin",      low=0, high=1,
       label="Employed Full-Time (No=0 vs Yes=1)"),
  list(type="numeric", var="ideo_right_num",           low=0, high=1,
       label="Ideology: Left (0) vs Right (1)"),
  list(type="numeric", var="ideo_interest_politics_num", low=0, high=1,
       label="Political Interest: Low (0) vs High (1)"),
  list(type="binary",  var="vote_PLC_bin",             low=0, high=1,
       label="Liberal voter (No=0 vs Yes=1)"),
  list(type="binary",  var="vote_PCC_bin",             low=0, high=1,
       label="Conservative voter (No=0 vs Yes=1)")
)

# ── 2.2. RHS formula ─────────────────────────────────────────
# Ontario is the omitted region reference (quebec_bin + alberta_bin +
# region_eastcoast_bin). educ_group and ses_income3Cat kept as factors.
rhs <- "quebec_bin + ideo_define_QC_first_bin + alberta_bin + region_eastcoast_bin +
        ses_male_bin + ses_age + educ_group + ses_income3Cat +
        ses_citizenYes_bin + ses_french_bin + trust_social_bin +
        employ_fulltime_bin + ideo_right_num + ideo_interest_politics_num +
        vote_PLC_bin + vote_PCC_bin"

# ── 2.3. Term labels ─────────────────────────────────────────
term_labels <- c(
  "quebec_bin"                 = "Quebec",
  "ideo_define_QC_first_bin"   = "Quebecker First",
  "alberta_bin"                = "Alberta",
  "region_eastcoast_bin"       = "Eastern Canada",
  "ses_male_bin"               = "Male",
  "ses_age"                    = "Age",
  "educ_groupeducHS"           = "Education: HS vs Below HS",
  "educ_groupeducUniv"         = "Education: Univ vs Below HS",
  "ses_income3CatMid"          = "Income: Mid vs Low",
  "ses_income3CatHigh"         = "Income: High vs Low",
  "ses_citizenYes_bin"         = "Citizen",
  "ses_french_bin"             = "French-speaking",
  "trust_social_bin"           = "Social Trust",
  "employ_fulltime_bin"        = "Employed Full-Time",
  "ideo_right_num"             = "Ideology (Right)",
  "ideo_interest_politics_num" = "Political Interest",
  "vote_PLC_bin"               = "Liberal voter",
  "vote_PCC_bin"               = "Conservative voter",
  # avg_slopes factor contrast labels
  "Education: HS vs Below HS"  = "Education: HS vs Below HS",
  "Education: Univ vs Below HS"= "Education: Univ vs Below HS",
  "Income: Mid vs Low"         = "Income: Mid vs Low",
  "Income: High vs Low"        = "Income: High vs Low"
)


#*************************************************************************#
#########        3. HELPER FUNCTIONS                              #########
#*************************************************************************#

robust_vcov <- function(model) vcovHC(model, type = "HC1")

# ── 3.1. Bivariate marginal means plot (from original, upgraded) ──────
# Fits a bivariate OLS (for predictions on 0-1 scale), adds robust SEs.
plot_marginal <- function(dv, dv_label, iv, data) {
  model_data <- data |> select(all_of(c(dv, iv$var))) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", iv$var)), data = model_data)
  vm         <- robust_vcov(model)

  plot_data <- if (iv$type == "numeric") {
    nd <- data.frame(x = c(iv$low, iv$high))
    names(nd) <- iv$var
    predictions(model, newdata = nd, vcov = vm) |>
      mutate(x_label = factor(.data[[iv$var]],
                              levels = c(iv$low, iv$high),
                              labels = c(as.character(iv$low), as.character(iv$high))))
  } else if (iv$type == "binary") {
    nd <- data.frame(x = c(0, 1))
    names(nd) <- iv$var
    predictions(model, newdata = nd, vcov = vm) |>
      mutate(x_label = factor(.data[[iv$var]],
                              levels = c(0, 1),
                              labels = c(as.character(iv$low), as.character(iv$high))))
  } else {
    predictions(model, by = iv$var, vcov = vm) |>
      mutate(x_label = .data[[iv$var]])
  }

  p <- ggplot(plot_data, aes(x = x_label, y = estimate)) +
    geom_point(size = 3, color = "#2166ac") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.12, color = "#2166ac") +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::label_number(accuracy = 0.1)) +
    labs(x = iv$label, y = "Predicted value (0-1 scale)", title = dv_label,
         caption = "OLS prediction. HC1 robust SEs. Error bars = 95% CI.") +
    theme_minimal(base_size = 13)

  ggsave(paste0("graphs/descriptives/marginal_", iv$var, "_", dv, ".png"),
         plot = p, width = 6, height = 4, dpi = 300)
  invisible(p)
}

# ── 3.2. Bivariate contrast table (robust SEs) ───────────────
interpret_contrast <- function(dv, dv_label, iv, data) {
  iv_var  <- iv$var
  iv_low  <- as.character(iv$low)
  iv_high <- as.character(iv$high)

  model_data <- data |> select(all_of(c(dv, iv_var))) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", iv_var)), data = model_data)
  vm         <- robust_vcov(model)

  contrast_spec        <- list(c(iv$low, iv$high))
  names(contrast_spec) <- iv_var

  tryCatch({
    avg_comparisons(model, variables = contrast_spec,
                    vcov = vm, newdata = model_data) |>
      as_tibble() |>
      transmute(
        iv        = iv$label,
        dv        = dv_label,
        contrast  = paste(iv_low, "vs", iv_high),
        estimate  = round(estimate, 3),
        conf.low  = round(conf.low, 3),
        conf.high = round(conf.high, 3),
        p.value   = round(p.value, 3),
        sig = case_when(
          p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
          p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".", TRUE ~ ""
        ),
        direction = case_when(
          conf.low  > 0 ~ paste(iv_high, ">", iv_low),
          conf.high < 0 ~ paste(iv_low, ">", iv_high),
          TRUE          ~ "No clear difference"
        )
      )
  }, error = function(e) {
    cat("FAILED:", iv_var, "~", dv, "\n  Error:", conditionMessage(e), "\n")
    tibble(iv=iv$label, dv=dv_label, contrast=paste(iv_low,"vs",iv_high),
           estimate=NA_real_, conf.low=NA_real_, conf.high=NA_real_,
           p.value=NA_real_, sig=NA_character_, direction="ERROR")
  })
}

# ── 3.3. Ordered logit AME helper ────────────────────────────
# polr() returns AMEs per outcome level. We extract:
#   (a) full level-by-level table saved to CSV
#   (b) AME for the highest category only (most interpretable summary)
tidy_polr_slopes <- function(model, dv_label, question = NULL, data = df) {
  fml        <- formula(model)
  model_vars <- all.vars(fml)
  model_data <- data |> select(all_of(model_vars)) |> drop_na()
  # Refit on clean data to avoid .x pronoun issue inside map2
  model_clean <- MASS::polr(fml, data = model_data, Hess = TRUE)

  # avg_slopes with HC1 — returns one row per predictor per DV level
  slopes_all <- avg_slopes(model_clean, vcov = "HC1",
                           newdata = model_data) |>
    as_tibble() |>
    mutate(
      dv       = dv_label,
      question = question,
      term_clean = case_when(
        term == "educ_group"     & contrast == "educHS - educBHS"   ~ "Education: HS vs Below HS",
        term == "educ_group"     & contrast == "educUniv - educBHS" ~ "Education: Univ vs Below HS",
        term == "ses_income3Cat" & contrast == "Mid - Low"          ~ "Income: Mid vs Low",
        term == "ses_income3Cat" & contrast == "High - Low"         ~ "Income: High vs Low",
        TRUE ~ term
      )
    )

  # Highest outcome level (last factor level = 1 or closest to 1)
  top_level <- slopes_all |> pull(group) |> as.character() |> unique() |> sort() |> tail(1)

  slopes_top <- slopes_all |>
    filter(as.character(group) == top_level) |>
    transmute(
      question, dv, term = term_clean,
      outcome_level = top_level,
      estimate  = round(estimate, 3),
      conf.low  = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value   = round(p.value, 3),
      sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".", TRUE ~ ""
      ),
      direction = case_when(
        conf.low  > 0 ~ "Positive",
        conf.high < 0 ~ "Negative",
        TRUE          ~ "No clear effect"
      )
    )

  list(top = slopes_top, full = slopes_all)
}

# ── 3.4. Coefficient plot helper ─────────────────────────────
plot_coefs <- function(coef_df, title_str, file_path, ncol_facet = 2) {
  coef_df |>
    mutate(term = recode(term, !!!term_labels)) |>
    ggplot(aes(x = estimate, y = reorder(term, estimate), color = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    scale_color_manual(values = c(
      "Positive"       = "#2166ac",
      "Negative"       = "#d6604d",
      "No clear effect"= "grey60"
    )) +
    facet_wrap(~ dv, ncol = ncol_facet, scales = "free_x") +
    labs(x = "Average marginal effect (HC1 robust SEs)", y = NULL,
         color = NULL, title = title_str,
         caption = "AME on P(highest response category). Ordered logit.") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold"))
  ggsave(file_path, width = 14, height = 10, dpi = 300)
}

# ── 3.5. R-squared summary plot (using McFadden pseudo-R2) ───
plot_r2 <- function(fit_df, title_str, file_path) {
  fit_df |>
    mutate(
      fit_level = case_when(
        pseudo_r2 >= 0.07 ~ "High (>=0.07)",
        pseudo_r2 >= 0.03 ~ "Moderate (0.03-0.07)",
        TRUE              ~ "Low (<0.03)"
      ),
      fit_level = factor(fit_level,
                         levels = c("High (>=0.07)", "Moderate (0.03-0.07)", "Low (<0.03)")),
      dv = fct_reorder(dv, pseudo_r2)
    ) |>
    ggplot(aes(x = pseudo_r2, y = dv, fill = fit_level)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.3f", pseudo_r2)), hjust = -0.15, size = 3.2) +
    scale_fill_manual(values = c(
      "High (>=0.07)"        = "#2166ac",
      "Moderate (0.03-0.07)" = "#92c5de",
      "Low (<0.03)"          = "#d6604d"
    )) +
    scale_x_continuous(limits = c(0, 0.20),
                       labels = scales::label_number(accuracy = 0.01)) +
    labs(x = "McFadden pseudo-R2", y = NULL, fill = "Model fit",
         title = title_str) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank())
  ggsave(file_path, width = 8, height = 5, dpi = 300)
}


#*************************************************************************#
#########   4. DESCRIPTIVE STATISTICS — BAR CHARTS               #########
#*************************************************************************#
# Show the distribution of each DV across its response categories.

desc_redis <- map_dfr(dv_vars, function(v) {
  df |>
    count(.data[[v]]) |>
    mutate(
      dv    = dv_labels[[v]],
      value = as.character(.data[[v]]),
      pct   = n / sum(n) * 100
    ) |>
    rename(raw_value = 1)
})

desc_redis |>
  mutate(raw_value = factor(raw_value, levels = sort(unique(as.numeric(raw_value))))) |>
  ggplot(aes(x = raw_value, y = pct, fill = raw_value)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.4, size = 3.2) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  facet_wrap(~ dv, ncol = 2, scales = "free_x") +
  labs(x = "Response category (0 = low, 1 = high)",
       y = "% of respondents",
       title = "Distribution of redistribution attitude items") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

ggsave("graphs/descriptives/desc_redis_distributions.png",
       width = 12, height = 10, dpi = 300)


#*************************************************************************#
#########   5. BIVARIATE MARGINAL MEANS                          #########
#*************************************************************************#
# One plot per IV x DV (uses OLS on 0-1 numeric DV for interpretability).
# Results saved to graphs/descriptives/.

walk(ivs, function(iv) {
  walk2(dv_vars, dv_labels, plot_marginal, iv = iv, data = df)
})

# Bivariate contrast table (robust SEs)
bivar_contrasts <- map_dfr(ivs, function(iv) {
  map2_dfr(dv_vars, dv_labels, interpret_contrast, iv = iv, data = df)
})

write.csv(bivar_contrasts,
          "graphs/regressions/bivar_contrasts_redis.csv",
          row.names = FALSE)


#*************************************************************************#
#########   6. ORDERED LOGIT — FIT MODELS                        #########
#*************************************************************************#

polr_models <- dv_ord_vars |>
  set_names(dv_labels[dv_vars]) |>
  map(function(dv_ord) {
    model_vars <- c(dv_ord, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> select(all_of(model_vars)) |> drop_na()
    MASS::polr(as.formula(paste(dv_ord, "~", rhs)),
               data  = model_data,
               Hess  = TRUE)   # Hess=TRUE needed for SEs
  })


#*************************************************************************#
#########   7. ORDERED LOGIT — AVERAGE MARGINAL EFFECTS          #########
#*************************************************************************#

# Run tidy_polr_slopes for each model
# Returns a list of lists: each element has $top (highest category) and $full
ame_results <- map2(
  polr_models,
  names(polr_models),
  function(model, dv_label) {
    tidy_polr_slopes(model, dv_label, data = df)
  }
)

# AME on highest response category — main results table
coef_redis_top <- map_dfr(ame_results, ~ .x$top)

# Full level-by-level AME — saved to CSV for reference
coef_redis_full <- map_dfr(ame_results, function(res) {
  res$full |>
    transmute(
      dv, question, term = term_clean,
      outcome_level = as.character(group),
      estimate  = round(estimate, 3),
      conf.low  = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value   = round(p.value, 3),
      sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.10 ~ ".", TRUE ~ ""
      )
    )
})

write.csv(coef_redis_full,
          "graphs/regressions/AME_all_levels_redis.csv",
          row.names = FALSE)

coef_redis_top


#*************************************************************************#
#########   8. ORDERED LOGIT — MODEL FIT                         #########
#*************************************************************************#

fit_redis <- map2_dfr(polr_models, names(polr_models), function(model, dv_label) {
  tibble(
    dv        = dv_label,
    pseudo_r2 = round(1 - (model$deviance / model$null.deviance), 3),
    n         = length(model$residuals)
  )
})

fit_redis


#*************************************************************************#
#########   9. ORDERED LOGIT — REGRESSION TABLE                  #########
#*************************************************************************#
# modelsummary handles polr via tidy() — shows log-odds coefficients.
# The AME table (Section 7) is the main interpretive tool.

modelsummary(
  polr_models,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = term_labels,
  gof_map   = c("nobs", "logLik", "AIC"),
  output    = "graphs/regressions/regtable_redis_polr_logodds.txt",
  notes     = paste("Ordered logit (polr). Log-odds coefficients.",
                    "Interpret via AME table (Section 7).",
                    "* p<0.05, ** p<0.01, *** p<0.001")
)

# AME table via modelsummary — wraps avg_slopes output
modelsummary(
  map(polr_models, function(m) {
    model_vars <- all.vars(formula(m))
    model_data <- df |> select(all_of(model_vars)) |> drop_na()
    model_clean <- MASS::polr(formula(m), data = model_data, Hess = TRUE)
    avg_slopes(model_clean, vcov = "HC1", newdata = model_data,
               # Keep only the highest outcome level for the table
               variables = names(term_labels)[names(term_labels) %in% all.vars(formula(m))])
  }),
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  coef_map  = term_labels,
  gof_map   = c("nobs"),
  output    = "graphs/regressions/regtable_redis_polr_AME.txt",
  notes     = paste("Average marginal effects on P(highest category) from ordered logit.",
                    "HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001")
)


#*************************************************************************#
#########   10. COEFFICIENT PLOTS                                #########
#*************************************************************************#

# Main: AME on highest response category
plot_coefs(
  coef_redis_top,
  title_str  = "Redistribution attitudes — Ordered logit AME (highest category)",
  file_path  = "graphs/regressions/coef_redis_polr_top.png"
)

# Ideology x Quebec marginal effects (using OLS on 0-1 numeric for ggpredict)
walk2(dv_vars, dv_labels, function(dv, dv_label) {
  model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
  model_data <- df |> select(all_of(model_vars)) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", rhs)), data = model_data)

  me <- ggpredict(model, terms = c("ideo_right_num [all]", "quebec_bin [0,1]"))

  ggplot(me, aes(x = x, y = predicted,
                 colour = as.factor(group), fill = as.factor(group))) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
    scale_colour_manual(values = c("0"="#d6604d","1"="#2166ac"),
                        labels = c("0"="Rest of Canada","1"="Quebec")) +
    scale_fill_manual(  values = c("0"="#d6604d","1"="#2166ac"),
                        labels = c("0"="Rest of Canada","1"="Quebec")) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Ideology (Left to Right)", y = "Predicted value (0-1)",
         colour = NULL, fill = NULL,
         title  = paste("Ideology x Quebec --", dv_label),
         caption = "OLS predictions for visual clarity. Shaded = 95% CI.") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(paste0("graphs/regressions/me_ideo_", dv, ".png"),
         width = 8, height = 5, dpi = 300)
})


#*************************************************************************#
#########   11. R-SQUARED SUMMARY                                #########
#*************************************************************************#

plot_r2(fit_redis,
        title_str = "Model fit — Redistribution attitudes (ordered logit, McFadden pseudo-R2)",
        file_path = "graphs/regressions/r2_redis_polr.png")


#*************************************************************************#
#########   12. QUICK DIAGNOSTICS                                #########
#*************************************************************************#

cat("\n========== ORDERED LOGIT: model fit ==========\n")
print(fit_redis)

cat("\n========== Most consistently significant predictors ==========\n")
coef_redis_top |>
  filter(sig %in% c("*","**","***")) |>
  count(term, sort = TRUE) |>
  mutate(term = recode(term, !!!term_labels)) |>
  print(n = 20)

cat("\n========== Full AME by outcome level — see CSV ==========\n")
cat("Saved to: graphs/regressions/AME_all_levels_redis.csv\n")
cat("Bivariate contrasts saved to: graphs/regressions/bivar_contrasts_redis.csv\n")
