# ==============================================================
# FAIRNESS ATTITUDES — FINAL SCRIPT (CPP-READY FIGURES)
# ==============================================================

# ==============================================================
# 0. SETUP
# ==============================================================

library(tidyverse)
library(ggplot2)
library(marginaleffects)
library(modelsummary)
library(MASS)
library(sandwich)
library(lmtest)
library(RColorBrewer)
library(kableExtra)
library(brant)

select <- dplyr::select
filter <- dplyr::filter

params <- list(
  dpi         = 300,
  plot_width  = 12,
  plot_height = 8,
  out_desc    = "graphs/descriptives_CPPFIG",
  out_reg     = "graphs/regressions_CPPFIG",
  data_path   = "data/clean_df_valid.csv"
)

dir.create(params$out_desc, recursive = TRUE, showWarnings = FALSE)
dir.create(params$out_reg,  recursive = TRUE, showWarnings = FALSE)

df <- read.csv(params$data_path)

# ==============================================================
# 1. VARIABLE CONSTRUCTION
# ==============================================================

df$univ_educ_bin <- as.integer(df$educ_group == "educUniv")
df$incomeHigh_bin <- as.integer(df$ses_income3Cat == "High")

df$educ_group <- factor(df$educ_group,
                        levels = c("educBHS","educHS","educUniv"))

df$ses_region_cat <- factor(
  dplyr::recode(df$ses_region_cat, "East Coast" = "Atlantic Canada"),
  levels = c("Ontario","Quebec","Alberta","Atlantic Canada")
)

df$ses_income3Cat <- factor(df$ses_income3Cat,
                            levels = c("Low","Mid","High"))

# ==============================================================
# 2. DV + LABELS
# ==============================================================

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

all_dv_vars <- c(prop_vars, recip_vars)

all_dv_labels <- c(
  redis_opportunity_num   = "Equal opportunity",
  redis_intelligence_num  = "Rewarded for effort & skill",
  redis_effort_num        = "Fairness of income distribution",
  redis_reasons_poor_num  = "Violating outcomes (Poor)",
  redis_reasons_rich_num  = "Violating outcomes (Rich)",
  redis_social_benefits_num = "Social benefits not a choice",
  redis_welfare_num         = "Welfare goes to undeserving",
  redis_no_cheat_system_num = "Trust not to cheat system"
)

dv_order <- unname(all_dv_labels)

# ==============================================================
# 3. TERM STRUCTURE (KEY FIX)
# ==============================================================

term_labels <- c(
  "incomeHigh_bin"="Income: High vs Low/Mid",
  "ses_male_bin"="Male",
  "ses_age"="Age",
  "univ_educ_bin"="Education: Univ vs Below",
  "employ_fulltime_bin"="Employed Full-Time",
  "ses_citizenYes_bin"="Citizen",
  "ideo_right_num"="Ideology (Right)",
  "ideo_interest_politics_num"="Political Interest",
  "ideo_define_QC_first_bin"="Quebecker First",
  "quebec_bin"="Quebec",
  "alberta_bin"="Alberta",
  "region_eastcoast_bin"="Atlantic Canada",
  "ses_french_bin"="French-speaking",
  "trust_social_bin"="Social Trust"
)

term_order <- c(
  "Income: High vs Low/Mid","Male","Age",
  "Education: Univ vs Below","Employed Full-Time","Citizen",
  "Ideology (Right)","Political Interest",
  "Quebecker First","Quebec","Alberta","Atlantic Canada",
  "French-speaking","Social Trust"
)

core_terms <- c(
  "Income: High vs Low/Mid",
  "Education: Univ vs Below",
  "Ideology (Right)",
  "Political Interest",
  "Social Trust"
)

identity_terms <- c(
  "Quebecker First","Quebec","Alberta",
  "Atlantic Canada","French-speaking"
)

# ==============================================================
# 4. MODEL FORMULA
# ==============================================================

rhs <- "incomeHigh_bin + ses_male_bin + ses_age + univ_educ_bin +
        employ_fulltime_bin + ses_citizenYes_bin +
        ideo_right_num + ideo_interest_politics_num +
        ideo_define_QC_first_bin + quebec_bin + alberta_bin + region_eastcoast_bin +
        ses_french_bin + trust_social_bin"

# ==============================================================
# 5. HELPERS
# ==============================================================

robust_vcov <- function(model) vcovHC(model, type = "HC1")

extract_ame <- function(dv, label){
  m <- lm(as.formula(paste(dv,"~",rhs)), data=df)
  avg_slopes(m, vcov=robust_vcov(m)) |>
    as_tibble() |>
    mutate(
      dv=label,
      term=recode(term, !!!term_labels),
      term=factor(term, levels=term_order)
    )
}

# ==============================================================
# 6. ESTIMATION
# ==============================================================

coef_df <- map2_dfr(all_dv_vars, all_dv_labels[all_dv_vars], extract_ame)

# ==============================================================
# 7. CLEAN COEF PLOT FUNCTION (FINAL)
# ==============================================================

plot_clean <- function(data, keep_terms=NULL, file){
  
  d <- data
  
  if(!is.null(keep_terms)){
    d <- d |> filter(term %in% keep_terms)
  }
  
  d <- d |>
    mutate(
      nonsig = conf.low <= 0 & conf.high >= 0,
      dv = factor(dv, levels=dv_order)
    )
  
  p <- ggplot(d, aes(estimate, term)) +
    geom_vline(xintercept=0, linetype="dashed", color="grey60") +
    geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, alpha=nonsig),
                   height=0, size=0.5) +
    geom_point(aes(alpha=nonsig), size=2) +
    scale_alpha_manual(values=c(`TRUE`=0.4, `FALSE`=1), guide="none") +
    coord_cartesian(xlim=c(-0.25,0.25)) +
    facet_wrap(~dv, ncol=2) +
    theme_minimal() +
    theme(
      strip.text=element_text(face="bold"),
      panel.grid.major.y=element_blank()
    )
  
  ggsave(file, p, width=12, height=8, dpi=300)
}

# ==============================================================
# 8. FINAL FIGURES (CPP STRUCTURE)
# ==============================================================

# Figure 1 — core predictors
plot_clean(
  coef_df |> filter(dv %in% unname(prop_vars)),
  core_terms,
  file.path(params$out_reg,"FIG1_core.png")
)

# Figure 2 — identity
plot_clean(
  coef_df |> filter(dv %in% unname(prop_vars)),
  identity_terms,
  file.path(params$out_reg,"FIG2_identity.png")
)

# ==============================================================
# 9. MEAN + CI (BEST FIGURE)
# ==============================================================

mean_df <- map_dfr(all_dv_vars, function(v){
  df |>
    summarise(
      mean=mean(.data[[v]], na.rm=TRUE),
      se=sd(.data[[v]], na.rm=TRUE)/sqrt(n())
    ) |>
    mutate(
      dv=all_dv_labels[[v]],
      lo=mean-1.96*se,
      hi=mean+1.96*se
    )
})

mean_df |>
  mutate(dv=reorder(dv, mean)) |>
  ggplot(aes(mean, dv)) +
  geom_point(size=3) +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=0.2) +
  theme_minimal() +
  labs(x="Mean (0–1 scale)", y=NULL)

ggsave(file.path(params$out_desc,"FIG_mean.png"),
       width=10,height=6,dpi=300)

# ==============================================================
# DONE
# ==============================================================

cat("All figures generated.\n")