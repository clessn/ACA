
# ============================================================
# Budgetary Preferences - Descriptives + Regressions
# Four-region Canadian survey: Alberta, Ontario, Quebec, Eastern Canada
#
# DV strategy:
#   budget_imp_X_bin   -> LPM (OLS) + Logit, binary outcome
#   budget_prio_X      -> OLS, continuous 0-100 raw score
#   budget_prio_X_pref / _intense -> descriptive bar charts only
#
# All models use HC1 robust standard errors.
# ============================================================

# 0.1. Packages
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(marginaleffects)
library(modelsummary)
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

# Ordered factors
df$educ_group     <- factor(df$educ_group,     levels = c("educBHS", "educHS", "educUniv"))
df$ses_region_cat <- factor(df$ses_region_cat, levels = c("Ontario", "Quebec", "Alberta", "East Coast"))
df$ses_income3Cat <- factor(df$ses_income3Cat, levels = c("Low", "Mid", "High"))


#*************************************************************************#
#########              2. DEFINE IVs AND MODEL RHS               #########
#*************************************************************************#

ivs <- list(
  list(type="binary",  var="quebec_bin",               low=0, high=1, label="Quebec (No=0 vs Yes=1)"),
  list(type="binary",  var="ideo_define_QC_first_bin",  low=0, high=1, label="Quebecker First (No=0 vs Yes=1)"),
  list(type="binary",  var="alberta_bin",              low=0, high=1, label="Alberta (No=0 vs Yes=1)"),
  list(type="binary",  var="ontario_bin",              low=0, high=1, label="Ontario (No=0 vs Yes=1)"),
  list(type="binary",  var="region_eastcoast_bin",     low=0, high=1, label="Eastern Canada (No=0 vs Yes=1)"),
  list(type="binary",  var="incomeHigh_bin",           low=0, high=1, label="High Income (No=0 vs Yes=1)"),
  list(type="binary",  var="ses_male_bin",             low=0, high=1, label="Gender (Female=0 vs Male=1)"),
  list(type="binary",  var="age18_34_bin",             low=0, high=1, label="Age 18-34 (No=0 vs Yes=1)"),
  list(type="binary",  var="age55plus_bin",            low=0, high=1, label="Age 55+ (No=0 vs Yes=1)"),
  list(type="binary",  var="univ_educ_bin",            low=0, high=1, label="University Education (No=0 vs Yes=1)"),
  list(type="binary",  var="ses_citizenYes_bin",       low=0, high=1, label="Citizen (No=0 vs Yes=1)"),
  list(type="binary",  var="ses_french_bin",           low=0, high=1, label="French-speaking (No=0 vs Yes=1)"),
  list(type="binary",  var="trust_social_bin",         low=0, high=1, label="Social Trust (Low=0 vs High=1)"),
  list(type="binary",  var="employ_fulltime_bin",      low=0, high=1, label="Employed Full-Time (No=0 vs Yes=1)"),
  list(type="numeric", var="ideo_right_num",           low=0, high=1, label="Ideology: Left (0) vs Right (1)"),
  list(type="binary",  var="vote_PLC_bin",             low=0, high=1, label="Liberal voter (No=0 vs Yes=1)"),
  list(type="binary",  var="vote_PCC_bin",             low=0, high=1, label="Conservative voter (No=0 vs Yes=1)")
)

# RHS formula -- Ontario is the omitted region reference category
rhs <- "quebec_bin + ideo_define_QC_first_bin + alberta_bin + region_eastcoast_bin +
        incomeHigh_bin + ses_male_bin + age18_34_bin + age55plus_bin +
        univ_educ_bin + ses_citizenYes_bin + ses_french_bin +
        trust_social_bin + employ_fulltime_bin +
        ideo_right_num + vote_PLC_bin + vote_PCC_bin"

term_labels <- c(
  "quebec_bin"               = "Quebec",
  "ideo_define_QC_first_bin" = "Quebecker First",
  "alberta_bin"              = "Alberta",
  "region_eastcoast_bin"     = "Eastern Canada",
  "incomeHigh_bin"           = "High Income",
  "ses_male_bin"             = "Male",
  "age18_34_bin"             = "Age 18-34",
  "age55plus_bin"            = "Age 55+",
  "univ_educ_bin"            = "University Education",
  "ses_citizenYes_bin"       = "Citizen",
  "ses_french_bin"           = "French-speaking",
  "trust_social_bin"         = "Social Trust",
  "employ_fulltime_bin"      = "Employed Full-Time",
  "ideo_right_num"           = "Ideology (Right)",
  "vote_PLC_bin"             = "Liberal voter",
  "vote_PCC_bin"             = "Conservative voter"
)

policy_labels <- c(
  "health"="Healthcare", "edu"="Education", "pensions"="Pensions",
  "taxes"="Tax reduction", "debt"="Debt reduction", "seniors"="Seniors",
  "cc"="Cost of living", "ecn"="Economy", "clim"="Climate"
)


#*************************************************************************#
#########        3. HELPER FUNCTIONS                              #########
#*************************************************************************#

robust_vcov <- function(model) vcovHC(model, type = "HC1")

tidy_avg_slopes <- function(model, dv_label, question = NULL, data = df) {
  fml         <- formula(model)
  model_vars  <- all.vars(fml)
  model_data  <- data |> select(all_of(model_vars)) |> drop_na()
  # Refit directly (update() loses .x pronoun once map2() moves on).
  # Pass model_data as newdata so marginaleffects only sees model columns,
  # suppressing the "N columns" performance warning.
  model_clean <- lm(fml, data = model_data)
  vm          <- robust_vcov(model_clean)
  avg_slopes(model_clean, vcov = vm, newdata = model_data) |>
    as_tibble() |>
    transmute(
      question, dv = dv_label, term,
      estimate  = round(estimate, 3), conf.low = round(conf.low, 3),
      conf.high = round(conf.high, 3), p.value  = round(p.value, 3),
      sig = case_when(p.value<0.001~"***",p.value<0.01~"**",p.value<0.05~"*",p.value<0.10~".",TRUE~""),
      direction = case_when(conf.low>0~"Positive",conf.high<0~"Negative",TRUE~"No clear effect")
    )
}

interpret_contrast <- function(dv, dv_label, iv, data) {
  iv_var <- iv$var; iv_low <- as.character(iv$low); iv_high <- as.character(iv$high)
  model_data <- data |> select(all_of(c(dv, iv_var))) |> drop_na()
  model      <- lm(as.formula(paste(dv, "~", iv_var)), data = model_data)
  contrast_spec <- list(c(iv$low, iv$high)); names(contrast_spec) <- iv_var
  tryCatch({
    avg_comparisons(model, variables = contrast_spec, vcov = robust_vcov(model)) |>
      as_tibble() |>
      transmute(
        iv = iv$label, dv = dv_label, contrast = paste(iv_low, "vs", iv_high),
        estimate=round(estimate,3), conf.low=round(conf.low,3),
        conf.high=round(conf.high,3), p.value=round(p.value,3),
        sig = case_when(p.value<0.001~"***",p.value<0.01~"**",p.value<0.05~"*",p.value<0.10~".",TRUE~""),
        direction = case_when(conf.low>0~paste(iv_high,">",iv_low),conf.high<0~paste(iv_low,">",iv_high),TRUE~"No clear difference")
      )
  }, error = function(e) {
    cat("FAILED:", iv_var, "~", dv, "\n  Error:", conditionMessage(e), "\n")
    tibble(iv=iv$label,dv=dv_label,contrast=paste(iv_low,"vs",iv_high),
           estimate=NA_real_,conf.low=NA_real_,conf.high=NA_real_,p.value=NA_real_,sig=NA_character_,direction="ERROR")
  })
}

plot_coefs <- function(coef_df, title_str, file_path) {
  coef_df |>
    mutate(term = recode(term, !!!term_labels)) |>
    ggplot(aes(x=estimate, y=reorder(term,estimate), color=direction)) +
    geom_vline(xintercept=0, linetype="dashed", color="grey50") +
    geom_point(size=2.5) +
    geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), height=0.2) +
    scale_color_manual(values=c("Positive"="#2166ac","Negative"="#d6604d","No clear effect"="grey60")) +
    facet_wrap(~dv, scales="free_x") +
    labs(x="Average marginal effect (HC1 robust SEs)", y=NULL, color=NULL, title=title_str) +
    theme_minimal(base_size=12) +
    theme(legend.position="bottom", strip.text=element_text(face="bold"))
  ggsave(file_path, width=13, height=8, dpi=300)
}

plot_r2 <- function(fit_df, r2_col="adj_r_sq", title_str, file_path) {
  fit_df |>
    mutate(
      fit_level = case_when(
        .data[[r2_col]] >= 0.07 ~ "High (>=0.07)",
        .data[[r2_col]] >= 0.03 ~ "Moderate (0.03-0.07)",
        TRUE ~ "Low (<0.03)"
      ),
      fit_level = factor(fit_level, levels=c("High (>=0.07)","Moderate (0.03-0.07)","Low (<0.03)")),
      dv = fct_reorder(dv, .data[[r2_col]])
    ) |>
    ggplot(aes(x=.data[[r2_col]], y=dv, fill=fit_level)) +
    geom_col() +
    geom_text(aes(label=sprintf("%.3f",.data[[r2_col]])), hjust=-0.15, size=3.2) +
    scale_fill_manual(values=c("High (>=0.07)"="#2166ac","Moderate (0.03-0.07)"="#92c5de","Low (<0.03)"="#d6604d")) +
    scale_x_continuous(limits=c(0,0.20), labels=scales::label_number(accuracy=0.01)) +
    labs(x=ifelse(r2_col=="pseudo_r2","McFadden pseudo-R2","Adjusted R2"),
         y=NULL, fill="Model fit", title=title_str) +
    theme_minimal(base_size=13) +
    theme(legend.position="bottom", panel.grid.major.y=element_blank())
  ggsave(file_path, width=8, height=5, dpi=300)
}


#*************************************************************************#
#########   4. DETECT DVs BY BATTERY                             #########
#*************************************************************************#

imp_bin_vars <- df |> select(matches("^budget_imp_.*_bin$")) |> names()
imp_policies <- str_extract(imp_bin_vars, "(?<=budget_imp_).*(?=_bin)")
imp_bin_df   <- tibble(var=imp_bin_vars, policy=imp_policies,
                       label=recode(imp_policies, !!!policy_labels, .default=imp_policies))

# budget_prio_X only -- end-anchor regex excludes _pref and _intense
prio_num_vars <- df |> select(matches("^budget_prio_[a-z]+$")) |> names()
prio_policies <- str_remove(prio_num_vars, "budget_prio_")
prio_num_df   <- tibble(var=prio_num_vars, policy=prio_policies,
                        label=recode(prio_policies, !!!policy_labels, .default=prio_policies))

prio_pref_vars    <- df |> select(matches("^budget_prio_.*_pref$"))    |> names()
prio_intense_vars <- df |> select(matches("^budget_prio_.*_intense$")) |> names()


#*************************************************************************#
#########   5. DESCRIPTIVE STATISTICS -- BAR CHARTS              #########
#*************************************************************************#

# 5.1. Importance binary: % reaching threshold
imp_bin_df |>
  mutate(pct=map_dbl(var,~mean(df[[.x]],na.rm=TRUE)*100), label=fct_reorder(label,pct)) |>
  ggplot(aes(x=pct, y=label)) +
  geom_col(fill="#2166ac", width=0.6) +
  geom_text(aes(label=sprintf("%.1f%%",pct)), hjust=-0.15, size=3.5) +
  scale_x_continuous(limits=c(0,100), labels=scales::label_number(suffix="%")) +
  labs(x="% of respondents above importance threshold (>50%)", y=NULL,
       title="Budget importance: share rating each area as a top priority",
       caption="Binary coding: 1 = allocated >50% of importance points to this area.") +
  theme_minimal(base_size=13) + theme(panel.grid.major.y=element_blank())
ggsave("graphs/descriptives/desc_imp_bin.png", width=9, height=6, dpi=300)

# 5.2. Priority raw score: mean allocation with SD error bars
prio_num_df |>
  mutate(mean_alloc=map_dbl(var,~mean(df[[.x]],na.rm=TRUE)),
         sd_alloc=map_dbl(var,~sd(df[[.x]],na.rm=TRUE)),
         label=fct_reorder(label,mean_alloc)) |>
  ggplot(aes(x=mean_alloc, y=label)) +
  geom_col(fill="#4dac26", width=0.6) +
  geom_errorbarh(aes(xmin=pmax(mean_alloc-sd_alloc,0), xmax=mean_alloc+sd_alloc),
                 height=0.25, color="grey30") +
  geom_text(aes(label=sprintf("%.1f",mean_alloc)), hjust=-0.2, size=3.5) +
  scale_x_continuous(limits=c(0,80), labels=scales::label_number(suffix=" pts")) +
  labs(x="Mean budget points allocated (out of 100)", y=NULL,
       title="Budget priority: mean allocation across spending areas",
       caption="Error bars = +/-1 SD. Points sum to 100 per respondent.") +
  theme_minimal(base_size=13) + theme(panel.grid.major.y=element_blank())
ggsave("graphs/descriptives/desc_prio_mean.png", width=9, height=6, dpi=300)

# 5.3. Priority first choice (_pref)
tibble(var=prio_pref_vars) |>
  mutate(policy=str_extract(var,"(?<=budget_prio_).*(?=_pref)"),
         label=recode(policy,!!!policy_labels,.default=policy),
         pct=map_dbl(var,~mean(df[[.x]],na.rm=TRUE)*100),
         label=fct_reorder(label,pct)) |>
  ggplot(aes(x=pct, y=label)) +
  geom_col(fill="#d6604d", width=0.6) +
  geom_text(aes(label=sprintf("%.1f%%",pct)), hjust=-0.15, size=3.5) +
  scale_x_continuous(limits=c(0,60), labels=scales::label_number(suffix="%")) +
  labs(x="% of respondents choosing this as first priority", y=NULL,
       title="Budget priority: share selecting each area as first choice",
       caption="Binary coding: 1 = respondent allocated most points to this area.") +
  theme_minimal(base_size=13) + theme(panel.grid.major.y=element_blank())
ggsave("graphs/descriptives/desc_prio_pref.png", width=9, height=6, dpi=300)

# 5.4. Intense allocators (_intense)
tibble(var=prio_intense_vars) |>
  mutate(policy=str_extract(var,"(?<=budget_prio_).*(?=_intense)"),
         label=recode(policy,!!!policy_labels,.default=policy),
         pct=map_dbl(var,~mean(df[[.x]],na.rm=TRUE)*100),
         label=fct_reorder(label,pct)) |>
  ggplot(aes(x=pct, y=label)) +
  geom_col(fill="#762a83", width=0.6) +
  geom_text(aes(label=sprintf("%.1f%%",pct)), hjust=-0.15, size=3.5) +
  scale_x_continuous(limits=c(0,40), labels=scales::label_number(suffix="%")) +
  labs(x="% of respondents allocating >50 points (intense preference)", y=NULL,
       title="Budget priority: share with intense preference per area",
       caption="Binary coding: 1 = respondent allocated >50 of 100 points to this area.") +
  theme_minimal(base_size=13) + theme(panel.grid.major.y=element_blank())
ggsave("graphs/descriptives/desc_prio_intense.png", width=9, height=6, dpi=300)

# 5.5. Combined overview -- all three binary indicators
bind_rows(
  imp_bin_df |> mutate(pct=map_dbl(var,~mean(df[[.x]],na.rm=TRUE)*100),
                       metric="Importance threshold (>50%)"),
  tibble(var=prio_pref_vars) |>
    mutate(policy=str_extract(var,"(?<=budget_prio_).*(?=_pref)"),
           label=recode(policy,!!!policy_labels,.default=policy),
           pct=map_dbl(var,~mean(df[[.x]],na.rm=TRUE)*100),
           metric="Priority: first choice"),
  tibble(var=prio_intense_vars) |>
    mutate(policy=str_extract(var,"(?<=budget_prio_).*(?=_intense)"),
           label=recode(policy,!!!policy_labels,.default=policy),
           pct=map_dbl(var,~mean(df[[.x]],na.rm=TRUE)*100),
           metric="Priority: intense (>50 pts)")
) |>
  filter(!is.na(label)) |>
  ggplot(aes(x=pct, y=reorder(label,pct), fill=metric)) +
  geom_col(position="dodge", width=0.7) +
  scale_fill_manual(values=c("Importance threshold (>50%)"="#2166ac",
                             "Priority: first choice"="#d6604d",
                             "Priority: intense (>50 pts)"="#762a83")) +
  scale_x_continuous(labels=scales::label_number(suffix="%")) +
  labs(x="% of respondents", y=NULL, fill=NULL,
       title="Budgetary preferences -- overview of all binary indicators") +
  theme_minimal(base_size=13) +
  theme(legend.position="bottom", panel.grid.major.y=element_blank())
ggsave("graphs/descriptives/desc_overview_combined.png", width=11, height=7, dpi=300)


#*************************************************************************#
#########   6. BIVARIATE MARGINAL MEANS (ALL IVs x ALL DVs)     #########
#*************************************************************************#

all_reg_df <- bind_rows(
  imp_bin_df  |> mutate(battery="imp_bin"),
  prio_num_df |> mutate(battery="prio_num")
)

marginal_results <- map_dfr(ivs, function(iv) {
  map2_dfr(all_reg_df$var, all_reg_df$label, interpret_contrast, iv=iv, data=df)
})

write.csv(marginal_results, "graphs/regressions/marginal_means_all.csv", row.names=FALSE)


#*************************************************************************#
#########   7. REGRESSIONS -- IMPORTANCE BINARY (LPM + LOGIT)   #########
#*************************************************************************#

# 7.1. Fit models
lpm_imp <- imp_bin_df$var |> set_names(imp_bin_df$label) |>
  map(~lm(as.formula(paste(.x,"~",rhs)), data=df))

logit_imp <- imp_bin_df$var |> set_names(imp_bin_df$label) |>
  map(~glm(as.formula(paste(.x,"~",rhs)), data=df, family=binomial(link="logit")))

# 7.2. Average marginal effects (robust SEs)
coef_lpm_imp <- map2_dfr(lpm_imp, imp_bin_df$label, tidy_avg_slopes, question="imp")

coef_logit_imp <- map2_dfr(logit_imp, imp_bin_df$label, function(model, dv_label) {
  fml        <- formula(model)
  model_vars <- all.vars(fml)
  model_data <- df |> select(all_of(model_vars)) |> drop_na()
  vm         <- robust_vcov(model)
  avg_slopes(model, vcov=vm, newdata=model_data) |> as_tibble() |>
    transmute(question="imp", dv=dv_label, term,
              estimate=round(estimate,3), conf.low=round(conf.low,3),
              conf.high=round(conf.high,3), p.value=round(p.value,3),
              sig=case_when(p.value<0.001~"***",p.value<0.01~"**",p.value<0.05~"*",p.value<0.10~".",TRUE~""),
              direction=case_when(conf.low>0~"Positive",conf.high<0~"Negative",TRUE~"No clear effect"))
})

# 7.3. Model fit
fit_lpm_imp <- map2_dfr(lpm_imp, imp_bin_df$label, function(model, dv_label) {
  s <- summary(model)
  tibble(dv=dv_label, model="LPM", r_squared=round(s$r.squared,3),
         adj_r_sq=round(s$adj.r.squared,3), n=length(model$residuals))
})

fit_logit_imp <- map2_dfr(logit_imp, imp_bin_df$label, function(model, dv_label) {
  tibble(dv=dv_label, model="Logit",
         pseudo_r2=round(1-(model$deviance/model$null.deviance),3), n=length(model$residuals))
})

# 7.4. Regression tables
modelsummary(lpm_imp, estimate="{estimate}{stars}", statistic="({std.error})",
             vcov=map(lpm_imp, robust_vcov), coef_map=term_labels,
             gof_map=c("nobs","r.squared","adj.r.squared"),
             output="graphs/regressions/regtable_imp_LPM.txt",
             notes="LPM (OLS). HC1 robust SEs in parentheses. * p<0.05, ** p<0.01, *** p<0.001")

modelsummary(logit_imp, estimate="{estimate}{stars}", statistic="({std.error})",
             vcov=map(logit_imp, robust_vcov), coef_map=term_labels,
             gof_map=c("nobs","logLik","AIC"),
             output="graphs/regressions/regtable_imp_logit_coefs.txt",
             notes="Logit coefficients (log-odds). HC1 robust SEs. Interpret via AME table.")

modelsummary(map(logit_imp,~avg_slopes(.x,vcov=robust_vcov(.x))),
             estimate="{estimate}{stars}", statistic="({std.error})",
             coef_map=term_labels, gof_map=c("nobs"),
             output="graphs/regressions/regtable_imp_logit_AME.txt",
             notes="Average marginal effects from logit. HC1 robust SEs. * p<0.05, ** p<0.01, *** p<0.001")

# 7.5. Coefficient plots
plot_coefs(coef_lpm_imp,   "Importance (binary) -- LPM coefficients",              "graphs/regressions/coef_imp_LPM.png")
plot_coefs(coef_logit_imp, "Importance (binary) -- Logit average marginal effects", "graphs/regressions/coef_imp_logit_AME.png")

# 7.6. LPM vs Logit AME robustness comparison
bind_rows(coef_lpm_imp|>mutate(model="LPM"), coef_logit_imp|>mutate(model="Logit AME")) |>
  mutate(term=recode(term,!!!term_labels)) |>
  ggplot(aes(x=estimate,y=reorder(term,estimate),color=model,shape=model)) +
  geom_vline(xintercept=0,linetype="dashed",color="grey50") +
  geom_point(size=2.2, position=position_dodge(width=0.5)) +
  geom_errorbarh(aes(xmin=conf.low,xmax=conf.high),height=0.2,position=position_dodge(width=0.5)) +
  scale_color_manual(values=c("LPM"="#2166ac","Logit AME"="#d6604d")) +
  facet_wrap(~dv) +
  labs(x="Estimated effect",y=NULL,color=NULL,shape=NULL,
       title="Importance battery -- LPM vs. Logit AME (robustness check)") +
  theme_minimal(base_size=11) +
  theme(legend.position="bottom",strip.text=element_text(face="bold"))
ggsave("graphs/regressions/coef_imp_LPM_vs_logit.png", width=14, height=9, dpi=300)

# 7.7. R-squared summaries
plot_r2(fit_lpm_imp,   r2_col="adj_r_sq",  title_str="Model fit -- Importance battery (LPM)",            file_path="graphs/regressions/r2_imp_LPM.png")
plot_r2(fit_logit_imp, r2_col="pseudo_r2", title_str="Model fit -- Importance battery (Logit pseudo-R2)", file_path="graphs/regressions/r2_imp_logit.png")


#*************************************************************************#
#########   8. REGRESSIONS -- PRIORITY RAW SCORE (OLS)          #########
#*************************************************************************#
# Note: budget_prio_X sums to 100 per respondent (compositional data).
# OLS is appropriate for exploration; consider Dirichlet regression for publication.

# 8.1. Fit models
lm_prio <- prio_num_df$var |> set_names(prio_num_df$label) |>
  map(~lm(as.formula(paste(.x,"~",rhs)), data=df))

# 8.2. Average marginal effects (robust SEs)
coef_prio <- map2_dfr(lm_prio, prio_num_df$label, tidy_avg_slopes, question="prio")

# 8.3. Model fit
fit_prio <- map2_dfr(lm_prio, prio_num_df$label, function(model, dv_label) {
  s <- summary(model)
  tibble(dv=dv_label, r_squared=round(s$r.squared,3),
         adj_r_sq=round(s$adj.r.squared,3), n=length(model$residuals))
})

# 8.4. Regression table
modelsummary(lm_prio, estimate="{estimate}{stars}", statistic="({std.error})",
             vcov=map(lm_prio, robust_vcov), coef_map=term_labels,
             gof_map=c("nobs","r.squared","adj.r.squared"),
             output="graphs/regressions/regtable_prio_OLS.txt",
             notes="OLS. HC1 robust SEs. DV = budget points allocated (0-100). * p<0.05, ** p<0.01, *** p<0.001")

# 8.5. Plots
plot_coefs(coef_prio, "Budget priority allocation (0-100 pts) -- OLS avg marginal effects", "graphs/regressions/coef_prio_OLS.png")
plot_r2(fit_prio, r2_col="adj_r_sq", title_str="Model fit -- Priority allocation (OLS)", file_path="graphs/regressions/r2_prio_OLS.png")


#*************************************************************************#
#########   9. MARGINAL EFFECTS PLOTS -- IDEOLOGY x QUEBEC       #########
#*************************************************************************#

walk2(
  c(imp_bin_df$var, prio_num_df$var),
  c(imp_bin_df$label, prio_num_df$label),
  function(dv, dv_label) {
    model_vars <- c(dv, all.vars(as.formula(paste("~", rhs))))
    model_data <- df |> select(all_of(model_vars)) |> drop_na()
    model      <- lm(as.formula(paste(dv,"~",rhs)), data=model_data)
    me         <- ggpredict(model, terms=c("ideo_right_num [all]","quebec_bin [0,1]"))
    
    ggplot(me, aes(x=x,y=predicted,colour=as.factor(group),fill=as.factor(group))) +
      geom_line(linewidth=1) +
      geom_ribbon(aes(ymin=conf.low,ymax=conf.high), alpha=0.15, colour=NA) +
      scale_colour_manual(values=c("0"="#d6604d","1"="#2166ac"),
                          labels=c("0"="Rest of Canada","1"="Quebec")) +
      scale_fill_manual(  values=c("0"="#d6604d","1"="#2166ac"),
                          labels=c("0"="Rest of Canada","1"="Quebec")) +
      labs(x="Ideology (Left to Right)", y="Predicted value",
           colour=NULL, fill=NULL,
           title=paste("Ideology x Quebec --", dv_label),
           caption="OLS predictions. HC1 robust SEs. Shaded area = 95% CI.") +
      theme_minimal(base_size=12) + theme(legend.position="bottom")
    
    ggsave(paste0("graphs/regressions/me_ideo_",dv,".png"), width=8, height=5, dpi=300)
  }
)


#*************************************************************************#
#########   10. QUICK DIAGNOSTICS                                #########
#*************************************************************************#

cat("\n========== IMPORTANCE: LPM fit ==========\n");         print(fit_lpm_imp)
cat("\n========== IMPORTANCE: Logit pseudo-R2 ==========\n"); print(fit_logit_imp)
cat("\n========== PRIORITY: OLS fit ==========\n");           print(fit_prio)

cat("\n========== Most significant predictors -- imp LPM ==========\n")
coef_lpm_imp |> filter(sig %in% c("*","**","***")) |>
  count(term, sort=TRUE) |> mutate(term=recode(term,!!!term_labels)) |> print(n=20)

cat("\n========== Most significant predictors -- prio OLS ==========\n")
coef_prio |> filter(sig %in% c("*","**","***")) |>
  count(term, sort=TRUE) |> mutate(term=recode(term,!!!term_labels)) |> print(n=20)

cat("\n========== LPM vs Logit direction agreement check ==========\n")
# Any row here = predictor where LPM and Logit disagree on direction
# among statistically significant results. Investigate if any appear.
bind_rows(coef_lpm_imp|>mutate(model="LPM"), coef_logit_imp|>mutate(model="Logit")) |>
  filter(sig %in% c("*","**","***")) |>
  select(dv, term, model, direction) |>
  pivot_wider(names_from=model, values_from=direction) |>
  filter(LPM != Logit) |>
  print()
