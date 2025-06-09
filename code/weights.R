# ===================================================
#  Weights for ACA Survey (April 2025)
# ===================================================
# Version: June 9th, 2025

# -----------------------
# 1. Load packages
# -----------------------
library(tidyverse)
library(tidyr)
library(dplyr)
library(anesrake)
library(questionr)
library(ggplot2)
#library(MASS) # use MAA:polr() to avoid errors with tidyverse


# -----------------------
# 2. Load data
# -----------------------
df <- read.csv("data/clean_df_full.csv") 

# -----------------------
# 3. Descriptive statistics & weights
# -----------------------
# Age, gender, employment, income, region, education, political interest and ideology
# names(df)
# prop.table(table(df$ideo_interest_politics_num))
# hist(df$ideo_interest_politics_num)

# Highly biased, anesrake is applied for age, gender, employment, income, 
# education using 2021 census data (categories not always the same, proportions done by myself)

# Step 1: Clean and rename
df$gender <- NA_integer_
df$gender[df$ses_male_bin == 1] <- 2          # man
df$gender[df$ses_male_bin == 0] <- 1       # women & other

# Convert to labeled factor
df$gender <- factor(ifelse(df$ses_male_bin == 1, "Man", "Woman/Other"),
                    levels = c("Woman/Other", "Man"))

table(df$gender)

df$age <- NA_integer_
df$age[df$age34 == 1] <- 1          # 18-34
df$age[df$age35_54 == 1] <- 2       # 35-54
df$age[is.na(df$age) | (df$age34 == 0 & df$age35_54 == 0)] <- 3  # 55+ group

# Convert to labeled factor
df$age <- factor(
  ifelse(df$age34 == 1, "18–34",
         ifelse(df$age35_54 == 1, "35–54", "55+")),
  levels = c("18–34", "35–54", "55+")
)
table(df$age)

df$education <- NA_integer_
df$education[df$educ_group %in% c("educBHS", "educHS")] <- 1
df$education[df$educ_group == "educUniv"] <- 2     

# Convert to labeled factor
df$education <- factor(
  ifelse(df$educ_group %in% c("educBHS", "educHS"), "Low", "University"),
  levels = c("Low", "University")
)
table(df$education)

df$income <- NA_integer_
df$income[df$ses_income3Cat %in% c("Low", "Mid")]  <- 1
df$income[df$ses_income3Cat == "High"] <- 2

# Convert to labeled factor
df$income <- factor(
  ifelse(df$ses_income3Cat == "High", "High", "Low/Mid"),
  levels = c("Low/Mid", "High")
)
table(df$income) 

# Census info & target list
targets <- list(
  gender = c("Woman/Other" = 0.51, "Man" = 0.49),
  age = c("18–34" = 0.24, "35–54" = 0.33, "55+" = 0.43),
  education = c("Low" = 0.71, "University" = 0.29),
  income = c("Low/Mid" = 0.835, "High" = 0.165)
)

str(df[, c("gender", "age", "education", "income")])


# id variable
df$caseid <- 1:length(df$gender)

anesrakefinder(targets, df, choosemethod = "total")

outsave <- anesrake(targets, df, caseid = df$caseid,
                    verbose= FALSE, cap = 5, choosemethod = "total",
                    type = "pctlim", pctlim = .05 , nlim = 5,
                    iterate = TRUE , force1 = TRUE)

summary(outsave)

# add weights to the dataset
df$weightvec <- outsave$weightvec  # safer and clearer

# calculate weighting loss (design effect)
n <- length(df$income)

# Kish's Effective Sample Size formula
weighting_loss <- ((sum(df$weightvec^2) / (sum(df$weightvec))^2) * n) - 1

# Checking
names(df)

unweighted <-  wpct(df$tradeoff_childcare_benefits_bin)
weighted  <-  wpct(df$tradeoff_childcare_benefits_bin, df$weightvec)
tab  <- data.frame(unweighted, weighted)

summary(df$weightvec)
class(df$weightvec)

sum(df$weightvec)   # should be ~ equal to n, if not normalized
mean(df$weightvec)

# Save
names(df)
write.csv(df, "data/ACA_weighted.csv", row.names = FALSE)
