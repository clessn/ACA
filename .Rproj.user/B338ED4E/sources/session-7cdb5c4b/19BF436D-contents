library(tidyverse)

# Cleaning & wrangling ACA

# Version April 8th, 2025, 7:10 am
data <- read.csv("data/ACA_apr8_2025_07_10am.csv")

# Variable names
unique(list(data$ses_gender))

#cleaning
data <- data %>%
  mutate(gender = ifelse(ses_gender == "Female", 1,
                         ifelse(ses_gender == "Male", 0, NA)))

# Univariate statistics
hist(data$gender)
