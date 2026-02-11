# ===================================================
# Wrangling ACA Winter 2026 and Saving as New CSV
# ===================================================
# Version: Feb 11, 2026 

# -----------------------
# Load packages
# -----------------------
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(rlang)

read_rds("data/codebook_clean.rds")
# -----------------------
# Preliminary cleaning by Shannon to collapse prov var
# -----------------------

 clean <- read.csv("data/aca_ethics_W26.csv")
range(clean$budget_tradeoff_cc1_17)
ggplot(clean, aes(x = budget_tradeoff_cc1_17)) + 
  geom_histogram()
# Collapse language, ideo_vote/define and trust_ variables (varied according to province/ter)
# Collapse language
clean <- clean %>%
  mutate(
    ses_language_clean = case_when(
      ses_language == 98 & !is.na(ses_language_98_TEXT) ~ ses_language_98_TEXT,
      ses_language == 98 & is.na(ses_language_98_TEXT) ~ "Other",
      TRUE ~ as.character(ses_language)
    )
  )


# Check: not useful in this sample
list(unique(clean$ses_language_clean))

clean <- clean %>%
  filter(!is.na(ses_language_clean)) %>%
  filter(ses_language_clean %in% c("French", "English", "Other"))

# Verify the result
head(clean$ses_language_clean)

# Remove prior ses_language var
clean <- clean %>%
  select(-ses_language, -ses_language_98_TEXT)

# Collapse all ideo_vote/define questions (with how to check)
# Step 1: Identify and inspect one variable
unique(clean$ideo_vote_fed_QC) # Check whether this has the expected values like "5", "1", "98", ""

# Step 2: Identify all vote/define columns
fed_vars    <- names(clean)[grepl("^ideo_vote_fed_", names(clean))]
prov_vars   <- names(clean)[grepl("^ideo_vote_prov_", names(clean))]
define_vars <- names(clean)[grepl("^ideo_define_", names(clean))]

# Step 3: Collapse vote/define into _clean variables
# Collapse fed vote
ideo_vote_fed_clean <- reduce(
  map(fed_vars, ~ na_if(as.character(clean[[.x]]), "")),
  coalesce
)

# Collapse prov vote
ideo_vote_prov_clean <- reduce(
  map(prov_vars, ~ na_if(as.character(clean[[.x]]), "")),
  coalesce
)

# Collapse define
ideo_define_clean <- reduce(
  map(define_vars, ~ na_if(as.character(clean[[.x]]), "")),
  coalesce
)
# Add collapsed columns to df
clean <- clean %>%
  mutate(
    ideo_vote_fed_clean  = ideo_vote_fed_clean,
    ideo_vote_prov_clean = ideo_vote_prov_clean,
    ideo_define_clean    = ideo_define_clean
  )


# Step 4: Drop only the old vote/define columns, keep clean and other ideo variables
vote_define_old <- c(fed_vars, prov_vars, define_vars)

clean <- clean %>%
  select(
    -all_of(vote_define_old),
    -matches("_TEXT$")
  )


# Step 5: Check what ideo columns remain
grep("^ideo_", names(clean), value = TRUE)

# Clean trust questions (Qualtrics issue, question not asked for msot)
n_obs <- clean %>% filter(!is.na(trust_institution_1)) %>% count()
print(n_obs) # 2411 GOOD!

# Collapse
trust_social_vars <- names(clean)[grepl("^trust_social_", names(clean))]

clean <- clean %>%
  mutate(across(all_of(trust_social_vars), ~na_if(as.character(.x), ""))) %>%
  mutate(trust_social_clean = coalesce(!!!syms(trust_social_vars))) %>%
  select(-all_of(trust_social_vars))

# Inst_trust 
# Define the mapping of new variables to existing trust variables
trust_mapping <- list(
  trust_inst_pp    = "trust_institution_1",
  trust_inst_fed   = "trust_institution_2",
  trust_inst_prov  = "trust_institution_3",
  trust_inst_media = "trust_institution_4")


# Get all relevant trust variables used in the mapping
all_trust_vars <- unique(unlist(trust_mapping))
existing_trust_vars <- intersect(all_trust_vars, names(clean))

# First, replace "" with NA across all relevant trust columns
clean <- clean %>%
  mutate(across(all_of(existing_trust_vars), ~na_if(as.character(.x), "")))

# Create each new variable using coalesce
for (new_var in names(trust_mapping)) {
  inst_vars <- trust_mapping[[new_var]]
  inst_vars <- inst_vars[inst_vars %in% names(clean)]  # Keep only existing ones
  
  if (length(inst_vars) > 0) {
    clean <- clean %>%
      mutate(!!new_var := coalesce(!!!syms(inst_vars)))
  }
}

# Drop all the old trust variables
clean <- clean %>%
  select(-any_of(existing_trust_vars))

write.csv(
  clean,
  file = "data/aca_clean_W26.csv",
  row.names = FALSE
)

# -----------------------
# 2. Clean metadata rows for codebook
# -----------------------
questions <- clean[1, ]
values <- clean[2, ]

data <- clean[-c(1,2), ]
names(clean) <- names(questions)
# -----------------------
# 4. Generate codebook
# -----------------------
# Create the `values` column with unique values per variable
values <- map_chr(clean, ~ paste(unique(.x), collapse = ", "))

# Assuming `questions` is defined and has the same length as `data`
codebook <- tibble(
  variable = names(clean),
  question = as.character(questions),
  values = values
)

list(codebook$variable)
saveRDS(codebook, file = "codebook_clean.rds")

# -----------------------
# ELSA : Rename variables and check for coding
#-------------------------
clean <- read.csv("data/aca_clean_W26.csv")
  
    
##Cleaning Elsa
  DataClean <- data.frame(id = 1:nrow(clean))


#User Language--------------------------------------------------------------------
attributes(clean$UserLanguage)
table(clean$UserLanguage)
DataClean$userFR_bin <- ifelse(clean$UserLanguage == "FR", 1, 0)
table(DataClean$userFR_bin)

#What is your gender?-------------------------------------------------------------
attributes(clean$ses_gender)
table(clean$ses_gender)
DataClean$ses_male_bin <- ifelse(clean$ses_gender == "Male", 1, 0)
DataClean$ses_male_bin[clean$ses_gender == "Other"] <- NA
table(DataClean$ses_male_bin)


#In which year you were born?-------------------------------------------------------------
table(clean$ses_year_born., useNA = "ifany")

# 2) Calculer l'âge 
DataClean$ses_age <- NA
DataClean$ses_age <- 2025 - as.numeric(clean$ses_year_born)

# 3) Vérifier l'âge
table(DataClean$ses_age, useNA = "ifany")

# 4) Binaires

# 18-34
DataClean$age18_34_bin <- NA
DataClean$age18_34_bin[DataClean$ses_age >= 18 & DataClean$ses_age <= 34] <- 1
DataClean$age18_34_bin[!(DataClean$ses_age >= 18 & DataClean$ses_age <= 34) &
                         !is.na(DataClean$ses_age)] <- 0
table(DataClean$age18_34_bin, useNA = "always")

# 35-54
DataClean$age35_54_bin <- NA
DataClean$age35_54_bin[DataClean$ses_age >= 35 & DataClean$ses_age <= 54] <- 1
DataClean$age35_54_bin[!(DataClean$ses_age >= 35 & DataClean$ses_age <= 54) &
                         !is.na(DataClean$ses_age)] <- 0
table(DataClean$age35_54_bin, useNA = "always")

# 55+
DataClean$age55plus_bin <- NA
DataClean$age55plus_bin[DataClean$ses_age >= 55] <- 1
DataClean$age55plus_bin[!(DataClean$ses_age >= 55) & !is.na(DataClean$ses_age)] <- 0
table(DataClean$age55plus_bin, useNA = "always")


# ! Check in the new survey, provinces include : AB, ON, QC, NB, NL, NS, PEI 
#          (to become a categorical variable (AB, ON, QC, EC & a bin for each, e.g. reg_ab [0,1]))

# In which province or territory are you currently living?-------------------------------------------------------------
attributes(clean$ses_region.)
table(clean$ses_region.)

DataClean$alberta_bin <- ifelse(clean$ses_region. == "Alberta", 1, 0)
DataClean$bcolumbia_bin <- ifelse(clean$ses_region. == "British Columbia", 1, 0)
DataClean$nova_scotia_bin <- ifelse(clean$ses_region. == "Nova Scotia", 1, 0)
DataClean$ontario_bin <- ifelse(clean$ses_region. == "Ontario", 1, 0)
DataClean$quebec_bin <- ifelse(clean$ses_region. == "Quebec", 1, 0)
DataClean$newfound_lab_bin <- ifelse(clean$ses_region. == "Newfoundland and Labrador", 1, 0)
DataClean$prince_edward_bin <- ifelse(clean$ses_region. == "Prince Edward Island", 1, 0)

table(DataClean$alberta_bin)
table(DataClean$quebec_bin)
table(DataClean$prince_edward_bin)

#AB, ON, QC, EC---------------------------------------------------------------------------------------------------------
attributes(clean$ses_region.)
table(clean$ses_region.)

# Nettoyage catégoriel ses_region. -> 4 catégories

clean$ses_region. <- trimws(as.character(clean$ses_region.))
clean$ses_region.[clean$ses_region. == ""] <- NA

main_regions <- c("Alberta", "Quebec", "Ontario")
east_coast_regions <- c("New Brunswick", "Nova Scotia",
                        "Newfoundland and Labrador", "Prince Edward Island")

clean$ses_region_cat <- NA_character_

mask_main <- clean$ses_region. %in% main_regions
mask_east <- clean$ses_region. %in% east_coast_regions

clean$ses_region_cat[mask_main] <- clean$ses_region.[mask_main]
clean$ses_region_cat[mask_east] <- "East Coast"

clean$ses_region_cat <- factor(clean$ses_region_cat,
                               levels = c("Alberta","Quebec","Ontario","East Coast"))


table(clean$ses_region., useNA="ifany")
table(clean$ses_region_cat, useNA="ifany")

DataClean$ses_region_cat <- clean$ses_region_cat

###binaire East Coast

DataClean$region_eastcoast_bin <- NA
DataClean$region_eastcoast_bin[DataClean$ses_region_cat == "East Coast"] <- 1
DataClean$region_eastcoast_bin[DataClean$ses_region_cat != "East Coast" &
                                 !is.na(DataClean$ses_region_cat)] <- 0
table(DataClean$region_eastcoast_bin, useNA="always")


# What language do you speak most often at home?-------------------------------------------------------------------------
table(clean$ses_language)
DataClean$ses_french_bin <- ifelse(clean$ses_language == "French", 1, 0)
DataClean$ses_french_bin[clean$ses_language == "Other"] <- NA
table(DataClean$ses_french_bin)

# What is the highest level of education that you have completed?--------------------------------------------------------
table(clean$ses_education)

# Initialisation
DataClean$educ_group <- NA

# Groupe 1 : Avant le secondaire (educBHS)
DataClean$educ_group[clean$ses_education == "Completed elementary school"] <- "educBHS"

# Groupe 2 : Secondaire et collégial/technique (educHS)
DataClean$educ_group[clean$ses_education %in% c(
  "Completed secondary/ high school",
  "Some secondary/ high school",
  "Completed technical, community college, CEGEP, College Classique",
  "Some technical, community college, CEGEP, College Classique"
)] <- "educHS"

# Groupe 3 : Universitaire (educUniv)
DataClean$educ_group[clean$ses_education %in% c(
  "Bachelor’s degree",
  "Some university",
  "Master’s degree",
  "Professional degree or doctorate"
)] <- "educUniv"

table(DataClean$educ_group)

# Variables binaires pour chaque groupe d'éducation
DataClean$educBHS <- ifelse(DataClean$educ_group == "educBHS", 1, 0)
DataClean$educHS  <- ifelse(DataClean$educ_group == "educHS", 1, 0)
#educUniv_bin comme référence

table(DataClean$educHS)

#Approximately, which of the following categories does your-----------------------------------------------------------------------------------------
table(clean$ses_income)
DataClean$ses_income_char <- NA
DataClean$ses_income3Cat <- NA

# Nettoyage des réponses income en format texte propre
DataClean$ses_income_char[clean$ses_income == "$1 to $30,000"]             <- "1_to_30000"
DataClean$ses_income_char[clean$ses_income == "$30,001 to $60,000"]        <- "30001_to_60000"
DataClean$ses_income_char[clean$ses_income == "$60,001 to $90,000"]        <- "60001_to_90000"
DataClean$ses_income_char[clean$ses_income == "$90,001 to $110,000"]       <- "90001_to_110000"
DataClean$ses_income_char[clean$ses_income == "$110,001 to $150,000"]      <- "110001_to_150000"
DataClean$ses_income_char[clean$ses_income == "$150,001 to $200,000"]      <- "150001_to_200000"
DataClean$ses_income_char[clean$ses_income == "More than $200,000"]        <- "more_than_200000"

# Création des 3 grandes catégories de revenu
DataClean$ses_income3Cat[DataClean$ses_income_char %in% c("1_to_30000")] <- "Low"

DataClean$ses_income3Cat[DataClean$ses_income_char %in% c(
  "30001_to_60000", 
  "60001_to_90000", 
  "90001_to_110000", 
  "110001_to_150000"
)] <- "Mid"

DataClean$ses_income3Cat[DataClean$ses_income_char %in% c(
  "150001_to_200000", 
  "more_than_200000"
)] <- "High"  # Référence

## Vérifier
table(DataClean$ses_income3Cat, useNA = "ifany")

# Low
DataClean$incomeLow_bin <- NA
DataClean$incomeLow_bin[DataClean$ses_income3Cat == "Low"] <- 1
DataClean$incomeLow_bin[DataClean$ses_income3Cat != "Low" & !is.na(DataClean$ses_income3Cat)] <- 0
table(DataClean$incomeLow_bin, useNA = "always")

# Mid
DataClean$incomeMid_bin <- NA
DataClean$incomeMid_bin[DataClean$ses_income3Cat == "Mid"] <- 1
DataClean$incomeMid_bin[DataClean$ses_income3Cat != "Mid" & !is.na(DataClean$ses_income3Cat)] <- 0
table(DataClean$incomeMid_bin, useNA = "always")

# High
DataClean$incomeHigh_bin <- NA
DataClean$incomeHigh_bin[DataClean$ses_income3Cat == "High"] <- 1
DataClean$incomeHigh_bin[DataClean$ses_income3Cat != "High" & !is.na(DataClean$ses_income3Cat)] <- 0
table(DataClean$incomeHigh_bin, useNA = "always")


# Vérification
table(DataClean$ses_income3Cat, useNA = "always")
table(DataClean$incomeLow_bin, useNA = "always")
table(DataClean$incomeMid_bin, useNA = "always")
table(DataClean$incomeHigh_bin, useNA = "always")



# What are the first three characters of your postal code?-----------------------------------------------------
#attributes(clean$ses_postal_code)
#table(clean$ses_postal_code)
#DataClean$ses_postalCode <- NA
#DataClean$ses_postalCode <- clean$ses_postal_code
#table(DataClean$ses_postalCode)

# Do you live...----------------------------------------------------------------------
attributes(clean$ses_children.)
table(clean$ses_children)

# Nettoyage de la variable enfants
DataClean$ses_children_char <- NA
DataClean$ses_children_char[clean$ses_children == "Alone with children"]        <- "alone_with_children"
DataClean$ses_children_char[clean$ses_children == "Alone without children"]     <- "alone_without_children"
DataClean$ses_children_char[clean$ses_children == "In couple with children"]    <- "in_couple_with_children"
DataClean$ses_children_char[clean$ses_children == "In couple without children"] <- "in_couple_without_children"

table(DataClean$ses_children_char)

# Variable binaire : 1 = avec enfants, 0 = sans enfants
DataClean$children_bin <- ifelse(DataClean$ses_children_char %in% c(
  "alone_with_children", "in_couple_with_children"
), 1, 0)
table(DataClean$children_bin)

#Were you born in Canada?-----------------------------------------------------------------------------------------
attributes(clean$ses_citizen_status)
table(clean$ses_citizen_status)
DataClean$ses_citizenYes_bin <- ifelse(clean$ses_citizen_status == "Yes", 1, 0)
table(DataClean$ses_citizenYes_bin)

## What best describes your current employment status? ------------------------

attributes(clean$ses_employ_status)
table(clean$ses_employ_status, useNA = "ifany")

DataClean$ses_employ_status <- NA

emp_std <- trimws(as.character(clean$ses_employ_status))

DataClean$ses_employ_status[emp_std == "A caregiver or homemaker"] <- "caregiver_or_homemaker"
DataClean$ses_employ_status[emp_std == "A student attending school"] <- "student_attending_school"
DataClean$ses_employ_status[emp_std == "Not working due to illness/disability, or not looking for work"] <- "not_working_due_to_illness_disability_or_not_looking_for_work"
DataClean$ses_employ_status[emp_std == "Retired"] <- "retired"
DataClean$ses_employ_status[emp_std == "Seasonal work"] <- "seasonal_work"
DataClean$ses_employ_status[emp_std == "Self-employed"] <- "self_employed"
DataClean$ses_employ_status[emp_std == "Temporarily not working (e.g. parental leave, seasonal worker, in the process of changing jobs)"] <- "temporarily_not_working"
DataClean$ses_employ_status[emp_std == "Unemployed, and looking for work"] <- "unemployed"
DataClean$ses_employ_status[emp_std == "Working full-time (35 or more hours per week)"] <- "working_full_time"
DataClean$ses_employ_status[emp_std == "Working part-time (less than 35 hours per week)"] <- "working_part_time"

# 4) Convertir en facteur (avec niveaux)
DataClean$ses_employ_status <- factor(
  DataClean$ses_employ_status,
  levels = c(
    "caregiver_or_homemaker",
    "student_attending_school",
    "not_working_due_to_illness_disability_or_not_looking_for_work",
    "retired",
    "seasonal_work",
    "self_employed",
    "temporarily_not_working",
    "unemployed",
    "working_full_time",
    "working_part_time"
  )
)

# 5) Vérifier
table(DataClean$ses_employ_status, useNA = "ifany")

# 6) Variable binaire
DataClean$employ_fulltime_bin <- NA
DataClean$employ_fulltime_bin[DataClean$ses_employ_status == "working_full_time"] <- 1
DataClean$employ_fulltime_bin[DataClean$ses_employ_status != "working_full_time" &
                                !is.na(DataClean$ses_employ_status)] <- 0

# 7) Vérification
table(DataClean$employ_fulltime_bin, useNA = "always")

#  Binaires pour CHAQUE catégorie 

# caregiver_or_homemaker
DataClean$employ_caregiver_bin <- NA
DataClean$employ_caregiver_bin[DataClean$ses_employ_status == "caregiver_or_homemaker"] <- 1
DataClean$employ_caregiver_bin[DataClean$ses_employ_status != "caregiver_or_homemaker" &
                                 !is.na(DataClean$ses_employ_status)] <- 0
table(DataClean$employ_caregiver_bin, useNA = "always")

# student_attending_school
DataClean$employ_student_bin <- NA
DataClean$employ_student_bin[DataClean$ses_employ_status == "student_attending_school"] <- 1
DataClean$employ_student_bin[DataClean$ses_employ_status != "student_attending_school" &
                               !is.na(DataClean$ses_employ_status)] <- 0
table(DataClean$employ_student_bin, useNA = "always")

# not_working_due_to_illness_disability_or_not_looking_for_work
DataClean$employ_notworking_health_bin <- NA
DataClean$employ_notworking_health_bin[
  DataClean$ses_employ_status == "not_working_due_to_illness_disability_or_not_looking_for_work"
] <- 1
DataClean$employ_notworking_health_bin[
  DataClean$ses_employ_status != "not_working_due_to_illness_disability_or_not_looking_for_work" &
    !is.na(DataClean$ses_employ_status)
] <- 0
table(DataClean$employ_notworking_health_bin, useNA = "always")

# retired  
DataClean$employ_retired_bin <- NA
DataClean$employ_retired_bin[DataClean$ses_employ_status == "retired"] <- 1
DataClean$employ_retired_bin[DataClean$ses_employ_status != "retired" &
                               !is.na(DataClean$ses_employ_status)] <- 0
table(DataClean$employ_retired_bin, useNA = "always")

# seasonal_work
DataClean$employ_seasonal_bin <- NA
DataClean$employ_seasonal_bin[DataClean$ses_employ_status == "seasonal_work"] <- 1
DataClean$employ_seasonal_bin[DataClean$ses_employ_status != "seasonal_work" &
                                !is.na(DataClean$ses_employ_status)] <- 0
table(DataClean$employ_seasonal_bin, useNA = "always")

# self_employed
DataClean$employ_selfemployed_bin <- NA
DataClean$employ_selfemployed_bin[DataClean$ses_employ_status == "self_employed"] <- 1
DataClean$employ_selfemployed_bin[DataClean$ses_employ_status != "self_employed" &
                                    !is.na(DataClean$ses_employ_status)] <- 0
table(DataClean$employ_selfemployed_bin, useNA = "always")

# temporarily_not_working
DataClean$employ_tempnotworking_bin <- NA
DataClean$employ_tempnotworking_bin[DataClean$ses_employ_status == "temporarily_not_working"] <- 1
DataClean$employ_tempnotworking_bin[DataClean$ses_employ_status != "temporarily_not_working" &
                                      !is.na(DataClean$ses_employ_status)] <- 0
table(DataClean$employ_tempnotworking_bin, useNA = "always")

# unemployed
DataClean$employ_unemployed_bin <- NA
DataClean$employ_unemployed_bin[DataClean$ses_employ_status == "unemployed"] <- 1
DataClean$employ_unemployed_bin[DataClean$ses_employ_status != "unemployed" &
                                  !is.na(DataClean$ses_employ_status)] <- 0
table(DataClean$employ_unemployed_bin, useNA = "always")

# working_part_time
DataClean$employ_parttime_bin <- NA
DataClean$employ_parttime_bin[DataClean$ses_employ_status == "working_part_time"] <- 1
DataClean$employ_parttime_bin[DataClean$ses_employ_status != "working_part_time" &
                                !is.na(DataClean$ses_employ_status)] <- 0
table(DataClean$employ_parttime_bin, useNA = "always")

#-----------------------------------------------------------------------------------------------------------------------------
#How many children do you have in each of the following age groups currently living with you in your household? - Ages 0 to 5--------------

# 1)
attributes(clean$ses_household_compo_1)
table(clean$ses_household_compo_1, useNA = "ifany")

# 2) Créer la variable clean
DataClean$ses_children05 <- NA

# 3) Recoder (codes numériques -> labels)
DataClean$ses_children05[clean$ses_household_compo_1 == 0] <- "0_child_0_5"
DataClean$ses_children05[clean$ses_household_compo_1 == 1] <- "1_child_0_5"
DataClean$ses_children05[clean$ses_household_compo_1 == 2] <- "2_children_0_5"
DataClean$ses_children05[clean$ses_household_compo_1 == 3] <- "3_children_0_5"


DataClean$ses_children05[clean$ses_household_compo_1 >= 4] <- "4plus_children_0_5"

# 4) Factor + levels
DataClean$ses_children05 <- factor(
  DataClean$ses_children05,
  levels = c("0_child_0_5",
             "1_child_0_5",
             "2_children_0_5",
             "3_children_0_5",
             "4plus_children_0_5")
)

# 5) Vérifier
table(DataClean$ses_children05, useNA = "ifany")

# 6) Binaire 
DataClean$children_preschool_bin <- NA
DataClean$children_preschool_bin[clean$ses_household_compo_1 == 0] <- 0
DataClean$children_preschool_bin[clean$ses_household_compo_1 > 0]  <- 1

# 7) Vérification
table(DataClean$children_preschool_bin, useNA = "always")


#How many children do you have in each of the following age groups currently living with you in your household? - Ages 6-12-----------------
table(clean$ses_household_compo_2)

# Nettoyage de la variable enfants 6-12 ans
DataClean$ses_children612_char <- NA
DataClean$ses_children612_char[clean$ses_household_compo_2 == 0] <- "0 enfant 6-12 ans"
DataClean$ses_children612_char[clean$ses_household_compo_2 == 1] <- "1 enfant 6-12 ans"
DataClean$ses_children612_char[clean$ses_household_compo_2 == 2] <- "2 enfants 6-12 ans"
DataClean$ses_children612_char[clean$ses_household_compo_2 == 3] <- "3 enfants 6-12 ans"

table(DataClean$ses_children612_char)

# Variable binaire : 1 = au moins 1 enfant 6-12 ans, 0 = aucun
DataClean$children_elementary_bin <- ifelse(clean$ses_household_compo_2 > 0, 1, 0)

# Vérification
table(DataClean$children_elementary_bin, useNA = "always")


#How many children do you have in each of the following age groups currently living with you in your household? - Ages 13-17-----------------
table(clean$ses_household_compo_3)

# Nettoyage de la variable enfants 13-17 ans
DataClean$ses_children1317_char <- NA
DataClean$ses_children1317_char[clean$ses_household_compo_3 == 0] <- "0 enfant 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo_3 == 1] <- "1 enfant 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo_3 == 2] <- "2 enfants 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo_3 == 3] <- "3 enfants 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo_3 == 4] <- "4 enfants 13-17 ans"

table(DataClean$ses_children1317_char)

# Variable binaire : 1 = au moins 1 enfant 13-17 ans, 0 = aucun
DataClean$children_teen_bin <- ifelse(clean$ses_household_compo_3 > 0, 1, 0)

# Vérification
table(DataClean$children_teen_bin, useNA = "always")


#How many children do you have in each of the following age groups currently living with you in your household? - Ages 18+------------------
table(clean$ses_household_compo_4)


# 2) Variable label (comme tu l'as déjà)
DataClean$ses_children18_char <- NA
DataClean$ses_children18_char[clean$ses_household_compo_4 == 0] <- "0 enfant 18+ ans"
DataClean$ses_children18_char[clean$ses_household_compo_4 == 1] <- "1 enfant 18+ ans"
DataClean$ses_children18_char[clean$ses_household_compo_4 == 2] <- "2 enfants 18+ ans"
DataClean$ses_children18_char[clean$ses_household_compo_4 == 3] <- "3 enfants 18+ ans"
DataClean$ses_children18_char[clean$ses_household_compo_4 == 4] <- "4 enfants 18+ ans"
DataClean$ses_children18_char[clean$ses_household_compo_4 >= 5] <- "5+ enfants 18+ ans"
table(DataClean$ses_children18_char, useNA = "ifany")

# 3) Binaire 
DataClean$children18plus_bin <- NA
DataClean$children18plus_bin[clean$ses_household_compo_4 == 0] <- 0
DataClean$children18plus_bin[clean$ses_household_compo_4 > 0]  <- 1
table(DataClean$children18plus_bin, useNA = "always")


#In politics, people sometimes talk of left and right. - Where would you place yourself on this scale with 0 being entirely to the left and 10 being entirely to the right?
table(clean$ideo_left_right_1)

DataClean$ideo_right_num <- NA
DataClean$ideo_right_num[clean$ideo_left_right_1 == 10] <- 1
DataClean$ideo_right_num[clean$ideo_left_right_1 == 9] <- 0.9
DataClean$ideo_right_num[clean$ideo_left_right_1 == 8] <- 0.8
DataClean$ideo_right_num[clean$ideo_left_right_1 == 7] <- 0.7
DataClean$ideo_right_num[clean$ideo_left_right_1 == 6] <- 0.6
DataClean$ideo_right_num[clean$ideo_left_right_1 == 5] <- 0.5
DataClean$ideo_right_num[clean$ideo_left_right_1 == 4] <- 0.4
DataClean$ideo_right_num[clean$ideo_left_right_1 == 3] <- 0.3
DataClean$ideo_right_num[clean$ideo_left_right_1 == 2] <- 0.2
DataClean$ideo_right_num[clean$ideo_left_right_1 == 1] <- 0.1
DataClean$ideo_right_num[clean$ideo_left_right_1 == 0] <- 0

table(DataClean$ideo_right_num)

#How interested are you in politics generally? - Select a number from 0 to 10, where 0 means no interest at all, and 10 means a great deal of interest.
table(clean$ideo_interest._1)

DataClean$ideo_interest_politics_num <- NA
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 10] <- 1
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 9] <- 0.9
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 8] <- 0.8
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 7] <- 0.7
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 6] <- 0.6
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 5] <- 0.5
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 4] <- 0.4
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 3] <- 0.3
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 2] <- 0.2
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 1] <- 0.1
DataClean$ideo_interest_politics_num[clean$ideo_interest._1 == 0] <- 0
table(DataClean$ideo_interest_politics_num)

#People have different ways of defining themselves. What do you consider yourself?
table(clean$ideo_define_clean)
# Clean better for full survey!
DataClean$ideo_define_num <- NA_real_
DataClean$ideo_define_num[clean$ideo_define_clean == "Solely as Canadian"] <- 1
DataClean$ideo_define_num[clean$ideo_define_clean == "First Canadian, second Albertan"] <- 0.75
DataClean$ideo_define_num[clean$ideo_define_clean == "First Canadian, second Ontarian"] <- 0.75
DataClean$ideo_define_num[clean$ideo_define_clean == "First Quebecer, second Canadian"] <- 0.75
DataClean$ideo_define_num[clean$ideo_define_clean == "First Canadian, second Nova Scotian"] <- 0.75
DataClean$ideo_define_num[clean$ideo_define_clean == "Equally Canadian and Albertan"] <- 0.5
DataClean$ideo_define_num[clean$ideo_define_clean == "Equally Canadian and Ontarian"] <- 0.5
DataClean$ideo_define_num[clean$ideo_define_clean == "Equally Canadian and Quebecer"] <- 0.5
DataClean$ideo_define_num[clean$ideo_define_clean == "First Quebecer, second Canadian"] <- 0.25
DataClean$ideo_define_num[clean$ideo_define_clean == "Solely as Quebecer"] <- 0
table(DataClean$ideo_define_num)

#binaire province

 
table(clean$ideo_define_clean, useNA = "ifany")

# 2) QUEBEC
DataClean$ideo_define_QC_first_bin <- NA
DataClean$ideo_define_QC_first_bin[clean$ideo_define_clean == "First Quebecer, second Canadian"] <- 1
DataClean$ideo_define_QC_first_bin[clean$ideo_define_clean != "First Quebecer, second Canadian" &
                                     !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_QC_first_bin, useNA = "always")

# 3) ONTARIO
DataClean$ideo_define_ON_first_bin <- NA
DataClean$ideo_define_ON_first_bin[clean$ideo_define_clean == "First Ontarian, second Canadian"] <- 1
DataClean$ideo_define_ON_first_bin[clean$ideo_define_clean != "First Ontarian, second Canadian" &
                                     !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_ON_first_bin, useNA = "always")

# 4) ALBERTA
DataClean$ideo_define_AL_first_bin <- NA
DataClean$ideo_define_AL_first_bin[clean$ideo_define_clean == "First Albertan, second Canadian"] <- 1
DataClean$ideo_define_AL_first_bin[clean$ideo_define_clean != "First Albertan, second Canadian" &
                                     !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_AL_first_bin, useNA = "always")

# 5) NEW BRUNSWICK
DataClean$ideo_define_NB_first_bin <- NA
DataClean$ideo_define_NB_first_bin[clean$ideo_define_clean == "First New Brunswicker, second Canadian"] <- 1
DataClean$ideo_define_NB_first_bin[clean$ideo_define_clean != "First New Brunswicker, second Canadian" &
                                     !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_NB_first_bin, useNA = "always")

# 6) NOVA SCOTIA
DataClean$ideo_define_NS_first_bin <- NA
DataClean$ideo_define_NS_first_bin[clean$ideo_define_clean == "First Nova Scotian, second Canadian"] <- 1
DataClean$ideo_define_NS_first_bin[clean$ideo_define_clean != "First Nova Scotian, second Canadian" &
                                     !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_NS_first_bin, useNA = "always")

# 7) NEWFOUNDLAND AND LABRADOR
DataClean$ideo_define_NL_first_bin <- NA
DataClean$ideo_define_NL_first_bin[clean$ideo_define_clean == "First Newfoundlander, second Canadian"] <- 1
DataClean$ideo_define_NL_first_bin[clean$ideo_define_clean != "First Newfoundlander, second Canadian" &
                                     !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_NL_first_bin, useNA = "always")

# 8) PRINCE EDWARD ISLAND
DataClean$ideo_define_PE_first_bin <- NA
DataClean$ideo_define_PE_first_bin[clean$ideo_define_clean == "First Prince Edward Islander, second Canadian"] <- 1
DataClean$ideo_define_PE_first_bin[clean$ideo_define_clean != "First Prince Edward Islander, second Canadian" &
                                     !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_PE_first_bin, useNA = "always")

#binaire canada


table(clean$ideo_define_clean, useNA = "ifany")

# 2) ALBERTA — First Canadian, second Albertan
DataClean$ideo_define_canAL_bin <- NA
DataClean$ideo_define_canAL_bin[clean$ideo_define_clean == "First Canadian, second Albertan"] <- 1
DataClean$ideo_define_canAL_bin[clean$ideo_define_clean != "First Canadian, second Albertan" &
                                  !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_canAL_bin, useNA = "always")

# 3) NEW BRUNSWICK — First Canadian, second New Brunswicker
DataClean$ideo_define_canNB_bin <- NA
DataClean$ideo_define_canNB_bin[clean$ideo_define_clean == "First Canadian, second New Brunswicker"] <- 1
DataClean$ideo_define_canNB_bin[clean$ideo_define_clean != "First Canadian, second New Brunswicker" &
                                  !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_canNB_bin, useNA = "always")

# 4) NEWFOUNDLAND & LABRADOR — First Canadian, second Newfoundlander and/or Labradorian
DataClean$ideo_define_canNL_bin <- NA
DataClean$ideo_define_canNL_bin[clean$ideo_define_clean == "First Canadian, second Newfoundlander and/or Labradorian"] <- 1
DataClean$ideo_define_canNL_bin[clean$ideo_define_clean != "First Canadian, second Newfoundlander and/or Labradorian" &
                                  !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_canNL_bin, useNA = "always")

# 5) NOVA SCOTIA — First Canadian, second Nova Scotian
DataClean$ideo_define_canNS_bin <- NA
DataClean$ideo_define_canNS_bin[clean$ideo_define_clean == "First Canadian, second Nova Scotian"] <- 1
DataClean$ideo_define_canNS_bin[clean$ideo_define_clean != "First Canadian, second Nova Scotian" &
                                  !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_canNS_bin, useNA = "always")

# 6) ONTARIO — First Canadian, second Ontarian
DataClean$ideo_define_canON_bin <- NA
DataClean$ideo_define_canON_bin[clean$ideo_define_clean == "First Canadian, second Ontarian"] <- 1
DataClean$ideo_define_canON_bin[clean$ideo_define_clean != "First Canadian, second Ontarian" &
                                  !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_canON_bin, useNA = "always")

# 7) PRINCE EDWARD ISLAND — First Canadian, second Prince Edward Islander
DataClean$ideo_define_canPE_bin <- NA
DataClean$ideo_define_canPE_bin[clean$ideo_define_clean == "First Canadian, second Prince Edward Islander"] <- 1
DataClean$ideo_define_canPE_bin[clean$ideo_define_clean != "First Canadian, second Prince Edward Islander" &
                                  !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_canPE_bin, useNA = "always")

# 8) QUEBEC — First Canadian, second Quebecer
DataClean$ideo_define_canQC_bin <- NA
DataClean$ideo_define_canQC_bin[clean$ideo_define_clean == "First Canadian, second Quebecer"] <- 1
DataClean$ideo_define_canQC_bin[clean$ideo_define_clean != "First Canadian, second Quebecer" &
                                  !is.na(clean$ideo_define_clean)] <- 0
table(DataClean$ideo_define_canQC_bin, useNA = "always")

#############################################################################################################
#DataClean <- read.csv("data/aca_wrangled_W26.csv")



#Now we would like to ask you about public finance questions. Please remember to read closely and pay attention. You will be asked questions to check your memory and comprehension. Please arrange the following policy issues by order of importance - Health
table(clean$budget_issue_imp_1)

DataClean$budget_imp_health_num <- NA_real_
DataClean$budget_imp_health_num[clean$budget_issue_imp_1 == 1] <- 1
DataClean$budget_imp_health_num[clean$budget_issue_imp_1 == 2] <- 0.75
DataClean$budget_imp_health_num[clean$budget_issue_imp_1 == 3] <- 0.50
DataClean$budget_imp_health_num[clean$budget_issue_imp_1 == 4] <- 0.25
DataClean$budget_imp_health_num[clean$budget_issue_imp_1 == 5] <- 0
table(DataClean$budget_imp_health_num)

# Création de la variable binaire : 1 = priorité forte à la santé (1), 0 = le reste
DataClean$budget_imp_health_bin <- ifelse(
  DataClean$budget_imp_health_num %in% c(1), 1, 0
)

# Vérification
table(DataClean$budget_imp_health_bin, useNA = "always")

#Now we would like to ask you about public finance questions. Please remember to read closely and pay attention. You will be asked questions to check your memory and comprehension. Please arrange the following policy issues by order of importance - Education
table(clean$budget_issue_imp_2)

DataClean$budget_imp_edu_num <- NA_real_
DataClean$budget_imp_edu_num[clean$budget_issue_imp_2 == 1] <- 1
DataClean$budget_imp_edu_num[clean$budget_issue_imp_2 == 2] <- 0.75
DataClean$budget_imp_edu_num[clean$budget_issue_imp_2 == 3] <- 0.50
DataClean$budget_imp_edu_num[clean$budget_issue_imp_2 == 4] <- 0.25
DataClean$budget_imp_edu_num[clean$budget_issue_imp_2 == 5] <- 0
table(DataClean$budget_imp_edu_num)

# Création de la variable binaire : 1 = priorité forte à l'éducation (1), 0 = le reste
DataClean$budget_imp_edu_bin <- ifelse(
  DataClean$budget_imp_edu_num %in% c(1), 1, 0
)

# Vérification
table(DataClean$budget_imp_edu_bin, useNA = "always")

#Now we would like to ask you about public finance questions. Please remember to read closely and pay attention. You will be asked questions to check your memory and comprehension. Please arrange the following policy issues by order of importance - Pensions
table(clean$budget_issue_imp_3)

DataClean$budget_imp_pensions_num <- NA_real_
DataClean$budget_imp_pensions_num[clean$budget_issue_imp_3 == 1] <- 1
DataClean$budget_imp_pensions_num[clean$budget_issue_imp_3 == 2] <- 0.75
DataClean$budget_imp_pensions_num[clean$budget_issue_imp_3 == 3] <- 0.50
DataClean$budget_imp_pensions_num[clean$budget_issue_imp_3 == 4] <- 0.25
DataClean$budget_imp_pensions_num[clean$budget_issue_imp_3 == 5] <- 0
table(DataClean$budget_imp_pensions_num)

# Création de la variable binaire : 1 = priorité forte aux pensions (1), 0 = le reste
DataClean$budget_imp_pensions_bin <- ifelse(
  DataClean$budget_imp_pensions_num %in% c(1), 1, 0
)

# Vérification
table(DataClean$budget_imp_pensions_bin, useNA = "always")

#Now we would like to ask you about public finance questions. Please remember to read closely and pay attention. You will be asked questions to check your memory and comprehension. Please arrange the following policy issues by order of importance - Taxes
table(clean$budget_issue_imp_4)

DataClean$budget_imp_taxes_num <- NA_real_
DataClean$budget_imp_taxes_num[clean$budget_issue_imp_4 == 1] <- 1
DataClean$budget_imp_taxes_num[clean$budget_issue_imp_4 == 2] <- 0.75
DataClean$budget_imp_taxes_num[clean$budget_issue_imp_4 == 3] <- 0.50
DataClean$budget_imp_taxes_num[clean$budget_issue_imp_4 == 4] <- 0.25
DataClean$budget_imp_taxes_num[clean$budget_issue_imp_4 == 5] <- 0
table(DataClean$budget_imp_taxes_num)

# Création de la variable binaire : 1 = priorité forte aux taxes (1), 0 = le reste
DataClean$budget_imp_taxes_bin <- ifelse(
  DataClean$budget_imp_taxes_num %in% c(1), 1, 0
)

# Vérification
table(DataClean$budget_imp_taxes_bin, useNA = "always")


#Now we would like to ask you about public finance questions. Please remember to read closely and pay attention. You will be asked questions to check your memory and comprehension. Please arrange the following policy issues by order of importance - Public debt
table(clean$budget_issue_imp_5)

DataClean$budget_imp_debt_num <- NA_real_
DataClean$budget_imp_debt_num[clean$budget_issue_imp_5 == 1] <- 1
DataClean$budget_imp_debt_num[clean$budget_issue_imp_5 == 2] <- 0.75
DataClean$budget_imp_debt_num[clean$budget_issue_imp_5 == 3] <- 0.50
DataClean$budget_imp_debt_num[clean$budget_issue_imp_5 == 4] <- 0.25
DataClean$budget_imp_debt_num[clean$budget_issue_imp_5 == 5] <- 0
table(DataClean$budget_imp_debt_num)

# Création de la variable binaire : 1 = priorité forte à la dette (1), 0 = le reste
DataClean$budget_imp_debt_bin <- ifelse(
  DataClean$budget_imp_debt_num %in% c(1), 1, 0
)

# Vérification
table(DataClean$budget_imp_debt_bin, useNA = "always")

# 100 points questions: For each question, you have 100 points to allocate...
#-----------------------------------------------------------------------------------------
##budget prio

# 1. Renommer les variables
DataClean <- clean %>%
  rename(
    budget_prio_health = budget_spend_prio_6,
    budget_prio_seniors = budget_spend_prio_7,
    budget_prio_cc = budget_spend_prio_8,
    budget_prio_ecn = budget_spend_prio_9,
    budget_prio_clim = budget_spend_prio_10
  )

# 2. Créer les variables binaires (1 = priorité #1, 0 = pas priorité #1)
DataClean <- DataClean %>%
  rowwise() %>%
  mutate(
    max_budget = max(c(budget_prio_health, budget_prio_seniors, budget_prio_cc, 
                       budget_prio_ecn, budget_prio_clim), na.rm = TRUE),
    
    # Compter combien de priorités partagent le maximum
    n_at_max = sum(c(budget_prio_health, budget_prio_seniors, budget_prio_cc, 
                     budget_prio_ecn, budget_prio_clim) == max_budget, na.rm = TRUE),
    
    # Binaire = 1 seulement si c'est le seul maximum
    budget_prio_health_bin = as.integer(budget_prio_health == max_budget & n_at_max == 1),
    budget_prio_seniors_bin = as.integer(budget_prio_seniors == max_budget & n_at_max == 1),
    budget_prio_cc_bin = as.integer(budget_prio_cc == max_budget & n_at_max == 1),
    budget_prio_ecn_bin = as.integer(budget_prio_ecn == max_budget & n_at_max == 1),
    budget_prio_clim_bin = as.integer(budget_prio_clim == max_budget & n_at_max == 1)
  ) %>%
  ungroup() %>%
  # Nettoyer les colonnes d'aide
  select(-max_budget, -n_at_max)

# Vérification

cat("=== VÉRIFICATION DES VARIABLES BINAIRES ===\n\n")

# Compter les 1 et les 0 pour chaque variable binaire
bin_vars <- c("budget_prio_health_bin", "budget_prio_seniors_bin", 
              "budget_prio_cc_bin", "budget_prio_ecn_bin", "budget_prio_clim_bin")

for (var in bin_vars) {
  cat(var, ":\n")
  print(table(DataClean[[var]], useNA = "ifany"))
  cat("\n")
}

# Résumé 
cat("=== RÉSUMÉ GLOBAL ===\n")
summary_bin <- DataClean %>%
  select(all_of(bin_vars)) %>%
  summarise(across(everything(), 
                   list(
                     zeros = ~sum(. == 0, na.rm = TRUE),
                     ones = ~sum(. == 1, na.rm = TRUE),
                     NAs = ~sum(is.na(.))
                   )))

print(summary_bin)

# Vérifier que chaque répondant a maximum 1 priorité
cat("\n=== VÉRIFICATION: Nombre de '1' par répondant ===\n")
DataClean <- DataClean %>%
  mutate(total_bins = budget_prio_health_bin + budget_prio_seniors_bin + 
           budget_prio_cc_bin + budget_prio_ecn_bin + budget_prio_clim_bin)

cat("Distribution du nombre de priorités #1 par répondant:\n")
print(table(DataClean$total_bins, useNA = "ifany"))
#1753 répondants ont une seule priorité claire (ils ont mis plus de points sur UN seul secteur) 
#662 répondants ont une égalité (ils ont mis le même nombre de points maximum sur 2+ secteurs) → pas de priorité claire, donc tous leurs "_bin" sont à 0

##----------------------------------------------------------------------------------------
##Tradeoff

# ============================================================
# FONCTION 
# ============================================================

create_policy_vars <- function(df, policy_cols) {
  df <- df %>%
    rowwise() %>%
    mutate(
      max_val = suppressWarnings(max(c_across(all_of(policy_cols)), na.rm = TRUE)),
      max_val = ifelse(is.infinite(max_val), NA, max_val),
      n_at_max = sum(c_across(all_of(policy_cols)) == max_val, na.rm = TRUE),
      mean_allocation = mean(c_across(all_of(policy_cols)), na.rm = TRUE)
    ) %>%
    ungroup()
  
  for (col in policy_cols) {
    bin_name <- paste0(col, "_bin")
    int_name <- paste0(col, "_intensity")
    
    df <- df %>%
      mutate(
        !!bin_name := ifelse(is.na(max_val), NA, as.integer(!!sym(col) == max_val & n_at_max == 1)),
        !!int_name := !!sym(col) - mean_allocation
      )
  }
  
  df <- df %>% select(-max_val, -n_at_max, -mean_allocation)
  return(df)
}

# nettoyage

DataClean <- clean %>%
  rename(
    tradeoff_cc1_tax = budget_tradeoff_cc1_17,
    tradeoff_cc1_cut = budget_tradeoff_cc1_18,
    tradeoff_cc1_debt = budget_tradeoff_cc1_19,
    tradeoff_cc1_no_spend = budget_tradeoff_cc1_20,
    
    tradeoff_ge_tax = budget_tradeoff_ge_6,
    tradeoff_ge_cut = budget_tradeoff_ge_7,
    tradeoff_ge_debt = budget_tradeoff_ge_8,
    tradeoff_ge_no_spend = budget_tradeoff_ge_9,
    
    tradeoff_tax_less_services = budget_tradeoff_tax_6,
    tradeoff_tax_sales_tax = budget_tradeoff_tax_7,
    tradeoff_tax_inc_tax = budget_tradeoff_tax_8,
    tradeoff_tax_wealth_tax = budget_tradeoff_tax_9,
    
    tradeoff_hc_all = budget_tradeoff_hc_21,
    tradeoff_hc_spend = budget_tradeoff_hc_22,
    tradeoff_hc_pensions = budget_tradeoff_hc_23,
    
    tradeoff_cc2_all = budget_tradeoff_cc2_21,
    tradeoff_cc2_low_inc = budget_tradeoff_cc2_22,
    tradeoff_cc2_educ_all = budget_tradeoff_cc2_23,
    tradeoff_cc2_educ_low_inc = budget_tradeoff_cc2_24
  ) %>%
  mutate(
    tradeoff_cc1_tax = as.numeric(tradeoff_cc1_tax),
    tradeoff_cc1_cut = as.numeric(tradeoff_cc1_cut),
    tradeoff_cc1_debt = as.numeric(tradeoff_cc1_debt),
    tradeoff_cc1_no_spend = as.numeric(tradeoff_cc1_no_spend),
    
    tradeoff_ge_tax = as.numeric(tradeoff_ge_tax),
    tradeoff_ge_cut = as.numeric(tradeoff_ge_cut),
    tradeoff_ge_debt = as.numeric(tradeoff_ge_debt),
    tradeoff_ge_no_spend = as.numeric(tradeoff_ge_no_spend),
    
    tradeoff_tax_less_services = as.numeric(tradeoff_tax_less_services),
    tradeoff_tax_sales_tax = as.numeric(tradeoff_tax_sales_tax),
    tradeoff_tax_inc_tax = as.numeric(tradeoff_tax_inc_tax),
    tradeoff_tax_wealth_tax = as.numeric(tradeoff_tax_wealth_tax),
    
    tradeoff_hc_all = as.numeric(tradeoff_hc_all),
    tradeoff_hc_spend = as.numeric(tradeoff_hc_spend),
    tradeoff_hc_pensions = as.numeric(tradeoff_hc_pensions),
    
    tradeoff_cc2_all = as.numeric(tradeoff_cc2_all),
    tradeoff_cc2_low_inc = as.numeric(tradeoff_cc2_low_inc),
    tradeoff_cc2_educ_all = as.numeric(tradeoff_cc2_educ_all),
    tradeoff_cc2_educ_low_inc = as.numeric(tradeoff_cc2_educ_low_inc)
  )

# Créer variables binaires et intensité
DataClean <- create_policy_vars(DataClean, c("tradeoff_cc1_tax", "tradeoff_cc1_cut", "tradeoff_cc1_debt", "tradeoff_cc1_no_spend"))
DataClean <- create_policy_vars(DataClean, c("tradeoff_ge_tax", "tradeoff_ge_cut", "tradeoff_ge_debt", "tradeoff_ge_no_spend"))
DataClean <- create_policy_vars(DataClean, c("tradeoff_tax_less_services", "tradeoff_tax_sales_tax", "tradeoff_tax_inc_tax", "tradeoff_tax_wealth_tax"))
DataClean <- create_policy_vars(DataClean, c("tradeoff_hc_all", "tradeoff_hc_spend", "tradeoff_hc_pensions"))
DataClean <- create_policy_vars(DataClean, c("tradeoff_cc2_all", "tradeoff_cc2_low_inc", "tradeoff_cc2_educ_all", "tradeoff_cc2_educ_low_inc"))

# ============================================================
# CRÉER VARIABLES BINAIRES BASÉES SUR INTENSITÉ >= 50
# ============================================================

DataClean <- DataClean %>%
  mutate(
    # cc1
    tradeoff_cc1_tax_strong = as.integer(tradeoff_cc1_tax_intensity >= 50),
    tradeoff_cc1_cut_strong = as.integer(tradeoff_cc1_cut_intensity >= 50),
    tradeoff_cc1_debt_strong = as.integer(tradeoff_cc1_debt_intensity >= 50),
    tradeoff_cc1_no_spend_strong = as.integer(tradeoff_cc1_no_spend_intensity >= 50),
    
    # ge
    tradeoff_ge_tax_strong = as.integer(tradeoff_ge_tax_intensity >= 50),
    tradeoff_ge_cut_strong = as.integer(tradeoff_ge_cut_intensity >= 50),
    tradeoff_ge_debt_strong = as.integer(tradeoff_ge_debt_intensity >= 50),
    tradeoff_ge_no_spend_strong = as.integer(tradeoff_ge_no_spend_intensity >= 50),
    
    # tax
    tradeoff_tax_less_services_strong = as.integer(tradeoff_tax_less_services_intensity >= 50),
    tradeoff_tax_sales_tax_strong = as.integer(tradeoff_tax_sales_tax_intensity >= 50),
    tradeoff_tax_inc_tax_strong = as.integer(tradeoff_tax_inc_tax_intensity >= 50),
    tradeoff_tax_wealth_tax_strong = as.integer(tradeoff_tax_wealth_tax_intensity >= 50),
    
    # hc
    tradeoff_hc_all_strong = as.integer(tradeoff_hc_all_intensity >= 50),
    tradeoff_hc_spend_strong = as.integer(tradeoff_hc_spend_intensity >= 50),
    tradeoff_hc_pensions_strong = as.integer(tradeoff_hc_pensions_intensity >= 50),
    
    # cc2
    tradeoff_cc2_all_strong = as.integer(tradeoff_cc2_all_intensity >= 50),
    tradeoff_cc2_low_inc_strong = as.integer(tradeoff_cc2_low_inc_intensity >= 50),
    tradeoff_cc2_educ_all_strong = as.integer(tradeoff_cc2_educ_all_intensity >= 50),
    tradeoff_cc2_educ_low_inc_strong = as.integer(tradeoff_cc2_educ_low_inc_intensity >= 50)
  )

# vérification


cat("=== VÉRIFICATION DES PRÉFÉRENCES FORTES (>= 50 points) ===\n\n")

cat("cc1:\n")
cat("Tax strong:", sum(DataClean$tradeoff_cc1_tax_strong, na.rm = TRUE), "\n")
cat("Cut strong:", sum(DataClean$tradeoff_cc1_cut_strong, na.rm = TRUE), "\n")
cat("Debt strong:", sum(DataClean$tradeoff_cc1_debt_strong, na.rm = TRUE), "\n")
cat("No spend strong:", sum(DataClean$tradeoff_cc1_no_spend_strong, na.rm = TRUE), "\n\n")

cat("ge:\n")
cat("Tax strong:", sum(DataClean$tradeoff_ge_tax_strong, na.rm = TRUE), "\n")
cat("Cut strong:", sum(DataClean$tradeoff_ge_cut_strong, na.rm = TRUE), "\n")
cat("Debt strong:", sum(DataClean$tradeoff_ge_debt_strong, na.rm = TRUE), "\n")
cat("No spend strong:", sum(DataClean$tradeoff_ge_no_spend_strong, na.rm = TRUE), "\n\n")

cat("tax:\n")
cat("Less services strong:", sum(DataClean$tradeoff_tax_less_services_strong, na.rm = TRUE), "\n")
cat("Sales tax strong:", sum(DataClean$tradeoff_tax_sales_tax_strong, na.rm = TRUE), "\n")
cat("Inc tax strong:", sum(DataClean$tradeoff_tax_inc_tax_strong, na.rm = TRUE), "\n")
cat("Wealth tax strong:", sum(DataClean$tradeoff_tax_wealth_tax_strong, na.rm = TRUE), "\n\n")

cat("hc:\n")
cat("All strong:", sum(DataClean$tradeoff_hc_all_strong, na.rm = TRUE), "\n")
cat("Spend strong:", sum(DataClean$tradeoff_hc_spend_strong, na.rm = TRUE), "\n")
cat("Pensions strong:", sum(DataClean$tradeoff_hc_pensions_strong, na.rm = TRUE), "\n\n")

cat("cc2:\n")
cat("All strong:", sum(DataClean$tradeoff_cc2_all_strong, na.rm = TRUE), "\n")
cat("Low inc strong:", sum(DataClean$tradeoff_cc2_low_inc_strong, na.rm = TRUE), "\n")
cat("Educ all strong:", sum(DataClean$tradeoff_cc2_educ_all_strong, na.rm = TRUE), "\n")
cat("Educ low inc strong:", sum(DataClean$tradeoff_cc2_educ_low_inc_strong, na.rm = TRUE), "\n")

# ============================================================
# VÉRIFICATIONS COMPLÈTES
# ============================================================

cat("\n========================================\n")
cat("VÉRIFICATIONS DES 3 TYPES DE VARIABLES\n")
cat("========================================\n\n")

# QUESTION CC1

##binaires, intensité et préférence forte

cat("--- BINAIRES (_bin) - Priorité #1 claire ---\n")
cat("tradeoff_cc1_tax_bin:\n")
print(table(DataClean$tradeoff_cc1_tax_bin, useNA = "ifany"))
cat("\ntradeoff_cc1_cut_bin:\n")
print(table(DataClean$tradeoff_cc1_cut_bin, useNA = "ifany"))
cat("\ntradeoff_cc1_debt_bin:\n")
print(table(DataClean$tradeoff_cc1_debt_bin, useNA = "ifany"))
cat("\ntradeoff_cc1_no_spend_bin:\n")
print(table(DataClean$tradeoff_cc1_no_spend_bin, useNA = "ifany"))

cat("\n--- INTENSITÉ (_intensity) - Écart par rapport à la moyenne ---\n")
print(summary(DataClean[, c("tradeoff_cc1_tax_intensity", "tradeoff_cc1_cut_intensity", 
                            "tradeoff_cc1_debt_intensity", "tradeoff_cc1_no_spend_intensity")]))

cat("\n--- PRÉFÉRENCES FORTES (_strong) - Intensité >= 50 points ---\n")
cat("Tax strong:", sum(DataClean$tradeoff_cc1_tax_strong, na.rm = TRUE), "personnes\n")
cat("Cut strong:", sum(DataClean$tradeoff_cc1_cut_strong, na.rm = TRUE), "personnes\n")
cat("Debt strong:", sum(DataClean$tradeoff_cc1_debt_strong, na.rm = TRUE), "personnes\n")
cat("No spend strong:", sum(DataClean$tradeoff_cc1_no_spend_strong, na.rm = TRUE), "personnes\n")

# ============================================================
# QUESTION GE (Green economy)


cat("--- BINAIRES (_bin) ---\n")
cat("tradeoff_ge_tax_bin:\n")
print(table(DataClean$tradeoff_ge_tax_bin, useNA = "ifany"))
cat("\ntradeoff_ge_cut_bin:\n")
print(table(DataClean$tradeoff_ge_cut_bin, useNA = "ifany"))
cat("\ntradeoff_ge_debt_bin:\n")
print(table(DataClean$tradeoff_ge_debt_bin, useNA = "ifany"))
cat("\ntradeoff_ge_no_spend_bin:\n")
print(table(DataClean$tradeoff_ge_no_spend_bin, useNA = "ifany"))

cat("\n--- INTENSITÉ (_intensity) ---\n")
print(summary(DataClean[, c("tradeoff_ge_tax_intensity", "tradeoff_ge_cut_intensity", 
                            "tradeoff_ge_debt_intensity", "tradeoff_ge_no_spend_intensity")]))

cat("\n--- PRÉFÉRENCES FORTES (_strong) ---\n")
cat("Tax strong:", sum(DataClean$tradeoff_ge_tax_strong, na.rm = TRUE), "personnes\n")
cat("Cut strong:", sum(DataClean$tradeoff_ge_cut_strong, na.rm = TRUE), "personnes\n")
cat("Debt strong:", sum(DataClean$tradeoff_ge_debt_strong, na.rm = TRUE), "personnes\n")
cat("No spend strong:", sum(DataClean$tradeoff_ge_no_spend_strong, na.rm = TRUE), "personnes\n")

# ============================================================
# QUESTION TAX


cat("--- BINAIRES (_bin) ---\n")
cat("tradeoff_tax_less_services_bin:\n")
print(table(DataClean$tradeoff_tax_less_services_bin, useNA = "ifany"))
cat("\ntradeoff_tax_sales_tax_bin:\n")
print(table(DataClean$tradeoff_tax_sales_tax_bin, useNA = "ifany"))
cat("\ntradeoff_tax_inc_tax_bin:\n")
print(table(DataClean$tradeoff_tax_inc_tax_bin, useNA = "ifany"))
cat("\ntradeoff_tax_wealth_tax_bin:\n")
print(table(DataClean$tradeoff_tax_wealth_tax_bin, useNA = "ifany"))

cat("\n--- INTENSITÉ (_intensity) ---\n")
print(summary(DataClean[, c("tradeoff_tax_less_services_intensity", "tradeoff_tax_sales_tax_intensity", 
                            "tradeoff_tax_inc_tax_intensity", "tradeoff_tax_wealth_tax_intensity")]))

cat("\n--- PRÉFÉRENCES FORTES (_strong) ---\n")
cat("Less services strong:", sum(DataClean$tradeoff_tax_less_services_strong, na.rm = TRUE), "personnes\n")
cat("Sales tax strong:", sum(DataClean$tradeoff_tax_sales_tax_strong, na.rm = TRUE), "personnes\n")
cat("Inc tax strong:", sum(DataClean$tradeoff_tax_inc_tax_strong, na.rm = TRUE), "personnes\n")
cat("Wealth tax strong:", sum(DataClean$tradeoff_tax_wealth_tax_strong, na.rm = TRUE), "personnes\n")

# ============================================================
# QUESTION HC

cat("--- BINAIRES (_bin) ---\n")
cat("tradeoff_hc_all_bin:\n")
print(table(DataClean$tradeoff_hc_all_bin, useNA = "ifany"))
cat("\ntradeoff_hc_spend_bin:\n")
print(table(DataClean$tradeoff_hc_spend_bin, useNA = "ifany"))
cat("\ntradeoff_hc_pensions_bin:\n")
print(table(DataClean$tradeoff_hc_pensions_bin, useNA = "ifany"))

cat("\n--- INTENSITÉ (_intensity) ---\n")
print(summary(DataClean[, c("tradeoff_hc_all_intensity", "tradeoff_hc_spend_intensity", 
                            "tradeoff_hc_pensions_intensity")]))

cat("\n--- PRÉFÉRENCES FORTES (_strong) ---\n")
cat("All strong:", sum(DataClean$tradeoff_hc_all_strong, na.rm = TRUE), "personnes\n")
cat("Spend strong:", sum(DataClean$tradeoff_hc_spend_strong, na.rm = TRUE), "personnes\n")
cat("Pensions strong:", sum(DataClean$tradeoff_hc_pensions_strong, na.rm = TRUE), "personnes\n")

# ============================================================
# QUESTION CC2


cat("--- BINAIRES (_bin) ---\n")
cat("tradeoff_cc2_all_bin:\n")
print(table(DataClean$tradeoff_cc2_all_bin, useNA = "ifany"))
cat("\ntradeoff_cc2_low_inc_bin:\n")
print(table(DataClean$tradeoff_cc2_low_inc_bin, useNA = "ifany"))
cat("\ntradeoff_cc2_educ_all_bin:\n")
print(table(DataClean$tradeoff_cc2_educ_all_bin, useNA = "ifany"))
cat("\ntradeoff_cc2_educ_low_inc_bin:\n")
print(table(DataClean$tradeoff_cc2_educ_low_inc_bin, useNA = "ifany"))

cat("\n--- INTENSITÉ (_intensity) ---\n")
print(summary(DataClean[, c("tradeoff_cc2_all_intensity", "tradeoff_cc2_low_inc_intensity", 
                            "tradeoff_cc2_educ_all_intensity", "tradeoff_cc2_educ_low_inc_intensity")]))

cat("\n--- PRÉFÉRENCES FORTES (_strong) ---\n")
cat("All strong:", sum(DataClean$tradeoff_cc2_all_strong, na.rm = TRUE), "personnes\n")
cat("Low inc strong:", sum(DataClean$tradeoff_cc2_low_inc_strong, na.rm = TRUE), "personnes\n")
cat("Educ all strong:", sum(DataClean$tradeoff_cc2_educ_all_strong, na.rm = TRUE), "personnes\n")
cat("Educ low inc strong:", sum(DataClean$tradeoff_cc2_educ_low_inc_strong, na.rm = TRUE), "personnes\n")

# ============================================================
# RÉSUMÉ GLOBAL


cat("Total de variables créées par question:\n")
cat("- CC1: ", length(grep("^tradeoff_cc1_.*_(bin|intensity|strong)$", names(DataClean))), " variables\n")
cat("- GE: ", length(grep("^tradeoff_ge_.*_(bin|intensity|strong)$", names(DataClean))), " variables\n")
cat("- TAX: ", length(grep("^tradeoff_tax_.*_(bin|intensity|strong)$", names(DataClean))), " variables\n")
cat("- HC: ", length(grep("^tradeoff_hc_.*_(bin|intensity|strong)$", names(DataClean))), " variables\n")
cat("- CC2: ", length(grep("^tradeoff_cc2_.*_(bin|intensity|strong)$", names(DataClean))), " variables\n")


##To what extent do you agree with the following statement: The government should increase spending on childcare.
#table(clean$tradeoff_invest_cc0)
#
#DataClean$tradeoff_childcare_num <- NA_real_
#DataClean$tradeoff_childcare_num[clean$tradeoff_invest_cc0 == "Strongly agree"] <- 1
#DataClean$tradeoff_childcare_num[clean$tradeoff_invest_cc0 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_childcare_num[clean$tradeoff_invest_cc0 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_childcare_num[clean$tradeoff_invest_cc0 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_childcare_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_childcare_num_bin <- ifelse(
#  DataClean$tradeoff_childcare_num %in% c(0.66, 1), 1, 0
#)
#
#To what extent do you agree with the following statement: The government should increase spending on childcare, even if that implies higher taxes.
#table(clean$tradeoff_invest_cc1)      
#
#DataClean$tradeoff_childcare_higher_taxes_num <- NA_real_
#DataClean$tradeoff_childcare_higher_taxes_num[clean$tradeoff_invest_cc1 == "Strongly agree"] <- 1
#DataClean$tradeoff_childcare_higher_taxes_num[clean$tradeoff_invest_cc1 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_childcare_higher_taxes_num[clean$tradeoff_invest_cc1 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_childcare_higher_taxes_num[clean$tradeoff_invest_cc1 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_childcare_higher_taxes_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_childcare_higher_taxes_bin <- ifelse(
#  DataClean$tradeoff_childcare_higher_taxes_num %in% c(0.66, 1), 1, 0
#)
#
##To what extent do you agree with the following statement: The government should increase spending on childcare, even if that implies cutting back in other areas.
#table(clean$tradeoff_childcare_by_cutting_num)      
#
#DataClean$tradeoff_childcare_by_cutting_num <- NA_real_
#DataClean$tradeoff_childcare_by_cutting_num[clean$tradeoff_invest_cc2 == "Strongly agree"] <- 1
#DataClean$tradeoff_childcare_by_cutting_num[clean$tradeoff_invest_cc2 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_childcare_by_cutting_num[clean$tradeoff_invest_cc2 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_childcare_by_cutting_num[clean$tradeoff_invest_cc2 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_childcare_by_cutting_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.75 ou 1), 0 = le reste
#DataClean$tradeoff_childcare_by_cutting_bin <- ifelse(
#  DataClean$tradeoff_childcare_by_cutting_num %in% c(0.66, 1), 1, 0
#)
#
##To what extent do you agree with the following statement: The government should increase spending on childcare, even if that implies a higher public debt.
#table(clean$tradeoff_childcare_debt_num)      
#
#DataClean$tradeoff_childcare_debt_num <- NA_real_
#DataClean$tradeoff_childcare_debt_num[clean$tradeoff_invest_cc3 == "Strongly agree"] <- 1
#DataClean$tradeoff_childcare_debt_num[clean$tradeoff_invest_cc3 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_childcare_debt_num[clean$tradeoff_invest_cc3 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_childcare_debt_num[clean$tradeoff_invest_cc3 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_childcare_debt_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_childcare_debt_bin <- ifelse(
#  DataClean$tradeoff_childcare_debt_num %in% c(0.66, 1), 1, 0
#)
#
##To what extent do you agree with the following statement:  Taxes are already high. The government should not collect more money from citizens through taxes.
#table(clean$tradeoff_taxconst_0)
#
#DataClean$tradeoff_no_taxes_num <- NA_real_
#DataClean$tradeoff_no_taxes_num[clean$tradeoff_taxconst_0 == "Strongly agree"] <- 1
#DataClean$tradeoff_no_taxes_num[clean$tradeoff_taxconst_0 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_no_taxes_num[clean$tradeoff_taxconst_0 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_no_taxes_num[clean$tradeoff_taxconst_0 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_no_taxes_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_no_taxes_bin <- ifelse(
#  DataClean$tradeoff_no_taxes_num %in% c(0.66, 1), 1, 0
#)
#
##To what extent do you agree with the following statement:  Taxes are already high. The government should not collect more money from citizens unless it is targeted at all citizens, through an increase in sales taxes. (Note: a tax added to the price of goods or services at the time of purchase).
#table(clean$tradeoff_taxconst_1)
#
#DataClean$tradeoff_taxes_sales_num <- NA_real_
#DataClean$tradeoff_taxes_sales_num[clean$tradeoff_taxconst_1 == "Strongly agree"] <- 1
#DataClean$tradeoff_taxes_sales_num[clean$tradeoff_taxconst_1 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_taxes_sales_num[clean$tradeoff_taxconst_1 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_taxes_sales_num[clean$tradeoff_taxconst_1 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_taxes_sales_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_taxes_sales_bin <- ifelse(
#  DataClean$tradeoff_taxes_sales_num %in% c(0.66, 1), 1, 0
#)
#
##To what extent do you agree with the following statement: Taxes are already high. The government should not collect more money from citizens unless it is targeted at high income citizens, like a tax on high incomes.
#table(clean$tradeoff_taxconst_2)
#

#DataClean$tradeoff_taxes_high_income_num <- NA_real_
#DataClean$tradeoff_taxes_high_income_num[clean$tradeoff_taxconst_2 == "Strongly agree"] <- 1
#DataClean$tradeoff_taxes_high_income_num[clean$tradeoff_taxconst_2 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_taxes_high_income_num[clean$tradeoff_taxconst_2 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_taxes_high_income_num[clean$tradeoff_taxconst_2 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_taxes_high_income_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_taxes_high_income_bin <- ifelse(
#  DataClean$tradeoff_taxes_high_income_num %in% c(0.66, 1), 1, 0
#)
#
##To what extent do you agree with the following statement:  Taxes are already high. The government should not collect more money from citizens unless it is targeted at wealthy citizens, like a capital gains tax. (Note: a tax you pay when you make money from selling something valuable for more than you paid for it, like property or stocks.)
#table(clean$tradeoff_taxconst_3)
#
#DataClean$tradeoff_taxes_wealthy_num <- NA_real_
#DataClean$tradeoff_taxes_wealthy_num[clean$tradeoff_taxconst_3 == "Strongly agree"] <- 1
#DataClean$tradeoff_taxes_wealthy_num[clean$tradeoff_taxconst_3 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_taxes_wealthy_num[clean$tradeoff_taxconst_3 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_taxes_wealthy_num[clean$tradeoff_taxconst_3 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_taxes_wealthy_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_taxes_wealthy_bin <- ifelse(
#  DataClean$tradeoff_taxes_wealthy_num %in% c(0.66, 1), 1, 0
#)
#
##In the question you just answered, which taxation policy was mentioned as a possible exception to the statement that the government should not collect more money from citizens?
#table(clean$tradeoff_taxconst_m)
#
#DataClean$tradeoff_taxconst_char <- NA
#DataClean$tradeoff_taxconst_char[clean$tradeoff_taxconst_m == "A capital gains tax."]   <- "A capital gains tax."
#DataClean$tradeoff_taxconst_char[clean$tradeoff_taxconst_m == "An increase in sales taxes."]  <- "An increase in sales taxes."
#DataClean$tradeoff_taxconst_char[clean$tradeoff_taxconst_m == "A tax on high incomes."]  <- "A tax on high incomes."
#DataClean$tradeoff_taxconst_char[clean$tradeoff_taxconst_m == "No exception was mentioned."]   <- "No exception was mentioned."
#DataClean$tradeoff_taxconst_char[clean$tradeoff_taxconst_m == "I don’t remember."]   <- "I don’t remember."
#
#table(DataClean$tradeoff_taxconst_char)
#
##binaires
#DataClean$attention_tradeoff_capital_bin <- ifelse(DataClean$tradeoff_taxconst_char == "A capital gains tax.", 1, 0)
#DataClean$attention_tradeoff_sales_bin <- ifelse(DataClean$tradeoff_taxconst_char == "An increase in sales taxes.", 1, 0)
#DataClean$attention_tradeoff_income_bin <- ifelse(DataClean$tradeoff_taxconst_char == "A tax on high incomes.", 1, 0)
#DataClean$attention_tradeoff_no_exception_bin <- ifelse(DataClean$tradeoff_taxconst_char == "No exception was mentioned.", 1, 0)
#DataClean$attention_tradeoff_dontremember_bin <- ifelse(DataClean$tradeoff_taxconst_char == "I don’t remember.", 1, 0)
#table(DataClean$attention_tradeoff_capital_bin)
#
#Please imagine that the government wants to improve certain social benefits. However, it can only do so by cutting back on other social benefits. To what extent do you find the following cutbacks acceptable in comparison to the improvement they allow? The government subsidizes child care for all families at a cost of lowering other family benefits (e.g., child tax credits or parental leave payments).
#table(clean$tradeoff_spend_cc_1)
#
#DataClean$tradeoff_childcare_benefits_num <- NA_real_
#DataClean$tradeoff_childcare_benefits_num[clean$tradeoff_spend_cc_1 == "Strongly agree"] <- 1
#DataClean$tradeoff_childcare_benefits_num[clean$tradeoff_spend_cc_1 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_childcare_benefits_num[clean$tradeoff_spend_cc_1 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_childcare_benefits_num[clean$tradeoff_spend_cc_1 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_childcare_benefits_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_childcare_benefits_bin <- ifelse(
#  DataClean$tradeoff_childcare_benefits_num %in% c(0.66, 1), 1, 0
#)
#
##Please imagine that the government wants to improve certain social benefits. However, it can only do so by cutting back on other social benefits. To what extent do you find the following cutbacks acceptable in comparison to the improvement they allow? The government subsidizes childcare for low-income families at a cost of increasing the price of childcare for middle and upper-class families.
#table(clean$tradeoff_spend_cc_2)
#
#DataClean$tradeoff_childcare_lowincome_num <- NA_real_
#DataClean$tradeoff_childcare_lowincome_num[clean$tradeoff_spend_cc_2 == "Strongly agree"] <- 1
#DataClean$tradeoff_childcare_lowincome_num[clean$tradeoff_spend_cc_2 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_childcare_lowincome_num[clean$tradeoff_spend_cc_2 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_childcare_lowincome_num[clean$tradeoff_spend_cc_2 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_childcare_lowincome_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_childcare_lowincome_bin <- ifelse(
#  DataClean$tradeoff_childcare_lowincome_num %in% c(0.66, 1), 1, 0
#)
#
##In the question  you just answered, what was mentioned as the trade-off for increasing childcare subsidies?
#table(clean$tradeoff_spend_cc_m)
#DataClean$tradeoff_childcare_char <- NA
#DataClean$tradeoff_childcare_char[clean$tradeoff_spend_cc_m == "A reduction in other family benefits (e.g., child tax credits or parental leave payments)."]   <- "A reduction in other family benefits (e.g., child tax credits or parental leave payments)."
#DataClean$tradeoff_childcare_char[clean$tradeoff_spend_cc_m == "An increase in the price of childcare for middle- and upper-class families."]  <- "An increase in the price of childcare for middle- and upper-class families."
#DataClean$tradeoff_childcare_char[clean$tradeoff_spend_cc_m == "I don’t remember."]  <- "I don’t remember."
#DataClean$tradeoff_childcare_char[clean$tradeoff_spend_cc_m == "No trade-off was mentioned."]   <- "No trade-off was mentioned."
#
#table(DataClean$tradeoff_childcare_char)
#
##binaires
#DataClean$attention_tradeoff_childcare_benefits_bin <- ifelse(DataClean$tradeoff_childcare_char == "A reduction in other family benefits (e.g., child tax credits or parental leave payments).", 1, 0)
#DataClean$attention_tradeoff_childcare_income_bin <- ifelse(DataClean$tradeoff_childcare_char == "An increase in the price of childcare for middle- and upper-class families.", 1, 0)
#DataClean$attention_tradeoff_childcare_dontremember_bin <- ifelse(DataClean$tradeoff_childcare_char == "I don’t remember.", 1, 0)
#DataClean$attention_tradeoff_childcare_no_mention_bin <- ifelse(DataClean$tradeoff_childcare_char == "No trade-off was mentioned.", 1, 0)
#table(DataClean$attention_tradeoff_childcare_benefits_bin)
#
##Please imagine that the government wants to improve certain social benefits. However, it can only do so by cutting back on other social benefits. To what extent do you find the following cutbacks acceptable in comparison to the improvement they allow? The government increases home care for all seniors at a cost of lowering maximum old age pension benefits.
#table(clean$tradeoff_spend_hc_1)
#
#DataClean$tradeoff_senior_benefits_num <- NA_real_
#DataClean$tradeoff_senior_benefits_num[clean$tradeoff_spend_hc_1 == "Strongly agree"] <- 1
#DataClean$tradeoff_senior_benefits_num[clean$tradeoff_spend_hc_1 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_senior_benefits_num[clean$tradeoff_spend_hc_1 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_senior_benefits_num[clean$tradeoff_spend_hc_1 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_senior_benefits_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_senior_benefits_bin <- ifelse(
#  DataClean$tradeoff_senior_benefits_num %in% c(0.66, 1), 1, 0
#)
#
##Please imagine that the government wants to improve certain social benefits. However, it can only do so by cutting back on other social benefits. To what extent do you find the following cutbacks acceptable in comparison to the improvement they allow? The government increases home care for low-income seniors at a cost of increasing the price of home care for middle and upper-class seniors.
#table(clean$tradeoff_spend_hc_2)
#
#DataClean$tradeoff_senior_income_num <- NA_real_
#DataClean$tradeoff_senior_income_num[clean$tradeoff_spend_hc_2 == "Strongly agree"] <- 1
#DataClean$tradeoff_senior_income_num[clean$tradeoff_spend_hc_2 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_senior_income_num[clean$tradeoff_spend_hc_2 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_senior_income_num[clean$tradeoff_spend_hc_2 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_senior_income_num)
#
## Création de la variable binaire : 1 = priorité forte à la dette (0.66 ou 1), 0 = le reste
#DataClean$tradeoff_senior_income_bin <- ifelse(
#  DataClean$tradeoff_senior_income_num %in% c(0.66, 1), 1, 0
#)

# ============================================================
#REDISTRIBUTION


# redis_fei_can1: most = 1, never = 0 
table(clean$redis_fei_can1)

DataClean$redis_intelligence_num <- NA_real_
DataClean$redis_intelligence_num[clean$redis_fei_can1 == "Most of the time"] <- 1
DataClean$redis_intelligence_num[clean$redis_fei_can1 == "Sometimes"] <- 0.66
DataClean$redis_intelligence_num[clean$redis_fei_can1 == "Rarely"] <- 0.33
DataClean$redis_intelligence_num[clean$redis_fei_can1 == "Never"] <- 0
table(DataClean$redis_intelligence_num)

DataClean$redis_intelligence_bin <- ifelse(
  DataClean$redis_intelligence_num %in% c(0.66, 1), 1, 0
)

# redis_fei_can2.: most = 1, never = 0 
table(clean$redis_fei_can2.)

DataClean$redis_opportunity_num <- NA_real_
DataClean$redis_opportunity_num[clean$redis_fei_can2. == "Most of the time"] <- 1
DataClean$redis_opportunity_num[clean$redis_fei_can2. == "Sometimes"] <- 0.66
DataClean$redis_opportunity_num[clean$redis_fei_can2. == "Rarely"] <- 0.33
DataClean$redis_opportunity_num[clean$redis_fei_can2. == "Never"] <- 0
table(DataClean$redis_opportunity_num)

DataClean$redis_opportunity_bin <- ifelse(
  DataClean$redis_opportunity_num %in% c(0.66, 1), 1, 0
)

# redis_pnvo_rich: most = 0, never = 1 ⚠ INVERSÉ
table(clean$redis_pnvo_rich)

DataClean$redis_reasons_rich_num <- NA_real_
DataClean$redis_reasons_rich_num[clean$redis_pnvo_rich == "Most of the time"] <- 0  # CHANGÉ de 1 à 0
DataClean$redis_reasons_rich_num[clean$redis_pnvo_rich == "Sometimes"] <- 0.33      # CHANGÉ de 0.66 à 0.33
DataClean$redis_reasons_rich_num[clean$redis_pnvo_rich == "Rarely"] <- 0.66         # CHANGÉ de 0.33 à 0.66
DataClean$redis_reasons_rich_num[clean$redis_pnvo_rich == "Never"] <- 1             # CHANGÉ de 0 à 1
table(DataClean$redis_reasons_rich_num)

DataClean$redis_reasons_rich_bin <- ifelse(
  DataClean$redis_reasons_rich_num %in% c(0.66, 1), 1, 0
)

# redis_pnvo_poor.: most = 0, never = 1 ⚠ INVERSÉ
table(clean$redis_pnvo_poor.)

DataClean$redis_reasons_poor_num <- NA_real_
DataClean$redis_reasons_poor_num[clean$redis_pnvo_poor. == "Most of the time"] <- 0  # CHANGÉ de 1 à 0
DataClean$redis_reasons_poor_num[clean$redis_pnvo_poor. == "Sometimes"] <- 0.33      # CHANGÉ de 0.66 à 0.33
DataClean$redis_reasons_poor_num[clean$redis_pnvo_poor. == "Rarely"] <- 0.66         # CHANGÉ de 0.33 à 0.66
DataClean$redis_reasons_poor_num[clean$redis_pnvo_poor. == "Never"] <- 1             # CHANGÉ de 0 à 1
table(DataClean$redis_reasons_poor_num)

DataClean$redis_reasons_poor_bin <- ifelse(
  DataClean$redis_reasons_poor_num %in% c(0.66, 1), 1, 0
)

# redis_fid_can: most = 1, never = 0 
table(clean$redis_fid_can)

DataClean$redis_effort_num <- NA_real_
DataClean$redis_effort_num[clean$redis_fid_can == "Most of the time"] <- 1
DataClean$redis_effort_num[clean$redis_fid_can == "Sometimes"] <- 0.66
DataClean$redis_effort_num[clean$redis_fid_can == "Rarely"] <- 0.33
DataClean$redis_effort_num[clean$redis_fid_can == "Never"] <- 0
table(DataClean$redis_effort_num)

DataClean$redis_effort_bin <- ifelse(
  DataClean$redis_effort_num %in% c(0.66, 1), 1, 0
)

# redis_prev_freerider: most = 1, never = 0 
table(clean$redis_prev_freerider)

DataClean$redis_social_benefits_num <- NA_real_
DataClean$redis_social_benefits_num[clean$redis_prev_freerider == "Most of the time"] <- 1
DataClean$redis_social_benefits_num[clean$redis_prev_freerider == "Sometimes"] <- 0.66
DataClean$redis_social_benefits_num[clean$redis_prev_freerider == "Rarely"] <- 0.33
DataClean$redis_social_benefits_num[clean$redis_prev_freerider == "Never"] <- 0
table(DataClean$redis_social_benefits_num)

DataClean$redis_social_benefits_bin <- ifelse(
  DataClean$redis_social_benefits_num %in% c(0.66, 1), 1, 0
)

# redis_iden_freerider: most = 0, never = 1 ⚠ INVERSÉ
table(clean$redis_iden_freerider)

DataClean$redis_welfare_num <- NA_real_
DataClean$redis_welfare_num[clean$redis_iden_freerider == "Most of the time"] <- 0   # CHANGÉ de 1 à 0
DataClean$redis_welfare_num[clean$redis_iden_freerider == "Sometimes"] <- 0.33       # CHANGÉ de 0.66 à 0.33
DataClean$redis_welfare_num[clean$redis_iden_freerider == "Rarely"] <- 0.66          # CHANGÉ de 0.33 à 0.66
DataClean$redis_welfare_num[clean$redis_iden_freerider == "Never"] <- 1              # CHANGÉ de 0 à 1
table(DataClean$redis_welfare_num)

DataClean$redis_welfare_bin <- ifelse(
  DataClean$redis_welfare_num %in% c(0.66, 1), 1, 0
)

# redis_human_nature.: most = 1, never = 0 
table(clean$redis_human_nature.)

DataClean$redis_no_cheat_system_num <- NA_real_
DataClean$redis_no_cheat_system_num[clean$redis_human_nature. == "Most of the time"] <- 1
DataClean$redis_no_cheat_system_num[clean$redis_human_nature. == "Sometimes"] <- 0.66
DataClean$redis_no_cheat_system_num[clean$redis_human_nature. == "Rarely"] <- 0.33
DataClean$redis_no_cheat_system_num[clean$redis_human_nature. == "Never"] <- 0
table(DataClean$redis_no_cheat_system_num)

DataClean$redis_no_cheat_system_bin <- ifelse(
  DataClean$redis_no_cheat_system_num %in% c(0.66, 1), 1, 0
)


#------------------------------------------------------------------------------------
#ideo_vote_fed_clean
#1deo_vote_prov_clean

#--------------------------------------------------------------------------------------
#trust

# Generally speaking would you say that most people can be trusted, or, that you need to be very careful when dealing with people? 
# On a scale of 0 to 10 where 0 means no trust and 10 means complete trust.

table(clean$trust_social_clean)

# Convertir en numérique
DataClean$trust_social_num <- as.numeric(clean$trust_social_clean)

# Créer variable binaire: 1 = confiance élevée (6-10), 0 = confiance faible (0-5)
DataClean$trust_social_bin <- ifelse(
  DataClean$trust_social_num >= 6, 1, 
  ifelse(DataClean$trust_social_num <= 5, 0, NA)
)

# Vérification
cat("Variable numérique (0-10):\n")
table(DataClean$trust_social_num, useNA = "ifany")

cat("\nVariable binaire (0 = faible confiance, 1 = haute confiance):\n")
table(DataClean$trust_social_bin, useNA = "ifany")


##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - Political parties

table(clean$trust_inst_pp)

# Convertir en numérique
DataClean$trust_inst_pp_num <- as.numeric(clean$trust_inst_pp)

# Créer variable binaire: 1 = confiance élevée (6-10), 0 = confiance faible (0-5)
DataClean$trust_inst_pp_bin <- ifelse(
  DataClean$trust_inst_pp_num >= 6, 1, 
  ifelse(DataClean$trust_inst_pp_num <= 5, 0, NA)
)

# Vérification
cat("Variable numérique (0-10):\n")
table(DataClean$trust_inst_pp_num, useNA = "ifany")

cat("\nVariable binaire (0 = faible confiance, 1 = haute confiance):\n")
table(DataClean$trust_inst_pp_bin, useNA = "ifany")


##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - The federal government
# Trust in federal government
# On a scale of 0 to 10 where 0 means no trust and 10 means complete trust.

table(clean$trust_inst_fed)

# Convertir en numérique
DataClean$trust_inst_fed_num <- as.numeric(clean$trust_inst_fed)

# Créer variable binaire: 1 = confiance élevée (6-10), 0 = confiance faible (0-5)
DataClean$trust_inst_fed_bin <- ifelse(
  DataClean$trust_inst_fed_num >= 6, 1, 
  ifelse(DataClean$trust_inst_fed_num <= 5, 0, NA)
)

# Vérification
cat("trust_inst_fed - Variable numérique (0-10):\n")
table(DataClean$trust_inst_fed_num, useNA = "ifany")

cat("\ntrust_inst_fed - Variable binaire:\n")
table(DataClean$trust_inst_fed_bin, useNA = "ifany")




# Trust in provincial government
# On a scale of 0 to 10 where 0 means no trust and 10 means complete trust.

table(clean$trust_inst_prov)

# Convertir en numérique
DataClean$trust_inst_prov_num <- as.numeric(clean$trust_inst_prov)

# Créer variable binaire: 1 = confiance élevée (6-10), 0 = confiance faible (0-5)
DataClean$trust_inst_prov_bin <- ifelse(
  DataClean$trust_inst_prov_num >= 6, 1, 
  ifelse(DataClean$trust_inst_prov_num <= 5, 0, NA)
)

# Vérification
cat("trust_inst_prov - Variable numérique (0-10):\n")
table(DataClean$trust_inst_prov_num, useNA = "ifany")

cat("\ntrust_inst_prov - Variable binaire:\n")
table(DataClean$trust_inst_prov_bin, useNA = "ifany")

# Trust in media
# On a scale of 0 to 10 where 0 means no trust and 10 means complete trust.

table(clean$trust_inst_media)

# Convertir en numérique
DataClean$trust_inst_media_num <- as.numeric(clean$trust_inst_media)

# Créer variable binaire: 1 = confiance élevée (6-10), 0 = confiance faible (0-5)
DataClean$trust_inst_media_bin <- ifelse(
  DataClean$trust_inst_media_num >= 6, 1, 
  ifelse(DataClean$trust_inst_media_num <= 5, 0, NA)
)

# Vérification
cat("trust_inst_media - Variable numérique (0-10):\n")
table(DataClean$trust_inst_media_num, useNA = "ifany")

cat("\ntrust_inst_media - Variable binaire:\n")
table(DataClean$trust_inst_media_bin, useNA = "ifany")



write.csv(DataClean, "data/clean_df_full.csv")

