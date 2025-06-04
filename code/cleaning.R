# ===================================================
# Cleaning & Wrangling ACA
# ===================================================
# Version: June 2nd, 2025

# -----------------------
# 1. Load packages
# -----------------------
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(rlang)

# -----------------------
# 2. Load and inspect data
# -----------------------
#raw_data <- read.csv("data/ACA_April_30.csv") 
# -----------------------
# 2. Remove IPAddress 
# -----------------------
#raw_data <- raw_data %>%
#  select(-IPAddress, -RecipientLastName, -RecipientFirstName,
#         -RecipientEmail, -LocationLatitude, -LocationLongitude,
#         -DistributionChannel)
#
#write.csv(raw_data, "data/ACA_April_30.csv")

clean <- read.csv("data/ACA_April_30.csv") 
# -----------------------
# 3. Clean metadata rows
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

# -----------------------
# 5. Clean
# -----------------------
# Remove unnecessary columns from Qualtrics
# Filter for finished survey and consented to all
# Once removing those who did not finish, did not consent and did not correctly respond
# to the attention check: from n=634 to n=482

clean <- data %>%
 dplyr::select(-StartDate, -EndDate, -Status, -Progress, -RecordedDate,
 #        -ExternalReferencel
 )%>%
  dplyr::filter(
    Finished == "True",
    consent == "I have read the consent form and agree to participate in this survey.",
    consent_end_1 == "Yes",
    consent_end_2 == "Yes",
    attention == "Brown"
  )

clean <- clean %>%
 dplyr::select(-Finished, - consent, -consent_end_1, -consent_end_2)

# START AGAIN WITH NEW DATA (character not numeric)
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

# Step 2: Coalesce across all 'ideo_vote/define_*' columns (without _TEXT)
fed_vars <- names(clean)[grepl("^ideo_vote_fed_[A-Z]{2}$", names(clean))]
prov_vars <- names(clean)[grepl("^ideo_vote_prov_[A-Z]{2}$", names(clean))]
define_vars <- names(clean)[grepl("^ideo_define_[A-Z]{2}$", names(clean))]


# Step 3: Collapse while forcing character (robust against factor/empty)
clean$ideo_vote_fed_clean <- dplyr::coalesce(!!!lapply(fed_vars, function(x) {
  val <- as.character(clean[[x]])
  val[val == ""] <- NA  # Optional: treat empty strings as NA
  val
}))

clean$ideo_vote_prov_clean <- dplyr::coalesce(!!!lapply(prov_vars, function(x) {
  val <- as.character(clean[[x]])
  val[val == ""] <- NA
  val
}))

clean$ideo_define_clean <- dplyr::coalesce(!!!lapply(define_vars, function(x) {
  val <- as.character(clean[[x]])
  val[val == ""] <- NA
  val
}))

# Verify
unique(clean$ideo_vote_fed_clean)
unique(clean$ideo_vote_prov_clean)
unique(clean$ideo_define_clean)

# Drop all _TEXT and previous ideo_vote/define
clean <- clean %>%
  select(-matches("_TEXT$")) %>%
  select(-matches("^ideo_vote_"), -matches("^ideo_define_")) %>%
  select(matches("_clean$"), everything())

# Clean trust questions (Qualtrics issue, question not asked for msot)
n_obs <- clean %>% filter(!is.na(trust_institution_1)) %>% count()
print(n_obs) # 482 GOOD!

# Collapse
trust_social_vars <- names(clean)[grepl("^trust_social_", names(clean))]

clean <- clean %>%
  mutate(across(all_of(trust_social_vars), ~na_if(as.character(.x), ""))) %>%
  mutate(trust_social_clean = coalesce(!!!syms(trust_social_vars))) %>%
  select(-all_of(trust_social_vars))

# Inst_trust 
# Define the mapping of new variables to existing trust variables
trust_mapping <- list(
  trust_inst_pp    = c("trust_institution_1", "trust_institut_terr_1"),
  trust_inst_fed   = c("trust_institution_2", "trust_institut_terr_2"),
  trust_inst_prov  = c("trust_institution_3", "trust_institut_terr_3"),
  trust_inst_media = c("trust_institution_4", "trust_institut_terr_4"),
  trust_inst_ter   = c("trust_institution_5", "trust_institut_terr_5")
)

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
  select(-all_of(existing_trust_vars))

# -----------------------
# Rename variables and check for coding
# -----------------------
# Rename all variables according to codebook
-------------------------

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
attributes(clean$ses_year_born.)
table(clean$ses_year_born.)

# Calcul de l'âge basé sur l'année de naissance
DataClean$ses_age <- 2025 - as.numeric(clean$ses_year_born.)

# Création des catégories d'âge
DataClean$age34    <- ifelse(DataClean$ses_age >= 18 & DataClean$ses_age <= 34, 1, 0)
DataClean$age35_54 <- ifelse(DataClean$ses_age >= 35 & DataClean$ses_age <= 54, 1, 0)
#(55 plus becomes reference)



# Vérification
table(DataClean$ses_age)
table(DataClean$age35_54)

#In which province or territory are you currently living?-------------------------------------------------------------
attributes(clean$ses_region.)
table(clean$ses_region.)

DataClean$alberta_bin <- ifelse(clean$ses_region. == "Alberta", 1, 0)
DataClean$bcolumbia_bin <- ifelse(clean$ses_region. == "British Columbia", 1, 0)
DataClean$nova_scotia_bin <- ifelse(clean$ses_region. == "Nova Scotia", 1, 0)
DataClean$ontario_bin <- ifelse(clean$ses_region. == "Ontario", 1, 0)
DataClean$quebec_bin <- ifelse(clean$ses_region. == "Quebec", 1, 0)
DataClean$yukon_bin <- ifelse(clean$ses_region. == "Yukon", 1, 0)

table(DataClean$alberta_bin)
table(DataClean$quebec_bin)

#What language do you speak most often at home?-------------------------------------------------------------------------
attributes(clean$ses_language)
table(clean$ses_language)
DataClean$ses_french_bin <- ifelse(clean$ses_language == "French", 1, 0)
DataClean$ses_french_bin[clean$ses_language == "Other"] <- NA
table(DataClean$ses_french_bin)

#What is the highest level of education that you have completed?--------------------------------------------------------
attributes(clean$ses_education)
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

# Création des variables binaires avec High comme référence
DataClean$incomeLow_bin <- ifelse(DataClean$ses_income3Cat == "Low", 1, 0)
DataClean$incomeMid_bin <- ifelse(DataClean$ses_income3Cat == "Mid", 1, 0)
# Pas de variable pour "High" → référence implicite

# Vérification
table(DataClean$ses_income3Cat, useNA = "always")
table(DataClean$incomeLow_bin, useNA = "always")
table(DataClean$incomeMid_bin, useNA = "always")


#What are the first three characters of your postal code?-----------------------------------------------------
attributes(clean$ses_postal_code)
table(clean$ses_postal_code)
DataClean$ses_postalCode <- NA
DataClean$ses_postalCode <- clean$ses_postal_code
table(DataClean$ses_postalCode)

#What is your marital status?-------------------------------------------------------------------------------------
attributes(clean$ses_marital_status)
table(clean$ses_marital_status)
DataClean$ses_matStatus_char <- NA

DataClean$ses_matStatus_char[clean$ses_marital_status == "Single"]                  <- "single"
DataClean$ses_matStatus_char[clean$ses_marital_status == "Married"]                 <- "married"
DataClean$ses_matStatus_char[clean$ses_marital_status == "Common-law relationship"] <- "common_law"
DataClean$ses_matStatus_char[clean$ses_marital_status == "Widower/widow"]           <- "widower"
DataClean$ses_matStatus_char[clean$ses_marital_status == "Divorced/separated"]      <- "divorced"

# Création de la variable binaire : 1 = marié, 0 = autres
DataClean$matStatus_married_bin <- ifelse(DataClean$ses_matStatus_char == "married", 1, 0)

table(DataClean$matStatus_married_bin)

#Is the home you currently live in------------------------------------------------------------
attributes(clean$ses_home_ownership)
table(clean$ses_home_ownership)

DataClean$ses_home_ownership_char <- NA
DataClean$ses_home_ownership_char[clean$ses_home_ownership == "Owned by your family or a member of your household"] <- "owned_by_your_family_or_a_member_of_your_household"
DataClean$ses_home_ownership_char[clean$ses_home_ownership == "Rented"] <- "rented"

DataClean$home_owned_bin  <- ifelse(DataClean$ses_home_ownership_char == "owned_by_your_family_or_a_member_of_your_household", 1, 0)
DataClean$home_rented_bin <- ifelse(DataClean$ses_home_ownership_char == "rented", 1, 0)

table(DataClean$home_owned_bin)
table(DataClean$home_rented_bin)

#Do you live...----------------------------------------------------------------------
attributes(clean$ses_children.)
table(clean$ses_children.)

# Nettoyage de la variable enfants
DataClean$ses_children_char <- NA
DataClean$ses_children_char[clean$ses_children. == "Alone with children"]        <- "alone_with_children"
DataClean$ses_children_char[clean$ses_children. == "Alone without children"]     <- "alone_without_children"
DataClean$ses_children_char[clean$ses_children. == "In couple with children"]    <- "in_couple_with_children"
DataClean$ses_children_char[clean$ses_children. == "In couple without children"] <- "in_couple_without_children"

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

#What best describes your current employment status?----------------------------------------------------------
attributes(clean$ses_employ_status.)
table(clean$ses_employ_status.)

# Nettoyage de la variable de statut d'emploi
DataClean$ses_employ_status_char <- NA

DataClean$ses_employ_status_char[clean$ses_employ_status. == "A caregiver or homemaker"] <- "caregiver_or_homemaker"
DataClean$ses_employ_status_char[clean$ses_employ_status. == "A student attending school"] <- "student_attending_school"
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Not working due to illness/disability, or not looking for work"] <- "not_working_due_to_illness_disability_or_not_looking_for_work"
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Retired"] <- "retired"
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Seasonal work"] <- "seasonal_work"
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Self-employed"] <- "self_employed"
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Temporarily not working (e.g. parental leave, seasonal worker, in the process of changing jobs)"] <- "temporarily_not_working"
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Unemployed, and looking for work"] <- "unemployed"
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Working full-time (35 or more hours per week)"] <- "working_full_time"
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Working part-time (less than 35 hours per week)"] <- "working_part_time"

# Variable binaire : 1 = employé à temps plein, 0 = tout le reste
DataClean$employ_fulltime_bin <- ifelse(DataClean$ses_employ_status_char == "working_full_time", 1, 0)

# Vérification
table(DataClean$employ_fulltime_bin, useNA = "always")


#Which option best describes your current family structure-----------------------------------------------------------------
table(clean$ses_family_structure)

# Nettoyage de la variable sur la structure familiale
DataClean$ses_family_structure_char <- NA
DataClean$ses_family_structure_char[clean$ses_family_structure == "Single parent"] <- "single_parent"
DataClean$ses_family_structure_char[clean$ses_family_structure == "Two parents in a relationship but living separately"] <- "separate_parents"
DataClean$ses_family_structure_char[clean$ses_family_structure == "Two parents in a relationship in a first union (marriage or common-law)"] <- "first_union"
DataClean$ses_family_structure_char[clean$ses_family_structure == "Two parents in a relationship in a second union (marriage or common-law)"] <- "second_union"
DataClean$ses_family_structure_char[clean$ses_family_structure == "Other/Prefer not to answer"] <- "other"

# Variable binaire : 1 = première union, 0 = le reste
DataClean$family_first_union_bin <- ifelse(DataClean$ses_family_structure_char == "first_union", 1, 0)

# Vérification
table(DataClean$family_first_union_bin, useNA = "always")


#How many children do you have in each of the following age groups currently living with you in your household? - Ages 0 to 5--------------
table(clean$ses_household_compo._1)

# Nettoyage de la variable enfants 0-5 ans
DataClean$ses_children05_char <- NA
DataClean$ses_children05_char[clean$ses_household_compo._1 == 0] <- "0 enfant 0-5 ans"
DataClean$ses_children05_char[clean$ses_household_compo._1 == 1] <- "1 enfant 0-5 ans"
DataClean$ses_children05_char[clean$ses_household_compo._1 == 2] <- "2 enfants 0-5 ans"
DataClean$ses_children05_char[clean$ses_household_compo._1 == 3] <- "3 enfants 0-5 ans"

# Variable binaire : 1 = au moins 1 enfant 0-5 ans, 0 = aucun
DataClean$youngChildren <- ifelse(clean$ses_household_compo._1 > 0, 1, 0)

# Vérification
table(DataClean$youngChildren, useNA = "always")


#How many children do you have in each of the following age groups currently living with you in your household? - Ages 6-12-----------------
table(clean$ses_household_compo._2)

# Nettoyage de la variable enfants 6-12 ans
DataClean$ses_children612_char <- NA
DataClean$ses_children612_char[clean$ses_household_compo._2 == 0] <- "0 enfant 6-12 ans"
DataClean$ses_children612_char[clean$ses_household_compo._2 == 1] <- "1 enfant 6-12 ans"
DataClean$ses_children612_char[clean$ses_household_compo._2 == 2] <- "2 enfants 6-12 ans"
DataClean$ses_children612_char[clean$ses_household_compo._2 == 3] <- "3 enfants 6-12 ans"

# Variable binaire : 1 = au moins 1 enfant 6-12 ans, 0 = aucun
DataClean$dependentChildren <- ifelse(clean$ses_household_compo._2 > 0, 1, 0)

# Vérification
table(DataClean$dependentChildren, useNA = "always")


#How many children do you have in each of the following age groups currently living with you in your household? - Ages 13-17-----------------
table(clean$ses_household_compo._3)

# Nettoyage de la variable enfants 13-17 ans
DataClean$ses_children1317_char <- NA
DataClean$ses_children1317_char[clean$ses_household_compo._3 == 0] <- "0 enfant 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo._3 == 1] <- "1 enfant 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo._3 == 2] <- "2 enfants 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo._3 == 3] <- "3 enfants 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo._3 == 4] <- "4 enfants 13-17 ans"

# Variable binaire : 1 = au moins 1 enfant 13-17 ans, 0 = aucun
DataClean$teenChildren <- ifelse(clean$ses_household_compo._3 > 0, 1, 0)

# Vérification
table(DataClean$teenChildren, useNA = "always")


#How many children do you have in each of the following age groups currently living with you in your household? - Ages 18+------------------
table(clean$ses_household_compo._4)

DataClean$ses_children18_char <- NA
DataClean$ses_children18_char[clean$ses_household_compo._4 == 0] <- "0 enfant 18+ ans"
DataClean$ses_children18_char[clean$ses_household_compo._4 == 1] <- "1 enfant 18+ ans"
DataClean$ses_children18_char[clean$ses_household_compo._4 == 2] <- "2 enfants 18+ ans"
DataClean$ses_children18_char[clean$ses_household_compo._4 == 3] <- "3 enfants 18+ ans"
DataClean$ses_children18_char[clean$ses_household_compo._4 == 4] <- "4 enfants 18+ ans"
#binaires
DataClean$ses_0children18_bin <- ifelse(DataClean$ses_children18_char == "0 enfant 18+ ans", 1, 0)
DataClean$ses_1children18_bin <- ifelse(DataClean$ses_children18_char == "1 enfant 18+ ans", 1, 0)
DataClean$ses_2children18_bin <- ifelse(DataClean$ses_children18_char == "2 enfants 18+ ans", 1, 0)
DataClean$ses_3children18_bin <- ifelse(DataClean$ses_children18_char == "3 enfants 18+ ans", 1, 0)
DataClean$ses_4children18_bin <- ifelse(DataClean$ses_children18_char == "4 enfants 18+ ans", 1, 0)


# Vérification
table(DataClean$ses_3children18_bin)

#In politics, people sometimes talk of left and right. - Where would you place yourself on this scale with 0 being entirely to the left and 10 being entirely to the right?
table(clean$ideo_left_right_1)

DataClean$ideo_right_num. <- NA
DataClean$ideo_right_num.[clean$ideo_left_right_1 == 10] <- 1
DataClean$ideo_right_num.[clean$ideo_left_right_1 == 9] <- 0.88
DataClean$ideo_right_num.[clean$ideo_left_right_1 == 8] <- 0.77
DataClean$ideo_right_num.[clean$ideo_left_right_1 == 7] <- 0.66
DataClean$ideo_right_num.[clean$ideo_left_right_1 == 6] <- 0.55
DataClean$ideo_right_num.[clean$ideo_left_right_1 == 5] <- 0.44
DataClean$ideo_right_num.[clean$ideo_left_right_1 == 4] <- 0.33
DataClean$ideo_right_num.[clean$ideo_left_right_1 == 3] <- 0.22
DataClean$ideo_right_num.[clean$ideo_left_right_1 == 2] <- 0.11
DataClean$ideo_right_num.[clean$ideo_left_right_1 == 1] <- 0
table(DataClean$ideo_right_num.)

#How interested are you in politics generally? - Select a number from 0 to 10, where 0 means no interest at all, and 10 means a great deal of interest.
table(clean$ideo_interest_1)

DataClean$ideo_interest_politics_num <- NA
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 10] <- 1
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 9] <- 0.9
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 8] <- 0.8
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 7] <- 0.7
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 6] <- 0.6
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 5] <- 0.5
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 4] <- 0.4
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 3] <- 0.3
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 2] <- 0.2
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 1] <- 0.1
DataClean$ideo_interest_politics_num[clean$ideo_interest_1 == 0] <- 0
table(DataClean$ideo_interest_politics_num)

#Do you think the politicians are out merely for themselves, for their party, or to do their best for their country?
table(clean$ideo_cynicism)
DataClean$ideo_cynicism <- clean$ideo_cynicism

# Nettoyage de la variable ideo_cynicism
DataClean$ideo_cynicism_char <- NA
DataClean$ideo_cynicism_char[clean$ideo_cynicism == "Themselves"]   <- "themselves"
DataClean$ideo_cynicism_char[clean$ideo_cynicism == "Their party"]  <- "their_party"
DataClean$ideo_cynicism_char[clean$ideo_cynicism == "The country"]  <- "the_country"
DataClean$ideo_cynicism_char[clean$ideo_cynicism == "Don’t know"]   <- "dont_know"

# Variables binaires
DataClean$ideo_party_them <- ifelse(DataClean$ideo_cynicism_char == "themselves", 1, 0)
DataClean$ideo_party_bin    <- ifelse(DataClean$ideo_cynicism_char == "their_party", 1, 0)
DataClean$ideo_country_bin  <- ifelse(DataClean$ideo_cynicism_char == "the_country", 1, 0)

# Vérification
table(DataClean$ideo_party_them, useNA = "always")
table(DataClean$ideo_party_bin, useNA = "always")
table(DataClean$ideo_country_bin, useNA = "always")



#People have different ways of defining themselves. What do you consider yourself?
#NE FONCTIONNE PAS ENCORE
table(data$ideo_define_AL)
table(data$ideo_define_BC)
table(data$ideo_define_MA)
table(data$ideo_define_NB.)
table(data$ideo_define_NL)
table(data$ideo_define_NT)
table(data$ideo_define_NS)
table(data$ideo_define_NU)
table(data$ideo_define_ON)
table(data$ideo_define_PE)
table(data$ideo_define_YU)
table(data$ideo_define_QC)
table(data$ideo_define_SA)

# ALBERTA
data$ideo_define_AL_bin <- ifelse(data$ideo_define_AL == "First Albertan, second Canadian", 1, 0)

# BRITISH COLUMBIA
data$ideo_define_BC_bin <- ifelse(data$ideo_define_BC == "First British Columbian, second Canadian", 1, 0)

# MANITOBA
data$ideo_define_MA_bin <- 0  # Aucun répondant n’a cette réponse, donc tout est codé à 0

# NEW BRUNSWICK
data$ideo_define_NB_bin <- ifelse(data$ideo_define_NB. == "First New Brunswicker, second Canadian", 1, 0)

# NEWFOUNDLAND AND LABRADOR
data$ideo_define_NL_bin <- 0  # Aucun répondant n’a cette réponse, donc tout est codé à 0

# NORTHWEST TERRITORIES
data$ideo_define_NT_bin <- 0  # Aucun répondant avec "province d'abord", donc 0

# NOVA SCOTIA
data$ideo_define_NS_bin <- ifelse(data$ideo_define_NS == "First Nova Scotian, second Canadian", 1, 0)

# NUNAVUT
data$ideo_define_NU_bin <- 0  # Aucun répondant n’a cette réponse, donc 0

# ONTARIO
data$ideo_define_ON_bin <- ifelse(data$ideo_define_ON == "First Canadian, second Ontarian", 0,
                                  ifelse(data$ideo_define_ON == "Solely as Canadian", 0, 0))  # Pas de "First Ontarian", donc 0

# PRINCE EDWARD ISLAND
data$ideo_define_PE_bin <- 0  # Aucun répondant n’a cette réponse, donc 0

# YUKON
data$ideo_define_YU_bin <- ifelse(data$ideo_define_YU == "First Yukoner, second Canadian", 1, 0)

# QUEBEC
data$ideo_define_QC_bin <- ifelse(data$ideo_define_QC == "First Quebecer, second Canadian", 1, 0)

# SASKATCHEWAN
data$ideo_define_SA_bin <- 0  # Aucun répondant n’a cette réponse, donc 0

table(data$ideo_define_SA_bin)
table(data$ideo_define_QC_bin)
#############################################################################################################


#Now we would like to ask you about public finance questions. Please remember to read closely and pay attention. You will be asked questions to check your memory and comprehension. Please arrange the following policy issues by order of importance - Health
table(clean$budget_issue_imp_1)

DataClean$budget_health_priority_num <- NA_real_
DataClean$budget_health_priority_num[clean$budget_issue_imp_1 == 1] <- 1
DataClean$budget_health_priority_num[clean$budget_issue_imp_1 == 2] <- 0.75
DataClean$budget_health_priority_num[clean$budget_issue_imp_1 == 3] <- 0.50
DataClean$budget_health_priority_num[clean$budget_issue_imp_1 == 4] <- 0.25
DataClean$budget_health_priority_num[clean$budget_issue_imp_1 == 5] <- 0
table(DataClean$budget_health_priority_num)

# Création de la variable binaire : 1 = priorité forte à la santé (0.75 ou 1), 0 = le reste
DataClean$budget_health_priority_bin <- ifelse(
  DataClean$budget_health_priority_num %in% c(0.75, 1), 1, 0
)

# Vérification
table(DataClean$budget_health_priority_bin, useNA = "always")

#Now we would like to ask you about public finance questions. Please remember to read closely and pay attention. You will be asked questions to check your memory and comprehension. Please arrange the following policy issues by order of importance - Education
table(clean$budget_issue_imp_2)

DataClean$budget_education_priority_num <- NA_real_
DataClean$budget_education_priority_num[clean$budget_issue_imp_2 == 1] <- 1
DataClean$budget_education_priority_num[clean$budget_issue_imp_2 == 2] <- 0.75
DataClean$budget_education_priority_num[clean$budget_issue_imp_2 == 3] <- 0.50
DataClean$budget_education_priority_num[clean$budget_issue_imp_2 == 4] <- 0.25
DataClean$budget_education_priority_num[clean$budget_issue_imp_2 == 5] <- 0
table(DataClean$budget_education_priority_num)

# Création de la variable binaire : 1 = priorité forte à l'éducation (0.75 ou 1), 0 = le reste
DataClean$budget_education_priority_bin <- ifelse(
  DataClean$budget_education_priority_num %in% c(0.75, 1), 1, 0
)

# Vérification
table(DataClean$budget_education_priority_bin, useNA = "always")

#Now we would like to ask you about public finance questions. Please remember to read closely and pay attention. You will be asked questions to check your memory and comprehension. Please arrange the following policy issues by order of importance - Pensions
table(clean$budget_issue_imp_3)

DataClean$budget_pensions_priority_num <- NA_real_
DataClean$budget_pensions_priority_num[clean$budget_issue_imp_3 == 1] <- 1
DataClean$budget_pensions_priority_num[clean$budget_issue_imp_3 == 2] <- 0.75
DataClean$budget_pensions_priority_num[clean$budget_issue_imp_3 == 3] <- 0.50
DataClean$budget_pensions_priority_num[clean$budget_issue_imp_3 == 4] <- 0.25
DataClean$budget_pensions_priority_num[clean$budget_issue_imp_3 == 5] <- 0
table(DataClean$budget_pensions_priority_num)

# Création de la variable binaire : 1 = priorité forte aux pensions (0.75 ou 1), 0 = le reste
DataClean$budget_pensions_priority_bin <- ifelse(
  DataClean$budget_pensions_priority_num %in% c(0.75, 1), 1, 0
)

# Vérification
table(DataClean$budget_pensions_priority_bin, useNA = "always")

#Now we would like to ask you about public finance questions. Please remember to read closely and pay attention. You will be asked questions to check your memory and comprehension. Please arrange the following policy issues by order of importance - Taxes
table(clean$budget_issue_imp_4)

DataClean$budget_taxes_priority_num <- NA_real_
DataClean$budget_taxes_priority_num[clean$budget_issue_imp_4 == 1] <- 1
DataClean$budget_taxes_priority_num[clean$budget_issue_imp_4 == 2] <- 0.75
DataClean$budget_taxes_priority_num[clean$budget_issue_imp_4 == 3] <- 0.50
DataClean$budget_taxes_priority_num[clean$budget_issue_imp_4 == 4] <- 0.25
DataClean$budget_taxes_priority_num[clean$budget_issue_imp_4 == 5] <- 0
table(DataClean$budget_taxes_priority_num)

# Création de la variable binaire : 1 = priorité forte aux taxes (0.75 ou 1), 0 = le reste
DataClean$budget_taxes_priority_bin <- ifelse(
  DataClean$budget_taxes_priority_num %in% c(0.75, 1), 1, 0
)

# Vérification
table(DataClean$budget_taxes_priority_bin, useNA = "always")


#Now we would like to ask you about public finance questions. Please remember to read closely and pay attention. You will be asked questions to check your memory and comprehension. Please arrange the following policy issues by order of importance - Public debt
table(clean$budget_issue_imp_5)

DataClean$budget_debt_priority_num <- NA_real_
DataClean$budget_debt_priority_num[clean$budget_issue_imp_5 == 1] <- 1
DataClean$budget_debt_priority_num[clean$budget_issue_imp_5 == 2] <- 0.75
DataClean$budget_debt_priority_num[clean$budget_issue_imp_5 == 3] <- 0.50
DataClean$budget_debt_priority_num[clean$budget_issue_imp_5 == 4] <- 0.25
DataClean$budget_debt_priority_num[clean$budget_issue_imp_5 == 5] <- 0
table(DataClean$budget_debt_priority_num)

# Création de la variable binaire : 1 = priorité forte à la dette (0.75 ou 1), 0 = le reste
DataClean$budget_debt_priority_bin <- ifelse(
  DataClean$budget_debt_priority_num %in% c(0.75, 1), 1, 0
)

# Vérification
table(DataClean$budget_debt_priority_bin, useNA = "always")

#Imagine the government has the means to increase spending in some areas, but not all. Among the following policy areas, which are most important to you? You can allocate 100 points. Give more points to the areas you consider to be most important and less to those you consider to be less important. The government should: - increase access to healthcare
##healthcare

attributes(clean$budget_spend_prio._6)
class(clean$budget_spend_prio._6)
table(clean$budget_spend_prio._6, useNA = "always")

clean <- clean %>%
  mutate(
    budget_spend_prio_health_num = as.numeric(budget_spend_prio._6),
    budget_spend_prio_health_raw = budget_spend_prio_health_num / 100,
    budget_spend_prio_health_norm = case_when(
      is.na(budget_spend_prio_health_raw) ~ NA_real_,
      budget_spend_prio_health_raw <= 0.125 ~ 0.00,
      budget_spend_prio_health_raw <= 0.375 ~ 0.25,
      budget_spend_prio_health_raw <= 0.625 ~ 0.50,
      budget_spend_prio_health_raw <= 0.875 ~ 0.75,
      TRUE ~ 1.00
    )
  )

DataClean$budget_spend_prio_health_norm <- clean$budget_spend_prio_health_norm


table(DataClean$budget_spend_prio_health_norm, useNA = "always")


#Imagine the government has the means to increase spending in some areas, but not all. Among the following policy areas, which are most important to you? You can allocate 100 points. Give more points to the areas you consider to be most important and less to those you consider to be less important. The government should: - increase home care for seniors
##seniors
attributes(clean$budget_spend_prio._7)
class(clean$budget_spend_prio._7)
table(clean$budget_spend_prio._7, useNA = "always")

clean <- clean %>%
  mutate(
    budget_spend_prio_seniors_num = as.numeric(budget_spend_prio._7),
    budget_spend_prio_seniors_raw = budget_spend_prio_seniors_num / 100,
    budget_spend_prio_seniors_norm = case_when(
      is.na(budget_spend_prio_seniors_raw) ~ NA_real_,
      budget_spend_prio_seniors_raw <= 0.125 ~ 0.00,
      budget_spend_prio_seniors_raw <= 0.375 ~ 0.25,
      budget_spend_prio_seniors_raw <= 0.625 ~ 0.50,
      budget_spend_prio_seniors_raw <= 0.875 ~ 0.75,
      TRUE ~ 1.00
    )
  )

DataClean$budget_spend_prio_seniors_norm <- clean$budget_spend_prio_seniors_norm


table(DataClean$budget_spend_prio_seniors_norm, useNA = "always")


#Imagine the government has the means to increase spending in some areas, but not all. Among the following policy areas, which are most important to you? You can allocate 100 points. Give more points to the areas you consider to be most important and less to those you consider to be less important. The government should: - increase the availability of subsidized childcare
#childcare

attributes(clean$budget_spend_prio._8)
class(clean$budget_spend_prio._8)
table(clean$budget_spend_prio._8, useNA = "always")

clean <- clean %>%
  mutate(
    budget_spend_prio_childcare_num = as.numeric(budget_spend_prio._8),
    budget_spend_prio_childcare_raw = budget_spend_prio_childcare_num / 100,
    budget_spend_prio_childcare_norm = case_when(
      is.na(budget_spend_prio_childcare_raw) ~ NA_real_,
      budget_spend_prio_childcare_raw <= 0.125 ~ 0.00,
      budget_spend_prio_childcare_raw <= 0.375 ~ 0.25,
      budget_spend_prio_childcare_raw <= 0.625 ~ 0.50,
      budget_spend_prio_childcare_raw <= 0.875 ~ 0.75,
      TRUE ~ 1.00
    )
  )

DataClean$budget_spend_prio_childcare_norm <- clean$budget_spend_prio_childcare_norm


table(DataClean$budget_spend_prio_childcare_norm, useNA = "always")

#Imagine the government has the means to increase spending in some areas, but not all. Among the following policy areas, which are most important to you? You can allocate 100 points. Give more points to the areas you consider to be most important and less to those you consider to be less important. The government should: - improve the cost of living
##cost of living

attributes(clean$budget_spend_prio._9)
class(clean$budget_spend_prio._9)
table(clean$budget_spend_prio._9, useNA = "always")

clean <- clean %>%
  mutate(
    budget_spend_prio_costLiving_num = as.numeric(budget_spend_prio._9),
    budget_spend_prio_costLiving_raw = budget_spend_prio_costLiving_num / 100,
    budget_spend_prio_costLiving_norm = case_when(
      is.na(budget_spend_prio_costLiving_raw) ~ NA_real_,
      budget_spend_prio_costLiving_raw <= 0.125 ~ 0.00,
      budget_spend_prio_costLiving_raw <= 0.375 ~ 0.25,
      budget_spend_prio_costLiving_raw <= 0.625 ~ 0.50,
      budget_spend_prio_costLiving_raw <= 0.875 ~ 0.75,
      TRUE ~ 1.00
    )
  )

DataClean$budget_spend_prio_costLiving_norm <- clean$budget_spend_prio_costLiving_norm


table(DataClean$budget_spend_prio_costLiving_norm, useNA = "always")

#Imagine the government has the means to increase spending in some areas, but not all. Among the following policy areas, which are most important to you? You can allocate 100 points. Give more points to the areas you consider to be most important and less to those you consider to be less important. The government should: - invest in the fight against climate change
##climate change

attributes(clean$budget_spend_prio._10)
class(clean$budget_spend_prio._10)
table(clean$budget_spend_prio._10, useNA = "always")

clean <- clean %>%
  mutate(
    budget_spend_prio_climateChange_num = as.numeric(budget_spend_prio._10),
    budget_spend_prio_climateChange_raw = budget_spend_prio_climateChange_num / 100,
    budget_spend_prio_climateChange_norm = case_when(
      is.na(budget_spend_prio_climateChange_raw) ~ NA_real_,
      budget_spend_prio_climateChange_raw <= 0.125 ~ 0.00,
      budget_spend_prio_climateChange_raw <= 0.375 ~ 0.25,
      budget_spend_prio_climateChange_raw <= 0.625 ~ 0.50,
      budget_spend_prio_climateChange_raw <= 0.875 ~ 0.75,
      TRUE ~ 1.00
    )
  )

DataClean$budget_spend_prio_climateChange_norm <- clean$budget_spend_prio_climateChange_norm


table(DataClean$budget_spend_prio_climateChange_norm, useNA = "always")

#To what extent do you agree with the following statement: The government should increase spending on childcare.
table(clean$tradeoff_invest_cc0)

DataClean$tradeoff_childcare_num <- NA_real_
DataClean$tradeoff_childcare_num[clean$tradeoff_invest_cc0 == "Strongly agree"] <- 1
DataClean$tradeoff_childcare_num[clean$tradeoff_invest_cc0 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_childcare_num[clean$tradeoff_invest_cc0 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_childcare_num[clean$tradeoff_invest_cc0 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_childcare_num)

#To what extent do you agree with the following statement: The government should increase spending on childcare, even if that implies higher taxes.
table(clean$tradeoff_invest_cc1)      

DataClean$tradeoff_childcare_higher_taxes_num <- NA_real_
DataClean$tradeoff_childcare_higher_taxes_num[clean$tradeoff_invest_cc1 == "Strongly agree"] <- 1
DataClean$tradeoff_childcare_higher_taxes_num[clean$tradeoff_invest_cc1 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_childcare_higher_taxes_num[clean$tradeoff_invest_cc1 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_childcare_higher_taxes_num[clean$tradeoff_invest_cc1 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_childcare_higher_taxes_num)

#To what extent do you agree with the following statement: The government should increase spending on childcare, even if that implies cutting back in other areas.
table(clean$tradeoff_invest_cc2)      

DataClean$tradeoff_childcare_by_cutting_num <- NA_real_
DataClean$tradeoff_childcare_by_cutting_num[clean$tradeoff_invest_cc2 == "Strongly agree"] <- 1
DataClean$tradeoff_childcare_by_cutting_num[clean$tradeoff_invest_cc2 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_childcare_by_cutting_num[clean$tradeoff_invest_cc2 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_childcare_by_cutting_num[clean$tradeoff_invest_cc2 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_childcare_by_cutting_num)

#To what extent do you agree with the following statement: The government should increase spending on childcare, even if that implies a higher public debt.
table(clean$tradeoff_invest_cc3)      

DataClean$tradeoff_childcare_debt_num <- NA_real_
DataClean$tradeoff_childcare_debt_num[clean$tradeoff_invest_cc3 == "Strongly agree"] <- 1
DataClean$tradeoff_childcare_debt_num[clean$tradeoff_invest_cc3 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_childcare_debt_num[clean$tradeoff_invest_cc3 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_childcare_debt_num[clean$tradeoff_invest_cc3 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_childcare_debt_num)

#To what extent do you agree with the following statement:  Taxes are already high. The government should not collect more money from citizens through taxes.
table(clean$tradeoff_taxconst_0)

DataClean$tradeoff_no_taxes_num <- NA_real_
DataClean$tradeoff_no_taxes_num[clean$tradeoff_taxconst_0 == "Strongly agree"] <- 1
DataClean$tradeoff_no_taxes_num[clean$tradeoff_taxconst_0 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_no_taxes_num[clean$tradeoff_taxconst_0 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_no_taxes_num[clean$tradeoff_taxconst_0 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_no_taxes_num)

#To what extent do you agree with the following statement:  Taxes are already high. The government should not collect more money from citizens unless it is targeted at all citizens, through an increase in sales taxes. (Note: a tax added to the price of goods or services at the time of purchase).
table(clean$tradeoff_taxconst_1)

DataClean$tradeoff_taxes_sales_num <- NA_real_
DataClean$tradeoff_taxes_sales_num[clean$tradeoff_taxconst_1 == "Strongly agree"] <- 1
DataClean$tradeoff_taxes_sales_num[clean$tradeoff_taxconst_1 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_taxes_sales_num[clean$tradeoff_taxconst_1 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_taxes_sales_num[clean$tradeoff_taxconst_1 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_taxes_sales_num)

#To what extent do you agree with the following statement: Taxes are already high. The government should not collect more money from citizens unless it is targeted at high income citizens, like a tax on high incomes.
table(clean$tradeoff_taxconst_2)

DataClean$tradeoff_taxes_high_income_num <- NA_real_
DataClean$tradeoff_taxes_high_income_num[clean$tradeoff_taxconst_2 == "Strongly agree"] <- 1
DataClean$tradeoff_taxes_high_income_num[clean$tradeoff_taxconst_2 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_taxes_high_income_num[clean$tradeoff_taxconst_2 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_taxes_high_income_num[clean$tradeoff_taxconst_2 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_taxes_high_income_num)

#To what extent do you agree with the following statement:  Taxes are already high. The government should not collect more money from citizens unless it is targeted at wealthy citizens, like a capital gains tax. (Note: a tax you pay when you make money from selling something valuable for more than you paid for it, like property or stocks.)
table(clean$tradeoff_taxconst_3)

DataClean$tradeoff_taxes_wealthy_num <- NA_real_
DataClean$tradeoff_taxes_wealthy_num[clean$tradeoff_taxconst_3 == "Strongly agree"] <- 1
DataClean$tradeoff_taxes_wealthy_num[clean$tradeoff_taxconst_3 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_taxes_wealthy_num[clean$tradeoff_taxconst_3 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_taxes_wealthy_num[clean$tradeoff_taxconst_3 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_taxes_wealthy_num)

#In the question you just answered, which taxation policy was mentioned as a possible exception to the statement that the government should not collect more money from citizens?
table(clean$tradeoff_taxconst_m)

DataClean$tradeoff_taxconst_char <- NA
DataClean$tradeoff_taxconst_char[clean$tradeoff_taxconst_m == "A capital gains tax."]   <- "A capital gains tax."
DataClean$tradeoff_taxconst_char[clean$tradeoff_taxconst_m == "An increase in sales taxes."]  <- "An increase in sales taxes."
DataClean$tradeoff_taxconst_char[clean$tradeoff_taxconst_m == "A tax on high incomes."]  <- "A tax on high incomes."
DataClean$tradeoff_taxconst_char[clean$tradeoff_taxconst_m == "No exception was mentioned."]   <- "No exception was mentioned."
DataClean$tradeoff_taxconst_char[clean$tradeoff_taxconst_m == "I don’t remember."]   <- "I don’t remember."

table(DataClean$tradeoff_taxconst_char)

#binaires
DataClean$tradeoff_capital_bin <- ifelse(DataClean$tradeoff_taxconst_char == "A capital gains tax.", 1, 0)
DataClean$tradeoff_sales_bin <- ifelse(DataClean$tradeoff_taxconst_char == "An increase in sales taxes.", 1, 0)
DataClean$tradeoff_income_bin <- ifelse(DataClean$tradeoff_taxconst_char == "A tax on high incomes.", 1, 0)
DataClean$tradeoff_no_exception_bin <- ifelse(DataClean$tradeoff_taxconst_char == "No exception was mentioned.", 1, 0)
DataClean$tradeoff_dontremember_bin <- ifelse(DataClean$tradeoff_taxconst_char == "I don’t remember.", 1, 0)
table(DataClean$tradeoff_capital_bin)

#Please imagine that the government wants to improve certain social benefits. However, it can only do so by cutting back on other social benefits. To what extent do you find the following cutbacks acceptable in comparison to the improvement they allow? The government subsidizes child care for all families at a cost of lowering other family benefits (e.g., child tax credits or parental leave payments).
table(clean$tradeoff_spend_cc_1)

DataClean$tradeoff_childcare_benefits_num <- NA_real_
DataClean$tradeoff_childcare_benefits_num[clean$tradeoff_spend_cc_1 == "Strongly agree"] <- 1
DataClean$tradeoff_childcare_benefits_num[clean$tradeoff_spend_cc_1 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_childcare_benefits_num[clean$tradeoff_spend_cc_1 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_childcare_benefits_num[clean$tradeoff_spend_cc_1 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_childcare_benefits_num)

#Please imagine that the government wants to improve certain social benefits. However, it can only do so by cutting back on other social benefits. To what extent do you find the following cutbacks acceptable in comparison to the improvement they allow? The government subsidizes childcare for low-income families at a cost of increasing the price of childcare for middle and upper-class families.
table(clean$tradeoff_spend_cc_2)

DataClean$tradeoff_childcare_lowincome_num <- NA_real_
DataClean$tradeoff_childcare_lowincome_num[clean$tradeoff_spend_cc_2 == "Strongly agree"] <- 1
DataClean$tradeoff_childcare_lowincome_num[clean$tradeoff_spend_cc_2 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_childcare_lowincome_num[clean$tradeoff_spend_cc_2 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_childcare_lowincome_num[clean$tradeoff_spend_cc_2 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_childcare_lowincome_num)

#In the question  you just answered, what was mentioned as the trade-off for increasing childcare subsidies?
table(clean$tradeoff_spend_cc_m)
DataClean$tradeoff_childcare_char <- NA
DataClean$tradeoff_childcare_char[clean$tradeoff_spend_cc_m == "A reduction in other family benefits (e.g., child tax credits or parental leave payments)."]   <- "A reduction in other family benefits (e.g., child tax credits or parental leave payments)."
DataClean$tradeoff_childcare_char[clean$tradeoff_spend_cc_m == "An increase in the price of childcare for middle- and upper-class families."]  <- "An increase in the price of childcare for middle- and upper-class families."
DataClean$tradeoff_childcare_char[clean$tradeoff_spend_cc_m == "I don’t remember."]  <- "I don’t remember."
DataClean$tradeoff_childcare_char[clean$tradeoff_spend_cc_m == "No trade-off was mentioned."]   <- "No trade-off was mentioned."

table(DataClean$tradeoff_childcare_char)

#binaires
DataClean$tradeoff_childcare_benefits_bin <- ifelse(DataClean$tradeoff_childcare_char == "A reduction in other family benefits (e.g., child tax credits or parental leave payments).", 1, 0)
DataClean$tradeoff_childcare_income_bin <- ifelse(DataClean$tradeoff_childcare_char == "An increase in the price of childcare for middle- and upper-class families.", 1, 0)
DataClean$tradeoff_childcare_dontremember_bin <- ifelse(DataClean$tradeoff_childcare_char == "I don’t remember.", 1, 0)
DataClean$tradeoff_childcare_no_mention_bin <- ifelse(DataClean$tradeoff_childcare_char == "No trade-off was mentioned.", 1, 0)
table(DataClean$tradeoff_childcare_benefits_bin)

#Please imagine that the government wants to improve certain social benefits. However, it can only do so by cutting back on other social benefits. To what extent do you find the following cutbacks acceptable in comparison to the improvement they allow? The government increases home care for all seniors at a cost of lowering maximum old age pension benefits.
table(clean$tradeoff_spend_hc_1)

DataClean$tradeoff_senior_benefits_num <- NA_real_
DataClean$tradeoff_senior_benefits_num[clean$tradeoff_spend_hc_1 == "Strongly agree"] <- 1
DataClean$tradeoff_senior_benefits_num[clean$tradeoff_spend_hc_1 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_senior_benefits_num[clean$tradeoff_spend_hc_1 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_senior_benefits_num[clean$tradeoff_spend_hc_1 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_senior_benefits_num)

#Please imagine that the government wants to improve certain social benefits. However, it can only do so by cutting back on other social benefits. To what extent do you find the following cutbacks acceptable in comparison to the improvement they allow? The government increases home care for low-income seniors at a cost of increasing the price of home care for middle and upper-class seniors.
table(clean$tradeoff_spend_hc_2)

DataClean$tradeoff_senior_income_num <- NA_real_
DataClean$tradeoff_senior_income_num[clean$tradeoff_spend_hc_2 == "Strongly agree"] <- 1
DataClean$tradeoff_senior_income_num[clean$tradeoff_spend_hc_2 == "Somewhat agree"] <- 0.66
DataClean$tradeoff_senior_income_num[clean$tradeoff_spend_hc_2 == "Somewhat disagree"] <- 0.33
DataClean$tradeoff_senior_income_num[clean$tradeoff_spend_hc_2 == "Strongly disagree"] <- 0
table(DataClean$tradeoff_senior_income_num)

#To what extent do you agree with the following statement? In Canada, people get rewarded for their intelligence and skill.
table(clean$redis_fei_can1)

DataClean$redis_intelligence_num <- NA_real_
DataClean$redis_intelligence_num[clean$redis_fei_can1 == "Most of the time"] <- 1
DataClean$redis_intelligence_num[clean$redis_fei_can1 == "Sometimes"] <- 0.66
DataClean$redis_intelligence_num[clean$redis_fei_can1 == "Rarely"] <- 0.33
DataClean$redis_intelligence_num[clean$redis_fei_can1 == "Never"] <- 0
table(DataClean$redis_intelligence_num)

#To what extent do you agree with the following statement? In Canada, people have equal opportunity to get ahead.
table(clean$redis_fei_can2.)

DataClean$redis_opportunity_num <- NA_real_
DataClean$redis_opportunity_num[clean$redis_fei_can2. == "Most of the time"] <- 1
DataClean$redis_opportunity_num[clean$redis_fei_can2. == "Sometimes"] <- 0.66
DataClean$redis_opportunity_num[clean$redis_fei_can2. == "Rarely"] <- 0.33
DataClean$redis_opportunity_num[clean$redis_fei_can2. == "Never"] <- 0
table(DataClean$redis_opportunity_num)

#In your opinion, what share of rich people are rich for reasons that have nothing to do with how hard they work?
table(clean$redis_pnvo_rich)

DataClean$redis_reasons_rich_num <- NA_real_
DataClean$redis_reasons_rich_num[clean$redis_pnvo_rich == "Most of the time"] <- 1
DataClean$redis_reasons_rich_num[clean$redis_pnvo_rich == "Sometimes"] <- 0.66
DataClean$redis_reasons_rich_num[clean$redis_pnvo_rich == "Rarely"] <- 0.33
DataClean$redis_reasons_rich_num[clean$redis_pnvo_rich == "Never"] <- 0
table(DataClean$redis_reasons_rich_num)

#In your opinion, what share of poor people are poor for reasons that have nothing to do with how hard they work?
table(clean$redis_pnvo_poor)

DataClean$redis_reasons_poor_num <- NA_real_
DataClean$redis_reasons_poor_num[clean$redis_pnvo_poor == "Most of the time"] <- 1
DataClean$redis_reasons_poor_num[clean$redis_pnvo_poor == "Sometimes"] <- 0.66
DataClean$redis_reasons_poor_num[clean$redis_pnvo_poor == "Rarely"] <- 0.33
DataClean$redis_reasons_poor_num[clean$redis_pnvo_poor == "Never"] <- 0
table(DataClean$redis_reasons_poor_num)

#To what extent do you agree with the following statement?  In Canada, income differences are legitimate reflections of differences in people’s  effort.
table(clean$redis_fid_can)

DataClean$redis_effort_num <- NA_real_
DataClean$redis_effort_num[clean$redis_fid_can == "Most of the time"] <- 1
DataClean$redis_effort_num[clean$redis_fid_can == "Sometimes"] <- 0.66
DataClean$redis_effort_num[clean$redis_fid_can == "Rarely"] <- 0.33
DataClean$redis_effort_num[clean$redis_fid_can == "Never"] <- 0
table(DataClean$redis_effort_num)

#To what extent do you agree with the following statement?  People on social benefits do not really have a choice.
table(clean$redis_prev_freerider)

DataClean$redis_social_benefits_num <- NA_real_
DataClean$redis_social_benefits_num[clean$redis_prev_freerider == "Most of the time"] <- 1
DataClean$redis_social_benefits_num[clean$redis_prev_freerider == "Sometimes"] <- 0.66
DataClean$redis_social_benefits_num[clean$redis_prev_freerider == "Rarely"] <- 0.33
DataClean$redis_social_benefits_num[clean$redis_prev_freerider == "Never"] <- 0
table(DataClean$redis_social_benefits_num)

#How often does welfare go to people who do not really deserve it?
table(clean$redis_iden_freerider)

DataClean$redis_welfare_num <- NA_real_
DataClean$redis_welfare_num[clean$redis_iden_freerider == "Most of the time"] <- 1
DataClean$redis_welfare_num[clean$redis_iden_freerider == "Sometimes"] <- 0.66
DataClean$redis_welfare_num[clean$redis_iden_freerider == "Rarely"] <- 0.33
DataClean$redis_welfare_num[clean$redis_iden_freerider == "Never"] <- 0
table(DataClean$redis_welfare_num)

#To what extent can others be trusted to not abuse and cheat the system?
table(clean$redis_human_nature.)

DataClean$redis_no_cheat_system_num <- NA_real_
DataClean$redis_no_cheat_system_num[clean$redis_human_nature. == "Most of the time"] <- 1
DataClean$redis_no_cheat_system_num[clean$redis_human_nature. == "Sometimes"] <- 0.66
DataClean$redis_no_cheat_system_num[clean$redis_human_nature. == "Rarely"] <- 0.33
DataClean$redis_no_cheat_system_num[clean$redis_human_nature. == "Never"] <- 0
table(DataClean$redis_no_cheat_system_num)



##Generally speaking would you say that most people can be trusted, or, that you need to be very careful when dealing with people? - On a scale of 0 to 10 where 0 means no trust and 10 means complete trust.
table(data$trust_social_1)
data$trust_social_bin <- ifelse(data$trust_social_1 >= 6, 1, 
                                ifelse(data$trust_social_1 <= 5, 0, NA))

DataClean$trust_social_bin <- data$trust_social_bin[match(rownames(DataClean), rownames(data))]

table(DataClean$trust_social_bin, useNA = "ifany")


##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - Political parties
table(data$trust_institution_1)
data$trust_pol_parties_bin <- ifelse(data$trust_institution_1 >= 6, 1,
                                       ifelse(data$trust_institution_1 <= 5, 0, NA))

DataClean$trust_pol_parties_bin <- data$trust_pol_parties_bin[match(rownames(DataClean), rownames(data))]

table(DataClean$trust_pol_parties_bin, useNA = "ifany")


##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - The federal government
table(data$trust_institution_2)

data$trust_fed_gov_bin <- ifelse(data$trust_institution_1 >= 6, 1,
                                     ifelse(data$trust_institution_1 <= 5, 0, NA))

DataClean$trust_fed_gov_bin <- data$trust_fed_gov_bin[match(rownames(DataClean), rownames(data))]

table(DataClean$trust_fed_gov_bin, useNA = "ifany")
##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - The provincial government
table(data$trust_institution_3)

data$trust_prov_gov_bin <- ifelse(data$trust_institution_1 >= 6, 1,
                                 ifelse(data$trust_institution_1 <= 5, 0, NA))

DataClean$trust_prov_gov_bin <- data$trust_prov_gov_bin[match(rownames(DataClean), rownames(data))]

table(DataClean$trust_prov_gov_bin, useNA = "ifany")
##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - The mass media
table(data$trust_institution_4)

data$trust_media_bin <- ifelse(data$trust_institution_1 >= 6, 1,
                                 ifelse(data$trust_institution_1 <= 5, 0, NA))

DataClean$trust_media_bin <- data$trust_media_bin[match(rownames(DataClean), rownames(data))]

table(DataClean$trust_media_bin, useNA = "ifany")

##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - Your provincial government
table(data$trust_institution_5)

data$trust_your_prov_gov_bin <- ifelse(data$trust_institution_1 >= 6, 1,
                                 ifelse(data$trust_institution_1 <= 5, 0, NA))

DataClean$trust_your_prov_gov_bin <- data$trust_your_prov_gov_bin[match(rownames(DataClean), rownames(data))]

table(DataClean$trust_your_prov_gov_bin, useNA = "ifany")
##Generally speaking would you say that most people can be trusted, or, that you need to be very careful when dealing with people? - On a scale of 0 to 10 where 0 means no trust and 10 means complete trust.
table(data$trust_social_terr_1)
##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - Political parties
table(data$trust_institut_terr_1)
##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - The federal government
table(data$trust_institut_terr_2)
##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - The provincial government
table(data$trust_institut_terr_3)
##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - The mass media
table(data$trust_institut_terr_4)
##Please indicate how much trust you have in the following: (On a scale of 0  to 10 where 0 means no trust and 10 means complete trust.) - Your territorial government
table(data$trust_institut_terr_5)




##To what extent do you agree with the following statement: The government should increase spending on green economy.
#table(clean$tradeoff_invest_ge0.)
#
#DataClean$tradeoff_invest_green_num <- NA_real_
#DataClean$tradeoff_invest_green_num[clean$tradeoff_invest_ge0. == "Strongly agree"] <- 1
#DataClean$tradeoff_invest_green_num[clean$tradeoff_invest_ge0. == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_invest_green_num[clean$tradeoff_invest_ge0. == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_invest_green_num[clean$tradeoff_invest_ge0. == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_invest_green_num)
#
##To what extent do you agree with the following statement: The government should increase spending on the green economy, even if that implies higher taxes.
#table(clean$tradeoff_invest_ge1)
#
#DataClean$tradeoff_taxes_green_num <- NA_real_
#DataClean$tradeoff_taxes_green_num[clean$tradeoff_invest_ge1 == "Strongly agree"] <- 1
#DataClean$tradeoff_taxes_green_num[clean$tradeoff_invest_ge1 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_taxes_green_num[clean$tradeoff_invest_ge1 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_taxes_green_num[clean$tradeoff_invest_ge1 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_taxes_green_num)
#
##To what extent do you agree with the following statement:  The government should increase spending on the green economy, even if that implies cutting back in other areas.
#table(clean$tradeoff_invest_ge2)
#
#DataClean$tradeoff_cutting_for_green_num <- NA_real_
#DataClean$tradeoff_cutting_for_green_num[clean$tradeoff_invest_ge2 == "Strongly agree"] <- 1
#DataClean$tradeoff_cutting_for_green_num[clean$tradeoff_invest_ge2 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_cutting_for_green_num[clean$tradeoff_invest_ge2 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_cutting_for_green_num[clean$tradeoff_invest_ge2 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_cutting_for_green_num)
#
##To what extent do you agree with the following statement:  The government should increase spending on the green economy, even if that implies a higher public debt.
#table(clean$tradeoff_invest_ge3)
#
#DataClean$tradeoff_debt_green_num <- NA_real_
#DataClean$tradeoff_debt_green_num[clean$tradeoff_invest_ge3 == "Strongly agree"] <- 1
#DataClean$tradeoff_debt_green_num[clean$tradeoff_invest_ge3 == "Somewhat agree"] <- 0.66
#DataClean$tradeoff_debt_green_num[clean$tradeoff_invest_ge3 == "Somewhat disagree"] <- 0.33
#DataClean$tradeoff_debt_green_num[clean$tradeoff_invest_ge3 == "Strongly disagree"] <- 0
#table(DataClean$tradeoff_debt_green_num)
#

##province
#table(clean$province)
#
#DataClean$province_char <- NA
#DataClean$province_char[clean$province == "Alberta"]   <- 1
#DataClean$province_char[clean$province == "British Columbia"]  <- 2
#DataClean$province_char[clean$province == "Colombie-Britannique"]  <- 3
#DataClean$province_char[clean$province == "Nova Scotia"]   <- 4
#DataClean$province_char[clean$province == "Ontario"]   <- 5
#DataClean$province_char[clean$province == "Quebec"]   <- 6
#DataClean$province_char[clean$province == "Québec"]   <- 7
#DataClean$province_char[clean$province == "Yukon"]   <- 8
#
#table(DataClean$province_char)
#
##binaires
#DataClean$alberta_bin <- ifelse(DataClean$province_char == 1, 1, 0)
#DataClean$BC_EN_bin <- ifelse(DataClean$province_char == 2, 1, 0)
#DataClean$BC_FR_bin <- ifelse(DataClean$province_char == 3, 1, 0)
#DataClean$nova_scotia_bin <- ifelse(DataClean$province_char == 4, 1, 0)
#DataClean$ontario_bin <- ifelse(DataClean$province_char == 5, 1, 0)
#DataClean$quebec_EN_bin <- ifelse(DataClean$province_char == 6, 1, 0)
#DataClean$quebec_FR_bin <- ifelse(DataClean$province_char == 7, 1, 0)
#DataClean$yukon_bin <- ifelse(DataClean$province_char == 8, 1, 0)
#table(DataClean$quebec_FR_bin)


write.csv(DataClean, "data/clean_df.csv")

