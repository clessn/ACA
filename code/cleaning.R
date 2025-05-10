# ===================================================
# Cleaning & Wrangling ACA
# ===================================================
# Version: April 30th, 2025, 9:30 am

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
raw_data <- read.csv("data/ACA_April_30.csv") 
# -----------------------
# 2. Remove IPAddress 
# -----------------------
#raw_data <- raw_data %>%
#  select(-IPAddress, -RecipientLastName, -RecipientFirstName,
#         -RecipientEmail, -LocationLatitude, -LocationLongitude,
#         -DistributionChannel)
#
#write.csv(raw_data, "data/ACA_April_30.csv")

raw_data <- read.csv("data/ACA_April_30.csv") 
# -----------------------
# 3. Clean metadata rows
# -----------------------
questions <- raw_data[1, ]
values <- raw_data[2, ]

data <- raw_data[-c(1,2), ]
names(data) <- names(questions)

# -----------------------
# 4. Generate codebook
# -----------------------
# Create the `values` column with unique values per variable
values <- map_chr(data, ~ paste(unique(.x), collapse = ", "))

# Assuming `questions` is defined and has the same length as `data`
codebook <- tibble(
  variable = names(data),
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
DataClean$eage1824   <- ifelse(DataClean$ses_age >= 18 & DataClean$ses_age <= 24, 1, 0)
DataClean$eage2534   <- ifelse(DataClean$ses_age >= 25 & DataClean$ses_age <= 34, 1, 0)
DataClean$eage3544   <- ifelse(DataClean$ses_age >= 35 & DataClean$ses_age <= 44, 1, 0)
DataClean$eage4554   <- ifelse(DataClean$ses_age >= 45 & DataClean$ses_age <= 54, 1, 0)
DataClean$eage5564   <- ifelse(DataClean$ses_age >= 55 & DataClean$ses_age <= 64, 1, 0)
DataClean$eage65plus <- ifelse(DataClean$ses_age >= 65, 1, 0)

# Vérification
table(DataClean$ses_age)
table(DataClean$age1824)

#In which province or territory are you currently living?-------------------------------------------------------------
attributes(clean$ses_region.)
table(clean$ses_region.)

DataClean$alberta <- ifelse(clean$ses_region. == "Alberta", 1, 0)
DataClean$bcolumbia <- ifelse(clean$ses_region. == "British Columbia", 1, 0)
DataClean$nova_scotia <- ifelse(clean$ses_region. == "Nova Scotia", 1, 0)
DataClean$ontario <- ifelse(clean$ses_region. == "Ontario", 1, 0)
DataClean$quebec <- ifelse(clean$ses_region. == "Quebec", 1, 0)
DataClean$yukon <- ifelse(clean$ses_region. == "Yukon", 1, 0)

table(DataClean$alberta)
table(DataClean$quebec)

#What language do you speak most often at home?-------------------------------------------------------------------------
attributes(clean$ses_language)
table(clean$ses_language)
DataClean$ses_french_bin <- ifelse(clean$ses_language == "French", 1, 0)
DataClean$ses_french_bin[clean$ses_language == "Other"] <- NA
table(DataClean$ses_french_bin)

#What is the highest level of education that you have completed?--------------------------------------------------------
attributes(clean$ses_education)
table(clean$ses_education)
DataClean$ses_education_char <- NA
DataClean$ses_education_char[clean$ses_education == "Bachelor’s degree"] <- 1
DataClean$ses_education_char[clean$ses_education == "Completed elementary school"] <- 2
DataClean$ses_education_char[clean$ses_education == "Completed secondary/ high school"] <- 3
DataClean$ses_education_char[clean$ses_education == "Completed technical, community college, CEGEP, College Classique"] <- 4
DataClean$ses_education_char[clean$ses_education == "Master’s degree"] <- 5
DataClean$ses_education_char[clean$ses_education == "Professional degree or doctorate"] <- 6
DataClean$ses_education_char[clean$ses_education == "Some secondary/ high school"] <- 7
DataClean$ses_education_char[clean$ses_education == "Some technical, community college, CEGEP, College Classique"] <- 8
DataClean$ses_education_char[clean$ses_education == "Some university"] <- 9

table(DataClean$ses_education_char)

DataClean$ses_educBachelor_bin <- ifelse(DataClean$ses_education_char == 1, 1, 0)
DataClean$ses_educElementary_bin <- ifelse(DataClean$ses_education_char == 2, 1, 0)
DataClean$ses_educSecondaryCompleted_bin <- ifelse(DataClean$ses_education_char == 3, 1, 0)
DataClean$ses_educTechnicalCompleted_bin <- ifelse(DataClean$ses_education_char == 4, 1, 0)
DataClean$ses_educMaster_bin <- ifelse(DataClean$ses_education_char == 5, 1, 0)
DataClean$ses_educDoctorate_bin <- ifelse(DataClean$ses_education_char == 6, 1, 0)
DataClean$ses_educHighSchoolSome_bin <- ifelse(DataClean$ses_education_char == 7, 1, 0)
DataClean$ses_educTechnicalSome_bin <- ifelse(DataClean$ses_education_char == 8, 1, 0)
DataClean$ses_educUniversitySome_bin <- ifelse(DataClean$ses_education_char == 9, 1, 0)

table(DataClean$ses_educBachelor_bin)

#Approximately, which of the following categories does your-----------------------------------------------------------------------------------------
attributes(clean$ses_income)
table(clean$ses_income)
DataClean$ses_income_char <- NA
DataClean$ses_income_num <- NA

DataClean$ses_income_char[clean$ses_income == "$1 to $30,000"]             <- 1
DataClean$ses_income_char[clean$ses_income == "$30,001 to $60,000"]        <- 2
DataClean$ses_income_char[clean$ses_income == "$60,001 to $90,000"]        <- 3
DataClean$ses_income_char[clean$ses_income == "$90,001 to $110,000"]       <- 4
DataClean$ses_income_char[clean$ses_income == "$110,001 to $150,000"]      <- 5
DataClean$ses_income_char[clean$ses_income == "$150,001 to $200,000"]      <- 6
DataClean$ses_income_char[clean$ses_income == "More than $200,000"]        <- 7


DataClean$ses_income30000_bin      <- ifelse(DataClean$ses_income_char == 1, 1, 0)
DataClean$ses_income60000_bin      <- ifelse(DataClean$ses_income_char == 2, 1, 0)
DataClean$ses_income90000_bin      <- ifelse(DataClean$ses_income_char == 3, 1, 0)
DataClean$ses_income110000_bin     <- ifelse(DataClean$ses_income_char == 4, 1, 0)
DataClean$ses_income150000_bin     <- ifelse(DataClean$ses_income_char == 5, 1, 0)
DataClean$ses_income200000_bin     <- ifelse(DataClean$ses_income_char == 6, 1, 0)
DataClean$ses_incomeMore200000_bin <- ifelse(DataClean$ses_income_char == 7, 1, 0)

# Vérifie
table(DataClean$ses_income110000_bin)

#What are the first three characters of your postal code?-----------------------------------------------------
attributes(clean$ses_postal_code)
table(clean$ses_postal_code)
DataClean$ses_postalCode <- NA
DataClean$ses_postalCode <- clean$ses_postal_code
table(DataClean$ses_postalCode)

#What is your marital status?-------------------------------------------------------------------------------------
attributes(clean$ses_marital_status)
table(clean$ses_marital_status)
DataClean$ses_matStatus <- NA

DataClean$ses_matStatus_char[clean$ses_marital_status == "Single"]                  <- 1
DataClean$ses_matStatus_char[clean$ses_marital_status == "Married"]                 <- 2
DataClean$ses_matStatus_char[clean$ses_marital_status == "Common-law relationship"] <- 3
DataClean$ses_matStatus_char[clean$ses_marital_status == "Widower/widow"]           <- 4
DataClean$ses_matStatus_char[clean$ses_marital_status == "Divorced/separated"]      <- 5

DataClean$matStatus_single_bin      <- ifelse(DataClean$ses_matStatus_char == 1, 1, 0)
DataClean$matStatus_married_bin     <- ifelse(DataClean$ses_matStatus_char == 2, 1, 0)
DataClean$matStatus_commonlaw_bin   <- ifelse(DataClean$ses_matStatus_char == 3, 1, 0)
DataClean$matStatus_widow_bin       <- ifelse(DataClean$ses_matStatus_char == 4, 1, 0)
DataClean$matStatus_divorced_bin    <- ifelse(DataClean$ses_matStatus_char == 5, 1, 0)

# Vérification
table(DataClean$matStatus_commonlaw_bin)

#Is the home you currently live in------------------------------------------------------------
attributes(clean$ses_home_ownership)
table(clean$ses_home_ownership)

DataClean$ses_home_ownership_char <- NA
DataClean$ses_home_ownership_char[clean$ses_home_ownership == "Owned by your family or a member of your household"] <- 1
DataClean$ses_home_ownership_char[clean$ses_home_ownership == "Rented"] <- 2

DataClean$home_owned_bin  <- ifelse(DataClean$ses_home_ownership_char == 1, 1, 0)
DataClean$home_rented_bin <- ifelse(DataClean$ses_home_ownership_char == 2, 1, 0)

table(DataClean$home_owned_bin)
table(DataClean$home_rented_bin)

#Do you live...----------------------------------------------------------------------
attributes(clean$ses_children.)
table(clean$ses_children.)

DataClean$ses_children_char <- NA
DataClean$ses_children_char[clean$ses_children. == "Alone with children"]           <- 1
DataClean$ses_children_char[clean$ses_children. == "Alone without children"]        <- 2
DataClean$ses_children_char[clean$ses_children. == "In couple with children"]       <- 3
DataClean$ses_children_char[clean$ses_children. == "In couple without children"]    <- 4

#binaires
DataClean$alone_with_children_bin        <- ifelse(DataClean$ses_children_char == 1, 1, 0)
DataClean$alone_without_children_bin     <- ifelse(DataClean$ses_children_char == 2, 1, 0)
DataClean$couple_with_children_bin       <- ifelse(DataClean$ses_children_char == 3, 1, 0)
DataClean$couple_without_children_bin    <- ifelse(DataClean$ses_children_char == 4, 1, 0)

# Vérification
table(DataClean$children_couple_with_bin)

#Were you born in Canada?-----------------------------------------------------------------------------------------
attributes(clean$ses_citizen_status)
table(clean$ses_citizen_status)
DataClean$ses_citizenYes_bin <- ifelse(clean$ses_citizen_status == "Yes", 1, 0)
table(DataClean$ses_citizenYes_bin)

#What best describes your current employment status?----------------------------------------------------------
attributes(clean$ses_employ_status.)
table(clean$ses_employ_status.)

DataClean$ses_employ_status_char <- NA
DataClean$ses_employ_status_char[clean$ses_employ_status. == "A caregiver or homemaker"] <- 1
DataClean$ses_employ_status_char[clean$ses_employ_status. == "A student attending school"] <- 2
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Not working due to illness/disability, or not looking for work"] <- 3
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Retired"] <- 4
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Seasonal work"] <- 5
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Self-employed"] <- 6
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Temporarily not working (e.g. parental leave, seasonal worker, in the process of changing jobs)"] <- 7
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Unemployed, and looking for work"] <- 8
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Working full-time (35 or more hours per week)"] <- 9
DataClean$ses_employ_status_char[clean$ses_employ_status. == "Working part-time (less than 35 hours per week)"] <- 10

#binaires
DataClean$employ_caregiver_bin     <- ifelse(DataClean$ses_employ_status_char == 1, 1, 0)
DataClean$employ_student_bin       <- ifelse(DataClean$ses_employ_status_char == 2, 1, 0)
DataClean$employ_disabled_bin      <- ifelse(DataClean$ses_employ_status_char == 3, 1, 0)
DataClean$employ_retired_bin       <- ifelse(DataClean$ses_employ_status_char == 4, 1, 0)
DataClean$employ_seasonal_bin      <- ifelse(DataClean$ses_employ_status_char == 5, 1, 0)
DataClean$employ_self_bin          <- ifelse(DataClean$ses_employ_status_char == 6, 1, 0)
DataClean$employ_temp_notwork_bin  <- ifelse(DataClean$ses_employ_status_char == 7, 1, 0)
DataClean$employ_unemployed_bin    <- ifelse(DataClean$ses_employ_status_char == 8, 1, 0)
DataClean$employ_fulltime_bin      <- ifelse(DataClean$ses_employ_status_char == 9, 1, 0)
DataClean$employ_parttime_bin      <- ifelse(DataClean$ses_employ_status_char == 10, 1, 0)

# Vérification
table(DataClean$employ_fulltime_bin)

#Which option best describes your current family structure-----------------------------------------------------------------
table(clean$ses_family_structure)

DataClean$ses_family_structure_char <- NA
DataClean$ses_family_structure_char[clean$ses_family_structure == "Single parent"] <- 1
DataClean$ses_family_structure_char[clean$ses_family_structure == "Two parents in a relationship but living separately"] <- 2
DataClean$ses_family_structure_char[clean$ses_family_structure == "Two parents in a relationship in a first union (marriage or common-law)"] <- 3
DataClean$ses_family_structure_char[clean$ses_family_structure == "Two parents in a relationship in a second union (marriage or common-law)"] <- 4
DataClean$ses_family_structure_char[clean$ses_family_structure == "Other/Prefer not to answer"] <- 5


#binaires
DataClean$family_single_bin       <- ifelse(DataClean$ses_family_structure_char == 1, 1, 0)
DataClean$family_separate_bin     <- ifelse(DataClean$ses_family_structure_char == 2, 1, 0)
DataClean$family_first_union_bin  <- ifelse(DataClean$ses_family_structure_char == 3, 1, 0)
DataClean$family_second_union_bin <- ifelse(DataClean$ses_family_structure_char == 4, 1, 0)
DataClean$family_other_bin        <- ifelse(DataClean$ses_family_structure_char == 5, 1, 0)

# Vérification
table(DataClean$family_first_union_bin)

#How many children do you have in each of the following age groups currently living with you in your household? - Ages 0 to 5--------------
table(clean$ses_household_compo._1)

DataClean$ses_children05_char <- NA
DataClean$ses_children05_char[clean$ses_household_compo._1 == 0] <- "0 enfant 0-5 ans"
DataClean$ses_children05_char[clean$ses_household_compo._1 == 1] <- "1 enfant 0-5 ans"
DataClean$ses_children05_char[clean$ses_household_compo._1 == 2] <- "2 enfants 0-5 ans"
DataClean$ses_children05_char[clean$ses_household_compo._1 == 3] <- "3 enfants 0-5 ans"

#binaires
DataClean$ses_0children05_bin <- ifelse(DataClean$ses_children05_char == "0 enfant 0-5 ans", 1, 0)
DataClean$ses_1children05_bin <- ifelse(DataClean$ses_children05_char == "1 enfant 0-5 ans", 1, 0)
DataClean$ses_2children05_bin <- ifelse(DataClean$ses_children05_char == "2 enfants 0-5 ans", 1, 0)
DataClean$ses_3children05_bin <- ifelse(DataClean$ses_children05_char == "3 enfants 0-5 ans", 1, 0)

# Vérification
table(DataClean$ses_3children05_bin)


#How many children do you have in each of the following age groups currently living with you in your household? - Ages 6-12-----------------
table(clean$ses_household_compo._2)

DataClean$ses_children612_char <- NA
DataClean$ses_children612_char[clean$ses_household_compo._2 == 0] <- "0 enfant 6-12 ans"
DataClean$ses_children612_char[clean$ses_household_compo._2 == 1] <- "1 enfant 6-12 ans"
DataClean$ses_children612_char[clean$ses_household_compo._2 == 2] <- "2 enfants 6-12 ans"
DataClean$ses_children612_char[clean$ses_household_compo._2 == 3] <- "3 enfants 6-12 ans"

#binaires
DataClean$ses_0children612_bin <- ifelse(DataClean$ses_children612_char == "0 enfant 6-12 ans", 1, 0)
DataClean$ses_1children612_bin <- ifelse(DataClean$ses_children612_char == "1 enfant 6-12 ans", 1, 0)
DataClean$ses_2children612_bin <- ifelse(DataClean$ses_children612_char == "2 enfants 6-12 ans", 1, 0)
DataClean$ses_3children612_bin <- ifelse(DataClean$ses_children612_char == "3 enfants 6-12 ans", 1, 0)

# Vérification
table(DataClean$ses_3children612_bin)

#How many children do you have in each of the following age groups currently living with you in your household? - Ages 13-17-----------------
table(clean$ses_household_compo._3)

DataClean$ses_children1317_char <- NA
DataClean$ses_children1317_char[clean$ses_household_compo._3 == 0] <- "0 enfant 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo._3 == 1] <- "1 enfant 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo._3 == 2] <- "2 enfants 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo._3 == 3] <- "3 enfants 13-17 ans"
DataClean$ses_children1317_char[clean$ses_household_compo._3 == 4] <- "4 enfants 13-17 ans"
#binaires
DataClean$ses_0children1317_bin <- ifelse(DataClean$ses_children1317_char == "0 enfant 13-17 ans", 1, 0)
DataClean$ses_1children1317_bin <- ifelse(DataClean$ses_children1317_char == "1 enfant 13-17 ans", 1, 0)
DataClean$ses_2children1317_bin <- ifelse(DataClean$ses_children1317_char == "2 enfants 13-17 ans", 1, 0)
DataClean$ses_3children1317_bin <- ifelse(DataClean$ses_children1317_char == "3 enfants 13-17 ans", 1, 0)
DataClean$ses_4children1317_bin <- ifelse(DataClean$ses_children1317_char == "4 enfants 13-17 ans", 1, 0)


# Vérification
table(DataClean$ses_3children1317_bin)

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
--------------------------------
table(clean$ideo_left_right_1)
