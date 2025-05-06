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
  select(-StartDate, -EndDate, -Status, -Progress, -RecordedDate,
         -ExternalReferencel) %>%
  filter(
    Finished == "True",
    consent == "I have read the consent form and agree to participate in this survey.",
    consent_end_1 == "Yes",
    consent_end_2 == "Yes",
    attention == "Brown"
  )

clean <- clean %>%
  select(-Finished, - consent, -consent_end_1, -consent_end_2)

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

