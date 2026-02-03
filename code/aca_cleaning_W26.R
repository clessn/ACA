# ===================================================
# Cleaning ACA Winter 2026 and Saving as New CSV
# ===================================================
# Version: Feb 3, 2026 (downloaded data frame from Qualtrics and removed unnecessary information)

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
# 1. Raw data
# -----------------------

data <- read.csv("data/ACA_feb3.csv") 

# -----------------------
# 2. Clean unecessary columns (incl. for ethics) and responses not 100%
# -----------------------
# Remove unnecessary columns from Qualtrics
# Filter for finished survey and consented to all
# Once removing those who did not finish, did not consent and did not correctly respond
# to the attention check: from n =  to n = 

clean <- subset(data, Progress == 100)

clean <- data %>%
 dplyr::select(-StartDate, -EndDate, -Status, -IPAddress, -Progress, -RecordedDate,
               -RecipientLastName, -RecipientFirstName, -RecipientEmail, -ExternalReference,
               -LocationLatitude, -LocationLongitude, -DistributionChannel
 ) %>%
  dplyr::filter(
    Finished == "True",
    consent == "I have read the consent form and agree to participate in this survey.",
    consent_end_1 == "Yes",
    consent_end_2 == "Yes",
    attention == "Brown"
  )

clean <- clean %>%
 dplyr::select(-Finished, - consent, -consent_end_1, -consent_end_2)

# -----------------------
# 3. Clean metadata rows for codebook
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
# 5. Save new dataframe & codebook for data wrangling
# -----------------------
write.csv(
  clean,
  file = "data/aca_ethics_W26.csv",
  row.names = FALSE
)

saveRDS(codebook, file = "codebook_ethics.rds")

# Move to aca_wrangling_W26.R script for rest
