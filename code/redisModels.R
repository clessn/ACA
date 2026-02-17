# ===================================================================
# MODELS FOR REDISTRIBUTION
# ===================================================================

# -----------------------
# Load packages
# -----------------------
library(tidyverse)
library(tidyr)
library(dplyr)

# -----------------------
# Load wrangled and clean data (from aca_wrangling_W26.csv, now data/clean_df_valid.csv)
# -----------------------

df <- read.csv("data/clean_df_valid.csv")

# -----------------------
# Regional differences on redistribution preferences
# -----------------------