# ==============================================================
# 00_packages.R
# Load all required packages and resolve namespace conflicts
# ==============================================================

library(tidyverse)
library(ggplot2)
library(ggeffects)
library(marginaleffects)
library(modelsummary)
library(sandwich)
library(lmtest)
library(ggthemes)
library(RColorBrewer)
library(ggpattern)
library(flextable)
library(officer)

# Resolve conflicts with MASS
select <- dplyr::select
filter <- dplyr::filter
recode <- dplyr::recode
