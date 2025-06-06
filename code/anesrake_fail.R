# Anesrake fail


# df_rake <- df[!is.na(df$id) & !is.na(df$gender) & !is.na(df$age) &
!is.na(df$education) & !is.na(df$income), ]

# table(df_rake$education, useNA = "ifany")

# Step 2: Convert all to numeric
# This will preserve levels if already numeric, or convert factor labels correctly
#df_rake$gender <- as.numeric(as.character(df_rake$gender))
#df_rake$age <- as.numeric(as.character(df_rake$age))
#df_rake$income <- as.numeric(as.character(df_rake$income))
#df_rake$education <- as.numeric(as.character(df_rake$education))
#
#
## Check distribution again
#lapply(df_rake[c("gender", "age", "education", "income")], table)

## Step 4: Define population proportions
#pop.margins <- list(
#  gender    = c("1" = 0.49, "2" = 0.51),
#  age       = c("1" = 0.24, "2" = 0.33, "3" = 0.43),
#  education = c("1" = 0.71, "2" = 0.29),   # Make sure keys are "1" and "2", not 1 and 2
#  income    = c("1" = 0.135, "2" = 0.7, "3" = 0.165)
#)



# Step 5: Construct inputter list (numeric vectors with population margins as attributes)
#inputter <- list(
#  gender    = df_rake$gender,
#  age       = df_rake$age,
#  education = df_rake$education,
#  income    = df_rake$income
#)
#
#for (var in names(inputter)) {
#  attr(inputter[[var]], "population") <- pop.margins[[var]]
#}
#
## Step 6: Run raking
#raking.weights <- anesrake(
#  inputter      = inputter,
#  dataframe     = df_rake,
#  caseid        = df_rake$id,
#  cap           = 5,
#  maxit         = 1000,
#  convcrit      = 1e-4,
#  choosemethod  = "total",
#  verbose       = TRUE
#)
