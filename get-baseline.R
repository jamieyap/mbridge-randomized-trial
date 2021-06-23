library(dplyr)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

###############################################################################
# Read in raw data files
###############################################################################

dat_baseline <- read.csv(file.path(path_input_data, "Baseline Data with HD variables added (2.8.21) final.csv"))

###############################################################################
# Create scores
###############################################################################

dat_baseline <- dat_baseline %>%
  rename(participant_id = External_Data_Reference) %>%
  # Re-code missing values in variables to be used for anxiety score: from -99 to NA
  mutate(MH1gad0 = replace(MH1gad0, MH1gad0 == -99, NA),
         MH2gad0 = replace(MH2gad0, MH2gad0 == -99, NA),
         MH3gad0 = replace(MH3gad0, MH3gad0 == -99, NA),
         MH4gad0 = replace(MH4gad0, MH4gad0 == -99, NA),
         MH5gad0 = replace(MH5gad0, MH5gad0 == -99, NA),
         MH6gad0 = replace(MH6gad0, MH6gad0 == -99, NA),
         MH7gad0 = replace(MH7gad0, MH7gad0 == -99, NA)) %>%
  # Re-code missing values in variables to be used for depression score: from -99 to NA
  mutate(MH1phq0 = replace(MH1phq0, MH1phq0 == -99, NA),
         MH2phq0 = replace(MH2phq0, MH2phq0 == -99, NA),
         MH3phq0 = replace(MH3phq0, MH3phq0 == -99, NA),
         MH4phq0 = replace(MH4phq0, MH4phq0 == -99, NA),
         MH5phq0 = replace(MH5phq0, MH5phq0 == -99, NA),
         MH6phq0 = replace(MH6phq0, MH6phq0 == -99, NA),
         MH7phq0 = replace(MH7phq0, MH7phq0 == -99, NA),
         MH8phq0 = replace(MH8phq0, MH8phq0 == -99, NA),
         MH9phq0 = replace(MH9phq0, MH9phq0 == -99, NA)) %>%
  # Re-code missing values in variables to be used for stress score: from -99 to NA
  mutate(MH1pss0 = replace(MH1pss0, MH1pss0 == -99, NA),
         MH2pss0 = replace(MH2pss0, MH2pss0 == -99, NA),
         MH3pss0 = replace(MH3pss0, MH3pss0 == -99, NA),
         MH4pss0 = replace(MH4pss0, MH4pss0 == -99, NA),
         MH5pss0 = replace(MH5pss0, MH5pss0 == -99, NA),
         MH6pss0 = replace(MH6pss0, MH6pss0 == -99, NA),
         MH7pss0 = replace(MH7pss0, MH7pss0 == -99, NA),
         MH8pss0 = replace(MH8pss0, MH8pss0 == -99, NA),
         MH9pss0 = replace(MH9pss0, MH9pss0 == -99, NA),
         MH10pss0 = replace(MH10pss0, MH10pss0 == -99, NA)) %>%
  # Reverse score items in preparation for the calculation of an overall stress score
  mutate(MH4pss0RC = abs(MH4pss0-4),
         MH5pss0RC = abs(MH5pss0-4),
         MH7pss0RC = abs(MH7pss0-4),
         MH8pss0RC = abs(MH8pss0-4)) %>%
  mutate(anxiety = MH1gad0 + MH2gad0 + MH3gad0 + MH4gad0 + MH5gad0 + MH6gad0 + MH7gad0,
         depression = MH1phq0 + MH2phq0 + MH3phq0 + MH4phq0 + MH5phq0 + MH6phq0 + MH7phq0 + MH8phq0 + MH9phq0,
         stress = MH1pss0 + MH2pss0 + MH3pss0 + MH4pss0RC + MH5pss0RC + MH6pss0 + MH7pss0RC + MH8pss0RC + MH9pss0 + MH10pss0)

###############################################################################
# Data preparation steps on other variables
###############################################################################

# Control variables
dat_baseline <- dat_baseline %>%
  mutate(Freqalc1 = replace(Freqalc1, Freqalc1 == -99, NA),
         ALCdrinkqty_30days0 = replace(ALCdrinkqty_30days0, ALCdrinkqty_30days0 == -99, NA),
         DGgender0 = replace(DGgender0, (DGgender0 == 4) | (DGgender0 == -99), NA),
         DGrace_white0 = replace(DGrace_white0, DGrace_white0 == -99, NA)) %>%
  mutate(ALCdrinkqty_30days0 = replace(ALCdrinkqty_30days0, Freqalc1==0, 0),
         is_female = if_else(DGgender0==1, 1, 0),
         is_male = if_else(DGgender0==0, 1, 0)) %>%
  rename(is_white = DGrace_white0,
         typical_num_drinks_per_day = ALCdrinkqty_30days0,
         tot_days_with_any_drinks = Freqalc1)

###############################################################################
# Grab columns, including the Product or Charity of choice that was specified
# by the participant at baseline
###############################################################################

dat_baseline <- dat_baseline %>%
  rename(baseline_charity_choice = CharityChoice, 
         baseline_product_choice = ProductChoice,
         baseline_anxiety = anxiety,
         baseline_depression = depression,
         baseline_stress = stress) %>%
  select(participant_id, 
         baseline_charity_choice, baseline_product_choice,
         baseline_anxiety, baseline_depression, baseline_stress,
         tot_days_with_any_drinks, typical_num_drinks_per_day, 
         is_female, is_male, is_white)

###############################################################################
# Save data files
###############################################################################

save(dat_baseline, file = file.path(path_staged_data, "dat_baseline.RData"))

