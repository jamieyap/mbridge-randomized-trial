library(dplyr)

source("paths.R")

###############################################################################
# Read in raw data files
###############################################################################

dat_baseline <- read.csv(file.path(path_input_data, "Baseline Data with HD variables added (2.8.21) final.csv"))

###############################################################################
# Initial clean up
###############################################################################

dat_baseline <- dat_baseline %>%
  mutate(across(.cols = where(~is.numeric(.)), .fns = ~replace(., . ==-99, NA_real_))) %>%
  mutate(across(.cols = where(~is.character(.)), .fns = ~replace(., . =="-99", NA_character_)))

dat_baseline <- dat_baseline %>% 
  rename(participant_id = External_Data_Reference,
         baseline_charity_choice = CharityChoice,
         baseline_product_choice = ProductChoice,
         grades_hs = DGhs_grades0)

###############################################################################
# Gender and race variables
###############################################################################

dat_baseline <- dat_baseline %>%
  mutate(DGgender0 = replace(DGgender0, DGgender0==4, NA_real_)) %>%
  mutate(is_female = if_else(DGgender0==1, 1, 0),
         is_male = if_else(DGgender0==0, 1, 0),
         is_white = DGrace_white0)

###############################################################################
# Baseline alcohol use variables
###############################################################################

dat_baseline <- dat_baseline %>%
  mutate(typical_num_drinks_per_day = replace(ALCdrinkqty_30days0, Freqalc1==0, 0),
         tot_days_with_any_drinks = Freqalc1)

###############################################################################
# Create scores
###############################################################################

dat_baseline <- dat_baseline %>%
  # Reverse score items in preparation for the calculation of an overall stress score
  mutate(MH4pss0RC = abs(MH4pss0-4),
         MH5pss0RC = abs(MH5pss0-4),
         MH7pss0RC = abs(MH7pss0-4),
         MH8pss0RC = abs(MH8pss0-4)) %>%
  mutate(anxiety = MH1gad0 + MH2gad0 + MH3gad0 + MH4gad0 + MH5gad0 + MH6gad0 + MH7gad0,
         depression = MH1phq0 + MH2phq0 + MH3phq0 + MH4phq0 + MH5phq0 + MH6phq0 + MH7phq0 + MH8phq0 + MH9phq0,
         stress = MH1pss0 + MH2pss0 + MH3pss0 + MH4pss0RC + MH5pss0RC + MH6pss0 + MH7pss0RC + MH8pss0RC + MH9pss0 + MH10pss0) %>%
  rename(baseline_anxiety = anxiety,
         baseline_depression = depression,
         baseline_stress = stress)

dat_baseline <- dat_baseline %>%
  mutate(PBSSHarmReduction = PROTECT1pbss0 + PROTECT7pbss0 + PROTECT8pbss0 + PROTECT15pbss0 + PROTECT16pbss0 + PROTECT17pbss0 + PROTECT19pbss0 + PROTECT20pbss0,
         PBSSLimitStop = PROTECT2pbss0 + PROTECT3pbss0 + PROTECT4pbss0 + PROTECT6pbss0 + PROTECT9pbss0 + PROTECT10pbss0 + PROTECT11pbss0,
         PBSSManner = PROTECT5pbss0 + PROTECT12pbss0 + PROTECT13pbss0 + PROTECT14pbss0 + PROTECT18pbss0) %>%
    mutate(PBSSOverall = PBSSHarmReduction + PBSSLimitStop + PBSSManner)


dat_baseline <- dat_baseline %>%
  mutate(PBSSOverall = PBSSHarmReduction + PBSSLimitStop + PBSSManner)

###############################################################################
# Save data files
###############################################################################

dat_baseline <- dat_baseline %>% 
  select(participant_id, 
         baseline_charity_choice, baseline_product_choice,
         baseline_anxiety, baseline_depression, baseline_stress,
         tot_days_with_any_drinks, typical_num_drinks_per_day, 
         is_female, is_male, is_white,
         grades_hs,
         PBSSHarmReduction, PBSSLimitStop, PBSSManner, PBSSOverall)

save(dat_baseline, file = file.path(path_staged_data, "dat_baseline.RData"))

