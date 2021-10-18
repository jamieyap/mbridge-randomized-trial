library(dplyr)
library(readxl)

source("paths.R")

###############################################################################
# Data preparation steps
###############################################################################

load(file.path(path_staged_data, "dat_analysis.RData"))
dat_analysis <- as.data.frame(dat_analysis)

dat_analysis <- dat_analysis %>% 
  filter(exclude_from_all == 0) %>% 
  mutate(randassign_invite = case_when(
    randassign_invite == "Product" ~ 1,
    randassign_invite == "Charity" ~ 0,
    TRUE ~ NA_real_)) %>%
  select(participant_id, decision_point, 
         randassign_invite, coinflip, Y_delta47,
         hrs_elapsed_invite_to_first_reminder, ALCdrink,
         everything())

dat_analysis <- dat_analysis %>%
  mutate(is_dp_early = if_else(decision_point==1 | decision_point==2, 1, 0),
         is_dp_late = if_else(decision_point==3 | decision_point==4, 1, 0))

###############################################################################
# More Data Preparation Steps
###############################################################################

dat_analysis <- dat_analysis %>%
  mutate(is_never_flagged = if_else(is.na(sm_flagged), 1, 0),
         is_flagged_01 = if_else(!is.na(sm_flagged) & sm_flagged=="SM 1", 1, 0),
         is_flagged_02 = if_else(!is.na(sm_flagged) & sm_flagged=="SM 2", 1, 0),
         is_flagged_03 = if_else(!is.na(sm_flagged) & sm_flagged=="SM 3", 1, 0),
         is_flagged_04 = if_else(!is.na(sm_flagged) & sm_flagged=="SM 4", 1, 0)) %>%
  mutate(is_flagged_by_02 = if_else(is_flagged_01==1 | is_flagged_02==1, 1, 0),
         is_flagged_by_03 = if_else(is_flagged_01==1 | is_flagged_02==1 | is_flagged_03==1, 1, 0),
         is_flagged_by_04 = if_else(is_flagged_01==1 | is_flagged_02==1 | is_flagged_03==1 | is_flagged_04==1, 1, 0)) %>%
  mutate(is_flagged_early = is_flagged_by_02,
         is_flagged_late = if_else(is_flagged_03==1 | is_flagged_04==1, 1, 0))

dat_new <- dat_analysis %>%
  group_by(participant_id) %>%
  summarise(is_never_flagged = unique(is_never_flagged),
            is_flagged_early = unique(is_flagged_early),
            is_flagged_late = unique(is_flagged_late),
            is_flagged_by_01 = unique(is_flagged_01),
            is_flagged_by_02 = unique(is_flagged_by_02),
            is_flagged_by_03 = unique(is_flagged_by_03),
            is_flagged_by_04 = unique(is_flagged_by_04),
            is_male = unique(is_male),
            is_white_only = unique(is_white_only),
            PBSSOverall = unique(PBSSOverall),
            grades_hs = unique(grades_hs),
            tot_days_with_any_drinks = unique(tot_days_with_any_drinks),
            typical_num_drinks_per_day = unique(typical_num_drinks_per_day)) 

dat_new <- dat_new %>%
  mutate(grades_hsAp = 1*(grades_hs==0),
         grades_hsAm = 1*(grades_hs==1))

dat_new <- dat_new %>% 
  mutate(sPBSSOverall = scale(PBSSOverall)[,1],
         stot_days_with_any_drinks = scale(tot_days_with_any_drinks)[,1],
         stypical_num_drinks_per_day = scale(typical_num_drinks_per_day)[,1])

###############################################################################
# Summary statistics
###############################################################################

table01 <- dat_new %>%
  group_by(is_flagged_early) %>%
  summarise(prop_male = mean(is_male),
            prop_white_only = mean(is_white_only),
            average_PBSSOverall = mean(PBSSOverall, na.rm=TRUE))

table02 <- dat_new %>%
  group_by(is_flagged_late) %>%
  summarise(prop_male = mean(is_male),
            prop_white_only = mean(is_white_only),
            average_PBSSOverall = mean(PBSSOverall, na.rm=TRUE))

table03 <- dat_new %>%
  group_by(is_never_flagged) %>%
  summarise(prop_male = mean(is_male),
            prop_white_only = mean(is_white_only),
            average_PBSSOverall = mean(PBSSOverall, na.rm=TRUE))

