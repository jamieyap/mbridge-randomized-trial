library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

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
         is_flagged_by_04 = if_else(is_flagged_01==1 | is_flagged_02==1 | is_flagged_03==1 | is_flagged_04==1, 1, 0))

dat_new <- dat_analysis %>%
  group_by(participant_id) %>%
  summarise(is_never_flagged = unique(is_never_flagged),
            is_flagged_by_01 = unique(is_flagged_01),
            is_flagged_by_02 = unique(is_flagged_by_02),
            is_flagged_by_03 = unique(is_flagged_by_03),
            is_flagged_by_04 = unique(is_flagged_by_04),
            is_male = unique(is_male),
            is_white = unique(is_white),
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
dat_summary <- dat_new %>%
  summarise(n1 = sum(is_flagged_by_01),
            n2 = sum(is_flagged_by_02),
            n3 = sum(is_flagged_by_03),
            n4 = sum(is_flagged_by_04),
            n_never = sum(is_never_flagged),
            p1 = sum(is_flagged_by_01)/n(),
            p2 = sum(is_flagged_by_02)/n(),
            p3 = sum(is_flagged_by_03)/n(),
            p4 = sum(is_flagged_by_04)/n(),
            p_never = sum(is_never_flagged)/n())


###############################################################################
# Fit models: is_flagged_by_01
###############################################################################

mod_flagged_by_01a <- glm(is_flagged_by_01 ~ is_white + is_male + sPBSSOverall + stot_days_with_any_drinks, data = dat_new, family = "binomial")
#summary(mod_flagged_by_01a)

mod_flagged_by_01b <- glm(is_flagged_by_01 ~ is_white + is_male + sPBSSOverall + stypical_num_drinks_per_day, data = dat_new, family = "binomial")
#summary(mod_flagged_by_01b)

###############################################################################
# Fit models: is_flagged_by_02
###############################################################################

mod_flagged_by_02a <- glm(is_flagged_by_02 ~ is_white + is_male + sPBSSOverall + stot_days_with_any_drinks, data = dat_new, family = "binomial")
#summary(mod_flagged_by_02a)

mod_flagged_by_02b <- glm(is_flagged_by_02 ~ is_white + is_male + sPBSSOverall + stypical_num_drinks_per_day, data = dat_new, family = "binomial")
#summary(mod_flagged_by_02b)

###############################################################################
# Fit models: is_flagged_by_03
###############################################################################

mod_flagged_by_03a <- glm(is_flagged_by_03 ~ is_white + is_male + sPBSSOverall + stot_days_with_any_drinks, data = dat_new, family = "binomial")
#summary(mod_flagged_by_03a)

mod_flagged_by_03b <- glm(is_flagged_by_03 ~ is_white + is_male + sPBSSOverall + stypical_num_drinks_per_day, data = dat_new, family = "binomial")
#summary(mod_flagged_by_03b)

###############################################################################
# Fit models: is_flagged_by_04
###############################################################################

mod_flagged_by_04a <- glm(is_flagged_by_04 ~ is_white + is_male + sPBSSOverall + stot_days_with_any_drinks, data = dat_new, family = "binomial")
#summary(mod_flagged_by_04a)

mod_flagged_by_04b <- glm(is_flagged_by_04 ~ is_white + is_male + sPBSSOverall + stypical_num_drinks_per_day, data = dat_new, family = "binomial")
#summary(mod_flagged_by_04b)

