library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

###############################################################################
# Load previously parsed data
###############################################################################

load(file.path(path_staged_data, "dat_masterlist.RData"))
load(file.path(path_staged_data, "dat_baseline.RData"))
load(file.path(path_staged_data, "dat_invite.RData"))
load(file.path(path_staged_data, "dat_reminder.RData"))
load(file.path(path_staged_data, "dat_survey.RData"))

###############################################################################
# If the participant had not selected a product or charity of their choice at 
# baseline, re-code as 'none'
###############################################################################

dat_baseline <- dat_baseline %>% 
  mutate(baseline_charity_choice = replace(baseline_charity_choice, 
                                           baseline_charity_choice == "Charity of your future choice", 
                                           "none"),
         baseline_product_choice = replace(baseline_product_choice, 
                                           baseline_product_choice == "Product of your future choice", 
                                           "none"))

###############################################################################
# Re-code 'No Charity' or 'No Product' as 'none'
###############################################################################

use_dat_invite <- dat_invite %>% 
  mutate(randassign = replace(randassign, randassign == "No Charity", "none")) %>%
  mutate(randassign = replace(randassign, randassign == "No Product", "none")) %>%
  rename(randassign_invite = randassign,
         randtime_invite_hrts = randtime_hrts) %>%
  select(-status)

use_dat_first_reminder <- dat_reminder %>%
  mutate(randassign = replace(randassign, randassign == "No Charity", "none")) %>%
  mutate(randassign = replace(randassign, randassign == "No Product", "none")) %>%
  mutate(randassign = replace(randassign, randassign == "No charity", "none")) %>%
  mutate(randassign = replace(randassign, randassign == "No product", "none")) %>%
  mutate(randassign = replace(randassign, randassign == "Charity 1", "Charity")) %>%
  mutate(randassign = replace(randassign, randassign == "Charity 2", "Charity")) %>%
  mutate(randassign = replace(randassign, randassign == "Product 1", "Product")) %>%
  mutate(randassign = replace(randassign, randassign == "Product 2", "Product")) %>%
  filter(reminder_number == 1) %>%
  mutate(randassign = substring(text = randassign, first = 1, last = 7)) %>%
  rename(randassign_first_reminder = randassign,
         randtime_first_reminder_hrts = randtime_hrts) %>%
  select(-status, -reminder_number, -reminder_total)

###############################################################################
# Merge data frames
###############################################################################

dat_masterlist <- left_join(x = dat_masterlist, y = dat_baseline, by = c("participant_id"))
dat_masterlist <- left_join(x = dat_masterlist, y = use_dat_invite, by = c("participant_id", "decision_point"))
dat_masterlist <- left_join(x = dat_masterlist, y = use_dat_first_reminder, by = c("participant_id", "decision_point"))
dat_masterlist <- left_join(x = dat_masterlist, y = dat_survey, by = c("participant_id", "decision_point"))

dat_masterlist <- dat_masterlist %>% arrange(sm_flagged, participant_id)

###############################################################################
# Sanity checks
###############################################################################

if(FALSE){
  # Checks: all rows having 'none' for the variables
  # 'randassign_invite' and 'randassign_first_reminder' actually correspond to 
  # those participants who did not specify a charity or product of their choice
  # at baseline, i.e., the value of the variable 'baseline_charity_choice'
  # is none and the value of the variable 'baseline_product_choice' is none
  
  dat_masterlist %>% filter(randassign_invite == "none") %>% View(.)
  dat_masterlist %>% filter(randassign_first_reminder == "none") %>% View(.)
  
  # Checks: for those participants who did not specify a charity or 
  # product of their choice at baseline, i.e., the value of the variable 
  # 'baseline_charity_choice' is none and the value of the variable 
  # 'baseline_product_choice' is none, were they randomized to any particular
  # product or charity after baseline?
  
  dat_masterlist %>% filter(baseline_charity_choice == "none") %>% View(.)
  dat_masterlist %>% filter(baseline_product_choice == "none") %>% View(.)
}

###############################################################################
# Construct a new variable, 'exclude_from_all', an indicator for whether
# a participant will be dropped from all analysis
###############################################################################

dat_masterlist <- dat_masterlist %>%
  mutate(exclude_from_all = case_when(
    baseline_charity_choice=="none" ~ 1,
    baseline_product_choice=="none" ~ 1,
    TRUE ~ 0
  ))

dat_masterlist <- dat_masterlist %>% 
  select(participant_id, exclude_from_all, 
         group, baseline_charity_choice, baseline_product_choice,
         sm_flagged, everything()) %>%
  arrange(desc(exclude_from_all), sm_flagged, participant_id)

###############################################################################
# Work with time variables: transform human-readable format time variables to
# UNIX time format
###############################################################################

dat_masterlist <- dat_masterlist %>%
  mutate(when_entered_unixts = as.numeric(when_entered_hrts),
         randtime_invite_unixts = as.numeric(randtime_invite_hrts),
         begintime_unixts = as.numeric(begintime_hrts),
         endtime_unixts = as.numeric(endtime_hrts),
         randtime_first_reminder_unixts = as.numeric(randtime_first_reminder_hrts)) 

###############################################################################
# Sanity checks
###############################################################################

if(FALSE){
  dat_masterlist %>%
    mutate(compare = randtime_invite_unixts > randtime_first_reminder_unixts) %>%
    # output must be equal to zero if no issue
    summarise(sum(compare, na.rm=TRUE))
}

###############################################################################
# Construct lagged time variables and calculate hours elapsed between events
###############################################################################

dat_masterlist <- dat_masterlist %>%
  mutate(days_elapsed_since_entering = case_when(
    group=="Experimental Early" & decision_point==1 ~ 0,
    group=="Experimental Early" & decision_point==2 ~ 14,
    group=="Experimental Early" & decision_point==3 ~ 28,
    group=="Experimental Early" & decision_point==4 ~ 42,
    group=="Experimental Late" & decision_point==1 ~ 14+0,
    group=="Experimental Late" & decision_point==2 ~ 14+14,
    group=="Experimental Late" & decision_point==3 ~ 14+28,
    group=="Experimental Late" & decision_point==4 ~ 14+42,
    TRUE ~ NA_real_
  ))

dat_masterlist <- dat_masterlist %>%
  group_by(participant_id) %>%
  mutate(randtime_invite_unixts_lagplusone = c(tail(randtime_invite_unixts, n=-1),NA))

dat_masterlist <- dat_masterlist %>%
  mutate(hrs_elapsed_invite_to_begin_survey = (begintime_unixts - randtime_invite_unixts)/(60*60),
         hrs_elapsed_invite_to_first_reminder = (randtime_first_reminder_unixts - randtime_invite_unixts)/(60*60),
         hrs_elapsed_between_dp = (randtime_invite_unixts_lagplusone - randtime_invite_unixts)/(60*60),
         hrs_to_complete_survey = (endtime_unixts - begintime_unixts)/(60*60))

###############################################################################
# Calculate summary statistics in preparation for constructing the outcome
###############################################################################

# What is the minimum and maximum hours elapsed between invite and first reminder?
dat_masterlist %>%
  filter(exclude_from_all==0) %>%
  filter(coinflip==1) %>%
  group_by(decision_point) %>%
  summarise(minimum_hrs_elapsed = min(hrs_elapsed_invite_to_first_reminder, na.rm=TRUE),
            maximum_hrs_elapsed = max(hrs_elapsed_invite_to_first_reminder, na.rm=TRUE))

# What is the minimum and maximum number of hours elapsed between two consecutive invites?
dat_masterlist %>%
  filter(exclude_from_all==0) %>%
  filter(coinflip==1) %>%
  filter(decision_point!=4) %>%
  group_by(decision_point) %>%
  summarise(min_hrs_elapsed = min(hrs_elapsed_between_dp, na.rm=TRUE),
            max_hrs_elapsed = max(hrs_elapsed_between_dp, na.rm=TRUE))

# What is the minimum and maximum number of hours elapsed between invite and beginning of survey?
dat_masterlist %>%
  filter(exclude_from_all==0) %>%
  filter(coinflip==1) %>%
  group_by(decision_point) %>%
  summarise(minimum_hrs_elapsed = min(hrs_elapsed_invite_to_begin_survey, na.rm=TRUE),
            maximum_hrs_elapsed = max(hrs_elapsed_invite_to_begin_survey, na.rm=TRUE),
            q50 = quantile(hrs_elapsed_invite_to_begin_survey, probs = .50, na.rm=TRUE),
            q95 = quantile(hrs_elapsed_invite_to_begin_survey, probs = .95, na.rm=TRUE),
            n_greater_than_two_weeks = sum(hrs_elapsed_invite_to_begin_survey > 14*24, na.rm = TRUE))

# What is the minimum and maximum number of hours to complete a survey?
dat_masterlist %>%
  filter(exclude_from_all==0) %>%
  filter(coinflip==1) %>%
  group_by(decision_point) %>%
  summarise(minimum_hrs = min(hrs_to_complete_survey, na.rm=TRUE),
            maximum_hrs = max(hrs_to_complete_survey, na.rm=TRUE),
            q50 = quantile(hrs_to_complete_survey, probs = .50, na.rm=TRUE),
            q95 = quantile(hrs_to_complete_survey, probs = .95, na.rm=TRUE),
            q98 = quantile(hrs_to_complete_survey, probs = .98, na.rm=TRUE),
            n_greater_than_01 = sum(hrs_to_complete_survey>1, na.rm=TRUE),
            n_greater_than_24 = sum(hrs_to_complete_survey>24, na.rm=TRUE),
            n_greater_than_62 = sum(hrs_to_complete_survey>62, na.rm=TRUE))

###############################################################################
# Construct an overall indicator for whether a row will be utilized to estimate
# treatment effects
###############################################################################
dat_masterlist <- dat_masterlist %>% ungroup(.)

# First, create an indicator for whether no missing data exist in any of the
# variables listed in check_these_vars
check_these_vars <- c("tot_days_with_any_drinks", "typical_num_drinks_per_day",
                      "is_female", "is_male", "is_white",
                      "baseline_anxiety", "baseline_depression", "baseline_stress")

reported_these_vars <- dat_masterlist %>% 
  select(all_of(check_these_vars)) %>%
  complete.cases(.)

# Add new column
dat_masterlist$reported_these_vars <- reported_these_vars*1

# Update exclude_from_all to account for additional exclusion criteria
dat_masterlist <- dat_masterlist %>%
  mutate(exclude_from_all = replace(exclude_from_all, reported_these_vars==0, 1))

# Second, are there instances when the participant was supposed to be randomized
# but was not?

# The summary statistics calculated in the following lines of code show that
# there are two such cases
dat_masterlist %>%
  filter(exclude_from_all==0) %>%
  filter(coinflip==1) %>%
  group_by(decision_point) %>%
  summarise(count = sum(is.na(randassign_invite)))

dat_masterlist <- dat_masterlist %>%
  mutate(coinflip = replace(coinflip, coinflip==1 & is.na(randassign_invite), 0))

###############################################################################
# Construct the outcome variable:
# Create indicators for whether the participant responded to the alcohol use
# question ALCdrink within DELTA hours of the coin flip
###############################################################################

# From the above summary statistics, select DELTA = 62
dat_masterlist <- dat_masterlist %>%
  mutate(Y_delta62 = if_else(!is.na(ALCdrink), 1, 0)) %>%
  mutate(Y_delta62 = replace(Y_delta62, hrs_elapsed_invite_to_begin_survey>62, 0))

###############################################################################
# Save data
###############################################################################

dat_analysis <- dat_masterlist
save(dat_analysis, file = file.path(path_staged_data, "dat_analysis.RData"))

