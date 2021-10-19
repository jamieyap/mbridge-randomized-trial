library(dplyr)
library(readxl)
library(lubridate)

source("paths.R")

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
# Re-code different variations of product (e.g., 'Product 1', 'Product 2')
# and different variations of charity (e.g., 'Charity 1', 'Charity 2')
# as 'Product' and 'Charity', respectively.
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
# A participant will be excluded from all analyses if they
# did not select their preferred product and/or charity at baseline
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
# Construct an indicator to tell whether an individual does not have any
# missing data in any of the baseline variables indicated below
###############################################################################

# Variables for the primary analysis
check_these_vars <- c("tot_days_with_any_drinks", 
                      "typical_num_drinks_per_day",
                      "is_female", 
                      "is_male", 
                      "is_white_only")

reported_these_vars <- dat_masterlist %>% 
  select(all_of(check_these_vars)) %>%
  complete.cases(.)

dat_masterlist[["did_not_report_primary_analysis_vars"]] <- -1 * (-1 + 1*reported_these_vars)

# Variables for the secondary analysis
check_these_vars <- c("tot_days_with_any_drinks", 
                      "typical_num_drinks_per_day",
                      "is_female", 
                      "is_male", 
                      "is_white_only",
                      "baseline_anxiety", 
                      "baseline_depression", 
                      "baseline_stress")

reported_these_vars <- dat_masterlist %>% 
  select(all_of(check_these_vars)) %>%
  complete.cases(.)

dat_masterlist[["did_not_report_secondary_analysis_vars"]] <- -1 * (-1 + 1*reported_these_vars)

if(FALSE){
  # Count the number of individuals who will be excluded from primary analysis
  dat_masterlist %>%
    filter(decision_point==1) %>%
    select(participant_id, exclude_from_all, did_not_report_primary_analysis_vars) %>%
    group_by(exclude_from_all, did_not_report_primary_analysis_vars) %>%
    summarise(count = n(), .groups = "keep")
}

if(FALSE){
  # Count the number of individuals who will be excluded from secondary analysis
  dat_masterlist %>%
    filter(decision_point==1) %>%
    select(participant_id, exclude_from_all, did_not_report_secondary_analysis_vars) %>%
    group_by(exclude_from_all, did_not_report_secondary_analysis_vars) %>%
    summarise(count = n(), .groups = "keep")
}

###############################################################################
# Are there instances when the participant was supposed to be randomized
# but was not?
###############################################################################

if(FALSE){
  # The summary statistics calculated in the following lines of code show that
  # there are only two such cases
  dat_masterlist %>%
    filter(exclude_from_all==0) %>%
    filter(coinflip==1) %>%
    group_by(decision_point) %>%
    summarise(count = sum(is.na(randassign_invite)))
}

dat_masterlist <- dat_masterlist %>%
  mutate(coinflip = replace(coinflip, 
                            (exclude_from_all==0) & 
                              (coinflip==1) & 
                              (is.na(randassign_invite)), 
                            0))

###############################################################################
# Construct indicator for whether an individual did not self-monitor at the
# current decision point k and was considered eligible for randomization at
# the following decision point k+1
###############################################################################

dat_masterlist <- dat_masterlist %>% 
  group_by(participant_id) %>%
  mutate(ALCdrink_lagminusone = c(NA_real_, head(ALCdrink, n=-1))) %>%
  mutate(is_missing_ALCdrink_lagminusone = is.na(ALCdrink_lagminusone))

if(FALSE){
  dat_masterlist %>%
    filter(decision_point==2) %>%
    group_by(coinflip, is_missing_ALCdrink_lagminusone) %>%
    summarise(count = n(), .groups = "keep")
  
  dat_masterlist %>%
    filter(decision_point==3) %>%
    group_by(coinflip, is_missing_ALCdrink_lagminusone) %>%
    summarise(count = n(), .groups = "keep")
  
  dat_masterlist %>%
    filter(decision_point==4) %>%
    group_by(coinflip, is_missing_ALCdrink_lagminusone) %>%
    summarise(count = n(), .groups = "keep")
}

###############################################################################
# Work with time variables: 
# - Calculate days elapsed since entering the MRT
# - Transform human-readable format time variables to UNIX time format
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
  mutate(when_entered_hrts_UTC = with_tz(when_entered_hrts, "UTC"),
         randtime_invite_hrts_UTC = with_tz(randtime_invite_hrts, "UTC"),
         begintime_hrts_UTC = with_tz(begintime_hrts, "UTC"),
         endtime_hrts_UTC = with_tz(endtime_hrts, "UTC"),
         randtime_first_reminder_hrts_UTC = with_tz(randtime_first_reminder_hrts, "UTC")) %>%
  mutate(when_entered_unixts = as.numeric(when_entered_hrts_UTC),
         randtime_invite_unixts = as.numeric(randtime_invite_hrts_UTC),
         begintime_unixts = as.numeric(begintime_hrts_UTC),
         endtime_unixts = as.numeric(endtime_hrts_UTC),
         randtime_first_reminder_unixts = as.numeric(randtime_first_reminder_hrts_UTC)) 

###############################################################################
# Sanity checks
###############################################################################

if(FALSE){
  dat_masterlist %>%
    mutate(compare = randtime_invite_unixts > randtime_first_reminder_unixts) %>%
    # output must be equal to zero if no issue
    summarise(count = sum(compare, na.rm=TRUE)) %>% 
    summarise(max_count = max(count))
}

###############################################################################
# Save data
###############################################################################

dat_merged <- dat_masterlist
save(dat_merged, file = file.path(path_staged_data, "dat_merged.RData"))

