library(dplyr)
library(readxl)

source("paths.R")

###############################################################################
# Load previously parsed data
###############################################################################

load(file.path(path_staged_data, "dat_merged.RData"))

###############################################################################
# Sanity checks
###############################################################################

if(FALSE){
  # How many unique participant ID's are present in the data frame dat_merged?
  # Output should be 591
  length(unique(dat_merged[["participant_id"]]))
}

###############################################################################
# Calculate hours elapsed between events
#
# hrs_elapsed_invite_to_first_reminder --  hours elapsed between delivery of
# invitation and when the first reminder occurred
#
# hrs_elapsed_between_dp -- hours elapsed between randomization just prior to
# the k^{th} invitation and the randomization just prior to 
# the (k+1)^{th} invitation
#
# hrs_to_begin_survey -- hours elapsed between delivery of invitation and
# when the participant responded to the survey
###############################################################################

dat_merged <- dat_merged %>%
  group_by(participant_id) %>%
  mutate(randtime_invite_unixts_lagplusone = c(tail(randtime_invite_unixts, n=-1),NA))

dat_merged <- dat_merged %>%
  mutate(hrs_elapsed_invite_to_first_reminder = (randtime_first_reminder_unixts - randtime_invite_unixts)/(60*60),
         hrs_elapsed_between_dp = (randtime_invite_unixts_lagplusone - randtime_invite_unixts)/(60*60),
         hrs_to_begin_survey = (begintime_unixts - randtime_invite_unixts)/(60*60))

###############################################################################
# Calculate summary statistics in preparation for constructing primary outcome
###############################################################################

if(FALSE){
  # What is the minimum and maximum hours elapsed between invite and first reminder?
  dat_merged %>%
    filter(exclude_from_all==0) %>%
    filter(coinflip==1) %>%
    group_by(decision_point) %>%
    summarise(minimum_hrs_elapsed = min(hrs_elapsed_invite_to_first_reminder, na.rm=TRUE),
              maximum_hrs_elapsed = max(hrs_elapsed_invite_to_first_reminder, na.rm=TRUE),
              q50 = quantile(hrs_elapsed_invite_to_first_reminder, probs = .50, na.rm=TRUE))
}

if(FALSE){
  # What is the minimum and maximum number of hours elapsed between two consecutive invites?
  dat_merged %>%
    filter(exclude_from_all==0) %>%
    filter(coinflip==1) %>%
    filter(decision_point!=4) %>%
    group_by(decision_point) %>%
    summarise(min_hrs_elapsed = min(hrs_elapsed_between_dp, na.rm=TRUE),
              max_hrs_elapsed = max(hrs_elapsed_between_dp, na.rm=TRUE),
              q50 = quantile(hrs_elapsed_between_dp, probs = .50, na.rm=TRUE))
}

if(FALSE){
  # What is the minimum and maximum number of hours to begin a survey from
  # when an invite was delivered?
  dat_merged %>%
    filter(exclude_from_all==0) %>%
    filter(coinflip==1) %>%
    group_by(decision_point) %>%
    summarise(minimum_hrs = min(hrs_to_begin_survey, na.rm=TRUE),
              maximum_hrs = max(hrs_to_begin_survey, na.rm=TRUE),
              q50 = quantile(hrs_to_begin_survey, probs = .50, na.rm=TRUE),
              q99 = quantile(hrs_to_begin_survey, probs = .99, na.rm=TRUE))
}

if(FALSE){
  dat_merged  %>%
    # Construct indicator for whether survey associated with decision point k was
    # responded to by the participant more than 14 days after invite was delivered
    mutate(is_responded_after_14days = 1*(hrs_to_begin_survey > 14*24)) %>%
    filter(exclude_from_all==0) %>%
    filter(coinflip==1) %>%
    filter(!is.na(is_responded_after_14days)) %>%
    mutate(not_miss = 1*(!is.na(ALCdrink))) %>%
    group_by(decision_point, is_responded_after_14days, not_miss) %>%
    summarise(count = n(), .groups = "keep") %>%
    # Of those surveys having is_responded_after_14days==1,
    # how many had a value for ALCdrink?
    filter(is_responded_after_14days == 1 & not_miss == 1)
}

###############################################################################
# Construct the outcome variable:
# Create indicators for whether the participant responded to the alcohol use
# question ALCdrink within DELTA hours of the coin flip
###############################################################################

# From the above summary statistics, select DELTA = 47
dat_merged <- dat_merged %>%
  mutate(responded_to_alcdrink_item = if_else(!is.na(ALCdrink), 1, 0)) %>%
  mutate(Y_delta47 = case_when(
    (responded_to_alcdrink_item == 1) & (!is.na(hrs_to_begin_survey)) & (hrs_to_begin_survey < 47) ~ 1, 
    (responded_to_alcdrink_item == 1) & (!is.na(hrs_to_begin_survey)) & (hrs_to_begin_survey >= 47) ~ 0, 
    TRUE ~ 0
  ))

###############################################################################
# Sanity checks
###############################################################################

if(FALSE){
  dat_merged %>%
    filter(exclude_from_all==0) %>%
    filter(coinflip==1) %>%
    group_by(decision_point) %>%
    summarise(count_randomized = n(),
              proportion_responded_within47hours = mean(Y_delta47),
              proportion_randomized_to_product = mean(1*(randassign_invite=="Product")))
}

if(FALSE){
  dat_merged %>%
    filter(exclude_from_all==0) %>%
    filter(coinflip==1) %>%
    filter(did_not_report_primary_analysis_vars==0) %>%
    group_by(decision_point) %>%
    summarise(count_randomized = n(),
              proportion_responded_within47hours = mean(Y_delta47),
              proportion_randomized_to_product = mean(1*(randassign_invite=="Product")))
}

if(FALSE){
  dat_merged %>%
    filter(exclude_from_all==0) %>%
    filter(coinflip==1) %>%
    filter(did_not_report_secondary_analysis_vars==0) %>%
    group_by(decision_point) %>%
    summarise(count_randomized = n(),
              proportion_responded_within47hours = mean(Y_delta47),
              proportion_randomized_to_product = mean(1*(randassign_invite=="Product")))
}

###############################################################################
# Save data
###############################################################################

dat_analysis <- dat_merged
save(dat_analysis, file = file.path(path_staged_data, "dat_analysis.RData"))

