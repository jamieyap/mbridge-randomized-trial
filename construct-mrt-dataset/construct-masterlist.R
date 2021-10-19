library(dplyr)
library(readxl)

source("paths.R")

###############################################################################
# Read in raw data files 'M-Bridge Pin Master List.xlsx' and 
# 'Stage 2 intervention groups 21.02.01.xlsx'
#
# Note: The file M-Bridge Pin Master List.xlsx contains a list of all 
# particiant ID's in the SMART and whether they were randomized to the
# control condition, or the experimental (i.e., 'Experimental Early' or
# 'Experimental Late') condition.
# 
# Note: The file 'Stage 2 intervention groups 21.02.01.xlsx' contains
# a list of participant ID's which were flagged by study staff at the 1st,
# 2nd, 3rd, 4th decision point as heavy drinkers.
#
# Note: We need to cross-match participant ID's between 'M-Bridge Pin Master List.xlsx'
# and 'Stage 2 intervention groups 21.02.01.xlsx' to identify those individuals
# who were never flagged as heavy drinkers by study staff at any of the four 
# decision points. Such individuals who have never been flagged would have
# received an invitation to self-monitor at each of the four decision points.
###############################################################################

dat_pin_masterlist <- read_xlsx(path = file.path(path_input_data, "M-Bridge Pin Master List.xlsx"), sheet = "M-Bridge Master List", col_types = c("numeric","text"))
dat_nonres_SM1 <- read_xlsx(path = file.path(path_input_data, "Stage 2 intervention groups 21.02.01.xlsx"), sheet = "SM 1", col_types = c("numeric","text","text","text","date","text"))
dat_nonres_SM2 <- read_xlsx(path = file.path(path_input_data, "Stage 2 intervention groups 21.02.01.xlsx"), sheet = "SM 2", col_types = c("numeric","text","text","text","date","text"))
dat_nonres_SM3 <- read_xlsx(path = file.path(path_input_data, "Stage 2 intervention groups 21.02.01.xlsx"), sheet = "SM 3", col_types = c("numeric","text","text","text","date","text"))
dat_nonres_SM4 <- read_xlsx(path = file.path(path_input_data, "Stage 2 intervention groups 21.02.01.xlsx"), sheet = "Sm 4", col_types = c("numeric","text","text","text","date","text"))

dat_nonres_SM1 <- dat_nonres_SM1 %>% filter(PIN != 3112) # remove row where an error in the raw data records occurred
dat_nonres_all <- rbind(dat_nonres_SM1, dat_nonres_SM2, dat_nonres_SM3, dat_nonres_SM4)

###############################################################################
# Begin data preparation steps
###############################################################################

dat_masterlist_wide <- dat_pin_masterlist %>% 
  # Drop individuals who were randomized to the control condition
  # That is, only retain individuals who were randomized to either
  # 'Experimental Early' or 'Experimental Late'
  filter(Group == "Experimental Early" | Group == "Experimental Late") %>%
  rename(participant_id = Pin, group = Group)

dat_masterlist_wide <- dat_nonres_all %>%
  select(PIN, `SM flagged`) %>%
  rename(participant_id = PIN,
         sm_flagged = `SM flagged`) %>%
  right_join(x = ., y = dat_masterlist_wide, by = "participant_id")

###############################################################################
# For each decision point, create an indicator for whether randomization
# should occur; this indicator is named 'coinflip'.
#
# Note: All participants randomized to the experimental condition of the SMART
# (i.e., 'Experimental Early'/'Experimental Late') were eligible for the 1st 
# coinflip in the micro-randomized trial (MRT).
#
# Some participants did not meet criteria to continue in the MRT at the 2nd
# decision point and were not randomized at the 2nd decision point. These
# participants consisted of those individuals who reported heavy drinking 
# at the 1st decision point. If an individual did not self-monitor
# at the 1st decision point (and hence did not provide information on drinking
# behavior at the 1st decision point), they were deemed eligible to continue
# in the MRT at the 2nd decision point and hence were randomized at the 
# 2nd decision point.
###############################################################################

dat_masterlist_wide <- dat_masterlist_wide %>%
  mutate(coinflip_1 = 1) %>%
  mutate(coinflip_2 = case_when(
    sm_flagged == "SM 1" ~ 0,
    is.na(sm_flagged) ~ 1,
    TRUE ~ 1
  )) %>%
  mutate(coinflip_3 = case_when(
    sm_flagged == "SM 1" ~ 0,
    sm_flagged == "SM 2" ~ 0,
    is.na(sm_flagged) ~ 1,
    TRUE ~ 1
  )) %>%
  mutate(coinflip_4 = case_when(
    sm_flagged == "SM 1" ~ 0,
    sm_flagged == "SM 2" ~ 0,
    sm_flagged == "SM 3" ~ 0,
    is.na(sm_flagged) ~ 1,
    TRUE ~ 1
  ))

dat_masterlist_wide <- dat_masterlist_wide %>% 
  arrange(sm_flagged, coinflip_1, coinflip_2, coinflip_3, coinflip_4)

###############################################################################
# Reshape from wide to long
###############################################################################

dat_01 <- dat_masterlist_wide %>% 
  mutate(decision_point = 1) %>%
  select(participant_id, decision_point, sm_flagged, group, coinflip_1) %>%
  rename(coinflip = coinflip_1)

dat_02 <- dat_masterlist_wide %>% 
  mutate(decision_point = 2) %>%
  select(participant_id, decision_point, sm_flagged, group, coinflip_2) %>%
  rename(coinflip = coinflip_2)

dat_03 <- dat_masterlist_wide %>% 
  mutate(decision_point = 3) %>%
  select(participant_id, decision_point, sm_flagged, group, coinflip_3) %>%
  rename(coinflip = coinflip_3)

dat_04 <- dat_masterlist_wide %>% 
  mutate(decision_point = 4) %>%
  select(participant_id, decision_point, sm_flagged, group, coinflip_4) %>%
  rename(coinflip = coinflip_4)

dat_masterlist_long <- rbind(dat_01, dat_02, dat_03, dat_04)
dat_masterlist_long <- dat_masterlist_long %>% arrange(sm_flagged, participant_id)

###############################################################################
# How many days elapsed since entering?
###############################################################################

dat_masterlist_long <- dat_masterlist_long %>%
  mutate(when_entered_hrts = case_when(
    group == "Experimental Early" ~ "2019-09-10 00:00:00",
    group == "Experimental Late" ~ "2019-09-18 00:00:00",
    TRUE ~ NA_character_
  )) %>%
  mutate(when_entered_hrts = strptime(when_entered_hrts, format = "%Y-%m-%d %H:%M:%S", tz = "US/Eastern"))

###############################################################################
# Save data files
###############################################################################

# Note: At this point, the data frame dat_masterlist will not contain any
# information of participants who were randomized to the control condition.
# 
# Note: dat_masterlist is a data frame that contains the complete listing of
# participant ID's who were randomized to the experimental condition of the SMART
# (i.e., 'Experimental Early'/'Experimental Late').
# Moreover, dat_masterlist is in long format. That is, each participant ID
# has 4 rows, corresponding to the four possible decision points
# within the micro-randomized trial (MRT).

dat_masterlist <- dat_masterlist_long
save(dat_masterlist, file = file.path(path_staged_data, "dat_masterlist.RData"))

