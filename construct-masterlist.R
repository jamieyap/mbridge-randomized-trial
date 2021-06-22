library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

###############################################################################
# Read in raw data files
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
  filter(Group == "Experimental Early" | Group == "Experimental Late") %>%
  rename(participant_id = Pin, group = Group)

dat_masterlist_wide <- dat_nonres_all %>%
  select(PIN, `SM flagged`) %>%
  rename(participant_id = PIN,
         sm_flagged = `SM flagged`) %>%
  right_join(x = ., y = dat_masterlist_wide, by = "participant_id")

###############################################################################
# For each decision point, create an indicator for whether randomization
# should occur; this indicator is named 'coinflip'
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
  mutate(when_entered_hrts = strptime(when_entered_hrts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

###############################################################################
# Save data files
###############################################################################

dat_masterlist <- dat_masterlist_long
save(dat_masterlist, file = file.path(path_staged_data, "dat_masterlist.RData"))

