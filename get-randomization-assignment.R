library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

###############################################################################
# Read in pre-processed data files
###############################################################################

load(file = file.path(path_staged_data, "dat_availability_wide.RData"))
load(file = file.path(path_staged_data, "dat_availability_long.RData"))

load(file = file.path(path_staged_data, "dat_rand_invite_sm1.RData"))
load(file = file.path(path_staged_data, "dat_rand_invite_sm2.RData"))
load(file = file.path(path_staged_data, "dat_rand_invite_sm3.RData"))
load(file = file.path(path_staged_data, "dat_rand_invite_sm4.RData"))

###############################################################################
# Merge randomization assignment to wide data
###############################################################################

current_cols <- colnames(dat_availability_wide)
dat_availability_wide <- dat_rand_invite_sm1 %>%
  rename(randassign_1 = randassign,
         randtime_1 = randtime,
         status_1 = status) %>%
  right_join(x = ., y = dat_availability_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

current_cols <- colnames(dat_availability_wide)
dat_availability_wide <- dat_rand_invite_sm2 %>%
  rename(randassign_2 = randassign,
         randtime_2 = randtime,
         status_2 = status) %>%
  right_join(x = ., y = dat_availability_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

current_cols <- colnames(dat_availability_wide)
dat_availability_wide <- dat_rand_invite_sm3 %>%
  rename(randassign_3 = randassign,
         randtime_3 = randtime,
         status_3 = status) %>%
  right_join(x = ., y = dat_availability_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

current_cols <- colnames(dat_availability_wide)
dat_availability_wide <- dat_rand_invite_sm4 %>%
  rename(randassign_4 = randassign,
         randtime_4 = randtime,
         status_4 = status) %>%
  right_join(x = ., y = dat_availability_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

dat_availability_wide <- dat_availability_wide %>%
  arrange(desc(exclude_from_all), availability_1, availability_2, availability_3, availability_4, sm_flagged)

###############################################################################
# Update long data
###############################################################################

# What is the randomization assignment?
dat_availability_long <- dat_availability_wide %>%
  select(participant_id, randassign_1) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

dat_availability_long <- dat_availability_wide %>%
  select(participant_id, randassign_2) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

dat_availability_long <- dat_availability_wide %>%
  select(participant_id, randassign_3) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

dat_availability_long <- dat_availability_wide %>%
  select(participant_id, randassign_4) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

# When was the randomization performed?
dat_availability_long <- dat_availability_wide %>%
  select(participant_id, randtime_1) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

dat_availability_long <- dat_availability_wide %>%
  select(participant_id, randtime_2) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

dat_availability_long <- dat_availability_wide %>%
  select(participant_id, randtime_3) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

dat_availability_long <- dat_availability_wide %>%
  select(participant_id, randtime_4) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

# Was the self-monitoring survey associated with a particular
# randomization assignment completed?
dat_availability_long <- dat_availability_wide %>%
  select(participant_id, status_1) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

dat_availability_long <- dat_availability_wide %>%
  select(participant_id, status_2) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

dat_availability_long <- dat_availability_wide %>%
  select(participant_id, status_3) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

dat_availability_long <- dat_availability_wide %>%
  select(participant_id, status_4) %>%
  right_join(x = ., y = dat_availability_long, by = "participant_id")

###############################################################################
# Merge information from each survey_number into one variable
###############################################################################

dat_availability_long <- dat_availability_long %>%
  mutate(randassign = case_when(
    survey_number == 1 ~ randassign_1,
    survey_number == 2 ~ randassign_2,
    survey_number == 3 ~ randassign_3,
    survey_number == 4 ~ randassign_4,
    TRUE ~ NA_character_
  ))

dat_availability_long <- dat_availability_long %>%
  mutate(randtime = case_when(
    survey_number == 1 ~ randtime_1,
    survey_number == 2 ~ randtime_2,
    survey_number == 3 ~ randtime_3,
    survey_number == 4 ~ randtime_4,
    TRUE ~ as.POSIXlt(NA)
  ))

dat_availability_long <- dat_availability_long %>%
  mutate(status = case_when(
    survey_number == 1 ~ status_1,
    survey_number == 2 ~ status_2,
    survey_number == 3 ~ status_3,
    survey_number == 4 ~ status_4,
    TRUE ~ as.character(NA)
  ))

dat_availability_long <- dat_availability_long %>%
  select(-randassign_1, -randassign_2, -randassign_3, -randassign_4,
         -randtime_1, -randtime_2, -randtime_3, -randtime_4,
         -status_1, -status_2, -status_3, -status_4)

dat_availability_long <- dat_availability_long %>% 
  arrange(desc(exclude_from_all), participant_id)

###############################################################################
# Save data files
###############################################################################

dat_rand_long <- dat_availability_long
dat_rand_wide <- dat_availability_wide

save(dat_rand_long, file = file.path(path_staged_data, "dat_rand_long.RData"))
save(dat_rand_wide, file = file.path(path_staged_data, "dat_rand_wide.RData"))


