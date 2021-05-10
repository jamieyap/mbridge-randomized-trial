library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

###############################################################################
# Read in pre-processed data files
###############################################################################

load(file = file.path(path_staged_data, "dat_availability_wide.RData"))
load(file = file.path(path_staged_data, "dat_availability_long.RData"))

###############################################################################
# Read in raw data files
###############################################################################

dat_paradata_sm1 <- read_xlsx(path = file.path(path_input_data, "SM Paradata_2.26.21.xlsx"), 
                              sheet = "SM1", na = "N/A", 
                              col_types = c("text", "numeric", "text", "text", "text", "date", "text", "text", "date", "date"))

dat_paradata_sm2 <- read_xlsx(path = file.path(path_input_data, "SM Paradata_2.26.21.xlsx"), 
                              sheet = "SM2", na = "N/A", 
                              col_types = c("text", "numeric", "text", "text", "text", "date", "date", "text", "date", "date"))

dat_paradata_sm3 <- read_xlsx(path = file.path(path_input_data, "SM Paradata_2.26.21.xlsx"), 
                              sheet = "SM3", na = "N/A", 
                              col_types = c("text", "numeric", "text", "text", "text", "date", "text", "text", "date", "date"))

dat_paradata_sm4 <- read_xlsx(path = file.path(path_input_data, "SM Paradata_2.26.21.xlsx"), 
                              sheet = "SM4", na = "N/A", 
                              col_types = c("text", "numeric", "text", "text", "text", "date", "text", "text", "date", "date"))

###############################################################################
# Initial data preparation tasks on paradata files
###############################################################################

dat_paradata_sm1 <- dat_paradata_sm1 %>% 
  rename(participant_id = Pin) %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(randtime = paste(Date, Time, sep=" ")) %>%
  mutate(randtime = strptime(x = randtime, format = "%Y-%m-%d %I:%M%p", tz = "UTC"))

dat_paradata_sm2 <- dat_paradata_sm2 %>%
  rename(participant_id = Pin) %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(Time = substring(text = Time, first = 12)) %>%
  mutate(randtime = paste(Date, Time, sep=" ")) %>%
  mutate(randtime = strptime(x = randtime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

dat_paradata_sm3 <- dat_paradata_sm3 %>%
  rename(participant_id = Pin) %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(randtime = paste(Date, Time, sep=" ")) %>%
  mutate(randtime = strptime(x = randtime, format = "%Y-%m-%d %I:%M%p", tz = "UTC"))

dat_paradata_sm4 <- dat_paradata_sm4 %>%
  rename(participant_id = Pin) %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(randtime = paste(Date, Time, sep=" ")) %>%
  mutate(randtime = strptime(x = randtime, format = "%Y-%m-%d %I:%M%p", tz = "UTC"))

###############################################################################
# Grab randomization assignment, if it exists
###############################################################################

dat_rand_invite_sm1 <- dat_paradata_sm1 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime) %>%
  rename(randassign = `Product or Charity`)

dat_rand_invite_sm1 <- dat_rand_invite_sm1 %>% arrange(participant_id, randtime)
# One participant ID was sent a reminder twice
# we only keep the earlier one
dat_rand_invite_sm1 <- dat_rand_invite_sm1 %>% filter(!duplicated(dat_rand_invite_sm1$participant_id))

dat_rand_invite_sm2 <- dat_paradata_sm2 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime) %>%
  rename(randassign = `Product or Charity`)

dat_rand_invite_sm2 <- dat_rand_invite_sm2 %>% arrange(participant_id, randtime)
# Each participant ID in this data frame was sent a reminder only once
# effectively, no rows are dropped at this filtering step
dat_rand_invite_sm2 <- dat_rand_invite_sm2 %>% filter(!duplicated(dat_rand_invite_sm2$participant_id))

dat_rand_invite_sm3 <- dat_paradata_sm3 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime) %>%
  rename(randassign = `Product or Charity`)

dat_rand_invite_sm3 <- dat_rand_invite_sm3 %>% arrange(participant_id, randtime)
# Each participant ID in this data frame was sent a reminder only once
# effectively, no rows are dropped at this filtering step
dat_rand_invite_sm3 <- dat_rand_invite_sm3 %>% filter(!duplicated(dat_rand_invite_sm3$participant_id))

dat_rand_invite_sm4 <- dat_paradata_sm4 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime) %>%
  rename(randassign = `Product or Charity`)

dat_rand_invite_sm4 <- dat_rand_invite_sm4 %>% arrange(participant_id, randtime)
# Each participant ID in this data frame was sent a reminder only once
# effectively, no rows are dropped at this filtering step
dat_rand_invite_sm4 <- dat_rand_invite_sm4 %>% filter(!duplicated(dat_rand_invite_sm4$participant_id))

###############################################################################
# Merge randomization assignment to wide data
###############################################################################

current_cols <- colnames(dat_availability_wide)
dat_availability_wide <- dat_rand_invite_sm1 %>%
  rename(randassign_1 = randassign,
         randtime_1 = randtime) %>%
  right_join(x = ., y = dat_availability_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

current_cols <- colnames(dat_availability_wide)
dat_availability_wide <- dat_rand_invite_sm2 %>%
  rename(randassign_2 = randassign,
         randtime_2 = randtime) %>%
  right_join(x = ., y = dat_availability_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

current_cols <- colnames(dat_availability_wide)
dat_availability_wide <- dat_rand_invite_sm3 %>%
  rename(randassign_3 = randassign,
         randtime_3 = randtime) %>%
  right_join(x = ., y = dat_availability_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

current_cols <- colnames(dat_availability_wide)
dat_availability_wide <- dat_rand_invite_sm4 %>%
  rename(randassign_4 = randassign,
         randtime_4 = randtime) %>%
  right_join(x = ., y = dat_availability_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

dat_availability_wide <- dat_availability_wide %>%
  arrange(desc(exclude_from_all), availability_1, availability_2, availability_3, availability_4, sm_flagged)

###############################################################################
# Update long data
###############################################################################

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
  select(-randassign_1, -randassign_2, -randassign_3, -randassign_4,
         -randtime_1, -randtime_2, -randtime_3, -randtime_4)

dat_availability_long <- dat_availability_long %>% 
  arrange(desc(exclude_from_all), participant_id)

###############################################################################
# Save data files
###############################################################################

dat_rand_long <- dat_availability_long
dat_rand_wide <- dat_availability_wide

save(dat_rand_long, file = file.path(path_staged_data, "dat_rand_long.RData"))
save(dat_rand_wide, file = file.path(path_staged_data, "dat_rand_wide.RData"))



