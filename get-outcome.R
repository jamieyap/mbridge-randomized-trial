library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

###############################################################################
# Read in pre-processed data files
###############################################################################

load(file = file.path(path_staged_data, "dat_rand_wide.RData"))
load(file = file.path(path_staged_data, "dat_rand_long.RData"))

###############################################################################
# Perform checks before brining in new data
###############################################################################

# Yields: 0
dat_rand_long %>% 
  filter(exclude_from_all == 0) %>% 
  filter(survey_number == 1) %>% 
  summarise(num_missing = sum(is.na(status)))

# Yields: 0
dat_rand_long %>% 
  filter(exclude_from_all == 0) %>% 
  filter(survey_number == 2 & availability == 1) %>% 
  summarise(num_missing = sum(is.na(status)))

# Yields: 1
dat_rand_long %>% 
  filter(exclude_from_all == 0) %>% 
  filter(survey_number == 3 & availability == 1) %>% 
  summarise(num_missing = sum(is.na(status)))

# Yields: 1
dat_rand_long %>% 
  filter(exclude_from_all == 0) %>% 
  filter(survey_number == 4 & availability == 1) %>% 
  summarise(num_missing = sum(is.na(status)))

###############################################################################
# Read in raw data from surveys
###############################################################################

dat_survey_sm1 <- read_xlsx(path = file.path(path_input_data, "SM1 Data_corrected4.21.21.xlsx"), sheet = "SM1 Data_4.20.21")
dat_survey_sm2 <- read.csv(file = file.path(path_input_data, "SM2 Data with updated PIN (6.5.20).csv"), header = TRUE, na.strings = "")
dat_survey_sm3 <- read_xlsx(path = file.path(path_input_data, "SM3 Data_corrected4.21.21.xlsx"), sheet = "SM3 Data (3.4.20)")
dat_survey_sm4 <- read.csv(file = file.path(path_input_data, "SM4 Data (3.4.20).csv"), header = TRUE, na.strings = "")

###############################################################################
# Columns named 'Externam_Data_Reference' pertain to participant ID
###############################################################################

dat_survey_sm1 <- dat_survey_sm1 %>% rename(participant_id = External_Data_Reference)
dat_survey_sm2 <- dat_survey_sm2 %>% rename(participant_id = External_Data_Reference)
dat_survey_sm3 <- dat_survey_sm3 %>% rename(participant_id = External_Data_Reference)
dat_survey_sm4 <- dat_survey_sm4 %>% rename(participant_id = External_Data_Reference)

###############################################################################
# Read in date time variables and ensure appropriate formatting
###############################################################################

dat_survey_sm1 <- dat_survey_sm1 %>% 
  mutate(StartDate = as.character(StartDate),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         EndDate = strptime(x = EndDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  rename(begintime_1 = StartDate, endtime_1 = EndDate)

dat_survey_sm2 <- dat_survey_sm2 %>% 
  mutate(StartDate = as.character(.data[["ï..StartDate"]]),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
         EndDate = strptime(x = EndDate, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")) %>%
  rename(begintime_2 = StartDate, endtime_2 = EndDate)

dat_survey_sm3 <- dat_survey_sm3 %>% 
  mutate(StartDate = as.character(StartDate),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         EndDate = strptime(x = EndDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  rename(begintime_3 = StartDate, endtime_3 = EndDate)

dat_survey_sm4 <- dat_survey_sm4 %>% 
  mutate(StartDate = as.character(.data[["ï..StartDate"]]),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
         EndDate = strptime(x = EndDate, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")) %>%
  rename(begintime_4 = StartDate, endtime_4 = EndDate)

###############################################################################
# Select relevant columns
###############################################################################

dat_survey_sm1 <- dat_survey_sm1 %>% 
  select(participant_id, begintime_1, endtime_1, ALCdrink_SM1) %>%
  mutate(ALCdrink_SM1 = as.numeric(ALCdrink_SM1))

dat_survey_sm2 <- dat_survey_sm2 %>% 
  select(participant_id, begintime_2, endtime_2, ALCdrink_SM2) %>%
  mutate(ALCdrink_SM2 = as.numeric(ALCdrink_SM2))

dat_survey_sm3 <- dat_survey_sm3 %>% 
  select(participant_id, begintime_3, endtime_3, ALCdrink_SM3) %>%
  mutate(ALCdrink_SM3 = as.numeric(ALCdrink_SM3))

dat_survey_sm4 <- dat_survey_sm4 %>% 
  select(participant_id, begintime_4, endtime_4, ALCdrink_SM4) %>%
  mutate(ALCdrink_SM4 = as.numeric(ALCdrink_SM4))

###############################################################################
# Append information to existing data frame in wide format
###############################################################################

current_cols <- colnames(dat_rand_wide)
dat_rand_wide <- right_join(x = dat_survey_sm1, y = dat_rand_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

current_cols <- colnames(dat_rand_wide)
dat_rand_wide <- right_join(x = dat_survey_sm2, y = dat_rand_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

current_cols <- colnames(dat_rand_wide)
dat_rand_wide <- right_join(x = dat_survey_sm3, y = dat_rand_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

current_cols <- colnames(dat_rand_wide)
dat_rand_wide <- right_join(x = dat_survey_sm4, y = dat_rand_wide, by = "participant_id") %>%
  select(all_of(current_cols), everything())

###############################################################################
# Append information to existing data frame in long format
###############################################################################

dat_rand_long <- dat_survey_sm1 %>%
  right_join(x = ., y = dat_rand_long, by = "participant_id")

dat_rand_long <- dat_survey_sm2 %>%
  right_join(x = ., y = dat_rand_long, by = "participant_id")

dat_rand_long <- dat_survey_sm3 %>%
  right_join(x = ., y = dat_rand_long, by = "participant_id")

dat_rand_long <- dat_survey_sm4 %>%
  right_join(x = ., y = dat_rand_long, by = "participant_id")

dat_rand_long <- dat_rand_long %>%
  mutate(begintime = case_when(
    survey_number == 1 ~ begintime_1,
    survey_number == 2 ~ begintime_2,
    survey_number == 3 ~ begintime_3,
    survey_number == 4 ~ begintime_4,
    TRUE ~ as.POSIXlt(NA)
  ))

dat_rand_long <- dat_rand_long %>%
  mutate(endtime = case_when(
    survey_number == 1 ~ endtime_1,
    survey_number == 2 ~ endtime_2,
    survey_number == 3 ~ endtime_3,
    survey_number == 4 ~ endtime_4,
    TRUE ~ as.POSIXlt(NA)
  ))

dat_rand_long <- dat_rand_long %>%
  mutate(ALCdrink_SM = case_when(
    survey_number == 1 ~ ALCdrink_SM1,
    survey_number == 2 ~ ALCdrink_SM2,
    survey_number == 3 ~ ALCdrink_SM3,
    survey_number == 4 ~ ALCdrink_SM4,
    TRUE ~ NA_real_
  ))

dat_rand_long <- dat_rand_long %>%
  select(-ALCdrink_SM1, -ALCdrink_SM2, -ALCdrink_SM3, -ALCdrink_SM4,
         -begintime_1, -begintime_2, -begintime_3, -begintime_4,
         -endtime_1, -endtime_2, -endtime_3, -endtime_4)

###############################################################################
# Create dependent variable for Primary Aim
###############################################################################

# Work with wide format data
dat_rand_wide <- dat_rand_wide %>%
  mutate(Y_1 = replace(ALCdrink_SM1, ALCdrink_SM1 == -99, NA)) %>%
  mutate(Y_1 = if_else(is.na(Y_1), 0, 1))

dat_rand_wide <- dat_rand_wide %>%
  mutate(Y_2 = replace(ALCdrink_SM2, ALCdrink_SM2 == -99, NA)) %>%
  mutate(Y_2 = if_else(is.na(Y_2), 0, 1))

dat_rand_wide <- dat_rand_wide %>%
  mutate(Y_3 = replace(ALCdrink_SM3, ALCdrink_SM3 == -99, NA)) %>%
  mutate(Y_3 = if_else(is.na(Y_3), 0, 1))

dat_rand_wide <- dat_rand_wide %>%
  mutate(Y_4 = replace(ALCdrink_SM4, ALCdrink_SM4 == -99, NA)) %>%
  mutate(Y_4 = if_else(is.na(Y_4), 0, 1))

# Work with long format data
dat_rand_long <- dat_rand_long %>%
  mutate(Y = replace(ALCdrink_SM, ALCdrink_SM == -99, NA)) %>%
  mutate(Y = if_else(is.na(Y), 0, 1))

###############################################################################
# Save data files
###############################################################################

dat_primary_aim_wide <- dat_rand_wide %>%
  arrange(desc(exclude_from_all), participant_id, 
          availability_1, availability_2, availability_3, availability_4)

dat_primary_aim_long <- dat_rand_long %>% 
  arrange(desc(exclude_from_all), participant_id, survey_number)

save(dat_primary_aim_wide, file = file.path(path_staged_data, "dat_primary_aim_wide.RData"))
save(dat_primary_aim_long, file = file.path(path_staged_data, "dat_primary_aim_long.RData"))



