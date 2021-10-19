library(dplyr)
library(readxl)

source("paths.R")

###############################################################################
# Read in raw paradata files
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
  mutate(randtime_hrts = paste(Date, Time, sep=" ")) %>%
  mutate(randtime_hrts = strptime(x = randtime_hrts, format = "%Y-%m-%d %I:%M%p", tz = "US/Eastern"))

dat_paradata_sm2 <- dat_paradata_sm2 %>%
  rename(participant_id = Pin) %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(Time = substring(text = Time, first = 12)) %>%
  mutate(randtime_hrts = paste(Date, Time, sep=" ")) %>%
  mutate(randtime_hrts = strptime(x = randtime_hrts, format = "%Y-%m-%d %H:%M:%S", tz = "US/Eastern"))

dat_paradata_sm3 <- dat_paradata_sm3 %>%
  rename(participant_id = Pin) %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(randtime_hrts = paste(Date, Time, sep=" ")) %>%
  mutate(randtime_hrts = strptime(x = randtime_hrts, format = "%Y-%m-%d %I:%M%p", tz = "US/Eastern"))

dat_paradata_sm4 <- dat_paradata_sm4 %>%
  rename(participant_id = Pin) %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(randtime_hrts = paste(Date, Time, sep=" ")) %>%
  mutate(randtime_hrts = strptime(x = randtime_hrts, format = "%Y-%m-%d %I:%M%p", tz = "US/Eastern"))

###############################################################################
# Grab randomization assignment for reminders, if any exist
###############################################################################

dat_reminder_sm1 <- dat_paradata_sm1 %>% 
  filter(`Invite or Reminder` == "Reminder") %>%
  select(participant_id, `Product or Charity`, randtime_hrts, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

# Sanity check: any duplicates?
# Note: No duplicates exist
if(sum(duplicated(dat_reminder_sm1)) > 0){
  print("Duplicates exist")
}

dat_reminder_sm1 <- dat_reminder_sm1 %>% 
  arrange(participant_id, randtime_hrts) %>%
  mutate(ones = 1) %>%
  group_by(participant_id) %>%
  mutate(reminder_number = cumsum(ones)) %>%
  mutate(reminder_total = max(reminder_number)) %>%
  select(-ones)

dat_reminder_sm2 <- dat_paradata_sm2 %>% 
  filter(`Invite or Reminder` == "Reminder") %>%
  select(participant_id, `Product or Charity`, randtime_hrts, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

# Sanity check: any duplicates?
# Note: No duplicates exist
if(sum(duplicated(dat_reminder_sm2)) > 0){
  print("Duplicates exist")
}

dat_reminder_sm2 <- dat_reminder_sm2 %>% 
  arrange(participant_id, randtime_hrts) %>%
  mutate(ones = 1) %>%
  group_by(participant_id) %>%
  mutate(reminder_number = cumsum(ones)) %>%
  mutate(reminder_total = max(reminder_number)) %>%
  select(-ones)

dat_reminder_sm3 <- dat_paradata_sm3 %>% 
  filter(`Invite or Reminder` == "Reminder") %>%
  select(participant_id, `Product or Charity`, randtime_hrts, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

# Sanity check: any duplicates?
# Note: No duplicates exist
if(sum(duplicated(dat_reminder_sm3)) > 0){
  print("Duplicates exist")
}

dat_reminder_sm3 <- dat_reminder_sm3 %>% 
  arrange(participant_id, randtime_hrts) %>%
  mutate(ones = 1) %>%
  group_by(participant_id) %>%
  mutate(reminder_number = cumsum(ones)) %>%
  mutate(reminder_total = max(reminder_number)) %>%
  select(-ones)

dat_reminder_sm4 <- dat_paradata_sm4 %>% 
  filter(`Invite or Reminder` == "Reminder") %>%
  select(participant_id, `Product or Charity`, randtime_hrts, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

# Sanity check: any duplicates?
# Note: No duplicates exist
if(sum(duplicated(dat_reminder_sm4)) > 0){
  print("Duplicates exist")
}

dat_reminder_sm4 <- dat_reminder_sm4 %>% 
  arrange(participant_id, randtime_hrts) %>%
  mutate(ones = 1) %>%
  group_by(participant_id) %>%
  mutate(reminder_number = cumsum(ones)) %>%
  mutate(reminder_total = max(reminder_number)) %>%
  select(-ones)

###############################################################################
# Wrap up data preparation
###############################################################################

dat_reminder_sm1 <- dat_reminder_sm1 %>% mutate(decision_point = 1)
dat_reminder_sm2 <- dat_reminder_sm2 %>% mutate(decision_point = 2)
dat_reminder_sm3 <- dat_reminder_sm3 %>% mutate(decision_point = 3)
dat_reminder_sm4 <- dat_reminder_sm4 %>% mutate(decision_point = 4)

dat_reminder <- rbind(dat_reminder_sm1,
                      dat_reminder_sm2,
                      dat_reminder_sm3,
                      dat_reminder_sm4)

dat_reminder <- dat_reminder %>%
  select(participant_id, decision_point, everything())

###############################################################################
# Save data files
###############################################################################

save(dat_reminder, file = file.path(path_staged_data, "dat_reminder.RData"))

