library(dplyr)
library(readxl)

source("paths.R")

###############################################################################
# Read in raw paradata files
###############################################################################

dat_paradata_sm1 <- read_xlsx(path = file.path(path_input_data, "SM Paradata_updated7.1.21.xlsx"), 
                              sheet = "SM1", na = "N/A", 
                              col_types = c("text", "numeric", "text", "text", "text", "date", "text", "text", "date", "date"))

dat_paradata_sm2 <- read_xlsx(path = file.path(path_input_data, "SM Paradata_updated7.1.21.xlsx"), 
                              sheet = "SM2", na = "N/A", 
                              col_types = c("text", "numeric", "text", "text", "text", "date", "date", "text", "date", "date", "text"))

dat_paradata_sm3 <- read_xlsx(path = file.path(path_input_data, "SM Paradata_updated7.1.21.xlsx"), 
                              sheet = "SM3", na = "N/A", 
                              col_types = c("text", "numeric", "text", "text", "text", "date", "text", "text", "date", "date"))

dat_paradata_sm4 <- read_xlsx(path = file.path(path_input_data, "SM Paradata_updated7.1.21.xlsx"), 
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
  mutate(randtime_hrts = strptime(x = randtime_hrts, format = "%Y-%m-%d %I:%M%p", tz = "UTC"))

dat_paradata_sm2 <- dat_paradata_sm2 %>%
  rename(participant_id = Pin) %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(Time = substring(text = Time, first = 12)) %>%
  mutate(randtime_hrts = paste(Date, Time, sep=" ")) %>%
  mutate(randtime_hrts = strptime(x = randtime_hrts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

dat_paradata_sm3 <- dat_paradata_sm3 %>%
  rename(participant_id = Pin) %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(randtime_hrts = paste(Date, Time, sep=" ")) %>%
  mutate(randtime_hrts = strptime(x = randtime_hrts, format = "%Y-%m-%d %I:%M%p", tz = "UTC"))

dat_paradata_sm4 <- dat_paradata_sm4 %>%
  rename(participant_id = Pin) %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(randtime_hrts = paste(Date, Time, sep=" ")) %>%
  mutate(randtime_hrts = strptime(x = randtime_hrts, format = "%Y-%m-%d %I:%M%p", tz = "UTC"))

###############################################################################
# Grab randomization assignment for invite, if it exists
###############################################################################

dat_invite_sm1 <- dat_paradata_sm1 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime_hrts, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

dat_invite_sm1 <- dat_invite_sm1 %>% arrange(participant_id, randtime_hrts)
# One participant ID was sent a invite twice
# we only keep the earlier one
dat_invite_sm1 <- dat_invite_sm1 %>% filter(!duplicated(dat_invite_sm1$participant_id))

dat_invite_sm2 <- dat_paradata_sm2 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime_hrts, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

dat_invite_sm2 <- dat_invite_sm2 %>% arrange(participant_id, randtime_hrts)
# Each participant ID in this data frame was sent a reminder only once
# effectively, no rows are dropped at this filtering step
dat_invite_sm2 <- dat_invite_sm2 %>% filter(!duplicated(dat_invite_sm2$participant_id))

dat_invite_sm3 <- dat_paradata_sm3 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime_hrts, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

dat_invite_sm3 <- dat_invite_sm3 %>% arrange(participant_id, randtime_hrts)
# Each participant ID in this data frame was sent a reminder only once
# effectively, no rows are dropped at this filtering step
dat_invite_sm3 <- dat_invite_sm3 %>% filter(!duplicated(dat_invite_sm3$participant_id))

dat_invite_sm4 <- dat_paradata_sm4 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime_hrts, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

dat_invite_sm4 <- dat_invite_sm4 %>% arrange(participant_id, randtime_hrts)
# Each participant ID in this data frame was sent a reminder only once
# effectively, no rows are dropped at this filtering step
dat_invite_sm4 <- dat_invite_sm4 %>% filter(!duplicated(dat_invite_sm4$participant_id))

###############################################################################
# Wrap up data preparation
###############################################################################

dat_invite_sm1 <- dat_invite_sm1 %>% mutate(decision_point = 1)
dat_invite_sm2 <- dat_invite_sm2 %>% mutate(decision_point = 2)
dat_invite_sm3 <- dat_invite_sm3 %>% mutate(decision_point = 3)
dat_invite_sm4 <- dat_invite_sm4 %>% mutate(decision_point = 4)

dat_invite <- rbind(dat_invite_sm1,
                    dat_invite_sm2,
                    dat_invite_sm3,
                    dat_invite_sm4)

dat_invite <- dat_invite %>% select(participant_id, decision_point, everything())

###############################################################################
# Save data files
###############################################################################

save(dat_invite, file = file.path(path_staged_data, "dat_invite.RData"))

