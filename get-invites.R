library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

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
# Grab randomization assignment for invite, if it exists
###############################################################################

dat_rand_invite_sm1 <- dat_paradata_sm1 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

dat_rand_invite_sm1 <- dat_rand_invite_sm1 %>% arrange(participant_id, randtime)
# One participant ID was sent a invite twice
# we only keep the earlier one
dat_rand_invite_sm1 <- dat_rand_invite_sm1 %>% filter(!duplicated(dat_rand_invite_sm1$participant_id))

dat_rand_invite_sm2 <- dat_paradata_sm2 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

dat_rand_invite_sm2 <- dat_rand_invite_sm2 %>% arrange(participant_id, randtime)
# Each participant ID in this data frame was sent a reminder only once
# effectively, no rows are dropped at this filtering step
dat_rand_invite_sm2 <- dat_rand_invite_sm2 %>% filter(!duplicated(dat_rand_invite_sm2$participant_id))

dat_rand_invite_sm3 <- dat_paradata_sm3 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

dat_rand_invite_sm3 <- dat_rand_invite_sm3 %>% arrange(participant_id, randtime)
# Each participant ID in this data frame was sent a reminder only once
# effectively, no rows are dropped at this filtering step
dat_rand_invite_sm3 <- dat_rand_invite_sm3 %>% filter(!duplicated(dat_rand_invite_sm3$participant_id))

dat_rand_invite_sm4 <- dat_paradata_sm4 %>% 
  filter(`Invite or Reminder` == "Invite") %>%
  select(participant_id, `Product or Charity`, randtime, `Completion Status`) %>%
  rename(randassign = `Product or Charity`, status = `Completion Status`)

dat_rand_invite_sm4 <- dat_rand_invite_sm4 %>% arrange(participant_id, randtime)
# Each participant ID in this data frame was sent a reminder only once
# effectively, no rows are dropped at this filtering step
dat_rand_invite_sm4 <- dat_rand_invite_sm4 %>% filter(!duplicated(dat_rand_invite_sm4$participant_id))

###############################################################################
# Save data files
###############################################################################

save(dat_rand_invite_sm1, file = file.path(path_staged_data, "dat_rand_invite_sm1.RData"))
save(dat_rand_invite_sm2, file = file.path(path_staged_data, "dat_rand_invite_sm2.RData"))
save(dat_rand_invite_sm3, file = file.path(path_staged_data, "dat_rand_invite_sm3.RData"))
save(dat_rand_invite_sm4, file = file.path(path_staged_data, "dat_rand_invite_sm4.RData"))

