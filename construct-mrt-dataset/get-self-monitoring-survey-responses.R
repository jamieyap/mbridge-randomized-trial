library(dplyr)
library(readxl)

source("paths.R")

###############################################################################
# Read in raw data from surveys
###############################################################################

dat_survey_sm1 <- read_xlsx(path = file.path(path_input_data, "SM1 Data_corrected4.21.21.xlsx"), sheet = "SM1 Data_4.20.21")
dat_survey_sm2 <- read.csv(file = file.path(path_input_data, "SM2 Data with updated PIN (6.5.20).csv"), header = TRUE, na.strings = "")
dat_survey_sm3 <- read_xlsx(path = file.path(path_input_data, "SM3 Data_corrected4.21.21.xlsx"), sheet = "SM3 Data (3.4.20)")
dat_survey_sm4 <- read.csv(file = file.path(path_input_data, "SM4 Data (3.4.20).csv"), header = TRUE, na.strings = "")

###############################################################################
# Columns named 'External_Data_Reference' pertain to participant ID
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
  mutate(StartDate = strptime(x = StartDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Eastern"),
         EndDate = strptime(x = EndDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Eastern")) %>%
  rename(begintime_hrts = StartDate, endtime_hrts = EndDate)

colnames(dat_survey_sm2)[1] <- "StartDate"

dat_survey_sm2 <- dat_survey_sm2 %>% 
  mutate(StartDate = as.character(StartDate),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%m/%d/%Y %H:%M:%S", tz = "US/Eastern"),
         EndDate = strptime(x = EndDate, format = "%m/%d/%Y %H:%M:%S", tz = "US/Eastern")) %>%
  rename(begintime_hrts = StartDate, endtime_hrts = EndDate)

dat_survey_sm3 <- dat_survey_sm3 %>% 
  mutate(StartDate = as.character(StartDate),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Eastern"),
         EndDate = strptime(x = EndDate, format = "%Y-%m-%d %H:%M:%S", tz = "US/Eastern")) %>%
  rename(begintime_hrts = StartDate, endtime_hrts = EndDate)

colnames(dat_survey_sm4)[1] <- "StartDate"

dat_survey_sm4 <- dat_survey_sm4 %>% 
  mutate(StartDate = as.character(StartDate),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%m/%d/%Y %H:%M:%S", tz = "US/Eastern"),
         EndDate = strptime(x = EndDate, format = "%m/%d/%Y %H:%M:%S", tz = "US/Eastern")) %>%
  rename(begintime_hrts = StartDate, endtime_hrts = EndDate)

###############################################################################
# Select relevant columns
###############################################################################

and_these_cols <- c("ALCdrink")

dat_survey_sm1 <- dat_survey_sm1 %>% 
  mutate(decision_point = 1) %>%
  select("participant_id", "decision_point", 
         "begintime_hrts", "endtime_hrts", "Finished",
         "SMdrawing_SM1_Prod", "SMdrawing_SM1_Char",
         all_of(paste(and_these_cols, "_SM1", sep=""))) %>%
  rename(SMdrawing_Prod = SMdrawing_SM1_Prod,
         SMdrawing_Char = SMdrawing_SM1_Char)

colnames(dat_survey_sm1) <- unlist(strsplit(colnames(dat_survey_sm1), "_SM1"))

dat_survey_sm2 <- dat_survey_sm2 %>% 
  mutate(decision_point = 2) %>%
  select("participant_id", "decision_point", 
         "begintime_hrts", "endtime_hrts", "Finished", 
         "SMdrawing_SM2_Prod", "SMdrawing_SM2_Char",
         all_of(paste(and_these_cols, "_SM2", sep=""))) %>%
  rename(SMdrawing_Prod = SMdrawing_SM2_Prod,
         SMdrawing_Char = SMdrawing_SM2_Char)

colnames(dat_survey_sm2) <- unlist(strsplit(colnames(dat_survey_sm2), "_SM2"))

dat_survey_sm3 <- dat_survey_sm3 %>% 
  mutate(decision_point = 3) %>%
  select("participant_id", "decision_point", 
         "begintime_hrts", "endtime_hrts", "Finished", 
         "SMdrawing_SM3_Prod", "SMdrawing_SM3_Char",
         all_of(paste(and_these_cols, "_SM3", sep=""))) %>%
  rename(SMdrawing_Prod = SMdrawing_SM3_Prod,
         SMdrawing_Char = SMdrawing_SM3_Char)

colnames(dat_survey_sm3) <- unlist(strsplit(colnames(dat_survey_sm3), "_SM3"))

dat_survey_sm4 <- dat_survey_sm4 %>% 
  mutate(decision_point = 4) %>%
  select("participant_id", "decision_point", 
         "begintime_hrts", "endtime_hrts", "Finished", 
         "SMdrawing_SM4_Prod", "SMdrawing_SM4_Char",
         all_of(paste(and_these_cols, "_SM4", sep=""))) %>%
  rename(SMdrawing_Prod = SMdrawing_SM4_Prod,
         SMdrawing_Char = SMdrawing_SM4_Char)

colnames(dat_survey_sm4) <- unlist(strsplit(colnames(dat_survey_sm4), "_SM4"))

###############################################################################
# Merge data from all decision points
###############################################################################

dat_survey <- rbind(dat_survey_sm1,
                    dat_survey_sm2,
                    dat_survey_sm3,
                    dat_survey_sm4)

dat_survey <- dat_survey %>% arrange(participant_id, decision_point)

###############################################################################
# Clean up the responses: replace -99 to NA
###############################################################################

dat_survey <- dat_survey %>% mutate(ALCdrink = replace(ALCdrink, ALCdrink == -99, NA))

###############################################################################
# Save output
###############################################################################

save(dat_survey, file = file.path(path_staged_data, "dat_survey.RData"))

