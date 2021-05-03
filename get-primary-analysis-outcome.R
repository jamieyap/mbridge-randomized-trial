library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

###############################################################################
# Read in raw data files
###############################################################################

dat_survey_sm1 <- read_xlsx(path = file.path(path_input_data, "SM1 Data_corrected4.21.21.xlsx"), sheet = "SM1 Data_4.20.21")
dat_survey_sm2 <- read.csv(file = file.path(path_input_data, "SM2 Data with updated PIN (6.5.20).csv"), header = TRUE, na.strings = "")
dat_survey_sm3 <- read_xlsx(path = file.path(path_input_data, "SM3 Data_corrected4.21.21.xlsx"), sheet = "SM3 Data (3.4.20)")
dat_survey_sm4 <- read.csv(file = file.path(path_input_data, "SM4 Data (3.4.20).csv"), header = TRUE, na.strings = "")

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

dat_pin_masterlist <- read_xlsx(path = file.path(path_input_data, "M-Bridge Pin Master List.xlsx"), sheet = "M-Bridge Master List", col_types = "numeric")
dat_baseline <- read.csv(file = file.path(path_input_data, "Baseline Data with HD variables added (2.8.21) final.csv"), header = TRUE, na.strings = "")


###############################################################################
# Columns named 'Externam_Data_Reference' or 'Pin" pertain to participant ID
# Values under both column names should be identical
###############################################################################

dat_survey_sm1 <- dat_survey_sm1 %>% rename(participant_id = External_Data_Reference)
dat_survey_sm2 <- dat_survey_sm2 %>% rename(participant_id = External_Data_Reference)
dat_survey_sm3 <- dat_survey_sm3 %>% rename(participant_id = External_Data_Reference)
dat_survey_sm4 <- dat_survey_sm4 %>% rename(participant_id = External_Data_Reference)

dat_paradata_sm1 <- dat_paradata_sm1 %>% rename(participant_id = Pin)
dat_paradata_sm2 <- dat_paradata_sm2 %>% rename(participant_id = Pin)
dat_paradata_sm3 <- dat_paradata_sm3 %>% rename(participant_id = Pin)
dat_paradata_sm4 <- dat_paradata_sm4 %>% rename(participant_id = Pin)

dat_pin_masterlist <- dat_pin_masterlist %>% rename(participant_id = Pin)
dat_baseline <- dat_baseline %>% rename(participant_id = External_Data_Reference)


###############################################################################
# Read in date time variables and ensure appropriate formatting
###############################################################################

dat_survey_sm1 <- dat_survey_sm1 %>% 
  mutate(StartDate = as.character(StartDate),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         EndDate = strptime(x = EndDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

dat_survey_sm2 <- dat_survey_sm2 %>% 
  mutate(StartDate = as.character(.data[["ï..StartDate"]]),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
         EndDate = strptime(x = EndDate, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"))

dat_survey_sm3 <- dat_survey_sm3 %>% 
  mutate(StartDate = as.character(StartDate),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         EndDate = strptime(x = EndDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

dat_survey_sm4 <- dat_survey_sm4 %>% 
  mutate(StartDate = as.character(.data[["ï..StartDate"]]),
         EndDate = as.character(EndDate)) %>%
  mutate(StartDate = strptime(x = StartDate, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
         EndDate = strptime(x = EndDate, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"))

dat_paradata_sm1 <- dat_paradata_sm1 %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(RandDate = paste(Date, Time, sep=" ")) %>%
  mutate(RandDate = strptime(x = RandDate, format = "%Y-%m-%d %I:%M%p", tz = "UTC"))

dat_paradata_sm2 <- dat_paradata_sm2 %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(Time = substring(text = Time, first = 12)) %>%
  mutate(RandDate = paste(Date, Time, sep=" ")) %>%
  mutate(RandDate = strptime(x = RandDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

dat_paradata_sm3 <- dat_paradata_sm3 %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(RandDate = paste(Date, Time, sep=" ")) %>%
  mutate(RandDate = strptime(x = RandDate, format = "%Y-%m-%d %I:%M%p", tz = "UTC"))

dat_paradata_sm4 <- dat_paradata_sm4 %>%
  mutate(Date = as.character(Date),
         Time = as.character(Time)) %>%
  mutate(RandDate = paste(Date, Time, sep=" ")) %>%
  mutate(RandDate = strptime(x = RandDate, format = "%Y-%m-%d %I:%M%p", tz = "UTC"))


###############################################################################
# Construct new data frame which will eventually contain the outcome variable
# and covariates for the primary aim analysis
###############################################################################

dat_analysis <- data.frame(participant_id = rep(dat_pin_masterlist[["participant_id"]], each = 4),
                           decision_point = rep(1:4, times = nrow(dat_pin_masterlist)))

###############################################################################
# Construct outcome variable for primary analysis
###############################################################################

subset_dat_survey_sm1 <- dat_survey_sm1 %>% 
  mutate(decision_point = 1,
         sm1_survey_delivered = 1,
         ALCdrink_SM1 = if_else(ALCdrink_SM1 == -99, NA_real_, as.numeric(ALCdrink_SM1))) %>% 
  mutate(Y1 = if_else(is.na(ALCdrink_SM1), 0, 1)) %>%
  select(participant_id, decision_point, Y1, sm1_survey_delivered)

subset_dat_paradata_sm1 <- dat_paradata_sm1 %>%
  group_by(participant_id) %>%
  summarise(indicator_missed_sm1 = 1*(sum(1*(`Completion Status` == "PARTIAL") + 1*(`Completion Status` == "COMPLETE")) == 0)) %>%
  mutate(decision_point = 1)

subset_dat_survey_sm2 <- dat_survey_sm2 %>% 
  mutate(decision_point = 2,
         sm2_survey_delivered = 1,
         ALCdrink_SM2 = if_else(ALCdrink_SM2 == -99, NA_real_, as.numeric(ALCdrink_SM2))) %>% 
  mutate(Y2 = if_else(is.na(ALCdrink_SM2), 0, 1)) %>%
  select(participant_id, decision_point, Y2, sm2_survey_delivered)

subset_dat_paradata_sm2 <- dat_paradata_sm2 %>%
  group_by(participant_id) %>%
  summarise(indicator_missed_sm2 = 1*(sum(1*(`Completion Status` == "PARTIAL") + 1*(`Completion Status` == "COMPLETE")) == 0)) %>%
  mutate(decision_point = 2)

subset_dat_survey_sm3 <- dat_survey_sm3 %>% 
  mutate(decision_point = 3,
         sm3_survey_delivered = 1,
         ALCdrink_SM3 = if_else(ALCdrink_SM3 == -99, NA_real_, as.numeric(ALCdrink_SM3))) %>% 
  mutate(Y3 = if_else(is.na(ALCdrink_SM3), 0, 1)) %>%
  select(participant_id, decision_point, Y3, sm3_survey_delivered)

subset_dat_paradata_sm3 <- dat_paradata_sm3 %>%
  group_by(participant_id) %>%
  summarise(indicator_missed_sm3 = 1*(sum(1*(`Completion Status` == "PARTIAL") + 1*(`Completion Status` == "COMPLETE")) == 0)) %>%
  mutate(decision_point = 3)

subset_dat_survey_sm4 <- dat_survey_sm4 %>% 
  mutate(decision_point = 4,
         sm4_survey_delivered = 1,
         ALCdrink_SM4 = if_else(ALCdrink_SM4 == -99, NA_real_, as.numeric(ALCdrink_SM4))) %>% 
  mutate(Y4 = if_else(is.na(ALCdrink_SM4), 0, 1)) %>%
  select(participant_id, decision_point, Y4, sm4_survey_delivered)

subset_dat_paradata_sm4 <- dat_paradata_sm4 %>%
  group_by(participant_id) %>%
  summarise(indicator_missed_sm4 = 1*(sum(1*(`Completion Status` == "PARTIAL") + 1*(`Completion Status` == "COMPLETE")) == 0)) %>%
  mutate(decision_point = 4)

dat_analysis <- left_join(x = dat_analysis, y = subset_dat_survey_sm1, by = c("participant_id", "decision_point"))
dat_analysis <- left_join(x = dat_analysis, y = subset_dat_survey_sm2, by = c("participant_id", "decision_point"))
dat_analysis <- left_join(x = dat_analysis, y = subset_dat_survey_sm3, by = c("participant_id", "decision_point"))
dat_analysis <- left_join(x = dat_analysis, y = subset_dat_survey_sm4, by = c("participant_id", "decision_point"))

dat_analysis <- left_join(x = dat_analysis, y = subset_dat_paradata_sm1, by = c("participant_id", "decision_point"))
dat_analysis <- left_join(x = dat_analysis, y = subset_dat_paradata_sm2, by = c("participant_id", "decision_point"))
dat_analysis <- left_join(x = dat_analysis, y = subset_dat_paradata_sm3, by = c("participant_id", "decision_point"))
dat_analysis <- left_join(x = dat_analysis, y = subset_dat_paradata_sm4, by = c("participant_id", "decision_point"))

dat_analysis <- dat_analysis %>%
  mutate(sm1_survey_delivered = if_else(is.na(sm1_survey_delivered), indicator_missed_sm1, sm1_survey_delivered),
         sm2_survey_delivered = if_else(is.na(sm2_survey_delivered), indicator_missed_sm2, sm2_survey_delivered),
         sm3_survey_delivered = if_else(is.na(sm3_survey_delivered), indicator_missed_sm3, sm3_survey_delivered),
         sm4_survey_delivered = if_else(is.na(sm4_survey_delivered), indicator_missed_sm4, sm4_survey_delivered))

dat_analysis <- dat_analysis %>% 
  mutate(Yk = case_when(
    decision_point == 1 ~ Y1,
    decision_point == 2 ~ Y2,
    decision_point == 3 ~ Y3,
    decision_point == 4 ~ Y4,
    TRUE ~ NA_real_
  )) %>%
  select(-Y1, -Y2, -Y3, -Y4)

dat_analysis <- dat_analysis %>%
  mutate(smk_survey_delivered = case_when(
    decision_point == 1 ~ sm1_survey_delivered,
    decision_point == 2 ~ sm2_survey_delivered,
    decision_point == 3 ~ sm3_survey_delivered,
    decision_point == 4 ~ sm4_survey_delivered,
    TRUE ~ NA_real_
  )) %>%
  mutate(smk_survey_delivered = replace(smk_survey_delivered, is.na(smk_survey_delivered), 0)) %>%
  select(-sm1_survey_delivered, -sm2_survey_delivered, -sm3_survey_delivered, -sm4_survey_delivered)

dat_analysis <- dat_analysis %>%
  mutate(indicator_missed_smk = case_when(
    decision_point == 1 ~ indicator_missed_sm1,
    decision_point == 2 ~ indicator_missed_sm2,
    decision_point == 3 ~ indicator_missed_sm3,
    decision_point == 4 ~ indicator_missed_sm4,
    TRUE ~ NA_real_
  )) %>%
  select(-indicator_missed_sm1, -indicator_missed_sm2, -indicator_missed_sm3, -indicator_missed_sm4)

dat_analysis <- dat_analysis %>% 
  mutate(indicator_missed_smk = if_else((smk_survey_delivered==1) & (is.na(indicator_missed_smk)), 0, indicator_missed_smk))

dat_analysis <- dat_analysis %>%
  mutate(Yk = if_else(is.na(Yk) & (smk_survey_delivered==1), 0, Yk))


###############################################################################
# Calculate summary statistics
###############################################################################

dat_analysis %>%
  group_by(decision_point) %>%
  summarise(mean(smk_survey_delivered))

dat_analysis %>%
  filter(smk_survey_delivered == 1) %>%
  group_by(decision_point) %>%
  summarise(mean(indicator_missed_smk))

dat_analysis %>%
  filter(smk_survey_delivered == 1) %>%
  group_by(decision_point) %>%
  summarise(mean(Yk))

###############################################################################
# Save to RData file
###############################################################################

save(dat_analysis, file = file.path(path_staged_data, "primary_analysis_outcome.RData"))

# Note: additional step needed to exclude those participant_id's randomized to
# the assessment only condition.

