library(dplyr)


path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")
path_output_data <- Sys.getenv("path_output_data")


load(file = file.path(path_staged_data, "dat_primary_aim_wide.RData"))
load(file = file.path(path_staged_data, "dat_primary_aim_long.RData"))


#write.csv(dat_primary_aim_wide, file = file.path(path_staged_data, "dat_primary_aim_wide.csv"), row.names = FALSE, na = "")
#write.csv(dat_primary_aim_long, file = file.path(path_staged_data, "dat_primary_aim_long.csv"), row.names = FALSE, na = "")

dat <- dat_primary_aim_long %>% 
  filter(exclude_from_all == 0) %>%
  mutate(diff = as.numeric(first_reminder_time - randtime)*(24)) %>%
  mutate(Y_within = if_else(begintime - randtime <=62, 1, 0))


dat_primary_aim_long %>% 
  filter(exclude_from_all == 0) %>%
  mutate(diff = as.numeric(first_reminder_time - randtime)*(24)) %>% 
  group_by(survey_number) %>%
  filter(availability == 1) %>%
  summarise(minimum = min(diff, na.rm = TRUE),
            q25 = quantile(diff, probs = .25, na.rm = TRUE),
            q50 = quantile(diff, probs = .50, na.rm = TRUE),
            q75 = quantile(diff, probs = .75, na.rm = TRUE),
            maximum = max(diff, na.rm = TRUE))

dat_primary_aim_wide %>% 
  ungroup(.) %>%
  filter(exclude_from_all == 0) %>%
  mutate(diff1 = as.numeric(randtime_2 - randtime_1)*24,
         diff2 = as.numeric(randtime_3 - randtime_2)*24,
         diff3 = as.numeric(randtime_4 - randtime_3)*24) %>%
  summarise(elapsed_1_to_2_min = min(diff1, na.rm=TRUE),
            elapsed_2_to_3_min = min(diff2, na.rm=TRUE),
            elapsed_3_to_4_min = min(diff3, na.rm=TRUE))


dat_primary_aim_wide %>% 
  ungroup(.) %>%
  filter(exclude_from_all == 0) %>%
  mutate(diff1 = as.numeric(randtime_2 - randtime_1)*24,
         diff2 = as.numeric(randtime_3 - randtime_2)*24,
         diff3 = as.numeric(randtime_4 - randtime_3)*24) %>%
  summarise(elapsed_1_to_2_max = max(diff1, na.rm=TRUE),
            elapsed_2_to_3_max = max(diff2, na.rm=TRUE),
            elapsed_3_to_4_max = max(diff3, na.rm=TRUE))


dat_primary_aim_wide %>% 
  ungroup(.) %>%
  filter(exclude_from_all == 0) %>%
  mutate(diff1 = as.numeric(randtime_2 - randtime_1)*24,
         diff2 = as.numeric(randtime_3 - randtime_2)*24,
         diff3 = as.numeric(randtime_4 - randtime_3)*24) %>%
  summarise(elapsed_1_to_2_median = quantile(diff1, probs = .50, na.rm=TRUE),
            elapsed_2_to_3_median = quantile(diff2, probs = .50, na.rm=TRUE),
            elapsed_3_to_4_median = quantile(diff3, probs = .50, na.rm=TRUE))

tmp3 <- dat_primary_aim_wide %>% 
  ungroup(.) %>%
  filter(exclude_from_all == 0) %>%
  mutate(diff1 = as.numeric(randtime_2 - randtime_1)*24,
         diff2 = as.numeric(randtime_3 - randtime_2)*24,
         diff3 = as.numeric(randtime_4 - randtime_3)*24)


# Was the survey completed prior to sending the first reminder?
dat_primary_aim_long %>% 
  filter(exclude_from_all == 0) %>%
  mutate(diff = (begintime - first_reminder_time)*(1/3600)*(1/24)) %>% 
  group_by(survey_number) %>%
  filter(availability == 1) %>%
  summarise(began_before_reminder = sum(diff < 0, na.rm=TRUE),
            began_after_reminder = sum(diff >= 0, na.rm=TRUE))



dat_primary_aim_long %>% 
  filter(exclude_from_all == 0) %>%
  filter(availability == 1) %>%
  group_by(Y, is.na(first_reminder_time)) %>% 
  summarise(n()) %>%
  arrange(Y)





dat_primary_aim_long %>% 
  filter(exclude_from_all == 0) %>%
  filter(!is.na(first_reminder_time) & !is.na(randtime)) %>%
  mutate(diff = as.numeric((first_reminder_time - randtime)*(24))) %>% 
  group_by(survey_number) %>%
  filter(availability == 1) %>%
  summarise(minimum = min(diff, na.rm = TRUE),
            q25 = quantile(diff, probs = .25, na.rm = TRUE),
            q50 = quantile(diff, probs = .50, na.rm = TRUE),
            q75 = quantile(diff, probs = .75, na.rm = TRUE),
            maximum = max(diff, na.rm = TRUE))





dat_primary_aim_long %>% 
  filter(exclude_from_all == 0) %>%
  mutate(diff = (begintime - randtime)*(1/3600)*(1/24)) %>%
  group_by(survey_number) %>%
  filter(availability == 1) %>%
  summarise(sum(diff >14, na.rm=TRUE),
            sum(diff <=3, na.rm = TRUE),
            sum(diff > 3 & diff <=14, na.rm=TRUE))


# Effect among individuals who did not receive any reminder
dat_primary_aim_long %>% 
  filter(exclude_from_all == 0) %>%
  filter(availability == 1) %>%
  filter(Y==0 & is.na(first_reminder_time))


tmp2 <- dat_primary_aim_wide %>% 
  filter(exclude_from_all == 0) %>%
  mutate(diff1 = (randtime_2 - randtime_1) * (1/3600) * (1/1),
         diff2 = (randtime_3 - randtime_2) * (1/3600) * (1/1),
         diff3 = (randtime_4 - randtime_3) * (1/3600) * (1/1)) %>%
  mutate(indicator = 1*(diff3 >=345)) %>%
  filter(indicator==0)
  #select(diff1, diff2, diff3)

# randomization timestamps are approximately correct
# half of people got an extra nine hours between the third and fourth



dat_primary_aim_long %>% 
  ungroup(.) %>% 
  filter(exclude_from_all == 0 & availability == 1 & Y == 1) %>% 
  mutate(diff = as.numeric((begintime - randtime)/(60*60))) %>% 
  group_by(survey_number) %>% 
  summarise(q50 = quantile(diff, probs = c(.5), na.rm=TRUE), 
            q60 = quantile(diff, probs = c(.6), na.rm=TRUE), 
            q70 = quantile(diff, probs = c(.7), na.rm=TRUE), 
            q80 = quantile(diff, probs = c(.8), na.rm=TRUE), 
            q90 = quantile(diff, probs = c(.9), na.rm=TRUE))

