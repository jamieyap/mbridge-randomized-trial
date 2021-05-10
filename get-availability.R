library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

###############################################################################
# Read in raw data files
###############################################################################

dat_pin_masterlist <- read_xlsx(path = file.path(path_input_data, "M-Bridge Pin Master List.xlsx"), sheet = "M-Bridge Master List", col_types = c("numeric","text"))
dat_baseline <- read.csv(file = file.path(path_input_data, "Baseline Data with HD variables added (2.8.21) final.csv"), header = TRUE, na.strings = "")
dat_nonres_SM1 <- read_xlsx(path = file.path(path_input_data, "Stage 2 intervention groups 21.02.01.xlsx"), sheet = "SM 1", col_types = c("numeric","text","text","text","date","text"))
dat_nonres_SM2 <- read_xlsx(path = file.path(path_input_data, "Stage 2 intervention groups 21.02.01.xlsx"), sheet = "SM 2", col_types = c("numeric","text","text","text","date","text"))
dat_nonres_SM3 <- read_xlsx(path = file.path(path_input_data, "Stage 2 intervention groups 21.02.01.xlsx"), sheet = "SM 3", col_types = c("numeric","text","text","text","date","text"))
dat_nonres_SM4 <- read_xlsx(path = file.path(path_input_data, "Stage 2 intervention groups 21.02.01.xlsx"), sheet = "Sm 4", col_types = c("numeric","text","text","text","date","text"))

dat_nonres_SM1 <- dat_nonres_SM1 %>% filter(PIN != 3112) # remove row where an error in the raw data records occurred
dat_nonres_all <- rbind(dat_nonres_SM1, dat_nonres_SM2, dat_nonres_SM3, dat_nonres_SM4)

###############################################################################
# Begin data preparation steps
###############################################################################

dat_analysis <- dat_pin_masterlist %>% 
  filter(Group == "Experimental Early" | Group == "Experimental Late") %>%
  rename(participant_id = Pin, group = Group)

dat_analysis <- dat_nonres_all %>%
  select(PIN, `SM flagged`) %>%
  rename(participant_id = PIN,
         sm_flagged = `SM flagged`) %>%
  right_join(x = ., y = dat_analysis, by = "participant_id")

dat_analysis <- dat_baseline %>%
  select(External_Data_Reference, CharityChoice, ProductChoice) %>%
  rename(participant_id = External_Data_Reference,
         charity_choice = CharityChoice,
         product_choice = ProductChoice) %>%
  right_join(x = ., y = dat_analysis, by = "participant_id")

dat_analysis <- dat_analysis %>% 
  mutate(charity_choice = replace(charity_choice, charity_choice == "Charity of your future choice", NA),
         product_choice = replace(product_choice, product_choice == "Product of your future choice", NA))

dat_analysis <- dat_analysis %>% 
  mutate(exclude_from_all = if_else(is.na(charity_choice) | is.na(product_choice), 1, 0))

###############################################################################
# Create indicator for availability per decision point
###############################################################################

dat_analysis <- dat_analysis %>%
  mutate(availability_1 = 1) %>%
  mutate(availability_2 = case_when(
    sm_flagged == "SM 1" ~ 0,
    is.na(sm_flagged) ~ 1,
    TRUE ~ 1
  )) %>%
  mutate(availability_3 = case_when(
    sm_flagged == "SM 1" ~ 0,
    sm_flagged == "SM 2" ~ 0,
    is.na(sm_flagged) ~ 1,
    TRUE ~ 1
  )) %>%
  mutate(availability_4 = case_when(
    sm_flagged == "SM 1" ~ 0,
    sm_flagged == "SM 2" ~ 0,
    sm_flagged == "SM 3" ~ 0,
    is.na(sm_flagged) ~ 1,
    TRUE ~ 1
  ))

dat_analysis <- dat_analysis %>% 
  arrange(desc(exclude_from_all), availability_1, availability_2, availability_3, availability_4, sm_flagged)

###############################################################################
# Reshape from wide to long
###############################################################################

dat_availability_long <- reshape(data = dat_analysis,
        varying = c("availability_1", "availability_2", "availability_3", "availability_4"),
        direction = "long", idvar = "participant_id", timevar = "survey_number", sep="_")

dat_availability_long <- dat_availability_long %>% arrange(desc(exclude_from_all), participant_id)
row.names(dat_availability_long) <- 1:nrow(dat_availability_long)

###############################################################################
# Save data files
###############################################################################

dat_availability_wide <- dat_analysis

save(dat_availability_wide, file = file.path(path_staged_data, "dat_availability_wide.RData"))
save(dat_availability_long, file = file.path(path_staged_data, "dat_availability_long.RData"))


