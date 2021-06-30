library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")
source("estimator.R")

###############################################################################
# Data preparation steps: format data in a manner expected by the software
###############################################################################

load(file.path(path_staged_data, "dat_analysis.RData"))
dat_analysis <- as.data.frame(dat_analysis)

these_vars <- c("tot_days_with_any_drinks", "typical_num_drinks_per_day",
                "is_female", "is_male", "is_white",
                "baseline_anxiety", "baseline_depression", "baseline_stress",
                "days_elapsed_since_entering")

dat_analysis <- dat_analysis %>% 
  filter(exclude_from_all == 0) %>% 
  mutate(randassign_invite = case_when(
    randassign_invite == "Product" ~ 1,
    randassign_invite == "Charity" ~ 0,
    TRUE ~ NA_real_)) %>%
  select(participant_id, decision_point, 
         randassign_invite, coinflip, Y_delta62,
         all_of(these_vars))

###############################################################################
# Estimate treatment effects: Hypothesis 1
###############################################################################

list_results_H1 <- binary_outcome_moderated_effect(dta = dat_analysis, 
                                                   control_var = c("tot_days_with_any_drinks", 
                                                                   "typical_num_drinks_per_day", 
                                                                   "is_white",
                                                                   "is_female", 
                                                                   "is_male", 
                                                                   "days_elapsed_since_entering"),
                                                   moderator = NULL,
                                                   id_var = "participant_id",
                                                   day_var = "decision_point",
                                                   trt_var = "randassign_invite",
                                                   outcome_var = "Y_delta62",
                                                   avail_var = "coinflip",
                                                   prob_treatment = 1/2,
                                                   significance_level = 0.05)

print(list_results_H1)

###############################################################################
# Estimate treatment effects: Hypothesis 2
###############################################################################

list_results_H2 <- binary_outcome_moderated_effect(dta = dat_analysis, 
                                                   control_var = c("tot_days_with_any_drinks", 
                                                                   "typical_num_drinks_per_day", 
                                                                   "is_white",
                                                                   "is_female", 
                                                                   "is_male", 
                                                                   "days_elapsed_since_entering"),
                                                   moderator = c("baseline_anxiety", "baseline_depression", "baseline_stress"),
                                                   id_var = "participant_id",
                                                   day_var = "decision_point",
                                                   trt_var = "randassign_invite",
                                                   outcome_var = "Y_delta62",
                                                   avail_var = "coinflip",
                                                   prob_treatment = 1/2,
                                                   significance_level = 0.05)

print(list_results_H2)


