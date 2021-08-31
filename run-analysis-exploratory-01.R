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
         randassign_invite, coinflip, Y_delta47,
         hrs_elapsed_invite_to_first_reminder, ALCdrink,
         all_of(these_vars))

dat_analysis <- dat_analysis %>%
  mutate(is_dp_early = if_else(decision_point==1 | decision_point==2, 1, 0),
         is_dp_late = if_else(decision_point==3 | decision_point==4, 1, 0))

###############################################################################
# Run exploratory analysis#1
###############################################################################

list_results_supp <- binary_outcome_moderated_effect(dta = dat_analysis, 
                                                     control_var = c("tot_days_with_any_drinks", 
                                                                     "typical_num_drinks_per_day", 
                                                                     "is_white",
                                                                     "is_female", 
                                                                     "is_male", 
                                                                     "days_elapsed_since_entering"),
                                                     moderator = c("is_dp_early"),
                                                     id_var = "participant_id",
                                                     day_var = "decision_point",
                                                     trt_var = "randassign_invite",
                                                     outcome_var = "Y_delta47",
                                                     avail_var = "coinflip",
                                                     prob_treatment = 1/2,
                                                     significance_level = 0.05)


significance_level <- 0.05
p <- list_results_supp[["dims"]]$p
q <- list_results_supp[["dims"]]$q
n <- list_results_supp[["sample_size"]]
all_estimates <- c(list_results_supp[["alpha_hat"]], list_results_supp[["beta_hat"]])
all_std_err <- c(list_results_supp[["alpha_se_ssa"]], list_results_supp[["beta_se_ssa"]])
test_stat <- all_estimates / all_std_err
critical_value <- qt(1 - significance_level/2, df = n - p - q) # two-sided
p_val <- lapply(test_stat, function(x){
  out <- 2 * pt(abs(x), df = n - p - q, lower.tail = FALSE) # two-sided
  return(out)
})
p_val <- do.call(rbind, p_val)

dat_results_supp <- data.frame(estimate = all_estimates,
                               std_err = all_std_err,
                               p = p_val)
dat_results_supp <- round(dat_results_supp, digits = 3)
row.names(dat_results_supp) <- c("Intercept", 
                                 "No. of Days with any drinks", 
                                 "No. of Drinks per day", 
                                 "White (1=Yes, 0=otherwise)", 
                                 "Female (1=Yes, 0=otherwise)",
                                 "Male (1=Yes, 0=otherwise)",
                                 "No. of Days elapsed since entering",
                                 "beta0",
                                 "beta1 (coefficient for is_dp_early)")

