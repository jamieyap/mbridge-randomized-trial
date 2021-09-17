library(dplyr)
library(readxl)

source("paths.R")
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
                                                   outcome_var = "Y_delta47",
                                                   avail_var = "coinflip",
                                                   prob_treatment = 1/2,
                                                   significance_level = 0.05)


significance_level <- 0.05
p <- list_results_H1[["dims"]]$p
q <- list_results_H1[["dims"]]$q
n <- list_results_H1[["sample_size"]]
all_estimates <- c(list_results_H1[["alpha_hat"]], list_results_H1[["beta_hat"]])
all_std_err <- c(list_results_H1[["alpha_se_ssa"]], list_results_H1[["beta_se_ssa"]])
test_stat <- all_estimates / all_std_err
critical_value <- qt(1 - significance_level/2, df = n - p - q) # two-sided
p_val <- lapply(test_stat, function(x){
  out <- 2 * pt(abs(x), df = n - p - q, lower.tail = FALSE) # two-sided
  return(out)
})
p_val <- do.call(rbind, p_val)

dat_results_H1 <- data.frame(estimate = all_estimates,
                             std_err = all_std_err,
                             p = p_val)
dat_results_H1 <- round(dat_results_H1, digits = 3)
row.names(dat_results_H1) <- c("Intercept", 
                               "No. of Days with any drinks", 
                               "No. of Drinks per day", 
                               "White (1=Yes, 0=otherwise)", 
                               "Female (1=Yes, 0=otherwise)",
                               "Male (1=Yes, 0=otherwise)",
                               "No. of Days elapsed since entering",
                               "beta0")

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
                                                   outcome_var = "Y_delta47",
                                                   avail_var = "coinflip",
                                                   prob_treatment = 1/2,
                                                   significance_level = 0.05)


significance_level <- 0.05
p <- list_results_H2[["dims"]]$p
q <- list_results_H2[["dims"]]$q
n <- list_results_H2[["sample_size"]]
all_estimates <- c(list_results_H2[["alpha_hat"]], list_results_H2[["beta_hat"]])
all_std_err <- c(list_results_H2[["alpha_se_ssa"]], list_results_H2[["beta_se_ssa"]])
test_stat <- all_estimates / all_std_err
critical_value <- qt(1 - significance_level/2, df = n - p - q) # two-sided
p_val <- lapply(test_stat, function(x){
  out <- 2 * pt(abs(x), df = n - p - q, lower.tail = FALSE) # two-sided
  return(out)
})
p_val <- do.call(rbind, p_val)

dat_results_H2 <- data.frame(estimate = all_estimates,
                             std_err = all_std_err,
                             p = p_val)
dat_results_H2 <- round(dat_results_H2, digits = 3)
row.names(dat_results_H2) <- c("Intercept", 
                               "No. of Days with any drinks", 
                               "No. of Drinks per day", 
                               "White (1=Yes, 0=otherwise)", 
                               "Female (1=Yes, 0=otherwise)",
                               "Male (1=Yes, 0=otherwise)",
                               "No. of Days elapsed since entering",
                               "beta0",
                               "beta1 (Coefficient for treatment x baseline anxiety)",
                               "beta2 (Coefficient for treatment x baseline depression)",
                               "beta3 (Coefficient for treatment x baseline stress)")



