library(dplyr)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")
path_output_data <- Sys.getenv("path_output_data")

load(file = file.path(path_staged_data, "primary_analysis_outcome.RData"))

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
# Write to csv file
###############################################################################
write.csv(dat_analysis, file.path(path_output_data, "primary_analysis_outcome.csv"), na = "", row.names = FALSE)


