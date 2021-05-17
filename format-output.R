library(dplyr)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")
path_output_data <- Sys.getenv("path_output_data")

load(file = file.path(path_staged_data, "dat_primary_aim_wide.RData"))
load(file = file.path(path_staged_data, "dat_primary_aim_long.RData"))

write.csv(dat_primary_aim_wide, file = file.path(path_staged_data, "dat_primary_aim_wide.csv"), row.names = FALSE, na = "")
write.csv(dat_primary_aim_long, file = file.path(path_staged_data, "dat_primary_aim_long.csv"), row.names = FALSE, na = "")

