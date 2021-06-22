library(dplyr)
library(readxl)

path_input_data <- Sys.getenv("path_input_data")
path_staged_data <- Sys.getenv("path_staged_data")

###############################################################################
# Load data for analysis after executing run-data-curation-pipeline.R
###############################################################################

load(file.path(path_staged_data, "dat_analysis.RData"))

