#-------------------------------------------------------------------------------
# Data curation pipeline
#-------------------------------------------------------------------------------

# Create a data frame containing all participant ID's of participants who
# were randomized to either 'Experimental Early' or 'Experimental Late'
# i.e., those participants NOT belonging to the control group
# Additionally, create an indicator for whether randomization should occur
source("construct-mrt-dataset/construct-masterlist.R")
rm(list = ls())

# Create baseline scores
source("construct-mrt-dataset/get-baseline.R")
rm(list = ls())

# Parse out data on invites
source("construct-mrt-dataset/get-invites.R")
rm(list = ls())

# Parse out data on reminders
source("construct-mrt-dataset/get-reminders.R")
rm(list = ls())

# Parse out survey responses
source("construct-mrt-dataset/get-self-monitoring-survey-responses.R")
rm(list = ls())

# Merge various kinds of data into one data frame
source("construct-mrt-dataset/merge-data.R")
rm(list = ls())

# Construct primary outcome
source("construct-mrt-dataset/construct-primary-outcome.R")
rm(list = ls())

#-------------------------------------------------------------------------------
# Display results of data analyses in a PDF file using R markdown
#-------------------------------------------------------------------------------
rmarkdown::render("analysis/do-main-analysis.Rmd")
rm(list = ls())
