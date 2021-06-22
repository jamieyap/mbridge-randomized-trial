# Create a data frame containing all participant ID's of participants who
# were randomized to either 'Experimental Early' or 'Experimental Late'
# i.e., those participants NOT belonging to the control group
# Additionally, create an indicator for whether randomization should occur
source("construct-masterlist.R")
rm(list = ls())

# Create baseline scores
source("get-baseline.R")
rm(list = ls())

# Parse out data on invites
source("get-invites.R")
rm(list = ls())

# Parse out data on reminders
source("get-reminders.R")
rm(list = ls())

# Parse out survey responses
source("get-self-monitoring-survey-responses.R")
rm(list = ls())

# Construct data for analysis
source("construct-data-for-analysis.R")
rm(list = ls())

