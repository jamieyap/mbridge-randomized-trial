#-------------------------------------------------------------------------------
# Data curation pipeline
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
# Data analysis pipeline
#-------------------------------------------------------------------------------

# Run analysis for Primary Aim
source("run-analysis-main.R")
rm(list = ls())

# Run exploratory analyses
source("run-analysis-exploratory-01.R")
rm(list = ls())

source("run-analysis-exploratory-02.R")
rm(list = ls())

source("run-analysis-exploratory-03.R")
rm(list = ls())

# Note that dataset for time-to-event analyses is created here
source("run-analysis-exploratory-04.R")
rm(list = ls())

#-------------------------------------------------------------------------------
# Display results of data analyses in a PDF file using R markdown
#-------------------------------------------------------------------------------
rmarkdown::render("summary-stats.Rmd")
rm(list = ls())

rmarkdown::render("get-results-main.Rmd")
rm(list = ls())

rmarkdown::render("get-results-exploratory-01.Rmd")
rm(list = ls())

rmarkdown::render("get-results-exploratory-02.Rmd")
rm(list = ls())

rmarkdown::render("get-results-exploratory-03.Rmd")
rm(list = ls())

rmarkdown::render("get-results-exploratory-04.Rmd")
rm(list = ls())

