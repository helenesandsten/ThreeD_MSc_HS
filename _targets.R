##

library("targets")
library("tarchetypes")

tar_option_set(packages = c("tidyverse", "readxl", "lubridate", "readr", "stringi"))

# source other scripts
#source("R scripts/ThreeD_create_metadata.R")
#source("ThreeD_load_packages.R")

# source target plans - can also construct plans directly in this file.
#source("R scripts/download_plan.R")
source("R scripts/transformation_plan.R")
#source("R scripts/analysis_plan.R")
#source("R scripts/figure_plan.R")
#source("R scripts/manuscript_plan.R")


#Combine target plans
combined_plan <- c(
  #download_plan,
  tranformation_plan
  #analysis_plan,
  #figure_plan
  # manuscript_plan
)

