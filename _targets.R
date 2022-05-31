##

library("targets") 
library("tarchetypes") 

tar_option_set(packages = c(
  "stringi", 
  "tidyverse", 
  "lubridate", 
  "stringi", 
  "readxl", 
  "readr", 
  "usethis", 
  "broom", 
  "performance", 
  "see", 
  "knitr",  
  "kableExtra", 
  "targets", 
  "tarchetypes", 
  "MuMIn", 
  # "english",
  "tibble"))

library("tibble")
library("tidyverse")

options(na.action = "na.fail")
# options(scipen = 100, digits = 4)

# source other scripts
source("R scripts/ThreeD_create_metadata.R") 
source("R scripts/ThreeD_load_packages.R") 

# source target plans - can also construct plans directly in this file.
source("R scripts/download_plan.R") 
source("R scripts/transformation_plan.R") 
source("R scripts/analysis_plan.R")
# source("R scripts/figure_plan.R") 
# source("R scripts/manuscript_plan.R") 


# Combine target plans
combined_plan <- c(
  download_plan,
  tranformation_plan,
  analysis_plan#,
  # figure_plan,
  # manuscript_plan
)

