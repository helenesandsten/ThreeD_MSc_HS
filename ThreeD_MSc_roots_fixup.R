### CLEANING DATA - ROOT INGROWTH CORES

source("ThreeD_create_metadata.R")
source("ThreeD_load_packages.R")

## import data
roots.df <- read_csv2(file = "Data/ThreeD_rootingrowthcores_2021.csv") 

## cleaning dataframe
roots.df2 <- roots.df %>% 
  select(-(c("X15", "X16", "X17", "X18"))) # remove excess columns
  
