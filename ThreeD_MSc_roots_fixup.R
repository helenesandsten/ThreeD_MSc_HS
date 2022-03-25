### CLEANING DATA - ROOT INGROWTH CORES

source("ThreeD_create_metadata.R")
source("ThreeD_load_packages.R")

## import data (excel) --------------------------------------------------

roots.df1 <- read_excel("Data/ThreeD_rootingrowthcores_2021.xlsx") 

## clean data ----------------------------------------------------------
# fixing dates
roots.df <- roots.df1 %>% 
  mutate(dateRIC_washed = ymd(dateRIC_washed)) %>% # change format to date
  mutate(date_roots_dried = ymd(date_roots_dried)) %>% # change format to date
  mutate(root_mass_g = total_mass_g - alutray_mass_g) %>% # new column with root mass 
  mutate_if(is.character, as.factor)  # change to factor 


### CSV FILES ----------------------------------------------------------

# ## import data
# roots.df <- read_csv2(file = "Data/ThreeD_rootingrowthcores_2021.csv") 
# 
# ## cleaning dataframe
# roots.df2 <- roots.df %>% 
#   select(-(c("X15", "X16", "X17", "X18"))) # remove excess columns
  
