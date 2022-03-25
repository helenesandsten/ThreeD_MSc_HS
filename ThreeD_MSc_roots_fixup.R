### CLEANING DATA - ROOT INGROWTH CORES

source("ThreeD_create_metadata.R")
source("ThreeD_load_packages.R")

## import data (excel) --------------------------------------------------

roots.df1 <- read_excel("Data/ThreeD_rootingrowthcores_2021.xlsx") 
names(roots.df1)
str(roots.df1)

View(roots.df1)


## clean data ----------------------------------------------------------
# 
# roots.df1 %>% 
#   
#   separate(col = dateRIC_washed, # separate column into several
#            into = c("year", "month", "day"), ".") %>% 
#   str(roots.df1)
  

roots.df2 <- roots.df1 %>% 
  mutate(root_mass_g = total_mass_g - alutray_mass_g) %>% # new column with root mass 
  mutate(dateRIC_washed1 = str_remove(dateRIC_washed, "\\.$")) %>% # remove last dot from date
  mutate(date_roots_dried1 = str_remove(date_roots_dried, "\\.$")) %>% 
  select(-c(dateRIC_washed, date_roots_dried))  # deleting columns 
  #mutate_if(is.character, as.factor) %>% # change to factor

str(roots.df2)

roots.df2 <- ymd(dateRIC_washed1)

#mutate(dateRIC_washed = as.numeric(str_remove(dateRIC_washed, ".")), dateRIC_washed)



### CSV FILES ----------------------------------------------------------

# ## import data
# roots.df <- read_csv2(file = "Data/ThreeD_rootingrowthcores_2021.csv") 
# 
# ## cleaning dataframe
# roots.df2 <- roots.df %>% 
#   select(-(c("X15", "X16", "X17", "X18"))) # remove excess columns
  
