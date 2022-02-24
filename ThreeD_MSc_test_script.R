### TEST SCRIPT 

source("ThreeD_create_metadata.R")
source("ThreeD_load_packages.R")
metaTurfID

## IMPORTING DATAFRAME 
soil.df <- read_csv2(file = "Data/ThreeD_soilcores_2021.csv")
str(soil.df) 
names(soil.df) 

## VISUALIZING DATA
soil_plot_1 <- ggplot(data = soil.df, aes(x = burn_mass1, y = burn_mass2)) +
       geom_point(color = "blue", size = 3, alpha = 0.5) 
soil_plot_1


## FILTER OUT OUTLIERS FROM PLOT
## INVESTIGATING WHY THEY ARE OUTLIERS
soil.df %>% 
  filter(burn_mass1>12, burn_mass2<11.5) %>% # bottom outlier
  select(alutray_ID) # VIK W B7 C

soil.df %>% 
  filter(burn_mass1<12, burn_mass2>12) %>% # top outlier
  select(alutray_ID) # VIK W B5 I 
  

## RECREATE NEW DATAFRAME AND FIX ERRORS
soil.df2 <- soil.df %>% 
  mutate(alutray_ID = str_replace(alutray_ID, "  ", " ")) %>% 
  mutate(burn_mass1 = if_else(
    alutray_ID == "VIK W B5 I" & burn_mass1 == 11.5771, # row and col you want to change
    12.5771, # what you change it to 
    burn_mass1)) %>% # this needs to be here, ask Aud
  separate(col = alutray_ID, # separate column into several
           into = c("destSiteID", "warming", "destBlockID", "grazing"), " ") %>% 
  mutate(destBlockID = as.numeric(str_remove(destBlockID, "B")), # remove letter B
         destSiteID) %>% 
  select(-turfID) %>% # remove column 'turfID' 
  anti_join(metaTurfID, # checks out what is not joined
            by = c("destSiteID", "warming", "destBlockID", "grazing")) %>% 
  mutate_if(is.character, as.factor) %>% # change to factor
  rename(alutray_mass = alutray_weight, # new name = old name
         wetmass = wet_mass_g,
         drymass_1_55 = dry_mass1, 
         drymass_2_sieved = dry_mass2, 
         drymass_3_sieved_105 = dry_mass3, 
         drymass_4_87 = dry_mass4, 
         porcelain_mass = porcelain_weight,
         burnmass_1_550 = burn_mass1, 
         burnmass_2_950 = burn_mass2,
         root_stone_mass = total_cf_mass)
View(soil.df2)
  
# filter(alutray_ID == "VIK W B5 I")
# soil.df %>% distinct(alutray_ID) %>% pn

## SAVE AS NEW CSV FILE

