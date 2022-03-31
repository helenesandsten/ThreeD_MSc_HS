### PLAYGROUND SCRIPT ###

## import data (excel)
roots.raw.df <- read_excel("Data/ThreeD_rootingrowthcores_2021.xlsx")
## clean data
roots.clean.df <- roots.raw.df %>%
  mutate(dateRIC_washed = ymd(dateRIC_washed),  # change format to date
         date_roots_dried = ymd(date_roots_dried), # change format to date
         root_mass_g = total_mass_g - alutray_mass_g, # new column with root mass
         root_mass_cm3 = (root_mass_g/(pi*(1.6)^2*RIC_length_cm))) %>% # calculate volume
  mutate_if(is.character, as.factor) %>% 
  left_join(NitrogenDictionary, by = "Nlevel") 
  # make log(Namount_kg_ha_y +1) into factor



