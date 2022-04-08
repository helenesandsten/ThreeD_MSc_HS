### TEST SCRIPT 

source("R scripts/ThreeD_create_metadata.R")
source("R scripts/ThreeD_load_packages.R")


## importing soil data -------------------------------------------
soil.raw.df <- read_csv2(file = "Data/ThreeD_soilcores_2021.csv")

## fixing raw soil data -------------------------------------------
soil.df <- soil.raw.df %>% 
  select(-c(X14, X15, X16, X17, X18)) %>% 
  mutate(alutray_ID = str_replace(alutray_ID, "  ", " ")) %>% 
  mutate(burn_mass1 = if_else(
    alutray_ID == "Vik W B5 I" & burn_mass1 == 11.5771, # row and col you want to change
    12.5771, # what you change it to 
    burn_mass1)) %>% # this needs to be here, ask Aud
  separate(col = alutray_ID, # separate column into several
           into = c("destSiteID", "warming", "destBlockID", "grazing"), " ") %>% 
  mutate(destBlockID = as.numeric(str_remove(destBlockID, "B")), # remove letter B
         destSiteID) %>% 
  select(-turfID) %>% # remove column 'turfID' because it is empty
  mutate_if(is.character, as.factor) %>% # change to factor
  mutate(#destPlotID = as.numeric(destPlotID),
         destBlockID = as.numeric(destBlockID),
         destSiteID = as.character(destSiteID)) %>% 
  # change to into correct class to merge with metaTurfID
  rename(alutray_mass = alutray_weight, # new name = old name
         wetmass = wet_mass_g,
         drymass_1_55 = dry_mass1, 
         drymass_2_sieved = dry_mass2, 
         drymass_3_sieved_105 = dry_mass3, 
         drymass_4_87 = dry_mass4, 
         porcelain_mass = porcelain_weight,
         burnmass_1_550 = burn_mass1, 
         burnmass_2_950 = burn_mass2,
         root_stone_mass = total_cf_mass) %>% 
  #merge(metaTurfID, by = c("destBlockID","warming", "grazing"), all.X = TRUE) 

  mutate(Nlevel = case_when(destBlockID == 1 ~ 1, # new_col = (case_when(old_c == old, ~new))
                            destBlockID == 2 ~ 6, 
                            destBlockID == 3 ~ 5, 
                            destBlockID == 4 ~ 3,
                            destBlockID == 5 ~ 10,
                            destBlockID == 6 ~ 7, 
                            destBlockID == 7 ~ 4, 
                            destBlockID == 8 ~ 8, 
                            destBlockID == 9 ~ 9, 
                            destBlockID == 10~ 2)) %>% 
  left_join(NitrogenDictionary, by = "Nlevel") %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% 
  mutate(origSiteID = case_when((destSiteID == "Lia" & warming == "A") ~ "Lia",
                                (destSiteID == "Joa" & warming == "W") ~ "Lia", 
                                (destSiteID == "Joa" & warming == "A") ~ "Joa",
                                (destSiteID == "Vik" & warming == "W") ~ "Joa"))

                             
  
  
  
  # cant merge/join datasets
  #full_join(metaTurfID, by = c("destBlockID", "warming", "grazing")) 
  




  
  # if_else(destBlockID == 1, Nlevel = 1,
  #         destBlockID == 2, Nlevel = 6,
  #         destBlockID == 3, Nlevel = 5,
  #         destBlockID == 4, Nlevel = 3,
  #         destBlockID == 5, Nlevel = 10,
  #         destBlockID == 6, Nlevel = 7,
  #         destBlockID == 7, Nlevel = 4,
  #         destBlockID == 8, Nlevel = 8,
  #         destBlockID == 9, Nlevel = 9,
  #         destBlockID == 10, Nlevel = 2)
  # 


#full_join(metaTurfID, by = c("destSiteID", "warming", "destBlockID", "grazing")) 
  # 
  #left_join(NitrogenDictionary, by = "Nlevel") %>% 
  # 
  # mutate(turfID = paste0(
  #   origPlotID, " ", warming, "N", Nlevel, grazing,  " ", destPlotID)) %>% 
  # 
  # mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1))

View() 







# 
# ## VISUALIZING DATA
# soil_plot_1 <- ggplot(data = soil.df, aes(x = burn_mass1, y = burn_mass2)) +
#        geom_point(color = "blue", size = 3, alpha = 0.5) 
# soil_plot_1
# 
# 
# ## FILTER OUT OUTLIERS FROM PLOT
# ## INVESTIGATING WHY THEY ARE OUTLIERS
# soil.df %>% 
#   filter(burn_mass1>12, burn_mass2<11.5) %>% # bottom outlier
#   select(alutray_ID) # VIK W B7 C
# 
# soil.df %>% 
#   filter(burn_mass1<12, burn_mass2>12) %>% # top outlier
#   select(alutray_ID) # VIK W B5 I 
#   




