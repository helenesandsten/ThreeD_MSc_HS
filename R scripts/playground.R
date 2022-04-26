### PLAYGROUND SCRIPT ###


knitr::kable(output.roots.wng[,], format="markdown")

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


# adding nitrogen levels to blocks 
mutate(Nlevel = case_when(destBlockID == 1 ~ 1, # new_col = (case_when(old_c == old, ~new))
                          destBlockID == 2 ~ 6, 
                          destBlockID == 3 ~ 5, 
                          destBlockID == 4 ~ 3,
                          destBlockID == 5 ~ 10,
                          destBlockID == 6 ~ 7, 
                          destBlockID == 7 ~ 4, 
                          destBlockID == 8 ~ 8, 
                          destBlockID == 9 ~ 9, 
                          destBlockID == 10~ 2))

mutate(column1 = case_when((column2 == "value_x") ~ "new_value_col1"))
mutate(column1 = case_when((column2 == "value_x" & column3 == "value_y") ~ "new_value_col1"))
       
mutate(origSiteID = case_when((destSiteID == "Lia" & warming == "A") ~ "Lia",
                              (destSiteID == "Joa" & warming == "W") ~ "Lia", 
                              (destSiteID == "Joa" & warming == "A") ~ "Joa",
                              (destSiteID == "Vik" & warming == "W") ~ "Joa"))




