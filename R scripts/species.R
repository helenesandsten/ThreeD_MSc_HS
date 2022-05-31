## species 

## importing raw data 
species.raw.df <- read_delim("Data/THREE-D_CommunitySubplot_2019_2020.csv",
                             delim = ",") 

## cleaning data 
species.df <- species.raw.df %>%  
  select(year, turfID, origSiteID, origBlockID, warming, grazing, Nlevel, 
         species, #subplot, 
         variable, value) %>% 
  group_by(#year, 
           turfID, 
           #origSiteID, origBlockID, warming, grazing, Nlevel, 
           species, #subplot, 
           #variable, 
           value
           ) %>% 
  filter(warming == "A",
         grazing == "C",
         Nlevel == 1 | Nlevel == 2 | Nlevel == 3) %>% 
  filter(variable == "cover") %>% 
  select(turfID, species, value
         ) %>% 
  unique()



