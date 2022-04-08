#### TEABAG / DECOMP TEST SCRIPT

source("R scripts/ThreeD_load_packages.R") 
source("R scripts/ThreeD_create_metadata.R") 

## importing data ---> dowload_plan
teabag.raw.df <- read_csv("Data/THREE-D_clean_decomposition_fall_2021.csv") 

## fixup ---> transformation_plan
teabag.df <- teabag.raw.df %>%  
  mutate_if(is.character, as.factor) %>% 
  mutate(origBlockID = as.factor(origBlockID),
         origPlotID = as.factor(origPlotID),
         destBlockID = as.factor(destBlockID),
         destPlotID = as.factor(destPlotID)) %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) # for better visualization

## analysis ---> analysis_plan


## figures ---> figure_plan