#### TEABAG / DECOMP TEST SCRIPT

source("R scripts/ThreeD_load_packages.R") 
source("R scripts/ThreeD_create_metadata.R") 

## importing data ---> dowload_plan ---------------------------------------
teabag.raw.df <- read_csv("Data/THREE-D_clean_decomposition_fall_2021.csv") 

## fixup ---> transformation_plan ---------------------------------------
teabag.df <- teabag.raw.df %>%  
  mutate_if(is.character, as.factor) %>% 
  mutate(origBlockID = as.factor(origBlockID),
         origPlotID = as.factor(origPlotID),
         destBlockID = as.factor(destBlockID),
         destPlotID = as.factor(destPlotID)) %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1))  # for better visualization

## analysis ---> analysis_plan ---------------------------------------
## calculating days buried 
tb.days.buried <- teabag.df %>% 
  mutate(days_buried = recover_date - burial_date)

## calculating decomposition rates


t <- tb.days.buried$days_buried
w <- tb.days.buried$post_burial_weight_g
#a <- 
# decomposition rate of labile fraction 
k1 <- 
# decomposition rate of recalcitrant fraction 
k2 <- 


## figures ---> figure_plan ---------------------------------------