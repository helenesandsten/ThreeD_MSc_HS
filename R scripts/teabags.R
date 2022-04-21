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
         destPlotID = as.factor(destPlotID),
         tea_type = as.logical(tea_type)) %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% # for better visualization
  # remove teabag weight 
  # mutate(preburial_weight_g = preburial_weight_g - XXX,
  #        post_burial_weight_g = post_burial_weight_g - XXX)
  # filter out plots with damaged teabags
  mutate(comment_3 = ifelse(is.na(comment_2), 1, 0)) %>% 
  group_by(teabag_ID) %>% 
  mutate(comment_3 = sum(comment_3)) %>% 
  filter(comment_3 == 2) %>% 
  # calculating days buried 
  mutate(days_buried = recover_date - burial_date)


## analysis ---> analysis_plan ---------------------------------------

## Equations and numbers from Keuskamp et al. 2013
## calculating decomposition rates

h_g <- 0.842 # hydrolyzable fraction green tea 
h_r <- 0.552 # hydrolyzable fraction rooibos/red tea 



tbi.g.df <- teabag.df %>% # decomposable fraction green tea 
  mutate(a_g = case_when(tea_type ~ "green",
                         TRUE ~ "red")) %>% 
  mutate(s = 1 - a_g/post_burial_weight_g) 
  

  # mutate(filter(tea_type == "green"), a_g = post_burial_weight_g)
  # 
  # filter(tea_type == "green") %>%  
  # select(post_burial_weight_g) %>% 
  # mutate(S = 1 - post_burial_weight_g/H_g) 


# 





# t <- tb.days.buried$days_buried
# w <- tb.days.buried$post_burial_weight_g
# #a <- 
# # decomposition rate of labile fraction 
# k1 <- 
# # decomposition rate of recalcitrant fraction 
# k2 <- 


## figures ---> figure_plan ---------------------------------------