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
         #tea_type = as.logical(tea_type)) %>% 
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
  mutate(days_buried = recover_date - burial_date) %>% 
  mutate(days_overdue = days_buried - 90) %>% 
  mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) %>% 
  mutate(grazing = recode(grazing, 
                          "C" = "Control", "I" = "Intensive", 
                          "M" = "Medium", "N" = "Natural"),
         warming = recode(warming, 
                          "A" = "Ambient", "W" = "Warmed")) 


## analysis ---> analysis_plan ---------------------------------------
## TEABAG INDEX -----------------------------------------------------
## Equations and numbers from Keuskamp et al. 2013
## calculating decomposition rates

# h_g <- 0.842 # hydrolyzable fraction green tea 
# h_r <- 0.552 # hydrolyzable fraction rooibos/red tea 
# 
# # dataset with green tea 
# tbi.g.df <- teabag.df %>% 
#   filter(tea_type == "green") %>% 
#   # calculating s from a_g with eqn: S = 1 - (a_g/h_g) 
#   # a_g = post_burial_weight_g 
#   mutate(s = 1 - post_burial_weight_g/h_g) 
# 
# ## NEGATIVE S-VALUES --> NOT GREAT 
# ## perhaps not possible to calculate decomposition rate?
# 
# # dataset with red tea 
# tbi.r.df <- teabag.df %>% 
#   filter(tea_type == "red") %>% 
#   # calculating a_r from S with eqn: a_r = h_r (1 - S) 
#   mutate(a_r = h_r (1 - S))

## ALTERNATIVE ANALYSIS OF TEABAG WEIGHTLOSS -------------------

plot_teabags <- teabag.df %>% 
  group_by(tea_type, origSiteID, warming, Namount_kg_ha_y, grazing) %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = weight_loss_g,
                       #color = tea_type,
                       fill = tea_type)) +
  geom_point() + 
  theme_minimal(base_size = 20) +
  scale_fill_manual(values = colors_tea) + 
  labs(title = "Mass loss of tebags", 
       x = bquote(Origin~site), 
       y = bquote(Mass~loss~(g))) +
  facet_grid(origSiteID ~ grazing) 
  #geom_smooth(method = "lm")
plot_teabags










## figures ---> figure_plan ---------------------------------------