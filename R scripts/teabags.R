#### TEABAG / DECOMP TEST SCRIPT

source("R scripts/ThreeD_load_packages.R") 
source("R scripts/ThreeD_create_metadata.R")  
source("R scripts/MSc_aesthetics.R")

## importing data ---> dowload_plan ---------------------------------------
teabag.raw.df <- read_csv("Data/THREE-D_clean_decomposition_fall_2021.csv") 

string.bag.weight.df <- read_excel("Data/additional_info.xlsx", 
                                   sheet = "string_bag_weight") 

tb.reweighed.df <- read_excel("Data/additional_info.xlsx", 
                         sheet = "teabags_reweighed") %>% 
                         mean(.)

###############################################################
## fixup ######################################################
###############################################################

## fixup mini dataset - bag and string weight 
mean_string_bag_mass <- mean(string.bag.weight.df$weight_bag_string) 

## fixup main dataset - teabags
teabag.df <- teabag.raw.df %>%  
  mutate_if(is.character, as.factor) %>% 
  mutate(origBlockID = as.factor(origBlockID),
         origPlotID = as.factor(origPlotID),
         destBlockID = as.factor(destBlockID),
         destPlotID = as.factor(destPlotID)) %>% 
         #tea_type = as.logical(tea_type)) %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% # for better visualization
  # filter out plots with damaged teabags
  mutate(comment_3 = ifelse(is.na(comment_2), 1, 0)) %>% 
  group_by(teabag_ID) %>% 
  mutate(comment_3 = sum(comment_3)) %>% 
  filter(comment_3 == 2) %>% 
  # calculating days buried 
  mutate(days_buried = recover_date - burial_date, 
         days_overdue = days_buried - 90) %>% 
  # reordeing factors 
  mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) %>% 
  mutate(grazing = recode(grazing, 
                          "C" = "Control", "I" = "Intensive", 
                          "M" = "Medium", "N" = "Natural"),
         warming = recode(warming, 
                          "A" = "Ambient", "W" = "Warmed")) %>% 
  # remove weight of bag and string from teabag weight 
  mutate(preburial_weight_g = preburial_weight_g - mean_string_bag_mass,
         post_burial_weight_g = post_burial_weight_g - mean_string_bag_mass,
  # renaming and recalculating mass loss 
         mass_loss_g = weight_loss_g, 
         mass_loss_g = preburial_weight_g - post_burial_weight_g,
  # calculating proportion of mass loss   
         mass_loss_proportion = post_burial_weight_g / preburial_weight_g)


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
# calculating 











## figures ----------------------------------------------------
plot_teabags <- teabag.df %>% 
  group_by(tea_type, origSiteID, warming, Namount_kg_ha_y, grazing) %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = mass_loss_proportion,
                       color = tea_type,
                       fill = tea_type,
                       shape = warming, 
                       size = 10, alpha = 0.7)) +
  geom_point() + 
  theme_minimal(base_size = 20) +
  scale_color_manual(values = colors_tea) + 
  scale_shape_manual(values = c(1, 16)) +
  #scale_size_manual(values = 10) +
  labs(title = "Mass loss of teabags",
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
       y = bquote(Mass~loss~proportion)) +
  #legend(horiz = TRUE) +
  #geom_line() +
  #geom_smooth(method = "lm") +
  facet_grid(origSiteID ~ grazing) 
plot_teabags
