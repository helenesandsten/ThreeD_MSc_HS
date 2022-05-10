#### TEABAG / DECOMP TEST SCRIPT

source("R scripts/ThreeD_load_packages.R") 
source("R scripts/ThreeD_create_metadata.R")  
source("R scripts/MSc_aesthetics.R")

## importing data ---> dowload_plan ---------------------------------------
teabag.raw.df <- read_csv("Data/THREE-D_clean_decomposition_fall_2021.csv") 


string.bag.weight.df <- read_excel("Data/additional_info.xlsx", 
                                   sheet = "string_bag_weight") 

tb.reweighed.df <- read_excel("Data/additional_info.xlsx", 
                         sheet = "teabags_reweighed") 

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
  # filter out damaged teabags 
  mutate(comment_3 = ifelse(is.na(comment_2), 1, 0)) %>% 
  filter(comment_3 == 1) %>% # do not run this line if you want to remove tea bag pairs
  # filter out all teabag pairs with 1 or 2 damaged teabags
  # group_by(teabag_ID) %>% 
  # mutate(comment_3 = sum(comment_3)) %>% 
  # filter(comment_3 == 2) %>% 
  # calculating days buried 
  mutate(days_buried = recover_date - burial_date, 
         days_overdue = days_buried - 90) %>% 
  # reordeing factors 
  mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) %>% 
  mutate(grazing = recode(grazing, 
                          "C" = "Control", "I" = "Intensive", 
                          "M" = "Medium", "N" = "Natural"),
         warming = recode(warming, 
                          "A" = "Ambient", "W" = "Warming")) %>% 
  # changing grazing into cont. variable too reduce degrees of freedom in models
  mutate(grazing_lvl = case_when((grazing == "Control") ~ 0,
                                 (grazing == "Medium") ~ 2,
                                 (grazing == "Intensive") ~ 4)) %>% 
  # remove weight of bag and string from teabag weight 
  mutate(preburial_weight_g = preburial_weight_g - mean_string_bag_mass,
         post_burial_weight_g = post_burial_weight_g - mean_string_bag_mass,
  # renaming and recalculating mass loss 
         mass_loss_g = weight_loss_g, 
         mass_loss_g = preburial_weight_g - post_burial_weight_g,
  # calculating proportion of mass loss   
         mass_loss_proportion = post_burial_weight_g / preburial_weight_g)
  

depth_tb <- teabag.df %>% 
  select(burial_depth_cm)

mean_depth_tb <- mean(teabag.df$burial_depth_cm)

## analysis ---> analysis_plan ---------------------------------------
## TEABAG INDEX -----------------------------------------------------
## additional weightloss of re-dried teabags 


## ALTERNATIVE ANALYSIS OF TEABAG WEIGHTLOSS -------------------
# calculating 


h_g <- 0.842 # hydrolyzable fraction green tea 
h_r <- 0.552 # hydrolyzable fraction rooibos/red tea 

# calculating S from green tea 
teabag.df.new <- teabag.df %>% 
  select(teabag_ID, tea_type, post_burial_weight_g, preburial_weight_g, incubation_time) %>% 
  pivot_wider(names_from = tea_type, 
              values_from = c(post_burial_weight_g, preburial_weight_g)) %>% 
  mutate(a_g = 
           (1 - (post_burial_weight_g_green/preburial_weight_g_green)),
         S = 1 - (a_g/h_g),
         a_r = h_r * (1 - S),
         w_t = (post_burial_weight_g_red/preburial_weight_g_red),
         k = log((a_r / (w_t - (1 - a_r)))/incubation_time)
         )
  
  

# 


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
