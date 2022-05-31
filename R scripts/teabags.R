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
         mass_loss_proportion = 1 - (post_burial_weight_g / preburial_weight_g)) 
  

# depth_tb <- teabag.df %>% select(burial_depth_cm)
# mean_depth_tb <- mean(teabag.df$burial_depth_cm)


### NEW TEABAG INDEX 
Hydrolysable_fraction_green <- 0.842
Hydrolysable_fraction_red <- 0.552

decomp.df <- teabag.df %>% 
  select(teabag_ID, tea_type, post_burial_weight_g, preburial_weight_g, incubation_time,
         origSiteID, destSiteID, destBlockID, origBlockID, warming, grazing, grazing_lvl,
         Namount_kg_ha_y, recover_date, burial_date) %>%
  # rearranging data to have teabag pairs on same row for calculations
  pivot_wider(names_from = tea_type, 
              values_from = c(post_burial_weight_g, preburial_weight_g)) %>%
  mutate(incubation_time = as.numeric(recover_date - burial_date), 
         # calculations for the teabag index
         fraction_decomposed_green = 1 - post_burial_weight_g_green/preburial_weight_g_green,
         fraction_remaining_green = post_burial_weight_g_green/preburial_weight_g_green,
         fraction_remaining_red = post_burial_weight_g_red/preburial_weight_g_red,
         # stabilisation rate 
         S = 1 - (fraction_decomposed_green / Hydrolysable_fraction_green),
         predicted_labile_fraction_red = Hydrolysable_fraction_red * (1 - S),
         # decomposition rate 
         k = log(predicted_labile_fraction_red / (fraction_remaining_red - (1 - predicted_labile_fraction_red))) / incubation_time)


## Models will be fitted for each dataset 

## proportion mass loss green tea
## Making dataset for alpine site
tea.green.alp.df <- teabag.df %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural") %>% 
  filter(origSiteID == "Lia") 
## Making dataset for sub-alpine site 
tea.green.sub.df <- teabag.df %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural") %>% 
  filter(origSiteID == "Joa")  

## proportion mass loss rooibos tea
## Making dataset for alpine site
tea.red.alp.df <- teabag.df %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural") %>% 
  filter(origSiteID == "Lia") 
## Making dataset for sub-alpine site 
tea.red.sub.df <- teabag.df %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural") %>% 
  filter(origSiteID == "Joa")

## decomposition rate 
## Making dataset for alpine site
decomp.k.alp.df <- decomp.df %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural") %>% 
  filter(!is.na(k)) %>% 
  filter(origSiteID == "Lia") 
# ## Making dataset for sub-alpine site 
# decomp.k.sub.df <- decomp.df %>% 
#   filter(origSiteID == "Joa") 

## stabilizations factor 
## Making dataset for alpine site
decomp.s.alp.df <- decomp.df %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural") %>% 
  filter(!is.na(S)) %>% 
  filter(origSiteID == "Lia") 
## Making dataset for sub-alpine site 
decomp.s.sub.df <- decomp.df %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural") %>% 
  filter(!is.na(S)) %>% 
  filter(origSiteID == "Joa") 



## figures ----------------------------------------------------

## PLOT MASS LOSS GREEN TEA ---------------------------------
plot_teabags_green <- teabag.df %>%
  group_by(turfID, tea_type, origSiteID, warming, Namount_kg_ha_y, grazing) %>%
  #filter(!turfID == "1 WN1M 84") %>%
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>%
  # filter(!grazing == "Natural") %>%
  mutate(tea_w_color = case_when(
    (tea_type == "green") ~ "Green tea",
    (tea_type == "red") ~ "Rooibos tea")) %>%
  # filter(!tea_w_color == "green_a" | !tea_w_color == "red_a") %>%
  filter(tea_type == "green") %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1),
                       y = mass_loss_proportion,
                       color = warming,
                       fill = warming,
                       #linetype = warming,
                       shape = warming)
  ) +
  geom_point(size = 2) +
  theme_minimal(base_size = 16) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_color_manual(values = colors_w) +
  scale_fill_manual(values = colors_w) +
  scale_shape_manual(values = c(21, 25)) +
  ylim(0.25, 1) +
  labs(title = "",
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
       y = bquote(Mass~loss~proportion)) +
  # geom_line() +
  geom_smooth(method = "lm") +
  facet_grid(origSiteID ~ grazing)
#plot_teabags_green

# ggsave('plot_msc_tb_green.png', 
#        plot_teabags_green, 
#        bg='transparent')


## PLOT MASS LOSS ROOIBOS TEA ---------------------------------
plot_teabags_red <- teabag.df %>%
  group_by(turfID, tea_type, origSiteID, warming, Namount_kg_ha_y, grazing) %>%
  #filter(!turfID == "1 WN1M 84") %>%
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>%
  #filter(!grazing == "Natural") %>%
  mutate(tea_w_color = case_when(
    (tea_type == "green") ~ "Green tea",
    (tea_type == "red") ~ "Rooibos tea")) %>%
  # filter(!tea_w_color == "green_a" | !tea_w_color == "red_a") %>% 
  filter(tea_type == "red") %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1),
                       y = mass_loss_proportion,
                       color = warming,
                       fill = warming,
                       #linetype = warming,
                       shape = warming)
  ) +
  geom_point(size = 2) + 
  theme_minimal(base_size = 16) + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.box = "horizontal") +
  scale_color_manual(values = colors_w) + 
  scale_fill_manual(values = colors_w) + 
  scale_shape_manual(values = c(21, 25)) + 
  ylim(0.25, 1) +
  labs(title = "",
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
       y = bquote(Mass~loss~proportion)) + 
  # geom_line() +
  geom_smooth(method = "lm") +
  facet_grid(origSiteID ~ grazing) 
#plot_teabags_red

# ggsave('plot_msc_tb_red.png', 
#        plot_teabags_red, 
#        bg='transparent')

## PLOT DEOMPOSITION RATE k ---------------------------------
plot_decomp_k_all <- decomp.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  #filter(!grazing == "Natural") %>%
  # filter(!origSiteID == "Sub-alpine") %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = k, 
                       color = warming,
                       fill = warming,
                       #linetype = warming,
                       shape = warming)) +
  geom_point(size = 2) + 
  theme_minimal(base_size = 16) + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.box = "horizontal") +
  scale_color_manual(values = colors_w) + 
  scale_fill_manual(values = colors_w) + 
  scale_shape_manual(values = c(21, 25)) + 
  #scale_linetype_manual(values = c("longdash", "solid")) + 
  labs(title = "", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(k)) + 
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
#plot_decomp_k_all

# ggsave('plot_msc_dec_k_all.png',
#        plot_decomp_k_all,
#        bg='transparent')

plot_decomp_k_cmi_alp <- decomp.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  filter(!grazing == "Natural") %>%
  filter(!origSiteID == "Sub-alpine") %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = k, 
                       color = warming,
                       fill = warming,
                       #linetype = warming,
                       shape = warming)) +
  geom_point(size = 2) + 
  theme_minimal(base_size = 16) + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.box = "horizontal") +
  scale_color_manual(values = colors_w) + 
  scale_fill_manual(values = colors_w) + 
  scale_shape_manual(values = c(21, 25)) + 
  #scale_linetype_manual(values = c("longdash", "solid")) + 
  labs(title = "", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(k)) + 
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
#plot_decomp_k_cmi_alp

# ggsave('plot_msc_dec_cmi_alp.png',
#        plot_decomp_k_cmi_alp,
#        bg='transparent')

plot_decomp_k_all_sub <- decomp.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  #filter(!grazing == "Natural") %>%
  filter(!origSiteID == "Alpine") %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = k, 
                       color = warming,
                       fill = warming,
                       #linetype = warming,
                       shape = warming)) +
  geom_point(size = 2) + 
  theme_minimal(base_size = 16) + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.box = "horizontal") +
  scale_color_manual(values = colors_w) + 
  scale_fill_manual(values = colors_w) + 
  scale_shape_manual(values = c(21, 25)) + 
  #scale_linetype_manual(values = c("longdash", "solid")) + 
  labs(title = "", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(k)) + 
  ylim(0, 0.06) +
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
#plot_decomp_k_all_sub

# ggsave('plot_msc_dec_all_sub.png',
#        plot_decomp_k_all_sub,
#        bg='transparent')

## PLOT STABILIZATIONS FACTOR S ALPINE  -------------------------
plot_decomp_s_alp <- decomp.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  #filter(!grazing == "Natural") %>%
  filter(!origSiteID == "Sub-alpine") %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = S, 
                       color = warming,
                       fill = warming,
                       #linetype = warming,
                       shape = warming)
  ) +
  geom_point(size = 2) + 
  theme_minimal(base_size = 16) + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.box = "horizontal") +
  scale_color_manual(values = colors_w) + 
  scale_fill_manual(values = colors_w) + 
  scale_shape_manual(values = c(21, 25)) + 
  #scale_linetype_manual(values = c("longdash", "solid")) + 
  labs(title = "", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Stabilisation~factor~(S))) + 
  ylim(0, 0.7) +
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
#plot_decomp_s_alp

# ggsave('plot_msc_dec_s_alp.png',
#        plot_decomp_s_alp,
#        bg='transparent')

## PLOT STABILIZATIONS FACTOR S SUB-ALPINE  -------------------------
plot_decomp_s_sub <- decomp.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  #filter(!grazing == "Natural") %>%
  filter(!origSiteID == "Alpine") %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = S, 
                       color = warming,
                       fill = warming,
                       #linetype = warming,
                       shape = warming)
  ) +
  geom_point(size = 2) + 
  theme_minimal(base_size = 16) + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.box = "horizontal") +
  scale_color_manual(values = colors_w) + 
  scale_fill_manual(values = colors_w) + 
  scale_shape_manual(values = c(21, 25)) + 
  #scale_linetype_manual(values = c("longdash", "solid")) + 
  labs(title = "", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Stabilisation~factor~(S))) + 
  ylim(0, 0.7) +
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
#plot_decomp_s_sub

# ggsave('plot_msc_dec_s_sub.png',
#        plot_decomp_s_sub,
#        bg='transparent')
