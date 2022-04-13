## FIGURES 

source("R scripts/transformation_plan.R")
#source("MSc_aesthetics.R")


##### BELOWGROUND BIOMASS - ROOTS ----------------------------------- 

# # plot of rootmass and RIC depth
# roots_plot1 <- roots.df %>% 
#   ggplot(aes(x = RIC_length_cm, y = root_mass_g), color = warming) + 
#   geom_point(size = 3, alpha = 0.5) +
#   theme_bw(base_size = 20) +
#   labs(title = "Root mass and RIC length", 
#        x = "RIC length (cm)",
#        y = "Root mass (g)")
# roots_plot1 
# 
# 
# # plot root biomass - warming 
# plot_roots_w <- roots.df %>% 
#   filter(grazing == "C") %>% 
#   filter(Nlevel == 1 & 2 & 3) %>% 
#   ggplot(mapping = aes(x = warming, y = root_mass_cm3)) +
#   geom_point() +#fill = c("#fecc5c", "#fd8d3c")) +
#   theme_bw(base_size = 20) +
#   labs(title = "Effect of warming on root growth", 
#        x = "warming", 
#        y = "Root mass (g/cm3)") + 
#   facet_wrap(~origSiteID) 
# plot_roots_w # why only one point per site? should there not be three?
# 
# # plot root biomass - nitrogen (kg)
# labels_nlevel <- c("0", "0", "0", "0.5", "1", "5", "10", "50", "100", "150")
# 
# plot_roots_namount <- roots.df %>% 
#   filter(warming == "A") %>% 
#   filter(grazing == "C") %>% 
#   group_by(Namount_kg_ha_y, origSiteID) %>% 
#   summarise(root_mass_g = mean(root_mass_g)) %>% 
#   ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), y = root_mass_g, color = origSiteID)) +
#                        #group = Nlevel, fill = Nlevel)) +
#   geom_point() +
#   theme_bw(base_size = 20) +
#   labs(title = "Effect of Nitrogen on root growth", 
#        x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
#        y = "Root mass (g)") 
#   #scale_x_discrete(labels = labels_nlevel)
# plot_roots_namount
# 
# # plot root biomass - nitrogen (levels)
# # plot_roots_nlvl <- roots.df %>% 
# #   filter(warming == "A") %>% 
# #   filter(grazing == "C") %>% 
# #   ggplot(data = roots.df, 
# #          mapping = aes(x = Nlevel, y = root_mass_g,
# #                        group = Nlevel, fill = Nlevel)) +
# #   geom_boxplot(fill = "cornflowerblue") +
# #   theme_bw(base_size = 20) +
# #   labs(title = "Effect of Nitrogen on root growth", x = "N level", y = "Root mass (g)") 
# # plot_roots_nlvl
# 
# 
# 
# 
# plot root biomass - grazing
labels_grz <- c("control", "intensive", "medium", "natural")

plot_roots_grz <- roots.df %>%
  filter(warming == "A") %>%
  filter(Nlevel == 1 & 2 & 3) %>%
  ggplot(mapping = aes(x = grazing, y = root_mass_g, fill = grazing)) +
  geom_boxplot() +
  theme_bw(base_size = 20) +
  labs(title = "Effect of grazing on root growth",
       x = "Grazing", y = "Root mass (g)") +
  scale_x_discrete(labels = labels_grz) +
  theme(legend.position = "none") +
  scale_fill_manual(values = colors_g)
plot_roots_grz

## megaplot roots 
## plot for simple model 
plot_roots_mega <- roots.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  summarise(root_mass_cm3 = mean(root_mass_cm3)) %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = root_mass_cm3, color = warming)) +
  geom_point() +
  theme_bw(base_size = 20) +
  labs(title = "", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Root~mass~(g/cm^3))) +
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm")
plot_roots_mega

  
  
