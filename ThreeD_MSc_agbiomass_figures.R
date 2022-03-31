##### ABOVEGROUND BIOMASS FIGURES

source("ThreeD_MSc_abovegroundbiomass_import.R") 

plot_agb_w_scatter <- agb.df %>% 
  #filter(year == 2021) %>% 
  filter(grazing == "C") %>%
  filter(Nlevel == 1 & 2 & 3) %>% 
  group_by(turfID) %>% 
  summarise(total_biomass = sum(biomass)) %>% 
  ggplot(data = agb.df, mapping = aes(x = Namount_kg_ha_y, y = biomass, color = warming)) +
  geom_point(alpha = 0.5, size = 3) +
  #geom_jitter() +
  scale_color_manual(values = c("#fecc5c", "#fd8d3c")) +
  theme_bw(base_size = 20) +
  labs(title = "Effect of warming on aboveground production", 
       x = "N (kg/ha/y)", y = "Aboveground biomass (g)") 
plot_agb_w_scatter
  

plot_agb_w_violin <- agb.df %>% 
  #filter(year == 2021) %>% 
  filter(grazing == "C") %>%
  filter(Nlevel == 1 & 2 & 3) %>% 
  group_by(turfID) %>% 
  summarise(total_biomass = sum(biomass)) %>% 
  ggplot(data = agb.df, mapping = aes(x = Namount_kg_ha_y, y = biomass, color = warming)) +
  geom_violin() +
  #geom_jitter() +
  scale_color_manual(values = c("#fecc5c", "#fd8d3c")) +
  theme_bw(base_size = 20) +
  labs(title = "Effect of warming on aboveground production", 
       x = "N (kg/ha/y)", y = "Aboveground biomass (g)") 
plot_agb_w_violin

agb.df %>%  
  filter(warming == "W", biomass > 35) %>% # top W outlier
  select(turfID) 

  