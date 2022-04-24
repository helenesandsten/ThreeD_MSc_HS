## aboveground biomass

source("R scripts/ThreeD_load_packages.R")
source("R scripts/ThreeD_create_metadata.R")

## importing data -----------------------------------
agb.raw.df <- read_csv("Data/THREE-D_clean_biomass_2020-2021.csv")

## fixup  ------------------------------------------
agb.df <- agb.raw.df %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(origBlockID = as.factor(origBlockID),
         origPlotID = as.factor(origPlotID),
         destBlockID = as.factor(destBlockID),
         destPlotID = as.factor(destPlotID)) %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% 
  filter(year == 2021) %>% 
  mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) %>% 
  mutate(grazing = recode(grazing, 
                          "C" = "Control", "I" = "Intensive", 
                          "M" = "Medium", "N" = "Natural"),
         warming = recode(warming, 
                          "A" = "Ambient", "W" = "Warmed"))

## analysis agb ----------------------------------------
## writes numbers out instead of on exponential form 
options(scipen = 100, digits = 4)

### model: agb ~ warming x nitrogen x grazing 
agb.df %>%
  group_by(origSiteID) %>% 
  nest() %>% 
  mutate(
    model.agb.wng = map(data, 
                          ~ lm(biomass ~ Namount_kg_ha_y * warming * grazing, 
                               data = .)),
    result.agb.wng = map(model.agb.wng, tidy)) %>%
  unnest(result.agb.wng) %>% # opens the nested dataframes 
  View() 

## making dataframe with model output 
output.agb.wng <- agb.df %>%
  group_by(origSiteID) %>% 
  nest() %>% 
  mutate(
    model.agb.wng = map(data, 
                        ~ lm(biomass ~ Namount_kg_ha_y * warming * grazing, 
                             data = .)),
    result.agb.wng = map(model.agb.wng, tidy)) %>%
  unnest(result.agb.wng) %>% 
  select(origSiteID, term, estimate, std.error, statistic, p.value)

View(output.agb.wng)



## figures -----------------------------------------
plot_agb_w_scatter <- agb.df %>% 
  #filter(year == 2021) %>% 
  filter(grazing == "C") %>%
  filter(Nlevel == 1 & 2 & 3) %>% 
  group_by(turfID) %>% 
  summarise(total_biomass = sum(biomass)) %>% 
  ggplot(data = agb.df, 
         mapping = aes(x = Namount_kg_ha_y, y = biomass, color = warming)) +
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

## plot aboveground biomass ~ w, n, g
plot_agb_wng <- agb.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  summarise(biomass = mean(biomass)) %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = biomass, 
                       color = warming,
                       linetype = warming,
                       shape = warming)) +
  geom_point() + 
  theme_minimal(base_size = 20) + 
  scale_color_manual(values = colors_w) + 
  scale_linetype_manual(values = c("longdash", "solid")) + 
  scale_shape_manual(values = c(1, 16)) + 
  labs(title = "Effect of warming, nitrogen and grazing on aboveground biomass", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Biomass~(g))) + 
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm")
plot_agb_wng


