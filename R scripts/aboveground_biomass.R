### SCRIPT ABOVEGROUND BIOMASS

source("R scripts/ThreeD_load_packages.R")
source("R scripts/ThreeD_create_metadata.R")
source("R scripts/MSc_aesthetics.R")

## importing data -----------------------------------
agb.raw.df <- read_csv("Data/THREE-D_clean_biomass_2020-2021.csv")


## checking for missing biomass (forbs/graminoids)
# agb.biomasscheck.df <- agb.raw.df %>%
#   mutate(biomass_m2 = ((biomass / area_cm2) * 10000)) %>% 
#   select(turfID, biomass, fun_group, origSiteID, destSiteID,
#          Nlevel, warming, grazing, destBlockID, biomass_m2) %>%
#   mutate_if(is.character, as.factor) %>%
#   filter(!fun_group == "legumes") %>%
#   filter(!fun_group == "litter") %>%
#   filter(!fun_group == "bryophytes") %>%
#   filter(!fun_group == "lichen") %>%
#   filter(!fun_group == "shrub") %>%
#   filter(!fun_group == "fungi") %>%
#   filter(!fun_group == "cyperaceae") %>%
#   filter(!Nlevel == 1,
#          !Nlevel == 2,
#          !Nlevel == 3,
#          !Nlevel == 4,
#          !Nlevel == 5,
#          !Nlevel == 6
#          ) %>%
#   filter(grazing == "M")



## fixup  ------------------------------------------
agb.df <- agb.raw.df %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(origBlockID = as.factor(origBlockID),
         origPlotID = as.factor(origPlotID),
         destBlockID = as.factor(destBlockID),
         destPlotID = as.factor(destPlotID)) %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% 
  # calculating biomass per m2 to standardize amounts
  mutate(biomass_m2 = ((biomass / area_cm2) * 10000)) %>% 
  filter(year == 2021) %>%
  filter(turfID != "147 WN9C 194") %>%# removing plots with missing forbs biomass
  # renaming variables 
  mutate(grazing = recode(grazing, 
                          "C" = "Control", "I" = "Intensive", 
                          "M" = "Medium", "N" = "Natural"),
         warming = recode(warming, 
                          "A" = "Ambient", "W" = "Warming")) %>% 
  # reordering variables 
  mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa")),
         grazing = factor(grazing, 
                          levels = c("C" = "Control", 
                                     "M" = "Medium",
                                     "I" = "Intensive",
                                     "N" = "Natural"))) %>% 
  # changing grazing into cont. variable too reduce degrees of freedom in models
  mutate(grazing_lvl = case_when((grazing == "Control") ~ 0,
                                 (grazing == "Medium") ~ 2,
                                 (grazing == "Intensive") ~ 4)) %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural")


## Models will be fitted for each dataset 
## Making dataset for alpine site
agb.alp.df <- agb.df %>% 
  filter(origSiteID == "Lia") 

## Making dataset for sub-alpine site 
agb.sub.df <- agb.df %>% 
  filter(origSiteID == "Joa")   
  

## plot aboveground biomass ~ w, n, g
plot_agb <- agb.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing, Nlevel, turfID) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  filter(!grazing == "Natural") %>%
  summarise(biomass_m2 = sum(biomass_m2)) %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = biomass_m2, 
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
  labs(title = "Aboveground productivity", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Biomass~(g~m^-2))) + 
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
plot_agb

ggsave('plot_msc_agb.png', 
       plot_agb, 
       bg='transparent')
