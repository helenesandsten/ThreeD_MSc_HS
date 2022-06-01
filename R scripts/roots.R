### ANALYSIS AND MODELS ROOTS

source("R scripts/ThreeD_load_packages.R")
source("R scripts/ThreeD_create_metadata.R") 
source("R scripts/MSc_aesthetics.R")

## import - roots data -----------------------------------------------
roots.raw.df <- read_excel("Data/ThreeD_rootingrowthcores_2021.xlsx") 

## fixup - roots data ----------------------------------------------
roots.df <- roots.raw.df %>%
mutate(dateRIC_washed = ymd(dateRIC_washed),  
       date_roots_dried = ymd(date_roots_dried), # change format to date
       root_mass_g = total_mass_g - alutray_mass_g, 
       root_mass_cm3 = (root_mass_g/(pi*(1.6)^2*RIC_length_cm))) %>% # calculate volume
  mutate_if(is.character, as.factor) %>%
  # deciding order of origSiteID, 1. Lia 2. Joa
  mutate(grazing = recode(grazing, 
                          "C" = "Control", "M" = "Medium",
                          "I" = "Intensive","N" = "Natural"),
         warming = recode(warming, 
                          "A" = "Ambient", "W" = "Warming")) %>% 
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
  left_join(NitrogenDictionary, by = "Nlevel") %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% 
  mutate(days_buried = recover_date_2021 - burial_date) 


## Models will be fitted for each dataset 
## Making dataset for alpine site
roots.alp.df <- roots.df %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural") %>% 
  filter(origSiteID == "Lia") 

## Making dataset for sub-alpine site 
roots.sub.df <- roots.df %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural") %>% 
  filter(origSiteID == "Joa")   


## figures - roots --------------------------------------------------

## plot - roots ~ w n g
#labels_site_type <- c("alpine", "sub-alpine")

plot_bgb <- roots.df %>%
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>%
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  #filter(!grazing == "Natural") %>%
  summarise(root_mass_cm3 = mean(root_mass_cm3)) %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1),
                       y = root_mass_cm3,
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
       y = bquote(Root~mass~(g/cm^3))) +
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
plot_bgb

# ggsave('plot_msc_bgb.png', 
#        plot_bgb, 
#        bg='transparent')


