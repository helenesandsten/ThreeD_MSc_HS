### TEST SCRIPT 

source("R scripts/ThreeD_load_packages.R")
source("R scripts/ThreeD_create_metadata.R")
source("R scripts/MSc_aesthetics.R")

## importing  ........-------------------------------------------
soil.raw.df <- read_csv2(file = "Data/ThreeD_soilcores_2021.csv")
#add.inf.df <- read_excel("Data/additional_info.xlsx", 
 #                        sheet = "coffeefilter_binder_mass")

## fixup -------------------------------------------------------
# mean mass of coffee filter + binder
# mean_cf_mass <- 1.9196

soil.df <- soil.raw.df %>% 
  # removing unwanted columns and fixing weird ones 
  select(-c(...14, ...15, ...16, ...17, ...18)) %>%
  mutate(alutray_ID = str_replace(alutray_ID, "  ", " ")) %>% 
  mutate(burn_mass1 = if_else(
    alutray_ID == "Vik W B5 I" & burn_mass1 == 11.5771, # row and col you want to change
    12.5771, # what you change it to 
    burn_mass1)) %>% # this needs to be here, ask Aud
  separate(col = alutray_ID, # separate column into several
           into = c("destSiteID", "warming", "destBlockID", "grazing"), " ") %>% 
  mutate(destBlockID = as.numeric(str_remove(destBlockID, "B")), # remove letter B
         destSiteID) %>% 
  select(-turfID) %>% # remove column 'turfID' because it is empty
  mutate_if(is.character, as.factor) %>% 
  mutate(#destPlotID = as.numeric(destPlotID),
         destBlockID = as.numeric(destBlockID),
         destSiteID = as.character(destSiteID)) %>% 
  # adding nitrogen levels to blocks 
  mutate(Nlevel = case_when(destBlockID == 1 ~ 1, # new_col = (case_when(old_c == old, ~new))
                            destBlockID == 2 ~ 6, 
                            destBlockID == 3 ~ 5, 
                            destBlockID == 4 ~ 3,
                            destBlockID == 5 ~ 10,
                            destBlockID == 6 ~ 7, 
                            destBlockID == 7 ~ 4, 
                            destBlockID == 8 ~ 8, 
                            destBlockID == 9 ~ 9, 
                            destBlockID == 10~ 2)) %>% 
  # adding nitrogen amount to Nlevels
  left_join(NitrogenDictionary, by = "Nlevel") %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% 
  # adding column origin site
  mutate(origSiteID = case_when((destSiteID == "Lia" & warming == "A") ~ "Lia",
                                (destSiteID == "Joa" & warming == "W") ~ "Lia", 
                                (destSiteID == "Joa" & warming == "A") ~ "Joa",
                                (destSiteID == "Vik" & warming == "W") ~ "Joa")) %>% 
  filter(!destSiteID == 'NA') %>% 
  # reordering factors to make plots less confusing 
  mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) %>% 
  mutate(grazing = recode(grazing, 
                          "C" = "Control", "M" = "Medium",
                          "I" = "Intensive","N" = "Natural"),
         warming = recode(warming, 
                          "A" = "Ambient", "W" = "Warming")) %>% 
  mutate(grazing = factor(grazing, levels = c("C" = "Control", 
                                              "M" = "Medium",
                                              "I" = "Intensive",
                                              "N" = "Natural"))) %>% 
  # changing grazing into numbered levels (Richard)
  mutate(grazing_lvl = case_when((grazing == "Control") ~ 0,
                                 (grazing == "Medium") ~ 2,
                                 (grazing == "Intensive") ~ 4)) %>% 
  # change column names to informative names
  rename(alutray_mass = alutray_weight, # new name = old name
         wetmass = wet_mass_g,
         drymass_1_55 = dry_mass1, 
         drymass_2_sieved = dry_mass2, 
         drymass_3_sieved_105 = dry_mass3, 
         drymass_4_87 = dry_mass4, 
         porcelain_mass = porcelain_weight,
         burnmass_1_550 = burn_mass1, 
         burnmass_2_950 = burn_mass2,
         root_stone_mass = total_cf_mass) %>%
  # removing container weight from mass weights 
  mutate(drymass_4_87 = drymass_4_87 - porcelain_mass,
         burnmass_1_550 = burnmass_1_550 - porcelain_mass,
         burnmass_2_950 = burnmass_2_950 - porcelain_mass) %>% 
  # removing impossible values i.e. typos from dataset 
  filter(!drymass_4_87 < burnmass_1_550,
         !burnmass_1_550 < burnmass_2_950) %>% 
  mutate(prop_sample_left = burnmass_1_550 / drymass_4_87) %>% 
  mutate(prop_org_mat = 1 - prop_sample_left) 


## Models will be fitted for each dataset 
## Making dataset for alpine site
soil.alp.df <- soil.df %>% 
  filter(!grazing == "Natural") %>% 
  filter(origSiteID == "Lia") 

## Making dataset for sub-alpine site 
soil.sub.df <- soil.df %>% 
  filter(!grazing == "Natural") %>% 
  filter(origSiteID == "Joa") 

## figures--------------------------------------------------------

## plot: soil C ~ warming * nitrogen * grazing 
plot_soil <- soil.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  # summarise(prop_org_mat = sum(prop_org_mat)) %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = prop_org_mat, 
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
       y = bquote(Proportion~soil~organic~material)) + 
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
plot_soil

# ggsave('plot_msc_soil.png',
#        plot_soil,
#        bg='transparent', height = 6, width = 8)

## TRASH AND MESSY STUFF --------------
# ## plot to see how the soil data is distributed between 1 and 0 
# plot_soil_density <- soil.df %>% 
#   group_by(Namount_kg_ha_y, Nlevel, origSiteID, warming, grazing) %>% 
#   mutate(Nlevel_fac = as.factor(Nlevel)) %>% 
#   ggplot(aes(x = prop_org_mat, 
#              y = drymass_4_87
#             # alpha = 0.5
#            # color = Nlevel_fac
#             )) +
#   geom_density() + 
#   theme_minimal() + 
#   #scale_color_manual(values = colors_Nlevel) + 
#   labs(title = "Soil samples", 
#        x = bquote(Proportion~organic~material), 
#        y = bquote(Dryweight~(pre~burn))
#        ) 
# plot_soil_density 
