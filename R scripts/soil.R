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
  select(-c(X14, X15, X16, X17, X18)) %>% 
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
  # reordering factors 
  mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) %>% 
  mutate(grazing = recode(grazing, 
                          "C" = "Control", "M" = "Medium",
                          "I" = "Intensive","N" = "Natural"),
         warming = recode(warming, 
                          "A" = "Ambient", "W" = "Warmed")) %>% 
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


## analysis -------------------------------------------------------

## model: soil burnmass_1_550 ~ warming x nitrogen x grazing 
soil.df %>%
  group_by(origSiteID) %>% # 
  nest() %>% # makes little dataframes inside my data, closed
  mutate( # makes column - do we want that?
    model.soil.wng = map(data, # runs model in each litte dataset
                          ~ lm(burnmass_1_550 ~ Namount_kg_ha_y * warming * grazing, 
                               data = .)),
    result.soil.wng = map(model.soil.wng, tidy)) %>%
  unnest(result.soil.wng) %>% # opens the nested dataframes 
  View()

## making dataframe with model output 
output.soil.wng <- soil.df %>%
  group_by(origSiteID) %>% # 
  nest() %>% # makes little dataframes inside my data, closed
  mutate( # makes column with the little dataframes 
    model.soil.wng = map(data, # runs model in each litte dataset
                          ~ lm(burnmass_1_550 ~ Namount_kg_ha_y * warming * grazing, 
                               data = .)),
    result.soil.wng = map(model.soil.wng, tidy)) %>%
  unnest(result.soil.wng) %>% # opens the nested dataframes 
  select(origSiteID, term, estimate, std.error, statistic, p.value)

View(output.soil.wng)

## figures--------------------------------------------------------
source("R scripts/MSc_aesthetics.R")

## plot: soil C ~ warming * nitrogen * grazing 
plot_soil_wng <- soil.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  summarise(prop_org_mat = mean(prop_org_mat)) %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = prop_org_mat, 
                       color = warming,
                       linetype = warming,
                       shape = warming)) +
  geom_point() + 
  theme_minimal(base_size = 20) + 
  scale_color_manual(values = colors_w) + 
  scale_linetype_manual(values = c("longdash", "solid")) + 
  scale_shape_manual(values = c(1, 16)) + 
  labs(title = "Proportion organic matter\nMass loss after burning at 550 *C", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Proportion~organic~material)) + 
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm") 
plot_soil_wng 
