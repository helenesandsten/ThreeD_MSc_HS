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
  #select(-c(X14, X15, X16, X17, X18)) %>%
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
                          "A" = "Ambient", "W" = "Warmed")) %>% 
  # removing grazing level N too reduce degrees of freedom (Richard)
  filter(!grazing == "Natural") %>% 
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


## analysis -------------------------------------------------------

### MODEL: soil ~ w * n * g ---------------------------------------
model.soil.wng.int <- soil.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.soil.wng.int = map(data, # runs model in each litte dataset
                             ~ lm(prop_org_mat ~
                                    warming * Namount_kg_ha_y * grazing_lvl,
                                  data = .x)))#,
#result.soil.wng.int = map(model.soil.wng.int, tidy)) #%>%
#unnest(result.soil.wng.int) #%>% # opens the nested dataframes
# View()

### MODEL: soil ~ w * n * g - check model --------------------
## must run model code without result- and unnest-line for this to be useful
## check performance 
#model_performance(model.soil.wng.int $ model.soil.wng.int[[1]])
## check assumptions 
#check_model(model.soil.wng.int $ model.soil.wng.int[[1]])
# collinearity


###############################################################
### MODEL: soil ~ w + n + g ---------------------------------------
model.soil.wng.add <- soil.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.soil.wng.add = map(data, # runs model in each litte dataset
                             ~ lm(prop_org_mat ~
                                    warming + Namount_kg_ha_y + grazing_lvl,
                                  data = .x)))#,
#result.soil.wng.add = map(model.soil.wng.add, tidy)) #%>%
#unnest(result.soil.wng.add) #%>% # opens the nested dataframes
# View()

### MODEL: soil ~ w + n + g - check model --------------------
## must run model code without result- and unnest-line for this to be useful
## check performance 
#model_performance(model.soil.wng.add$model.soil.wng.add[[1]]) 
## check assumtions 
#check_model(model.soil.wng.add$model.soil.wng.add[[1]]) 
# all diagnostic plots looks good



###############################################################
### MODEL: soil ~ w * n + g ----------------------------------
model.soil.wng.intadd <- soil.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.soil.wng.intadd = map(data, # runs model in each litte dataset
                                ~ lm(prop_org_mat ~
                                       warming * Namount_kg_ha_y + grazing_lvl,
                                     data = .x)))#,
#result.soil.wng.intadd = map(model.soil.wng.intadd, tidy)) #%>%
#unnest(result.soil.wng.intadd) #%>% # opens the nested dataframes
# View()

### MODEL: soil ~ w * n + g - check model --------------------
## check performance 
#model_performance(model.soil.wng.intadd $ model.soil.wng.intadd[[1]])
## check assumptions 
#check_model(model.soil.wng.intadd $ model.soil.wng.intadd[[1]])
## all assumptions looks good 



###############################################################
### MODEL: soil ~ w + n * g ----------------------------------
model.soil.wng.addint <- soil.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.soil.wng.addint = map(data, # runs model in each litte dataset
                                ~ lm(prop_org_mat ~
                                       warming + Namount_kg_ha_y * grazing_lvl,
                                     data = .x)))#,
#result.soil.wng.addint = map(model.soil.wng.addint, tidy)) #%>%
#unnest(result.soil.wng.addint) #%>% # opens the nested dataframes
# View()

### MODEL: soil ~ w + n * g - check model --------------------
## check performance 
#model_performance(model.soil.wng.addint $ model.soil.wng.addint[[1]])
## check assumptions 
#check_model(model.soil.wng.addint $ model.soil.wng.addint[[1]])
# some collinearity


###############################################################
### MODEL: soil ~ w * g + n ----------------------------------
model.soil.wgn.intadd <- soil.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.soil.wgn.intadd = map(data, # runs model in each litte dataset
                                ~ lm(prop_org_mat ~
                                       warming * grazing_lvl + Namount_kg_ha_y,
                                     data = .x)))#,
#result.soil.wgn.intadd = map(model.soil.wgn.intadd, tidy)) #%>%
#unnest(result.soil.wgn.intadd) #%>% # opens the nested dataframes
# View()

### MODEL: soil ~ w * g + n - check model --------------------
## check performance 
#model_performance(model.soil.wgn.intadd $ model.soil.wgn.intadd[[1]])
## check assumptions 
#check_model(model.soil.wgn.intadd $ model.soil.wgn.intadd[[1]])
# some collinearity


###############################################################
### compare models -------------------------------------------
model_comparison_soil <- compare_performance(
  model.soil.wng.int$model.soil.wng.int[[1]],        # w * n * g 
  model.soil.wng.add$model.soil.wng.add[[1]],        # w + n + g 
  model.soil.wng.intadd$model.soil.wng.intadd[[1]],  # w * n + g
  model.soil.wng.addint$model.soil.wng.addint[[1]],  # w + n * g
  model.soil.wgn.intadd$model.soil.wgn.intadd[[1]]  # w + n * g 
)         

# plot.models.soil.ng <-
#   plot(compare_performance(
#     model.soil.wng.int$model.soil.wng.int[[1]],        # w * n * g
#     model.soil.wng.add$model.soil.wng.add[[1]],        # w + n + g 
#     model.soil.wng.intadd$model.soil.wng.intadd[[1]],  # w * n + g
#     model.soil.wng.addint$model.soil.wng.addint[[1]],  # w + n * g
#     model.soil.wgn.intadd$model.soil.wgn.intadd[[1]]  # w + n * g 
#   ))

## making output of best model -----------------------------------
## soil ~ w * n * g 
options(scipen = 100, digits = 5)
## running model with result and unnest to create output 
output.model.soil <- soil.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.soil.wng.add = map(data, # runs model in each litte dataset
                             ~ lm(prop_org_mat ~
                                    warming + Namount_kg_ha_y + grazing_lvl,
                                  data = .x)),
    result.soil.wng.add = map(model.soil.wng.add, tidy)) %>%
  unnest(result.soil.wng.add) %>% # opens the nested dataframes
  select(origSiteID, term, estimate, std.error, statistic, p.value)
# View()

## making clean and readable output for table ---------------------
clean_output.model.soil <- output.model.soil %>%
  mutate(term = case_when(
    (term == "(Intercept)") ~ "Intercept (no N, not warmed, not grazed)",
    (term == "warmingWarmed") ~ "Warmed",
    (term == "Namount_kg_ha_y") ~ "Nitrogen",
    (term == "grazing_lvl") ~ "Grazing",
    (term == "warmingWarmed:Namount_kg_ha_y") ~ "Warmed : Nitrogen",
    (term == "warmingWarmed:grazing_lvl") ~ "Warmed : Grazing",
    (term == "Namount_kg_ha_y:grazing_lvl") ~ "Nitrogen : Grazing",
    (term == "warmingWarmed:Namount_kg_ha_y:grazing_lvl") ~
      "Warmed : Nitrogen : Grazing"
  ))


## figures--------------------------------------------------------

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


## soil org matter plot wng
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
