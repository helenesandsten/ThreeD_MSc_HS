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
  mutate(prop_org_mat = 1 - prop_sample_left) %>% 
  # removing grazing level N because it is too different from other 
  # levels to be included in analysis
  filter(!grazing == "Natural")


## Models will be fitted for each dataset 
## Making dataset for alpine site
soil.alp.df <- soil.df %>% 
  filter(origSiteID == "Lia") 

## Making dataset for sub-alpine site 
soil.sub.df <- soil.df %>% 
  filter(origSiteID == "Joa") 



## analysis -------------------------------------------------------

# ### ------ MODELS FOR ALPINE / LIAHOVDEN ------------------------
# ### MODEL: soil ~ w * n * g -------------------------------------
# model.soil.wng.int.lia <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Lia") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wng.int.lia = map(data, # runs model in each litte dataset
#                                  ~ lm(prop_org_mat ~
#                                         warming * Namount_kg_ha_y * grazing_lvl,
#                                       data = .x)))#,
# #result.soil.wng.int.lia = map(model.soil.wng.int.lia, tidy)) #%>%
# #unnest(result.soil.wng.int.lia) #%>% # opens the nested dataframes
# # View()
# 
# ### MODEL: soil ~ w * n * g - check model --------------------
# ## must run model code without result- and unnest-line for this to be useful 
# 
# ## check assumptions 
# #check_model(model.soil.wng.int.lia $ model.soil.wng.int.lia[[1]])
# # moderate collinearity
# 
# 
# ###############################################################
# ### MODEL: soil ~ w + n + g ---------------------------------------
# model.soil.wng.add.lia <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Lia") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wng.add.lia = map(data, # runs model in each litte dataset
#                                  ~ lm(prop_org_mat ~
#                                         warming + Namount_kg_ha_y + grazing_lvl,
#                                       data = .x)))#,
# #result.soil.wng.add.lia = map(model.soil.wng.add.lia, tidy)) #%>%
# #unnest(result.soil.wng.add.lia) #%>% # opens the nested dataframes
# # View()
# 
# ### MODEL: soil ~ w + n + g - check model --------------------
# ## must run model code without result- and unnest-line for this to be useful
# 
# ## check assumtions 
# #check_model(model.soil.wng.add.lia$model.soil.wng.add.lia[[1]]) 
# # all diagnostic plots looks good
# 
# 
# 
# ###############################################################
# ### MODEL: soil ~ w * n + g ----------------------------------
# model.soil.wng.intadd.lia <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Lia") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wng.intadd.lia = map(data, # runs model in each litte dataset
#                                     ~ lm(prop_org_mat ~
#                                            warming * Namount_kg_ha_y + grazing_lvl,
#                                          data = .x)))#,
# #result.soil.wng.intadd.lia = map(model.soil.wng.intadd.lia, tidy)) #%>%
# #unnest(result.soil.wng.intadd.lia) #%>% # opens the nested dataframes
# # View()
# 
# ### MODEL: soil ~ w * n + g - check model --------------------
# 
# ## check assumptions 
# #check_model(model.soil.wng.intadd.lia $ model.soil.wng.intadd.lia[[1]])
# ## all assumptions looks good 
# 
# 
# 
# ###############################################################
# ### MODEL: soil ~ w + n * g ----------------------------------
# model.soil.wng.addint.lia <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Lia") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wng.addint.lia = map(data, # runs model in each litte dataset
#                                     ~ lm(prop_org_mat ~
#                                            warming + Namount_kg_ha_y * grazing_lvl,
#                                          data = .x)))#,
# #result.soil.wng.addint.lia = map(model.soil.wng.addint.lia, tidy)) #%>%
# #unnest(result.soil.wng.addint.lia) #%>% # opens the nested dataframes
# # View()
# 
# ### MODEL: soil ~ w + n * g - check model --------------------
# 
# ## check assumptions 
# #check_model(model.soil.wng.addint.lia $ model.soil.wng.addint.lia[[1]])
# # some collinearity
# 
# 
# ###############################################################
# ### MODEL: soil ~ w * g + n ----------------------------------
# model.soil.wgn.intadd.lia <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Lia") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wgn.intadd.lia = map(data, # runs model in each litte dataset
#                                     ~ lm(prop_org_mat ~
#                                            warming * grazing_lvl + Namount_kg_ha_y,
#                                          data = .x)))#,
# #result.soil.wgn.intadd.lia = map(model.soil.wgn.intadd.lia, tidy)) #%>%
# #unnest(result.soil.wgn.intadd.lia) #%>% # opens the nested dataframes
# # View()
# 
# ### MODEL: soil ~ w * g + n - check model --------------------
# 
# ## check assumptions 
# #check_model(model.soil.wgn.intadd.lia $ model.soil.wgn.intadd.lia[[1]])
# # some collinearity
# 
# 
# ###############################################################
# ### compare models lia -------------------------------------------
# model_comparison_soil.lia <- compare_performance(
#   model.soil.wng.add.lia$model.soil.wng.add.lia[[1]],         # w + n + g 
#   model.soil.wng.addint.lia$model.soil.wng.addint.lia[[1]],   # w + n * g
#   model.soil.wng.intadd.lia$model.soil.wng.intadd.lia[[1]],   # w * n + g
#   model.soil.wgn.intadd.lia$model.soil.wgn.intadd.lia[[1]],   # w * g + n
#   model.soil.wng.int.lia$model.soil.wng.int.lia[[1]]          # w * n * g
# )
# 
# 
# # plot.models.soil.ng <-
# #   plot(model_comparison_soil.lia)
# 
# ## making output of best model -----------------------------------
# ## soil ~ w * n * g 
# options(scipen = 100, digits = 4)
# ### MODEL: soil ~ w + n + g ---------------------------------------
# output.model.soil.lia <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Lia") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wng.add.lia = map(data, # runs model in each litte dataset
#                                  ~ lm(prop_org_mat ~
#                                         warming + Namount_kg_ha_y + grazing_lvl,
#                                       data = .x)),
#     result.soil.wng.add.lia = map(model.soil.wng.add.lia, tidy)) %>%
#   unnest(result.soil.wng.add.lia) %>% # opens the nested dataframes
#   select(origSiteID, term, estimate, std.error, statistic, p.value)
# 
# 
# ## making clean and readable output for table ---------------------
# clean_output.model.soil.lia <- output.model.soil.lia %>%
#   mutate(term = case_when(
#     (term == "(Intercept)") ~ "Intercept (no N, no warming, no grazing)",
#     (term == "warmingWarming") ~ "Warming",
#     (term == "Namount_kg_ha_y") ~ "Nitrogen",
#     (term == "grazing_lvl") ~ "Grazing",
#     (term == "warmingWarming:Namount_kg_ha_y") ~ "Warming : Nitrogen",
#     (term == "warmingWarming:grazing_lvl") ~ "Warming : Grazing",
#     (term == "Namount_kg_ha_y:grazing_lvl") ~ "Nitrogen : Grazing",
#     (term == "warmingWarming:Namount_kg_ha_y:grazing_lvl") ~
#       "Warming : Nitrogen : Grazing"
#   )) %>% 
#   mutate(origSiteID = case_when(
#     (origSiteID == "Lia") ~ "Alpine",
#     (origSiteID == "Joa") ~ "Sub-alpine"))
# 
# 
# ### ------ MODELS FOR SUB ALPINE / JOASETE ------------------------
# ### MODEL: soil ~ w * n * g -------------------------------------
# model.soil.wng.int.joa <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Joa") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wng.int.joa = map(data, # runs model in each litte dataset
#                                  ~ lm(prop_org_mat ~
#                                         warming * Namount_kg_ha_y * grazing_lvl,
#                                       data = .x)))#,
# #result.soil.wng.int.joa = map(model.soil.wng.int.joa, tidy)) #%>%
# #unnest(result.soil.wng.int.joa) #%>% # opens the nested dataframes
# # View()
# 
# ### MODEL: soil ~ w * n * g - check model --------------------
# ## must run model code without result- and unnest-line for this to be useful 
# 
# ## check assumptions 
# #check_model(model.soil.wng.int.joa $ model.soil.wng.int.joa[[1]])
# # moderate collinearity
# 
# 
# ###############################################################
# ### MODEL: soil ~ w + n + g ---------------------------------------
# model.soil.wng.add.joa <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Joa") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wng.add.joa = map(data, # runs model in each litte dataset
#                                  ~ lm(prop_org_mat ~
#                                         warming + Namount_kg_ha_y + grazing_lvl,
#                                       data = .x)))#,
# #result.soil.wng.add.joa = map(model.soil.wng.add.joa, tidy)) #%>%
# #unnest(result.soil.wng.add).joa #%>% # opens the nested dataframes
# # View()
# 
# ### MODEL: soil ~ w + n + g - check model --------------------
# ## must run model code without result- and unnest-line for this to be useful
# 
# ## check assumtions 
# #check_model(model.soil.wng.add.joa$model.soil.wng.add.joa[[1]]) 
# # all diagnostic plots looks good
# 
# 
# 
# ###############################################################
# ### MODEL: soil ~ w * n + g ----------------------------------
# model.soil.wng.intadd.joa <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Joa") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wng.intadd.joa = map(data, # runs model in each litte dataset
#                                     ~ lm(prop_org_mat ~
#                                            warming * Namount_kg_ha_y + grazing_lvl,
#                                          data = .x)))#,
# #result.soil.wng.intadd.joa = map(model.soil.wng.intadd.joa, tidy)) #%>%
# #unnest(result.soil.wng.intadd.joa) #%>% # opens the nested dataframes
# # View()
# 
# ### MODEL: soil ~ w * n + g - check model --------------------
# 
# ## check assumptions 
# #check_model(model.soil.wng.intadd.joa $ model.soil.wng.intadd.joa[[1]])
# ## all assumptions looks good 
# 
# 
# 
# ###############################################################
# ### MODEL: soil ~ w + n * g ----------------------------------
# model.soil.wng.addint.joa <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Joa") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wng.addint.joa = map(data, # runs model in each litte dataset
#                                     ~ lm(prop_org_mat ~
#                                            warming + Namount_kg_ha_y * grazing_lvl,
#                                          data = .x)))#,
# #result.soil.wng.addint.joa = map(model.soil.wng.addint.joa, tidy)) #%>%
# #unnest(result.soil.wng.addint.joa) #%>% # opens the nested dataframes
# # View()
# 
# ### MODEL: soil ~ w + n * g - check model --------------------
# 
# ## check assumptions 
# #check_model(model.soil.wng.addint.joa $ model.soil.wng.addint.joa[[1]])
# # some collinearity
# 
# 
# ###############################################################
# ### MODEL: soil ~ w * g + n ----------------------------------
# model.soil.wgn.intadd.joa <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Joa") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wgn.intadd.joa = map(data, # runs model in each litte dataset
#                                     ~ lm(prop_org_mat ~
#                                            warming * grazing_lvl + Namount_kg_ha_y,
#                                          data = .x)))#,
# #result.soil.wgn.intadd.joa = map(model.soil.wgn.intadd.joa, tidy)) #%>%
# #unnest(result.soil.wgn.intadd.joa) #%>% # opens the nested dataframes
# # View()
# 
# ### MODEL: soil ~ w * g + n - check model --------------------
# 
# ## check assumptions 
# #check_model(model.soil.wgn.intadd.joa $ model.soil.wgn.intadd.joa[[1]])
# # some collinearity
# 
# 
# ###############################################################
# ### compare models -------------------------------------------
# model_comparison_soil.joa <- compare_performance(
#   model.soil.wng.add.joa$model.soil.wng.add.joa[[1]],         # w + n + g 
#   model.soil.wng.addint.joa$model.soil.wng.addint.joa[[1]],   # w + n * g
#   model.soil.wng.intadd.joa$model.soil.wng.intadd.joa[[1]],   # w * n + g
#   model.soil.wgn.intadd.joa$model.soil.wgn.intadd.joa[[1]],   # w * g + n
#   model.soil.wng.int.joa$model.soil.wng.int.joa[[1]]          # w * n * g
# )
# 
# 
# # plot.models.soil.ng <-
# #   plot(model_comparison_soil.joa)
# 
# ## making output of best model -----------------------------------
# options(scipen = 100, digits = 4)
# ### MODEL: soil ~ w * g + n ---------------------------------------
# output.model.soil.joa <- soil.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>% 
#   group_by(origSiteID) %>% 
#   filter(origSiteID == "Joa") %>% 
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.soil.wgn.intadd.joa = map(data, # runs model in each litte dataset
#                                  ~ lm(prop_org_mat ~
#                                         warming * grazing_lvl + Namount_kg_ha_y,
#                                       data = .x)),
#     result.soil.wgn.intadd.joa = map(model.soil.wgn.intadd.joa, tidy)) %>%
#   unnest(result.soil.wgn.intadd.joa) %>% # opens the nested dataframes
#   select(origSiteID, term, estimate, std.error, statistic, p.value)
# 
# 
# ## making clean and readable output for table ---------------------
# clean_output.model.soil.joa <- output.model.soil.joa %>%
#   mutate(term = case_when(
#     (term == "(Intercept)") ~ "Intercept (no N, no warming, no grazing)",
#     (term == "warmingWarming") ~ "Warming",
#     (term == "Namount_kg_ha_y") ~ "Nitrogen",
#     (term == "grazing_lvl") ~ "Grazing",
#     (term == "warmingWarming:Namount_kg_ha_y") ~ "Warming : Nitrogen",
#     (term == "warmingWarming:grazing_lvl") ~ "Warming : Grazing",
#     (term == "Namount_kg_ha_y:grazing_lvl") ~ "Nitrogen : Grazing",
#     (term == "warmingWarming:Namount_kg_ha_y:grazing_lvl") ~
#       "Warming : Nitrogen : Grazing"
#   )) %>% 
#   mutate(origSiteID = case_when(
#     (origSiteID == "Lia") ~ "Alpine",
#     (origSiteID == "Joa") ~ "Sub-alpine"))




## figures--------------------------------------------------------

## plot: soil C ~ warming * nitrogen * grazing 
plot_soil_wng <- soil.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  filter(!grazing == "Natural") %>%
  summarise(prop_org_mat = sum(prop_org_mat)) %>% 
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
  labs(title = "Proportion of soil organic matter", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Proportion~organic~material)) + 
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
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
