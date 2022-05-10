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

### analysis - roots data ---------------------------------------
###############################################################
### ------ MODELS FOR ALPINE / LIAHOVDEN ------------------------
### MODEL: roots ~ w * n * g ------------------------------------
model.roots.wng.int.lia <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% #
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.int.lia = map(data, # runs model in each litte dataset
                                  ~ lm(root_mass_cm3 ~
                                         warming * Namount_kg_ha_y * grazing_lvl,
                                       data = .x)))#,
#result.roots.wng.int.lia = map(model.roots.wng.int.lia, tidy)) #%>%
#unnest(result.roots.wng.int.lia) #%>% # opens the nested dataframes 
# View() 

### MODEL: roots ~ w * n * g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumptions 
#check_model(model.roots.wng.int.lia $ model.roots.wng.int.lia[[1]])

## check plots that are off: collinearity 
## exspected as the model has interaction terms
#check_collinearity(model.roots.wng.int.lia $ model.roots.wng.int.lia[[1]])
#plot(check_collinearity(model.roots.wng.int.lia $ model.roots.wng.int.lia[[1]]))


###############################################################
### MODEL: roots ~ w + n + g ---------------------------------------
model.roots.wng.add.lia <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.add.lia = map(data, # runs model in each litte dataset
                                  ~ lm(root_mass_cm3 ~
                                         warming + Namount_kg_ha_y + grazing_lvl,
                                       data = .x)))#,
#result.roots.wng.add = map(model.roots.wng.add.lia, tidy)) #%>%
#unnest(result.roots.wng.add.lia) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w + n + g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumtions 
#check_model(model.roots.wng.add.lia$model.roots.wng.add.lia[[1]]) 
# all diagnostic plots looks good



###############################################################
### MODEL: roots ~ w * n + g ----------------------------------
model.roots.wng.intadd.lia <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.intadd.lia = map(data, # runs model in each litte dataset
                                     ~ lm(root_mass_cm3 ~
                                            warming * Namount_kg_ha_y + grazing_lvl,
                                          data = .x)))#,
#result.roots.wng.intadd.lia = map(model.roots.wng.intadd.lia, tidy)) #%>%
#unnest(result.roots.wng.intadd.lia) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w * n + g - check model --------------------

## check assumptions 
#check_model(model.roots.wng.intadd.lia $ model.roots.wng.intadd.lia[[1]])
## all diagnostic plots looks good 



###############################################################
### MODEL: roots ~ w + n * g ----------------------------------
model.roots.wng.addint.lia <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.addint.lia = map(data, # runs model in each litte dataset
                                     ~ lm(root_mass_cm3 ~
                                            warming + Namount_kg_ha_y * grazing_lvl,
                                          data = .x)))#,
#result.roots.wng.intadd.lia = map(model.roots.wng.addint.lia, tidy)) #%>%
#unnest(result.roots.wng.intadd.lia) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w + n * g - check model --------------------

## check assumptions 
#check_model(model.roots.wng.addint.lia $ model.roots.wng.addint.lia[[1]])
## all diagnostic plots looks good 


###############################################################
### MODEL: roots ~ w * g + n ----------------------------------
model.roots.wgn.intadd.lia <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wgn.intadd.lia = map(data, # runs model in each litte dataset
                                     ~ lm(root_mass_cm3 ~
                                            warming * grazing_lvl + Namount_kg_ha_y,
                                          data = .x)))#,
#result.roots.wgn.intadd.lia = map(model.roots.wgn.intadd.lia, tidy)) #%>%
#unnest(result.roots.wgn.intadd.lia) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w * g + n - check model --------------------

## check assumptions 
#check_model(model.roots.wgn.intadd.lia $ model.roots.wgn.intadd.lia[[1]])
## all diagnostic plots looks good 

###############################################################
### compare models -------------------------------------------
### warming & nitrogen & grazing 
model_comparison_roots.lia <- compare_performance(
  model.roots.wng.add.lia$model.roots.wng.add.lia[[1]],         # w + n + g 
  model.roots.wng.addint.lia$model.roots.wng.addint.lia[[1]],   # w + n * g
  model.roots.wng.intadd.lia$model.roots.wng.intadd.lia[[1]],   # w * n + g
  model.roots.wgn.intadd.lia$model.roots.wgn.intadd.lia[[1]],   # w * g + n
  model.roots.wng.int.lia$model.roots.wng.int.lia[[1]]          # w * n * g
)


# plot.models.roots.ng <-
#   plot(model_comparison_roots.lia)



## making output of best model ALPINE / LIAHOVDEN ---------------------------
## roots ~ w * n + g 
options(scipen = 100, digits = 4)
## running model with result and unnest to create output 
output.model.roots.lia <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.intadd.lia = map(data, # runs model in each litte dataset
                                     ~ lm(root_mass_cm3 ~
                                            warming * Namount_kg_ha_y + grazing_lvl,
                                          data = .x)),
    result.roots.wng.intadd.lia = map(model.roots.wng.intadd.lia, tidy)) %>%
  unnest(result.roots.wng.intadd.lia) %>% # opens the nested dataframes
  select(origSiteID, term, estimate, std.error, statistic, p.value)

## making clean and readable output for table ---------------------
clean_output.model.roots.lia <- output.model.roots.lia %>%
  mutate(term = case_when(
    (term == "(Intercept)") ~ "Intercept (no N, no warming, no grazing)",
    (term == "Namount_kg_ha_y") ~ "Nitrogen",
    (term == "warmingWarming") ~ "Warming",
    (term == "grazing_lvl") ~ "Grazing",
    (term == "warmingWarming:Namount_kg_ha_y") ~ "Warming : Nitrogen",
    (term == "warmingWarming:grazing_lvl") ~ "Warming : Grazing",
    (term == "Namount_kg_ha_y:grazing_lvl") ~ "Nitrogen : Grazing",
    (term == "warmingWarming:Namount_kg_ha_y:grazing_lvl") ~
      "Warming : Nitrogen : Grazing"
  ))



### ------ MODELS FOR SUB ALPINE / JOASETE ----------------------
### MODEL: roots ~ w * n * g ------------------------------------
model.roots.wng.int.joa <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% #
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.int.joa = map(data, # runs model in each litte dataset
                                  ~ lm(root_mass_cm3 ~
                                         warming * Namount_kg_ha_y * grazing_lvl,
                                       data = .x)))#,
#result.roots.wng.int.joa = map(model.roots.wng.int.joa, tidy)) #%>%
#unnest(result.roots.wng.int.joa) #%>% # opens the nested dataframes 
# View() 

### MODEL: roots ~ w * n * g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumptions 
#check_model(model.roots.wng.int.joa $ model.roots.wng.int.joa[[1]])

## check plots that are off: collinearity 
## exspected as the model has interaction terms
#check_collinearity(model.roots.wng.int.joa $ model.roots.wng.int.joa[[1]])
#plot(check_collinearity(model.roots.wng.int.joa $ model.roots.wng.int.joa[[1]]))


###############################################################
### MODEL: roots ~ w + n + g ---------------------------------------
model.roots.wng.add.joa <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.add.joa = map(data, # runs model in each litte dataset
                                  ~ lm(root_mass_cm3 ~
                                         warming + Namount_kg_ha_y + grazing_lvl,
                                       data = .x)))#,
#result.roots.wng.add = map(model.roots.wng.add.joa, tidy)) #%>%
#unnest(result.roots.wng.add.joa) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w + n + g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumtions 
#check_model(model.roots.wng.add.joa$model.roots.wng.add.joa[[1]]) 
# all diagnostic plots looks good 



###############################################################
### MODEL: roots ~ w * n + g ----------------------------------
model.roots.wng.intadd.joa <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.intadd.joa = map(data, # runs model in each litte dataset
                                     ~ lm(root_mass_cm3 ~
                                            warming * Namount_kg_ha_y + grazing_lvl,
                                          data = .x)))#,
#result.roots.wng.intadd.joa = map(model.roots.wng.intadd.joa, tidy)) #%>%
#unnest(result.roots.wng.intadd.joa) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w * n + g - check model --------------------

## check assumptions 
#check_model(model.roots.wng.intadd.joa $ model.roots.wng.intadd.joa[[1]])
## all diagnostic plots looks good 



###############################################################
### MODEL: roots ~ w + n * g ----------------------------------
model.roots.wng.addint.joa <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.addint.joa = map(data, # runs model in each litte dataset
                                     ~ lm(root_mass_cm3 ~
                                            warming + Namount_kg_ha_y * grazing_lvl,
                                          data = .x)))#,
#result.roots.wng.intadd.joa = map(model.roots.wng.addint.joa, tidy)) #%>%
#unnest(result.roots.wng.intadd.joa) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w + n * g - check model --------------------

## check assumptions 
#check_model(model.roots.wng.addint.joa $ model.roots.wng.addint.joa[[1]])
## all diagnostic plots looks good 


###############################################################
### MODEL: roots ~ w * g + n ----------------------------------
model.roots.wgn.intadd.joa <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wgn.intadd.joa = map(data, # runs model in each litte dataset
                                     ~ lm(root_mass_cm3 ~
                                            warming * grazing_lvl + Namount_kg_ha_y,
                                          data = .x)))#,
#result.roots.wgn.intadd.joa = map(model.roots.wgn.intadd.joa, tidy)) #%>%
#unnest(result.roots.wgn.intadd.joa) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w * g + n - check model --------------------

## check assumptions 
#check_model(model.roots.wgn.intadd.joa $ model.roots.wgn.intadd.joa[[1]])
## all diagnostic plots looks good 

###############################################################
### compare models -------------------------------------------
### warming & nitrogen & grazing 
model_comparison_roots.joa <- compare_performance(
  model.roots.wng.add.joa$model.roots.wng.add.joa[[1]],         # w + n + g 
  model.roots.wng.addint.joa$model.roots.wng.addint.joa[[1]],   # w + n * g
  model.roots.wng.intadd.joa$model.roots.wng.intadd.joa[[1]],   # w * n + g
  model.roots.wgn.intadd.joa$model.roots.wgn.intadd.joa[[1]],   # w * g + n
  model.roots.wng.int.joa$model.roots.wng.int.joa[[1]]          # w * n * g
)


# plot.models.roots.ng <-
#   plot(model_comparison_roots.joa)


## making output of best model SUB ALPINE / JOASETE ---------------------------
## roots ~ w + n + g 
options(scipen = 100, digits = 4)
## running model with result and unnest to create output 
output.model.roots.joa <- roots.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.intadd.joa = map(data, # runs model in each litte dataset
                                     ~ lm(root_mass_cm3 ~
                                            warming + Namount_kg_ha_y + grazing_lvl,
                                          data = .x)),
    result.roots.wng.intadd.joa = map(model.roots.wng.intadd.joa, tidy)) %>%
  unnest(result.roots.wng.intadd.joa) %>% # opens the nested dataframes
  select(origSiteID, term, estimate, std.error, statistic, p.value)

## making clean and readable output for table ---------------------
clean_output.model.roots.joa <- output.model.roots.joa %>%
  mutate(term = case_when(
    (term == "(Intercept)") ~ "Intercept (no N, no warming, no grazing)",
    (term == "Namount_kg_ha_y") ~ "Nitrogen",
    (term == "warmingWarming") ~ "Warming",
    (term == "grazing_lvl") ~ "Grazing",
    (term == "warmingWarming:Namount_kg_ha_y") ~ "Warming : Nitrogen",
    (term == "warmingWarming:grazing_lvl") ~ "Warming : Grazing",
    (term == "Namount_kg_ha_y:grazing_lvl") ~ "Nitrogen : Grazing",
    (term == "warmingWarming:Namount_kg_ha_y:grazing_lvl") ~
      "Warming : Nitrogen : Grazing"
  ))




## figures - roots --------------------------------------------------

## plot - roots ~ w n g
#labels_site_type <- c("alpine", "sub-alpine")

plot_roots_wng <- roots.df %>%
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>%
  summarise(root_mass_cm3 = mean(root_mass_cm3)) %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1),
                       y = root_mass_cm3,
                       color = warming,
                       linetype = warming,
                       shape = warming)) +
  geom_point() +
  theme_minimal(base_size = 20) +
  scale_color_manual(values = colors_w) +
  scale_linetype_manual(values = c("longdash", "solid")) +
  scale_shape_manual(values = c(1, 16)) +
  labs(title = "Belowground productivity",
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
       y = bquote(Root~mass~(g/cm^3))) +
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm")
plot_roots_wng


# # plot roots ~ grazing
# labels_g <- c("Control", "Intensive", "Medium", "Natural")
# 
# plot_roots_g <- roots.df %>%
#   filter(warming == "Ambient") %>%
#   filter(Nlevel == 1 & 2 & 3) %>%
#   ggplot(mapping = aes(x = grazing, y = root_mass_g, fill = grazing)) +
#   geom_boxplot() +
#   theme_minimal(base_size = 20) +
#   labs(title = "Effect of grazing on root growth",
#        x = "Grazing", y = "Root mass (g)") +
#   scale_x_discrete(labels = labels_g) +
#   theme(legend.position = "none") +
#   scale_fill_manual(values = colors_g)
# plot_roots_g




