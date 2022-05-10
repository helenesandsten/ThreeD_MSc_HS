### SCRIPT ABOVEGROUND BIOMASS

source("R scripts/ThreeD_load_packages.R")
source("R scripts/ThreeD_create_metadata.R")
source("R scripts/MSc_aesthetics.R")

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
                                 (grazing == "Intensive") ~ 4))

## analysis agb ----------------------------------------

### ------ MODELS FOR ALPINE / LIAHOVDEN ------------------------
### MODEL: agb ~ w * n * g ---------------------------------------
model.agb.wng.int.lia <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>% 
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.int.lia = map(data, # runs model in each litte dataset
                            ~ lm(biomass ~
                                   warming * Namount_kg_ha_y * grazing_lvl,
                                 data = .x)))#,
#result.agb.wng.int.lia = map(model.agb.wng.int.lia, tidy)) #%>%
#unnest(result.agb.wng.int.lia) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w * n * g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumptions 
#check_model(model.agb.wng.int.lia $ model.agb.wng.int.lia[[1]])

## check plots that are off: collinearity 
## exspected as the model has interaction terms
#check_collinearity(model.agb.wng.int.lia $ model.agb.wng.int.lia[[1]])
#plot(check_collinearity(model.agb.wng.int.lia $ model.agb.wng.int.lia[[1]])) 


###############################################################
### MODEL: agb ~ w + n + g ---------------------------------------
model.agb.wng.add.lia <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% #
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.add.lia = map(data, # runs model in each litte dataset
                            ~ lm(biomass ~
                                   warming + Namount_kg_ha_y + grazing_lvl,
                                 data = .x)))#,
#result.agb.wng.add.lia = map(model.agb.wng.add.lia, tidy)) #%>%
#unnest(result.agb.wng.add.lia) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w + n + g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumtions 
#check_model(model.agb.wng.add.lia$model.agb.wng.add.lia[[1]]) 
# all diagnostic plots looks good (green/blue) but 
# posterior predictive check is spiky and 
# normality of residuals is not normal at the right 



###############################################################
### MODEL: agb ~ w * n + g ----------------------------------
model.agb.wng.intadd.lia <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.intadd.lia = map(data, # runs model in each litte dataset
                               ~ lm(biomass ~
                                      warming * Namount_kg_ha_y + grazing_lvl,
                                    data = .x)))#,
#result.agb.wng.intadd.lia = map(model.agb.wng.intadd.lia, tidy)) #%>%
#unnest(result.agb.wng.intadd.lia) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w * n + g - check model --------------------

## check assumptions 
#check_model(model.agb.wng.intadd.lia $ model.agb.wng.intadd.lia[[1]])
## all assumptions looks good 



###############################################################
### MODEL: agb ~ w + n * g ----------------------------------
model.agb.wng.addint.lia <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.addint.lia = map(data, # runs model in each litte dataset
                               ~ lm(biomass ~
                                      warming + Namount_kg_ha_y * grazing_lvl,
                                    data = .x)))#,
#result.agb.wng.addint.lia = map(model.agb.wng.addint.lia, tidy)) #%>%
#unnest(result.agb.wng.addint.lia) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w + n * g - check model --------------------

## check assumptions 
#check_model(model.agb.wng.addint.lia $ model.agb.wng.addint.lia[[1]])



###############################################################
### MODEL: agb ~ w * g + n ----------------------------------
model.agb.wgn.intadd.lia <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wgn.intadd.lia = map(data, # runs model in each litte dataset
                               ~ lm(biomass ~
                                      warming * grazing_lvl + Namount_kg_ha_y,
                                    data = .x)))#,
#result.agb.wgn.intadd.lia = map(model.agb.wgn.intadd.lia, tidy)) #%>%
#unnest(result.agb.wgn.intadd.lia) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w * g + n - check model --------------------

## check assumptions 
#check_model(model.agb.wgn.intadd.lia $ model.agb.wgn.intadd.lia[[1]])


###############################################################
### compare models ALPINE / LIA ---------------------------------
model_comparison_agb.lia <- compare_performance(
  model.agb.wng.add.lia$model.agb.wng.add.lia[[1]],         # w + n + g 
  model.agb.wng.addint.lia$model.agb.wng.addint.lia[[1]],   # w + n * g
  model.agb.wng.intadd.lia$model.agb.wng.intadd.lia[[1]],   # w * n + g
  model.agb.wgn.intadd.lia$model.agb.wgn.intadd.lia[[1]],   # w * g + n
  model.agb.wng.int.lia$model.agb.wng.int.lia[[1]]          # w * n * g
)
  

# plot.models.agb.ng <-
#   plot(model_comparison_agb.lia)

## making output of best model -----------------------------------
## agb ~ w * n + g 
# options(scipen = 100, digits = 4)
# ## running model with result and unnest to create output 
# output.model.agb.lia <- agb.df %>%
#   # removing grazing level N to reduce degrees of freedom
#   filter(!grazing == "Natural") %>%
#   group_by(origSiteID) %>% #
#   nest() %>% # makes little dataframes inside my data, closed
#   mutate(
#     model.agb.wng.intadd = map(data, # runs model in each litte dataset
#                             ~ lm(biomass ~
#                                    warming * Namount_kg_ha_y + grazing_lvl,
#                                  data = .)),
#     result.agb.wng.intadd = map(model.agb.wng.intadd, tidy)) %>%
#   unnest(result.agb.wng.intadd) %>% # opens the nested dataframes
#   select(origSiteID, term, estimate, std.error, statistic, p.value)
# # View()
# 
# ## making clean and readable output for table ---------------------
# clean_output.model.agb.lia <- output.model.agb.lia %>%
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
#   ))

### ------ MODELS FOR SUB ALPINE / JOASETE -----------------------
### MODEL: agb ~ w * n * g ---------------------------------------
model.agb.wng.int.joa <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>% 
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.int.joa = map(data, # runs model in each litte dataset
                                ~ lm(biomass ~
                                       warming * Namount_kg_ha_y * grazing_lvl,
                                     data = .x)))#,
#result.agb.wng.int.joa = map(model.agb.wng.int.joa, tidy)) #%>%
#unnest(result.agb.wng.int.joa) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w * n * g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumptions 
#check_model(model.agb.wng.int.joa $ model.agb.wng.int.joa[[1]])

## check plots that are off: collinearity 
## exspected as the model has interaction terms
#check_collinearity(model.agb.wng.int.joa $ model.agb.wng.int.joa[[1]])
#plot(check_collinearity(model.agb.wng.int.joa $ model.agb.wng.int.joa[[1]])) 


###############################################################
### MODEL: agb ~ w + n + g ---------------------------------------
model.agb.wng.add.joa <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% #
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.add.joa = map(data, # runs model in each litte dataset
                                ~ lm(biomass ~
                                       warming + Namount_kg_ha_y + grazing_lvl,
                                     data = .x)))#,
#result.agb.wng.add.joa = map(model.agb.wng.add.joa, tidy)) #%>%
#unnest(result.agb.wng.add.joa) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w + n + g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumtions 
#check_model(model.agb.wng.add.joa$model.agb.wng.add.joa[[1]]) 
# all diagnostic plots looks good (green/blue) but 
# posterior predictive check is spiky and 
# normality of residuals is not normal at the right 



###############################################################
### MODEL: agb ~ w * n + g ----------------------------------
model.agb.wng.intadd.joa <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.intadd.joa = map(data, # runs model in each litte dataset
                                   ~ lm(biomass ~
                                          warming * Namount_kg_ha_y + grazing_lvl,
                                        data = .x)))#,
#result.agb.wng.intadd.joa = map(model.agb.wng.intadd.joa, tidy)) #%>%
#unnest(result.agb.wng.intadd.joa) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w * n + g - check model --------------------

## check assumptions 
#check_model(model.agb.wng.intadd.joa $ model.agb.wng.intadd.joa[[1]])
## all assumptions looks good 



###############################################################
### MODEL: agb ~ w + n * g ----------------------------------
model.agb.wng.addint.joa <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.addint.joa = map(data, # runs model in each litte dataset
                                   ~ lm(biomass ~
                                          warming + Namount_kg_ha_y * grazing_lvl,
                                        data = .x)))#,
#result.agb.wng.addint.joa = map(model.agb.wng.addint.joa, tidy)) #%>%
#unnest(result.agb.wng.addint.joa) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w + n * g - check model --------------------

## check assumptions 
#check_model(model.agb.wng.addint.joa $ model.agb.wng.addint.joa[[1]])
## looks good



###############################################################
### MODEL: agb ~ w * g + n ----------------------------------
model.agb.wgn.intadd.joa <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wgn.intadd.joa = map(data, # runs model in each litte dataset
                                   ~ lm(biomass ~
                                          warming * grazing_lvl + Namount_kg_ha_y,
                                        data = .x)))#,
#result.agb.wgn.intadd.joa = map(model.agb.wgn.intadd.joa, tidy)) #%>%
#unnest(result.agb.wgn.intadd.joa) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w * g + n - check model --------------------

## check assumptions 
#check_model(model.agb.wgn.intadd.joa $ model.agb.wgn.intadd.joa[[1]])

###############################################################
### compare models SUB ALPINE / JOA ---------------------------------
model_comparison_agb.joa <- compare_performance(
  model.agb.wng.add.joa$model.agb.wng.add.joa[[1]],         # w + n + g 
  model.agb.wng.addint.joa$model.agb.wng.addint.joa[[1]],   # w + n * g
  model.agb.wng.intadd.joa$model.agb.wng.intadd.joa[[1]],   # w * n + g
  model.agb.wgn.intadd.joa$model.agb.wgn.intadd.joa[[1]],   # w * g + n
  model.agb.wng.int.joa$model.agb.wng.int.joa[[1]]          # w * n * g
)


# plot.models.agb.ng <-
#   plot(model_comparison_agb.joa)



## lia 
model.agb.wng.intadd.lia$model.agb.wng.intadd.lia[[1]]


# making output of best model -----------------------------------
# agb ~ w * n + g
options(scipen = 100, digits = 4)
## running model with result and unnest to create output
output.model.agb.lia <- agb.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% #
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.intadd.lia = map(data, # runs model in each litte dataset
                            ~ lm(biomass ~
                                   warming * Namount_kg_ha_y + grazing_lvl,
                                 data = .)),
    result.agb.wng.intadd.lia = map(model.agb.wng.intadd.lia, tidy)) %>%
  unnest(result.agb.wng.intadd.lia) %>% # opens the nested dataframes
  select(origSiteID, term, estimate, std.error, statistic, p.value)
# View()

## making clean and readable output for table ---------------------
clean_output.model.agb.lia <- output.model.agb.lia %>%
  mutate(term = case_when(
    (term == "(Intercept)") ~ "Intercept (no N, no warming, no grazing)",
    (term == "warmingWarming") ~ "Warming",
    (term == "Namount_kg_ha_y") ~ "Nitrogen",
    (term == "grazing_lvl") ~ "Grazing",
    (term == "warmingWarming:Namount_kg_ha_y") ~ "Warming : Nitrogen",
    (term == "warmingWarming:grazing_lvl") ~ "Warming : Grazing",
    (term == "Namount_kg_ha_y:grazing_lvl") ~ "Nitrogen : Grazing",
    (term == "warmingWarming:Namount_kg_ha_y:grazing_lvl") ~
      "Warming : Nitrogen : Grazing"
  ))


## figures -----------------------------------------
## add nice label and show this plot?
#plot(check_collinearity(model.agb.wng.int $ model.agb.wng.int[[1]]))


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
  labs(title = "Aboveground productivity", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Biomass~(g))) + 
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm")
plot_agb_wng


