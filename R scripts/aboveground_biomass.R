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
                          "A" = "Ambient", "W" = "Warmed")) %>% 
  # reordering variables 
  mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa")),
         grazing = factor(grazing, 
                          levels = c("C" = "Control", 
                                     "M" = "Medium",
                                     "I" = "Intensive",
                                     "N" = "Natural"))) 

## analysis agb ----------------------------------------

### MODEL: agb ~ w * n * g ---------------------------------------
model.agb.wng.int <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.int = map(data, # runs model in each litte dataset
                              ~ lm(biomass ~
                                     warming * Namount_kg_ha_y * grazing,
                                   data = .)))#,
#result.agb.wng.int = map(model.agb.wng.int, tidy)) #%>%
#unnest(result.agb.wng.int) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w * n * g - check model --------------------
## must run model code without result- and unnest-line for this to be useful
## check performance 
model_performance(model.agb.wng.int $ model.agb.wng.int[[1]])
## check assumptions 
check_model(model.agb.wng.int $ model.agb.wng.int[[1]])
## check plots that are off: collinearity 
## exspected as the model has interaction terms
#check_collinearity(model.agb.wng.int $ model.agb.wng.int[[1]])



###############################################################
### MODEL: agb ~ w + n + g ---------------------------------------
model.agb.wng.add <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.add = map(data, # runs model in each litte dataset
                              ~ lm(biomass ~
                                     warming + Namount_kg_ha_y + grazing,
                                   data = .)))#,
#result.agb.wng.add = map(model.agb.wng.add, tidy)) #%>%
#unnest(result.agb.wng.add) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w + n + g - check model --------------------
## must run model code without result- and unnest-line for this to be useful
## check performance 
#model_performance(model.agb.wng.add$model.agb.wng.add[[1]]) 
## check assumtions 
#check_model(model.agb.wng.add$model.agb.wng.add[[1]]) 
# all diagnostic plots looks good



###############################################################
### MODEL: agb ~ w * n + g ----------------------------------
model.agb.wng.intadd <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.intadd = map(data, # runs model in each litte dataset
                                 ~ lm(biomass ~
                                        warming * Namount_kg_ha_y + grazing,
                                      data = .)))#,
#result.agb.wng.intadd = map(model.agb.wng.intadd, tidy)) #%>%
#unnest(result.agb.wng.intadd) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w * n + g - check model --------------------
## check performance 
#model_performance(model.agb.wng.intadd $ model.agb.wng.intadd[[1]])
## check assumptions 
#check_model(model.agb.wng.intadd $ model.agb.wng.intadd[[1]])
## all assumptions looks good 



###############################################################
### MODEL: agb ~ w + n * g ----------------------------------
model.agb.wng.addint <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wng.intadd = map(data, # runs model in each litte dataset
                                 ~ lm(biomass ~
                                        warming + Namount_kg_ha_y * grazing,
                                      data = .)))#,
#result.agb.wng.intadd = map(model.agb.wng.intadd, tidy)) #%>%
#unnest(result.agb.wng.intadd) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w + n * g - check model --------------------
## check performance 
#model_performance(model.agb.wng.intadd $ model.agb.wng.intadd[[1]])
## check assumptions 
#check_model(model.agb.wng.intadd $ model.agb.wng.intadd[[1]])



###############################################################
### MODEL: agb ~ w * g + n ----------------------------------
model.agb.wgn.intadd <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wgn.intadd = map(data, # runs model in each litte dataset
                                 ~ lm(biomass ~
                                        warming * grazing + Namount_kg_ha_y,
                                      data = .)))#,
#result.agb.wgn.intadd = map(model.agb.wgn.intadd, tidy)) #%>%
#unnest(result.agb.wgn.intadd) #%>% # opens the nested dataframes
# View()

### MODEL: agb ~ w * g + n - check model --------------------
## check performance 
#model_performance(model.agb.wgn.intadd $ model.agb.wgn.intadd[[1]])
## check assumptions 
#check_model(model.agb.wgn.intadd $ model.agb.wgn.intadd[[1]])


###############################################################
### MODEL: agb ~ w * n ---------------------------------
model.agb.wn.int <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wn.int = map(
      data, # runs model in each litte dataset
      ~ lm(biomass ~ warming * Namount_kg_ha_y,
           data = .)))
#    result.agb.wn.int = map(model.agb.wn.int, tidy) %>%
#  unnest(result.agb.wn.int) # opens the nested dataframes
#  #View()

### MODEL: agb ~ w * n - check model ------------
## check performance 
#model_performance(model.agb.wn.int$model.agb.wn.int[[1]])
## check assumptions 
#check_model(model.agb.wn.int$model.agb.wn.int[[1]]) # looks alright 

###############################################################
### MODEL: agb ~ w + n ---------------------------------
model.agb.wn.add <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wn.add = map(
      data, # runs model in each litte dataset
      ~ lm(biomass ~ warming + Namount_kg_ha_y,
           data = .)))
#    result.agb.wn.add = map(model.agb.wn.add, tidy) %>%
#  unnest(result.agb.wn.add) # opens the nested dataframes
#  #View()

### MODEL: agb ~ w + n - check model ------------
## check performance 
#model_performance(model.agb.wn.add$model.agb.wn.add[[1]])
## check assumptions 
#check_model(model.agb.wn.add$model.agb.wn.add[[1]])
###############################################################
### MODEL: agb ~ w * g ---------------------------------
model.agb.wg.int <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wg.int = map(
      data, # runs model in each litte dataset
      ~ lm(biomass ~ warming * grazing,
           data = .)))
#    result.agb.wg.int = map(model.agb.wg.int, tidy) %>%
#  unnest(result.agb.wg.int) # opens the nested dataframes
#  #View()

### MODEL: agb ~ w * g - check model ------------
## check performance 
#model_performance(model.agb.wg.int$model.agb.wg.int[[1]])
## check assumptions 
#check_model(model.agb.wg.int$model.agb.wg.int[[1]])
## check plots that are off
#check_outliers(model.agb.wg.int$model.agb.wg.int[[1]])
#check_collinearity(model.agb.wg.int$model.agb.wg.int[[1]])
###############################################################
### MODEL: agb ~ w + g ---------------------------------
model.agb.wg.add <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.wg.add = map(
      data, # runs model in each litte dataset
      ~ lm(biomass ~ warming + grazing,
           data = .)))
#    result.agb.wg.add = map(model.agb.wg.add, tidy) %>%
#  unnest(result.agb.wg.add) # opens the nested dataframes
#  #View()

### MODEL: agb ~ w + g - check model ------------
## check performance 
#model_performance(model.agb.wg.add$model.agb.wg.add[[1]])
## check assumptions 
#check_model(model.agb.wg.add$model.agb.wg.add[[1]])
###############################################################
### MODEL: agb ~ n * g ---------------------------------
model.agb.ng.int <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.ng.int = map(
      data, # runs model in each litte dataset
      ~ lm(biomass ~ Namount_kg_ha_y * grazing,
           data = .)))
#    result.agb.ng.int = map(model.agb.ng.int, tidy) %>%
#  unnest(result.agb.ng.int) # opens the nested dataframes
#  #View()

### MODEL: agb ~ n * g - check model ------------
## check performance 
#model_performance(model.agb.ng.int$model.agb.ng.int[[1]])
## check assumptions 
#check_model(model.agb.ng.int$model.agb.ng.int[[1]]) # looks alright 

###############################################################
### MODEL: agb ~ n + g ---------------------------------
model.agb.ng.add <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.ng.add = map(
      data, # runs model in each litte dataset
      ~ lm(biomass ~ warming + Namount_kg_ha_y,
           data = .)))
#    result.agb.ng.add = map(model.agb.ng.add, tidy) %>%
#  unnest(result.agb.ng.add) # opens the nested dataframes
#  #View()

### MODEL: agb ~ n + g  - check model ------------
## check performance 
#model_performance(model.agb.ng.add$model.agb.ng.add[[1]])
## check assumptions 
#check_model(model.agb.ng.add$model.agb.ng.add[[1]])

###############################################################
### MODEL: agb ~ w ---------------------------------
model.agb.w <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.w = map(
      data, # runs model in each litte dataset
      ~ lm(biomass ~ warming,
           data = .)))
#    result.agb.w = map(model.agb.w, tidy) %>%
#  unnest(result.agb.w) # opens the nested dataframes
#  #View()

### MODEL: agb ~ w - check model ------------
## check performance 
#model_performance(model.agb.w$model.agb.w[[1]])
## check assumptions 
#check_model(model.agb.w$model.agb.w[[1]])
###############################################################
### MODEL: agb ~ n ---------------------------------
model.agb.n <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.n = map(
      data, # runs model in each litte dataset
      ~ lm(biomass ~ Namount_kg_ha_y,
           data = .)))
#    result.agb.n = map(model.agb.n, tidy) %>%
#  unnest(result.agb.n) # opens the nested dataframes
#  #View()

### MODEL: agb ~ n - check model ------------
## check performance 
#model_performance(model.agb.n$model.agb.n[[1]])
## check assumptions 
#check_model(model.agb.n$model.agb.n[[1]])
###############################################################
### MODEL: agb ~ g ---------------------------------
model.agb.g <- agb.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.agb.g = map(
      data, # runs model in each litte dataset
      ~ lm(biomass ~ grazing,
           data = .)))
#    result.agb.g = map(model.agb.g, tidy) %>%
#  unnest(result.agb.g) # opens the nested dataframes
#  #View()

### MODEL: agb ~ g - check model ------------
## check performance 
#model_performance(model.agb.g$model.agb.g[[1]])
## check assumptions 
#check_model(model.agb.g$model.agb.g[[1]])

###############################################################
### compare models -------------------------------------------
### warming & grazing  
compare_performance(
  model.agb.wng.int$model.agb.wng.int[[1]],        # w * n * g
  model.agb.wng.add$model.agb.wng.add[[1]],        # w + n + g 
  model.agb.wng.intadd$model.agb.wng.intadd[[1]],  # w * n + g
  model.agb.wgn.intadd$model.agb.wgn.intadd[[1]],  # w + n * g 
  model.agb.wn.int$model.agb.wn.int[[1]],          # w * n
  model.agb.wn.add$model.agb.wn.add[[1]],          # w + n
  model.agb.wg.int$model.agb.wg.int[[1]],          # w * g
  model.agb.wg.add$model.agb.wg.add[[1]],          # w + g
  model.agb.ng.int$model.agb.ng.int[[1]],          # n * g
  model.agb.ng.add$model.agb.ng.add[[1]])          # n + g

plot.models.agb.ng <-
  plot(compare_performance(
    model.agb.wng.int$model.agb.wng.int[[1]],        # w * n * g
    model.agb.wng.add$model.agb.wng.add[[1]],        # w + n + g 
    model.agb.wng.intadd$model.agb.wng.intadd[[1]],  # w * n + g
    model.agb.wgn.intadd$model.agb.wgn.intadd[[1]],  # w + n * g 
    model.agb.wn.int$model.agb.wn.int[[1]],          # w * n
    model.agb.wn.add$model.agb.wn.add[[1]],          # w + n
    model.agb.wg.int$model.agb.wg.int[[1]],          # w * g
    model.agb.wg.add$model.agb.wg.add[[1]],          # w + g
    model.agb.ng.int$model.agb.ng.int[[1]],          # n * g
    model.agb.ng.add$model.agb.ng.add[[1]])          # n + g
  )

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


