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
                          "A" = "Ambient", "W" = "Warmed")) %>% 
  mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa")),
         grazing = factor(grazing, 
                          levels = c("C" = "Control", 
                                     "M" = "Medium",
                                     "I" = "Intensive",
                                     "N" = "Natural"))) %>% 
  left_join(NitrogenDictionary, by = "Nlevel") %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1))


## analysis - roots data --------------------------------------------


### MODEL: roots ~ w * n * g ---------------------------------------
model.roots.wng <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng = map(data, # runs model in each litte dataset
                          ~ lm(root_mass_cm3 ~
                                 Namount_kg_ha_y * warming * grazing,
                               data = .)))#,
    #result.roots.wng = map(model.roots.wng, tidy)) #%>%
  #unnest(result.roots.wng) #%>% # opens the nested dataframes
 # View()


### MODEL: roots ~ w * n * g - create table of model outupt ----------
# writes numbers out instead of on exponential form
options(scipen = 100, digits = 4)
## running model with result and unnest to create output 
output.roots.wng <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate( # makes column - do we want that?
    model.roots.wng = map(data, # runs model in each litte dataset
                          ~ lm(root_mass_cm3 ~
                                 Namount_kg_ha_y * warming * grazing,
                               data = .)),
    result.roots.wng = map(model.roots.wng, tidy)) %>%
  unnest(result.roots.wng) %>% # opens the nested dataframes
  select(origSiteID, term, estimate, std.error, statistic, p.value)
#View(output.roots.wng)
## none of the terms are significantly different from intercept

### MODEL: roots ~ w * n * g - make neat table for pdf ------------

# writes numbers out instead of on exponential form
options(scipen = 100, digits = 5)

clean_output.roots.wng <- output.roots.wng %>%
  mutate(term = case_when(
    (term == "(Intercept)") ~ "Intercept (no N, not warmed, not grazed)",
    (term == "Namount_kg_ha_y") ~ "Nitrogen",
    (term == "warmingWarmed") ~ "Warmed",
    (term == "grazingIntensive") ~ "Intensive grazing",
    (term == "grazingMedium") ~ "Medium grazing",
    (term == "grazingNatural") ~ "Natural grazing",
    (term == "Namount_kg_ha_y:warmingWarmed") ~ "Nitrogen : Warmed",
    (term == "Namount_kg_ha_y:grazingIntensive") ~ "Nitrogen : Intensive grazing",
    (term == "Namount_kg_ha_y:grazingMedium") ~ "Nitrogen : Medium grazing",
    (term == "Namount_kg_ha_y:grazingNatural") ~ "Nitrogen : Natural grazing",
    (term == "warmingWarmed:grazingIntensive") ~ "Warmed : Intensive grazing",
    (term == "warmingWarmed:grazingMedium") ~ "Warmed : Medium grzing",
    (term == "warmingWarmed:grazingNatural") ~ "Warmed : Natural grazing",
    (term == "Namount_kg_ha_y:warmingWarmed:grazingIntensive") ~ 
      "Nitrogen : Warmed : Intensive grazing",
    (term == "Namount_kg_ha_y:warmingWarmed:grazingMedium") ~ 
      "Nitrogen : Warmed : Medium grazing",
    (term == "Namount_kg_ha_y:warmingWarmed:grazingNatural") ~ 
      "Nitrogen : Warmed : Natural grazing"
    ))

### MODEL: roots ~ w * n * g - check model performance ------------
## must run model code without result- and unnest-line for this to be useful
model_performance(model.roots.wng$model.roots.wng[[1]])


###############################################################
### MODEL: roots ~ w * n ---------------------------------
model.roots.wn <- roots.df %>%
  group_by(origSiteID, grazing) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wn = map(data, # runs model in each litte dataset
                         ~ lm(root_mass_cm3 ~ Namount_kg_ha_y * warming,
                              data = .)))
#    result.roots.wn = map(model.roots.wn, tidy) %>%
#  unnest(result.roots.wn) # opens the nested dataframes
#  #View()

### MODEL: roots ~ w * n - create table of model outupt ----------
## making dataframe with model output

# writes numbers out instead of on exponential form
options(scipen = 100, digits = 5)

output.roots.wn <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate( # makes column - do we want that?
    model.roots.wn = map(data, # runs model in each litte dataset
                          ~ lm(root_mass_cm3 ~ Namount_kg_ha_y * warming,
                               data = .)),
    result.roots.wn = map(model.roots.wn, tidy)) %>%
  unnest(result.roots.wn) %>% # opens the nested dataframes
  select(origSiteID, term, estimate, std.error, statistic, p.value)
## View(output.roots.wn)

## warming sat Lia ignificantly different from intercept
## warming has an affect on plots at Lia

### MODEL: roots ~ w * n - make neat table for pdf ------------

# writes numbers out instead of on exponential form
options(scipen = 100, digits = 5)

clean_output.roots.wn <- output.roots.wn %>%
  mutate(term = case_when(
    (term == "(Intercept)") ~ "Intercept (no N, not warmed)",
    (term == "Namount_kg_ha_y") ~ "Nitrogen",
    (term == "warmingWarmed") ~ "Warmed",
    (term == "Namount_kg_ha_y:warmingWarmed") ~ "Nitrogen : Warmed"
    ))

### MODEL: roots ~ w * n - check model performance ------------
## checking model performance: model.roots.wn ----------------------
## must run model code without result- and unnest-line for this to be useful
model_performance(model.roots.wn$model.roots.wn[[1]])



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
  labs(title = "Effect of warming, nitrogen and grazing on root growth",
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
