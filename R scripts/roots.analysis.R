### ROOTS ANALYSIS

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
  # removing grazing level N too reduce degrees of freedom (Richard)
  filter(!grazing == "Natural") %>% 
  # changing grazing into numbered levels (Richard)
  mutate(grazing_lvl = case_when((grazing == "Control") ~ 0,
                                 (grazing == "Medium") ~ 2,
                                 (grazing == "Intensive") ~ 4)) %>% 
  left_join(NitrogenDictionary, by = "Nlevel") %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) 



## RICHARD analysis - roots data ----------------------------------------
###############################################################
### MODEL: roots ~ w * n * g ---------------------------------------
model.roots.wng.int.2 <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.int.2 = map(data, # runs model in each litte dataset
                              ~ lm(root_mass_cm3 ~
                                     warming * Namount_kg_ha_y * grazing_lvl,
                                   data = .)))#,
#result.roots.wng.int.2 = map(model.roots.wng.int.2, tidy)) #%>%
#unnest(result.roots.wng.int.2) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w * n * g - check model --------------------
## must run model code without result- and unnest-line for this to be useful
## check performance 
#model_performance(model.roots.wng.int.2 $ model.roots.wng.int.2[[1]])
## check assumptions 
#check_model(model.roots.wng.int.2 $ model.roots.wng.int.2[[1]])
## check plots that are off: collinearity 
## exspected as the model has interaction terms
#check_collinearity(model.roots.wng.int.2 $ model.roots.wng.int.2[[1]])

## OUTPUT OF MODEL 1: roots ~ w * n * g

options(scipen = 100, digits = 5)
## running model with result and unnest to create output 
output.roots.wng.2 <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.int.2 = map(data, # runs model in each litte dataset
                                ~ lm(root_mass_cm3 ~
                                       warming * Namount_kg_ha_y * grazing_lvl,
                                     data = .)),
    result.roots.wng.int.2 = map(model.roots.wng.int.2, tidy)) %>%
  unnest(result.roots.wng.int.2) %>% # opens the nested dataframes 
  select(origSiteID, term, estimate, std.error, statistic, p.value)
# View()





###############################################################
### MODEL: roots ~ w + n + g ---------------------------------------
model.roots.wng.add.2 <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.add.2 = map(data, # runs model in each litte dataset
                              ~ lm(root_mass_cm3 ~
                                     warming + Namount_kg_ha_y + grazing_lvl,
                                   data = .)))#,
#result.roots.wng.add = map(model.roots.wng.add.2, tidy)) #%>%
#unnest(result.roots.wng.add) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w + n + g - check model --------------------
## must run model code without result- and unnest-line for this to be useful
## check performance 
#model_performance(model.roots.wng.add.2$model.roots.wng.add.2[[1]]) 
## check assumtions 
#check_model(model.roots.wng.add.2$model.roots.wng.add.2[[1]]) 
# all diagnostic plots looks good



###############################################################
### MODEL: roots ~ w * n + g ----------------------------------
model.roots.wng.intadd.2 <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.intadd.2 = map(data, # runs model in each litte dataset
                                 ~ lm(root_mass_cm3 ~
                                        warming * Namount_kg_ha_y + grazing_lvl,
                                      data = .)))#,
#result.roots.wng.intadd.2 = map(model.roots.wng.intadd.2, tidy)) #%>%
#unnest(result.roots.wng.intadd.2) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w * n + g - check model --------------------
## check performance 
#model_performance(model.roots.wng.intadd.2 $ model.roots.wng.intadd.2[[1]])
## check assumptions 
#check_model(model.roots.wng.intadd.2 $ model.roots.wng.intadd.2[[1]])
## all assumptions looks good 



###############################################################
### MODEL: roots ~ w + n * g ----------------------------------
model.roots.wng.addint.2 <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.addint.2 = map(data, # runs model in each litte dataset
                                 ~ lm(root_mass_cm3 ~
                                        warming + Namount_kg_ha_y * grazing_lvl,
                                      data = .)))#,
#result.roots.wng.intadd.2 = map(model.roots.wng.addint.2, tidy)) #%>%
#unnest(result.roots.wng.intadd.2) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w + n * g - check model --------------------
## check performance 
#model_performance(model.roots.wng.addint.2 $ model.roots.wng.addint.2[[1]])
## check assumptions 
#check_model(model.roots.wng.addint.2 $ model.roots.wng.addint.2[[1]])



###############################################################
### MODEL: roots ~ w * g + n ----------------------------------
model.roots.wgn.intadd.2 <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wgn.intadd.2 = map(data, # runs model in each litte dataset
                                 ~ lm(root_mass_cm3 ~
                                        warming * grazing_lvl + Namount_kg_ha_y,
                                      data = .)))#,
#result.roots.wgn.intadd.2 = map(model.roots.wgn.intadd.2, tidy)) #%>%
#unnest(result.roots.wgn.intadd.2) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w * g + n - check model --------------------
## check performance 
#model_performance(model.roots.wgn.intadd.2 $ model.roots.wgn.intadd.2[[1]])
## check assumptions 
#check_model(model.roots.wgn.intadd.2 $ model.roots.wgn.intadd.2[[1]])

###############################################################
### compare models -------------------------------------------
### warming & nitrogen & grazing 
model_performance_roots <- compare_performance(
  model.roots.wng.int.2$model.roots.wng.int.2[[1]], 
  model.roots.wng.add.2$model.roots.wng.add.2[[1]],
  model.roots.wng.intadd.2$model.roots.wng.intadd.2[[1]],
  model.roots.wng.addint.2$model.roots.wng.addint.2[[1]],
  model.roots.wgn.intadd.2$model.roots.wgn.intadd.2[[1]])

plot_model_performance_roots <-
  plot(compare_performance(
    model.roots.wng.int.2$model.roots.wng.int.2[[1]], 
    model.roots.wng.add.2$model.roots.wng.add.2[[1]],
    model.roots.wng.intadd.2$model.roots.wng.intadd.2[[1]],
    model.roots.wng.addint.2$model.roots.wng.addint.2[[1]],
    model.roots.wgn.intadd.2$model.roots.wgn.intadd.2[[1]]))


## making clean and readable output for table 
clean_output.roots.wng.2 <- output.roots.wng.2 %>%
  mutate(term = case_when(
    (term == "(Intercept)") ~ "Intercept (no N, not warmed, not grazed)",
    (term == "Namount_kg_ha_y") ~ "Nitrogen",
    (term == "warmingWarmed") ~ "Warmed",
    (term == "grazing_lvl") ~ "Grazing",
    (term == "warmingWarmed:Namount_kg_ha_y") ~ "Warmed : Nitrogen",
    (term == "warmingWarmed:grazing_lvl") ~ "Warmed : Grazing",
    (term == "Namount_kg_ha_y:grazing_lvl") ~ "Nitrogen : Grazing",
    (term == "warmingWarmed:Namount_kg_ha_y:grazing_lvl") ~
      "Warmed : Nitrogen : Grazing"
  ))
