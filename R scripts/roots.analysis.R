### ROOTS ANALYSIS

## analysis - roots data --------------------------------------------
###############################################################
### MODEL: roots ~ w * n * g ---------------------------------------
model.roots.wng.int <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.int = map(data, # runs model in each litte dataset
                              ~ lm(root_mass_cm3 ~
                                     warming * Namount_kg_ha_y * grazing,
                                   data = .)))#,
#result.roots.wng.int = map(model.roots.wng.int, tidy)) #%>%
#unnest(result.roots.wng.int) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w * n * g - check model --------------------
## must run model code without result- and unnest-line for this to be useful
## check performance 
#model_performance(model.roots.wng.int $ model.roots.wng.int[[1]])
## check assumptions 
#check_model(model.roots.wng.int $ model.roots.wng.int[[1]])
## check plots that are off: collinearity 
## exspected as the model has interaction terms
#check_collinearity(model.roots.wng.int $ model.roots.wng.int[[1]])



###############################################################
### MODEL: roots ~ w + n + g ---------------------------------------
model.roots.wng.add <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.add = map(data, # runs model in each litte dataset
                              ~ lm(root_mass_cm3 ~
                                     warming + Namount_kg_ha_y + grazing,
                                   data = .)))#,
#result.roots.wng.add = map(model.roots.wng.add, tidy)) #%>%
#unnest(result.roots.wng.add) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w + n + g - check model --------------------
## must run model code without result- and unnest-line for this to be useful
## check performance 
#model_performance(model.roots.wng.add$model.roots.wng.add[[1]]) 
## check assumtions 
#check_model(model.roots.wng.add$model.roots.wng.add[[1]]) 
# all diagnostic plots looks good



###############################################################
### MODEL: roots ~ w * n + g ----------------------------------
model.roots.wng.intadd <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.intadd = map(data, # runs model in each litte dataset
                                 ~ lm(root_mass_cm3 ~
                                        warming * Namount_kg_ha_y + grazing,
                                      data = .)))#,
#result.roots.wng.intadd = map(model.roots.wng.intadd, tidy)) #%>%
#unnest(result.roots.wng.intadd) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w * n + g - check model --------------------
## check performance 
#model_performance(model.roots.wng.intadd $ model.roots.wng.intadd[[1]])
## check assumptions 
#check_model(model.roots.wng.intadd $ model.roots.wng.intadd[[1]])
## all assumptions looks good 



###############################################################
### MODEL: roots ~ w + n * g ----------------------------------
model.roots.wng.addint <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wng.intadd = map(data, # runs model in each litte dataset
                                 ~ lm(root_mass_cm3 ~
                                        warming + Namount_kg_ha_y * grazing,
                                      data = .)))#,
#result.roots.wng.intadd = map(model.roots.wng.intadd, tidy)) #%>%
#unnest(result.roots.wng.intadd) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w + n * g - check model --------------------
## check performance 
#model_performance(model.roots.wng.intadd $ model.roots.wng.intadd[[1]])
## check assumptions 
#check_model(model.roots.wng.intadd $ model.roots.wng.intadd[[1]])



###############################################################
### MODEL: roots ~ w * g + n ----------------------------------
model.roots.wgn.intadd <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wgn.intadd = map(data, # runs model in each litte dataset
                                 ~ lm(root_mass_cm3 ~
                                        warming * grazing + Namount_kg_ha_y,
                                      data = .)))#,
#result.roots.wgn.intadd = map(model.roots.wgn.intadd, tidy)) #%>%
#unnest(result.roots.wgn.intadd) #%>% # opens the nested dataframes
# View()

### MODEL: roots ~ w * g + n - check model --------------------
## check performance 
#model_performance(model.roots.wgn.intadd $ model.roots.wgn.intadd[[1]])
## check assumptions 
#check_model(model.roots.wgn.intadd $ model.roots.wgn.intadd[[1]])


###############################################################
### MODEL: roots ~ w * n ---------------------------------
model.roots.wn.int <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wn.int = map(
      data, # runs model in each litte dataset
      ~ lm(root_mass_cm3 ~ warming * Namount_kg_ha_y,
           data = .)))
#    result.roots.wn.int = map(model.roots.wn.int, tidy) %>%
#  unnest(result.roots.wn.int) # opens the nested dataframes
#  #View()

### MODEL: roots ~ w * n - check model ------------
## check performance 
#model_performance(model.roots.wn.int$model.roots.wn.int[[1]])
## check assumptions 
#check_model(model.roots.wn.int$model.roots.wn.int[[1]]) # looks alright 

###############################################################
### MODEL: roots ~ w + n ---------------------------------
model.roots.wn.add <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wn.add = map(
      data, # runs model in each litte dataset
      ~ lm(root_mass_cm3 ~ warming + Namount_kg_ha_y,
           data = .)))
#    result.roots.wn.add = map(model.roots.wn.add, tidy) %>%
#  unnest(result.roots.wn.add) # opens the nested dataframes
#  #View()

### MODEL: roots ~ w + n - check model ------------
## check performance 
#model_performance(model.roots.wn.add$model.roots.wn.add[[1]])
## check assumptions 
#check_model(model.roots.wn.add$model.roots.wn.add[[1]])
###############################################################
### MODEL: roots ~ w * g ---------------------------------
model.roots.wg.int <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wg.int = map(
      data, # runs model in each litte dataset
      ~ lm(root_mass_cm3 ~ warming * grazing,
           data = .)))
#    result.roots.wg.int = map(model.roots.wg.int, tidy) %>%
#  unnest(result.roots.wg.int) # opens the nested dataframes
#  #View()

### MODEL: roots ~ w * g - check model ------------
## check performance 
model_performance(model.roots.wg.int$model.roots.wg.int[[1]])
## check assumptions 
#check_model(model.roots.wg.int$model.roots.wg.int[[1]])
## check plots that are off
#check_outliers(model.roots.wg.int$model.roots.wg.int[[1]])
check_collinearity(model.roots.wg.int$model.roots.wg.int[[1]])
###############################################################
### MODEL: roots ~ w + g ---------------------------------
model.roots.wg.add <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.wg.add = map(
      data, # runs model in each litte dataset
      ~ lm(root_mass_cm3 ~ warming + grazing,
           data = .)))
#    result.roots.wg.add = map(model.roots.wg.add, tidy) %>%
#  unnest(result.roots.wg.add) # opens the nested dataframes
#  #View()

### MODEL: roots ~ w + g - check model ------------
## check performance 
#model_performance(model.roots.wg.add$model.roots.wg.add[[1]])
## check assumptions 
#check_model(model.roots.wg.add$model.roots.wg.add[[1]])
###############################################################
### MODEL: roots ~ n * g ---------------------------------
model.roots.ng.int <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.ng.int = map(
      data, # runs model in each litte dataset
      ~ lm(root_mass_cm3 ~ Namount_kg_ha_y * grazing,
           data = .)))
#    result.roots.ng.int = map(model.roots.ng.int, tidy) %>%
#  unnest(result.roots.ng.int) # opens the nested dataframes
#  #View()

### MODEL: roots ~ n * g - check model ------------
## check performance 
#model_performance(model.roots.ng.int$model.roots.ng.int[[1]])
## check assumptions 
#check_model(model.roots.ng.int$model.roots.ng.int[[1]]) # looks alright 

###############################################################
### MODEL: roots ~ n + g ---------------------------------
model.roots.ng.add <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.ng.add = map(
      data, # runs model in each litte dataset
      ~ lm(root_mass_cm3 ~ warming + Namount_kg_ha_y,
           data = .)))
#    result.roots.ng.add = map(model.roots.ng.add, tidy) %>%
#  unnest(result.roots.ng.add) # opens the nested dataframes
#  #View()

### MODEL: roots ~ n + g  - check model ------------
## check performance 
#model_performance(model.roots.ng.add$model.roots.ng.add[[1]])
## check assumptions 
#check_model(model.roots.ng.add$model.roots.ng.add[[1]])

###############################################################
### MODEL: roots ~ w ---------------------------------
model.roots.w <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.w = map(
      data, # runs model in each litte dataset
      ~ lm(root_mass_cm3 ~ warming,
           data = .)))
#    result.roots.w = map(model.roots.w, tidy) %>%
#  unnest(result.roots.w) # opens the nested dataframes
#  #View()

### MODEL: roots ~ w - check model ------------
## check performance 
#model_performance(model.roots.w$model.roots.w[[1]])
## check assumptions 
#check_model(model.roots.w$model.roots.w[[1]])
###############################################################
### MODEL: roots ~ n ---------------------------------
model.roots.n <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.n = map(
      data, # runs model in each litte dataset
      ~ lm(root_mass_cm3 ~ Namount_kg_ha_y,
           data = .)))
#    result.roots.n = map(model.roots.n, tidy) %>%
#  unnest(result.roots.n) # opens the nested dataframes
#  #View()

### MODEL: roots ~ n - check model ------------
## check performance 
#model_performance(model.roots.n$model.roots.n[[1]])
## check assumptions 
#check_model(model.roots.n$model.roots.n[[1]])
###############################################################
### MODEL: roots ~ g ---------------------------------
model.roots.g <- roots.df %>%
  group_by(origSiteID) %>% #
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.roots.g = map(
      data, # runs model in each litte dataset
      ~ lm(root_mass_cm3 ~ grazing,
           data = .)))
#    result.roots.g = map(model.roots.g, tidy) %>%
#  unnest(result.roots.g) # opens the nested dataframes
#  #View()

### MODEL: roots ~ g - check model ------------
## check performance 
#model_performance(model.roots.g$model.roots.g[[1]])
## check assumptions 
#check_model(model.roots.g$model.roots.g[[1]])
###############################################################
### compare models -------------------------------------------
### warming & nitrogen & grazing 
compare_performance(model.roots.wng.int$model.roots.wng.int[[1]], 
                    model.roots.wng.add$model.roots.wng.add[[1]],
                    model.roots.wng.intadd$model.roots.wng.intadd[[1]],
                    model.roots.wgn.intadd$model.roots.wgn.intadd[[1]])

plot.models.roots.wng <-
  plot(compare_performance(model.roots.wng.int$model.roots.wng.int[[1]], 
                           model.roots.wng.add$model.roots.wng.add[[1]],
                           model.roots.wng.intadd$model.roots.wng.intadd[[1]],
                           model.roots.wgn.intadd$model.roots.wgn.intadd[[1]])) # best
###############################################################
### compare models -------------------------------------------
### warming & nitrogen  
compare_performance(model.roots.wn.int$model.roots.wn.int[[1]], 
                    model.roots.wn.add$model.roots.wn.add[[1]])

plot.models.roots.wn <-
  plot(compare_performance(model.roots.wn.int$model.roots.wn.int[[1]], 
                           model.roots.wn.add$model.roots.wn.add[[1]])) # best 
###############################################################
### compare models -------------------------------------------
### warming & grazing  
compare_performance(model.roots.wg.int$model.roots.wg.int[[1]], 
                    model.roots.wg.add$model.roots.wg.add[[1]])

plot.models.roots.wg <-
  plot(compare_performance(model.roots.wg.int$model.roots.wg.int[[1]],
                           # 1 best but check assumptions
                           model.roots.wg.add$model.roots.wg.add[[1]]))
###############################################################
### compare models -------------------------------------------
### warming & grazing  
compare_performance(model.roots.ng.int$model.roots.ng.int[[1]], 
                    model.roots.ng.add$model.roots.ng.add[[1]])

plot.models.roots.ng <-
  plot(compare_performance(model.roots.ng.int$model.roots.ng.int[[1]], 
                           model.roots.ng.add$model.roots.ng.add[[1]])) # best
###############################################################
### compare models -------------------------------------------
### warming & grazing  
compare_performance(
  model.roots.wng.int$model.roots.wng.int[[1]],        # w * n * g
  model.roots.wng.add$model.roots.wng.add[[1]],        # w + n + g 
  model.roots.wng.intadd$model.roots.wng.intadd[[1]],  # w * n + g
  model.roots.wgn.intadd$model.roots.wgn.intadd[[1]],  # w + n * g 
  model.roots.wn.int$model.roots.wn.int[[1]],          # w * n
  model.roots.wn.add$model.roots.wn.add[[1]],          # w + n
  model.roots.wg.int$model.roots.wg.int[[1]],          # w * g
  model.roots.wg.add$model.roots.wg.add[[1]],          # w + g
  model.roots.ng.int$model.roots.ng.int[[1]],          # n * g
  model.roots.ng.add$model.roots.ng.add[[1]])          # n + g

plot.models.roots.ng <-
  plot(compare_performance(
    model.roots.wng.int$model.roots.wng.int[[1]],        # w * n * g
    model.roots.wng.add$model.roots.wng.add[[1]],        # w + n + g 
    model.roots.wng.intadd$model.roots.wng.intadd[[1]],  # w * n + g
    model.roots.wgn.intadd$model.roots.wgn.intadd[[1]],  # w + n * g 
    model.roots.wn.int$model.roots.wn.int[[1]],          # w * n
    model.roots.wn.add$model.roots.wn.add[[1]],          # w + n
    model.roots.wg.int$model.roots.wg.int[[1]],          # w * g
    model.roots.wg.add$model.roots.wg.add[[1]],          # w + g
    model.roots.ng.int$model.roots.ng.int[[1]],          # n * g
    model.roots.ng.add$model.roots.ng.add[[1]])          # n + g
  )