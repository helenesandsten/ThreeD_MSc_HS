### ANALYSIS AND MODELS ROOTS

# model 1 
lm_roots <- lm()


roots.df %>%
  group_by(origSiteID, grazing) %>%
  nest() %>% #makes little dataframes inside my data, closed
  mutate(model.roots = map(data, # runs model in each litte dataset
                     ~ lm(root_mass_g ~ log(Namount_kg_ha_y +1) * warming, data = .)),
         result.roots = map(model.roots, tidy)) %>%
  unnest(result.roots) %>% # opens the nested dataframes 
  View()

