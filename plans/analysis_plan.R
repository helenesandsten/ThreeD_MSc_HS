# analysis plan 

source("R scripts/transformation_plan.R")

analysis_plan <- list(
  tar_target(
    name = model.agb.wng.int.lia,
    command = {
    ### MODEL: agb ~ w * n * g ---------------------------------------
    model.agb.wng.int.lia <- agb.df %>%
      # removing grazing level N to reduce degrees of freedom
      filter(!grazing == "Natural") %>% 
      group_by(origSiteID) %>% 
      filter(origSiteID == "Lia") %>% 
      nest() %>% # makes little dataframes inside my data, closed
      mutate(
        model.agb.wng.int.lia = map(data, # runs model in each litte dataset
                                    ~ lm(biomass_m2 ~
                                           warming * Namount_kg_ha_y * grazing_lvl,
                                         data = .x)))#,
    #result.agb.wng.int.lia = map(model.agb.wng.int.lia, tidy)) #%>%
    #unnest(result.agb.wng.int.lia) #%>% # opens the nested dataframes
    # View()
  }
),

tar_target(
  name = model.agb.wng.int.lia,
  command = {
    ### MODEL: agb ~ w * n * g ---------------------------------------
    model.agb.wng.int.lia <- agb.df %>%
      # removing grazing level N to reduce degrees of freedom
      filter(!grazing == "Natural") %>% 
      group_by(origSiteID) %>% 
      filter(origSiteID == "Lia") %>% 
      nest() %>% # makes little dataframes inside my data, closed
      mutate(
        model.agb.wng.int.lia = map(data, # runs model in each litte dataset
                                    ~ lm(biomass_m2 ~
                                           warming * Namount_kg_ha_y * grazing_lvl,
                                         data = .x)))#,
    #result.agb.wng.int.lia = map(model.agb.wng.int.lia, tidy)) #%>%
    #unnest(result.agb.wng.int.lia) #%>% # opens the nested dataframes
    # View()
  }
),

)