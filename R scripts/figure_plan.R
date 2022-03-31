### figures plan


tranformation_plan <- list(
  
  ### ROOTS / BELOWGROUND BIOMASS --------------------------------
  
  ## import root biomass and fix it
  tar_target(
    name = fig_roots_mega,
    command = {
      
      ## MAKE INTO FUNCTION IN "figures_roots.R
      ## megaplot roots
      plot_roots_mega <- roots.df %>% 
        group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
        summarise(root_mass_cm3 = mean(root_mass_cm3)) %>% 
        ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                             y = root_mass_cm3, color = warming)) +
        geom_point() +
        theme_bw(base_size = 20) +
        labs(title = "", 
             x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
             y = bquote(Root~mass~(g/cm^3))) +
        facet_grid(origSiteID ~ grazing) +
        geom_smooth(method = "lm")
      plot_roots_mega
      
      
      
      
    })
  
  ### ABOVEGROUND BIOMASS -------------------------------------
  # # 
  # tar_target(
  #   name = ,
  #   command = )
  # 
  
  ### TEABAG / DECOMPOSITION  --------------------------------
  
  # # 
  # tar_target(
  #   name = ,
  #   command = 
  # )
  # 
  
  ### SOIL  ---------------------------------------------
  
  # # 
  # tar_target(
  #   name = data_,
  #   command = 
  # )
  # 
  # 
)

# # 
# tar_target(
#   name = data_,
#   command = 
# )