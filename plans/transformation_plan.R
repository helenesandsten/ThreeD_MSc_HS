## transformation plan 

# source("R scripts/ThreeD_load_packages.R")
# source("R scripts/ThreeD_create_metadata.R")
source("R scripts/download_plan.R")

#tranformation_plan <- list(
  

  ## ABOVEGROUND BIOMASS -------------------------------------
  # import biomass and fix it
  tar_target(
    name = data.agb.df,
    command = agb.df <- agb.raw.df %>%
mutate_if(is.character, as.factor) %>%
  mutate(origBlockID = as.factor(origBlockID),
         origPlotID = as.factor(origPlotID),
         destBlockID = as.factor(destBlockID),
         destPlotID = as.factor(destPlotID)) %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% 
  # calculating biomass per m2 to standardize amounts
  mutate(biomass_m2 = ((biomass / area_cm2) * 10000)) %>% 
  filter(year == 2021) %>%
  filter(turfID != "147 WN9C 194") %>%# removing plots with missing forbs biomass
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
  
  

### ROOTS / BELOWGROUND BIOMASS --------------------------------

## import root biomass and fix it
# tar_target(
#   name = roots_clean_df,
#   command = {
#  
#   })


  ### TEABAG / DECOMPOSITION BIOMASS --------------------------------
  
  # # import teabag data and fix it
  # tar_target(
  #   name = data_,
  #   command = 
  # )
  # 
  
  ### SOIL DATA ---------------------------------------------
  
  # # import soil data and fix it 
  # tar_target(
  #   name = data_,
  #   command = 
  # )
  # 
  # 
)





