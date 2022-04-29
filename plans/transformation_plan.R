## transformation plan 

# source("R scripts/ThreeD_load_packages.R")
# source("R scripts/ThreeD_create_metadata.R")
source("R scripts/download_plan.R")

#tranformation_plan <- list(
  
  ### ROOTS / BELOWGROUND BIOMASS --------------------------------
  
  ## import root biomass and fix it
  tar_target(
    name = roots_clean_df,
    command = {
   
    })
  
  ### ABOVEGROUND BIOMASS -------------------------------------
  # # import biomass and fix it
  # tar_target(
  #   name = data_agbiomass,
  #   command = agb.df <- read_csv("Data/THREE-D_clean_biomass_2020-2021.csv", col_names = TRUE,
  #                                cols(
  #                                  .default = col_double(),
  #                                  origSiteID = col_factor(),
  #                                  origBlockID = col_factor(),
  #                                  turfID = col_character(),
  #                                  destSiteID = col_factor(),
  #                                  destBlockID = col_factor(),
  #                                  Namount_kg_ha_y = col_factor(),
  #                                  warming = col_factor(),
  #                                  grazing = col_factor(),
  #                                  date = col_datetime(format = ""),
  #                                  fun_group = col_factor(),
  #                                  unit = col_character(),
  #                                  collector = col_character(),
  #                                  remark = col_character()
  #                                )) %>% 
  #     filter(year == 2021)
  #     )
  # 
  
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





