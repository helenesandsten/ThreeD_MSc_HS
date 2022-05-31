### download plan 

source("R scripts/ThreeD_load_packages.R")
source("R scripts/ThreeD_create_metadata.R")
source("R scripts/MSc_aesthetics.R")

## Data dowloaded from osf

download_plan <- list(
  ### ABOVEGROUND BIOMASS -------------------------------------
  
  ## import root biomass and fix it
  tar_target(
    name = agb.raw.df,
    command = {
      agb.raw.df <- read_csv("Data/THREE-D_clean_biomass_2020-2021.csv")
    }),
  
  ## ROOTS / BELOWGROUND BIOMASS --------------------------------
  #
  tar_target(
    name = roots.raw.df,
    command = {
      roots.raw.df <- read_excel("Data/ThreeD_rootingrowthcores_2021.xlsx") 
  }),

  
  ### TEABAG / DECOMPOSITION  --------------------------------
  
  #
  tar_target(
    name = teabag.raw.df,
    command = {
      teabag.raw.df <- read_csv("Data/THREE-D_clean_decomposition_fall_2021.csv")
  }
  ),
  
  tar_target(
    name = string.bag.weight.df,
    command = {
      string.bag.weight.df <- read_excel("Data/additional_info.xlsx", 
                                         sheet = "string_bag_weight")
    }
  ),
  
  tar_target(
    name = tb.reweighed.df,
    command = {
      tb.reweighed.df <- read_excel("Data/additional_info.xlsx", 
                                    sheet = "teabags_reweighed")
    }
  ),

  
  ### SOIL  ---------------------------------------------
  
  tar_target(
    name = soil.raw.df,
    command = {
      soil.raw.df <- read_csv2(file = "Data/ThreeD_soilcores_2021.csv")
    }
  )


  )
  

  # tar_target(
  #   name = data_,
  #   command =
  # )
  

