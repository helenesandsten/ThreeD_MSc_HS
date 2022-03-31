### 

source("ThreeD_load_packages.R")

## importing data
agb.df <- read_csv("Data/THREE-D_clean_biomass_2020-2021.csv", col_names = TRUE,
                    cols(
                      .default = col_double(),
                      origSiteID = col_factor(),
                      origBlockID = col_factor(),
                      turfID = col_character(),
                      destSiteID = col_factor(),
                      destBlockID = col_factor(),
                      Namount_kg_ha_y = col_factor(),
                      warming = col_factor(),
                      grazing = col_factor(),
                      date = col_datetime(format = ""),
                      fun_group = col_factor(),
                      unit = col_character(),
                      collector = col_character(),
                      remark = col_character()
                    )) %>% 
  filter(year == 2021)

str(agb.df)





