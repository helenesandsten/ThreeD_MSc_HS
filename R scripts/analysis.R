### mumin analysis 

source("R scripts/ThreeD_load_packages.R") 
source("R scripts/ThreeD_create_metadata.R") 
source("R scripts/MSc_aesthetics.R") 
source("R scripts/aboveground_biomass.R") 
source("R scripts/roots.R")

### ABOVEGROUND PRODUCTIVITY ----------------------------------------
## running models for aboveground productivity
options(na.action = "na.fail") 

## models for alpine site 
fit_models_agb_alp <- 
  lm(biomass_m2 ~ warming * Namount_kg_ha_y * grazing_lvl, data = agb.alp.df) 
dredge(fit_models_agb_alp, rank = "AICc", extra = "adjR^2") 

## models for sub-alpine site 
fit_models_agb_sub <- 
  lm(biomass_m2 ~ warming * Namount_kg_ha_y * grazing_lvl, data = agb.sub.df) 
dredge(fit_models_agb_sub, rank = "AICc", extra = "adjR^2") 


### BELOWGROUND PRODUCTIVITY ----------------------------------------

## models for alpine site 
fit_models_roots_alp <- 
  lm(root_mass_cm3 ~ warming * Namount_kg_ha_y * grazing_lvl, data = roots.alp.df) 
dredge(fit_models_roots_alp, rank = "AICc", extra = "adjR^2") 

## models for sub-alpine site 
fit_models_roots_sub <- 
  lm(root_mass_cm3 ~ warming * Namount_kg_ha_y * grazing_lvl, data = roots.sub.df) 
dredge(fit_models_roots_sub, rank = "AICc", extra = "adjR^2") 










