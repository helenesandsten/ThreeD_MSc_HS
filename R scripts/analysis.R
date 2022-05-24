### mumin analysis 

source("R scripts/ThreeD_load_packages.R") 
source("R scripts/ThreeD_create_metadata.R") 
source("R scripts/MSc_aesthetics.R") 
source("R scripts/aboveground_biomass.R") 
source("R scripts/roots.R")
source("R scripts/teabags.R")
source("R scripts/soil.R")

### ABOVEGROUND PRODUCTIVITY ----------------------------------------
## running models for aboveground productivity
options(na.action = "na.fail") 

## models for alpine site 
fit_models_agb_alp <- 
  lm(biomass_m2 ~ warming * Namount_kg_ha_y * grazing_lvl, data = agb.alp.df) 
dredge(fit_models_agb_alp, rank = "AICc", extra = "adjR^2") 

# best models from dredge 
mod.agb.wng.int.lia <- 
  lm(biomass_m2 ~ warming * Namount_kg_ha_y * grazing_lvl, data = agb.alp.df)
# checking model assumptions 
check_model(mod.agb.wng.int.lia) # ok 


## models for sub-alpine site 
fit_models_agb_sub <- 
  lm(biomass_m2 ~ warming * Namount_kg_ha_y * grazing_lvl, data = agb.sub.df) 
dredge(fit_models_agb_sub, rank = "AICc", extra = "adjR^2") 

# best models from dredge 
mod.agb.g.lia <- 
  lm(biomass_m2 ~ grazing_lvl, data = agb.sub.df)
# checking model assumptions 
check_model(mod.agb.g.lia) # not good

mod.agb.wg.int.lia <- 
  lm(biomass_m2 ~ warming * grazing_lvl, data = agb.sub.df)
# checking model assumptions 
check_model(mod.agb.wg.int.lia) # looks ok

mod.agb.wng.int.lia <- 
  lm(biomass_m2 ~ warming * Namount_kg_ha_y * grazing_lvl, data = agb.sub.df)
# checking model assumptions 
check_model(mod.agb.wng.int.lia) # looks ok 


### BELOWGROUND PRODUCTIVITY ----------------------------------------

## models for alpine site 
fit_models_roots_alp <- 
  lm(root_mass_cm3 ~ warming * Namount_kg_ha_y * grazing_lvl, data = roots.alp.df) 
dredge(fit_models_roots_alp, rank = "AICc", extra = "adjR^2") 

## models for sub-alpine site 
fit_models_roots_sub <- 
  lm(root_mass_cm3 ~ warming * Namount_kg_ha_y * grazing_lvl, data = roots.sub.df) 
dredge(fit_models_roots_sub, rank = "AICc", extra = "adjR^2") 


### DECOMPOSITION ------------------------------------------------
## models for mass loss of tea -----------------------------------
## models for mass loss of green tea
## models for alpine site 
fit_models_tea_green_alp <- 
  lm(mass_loss_proportion ~ warming * Namount_kg_ha_y * grazing_lvl, data = tea.green.alp.df) 
dredge(fit_models_tea_green_alp, rank = "AICc", extra = "adjR^2") 

## models for sub-alpine site 
fit_models_tea_green_sub <- 
  lm(mass_loss_proportion ~ warming * Namount_kg_ha_y * grazing_lvl, data = tea.green.sub.df) 
dredge(fit_models_tea_green_sub, rank = "AICc", extra = "adjR^2")  


## models for mass loss of rooibos tea
## models for alpine site 
fit_models_tea_red_alp <- 
  lm(mass_loss_proportion ~ warming * Namount_kg_ha_y * grazing_lvl, data = tea.red.alp.df) 
dredge(fit_models_tea_red_alp, rank = "AICc", extra = "adjR^2") 

## models for sub-alpine site 
fit_models_tea_red_sub <- 
  lm(mass_loss_proportion ~ warming * Namount_kg_ha_y * grazing_lvl, data = tea.red.sub.df) 
dredge(fit_models_tea_red_sub, rank = "AICc", extra = "adjR^2")  



## models decomposition rate k and stabilization factor S ---------
options(na.action = "na.fail") 

## models for decomposition rate k (only for alpine site)
## models for alpine site 
fit_models_decomp_k_alp <-
  lm(k ~ warming * Namount_kg_ha_y * grazing_lvl, data = decomp.k.alp.df)
dredge(fit_models_decomp_k_alp, rank = "AICc", extra = "adjR^2")

## models for sub-alpine site
# fit_models_decomp_k_sub <-
#   lm(k ~ warming * Namount_kg_ha_y * grazing_lvl, data = decomp.k.sub.df)
# dredge(fit_models_decomp_k_sub, rank = "AICc", extra = "adjR^2")


## models for stabilization factor S
## models for alpine site 
fit_models_decomp_s_alp <-
  lm(S ~ warming * Namount_kg_ha_y * grazing_lvl, data = decomp.s.alp.df)
dredge(fit_models_decomp_s_alp, rank = "AICc", extra = "adjR^2")

## models for sub-alpine site
fit_models_decomp_s_sub <-
  lm(S ~ warming * Namount_kg_ha_y * grazing_lvl, data = decomp.s.sub.df)
dredge(fit_models_decomp_s_sub, rank = "AICc", extra = "adjR^2")


### SOIL ORGANIC MATTER ----------------------------------------

## models for alpine site
fit_models_soil_alp <-
  lm(prop_org_mat ~ warming * Namount_kg_ha_y * grazing_lvl, data = soil.alp.df)
dredge(fit_models_soil_alp, rank = "AICc", extra = "adjR^2")

## models for sub-alpine site
fit_models_soil_sub <-
  lm(prop_org_mat ~ warming * Namount_kg_ha_y * grazing_lvl, data = soil.sub.df)
dredge(fit_models_soil_sub, rank = "AICc", extra = "adjR^2")


