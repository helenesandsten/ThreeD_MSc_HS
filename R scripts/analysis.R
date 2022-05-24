### mumin analysis 

source("R scripts/ThreeD_load_packages.R") 
source("R scripts/ThreeD_create_metadata.R") 
source("R scripts/MSc_aesthetics.R") 
source("R scripts/aboveground_biomass.R") 
source("R scripts/roots.R")
source("R scripts/teabags.R")
source("R scripts/soil.R")

##################################################################
##################################################################
##################################################################
### ABOVEGROUND PRODUCTIVITY -------------------------------------
## running models for aboveground productivity
options(na.action = "na.fail") 

## models for alpine site -------------------------------------  
fit_models_agb_alp <- 
  lm(biomass_m2 ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = agb.alp.df) 
dredge(fit_models_agb_alp, rank = "AICc", extra = "adjR^2") 

## best models from dredge 
##1 , 2, 3
mod.agb.wng.int.lia <- 
  lm(biomass_m2 ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = agb.alp.df)
# checking model assumptions 
check_model(mod.agb.wng.int.lia) # ok 


## models for sub-alpine site ------------------------------------- 
fit_models_agb_sub <- 
  lm(biomass_m2 ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = agb.sub.df) 
dredge(fit_models_agb_sub, rank = "AICc", extra = "adjR^2") 

## best models from dredge 
## 1 
mod.agb.g.sub <- 
  lm(biomass_m2 ~ grazing_lvl, 
     data = agb.sub.df)
# checking model assumptions 
check_model(mod.agb.g.sub) # not good

## 2 
mod.agb.wg.int.sub <- 
  lm(biomass_m2 ~ warming * grazing_lvl, 
     data = agb.sub.df)
# checking model assumptions 
check_model(mod.agb.wg.int.sub) # looks ok

## 3 
mod.agb.wng.int.sub <- 
  lm(biomass_m2 ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = agb.sub.df)
# checking model assumptions 
check_model(mod.agb.wng.int.sub) # looks ok 

##################################################################
##################################################################
##################################################################
### BELOWGROUND PRODUCTIVITY -------------------------------------

## models for alpine site ------------------------------------- 
fit_models_roots_alp <- 
  lm(root_mass_cm3 ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = roots.alp.df) 
dredge(fit_models_roots_alp, rank = "AICc", extra = "adjR^2") 

## best models from dredge 
## 1 
## no model 

## 2 
mod.roots.wn.int.alp <- 
  lm(root_mass_cm3 ~ warming * Namount_kg_ha_y, 
     data = roots.alp.df)
# checking model assumptions 
check_model(mod.roots.wn.int.alp) # ok ---> best

## 3 
mod.roots.w.alp <- 
  lm(root_mass_cm3 ~ warming, 
     data = roots.alp.df)
# checking model assumptions 
check_model(mod.roots.w.alp) # little off


## models for sub-alpine site ------------------------------------- 
fit_models_roots_sub <- 
  lm(root_mass_cm3 ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = roots.sub.df) 
dredge(fit_models_roots_sub, rank = "AICc", extra = "adjR^2") 

## 1 
mod.roots.ng.add.sub <- 
  lm(root_mass_cm3 ~ Namount_kg_ha_y + grazing_lvl, 
     data = roots.sub.df)
# checking model assumptions 
check_model(mod.roots.ng.add.sub) # ok

## 2 
mod.roots.n.sub <- 
  lm(root_mass_cm3 ~ Namount_kg_ha_y, data = roots.sub.df)
# checking model assumptions 
check_model(mod.roots.n.sub) # ok

## 3 
mod.roots.wng.add.sub <- 
  lm(root_mass_cm3 ~ warming + Namount_kg_ha_y + grazing_lvl, 
     data = roots.sub.df)
# checking model assumptions 
check_model(mod.roots.wng.add.sub) # 


##################################################################
##################################################################
##################################################################
### DECOMPOSITION ------------------------------------------------
## models for mass loss of tea -----------------------------------
##################################################################
## models for mass loss of green tea
## models for alpine site 
fit_models_tea_green_alp <- 
  lm(mass_loss_proportion ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = tea.green.alp.df) 
dredge(fit_models_tea_green_alp, rank = "AICc", extra = "adjR^2") 

## 1
## no model

## 2
mod.tea.green.w.alp <-
  lm(mass_loss_proportion ~ warming,
     data = tea.green.alp.df)
# checking model assumptions
check_model(mod.tea.green.w.alp) # not good

## 3
mod.tea.green.n.alp <-
  lm(mass_loss_proportion ~ Namount_kg_ha_y,
     data = tea.green.alp.df)
# checking model assumptions
check_model(mod.tea.green.n.alp) # not good


## models for sub-alpine site 
fit_models_tea_green_sub <- 
  lm(mass_loss_proportion ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = tea.green.sub.df) 
dredge(fit_models_tea_green_sub, rank = "AICc", extra = "adjR^2")  

## 1 
mod.tea.green.w.sub <- 
  lm(mass_loss_proportion ~ warming, 
     data = tea.green.sub.df)
# checking model assumptions
check_model(mod.tea.green.w.sub) # not good

## 2
## no model

## 3
mod.tea.green.wg.add.sub <- 
  lm(mass_loss_proportion ~ warming + grazing_lvl, 
     data = tea.green.sub.df)
# checking model assumptions
check_model(mod.tea.green.wg.add.sub) # little off

##################################################################
## models for mass loss of rooibos tea
## models for alpine site 
fit_models_tea_red_alp <- 
  lm(mass_loss_proportion ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = tea.red.alp.df) 
dredge(fit_models_tea_red_alp, rank = "AICc", extra = "adjR^2") 

## 1
## no model

## 2
mod.tea.red.w.alp <- 
  lm(mass_loss_proportion ~ warming, 
     data = tea.red.alp.df)
# checking model assumptions
check_model(mod.tea.red.w.alp) # not very good

## 3
mod.tea.red.n.alp <- 
  lm(mass_loss_proportion ~ Namount_kg_ha_y, 
     data = tea.red.alp.df)
# checking model assumptions
check_model(mod.tea.red.n.alp) #


## models for sub-alpine site 
fit_models_tea_red_sub <- 
  lm(mass_loss_proportion ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = tea.red.sub.df) 
dredge(fit_models_tea_red_sub, rank = "AICc", extra = "adjR^2")  

## 1
mod.tea.red.w.sub <- 
  lm(mass_loss_proportion ~ warming, 
     data = tea.red.sub.df)
# checking model assumptions
check_model(mod.tea.red.w.sub) # not good

## 2
## no model

## 3
mod.tea.red.wg.add.sub <- 
  lm(mass_loss_proportion ~ warming + grazing_lvl, 
     data = tea.red.sub.df)
# checking model assumptions
check_model(mod.tea.red.wg.add.sub) #

##################################################################
## models decomposition rate k and stabilization factor S ---------
options(na.action = "na.fail") 
##################################################################
## models for decomposition rate k (only for alpine site)
## models for alpine site 
fit_models_decomp_k_alp <-
  lm(k ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = decomp.k.alp.df)
dredge(fit_models_decomp_k_alp, rank = "AICc", extra = "adjR^2")

## 1 
## no model

## 2
mod.decomp.k.g.alp <- 
  lm(k ~ grazing_lvl, 
     data = decomp.k.alp.df)
# checking model assumptions
check_model(mod.decomp.k.g.alp) # little off

## 3
mod.decomp.k.ng.int.alp <- 
  lm(k ~ Namount_kg_ha_y * grazing_lvl, 
     data = decomp.k.alp.df)
# checking model assumptions
check_model(mod.decomp.k.ng.int.alp) # ok (-)


## models for sub-alpine site
# fit_models_decomp_k_sub <-
#   lm(k ~ warming * Namount_kg_ha_y * grazing_lvl, data = decomp.k.sub.df)
# dredge(fit_models_decomp_k_sub, rank = "AICc", extra = "adjR^2")

##################################################################
## models for stabilization factor S
## models for alpine site --------------------------------- 
fit_models_decomp_s_alp <-
  lm(S ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = decomp.s.alp.df)
dredge(fit_models_decomp_s_alp, rank = "AICc", extra = "adjR^2")

## 1 
mod.decomp.s.w.alp <-
  lm(S ~ warming, 
     data = decomp.s.alp.df)
# checking model assumptions
check_model(mod.decomp.s.w.alp) # off

## 2
mod.decomp.s.wn.add.alp <-
  lm(S ~ warming + Namount_kg_ha_y, 
     data = decomp.s.alp.df)
# checking model assumptions
check_model(mod.decomp.s.wn.add.alp) # ok

## 3
mod.decomp.s.wg.add.alp <-
  lm(S ~ warming + grazing_lvl, 
     data = decomp.s.alp.df)
# checking model assumptions
check_model(mod.decomp.s.wg.add.alp) # ok

## models for sub-alpine site ---------------------------------
fit_models_decomp_s_sub <-
  lm(S ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = decomp.s.sub.df)
dredge(fit_models_decomp_s_sub, rank = "AICc", extra = "adjR^2")

## 1 
mod.decomp.s.w.sub <- 
  lm(S ~ warming, 
     data = decomp.s.sub.df)
# checking model assumptions
check_model(mod.decomp.s.w.sub) # litte off
  

## 2
mod.decomp.s.wn.add.sub <- 
  lm(S ~ warming + Namount_kg_ha_y, 
     data = decomp.s.sub.df)
# checking model assumptions
check_model(mod.decomp.s.wn.add.sub) # ok 

## 3
mod.decomp.s.wg.add.sub <- 
  lm(S ~ warming + grazing_lvl, 
     data = decomp.s.sub.df)
# checking model assumptions
check_model(mod.decomp.s.wg.add.sub) # ok 

##################################################################
##################################################################
##################################################################
### SOIL ORGANIC MATTER ----------------------------------------

## models for alpine site
fit_models_soil_alp <-
  lm(prop_org_mat ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = soil.alp.df)
dredge(fit_models_soil_alp, rank = "AICc", extra = "adjR^2")

## 1 
mod.soil.w.alp <-
  lm(prop_org_mat ~ warming, 
     data = soil.alp.df)
# checking model assumptions 
check_model(mod.soil.w.alp) # not good

## 2
mod.soil.wn.add.alp <-
  lm(prop_org_mat ~ warming + Namount_kg_ha_y, 
     data = soil.alp.df)
# checking model assumptions 
check_model(mod.soil.wn.add.alp) # ok ---> best (?)

## 3
mod.soil.wg.add.alp <-
  lm(prop_org_mat ~ warming + grazing_lvl, 
     data = soil.alp.df)
# checking model assumptions 
check_model(mod.soil.wg.add.alp) # little off


## models for sub-alpine site
fit_models_soil_sub <-
  lm(prop_org_mat ~ warming * Namount_kg_ha_y * grazing_lvl, 
     data = soil.sub.df)
dredge(fit_models_soil_sub, rank = "AICc", extra = "adjR^2")

## 1 
mod.soil.w.sub <-
  lm(prop_org_mat ~ warming, 
     data = soil.sub.df)
# checking model assumptions 
check_model(mod.soil.w.sub) # little off

## 2
mod.soil.wg.int.sub <-
  lm(prop_org_mat ~ warming * grazing_lvl, 
     data = soil.sub.df)
# checking model assumptions 
check_model(mod.soil.wg.int.sub) # ok 

## 3
mod.soil.wg.add.sub <-
  lm(prop_org_mat ~ warming + grazing_lvl, 
     data = soil.sub.df)
# checking model assumptions 
check_model(mod.soil.wg.add.sub) # ok 

