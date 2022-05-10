##### MASTER AESTHETICS 
source("R scripts/ThreeD_load_packages.R")

## tables
## table for nitrogen amount in blocks 
nitrogen_table <- tibble(
  "Block" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  "N level" = c(1,6,5,3,10,7,4,8,9,2),
  "N kg/ha/y" = c(0, 5, 1, 0, 150, 10, 0.5, 50, 100, 0))

## table for nitrogen amount in blocks 
data_variable_table <- tibble(
  "Parameter investigated" = c("Aboveground productivity", 
                               "Belowground productivity", 
                               "Decomposition", 
                               "Soil organic matter"),
  # "Dataset" = c("THREE-D_clean_biomass_2020_2021.csv", 
  #                 "ThreeD_rootingrowthcores_2021.xlsx", 
  #                 "THREE_D_clean_decomposition_fall_2021.csv", 
  #                 "ThreeD_soilcores_2021.xlsx"),
  "Explanatory variables" = c("Warming, Nitrogen, Grazing", 
                              "Warming, Nitrogen, Grazing",
                              "Warming, Nitrogen, Grazing",
                              "Warming, Nitrogen, Grazing"),
  "Response variables" = c("Biomass (g) per m2", 
                           "Root mass (g) per cm3",
                           "Proportion of mass left after growing season",
                           "Proportion of organic matter per sample")
  )

models_table <- tibble(
  "Model #" = c("model 1", 
                "model 2", 
                "model 3", 
                "model 4", 
                "model 5"),
  "Equation" = c("y ~ w + n + g", "y ~ w + n * g", "y ~ w * n + g", "y ~ w * g + n", "y ~ w * n * g"),
  "Explanation" = c("additive model",
                    "n-g-interaction model",
                    "w-n-interaction model", 
                    "w-g-interaction model",
                    "three-way interaction model")
)


best_models_table_both <- tibble(
  "Site"    = c("Alpine", "Alpine", "Alpine", "Alpine", "Alpine",
                "Sub-alpine", "Sub-alpine", "Sub-alpine", "Sub-alpine", "Sub-alpine"),
  
  "Model"   = c("w + n + g", "w + n * g", "w * n + g", "w * g + n", "w * n * g",
                "w + n + g", "w + n * g", "w * n + g", "w * g + n", "w * n * g"),
  
  "AGB"     = c("-", "-", "BEST", "-", "-",
                "BEST", "-", "-", "-", "-"),
  
  "Roots"   = c("-", "-", "BEST", "-", "-",
                "BEST", "-", "-", "-", "-"),
  
  "Decomp." = c("-", "-", "-", "-", "-",
                "-", "-", "-", "-", "-"),
  
  "SOM"     = c("BEST", "-", "-", "-", "-",
                "good", "-", "-", "BEST", "-")
)


## figure labels --------------------------------------
## appumptions and diagnostic plots - x-axis 
label_x_diagnostic <- c("Namount_kg_ha_y" = "N",
                   "warming" = "W",
                   "grazing_lvl" = "G",
                   "Namount_kg_ha_y:grazing_lvl" = "N:G",
                   "warming:grazing_lvl" = "W:G",
                   "warming:Namount_kg_ha_y" = "W:N",
                   "warming:Namount_kg_ha_y:grazing_lvl" = "W:N:G"
)



## figure settings for all figures
# theme, size, 

## colors for figures
# scale_fill_manual(values = ___)

# colors for grazing 
colors_g <- c("Control" = "#8FAADC", "Intensive" = "#F8CBAD", 
              "Medium" = "#FFE699", "Natural" = "#A9D18E")

# colors for warming 
colors_w <- c("Ambient" = "black", "Warming" = "firebrick2")

# colors_n 
colors_Nlevel <- c("1" = "#969696",
                   "2" = "#6baed6",
                   "3" = "#9ecae1",
                   "4" = "#969696",
                   "5" = "#08306b",
                   "6" = "#4292c6",
                   "7" = "#c6dbef",
                   "8" = "#2171b5",
                   "9" = "#08519c",
                   "10" = "#969696"
                   )


#scale_fill_viridis(option = "mako") 

# colors for teabags
colors_tea <- c("olivedrab4", "orangered3")

# better table names 
# clean_output.DATA.XXX <- output.DATA.XXX %>%
#   mutate(term = case_when(
#     (term == "(Intercept)") ~ "Intercept (no N, not warmed, not grazed)",
#     (term == "Namount_kg_ha_y") ~ "Nitrogen",
#     (term == "warmingWarmed") ~ "Warmed",
#     (term == "grazingIntensive") ~ "Intensive grazing",
#     (term == "grazingMedium") ~ "Medium grazing",
#     (term == "grazingNatural") ~ "Natural grazing",
#     (term == "Namount_kg_ha_y:warmingWarmed") ~ "Nitrogen : Warmed",
#     (term == "Namount_kg_ha_y:grazingIntensive") ~ "Nitrogen : Intensive grazing",
#     (term == "Namount_kg_ha_y:grazingMedium") ~ "Nitrogen : Medium grazing",
#     (term == "Namount_kg_ha_y:grazingNatural") ~ "Nitrogen : Natural grazing",
#     (term == "warmingWarmed:grazingIntensive") ~ "Warmed : Intensive grazing",
#     (term == "warmingWarmed:grazingMedium") ~ "Warmed : Medium grzing",
#     (term == "warmingWarmed:grazingNatural") ~ "Warmed : Natural grazing",
#     (term == "Namount_kg_ha_y:warmingWarmed:grazingIntensive") ~ 
#       "Nitrogen : Warmed : Intensive grazing",
#     (term == "Namount_kg_ha_y:warmingWarmed:grazingMedium") ~ 
#       "Nitrogen : Warmed : Medium grazing",
#     (term == "Namount_kg_ha_y:warmingWarmed:grazingNatural") ~ 
#       "Nitrogen : Warmed : Natural grazing"
#   ))

