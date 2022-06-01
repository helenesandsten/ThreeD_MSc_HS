##### MASTER AESTHETICS 
source("R scripts/ThreeD_load_packages.R")

## figure labels --------------------------------------
## appumptions and diagnostic plots - x-axis 
# label_x_diagnostic <- c("Namount_kg_ha_y" = "N",
#                    "warming" = "W",
#                    "grazing_lvl" = "G",
#                    "Namount_kg_ha_y:grazing_lvl" = "N:G",
#                    "warming:grazing_lvl" = "W:G",
#                    "warming:Namount_kg_ha_y" = "W:N",
#                    "warming:Namount_kg_ha_y:grazing_lvl" = "W:N:G"
# )



# colors for grazing 
# colors_g <- c("Control" = "#8FAADC", "Intensive" = "#F8CBAD", 
#               "Medium" = "#FFE699", "Natural" = "#A9D18E")

# colors for warming 
colors_w <- c("Ambient" = "gray25", "Warming" = "darkorange2")
colors_oikos <- c("Ambient" = "gray25", "Warming" = "firebrick3")

# colors_n 
# colors_Nlevel <- c("1" = "#969696",
#                    "2" = "#6baed6",
#                    "3" = "#9ecae1",
#                    "4" = "#969696",
#                    "5" = "#08306b",
#                    "6" = "#4292c6",
#                    "7" = "#c6dbef",
#                    "8" = "#2171b5",
#                    "9" = "#08519c",
#                    "10" = "#969696"
#                    )


#scale_fill_viridis(option = "mako") 

# colors for teabags
colors_tea_4 <- c("green_a" = "#b8e186", 
                "green_w" = "#7fbc41", 
                "red_a" = "#f1b6da", 
                "red_w" = "#c51b7d")

colors_tea_2 <- c("Green tea" = "#7fbc41", 
                  "Rooibos tea" = "#c51b7d")

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

