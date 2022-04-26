##### MASTER AESTHETICS 

## figure settings for all figures
# theme, size, 

## colors for figures
# scale_fill_manual(values = ___)

# colors for grazing 
colors_g <- c("Control" = "#8FAADC", "Intensive" = "#F8CBAD", 
              "Medium" = "#FFE699", "Natural" = "#A9D18E")

# colors for warming 
colors_w <- c("Ambient" = "blue", "Warmed" = "red")

# colors_n 
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

