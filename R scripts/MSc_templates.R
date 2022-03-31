###### TEMPLATES AND HOW TO ORGANIZE STUFF #####

#### NAMING ---------------------------------------------------------
#### FILE NAMES

### MANUSCRIPT
# MSc_HS_Manuscript_YYYY.MM.DD.Rmd

### SIGNLE SCRIPTS
# ThreeD_MSc_DATASET_import.R
# ThreeD_MSc_DATASET_fixup.R
# ThreeD_MSc_DATASET_figures.R
# ThreeD_MSc_DATASET_analysis.R

#   eks: ThreeD_MSc_roots_figures.R
#   eks: ThreeD_MSc_soil_analysis.R 





### FIGURES ---------------------------------------------------------

# SCATTER PLOT
plotname <- ggplot(data = DF, aes(x = , y = )) +
  geom_point(color = "blue", size = 3, alpha = 0.5) +
  labs( # if you put = waiver() it will fill it with default values 
    title = waiver(),
    subtitle = waiver(),
    caption = waiver(),
    tag = waiver(),
    alt = waiver(),
    alt_insight = waiver()
  )

xlab(label)

ylab(label)


### FILTERS
# # only warming 
# filter(grazing == "C") %>% 
# filter(Nlevel == 1 & 2 & 3) %>% 
# 
# # only nitrogen
# filter(warming == "A") %>% 
# filter(grazing == "C") %>% 
# 
# # only grazing 
# filter(warming == "A") %>% 
# filter(Nlevel == "1" & "2" & "3") %>% 
    