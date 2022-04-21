###### TEMPLATES AND HOW TO ORGANIZE STUFF #####

#### NAMING ---------------------------------------------------------
#### FILE NAMES

### MANUSCRIPT
# MSc_HS_Manuscript_YYYY.MM.DD.Rmd

### SIGNLE SCRIPTS
# import_data.R ---> fixup_data.R ---> analysis_data.R ---> figures_data.R

# all scripts go into their respective _plans
# import_plan.R ---> transformation_plan.R ---> analysis_plan.R ---> figures_plan.R 

# all plans go into combined_plan.R 
# combined_plan.R goes into manuscript_plan (?)


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
    