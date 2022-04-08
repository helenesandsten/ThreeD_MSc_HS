### ANALYSIS AND MODELS ROOTS

source("R scripts/ThreeD_load_packages.R")
source("R scripts/ThreeD_create_metadata.R")


## import ------------------------------------------------------
roots.raw.df <- read_excel("Data/ThreeD_rootingrowthcores_2021.xlsx") 

## fixup ------------------------------------------------------
roots.df <- roots.raw.df %>%
mutate(dateRIC_washed = ymd(dateRIC_washed),  # change format to date
       date_roots_dried = ymd(date_roots_dried), # change format to date
       root_mass_g = total_mass_g - alutray_mass_g, # new column with root mass
       root_mass_cm3 = (root_mass_g/(pi*(1.6)^2*RIC_length_cm))) %>% # calculate volume
  mutate_if(is.character, as.factor) %>%
  left_join(NitrogenDictionary, by = "Nlevel") %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1))

## analysis ------------------------------------------------------
## writes numbers out instead of on exponential form 
options(scipen = 100, digits = 4)

### model: roots ~ warming x nitrogen 
roots.df %>%
  group_by(origSiteID, grazing) %>% # 
  nest() %>% # makes little dataframes inside my data, closed
  mutate( # makes column - do we want that?
    model.roots.wn = map(data, # runs model in each litte dataset
                         ~ lm(root_mass_cm3 ~ Namount_kg_ha_y * warming, 
                              data = .)), 
    result.roots.wn = map(model.roots.wn, tidy)) %>%
  unnest(result.roots.wn) %>% # opens the nested dataframes 
  View() 


### model: roots ~ warming x nitrogen x grazing 
roots.df %>%
  group_by(origSiteID) %>% # 
  nest() %>% # makes little dataframes inside my data, closed
  mutate( # makes column - do we want that?
    model.roots.wng = map(data, # runs model in each litte dataset
                          ~ lm(root_mass_cm3 ~ Namount_kg_ha_y * warming * grazing, 
                               data = .)),
    result.roots.wng = map(model.roots.wng, tidy)) %>%
  unnest(result.roots.wng) %>% # opens the nested dataframes 
  View() 

## making dataframe with model output 
output.roots.wng <- roots.df %>%
  group_by(origSiteID) %>% # 
  nest() %>% # makes little dataframes inside my data, closed
  mutate( # makes column - do we want that?
    model.roots.wng = map(data, # runs model in each litte dataset
                          ~ lm(root_mass_cm3 ~ Namount_kg_ha_y * warming * grazing, 
                               data = .)),
    result.roots.wng = map(model.roots.wng, tidy)) %>%
  unnest(result.roots.wng) %>% # opens the nested dataframes 
  select(origSiteID, term, estimate, std.error, statistic, p.value)

View(output.roots.wng)


## figures  ------------------------------------------------------

# megaplot - roots and warming x N x grazing 
plot_roots_mega <- roots.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  summarise(root_mass_cm3 = mean(root_mass_cm3)) %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = root_mass_cm3, color = warming)) +
  geom_point() +
  theme_bw(base_size = 20) +
  labs(title = "", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Root~mass~(g/cm^3))) +
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm")
plot_roots_mega




