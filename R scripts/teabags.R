#### TEABAG / DECOMP TEST SCRIPT

source("R scripts/ThreeD_load_packages.R") 
source("R scripts/ThreeD_create_metadata.R")  
source("R scripts/MSc_aesthetics.R")

## importing data ---> dowload_plan ---------------------------------------
teabag.raw.df <- read_csv("Data/THREE-D_clean_decomposition_fall_2021.csv") 


string.bag.weight.df <- read_excel("Data/additional_info.xlsx", 
                                   sheet = "string_bag_weight") 

tb.reweighed.df <- read_excel("Data/additional_info.xlsx", 
                         sheet = "teabags_reweighed") 

###############################################################
## fixup ######################################################
###############################################################

## fixup mini dataset - bag and string weight 
mean_string_bag_mass <- mean(string.bag.weight.df$weight_bag_string) 

## fixup main dataset - teabags
teabag.df <- teabag.raw.df %>%  
  mutate_if(is.character, as.factor) %>% 
  mutate(origBlockID = as.factor(origBlockID),
         origPlotID = as.factor(origPlotID),
         destBlockID = as.factor(destBlockID),
         destPlotID = as.factor(destPlotID)) %>% 
         #tea_type = as.logical(tea_type)) %>% 
  mutate(Namount_kg_ha_y = log(Namount_kg_ha_y +1)) %>% # for better visualization
  # filter out damaged teabags 
  mutate(comment_3 = ifelse(is.na(comment_2), 1, 0)) %>% 
  filter(comment_3 == 1) %>% # do not run this line if you want to remove tea bag pairs
  # filter out all teabag pairs with 1 or 2 damaged teabags
  # group_by(teabag_ID) %>% 
  # mutate(comment_3 = sum(comment_3)) %>% 
  # filter(comment_3 == 2) %>% 
  # calculating days buried 
  mutate(days_buried = recover_date - burial_date, 
         days_overdue = days_buried - 90) %>% 
  # reordeing factors 
  mutate(origSiteID = factor(origSiteID, levels = c("Lia", "Joa"))) %>% 
  mutate(grazing = recode(grazing, 
                          "C" = "Control", "I" = "Intensive", 
                          "M" = "Medium", "N" = "Natural"),
         warming = recode(warming, 
                          "A" = "Ambient", "W" = "Warming")) %>% 
  # changing grazing into cont. variable too reduce degrees of freedom in models
  mutate(grazing_lvl = case_when((grazing == "Control") ~ 0,
                                 (grazing == "Medium") ~ 2,
                                 (grazing == "Intensive") ~ 4)) %>% 
  # remove weight of bag and string from teabag weight 
  mutate(preburial_weight_g = preburial_weight_g - mean_string_bag_mass,
         post_burial_weight_g = post_burial_weight_g - mean_string_bag_mass,
  # renaming and recalculating mass loss 
         mass_loss_g = weight_loss_g, 
         mass_loss_g = preburial_weight_g - post_burial_weight_g,
  # calculating proportion of mass loss   
         mass_loss_proportion = 1 - (post_burial_weight_g / preburial_weight_g))
  

# depth_tb <- teabag.df %>% select(burial_depth_cm)
# mean_depth_tb <- mean(teabag.df$burial_depth_cm)


### NEW TEABAG INDEX 
Hydrolysable_fraction_green <- 0.842
Hydrolysable_fraction_red <- 0.552

decomp.df <- teabag.df %>% 
  select(teabag_ID, tea_type, post_burial_weight_g, preburial_weight_g, incubation_time,
         origSiteID, destSiteID, destBlockID, origBlockID, warming, grazing, grazing_lvl,
         Namount_kg_ha_y, recover_date, burial_date) %>%
  # rearranging data to have teabag pairs on same row for calculations
  pivot_wider(names_from = tea_type, 
              values_from = c(post_burial_weight_g, preburial_weight_g)) %>%
  mutate(incubation_time = as.numeric(recover_date - burial_date), 
         # calculations for the teabag index
         fraction_decomposed_green = 1 - post_burial_weight_g_green/preburial_weight_g_green,
         fraction_remaining_green = post_burial_weight_g_green/preburial_weight_g_green,
         fraction_remaining_red = post_burial_weight_g_red/preburial_weight_g_red,
         # stabilisation rate 
         S = 1 - (fraction_decomposed_green / Hydrolysable_fraction_green),
         predicted_labile_fraction_red = Hydrolysable_fraction_red * (1 - S),
         # decomposition rate 
         k = log(predicted_labile_fraction_red / (fraction_remaining_red - (1 - predicted_labile_fraction_red))) / incubation_time)



### ------ MODELS FOR ALPINE / LIAHOVDEN ------------------------
### MODEL: decomp ~ w * n * g ---------------------------------------
model.decomp.wng.int.lia <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>% 
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wng.int.lia = map(data, # runs model in each litte dataset
                                   ~ lm(S ~
                                          warming * Namount_kg_ha_y * grazing_lvl,
                                        data = .x)))#,
#result.decomp.wng.int.lia = map(model.decomp.wng.int.lia, tidy)) #%>%
#unnest(result.decomp.wng.int.lia) #%>% # opens the nested dataframes
# View()

### MODEL: decomp ~ w * n * g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumptions 
#check_model(model.decomp.wng.int.lia $ model.decomp.wng.int.lia[[1]])

## check plots that are off: collinearity 
## exspected as the model has interaction terms
#check_collinearity(model.decomp.wng.int.lia $ model.decomp.wng.int.lia[[1]])
#plot(check_collinearity(model.decomp.wng.int.lia $ model.decomp.wng.int.lia[[1]])) 


###############################################################
### MODEL: decomp ~ w + n + g ---------------------------------------
model.decomp.wng.add.lia <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% #
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wng.add.lia = map(data, # runs model in each litte dataset
                                   ~ lm(S ~
                                          warming + Namount_kg_ha_y + grazing_lvl,
                                        data = .x)))#,
#result.decomp.wng.add.lia = map(model.decomp.wng.add.lia, tidy)) #%>%
#unnest(result.decomp.wng.add.lia) #%>% # opens the nested dataframes
# View()

### MODEL: decomp ~ w + n + g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumtions 
#check_model(model.decomp.wng.add.lia$model.decomp.wng.add.lia[[1]]) 
# all diagnostic plots looks good (green/blue) but 
# posterior predictive check is spiky and 
# normality of residuals is not normal at the right 



###############################################################
### MODEL: decomp ~ w * n + g ----------------------------------
model.decomp.wng.intadd.lia <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wng.intadd.lia = map(data, # runs model in each litte dataset
                                      ~ lm(S ~
                                             warming * Namount_kg_ha_y + grazing_lvl,
                                           data = .x)))#,
#result.decomp.wng.intadd.lia = map(model.decomp.wng.intadd.lia, tidy)) #%>%
#unnest(result.decomp.wng.intadd.lia) #%>% # opens the nested dataframes
# View()

### MODEL: decomp ~ w * n + g - check model --------------------

## check assumptions 
#check_model(model.decomp.wng.intadd.lia $ model.decomp.wng.intadd.lia[[1]])
## all assumptions looks good 



###############################################################
### MODEL: decomp ~ w + n * g ----------------------------------
model.decomp.wng.addint.lia <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wng.addint.lia = map(data, # runs model in each litte dataset
                                      ~ lm(S ~
                                             warming + Namount_kg_ha_y * grazing_lvl,
                                           data = .x)))#,
#result.decomp.wng.addint.lia = map(model.decomp.wng.addint.lia, tidy)) #%>%
#unnest(result.decomp.wng.addint.lia) #%>% # opens the nested dataframes
# View()

### MODEL: decomp ~ w + n * g - check model --------------------

## check assumptions 
#check_model(model.decomp.wng.addint.lia $ model.decomp.wng.addint.lia[[1]])



###############################################################
### MODEL: decomp ~ w * g + n ----------------------------------
model.decomp.wgn.intadd.lia <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wgn.intadd.lia = map(data, # runs model in each litte dataset
                                      ~ lm(S ~
                                             warming * grazing_lvl + Namount_kg_ha_y,
                                           data = .x)))#,
#result.decomp.wgn.intadd.lia = map(model.decomp.wgn.intadd.lia, tidy)) #%>%
#unnest(result.decomp.wgn.intadd.lia) #%>% # opens the nested dataframes
# View()

### MODEL: decomp ~ w * g + n - check model --------------------

## check assumptions 
#check_model(model.decomp.wgn.intadd.lia $ model.decomp.wgn.intadd.lia[[1]])


###############################################################
### compare models ALPINE / LIA ---------------------------------
model_comparison_decomp.lia <- compare_performance(
  model.decomp.wng.add.lia$model.decomp.wng.add.lia[[1]],         # w + n + g 
  model.decomp.wng.addint.lia$model.decomp.wng.addint.lia[[1]],   # w + n * g
  model.decomp.wng.intadd.lia$model.decomp.wng.intadd.lia[[1]],   # w * n + g
  model.decomp.wgn.intadd.lia$model.decomp.wgn.intadd.lia[[1]],   # w * g + n
  model.decomp.wng.int.lia$model.decomp.wng.int.lia[[1]]          # w * n * g
)


# plot.models.decomp.ng <-
#   plot(model_comparison_decomp.lia)

## making output of best model -----------------------------------
## decomp ~ w * n + g 
# options(scipen = 100, digits = 4)

## running model with result and unnest to create output 
output.model.decomp.lia <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Lia") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wng.intadd.lia = map(data, # runs model in each litte dataset
                                      ~ lm(S ~
                                             warming * Namount_kg_ha_y + grazing_lvl,
                                           data = .x)),
    result.decomp.wng.intadd.lia = map(model.decomp.wng.intadd.lia, tidy)) %>%
  unnest(result.decomp.wng.intadd.lia) %>% # opens the nested dataframes
  select(origSiteID, term, estimate, std.error, statistic, p.value)
# View()

## making clean and readable output for table ---------------------
clean_output.model.decomp.lia <- output.model.decomp.lia %>%
  mutate(term = case_when(
    (term == "(Intercept)") ~ "Intercept (no N, no warming, no grazing)",
    (term == "warmingWarming") ~ "Warming",
    (term == "Namount_kg_ha_y") ~ "Nitrogen",
    (term == "grazing_lvl") ~ "Grazing",
    (term == "warmingWarming:Namount_kg_ha_y") ~ "Warming : Nitrogen",
    (term == "warmingWarming:grazing_lvl") ~ "Warming : Grazing",
    (term == "Namount_kg_ha_y:grazing_lvl") ~ "Nitrogen : Grazing",
    (term == "warmingWarming:Namount_kg_ha_y:grazing_lvl") ~
      "Warming : Nitrogen : Grazing"
  )) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine"))

### ------ MODELS FOR SUB ALPINE / JOASETE -----------------------
### MODEL: decomp ~ w * n * g ---------------------------------------
model.decomp.wng.int.joa <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>% 
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wng.int.joa = map(data, # runs model in each litte dataset
                                   ~ lm(S ~
                                          warming * Namount_kg_ha_y * grazing_lvl,
                                        data = .x)))#,
#result.decomp.wng.int.joa = map(model.decomp.wng.int.joa, tidy)) #%>%
#unnest(result.decomp.wng.int.joa) #%>% # opens the nested dataframes
# View()

### MODEL: decomp ~ w * n * g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumptions 
#check_model(model.decomp.wng.int.joa $ model.decomp.wng.int.joa[[1]])

## check plots that are off: collinearity 
## exspected as the model has interaction terms
#check_collinearity(model.decomp.wng.int.joa $ model.decomp.wng.int.joa[[1]])
#plot(check_collinearity(model.decomp.wng.int.joa $ model.decomp.wng.int.joa[[1]])) 


###############################################################
### MODEL: decomp ~ w + n + g ---------------------------------------
model.decomp.wng.add.joa <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% #
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wng.add.joa = map(data, # runs model in each litte dataset
                                   ~ lm(S ~
                                          warming + Namount_kg_ha_y + grazing_lvl,
                                        data = .x)))#,
#result.decomp.wng.add.joa = map(model.decomp.wng.add.joa, tidy)) #%>%
#unnest(result.decomp.wng.add.joa) #%>% # opens the nested dataframes
# View()

### MODEL: decomp ~ w + n + g - check model --------------------
## must run model code without result- and unnest-line for this to be useful

## check assumtions 
#check_model(model.decomp.wng.add.joa$model.decomp.wng.add.joa[[1]]) 
# all diagnostic plots looks good (green/blue) but 
# posterior predictive check is spiky and 
# normality of residuals is not normal at the right 



###############################################################
### MODEL: decomp ~ w * n + g ----------------------------------
model.decomp.wng.intadd.joa <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wng.intadd.joa = map(data, # runs model in each litte dataset
                                      ~ lm(S ~
                                             warming * Namount_kg_ha_y + grazing_lvl,
                                           data = .x)))#,
#result.decomp.wng.intadd.joa = map(model.decomp.wng.intadd.joa, tidy)) #%>%
#unnest(result.decomp.wng.intadd.joa) #%>% # opens the nested dataframes
# View()

### MODEL: decomp ~ w * n + g - check model --------------------

## check assumptions 
#check_model(model.decomp.wng.intadd.joa $ model.decomp.wng.intadd.joa[[1]])
## all assumptions looks good 



###############################################################
### MODEL: decomp ~ w + n * g ----------------------------------
model.decomp.wng.addint.joa <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wng.addint.joa = map(data, # runs model in each litte dataset
                                      ~ lm(S ~
                                             warming + Namount_kg_ha_y * grazing_lvl,
                                           data = .x)))#,
#result.decomp.wng.addint.joa = map(model.decomp.wng.addint.joa, tidy)) #%>%
#unnest(result.decomp.wng.addint.joa) #%>% # opens the nested dataframes
# View()

### MODEL: decomp ~ w + n * g - check model --------------------

## check assumptions 
#check_model(model.decomp.wng.addint.joa $ model.decomp.wng.addint.joa[[1]])
## looks good



###############################################################
### MODEL: decomp ~ w * g + n ----------------------------------
model.decomp.wgn.intadd.joa <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% 
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wgn.intadd.joa = map(data, # runs model in each litte dataset
                                      ~ lm(S ~
                                             warming * grazing_lvl + Namount_kg_ha_y,
                                           data = .x)))#,
#result.decomp.wgn.intadd.joa = map(model.decomp.wgn.intadd.joa, tidy)) #%>%
#unnest(result.decomp.wgn.intadd.joa) #%>% # opens the nested dataframes
# View()

### MODEL: decomp ~ w * g + n - check model --------------------

## check assumptions 
#check_model(model.decomp.wgn.intadd.joa $ model.decomp.wgn.intadd.joa[[1]])

###############################################################
### compare models SUB ALPINE / JOA ---------------------------------
model_comparison_decomp.joa <- compare_performance(
  model.decomp.wng.add.joa$model.decomp.wng.add.joa[[1]],         # w + n + g 
  model.decomp.wng.addint.joa$model.decomp.wng.addint.joa[[1]],   # w + n * g
  model.decomp.wng.intadd.joa$model.decomp.wng.intadd.joa[[1]],   # w * n + g
  model.decomp.wgn.intadd.joa$model.decomp.wgn.intadd.joa[[1]],   # w * g + n
  model.decomp.wng.int.joa$model.decomp.wng.int.joa[[1]]          # w * n * g
)

# plot.models.decomp.ng <-
#   plot(model_comparison_decomp.joa)


# making output of best model -----------------------------------
# decomp ~ w * n + g
options(scipen = 100, digits = 4)
## running model with result and unnest to create output
output.model.decomp.joa <- decomp.df %>%
  # removing grazing level N to reduce degrees of freedom
  filter(!grazing == "Natural") %>%
  group_by(origSiteID) %>% #
  filter(origSiteID == "Joa") %>% 
  nest() %>% # makes little dataframes inside my data, closed
  mutate(
    model.decomp.wng.add.joa = map(data, # runs model in each litte dataset
                                   ~ lm(S ~
                                          warming + Namount_kg_ha_y + grazing_lvl,
                                        data = .x)),
    result.decomp.wng.add.joa = map(model.decomp.wng.add.joa, tidy)) %>%
  unnest(result.decomp.wng.add.joa) %>% # opens the nested dataframes
  select(origSiteID, term, estimate, std.error, statistic, p.value)
# View()

## making clean and readable output for table ---------------------
clean_output.model.decomp.joa <- output.model.decomp.joa %>%
  mutate(term = case_when(
    (term == "(Intercept)") ~ "Intercept (no N, no warming, no grazing)",
    (term == "warmingWarming") ~ "Warming",
    (term == "Namount_kg_ha_y") ~ "Nitrogen",
    (term == "grazing_lvl") ~ "Grazing",
    (term == "warmingWarming:Namount_kg_ha_y") ~ "Warming : Nitrogen",
    (term == "warmingWarming:grazing_lvl") ~ "Warming : Grazing",
    (term == "Namount_kg_ha_y:grazing_lvl") ~ "Nitrogen : Grazing",
    (term == "warmingWarming:Namount_kg_ha_y:grazing_lvl") ~
      "Warming : Nitrogen : Grazing"
  )) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine"))



## figures ----------------------------------------------------
## decomposition plot from S-factor
plot_decomp <- decomp.df %>% 
  group_by(#tea_type, 
           origSiteID, warming, Namount_kg_ha_y, grazing) %>%
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>%
  filter(!grazing == "Natural") %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1),
                       y = S,
                       color = warming,
                       linetype = warming,
                       shape = warming)) +
  geom_point(size = 4) +
  theme_minimal(base_size = 20) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_color_manual(values = colors_w) +
  scale_linetype_manual(values = c("longdash", "solid")) +
  scale_shape_manual(values = c(1, 16)) +
  #scale_size_manual(values = 10) +
  labs(title = "Decomposition",
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
       y = bquote(Stabilization~factor~(S))) +
  #geom_line() +
  geom_smooth(method = "lm", size = 2, fill = "grey") +
  facet_grid(origSiteID ~ grazing)
plot_decomp


plot_decomp_k_wng <- decomp.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  filter(!grazing == "Natural") %>%
  filter(!origSiteID == "Sub-alpine") %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = k, 
                       color = warming,
                       fill = warming,
                       #linetype = warming,
                       shape = warming)
  ) +
  geom_point(size = 2) + 
  theme_minimal(base_size = 16) + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.box = "horizontal") +
  scale_color_manual(values = colors_w) + 
  scale_fill_manual(values = colors_w) + 
  scale_shape_manual(values = c(21, 25)) + 
  #scale_linetype_manual(values = c("longdash", "solid")) + 
  labs(title = "Decomposition rate k", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(k)) + 
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
plot_decomp_k_wng 


plot_decomp_s_wng <- decomp.df %>% 
  group_by(Namount_kg_ha_y, origSiteID, warming, grazing) %>% 
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
  filter(!grazing == "Natural") %>%
  #filter(!origSiteID == "Sub-alpine") %>%
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
                       y = S, 
                       color = warming,
                       fill = warming,
                       #linetype = warming,
                       shape = warming)
  ) +
  geom_point(size = 2) + 
  theme_minimal(base_size = 16) + 
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.box = "horizontal") +
  scale_color_manual(values = colors_w) + 
  scale_fill_manual(values = colors_w) + 
  scale_shape_manual(values = c(21, 25)) + 
  #scale_linetype_manual(values = c("longdash", "solid")) + 
  labs(title = "Decomposition stabilisation factor S", 
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Stabilisation~factor~(S))) + 
  facet_grid(origSiteID ~ grazing) +
  geom_smooth(method = "lm", size = 1)
plot_decomp_s_wng 


#source("R scripts/MSc_aesthetics.R")

## MASS LOSS AMBIENT
plot_teabags_amb <- teabag.df %>%
  group_by(turfID, tea_type, origSiteID, warming, Namount_kg_ha_y, grazing) %>%
  #filter(!turfID == "1 WN1M 84") %>%
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>%
  filter(!grazing == "Natural") %>%
  mutate(tea_w_color = case_when(
    (tea_type == "green") ~ "Green tea",
    (tea_type == "red") ~ "Rooibos tea")) %>%
  # filter(!tea_w_color == "green_a" | !tea_w_color == "red_a") %>% 
  filter(warming == "Ambient") %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1),
                       y = mass_loss_proportion,
                       color = tea_w_color,
                       fill = tea_w_color,
                       linetype = warming,
                       shape = warming)) +
  geom_point(size = 3) +
  guides(size = "none") +
  theme_minimal(base_size = 16) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_color_manual(values = colors_tea_2) +
  scale_fill_manual(values = colors_tea_2) + 
  scale_shape_manual(values = 21) +
  ylim(0.25, 1) +
  labs(title = "Mass loss of teabags (ambient)",
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)), 
       y = bquote(Mass~loss~proportion)) + 
  # geom_line() +
  geom_smooth(method = "lm") +
  facet_grid(origSiteID ~ grazing)
plot_teabags_amb


## MASS LOSS WARMING
plot_teabags_war <- teabag.df %>%
  group_by(turfID, tea_type, origSiteID, warming, Namount_kg_ha_y, grazing) %>%
  #filter(!turfID == "1 WN1M 84") %>%
  mutate(origSiteID = case_when(
    (origSiteID == "Lia") ~ "Alpine",
    (origSiteID == "Joa") ~ "Sub-alpine")) %>%
  filter(!grazing == "Natural") %>%
  mutate(tea_w_color = case_when(
    (tea_type == "green") ~ "Green tea",
    (tea_type == "red") ~ "Rooibos tea")) %>%
  # filter(!tea_w_color == "green_a" | !tea_w_color == "red_a") %>% 
  filter(warming == "Warming") %>% 
  ggplot(mapping = aes(x = log(Namount_kg_ha_y +1),
                       y = mass_loss_proportion,
                       color = tea_w_color,
                       fill = tea_w_color,
                       linetype = warming,
                       shape = warming)) +
  geom_point(size = 3) +
  guides(size = "none") +
  theme_minimal(base_size = 16) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal") +
  scale_color_manual(values = colors_tea_2) +
  scale_fill_manual(values = colors_tea_2) + 
  scale_shape_manual(values = 25) +
  ylim(0.25, 1) +
  labs(title = "Mass loss of teabags (warming)",
       x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
       y = bquote(Mass~loss~proportion)) + 
  # geom_line() +
  geom_smooth(method = "lm") +
  facet_grid(origSiteID ~ grazing)
plot_teabags_war


## checking strange datapoint
## 1 WN1M 84 green is very low
teabag.df %>% 
  filter(origSiteID == "Lia",
         grazing == "Medium",
         mass_loss_proportion < 0.4,
         tea_type == "green") %>% 
  View()