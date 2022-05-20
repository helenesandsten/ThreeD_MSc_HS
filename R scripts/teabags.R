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
         mass_loss_proportion = post_burial_weight_g / preburial_weight_g)
  

depth_tb <- teabag.df %>% 
  select(burial_depth_cm)

mean_depth_tb <- mean(teabag.df$burial_depth_cm)

## analysis ---> analysis_plan ---------------------------------------
## TEABAG INDEX -----------------------------------------------------
## additional weightloss of re-dried teabags 


h_g <- 0.842 # hydrolyzable fraction green tea 
h_r <- 0.552 # hydrolyzable fraction rooibos/red tea 

# calculating S from green tea 
teabag.df.new <- teabag.df %>% 
  select(teabag_ID, tea_type, post_burial_weight_g, preburial_weight_g, incubation_time,
         origSiteID, destSiteID, destBlockID, origBlockID, warming, grazing, grazing_lvl,
         Namount_kg_ha_y) %>% 
  pivot_wider(names_from = tea_type, 
              values_from = c(post_burial_weight_g, preburial_weight_g)) %>% 
  mutate(a_g = (1 - (post_burial_weight_g_green/preburial_weight_g_green)),
         S = 1 - (a_g/h_g),
         a_r = h_r * (1 - S),
         w_t = (post_burial_weight_g_red/preburial_weight_g_red),
         k = log((a_r / (w_t - (1 - a_r)))/incubation_time)
         ) %>%
  drop_na() 
  
## MODELS 
### ------ MODELS FOR ALPINE / LIAHOVDEN ------------------------
### MODEL: decomp ~ w * n * g ---------------------------------------
model.decomp.wng.int.lia <- teabag.df.new %>%
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
model.decomp.wng.add.lia <- teabag.df.new %>%
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
model.decomp.wng.intadd.lia <- teabag.df.new %>%
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
model.decomp.wng.addint.lia <- teabag.df.new %>%
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
model.decomp.wgn.intadd.lia <- teabag.df.new %>%
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
output.model.decomp.lia <- teabag.df.new %>%
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
model.decomp.wng.int.joa <- teabag.df.new %>%
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
model.decomp.wng.add.joa <- teabag.df.new %>%
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
model.decomp.wng.intadd.joa <- teabag.df.new %>%
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
model.decomp.wng.addint.joa <- teabag.df.new %>%
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
model.decomp.wgn.intadd.joa <- teabag.df.new %>%
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
output.model.decomp.joa <- teabag.df.new %>%
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
plot_decomp <- teabag.df.new %>% 
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


## OLD PLOT WITH TWO TEABAGS
# plot_teabags <- teabag.df %>% 
#   group_by(tea_type, origSiteID, warming, Namount_kg_ha_y, grazing) %>% 
#   mutate(origSiteID = case_when(
#     (origSiteID == "Lia") ~ "Alpine",
#     (origSiteID == "Joa") ~ "Sub-alpine")) %>% 
#   filter(!grazing == "Natural") %>% 
#   ggplot(mapping = aes(x = log(Namount_kg_ha_y +1), 
#                        y = mass_loss_proportion,
#                        color = tea_type,
#                        fill = tea_type,
#                        linetype = warming,
#                        shape = warming, 
#                        alpha = 0.7)) +
#   geom_point(size = 4) + 
#   theme_minimal(base_size = 20) +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom", 
#         legend.box = "horizontal") + 
#   scale_color_manual(values = colors_tea) + 
#   scale_linetype_manual(values = c("longdash", "solid")) + 
#   scale_shape_manual(values = c(1, 16)) +
#   #scale_size_manual(values = 10) +
#   labs(title = "Mass loss of teabags",
#        x = bquote(log(Nitrogen)~(kg~ha^-1~y^-1)),
#        y = bquote(Mass~loss~proportion)) +
#   #geom_line() +
#   geom_smooth(method = "lm", size = 2, fill = "grey") +
#   facet_grid(origSiteID ~ grazing) 
# plot_teabags
